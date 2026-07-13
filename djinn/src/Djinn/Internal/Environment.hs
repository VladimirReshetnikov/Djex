--
-- Transactional validation of declarations stored by a frontend, plus the
-- declaration-shape checks shared by the CLI and the library facade.
--
module Djinn.Internal.Environment (
    TypeDefinition, Axiom, ClassDefinition, Environment(..),
    PreparedEnvironment, prepareEnvironment,
    preparedEnvironmentSource, preparedEnvironmentInventory,
    preparedEnvironmentKindCheck,
    SynthesisEnvironment, SynthesisInventory,
    SynthesisEnvironmentError(..),
    toSynthesisEnvironment, toSynthesisInventory,
    validateEnvironment,
    replace, requireDistinct, requireUnusedName,
    checkConstructors
    ) where

import Data.List (nub)
import qualified Language.Haskell.Synthesis.Declaration as SharedDeclaration
import qualified Language.Haskell.Synthesis.Environment as SharedEnvironment
import qualified Language.Haskell.Synthesis.Inventory as SharedInventory
import qualified Language.Haskell.Synthesis.KindInference as SharedInference

import Djinn.Internal.Declaration
import Djinn.Internal.HCheck
    ( PreparedKindCheck
    , htCheckEnv
    , htCheckType
    , htInferClassKinds
    , prepareKindCheckWithAssumptions
    )
import Djinn.Internal.HTypes

type TypeDefinition = (HSymbol, ([HSymbol], HType, HKind))
type Axiom = (HSymbol, HType)
type ClassDefinition = (HSymbol, ([(HSymbol, HKind)], [Axiom]))

data Environment = Environment {
    envTypes :: [TypeDefinition],
    envFunctions :: [Axiom],
    envClasses :: [ClassDefinition]
    }
    deriving (Eq, Show)

-- | A source environment sealed together with the exact shared inventory and
-- query-time kind-check state derived from it.  The constructor is private:
-- cached assumptions therefore cannot drift away from the declarations whose
-- synonym rules and proof-search lowering they accompany.
data PreparedEnvironment = PreparedEnvironment
    Environment
    SynthesisInventory
    PreparedKindCheck

type SynthesisEnvironment =
    SharedEnvironment.Environment HSymbol Int ()

type SynthesisInventory = SharedInventory.Inventory HSymbol ()

data ClassKindProjection
    -- The historical shared-environment round trip lowers through the
    -- unkinded compatibility 'ClassDecl'. Inventories are one-way checked
    -- session artifacts and can retain the validated parameter kinds.
    = OmitInferredClassKinds
    | RetainInferredClassKinds

data SynthesisEnvironmentError
    = SynthesisEnvironmentDeclarationError SynthesisDeclarationError
    | InvalidSynthesisEnvironment
        (SharedEnvironment.EnvironmentError HSymbol)
    | InvalidSynthesisInventory
        (SharedInventory.InventoryError HSymbol Int)
    | DjinnEnvironmentValidationError String
    deriving (Eq, Show)

toSynthesisEnvironment
    :: Environment
    -> Either SynthesisEnvironmentError SynthesisEnvironment
toSynthesisEnvironment environment = do
    declarations <- synthesisDeclarations OmitInferredClassKinds environment
    either (Left . InvalidSynthesisEnvironment) Right $
        SharedEnvironment.mkEnvironment declarations

-- | Seal the shared structural view together with the kinds inferred from
-- exactly those declarations. Standalone declaration adapters may still
-- round-trip 'KVar'; a checked environment cannot retain one, and the
-- inventory reports its identity through 'UngroundedInventoryKind'.
toSynthesisInventory
    :: Environment
    -> Either SynthesisEnvironmentError SynthesisInventory
toSynthesisInventory environment = do
    declarations <- synthesisDeclarations RetainInferredClassKinds environment
    either (Left . InvalidSynthesisInventory) Right $
        SharedInventory.mkInventory
            SharedInference.ClosedKindInventory declarations

-- | Seal all reusable views of an environment in one operation.  Inventory
-- kind inference is the sole whole-environment kind pass; individual queries,
-- context arguments, and instantiated methods consume the cached result.
prepareEnvironment
    :: Environment
    -> Either SynthesisEnvironmentError PreparedEnvironment
prepareEnvironment environment = do
    inventory <- toSynthesisInventory environment
    return $ PreparedEnvironment environment inventory $
        prepareKindCheckWithAssumptions
            (envTypes environment)
            (SharedInventory.inventoryKindAssumptions inventory)

preparedEnvironmentSource :: PreparedEnvironment -> Environment
preparedEnvironmentSource (PreparedEnvironment environment _ _) = environment

preparedEnvironmentInventory :: PreparedEnvironment -> SynthesisInventory
preparedEnvironmentInventory (PreparedEnvironment _ inventory _) = inventory

preparedEnvironmentKindCheck :: PreparedEnvironment -> PreparedKindCheck
preparedEnvironmentKindCheck (PreparedEnvironment _ _ kindCheck) = kindCheck

synthesisDeclarations
    :: ClassKindProjection
    -> Environment
    -> Either SynthesisEnvironmentError [SynthesisDeclaration]
synthesisDeclarations classKindProjection environment = do
    ordinary <- mapM convertedDeclaration $
        map typeDeclaration (envTypes environment) ++
        [Function name functionType |
            (name, functionType) <- envFunctions environment]
    classes <- mapM convertedClass $ envClasses environment
    return $ ordinary ++ classes
  where
    typeDeclaration (name, (parameters, body, kind)) = case body of
        HTUnion constructors -> DataType name parameters constructors
        HTAbstract _ _ -> AbstractType name kind
        _ -> TypeSynonym name parameters body

    convertedDeclaration = either
        (Left . SynthesisEnvironmentDeclarationError) Right .
        toSynthesisDeclaration

    -- The compatibility 'ClassDecl' stores only source parameter names, while
    -- a validated Environment stores their inferred kinds as well. Preserve
    -- those fixed kinds in the session inventory; otherwise a method-less
    -- Djinn class would incorrectly become poly-kinded at the shared boundary.
    convertedClass (name, (parameters, methods)) = do
        declaration <- convertedDeclaration $
            ClassDecl name (map fst parameters) methods
        case declaration of
            SharedDeclaration.ClassDeclaration
                    annotation sharedName sharedParameters superclasses
                    sharedMethods ->
                return $ SharedDeclaration.ClassDeclaration annotation
                    sharedName
                    (case classKindProjection of
                        RetainInferredClassKinds ->
                            zipWith retainKind sharedParameters parameters
                        OmitInferredClassKinds -> sharedParameters)
                    superclasses sharedMethods
            _ -> Left $ DjinnEnvironmentValidationError $
                "internal class conversion did not produce a class: " ++ name

    retainKind parameter (_, kind) = parameter {
        SharedDeclaration.parameterKind = Just $ toSynthesisKind kind
        }

-- Rebuild inferred kinds first, then check every declaration that depends
-- on them.  Class parameter kinds are re-inferred against the rebuilt type
-- graph, so they cannot go stale when a mentioned type changes.  Callers
-- update their state only with the returned environment, making deletion
-- and replacement atomic even when validation fails midway.
validateEnvironment ::
    [TypeDefinition] -> [Axiom] -> [ClassDefinition] ->
    Either String ([TypeDefinition], [ClassDefinition])
validateEnvironment definitions axioms classes = do
    checkValueNamespace axioms classes
    checked <- withContext "type environment" $ htCheckEnv definitions
    mapM_ (checkAxiom checked) axioms
    refreshed <- mapM (checkClass checked) classes
    return (checked, refreshed)

-- Function assumptions and class selectors are both printed as term-level
-- references in generated Haskell.  Consequently an unqualified assumption
-- cannot share a spelling with a method: the assumption would shadow the
-- selector and could make otherwise well-checked output ill-typed.  Exact
-- comparison is intentional.  A qualified assumption such as @External.f@
-- remains distinct from the necessarily unqualified method @f@.
--
-- Keep this check at the full-environment boundary rather than only in the
-- individual declaration operations.  Replacement and deletion then rebuild
-- the same invariant transactionally, and internal callers cannot accidentally
-- assemble an ambiguous environment either.
checkValueNamespace :: [Axiom] -> [ClassDefinition] -> Either String ()
checkValueNamespace axioms classes = do
    requireDistinct "function assumption" (map fst axioms)
    requireDistinct "class" (map fst classes)
    mapM_ checkMethodsWithinClass classes
    checkMethodsAcrossClasses classes
    case [(functionName, methodName, className)
            | (functionName, _) <- axioms
            , (className, (_, methods)) <- classes
            , (methodName, _) <- methods
            , functionName == methodName] of
        [] -> Right ()
        (functionName, methodName, className) : _ -> Left $
            "Function assumption " ++ prHSymbolOp functionName ++
            " conflicts with method " ++ prHSymbolOp methodName ++
            " of class " ++ className
  where
    checkMethodsWithinClass (className, (_, methods)) =
        requireDistinct ("method of class " ++ className) (map fst methods)

    checkMethodsAcrossClasses [] = Right ()
    checkMethodsAcrossClasses ((owner, (_, methods)) : rest) =
        checkMethodNames owner methods rest >> checkMethodsAcrossClasses rest

checkAxiom :: [TypeDefinition] -> Axiom -> Either String ()
checkAxiom definitions (name, axiomType) =
    withContext ("axiom " ++ name) $ htCheckType definitions axiomType

checkClass :: [TypeDefinition] -> ClassDefinition
           -> Either String ClassDefinition
checkClass definitions (className, (params, methods)) = do
    mapM_ checkMethod methods
    kinds <- withContext ("class " ++ className) $
        htInferClassKinds definitions (map fst params) (map snd methods)
    return (className, (kinds, methods))
  where
    checkMethod (methodName, methodType) =
        withContext
            ("method " ++ methodName ++ " of class " ++ className) $
            htCheckType definitions methodType

withContext :: String -> Either String a -> Either String a
withContext description result =
    case result of
        Left message -> Left $ description ++ ": " ++ message
        Right value -> Right value

-- Add or overwrite one binding in an association list.
replace :: HSymbol -> (HSymbol, a) -> [(HSymbol, a)] -> [(HSymbol, a)]
replace name binding = (binding :) . filter ((/= name) . fst)

requireDistinct :: String -> [HSymbol] -> Either String ()
requireDistinct what names =
    case [name | name <- nub names, length (filter (== name) names) > 1] of
        [] -> Right ()
        name:_ -> Left $ "Duplicate " ++ what ++ ": " ++ name

requireUnusedName :: String -> HSymbol -> [(HSymbol, a)] -> Either String ()
requireUnusedName existingKind name definitions
    | name `elem` map fst definitions =
        Left $ name ++ " is already defined as a " ++ existingKind
    | otherwise = Right ()

-- A data declaration may not reuse a constructor name, either within
-- itself or from another stored data type.
checkConstructors :: HSymbol -> HType -> [TypeDefinition]
                  -> Either String ()
checkConstructors owner (HTUnion constructors) definitions = do
    requireDistinct "data constructor" names
    case [(constructor, typeName)
            | (typeName, (_, HTUnion existing, _)) <- definitions
            , typeName /= owner
            , (constructor, _) <- existing
            , constructor `elem` names] of
        [] -> Right ()
        (constructor, typeName):_ -> Left $
            "Data constructor " ++ constructor ++
            " is already defined by " ++ typeName
  where names = map fst constructors
checkConstructors _ _ _ = Right ()

-- A class may not reuse a method name owned by another class.
checkMethodNames :: HSymbol -> [Axiom] -> [ClassDefinition]
                 -> Either String ()
checkMethodNames owner methods definitions =
    case [(method, className)
            | (className, (_, existing)) <- definitions
            , className /= owner
            , (method, _) <- existing
            , method `elem` names] of
        [] -> Right ()
        (method, className):_ -> Left $
            "Method " ++ prHSymbolOp method ++
            " is already defined by class " ++ className
  where names = map fst methods
