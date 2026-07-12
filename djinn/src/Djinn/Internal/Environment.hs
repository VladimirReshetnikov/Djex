--
-- Transactional validation of declarations stored by a frontend, plus the
-- declaration-shape checks shared by the CLI and the library facade.
--
module Djinn.Internal.Environment (
    TypeDefinition, Axiom, ClassDefinition, Environment(..),
    SynthesisEnvironment, SynthesisEnvironmentError(..),
    toSynthesisEnvironment,
    validateEnvironment,
    replace, requireDistinct, requireUnusedName,
    checkConstructors
    ) where

import Data.List (nub)
import qualified Language.Haskell.Synthesis.Environment as SharedEnvironment

import Djinn.Internal.Declaration
import Djinn.Internal.HCheck (htCheckEnv, htCheckType, htInferClassKinds)
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

type SynthesisEnvironment =
    SharedEnvironment.Environment HSymbol Int ()

data SynthesisEnvironmentError
    = SynthesisEnvironmentDeclarationError SynthesisDeclarationError
    | InvalidSynthesisEnvironment
        (SharedEnvironment.EnvironmentError HSymbol)
    | DjinnEnvironmentValidationError String
    deriving (Eq, Show)

toSynthesisEnvironment
    :: Environment
    -> Either SynthesisEnvironmentError SynthesisEnvironment
toSynthesisEnvironment environment = do
    declarations <- mapM convertedDeclaration $
        map typeDeclaration (envTypes environment) ++
        [Function name functionType |
            (name, functionType) <- envFunctions environment] ++
        [ClassDecl name (map fst parameters) methods |
            (name, (parameters, methods)) <- envClasses environment]
    either (Left . InvalidSynthesisEnvironment) Right $
        SharedEnvironment.mkEnvironment declarations
  where
    typeDeclaration (name, (parameters, body, kind)) = case body of
        HTUnion constructors -> DataType name parameters constructors
        HTAbstract _ _ -> AbstractType name kind
        _ -> TypeSynonym name parameters body

    convertedDeclaration = either
        (Left . SynthesisEnvironmentDeclarationError) Right .
        toSynthesisDeclaration

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
