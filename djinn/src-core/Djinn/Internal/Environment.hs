--
-- Transactional validation of editable Djinn declarations, plus the
-- authoritative lowering from Djex's neutral environment into proof-search
-- indexes.
--
module Djinn.Internal.Environment (
    TypeDefinition, Axiom, ClassDefinition, Environment(..),
    PreparedEnvironment, prepareEnvironment, prepareSynthesisEnvironment,
    preparedEnvironmentSource, preparedEnvironmentInventory,
    checkPreparedTypesKinds, preparedEnvironmentFormulaTranslator,
    preparedEnvironmentFunctionPremises, lookupPreparedEnvironmentClass,
    elaboratePreparedTypes,
    SynthesisEnvironment, SynthesisInventory,
    SynthesisEnvironmentError(..),
    toSynthesisEnvironment, toSynthesisInventory,
    declareSynthesisEnvironment, removeSynthesisDeclaration,
    validateEnvironment,
    replace, requireDistinct, requireUnusedName,
    checkConstructors
    ) where

import Data.Bifunctor (first)
import Data.List (find)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void, absurd)
import qualified Language.Haskell.Synthesis.Collection as SharedCollection
import qualified Language.Haskell.Synthesis.Declaration as SharedDeclaration
import qualified Language.Haskell.Synthesis.Environment as SharedEnvironment
import qualified Language.Haskell.Synthesis.Inventory as SharedInventory
import qualified Language.Haskell.Synthesis.KindInference as SharedInference
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.TypeSynonym as SharedTypeSynonym

import Djinn.Internal.Declaration
import Djinn.Internal.Fresh (allocateFresh)
import Djinn.Internal.HCheck.Implementation
    ( PreparedKindCheck
    , htCheckTypePrepared
    , htCheckTypesKindsPrepared
    , htInferClassKindsPrepared
    , prepareKindEnvironment
    , prepareKindCheckWithAssumptions
    )
import Djinn.Internal.HTypes
import Djinn.Internal.LJTFormula (Formula, Symbol(..))
import Djinn.Internal.Type (fromSynthesisType, toSynthesisType)

type TypeDefinition = (HSymbol, ([HSymbol], HType, HKind))
type Axiom = (HSymbol, HType)
type ClassDefinition = (HSymbol, ([(HSymbol, HKind)], [Axiom]))

data Environment = Environment {
    envTypes :: [TypeDefinition],
    envFunctions :: [Axiom],
    envClasses :: [ClassDefinition]
    }
    deriving (Eq, Show)

-- | One authoritative shared inventory and the private proof-search indexes
-- derived from it. The constructor is private, so cached class lookup,
-- translated global premises, kind assumptions, synonyms, and formula
-- definitions cannot drift away from their declarations.
data PreparedEnvironment = PreparedEnvironment
    PreparedSynthesisInventory
    PreparedKindCheck
    (Map.Map HSymbol ([(HSymbol, HKind)], [Axiom]))
    [(Symbol, Formula)]
    (HType -> Either String Formula)

type SynthesisEnvironment =
    SharedEnvironment.Environment HSymbol Int ()

type SynthesisInventory = SharedInventory.Inventory HSymbol ()

type PreparedSynthesisInventory =
    SharedTypeSynonym.PreparedInventory HSymbol ()

data SynthesisEnvironmentError
    = SynthesisEnvironmentDeclarationError SynthesisDeclarationError
    | InvalidSynthesisEnvironment
        (SharedEnvironment.EnvironmentError HSymbol)
    | InvalidSynthesisInventory
        (SharedInventory.InventoryError HSymbol Int)
    | InvalidSynthesisTypeSynonyms
        (SharedTypeSynonym.SynonymExpansionError HSymbol)
    | RecursiveSynthesisDataTypes [SharedName.Name]
    | InvalidSynthesisFormulaDefinitions String
    | MissingSynthesisTypeKind SharedName.Name
    | MissingSynthesisClassKinds SharedName.Name
    | SynthesisClassKindArityMismatch SharedName.Name Int Int
    | UnresolvedSynthesisClassKind SharedName.Name HSymbol
    | SynthesisDeclarationNotFound HSymbol
    | ProtectedSynthesisUnit
    | DjinnEnvironmentValidationError String
    deriving (Eq, Show)

toSynthesisEnvironment
    :: Environment
    -> Either SynthesisEnvironmentError SynthesisEnvironment
toSynthesisEnvironment environment = do
    declarations <- synthesisDeclarations environment
    either (Left . InvalidSynthesisEnvironment) Right $
        SharedEnvironment.mkEnvironment declarations

-- | Seal the shared structural view together with the kinds inferred from
-- exactly those declarations. Standalone declaration adapters may still
-- round-trip 'KVar'; a checked environment cannot retain one, and the
-- inventory reports its identity through
-- 'SharedInventory.UngroundedInventoryKind'.
toSynthesisInventory
    :: Environment
    -> Either SynthesisEnvironmentError SynthesisInventory
toSynthesisInventory environment = do
    declarations <- synthesisDeclarations environment
    synthesisInventory declarations

synthesisInventory
    :: [SynthesisDeclaration]
    -> Either SynthesisEnvironmentError SynthesisInventory
synthesisInventory declarations =
    either (Left . InvalidSynthesisInventory) Right $
        SharedInventory.mkInventoryWithClassPolicy
            SharedInference.ClosedKindInventory
            SharedInference.DefaultClassKinds declarations

-- | Seal all reusable views of an environment in one operation.  Inventory
-- kind inference is the sole whole-environment kind pass; individual queries,
-- context arguments, and instantiated methods consume the cached result.
-- Alias expansion and recursive-datatype classification use the same neutral
-- preflight as 'prepareSynthesisEnvironment', so even a raw constructor-forged
-- environment cannot send a recursive expansion graph into formula lowering.
prepareEnvironment
    :: Environment
    -> Either SynthesisEnvironmentError PreparedEnvironment
prepareEnvironment environment = do
    declarations <- synthesisDeclarations environment
    inventory <- synthesisInventory declarations
    sourceDeclarations <- mapM preflightDeclaration declarations
    prepared <- prepareInventoryExpansion inventory
    projected <- projectSynthesisEnvironment
        (SharedInventory.inventoryKindAssumptions inventory)
        sourceDeclarations
    sealPreparedEnvironment projected prepared

-- | Validate a neutral environment once, then derive Djinn's compatibility
-- projection from the resulting inventory. Conversion is deliberately split
-- into phases: every declaration first crosses Djinn's lexical/feature
-- boundary in source order; explicit kinds are then grounded without
-- rebuilding the environment; finally the shared kind assumptions become the
-- sole authority for every kind embedded in Djinn's raw representation.
--
-- Synonym declarations stay in the authoritative Inventory. The synonym
-- table and formula translator are derived caches, while raw declaration
-- tables are reconstructed only for compatibility inspection. Aliases are
-- expanded across every declaration before sealing, both to enforce Haskell's
-- saturation rule and to classify recursive datatypes by their actual
-- (alias-free) fields.
prepareSynthesisEnvironment
    :: SynthesisEnvironment
    -> Either SynthesisEnvironmentError PreparedEnvironment
prepareSynthesisEnvironment sourceEnvironment = do
    sourceDeclarations <- mapM preflightDeclaration $
        SharedEnvironment.environmentDeclarations sourceEnvironment
    groundedEnvironment <- first
        (InvalidSynthesisInventory .
            SharedInventory.UngroundedInventoryKind) $
        SharedEnvironment.groundEnvironmentKinds sourceEnvironment
    inventory <- first
        (InvalidSynthesisInventory . promoteVoidInventoryError) $
        SharedInventory.mkInventoryFromEnvironmentWithClassPolicy
            SharedInference.ClosedKindInventory
            SharedInference.DefaultClassKinds groundedEnvironment
    prepared <- prepareInventoryExpansion inventory
    environment <- projectSynthesisEnvironment
        (SharedInventory.inventoryKindAssumptions inventory)
        sourceDeclarations
    sealPreparedEnvironment environment prepared

-- | Apply one compatibility declaration directly to the shared environment,
-- then seal the resulting session state before returning it.  Djinn's
-- historical association lists put replacements first within each category;
-- encode that policy here rather than round-tripping through those lists and
-- accidentally making them the editable authority again.
declareSynthesisEnvironment
    :: Declaration
    -> SynthesisEnvironment
    -> Either SynthesisEnvironmentError
        (SynthesisEnvironment, PreparedEnvironment)
declareSynthesisEnvironment declaration sourceEnvironment = do
    sharedDeclaration <- first SynthesisEnvironmentDeclarationError $
        toSynthesisDeclaration declaration
    (group, owner) <- synthesisDeclarationOwner sharedDeclaration
    categorized <- mapM categorize $
        SharedEnvironment.environmentDeclarations sourceEnvironment
    let declarations selected =
            [ candidate
            | (candidate, candidateGroup, candidateOwner) <- categorized
            , candidateGroup == selected
            , candidateGroup /= group || candidateOwner /= owner
            ]
        candidateDeclarations =
            declarations SynthesisTypeDeclaration ++
            declarations SynthesisValueDeclaration ++
            declarations SynthesisClassDeclaration
        withReplacement = case group of
            SynthesisTypeDeclaration ->
                sharedDeclaration : candidateDeclarations
            SynthesisValueDeclaration ->
                declarations SynthesisTypeDeclaration ++
                sharedDeclaration :
                (declarations SynthesisValueDeclaration ++
                 declarations SynthesisClassDeclaration)
            SynthesisClassDeclaration ->
                declarations SynthesisTypeDeclaration ++
                declarations SynthesisValueDeclaration ++
                sharedDeclaration : declarations SynthesisClassDeclaration
    candidate <- first InvalidSynthesisEnvironment $
        SharedEnvironment.mkEnvironment withReplacement
    prepared <- prepareSynthesisEnvironment candidate
    return (candidate, prepared)
  where
    categorize candidate = do
        (group, owner) <- synthesisDeclarationOwner candidate
        return (candidate, group, owner)

-- | Delete one top-level Djinn declaration from the shared environment and
-- validate every survivor before committing. Constructor and method names are
-- deliberately not top-level deletion keys, matching the historical REPL.
removeSynthesisDeclaration
    :: HSymbol
    -> SynthesisEnvironment
    -> Either SynthesisEnvironmentError
        (SynthesisEnvironment, PreparedEnvironment)
removeSynthesisDeclaration sourceName sourceEnvironment = do
    owner <- case SharedName.parseName sourceName of
        Left _ -> Left $ SynthesisDeclarationNotFound sourceName
        Right name -> Right name
    declarations <- mapM attachOwner $
        SharedEnvironment.environmentDeclarations sourceEnvironment
    let matching = [declaration |
            (declaration, candidateOwner) <- declarations,
            candidateOwner == owner]
    if null matching then Left $ SynthesisDeclarationNotFound sourceName
    else if sourceName == "()" then Left ProtectedSynthesisUnit
    else do
        candidate <- first InvalidSynthesisEnvironment $
            SharedEnvironment.mkEnvironment
                [ declaration
                | (declaration, candidateOwner) <- declarations
                , candidateOwner /= owner
                ]
        prepared <- prepareSynthesisEnvironment candidate
        return (candidate, prepared)
  where
    attachOwner declaration = do
        (_, owner) <- synthesisDeclarationOwner declaration
        return (declaration, owner)

data SynthesisDeclarationGroup
    = SynthesisTypeDeclaration
    | SynthesisValueDeclaration
    | SynthesisClassDeclaration
    deriving (Eq)

synthesisDeclarationOwner
    :: SynthesisDeclaration
    -> Either SynthesisEnvironmentError
        (SynthesisDeclarationGroup, SharedName.Name)
synthesisDeclarationOwner declaration = case declaration of
    SharedDeclaration.TypeSynonymDeclaration _ name _ _ ->
        Right (SynthesisTypeDeclaration, name)
    SharedDeclaration.DataTypeDeclaration _ name _ _ ->
        Right (SynthesisTypeDeclaration, name)
    SharedDeclaration.AbstractTypeDeclaration _ name _ ->
        Right (SynthesisTypeDeclaration, name)
    SharedDeclaration.ValueDeclaration signature -> Right
        (SynthesisValueDeclaration, SharedDeclaration.valueName signature)
    SharedDeclaration.ClassDeclaration _ name _ _ _ ->
        Right (SynthesisClassDeclaration, name)
    SharedDeclaration.InstanceDeclaration{} -> Left $
        SynthesisEnvironmentDeclarationError InstanceDeclarationUnsupported

-- Expand aliases before classifying datatype recursion. Looking only at raw
-- fields can hide a real cycle through an alias or invent one in an argument
-- erased by a phantom alias, so both raw and neutral session construction must
-- share this exact preflight. Return the same prepared table retained by the
-- sealed environment rather than expanding its declarations a second time.
prepareInventoryExpansion
    :: SynthesisInventory
    -> Either SynthesisEnvironmentError
        PreparedSynthesisInventory
prepareInventoryExpansion inventory = do
    prepared <- first InvalidSynthesisTypeSynonyms $
        SharedTypeSynonym.prepareInventory
            freshDjinnTypeVariable inventory
    let synonyms = SharedTypeSynonym.preparedTypeSynonyms prepared
    expandedDeclarations <- mapM (expandForRecursion synonyms)
        (SharedEnvironment.environmentDeclarations $
            SharedInventory.inventoryEnvironment inventory)
    let recursiveNames = SharedDeclaration.recursiveDataTypeNames
            expandedDeclarations
    if Set.null recursiveNames then return prepared else
        Left $ RecursiveSynthesisDataTypes $ Set.toAscList recursiveNames
  where
    expandForRecursion synonyms declaration =
        case declaration of
            -- Preparation above has already normalized and validated every
            -- synonym, including unused ones. Recursion classification ignores
            -- synonym declarations, so retain their source shape instead of
            -- materializing the same expansion a second time.
            SharedDeclaration.TypeSynonymDeclaration{} -> Right declaration
            _ -> first InvalidSynthesisTypeSynonyms $
                SharedTypeSynonym.expandDeclarationTypeSynonyms
                    freshDjinnTypeVariable synonyms declaration

sealPreparedEnvironment
    :: Environment
    -> PreparedSynthesisInventory
    -> Either SynthesisEnvironmentError PreparedEnvironment
sealPreparedEnvironment (Environment types functions classes)
        prepared = do
    translate <- first InvalidSynthesisFormulaDefinitions $
        prepareTypeFormulaTranslator types
    -- Moving an invariant translation failure from query execution to sealing
    -- is deliberate. Kind checking plus whole-definition graph validation
    -- excludes such failures for supported declarations, and eager preparation
    -- ensures every published environment owns a complete premise cache.
    premises <- first InvalidSynthesisFormulaDefinitions $
        mapM (translateFunction translate) functions
    let kindCheck = prepareKindCheckWithAssumptions types
            $ SharedInventory.inventoryKindAssumptions inventory
        classIndex = Map.fromList classes
    -- Force the derived index before the transient compatibility projection
    -- leaves scope; its field cannot retain the complete raw Environment thunk.
    classIndex `seq` kindCheck `seq` return (PreparedEnvironment
        prepared kindCheck classIndex premises translate)
  where
    inventory = SharedTypeSynonym.preparedInventory prepared

    translateFunction translate (name, source) =
        (,) (Symbol name) `fmap`
            first (("function " ++ prHSymbolOp name ++ ": ") ++)
                (translate source)

-- | Reconstruct the historical raw declaration tables on demand. The
-- inventory is opaque and can enter t'PreparedEnvironment' only after Djinn's
-- declaration preflight, so a failure here denotes an internal invariant
-- violation rather than a caller error.
preparedEnvironmentSource :: PreparedEnvironment -> Environment
preparedEnvironmentSource prepared =
    case projectPreparedInventory prepared of
        Right environment -> environment
        Left failure -> error $
            "Djinn.Internal.Environment.preparedEnvironmentSource: " ++
            "sealed inventory invariant failed: " ++ show failure

preparedEnvironmentInventory :: PreparedEnvironment -> SynthesisInventory
preparedEnvironmentInventory
        (PreparedEnvironment prepared _ _ _ _) =
    SharedTypeSynonym.preparedInventory prepared

-- | Check a batch in the one kind scope sealed with this exact environment.
-- Keeping the cache behind this operation prevents callers from retaining or
-- pairing its private synonym arities and assumptions independently.
checkPreparedTypesKinds
    :: PreparedEnvironment
    -> [(HKind, HType)]
    -> Either String ()
checkPreparedTypesKinds
        (PreparedEnvironment _ kindCheck _ _ _) =
    htCheckTypesKindsPrepared kindCheck

preparedEnvironmentFunctionPremises
    :: PreparedEnvironment
    -> [(Symbol, Formula)]
preparedEnvironmentFunctionPremises
        (PreparedEnvironment _ _ _ premises _) = premises

-- | The definition table is validated and compiled exactly once when the
-- environment is sealed. Individual queries retain only their source-local
-- expansion-path check; they never repeat whole-table SCC analysis.
preparedEnvironmentFormulaTranslator
    :: PreparedEnvironment
    -> HType
    -> Either String Formula
preparedEnvironmentFormulaTranslator
        (PreparedEnvironment _ _ _ _ translate) = translate

preparedEnvironmentTypeSynonyms
    :: PreparedEnvironment
    -> SharedTypeSynonym.TypeSynonyms HSymbol
preparedEnvironmentTypeSynonyms
        (PreparedEnvironment prepared _ _ _ _) =
    SharedTypeSynonym.preparedTypeSynonyms prepared

projectPreparedInventory
    :: PreparedEnvironment
    -> Either SynthesisEnvironmentError Environment
projectPreparedInventory prepared = do
    sourceDeclarations <- mapM preflightDeclaration $
        map (SharedDeclaration.mapDeclarationKindVariables absurd) $
        SharedEnvironment.environmentDeclarations environment
    projectSynthesisEnvironment assumptions sourceDeclarations
  where
    inventory = preparedEnvironmentInventory prepared
    environment = SharedInventory.inventoryEnvironment inventory
    assumptions = SharedInventory.inventoryKindAssumptions inventory

lookupPreparedEnvironmentClass
    :: HSymbol
    -> PreparedEnvironment
    -> Maybe ([(HSymbol, HKind)], [Axiom])
lookupPreparedEnvironmentClass name
        (PreparedEnvironment _ _ classes _ _) = Map.lookup name classes

-- | Elaborate a query batch through the exact alias table retained by its
-- prepared environment. The list is checked in one free-variable kind scope;
-- conversion back to Djinn's raw syntax is safe because environment preflight
-- already excludes shared forms (such as explicit forall and unboxed tuples)
-- that Djinn cannot represent.
elaboratePreparedTypes
    :: PreparedEnvironment
    -> [(HKind, HType)]
    -> Either String [HType]
elaboratePreparedTypes prepared obligations = do
    sharedObligations <- mapM convertObligation obligations
    elaborated <- first renderElaborationError $
        SharedTypeSynonym.elaborateTypes
            freshDjinnTypeVariable synonyms sharedObligations
    mapM (first show . fromSynthesisType) elaborated
  where
    convertObligation (expected, source) = (,)
        <$> checkedGroundHKind expected
        <*> first show (toSynthesisType source)

    synonyms = preparedEnvironmentTypeSynonyms prepared

-- Match the compatibility checker's established spelling for the one query
-- failure users commonly act on. Other failures retain their structured Show
-- representation; in normal operation the legacy preflight has already
-- rejected them with its more local source label.
renderElaborationError
    :: SharedTypeSynonym.TypeElaborationError HSymbol
    -> String
renderElaborationError failure = case failure of
    SharedTypeSynonym.IllKindedType _ kindError -> show kindError
    SharedTypeSynonym.InvalidElaborationType _ typeError -> show typeError
    SharedTypeSynonym.SynonymExpansionFailed expansionError ->
        case expansionError of
            SharedTypeSynonym.UnsaturatedTypeSynonym name expected supplied ->
                "Type synonym " ++ SharedName.renderCanonical name ++
                " expects at least " ++ show expected ++
                " argument(s), but got " ++ show supplied
            _ -> show expansionError

checkedGroundHKind :: HKind -> Either String SharedInference.GroundKind
checkedGroundHKind = first renderUnsolved . groundHKind
  where
    -- Preserve the raw environment API's historical bare-identity spelling.
    renderUnsolved variable =
        "kind contains an unsolved variable: " ++ show variable

synthesisDeclarations
    :: Environment
    -> Either SynthesisEnvironmentError [SynthesisDeclaration]
synthesisDeclarations environment =
    mapM convertedDeclaration $
        map typeDeclaration (envTypes environment) ++
        [Function name functionType |
            (name, functionType) <- envFunctions environment] ++
        [ClassDecl name (map fst parameters) methods |
            (name, (parameters, methods)) <- envClasses environment]
  where
    typeDeclaration (name, (parameters, body, kind)) = case body of
        HTUnion constructors -> DataType name parameters constructors
        HTAbstract _ _ -> AbstractType name kind
        _ -> TypeSynonym name parameters body

    convertedDeclaration = either
        (Left . SynthesisEnvironmentDeclarationError) Right .
        toSynthesisDeclaration

preflightDeclaration
    :: SynthesisDeclaration
    -> Either SynthesisEnvironmentError (SynthesisDeclaration, Declaration)
preflightDeclaration sharedDeclaration = do
    rawDeclaration <- first SynthesisEnvironmentDeclarationError $
        fromSynthesisDeclaration sharedDeclaration
    return (sharedDeclaration, rawDeclaration)

data ProjectedDeclaration
    = ProjectedType TypeDefinition
    | ProjectedFunction Axiom
    | ProjectedClass ClassDefinition

projectSynthesisEnvironment
    :: SharedInference.KindAssumptions
    -> [(SynthesisDeclaration, Declaration)]
    -> Either SynthesisEnvironmentError Environment
projectSynthesisEnvironment assumptions declarations = do
    projected <- mapM (projectDeclaration assumptions) declarations
    let (types, functions, classes) =
            foldr collect ([], [], []) projected
    return Environment
        { envTypes = types
        , envFunctions = functions
        , envClasses = classes
        }
  where
    collect declaration (types, functions, classes) = case declaration of
        ProjectedType typeDefinition ->
            (typeDefinition : types, functions, classes)
        ProjectedFunction function ->
            (types, function : functions, classes)
        ProjectedClass classDefinition ->
            (types, functions, classDefinition : classes)

projectDeclaration
    :: SharedInference.KindAssumptions
    -> (SynthesisDeclaration, Declaration)
    -> Either SynthesisEnvironmentError ProjectedDeclaration
projectDeclaration assumptions pair = case pair of
    (SharedDeclaration.TypeSynonymDeclaration _ sharedName _ _,
            TypeSynonym name parameters body) -> do
        kind <- requiredTypeKind assumptions sharedName
        return $ ProjectedType (name, (parameters, body, kind))
    (SharedDeclaration.DataTypeDeclaration _ sharedName _ _,
            DataType name parameters constructors) -> do
        kind <- requiredTypeKind assumptions sharedName
        return $ ProjectedType
            (name, (parameters, HTUnion constructors, kind))
    (SharedDeclaration.AbstractTypeDeclaration _ sharedName _,
            AbstractType name _) -> do
        kind <- requiredTypeKind assumptions sharedName
        return $ ProjectedType (name, ([], HTAbstract name kind, kind))
    (SharedDeclaration.ValueDeclaration{},
            Function name functionType) ->
        Right $ ProjectedFunction (name, functionType)
    (SharedDeclaration.ClassDeclaration _ sharedName _ _ _,
            ClassDecl name parameters methods) -> do
        kinds <- requiredClassKinds assumptions sharedName parameters
        return $ ProjectedClass
            (name, (zip parameters kinds, methods))
    _ -> Left $ DjinnEnvironmentValidationError $
        "internal shared declaration projection changed shape"

requiredTypeKind
    :: SharedInference.KindAssumptions
    -> SharedName.Name
    -> Either SynthesisEnvironmentError HKind
requiredTypeKind assumptions name =
    maybe (Left $ MissingSynthesisTypeKind name)
        (Right . fromGroundHKind) $
        Map.lookup name $ SharedInference.typeConstructorKinds assumptions

requiredClassKinds
    :: SharedInference.KindAssumptions
    -> SharedName.Name
    -> [HSymbol]
    -> Either SynthesisEnvironmentError [HKind]
requiredClassKinds assumptions name parameters =
    case Map.lookup name $ SharedInference.classParameterKinds assumptions of
        Nothing -> Left $ MissingSynthesisClassKinds name
        Just kinds
            | length kinds /= length parameters -> Left $
                SynthesisClassKindArityMismatch name
                    (length parameters) (length kinds)
            | otherwise -> sequence
                [ maybe (Left $ UnresolvedSynthesisClassKind name parameter)
                    (Right . fromGroundHKind) kind
                | (parameter, kind) <- zip parameters kinds
                ]

freshDjinnTypeVariable
    :: SharedTypeSynonym.FreshVariable HSymbol
freshDjinnTypeVariable reserved variable = Just fresh
  where
    (fresh, _, _) = allocateFresh
        (\candidate -> (candidate, candidate ++ "'"))
        reserved (variable ++ "'")

-- The Environment-native constructor cannot encounter an ungrounded kind,
-- but its error parameter remembers the already-erased input type. Retag the
-- two inhabited alternatives for the compatibility error vocabulary and
-- discharge the impossible third one.
promoteVoidInventoryError
    :: SharedInventory.InventoryError HSymbol Void
    -> SharedInventory.InventoryError HSymbol Int
promoteVoidInventoryError failure = case failure of
    SharedInventory.InvalidInventoryEnvironment environmentError ->
        SharedInventory.InvalidInventoryEnvironment environmentError
    SharedInventory.UngroundedInventoryKind impossible -> absurd impossible
    SharedInventory.InvalidInventoryKinds kindError ->
        SharedInventory.InvalidInventoryKinds kindError

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
    (checked, prepared) <- withContext "type environment" $
        prepareKindEnvironment definitions
    mapM_ (checkAxiom prepared) axioms
    refreshed <- mapM (checkClass prepared) classes
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

checkAxiom :: PreparedKindCheck -> Axiom -> Either String ()
checkAxiom prepared (name, axiomType) =
    withContext ("axiom " ++ name) $
        htCheckTypePrepared prepared axiomType

checkClass :: PreparedKindCheck -> ClassDefinition
           -> Either String ClassDefinition
checkClass prepared (className, (params, methods)) = do
    mapM_ checkMethod methods
    kinds <- withContext ("class " ++ className) $
        htInferClassKindsPrepared prepared
            (map fst params) (map snd methods)
    return (className, (kinds, methods))
  where
    checkMethod (methodName, methodType) =
        withContext
            ("method " ++ methodName ++ " of class " ++ className) $
            htCheckTypePrepared prepared methodType

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
    case find (`Set.member` repeated) names of
        Nothing -> Right ()
        Just name -> Left $ "Duplicate " ++ what ++ ": " ++ name
  where
    -- Preserve the historical precedence of the repeated value whose first
    -- occurrence is earliest; that is not necessarily the value that repeats
    -- first.  The shared summary removes the former quadratic rescan.
    repeated = SharedCollection.repeatedValueSet
        $ SharedCollection.summarizeDuplicates names

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
