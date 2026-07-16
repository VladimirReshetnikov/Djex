--
-- Transactional validation of editable Djinn declarations, plus the
-- authoritative lowering from Djex's neutral environment into proof-search
-- indexes.
--
module Djinn.Internal.Environment (
    TypeDefinition, Axiom, ClassDefinition, Environment(..),
    PreparedEnvironment, prepareEnvironment, prepareSynthesisEnvironment,
    prepareGroundSynthesisEnvironment,
    preparedEnvironmentSource, preparedEnvironmentInventory,
    checkPreparedTypesKinds, checkPreparedSynthesisTypesKinds,
    preparedEnvironmentFormulaTranslator,
    preparedEnvironmentSynthesisFormulaTranslator,
    preparedEnvironmentFunctionPremises,
    lookupPreparedSynthesisClass, synthesisMethodSymbol,
    elaboratePreparedTypes, elaboratePreparedSynthesisTypes,
    SynthesisEnvironment, SynthesisInventory,
    SynthesisEnvironmentError(..),
    toSynthesisEnvironment, toSynthesisInventory,
    declareSynthesisEnvironment, removeSynthesisDeclaration,
    validateEnvironment,
    requireDistinct
    ) where

import Data.Bifunctor (first)
import Data.List (find)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void, absurd)
import qualified Language.Haskell.Synthesis.Collection as SharedCollection
import qualified Language.Haskell.Synthesis.Declaration as SharedDeclaration
import qualified Language.Haskell.Synthesis.Environment as SharedEnvironment
import Language.Haskell.Synthesis.Fresh (allocateFresh)
import qualified Language.Haskell.Synthesis.Inventory as SharedInventory
import qualified Language.Haskell.Synthesis.KindInference as SharedInference
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.TypeSynonym as SharedTypeSynonym

import Djinn.Internal.Declaration
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
import Djinn.Internal.TypeFormula
import Djinn.Internal.Type
    ( djinnTypeConstructorSymbol
    , fromSynthesisType
    , normalizeSynthesisType
    , toSynthesisType
    )
import qualified Language.Haskell.Synthesis.Type as SharedType

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
-- translated global premises, the raw compatibility kind checker, synonyms,
-- and formula definitions cannot drift away from their declarations. Native
-- kind checks consume the Inventory assumptions and opaque synonym table
-- directly rather than this legacy string-keyed cache.
data PreparedEnvironment = PreparedEnvironment
    PreparedSynthesisInventory
    PreparedKindCheck
    (Map.Map SharedName.Name SynthesisClassDefinition)
    [(Symbol, Formula)]
    PreparedFormulaCompiler

-- The prepared class index stays in the authoritative shared type and name
-- vocabulary. Historical context APIs wrap final methods in 'HType' only at
-- their compatibility edge; native queries never cross that view.
type SynthesisClassDefinition =
    ( [(HSymbol, HKind)]
    , [(SharedName.Name, SharedType.Type HSymbol)]
    )

-- | Project a validated shared class-method name into Djinn's historical
-- proof-symbol namespace. Operators are stored bare there (for example
-- @==@, not the canonical prefix spelling @(==)@). Prepared inventories can
-- contain only the unqualified variable names accepted by Djinn's declaration
-- boundary, but keep that invariant checked at this private projection edge.
synthesisMethodSymbol :: SharedName.Name -> Either String HSymbol
synthesisMethodSymbol = synthesisValueSymbol MethodOwner "class method"

-- Identifiers retain their canonical qualification; operators use the bare
-- spelling stored in Djinn's proof namespace. Qualified operators and every
-- other name form are outside the compatibility declaration grammar.
synthesisValueSymbol
    :: DjinnDeclarationNameRole
    -> String
    -> SharedName.Name
    -> Either String HSymbol
synthesisValueSymbol role description name =
    case djinnDeclarationSpelling name of
        Just spelling
            | isDjinnDeclarationName role spelling -> Right spelling
        _ -> Left $
            "sealed " ++ description ++
                " name is not a Djinn value symbol: " ++
                SharedName.renderCanonical name

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
    | ProtectedSynthesisUnitDeclaration
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
    expansion <- prepareInventoryExpansion inventory
    projected <- projectSynthesisEnvironment
        (SharedInventory.inventoryKindAssumptions inventory)
        sourceDeclarations
    sealPreparedEnvironment projected expansion

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
    prepareGroundSynthesisEnvironmentFrom
        sourceDeclarations groundedEnvironment

-- | Seal the kind-ground neutral environment accepted by the stable Djex
-- adapter. Unlike the raw compatibility entrance above, this path does not
-- weaken impossible kind variables merely to traverse and reject them again.
prepareGroundSynthesisEnvironment
    :: SharedEnvironment.Environment HSymbol Void ()
    -> Either SynthesisEnvironmentError PreparedEnvironment
prepareGroundSynthesisEnvironment sourceEnvironment = do
    sourceDeclarations <- mapM preflightGroundDeclaration $
        SharedEnvironment.environmentDeclarations sourceEnvironment
    prepareGroundSynthesisEnvironmentFrom
        sourceDeclarations sourceEnvironment

prepareGroundSynthesisEnvironmentFrom
    :: [(SynthesisDeclaration, Declaration)]
    -> SharedEnvironment.Environment HSymbol Void ()
    -> Either SynthesisEnvironmentError PreparedEnvironment
prepareGroundSynthesisEnvironmentFrom sourceDeclarations sourceEnvironment = do
    inventory <- first
        (InvalidSynthesisInventory . promoteVoidInventoryError) $
        SharedInventory.mkInventoryFromEnvironmentWithClassPolicy
            SharedInference.ClosedKindInventory
            SharedInference.DefaultClassKinds sourceEnvironment
    expansion <- prepareInventoryExpansion inventory
    environment <- projectSynthesisEnvironment
        (SharedInventory.inventoryKindAssumptions inventory)
        sourceDeclarations
    sealPreparedEnvironment environment expansion

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
    -- The canonical unit declaration is representable only so trusted raw
    -- environments can cross the shared boundary. Public editing must not
    -- use that representational exception to install grammar-level @()@.
    if declaration == DataType "()" [] [("()", [])]
        then Left ProtectedSynthesisUnitDeclaration
        else Right ()
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
-- sealed environment together with the transient expanded declarations, so
-- formula definitions and premises do not repeat the same expansion.
prepareInventoryExpansion
    :: SynthesisInventory
    -> Either SynthesisEnvironmentError
        PreparedInventoryExpansion
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
    if Set.null recursiveNames then
        return $ PreparedInventoryExpansion prepared expandedDeclarations
    else
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

data PreparedInventoryExpansion = PreparedInventoryExpansion
    PreparedSynthesisInventory
    [SharedDeclaration.Declaration HSymbol Void ()]

-- Build the formula-definition cache from the same transient declaration
-- stream used for recursion classification. Synonyms retain their checked
-- source bodies; every other declaration has already had aliases expanded.
-- Source order is significant for deterministic recursive-component errors.
prepareSynthesisFormulaCompiler
    :: [SharedDeclaration.Declaration HSymbol kindVariable annotation]
    -> Either String PreparedFormulaCompiler
prepareSynthesisFormulaCompiler declarations = do
    definitions <- concat `fmap`
        mapM synthesisFormulaDefinition declarations
    prepareFormulaCompiler synthesisFormulaTypeView definitions

synthesisFormulaDefinition
    :: SharedDeclaration.Declaration HSymbol kindVariable annotation
    -> Either String
        [FormulaDefinition (SharedType.Type HSymbol)]
synthesisFormulaDefinition declaration = case declaration of
    SharedDeclaration.TypeSynonymDeclaration _ name parameters body -> do
        owner <- synthesisFormulaTypeSymbol name
        return [FormulaAlias owner (map parameterVariable parameters) $
            SharedType.canonicalizeType body]
    SharedDeclaration.DataTypeDeclaration _ name parameters constructors -> do
        owner <- synthesisFormulaTypeSymbol name
        alternatives <- mapM convertConstructor constructors
        return [FormulaData owner
            (map parameterVariable parameters) alternatives]
    SharedDeclaration.AbstractTypeDeclaration _ name _ -> do
        owner <- synthesisFormulaTypeSymbol name
        return [FormulaAbstract owner [] owner]
    SharedDeclaration.ValueDeclaration{} -> Right []
    SharedDeclaration.ClassDeclaration{} -> Right []
    SharedDeclaration.InstanceDeclaration{} -> Right []
  where
    parameterVariable = SharedDeclaration.parameterVariable

    convertConstructor constructor = do
        name <- synthesisFormulaTypeSymbol $
            SharedDeclaration.constructorName constructor
        return (name, map SharedType.canonicalizeType $
            SharedDeclaration.constructorFields constructor)

-- Inputs are canonicalized once before entering this one-layer view. The zero
-- boxed tuple is deliberately projected to the nominal @()@ constructor used
-- by Djinn's standard environment rather than to structural truth.
synthesisFormulaTypeView
    :: TypeView (SharedType.Type HSymbol)
synthesisFormulaTypeView source = case source of
    SharedType.TypeVariable variable ->
        Right $ TypeVariableLayer variable
    SharedType.TypeConstructor name ->
        TypeConstructorLayer `fmap` synthesisFormulaTypeSymbol name
    SharedType.TypeApplication function argument ->
        Right $ TypeApplicationLayer function argument
    SharedType.FunctionType argument result ->
        Right $ TypeArrowLayer argument result
    SharedType.TupleType SharedName.Boxed [] ->
        Right $ TypeConstructorLayer "()"
    SharedType.TupleType SharedName.Boxed elements ->
        Right $ TypeTupleLayer elements
    SharedType.TupleType SharedName.Unboxed elements -> Left $
        "unboxed tuple reached Djinn formula compilation (arity " ++
            show (length elements) ++ ")"
    SharedType.ForallType{} ->
        Left "forall type reached Djinn formula compilation"

compileSynthesisFormula
    :: PreparedFormulaCompiler
    -> SharedType.Type HSymbol
    -> Either String Formula
compileSynthesisFormula compiler =
    compileFormula synthesisFormulaTypeView compiler .
        SharedType.canonicalizeType

synthesisFormulaTypeSymbol :: SharedName.Name -> Either String HSymbol
synthesisFormulaTypeSymbol = first show . djinnTypeConstructorSymbol

-- The compatibility operation below enters the same compiler through the raw
-- tree. Keeping this tiny view at the sealed boundary avoids retaining a raw
-- definition table or exposing package-private compiler types from HTypes.
rawFormulaTypeView :: TypeView HType
rawFormulaTypeView source = Right $ case source of
    HTApp function argument -> TypeApplicationLayer function argument
    HTVar variable -> TypeVariableLayer variable
    HTCon name -> TypeConstructorLayer name
    HTTuple types -> TypeTupleLayer types
    HTArrow argument result -> TypeArrowLayer argument result
    HTUnion constructors -> TypeUnionLayer constructors
    HTAbstract name _ -> TypeAbstractLayer name

sealPreparedEnvironment
    :: Environment
    -> PreparedInventoryExpansion
    -> Either SynthesisEnvironmentError PreparedEnvironment
sealPreparedEnvironment (Environment types _ _)
        (PreparedInventoryExpansion prepared expandedDeclarations) = do
    compiler <- first InvalidSynthesisFormulaDefinitions $
        prepareSynthesisFormulaCompiler expandedDeclarations
    -- Moving an invariant translation failure from query execution to sealing
    -- is deliberate. Kind checking plus whole-definition graph validation
    -- excludes such failures for supported declarations, and eager preparation
    -- ensures every published environment owns a complete premise cache.
    premises <- first InvalidSynthesisFormulaDefinitions $
        mapM (translateFunction compiler)
            [ signature
            | SharedDeclaration.ValueDeclaration signature <-
                expandedDeclarations
            ]
    let kindCheck = prepareKindCheckWithAssumptions types
            $ SharedInventory.inventoryKindAssumptions inventory
    classIndex <- prepareSynthesisClassIndex inventory
    -- Force the derived index before the transient compatibility projection
    -- leaves scope; its field cannot retain the complete raw Environment thunk.
    compiler `seq` classIndex `seq` kindCheck `seq`
        return (PreparedEnvironment
            prepared kindCheck classIndex premises compiler)
  where
    inventory = SharedTypeSynonym.preparedInventory prepared

    translateFunction compiler signature = do
        name <- synthesisValueSymbol FunctionOwner "function" $
            SharedDeclaration.valueName signature
        formula <- first (("function " ++ prHSymbolOp name ++ ": ") ++) $
            compileSynthesisFormula compiler $
                SharedDeclaration.valueType signature
        return (Symbol name, formula)

prepareSynthesisClassIndex
    :: SynthesisInventory
    -> Either SynthesisEnvironmentError
        (Map.Map SharedName.Name SynthesisClassDefinition)
prepareSynthesisClassIndex inventory =
    Map.fromList `fmap` mapM prepareClass
        [ (name, parameters, methods)
        | SharedDeclaration.ClassDeclaration
              _ name parameters _ methods <- declarations
        ]
  where
    declarations = SharedEnvironment.environmentDeclarations $
        SharedInventory.inventoryEnvironment inventory
    assumptions = SharedInventory.inventoryKindAssumptions inventory

    prepareClass (name, parameters, methods) = do
        let parameterNames = map SharedDeclaration.parameterVariable parameters
        kinds <- requiredClassKinds assumptions name parameterNames
        return
            ( name
            , ( zip parameterNames kinds
              , [ ( SharedDeclaration.valueName method
                  , SharedDeclaration.valueType method
                  )
                | method <- methods
                ]
              )
            )

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

-- | Native shared-type counterpart of 'checkPreparedTypesKinds'. It consumes
-- the opaque synonym table and kind assumptions from the exact prepared
-- Inventory without rebuilding a compatibility tree or a string-keyed arity
-- list.
checkPreparedSynthesisTypesKinds
    :: PreparedEnvironment
    -> [(HKind, SharedType.Type HSymbol)]
    -> Either String ()
checkPreparedSynthesisTypesKinds prepared expectedTypes = do
    mapM_ (checkSaturation . snd) expectedTypes
    obligations <- mapM convertObligation expectedTypes
    first show $ SharedInference.checkTypesKinds assumptions obligations
  where
    synonyms = preparedEnvironmentTypeSynonyms prepared
    assumptions = SharedInventory.inventoryKindAssumptions $
        preparedEnvironmentInventory prepared

    checkSaturation = first renderSynonymExpansionError .
        SharedTypeSynonym.checkTypeSynonymSaturation synonyms

    convertObligation (expected, source) = (,)
        <$> checkedGroundHKind expected
        <*> first show (normalizeSynthesisType source)

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
        (PreparedEnvironment _ _ _ _ compiler) =
    compileFormula rawFormulaTypeView compiler

-- | Translate a checked shared type directly. Stable raw and native queries
-- meet here after validation and use the exact same prepared
-- formula-definition cache.
preparedEnvironmentSynthesisFormulaTranslator
    :: PreparedEnvironment
    -> SharedType.Type HSymbol
    -> Either String Formula
preparedEnvironmentSynthesisFormulaTranslator
        (PreparedEnvironment _ _ _ _ compiler) =
    compileSynthesisFormula compiler

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

-- | Look up one class in the authoritative shared name/type index retained by
-- a sealed environment. Raw compatibility callers project only their final
-- instantiated methods; no second HType class index is retained or exposed.
lookupPreparedSynthesisClass
    :: SharedName.Name
    -> PreparedEnvironment
    -> Maybe
        ( [(HSymbol, HKind)]
        , [(SharedName.Name, SharedType.Type HSymbol)]
        )
lookupPreparedSynthesisClass name
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
    elaborated <- elaboratePreparedSynthesisTypes prepared sharedObligations
    mapM (first show . fromSynthesisType) elaborated
  where
    convertObligation (expected, source) = do
        -- Retain the raw helper's historical per-obligation precedence:
        -- reject an unsolved expected kind before inspecting its type.
        _ <- checkedGroundHKind expected
        shared <- first show $ toSynthesisType source
        return (expected, shared)

-- | Elaborate native shared types in one free-variable kind scope through the
-- exact synonym table retained by the prepared inventory.
elaboratePreparedSynthesisTypes
    :: PreparedEnvironment
    -> [(HKind, SharedType.Type HSymbol)]
    -> Either String [SharedType.Type HSymbol]
elaboratePreparedSynthesisTypes prepared obligations = do
    sharedObligations <- mapM convertObligation obligations
    first renderElaborationError $
        SharedTypeSynonym.elaborateTypes
            freshDjinnTypeVariable synonyms sharedObligations
  where
    convertObligation (expected, source) = (,)
        <$> checkedGroundHKind expected
        <*> pure source

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
        renderSynonymExpansionError expansionError

renderSynonymExpansionError
    :: SharedTypeSynonym.SynonymExpansionError HSymbol
    -> String
renderSynonymExpansionError expansionError = case expansionError of
    SharedTypeSynonym.UnsaturatedTypeSynonym name expected supplied ->
        "Type synonym " ++ SharedName.renderCanonical name ++
        " expects at least " ++ show expected ++
        " argument(s), but got " ++ show supplied
    _ -> show expansionError

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

preflightGroundDeclaration
    :: SharedDeclaration.Declaration HSymbol Void ()
    -> Either SynthesisEnvironmentError (SynthesisDeclaration, Declaration)
preflightGroundDeclaration = preflightDeclaration .
    SharedDeclaration.mapDeclarationKindVariables absurd

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
