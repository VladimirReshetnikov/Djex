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
    preparedEnvironmentSynthesisFormulaTranslator,
    preparedEnvironmentNominalSynthesisFormulaTranslator,
    preparedEnvironmentPolarizedSynthesisFormulaPlans,
    preparedEnvironmentNominalPolarizedSynthesisFormulaPlans,
    preparedEnvironmentFunctionPremises,
    preparedEnvironmentPolarizedFunctionPremises,
    preparedEnvironmentNominalPolarizedFunctionPremises,
    preparedEnvironmentQueryUsesParametricData,
    lookupPreparedSynthesisClass, synthesisMethodSymbol,
    elaboratePreparedSynthesisTypes,
    SynthesisEnvironment, SynthesisInventory,
    SynthesisEnvironmentError(..),
    toSynthesisEnvironment, toSynthesisInventory,
    declareSynthesisEnvironment, removeSynthesisDeclaration,
    declareGroundSynthesisEnvironment, removeGroundSynthesisDeclaration,
    validateEnvironment
    ) where

import Data.Bifunctor (first)
import Data.List (find)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void, absurd)
import Numeric.Natural (Natural)
import qualified Language.Haskell.Synthesis.Class as SharedClass
import qualified Language.Haskell.Synthesis.Collection as SharedCollection
import qualified Language.Haskell.Synthesis.Declaration as SharedDeclaration
import qualified Language.Haskell.Synthesis.Environment as SharedEnvironment
import qualified Language.Haskell.Synthesis.Inventory as SharedInventory
import qualified Language.Haskell.Synthesis.KindInference as SharedInference
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.TypeAtom as SharedTypeAtom
import qualified Language.Haskell.Synthesis.TypeSynonym as SharedTypeSynonym

import Djinn.Internal.Declaration
import Djinn.Internal.HCheck.Implementation
    ( AbstractTypeDefinitionError(..)
    , PreparedKindCheck
    , htCheckTypePrepared
    , htCheckTypesKindsWith
    , htInferClassKindsPrepared
    , normalizeAbstractTypeDefinitionsWith
    , prepareKindEnvironment
    )
import Djinn.Internal.HTypes
import Djinn.Internal.LJTFormula (Formula, Symbol(..))
import Djinn.Internal.TypeFormula
import Djinn.Internal.Type
    ( djinnTypeConstructorSymbol
    , freshPrimedVariable
    , normalizeSynthesisType
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
-- translated global premises, synonyms, and formula definitions cannot drift
-- away from their declarations. Raw compatibility queries retain their syntax
-- traversal but consult the Inventory's assumptions and alias table directly;
-- no second kind/synonym cache survives session sealing.
data PreparedEnvironment = PreparedEnvironment
    PreparedSynthesisInventory
    (SharedClass.PreparedClassIndex HSymbol)
    [(Symbol, Formula)]
    PreparedPolarizedPremises
    PreparedPolarizedPremises
    PreparedFormulaCompiler
    PreparedFormulaCompiler
    PreparedNominalReachability

-- A small query-directed dependency index for deciding whether the nominal
-- datatype projection is relevant. Value and aggregate-projection results are
-- matched backward from the goal; selecting a provider adds the positive
-- demand structure of its domains. The source types have already crossed
-- synonym expansion.
data PreparedNominalReachability = PreparedNominalReachability
    (Set.Set SharedName.Name)
    PreparedDatatypeProjections
    [PreparedValueFlow]

data PreparedValueFlow = PreparedValueFlow
    (Set.Set HSymbol)
    [SharedType.Type HSymbol]
    (SharedType.Type HSymbol)

data PreparedDatatypeProjection = PreparedDatatypeProjection
    [HSymbol]
    [SharedType.Type HSymbol]

type PreparedDatatypeProjections =
    Map.Map SharedName.Name PreparedDatatypeProjection

-- Alternate opaque views of a premise are safe and do not themselves weaken
-- negative evidence. The aggregate bit records only whether a declaration's
-- primary polarized translation was incomplete; exhaustion of that primary
-- approximation must not prove a negative about the source environment.
-- The spellings collect every premise-scope type variable and opened-forall
-- skolem so query-time instantiation policy can reuse them as candidates
-- without reparsing rendered atoms.
data PreparedPolarizedPremises = PreparedPolarizedPremises
    [(Symbol, Formula)]
    Bool
    [String]

-- Historical context APIs wrap final methods in 'HType' only at their
-- compatibility edge; native queries never cross this projection.
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
-- other name form are outside the compatibility declaration grammar; the
-- role-aware projection itself is owned by the declaration adapter.
synthesisValueSymbol
    :: DjinnDeclarationNameRole
    -> String
    -> SharedName.Name
    -> Either String HSymbol
synthesisValueSymbol role description name = maybe
    (Left $ "sealed " ++ description ++
        " name is not a Djinn value symbol: " ++
        SharedName.renderCanonical name)
    Right
    $ djinnDeclarationSymbol role name

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
    | SynthesisAbstractTypeNameMismatch HSymbol HSymbol
    | SynthesisAbstractTypeParameters HSymbol [HSymbol]
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
    mapM_ preflightDeclaration declarations
    expansion <- prepareInventoryExpansion inventory
    sealPreparedEnvironment expansion

-- | Validate a neutral environment once and seal it without constructing a
-- raw Djinn environment. Conversion is deliberately split into phases: every
-- declaration first crosses Djinn's lexical/feature boundary in source order,
-- then explicit kinds are grounded once before Inventory preparation. The
-- historical raw projection is reconstructed from that Inventory only when a
-- compatibility caller requests it.
--
-- Synonym declarations stay in the authoritative Inventory, whose prepared
-- witness owns their exact table. The formula translator is a derived cache,
-- while raw declaration tables are reconstructed only for compatibility
-- inspection. Aliases are
-- expanded across every declaration before sealing, both to enforce Haskell's
-- saturation rule and to classify recursive datatypes by their actual
-- (alias-free) fields.
prepareSynthesisEnvironment
    :: SynthesisEnvironment
    -> Either SynthesisEnvironmentError PreparedEnvironment
prepareSynthesisEnvironment sourceEnvironment = do
    mapM_ preflightDeclaration $
        SharedEnvironment.environmentDeclarations sourceEnvironment
    groundedEnvironment <- first
        (InvalidSynthesisInventory .
            SharedInventory.UngroundedInventoryKind) $
        SharedEnvironment.groundEnvironmentKinds sourceEnvironment
    prepareGroundSynthesisEnvironmentFrom groundedEnvironment

-- | Seal the kind-ground neutral environment accepted by the stable Djex
-- adapter. Unlike the raw compatibility entrance above, this path does not
-- weaken impossible kind variables merely to traverse and reject them again.
prepareGroundSynthesisEnvironment
    :: SharedEnvironment.Environment HSymbol Void ()
    -> Either SynthesisEnvironmentError PreparedEnvironment
prepareGroundSynthesisEnvironment sourceEnvironment = do
    mapM_ preflightGroundDeclaration $
        SharedEnvironment.environmentDeclarations sourceEnvironment
    prepareGroundSynthesisEnvironmentFrom sourceEnvironment

prepareGroundSynthesisEnvironmentFrom
    :: SharedEnvironment.Environment HSymbol Void ()
    -> Either SynthesisEnvironmentError PreparedEnvironment
prepareGroundSynthesisEnvironmentFrom sourceEnvironment = do
    inventory <- first
        (InvalidSynthesisInventory . promoteVoidInventoryError) $
        SharedInventory.mkInventoryFromEnvironmentWithClassPolicy
            SharedInference.ClosedKindInventory
            SharedInference.DefaultClassKinds sourceEnvironment
    expansion <- prepareInventoryExpansion inventory
    sealPreparedEnvironment expansion

-- | Apply one compatibility declaration directly to the shared environment,
-- then seal the resulting session state before returning it.  Djinn's
-- historical association lists put replacements first within each category;
-- The private @declareSharedDeclaration@ helper encodes that policy rather
-- than round-tripping through those lists and accidentally making them the
-- editable authority again.
declareSynthesisEnvironment
    :: Declaration
    -> SynthesisEnvironment
    -> Either SynthesisEnvironmentError
        (SynthesisEnvironment, PreparedEnvironment)
declareSynthesisEnvironment declaration sourceEnvironment = do
    sharedDeclaration <- checkedEditDeclaration declaration
    declareSharedDeclaration
        sharedDeclaration prepareSynthesisEnvironment sourceEnvironment

-- | Ground-kinded counterpart of 'declareSynthesisEnvironment' for the
-- stable adapter: the session's sealed @Void@-kinded environment is edited
-- directly, so kinds are never weakened merely to be re-grounded during
-- resealing. Only the one new declaration crosses the grounding boundary,
-- and an unsolved kind variable in it is rejected with the same error value
-- the resealing path would have produced.
declareGroundSynthesisEnvironment
    :: Declaration
    -> SharedEnvironment.Environment HSymbol Void ()
    -> Either SynthesisEnvironmentError
        (SharedEnvironment.Environment HSymbol Void (), PreparedEnvironment)
declareGroundSynthesisEnvironment declaration sourceEnvironment = do
    sharedDeclaration <- checkedEditDeclaration declaration
    groundDeclaration <- first
        (InvalidSynthesisInventory . SharedInventory.UngroundedInventoryKind)
        $ SharedDeclaration.groundDeclarationKinds sharedDeclaration
    declareSharedDeclaration
        groundDeclaration prepareGroundSynthesisEnvironment sourceEnvironment

-- The unit guard and lexical conversion shared by both declare entrances.
-- The canonical unit declaration is representable only so trusted raw
-- environments can cross the shared boundary; public editing must not use
-- that representational exception to install grammar-level @()@.
checkedEditDeclaration
    :: Declaration
    -> Either SynthesisEnvironmentError SynthesisDeclaration
checkedEditDeclaration declaration = do
    if declaration == canonicalUnitDeclaration
        then Left ProtectedSynthesisUnitDeclaration
        else Right ()
    first SynthesisEnvironmentDeclarationError $
        toSynthesisDeclaration declaration

-- The kind-polymorphic replacement transaction. Both public entrances share
-- this exact body, so the replacement-first-within-category policy cannot
-- drift between the raw and ground paths; only the resealing operation is
-- entrance-specific.
declareSharedDeclaration
    :: SharedDeclaration.Declaration HSymbol kindVariable ()
    -> (SharedEnvironment.Environment HSymbol kindVariable ()
        -> Either SynthesisEnvironmentError PreparedEnvironment)
    -> SharedEnvironment.Environment HSymbol kindVariable ()
    -> Either SynthesisEnvironmentError
        ( SharedEnvironment.Environment HSymbol kindVariable ()
        , PreparedEnvironment
        )
declareSharedDeclaration sharedDeclaration reseal sourceEnvironment = do
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
    prepared <- reseal candidate
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
removeSynthesisDeclaration sourceName =
    removeSharedDeclaration sourceName prepareSynthesisEnvironment

-- | Ground-kinded counterpart of 'removeSynthesisDeclaration' for the stable
-- adapter. Removal introduces no kinds, so the ground path needs no
-- conversion at all.
removeGroundSynthesisDeclaration
    :: HSymbol
    -> SharedEnvironment.Environment HSymbol Void ()
    -> Either SynthesisEnvironmentError
        (SharedEnvironment.Environment HSymbol Void (), PreparedEnvironment)
removeGroundSynthesisDeclaration sourceName =
    removeSharedDeclaration sourceName prepareGroundSynthesisEnvironment

removeSharedDeclaration
    :: HSymbol
    -> (SharedEnvironment.Environment HSymbol kindVariable ()
        -> Either SynthesisEnvironmentError PreparedEnvironment)
    -> SharedEnvironment.Environment HSymbol kindVariable ()
    -> Either SynthesisEnvironmentError
        ( SharedEnvironment.Environment HSymbol kindVariable ()
        , PreparedEnvironment
        )
removeSharedDeclaration sourceName reseal sourceEnvironment = do
    owner <- case SharedName.parseName sourceName of
        Left _ -> Left $ SynthesisDeclarationNotFound sourceName
        Right name -> Right name
    declarations <- mapM attachOwner $
        SharedEnvironment.environmentDeclarations sourceEnvironment
    let matching = [declaration |
            (declaration, candidateOwner) <- declarations,
            candidateOwner == owner]
    if null matching then Left $ SynthesisDeclarationNotFound sourceName
    -- 'parseName' deliberately accepts surrounding whitespace. Protect the
    -- declaration we actually resolved rather than only its canonical raw
    -- spelling, or a caller could delete the wired-in unit with @" () "@.
    else if SharedName.nameSpecial owner == Just
            (SharedName.TupleConstructor SharedName.Boxed 0)
        then Left ProtectedSynthesisUnit
    else do
        candidate <- first InvalidSynthesisEnvironment $
            SharedEnvironment.mkEnvironment
                [ declaration
                | (declaration, candidateOwner) <- declarations
                , candidateOwner /= owner
                ]
        prepared <- reseal candidate
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
    :: SharedDeclaration.Declaration HSymbol kindVariable annotation
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

-- Use the foundation's single prepared-and-expanded view before applying
-- Djinn's stricter no-recursive-datatypes policy. Keep projecting both shared
-- synonym phases to the historical raw error constructor; Exference's public
-- vocabulary already preserves the additional declaration attribution.
prepareInventoryExpansion
    :: SynthesisInventory
    -> Either SynthesisEnvironmentError
        PreparedInventoryExpansion
prepareInventoryExpansion inventory = do
    expansion <- first promoteExpansionError $
        SharedTypeSynonym.prepareInventoryExpansion
            freshDjinnTypeVariable inventory
    let recursiveNames =
            SharedTypeSynonym.inventoryExpansionRecursiveDataTypeNames expansion
    if Set.null recursiveNames then
        return expansion
    else
        Left $ RecursiveSynthesisDataTypes $ Set.toAscList recursiveNames
  where
    promoteExpansionError failure = case failure of
        SharedTypeSynonym.InventorySynonymPreparationError cause ->
            InvalidSynthesisTypeSynonyms cause
        SharedTypeSynonym.InventoryDeclarationExpansionError
                _ _ cause ->
            InvalidSynthesisTypeSynonyms cause

type PreparedInventoryExpansion =
    SharedTypeSynonym.PreparedInventoryExpansion HSymbol ()

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

-- Build the complementary nominal-family compiler from the same checked
-- declaration stream. Synonyms remain definitions so aliases still normalize
-- to their real heads, while parametric datatypes are deliberately absent: a
-- saturated application such as @MaybeLike (forall a. a -> a)@ therefore
-- follows the ordinary unknown-head path and becomes one alpha-aware atom
-- containing the complete application. Nullary datatypes remain structural;
-- without an argument to preserve they need no second projection. Abstract
-- declarations retain their historical handling. This is a search projection
-- only; the structural compiler above remains authoritative for constructor
-- introduction and elimination.
prepareNominalSynthesisFormulaCompiler
    :: [SharedDeclaration.Declaration HSymbol kindVariable annotation]
    -> Either String PreparedFormulaCompiler
prepareNominalSynthesisFormulaCompiler declarations = do
    definitions <- concat `fmap`
        mapM nominalFormulaDefinition declarations
    prepareFormulaCompiler synthesisFormulaTypeView definitions
  where
    nominalFormulaDefinition declaration = case declaration of
        SharedDeclaration.DataTypeDeclaration _ _ (_ : _) _ -> Right []
        _ -> synthesisFormulaDefinition declaration

prepareValueFlows
    :: PreparedDatatypeProjections
    -> SharedType.Type HSymbol
    -> [PreparedValueFlow]
prepareValueFlows = prepareValueFlowsWithFreeVariables True

prepareLocalValueFlows
    :: PreparedDatatypeProjections
    -> SharedType.Type HSymbol
    -> [PreparedValueFlow]
prepareLocalValueFlows = prepareValueFlowsWithFreeVariables False

prepareValueFlowsWithFreeVariables
    :: Bool
    -> PreparedDatatypeProjections
    -> SharedType.Type HSymbol
    -> [PreparedValueFlow]
prepareValueFlowsWithFreeVariables includeFreeVariables projections source =
    positiveResultFlows projections Set.empty flexible domains result
  where
    canonical = SharedType.canonicalizeType source
    (outerBinders, _, body) = SharedType.splitLeadingForalls canonical
    (domains, result) = SharedType.functionSpine body
    flexible = Set.fromList outerBinders `Set.union` if includeFreeVariables
        then SharedType.freeVariables canonical
        else Set.empty

-- Function parameters introduced while constructing the positive query are
-- local value providers. Enclosing query binders remain rigid because this
-- traversal returns only the parameter type itself; an explicit forall inside
-- that parameter is reopened later by 'prepareLocalValueFlows'. Class
-- constraints do not introduce term providers in Djinn.
queryLocalHypotheses
    :: SharedType.Type HSymbol
    -> [SharedType.Type HSymbol]
queryLocalHypotheses source = case SharedType.canonicalizeType source of
    SharedType.ForallType _ _ body -> queryLocalHypotheses body
    SharedType.FunctionType parameter result ->
        parameter : queryLocalHypotheses result
    SharedType.TupleType _ elements -> concatMap queryLocalHypotheses elements
    _ -> []

-- A loaded aggregate can provide more than its complete result. Tuple
-- elimination exposes each element, and an exposed function can in turn
-- produce its positive result after its parameter is supplied. Retain the
-- aggregate/function itself as a route too: a query may consume that complete
-- value directly. Leading result-local foralls are specialization variables,
-- consistently with ordinary value-flow matching.
positiveResultFlows
    :: PreparedDatatypeProjections
    -> Set.Set SharedName.Name
    -> Set.Set HSymbol
    -> [SharedType.Type HSymbol]
    -> SharedType.Type HSymbol
    -> [PreparedValueFlow]
positiveResultFlows projections visited flexible domains source =
    PreparedValueFlow flexible' domains body : case body of
        SharedType.FunctionType parameter result ->
            positiveResultFlows projections visited flexible'
                (parameter : domains) result
        SharedType.TupleType _ elements -> concatMap
            (positiveResultFlows projections visited flexible' domains)
            elements
        _ -> datatypeFieldFlows body
  where
    canonical = SharedType.canonicalizeType source
    (binders, _, body) = SharedType.splitLeadingForalls canonical
    flexible' = Set.fromList binders `Set.union` flexible

    -- Project fields only from this loaded value's actual positive owner
    -- occurrence. A declaration such as @data Box a = Box a@ therefore cannot
    -- make an arbitrary rigid goal relevant unless some loaded value first
    -- provides @Box t@. The visited-head guard keeps even future recursive
    -- declaration support finite. Multi-constructor fields deliberately
    -- overapproximate after that owner evidence exists; proof search remains
    -- authoritative about whether all branches can be eliminated.
    datatypeFieldFlows typeExpression = case
            SharedType.applicationSpine typeExpression of
        (SharedType.TypeConstructor owner, arguments)
            | owner `Set.notMember` visited
            , Just (PreparedDatatypeProjection parameters fields) <-
                Map.lookup owner projections
            , length parameters == length arguments -> concatMap
                (positiveResultFlows projections
                    (Set.insert owner visited) flexible' domains .
                    specialize parameters arguments)
                fields
        _ -> []

    specialize parameters arguments field = case
            SharedType.substituteTypeVariables
                (\reserved variable -> Just $ fst $
                    freshPrimedVariable reserved variable)
                Set.empty
                (Map.fromList $ zip parameters arguments)
                field of
        Right specialized -> specialized
        Left failure -> error $
            "Djinn datatype projection substitution invariant failed: " ++
                show failure

-- Positive demand structure only: when constructing a function, its result is
-- demanded and its parameter becomes a local hypothesis. The complete type is
-- retained as well so a loaded value can discharge the whole demand directly.
demandStructure
    :: SharedType.Type HSymbol
    -> [SharedType.Type HSymbol]
demandStructure source = SharedCollection.distinctOn
    SharedTypeAtom.alphaTypeKey $ canonical : nested canonical
  where
    canonical = SharedType.canonicalizeType source
    nested typeExpression = case typeExpression of
        SharedType.FunctionType _ result -> demandStructure result
        SharedType.TupleType _ elements -> concatMap demandStructure elements
        SharedType.ForallType _ _ body -> demandStructure body
        _ -> []

-- Directional first-order matching is enough for reachability: a provider's
-- explicit, result-local, and implicit variables may instantiate to the
-- current demand, while constructors and rigid structure must agree. Nested
-- quantified types remain alpha-aware atoms, matching the guarded
-- instantiation policy used by proof search.
flowMatch
    :: PreparedValueFlow
    -> SharedType.Type HSymbol
    -> Maybe (Map.Map HSymbol (SharedType.Type HSymbol))
flowMatch (PreparedValueFlow flexible _ result) demand =
    matchReachableType flexible Map.empty result demand

matchReachableType
    :: Set.Set HSymbol
    -> Map.Map HSymbol (SharedType.Type HSymbol)
    -> SharedType.Type HSymbol
    -> SharedType.Type HSymbol
    -> Maybe (Map.Map HSymbol (SharedType.Type HSymbol))
matchReachableType flexible substitutions source target =
    case (SharedType.canonicalizeType source,
            SharedType.canonicalizeType target) of
        (SharedType.TypeVariable variable, targetType)
            | variable `Set.member` flexible ->
                case Map.lookup variable substitutions of
                    Nothing -> Just $ Map.insert variable targetType
                        substitutions
                    Just previous
                        | SharedTypeAtom.alphaTypeKey previous ==
                            SharedTypeAtom.alphaTypeKey targetType ->
                                Just substitutions
                        | otherwise -> Nothing
        (SharedType.TypeVariable left, SharedType.TypeVariable right)
            | left == right -> Just substitutions
        (SharedType.TypeConstructor left, SharedType.TypeConstructor right)
            | left == right -> Just substitutions
        (SharedType.TypeApplication leftFunction leftArgument,
                SharedType.TypeApplication rightFunction rightArgument) -> do
            afterFunction <- matchReachableType flexible substitutions
                leftFunction rightFunction
            matchReachableType flexible afterFunction
                leftArgument rightArgument
        (SharedType.FunctionType leftParameter leftResult,
                SharedType.FunctionType rightParameter rightResult) -> do
            afterParameter <- matchReachableType flexible substitutions
                leftParameter rightParameter
            matchReachableType flexible afterParameter leftResult rightResult
        (SharedType.TupleType leftBoxity leftElements,
                SharedType.TupleType rightBoxity rightElements)
            | leftBoxity == rightBoxity ->
                matchReachableTypes flexible substitutions
                    leftElements rightElements
        (left@SharedType.ForallType{}, right@SharedType.ForallType{})
            | SharedTypeAtom.alphaTypeKey left ==
                SharedTypeAtom.alphaTypeKey right -> Just substitutions
        _ -> Nothing

matchReachableTypes
    :: Set.Set HSymbol
    -> Map.Map HSymbol (SharedType.Type HSymbol)
    -> [SharedType.Type HSymbol]
    -> [SharedType.Type HSymbol]
    -> Maybe (Map.Map HSymbol (SharedType.Type HSymbol))
matchReachableTypes _ substitutions [] [] = Just substitutions
matchReachableTypes flexible substitutions
        (source : sources) (target : targets) = do
    next <- matchReachableType flexible substitutions source target
    matchReachableTypes flexible next sources targets
matchReachableTypes _ _ _ _ = Nothing

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
    SharedType.ForallType [] [] body -> synthesisFormulaTypeView body
    quantified@SharedType.ForallType{} -> TypeForallLayer <$>
        first show (SharedTypeAtom.mkTypeAtom quantified)

compileSynthesisFormula
    :: PreparedFormulaCompiler
    -> SharedType.Type HSymbol
    -> Either String Formula
compileSynthesisFormula compiler =
    compileFormula synthesisFormulaTypeView compiler .
        SharedType.canonicalizeType

compilePolarizedSynthesisFormulaPlans
    :: Natural
    -> FormulaPolarity
    -> PreparedFormulaCompiler
    -> SharedType.Type HSymbol
    -> Either String PolarizedFormulaPlans
compilePolarizedSynthesisFormulaPlans namespace polarity compiler =
    compilePolarizedFormulaPlans namespace polarity
        synthesisFormulaTypeView synthesisFormulaTypeView compiler .
            SharedType.canonicalizeType

synthesisFormulaTypeSymbol :: SharedName.Name -> Either String HSymbol
synthesisFormulaTypeSymbol = first show . djinnTypeConstructorSymbol

sealPreparedEnvironment
    :: PreparedInventoryExpansion
    -> Either SynthesisEnvironmentError PreparedEnvironment
sealPreparedEnvironment expansion = do
    compiler <- first InvalidSynthesisFormulaDefinitions $
        prepareSynthesisFormulaCompiler expandedDeclarations
    nominalCompiler <- first InvalidSynthesisFormulaDefinitions $
        prepareNominalSynthesisFormulaCompiler expandedDeclarations
    -- Moving an invariant translation failure from query execution to sealing
    -- is deliberate. Kind checking plus whole-definition graph validation
    -- excludes such failures for supported declarations, and eager preparation
    -- ensures every published environment owns a complete premise cache.
    translatedPremises <- first InvalidSynthesisFormulaDefinitions $
        mapM (translateFunction compiler) $ zip [1 ..]
            [ signature
            | SharedDeclaration.ValueDeclaration signature <-
                expandedDeclarations
            ]
    nominalTranslatedPremises <- first InvalidSynthesisFormulaDefinitions $
        mapM (translateFunction nominalCompiler) $ zip [1 ..]
            [ signature
            | SharedDeclaration.ValueDeclaration signature <-
                expandedDeclarations
            ]
    let premises = map opaquePremise translatedPremises
        polarizedPremises = preparePolarizedPremises translatedPremises
        nominalPolarizedPremises =
            preparePolarizedPremises nominalTranslatedPremises
    let classIndex = SharedClass.prepareClassIndex inventory
    -- Djinn uses Haskell-98 kind defaulting, so every parameter must have a
    -- ground kind. Check that backend-specific requirement at sealing while
    -- retaining the neutral index's generalized-kind vocabulary.
    mapM_ (fmap (const ()) . projectPreparedSynthesisClass) $
        SharedClass.preparedClasses classIndex
    -- Force each retained projection so it cannot keep the transient
    -- expanded declaration product alive through an unevaluated selector.
    prepared `seq` compiler `seq` nominalCompiler `seq` classIndex `seq`
        nominalReachability `seq`
        return (PreparedEnvironment
            prepared classIndex premises polarizedPremises
                nominalPolarizedPremises compiler nominalCompiler
                nominalReachability)
  where
    prepared =
        SharedTypeSynonym.inventoryExpansionPreparedInventory expansion
    expandedDeclarations =
        SharedTypeSynonym.inventoryExpansionDeclarations expansion
    inventory = SharedTypeSynonym.preparedInventory prepared
    parametricDataNames = Set.fromList
        [ name
        | SharedDeclaration.DataTypeDeclaration _ name (_ : _) _ <-
            expandedDeclarations
        ]
    datatypeProjections = Map.fromList
        [ ( owner
          , PreparedDatatypeProjection
                (map SharedDeclaration.parameterVariable parameters)
                (concatMap SharedDeclaration.constructorFields constructors)
          )
        | SharedDeclaration.DataTypeDeclaration
            _ owner parameters constructors <- expandedDeclarations
        ]
    nominalReachability = PreparedNominalReachability
        parametricDataNames
        datatypeProjections
        [ flow
        | SharedDeclaration.ValueDeclaration signature <- expandedDeclarations
        , flow <- prepareValueFlows datatypeProjections
            (SharedDeclaration.valueType signature)
        ]

    translateFunction compiler (namespace, signature) = do
        name <- synthesisValueSymbol FunctionOwner "function" $
            SharedDeclaration.valueName signature
        let sourceType = SharedDeclaration.valueType signature
            (_, constraints, _) = SharedType.splitLeadingForalls sourceType
        if null constraints then pure () else Left $
            "function " ++ prHSymbolOp name ++
                ": constrained premises are unsupported"
        implicit <- first
            ((("function " ++ prHSymbolOp name ++ ": ") ++) . show) $
            fmap fst $ SharedType.implicitizeLeadingForalls
                (const (Nothing :: Maybe ())) freshBinder mempty sourceType
        let (_, _, body) = SharedType.splitLeadingForalls implicit
        plans <- first (("function " ++ prHSymbolOp name ++ ": ") ++) $
            compilePolarizedSynthesisFormulaPlans namespace NegativeFormula
                compiler body
        let spellings =
                SharedType.freeVariablesInFirstOccurrenceOrder body ++
                polarizedFormulaPlanSkolems plans
        return (Symbol name, plans, spellings)

    opaquePremise (symbol, plans, _) =
        (symbol, exactOpaqueFormulaPlan plans)

    preparePolarizedPremises translated = PreparedPolarizedPremises
        ( concatMap primaryPremise premiseVariantGroups ++
            concatMap alternatePremises premiseVariantGroups
        )
        (any premiseTranslationIncomplete translated)
        (SharedCollection.distinctOn id $
            concatMap premiseCandidateSpellings translated)
      where
        premiseVariantGroups = map polarizedPremiseVariants translated

    polarizedPremiseVariants (symbol, plans, _) =
        [ (symbol, formula)
        | formula <- SharedCollection.distinctOn id $
            map translatedFormula
                (primaryFormulaPlan plans : singleOpaqueFormulaPlans plans) ++
            [exactOpaqueFormulaPlan plans] ++
            map translatedFormula (singleOpenFormulaPlans plans) ++
            map translatedFormula (pairOpaqueFormulaPlans plans) ++
            map translatedFormula (pairOpenFormulaPlans plans) ++
            map translatedFormula (tripleOpaqueFormulaPlans plans) ++
            map translatedFormula (tripleOpenFormulaPlans plans)
        ]
    primaryPremise variants = case variants of
        primary : _ -> [primary]
        [] -> []
    alternatePremises variants = case variants of
        _ : alternatives -> alternatives
        [] -> []
    premiseTranslationIncomplete (_, plans, _) =
        translationIncomplete $ primaryFormulaPlan plans
    premiseCandidateSpellings (_, _, spellings) = spellings

    freshBinder reserved variable = Just
        $ fst $ freshPrimedVariable reserved variable

projectPreparedSynthesisClass
    :: SharedClass.PreparedClass HSymbol
    -> Either SynthesisEnvironmentError SynthesisClassDefinition
projectPreparedSynthesisClass preparedClass = do
    parameters <- mapM projectParameter $
        SharedClass.preparedClassParameters preparedClass
    return (parameters, SharedClass.preparedClassMethods preparedClass)
  where
    className = SharedClass.preparedClassName preparedClass

    projectParameter (parameter, Nothing) = Left $
        UnresolvedSynthesisClassKind className parameter
    projectParameter (parameter, Just kind) =
        Right (parameter, fromGroundHKind kind)

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

preparedEnvironmentWitness
    :: PreparedEnvironment
    -> PreparedSynthesisInventory
preparedEnvironmentWitness
        (PreparedEnvironment prepared _ _ _ _ _ _ _) =
    prepared

preparedEnvironmentInventory :: PreparedEnvironment -> SynthesisInventory
preparedEnvironmentInventory =
    SharedTypeSynonym.preparedInventory .
        preparedEnvironmentWitness

-- | Whether an already elaborated query can reach a datatype with at least
-- one parameter. The goal is checked directly, then its positive result
-- structure seeds a backward slice through loaded value codomains and
-- domains. This admits closed goals whose consumer/provider chain contains
-- the datatype without letting unrelated sealed premises perturb historical
-- searches. Callers elaborate aliases first, so aliases remain transparent.
preparedEnvironmentQueryUsesParametricData
    :: PreparedEnvironment
    -> SharedType.Type HSymbol
    -> Bool
preparedEnvironmentQueryUsesParametricData
        (PreparedEnvironment _ _ _ _ _ _ _
            (PreparedNominalReachability
                parametricDataNames datatypeProjections valueFlows))
        source =
    reachesParametricData (demandStructure source) $
        localValueFlows ++ valueFlows
  where
    -- A function parameter in the elaborated query is already available when
    -- its result is constructed, so its complete value and finite aggregate
    -- projections are zero-domain providers. Build those projections from the
    -- same specialized declaration templates as loaded values. Merely
    -- declaring a parametric datatype still contributes no flow of its own.
    localValueFlows =
        [ flow
        | hypothesis <- queryLocalHypotheses source
        , flow <- prepareLocalValueFlows datatypeProjections hypothesis
        ]

    reachesParametricData frontier remaining
        | any usesParametricData frontier = True
        | null reached = False
        | any (flowUsesParametricData . fst) reached = True
        | otherwise = reachesParametricData expanded unreached
      where
        reached =
            [ (flow, matches)
            | flow <- remaining
            , let matches =
                    [ substitutions
                    | demand <- frontier
                    , Just substitutions <- [flowMatch flow demand]
                    ]
            , not $ null matches
            ]
        unreached =
            [ flow
            | flow <- remaining
            , not $ any (hasFlowMatch flow) frontier
            ]
        expanded = SharedCollection.distinctOn SharedTypeAtom.alphaTypeKey $
            frontier ++
                [ demand
                | (flow, matches) <- reached
                , substitutions <- matches
                , demand <- flowDemands flow substitutions
                ]

    hasFlowMatch flow demand = case flowMatch flow demand of
        Just _ -> True
        Nothing -> False

    usesParametricData typeExpression = not $ Set.null $
        SharedType.typeConstructors typeExpression `Set.intersection`
            parametricDataNames
    flowUsesParametricData (PreparedValueFlow _ domains result) =
        any usesParametricData $ result : domains
    flowDemands (PreparedValueFlow _ domains _) substitutions =
        concatMap (demandStructure . specialize substitutions) domains

    specialize substitutions typeExpression = case
            SharedType.substituteTypeVariables
                (\reserved variable -> Just $ fst $
                    freshPrimedVariable reserved variable)
                Set.empty substitutions typeExpression of
        Right specialized -> specialized
        Left failure -> error $
            "Djinn nominal reachability substitution invariant failed: " ++
                show failure

preparedEnvironmentKindAssumptions
    :: PreparedEnvironment
    -> SharedInference.KindAssumptions
preparedEnvironmentKindAssumptions =
    SharedInventory.inventoryKindAssumptions .
        preparedEnvironmentInventory

-- | Check a raw batch in one kind scope against this exact sealed witness.
-- The raw traversal preserves compatibility diagnostics, while synonym facts
-- and kind assumptions remain owned by the shared prepared inventory.
checkPreparedTypesKinds
    :: PreparedEnvironment
    -> [(HKind, HType)]
    -> Either String ()
checkPreparedTypesKinds prepared =
    htCheckTypesKindsWith checkApplication assumptions
  where
    foundation = preparedEnvironmentWitness prepared
    assumptions = preparedEnvironmentKindAssumptions prepared

    checkApplication sourceName supplied =
        case SharedName.parseName sourceName of
            -- Malformed raw names are not aliases. Leave their diagnostic to
            -- structural conversion after the complete saturation walk.
            Left _ -> Right ()
            Right name -> first renderSynonymExpansionError $
                SharedTypeSynonym.checkPreparedTypeSynonymApplicationSaturation
                    foundation name supplied

-- | Native shared-type counterpart of 'checkPreparedTypesKinds'. It consumes
-- the exact prepared Inventory without rebuilding a compatibility tree or a
-- string-keyed arity list.
checkPreparedSynthesisTypesKinds
    :: PreparedEnvironment
    -> [(HKind, SharedType.Type HSymbol)]
    -> Either String ()
checkPreparedSynthesisTypesKinds prepared expectedTypes = do
    mapM_ (checkSaturation . snd) expectedTypes
    obligations <- mapM convertObligation expectedTypes
    first show $ SharedInference.checkTypesKinds assumptions obligations
  where
    foundation = preparedEnvironmentWitness prepared
    assumptions = preparedEnvironmentKindAssumptions prepared

    checkSaturation = first renderSynonymExpansionError .
        SharedTypeSynonym.checkPreparedTypeSynonymSaturation foundation

    convertObligation (expected, source) = (,)
        <$> checkedGroundHKind expected
        <*> first show (normalizeSynthesisType source)

preparedEnvironmentFunctionPremises
    :: PreparedEnvironment
    -> [(Symbol, Formula)]
preparedEnvironmentFunctionPremises
        (PreparedEnvironment _ _ premises _ _ _ _ _) = premises

-- | Every sound rank-N view of each premise, with all historical primary views
-- first in declaration order, paired with whether any primary translation had
-- to leave a quantified subtree opaque, and with the premise-scope variable
-- and skolem spellings available to query-time instantiation policy.
preparedEnvironmentPolarizedFunctionPremises
    :: PreparedEnvironment
    -> ([(Symbol, Formula)], Bool, [String])
preparedEnvironmentPolarizedFunctionPremises
        (PreparedEnvironment _ _ _
            (PreparedPolarizedPremises premises incomplete spellings)
            _ _ _ _) =
        (premises, incomplete, spellings)

-- | The complementary premise cache in which datatype applications retain
-- their complete nominal heads and arguments. It is searched only by the
-- matching nominal goal family, after the primary historical structural plan.
preparedEnvironmentNominalPolarizedFunctionPremises
    :: PreparedEnvironment
    -> ([(Symbol, Formula)], Bool, [String])
preparedEnvironmentNominalPolarizedFunctionPremises
        (PreparedEnvironment _ _ _ _
            (PreparedPolarizedPremises premises incomplete spellings)
            _ _ _) =
        (premises, incomplete, spellings)

-- | Translate a checked shared type directly. Stable raw and native queries
-- meet here after raw compatibility validation and use the exact same
-- prepared formula-definition cache. The historical unchecked formula
-- operation remains 'hTypeToFormula'; a sealed environment no longer exposes
-- a second raw entrance that production query execution does not use.
preparedEnvironmentSynthesisFormulaTranslator
    :: PreparedEnvironment
    -> SharedType.Type HSymbol
    -> Either String Formula
preparedEnvironmentSynthesisFormulaTranslator
        (PreparedEnvironment _ _ _ _ _ compiler _ _) =
    compileSynthesisFormula compiler

-- | Translate with datatypes retained as complete nominal applications. This
-- translator must accompany the nominal goal and premise projection when
-- constructing erased hypothesis-instantiation axioms.
preparedEnvironmentNominalSynthesisFormulaTranslator
    :: PreparedEnvironment
    -> SharedType.Type HSymbol
    -> Either String Formula
preparedEnvironmentNominalSynthesisFormulaTranslator
        (PreparedEnvironment _ _ _ _ _ _ compiler _) =
    compileSynthesisFormula compiler

-- | Translate a checked positive goal into one nonempty, categorized plan
-- family.  Consumers retain the historical primary/exact/singleton prefix and
-- append the pairwise and triple polynomial tails without guessing category
-- boundaries in a flat list.
preparedEnvironmentPolarizedSynthesisFormulaPlans
    :: PreparedEnvironment
    -> SharedType.Type HSymbol
    -> Either String PolarizedFormulaPlans
preparedEnvironmentPolarizedSynthesisFormulaPlans
        (PreparedEnvironment _ _ _ _ _ compiler _ _) =
    compilePolarizedSynthesisFormulaPlans 0 PositiveFormula compiler

-- | Compile the complementary positive goal family without unfolding
-- parametric datatypes. The caller marks this approximation unsuitable for
-- negative evidence and schedules it after the primary historical structural
-- plan.
preparedEnvironmentNominalPolarizedSynthesisFormulaPlans
    :: PreparedEnvironment
    -> SharedType.Type HSymbol
    -> Either String PolarizedFormulaPlans
preparedEnvironmentNominalPolarizedSynthesisFormulaPlans
        (PreparedEnvironment _ _ _ _ _ _ compiler _) =
    compilePolarizedSynthesisFormulaPlans 0 PositiveFormula compiler

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
        (PreparedEnvironment _ classes _ _ _ _ _ _) = do
    preparedClass <- SharedClass.lookupPreparedClass name classes
    case projectPreparedSynthesisClass preparedClass of
        Right projected -> Just projected
        Left failure -> error $
            "Djinn.Internal.Environment.lookupPreparedSynthesisClass: " ++
            "sealed class-index invariant failed: " ++ show failure

-- | Elaborate native shared types in one free-variable kind scope through the
-- exact prepared inventory.
elaboratePreparedSynthesisTypes
    :: PreparedEnvironment
    -> [(HKind, SharedType.Type HSymbol)]
    -> Either String [SharedType.Type HSymbol]
elaboratePreparedSynthesisTypes prepared obligations = do
    sharedObligations <- mapM convertObligation obligations
    first renderElaborationError $
        SharedTypeSynonym.elaboratePreparedTypes
            freshDjinnTypeVariable foundation sharedObligations
  where
    convertObligation (expected, source) = (,)
        <$> checkedGroundHKind expected
        <*> pure source

    foundation = preparedEnvironmentWitness prepared

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
synthesisDeclarations environment = do
    normalizedTypes <- first abstractTypeDefinitionFailure $
        normalizeAbstractTypeDefinitionsWith
            (\_ embeddedKind -> embeddedKind) (envTypes environment)
    mapM convertedDeclaration $
        map typeDeclaration normalizedTypes ++
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

    abstractTypeDefinitionFailure failure = case failure of
        AbstractTypeDefinitionNameMismatch outerName embeddedName ->
            SynthesisAbstractTypeNameMismatch outerName embeddedName
        AbstractTypeDefinitionHasParameters name parameters ->
            SynthesisAbstractTypeParameters name parameters

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
freshDjinnTypeVariable reserved variable =
    Just $ fst $ freshPrimedVariable reserved variable

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

-- | Preserve the historical raw research API's validation and diagnostic
-- order. Stable edits and sessions use 'prepareEnvironment' instead; this
-- compatibility boundary deliberately checks value namespaces before raw
-- type inference, axioms, classes, and the final shared structural seal.
-- Class parameter kinds and the outer kind caches are still refreshed in the
-- returned projection.
validateEnvironment ::
    [TypeDefinition] -> [Axiom] -> [ClassDefinition] ->
    Either String ([TypeDefinition], [ClassDefinition])
validateEnvironment definitions axioms classes = do
    checkValueNamespace axioms classes
    (checked, prepared) <- withContext "type environment" $
        prepareKindEnvironment definitions
    mapM_ (checkAxiom prepared) axioms
    refreshed <- mapM (checkClass prepared) classes
    -- Preserve the compatibility checker's established error order by
    -- running its kind-dependent phases first.  The final shared preflight
    -- then seals the namespaces that those historical phases do not index:
    -- constructors across datatypes, constructors within one datatype, and
    -- the common type/class owner namespace.  Reusing the same environment
    -- constructor as the stable API prevents the two entrances from growing
    -- different structural rules.
    checkSharedDeclarationStructure checked axioms refreshed
    return (checked, refreshed)

checkSharedDeclarationStructure
    :: [TypeDefinition] -> [Axiom] -> [ClassDefinition] -> Either String ()
checkSharedDeclarationStructure definitions axioms classes =
    case toSynthesisEnvironment Environment
            { envTypes = definitions
            , envFunctions = axioms
            , envClasses = classes
            } of
        Left failure -> Left $ renderSharedStructureFailure failure
        Right _ -> Right ()

-- Keep the two namespace collisions that callers can act on consistent with
-- the public editing API.  Less common declaration-local failures retain
-- their structured rendering rather than duplicating the shared validator's
-- complete diagnostic vocabulary here.
renderSharedStructureFailure :: SynthesisEnvironmentError -> String
renderSharedStructureFailure failure = case failure of
    InvalidSynthesisEnvironment
            (SharedEnvironment.DuplicateTypeDeclaration name) ->
        "Type name is already declared: " ++
            SharedName.renderCanonical name
    InvalidSynthesisEnvironment
            (SharedEnvironment.DuplicateValueDeclaration name) ->
        "Value name is already declared: " ++
            SharedName.renderCanonical name
    _ -> show failure

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
