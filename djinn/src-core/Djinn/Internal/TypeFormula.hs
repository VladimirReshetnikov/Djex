--
-- Copyright (c) 2005 Lennart Augustsson
-- See LICENSE for licensing details.
--
-- Representation-neutral compilation of Djinn source types into LJT formulae.
--
-- The public compatibility surface still accepts 'HType', while checked Djex
-- sessions use the shared synthesis type tree.  Both representations enter
-- this package-private engine through a one-layer view and share the same
-- validated definition cache, expansion provenance, and atom renderer.
--
module Djinn.Internal.TypeFormula
    ( TypeLayer(..)
    , TypeView
    , FormulaDefinition(..)
    , PreparedFormulaCompiler
    , FormulaPolarity(..)
    , FormulaTranslation
    , translatedFormula
    , translationIncomplete
    , translationIntroducedSkolems
    , PolarizedFormulaPlans
    , primaryFormulaPlan
    , exactOpaqueFormulaPlan
    , singleOpaqueFormulaPlans
    , singleOpenFormulaPlans
    , pairOpaqueFormulaPlans
    , pairOpenFormulaPlans
    , tripleOpaqueFormulaPlans
    , tripleOpenFormulaPlans
    , polarizedFormulaPlanSkolems
    , prepareFormulaCompiler
    , prepareFormulaCompilerWithRecursiveData
    , compileFormula
    , compilePolarizedFormulaPlans
    ) where

import Control.Monad (zipWithM)
import Data.Bifunctor (first)
import Data.Graph (SCC(..), stronglyConnComp)
import Data.List (intercalate, isSuffixOf, sortOn)
import qualified Data.Map.Lazy as LazyMap
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Numeric.Natural (Natural)

import Djinn.Internal.HIdentifier (isQualifiedConId)
import Djinn.Internal.LJTFormula
import qualified Language.Haskell.Synthesis.Collection as SharedCollection
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Type as SharedType
import qualified Language.Haskell.Synthesis.TypeAtom as SharedTypeAtom
import qualified Language.Haskell.Synthesis.TypeRender as SharedTypeRender

-- | A single observable layer of a source type.  Keeping children in the
-- caller's representation avoids introducing another source-facing recursive
-- tree merely to connect the legacy and shared frontends.
data TypeLayer source
    = TypeApplicationLayer source source
    | TypeVariableLayer String
    | TypeConstructorLayer String
    | TypeTupleLayer [source]
    | TypeArrowLayer source source
    | TypeForallLayer (SharedTypeAtom.TypeAtom String)
    | TypeUnionLayer [(String, [source])]
    | TypeAbstractLayer String

type TypeView source = source -> Either String (TypeLayer source)

-- | The three declaration shapes relevant to formula compilation.  Value and
-- class declarations are premises rather than type definitions and therefore
-- never enter this table.
data FormulaDefinition source
    = FormulaAlias String [String] source
    | FormulaData String [String] [(String, [source])]
    | FormulaAbstract String [String] String

-- Expansion arguments carry the definition path at which they were supplied.
-- Returning a parameter as a complete type resumes at that origin, so finite
-- @Id (Id a)@ nesting does not look recursive. If a parameter is used in
-- function position, application-spine discovery deliberately retains the
-- current path: @A a = B a; B f = f f@ must reject the query @A A@ rather than
-- resetting past the active @A@ expansion.
data ExpansionType
    = ExpansionApp ExpansionType ExpansionType
    | ExpansionVar String
    | ExpansionCon String ExpansionOrigin
    | ExpansionTuple [ExpansionType]
    | ExpansionArrow ExpansionType ExpansionType
    | ExpansionForall ExpansionOrigin (SharedTypeAtom.TypeAtom String)
    | ExpansionUnion [(String, [ExpansionType])]
    | ExpansionAbstract String
    | ExpansionArgument ExpansionPath ExpansionType

-- A constructor occurrence keeps its identity when substitution duplicates
-- it. Query nodes are identified by their tree path. Nodes written in a
-- definition body are templates until that definition is expanded, when they
-- are placed under the concrete head occurrence that created this instance.
data ExpansionOrigin
    = QueryOrigin [Natural]
    | DefinitionTemplateOrigin [Natural]
    | DefinitionOrigin ExpansionOrigin String [Natural]
    -- A quantified body is converted lazily only if its forall is opened.
    -- Retaining the parent occurrence makes skolems from two equal-looking
    -- forall nodes distinct, including after definition instantiation.
    | OpenedForallOrigin ExpansionOrigin [Natural]
    deriving (Eq, Ord, Show)

data ExpansionFrame = ExpansionFrame String ExpansionOrigin

data ExpansionPath =
    ExpansionPath
        [ExpansionFrame]
        (Set.Set ExpansionOrigin)
        (Set.Set String)
        -- ^ recursive component representatives already opened on this
        -- logical lowering path

emptyExpansionPath :: ExpansionPath
emptyExpansionPath = ExpansionPath [] Set.empty Set.empty

pushExpansion
    :: String -> ExpansionOrigin -> ExpansionPath -> ExpansionPath
pushExpansion name origin (ExpansionPath frames active recursiveComponents) =
    ExpansionPath
        (ExpansionFrame name origin : frames)
        (Set.insert origin active)
        recursiveComponents

rememberRecursiveComponent
    :: PreparedFormulaCompiler
    -> String
    -> ExpansionPath
    -> ExpansionPath
rememberRecursiveComponent definitions name
        (ExpansionPath frames active recursiveComponents) =
    ExpansionPath frames active $ Set.insert component recursiveComponents
  where
    -- The preparation invariant indexes every admitted recursive head.  The
    -- head itself is a collision-free singleton fallback for defensive
    -- package-internal callers should that invariant ever change.
    component = case formulaDefinitionRecursiveComponent name definitions of
        Just preparedComponent -> preparedComponent
        Nothing -> name

resumeExpansionArgument
    :: ExpansionPath -> ExpansionPath -> ExpansionPath
resumeExpansionArgument
        (ExpansionPath _ _ currentComponents)
        (ExpansionPath originFrames originActive originComponents) =
    ExpansionPath originFrames originActive $
        Set.union currentComponents originComponents

-- | The complete first-binding table after source representations have been
-- discarded.  A checked environment stores one value and translates both raw
-- and native query types through it.
data PreparedFormulaCompiler = PreparedFormulaCompiler
    (Map.Map String ([String], ExpansionType))
    (Set.Set String)
    (Set.Set String)
    (Map.Map String String)
    -- ^ recursive datatype head -> source-order representative of its
    -- alias-normalized definition-graph SCC

-- | Variance at the current formula node. A function parameter reverses it;
-- products, sums, and definition expansion preserve it.
data FormulaPolarity
    = PositiveFormula
    | NegativeFormula
    deriving (Eq, Show)

-- | A polarized formula plus an honesty bit. 'translationIncomplete' means
-- that at least one quantified subtree remained opaque or a recursive
-- datatype was unfolded/atomized only to the configured positive bound.
-- Proofs are still sound, but an empty proof search is not a refutation of the
-- Haskell type.
-- 'translationIntroducedSkolems' lists the fresh rigid variables allocated by
-- opened positive foralls; instantiation policy treats them as ordinary
-- sequent variables without reparsing rendered atom spellings.
data FormulaTranslation = FormulaTranslation
    { translatedFormula :: Formula
    , translationIncomplete :: Bool
    , translationOpenableForalls :: [ForallSite]
    , translationIntroducedSkolems :: [String]
    }
    deriving (Eq, Show)

-- | One nonempty, deliberately bounded family of coherent translations.  The
-- primary plan opens every supported positive forall.  The historical exact
-- plan opens none; the first two local frontiers differ from either extreme at
-- one independently reachable occurrence.  Quadratic and cubic tail frontiers
-- make the corresponding choice at unordered pairs and triples of sites.
-- Keeping the categories explicit lets goal search and prepared-premise
-- caching preserve their established order without reconstructing it from an
-- unlabelled list.
data PolarizedFormulaPlans = PolarizedFormulaPlans
    { primaryFormulaPlan :: FormulaTranslation
    , exactOpaqueFormulaPlan :: Formula
    , singleOpaqueFormulaPlans :: [FormulaTranslation]
    , singleOpenFormulaPlans :: [FormulaTranslation]
    , pairOpaqueFormulaPlans :: [FormulaTranslation]
    , pairOpenFormulaPlans :: [FormulaTranslation]
    , tripleOpaqueFormulaPlans :: [FormulaTranslation]
    , tripleOpenFormulaPlans :: [FormulaTranslation]
    }
    deriving (Eq, Show)

-- | Every distinct skolem spelling introduced by any plan of this family, in
-- first-plan order. Frontier plans reuse the primary expansion and namespace,
-- so central deduplication prevents the polynomial tail from multiplying the
-- instantiation candidate inventory.
polarizedFormulaPlanSkolems :: PolarizedFormulaPlans -> [String]
polarizedFormulaPlanSkolems plans = SharedCollection.distinctOn id $
    translationIntroducedSkolems (primaryFormulaPlan plans) ++
    concatMap translationIntroducedSkolems
        (singleOpaqueFormulaPlans plans ++ singleOpenFormulaPlans plans ++
            pairOpaqueFormulaPlans plans ++ pairOpenFormulaPlans plans ++
            tripleOpaqueFormulaPlans plans ++ tripleOpenFormulaPlans plans)

-- A definition origin plus the reverse source path is stable across alias
-- expansion, duplicated arguments, datatype fields, and reopened forall
-- bodies. It therefore identifies exactly one positive introduction choice
-- without depending on source binder spelling.
data ForallSite = ForallSite ExpansionOrigin [Natural]
    deriving (Eq, Ord, Show)

-- Reverse occurrence paths extend at the front, so an enclosing site's path
-- is a suffix of every reachable descendant.  Origin ancestry supplies the
-- corresponding definition/opened-forall provenance; the path check keeps two
-- copies of one substituted source occurrence distinct.
forallSiteLeadsTo :: ForallSite -> ForallSite -> Bool
forallSiteLeadsTo
        (ForallSite ancestorOrigin ancestorPath)
        (ForallSite descendantOrigin descendantPath) =
    ancestorPath `isSuffixOf` descendantPath &&
        originDescendsFrom ancestorOrigin descendantOrigin

originDescendsFrom :: ExpansionOrigin -> ExpansionOrigin -> Bool
originDescendsFrom ancestor descendant
    | ancestor == descendant = True
    | otherwise = case descendant of
        DefinitionOrigin parent _ _ -> originDescendsFrom ancestor parent
        OpenedForallOrigin parent _ -> originDescendsFrom ancestor parent
        QueryOrigin{} -> False
        DefinitionTemplateOrigin{} -> False

data PreparedDefinition = PreparedDefinition
    String
    [String]
    ExpansionType
    Bool

-- | Validate a definition table once under the historical policy which rejects
-- every recursive expansion component.  Checked environments which have
-- already classified recursive datatypes use
-- 'prepareFormulaCompilerWithRecursiveData' instead.
prepareFormulaCompiler
    :: TypeView source
    -> [FormulaDefinition source]
    -> Either String PreparedFormulaCompiler
prepareFormulaCompiler = prepareFormulaCompilerWithRecursiveData Set.empty

-- | Validate a definition table while admitting an already checked set of
-- recursive datatype heads. Synonym cycles and every cycle containing an
-- unmarked definition remain errors. Marked datatypes stay in the definition
-- cache so polarized query compilation can expose one positive constructor
-- layer; conservative and negative translations retain their complete
-- applications as opaque atoms.
--
-- Names which do not select a first-binding 'FormulaData' declaration are
-- ignored. The caller therefore cannot weaken synonym validation by
-- accidentally or maliciously marking an alias as recursive data.
prepareFormulaCompilerWithRecursiveData
    :: Set.Set String
    -> TypeView source
    -> [FormulaDefinition source]
    -> Either String PreparedFormulaCompiler
prepareFormulaCompilerWithRecursiveData requestedRecursiveData view supplied = do
    definitions <- mapM (prepareDefinition view) selected
    let preliminary = PreparedFormulaCompiler
            (Map.fromList
                [(name, (parameters, body)) |
                    PreparedDefinition name parameters body _ <- definitions])
            (Set.fromList
                [name |
                    PreparedDefinition name _ _ True <- definitions])
            recursiveData
            Map.empty
    references <- validateDefinitionExpansion
        recursiveData definitions preliminary
    return $ PreparedFormulaCompiler
        (Map.fromList
            [(name, (parameters, body)) |
                PreparedDefinition name parameters body _ <- definitions])
        (Set.fromList
            [name |
                PreparedDefinition name _ _ True <- definitions])
        recursiveData
        (recursiveDataComponents recursiveData definitions references)
  where
    selected = firstBindings supplied
    dataNames = Set.fromList
        [name | FormulaData name _ _ <- selected]
    recursiveData = Set.intersection requestedRecursiveData dataNames

prepareDefinition
    :: TypeView source
    -> FormulaDefinition source
    -> Either String PreparedDefinition
prepareDefinition view definition = case definition of
    FormulaAlias name parameters body ->
        PreparedDefinition name parameters
            <$> expansionTypeAt view DefinitionTemplateOrigin [] body
            <*> pure True
    FormulaData name parameters constructors -> do
        expandedConstructors <- zipWithM convertConstructor
            [0 ..] constructors
        return $ PreparedDefinition name parameters
            (ExpansionUnion expandedConstructors) False
      where
        convertConstructor constructorIndex (constructor, fields) = do
            expandedFields <- zipWithM
                (\fieldIndex -> expansionTypeAt view
                    DefinitionTemplateOrigin
                    [fieldIndex, constructorIndex])
                [0 ..]
                fields
            return (constructor, expandedFields)
    FormulaAbstract name parameters abstractName -> Right $
        PreparedDefinition name parameters
            (ExpansionAbstract abstractName) False

-- | Compile one source type with a previously checked definition table.
compileFormula
    :: TypeView source
    -> PreparedFormulaCompiler
    -> source
    -> Either String Formula
compileFormula view prepared source = do
    expanded <- expansionTypeAt view QueryOrigin [] source
    translatedFormula <$> lowerExpansionType
        OpaqueForalls prepared emptyExpansionPath [] expanded

-- | Compile the bounded rank-N fragment used by checked Djinn queries.
-- Foralls in positive position are opened with fresh rigid proposition names.
-- Their already validated class contexts are deliberately ignored for proof
-- power, consistently with Djinn's dictionary-independent treatment of the
-- query's prenex context. Unsupported occurrences stay alpha-stable opaque
-- atoms.
-- Besides the two historical extremes, retain both singleton frontiers: one
-- opaque occurrence among opened siblings, and one opened occurrence (plus
-- any enclosing forall chain needed to reach it) among opaque siblings.
-- Pairwise and triple tail frontiers make the same choices at unordered pairs
-- and triples. This is exhaustive for seven independent sites without
-- constructing a power set; its additional plan count is cubic rather than
-- exponential.
-- The numeric namespace must be distinct for independently compiled goals and
-- premises so their locally introduced skolems cannot accidentally meet.
compilePolarizedFormulaPlans
    :: Natural
    -> FormulaPolarity
    -> TypeView (SharedType.Type String)
    -> TypeView source
    -> PreparedFormulaCompiler
    -> source
    -> Either String PolarizedFormulaPlans
compilePolarizedFormulaPlans namespace polarity openedView view prepared
        source = do
    expanded <- expansionTypeAt view QueryOrigin [] source
    primary <- lowerExpansionType
        (PolarizedForalls namespace polarity openedView Set.empty)
        prepared emptyExpansionPath [] expanded
    -- Exact opacity applies to recursive data as well as quantified subtrees.
    -- This complementary view preserves forwarding such as @Rec a -> Rec a@
    -- when the primary positive view exposes one constructor layer.
    exact <- translatedFormula <$> lowerExpansionType
        OpaqueForalls prepared emptyExpansionPath [] expanded
    let sites = translationOpenableForalls primary
        allSites = Set.fromList sites
    singleOpaque <- mapM
        (compileSelection expanded . Set.singleton) sites
    singleOpen <- if atLeast 3 sites then mapM
            (compileSelection expanded . opaqueExceptReachable allSites) sites
        else Right []
    let sitePairs = unorderedPairs sites
    pairOpaque <- if atLeast 4 sites then mapM
            (compileSelection expanded . Set.fromList . pairMembers) sitePairs
        else Right []
    pairOpen <- if atLeast 5 sites then mapM
            (compileSelection expanded . opaqueExceptPair allSites) sitePairs
        else Right []
    let siteTriples = unorderedTriples sites
    tripleOpaque <- if atLeast 6 sites then mapM
            (compileSelection expanded . Set.fromList . tripleMembers)
            siteTriples
        else Right []
    tripleOpen <- if atLeast 7 sites then mapM
            (compileSelection expanded . opaqueExceptTriple allSites)
            siteTriples
        else Right []
    return PolarizedFormulaPlans
        { primaryFormulaPlan = primary
        , exactOpaqueFormulaPlan = exact
        , singleOpaqueFormulaPlans = singleOpaque
        , singleOpenFormulaPlans = singleOpen
        , pairOpaqueFormulaPlans = pairOpaque
        , pairOpenFormulaPlans = pairOpen
        , tripleOpaqueFormulaPlans = tripleOpaque
        , tripleOpenFormulaPlans = tripleOpen
        }
  where
    compileSelection expanded opaqueSites = lowerExpansionType
        (PolarizedForalls namespace polarity openedView opaqueSites)
        prepared emptyExpansionPath [] expanded

    -- Opening a nested target necessarily opens its enclosing forall chain.
    -- Every unrelated site remains opaque, so this is the dual of selecting
    -- one opaque site in the fully opened plan rather than an accidental exact
    -- plan whenever the chosen occurrence is nested.
    opaqueExceptReachable sites target = Set.filter
        (not . (`forallSiteLeadsTo` target)) sites

    -- Opening either nested target necessarily opens the union of both
    -- ancestor chains.  Unrelated occurrences remain opaque, making this the
    -- exact dual of selecting two opaque sites in the fully opened plan.
    opaqueExceptPair sites (firstSite, secondSite) = Set.filter
        (\site -> not
            (site `forallSiteLeadsTo` firstSite ||
                site `forallSiteLeadsTo` secondSite))
        sites

    pairMembers (firstSite, secondSite) = [firstSite, secondSite]

    -- The triple dual follows the same ancestry rule as the singleton and
    -- pair frontiers: reaching a nested target opens every enclosing forall.
    opaqueExceptTriple sites (firstSite, secondSite, thirdSite) = Set.filter
        (\site -> not
            (site `forallSiteLeadsTo` firstSite ||
                site `forallSiteLeadsTo` secondSite ||
                site `forallSiteLeadsTo` thirdSite))
        sites

    tripleMembers (firstSite, secondSite, thirdSite) =
        [firstSite, secondSite, thirdSite]

    unorderedPairs [] = []
    unorderedPairs (firstSite : remaining) =
        map ((,) firstSite) remaining ++ unorderedPairs remaining

    unorderedTriples [] = []
    unorderedTriples (firstSite : remaining) =
        [ (firstSite, secondSite, thirdSite)
        | (secondSite, thirdSite) <- unorderedPairs remaining
        ] ++ unorderedTriples remaining

    atLeast :: Int -> [element] -> Bool
    atLeast count = go count
      where
        go remaining _ | remaining <= 0 = True
        go _ [] = False
        go remaining (_ : rest) = go (remaining - 1) rest

data ForallLowering
    = OpaqueForalls
    | PolarizedForalls
        Natural
        FormulaPolarity
        (TypeView (SharedType.Type String))
        (Set.Set ForallSite)

lookupFormulaDefinition
    :: String
    -> PreparedFormulaCompiler
    -> Maybe ([String], ExpansionType)
lookupFormulaDefinition name (PreparedFormulaCompiler definitions _ _ _) =
    Map.lookup name definitions

formulaDefinitionIsAlias
    :: String -> PreparedFormulaCompiler -> Bool
formulaDefinitionIsAlias name (PreparedFormulaCompiler _ aliases _ _) =
    name `Set.member` aliases

formulaDefinitionIsRecursiveData
    :: String -> PreparedFormulaCompiler -> Bool
formulaDefinitionIsRecursiveData
        name (PreparedFormulaCompiler _ _ recursive _) =
    name `Set.member` recursive

formulaDefinitionRecursiveComponent
    :: String -> PreparedFormulaCompiler -> Maybe String
formulaDefinitionRecursiveComponent
        name (PreparedFormulaCompiler _ _ _ components) =
    Map.lookup name components

-- Reverse tree paths make child extension constant-time. Constructor and
-- field positions are both included, so two equal spellings in one finite
-- input still receive distinct identities.
expansionTypeAt
    :: TypeView source
    -> ([Natural] -> ExpansionOrigin)
    -> [Natural]
    -> source
    -> Either String ExpansionType
expansionTypeAt view origin = convert
  where
    convert path source = do
        layer <- view source
        case layer of
            TypeApplicationLayer function argument -> ExpansionApp
                <$> convert (0 : path) function
                <*> convert (1 : path) argument
            TypeVariableLayer variable -> Right $ ExpansionVar variable
            TypeConstructorLayer name ->
                Right $ ExpansionCon name $ origin path
            TypeTupleLayer types -> ExpansionTuple <$> zipWithM
                (\index -> convert (index : path)) [0 ..] types
            TypeArrowLayer argument result -> ExpansionArrow
                <$> convert (0 : path) argument
                <*> convert (1 : path) result
            TypeForallLayer atom -> Right $ ExpansionForall (origin path) atom
            TypeUnionLayer constructors -> ExpansionUnion <$> zipWithM
                (convertConstructor path) [0 ..] constructors
            TypeAbstractLayer name -> Right $ ExpansionAbstract name

    convertConstructor path constructorIndex (constructor, fields) = do
        expandedFields <- zipWithM
            (\fieldIndex ->
                convert (fieldIndex : constructorIndex : path))
            [0 ..]
            fields
        return (constructor, expandedFields)

-- Turn cached body-local origins into identities for this particular
-- expansion instance. Origins brought in later by parameter substitution are
-- already concrete and are deliberately left unchanged.
instantiateDefinitionOrigins
    :: ExpansionOrigin
    -> String
    -> ExpansionType
    -> ExpansionType
instantiateDefinitionOrigins parent owner = instantiate
  where
    instantiate source = case source of
        ExpansionApp function argument -> ExpansionApp
            (instantiate function) (instantiate argument)
        variable@(ExpansionVar _) -> variable
        ExpansionCon name origin ->
            ExpansionCon name $ instantiateOrigin origin
        ExpansionTuple types -> ExpansionTuple $ map instantiate types
        ExpansionArrow argument result ->
            ExpansionArrow (instantiate argument) (instantiate result)
        ExpansionForall origin atom ->
            ExpansionForall (instantiateOrigin origin) atom
        ExpansionUnion constructors -> ExpansionUnion
            [(constructor, map instantiate fields) |
                (constructor, fields) <- constructors]
        abstract@(ExpansionAbstract _) -> abstract
        ExpansionArgument path argument ->
            ExpansionArgument path $ instantiate argument

    instantiateOrigin origin = case origin of
        DefinitionTemplateOrigin sourcePath ->
            DefinitionOrigin parent owner sourcePath
        _ -> origin

expansionApp :: ExpansionType -> ExpansionType -> ExpansionType
expansionApp function argument = case partialExpansionArrow function of
    Just arrowArgument -> ExpansionArrow arrowArgument argument
    Nothing -> ExpansionApp function argument

-- Match the legacy smart constructor even when substitution has put an
-- argument-origin marker around @(->)@ or an already partial arrow.
partialExpansionArrow :: ExpansionType -> Maybe ExpansionType
partialExpansionArrow source = case source of
    ExpansionApp headType argument
        | expansionArrowHead headType -> Just argument
    ExpansionArgument origin function ->
        ExpansionArgument origin `fmap` partialExpansionArrow function
    _ -> Nothing

expansionArrowHead :: ExpansionType -> Bool
expansionArrowHead source = case source of
    ExpansionCon "->" _ -> True
    ExpansionArgument _ headType -> expansionArrowHead headType
    _ -> False

lowerExpansionType
    :: ForallLowering
    -> PreparedFormulaCompiler
    -> ExpansionPath
    -> [Natural]
    -> ExpansionType
    -> Either String FormulaTranslation
lowerExpansionType lowering definitions path occurrencePath source = case source of
    ExpansionArgument origin argument ->
        lowerExpansionType lowering definitions
            (resumeExpansionArgument path origin) occurrencePath argument
    ExpansionTuple types -> do
        translated <- zipWithM
            (\index -> lowerChild index) [0 ..] types
        return $ combineTranslations Conj translated
    ExpansionArrow argument result -> do
        translatedArgument <- lowerExpansionType
            (reverseFormulaPolarity lowering) definitions path
            (0 : occurrencePath) argument
        translatedResult <- lowerChild 1 result
        return $ combineBinary (:->) translatedArgument translatedResult
    ExpansionForall origin atom ->
        lowerForall lowering definitions path occurrencePath origin atom
    ExpansionUnion [] -> Right $ completeTranslation false
    ExpansionUnion constructors -> do
        translated <- zipWithM lowerConstructor [0 ..] constructors
        return $ FormulaTranslation
            (Disj $ map fst translated)
            (any (translationIncomplete . snd) translated)
            (concatMap (translationOpenableForalls . snd) translated)
            (concatMap (translationIntroducedSkolems . snd) translated)
    _ -> lowerApplication lowering definitions path occurrencePath source
  where
    lowerChild index = lowerExpansionType lowering definitions path
        (index : occurrencePath)

    lowerConstructor constructorIndex (constructor, fields) = do
        translation <- lowerExpansionType lowering definitions path
            (constructorIndex : occurrencePath) $ ExpansionTuple fields
        return
            ( (ConsDesc constructor (length fields),
                translatedFormula translation)
            , translation
            )

lowerForall
    :: ForallLowering
    -> PreparedFormulaCompiler
    -> ExpansionPath
    -> [Natural]
    -> ExpansionOrigin
    -> SharedTypeAtom.TypeAtom String
    -> Either String FormulaTranslation
lowerForall lowering definitions path occurrencePath origin atom = case lowering of
    OpaqueForalls -> Right opaque
    PolarizedForalls namespace PositiveFormula openedView opaqueSites ->
        case SharedTypeAtom.typeAtomType atom of
            -- Context validation belongs to the checked request/session edge.
            -- LJT receives only the body: accepting a contextual positive
            -- forall therefore permits dictionary-independent introduction
            -- without pretending that class methods are proof premises.
            SharedType.ForallType binders _ body
                | site `Set.member` opaqueSites -> Right incompleteOpaque
                | otherwise -> do
                    (skolems, opened) <- openForallBody
                        namespace occurrencePath origin binders body
                    expanded <- expansionTypeAt openedView
                        (OpenedForallOrigin origin) [] opened
                    translation <- lowerExpansionType lowering definitions path
                        occurrencePath expanded
                    return translation
                        { translationOpenableForalls = site
                            : translationOpenableForalls translation
                        , translationIntroducedSkolems = skolems
                            ++ translationIntroducedSkolems translation
                        }
            _ -> Right incompleteOpaque
    PolarizedForalls _ NegativeFormula _ _ -> Right incompleteOpaque
  where
    site = ForallSite origin occurrencePath
    opaque = completeTranslation $ PVar $ opaqueTypeSymbol
        $ SharedTypeAtom.typeAtomType atom
    incompleteOpaque = opaque {translationIncomplete = True}

-- Opening happens after removing the forall wrapper, so its binders are free
-- in @body@ and ordinary capture-avoiding substitution can replace them. The
-- private '$' namespace cannot be produced by Djinn's checked source parser;
-- the reservation walk also protects programmatically constructed inputs.
-- The allocated skolem spellings are returned so instantiation policy can
-- treat them as sequent variables without reparsing rendered atoms.
openForallBody
    :: Natural
    -> [Natural]
    -> ExpansionOrigin
    -> [String]
    -> SharedType.Type String
    -> Either String ([String], SharedType.Type String)
openForallBody namespace occurrencePath origin binders body = do
    opened <- first show $ SharedType.substituteTypeVariables
        allocateShadow Set.empty replacements body
    return (map snd assignments, opened)
  where
    source = SharedType.ForallType binders [] body
    (_, assignments) = foldl allocateSkolem
        (foldMap Set.singleton source, [])
        $ zip [0 :: Natural ..] binders
    replacements = Map.fromList
        [ (binder, SharedType.TypeVariable fresh)
        | (binder, fresh) <- assignments
        ]

    allocateSkolem (reserved, chosen) (index, binder) =
        let fresh = chooseFresh reserved $ skolemBase index
        in ( Set.insert fresh reserved
           , chosen ++ [(binder, fresh)]
           )

    skolemBase index = "$djinn$skolem$" ++ show namespace ++ "$" ++
        show occurrencePath ++ "$" ++ show origin ++ "$" ++ show index

    allocateShadow reserved binder = Just $ chooseFresh reserved $
        "$djinn$shadow$" ++ binder

    chooseFresh reserved candidate
        | candidate `Set.member` reserved =
            chooseFresh reserved $ candidate ++ "'"
        | otherwise = candidate

completeTranslation :: Formula -> FormulaTranslation
completeTranslation formula = FormulaTranslation formula False [] []

combineTranslations
    :: ([Formula] -> Formula)
    -> [FormulaTranslation]
    -> FormulaTranslation
combineTranslations constructor translations = FormulaTranslation
    (constructor $ map translatedFormula translations)
    (any translationIncomplete translations)
    (concatMap translationOpenableForalls translations)
    (concatMap translationIntroducedSkolems translations)

combineBinary
    :: (Formula -> Formula -> Formula)
    -> FormulaTranslation
    -> FormulaTranslation
    -> FormulaTranslation
combineBinary constructor left right = FormulaTranslation
    (constructor (translatedFormula left) $ translatedFormula right)
    (translationIncomplete left || translationIncomplete right)
    (translationOpenableForalls left ++ translationOpenableForalls right)
    (translationIntroducedSkolems left ++ translationIntroducedSkolems right)

reverseFormulaPolarity :: ForallLowering -> ForallLowering
reverseFormulaPolarity lowering = case lowering of
    OpaqueForalls -> OpaqueForalls
    PolarizedForalls namespace polarity openedView selected ->
        PolarizedForalls namespace reversed openedView selected
      where
        reversed = case polarity of
            PositiveFormula -> NegativeFormula
            NegativeFormula -> PositiveFormula

lowerApplication
    :: ForallLowering
    -> PreparedFormulaCompiler
    -> ExpansionPath
    -> [Natural]
    -> ExpansionType
    -> Either String FormulaTranslation
lowerApplication lowering definitions path occurrencePath source =
    case expansionApplication source [] of
        (ExpansionCon name origin, arguments) ->
            case lookupFormulaDefinition name definitions of
                Just (parameters, body)
                    | length parameters == length arguments ->
                        if formulaDefinitionIsRecursiveData name definitions
                            then lowerRecursiveData
                                name origin parameters body arguments
                            else expandAndLower False
                                name origin parameters body arguments
                _ -> atom False
        _ -> atom False
  where
    -- Recursive data is deliberately asymmetric. Each recursive component on
    -- a positive expansion path may expose one constructor layer; a field that
    -- would reopen that component, every negative occurrence, and the
    -- exact/legacy view retain the complete application as one atom.
    -- Every such decision is incomplete: the bounded positive view omitted a
    -- deeper layer, while the atomized view omitted constructor structure.
    lowerRecursiveData name origin parameters body arguments
        | recursiveDataCanUnfold lowering definitions name path =
            expandAndLower True name origin parameters body arguments
        | otherwise = atom True

    expandAndLower forceIncomplete name origin parameters body arguments = do
        expanded <- expandDefinitionStep definitions path
            name origin parameters body arguments
        let descentPath = pushExpansion name origin $
                if forceIncomplete
                    then rememberRecursiveComponent definitions name path
                    else path
        translation <- case expanded of
            ExpansionUnion [] -> do
                normalized <- normalizeExpansionAliases
                    definitions path source
                completeTranslation . Empty <$> expansionSymbol normalized
            _ -> lowerExpansionType lowering definitions
                descentPath
                occurrencePath expanded
        return $ if forceIncomplete
            then translation {translationIncomplete = True}
            else translation

    atom forceIncomplete = do
        normalized <- normalizeExpansionAliases definitions path source
        formula <- PVar <$> expansionSymbol normalized
        return $ FormulaTranslation formula
            (forceIncomplete || polarizedOpaqueForall lowering normalized) [] []

recursiveDataCanUnfold
    :: ForallLowering
    -> PreparedFormulaCompiler
    -> String
    -> ExpansionPath
    -> Bool
recursiveDataCanUnfold lowering definitions name path = case lowering of
    PolarizedForalls _ PositiveFormula _ _ ->
        recursiveComponentLayerAvailable path &&
            not (expansionPathContainsRecursiveComponent definitions name path)
    OpaqueForalls -> False
    PolarizedForalls _ NegativeFormula _ _ -> False

-- Two independent recursive components recover the common finite wrapper
-- shape (@Outer (Inner a)@) without allowing a duplicated component chain to
-- grow the proof formula exponentially before search budgets can apply.
maximumRecursiveComponentLayers :: Int
maximumRecursiveComponentLayers = 2

recursiveComponentLayerAvailable :: ExpansionPath -> Bool
recursiveComponentLayerAvailable (ExpansionPath _ _ openedComponents) =
    Set.size openedComponents < maximumRecursiveComponentLayers

-- Within the total layer bound, each recursive SCC may expose one positive
-- constructor layer along a path.  A field in an independent recursive
-- component can therefore contribute its own finite layer, while direct,
-- alias-hidden, and mutual recursion stop as soon as the path would reopen the
-- same component.
expansionPathContainsRecursiveComponent
    :: PreparedFormulaCompiler -> String -> ExpansionPath -> Bool
expansionPathContainsRecursiveComponent definitions name
        (ExpansionPath _ _ openedComponents) = case
            formulaDefinitionRecursiveComponent name definitions of
    Just component -> component `Set.member` openedComponents
    -- Every admitted recursive datatype is indexed during preparation.  Keep
    -- the old global bound as a conservative fallback if that invariant ever
    -- changes internally.
    Nothing -> not $ Set.null openedComponents

polarizedOpaqueForall :: ForallLowering -> ExpansionType -> Bool
polarizedOpaqueForall lowering source = case lowering of
    OpaqueForalls -> False
    PolarizedForalls{} -> expansionContainsForall source

-- Ordinary historical atoms retain their exact renderer identity. As soon as
-- an opaque application contains a quantified child, however, its proposition
-- key is the shared alpha-normal structure of the whole application. This is
-- what makes impredicative wrappers compare correctly without flattening or
-- otherwise changing their existing logical treatment.
expansionSymbol :: ExpansionType -> Either String Symbol
expansionSymbol source
    | expansionContainsForall source = opaqueTypeSymbol
        <$> expansionSourceType source
    | otherwise = Right $ Symbol $ renderExpansionType source

expansionContainsForall :: ExpansionType -> Bool
expansionContainsForall source = case source of
    ExpansionApp function argument ->
        expansionContainsForall function || expansionContainsForall argument
    ExpansionVar{} -> False
    ExpansionCon{} -> False
    ExpansionTuple types -> any expansionContainsForall types
    ExpansionArrow argument result ->
        expansionContainsForall argument || expansionContainsForall result
    ExpansionForall{} -> True
    ExpansionUnion constructors -> any
        (any expansionContainsForall . snd) constructors
    ExpansionAbstract{} -> False
    ExpansionArgument _ argument -> expansionContainsForall argument

-- Recover the shared source structure of a normalized, non-logical expansion
-- subtree. Unions are formula definitions rather than Haskell source types and
-- therefore cannot become opaque atoms. Constructor origins are operational
-- expansion metadata and deliberately disappear from logical identity.
expansionSourceType
    :: ExpansionType
    -> Either String (SharedType.Type String)
expansionSourceType source = SharedType.canonicalizeType <$> case source of
    ExpansionApp function argument -> SharedType.TypeApplication
        <$> expansionSourceType function <*> expansionSourceType argument
    ExpansionVar variable -> Right $ SharedType.TypeVariable variable
    ExpansionCon name _ -> SharedType.TypeConstructor
        <$> first show (SharedName.parseName name)
    ExpansionTuple types -> SharedType.TupleType SharedName.Boxed
        <$> mapM expansionSourceType types
    ExpansionArrow argument result -> SharedType.FunctionType
        <$> expansionSourceType argument <*> expansionSourceType result
    ExpansionForall _ atom -> Right $ SharedTypeAtom.typeAtomType atom
    ExpansionUnion{} -> Left
        "datatype expansion reached opaque type identity"
    ExpansionAbstract name -> SharedType.TypeConstructor
        <$> first show (SharedName.parseName name)
    ExpansionArgument _ argument -> expansionSourceType argument

-- A marker in function position is unwrapped without resetting the current
-- definition path; a marker around the complete expression is handled by
-- 'lowerExpansionType' above and does reset it.
expansionApplication
    :: ExpansionType
    -> [ExpansionType]
    -> (ExpansionType, [ExpansionType])
expansionApplication (ExpansionApp function argument) arguments =
    expansionApplication function (argument : arguments)
expansionApplication (ExpansionArgument _ function) arguments =
    expansionApplication function arguments
expansionApplication headType arguments = (headType, arguments)

substituteExpansion
    :: LazyMap.Map String ExpansionType
    -> ExpansionType
    -> Either String ExpansionType
substituteExpansion replacements source = case source of
    ExpansionApp function argument -> expansionApp
        <$> substituteExpansion replacements function
        <*> substituteExpansion replacements argument
    variable@(ExpansionVar name) ->
        Right $ LazyMap.findWithDefault variable name replacements
    constructor@(ExpansionCon _ _) -> Right constructor
    ExpansionTuple types ->
        ExpansionTuple <$> mapM (substituteExpansion replacements) types
    ExpansionArrow argument result -> ExpansionArrow
        <$> substituteExpansion replacements argument
        <*> substituteExpansion replacements result
    ExpansionForall origin atom -> ExpansionForall origin <$>
        substituteExpansionAtom replacements atom
    ExpansionUnion constructors -> ExpansionUnion <$>
        mapM substituteConstructor constructors
      where
        substituteConstructor (constructor, fields) = do
            substitutedFields <- mapM
                (substituteExpansion replacements) fields
            pure (constructor, substitutedFields)
    abstract@(ExpansionAbstract _) -> Right abstract
    argument@(ExpansionArgument _ _) -> Right argument

-- Definition parameters remain ordinary free variables when they occur under
-- a nested forall. Substitute them through the shared capture-avoiding worker,
-- then rebuild the atom and its cached alpha key. Substitution itself does not
-- inspect the quantified body logically; polarized lowering may later reopen
-- a validated occurrence in positive position, ignoring its context for proof
-- power under Djinn's dictionary-independent policy.
substituteExpansionAtom
    :: LazyMap.Map String ExpansionType
    -> SharedTypeAtom.TypeAtom String
    -> Either String (SharedTypeAtom.TypeAtom String)
substituteExpansionAtom replacements atom = do
    sharedReplacements <- Map.fromList `fmap` mapM convertReplacement
        [ (variable, replacement)
        | (variable, replacement) <- LazyMap.toList replacements
        , variable `Set.member` SharedTypeAtom.typeAtomFreeVariables atom
        ]
    first show $ SharedTypeAtom.substituteTypeAtomVariables
        freshAtomVariable Set.empty sharedReplacements atom
  where
    convertReplacement (variable, replacement) =
        fmap (\converted -> (variable, converted))
            $ expansionSourceType replacement

    freshAtomVariable reserved variable = Just $ choose (variable ++ "'")
      where
        choose candidate
            | candidate `Set.member` reserved = choose $ candidate ++ "'"
            | otherwise = candidate

-- Raw low-level definitions may repeat a parameter. Association-list lookup
-- historically selected its first binding; folding inserts from the right.
firstExpansionSubstitutions
    :: [(String, ExpansionType)]
    -> LazyMap.Map String ExpansionType
firstExpansionSubstitutions =
    foldr (uncurry LazyMap.insert) LazyMap.empty

-- One checked definition-expansion step: reject an active expansion cycle,
-- then instantiate the definition body with argument markers anchored at the
-- current path. Logical lowering and the alias fold both continue with the
-- result at @pushExpansion name origin path@, so the two consumers cannot
-- drift in substitution or provenance semantics.
expandDefinitionStep
    :: PreparedFormulaCompiler
    -> ExpansionPath
    -> String
    -> ExpansionOrigin
    -> [String]
    -> ExpansionType
    -> [ExpansionType]
    -> Either String ExpansionType
expandDefinitionStep definitions path name origin parameters body arguments = do
    rejectActiveExpansion definitions path name origin
    let replacements = firstExpansionSubstitutions $
            zip parameters $ map (ExpansionArgument path) arguments
    substituteExpansion replacements $
        instantiateDefinitionOrigins origin name body

rejectActiveExpansion
    :: PreparedFormulaCompiler
    -> ExpansionPath
    -> String
    -> ExpansionOrigin
    -> Either String ()
rejectActiveExpansion
        (PreparedFormulaCompiler _ aliasNames _ _)
        (ExpansionPath frames active _)
        name
        origin
    | origin `Set.notMember` active = Right ()
    | otherwise = Left $
        "recursive type " ++ expansionKind ++ " expansion: " ++
            intercalate ", " cycleMembers
  where
    newerFrames = takeWhile ((/= origin) . frameOrigin) frames
    cycleMembers = name : reverse (map frameName newerFrames) ++ [name]
    expansionKind
        | all (`Set.member` aliasNames) cycleMembers = "synonym"
        | otherwise = "definition"

    frameName (ExpansionFrame frameName' _) = frameName'
    frameOrigin (ExpansionFrame _ frameOrigin') = frameOrigin'

-- Alias expansion has two consumers: atom rendering and a reference summary
-- for whole-table validation. Keeping traversal, lazy substitution,
-- occurrence provenance, and error order in one fold prevents them drifting.
data ExpansionAlgebra result = ExpansionAlgebra
    { algebraVariable :: String -> result
    , algebraConstructorApplication :: String -> [result] -> result
    , algebraApplication :: result -> [result] -> result
    , algebraTuple :: [result] -> result
    , algebraArrow :: result -> result -> result
    , algebraForall ::
        ExpansionOrigin -> SharedTypeAtom.TypeAtom String -> result
    , algebraUnion :: [(String, [result])] -> result
    , algebraAbstract :: String -> result
    }

foldExpansionAliases
    :: PreparedFormulaCompiler
    -> ExpansionAlgebra result
    -> ExpansionPath
    -> ExpansionType
    -> Either String result
foldExpansionAliases definitions algebra path source = case source of
    ExpansionArgument origin argument ->
        foldExpansionAliases definitions algebra
            (resumeExpansionArgument path origin) argument
    ExpansionApp _ _ -> foldApplication
    ExpansionCon _ _ -> foldApplication
    ExpansionVar variable -> Right $ algebraVariable algebra variable
    ExpansionTuple types ->
        algebraTuple algebra `fmap` mapM foldCurrent types
    ExpansionArrow argument result -> do
        foldedArgument <- foldCurrent argument
        foldedResult <- foldCurrent result
        return $ algebraArrow algebra foldedArgument foldedResult
    ExpansionForall origin atom -> Right $
        algebraForall algebra origin atom
    ExpansionUnion constructors ->
        algebraUnion algebra `fmap` mapM foldConstructor constructors
    ExpansionAbstract name -> Right $ algebraAbstract algebra name
  where
    foldCurrent = foldExpansionAliases definitions algebra path

    foldApplication = case expansionApplication source [] of
        (ExpansionCon name origin, arguments) ->
            case lookupFormulaDefinition name definitions of
                Just (parameters, body)
                    | length parameters == length arguments &&
                      formulaDefinitionIsAlias name definitions -> do
                        expanded <- expandDefinitionStep definitions path
                            name origin parameters body arguments
                        foldExpansionAliases definitions algebra
                            (pushExpansion name origin path) expanded
                _ -> do
                    foldedArguments <- mapM foldCurrent arguments
                    return $ algebraConstructorApplication algebra
                        name foldedArguments
        (headType, arguments) -> do
            foldedHead <- foldCurrent headType
            foldedArguments <- mapM foldCurrent arguments
            return $ algebraApplication algebra foldedHead foldedArguments

    foldConstructor (constructor, fields) = do
        foldedFields <- mapM foldCurrent fields
        return (constructor, foldedFields)

-- Alias-normalize an atom without unfolding its outer datatype/abstract
-- constructor. Argument markers preserve the same path semantics as logical
-- lowering. Rebuilding the existing expansion tree keeps rendering private to
-- this module without allocating either frontend's recursive type value.
normalizeExpansionAliases
    :: PreparedFormulaCompiler
    -> ExpansionPath
    -> ExpansionType
    -> Either String ExpansionType
normalizeExpansionAliases definitions =
    foldExpansionAliases definitions normalizedExpansionAlgebra

normalizedExpansionAlgebra :: ExpansionAlgebra ExpansionType
normalizedExpansionAlgebra = ExpansionAlgebra
    { algebraVariable = ExpansionVar
    , algebraConstructorApplication = \name arguments ->
        foldl' expansionApp
            (ExpansionCon name $ QueryOrigin []) arguments
    , algebraApplication = foldl' expansionApp
    , algebraTuple = ExpansionTuple
    , algebraArrow = ExpansionArrow
    , algebraForall = ExpansionForall
    , algebraUnion = ExpansionUnion
    , algebraAbstract = ExpansionAbstract
    }

-- Render the normalized atom with Djinn's historical HType precedence and
-- spellings. This deliberately handles raw-only malformed shapes (notably a
-- unary tuple or nested declaration body) even though checked shared types
-- cannot construct them.
renderExpansionType :: ExpansionType -> String
renderExpansionType source = showsExpansionType 0 source ""

showsExpansionType :: Int -> ExpansionType -> ShowS
showsExpansionType precedence source = case source of
    ExpansionArgument _ argument ->
        showsExpansionType precedence argument
    ExpansionApp (ExpansionCon "[]" _) element ->
        showString "[" . showsExpansionType 0 element . showString "]"
    ExpansionApp function argument -> showParen (precedence > 2) $
        showsExpansionType 2 function . showString " " .
            showsExpansionType 3 argument
    ExpansionVar variable -> showString variable
    ExpansionCon "()" _ -> showString "()"
    ExpansionCon name _
        | not (isQualifiedConId name) ->
            showParen True $ showString name
        | otherwise -> showString name
    ExpansionTuple types -> showParen True $ renderTuple types
    ExpansionArrow argument result -> showParen (precedence > 0) $
        showsExpansionType 1 argument . showString " -> " .
            showsExpansionType 0 result
    ExpansionForall _ atom -> showParen (precedence > 0) $ showString
        $ SharedTypeRender.renderType id $ SharedTypeAtom.typeAtomType atom
    ExpansionUnion constructors -> renderConstructors constructors
    ExpansionAbstract name -> showString name
  where
    renderTuple [] = id
    renderTuple [element] = showsExpansionType 0 element
    renderTuple (element : elements) =
        showsExpansionType 0 element . showString ", " .
            renderTuple elements

    renderConstructors [] = id
    renderConstructors [constructor] = renderConstructor constructor
    renderConstructors (constructor : constructors) =
        renderConstructor constructor . showString " | " .
            renderConstructors constructors

    renderConstructor (constructor, fields) = foldl
        (\render field ->
            render . showString " " . showsExpansionType 10 field)
        (showString constructor)
        fields

-- Collect the definition graph after alias normalization without allocating a
-- normalized frontend type. Every child is visited before the algebra is
-- invoked, retaining higher-order expansion failures and their order.
summarizeExpansionReferences
    :: Set.Set String
    -> PreparedFormulaCompiler
    -> ExpansionPath
    -> ExpansionType
    -> Either String (Set.Set String)
summarizeExpansionReferences interesting definitions =
    foldExpansionAliases definitions $ referenceExpansionAlgebra interesting

referenceExpansionAlgebra
    :: Set.Set String
    -> ExpansionAlgebra (Set.Set String)
referenceExpansionAlgebra interesting = ExpansionAlgebra
    { algebraVariable = const Set.empty
    , algebraConstructorApplication = summarizeConstructor
    , algebraApplication = \headReferences argumentReferences ->
        Set.unions $ headReferences : argumentReferences
    , algebraTuple = Set.unions
    , algebraArrow = Set.union
    , algebraForall = \_ -> Set.intersection interesting .
        Set.map SharedName.renderCanonical .
        SharedType.typeConstructors . SharedTypeAtom.typeAtomType
    , algebraUnion = Set.unions . concatMap snd
    , algebraAbstract = const Set.empty
    }
  where
    -- Saturated prefix arrows normalize structurally and therefore contribute
    -- no nominal @->@ reference, matching HType's historical smart constructor.
    summarizeConstructor name arguments
        | name == "->" && length arguments >= 2 = references
        | name `Set.member` interesting = Set.insert name references
        | otherwise = references
      where
        references = Set.unions arguments

-- Validate the actual definition-expansion graph, not merely repeated runtime
-- types. The latter misses growing cycles such as @A a = A [a]@, while an
-- active-name guard falsely rejects finite @Id (Id a)@. Synonym SCCs are found
-- before normalization; datatype SCCs are classified from alias-free fields.
validateDefinitionExpansion
    :: Set.Set String
    -> [PreparedDefinition]
    -> PreparedFormulaCompiler
    -> Either String (Map.Map String (Set.Set String))
validateDefinitionExpansion recursiveData definitions prepared = do
    rejectFirstCycle True Set.empty aliases aliasReferences
    summaries <- Map.fromList `fmap` mapM summarizeDefinition definitions
    let summarizedReferences definition = Map.findWithDefault Set.empty
            (preparedDefinitionName definition) summaries
    rejectFirstCycle False recursiveData definitions summarizedReferences
    return summaries
  where
    aliases = filter preparedDefinitionIsAlias definitions
    aliasNames = Set.fromList $ map preparedDefinitionName aliases
    definitionNames = Set.fromList $ map preparedDefinitionName definitions
    aliasReferences = Set.intersection aliasNames .
        definitionReferences . preparedDefinitionBody

    summarizeDefinition definition = do
        let name = preparedDefinitionName definition
        summary <- first (("type definition " ++ name ++ ": ") ++) $
            summarizeExpansionReferences definitionNames
                prepared emptyExpansionPath $
                    preparedDefinitionBody definition
        return (name, summary)

-- Index every admitted recursive datatype by the source-first representative
-- of its alias-normalized definition-graph component.  Marked acyclic data
-- remain singleton components, preserving the low-level preparation API's
-- defensive behavior even though checked environments normally mark only
-- genuinely recursive heads.
recursiveDataComponents
    :: Set.Set String
    -> [PreparedDefinition]
    -> Map.Map String (Set.Set String)
    -> Map.Map String String
recursiveDataComponents recursiveData definitions references = Map.fromList
    [ (name, representative)
    | component <- stronglyConnComp graph
    , let names = case component of
            AcyclicSCC name -> [name]
            CyclicSCC members -> members
    , Just representative <- [listToMaybe $ sortOn position names]
    , name <- names
    ]
  where
    positions = Map.fromList $ zip
        (map preparedDefinitionName definitions) [0 :: Natural ..]
    position name = Map.findWithDefault fallbackPosition name positions
    fallbackPosition = 1 + maximum (0 : Map.elems positions)
    graph =
        [ (name, name, Set.toAscList $ Set.intersection recursiveData $
            Map.findWithDefault Set.empty name references)
        | definition <- definitions
        , let name = preparedDefinitionName definition
        , name `Set.member` recursiveData
        ]

rejectFirstCycle
    :: Bool
    -> Set.Set String
    -> [PreparedDefinition]
    -> (PreparedDefinition -> Set.Set String)
    -> Either String ()
rejectFirstCycle synonymsOnly allowed definitions references =
    case firstRecursiveComponent allowed definitions references of
        Nothing -> Right ()
        Just names -> Left $
            recursiveDefinitionsMessage synonymsOnly names

firstRecursiveComponent
    :: Set.Set String
    -> [PreparedDefinition]
    -> (PreparedDefinition -> Set.Set String)
    -> Maybe [String]
firstRecursiveComponent allowed definitions references =
    listToMaybe $ sortOn componentPosition orderedCycles
  where
    positions = Map.fromList $ zip
        (map preparedDefinitionName definitions) [0 :: Natural ..]
    position name = Map.findWithDefault fallbackPosition name positions
    fallbackPosition = 1 + maximum (0 : Map.elems positions)
    componentPosition names = minimum $ map position names
    orderedCycles =
        [sortOn position names |
            CyclicSCC names <- stronglyConnComp
                [(name, name, Set.toAscList $ references definition) |
                    definition <- definitions,
                    let name = preparedDefinitionName definition],
            not $ Set.fromList names `Set.isSubsetOf` allowed]

recursiveDefinitionsMessage :: Bool -> [String] -> String
recursiveDefinitionsMessage synonymsOnly names =
    "recursive type " ++ noun ++ ": " ++ intercalate ", " names
  where
    noun
        | synonymsOnly && length names == 1 = "synonym"
        | synonymsOnly = "synonyms"
        | length names == 1 = "definition"
        | otherwise = "definitions"

-- Match association-list lookup: later duplicates are unreachable and cannot
-- add an expansion edge, a malformed view failure, or a spurious cycle.
firstBindings
    :: [FormulaDefinition source]
    -> [FormulaDefinition source]
firstBindings = SharedCollection.distinctOn definitionName

definitionName :: FormulaDefinition source -> String
definitionName definition = case definition of
    FormulaAlias name _ _ -> name
    FormulaData name _ _ -> name
    FormulaAbstract name _ _ -> name

preparedDefinitionName :: PreparedDefinition -> String
preparedDefinitionName (PreparedDefinition name _ _ _) = name

preparedDefinitionBody :: PreparedDefinition -> ExpansionType
preparedDefinitionBody (PreparedDefinition _ _ body _) = body

preparedDefinitionIsAlias :: PreparedDefinition -> Bool
preparedDefinitionIsAlias (PreparedDefinition _ _ _ isAlias) = isAlias

-- Collect every nominal reference, not only applications saturated in the
-- source spelling. Substitution can turn a higher-order argument into a fresh
-- saturated redex.
definitionReferences :: ExpansionType -> Set.Set String
definitionReferences source = case source of
    ExpansionApp function argument ->
        definitionReferences function `Set.union`
            definitionReferences argument
    ExpansionCon name _ -> Set.singleton name
    ExpansionTuple types -> Set.unions $ map definitionReferences types
    ExpansionArrow argument result ->
        definitionReferences argument `Set.union`
            definitionReferences result
    ExpansionForall _ atom -> Set.map SharedName.renderCanonical
        $ SharedType.typeConstructors
        $ SharedTypeAtom.typeAtomType atom
    ExpansionUnion constructors -> Set.unions
        [definitionReferences field |
            (_, fields) <- constructors, field <- fields]
    ExpansionArgument _ argument -> definitionReferences argument
    ExpansionVar _ -> Set.empty
    ExpansionAbstract _ -> Set.empty
