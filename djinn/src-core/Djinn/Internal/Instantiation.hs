-- |
-- Bounded instantiation of hypothesis-side rank-N foralls.
--
-- LJT search is propositional: a quantified type is one opaque proposition.
-- A hypothesis of type @forall as. t@ nevertheless justifies every instance
-- @t[as := ss]@. Generated Haskell normally uses the hypothesis expression
-- directly and lets GHC instantiate it implicitly. A loaded scheme with a
-- binder absent from its residual body instead retains a bounded visible type
-- application: frontends may have erased the constraint which originally
-- determined that binder. This module turns both cases into premise axioms
-- @Opaque(forall as. t) -> compile(t[as := ss])@ over a finite candidate set:
-- the sequent's type variables (including skolems introduced by opened
-- positive foralls), every quantified atom the sequent already mentions,
-- and—for separate additive tails—closed forall-free subtrees of checked
-- queries and value signatures. Quantified candidates give guarded
-- impredicative instantiation; closed candidates may have any kind, but the
-- complete substituted body is kind-checked before compilation.
--
-- Each axiom is a self-contained semantic truth about Haskell types, so
-- adding them can only enlarge the set of provable goals; they never
-- threaten negative evidence. Boundedness loses completeness only, and the
-- query translation and retained non-target loaded schemes report the
-- corresponding incompleteness, so exhausting bounded plans cannot justify a
-- refutation.
module Djinn.Internal.Instantiation
    ( InstantiationAxioms
    , ProviderInstantiationPremises
    , instantiationAxioms
    , queryCorrelatedInstantiationAxioms
    , queryClosedInstantiationAxioms
    , loadedInstantiationAxioms
    , providerInstantiationPremises
    , providerInstantiationAssignmentPremises
    , closedMonotypeSubtrees
    , instantiationAxiomPremises
    , instantiationAxiomSymbols
    , instantiationVisibleApplications
    , providerInstantiationPremiseBindings
    , providerInstantiationApplications
    , rewriteProviderInstantiationEvidence
    , usesInstantiationEvidence
    , eliminateInstantiationEvidence
    ) where

import Data.List (sort, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Djinn.Internal.LJTFormula
import Language.Haskell.Synthesis.Collection (distinctOn)
import Language.Haskell.Synthesis.Constraint (constraintArguments)
import qualified Language.Haskell.Synthesis.Type as SharedType
import qualified Language.Haskell.Synthesis.TypeAtom as SharedTypeAtom
import qualified Language.Haskell.Synthesis.TypeRender as SharedTypeRender
import qualified Language.Haskell.Synthesis.Generated as SharedGenerated
import qualified Language.Haskell.Synthesis.Query as SharedQuery

-- Instantiating a leading chain replaces every binder at once, mirroring
-- Exference's provider rule. Keep this eligibility boundary aligned with the
-- shared exact-assignment contract; longer chains multiply the candidate tuple
-- space and stay opaque.
maxInstantiationBinders :: Int
maxInstantiationBinders =
    SharedQuery.maximumProviderInstantiationArguments

-- The original one- through three-binder rule enumerated a lexically sorted
-- Cartesian product. Keep that exact prefix so widening the rule cannot
-- perturb already documented candidates or rankings.
maxCartesianInstantiationBinders :: Int
maxCartesianInstantiationBinders = 3

-- Axioms are premises in every plan of one instantiation family, so their
-- count bounds both formula size and search branching. The caps are deliberate
-- completeness boundaries, never soundness ones: a dropped axiom can only
-- cause a miss, and misses in this fragment already report 'NoEvidence'.
maxInstantiationAxioms :: Int
maxInstantiationAxioms = 64

maxInstantiationAttempts :: Int
maxInstantiationAttempts = 512

-- Each scheme has a smaller distinct-axiom allowance. Loaded jobs also
-- interleave tuple attempts, so duplicate or ill-kinded tuples from one scheme
-- cannot starve its siblings under the family attempt cap.
maxInstantiationAxiomsPerScheme :: Int
maxInstantiationAxiomsPerScheme = 16

-- Keep direct provider evidence within the same global bound as the public
-- candidate association list, even when several candidates form tuples for a
-- multiply quantified provider.
maxProviderInstantiationPremises :: Int
maxProviderInstantiationPremises = 32

-- | The generated axiom premises together with their reserved proof symbols.
-- The symbols use Djinn's private @$@ namespace, so they cannot collide with
-- declared functions. Ordinary inferable evidence is erased before code
-- generation. Query-local or loaded evidence whose vacuous binders require a
-- visible choice is retained in 'instantiationVisibleApplications' until proof
-- conversion.
data InstantiationAxioms = InstantiationAxioms
    { instantiationAxiomPremises :: [(Symbol, Formula)]
    , instantiationAxiomSymbols :: Set.Set Symbol
    , instantiationVisibleApplications ::
        Map.Map Symbol [SharedGenerated.VisibleTypeArgument]
    }

-- | Provider-local specializations justified by caller-supplied evidence.
--
-- Unlike an ordinary instantiation axiom, each premise is the already
-- specialized provider result rather than an implication from a scheme atom.
-- Its synthetic proof symbol remains paired with the one exact provider whose
-- external environment established the choice. Proof checking sees only the
-- specialized premise; generated lowering later expands a use into an
-- application of the synthetic evidence to that provider.
data ProviderInstantiationPremises = ProviderInstantiationPremises
    { providerInstantiationPremiseBindings :: [(Symbol, Formula)]
    , providerInstantiationApplications ::
        Map.Map Symbol (Symbol, [SharedGenerated.VisibleTypeArgument])
    }

-- Structural lowering may erase a caller-supplied type argument before proof
-- search sees it. The prepared environment marks the complete simultaneous
-- substitution, then checks every reached datatype-argument boundary. This
-- catches phantom loss inside an argument and through a higher-kinded head
-- without rejecting constructor fields which do preserve the choice. Nominal
-- lowering preserves the full application and therefore passes no checker. A
-- wholly vacuous vector remains accepted because no marker reaches the body.
type ProviderInstantiationFidelity =
    [String]
    -> [SharedType.Type String]
    -> SharedType.Type String
    -> Either String Bool

retainsProviderInstantiationFidelity
    :: Maybe ProviderInstantiationFidelity
    -> InstantiationScheme
    -> [SharedType.Type String]
    -> Bool
retainsProviderInstantiationFidelity fidelityTranslator scheme arguments =
    case fidelityTranslator of
        Nothing -> True
        Just checkFidelity -> case checkFidelity
                (schemeBinders scheme)
                arguments
                (schemeBody scheme) of
            Right retainedFidelity -> retainedFidelity
            Left _ -> False

-- One retained logical axiom plus the explicit type arguments required when
-- its proof evidence cannot safely collapse to an implicitly instantiated
-- occurrence. The latter is populated only when a leading binder is absent
-- from the residual body.
data InstantiationAxiom = InstantiationAxiom
    { instantiationAxiomFormula :: Formula
    , instantiationAxiomVisibleArguments ::
        Maybe [SharedGenerated.VisibleTypeArgument]
    }
    deriving (Eq, Ord)

-- Whether a subformula position provides data to the prover or demands it.
-- A premise root is hypothesis-side; a goal root is obligation-side; each
-- implication domain flips the side; every other connective preserves it.
data SequentSide = HypothesisSide | ObligationSide
    deriving Eq

flipSide :: SequentSide -> SequentSide
flipSide side = case side of
    HypothesisSide -> ObligationSide
    ObligationSide -> HypothesisSide

sidedAtomSymbols :: SequentSide -> Formula -> [(SequentSide, Symbol)]
sidedAtomSymbols side formula = case formula of
    Conj children -> concatMap (sidedAtomSymbols side) children
    Disj alternatives ->
        concatMap (sidedAtomSymbols side . snd) alternatives
    Empty _ -> []
    argument :-> result ->
        sidedAtomSymbols (flipSide side) argument ++
        sidedAtomSymbols side result
    PVar symbol -> [(side, symbol)]

-- One instantiable hypothesis scheme: a context-free leading forall chain of
-- bounded length. Constrained chains stay opaque because Djinn has no
-- premise vocabulary for their class obligations.
data InstantiationScheme = InstantiationScheme
    { schemeSource :: SharedType.Type String
    , schemeBinders :: [String]
    , schemeBody :: SharedType.Type String
    }

instantiationScheme
    :: SharedType.Type String -> Maybe InstantiationScheme
instantiationScheme source = case SharedType.splitLeadingForalls source of
    (binders@(_ : _), [], body)
        | length binders <= maxInstantiationBinders ->
            Just $ InstantiationScheme source binders body
    _ -> Nothing

schemeKey :: InstantiationScheme -> SharedTypeAtom.TypeAtomKey String
schemeKey = SharedTypeAtom.alphaTypeKey . schemeSource

-- | Build the bounded axiom set for one query.
--
-- The translator must be the sealed environment's opaque formula compiler:
-- instantiated bodies keep nested quantifiers as alpha-stable atoms, so an
-- axiom can chain into another axiom only through an atom the closure has
-- itself collected. Goal formulas enter at obligation side and premises at
-- hypothesis side; candidate variable spellings arrive from the source-level
-- goal and premise scopes plus every opened-forall skolem, so no rendered
-- atom spelling is ever parsed back into a type.
--
-- A failed instantiation or compilation drops that one axiom rather than the
-- query: the axioms are an optional capability, and every kept axiom is
-- checked by construction against the same formula vocabulary the sequent
-- uses.
instantiationAxioms
    :: (SharedType.Type String -> Either String Formula)
    -> (SharedType.Type String ->
        Maybe SharedGenerated.VisibleTypeArgument)
    -> [String]
    -> [Formula]
    -> [Formula]
    -> InstantiationAxioms
instantiationAxioms translator visibleArgument variableSpellings goalFormulas
        premiseFormulas =
    buildInstantiationAxioms "$djinn$instantiation$" translator
        False
        visibleArgument
        (\scheme -> historicalCandidateTuples
            historicalCandidates wideCandidates
            (length $ schemeBinders scheme))
        initialSchemes
  where
    atoms =
        concatMap (sidedAtomSymbols ObligationSide) goalFormulas ++
        concatMap (sidedAtomSymbols HypothesisSide) premiseFormulas
    opaqueSources =
        [ source
        | (_, symbol) <- atoms
        , Just source <- [opaqueSymbolSource symbol]
        ]
    sourceVariableCandidates = map SharedType.TypeVariable $
        distinctOn id variableSpellings
    historicalVariableCandidates = map SharedType.TypeVariable $
        distinctOn id $ sort variableSpellings
    quantifiedCandidates =
        distinctOn SharedTypeAtom.alphaTypeKey $
        sortOn (SharedTypeRender.renderType id) $
        concatMap closedQuantifiedSubtrees opaqueSources
    historicalCandidates =
        historicalVariableCandidates ++ quantifiedCandidates
    -- The callers supply source first-occurrence order: goal variables first,
    -- then opened skolems and sealed premise scopes. Retain that order for the
    -- wide-binder heuristics so alpha-renaming cannot reshuffle useful
    -- source-ordered runs merely because their spellings sort differently.
    wideCandidates = sourceVariableCandidates ++ quantifiedCandidates
    initialSchemes = distinctOn schemeKey
        [ scheme
        | (HypothesisSide, symbol) <- atoms
        , Just source <- [opaqueSymbolSource symbol]
        , Just scheme <- [instantiationScheme source]
        ]

-- | Build an additive positive-only family for a query-local scheme instance
-- whose complete result already occurs in the checked query.
--
-- The historical one- through three-binder family deliberately keeps its
-- lexical Cartesian prefix, which means its sixteen-axiom scheme allowance can
-- miss a later tuple of two different guarded quantified candidates.  This
-- tail preserves that prefix and instead fairly considers the same finite
-- variable/quantified vocabulary.  It retains only tuples which put a
-- quantified candidate in a result-relevant binder, produce a logical axiom
-- not already retained by the complete bounded historical run, and instantiate
-- the scheme body to an alpha-equal subtree of the exact query.
-- No type is invented and every surviving body still crosses the prepared
-- kind check, formula translator, proof checker, and evidence-erasure boundary.
queryCorrelatedInstantiationAxioms
    :: (SharedType.Type String -> Either String Formula)
    -> (SharedType.Type String ->
        Maybe SharedGenerated.VisibleTypeArgument)
    -> InstantiationAxioms
    -> [String]
    -> SharedType.Type String
    -> [Formula]
    -> [Formula]
    -> InstantiationAxioms
queryCorrelatedInstantiationAxioms translator visibleArgument
        historicalAxioms variableSpellings elaboratedGoal
        goalFormulas premiseFormulas =
    buildInstantiationAxiomsExcluding historicalAxiomSet
        "$djinn$query-correlated-instantiation$" translator True
        visibleArgument correlatedTuples initialSchemes
  where
    candidateAtoms =
        concatMap (sidedAtomSymbols ObligationSide) goalFormulas ++
        concatMap (sidedAtomSymbols HypothesisSide) premiseFormulas
    schemeAtoms = concatMap (sidedAtomSymbols ObligationSide) goalFormulas
    opaqueSources =
        [ source
        | (_, symbol) <- candidateAtoms
        , Just source <- [opaqueSymbolSource symbol]
        ]
    sourceVariableCandidates = map SharedType.TypeVariable $
        distinctOn id variableSpellings
    quantifiedCandidates =
        distinctOn SharedTypeAtom.alphaTypeKey $
        sortOn (SharedTypeRender.renderType id) $
        concatMap closedQuantifiedSubtrees opaqueSources
    quantifiedCandidateKeys = Set.fromList $
        map SharedTypeAtom.alphaTypeKey quantifiedCandidates
    candidates = distinctOn SharedTypeAtom.alphaTypeKey $
        sourceVariableCandidates ++ quantifiedCandidates
    queryTargetKeys = Set.fromList $
        map SharedTypeAtom.alphaTypeKey $
        typeSubtrees $ SharedType.canonicalizeType elaboratedGoal
    historicalAxiomSet = Set.fromList
        [ InstantiationAxiom formula $
            Map.lookup symbol $
                instantiationVisibleApplications historicalAxioms
        | (symbol, formula) <- instantiationAxiomPremises historicalAxioms
        ]
    correlatedTuples _ | null quantifiedCandidates = []
    correlatedTuples scheme =
        [ arguments
        | arguments <- take maxInstantiationAttempts $
            fairCandidateTuples candidates arity
        , or
            [ binder `Set.member` bodyVariables &&
                SharedTypeAtom.alphaTypeKey argument
                    `Set.member` quantifiedCandidateKeys
            | (binder, argument) <- zip (schemeBinders scheme) arguments
            ]
        , Right instantiated <- [instantiateSchemeBody scheme arguments]
        , SharedTypeAtom.alphaTypeKey instantiated
            `Set.member` queryTargetKeys
        ]
      where
        arity = length $ schemeBinders scheme
        bodyVariables = SharedType.freeVariables $ schemeBody scheme
    initialSchemes = distinctOn schemeKey
        [ scheme
        | (HypothesisSide, symbol) <- schemeAtoms
        , Just source <- [opaqueSymbolSource symbol]
        , Just scheme <- [instantiationScheme source]
        ]

-- | Build an additive hypothesis-instantiation tail whose tuples use at
-- least one closed, forall-free subtree already present in the checked query.
--
-- The historical family above deliberately retains its exact candidate order
-- and proof-plan position.  Re-enumerating its expanded pool in place would
-- perturb established result prefixes, while enumerating all tuples again in
-- a second family would duplicate every historical axiom.  This family
-- therefore fairly schedules the combined pool but keeps only tuples which
-- contain a supplied closed candidate.  Mixed variable/quantified/closed
-- tuples remain available, and the existing per-scheme, family, and attempt
-- caps apply independently to this positive-only completeness extension.
queryClosedInstantiationAxioms
    :: (SharedType.Type String -> Either String Formula)
    -> (SharedType.Type String ->
        Maybe SharedGenerated.VisibleTypeArgument)
    -> [String]
    -> [SharedType.Type String]
    -> [Formula]
    -> [Formula]
    -> InstantiationAxioms
queryClosedInstantiationAxioms translator visibleArgument variableSpellings
        closedCandidates goalFormulas premiseFormulas =
    buildInstantiationAxioms "$djinn$query-closed-instantiation$" translator
        True
        visibleArgument
        candidateTuples
        initialSchemes
  where
    candidateAtoms =
        concatMap (sidedAtomSymbols ObligationSide) goalFormulas ++
        concatMap (sidedAtomSymbols HypothesisSide) premiseFormulas
    -- Only hypothesis-side schemes embedded in the requested goal belong to
    -- this local family. Exact loaded schemes have their own retained-source
    -- path; rediscovering global premise variants here would duplicate that
    -- path and could misclassify an ordinary safe local axiom as target-only
    -- diagnostic evidence.
    schemeAtoms = concatMap (sidedAtomSymbols ObligationSide) goalFormulas
    opaqueSources =
        [ source
        | (_, symbol) <- candidateAtoms
        , Just source <- [opaqueSymbolSource symbol]
        ]
    variableCandidates = map SharedType.TypeVariable $
        distinctOn id variableSpellings
    quantifiedCandidates =
        distinctOn SharedTypeAtom.alphaTypeKey $
        sortOn (SharedTypeRender.renderType id) $
        concatMap closedQuantifiedSubtrees opaqueSources
    retainedClosedCandidates =
        distinctOn SharedTypeAtom.alphaTypeKey closedCandidates
    closedCandidateKeys = Set.fromList $
        map SharedTypeAtom.alphaTypeKey retainedClosedCandidates
    candidates = distinctOn SharedTypeAtom.alphaTypeKey $
        variableCandidates ++ quantifiedCandidates ++ retainedClosedCandidates
    candidateTuples scheme = filter containsClosedCandidate $
        fairCandidateTuples candidates $ length $ schemeBinders scheme
    containsClosedCandidate = any $ \candidate ->
        SharedTypeAtom.alphaTypeKey candidate `Set.member` closedCandidateKeys
    initialSchemes = distinctOn schemeKey
        [ scheme
        | (HypothesisSide, symbol) <- schemeAtoms
        , Just source <- [opaqueSymbolSource symbol]
        , Just scheme <- [instantiationScheme source]
        ]

-- | Build the additive instantiation tail for polymorphic values retained from
-- the sealed environment. Unlike the historical hypothesis rule, this phase
-- extends the variable and guarded-impredicative candidates with closed
-- monotype subtrees supplied by the checked query and loaded signatures. Its
-- scheme seeds are explicit so ordinary query-local schemes are not
-- rediscovered under a second proof-symbol family.
loadedInstantiationAxioms
    :: (SharedType.Type String -> Either String Formula)
    -> (SharedType.Type String ->
        Maybe SharedGenerated.VisibleTypeArgument)
    -> [String]
    -> [SharedType.Type String]
    -> [Formula]
    -> [Formula]
    -> [Formula]
    -> InstantiationAxioms
loadedInstantiationAxioms translator visibleArgument variableSpellings
        closedCandidates goalFormulas premiseFormulas loadedSchemeFormulas =
    buildInstantiationAxioms "$djinn$loaded-instantiation$" translator
        True
        visibleArgument
        (\scheme -> fairCandidateTuples candidates $
            length $ schemeBinders scheme)
        initialSchemes
  where
    candidateAtoms =
        concatMap (sidedAtomSymbols ObligationSide) goalFormulas ++
        concatMap (sidedAtomSymbols HypothesisSide) premiseFormulas
    schemeAtoms = concatMap (sidedAtomSymbols HypothesisSide)
        loadedSchemeFormulas
    opaqueSources =
        [ source
        | (_, symbol) <- candidateAtoms
        , Just source <- [opaqueSymbolSource symbol]
        ]
    variableCandidates = map SharedType.TypeVariable $
        distinctOn id variableSpellings
    quantifiedCandidates =
        distinctOn SharedTypeAtom.alphaTypeKey $
        sortOn (SharedTypeRender.renderType id) $
        concatMap closedQuantifiedSubtrees opaqueSources
    candidates = distinctOn SharedTypeAtom.alphaTypeKey $
        variableCandidates ++ quantifiedCandidates ++ closedCandidates
    initialSchemes = distinctOn schemeKey
        [ scheme
        | (HypothesisSide, symbol) <- schemeAtoms
        , Just source <- [opaqueSymbolSource symbol]
        , Just scheme <- [instantiationScheme source]
        ]

-- | Build a bounded family of direct provider-local specialization premises.
--
-- Candidate associations have already crossed the checked public boundary:
-- their provider symbols name exact loaded schemes and each type is a closed,
-- context-free proper type with the retained visible argument supplied beside
-- it. Scheme order is the prepared environment's declaration order; candidate
-- order is the caller's first-occurrence order for that provider. A provider
-- receives no candidate associated with another provider, even when their
-- quantified schemes are alpha-equivalent. When a structural checker is
-- supplied, a completed Cartesian tuple is retained only if lowering preserves
-- that exact vector; nominal specialization supplies no checker.
providerInstantiationPremises
    :: String
    -> (SharedType.Type String -> Either String Formula)
    -> Maybe ProviderInstantiationFidelity
    -> [(Symbol, Formula)]
    -> [( Symbol
        , SharedType.Type String
        , SharedGenerated.VisibleTypeArgument
        )]
    -> ProviderInstantiationPremises
providerInstantiationPremises
        symbolPrefix translator fidelityTranslator schemes candidates =
    ProviderInstantiationPremises premises applications
  where
    retained = take maxProviderInstantiationPremises $ concatMap specialize schemes
    entries =
        [ (Symbol $ symbolPrefix ++ show index, provider, formula, arguments)
        | (index, (provider, formula, arguments)) <-
            zip [0 :: Int ..] retained
        ]
    premises =
        [ (synthetic, formula)
        | (synthetic, _, formula, _) <- entries
        ]
    applications = Map.fromList
        [ (synthetic, (provider, arguments))
        | (synthetic, provider, _, arguments) <- entries
        ]

    specialize (provider, schemeFormula) = case schemeSourceFromFormula
            schemeFormula >>= instantiationScheme of
        Nothing -> []
        Just scheme -> take maxInstantiationAxiomsPerScheme
            [ (provider, formula, map third tuple)
            | tuple <- take maxInstantiationAttempts $
                sequence $ replicate (length $ schemeBinders scheme)
                    providerCandidates
            , Right instantiated <-
                [instantiateSchemeBody scheme $ map second tuple]
            , Right formula <- [translator instantiated]
            , retainsProviderInstantiationFidelity
                fidelityTranslator scheme (map second tuple)
            ]
      where
        providerCandidates =
            [ (candidateType, visibleArgument)
            | (candidateProvider, candidateType, visibleArgument) <- candidates
            , candidateProvider == provider
            ]

    schemeSourceFromFormula formula = case formula of
        PVar symbol -> opaqueSymbolSource symbol
        _ -> Nothing

    second (value, _) = value
    third (_, value) = value

-- | Build direct provider-local premises from complete ordered assignments.
--
-- Each vector has already been checked against the exact provider scheme at
-- the public boundary.  In particular, its length is the leading-binder
-- arity, its arguments are closed types checked at their positional binder
-- kinds, and duplicate alpha-equivalent vectors for one provider have been
-- removed.  Consume the vector as one
-- correlated choice: unlike 'providerInstantiationPremises', this path never
-- reconstructs tuples through a Cartesian product and therefore does not use
-- the historical per-scheme attempt window.
providerInstantiationAssignmentPremises
    :: String
    -> (SharedType.Type String -> Either String Formula)
    -> Maybe ProviderInstantiationFidelity
    -> [(Symbol, Formula)]
    -> [( Symbol
        , [( SharedType.Type String
           , SharedGenerated.VisibleTypeArgument
           )]
        )]
    -> ProviderInstantiationPremises
providerInstantiationAssignmentPremises
        symbolPrefix translator fidelityTranslator schemes assignments =
    ProviderInstantiationPremises premises applications
  where
    retained = take maxProviderInstantiationPremises $
        concatMap specialize schemes
    entries =
        [ (Symbol $ symbolPrefix ++ show index, provider, formula, arguments)
        | (index, (provider, formula, arguments)) <-
            zip [0 :: Int ..] retained
        ]
    premises =
        [ (synthetic, formula)
        | (synthetic, _, formula, _) <- entries
        ]
    applications = Map.fromList
        [ (synthetic, (provider, arguments))
        | (synthetic, provider, _, arguments) <- entries
        ]

    specialize (provider, schemeFormula) = case schemeSourceFromFormula
            schemeFormula >>= instantiationScheme of
        Nothing -> []
        Just scheme ->
            [ (provider, formula, map snd assignment)
            | (assignmentProvider, assignment) <- assignments
            , assignmentProvider == provider
            , length assignment == length (schemeBinders scheme)
            , Right instantiated <-
                [instantiateSchemeBody scheme $ map fst assignment]
            , Right formula <- [translator instantiated]
            , retainsProviderInstantiationFidelity
                fidelityTranslator scheme (map fst assignment)
            ]

    schemeSourceFromFormula formula = case formula of
        PVar symbol -> opaqueSymbolSource symbol
        _ -> Nothing

-- | Expand every free use of a provider-specialization premise into an
-- application to its exact provider. The synthetic head remains in place so
-- the existing visible-evidence converter can turn
-- @synthetic provider@ into @provider @T ...@. LJT reserves environment
-- symbols from binder allocation, but deleting a shadowed key keeps this
-- helper correct for independently constructed terms as well.
rewriteProviderInstantiationEvidence
    :: Map.Map Symbol
        (Symbol, [SharedGenerated.VisibleTypeArgument])
    -> Term
    -> Term
rewriteProviderInstantiationEvidence evidence = go evidence
  where
    go visible term = case term of
        Var synthetic
            | Just (provider, _) <- Map.lookup synthetic visible ->
                Apply (Var synthetic) (Var provider)
        Lam binder body -> Lam binder $ go (Map.delete binder visible) body
        Apply function argument -> Apply
            (go visible function) (go visible argument)
        Xsel index arity expression ->
            Xsel index arity $ go visible expression
        _ -> term

buildInstantiationAxioms
    :: String
    -> (SharedType.Type String -> Either String Formula)
    -> Bool
    -> (SharedType.Type String ->
        Maybe SharedGenerated.VisibleTypeArgument)
    -> (InstantiationScheme -> [[SharedType.Type String]])
    -> [InstantiationScheme]
    -> InstantiationAxioms
buildInstantiationAxioms =
    buildInstantiationAxiomsWithExclusions False Set.empty

buildInstantiationAxiomsExcluding
    :: Set.Set InstantiationAxiom
    -> String
    -> (SharedType.Type String -> Either String Formula)
    -> Bool
    -> (SharedType.Type String ->
        Maybe SharedGenerated.VisibleTypeArgument)
    -> (InstantiationScheme -> [[SharedType.Type String]])
    -> [InstantiationScheme]
    -> InstantiationAxioms
buildInstantiationAxiomsExcluding =
    buildInstantiationAxiomsWithExclusions True

buildInstantiationAxiomsWithExclusions
    :: Bool
    -> Set.Set InstantiationAxiom
    -> String
    -> (SharedType.Type String -> Either String Formula)
    -> Bool
    -> (SharedType.Type String ->
        Maybe SharedGenerated.VisibleTypeArgument)
    -> (InstantiationScheme -> [[SharedType.Type String]])
    -> [InstantiationScheme]
    -> InstantiationAxioms
buildInstantiationAxiomsWithExclusions deduplicateFormula excludedAxioms
        symbolPrefix translator interleaveSchemes
        visibleArgument candidateTuples schemes =
    InstantiationAxioms premises symbols visibleApplications
  where
    entries =
        [ (Symbol $ symbolPrefix ++ show index, axiom)
        | (index, axiom) <- zip [0 :: Int ..] $
            buildAxiomFormulas deduplicateFormula excludedAxioms
                interleaveSchemes translator visibleArgument
                candidateTuples schemes
        ]
    premises =
        [ (symbol, instantiationAxiomFormula axiom)
        | (symbol, axiom) <- entries
        ]
    symbols = Set.fromList $ map fst premises
    visibleApplications = Map.fromList
        [ (symbol, arguments)
        | (symbol, axiom) <- entries
        , Just arguments <- [instantiationAxiomVisibleArguments axiom]
        ]

-- | Enumerate every source subtree which is closed and contains no explicit
-- forall. Candidates may have any kind: a higher-kinded subtree can be the
-- correct image of a higher-kinded binder. The caller checks the complete
-- instantiated body in its prepared kind scope before retaining an axiom.
closedMonotypeSubtrees
    :: SharedType.Type String -> [SharedType.Type String]
closedMonotypeSubtrees = distinctOn SharedTypeAtom.alphaTypeKey .
    filter isClosedMonotype . typeSubtrees . SharedType.canonicalizeType
  where
    isClosedMonotype source =
        Set.null (SharedType.freeVariables source) &&
            not (SharedType.containsForall source)

typeSubtrees :: SharedType.Type String -> [SharedType.Type String]
typeSubtrees source = source : case source of
    SharedType.TypeVariable{} -> []
    SharedType.TypeConstructor{} -> []
    SharedType.TypeApplication function argument ->
        typeSubtrees function ++ typeSubtrees argument
    SharedType.FunctionType parameter result ->
        typeSubtrees parameter ++ typeSubtrees result
    SharedType.TupleType _ elements -> concatMap typeSubtrees elements
    SharedType.ForallType _ constraints body ->
        concatMap typeSubtrees (concatMap constraintArguments constraints) ++
            typeSubtrees body

-- Quantified subtrees and their impredicative wrappers are usable as
-- instantiation arguments. A nested forall whose body mentions an enclosing
-- binder is not a sequent-level type, so only subtrees closed under the whole
-- atom's free variables qualify.
closedQuantifiedSubtrees
    :: SharedType.Type String -> [SharedType.Type String]
closedQuantifiedSubtrees source =
    [ subtree
    | subtree <- quantifiedSubtrees source
    , SharedType.freeVariables subtree `Set.isSubsetOf` sourceFree
    ]
  where
    sourceFree = SharedType.freeVariables source

-- Every subtree which contains an explicit forall, including structural
-- ancestors such as @Maybe (forall a. a -> a)@. The containing wrapper is a
-- query-supplied type just as surely as the quantified atom itself; retaining
-- it lets guarded impredicative instantiation use the complete supplied shape
-- without guessing a new quantifier.
quantifiedSubtrees :: SharedType.Type String -> [SharedType.Type String]
quantifiedSubtrees source = case source of
    SharedType.TypeVariable{} -> []
    SharedType.TypeConstructor{} -> []
    SharedType.TypeApplication function argument ->
        containing source $
            quantifiedSubtrees function ++ quantifiedSubtrees argument
    SharedType.FunctionType parameter result ->
        containing source $
            quantifiedSubtrees parameter ++ quantifiedSubtrees result
    SharedType.TupleType _ elements -> containing source $
        concatMap quantifiedSubtrees elements
    SharedType.ForallType _ constraints body ->
        source :
            concatMap quantifiedSubtrees
                (concatMap constraintArguments constraints) ++
            quantifiedSubtrees body
  where
    containing wrapper nested
        | null nested = []
        | otherwise = wrapper : nested

-- Worklist over schemes. Instantiating a scheme can expose a strictly
-- shallower hypothesis-side forall in its own body; such discoveries join the
-- end of the queue. Loaded jobs are interleaved for attempt fairness, while
-- historical jobs retain their compatibility order.
data InstantiationJob
    = SchemeJob InstantiationScheme
    | AxiomJob InstantiationScheme Int [[SharedType.Type String]]

buildAxiomFormulas
    :: Bool
    -> Set.Set InstantiationAxiom
    -> Bool
    -> (SharedType.Type String -> Either String Formula)
    -> (SharedType.Type String ->
        Maybe SharedGenerated.VisibleTypeArgument)
    -> (InstantiationScheme -> [[SharedType.Type String]])
    -> [InstantiationScheme]
    -> [InstantiationAxiom]
buildAxiomFormulas deduplicateFormula excludedAxioms interleaveSchemes
        translator visibleArgument candidateTuples initialSchemes = loop
    (Set.fromList $ map schemeKey startingSchemes)
    excludedAxioms
    maxInstantiationAttempts
    maxInstantiationAxioms
    (map SchemeJob startingSchemes)
  where
    -- A retained historical axiom can expose a shallower hypothesis scheme
    -- without the correlated tuple producer ever recreating that bridge (for
    -- example when the bridge itself uses no quantified argument). Seed those
    -- descendants directly; later duplicate attempts retain the same closure
    -- behavior for schemes exposed recursively by correlated formulas.
    startingSchemes = distinctOn schemeKey $
        initialSchemes ++
        concatMap
            (discoveredSchemes . instantiationAxiomFormula)
            (Set.toList excludedAxioms)

    loop seenSchemes seenAxioms attempts allowance queue = case queue of
        _ | attempts <= 0 || allowance <= 0 -> []
        [] -> []
        SchemeJob scheme : jobs -> loop seenSchemes seenAxioms attempts
            allowance $
                AxiomJob scheme maxInstantiationAxiomsPerScheme
                    (candidateTuples scheme)
                : jobs
        AxiomJob _ _ [] : jobs ->
            loop seenSchemes seenAxioms attempts allowance jobs
        AxiomJob scheme schemeAllowance (arguments : tuples) : jobs
            | schemeAllowance <= 0 ->
                loop seenSchemes seenAxioms attempts allowance jobs
            | otherwise -> case schemeAxiom scheme arguments of
                Just axiom ->
                    let discovered = distinctOn schemeKey
                            [ found
                            | found <- discoveredSchemes
                                $ instantiationAxiomFormula axiom
                            , schemeKey found `Set.notMember` seenSchemes
                            ]
                        seenSchemes' = foldr (Set.insert . schemeKey)
                            seenSchemes discovered
                        next retainedAllowance =
                            reschedule
                                (AxiomJob scheme retainedAllowance tuples)
                                jobs ++ map SchemeJob discovered
                        alreadySeen =
                            axiom `Set.member` seenAxioms ||
                            ( deduplicateFormula && any
                                ((instantiationAxiomFormula axiom ==) .
                                    instantiationAxiomFormula)
                                (Set.toList seenAxioms)
                            )
                    in if alreadySeen
                        then loop seenSchemes' seenAxioms
                            (attempts - 1) allowance
                            (next schemeAllowance)
                        else axiom : loop seenSchemes'
                            (Set.insert axiom seenAxioms)
                            (attempts - 1)
                            (allowance - 1)
                            (next $ schemeAllowance - 1)
                _ -> loop seenSchemes seenAxioms (attempts - 1) allowance
                    (reschedule
                        (AxiomJob scheme schemeAllowance tuples) jobs)

    -- Historical query-local axioms retain their exact depth-first sequence.
    -- Loaded declarations instead take one tuple attempt per turn, so a
    -- vacuous or mostly ill-kinded early scheme cannot spend the global
    -- attempt allowance before a later provider receives its first chance.
    reschedule job jobs
        | interleaveSchemes = jobs ++ [job]
        | otherwise = job : jobs

    schemeAxiom scheme arguments = do
        instantiated <- rightToMaybe $
            instantiateSchemeBody scheme arguments
        bodyFormula <- rightToMaybe $ translator instantiated
        let hypothesis = PVar $ opaqueTypeSymbol $ schemeSource scheme
        if bodyFormula == hypothesis
            then Nothing
            else Just $ InstantiationAxiom
                (hypothesis :-> bodyFormula)
                (visibleArguments scheme arguments)

    visibleArguments scheme arguments = case vacuousPrefixLengths of
        [] -> Nothing
        lengths -> traverse retainArgument $
            take (last lengths) $ zip (schemeBinders scheme) arguments
      where
        bodyVariables = SharedType.freeVariables $ schemeBody scheme
        retainArgument (binder, argument)
            | binder `Set.member` bodyVariables =
                Just SharedGenerated.inferredVisibleTypeArgument
            | otherwise = visibleArgument argument
        -- Visible application is positional. Retain the shortest prefix that
        -- reaches every vacuous binder, using inferred placeholders for any
        -- earlier open choice; later binders remain implicit so GHC can still
        -- perform ordinary and guarded impredicative inference for them.
        vacuousPrefixLengths =
            [ index
            | (index, binder) <- zip [1 :: Int ..] $ schemeBinders scheme
            , binder `Set.notMember` bodyVariables
            ]

    -- The axiom itself acts as a premise: its domain atom becomes an
    -- obligation while its body joins the hypothesis side, so only body
    -- schemes feed the closure.
    discoveredSchemes axiom =
        [ scheme
        | (HypothesisSide, symbol) <-
            sidedAtomSymbols HypothesisSide axiom
        , Just source <- [opaqueSymbolSource symbol]
        , Just scheme <- [instantiationScheme source]
        ]

    rightToMaybe = either (const Nothing) Just

-- One- through three-binder query-local schemes retain their exact historical
-- lexical Cartesian order. Wider schemes use the source-order widening
-- introduced with the original bounded rule.
historicalCandidateTuples
    :: [SharedType.Type String]
    -> [SharedType.Type String]
    -> Int
    -> [[SharedType.Type String]]
historicalCandidateTuples historicalCandidates wideCandidates arity
    | arity <= maxCartesianInstantiationBinders =
        sequence $ replicate arity historicalCandidates
    | otherwise = fairCandidateTuplesWith False wideCandidates arity

-- Loaded declarations can be late in a standard session. Draw from both ends
-- and interleave useful tuple families for every arity so the bounded prefix
-- cannot be monopolized by unrelated earlier declarations.
fairCandidateTuples
    :: [SharedType.Type String]
    -> Int
    -> [[SharedType.Type String]]
fairCandidateTuples = fairCandidateTuplesWith True

fairCandidateTuplesWith
    :: Bool
    -> [SharedType.Type String]
    -> Int
    -> [[SharedType.Type String]]
fairCandidateTuplesWith recentFirst candidates arity =
    distinctOn id $ roundRobin
    [ prioritizeEdges recentFirst $ candidateWindows arity candidates
    , map (replicate arity) $ prioritizeEdges recentFirst candidates
    , roundRobin
        [ orderedSelections arity candidates
        , orderedSelections arity $ reverse candidates
        ]
    , sequence $ replicate arity candidates
    ]

candidateWindows :: Int -> [value] -> [[value]]
candidateWindows arity values
    | arity <= 0 = [[]]
    | otherwise = case splitAt arity values of
        (window, _)
            | length window == arity ->
                window : candidateWindows arity (drop 1 values)
        _ -> []

orderedSelections :: Int -> [value] -> [[value]]
orderedSelections 0 _ = [[]]
orderedSelections _ [] = []
orderedSelections count (value : values) =
    map (value :)
        (orderedSelections (count - 1) values) ++
    orderedSelections count values

edgeFirst :: Ord value => [value] -> [value]
edgeFirst values = distinctOn id $ roundRobin [values, reverse values]

-- Loaded declarations are commonly appended to the standard environment, so
-- give that recent edge the first slot while still alternating both ends.
lateEdgeFirst :: Ord value => [value] -> [value]
lateEdgeFirst values = distinctOn id $ roundRobin [reverse values, values]

prioritizeEdges :: Ord value => Bool -> [value] -> [value]
prioritizeEdges recentFirst
    | recentFirst = lateEdgeFirst
    | otherwise = edgeFirst

roundRobin :: [[value]] -> [value]
roundRobin streams = case
        [ (value, remaining)
        | value : remaining <- streams
        ] of
    [] -> []
    active -> map fst active ++ roundRobin (map snd active)

instantiateSchemeBody
    :: InstantiationScheme
    -> [SharedType.Type String]
    -> Either
        (SharedType.SubstitutionError String)
        (SharedType.Type String)
instantiateSchemeBody scheme arguments =
    SharedType.substituteTypeVariables freshVariable Set.empty
        (Map.fromList $ zip (schemeBinders scheme) arguments)
        (schemeBody scheme)
  where
    freshVariable reserved variable = Just $ choose $ variable ++ "'"
      where
        choose candidate
            | candidate `Set.member` reserved = choose $ candidate ++ "'"
            | otherwise = candidate

-- | Whether a checked proof actually refers to one of the query's erased
-- instantiation axioms. Merely having axioms in the proof environment must not
-- perturb historical simplification for proofs which do not consume them.
usesInstantiationEvidence :: Set.Set Symbol -> Term -> Bool
usesInstantiationEvidence axioms
    | Set.null axioms = const False
    | otherwise = go
  where
    go term = case term of
        Var symbol -> symbol `Set.member` axioms
        Lam _ body -> go body
        Apply function argument -> go function || go argument
        Xsel _ _ expression -> go expression
        _ -> False

-- | Erase the caller-selected implicit instantiation evidence from a checked
-- proof before code generation. Semantically each selected axiom is the
-- identity function: an applied occurrence reduces to its argument, so the
-- generated Haskell uses the polymorphic hypothesis directly and GHC
-- re-instantiates it at the required type; a bare occurrence becomes an
-- explicit identity lambda, which GHC checks against the implication's rank-N
-- domain bidirectionally. Evidence with a visible type choice is deliberately
-- excluded by the caller and lowered separately. LJT allocates binders away
-- from every environment symbol, so an axiom symbol can never be shadowed
-- inside a proof term.
eliminateInstantiationEvidence :: Set.Set Symbol -> Term -> Term
eliminateInstantiationEvidence axioms
    | Set.null axioms = id
    | otherwise = go
  where
    go term = case term of
        -- The applied test precedes recursion: rewriting the axiom variable
        -- first would leave a redundant identity redex in the output.
        Apply (Var symbol) argument
            | symbol `Set.member` axioms -> go argument
        Apply function argument -> Apply (go function) $ go argument
        Var symbol
            | symbol `Set.member` axioms ->
                Lam identityBinder $ Var identityBinder
        Lam binder body -> Lam binder $ go body
        Xsel index arity body -> Xsel index arity $ go body
        _ -> term

    -- Renamed by generated-output freshening; the spelling only needs to be
    -- outside the declared-function namespace.
    identityBinder = Symbol "$djinn$instantiated"
