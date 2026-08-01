-- |
-- Bounded instantiation of hypothesis-side rank-N foralls.
--
-- LJT search is propositional: a quantified type is one opaque proposition.
-- A hypothesis of type @forall as. t@ nevertheless justifies every instance
-- @t[as := ss]@, and the generated Haskell evidence for that step is the
-- hypothesis expression itself, because GHC instantiates value occurrences
-- implicitly. This module turns that semantic fact into explicit premise
-- axioms @Opaque(forall as. t) -> compile(t[as := ss])@ over a finite
-- candidate set: the sequent's type variables (including skolems introduced
-- by opened positive foralls) and every quantified atom the sequent already
-- mentions. The quantified candidates give a guarded form of impredicative
-- instantiation: a binder may be solved with a polytype, but only one that
-- already occurs in the query or its premises.
--
-- Each axiom is a self-contained semantic truth about Haskell types, so
-- adding them can only enlarge the set of provable goals; they never
-- threaten negative evidence. Boundedness loses completeness only, and the
-- polarized translation already reports incompleteness whenever a
-- hypothesis-side forall exists, so exhausting a search that includes these
-- axioms still yields no refutation.
module Djinn.Internal.Instantiation
    ( InstantiationAxioms
    , instantiationAxioms
    , instantiationAxiomPremises
    , instantiationAxiomSymbols
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

-- Instantiating a leading chain replaces every binder at once, mirroring
-- Exference's provider rule. Longer chains multiply the candidate tuple space
-- without matching realistic queries, so they stay opaque.
maxInstantiationBinders :: Int
maxInstantiationBinders = 4

-- The original one- through three-binder rule enumerated a lexically sorted
-- Cartesian product. Keep that exact prefix so widening the rule cannot
-- perturb already documented candidates or rankings.
maxCartesianInstantiationBinders :: Int
maxCartesianInstantiationBinders = 3

-- Axioms are premises in every plan of the query, so their count bounds both
-- formula size and search branching. The caps are deliberate completeness
-- boundaries, never soundness ones: a dropped axiom can only cause a miss,
-- and misses in this fragment already report 'NoEvidence'.
maxInstantiationAxioms :: Int
maxInstantiationAxioms = 64

maxInstantiationAttempts :: Int
maxInstantiationAttempts = 512

-- One scheme with a large tuple space must not starve its siblings under the
-- global cap; each scheme receives its own smaller allowance as well.
maxInstantiationAxiomsPerScheme :: Int
maxInstantiationAxiomsPerScheme = 16

-- | The generated axiom premises together with their reserved proof symbols.
-- The symbols use Djinn's private @$@ namespace, so they cannot collide with
-- declared functions, and they must be erased from checked proofs by
-- 'eliminateInstantiationEvidence' before code generation.
data InstantiationAxioms = InstantiationAxioms
    { instantiationAxiomPremises :: [(Symbol, Formula)]
    , instantiationAxiomSymbols :: Set.Set Symbol
    }

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
    -> [String]
    -> [Formula]
    -> [Formula]
    -> InstantiationAxioms
instantiationAxioms translator variableSpellings goalFormulas
        premiseFormulas =
    InstantiationAxioms premises $ Set.fromList $ map fst premises
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
    -- new four-binder heuristics so alpha-renaming cannot reshuffle useful
    -- source-ordered runs merely because their spellings sort differently.
    wideCandidates = sourceVariableCandidates ++ quantifiedCandidates
    initialSchemes = distinctOn schemeKey
        [ scheme
        | (HypothesisSide, symbol) <- atoms
        , Just source <- [opaqueSymbolSource symbol]
        , Just scheme <- [instantiationScheme source]
        ]
    premises =
        [ (Symbol $ "$djinn$instantiation$" ++ show index, formula)
        | (index, formula) <- zip [0 :: Int ..] $
            buildAxiomFormulas translator historicalCandidates wideCandidates
                initialSchemes
        ]

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
-- end of the queue, and every scheme owns a private allowance so one large
-- tuple space cannot starve a sibling hypothesis under the global cap.
data InstantiationJob
    = SchemeJob InstantiationScheme
    | AxiomJob InstantiationScheme Int [[SharedType.Type String]]

buildAxiomFormulas
    :: (SharedType.Type String -> Either String Formula)
    -> [SharedType.Type String]
    -> [SharedType.Type String]
    -> [InstantiationScheme]
    -> [Formula]
buildAxiomFormulas translator historicalCandidates wideCandidates
        initialSchemes = loop
    (Set.fromList $ map schemeKey initialSchemes)
    Set.empty
    maxInstantiationAttempts
    maxInstantiationAxioms
    (map SchemeJob initialSchemes)
  where
    loop seenSchemes seenAxioms attempts allowance queue = case queue of
        _ | attempts <= 0 || allowance <= 0 -> []
        [] -> []
        SchemeJob scheme : jobs -> loop seenSchemes seenAxioms attempts
            allowance $
                AxiomJob scheme maxInstantiationAxiomsPerScheme
                    (candidateTuples $ length $ schemeBinders scheme)
                : jobs
        AxiomJob _ _ [] : jobs ->
            loop seenSchemes seenAxioms attempts allowance jobs
        AxiomJob scheme schemeAllowance (arguments : tuples) : jobs
            | schemeAllowance <= 0 ->
                loop seenSchemes seenAxioms attempts allowance jobs
            | otherwise -> case schemeAxiom scheme arguments of
                Just axiom
                    | axiom `Set.notMember` seenAxioms ->
                        let discovered = distinctOn schemeKey
                                [ found
                                | found <- discoveredSchemes axiom
                                , schemeKey found
                                    `Set.notMember` seenSchemes
                                ]
                        in axiom : loop
                            (foldr (Set.insert . schemeKey)
                                seenSchemes discovered)
                            (Set.insert axiom seenAxioms)
                            (attempts - 1)
                            (allowance - 1)
                            (AxiomJob scheme (schemeAllowance - 1) tuples
                                : jobs ++ map SchemeJob discovered)
                _ -> loop seenSchemes seenAxioms (attempts - 1) allowance
                    (AxiomJob scheme schemeAllowance tuples : jobs)

    -- One- through three-binder schemes retain their exact historical lexical
    -- Cartesian order. Four-binder schemes need a useful bounded prefix: draw
    -- fairly from source-order windows, repeated arguments, sparse monotone
    -- selections, and the complete Cartesian tail. Edge-first ordering keeps
    -- a useful late goal variable from being hidden by unrelated source
    -- variables, while round-robin merging prevents any one family from
    -- consuming the full per-scheme allowance.
    candidateTuples arity
        | arity <= maxCartesianInstantiationBinders =
            sequence $ replicate arity historicalCandidates
        | otherwise = distinctOn id $ roundRobin
            [ edgeFirst $ candidateWindows arity wideCandidates
            , map (replicate arity) $ edgeFirst wideCandidates
            , roundRobin
                [ orderedSelections arity wideCandidates
                , orderedSelections arity $ reverse wideCandidates
                ]
            , sequence $ replicate arity wideCandidates
            ]

    candidateWindows arity values
        | arity <= 0 = [[]]
        | otherwise = case splitAt arity values of
            (window, _)
                | length window == arity ->
                    window : candidateWindows arity (drop 1 values)
            _ -> []

    orderedSelections 0 _ = [[]]
    orderedSelections _ [] = []
    orderedSelections count (value : values) =
        map (value :)
            (orderedSelections (count - 1) values) ++
        orderedSelections count values

    edgeFirst values = distinctOn id $ roundRobin [values, reverse values]

    roundRobin streams = case
            [ (value, remaining)
            | value : remaining <- streams
            ] of
        [] -> []
        active -> map fst active ++ roundRobin (map snd active)

    schemeAxiom scheme arguments = do
        instantiated <- rightToMaybe $
            instantiateSchemeBody scheme arguments
        bodyFormula <- rightToMaybe $ translator instantiated
        let hypothesis = PVar $ opaqueTypeSymbol $ schemeSource scheme
        if bodyFormula == hypothesis
            then Nothing
            else Just $ hypothesis :-> bodyFormula

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

-- | Erase instantiation-axiom evidence from a checked proof before code
-- generation. Semantically every axiom is the identity function: an applied
-- occurrence reduces to its argument, so the generated Haskell uses the
-- polymorphic hypothesis directly and GHC re-instantiates it at the required
-- type; a bare occurrence becomes an explicit identity lambda, which GHC
-- checks against the implication's rank-N domain bidirectionally. LJT
-- allocates binders away from every environment symbol, so an axiom symbol
-- can never be shadowed inside a proof term.
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
