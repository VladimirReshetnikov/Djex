{-# LANGUAGE RankNTypes #-}
--
-- Copyright (c) 2005, 2008 Lennart Augustsson
-- See LICENSE for licensing details.
--
-- Intuitionistic theorem prover
-- Written by Roy Dyckhoff, Summer 1991
-- Modified to use the LWB syntax  Summer 1997
-- and simplified in various ways...
--
-- Translated to Haskell by Lennart Augustsson December 2005
--
-- Incorporates the Vorob'ev-Hudelmaier etc calculus (I call it LJT)
-- See RD's paper in JSL 1992:
-- "Contraction-free calculi for intuitionistic logic"
--
-- Torkel Franzen (at SICS) gave me good ideas about how to write this
-- properly, taking account of first-argument indexing,
-- and I learnt a trick or two from Neil Tennant's "Autologic" book.

-- | The intuitionistic propositional prover at the heart of Djinn: proof
-- search in Dyckhoff's contraction-free LJT calculus, extended to produce
-- proof terms ('Proof' is a "Djinn.Internal.LJTFormula" 'Term').  A
-- t'SearchMode' selects the branch strategy, alternative retention, and an
-- optional choice-point budget. Historical LJT is a decision procedure for
-- its propositional fragment. Explicit interleaved term alternatives may
-- continue enumerating normal terms without a finite bound, even after the
-- original formula is proved. 'proveWithModeChecked' first validates the
-- assumption identities.
module Djinn.Internal.LJT (
    module Djinn.Internal.LJTFormula, provable, prove, Proof,
    SearchMode(..), Strategy(..), SearchOutcome(..),
    defaultSearchMode, proveWithMode, proveWithModeChecked,
    proveFirstWithModeChecked,
    ProofSearchCursor, ProofSearchObservation(..),
    startProofSearchChecked, startProofSearchWithNormalPriorityChecked, observeProofSearch
    ) where

import Control.Applicative (Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(mzero, mplus), ap, foldM)
import Data.List ((!?), sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Fresh (allocateFresh)
import qualified Language.Haskell.Synthesis.CandidateQuality as Quality
import qualified Language.Haskell.Synthesis.Generated as Generated
import qualified Language.Haskell.Synthesis.Name as Name
import Djinn.Internal.LJTFormula
import Djinn.Internal.ProofCheck (checkProofEnvironment)

-- Whether local proof-search cuts should retain their alternative paths.
type MoreSolutions = Bool

-- | How alternative branches are explored at each choice point.  DepthFirst
-- is the classical order (fully explore the first branch before the second),
-- except for the bounded local rotation of the three oldest proofs while
-- enumerating retained alternatives for an exact atomic @A -> A -> A@ suffix.
-- Interleave rotates ordinary choice branches at each choice point. The
-- optional LJT-tail/normal-term merge instead preempts on a proof or 64
-- observed choices; the source query's optional formula-plan scheduler uses
-- the same turn rule. Every observed choice retains its original charge.
data Strategy = DepthFirst | Interleave
    deriving (Eq, Show)

-- | A named description of one proof search.
data SearchMode = SearchMode {
    -- Retain alternative proofs at local search cuts (multiple solutions).
    searchAlternatives :: Bool,
    -- Together with searchAlternatives and Interleave, enumerate checked
    -- normal terms after the exact historical first-proof prefix. This is
    -- disabled in default raw modes; source queries enable it only through
    -- explicit optionAlternatives, not through sorting alone. A proved finite
    -- maximum ends the extra size ladder; unresolved cycles remain unbounded.
    searchTermAlternatives :: Bool,
    searchStrategy :: Strategy,
    -- Maximum number of choice points to explore; Nothing is unlimited.
    -- With a limit the search is no longer a decision procedure: an empty
    -- result with searchExhausted set means "not found", not "unprovable".
    searchBudget :: Maybe Integer,
    -- Ranking only permutes finite alternatives; every choice remains charged.
    searchRanking :: Quality.CandidateRankingPolicy,
    searchProviderNames :: Map.Map Symbol Name.Name,
    searchProviderCosts :: Map.Map Name.Name Natural
    }
    deriving (Show)

-- | The classical search: depth-first apart from the documented exact binary-
-- endomorphism exception, unbudgeted, and complete.
defaultSearchMode :: MoreSolutions -> SearchMode
defaultSearchMode more = SearchMode {
    searchAlternatives = more,
    searchTermAlternatives = False,
    searchStrategy = DepthFirst,
    searchBudget = Nothing,
    searchRanking = Quality.LegacyCandidateRanking,
    searchProviderNames = Map.empty,
    searchProviderCosts = Map.empty
    }

-- | The result of one mode-aware search: the lazily produced proof terms
-- (whose free variables are the assumption names), whether the choice-point
-- budget ran out with unexplored space left, and the budget remaining.  An
-- empty proof list means "unprovable" only when 'searchExhausted' is
-- 'False'.
data SearchOutcome = SearchOutcome {
    searchProofs :: [Proof],
    -- True when the budget ran out with unexplored search space left.
    searchExhausted :: Bool,
    -- Fuel left after the explored prefix.  A caller that performs a
    -- follow-up search can pass this remainder on without silently turning
    -- one query budget into two.  For an unbounded search it stays 'Nothing'.
    remainingSearchBudget :: Maybe Integer
    }

-- | Whether a formula is intuitionistically provable from no assumptions,
-- by an unbudgeted depth-first search (so this is a decision procedure).
provable :: Formula -> Bool
provable = not . null . prove False []

-- | Historical unchecked proof search.  Duplicate assumption identities are
-- resolved by association-list order and make the resulting free proof
-- variables ambiguous.  New callers that accept an environment should use
-- 'proveWithModeChecked'; this compatibility entry remains available to code
-- that already owns the identity invariant.
prove :: MoreSolutions -> [(Symbol, Formula)] -> Formula -> [Proof]
prove more env = searchProofs . proveWithMode (defaultSearchMode more) env

-- | Historical mode-aware search without boundary validation.  Prefer
-- 'proveWithModeChecked' unless the caller has already assigned unique proof
-- identities.
proveWithMode :: SearchMode -> [(Symbol, Formula)] -> Formula -> SearchOutcome
proveWithMode = proveWithModeBy id

-- Apply the result policy inside the choice-counted computation. In
-- particular a first-result policy must not obtain its remaining fuel by
-- traversing an already-produced list's unobserved proof tail.
proveWithModeBy
    :: (P Proof -> P Proof)
    -> SearchMode -> [(Symbol, Formula)] -> Formula -> SearchOutcome
proveWithModeBy policy mode env goal =
    SearchOutcome proofs exhausted remaining
  where
    (proofs, exhausted, remaining) =
        runBounded (searchBudget mode) (searchStrategy mode) reservedSymbols $
            policy $ proofSearchComputation mode env goal
    -- Symbol is shared by proof variables and propositional atoms.  Reserving
    -- both namespaces prevents generated binders from capturing environment
    -- variables and keeps the atom introduced for disjunction genuinely fresh.
    reservedSymbols =
        map fst env ++ concatMap (formulaSymbols . snd) env ++ formulaSymbols goal

-- | Search after checking that every external assumption has a unique proof
-- identity.  This is the canonical raw LJT entry: it uses the same validator
-- and diagnostic as the independent proof checker.  The returned
-- proof stream remains lazy once the finite environment boundary is accepted.
proveWithModeChecked
    :: SearchMode
    -> [(Symbol, Formula)]
    -> Formula
    -> Either String SearchOutcome
proveWithModeChecked mode environment goal = do
    checkProofEnvironment environment
    return $ proveWithMode mode environment goal

-- | Search just through the first proof, preserving the exact choice budget
-- left at that prefix. No later proof or choice is inspected. The ordinary
-- prover's enumeration policy is unchanged; this entrance supports a charged
-- first-result stage followed by a different bounded search family.
proveFirstWithModeChecked
    :: SearchMode -> [(Symbol, Formula)] -> Formula
    -> Either String SearchOutcome
proveFirstWithModeChecked mode environment goal = do
    checkProofEnvironment environment
    return $ proveWithModeBy atMostOne mode environment goal

-- | A resumable, unconsumed search stream. The cursor owns the branch-local
-- freshness state; resuming it never starts the proof search again. Its
-- caller must charge every 'ProofSearchChoice' against its shared query
-- budget and every 'ProofSearchResult' against its raw candidate allowance.
-- The mode's per-search budget is deliberately not applied a second time.
newtype ProofSearchCursor = ProofSearchCursor (Steps (PS, Proof))

data ProofSearchObservation
    = ProofSearchFinished
    | ProofSearchChoice ProofSearchCursor
    | ProofSearchResult Proof ProofSearchCursor

-- | Validate a plan and retain its lazy continuation, without observing a
-- proof or choice. This supports fair outer-plan scheduling with one budget.
startProofSearchChecked
    :: SearchMode -> [(Symbol, Formula)] -> Formula
    -> Either String ProofSearchCursor
startProofSearchChecked = startProofSearchCheckedBy proofSearchComputation

-- | A streaming-only scheduling preference for an already selected exact
-- instantiation context. Keep the historical first proof, then spend more of
-- each finite turn on increasing-size normal forms than on the unrestricted
-- LJT tail. Both continuations and every charged choice remain present.
startProofSearchWithNormalPriorityChecked
    :: SearchMode -> [(Symbol, Formula)] -> Formula
    -> Either String ProofSearchCursor
startProofSearchWithNormalPriorityChecked = startProofSearchCheckedBy $
    proofSearchComputationBy $ interleaveProofWorkWeighted 64 4096

startProofSearchCheckedBy
    :: (SearchMode -> [(Symbol, Formula)] -> Formula -> P Proof)
    -> SearchMode -> [(Symbol, Formula)] -> Formula
    -> Either String ProofSearchCursor
startProofSearchCheckedBy computation mode environment goal = do
    checkProofEnvironment environment
    return $ ProofSearchCursor $ reify (searchStrategy mode)
        (startPS reservedSymbols) $
        computation mode environment goal
  where
    reservedSymbols = map fst environment ++
        concatMap (formulaSymbols . snd) environment ++ formulaSymbols goal

-- | Observe exactly one stream node. Neither a result's tail nor a choice's
-- continuation is forced here, so a caller can stop at either exact bound.
observeProofSearch :: ProofSearchCursor -> ProofSearchObservation
observeProofSearch (ProofSearchCursor stream) = case stream of
    Done -> ProofSearchFinished
    Step rest -> ProofSearchChoice $ ProofSearchCursor rest
    Yield (_, proof) rest -> ProofSearchResult proof $ ProofSearchCursor rest

-- Preserve the exact historical first-proof prefix. Only an explicit
-- interleaved term-alternative request can then add normal forms, alongside
-- the unconsumed LJT tail. Support detection and the additional stream stay
-- behind that first Yield, so a first-result cut never forces them and an
-- unsuccessful LJT search keeps its original negative evidence and budget.
proofSearchComputation
    :: SearchMode -> [(Symbol, Formula)] -> Formula -> P Proof
proofSearchComputation = proofSearchComputationBy $ interleaveProofWork 64

proofSearchComputationBy
    :: (Steps (PS, Proof) -> Steps (PS, Proof) -> Steps (PS, Proof))
    -> SearchMode -> [(Symbol, Formula)] -> Formula -> P Proof
proofSearchComputationBy schedule mode environment goal
    | searchAlternatives mode && searchTermAlternatives mode &&
        searchStrategy mode == Interleave = P $ \strategy state sk fk ->
            let normalTail
                    | all (normalFormula . snd) environment && normalFormula goal =
                        reify strategy state $ chargeNormalAttempt $
                            normalProofSearch environment goal
                    | otherwise = Done
                afterFirst Done = Done
                afterFirst (Step rest) = Step (afterFirst rest)
                afterFirst (Yield result rest) = Yield result (schedule rest normalTail)
            in replay sk fk $ afterFirst $ reify strategy state original
    | otherwise = original
  where
    original = redtop mode (searchAlternatives mode) environment goal

-- This additional grammar uses only the exact existing atomic identities and
-- arrows. Structural sums/products and their eliminators remain with LJT.
normalFormula :: Formula -> Bool
normalFormula (PVar _) = True
normalFormula (argument :-> result) = normalFormula argument && normalFormula result
normalFormula _ = False

-- Index every exact residual of an assumption's arrow spine. A residual may
-- itself be an arrow, retaining forwarding and partial application. Each head
-- occurs once per residual; the lists retain association-list encounter order.
-- The maps are immutable: introducing a lambda prepends only its fresh head
-- and shares all unrelated residual entries with the enclosing context.
data NormalContext = NormalContext
    { normalCompatibleHeads :: Map.Map Formula [(Symbol, [Formula])]
    , normalMinimumHeadCosts :: Map.Map Formula Integer
    }

normalContext :: [(Symbol, Formula)] -> NormalContext
normalContext = foldr (uncurry extendNormalContext) $
    NormalContext Map.empty Map.empty

extendNormalContext :: Symbol -> Formula -> NormalContext -> NormalContext
extendNormalContext name source context = NormalContext
    (insertHead [] source $ normalCompatibleHeads context)
    (extendNormalMinimumCosts source $ normalMinimumHeadCosts context)
  where
    insertHead reversedArguments residual heads =
        let extended = Map.insertWith (++) residual
                [(name, reverse reversedArguments)] heads
        in case residual of
            argument :-> result -> insertHead (argument : reversedArguments) result extended
            _ -> extended

-- One neutral head plus at least one head use per supplied argument. This
-- projection needs no proof identity, so lower-bound lambda exploration can
-- extend it without allocating or inventing a term binder.
extendNormalMinimumCosts :: Formula -> Map.Map Formula Integer -> Map.Map Formula Integer
extendNormalMinimumCosts = insertCost 1
  where
    insertCost cost residual costs =
        let extended = Map.insertWith min residual cost costs
        in case residual of
            _ :-> result -> insertCost (cost + 1) result extended
            _ -> extended

-- Size counts neutral head uses, including repeated uses of one assumption.
-- Lambda introduction is free; existing neutral functions can also be
-- forwarded or partially applied. Each applied head costs one, so
-- every argument has a strictly smaller positive size. Finite input formulae
-- bound consecutive lambda introductions. Increasing layers share the outer
-- cursor's budget; even advancing to another empty layer is a charged Step.
-- The context is shared across size layers. Its initial thunk is first
-- demanded inside normalProofAtSize, after the normal lane's charged Step.
-- A finite maximum is proved over exact type-set/goal states. Multiplicity
-- changes the number of terms, but not their attainable head-use sizes. Only
-- actual assumption types enter the state: indexed arrow residuals are not
-- assumptions. A live cycle is Unknown, never an invented finite cut-off.
data NormalMaximum = NormalImpossible | NormalFinite Integer | NormalUnknown
    deriving (Eq, Show)

type NormalBoundState = (Set.Set Formula, Formula)
type NormalBoundMemo = Map.Map NormalBoundState NormalMaximum

normalProofSearch :: [(Symbol, Formula)] -> Formula -> P Proof
normalProofSearch environment goal = do
    maximumSize <- normalMaximumSize (Set.fromList $ map snd environment) goal
    normalProofLayers maximumSize (normalContext environment) goal 1

-- Every recursive state, candidate head, arrow-spine link, and argument is
-- behind a charged Step. This deterministic analysis shares the normal
-- lane's existing cursor fuel; it has no private allowance or depth cap.
-- It runs only behind the historical first proof and the lane's first Step.
-- Unknown cycles return promptly and keep the old unbounded size ladder.
normalMaximumSize :: Set.Set Formula -> Formula -> P NormalMaximum
normalMaximumSize assumptions goal =
    fst <$> inspect Set.empty Map.empty assumptions goal
  where
    inspect :: Set.Set NormalBoundState -> NormalBoundMemo
        -> Set.Set Formula -> Formula -> P (NormalMaximum, NormalBoundMemo)
    inspect active memo available target = chargeNormalAttempt $
        case Map.lookup key memo of
            Just known -> return (known, memo)
            Nothing
                | Set.member key active -> return (NormalUnknown, memo)
                | otherwise -> do
                    let active' = Set.insert key active
                    (lambdaMaximum, memo') <- case target of
                        argument :-> result ->
                            inspect active' memo (Set.insert argument available) result
                        _ -> return (NormalImpossible, memo)
                    (maximumSize, memo'') <- heads active' available target
                        lambdaMaximum memo' (Set.toList available)
                    return (maximumSize, Map.insert key maximumSize memo'')
      where
        key = (available, target)

    -- Unknown is absorbing for alternatives. It is NOT absorbing for the
    -- arguments of one head: a later Impossible argument kills that head.
    heads _ _ _ NormalUnknown memo _ = return (NormalUnknown, memo)
    heads _ _ _ accumulated memo [] = return (accumulated, memo)
    heads active available target accumulated memo (source : sources) =
        chargeNormalAttempt $ do
            matched <- matchingArguments target [] source
            case matched of
                Nothing -> heads active available target accumulated memo sources
                Just arguments -> do
                    (headMaximum, memo') <- argumentsMaximum active available
                        (NormalFinite 1) memo arguments
                    heads active available target
                        (alternativeMaximum accumulated headMaximum) memo' sources

    matchingArguments target reversedArguments source = chargeNormalAttempt $
        if source == target then return $ Just $ reverse reversedArguments
        else case source of
            argument :-> result ->
                matchingArguments target (argument : reversedArguments) result
            _ -> return Nothing

    argumentsMaximum _ _ NormalImpossible memo _ = return (NormalImpossible, memo)
    argumentsMaximum _ _ accumulated memo [] = return (accumulated, memo)
    argumentsMaximum active available accumulated memo (argument : arguments) =
        chargeNormalAttempt $ do
            (argumentMaximum, memo') <- inspect active memo available argument
            argumentsMaximum active available
                (productMaximum accumulated argumentMaximum) memo' arguments

    alternativeMaximum NormalUnknown _ = NormalUnknown
    alternativeMaximum _ NormalUnknown = NormalUnknown
    alternativeMaximum NormalImpossible other = other
    alternativeMaximum other NormalImpossible = other
    alternativeMaximum (NormalFinite left) (NormalFinite right) =
        NormalFinite $ max left right

    productMaximum NormalImpossible _ = NormalImpossible
    productMaximum _ NormalImpossible = NormalImpossible
    productMaximum NormalUnknown _ = NormalUnknown
    productMaximum _ NormalUnknown = NormalUnknown
    productMaximum (NormalFinite left) (NormalFinite right) = NormalFinite $ left + right

normalProofLayers :: NormalMaximum -> NormalContext -> Formula -> Integer -> P Proof
normalProofLayers NormalImpossible _ _ _ = mzero
normalProofLayers maximumSize context goal size = P $ \strategy state sk fk ->
    let continue = case maximumSize of
            NormalFinite limit | size >= limit -> fk
            _ -> Step $ unP (normalProofLayers maximumSize context goal (size + 1))
                    strategy state sk fk
    in unP (normalProofAtSize context goal size) strategy state sk continue

normalProofAtSize :: NormalContext -> Formula -> Integer -> P Proof
normalProofAtSize context goal size = chargeNormalAttempt $
    case normalLowerBound context goal of
        Nothing -> mzero
        Just required | size < required -> mzero
        _ -> case goal of
            argument :-> result -> normalChoices
                [ neutral
                , do
                    binder <- newSym "n"
                    Lam binder <$> normalProofAtSize (extendNormalContext binder argument context) result size
                ]
            PVar _ -> neutral
            _ -> mzero
  where
    neutral = normalChoices
        [applyHead name arguments
        | (name, arguments) <- Map.findWithDefault [] goal $ normalCompatibleHeads context]
    applyHead name arguments = case traverse (normalLowerBound context) arguments of
        Nothing -> mzero
        Just minima -> bindInterleaved (normalSizePartitions minima (size - 1)) $ \argumentSizes -> do
            arguments' <- normalArgumentProduct arguments argumentSizes
            return $ applys (Var name) arguments'
    normalArgumentProduct [] [] = return []
    normalArgumentProduct (argument : arguments) (argumentSize : sizes) =
        bindInterleaved (normalProofAtSize context argument argumentSize) $ \proof ->
            (proof :) <$> normalArgumentProduct arguments sizes
    normalArgumentProduct _ _ = mzero

chargeNormalAttempt :: P a -> P a
chargeNormalAttempt attempt = P $ \strategy state sk fk ->
    Step $ unP attempt strategy state sk fk

-- An admissible cost bound only: required lambdas introduce their domains
-- before head lookup, and every argument of a matching head needs at least
-- one head use. No type- or syntax-specific construction rule is involved.
-- The stored minimum is exactly the previous scan's neutral minimum; this
-- lookup does not instantiate, unify, or identify merely similar formulae.
normalLowerBound :: NormalContext -> Formula -> Maybe Integer
normalLowerBound context = lowerBound $ normalMinimumHeadCosts context
  where
    lowerBound costs goal = case (Map.lookup goal costs, lambdaBound costs goal) of
        (Nothing, other) -> other
        (other, Nothing) -> other
        (Just neutralCost, Just lambdaCost) -> Just $ min neutralCost lambdaCost
    lambdaBound costs (argument :-> result) =
        lowerBound (extendNormalMinimumCosts argument costs) result
    lambdaBound _ _ = Nothing

normalSizePartitions :: [Integer] -> Integer -> P [Integer]
normalSizePartitions [] size = chargeNormalAttempt $
    if size == 0 then return [] else mzero
normalSizePartitions (minimumHere : minima) size = chargeNormalAttempt $
    normalChoices
        [(part :) <$> normalSizePartitions minima (size - part)
        | part <- [minimumHere .. size - sum minima]]

-- Every attempted head/partition/intro branch is charged, even on failure.
-- Finite choices are visited round-robin without building their products.
normalChoices :: [P a] -> P a
normalChoices branches = P $ \strategy state sk fk ->
    replay sk fk $ roundRobinSteps $ map (Step . reify strategy state) branches

-- Fold the environment into the goal as premises, prove the resulting
-- implication, then apply the proof to the environment variables and
-- normalize, leaving a term whose free variables are the assumption names.
redtop :: SearchMode -> MoreSolutions -> [(Symbol, Formula)] -> Formula -> P Proof
redtop mode more env goal
    | Quality.LegacyCandidateRanking <- searchRanking mode = do
        let form = foldr (:->) goal (map snd env)
        p <- redant mode more [] Map.empty [] Map.empty form
        nf (applys p (map (Var . fst) env))
    | otherwise = do
        -- Keep source assumption identities while choosing proofs so provider
        -- ratings cannot be lost behind the temporary outer lambda prefix.
        -- The independent proof checker sees exactly this same environment.
        p <- redant mode more [A (Var name) formula | (name, formula) <- env]
            Map.empty [] Map.empty goal
        nf p

------------------------------
-----
-- | A proof is a lambda term of the proof calculus; the search returns
-- normalized terms whose free variables are the assumption symbols.
type Proof = Term

-- The proof search gives every binder a globally fresh symbol (including with
-- respect to caller-supplied symbols).  Substitution can therefore stay small:
-- only ordinary shadowing needs an explicit check.  A replacement is copied
-- through the shared binder-freshening traversal so that its binders remain
-- globally unique at every occurrence.
subst :: Term -> Symbol -> Term -> P Term
subst replacement variable = substitute
  where
    substitute t@(Var s)
        | variable == s = freshenTermBinders (newSym "c") replacement
        | otherwise = return t
    substitute t@(Lam s body)
        | variable == s = return t
        | otherwise = Lam s <$> substitute body
    substitute (Apply f a) = Apply <$> substitute f <*> substitute a
    substitute (Xsel i n e) = Xsel i n <$> substitute e
    substitute t = return t

------------------------------

-- These helpers use readable local binder names.  They enter a proof only via
-- 'subst', whose copying step replaces those binders with fresh symbols.

curryTuple :: Int -> Term -> Term
curryTuple n p = foldr Lam (Apply p (applys (Ctuple n) (map Var xs))) xs
  where
    xs = [Symbol ("x_" ++ show i) | i <- [0 .. n - 1]]

inj :: ConsDesc -> Int -> Term -> Term
inj cd i p = Lam x $ Apply p (Apply (Cinj cd i) (Var x))
  where x = Symbol "x"

-- From p : (c->d)->b and q : (d->b)->(c->d), derive b.
applyImp :: Term -> Term -> Term
applyImp p q = Apply p (Apply q (Lam y $ Apply p (Lam x (Var y))))
  where x = Symbol "x"
        y = Symbol "y"

-- ((c->d)->false) -> ((c->false)->false, d->false)
-- p : (c->d)->false
-- replace p1 and p2 with the components of the pair
cImpDImpFalse :: Symbol -> Symbol -> Term -> Term -> P Term
cImpDImpFalse p1 p2 cdf gp = do
    let p1b = Lam cf $ Apply cdf $ Lam x $ Apply (Ccases []) $ Apply (Var cf) (Var x)
        p2b = Lam d $ Apply cdf $ Lam c $ Var d
        cf = Symbol "cf"
        x = Symbol "x"
        d = Symbol "d"
        c = Symbol "c"
    subst p1b p1 gp >>= subst p2b p2

------------------------------

-- Further possible simplifications:
--   * Remove a split when none of its variables are used.
--   * Merge case alternatives with equal right-hand sides.

-- Compute the normal form
nf :: Term -> P Term
nf term = spine term []
  where
    spine (Apply f a) args = do
        a' <- nf a
        spine f (a' : args)
    spine (Lam s body) [] = Lam s <$> nf body
    spine (Lam s body) (a : args) = do
        body' <- subst a s body
        spine body' args
    spine (Csplit n) (branch : tuple : args)
        | isTuple && tupleArity == n && n <= length tupleArgs =
            spine (applys branch tupleArgs) args
      where
        (isTuple, tupleArity, tupleArgs) = viewTuple [] tuple
        viewTuple acc (Apply f a) = viewTuple (a : acc) f
        viewTuple acc (Ctuple arity) = (True, arity, acc)
        viewTuple _ _ = (False, 0, [])
    spine (Ccases []) (e@(Apply (Ccases []) _) : args) = spine e args
    spine cases@(Ccases constructors) (injected@(Apply (Cinj constructor i) x) : args)
        | length args >= branchCount =
            case (constructors !? i, args !? i) of
            (Just expected, Just branch) | constructor == expected ->
                spine (Apply branch x) (drop branchCount args)
            _ -> return $ applys cases (injected : args)
      where
        branchCount = length constructors
    spine f args = return $ applys f args


------------------------------
----- Our Proof monad, P: backtracking with per-branch state, delivered
----- through success/failure continuations that emit a lazy stream of
----- results punctuated by explicit choice-point markers.

-- A result stream.  Step marks one explored choice point, so consuming the
-- stream under a budget bounds the amount of search performed, and a fair
-- strategy can alternate branches at Step granularity.  With Steps ignored
-- the stream is exactly the classical lazy result list.
data Steps a
    = Done
    | Yield a (Steps a)
    | Step (Steps a)

-- Fair merge: swap branches at every choice point, so results from the
-- second branch surface even while the first is still searching.
interleaveS :: Steps a -> Steps a -> Steps a
interleaveS Done ys = ys
interleaveS (Yield x xs) ys = Yield x (interleaveS ys xs)
interleaveS (Step xs) ys = Step (interleaveS ys xs)

-- Balance a proof-producing stream with a stream that needs several failed
-- expansions before its next proof. A turn ends at one Yield or a positive
-- quantum of Steps. Each Step is emitted lazily and unchanged: reaching a
-- caller's exact budget never evaluates the rest of the turn. A quantum of
-- one is precisely interleaveS. This is a scheduling quantum, not a proof,
-- size, or total-work cap; neither stream is restarted or granted new fuel.
interleaveProofWork :: Int -> Steps a -> Steps a -> Steps a
interleaveProofWork quantum = interleaveProofWorkWeighted quantum quantum

-- Every yielded proof hands over immediately; an unproductive turn hands
-- over after its own finite choice quantum. Distinct quanta change effort
-- allocation only, never the number of observed choices or retained proofs.
interleaveProofWorkWeighted :: Int -> Int -> Steps a -> Steps a -> Steps a
interleaveProofWorkWeighted leftQuantum rightQuantum =
    advance leftSize leftSize rightSize
  where
    leftSize = max 1 leftQuantum
    rightSize = max 1 rightQuantum
    advance _ _ _ Done right = right
    advance _ ownSize nextSize (Yield result rest) right =
        Yield result (advance nextSize nextSize ownSize right rest)
    advance remaining ownSize nextSize (Step rest) right = Step $
        if remaining <= 1
            then advance nextSize nextSize ownSize right rest
            else advance (remaining - 1) ownSize nextSize rest right

-- The success continuation receives the value's final state and the rest
-- of the stream (all remaining alternatives) as an already-built tail.
type Success r a = PS -> a -> Steps r -> Steps r

-- The continuation encoding (a LogicT-style two-continuation monad) makes
-- bind constant-time and builds each stream node exactly once.  A direct
-- Steps-returning implementation was measured first and rejected: failed
-- branches leave Step-node chains that nested appends re-traverse, which
-- made refutation-heavy searches several times slower on the benchmark
-- corpus.  Every alternative restarts from the state of its choice point,
-- which is what makes backtracking cheap.
newtype P a = P {
    unP :: forall r. Strategy -> PS -> Success r a -> Steps r -> Steps r
    }

instance Functor P where
    fmap f (P m) = P $ \ strat s sk fk ->
        m strat s (\ s' x rest -> sk s' (f x) rest) fk

instance Applicative P where
    pure x = P $ \ _ s sk fk -> sk s x fk
    (<*>) = ap

instance Monad P where
    return = pure
    P m >>= f = P $ \ strat s sk fk ->
        m strat s (\ s' x rest -> unP (f x) strat s' sk rest) fk

instance Alternative P where
    empty = mzero
    (<|>) = mplus

instance MonadPlus P where
    mzero = P $ \ _ _ _ fk -> fk
    P m `mplus` P n = P $ \ strat s sk fk ->
        case strat of
            DepthFirst -> m strat s sk (Step (n strat s sk fk))
            -- Fairness needs both branch streams reified before merging;
            -- laziness ensures only the explored prefixes are built.
            Interleave ->
                replay sk fk $
                    interleaveS (reify strat s (P m))
                                (Step (reify strat s (P n)))

-- Reify a computation to its result stream.
reify :: Strategy -> PS -> P a -> Steps (PS, a)
reify strat s (P m) = m strat s (\ s' x rest -> Yield (s', x) rest) Done

-- Feed a reified stream back through continuation form.
replay :: Success r a -> Steps r -> Steps (PS, a) -> Steps r
replay sk fk = go
  where
    go Done = fk
    go (Yield (s', x) rest) = sk s' x (go rest)
    go (Step rest) = Step (go rest)

-- Fairly combine an argument stream with its dependent proof searches.
-- Ordinary monadic bind deliberately retains the historical depth-first
-- continuation order. Here each argument keeps its own freshness state and
-- unconsumed tail, while the existing stream merge advances later arguments
-- beside an expensive earlier continuation. Every source/continuation Step
-- is retained exactly once; admitting an argument adds no synthetic choice.
bindInterleaved :: P a -> (a -> P b) -> P b
bindInterleaved source continue = P $ \strategy state sk fk ->
    let combine Done = Done
        combine (Step rest) = Step (combine rest)
        combine (Yield (branchState, argument) rest) =
            interleaveS (reify strategy branchState $ continue argument) (combine rest)
    in replay sk fk $ combine $ reify strategy state source

-- The state carries both the next suffix and every symbol already in use.
-- The initial used set contains caller-supplied term and formula symbols; each
-- generated symbol is then recorded here as well.
data PS = PS !Natural (Set.Set Symbol)

startPS :: [Symbol] -> PS
startPS = PS 1 . Set.fromList

choose :: [a] -> P a
choose values = P $ \ _ s sk fk ->
    let stream [] = fk
        stream [x] = sk s x fk
        stream (x : xs) = sk s x (Step (stream xs))
    in stream values

-- Explore the supplied computations round-robin even under the historical
-- depth-first strategy.  This is deliberately narrower than 'Interleave': it
-- is used only along a curried premise with adjacent repeated domains, so one
-- argument choice's descendant compositions cannot starve every sibling
-- choice.  The 'Step' before the remaining computations preserves the same
-- finite choice-point accounting as 'choose'.
interleaveChoices :: [P a] -> P a
interleaveChoices [] = mzero
interleaveChoices [choice] = choice
interleaveChoices (choice : choices) = P $ \ strat s sk fk ->
    replay sk fk $ roundRobinSteps
        (reify strat s choice : map (Step . reify strat s) choices)

-- Advance every live stream by one node per round. The reversed rear list
-- makes queue rotation amortized constant-time without favoring a right-
-- nested suffix when three or more proofs are available.
roundRobinSteps :: [Steps a] -> Steps a
roundRobinSteps initialStreams = advance initialStreams []
  where
    advance [] [] = Done
    advance [] rear = advance (reverse rear) []
    advance (Done : streams) rear = advance streams rear
    advance (Yield result rest : streams) rear =
        Yield result (advance streams (rest : rear))
    advance (Step rest : streams) rear =
        Step (advance streams (rest : rear))

-- A cost-only structural view of a partial proof. It is never emitted or
-- admitted as typing evidence: ordering preserves the original proof handles.
-- Logical eliminators retain an explicit Case cost, even before saturation.
orderProofs :: SearchMode -> [Term] -> [Term]
orderProofs mode = Quality.rankCandidatesByQuality (searchRanking mode)
    (\name -> Map.findWithDefault (Quality.defaultCandidateProviderCost name) name $ searchProviderCosts mode)
    (proofQualityExpression mode)

orderNestedProofs :: SearchMode -> [NestImp] -> [NestImp]
orderNestedProofs mode = Quality.rankCandidatesByQuality (searchRanking mode)
    (\name -> Map.findWithDefault (Quality.defaultCandidateProviderCost name) name $ searchProviderCosts mode)
    (\(NestImp proof _ _ _) -> proofQualityExpression mode proof)

-- An exact pending assumption can finish the proof before it enters an atom
-- index. Rank this finite work list as well, so an expensive direct provider
-- cannot bypass the same policy used by indexed choices. Stable scalar order
-- retains ties without repeatedly applying diversity penalties to one worklist.
orderAntecedents :: SearchMode -> Antecedents -> Antecedents
orderAntecedents mode = case searchRanking mode of
    Quality.LegacyCandidateRanking -> id
    ranking -> sortOn $ \(A proof _) -> Quality.candidateQualityCost ranking
        (\name -> Map.findWithDefault
            (Quality.defaultCandidateProviderCost name) name $ searchProviderCosts mode)
        $ proofQualityExpression mode proof

proofQualityExpression :: SearchMode -> Term -> Generated.Expression Symbol
proofQualityExpression mode = go
  where
    go (Var symbol) = case Map.lookup symbol $ searchProviderNames mode of
        Just name -> Generated.Global name
        Nothing -> Generated.Local symbol
    go (Lam binder body) = Generated.Lambda [Generated.Bind binder] $ go body
    go (Apply function argument) = Generated.Apply (go function) (go argument)
    go (Xsel _ _ value) = Generated.Case (go value)
        [(Generated.Wildcard, Generated.Hole $ Symbol "qualityHole")]
    go (Ccases constructors) = Generated.Case (Generated.Hole $ Symbol "qualityHole")
        [(Generated.Wildcard, Generated.Hole $ Symbol "qualityHole") | _ <- constructors]
    go (Csplit _) = Generated.Case (Generated.Hole $ Symbol "qualityHole")
        [(Generated.Wildcard, Generated.Hole $ Symbol "qualityHole")]
    go (Cinj (ConsDesc spelling _) _) = case Name.parseName spelling of
        Right name -> Generated.Global name
        Left _ -> Generated.Local $ Symbol spelling
    go (Ctuple _) = Generated.Tuple []

-- Cut a subsearch to its first result, preserving the choice points that
-- were explored to reach it so budgets stay honest.
atMostOne :: P a -> P a
atMostOne p = P $ \ strat s sk fk ->
    let cut Done = fk
        cut (Yield (s', x) _) = sk s' x fk
        cut (Step rest) = Step (cut rest)
    in cut (reify strat s p)

-- Run a proof search, exploring at most the given number of choice points.
-- The Bool reports whether the budget expired with search space remaining;
-- it is False whenever the search space was genuinely finished.  The final
-- component is the unspent fuel, for budget-honest follow-up work.
runBounded :: Maybe Integer -> Strategy -> [Symbol] -> P a
           -> ([a], Bool, Maybe Integer)
runBounded budget strat reserved p =
    consume (fmap (max 0) budget) (reify strat (startPS reserved) p)
  where
    consume b Done = ([], False, b)
    consume b (Yield (_, x) rest) =
        let (xs, exhausted, remaining) = consume b rest
        in (x : xs, exhausted, remaining)
    consume (Just remaining) (Step _) | remaining <= 0 =
        ([], True, Just 0)
    consume b (Step rest) = consume (fmap (subtract 1) b) rest


------------------------------
----- Proofs of atomic formulae, indexed by the formula symbol.

type AtomicProofs = Map.Map Symbol [Term]

findAtoms :: Symbol -> AtomicProofs -> [Term]
findAtoms = Map.findWithDefault []

addAtom :: Term -> Symbol -> AtomicProofs -> AtomicProofs
addAtom proof atom = Map.alter (Just . insertUnique . fromMaybe []) atom
  where
    -- Oldest first: an atom's proofs are consulted in arrival order, so
    -- the least-derived evidence (a goal argument, a named premise) is
    -- tried before compositions freshly derived from it.  Preferring
    -- recency here made every atom choice point reach for the newest
    -- derived junk first and buried argument-using proofs beyond any
    -- practical candidate window.
    insertUnique proofs
        | proof `elem` proofs = proofs
        | otherwise = proofs ++ [proof]

------------------------------
----- Implications of one atom, indexed by that atom.

type AtomImps = Map.Map Symbol Antecedents

extract :: AtomImps -> Symbol -> ([Antecedent], AtomImps)
extract atomImps a =
    case Map.updateLookupWithKey (\ _ _ -> Nothing) a atomImps of
        (found, rest) -> (fromMaybe [] found, rest)

-- Oldest first, as for atomic proofs: consequences fire in the order
-- their implications arrived.
insert :: AtomImps -> Symbol -> Antecedents -> AtomImps
insert atomImps a bs = Map.insertWith (flip (++)) a bs atomImps

------------------------------
----- Nested implications, (a -> b) -> c

-- NestImp proof a b c represents an antecedent (a :-> b) :-> c.
data NestImp = NestImp Term Formula Formula Formula
    deriving (Eq)

type NestImps = [NestImp]

-- Oldest first, as for atomic proofs: the branching over nested
-- implications tries them in arrival order.
addNestImp :: NestImp -> NestImps -> NestImps
addNestImp nested nestedImps
    | nested `elem` nestedImps = nestedImps
    | otherwise = nestedImps ++ [nested]

------------------------------
----- Generate a new unique variable
newSym :: String -> P Symbol
newSym prefix = P $ \ _ (PS next used) sk fk ->
    let (symbol, used', next') = allocateFresh
            (\suffix -> (Symbol $ prefix ++ show suffix, suffix + 1))
            used next
    in sk (PS next' used') symbol fk

------------------------------
----- Generate all ways to select one element of a list
select :: [a] -> P (a, [a])
select = choose . selections
  where
    selections [] = []
    selections (x : xs) =
        (x, xs) : [(y, x : ys) | (y, ys) <- selections xs]

------------------------------
-----

data Antecedent = A Term Formula
type Antecedents = [Antecedent]

type Goal = Formula

--
-- This is the main loop of the proof search.
--
-- The redant functions reduce antecedents and the redsucc
-- function reduces the goal (succedent).
--
-- The antecedents are kept in four groups: pending antecedents, atomic
-- implications, nested implications, and indexed atomic proofs.
--   Antecedents contains as yet unclassified antecedents; the redant functions
--     go through them one by one and reduces and classifies them.
--   AtomImps contains implications of the form (a -> b), where `a' is an atom.
--     To speed up the processing it is stored as a map from the `a' to all the
--     formulae it implies.
--   NestImps contains implications of the form ((b -> c) -> d)
--   AtomicProofs maps atomic formulae to their available proofs.
--
-- There is also a proof object associated with each antecedent.
--
redant :: SearchMode -> MoreSolutions -> Antecedents -> AtomImps -> NestImps
       -> AtomicProofs -> Goal -> P Proof
redant mode more antes atomImps nestImps atoms goal =
    case orderAntecedents mode antes of
        [] -> redsucc goal
        a : rest -> redant1 Nothing a rest goal
  where
    redant0 pending g = redant mode more pending atomImps nestImps atoms g

    redant1 ::
        Maybe (Symbol, [Term]) -> Antecedent -> Antecedents -> Goal -> P Proof
    redant1 fairChain antecedent@(A p f) pending g
        -- Prefer the direct identity between the same nominal empty type.
        -- Exploring elimination as an alternative would cause result scoring
        -- to print the less useful explicit empty case instead.
        | f == g && isNominalEmpty f = return p
        | f /= g = reduceAntecedent fairChain antecedent pending g
        | more = return p `mplus`
            reduceAntecedent fairChain antecedent pending g
        | otherwise = return p
      where
        isNominalEmpty (Empty _) = True
        isNominalEmpty _ = False

    -- Reduce and classify the first pending antecedent.
    reduceAntecedent ::
        Maybe (Symbol, [Term]) -> Antecedent -> Antecedents -> Goal -> P Proof
    reduceAntecedent _ (A p (PVar s)) pending g =
        let (consequences, remainingAtomImps) = extract atomImps s
            newAntecedents =
                [A (Apply f p) b | A f b <- consequences] ++ pending
        in redant mode more newAntecedents remainingAtomImps nestImps (addAtom p s atoms) g
    reduceAntecedent _ (A p (Conj conjuncts)) pending g = do
        variables <- mapM (const (newSym "v")) conjuncts
        proof <- redant0
            (zipWith (\ v f -> A (Var v) f) variables conjuncts ++ pending) g
        return $ applys (Csplit (length conjuncts))
            [foldr Lam proof variables, p]
    reduceAntecedent _ (A p (Disj alternatives)) pending g = do
        variables <- mapM (const (newSym "d")) alternatives
        proofs <- sequenceProofs proveAlternative (zip variables alternatives)
        -- Even when both propositions print as @false@, a raw empty
        -- disjunction and a nominal empty datatype are distinct proof-checker
        -- types.  Cross that boundary with the proper empty eliminator rather
        -- than returning an ill-typed identity proof.
        return $ applys (Ccases (map fst alternatives))
            (p : zipWith Lam variables proofs)
      where
        proveAlternative (v, (_, f)) =
            redant1 Nothing (A (Var v) f) pending g
    -- Empty datatypes have no constructors.  Preserve their nominal identity
    -- for equality, but eliminate any one of them explicitly with an empty case.
    reduceAntecedent _ (A p (Empty _)) _ _ =
        return $ Apply (Ccases []) p
    reduceAntecedent fairChain (A p (a :-> b)) pending g =
        reduceImp fairChain p a b pending g

    -- Reduce an implication antecedent.
    reduceImp ::
        Maybe (Symbol, [Term]) -> Term -> Formula -> Formula
        -> Antecedents -> Goal -> P Proof
    -- p : PVar s -> b
    reduceImp fairChain p (PVar s) b pending g =
        reduceAtomicImp fairChain p s b pending g
    -- p : (c & d) -> b
    reduceImp _ p (Conj conjuncts) b pending g = do
        x <- newSym "x"
        let implication = foldr (:->) b conjuncts
        proof <- redant1 Nothing (A (Var x) implication) pending g
        subst (curryTuple (length conjuncts) p) x proof
    -- p : (c | d) -> b
    reduceImp _ p (Disj alternatives) b pending g = do
        variables <- mapM (const (newSym "d")) alternatives
        proof <- redant0
            (zipWith (\ v (_, d) -> A (Var v) (d :-> b)) variables alternatives
                ++ pending) g
        foldM substituteInjection proof (zip3 [0 ..] variables alternatives)
      where
        substituteInjection result (i, v, (constructor, _)) =
            subst (inj constructor i p) v result
    -- An implication from an empty type is always available and contributes
    -- no usable premise.
    reduceImp _ _ (Empty _) _ pending g = redant0 pending g
    -- p : (c -> d) -> b
    reduceImp _ p (c :-> d) b pending g =
        reduceNestedImp p c d b pending g

    -- Reduce a nested implication antecedent.
    reduceNestedImp ::
        Term -> Formula -> Formula -> Formula -> Antecedents -> Goal -> P Proof
    -- Exploit ~(C->D) <=> (~~C & ~D), retaining the particular empty result
    -- type throughout the transformation.
    reduceNestedImp p c d emptyResult@(Empty _) pending g
        | d /= emptyResult = do
        x <- newSym "x"
        y <- newSym "y"
        proof <- reduceNestedImp (Var x) c emptyResult emptyResult
            (A (Var y) (d :-> emptyResult) : pending) g
        cImpDImpFalse x y p proof
    reduceNestedImp p c d b pending g =
        redant mode more pending atomImps
            (addNestImp (NestImp p c d b) nestImps) atoms g

    -- Reduce an implication whose antecedent is atomic.  One branch applies
    -- it to an atom already in scope; the other indexes it for later use.
    reduceAtomicImp ::
        Maybe (Symbol, [Term]) -> Term -> Symbol -> Formula
        -> Antecedents -> Goal -> P Proof
    reduceAtomicImp fairChain p s b pending g =
        applyAvailable
        `mplus`
        redant mode more pending (insert atomImps s [A p b])
            nestImps atoms g
      where
        available = orderProofs mode $ findAtoms s atoms
        -- A binary endomorphism is the common combining shape where reusing
        -- the first argument can starve a direct mixed application.  Prefer
        -- unused proofs within a small oldest-first cohort, fairly
        -- interleaving that cohort so repeated applications remain available.
        -- The untouched tail and exact shape guard keep unrelated wide rank-N
        -- plans on their historical depth-first traversal.
        fairSiblingLimit = 3
        (fairAvailable, remainingAvailable) =
            splitAt fairSiblingLimit available
        priorArguments = case fairChain of
            Just (domain, arguments) | domain == s -> arguments
            _ -> []
        orderedFair
            | null priorArguments = fairAvailable
            | otherwise =
                filter (`notElem` priorArguments) fairAvailable
                ++ filter (`elem` priorArguments) fairAvailable
        applyAvailable = case available of
            [] -> mzero
            atom : _ | not more -> applyAtom atom
            _ | continueFair -> applyFair
            _ -> choose available >>= applyAtom
        applyFair = case remainingAvailable of
            [] -> interleaveChoices (map applyAtom orderedFair)
            _ -> interleaveChoices (map applyAtom orderedFair)
                `mplus` (choose remainingAvailable >>= applyAtom)
        continueFair = not (null priorArguments) || repeatedEndomorphism s b
        repeatedEndomorphism domain (PVar next :-> PVar result) =
            domain == next && domain == result
        repeatedEndomorphism _ _ = False
        applyAtom atom = do
            x <- newSym "x"
            let nextChain
                    | continueFair = Just (s, atom : priorArguments)
                    | otherwise = Nothing
            proof <- redant1 nextChain (A (Var x) b) pending g
            subst (Apply p atom) x proof

    -- Reduce the goal once every antecedent has been classified.
    redsucc :: Goal -> P Proof
    redsucc atomicGoal@(PVar s) =
        cutSearch more (choose (orderProofs mode $ findAtoms s atoms))
        `mplus`
        if goalMayBeReachable s atomImps nestImps then
            chooseNestedImp atomicGoal
        else
            mzero
    redsucc (Conj conjuncts) = do
        proofs <- sequenceProofs redsucc conjuncts
        return $ applys (Ctuple (length conjuncts)) proofs
    -- Push the choice of disjunct into implication processing on the left.
    -- 'newSym' is seeded with every input atom, so the continuation atom is
    -- fresh even when an input type contains names such as @_2@.
    redsucc (Disj alternatives) = do
        continuation <- newSym "_"
        let v = PVar continuation
            injections =
                [A (Cinj constructor i) (d :-> v)
                    | (i, (constructor, d)) <- zip [0 ..] alternatives]
        redant0 injections v
    -- An empty goal follows exactly when the antecedents are contradictory.
    -- Use the disjunction encoding with no injections: prove a fresh atom
    -- that nothing else mentions, so only ex falso reasoning can reach it.
    -- The proof is parametric in that atom and therefore proves the empty
    -- goal as well.  (Returning mzero here would wrongly reject theorems
    -- such as Not (Not (Either a (Not a))), whose final goal is Void and
    -- needs the nested-implication machinery below.)
    redsucc (Empty _) = do
        continuation <- newSym "_"
        redant0 [] (PVar continuation)
    redsucc implication@(a :-> b) =
        cutSearch more (choose $ orderProofs mode $ findIndexedImplications implication)
        `mplus`
        do
            s <- newSym "x"
            proof <- redant1 Nothing (A (Var s) a) [] b
            return $ Lam s proof

    -- Implications are indexed after antecedent processing.  Consult those
    -- indexes before eta-expanding an implication goal; otherwise moving a
    -- complex implication through a tuple can lose its direct identity proof.
    findIndexedImplications :: Formula -> [Term]
    findIndexedImplications (PVar atom :-> consequent) =
        [proof |
            A proof indexedConsequent <-
                Map.findWithDefault [] atom atomImps,
            indexedConsequent == consequent]
    findIndexedImplications ((argument :-> result) :-> consequent) =
        [proof |
            NestImp proof indexedArgument indexedResult indexedConsequent <-
                nestImps,
            indexedArgument == argument,
            indexedResult == result,
            indexedConsequent == consequent]
    findIndexedImplications _ = []

    -- Nested implications are the branching point of the search.  Try each
    -- one once, removing the selected implication from the recursive calls.
    chooseNestedImp :: Goal -> P Proof
    chooseNestedImp g = do
        (NestImp p c d b, remaining) <- select $ orderNestedProofs mode nestImps
        x <- newSym "x"
        z <- newSym "z"
        qz <- redant mode more [A (Var z) (d :-> b)] atomImps remaining atoms (c :-> d)
        proof <- redant mode more [A (Var x) b] atomImps remaining atoms g
        subst (applyImp p (Lam z qz)) x proof

    -- Each tuple component and case branch has its own alternatives. Ordinary
    -- bind exhausts every later component before advancing the first one,
    -- even when choice nodes themselves interleave. Use the existing charged,
    -- freshness-preserving fair bind for explicitly interleaved alternatives.
    sequenceProofs _ [] = pure []
    sequenceProofs proveOne (part : parts)
        | more && searchStrategy mode == Interleave =
            bindInterleaved (proveOne part) $ \proof ->
                (proof :) <$> sequenceProofs proveOne parts
        | otherwise = do
            proof <- proveOne part
            (proof :) <$> sequenceProofs proveOne parts

-- A cheap necessary-condition check before branching over nested implications.
-- On the left, every disjunct must yield the atom, while any conjunct may do
-- so.  Consequently false (an empty disjunction) yields every atom and true
-- (an empty conjunction) yields none.
goalMayBeReachable :: Symbol -> AtomImps -> NestImps -> Bool
goalMayBeReachable goal atomImps nestImps =
    any atomImpMayYield (Map.elems atomImps) || any nestedImpMayYield nestImps
  where
    atomImpMayYield = any (\ (A _ f) -> mayYield goal f)
    nestedImpMayYield (NestImp _ _ _ consequent) = mayYield goal consequent

mayYield :: Symbol -> Formula -> Bool
mayYield goal (Disj alternatives) = all (mayYield goal . snd) alternatives
mayYield _ (Empty _) = True
mayYield goal (Conj conjuncts) = any (mayYield goal) conjuncts
mayYield goal (_ :-> consequent) = mayYield goal consequent
mayYield goal (PVar atom) = goal == atom

cutSearch :: MoreSolutions -> P a -> P a
cutSearch False p = atMostOne p
cutSearch True p = p

---------------------------
