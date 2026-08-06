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

module Djinn.Internal.LJT (
    module Djinn.Internal.LJTFormula, provable, prove, Proof,
    SearchMode(..), Strategy(..), SearchOutcome(..),
    defaultSearchMode, proveWithMode, proveWithModeChecked
    ) where

import Control.Applicative (Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(mzero, mplus), ap, foldM)
import Data.List ((!?))
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Fresh (allocateFresh)
import Djinn.Internal.LJTFormula
import Djinn.Internal.ProofCheck (checkProofEnvironment)

-- Whether local proof-search cuts should retain their alternative paths.
type MoreSolutions = Bool

-- How alternative branches are explored at each choice point.  DepthFirst
-- is the classical order (fully explore the first branch before the
-- second); Interleave alternates between branches at every choice point,
-- so an expensive dead end cannot starve a cheap alternative.
data Strategy = DepthFirst | Interleave
    deriving (Eq, Show)

-- A named description of one proof search.
data SearchMode = SearchMode {
    -- Retain alternative proofs at local search cuts (multiple solutions).
    searchAlternatives :: Bool,
    searchStrategy :: Strategy,
    -- Maximum number of choice points to explore; Nothing is unlimited.
    -- With a limit the search is no longer a decision procedure: an empty
    -- result with searchExhausted set means "not found", not "unprovable".
    searchBudget :: Maybe Integer
    }
    deriving (Show)

-- The classical search: depth-first, unbudgeted, complete.
defaultSearchMode :: MoreSolutions -> SearchMode
defaultSearchMode more = SearchMode {
    searchAlternatives = more,
    searchStrategy = DepthFirst,
    searchBudget = Nothing
    }

data SearchOutcome = SearchOutcome {
    searchProofs :: [Proof],
    -- True when the budget ran out with unexplored search space left.
    searchExhausted :: Bool,
    -- Fuel left after the explored prefix.  A caller that performs a
    -- follow-up search can pass this remainder on without silently turning
    -- one query budget into two.  For an unbounded search it stays 'Nothing'.
    remainingSearchBudget :: Maybe Integer
    }

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
proveWithMode mode env goal =
    SearchOutcome proofs exhausted remaining
  where
    (proofs, exhausted, remaining) =
        runBounded (searchBudget mode) (searchStrategy mode) reservedSymbols $
            redtop (searchAlternatives mode) env goal
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

-- Fold the environment into the goal as premises, prove the resulting
-- implication, then apply the proof to the environment variables and
-- normalize, leaving a term whose free variables are the assumption names.
redtop :: MoreSolutions -> [(Symbol, Formula)] -> Formula -> P Proof
redtop more env goal = do
    let form = foldr (:->) goal (map snd env)
    p <- redant more [] Map.empty [] Map.empty form
    nf (applys p (map (Var . fst) env))

------------------------------
-----
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
    replay sk fk $ roundRobin
        (reify strat s choice : map (Step . reify strat s) choices)
  where
    -- Advance every live stream by one node per round.  The reversed rear
    -- list makes queue rotation amortized constant-time without favoring a
    -- right-nested suffix when three or more proofs are available.
    roundRobin streams = advance streams []
    advance [] [] = Done
    advance [] rear = advance (reverse rear) []
    advance (Done : streams) rear = advance streams rear
    advance (Yield result rest : streams) rear =
        Yield result (advance streams (rest : rear))
    advance (Step rest : streams) rear =
        Step (advance streams (rest : rear))

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
redant :: MoreSolutions -> Antecedents -> AtomImps -> NestImps
       -> AtomicProofs -> Goal -> P Proof
redant more antes atomImps nestImps atoms goal =
    case antes of
        [] -> redsucc goal
        a : rest -> redant1 False a rest goal
  where
    redant0 pending g = redant more pending atomImps nestImps atoms g

    redant1 :: Bool -> Antecedent -> Antecedents -> Goal -> P Proof
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
    reduceAntecedent :: Bool -> Antecedent -> Antecedents -> Goal -> P Proof
    reduceAntecedent _ (A p (PVar s)) pending g =
        let (consequences, remainingAtomImps) = extract atomImps s
            newAntecedents =
                [A (Apply f p) b | A f b <- consequences] ++ pending
        in redant more newAntecedents remainingAtomImps nestImps
             (addAtom p s atoms) g
    reduceAntecedent _ (A p (Conj conjuncts)) pending g = do
        variables <- mapM (const (newSym "v")) conjuncts
        proof <- redant0
            (zipWith (\ v f -> A (Var v) f) variables conjuncts ++ pending) g
        return $ applys (Csplit (length conjuncts))
            [foldr Lam proof variables, p]
    reduceAntecedent _ (A p (Disj alternatives)) pending g = do
        variables <- mapM (const (newSym "d")) alternatives
        proofs <- mapM proveAlternative (zip variables alternatives)
        -- Even when both propositions print as @false@, a raw empty
        -- disjunction and a nominal empty datatype are distinct proof-checker
        -- types.  Cross that boundary with the proper empty eliminator rather
        -- than returning an ill-typed identity proof.
        return $ applys (Ccases (map fst alternatives))
            (p : zipWith Lam variables proofs)
      where
        proveAlternative (v, (_, f)) =
            redant1 False (A (Var v) f) pending g
    -- Empty datatypes have no constructors.  Preserve their nominal identity
    -- for equality, but eliminate any one of them explicitly with an empty case.
    reduceAntecedent _ (A p (Empty _)) _ _ =
        return $ Apply (Ccases []) p
    reduceAntecedent fairChain (A p (a :-> b)) pending g =
        reduceImp fairChain p a b pending g

    -- Reduce an implication antecedent.
    reduceImp ::
        Bool -> Term -> Formula -> Formula -> Antecedents -> Goal -> P Proof
    -- p : PVar s -> b
    reduceImp fairChain p (PVar s) b pending g =
        reduceAtomicImp fairChain p s b pending g
    -- p : (c & d) -> b
    reduceImp _ p (Conj conjuncts) b pending g = do
        x <- newSym "x"
        let implication = foldr (:->) b conjuncts
        proof <- redant1 False (A (Var x) implication) pending g
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
        redant more pending atomImps
            (addNestImp (NestImp p c d b) nestImps) atoms g

    -- Reduce an implication whose antecedent is atomic.  One branch applies
    -- it to an atom already in scope; the other indexes it for later use.
    reduceAtomicImp ::
        Bool -> Term -> Symbol -> Formula -> Antecedents -> Goal -> P Proof
    reduceAtomicImp fairChain p s b pending g =
        applyAvailable
        `mplus`
        redant more pending (insert atomImps s [A p b])
            nestImps atoms g
      where
        available = findAtoms s atoms
        applyAvailable = case available of
            [] -> mzero
            atom : _ | not more -> applyAtom atom
            _ | continueFair -> interleaveChoices (map applyAtom available)
            _ -> choose available >>= applyAtom
        continueFair = fairChain || repeatsDomain s b
        repeatsDomain domain (PVar next :-> _) = domain == next
        repeatsDomain _ _ = False
        applyAtom atom = do
            x <- newSym "x"
            proof <- redant1 continueFair (A (Var x) b) pending g
            subst (Apply p atom) x proof

    -- Reduce the goal once every antecedent has been classified.
    redsucc :: Goal -> P Proof
    redsucc atomicGoal@(PVar s) =
        cutSearch more (choose (findAtoms s atoms))
        `mplus`
        if goalMayBeReachable s atomImps nestImps then
            chooseNestedImp atomicGoal
        else
            mzero
    redsucc (Conj conjuncts) = do
        proofs <- mapM redsucc conjuncts
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
        cutSearch more (choose $ findIndexedImplications implication)
        `mplus`
        do
            s <- newSym "x"
            proof <- redant1 False (A (Var s) a) [] b
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
        (NestImp p c d b, remaining) <- select nestImps
        x <- newSym "x"
        z <- newSym "z"
        qz <- redant more [A (Var z) (d :-> b)] atomImps remaining atoms
            (c :-> d)
        proof <- redant more [A (Var x) b] atomImps remaining atoms g
        subst (applyImp p (Lam z qz)) x proof

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
