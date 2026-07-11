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

module LJT (module LJTFormula, provable, prove, Proof) where

import Control.Applicative (Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(mzero, mplus), ap, foldM)
import Data.List ((!?))
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import LJTFormula

-- Whether local proof-search cuts should retain their alternative paths.
type MoreSolutions = Bool

provable :: Formula -> Bool
provable = not . null . prove False []

prove :: MoreSolutions -> [(Symbol, Formula)] -> Formula -> [Proof]
prove more env goal =
    runP reservedSymbols $ redtop more env goal
  where
    -- Symbol is shared by proof variables and propositional atoms.  Reserving
    -- both namespaces prevents generated binders from capturing environment
    -- variables and keeps the atom introduced for disjunction genuinely fresh.
    reservedSymbols =
        map fst env ++ concatMap (formulaSymbols . snd) env ++ formulaSymbols goal

-- Fold the environment into the goal as premises, prove the resulting
-- implication, then apply the proof to the environment variables and
-- normalize, leaving a term whose free variables are the assumption names.
redtop :: MoreSolutions -> [(Symbol, Formula)] -> Formula -> P Proof
redtop more env goal = do
    let form = foldr (:->) goal (map snd env)
    p <- redant more [] [] [] [] form
    nf (applys p (map (Var . fst) env))

------------------------------
-----
type Proof = Term

-- The proof search gives every binder a globally fresh symbol (including with
-- respect to caller-supplied symbols).  Substitution can therefore stay small:
-- only ordinary shadowing needs an explicit check.  A replacement is copied so
-- that its binders remain globally unique at every occurrence.
subst :: Term -> Symbol -> Term -> P Term
subst replacement variable = substitute
  where
    substitute t@(Var s)
        | variable == s = copy [] replacement
        | otherwise = return t
    substitute t@(Lam s body)
        | variable == s = return t
        | otherwise = Lam s <$> substitute body
    substitute (Apply f a) = Apply <$> substitute f <*> substitute a
    substitute (Xsel i n e) = Xsel i n <$> substitute e
    substitute t = return t

copy :: [(Symbol, Symbol)] -> Term -> P Term
copy renamings (Var s) = return $ Var $ fromMaybe s $ lookup s renamings
copy renamings (Lam s body) = do
    s' <- newSym "c"
    Lam s' <$> copy ((s, s') : renamings) body
copy renamings (Apply f a) = Apply <$> copy renamings f <*> copy renamings a
copy renamings (Xsel i n e) = Xsel i n <$> copy renamings e
copy _ t = return t

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
----- Our Proof monad, P, a monad with state and multiple results

-- Note, this is the non-standard way to combine state with multiple
-- results.  But this is much better for backtracking.
newtype P a = P { unP :: PS -> [(PS, a)] }

instance Functor P where
    fmap f (P m) = P $ \ s ->
        [(s', f x) | (s', x) <- m s]

instance Applicative P where
    pure x = P $ \ s -> [(s, x)]
    (<*>) = ap

instance Monad P where
    return = pure
    P m >>= f = P $ \ s ->
        [ y | (s',x) <- m s, y <- unP (f x) s' ]

instance Alternative P where
    empty = mzero
    (<|>) = mplus

instance MonadPlus P where
    mzero = P $ \ _s -> []
    P fxs `mplus` P fys = P $ \ s -> fxs s ++ fys s

-- The state carries both the next suffix and every symbol already in use.
-- The initial used set contains caller-supplied term and formula symbols; each
-- generated symbol is then recorded here as well.
data PS = PS !Integer (Set.Set Symbol)

startPS :: [Symbol] -> PS
startPS = PS 1 . Set.fromList

choose :: [a] -> P a
choose xs = P $ \ s -> [(s, x) | x <- xs]

atMostOne :: P a -> P a
atMostOne (P f) = P $ \ s -> take 1 (f s)

runP :: [Symbol] -> P a -> [a]
runP reserved (P m) = map snd (m (startPS reserved))


------------------------------
----- Atomic formulae
data AtomF = AtomF Term Symbol
    deriving (Eq)

type AtomFs = [AtomF]

findAtoms :: Symbol -> AtomFs -> [Term]
findAtoms s atoms = [ p | AtomF p s' <- atoms, s == s' ]

addAtom :: AtomF -> AtomFs -> AtomFs
addAtom atom atoms
    | atom `elem` atoms = atoms
    | otherwise = atom : atoms

------------------------------
----- Implications of one atom

data AtomImp = AtomImp Symbol Antecedents
type AtomImps = [AtomImp]

extract :: AtomImps -> Symbol -> ([Antecedent], AtomImps)
extract aatomImps@(atomImp@(AtomImp a' bs) : atomImps) a =
    case compare a a' of
    GT -> let (rbs, restImps) = extract atomImps a in (rbs, atomImp : restImps)
    EQ -> (bs, atomImps)
    LT -> ([], aatomImps)
extract [] _ = ([], [])

insert :: AtomImps -> AtomImp -> AtomImps
insert [] ai = [ ai ]
insert aatomImps@(atomImp@(AtomImp a' bs') : atomImps) ai@(AtomImp a bs) =
    case compare a a' of
    GT -> atomImp : insert atomImps ai
    EQ -> AtomImp a (bs ++ bs') : atomImps
    LT -> ai : aatomImps

------------------------------
----- Nested implications, (a -> b) -> c

-- NestImp proof a b c represents an antecedent (a :-> b) :-> c.
data NestImp = NestImp Term Formula Formula Formula
    deriving (Eq)

type NestImps = [NestImp]

addNestImp :: NestImp -> NestImps -> NestImps
addNestImp nested nestedImps
    | nested `elem` nestedImps = nestedImps
    | otherwise = nested : nestedImps

------------------------------
----- Generate a new unique variable
newSym :: String -> P Symbol
newSym prefix = P $ \ (PS next used) ->
    let (suffix, symbol) = firstUnused next
        firstUnused i =
            let candidate = Symbol (prefix ++ show i)
            in if candidate `Set.member` used
               then firstUnused (i + 1)
               else (i, candidate)
    in [(PS (suffix + 1) (Set.insert symbol used), symbol)]

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
-- The antecedents are kept in four groups: Antecedents, AtomImps, NestImps, AtomFs
--   Antecedents contains as yet unclassified antecedents; the redant functions
--     go through them one by one and reduces and classifies them.
--   AtomImps contains implications of the form (a -> b), where `a' is an atom.
--     To speed up the processing it is stored as a map from the `a' to all the
--     formulae it implies.
--   NestImps contains implications of the form ((b -> c) -> d)
--   AtomFs contains atomic formulae.
--
-- There is also a proof object associated with each antecedent.
--
redant :: MoreSolutions -> Antecedents -> AtomImps -> NestImps -> AtomFs -> Goal -> P Proof
redant more antes atomImps nestImps atoms goal =
    case antes of
        [] -> redsucc goal
        a : rest -> redant1 a rest goal
  where
    redant0 pending g = redant more pending atomImps nestImps atoms g

    redant1 :: Antecedent -> Antecedents -> Goal -> P Proof
    redant1 antecedent@(A p f) pending g
        -- Prefer the direct identity between the same nominal empty type.
        -- Exploring elimination as an alternative would cause result scoring
        -- to print the less useful @void@ instead.
        | f == g && isNominalEmpty f = return p
        | f /= g = reduceAntecedent antecedent pending g
        | more = return p `mplus` reduceAntecedent antecedent pending g
        | otherwise = return p
      where
        isNominalEmpty (Empty _) = True
        isNominalEmpty _ = False

    -- Reduce and classify the first pending antecedent.
    reduceAntecedent :: Antecedent -> Antecedents -> Goal -> P Proof
    reduceAntecedent (A p (PVar s)) pending g =
        let atom = AtomF p s
            (consequences, remainingAtomImps) = extract atomImps s
            newAntecedents =
                [A (Apply f p) b | A f b <- consequences] ++ pending
        in redant more newAntecedents remainingAtomImps nestImps
             (addAtom atom atoms) g
    reduceAntecedent (A p (Conj conjuncts)) pending g = do
        variables <- mapM (const (newSym "v")) conjuncts
        proof <- redant0
            (zipWith (\ v f -> A (Var v) f) variables conjuncts ++ pending) g
        return $ applys (Csplit (length conjuncts))
            [foldr Lam proof variables, p]
    reduceAntecedent (A p (Disj alternatives)) pending g = do
        variables <- mapM (const (newSym "d")) alternatives
        proofs <- mapM proveAlternative (zip variables alternatives)
        if null alternatives && g == false
            then
                -- Avoid constructing @void p :: Void@; @p@ already has type Void.
                return p
            else
                return $ applys (Ccases (map fst alternatives))
                    (p : zipWith Lam variables proofs)
      where
        proveAlternative (v, (_, f)) = redant1 (A (Var v) f) pending g
    -- Empty datatypes have no constructors.  Preserve their nominal identity
    -- for equality, but eliminate any one of them explicitly with @void@.
    reduceAntecedent (A p (Empty _)) _ _ =
        return $ Apply (Ccases []) p
    reduceAntecedent (A p (a :-> b)) pending g =
        reduceImp p a b pending g

    -- Reduce an implication antecedent.
    reduceImp :: Term -> Formula -> Formula -> Antecedents -> Goal -> P Proof
    -- p : PVar s -> b
    reduceImp p (PVar s) b pending g = reduceAtomicImp p s b pending g
    -- p : (c & d) -> b
    reduceImp p (Conj conjuncts) b pending g = do
        x <- newSym "x"
        let implication = foldr (:->) b conjuncts
        proof <- redant1 (A (Var x) implication) pending g
        subst (curryTuple (length conjuncts) p) x proof
    -- p : (c | d) -> b
    reduceImp p (Disj alternatives) b pending g = do
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
    reduceImp _ (Empty _) _ pending g = redant0 pending g
    -- p : (c -> d) -> b
    reduceImp p (c :-> d) b pending g = reduceNestedImp p c d b pending g

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
    reduceAtomicImp :: Term -> Symbol -> Formula -> Antecedents -> Goal -> P Proof
    reduceAtomicImp p s b pending g =
        (do
            atom <- cutSearch more $ choose (findAtoms s atoms)
            x <- newSym "x"
            proof <- redant1 (A (Var x) b) pending g
            subst (Apply p atom) x proof)
        `mplus`
        redant more pending (insert atomImps (AtomImp s [A p b]))
            nestImps atoms g

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
            proof <- redant1 (A (Var s) a) [] b
            return $ Lam s proof

    -- Implications are indexed after antecedent processing.  Consult those
    -- indexes before eta-expanding an implication goal; otherwise moving a
    -- complex implication through a tuple can lose its direct identity proof.
    findIndexedImplications :: Formula -> [Term]
    findIndexedImplications (PVar atom :-> consequent) =
        [proof |
            AtomImp indexedAtom consequences <- atomImps,
            indexedAtom == atom,
            A proof indexedConsequent <- consequences,
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
    any atomImpMayYield atomImps || any nestedImpMayYield nestImps
  where
    atomImpMayYield (AtomImp _ consequences) =
        any (\ (A _ f) -> mayYield goal f) consequences
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
