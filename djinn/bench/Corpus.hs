--
-- A representative formula corpus for benchmarking LJT proof search.
--
-- Families are size-parameterized so the same shapes can be measured at
-- several scales.  Every entry records whether it is provable, so harness
-- and tests can assert the corpus itself stays honest.
--
module Corpus (
    Entry(..), corpus,
    atom, nots, implChain, pickOne, wideDisj, projection
    ) where

import LJTFormula

data Entry = Entry {
    entryName :: String,
    entryProvable :: Bool,
    entryFormula :: Formula
    }

atom :: Int -> Formula
atom i = PVar $ Symbol ("a" ++ show i)

-- k-fold negation of a1.
nots :: Int -> Formula
nots k = iterate fnot (atom 1) !! k

-- (a1 -> a2) -> ... -> (a_{n-1} -> a_n) -> a1 -> a_n.  Populates n distinct
-- entries in the AtomImps index; forward chaining is linear, so this family
-- stresses indexed lookup rather than branching.
implChain :: Int -> Formula
implChain n =
    foldr (:->) (atom 1 :-> atom n)
        [atom i :-> atom (i + 1) | i <- [1 .. n - 1]]

-- a -> a -> ... -> a (n premises) -> a.  One proof per premise, so with
-- alternatives enabled this measures multi-solution enumeration.
pickOne :: Int -> Formula
pickOne n = foldr (:->) (atom 0) (replicate n (atom 0))

-- An n-way disjunction mapped to itself.  Case analysis multiplied by
-- injection choice.
wideDisj :: Int -> Formula
wideDisj n = disj :-> disj
  where
    disj = Disj [(ConsDesc ("C" ++ show i) 1, atom i) | i <- [1 .. n]]

-- (a1, ..., an) -> a_{n/2}.
projection :: Int -> Formula
projection n = Conj (map atom [1 .. n]) :-> atom (max 1 (n `div` 2))

-- Continuation-monad bind: ((a->r)->r) -> (a -> (b->r)->r) -> (b->r)->r.
contBind :: Formula
contBind =
    ca :-> ((a :-> cb) :-> cb)
  where
    a = atom 1
    b = atom 2
    r = atom 3
    ca = (a :-> r) :-> r
    cb = (b :-> r) :-> r

-- callCC: ((a -> (b->r)->r) -> (a->r)->r) -> (a->r)->r.
callCC :: Formula
callCC = ((a :-> cb) :-> ca) :-> ca
  where
    a = atom 1
    b = atom 2
    r = atom 3
    ca = (a :-> r) :-> r
    cb = (b :-> r) :-> r

-- State-monad bind: (s -> (a, s)) -> (a -> s -> (b, s)) -> s -> (b, s).
stateBind :: Formula
stateBind = sa :-> ((a :-> sb) :-> sb)
  where
    a = atom 1
    b = atom 2
    s = atom 3
    sa = s :-> (a & s)
    sb = s :-> (b & s)

-- Double negation of the law of excluded middle: an empty final goal with
-- contradictory antecedents, exercising the ex falso machinery.
dnLEM :: Formula
dnLEM = fnot $ fnot $ atom 1 |: fnot (atom 1)

-- Peirce's law: classically valid, intuitionistically unprovable.  Search
-- must exhaust the space to answer.
peirce :: Formula
peirce = ((atom 1 :-> atom 2) :-> atom 1) :-> atom 1

-- Nested Peirce-flavored non-theorem with a larger refutation space.
peirceTower :: Int -> Formula
peirceTower n = foldr wrap (atom 0) [1 .. n]
  where
    wrap i inner = ((inner :-> atom i) :-> inner) :-> inner

-- A disjunctive goal whose first alternative hides an expensive refutation
-- and whose second alternative is trivially provable.  A depth-first search
-- pays for the dead end before reaching the easy disjunct; a fair strategy
-- can reach it sooner.
starvation :: Int -> Formula
starvation n =
    Disj [ (ConsDesc "Hard" 1, peirceTower n)
         , (ConsDesc "Easy" 1, atom 50 :-> atom 50)
         ]

corpus :: [Entry]
corpus =
    [ Entry "projection/16" True (projection 16)
    , Entry "implChain/12" True (implChain 12)
    , Entry "implChain/48" True (implChain 48)
    , Entry "pickOne/8" True (pickOne 8)
    , Entry "wideDisj/6" True (wideDisj 6)
    , Entry "contBind" True contBind
    , Entry "callCC" True callCC
    , Entry "stateBind" True stateBind
    , Entry "dnLEM" True dnLEM
    , Entry "tripleNeg" True (nots 3 :-> nots 1)
    , Entry "starvation/3" True (starvation 3)
    , Entry "peirce" False peirce
    , Entry "peirceTower/3" False (peirceTower 3)
    , Entry "implChainBack/10" False backChain
    ]
  where
    -- Reversed chain: a_n cannot reach a_1, forcing exhaustive search over
    -- the same indexes that implChain uses productively.
    backChain =
        foldr (:->) (atom 10 :-> atom 1)
            [atom i :-> atom (i + 1) | i <- [1 .. 9]]
