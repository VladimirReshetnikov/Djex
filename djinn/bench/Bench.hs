--
-- Proof-search benchmarks over the corpus in Corpus.hs.
--
-- Three measurements per applicable entry:
--   first:  time to the first proof (or to refutation for non-theorems),
--           the latency an interactive query pays before printing anything;
--   multi:  time to enumerate up to 200 alternative proofs, the default
--           +sorted/+multi collection cost;
--   decide: full 'provable' answer, which for non-theorems is the cost of
--           exhausting the search space.
--
module Main (main) where

import Test.Tasty.Bench

import Djinn.Internal.Environment
    (Environment, TypeDefinition, prepareEnvironment)
import Djinn.Internal.HTypes (prepareTypeFormulaTranslator)
import Djinn.Internal.LJT
import Corpus

main :: IO ()
main = defaultMain
    [ bgroup "first"
        [ bench (entryName e) $ whnf firstProofSize (entryFormula e)
        | e <- corpus ]
    , bgroup "multi200"
        [ bench (entryName e) $ whnf (multiSize 200) (entryFormula e)
        | e <- corpus, entryProvable e ]
    , bgroup "decide"
        [ bench (entryName e) $ whnf provable (entryFormula e)
        | e <- corpus ]
    -- First-result latency under each branch-exploration strategy.
    , bgroup "strategy"
        [ bench (entryName e ++ "/" ++ show strat) $
            whnf (firstIn strat) (entryFormula e)
        | e <- corpus
        , strat <- [DepthFirst, Interleave] ]
    -- What the Step-counting consumer costs when a budget is set but never
    -- reached (versus the unlimited path above).
    , bgroup "budgetOverhead"
        [ bench (entryName e) $ whnf budgetedFirst (entryFormula e)
        | e <- corpus ]
    -- Preparation benchmarks stay separate from proof search.  The first
    -- group measures the raw formula translator's whole-table validation;
    -- the second includes shared inventory/kind/synonym preparation and the
    -- cached translator retained by a complete checked environment.
    , bgroup "aliasPreparation"
        [ bgroup "translator"
            [ bench (aliasEntryName entry) $
                whnf prepareTranslator (aliasDefinitions entry)
            | entry <- aliasPreparationCorpus
            ]
        , bgroup "environment"
            [ bench (aliasEntryName entry) $
                whnf prepareFullEnvironment (aliasEnvironment entry)
            | entry <- aliasPreparationCorpus
            ]
        ]
    ]

-- Pattern-matching on Right forces the validation and closure construction.
-- The closure itself is put into WHNF, but no query is translated and no
-- expanded formula tree is rendered or traversed by the benchmark consumer.
{-# NOINLINE prepareTranslator #-}
prepareTranslator :: [TypeDefinition] -> Bool
prepareTranslator definitions = case
        prepareTypeFormulaTranslator definitions of
    Left failure -> error $ "invalid alias benchmark: " ++ failure
    Right translate -> translate `seq` True

-- The PreparedEnvironment constructor is private, so seq is the narrowest
-- public way to force successful sealing without inspecting or rendering any
-- of its cached expanded representations.
{-# NOINLINE prepareFullEnvironment #-}
prepareFullEnvironment :: Environment -> Bool
prepareFullEnvironment environment = case prepareEnvironment environment of
    Left failure -> error $ "invalid environment benchmark: " ++ show failure
    Right prepared -> prepared `seq` True

firstIn :: Strategy -> Formula -> Int
firstIn strat formula =
    case searchProofs outcome of
        [] -> 0
        proof : _ -> termSize proof
  where
    outcome = proveWithMode
        (defaultSearchMode False) { searchStrategy = strat } [] formula

budgetedFirst :: Formula -> Int
budgetedFirst formula =
    case searchProofs outcome of
        [] -> 0
        proof : _ -> termSize proof
  where
    outcome = proveWithMode
        (defaultSearchMode False) { searchBudget = Just 1000000000 }
        [] formula

-- Fully force one proof term without needing an NFData instance.
termSize :: Term -> Int
termSize (Var _) = 1
termSize (Lam _ body) = 1 + termSize body
termSize (Apply f a) = 1 + termSize f + termSize a
termSize (Xsel _ _ e) = 1 + termSize e
termSize _ = 1

firstProofSize :: Formula -> Int
firstProofSize formula =
    case prove False [] formula of
        [] -> 0
        proof : _ -> termSize proof

multiSize :: Int -> Formula -> Int
multiSize limit formula =
    sum $ map termSize $ take limit $ prove True [] formula
