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

import LJT
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
    ]

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
