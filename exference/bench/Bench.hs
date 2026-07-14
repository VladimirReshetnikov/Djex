{-# LANGUAGE BangPatterns #-}

--
-- Bounded search-core benchmarks over the fixtures in Corpus.hs.  Each
-- consumer forces exactly the intended portion of Exference's lazy trace and
-- rejects a semantic change that would otherwise look like a speedup.
--
module Main (main) where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Numeric.Natural (Natural)
import Test.Tasty.Bench (Benchmark, bench, defaultMain, whnf)

import Corpus
import Language.Haskell.Exference.Core
  ( ExferenceGeneratedSearchBatch
  , ExferenceQuery (..)
  , findGeneratedSearchBatchesInEnvironmentEither
  )
import Language.Haskell.Exference.Core.Candidate
  ( ExferenceCandidateDetails (..)
  , ExferenceGeneratedCandidate
  )
import Language.Haskell.Exference.Core.ExferenceStats
  ( ExferenceBatchMetadata (..)
  , ExferenceStats (..)
  )
import qualified Language.Haskell.Synthesis.Candidate as Candidate
import qualified Language.Haskell.Synthesis.Generated as Generated
import qualified Language.Haskell.Synthesis.Search as Search

main :: IO ()
main = defaultMain $ map benchmarkEntry corpus

benchmarkEntry :: Entry -> Benchmark
benchmarkEntry entry = bench (entryName entry)
  $ whnf (runEntry entry) (entryQuery entry)

-- Keep the checked query as the argument supplied by tasty-bench.  Returning
-- a precomputed top-level digest would accidentally benchmark sharing rather
-- than a fresh search.
{-# NOINLINE runEntry #-}
runEntry :: Entry -> ExferenceQuery -> Int
runEntry entry query = case
    findGeneratedSearchBatchesInEnvironmentEither
      (entryEnvironment entry) query of
  Left failure -> error $ entryName entry ++ ": search failed: " ++ show failure
  Right batches -> case entryDemand entry of
    FirstCandidate -> candidateDigest $ firstCandidate label batches
    CandidatePrefix count -> prefixDigest label count batches
    FirstCaseCandidate ->
      let candidate = firstCandidate label batches
          output = Candidate.candidateOutput candidate
      in if containsCase output
          then candidateDigest candidate
          else error $ label ++ ": first candidate did not contain a case"
    FirstConstraintFreeCandidate ->
      let candidate = firstCandidate label batches
      in if null $ Candidate.candidateResidualConstraints candidate
          then candidateDigest candidate
          else error $ label ++ ": ground instance left a residual constraint"
    BoundedNoCandidates -> boundedNoCandidateDigest
      label (queryMaximumSteps query) batches
 where
  label = entryName entry

firstCandidate
  :: String
  -> [ExferenceGeneratedSearchBatch]
  -> ExferenceGeneratedCandidate
firstCandidate label batches = case
    concatMap Search.batchCandidates batches of
  candidate : _ -> candidate
  [] -> error $ label ++ ": expected a candidate"

prefixDigest
  :: String
  -> Int
  -> [ExferenceGeneratedSearchBatch]
  -> Int
prefixDigest label requested batches
  | requested <= 0 = error $ label ++ ": non-positive candidate prefix"
  | found /= requested = error $ label ++ ": expected " ++ show requested
      ++ " candidates, found " ++ show found
  | otherwise = List.foldl'
      (\total candidate -> total + candidateDigest candidate)
      0 selected
 where
  selected = take requested $ concatMap Search.batchCandidates batches
  found = length selected

candidateDigest :: ExferenceGeneratedCandidate -> Int
candidateDigest candidate =
  Generated.expressionSize (Candidate.candidateOutput candidate)
  + length (Candidate.candidateResidualConstraints candidate)
  + exference_steps
      (exferenceCandidateStats $ Candidate.candidateDetails candidate)

boundedNoCandidateDigest
  :: String
  -> Int
  -> [ExferenceGeneratedSearchBatch]
  -> Int
boundedNoCandidateDigest label expectedSteps = go 0 0 Nothing 0
 where
  go
    :: Int
    -> Int
    -> Maybe Search.Progress
    -> Natural
    -> [ExferenceGeneratedSearchBatch]
    -> Int
  go !count !checksum latestProgress latestQueuePruned []
    | count /= expectedSteps = error $ label ++ ": expected "
        ++ show expectedSteps ++ " batches, observed " ++ show count
    | latestQueuePruned == 0 = error $ label
        ++ ": cyclic frontier never exercised queue pruning"
    | Just progress <- latestProgress
    , stepAndQueueLimited progress =
        checksum + count + fromIntegral latestQueuePruned
    | otherwise = error $ label
        ++ ": terminal progress lost its step/queue truncation"
  go !count !checksum _ _ (batch : batches)
    | not $ null $ Search.batchCandidates batch = error $ label
        ++ ": negative fixture unexpectedly produced a candidate"
    | otherwise = go
        (count + 1)
        (checksum + batchDigest batch)
        (Just $ Search.batchProgress batch)
        (exferenceQueuePruned $ Search.batchMetadata batch)
        batches

batchDigest :: ExferenceGeneratedSearchBatch -> Int
batchDigest batch =
  progressDigest (Search.batchProgress batch)
  + fromIntegral (exferenceQueuePruned metadata)
  + fromIntegral (exferenceDepthPruned metadata)
 where
  metadata = Search.batchMetadata batch

progressDigest :: Search.Progress -> Int
progressDigest Search.Continuing = 1
progressDigest (Search.Completed Search.Finished) = 2
progressDigest (Search.Completed (Search.Truncated reasons)) =
  3 + NonEmpty.length reasons

stepAndQueueLimited :: Search.Progress -> Bool
stepAndQueueLimited
    (Search.Completed (Search.Truncated reasons)) =
  Search.StepLimitReached `elem` reasonList
    && any queuePruned reasonList
 where
  reasonList = NonEmpty.toList reasons
  queuePruned (Search.QueueLimitPruned count) = count > 0
  queuePruned _ = False
stepAndQueueLimited _ = False

containsCase :: Generated.Expression local -> Bool
containsCase expression = case expression of
  Generated.Lambda _ body -> containsCase body
  Generated.Apply function argument ->
    containsCase function || containsCase argument
  Generated.Tuple elements -> any containsCase elements
  Generated.Let _ value body -> containsCase value || containsCase body
  Generated.Case{} -> True
  Generated.Local{} -> False
  Generated.Global{} -> False
  Generated.Hole{} -> False
