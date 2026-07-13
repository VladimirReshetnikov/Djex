{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Backend-neutral presentation policies for a lazy query-result trace.
--
-- Search engines decide which candidates are sound and how batches are
-- produced.  This module performs the smaller, reusable job of selecting the
-- candidates a frontend should present.  Ranking and admissibility remain
-- caller-supplied so, for example, a frontend can exclude residual constraints
-- without teaching the synthesis foundation what those constraints mean.
module Language.Haskell.Synthesis.Selection
  ( SelectionMode (..)
  , Selection (..)
  , selectQueryResults
  , selectPreferredQueryResults
  ) where

import Control.DeepSeq (NFData)
import qualified Data.List as List
import GHC.Generics (Generic)

import Language.Haskell.Synthesis.Query (QueryResult, resultSearch)
import Language.Haskell.Synthesis.Search
  ( Progress
  , batchCandidates
  , batchProgress
  )

-- | How candidates should be selected from a query-result trace.
data SelectionMode
  = SelectFirst
    -- ^ Stop at the first admissible candidate.
  | SelectBest
    -- ^ Inspect the whole trace and retain every candidate with the globally
    -- minimal rank.
  | SelectBestLookahead Int
    -- ^ Retain the current minimal-rank candidates, stopping after this many
    -- subsequent batches without a strictly better rank.  A better candidate
    -- resets the batch budget.  Empty batches and batches containing only
    -- inadmissible, equal, or worse candidates consume one unit; batches before
    -- the first admissible candidate do not.  A non-positive budget therefore
    -- stops after the first batch containing an admissible candidate.
  | SelectAll
    -- ^ Retain every admissible candidate in encounter order.
  deriving (Eq, Ord, Show, Generic)

instance NFData SelectionMode

-- | The candidates chosen by a presentation policy and the progress of the
-- last batch that policy actually inspected.
--
-- An empty input has no progress.  A nonempty trace that contains no
-- admissible candidates retains the final inspected progress, which lets a
-- frontend distinguish an exhausted search from an empty inspected prefix.
data Selection candidate = Selection
  { selectionProgress :: Maybe Progress
  , selectionCandidates :: [candidate]
  }
  deriving
    ( Eq
    , Ord
    , Show
    , Functor
    , Foldable
    , Traversable
    , Generic
    )

instance NFData candidate => NFData (Selection candidate)

-- | Apply a presentation policy to a lazy query-result trace.
--
-- 'SelectFirst' stops without forcing the uninspected suffix.  'SelectAll'
-- keeps its candidate list streaming; evaluating 'selectionProgress'
-- separately necessarily inspects the trace to its end.  Both best policies
-- must inspect through their stopping point before they can establish their
-- result.
selectQueryResults
  :: Ord rank
  => SelectionMode
  -> (candidate -> rank)
  -> (candidate -> Bool)
  -> [QueryResult metadata candidate]
  -> Selection candidate
selectQueryResults mode rank admissible results = case mode of
  SelectFirst -> selectFirst admissible results
  SelectBest -> selectBest rank admissible results
  SelectBestLookahead count ->
    selectBestLookahead (max 0 count) rank admissible results
  SelectAll -> Selection
    { selectionProgress = lastInspectedProgress results
    , selectionCandidates =
        concatMap (filter admissible . batchCandidates . resultSearch) results
    }

-- | Select a best preferred tier in one pass over a lazy result trace.
--
-- The fourth argument classifies an admissible candidate as preferred.
-- Before the first preferred candidate appears, the selector retains every
-- globally minimal-rank fallback candidate.  The first batch containing a
-- preferred candidate discards that fallback regardless of relative rank;
-- from then on, only preferred candidates participate in the same
-- batch-counted lookahead policy as 'SelectBestLookahead'.
--
-- This is useful when a frontend prefers candidates without residual
-- obligations but still wants constrained candidates as a fallback.  It does
-- not have to inspect the lazy trace once to detect a preferred candidate and
-- again to recover a fallback.
selectPreferredQueryResults
  :: Ord rank
  => Int
  -> (candidate -> rank)
  -> (candidate -> Bool)
  -> (candidate -> Bool)
  -> [QueryResult metadata candidate]
  -> Selection candidate
selectPreferredQueryResults lookahead rank admissible preferred =
    beforePreferred Nothing Nothing
 where
  maximumLookahead = max 0 lookahead

  beforePreferred progress fallback [] =
    Selection progress $ reverse $ maybe [] snd fallback
  beforePreferred _ fallback (result : results) =
    let batch = resultSearch result
        progress = Just $ batchProgress batch
        (batchFallback, batchPreferred) = bestTiersInBatch
          rank admissible preferred $ batchCandidates batch
    in case batchPreferred of
      Just (preferredRank, reversed) -> continueBestLookahead
        maximumLookahead rank (\candidate ->
          admissible candidate && preferred candidate)
        progress maximumLookahead preferredRank reversed results
      Nothing -> beforePreferred progress
        (mergeBest fallback batchFallback) results

selectFirst
  :: (candidate -> Bool)
  -> [QueryResult metadata candidate]
  -> Selection candidate
selectFirst admissible = go Nothing
 where
  go progress [] = Selection progress []
  go _ (result : results) =
    let batch = resultSearch result
        progress = Just $ batchProgress batch
    in case List.find admissible $ batchCandidates batch of
      Just candidate -> Selection progress [candidate]
      Nothing -> go progress results

selectBest
  :: Ord rank
  => (candidate -> rank)
  -> (candidate -> Bool)
  -> [QueryResult metadata candidate]
  -> Selection candidate
selectBest rank admissible = finish
  . List.foldl' inspect (Nothing, Nothing)
 where
  finish (progress, best) = Selection progress
    $ reverse $ maybe [] snd best

  inspect (_, best) result =
    let batch = resultSearch result
        progress = Just $ batchProgress batch
        nextBest = List.foldl' (consider rank admissible) best
          $ batchCandidates batch
    -- 'foldl'' only forces the pair constructor. Force both fields here so
    -- an empty run of batches cannot leave a chain of deferred accumulators.
    in progress `seq` nextBest `seq` (progress, nextBest)

selectBestLookahead
  :: Ord rank
  => Int
  -> (candidate -> rank)
  -> (candidate -> Bool)
  -> [QueryResult metadata candidate]
  -> Selection candidate
selectBestLookahead maximumLookahead rank admissible =
    beforeFirst Nothing
 where
  -- Batches before the first admissible candidate establish no rank against
  -- which "without improvement" could be measured, so they do not spend the
  -- lookahead budget.
  beforeFirst progress [] = Selection progress []
  beforeFirst _ (result : results) =
    let batch = resultSearch result
        progress = Just $ batchProgress batch
    in case bestInBatch rank admissible $ batchCandidates batch of
      Nothing -> beforeFirst progress results
      Just (bestRank, reversed) -> continueBestLookahead
        maximumLookahead rank admissible progress maximumLookahead
        bestRank reversed results

continueBestLookahead
  :: Ord rank
  => Int
  -> (candidate -> rank)
  -> (candidate -> Bool)
  -> Maybe Progress
  -> Int
  -> rank
  -> [candidate]
  -> [QueryResult metadata candidate]
  -> Selection candidate
continueBestLookahead _ _ _ progress _ _ reversed [] =
  Selection progress $ reverse reversed
continueBestLookahead _ _ _ progress 0 _ reversed _ =
  Selection progress $ reverse reversed
continueBestLookahead maximumLookahead rank admissible _ remaining
    bestRank reversed (result : results) =
  let batch = resultSearch result
      progress = Just $ batchProgress batch
  in case bestInBatch rank admissible $ batchCandidates batch of
    Just (candidateRank, candidates)
      | candidateRank < bestRank -> continueBestLookahead
          maximumLookahead rank admissible progress maximumLookahead
          candidateRank candidates results
      | candidateRank == bestRank -> continueBestLookahead
          maximumLookahead rank admissible progress (remaining - 1)
          bestRank (candidates ++ reversed) results
    _ -> continueBestLookahead maximumLookahead rank admissible progress
      (remaining - 1) bestRank reversed results

-- Accumulate candidates in reverse encounter order, replacing the previous
-- group when a strictly better rank is seen.
consider
  :: Ord rank
  => (candidate -> rank)
  -> (candidate -> Bool)
  -> Maybe (rank, [candidate])
  -> candidate
  -> Maybe (rank, [candidate])
consider rank admissible best candidate
  | not $ admissible candidate = best
  | otherwise = case best of
      Nothing -> Just (candidateRank, [candidate])
      Just (bestRank, reversed) -> case compare candidateRank bestRank of
        LT -> Just (candidateRank, [candidate])
        EQ -> Just (bestRank, candidate : reversed)
        GT -> best
 where
  candidateRank = rank candidate

bestInBatch
  :: Ord rank
  => (candidate -> rank)
  -> (candidate -> Bool)
  -> [candidate]
  -> Maybe (rank, [candidate])
bestInBatch rank admissible =
  List.foldl' (consider rank admissible) Nothing

-- Find both tiers in one traversal of a batch.  Candidates are retained in
-- reverse encounter order to make tied accumulation constant-time.
bestTiersInBatch
  :: Ord rank
  => (candidate -> rank)
  -> (candidate -> Bool)
  -> (candidate -> Bool)
  -> [candidate]
  -> (Maybe (rank, [candidate]), Maybe (rank, [candidate]))
bestTiersInBatch rank admissible preferred = List.foldl' inspect (Nothing, Nothing)
 where
  inspect tiers candidate
    | not $ admissible candidate = tiers
    | preferred candidate = case tiers of
        (fallback, preferredBest) ->
          (fallback, addRanked (rank candidate) candidate preferredBest)
    | otherwise = case tiers of
        (fallback, preferredBest) ->
          (addRanked (rank candidate) candidate fallback, preferredBest)

addRanked
  :: Ord rank
  => rank
  -> candidate
  -> Maybe (rank, [candidate])
  -> Maybe (rank, [candidate])
addRanked candidateRank candidate Nothing =
  Just (candidateRank, [candidate])
addRanked candidateRank candidate best@(Just (bestRank, reversed)) =
  case compare candidateRank bestRank of
    LT -> Just (candidateRank, [candidate])
    EQ -> Just (bestRank, candidate : reversed)
    GT -> best

-- Merge summaries from earlier and later batches while retaining reverse
-- encounter order for ties.
mergeBest
  :: Ord rank
  => Maybe (rank, [candidate])
  -> Maybe (rank, [candidate])
  -> Maybe (rank, [candidate])
mergeBest earlier Nothing = earlier
mergeBest Nothing later = later
mergeBest earlier@(Just (earlierRank, earlierReversed))
    later@(Just (laterRank, laterReversed)) =
  case compare laterRank earlierRank of
    LT -> later
    EQ -> Just (earlierRank, laterReversed ++ earlierReversed)
    GT -> earlier

lastInspectedProgress
  :: [QueryResult metadata candidate]
  -> Maybe Progress
lastInspectedProgress = List.foldl' inspect Nothing
 where
  inspect _ = Just . batchProgress . resultSearch
