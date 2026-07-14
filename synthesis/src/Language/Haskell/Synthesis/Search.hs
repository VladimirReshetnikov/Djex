{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Backend-independent operational search status.
--
-- These values say whether a configured exploration finished or stopped at
-- resource bounds. They intentionally do not claim that a type is inhabited
-- or uninhabited: Djinn may attach proof-backed logical evidence, whereas an
-- Exference search reports only what its heuristic exploration established.
module Language.Haskell.Synthesis.Search
  ( TruncationReason (..)
  , Completion (..)
  , Progress (..)
  , ObservedProgress (..)
  , observeProgress
  , SearchBatch (..)
  , truncated
  ) where

import Control.DeepSeq (NFData)
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

-- | A distinct reason why unexplored work may remain.
data TruncationReason
  = StepLimitReached
  | ChoicePointLimitReached
  | CandidateLimitReached
  | QueueLimitPruned Natural
  | DepthLimitPruned Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData TruncationReason

-- | Terminal operational status for one configured search policy.
data Completion
  = Finished
  | Truncated (NonEmpty TruncationReason)
  deriving (Eq, Ord, Show, Generic)

instance NFData Completion

-- | A lazy or incremental search is either still producing batches or has a
-- terminal 'Completion'.
data Progress
  = Continuing
  | Completed Completion
  deriving (Eq, Ord, Show, Generic)

instance NFData Progress

-- | A lossless frontend-oriented view of the last progress value actually
-- observed.
--
-- In particular, 'ObservedContinuing' does not inspect an incremental trace
-- for a later terminal batch, and 'ObservedFinished' makes no inhabitation
-- claim. Logical evidence remains in the accompanying query result.
data ObservedProgress
  = NoProgressObserved
    -- ^ No search batch was inspected.
  | ObservedContinuing
    -- ^ The last inspected batch reported that more batches may follow.
  | ObservedFinished
    -- ^ The configured exploration finished without a resource truncation.
  | ObservedTruncated (NonEmpty TruncationReason)
    -- ^ The configured exploration stopped with the retained reasons.
  deriving (Eq, Ord, Show, Generic)

instance NFData ObservedProgress

-- | Classify supplied optional progress without looking beyond that
-- observation or interpreting the backend's logical evidence.
observeProgress :: Maybe Progress -> ObservedProgress
observeProgress Nothing = NoProgressObserved
observeProgress (Just Continuing) = ObservedContinuing
observeProgress (Just (Completed Finished)) = ObservedFinished
observeProgress (Just (Completed (Truncated reasons))) =
  ObservedTruncated reasons

-- | Common envelope for a chunk of backend-owned candidates and metadata.
-- The candidate parameter is last so ordinary 'fmap' transforms candidates
-- without touching progress or statistics.
data SearchBatch metadata candidate = SearchBatch
  { batchProgress :: Progress
  , batchMetadata :: metadata
  , batchCandidates :: [candidate]
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (NFData metadata, NFData candidate) =>
    NFData (SearchBatch metadata candidate)

truncated :: TruncationReason -> Completion
truncated reason = Truncated (reason :| [])
