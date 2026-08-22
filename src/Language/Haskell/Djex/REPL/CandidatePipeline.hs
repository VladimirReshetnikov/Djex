-- | Bounded producer/owner overlap for behavioral best-candidate selection.
--
-- The producer advances the lazy candidate trace and performs only the
-- caller-supplied preparation action.  The calling thread remains the sole
-- owner of admission, ranking, batch commits, and the returned selection.
-- One permit bounds the producer to a single candidate ahead of that owner.
module Language.Haskell.Djex.REPL.CandidatePipeline
  ( behavioralBestPipelineEligible
  , selectBestQueryResultsPipelinedM
  ) where

import Control.Concurrent.Chan
  ( Chan
  , newChan
  , readChan
  , writeChan
  )
import Control.Concurrent.MVar
  ( MVar
  , newMVar
  , putMVar
  , takeMVar
  )
import Control.Concurrent.Async (withAsync)
import Control.Exception
  ( SomeException
  , mask
  , throwIO
  , try
  )
import qualified Data.List as List

import Language.Haskell.Synthesis.Query (QueryResult, resultSearch)
import Language.Haskell.Synthesis.Search
  ( Progress
  , batchCandidates
  , batchProgress
  )
import Language.Haskell.Synthesis.Selection
  ( Selection (..)
  , SelectionMode (SelectBest)
  )

-- | Whether a command may use the behavioral candidate pipeline.
--
-- A second configured search job authorizes preparation to overlap the
-- owner-thread admission/ranking work.  This deliberately does not inspect
-- RTS capabilities: @jobs=2@ remains eligible under @+RTS -N1@ as a semantic
-- and performance control.  Only the unbounded whole-trace best policy has
-- the batch and demand contract implemented by this foundation.
behavioralBestPipelineEligible :: Int -> SelectionMode -> Bool
behavioralBestPipelineEligible searchJobs mode =
  searchJobs >= 2 && mode == SelectBest

-- The constructor fields intentionally remain lazy.  Candidate forcing is
-- exclusively the caller-supplied producer preparation action, while progress
-- and rank retain the serial selector's owner-thread demand points.
data CandidatePipelineEvent candidate
  = Candidate candidate
  | NonEmptyBatchEnd Progress
  | Terminal (Either SomeException (Maybe Progress))

-- | Select the globally minimal-rank admissible candidates while overlapping
-- preparation of at most one later candidate with owner-thread work.
--
-- This has the behavioral contract of monadic 'SelectBest' with preparation
-- inserted immediately before each candidate's admission.  Admission still
-- completes for a whole batch before any rank in that batch is demanded.
-- Producer exceptions are published behind every earlier event, so the owner
-- observes them only at the point where the corresponding serial traversal
-- would next demand the trace.  'withAsync' scopes every exit through producer
-- cancellation and join.
selectBestQueryResultsPipelinedM
  :: Ord rank
  => (candidate -> IO ())
  -> (candidate -> rank)
  -> (candidate -> IO Bool)
  -> [QueryResult metadata candidate]
  -> IO (Selection candidate)
selectBestQueryResultsPipelinedM prepare rank admissible results = do
  events <- newChan
  candidatePermit <- newMVar ()
  withAsync
      (produceCandidateEvents prepare candidatePermit events results) $
      \_producer -> consumeCandidateEvents
        rank admissible candidatePermit events Nothing Nothing []

produceCandidateEvents
  :: (candidate -> IO ())
  -> MVar ()
  -> Chan (CandidatePipelineEvent candidate)
  -> [QueryResult metadata candidate]
  -> IO ()
produceCandidateEvents prepare candidatePermit events results =
  mask $ \restore -> do
    outcome <- try $ restore $ produceResults Nothing results
    -- Publication remains masked after success, synchronous failure, or
    -- cancellation so a terminal observation cannot be lost between catch
    -- and the FIFO write.
    writeChan events $ Terminal outcome
 where
  produceResults progress [] = pure progress
  produceResults _ (result : remainingResults) = do
    let batch = resultSearch result
        progress = batchProgress batch
    nonEmpty <- produceBatch False $ batchCandidates batch
    if nonEmpty
      then writeChan events $ NonEmptyBatchEnd progress
      else pure ()
    produceResults (Just progress) remainingResults

  -- Take the permit before inspecting the candidate spine.  Consequently the
  -- producer cannot even discover a second successor until the owner dequeues
  -- the outstanding candidate and returns the permit.
  produceBatch nonEmpty candidates = do
    takeMVar candidatePermit
    case candidates of
      [] -> do
        putMVar candidatePermit ()
        pure nonEmpty
      candidate : remainingCandidates -> do
        prepare candidate
        writeChan events $ Candidate candidate
        produceBatch True remainingCandidates

consumeCandidateEvents
  :: Ord rank
  => (candidate -> rank)
  -> (candidate -> IO Bool)
  -> MVar ()
  -> Chan (CandidatePipelineEvent candidate)
  -> Maybe Progress
  -> Maybe (rank, [candidate])
  -> [candidate]
  -> IO (Selection candidate)
consumeCandidateEvents rank admissible candidatePermit events = go
 where
  go progress best admittedInBatch = do
    event <- readChan events
    case event of
      Candidate candidate -> do
        -- Releasing before admission is the sole overlap window: preparation
        -- of one successor may run while the owner checks this candidate.
        putMVar candidatePermit ()
        admitted <- admissible candidate
        go progress best $
          if admitted then candidate : admittedInBatch else admittedInBatch
      NonEmptyBatchEnd observedProgress -> do
        let nextProgress = Just observedProgress
            nextBest = List.foldl' (consider rank) best
              $ reverse admittedInBatch
        -- Match selectBestM: every admission in the batch precedes progress,
        -- whose Maybe constructor, but not payload, precedes every rank
        -- demanded by this batch commit.
        nextProgress `seq` nextBest `seq`
          go nextProgress nextBest []
      Terminal outcome -> case outcome of
        Left exception -> throwIO exception
        Right terminalProgress -> pure $ Selection terminalProgress
          $ reverse $ maybe [] snd best

consider
  :: Ord rank
  => (candidate -> rank)
  -> Maybe (rank, [candidate])
  -> candidate
  -> Maybe (rank, [candidate])
consider rank best candidate = addRanked (rank candidate) candidate best

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
