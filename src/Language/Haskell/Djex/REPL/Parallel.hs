-- | Scoped two-lane execution with deterministic observation order.
--
-- Both actions start before either result is observed.  Each result is forced
-- completely in its worker.  The left consumer runs before the right result is
-- observed, regardless of completion order.  'withAsync' supplies masked
-- spawning plus cancel-and-wait cleanup when a worker, consumer, or caller
-- raises an exception.
module Language.Haskell.Djex.REPL.Parallel
  ( ParallelDeadline (..)
  , ParallelLaneOutcome (..)
  , parallelPairEligible
  , runParallelPairOrdered
  , runParallelPairOrderedBefore
  , runParallelPairOrderedBeforeWithPublicationHook
  ) where

import Control.Concurrent.Async
  ( Async
  , cancel
  , poll
  , wait
  , waitEitherCatch
  , withAsync
  )
import Control.Concurrent.MVar
  ( MVar
  , newEmptyMVar
  , readMVar
  , tryPutMVar
  , tryReadMVar
  )
import Control.DeepSeq (NFData (rnf), force)
import Control.Exception
  ( SomeAsyncException
  , SomeException
  , evaluate
  , fromException
  , mask
  , throwIO
  , tryJust
  )
import Control.Monad (unless)

-- | One caller-owned monotonic cutoff and the single signal which becomes
-- ready at that absolute cutoff.  The clock and signal are explicit so the
-- private runner can later share a frontend deadline without refreshing it;
-- deterministic tests can supply logical time instead of sleeping.
--
-- The clock must use the same monotonic time domain as the cutoff and must not
-- block: it runs inside the masked stamp-and-publication section.  The signal
-- action is run exactly once for the pair.
data ParallelDeadline instant = ParallelDeadline
  { parallelDeadlineCutoff :: !instant
  , parallelDeadlineNow :: IO instant
  , parallelDeadlineAwait :: IO ()
  }

-- | Strict, lane-local result of an absolute-deadline race.  An ordinary
-- checked failure remains inside 'ParallelLaneCompleted'; only failure to
-- produce and force the lane value strictly before the cutoff is a timeout.
data ParallelLaneOutcome value
  = ParallelLaneCompleted !value
  | ParallelLaneTimedOut
  deriving (Eq, Show)

instance NFData value => NFData (ParallelLaneOutcome value) where
  rnf outcome = case outcome of
    ParallelLaneCompleted value -> rnf value
    ParallelLaneTimedOut -> ()

-- The terminal cell is published by the strict worker before its 'Async'
-- result can become observable.  It closes the scheduler gap between a
-- pre-cutoff stamp and the outer async result publication.  Synchronous
-- worker failures use the same cell so deadline cancellation cannot silently
-- turn an already-produced failure into a timeout.
data StrictLaneTerminal instant value
  = StrictLaneCompletedAt !instant !value
  | StrictLaneFailed !SomeException

-- | Whether a two-backend command may use the strict buffered worker path.
-- Timed commands and streaming selection deliberately retain their original
-- serial IO boundaries.
parallelPairEligible :: Int -> Bool -> Bool -> Bool
parallelPairEligible jobs hasTimeout streamsResults =
  jobs >= 2 && not hasTimeout && not streamsResults

-- | Start two actions together, force their results in their own workers, and
-- consume them in stable left-to-right order.
runParallelPairOrdered
  :: (NFData left, NFData right)
  => IO left
  -> IO right
  -> (left -> IO ())
  -> (right -> IO ())
  -> IO ()
runParallelPairOrdered left right consumeLeft consumeRight =
  withAsync (left >>= evaluate . force) $ \leftWorker ->
    withAsync (right >>= evaluate . force) $ \rightWorker -> do
      wait leftWorker >>= consumeLeft
      wait rightWorker >>= consumeRight

-- | Run two strict lanes against one absolute monotonic deadline and consume
-- their outcomes in stable left-to-right order.
--
-- Both strict workers are forked before either result is observed.  A shared
-- deadline watcher is forked once, while independent lane arbiters race each
-- worker against that watcher.  Expiry cancels and joins only the unfinished
-- worker belonging to that arbiter.  A completed value is successful exactly
-- when its completion stamp is strictly less than the cutoff, so equality is
-- resolved in favour of timeout.  Worker exceptions are not timeout values;
-- they retain the same left-first observation and scoped sibling cleanup as
-- 'runParallelPairOrdered'.
runParallelPairOrderedBefore
  :: (Ord instant, NFData left, NFData right)
  => ParallelDeadline instant
  -> IO left
  -> IO right
  -> (ParallelLaneOutcome left -> IO ())
  -> (ParallelLaneOutcome right -> IO ())
  -> IO ()
runParallelPairOrderedBefore deadline =
  runParallelPairOrderedBeforeWithPublicationHook deadline $ pure ()

-- | Package-private test seam for opening the otherwise tiny interval after
-- a lane has published its terminal cell and before its 'Async' has returned.
-- Production callers use 'runParallelPairOrderedBefore', whose hook is a
-- no-op.  The hook runs outside the masked stamp-and-publication section.
runParallelPairOrderedBeforeWithPublicationHook
  :: (Ord instant, NFData left, NFData right)
  => ParallelDeadline instant
  -> IO ()
  -> IO left
  -> IO right
  -> (ParallelLaneOutcome left -> IO ())
  -> (ParallelLaneOutcome right -> IO ())
  -> IO ()
runParallelPairOrderedBeforeWithPublicationHook
    deadline afterCompletionPublished left right consumeLeft consumeRight = do
  leftTerminal <- newEmptyMVar
  rightTerminal <- newEmptyMVar
  withAsync
      (runStrictLane deadline leftTerminal afterCompletionPublished left) $
      \leftWorker ->
    withAsync
        (runStrictLane deadline rightTerminal afterCompletionPublished right) $
        \rightWorker ->
      withAsync (parallelDeadlineAwait deadline) $ \deadlineWorker ->
        withAsync
            (awaitLane deadline deadlineWorker leftTerminal leftWorker) $
          \leftArbiter ->
            withAsync
                (awaitLane deadline deadlineWorker rightTerminal rightWorker) $
              \rightArbiter -> do
                wait leftArbiter >>= consumeLeft
                wait rightArbiter >>= consumeRight

runStrictLane
  :: NFData value
  => ParallelDeadline instant
  -> MVar (StrictLaneTerminal instant value)
  -> IO ()
  -> IO value
  -> IO ()
runStrictLane deadline terminalCell afterCompletionPublished action =
  mask $ \restore -> do
    result <- tryJust synchronousException $
      restore $ action >>= evaluate . force
    terminal <- case result of
      Left exception -> evaluate $ StrictLaneFailed exception
      Right value -> do
        completedAt <- parallelDeadlineNow deadline
        evaluate $ StrictLaneCompletedAt completedAt value
    published <- tryPutMVar terminalCell terminal
    unless published $ throwIO $ userError
      "Djex parallel lane terminal cell was already populated"
    restore afterCompletionPublished

synchronousException :: SomeException -> Maybe SomeException
synchronousException exception =
  case fromException exception :: Maybe SomeAsyncException of
    Just _ -> Nothing
    Nothing -> Just exception

awaitLane
  :: Ord instant
  => ParallelDeadline instant
  -> Async ()
  -> MVar (StrictLaneTerminal instant value)
  -> Async ()
  -> IO (ParallelLaneOutcome value)
awaitLane deadline deadlineWorker terminalCell laneWorker = do
  first <- waitEitherCatch laneWorker deadlineWorker
  case first of
    Left laneResult ->
      observeLaneWorker deadline terminalCell laneResult
    Right deadlineResult -> case deadlineResult of
      Left exception -> throwIO exception
      Right () -> do
        laneResult <- poll laneWorker
        case laneResult of
          Just completed ->
            observeLaneWorker deadline terminalCell completed
          Nothing -> do
            cancel laneWorker
            terminal <- tryReadMVar terminalCell
            case terminal of
              Just completed -> classifyLaneTerminal deadline completed
              Nothing -> do
                stopped <- poll laneWorker
                case stopped of
                  Just (Left exception)
                    | isAsynchronousException exception ->
                        pure ParallelLaneTimedOut
                    | otherwise -> throwIO exception
                  Just (Right ()) -> throwIO $ userError
                    "Djex parallel lane stopped without a terminal value"
                  Nothing -> throwIO $ userError
                    "Djex parallel lane cancellation did not join the worker"

observeLaneWorker
  :: Ord instant
  => ParallelDeadline instant
  -> MVar (StrictLaneTerminal instant value)
  -> Either SomeException ()
  -> IO (ParallelLaneOutcome value)
observeLaneWorker deadline terminalCell result = case result of
  Left exception -> throwIO exception
  Right () -> readMVar terminalCell >>= classifyLaneTerminal deadline

classifyLaneTerminal
  :: Ord instant
  => ParallelDeadline instant
  -> StrictLaneTerminal instant value
  -> IO (ParallelLaneOutcome value)
classifyLaneTerminal deadline terminal = case terminal of
  StrictLaneFailed exception -> throwIO exception
  StrictLaneCompletedAt completedAt value
    | completedAt < parallelDeadlineCutoff deadline ->
        evaluate $ ParallelLaneCompleted value
    | otherwise -> pure ParallelLaneTimedOut

isAsynchronousException :: SomeException -> Bool
isAsynchronousException exception = case
    fromException exception :: Maybe SomeAsyncException of
  Just _ -> True
  Nothing -> False
