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
  , timedParallelPairEligible
  , monotonicParallelDeadlineAfterSeconds
  , monotonicParallelDeadlineAfterSecondsWith
  , runParallelPairOrdered
  , runCheckedParallelPairOrderedBefore
  , runParallelPairOrderedBefore
  , runParallelPairOrderedBeforeWithPublicationHook
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
  ( Async
  , cancelWith
  , poll
  , wait
  , waitCatch
  , waitEitherCatch
  , withAsync
  )
import Control.Concurrent.MVar
  ( MVar
  , modifyMVar
  , newEmptyMVar
  , newMVar
  , readMVar
  , tryPutMVar
  , tryReadMVar
  )
import Control.DeepSeq (NFData (rnf), force)
import Control.Exception
  ( Exception
  , SomeAsyncException
  , SomeException
  , evaluate
  , fromException
  , mask
  , throwIO
  , tryJust
  )
import Control.Monad (unless)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

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

-- This token belongs only to one lane arbiter's deadline decision.  Matching
-- it exactly after 'cancelWith' prevents an unrelated asynchronous exception
-- raised by the worker from being silently reclassified as a timeout.
data ParallelLaneDeadlineExpired = ParallelLaneDeadlineExpired
  deriving (Show)

instance Exception ParallelLaneDeadlineExpired

-- | Whether an untimed two-backend command may use the original strict
-- buffered worker path. Timed commands have a distinct absolute-deadline
-- predicate below; streaming selection retains its serial IO boundary.
parallelPairEligible :: Int -> Bool -> Bool -> Bool
parallelPairEligible jobs hasTimeout streamsResults =
  jobs >= 2 && not hasTimeout && not streamsResults

-- | Whether a timed two-backend command may use the absolute-deadline strict
-- buffered path.  This is separate from 'parallelPairEligible' so the
-- established untimed route and its eligibility contract remain unchanged.
timedParallelPairEligible :: Int -> Bool -> Bool -> Bool
timedParallelPairEligible jobs hasTimeout streamsResults =
  jobs >= 2 && hasTimeout && not streamsResults

-- | Capture one absolute elapsed-time cutoff for a positive whole-second
-- budget.  A private epoch extender samples the bounded GHC monotonic clock
-- at least once per observation chunk and accumulates modular deltas into an
-- 'Integer'.  Accepted multi-century budgets therefore cannot create an
-- unreachable cutoff after the underlying 'Word64' clock wraps.  The watcher
-- always recomputes the delay remaining to the original cutoff; an early or
-- late wake cannot refresh the budget.
monotonicParallelDeadlineAfterSeconds
  :: Int
  -> IO (ParallelDeadline Integer)
monotonicParallelDeadlineAfterSeconds =
  monotonicParallelDeadlineAfterSecondsWith getMonotonicTimeNSec threadDelay

-- | Package-private deterministic seam for the monotonic deadline
-- constructor.  Tests supply a logical clock and delay action instead of
-- depending on scheduler timing or wall-clock thresholds.
monotonicParallelDeadlineAfterSecondsWith
  :: IO Word64
  -> (Int -> IO ())
  -> Int
  -> IO (ParallelDeadline Integer)
monotonicParallelDeadlineAfterSecondsWith readNow delay seconds = do
  startedAt <- readNow
  clockState <- newMVar (startedAt, 0 :: Integer)
  let cutoff = toInteger seconds * nanosecondsPerSecond
      readElapsed = modifyMVar clockState $ \(previous, elapsed) -> do
        current <- readNow
        let next = elapsed + toInteger (current - previous)
        pure ((current, next), next)
  pure ParallelDeadline
    { parallelDeadlineCutoff = cutoff
    , parallelDeadlineNow = readElapsed
    , parallelDeadlineAwait = awaitCutoff readElapsed cutoff
    }
 where
  awaitCutoff readElapsed cutoff = do
    now <- readElapsed
    let remaining = cutoff - now
    if remaining <= 0
      then pure ()
      else do
        delay $ nanosecondsToDelayMicroseconds remaining
        awaitCutoff readElapsed cutoff

nanosecondsPerSecond :: Integer
nanosecondsPerSecond = 1000000000

nanosecondsToDelayMicroseconds :: Integer -> Int
nanosecondsToDelayMicroseconds nanoseconds = fromInteger $ min
  maximumClockObservationDelayMicroseconds
  ((nanoseconds + nanosecondsPerMicrosecond - 1)
    `div` nanosecondsPerMicrosecond)

-- Thirty minutes is below the 32-bit 'threadDelay' ceiling and many orders of
-- magnitude below one Word64 nanosecond cycle.  Rechecking at this cadence
-- lets the epoch extender account for any number of clock wraps over a large
-- validated query budget.
maximumClockObservationDelayMicroseconds :: Integer
maximumClockObservationDelayMicroseconds = 1800000000

nanosecondsPerMicrosecond :: Integer
nanosecondsPerMicrosecond = 1000

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

-- | Complete both checked request admissions, capture one deadline, and only
-- then start the two strict lanes.  Keeping this ordering in the private
-- scheduler makes it directly characterizable without a wall-clock test and
-- prevents either request check from drifting under the search budget.
runCheckedParallelPairOrderedBefore
  :: (Ord instant, NFData left, NFData right)
  => IO checkedLeft
  -> IO checkedRight
  -> IO (ParallelDeadline instant)
  -> (checkedLeft -> IO left)
  -> (checkedRight -> IO right)
  -> (ParallelLaneOutcome left -> IO ())
  -> (ParallelLaneOutcome right -> IO ())
  -> IO ()
runCheckedParallelPairOrderedBefore
    checkLeft checkRight captureDeadline left right consumeLeft consumeRight = do
  checkedLeft <- checkLeft
  checkedRight <- checkRight
  deadline <- captureDeadline
  runParallelPairOrderedBefore deadline
    (left checkedLeft)
    (right checkedRight)
    consumeLeft
    consumeRight

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
  case fromException exception :: Maybe ParallelLaneDeadlineExpired of
    Just _ -> Nothing
    Nothing -> case fromException exception :: Maybe SomeAsyncException of
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
            cancelWith laneWorker ParallelLaneDeadlineExpired
            stopped <- waitCatch laneWorker
            terminal <- tryReadMVar terminalCell
            case terminal of
              Just completed -> classifyLaneTerminal deadline completed
              Nothing -> case stopped of
                Left exception
                  | isDeadlineExpiration exception ->
                      pure ParallelLaneTimedOut
                  | otherwise -> throwIO exception
                Right () -> throwIO $ userError
                  "Djex parallel lane stopped without a terminal value"

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

isDeadlineExpiration :: SomeException -> Bool
isDeadlineExpiration exception = case
    fromException exception :: Maybe ParallelLaneDeadlineExpired of
  Just _ -> True
  Nothing -> False
