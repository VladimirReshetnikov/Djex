module Main (main) where

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async
  ( Async
  , async
  , cancel
  , poll
  , wait
  )
import Control.Concurrent.MVar
  ( MVar
  , modifyMVar
  , modifyMVar_
  , newEmptyMVar
  , newMVar
  , putMVar
  , readMVar
  , takeMVar
  , tryReadMVar
  )
import Control.Exception
  ( AsyncException (UserInterrupt)
  , Exception
  , SomeException
  , catch
  , finally
  , fromException
  , throw
  , throwIO
  , try
  )
import Control.Monad (when)
import Data.Word (Word64)
import System.Exit (ExitCode (ExitSuccess))
import System.Timeout (timeout)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
  ( Assertion
  , assertBool
  , assertEqual
  , assertFailure
  , testCase
  )

import Language.Haskell.Djex.Command.Output
import Language.Haskell.Djex.REPL.CandidatePipeline
import Language.Haskell.Djex.REPL.Parallel
import Language.Haskell.Synthesis.Query
  ( QueryResult
  , queryResultFromCandidates
  )
import Language.Haskell.Synthesis.Search
  ( Completion (Finished)
  , Progress (Completed, Continuing)
  , SearchBatch (SearchBatch)
  )
import Language.Haskell.Synthesis.Selection
  ( Selection (..)
  , SelectionMode (..)
  , selectQueryResultsM
  )

main :: IO ()
main = defaultMain $ testGroup "Djex deterministic parallel pair"
  [ testCase "eligibility separates untimed, timed, and serial routes"
      testEligibility
  , testCase "monotonic deadlines retain one cutoff after early wakes"
      testMonotonicDeadline
  , testCase "monotonic deadlines extend across clock wrap and late wake"
      testMonotonicDeadlineWrap
  , testCase "maximum accepted budgets use bounded observation chunks"
      testMaximumMonotonicDeadline
  , testCase "both request checks precede cutoff capture and lane start"
      testCheckedPairSequence
  , testCase "both workers start before either is released"
      testSimultaneousStart
  , testCase "completion order cannot reorder consumers"
      testStableConsumptionOrder
  , testCase "ordinary failure values do not cancel their sibling"
      testOrdinaryFailureIsolation
  , testCase "a left exception cancels and joins the right worker"
      testLeftExceptionCleanup
  , testCase "a right exception waits for left observation"
      testRightExceptionOrdering
  , testCase "caller cancellation cancels and joins both workers"
      testCallerCancellation
  , testCase "worker forcing rejects a poisoned output plan before replay"
      testStrictOutputPlan
  , testCase "timed lanes share one cutoff signal and both start"
      testTimedCommonDeadlineStart
  , testCase "pre-cutoff completions survive deadline watcher cleanup"
      testTimedCompletionBeforeDeadline
  , testCase "published pre-cutoff completions survive worker cancellation"
      testTimedPublishedCompletionSurvivesCancellation
  , testCase "published worker failures survive deadline cancellation"
      testTimedPublishedFailureSurvivesCancellation
  , testCase "deadline expiry cancels and joins both lane-local workers"
      testTimedDeadlineExpiry
  , testCase "deadline cancellation cannot swallow another async exception"
      testTimedDeadlineCancellationException
  , testCase "deadline watcher failures propagate after worker cleanup"
      testTimedDeadlineWatcherFailure
  , testCase "completion exactly at the cutoff is a timeout"
      testTimedCutoffTie
  , testCase "a retained success is observed after its sibling timeout"
      testTimedSuccessAndTimeout
  , testCase "an ordinary failure value is retained beside a timeout"
      testTimedFailureAndTimeout
  , testCase "timed unexpected exceptions retain left-first precedence"
      testTimedExceptionPrecedence
  , testCase "a left timeout is observed before a right exception"
      testTimedTimeoutBeforeRightException
  , testCase "caller cancellation cleans timed workers and deadline watcher"
      testTimedCallerCancellation
  , testCase "timed worker forcing rejects a poisoned output plan"
      testTimedStrictOutputPlan
  , testCase "timed consumers remain stable when right completes first"
      testTimedStableConsumptionOrder
  , candidatePipelineTests
  ]

candidatePipelineTests :: TestTree
candidatePipelineTests = testGroup "bounded SelectBest candidate pipeline"
  [ testCase "eligibility admits only behavioral best with two search jobs"
      testCandidatePipelineEligibility
  , testCase "finite selection and owner action order match serial SelectBest"
      testCandidatePipelineSerialParity
  , testCase "preparation runs off-owner and admission stays on-owner"
      testCandidatePipelineThreadOwnership
  , testCase "one permit defers a poisoned successor until the next dequeue"
      testCandidatePipelineOneAhead
  , testCase "owner admission outranks an already-produced successor failure"
      testCandidatePipelineAdmissionExceptionPrecedence
  , testCase "whole-batch admission completes before any batch rank"
      testCandidatePipelineWholeBatchAdmission
  , testCase "producer failure inside a batch precedes partial-batch rank"
      testCandidatePipelineProducerBeforePartialRank
  , testCase "completed-batch rank precedes a later producer failure"
      testCandidatePipelineBatchBoundaryPrecedence
  , testCase "progress payload stays lazy behind batch ranking"
      testCandidatePipelineProgressDemand
  , testCase "empty batches coalesce while retaining terminal progress"
      testCandidatePipelineEmptyBatches
  , testCase "empty progress stays lazy while the result suffix is demanded"
      testCandidatePipelineEmptyProgressDemand
  , testCase "owner failure cancels and joins blocked preparation"
      testCandidatePipelineOwnerFailureCleanup
  , testCase "caller cancellation cancels and joins blocked preparation"
      testCandidatePipelineCallerCancellation
  ]

testCandidatePipelineEligibility :: Assertion
testCandidatePipelineEligibility = do
  assertBool "two search jobs enable unbounded behavioral best" $
    behavioralBestPipelineEligible 2 SelectBest
  assertBool "larger search-job budgets retain the route" $
    behavioralBestPipelineEligible 16 SelectBest
  assertBool "one search job retains the serial route" $
    not $ behavioralBestPipelineEligible 1 SelectBest
  assertBool "zero search jobs are ineligible" $
    not $ behavioralBestPipelineEligible 0 SelectBest
  assertBool "negative search jobs are ineligible" $
    not $ behavioralBestPipelineEligible (-1) SelectBest
  assertBool "first has an earlier stopping point" $
    not $ behavioralBestPipelineEligible 2 SelectFirst
  assertBool "lookahead has a bounded stopping point" $
    not $ behavioralBestPipelineEligible 2 $ SelectBestLookahead 2
  assertBool "all has a streaming result contract" $
    not $ behavioralBestPipelineEligible 2 SelectAll

testCandidatePipelineSerialParity :: Assertion
testCandidatePipelineSerialParity = do
  let terminal = Completed Finished
      results =
        [ pipelineQueryResult Continuing []
        , pipelineQueryResult Continuing
            [(3 :: Int, "first"), (2, "inadmissible")]
        , pipelineQueryResult Continuing
            [(1, "second"), (1, "third"), (4, "worse")]
        , pipelineQueryResult terminal []
        ]
      rank = fst
      isAdmissible = (/= "inadmissible") . snd
      logged logCell candidate = do
        modifyMVar_ logCell $ \previous -> pure $ previous ++ [candidate]
        pure $ isAdmissible candidate
  serialLog <- newMVar []
  pipelineLog <- newMVar []
  serial <- selectQueryResultsM SelectBest rank (logged serialLog) results
  pipelined <- selectBestQueryResultsPipelinedM
    (const $ pure ()) rank (logged pipelineLog) results
  assertEqual "selection parity" serial pipelined
  assertEqual "expected stable tied winners"
    (Selection (Just terminal) [(1, "second"), (1, "third")]) pipelined
  serialAdmissions <- readMVar serialLog
  pipelineAdmissions <- readMVar pipelineLog
  assertEqual "admission action order and cardinality"
    serialAdmissions pipelineAdmissions

testCandidatePipelineThreadOwnership :: Assertion
testCandidatePipelineThreadOwnership = do
  owner <- myThreadId
  producerThreads <- newMVar []
  admissionThreads <- newMVar []
  let observe cell = do
        thread <- myThreadId
        modifyMVar_ cell $ \previous -> pure $ previous ++ [thread]
      prepare _ = observe producerThreads
      admissible _ = observe admissionThreads >> pure True
  selection <- selectBestQueryResultsPipelinedM prepare id admissible
    [pipelineQueryResult (Completed Finished) [2 :: Int, 1]]
  assertEqual "ordinary best result" (Selection
    (Just $ Completed Finished) [1]) selection
  preparedBy <- readMVar producerThreads
  admittedBy <- readMVar admissionThreads
  assertEqual "every candidate prepared once" 2 $ length preparedBy
  assertEqual "every candidate admitted once" 2 $ length admittedBy
  assertBool "all preparation stays on one non-owner worker" $
    case preparedBy of
      [] -> False
      producer : remaining ->
        producer /= owner && all (== producer) remaining
  assertBool "admission remains on the calling owner" $
    all (== owner) admittedBy

testCandidatePipelineOneAhead :: Assertion
testCandidatePipelineOneAhead = do
  firstAdmissionStarted <- newEmptyMVar
  releaseFirstAdmission <- newEmptyMVar
  secondPrepared <- newEmptyMVar
  thirdPrepared <- newEmptyMVar
  let prepare candidate = case candidate of
        2 -> putMVar secondPrepared ()
        3 -> do
          putMVar thirdPrepared ()
          throwIO $ TaggedFailure "third preparation"
        _ -> pure ()
      admissible candidate = case candidate of
        1 -> do
          putMVar firstAdmissionStarted ()
          takeMVar releaseFirstAdmission
          pure True
        _ -> pure True
  runner <- async $ selectBestQueryResultsPipelinedM prepare id admissible
    [pipelineQueryResult (Completed Finished) [1 :: Int, 2, 3]]
  await "first owner admission" firstAdmissionStarted
  await "one prepared successor" secondPrepared
  earlyThird <- tryReadMVar thirdPrepared
  assertEqual "the second outstanding candidate consumes the sole permit"
    Nothing earlyThird
  putMVar releaseFirstAdmission ()
  await "third preparation after second dequeue" thirdPrepared
  outcome <- try $ awaitAsync "one-ahead poisoned pipeline" runner
  assertTaggedException "third preparation" outcome

testCandidatePipelineAdmissionExceptionPrecedence :: Assertion
testCandidatePipelineAdmissionExceptionPrecedence = do
  successorFailed <- newEmptyMVar
  let prepare candidate
        | candidate == (2 :: Int) = do
            putMVar successorFailed ()
            throwIO $ TaggedFailure "prepared successor"
        | otherwise = pure ()
      admissible candidate
        | candidate == 1 = do
            takeMVar successorFailed
            throwIO $ TaggedFailure "current admission"
        | otherwise = pure True
  outcome <- try $ selectBestQueryResultsPipelinedM prepare id admissible
    [pipelineQueryResult (Completed Finished) [1 :: Int, 2]]
  assertTaggedException "current admission" outcome

testCandidatePipelineWholeBatchAdmission :: Assertion
testCandidatePipelineWholeBatchAdmission = do
  let rank candidate
        | candidate == (1 :: Int) =
            throw $ TaggedFailure "premature batch rank"
        | otherwise = candidate
      admissible candidate
        | candidate == 2 = throwIO $ TaggedFailure "later admission"
        | otherwise = pure True
  outcome <- try $ selectBestQueryResultsPipelinedM
    (const $ pure ()) rank admissible
    [pipelineQueryResult (Completed Finished) [1 :: Int, 2]]
  assertTaggedException "later admission" outcome

testCandidatePipelineProducerBeforePartialRank :: Assertion
testCandidatePipelineProducerBeforePartialRank = do
  let prepare candidate
        | candidate == (2 :: Int) =
            throwIO $ TaggedFailure "incomplete-batch preparation"
        | otherwise = pure ()
      rank _ = throw $ TaggedFailure "partial-batch rank" :: Int
  outcome <- try $ selectBestQueryResultsPipelinedM prepare rank
    (const $ pure True)
    [pipelineQueryResult (Completed Finished) [1 :: Int, 2]]
  assertTaggedException "incomplete-batch preparation" outcome

testCandidatePipelineBatchBoundaryPrecedence :: Assertion
testCandidatePipelineBatchBoundaryPrecedence = do
  let prepare candidate
        | candidate == (3 :: Int) =
            throwIO $ TaggedFailure "later-batch preparation"
        | otherwise = pure ()
      rank _ = throw $ TaggedFailure "completed-batch rank" :: Int
      results =
        [ pipelineQueryResult Continuing [1 :: Int, 2]
        , pipelineQueryResult (Completed Finished) [3]
        ]
  outcome <- try $ selectBestQueryResultsPipelinedM prepare rank
    (const $ pure True) results
  assertTaggedException "completed-batch rank" outcome

testCandidatePipelineProgressDemand :: Assertion
testCandidatePipelineProgressDemand = do
  let poisonedProgress = throw $ TaggedFailure "batch progress"
      rank _ = throw $ TaggedFailure "batch rank" :: Int
  outcome <- try $ selectBestQueryResultsPipelinedM
    (const $ pure ()) rank (const $ pure True)
    [pipelineQueryResult poisonedProgress [1 :: Int, 2]]
  assertTaggedException "batch rank" outcome

  selection <- selectBestQueryResultsPipelinedM
    (const $ pure ()) id (const $ pure True)
    [pipelineQueryResult poisonedProgress [1 :: Int]]
  assertEqual "candidate projection does not force retained progress"
    [1] $ selectionCandidates selection
  progressOutcome <- try $ case selectionProgress selection of
    Nothing -> assertFailure "nonempty trace lost its progress"
    Just progress -> progress `seq` pure ()
  assertTaggedException "batch progress" progressOutcome

testCandidatePipelineEmptyBatches :: Assertion
testCandidatePipelineEmptyBatches = do
  preparationCount <- newMVar (0 :: Int)
  let terminal = Completed Finished
      results =
        [ pipelineQueryResult Continuing []
        , pipelineQueryResult Continuing []
        , pipelineQueryResult Continuing [2 :: Int, 1]
        , pipelineQueryResult Continuing []
        , pipelineQueryResult terminal []
        ]
      prepare _ = modifyMVar_ preparationCount $ pure . (+ 1)
  selection <- selectBestQueryResultsPipelinedM prepare id
    (const $ pure True) results
  assertEqual "terminal empty progress wins"
    (Selection (Just terminal) [1]) selection
  assertEqual "empty batches publish no candidate work" 2
    =<< readMVar preparationCount

testCandidatePipelineEmptyProgressDemand :: Assertion
testCandidatePipelineEmptyProgressDemand = do
  let poisonedProgress = throw $ TaggedFailure "empty progress"
      results =
        [ pipelineQueryResult poisonedProgress ([] :: [Int])
        , throw $ TaggedFailure "result suffix"
        ]
  outcome <- try $ selectBestQueryResultsPipelinedM
    (const $ pure ()) id (const $ pure True) results
  assertTaggedException "result suffix" outcome

  selection <- selectBestQueryResultsPipelinedM
    (const $ pure ()) id (const $ pure True)
    [pipelineQueryResult poisonedProgress ([] :: [Int])]
  assertEqual "empty terminal retains no candidates" []
    $ selectionCandidates selection
  progressOutcome <- try $ case selectionProgress selection of
    Nothing -> assertFailure "empty batch lost its progress"
    Just progress -> progress `seq` pure ()
  assertTaggedException "empty progress" progressOutcome

testCandidatePipelineOwnerFailureCleanup :: Assertion
testCandidatePipelineOwnerFailureCleanup = do
  preparationStarted <- newEmptyMVar
  preparationStopped <- newEmptyMVar
  neverFinishPreparation <- newEmptyMVar
  let prepare candidate
        | candidate == (2 :: Int) =
            (putMVar preparationStarted () >> takeMVar neverFinishPreparation)
              `finally` putMVar preparationStopped ()
        | otherwise = pure ()
      admissible candidate
        | candidate == 1 = do
            takeMVar preparationStarted
            throwIO $ TaggedFailure "owner action"
        | otherwise = pure True
  outcome <- try $ selectBestQueryResultsPipelinedM prepare id admissible
    [pipelineQueryResult (Completed Finished) [1 :: Int, 2]]
  assertTaggedException "owner action" outcome
  await "producer join after owner failure" preparationStopped

testCandidatePipelineCallerCancellation :: Assertion
testCandidatePipelineCallerCancellation = do
  preparationStarted <- newEmptyMVar
  preparationStopped <- newEmptyMVar
  neverFinishPreparation <- newEmptyMVar
  let prepare _ =
        (putMVar preparationStarted () >> takeMVar neverFinishPreparation)
          `finally` putMVar preparationStopped ()
  runner <- async $ selectBestQueryResultsPipelinedM prepare id
    (const $ pure True)
    [pipelineQueryResult (Completed Finished) [1 :: Int]]
  await "blocked candidate preparation" preparationStarted
  cancel runner
  await "producer join after caller cancellation" preparationStopped

pipelineQueryResult
  :: Progress
  -> [candidate]
  -> QueryResult () candidate
pipelineQueryResult progress candidates = queryResultFromCandidates
  $ SearchBatch progress () candidates

testEligibility :: Assertion
testEligibility = do
  assertBool "default two-worker strict command" $
    parallelPairEligible 2 False False
  assertBool "larger future job budget" $
    parallelPairEligible 8 False False
  assertBool "one worker is serial" $
    not $ parallelPairEligible 1 False False
  assertBool "zero workers is inadmissible and serial" $
    not $ parallelPairEligible 0 False False
  assertBool "timed command is serial" $
    not $ parallelPairEligible 2 True False
  assertBool "streaming command is serial" $
    not $ parallelPairEligible 2 False True
  assertBool "timed streaming command is serial" $
    not $ parallelPairEligible 2 True True
  assertBool "timed two-worker strict command" $
    timedParallelPairEligible 2 True False
  assertBool "timed larger future job budget" $
    timedParallelPairEligible 8 True False
  assertBool "timed one-worker command is serial" $
    not $ timedParallelPairEligible 1 True False
  assertBool "untimed commands do not select the timed route" $
    not $ timedParallelPairEligible 2 False False
  assertBool "timed streaming commands remain serial" $
    not $ timedParallelPairEligible 2 True True

testMonotonicDeadline :: Assertion
testMonotonicDeadline = do
  logicalNow <- newMVar (4000001 :: Word64)
  clockReads <- newMVar (0 :: Int)
  delays <- newMVar ([] :: [Int])
  let readNow = do
        modifyMVar_ clockReads $ pure . (+ 1)
        readMVar logicalNow
      delay microseconds = do
        call <- modifyMVar delays $ \previous ->
          let current = length previous + 1
          in pure (previous ++ [microseconds], current)
        modifyMVar_ logicalNow $ \now -> pure $ now + case call of
          1 -> 250000001
          _ -> fromIntegral microseconds * 1000
  deadline <- monotonicParallelDeadlineAfterSecondsWith readNow delay 1
  assertEqual "one captured elapsed-time cutoff" 1000000000
    $ parallelDeadlineCutoff deadline
  assertEqual "construction reads the clock exactly once" 1
    =<< readMVar clockReads
  parallelDeadlineAwait deadline
  assertEqual "an early wake waits only the remaining rounded-up budget"
    [1000000, 750000] =<< readMVar delays
  assertEqual "the watcher observes start, early wake, and cutoff" 4
    =<< readMVar clockReads

testMonotonicDeadlineWrap :: Assertion
testMonotonicDeadlineWrap = do
  logicalNow <- newMVar (maxBound - 499999999 :: Word64)
  delays <- newMVar ([] :: [Int])
  let readNow = readMVar logicalNow
      delay microseconds = do
        modifyMVar_ delays $ \previous -> pure $ previous ++ [microseconds]
        modifyMVar_ logicalNow $ \now -> pure $
          now + fromIntegral microseconds * 1000 + 250000000
  deadline <- monotonicParallelDeadlineAfterSecondsWith readNow delay 1
  assertEqual "wrap-safe elapsed cutoff" 1000000000
    $ parallelDeadlineCutoff deadline
  parallelDeadlineAwait deadline
  assertEqual "one late wake crosses the Word64 epoch" [1000000]
    =<< readMVar delays
  assertEqual "modular delta extends the epoch after wrap" 1250000000
    =<< parallelDeadlineNow deadline

testMaximumMonotonicDeadline :: Assertion
testMaximumMonotonicDeadline = do
  observedDelay <- newEmptyMVar
  let seconds = maxBound `div` 1000000 :: Int
      stopAtFirstDelay microseconds = do
        putMVar observedDelay microseconds
        throwIO $ TaggedFailure "stop maximum deadline watcher"
  deadline <- monotonicParallelDeadlineAfterSecondsWith
    (pure 7) stopAtFirstDelay seconds
  assertEqual "maximum accepted budget remains exact in Integer nanoseconds"
    (toInteger seconds * 1000000000)
    (parallelDeadlineCutoff deadline)
  outcome <- try $ parallelDeadlineAwait deadline
  assertTaggedException "stop maximum deadline watcher" outcome
  assertEqual "large budgets use bounded observation chunks" 1800000000
    =<< takeMVar observedDelay

testCheckedPairSequence :: Assertion
testCheckedPairSequence = do
  events <- newMVar ([] :: [String])
  leftStarted <- newEmptyMVar
  rightStarted <- newEmptyMVar
  release <- newEmptyMVar
  neverDeadline <- newEmptyMVar
  consumed <- newMVar
    ([] :: [ParallelLaneOutcome (Either String String)])
  let event label = modifyMVar_ events $ \previous ->
        pure $ previous ++ [label]
      checkedLeft = event "check-left" >> pure (Left "ordinary left")
      checkedRight = event "check-right" >> pure (Right "right")
      captureDeadline = do
        event "deadline"
        pure $ logicalDeadline (pure BeforeCutoff) $ takeMVar neverDeadline
      runLeft checked = do
        event "left-start"
        putMVar leftStarted ()
        readMVar release
        pure checked
      runRight checked = do
        event "right-start"
        putMVar rightStarted ()
        readMVar release
        pure checked
  runner <- async $ runCheckedParallelPairOrderedBefore
    checkedLeft checkedRight captureDeadline runLeft runRight
    (record consumed) (record consumed)
  await "checked left lane start" leftStarted
  await "checked right lane start" rightStarted
  observed <- readMVar events
  assertEqual "checks and cutoff precede either worker"
    ["check-left", "check-right", "deadline"]
    (take 3 observed)
  putMVar release ()
  awaitAsync "checked pair completion" runner
  assertEqual "ordinary checked failure still starts both lanes"
    [ ParallelLaneCompleted $ Left "ordinary left"
    , ParallelLaneCompleted $ Right "right"
    ]
    =<< readMVar consumed

testSimultaneousStart :: Assertion
testSimultaneousStart = do
  leftStarted <- newEmptyMVar
  rightStarted <- newEmptyMVar
  leftRelease <- newEmptyMVar
  rightRelease <- newEmptyMVar
  consumed <- newMVar []
  runner <- async $ runParallelPairOrdered
    (latchedValue leftStarted leftRelease "left")
    (latchedValue rightStarted rightRelease "right")
    (record consumed)
    (record consumed)
  await "left worker start" leftStarted
  await "right worker start before either release" rightStarted
  putMVar rightRelease ()
  putMVar leftRelease ()
  awaitAsync "parallel pair completion" runner
  assertEqual "stable consumers" ["left", "right"] =<< readMVar consumed

testStableConsumptionOrder :: Assertion
testStableConsumptionOrder = do
  leftStarted <- newEmptyMVar
  leftRelease <- newEmptyMVar
  rightFinished <- newEmptyMVar
  consumed <- newMVar []
  runner <- async $ runParallelPairOrdered
    (latchedValue leftStarted leftRelease "left")
    (putMVar rightFinished () >> pure "right")
    (record consumed)
    (record consumed)
  await "left worker start" leftStarted
  await "right worker completion" rightFinished
  assertEqual "right completion is not observed early" [] =<< readMVar consumed
  putMVar leftRelease ()
  awaitAsync "ordered pair completion" runner
  assertEqual "left-to-right observation" ["left", "right"]
    =<< readMVar consumed

testOrdinaryFailureIsolation :: Assertion
testOrdinaryFailureIsolation = do
  rightRan <- newEmptyMVar
  consumed <- newMVar []
  runParallelPairOrdered
    (pure $ Left "left failure")
    (putMVar rightRan () >> pure (Right "right success"))
    (record consumed)
    (record consumed)
  await "ordinary-failure sibling" rightRan
  assertEqual "both ordinary outcomes" [Left "left failure", Right "right success"]
    =<< readMVar consumed

testLeftExceptionCleanup :: Assertion
testLeftExceptionCleanup = do
  rightStarted <- newEmptyMVar
  rightStopped <- newEmptyMVar
  never <- newEmptyMVar
  consumed <- newMVar ([] :: [String])
  outcome <- try $ runParallelPairOrdered
    (takeMVar rightStarted >> throwIO (userError "left failed") :: IO String)
    ((putMVar rightStarted () >> takeMVar never)
      `finally` putMVar rightStopped ())
    (record consumed)
    (record consumed)
  assertException "left worker exception" outcome
  await "right worker cancel-and-wait cleanup" rightStopped
  assertEqual "exceptional pair has no consumer" [] =<< readMVar consumed

testRightExceptionOrdering :: Assertion
testRightExceptionOrdering = do
  leftStarted <- newEmptyMVar
  leftRelease <- newEmptyMVar
  rightStopped <- newEmptyMVar
  consumed <- newMVar ([] :: [String])
  runner <- async $ try $ runParallelPairOrdered
    (latchedValue leftStarted leftRelease "left")
    ((throwIO (userError "right failed") :: IO String)
      `finally` putMVar rightStopped ())
    (record consumed)
    (record consumed)
  await "left worker start" leftStarted
  await "right worker exception" rightStopped
  early <- poll runner
  assertBool "right exception remains ordered behind left" $ case early of
    Nothing -> True
    Just _ -> False
  putMVar leftRelease ()
  outcome <- awaitAsync "right exception delivery" runner
  assertException "right worker exception" outcome
  assertEqual "left value consumed first" ["left"] =<< readMVar consumed

testCallerCancellation :: Assertion
testCallerCancellation = do
  leftStarted <- newEmptyMVar
  rightStarted <- newEmptyMVar
  leftStopped <- newEmptyMVar
  rightStopped <- newEmptyMVar
  neverLeft <- newEmptyMVar
  neverRight <- newEmptyMVar
  consumed <- newMVar ([] :: [String])
  runner <- async $ runParallelPairOrdered
    ((putMVar leftStarted () >> takeMVar neverLeft)
      `finally` putMVar leftStopped ())
    ((putMVar rightStarted () >> takeMVar neverRight)
      `finally` putMVar rightStopped ())
    (record consumed)
    (record consumed)
  await "left worker start" leftStarted
  await "right worker start" rightStarted
  cancel runner
  await "left worker cancellation cleanup" leftStopped
  await "right worker cancellation cleanup" rightStopped
  assertEqual "cancelled pair has no consumer" [] =<< readMVar consumed

testStrictOutputPlan :: Assertion
testStrictOutputPlan = do
  rightStarted <- newEmptyMVar
  rightStopped <- newEmptyMVar
  never <- newEmptyMVar
  consumed <- newMVar ([] :: [CommandOutput])
  let poisoned = CommandOutput
        [CommandStandardOutputLine $ "prefix" ++ error "escaped output thunk"]
        ExitSuccess
  outcome <- try $ runParallelPairOrdered
    (takeMVar rightStarted >> pure poisoned)
    ((putMVar rightStarted () >> takeMVar never)
      `finally` putMVar rightStopped ())
    (record consumed)
    (record consumed)
  assertException "poisoned event" outcome
  await "strictness sibling cleanup" rightStopped
  assertEqual "poisoned plan never reaches replay" [] =<< readMVar consumed

data LogicalTime
  = BeforeCutoff
  | AtCutoff
  deriving (Eq, Ord, Show)

data TaggedFailure = TaggedFailure String
  deriving (Eq, Show)

instance Exception TaggedFailure

logicalDeadline :: IO LogicalTime -> IO () -> ParallelDeadline LogicalTime
logicalDeadline readNow awaitCutoff = ParallelDeadline
  { parallelDeadlineCutoff = AtCutoff
  , parallelDeadlineNow = readNow
  , parallelDeadlineAwait = awaitCutoff
  }

testTimedCommonDeadlineStart :: Assertion
testTimedCommonDeadlineStart = do
  leftStarted <- newEmptyMVar
  rightStarted <- newEmptyMVar
  leftStopped <- newEmptyMVar
  rightStopped <- newEmptyMVar
  deadlineStarted <- newEmptyMVar
  deadlineRelease <- newEmptyMVar
  deadlineCalls <- newMVar (0 :: Int)
  neverLeft <- newEmptyMVar
  neverRight <- newEmptyMVar
  consumed <- newMVar ([] :: [ParallelLaneOutcome String])
  let awaitCutoff = do
        modifyMVar_ deadlineCalls $ pure . (+ 1)
        putMVar deadlineStarted ()
        takeMVar deadlineRelease
  runner <- async $ runParallelPairOrderedBefore
    (logicalDeadline (pure AtCutoff) awaitCutoff)
    ((putMVar leftStarted () >> takeMVar neverLeft >> pure "left")
      `finally` putMVar leftStopped ())
    ((putMVar rightStarted () >> takeMVar neverRight >> pure "right")
      `finally` putMVar rightStopped ())
    (record consumed)
    (record consumed)
  await "timed left worker start" leftStarted
  await "timed right worker start" rightStarted
  await "common deadline watcher start" deadlineStarted
  assertEqual "one common deadline waiter" 1 =<< readMVar deadlineCalls
  putMVar deadlineRelease ()
  awaitAsync "common-deadline pair completion" runner
  await "common-deadline left join" leftStopped
  await "common-deadline right join" rightStopped
  assertEqual "lane-local timeout outcomes"
    [ParallelLaneTimedOut, ParallelLaneTimedOut]
    =<< readMVar consumed
  assertEqual "deadline action ran exactly once" 1 =<< readMVar deadlineCalls

testTimedCompletionBeforeDeadline :: Assertion
testTimedCompletionBeforeDeadline = do
  deadlineStarted <- newEmptyMVar
  neverDeadline <- newEmptyMVar
  deadlineStopped <- newEmptyMVar
  consumed <- newMVar ([] :: [ParallelLaneOutcome String])
  runParallelPairOrderedBefore
    (logicalDeadline (pure BeforeCutoff) $
      (putMVar deadlineStarted () >> takeMVar neverDeadline)
        `finally` putMVar deadlineStopped ())
    (readMVar deadlineStarted >> pure "left")
    (readMVar deadlineStarted >> pure "right")
    (record consumed)
    (record consumed)
  await "unused deadline watcher cleanup" deadlineStopped
  assertEqual "both pre-cutoff completions"
    [ParallelLaneCompleted "left", ParallelLaneCompleted "right"]
    =<< readMVar consumed

testTimedPublishedCompletionSurvivesCancellation :: Assertion
testTimedPublishedCompletionSurvivesCancellation = do
  publicationCount <- newMVar (0 :: Int)
  bothPublished <- newEmptyMVar
  publicationStopped <- newMVar (0 :: Int)
  bothStopped <- newEmptyMVar
  neverReturnFromPublication <- newEmptyMVar
  deadlineStarted <- newEmptyMVar
  deadlineRelease <- newEmptyMVar
  consumed <- newMVar ([] :: [ParallelLaneOutcome String])
  let afterCompletionPublished =
        (do
          count <- modifyMVar publicationCount $ \previous ->
            let current = previous + 1
            in pure (current, current)
          when (count == 2) $ putMVar bothPublished ()
          takeMVar neverReturnFromPublication)
        `finally` do
          count <- modifyMVar publicationStopped $ \previous ->
            let current = previous + 1
            in pure (current, current)
          when (count == 2) $ putMVar bothStopped ()
  runner <- async $ runParallelPairOrderedBeforeWithPublicationHook
    (logicalDeadline (pure BeforeCutoff) $
      putMVar deadlineStarted () >> takeMVar deadlineRelease)
    afterCompletionPublished
    (pure "left")
    (pure "right")
    (record consumed)
    (record consumed)
  await "post-publication deadline start" deadlineStarted
  await "both terminal cells published" bothPublished
  putMVar deadlineRelease ()
  awaitAsync "post-publication deadline race" runner
  await "post-publication worker joins" bothStopped
  assertEqual "published values survive cancellation before Async return"
    [ParallelLaneCompleted "left", ParallelLaneCompleted "right"]
    =<< readMVar consumed

testTimedPublishedFailureSurvivesCancellation :: Assertion
testTimedPublishedFailureSurvivesCancellation = do
  publicationCount <- newMVar (0 :: Int)
  bothPublished <- newEmptyMVar
  neverReturnFromPublication <- newEmptyMVar
  deadlineStarted <- newEmptyMVar
  deadlineRelease <- newEmptyMVar
  consumed <- newMVar ([] :: [ParallelLaneOutcome String])
  let afterCompletionPublished = do
        count <- modifyMVar publicationCount $ \previous ->
          let current = previous + 1
          in pure (current, current)
        when (count == 2) $ putMVar bothPublished ()
        takeMVar neverReturnFromPublication
  runner <- async $ try $ runParallelPairOrderedBeforeWithPublicationHook
    (logicalDeadline (pure BeforeCutoff) $
      putMVar deadlineStarted () >> takeMVar deadlineRelease)
    afterCompletionPublished
    (throwIO $ TaggedFailure "published left failure")
    (pure "right")
    (record consumed)
    (record consumed)
  await "published-failure deadline start" deadlineStarted
  await "failure and value terminal cells published" bothPublished
  putMVar deadlineRelease ()
  outcome <- awaitAsync "published failure deadline race" runner
  assertTaggedException "published left failure" outcome
  assertEqual "published failure prevents consumer replay" []
    =<< readMVar consumed

testTimedDeadlineExpiry :: Assertion
testTimedDeadlineExpiry = do
  leftStarted <- newEmptyMVar
  rightStarted <- newEmptyMVar
  leftStopped <- newEmptyMVar
  rightStopped <- newEmptyMVar
  deadlineStarted <- newEmptyMVar
  deadlineRelease <- newEmptyMVar
  neverLeft <- newEmptyMVar
  neverRight <- newEmptyMVar
  consumed <- newMVar ([] :: [ParallelLaneOutcome String])
  runner <- async $ runParallelPairOrderedBefore
    (logicalDeadline (pure AtCutoff) $
      putMVar deadlineStarted () >> takeMVar deadlineRelease)
    ((putMVar leftStarted () >> takeMVar neverLeft >> pure "left")
      `finally` putMVar leftStopped ())
    ((putMVar rightStarted () >> takeMVar neverRight >> pure "right")
      `finally` putMVar rightStopped ())
    (record consumed)
    (record consumed)
  await "expiring left worker start" leftStarted
  await "expiring right worker start" rightStarted
  await "expiring deadline watcher start" deadlineStarted
  putMVar deadlineRelease ()
  awaitAsync "expired pair completion" runner
  await "expired left worker join" leftStopped
  await "expired right worker join" rightStopped
  assertEqual "deadline outcomes"
    [ParallelLaneTimedOut, ParallelLaneTimedOut]
    =<< readMVar consumed

testTimedDeadlineCancellationException :: Assertion
testTimedDeadlineCancellationException = do
  leftStarted <- newEmptyMVar
  rightStarted <- newEmptyMVar
  leftStopped <- newEmptyMVar
  rightStopped <- newEmptyMVar
  deadlineStarted <- newEmptyMVar
  deadlineRelease <- newEmptyMVar
  neverLeft <- newEmptyMVar
  neverRight <- newEmptyMVar
  consumed <- newMVar ([] :: [ParallelLaneOutcome String])
  runner <- async $ try $ runParallelPairOrderedBefore
    (logicalDeadline (pure AtCutoff) $
      putMVar deadlineStarted () >> takeMVar deadlineRelease)
    ((replaceCancellationWithUserInterrupt $
        putMVar leftStarted () >> takeMVar neverLeft >> pure "left")
      `finally` putMVar leftStopped ())
    ((replaceCancellationWithUserInterrupt $
        putMVar rightStarted () >> takeMVar neverRight >> pure "right")
      `finally` putMVar rightStopped ())
    (record consumed)
    (record consumed)
  await "replacement-exception left start" leftStarted
  await "replacement-exception right start" rightStarted
  await "replacement-exception deadline start" deadlineStarted
  putMVar deadlineRelease ()
  outcome <- awaitAsync "replacement async exception delivery" runner
  assertUserInterrupt outcome
  await "replacement-exception left cleanup" leftStopped
  await "replacement-exception right cleanup" rightStopped
  assertEqual "worker async exception is not reported as timeout" []
    =<< readMVar consumed

testTimedDeadlineWatcherFailure :: Assertion
testTimedDeadlineWatcherFailure = do
  leftStarted <- newEmptyMVar
  rightStarted <- newEmptyMVar
  leftStopped <- newEmptyMVar
  rightStopped <- newEmptyMVar
  deadlineStarted <- newEmptyMVar
  deadlineRelease <- newEmptyMVar
  deadlineStopped <- newEmptyMVar
  neverLeft <- newEmptyMVar
  neverRight <- newEmptyMVar
  consumed <- newMVar ([] :: [ParallelLaneOutcome String])
  runner <- async $ try $ runParallelPairOrderedBefore
    (logicalDeadline (pure BeforeCutoff) $
      (putMVar deadlineStarted () >> takeMVar deadlineRelease
        >> throwIO (TaggedFailure "deadline"))
      `finally` putMVar deadlineStopped ())
    ((putMVar leftStarted () >> takeMVar neverLeft >> pure "left")
      `finally` putMVar leftStopped ())
    ((putMVar rightStarted () >> takeMVar neverRight >> pure "right")
      `finally` putMVar rightStopped ())
    (record consumed)
    (record consumed)
  await "watcher-failure left start" leftStarted
  await "watcher-failure right start" rightStarted
  await "watcher-failure deadline start" deadlineStarted
  putMVar deadlineRelease ()
  outcome <- awaitAsync "deadline watcher failure" runner
  assertTaggedException "deadline" outcome
  await "failed deadline watcher cleanup" deadlineStopped
  await "watcher-failure left worker join" leftStopped
  await "watcher-failure right worker join" rightStopped
  assertEqual "watcher failure prevents consumer replay" []
    =<< readMVar consumed

testTimedCutoffTie :: Assertion
testTimedCutoffTie = do
  deadlineStarted <- newEmptyMVar
  neverDeadline <- newEmptyMVar
  deadlineStopped <- newEmptyMVar
  consumed <- newMVar ([] :: [ParallelLaneOutcome String])
  runParallelPairOrderedBefore
    (logicalDeadline (pure AtCutoff) $
      (putMVar deadlineStarted () >> takeMVar neverDeadline)
        `finally` putMVar deadlineStopped ())
    (readMVar deadlineStarted >> pure "left")
    (readMVar deadlineStarted >> pure "right")
    (record consumed)
    (record consumed)
  await "tie deadline watcher cleanup" deadlineStopped
  assertEqual "equality belongs to timeout"
    [ParallelLaneTimedOut, ParallelLaneTimedOut]
    =<< readMVar consumed

testTimedSuccessAndTimeout :: Assertion
testTimedSuccessAndTimeout = do
  leftStarted <- newEmptyMVar
  leftStopped <- newEmptyMVar
  rightStamped <- newEmptyMVar
  deadlineStarted <- newEmptyMVar
  deadlineRelease <- newEmptyMVar
  neverLeft <- newEmptyMVar
  consumed <- newMVar ([] :: [ParallelLaneOutcome String])
  runner <- async $ runParallelPairOrderedBefore
    (logicalDeadline
      (putMVar rightStamped () >> pure BeforeCutoff)
      (putMVar deadlineStarted () >> takeMVar deadlineRelease))
    ((putMVar leftStarted () >> takeMVar neverLeft >> pure "left")
      `finally` putMVar leftStopped ())
    (pure "right")
    (record consumed)
    (record consumed)
  await "success-timeout left start" leftStarted
  await "success-timeout deadline start" deadlineStarted
  await "right result stamped before cutoff" rightStamped
  putMVar deadlineRelease ()
  awaitAsync "success-timeout pair completion" runner
  await "timed-out left lane join" leftStopped
  assertEqual "right success retained past parent observation of deadline"
    [ParallelLaneTimedOut, ParallelLaneCompleted "right"]
    =<< readMVar consumed

testTimedFailureAndTimeout :: Assertion
testTimedFailureAndTimeout = do
  leftStamped <- newEmptyMVar
  rightStarted <- newEmptyMVar
  rightStopped <- newEmptyMVar
  deadlineStarted <- newEmptyMVar
  deadlineRelease <- newEmptyMVar
  neverRight <- newEmptyMVar
  consumed <- newMVar
    ([] :: [ParallelLaneOutcome (Either String String)])
  runner <- async $ runParallelPairOrderedBefore
    (logicalDeadline
      (putMVar leftStamped () >> pure BeforeCutoff)
      (putMVar deadlineStarted () >> takeMVar deadlineRelease))
    (pure $ Left "checked failure")
    ((putMVar rightStarted () >> takeMVar neverRight
        >> pure (Right "right"))
      `finally` putMVar rightStopped ())
    (record consumed)
    (record consumed)
  await "ordinary failure stamp" leftStamped
  await "failure-timeout right start" rightStarted
  await "failure-timeout deadline start" deadlineStarted
  putMVar deadlineRelease ()
  awaitAsync "failure-timeout pair completion" runner
  await "failure-timeout right join" rightStopped
  assertEqual "ordinary failure remains a completed lane value"
    [ ParallelLaneCompleted $ Left "checked failure"
    , ParallelLaneTimedOut
    ]
    =<< readMVar consumed

testTimedExceptionPrecedence :: Assertion
testTimedExceptionPrecedence = do
  leftStarted <- newEmptyMVar
  leftRelease <- newEmptyMVar
  rightStopped <- newEmptyMVar
  neverDeadline <- newEmptyMVar
  consumed <- newMVar ([] :: [ParallelLaneOutcome String])
  runner <- async $ try $ runParallelPairOrderedBefore
    (logicalDeadline (pure BeforeCutoff) $ takeMVar neverDeadline)
    (putMVar leftStarted () >> takeMVar leftRelease
      >> throwIO (TaggedFailure "left"))
    ((throwIO (TaggedFailure "right") :: IO String)
      `finally` putMVar rightStopped ())
    (record consumed)
    (record consumed)
  await "exceptional timed left start" leftStarted
  await "right exception completion" rightStopped
  early <- poll runner
  assertBool "right timed exception remains behind left" $ case early of
    Nothing -> True
    Just _ -> False
  putMVar leftRelease ()
  outcome <- awaitAsync "left-first timed exception" runner
  assertTaggedException "left" outcome
  assertEqual "no timed exception reaches a consumer" []
    =<< readMVar consumed

testTimedTimeoutBeforeRightException :: Assertion
testTimedTimeoutBeforeRightException = do
  leftStarted <- newEmptyMVar
  leftStopped <- newEmptyMVar
  rightStopped <- newEmptyMVar
  deadlineStarted <- newEmptyMVar
  deadlineRelease <- newEmptyMVar
  neverLeft <- newEmptyMVar
  consumed <- newMVar ([] :: [ParallelLaneOutcome String])
  runner <- async $ try $ runParallelPairOrderedBefore
    (logicalDeadline (pure BeforeCutoff) $
      putMVar deadlineStarted () >> takeMVar deadlineRelease)
    ((putMVar leftStarted () >> takeMVar neverLeft >> pure "left")
      `finally` putMVar leftStopped ())
    ((throwIO (TaggedFailure "right") :: IO String)
      `finally` putMVar rightStopped ())
    (record consumed)
    (record consumed)
  await "timeout-exception left start" leftStarted
  await "timeout-exception right completion" rightStopped
  await "timeout-exception deadline start" deadlineStarted
  putMVar deadlineRelease ()
  outcome <- awaitAsync "timeout before right exception" runner
  await "timeout-exception left join" leftStopped
  assertTaggedException "right" outcome
  assertEqual "left timeout consumed before right exception"
    [ParallelLaneTimedOut]
    =<< readMVar consumed

testTimedCallerCancellation :: Assertion
testTimedCallerCancellation = do
  leftStarted <- newEmptyMVar
  rightStarted <- newEmptyMVar
  deadlineStarted <- newEmptyMVar
  leftStopped <- newEmptyMVar
  rightStopped <- newEmptyMVar
  deadlineStopped <- newEmptyMVar
  neverLeft <- newEmptyMVar
  neverRight <- newEmptyMVar
  neverDeadline <- newEmptyMVar
  consumed <- newMVar ([] :: [ParallelLaneOutcome String])
  runner <- async $ runParallelPairOrderedBefore
    (logicalDeadline (pure BeforeCutoff) $
      (putMVar deadlineStarted () >> takeMVar neverDeadline)
        `finally` putMVar deadlineStopped ())
    ((putMVar leftStarted () >> takeMVar neverLeft >> pure "left")
      `finally` putMVar leftStopped ())
    ((putMVar rightStarted () >> takeMVar neverRight >> pure "right")
      `finally` putMVar rightStopped ())
    (record consumed)
    (record consumed)
  await "cancelled timed left start" leftStarted
  await "cancelled timed right start" rightStarted
  await "cancelled deadline start" deadlineStarted
  cancel runner
  await "cancelled timed left cleanup" leftStopped
  await "cancelled timed right cleanup" rightStopped
  await "cancelled deadline cleanup" deadlineStopped
  assertEqual "caller interruption is not a timeout outcome" []
    =<< readMVar consumed

testTimedStrictOutputPlan :: Assertion
testTimedStrictOutputPlan = do
  rightStarted <- newEmptyMVar
  rightStopped <- newEmptyMVar
  neverRight <- newEmptyMVar
  neverDeadline <- newEmptyMVar
  consumed <- newMVar ([] :: [ParallelLaneOutcome CommandOutput])
  let poisoned = CommandOutput
        [CommandStandardOutputLine $ "prefix" ++ error "escaped output thunk"]
        ExitSuccess
  outcome <- try $ runParallelPairOrderedBefore
    (logicalDeadline (pure BeforeCutoff) $ takeMVar neverDeadline)
    (takeMVar rightStarted >> pure poisoned)
    ((putMVar rightStarted () >> takeMVar neverRight
        >> pure (CommandOutput [] ExitSuccess))
      `finally` putMVar rightStopped ())
    (record consumed)
    (record consumed)
  assertException "timed poisoned event" outcome
  await "timed strictness sibling cleanup" rightStopped
  assertEqual "timed poisoned plan never reaches replay" []
    =<< readMVar consumed

testTimedStableConsumptionOrder :: Assertion
testTimedStableConsumptionOrder = do
  leftStarted <- newEmptyMVar
  leftRelease <- newEmptyMVar
  rightFinished <- newEmptyMVar
  neverDeadline <- newEmptyMVar
  consumed <- newMVar ([] :: [ParallelLaneOutcome String])
  runner <- async $ runParallelPairOrderedBefore
    (logicalDeadline (pure BeforeCutoff) $ takeMVar neverDeadline)
    (latchedValue leftStarted leftRelease "left")
    (putMVar rightFinished () >> pure "right")
    (record consumed)
    (record consumed)
  await "timed ordered left start" leftStarted
  await "timed ordered right completion" rightFinished
  assertEqual "timed right completion is not observed early" []
    =<< readMVar consumed
  putMVar leftRelease ()
  awaitAsync "timed ordered pair completion" runner
  assertEqual "timed left-to-right observation"
    [ParallelLaneCompleted "left", ParallelLaneCompleted "right"]
    =<< readMVar consumed

assertTaggedException
  :: String
  -> Either SomeException value
  -> Assertion
assertTaggedException expected outcome = case outcome of
  Left exception -> case fromException exception of
    Just (TaggedFailure actual) ->
      assertEqual "tagged exception" expected actual
    Nothing -> assertFailure $ "unexpected exception type: " ++ show exception
  Right _ -> assertFailure $ "expected TaggedFailure " ++ show expected

replaceCancellationWithUserInterrupt :: IO value -> IO value
replaceCancellationWithUserInterrupt action = action `catch` throwUserInterrupt

throwUserInterrupt :: SomeException -> IO value
throwUserInterrupt _ = throwIO UserInterrupt

assertUserInterrupt :: Either SomeException value -> Assertion
assertUserInterrupt outcome = case outcome of
  Left exception -> case fromException exception of
    Just UserInterrupt -> pure ()
    _ -> assertFailure $ "unexpected exception type: " ++ show exception
  Right _ -> assertFailure "expected UserInterrupt"

latchedValue :: MVar () -> MVar () -> value -> IO value
latchedValue started release value =
  putMVar started () >> takeMVar release >> pure value

record :: MVar [value] -> value -> IO ()
record values value = modifyMVar_ values $ \previous ->
  pure $ previous ++ [value]

await :: String -> MVar value -> IO value
await label signal = do
  observed <- timeout testTimeoutMicroseconds $ takeMVar signal
  maybe (assertFailure $ "timed out waiting for " ++ label) pure observed

awaitAsync :: String -> Async value -> IO value
awaitAsync label worker = do
  observed <- timeout testTimeoutMicroseconds $ wait worker
  maybe (assertFailure $ "timed out waiting for " ++ label) pure observed

assertException :: String -> Either SomeException value -> Assertion
assertException label outcome = assertBool label $ case outcome of
  Left _ -> True
  Right _ -> False

testTimeoutMicroseconds :: Int
testTimeoutMicroseconds = 5000000
