module Main (main) where

import Control.Concurrent.Async
  ( Async
  , async
  , cancel
  , poll
  , wait
  )
import Control.Concurrent.MVar
  ( MVar
  , modifyMVar_
  , newEmptyMVar
  , newMVar
  , putMVar
  , readMVar
  , takeMVar
  )
import Control.Exception
  ( SomeException
  , finally
  , throwIO
  , try
  )
import System.Exit (ExitCode (ExitSuccess))
import System.Timeout (timeout)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit
  ( Assertion
  , assertBool
  , assertEqual
  , assertFailure
  , testCase
  )

import Language.Haskell.Djex.Command.Output
import Language.Haskell.Djex.REPL.Parallel

main :: IO ()
main = defaultMain $ testGroup "Djex deterministic parallel pair"
  [ testCase "eligibility excludes one worker, timeouts, and streaming"
      testEligibility
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
  ]

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
