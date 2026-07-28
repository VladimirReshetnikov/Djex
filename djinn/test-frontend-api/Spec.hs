module Main (main) where

import Control.Exception (bracket, evaluate, finally, try)
import Data.Either (isRight)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (ExitFailure))
import System.IO
  ( Handle
  , SeekMode (AbsoluteSeek)
  , hClose
  , hFlush
  , hGetContents
  , hPutStr
  , hSeek
  , openTempFile
  , stderr
  , stdin
  , stdout
  )
import System.IO.Error (tryIOError)

import qualified Djinn
import Djinn.Core (parseHType)
import Language.Haskell.Djex.Djinn (standardDjinnSession)
import qualified Language.Haskell.Synthesis.Diagnostic as Diagnostic
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)

main :: IO ()
main = defaultMain $ testGroup "Djinn frontend import surface"
  [ testCase "reexports the historical core facade" $
      assertBool "the reexported Djinn parser rejected a valid type"
        $ isRight $ parseHType "a -> a"
  , testCase "reexports the checked Djex adapter" $
      assertBool "the reexported standard Djinn session did not seal"
        $ isRight standardDjinnSession
  , testCase "owns the historical compatibility module" $
      Djinn.main `seq` pure ()
  , testCase "renders a structured startup failure and exits cleanly"
      testStartupFailure
  , testCase "a failed clear retains a usable session"
      testClearFailure
  ]

testStartupFailure :: Assertion
testStartupFailure = do
  let failure = testFailure "DJINN_TEST_STARTUP"
        "fixture session initialization failed"
      runFailure = try
        (Djinn.runWithSessionInitializer (pure $ Left failure) [])
          :: IO (Either ExitCode ())
  (result, errors) <- captureHandle stderr runFailure
  assertEqual "startup failure should use the historical nonzero status"
    (Left $ ExitFailure 1) result
  assertContains "startup should retain the structured diagnostic code"
    "error [DJINN_TEST_STARTUP]: fixture session initialization failed"
    errors
  assertContains "startup should retain structured diagnostic context"
    "context: injected frontend failure" errors

testClearFailure :: Assertion
testClearFailure = do
  session <- either (fail . Diagnostic.renderDiagnostic) pure
    standardDjinnSession
  attempts <- newIORef (0 :: Int)
  let failure = testFailure "DJINN_TEST_CLEAR"
        "fixture session reset failed"
      initializeSession = do
        attempt <- atomicModifyIORef' attempts $ \count ->
          (count + 1, count)
        pure $ if attempt == 0 then Right session else Left failure
  (_, output) <- withInput ":clear\n:quit\n" $
    captureHandle stdout $
      Djinn.runWithSessionInitializer initializeSession []
  count <- readIORef attempts
  assertEqual "startup and clear should each invoke the checked initializer"
    2 count
  assertContains "clear should render the structured reset diagnostic"
    "error [DJINN_TEST_CLEAR]: fixture session reset failed" output
  assertContains "the loop should remain usable after a failed clear"
    "Bye." output

testFailure :: String -> String -> Diagnostic.Diagnostic
testFailure code message = Diagnostic.contextualDiagnostic
  Diagnostic.Error code message "injected frontend failure"

assertContains :: String -> String -> String -> Assertion
assertContains description needle haystack =
  assertBool (description ++ ": missing " ++ show needle) $
    needle `isInfixOf` haystack

captureHandle :: Handle -> IO result -> IO (result, String)
captureHandle destination action =
  withTemporaryHandle "djinn-frontend-output" $ \capture ->
    bracket (hDuplicate destination) hClose $ \saved -> do
      result <- (hDuplicateTo capture destination >> action)
        `finally` do
          hFlush destination
          hDuplicateTo saved destination
      hFlush capture
      hSeek capture AbsoluteSeek 0
      output <- hGetContents capture
      _ <- evaluate $ length output
      pure (result, output)

withInput :: String -> IO result -> IO result
withInput contents action =
  withTemporaryHandle "djinn-frontend-input" $ \input -> do
    hPutStr input contents
    hFlush input
    hSeek input AbsoluteSeek 0
    bracket (hDuplicate stdin) hClose $ \saved ->
      (hDuplicateTo input stdin >> action)
        `finally` hDuplicateTo saved stdin

withTemporaryHandle :: String -> (Handle -> IO result) -> IO result
withTemporaryHandle template action = bracket acquire release $ \(_, handle) ->
  action handle
  where
    acquire = do
      temporaryDirectory <- getTemporaryDirectory
      openTempFile temporaryDirectory template
    release (path, handle) = do
      _ <- tryIOError $ hClose handle
      _ <- tryIOError $ removeFile path
      pure ()
