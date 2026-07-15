module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Char (toLower)
import Data.List (isInfixOf)
import System.Directory
  ( createDirectory
  , getTemporaryDirectory
  , removeFile
  , removePathForcibly
  )
import System.Exit (ExitCode (..))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit
  ( Assertion
  , assertBool
  , assertEqual
  , testCase
  )

main :: IO ()
main = defaultMain $ testGroup "Djex CLI integration"
  [ testCase "global help and version load no backend" testGlobalInformation
  , testCase "backend help loads no environment" testBackendHelp
  , testCase "usage errors have a distinct exit status" testUsageErrors
  , testCase "backend options are validated before search" testOptionErrors
  , testCase "machine-sized options cannot wrap around" testIntOptionOverflow
  , testCase "Djinn renders definitions and expressions" testDjinnRendering
  , testCase "Djinn parses its shared class-context grammar" testDjinnContext
  , testCase "Djinn distinguishes refutation from truncation"
      testDjinnEvidence
  , testCase "Djinn selection preserves candidate-limit status"
      testDjinnSelection
  , testCase "Exference renders through the installed environment"
      testExferenceRendering
  , testCase "Exference accepts an empty checked environment"
      testEmptyExferenceEnvironment
  , testCase "Exference environment IO failures stay structured"
      testMissingExferenceEnvironment
  , testCase "Exference rejects unsupported source vocabulary"
      testUnsupportedExferenceEnvironment
  , testCase "Exference parse failures are runtime diagnostics"
      testExferenceParseFailure
  , testCase "Exference resource bounds retain successful status"
      testExferenceBounds
  , testCase "residual constraints remain attached to generated code"
      testResidualConstraintRendering
  ]

testGlobalInformation :: Assertion
testGlobalInformation = do
  (helpExit, help, helpErrors) <- runDjex ["--help"]
  assertEqual "help exit" ExitSuccess helpExit
  assertContains "global help" "djex djinn [OPTION...] TYPE" help
  assertContains "global help" "djex exference [OPTION...] TYPE" help
  assertEqual "help stderr" "" helpErrors

  (versionExit, versionOutput, versionErrors) <- runDjex ["--version"]
  assertEqual "version exit" ExitSuccess versionExit
  assertContains "version output" "djex version 2026.7.14" versionOutput
  assertEqual "version stderr" "" versionErrors

testBackendHelp :: Assertion
testBackendHelp = do
  forM_ ["djinn", "exference"] $ \backend -> do
    (exitCode, output, errors) <- runDjex [backend, "--help"]
    assertEqual (backend ++ " help exit") ExitSuccess exitCode
    assertContains (backend ++ " help")
      ("Usage: djex " ++ backend ++ " [OPTION...] TYPE") output
    assertEqual (backend ++ " help stderr") "" errors

testUsageErrors :: Assertion
testUsageErrors = do
  assertUsageFailure [] "a backend is required"
  assertUsageFailure ["unknown", "a -> a"] "unknown backend"
  assertUsageFailure ["djinn"] "exactly one input type is required"
  assertUsageFailure ["djinn", "a", "b"]
    "exactly one input type is required, but got 2"
  assertUsageFailure ["djinn", "--max-steps", "1", "a -> a"]
    "unrecognized option"
  assertUsageFailure ["exference", "--choice-budget", "1", "a -> a"]
    "unrecognized option"

testOptionErrors :: Assertion
testOptionErrors = do
  assertUsageFailure
    ["djinn", "--candidate-limit", "0", "a -> a"]
    "--candidate-limit must be a positive integer"
  assertUsageFailure
    ["djinn", "--target", "Data.result", "a -> a"]
    "invalid --target"
  assertUsageFailure
    ["exference", "--target", "Data.result", "("]
    "invalid --target"
  assertUsageFailure
    ["exference", "--max-depth", "NaN", "a -> a"]
    "--max-depth must be a finite non-negative number"
  assertUsageFailure
    ["exference", "--allow-unused", "--allow-unused", "a -> a"]
    "--allow-unused may be specified only once"

testIntOptionOverflow :: Assertion
testIntOptionOverflow = do
  let wrappedZero = show intModulus
      wrappedOne = show $ intModulus + 1
  assertUsageFailure
    ["djinn", "--candidate-limit", wrappedOne, "a -> a"]
    "--candidate-limit must be a positive integer"
  assertUsageFailure
    [ "exference"
    , "--constraint-deferral-steps", wrappedZero
    , "a -> a"
    ]
    "--constraint-deferral-steps must be a non-negative integer"
  assertUsageFailure
    ["exference", "--max-steps", wrappedOne, "a -> a"]
    "--max-steps must be a positive integer"
  assertUsageFailure
    ["exference", "--max-queue", wrappedZero, "a -> a"]
    "--max-queue must be a non-negative integer"

-- Values at these offsets previously became zero or one when read as Int.
-- Deriving the modulus from the host bounds keeps the regression portable.
intModulus :: Integer
intModulus =
  toInteger (maxBound :: Int) - toInteger (minBound :: Int) + 1

testDjinnRendering :: Assertion
testDjinnRendering = do
  definition <- assertSuccess ["djinn", "a -> a"]
  assertEqual "Djinn definition" "djexResult a = a\n" definition

  expression <- assertSuccess
    ["djinn", "--render", "expression", "a -> a"]
  assertEqual "Djinn expression must reconstruct its leading lambda"
    "\\a -> a\n" expression

testDjinnContext :: Assertion
testDjinnContext = do
  output <- assertSuccess ["djinn", "Eq a => a -> a"]
  assertContains "contextual Djinn result" "djexResult a = a" output

testDjinnEvidence :: Assertion
testDjinnEvidence = do
  (negativeExit, negativeOutput, negativeErrors) <- runDjex
    ["djinn", "((a -> b) -> a) -> a"]
  assertEqual "logical refutation exit" ExitSuccess negativeExit
  assertEqual "logical refutation stdout" "" negativeOutput
  assertContains "logical refutation diagnostic"
    "[DJEX_DJINN_UNINHABITABLE]" negativeErrors
  assertBool "a refutation must not be described as undecided" $
    not $ "DJEX_DJINN_UNDECIDED" `isInfixOf` negativeErrors

  (boundedExit, boundedOutput, boundedErrors) <- runDjex
    ["djinn", "--choice-budget", "1", "((a -> b) -> a) -> a"]
  assertEqual "bounded search exit" ExitSuccess boundedExit
  assertEqual "bounded search stdout" "" boundedOutput
  assertContains "bounded search evidence" "[DJEX_DJINN_UNDECIDED]"
    boundedErrors
  assertContains "bounded search status" "[DJEX_SEARCH_TRUNCATED]"
    boundedErrors

testDjinnSelection :: Assertion
testDjinnSelection = do
  (limitedExit, limitedOutput, limitedErrors) <- runDjex
    [ "djinn"
    , "--select", "all"
    , "--candidate-limit", "1"
    , "a -> a -> a"
    ]
  assertEqual "candidate-limited exit" ExitSuccess limitedExit
  assertContains "candidate retained across truncation" "djexResult"
    limitedOutput
  assertContains "candidate-limit warning" "CandidateLimitReached"
    limitedErrors

  allOutput <- assertSuccess
    ["djinn", "--select", "all", "a -> a -> a"]
  assertContains "all mode candidate separator" "-- or" allOutput

testExferenceRendering :: Assertion
testExferenceRendering = do
  definition <- assertSuccess
    ["exference", "--select", "first", "a -> a"]
  assertContains "Exference definition" "djexResult" definition
  assertContains "Exference definition body" "\\a -> a" definition

  expression <- assertSuccess
    [ "exference", "--select", "first"
    , "--render", "expression"
    , "a -> a"
    ]
  assertEqual "Exference expression" "\\a -> a\n" expression

testEmptyExferenceEnvironment :: Assertion
testEmptyExferenceEnvironment = withTemporaryEnvironment [] $ \directory -> do
  output <- assertSuccess
    [ "exference", "--environment", directory
    , "--select", "first"
    , "--render", "expression"
    , "a -> a"
    ]
  assertEqual "environment-free lambda" "\\a -> a\n" output

testMissingExferenceEnvironment :: Assertion
testMissingExferenceEnvironment = withMissingPath $ \path -> do
  (exitCode, output, errors) <- runDjex
    ["exference", "--environment", path, "a -> a"]
  assertEqual "missing environment exit" (ExitFailure 1) exitCode
  assertEqual "missing environment stdout" "" output
  assertContains "missing environment code"
    "[EXF_ENV_DIRECTORY_READ]" errors
  assertContains "missing environment path" path errors
  assertNoCallStack errors

testUnsupportedExferenceEnvironment :: Assertion
testUnsupportedExferenceEnvironment = withTemporaryEnvironment
    [("Broken.hs", unlines
      [ "{-# LANGUAGE TypeFamilies #-}"
      , "module Broken where"
      , "type family F a"
      ])] $ \directory -> do
  (exitCode, output, errors) <- runDjex
    ["exference", "--environment", directory, "a -> a"]
  assertEqual "unsupported environment exit" (ExitFailure 1) exitCode
  assertEqual "unsupported environment stdout" "" output
  assertContains "unsupported vocabulary code"
    "[EXF_UNSUPPORTED_VOCABULARY]" errors
  assertNoCallStack errors

testExferenceParseFailure :: Assertion
testExferenceParseFailure = do
  (exitCode, output, errors) <- runDjex
    ["exference", "--select", "first", "("]
  assertEqual "parse failure exit" (ExitFailure 1) exitCode
  assertEqual "parse failure stdout" "" output
  assertContains "parse failure code" "[DJEX_EXF_PARSE]" errors
  assertContains "parse failure source" "<command-line>" errors
  assertNoCallStack errors

testExferenceBounds :: Assertion
testExferenceBounds = withTemporaryEnvironment [] $ \directory -> do
  (exitCode, output, errors) <- runDjex
    [ "exference", "--environment", directory
    , "--select", "first"
    , "--max-steps", "1"
    , "--max-queue", "0"
    , "a -> a"
    ]
  assertEqual "bounded Exference exit" ExitSuccess exitCode
  assertEqual "bounded Exference stdout" "" output
  assertContains "bounded Exference no-result diagnostic"
    "[DJEX_EXF_NO_RESULT]" errors
  assertContains "bounded Exference truncation diagnostic"
    "[DJEX_SEARCH_TRUNCATED]" errors

testResidualConstraintRendering :: Assertion
testResidualConstraintRendering = withTemporaryEnvironment
    [("Fixture.hs", unlines
      [ "module Fixture where"
      , "class C a"
      , "witness :: C a => a"
      ])] $ \directory -> do
  (exitCode, output, errors) <- runDjex
    [ "exference", "--environment", directory
    , "--select", "first"
    , "--allow-constraints"
    , "--max-steps", "2048"
    , "a"
    ]
  assertEqual ("residual search stderr: " ++ errors) ExitSuccess exitCode
  assertContains "residual comment" "-- requires:" output
  assertContains "residual class" "Fixture.C" output
  assertContains "residual candidate" "Fixture.witness" output
  assertBool "residual constraints must not be detached as diagnostics" $
    not $ "DJEX_EXF_RESIDUAL_CONSTRAINTS" `isInfixOf` errors

  unqualified <- assertSuccess
    [ "exference", "--environment", directory
    , "--select", "first"
    , "--allow-constraints"
    , "--qualification", "none"
    , "--max-steps", "2048"
    , "a"
    ]
  assertContains "unqualified candidate" "witness" unqualified
  assertBool "none qualification retained a global module prefix" $
    not $ "Fixture.witness" `isInfixOf` unqualified

assertSuccess :: [String] -> IO String
assertSuccess arguments = do
  (exitCode, output, errors) <- runDjex arguments
  assertEqual ("djex stderr: " ++ errors) ExitSuccess exitCode
  assertBool ("successful synthesis emitted an error: " ++ errors) $
    not $ "error" `isInfixOf` map toLower errors
  pure output

assertUsageFailure :: [String] -> String -> Assertion
assertUsageFailure arguments expected = do
  (exitCode, output, errors) <- runDjex arguments
  assertEqual "usage failure exit" (ExitFailure 2) exitCode
  assertEqual "usage failure stdout" "" output
  assertContains "usage diagnostic" expected errors
  assertContains "usage hint" "Try 'djex --help'" errors
  assertNoCallStack errors

runDjex :: [String] -> IO (ExitCode, String, String)
runDjex arguments = readProcessWithExitCode "djex" arguments ""

withTemporaryEnvironment
  :: [(FilePath, String)]
  -> (FilePath -> IO result)
  -> IO result
withTemporaryEnvironment files action = bracket create removePathForcibly use
 where
  create = do
    temporaryRoot <- getTemporaryDirectory
    (path, handle) <- openTempFile temporaryRoot "djex-environment"
    hClose handle
    removeFile path
    createDirectory path
    pure path
  use path = do
    forM_ files $ \(name, contents) -> writeFile (path ++ "/" ++ name) contents
    action path

withMissingPath :: (FilePath -> IO result) -> IO result
withMissingPath action = do
  temporaryRoot <- getTemporaryDirectory
  (path, handle) <- openTempFile temporaryRoot "missing-djex-environment"
  hClose handle
  removeFile path
  action path

assertContains :: String -> String -> String -> Assertion
assertContains message needle haystack = assertBool
  (message ++ ": missing " ++ show needle)
  (needle `isInfixOf` haystack)

assertNoCallStack :: String -> Assertion
assertNoCallStack output = assertBool "controlled failure exposed a CallStack" $
  not $ "CallStack" `isInfixOf` output
