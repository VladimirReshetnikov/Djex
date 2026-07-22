module Main (main) where

import CLIAssertions (assertContains, countOccurrences)
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
import qualified Language.Haskell.Djex as Djex

main :: IO ()
main = defaultMain $ testGroup "Djex CLI integration"
  [ testCase "global help and version load no backend" testGlobalInformation
  , testCase "REPL dispatch validates startup options without loading"
      testReplDispatch
  , testCase "REPL keeps both backend sessions and settings alive"
      testReplSharedSession
  , testCase "REPL multiline, repeat, and command errors recover"
      testReplInputRecovery
  , testCase "REPL both mode isolates an unavailable backend"
      testReplBackendIsolation
  , testCase "REPL environment replacement is transactional"
      testReplTransactionalLoad
  , testCase "REPL fix policy rebuilds the Exference session"
      testReplFixReload
  , testCase "REPL scripts persist state and reject recursion"
      testReplScripts
  , testCase "REPL history preserves chronological numbering"
      testReplHistory
  , testCase "explicit RTS tuning reaches the application" testRtsOptions
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
  , testCase "Exference recursion helpers require explicit command opt-in"
      testExferenceRecursionPolicy
  , testCase "Exference environment IO failures stay structured"
      testMissingExferenceEnvironment
  , testCase "Exference rejects unsupported source vocabulary"
      testUnsupportedExferenceEnvironment
  , testCase "Exference parse failures are runtime diagnostics"
      testExferenceParseFailure
  , testCase "Exference resource bounds retain successful status"
      testExferenceBounds
  , testCase "Exference all-selection streams through terminal progress"
      testExferenceStreamingSelection
  , testCase "residual constraints remain attached to generated code"
      testResidualConstraintRendering
  ]

testGlobalInformation :: Assertion
testGlobalInformation = do
  (helpExit, help, helpErrors) <- runDjex ["--help"]
  assertEqual "help exit" ExitSuccess helpExit
  assertContains "global help" "djex djinn [OPTION...] TYPE" help
  assertContains "global help" "djex exference [OPTION...] TYPE" help
  assertContains "Djinn default follows its public options"
    ("positive proof-candidate limit (default: "
      ++ show (Djex.optionCutoff Djex.defaultQueryOptions) ++ ")") help
  assertContains "Djinn budget follows its public options"
    ("non-negative choice-point budget; 0 is unlimited (default: "
      ++ maybe "0" show (Djex.optionBudget Djex.defaultQueryOptions) ++ ")")
    help
  assertContains "Exference steps follow its public options"
    ("positive search-step limit (default: "
      ++ show (Djex.exferenceMaximumSteps Djex.defaultExferenceOptions) ++ ")")
    help
  assertContains "Exference queue follows its public options"
    ("non-negative queue limit or unbounded (default: "
      ++ maybe "unbounded" show
          (Djex.exferenceMaximumQueueSize Djex.defaultExferenceOptions)
      ++ ")") help
  assertEqual "help stderr" "" helpErrors

  (versionExit, versionOutput, versionErrors) <- runDjex ["--version"]
  assertEqual "version exit" ExitSuccess versionExit
  assertContains "version output" "djex version 2026.7.17" versionOutput
  assertEqual "version stderr" "" versionErrors

testReplDispatch :: Assertion
testReplDispatch = do
  (helpExit, help, helpErrors) <- runDjex ["repl", "--help"]
  assertEqual "REPL help exit" ExitSuccess helpExit
  assertContains "REPL help usage" "Usage: djex repl [OPTION...]" help
  assertContains "REPL backend startup option"
    "--backend=djinn|exference|both" help
  assertBool "REPL help must not initialize a session" $
    not $ "Djex REPL" `isInfixOf` help
  assertEqual "REPL help stderr" "" helpErrors

  (bareExit, bareOutput, _) <- runDjex []
  assertEqual "bare djex EOF exit" ExitSuccess bareExit
  assertContains "bare djex starts the REPL" "Djex REPL 2026.7.17" bareOutput

  assertUsageFailure ["repl", "--backend", "unknown"] "unknown backend"
  assertUsageFailure
    ["repl", "--backend", "djinn", "--backend", "both"]
    "--backend may be specified only once"
  assertUsageFailure ["repl", "unexpected"]
    "repl takes no positional arguments"

testReplSharedSession :: Assertion
testReplSharedSession = withTemporaryEnvironment [] $ \directory -> do
  (exitCode, output, errors) <- runRepl directory
    [ ":set render expression"
    , "a -> a"
    , ":backend exference"
    , "a -> a"
    , ":djinn a -> a"
    , ":backend"
    , ":compare a -> a"
    ]
  assertEqual "shared REPL exit" ExitSuccess exitCode
  assertEqual "five independent identity results" 5
    $ countOccurrences "\\a -> a" output
  assertContains "backend switch" "Active backend: exference" output
  assertContains "explicit Djinn query did not switch backend"
    "exference\n" output
  assertContains "comparison labels Djinn" "-- Djinn" output
  assertContains "comparison labels Exference" "-- Exference" output
  assertBool "successful shared session emitted an error" $
    not $ "error" `isInfixOf` map toLower errors

testReplInputRecovery :: Assertion
testReplInputRecovery = withTemporaryEnvironment [] $ \directory -> do
  (exitCode, output, errors) <- runRepl directory
    [ ":set render expression"
    , ":{"
    , "a"
    , " -> a"
    , ":}"
    , ":"
    , ":c ignored"
    , ":wat"
    , "a -> a"
    ]
  assertEqual "recovering REPL exit" ExitSuccess exitCode
  assertEqual "multiline, repeat, and post-error results" 3
    $ countOccurrences "\\a -> a" output
  assertContains "ambiguous abbreviation diagnostic"
    "ambiguous command :c" errors
  assertContains "unknown command diagnostic" "unknown command :wat" errors
  assertNoCallStack errors

testReplBackendIsolation :: Assertion
testReplBackendIsolation = withMissingPath $ \missing -> do
  (exitCode, output, errors) <- runDjexInput
    ["repl", "--backend", "both", "--environment", missing]
    $ unlines [":set prompt \"\"", "a -> a", ":quit"]
  assertEqual "isolated both-mode exit" ExitSuccess exitCode
  assertContains "Djinn section survives" "-- Djinn" output
  assertContains "Djinn result survives" "djexResult a = a" output
  assertContains "Exference section is still attempted" "-- Exference" output
  assertContains "missing environment diagnostic"
    "[EXF_ENV_DIRECTORY_READ]" errors
  assertContains "unavailable backend diagnostic"
    "[DJEX_REPL_EXFERENCE_UNAVAILABLE]" errors

testReplTransactionalLoad :: Assertion
testReplTransactionalLoad = withTemporaryEnvironment [] $ \directory ->
  withMissingPath $ \missing -> do
    (exitCode, output, errors) <- runRepl directory
      [ ":backend exference"
      , ":set render expression"
      , ":load " ++ show missing
      , "a -> a"
      , ":cd /"
      , ":reload"
      , "a -> a"
      , ":show environment"
      ]
    assertEqual "transactional load exit" ExitSuccess exitCode
    assertContains "failed replacement retains the old session"
      "retaining the previous session and settings" output
    assertEqual "old session remains usable across failed load and cwd change" 2
      $ countOccurrences "\\a -> a" output
    assertContains "reload retains canonical environment path" directory output
    assertContains "failed load reports missing source"
      "[EXF_ENV_DIRECTORY_READ]" errors

testReplFixReload :: Assertion
testReplFixReload = withTemporaryEnvironment
    [("Fix.hs", unlines
      [ "module Data.Function where"
      , "fix :: (a -> a) -> a"
      ])] $ \directory -> do
  (exitCode, output, errors) <- runRepl directory
    [ ":backend exference"
    , ":set render expression"
    , ":set max-steps 8"
    , "a"
    , ":set +fix"
    , "a"
    ]
  assertEqual "fix reload exit" ExitSuccess exitCode
  assertContains "policy change reloads the source environment"
    "Loaded Exference environment:" output
  assertEqual "fix is introduced only after opt-in" 1
    $ countOccurrences "Data.Function.fix" output
  assertContains "safe policy finds no unrestricted inhabitant first"
    "[DJEX_EXF_NO_RESULT]" errors

testReplScripts :: Assertion
testReplScripts = withTemporaryEnvironment [] $ \directory -> do
  let script = directory ++ "/commands.djex"
      recursive = directory ++ "/recursive.djex"
  writeFile script $ unlines
    [ ":set render expression"
    , ":backend exference"
    , "a -> a"
    ]
  writeFile recursive $ ":script " ++ show recursive ++ "\n"
  (exitCode, output, errors) <- runRepl directory
    [ ":script " ++ show script
    , ":backend"
    , ":script " ++ show recursive
    , "a -> a"
    ]
  assertEqual "script REPL exit" ExitSuccess exitCode
  assertEqual "script and recovered interactive result" 2
    $ countOccurrences "\\a -> a" output
  assertContains "script setting persists" "exference\n" output
  assertContains "recursive script is rejected"
    "[DJEX_REPL_SCRIPT_CYCLE]" errors

testReplHistory :: Assertion
testReplHistory = withTemporaryEnvironment [] $ \directory -> do
  let history = directory ++ "/history"
  -- Haskeline persists newest-first, matching 'historyLines'. The driver
  -- reverses that representation before assigning chronological line numbers.
  writeFile history "old-two\nold-one\n"
  (exitCode, output, errors) <- runDjexInput
    ["repl", "--environment", directory, "--history", history]
    $ unlines [":set prompt \"\"", ":history 1", ":quit"]
  assertEqual "history REPL exit" ExitSuccess exitCode
  assertContains "latest history entry" "2  old-two" output
  assertBool "history selected the oldest entry" $
    not $ "1  old-one" `isInfixOf` output
  assertBool "history session emitted an error" $
    not $ "error" `isInfixOf` map toLower errors

testRtsOptions :: Assertion
testRtsOptions = do
  (exitCode, output, errors) <-
    runDjex ["+RTS", "-K64m", "-RTS", "--version"]
  assertEqual "RTS-tuned version exit" ExitSuccess exitCode
  assertContains "application ran after RTS parsing"
    "djex version 2026.7.17" output
  assertEqual "RTS-tuned version stderr" "" errors

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
  assertUsageFailure ["unknown", "a -> a"] "unknown command"
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
  assertUsageFailure
    ["exference", "--fix", "--fix", "a -> a"]
    "--fix may be specified only once"

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
  assertEqual "Exference promoted definition" "djexResult a = a\n" definition

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

testExferenceRecursionPolicy :: Assertion
testExferenceRecursionPolicy = withTemporaryEnvironment
    [("Fix.hs", unlines
      [ "module Data.Function where"
      , "fix :: (a -> a) -> a"
      ])] $ \directory -> do
  let common =
        [ "exference"
        , "--environment", directory
        , "--select", "first"
        , "--render", "expression"
        , "--max-steps", "8"
        ]
      goal = "a"
  (defaultExit, defaultOutput, defaultErrors) <- runDjex $ common ++ [goal]
  assertEqual "safe default exit" ExitSuccess defaultExit
  assertEqual "safe default output" "" defaultOutput
  assertContains "safe default no-result diagnostic"
    "[DJEX_EXF_NO_RESULT]" defaultErrors
  assertBool "safe default admitted Data.Function.fix" $
    not $ "Data.Function.fix" `isInfixOf` defaultOutput

  allowedOutput <- assertSuccess $ common ++ ["--fix", goal]
  assertContains "explicit recursion-helper result"
    "Data.Function.fix" allowedOutput

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

testExferenceStreamingSelection :: Assertion
testExferenceStreamingSelection =
    withTemporaryEnvironment [] $ \directory -> do
  (exitCode, output, errors) <- runDjex
    [ "exference", "--environment", directory
    , "--select", "all"
    , "--max-steps", "5"
    , "(a -> a) -> a -> a"
    ]
  assertEqual "streaming all-selection exit" ExitSuccess exitCode
  assertEqual "streaming all-selection separators"
    ( "djexResult f1 = f1\n\n-- or\n\n"
      ++ "djexResult f1 b = f1 (f1 b)\n"
    ) output
  assertContains "streaming terminal truncation"
    "[DJEX_SEARCH_TRUNCATED]" errors

testResidualConstraintRendering :: Assertion
testResidualConstraintRendering = withTemporaryEnvironment
    [("Fixture.hs", unlines
      [ "module Fixture where"
      , "data Box a = Box a"
      , "class C a"
      , "(<+>) :: C (Box a) => Box a"
      ])] $ \directory -> do
  unqualified <- search directory "none"
  assertContains "unqualified residual"
    "-- requires: C (Box a)" unqualified
  assertContains "unqualified operator" "(<+>)" unqualified
  assertBool "none qualification retained a global module prefix" $
    not $ "Fixture." `isInfixOf` unqualified

  identifiers <- search directory "identifiers"
  assertContains "identifier-qualified residual"
    "-- requires: Fixture.C (Fixture.Box a)" identifiers
  assertContains "middle-policy operator" "(<+>)" identifiers
  assertBool "middle qualification unexpectedly qualified an operator" $
    not $ "Fixture.<+>" `isInfixOf` identifiers

  fullyQualified <- search directory "full"
  assertContains "fully qualified residual"
    "-- requires: Fixture.C (Fixture.Box a)" fullyQualified
  assertContains "fully qualified operator" "(Fixture.<+>)" fullyQualified
 where
  search directory qualification = do
    (exitCode, output, errors) <- runDjex
      [ "exference", "--environment", directory
      , "--select", "first"
      , "--allow-constraints"
      , "--qualification", qualification
      , "--render", "expression"
      , "--max-steps", "2048"
      , "Fixture.Box a"
      ]
    assertEqual ("residual search stderr: " ++ errors) ExitSuccess exitCode
    assertBool "residual constraints must not be detached as diagnostics" $
      not $ "DJEX_EXF_RESIDUAL_CONSTRAINTS" `isInfixOf` errors
    pure output

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
runDjex arguments = runDjexInput arguments ""

runDjexInput :: [String] -> String -> IO (ExitCode, String, String)
runDjexInput = readProcessWithExitCode "djex"

runRepl
  :: FilePath
  -> [String]
  -> IO (ExitCode, String, String)
runRepl directory inputs = runDjexInput
  ["repl", "--environment", directory]
  $ unlines $ ":set prompt \"\"" : inputs ++ [":quit"]

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

assertNoCallStack :: String -> Assertion
assertNoCallStack output = assertBool "controlled failure exposed a CallStack" $
  not $ "CallStack" `isInfixOf` output
