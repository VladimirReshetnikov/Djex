module Main (main) where

import Control.Exception (bracket)
import Data.List (isInfixOf, isPrefixOf)
import System.Directory
  (createDirectory, getTemporaryDirectory, removeFile, removePathForcibly)
import System.Exit (ExitCode (..))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)

main :: IO ()
main = defaultMain $ testGroup "Exference CLI integration"
  [ testCase "no arguments print help" testHelp
  , testCase "the shipped environment supports identity search" testIdentity
  , testCase "parse failures are controlled diagnostics" testParseFailure
  , testCase "ill-kinded queries stop before search" testKindFailure
  , testCase "invalid searches never enter reporting modes" testInvalidSearch
  , testCase "invalid verbosity is a controlled usage error" testInvalidVerbosity
  , testCase "repeated inputs are all searched" testRepeatedInputs
  , testCase "conflicting selection modes are rejected" testConflictingModes
  , testCase "short mode contributes structural expression cost" testShortMode
  , testCase "missing environment directories fail closed" testMissingEnvironment
  , testCase "invalid class environments fail closed" testInvalidEnvironment
  , testCase "invalid synonym inventories fail closed" testInvalidSynonyms
  , testCase "invalid environment modules fail closed" testInvalidModule
  , testCase "ill-kinded environments fail before queries" testInvalidKinds
  , testCase "parsed datatypes participate in pattern matching"
      testParsedDatatypePatternMatch
  , testCase "version mode does not load the environment" testVersion
  ]

testHelp :: Assertion
testHelp = do
  output <- runExference []
  assertContains "help should describe invocation" "Usage: exference" output
  assertBool "the removed embedded test mode must stay absent"
    (not $ "--tests" `isInfixOf` output)

-- This is deliberately an end-to-end default-environment test. The shipped
-- ratings include negative bonuses; applying the non-negative heuristic policy
-- to those ratings previously made every CLI query return no results.
testIdentity :: Assertion
testIdentity = do
  output <- runExference ["--first", "a -> a"]
  -- The environment-free simplifier deliberately keeps the checked lambda
  -- instead of assuming that an unqualified Prelude.id is available.
  assertContains "identity should be synthesized" "\\a -> a" output
  assertContains "candidate metrics should describe the emitted queue state"
    "(depth 0.42000000000000004, 3 steps, 145 final queue size)" output
  assertBool "a final queue size must not be reported as a historical maximum"
    (not $ "max pqueue size" `isInfixOf` output)
  assertBool "the adapter's internal clause target must stay hidden"
    (not $ "_djexResult" `isInfixOf` output)
  assertBool "a valid query must not be rejected during input validation"
    (not $ "invalid search input" `isInfixOf` output)

testParseFailure :: Assertion
testParseFailure = do
  (output, errors) <- runExferenceFailure ["--first", "("]
  assertContains "invalid types should carry a controlled diagnostic"
    "could not parse input type:" errors
  assertEqual "parse failure stdout" "" output

testKindFailure :: Assertion
testKindFailure = do
  (output, errors) <- runExferenceFailure
    ["--first", "Data.Maybe.Maybe Data.Maybe.Maybe"]
  assertContains "ill-kinded input should carry a controlled diagnostic"
    "ill-kinded input type:" errors
  assertBool "ill-kinded input must not enter search"
    (not $ "[selecting" `isInfixOf` output)

testInvalidSearch :: Assertion
testInvalidSearch = do
  (output, errors) <- runExferenceFailure
    ["--envUsage", "(forall a. a) -> Int"]
  assertContains "rank-N input should fail at the checked search boundary"
    "NestedForallInGoal" errors
  assertBool "environment-usage reporting must not evaluate an empty trace"
    (not $ "Prelude.last" `isInfixOf` output)

testInvalidVerbosity :: Assertion
testInvalidVerbosity = do
  (output, errors) <- runExferenceFailure
    ["--verbose=wat", "a -> a"]
  assertEqual "invalid verbosity stdout" "" output
  assertContains "invalid verbosity should identify its value"
    "invalid verbosity \"wat\"" errors
  assertBool "invalid verbosity must not expose partial read"
    (not $ "Prelude.read" `isInfixOf` errors)
  assertBool "invalid verbosity must not expose a call stack"
    (not $ "CallStack" `isInfixOf` errors)

testRepeatedInputs :: Assertion
testRepeatedInputs = do
  output <- runExference
    ["--first", "--input", "a -> a", "a -> a"]
  assertEqual "both query results should be printed"
    2 $ countOccurrences "final queue size" output

testConflictingModes :: Assertion
testConflictingModes = do
  (output, errors) <- runExferenceFailure
    ["--first", "--best", "a -> a"]
  assertEqual "conflicting mode stdout" "" output
  assertContains "conflicting modes should be explicit"
    "conflicting selection mode options" errors

testShortMode :: Assertion
testShortMode = do
  ordinary <- runExference ["--first", "a -> a"]
  short <- runExference ["--first", "--short", "a -> a"]
  assertBool "short mode should change structural candidate cost"
    (ordinary /= short)

testMissingEnvironment :: Assertion
testMissingEnvironment = withMissingTemporaryEnvironment $ \environmentDirectory -> do
  (exitCode, output, errors) <- readProcessWithExitCode "exference"
    ["--envdir", environmentDirectory, "a -> a"] ""
  case exitCode of
    ExitFailure _ -> pure ()
    ExitSuccess -> fail "missing environment directory returned success"
  assertContains "directory failures should use the controlled loader diagnostic"
    "could not load source environment:" errors
  assertContains "the typed directory failure should remain visible"
    "EnvironmentDirectoryReadError" errors
  assertEqual "a missing environment must produce no synthesized output" "" output
  assertBool "a controlled directory failure must not expose a Haskell call stack"
    (not $ "CallStack" `isInfixOf` errors)

testInvalidEnvironment :: Assertion
testInvalidEnvironment = withTemporaryEnvironment $ \environmentDirectory -> do
  writeFile (environmentDirectory ++ "/Broken.hs") $ unlines
    [ "module Broken where"
    , "class C a where"
    , "class C a b where"
    ]
  (exitCode, output, errors) <- readProcessWithExitCode "exference"
    ["--envdir", environmentDirectory, "a -> a"] ""
  case exitCode of
    ExitFailure _ -> pure ()
    ExitSuccess -> fail "invalid class environment returned success"
  assertContains "fatal class diagnostics belong on stderr"
    "could not load source environment:" errors
  assertContains "the duplicate class should remain visible"
    "duplicate type class: C (Broken.C)" errors
  assertBool "a failed environment must never enter synthesis"
    (not $ "\\a -> a" `isInfixOf` output)

testInvalidSynonyms :: Assertion
testInvalidSynonyms = withTemporaryEnvironment $ \environmentDirectory -> do
  writeFile (environmentDirectory ++ "/Broken.hs") $ unlines
    [ "module Broken where"
    , "type A = B"
    , "type B = A"
    ]
  (exitCode, output, errors) <- readProcessWithExitCode "exference"
    ["--envdir", environmentDirectory, "a -> a"] ""
  case exitCode of
    ExitFailure _ -> pure ()
    ExitSuccess -> fail "cyclic synonym environment returned success"
  assertContains "fatal synonym diagnostics belong on stderr"
    "TypeDeclarationErrors" errors
  assertContains "the synonym cycle should remain visible"
    "cyclic type synonym" errors
  assertBool "a failed environment must never enter synthesis"
    (not $ "\\a -> a" `isInfixOf` output)

testInvalidModule :: Assertion
testInvalidModule = withTemporaryEnvironment $ \environmentDirectory -> do
  writeFile (environmentDirectory ++ "/Broken.hs") $ unlines
    [ "module Broken where"
    , "broken :: ("
    ]
  (exitCode, output, errors) <- readProcessWithExitCode "exference"
    ["--envdir", environmentDirectory, "a -> a"] ""
  case exitCode of
    ExitFailure _ -> pure ()
    ExitSuccess -> fail "malformed environment module returned success"
  assertContains "fatal parser diagnostics belong on stderr"
    "ModuleParseErrors" errors
  assertBool "a failed environment must never enter synthesis"
    (not $ "\\a -> a" `isInfixOf` output)

testInvalidKinds :: Assertion
testInvalidKinds = withTemporaryEnvironment $ \environmentDirectory -> do
  writeFile (environmentDirectory ++ "/Broken.hs") $ unlines
    [ "module Broken where"
    , "data T = MkT"
    , "bad :: T T"
    ]
  (exitCode, output, errors) <- readProcessWithExitCode "exference"
    ["--envdir", environmentDirectory, "a -> a"] ""
  case exitCode of
    ExitFailure _ -> pure ()
    ExitSuccess -> fail "ill-kinded environment returned success"
  assertContains "shared inventory failure belongs on stderr"
    "InvalidSourceInventory" errors
  assertContains "the kind failure should remain visible"
    "KindMismatch" errors
  assertBool "an unchecked environment must never reach query parsing"
    (not $ "could not parse input type" `isInfixOf` output)

testParsedDatatypePatternMatch :: Assertion
testParsedDatatypePatternMatch =
  withTemporaryEnvironment $ \environmentDirectory -> do
    writeFile (environmentDirectory ++ "/Box.hs") $ unlines
      [ "module Fixture where"
      , "data Box a = Box a"
      ]
    (exitCode, output, errors) <- readProcessWithExitCode "exference"
      [ "--envdir", environmentDirectory
      , "--first"
      , "Fixture.Box a -> a"
      ] ""
    assertEqual ("pattern-match stderr: " ++ errors) ExitSuccess exitCode
    assertEqual "pattern matching should not write to stderr" "" errors
    assertContains "the result must eliminate the parsed Box constructor"
      "Box" output
    assertBool "the parsed datatype must not be silently omitted"
      (not $ "no results" `isInfixOf` output)

testVersion :: Assertion
testVersion = do
  output <- runExference ["--version"]
  assertContains "version should be reported" "exference version 2026.7.12" output
  assertBool "version mode should not parse environment files"
    (not $ "environment warning:" `isInfixOf` output)

runExference :: [String] -> IO String
runExference arguments = do
  (exitCode, output, errors) <-
    readProcessWithExitCode "exference" arguments ""
  assertEqual ("exference stderr: " ++ errors) ExitSuccess exitCode
  assertEqual "exference should not write to stderr" "" errors
  pure output

runExferenceFailure :: [String] -> IO (String, String)
runExferenceFailure arguments = do
  (exitCode, output, errors) <-
    readProcessWithExitCode "exference" arguments ""
  case exitCode of
    ExitFailure _ -> pure ()
    ExitSuccess -> fail "invalid exference invocation returned success"
  pure (output, errors)

countOccurrences :: String -> String -> Int
countOccurrences needle source
  | null needle = 0
  | otherwise = go source
 where
  go remaining
    | needle `isPrefixOf` remaining = 1 + go (drop (length needle) remaining)
    | _ : rest <- remaining = go rest
    | otherwise = 0

withTemporaryEnvironment :: (FilePath -> IO a) -> IO a
withTemporaryEnvironment action = do
  temporaryRoot <- getTemporaryDirectory
  (path, handle) <- openTempFile temporaryRoot "exference-environment"
  hClose handle
  removeFile path
  createDirectory path
  bracket (pure path) removePathForcibly action

-- Reserve a unique temporary name with a file, then remove it so the action is
-- guaranteed to receive a path that does not exist when it begins.
withMissingTemporaryEnvironment :: (FilePath -> IO a) -> IO a
withMissingTemporaryEnvironment action = do
  temporaryRoot <- getTemporaryDirectory
  (path, handle) <- openTempFile temporaryRoot "missing-exference-environment"
  hClose handle
  removeFile path
  action path

assertContains :: String -> String -> String -> Assertion
assertContains message needle haystack = assertBool
  (message ++ ": missing " ++ show needle)
  (needle `isInfixOf` haystack)
