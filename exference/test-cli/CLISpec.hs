module Main (main) where

import CLIAssertions
  ( assertContains
  , assertContainsPath
  , countOccurrences
  )
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.List (isInfixOf, isPrefixOf)
import System.Directory
  (createDirectory, getTemporaryDirectory, removeFile, removePathForcibly)
import System.Exit (ExitCode (..))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)

import Language.Haskell.Djex.Exference
  ( ExferenceOptions (..)
  , defaultExferenceOptions
  )

main :: IO ()
main = defaultMain $ testGroup "Exference CLI integration"
  [ testCase "no arguments print help" testHelp
  , testCase "explicit RTS tuning reaches the application" testRtsOptions
  , testCase "the shipped environment supports identity search" testIdentity
  , testCase "parse failures are controlled diagnostics" testParseFailure
  , testCase "ill-kinded queries stop before search" testKindFailure
  , testCase "rank-N queries synthesize through opaque atoms" testRankNSearch
  , testCase "invalid verbosity is a controlled usage error" testInvalidVerbosity
  , testCase "out-of-range verbosity cannot wrap" testVerbosityComponentOverflow
  , testCase "repeated verbosity cannot overflow" testVerbosityOverflow
  , testCase "repeated inputs are all searched" testRepeatedInputs
  , testCase "conflicting selection modes are rejected" testConflictingModes
  , testCase "query-only flags require an input type" testQueryFlagsNeedInput
  , testCase "short mode contributes structural expression cost" testShortMode
  , testCase "recursion helpers require explicit command opt-in"
      testRecursionPolicy
  , testCase "loader warnings are visible without verbose mode"
      testLoaderWarningVisibility
  , testCase "loader warnings survive fatal environment validation"
      testLoaderWarningsBeforeFatal
  , testCase "session warnings are visible without verbose mode"
      testSessionWarningVisibility
  , testCase "missing environment directories fail closed" testMissingEnvironment
  , testCase "invalid class environments fail closed" testInvalidEnvironment
  , testCase "invalid synonym inventories fail closed" testInvalidSynonyms
  , testCase "invalid environment modules fail closed" testInvalidModule
  , testCase "ill-kinded environments fail before queries" testInvalidKinds
  , testCase "parsed datatypes participate in pattern matching"
      testParsedDatatypePatternMatch
  , testCase "qualification also applies to residual constraints"
      testResidualConstraintQualification
  , testCase "version mode does not load the environment" testVersion
  ]

testHelp :: Assertion
testHelp = do
  output <- runExference []
  assertContains "help should describe invocation" "Usage: exference" output
  assertContains "constraint deferral follows the public default"
    ("constraint deferral steps: "
      ++ show (exferenceConstraintDeferralSteps defaultExferenceOptions)) output
  assertContains "maximum steps follow the public default"
    ("maximum search steps: "
      ++ show (exferenceMaximumSteps defaultExferenceOptions)) output
  assertContains "maximum queue size follows the public default"
    ("maximum queue size: "
      ++ renderBounded (exferenceMaximumQueueSize defaultExferenceOptions)) output
  assertContains "maximum depth follows the public default"
    ("maximum search depth: "
      ++ renderBounded (exferenceMaximumDepth defaultExferenceOptions)) output
  assertBool "the removed embedded test mode must stay absent"
    (not $ "--tests" `isInfixOf` output)

renderBounded :: Show value => Maybe value -> String
renderBounded = maybe "unbounded" show

testRtsOptions :: Assertion
testRtsOptions = do
  output <- runExference ["+RTS", "-K64m", "-RTS", "--version"]
  assertContains "application ran after RTS parsing"
    "exference version 2026.7.17" output

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
    "(depth 0.42000000000000004, 3 steps, 149 final queue size)" output
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

testRankNSearch :: Assertion
testRankNSearch = do
  output <- runExference
    ["--first", "(forall a. a -> a) -> (forall b. b -> b)"]
  assertContains "rank-N identity should use its opaque argument" "\\" output
  assertBool "rank-N input was rejected at the checked search boundary"
    (not $ "NestedForall" `isInfixOf` output)

testInvalidVerbosity :: Assertion
testInvalidVerbosity = do
  (output, errors) <- runExferenceFailure
    ["--verbose=wat", "a -> a"]
  assertEqual "invalid verbosity stdout" "" output
  assertContains "invalid verbosity should identify its value"
    "invalid verbosity \"wat\"" errors
  assertEqual "invalid verbosity should print usage exactly once"
    1 $ countOccurrences "Usage: exference" errors
  assertBool "invalid verbosity must not expose partial read"
    (not $ "Prelude.read" `isInfixOf` errors)
  assertBool "invalid verbosity must not expose a call stack"
    (not $ "CallStack" `isInfixOf` errors)

testVerbosityComponentOverflow :: Assertion
testVerbosityComponentOverflow = do
  let wrappedOne = show $ intModulus + 1
  (output, errors) <- runExferenceFailure
    ["--verbose=" ++ wrappedOne, "a -> a"]
  assertEqual "out-of-range verbosity stdout" "" output
  assertContains "out-of-range verbosity should identify its value"
    ("invalid verbosity " ++ show wrappedOne) errors
  assertEqual "a verbosity error should print usage exactly once"
    1 $ countOccurrences "Usage: exference" errors
  assertBool "out-of-range verbosity must fail before loading the environment"
    (not $ "[Environment]" `isInfixOf` output)

testVerbosityOverflow :: Assertion
testVerbosityOverflow = do
  (output, errors) <- runExferenceFailure
    [ "--verbose=" ++ show (maxBound :: Int)
    , "--verbose=1"
    , "a -> a"
    ]
  assertEqual "overflowing verbosity stdout" "" output
  assertContains "the overflowing sum should report its bound"
    "combined verbosity exceeds maximum " errors
  assertEqual "a verbosity error should print usage exactly once"
    1 $ countOccurrences "Usage: exference" errors
  assertBool "overflowing verbosity must fail before loading the environment"
    (not $ "[Environment]" `isInfixOf` output)

-- Read Int used to accept this plus one as verbosity 1. Computing the modulus
-- from the host bounds makes the subprocess regression architecture-neutral.
intModulus :: Integer
intModulus =
  toInteger (maxBound :: Int) - toInteger (minBound :: Int) + 1

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
  assertContains "conflicts should use public option spellings"
    "--first, --best" errors
  assertBool "internal flag constructors leaked into the diagnostic"
    $ not $ "FirstSol" `isInfixOf` errors

testQueryFlagsNeedInput :: Assertion
testQueryFlagsNeedInput = forM_ queryOnlyOptions $ \option -> do
  (output, errors) <- runExferenceFailure [option]
  assertEqual (option ++ " no-input stdout") "" output
  assertContains (option ++ " should require a query")
    "a search or selection option requires an input type" errors
  assertEqual (option ++ " should print usage exactly once")
    1 $ countOccurrences "Usage: exference" errors
 where
  queryOnlyOptions =
    [ "--all"
    , "--envUsage"
    , "--short"
    , "--first"
    , "--best"
    , "--allowUnused"
    , "--patternMatchMC"
    , "--fullqualification"
    , "--somequalification"
    , "--allowConstraints"
    ]

testShortMode :: Assertion
testShortMode = do
  ordinary <- runExference ["--first", "a -> a"]
  short <- runExference ["--first", "--short", "a -> a"]
  assertBool "short mode should change structural candidate cost"
    (ordinary /= short)

testRecursionPolicy :: Assertion
testRecursionPolicy = withTemporaryEnvironment $ \environmentDirectory -> do
  writeFile (environmentDirectory ++ "/Fix.hs") $ unlines
    [ "module Data.Function where"
    , "fix :: (a -> a) -> a"
    ]
  let inspect =
        [ "--envdir", environmentDirectory
        , "--verbose=1"
        , "--printenv"
        ]
  safe <- runExference inspect
  assertContains "safe default policy omission"
    "DJEX_EXF_POLICY_OMISSION" safe

  allowed <- runExference $ "--fix" : inspect
  assertBool "explicit opt-in retained a policy omission" $
    not $ "DJEX_EXF_POLICY_OMISSION" `isInfixOf` allowed

testLoaderWarningVisibility :: Assertion
testLoaderWarningVisibility =
  withTemporaryEnvironment $ \environmentDirectory -> do
    let ratingPath = environmentDirectory ++ "/Broken.ratings"
    writeFile (environmentDirectory ++ "/Fixture.hs") $ unlines
      [ "module Fixture where"
      , "identity :: a -> a"
      ]
    writeFile ratingPath "Fixture.identity not-a-finite-number"

    (output, errors) <- runExferenceCapture
      [ "--envdir", environmentDirectory
      , "--first"
      , "a -> a"
      ]
    assertContains "a recoverable rating warning must not stop synthesis"
      "\\a -> a" output
    assertContainsPath "the default command must report loader warnings on stderr"
      (ratingPath ++ ": warning: could not parse rating file:") errors
    assertEqual "the loader warning should be emitted exactly once"
      1 $ countOccurrences "warning: could not parse rating file:" errors
    assertBool "loader warnings must not contaminate candidate output"
      $ not $ "warning: could not parse rating file:" `isInfixOf` output
    assertBool "informational loader summaries remain opt-in"
      $ not $ "environment info:" `isInfixOf` output

testLoaderWarningsBeforeFatal :: Assertion
testLoaderWarningsBeforeFatal =
  withTemporaryEnvironment $ \environmentDirectory -> do
    let ratingPath = environmentDirectory ++ "/Broken.ratings"
    writeFile (environmentDirectory ++ "/Broken.hs") $ unlines
      [ "module Broken where"
      , "data T = MkT"
      , "bad :: T T"
      ]
    writeFile ratingPath "Broken.bad not-a-finite-number"

    (output, errors) <- runExferenceFailure
      ["--envdir", environmentDirectory, "a -> a"]
    assertContainsPath "a warning accumulated before validation remains visible"
      (ratingPath ++ ": warning: could not parse rating file:") errors
    assertEqual "the warning preceding a fatal load is emitted exactly once" 1
      $ countOccurrences "warning: could not parse rating file:" errors
    assertContains "the later fatal inventory diagnostic remains visible"
      "error [EXF_SOURCE_INVENTORY]:" errors
    assertBool "a fatally invalid environment cannot enter synthesis"
      $ not $ "\\a -> a" `isInfixOf` output

testSessionWarningVisibility :: Assertion
testSessionWarningVisibility =
  withTemporaryEnvironment $ \environmentDirectory -> do
    writeFile (environmentDirectory ++ "/Recursive.hs") $ unlines
      [ "module Recursive where"
      , "data Nat = Zero | Succ Nat"
      ]

    (output, errors) <- runExferenceCapture
      ["--envdir", environmentDirectory]
    assertEqual "a load-only command should keep stdout quiet" "" output
    assertContains "the default command must report session warnings on stderr"
      "session warning [DJEX_EXF_RECURSIVE_OMISSION]:" errors
    assertContains "the omission should identify the affected datatype"
      "Recursive.Nat" errors
    assertEqual "the recursive fixture should be diagnosed exactly once"
      1 $ countOccurrences "context: Recursive.Nat" errors

testMissingEnvironment :: Assertion
testMissingEnvironment = withMissingTemporaryEnvironment $ \environmentDirectory -> do
  (exitCode, output, errors) <- readProcessWithExitCode "exference"
    ["--envdir", environmentDirectory, "a -> a"] ""
  case exitCode of
    ExitFailure _ -> pure ()
    ExitSuccess -> fail "missing environment directory returned success"
  assertContains "directory failures should use the controlled loader diagnostic"
    "could not load source environment:" errors
  assertContains "directory failures should retain their structured code"
    "error [EXF_ENV_DIRECTORY_READ]:" errors
  assertContains "directory failures should retain their source path"
    (environmentDirectory ++ ": error [EXF_ENV_DIRECTORY_READ]:") errors
  assertBool "the raw directory constructor leaked through the CLI"
    (not $ "EnvironmentDirectoryReadError" `isInfixOf` errors)
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
  assertContainsPath "class diagnostics retain code and declaration span"
    (environmentDirectory
      ++ "/Broken.hs:2:1: error [EXF_CLASS_DECLARATION]:"
      ++ " could not load a source class declaration") errors
  assertContains "the duplicate class should remain visible"
    "duplicate type class: C (Broken.C)" errors
  assertBool "the raw class loader constructor leaked through the CLI"
    (not $ "ClassEnvironmentLoadFailure" `isInfixOf` errors)
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
  assertEqual "the historical loader prefix should be printed once"
    1 $ countOccurrences "could not load source environment:" errors
  assertEqual "both synonym failures should remain accumulated"
    2 $ countOccurrences "[EXF_TYPE_DECLARATION]" errors
  assertContainsPath "the first synonym failure retains its declaration span"
    (environmentDirectory
      ++ "/Broken.hs:2:1-11: error [EXF_TYPE_DECLARATION]:"
      ++ " could not load a source type declaration") errors
  assertContainsPath "the second synonym failure retains its declaration span"
    (environmentDirectory
      ++ "/Broken.hs:3:1-11: error [EXF_TYPE_DECLARATION]:"
      ++ " could not load a source type declaration") errors
  assertContains "the synonym cycle should remain visible"
    "cyclic type synonym" errors
  assertBool "the raw synonym loader constructor leaked through the CLI"
    (not $ "TypeDeclarationErrors" `isInfixOf` errors)
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
  assertContains "fatal parser diagnostics retain their structured code"
    "error [EXF_MODULE_PARSE]:" errors
  assertBool "the raw parser loader constructor leaked through the CLI"
    (not $ "ModuleParseErrors" `isInfixOf` errors)
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
    "error [EXF_SOURCE_INVENTORY]:"
    errors
  assertContains "shared inventory failure keeps its phase message"
    "the source environment failed shared inventory validation" errors
  assertContains "the kind failure should remain visible"
    "KindMismatch" errors
  assertBool "the raw inventory loader constructor leaked through the CLI"
    (not $ "InvalidSourceInventory" `isInfixOf` errors)
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
    assertOnlyWarningDiagnostics errors
    assertContains "the result must eliminate the parsed Box constructor"
      "Box" output
    assertBool "the parsed datatype must not be silently omitted"
      (not $ "no results" `isInfixOf` output)

testResidualConstraintQualification :: Assertion
testResidualConstraintQualification =
  withTemporaryEnvironment $ \environmentDirectory -> do
    writeFile (environmentDirectory ++ "/Fixture.hs") $ unlines
      [ "module Fixture where"
      , "data Box a = Box a"
      , "class C a"
      , "(<+>) :: C (Box a) => Box a"
      ]

    unqualified <- search environmentDirectory []
    assertContains "unqualified residual"
      "but only with additional constraints: C (Box a)" unqualified
    assertContains "unqualified operator" "(<+>)" unqualified
    assertBool "unqualified output retained a module prefix" $
      not $ "Fixture." `isInfixOf` unqualified

    identifiers <- search environmentDirectory ["--somequalification"]
    assertContains "identifier-qualified residual"
      "but only with additional constraints: Fixture.C (Fixture.Box a)"
      identifiers
    assertContains "middle-policy operator" "(<+>)" identifiers
    assertBool "middle qualification unexpectedly qualified an operator" $
      not $ "Fixture.<+>" `isInfixOf` identifiers

    fullyQualified <- search environmentDirectory ["--fullqualification"]
    assertContains "fully qualified residual"
      "but only with additional constraints: Fixture.C (Fixture.Box a)"
      fullyQualified
    assertContains "fully qualified operator" "(Fixture.<+>)" fullyQualified
 where
  search environmentDirectory qualificationFlags = runExference $
    [ "--envdir", environmentDirectory
    , "--first"
    , "--allowConstraints"
    ] ++ qualificationFlags ++ ["Fixture.Box a"]

testVersion :: Assertion
testVersion = do
  output <- runExference ["--version"]
  assertContains "version should be reported" "exference version 2026.7.17" output
  assertBool "version mode should not parse environment files"
    (not $ "environment warning:" `isInfixOf` output)

runExference :: [String] -> IO String
runExference arguments = do
  (output, errors) <- runExferenceCapture arguments
  assertOnlyWarningDiagnostics errors
  pure output

-- Successful commands can report checked loader/session omissions. Keep the
-- general subprocess helper strict enough that an Info diagnostic, raw output,
-- or an accidentally non-fatal Error still fails every ordinary CLI test.
assertOnlyWarningDiagnostics :: String -> Assertion
assertOnlyWarningDiagnostics errors =
  assertBool ("unexpected exference stderr: " ++ errors)
    $ all isWarningLine $ lines errors
 where
  isWarningLine line
    | "  context: " `isPrefixOf` line = True
    | otherwise =
        ("environment " `isPrefixOf` line || "session " `isPrefixOf` line)
          && (": warning" `isInfixOf` line
            || " warning [" `isInfixOf` line)

runExferenceCapture :: [String] -> IO (String, String)
runExferenceCapture arguments = do
  (exitCode, output, errors) <-
    readProcessWithExitCode "exference" arguments ""
  assertEqual ("exference stderr: " ++ errors) ExitSuccess exitCode
  pure (output, errors)

runExferenceFailure :: [String] -> IO (String, String)
runExferenceFailure arguments = do
  (exitCode, output, errors) <-
    readProcessWithExitCode "exference" arguments ""
  case exitCode of
    ExitFailure _ -> pure ()
    ExitSuccess -> fail "invalid exference invocation returned success"
  pure (output, errors)

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
