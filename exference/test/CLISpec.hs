module Main (main) where

import Data.List (isInfixOf)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)

main :: IO ()
main = defaultMain $ testGroup "Exference CLI integration"
  [ testCase "no arguments print help" testHelp
  , testCase "the shipped environment supports identity search" testIdentity
  , testCase "parse failures are controlled diagnostics" testParseFailure
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
  assertContains "identity should be synthesized" "id" output
  assertBool "a valid query must not be rejected during input validation"
    (not $ "invalid search input" `isInfixOf` output)

testParseFailure :: Assertion
testParseFailure = do
  output <- runExference ["--first", "("]
  assertContains "invalid types should carry a controlled diagnostic"
    "could not parse input type:" output

testVersion :: Assertion
testVersion = do
  output <- runExference ["--version"]
  assertContains "version should be reported" "exference version 1.6.0.0" output
  assertBool "version mode should not parse environment files"
    (not $ "environment warning:" `isInfixOf` output)

runExference :: [String] -> IO String
runExference arguments = do
  (exitCode, output, errors) <-
    readProcessWithExitCode "exference" arguments ""
  assertEqual ("exference stderr: " ++ errors) ExitSuccess exitCode
  assertEqual "exference should not write to stderr" "" errors
  pure output

assertContains :: String -> String -> String -> Assertion
assertContains message needle haystack = assertBool
  (message ++ ": missing " ++ show needle)
  (needle `isInfixOf` haystack)
