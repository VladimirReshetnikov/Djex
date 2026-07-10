module Main (main) where

import Data.List (isInfixOf)
import System.Exit (ExitCode(ExitSuccess))
import System.Process (readProcessWithExitCode)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)

main :: IO ()
main = defaultMain $ testGroup "Djinn CLI integration"
    [ testCase "EOF exits a successful session" testEof
    , testCase "same-named assumptions cannot become recursion"
        testSelfReference
    , testCase "nominal empty conversions use explicit elimination"
        testNominalEmptyTypes
    , testCase "failed deletion rolls back the environment"
        testMutationRollback
    , testCase "qualified and underscore names render correctly"
        testIdentifiers
    , testCase "file errors do not terminate the session"
        testFileErrorRecovery
    , testCase "invalid settings and names are controlled parse errors"
        testParseErrors
    ]

testEof :: Assertion
testEof = do
    output <- runSession ["identity ? a -> a"]
    assertContains "identity should be generated"
        "identity a = a" output
    assertContains "EOF should run the exit action" "Bye." output

testSelfReference :: Assertion
testSelfReference = do
    output <- runSession
        [ "token :: a"
        , "token ? a"
        , "fallback :: a"
        , "token ? a"
        , ":quit"
        ]
    assertContains "the unsafe query should be diagnosed"
        "token cannot be safely realized without a recursive self-reference"
        output
    assertContains "a different safe assumption should remain usable"
        "token = fallback" output
    assertBool "recursive output must never be emitted"
        (not $ "token = token" `isInfixOf` output)

testNominalEmptyTypes :: Assertion
testNominalEmptyTypes = do
    output <- runSession
        [ "data EmptyA"
        , "data EmptyB"
        , "same ? EmptyA -> EmptyA"
        , "cast ? EmptyA -> EmptyB"
        , ":quit"
        ]
    assertContains "same-type conversion should be identity"
        "same a = a" output
    assertContains "cross-type conversion should eliminate the empty input"
        "cast = void" output

testMutationRollback :: Assertion
testMutationRollback = do
    output <- runSession
        [ "data Base = Base"
        , "type Alias = Base"
        , ":delete Base"
        , ":environment"
        , ":quit"
        ]
    assertContains "the dependent deletion should fail"
        "Error: cannot delete Base" output
    assertContains "the original datatype should remain"
        "data Base = Base" output
    assertContains "the dependent synonym should remain"
        "type Alias = Base" output

testIdentifiers :: Assertion
testIdentifiers = do
    output <- runSession
        [ "Data.Value.value :: a"
        , "_answer ? a"
        , "(/?) ? b -> b"
        , ":quit"
        ]
    assertContains "qualified assumptions should remain qualified"
        "_answer = Data.Value.value" output
    assertContains "operator targets should print in prefix form"
        "(/?) a = a" output

testFileErrorRecovery :: Assertion
testFileErrorRecovery = do
    output <- runSession
        [ ":load __djinn_file_that_does_not_exist__.djinn"
        , "identity ? a -> a"
        , ":quit"
        ]
    assertContains "the missing file should be reported"
        "Error loading \"__djinn_file_that_does_not_exist__.djinn\"" output
    assertContains "the next command should still run"
        "identity a = a" output

testParseErrors :: Assertion
testParseErrors = do
    output <- runSession
        [ ":set cutoff=0"
        , "case ? a -> a"
        , "bad..name :: a"
        , ":quit"
        ]
    assertEqual "all three invalid commands should be rejected"
        3 (countOccurrences "Cannot parse command" output)

runSession :: [String] -> IO String
runSession commands = do
    (exitCode, output, errors) <-
        readProcessWithExitCode "djinn" [] $ unlines commands
    assertEqual ("djinn stderr: " ++ errors) ExitSuccess exitCode
    assertEqual "djinn should not write to stderr" "" errors
    return output

assertContains :: String -> String -> String -> Assertion
assertContains message needle haystack =
    assertBool (message ++ ": missing " ++ show needle) $
        needle `isInfixOf` haystack

countOccurrences :: String -> String -> Int
countOccurrences needle = count
  where
    count source =
        case dropUntil needle source of
            Nothing -> 0
            Just rest -> 1 + count rest

    dropUntil _ [] = Nothing
    dropUntil target source@(_ : rest)
        | target `isPrefixOf` source = Just $ drop (length target) source
        | otherwise = dropUntil target rest

    isPrefixOf prefix value = take (length prefix) value == prefix
