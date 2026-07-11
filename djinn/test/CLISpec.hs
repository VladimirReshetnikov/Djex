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
    , testCase "an expired search budget reports an undecided result"
        testSearchBudget
    , testCase "class arguments are checked against inferred kinds"
        testClassKindEnforcement
    , testCase "instance output is atomic across all methods"
        testInstanceOutputAtomic
    ]

testInstanceOutputAtomic :: Assertion
testInstanceOutputAtomic = do
    output <- runSession
        [ "class Partial a where impossible :: x -> y; identity :: z -> z"
        , "?instance Partial Bool"
        , "class Total a where keep :: z -> z"
        , "?instance Total Bool"
        , ":quit"
        ]
    assertContains "the unrealizable method is diagnosed"
        "impossible cannot be realized" output
    assertContains "the whole instance failure is explicit"
        "cannot generate instance Partial Bool" output
    assertBool "a failed method must suppress the instance-shaped header" $
        not $ "instance Partial Bool where" `isInfixOf` output
    assertBool "successful siblings must not be emitted as loose methods" $
        not $ "identity a = a" `isInfixOf` output
    assertContains "a complete instance still gets its declaration header"
        "instance Total Bool where" output
    assertContains "a complete instance still gets its method body"
        "keep a = a" output

testClassKindEnforcement :: Assertion
testClassKindEnforcement = do
    output <- runSession
        [ "class Empty a where"
        , "bad ? Empty (Bool a) => b -> b"
        , "?instance Monad Bool"
        , "class Value a where"
        , "class Higher f where use :: f a -> f a"
        , "?instance (Value f, Higher f) => Value x"
        , "good ? Empty c => c -> c"
        , "fine ? Monad m => a -> m a"
        , ":quit"
        ]
    assertContains "an ill-kinded argument to a method-less class is rejected"
        "Error: argument Bool a of class Empty" output
    assertContains "a kind-mismatched instance argument is rejected"
        "Error: argument Bool of class Monad" output
    assertContains "an instance request has one shared kind scope"
        "argument f of class Higher" output
    assertBool "an invalid joint instance must not print a declaration header" $
        not $ "instance (Value f, Higher f) => Value x where"
            `isInfixOf` output
    assertContains "a well-kinded phantom context still works"
        "good a = a" output
    assertContains "a higher-kinded context still works"
        "fine = return" output

testSearchBudget :: Assertion
testSearchBudget = do
    output <- runSession
        [ ":set budget=2"
        , "f ? ((a -> b) -> a) -> a"
        , ":set budget=0"
        , "g ? ((a -> b) -> a) -> a"
        , ":quit"
        ]
    assertContains "an expired budget must not claim unprovability"
        "f: no proof found within budget 2; inhabitation is undecided."
        output
    assertContains "an unlimited search remains a decision procedure"
        "g cannot be realized" output

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
