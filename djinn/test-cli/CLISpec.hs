module Main (main) where

import Data.Maybe (fromMaybe)
import CLIAssertions (assertContains, countOccurrences)
import Control.Exception (bracket)
import Data.Char (isSpace)
import Data.List (intercalate, isInfixOf)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.IO (hClose, hPutStr, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)

import Language.Haskell.Djex.Djinn
    ( QueryOptions (..)
    , defaultQueryOptions
    )

main :: IO ()
main = defaultMain $ testGroup "Djinn CLI integration"
    [ testCase "EOF exits a successful session" testEof
    , testCase "startup settings follow the library query defaults"
        testDefaultSettings
    , testCase "clear restores the standard session and settings"
        testClear
    , testCase "verbose help renders the package version"
        testVerboseHelpVersion
    , testCase "explicit RTS tuning reaches the application" testRtsOptions
    , testCase "same-named assumptions cannot become recursion"
        testSelfReference
    , testCase "nominal empty conversions use explicit elimination"
        testNominalEmptyTypes
    , testCase "failed deletion rolls back the environment"
        testMutationRollback
    , testCase "failed shared sealing rolls back the session"
        testSealRollback
    , testCase "qualified and underscore names render correctly"
        testIdentifiers
    , testCase "file errors do not terminate the session"
        testFileErrorRecovery
    , testCase "invalid settings and names are controlled parse errors"
        testParseErrors
    , testCase "declaration keywords require lexical token boundaries"
        testKeywordBoundaries
    , testCase "an expired search budget reports an undecided result"
        testSearchBudget
    , testCase "bounded rank-N searches distinguish success from uncertainty"
        testBoundedRankNSearch
    , testCase "loaded polymorphic values instantiate at closed monotypes"
        testLoadedPolymorphicValue
    , testCase "class arguments are checked against inferred kinds"
        testClassKindEnforcement
    , testCase "structured query context is rendered once"
        testDiagnosticRendering
    , testCase "instance output is atomic across all methods"
        testInstanceOutputAtomic
    , testCase "startup file errors set aggregate exit status"
        testBatchFailureStatus
    , testCase "missing startup files fail" testMissingBatchFile
    , testCase "options after files still configure the session"
        testOptionsAfterFiles
    , testCase "unknown startup options fail with usage" testUnknownOption
    , testCase "logical negative results are successful batch answers"
        testNegativeBatchResult
    ]

testDefaultSettings :: Assertion
testDefaultSettings = do
    output <- runSession [":help", ":quit"]
    let defaults = defaultQueryOptions
        setting enabled name = (if enabled then "+" else "-") ++ name
        defaultBudget = fromMaybe 0 $ optionBudget defaults
    assertContains "multi follows the public default"
        (setting (optionAlternatives defaults) "multi") output
    assertContains "sorting follows the public default"
        (setting (optionSorted defaults) "sorted") output
    assertContains "candidate cutoff follows the public default"
        ("cutoff=" ++ show (optionCutoff defaults)) output
    assertContains "choice budget follows the public default"
        ("budget=" ++ show defaultBudget) output

testClear :: Assertion
testClear = do
    output <- runSession
        [ ":set +multi"
        , "temporary :: a"
        , ":clear"
        , ":help"
        , ":environment"
        , ":quit"
        ]
    assertContains "clear should restore the default alternatives setting"
        "-multi" output
    assertBool "clear should discard user declarations" $
        not $ "temporary :: a" `isInfixOf` output
    assertContains "clear should restore the standard declarations"
        "data Bool = False | True" output

testVerboseHelpVersion :: Assertion
testVerboseHelpVersion = do
    output <- runSession [":verbose-help", ":quit"]
    case map (dropWhile isSpace) $ filter (isInfixOf welcomePrefix)
        $ lines output of
        [startupWelcome, helpWelcome] ->
            assertEqual "verbose-help example matches the versioned startup"
                startupWelcome helpWelcome
        welcomes -> fail $ "expected two version welcome lines, got "
            ++ show welcomes
    assertBool "verbose help must not expose its source placeholder" $
        not $ "<package-version>" `isInfixOf` output
    assertContains "verbose help qualifies completeness for rank-N types"
        "Rank-N support is deliberately bounded" output
    assertContains "verbose help documents bounded loaded instantiation"
        "Context-free polymorphic functions use a bounded instantiation rule"
        output
    assertContains "verbose help keeps class evidence dictionary-independent"
        "its methods are not proof premises" output
    assertBool "verbose help retained the obsolete instantiation claim" $
        not $ "does *not* instantiate polymorphic functions"
            `isInfixOf` output
  where
    welcomePrefix = "Welcome to Djinn version "

testRtsOptions :: Assertion
testRtsOptions = do
    (exitCode, output, errors) <- readProcessWithExitCode "djinn"
        ["+RTS", "-K64m", "-RTS"] ":quit\n"
    assertEqual ("RTS-tuned Djinn stderr: " ++ errors)
        ExitSuccess exitCode
    assertContains "application ran after RTS parsing"
        "Welcome to Djinn" output
    assertContains "the RTS-tuned session exited normally" "Bye." output
    assertEqual "RTS-tuned Djinn stderr" "" errors

testBatchFailureStatus :: Assertion
testBatchFailureStatus = withCommandFile (unlines
    [ "this is not a command"
    , ":clear"
    , "identity ? a -> a"
    ]) $ \path -> do
        (exitCode, output, errors) <-
            readProcessWithExitCode "djinn" [path] ""
        case exitCode of
            ExitFailure _ -> return ()
            ExitSuccess -> fail "invalid startup file returned success"
        assertEqual "batch command errors remain compatibility stdout"
            "" errors
        assertContains "the parse error should remain visible"
            "Cannot parse command" output
        assertContains "batch processing should continue after the error"
            "identity a = a" output

testMissingBatchFile :: Assertion
testMissingBatchFile = do
    (exitCode, output, _) <- readProcessWithExitCode "djinn"
        ["__djinn_startup_file_that_does_not_exist__.djinn"] ""
    case exitCode of
        ExitFailure _ -> return ()
        ExitSuccess -> fail "missing startup file returned success"
    assertContains "missing startup file should be diagnosed"
        "Error loading" output

testOptionsAfterFiles :: Assertion
testOptionsAfterFiles = withCommandFile
    "choice ? a -> a -> a\n" $ \path -> do
        (defaultExit, defaultOutput, defaultErrors) <-
            readProcessWithExitCode "djinn" [path] ""
        assertEqual ("default djinn batch stderr: " ++ defaultErrors)
            ExitSuccess defaultExit
        assertBool "default mode should select exactly one realization" $
            not $ "-- or" `isInfixOf` defaultOutput

        (exitCode, output, errors) <-
            readProcessWithExitCode "djinn" [path, "+multi"] ""
        assertEqual ("djinn batch stderr: " ++ errors) ExitSuccess exitCode
        assertContains "multi mode should print another realization"
            "-- or" output
        assertBool "the option must not be treated as a filename" $
            not $ "loading file +multi" `isInfixOf` output

testUnknownOption :: Assertion
testUnknownOption = do
    (exitCode, _, errors) <-
        readProcessWithExitCode "djinn" ["-unknown"] ""
    case exitCode of
        ExitFailure _ -> return ()
        ExitSuccess -> fail "unknown startup option returned success"
    assertContains "unknown option should be named"
        "Unknown Djinn option: unknown" errors
    assertContains "unknown option should print usage"
        "Usage: djinn" errors

testNegativeBatchResult :: Assertion
testNegativeBatchResult = withCommandFile
    "peirce ? ((a -> b) -> a) -> a\n" $ \path -> do
        (exitCode, output, errors) <-
            readProcessWithExitCode "djinn" [path] ""
        assertEqual ("djinn batch stderr: " ++ errors) ExitSuccess exitCode
        assertContains "uninhabitability is a successful logical result"
            "peirce cannot be realized" output

testInstanceOutputAtomic :: Assertion
testInstanceOutputAtomic = do
    output <- runSession
        [ "class Partial a where impossible :: x -> y; identity :: z -> z"
        , "?instance Partial Bool"
        , "class Total a where keep :: z -> z"
        , "?instance Total Bool"
        , ":set +multi"
        , "class Choice a where choose :: x -> x -> x"
        , "?instance Choice Bool"
        , ":set -multi"
        , ":set +debug"
        , "class Structured a where swapEither :: Either x y -> Either y x"
        , "?instance Structured Bool"
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
    assertContains "multi mode still emits the complete instance"
        "instance Choice Bool where" output
    assertEqual "an instance selects exactly one method realization"
        1 (countOccurrences "\n   choose " output)
    assertBool "query alternatives must not become overlapping method equations" $
        not $ "-- or" `isInfixOf` output
    assertBool "debug metadata must not split an instance layout block" $
        not $ any (`isInfixOf` output) ["***", "+++"]
    assertContains "every physical line of a generated method stays indented"
        (intercalate "\n"
            [ "instance Structured Bool where"
            , "   swapEither a ="
            , "     case a of"
            , "     Left b -> Right b"
            , "     Right c -> Left c"
            ]) output

testClassKindEnforcement :: Assertion
testClassKindEnforcement = do
    output <- runSession
        [ "class Empty a where"
        , "bad ? Empty (Bool a) => a -> b -> b"
        , "?instance Monad Bool"
        , "class Value a where"
        , "class Higher f where use :: f a -> f a"
        , "?instance (Value f, Higher f) => Value x"
        , "class Capture a where capture :: f a"
        , "safe ? Capture f => f -> x -> x"
        , "class Independent a where left :: f a; right :: f"
        , ":environment"
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
    assertContains "a context argument cannot capture a method-local variable"
        "safe _ a = a" output
    assertBool "capture avoidance must prevent a cyclic kind" $
        not $ "cyclic kind" `isInfixOf` output
    assertContains "same-spelled variables remain local to each class method"
        "class Independent a where left :: f a; right :: f" output
    assertBool "independent method scopes must not cause a class kind mismatch" $
        not $ "Error: class Independent:" `isInfixOf` output
    assertContains "a well-kinded phantom context still works"
        "good a = a" output
    assertContains "an essential higher-kinded method is not assumed"
        "fine cannot be realized" output

testDiagnosticRendering :: Assertion
testDiagnosticRendering = do
    output <- runSession
        [ "bad ? Bool Bool"
        , ":quit"
        ]
    let context = "goal type Bool Bool: KindMismatch ProperTypeKind " ++
            "(FunctionKind ProperTypeKind ProperTypeKind)"
    assertContains "the stable query diagnostic code should remain visible"
        "[DJEX_DJINN_QUERY]" output
    assertEqual "the structured renderer should own context presentation"
        1 (countOccurrences context output)

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

testBoundedRankNSearch :: Assertion
testBoundedRankNSearch = do
    output <- runSession
        [ "instantiate ? (forall a. a) -> b"
        , "twoOpaque ? (forall a. a) -> (forall b. b -> q) -> " ++
            "((forall c. c), (forall d. d -> q), (forall e. e -> e))"
        , "inconclusive ? " ++
            "(forall a b c d e f. a -> (a, b, c, d, e, f)) -> g"
        , ":quit"
        ]
    assertContains "hypothesis instantiation realizes the opaque elimination"
        "instantiate a = a" output
    assertContains "the dual rank-N frontier solves two opaque siblings"
        "twoOpaque a b = (a, b, \\c -> c)" output
    assertContains "an uninstantiable rank-N chain remains inconclusive"
        ("inconclusive: no proof found in the supported inference fragment; " ++
            "inhabitation is undecided.") output
    assertBool "a valid inconclusive result was reported as an internal error"
        $ not $ "Djinn returned no evidence" `isInfixOf` output

testLoadedPolymorphicValue :: Assertion
testLoadedPolymorphicValue = do
    output <- runSession
        [ "type Input :: *"
        , "type Result :: *"
        , "seed :: Input"
        , "consume :: forall item. item -> Result"
        , "use ? Result"
        , "type FairInput :: *"
        , "type FairResult :: *"
        , "type FairJunk :: *"
        , "type FairX1 :: *"
        , "type FairX2 :: *"
        , "type FairX3 :: *"
        , "type FairX4 :: *"
        , "fairX1 :: FairX1"
        , "fairX2 :: FairX2"
        , "fairX3 :: FairX3"
        , "fairX4 :: FairX4"
        , "aaaVacuous :: forall a b c d. FairJunk"
        , "fairSeed :: FairInput"
        , "mmmConsume :: forall item. item -> FairResult"
        , "zzzVacuous :: forall a b c d. FairJunk"
        , "fairUse ? FairResult"
        , ":quit"
        ]
    assertContains "a loaded forall provider should reach the closed result"
        "use = consume" output
    assertContains "the composition should consume the loaded closed value"
        "seed" output
    assertBool "a valid loaded-provider composition was falsely refuted"
        $ not $ "use cannot be realized" `isInfixOf` output
    assertContains
        "vacuous wide schemes must not starve a later useful provider"
        "fairUse = mmmConsume" output
    assertBool "scheme starvation made a supported query inconclusive"
        $ not $ "fairUse: no proof found" `isInfixOf` output

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
        , ":clear"
        , "type SelfInput :: *"
        , "type SelfBox :: * -> *"
        , "selfSeed :: SelfInput"
        , "selfUse :: forall a. a -> SelfBox a"
        , "selfUse ? SelfBox SelfInput"
        , ":clear"
        , "type NominalInput :: *"
        , "type NominalLocked :: *"
        , "data NominalBox a = NominalBox a NominalLocked"
        , "nominalSeed :: NominalInput"
        , "nominalUse :: forall a. a -> NominalBox a"
        , "nominalUse ? NominalBox NominalInput"
        , ":clear"
        , "type UnusedInput :: *"
        , "type UnusedLocked :: *"
        , "data UnusedBox a = UnusedBox a UnusedLocked"
        , "unusedUse :: forall a. a -> UnusedBox a"
        , "unusedUse ? UnusedBox UnusedInput"
        , ":quit"
        ]
    assertContains "the unsafe query should be diagnosed"
        "token cannot be safely realized without a recursive self-reference"
        output
    assertContains "a different safe assumption should remain usable"
        "token = fallback" output
    assertContains "a specialized polymorphic target stays diagnostic-only"
        ("selfUse cannot be safely realized without a recursive " ++
            "self-reference") output
    assertContains "a nominal tail preserves target-reference evidence"
        ("nominalUse cannot be safely realized without a recursive " ++
            "self-reference") output
    assertContains "an unusable target-only scheme preserves refutation"
        "unusedUse cannot be realized" output
    assertBool "recursive output must never be emitted"
        (not $ "token = token" `isInfixOf` output)
    assertBool "target instantiation axioms must never become output"
        (not $ any (`isInfixOf` output)
            ["selfUse =", "nominalUse =", "unusedUse ="])

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
        "cast a = case a of {}" output

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

testSealRollback :: Assertion
testSealRollback = do
    output <- runSession
        [ "type Bad a = b"
        , ":environment"
        , "identity ? a -> a"
        , ":quit"
        ]
    assertContains "the shared sealing failure lost its stable code"
        "DJEX_DJINN_ENV" output
    assertBool "the rejected declaration appeared in the environment"
        (not $ "type Bad a" `isInfixOf` output)
    assertContains "the prior session stopped answering after rollback"
        "identity a = a" output

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

testKeywordBoundaries :: Assertion
testKeywordBoundaries = do
    output <- runSession
        [ "typeFoo"
        , "dataBar"
        , "classQuux a where identity :: a -> a"
        , "class Quux a whereidentity :: a -> a"
        , "?instanceQuux"
        , "type Identity a = a"
        , "data Token = Token"
        , "class Empty a where"
        , "?instance(Empty Bool) => Empty Bool"
        , "recover ? Identity Token -> Token"
        , ":e"
        , ":q"
        ]
    assertEqual "every glued declaration keyword should be rejected"
        5 (countOccurrences "Cannot parse command" output)
    assertContains "parsing should recover for a later valid query"
        "recover a = a" output
    assertContains "punctuation remains a valid instance-keyword boundary"
        "instance (Empty Bool) => Empty Bool where" output
    assertContains "command-prefix abbreviations remain supported"
        "data Token = Token" output
    assertContains "the abbreviated quit command should still run"
        "Bye." output

    withCommandFile (unlines
        [ "class CommentBoundary a where-- trailing comment"
        , "?instance CommentBoundary Bool"
        ]) $ \path -> do
            (exitCode, commentOutput, errors) <-
                readProcessWithExitCode "djinn" [path] ""
            assertEqual ("djinn batch stderr: " ++ errors)
                ExitSuccess exitCode
            assertEqual "djinn should not write comment-boundary errors"
                "" errors
            assertContains "a line comment is a valid keyword boundary"
                "instance CommentBoundary Bool where" commentOutput

runSession :: [String] -> IO String
runSession commands = do
    (exitCode, output, errors) <-
        readProcessWithExitCode "djinn" [] $ unlines commands
    assertEqual ("djinn stderr: " ++ errors) ExitSuccess exitCode
    assertEqual "djinn should not write to stderr" "" errors
    return output

withCommandFile :: String -> (FilePath -> IO result) -> IO result
withCommandFile contents action = bracket create removeFile action
  where
    create = do
        temporaryDirectory <- getTemporaryDirectory
        (path, handle) <- openTempFile temporaryDirectory "djinn-script"
        hPutStr handle contents
        hClose handle
        return path
