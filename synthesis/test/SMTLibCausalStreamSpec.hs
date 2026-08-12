module SMTLibCausalStreamSpec (smtLibCausalStreamTests) where

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Data.Word (Word8)
import Numeric.Natural (Natural)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool
  , assertFailure
  , testCase
  , (@?=)
  )

import Language.Haskell.Synthesis.Internal.SMTLib.Causal.Stream
import Language.Haskell.Synthesis.Internal.SMTLib.Stream
  ( SMTLibStreamFramingError (..)
  , SMTLibStreamLimitSource (..)
  , SMTLibStreamLimits
  , mkSMTLibStreamLimits
  , smtLibStreamFrameByteLimit
  , smtLibStreamNestingDepthLimit
  , smtLibStreamTotalByteLimit
  )

smtLibCausalStreamTests :: TestTree
smtLibCausalStreamTests = testGroup "cumulative causal SMT-LIB framing"
  [ testCase "retain one exact frame and cumulative policy" $ do
      let configured = streamLimits 17
          selectedPolicy = mkSMTLibCausalStreamPolicy configured 29
      smtLibCausalStreamPolicyStreamLimits selectedPolicy @?= configured
      smtLibCausalStreamPolicyCumulativeByteLimit selectedPolicy @?= 29
      smtLibStreamTotalByteLimit
        (smtLibCausalStreamPolicyStreamLimits selectedPolicy) @?= 17
      smtLibStreamFrameByteLimit
        (smtLibCausalStreamPolicyStreamLimits selectedPolicy) @?= 64
      smtLibStreamNestingDepthLimit
        (smtLibCausalStreamPolicyStreamLimits selectedPolicy) @?= 8
      rnf selectedPolicy @?= ()
      rnf (startSMTLibCausalStreamCursor selectedPolicy) @?= ()
  , testCase "preserve configured-total ties and cumulative strictness" $ do
      assertLeft
        (SMTLibCausalStreamFramingFailure
          $ SMTLibStreamTotalByteLimitExceeded 3 4)
        $ feedSMTLibCausalStreamCursor
            (startSMTLibCausalStreamCursor $ policy 3 3)
            $ replicate 4 32
      assertLeft
        (SMTLibCausalStreamCumulativeByteLimitExceeded 3 4)
        $ feedSMTLibCausalStreamCursor
            (startSMTLibCausalStreamCursor $ policy 4 3)
            $ replicate 4 32
      atTwo <- expectComplete $ feedSMTLibCausalStreamCursor
        (startSMTLibCausalStreamCursor $ policy 4 5)
        $ ascii "()" ++ replicate 4 32
      assertLeft
        (SMTLibCausalStreamCumulativeByteLimitExceeded 5 6)
        $ continueSMTLibCausalStreamCompletedFrame atTwo
      tiedAtTwo <- expectComplete $ feedSMTLibCausalStreamCursor
        (startSMTLibCausalStreamCursor $ policy 3 5)
        $ ascii "()" ++ replicate 4 32
      assertLeft
        (SMTLibCausalStreamFramingFailure
          $ SMTLibStreamTotalByteLimitExceeded 3 4)
        $ continueSMTLibCausalStreamCompletedFrame tiedAtTwo
  , testCase "continue an untouched tail under its original owner" $ do
      firstFrame <- expectComplete $ feedSMTLibCausalStreamCursor
        (startSMTLibCausalStreamCursor $ policy 16 32)
        $ ascii "sat\nunsat\nX"
      smtLibCausalStreamCompletedFrameBytes firstFrame @?= ascii "sat"
      secondFrame <- expectComplete
        $ continueSMTLibCausalStreamCompletedFrame firstFrame
      smtLibCausalStreamCompletedFrameBytes secondFrame @?= ascii "unsat"
      assertLeft
        (SMTLibCausalStreamUnexpectedBoundaryByte 10 88)
        $ consumeSMTLibCausalStreamBoundaryWhitespace secondFrame
  , testCase "seal one boundary before starting the next write" $ do
      completed <- expectComplete $ feedSMTLibCausalStreamCursor
        (startSMTLibCausalStreamCursor $ policy 16 32)
        $ ascii "(x) \n"
      boundary <- expectRight
        $ consumeSMTLibCausalStreamBoundaryWhitespace completed
      next <- expectComplete $ feedSMTLibCausalStreamCursor
        (startSMTLibCausalStreamCursorAtBoundary boundary)
        $ ascii "sat\n"
      smtLibCausalStreamCompletedFrameBytes next @?= ascii "sat"
  , testCase "bound finite and cyclic boundary whitespace at max plus one" $ do
      completed <- expectComplete $ feedSMTLibCausalStreamCursor
        (startSMTLibCausalStreamCursor $ policy 100 5)
        $ ascii "(x)   "
      assertLeft
        (SMTLibCausalStreamCumulativeByteLimitExceeded 5 6)
        $ consumeSMTLibCausalStreamBoundaryWhitespace completed
      let cyclicWhitespace = 32 : cyclicWhitespace
      cyclicFrame <- expectComplete $ feedSMTLibCausalStreamCursor
        (startSMTLibCausalStreamCursor $ policy 100 5)
        $ ascii "(x)" ++ cyclicWhitespace
      cyclic <- evaluateWithin
        $ consumeSMTLibCausalStreamBoundaryWhitespace cyclicFrame
      assertLeft
        (SMTLibCausalStreamCumulativeByteLimitExceeded 5 6) cyclic
  , testCase "report an exhausted boundary before inspecting its first byte" $ do
      exhausted <- expectComplete $ feedSMTLibCausalStreamCursor
        (startSMTLibCausalStreamCursor $ policy 100 3)
        $ ascii "(x)" ++
            ((error "exhausted boundary byte forced" :: Word8) :
              error "byte after exhausted boundary forced")
      observed <- evaluateWithin
        $ consumeSMTLibCausalStreamBoundaryWhitespace exhausted
      assertLeft
        (SMTLibCausalStreamCumulativeByteLimitExceeded 3 4) observed
  , testCase "leave a completed frame's poison tail lazy" $ do
      observed <- evaluateWithin $ case feedSMTLibCausalStreamCursor
          (startSMTLibCausalStreamCursor $ policy 16 32)
          (ascii "(x)" ++ error "causal completed-frame tail forced") of
        Right (SMTLibCausalStreamComplete completed) ->
          smtLibCausalStreamCompletedFrameBytes completed == ascii "(x)"
        _ -> False
      assertBool "the causal wrapper forced or lost a completed frame" observed
  , testCase "retain lexical EOF classification inside the frame policy" $ do
      pending <- expectPending $ feedSMTLibCausalStreamCursor
        (startSMTLibCausalStreamCursor $ policy 16 32)
        $ ascii "sa"
      assertLeft
        (SMTLibCausalStreamFramingFailure
          $ SMTLibStreamMissingWhitespaceAfterAtom 2)
        $ finishSMTLibCausalStreamCursor pending
  ]

policy :: Natural -> Natural -> SMTLibCausalStreamPolicy
policy total cumulative = mkSMTLibCausalStreamPolicy
  (streamLimits total) cumulative

streamLimits :: Natural -> SMTLibStreamLimits
streamLimits total = mkSMTLibStreamLimits
  SMTLibStreamLimitSource
    { smtLibStreamLimitSourceTotalBytes = total
    , smtLibStreamLimitSourceFrameBytes = 64
    , smtLibStreamLimitSourceNestingDepth = 8
    }

expectPending
  :: Either SMTLibCausalStreamFailure SMTLibCausalStreamStep
  -> IO SMTLibCausalStreamCursor
expectPending result = case result of
  Left failure -> assertFailure $ "unexpected causal-stream rejection: " ++
    show failure
  Right (SMTLibCausalStreamPending cursor) -> pure cursor
  Right SMTLibCausalStreamComplete{} ->
    assertFailure "expected an incomplete causal frame"

expectComplete
  :: Either SMTLibCausalStreamFailure SMTLibCausalStreamStep
  -> IO SMTLibCausalStreamCompletedFrame
expectComplete result = case result of
  Left failure -> assertFailure $ "unexpected causal-stream rejection: " ++
    show failure
  Right (SMTLibCausalStreamComplete completed) -> pure completed
  Right SMTLibCausalStreamPending{} ->
    assertFailure "expected one complete causal frame"

assertLeft
  :: (Eq failure, Show failure)
  => failure
  -> Either failure value
  -> IO ()
assertLeft expected result = case result of
  Left actual -> actual @?= expected
  Right _ -> assertFailure $ "expected rejection: " ++ show expected

expectRight :: Show failure => Either failure value -> IO value
expectRight result = case result of
  Left failure -> assertFailure $ "unexpected rejection: " ++ show failure
  Right value -> pure value

evaluateWithin :: value -> IO value
evaluateWithin value = do
  observed <- timeout 2000000 $ evaluate value
  case observed of
    Nothing -> assertFailure "bounded causal-stream operation did not terminate"
    Just result -> pure result

ascii :: String -> [Word8]
ascii = map $ fromIntegral . fromEnum
