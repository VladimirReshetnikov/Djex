module SMTLibCausalDriverSpec (smtLibCausalDriverTests) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Word (Word8)
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion
  , assertFailure
  , testCase
  , (@?=)
  )

import Language.Haskell.Synthesis.Internal.SMTLib.Causal
  ( SMTLibCausalAction (..) )
import Language.Haskell.Synthesis.Internal.SMTLib.Causal.Driver
  ( SMTLibCausalFailure (..)
  , SMTLibCausalInitialBoundary (..)
  , SMTLibCausalTranscript
  , SMTLibCausalTransportOps (..)
  , driveSMTLibCausalActions
  , smtLibCausalTranscriptByteCount
  , smtLibCausalTranscriptEpochBytes
  , smtLibCausalTranscriptEpochKind
  , smtLibCausalTranscriptEpochs
  , smtLibCausalTranscriptInheritedBytes
  )

smtLibCausalDriverTests :: TestTree
smtLibCausalDriverTests = testGroup "causal SMT-LIB transport driver"
  [ testCase "reject a non-write action before transport demand" $ do
      result <- driveSMTLibCausalActions
        SMTLibCausalRequireEmptyBoundary 0
        (error "feed forced for invalid initial action")
        (error "finish forced for invalid initial action")
        TestUnexpectedBoundary
        (error "transport operations forced for invalid initial action" ::
          SMTLibCausalTransportOps TestTransport TestTransportFailure)
        (error "transport forced for invalid initial action" :: TestTransport)
        (SMTLibCausalAwait TestInitial ::
          SMTLibCausalAction TestKind TestReceiver String)
      assertDriverFailure SMTLibCausalInternalFailure result
  , testCase "write before feeding inherited boundary bytes" $ do
      transport <- newTestTransport
        [] [Right $ bytes " \n"] [Left TestTransportFailure] []
      result <- driveTestMachine
        SMTLibCausalAdoptPredecessorWhitespace 64
        (error "inherited bytes fed after failed write")
        (error "finish forced after failed write")
        transport initialWrite
      assertDriverFailure
        (SMTLibCausalTransportFailure TestTransportFailure) result
      events <- readIORef $ testTransportEvents transport
      events @?=
        [TestDrain, TestWrite $ bytes "command-1"]
  , testCase "retain canonical inherited and per-write transcript bytes" $ do
      transport <- newTestTransport
        [Right ()]
        [Right $ bytes "  ", Right BS.empty]
        [Right (), Right ()]
        [Right $ bytes "first\n", Right $ bytes "second\n"]
      result <- driveTestMachine
        SMTLibCausalRequireEmptyBoundary 64 twoWriteFeed
        (error "finish forced for completed two-write machine")
        transport initialWrite
      case result of
        Left failure -> assertFailure $
          "two-write driver failed: " ++ show failure
        Right (outcome, transcript) -> do
          outcome @?= "complete"
          assertTranscript transcript BS.empty
            [(TestFirst, bytes "first\n  "), (TestSecond, bytes "second\n")]
          smtLibCausalTranscriptByteCount transcript @?= 15
      events <- readIORef $ testTransportEvents transport
      events @?=
        [ TestReady
        , TestWrite $ bytes "command-1"
        , TestNext
        , TestDrain
        , TestWrite $ bytes "command-2"
        , TestNext
        , TestDrain
        ]
  , testCase "reject a stale boundary byte before the next write" $ do
      transport <- newTestTransport
        [Right ()] [] [Right ()]
        [Right $ bytes "first", Right $ bytes " x"]
      result <- driveTestMachine
        SMTLibCausalRequireEmptyBoundary 64 boundaryWriteFeed
        (error "finish forced after stale boundary byte")
        transport initialWrite
      assertDriverFailure
        (SMTLibCausalMachineFailure $ TestUnexpectedBoundary 6 120) result
      events <- readIORef $ testTransportEvents transport
      events @?=
        [ TestReady
        , TestWrite $ bytes "command-1"
        , TestNext
        , TestNext
        ]
  , testCase "let machine EOF classification precede transport EOF" $ do
      machineTransport <- eofTransport
      machineResult <- driveTestMachine
        SMTLibCausalRequireEmptyBoundary 64
        (error "feed forced before EOF")
        (const $ Left $ TestMachineFailure "positional eof")
        machineTransport initialWrite
      assertDriverFailure
        (SMTLibCausalMachineFailure $ TestMachineFailure "positional eof")
        machineResult
      transport <- eofTransport
      transportResult <- driveTestMachine
        SMTLibCausalRequireEmptyBoundary 64
        (error "feed forced before EOF")
        (const $ Right $ SMTLibCausalComplete "ignored")
        transport initialWrite
      assertDriverFailure
        (SMTLibCausalTransportFailure TestStdoutEOF) transportResult
  , testCase "cap cumulative excess at maximum plus one" $ do
      admitted <- successfulSingleWrite 4
      case admitted of
        Left failure -> assertFailure $
          "exact cumulative maximum failed: " ++ show failure
        Right (outcome, transcript) -> do
          outcome @?= "complete"
          smtLibCausalTranscriptByteCount transcript @?= 4
      rejected <- successfulSingleWrite 3
      assertDriverFailure
        (SMTLibCausalCumulativeOutputByteLimitExceeded 3 4) rejected
  ]

data TestKind = TestFirst | TestSecond
  deriving (Eq, Show)

data TestReceiver = TestInitial | TestSecondReceiver
  deriving (Eq, Show)

data TestTransportFailure = TestStdoutEOF | TestTransportFailure
  deriving (Eq, Ord, Show)

data TestMachineFailure
  = TestMachineFailure String
  | TestUnexpectedBoundary Natural Word8
  deriving (Eq, Ord, Show)

data TestTransportEvent
  = TestReady
  | TestDrain
  | TestWrite ByteString
  | TestNext
  deriving (Eq, Show)

data TestTransport = TestTransport
  { testTransportReadyResults :: IORef [Either TestTransportFailure ()]
  , testTransportDrainResults ::
      IORef [Either TestTransportFailure ByteString]
  , testTransportWriteResults :: IORef [Either TestTransportFailure ()]
  , testTransportNextResults ::
      IORef [Either TestTransportFailure ByteString]
  , testTransportEvents :: IORef [TestTransportEvent]
  }

newTestTransport
  :: [Either TestTransportFailure ()]
  -> [Either TestTransportFailure ByteString]
  -> [Either TestTransportFailure ()]
  -> [Either TestTransportFailure ByteString]
  -> IO TestTransport
newTestTransport ready drain write next = TestTransport
  <$> newIORef ready
  <*> newIORef drain
  <*> newIORef write
  <*> newIORef next
  <*> newIORef []

testTransportOps
  :: SMTLibCausalTransportOps TestTransport TestTransportFailure
testTransportOps = SMTLibCausalTransportOps
  { smtLibCausalTransportCheckReady = \transport -> do
      record transport TestReady
      pop "ready" $ testTransportReadyResults transport
  , smtLibCausalTransportDrainBoundaryWhitespace = \transport -> do
      record transport TestDrain
      pop "drain" $ testTransportDrainResults transport
  , smtLibCausalTransportWrite = \transport written -> do
      record transport $ TestWrite written
      pop "write" $ testTransportWriteResults transport
  , smtLibCausalTransportNextStdoutChunk = \transport -> do
      record transport TestNext
      pop "next" $ testTransportNextResults transport
  , smtLibCausalTransportFailureIsStdoutEOF = (== TestStdoutEOF)
  }

record :: TestTransport -> TestTransportEvent -> IO ()
record transport event = modifyIORef' (testTransportEvents transport)
  (++ [event])

pop :: String -> IORef [value] -> IO value
pop label ref = do
  values <- readIORef ref
  case values of
    [] -> error $ "missing scripted " ++ label ++ " result"
    value : remaining -> do
      modifyIORef' ref $ const remaining
      pure value

driveTestMachine
  :: SMTLibCausalInitialBoundary
  -> Natural
  -> (TestReceiver
      -> [Word8]
      -> Either TestMachineFailure
          (SMTLibCausalAction TestKind TestReceiver String))
  -> (TestReceiver
      -> Either TestMachineFailure
          (SMTLibCausalAction TestKind TestReceiver String))
  -> TestTransport
  -> SMTLibCausalAction TestKind TestReceiver String
  -> IO
      (Either
        (SMTLibCausalFailure TestTransportFailure TestMachineFailure)
        (String, SMTLibCausalTranscript TestKind))
driveTestMachine boundary maximumBytes feed finish transport =
  driveSMTLibCausalActions boundary maximumBytes feed finish
    TestUnexpectedBoundary testTransportOps transport

initialWrite :: SMTLibCausalAction TestKind TestReceiver String
initialWrite = SMTLibCausalWrite TestFirst (ascii "command-1") TestInitial

twoWriteFeed
  :: TestReceiver
  -> [Word8]
  -> Either TestMachineFailure
      (SMTLibCausalAction TestKind TestReceiver String)
twoWriteFeed receiver input = case (receiver, input) of
  (TestInitial, observed)
    | observed == ascii "first\n" -> Right $
        SMTLibCausalWrite TestSecond (ascii "command-2") TestSecondReceiver
  (TestSecondReceiver, observed)
    | observed == ascii "  " -> Right $ SMTLibCausalAwait TestSecondReceiver
    | observed == ascii "second\n" -> Right $
        SMTLibCausalComplete "complete"
  _ -> Left $ TestMachineFailure "unexpected feed"

boundaryWriteFeed
  :: TestReceiver
  -> [Word8]
  -> Either TestMachineFailure
      (SMTLibCausalAction TestKind TestReceiver String)
boundaryWriteFeed TestInitial observed
  | observed == ascii "first" = Right $
      SMTLibCausalWrite TestSecond (ascii "command-2") TestSecondReceiver
boundaryWriteFeed _ _ = Left $ TestMachineFailure "unexpected boundary feed"

eofTransport :: IO TestTransport
eofTransport = newTestTransport
  [Right ()] [] [Right ()] [Left TestStdoutEOF]

successfulSingleWrite
  :: Natural
  -> IO
      (Either
        (SMTLibCausalFailure TestTransportFailure TestMachineFailure)
        (String, SMTLibCausalTranscript TestKind))
successfulSingleWrite maximumBytes = do
  transport <- newTestTransport
    [Right ()] [Right BS.empty] [Right ()] [Right $ bytes "ok\n\n"]
  driveTestMachine SMTLibCausalRequireEmptyBoundary maximumBytes
    singleWriteFeed
    (error "finish forced for complete response")
    transport initialWrite

singleWriteFeed
  :: TestReceiver
  -> [Word8]
  -> Either TestMachineFailure
      (SMTLibCausalAction TestKind TestReceiver String)
singleWriteFeed receiver observed = case receiver of
  TestInitial
    | observed == ascii "ok\n\n" -> Right $
        SMTLibCausalComplete "complete"
  _ -> Left $ TestMachineFailure "unexpected single response"

assertTranscript
  :: SMTLibCausalTranscript TestKind
  -> ByteString
  -> [(TestKind, ByteString)]
  -> IO ()
assertTranscript transcript inherited expected = do
  smtLibCausalTranscriptInheritedBytes transcript @?= inherited
  map
      (\epoch ->
        ( smtLibCausalTranscriptEpochKind epoch
        , smtLibCausalTranscriptEpochBytes epoch
        ))
      (smtLibCausalTranscriptEpochs transcript)
    @?= expected

assertDriverFailure
  :: (Eq transportFailure, Show transportFailure, Eq machineFailure,
      Show machineFailure)
  => SMTLibCausalFailure transportFailure machineFailure
  -> Either
      (SMTLibCausalFailure transportFailure machineFailure)
      success
  -> Assertion
assertDriverFailure expected result = case result of
  Left actual -> actual @?= expected
  Right _ -> assertFailure "causal driver unexpectedly succeeded"

bytes :: String -> ByteString
bytes = BS.pack . ascii

ascii :: String -> [Word8]
ascii = map $ fromIntegral . fromEnum
