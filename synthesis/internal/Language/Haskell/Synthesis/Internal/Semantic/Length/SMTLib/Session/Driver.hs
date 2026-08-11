{-# LANGUAGE DeriveGeneric #-}

-- | Package-private causal ownership shared by the readiness and query FSMs.
--
-- A pure machine may expose a receiver only after returning an exact write
-- obligation.  This driver performs that write before feeding the receiver,
-- owns all pipe reads under one absolute deadline, and canonicalizes delayed
-- boundary whitespace independently of operating-system chunking.
module Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session.Driver
  ( lengthSMTLibCausalDriverSchemaTag
  , LengthSMTLibCausalInitialBoundary (..)
  , LengthSMTLibCausalAction (..)
  , LengthSMTLibCausalFailure (..)
  , LengthSMTLibCausalTranscript
  , lengthSMTLibCausalTranscriptInheritedBytes
  , lengthSMTLibCausalTranscriptEpochs
  , lengthSMTLibCausalTranscriptByteCount
  , LengthSMTLibCausalTranscriptEpoch
  , lengthSMTLibCausalTranscriptEpochKind
  , lengthSMTLibCausalTranscriptEpochBytes
  , driveLengthSMTLibCausalActions
  ) where

import Control.DeepSeq (NFData (rnf))
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session.Process
  ( LengthSMTLibProcess
  , LengthSMTLibProcessCancellation
  , LengthSMTLibProcessDeadline
  , LengthSMTLibProcessError
  , LengthSMTLibProcessFailureClass (..)
  , checkLengthSMTLibProcessReady
  , drainLengthSMTLibProcessBoundaryWhitespace
  , lengthSMTLibProcessErrorClass
  , nextLengthSMTLibProcessStdoutChunk
  , writeLengthSMTLibProcess
  )

lengthSMTLibCausalDriverSchemaTag :: [Word8]
lengthSMTLibCausalDriverSchemaTag = ascii
  "djex-length-z3-causal-byte-stream-driver/v1"

-- | Readiness starts before any predecessor output exists.  An ordinary
-- query instead adopts delayed whitespace from the preceding committed run;
-- those bytes are charged to the new receiver but retained in a distinct
-- inherited transcript prefix.
data LengthSMTLibCausalInitialBoundary
  = LengthSMTLibCausalRequireEmptyBoundary
  | LengthSMTLibCausalAdoptPredecessorWhitespace
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibCausalInitialBoundary

data LengthSMTLibCausalAction kind receiver outcome
  = LengthSMTLibCausalWrite !kind [Word8] !receiver
  | LengthSMTLibCausalAwait !receiver
  | LengthSMTLibCausalComplete !outcome

instance
    (NFData kind, NFData receiver, NFData outcome) =>
    NFData (LengthSMTLibCausalAction kind receiver outcome) where
  rnf action = case action of
    LengthSMTLibCausalWrite kind bytes receiver ->
      rnf kind `seq` rnf bytes `seq` rnf receiver
    LengthSMTLibCausalAwait receiver -> rnf receiver
    LengthSMTLibCausalComplete outcome -> rnf outcome

data LengthSMTLibCausalFailure machineFailure
  = LengthSMTLibCausalProcessFailure !LengthSMTLibProcessError
  | LengthSMTLibCausalMachineFailure !machineFailure
  | LengthSMTLibCausalCumulativeOutputByteLimitExceeded !Natural !Natural
  | LengthSMTLibCausalInternalFailure
  deriving (Eq, Ord, Show, Generic)

data LengthSMTLibCausalTranscript kind = LengthSMTLibCausalTranscript
  !ByteString [LengthSMTLibCausalTranscriptEpoch kind]

instance NFData kind => NFData (LengthSMTLibCausalTranscript kind) where
  rnf (LengthSMTLibCausalTranscript inherited epochs) =
    rnf inherited `seq` rnf epochs

data LengthSMTLibCausalTranscriptEpoch kind =
  LengthSMTLibCausalTranscriptEpoch !kind !ByteString

instance NFData kind => NFData (LengthSMTLibCausalTranscriptEpoch kind) where
  rnf (LengthSMTLibCausalTranscriptEpoch kind bytes) =
    rnf kind `seq` rnf bytes

lengthSMTLibCausalTranscriptInheritedBytes
  :: LengthSMTLibCausalTranscript kind
  -> ByteString
lengthSMTLibCausalTranscriptInheritedBytes
    (LengthSMTLibCausalTranscript bytes _) = bytes

lengthSMTLibCausalTranscriptEpochs
  :: LengthSMTLibCausalTranscript kind
  -> [LengthSMTLibCausalTranscriptEpoch kind]
lengthSMTLibCausalTranscriptEpochs
    (LengthSMTLibCausalTranscript _ epochs) = epochs

lengthSMTLibCausalTranscriptByteCount
  :: LengthSMTLibCausalTranscript kind
  -> Natural
lengthSMTLibCausalTranscriptByteCount transcript =
  byteCount (lengthSMTLibCausalTranscriptInheritedBytes transcript) +
  sum (map (byteCount . lengthSMTLibCausalTranscriptEpochBytes)
    $ lengthSMTLibCausalTranscriptEpochs transcript)

lengthSMTLibCausalTranscriptEpochKind
  :: LengthSMTLibCausalTranscriptEpoch kind
  -> kind
lengthSMTLibCausalTranscriptEpochKind
    (LengthSMTLibCausalTranscriptEpoch kind _) = kind

lengthSMTLibCausalTranscriptEpochBytes
  :: LengthSMTLibCausalTranscriptEpoch kind
  -> ByteString
lengthSMTLibCausalTranscriptEpochBytes
    (LengthSMTLibCausalTranscriptEpoch _ bytes) = bytes

-- | Drive one complete pure action machine.  The initial action must be a
-- write.  Every later write is separated from its predecessor by at least one
-- SMT-LIB whitespace delimiter, and no non-whitespace byte may cross that
-- causal boundary.  EOF is offered to the pure machine for exact positional
-- classification before the transport failure is used.
driveLengthSMTLibCausalActions
  :: LengthSMTLibCausalInitialBoundary
  -> Natural
  -> (receiver
      -> [Word8]
      -> Either machineFailure
          (LengthSMTLibCausalAction kind receiver outcome))
  -> (receiver
      -> Either machineFailure
          (LengthSMTLibCausalAction kind receiver outcome))
  -> (Natural -> Word8 -> machineFailure)
  -> LengthSMTLibProcess
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> LengthSMTLibCausalAction kind receiver outcome
  -> IO
      (Either
        (LengthSMTLibCausalFailure machineFailure)
        (outcome, LengthSMTLibCausalTranscript kind))
driveLengthSMTLibCausalActions initialBoundary cumulativeMaximum
    feedReceiver finishReceiver unexpectedBoundaryByte
    process cancellation deadline = start
 where
  start action = case action of
    LengthSMTLibCausalWrite kind bytes receiver -> do
      admitted <- case initialBoundary of
        LengthSMTLibCausalRequireEmptyBoundary -> do
          checked <- checkLengthSMTLibProcessReady
            process cancellation deadline
          pure $ case checked of
            Left failure -> Left $ LengthSMTLibCausalProcessFailure failure
            Right () -> Right BS.empty
        LengthSMTLibCausalAdoptPredecessorWhitespace -> do
          drained <- drainLengthSMTLibProcessBoundaryWhitespace
            process cancellation deadline
          pure $ case drained of
            Left failure -> Left $ LengthSMTLibCausalProcessFailure failure
            Right whitespace -> Right whitespace
      case admitted of
        Left failure -> pure $ Left failure
        Right inherited ->
          issueWrite inherited [] kind bytes receiver inherited
    _ -> pure $ Left LengthSMTLibCausalInternalFailure

  go inherited completed action = case action of
    LengthSMTLibCausalWrite kind bytes receiver
      | null completed -> pure $ Left LengthSMTLibCausalInternalFailure
      | otherwise -> do
          boundary <- collectBoundaryWhitespace inherited completed
          case boundary of
            Left failure -> pure $ Left failure
            Right (whitespace, completed') ->
              issueWrite inherited completed' kind bytes receiver whitespace
    LengthSMTLibCausalAwait _ -> pure $ Left
      LengthSMTLibCausalInternalFailure
    LengthSMTLibCausalComplete outcome ->
      completeAtBoundary inherited completed outcome

  issueWrite inherited completed kind bytes receiver boundaryWhitespace = do
    written <- writeLengthSMTLibProcess process cancellation deadline
      $ BS.pack bytes
    case written of
      Left failure -> pure $ Left $ LengthSMTLibCausalProcessFailure failure
      Right () -> case feedBoundary boundaryWhitespace receiver of
        Left failure -> pure $ Left failure
        Right prepared -> await inherited completed kind []
          (not (null completed) ||
            initialBoundary == LengthSMTLibCausalAdoptPredecessorWhitespace)
          prepared

  feedBoundary whitespace receiver
    | BS.null whitespace = Right receiver
    | otherwise = case feedReceiver receiver $ BS.unpack whitespace of
        Left failure -> Left $ LengthSMTLibCausalMachineFailure failure
        Right (LengthSMTLibCausalAwait prepared) -> Right prepared
        Right _ -> Left LengthSMTLibCausalInternalFailure

  await inherited completed kind chunks boundaryOpen receiver = do
    next <- nextLengthSMTLibProcessStdoutChunk process cancellation deadline
    case next of
      Left failure
        | lengthSMTLibProcessErrorClass failure ==
            LengthSMTLibProcessStdoutEOF ->
            pure $ case finishReceiver receiver of
              Left machineFailure -> Left
                $ LengthSMTLibCausalMachineFailure machineFailure
              Right _ -> Left $ LengthSMTLibCausalProcessFailure failure
        | otherwise -> pure $ Left
            $ LengthSMTLibCausalProcessFailure failure
      Right chunk -> do
        let (priorWhitespace, retainedChunk, nextBoundaryOpen) =
              splitBoundaryWhitespace boundaryOpen chunk
            (nextInherited, nextCompleted) =
              appendPredecessor priorWhitespace inherited completed
            retained
              | BS.null retainedChunk = chunks
              | otherwise = retainedChunk : chunks
            epochRecord = LengthSMTLibCausalTranscriptEpoch kind
              $ BS.concat $ reverse retained
        case feedReceiver receiver $ BS.unpack chunk of
          Left failure -> pure $ Left
            $ LengthSMTLibCausalMachineFailure failure
          Right (LengthSMTLibCausalAwait nextReceiver) ->
            await nextInherited nextCompleted kind retained
              nextBoundaryOpen nextReceiver
          Right nextAction@LengthSMTLibCausalWrite {} ->
            go nextInherited (epochRecord : nextCompleted) nextAction
          Right (LengthSMTLibCausalComplete outcome) ->
            completeAtBoundary nextInherited
              (epochRecord : nextCompleted) outcome

  completeAtBoundary inherited completed outcome = do
    boundary <- collectBoundaryWhitespace inherited completed
    case boundary of
      Left failure -> pure $ Left failure
      Right (_, completed') ->
        let transcript = LengthSMTLibCausalTranscript inherited
              $ reverse completed'
            observed = lengthSMTLibCausalTranscriptByteCount transcript
        in if observed > cumulativeMaximum
          then pure $ Left
            $ LengthSMTLibCausalCumulativeOutputByteLimitExceeded
                cumulativeMaximum (cumulativeMaximum + 1)
          else pure $ Right (outcome, transcript)

  collectBoundaryWhitespace inherited completed = case completed of
    [] -> pure $ Left LengthSMTLibCausalInternalFailure
    LengthSMTLibCausalTranscriptEpoch _ latest : _
      | endsInWhitespace latest -> drainMore BS.empty completed
      | otherwise -> do
          next <- nextLengthSMTLibProcessStdoutChunk
            process cancellation deadline
          case next of
            Left failure -> pure $ Left
              $ LengthSMTLibCausalProcessFailure failure
            Right bytes -> case firstNonWhitespace bytes of
              Just (offset, byte) -> pure $ Left
                $ LengthSMTLibCausalMachineFailure
                $ unexpectedBoundaryByte
                    (byteCount inherited + completedByteCount completed + offset)
                    byte
              Nothing -> case appendLatest bytes completed of
                Nothing -> pure $ Left LengthSMTLibCausalInternalFailure
                Just completed' -> drainMore bytes completed'

  drainMore retained completed = do
    drained <- drainLengthSMTLibProcessBoundaryWhitespace
      process cancellation deadline
    case drained of
      Left failure -> pure $ Left
        $ LengthSMTLibCausalProcessFailure failure
      Right bytes -> case appendLatest bytes completed of
        Nothing -> pure $ Left LengthSMTLibCausalInternalFailure
        Just completed' -> pure $ Right (retained <> bytes, completed')

  appendPredecessor bytes inherited completed
    | BS.null bytes = (inherited, completed)
    | otherwise = case appendLatest bytes completed of
        Just completed' -> (inherited, completed')
        Nothing -> (inherited <> bytes, completed)

  appendLatest bytes epochs = case epochs of
    [] -> Nothing
    LengthSMTLibCausalTranscriptEpoch kind previous : rest -> Just
      $ LengthSMTLibCausalTranscriptEpoch kind (previous <> bytes) : rest

  splitBoundaryWhitespace boundaryOpen bytes
    | not boundaryOpen = (BS.empty, bytes, False)
    | otherwise =
        let (prefix, suffix) = BS.span isSMTLibWhitespaceByte bytes
        in (prefix, suffix, BS.null suffix)

  completedByteCount = sum . map
    (byteCount . lengthSMTLibCausalTranscriptEpochBytes)

endsInWhitespace :: ByteString -> Bool
endsInWhitespace bytes = case BS.unsnoc bytes of
  Nothing -> False
  Just (_, byte) -> isSMTLibWhitespaceByte byte

firstNonWhitespace :: ByteString -> Maybe (Natural, Word8)
firstNonWhitespace bytes = case BS.findIndex
    (not . isSMTLibWhitespaceByte) bytes of
  Nothing -> Nothing
  Just offset -> Just (fromIntegral offset, BS.index bytes offset)

isSMTLibWhitespaceByte :: Word8 -> Bool
isSMTLibWhitespaceByte byte =
  byte == 9 || byte == 10 || byte == 13 || byte == 32

byteCount :: ByteString -> Natural
byteCount = fromIntegral . BS.length

ascii :: String -> [Word8]
ascii = map $ fromIntegral . fromEnum
