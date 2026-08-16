{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Package-private transport driver for pure causal SMT-LIB machines.
--
-- A pure machine may expose a receiver only after returning an exact write
-- obligation. This driver performs that write before feeding the receiver,
-- owns all transport reads through one caller-owned handle, and canonicalizes
-- delayed boundary whitespace independently of transport chunking. Domain
-- protocols supply only their pure feed/finish operations and closed failure
-- constructors; process, cancellation, and deadline policy stay associated
-- behind one transport operations-and-handle pair.
module Language.Haskell.Synthesis.Internal.SMTLib.Causal.Driver
  ( SMTLibCausalInitialBoundary (..)
  , SMTLibCausalFailure (..)
  , SMTLibCausalTranscript
  , smtLibCausalTranscriptInheritedBytes
  , smtLibCausalTranscriptEpochs
  , smtLibCausalTranscriptByteCount
  , SMTLibCausalTranscriptEpoch
  , smtLibCausalTranscriptEpochKind
  , smtLibCausalTranscriptEpochBytes
  , SMTLibCausalTransportOps (..)
  , driveSMTLibCausalActions
  ) where

import Control.DeepSeq (NFData (rnf))
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Internal.SMTLib.Causal
  ( SMTLibCausalAction (..) )
import Language.Haskell.Synthesis.Internal.SMTLib.Causal.BoundaryWhitespace
  ( SMTLibCausalBoundaryWhitespace
  , concatSMTLibCausalBoundaryWhitespace
  , smtLibCausalBoundaryWhitespaceBytes
  )
import Language.Haskell.Synthesis.Internal.SMTLib.Causal.StdoutChunk
  ( SMTLibCausalStdoutChunk
  , smtLibCausalStdoutChunkBytes
  )
import Language.Haskell.Synthesis.Internal.SMTLib.Lexical
  ( isSMTLibWhitespaceByte )

-- | A new driver may require an empty transport boundary, while a successor
-- operation may adopt delayed whitespace from its committed predecessor.
data SMTLibCausalInitialBoundary
  = SMTLibCausalRequireEmptyBoundary
  | SMTLibCausalAdoptPredecessorWhitespace
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData SMTLibCausalInitialBoundary

-- | Why 'driveSMTLibCausalActions' stopped: a transport operation failed, the
-- pure machine rejected input (including a stale boundary byte reported
-- through the caller's constructor), the completed transcript exceeded the
-- cumulative output maximum (maximum, then observed as maximum plus one), or
-- the machine returned an action the driver protocol does not permit at that
-- point (for example a non-write initial action, or any response other than
-- an await to fed boundary whitespace).
data SMTLibCausalFailure transportFailure machineFailure
  = SMTLibCausalTransportFailure !transportFailure
  | SMTLibCausalMachineFailure !machineFailure
  | SMTLibCausalCumulativeOutputByteLimitExceeded !Natural !Natural
  | SMTLibCausalInternalFailure
  deriving (Eq, Ord, Show, Generic)

type role SMTLibCausalFailure nominal nominal

-- | Every stdout byte the driver consumed while running one machine to
-- completion: the whitespace attributed to the boundary before the first
-- write's response, followed by one epoch per write in write order.  The
-- constructor is hidden; a transcript is produced only by
-- 'driveSMTLibCausalActions'.
data SMTLibCausalTranscript kind = SMTLibCausalTranscript
  !ByteString [SMTLibCausalTranscriptEpoch kind]

type role SMTLibCausalTranscript nominal

instance NFData kind => NFData (SMTLibCausalTranscript kind) where
  rnf (SMTLibCausalTranscript inherited epochs) =
    rnf inherited `seq` rnf epochs

-- | The stdout bytes attributed to one write: the write's kind and the
-- response bytes read for it, including any boundary whitespace drained
-- after the response and before the next write or completion.
data SMTLibCausalTranscriptEpoch kind =
  SMTLibCausalTranscriptEpoch !kind !ByteString

type role SMTLibCausalTranscriptEpoch nominal

instance NFData kind => NFData (SMTLibCausalTranscriptEpoch kind) where
  rnf (SMTLibCausalTranscriptEpoch kind bytes) =
    rnf kind `seq` rnf bytes

-- | Whitespace consumed before the first write's response: the predecessor
-- boundary whitespace adopted under
-- 'SMTLibCausalAdoptPredecessorWhitespace' plus any whitespace prefix of the
-- first response.  Empty under 'SMTLibCausalRequireEmptyBoundary'.
smtLibCausalTranscriptInheritedBytes
  :: SMTLibCausalTranscript kind
  -> ByteString
smtLibCausalTranscriptInheritedBytes
    (SMTLibCausalTranscript bytes _) = bytes

-- | One epoch per write the driver issued, in write order.
smtLibCausalTranscriptEpochs
  :: SMTLibCausalTranscript kind
  -> [SMTLibCausalTranscriptEpoch kind]
smtLibCausalTranscriptEpochs
    (SMTLibCausalTranscript _ epochs) = epochs

-- | Total stdout bytes in the transcript: the inherited bytes plus every
-- epoch's bytes.  This is the quantity 'driveSMTLibCausalActions' compares
-- against its cumulative maximum on completion.
smtLibCausalTranscriptByteCount
  :: SMTLibCausalTranscript kind
  -> Natural
smtLibCausalTranscriptByteCount transcript =
  byteCount (smtLibCausalTranscriptInheritedBytes transcript) +
  sum (map (byteCount . smtLibCausalTranscriptEpochBytes)
    $ smtLibCausalTranscriptEpochs transcript)

-- | The kind carried by the 'SMTLibCausalWrite' this epoch answers.
smtLibCausalTranscriptEpochKind
  :: SMTLibCausalTranscriptEpoch kind
  -> kind
smtLibCausalTranscriptEpochKind
    (SMTLibCausalTranscriptEpoch kind _) = kind

-- | The stdout bytes read in response to this epoch's write, in transport
-- order, including trailing boundary whitespace drained before the next
-- write or completion.
smtLibCausalTranscriptEpochBytes
  :: SMTLibCausalTranscriptEpoch kind
  -> ByteString
smtLibCausalTranscriptEpochBytes
    (SMTLibCausalTranscriptEpoch _ bytes) = bytes

-- | Transport-specific effects used by the causal driver. A caller supplies
-- one transport handle, keeping process, cancellation, and deadline authority
-- associated instead of passing those capabilities independently at each
-- operation.
--
-- The adapter laws are part of this package-private trust boundary:
--
-- * @checkReady@ succeeds only for a live transport with no queued output;
-- * @drainBoundaryWhitespace@ is a finite, caller-bounded, nonblocking FIFO
--   snapshot whose successful opaque receipt proves every byte is SMT-LIB
--   whitespace, and is all-or-nothing (on encountering other bytes it restores
--   them in order and fails);
-- * @write@ succeeds only after every exact byte has been written and flushed;
-- * @nextStdoutChunk@ preserves FIFO order and returns an opaque nonempty
--   strict-byte receipt; the concrete adapter owns its configured byte bound;
-- * the EOF predicate recognizes exactly stdout EOF.
--
-- The driver's cumulative maximum is asserted when the machine completes.
-- The adapter and pure machine must independently bound incremental chunks and
-- work so a nonterminating or oversized source cannot evade productivity.
data SMTLibCausalTransportOps transport transportFailure =
  SMTLibCausalTransportOps
    { smtLibCausalTransportCheckReady
        :: transport -> IO (Either transportFailure ())
    , smtLibCausalTransportDrainBoundaryWhitespace
        :: transport
        -> IO
            (Either transportFailure SMTLibCausalBoundaryWhitespace)
    , smtLibCausalTransportWrite
        :: transport -> ByteString -> IO (Either transportFailure ())
    , smtLibCausalTransportNextStdoutChunk
        :: transport -> IO (Either transportFailure SMTLibCausalStdoutChunk)
    , smtLibCausalTransportFailureIsStdoutEOF
        :: transportFailure -> Bool
    }

type role SMTLibCausalTransportOps nominal nominal

-- | Drive one complete pure action machine. The initial action must be a
-- write. Every later write is separated from its predecessor by at least one
-- SMT-LIB whitespace delimiter, and no non-whitespace byte may cross that
-- causal boundary. EOF is offered to the pure machine for exact positional
-- classification before the transport failure is used. The cumulative
-- maximum must come from the exact sealed plan which produced the initial
-- action, rather than from a separately pairable limits value.
driveSMTLibCausalActions
  :: SMTLibCausalInitialBoundary
  -> Natural
  -> (receiver
      -> [Word8]
      -> Either machineFailure
          (SMTLibCausalAction kind receiver outcome))
  -> (receiver
      -> Either machineFailure
          (SMTLibCausalAction kind receiver outcome))
  -> (Natural -> Word8 -> machineFailure)
  -> SMTLibCausalTransportOps transport transportFailure
  -> transport
  -> SMTLibCausalAction kind receiver outcome
  -> IO
      (Either
        (SMTLibCausalFailure transportFailure machineFailure)
        (outcome, SMTLibCausalTranscript kind))
driveSMTLibCausalActions initialBoundary cumulativeMaximum
    feedReceiver finishReceiver unexpectedBoundaryByte
    transportOps transport = start
 where
  start action = case action of
    SMTLibCausalWrite kind bytes receiver -> do
      admitted <- case initialBoundary of
        SMTLibCausalRequireEmptyBoundary -> do
          checked <- smtLibCausalTransportCheckReady transportOps transport
          pure $ case checked of
            Left failure -> Left $ SMTLibCausalTransportFailure failure
            Right () -> Right $ concatSMTLibCausalBoundaryWhitespace []
        SMTLibCausalAdoptPredecessorWhitespace -> do
          drained <- smtLibCausalTransportDrainBoundaryWhitespace
            transportOps transport
          pure $ case drained of
            Left failure -> Left $ SMTLibCausalTransportFailure failure
            Right whitespace -> Right whitespace
      case admitted of
        Left failure -> pure $ Left failure
        Right inheritedReceipt ->
          issueWriteWithBoundaryReceipt [] kind bytes receiver
            inheritedReceipt
    _ -> pure $ Left SMTLibCausalInternalFailure

  go inherited completed action = case action of
    SMTLibCausalWrite kind bytes receiver
      | null completed -> pure $ Left SMTLibCausalInternalFailure
      | otherwise -> do
          boundary <- collectBoundaryWhitespace inherited completed
          case boundary of
            Left failure -> pure $ Left failure
            Right (whitespace, completed') ->
              issueWrite inherited completed' kind bytes receiver whitespace
    SMTLibCausalAwait _ -> pure $ Left SMTLibCausalInternalFailure
    SMTLibCausalComplete outcome ->
      completeAtBoundary inherited completed outcome

  issueWrite inherited completed kind bytes receiver boundaryWhitespace =
    issueWriteWith (const (inherited, boundaryWhitespace))
      completed kind bytes receiver

  issueWriteWithBoundaryReceipt completed kind bytes receiver
      boundaryWhitespace =
    issueWriteWith
      (\() ->
        let bytes' = smtLibCausalBoundaryWhitespaceBytes boundaryWhitespace
        in (bytes', bytes'))
      completed kind bytes receiver

  issueWriteWith boundaryBytes completed kind bytes receiver = do
    written <- smtLibCausalTransportWrite transportOps transport $ BS.pack bytes
    case written of
      Left failure -> pure $ Left $ SMTLibCausalTransportFailure failure
      Right () ->
        let (inherited, boundaryWhitespace) = boundaryBytes ()
        in case feedBoundary boundaryWhitespace receiver of
          Left failure -> pure $ Left failure
          Right prepared -> await inherited completed kind []
            (not (null completed) ||
              initialBoundary == SMTLibCausalAdoptPredecessorWhitespace)
            prepared

  feedBoundary whitespace receiver
    | BS.null whitespace = Right receiver
    | otherwise = case feedReceiver receiver $ BS.unpack whitespace of
        Left failure -> Left $ SMTLibCausalMachineFailure failure
        Right (SMTLibCausalAwait prepared) -> Right prepared
        Right _ -> Left SMTLibCausalInternalFailure

  await inherited completed kind chunks boundaryOpen receiver = do
    next <- smtLibCausalTransportNextStdoutChunk transportOps transport
    case next of
      Left failure
        | smtLibCausalTransportFailureIsStdoutEOF transportOps failure ->
            pure $ case finishReceiver receiver of
              Left machineFailure -> Left
                $ SMTLibCausalMachineFailure machineFailure
              Right _ -> Left $ SMTLibCausalTransportFailure failure
        | otherwise -> pure $ Left $ SMTLibCausalTransportFailure failure
      Right chunkReceipt -> do
        let chunk = smtLibCausalStdoutChunkBytes chunkReceipt
            (priorWhitespace, retainedChunk, nextBoundaryOpen) =
              splitBoundaryWhitespace boundaryOpen chunk
            (nextInherited, nextCompleted) =
              appendPredecessor priorWhitespace inherited completed
            retained
              | BS.null retainedChunk = chunks
              | otherwise = retainedChunk : chunks
            epochRecord = SMTLibCausalTranscriptEpoch kind
              $ BS.concat $ reverse retained
        case feedReceiver receiver $ BS.unpack chunk of
          Left failure -> pure $ Left $ SMTLibCausalMachineFailure failure
          Right (SMTLibCausalAwait nextReceiver) ->
            await nextInherited nextCompleted kind retained
              nextBoundaryOpen nextReceiver
          Right nextAction@SMTLibCausalWrite {} ->
            go nextInherited (epochRecord : nextCompleted) nextAction
          Right (SMTLibCausalComplete outcome) ->
            completeAtBoundary nextInherited
              (epochRecord : nextCompleted) outcome

  completeAtBoundary inherited completed outcome = do
    boundary <- collectBoundaryWhitespace inherited completed
    case boundary of
      Left failure -> pure $ Left failure
      Right (_, completed') ->
        let transcript = SMTLibCausalTranscript inherited $ reverse completed'
            observed = smtLibCausalTranscriptByteCount transcript
        in if observed > cumulativeMaximum
          then pure $ Left $ SMTLibCausalCumulativeOutputByteLimitExceeded
            cumulativeMaximum (cumulativeMaximum + 1)
          else pure $ Right (outcome, transcript)

  collectBoundaryWhitespace inherited completed = case completed of
    [] -> pure $ Left SMTLibCausalInternalFailure
    SMTLibCausalTranscriptEpoch _ latest : _
      | endsInWhitespace latest -> drainMore BS.empty completed
      | otherwise -> do
          next <- smtLibCausalTransportNextStdoutChunk transportOps transport
          case next of
            Left failure -> pure $ Left $ SMTLibCausalTransportFailure failure
            Right chunkReceipt ->
              let bytes = smtLibCausalStdoutChunkBytes chunkReceipt
              in case firstNonWhitespace bytes of
                Just (offset, byte) -> pure $ Left $ SMTLibCausalMachineFailure
                  $ unexpectedBoundaryByte
                      (byteCount inherited + completedByteCount completed + offset)
                      byte
                Nothing -> case appendLatest bytes completed of
                  Nothing -> pure $ Left SMTLibCausalInternalFailure
                  Just completed' -> drainMore bytes completed'

  drainMore retained completed = do
    drained <- smtLibCausalTransportDrainBoundaryWhitespace
      transportOps transport
    case drained of
      Left failure -> pure $ Left $ SMTLibCausalTransportFailure failure
      Right whitespace ->
        let bytes = smtLibCausalBoundaryWhitespaceBytes whitespace
        in case appendLatest bytes completed of
          Nothing -> pure $ Left SMTLibCausalInternalFailure
          Just completed' -> pure $ Right (retained <> bytes, completed')

  appendPredecessor bytes inherited completed
    | BS.null bytes = (inherited, completed)
    | otherwise = case appendLatest bytes completed of
        Just completed' -> (inherited, completed')
        Nothing -> (inherited <> bytes, completed)

  appendLatest bytes epochs = case epochs of
    [] -> Nothing
    SMTLibCausalTranscriptEpoch kind previous : rest -> Just
      $ SMTLibCausalTranscriptEpoch kind (previous <> bytes) : rest

  splitBoundaryWhitespace boundaryOpen bytes
    | not boundaryOpen = (BS.empty, bytes, False)
    | otherwise =
        let (prefix, suffix) = BS.span isSMTLibWhitespaceByte bytes
        in (prefix, suffix, BS.null suffix)

  completedByteCount = sum . map
    (byteCount . smtLibCausalTranscriptEpochBytes)

endsInWhitespace :: ByteString -> Bool
endsInWhitespace bytes = case BS.unsnoc bytes of
  Nothing -> False
  Just (_, byte) -> isSMTLibWhitespaceByte byte

firstNonWhitespace :: ByteString -> Maybe (Natural, Word8)
firstNonWhitespace bytes = case BS.findIndex
    (not . isSMTLibWhitespaceByte) bytes of
  Nothing -> Nothing
  Just offset -> Just (fromIntegral offset, BS.index bytes offset)

byteCount :: ByteString -> Natural
byteCount = fromIntegral . BS.length
