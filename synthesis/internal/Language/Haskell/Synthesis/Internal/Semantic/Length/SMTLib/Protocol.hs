{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Package-private, pure framing and phase control for one Length/Z3 query.
--
-- This module owns the causal boundary between the initial @check-sat@ write
-- and a conditional @get-value@ write.  It recursively consumes stream tails
-- only while those frames answer commands which have already been written.
-- Once the status barrier is accepted, any remaining bytes must be bounded
-- SMT-LIB whitespace before a value-write action is returned.  Consequently
-- a valid-looking valuation already buffered before @get-value@ cannot be
-- mistaken for the answer to that later command.
--
-- Plans, receivers, and decoded outcomes are deliberately package-private.
-- Caller-supplied nonce bytes can be fabricated and their construction proves
-- neither freshness nor process execution.  A later live layer must generate
-- session-wide distinct barriers, attest and probe its worker, enforce the
-- returned write actions, and poison that worker after every 'Left'.
module Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol
  ( lengthSMTLibProtocolPlanSchemaTag
  , lengthSMTLibProtocolPhaseMachineSchemaTag
  , lengthSMTLibProtocolPostBarrierSchemaTag
  , LengthSMTLibProtocolLimitSource (..)
  , LengthSMTLibProtocolLimits
  , mkLengthSMTLibProtocolLimits
  , defaultLengthSMTLibProtocolLimitSource
  , defaultLengthSMTLibProtocolLimits
  , lengthSMTLibProtocolStreamLimits
  , lengthSMTLibProtocolCumulativeStdoutByteLimit
  , lengthSMTLibProtocolPlanFingerprintByteLimit
  , LengthSMTLibProtocolBarrier (..)
  , LengthSMTLibProtocolRequiredFrame (..)
  , LengthSMTLibProtocolRequiredLimit (..)
  , LengthSMTLibProtocolPlanError (..)
  , LengthSMTLibProtocolPlan
  , LengthSMTLibProtocolPlanFingerprintSubject
  , sealLengthSMTLibProtocolPlan
  , lengthSMTLibProtocolInitialWriteBytes
  , lengthSMTLibProtocolInputValueWriteBytes
  , lengthSMTLibProtocolPlanFingerprint
  , lengthSMTLibProtocolPlanMinimumStdoutByteCount
  , LengthSMTLibProtocolPhase (..)
  , LengthSMTLibProtocolWriteKind (..)
  , LengthSMTLibProtocolReceiver
  , lengthSMTLibProtocolReceiverPhase
  , LengthSMTLibProtocolAction (..)
  , startLengthSMTLibProtocol
  , feedLengthSMTLibProtocol
  , finishLengthSMTLibProtocol
  , LengthSMTLibProtocolError (..)
  , LengthSMTLibProtocolDecoded
  , lengthSMTLibProtocolDecodedStatus
  , lengthSMTLibProtocolDecodedStatusFrame
  , lengthSMTLibProtocolDecodedInputValueFrame
  , lengthSMTLibProtocolDecodedInputValues
  , lengthSMTLibProtocolDecodedPlanFingerprint
  ) where

import Control.DeepSeq (NFData (rnf))
import Data.Bifunctor (first)
import Data.List (genericLength)
import Data.Maybe (isJust)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import qualified Language.Haskell.Synthesis.Fingerprint as PublicFingerprint
import Language.Haskell.Synthesis.Internal.Fingerprint
  ( Fingerprint
  , FingerprintBuilder (..)
  , FingerprintField (..)
  , FingerprintLimitError (..)
  , buildFingerprintWithin
  , fingerprintCanonicalBytes
  )
import Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Execution
  ( LengthSMTLibArtifactPolicy (..)
  , LengthSMTLibExecutionConfig
  , lengthSMTLibExecutionArtifactPolicy
  , lengthSMTLibExecutionPolicyFingerprint
  , lengthSMTLibExecutionQueryResetBytes
  , lengthSMTLibExecutionResponseLimits
  )
import Language.Haskell.Synthesis.Internal.SMTLib.Stream
  ( SMTLibEchoSentinel
  , SMTLibEchoSentinelError
  , SMTLibStreamFramer
  , SMTLibStreamFramingError (..)
  , SMTLibStreamFramingStep (..)
  , SMTLibStreamLimitSource (..)
  , SMTLibStreamLimits
  , defaultSMTLibStreamLimitSource
  , feedSMTLibStreamFramer
  , finishSMTLibStreamFramer
  , isExactSMTLibEchoSentinelResponse
  , mkSMTLibEchoSentinel
  , mkSMTLibStreamLimits
  , smtLibEchoSentinelCommandBytes
  , smtLibEchoSentinelResponseBytes
  , smtLibStreamFrameByteLimit
  , smtLibStreamFramingSchemaTag
  , smtLibStreamNestingDepthLimit
  , smtLibStreamTotalByteLimit
  , startSMTLibStreamFramer
  )
import Language.Haskell.Synthesis.Semantic.Length.SMTLib
  ( LengthSMTLibIntegerBinding
  , LengthSMTLibQuery
  , lengthSMTLibQueryCheckBytes
  , lengthSMTLibQueryFingerprint
  , lengthSMTLibQueryInputSymbols
  , lengthSMTLibQueryInputValueRequestBytes
  )
import Language.Haskell.Synthesis.Semantic.Length.SMTLib.Response
  ( LengthSMTLibResponseError
  , lengthSMTLibResponseByteLimit
  , lengthSMTLibResponseNestingDepthLimit
  , lengthSMTLibResponseNodeLimit
  , lengthSMTLibResponseSchemaTag
  , lengthSMTLibResponseTokenByteLimit
  , parseLengthSMTLibCheckResponse
  , parseLengthSMTLibInputValueResponse
  )
import Language.Haskell.Synthesis.Semantic.Observation
  ( SolverStatus (..))

-- | Complete pure-plan schema.  This is distinct from a live session or run
-- identity: no process has been opened, inspected, or observed here.
lengthSMTLibProtocolPlanSchemaTag :: [Word8]
lengthSMTLibProtocolPlanSchemaTag =
  ascii "djex-length-z3-smtlib2-protocol-plan/v1"

-- | Ordered write, decode, and exact positional barrier policy.
lengthSMTLibProtocolPhaseMachineSchemaTag :: [Word8]
lengthSMTLibProtocolPhaseMachineSchemaTag =
  ascii "djex-length-z3-smtlib2-protocol-phase-machine/v1"

-- | Only the four SMT-LIB whitespace bytes may remain in the chunk which
-- crosses a barrier into a new write or terminal action.  Comments are not
-- accepted as transport trivia.
lengthSMTLibProtocolPostBarrierSchemaTag :: [Word8]
lengthSMTLibProtocolPostBarrierSchemaTag =
  ascii "djex-smtlib2-post-barrier-whitespace/v1"

-- | Raw pure-protocol bounds.  Stream limits are semantic and reset for each
-- expected frame.  The cumulative stdout limit charges every consumed frame
-- byte and every accepted post-barrier whitespace byte across the transaction.
-- The fingerprint limit is admission-only and is not part of plan identity.
data LengthSMTLibProtocolLimitSource = LengthSMTLibProtocolLimitSource
  { lengthSMTLibProtocolLimitSourceStreamLimits :: SMTLibStreamLimitSource
  , lengthSMTLibProtocolLimitSourceCumulativeStdoutBytes :: Natural
  , lengthSMTLibProtocolLimitSourcePlanFingerprintBytes :: Natural
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibProtocolLimitSource

data LengthSMTLibProtocolLimits = LengthSMTLibProtocolLimits
  !SMTLibStreamLimits !Natural !Natural
  deriving (Eq, Ord)

instance NFData LengthSMTLibProtocolLimits where
  rnf (LengthSMTLibProtocolLimits stream cumulative fingerprint) =
    rnf stream `seq` rnf cumulative `seq` rnf fingerprint

mkLengthSMTLibProtocolLimits
  :: LengthSMTLibProtocolLimitSource
  -> LengthSMTLibProtocolLimits
mkLengthSMTLibProtocolLimits source = LengthSMTLibProtocolLimits
  (mkSMTLibStreamLimits
    $ lengthSMTLibProtocolLimitSourceStreamLimits source)
  (lengthSMTLibProtocolLimitSourceCumulativeStdoutBytes source)
  (lengthSMTLibProtocolLimitSourcePlanFingerprintBytes source)

defaultLengthSMTLibProtocolLimitSource :: LengthSMTLibProtocolLimitSource
defaultLengthSMTLibProtocolLimitSource = LengthSMTLibProtocolLimitSource
  { lengthSMTLibProtocolLimitSourceStreamLimits =
      defaultSMTLibStreamLimitSource
  , lengthSMTLibProtocolLimitSourceCumulativeStdoutBytes = 524288
  , lengthSMTLibProtocolLimitSourcePlanFingerprintBytes = 262144
  }

defaultLengthSMTLibProtocolLimits :: LengthSMTLibProtocolLimits
defaultLengthSMTLibProtocolLimits =
  mkLengthSMTLibProtocolLimits defaultLengthSMTLibProtocolLimitSource

lengthSMTLibProtocolStreamLimits
  :: LengthSMTLibProtocolLimits
  -> SMTLibStreamLimits
lengthSMTLibProtocolStreamLimits
    (LengthSMTLibProtocolLimits value _ _) = value

lengthSMTLibProtocolCumulativeStdoutByteLimit
  :: LengthSMTLibProtocolLimits
  -> Natural
lengthSMTLibProtocolCumulativeStdoutByteLimit
    (LengthSMTLibProtocolLimits _ value _) = value

lengthSMTLibProtocolPlanFingerprintByteLimit
  :: LengthSMTLibProtocolLimits
  -> Natural
lengthSMTLibProtocolPlanFingerprintByteLimit
    (LengthSMTLibProtocolLimits _ _ value) = value

data LengthSMTLibProtocolBarrier
  = LengthSMTLibProtocolCheckBarrier
  | LengthSMTLibProtocolInputValueBarrier
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibProtocolBarrier

data LengthSMTLibProtocolRequiredFrame
  = LengthSMTLibProtocolCheckStatusFrame
  | LengthSMTLibProtocolCheckBarrierFrame
  | LengthSMTLibProtocolInputValueFrame
  | LengthSMTLibProtocolInputValueBarrierFrame
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibProtocolRequiredFrame

data LengthSMTLibProtocolRequiredLimit
  = LengthSMTLibProtocolStreamTotalBytes
  | LengthSMTLibProtocolStreamFrameBytes
  | LengthSMTLibProtocolStreamNestingDepth
  | LengthSMTLibProtocolResponseBytes
  | LengthSMTLibProtocolResponseNestingDepth
  | LengthSMTLibProtocolResponseNodes
  | LengthSMTLibProtocolResponseTokenBytes
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibProtocolRequiredLimit

-- | Fixed-precedence plan admission failures.  No constructor retains nonce,
-- marker, executable-path, or executable-pin material.
data LengthSMTLibProtocolPlanError
  = LengthSMTLibProtocolRequiredLimitTooSmall
      !LengthSMTLibProtocolRequiredFrame
      !LengthSMTLibProtocolRequiredLimit
      !Natural
      !Natural
  | LengthSMTLibProtocolMinimumStdoutByteLimitExceeded !Natural !Natural
  | LengthSMTLibProtocolBarrierNonceError
      !LengthSMTLibProtocolBarrier !SMTLibEchoSentinelError
  | LengthSMTLibProtocolMissingInputValueBarrierNonce
  | LengthSMTLibProtocolUnexpectedInputValueBarrierNonce
  | LengthSMTLibProtocolRepeatedBarrierNonce
  | LengthSMTLibProtocolPlanFingerprintByteLimitExceeded !Natural !Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibProtocolPlanError

-- | Opaque association of exact policy, query, framing policy, barriers, and
-- writes.  The private fingerprint is a reversible complete key, not a digest.
data LengthSMTLibProtocolPlan identity local = LengthSMTLibProtocolPlan
  !LengthSMTLibExecutionConfig
  !(LengthSMTLibQuery identity local)
  !SMTLibStreamLimits
  !Natural
  !SMTLibEchoSentinel
  !(Maybe SMTLibEchoSentinel)
  [Word8]
  !(Maybe [Word8])
  !(Fingerprint LengthSMTLibProtocolPlanFingerprintSubject)

type role LengthSMTLibProtocolPlan nominal nominal

instance NFData (LengthSMTLibProtocolPlan identity local) where
  rnf (LengthSMTLibProtocolPlan execution query stream cumulative
      checkBarrier valueBarrier initialWrite valueWrite fingerprint) =
    rnf execution `seq` rnf query `seq` rnf stream `seq` rnf cumulative `seq`
    rnf checkBarrier `seq` rnf valueBarrier `seq` rnf initialWrite `seq`
    rnf valueWrite `seq` rnf fingerprint

data LengthSMTLibProtocolPlanFingerprintSubject

-- | Seal a pure transaction from caller-provided barrier nonce bytes.  The
-- optional value nonce is required exactly when the artifact policy requests
-- values and the query has at least one input symbol.
sealLengthSMTLibProtocolPlan
  :: LengthSMTLibProtocolLimits
  -> LengthSMTLibExecutionConfig
  -> LengthSMTLibQuery identity local
  -> [Word8]
  -> Maybe [Word8]
  -> Either
      LengthSMTLibProtocolPlanError
      (LengthSMTLibProtocolPlan identity local)
sealLengthSMTLibProtocolPlan limits execution query
    rawCheckNonce rawValueNonce = do
  validatePlanFraming limits execution query requiresValues
  checkBarrier <- first
    (LengthSMTLibProtocolBarrierNonceError
      LengthSMTLibProtocolCheckBarrier)
    $ mkSMTLibEchoSentinel rawCheckNonce
  valueBarrier <- case (requiresValues, rawValueNonce) of
    (False, Nothing) -> Right Nothing
    (False, Just _) ->
      Left LengthSMTLibProtocolUnexpectedInputValueBarrierNonce
    (True, Nothing) ->
      Left LengthSMTLibProtocolMissingInputValueBarrierNonce
    (True, Just nonce) -> Just <$> first
      (LengthSMTLibProtocolBarrierNonceError
        LengthSMTLibProtocolInputValueBarrier)
      (mkSMTLibEchoSentinel nonce)
  case valueBarrier of
    Just barrier
      | barrier == checkBarrier ->
          Left LengthSMTLibProtocolRepeatedBarrierNonce
    _ -> Right ()
  let initialWrite =
        lengthSMTLibExecutionQueryResetBytes ++
        lengthSMTLibQueryCheckBytes query ++
        smtLibEchoSentinelCommandBytes checkBarrier
      valueWrite = case
          (lengthSMTLibQueryInputValueRequestBytes query, valueBarrier) of
        (Just request, Just barrier) -> Just $
          request ++ smtLibEchoSentinelCommandBytes barrier
        _ -> Nothing
  fingerprint <- buildPlanFingerprint limits execution query
    checkBarrier valueBarrier initialWrite valueWrite
  pure $ LengthSMTLibProtocolPlan execution query
    (lengthSMTLibProtocolStreamLimits limits)
    (lengthSMTLibProtocolCumulativeStdoutByteLimit limits)
    checkBarrier valueBarrier initialWrite valueWrite fingerprint
 where
  requiresValues =
    lengthSMTLibExecutionArtifactPolicy execution ==
      LengthSMTLibInputValuesAfterSatisfiable &&
    isJust (lengthSMTLibQueryInputValueRequestBytes query)

lengthSMTLibProtocolInitialWriteBytes
  :: LengthSMTLibProtocolPlan identity local
  -> [Word8]
lengthSMTLibProtocolInitialWriteBytes
    (LengthSMTLibProtocolPlan _ _ _ _ _ _ value _ _) = value

lengthSMTLibProtocolInputValueWriteBytes
  :: LengthSMTLibProtocolPlan identity local
  -> Maybe [Word8]
lengthSMTLibProtocolInputValueWriteBytes
    (LengthSMTLibProtocolPlan _ _ _ _ _ _ _ value _) = value

lengthSMTLibProtocolPlanFingerprint
  :: LengthSMTLibProtocolPlan identity local
  -> Fingerprint LengthSMTLibProtocolPlanFingerprintSubject
lengthSMTLibProtocolPlanFingerprint
    (LengthSMTLibProtocolPlan _ _ _ _ _ _ _ _ value) = value

-- | Smallest complete live transcript admitted by this exact plan, including
-- the required lexical delimiter after each bare status and final echo.  A
-- session uses this before reserving an ordinal so the remaining process-wide
-- stdout budget can admit at least one complete branch.
lengthSMTLibProtocolPlanMinimumStdoutByteCount
  :: LengthSMTLibProtocolPlan identity local
  -> Natural
lengthSMTLibProtocolPlanMinimumStdoutByteCount plan =
  minimumProtocolStdoutBytes (planQuery plan)
    $ case planValueBarrier plan of
        Nothing -> False
        Just _ -> True

data LengthSMTLibProtocolPhase
  = LengthSMTLibProtocolCheckStatusPhase
  | LengthSMTLibProtocolCheckBarrierPhase
  | LengthSMTLibProtocolInputValuePhase
  | LengthSMTLibProtocolInputValueBarrierPhase
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibProtocolPhase

data LengthSMTLibProtocolWriteKind
  = LengthSMTLibProtocolInitialQueryWrite
  | LengthSMTLibProtocolInputValueWrite
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibProtocolWriteKind

data LengthSMTLibProtocolPhaseState
  = AwaitLengthSMTLibCheckStatus
  | AwaitLengthSMTLibCheckBarrier !SolverStatus [Word8]
  | AwaitLengthSMTLibInputValues !SolverStatus [Word8]
  | AwaitLengthSMTLibInputValueBarrier
      !SolverStatus [Word8] [Word8] [LengthSMTLibIntegerBinding]

instance NFData LengthSMTLibProtocolPhaseState where
  rnf state = case state of
    AwaitLengthSMTLibCheckStatus -> ()
    AwaitLengthSMTLibCheckBarrier status rawStatus ->
      rnf status `seq` rnf rawStatus
    AwaitLengthSMTLibInputValues status rawStatus ->
      rnf status `seq` rnf rawStatus
    AwaitLengthSMTLibInputValueBarrier
        status rawStatus rawValues bindings ->
      rnf status `seq` rnf rawStatus `seq` rnf rawValues `seq` rnf bindings

-- | A continuation for bytes received only after the write which produced it
-- has completed.  Constructors and raw framing state never leave the package.
data LengthSMTLibProtocolReceiver identity local =
  LengthSMTLibProtocolReceiver
    !(LengthSMTLibProtocolPlan identity local)
    !LengthSMTLibProtocolPhaseState
    !Natural
    !Bool
    !SMTLibStreamFramer

type role LengthSMTLibProtocolReceiver nominal nominal

instance NFData (LengthSMTLibProtocolReceiver identity local) where
  rnf (LengthSMTLibProtocolReceiver plan phase frameStart capped framer) =
    rnf plan `seq` rnf phase `seq` rnf frameStart `seq`
    rnf capped `seq` rnf framer

lengthSMTLibProtocolReceiverPhase
  :: LengthSMTLibProtocolReceiver identity local
  -> LengthSMTLibProtocolPhase
lengthSMTLibProtocolReceiverPhase
    (LengthSMTLibProtocolReceiver _ phase _ _ _) = phaseName phase

-- | A write action carries a causal obligation: its receiver must not be fed
-- until the exact bytes have been written completely.  'Await' means the previous
-- write remains outstanding for responses; 'Complete' is only a syntactic,
-- caller-feedable decode and does not attest execution.
data LengthSMTLibProtocolAction identity local
  = LengthSMTLibProtocolWrite
      !LengthSMTLibProtocolWriteKind
      [Word8]
      !(LengthSMTLibProtocolReceiver identity local)
  | LengthSMTLibProtocolAwait
      !(LengthSMTLibProtocolReceiver identity local)
  | LengthSMTLibProtocolComplete
      !(LengthSMTLibProtocolDecoded identity local)

type role LengthSMTLibProtocolAction nominal nominal

instance NFData (LengthSMTLibProtocolAction identity local) where
  rnf action = case action of
    LengthSMTLibProtocolWrite kind bytes receiver ->
      rnf kind `seq` rnf bytes `seq` rnf receiver
    LengthSMTLibProtocolAwait receiver -> rnf receiver
    LengthSMTLibProtocolComplete decoded -> rnf decoded

-- | Start with reset, canonical check commands, and the status barrier in one
-- exact write.  Any unexpected reset response consequently occupies the
-- status slot and fails closed.
startLengthSMTLibProtocol
  :: LengthSMTLibProtocolPlan identity local
  -> LengthSMTLibProtocolAction identity local
startLengthSMTLibProtocol plan = LengthSMTLibProtocolWrite
  LengthSMTLibProtocolInitialQueryWrite
  (lengthSMTLibProtocolInitialWriteBytes plan)
  (newReceiver plan AwaitLengthSMTLibCheckStatus 0)

feedLengthSMTLibProtocol
  :: LengthSMTLibProtocolReceiver identity local
  -> [Word8]
  -> Either
      LengthSMTLibProtocolError
      (LengthSMTLibProtocolAction identity local)
feedLengthSMTLibProtocol receiver bytes = case
    feedSMTLibStreamFramer (receiverFramer receiver) bytes of
  Left failure -> Left $ classifyFramingFailure receiver failure
  Right (SMTLibStreamFramingPending next) ->
    Right $ LengthSMTLibProtocolAwait $ replaceReceiverFramer receiver next
  Right (SMTLibStreamFramingComplete frame tailBytes consumed) ->
    handleFrame receiver
      (receiverFrameStart receiver + consumed) frame tailBytes

-- | EOF is never a successful terminal delimiter for a reusable worker.
-- Lexical EOF failures retain precedence; otherwise every phase reports its
-- exact missing response position.
finishLengthSMTLibProtocol
  :: LengthSMTLibProtocolReceiver identity local
  -> Either
      LengthSMTLibProtocolError
      (LengthSMTLibProtocolAction identity local)
finishLengthSMTLibProtocol receiver = case
    finishSMTLibStreamFramer $ receiverFramer receiver of
  Left failure -> Left $ classifyFramingFailure receiver failure
  Right _ -> Left $ LengthSMTLibProtocolUnexpectedEOF
    $ lengthSMTLibProtocolReceiverPhase receiver

data LengthSMTLibProtocolError
  = LengthSMTLibProtocolFramingFailure
      !LengthSMTLibProtocolPhase !SMTLibStreamFramingError
  | LengthSMTLibProtocolResponseFailure
      !LengthSMTLibProtocolPhase !LengthSMTLibResponseError
  | LengthSMTLibProtocolBarrierMismatch !LengthSMTLibProtocolBarrier
  | LengthSMTLibProtocolCumulativeStdoutByteLimitExceeded !Natural !Natural
  | LengthSMTLibProtocolUnexpectedPostBarrierByte !Natural !Word8
  | LengthSMTLibProtocolUnexpectedEOF !LengthSMTLibProtocolPhase
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibProtocolError

data LengthSMTLibProtocolInputValues = LengthSMTLibProtocolInputValues
  !(Maybe [Word8]) [LengthSMTLibIntegerBinding]

instance NFData LengthSMTLibProtocolInputValues where
  rnf (LengthSMTLibProtocolInputValues frame bindings) =
    rnf frame `seq` rnf bindings

-- | Pure, syntactically decoded transcript outcome.  A satisfiable zero-input
-- query under the input-value policy carries a vacuous @Just []@ result without
-- fabricating a frame or emitting an empty @get-value@ command.  This type is
-- intentionally not named or represented as an execution observation.
data LengthSMTLibProtocolDecoded identity local = LengthSMTLibProtocolDecoded
  !(LengthSMTLibProtocolPlan identity local)
  !SolverStatus
  [Word8]
  !(Maybe LengthSMTLibProtocolInputValues)

type role LengthSMTLibProtocolDecoded nominal nominal

instance NFData (LengthSMTLibProtocolDecoded identity local) where
  rnf (LengthSMTLibProtocolDecoded plan status rawStatus values) =
    rnf plan `seq` rnf status `seq` rnf rawStatus `seq` rnf values

lengthSMTLibProtocolDecodedStatus
  :: LengthSMTLibProtocolDecoded identity local
  -> SolverStatus
lengthSMTLibProtocolDecodedStatus
    (LengthSMTLibProtocolDecoded _ value _ _) = value

lengthSMTLibProtocolDecodedStatusFrame
  :: LengthSMTLibProtocolDecoded identity local
  -> [Word8]
lengthSMTLibProtocolDecodedStatusFrame
    (LengthSMTLibProtocolDecoded _ _ value _) = value

lengthSMTLibProtocolDecodedInputValueFrame
  :: LengthSMTLibProtocolDecoded identity local
  -> Maybe [Word8]
lengthSMTLibProtocolDecodedInputValueFrame
    (LengthSMTLibProtocolDecoded _ _ _ values) = case values of
      Nothing -> Nothing
      Just (LengthSMTLibProtocolInputValues frame _) -> frame

lengthSMTLibProtocolDecodedInputValues
  :: LengthSMTLibProtocolDecoded identity local
  -> Maybe [LengthSMTLibIntegerBinding]
lengthSMTLibProtocolDecodedInputValues
    (LengthSMTLibProtocolDecoded _ _ _ values) = case values of
      Nothing -> Nothing
      Just (LengthSMTLibProtocolInputValues _ bindings) -> Just bindings

lengthSMTLibProtocolDecodedPlanFingerprint
  :: LengthSMTLibProtocolDecoded identity local
  -> Fingerprint LengthSMTLibProtocolPlanFingerprintSubject
lengthSMTLibProtocolDecodedPlanFingerprint
    (LengthSMTLibProtocolDecoded plan _ _ _) =
  lengthSMTLibProtocolPlanFingerprint plan

handleFrame
  :: LengthSMTLibProtocolReceiver identity local
  -> Natural
  -> [Word8]
  -> [Word8]
  -> Either
      LengthSMTLibProtocolError
      (LengthSMTLibProtocolAction identity local)
handleFrame receiver consumed frame tailBytes = case receiverPhase receiver of
  AwaitLengthSMTLibCheckStatus -> do
    let plan = receiverPlan receiver
        limits = lengthSMTLibExecutionResponseLimits $ planExecution plan
    status <- first
      (LengthSMTLibProtocolResponseFailure
        LengthSMTLibProtocolCheckStatusPhase)
      $ parseLengthSMTLibCheckResponse limits frame
    continueWithinWrite plan
      (AwaitLengthSMTLibCheckBarrier status frame) consumed tailBytes
  AwaitLengthSMTLibCheckBarrier status rawStatus -> do
    let plan = receiverPlan receiver
    if isExactSMTLibEchoSentinelResponse (planCheckBarrier plan) frame
      then case
          ( status
          , planValueBarrier plan
          , lengthSMTLibProtocolInputValueWriteBytes plan
          ) of
        (SolverSatisfiable, Just _, Just valueWrite) -> do
          afterBoundary <- consumePostBarrierWhitespace
            plan consumed tailBytes
          Right $ LengthSMTLibProtocolWrite
            LengthSMTLibProtocolInputValueWrite valueWrite
            $ newReceiver plan
                (AwaitLengthSMTLibInputValues status rawStatus)
                afterBoundary
        _ -> do
          _ <- consumePostBarrierWhitespace plan consumed tailBytes
          Right $ LengthSMTLibProtocolComplete
            $ LengthSMTLibProtocolDecoded plan status rawStatus
            $ terminalInputValues plan status
      else Left $ LengthSMTLibProtocolBarrierMismatch
        LengthSMTLibProtocolCheckBarrier
  AwaitLengthSMTLibInputValues status rawStatus -> do
    let plan = receiverPlan receiver
        limits = lengthSMTLibExecutionResponseLimits $ planExecution plan
    bindings <- first
      (LengthSMTLibProtocolResponseFailure
        LengthSMTLibProtocolInputValuePhase)
      $ parseLengthSMTLibInputValueResponse limits (planQuery plan) frame
    continueWithinWrite plan
      (AwaitLengthSMTLibInputValueBarrier
        status rawStatus frame bindings)
      consumed tailBytes
  AwaitLengthSMTLibInputValueBarrier
      status rawStatus rawValues bindings -> do
    let plan = receiverPlan receiver
    case planValueBarrier plan of
      Just barrier
        | isExactSMTLibEchoSentinelResponse barrier frame -> do
            _ <- consumePostBarrierWhitespace plan consumed tailBytes
            Right $ LengthSMTLibProtocolComplete
              $ LengthSMTLibProtocolDecoded plan status rawStatus
              $ Just $ LengthSMTLibProtocolInputValues
                  (Just rawValues) bindings
      _ -> Left $ LengthSMTLibProtocolBarrierMismatch
        LengthSMTLibProtocolInputValueBarrier

continueWithinWrite
  :: LengthSMTLibProtocolPlan identity local
  -> LengthSMTLibProtocolPhaseState
  -> Natural
  -> [Word8]
  -> Either
      LengthSMTLibProtocolError
      (LengthSMTLibProtocolAction identity local)
continueWithinWrite plan phase consumed tailBytes =
  feedLengthSMTLibProtocol (newReceiver plan phase consumed) tailBytes

terminalInputValues
  :: LengthSMTLibProtocolPlan identity local
  -> SolverStatus
  -> Maybe LengthSMTLibProtocolInputValues
terminalInputValues plan status
  | status == SolverSatisfiable
  , lengthSMTLibExecutionArtifactPolicy (planExecution plan) ==
      LengthSMTLibInputValuesAfterSatisfiable
  , not $ isJust $ lengthSMTLibQueryInputValueRequestBytes $ planQuery plan =
      Just $ LengthSMTLibProtocolInputValues Nothing []
  | otherwise = Nothing

consumePostBarrierWhitespace
  :: LengthSMTLibProtocolPlan identity local
  -> Natural
  -> [Word8]
  -> Either LengthSMTLibProtocolError Natural
consumePostBarrierWhitespace plan = go
 where
  maximumBytes = planCumulativeStdoutByteLimit plan

  go !consumed [] = Right consumed
  go !consumed (byte : bytes)
    | consumed >= maximumBytes = Left $
        LengthSMTLibProtocolCumulativeStdoutByteLimitExceeded
          maximumBytes (maximumBytes + 1)
    | isSMTLibWhitespace byte = go (consumed + 1) bytes
    | otherwise = Left $
        LengthSMTLibProtocolUnexpectedPostBarrierByte consumed byte

newReceiver
  :: LengthSMTLibProtocolPlan identity local
  -> LengthSMTLibProtocolPhaseState
  -> Natural
  -> LengthSMTLibProtocolReceiver identity local
newReceiver plan phase frameStart = LengthSMTLibProtocolReceiver
  plan phase frameStart cumulativeCapsFrame
  $ startSMTLibStreamFramer effectiveLimits
 where
  configured = planStreamLimits plan
  cumulativeMaximum = planCumulativeStdoutByteLimit plan
  remaining
    | frameStart >= cumulativeMaximum = 0
    | otherwise = cumulativeMaximum - frameStart
  configuredTotal = smtLibStreamTotalByteLimit configured
  -- The frame-total error wins an exact tie; cumulative failure is reported
  -- only when the transaction's remaining budget is the strictly smaller cap.
  cumulativeCapsFrame = remaining < configuredTotal
  effectiveLimits = mkSMTLibStreamLimits SMTLibStreamLimitSource
    { smtLibStreamLimitSourceTotalBytes = min configuredTotal remaining
    , smtLibStreamLimitSourceFrameBytes =
        smtLibStreamFrameByteLimit configured
    , smtLibStreamLimitSourceNestingDepth =
        smtLibStreamNestingDepthLimit configured
    }

classifyFramingFailure
  :: LengthSMTLibProtocolReceiver identity local
  -> SMTLibStreamFramingError
  -> LengthSMTLibProtocolError
classifyFramingFailure receiver failure = case failure of
  SMTLibStreamTotalByteLimitExceeded _ _
    | receiverCumulativeCapsFrame receiver ->
        LengthSMTLibProtocolCumulativeStdoutByteLimitExceeded
          cumulativeMaximum (cumulativeMaximum + 1)
  _ -> LengthSMTLibProtocolFramingFailure
    (lengthSMTLibProtocolReceiverPhase receiver) failure
 where
  cumulativeMaximum = planCumulativeStdoutByteLimit $ receiverPlan receiver

receiverPlan
  :: LengthSMTLibProtocolReceiver identity local
  -> LengthSMTLibProtocolPlan identity local
receiverPlan (LengthSMTLibProtocolReceiver value _ _ _ _) = value

receiverPhase
  :: LengthSMTLibProtocolReceiver identity local
  -> LengthSMTLibProtocolPhaseState
receiverPhase (LengthSMTLibProtocolReceiver _ value _ _ _) = value

receiverFrameStart
  :: LengthSMTLibProtocolReceiver identity local
  -> Natural
receiverFrameStart (LengthSMTLibProtocolReceiver _ _ value _ _) = value

receiverCumulativeCapsFrame
  :: LengthSMTLibProtocolReceiver identity local
  -> Bool
receiverCumulativeCapsFrame
    (LengthSMTLibProtocolReceiver _ _ _ value _) = value

receiverFramer
  :: LengthSMTLibProtocolReceiver identity local
  -> SMTLibStreamFramer
receiverFramer (LengthSMTLibProtocolReceiver _ _ _ _ value) = value

replaceReceiverFramer
  :: LengthSMTLibProtocolReceiver identity local
  -> SMTLibStreamFramer
  -> LengthSMTLibProtocolReceiver identity local
replaceReceiverFramer
    (LengthSMTLibProtocolReceiver plan phase start capped _) framer =
  LengthSMTLibProtocolReceiver plan phase start capped framer

phaseName :: LengthSMTLibProtocolPhaseState -> LengthSMTLibProtocolPhase
phaseName phase = case phase of
  AwaitLengthSMTLibCheckStatus -> LengthSMTLibProtocolCheckStatusPhase
  AwaitLengthSMTLibCheckBarrier{} ->
    LengthSMTLibProtocolCheckBarrierPhase
  AwaitLengthSMTLibInputValues{} ->
    LengthSMTLibProtocolInputValuePhase
  AwaitLengthSMTLibInputValueBarrier{} ->
    LengthSMTLibProtocolInputValueBarrierPhase

planExecution
  :: LengthSMTLibProtocolPlan identity local
  -> LengthSMTLibExecutionConfig
planExecution (LengthSMTLibProtocolPlan value _ _ _ _ _ _ _ _) = value

planQuery
  :: LengthSMTLibProtocolPlan identity local
  -> LengthSMTLibQuery identity local
planQuery (LengthSMTLibProtocolPlan _ value _ _ _ _ _ _ _) = value

planStreamLimits
  :: LengthSMTLibProtocolPlan identity local
  -> SMTLibStreamLimits
planStreamLimits (LengthSMTLibProtocolPlan _ _ value _ _ _ _ _ _) = value

planCumulativeStdoutByteLimit
  :: LengthSMTLibProtocolPlan identity local
  -> Natural
planCumulativeStdoutByteLimit
    (LengthSMTLibProtocolPlan _ _ _ value _ _ _ _ _) = value

planCheckBarrier
  :: LengthSMTLibProtocolPlan identity local
  -> SMTLibEchoSentinel
planCheckBarrier
    (LengthSMTLibProtocolPlan _ _ _ _ value _ _ _ _) = value

planValueBarrier
  :: LengthSMTLibProtocolPlan identity local
  -> Maybe SMTLibEchoSentinel
planValueBarrier
    (LengthSMTLibProtocolPlan _ _ _ _ _ value _ _ _) = value

validatePlanFraming
  :: LengthSMTLibProtocolLimits
  -> LengthSMTLibExecutionConfig
  -> LengthSMTLibQuery identity local
  -> Bool
  -> Either LengthSMTLibProtocolPlanError ()
validatePlanFraming limits execution query requiresValues = do
  validateStreamFrame LengthSMTLibProtocolCheckStatusFrame
    checkStatusFrameByteCount 0
  validateResponseFrame LengthSMTLibProtocolCheckStatusFrame
    checkStatusFrameByteCount 0 1 checkStatusFrameByteCount
  validateStreamFrame LengthSMTLibProtocolCheckBarrierFrame
    fixedSentinelResponseByteCount 0
  if requiresValues
    then do
      validateStreamFrame LengthSMTLibProtocolInputValueFrame valueBytes 2
      validateResponseFrame LengthSMTLibProtocolInputValueFrame
        valueBytes 2 valueNodes valueTokenBytes
      validateStreamFrame LengthSMTLibProtocolInputValueBarrierFrame
        fixedSentinelResponseByteCount 0
    else Right ()
  let minimumBytes = minimumProtocolStdoutBytes query requiresValues
      maximumBytes = lengthSMTLibProtocolCumulativeStdoutByteLimit limits
  if maximumBytes < minimumBytes
    then Left $ LengthSMTLibProtocolMinimumStdoutByteLimitExceeded
      maximumBytes minimumBytes
    else Right ()
 where
  stream = lengthSMTLibProtocolStreamLimits limits
  responses = lengthSMTLibExecutionResponseLimits execution
  valueBytes = minimalInputValueFrameByteCount query
  valueNodes = 1 + 3 * genericLength
    (lengthSMTLibQueryInputSymbols query)
  valueTokenBytes = maximum $ 1 : map genericLength
    (lengthSMTLibQueryInputSymbols query)

  validateStreamFrame site required depth = do
    validateRequiredLimit site LengthSMTLibProtocolStreamTotalBytes
      (smtLibStreamTotalByteLimit stream) required
    validateRequiredLimit site LengthSMTLibProtocolStreamFrameBytes
      (smtLibStreamFrameByteLimit stream) required
    validateRequiredLimit site LengthSMTLibProtocolStreamNestingDepth
      (smtLibStreamNestingDepthLimit stream) depth

  validateResponseFrame site bytes depth nodes tokenBytes = do
    validateRequiredLimit site LengthSMTLibProtocolResponseBytes
      (lengthSMTLibResponseByteLimit responses) bytes
    validateRequiredLimit site LengthSMTLibProtocolResponseNestingDepth
      (fromIntegral $ lengthSMTLibResponseNestingDepthLimit responses) depth
    validateRequiredLimit site LengthSMTLibProtocolResponseNodes
      (lengthSMTLibResponseNodeLimit responses) nodes
    validateRequiredLimit site LengthSMTLibProtocolResponseTokenBytes
      (lengthSMTLibResponseTokenByteLimit responses) tokenBytes

validateRequiredLimit
  :: LengthSMTLibProtocolRequiredFrame
  -> LengthSMTLibProtocolRequiredLimit
  -> Natural
  -> Natural
  -> Either LengthSMTLibProtocolPlanError ()
validateRequiredLimit site field maximumValue required
  | maximumValue < required = Left $
      LengthSMTLibProtocolRequiredLimitTooSmall
        site field maximumValue required
  | otherwise = Right ()

checkStatusFrameByteCount :: Natural
checkStatusFrameByteCount = 7

fixedSentinelResponseByteCount :: Natural
fixedSentinelResponseByteCount =
  2 + genericLength (ascii "djex-smtlib-frame/v1/") + 64

minimumProtocolStdoutBytes
  :: LengthSMTLibQuery identity local
  -> Bool
  -> Natural
minimumProtocolStdoutBytes query requiresValues =
  max unknownPath valuePath
 where
  marker = fixedSentinelResponseByteCount
  -- Every bare status and quoted marker needs one lexical separator.
  unknownPath = 7 + 1 + marker + 1
  valuePath
    | requiresValues =
        3 + 1 + marker + 1 +
        minimalInputValueFrameByteCount query + marker + 1
    | otherwise = 3 + 1 + marker + 1

minimalInputValueFrameByteCount
  :: LengthSMTLibQuery identity local
  -> Natural
minimalInputValueFrameByteCount query = genericLength $
  [openParen] ++ concatMap binding
    (lengthSMTLibQueryInputSymbols query) ++ [closeParen]
 where
  binding symbol =
    [openParen] ++ symbol ++ [space, digitZero, closeParen]

buildPlanFingerprint
  :: LengthSMTLibProtocolLimits
  -> LengthSMTLibExecutionConfig
  -> LengthSMTLibQuery identity local
  -> SMTLibEchoSentinel
  -> Maybe SMTLibEchoSentinel
  -> [Word8]
  -> Maybe [Word8]
  -> Either
      LengthSMTLibProtocolPlanError
      (Fingerprint LengthSMTLibProtocolPlanFingerprintSubject)
buildPlanFingerprint limits execution query checkBarrier valueBarrier
    initialWrite valueWrite = case
  buildFingerprintWithin maximumBytes FingerprintBuilder
    { fingerprintBuilderVersion = 1
    , fingerprintBuilderRole = ascii
        "finite-list-spine-length/z3-smtlib2-protocol-plan"
    , fingerprintBuilderFields =
        [ tagged "schema"
            [FingerprintBytes lengthSMTLibProtocolPlanSchemaTag]
        , tagged "execution-policy"
            [ FingerprintBytes $ fingerprintCanonicalBytes
                $ lengthSMTLibExecutionPolicyFingerprint execution
            ]
        , tagged "query"
            [ FingerprintBytes $ PublicFingerprint.fingerprintCanonicalBytes
                $ lengthSMTLibQueryFingerprint query
            ]
        , tagged "stream-framing"
            [ FingerprintBytes smtLibStreamFramingSchemaTag
            , FingerprintNatural $ smtLibStreamTotalByteLimit stream
            , FingerprintNatural $ smtLibStreamFrameByteLimit stream
            , FingerprintNatural $ smtLibStreamNestingDepthLimit stream
            ]
        , tagged "cumulative-stdout"
            [FingerprintNatural cumulative]
        , tagged "post-barrier"
            [ FingerprintBytes lengthSMTLibProtocolPostBarrierSchemaTag
            , FingerprintBytes smtLibWhitespaceBytes
            ]
        , tagged "phase-machine"
            [ FingerprintBytes lengthSMTLibProtocolPhaseMachineSchemaTag
            , FingerprintSequence $ map (FingerprintBytes . ascii)
                [ "write-reset-check-status-barrier"
                , "decode-check-status"
                , "match-check-barrier"
                , "conditionally-write-input-value-barrier"
                , "decode-input-values"
                , "match-input-value-barrier"
                ]
            ]
        , tagged "initial-write"
            [ FingerprintBytes lengthSMTLibExecutionQueryResetBytes
            , FingerprintBytes $ lengthSMTLibQueryCheckBytes query
            , FingerprintBytes $ smtLibEchoSentinelCommandBytes checkBarrier
            , FingerprintBytes initialWrite
            ]
        , tagged "check-response"
            [ FingerprintBytes lengthSMTLibResponseSchemaTag
            , FingerprintBytes $ smtLibEchoSentinelResponseBytes checkBarrier
            ]
        , tagged "input-values" [case
              ( lengthSMTLibQueryInputValueRequestBytes query
              , valueBarrier
              , valueWrite
              ) of
            (Just request, Just barrier, Just write) -> tagged "present"
              [ FingerprintBytes request
              , FingerprintBytes $ smtLibEchoSentinelCommandBytes barrier
              , FingerprintBytes write
              , FingerprintBytes lengthSMTLibResponseSchemaTag
              , FingerprintBytes $ smtLibEchoSentinelResponseBytes barrier
              ]
            _
              | lengthSMTLibExecutionArtifactPolicy execution ==
                  LengthSMTLibInputValuesAfterSatisfiable ->
                  tagged "vacuous-zero-input" []
              | otherwise -> tagged "absent" []]
        ]
    } of
  Left FingerprintLimitExceeded
      { fingerprintMaximumBytes = admitted
      , fingerprintObservedBytesAtLeast = observed
      } -> Left $ LengthSMTLibProtocolPlanFingerprintByteLimitExceeded
        admitted observed
  Right fingerprint -> Right fingerprint
 where
  maximumBytes = lengthSMTLibProtocolPlanFingerprintByteLimit limits
  stream = lengthSMTLibProtocolStreamLimits limits
  cumulative = lengthSMTLibProtocolCumulativeStdoutByteLimit limits

tagged :: String -> [FingerprintField] -> FingerprintField
tagged name = FingerprintTag $ ascii name

isSMTLibWhitespace :: Word8 -> Bool
isSMTLibWhitespace byte = byte == space || byte == horizontalTab ||
  byte == lineFeed || byte == carriageReturn

smtLibWhitespaceBytes :: [Word8]
smtLibWhitespaceBytes = [horizontalTab, lineFeed, carriageReturn, space]

ascii :: String -> [Word8]
ascii = map $ fromIntegral . fromEnum

horizontalTab, lineFeed, carriageReturn, space, openParen, closeParen,
  digitZero :: Word8
horizontalTab = 9
lineFeed = 10
carriageReturn = 13
space = 32
openParen = 40
closeParen = 41
digitZero = 48
