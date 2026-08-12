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
  , LengthSMTLibProtocolAction
  , startLengthSMTLibProtocol
  , feedLengthSMTLibProtocol
  , finishLengthSMTLibProtocol
  , LengthSMTLibProtocolError (..)
  , LengthSMTLibProtocolDecoded
  , lengthSMTLibProtocolDecodedStatus
  , lengthSMTLibProtocolDecodedInputValues
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
import Language.Haskell.Synthesis.Internal.SMTLib.Causal
  ( SMTLibCausalAction (..) )
import Language.Haskell.Synthesis.Internal.SMTLib.Causal.Stream
  ( SMTLibCausalStreamBoundary
  , SMTLibCausalStreamCompletedFrame
  , SMTLibCausalStreamCursor
  , SMTLibCausalStreamFailure (..)
  , SMTLibCausalStreamPolicy
  , SMTLibCausalStreamStep (..)
  , continueSMTLibCausalStreamCompletedFrame
  , consumeSMTLibCausalStreamBoundaryWhitespace
  , feedSMTLibCausalStreamCursor
  , finishSMTLibCausalStreamCursor
  , mkSMTLibCausalStreamPolicy
  , smtLibCausalStreamCompletedFrameBytes
  , smtLibCausalStreamPolicyCumulativeByteLimit
  , smtLibCausalStreamPolicyStreamLimits
  , startSMTLibCausalStreamCursor
  , startSMTLibCausalStreamCursorAtBoundary
  )
import Language.Haskell.Synthesis.Internal.SMTLib.Stream
  ( SMTLibEchoSentinel
  , SMTLibEchoSentinelError
  , SMTLibStreamFramingError (..)
  , SMTLibStreamLimitSource (..)
  , SMTLibStreamLimits
  , defaultSMTLibStreamLimitSource
  , isExactSMTLibEchoSentinelResponse
  , mkSMTLibEchoSentinel
  , mkSMTLibStreamLimits
  , smtLibEchoSentinelCommandBytes
  , smtLibEchoSentinelResponseBytes
  , smtLibStreamFrameByteLimit
  , smtLibStreamFramingSchemaTag
  , smtLibStreamNestingDepthLimit
  , smtLibStreamTotalByteLimit
  , smtLibWhitespaceBytes
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
  !SMTLibCausalStreamPolicy !Natural
  deriving (Eq, Ord)

instance NFData LengthSMTLibProtocolLimits where
  rnf (LengthSMTLibProtocolLimits streamPolicy fingerprint) =
    rnf streamPolicy `seq` rnf fingerprint

mkLengthSMTLibProtocolLimits
  :: LengthSMTLibProtocolLimitSource
  -> LengthSMTLibProtocolLimits
mkLengthSMTLibProtocolLimits source = LengthSMTLibProtocolLimits
  (mkSMTLibCausalStreamPolicy
    (mkSMTLibStreamLimits
      $ lengthSMTLibProtocolLimitSourceStreamLimits source)
    (lengthSMTLibProtocolLimitSourceCumulativeStdoutBytes source))
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
    (LengthSMTLibProtocolLimits streamPolicy _) =
      smtLibCausalStreamPolicyStreamLimits streamPolicy

lengthSMTLibProtocolCumulativeStdoutByteLimit
  :: LengthSMTLibProtocolLimits
  -> Natural
lengthSMTLibProtocolCumulativeStdoutByteLimit
    (LengthSMTLibProtocolLimits streamPolicy _) =
      smtLibCausalStreamPolicyCumulativeByteLimit streamPolicy

lengthSMTLibProtocolPlanFingerprintByteLimit
  :: LengthSMTLibProtocolLimits
  -> Natural
lengthSMTLibProtocolPlanFingerprintByteLimit
    (LengthSMTLibProtocolLimits _ value) = value

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
-- the complete identity of their deterministic writes.  The writes are
-- rendered transiently for fingerprint admission and later derived on demand
-- through the selectors used at their causal action edges; the private
-- fingerprint remains a reversible complete key, not a digest.
data LengthSMTLibProtocolPlan identity local = LengthSMTLibProtocolPlan
  !LengthSMTLibExecutionConfig
  !(LengthSMTLibQuery identity local)
  !SMTLibCausalStreamPolicy
  !SMTLibEchoSentinel
  !(Maybe SMTLibEchoSentinel)
  !(Fingerprint LengthSMTLibProtocolPlanFingerprintSubject)

type role LengthSMTLibProtocolPlan nominal nominal

instance NFData (LengthSMTLibProtocolPlan identity local) where
  rnf (LengthSMTLibProtocolPlan execution query streamPolicy
      checkBarrier valueBarrier fingerprint) =
    rnf execution `seq` rnf query `seq` rnf streamPolicy `seq`
    rnf checkBarrier `seq` rnf valueBarrier `seq` rnf fingerprint

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
  let symbols = lengthSMTLibQueryInputSymbols query
      valueRequest = lengthSMTLibQueryInputValueRequestBytes query
      requiresValues =
        lengthSMTLibExecutionArtifactPolicy execution ==
          LengthSMTLibInputValuesAfterSatisfiable &&
        isJust valueRequest
  validatePlanFraming limits execution symbols requiresValues
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
  let initialWrite = renderProtocolInitialWrite query checkBarrier
      valueWrite = renderProtocolInputValueWrite valueRequest valueBarrier
  fingerprint <- buildPlanFingerprint limits execution query valueRequest
    checkBarrier valueBarrier initialWrite valueWrite
  pure $ LengthSMTLibProtocolPlan execution query
    (limitsStreamPolicy limits)
    checkBarrier valueBarrier fingerprint

renderProtocolInitialWrite
  :: LengthSMTLibQuery identity local
  -> SMTLibEchoSentinel
  -> [Word8]
renderProtocolInitialWrite query barrier =
  lengthSMTLibExecutionQueryResetBytes ++
  lengthSMTLibQueryCheckBytes query ++
  smtLibEchoSentinelCommandBytes barrier

renderProtocolInputValueWrite
  :: Maybe [Word8]
  -> Maybe SMTLibEchoSentinel
  -> Maybe [Word8]
renderProtocolInputValueWrite _ Nothing = Nothing
renderProtocolInputValueWrite request (Just barrier) =
  fmap (++ smtLibEchoSentinelCommandBytes barrier)
    request

lengthSMTLibProtocolInitialWriteBytes
  :: LengthSMTLibProtocolPlan identity local
  -> [Word8]
lengthSMTLibProtocolInitialWriteBytes
    (LengthSMTLibProtocolPlan _ query _ checkBarrier _ _) =
      renderProtocolInitialWrite query checkBarrier

lengthSMTLibProtocolInputValueWriteBytes
  :: LengthSMTLibProtocolPlan identity local
  -> Maybe [Word8]
lengthSMTLibProtocolInputValueWriteBytes
    (LengthSMTLibProtocolPlan _ query _ _ valueBarrier _) =
      renderProtocolInputValueWrite
        (lengthSMTLibQueryInputValueRequestBytes query) valueBarrier

lengthSMTLibProtocolPlanFingerprint
  :: LengthSMTLibProtocolPlan identity local
  -> Fingerprint LengthSMTLibProtocolPlanFingerprintSubject
lengthSMTLibProtocolPlanFingerprint
    (LengthSMTLibProtocolPlan _ _ _ _ _ value) = value

-- | Smallest complete live transcript admitted by this exact plan, including
-- the required lexical delimiter after each bare status and final echo.  A
-- session uses this before reserving an ordinal so the remaining process-wide
-- stdout budget can admit at least one complete branch.
lengthSMTLibProtocolPlanMinimumStdoutByteCount
  :: LengthSMTLibProtocolPlan identity local
  -> Natural
lengthSMTLibProtocolPlanMinimumStdoutByteCount plan =
  minimumProtocolStdoutBytes
    (minimalInputValueFrameByteCount
      $ lengthSMTLibQueryInputSymbols $ planQuery plan)
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
  | AwaitLengthSMTLibCheckBarrier !SolverStatus
  | AwaitLengthSMTLibInputValues !SolverStatus
  | AwaitLengthSMTLibInputValueBarrier
      !SolverStatus [LengthSMTLibIntegerBinding]

instance NFData LengthSMTLibProtocolPhaseState where
  rnf state = case state of
    AwaitLengthSMTLibCheckStatus -> ()
    AwaitLengthSMTLibCheckBarrier status -> rnf status
    AwaitLengthSMTLibInputValues status -> rnf status
    AwaitLengthSMTLibInputValueBarrier status bindings ->
      rnf status `seq` rnf bindings

-- | A continuation for bytes received only after the write which produced it
-- has completed.  Constructors and raw framing state never leave the package.
data LengthSMTLibProtocolReceiver identity local =
  LengthSMTLibProtocolReceiver
    !(LengthSMTLibProtocolPlan identity local)
    !LengthSMTLibProtocolPhaseState
    !SMTLibCausalStreamCursor

type role LengthSMTLibProtocolReceiver nominal nominal

instance NFData (LengthSMTLibProtocolReceiver identity local) where
  rnf (LengthSMTLibProtocolReceiver plan phase cursor) =
    rnf plan `seq` rnf phase `seq` rnf cursor

lengthSMTLibProtocolReceiverPhase
  :: LengthSMTLibProtocolReceiver identity local
  -> LengthSMTLibProtocolPhase
lengthSMTLibProtocolReceiverPhase
    (LengthSMTLibProtocolReceiver _ phase _) = phaseName phase

-- | The shared causal action specialized to this plan's nominal receiver and
-- decoded outcome.  The shared type keeps all three parameters nominal.
type LengthSMTLibProtocolAction identity local =
  SMTLibCausalAction
    LengthSMTLibProtocolWriteKind
    (LengthSMTLibProtocolReceiver identity local)
    (LengthSMTLibProtocolDecoded identity local)

-- | Start with reset, canonical check commands, and the status barrier in one
-- exact write.  Any unexpected reset response consequently occupies the
-- status slot and fails closed.
startLengthSMTLibProtocol
  :: LengthSMTLibProtocolPlan identity local
  -> LengthSMTLibProtocolAction identity local
startLengthSMTLibProtocol plan = SMTLibCausalWrite
  LengthSMTLibProtocolInitialQueryWrite
  (lengthSMTLibProtocolInitialWriteBytes plan)
  (startReceiver plan AwaitLengthSMTLibCheckStatus)

feedLengthSMTLibProtocol
  :: LengthSMTLibProtocolReceiver identity local
  -> [Word8]
  -> Either
      LengthSMTLibProtocolError
      (LengthSMTLibProtocolAction identity local)
feedLengthSMTLibProtocol receiver bytes = do
  step <- first
    (mapStreamFailure $ lengthSMTLibProtocolReceiverPhase receiver)
    $ feedSMTLibCausalStreamCursor (receiverCursor receiver) bytes
  acceptStreamStep (receiverPlan receiver) (receiverPhase receiver) step

-- | EOF is never a successful terminal delimiter for a reusable worker.
-- Lexical EOF failures retain precedence; otherwise every phase reports its
-- exact missing response position.
finishLengthSMTLibProtocol
  :: LengthSMTLibProtocolReceiver identity local
  -> Either
      LengthSMTLibProtocolError
      (LengthSMTLibProtocolAction identity local)
finishLengthSMTLibProtocol receiver = case
    finishSMTLibCausalStreamCursor $ receiverCursor receiver of
  Left failure -> Left $ mapStreamFailure
    (lengthSMTLibProtocolReceiverPhase receiver) failure
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

-- | Pure, syntactically decoded transcript outcome.  A satisfiable zero-input
-- query under the input-value policy carries a vacuous @Just []@ result
-- without fabricating a frame or emitting an empty @get-value@ command.  The
-- receiver owns the plan until completion, and the live Session carries that
-- same plan separately as the exact run-identity input; it is not copied into
-- this terminal branch.  In a live run raw status and input-value frames
-- likewise remain in the process-owning causal transcript.  This type is
-- intentionally not named or represented as an execution observation.
data LengthSMTLibProtocolDecoded identity local = LengthSMTLibProtocolDecoded
  !SolverStatus
  !(Maybe [LengthSMTLibIntegerBinding])

type role LengthSMTLibProtocolDecoded nominal nominal

instance NFData (LengthSMTLibProtocolDecoded identity local) where
  rnf (LengthSMTLibProtocolDecoded status values) =
    rnf status `seq` rnf values

lengthSMTLibProtocolDecodedStatus
  :: LengthSMTLibProtocolDecoded identity local
  -> SolverStatus
lengthSMTLibProtocolDecodedStatus
    (LengthSMTLibProtocolDecoded value _) = value

lengthSMTLibProtocolDecodedInputValues
  :: LengthSMTLibProtocolDecoded identity local
  -> Maybe [LengthSMTLibIntegerBinding]
lengthSMTLibProtocolDecodedInputValues
    (LengthSMTLibProtocolDecoded _ values) = values

handleFrame
  :: LengthSMTLibProtocolPlan identity local
  -> LengthSMTLibProtocolPhaseState
  -> SMTLibCausalStreamCompletedFrame
  -> Either
      LengthSMTLibProtocolError
      (LengthSMTLibProtocolAction identity local)
handleFrame plan phase completed = case phase of
  AwaitLengthSMTLibCheckStatus -> do
    let limits = lengthSMTLibExecutionResponseLimits $ planExecution plan
    status <- first
      (LengthSMTLibProtocolResponseFailure
        LengthSMTLibProtocolCheckStatusPhase)
      $ parseLengthSMTLibCheckResponse limits frame
    continueWithinWrite plan
      (AwaitLengthSMTLibCheckBarrier status) completed
  AwaitLengthSMTLibCheckBarrier status -> do
    if isExactSMTLibEchoSentinelResponse (planCheckBarrier plan) frame
      then case
          ( status
          , planValueBarrier plan
          , lengthSMTLibProtocolInputValueWriteBytes plan
          ) of
        (SolverSatisfiable, Just _, Just valueWrite) -> do
          boundary <- consumeBoundary
            LengthSMTLibProtocolCheckBarrierPhase completed
          Right $ SMTLibCausalWrite
            LengthSMTLibProtocolInputValueWrite valueWrite
            $ receiverAtBoundary plan
                (AwaitLengthSMTLibInputValues status)
                boundary
        _ -> do
          _ <- consumeBoundary
            LengthSMTLibProtocolCheckBarrierPhase completed
          Right $ SMTLibCausalComplete
            $ LengthSMTLibProtocolDecoded status
            $ terminalInputValues plan status
      else Left $ LengthSMTLibProtocolBarrierMismatch
        LengthSMTLibProtocolCheckBarrier
  AwaitLengthSMTLibInputValues status -> do
    let limits = lengthSMTLibExecutionResponseLimits $ planExecution plan
    bindings <- first
      (LengthSMTLibProtocolResponseFailure
        LengthSMTLibProtocolInputValuePhase)
      $ parseLengthSMTLibInputValueResponse limits (planQuery plan) frame
    continueWithinWrite plan
      (AwaitLengthSMTLibInputValueBarrier
        status bindings)
      completed
  AwaitLengthSMTLibInputValueBarrier
      status bindings -> do
    case planValueBarrier plan of
      Just barrier
        | isExactSMTLibEchoSentinelResponse barrier frame -> do
            _ <- consumeBoundary
              LengthSMTLibProtocolInputValueBarrierPhase completed
            Right $ SMTLibCausalComplete
              $ LengthSMTLibProtocolDecoded status $ Just bindings
      _ -> Left $ LengthSMTLibProtocolBarrierMismatch
        LengthSMTLibProtocolInputValueBarrier
 where
  frame = smtLibCausalStreamCompletedFrameBytes completed

continueWithinWrite
  :: LengthSMTLibProtocolPlan identity local
  -> LengthSMTLibProtocolPhaseState
  -> SMTLibCausalStreamCompletedFrame
  -> Either
      LengthSMTLibProtocolError
      (LengthSMTLibProtocolAction identity local)
continueWithinWrite plan phase completed = do
  step <- first (mapStreamFailure $ phaseName phase)
    $ continueSMTLibCausalStreamCompletedFrame completed
  acceptStreamStep plan phase step

terminalInputValues
  :: LengthSMTLibProtocolPlan identity local
  -> SolverStatus
  -> Maybe [LengthSMTLibIntegerBinding]
terminalInputValues plan status
  | status == SolverSatisfiable
  , lengthSMTLibExecutionArtifactPolicy (planExecution plan) ==
      LengthSMTLibInputValuesAfterSatisfiable
  , not $ isJust $ lengthSMTLibQueryInputValueRequestBytes $ planQuery plan =
      Just []
  | otherwise = Nothing

startReceiver
  :: LengthSMTLibProtocolPlan identity local
  -> LengthSMTLibProtocolPhaseState
  -> LengthSMTLibProtocolReceiver identity local
startReceiver plan phase = LengthSMTLibProtocolReceiver plan phase
  $ startSMTLibCausalStreamCursor $ planStreamPolicy plan

receiverAtBoundary
  :: LengthSMTLibProtocolPlan identity local
  -> LengthSMTLibProtocolPhaseState
  -> SMTLibCausalStreamBoundary
  -> LengthSMTLibProtocolReceiver identity local
receiverAtBoundary plan phase boundary = LengthSMTLibProtocolReceiver
  plan phase $ startSMTLibCausalStreamCursorAtBoundary boundary

acceptStreamStep
  :: LengthSMTLibProtocolPlan identity local
  -> LengthSMTLibProtocolPhaseState
  -> SMTLibCausalStreamStep
  -> Either
      LengthSMTLibProtocolError
      (LengthSMTLibProtocolAction identity local)
acceptStreamStep plan phase step = case step of
  SMTLibCausalStreamPending cursor -> Right $ SMTLibCausalAwait
    $ LengthSMTLibProtocolReceiver plan phase cursor
  SMTLibCausalStreamComplete completed -> handleFrame plan phase completed

consumeBoundary
  :: LengthSMTLibProtocolPhase
  -> SMTLibCausalStreamCompletedFrame
  -> Either LengthSMTLibProtocolError SMTLibCausalStreamBoundary
consumeBoundary phase = first (mapStreamFailure phase)
  . consumeSMTLibCausalStreamBoundaryWhitespace

mapStreamFailure
  :: LengthSMTLibProtocolPhase
  -> SMTLibCausalStreamFailure
  -> LengthSMTLibProtocolError
mapStreamFailure phase failure = case failure of
  SMTLibCausalStreamFramingFailure framing ->
    LengthSMTLibProtocolFramingFailure phase framing
  SMTLibCausalStreamCumulativeByteLimitExceeded maximumBytes observed ->
    LengthSMTLibProtocolCumulativeStdoutByteLimitExceeded
      maximumBytes observed
  SMTLibCausalStreamUnexpectedBoundaryByte offset byte ->
    LengthSMTLibProtocolUnexpectedPostBarrierByte offset byte

receiverPlan
  :: LengthSMTLibProtocolReceiver identity local
  -> LengthSMTLibProtocolPlan identity local
receiverPlan (LengthSMTLibProtocolReceiver value _ _) = value

receiverPhase
  :: LengthSMTLibProtocolReceiver identity local
  -> LengthSMTLibProtocolPhaseState
receiverPhase (LengthSMTLibProtocolReceiver _ value _) = value

receiverCursor
  :: LengthSMTLibProtocolReceiver identity local
  -> SMTLibCausalStreamCursor
receiverCursor (LengthSMTLibProtocolReceiver _ _ value) = value

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
planExecution (LengthSMTLibProtocolPlan value _ _ _ _ _) = value

planQuery
  :: LengthSMTLibProtocolPlan identity local
  -> LengthSMTLibQuery identity local
planQuery (LengthSMTLibProtocolPlan _ value _ _ _ _) = value

planStreamPolicy
  :: LengthSMTLibProtocolPlan identity local
  -> SMTLibCausalStreamPolicy
planStreamPolicy (LengthSMTLibProtocolPlan _ _ value _ _ _) = value

limitsStreamPolicy
  :: LengthSMTLibProtocolLimits
  -> SMTLibCausalStreamPolicy
limitsStreamPolicy (LengthSMTLibProtocolLimits value _) = value

planCheckBarrier
  :: LengthSMTLibProtocolPlan identity local
  -> SMTLibEchoSentinel
planCheckBarrier
    (LengthSMTLibProtocolPlan _ _ _ value _ _) = value

planValueBarrier
  :: LengthSMTLibProtocolPlan identity local
  -> Maybe SMTLibEchoSentinel
planValueBarrier
    (LengthSMTLibProtocolPlan _ _ _ _ value _) = value

validatePlanFraming
  :: LengthSMTLibProtocolLimits
  -> LengthSMTLibExecutionConfig
  -> [[Word8]]
  -> Bool
  -> Either LengthSMTLibProtocolPlanError ()
validatePlanFraming limits execution symbols requiresValues = do
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
  let minimumBytes = minimumProtocolStdoutBytes valueBytes requiresValues
      maximumBytes = lengthSMTLibProtocolCumulativeStdoutByteLimit limits
  if maximumBytes < minimumBytes
    then Left $ LengthSMTLibProtocolMinimumStdoutByteLimitExceeded
      maximumBytes minimumBytes
    else Right ()
 where
  stream = lengthSMTLibProtocolStreamLimits limits
  responses = lengthSMTLibExecutionResponseLimits execution
  valueBytes = minimalInputValueFrameByteCount symbols
  valueNodes = 1 + 3 * genericLength
    symbols
  valueTokenBytes = maximum $ 1 : map genericLength
    symbols

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
  :: Natural
  -> Bool
  -> Natural
minimumProtocolStdoutBytes valueBytes requiresValues =
  max unknownPath valuePath
 where
  marker = fixedSentinelResponseByteCount
  -- Every bare status and quoted marker needs one lexical separator.
  unknownPath = 7 + 1 + marker + 1
  valuePath
    | requiresValues =
        3 + 1 + marker + 1 +
        valueBytes + marker + 1
    | otherwise = 3 + 1 + marker + 1

minimalInputValueFrameByteCount
  :: [[Word8]]
  -> Natural
minimalInputValueFrameByteCount symbols = genericLength $
  [openParen] ++ concatMap binding
    symbols ++ [closeParen]
 where
  binding symbol =
    [openParen] ++ symbol ++ [space, digitZero, closeParen]

buildPlanFingerprint
  :: LengthSMTLibProtocolLimits
  -> LengthSMTLibExecutionConfig
  -> LengthSMTLibQuery identity local
  -> Maybe [Word8]
  -> SMTLibEchoSentinel
  -> Maybe SMTLibEchoSentinel
  -> [Word8]
  -> Maybe [Word8]
  -> Either
      LengthSMTLibProtocolPlanError
      (Fingerprint LengthSMTLibProtocolPlanFingerprintSubject)
buildPlanFingerprint limits execution query valueRequest checkBarrier valueBarrier
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
              ( valueRequest
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

ascii :: String -> [Word8]
ascii = map $ fromIntegral . fromEnum

space, openParen, closeParen, digitZero :: Word8
space = 32
openParen = 40
closeParen = 41
digitZero = 48
