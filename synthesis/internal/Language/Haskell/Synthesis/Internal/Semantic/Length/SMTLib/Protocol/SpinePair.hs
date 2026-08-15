{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Package-private, pure framing and phase control for one binary-product Length/Z3 query.
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
module Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol.SpinePair
  ( lengthSpinePairSMTLibProtocolPlanSchemaTag
  , lengthSpinePairSMTLibProtocolPhaseMachineSchemaTag
  , lengthSpinePairSMTLibProtocolPostBarrierSchemaTag
  , LengthSpinePairSMTLibProtocolLimitSource (..)
  , LengthSpinePairSMTLibProtocolLimits
  , mkLengthSpinePairSMTLibProtocolLimits
  , defaultLengthSpinePairSMTLibProtocolLimitSource
  , defaultLengthSpinePairSMTLibProtocolLimits
  , lengthSpinePairSMTLibProtocolStreamLimits
  , lengthSpinePairSMTLibProtocolCumulativeStdoutByteLimit
  , lengthSpinePairSMTLibProtocolPlanFingerprintByteLimit
  , LengthSpinePairSMTLibProtocolBarrier (..)
  , LengthSpinePairSMTLibProtocolRequiredFrame (..)
  , LengthSpinePairSMTLibProtocolRequiredLimit (..)
  , LengthSpinePairSMTLibProtocolPlanError (..)
  , LengthSpinePairSMTLibProtocolPlan
  , LengthSpinePairSMTLibProtocolPlanFingerprintSubject
  , sealLengthSpinePairSMTLibProtocolPlan
  , lengthSpinePairSMTLibProtocolInitialWriteBytes
  , lengthSpinePairSMTLibProtocolInputValueWriteBytes
  , lengthSpinePairSMTLibProtocolPlanFingerprint
  , lengthSpinePairSMTLibProtocolPlanQuery
  , lengthSpinePairSMTLibProtocolPlanArtifactPolicy
  , lengthSpinePairSMTLibProtocolPlanCumulativeStdoutByteLimit
  , lengthSpinePairSMTLibProtocolPlanMinimumStdoutByteCount
  , LengthSpinePairSMTLibProtocolPhase (..)
  , LengthSpinePairSMTLibProtocolWriteKind (..)
  , LengthSpinePairSMTLibProtocolReceiver
  , lengthSpinePairSMTLibProtocolReceiverPhase
  , LengthSpinePairSMTLibProtocolAction
  , startLengthSpinePairSMTLibProtocol
  , feedLengthSpinePairSMTLibProtocol
  , finishLengthSpinePairSMTLibProtocol
  , LengthSpinePairSMTLibProtocolError (..)
  , LengthSpinePairSMTLibProtocolDecoded
  , LengthSpinePairSMTLibProtocolObservation
  , lengthSpinePairSMTLibProtocolDecodedObservation
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
  , LengthSMTLibPostLaunchExecutionPolicy
  , lengthSMTLibPostLaunchArtifactPolicy
  , lengthSMTLibPostLaunchExecutionPolicyFingerprint
  , lengthSMTLibPostLaunchResponseLimits
  )
import Language.Haskell.Synthesis.Internal.SMTLib.Causal
  ( SMTLibCausalAction (..) )
import Language.Haskell.Synthesis.Internal.SMTLib.Z3.Execution
  ( z3SMTLibExecutionQueryResetBytes )
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
import Language.Haskell.Synthesis.Internal.SMTLib.Lexical
  ( smtLibWhitespaceBytes )
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
  )
import Language.Haskell.Synthesis.Semantic.Length.SMTLib
  ( LengthSMTLibIntegerBinding
  , LengthSpinePairSMTLibQuery
  , lengthSpinePairSMTLibQueryLogic
  , lengthSpinePairSMTLibQuerySchemaTag
  , lengthSpinePairSMTLibQueryCheckBytes
  , lengthSpinePairSMTLibQueryFingerprint
  , lengthSpinePairSMTLibQueryInputSymbols
  , lengthSpinePairSMTLibQueryInputValueRequestBytes
  )
import Language.Haskell.Synthesis.Semantic.Length.SMTLib.Response
  ( LengthSMTLibResponseError
  , LengthSMTLibResponseLimits
  , lengthSMTLibResponseByteLimit
  , lengthSMTLibResponseNestingDepthLimit
  , lengthSMTLibResponseNodeLimit
  , lengthSMTLibResponseSchemaTag
  , lengthSMTLibResponseTokenByteLimit
  , parseLengthSMTLibCheckResponse
  , parseLengthSpinePairSMTLibInputValueResponse
  )
import Language.Haskell.Synthesis.Semantic.Observation
  ( SolverObservation (..)
  , SolverStatus (..)
  )

-- | Complete pure-plan schema.  This is distinct from a live session or run
-- identity: no process has been opened, inspected, or observed here.
lengthSpinePairSMTLibProtocolPlanSchemaTag :: [Word8]
lengthSpinePairSMTLibProtocolPlanSchemaTag =
  ascii "djex-length-spine-pair-z3-smtlib2-protocol-plan/v1"

-- | Ordered write, decode, and exact positional barrier policy.
lengthSpinePairSMTLibProtocolPhaseMachineSchemaTag :: [Word8]
lengthSpinePairSMTLibProtocolPhaseMachineSchemaTag =
  ascii "djex-length-spine-pair-z3-smtlib2-protocol-phase-machine/v1"

-- | Only the four SMT-LIB whitespace bytes may remain in the chunk which
-- crosses a barrier into a new write or terminal action.  Comments are not
-- accepted as transport trivia.
lengthSpinePairSMTLibProtocolPostBarrierSchemaTag :: [Word8]
lengthSpinePairSMTLibProtocolPostBarrierSchemaTag =
  ascii "djex-smtlib2-post-barrier-whitespace/v1"

-- | Raw pure-protocol bounds.  Stream limits are semantic and reset for each
-- expected frame.  The cumulative stdout limit charges every consumed frame
-- byte and every accepted post-barrier whitespace byte across the transaction.
-- The fingerprint limit is admission-only and is not part of plan identity.
data LengthSpinePairSMTLibProtocolLimitSource = LengthSpinePairSMTLibProtocolLimitSource
  { lengthSpinePairSMTLibProtocolLimitSourceStreamLimits :: SMTLibStreamLimitSource
  , lengthSpinePairSMTLibProtocolLimitSourceCumulativeStdoutBytes :: Natural
  , lengthSpinePairSMTLibProtocolLimitSourcePlanFingerprintBytes :: Natural
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSpinePairSMTLibProtocolLimitSource

data LengthSpinePairSMTLibProtocolLimits = LengthSpinePairSMTLibProtocolLimits
  !SMTLibCausalStreamPolicy !Natural
  deriving (Eq, Ord)

instance NFData LengthSpinePairSMTLibProtocolLimits where
  rnf (LengthSpinePairSMTLibProtocolLimits streamPolicy fingerprint) =
    rnf streamPolicy `seq` rnf fingerprint

mkLengthSpinePairSMTLibProtocolLimits
  :: LengthSpinePairSMTLibProtocolLimitSource
  -> LengthSpinePairSMTLibProtocolLimits
mkLengthSpinePairSMTLibProtocolLimits source = LengthSpinePairSMTLibProtocolLimits
  (mkSMTLibCausalStreamPolicy
    (mkSMTLibStreamLimits
      $ lengthSpinePairSMTLibProtocolLimitSourceStreamLimits source)
    (lengthSpinePairSMTLibProtocolLimitSourceCumulativeStdoutBytes source))
  (lengthSpinePairSMTLibProtocolLimitSourcePlanFingerprintBytes source)

defaultLengthSpinePairSMTLibProtocolLimitSource :: LengthSpinePairSMTLibProtocolLimitSource
defaultLengthSpinePairSMTLibProtocolLimitSource = LengthSpinePairSMTLibProtocolLimitSource
  { lengthSpinePairSMTLibProtocolLimitSourceStreamLimits =
      defaultSMTLibStreamLimitSource
  , lengthSpinePairSMTLibProtocolLimitSourceCumulativeStdoutBytes = 524288
  , lengthSpinePairSMTLibProtocolLimitSourcePlanFingerprintBytes = 262144
  }

defaultLengthSpinePairSMTLibProtocolLimits :: LengthSpinePairSMTLibProtocolLimits
defaultLengthSpinePairSMTLibProtocolLimits =
  mkLengthSpinePairSMTLibProtocolLimits defaultLengthSpinePairSMTLibProtocolLimitSource

lengthSpinePairSMTLibProtocolStreamLimits
  :: LengthSpinePairSMTLibProtocolLimits
  -> SMTLibStreamLimits
lengthSpinePairSMTLibProtocolStreamLimits
    (LengthSpinePairSMTLibProtocolLimits streamPolicy _) =
      smtLibCausalStreamPolicyStreamLimits streamPolicy

lengthSpinePairSMTLibProtocolCumulativeStdoutByteLimit
  :: LengthSpinePairSMTLibProtocolLimits
  -> Natural
lengthSpinePairSMTLibProtocolCumulativeStdoutByteLimit
    (LengthSpinePairSMTLibProtocolLimits streamPolicy _) =
      smtLibCausalStreamPolicyCumulativeByteLimit streamPolicy

lengthSpinePairSMTLibProtocolPlanFingerprintByteLimit
  :: LengthSpinePairSMTLibProtocolLimits
  -> Natural
lengthSpinePairSMTLibProtocolPlanFingerprintByteLimit
    (LengthSpinePairSMTLibProtocolLimits _ value) = value

data LengthSpinePairSMTLibProtocolBarrier
  = LengthSpinePairSMTLibProtocolCheckBarrier
  | LengthSpinePairSMTLibProtocolInputValueBarrier
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSpinePairSMTLibProtocolBarrier

data LengthSpinePairSMTLibProtocolRequiredFrame
  = LengthSpinePairSMTLibProtocolCheckStatusFrame
  | LengthSpinePairSMTLibProtocolCheckBarrierFrame
  | LengthSpinePairSMTLibProtocolInputValueFrame
  | LengthSpinePairSMTLibProtocolInputValueBarrierFrame
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSpinePairSMTLibProtocolRequiredFrame

data LengthSpinePairSMTLibProtocolRequiredLimit
  = LengthSpinePairSMTLibProtocolStreamTotalBytes
  | LengthSpinePairSMTLibProtocolStreamFrameBytes
  | LengthSpinePairSMTLibProtocolStreamNestingDepth
  | LengthSpinePairSMTLibProtocolResponseBytes
  | LengthSpinePairSMTLibProtocolResponseNestingDepth
  | LengthSpinePairSMTLibProtocolResponseNodes
  | LengthSpinePairSMTLibProtocolResponseTokenBytes
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSpinePairSMTLibProtocolRequiredLimit

-- | Fixed-precedence plan admission failures.  No constructor retains nonce,
-- marker, executable-path, or executable-pin material.
data LengthSpinePairSMTLibProtocolPlanError
  = LengthSpinePairSMTLibProtocolRequiredLimitTooSmall
      !LengthSpinePairSMTLibProtocolRequiredFrame
      !LengthSpinePairSMTLibProtocolRequiredLimit
      !Natural
      !Natural
  | LengthSpinePairSMTLibProtocolMinimumStdoutByteLimitExceeded !Natural !Natural
  | LengthSpinePairSMTLibProtocolBarrierNonceError
      !LengthSpinePairSMTLibProtocolBarrier !SMTLibEchoSentinelError
  | LengthSpinePairSMTLibProtocolMissingInputValueBarrierNonce
  | LengthSpinePairSMTLibProtocolUnexpectedInputValueBarrierNonce
  | LengthSpinePairSMTLibProtocolRepeatedBarrierNonce
  | LengthSpinePairSMTLibProtocolPlanFingerprintByteLimitExceeded !Natural !Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSpinePairSMTLibProtocolPlanError

-- | Opaque association of exact artifact/response policy, query, framing
-- policy, barriers, and the complete identity of their deterministic writes.
-- Sealing accepts the associated post-launch policy rather than the full
-- structured execution configuration; it consumes that policy's artifact,
-- response, and original complete-key projections, while the deadline remains
-- a worker concern.  The artifact/response projections remain runtime fields,
-- and the complete canonical key remains embedded in the reversible plan
-- fingerprint.  The writes are rendered transiently for fingerprint admission
-- and later derived on demand through the selectors used at their causal
-- action edges.
data LengthSpinePairSMTLibProtocolPlan identity local = LengthSpinePairSMTLibProtocolPlan
  !LengthSMTLibArtifactPolicy
  !LengthSMTLibResponseLimits
  !(LengthSpinePairSMTLibQuery identity local)
  !SMTLibCausalStreamPolicy
  !SMTLibEchoSentinel
  !(Maybe SMTLibEchoSentinel)
  !(Fingerprint LengthSpinePairSMTLibProtocolPlanFingerprintSubject)

type role LengthSpinePairSMTLibProtocolPlan nominal nominal

instance NFData (LengthSpinePairSMTLibProtocolPlan identity local) where
  rnf (LengthSpinePairSMTLibProtocolPlan artifact responses query streamPolicy
      checkBarrier valueBarrier fingerprint) =
    rnf artifact `seq` rnf responses `seq` rnf query `seq`
    rnf streamPolicy `seq`
    rnf checkBarrier `seq` rnf valueBarrier `seq` rnf fingerprint

data LengthSpinePairSMTLibProtocolPlanFingerprintSubject

-- | Seal a pure transaction from caller-provided barrier nonce bytes.  The
-- optional value nonce is required exactly when the artifact policy requests
-- values and the query has at least one input symbol.
sealLengthSpinePairSMTLibProtocolPlan
  :: LengthSpinePairSMTLibProtocolLimits
  -> LengthSMTLibPostLaunchExecutionPolicy
  -> LengthSpinePairSMTLibQuery identity local
  -> [Word8]
  -> Maybe [Word8]
  -> Either
      LengthSpinePairSMTLibProtocolPlanError
      (LengthSpinePairSMTLibProtocolPlan identity local)
sealLengthSpinePairSMTLibProtocolPlan limits execution query
    rawCheckNonce rawValueNonce = do
  let symbols = lengthSpinePairSMTLibQueryInputSymbols query
      valueRequest = lengthSpinePairSMTLibQueryInputValueRequestBytes query
      requiresValues =
        lengthSMTLibPostLaunchArtifactPolicy execution ==
          LengthSMTLibInputValuesAfterSatisfiable &&
        isJust valueRequest
  validatePlanFraming limits execution symbols requiresValues
  checkBarrier <- first
    (LengthSpinePairSMTLibProtocolBarrierNonceError
      LengthSpinePairSMTLibProtocolCheckBarrier)
    $ mkSMTLibEchoSentinel rawCheckNonce
  valueBarrier <- case (requiresValues, rawValueNonce) of
    (False, Nothing) -> Right Nothing
    (False, Just _) ->
      Left LengthSpinePairSMTLibProtocolUnexpectedInputValueBarrierNonce
    (True, Nothing) ->
      Left LengthSpinePairSMTLibProtocolMissingInputValueBarrierNonce
    (True, Just nonce) -> Just <$> first
      (LengthSpinePairSMTLibProtocolBarrierNonceError
        LengthSpinePairSMTLibProtocolInputValueBarrier)
      (mkSMTLibEchoSentinel nonce)
  case valueBarrier of
    Just barrier
      | barrier == checkBarrier ->
          Left LengthSpinePairSMTLibProtocolRepeatedBarrierNonce
    _ -> Right ()
  let initialWrite = renderProtocolInitialWrite query checkBarrier
      valueWrite = renderProtocolInputValueWrite valueRequest valueBarrier
  fingerprint <- buildPlanFingerprint limits execution query valueRequest
    checkBarrier valueBarrier initialWrite valueWrite
  pure $ LengthSpinePairSMTLibProtocolPlan
    (lengthSMTLibPostLaunchArtifactPolicy execution)
    (lengthSMTLibPostLaunchResponseLimits execution)
    query
    (limitsStreamPolicy limits)
    checkBarrier valueBarrier fingerprint

renderProtocolInitialWrite
  :: LengthSpinePairSMTLibQuery identity local
  -> SMTLibEchoSentinel
  -> [Word8]
renderProtocolInitialWrite query barrier =
  z3SMTLibExecutionQueryResetBytes ++
  lengthSpinePairSMTLibQueryCheckBytes query ++
  smtLibEchoSentinelCommandBytes barrier

renderProtocolInputValueWrite
  :: Maybe [Word8]
  -> Maybe SMTLibEchoSentinel
  -> Maybe [Word8]
renderProtocolInputValueWrite _ Nothing = Nothing
renderProtocolInputValueWrite request (Just barrier) =
  fmap (++ smtLibEchoSentinelCommandBytes barrier)
    request

lengthSpinePairSMTLibProtocolInitialWriteBytes
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> [Word8]
lengthSpinePairSMTLibProtocolInitialWriteBytes
    (LengthSpinePairSMTLibProtocolPlan _ _ query _ checkBarrier _ _) =
      renderProtocolInitialWrite query checkBarrier

lengthSpinePairSMTLibProtocolInputValueWriteBytes
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> Maybe [Word8]
lengthSpinePairSMTLibProtocolInputValueWriteBytes
    (LengthSpinePairSMTLibProtocolPlan _ _ query _ _ valueBarrier _) =
      renderProtocolInputValueWrite
        (lengthSpinePairSMTLibQueryInputValueRequestBytes query) valueBarrier

lengthSpinePairSMTLibProtocolPlanFingerprint
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> Fingerprint LengthSpinePairSMTLibProtocolPlanFingerprintSubject
lengthSpinePairSMTLibProtocolPlanFingerprint
    (LengthSpinePairSMTLibProtocolPlan _ _ _ _ _ _ value) = value

-- | Exact query retained by this sealed protocol plan.
lengthSpinePairSMTLibProtocolPlanQuery
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> LengthSpinePairSMTLibQuery identity local
lengthSpinePairSMTLibProtocolPlanQuery = planQuery

-- | Artifact policy retained by this exact sealed protocol plan.
lengthSpinePairSMTLibProtocolPlanArtifactPolicy
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> LengthSMTLibArtifactPolicy
lengthSpinePairSMTLibProtocolPlanArtifactPolicy = planArtifactPolicy

-- | Final causal transcript cap retained by this exact sealed plan.
lengthSpinePairSMTLibProtocolPlanCumulativeStdoutByteLimit
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> Natural
lengthSpinePairSMTLibProtocolPlanCumulativeStdoutByteLimit =
  smtLibCausalStreamPolicyCumulativeByteLimit . planStreamPolicy

-- | Smallest complete live transcript admitted by this exact plan, including
-- the required lexical delimiter after each bare status and final echo.  A
-- session uses this before reserving an ordinal so the remaining process-wide
-- stdout budget can admit at least one complete branch.
lengthSpinePairSMTLibProtocolPlanMinimumStdoutByteCount
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> Natural
lengthSpinePairSMTLibProtocolPlanMinimumStdoutByteCount plan =
  minimumProtocolStdoutBytes
    (minimalInputValueFrameByteCount
      $ lengthSpinePairSMTLibQueryInputSymbols $ planQuery plan)
    $ case planValueBarrier plan of
        Nothing -> False
        Just _ -> True

data LengthSpinePairSMTLibProtocolPhase
  = LengthSpinePairSMTLibProtocolCheckStatusPhase
  | LengthSpinePairSMTLibProtocolCheckBarrierPhase
  | LengthSpinePairSMTLibProtocolInputValuePhase
  | LengthSpinePairSMTLibProtocolInputValueBarrierPhase
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSpinePairSMTLibProtocolPhase

data LengthSpinePairSMTLibProtocolWriteKind
  = LengthSpinePairSMTLibProtocolInitialQueryWrite
  | LengthSpinePairSMTLibProtocolInputValueWrite
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSpinePairSMTLibProtocolWriteKind

data LengthSpinePairSMTLibProtocolPhaseState
  = AwaitLengthSMTLibCheckStatus
  | AwaitLengthSMTLibCheckBarrier !SolverStatus
  | AwaitLengthSMTLibInputValues
  | AwaitLengthSMTLibInputValueBarrier [LengthSMTLibIntegerBinding]

instance NFData LengthSpinePairSMTLibProtocolPhaseState where
  rnf state = case state of
    AwaitLengthSMTLibCheckStatus -> ()
    AwaitLengthSMTLibCheckBarrier status -> rnf status
    AwaitLengthSMTLibInputValues -> ()
    AwaitLengthSMTLibInputValueBarrier bindings -> rnf bindings

-- | A continuation for bytes received only after the write which produced it
-- has completed.  Constructors and raw framing state never leave the package.
data LengthSpinePairSMTLibProtocolReceiver identity local =
  LengthSpinePairSMTLibProtocolReceiver
    !(LengthSpinePairSMTLibProtocolPlan identity local)
    !LengthSpinePairSMTLibProtocolPhaseState
    !SMTLibCausalStreamCursor

type role LengthSpinePairSMTLibProtocolReceiver nominal nominal

instance NFData (LengthSpinePairSMTLibProtocolReceiver identity local) where
  rnf (LengthSpinePairSMTLibProtocolReceiver plan phase cursor) =
    rnf plan `seq` rnf phase `seq` rnf cursor

lengthSpinePairSMTLibProtocolReceiverPhase
  :: LengthSpinePairSMTLibProtocolReceiver identity local
  -> LengthSpinePairSMTLibProtocolPhase
lengthSpinePairSMTLibProtocolReceiverPhase
    (LengthSpinePairSMTLibProtocolReceiver _ phase _) = phaseName phase

-- | The shared causal action specialized to this plan's nominal receiver and
-- decoded outcome.  The shared type keeps all three parameters nominal.
type LengthSpinePairSMTLibProtocolAction identity local =
  SMTLibCausalAction
    LengthSpinePairSMTLibProtocolWriteKind
    (LengthSpinePairSMTLibProtocolReceiver identity local)
    (LengthSpinePairSMTLibProtocolDecoded identity local)

-- | Start with reset, canonical check commands, and the status barrier in one
-- exact write.  Any unexpected reset response consequently occupies the
-- status slot and fails closed.
startLengthSpinePairSMTLibProtocol
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> LengthSpinePairSMTLibProtocolAction identity local
startLengthSpinePairSMTLibProtocol plan = SMTLibCausalWrite
  LengthSpinePairSMTLibProtocolInitialQueryWrite
  (lengthSpinePairSMTLibProtocolInitialWriteBytes plan)
  (startReceiver plan AwaitLengthSMTLibCheckStatus)

feedLengthSpinePairSMTLibProtocol
  :: LengthSpinePairSMTLibProtocolReceiver identity local
  -> [Word8]
  -> Either
      LengthSpinePairSMTLibProtocolError
      (LengthSpinePairSMTLibProtocolAction identity local)
feedLengthSpinePairSMTLibProtocol receiver bytes = do
  step <- first
    (mapStreamFailure $ lengthSpinePairSMTLibProtocolReceiverPhase receiver)
    $ feedSMTLibCausalStreamCursor (receiverCursor receiver) bytes
  acceptStreamStep (receiverPlan receiver) (receiverPhase receiver) step

-- | EOF is never a successful terminal delimiter for a reusable worker.
-- Lexical EOF failures retain precedence; otherwise every phase reports its
-- exact missing response position.
finishLengthSpinePairSMTLibProtocol
  :: LengthSpinePairSMTLibProtocolReceiver identity local
  -> Either
      LengthSpinePairSMTLibProtocolError
      (LengthSpinePairSMTLibProtocolAction identity local)
finishLengthSpinePairSMTLibProtocol receiver = case
    finishSMTLibCausalStreamCursor $ receiverCursor receiver of
  Left failure -> Left $ mapStreamFailure
    (lengthSpinePairSMTLibProtocolReceiverPhase receiver) failure
  Right _ -> Left $ LengthSpinePairSMTLibProtocolUnexpectedEOF
    $ lengthSpinePairSMTLibProtocolReceiverPhase receiver

data LengthSpinePairSMTLibProtocolError
  = LengthSpinePairSMTLibProtocolFramingFailure
      !LengthSpinePairSMTLibProtocolPhase !SMTLibStreamFramingError
  | LengthSpinePairSMTLibProtocolResponseFailure
      !LengthSpinePairSMTLibProtocolPhase !LengthSMTLibResponseError
  | LengthSpinePairSMTLibProtocolBarrierMismatch !LengthSpinePairSMTLibProtocolBarrier
  | LengthSpinePairSMTLibProtocolCumulativeStdoutByteLimitExceeded !Natural !Natural
  | LengthSpinePairSMTLibProtocolUnexpectedPostBarrierByte !Natural !Word8
  | LengthSpinePairSMTLibProtocolUnexpectedEOF !LengthSpinePairSMTLibProtocolPhase
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSpinePairSMTLibProtocolError

-- | The one status-indexed value admitted by pure protocol completion.
-- Satisfiable observations distinguish status-only @Nothing@, vacuous
-- zero-input @Just []@, and framed @Just (_ : _)@ values. Unsatisfiable and
-- unknown observations have no value-bearing constructor.
type LengthSpinePairSMTLibProtocolObservation =
  SolverObservation (Maybe [LengthSMTLibIntegerBinding]) () ()

-- | Pure, syntactically decoded transcript outcome. A satisfiable zero-input
-- query under the input-value policy carries a vacuous @Just []@ artifact
-- without fabricating a frame or emitting an empty @get-value@ command. The
-- receiver owns the plan until completion, and the live Session carries that
-- same plan separately as the exact run-identity input; it is not copied into
-- this terminal branch. In a live run raw status and input-value frames
-- likewise remain in the process-owning causal transcript. This value remains
-- a pure response/protocol result, not an execution observation.
data LengthSpinePairSMTLibProtocolDecoded identity local = LengthSpinePairSMTLibProtocolDecoded
  !LengthSpinePairSMTLibProtocolObservation

type role LengthSpinePairSMTLibProtocolDecoded nominal nominal

instance NFData (LengthSpinePairSMTLibProtocolDecoded identity local) where
  rnf (LengthSpinePairSMTLibProtocolDecoded observation) = rnf observation

lengthSpinePairSMTLibProtocolDecodedObservation
  :: LengthSpinePairSMTLibProtocolDecoded identity local
  -> LengthSpinePairSMTLibProtocolObservation
lengthSpinePairSMTLibProtocolDecodedObservation
    (LengthSpinePairSMTLibProtocolDecoded observation) = observation

-- Generic 'SolverObservation' artifacts deliberately remain lazy. This
-- trusted wrapper separately forces only the satisfiable 'Maybe' spine to
-- preserve the former strict decoded-field demand without forcing bindings.
retainLengthSpinePairSMTLibProtocolDecoded
  :: LengthSpinePairSMTLibProtocolObservation
  -> LengthSpinePairSMTLibProtocolDecoded identity local
retainLengthSpinePairSMTLibProtocolDecoded observation = case observation of
  SatisfiableObservation values -> values `seq`
    LengthSpinePairSMTLibProtocolDecoded observation
  UnsatisfiableObservation () -> LengthSpinePairSMTLibProtocolDecoded observation
  UnknownObservation () -> LengthSpinePairSMTLibProtocolDecoded observation

handleFrame
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> LengthSpinePairSMTLibProtocolPhaseState
  -> SMTLibCausalStreamCompletedFrame
  -> Either
      LengthSpinePairSMTLibProtocolError
      (LengthSpinePairSMTLibProtocolAction identity local)
handleFrame plan phase completed = case phase of
  AwaitLengthSMTLibCheckStatus -> do
    let limits = planResponseLimits plan
    status <- first
      (LengthSpinePairSMTLibProtocolResponseFailure
        LengthSpinePairSMTLibProtocolCheckStatusPhase)
      $ parseLengthSMTLibCheckResponse limits frame
    continueWithinWrite plan
      (AwaitLengthSMTLibCheckBarrier status) completed
  AwaitLengthSMTLibCheckBarrier status -> do
    if isExactSMTLibEchoSentinelResponse (planCheckBarrier plan) frame
      then case
          ( status
          , planValueBarrier plan
          , lengthSpinePairSMTLibProtocolInputValueWriteBytes plan
          ) of
        (SolverSatisfiable, Just _, Just valueWrite) -> do
          boundary <- consumeBoundary
            LengthSpinePairSMTLibProtocolCheckBarrierPhase completed
          Right $ SMTLibCausalWrite
            LengthSpinePairSMTLibProtocolInputValueWrite valueWrite
            $ receiverAtBoundary plan
                AwaitLengthSMTLibInputValues
                boundary
        _ -> do
          _ <- consumeBoundary
            LengthSpinePairSMTLibProtocolCheckBarrierPhase completed
          Right $ SMTLibCausalComplete
            $ retainLengthSpinePairSMTLibProtocolDecoded
            $ terminalObservation plan status
      else Left $ LengthSpinePairSMTLibProtocolBarrierMismatch
        LengthSpinePairSMTLibProtocolCheckBarrier
  AwaitLengthSMTLibInputValues -> do
    let limits = planResponseLimits plan
    bindings <- first
      (LengthSpinePairSMTLibProtocolResponseFailure
        LengthSpinePairSMTLibProtocolInputValuePhase)
      $ parseLengthSpinePairSMTLibInputValueResponse limits (planQuery plan) frame
    continueWithinWrite plan
      (AwaitLengthSMTLibInputValueBarrier bindings)
      completed
  AwaitLengthSMTLibInputValueBarrier bindings -> do
    case planValueBarrier plan of
      Just barrier
        | isExactSMTLibEchoSentinelResponse barrier frame -> do
            _ <- consumeBoundary
              LengthSpinePairSMTLibProtocolInputValueBarrierPhase completed
            Right $ SMTLibCausalComplete
              $ retainLengthSpinePairSMTLibProtocolDecoded
              $ SatisfiableObservation $ Just bindings
      _ -> Left $ LengthSpinePairSMTLibProtocolBarrierMismatch
        LengthSpinePairSMTLibProtocolInputValueBarrier
 where
  frame = smtLibCausalStreamCompletedFrameBytes completed

continueWithinWrite
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> LengthSpinePairSMTLibProtocolPhaseState
  -> SMTLibCausalStreamCompletedFrame
  -> Either
      LengthSpinePairSMTLibProtocolError
      (LengthSpinePairSMTLibProtocolAction identity local)
continueWithinWrite plan phase completed = do
  step <- first (mapStreamFailure $ phaseName phase)
    $ continueSMTLibCausalStreamCompletedFrame completed
  acceptStreamStep plan phase step

terminalObservation
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> SolverStatus
  -> LengthSpinePairSMTLibProtocolObservation
terminalObservation plan status = case status of
  SolverSatisfiable -> SatisfiableObservation
    $ if planArtifactPolicy plan ==
          LengthSMTLibInputValuesAfterSatisfiable
        && not (isJust
          $ lengthSpinePairSMTLibQueryInputValueRequestBytes $ planQuery plan)
      then Just []
      else Nothing
  SolverUnsatisfiable -> UnsatisfiableObservation ()
  SolverUnknown -> UnknownObservation ()

startReceiver
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> LengthSpinePairSMTLibProtocolPhaseState
  -> LengthSpinePairSMTLibProtocolReceiver identity local
startReceiver plan phase = LengthSpinePairSMTLibProtocolReceiver plan phase
  $ startSMTLibCausalStreamCursor $ planStreamPolicy plan

receiverAtBoundary
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> LengthSpinePairSMTLibProtocolPhaseState
  -> SMTLibCausalStreamBoundary
  -> LengthSpinePairSMTLibProtocolReceiver identity local
receiverAtBoundary plan phase boundary = LengthSpinePairSMTLibProtocolReceiver
  plan phase $ startSMTLibCausalStreamCursorAtBoundary boundary

acceptStreamStep
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> LengthSpinePairSMTLibProtocolPhaseState
  -> SMTLibCausalStreamStep
  -> Either
      LengthSpinePairSMTLibProtocolError
      (LengthSpinePairSMTLibProtocolAction identity local)
acceptStreamStep plan phase step = case step of
  SMTLibCausalStreamPending cursor -> Right $ SMTLibCausalAwait
    $ LengthSpinePairSMTLibProtocolReceiver plan phase cursor
  SMTLibCausalStreamComplete completed -> handleFrame plan phase completed

consumeBoundary
  :: LengthSpinePairSMTLibProtocolPhase
  -> SMTLibCausalStreamCompletedFrame
  -> Either LengthSpinePairSMTLibProtocolError SMTLibCausalStreamBoundary
consumeBoundary phase = first (mapStreamFailure phase)
  . consumeSMTLibCausalStreamBoundaryWhitespace

mapStreamFailure
  :: LengthSpinePairSMTLibProtocolPhase
  -> SMTLibCausalStreamFailure
  -> LengthSpinePairSMTLibProtocolError
mapStreamFailure phase failure = case failure of
  SMTLibCausalStreamFramingFailure framing ->
    LengthSpinePairSMTLibProtocolFramingFailure phase framing
  SMTLibCausalStreamCumulativeByteLimitExceeded maximumBytes observed ->
    LengthSpinePairSMTLibProtocolCumulativeStdoutByteLimitExceeded
      maximumBytes observed
  SMTLibCausalStreamUnexpectedBoundaryByte offset byte ->
    LengthSpinePairSMTLibProtocolUnexpectedPostBarrierByte offset byte

receiverPlan
  :: LengthSpinePairSMTLibProtocolReceiver identity local
  -> LengthSpinePairSMTLibProtocolPlan identity local
receiverPlan (LengthSpinePairSMTLibProtocolReceiver value _ _) = value

receiverPhase
  :: LengthSpinePairSMTLibProtocolReceiver identity local
  -> LengthSpinePairSMTLibProtocolPhaseState
receiverPhase (LengthSpinePairSMTLibProtocolReceiver _ value _) = value

receiverCursor
  :: LengthSpinePairSMTLibProtocolReceiver identity local
  -> SMTLibCausalStreamCursor
receiverCursor (LengthSpinePairSMTLibProtocolReceiver _ _ value) = value

phaseName :: LengthSpinePairSMTLibProtocolPhaseState -> LengthSpinePairSMTLibProtocolPhase
phaseName phase = case phase of
  AwaitLengthSMTLibCheckStatus -> LengthSpinePairSMTLibProtocolCheckStatusPhase
  AwaitLengthSMTLibCheckBarrier{} ->
    LengthSpinePairSMTLibProtocolCheckBarrierPhase
  AwaitLengthSMTLibInputValues{} ->
    LengthSpinePairSMTLibProtocolInputValuePhase
  AwaitLengthSMTLibInputValueBarrier{} ->
    LengthSpinePairSMTLibProtocolInputValueBarrierPhase

planArtifactPolicy
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> LengthSMTLibArtifactPolicy
planArtifactPolicy (LengthSpinePairSMTLibProtocolPlan value _ _ _ _ _ _) = value

planResponseLimits
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> LengthSMTLibResponseLimits
planResponseLimits (LengthSpinePairSMTLibProtocolPlan _ value _ _ _ _ _) = value

planQuery
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> LengthSpinePairSMTLibQuery identity local
planQuery (LengthSpinePairSMTLibProtocolPlan _ _ value _ _ _ _) = value

planStreamPolicy
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> SMTLibCausalStreamPolicy
planStreamPolicy (LengthSpinePairSMTLibProtocolPlan _ _ _ value _ _ _) = value

limitsStreamPolicy
  :: LengthSpinePairSMTLibProtocolLimits
  -> SMTLibCausalStreamPolicy
limitsStreamPolicy (LengthSpinePairSMTLibProtocolLimits value _) = value

planCheckBarrier
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> SMTLibEchoSentinel
planCheckBarrier
    (LengthSpinePairSMTLibProtocolPlan _ _ _ _ value _ _) = value

planValueBarrier
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> Maybe SMTLibEchoSentinel
planValueBarrier
    (LengthSpinePairSMTLibProtocolPlan _ _ _ _ _ value _) = value

validatePlanFraming
  :: LengthSpinePairSMTLibProtocolLimits
  -> LengthSMTLibPostLaunchExecutionPolicy
  -> [[Word8]]
  -> Bool
  -> Either LengthSpinePairSMTLibProtocolPlanError ()
validatePlanFraming limits execution symbols requiresValues = do
  validateStreamFrame LengthSpinePairSMTLibProtocolCheckStatusFrame
    checkStatusFrameByteCount 0
  validateResponseFrame LengthSpinePairSMTLibProtocolCheckStatusFrame
    checkStatusFrameByteCount 0 1 checkStatusFrameByteCount
  validateStreamFrame LengthSpinePairSMTLibProtocolCheckBarrierFrame
    fixedSentinelResponseByteCount 0
  if requiresValues
    then do
      validateStreamFrame LengthSpinePairSMTLibProtocolInputValueFrame valueBytes 2
      validateResponseFrame LengthSpinePairSMTLibProtocolInputValueFrame
        valueBytes 2 valueNodes valueTokenBytes
      validateStreamFrame LengthSpinePairSMTLibProtocolInputValueBarrierFrame
        fixedSentinelResponseByteCount 0
    else Right ()
  let minimumBytes = minimumProtocolStdoutBytes valueBytes requiresValues
      maximumBytes = lengthSpinePairSMTLibProtocolCumulativeStdoutByteLimit limits
  if maximumBytes < minimumBytes
    then Left $ LengthSpinePairSMTLibProtocolMinimumStdoutByteLimitExceeded
      maximumBytes minimumBytes
    else Right ()
 where
  stream = lengthSpinePairSMTLibProtocolStreamLimits limits
  responses = lengthSMTLibPostLaunchResponseLimits execution
  valueBytes = minimalInputValueFrameByteCount symbols
  valueNodes = 1 + 3 * genericLength
    symbols
  valueTokenBytes = maximum $ 1 : map genericLength
    symbols

  validateStreamFrame site required depth = do
    validateRequiredLimit site LengthSpinePairSMTLibProtocolStreamTotalBytes
      (smtLibStreamTotalByteLimit stream) required
    validateRequiredLimit site LengthSpinePairSMTLibProtocolStreamFrameBytes
      (smtLibStreamFrameByteLimit stream) required
    validateRequiredLimit site LengthSpinePairSMTLibProtocolStreamNestingDepth
      (smtLibStreamNestingDepthLimit stream) depth

  validateResponseFrame site bytes depth nodes tokenBytes = do
    validateRequiredLimit site LengthSpinePairSMTLibProtocolResponseBytes
      (lengthSMTLibResponseByteLimit responses) bytes
    validateRequiredLimit site LengthSpinePairSMTLibProtocolResponseNestingDepth
      (fromIntegral $ lengthSMTLibResponseNestingDepthLimit responses) depth
    validateRequiredLimit site LengthSpinePairSMTLibProtocolResponseNodes
      (lengthSMTLibResponseNodeLimit responses) nodes
    validateRequiredLimit site LengthSpinePairSMTLibProtocolResponseTokenBytes
      (lengthSMTLibResponseTokenByteLimit responses) tokenBytes

validateRequiredLimit
  :: LengthSpinePairSMTLibProtocolRequiredFrame
  -> LengthSpinePairSMTLibProtocolRequiredLimit
  -> Natural
  -> Natural
  -> Either LengthSpinePairSMTLibProtocolPlanError ()
validateRequiredLimit site field maximumValue required
  | maximumValue < required = Left $
      LengthSpinePairSMTLibProtocolRequiredLimitTooSmall
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
  :: LengthSpinePairSMTLibProtocolLimits
  -> LengthSMTLibPostLaunchExecutionPolicy
  -> LengthSpinePairSMTLibQuery identity local
  -> Maybe [Word8]
  -> SMTLibEchoSentinel
  -> Maybe SMTLibEchoSentinel
  -> [Word8]
  -> Maybe [Word8]
  -> Either
      LengthSpinePairSMTLibProtocolPlanError
      (Fingerprint LengthSpinePairSMTLibProtocolPlanFingerprintSubject)
buildPlanFingerprint limits execution query valueRequest checkBarrier valueBarrier
    initialWrite valueWrite = case
  buildFingerprintWithin maximumBytes FingerprintBuilder
    { fingerprintBuilderVersion = 1
    , fingerprintBuilderRole = ascii
        "finite-binary-product-spine-lengths/z3-smtlib2-protocol-plan"
    , fingerprintBuilderFields =
        [ tagged "schema"
            [FingerprintBytes lengthSpinePairSMTLibProtocolPlanSchemaTag]
        , tagged "execution-policy"
            [ FingerprintBytes $ fingerprintCanonicalBytes
                $ lengthSMTLibPostLaunchExecutionPolicyFingerprint execution
            ]
        , tagged "common-qf-lia-readiness-capability-reuse"
            [ FingerprintBytes $ ascii $ concat
                [ "reuse-scalar-named-ready-worker-only-as-exact-common-"
                , "qf-lia-input-value-transport-profile/"
                , "no-scalar-behavioral-authority/v1"
                ]
            , FingerprintBytes lengthSpinePairSMTLibQuerySchemaTag
            , FingerprintBytes lengthSpinePairSMTLibQueryLogic
            ]
        , tagged "query"
            [ FingerprintBytes lengthSpinePairSMTLibQuerySchemaTag
            , FingerprintBytes $ PublicFingerprint.fingerprintCanonicalBytes
                $ lengthSpinePairSMTLibQueryFingerprint query
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
            [ FingerprintBytes lengthSpinePairSMTLibProtocolPostBarrierSchemaTag
            , FingerprintBytes smtLibWhitespaceBytes
            ]
        , tagged "phase-machine"
            [ FingerprintBytes lengthSpinePairSMTLibProtocolPhaseMachineSchemaTag
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
            [ FingerprintBytes z3SMTLibExecutionQueryResetBytes
            , FingerprintBytes $ lengthSpinePairSMTLibQueryCheckBytes query
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
              | lengthSMTLibPostLaunchArtifactPolicy execution ==
                  LengthSMTLibInputValuesAfterSatisfiable ->
                  tagged "vacuous-zero-input" []
              | otherwise -> tagged "absent" []]
        ]
    } of
  Left FingerprintLimitExceeded
      { fingerprintMaximumBytes = admitted
      , fingerprintObservedBytesAtLeast = observed
      } -> Left $ LengthSpinePairSMTLibProtocolPlanFingerprintByteLimitExceeded
        admitted observed
  Right fingerprint -> Right fingerprint
 where
  maximumBytes = lengthSpinePairSMTLibProtocolPlanFingerprintByteLimit limits
  stream = lengthSpinePairSMTLibProtocolStreamLimits limits
  cumulative = lengthSpinePairSMTLibProtocolCumulativeStdoutByteLimit limits

tagged :: String -> [FingerprintField] -> FingerprintField
tagged name = FingerprintTag $ ascii name

ascii :: String -> [Word8]
ascii = map $ fromIntegral . fromEnum

space, openParen, closeParen, digitZero :: Word8
space = 32
openParen = 40
closeParen = 41
digitZero = 48
