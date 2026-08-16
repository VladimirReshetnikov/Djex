{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Package-private, pure readiness handshake for one Length/Z3 worker.
--
-- The four writes in this module are deliberately separated by positional
-- echo barriers.  Bytes following a barrier may cross into another write only
-- when they are bounded SMT-LIB whitespace.  Tails are consumed recursively
-- only between responses to commands which were already written together.
-- Consequently neither a pre-emitted model nor a pre-emitted readiness answer
-- can satisfy a later causal phase.
--
-- A completed value says only that caller-fed bytes matched this capability
-- plan.  The composing live session generates distinct nonces, performs every
-- returned write completely before feeding its receiver, imposes deadlines,
-- and poisons its worker after every 'Left'.
module Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session.Capability
  ( lengthSMTLibCapabilityPlanSchemaTag
  , lengthSMTLibCapabilityPhaseMachineSchemaTag
  , lengthSMTLibCapabilityPostBarrierSchemaTag
  , lengthSMTLibCapabilityExactResponseSchemaTag
  , LengthSMTLibCapabilityLimitSource (..)
  , LengthSMTLibCapabilityLimits
  , mkLengthSMTLibCapabilityLimits
  , defaultLengthSMTLibCapabilityLimitSource
  , defaultLengthSMTLibCapabilityLimits
  , lengthSMTLibCapabilityStreamLimits
  , lengthSMTLibCapabilityCumulativeOutputByteLimit
  , lengthSMTLibCapabilityPlanFingerprintByteLimit
  , lengthSMTLibCapabilityMinimumOutputByteCount
  , LengthSMTLibCapabilityBarrier (..)
  , LengthSMTLibCapabilityRequiredFrame (..)
  , LengthSMTLibCapabilityRequiredLimit (..)
  , LengthSMTLibCapabilityPlanError (..)
  , LengthSMTLibCapabilityPlan
  , LengthSMTLibCapabilityPlanFingerprintSubject
  , sealLengthSMTLibCapabilityPlan
  , lengthSMTLibCapabilityStartupWriteBytes
  , lengthSMTLibCapabilityCheckWriteBytes
  , lengthSMTLibCapabilityInputValueWriteBytes
  , lengthSMTLibCapabilityReadyWriteBytes
  , lengthSMTLibCapabilityPlanFingerprint
  , lengthSMTLibCapabilityPlanCumulativeOutputByteLimit
  , LengthSMTLibCapabilityPhase (..)
  , LengthSMTLibCapabilityWriteKind (..)
  , LengthSMTLibCapabilityReceiver
  , lengthSMTLibCapabilityReceiverPhase
  , LengthSMTLibCapabilityAction
  , startLengthSMTLibCapability
  , feedLengthSMTLibCapability
  , finishLengthSMTLibCapability
  , LengthSMTLibCapabilityError (..)
  , LengthSMTLibCapabilityOutcome
  , lengthSMTLibCapabilityOutcomePlanFingerprint
  ) where

import Control.DeepSeq (NFData (rnf))
import Data.Bifunctor (first)
import Data.List (genericLength)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Internal.Fingerprint
  ( Fingerprint
  , FingerprintBuilder (..)
  , FingerprintField (..)
  , FingerprintLimitError (..)
  , buildFingerprintWithin
  )
import Language.Haskell.Synthesis.Semantic.Length.SMTLib
  ( lengthSMTLibQueryLogic
  , lengthSMTLibQuerySchemaTag
  )
import Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Execution
  ( lengthSMTLibExecutionProtocolSchemaTag )
import Language.Haskell.Synthesis.Internal.SMTLib.Causal
  ( SMTLibCausalAction (..) )
import Language.Haskell.Synthesis.Internal.SMTLib.Response.Standard
  ( smtLibSolverStatusResponseBytes )
import Language.Haskell.Synthesis.Internal.SMTLib.Z3.Execution
  ( z3SMTLibExecutionQueryResetBytes
  , z3SMTLibExecutionStartupCommandBytes
  )
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
import Language.Haskell.Synthesis.Semantic.Observation
  ( SolverStatus (..) )

-- | Complete pure capability-plan schema.  It identifies neither a process
-- nor an executable image.
lengthSMTLibCapabilityPlanSchemaTag :: [Word8]
lengthSMTLibCapabilityPlanSchemaTag =
  ascii "djex-length-z3-smtlib2-capability-plan/v1"

-- | Exact ordered write, response, and barrier state machine.
lengthSMTLibCapabilityPhaseMachineSchemaTag :: [Word8]
lengthSMTLibCapabilityPhaseMachineSchemaTag =
  ascii "djex-length-z3-smtlib2-capability-phase-machine/v1"

-- | Trivia crossing a causal write boundary is restricted to the four
-- SMT-LIB whitespace bytes.  Comments are not transport trivia here.
lengthSMTLibCapabilityPostBarrierSchemaTag :: [Word8]
lengthSMTLibCapabilityPostBarrierSchemaTag =
  ascii "djex-smtlib2-capability-post-barrier-whitespace/v1"

-- | Capability response frames use byte-exact comparison after framing.
lengthSMTLibCapabilityExactResponseSchemaTag :: [Word8]
lengthSMTLibCapabilityExactResponseSchemaTag =
  ascii "djex-length-z3-capability-exact-responses/v1"

-- | Raw admission and transcript bounds.  Stream limits reset for each
-- expected response frame.  The cumulative limit charges every framed byte
-- and every accepted post-barrier whitespace byte.  The fingerprint limit is
-- admission-only and does not change plan identity.
data LengthSMTLibCapabilityLimitSource = LengthSMTLibCapabilityLimitSource
  { lengthSMTLibCapabilityLimitSourceStreamLimits
      :: SMTLibStreamLimitSource
  , lengthSMTLibCapabilityLimitSourceCumulativeOutputBytes :: Natural
  , lengthSMTLibCapabilityLimitSourcePlanFingerprintBytes :: Natural
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibCapabilityLimitSource

-- | Retained capability bounds: the causal stream policy (per-frame stream
-- limits plus the cumulative transcript maximum) which every sealed plan
-- copies, and the plan-fingerprint byte limit which is consulted only while
-- sealing.
data LengthSMTLibCapabilityLimits = LengthSMTLibCapabilityLimits
  !SMTLibCausalStreamPolicy !Natural
  deriving (Eq, Ord)

instance NFData LengthSMTLibCapabilityLimits where
  rnf (LengthSMTLibCapabilityLimits streamPolicy fingerprint) =
    rnf streamPolicy `seq` rnf fingerprint

-- | Retain a limit source.  This is total: the stream limits are converted
-- as given and no minimum is enforced here; whether they can frame every
-- required response is checked by 'sealLengthSMTLibCapabilityPlan'.
mkLengthSMTLibCapabilityLimits
  :: LengthSMTLibCapabilityLimitSource
  -> LengthSMTLibCapabilityLimits
mkLengthSMTLibCapabilityLimits source = LengthSMTLibCapabilityLimits
  (mkSMTLibCausalStreamPolicy
    (mkSMTLibStreamLimits
      $ lengthSMTLibCapabilityLimitSourceStreamLimits source)
    (lengthSMTLibCapabilityLimitSourceCumulativeOutputBytes source))
  (lengthSMTLibCapabilityLimitSourcePlanFingerprintBytes source)

-- | The default stream limits with a 512 KiB cumulative transcript maximum
-- and a 256 KiB plan-fingerprint admission limit.
defaultLengthSMTLibCapabilityLimitSource
  :: LengthSMTLibCapabilityLimitSource
defaultLengthSMTLibCapabilityLimitSource = LengthSMTLibCapabilityLimitSource
  { lengthSMTLibCapabilityLimitSourceStreamLimits =
      defaultSMTLibStreamLimitSource
  , lengthSMTLibCapabilityLimitSourceCumulativeOutputBytes = 524288
  , lengthSMTLibCapabilityLimitSourcePlanFingerprintBytes = 262144
  }

-- | 'defaultLengthSMTLibCapabilityLimitSource' retained.
defaultLengthSMTLibCapabilityLimits :: LengthSMTLibCapabilityLimits
defaultLengthSMTLibCapabilityLimits =
  mkLengthSMTLibCapabilityLimits defaultLengthSMTLibCapabilityLimitSource

-- | The per-frame stream limits (total, frame, and nesting-depth bounds)
-- applied afresh to each expected response frame.
lengthSMTLibCapabilityStreamLimits
  :: LengthSMTLibCapabilityLimits
  -> SMTLibStreamLimits
lengthSMTLibCapabilityStreamLimits
    (LengthSMTLibCapabilityLimits streamPolicy _) =
      smtLibCausalStreamPolicyStreamLimits streamPolicy

-- | The maximum number of stdout bytes charged across the whole readiness
-- transcript, counting every framed byte and every accepted post-barrier
-- whitespace byte.  Sealing rejects a value below
-- 'lengthSMTLibCapabilityMinimumOutputByteCount'.
lengthSMTLibCapabilityCumulativeOutputByteLimit
  :: LengthSMTLibCapabilityLimits
  -> Natural
lengthSMTLibCapabilityCumulativeOutputByteLimit
    (LengthSMTLibCapabilityLimits streamPolicy _) =
      smtLibCausalStreamPolicyCumulativeByteLimit streamPolicy

-- | The maximum encoded size admitted for the plan fingerprint while
-- sealing.  It bounds admission only and is not itself part of the plan
-- identity.
lengthSMTLibCapabilityPlanFingerprintByteLimit
  :: LengthSMTLibCapabilityLimits
  -> Natural
lengthSMTLibCapabilityPlanFingerprintByteLimit
    (LengthSMTLibCapabilityLimits _ value) = value

-- | The four positional echo barriers of the handshake, one per write, in
-- transcript order.  Each is carried by its own distinct nonce.
data LengthSMTLibCapabilityBarrier
  = LengthSMTLibCapabilityStartupBarrier
  | LengthSMTLibCapabilityCheckBarrier
  | LengthSMTLibCapabilityInputValueBarrier
  | LengthSMTLibCapabilityReadyBarrier
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibCapabilityBarrier

-- | The seven response frames the fixed machine must be able to receive, in
-- transcript order.  Sealing checks each against the stream limits and
-- names the first frame which they cannot accommodate.
data LengthSMTLibCapabilityRequiredFrame
  = LengthSMTLibCapabilityStartupBarrierFrame
  | LengthSMTLibCapabilityCheckStatusFrame
  | LengthSMTLibCapabilityCheckBarrierFrame
  | LengthSMTLibCapabilityInputValueFrame
  | LengthSMTLibCapabilityInputValueBarrierFrame
  | LengthSMTLibCapabilityReadyStatusFrame
  | LengthSMTLibCapabilityReadyBarrierFrame
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibCapabilityRequiredFrame

-- | Which stream limit was too small for a required frame: the per-frame
-- total byte, frame byte, or nesting-depth bound.
data LengthSMTLibCapabilityRequiredLimit
  = LengthSMTLibCapabilityStreamTotalBytes
  | LengthSMTLibCapabilityStreamFrameBytes
  | LengthSMTLibCapabilityStreamNestingDepth
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibCapabilityRequiredLimit

-- | Fixed-precedence sealing failures.  No constructor retains nonce or
-- sentinel material.
data LengthSMTLibCapabilityPlanError
  = LengthSMTLibCapabilityRequiredLimitTooSmall
      !LengthSMTLibCapabilityRequiredFrame
      !LengthSMTLibCapabilityRequiredLimit
      !Natural
      !Natural
  | LengthSMTLibCapabilityMinimumOutputByteLimitExceeded !Natural !Natural
  | LengthSMTLibCapabilityBarrierNonceError
      !LengthSMTLibCapabilityBarrier !SMTLibEchoSentinelError
  | LengthSMTLibCapabilityRepeatedBarrierNonce
      !LengthSMTLibCapabilityBarrier !LengthSMTLibCapabilityBarrier
  | LengthSMTLibCapabilityPlanFingerprintByteLimitExceeded !Natural !Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibCapabilityPlanError

-- | Opaque association of one cumulative framing policy, four positional
-- barriers, their deterministically rendered exact writes, and a complete
-- reversible plan key.
data LengthSMTLibCapabilityPlan identity = LengthSMTLibCapabilityPlan
  !SMTLibCausalStreamPolicy
  !SMTLibEchoSentinel
  !SMTLibEchoSentinel
  !SMTLibEchoSentinel
  !SMTLibEchoSentinel
  !(Fingerprint LengthSMTLibCapabilityPlanFingerprintSubject)

type role LengthSMTLibCapabilityPlan nominal

instance NFData (LengthSMTLibCapabilityPlan identity) where
  rnf (LengthSMTLibCapabilityPlan streamPolicy startup check value ready
      fingerprint) =
    rnf streamPolicy `seq`
    rnf startup `seq` rnf check `seq` rnf value `seq` rnf ready `seq`
    rnf fingerprint

-- | Phantom subject distinguishing capability-plan fingerprints from every
-- other 'Fingerprint' in the package.  It has no values.
data LengthSMTLibCapabilityPlanFingerprintSubject

-- | Seal the capability transaction from four exact, pairwise-distinct
-- 256-bit nonces.  Construction establishes length and distinctness, not
-- freshness or entropy; those remain live-session obligations.
sealLengthSMTLibCapabilityPlan
  :: LengthSMTLibCapabilityLimits
  -> [Word8]
  -> [Word8]
  -> [Word8]
  -> [Word8]
  -> Either
      LengthSMTLibCapabilityPlanError
      (LengthSMTLibCapabilityPlan identity)
sealLengthSMTLibCapabilityPlan limits rawStartup rawCheck rawValue rawReady = do
  validateCapabilityFraming limits
  startup <- makeBarrier LengthSMTLibCapabilityStartupBarrier rawStartup
  check <- makeBarrier LengthSMTLibCapabilityCheckBarrier rawCheck
  value <- makeBarrier LengthSMTLibCapabilityInputValueBarrier rawValue
  ready <- makeBarrier LengthSMTLibCapabilityReadyBarrier rawReady
  validateDistinctBarriers
    [ (LengthSMTLibCapabilityStartupBarrier, startup)
    , (LengthSMTLibCapabilityCheckBarrier, check)
    , (LengthSMTLibCapabilityInputValueBarrier, value)
    , (LengthSMTLibCapabilityReadyBarrier, ready)
    ]
  let startupWrite = renderCapabilityStartupWrite startup
      checkWrite = renderCapabilityCheckWrite check
      valueWrite = renderCapabilityInputValueWrite value
      readyWrite = renderCapabilityReadyWrite ready
  fingerprint <- buildCapabilityPlanFingerprint limits
    startup check value ready startupWrite checkWrite valueWrite readyWrite
  pure $ LengthSMTLibCapabilityPlan
    (limitsStreamPolicy limits)
    startup check value ready fingerprint
 where
  makeBarrier site = first (LengthSMTLibCapabilityBarrierNonceError site)
    . mkSMTLibEchoSentinel

renderCapabilityStartupWrite :: SMTLibEchoSentinel -> [Word8]
renderCapabilityStartupWrite startup =
  z3SMTLibExecutionStartupCommandBytes ++
  smtLibEchoSentinelCommandBytes startup

renderCapabilityCheckWrite :: SMTLibEchoSentinel -> [Word8]
renderCapabilityCheckWrite check =
  z3SMTLibExecutionQueryResetBytes ++
  capabilityCanonicalPreambleBytes ++
  capabilityDeclarationBytes ++
  capabilityAssertZeroBytes ++
  capabilityCheckSatisfiableBytes ++
  smtLibEchoSentinelCommandBytes check

renderCapabilityInputValueWrite :: SMTLibEchoSentinel -> [Word8]
renderCapabilityInputValueWrite value =
  capabilityInputValueRequestBytes ++
  smtLibEchoSentinelCommandBytes value

renderCapabilityReadyWrite :: SMTLibEchoSentinel -> [Word8]
renderCapabilityReadyWrite ready =
  z3SMTLibExecutionQueryResetBytes ++
  capabilityCanonicalPreambleBytes ++
  capabilityDeclarationBytes ++
  capabilityAssertZeroBytes ++
  capabilityAssertOneBytes ++
  capabilityCheckSatisfiableBytes ++
  smtLibEchoSentinelCommandBytes ready

-- | The exact bytes of the first write: the Z3 startup print-success
-- suppression followed by the startup barrier's echo command.  These are the
-- bytes 'startLengthSMTLibCapability' emits.
lengthSMTLibCapabilityStartupWriteBytes
  :: LengthSMTLibCapabilityPlan identity
  -> [Word8]
lengthSMTLibCapabilityStartupWriteBytes =
  renderCapabilityStartupWrite . planStartupBarrier

-- | The exact bytes of the second write: a query reset, the canonical Length
-- preamble, a declaration asserted equal to zero, @(check-sat)@, and the
-- check barrier's echo command.  Its expected responses are @sat@ then the
-- barrier echo.
lengthSMTLibCapabilityCheckWriteBytes
  :: LengthSMTLibCapabilityPlan identity
  -> [Word8]
lengthSMTLibCapabilityCheckWriteBytes =
  renderCapabilityCheckWrite . planCheckBarrier

-- | The exact bytes of the third write: a @get-value@ request for the
-- declared constant followed by the input-value barrier's echo command.  Its
-- expected responses are @((djex_capability_input 0))@ then the barrier
-- echo.
lengthSMTLibCapabilityInputValueWriteBytes
  :: LengthSMTLibCapabilityPlan identity
  -> [Word8]
lengthSMTLibCapabilityInputValueWriteBytes =
  renderCapabilityInputValueWrite . planValueBarrier

-- | The exact bytes of the fourth write: a query reset, the canonical
-- preamble, the declaration asserted equal to both zero and one,
-- @(check-sat)@, and the ready barrier's echo command.  Its expected
-- responses are @unsat@ then the barrier echo, which completes readiness.
lengthSMTLibCapabilityReadyWriteBytes
  :: LengthSMTLibCapabilityPlan identity
  -> [Word8]
lengthSMTLibCapabilityReadyWriteBytes =
  renderCapabilityReadyWrite . planReadyBarrier

-- | The complete plan key sealed at construction.  It binds the schema tags,
-- canonical query profile, stream and cumulative limits, phase machine, all
-- four exact writes, and all expected responses; the same key is what a
-- completed outcome retains.
lengthSMTLibCapabilityPlanFingerprint
  :: LengthSMTLibCapabilityPlan identity
  -> Fingerprint LengthSMTLibCapabilityPlanFingerprintSubject
lengthSMTLibCapabilityPlanFingerprint
    (LengthSMTLibCapabilityPlan _ _ _ _ _ value) = value

-- | Final causal transcript cap retained by this exact sealed plan.
lengthSMTLibCapabilityPlanCumulativeOutputByteLimit
  :: LengthSMTLibCapabilityPlan identity
  -> Natural
lengthSMTLibCapabilityPlanCumulativeOutputByteLimit =
  smtLibCausalStreamPolicyCumulativeByteLimit . planStreamPolicy

-- | Which of the seven expected response frames a receiver is currently
-- awaiting, in transcript order.  Errors name the phase in which they were
-- detected.
data LengthSMTLibCapabilityPhase
  = LengthSMTLibCapabilityStartupBarrierPhase
  | LengthSMTLibCapabilityCheckStatusPhase
  | LengthSMTLibCapabilityCheckBarrierPhase
  | LengthSMTLibCapabilityInputValuePhase
  | LengthSMTLibCapabilityInputValueBarrierPhase
  | LengthSMTLibCapabilityReadyStatusPhase
  | LengthSMTLibCapabilityReadyBarrierPhase
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibCapabilityPhase

-- | Label carried by each write action the machine emits, naming which of
-- the four exact plan writes the accompanying bytes are.
data LengthSMTLibCapabilityWriteKind
  = LengthSMTLibCapabilityStartupWrite
  | LengthSMTLibCapabilityCheckWrite
  | LengthSMTLibCapabilityInputValueWrite
  | LengthSMTLibCapabilityReadyWrite
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibCapabilityWriteKind

data LengthSMTLibCapabilityPhaseState
  = AwaitLengthSMTLibCapabilityStartupBarrier
  | AwaitLengthSMTLibCapabilityCheckStatus
  | AwaitLengthSMTLibCapabilityCheckBarrier
  | AwaitLengthSMTLibCapabilityInputValue
  | AwaitLengthSMTLibCapabilityInputValueBarrier
  | AwaitLengthSMTLibCapabilityReadyStatus
  | AwaitLengthSMTLibCapabilityReadyBarrier

instance NFData LengthSMTLibCapabilityPhaseState where
  rnf state = state `seq` ()

-- | The pure receiving state of the handshake between two actions: the
-- sealed plan, the phase being awaited, and the causal stream cursor
-- holding partially framed bytes.  A receiver is only obtained from an
-- action of the same plan and is advanced by 'feedLengthSMTLibCapability'.
data LengthSMTLibCapabilityReceiver identity =
  LengthSMTLibCapabilityReceiver
    !(LengthSMTLibCapabilityPlan identity)
    !LengthSMTLibCapabilityPhaseState
    !SMTLibCausalStreamCursor

type role LengthSMTLibCapabilityReceiver nominal

instance NFData (LengthSMTLibCapabilityReceiver identity) where
  rnf (LengthSMTLibCapabilityReceiver plan phase cursor) =
    rnf plan `seq` rnf phase `seq` rnf cursor

-- | The response frame this receiver is currently awaiting.
lengthSMTLibCapabilityReceiverPhase
  :: LengthSMTLibCapabilityReceiver identity
  -> LengthSMTLibCapabilityPhase
lengthSMTLibCapabilityReceiverPhase
    (LengthSMTLibCapabilityReceiver _ phase _) = phaseName phase

-- | The shared causal action specialized to this plan's nominal receiver and
-- decoded readiness outcome.  The shared type keeps all parameters nominal.
type LengthSMTLibCapabilityAction identity =
  SMTLibCausalAction
    LengthSMTLibCapabilityWriteKind
    (LengthSMTLibCapabilityReceiver identity)
    (LengthSMTLibCapabilityOutcome identity)

-- | Begin with the startup print-success suppression and its positional echo.
startLengthSMTLibCapability
  :: LengthSMTLibCapabilityPlan identity
  -> LengthSMTLibCapabilityAction identity
startLengthSMTLibCapability plan = SMTLibCausalWrite
  LengthSMTLibCapabilityStartupWrite
  (lengthSMTLibCapabilityStartupWriteBytes plan)
  (startReceiver plan AwaitLengthSMTLibCapabilityStartupBarrier)

-- | Feed one stdout chunk to the receiver.  Bytes are framed under the
-- plan's stream policy; an incomplete frame yields an await on the advanced
-- receiver, and a completed frame is compared byte-exactly against the
-- phase's expected status, value, or barrier echo, yielding the next write,
-- the next await, or the completed outcome after the ready barrier.  The
-- composing live session poisons its worker after any 'Left'.
feedLengthSMTLibCapability
  :: LengthSMTLibCapabilityReceiver identity
  -> [Word8]
  -> Either
      LengthSMTLibCapabilityError
      (LengthSMTLibCapabilityAction identity)
feedLengthSMTLibCapability receiver bytes = do
  step <- first
    (mapStreamFailure $ lengthSMTLibCapabilityReceiverPhase receiver)
    $ feedSMTLibCausalStreamCursor (receiverCursor receiver) bytes
  acceptStreamStep (receiverPlan receiver) (receiverPhase receiver) step

-- | EOF never completes readiness for a reusable worker.
finishLengthSMTLibCapability
  :: LengthSMTLibCapabilityReceiver identity
  -> Either
      LengthSMTLibCapabilityError
      (LengthSMTLibCapabilityAction identity)
finishLengthSMTLibCapability receiver = case
    finishSMTLibCausalStreamCursor $ receiverCursor receiver of
  Left failure -> Left $ mapStreamFailure
    (lengthSMTLibCapabilityReceiverPhase receiver) failure
  Right _ -> Left $ LengthSMTLibCapabilityUnexpectedEOF
    $ lengthSMTLibCapabilityReceiverPhase receiver

-- | Sanitized capability failures.  Frame mismatches expose only their phase;
-- marker errors expose only the positional barrier.
data LengthSMTLibCapabilityError
  = LengthSMTLibCapabilityFramingFailure
      !LengthSMTLibCapabilityPhase !SMTLibStreamFramingError
  | LengthSMTLibCapabilityUnexpectedExactResponse
      !LengthSMTLibCapabilityPhase
  | LengthSMTLibCapabilityBarrierMismatch !LengthSMTLibCapabilityBarrier
  | LengthSMTLibCapabilityCumulativeOutputByteLimitExceeded !Natural !Natural
  | LengthSMTLibCapabilityUnexpectedPostBarrierByte !Natural !Word8
  | LengthSMTLibCapabilityUnexpectedEOF !LengthSMTLibCapabilityPhase
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibCapabilityError

-- | Pure completion of the exact readiness transcript.  This is not process
-- evidence and retains only the complete sealed plan key, not the plan's
-- barriers, writes, limits, or caller-fed frame bytes.
data LengthSMTLibCapabilityOutcome identity =
  LengthSMTLibCapabilityOutcome
    !(Fingerprint LengthSMTLibCapabilityPlanFingerprintSubject)

type role LengthSMTLibCapabilityOutcome nominal

instance NFData (LengthSMTLibCapabilityOutcome identity) where
  rnf (LengthSMTLibCapabilityOutcome fingerprint) = rnf fingerprint

-- | The sealed plan key of the plan whose transcript this outcome completed;
-- it equals 'lengthSMTLibCapabilityPlanFingerprint' of that plan.
lengthSMTLibCapabilityOutcomePlanFingerprint
  :: LengthSMTLibCapabilityOutcome identity
  -> Fingerprint LengthSMTLibCapabilityPlanFingerprintSubject
lengthSMTLibCapabilityOutcomePlanFingerprint
    (LengthSMTLibCapabilityOutcome fingerprint) =
  fingerprint

handleFrame
  :: LengthSMTLibCapabilityPlan identity
  -> LengthSMTLibCapabilityPhaseState
  -> SMTLibCausalStreamCompletedFrame
  -> Either
      LengthSMTLibCapabilityError
      (LengthSMTLibCapabilityAction identity)
handleFrame plan phase completed = case phase of
  AwaitLengthSMTLibCapabilityStartupBarrier ->
    crossBarrier
      LengthSMTLibCapabilityStartupBarrier
      (planStartupBarrier plan)
      (\boundary -> SMTLibCausalWrite
        LengthSMTLibCapabilityCheckWrite
        (lengthSMTLibCapabilityCheckWriteBytes plan)
        $ receiverAtBoundary plan
            AwaitLengthSMTLibCapabilityCheckStatus boundary)
  AwaitLengthSMTLibCapabilityCheckStatus ->
    if frame == capabilitySatisfiableResponseBytes
      then continueWithinWrite plan
        AwaitLengthSMTLibCapabilityCheckBarrier completed
      else Left $ LengthSMTLibCapabilityUnexpectedExactResponse
        LengthSMTLibCapabilityCheckStatusPhase
  AwaitLengthSMTLibCapabilityCheckBarrier ->
    crossBarrier
      LengthSMTLibCapabilityCheckBarrier
      (planCheckBarrier plan)
      (\boundary -> SMTLibCausalWrite
        LengthSMTLibCapabilityInputValueWrite
        (lengthSMTLibCapabilityInputValueWriteBytes plan)
        $ receiverAtBoundary plan
            AwaitLengthSMTLibCapabilityInputValue boundary)
  AwaitLengthSMTLibCapabilityInputValue ->
    if frame == capabilityInputValueResponseBytes
      then continueWithinWrite plan
        AwaitLengthSMTLibCapabilityInputValueBarrier completed
      else Left $ LengthSMTLibCapabilityUnexpectedExactResponse
        LengthSMTLibCapabilityInputValuePhase
  AwaitLengthSMTLibCapabilityInputValueBarrier ->
    crossBarrier
      LengthSMTLibCapabilityInputValueBarrier
      (planValueBarrier plan)
      (\boundary -> SMTLibCausalWrite
        LengthSMTLibCapabilityReadyWrite
        (lengthSMTLibCapabilityReadyWriteBytes plan)
        $ receiverAtBoundary plan
            AwaitLengthSMTLibCapabilityReadyStatus boundary)
  AwaitLengthSMTLibCapabilityReadyStatus ->
    if frame == capabilityUnsatisfiableResponseBytes
      then continueWithinWrite plan
        AwaitLengthSMTLibCapabilityReadyBarrier completed
      else Left $ LengthSMTLibCapabilityUnexpectedExactResponse
        LengthSMTLibCapabilityReadyStatusPhase
  AwaitLengthSMTLibCapabilityReadyBarrier ->
    crossBarrier
      LengthSMTLibCapabilityReadyBarrier
      (planReadyBarrier plan)
      (const $ SMTLibCausalComplete
        $ LengthSMTLibCapabilityOutcome
        $ lengthSMTLibCapabilityPlanFingerprint plan)
 where
  frame = smtLibCausalStreamCompletedFrameBytes completed
  crossBarrier site barrier next
    | isExactSMTLibEchoSentinelResponse barrier frame = do
        boundary <- consumeBoundary (phaseName phase) completed
        Right $ next boundary
    | otherwise = Left $ LengthSMTLibCapabilityBarrierMismatch site

continueWithinWrite
  :: LengthSMTLibCapabilityPlan identity
  -> LengthSMTLibCapabilityPhaseState
  -> SMTLibCausalStreamCompletedFrame
  -> Either
      LengthSMTLibCapabilityError
      (LengthSMTLibCapabilityAction identity)
continueWithinWrite plan phase completed = do
  step <- first (mapStreamFailure $ phaseName phase)
    $ continueSMTLibCausalStreamCompletedFrame completed
  acceptStreamStep plan phase step

startReceiver
  :: LengthSMTLibCapabilityPlan identity
  -> LengthSMTLibCapabilityPhaseState
  -> LengthSMTLibCapabilityReceiver identity
startReceiver plan phase = LengthSMTLibCapabilityReceiver plan phase
  $ startSMTLibCausalStreamCursor $ planStreamPolicy plan

receiverAtBoundary
  :: LengthSMTLibCapabilityPlan identity
  -> LengthSMTLibCapabilityPhaseState
  -> SMTLibCausalStreamBoundary
  -> LengthSMTLibCapabilityReceiver identity
receiverAtBoundary plan phase boundary = LengthSMTLibCapabilityReceiver
  plan phase $ startSMTLibCausalStreamCursorAtBoundary boundary

acceptStreamStep
  :: LengthSMTLibCapabilityPlan identity
  -> LengthSMTLibCapabilityPhaseState
  -> SMTLibCausalStreamStep
  -> Either
      LengthSMTLibCapabilityError
      (LengthSMTLibCapabilityAction identity)
acceptStreamStep plan phase step = case step of
  SMTLibCausalStreamPending cursor -> Right $ SMTLibCausalAwait
    $ LengthSMTLibCapabilityReceiver plan phase cursor
  SMTLibCausalStreamComplete completed -> handleFrame plan phase completed

consumeBoundary
  :: LengthSMTLibCapabilityPhase
  -> SMTLibCausalStreamCompletedFrame
  -> Either LengthSMTLibCapabilityError SMTLibCausalStreamBoundary
consumeBoundary phase = first (mapStreamFailure phase)
  . consumeSMTLibCausalStreamBoundaryWhitespace

mapStreamFailure
  :: LengthSMTLibCapabilityPhase
  -> SMTLibCausalStreamFailure
  -> LengthSMTLibCapabilityError
mapStreamFailure phase failure = case failure of
  SMTLibCausalStreamFramingFailure framing ->
    LengthSMTLibCapabilityFramingFailure phase framing
  SMTLibCausalStreamCumulativeByteLimitExceeded maximumBytes observed ->
    LengthSMTLibCapabilityCumulativeOutputByteLimitExceeded
      maximumBytes observed
  SMTLibCausalStreamUnexpectedBoundaryByte offset byte ->
    LengthSMTLibCapabilityUnexpectedPostBarrierByte offset byte

receiverPlan
  :: LengthSMTLibCapabilityReceiver identity
  -> LengthSMTLibCapabilityPlan identity
receiverPlan (LengthSMTLibCapabilityReceiver value _ _) = value

receiverPhase
  :: LengthSMTLibCapabilityReceiver identity
  -> LengthSMTLibCapabilityPhaseState
receiverPhase (LengthSMTLibCapabilityReceiver _ value _) = value

receiverCursor
  :: LengthSMTLibCapabilityReceiver identity
  -> SMTLibCausalStreamCursor
receiverCursor (LengthSMTLibCapabilityReceiver _ _ value) = value

phaseName
  :: LengthSMTLibCapabilityPhaseState
  -> LengthSMTLibCapabilityPhase
phaseName phase = case phase of
  AwaitLengthSMTLibCapabilityStartupBarrier ->
    LengthSMTLibCapabilityStartupBarrierPhase
  AwaitLengthSMTLibCapabilityCheckStatus ->
    LengthSMTLibCapabilityCheckStatusPhase
  AwaitLengthSMTLibCapabilityCheckBarrier ->
    LengthSMTLibCapabilityCheckBarrierPhase
  AwaitLengthSMTLibCapabilityInputValue ->
    LengthSMTLibCapabilityInputValuePhase
  AwaitLengthSMTLibCapabilityInputValueBarrier ->
    LengthSMTLibCapabilityInputValueBarrierPhase
  AwaitLengthSMTLibCapabilityReadyStatus ->
    LengthSMTLibCapabilityReadyStatusPhase
  AwaitLengthSMTLibCapabilityReadyBarrier ->
    LengthSMTLibCapabilityReadyBarrierPhase

planStreamPolicy
  :: LengthSMTLibCapabilityPlan identity
  -> SMTLibCausalStreamPolicy
planStreamPolicy
    (LengthSMTLibCapabilityPlan value _ _ _ _ _) = value

limitsStreamPolicy
  :: LengthSMTLibCapabilityLimits
  -> SMTLibCausalStreamPolicy
limitsStreamPolicy (LengthSMTLibCapabilityLimits value _) = value

planStartupBarrier
  :: LengthSMTLibCapabilityPlan identity
  -> SMTLibEchoSentinel
planStartupBarrier
    (LengthSMTLibCapabilityPlan _ value _ _ _ _) = value

planCheckBarrier
  :: LengthSMTLibCapabilityPlan identity
  -> SMTLibEchoSentinel
planCheckBarrier
    (LengthSMTLibCapabilityPlan _ _ value _ _ _) = value

planValueBarrier
  :: LengthSMTLibCapabilityPlan identity
  -> SMTLibEchoSentinel
planValueBarrier
    (LengthSMTLibCapabilityPlan _ _ _ value _ _) = value

planReadyBarrier
  :: LengthSMTLibCapabilityPlan identity
  -> SMTLibEchoSentinel
planReadyBarrier
    (LengthSMTLibCapabilityPlan _ _ _ _ value _) = value

validateDistinctBarriers
  :: [(LengthSMTLibCapabilityBarrier, SMTLibEchoSentinel)]
  -> Either LengthSMTLibCapabilityPlanError ()
validateDistinctBarriers [] = Right ()
validateDistinctBarriers ((site, sentinel) : remaining) =
  case firstMatchingBarrier sentinel remaining of
    Just repeated -> Left $
      LengthSMTLibCapabilityRepeatedBarrierNonce site repeated
    Nothing -> validateDistinctBarriers remaining

firstMatchingBarrier
  :: SMTLibEchoSentinel
  -> [(LengthSMTLibCapabilityBarrier, SMTLibEchoSentinel)]
  -> Maybe LengthSMTLibCapabilityBarrier
firstMatchingBarrier _ [] = Nothing
firstMatchingBarrier sentinel ((site, candidate) : remaining)
  | sentinel == candidate = Just site
  | otherwise = firstMatchingBarrier sentinel remaining

validateCapabilityFraming
  :: LengthSMTLibCapabilityLimits
  -> Either LengthSMTLibCapabilityPlanError ()
validateCapabilityFraming limits = do
  validateFrame LengthSMTLibCapabilityStartupBarrierFrame
    fixedSentinelResponseByteCount fixedSentinelResponseByteCount 0
  validateFrame LengthSMTLibCapabilityCheckStatusFrame
    satisfiableResponseByteCount satisfiableResponseByteCount 0
  -- A bare status requires one whitespace lookahead.  Stream leaves that byte
  -- in the tail, so the following receiver charges it as leading trivia.
  validateFrame LengthSMTLibCapabilityCheckBarrierFrame
    (fixedSentinelResponseByteCount + 1) fixedSentinelResponseByteCount 0
  validateFrame LengthSMTLibCapabilityInputValueFrame
    inputValueResponseByteCount inputValueResponseByteCount 2
  -- A closed list needs no lexical separator before the following string.
  validateFrame LengthSMTLibCapabilityInputValueBarrierFrame
    fixedSentinelResponseByteCount fixedSentinelResponseByteCount 0
  validateFrame LengthSMTLibCapabilityReadyStatusFrame
    unsatisfiableResponseByteCount unsatisfiableResponseByteCount 0
  validateFrame LengthSMTLibCapabilityReadyBarrierFrame
    (fixedSentinelResponseByteCount + 1) fixedSentinelResponseByteCount 0
  let maximumBytes =
        lengthSMTLibCapabilityCumulativeOutputByteLimit limits
  if maximumBytes < minimumCapabilityOutputBytes
    then Left $ LengthSMTLibCapabilityMinimumOutputByteLimitExceeded
      maximumBytes minimumCapabilityOutputBytes
    else Right ()
 where
  stream = lengthSMTLibCapabilityStreamLimits limits

  validateFrame site totalBytes frameBytes depth = do
    validateRequiredLimit site LengthSMTLibCapabilityStreamTotalBytes
      (smtLibStreamTotalByteLimit stream) totalBytes
    validateRequiredLimit site LengthSMTLibCapabilityStreamFrameBytes
      (smtLibStreamFrameByteLimit stream) frameBytes
    validateRequiredLimit site LengthSMTLibCapabilityStreamNestingDepth
      (smtLibStreamNestingDepthLimit stream) depth

validateRequiredLimit
  :: LengthSMTLibCapabilityRequiredFrame
  -> LengthSMTLibCapabilityRequiredLimit
  -> Natural
  -> Natural
  -> Either LengthSMTLibCapabilityPlanError ()
validateRequiredLimit site field maximumValue required
  | maximumValue < required = Left $
      LengthSMTLibCapabilityRequiredLimitTooSmall
        site field maximumValue required
  | otherwise = Right ()

-- Marker strings need a following whitespace byte before a write boundary;
-- bare statuses need whitespace before their following marker.  A parenthesized
-- value can be followed immediately by its marker.  These are syntactic minima,
-- not an assumption that Z3 always chooses LF as its separator.
minimumCapabilityOutputBytes :: Natural
minimumCapabilityOutputBytes =
  4 * fixedSentinelResponseByteCount +
  satisfiableResponseByteCount +
  inputValueResponseByteCount +
  unsatisfiableResponseByteCount +
  6

-- | Smallest byte count of any transcript accepted by the fixed capability
-- machine.  A live transport must admit at least this many stdout bytes or its
-- sealed worker configuration has no successful path.
lengthSMTLibCapabilityMinimumOutputByteCount :: Natural
lengthSMTLibCapabilityMinimumOutputByteCount = minimumCapabilityOutputBytes

fixedSentinelResponseByteCount :: Natural
fixedSentinelResponseByteCount =
  2 + genericLength (ascii "djex-smtlib-frame/v1/") + 64

satisfiableResponseByteCount :: Natural
satisfiableResponseByteCount = genericLength capabilitySatisfiableResponseBytes

inputValueResponseByteCount :: Natural
inputValueResponseByteCount = genericLength capabilityInputValueResponseBytes

unsatisfiableResponseByteCount :: Natural
unsatisfiableResponseByteCount =
  genericLength capabilityUnsatisfiableResponseBytes

buildCapabilityPlanFingerprint
  :: LengthSMTLibCapabilityLimits
  -> SMTLibEchoSentinel
  -> SMTLibEchoSentinel
  -> SMTLibEchoSentinel
  -> SMTLibEchoSentinel
  -> [Word8]
  -> [Word8]
  -> [Word8]
  -> [Word8]
  -> Either
      LengthSMTLibCapabilityPlanError
      (Fingerprint LengthSMTLibCapabilityPlanFingerprintSubject)
buildCapabilityPlanFingerprint limits startup check value ready
    startupWrite checkWrite valueWrite readyWrite = case
  buildFingerprintWithin maximumBytes FingerprintBuilder
    { fingerprintBuilderVersion = 1
    , fingerprintBuilderRole = ascii
        "finite-list-spine-length/z3-smtlib2-capability-plan"
    , fingerprintBuilderFields =
        [ tagged "schema"
            [ FingerprintBytes lengthSMTLibCapabilityPlanSchemaTag
            , FingerprintBytes lengthSMTLibExecutionProtocolSchemaTag
            , FingerprintBytes lengthSMTLibQuerySchemaTag
            ]
        , tagged "canonical-query-profile"
            [ FingerprintBytes lengthSMTLibQueryLogic
            , FingerprintBytes capabilityCanonicalPreambleBytes
            ]
        , tagged "stream-framing"
            [ FingerprintBytes smtLibStreamFramingSchemaTag
            , FingerprintNatural $ smtLibStreamTotalByteLimit stream
            , FingerprintNatural $ smtLibStreamFrameByteLimit stream
            , FingerprintNatural $ smtLibStreamNestingDepthLimit stream
            ]
        , tagged "cumulative-output" [FingerprintNatural cumulative]
        , tagged "post-barrier"
            [ FingerprintBytes lengthSMTLibCapabilityPostBarrierSchemaTag
            , FingerprintBytes smtLibWhitespaceBytes
            ]
        , tagged "phase-machine"
            [ FingerprintBytes lengthSMTLibCapabilityPhaseMachineSchemaTag
            , FingerprintSequence $ map (FingerprintBytes . ascii)
                [ "write-startup-suppression-and-barrier"
                , "match-startup-barrier"
                , "write-reset-satisfiable-check-and-barrier"
                , "match-exact-satisfiable-status"
                , "match-check-barrier"
                , "write-input-value-request-and-barrier"
                , "match-exact-input-value"
                , "match-input-value-barrier"
                , "write-reset-contradictory-check-and-ready-barrier"
                , "match-exact-unsatisfiable-status"
                , "match-ready-barrier"
                ]
            ]
        , tagged "writes"
            [ tagged "startup"
                [ FingerprintBytes z3SMTLibExecutionStartupCommandBytes
                , FingerprintBytes $
                    smtLibEchoSentinelCommandBytes startup
                , FingerprintBytes startupWrite
                ]
            , tagged "check"
                [ FingerprintBytes z3SMTLibExecutionQueryResetBytes
                , FingerprintBytes checkWrite
                ]
            , tagged "input-value" [FingerprintBytes valueWrite]
            , tagged "ready"
                [ FingerprintBytes z3SMTLibExecutionQueryResetBytes
                , FingerprintBytes readyWrite
                ]
            ]
        , tagged "expected-responses"
            [ FingerprintBytes lengthSMTLibCapabilityExactResponseSchemaTag
            , tagged "startup-barrier"
                [FingerprintBytes $ smtLibEchoSentinelResponseBytes startup]
            , tagged "check-status"
                [FingerprintBytes capabilitySatisfiableResponseBytes]
            , tagged "check-barrier"
                [FingerprintBytes $ smtLibEchoSentinelResponseBytes check]
            , tagged "input-value"
                [FingerprintBytes capabilityInputValueResponseBytes]
            , tagged "input-value-barrier"
                [FingerprintBytes $ smtLibEchoSentinelResponseBytes value]
            , tagged "ready-status"
                [FingerprintBytes capabilityUnsatisfiableResponseBytes]
            , tagged "ready-barrier"
                [FingerprintBytes $ smtLibEchoSentinelResponseBytes ready]
            ]
        ]
    } of
  Left FingerprintLimitExceeded
      { fingerprintMaximumBytes = admitted
      , fingerprintObservedBytesAtLeast = observed
      } -> Left $ LengthSMTLibCapabilityPlanFingerprintByteLimitExceeded
        admitted observed
  Right fingerprint -> Right fingerprint
 where
  maximumBytes = lengthSMTLibCapabilityPlanFingerprintByteLimit limits
  stream = lengthSMTLibCapabilityStreamLimits limits
  cumulative = lengthSMTLibCapabilityCumulativeOutputByteLimit limits

-- This is a byte-for-byte rendering of the canonical fixed preamble used by
-- Length queries.  Its translator schema and logic are bound above so any
-- future canonical-profile change requires a capability schema revision.
capabilityCanonicalPreambleBytes :: [Word8]
capabilityCanonicalPreambleBytes = ascii $
  "(set-option :produce-models true)\n" ++
  "(set-option :random-seed 1)\n" ++
  "(set-logic QF_LIA)\n" ++
  "(define-fun djex_nat_monus ((x Int) (y Int)) Int " ++
    "(ite (<= y x) (- x y) 0))\n" ++
  "(define-fun djex_nat_min ((x Int) (y Int)) Int " ++
    "(ite (<= x y) x y))\n" ++
  "(define-fun djex_nat_max ((x Int) (y Int)) Int " ++
    "(ite (<= x y) y x))\n"

capabilityDeclarationBytes :: [Word8]
capabilityDeclarationBytes =
  ascii "(declare-const djex_capability_input Int)\n"

capabilityAssertZeroBytes :: [Word8]
capabilityAssertZeroBytes =
  ascii "(assert (= djex_capability_input 0))\n"

capabilityAssertOneBytes :: [Word8]
capabilityAssertOneBytes =
  ascii "(assert (= djex_capability_input 1))\n"

capabilityCheckSatisfiableBytes :: [Word8]
capabilityCheckSatisfiableBytes = ascii "(check-sat)\n"

capabilityInputValueRequestBytes :: [Word8]
capabilityInputValueRequestBytes =
  ascii "(get-value (djex_capability_input))\n"

capabilitySatisfiableResponseBytes :: [Word8]
capabilitySatisfiableResponseBytes =
  smtLibSolverStatusResponseBytes SolverSatisfiable

capabilityInputValueResponseBytes :: [Word8]
capabilityInputValueResponseBytes =
  ascii "((djex_capability_input 0))"

capabilityUnsatisfiableResponseBytes :: [Word8]
capabilityUnsatisfiableResponseBytes =
  smtLibSolverStatusResponseBytes SolverUnsatisfiable

tagged :: String -> [FingerprintField] -> FingerprintField
tagged name = FingerprintTag $ ascii name

ascii :: String -> [Word8]
ascii = map $ fromIntegral . fromEnum
