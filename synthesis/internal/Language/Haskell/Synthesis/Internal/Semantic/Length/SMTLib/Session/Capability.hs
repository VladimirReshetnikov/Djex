{-# LANGUAGE BangPatterns #-}
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
  ( lengthSMTLibExecutionProtocolSchemaTag
  , lengthSMTLibExecutionQueryResetBytes
  , lengthSMTLibExecutionStartupCommandBytes
  )
import Language.Haskell.Synthesis.Internal.SMTLib.Causal
  ( SMTLibCausalAction (..) )
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
  , smtLibStreamFramerTotalByteLimit
  , smtLibStreamFramingSchemaTag
  , smtLibStreamNestingDepthLimit
  , smtLibStreamTotalByteLimit
  , startSMTLibStreamFramer
  )

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

data LengthSMTLibCapabilityLimits = LengthSMTLibCapabilityLimits
  !SMTLibStreamLimits !Natural !Natural
  deriving (Eq, Ord)

instance NFData LengthSMTLibCapabilityLimits where
  rnf (LengthSMTLibCapabilityLimits stream cumulative fingerprint) =
    rnf stream `seq` rnf cumulative `seq` rnf fingerprint

mkLengthSMTLibCapabilityLimits
  :: LengthSMTLibCapabilityLimitSource
  -> LengthSMTLibCapabilityLimits
mkLengthSMTLibCapabilityLimits source = LengthSMTLibCapabilityLimits
  (mkSMTLibStreamLimits
    $ lengthSMTLibCapabilityLimitSourceStreamLimits source)
  (lengthSMTLibCapabilityLimitSourceCumulativeOutputBytes source)
  (lengthSMTLibCapabilityLimitSourcePlanFingerprintBytes source)

defaultLengthSMTLibCapabilityLimitSource
  :: LengthSMTLibCapabilityLimitSource
defaultLengthSMTLibCapabilityLimitSource = LengthSMTLibCapabilityLimitSource
  { lengthSMTLibCapabilityLimitSourceStreamLimits =
      defaultSMTLibStreamLimitSource
  , lengthSMTLibCapabilityLimitSourceCumulativeOutputBytes = 524288
  , lengthSMTLibCapabilityLimitSourcePlanFingerprintBytes = 262144
  }

defaultLengthSMTLibCapabilityLimits :: LengthSMTLibCapabilityLimits
defaultLengthSMTLibCapabilityLimits =
  mkLengthSMTLibCapabilityLimits defaultLengthSMTLibCapabilityLimitSource

lengthSMTLibCapabilityStreamLimits
  :: LengthSMTLibCapabilityLimits
  -> SMTLibStreamLimits
lengthSMTLibCapabilityStreamLimits
    (LengthSMTLibCapabilityLimits value _ _) = value

lengthSMTLibCapabilityCumulativeOutputByteLimit
  :: LengthSMTLibCapabilityLimits
  -> Natural
lengthSMTLibCapabilityCumulativeOutputByteLimit
    (LengthSMTLibCapabilityLimits _ value _) = value

lengthSMTLibCapabilityPlanFingerprintByteLimit
  :: LengthSMTLibCapabilityLimits
  -> Natural
lengthSMTLibCapabilityPlanFingerprintByteLimit
    (LengthSMTLibCapabilityLimits _ _ value) = value

data LengthSMTLibCapabilityBarrier
  = LengthSMTLibCapabilityStartupBarrier
  | LengthSMTLibCapabilityCheckBarrier
  | LengthSMTLibCapabilityInputValueBarrier
  | LengthSMTLibCapabilityReadyBarrier
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibCapabilityBarrier

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

-- | Opaque association of framing policy, four positional barriers, their
-- deterministically rendered exact writes, and a complete reversible plan key.
data LengthSMTLibCapabilityPlan identity = LengthSMTLibCapabilityPlan
  !SMTLibStreamLimits
  !Natural
  !SMTLibEchoSentinel
  !SMTLibEchoSentinel
  !SMTLibEchoSentinel
  !SMTLibEchoSentinel
  !(Fingerprint LengthSMTLibCapabilityPlanFingerprintSubject)

type role LengthSMTLibCapabilityPlan nominal

instance NFData (LengthSMTLibCapabilityPlan identity) where
  rnf (LengthSMTLibCapabilityPlan stream cumulative startup check value ready
      fingerprint) =
    rnf stream `seq` rnf cumulative `seq`
    rnf startup `seq` rnf check `seq` rnf value `seq` rnf ready `seq`
    rnf fingerprint

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
    (lengthSMTLibCapabilityStreamLimits limits)
    (lengthSMTLibCapabilityCumulativeOutputByteLimit limits)
    startup check value ready fingerprint
 where
  makeBarrier site = first (LengthSMTLibCapabilityBarrierNonceError site)
    . mkSMTLibEchoSentinel

renderCapabilityStartupWrite :: SMTLibEchoSentinel -> [Word8]
renderCapabilityStartupWrite startup =
  lengthSMTLibExecutionStartupCommandBytes ++
  smtLibEchoSentinelCommandBytes startup

renderCapabilityCheckWrite :: SMTLibEchoSentinel -> [Word8]
renderCapabilityCheckWrite check =
  lengthSMTLibExecutionQueryResetBytes ++
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
  lengthSMTLibExecutionQueryResetBytes ++
  capabilityCanonicalPreambleBytes ++
  capabilityDeclarationBytes ++
  capabilityAssertZeroBytes ++
  capabilityAssertOneBytes ++
  capabilityCheckSatisfiableBytes ++
  smtLibEchoSentinelCommandBytes ready

lengthSMTLibCapabilityStartupWriteBytes
  :: LengthSMTLibCapabilityPlan identity
  -> [Word8]
lengthSMTLibCapabilityStartupWriteBytes =
  renderCapabilityStartupWrite . planStartupBarrier

lengthSMTLibCapabilityCheckWriteBytes
  :: LengthSMTLibCapabilityPlan identity
  -> [Word8]
lengthSMTLibCapabilityCheckWriteBytes =
  renderCapabilityCheckWrite . planCheckBarrier

lengthSMTLibCapabilityInputValueWriteBytes
  :: LengthSMTLibCapabilityPlan identity
  -> [Word8]
lengthSMTLibCapabilityInputValueWriteBytes =
  renderCapabilityInputValueWrite . planValueBarrier

lengthSMTLibCapabilityReadyWriteBytes
  :: LengthSMTLibCapabilityPlan identity
  -> [Word8]
lengthSMTLibCapabilityReadyWriteBytes =
  renderCapabilityReadyWrite . planReadyBarrier

lengthSMTLibCapabilityPlanFingerprint
  :: LengthSMTLibCapabilityPlan identity
  -> Fingerprint LengthSMTLibCapabilityPlanFingerprintSubject
lengthSMTLibCapabilityPlanFingerprint
    (LengthSMTLibCapabilityPlan _ _ _ _ _ _ value) = value

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

data LengthSMTLibCapabilityReceiver identity =
  LengthSMTLibCapabilityReceiver
    !(LengthSMTLibCapabilityPlan identity)
    !LengthSMTLibCapabilityPhaseState
    !Natural
    !SMTLibStreamFramer

type role LengthSMTLibCapabilityReceiver nominal

instance NFData (LengthSMTLibCapabilityReceiver identity) where
  rnf (LengthSMTLibCapabilityReceiver plan phase frameStart framer) =
    rnf plan `seq` rnf phase `seq` rnf frameStart `seq` rnf framer

lengthSMTLibCapabilityReceiverPhase
  :: LengthSMTLibCapabilityReceiver identity
  -> LengthSMTLibCapabilityPhase
lengthSMTLibCapabilityReceiverPhase
    (LengthSMTLibCapabilityReceiver _ phase _ _) = phaseName phase

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
  (newReceiver plan AwaitLengthSMTLibCapabilityStartupBarrier 0)

feedLengthSMTLibCapability
  :: LengthSMTLibCapabilityReceiver identity
  -> [Word8]
  -> Either
      LengthSMTLibCapabilityError
      (LengthSMTLibCapabilityAction identity)
feedLengthSMTLibCapability receiver bytes = case
    feedSMTLibStreamFramer (receiverFramer receiver) bytes of
  Left failure -> Left $ classifyFramingFailure receiver failure
  Right (SMTLibStreamFramingPending next) ->
    Right $ SMTLibCausalAwait
      $ replaceReceiverFramer receiver next
  Right (SMTLibStreamFramingComplete frame tailBytes consumed) ->
    handleFrame receiver
      (receiverFrameStart receiver + consumed) frame tailBytes

-- | EOF never completes readiness for a reusable worker.
finishLengthSMTLibCapability
  :: LengthSMTLibCapabilityReceiver identity
  -> Either
      LengthSMTLibCapabilityError
      (LengthSMTLibCapabilityAction identity)
finishLengthSMTLibCapability receiver = case
    finishSMTLibStreamFramer $ receiverFramer receiver of
  Left failure -> Left $ classifyFramingFailure receiver failure
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

lengthSMTLibCapabilityOutcomePlanFingerprint
  :: LengthSMTLibCapabilityOutcome identity
  -> Fingerprint LengthSMTLibCapabilityPlanFingerprintSubject
lengthSMTLibCapabilityOutcomePlanFingerprint
    (LengthSMTLibCapabilityOutcome fingerprint) =
  fingerprint

handleFrame
  :: LengthSMTLibCapabilityReceiver identity
  -> Natural
  -> [Word8]
  -> [Word8]
  -> Either
      LengthSMTLibCapabilityError
      (LengthSMTLibCapabilityAction identity)
handleFrame receiver consumed frame tailBytes = case receiverPhase receiver of
  AwaitLengthSMTLibCapabilityStartupBarrier ->
    crossBarrier
      LengthSMTLibCapabilityStartupBarrier
      (planStartupBarrier plan)
      (SMTLibCausalWrite
        LengthSMTLibCapabilityCheckWrite
        (lengthSMTLibCapabilityCheckWriteBytes plan)
        . newReceiver plan AwaitLengthSMTLibCapabilityCheckStatus)
  AwaitLengthSMTLibCapabilityCheckStatus ->
    if frame == capabilitySatisfiableResponseBytes
      then continueWithinWrite plan
        AwaitLengthSMTLibCapabilityCheckBarrier consumed tailBytes
      else Left $ LengthSMTLibCapabilityUnexpectedExactResponse
        LengthSMTLibCapabilityCheckStatusPhase
  AwaitLengthSMTLibCapabilityCheckBarrier ->
    crossBarrier
      LengthSMTLibCapabilityCheckBarrier
      (planCheckBarrier plan)
      (SMTLibCausalWrite
        LengthSMTLibCapabilityInputValueWrite
        (lengthSMTLibCapabilityInputValueWriteBytes plan)
        . newReceiver plan AwaitLengthSMTLibCapabilityInputValue)
  AwaitLengthSMTLibCapabilityInputValue ->
    if frame == capabilityInputValueResponseBytes
      then continueWithinWrite plan
        AwaitLengthSMTLibCapabilityInputValueBarrier consumed tailBytes
      else Left $ LengthSMTLibCapabilityUnexpectedExactResponse
        LengthSMTLibCapabilityInputValuePhase
  AwaitLengthSMTLibCapabilityInputValueBarrier ->
    crossBarrier
      LengthSMTLibCapabilityInputValueBarrier
      (planValueBarrier plan)
      (SMTLibCausalWrite
        LengthSMTLibCapabilityReadyWrite
        (lengthSMTLibCapabilityReadyWriteBytes plan)
        . newReceiver plan AwaitLengthSMTLibCapabilityReadyStatus)
  AwaitLengthSMTLibCapabilityReadyStatus ->
    if frame == capabilityUnsatisfiableResponseBytes
      then continueWithinWrite plan
        AwaitLengthSMTLibCapabilityReadyBarrier consumed tailBytes
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
  plan = receiverPlan receiver
  crossBarrier site barrier next
    | isExactSMTLibEchoSentinelResponse barrier frame = do
        afterBoundary <- consumePostBarrierWhitespace plan consumed tailBytes
        Right $ next afterBoundary
    | otherwise = Left $ LengthSMTLibCapabilityBarrierMismatch site

continueWithinWrite
  :: LengthSMTLibCapabilityPlan identity
  -> LengthSMTLibCapabilityPhaseState
  -> Natural
  -> [Word8]
  -> Either
      LengthSMTLibCapabilityError
      (LengthSMTLibCapabilityAction identity)
continueWithinWrite plan phase consumed tailBytes =
  feedLengthSMTLibCapability (newReceiver plan phase consumed) tailBytes

consumePostBarrierWhitespace
  :: LengthSMTLibCapabilityPlan identity
  -> Natural
  -> [Word8]
  -> Either LengthSMTLibCapabilityError Natural
consumePostBarrierWhitespace plan = go
 where
  maximumBytes = planCumulativeOutputByteLimit plan

  go !consumed [] = Right consumed
  go !consumed (byte : bytes)
    | consumed >= maximumBytes = Left $
        LengthSMTLibCapabilityCumulativeOutputByteLimitExceeded
          maximumBytes (maximumBytes + 1)
    | isSMTLibWhitespace byte = go (consumed + 1) bytes
    | otherwise = Left $
        LengthSMTLibCapabilityUnexpectedPostBarrierByte consumed byte

newReceiver
  :: LengthSMTLibCapabilityPlan identity
  -> LengthSMTLibCapabilityPhaseState
  -> Natural
  -> LengthSMTLibCapabilityReceiver identity
newReceiver plan phase frameStart = LengthSMTLibCapabilityReceiver
  plan phase frameStart $ startSMTLibStreamFramer effectiveLimits
 where
  configured = planStreamLimits plan
  cumulativeMaximum = planCumulativeOutputByteLimit plan
  remaining
    | frameStart >= cumulativeMaximum = 0
    | otherwise = cumulativeMaximum - frameStart
  configuredTotal = smtLibStreamTotalByteLimit configured
  effectiveLimits = mkSMTLibStreamLimits SMTLibStreamLimitSource
    { smtLibStreamLimitSourceTotalBytes = min configuredTotal remaining
    , smtLibStreamLimitSourceFrameBytes =
        smtLibStreamFrameByteLimit configured
    , smtLibStreamLimitSourceNestingDepth =
        smtLibStreamNestingDepthLimit configured
    }

classifyFramingFailure
  :: LengthSMTLibCapabilityReceiver identity
  -> SMTLibStreamFramingError
  -> LengthSMTLibCapabilityError
classifyFramingFailure receiver failure = case failure of
  SMTLibStreamTotalByteLimitExceeded _ _
    | receiverCumulativeCapsFrame receiver ->
        LengthSMTLibCapabilityCumulativeOutputByteLimitExceeded
          cumulativeMaximum (cumulativeMaximum + 1)
  _ -> LengthSMTLibCapabilityFramingFailure
    (lengthSMTLibCapabilityReceiverPhase receiver) failure
 where
  cumulativeMaximum = planCumulativeOutputByteLimit $ receiverPlan receiver

receiverPlan
  :: LengthSMTLibCapabilityReceiver identity
  -> LengthSMTLibCapabilityPlan identity
receiverPlan (LengthSMTLibCapabilityReceiver value _ _ _) = value

receiverPhase
  :: LengthSMTLibCapabilityReceiver identity
  -> LengthSMTLibCapabilityPhaseState
receiverPhase (LengthSMTLibCapabilityReceiver _ value _ _) = value

receiverFrameStart
  :: LengthSMTLibCapabilityReceiver identity
  -> Natural
receiverFrameStart (LengthSMTLibCapabilityReceiver _ _ value _) = value

-- The configured frame-total error wins an exact tie.  Cumulative failure is
-- selected only when the remaining transaction budget is strictly lower.
receiverCumulativeCapsFrame
  :: LengthSMTLibCapabilityReceiver identity
  -> Bool
receiverCumulativeCapsFrame receiver =
  smtLibStreamFramerTotalByteLimit (receiverFramer receiver) < configuredTotal
 where
  plan = receiverPlan receiver
  configuredTotal = smtLibStreamTotalByteLimit $ planStreamLimits plan

receiverFramer
  :: LengthSMTLibCapabilityReceiver identity
  -> SMTLibStreamFramer
receiverFramer (LengthSMTLibCapabilityReceiver _ _ _ value) = value

replaceReceiverFramer
  :: LengthSMTLibCapabilityReceiver identity
  -> SMTLibStreamFramer
  -> LengthSMTLibCapabilityReceiver identity
replaceReceiverFramer
    (LengthSMTLibCapabilityReceiver plan phase start _) framer =
  LengthSMTLibCapabilityReceiver plan phase start framer

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

planStreamLimits
  :: LengthSMTLibCapabilityPlan identity
  -> SMTLibStreamLimits
planStreamLimits
    (LengthSMTLibCapabilityPlan value _ _ _ _ _ _) = value

planCumulativeOutputByteLimit
  :: LengthSMTLibCapabilityPlan identity
  -> Natural
planCumulativeOutputByteLimit
    (LengthSMTLibCapabilityPlan _ value _ _ _ _ _) = value

planStartupBarrier
  :: LengthSMTLibCapabilityPlan identity
  -> SMTLibEchoSentinel
planStartupBarrier
    (LengthSMTLibCapabilityPlan _ _ value _ _ _ _) = value

planCheckBarrier
  :: LengthSMTLibCapabilityPlan identity
  -> SMTLibEchoSentinel
planCheckBarrier
    (LengthSMTLibCapabilityPlan _ _ _ value _ _ _) = value

planValueBarrier
  :: LengthSMTLibCapabilityPlan identity
  -> SMTLibEchoSentinel
planValueBarrier
    (LengthSMTLibCapabilityPlan _ _ _ _ value _ _) = value

planReadyBarrier
  :: LengthSMTLibCapabilityPlan identity
  -> SMTLibEchoSentinel
planReadyBarrier
    (LengthSMTLibCapabilityPlan _ _ _ _ _ value _) = value

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
                [ FingerprintBytes lengthSMTLibExecutionStartupCommandBytes
                , FingerprintBytes $
                    smtLibEchoSentinelCommandBytes startup
                , FingerprintBytes startupWrite
                ]
            , tagged "check"
                [ FingerprintBytes lengthSMTLibExecutionQueryResetBytes
                , FingerprintBytes checkWrite
                ]
            , tagged "input-value" [FingerprintBytes valueWrite]
            , tagged "ready"
                [ FingerprintBytes lengthSMTLibExecutionQueryResetBytes
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
capabilitySatisfiableResponseBytes = ascii "sat"

capabilityInputValueResponseBytes :: [Word8]
capabilityInputValueResponseBytes =
  ascii "((djex_capability_input 0))"

capabilityUnsatisfiableResponseBytes :: [Word8]
capabilityUnsatisfiableResponseBytes = ascii "unsat"

tagged :: String -> [FingerprintField] -> FingerprintField
tagged name = FingerprintTag $ ascii name

isSMTLibWhitespace :: Word8 -> Bool
isSMTLibWhitespace byte = byte == horizontalTab || byte == lineFeed ||
  byte == carriageReturn || byte == space

smtLibWhitespaceBytes :: [Word8]
smtLibWhitespaceBytes = [horizontalTab, lineFeed, carriageReturn, space]

ascii :: String -> [Word8]
ascii = map $ fromIntegral . fromEnum

horizontalTab, lineFeed, carriageReturn, space :: Word8
horizontalTab = 9
lineFeed = 10
carriageReturn = 13
space = 32
