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
--
-- This module also owns the complete phase machine, framing validation, and
-- fingerprint construction shared with the nominal binary-product plan.  A
-- 'LengthSMTLibProtocolIdentity' record carries everything that
-- distinguishes one protocol domain: its schema tags, plan fingerprint role,
-- domain-specific fingerprint fields, and the exact query projections.  The
-- scalar identity lives here; the product identity and its nominally
-- distinct plan, receiver, and decoded synonyms live in the thin
-- "Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol.SpinePair"
-- wrapper.  The barrier, phase, write-kind, and error vocabularies are
-- deliberately shared: domain authority lives in the nominal plan, receiver,
-- and decoded types, which remain distinct through their query and
-- fingerprint-subject parameters.
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
  , LengthSMTLibProtocolIdentity (..)
  , LengthSMTLibProtocolPlan
  , LengthSMTLibQueryProtocolPlan
  , LengthSMTLibProtocolPlanFingerprintSubject
  , sealLengthSMTLibProtocolPlan
  , sealLengthSMTLibQueryProtocolPlan
  , lengthSMTLibProtocolInitialWriteBytes
  , lengthSMTLibProtocolInputValueWriteBytes
  , lengthSMTLibProtocolPlanFingerprint
  , lengthSMTLibProtocolPlanQuery
  , lengthSMTLibProtocolPlanArtifactPolicy
  , lengthSMTLibProtocolPlanCumulativeStdoutByteLimit
  , lengthSMTLibProtocolPlanMinimumStdoutByteCount
  , LengthSMTLibProtocolPhase (..)
  , LengthSMTLibProtocolWriteKind (..)
  , LengthSMTLibProtocolReceiver
  , LengthSMTLibQueryProtocolReceiver
  , lengthSMTLibProtocolReceiverPhase
  , LengthSMTLibProtocolAction
  , LengthSMTLibQueryProtocolAction
  , startLengthSMTLibProtocol
  , feedLengthSMTLibProtocol
  , finishLengthSMTLibProtocol
  , LengthSMTLibProtocolError (..)
  , LengthSMTLibProtocolDecoded
  , LengthSMTLibQueryProtocolDecoded
  , LengthSMTLibProtocolObservation
  , lengthSMTLibProtocolDecodedObservation
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
  , LengthSMTLibQuery
  , lengthSMTLibQueryCheckBytes
  , lengthSMTLibQueryFingerprint
  , lengthSMTLibQueryInputSymbols
  , lengthSMTLibQueryInputValueRequestBytes
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
  , parseLengthSMTLibInputValueResponse
  )
import Language.Haskell.Synthesis.Semantic.Observation
  ( SolverObservation (..)
  , SolverStatus (..)
  )

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

-- | Retained pure-protocol bounds: the causal stream policy (per-frame limits
-- plus the cumulative stdout cap) and the admission-only plan fingerprint
-- byte limit.  Constructed only through 'mkLengthSMTLibProtocolLimits'.
data LengthSMTLibProtocolLimits = LengthSMTLibProtocolLimits
  !SMTLibCausalStreamPolicy !Natural
  deriving (Eq, Ord)

instance NFData LengthSMTLibProtocolLimits where
  rnf (LengthSMTLibProtocolLimits streamPolicy fingerprint) =
    rnf streamPolicy `seq` rnf fingerprint

-- | Retain raw protocol bounds without validation: the stream limits become
-- the per-frame framing policy, the cumulative stdout bytes become the
-- transaction cap, and the fingerprint bytes are kept as the admission bound.
mkLengthSMTLibProtocolLimits
  :: LengthSMTLibProtocolLimitSource
  -> LengthSMTLibProtocolLimits
mkLengthSMTLibProtocolLimits source = LengthSMTLibProtocolLimits
  (mkSMTLibCausalStreamPolicy
    (mkSMTLibStreamLimits
      $ lengthSMTLibProtocolLimitSourceStreamLimits source)
    (lengthSMTLibProtocolLimitSourceCumulativeStdoutBytes source))
  (lengthSMTLibProtocolLimitSourcePlanFingerprintBytes source)

-- | Default raw bounds: the default per-frame stream limits, a 512 KiB
-- cumulative stdout cap, and a 256 KiB plan fingerprint bound.
defaultLengthSMTLibProtocolLimitSource :: LengthSMTLibProtocolLimitSource
defaultLengthSMTLibProtocolLimitSource = LengthSMTLibProtocolLimitSource
  { lengthSMTLibProtocolLimitSourceStreamLimits =
      defaultSMTLibStreamLimitSource
  , lengthSMTLibProtocolLimitSourceCumulativeStdoutBytes = 524288
  , lengthSMTLibProtocolLimitSourcePlanFingerprintBytes = 262144
  }

-- | 'defaultLengthSMTLibProtocolLimitSource' retained through
-- 'mkLengthSMTLibProtocolLimits'.
defaultLengthSMTLibProtocolLimits :: LengthSMTLibProtocolLimits
defaultLengthSMTLibProtocolLimits =
  mkLengthSMTLibProtocolLimits defaultLengthSMTLibProtocolLimitSource

-- | Per-frame framing limits (total bytes, retained frame bytes, nesting
-- depth) applied afresh to each expected response frame.
lengthSMTLibProtocolStreamLimits
  :: LengthSMTLibProtocolLimits
  -> SMTLibStreamLimits
lengthSMTLibProtocolStreamLimits
    (LengthSMTLibProtocolLimits streamPolicy _) =
      smtLibCausalStreamPolicyStreamLimits streamPolicy

-- | Maximum stdout bytes one whole transaction may consume across all its
-- frames and accepted post-barrier whitespace.  Plan sealing rejects a cap
-- below the smallest complete transcript the plan can admit.
lengthSMTLibProtocolCumulativeStdoutByteLimit
  :: LengthSMTLibProtocolLimits
  -> Natural
lengthSMTLibProtocolCumulativeStdoutByteLimit
    (LengthSMTLibProtocolLimits streamPolicy _) =
      smtLibCausalStreamPolicyCumulativeByteLimit streamPolicy

-- | Admission-only cap on the canonical byte size of a sealed plan
-- fingerprint; exceeding it fails sealing with
-- 'LengthSMTLibProtocolPlanFingerprintByteLimitExceeded'.  It is not itself
-- a fingerprint field.
lengthSMTLibProtocolPlanFingerprintByteLimit
  :: LengthSMTLibProtocolLimits
  -> Natural
lengthSMTLibProtocolPlanFingerprintByteLimit
    (LengthSMTLibProtocolLimits _ value) = value

-- | Which of the two echo barriers a nonce or mismatch diagnostic refers to:
-- the one following @check-sat@ or the one following @get-value@.
data LengthSMTLibProtocolBarrier
  = LengthSMTLibProtocolCheckBarrier
  | LengthSMTLibProtocolInputValueBarrier
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibProtocolBarrier

-- | Response frame whose minimal size the configured limits must admit at
-- sealing time.  The two input-value frames are checked only when the plan
-- requests values.
data LengthSMTLibProtocolRequiredFrame
  = LengthSMTLibProtocolCheckStatusFrame
  | LengthSMTLibProtocolCheckBarrierFrame
  | LengthSMTLibProtocolInputValueFrame
  | LengthSMTLibProtocolInputValueBarrierFrame
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibProtocolRequiredFrame

-- | Which stream-framing or response-parsing bound was too small for a
-- required frame; reported by 'LengthSMTLibProtocolRequiredLimitTooSmall'
-- together with the frame, the configured maximum, and the requirement.
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

-- | Everything that distinguishes one protocol domain from another: the
-- domain schema tags, the plan fingerprint role, the domain-specific
-- fingerprint fields, and the exact projections of its nominal query type.
-- The record is static configuration; its function fields never close over
-- per-plan state.  Deep evaluation forces the retained tag and field
-- constants and reduces the projection closures to head normal form.
data LengthSMTLibProtocolIdentity subject query =
  LengthSMTLibProtocolIdentity
    { protocolIdentityPlanSchemaTag :: [Word8]
    , protocolIdentityPhaseMachineSchemaTag :: [Word8]
    , protocolIdentityPlanFingerprintRole :: [Word8]
      -- | Complete domain-owned fingerprint fields spliced between the
      -- @execution-policy@ and @query@ groups; empty for the scalar plan.
    , protocolIdentityCapabilityReuseFields :: [FingerprintField]
      -- | Domain-owned prefix of the @query@ group; empty for the scalar
      -- plan.
    , protocolIdentityQueryFieldPrefix :: [FingerprintField]
    , protocolIdentityQueryCheckBytes :: query -> [Word8]
    , protocolIdentityQueryFingerprintBytes :: query -> [Word8]
    , protocolIdentityQueryInputSymbols :: query -> [[Word8]]
    , protocolIdentityQueryInputValueRequestBytes :: query -> Maybe [Word8]
    , protocolIdentityParseInputValueResponse
        :: LengthSMTLibResponseLimits
        -> query
        -> [Word8]
        -> Either LengthSMTLibResponseError [LengthSMTLibIntegerBinding]
    }

type role LengthSMTLibProtocolIdentity nominal nominal

instance NFData (LengthSMTLibProtocolIdentity subject query) where
  rnf identity =
    rnf (protocolIdentityPlanSchemaTag identity) `seq`
    rnf (protocolIdentityPhaseMachineSchemaTag identity) `seq`
    rnf (protocolIdentityPlanFingerprintRole identity) `seq`
    forceFieldSpine (protocolIdentityCapabilityReuseFields identity) `seq`
    forceFieldSpine (protocolIdentityQueryFieldPrefix identity) `seq`
    protocolIdentityQueryCheckBytes identity `seq`
    protocolIdentityQueryFingerprintBytes identity `seq`
    protocolIdentityQueryInputSymbols identity `seq`
    protocolIdentityQueryInputValueRequestBytes identity `seq`
    protocolIdentityParseInputValueResponse identity `seq` ()
   where
    forceFieldSpine = foldr seq ()

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
data LengthSMTLibQueryProtocolPlan subject query =
  LengthSMTLibQueryProtocolPlan
    !(LengthSMTLibProtocolIdentity subject query)
    !LengthSMTLibArtifactPolicy
    !LengthSMTLibResponseLimits
    !query
    !SMTLibCausalStreamPolicy
    !SMTLibEchoSentinel
    !(Maybe SMTLibEchoSentinel)
    !(Fingerprint subject)

type role LengthSMTLibQueryProtocolPlan nominal nominal

instance NFData query
    => NFData (LengthSMTLibQueryProtocolPlan subject query) where
  rnf (LengthSMTLibQueryProtocolPlan identity artifact responses query
      streamPolicy checkBarrier valueBarrier fingerprint) =
    rnf identity `seq`
    rnf artifact `seq` rnf responses `seq` rnf query `seq`
    rnf streamPolicy `seq`
    rnf checkBarrier `seq` rnf valueBarrier `seq` rnf fingerprint

-- | The scalar plan is the generic machine at the scalar query and
-- fingerprint subject.
type LengthSMTLibProtocolPlan identity local =
  LengthSMTLibQueryProtocolPlan
    LengthSMTLibProtocolPlanFingerprintSubject
    (LengthSMTLibQuery identity local)

-- | Uninhabited phantom subject of a scalar plan fingerprint.  It keeps a
-- scalar plan's fingerprint nominally distinct from the binary-product one
-- even when the two would render identical bytes.
data LengthSMTLibProtocolPlanFingerprintSubject

-- | The scalar protocol identity: the historical Length/Z3 tags, no
-- domain-owned fingerprint fields, and the scalar query projections.
scalarProtocolIdentity
  :: LengthSMTLibProtocolIdentity
      LengthSMTLibProtocolPlanFingerprintSubject
      (LengthSMTLibQuery identity local)
scalarProtocolIdentity = LengthSMTLibProtocolIdentity
  { protocolIdentityPlanSchemaTag = lengthSMTLibProtocolPlanSchemaTag
  , protocolIdentityPhaseMachineSchemaTag =
      lengthSMTLibProtocolPhaseMachineSchemaTag
  , protocolIdentityPlanFingerprintRole = ascii
      "finite-list-spine-length/z3-smtlib2-protocol-plan"
  , protocolIdentityCapabilityReuseFields = []
  , protocolIdentityQueryFieldPrefix = []
  , protocolIdentityQueryCheckBytes = lengthSMTLibQueryCheckBytes
  , protocolIdentityQueryFingerprintBytes =
      PublicFingerprint.fingerprintCanonicalBytes
        . lengthSMTLibQueryFingerprint
  , protocolIdentityQueryInputSymbols = lengthSMTLibQueryInputSymbols
  , protocolIdentityQueryInputValueRequestBytes =
      lengthSMTLibQueryInputValueRequestBytes
  , protocolIdentityParseInputValueResponse =
      parseLengthSMTLibInputValueResponse
  }

-- | Seal a pure transaction from caller-provided barrier nonce bytes.  The
-- optional value nonce is required exactly when the artifact policy requests
-- values and the query has at least one input symbol.
sealLengthSMTLibProtocolPlan
  :: LengthSMTLibProtocolLimits
  -> LengthSMTLibPostLaunchExecutionPolicy
  -> LengthSMTLibQuery identity local
  -> [Word8]
  -> Maybe [Word8]
  -> Either
      LengthSMTLibProtocolPlanError
      (LengthSMTLibProtocolPlan identity local)
sealLengthSMTLibProtocolPlan =
  sealLengthSMTLibQueryProtocolPlan scalarProtocolIdentity

-- | Generic sealing shared with the binary-product wrapper.  The identity
-- record supplies every domain-owned tag, fingerprint field, and query
-- projection; the phase machine, framing validation, and write rendering are
-- common to both domains.
sealLengthSMTLibQueryProtocolPlan
  :: LengthSMTLibProtocolIdentity subject query
  -> LengthSMTLibProtocolLimits
  -> LengthSMTLibPostLaunchExecutionPolicy
  -> query
  -> [Word8]
  -> Maybe [Word8]
  -> Either
      LengthSMTLibProtocolPlanError
      (LengthSMTLibQueryProtocolPlan subject query)
sealLengthSMTLibQueryProtocolPlan identity limits execution query
    rawCheckNonce rawValueNonce = do
  let symbols = protocolIdentityQueryInputSymbols identity query
      valueRequest =
        protocolIdentityQueryInputValueRequestBytes identity query
      requiresValues =
        lengthSMTLibPostLaunchArtifactPolicy execution ==
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
  let initialWrite = renderProtocolInitialWrite identity query checkBarrier
      valueWrite = renderProtocolInputValueWrite valueRequest valueBarrier
  fingerprint <- buildPlanFingerprint identity limits execution query
    valueRequest checkBarrier valueBarrier initialWrite valueWrite
  pure $ LengthSMTLibQueryProtocolPlan
    identity
    (lengthSMTLibPostLaunchArtifactPolicy execution)
    (lengthSMTLibPostLaunchResponseLimits execution)
    query
    (limitsStreamPolicy limits)
    checkBarrier valueBarrier fingerprint

renderProtocolInitialWrite
  :: LengthSMTLibProtocolIdentity subject query
  -> query
  -> SMTLibEchoSentinel
  -> [Word8]
renderProtocolInitialWrite identity query barrier =
  z3SMTLibExecutionQueryResetBytes ++
  protocolIdentityQueryCheckBytes identity query ++
  smtLibEchoSentinelCommandBytes barrier

renderProtocolInputValueWrite
  :: Maybe [Word8]
  -> Maybe SMTLibEchoSentinel
  -> Maybe [Word8]
renderProtocolInputValueWrite _ Nothing = Nothing
renderProtocolInputValueWrite request (Just barrier) =
  fmap (++ smtLibEchoSentinelCommandBytes barrier)
    request

-- | Exact bytes of the plan's first write: the Z3 query reset prefix, the
-- query's canonical check commands, then the status-barrier echo command.
-- Rendered on demand from the retained plan; identical bytes were bound into
-- the plan fingerprint at sealing.
lengthSMTLibProtocolInitialWriteBytes
  :: LengthSMTLibQueryProtocolPlan subject query
  -> [Word8]
lengthSMTLibProtocolInitialWriteBytes
    (LengthSMTLibQueryProtocolPlan identity _ _ query _ checkBarrier _ _) =
      renderProtocolInitialWrite identity query checkBarrier

-- | Exact bytes of the conditional second write: the query's @get-value@
-- request followed by the value-barrier echo command.  'Nothing' when the
-- plan carries no value barrier or the query has no input symbols; the write
-- is issued only after a satisfiable status and matched check barrier.
lengthSMTLibProtocolInputValueWriteBytes
  :: LengthSMTLibQueryProtocolPlan subject query
  -> Maybe [Word8]
lengthSMTLibProtocolInputValueWriteBytes
    (LengthSMTLibQueryProtocolPlan identity _ _ query _ _ valueBarrier _) =
      renderProtocolInputValueWrite
        (protocolIdentityQueryInputValueRequestBytes identity query)
        valueBarrier

-- | Complete reversible identity of the sealed plan, built at sealing from
-- the domain schema, execution-policy key, query, framing limits, cumulative
-- cap, phase machine, exact write bytes, and barrier response bytes.  Its
-- subject is the domain's fingerprint subject, so scalar and product plan
-- fingerprints do not unify.
lengthSMTLibProtocolPlanFingerprint
  :: LengthSMTLibQueryProtocolPlan subject query
  -> Fingerprint subject
lengthSMTLibProtocolPlanFingerprint
    (LengthSMTLibQueryProtocolPlan _ _ _ _ _ _ _ value) = value

-- | Exact query retained by this sealed protocol plan.
lengthSMTLibProtocolPlanQuery
  :: LengthSMTLibQueryProtocolPlan subject query
  -> query
lengthSMTLibProtocolPlanQuery = planQuery

-- | Artifact policy retained by this exact sealed protocol plan.
lengthSMTLibProtocolPlanArtifactPolicy
  :: LengthSMTLibQueryProtocolPlan subject query
  -> LengthSMTLibArtifactPolicy
lengthSMTLibProtocolPlanArtifactPolicy = planArtifactPolicy

-- | Final causal transcript cap retained by this exact sealed plan.
lengthSMTLibProtocolPlanCumulativeStdoutByteLimit
  :: LengthSMTLibQueryProtocolPlan subject query
  -> Natural
lengthSMTLibProtocolPlanCumulativeStdoutByteLimit =
  smtLibCausalStreamPolicyCumulativeByteLimit . planStreamPolicy

-- | Smallest complete live transcript admitted by this exact plan, including
-- the required lexical delimiter after each bare status and final echo.  A
-- session uses this before reserving an ordinal so the remaining process-wide
-- stdout budget can admit at least one complete branch.
lengthSMTLibProtocolPlanMinimumStdoutByteCount
  :: LengthSMTLibQueryProtocolPlan subject query
  -> Natural
lengthSMTLibProtocolPlanMinimumStdoutByteCount plan =
  minimumProtocolStdoutBytes
    (minimalInputValueFrameByteCount
      $ protocolIdentityQueryInputSymbols (planIdentity plan)
      $ planQuery plan)
    $ case planValueBarrier plan of
        Nothing -> False
        Just _ -> True

-- | Which response frame a receiver is currently awaiting, in transaction
-- order: the @check-sat@ status, its echo barrier, the @get-value@ frame,
-- then its echo barrier.  Framing, response, and EOF errors are tagged with
-- the phase in which they arose.
data LengthSMTLibProtocolPhase
  = LengthSMTLibProtocolCheckStatusPhase
  | LengthSMTLibProtocolCheckBarrierPhase
  | LengthSMTLibProtocolInputValuePhase
  | LengthSMTLibProtocolInputValueBarrierPhase
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibProtocolPhase

-- | Which of the plan's two writes a returned 'SMTLibCausalWrite' action
-- carries: the initial reset/check/barrier write, or the conditional
-- input-value write issued after a satisfiable status.
data LengthSMTLibProtocolWriteKind
  = LengthSMTLibProtocolInitialQueryWrite
  | LengthSMTLibProtocolInputValueWrite
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibProtocolWriteKind

data LengthSMTLibProtocolPhaseState
  = AwaitLengthSMTLibCheckStatus
  | AwaitLengthSMTLibCheckBarrier !SolverStatus
  | AwaitLengthSMTLibInputValues
  | AwaitLengthSMTLibInputValueBarrier [LengthSMTLibIntegerBinding]

instance NFData LengthSMTLibProtocolPhaseState where
  rnf state = case state of
    AwaitLengthSMTLibCheckStatus -> ()
    AwaitLengthSMTLibCheckBarrier status -> rnf status
    AwaitLengthSMTLibInputValues -> ()
    AwaitLengthSMTLibInputValueBarrier bindings -> rnf bindings

-- | A continuation for bytes received only after the write which produced it
-- has completed.  Constructors and raw framing state never leave the package.
data LengthSMTLibQueryProtocolReceiver subject query =
  LengthSMTLibQueryProtocolReceiver
    !(LengthSMTLibQueryProtocolPlan subject query)
    !LengthSMTLibProtocolPhaseState
    !SMTLibCausalStreamCursor

type role LengthSMTLibQueryProtocolReceiver nominal nominal

instance NFData query
    => NFData (LengthSMTLibQueryProtocolReceiver subject query) where
  rnf (LengthSMTLibQueryProtocolReceiver plan phase cursor) =
    rnf plan `seq` rnf phase `seq` rnf cursor

-- | The scalar receiver is the generic machine at the scalar query and
-- fingerprint subject.
type LengthSMTLibProtocolReceiver identity local =
  LengthSMTLibQueryProtocolReceiver
    LengthSMTLibProtocolPlanFingerprintSubject
    (LengthSMTLibQuery identity local)

-- | Name of the response frame this receiver is waiting for.  Only the phase
-- tag is exposed; the decoded status or bindings carried by the internal
-- state stay package-private.
lengthSMTLibProtocolReceiverPhase
  :: LengthSMTLibQueryProtocolReceiver subject query
  -> LengthSMTLibProtocolPhase
lengthSMTLibProtocolReceiverPhase
    (LengthSMTLibQueryProtocolReceiver _ phase _) = phaseName phase

-- | The shared causal action specialized to this plan's nominal receiver and
-- decoded outcome.  The shared type keeps all three parameters nominal.
type LengthSMTLibQueryProtocolAction subject query =
  SMTLibCausalAction
    LengthSMTLibProtocolWriteKind
    (LengthSMTLibQueryProtocolReceiver subject query)
    (LengthSMTLibQueryProtocolDecoded subject query)

-- | The scalar action synonym retained for scalar consumers.
type LengthSMTLibProtocolAction identity local =
  LengthSMTLibQueryProtocolAction
    LengthSMTLibProtocolPlanFingerprintSubject
    (LengthSMTLibQuery identity local)

-- | Start with reset, canonical check commands, and the status barrier in one
-- exact write.  Any unexpected reset response consequently occupies the
-- status slot and fails closed.
startLengthSMTLibProtocol
  :: LengthSMTLibQueryProtocolPlan subject query
  -> LengthSMTLibQueryProtocolAction subject query
startLengthSMTLibProtocol plan = SMTLibCausalWrite
  LengthSMTLibProtocolInitialQueryWrite
  (lengthSMTLibProtocolInitialWriteBytes plan)
  (startReceiver plan AwaitLengthSMTLibCheckStatus)

-- | Feed one chunk of stdout bytes received after the receiver's write.  A
-- completed frame is decoded and matched in the current phase; the stream
-- tail is consumed further only while it answers commands already written,
-- and once a barrier is accepted only bounded SMT-LIB whitespace may remain
-- before the next 'SMTLibCausalWrite' or 'SMTLibCausalComplete'.  A 'Left'
-- carries no continuation; the module header requires a live layer to poison
-- its worker after one.
feedLengthSMTLibProtocol
  :: LengthSMTLibQueryProtocolReceiver subject query
  -> [Word8]
  -> Either
      LengthSMTLibProtocolError
      (LengthSMTLibQueryProtocolAction subject query)
feedLengthSMTLibProtocol receiver bytes = do
  step <- first
    (mapStreamFailure $ lengthSMTLibProtocolReceiverPhase receiver)
    $ feedSMTLibCausalStreamCursor (receiverCursor receiver) bytes
  acceptStreamStep (receiverPlan receiver) (receiverPhase receiver) step

-- | EOF is never a successful terminal delimiter for a reusable worker.
-- Lexical EOF failures retain precedence; otherwise every phase reports its
-- exact missing response position.
finishLengthSMTLibProtocol
  :: LengthSMTLibQueryProtocolReceiver subject query
  -> Either
      LengthSMTLibProtocolError
      (LengthSMTLibQueryProtocolAction subject query)
finishLengthSMTLibProtocol receiver = case
    finishSMTLibCausalStreamCursor $ receiverCursor receiver of
  Left failure -> Left $ mapStreamFailure
    (lengthSMTLibProtocolReceiverPhase receiver) failure
  Right _ -> Left $ LengthSMTLibProtocolUnexpectedEOF
    $ lengthSMTLibProtocolReceiverPhase receiver

-- | Closed transaction failure reported by 'feedLengthSMTLibProtocol' and
-- 'finishLengthSMTLibProtocol'.  Framing, response, and EOF failures carry
-- the phase in which they arose; the cumulative-limit and post-barrier-byte
-- constructors carry the configured maximum and observed count, and the
-- zero-based offset and offending byte, respectively.  A barrier mismatch
-- names the barrier but retains neither the expected nor the observed bytes.
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

-- | The one status-indexed value admitted by pure protocol completion.
-- Satisfiable observations distinguish status-only @Nothing@, vacuous
-- zero-input @Just []@, and framed @Just (_ : _)@ values. Unsatisfiable and
-- unknown observations have no value-bearing constructor.
type LengthSMTLibProtocolObservation =
  SolverObservation (Maybe [LengthSMTLibIntegerBinding]) () ()

-- | Pure, syntactically decoded transcript outcome. A satisfiable zero-input
-- query under the input-value policy carries a vacuous @Just []@ artifact
-- without fabricating a frame or emitting an empty @get-value@ command. The
-- receiver owns the plan until completion, and the live Session carries that
-- same plan separately as the exact run-identity input; it is not copied into
-- this terminal branch. In a live run raw status and input-value frames
-- likewise remain in the process-owning causal transcript. This value remains
-- a pure response/protocol result, not an execution observation.
data LengthSMTLibQueryProtocolDecoded subject query =
  LengthSMTLibQueryProtocolDecoded
    !LengthSMTLibProtocolObservation

type role LengthSMTLibQueryProtocolDecoded nominal nominal

instance NFData (LengthSMTLibQueryProtocolDecoded subject query) where
  rnf (LengthSMTLibQueryProtocolDecoded observation) = rnf observation

-- | The scalar decoded outcome is the generic machine at the scalar query
-- and fingerprint subject.
type LengthSMTLibProtocolDecoded identity local =
  LengthSMTLibQueryProtocolDecoded
    LengthSMTLibProtocolPlanFingerprintSubject
    (LengthSMTLibQuery identity local)

-- | The status-indexed observation decoded by a completed transaction.  This
-- is a syntactic protocol result only: it carries no run identity and grants
-- no semantic authority to the status or bindings.
lengthSMTLibProtocolDecodedObservation
  :: LengthSMTLibQueryProtocolDecoded subject query
  -> LengthSMTLibProtocolObservation
lengthSMTLibProtocolDecodedObservation
    (LengthSMTLibQueryProtocolDecoded observation) = observation

-- Generic 'SolverObservation' artifacts deliberately remain lazy. This
-- trusted wrapper separately forces only the satisfiable 'Maybe' spine to
-- preserve the former strict decoded-field demand without forcing bindings.
retainLengthSMTLibProtocolDecoded
  :: LengthSMTLibProtocolObservation
  -> LengthSMTLibQueryProtocolDecoded subject query
retainLengthSMTLibProtocolDecoded observation = case observation of
  SatisfiableObservation values -> values `seq`
    LengthSMTLibQueryProtocolDecoded observation
  UnsatisfiableObservation () -> LengthSMTLibQueryProtocolDecoded observation
  UnknownObservation () -> LengthSMTLibQueryProtocolDecoded observation

handleFrame
  :: LengthSMTLibQueryProtocolPlan subject query
  -> LengthSMTLibProtocolPhaseState
  -> SMTLibCausalStreamCompletedFrame
  -> Either
      LengthSMTLibProtocolError
      (LengthSMTLibQueryProtocolAction subject query)
handleFrame plan phase completed = case phase of
  AwaitLengthSMTLibCheckStatus -> do
    let limits = planResponseLimits plan
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
                AwaitLengthSMTLibInputValues
                boundary
        _ -> do
          _ <- consumeBoundary
            LengthSMTLibProtocolCheckBarrierPhase completed
          Right $ SMTLibCausalComplete
            $ retainLengthSMTLibProtocolDecoded
            $ terminalObservation plan status
      else Left $ LengthSMTLibProtocolBarrierMismatch
        LengthSMTLibProtocolCheckBarrier
  AwaitLengthSMTLibInputValues -> do
    let limits = planResponseLimits plan
    bindings <- first
      (LengthSMTLibProtocolResponseFailure
        LengthSMTLibProtocolInputValuePhase)
      $ protocolIdentityParseInputValueResponse (planIdentity plan)
          limits (planQuery plan) frame
    continueWithinWrite plan
      (AwaitLengthSMTLibInputValueBarrier bindings)
      completed
  AwaitLengthSMTLibInputValueBarrier bindings -> do
    case planValueBarrier plan of
      Just barrier
        | isExactSMTLibEchoSentinelResponse barrier frame -> do
            _ <- consumeBoundary
              LengthSMTLibProtocolInputValueBarrierPhase completed
            Right $ SMTLibCausalComplete
              $ retainLengthSMTLibProtocolDecoded
              $ SatisfiableObservation $ Just bindings
      _ -> Left $ LengthSMTLibProtocolBarrierMismatch
        LengthSMTLibProtocolInputValueBarrier
 where
  frame = smtLibCausalStreamCompletedFrameBytes completed

continueWithinWrite
  :: LengthSMTLibQueryProtocolPlan subject query
  -> LengthSMTLibProtocolPhaseState
  -> SMTLibCausalStreamCompletedFrame
  -> Either
      LengthSMTLibProtocolError
      (LengthSMTLibQueryProtocolAction subject query)
continueWithinWrite plan phase completed = do
  step <- first (mapStreamFailure $ phaseName phase)
    $ continueSMTLibCausalStreamCompletedFrame completed
  acceptStreamStep plan phase step

terminalObservation
  :: LengthSMTLibQueryProtocolPlan subject query
  -> SolverStatus
  -> LengthSMTLibProtocolObservation
terminalObservation plan status = case status of
  SolverSatisfiable -> SatisfiableObservation
    $ if planArtifactPolicy plan ==
          LengthSMTLibInputValuesAfterSatisfiable
        && not (isJust
          $ protocolIdentityQueryInputValueRequestBytes (planIdentity plan)
          $ planQuery plan)
      then Just []
      else Nothing
  SolverUnsatisfiable -> UnsatisfiableObservation ()
  SolverUnknown -> UnknownObservation ()

startReceiver
  :: LengthSMTLibQueryProtocolPlan subject query
  -> LengthSMTLibProtocolPhaseState
  -> LengthSMTLibQueryProtocolReceiver subject query
startReceiver plan phase = LengthSMTLibQueryProtocolReceiver plan phase
  $ startSMTLibCausalStreamCursor $ planStreamPolicy plan

receiverAtBoundary
  :: LengthSMTLibQueryProtocolPlan subject query
  -> LengthSMTLibProtocolPhaseState
  -> SMTLibCausalStreamBoundary
  -> LengthSMTLibQueryProtocolReceiver subject query
receiverAtBoundary plan phase boundary = LengthSMTLibQueryProtocolReceiver
  plan phase $ startSMTLibCausalStreamCursorAtBoundary boundary

acceptStreamStep
  :: LengthSMTLibQueryProtocolPlan subject query
  -> LengthSMTLibProtocolPhaseState
  -> SMTLibCausalStreamStep
  -> Either
      LengthSMTLibProtocolError
      (LengthSMTLibQueryProtocolAction subject query)
acceptStreamStep plan phase step = case step of
  SMTLibCausalStreamPending cursor -> Right $ SMTLibCausalAwait
    $ LengthSMTLibQueryProtocolReceiver plan phase cursor
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
  :: LengthSMTLibQueryProtocolReceiver subject query
  -> LengthSMTLibQueryProtocolPlan subject query
receiverPlan (LengthSMTLibQueryProtocolReceiver value _ _) = value

receiverPhase
  :: LengthSMTLibQueryProtocolReceiver subject query
  -> LengthSMTLibProtocolPhaseState
receiverPhase (LengthSMTLibQueryProtocolReceiver _ value _) = value

receiverCursor
  :: LengthSMTLibQueryProtocolReceiver subject query
  -> SMTLibCausalStreamCursor
receiverCursor (LengthSMTLibQueryProtocolReceiver _ _ value) = value

phaseName :: LengthSMTLibProtocolPhaseState -> LengthSMTLibProtocolPhase
phaseName phase = case phase of
  AwaitLengthSMTLibCheckStatus -> LengthSMTLibProtocolCheckStatusPhase
  AwaitLengthSMTLibCheckBarrier{} ->
    LengthSMTLibProtocolCheckBarrierPhase
  AwaitLengthSMTLibInputValues{} ->
    LengthSMTLibProtocolInputValuePhase
  AwaitLengthSMTLibInputValueBarrier{} ->
    LengthSMTLibProtocolInputValueBarrierPhase

planIdentity
  :: LengthSMTLibQueryProtocolPlan subject query
  -> LengthSMTLibProtocolIdentity subject query
planIdentity (LengthSMTLibQueryProtocolPlan value _ _ _ _ _ _ _) = value

planArtifactPolicy
  :: LengthSMTLibQueryProtocolPlan subject query
  -> LengthSMTLibArtifactPolicy
planArtifactPolicy
    (LengthSMTLibQueryProtocolPlan _ value _ _ _ _ _ _) = value

planResponseLimits
  :: LengthSMTLibQueryProtocolPlan subject query
  -> LengthSMTLibResponseLimits
planResponseLimits
    (LengthSMTLibQueryProtocolPlan _ _ value _ _ _ _ _) = value

planQuery
  :: LengthSMTLibQueryProtocolPlan subject query
  -> query
planQuery (LengthSMTLibQueryProtocolPlan _ _ _ value _ _ _ _) = value

planStreamPolicy
  :: LengthSMTLibQueryProtocolPlan subject query
  -> SMTLibCausalStreamPolicy
planStreamPolicy
    (LengthSMTLibQueryProtocolPlan _ _ _ _ value _ _ _) = value

limitsStreamPolicy
  :: LengthSMTLibProtocolLimits
  -> SMTLibCausalStreamPolicy
limitsStreamPolicy (LengthSMTLibProtocolLimits value _) = value

planCheckBarrier
  :: LengthSMTLibQueryProtocolPlan subject query
  -> SMTLibEchoSentinel
planCheckBarrier
    (LengthSMTLibQueryProtocolPlan _ _ _ _ _ value _ _) = value

planValueBarrier
  :: LengthSMTLibQueryProtocolPlan subject query
  -> Maybe SMTLibEchoSentinel
planValueBarrier
    (LengthSMTLibQueryProtocolPlan _ _ _ _ _ _ value _) = value

validatePlanFraming
  :: LengthSMTLibProtocolLimits
  -> LengthSMTLibPostLaunchExecutionPolicy
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
  responses = lengthSMTLibPostLaunchResponseLimits execution
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
  :: LengthSMTLibProtocolIdentity subject query
  -> LengthSMTLibProtocolLimits
  -> LengthSMTLibPostLaunchExecutionPolicy
  -> query
  -> Maybe [Word8]
  -> SMTLibEchoSentinel
  -> Maybe SMTLibEchoSentinel
  -> [Word8]
  -> Maybe [Word8]
  -> Either
      LengthSMTLibProtocolPlanError
      (Fingerprint subject)
buildPlanFingerprint identity limits execution query valueRequest checkBarrier
    valueBarrier initialWrite valueWrite = case
  buildFingerprintWithin maximumBytes FingerprintBuilder
    { fingerprintBuilderVersion = 1
    , fingerprintBuilderRole =
        protocolIdentityPlanFingerprintRole identity
    , fingerprintBuilderFields =
        [ tagged "schema"
            [FingerprintBytes $ protocolIdentityPlanSchemaTag identity]
        , tagged "execution-policy"
            [ FingerprintBytes $ fingerprintCanonicalBytes
                $ lengthSMTLibPostLaunchExecutionPolicyFingerprint execution
            ]
        ] ++
        protocolIdentityCapabilityReuseFields identity ++
        [ tagged "query"
            $ protocolIdentityQueryFieldPrefix identity ++
            [ FingerprintBytes
                $ protocolIdentityQueryFingerprintBytes identity query
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
            [ FingerprintBytes
                $ protocolIdentityPhaseMachineSchemaTag identity
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
            , FingerprintBytes
                $ protocolIdentityQueryCheckBytes identity query
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
