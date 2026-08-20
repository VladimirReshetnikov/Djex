-- | Package-private, pure framing and phase control for one binary-product
-- Length/Z3 query.
--
-- The complete phase machine, framing validation, write rendering, and
-- fingerprint construction live in
-- "Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol";
-- this wrapper contributes only what distinguishes the product domain: its
-- schema tags, plan fingerprint role, the explicit
-- @common-qf-lia-readiness-capability-reuse@ fingerprint fields, and the
-- exact product-query projections.  The nominal plan, receiver, and decoded
-- types remain distinct from their scalar siblings through the product query
-- type and fingerprint subject, so evidence sealed under one domain cannot
-- be presented under the other even when the rendered wire bytes coincide.
-- The barrier, phase, write-kind, and error vocabularies are deliberately
-- the shared ones re-exported below: they are diagnostics, not domain
-- authority.
module Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol.SpinePair
  ( lengthSpinePairSMTLibProtocolPlanSchemaTag
  , lengthSpinePairSMTLibProtocolPhaseMachineSchemaTag
  , lengthSpinePairSMTLibProtocolPostBarrierSchemaTag
  , LengthSpinePairSMTLibProtocolLimitSource
  , LengthSpinePairSMTLibProtocolLimits
  , mkLengthSpinePairSMTLibProtocolLimits
  , defaultLengthSpinePairSMTLibProtocolLimitSource
  , defaultLengthSpinePairSMTLibProtocolLimits
  , lengthSpinePairSMTLibProtocolStreamLimits
  , lengthSpinePairSMTLibProtocolCumulativeStdoutByteLimit
  , lengthSpinePairSMTLibProtocolPlanFingerprintByteLimit
  , LengthSMTLibProtocolBarrier (..)
  , LengthSMTLibProtocolRequiredFrame (..)
  , LengthSMTLibProtocolRequiredLimit (..)
  , LengthSMTLibProtocolPlanError (..)
  , LengthSpinePairSMTLibProtocolPlanError
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
  , LengthSMTLibProtocolPhase (..)
  , LengthSMTLibProtocolWriteKind (..)
  , LengthSpinePairSMTLibProtocolWriteKind
  , LengthSpinePairSMTLibProtocolReceiver
  , lengthSpinePairSMTLibProtocolReceiverPhase
  , LengthSpinePairSMTLibProtocolAction
  , startLengthSpinePairSMTLibProtocol
  , feedLengthSpinePairSMTLibProtocol
  , finishLengthSpinePairSMTLibProtocol
  , LengthSMTLibProtocolError (..)
  , LengthSpinePairSMTLibProtocolError
  , LengthSpinePairSMTLibProtocolDecoded
  , LengthSpinePairSMTLibProtocolObservation
  , lengthSpinePairSMTLibProtocolDecodedObservation
  ) where

import Data.Word (Word8)
import Numeric.Natural (Natural)

import qualified Language.Haskell.Synthesis.Fingerprint as PublicFingerprint
import Language.Haskell.Synthesis.Internal.Fingerprint
  ( Fingerprint
  , FingerprintField (..)
  )
import Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Execution
  ( LengthSMTLibArtifactPolicy
  , LengthSMTLibPostLaunchExecutionPolicy
  )
import Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol
  ( LengthSMTLibProtocolBarrier (..)
  , LengthSMTLibProtocolError (..)
  , LengthSMTLibProtocolIdentity (..)
  , LengthSMTLibProtocolLimitSource
  , LengthSMTLibProtocolLimits
  , LengthSMTLibProtocolObservation
  , LengthSMTLibProtocolPhase (..)
  , LengthSMTLibProtocolPlanError (..)
  , LengthSMTLibProtocolRequiredFrame (..)
  , LengthSMTLibProtocolRequiredLimit (..)
  , LengthSMTLibProtocolWriteKind (..)
  , LengthSMTLibQueryProtocolAction
  , LengthSMTLibQueryProtocolDecoded
  , LengthSMTLibQueryProtocolPlan
  , LengthSMTLibQueryProtocolReceiver
  , defaultLengthSMTLibProtocolLimitSource
  , defaultLengthSMTLibProtocolLimits
  , feedLengthSMTLibProtocol
  , finishLengthSMTLibProtocol
  , lengthSMTLibProtocolCumulativeStdoutByteLimit
  , lengthSMTLibProtocolDecodedObservation
  , lengthSMTLibProtocolInitialWriteBytes
  , lengthSMTLibProtocolInputValueWriteBytes
  , lengthSMTLibProtocolPlanArtifactPolicy
  , lengthSMTLibProtocolPlanCumulativeStdoutByteLimit
  , lengthSMTLibProtocolPlanFingerprint
  , lengthSMTLibProtocolPlanFingerprintByteLimit
  , lengthSMTLibProtocolPlanMinimumStdoutByteCount
  , lengthSMTLibProtocolPlanQuery
  , lengthSMTLibProtocolPostBarrierSchemaTag
  , lengthSMTLibProtocolReceiverPhase
  , lengthSMTLibProtocolStreamLimits
  , mkLengthSMTLibProtocolLimits
  , sealLengthSMTLibQueryProtocolPlan
  , startLengthSMTLibProtocol
  )
import Language.Haskell.Synthesis.Internal.SMTLib.Stream
  ( SMTLibStreamLimits )
import Language.Haskell.Synthesis.Semantic.Length.SMTLib
  ( LengthSpinePairSMTLibQuery
  , lengthSpinePairSMTLibQueryLogic
  , lengthSpinePairSMTLibQuerySchemaTag
  , lengthSpinePairSMTLibQueryCheckBytes
  , lengthSpinePairSMTLibQueryFingerprint
  , lengthSpinePairSMTLibQueryInputSymbols
  , lengthSpinePairSMTLibQueryInputValueRequestBytes
  )
import Language.Haskell.Synthesis.Semantic.Length.SMTLib.Response
  ( parseLengthSpinePairSMTLibInputValueResponse )

-- | Complete pure-plan schema.  This is distinct from a live session or run
-- identity: no process has been opened, inspected, or observed here.
lengthSpinePairSMTLibProtocolPlanSchemaTag :: [Word8]
lengthSpinePairSMTLibProtocolPlanSchemaTag =
  ascii "djex-length-spine-pair-z3-smtlib2-protocol-plan/v1"

-- | Ordered write, decode, and exact positional barrier policy.
lengthSpinePairSMTLibProtocolPhaseMachineSchemaTag :: [Word8]
lengthSpinePairSMTLibProtocolPhaseMachineSchemaTag =
  ascii "djex-length-spine-pair-z3-smtlib2-protocol-phase-machine/v1"

-- | The shared post-barrier whitespace policy; the bytes are common to both
-- protocol domains.
lengthSpinePairSMTLibProtocolPostBarrierSchemaTag :: [Word8]
lengthSpinePairSMTLibProtocolPostBarrierSchemaTag =
  lengthSMTLibProtocolPostBarrierSchemaTag

-- | Pure-protocol bounds are domain-neutral policy shared with the scalar
-- plan.
type LengthSpinePairSMTLibProtocolLimitSource =
  LengthSMTLibProtocolLimitSource

-- | Retained pure-protocol bounds for a product plan; the same type as the
-- scalar 'LengthSMTLibProtocolLimits', so one value may seal plans of either
-- domain.
type LengthSpinePairSMTLibProtocolLimits = LengthSMTLibProtocolLimits

-- | Retain raw product-protocol bounds without validation, exactly as
-- 'mkLengthSMTLibProtocolLimits' does for the scalar plan.
mkLengthSpinePairSMTLibProtocolLimits
  :: LengthSpinePairSMTLibProtocolLimitSource
  -> LengthSpinePairSMTLibProtocolLimits
mkLengthSpinePairSMTLibProtocolLimits = mkLengthSMTLibProtocolLimits

-- | The scalar default raw bounds: default per-frame stream limits, a
-- 512 KiB cumulative stdout cap, and a 256 KiB plan fingerprint bound.
defaultLengthSpinePairSMTLibProtocolLimitSource
  :: LengthSpinePairSMTLibProtocolLimitSource
defaultLengthSpinePairSMTLibProtocolLimitSource =
  defaultLengthSMTLibProtocolLimitSource

-- | 'defaultLengthSpinePairSMTLibProtocolLimitSource' retained through
-- 'mkLengthSpinePairSMTLibProtocolLimits'.
defaultLengthSpinePairSMTLibProtocolLimits
  :: LengthSpinePairSMTLibProtocolLimits
defaultLengthSpinePairSMTLibProtocolLimits =
  defaultLengthSMTLibProtocolLimits

-- | Per-frame framing limits (total bytes, retained frame bytes, nesting
-- depth) applied afresh to each expected product response frame.
lengthSpinePairSMTLibProtocolStreamLimits
  :: LengthSpinePairSMTLibProtocolLimits
  -> SMTLibStreamLimits
lengthSpinePairSMTLibProtocolStreamLimits =
  lengthSMTLibProtocolStreamLimits

-- | Maximum stdout bytes one product transaction may consume across all its
-- frames and accepted post-barrier whitespace; sealing rejects a cap below
-- the plan's smallest complete transcript.
lengthSpinePairSMTLibProtocolCumulativeStdoutByteLimit
  :: LengthSpinePairSMTLibProtocolLimits
  -> Natural
lengthSpinePairSMTLibProtocolCumulativeStdoutByteLimit =
  lengthSMTLibProtocolCumulativeStdoutByteLimit

-- | Admission-only cap on the canonical byte size of a sealed product plan
-- fingerprint; it is not itself a fingerprint field.
lengthSpinePairSMTLibProtocolPlanFingerprintByteLimit
  :: LengthSpinePairSMTLibProtocolLimits
  -> Natural
lengthSpinePairSMTLibProtocolPlanFingerprintByteLimit =
  lengthSMTLibProtocolPlanFingerprintByteLimit

-- | Plan admission failures share the scalar vocabulary.
type LengthSpinePairSMTLibProtocolPlanError = LengthSMTLibProtocolPlanError

-- | Uninhabited phantom subject of a binary-product plan fingerprint.  It is
-- distinct from the scalar 'LengthSMTLibProtocolPlanFingerprintSubject', so
-- product and scalar plan fingerprints never unify.
data LengthSpinePairSMTLibProtocolPlanFingerprintSubject

-- | The binary-product plan is the shared machine at the product query and
-- fingerprint subject.
type LengthSpinePairSMTLibProtocolPlan identity local =
  LengthSMTLibQueryProtocolPlan
    LengthSpinePairSMTLibProtocolPlanFingerprintSubject
    (LengthSpinePairSMTLibQuery identity local)

-- | The binary-product protocol identity: product schema tags, the product
-- plan fingerprint role, the explicit common-capability-reuse fingerprint
-- fields, the product query-schema prefix of the @query@ group, and the
-- exact product-query projections.
spinePairProtocolIdentity
  :: LengthSMTLibProtocolIdentity
      LengthSpinePairSMTLibProtocolPlanFingerprintSubject
      (LengthSpinePairSMTLibQuery identity local)
spinePairProtocolIdentity = LengthSMTLibProtocolIdentity
  { protocolIdentityPlanSchemaTag =
      lengthSpinePairSMTLibProtocolPlanSchemaTag
  , protocolIdentityPhaseMachineSchemaTag =
      lengthSpinePairSMTLibProtocolPhaseMachineSchemaTag
  , protocolIdentityPlanFingerprintRole = ascii
      "finite-binary-product-spine-lengths/z3-smtlib2-protocol-plan"
  , protocolIdentityCapabilityReuseFields =
      [ FingerprintTag
          (ascii "common-qf-lia-readiness-capability-reuse")
          [ FingerprintBytes $ ascii $ concat
              [ "reuse-scalar-named-ready-worker-only-as-exact-common-"
              , "qf-lia-input-value-transport-profile/"
              , "no-scalar-behavioral-authority/v1"
              ]
          , FingerprintBytes lengthSpinePairSMTLibQuerySchemaTag
          , FingerprintBytes lengthSpinePairSMTLibQueryLogic
          ]
      ]
  , protocolIdentityQueryFieldPrefix =
      [FingerprintBytes lengthSpinePairSMTLibQuerySchemaTag]
  , protocolIdentityQueryCheckBytes = lengthSpinePairSMTLibQueryCheckBytes
  , protocolIdentityQueryFingerprintBytes =
      PublicFingerprint.fingerprintCanonicalBytes
        . lengthSpinePairSMTLibQueryFingerprint
  , protocolIdentityQueryInputSymbols =
      lengthSpinePairSMTLibQueryInputSymbols
  , protocolIdentityQueryInputValueRequestBytes =
      lengthSpinePairSMTLibQueryInputValueRequestBytes
  , protocolIdentityParseInputValueResponse =
      parseLengthSpinePairSMTLibInputValueResponse
  }

-- | Seal a pure product transaction from caller-provided barrier nonce
-- bytes.  The optional value nonce is required exactly when the artifact
-- policy requests values and the query has at least one input symbol.
sealLengthSpinePairSMTLibProtocolPlan
  :: LengthSpinePairSMTLibProtocolLimits
  -> LengthSMTLibPostLaunchExecutionPolicy
  -> LengthSpinePairSMTLibQuery identity local
  -> [Word8]
  -> Maybe [Word8]
  -> Either
      LengthSpinePairSMTLibProtocolPlanError
      (LengthSpinePairSMTLibProtocolPlan identity local)
sealLengthSpinePairSMTLibProtocolPlan =
  sealLengthSMTLibQueryProtocolPlan spinePairProtocolIdentity

-- | Exact bytes of the product plan's first write: the Z3 query reset
-- prefix, the product query's canonical check commands, then the
-- status-barrier echo command.  Rendered on demand from the retained plan.
lengthSpinePairSMTLibProtocolInitialWriteBytes
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> [Word8]
lengthSpinePairSMTLibProtocolInitialWriteBytes =
  lengthSMTLibProtocolInitialWriteBytes

-- | Exact bytes of the conditional second write: the product query's
-- @get-value@ request followed by the value-barrier echo command.  'Nothing'
-- when the plan carries no value barrier or the query has no input symbols.
lengthSpinePairSMTLibProtocolInputValueWriteBytes
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> Maybe [Word8]
lengthSpinePairSMTLibProtocolInputValueWriteBytes =
  lengthSMTLibProtocolInputValueWriteBytes

-- | Complete reversible identity of the sealed product plan.  Beyond the
-- shared plan fields it binds the product schema tags, the
-- @common-qf-lia-readiness-capability-reuse@ fields, and the product
-- query-schema prefix, under the product fingerprint subject.
lengthSpinePairSMTLibProtocolPlanFingerprint
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> Fingerprint LengthSpinePairSMTLibProtocolPlanFingerprintSubject
lengthSpinePairSMTLibProtocolPlanFingerprint =
  lengthSMTLibProtocolPlanFingerprint

-- | Exact product query retained by this sealed protocol plan.
lengthSpinePairSMTLibProtocolPlanQuery
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> LengthSpinePairSMTLibQuery identity local
lengthSpinePairSMTLibProtocolPlanQuery = lengthSMTLibProtocolPlanQuery

-- | Artifact policy retained by this exact sealed protocol plan.
lengthSpinePairSMTLibProtocolPlanArtifactPolicy
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> LengthSMTLibArtifactPolicy
lengthSpinePairSMTLibProtocolPlanArtifactPolicy =
  lengthSMTLibProtocolPlanArtifactPolicy

-- | Final causal transcript cap retained by this exact sealed plan.
lengthSpinePairSMTLibProtocolPlanCumulativeStdoutByteLimit
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> Natural
lengthSpinePairSMTLibProtocolPlanCumulativeStdoutByteLimit =
  lengthSMTLibProtocolPlanCumulativeStdoutByteLimit

-- | Smallest complete live transcript admitted by this exact plan, including
-- the required lexical delimiter after each bare status and final echo.
lengthSpinePairSMTLibProtocolPlanMinimumStdoutByteCount
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> Natural
lengthSpinePairSMTLibProtocolPlanMinimumStdoutByteCount =
  lengthSMTLibProtocolPlanMinimumStdoutByteCount

-- | Write kinds share the scalar vocabulary.
type LengthSpinePairSMTLibProtocolWriteKind = LengthSMTLibProtocolWriteKind

-- | The binary-product receiver is the shared machine at the product query
-- and fingerprint subject.
type LengthSpinePairSMTLibProtocolReceiver identity local =
  LengthSMTLibQueryProtocolReceiver
    LengthSpinePairSMTLibProtocolPlanFingerprintSubject
    (LengthSpinePairSMTLibQuery identity local)

-- | Name of the response frame this product receiver is waiting for; the
-- phase vocabulary is shared with the scalar protocol.
lengthSpinePairSMTLibProtocolReceiverPhase
  :: LengthSpinePairSMTLibProtocolReceiver identity local
  -> LengthSMTLibProtocolPhase
lengthSpinePairSMTLibProtocolReceiverPhase =
  lengthSMTLibProtocolReceiverPhase

-- | The shared causal action at the product receiver and decoded outcome.
type LengthSpinePairSMTLibProtocolAction identity local =
  LengthSMTLibQueryProtocolAction
    LengthSpinePairSMTLibProtocolPlanFingerprintSubject
    (LengthSpinePairSMTLibQuery identity local)

-- | Start with reset, canonical check commands, and the status barrier in
-- one exact write.
startLengthSpinePairSMTLibProtocol
  :: LengthSpinePairSMTLibProtocolPlan identity local
  -> LengthSpinePairSMTLibProtocolAction identity local
startLengthSpinePairSMTLibProtocol = startLengthSMTLibProtocol

-- | Feed one chunk of stdout bytes to a product receiver.  The phase machine
-- is the shared one: a completed frame is decoded with the product
-- input-value parser, barriers are matched exactly, and only bounded SMT-LIB
-- whitespace may follow an accepted barrier before the next action.
feedLengthSpinePairSMTLibProtocol
  :: LengthSpinePairSMTLibProtocolReceiver identity local
  -> [Word8]
  -> Either
      LengthSpinePairSMTLibProtocolError
      (LengthSpinePairSMTLibProtocolAction identity local)
feedLengthSpinePairSMTLibProtocol = feedLengthSMTLibProtocol

-- | EOF is never a successful terminal delimiter for a reusable worker.
finishLengthSpinePairSMTLibProtocol
  :: LengthSpinePairSMTLibProtocolReceiver identity local
  -> Either
      LengthSpinePairSMTLibProtocolError
      (LengthSpinePairSMTLibProtocolAction identity local)
finishLengthSpinePairSMTLibProtocol = finishLengthSMTLibProtocol

-- | Protocol failures share the scalar vocabulary.
type LengthSpinePairSMTLibProtocolError = LengthSMTLibProtocolError

-- | The binary-product decoded outcome is the shared machine at the product
-- query and fingerprint subject.
type LengthSpinePairSMTLibProtocolDecoded identity local =
  LengthSMTLibQueryProtocolDecoded
    LengthSpinePairSMTLibProtocolPlanFingerprintSubject
    (LengthSpinePairSMTLibQuery identity local)

-- | The one status-indexed value admitted by pure protocol completion.
type LengthSpinePairSMTLibProtocolObservation =
  LengthSMTLibProtocolObservation

-- | The status-indexed observation decoded by a completed product
-- transaction.  It is a syntactic protocol result only and grants no
-- semantic authority to the status or bindings.
lengthSpinePairSMTLibProtocolDecodedObservation
  :: LengthSpinePairSMTLibProtocolDecoded identity local
  -> LengthSpinePairSMTLibProtocolObservation
lengthSpinePairSMTLibProtocolDecodedObservation =
  lengthSMTLibProtocolDecodedObservation

ascii :: String -> [Word8]
ascii = map $ fromIntegral . fromEnum
