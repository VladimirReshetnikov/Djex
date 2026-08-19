{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Package-private ownership of one capability-probed Length/Z3 worker.
--
-- A worker exists only inside the rank-N callback supplied to
-- 'withLengthSMTLibReadyWorker'.  The scope owns the executable observation,
-- fresh working directory observed empty under the stable-namespace
-- assumption, four entropy-derived capability barriers,
-- subprocess, background pipe readers, and structurally bounded cleanup.  No
-- raw process handle or secret barrier seed is projected.  The private,
-- reversible identity deliberately retains spent readiness barriers and their
-- transcript; those values cannot derive the seed used by later query roles.
-- After readiness, the worker retains only the query-count and run-identity
-- caps, protocol limits, and a strict post-launch policy containing the host
-- deadline, artifact/response policy, and original complete execution key
-- needed to derive each query deadline and seal future plans.  The structured
-- Z3 launch profile is not retained.  Process limits remain owned by the exact
-- retained process; opener,
-- workspace-allocation, capability, and ready-identity bounds stay outside the
-- lent worker.  Replay projects its query and artifact policy from the exact
-- sealed plan rather than pairing that plan with worker-wide copies.
--
-- The executable identity is intentionally named a pre-spawn pathname
-- snapshot rather than an attested image.  The portable @process@ backend
-- cannot execute the already-hashed file descriptor, and its digest excludes
-- the dynamic loader and shared libraries.  Likewise, "no stderr observed"
-- is a point-in-time reader observation: a later stderr byte poisons the
-- worker, but independent pipes cannot prove per-command stderr absence.
module Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session
  ( lengthSMTLibSessionSchemaTag
  , lengthSMTLibReadyWorkerSchemaTag
  , lengthSMTLibSessionEpochSchemaTag
  , lengthSMTLibSessionWorkspaceSchemaTag
  , LengthSMTLibSessionLimitField (..)
  , LengthSMTLibSessionLimitSource (..)
  , defaultLengthSMTLibSessionLimitSource
  , LengthSMTLibSessionLimits
  , mkLengthSMTLibSessionLimits
  , defaultLengthSMTLibSessionLimits
  , LengthSMTLibSessionConfigError (..)
  , LengthSMTLibSessionConfig
  , sealLengthSMTLibSessionConfig
  , LengthSMTLibSessionWorkspaceFailure (..)
  , LengthSMTLibSessionWorkspaceCleanupStatus (..)
  , LengthSMTLibSessionCleanupStatus (..)
  , LengthSMTLibSessionError (..)
  , LengthSMTLibSessionScopeError (..)
  , LengthSMTLibSessionUsableWorkDeadline
  , withLengthSMTLibSessionUsableWorkDeadline
  , withLengthSMTLibSessionUsableWorkDeadlineForBudgetedSession
  , LengthSMTLibSessionScopedUsableWorkDeadline
  , withLengthSMTLibSessionScopedUsableWorkDeadline
  , withLengthSMTLibSessionScopedUsableWorkDeadlineForBudgetedSession
  , checkLengthSMTLibSessionScopedUsableWorkDeadline
  , LengthSMTLibReadyWorker
  , LengthSMTLibReadyWorkerIdentitySubject
  , withLengthSMTLibReadyWorker
  , withLengthSMTLibReadyWorkerUnderDeadline
  , withLengthSMTLibReadyWorkerUnderScopedDeadline
  , lengthSMTLibReadyWorkerIdentityFingerprint
  , lengthSMTLibReadyWorkerIdentityFingerprintField
  , lengthSMTLibReadyWorkerExecutableSHA256
  , lengthSMTLibReadyWorkerExecutableByteCount
  , lengthSMTLibReadyWorkerExecutableSnapshotStrengthTag
  , lengthSMTLibReadyWorkerCapabilityTranscriptSHA256
  , lengthSMTLibReadyWorkerCapabilityTranscriptByteCount
  , lengthSMTLibReadyWorkerObservedStdoutBytes
  , lengthSMTLibReadyWorkerObservedStderrBytes
  , lengthSMTLibReadyWorkerWorkingDirectory
  , lengthSMTLibQueryBarrierSchemaTag
  , lengthSMTLibQueryRunSchemaTag
  , lengthSMTLibBudgetedQueryRunSchemaTag
  , lengthSMTLibScopedBudgetedQueryRunSchemaTag
  , LengthSMTLibQueryRunFailure (..)
  , LengthSMTLibQueryRunError (..)
  , LengthSMTLibQueryRun
  , LengthSMTLibQueryRunIdentitySubject
  , runLengthSMTLibReadyWorkerQuery
  , lengthSMTLibQueryRunOrdinal
  , LengthSMTLibQueryRunObservation
  , lengthSMTLibQueryRunObservation
  , lengthSMTLibQueryRunIdentityFingerprint
  , lengthSMTLibQueryRunIdentityFingerprintField
  , lengthSMTLibQueryRunTranscriptSHA256
  , lengthSMTLibQueryRunTranscriptByteCount
  , lengthSMTLibQueryRunStdoutStart
  , lengthSMTLibQueryRunStdoutEnd
  , lengthSMTLibQueryRunStderrStart
  , lengthSMTLibQueryRunStderrEnd
  , lengthSpinePairSMTLibQueryRunSchemaTag
  , lengthSpinePairSMTLibBudgetedQueryRunSchemaTag
  , lengthSpinePairSMTLibScopedBudgetedQueryRunSchemaTag
  , LengthSpinePairSMTLibQueryRunFailure (..)
  , LengthSpinePairSMTLibQueryRunError (..)
  , LengthSpinePairSMTLibQueryRun
  , LengthSpinePairSMTLibQueryRunIdentitySubject
  , runLengthSpinePairSMTLibReadyWorkerQuery
  , lengthSpinePairSMTLibQueryRunOrdinal
  , LengthSpinePairSMTLibQueryRunObservation
  , lengthSpinePairSMTLibQueryRunObservation
  , lengthSpinePairSMTLibQueryRunIdentityFingerprint
  , lengthSpinePairSMTLibQueryRunIdentityFingerprintField
  , lengthSpinePairSMTLibQueryRunTranscriptSHA256
  , lengthSpinePairSMTLibQueryRunTranscriptByteCount
  , lengthSpinePairSMTLibQueryRunStdoutStart
  , lengthSpinePairSMTLibQueryRunStdoutEnd
  , lengthSpinePairSMTLibQueryRunStderrStart
  , lengthSpinePairSMTLibQueryRunStderrEnd
  ) where

import Control.Concurrent
  ( ThreadId
  , forkIOWithUnmask
  , myThreadId
  )
import Control.Concurrent.MVar
  ( MVar
  , modifyMVarMasked
  , newEmptyMVar
  , newMVar
  , putMVar
  , takeMVar
  , withMVar
  )
import Control.Concurrent.STM
  ( TMVar
  , TVar
  , atomically
  , newTMVarIO
  , newTVarIO
  , putTMVar
  , readTMVar
  , readTVar
  , readTVarIO
  , takeTMVar
  , tryTakeTMVar
  , writeTVar
  )
import Control.DeepSeq (NFData (rnf), force)
import Control.Exception
  ( SomeException
  , evaluate
  , finally
  , mask
  , onException
  , try
  )
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Bifunctor (bimap)
import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (intToDigit, ord)
import Data.List (nub)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Word (Word64, Word8)
import Numeric.Natural (Natural)
import System.Directory
  ( canonicalizePath
  , getTemporaryDirectory
  , removeDirectory
  )
import System.FilePath
  ( (</>)
  , isAbsolute
  )
import System.IO.Error
  ( isAlreadyExistsError
  , tryIOError
  )
import System.Entropy (getEntropy)

#ifndef mingw32_HOST_OS
import Control.Exception (bracket)
import qualified System.Posix.Directory as PosixDirectory
import qualified System.Posix.Files as PosixFiles
import qualified System.Posix.IO as PosixIO
import System.Posix.Types (DeviceID, Fd, FileID, FileMode, UserID)
import qualified System.Posix.User as PosixUser
#else
import System.Directory
  ( createDirectory
  , doesDirectoryExist
  , listDirectory
  , pathIsSymbolicLink
  )
#endif

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
  , LengthSMTLibExecutableLaunchStrategy (..)
  , LengthSMTLibPostLaunchExecutionPolicy
  , lengthSMTLibExecutionExecutableLaunchStrategy
  , lengthSMTLibExecutionPolicyFingerprint
  , lengthSMTLibExecutionZ3Profile
  , lengthSMTLibPostLaunchArtifactPolicy
  , lengthSMTLibPostLaunchHostDeadlineMilliseconds
  , retainLengthSMTLibPostLaunchExecutionPolicy
  )
import Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol
  ( LengthSMTLibProtocolError (..)
  , LengthSMTLibProtocolLimits
  , LengthSMTLibProtocolPlanError
  , LengthSMTLibProtocolPlanFingerprintSubject
  , LengthSMTLibProtocolWriteKind (..)
  , LengthSMTLibQueryProtocolDecoded
  , LengthSMTLibQueryProtocolPlan
  , feedLengthSMTLibProtocol
  , finishLengthSMTLibProtocol
  , lengthSMTLibProtocolCumulativeStdoutByteLimit
  , lengthSMTLibProtocolDecodedObservation
  , lengthSMTLibProtocolInputValueWriteBytes
  , lengthSMTLibProtocolPlanCumulativeStdoutByteLimit
  , lengthSMTLibProtocolPlanFingerprint
  , lengthSMTLibProtocolPlanArtifactPolicy
  , lengthSMTLibProtocolPlanQuery
  , lengthSMTLibProtocolPlanMinimumStdoutByteCount
  , lengthSMTLibProtocolStreamLimits
  , sealLengthSMTLibProtocolPlan
  , startLengthSMTLibProtocol
  )
import Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol.SpinePair
  ( LengthSpinePairSMTLibProtocolError
  , LengthSpinePairSMTLibProtocolPlanError
  , LengthSpinePairSMTLibProtocolPlanFingerprintSubject
  , defaultLengthSpinePairSMTLibProtocolLimits
  , sealLengthSpinePairSMTLibProtocolPlan
  )
import Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session.Capability
  ( LengthSMTLibCapabilityError (..)
  , LengthSMTLibCapabilityLimits
  , LengthSMTLibCapabilityOutcome
  , LengthSMTLibCapabilityPlan
  , LengthSMTLibCapabilityPlanError
  , LengthSMTLibCapabilityWriteKind (..)
  , feedLengthSMTLibCapability
  , finishLengthSMTLibCapability
  , lengthSMTLibCapabilityOutcomePlanFingerprint
  , lengthSMTLibCapabilityMinimumOutputByteCount
  , lengthSMTLibCapabilityPlanCumulativeOutputByteLimit
  , sealLengthSMTLibCapabilityPlan
  , startLengthSMTLibCapability
  )
import Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session.Process
  ( LengthSMTLibProcess
  , LengthSMTLibProcessCancellation
  , LengthSMTLibProcessCleanupEscalation (..)
  , LengthSMTLibProcessCleanupStatus (..)
  , LengthSMTLibProcessDeadline
  , LengthSMTLibProcessError (..)
  , LengthSMTLibProcessFailureClass (..)
  , LengthSMTLibProcessLimits
  , LengthSMTLibProcessPhase (..)
  , cancelLengthSMTLibProcess
  , checkLengthSMTLibProcessDeadline
  , checkLengthSMTLibProcessReady
  , closeLengthSMTLibProcess
  , lengthSMTLibExecutableSnapshotByteCount
  , lengthSMTLibExecutableSnapshotSHA256
  , lengthSMTLibExecutableSnapshotStrengthTag
  , lengthSMTLibDescriptorBoundExecutableLaunchStrengthTag
  , lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchStrengthTag
  , lengthSMTLibDescriptorBoundExecveCheckExecutableAccessLaunchStrengthTag
  , lengthSMTLibProcessDeadlineAfterMilliseconds
  , lengthSMTLibProcessDeadlineFingerprintField
  , lengthSMTLibProcessFingerprintField
  , lengthSMTLibProcessLimits
  , lengthSMTLibProcessObservedStderrBytes
  , lengthSMTLibProcessObservedStdoutBytes
  , lengthSMTLibProcessStderrByteLimit
  , lengthSMTLibProcessStdoutByteLimit
  , lengthSMTLibProcessSnapshot
  , lengthSMTLibProcessUsesDescriptorBoundExecutableLaunch
  , lengthSMTLibProcessUsesDescriptorBoundEffectiveIDExecutableAccessLaunch
  , lengthSMTLibProcessUsesDescriptorBoundExecveCheckExecutableAccessLaunch
  , mkLengthSMTLibWorkingDirectoryDescriptor
  , newLengthSMTLibProcessCancellation
  , compareLengthSMTLibProcessDeadline
  , minimumLengthSMTLibProcessDeadline
  , openLengthSMTLibProcess
  , openLengthSMTLibDescriptorBoundProcess
  , openLengthSMTLibDescriptorBoundEffectiveIDExecutableAccessProcess
  , openLengthSMTLibDescriptorBoundExecveCheckExecutableAccessProcess
  , runBeforeLengthSMTLibProcessDeadline
  , waitLengthSMTLibProcessControl
  )
import Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session.Transport
  ( lengthSMTLibCausalTransport
  , lengthSMTLibCausalTransportOps
  )
import Language.Haskell.Synthesis.Internal.SMTLib.Causal.Driver
  ( SMTLibCausalFailure (..)
  , SMTLibCausalInitialBoundary (..)
  , SMTLibCausalTranscript
  , SMTLibCausalTranscriptEpoch
  , driveSMTLibCausalActions
  , smtLibCausalTranscriptByteCount
  , smtLibCausalTranscriptEpochBytes
  , smtLibCausalTranscriptEpochKind
  , smtLibCausalTranscriptEpochs
  , smtLibCausalTranscriptInheritedBytes
  )
import Language.Haskell.Synthesis.Internal.SMTLib.Stream
  ( smtLibStreamFrameByteLimit
  , smtLibStreamFramingSchemaTag
  , smtLibStreamNestingDepthLimit
  , smtLibStreamTotalByteLimit
  )
import Language.Haskell.Synthesis.Semantic.Length
  ( FiniteBinaryProductSpineLengthsV1
  , FiniteListSpineLengthV1
  )
import Language.Haskell.Synthesis.Semantic.Length.Evaluate
  ( LengthEvaluationLimits
  , ValidatedLengthCounterexample
  , ValidatedLengthSpinePairCounterexample
  , lengthAssignmentValueBitLimit
  , lengthIntermediateValueBitLimit
  )
import Language.Haskell.Synthesis.Semantic.Length.SMTLib
  ( LengthSMTLibIntegerBinding
  , LengthSMTLibModelError
  , LengthSMTLibQuery
  , LengthSpinePairSMTLibModelError
  , LengthSpinePairSMTLibQuery
  , lengthSMTLibQueryInputValueRequestBytes
  , lengthSpinePairSMTLibQueryInputValueRequestBytes
  , lengthSpinePairSMTLibQueryLogic
  , lengthSpinePairSMTLibQuerySchemaTag
  , validateLengthSMTLibCounterexample
  , validateLengthSpinePairSMTLibCounterexample
  )
import Language.Haskell.Synthesis.Semantic.Observation
  ( SolverObservation (..)
  , SolverStatus (..)
  )
import Language.Haskell.Synthesis.Semantic.Problem
  ( BehavioralEvidence )

-- | Schema tag of the scoped worker-session layer, bound as the first field
-- of every ready-worker identity fingerprint ahead of the launch-specific
-- ready-worker tag.
lengthSMTLibSessionSchemaTag :: [Word8]
lengthSMTLibSessionSchemaTag =
  ascii "djex-length-z3-scoped-worker-session/v3"

-- The driver implementation is now domain-neutral, but the historical
-- Length/Z3 literal remains owned by this identity layer so the abstraction
-- move cannot change ready-worker or query-run fingerprint bytes.
lengthSMTLibCausalDriverSchemaTag :: [Word8]
lengthSMTLibCausalDriverSchemaTag = ascii
  "djex-length-z3-causal-byte-stream-driver/v1"

-- | Ready-worker identity schema tag for the portable pre-spawn
-- pathname-snapshot launch.  Descriptor-bound launches bind their own
-- sibling tags, so this value appears in a ready-worker identity only when
-- the process was spawned directly from its pathname.
lengthSMTLibReadyWorkerSchemaTag :: [Word8]
lengthSMTLibReadyWorkerSchemaTag =
  ascii "djex-length-z3-capability-probed-ready-worker/v4"

lengthSMTLibDescriptorBoundReadyWorkerSchemaTag :: [Word8]
lengthSMTLibDescriptorBoundReadyWorkerSchemaTag = ascii
  "djex-length-z3-capability-probed-sealed-main-image-ready-worker/v1"

lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessReadyWorkerSchemaTag
  :: [Word8]
lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessReadyWorkerSchemaTag =
  ascii $ concat
    [ "djex-length-z3-capability-probed-effective-id-executable-access-"
    , "sealed-main-image-ready-worker/v1"
    ]

lengthSMTLibDescriptorBoundExecveCheckExecutableAccessReadyWorkerSchemaTag
  :: [Word8]
lengthSMTLibDescriptorBoundExecveCheckExecutableAccessReadyWorkerSchemaTag =
  ascii $ concat
    [ "djex-length-z3-capability-probed-execve-check-executable-access-"
    , "sealed-main-image-ready-worker/v1"
    ]

-- | Each exclusive workspace attempt samples independent secret and public
-- 256-bit halves.  The public half names the directory; readiness barriers are
-- SHA-256-domain-separated and query barriers use HMAC-SHA256 over fixed-width
-- ordinals.  Every emitted role is also checked against a bounded session-wide
-- set.  The child can observe its cwd without learning the barrier seed.  This
-- is an entropy/HMAC/collision-check claim, not mathematical global freshness.
lengthSMTLibSessionEpochSchemaTag :: [Word8]
lengthSMTLibSessionEpochSchemaTag =
  ascii "os-entropy-512/split-public-label-secret-barrier-seed/v3"

-- | Schema tag for per-query barrier derivation.  Each query barrier is
-- HMAC-SHA256 under the secret session seed over this tag, a one-byte role
-- (check or input-value), and the big-endian 64-bit query ordinal; the tag is
-- also bound in the query-allocation field of every query-run identity.
lengthSMTLibQueryBarrierSchemaTag :: [Word8]
lengthSMTLibQueryBarrierSchemaTag = ascii
  "hmac-sha256/secret-seed/fixed-role/u64be-ordinal/v1"

-- | Query-run identity schema tag for a scalar run on a portable
-- pathname-snapshot worker under the fresh-per-query deadline policy.
-- Budgeted, scoped, and descriptor-bound workers bind their own sibling tags.
lengthSMTLibQueryRunSchemaTag :: [Word8]
lengthSMTLibQueryRunSchemaTag = ascii
  "djex-length-z3-capability-probed-pre-spawn-pathname-snapshot-worker-query-run/v1"

-- | Additive envelope for a scalar run whose effective absolute deadline is
-- selected under one shared usable-work deadline.  The legacy schema remains
-- byte-exact for workers which retain the historical fresh-per-query policy.
lengthSMTLibBudgetedQueryRunSchemaTag :: [Word8]
lengthSMTLibBudgetedQueryRunSchemaTag = ascii $ concat
  [ "djex-length-z3-capability-probed-pre-spawn-pathname-snapshot-worker-"
  , "query-run/shared-usable-work-deadline/v1"
  ]

-- | Scalar run envelope for the owner-thread-affine, dynamically scoped
-- shared deadline.  Checkpoint observations never enter this identity; the
-- schema records only the policy under which the worker and run were admitted.
lengthSMTLibScopedBudgetedQueryRunSchemaTag :: [Word8]
lengthSMTLibScopedBudgetedQueryRunSchemaTag = ascii $ concat
  [ "djex-length-z3-capability-probed-pre-spawn-pathname-snapshot-worker-"
  , "query-run/scoped-shared-usable-work-deadline/v2"
  ]

lengthSMTLibDescriptorBoundQueryRunSchemaTag :: [Word8]
lengthSMTLibDescriptorBoundQueryRunSchemaTag = ascii
  "djex-length-z3-capability-probed-sealed-main-image-worker-query-run/v1"

lengthSMTLibDescriptorBoundBudgetedQueryRunSchemaTag :: [Word8]
lengthSMTLibDescriptorBoundBudgetedQueryRunSchemaTag = ascii $ concat
  [ "djex-length-z3-capability-probed-sealed-main-image-worker-query-run/"
  , "shared-usable-work-deadline/v1"
  ]

lengthSMTLibDescriptorBoundScopedBudgetedQueryRunSchemaTag :: [Word8]
lengthSMTLibDescriptorBoundScopedBudgetedQueryRunSchemaTag = ascii $ concat
  [ "djex-length-z3-capability-probed-sealed-main-image-worker-query-run/"
  , "scoped-shared-usable-work-deadline/v1"
  ]

lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessQueryRunSchemaTag
  :: [Word8]
lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessQueryRunSchemaTag =
  ascii $ concat
    [ "djex-length-z3-capability-probed-effective-id-executable-access-"
    , "sealed-main-image-worker-query-run/v1"
    ]

lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessBudgetedQueryRunSchemaTag
  :: [Word8]
lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessBudgetedQueryRunSchemaTag =
  ascii $ concat
    [ "djex-length-z3-capability-probed-effective-id-executable-access-"
    , "sealed-main-image-worker-query-run/shared-usable-work-deadline/v1"
    ]

lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessScopedBudgetedQueryRunSchemaTag
  :: [Word8]
lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessScopedBudgetedQueryRunSchemaTag =
  ascii $ concat
    [ "djex-length-z3-capability-probed-effective-id-executable-access-"
    , "sealed-main-image-worker-query-run/"
    , "scoped-shared-usable-work-deadline/v1"
    ]

lengthSMTLibDescriptorBoundExecveCheckExecutableAccessQueryRunSchemaTag
  :: [Word8]
lengthSMTLibDescriptorBoundExecveCheckExecutableAccessQueryRunSchemaTag =
  ascii $ concat
    [ "djex-length-z3-capability-probed-execve-check-executable-access-"
    , "sealed-main-image-worker-query-run/v1"
    ]

lengthSMTLibDescriptorBoundExecveCheckExecutableAccessBudgetedQueryRunSchemaTag
  :: [Word8]
lengthSMTLibDescriptorBoundExecveCheckExecutableAccessBudgetedQueryRunSchemaTag =
  ascii $ concat
    [ "djex-length-z3-capability-probed-execve-check-executable-access-"
    , "sealed-main-image-worker-query-run/shared-usable-work-deadline/v1"
    ]

lengthSMTLibDescriptorBoundExecveCheckExecutableAccessScopedBudgetedQueryRunSchemaTag
  :: [Word8]
lengthSMTLibDescriptorBoundExecveCheckExecutableAccessScopedBudgetedQueryRunSchemaTag =
  ascii $ concat
    [ "djex-length-z3-capability-probed-execve-check-executable-access-"
    , "sealed-main-image-worker-query-run/"
    , "scoped-shared-usable-work-deadline/v1"
    ]

-- | Nominal product-domain run envelope.  The shared ready-worker capability
-- is embedded only as an exact QF_LIA/input-value transport observation; it
-- contributes no scalar behavioral authority to this product run.
lengthSpinePairSMTLibQueryRunSchemaTag :: [Word8]
lengthSpinePairSMTLibQueryRunSchemaTag = ascii
  "djex-length-spine-pair-z3-capability-probed-pre-spawn-pathname-snapshot-worker-query-run/v1"

-- | Additive binary-product sibling of
-- 'lengthSMTLibBudgetedQueryRunSchemaTag'.
lengthSpinePairSMTLibBudgetedQueryRunSchemaTag :: [Word8]
lengthSpinePairSMTLibBudgetedQueryRunSchemaTag = ascii $ concat
  [ "djex-length-spine-pair-z3-capability-probed-pre-spawn-pathname-"
  , "snapshot-worker-query-run/shared-usable-work-deadline/v1"
  ]

-- | Nominal binary-product sibling of
-- 'lengthSMTLibScopedBudgetedQueryRunSchemaTag'.
lengthSpinePairSMTLibScopedBudgetedQueryRunSchemaTag :: [Word8]
lengthSpinePairSMTLibScopedBudgetedQueryRunSchemaTag = ascii $ concat
  [ "djex-length-spine-pair-z3-capability-probed-pre-spawn-pathname-"
  , "snapshot-worker-query-run/scoped-shared-usable-work-deadline/v2"
  ]

lengthSpinePairSMTLibDescriptorBoundQueryRunSchemaTag :: [Word8]
lengthSpinePairSMTLibDescriptorBoundQueryRunSchemaTag = ascii $ concat
  [ "djex-length-spine-pair-z3-capability-probed-sealed-main-image-"
  , "worker-query-run/v1"
  ]

lengthSpinePairSMTLibDescriptorBoundBudgetedQueryRunSchemaTag :: [Word8]
lengthSpinePairSMTLibDescriptorBoundBudgetedQueryRunSchemaTag = ascii $ concat
  [ "djex-length-spine-pair-z3-capability-probed-sealed-main-image-"
  , "worker-query-run/shared-usable-work-deadline/v1"
  ]

lengthSpinePairSMTLibDescriptorBoundScopedBudgetedQueryRunSchemaTag :: [Word8]
lengthSpinePairSMTLibDescriptorBoundScopedBudgetedQueryRunSchemaTag =
  ascii $ concat
    [ "djex-length-spine-pair-z3-capability-probed-sealed-main-image-"
    , "worker-query-run/scoped-shared-usable-work-deadline/v1"
    ]

lengthSpinePairSMTLibDescriptorBoundEffectiveIDExecutableAccessQueryRunSchemaTag
  :: [Word8]
lengthSpinePairSMTLibDescriptorBoundEffectiveIDExecutableAccessQueryRunSchemaTag =
  ascii $ concat
    [ "djex-length-spine-pair-z3-capability-probed-effective-id-executable-"
    , "access-sealed-main-image-worker-query-run/v1"
    ]

lengthSpinePairSMTLibDescriptorBoundEffectiveIDExecutableAccessBudgetedQueryRunSchemaTag
  :: [Word8]
lengthSpinePairSMTLibDescriptorBoundEffectiveIDExecutableAccessBudgetedQueryRunSchemaTag =
  ascii $ concat
    [ "djex-length-spine-pair-z3-capability-probed-effective-id-executable-"
    , "access-sealed-main-image-worker-query-run/"
    , "shared-usable-work-deadline/v1"
    ]

lengthSpinePairSMTLibDescriptorBoundEffectiveIDExecutableAccessScopedBudgetedQueryRunSchemaTag
  :: [Word8]
lengthSpinePairSMTLibDescriptorBoundEffectiveIDExecutableAccessScopedBudgetedQueryRunSchemaTag =
  ascii $ concat
    [ "djex-length-spine-pair-z3-capability-probed-effective-id-executable-"
    , "access-sealed-main-image-worker-query-run/"
    , "scoped-shared-usable-work-deadline/v1"
    ]

lengthSpinePairSMTLibDescriptorBoundExecveCheckExecutableAccessQueryRunSchemaTag
  :: [Word8]
lengthSpinePairSMTLibDescriptorBoundExecveCheckExecutableAccessQueryRunSchemaTag =
  ascii $ concat
    [ "djex-length-spine-pair-z3-capability-probed-execve-check-executable-"
    , "access-sealed-main-image-worker-query-run/v1"
    ]

lengthSpinePairSMTLibDescriptorBoundExecveCheckExecutableAccessBudgetedQueryRunSchemaTag
  :: [Word8]
lengthSpinePairSMTLibDescriptorBoundExecveCheckExecutableAccessBudgetedQueryRunSchemaTag =
  ascii $ concat
    [ "djex-length-spine-pair-z3-capability-probed-execve-check-executable-"
    , "access-sealed-main-image-worker-query-run/"
    , "shared-usable-work-deadline/v1"
    ]

lengthSpinePairSMTLibDescriptorBoundExecveCheckExecutableAccessScopedBudgetedQueryRunSchemaTag
  :: [Word8]
lengthSpinePairSMTLibDescriptorBoundExecveCheckExecutableAccessScopedBudgetedQueryRunSchemaTag =
  ascii $ concat
    [ "djex-length-spine-pair-z3-capability-probed-execve-check-executable-"
    , "access-sealed-main-image-worker-query-run/"
    , "scoped-shared-usable-work-deadline/v1"
    ]

-- | Platform-specific schema tag for the fresh working directory, bound in
-- the ready-worker identity next to the workspace path.  It names how the
-- directory is created and verified (an exclusive POSIX 0700 directory
-- tracked through an open descriptor, or a Windows path-only observation) and
-- that cleanup only ever removes it as an empty directory.
lengthSMTLibSessionWorkspaceSchemaTag :: [Word8]
#ifndef mingw32_HOST_OS
lengthSMTLibSessionWorkspaceSchemaTag =
  ascii $ concat
    [ "exclusive-posix-0700-fd-identity-observed/"
    , "pre-spawn-path-observed/empty-only-rmdir/v3"
    ]
#else
lengthSMTLibSessionWorkspaceSchemaTag =
  ascii $ concat
    [ "exclusive-windows-inherited-acl/path-observed/"
    , "no-stable-file-id/empty-only-rmdir/v3"
    ]
#endif

-- | Identifies which session limit a 'LengthSMTLibSessionConfigError' refers
-- to.  Constructor order matches the field order of
-- 'LengthSMTLibSessionLimitSource' and the order in which
-- 'mkLengthSMTLibSessionLimits' validates the fields.
data LengthSMTLibSessionLimitField
  = LengthSMTLibSessionOpenerDeadlineMilliseconds
  | LengthSMTLibSessionWorkspaceAllocationAttempts
  | LengthSMTLibSessionMaximumQueries
  | LengthSMTLibSessionIdentityFingerprintBytes
  | LengthSMTLibSessionQueryRunIdentityFingerprintBytes
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Raw, unvalidated session limits: the opener deadline in milliseconds,
-- how many exclusive workspace-creation attempts may collide before
-- allocation gives up, the maximum number of queries one worker admits
-- (shared between scalar and spine-pair runs), and the byte caps for the
-- ready-worker identity fingerprint and for each query-run identity
-- fingerprint.  Validate with 'mkLengthSMTLibSessionLimits'.
data LengthSMTLibSessionLimitSource = LengthSMTLibSessionLimitSource
  { lengthSMTLibSessionLimitSourceOpenerDeadlineMilliseconds :: !Natural
  , lengthSMTLibSessionLimitSourceWorkspaceAllocationAttempts :: !Natural
  , lengthSMTLibSessionLimitSourceMaximumQueries :: !Natural
  , lengthSMTLibSessionLimitSourceIdentityFingerprintBytes :: !Natural
  , lengthSMTLibSessionLimitSourceQueryRunIdentityFingerprintBytes :: !Natural
  }
  deriving (Eq, Ord, Show)

instance NFData LengthSMTLibSessionLimitSource where
  rnf source =
    rnf (lengthSMTLibSessionLimitSourceOpenerDeadlineMilliseconds source) `seq`
    rnf (lengthSMTLibSessionLimitSourceWorkspaceAllocationAttempts source) `seq`
    rnf (lengthSMTLibSessionLimitSourceMaximumQueries source) `seq`
    rnf (lengthSMTLibSessionLimitSourceIdentityFingerprintBytes source) `seq`
    rnf (lengthSMTLibSessionLimitSourceQueryRunIdentityFingerprintBytes source)

-- | Default raw limits: a 5000 ms opener deadline, 8 workspace-allocation
-- attempts, 64 queries per worker, 262144 ready-identity fingerprint bytes,
-- and 2097152 query-run identity fingerprint bytes.
-- 'mkLengthSMTLibSessionLimits' accepts these values and yields the same
-- limits as 'defaultLengthSMTLibSessionLimits'.
defaultLengthSMTLibSessionLimitSource :: LengthSMTLibSessionLimitSource
defaultLengthSMTLibSessionLimitSource = LengthSMTLibSessionLimitSource
  { lengthSMTLibSessionLimitSourceOpenerDeadlineMilliseconds = 5000
  , lengthSMTLibSessionLimitSourceWorkspaceAllocationAttempts = 8
  , lengthSMTLibSessionLimitSourceMaximumQueries = 64
  , lengthSMTLibSessionLimitSourceIdentityFingerprintBytes = 262144
  , lengthSMTLibSessionLimitSourceQueryRunIdentityFingerprintBytes = 2097152
  }

-- | Validated session limits accepted by 'sealLengthSMTLibSessionConfig'.
-- Every limit is positive and the opener deadline is retained as an 'Int'
-- millisecond count.  The constructor is private; obtain a value through
-- 'mkLengthSMTLibSessionLimits' or 'defaultLengthSMTLibSessionLimits'.
data LengthSMTLibSessionLimits = LengthSMTLibSessionLimits
  !Int !Natural !Natural !Natural !Natural

instance NFData LengthSMTLibSessionLimits where
  rnf (LengthSMTLibSessionLimits deadline attempts queries identity runIdentity) =
    rnf deadline `seq` rnf attempts `seq` rnf queries `seq` rnf identity `seq`
    rnf runIdentity

-- | Why 'mkLengthSMTLibSessionLimits' or 'sealLengthSMTLibSessionConfig'
-- rejected its inputs: a zero limit, a limit too large for its runtime
-- representation, capability limits that cannot admit even a fixed-nonce
-- four-stage plan, or a process stdout limit below the minimum capability
-- output.  Limit failures carry the field and its raw value; the stdout
-- failure carries the configured maximum and the required minimum.
data LengthSMTLibSessionConfigError
  = LengthSMTLibSessionNonPositiveLimit
      !LengthSMTLibSessionLimitField !Natural
  | LengthSMTLibSessionLimitConversionOverflow
      !LengthSMTLibSessionLimitField !Natural
  | LengthSMTLibSessionCapabilityAdmissionFailure
      !LengthSMTLibCapabilityPlanError
  | LengthSMTLibSessionProcessStdoutAdmissionTooSmall !Natural !Natural
  deriving (Eq, Ord, Show)

-- | Validate raw session limits in field order, reporting the first failing
-- field.  Every limit must be positive; in addition the opener deadline must
-- not exceed one thousandth of the largest 'Int' and the query maximum must
-- fit a 'Word64', otherwise a 'LengthSMTLibSessionLimitConversionOverflow'
-- names the field.
mkLengthSMTLibSessionLimits
  :: LengthSMTLibSessionLimitSource
  -> Either LengthSMTLibSessionConfigError LengthSMTLibSessionLimits
mkLengthSMTLibSessionLimits source = do
  deadline <- positiveInt
    LengthSMTLibSessionOpenerDeadlineMilliseconds
    $ lengthSMTLibSessionLimitSourceOpenerDeadlineMilliseconds source
  attempts <- positive LengthSMTLibSessionWorkspaceAllocationAttempts
    $ lengthSMTLibSessionLimitSourceWorkspaceAllocationAttempts source
  queries <- positive LengthSMTLibSessionMaximumQueries
    $ lengthSMTLibSessionLimitSourceMaximumQueries source
  if queries > fromIntegral (maxBound :: Word64)
    then Left $ LengthSMTLibSessionLimitConversionOverflow
      LengthSMTLibSessionMaximumQueries queries
    else Right ()
  identity <- positive LengthSMTLibSessionIdentityFingerprintBytes
    $ lengthSMTLibSessionLimitSourceIdentityFingerprintBytes source
  runIdentity <- positive LengthSMTLibSessionQueryRunIdentityFingerprintBytes
    $ lengthSMTLibSessionLimitSourceQueryRunIdentityFingerprintBytes source
  pure $ LengthSMTLibSessionLimits
    deadline attempts queries identity runIdentity
 where
  positive field value
    | value == 0 = Left $ LengthSMTLibSessionNonPositiveLimit field value
    | otherwise = Right value
  positiveInt field value = do
    retained <- positive field value
    if retained > fromIntegral ((maxBound :: Int) `div` 1000)
      then Left $ LengthSMTLibSessionLimitConversionOverflow field retained
      else Right $ fromIntegral retained

-- | The validated form of 'defaultLengthSMTLibSessionLimitSource': a 5000 ms
-- opener deadline, 8 workspace-allocation attempts, 64 queries per worker,
-- and 262144 / 2097152-byte caps on the ready-worker and query-run identity
-- fingerprints.
defaultLengthSMTLibSessionLimits :: LengthSMTLibSessionLimits
defaultLengthSMTLibSessionLimits =
  LengthSMTLibSessionLimits 5000 8 64 262144 2097152

-- | Sealed inputs for opening one worker: session, process, capability, and
-- protocol limits together with the execution configuration.  Only
-- 'sealLengthSMTLibSessionConfig' constructs it, so a value has already
-- passed the capability-admission and stdout-capacity preflight.
data LengthSMTLibSessionConfig = LengthSMTLibSessionConfig
  !LengthSMTLibSessionLimits
  !LengthSMTLibProcessLimits
  !LengthSMTLibCapabilityLimits
  !LengthSMTLibProtocolLimits
  !LengthSMTLibExecutionConfig

-- | Seal all pure inputs and prove that the configured capability bounds can
-- admit at least one complete four-stage plan.  Fixed test nonces are used
-- only for admission; live barriers are derived from a fresh session epoch.
sealLengthSMTLibSessionConfig
  :: LengthSMTLibSessionLimits
  -> LengthSMTLibProcessLimits
  -> LengthSMTLibCapabilityLimits
  -> LengthSMTLibProtocolLimits
  -> LengthSMTLibExecutionConfig
  -> Either LengthSMTLibSessionConfigError LengthSMTLibSessionConfig
sealLengthSMTLibSessionConfig session process capability protocol execution = do
  let transportMaximum = lengthSMTLibProcessStdoutByteLimit process
      capabilityMinimum = lengthSMTLibCapabilityMinimumOutputByteCount
  if transportMaximum < capabilityMinimum
    then Left $ LengthSMTLibSessionProcessStdoutAdmissionTooSmall
      transportMaximum capabilityMinimum
    else Right ()
  let preflight :: Either
        LengthSMTLibCapabilityPlanError
        (LengthSMTLibCapabilityPlan ())
      preflight = sealLengthSMTLibCapabilityPlan capability
        (replicate 32 0) (replicate 32 1)
        (replicate 32 2) (replicate 32 3)
  case preflight of
    Left failure -> Left $ LengthSMTLibSessionCapabilityAdmissionFailure failure
    Right _ -> Right $ LengthSMTLibSessionConfig
      session process capability protocol execution

-- | Why the fresh working directory could not be allocated, verified, or
-- removed.  Allocation failures cover the temporary-directory lookup, OS
-- entropy, exhausted collision retries, and directory creation; the
-- postcondition failure means the directory did not verify as the owned,
-- empty, non-symlink directory that was created; the last two describe
-- cleanup (removal failed, or the child process was not confirmed released
-- so the pathname was retained).
data LengthSMTLibSessionWorkspaceFailure
  = LengthSMTLibSessionTemporaryDirectoryUnavailable
  | LengthSMTLibSessionTemporaryDirectoryNotAbsolute
  | LengthSMTLibSessionEntropyUnavailable
  | LengthSMTLibSessionEntropyLengthMismatch !Natural !Natural
  | LengthSMTLibSessionWorkspaceCollisionLimitExceeded !Natural
  | LengthSMTLibSessionWorkspaceCreationFailed
  | LengthSMTLibSessionWorkspacePostconditionFailed
  | LengthSMTLibSessionWorkspaceRemovalFailed
  | LengthSMTLibSessionWorkspaceProcessCleanupIncomplete
  deriving (Eq, Ord, Show)

-- | Final disposition of the workspace directory.
-- 'LengthSMTLibSessionWorkspaceRemoved' is the only successful outcome once a
-- directory exists; 'LengthSMTLibSessionWorkspaceRetained' means the pathname
-- was deliberately left in place for the stated reason (for example while the
-- child's release was unconfirmed), and
-- 'LengthSMTLibSessionWorkspaceCleanupIncomplete' means removal or descriptor
-- release itself failed.
data LengthSMTLibSessionWorkspaceCleanupStatus
  = LengthSMTLibSessionWorkspaceNotAllocated
  | LengthSMTLibSessionWorkspaceRemoved !Natural
  | LengthSMTLibSessionWorkspaceRetained
      !LengthSMTLibSessionWorkspaceFailure !(Maybe Natural)
  | LengthSMTLibSessionWorkspaceCleanupIncomplete
      !LengthSMTLibSessionWorkspaceFailure
  deriving (Eq, Ord, Show)

-- | Combined cleanup outcome of one worker scope: the process cleanup status
-- when a process was opened, whether process cleanup threw, and the
-- workspace disposition.  A scope whose callback returned normally still
-- fails with 'LengthSMTLibSessionCleanupFailure' unless the process closed
-- without escalation or failures, its readers stopped, and the workspace was
-- removed.
data LengthSMTLibSessionCleanupStatus = LengthSMTLibSessionCleanupStatus
  { lengthSMTLibSessionProcessCleanupStatus
      :: !(Maybe LengthSMTLibProcessCleanupStatus)
  , lengthSMTLibSessionProcessCleanupThrew :: !Bool
  , lengthSMTLibSessionWorkspaceCleanupStatus
      :: !LengthSMTLibSessionWorkspaceCleanupStatus
  }
  deriving (Eq, Ord, Show)

-- | Primary failure of a worker scope, from the opener deadline through
-- workspace allocation, capability planning and probing, process launch,
-- barrier derivation, ready-point transcript accounting, identity fingerprint
-- sizing, final cleanup, and scoped-deadline admission.  It is paired with
-- the cleanup outcome in 'LengthSMTLibSessionScopeError'.
data LengthSMTLibSessionError
  = LengthSMTLibSessionDeadlineFailure !LengthSMTLibProcessError
  | LengthSMTLibSessionWorkspaceFailure !LengthSMTLibSessionWorkspaceFailure
  | LengthSMTLibSessionCapabilityPlanFailure
      !LengthSMTLibCapabilityPlanError
  | LengthSMTLibSessionProcessFailure !LengthSMTLibProcessError
  | LengthSMTLibSessionCapabilityFailure !LengthSMTLibCapabilityError
  | LengthSMTLibSessionBarrierDerivationCollision
  | LengthSMTLibSessionTranscriptAccountingMismatch !Natural !Natural
  | LengthSMTLibSessionIdentityFingerprintByteLimitExceeded !Natural !Natural
  | LengthSMTLibSessionCleanupFailure
  | LengthSMTLibSessionUsableWorkScopeUnavailable
  deriving (Eq, Ord, Show)

-- | What a worker scope returns on failure: the primary
-- 'LengthSMTLibSessionError' and the cleanup status reached before
-- returning.  Callback exceptions are not converted into this type; they are
-- rethrown after cleanup has been started.
data LengthSMTLibSessionScopeError = LengthSMTLibSessionScopeError
  { lengthSMTLibSessionScopePrimaryError :: !LengthSMTLibSessionError
  , lengthSMTLibSessionScopeCleanupStatus :: !LengthSMTLibSessionCleanupStatus
  }
  deriving (Eq, Ord, Show)

-- | One generative absolute monotonic deadline shared by session opening and
-- every query admitted beneath it.  The constructor and projections remain
-- package-private so a caller can neither forge nor extend usable work.
data LengthSMTLibSessionUsableWorkDeadline budget =
  LengthSMTLibSessionUsableWorkDeadline
    !Int
    !LengthSMTLibProcessDeadline

type role LengthSMTLibSessionUsableWorkDeadline nominal

-- | Capture one shared deadline before any eager session work.  This owner
-- does not interrupt arbitrary callback IO, but it rejects a normally
-- returning callback after expiry.  An overrun can therefore be observed by
-- the next live operation, by a budgeted session's callback-return check, or
-- by this outer owner when the callback returns without another live operation.
withLengthSMTLibSessionUsableWorkDeadline
  :: forall result. Int
  -> (forall budget. LengthSMTLibSessionUsableWorkDeadline budget -> IO result)
  -> IO (Either LengthSMTLibSessionError result)
withLengthSMTLibSessionUsableWorkDeadline =
  withLengthSMTLibSessionUsableWorkDeadlineOwner True

-- | Capture the same generative token when the callback immediately delegates
-- ownership to 'withLengthSMTLibReadyWorkerUnderDeadline'.  That session
-- checks expiry as soon as its user callback returns, before its fresh final
-- readiness and cleanup windows.  Omitting a second owner check here prevents
-- those excluded windows from being charged to usable work.
withLengthSMTLibSessionUsableWorkDeadlineForBudgetedSession
  :: forall result. Int
  -> (forall budget. LengthSMTLibSessionUsableWorkDeadline budget -> IO result)
  -> IO (Either LengthSMTLibSessionError result)
withLengthSMTLibSessionUsableWorkDeadlineForBudgetedSession =
  withLengthSMTLibSessionUsableWorkDeadlineOwner False

withLengthSMTLibSessionUsableWorkDeadlineOwner
  :: forall result. Bool
  -> Int
  -> (forall budget. LengthSMTLibSessionUsableWorkDeadline budget -> IO result)
  -> IO (Either LengthSMTLibSessionError result)
withLengthSMTLibSessionUsableWorkDeadlineOwner checkOnReturn milliseconds use = do
  created <- lengthSMTLibProcessDeadlineAfterMilliseconds milliseconds
  case created of
    Left failure -> pure $ Left $ LengthSMTLibSessionDeadlineFailure failure
    Right deadline -> do
      result <- use
        $ LengthSMTLibSessionUsableWorkDeadline milliseconds deadline
      if checkOnReturn
        then do
          completed <- checkLengthSMTLibProcessDeadline deadline
          pure $ case completed of
            Left failure -> Left $ LengthSMTLibSessionDeadlineFailure failure
            Right () -> Right result
        else pure $ Right result

-- | Runtime scope state for the additive owner-thread-affine deadline.  The
-- thread check rejects forked use while this state is open; the closed state
-- rejects an action closure invoked later on the original owner thread.
data LengthSMTLibSessionScopedUsableWorkState
  = LengthSMTLibSessionScopedUsableWorkOpen
  | LengthSMTLibSessionScopedUsableWorkClosed
  deriving (Eq)

-- | A v2 shared deadline with an exact dynamic-use boundary in addition to
-- its generative phantom.  Neither the owner thread nor the state is an
-- execution-identity field.
data LengthSMTLibSessionScopedUsableWorkDeadline budget =
  LengthSMTLibSessionScopedUsableWorkDeadline
    !Int
    !LengthSMTLibProcessDeadline
    !ThreadId
    !(TVar LengthSMTLibSessionScopedUsableWorkState)

type role LengthSMTLibSessionScopedUsableWorkDeadline nominal

-- | Capture a v2 deadline and close its runtime admission on every callback
-- exit.  A normal result is accepted only when the same absolute deadline is
-- still live after the scope has been closed.
withLengthSMTLibSessionScopedUsableWorkDeadline
  :: forall result. Int
  -> (forall budget.
        LengthSMTLibSessionScopedUsableWorkDeadline budget -> IO result)
  -> IO (Either LengthSMTLibSessionError result)
withLengthSMTLibSessionScopedUsableWorkDeadline =
  withLengthSMTLibSessionScopedUsableWorkDeadlineOwner True

-- | Scoped sibling used when one nested ready-worker session owns the final
-- usable-work observation before its fresh final-readiness and cleanup
-- windows.  The token is still closed after the nested session returns, but
-- those excluded windows do not trigger a second shared-deadline failure.
withLengthSMTLibSessionScopedUsableWorkDeadlineForBudgetedSession
  :: forall result. Int
  -> (forall budget.
        LengthSMTLibSessionScopedUsableWorkDeadline budget -> IO result)
  -> IO (Either LengthSMTLibSessionError result)
withLengthSMTLibSessionScopedUsableWorkDeadlineForBudgetedSession =
  withLengthSMTLibSessionScopedUsableWorkDeadlineOwner False

withLengthSMTLibSessionScopedUsableWorkDeadlineOwner
  :: forall result. Bool
  -> Int
  -> (forall budget.
        LengthSMTLibSessionScopedUsableWorkDeadline budget -> IO result)
  -> IO (Either LengthSMTLibSessionError result)
withLengthSMTLibSessionScopedUsableWorkDeadlineOwner
    checkOnReturn milliseconds use = mask $ \restore -> do
  created <- lengthSMTLibProcessDeadlineAfterMilliseconds milliseconds
  case created of
    Left failure -> pure $ Left $ LengthSMTLibSessionDeadlineFailure failure
    Right deadline -> do
      owner <- myThreadId
      state <- newTVarIO LengthSMTLibSessionScopedUsableWorkOpen
      let scoped = LengthSMTLibSessionScopedUsableWorkDeadline
            milliseconds deadline owner state
          close = atomically $ writeTVar state
            LengthSMTLibSessionScopedUsableWorkClosed
      result <- restore (use scoped) `onException` close
      close
      if checkOnReturn
        then do
          completed <- checkLengthSMTLibProcessDeadline deadline
          pure $ case completed of
            Left failure -> Left $ LengthSMTLibSessionDeadlineFailure failure
            Right () -> Right result
        else pure $ Right result

-- | Observe the v2 deadline without refreshing it.  Owner-thread and open
-- state admission precede the clock read, so stale or forked authority is
-- rejected even when the underlying absolute deadline has also expired.
checkLengthSMTLibSessionScopedUsableWorkDeadline
  :: LengthSMTLibSessionScopedUsableWorkDeadline budget
  -> IO (Either LengthSMTLibSessionError ())
checkLengthSMTLibSessionScopedUsableWorkDeadline
    (LengthSMTLibSessionScopedUsableWorkDeadline
      _ deadline owner state) = do
  current <- myThreadId
  if current /= owner
    then pure $ Left LengthSMTLibSessionUsableWorkScopeUnavailable
    else do
      mode <- readTVarIO state
      case mode of
        LengthSMTLibSessionScopedUsableWorkClosed -> pure $ Left
          LengthSMTLibSessionUsableWorkScopeUnavailable
        LengthSMTLibSessionScopedUsableWorkOpen -> do
          checked <- checkLengthSMTLibProcessDeadline deadline
          pure $ case checked of
            Left failure -> Left $ LengthSMTLibSessionDeadlineFailure failure
            Right () -> Right ()

data LengthSMTLibUsableWorkDeadlinePolicy
  = LengthSMTLibFreshPerQueryDeadline
  | LengthSMTLibSharedUsableWorkDeadline
      !Int
      !LengthSMTLibProcessDeadline
  | LengthSMTLibScopedSharedUsableWorkDeadline
      !Int
      !LengthSMTLibProcessDeadline

data LengthSMTLibEffectiveDeadlineCause
  = LengthSMTLibEffectivePerQueryDeadline
  | LengthSMTLibEffectiveSharedUsableWorkDeadline

data LengthSMTLibEffectiveDeadline = LengthSMTLibEffectiveDeadline
  !LengthSMTLibEffectiveDeadlineCause
  !LengthSMTLibProcessDeadline

effectiveLengthSMTLibQueryDeadline
  :: LengthSMTLibUsableWorkDeadlinePolicy
  -> LengthSMTLibProcessDeadline
  -> LengthSMTLibEffectiveDeadline
effectiveLengthSMTLibQueryDeadline policy queryDeadline = case policy of
  LengthSMTLibFreshPerQueryDeadline -> LengthSMTLibEffectiveDeadline
    LengthSMTLibEffectivePerQueryDeadline queryDeadline
  LengthSMTLibSharedUsableWorkDeadline _ sharedDeadline ->
    case compareLengthSMTLibProcessDeadline sharedDeadline queryDeadline of
      LT -> shared
      EQ -> shared
      GT -> LengthSMTLibEffectiveDeadline
        LengthSMTLibEffectivePerQueryDeadline queryDeadline
   where
    shared = LengthSMTLibEffectiveDeadline
      LengthSMTLibEffectiveSharedUsableWorkDeadline
      $ minimumLengthSMTLibProcessDeadline sharedDeadline queryDeadline
  LengthSMTLibScopedSharedUsableWorkDeadline _ sharedDeadline ->
    case compareLengthSMTLibProcessDeadline sharedDeadline queryDeadline of
      LT -> shared
      EQ -> shared
      GT -> LengthSMTLibEffectiveDeadline
        LengthSMTLibEffectivePerQueryDeadline queryDeadline
   where
    shared = LengthSMTLibEffectiveDeadline
      LengthSMTLibEffectiveSharedUsableWorkDeadline
      $ minimumLengthSMTLibProcessDeadline sharedDeadline queryDeadline

-- Select by configured duration before constructing a fresh local absolute
-- deadline.  A validated shared duration no greater than the local duration
-- necessarily wins (including an exact tie), so constructing the later local
-- candidate would add only an avoidable conversion-overflow failure.  When the
-- local duration is shorter, it is representable because it is strictly below
-- the already validated shared duration; elapsed time can still make the
-- shared absolute deadline earlier, so the final selection compares absolutes.
-- At monotonic-clock saturation, adding even that shorter duration can fail
-- although the previously captured shared absolute deadline remains valid;
-- only that exact deadline-phase conversion overflow falls back to shared.
effectiveLengthSMTLibDeadlineAfterMilliseconds
  :: LengthSMTLibUsableWorkDeadlinePolicy
  -> Int
  -> IO
      (Either
        LengthSMTLibProcessError
        LengthSMTLibEffectiveDeadline)
effectiveLengthSMTLibDeadlineAfterMilliseconds policy localMilliseconds =
  case policy of
    LengthSMTLibFreshPerQueryDeadline -> do
      created <- lengthSMTLibProcessDeadlineAfterMilliseconds localMilliseconds
      pure $ LengthSMTLibEffectiveDeadline
        LengthSMTLibEffectivePerQueryDeadline <$> created
    LengthSMTLibSharedUsableWorkDeadline
        sharedMilliseconds sharedDeadline
      | sharedMilliseconds <= localMilliseconds -> pure $ Right
          $ LengthSMTLibEffectiveDeadline
              LengthSMTLibEffectiveSharedUsableWorkDeadline sharedDeadline
      | otherwise -> do
          created <-
            lengthSMTLibProcessDeadlineAfterMilliseconds localMilliseconds
          pure $ case created of
            Left failure
              | lengthSMTLibProcessErrorPhase failure ==
                    LengthSMTLibProcessDeadlinePhase &&
                  lengthSMTLibProcessErrorClass failure ==
                    LengthSMTLibProcessLimitConversionOverflow -> Right
                      $ LengthSMTLibEffectiveDeadline
                          LengthSMTLibEffectiveSharedUsableWorkDeadline
                          sharedDeadline
              | otherwise -> Left failure
            Right localDeadline -> Right
              $ effectiveLengthSMTLibQueryDeadline policy localDeadline
    LengthSMTLibScopedSharedUsableWorkDeadline
        sharedMilliseconds sharedDeadline
      | sharedMilliseconds <= localMilliseconds -> pure $ Right
          $ LengthSMTLibEffectiveDeadline
              LengthSMTLibEffectiveSharedUsableWorkDeadline sharedDeadline
      | otherwise -> do
          created <-
            lengthSMTLibProcessDeadlineAfterMilliseconds localMilliseconds
          pure $ case created of
            Left failure
              | lengthSMTLibProcessErrorPhase failure ==
                    LengthSMTLibProcessDeadlinePhase &&
                  lengthSMTLibProcessErrorClass failure ==
                    LengthSMTLibProcessLimitConversionOverflow -> Right
                      $ LengthSMTLibEffectiveDeadline
                          LengthSMTLibEffectiveSharedUsableWorkDeadline
                          sharedDeadline
              | otherwise -> Left failure
            Right localDeadline -> Right
              $ effectiveLengthSMTLibQueryDeadline policy localDeadline

-- | Primary reason a scalar query run failed, from lease admission (worker
-- closing or spent, query maximum reached) through protocol planning,
-- barrier reservation, deadline, process, protocol, transcript and stderr
-- accounting, counterexample replay, and run-identity sizing.  These are
-- package-private diagnostics and may retain bounded child bytes or integer
-- values.
data LengthSMTLibQueryRunFailure
  = LengthSMTLibQueryWorkerClosing
  | LengthSMTLibQueryWorkerSpent
  | LengthSMTLibQueryLimitExceeded !Natural !Natural
  | LengthSMTLibQueryProtocolPlanFailure !LengthSMTLibProtocolPlanError
  | LengthSMTLibQueryProcessStdoutCapacityTooSmall !Natural !Natural
  | LengthSMTLibQueryBarrierCollision
  | LengthSMTLibQueryDeadlineFailure !LengthSMTLibProcessError
  | LengthSMTLibQueryProcessFailure !LengthSMTLibProcessError
  | LengthSMTLibQueryProtocolFailure !LengthSMTLibProtocolError
  | LengthSMTLibQueryTranscriptAccountingMismatch !Natural !Natural
  | LengthSMTLibQueryStderrAccountingMismatch !Natural !Natural
  | LengthSMTLibQueryModelFailure !LengthSMTLibModelError
  | LengthSMTLibQueryModelNotCounterexample
  | LengthSMTLibQueryRunIdentityAdmissionTooSmall !Natural !Natural
  | LengthSMTLibQueryRunIdentityFingerprintByteLimitExceeded
      !Natural !Natural
  | LengthSMTLibQueryInternalFailure
  deriving (Eq, Ord, Show)

-- | Failure returned by 'runLengthSMTLibReadyWorkerQuery': the primary
-- failure plus the process cleanup status when the failure spent the worker
-- (its lease closed and the process cancelled and closed).  The status is
-- 'Nothing' when the worker remains usable for further queries.
data LengthSMTLibQueryRunError = LengthSMTLibQueryRunError
  { lengthSMTLibQueryRunPrimaryFailure :: !LengthSMTLibQueryRunFailure
  , lengthSMTLibQueryRunProcessCleanupStatus
      :: !(Maybe LengthSMTLibProcessCleanupStatus)
  }
  deriving (Eq, Ord, Show)

data QueryLeaseMode
  = QueryLeaseAccepting
  | QueryLeaseClosing
  | QueryLeaseSpent
  deriving (Eq)

data QueryLeaseState = QueryLeaseState
  !QueryLeaseMode
  !Natural
  !(Set ByteString)
  !(Maybe Natural)
  !Natural
  !Natural

-- | Phantom fingerprint subject of a scalar query-run identity, keeping it
-- distinct from ready-worker and spine-pair run fingerprints.
data LengthSMTLibQueryRunIdentitySubject

-- | One successfully delimited live query and its independent Length replay.
-- Decoded bindings remain local through replay and complete run-identity
-- construction, then the committed run retains one strict status-indexed
-- observation: only its satisfiable branch can carry optional problem-bound
-- evidence. Its private reversible identity still contains the exact causal
-- transcript, so this is
-- structured-authority narrowing rather than byte scrubbing.  The run remains
-- a capability-probed pathname-snapshot observation, not an executable-image
-- attestation or a proof of solver soundness.
type LengthSMTLibQueryRunObservation = SolverObservation
  (Maybe
    (BehavioralEvidence
      FiniteListSpineLengthV1
      ValidatedLengthCounterexample))
  ()
  ()

-- | One committed scalar query run: its zero-based ordinal in the worker's
-- lease, the replayed status-indexed observation, its private identity
-- fingerprint, the SHA-256 of the causal transcript, and the cumulative
-- stdout and stderr byte boundaries at which it started and ended.  All
-- three phantoms are nominal, tying the run to its worker epoch and to the
-- query's identity and local scopes; only the @lengthSMTLibQueryRun*@
-- projections are exported.
data LengthSMTLibQueryRun epoch identity local = LengthSMTLibQueryRun
  !Natural
  !LengthSMTLibQueryRunObservation
  !(Fingerprint LengthSMTLibQueryRunIdentitySubject)
  !ByteString
  !Natural
  !Natural
  !Natural
  !Natural

type role LengthSMTLibQueryRun nominal nominal nominal

instance NFData (LengthSMTLibQueryRun epoch identity local) where
  rnf (LengthSMTLibQueryRun ordinal observation identity digest
      stdoutStart stdoutEnd stderrStart stderrEnd) =
    rnf ordinal `seq` rnf observation `seq` rnf identity `seq`
    rnf digest `seq` rnf stdoutStart `seq` rnf stdoutEnd `seq` rnf stderrStart
      `seq` rnf stderrEnd

-- | Nominal product-query sibling of the scalar run failures.  These retain
-- package-private diagnostics only; the public facade maps them to bounded,
-- byte-free classes.
data LengthSpinePairSMTLibQueryRunFailure
  = LengthSpinePairSMTLibQueryWorkerClosing
  | LengthSpinePairSMTLibQueryWorkerSpent
  | LengthSpinePairSMTLibQueryLimitExceeded !Natural !Natural
  | LengthSpinePairSMTLibQueryProtocolPlanFailure
      !LengthSpinePairSMTLibProtocolPlanError
  | LengthSpinePairSMTLibQueryProcessStdoutCapacityTooSmall !Natural !Natural
  | LengthSpinePairSMTLibQueryBarrierCollision
  | LengthSpinePairSMTLibQueryDeadlineFailure !LengthSMTLibProcessError
  | LengthSpinePairSMTLibQueryProcessFailure !LengthSMTLibProcessError
  | LengthSpinePairSMTLibQueryProtocolFailure
      !LengthSpinePairSMTLibProtocolError
  | LengthSpinePairSMTLibQueryTranscriptAccountingMismatch !Natural !Natural
  | LengthSpinePairSMTLibQueryStderrAccountingMismatch !Natural !Natural
  | LengthSpinePairSMTLibQueryModelFailure
      !LengthSpinePairSMTLibModelError
  | LengthSpinePairSMTLibQueryModelNotCounterexample
  | LengthSpinePairSMTLibQueryRunIdentityAdmissionTooSmall !Natural !Natural
  | LengthSpinePairSMTLibQueryRunIdentityFingerprintByteLimitExceeded
      !Natural !Natural
  | LengthSpinePairSMTLibQueryInternalFailure
  deriving (Eq, Ord, Show)

-- | Failure returned by 'runLengthSpinePairSMTLibReadyWorkerQuery': the
-- primary failure plus the process cleanup status when the failure spent the
-- shared worker (its lease closed and the process cancelled and closed).  The
-- status is 'Nothing' when the worker remains usable.
data LengthSpinePairSMTLibQueryRunError = LengthSpinePairSMTLibQueryRunError
  { lengthSpinePairSMTLibQueryRunPrimaryFailure
      :: !LengthSpinePairSMTLibQueryRunFailure
  , lengthSpinePairSMTLibQueryRunProcessCleanupStatus
      :: !(Maybe LengthSMTLibProcessCleanupStatus)
  }
  deriving (Eq, Ord, Show)

-- | Phantom fingerprint subject of a spine-pair query-run identity, keeping
-- it distinct from scalar run and ready-worker fingerprints.
data LengthSpinePairSMTLibQueryRunIdentitySubject

-- | Binary-product sibling of 'LengthSMTLibQueryRunObservation': a strict
-- status-indexed observation whose satisfiable branch alone may carry
-- independently replayed spine-pair counterexample evidence.
type LengthSpinePairSMTLibQueryRunObservation = SolverObservation
  (Maybe
    (BehavioralEvidence
      FiniteBinaryProductSpineLengthsV1
      ValidatedLengthSpinePairCounterexample))
  ()
  ()

-- | One committed spine-pair query run on the shared ready worker, with the
-- same shape as 'LengthSMTLibQueryRun': ordinal, replayed observation,
-- private identity fingerprint, transcript SHA-256, and the cumulative stdout
-- and stderr boundaries of the run.  Ordinals are drawn from the same lease as
-- scalar runs; only the @lengthSpinePairSMTLibQueryRun*@ projections are
-- exported.
data LengthSpinePairSMTLibQueryRun epoch identity local =
  LengthSpinePairSMTLibQueryRun
    !Natural
    !LengthSpinePairSMTLibQueryRunObservation
    !(Fingerprint LengthSpinePairSMTLibQueryRunIdentitySubject)
    !ByteString
    !Natural
    !Natural
    !Natural
    !Natural

type role LengthSpinePairSMTLibQueryRun nominal nominal nominal

instance NFData
    (LengthSpinePairSMTLibQueryRun epoch identity local) where
  rnf (LengthSpinePairSMTLibQueryRun ordinal observation identity digest
      stdoutStart stdoutEnd stderrStart stderrEnd) =
    rnf ordinal `seq` rnf observation `seq` rnf identity `seq`
    rnf digest `seq` rnf stdoutStart `seq` rnf stdoutEnd `seq` rnf stderrStart
      `seq` rnf stderrEnd

-- | Phantom fingerprint subject of a ready-worker identity, keeping it
-- distinct from query-run fingerprints.
data LengthSMTLibReadyWorkerIdentitySubject

-- | Exact policy the worker itself still needs after capability admission.
-- Workspace-allocation, capability, and ready-identity bounds have completed;
-- the opener deadline and Session workspace-cleanup authority remain in the
-- enclosing scope, while process limits remain associated with the process.
-- The nested execution authority is already narrowed past its structured Z3
-- launch profile before this query policy can be constructed.
data LengthSMTLibReadyWorkerQueryPolicy = LengthSMTLibReadyWorkerQueryPolicy
  { readyQueryMaximumQueries :: !Natural
  , readyQueryRunIdentityFingerprintByteLimit :: !Natural
  , readyQueryProtocolLimits :: !LengthSMTLibProtocolLimits
  , readyQueryPostLaunchExecution :: !LengthSMTLibPostLaunchExecutionPolicy
  , readyQueryUsableWorkDeadlinePolicy
      :: !LengthSMTLibUsableWorkDeadlinePolicy
  }

retainLengthSMTLibReadyWorkerQueryPolicy
  :: LengthSMTLibSessionLimits
  -> LengthSMTLibProtocolLimits
  -> LengthSMTLibPostLaunchExecutionPolicy
  -> LengthSMTLibReadyWorkerQueryPolicy
retainLengthSMTLibReadyWorkerQueryPolicy
    (LengthSMTLibSessionLimits _ _ maximumQueries _ runIdentityLimit)
    protocolLimits postLaunchExecution = LengthSMTLibReadyWorkerQueryPolicy
      maximumQueries runIdentityLimit protocolLimits postLaunchExecution
      LengthSMTLibFreshPerQueryDeadline

retainLengthSMTLibReadyWorkerQueryPolicyUnderDeadline
  :: LengthSMTLibSessionLimits
  -> LengthSMTLibProtocolLimits
  -> LengthSMTLibPostLaunchExecutionPolicy
  -> Int
  -> LengthSMTLibProcessDeadline
  -> LengthSMTLibReadyWorkerQueryPolicy
retainLengthSMTLibReadyWorkerQueryPolicyUnderDeadline
    (LengthSMTLibSessionLimits _ _ maximumQueries _ runIdentityLimit)
    protocolLimits postLaunchExecution milliseconds deadline =
      LengthSMTLibReadyWorkerQueryPolicy
        maximumQueries runIdentityLimit protocolLimits postLaunchExecution
        $ LengthSMTLibSharedUsableWorkDeadline milliseconds deadline

retainLengthSMTLibReadyWorkerQueryPolicyUnderScopedDeadline
  :: LengthSMTLibSessionLimits
  -> LengthSMTLibProtocolLimits
  -> LengthSMTLibPostLaunchExecutionPolicy
  -> Int
  -> LengthSMTLibProcessDeadline
  -> LengthSMTLibReadyWorkerQueryPolicy
retainLengthSMTLibReadyWorkerQueryPolicyUnderScopedDeadline
    (LengthSMTLibSessionLimits _ _ maximumQueries _ runIdentityLimit)
    protocolLimits postLaunchExecution milliseconds deadline =
      LengthSMTLibReadyWorkerQueryPolicy
        maximumQueries runIdentityLimit protocolLimits postLaunchExecution
        $ LengthSMTLibScopedSharedUsableWorkDeadline milliseconds deadline

-- | One capability-probed, ready Z3 worker lent to the callback of
-- 'withLengthSMTLibReadyWorker' or one of its deadline variants.  It owns the
-- process, its cancellation, the retained query policy, the ready identity,
-- the barrier seed, and the serial query lease shared by scalar and
-- spine-pair runs; the nominal @epoch@ phantom keeps a worker from escaping
-- its scope at the type level.  Fields are private; use the
-- @lengthSMTLibReadyWorker*@ projections.
data LengthSMTLibReadyWorker epoch = LengthSMTLibReadyWorker
  { readyWorkerProcess :: !LengthSMTLibProcess
  , readyWorkerCancellation :: !LengthSMTLibProcessCancellation
  , readyWorkerQueryPolicy :: !LengthSMTLibReadyWorkerQueryPolicy
  , readyWorkerIdentity
      :: !(Fingerprint LengthSMTLibReadyWorkerIdentitySubject)
  , readyWorkerTranscriptDigest :: !ByteString
  , readyWorkerStdoutAtCommit :: !Natural
  , readyWorkerStderrAtCommit :: !Natural
  , readyWorkerWorkspace :: FilePath
  , readyWorkerBarrierSeed :: !ByteString
  , readyWorkerQueryState :: !(TVar QueryLeaseState)
  , readyWorkerQueryGate :: !(TMVar ())
  }

type role LengthSMTLibReadyWorker nominal

-- | The private, reversible ready-worker identity computed at the ready
-- point.  It binds the session and launch schema tags, the execution policy,
-- the process observation, the executable snapshot strength, the capability
-- outcome and its exact transcript, the session-epoch commitment, the
-- workspace, protocol and session limits, and the output counts observed at
-- ready commit; under a shared usable-work deadline it additionally wraps
-- that legacy identity with the deadline policy.
lengthSMTLibReadyWorkerIdentityFingerprint
  :: LengthSMTLibReadyWorker epoch
  -> Fingerprint LengthSMTLibReadyWorkerIdentitySubject
lengthSMTLibReadyWorkerIdentityFingerprint = readyWorkerIdentity

-- | The ready-worker identity as a tagged fingerprint field, ready to embed
-- in a dependent fingerprint.  Every query-run identity built on this worker
-- includes exactly this field.
lengthSMTLibReadyWorkerIdentityFingerprintField
  :: LengthSMTLibReadyWorker epoch
  -> FingerprintField
lengthSMTLibReadyWorkerIdentityFingerprintField worker = FingerprintTag
  (ascii "capability-probed-ready-worker-identity")
  [FingerprintBytes $ fingerprintCanonicalBytes $ readyWorkerIdentity worker]

-- | SHA-256 digest of the solver executable image observed at launch: for
-- the portable launch, the file at the configured pathname read before
-- spawning; for descriptor-bound launches, the bytes copied into the sealed
-- image that was executed.  The digest covers the main image only, not the
-- dynamic loader or shared libraries.
lengthSMTLibReadyWorkerExecutableSHA256
  :: LengthSMTLibReadyWorker epoch
  -> ByteString
lengthSMTLibReadyWorkerExecutableSHA256 =
  lengthSMTLibExecutableSnapshotSHA256 .
    lengthSMTLibProcessSnapshot . readyWorkerProcess

-- | Byte count of the same executable snapshot whose digest
-- 'lengthSMTLibReadyWorkerExecutableSHA256' reports.
lengthSMTLibReadyWorkerExecutableByteCount
  :: LengthSMTLibReadyWorker epoch
  -> Natural
lengthSMTLibReadyWorkerExecutableByteCount =
  lengthSMTLibExecutableSnapshotByteCount .
    lengthSMTLibProcessSnapshot . readyWorkerProcess

-- | Tag naming how strongly the executable digest is bound to the running
-- process, chosen from the process launch strategy: the descriptor-bound
-- execve-check tag first, then the effective-ID descriptor-bound tag, then
-- the plain descriptor-bound tag, otherwise the portable pathname-snapshot
-- tag.  The same tag is bound in the ready-worker identity.
lengthSMTLibReadyWorkerExecutableSnapshotStrengthTag
  :: LengthSMTLibReadyWorker epoch
  -> ByteString
lengthSMTLibReadyWorkerExecutableSnapshotStrengthTag worker
  | lengthSMTLibProcessUsesDescriptorBoundExecveCheckExecutableAccessLaunch
      $ readyWorkerProcess worker =
      lengthSMTLibDescriptorBoundExecveCheckExecutableAccessLaunchStrengthTag
  | lengthSMTLibProcessUsesDescriptorBoundEffectiveIDExecutableAccessLaunch
      $ readyWorkerProcess worker =
      lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchStrengthTag
  | lengthSMTLibProcessUsesDescriptorBoundExecutableLaunch
      $ readyWorkerProcess worker =
      lengthSMTLibDescriptorBoundExecutableLaunchStrengthTag
  | otherwise = lengthSMTLibExecutableSnapshotStrengthTag

-- | SHA-256 of the complete causal stdout transcript of the readiness
-- capability probe: the inherited predecessor whitespace followed by every
-- write epoch's bytes, in order.
lengthSMTLibReadyWorkerCapabilityTranscriptSHA256
  :: LengthSMTLibReadyWorker epoch
  -> ByteString
lengthSMTLibReadyWorkerCapabilityTranscriptSHA256 = readyWorkerTranscriptDigest

-- | Byte count of the readiness capability transcript.  It equals the total
-- stdout byte count observed at the ready commit point, because readiness
-- fails with 'LengthSMTLibSessionTranscriptAccountingMismatch' whenever the
-- transcript count and that boundary differ.
lengthSMTLibReadyWorkerCapabilityTranscriptByteCount
  :: LengthSMTLibReadyWorker epoch
  -> Natural
-- Readiness construction admits the worker only after proving the causal
-- transcript count equals this immutable ready-point stdout boundary.
lengthSMTLibReadyWorkerCapabilityTranscriptByteCount =
  readyWorkerStdoutAtCommit

-- | Total stdout bytes the process had produced when the worker was
-- committed ready.  It equals
-- 'lengthSMTLibReadyWorkerCapabilityTranscriptByteCount' and is the stdout
-- start boundary of the first query run.
lengthSMTLibReadyWorkerObservedStdoutBytes
  :: LengthSMTLibReadyWorker epoch
  -> Natural
lengthSMTLibReadyWorkerObservedStdoutBytes = readyWorkerStdoutAtCommit

-- | Total stderr bytes observed when the worker was committed ready, and the
-- stderr start boundary of the first query run.  This is a point-in-time
-- reader observation, not proof that the child never writes to stderr.
lengthSMTLibReadyWorkerObservedStderrBytes
  :: LengthSMTLibReadyWorker epoch
  -> Natural
lengthSMTLibReadyWorkerObservedStderrBytes = readyWorkerStderrAtCommit

-- | Absolute path of the fresh, exclusively created directory the solver
-- process was started in.  It is bound in the ready-worker identity, and
-- scope cleanup removes it only after re-verifying that the pathname still
-- names the directory the scope created, and only when it is empty.
lengthSMTLibReadyWorkerWorkingDirectory
  :: LengthSMTLibReadyWorker epoch
  -> FilePath
lengthSMTLibReadyWorkerWorkingDirectory = readyWorkerWorkspace

-- | Zero-based ordinal of this run in its worker's serial query lease.
-- Ordinals are shared between scalar and spine-pair runs on the same worker
-- and increase by one per reserved run.
lengthSMTLibQueryRunOrdinal
  :: LengthSMTLibQueryRun epoch identity local
  -> Natural
lengthSMTLibQueryRunOrdinal
    (LengthSMTLibQueryRun value _ _ _ _ _ _ _) = value

-- | The retained solver observation: satisfiable (carrying independently
-- replayed counterexample evidence exactly when the artifact policy requested
-- input values), unsatisfiable, or unknown.  No branch asserts solver
-- soundness.
lengthSMTLibQueryRunObservation
  :: LengthSMTLibQueryRun epoch identity local
  -> LengthSMTLibQueryRunObservation
lengthSMTLibQueryRunObservation
    (LengthSMTLibQueryRun _ observation _ _ _ _ _ _) = observation

-- | Private, reversible identity of the run.  It binds the run schema tag,
-- the executable authority, the ready-worker identity, the query allocation
-- (ordinal, both spent barrier markers, and the seed commitment), the
-- protocol plan, the effective deadline and its selection, the exact causal
-- transcript, the decoded outcome and replay policy, and the transport
-- commit boundaries.
lengthSMTLibQueryRunIdentityFingerprint
  :: LengthSMTLibQueryRun epoch identity local
  -> Fingerprint LengthSMTLibQueryRunIdentitySubject
lengthSMTLibQueryRunIdentityFingerprint
    (LengthSMTLibQueryRun _ _ value _ _ _ _ _) = value

-- | The run identity as a tagged fingerprint field for embedding in a
-- dependent fingerprint.
lengthSMTLibQueryRunIdentityFingerprintField
  :: LengthSMTLibQueryRun epoch identity local
  -> FingerprintField
lengthSMTLibQueryRunIdentityFingerprintField run = tagged "query-run-identity"
  [FingerprintBytes $ fingerprintCanonicalBytes
    $ lengthSMTLibQueryRunIdentityFingerprint run]

-- | SHA-256 of the exact causal stdout transcript of this run: the inherited
-- predecessor whitespace followed by every write epoch's bytes, in order.
lengthSMTLibQueryRunTranscriptSHA256
  :: LengthSMTLibQueryRun epoch identity local
  -> ByteString
lengthSMTLibQueryRunTranscriptSHA256
    (LengthSMTLibQueryRun _ _ _ value _ _ _ _) = value

-- | Byte count of the run's causal transcript, computed as the difference
-- between its stdout end and start boundaries.  A run is constructed only
-- after accounting proved that this difference equals the transcript's own
-- byte count, so the subtraction cannot underflow.
lengthSMTLibQueryRunTranscriptByteCount
  :: LengthSMTLibQueryRun epoch identity local
  -> Natural
-- The private constructor is reached only after exact transcript accounting
-- proves that these immutable boundaries are ordered and their delta is the
-- causal transcript count.
lengthSMTLibQueryRunTranscriptByteCount
    (LengthSMTLibQueryRun _ _ _ _ stdoutStart stdoutEnd _ _) =
      stdoutEnd - stdoutStart

-- | Cumulative process stdout byte count at which this run's transport
-- began: the previous committed run's end boundary, or the ready-commit count
-- for the first run.
lengthSMTLibQueryRunStdoutStart
  :: LengthSMTLibQueryRun epoch identity local
  -> Natural
lengthSMTLibQueryRunStdoutStart
    (LengthSMTLibQueryRun _ _ _ _ value _ _ _) = value

-- | Cumulative process stdout byte count observed at the run's final
-- boundary.  It exceeds the start by exactly the transcript byte count and
-- becomes the start boundary of the next committed run.
lengthSMTLibQueryRunStdoutEnd
  :: LengthSMTLibQueryRun epoch identity local
  -> Natural
lengthSMTLibQueryRunStdoutEnd
    (LengthSMTLibQueryRun _ _ _ _ _ value _ _) = value

-- | Cumulative process stderr byte count when this run's transport began.
lengthSMTLibQueryRunStderrStart
  :: LengthSMTLibQueryRun epoch identity local
  -> Natural
lengthSMTLibQueryRunStderrStart
    (LengthSMTLibQueryRun _ _ _ _ _ _ value _) = value

-- | Cumulative process stderr byte count at the run's final boundary.  For a
-- committed run it always equals the start, because any stderr byte observed
-- during the run fails accounting instead of producing a run.
lengthSMTLibQueryRunStderrEnd
  :: LengthSMTLibQueryRun epoch identity local
  -> Natural
lengthSMTLibQueryRunStderrEnd
    (LengthSMTLibQueryRun _ _ _ _ _ _ _ value) = value

-- | Zero-based ordinal of this run in the shared worker's serial query lease;
-- scalar and spine-pair runs draw ordinals from the same counter.
lengthSpinePairSMTLibQueryRunOrdinal
  :: LengthSpinePairSMTLibQueryRun epoch identity local
  -> Natural
lengthSpinePairSMTLibQueryRunOrdinal
    (LengthSpinePairSMTLibQueryRun value _ _ _ _ _ _ _) = value

-- | The retained solver observation of the product query: satisfiable
-- (carrying independently replayed spine-pair counterexample evidence exactly
-- when the artifact policy requested input values), unsatisfiable, or
-- unknown.  No branch asserts solver soundness.
lengthSpinePairSMTLibQueryRunObservation
  :: LengthSpinePairSMTLibQueryRun epoch identity local
  -> LengthSpinePairSMTLibQueryRunObservation
lengthSpinePairSMTLibQueryRunObservation
    (LengthSpinePairSMTLibQueryRun _ observation _ _ _ _ _ _) = observation

-- | Private, reversible identity of the spine-pair run, the product-domain
-- sibling of 'lengthSMTLibQueryRunIdentityFingerprint'.  It embeds the
-- shared ready-worker identity, the query allocation, the spine-pair
-- protocol plan, the effective deadline, the exact causal transcript, and
-- the decoded outcome, replay policy, and transport commit boundaries.
lengthSpinePairSMTLibQueryRunIdentityFingerprint
  :: LengthSpinePairSMTLibQueryRun epoch identity local
  -> Fingerprint LengthSpinePairSMTLibQueryRunIdentitySubject
lengthSpinePairSMTLibQueryRunIdentityFingerprint
    (LengthSpinePairSMTLibQueryRun _ _ value _ _ _ _ _) = value

-- | The spine-pair run identity as a tagged fingerprint field for embedding
-- in a dependent fingerprint.
lengthSpinePairSMTLibQueryRunIdentityFingerprintField
  :: LengthSpinePairSMTLibQueryRun epoch identity local
  -> FingerprintField
lengthSpinePairSMTLibQueryRunIdentityFingerprintField run =
  tagged "spine-pair-query-run-identity"
    [FingerprintBytes $ fingerprintCanonicalBytes
      $ lengthSpinePairSMTLibQueryRunIdentityFingerprint run]

-- | SHA-256 of the exact causal stdout transcript of this run: the inherited
-- predecessor whitespace followed by every write epoch's bytes, in order.
lengthSpinePairSMTLibQueryRunTranscriptSHA256
  :: LengthSpinePairSMTLibQueryRun epoch identity local
  -> ByteString
lengthSpinePairSMTLibQueryRunTranscriptSHA256
    (LengthSpinePairSMTLibQueryRun _ _ _ value _ _ _ _) = value

-- | Byte count of the run's causal transcript, computed as the difference
-- between its stdout end and start boundaries.  A run is constructed only
-- after accounting proved that this difference equals the transcript's own
-- byte count, so the subtraction cannot underflow.
lengthSpinePairSMTLibQueryRunTranscriptByteCount
  :: LengthSpinePairSMTLibQueryRun epoch identity local
  -> Natural
lengthSpinePairSMTLibQueryRunTranscriptByteCount
    (LengthSpinePairSMTLibQueryRun _ _ _ _ stdoutStart stdoutEnd _ _) =
      stdoutEnd - stdoutStart

-- | Cumulative process stdout byte count at which this run's transport
-- began: the previous committed run's end boundary (of either domain), or
-- the ready-commit count for the first run.
lengthSpinePairSMTLibQueryRunStdoutStart
  :: LengthSpinePairSMTLibQueryRun epoch identity local
  -> Natural
lengthSpinePairSMTLibQueryRunStdoutStart
    (LengthSpinePairSMTLibQueryRun _ _ _ _ value _ _ _) = value

-- | Cumulative process stdout byte count observed at the run's final
-- boundary.  It exceeds the start by exactly the transcript byte count and
-- becomes the start boundary of the next committed run.
lengthSpinePairSMTLibQueryRunStdoutEnd
  :: LengthSpinePairSMTLibQueryRun epoch identity local
  -> Natural
lengthSpinePairSMTLibQueryRunStdoutEnd
    (LengthSpinePairSMTLibQueryRun _ _ _ _ _ value _ _) = value

-- | Cumulative process stderr byte count when this run's transport began.
lengthSpinePairSMTLibQueryRunStderrStart
  :: LengthSpinePairSMTLibQueryRun epoch identity local
  -> Natural
lengthSpinePairSMTLibQueryRunStderrStart
    (LengthSpinePairSMTLibQueryRun _ _ _ _ _ _ value _) = value

-- | Cumulative process stderr byte count at the run's final boundary.  For a
-- committed run it always equals the start, because any stderr byte observed
-- during the run fails accounting instead of producing a run.
lengthSpinePairSMTLibQueryRunStderrEnd
  :: LengthSpinePairSMTLibQueryRun epoch identity local
  -> Natural
lengthSpinePairSMTLibQueryRunStderrEnd
    (LengthSpinePairSMTLibQueryRun _ _ _ _ _ _ _ value) = value

-- Query runs ---------------------------------------------------------------
--
-- The scalar and binary-product query-run pipelines are one pipeline.  Both
-- domains take the same gate, prepare, reserve, execute, and commit path
-- against the same ready worker and the same shared ordinal lease; they
-- differ only in the nominal query, evidence, and model-error vocabulary,
-- the protocol plan they seal, the identity role and schema tags they bind,
-- and the two explicit capability-reuse fields the product run carries.  A
-- 'QueryRunDomain' names exactly those differences; 'runQueryRunDomain' is
-- the shared flow, spoken over the private 'QueryRunFailure' vocabulary and
-- the private 'QueryRunRecord', and each public entrance translates both
-- back into its domain's nominal failure and run types at the boundary.

-- | What distinguishes one domain's query run from the other's.
data QueryRunDomain planSubject runSubject query evidenceFamily counterexample
    modelError =
  QueryRunDomain
    { queryDomainRolePrefix :: String
      -- ^ the domain segment every run-identity and replay role starts with
    , queryDomainReplayRole :: [Word8]
      -- ^ the independent-replay role bound in the run identity
    , queryDomainSchemaTag :: QueryRunLaunch -> QueryRunDeadlineKind -> [Word8]
      -- ^ the run schema tag for the worker's launch and deadline policy
    , queryDomainProtocolLimits
        :: LengthSMTLibReadyWorkerQueryPolicy -> LengthSMTLibProtocolLimits
      -- ^ the protocol limits the plan is sealed under: the worker's own for
      -- the scalar profile it was admitted with, the product defaults for
      -- the product run which only reuses the worker as transport
    , queryDomainSealPlan
        :: LengthSMTLibProtocolLimits
        -> LengthSMTLibPostLaunchExecutionPolicy
        -> query
        -> [Word8]
        -> Maybe [Word8]
        -> Either
            LengthSMTLibProtocolPlanError
            (LengthSMTLibQueryProtocolPlan planSubject query)
    , queryDomainInputValueRequestBytes :: query -> Maybe [Word8]
    , queryDomainValidateCounterexample
        :: LengthEvaluationLimits
        -> query
        -> [LengthSMTLibIntegerBinding]
        -> Either modelError
            (Maybe (BehavioralEvidence evidenceFamily counterexample))
      -- ^ the domain's independent replay of decoded bindings
    , queryDomainCapabilityReuseFields :: [FingerprintField]
      -- ^ identity fields spliced after the executable authority; empty for
      -- the scalar run, the explicit common-QF_LIA reuse statement for the
      -- product run
    , queryDomainQueryAllocationPrefix :: Natural -> [FingerprintField]
      -- ^ identity fields spliced at the head of the @query-allocation@
      -- group, given the session's query budget; empty for the scalar run
    }

-- | The shared private failure vocabulary of the pipeline, parameterized by
-- the domain's nominal model-error type.  Each public entrance maps it onto
-- its domain's nominal failure constructors.
data QueryRunFailure modelError
  = QueryWorkerClosing
  | QueryWorkerSpent
  | QueryLimitExceeded !Natural !Natural
  | QueryProtocolPlanFailure !LengthSMTLibProtocolPlanError
  | QueryProcessStdoutCapacityTooSmall !Natural !Natural
  | QueryBarrierCollision
  | QueryDeadlineFailure !LengthSMTLibProcessError
  | QueryProcessFailure !LengthSMTLibProcessError
  | QueryProtocolFailure !LengthSMTLibProtocolError
  | QueryTranscriptAccountingMismatch !Natural !Natural
  | QueryStderrAccountingMismatch !Natural !Natural
  | QueryModelFailure !modelError
  | QueryModelNotCounterexample
  | QueryRunIdentityAdmissionTooSmall !Natural !Natural
  | QueryRunIdentityFingerprintByteLimitExceeded !Natural !Natural
  | QueryInternalFailure

-- | The primary failure plus the process cleanup status when the failure
-- spent the worker.
data QueryRunError modelError =
  QueryRunError
    !(QueryRunFailure modelError)
    !(Maybe LengthSMTLibProcessCleanupStatus)

-- | The status-indexed observation of one committed run in either domain.
type QueryRunObservation evidenceFamily counterexample =
  SolverObservation
    (Maybe (BehavioralEvidence evidenceFamily counterexample)) () ()

-- | One committed run before it is wrapped in its domain's nominal run type:
-- ordinal, replayed observation, private identity fingerprint, transcript
-- SHA-256, and the cumulative stdout and stderr boundaries.
data QueryRunRecord runSubject evidenceFamily counterexample = QueryRunRecord
  !Natural
  !(QueryRunObservation evidenceFamily counterexample)
  !(Fingerprint runSubject)
  !ByteString
  !Natural
  !Natural
  !Natural
  !Natural

queryRunRecordOrdinal
  :: QueryRunRecord runSubject evidenceFamily counterexample -> Natural
queryRunRecordOrdinal (QueryRunRecord value _ _ _ _ _ _ _) = value

queryRunRecordStdoutStart
  :: QueryRunRecord runSubject evidenceFamily counterexample -> Natural
queryRunRecordStdoutStart (QueryRunRecord _ _ _ _ value _ _ _) = value

queryRunRecordStdoutEnd
  :: QueryRunRecord runSubject evidenceFamily counterexample -> Natural
queryRunRecordStdoutEnd (QueryRunRecord _ _ _ _ _ value _ _) = value

queryRunRecordStderrStart
  :: QueryRunRecord runSubject evidenceFamily counterexample -> Natural
queryRunRecordStderrStart (QueryRunRecord _ _ _ _ _ _ value _) = value

queryRunRecordStderrEnd
  :: QueryRunRecord runSubject evidenceFamily counterexample -> Natural
queryRunRecordStderrEnd (QueryRunRecord _ _ _ _ _ _ _ value) = value


-- Everything a reserved ordinal needs to run: its ordinal, both barrier
-- markers, the sealed plan, and the evaluation limits and deadline it was
-- admitted under.
data PreparedQueryRun planSubject query =
  PreparedQueryRun
    !Natural
    !ByteString
    !ByteString
    !(LengthSMTLibQueryProtocolPlan planSubject query)
    !LengthEvaluationLimits
    !LengthSMTLibProcessDeadline

type role PreparedQueryRun nominal nominal

-- The last-committed accounting anchors are read from the lease state in the
-- same STM transaction which burns the ordinal and both marker roles.  Only
-- this receipt can cross the execution boundary; a merely prepared plan has
-- no transport-commit anchor.
data ReservedQueryRun planSubject query =
  ReservedQueryRun
    !(PreparedQueryRun planSubject query)
    !Natural
    !Natural

type role ReservedQueryRun nominal nominal

-- The one query-run pipeline behind 'runLengthSMTLibReadyWorkerQuery' and
-- 'runLengthSpinePairSMTLibReadyWorkerQuery': deadline selection, the query
-- gate, preparation, reservation, execution, and commit are identical for
-- both domains; the 'QueryRunDomain' supplies what differs.
runQueryRunDomain
  :: (NFData modelError, NFData counterexample)
  => QueryRunDomain planSubject runSubject query evidenceFamily counterexample
      modelError
  -> LengthEvaluationLimits
  -> LengthSMTLibReadyWorker epoch
  -> query
  -> IO
      (Either
        (QueryRunError modelError)
        (QueryRunRecord runSubject evidenceFamily counterexample))
runQueryRunDomain domain evaluationLimits worker query =
  mask $ \restore -> do
    deadlineResult <- effectiveLengthSMTLibDeadlineAfterMilliseconds
      (readyQueryUsableWorkDeadlinePolicy $ readyWorkerQueryPolicy worker)
      $ lengthSMTLibPostLaunchHostDeadlineMilliseconds postLaunchExecution
    case deadlineResult of
      Left failure -> pure $ queryRunLeft
        $ QueryDeadlineFailure failure
      Right (LengthSMTLibEffectiveDeadline _ deadline) -> do
        acquired <- acquireQueryGate worker deadline
        case acquired of
          Left failure@QueryProcessFailure {} ->
            spendQueryWorker worker failure
          Left failure@QueryInternalFailure ->
            spendQueryWorker worker failure
          Left failure -> pure $ queryRunLeft failure
          Right () -> finally
            (runWithGate restore deadline)
            (atomically $ putTMVar (readyWorkerQueryGate worker) ())
 where
  postLaunchExecution = readyQueryPostLaunchExecution
    $ readyWorkerQueryPolicy worker

  runWithGate restore deadline = do
    prepared <- prepareQueryRun domain
      evaluationLimits worker query deadline
    case prepared of
      Left (mustSpend, failure)
        | mustSpend -> spendQueryWorker worker failure
        | otherwise -> pure $ queryRunLeft failure
      Right preparation -> do
        reserved <- reserveQueryRun worker preparation
        case reserved of
          Left QueryBarrierCollision ->
            spendQueryWorker worker
              QueryBarrierCollision
          Left QueryInternalFailure ->
            spendQueryWorker worker
              QueryInternalFailure
          Left failure -> pure $ queryRunLeft failure
          Right reservation -> do
            executed <- restore
              (executeQueryRun domain worker reservation)
              `onException` abandonQueryWorker worker
            case executed of
              Left failure -> spendQueryWorker worker failure
              Right run -> do
                committed <- commitQueryRun worker reservation run
                if committed
                  then pure $ Right run
                  else spendQueryWorker worker
                    QueryInternalFailure

queryRunLeft
  :: QueryRunFailure modelError
  -> Either (QueryRunError modelError) value
queryRunLeft failure = Left $ QueryRunError failure Nothing

acquireQueryGate
  :: LengthSMTLibReadyWorker epoch
  -> LengthSMTLibProcessDeadline
  -> IO (Either (QueryRunFailure modelError) ())
acquireQueryGate worker deadline = mask $ const loop
 where
  maximumQueries = readyQueryMaximumQueries $ readyWorkerQueryPolicy worker
  process = readyWorkerProcess worker
  cancellation = readyWorkerCancellation worker
  stateVariable = readyWorkerQueryState worker
  gate = readyWorkerQueryGate worker

  loop = do
    initial <- atomically $ queryLeaseAdmission maximumQueries
      <$> readTVar stateVariable
    case initial of
      Left failure -> pure $ Left failure
      Right () -> do
        -- The controlled action only observes the token.  Destructive
        -- acquisition happens later under masking because waitControlled has
        -- a deliberate post-action cancellation/deadline precedence check.
        observed <- waitLengthSMTLibProcessControl process cancellation deadline
          LengthSMTLibProcessQueryPhase $ do
            state <- readTVar stateVariable
            case queryLeaseAdmission maximumQueries state of
              Left failure -> pure $ Left failure
              Right () -> Right <$> readTMVar gate
        case observed of
          Left processFailure -> do
            state <- readTVarIO stateVariable
            pure $ Left $ queryGateProcessFailure state processFailure
          Right (Left failure) -> pure $ Left failure
          Right (Right ()) -> do
            claimed <- atomically $ do
              state <- readTVar stateVariable
              case queryLeaseAdmission maximumQueries state of
                Left failure -> pure $ Left failure
                Right () -> do
                  token <- tryTakeTMVar gate
                  pure $ Right token
            case claimed of
              Left failure -> pure $ Left failure
              Right Nothing -> loop
              Right (Just ()) -> pure $ Right ()

queryLeaseAdmission
  :: Natural
  -> QueryLeaseState
  -> Either (QueryRunFailure modelError) ()
queryLeaseAdmission maximumQueries
    (QueryLeaseState mode nextOrdinal _ _ _ _) = case mode of
  QueryLeaseClosing -> Left QueryWorkerClosing
  QueryLeaseSpent -> Left QueryWorkerSpent
  QueryLeaseAccepting
    | nextOrdinal >= maximumQueries -> Left
        $ QueryLimitExceeded maximumQueries (maximumQueries + 1)
    | otherwise -> Right ()

queryGateProcessFailure
  :: QueryLeaseState
  -> LengthSMTLibProcessError
  -> QueryRunFailure modelError
queryGateProcessFailure (QueryLeaseState mode _ _ _ _ _) failure = case mode of
  QueryLeaseClosing -> QueryWorkerClosing
  QueryLeaseSpent -> QueryWorkerSpent
  _ -> queryProcessFailure failure

prepareQueryRun
  :: QueryRunDomain planSubject runSubject query evidenceFamily counterexample
      modelError
  -> LengthEvaluationLimits
  -> LengthSMTLibReadyWorker epoch
  -> query
  -> LengthSMTLibProcessDeadline
  -> IO
      (Either
        (Bool, QueryRunFailure modelError)
        (PreparedQueryRun planSubject query))
prepareQueryRun domain evaluationLimits worker query deadline = do
  state@(QueryLeaseState _ ordinal used inFlight _ _) <-
    readTVarIO $ readyWorkerQueryState worker
  case queryLeaseAdmission maximumQueries state of
    Left failure -> pure $ Left (False, failure)
    Right () | isJust inFlight -> pure
      $ Left (True, QueryInternalFailure)
    Right () -> do
      controlled <- waitLengthSMTLibProcessControl process
        (readyWorkerCancellation worker) deadline LengthSMTLibProcessQueryPhase
        $ pure ()
      case controlled of
        Left failure -> pure $ Left
          ( lengthSMTLibProcessErrorClass failure /=
              LengthSMTLibProcessDeadlineExceeded
          , queryProcessFailure failure
          )
        Right () -> prepareControlled ordinal used
 where
  policy = readyWorkerQueryPolicy worker
  maximumQueries = readyQueryMaximumQueries policy
  protocolLimits = queryDomainProtocolLimits domain policy
  postLaunchExecution = readyQueryPostLaunchExecution policy
  process = readyWorkerProcess worker
  processLimits = lengthSMTLibProcessLimits process
  transportMaximum = lengthSMTLibProcessStdoutByteLimit processLimits

  prepareControlled ordinal used = do
    let ordinalWord = fromIntegral ordinal
        checkBarrier = deriveQueryBarrier
          (readyWorkerBarrierSeed worker) ordinalWord queryCheckBarrierRole
        valueBarrier = deriveQueryBarrier
          (readyWorkerBarrierSeed worker) ordinalWord queryValueBarrierRole
        needsValueBarrier =
          lengthSMTLibPostLaunchArtifactPolicy postLaunchExecution ==
            LengthSMTLibInputValuesAfterSatisfiable &&
          isJust (queryDomainInputValueRequestBytes domain query)
        valueNonce
          | needsValueBarrier = Just $ BS.unpack valueBarrier
          | otherwise = Nothing
    case queryDomainSealPlan domain protocolLimits postLaunchExecution query
        (BS.unpack checkBarrier) valueNonce of
      Left failure -> pure $ Left
        (False, QueryProtocolPlanFailure failure)
      Right plan
        | Set.member checkBarrier used || Set.member valueBarrier used ||
            checkBarrier == valueBarrier -> pure
              $ Left (True, QueryBarrierCollision)
        | otherwise -> do
            observedStdout <- lengthSMTLibProcessObservedStdoutBytes process
            let required = lengthSMTLibProtocolPlanMinimumStdoutByteCount plan
                remaining
                  | observedStdout >= transportMaximum = 0
                  | otherwise = transportMaximum - observedStdout
            if required > remaining
              then pure $ Left (False,
                QueryProcessStdoutCapacityTooSmall
                  remaining required)
              else case admitQueryRunIdentity domain
                  worker plan
                  evaluationLimits ordinal deadline
                  checkBarrier valueBarrier of
                Left failure -> pure $ Left (False, failure)
                Right () -> pure $ Right $ PreparedQueryRun
                  ordinal checkBarrier valueBarrier plan evaluationLimits deadline

reserveQueryRun
  :: LengthSMTLibReadyWorker epoch
  -> PreparedQueryRun planSubject query
  -> IO
      (Either
        (QueryRunFailure modelError)
        (ReservedQueryRun planSubject query))
reserveQueryRun worker
    preparation@(PreparedQueryRun
      ordinal checkBarrier valueBarrier _ _ _) = atomically $ do
  state@(QueryLeaseState mode nextOrdinal used inFlight stdoutCount stderrCount) <-
    readTVar $ readyWorkerQueryState worker
  case queryLeaseAdmission maximumQueries state of
    Left failure -> pure $ Left failure
    Right ()
      | mode /= QueryLeaseAccepting || nextOrdinal /= ordinal ||
          isJust inFlight -> pure $ Left QueryInternalFailure
      | Set.member checkBarrier used || Set.member valueBarrier used ||
          checkBarrier == valueBarrier -> pure
            $ Left QueryBarrierCollision
      | otherwise -> do
          writeTVar (readyWorkerQueryState worker) $ QueryLeaseState
            QueryLeaseAccepting (ordinal + 1)
            (Set.insert valueBarrier $ Set.insert checkBarrier used)
            (Just ordinal) stdoutCount stderrCount
          pure $ Right $ ReservedQueryRun
            preparation stdoutCount stderrCount
 where
  maximumQueries = readyQueryMaximumQueries $ readyWorkerQueryPolicy worker

commitQueryRun
  :: LengthSMTLibReadyWorker epoch
  -> ReservedQueryRun planSubject query
  -> QueryRunRecord runSubject evidenceFamily counterexample
  -> IO Bool
commitQueryRun worker
    (ReservedQueryRun
      (PreparedQueryRun ordinal _ _ _ _ _)
      stdoutStart stderrStart)
    run =
  atomically $ do
    QueryLeaseState mode nextOrdinal used inFlight
        stateStdoutStart stateStderrStart <-
      readTVar $ readyWorkerQueryState worker
    if inFlight /= Just ordinal || nextOrdinal /= ordinal + 1 ||
        mode == QueryLeaseSpent ||
        stateStdoutStart /= stdoutStart || stateStderrStart /= stderrStart ||
        queryRunRecordOrdinal run /= ordinal ||
        queryRunRecordStdoutStart run /= stdoutStart ||
        queryRunRecordStderrStart run /= stderrStart
      then pure False
      else do
        writeTVar (readyWorkerQueryState worker) $ QueryLeaseState
          mode nextOrdinal used Nothing stdoutEnd stderrEnd
        pure True
 where
  stdoutEnd = queryRunRecordStdoutEnd run
  stderrEnd = queryRunRecordStderrEnd run

spendQueryWorker
  :: LengthSMTLibReadyWorker epoch
  -> QueryRunFailure modelError
  -> IO (Either (QueryRunError modelError) value)
spendQueryWorker worker failure = do
  cleanup <- abandonQueryWorker worker
  pure $ Left $ QueryRunError failure $ Just cleanup

abandonQueryWorker
  :: LengthSMTLibReadyWorker epoch
  -> IO LengthSMTLibProcessCleanupStatus
abandonQueryWorker worker = do
  atomically $ do
    QueryLeaseState _ ordinal used _ stdoutCount stderrCount <-
      readTVar $ readyWorkerQueryState worker
    writeTVar (readyWorkerQueryState worker) $ QueryLeaseState
      QueryLeaseSpent ordinal used Nothing stdoutCount stderrCount
  cancelLengthSMTLibProcess $ readyWorkerCancellation worker
  closeLengthSMTLibProcess $ readyWorkerProcess worker

queryProcessFailure
  :: LengthSMTLibProcessError
  -> QueryRunFailure modelError
queryProcessFailure failure
  | lengthSMTLibProcessErrorClass failure ==
      LengthSMTLibProcessDeadlineExceeded =
        QueryDeadlineFailure failure
  | otherwise = QueryProcessFailure failure

executeQueryRun
  :: (NFData modelError, NFData counterexample)
  => QueryRunDomain planSubject runSubject query evidenceFamily counterexample
      modelError
  -> LengthSMTLibReadyWorker epoch
  -> ReservedQueryRun planSubject query
  -> IO
      (Either
        (QueryRunFailure modelError)
        (QueryRunRecord runSubject evidenceFamily counterexample))
executeQueryRun domain worker
    (ReservedQueryRun
      (PreparedQueryRun ordinal checkBarrier valueBarrier plan
        evaluationLimits deadline)
      stdoutStart stderrStart) = do
  driven <- driveProtocolQuery worker deadline plan
  case driven of
    Left failure -> pure $ Left failure
    Right (decoded, transcript) -> do
      firstBoundary <- observeQueryBoundary worker deadline
      case firstBoundary of
        Left failure -> pure $ Left failure
        Right (stdoutEnd, stderrEnd) ->
          case validateQueryAccounting
              transcript stdoutStart stdoutEnd stderrStart stderrEnd of
            Left failure -> pure $ Left failure
            Right () -> do
              replayed <- replayQuery domain
                evaluationLimits worker plan deadline decoded
              case replayed of
                Left failure -> pure $ Left failure
                Right outcome -> do
                  committedBoundary <-
                    observeQueryBoundary worker deadline
                  case committedBoundary of
                    Left failure -> pure $ Left failure
                    Right (stdoutCommitted, stderrCommitted)
                      | stdoutCommitted /= stdoutEnd -> pure $ Left
                          $ QueryTranscriptAccountingMismatch
                              (smtLibCausalTranscriptByteCount transcript)
                              (stdoutCommitted -| stdoutStart)
                      | stderrCommitted /= stderrEnd -> pure $ Left
                          $ QueryStderrAccountingMismatch
                              stderrStart stderrCommitted
                      | otherwise -> case buildQueryRunIdentity domain
                          worker plan evaluationLimits ordinal deadline
                          checkBarrier valueBarrier outcome transcript
                          stdoutStart stdoutCommitted stderrStart stderrCommitted of
                        Left failure -> pure $ Left failure
                        Right identity -> do
                          let transcriptBytes = causalTranscriptBytes transcript
                              transcriptDigest = SHA256.hash transcriptBytes
                          finalBoundary <-
                            observeQueryBoundary worker deadline
                          pure $ case finalBoundary of
                            Left failure -> Left failure
                            Right (stdoutFinal, stderrFinal)
                              | stdoutFinal /= stdoutCommitted -> Left
                                  $ QueryTranscriptAccountingMismatch
                                      (smtLibCausalTranscriptByteCount
                                        transcript)
                                      (stdoutFinal -| stdoutStart)
                              | stderrFinal /= stderrCommitted -> Left
                                  $ QueryStderrAccountingMismatch
                                      stderrStart stderrFinal
                              | otherwise -> Right $ QueryRunRecord
                                  ordinal
                                  (replayedQueryObservation outcome)
                                  identity
                                  transcriptDigest
                                  stdoutStart stdoutFinal
                                  stderrStart stderrFinal

driveProtocolQuery
  :: LengthSMTLibReadyWorker epoch
  -> LengthSMTLibProcessDeadline
  -> LengthSMTLibQueryProtocolPlan planSubject query
  -> IO
      (Either
        (QueryRunFailure modelError)
        ( LengthSMTLibQueryProtocolDecoded planSubject query
        , SMTLibCausalTranscript LengthSMTLibProtocolWriteKind
        ))
driveProtocolQuery worker deadline plan = do
  driven <- driveSMTLibCausalActions
    SMTLibCausalAdoptPredecessorWhitespace
    (lengthSMTLibProtocolPlanCumulativeStdoutByteLimit plan)
    feedLengthSMTLibProtocol finishLengthSMTLibProtocol
    LengthSMTLibProtocolUnexpectedPostBarrierByte
    lengthSMTLibCausalTransportOps
    (lengthSMTLibCausalTransport
      (readyWorkerProcess worker) (readyWorkerCancellation worker) deadline)
    $ startLengthSMTLibProtocol plan
  pure $ case driven of
    Left failure -> Left $ mapFailure failure
    Right value -> Right value
 where
  mapFailure failure = case failure of
    SMTLibCausalTransportFailure processFailure ->
      queryProcessFailure processFailure
    SMTLibCausalMachineFailure protocolFailure ->
      QueryProtocolFailure protocolFailure
    SMTLibCausalCumulativeOutputByteLimitExceeded maximumBytes observed ->
      QueryProtocolFailure
        $ LengthSMTLibProtocolCumulativeStdoutByteLimitExceeded
            maximumBytes observed
    SMTLibCausalInternalFailure -> QueryInternalFailure

observeQueryBoundary
  :: LengthSMTLibReadyWorker epoch
  -> LengthSMTLibProcessDeadline
  -> IO (Either (QueryRunFailure modelError) (Natural, Natural))
observeQueryBoundary worker deadline = do
  ready <- checkLengthSMTLibProcessReady
    (readyWorkerProcess worker) (readyWorkerCancellation worker) deadline
  case ready of
    Left failure -> pure $ Left $ queryProcessFailure failure
    Right () -> do
      stdoutCount <- lengthSMTLibProcessObservedStdoutBytes
        $ readyWorkerProcess worker
      stderrCount <- lengthSMTLibProcessObservedStderrBytes
        $ readyWorkerProcess worker
      pure $ Right (stdoutCount, stderrCount)

validateQueryAccounting
  :: SMTLibCausalTranscript kind
  -> Natural
  -> Natural
  -> Natural
  -> Natural
  -> Either (QueryRunFailure modelError) ()
validateQueryAccounting transcript stdoutStart stdoutEnd
    stderrStart stderrEnd
  | stdoutEnd < stdoutStart = Left QueryInternalFailure
  | transcriptCount /= stdoutEnd - stdoutStart = Left
      $ QueryTranscriptAccountingMismatch
          transcriptCount (stdoutEnd - stdoutStart)
  | stderrEnd /= stderrStart = Left
      $ QueryStderrAccountingMismatch stderrStart stderrEnd
  | otherwise = Right ()
 where
  transcriptCount = smtLibCausalTranscriptByteCount transcript

replayQuery
  :: (NFData modelError, NFData counterexample)
  => QueryRunDomain planSubject runSubject query evidenceFamily counterexample
      modelError
  -> LengthEvaluationLimits
  -> LengthSMTLibReadyWorker epoch
  -> LengthSMTLibQueryProtocolPlan planSubject query
  -> LengthSMTLibProcessDeadline
  -> LengthSMTLibQueryProtocolDecoded planSubject query
  -> IO
      (Either
        (QueryRunFailure modelError)
        (ReplayedQueryOutcome evidenceFamily counterexample))
replayQuery domain evaluationLimits worker plan deadline decoded =
  case ( lengthSMTLibProtocolDecodedObservation decoded
       , lengthSMTLibProtocolPlanArtifactPolicy plan) of
    ( SatisfiableObservation (Just values)
      , LengthSMTLibInputValuesAfterSatisfiable) -> do
      replayed <- runBeforeLengthSMTLibProcessDeadline
        (readyWorkerCancellation worker) deadline
        (evaluate $ force
          $ queryDomainValidateCounterexample domain evaluationLimits query
              values)
        (const $ pure ())
      pure $ case replayed of
        Left failure -> Left $ queryProcessFailure failure
        Right (Left failure) -> Left $ QueryModelFailure failure
        Right (Right Nothing) -> Left QueryModelNotCounterexample
        Right (Right (Just evidence)) -> Right $ case values of
          [] -> ReplayedSatisfiableVacuous evidence
          _ : _ -> ReplayedSatisfiableFramed evidence
    ( SatisfiableObservation Nothing
      , LengthSMTLibInputValuesAfterSatisfiable) ->
      pure $ Left QueryInternalFailure
    (SatisfiableObservation Nothing, LengthSMTLibStatusOnly) ->
      pure $ Right ReplayedSatisfiableStatusOnly
    (SatisfiableObservation (Just _), LengthSMTLibStatusOnly) ->
      pure $ Left QueryInternalFailure
    (UnsatisfiableObservation (), _) ->
      pure $ Right ReplayedUnsatisfiable
    (UnknownObservation (), _) ->
      pure $ Right ReplayedUnknown
 where
  query = lengthSMTLibProtocolPlanQuery plan

-- | The five successful identity branches after independent replay. This one
-- transient owner prevents decoded status/value classification from being
-- paired independently with replay evidence while the unchanged v1 identity
-- fields are built.
data ReplayedQueryOutcome evidenceFamily counterexample
  = ReplayedSatisfiableStatusOnly
  | ReplayedSatisfiableVacuous
      !(BehavioralEvidence evidenceFamily counterexample)
  | ReplayedSatisfiableFramed
      !(BehavioralEvidence evidenceFamily counterexample)
  | ReplayedUnsatisfiable
  | ReplayedUnknown

-- Generic 'SolverObservation' artifacts remain lazy. Constructing the final
-- strict run owner separately forces only the satisfiable 'Maybe' spine, as
-- the former strict evidence field did, without adding a global payload bang.
replayedQueryObservation
  :: ReplayedQueryOutcome evidenceFamily counterexample
  -> QueryRunObservation evidenceFamily counterexample
replayedQueryObservation outcome = case outcome of
  ReplayedSatisfiableStatusOnly ->
    let evidence = Nothing
    in evidence `seq` SatisfiableObservation evidence
  ReplayedSatisfiableVacuous retained ->
    let evidence = Just retained
    in evidence `seq` SatisfiableObservation evidence
  ReplayedSatisfiableFramed retained ->
    let evidence = Just retained
    in evidence `seq` SatisfiableObservation evidence
  ReplayedUnsatisfiable -> UnsatisfiableObservation ()
  ReplayedUnknown -> UnknownObservation ()

(-|) :: Natural -> Natural -> Natural
left -| right
  | left < right = 0
  | otherwise = left - right

buildQueryRunIdentity
  :: QueryRunDomain planSubject runSubject query evidenceFamily counterexample
      modelError
  -> LengthSMTLibReadyWorker epoch
  -> LengthSMTLibQueryProtocolPlan planSubject query
  -> LengthEvaluationLimits
  -> Natural
  -> LengthSMTLibProcessDeadline
  -> ByteString
  -> ByteString
  -> ReplayedQueryOutcome evidenceFamily counterexample
  -> SMTLibCausalTranscript LengthSMTLibProtocolWriteKind
  -> Natural
  -> Natural
  -> Natural
  -> Natural
  -> Either
      (QueryRunFailure modelError)
      (Fingerprint runSubject)
buildQueryRunIdentity domain worker plan evaluationLimits ordinal deadline
    checkBarrier valueBarrier outcome transcript
    stdoutStart stdoutEnd stderrStart stderrEnd =
  case buildFingerprintWithin maximumBytes FingerprintBuilder
      { fingerprintBuilderVersion = 1
      , fingerprintBuilderRole = queryRunFingerprintRoleForWorker domain worker
      , fingerprintBuilderFields =
          queryRunIdentityPrefixFields domain worker plan ordinal deadline
            checkBarrier valueBarrier ++
          [queryRunTranscriptField transcript] ++
          queryRunIdentitySuffixFields domain evaluationLimits outcome
            stdoutStart stdoutEnd stderrStart stderrEnd
            (smtLibCausalTranscriptByteCount transcript)
      } of
    Left (FingerprintLimitExceeded fingerprintMaximum observed) -> Left
      $ QueryRunIdentityFingerprintByteLimitExceeded
          fingerprintMaximum observed
    Right identity -> Right identity
 where
  maximumBytes = readyQueryRunIdentityFingerprintByteLimit
    $ readyWorkerQueryPolicy worker

admitQueryRunIdentity
  :: QueryRunDomain planSubject runSubject query evidenceFamily counterexample
      modelError
  -> LengthSMTLibReadyWorker epoch
  -> LengthSMTLibQueryProtocolPlan planSubject query
  -> LengthEvaluationLimits
  -> Natural
  -> LengthSMTLibProcessDeadline
  -> ByteString
  -> ByteString
  -> Either (QueryRunFailure modelError) ()
admitQueryRunIdentity domain worker plan evaluationLimits
    ordinal deadline checkBarrier valueBarrier
  | requiredMaximum > maximumBytes = Left
      $ QueryRunIdentityAdmissionTooSmall
          maximumBytes requiredMaximum
  | otherwise = Right ()
 where
  maximumBytes = readyQueryRunIdentityFingerprintByteLimit
    $ readyWorkerQueryPolicy worker
  processLimits = lengthSMTLibProcessLimits $ readyWorkerProcess worker
  transcriptMaximum =
    lengthSMTLibProtocolPlanCumulativeStdoutByteLimit plan
  epochMaximum = if isJust $ lengthSMTLibProtocolInputValueWriteBytes plan
    then 2
    else 1
  prefixCounts = map fingerprintFieldByteCount
    $ queryRunIdentityPrefixFields domain worker plan ordinal deadline
        checkBarrier valueBarrier
  transcriptCount = queryRunTranscriptMaximumFieldByteCount
    transcriptMaximum epochMaximum
  suffixCounts = map fingerprintFieldByteCount
    $ queryRunIdentityMaximumSuffixFields domain evaluationLimits
        (lengthSMTLibProcessStdoutByteLimit processLimits)
        (lengthSMTLibProcessStderrByteLimit processLimits)
        transcriptMaximum
  requiredMaximum = fingerprintBuilderByteCount
    (queryRunFingerprintRoleForWorker domain worker)
    $ prefixCounts ++ transcriptCount : suffixCounts

-- How the worker was launched and which usable-work deadline policy it
-- runs under: the two axes every run-identity role and schema tag is keyed
-- by.
data QueryRunLaunch
  = PathnameSnapshotLaunch
  | DescriptorBoundLaunch
  | DescriptorBoundEffectiveIDLaunch
  | DescriptorBoundExecveCheckLaunch

data QueryRunDeadlineKind
  = FreshPerQueryDeadline
  | SharedUsableWorkDeadline
  | ScopedSharedUsableWorkDeadline

queryRunLaunch :: LengthSMTLibReadyWorker epoch -> QueryRunLaunch
queryRunLaunch worker
  | workerUsesDescriptorBoundExecveCheckExecutableAccessLaunch worker =
      DescriptorBoundExecveCheckLaunch
  | workerUsesDescriptorBoundEffectiveIDExecutableAccessLaunch worker =
      DescriptorBoundEffectiveIDLaunch
  | workerUsesDescriptorBoundExecutableLaunch worker = DescriptorBoundLaunch
  | otherwise = PathnameSnapshotLaunch

queryRunDeadlineKind :: LengthSMTLibReadyWorker epoch -> QueryRunDeadlineKind
queryRunDeadlineKind worker =
  case readyQueryUsableWorkDeadlinePolicy $ readyWorkerQueryPolicy worker of
    LengthSMTLibFreshPerQueryDeadline -> FreshPerQueryDeadline
    LengthSMTLibSharedUsableWorkDeadline {} -> SharedUsableWorkDeadline
    LengthSMTLibScopedSharedUsableWorkDeadline {} ->
      ScopedSharedUsableWorkDeadline

-- The run-identity fingerprint role: the domain's role prefix, the shared
-- @z3-live-query-run@ segment, and a suffix keyed by launch and deadline
-- policy.  The historical pathname-snapshot, fresh-per-query role has no
-- suffix at all.
queryRunFingerprintRoleForWorker
  :: QueryRunDomain planSubject runSubject query evidenceFamily counterexample
      modelError
  -> LengthSMTLibReadyWorker epoch
  -> [Word8]
queryRunFingerprintRoleForWorker domain worker = ascii
  $ queryDomainRolePrefix domain ++ "/z3-live-query-run" ++ suffix
 where
  suffix = case (queryRunLaunch worker, queryRunDeadlineKind worker) of
    (PathnameSnapshotLaunch, FreshPerQueryDeadline) -> ""
    (PathnameSnapshotLaunch, SharedUsableWorkDeadline) ->
      "/shared-usable-work-deadline"
    (PathnameSnapshotLaunch, ScopedSharedUsableWorkDeadline) ->
      "/scoped-shared-usable-work-deadline/v2"
    (DescriptorBoundLaunch, FreshPerQueryDeadline) -> "/sealed-main-image/v1"
    (DescriptorBoundLaunch, SharedUsableWorkDeadline) ->
      "/sealed-main-image/shared-usable-work-deadline/v1"
    (DescriptorBoundLaunch, ScopedSharedUsableWorkDeadline) ->
      "/sealed-main-image/scoped-shared-usable-work-deadline/v1"
    (DescriptorBoundEffectiveIDLaunch, FreshPerQueryDeadline) ->
      "/effective-id-executable-access-sealed-main-image/v1"
    (DescriptorBoundEffectiveIDLaunch, SharedUsableWorkDeadline) -> concat
      [ "/effective-id-executable-access-sealed-main-image/"
      , "shared-usable-work-deadline/v1"
      ]
    (DescriptorBoundEffectiveIDLaunch, ScopedSharedUsableWorkDeadline) ->
      concat
        [ "/effective-id-executable-access-sealed-main-image/"
        , "scoped-shared-usable-work-deadline/v1"
        ]
    (DescriptorBoundExecveCheckLaunch, FreshPerQueryDeadline) ->
      "/execve-check-executable-access-sealed-main-image/v1"
    (DescriptorBoundExecveCheckLaunch, SharedUsableWorkDeadline) -> concat
      [ "/execve-check-executable-access-sealed-main-image/"
      , "shared-usable-work-deadline/v1"
      ]
    (DescriptorBoundExecveCheckLaunch, ScopedSharedUsableWorkDeadline) ->
      concat
        [ "/execve-check-executable-access-sealed-main-image/"
        , "scoped-shared-usable-work-deadline/v1"
        ]

queryRunSchemaTagForWorker
  :: QueryRunDomain planSubject runSubject query evidenceFamily counterexample
      modelError
  -> LengthSMTLibReadyWorker epoch
  -> [Word8]
queryRunSchemaTagForWorker domain worker =
  queryDomainSchemaTag domain (queryRunLaunch worker)
    (queryRunDeadlineKind worker)

workerUsesDescriptorBoundExecutableLaunch
  :: LengthSMTLibReadyWorker epoch
  -> Bool
workerUsesDescriptorBoundExecutableLaunch =
  lengthSMTLibProcessUsesDescriptorBoundExecutableLaunch
  . readyWorkerProcess

workerUsesDescriptorBoundEffectiveIDExecutableAccessLaunch
  :: LengthSMTLibReadyWorker epoch
  -> Bool
workerUsesDescriptorBoundEffectiveIDExecutableAccessLaunch =
  lengthSMTLibProcessUsesDescriptorBoundEffectiveIDExecutableAccessLaunch
  . readyWorkerProcess

workerUsesDescriptorBoundExecveCheckExecutableAccessLaunch
  :: LengthSMTLibReadyWorker epoch
  -> Bool
workerUsesDescriptorBoundExecveCheckExecutableAccessLaunch =
  lengthSMTLibProcessUsesDescriptorBoundExecveCheckExecutableAccessLaunch
  . readyWorkerProcess

queryExecutableAuthorityField
  :: LengthSMTLibReadyWorker epoch
  -> FingerprintField
queryExecutableAuthorityField worker = FingerprintBytes $ ascii $
  if workerUsesDescriptorBoundExecveCheckExecutableAccessLaunch worker
    then concat
      [ "live-syntactic-process-observation/"
      , "independent-counterexample-replay/"
      , "two-point-source-effective-id-vfs-and-execve-check-executable-"
      , "access-admitted/"
      , "sealed-mfd-exec-fixed-0500-f-seal-exec-staged-execve-check-main-"
      , "image-bytes-bound/"
      , "point-in-time-format-and-interpreter-dependencies-ignored/"
      , "no-source-authorization-transfer-binfmt-bprm-check-credential-"
      , "transition-interpreter-loader-library-or-solver-soundness-"
      , "authority/v1"
      ]
  else if workerUsesDescriptorBoundEffectiveIDExecutableAccessLaunch worker
    then concat
      [ "live-syntactic-process-observation/"
      , "independent-counterexample-replay/"
      , "two-point-effective-id-source-vfs-executable-access-admitted/"
      , "sealed-staged-main-image-bytes-bound/"
      , "point-in-time-not-full-exec-bprm-lsm-ima-binfmt-authority/"
      , "no-setuid-file-capability-loader-library-interpreter-or-solver-"
      , "soundness-authority/v1"
      ]
  else if workerUsesDescriptorBoundExecutableLaunch worker
  then concat
    [ "live-syntactic-process-observation/"
    , "independent-counterexample-replay/"
    , "sealed-staged-main-image-bytes-bound/"
    , "no-setuid-file-capability-loader-library-interpreter-or-solver-"
    , "soundness-authority/v1"
    ]
  else concat
    [ "live-syntactic-process-observation/"
    , "independent-counterexample-replay/"
    , "no-solver-soundness-or-executable-image-attestation/v1"
    ]

queryRunIdentityPrefixFields
  :: QueryRunDomain planSubject runSubject query evidenceFamily counterexample
      modelError
  -> LengthSMTLibReadyWorker epoch
  -> LengthSMTLibQueryProtocolPlan planSubject query
  -> Natural
  -> LengthSMTLibProcessDeadline
  -> ByteString
  -> ByteString
  -> [FingerprintField]
queryRunIdentityPrefixFields domain worker plan ordinal deadline
    checkBarrier valueBarrier =
  [ FingerprintBytes $ queryRunSchemaTagForWorker domain worker
  , tagged "authority"
      [queryExecutableAuthorityField worker]
  ] ++ queryDomainCapabilityReuseFields domain ++
  [ lengthSMTLibReadyWorkerIdentityFingerprintField worker
  , tagged "query-allocation"
      $ FingerprintBytes lengthSMTLibQueryBarrierSchemaTag
      : queryDomainQueryAllocationPrefix domain
          (readyQueryMaximumQueries $ readyWorkerQueryPolicy worker) ++
      [ FingerprintBytes $ ascii
          "zero-based-u64be/reserve-both-roles/burn-on-live-failure/v1"
      , FingerprintNatural ordinal
      , FingerprintBytes $ BS.unpack $ encodeWord64BE ordinalWord
      , tagged "check-role-spent-marker"
          [FingerprintBytes $ BS.unpack checkBarrier]
      , tagged "input-value-role-spent-marker"
          [FingerprintBytes $ BS.unpack valueBarrier]
      , FingerprintBytes $ BS.unpack
          $ barrierSeedCommitment $ readyWorkerBarrierSeed worker
      ]
  , tagged "protocol-plan"
      [ FingerprintBytes $ fingerprintCanonicalBytes
          $ lengthSMTLibProtocolPlanFingerprint plan
      ]
  ] ++ queryDeadlineIdentityFields worker deadline
 where
  -- Session-limit admission proves every runnable ordinal fits the chosen
  -- wire representation. Keep the Natural lease ordinal authoritative and
  -- derive its fixed-width encoding only at this identity edge.
  ordinalWord = fromIntegral ordinal

queryDeadlineIdentityFields
  :: LengthSMTLibReadyWorker epoch
  -> LengthSMTLibProcessDeadline
  -> [FingerprintField]
queryDeadlineIdentityFields worker effectiveDeadline =
  lengthSMTLibProcessDeadlineFingerprintField effectiveDeadline :
  case readyQueryUsableWorkDeadlinePolicy $ readyWorkerQueryPolicy worker of
    LengthSMTLibFreshPerQueryDeadline -> []
    LengthSMTLibSharedUsableWorkDeadline milliseconds sharedDeadline ->
      [ tagged "shared-usable-work-deadline-selection"
          [ FingerprintBytes $ ascii
              "minimum-absolute-monotonic-deadline/shared-wins-tie/v1"
          , FingerprintNatural $ fromIntegral milliseconds
          , tagged "shared-deadline"
              [lengthSMTLibProcessDeadlineFingerprintField sharedDeadline]
          , tagged "effective-cause"
              [FingerprintBytes $ ascii cause]
          ]
      ]
     where
      cause = case compareLengthSMTLibProcessDeadline
          sharedDeadline effectiveDeadline of
        EQ -> "shared-usable-work-deadline"
        GT -> "fresh-per-query-deadline"
        LT -> "invalid-effective-deadline"
    LengthSMTLibScopedSharedUsableWorkDeadline
        milliseconds sharedDeadline ->
      [ tagged "scoped-shared-usable-work-deadline-selection"
          [ FingerprintBytes $ ascii
              "minimum-absolute-monotonic-deadline/shared-wins-tie/v1"
          , FingerprintNatural $ fromIntegral milliseconds
          , tagged "shared-deadline"
              [lengthSMTLibProcessDeadlineFingerprintField sharedDeadline]
          , tagged "effective-cause"
              [FingerprintBytes $ ascii cause]
          , tagged "scoped-admission"
              [ FingerprintBytes $ ascii $ concat
                  [ "token-use-owner-thread/open-only-before-clock-"
                  , "configuration-and-workspace/checkpoint-and-session-"
                  , "admission-scope-unavailable-wins-expiry/v2"
                  ]
              ]
          , tagged "cooperative-checkpoint"
              [ FingerprintBytes $ ascii $ concat
                  [ "same-absolute-deadline/no-refresh/no-query-ordinal/"
                  , "no-smtlib/no-observation-count/v2"
                  ]
              ]
          ]
      ]
     where
      cause = case compareLengthSMTLibProcessDeadline
          sharedDeadline effectiveDeadline of
        EQ -> "scoped-shared-usable-work-deadline"
        GT -> "fresh-per-query-deadline"
        LT -> "invalid-effective-deadline"

queryRunTranscriptField
  :: SMTLibCausalTranscript LengthSMTLibProtocolWriteKind
  -> FingerprintField
queryRunTranscriptField transcript = tagged "causal-transcript"
  [ FingerprintBytes lengthSMTLibCausalDriverSchemaTag
  , FingerprintBytes $ ascii
      "predecessor-whitespace-observed-and-owned-by-successor/v1"
  , tagged "segment-layout"
      [ tagged "inherited-predecessor-whitespace"
          [FingerprintNatural $ byteCountBytes inherited]
      , FingerprintSequence $ map queryRunTranscriptEpochLayout epochs
      ]
  , tagged "exact-bytes"
      [FingerprintBytes $ BS.unpack $ causalTranscriptBytes transcript]
  ]
 where
  inherited = smtLibCausalTranscriptInheritedBytes transcript
  epochs = smtLibCausalTranscriptEpochs transcript

queryRunTranscriptEpochLayout
  :: SMTLibCausalTranscriptEpoch LengthSMTLibProtocolWriteKind
  -> FingerprintField
queryRunTranscriptEpochLayout epoch = tagged "write-epoch"
  [ queryProtocolWriteKindField
      $ smtLibCausalTranscriptEpochKind epoch
  , FingerprintNatural $ byteCountBytes
      $ smtLibCausalTranscriptEpochBytes epoch
  ]

queryRunIdentitySuffixFields
  :: QueryRunDomain planSubject runSubject query evidenceFamily counterexample
      modelError
  -> LengthEvaluationLimits
  -> ReplayedQueryOutcome evidenceFamily counterexample
  -> Natural
  -> Natural
  -> Natural
  -> Natural
  -> Natural
  -> [FingerprintField]
queryRunIdentitySuffixFields domain evaluationLimits outcome
    stdoutStart stdoutEnd stderrStart stderrEnd transcriptCount =
  [ queryDecodedOutcomeField outcome
  , queryReplayField domain evaluationLimits outcome
  , queryTransportCommitField stdoutStart stdoutEnd stderrStart stderrEnd
      transcriptCount
  ]

queryRunIdentityMaximumSuffixFields
  :: QueryRunDomain planSubject runSubject query evidenceFamily counterexample
      modelError
  -> LengthEvaluationLimits
  -> Natural
  -> Natural
  -> Natural
  -> [FingerprintField]
queryRunIdentityMaximumSuffixFields domain evaluationLimits stdoutMaximum
    stderrMaximum transcriptMaximum =
  [ tagged "decoded-branch"
      [ longestField $ map (FingerprintBytes . ascii)
          ["satisfiable", "unsatisfiable", "unknown"]
      , longestField $ map (FingerprintBytes . ascii)
          ["absent", "vacuous-zero-input", "framed-input-values"]
      ]
  , tagged "independent-replay"
      [ FingerprintBytes $ queryDomainReplayRole domain
      , FingerprintNatural $ fromIntegral
          $ lengthAssignmentValueBitLimit evaluationLimits
      , FingerprintNatural $ fromIntegral
          $ lengthIntermediateValueBitLimit evaluationLimits
      , longestField $ map (FingerprintBytes . ascii)
          [ "not-applicable-status"
          , "not-requested-policy"
          , "validated-counterexample"
          ]
      ]
  , queryTransportCommitField stdoutMaximum stdoutMaximum
      stderrMaximum stderrMaximum transcriptMaximum
  ]

queryDecodedOutcomeField
  :: ReplayedQueryOutcome evidenceFamily counterexample
  -> FingerprintField
queryDecodedOutcomeField outcome = tagged "decoded-branch"
  [ solverStatusField $ replayedQueryStatus outcome
  , FingerprintBytes $ ascii valuesTag
  ]
 where
  -- The v1 protocol never emits @get-value@ for a zero-input query, while the
  -- query-aware decoder requires exact arity for every emitted nonempty
  -- request.  The decoded binding spine therefore preserves the former raw
  -- frame distinction while sealing identity, before successful run
  -- construction releases that parsed representation.
  valuesTag = case outcome of
    ReplayedSatisfiableStatusOnly -> "absent"
    ReplayedSatisfiableVacuous{} -> "vacuous-zero-input"
    ReplayedSatisfiableFramed{} -> "framed-input-values"
    ReplayedUnsatisfiable -> "absent"
    ReplayedUnknown -> "absent"

queryReplayField
  :: QueryRunDomain planSubject runSubject query evidenceFamily counterexample
      modelError
  -> LengthEvaluationLimits
  -> ReplayedQueryOutcome evidenceFamily counterexample
  -> FingerprintField
queryReplayField domain evaluationLimits outcome = tagged
  "independent-replay"
  [ FingerprintBytes $ queryDomainReplayRole domain
  , FingerprintNatural $ fromIntegral
      $ lengthAssignmentValueBitLimit evaluationLimits
  , FingerprintNatural $ fromIntegral
      $ lengthIntermediateValueBitLimit evaluationLimits
  , FingerprintBytes $ ascii replayTag
  ]
 where
  replayTag = case outcome of
    ReplayedSatisfiableStatusOnly -> "not-requested-policy"
    ReplayedSatisfiableVacuous{} -> "validated-counterexample"
    ReplayedSatisfiableFramed{} -> "validated-counterexample"
    ReplayedUnsatisfiable -> "not-applicable-status"
    ReplayedUnknown -> "not-applicable-status"

replayedQueryStatus
  :: ReplayedQueryOutcome evidenceFamily counterexample
  -> SolverStatus
replayedQueryStatus outcome = case outcome of
  ReplayedSatisfiableStatusOnly -> SolverSatisfiable
  ReplayedSatisfiableVacuous{} -> SolverSatisfiable
  ReplayedSatisfiableFramed{} -> SolverSatisfiable
  ReplayedUnsatisfiable -> SolverUnsatisfiable
  ReplayedUnknown -> SolverUnknown

queryTransportCommitField
  :: Natural
  -> Natural
  -> Natural
  -> Natural
  -> Natural
  -> FingerprintField
queryTransportCommitField stdoutStart stdoutEnd stderrStart stderrEnd
    transcriptCount = tagged "transport-commit"
  [ FingerprintNatural stdoutStart
  , FingerprintNatural stdoutEnd
  , FingerprintNatural stderrStart
  , FingerprintNatural stderrEnd
  , FingerprintNatural transcriptCount
  , FingerprintBytes $ ascii
      "stdout-delta-equals-exact-transcript/no-stderr-at-commit/v1"
  , FingerprintBytes $ ascii
      "worker-open/queues-empty-final-snapshot-before-query-commit/late-predecessor-whitespace-adopted-and-charged-to-next-query/other-late-output-poisons/v1"
  ]

solverStatusField :: SolverStatus -> FingerprintField
solverStatusField status = FingerprintBytes $ ascii $ case status of
  SolverSatisfiable -> "satisfiable"
  SolverUnsatisfiable -> "unsatisfiable"
  SolverUnknown -> "unknown"

queryProtocolWriteKindField
  :: LengthSMTLibProtocolWriteKind
  -> FingerprintField
queryProtocolWriteKindField kind = FingerprintBytes $ ascii $ case kind of
  LengthSMTLibProtocolInitialQueryWrite -> "initial-query"
  LengthSMTLibProtocolInputValueWrite -> "input-value"

queryRunTranscriptMaximumFieldByteCount
  :: Natural
  -> Natural
  -> Natural
queryRunTranscriptMaximumFieldByteCount maximumBytes maximumEpochs =
  taggedFieldByteCount "causal-transcript"
    [ bytesFieldByteCount
        $ fromIntegral $ length lengthSMTLibCausalDriverSchemaTag
    , bytesFieldByteCount $ stringByteCount
        "predecessor-whitespace-observed-and-owned-by-successor/v1"
    , taggedFieldByteCount "segment-layout"
        [ taggedFieldByteCount "inherited-predecessor-whitespace"
            [naturalFieldByteCount maximumBytes]
        , sequenceFieldByteCount $ replicateNatural maximumEpochs
            $ taggedFieldByteCount "write-epoch"
                [ maximum
                    [ fingerprintFieldByteCount $ queryProtocolWriteKindField
                        LengthSMTLibProtocolInitialQueryWrite
                    , fingerprintFieldByteCount $ queryProtocolWriteKindField
                        LengthSMTLibProtocolInputValueWrite
                    ]
                , naturalFieldByteCount maximumBytes
                ]
        ]
    , taggedFieldByteCount "exact-bytes"
        [bytesFieldByteCount maximumBytes]
    ]

-- The scalar domain -----------------------------------------------------------

-- | Execute one serial, ordinal-bound query against a scoped ready worker.
-- The returned value is a live syntactic observation with independently
-- replayed counterexample evidence when the configured artifact policy asks
-- for a satisfiable model.  It is neither executable-image attestation nor a
-- proof that an unsatisfiable or unknown solver status is sound.
runLengthSMTLibReadyWorkerQuery
  :: LengthEvaluationLimits
  -> LengthSMTLibReadyWorker epoch
  -> LengthSMTLibQuery identity local
  -> IO
      (Either
        LengthSMTLibQueryRunError
        (LengthSMTLibQueryRun epoch identity local))
runLengthSMTLibReadyWorkerQuery evaluationLimits worker query =
  bimap scalarQueryRunError scalarQueryRun
    <$> runQueryRunDomain scalarQueryRunDomain evaluationLimits worker query

scalarQueryRunDomain
  :: QueryRunDomain
      LengthSMTLibProtocolPlanFingerprintSubject
      LengthSMTLibQueryRunIdentitySubject
      (LengthSMTLibQuery identity local)
      FiniteListSpineLengthV1
      ValidatedLengthCounterexample
      LengthSMTLibModelError
scalarQueryRunDomain = QueryRunDomain
  { queryDomainRolePrefix = "finite-list-spine-length"
  , queryDomainReplayRole = ascii
      "finite-list-spine-length/counterexample-replay/v1"
  , queryDomainSchemaTag = \launch deadline -> case (launch, deadline) of
      (PathnameSnapshotLaunch, FreshPerQueryDeadline) ->
        lengthSMTLibQueryRunSchemaTag
      (PathnameSnapshotLaunch, SharedUsableWorkDeadline) ->
        lengthSMTLibBudgetedQueryRunSchemaTag
      (PathnameSnapshotLaunch, ScopedSharedUsableWorkDeadline) ->
        lengthSMTLibScopedBudgetedQueryRunSchemaTag
      (DescriptorBoundLaunch, FreshPerQueryDeadline) ->
        lengthSMTLibDescriptorBoundQueryRunSchemaTag
      (DescriptorBoundLaunch, SharedUsableWorkDeadline) ->
        lengthSMTLibDescriptorBoundBudgetedQueryRunSchemaTag
      (DescriptorBoundLaunch, ScopedSharedUsableWorkDeadline) ->
        lengthSMTLibDescriptorBoundScopedBudgetedQueryRunSchemaTag
      (DescriptorBoundEffectiveIDLaunch, FreshPerQueryDeadline) ->
        lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessQueryRunSchemaTag
      (DescriptorBoundEffectiveIDLaunch, SharedUsableWorkDeadline) ->
        lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessBudgetedQueryRunSchemaTag
      (DescriptorBoundEffectiveIDLaunch, ScopedSharedUsableWorkDeadline) ->
        lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessScopedBudgetedQueryRunSchemaTag
      (DescriptorBoundExecveCheckLaunch, FreshPerQueryDeadline) ->
        lengthSMTLibDescriptorBoundExecveCheckExecutableAccessQueryRunSchemaTag
      (DescriptorBoundExecveCheckLaunch, SharedUsableWorkDeadline) ->
        lengthSMTLibDescriptorBoundExecveCheckExecutableAccessBudgetedQueryRunSchemaTag
      (DescriptorBoundExecveCheckLaunch, ScopedSharedUsableWorkDeadline) ->
        lengthSMTLibDescriptorBoundExecveCheckExecutableAccessScopedBudgetedQueryRunSchemaTag
  , queryDomainProtocolLimits = readyQueryProtocolLimits
  , queryDomainSealPlan = sealLengthSMTLibProtocolPlan
  , queryDomainInputValueRequestBytes =
      lengthSMTLibQueryInputValueRequestBytes
  , queryDomainValidateCounterexample = validateLengthSMTLibCounterexample
  , queryDomainCapabilityReuseFields = []
  , queryDomainQueryAllocationPrefix = const []
  }

scalarQueryRun
  :: QueryRunRecord
      LengthSMTLibQueryRunIdentitySubject
      FiniteListSpineLengthV1
      ValidatedLengthCounterexample
  -> LengthSMTLibQueryRun epoch identity local
scalarQueryRun (QueryRunRecord ordinal observation identity digest
    stdoutStart stdoutEnd stderrStart stderrEnd) =
  LengthSMTLibQueryRun ordinal observation identity digest
    stdoutStart stdoutEnd stderrStart stderrEnd

scalarQueryRunError
  :: QueryRunError LengthSMTLibModelError -> LengthSMTLibQueryRunError
scalarQueryRunError (QueryRunError failure cleanup) =
  LengthSMTLibQueryRunError (scalarQueryRunFailure failure) cleanup

scalarQueryRunFailure
  :: QueryRunFailure LengthSMTLibModelError -> LengthSMTLibQueryRunFailure
scalarQueryRunFailure failure = case failure of
  QueryWorkerClosing -> LengthSMTLibQueryWorkerClosing
  QueryWorkerSpent -> LengthSMTLibQueryWorkerSpent
  QueryLimitExceeded maximumQueries observed ->
    LengthSMTLibQueryLimitExceeded maximumQueries observed
  QueryProtocolPlanFailure nested ->
    LengthSMTLibQueryProtocolPlanFailure nested
  QueryProcessStdoutCapacityTooSmall remaining required ->
    LengthSMTLibQueryProcessStdoutCapacityTooSmall remaining required
  QueryBarrierCollision -> LengthSMTLibQueryBarrierCollision
  QueryDeadlineFailure nested -> LengthSMTLibQueryDeadlineFailure nested
  QueryProcessFailure nested -> LengthSMTLibQueryProcessFailure nested
  QueryProtocolFailure nested -> LengthSMTLibQueryProtocolFailure nested
  QueryTranscriptAccountingMismatch expected observed ->
    LengthSMTLibQueryTranscriptAccountingMismatch expected observed
  QueryStderrAccountingMismatch expected observed ->
    LengthSMTLibQueryStderrAccountingMismatch expected observed
  QueryModelFailure nested -> LengthSMTLibQueryModelFailure nested
  QueryModelNotCounterexample -> LengthSMTLibQueryModelNotCounterexample
  QueryRunIdentityAdmissionTooSmall maximumBytes required ->
    LengthSMTLibQueryRunIdentityAdmissionTooSmall maximumBytes required
  QueryRunIdentityFingerprintByteLimitExceeded maximumBytes observed ->
    LengthSMTLibQueryRunIdentityFingerprintByteLimitExceeded
      maximumBytes observed
  QueryInternalFailure -> LengthSMTLibQueryInternalFailure

-- The binary-product domain --------------------------------------------------

-- | Execute one nominal binary-product query in the shared serial ordinal
-- space.  The worker is reused only as the exact common QF_LIA input-value
-- transport it was admitted as; the product run binds that reuse explicitly
-- in its identity and takes no scalar behavioral authority from it.
runLengthSpinePairSMTLibReadyWorkerQuery
  :: LengthEvaluationLimits
  -> LengthSMTLibReadyWorker epoch
  -> LengthSpinePairSMTLibQuery identity local
  -> IO
      (Either
        LengthSpinePairSMTLibQueryRunError
        (LengthSpinePairSMTLibQueryRun epoch identity local))
runLengthSpinePairSMTLibReadyWorkerQuery evaluationLimits worker query =
  bimap spinePairQueryRunError spinePairQueryRun
    <$> runQueryRunDomain spinePairQueryRunDomain evaluationLimits worker
          query

spinePairQueryRunDomain
  :: QueryRunDomain
      LengthSpinePairSMTLibProtocolPlanFingerprintSubject
      LengthSpinePairSMTLibQueryRunIdentitySubject
      (LengthSpinePairSMTLibQuery identity local)
      FiniteBinaryProductSpineLengthsV1
      ValidatedLengthSpinePairCounterexample
      LengthSpinePairSMTLibModelError
spinePairQueryRunDomain = QueryRunDomain
  { queryDomainRolePrefix = "finite-binary-product-spine-lengths"
  , queryDomainReplayRole = ascii
      "finite-binary-product-spine-lengths/counterexample-replay/v1"
  , queryDomainSchemaTag = \launch deadline -> case (launch, deadline) of
      (PathnameSnapshotLaunch, FreshPerQueryDeadline) ->
        lengthSpinePairSMTLibQueryRunSchemaTag
      (PathnameSnapshotLaunch, SharedUsableWorkDeadline) ->
        lengthSpinePairSMTLibBudgetedQueryRunSchemaTag
      (PathnameSnapshotLaunch, ScopedSharedUsableWorkDeadline) ->
        lengthSpinePairSMTLibScopedBudgetedQueryRunSchemaTag
      (DescriptorBoundLaunch, FreshPerQueryDeadline) ->
        lengthSpinePairSMTLibDescriptorBoundQueryRunSchemaTag
      (DescriptorBoundLaunch, SharedUsableWorkDeadline) ->
        lengthSpinePairSMTLibDescriptorBoundBudgetedQueryRunSchemaTag
      (DescriptorBoundLaunch, ScopedSharedUsableWorkDeadline) ->
        lengthSpinePairSMTLibDescriptorBoundScopedBudgetedQueryRunSchemaTag
      (DescriptorBoundEffectiveIDLaunch, FreshPerQueryDeadline) ->
        lengthSpinePairSMTLibDescriptorBoundEffectiveIDExecutableAccessQueryRunSchemaTag
      (DescriptorBoundEffectiveIDLaunch, SharedUsableWorkDeadline) ->
        lengthSpinePairSMTLibDescriptorBoundEffectiveIDExecutableAccessBudgetedQueryRunSchemaTag
      (DescriptorBoundEffectiveIDLaunch, ScopedSharedUsableWorkDeadline) ->
        lengthSpinePairSMTLibDescriptorBoundEffectiveIDExecutableAccessScopedBudgetedQueryRunSchemaTag
      (DescriptorBoundExecveCheckLaunch, FreshPerQueryDeadline) ->
        lengthSpinePairSMTLibDescriptorBoundExecveCheckExecutableAccessQueryRunSchemaTag
      (DescriptorBoundExecveCheckLaunch, SharedUsableWorkDeadline) ->
        lengthSpinePairSMTLibDescriptorBoundExecveCheckExecutableAccessBudgetedQueryRunSchemaTag
      (DescriptorBoundExecveCheckLaunch, ScopedSharedUsableWorkDeadline) ->
        lengthSpinePairSMTLibDescriptorBoundExecveCheckExecutableAccessScopedBudgetedQueryRunSchemaTag
  , queryDomainProtocolLimits =
      const defaultLengthSpinePairSMTLibProtocolLimits
  , queryDomainSealPlan = sealLengthSpinePairSMTLibProtocolPlan
  , queryDomainInputValueRequestBytes =
      lengthSpinePairSMTLibQueryInputValueRequestBytes
  , queryDomainValidateCounterexample =
      validateLengthSpinePairSMTLibCounterexample
  , queryDomainCapabilityReuseFields =
      [ tagged "common-qf-lia-readiness-capability-reuse"
          [ FingerprintBytes $ ascii $ concat
              [ "reuse-scalar-named-ready-worker-only-as-exact-common-"
              , "qf-lia-input-value-transport-profile/"
              , "no-scalar-behavioral-authority/v1"
              ]
          , FingerprintBytes lengthSpinePairSMTLibQuerySchemaTag
          , FingerprintBytes lengthSpinePairSMTLibQueryLogic
          ]
      ]
  , queryDomainQueryAllocationPrefix = \maximumQueries ->
      [ FingerprintBytes $ ascii $ concat
          [ "one-shared-zero-based-ordinal-and-session-configured-query-"
          , "budget-across-"
          , "scalar-and-binary-product-runs/v1"
          ]
      , FingerprintNatural maximumQueries
      ]
  }

spinePairQueryRun
  :: QueryRunRecord
      LengthSpinePairSMTLibQueryRunIdentitySubject
      FiniteBinaryProductSpineLengthsV1
      ValidatedLengthSpinePairCounterexample
  -> LengthSpinePairSMTLibQueryRun epoch identity local
spinePairQueryRun (QueryRunRecord ordinal observation identity digest
    stdoutStart stdoutEnd stderrStart stderrEnd) =
  LengthSpinePairSMTLibQueryRun ordinal observation identity digest
    stdoutStart stdoutEnd stderrStart stderrEnd

spinePairQueryRunError
  :: QueryRunError LengthSpinePairSMTLibModelError
  -> LengthSpinePairSMTLibQueryRunError
spinePairQueryRunError (QueryRunError failure cleanup) =
  LengthSpinePairSMTLibQueryRunError
    (spinePairQueryRunFailure failure) cleanup

spinePairQueryRunFailure
  :: QueryRunFailure LengthSpinePairSMTLibModelError
  -> LengthSpinePairSMTLibQueryRunFailure
spinePairQueryRunFailure failure = case failure of
  QueryWorkerClosing -> LengthSpinePairSMTLibQueryWorkerClosing
  QueryWorkerSpent -> LengthSpinePairSMTLibQueryWorkerSpent
  QueryLimitExceeded maximumQueries observed ->
    LengthSpinePairSMTLibQueryLimitExceeded maximumQueries observed
  QueryProtocolPlanFailure nested ->
    LengthSpinePairSMTLibQueryProtocolPlanFailure nested
  QueryProcessStdoutCapacityTooSmall remaining required ->
    LengthSpinePairSMTLibQueryProcessStdoutCapacityTooSmall remaining required
  QueryBarrierCollision -> LengthSpinePairSMTLibQueryBarrierCollision
  QueryDeadlineFailure nested ->
    LengthSpinePairSMTLibQueryDeadlineFailure nested
  QueryProcessFailure nested ->
    LengthSpinePairSMTLibQueryProcessFailure nested
  QueryProtocolFailure nested ->
    LengthSpinePairSMTLibQueryProtocolFailure nested
  QueryTranscriptAccountingMismatch expected observed ->
    LengthSpinePairSMTLibQueryTranscriptAccountingMismatch expected observed
  QueryStderrAccountingMismatch expected observed ->
    LengthSpinePairSMTLibQueryStderrAccountingMismatch expected observed
  QueryModelFailure nested -> LengthSpinePairSMTLibQueryModelFailure nested
  QueryModelNotCounterexample ->
    LengthSpinePairSMTLibQueryModelNotCounterexample
  QueryRunIdentityAdmissionTooSmall maximumBytes required ->
    LengthSpinePairSMTLibQueryRunIdentityAdmissionTooSmall
      maximumBytes required
  QueryRunIdentityFingerprintByteLimitExceeded maximumBytes observed ->
    LengthSpinePairSMTLibQueryRunIdentityFingerprintByteLimitExceeded
      maximumBytes observed
  QueryInternalFailure -> LengthSpinePairSMTLibQueryInternalFailure


fingerprintBuilderByteCount :: [Word8] -> [Natural] -> Natural
fingerprintBuilderByteCount role fields =
  7 + naturalFieldByteCount 1 +
  bytesFieldByteCount (fromIntegral $ length role) +
  sequenceFieldByteCount fields

fingerprintFieldByteCount :: FingerprintField -> Natural
fingerprintFieldByteCount field = case field of
  FingerprintNatural value -> naturalFieldByteCount value
  FingerprintBytes bytes -> bytesFieldByteCount $ fromIntegral $ length bytes
  FingerprintSequence fields -> sequenceFieldByteCount
    $ map fingerprintFieldByteCount fields
  FingerprintTag name fields -> taggedFieldByteCountFromLength
    (fromIntegral $ length name) $ map fingerprintFieldByteCount fields
  FingerprintName _ -> error
    "query-run identity sizing does not admit structural Name fields"

naturalFieldByteCount :: Natural -> Natural
naturalFieldByteCount = sizedFieldByteCount . naturalMagnitudeByteCount

bytesFieldByteCount :: Natural -> Natural
bytesFieldByteCount = sizedFieldByteCount

sequenceFieldByteCount :: [Natural] -> Natural
sequenceFieldByteCount = sizedFieldByteCount . sum

taggedFieldByteCount :: String -> [Natural] -> Natural
taggedFieldByteCount name = taggedFieldByteCountFromLength
  $ stringByteCount name

taggedFieldByteCountFromLength :: Natural -> [Natural] -> Natural
taggedFieldByteCountFromLength nameLength fields = sizedFieldByteCount
  $ bytesFieldByteCount nameLength + sequenceFieldByteCount fields

sizedFieldByteCount :: Natural -> Natural
sizedFieldByteCount payload =
  1 + variableNaturalByteCount payload + payload

naturalMagnitudeByteCount :: Natural -> Natural
naturalMagnitudeByteCount 0 = 1
naturalMagnitudeByteCount value = go value 0
 where
  go 0 retained = retained
  go remaining retained = go (remaining `quot` 256) $ retained + 1

variableNaturalByteCount :: Natural -> Natural
variableNaturalByteCount value = go value 1
 where
  go remaining retained
    | remaining < 128 = retained
    | otherwise = go (remaining `quot` 128) $ retained + 1

longestField :: [FingerprintField] -> FingerprintField
longestField [] = FingerprintBytes []
longestField (field : fields) = foldl retainLonger field fields
 where
  retainLonger retained candidate
    | fingerprintFieldByteCount candidate >
        fingerprintFieldByteCount retained = candidate
    | otherwise = retained

replicateNatural :: Natural -> value -> [value]
replicateNatural count value = go count
 where
  go 0 = []
  go remaining = value : go (remaining - 1)

stringByteCount :: String -> Natural
stringByteCount = fromIntegral . length

byteCountBytes :: ByteString -> Natural
byteCountBytes = fromIntegral . BS.length

data Workspace = Workspace
  !ByteString
  !ByteString
  FilePath
  !(MVar WorkspaceState)

data WorkspaceState
  = WorkspaceLive !WorkspaceIdentity
  | WorkspaceFinished !LengthSMTLibSessionWorkspaceCleanupStatus

#ifndef mingw32_HOST_OS
data WorkspaceIdentity = PosixWorkspaceIdentity
  !Fd !DeviceID !FileID !UserID !FileMode
#else
data WorkspaceIdentity = WindowsWorkspaceIdentity
#endif

openConfiguredLengthSMTLibProcess
  :: LengthSMTLibProcessLimits
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> LengthSMTLibExecutionConfig
  -> Workspace
  -> IO (Either LengthSMTLibProcessError LengthSMTLibProcess)
openConfiguredLengthSMTLibProcess limits cancellation deadline execution
    (Workspace _ _ path _workspaceGate) =
  case lengthSMTLibExecutionExecutableLaunchStrategy execution of
    LengthSMTLibPathSnapshotThenDirectSpawn ->
      openLengthSMTLibProcess limits cancellation deadline
        (lengthSMTLibExecutionZ3Profile execution) path
    LengthSMTLibDescriptorBoundExecutableLaunch ->
#ifndef mingw32_HOST_OS
      withMVar _workspaceGate $ \state -> case state of
        WorkspaceFinished _ -> pure $ Left workspaceDescriptorFailure
        WorkspaceLive identity@(PosixWorkspaceIdentity descriptor _ _ _ _) -> do
          verified <- tryIOError $ verifyWorkspaceIdentity path identity
          case verified of
            Right True -> openLengthSMTLibDescriptorBoundProcess limits
              cancellation deadline (lengthSMTLibExecutionZ3Profile execution)
              path $ mkLengthSMTLibWorkingDirectoryDescriptor
                $ fromIntegral descriptor
            _ -> pure $ Left workspaceDescriptorFailure
#else
      openLengthSMTLibDescriptorBoundProcess limits cancellation deadline
        (lengthSMTLibExecutionZ3Profile execution) path
        $ mkLengthSMTLibWorkingDirectoryDescriptor (-1)
#endif
    LengthSMTLibDescriptorBoundEffectiveIDExecutableAccessLaunch ->
#ifndef mingw32_HOST_OS
      withMVar _workspaceGate $ \state -> case state of
        WorkspaceFinished _ -> pure $ Left workspaceDescriptorFailure
        WorkspaceLive identity@(PosixWorkspaceIdentity descriptor _ _ _ _) -> do
          verified <- tryIOError $ verifyWorkspaceIdentity path identity
          case verified of
            Right True ->
              openLengthSMTLibDescriptorBoundEffectiveIDExecutableAccessProcess
                limits cancellation deadline
                (lengthSMTLibExecutionZ3Profile execution) path
                $ mkLengthSMTLibWorkingDirectoryDescriptor
                    $ fromIntegral descriptor
            _ -> pure $ Left workspaceDescriptorFailure
#else
      openLengthSMTLibDescriptorBoundEffectiveIDExecutableAccessProcess
        limits cancellation deadline
        (lengthSMTLibExecutionZ3Profile execution) path
        $ mkLengthSMTLibWorkingDirectoryDescriptor (-1)
#endif
    LengthSMTLibDescriptorBoundExecveCheckExecutableAccessLaunch ->
#ifndef mingw32_HOST_OS
      withMVar _workspaceGate $ \state -> case state of
        WorkspaceFinished _ -> pure $ Left workspaceDescriptorFailure
        WorkspaceLive identity@(PosixWorkspaceIdentity descriptor _ _ _ _) -> do
          verified <- tryIOError $ verifyWorkspaceIdentity path identity
          case verified of
            Right True ->
              openLengthSMTLibDescriptorBoundExecveCheckExecutableAccessProcess
                limits cancellation deadline
                (lengthSMTLibExecutionZ3Profile execution) path
                $ mkLengthSMTLibWorkingDirectoryDescriptor
                    $ fromIntegral descriptor
            _ -> pure $ Left workspaceDescriptorFailure
#else
      openLengthSMTLibDescriptorBoundExecveCheckExecutableAccessProcess
        limits cancellation deadline
        (lengthSMTLibExecutionZ3Profile execution) path
        $ mkLengthSMTLibWorkingDirectoryDescriptor (-1)
#endif
#ifndef mingw32_HOST_OS
 where
  workspaceDescriptorFailure = LengthSMTLibProcessError
    { lengthSMTLibProcessErrorPhase =
        LengthSMTLibProcessWorkingDirectoryPhase
    , lengthSMTLibProcessErrorClass =
        LengthSMTLibProcessWorkingDirectoryUnavailable
    , lengthSMTLibProcessErrorObservedAtLeast = Nothing
    , lengthSMTLibProcessErrorCleanupStatus = Nothing
    }
#endif

-- | Open, probe, lend, and close exactly one worker.  Callback exceptions are
-- rethrown after durable cleanup has been started.  Opener/session failures
-- retain no path, command, exception text, or barrier nonce.  Package-private
-- query failures may retain bounded child response bytes, generated symbols,
-- or integer values for diagnosis; a future public facade must map those to
-- byte-free classes.  The nominal phantom separates worker epochs statically;
-- runtime lifecycle checks still reject any package-internal existential
-- wrapper used after the scope.
withLengthSMTLibReadyWorker
  :: forall result. LengthSMTLibSessionConfig
  -> (forall epoch. LengthSMTLibReadyWorker epoch -> IO result)
  -> IO (Either LengthSMTLibSessionScopeError result)
withLengthSMTLibReadyWorker = withLengthSMTLibReadyWorkerWithDeadlinePolicy
  LengthSMTLibFreshPerQueryDeadline

-- | Open, probe, lend, and close one worker as 'withLengthSMTLibReadyWorker'
-- does, but under an already captured shared usable-work deadline.  The
-- effective opener deadline and every query deadline become the earlier of
-- their fresh local deadline and the shared one (the shared deadline wins a
-- tie); expiry is checked once more just before the callback runs and again
-- as soon as it returns, before the final readiness and cleanup windows, and
-- the worker and run identities record the shared-deadline policy.
withLengthSMTLibReadyWorkerUnderDeadline
  :: forall budget result. LengthSMTLibSessionUsableWorkDeadline budget
  -> LengthSMTLibSessionConfig
  -> (forall epoch. LengthSMTLibReadyWorker epoch -> IO result)
  -> IO (Either LengthSMTLibSessionScopeError result)
withLengthSMTLibReadyWorkerUnderDeadline
    (LengthSMTLibSessionUsableWorkDeadline milliseconds deadline) =
  withLengthSMTLibReadyWorkerWithDeadlinePolicy
    $ LengthSMTLibSharedUsableWorkDeadline milliseconds deadline

-- | Admit a worker only while the v2 authority is open on its owner thread.
-- This check precedes evaluation of the session configuration and every
-- workspace or process resource acquired by the common opener.
withLengthSMTLibReadyWorkerUnderScopedDeadline
  :: forall budget result.
      LengthSMTLibSessionScopedUsableWorkDeadline budget
  -> LengthSMTLibSessionConfig
  -> (forall epoch. LengthSMTLibReadyWorker epoch -> IO result)
  -> IO (Either LengthSMTLibSessionScopeError result)
withLengthSMTLibReadyWorkerUnderScopedDeadline scoped config use = do
  admitted <- checkLengthSMTLibSessionScopedUsableWorkDeadline scoped
  case admitted of
    Left failure -> pure $ Left $ scopeError failure emptyCleanup
    Right () -> case scoped of
      LengthSMTLibSessionScopedUsableWorkDeadline
          milliseconds deadline _ _ ->
        withLengthSMTLibReadyWorkerWithDeadlinePolicy
          (LengthSMTLibScopedSharedUsableWorkDeadline milliseconds deadline)
          config use

withLengthSMTLibReadyWorkerWithDeadlinePolicy
  :: forall result. LengthSMTLibUsableWorkDeadlinePolicy
  -> LengthSMTLibSessionConfig
  -> (forall epoch. LengthSMTLibReadyWorker epoch -> IO result)
  -> IO (Either LengthSMTLibSessionScopeError result)
withLengthSMTLibReadyWorkerWithDeadlinePolicy deadlinePolicy config use =
  mask $ \restore -> do
  cancellation <- newLengthSMTLibProcessCancellation
  deadlineResult <- effectiveLengthSMTLibDeadlineAfterMilliseconds
    deadlinePolicy openerDeadline
  case deadlineResult of
    Left failure -> pure $ Left $ scopeError
      (LengthSMTLibSessionDeadlineFailure failure) emptyCleanup
    Right (LengthSMTLibEffectiveDeadline _ deadline) -> do
      rolledBackWorkspace <- newTVarIO Nothing
      allocated <- runBeforeLengthSMTLibProcessDeadline cancellation deadline
        (allocateWorkspace sessionLimits $ recordCleanup rolledBackWorkspace)
        $ \allocation -> case allocation of
            Left (_, cleanup) -> recordCleanup rolledBackWorkspace cleanup
            Right workspace -> cleanupWorkspace sessionLimits workspace
              >>= recordCleanup rolledBackWorkspace
      case allocated of
        Left failure -> do
          rollback <- readTVarIO rolledBackWorkspace
          let cleanup = emptyCleanup
                { lengthSMTLibSessionWorkspaceCleanupStatus =
                    fromMaybe LengthSMTLibSessionWorkspaceNotAllocated rollback
                }
          pure $ Left $ scopeError
            (LengthSMTLibSessionDeadlineFailure failure) cleanup
        Right (Left (failure, workspaceCleanup)) -> pure $ Left $ scopeError
          (LengthSMTLibSessionWorkspaceFailure failure)
          $ emptyCleanup
              { lengthSMTLibSessionWorkspaceCleanupStatus = workspaceCleanup }
        Right (Right workspace) -> do
          let protectWorkspace = do
                cancelLengthSMTLibProcess cancellation
                _ <- cleanupWorkspace sessionLimits workspace
                pure ()
              protectOpen = do
                cancelLengthSMTLibProcess cancellation
                -- The opener owns any partially spawned child, but an
                -- asynchronous exception does not return its cleanup status.
                -- Retain the pathname rather than risk unlinking a live cwd.
                _ <- retainWorkspace workspace
                pure ()
              continueAfterInspection preSpawn = case preSpawn of
                Left failure -> do
                  workspaceCleanup <- cleanupWorkspace sessionLimits workspace
                  pure $ Left $ scopeError
                    (LengthSMTLibSessionWorkspaceFailure failure)
                    $ emptyCleanup
                      { lengthSMTLibSessionWorkspaceCleanupStatus =
                          workspaceCleanup }
                Right () -> do
                  opened <- restore
                    (openConfiguredLengthSMTLibProcess processLimits
                      cancellation deadline execution workspace)
                    `onException` protectOpen
                  case opened of
                    Left failure -> do
                      workspaceCleanup <- cleanupWorkspaceAfterProcessFailure
                        sessionLimits workspace failure
                      pure $ Left $ scopeError
                        (LengthSMTLibSessionProcessFailure failure)
                        $ emptyCleanup
                          { lengthSMTLibSessionWorkspaceCleanupStatus =
                              workspaceCleanup }
                    Right process -> restore
                      (runOpened cancellation deadline workspace process)
                      `onException` durableOwnedCleanup
                        sessionLimits cancellation workspace process
          case deadlinePolicy of
            LengthSMTLibFreshPerQueryDeadline -> do
              preSpawn <- restore (inspectOwnedWorkspace workspace)
                `onException` protectWorkspace
              continueAfterInspection preSpawn
            LengthSMTLibSharedUsableWorkDeadline {} -> do
              rolledBackInspection <- newTVarIO Nothing
              controlled <- restore
                (runBeforeLengthSMTLibProcessDeadline cancellation deadline
                  (inspectOwnedWorkspace workspace)
                  $ \_ -> cleanupWorkspace sessionLimits workspace
                      >>= recordCleanup rolledBackInspection)
                `onException` protectWorkspace
              case controlled of
                Left failure -> do
                  rolledBack <- readTVarIO rolledBackInspection
                  workspaceCleanup <- case rolledBack of
                    Nothing -> cleanupWorkspace sessionLimits workspace
                    Just cleanup -> pure cleanup
                  pure $ Left $ scopeError
                    (LengthSMTLibSessionDeadlineFailure failure)
                    $ emptyCleanup
                        { lengthSMTLibSessionWorkspaceCleanupStatus =
                            workspaceCleanup }
                Right preSpawn -> continueAfterInspection preSpawn
            LengthSMTLibScopedSharedUsableWorkDeadline {} -> do
              rolledBackInspection <- newTVarIO Nothing
              controlled <- restore
                (runBeforeLengthSMTLibProcessDeadline cancellation deadline
                  (inspectOwnedWorkspace workspace)
                  $ \_ -> cleanupWorkspace sessionLimits workspace
                      >>= recordCleanup rolledBackInspection)
                `onException` protectWorkspace
              case controlled of
                Left failure -> do
                  rolledBack <- readTVarIO rolledBackInspection
                  workspaceCleanup <- case rolledBack of
                    Nothing -> cleanupWorkspace sessionLimits workspace
                    Just cleanup -> pure cleanup
                  pure $ Left $ scopeError
                    (LengthSMTLibSessionDeadlineFailure failure)
                    $ emptyCleanup
                        { lengthSMTLibSessionWorkspaceCleanupStatus =
                            workspaceCleanup }
                Right preSpawn -> continueAfterInspection preSpawn
 where
  LengthSMTLibSessionConfig sessionLimits processLimits capabilityLimits
      protocolLimits execution = config
  LengthSMTLibSessionLimits openerDeadline _ _ _ _ = sessionLimits

  runOpened
    :: LengthSMTLibProcessCancellation
    -> LengthSMTLibProcessDeadline
    -> Workspace
    -> LengthSMTLibProcess
    -> IO (Either LengthSMTLibSessionScopeError result)
  runOpened cancellation deadline workspace process = do
    probed <- probeReadyWorker capabilityLimits process cancellation
      deadline $ workspaceEpoch workspace
    case probed of
      Left failure -> finishFailure workspace process failure
      Right (outcome, transcript) -> do
        ready <- checkLengthSMTLibProcessReady process cancellation deadline
        case ready of
          Left failure -> finishFailure workspace process
            $ LengthSMTLibSessionProcessFailure failure
          Right () -> do
            stdoutCount <- lengthSMTLibProcessObservedStdoutBytes process
            stderrCount <- lengthSMTLibProcessObservedStderrBytes process
            let transcriptBytes =
                  smtLibCausalTranscriptByteCount transcript
            if transcriptBytes /= stdoutCount
              then finishFailure workspace process
                $ LengthSMTLibSessionTranscriptAccountingMismatch
                    transcriptBytes stdoutCount
              else case buildReadyWorkerIdentityForDeadlinePolicy
                  deadlinePolicy sessionLimits protocolLimits execution process
                  outcome transcript workspace stdoutCount stderrCount of
              Left failure -> finishFailure workspace process failure
              Right identity -> do
                queryState <- newTVarIO $ QueryLeaseState
                  QueryLeaseAccepting 0
                  (Set.fromList $ readinessBarriers $ workspaceEpoch workspace)
                  Nothing stdoutCount stderrCount
                queryGate <- newTMVarIO ()
                let transcriptDigest = SHA256.hash
                      $ causalTranscriptBytes transcript
                    postLaunchExecution =
                      retainLengthSMTLibPostLaunchExecutionPolicy execution
                    queryPolicy = case deadlinePolicy of
                      LengthSMTLibFreshPerQueryDeadline ->
                        retainLengthSMTLibReadyWorkerQueryPolicy
                          sessionLimits protocolLimits postLaunchExecution
                      LengthSMTLibSharedUsableWorkDeadline
                          milliseconds sharedDeadline ->
                        retainLengthSMTLibReadyWorkerQueryPolicyUnderDeadline
                          sessionLimits protocolLimits postLaunchExecution
                          milliseconds sharedDeadline
                      LengthSMTLibScopedSharedUsableWorkDeadline
                          milliseconds sharedDeadline ->
                        retainLengthSMTLibReadyWorkerQueryPolicyUnderScopedDeadline
                          sessionLimits protocolLimits postLaunchExecution
                          milliseconds sharedDeadline
                    worker = LengthSMTLibReadyWorker
                      { readyWorkerProcess = process
                      , readyWorkerCancellation = cancellation
                      , readyWorkerQueryPolicy = queryPolicy
                      , readyWorkerIdentity = identity
                      , readyWorkerTranscriptDigest = transcriptDigest
                      , readyWorkerStdoutAtCommit = stdoutCount
                      , readyWorkerStderrAtCommit = stderrCount
                      , readyWorkerWorkspace = workspacePath workspace
                      , readyWorkerBarrierSeed = workspaceEpoch workspace
                      , readyWorkerQueryState = queryState
                      , readyWorkerQueryGate = queryGate
                      }
                    runCallback readyWorker = do
                      callbackResult <- use readyWorker
                      finalMode <- closeQueryAdmission readyWorker
                      usableWorkComplete <-
                        checkUsableWorkDeadline deadlinePolicy
                      case usableWorkComplete of
                        Left failure -> finishFailure workspace process
                          $ LengthSMTLibSessionDeadlineFailure failure
                        Right () -> case finalMode of
                          QueryLeaseSpent ->
                            finishSuccess workspace process callbackResult
                          _ -> do
                            finalDeadline <-
                              lengthSMTLibProcessDeadlineAfterMilliseconds
                                openerDeadline
                            case finalDeadline of
                              Left failure -> finishFailure workspace process
                                $ LengthSMTLibSessionDeadlineFailure failure
                              Right checkedUntil -> do
                                stillReady <- checkLengthSMTLibProcessReady
                                  process cancellation checkedUntil
                                case stillReady of
                                  Left failure ->
                                    finishFailure workspace process
                                      $ LengthSMTLibSessionProcessFailure failure
                                  Right () -> finishSuccess
                                    workspace process callbackResult
                case deadlinePolicy of
                  LengthSMTLibFreshPerQueryDeadline -> runCallback worker
                  LengthSMTLibSharedUsableWorkDeadline {} -> do
                    constructedWorker <- evaluate worker
                    usableWorkReady <-
                      checkLengthSMTLibProcessDeadline deadline
                    case usableWorkReady of
                      Left failure -> finishFailure workspace process
                        $ LengthSMTLibSessionDeadlineFailure failure
                      Right () -> runCallback constructedWorker
                  LengthSMTLibScopedSharedUsableWorkDeadline {} -> do
                    constructedWorker <- evaluate worker
                    usableWorkReady <-
                      checkLengthSMTLibProcessDeadline deadline
                    case usableWorkReady of
                      Left failure -> finishFailure workspace process
                        $ LengthSMTLibSessionDeadlineFailure failure
                      Right () -> runCallback constructedWorker

  finishFailure workspace process failure = do
    cleanup <- cleanupOwned sessionLimits workspace process
    pure $ Left $ scopeError failure cleanup

  finishSuccess workspace process value = do
    cleanup <- cleanupOwned sessionLimits workspace process
    if successfulCleanup cleanup
      then pure $ Right value
      else pure $ Left $ scopeError LengthSMTLibSessionCleanupFailure cleanup

  recordCleanup target status =
    atomically $ writeTVar target $ Just status

checkUsableWorkDeadline
  :: LengthSMTLibUsableWorkDeadlinePolicy
  -> IO (Either LengthSMTLibProcessError ())
checkUsableWorkDeadline policy = case policy of
  LengthSMTLibFreshPerQueryDeadline -> pure $ Right ()
  LengthSMTLibSharedUsableWorkDeadline _ deadline ->
    checkLengthSMTLibProcessDeadline deadline
  LengthSMTLibScopedSharedUsableWorkDeadline _ deadline ->
    checkLengthSMTLibProcessDeadline deadline

-- Close admission before quiescing the one-query gate.  A query which already
-- owns the gate is bounded by its absolute deadline and commits without
-- reopening Closing.  The finalizer deliberately retains the gate until the
-- process has been closed, so queued package-internal callers cannot start a
-- transaction after callback return.
closeQueryAdmission
  :: LengthSMTLibReadyWorker epoch
  -> IO QueryLeaseMode
closeQueryAdmission worker = mask $ \_ -> do
  atomically $ do
    QueryLeaseState mode ordinal barriers inFlight stdoutCount stderrCount <-
      readTVar $ readyWorkerQueryState worker
    let closing = case mode of
          QueryLeaseAccepting -> QueryLeaseClosing
          _ -> mode
    writeTVar (readyWorkerQueryState worker)
      $ QueryLeaseState closing ordinal barriers inFlight
          stdoutCount stderrCount
  atomically $ takeTMVar $ readyWorkerQueryGate worker
  atomically $ do
    (QueryLeaseState mode ordinal barriers inFlight
      stdoutCount stderrCount) <- readTVar $ readyWorkerQueryState worker
    case inFlight of
      Nothing -> pure mode
      Just _ -> do
        writeTVar (readyWorkerQueryState worker) $ QueryLeaseState
          QueryLeaseSpent ordinal barriers Nothing stdoutCount stderrCount
        pure QueryLeaseSpent

scopeError
  :: LengthSMTLibSessionError
  -> LengthSMTLibSessionCleanupStatus
  -> LengthSMTLibSessionScopeError
scopeError = LengthSMTLibSessionScopeError

emptyCleanup :: LengthSMTLibSessionCleanupStatus
emptyCleanup = LengthSMTLibSessionCleanupStatus
  { lengthSMTLibSessionProcessCleanupStatus = Nothing
  , lengthSMTLibSessionProcessCleanupThrew = False
  , lengthSMTLibSessionWorkspaceCleanupStatus =
      LengthSMTLibSessionWorkspaceNotAllocated
  }

cleanupOwned
  :: LengthSMTLibSessionLimits
  -> Workspace
  -> LengthSMTLibProcess
  -> IO LengthSMTLibSessionCleanupStatus
cleanupOwned limits workspace process = do
  processStatus <- closeLengthSMTLibProcess process
  workspaceStatus <- cleanupWorkspaceAfterProcessStatus
    limits workspace processStatus
  pure LengthSMTLibSessionCleanupStatus
    { lengthSMTLibSessionProcessCleanupStatus = Just processStatus
    , lengthSMTLibSessionProcessCleanupThrew = False
    , lengthSMTLibSessionWorkspaceCleanupStatus = workspaceStatus
    }

-- | Start cleanup in an independently unmasked owner.  If the interrupted
-- thread receives another asynchronous exception while waiting, the cleanup
-- owner continues; the original scope never regains ownership of the worker.
durableOwnedCleanup
  :: LengthSMTLibSessionLimits
  -> LengthSMTLibProcessCancellation
  -> Workspace
  -> LengthSMTLibProcess
  -> IO ()
durableOwnedCleanup limits cancellation workspace process = do
  cancelLengthSMTLibProcess cancellation
  finished <- newEmptyMVar
  _ <- forkIOWithUnmask $ \unmask -> do
    processResult <- tryAny $ unmask $ closeLengthSMTLibProcess process
    case processResult of
      Left _ -> do
        _ <- tryAny $ unmask $ retainWorkspace workspace
        pure ()
      Right status -> do
        _ <- tryAny $ unmask
          $ cleanupWorkspaceAfterProcessStatus limits workspace status
        pure ()
    putMVar finished ()
  takeMVar finished

successfulCleanup :: LengthSMTLibSessionCleanupStatus -> Bool
successfulCleanup cleanup =
  not (lengthSMTLibSessionProcessCleanupThrew cleanup) &&
  case ( lengthSMTLibSessionProcessCleanupStatus cleanup
       , lengthSMTLibSessionWorkspaceCleanupStatus cleanup) of
    (Just process, LengthSMTLibSessionWorkspaceRemoved _) ->
      lengthSMTLibProcessCleanupEscalation process /=
        LengthSMTLibProcessCleanupIncomplete &&
      lengthSMTLibProcessCleanupFailureCount process == 0 &&
      lengthSMTLibProcessCleanupReadersStopped process
    _ -> False

allocateWorkspace
  :: LengthSMTLibSessionLimits
  -> (LengthSMTLibSessionWorkspaceCleanupStatus -> IO ())
  -> IO
      (Either
        ( LengthSMTLibSessionWorkspaceFailure
        , LengthSMTLibSessionWorkspaceCleanupStatus
        )
        Workspace)
allocateWorkspace
    limits@(LengthSMTLibSessionLimits _ maximumAttempts _ _ _) recordCleanup = do
  temporaryResult <- tryIOError $ getTemporaryDirectory >>= canonicalizePath
  case temporaryResult of
    Left _ -> pure $ unallocated
      LengthSMTLibSessionTemporaryDirectoryUnavailable
    Right parent | not $ isAbsolute parent -> pure $ unallocated
      LengthSMTLibSessionTemporaryDirectoryNotAbsolute
      | otherwise -> attempt parent 0
 where
  unallocated failure = Left
    (failure, LengthSMTLibSessionWorkspaceNotAllocated)

  attempt parent tried
    | tried >= maximumAttempts = pure $ unallocated
        $ LengthSMTLibSessionWorkspaceCollisionLimitExceeded maximumAttempts
    | otherwise = mask $ \restore -> do
        entropyResult <- restore $ tryIOError $ getEntropy 64
        case entropyResult of
          Left _ -> pure $ unallocated LengthSMTLibSessionEntropyUnavailable
          Right entropy | BS.length entropy /= 64 -> pure $ unallocated
              $ LengthSMTLibSessionEntropyLengthMismatch 64
                $ fromIntegral $ BS.length entropy
            | otherwise -> do
                let (barrierSeed, workspaceLabel) = BS.splitAt 32 entropy
                    path = parent </> ("djex-z3-" ++ hexadecimal workspaceLabel)
                -- Creation runs masked through descriptor acquisition.  Before
                -- a no-follow descriptor exists we deliberately retain, rather
                -- than deleting, an unverified pathname on failure.
                created <- createPrivateDirectory path
                case created of
                  Left collision | collision -> restore $ attempt parent $ tried + 1
                  Left _ -> pure $ unallocated
                    LengthSMTLibSessionWorkspaceCreationFailed
                  Right () -> do
                    -- From this point a directory may survive even if an
                    -- interruptible descriptor acquisition loses the deadline
                    -- race.  Publish conservative ownership immediately;
                    -- idempotent descriptor-backed cleanup overwrites it once
                    -- the gate exists.
                    recordCleanup
                      $ LengthSMTLibSessionWorkspaceCleanupIncomplete
                          LengthSMTLibSessionWorkspacePostconditionFailed
                    acquired <- acquireWorkspaceIdentity path
                    case acquired of
                      Left failure -> pure $ Left
                        ( failure
                        , LengthSMTLibSessionWorkspaceRetained failure Nothing
                        )
                      Right identity -> do
                        gate <- newMVar $ WorkspaceLive identity
                        let workspace = Workspace barrierSeed workspaceLabel path gate
                            rollback = cleanupWorkspace
                              limits workspace
                              >>= recordCleanup
                        inspected <- restore (inspectOwnedWorkspace workspace)
                          `onException` rollback
                        case inspected of
                          Left failure -> do
                            cleanup <- cleanupWorkspace
                              limits workspace
                            pure $ Left (failure, cleanup)
                          Right () -> pure $ Right workspace

createPrivateDirectory :: FilePath -> IO (Either Bool ())
createPrivateDirectory path = do
#ifndef mingw32_HOST_OS
  created <- tryIOError $ PosixDirectory.createDirectory path 0o700
#else
  created <- tryIOError $ createDirectory path
#endif
  pure $ case created of
    Left failure -> Left $ isAlreadyExistsError failure
    Right () -> Right ()

acquireWorkspaceIdentity
  :: FilePath
  -> IO (Either LengthSMTLibSessionWorkspaceFailure WorkspaceIdentity)
#ifndef mingw32_HOST_OS
acquireWorkspaceIdentity path = do
  opened <- tryIOError $ PosixIO.openFd path PosixIO.ReadOnly
    PosixIO.defaultFileFlags
      { PosixIO.nofollow = True
      , PosixIO.cloexec = True
      , PosixIO.directory = True
      }
  case opened of
    Left _ -> pure $ Left LengthSMTLibSessionWorkspacePostconditionFailed
    Right descriptor -> do
      inspected <- tryIOError $ do
        PosixFiles.setFdMode descriptor 0o700
        status <- PosixFiles.getFdStatus descriptor
        owner <- PosixUser.getEffectiveUserID
        pure (status, owner)
      case inspected of
        Right (status, owner)
          | validPosixWorkspaceStatus owner status -> pure $ Right
              $ PosixWorkspaceIdentity descriptor
                  (PosixFiles.deviceID status)
                  (PosixFiles.fileID status)
                  owner
                  (workspaceAccessMode status)
        _ -> do
          _ <- tryIOError $ PosixIO.closeFd descriptor
          pure $ Left LengthSMTLibSessionWorkspacePostconditionFailed
#else
acquireWorkspaceIdentity _ = pure $ Right WindowsWorkspaceIdentity
#endif

inspectOwnedWorkspace
  :: Workspace
  -> IO (Either LengthSMTLibSessionWorkspaceFailure ())
inspectOwnedWorkspace (Workspace _ _ path gate) = withMVar gate $ \state ->
  case state of
    WorkspaceFinished _ -> pure $ Left
      LengthSMTLibSessionWorkspacePostconditionFailed
    WorkspaceLive identity -> do
      inspected <- tryIOError $ do
        before <- verifyWorkspaceIdentity path identity
        empty <- workspaceDirectoryIsEmpty path
        after <- verifyWorkspaceIdentity path identity
        pure $ before && empty && after
      pure $ case inspected of
        Right True -> Right ()
        _ -> Left LengthSMTLibSessionWorkspacePostconditionFailed

workspaceDirectoryIsEmpty :: FilePath -> IO Bool
#ifndef mingw32_HOST_OS
workspaceDirectoryIsEmpty path = bracket
  (PosixDirectory.openDirStream path)
  PosixDirectory.closeDirStream
  $ \stream -> go stream (3 :: Int)
 where
  -- An empty POSIX stream contains at most dot, dot-dot, and end.  Stop after
  -- those three bounded reads; any other name proves non-emptiness without
  -- allocating an attacker-controlled directory listing.
  go _ 0 = pure False
  go stream remaining = do
    entry <- PosixDirectory.readDirStream stream
    if null entry
      then pure True
      else if entry == "." || entry == ".."
        then go stream $ remaining - 1
        else pure False
#else
workspaceDirectoryIsEmpty path = null <$> listDirectory path
#endif

verifyWorkspaceIdentity :: FilePath -> WorkspaceIdentity -> IO Bool
#ifndef mingw32_HOST_OS
verifyWorkspaceIdentity path
    (PosixWorkspaceIdentity descriptor device inode owner mode) = do
  descriptorStatus <- PosixFiles.getFdStatus descriptor
  pathStatus <- PosixFiles.getSymbolicLinkStatus path
  canonical <- canonicalizePath path
  pure $ canonical == path && isAbsolute canonical &&
    validPosixWorkspaceStatus owner descriptorStatus &&
    validPosixWorkspaceStatus owner pathStatus &&
    not (PosixFiles.isSymbolicLink pathStatus) &&
    PosixFiles.deviceID descriptorStatus == device &&
    PosixFiles.fileID descriptorStatus == inode &&
    PosixFiles.deviceID pathStatus == device &&
    PosixFiles.fileID pathStatus == inode &&
    workspaceAccessMode descriptorStatus == mode &&
    workspaceAccessMode pathStatus == mode

validPosixWorkspaceStatus
  :: UserID
  -> PosixFiles.FileStatus
  -> Bool
validPosixWorkspaceStatus owner status =
  PosixFiles.isDirectory status &&
  PosixFiles.fileOwner status == owner &&
  workspaceAccessMode status == 0o700

workspaceAccessMode :: PosixFiles.FileStatus -> FileMode
workspaceAccessMode status = PosixFiles.fileMode status
  `PosixFiles.intersectFileModes` PosixFiles.accessModes
#else
verifyWorkspaceIdentity path WindowsWorkspaceIdentity = do
  present <- doesDirectoryExist path
  symbolic <- pathIsSymbolicLink path
  canonical <- canonicalizePath path
  pure $ present && not symbolic && isAbsolute canonical && canonical == path
#endif

cleanupWorkspace
  :: LengthSMTLibSessionLimits
  -> Workspace
  -> IO LengthSMTLibSessionWorkspaceCleanupStatus
cleanupWorkspace _ (Workspace _ _ path gate) =
  modifyMVarMasked gate $ \state -> case state of
    WorkspaceFinished status -> pure (state, status)
    WorkspaceLive identity -> do
      status <- cleanupLiveWorkspace path identity
      pure (WorkspaceFinished status, status)

cleanupWorkspaceAfterProcessFailure
  :: LengthSMTLibSessionLimits
  -> Workspace
  -> LengthSMTLibProcessError
  -> IO LengthSMTLibSessionWorkspaceCleanupStatus
cleanupWorkspaceAfterProcessFailure limits workspace failure =
  case lengthSMTLibProcessErrorCleanupStatus failure of
    Nothing -> cleanupWorkspace limits workspace
    Just status -> cleanupWorkspaceAfterProcessStatus limits workspace status

cleanupWorkspaceAfterProcessStatus
  :: LengthSMTLibSessionLimits
  -> Workspace
  -> LengthSMTLibProcessCleanupStatus
  -> IO LengthSMTLibSessionWorkspaceCleanupStatus
cleanupWorkspaceAfterProcessStatus limits workspace status
  | processReleasedDirectChild status = cleanupWorkspace limits workspace
  | otherwise = retainWorkspace workspace

processReleasedDirectChild :: LengthSMTLibProcessCleanupStatus -> Bool
processReleasedDirectChild status =
  lengthSMTLibProcessCleanupEscalation status /=
    LengthSMTLibProcessCleanupIncomplete &&
  case lengthSMTLibProcessCleanupExitCode status of
    Nothing -> False
    Just _ -> True

-- | Relinquish the held directory descriptor without touching its pathname.
-- This is the conservative fallback when direct-child cleanup is incomplete.
retainWorkspace
  :: Workspace
  -> IO LengthSMTLibSessionWorkspaceCleanupStatus
retainWorkspace (Workspace _ _ _ gate) =
  modifyMVarMasked gate $ \state -> case state of
    WorkspaceFinished status -> pure (state, status)
    WorkspaceLive identity -> do
      closed <- closeWorkspaceIdentity identity
      let status
            | closed = LengthSMTLibSessionWorkspaceRetained
                LengthSMTLibSessionWorkspaceProcessCleanupIncomplete Nothing
            | otherwise = LengthSMTLibSessionWorkspaceCleanupIncomplete
                LengthSMTLibSessionWorkspaceRemovalFailed
      pure (WorkspaceFinished status, status)

cleanupLiveWorkspace
  :: FilePath
  -> WorkspaceIdentity
  -> IO LengthSMTLibSessionWorkspaceCleanupStatus
#ifndef mingw32_HOST_OS
cleanupLiveWorkspace path identity@(PosixWorkspaceIdentity descriptor _ _ _ _) = do
  verified <- tryIOError $ verifyWorkspaceIdentity path identity
  case verified of
    Left _ -> closeWith
      $ LengthSMTLibSessionWorkspaceCleanupIncomplete
          LengthSMTLibSessionWorkspacePostconditionFailed
    Right False -> closeWith
      $ LengthSMTLibSessionWorkspaceRetained
          LengthSMTLibSessionWorkspacePostconditionFailed Nothing
    Right True -> do
      removed <- tryIOError $ removeDirectory path
      case removed of
        Left _ -> closeWith $ LengthSMTLibSessionWorkspaceRetained
          LengthSMTLibSessionWorkspaceRemovalFailed Nothing
        Right () -> do
          detached <- tryIOError $ do
            status <- PosixFiles.getFdStatus descriptor
            pure $ PosixFiles.linkCount status == 0
          closeWith $ case detached of
            Right True -> LengthSMTLibSessionWorkspaceRemoved 0
            _ -> LengthSMTLibSessionWorkspaceCleanupIncomplete
              LengthSMTLibSessionWorkspaceRemovalFailed
 where
  closeWith status = do
    closed <- closeWorkspaceIdentity identity
    pure $ if closed
      then status
      else LengthSMTLibSessionWorkspaceCleanupIncomplete
        LengthSMTLibSessionWorkspaceRemovalFailed
#else
cleanupLiveWorkspace path identity = do
  verified <- tryIOError $ verifyWorkspaceIdentity path identity
  case verified of
    Right True -> do
      removed <- tryIOError $ removeDirectory path
      pure $ case removed of
        Right () -> LengthSMTLibSessionWorkspaceRemoved 0
        Left _ -> LengthSMTLibSessionWorkspaceRetained
          LengthSMTLibSessionWorkspaceRemovalFailed Nothing
    Right False -> pure $ LengthSMTLibSessionWorkspaceRetained
      LengthSMTLibSessionWorkspacePostconditionFailed Nothing
    Left _ -> pure $ LengthSMTLibSessionWorkspaceCleanupIncomplete
      LengthSMTLibSessionWorkspacePostconditionFailed
#endif

closeWorkspaceIdentity :: WorkspaceIdentity -> IO Bool
#ifndef mingw32_HOST_OS
closeWorkspaceIdentity (PosixWorkspaceIdentity descriptor _ _ _ _) = do
  closed <- tryIOError $ PosixIO.closeFd descriptor
  pure $ case closed of
    Left _ -> False
    Right () -> True
#else
closeWorkspaceIdentity WindowsWorkspaceIdentity = pure True
#endif

probeReadyWorker
  :: LengthSMTLibCapabilityLimits
  -> LengthSMTLibProcess
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> ByteString
  -> IO
      (Either
        LengthSMTLibSessionError
        ( LengthSMTLibCapabilityOutcome epoch
        , SMTLibCausalTranscript LengthSMTLibCapabilityWriteKind
        ))
probeReadyWorker limits process cancellation deadline epoch = do
  let barriers = readinessBarriers epoch
  if length (nub barriers) /= 4
    then pure $ Left LengthSMTLibSessionBarrierDerivationCollision
    else case barriers of
      [startup, check, value, ready] -> case sealLengthSMTLibCapabilityPlan limits
          (BS.unpack startup) (BS.unpack check) (BS.unpack value) (BS.unpack ready) of
        Left failure -> pure $ Left
          $ LengthSMTLibSessionCapabilityPlanFailure failure
        Right plan -> driveCapability plan process cancellation deadline
      _ -> pure $ Left LengthSMTLibSessionBarrierDerivationCollision

driveCapability
  :: LengthSMTLibCapabilityPlan epoch
  -> LengthSMTLibProcess
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> IO
      (Either
        LengthSMTLibSessionError
        ( LengthSMTLibCapabilityOutcome epoch
        , SMTLibCausalTranscript LengthSMTLibCapabilityWriteKind
        ))
driveCapability plan process cancellation deadline = do
  driven <- driveSMTLibCausalActions
    SMTLibCausalRequireEmptyBoundary
    (lengthSMTLibCapabilityPlanCumulativeOutputByteLimit plan)
    feedLengthSMTLibCapability finishLengthSMTLibCapability
    LengthSMTLibCapabilityUnexpectedPostBarrierByte
    lengthSMTLibCausalTransportOps
    (lengthSMTLibCausalTransport process cancellation deadline)
    $ startLengthSMTLibCapability plan
  pure $ case driven of
    Left failure -> Left $ mapFailure failure
    Right value -> Right value
 where
  mapFailure failure = case failure of
    SMTLibCausalTransportFailure processFailure ->
      LengthSMTLibSessionProcessFailure processFailure
    SMTLibCausalMachineFailure capabilityFailure ->
      LengthSMTLibSessionCapabilityFailure capabilityFailure
    SMTLibCausalCumulativeOutputByteLimitExceeded maximumBytes observed ->
      LengthSMTLibSessionCapabilityFailure
        $ LengthSMTLibCapabilityCumulativeOutputByteLimitExceeded
            maximumBytes observed
    SMTLibCausalInternalFailure ->
      LengthSMTLibSessionProcessFailure
        $ internalProcessFailure LengthSMTLibProcessInternalFailure

data ReadyWorkerExecutableLaunchIdentity
  = ReadyWorkerPathSnapshotLaunchIdentity
  | ReadyWorkerDescriptorBoundLaunchIdentity
  | ReadyWorkerDescriptorBoundEffectiveIDExecutableAccessLaunchIdentity
  | ReadyWorkerDescriptorBoundExecveCheckExecutableAccessLaunchIdentity

buildReadyWorkerIdentityForDeadlinePolicy
  :: LengthSMTLibUsableWorkDeadlinePolicy
  -> LengthSMTLibSessionLimits
  -> LengthSMTLibProtocolLimits
  -> LengthSMTLibExecutionConfig
  -> LengthSMTLibProcess
  -> LengthSMTLibCapabilityOutcome epoch
  -> SMTLibCausalTranscript LengthSMTLibCapabilityWriteKind
  -> Workspace
  -> Natural
  -> Natural
  -> Either
      LengthSMTLibSessionError
      (Fingerprint LengthSMTLibReadyWorkerIdentitySubject)
buildReadyWorkerIdentityForDeadlinePolicy policy sessionLimits protocol execution
    process outcome transcript workspace stdoutCount stderrCount = do
  legacy <- buildReadyWorkerIdentity sessionLimits protocol execution process
    outcome transcript workspace stdoutCount stderrCount
  let launchIdentity = readyWorkerExecutableLaunchIdentity process
  case policy of
    LengthSMTLibFreshPerQueryDeadline -> Right legacy
    LengthSMTLibSharedUsableWorkDeadline milliseconds deadline ->
      buildBudgetedReadyWorkerIdentity
        launchIdentity sessionLimits milliseconds deadline legacy
    LengthSMTLibScopedSharedUsableWorkDeadline milliseconds deadline ->
      buildScopedBudgetedReadyWorkerIdentity
        launchIdentity sessionLimits milliseconds deadline legacy

buildBudgetedReadyWorkerIdentity
  :: ReadyWorkerExecutableLaunchIdentity
  -> LengthSMTLibSessionLimits
  -> Int
  -> LengthSMTLibProcessDeadline
  -> Fingerprint LengthSMTLibReadyWorkerIdentitySubject
  -> Either
      LengthSMTLibSessionError
      (Fingerprint LengthSMTLibReadyWorkerIdentitySubject)
buildBudgetedReadyWorkerIdentity
    launchIdentity (LengthSMTLibSessionLimits _ _ _ maximumBytes _)
    milliseconds deadline legacy =
  case buildFingerprintWithin maximumBytes FingerprintBuilder
      { fingerprintBuilderVersion = 1
      , fingerprintBuilderRole = ascii $ case launchIdentity of
          ReadyWorkerPathSnapshotLaunchIdentity ->
            "finite-list-spine-length/z3-budgeted-capability-probed-ready-worker"
          ReadyWorkerDescriptorBoundLaunchIdentity -> concat
            [ "finite-list-spine-length/z3-budgeted-capability-probed-"
            , "ready-worker/sealed-main-image/v1"
            ]
          ReadyWorkerDescriptorBoundEffectiveIDExecutableAccessLaunchIdentity ->
            concat
              [ "finite-list-spine-length/z3-budgeted-capability-probed-"
              , "ready-worker/effective-id-executable-access-sealed-main-"
              , "image/v1"
              ]
          ReadyWorkerDescriptorBoundExecveCheckExecutableAccessLaunchIdentity ->
            concat
              [ "finite-list-spine-length/z3-budgeted-capability-probed-"
              , "ready-worker/execve-check-executable-access-sealed-main-"
              , "image/v1"
              ]
      , fingerprintBuilderFields =
          [ FingerprintBytes $ ascii $ case launchIdentity of
              ReadyWorkerPathSnapshotLaunchIdentity ->
                "djex-length-z3-shared-usable-work-deadline/v1"
              ReadyWorkerDescriptorBoundLaunchIdentity -> concat
                [ "djex-length-z3-sealed-main-image-ready-worker/"
                , "shared-usable-work-deadline/v1"
                ]
              ReadyWorkerDescriptorBoundEffectiveIDExecutableAccessLaunchIdentity ->
                concat
                  [ "djex-length-z3-effective-id-executable-access-sealed-"
                  , "main-image-ready-worker/shared-usable-work-deadline/v1"
                  ]
              ReadyWorkerDescriptorBoundExecveCheckExecutableAccessLaunchIdentity ->
                concat
                  [ "djex-length-z3-execve-check-executable-access-sealed-"
                  , "main-image-ready-worker/shared-usable-work-deadline/v1"
                  ]
          , tagged (case launchIdentity of
                ReadyWorkerPathSnapshotLaunchIdentity ->
                  "legacy-ready-worker-identity"
                ReadyWorkerDescriptorBoundLaunchIdentity ->
                  "descriptor-bound-ready-worker-identity"
                ReadyWorkerDescriptorBoundEffectiveIDExecutableAccessLaunchIdentity ->
                  "descriptor-bound-effective-id-executable-access-ready-worker-identity"
                ReadyWorkerDescriptorBoundExecveCheckExecutableAccessLaunchIdentity ->
                  "descriptor-bound-execve-check-executable-access-ready-worker-identity")
              [FingerprintBytes $ fingerprintCanonicalBytes legacy]
          , tagged "shared-usable-work-budget"
              [ FingerprintNatural $ fromIntegral milliseconds
              , lengthSMTLibProcessDeadlineFingerprintField deadline
              ]
          , tagged "deadline-coverage"
              [ FingerprintBytes $ ascii $ concat
                  [ "workspace-launch-capability-and-all-query-operations/"
                  , "minimum-with-fresh-local-deadline/shared-wins-tie/v1"
                  ]
              ]
          , tagged "callback-and-finalizer-boundary"
              [ FingerprintBytes $ ascii $ concat
                  [ "no-arbitrary-callback-interruption/expiry-checked-after-"
                  , "callback/fresh-final-readiness-and-cleanup-windows/v1"
                  ]
              ]
          ]
      } of
    Left (FingerprintLimitExceeded fingerprintMaximum observed) -> Left
      $ LengthSMTLibSessionIdentityFingerprintByteLimitExceeded
          fingerprintMaximum observed
    Right identity -> Right identity

buildScopedBudgetedReadyWorkerIdentity
  :: ReadyWorkerExecutableLaunchIdentity
  -> LengthSMTLibSessionLimits
  -> Int
  -> LengthSMTLibProcessDeadline
  -> Fingerprint LengthSMTLibReadyWorkerIdentitySubject
  -> Either
      LengthSMTLibSessionError
      (Fingerprint LengthSMTLibReadyWorkerIdentitySubject)
buildScopedBudgetedReadyWorkerIdentity
    launchIdentity (LengthSMTLibSessionLimits _ _ _ maximumBytes _)
    milliseconds deadline legacy =
  case buildFingerprintWithin maximumBytes FingerprintBuilder
      { fingerprintBuilderVersion = 1
      , fingerprintBuilderRole = ascii $ case launchIdentity of
          ReadyWorkerPathSnapshotLaunchIdentity -> concat
            [ "finite-list-spine-length/z3-capability-probed-ready-worker/"
            , "scoped-shared-usable-work-deadline/v2"
            ]
          ReadyWorkerDescriptorBoundLaunchIdentity -> concat
            [ "finite-list-spine-length/z3-capability-probed-ready-worker/"
            , "sealed-main-image/scoped-shared-usable-work-deadline/v1"
            ]
          ReadyWorkerDescriptorBoundEffectiveIDExecutableAccessLaunchIdentity ->
            concat
              [ "finite-list-spine-length/z3-capability-probed-ready-worker/"
              , "effective-id-executable-access-sealed-main-image/"
              , "scoped-shared-usable-work-deadline/v1"
              ]
          ReadyWorkerDescriptorBoundExecveCheckExecutableAccessLaunchIdentity ->
            concat
              [ "finite-list-spine-length/z3-capability-probed-ready-worker/"
              , "execve-check-executable-access-sealed-main-image/"
              , "scoped-shared-usable-work-deadline/v1"
              ]
      , fingerprintBuilderFields =
          [ FingerprintBytes $ ascii $ case launchIdentity of
              ReadyWorkerPathSnapshotLaunchIdentity ->
                "djex-length-z3-scoped-shared-usable-work-deadline/v2"
              ReadyWorkerDescriptorBoundLaunchIdentity -> concat
                [ "djex-length-z3-sealed-main-image-ready-worker/"
                , "scoped-shared-usable-work-deadline/v1"
                ]
              ReadyWorkerDescriptorBoundEffectiveIDExecutableAccessLaunchIdentity ->
                concat
                  [ "djex-length-z3-effective-id-executable-access-sealed-"
                  , "main-image-ready-worker/"
                  , "scoped-shared-usable-work-deadline/v1"
                  ]
              ReadyWorkerDescriptorBoundExecveCheckExecutableAccessLaunchIdentity ->
                concat
                  [ "djex-length-z3-execve-check-executable-access-sealed-"
                  , "main-image-ready-worker/"
                  , "scoped-shared-usable-work-deadline/v1"
                  ]
          , tagged (case launchIdentity of
                ReadyWorkerPathSnapshotLaunchIdentity ->
                  "legacy-ready-worker-identity"
                ReadyWorkerDescriptorBoundLaunchIdentity ->
                  "descriptor-bound-ready-worker-identity"
                ReadyWorkerDescriptorBoundEffectiveIDExecutableAccessLaunchIdentity ->
                  "descriptor-bound-effective-id-executable-access-ready-worker-identity"
                ReadyWorkerDescriptorBoundExecveCheckExecutableAccessLaunchIdentity ->
                  "descriptor-bound-execve-check-executable-access-ready-worker-identity")
              [FingerprintBytes $ fingerprintCanonicalBytes legacy]
          , tagged "scoped-shared-usable-work-budget"
              [ FingerprintNatural $ fromIntegral milliseconds
              , lengthSMTLibProcessDeadlineFingerprintField deadline
              ]
          , tagged "deadline-coverage"
              [ FingerprintBytes $ ascii $ concat
                  [ "workspace-launch-capability-and-all-query-operations/"
                  , "minimum-with-fresh-local-deadline/shared-wins-tie/v1"
                  ]
              ]
          , tagged "scoped-token-lifecycle"
              [ FingerprintBytes $ ascii $ concat
                  [ "token-use-owner-thread/open-only-before-clock-"
                  , "configuration-and-workspace/closed-on-normal-or-"
                  , "exception-owner-callback-"
                  , "exit/"
                  , "checkpoint-and-session-admission-scope-unavailable-"
                  , "wins-expiry/v2"
                  ]
              ]
          , tagged "cooperative-checkpoint"
              [ FingerprintBytes $ ascii $ concat
                  [ "same-absolute-deadline/no-refresh/no-query-ordinal/"
                  , "no-smtlib/no-observation-count/v2"
                  ]
              ]
          , tagged "callback-and-finalizer-boundary"
              [ FingerprintBytes $ ascii $ concat
                  [ "no-arbitrary-callback-interruption/expiry-checked-after-"
                  , "session-callback/fresh-final-readiness-and-cleanup-"
                  , "windows/v2"
                  ]
              ]
          ]
      } of
    Left (FingerprintLimitExceeded fingerprintMaximum observed) -> Left
      $ LengthSMTLibSessionIdentityFingerprintByteLimitExceeded
          fingerprintMaximum observed
    Right identity -> Right identity

buildReadyWorkerIdentity
  :: LengthSMTLibSessionLimits
  -> LengthSMTLibProtocolLimits
  -> LengthSMTLibExecutionConfig
  -> LengthSMTLibProcess
  -> LengthSMTLibCapabilityOutcome epoch
  -> SMTLibCausalTranscript LengthSMTLibCapabilityWriteKind
  -> Workspace
  -> Natural
  -> Natural
  -> Either
      LengthSMTLibSessionError
      (Fingerprint LengthSMTLibReadyWorkerIdentitySubject)
buildReadyWorkerIdentity
    (LengthSMTLibSessionLimits opener _ maximumQueries maximumBytes _)
    protocol execution process outcome transcript
    (Workspace barrierSeed _ workspace _)
    stdoutCount stderrCount =
  case buildFingerprintWithin maximumBytes FingerprintBuilder
      { fingerprintBuilderVersion = 1
      , fingerprintBuilderRole = ascii $ case launchIdentity of
          ReadyWorkerPathSnapshotLaunchIdentity ->
            "finite-list-spine-length/z3-capability-probed-ready-worker"
          ReadyWorkerDescriptorBoundLaunchIdentity -> concat
            [ "finite-list-spine-length/z3-capability-probed-ready-worker/"
            , "sealed-main-image/v1"
            ]
          ReadyWorkerDescriptorBoundEffectiveIDExecutableAccessLaunchIdentity ->
            concat
              [ "finite-list-spine-length/z3-capability-probed-ready-worker/"
              , "effective-id-executable-access-sealed-main-image/v1"
              ]
          ReadyWorkerDescriptorBoundExecveCheckExecutableAccessLaunchIdentity ->
            concat
              [ "finite-list-spine-length/z3-capability-probed-ready-worker/"
              , "execve-check-executable-access-sealed-main-image/v1"
              ]
      , fingerprintBuilderFields =
          [ FingerprintBytes lengthSMTLibSessionSchemaTag
          , FingerprintBytes $ case launchIdentity of
              ReadyWorkerPathSnapshotLaunchIdentity ->
                lengthSMTLibReadyWorkerSchemaTag
              ReadyWorkerDescriptorBoundLaunchIdentity ->
                lengthSMTLibDescriptorBoundReadyWorkerSchemaTag
              ReadyWorkerDescriptorBoundEffectiveIDExecutableAccessLaunchIdentity ->
                lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessReadyWorkerSchemaTag
              ReadyWorkerDescriptorBoundExecveCheckExecutableAccessLaunchIdentity ->
                lengthSMTLibDescriptorBoundExecveCheckExecutableAccessReadyWorkerSchemaTag
          , tagged "execution-policy"
              [FingerprintBytes $ fingerprintCanonicalBytes
                $ lengthSMTLibExecutionPolicyFingerprint execution]
          , lengthSMTLibProcessFingerprintField process
          , tagged "snapshot-strength"
              [FingerprintBytes $ BS.unpack
                $ case launchIdentity of
                    ReadyWorkerPathSnapshotLaunchIdentity ->
                      lengthSMTLibExecutableSnapshotStrengthTag
                    ReadyWorkerDescriptorBoundLaunchIdentity ->
                      lengthSMTLibDescriptorBoundExecutableLaunchStrengthTag
                    ReadyWorkerDescriptorBoundEffectiveIDExecutableAccessLaunchIdentity ->
                      lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchStrengthTag
                    ReadyWorkerDescriptorBoundExecveCheckExecutableAccessLaunchIdentity ->
                      lengthSMTLibDescriptorBoundExecveCheckExecutableAccessLaunchStrengthTag]
          , tagged "capability-outcome"
              [FingerprintBytes $ fingerprintCanonicalBytes
                $ lengthSMTLibCapabilityOutcomePlanFingerprint outcome]
          , tagged "capability-transcript"
              [ FingerprintBytes lengthSMTLibCausalDriverSchemaTag
              , FingerprintBytes $ ascii
                  "leading-boundary-whitespace-to-preceding-write/v1"
              , tagged "inherited-predecessor-whitespace"
                  [FingerprintBytes $ BS.unpack
                    $ smtLibCausalTranscriptInheritedBytes transcript]
              , FingerprintSequence $ map capabilityTranscriptEpochField
                  $ smtLibCausalTranscriptEpochs transcript
              ]
          , tagged "session-epoch"
              [ FingerprintBytes lengthSMTLibSessionEpochSchemaTag
              , FingerprintBytes $ BS.unpack $ barrierSeedCommitment barrierSeed
              ]
          , tagged "working-directory"
              [ FingerprintBytes lengthSMTLibSessionWorkspaceSchemaTag
              , textField workspace
              ]
          , protocolLimitsField protocol
          , tagged "session-semantic-limits"
              [ FingerprintNatural $ fromIntegral opener
              , FingerprintNatural maximumQueries
              ]
          , tagged "observed-output-at-ready-commit"
              [ FingerprintNatural stdoutCount
              , FingerprintNatural stderrCount
              , FingerprintBytes $ ascii
                  "no-stderr-byte-observed-at-commit/late-byte-poisons/v1"
              ]
          , tagged "ready-point" [FingerprintBytes $ ascii $ concat
              [ "capability-complete/process-alive/"
              , "queues-empty-point-in-time-observation/v2"
              ]]
          ]
      } of
    Left (FingerprintLimitExceeded maximumBytes' observed) -> Left
      $ LengthSMTLibSessionIdentityFingerprintByteLimitExceeded
        maximumBytes' observed
    Right identity -> Right identity
 where
  launchIdentity = readyWorkerExecutableLaunchIdentity process

readyWorkerExecutableLaunchIdentity
  :: LengthSMTLibProcess
  -> ReadyWorkerExecutableLaunchIdentity
readyWorkerExecutableLaunchIdentity process
  | lengthSMTLibProcessUsesDescriptorBoundExecveCheckExecutableAccessLaunch
      process =
      ReadyWorkerDescriptorBoundExecveCheckExecutableAccessLaunchIdentity
  | lengthSMTLibProcessUsesDescriptorBoundEffectiveIDExecutableAccessLaunch
      process =
      ReadyWorkerDescriptorBoundEffectiveIDExecutableAccessLaunchIdentity
  | lengthSMTLibProcessUsesDescriptorBoundExecutableLaunch process =
      ReadyWorkerDescriptorBoundLaunchIdentity
  | otherwise = ReadyWorkerPathSnapshotLaunchIdentity

protocolLimitsField :: LengthSMTLibProtocolLimits -> FingerprintField
protocolLimitsField limits = tagged "live-query-protocol-policy"
  [ FingerprintBytes smtLibStreamFramingSchemaTag
  , FingerprintNatural $ smtLibStreamTotalByteLimit stream
  , FingerprintNatural $ smtLibStreamFrameByteLimit stream
  , FingerprintNatural $ smtLibStreamNestingDepthLimit stream
  , FingerprintNatural $ lengthSMTLibProtocolCumulativeStdoutByteLimit limits
  ]
 where
  stream = lengthSMTLibProtocolStreamLimits limits

capabilityTranscriptEpochField
  :: SMTLibCausalTranscriptEpoch LengthSMTLibCapabilityWriteKind
  -> FingerprintField
capabilityTranscriptEpochField epoch = tagged "write-epoch"
  [ capabilityWriteKindField $ smtLibCausalTranscriptEpochKind epoch
  , FingerprintBytes $ BS.unpack bytes
  ]
 where
  bytes = smtLibCausalTranscriptEpochBytes epoch

capabilityWriteKindField :: LengthSMTLibCapabilityWriteKind -> FingerprintField
capabilityWriteKindField kind = FingerprintBytes $ ascii $ case kind of
  LengthSMTLibCapabilityStartupWrite -> "startup"
  LengthSMTLibCapabilityCheckWrite -> "check"
  LengthSMTLibCapabilityInputValueWrite -> "input-value"
  LengthSMTLibCapabilityReadyWrite -> "ready"

causalTranscriptBytes :: SMTLibCausalTranscript kind -> ByteString
causalTranscriptBytes transcript = BS.concat
  $ smtLibCausalTranscriptInheritedBytes transcript
  : map smtLibCausalTranscriptEpochBytes
      (smtLibCausalTranscriptEpochs transcript)

workspaceEpoch :: Workspace -> ByteString
workspaceEpoch (Workspace barrierSeed _ _ _) = barrierSeed

workspacePath :: Workspace -> FilePath
workspacePath (Workspace _ _ path _) = path

barrierSeedCommitment :: ByteString -> ByteString
barrierSeedCommitment barrierSeed = SHA256.hash $ BS.concat
  [ BS.pack $ ascii "djex-length-z3-barrier-seed-commitment/v1"
  , BS.singleton 0
  , barrierSeed
  ]

deriveBarrier :: ByteString -> String -> ByteString
deriveBarrier epoch role = SHA256.hash $ BS.concat
  [ BS.pack lengthSMTLibSessionEpochSchemaTag
  , BS.singleton 0
  , epoch
  , BS.singleton 0
  , BS.pack $ ascii role
  ]

queryCheckBarrierRole :: Word8
queryCheckBarrierRole = 1

queryValueBarrierRole :: Word8
queryValueBarrierRole = 2

deriveQueryBarrier :: ByteString -> Word64 -> Word8 -> ByteString
deriveQueryBarrier barrierSeed ordinal role = SHA256.hmac barrierSeed
  $ BS.concat
      [ BS.pack lengthSMTLibQueryBarrierSchemaTag
      , BS.singleton role
      , encodeWord64BE ordinal
      ]

encodeWord64BE :: Word64 -> ByteString
encodeWord64BE value = BS.pack
  [ fromIntegral $ value `shiftR` 56
  , fromIntegral $ value `shiftR` 48
  , fromIntegral $ value `shiftR` 40
  , fromIntegral $ value `shiftR` 32
  , fromIntegral $ value `shiftR` 24
  , fromIntegral $ value `shiftR` 16
  , fromIntegral $ value `shiftR` 8
  , fromIntegral value
  ]

readinessBarriers :: ByteString -> [ByteString]
readinessBarriers epoch = map (deriveBarrier epoch)
  [ "startup", "check", "input-value", "ready" ]

internalProcessFailure
  :: LengthSMTLibProcessFailureClass
  -> LengthSMTLibProcessError
internalProcessFailure failure = LengthSMTLibProcessError
  { lengthSMTLibProcessErrorPhase = LengthSMTLibProcessReadyPhase
  , lengthSMTLibProcessErrorClass = failure
  , lengthSMTLibProcessErrorObservedAtLeast = Nothing
  , lengthSMTLibProcessErrorCleanupStatus = Nothing
  }

tryAny :: IO value -> IO (Either SomeException value)
tryAny = try

hexadecimal :: ByteString -> String
hexadecimal = concatMap byteHex . BS.unpack
 where
  byteHex byte =
    [ intToDigit $ fromIntegral byte `div` 16
    , intToDigit $ fromIntegral byte `mod` 16
    ]

tagged :: String -> [FingerprintField] -> FingerprintField
tagged name = FingerprintTag $ ascii name

textField :: String -> FingerprintField
textField = FingerprintBytes . concatMap encodeCharacter
 where
  encodeCharacter character =
    let value = ord character
    in [ fromIntegral $ value `div` 16777216
       , fromIntegral $ value `div` 65536
       , fromIntegral $ value `div` 256
       , fromIntegral value
       ]

ascii :: String -> [Word8]
ascii = map $ fromIntegral . ord
