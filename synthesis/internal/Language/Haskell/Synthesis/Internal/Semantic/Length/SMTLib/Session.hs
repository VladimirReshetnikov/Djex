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
  , LengthSMTLibReadyWorker
  , LengthSMTLibReadyWorkerIdentitySubject
  , withLengthSMTLibReadyWorker
  , withLengthSMTLibReadyWorkerUnderDeadline
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

import Control.Concurrent (forkIOWithUnmask)
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
import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (intToDigit, ord)
import Data.List (nub)
import Data.Maybe (isJust)
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
  , LengthSMTLibPostLaunchExecutionPolicy
  , lengthSMTLibExecutionPolicyFingerprint
  , lengthSMTLibExecutionZ3Profile
  , lengthSMTLibPostLaunchArtifactPolicy
  , lengthSMTLibPostLaunchHostDeadlineMilliseconds
  , retainLengthSMTLibPostLaunchExecutionPolicy
  )
import Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol
  ( LengthSMTLibProtocolDecoded
  , LengthSMTLibProtocolError (..)
  , LengthSMTLibProtocolLimits
  , LengthSMTLibProtocolPlan
  , LengthSMTLibProtocolPlanError
  , LengthSMTLibProtocolWriteKind (..)
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
  ( LengthSpinePairSMTLibProtocolDecoded
  , LengthSpinePairSMTLibProtocolError
  , LengthSpinePairSMTLibProtocolPlan
  , LengthSpinePairSMTLibProtocolPlanError
  , LengthSpinePairSMTLibProtocolWriteKind
  , defaultLengthSpinePairSMTLibProtocolLimits
  , feedLengthSpinePairSMTLibProtocol
  , finishLengthSpinePairSMTLibProtocol
  , lengthSpinePairSMTLibProtocolInputValueWriteBytes
  , lengthSpinePairSMTLibProtocolPlanArtifactPolicy
  , lengthSpinePairSMTLibProtocolPlanCumulativeStdoutByteLimit
  , lengthSpinePairSMTLibProtocolPlanFingerprint
  , lengthSpinePairSMTLibProtocolPlanMinimumStdoutByteCount
  , lengthSpinePairSMTLibProtocolPlanQuery
  , lengthSpinePairSMTLibProtocolDecodedObservation
  , sealLengthSpinePairSMTLibProtocolPlan
  , startLengthSpinePairSMTLibProtocol
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
  , lengthSMTLibProcessDeadlineAfterMilliseconds
  , lengthSMTLibProcessDeadlineFingerprintField
  , lengthSMTLibProcessFingerprintField
  , lengthSMTLibProcessLimits
  , lengthSMTLibProcessObservedStderrBytes
  , lengthSMTLibProcessObservedStdoutBytes
  , lengthSMTLibProcessStderrByteLimit
  , lengthSMTLibProcessStdoutByteLimit
  , lengthSMTLibProcessSnapshot
  , newLengthSMTLibProcessCancellation
  , compareLengthSMTLibProcessDeadline
  , minimumLengthSMTLibProcessDeadline
  , openLengthSMTLibProcess
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
  ( LengthSMTLibModelError
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

lengthSMTLibSessionSchemaTag :: [Word8]
lengthSMTLibSessionSchemaTag =
  ascii "djex-length-z3-scoped-worker-session/v3"

-- The driver implementation is now domain-neutral, but the historical
-- Length/Z3 literal remains owned by this identity layer so the abstraction
-- move cannot change ready-worker or query-run fingerprint bytes.
lengthSMTLibCausalDriverSchemaTag :: [Word8]
lengthSMTLibCausalDriverSchemaTag = ascii
  "djex-length-z3-causal-byte-stream-driver/v1"

lengthSMTLibReadyWorkerSchemaTag :: [Word8]
lengthSMTLibReadyWorkerSchemaTag =
  ascii "djex-length-z3-capability-probed-ready-worker/v4"

-- | Each exclusive workspace attempt samples independent secret and public
-- 256-bit halves.  The public half names the directory; readiness barriers are
-- SHA-256-domain-separated and query barriers use HMAC-SHA256 over fixed-width
-- ordinals.  Every emitted role is also checked against a bounded session-wide
-- set.  The child can observe its cwd without learning the barrier seed.  This
-- is an entropy/HMAC/collision-check claim, not mathematical global freshness.
lengthSMTLibSessionEpochSchemaTag :: [Word8]
lengthSMTLibSessionEpochSchemaTag =
  ascii "os-entropy-512/split-public-label-secret-barrier-seed/v3"

lengthSMTLibQueryBarrierSchemaTag :: [Word8]
lengthSMTLibQueryBarrierSchemaTag = ascii
  "hmac-sha256/secret-seed/fixed-role/u64be-ordinal/v1"

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

data LengthSMTLibSessionLimitField
  = LengthSMTLibSessionOpenerDeadlineMilliseconds
  | LengthSMTLibSessionWorkspaceAllocationAttempts
  | LengthSMTLibSessionMaximumQueries
  | LengthSMTLibSessionIdentityFingerprintBytes
  | LengthSMTLibSessionQueryRunIdentityFingerprintBytes
  deriving (Bounded, Enum, Eq, Ord, Show)

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

defaultLengthSMTLibSessionLimitSource :: LengthSMTLibSessionLimitSource
defaultLengthSMTLibSessionLimitSource = LengthSMTLibSessionLimitSource
  { lengthSMTLibSessionLimitSourceOpenerDeadlineMilliseconds = 5000
  , lengthSMTLibSessionLimitSourceWorkspaceAllocationAttempts = 8
  , lengthSMTLibSessionLimitSourceMaximumQueries = 64
  , lengthSMTLibSessionLimitSourceIdentityFingerprintBytes = 262144
  , lengthSMTLibSessionLimitSourceQueryRunIdentityFingerprintBytes = 2097152
  }

data LengthSMTLibSessionLimits = LengthSMTLibSessionLimits
  !Int !Natural !Natural !Natural !Natural

instance NFData LengthSMTLibSessionLimits where
  rnf (LengthSMTLibSessionLimits deadline attempts queries identity runIdentity) =
    rnf deadline `seq` rnf attempts `seq` rnf queries `seq` rnf identity `seq`
    rnf runIdentity

data LengthSMTLibSessionConfigError
  = LengthSMTLibSessionNonPositiveLimit
      !LengthSMTLibSessionLimitField !Natural
  | LengthSMTLibSessionLimitConversionOverflow
      !LengthSMTLibSessionLimitField !Natural
  | LengthSMTLibSessionCapabilityAdmissionFailure
      !LengthSMTLibCapabilityPlanError
  | LengthSMTLibSessionProcessStdoutAdmissionTooSmall !Natural !Natural
  deriving (Eq, Ord, Show)

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

defaultLengthSMTLibSessionLimits :: LengthSMTLibSessionLimits
defaultLengthSMTLibSessionLimits =
  LengthSMTLibSessionLimits 5000 8 64 262144 2097152

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

data LengthSMTLibSessionWorkspaceCleanupStatus
  = LengthSMTLibSessionWorkspaceNotAllocated
  | LengthSMTLibSessionWorkspaceRemoved !Natural
  | LengthSMTLibSessionWorkspaceRetained
      !LengthSMTLibSessionWorkspaceFailure !(Maybe Natural)
  | LengthSMTLibSessionWorkspaceCleanupIncomplete
      !LengthSMTLibSessionWorkspaceFailure
  deriving (Eq, Ord, Show)

data LengthSMTLibSessionCleanupStatus = LengthSMTLibSessionCleanupStatus
  { lengthSMTLibSessionProcessCleanupStatus
      :: !(Maybe LengthSMTLibProcessCleanupStatus)
  , lengthSMTLibSessionProcessCleanupThrew :: !Bool
  , lengthSMTLibSessionWorkspaceCleanupStatus
      :: !LengthSMTLibSessionWorkspaceCleanupStatus
  }
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

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

data LengthSMTLibUsableWorkDeadlinePolicy
  = LengthSMTLibFreshPerQueryDeadline
  | LengthSMTLibSharedUsableWorkDeadline
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

data LengthSpinePairSMTLibQueryRunError = LengthSpinePairSMTLibQueryRunError
  { lengthSpinePairSMTLibQueryRunPrimaryFailure
      :: !LengthSpinePairSMTLibQueryRunFailure
  , lengthSpinePairSMTLibQueryRunProcessCleanupStatus
      :: !(Maybe LengthSMTLibProcessCleanupStatus)
  }
  deriving (Eq, Ord, Show)

data LengthSpinePairSMTLibQueryRunIdentitySubject

type LengthSpinePairSMTLibQueryRunObservation = SolverObservation
  (Maybe
    (BehavioralEvidence
      FiniteBinaryProductSpineLengthsV1
      ValidatedLengthSpinePairCounterexample))
  ()
  ()

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

lengthSMTLibReadyWorkerIdentityFingerprint
  :: LengthSMTLibReadyWorker epoch
  -> Fingerprint LengthSMTLibReadyWorkerIdentitySubject
lengthSMTLibReadyWorkerIdentityFingerprint = readyWorkerIdentity

lengthSMTLibReadyWorkerIdentityFingerprintField
  :: LengthSMTLibReadyWorker epoch
  -> FingerprintField
lengthSMTLibReadyWorkerIdentityFingerprintField worker = FingerprintTag
  (ascii "capability-probed-ready-worker-identity")
  [FingerprintBytes $ fingerprintCanonicalBytes $ readyWorkerIdentity worker]

lengthSMTLibReadyWorkerExecutableSHA256
  :: LengthSMTLibReadyWorker epoch
  -> ByteString
lengthSMTLibReadyWorkerExecutableSHA256 =
  lengthSMTLibExecutableSnapshotSHA256 .
    lengthSMTLibProcessSnapshot . readyWorkerProcess

lengthSMTLibReadyWorkerExecutableByteCount
  :: LengthSMTLibReadyWorker epoch
  -> Natural
lengthSMTLibReadyWorkerExecutableByteCount =
  lengthSMTLibExecutableSnapshotByteCount .
    lengthSMTLibProcessSnapshot . readyWorkerProcess

lengthSMTLibReadyWorkerExecutableSnapshotStrengthTag
  :: LengthSMTLibReadyWorker epoch
  -> ByteString
lengthSMTLibReadyWorkerExecutableSnapshotStrengthTag _ =
  lengthSMTLibExecutableSnapshotStrengthTag

lengthSMTLibReadyWorkerCapabilityTranscriptSHA256
  :: LengthSMTLibReadyWorker epoch
  -> ByteString
lengthSMTLibReadyWorkerCapabilityTranscriptSHA256 = readyWorkerTranscriptDigest

lengthSMTLibReadyWorkerCapabilityTranscriptByteCount
  :: LengthSMTLibReadyWorker epoch
  -> Natural
-- Readiness construction admits the worker only after proving the causal
-- transcript count equals this immutable ready-point stdout boundary.
lengthSMTLibReadyWorkerCapabilityTranscriptByteCount =
  readyWorkerStdoutAtCommit

lengthSMTLibReadyWorkerObservedStdoutBytes
  :: LengthSMTLibReadyWorker epoch
  -> Natural
lengthSMTLibReadyWorkerObservedStdoutBytes = readyWorkerStdoutAtCommit

lengthSMTLibReadyWorkerObservedStderrBytes
  :: LengthSMTLibReadyWorker epoch
  -> Natural
lengthSMTLibReadyWorkerObservedStderrBytes = readyWorkerStderrAtCommit

lengthSMTLibReadyWorkerWorkingDirectory
  :: LengthSMTLibReadyWorker epoch
  -> FilePath
lengthSMTLibReadyWorkerWorkingDirectory = readyWorkerWorkspace

lengthSMTLibQueryRunOrdinal
  :: LengthSMTLibQueryRun epoch identity local
  -> Natural
lengthSMTLibQueryRunOrdinal
    (LengthSMTLibQueryRun value _ _ _ _ _ _ _) = value

lengthSMTLibQueryRunObservation
  :: LengthSMTLibQueryRun epoch identity local
  -> LengthSMTLibQueryRunObservation
lengthSMTLibQueryRunObservation
    (LengthSMTLibQueryRun _ observation _ _ _ _ _ _) = observation

lengthSMTLibQueryRunIdentityFingerprint
  :: LengthSMTLibQueryRun epoch identity local
  -> Fingerprint LengthSMTLibQueryRunIdentitySubject
lengthSMTLibQueryRunIdentityFingerprint
    (LengthSMTLibQueryRun _ _ value _ _ _ _ _) = value

lengthSMTLibQueryRunIdentityFingerprintField
  :: LengthSMTLibQueryRun epoch identity local
  -> FingerprintField
lengthSMTLibQueryRunIdentityFingerprintField run = tagged "query-run-identity"
  [FingerprintBytes $ fingerprintCanonicalBytes
    $ lengthSMTLibQueryRunIdentityFingerprint run]

lengthSMTLibQueryRunTranscriptSHA256
  :: LengthSMTLibQueryRun epoch identity local
  -> ByteString
lengthSMTLibQueryRunTranscriptSHA256
    (LengthSMTLibQueryRun _ _ _ value _ _ _ _) = value

lengthSMTLibQueryRunTranscriptByteCount
  :: LengthSMTLibQueryRun epoch identity local
  -> Natural
-- The private constructor is reached only after exact transcript accounting
-- proves that these immutable boundaries are ordered and their delta is the
-- causal transcript count.
lengthSMTLibQueryRunTranscriptByteCount
    (LengthSMTLibQueryRun _ _ _ _ stdoutStart stdoutEnd _ _) =
      stdoutEnd - stdoutStart

lengthSMTLibQueryRunStdoutStart
  :: LengthSMTLibQueryRun epoch identity local
  -> Natural
lengthSMTLibQueryRunStdoutStart
    (LengthSMTLibQueryRun _ _ _ _ value _ _ _) = value

lengthSMTLibQueryRunStdoutEnd
  :: LengthSMTLibQueryRun epoch identity local
  -> Natural
lengthSMTLibQueryRunStdoutEnd
    (LengthSMTLibQueryRun _ _ _ _ _ value _ _) = value

lengthSMTLibQueryRunStderrStart
  :: LengthSMTLibQueryRun epoch identity local
  -> Natural
lengthSMTLibQueryRunStderrStart
    (LengthSMTLibQueryRun _ _ _ _ _ _ value _) = value

lengthSMTLibQueryRunStderrEnd
  :: LengthSMTLibQueryRun epoch identity local
  -> Natural
lengthSMTLibQueryRunStderrEnd
    (LengthSMTLibQueryRun _ _ _ _ _ _ _ value) = value

lengthSpinePairSMTLibQueryRunOrdinal
  :: LengthSpinePairSMTLibQueryRun epoch identity local
  -> Natural
lengthSpinePairSMTLibQueryRunOrdinal
    (LengthSpinePairSMTLibQueryRun value _ _ _ _ _ _ _) = value

lengthSpinePairSMTLibQueryRunObservation
  :: LengthSpinePairSMTLibQueryRun epoch identity local
  -> LengthSpinePairSMTLibQueryRunObservation
lengthSpinePairSMTLibQueryRunObservation
    (LengthSpinePairSMTLibQueryRun _ observation _ _ _ _ _ _) = observation

lengthSpinePairSMTLibQueryRunIdentityFingerprint
  :: LengthSpinePairSMTLibQueryRun epoch identity local
  -> Fingerprint LengthSpinePairSMTLibQueryRunIdentitySubject
lengthSpinePairSMTLibQueryRunIdentityFingerprint
    (LengthSpinePairSMTLibQueryRun _ _ value _ _ _ _ _) = value

lengthSpinePairSMTLibQueryRunIdentityFingerprintField
  :: LengthSpinePairSMTLibQueryRun epoch identity local
  -> FingerprintField
lengthSpinePairSMTLibQueryRunIdentityFingerprintField run =
  tagged "spine-pair-query-run-identity"
    [FingerprintBytes $ fingerprintCanonicalBytes
      $ lengthSpinePairSMTLibQueryRunIdentityFingerprint run]

lengthSpinePairSMTLibQueryRunTranscriptSHA256
  :: LengthSpinePairSMTLibQueryRun epoch identity local
  -> ByteString
lengthSpinePairSMTLibQueryRunTranscriptSHA256
    (LengthSpinePairSMTLibQueryRun _ _ _ value _ _ _ _) = value

lengthSpinePairSMTLibQueryRunTranscriptByteCount
  :: LengthSpinePairSMTLibQueryRun epoch identity local
  -> Natural
lengthSpinePairSMTLibQueryRunTranscriptByteCount
    (LengthSpinePairSMTLibQueryRun _ _ _ _ stdoutStart stdoutEnd _ _) =
      stdoutEnd - stdoutStart

lengthSpinePairSMTLibQueryRunStdoutStart
  :: LengthSpinePairSMTLibQueryRun epoch identity local
  -> Natural
lengthSpinePairSMTLibQueryRunStdoutStart
    (LengthSpinePairSMTLibQueryRun _ _ _ _ value _ _ _) = value

lengthSpinePairSMTLibQueryRunStdoutEnd
  :: LengthSpinePairSMTLibQueryRun epoch identity local
  -> Natural
lengthSpinePairSMTLibQueryRunStdoutEnd
    (LengthSpinePairSMTLibQueryRun _ _ _ _ _ value _ _) = value

lengthSpinePairSMTLibQueryRunStderrStart
  :: LengthSpinePairSMTLibQueryRun epoch identity local
  -> Natural
lengthSpinePairSMTLibQueryRunStderrStart
    (LengthSpinePairSMTLibQueryRun _ _ _ _ _ _ value _) = value

lengthSpinePairSMTLibQueryRunStderrEnd
  :: LengthSpinePairSMTLibQueryRun epoch identity local
  -> Natural
lengthSpinePairSMTLibQueryRunStderrEnd
    (LengthSpinePairSMTLibQueryRun _ _ _ _ _ _ _ value) = value

data PreparedLengthSMTLibQueryRun epoch identity local =
  PreparedLengthSMTLibQueryRun
    !Natural
    !ByteString
    !ByteString
    !(LengthSMTLibProtocolPlan identity local)
    !LengthEvaluationLimits
    !LengthSMTLibProcessDeadline

type role PreparedLengthSMTLibQueryRun nominal nominal nominal

-- The last-committed accounting anchors are read from the lease state in the
-- same STM transaction which burns the ordinal and both marker roles.  Only
-- this receipt can cross the execution boundary; a merely prepared plan has
-- no transport-commit anchor.
data ReservedLengthSMTLibQueryRun epoch identity local =
  ReservedLengthSMTLibQueryRun
    !(PreparedLengthSMTLibQueryRun epoch identity local)
    !Natural
    !Natural

type role ReservedLengthSMTLibQueryRun nominal nominal nominal

-- | Execute one serial, ordinal-bound query against a scoped ready worker.
-- The returned value is a live syntactic observation with independently
-- replayed counterexample evidence when the configured artifact policy asks
-- for a satisfiable model.  It is neither executable-image attestation nor a
-- proof that an unsatisfiable or unknown solver status is sound.
runLengthSMTLibReadyWorkerQuery
  :: forall epoch identity local.
     LengthEvaluationLimits
  -> LengthSMTLibReadyWorker epoch
  -> LengthSMTLibQuery identity local
  -> IO
      (Either
        LengthSMTLibQueryRunError
        (LengthSMTLibQueryRun epoch identity local))
runLengthSMTLibReadyWorkerQuery evaluationLimits worker query =
  mask $ \restore -> do
    deadlineResult <- effectiveLengthSMTLibDeadlineAfterMilliseconds
      (readyQueryUsableWorkDeadlinePolicy $ readyWorkerQueryPolicy worker)
      $ lengthSMTLibPostLaunchHostDeadlineMilliseconds postLaunchExecution
    case deadlineResult of
      Left failure -> pure $ queryRunLeft
        $ LengthSMTLibQueryDeadlineFailure failure
      Right (LengthSMTLibEffectiveDeadline _ deadline) -> do
        acquired <- acquireLengthSMTLibQueryGate worker deadline
        case acquired of
          Left failure@LengthSMTLibQueryProcessFailure {} ->
            spendLengthSMTLibQueryWorker worker failure
          Left failure@LengthSMTLibQueryInternalFailure ->
            spendLengthSMTLibQueryWorker worker failure
          Left failure -> pure $ queryRunLeft failure
          Right () -> finally
            (runWithGate restore deadline)
            (atomically $ putTMVar (readyWorkerQueryGate worker) ())
 where
  postLaunchExecution = readyQueryPostLaunchExecution
    $ readyWorkerQueryPolicy worker

  runWithGate restore deadline = do
    prepared <- prepareLengthSMTLibQueryRun
      evaluationLimits worker query deadline
    case prepared of
      Left (mustSpend, failure)
        | mustSpend -> spendLengthSMTLibQueryWorker worker failure
        | otherwise -> pure $ queryRunLeft failure
      Right preparation -> do
        reserved <- reservePreparedLengthSMTLibQueryRun worker preparation
        case reserved of
          Left LengthSMTLibQueryBarrierCollision ->
            spendLengthSMTLibQueryWorker worker
              LengthSMTLibQueryBarrierCollision
          Left LengthSMTLibQueryInternalFailure ->
            spendLengthSMTLibQueryWorker worker
              LengthSMTLibQueryInternalFailure
          Left failure -> pure $ queryRunLeft failure
          Right reservation -> do
            executed <- restore
              (executeReservedLengthSMTLibQueryRun worker reservation)
              `onException` abandonLengthSMTLibQueryWorker worker
            case executed of
              Left failure -> spendLengthSMTLibQueryWorker worker failure
              Right run -> do
                committed <- commitLengthSMTLibQueryRun worker reservation run
                if committed
                  then pure $ Right run
                  else spendLengthSMTLibQueryWorker worker
                    LengthSMTLibQueryInternalFailure

queryRunLeft
  :: LengthSMTLibQueryRunFailure
  -> Either LengthSMTLibQueryRunError value
queryRunLeft failure = Left $ LengthSMTLibQueryRunError failure Nothing

acquireLengthSMTLibQueryGate
  :: LengthSMTLibReadyWorker epoch
  -> LengthSMTLibProcessDeadline
  -> IO (Either LengthSMTLibQueryRunFailure ())
acquireLengthSMTLibQueryGate worker deadline = mask $ \_ -> loop
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
            state <- atomically $ readTVar stateVariable
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
  -> Either LengthSMTLibQueryRunFailure ()
queryLeaseAdmission maximumQueries
    (QueryLeaseState mode nextOrdinal _ _ _ _) = case mode of
  QueryLeaseClosing -> Left LengthSMTLibQueryWorkerClosing
  QueryLeaseSpent -> Left LengthSMTLibQueryWorkerSpent
  QueryLeaseAccepting
    | nextOrdinal >= maximumQueries -> Left
        $ LengthSMTLibQueryLimitExceeded maximumQueries (maximumQueries + 1)
    | otherwise -> Right ()

queryGateProcessFailure
  :: QueryLeaseState
  -> LengthSMTLibProcessError
  -> LengthSMTLibQueryRunFailure
queryGateProcessFailure (QueryLeaseState mode _ _ _ _ _) failure = case mode of
  QueryLeaseClosing -> LengthSMTLibQueryWorkerClosing
  QueryLeaseSpent -> LengthSMTLibQueryWorkerSpent
  _ -> queryProcessFailure failure

prepareLengthSMTLibQueryRun
  :: LengthEvaluationLimits
  -> LengthSMTLibReadyWorker epoch
  -> LengthSMTLibQuery identity local
  -> LengthSMTLibProcessDeadline
  -> IO
      (Either
        (Bool, LengthSMTLibQueryRunFailure)
        (PreparedLengthSMTLibQueryRun epoch identity local))
prepareLengthSMTLibQueryRun evaluationLimits worker query deadline = do
  state@(QueryLeaseState _ ordinal used inFlight _ _) <-
    atomically $ readTVar $ readyWorkerQueryState worker
  case queryLeaseAdmission maximumQueries state of
    Left failure -> pure $ Left (False, failure)
    Right () | isJust inFlight -> pure
      $ Left (True, LengthSMTLibQueryInternalFailure)
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
  protocolLimits = readyQueryProtocolLimits policy
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
          isJust (lengthSMTLibQueryInputValueRequestBytes query)
        valueNonce
          | needsValueBarrier = Just $ BS.unpack valueBarrier
          | otherwise = Nothing
    case sealLengthSMTLibProtocolPlan protocolLimits postLaunchExecution query
        (BS.unpack checkBarrier) valueNonce of
      Left failure -> pure $ Left
        (False, LengthSMTLibQueryProtocolPlanFailure failure)
      Right plan
        | Set.member checkBarrier used || Set.member valueBarrier used ||
            checkBarrier == valueBarrier -> pure
              $ Left (True, LengthSMTLibQueryBarrierCollision)
        | otherwise -> do
            observedStdout <- lengthSMTLibProcessObservedStdoutBytes process
            let required = lengthSMTLibProtocolPlanMinimumStdoutByteCount plan
                remaining
                  | observedStdout >= transportMaximum = 0
                  | otherwise = transportMaximum - observedStdout
            if required > remaining
              then pure $ Left (False,
                LengthSMTLibQueryProcessStdoutCapacityTooSmall
                  remaining required)
              else case admitLengthSMTLibQueryRunIdentity
                  worker plan
                  evaluationLimits ordinal deadline
                  checkBarrier valueBarrier of
                Left failure -> pure $ Left (False, failure)
                Right () -> pure $ Right $ PreparedLengthSMTLibQueryRun
                  ordinal checkBarrier valueBarrier plan evaluationLimits deadline

reservePreparedLengthSMTLibQueryRun
  :: LengthSMTLibReadyWorker epoch
  -> PreparedLengthSMTLibQueryRun epoch identity local
  -> IO
      (Either
        LengthSMTLibQueryRunFailure
        (ReservedLengthSMTLibQueryRun epoch identity local))
reservePreparedLengthSMTLibQueryRun worker
    preparation@(PreparedLengthSMTLibQueryRun
      ordinal checkBarrier valueBarrier _ _ _) = atomically $ do
  state@(QueryLeaseState mode nextOrdinal used inFlight stdoutCount stderrCount) <-
    readTVar $ readyWorkerQueryState worker
  case queryLeaseAdmission maximumQueries state of
    Left failure -> pure $ Left failure
    Right ()
      | mode /= QueryLeaseAccepting || nextOrdinal /= ordinal ||
          isJust inFlight -> pure $ Left LengthSMTLibQueryInternalFailure
      | Set.member checkBarrier used || Set.member valueBarrier used ||
          checkBarrier == valueBarrier -> pure
            $ Left LengthSMTLibQueryBarrierCollision
      | otherwise -> do
          writeTVar (readyWorkerQueryState worker) $ QueryLeaseState
            QueryLeaseAccepting (ordinal + 1)
            (Set.insert valueBarrier $ Set.insert checkBarrier used)
            (Just ordinal) stdoutCount stderrCount
          pure $ Right $ ReservedLengthSMTLibQueryRun
            preparation stdoutCount stderrCount
 where
  maximumQueries = readyQueryMaximumQueries $ readyWorkerQueryPolicy worker

commitLengthSMTLibQueryRun
  :: LengthSMTLibReadyWorker epoch
  -> ReservedLengthSMTLibQueryRun epoch identity local
  -> LengthSMTLibQueryRun epoch identity local
  -> IO Bool
commitLengthSMTLibQueryRun worker
    (ReservedLengthSMTLibQueryRun
      (PreparedLengthSMTLibQueryRun ordinal _ _ _ _ _)
      stdoutStart stderrStart)
    run =
  atomically $ do
    QueryLeaseState mode nextOrdinal used inFlight
        stateStdoutStart stateStderrStart <-
      readTVar $ readyWorkerQueryState worker
    if inFlight /= Just ordinal || nextOrdinal /= ordinal + 1 ||
        mode == QueryLeaseSpent ||
        stateStdoutStart /= stdoutStart || stateStderrStart /= stderrStart ||
        lengthSMTLibQueryRunOrdinal run /= ordinal ||
        lengthSMTLibQueryRunStdoutStart run /= stdoutStart ||
        lengthSMTLibQueryRunStderrStart run /= stderrStart
      then pure False
      else do
        writeTVar (readyWorkerQueryState worker) $ QueryLeaseState
          mode nextOrdinal used Nothing stdoutEnd stderrEnd
        pure True
 where
  stdoutEnd = lengthSMTLibQueryRunStdoutEnd run
  stderrEnd = lengthSMTLibQueryRunStderrEnd run

spendLengthSMTLibQueryWorker
  :: LengthSMTLibReadyWorker epoch
  -> LengthSMTLibQueryRunFailure
  -> IO (Either LengthSMTLibQueryRunError value)
spendLengthSMTLibQueryWorker worker failure = do
  cleanup <- abandonLengthSMTLibQueryWorker worker
  pure $ Left $ LengthSMTLibQueryRunError failure $ Just cleanup

abandonLengthSMTLibQueryWorker
  :: LengthSMTLibReadyWorker epoch
  -> IO LengthSMTLibProcessCleanupStatus
abandonLengthSMTLibQueryWorker worker = do
  atomically $ do
    QueryLeaseState _ ordinal used _ stdoutCount stderrCount <-
      readTVar $ readyWorkerQueryState worker
    writeTVar (readyWorkerQueryState worker) $ QueryLeaseState
      QueryLeaseSpent ordinal used Nothing stdoutCount stderrCount
  cancelLengthSMTLibProcess $ readyWorkerCancellation worker
  closeLengthSMTLibProcess $ readyWorkerProcess worker

queryProcessFailure
  :: LengthSMTLibProcessError
  -> LengthSMTLibQueryRunFailure
queryProcessFailure failure
  | lengthSMTLibProcessErrorClass failure ==
      LengthSMTLibProcessDeadlineExceeded =
        LengthSMTLibQueryDeadlineFailure failure
  | otherwise = LengthSMTLibQueryProcessFailure failure

executeReservedLengthSMTLibQueryRun
  :: LengthSMTLibReadyWorker epoch
  -> ReservedLengthSMTLibQueryRun epoch identity local
  -> IO
      (Either
        LengthSMTLibQueryRunFailure
        (LengthSMTLibQueryRun epoch identity local))
executeReservedLengthSMTLibQueryRun worker
    (ReservedLengthSMTLibQueryRun
      (PreparedLengthSMTLibQueryRun ordinal checkBarrier valueBarrier plan
        evaluationLimits deadline)
      stdoutStart stderrStart) = do
  driven <- driveProtocolQuery worker deadline plan
  case driven of
    Left failure -> pure $ Left failure
    Right (decoded, transcript) -> do
      firstBoundary <- observeLengthSMTLibQueryBoundary worker deadline
      case firstBoundary of
        Left failure -> pure $ Left failure
        Right (stdoutEnd, stderrEnd) ->
          case validateLengthSMTLibQueryAccounting
              transcript stdoutStart stdoutEnd stderrStart stderrEnd of
            Left failure -> pure $ Left failure
            Right () -> do
              replayed <- replayLengthSMTLibQuery
                evaluationLimits worker plan deadline decoded
              case replayed of
                Left failure -> pure $ Left failure
                Right outcome -> do
                  committedBoundary <-
                    observeLengthSMTLibQueryBoundary worker deadline
                  case committedBoundary of
                    Left failure -> pure $ Left failure
                    Right (stdoutCommitted, stderrCommitted)
                      | stdoutCommitted /= stdoutEnd -> pure $ Left
                          $ LengthSMTLibQueryTranscriptAccountingMismatch
                              (smtLibCausalTranscriptByteCount transcript)
                              (stdoutCommitted -| stdoutStart)
                      | stderrCommitted /= stderrEnd -> pure $ Left
                          $ LengthSMTLibQueryStderrAccountingMismatch
                              stderrStart stderrCommitted
                      | otherwise -> case buildLengthSMTLibQueryRunIdentity
                          worker plan evaluationLimits ordinal deadline
                          checkBarrier valueBarrier outcome transcript
                          stdoutStart stdoutCommitted stderrStart stderrCommitted of
                        Left failure -> pure $ Left failure
                        Right identity -> do
                          let transcriptBytes = causalTranscriptBytes transcript
                              transcriptDigest = SHA256.hash transcriptBytes
                          finalBoundary <-
                            observeLengthSMTLibQueryBoundary worker deadline
                          pure $ case finalBoundary of
                            Left failure -> Left failure
                            Right (stdoutFinal, stderrFinal)
                              | stdoutFinal /= stdoutCommitted -> Left
                                  $ LengthSMTLibQueryTranscriptAccountingMismatch
                                      (smtLibCausalTranscriptByteCount
                                        transcript)
                                      (stdoutFinal -| stdoutStart)
                              | stderrFinal /= stderrCommitted -> Left
                                  $ LengthSMTLibQueryStderrAccountingMismatch
                                      stderrStart stderrFinal
                              | otherwise -> Right $ LengthSMTLibQueryRun
                                  ordinal
                                  (replayedLengthSMTLibQueryObservation outcome)
                                  identity
                                  transcriptDigest
                                  stdoutStart stdoutFinal
                                  stderrStart stderrFinal

driveProtocolQuery
  :: LengthSMTLibReadyWorker epoch
  -> LengthSMTLibProcessDeadline
  -> LengthSMTLibProtocolPlan identity local
  -> IO
      (Either
        LengthSMTLibQueryRunFailure
        ( LengthSMTLibProtocolDecoded identity local
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
      LengthSMTLibQueryProtocolFailure protocolFailure
    SMTLibCausalCumulativeOutputByteLimitExceeded maximumBytes observed ->
      LengthSMTLibQueryProtocolFailure
        $ LengthSMTLibProtocolCumulativeStdoutByteLimitExceeded
            maximumBytes observed
    SMTLibCausalInternalFailure -> LengthSMTLibQueryInternalFailure

observeLengthSMTLibQueryBoundary
  :: LengthSMTLibReadyWorker epoch
  -> LengthSMTLibProcessDeadline
  -> IO (Either LengthSMTLibQueryRunFailure (Natural, Natural))
observeLengthSMTLibQueryBoundary worker deadline = do
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

validateLengthSMTLibQueryAccounting
  :: SMTLibCausalTranscript kind
  -> Natural
  -> Natural
  -> Natural
  -> Natural
  -> Either LengthSMTLibQueryRunFailure ()
validateLengthSMTLibQueryAccounting transcript stdoutStart stdoutEnd
    stderrStart stderrEnd
  | stdoutEnd < stdoutStart = Left LengthSMTLibQueryInternalFailure
  | transcriptCount /= stdoutEnd - stdoutStart = Left
      $ LengthSMTLibQueryTranscriptAccountingMismatch
          transcriptCount (stdoutEnd - stdoutStart)
  | stderrEnd /= stderrStart = Left
      $ LengthSMTLibQueryStderrAccountingMismatch stderrStart stderrEnd
  | otherwise = Right ()
 where
  transcriptCount = smtLibCausalTranscriptByteCount transcript

replayLengthSMTLibQuery
  :: LengthEvaluationLimits
  -> LengthSMTLibReadyWorker epoch
  -> LengthSMTLibProtocolPlan identity local
  -> LengthSMTLibProcessDeadline
  -> LengthSMTLibProtocolDecoded identity local
  -> IO
      (Either
        LengthSMTLibQueryRunFailure
        ReplayedLengthSMTLibQueryOutcome)
replayLengthSMTLibQuery evaluationLimits worker plan deadline decoded =
  case ( lengthSMTLibProtocolDecodedObservation decoded
       , lengthSMTLibProtocolPlanArtifactPolicy plan) of
    ( SatisfiableObservation (Just values)
      , LengthSMTLibInputValuesAfterSatisfiable) -> do
      replayed <- runBeforeLengthSMTLibProcessDeadline
        (readyWorkerCancellation worker) deadline
        (evaluate $ force
          $ validateLengthSMTLibCounterexample evaluationLimits query values)
        (const $ pure ())
      pure $ case replayed of
        Left failure -> Left $ queryProcessFailure failure
        Right (Left failure) -> Left $ LengthSMTLibQueryModelFailure failure
        Right (Right Nothing) -> Left LengthSMTLibQueryModelNotCounterexample
        Right (Right (Just evidence)) -> Right $ case values of
          [] -> ReplayedLengthSMTLibSatisfiableVacuous evidence
          _ : _ -> ReplayedLengthSMTLibSatisfiableFramed evidence
    ( SatisfiableObservation Nothing
      , LengthSMTLibInputValuesAfterSatisfiable) ->
      pure $ Left LengthSMTLibQueryInternalFailure
    (SatisfiableObservation Nothing, LengthSMTLibStatusOnly) ->
      pure $ Right ReplayedLengthSMTLibSatisfiableStatusOnly
    (SatisfiableObservation (Just _), LengthSMTLibStatusOnly) ->
      pure $ Left LengthSMTLibQueryInternalFailure
    (UnsatisfiableObservation (), _) ->
      pure $ Right ReplayedLengthSMTLibUnsatisfiable
    (UnknownObservation (), _) ->
      pure $ Right ReplayedLengthSMTLibUnknown
 where
  query = lengthSMTLibProtocolPlanQuery plan

-- | The five successful identity branches after independent replay. This one
-- transient owner prevents decoded status/value classification from being
-- paired independently with replay evidence while the unchanged v1 identity
-- fields are built.
data ReplayedLengthSMTLibQueryOutcome
  = ReplayedLengthSMTLibSatisfiableStatusOnly
  | ReplayedLengthSMTLibSatisfiableVacuous
      !(BehavioralEvidence
        FiniteListSpineLengthV1
        ValidatedLengthCounterexample)
  | ReplayedLengthSMTLibSatisfiableFramed
      !(BehavioralEvidence
        FiniteListSpineLengthV1
        ValidatedLengthCounterexample)
  | ReplayedLengthSMTLibUnsatisfiable
  | ReplayedLengthSMTLibUnknown

-- Generic 'SolverObservation' artifacts remain lazy. Constructing the final
-- strict run owner separately forces only the satisfiable 'Maybe' spine, as
-- the former strict evidence field did, without adding a global payload bang.
replayedLengthSMTLibQueryObservation
  :: ReplayedLengthSMTLibQueryOutcome
  -> LengthSMTLibQueryRunObservation
replayedLengthSMTLibQueryObservation outcome = case outcome of
  ReplayedLengthSMTLibSatisfiableStatusOnly ->
    let evidence = Nothing
    in evidence `seq` SatisfiableObservation evidence
  ReplayedLengthSMTLibSatisfiableVacuous retained ->
    let evidence = Just retained
    in evidence `seq` SatisfiableObservation evidence
  ReplayedLengthSMTLibSatisfiableFramed retained ->
    let evidence = Just retained
    in evidence `seq` SatisfiableObservation evidence
  ReplayedLengthSMTLibUnsatisfiable -> UnsatisfiableObservation ()
  ReplayedLengthSMTLibUnknown -> UnknownObservation ()

(-|) :: Natural -> Natural -> Natural
left -| right
  | left < right = 0
  | otherwise = left - right

buildLengthSMTLibQueryRunIdentity
  :: LengthSMTLibReadyWorker epoch
  -> LengthSMTLibProtocolPlan identity local
  -> LengthEvaluationLimits
  -> Natural
  -> LengthSMTLibProcessDeadline
  -> ByteString
  -> ByteString
  -> ReplayedLengthSMTLibQueryOutcome
  -> SMTLibCausalTranscript LengthSMTLibProtocolWriteKind
  -> Natural
  -> Natural
  -> Natural
  -> Natural
  -> Either
      LengthSMTLibQueryRunFailure
      (Fingerprint LengthSMTLibQueryRunIdentitySubject)
buildLengthSMTLibQueryRunIdentity worker plan evaluationLimits ordinal deadline
    checkBarrier valueBarrier outcome transcript
    stdoutStart stdoutEnd stderrStart stderrEnd =
  case buildFingerprintWithin maximumBytes FingerprintBuilder
      { fingerprintBuilderVersion = 1
      , fingerprintBuilderRole = queryRunFingerprintRoleForWorker worker
      , fingerprintBuilderFields =
          queryRunIdentityPrefixFields worker plan ordinal deadline
            checkBarrier valueBarrier ++
          [queryRunTranscriptField transcript] ++
          queryRunIdentitySuffixFields evaluationLimits outcome
            stdoutStart stdoutEnd stderrStart stderrEnd
            (smtLibCausalTranscriptByteCount transcript)
      } of
    Left (FingerprintLimitExceeded fingerprintMaximum observed) -> Left
      $ LengthSMTLibQueryRunIdentityFingerprintByteLimitExceeded
          fingerprintMaximum observed
    Right identity -> Right identity
 where
  maximumBytes = readyQueryRunIdentityFingerprintByteLimit
    $ readyWorkerQueryPolicy worker

admitLengthSMTLibQueryRunIdentity
  :: LengthSMTLibReadyWorker epoch
  -> LengthSMTLibProtocolPlan identity local
  -> LengthEvaluationLimits
  -> Natural
  -> LengthSMTLibProcessDeadline
  -> ByteString
  -> ByteString
  -> Either LengthSMTLibQueryRunFailure ()
admitLengthSMTLibQueryRunIdentity worker plan evaluationLimits
    ordinal deadline checkBarrier valueBarrier
  | requiredMaximum > maximumBytes = Left
      $ LengthSMTLibQueryRunIdentityAdmissionTooSmall
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
    $ queryRunIdentityPrefixFields worker plan ordinal deadline
        checkBarrier valueBarrier
  transcriptCount = queryRunTranscriptMaximumFieldByteCount
    transcriptMaximum epochMaximum
  suffixCounts = map fingerprintFieldByteCount
    $ queryRunIdentityMaximumSuffixFields evaluationLimits
        (lengthSMTLibProcessStdoutByteLimit processLimits)
        (lengthSMTLibProcessStderrByteLimit processLimits)
        transcriptMaximum
  requiredMaximum = fingerprintBuilderByteCount
    (queryRunFingerprintRoleForWorker worker)
    $ prefixCounts ++ transcriptCount : suffixCounts

queryRunFingerprintRole :: [Word8]
queryRunFingerprintRole = ascii
  "finite-list-spine-length/z3-live-query-run"

budgetedQueryRunFingerprintRole :: [Word8]
budgetedQueryRunFingerprintRole = ascii
  "finite-list-spine-length/z3-live-query-run/shared-usable-work-deadline"

queryRunFingerprintRoleForWorker
  :: LengthSMTLibReadyWorker epoch
  -> [Word8]
queryRunFingerprintRoleForWorker worker =
  case readyQueryUsableWorkDeadlinePolicy $ readyWorkerQueryPolicy worker of
    LengthSMTLibFreshPerQueryDeadline -> queryRunFingerprintRole
    LengthSMTLibSharedUsableWorkDeadline {} ->
      budgetedQueryRunFingerprintRole

queryRunSchemaTagForWorker
  :: LengthSMTLibReadyWorker epoch
  -> [Word8]
queryRunSchemaTagForWorker worker =
  case readyQueryUsableWorkDeadlinePolicy $ readyWorkerQueryPolicy worker of
    LengthSMTLibFreshPerQueryDeadline -> lengthSMTLibQueryRunSchemaTag
    LengthSMTLibSharedUsableWorkDeadline {} ->
      lengthSMTLibBudgetedQueryRunSchemaTag

queryRunIdentityPrefixFields
  :: LengthSMTLibReadyWorker epoch
  -> LengthSMTLibProtocolPlan identity local
  -> Natural
  -> LengthSMTLibProcessDeadline
  -> ByteString
  -> ByteString
  -> [FingerprintField]
queryRunIdentityPrefixFields worker plan ordinal deadline
    checkBarrier valueBarrier =
  [ FingerprintBytes $ queryRunSchemaTagForWorker worker
  , tagged "authority"
      [ FingerprintBytes $ ascii $ concat
          [ "live-syntactic-process-observation/"
          , "independent-counterexample-replay/"
          , "no-solver-soundness-or-executable-image-attestation/v1"
          ]
      ]
  , lengthSMTLibReadyWorkerIdentityFingerprintField worker
  , tagged "query-allocation"
      [ FingerprintBytes lengthSMTLibQueryBarrierSchemaTag
      , FingerprintBytes $ ascii
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
  :: LengthEvaluationLimits
  -> ReplayedLengthSMTLibQueryOutcome
  -> Natural
  -> Natural
  -> Natural
  -> Natural
  -> Natural
  -> [FingerprintField]
queryRunIdentitySuffixFields evaluationLimits outcome
    stdoutStart stdoutEnd stderrStart stderrEnd transcriptCount =
  [ queryDecodedOutcomeField outcome
  , queryReplayField evaluationLimits outcome
  , queryTransportCommitField stdoutStart stdoutEnd stderrStart stderrEnd
      transcriptCount
  ]

queryRunIdentityMaximumSuffixFields
  :: LengthEvaluationLimits
  -> Natural
  -> Natural
  -> Natural
  -> [FingerprintField]
queryRunIdentityMaximumSuffixFields evaluationLimits stdoutMaximum
    stderrMaximum transcriptMaximum =
  [ tagged "decoded-branch"
      [ longestField $ map (FingerprintBytes . ascii)
          ["satisfiable", "unsatisfiable", "unknown"]
      , longestField $ map (FingerprintBytes . ascii)
          ["absent", "vacuous-zero-input", "framed-input-values"]
      ]
  , tagged "independent-replay"
      [ FingerprintBytes $ ascii
          "finite-list-spine-length/counterexample-replay/v1"
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
  :: ReplayedLengthSMTLibQueryOutcome
  -> FingerprintField
queryDecodedOutcomeField outcome = tagged "decoded-branch"
  [ solverStatusField $ replayedLengthSMTLibQueryStatus outcome
  , FingerprintBytes $ ascii valuesTag
  ]
 where
  -- The v1 protocol never emits @get-value@ for a zero-input query, while the
  -- query-aware decoder requires exact arity for every emitted nonempty
  -- request.  The decoded binding spine therefore preserves the former raw
  -- frame distinction while sealing identity, before successful run
  -- construction releases that parsed representation.
  valuesTag = case outcome of
    ReplayedLengthSMTLibSatisfiableStatusOnly -> "absent"
    ReplayedLengthSMTLibSatisfiableVacuous{} -> "vacuous-zero-input"
    ReplayedLengthSMTLibSatisfiableFramed{} -> "framed-input-values"
    ReplayedLengthSMTLibUnsatisfiable -> "absent"
    ReplayedLengthSMTLibUnknown -> "absent"

queryReplayField
  :: LengthEvaluationLimits
  -> ReplayedLengthSMTLibQueryOutcome
  -> FingerprintField
queryReplayField evaluationLimits outcome = tagged
  "independent-replay"
  [ FingerprintBytes $ ascii
      "finite-list-spine-length/counterexample-replay/v1"
  , FingerprintNatural $ fromIntegral
      $ lengthAssignmentValueBitLimit evaluationLimits
  , FingerprintNatural $ fromIntegral
      $ lengthIntermediateValueBitLimit evaluationLimits
  , FingerprintBytes $ ascii replayTag
  ]
 where
  replayTag = case outcome of
    ReplayedLengthSMTLibSatisfiableStatusOnly -> "not-requested-policy"
    ReplayedLengthSMTLibSatisfiableVacuous{} -> "validated-counterexample"
    ReplayedLengthSMTLibSatisfiableFramed{} -> "validated-counterexample"
    ReplayedLengthSMTLibUnsatisfiable -> "not-applicable-status"
    ReplayedLengthSMTLibUnknown -> "not-applicable-status"

replayedLengthSMTLibQueryStatus
  :: ReplayedLengthSMTLibQueryOutcome
  -> SolverStatus
replayedLengthSMTLibQueryStatus outcome = case outcome of
  ReplayedLengthSMTLibSatisfiableStatusOnly -> SolverSatisfiable
  ReplayedLengthSMTLibSatisfiableVacuous{} -> SolverSatisfiable
  ReplayedLengthSMTLibSatisfiableFramed{} -> SolverSatisfiable
  ReplayedLengthSMTLibUnsatisfiable -> SolverUnsatisfiable
  ReplayedLengthSMTLibUnknown -> SolverUnknown

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

data PreparedLengthSpinePairSMTLibQueryRun epoch identity local =
  PreparedLengthSpinePairSMTLibQueryRun
    !Natural
    !ByteString
    !ByteString
    !(LengthSpinePairSMTLibProtocolPlan identity local)
    !LengthEvaluationLimits
    !LengthSMTLibProcessDeadline

type role PreparedLengthSpinePairSMTLibQueryRun nominal nominal nominal

-- The last-committed accounting anchors are read from the lease state in the
-- same STM transaction which burns the ordinal and both marker roles.  Only
-- this receipt can cross the execution boundary; a merely prepared plan has
-- no transport-commit anchor.
data ReservedLengthSpinePairSMTLibQueryRun epoch identity local =
  ReservedLengthSpinePairSMTLibQueryRun
    !(PreparedLengthSpinePairSMTLibQueryRun epoch identity local)
    !Natural
    !Natural

type role ReservedLengthSpinePairSMTLibQueryRun nominal nominal nominal

-- | Execute one nominal binary-product query in the shared serial ordinal space.
-- The returned value is a live syntactic observation with independently
-- replayed counterexample evidence when the configured artifact policy asks
-- for a satisfiable model.  It is neither executable-image attestation nor a
-- proof that an unsatisfiable or unknown solver status is sound.
runLengthSpinePairSMTLibReadyWorkerQuery
  :: forall epoch identity local.
     LengthEvaluationLimits
  -> LengthSMTLibReadyWorker epoch
  -> LengthSpinePairSMTLibQuery identity local
  -> IO
      (Either
        LengthSpinePairSMTLibQueryRunError
        (LengthSpinePairSMTLibQueryRun epoch identity local))
runLengthSpinePairSMTLibReadyWorkerQuery evaluationLimits worker query =
  mask $ \restore -> do
    deadlineResult <- effectiveLengthSMTLibDeadlineAfterMilliseconds
      (readyQueryUsableWorkDeadlinePolicy $ readyWorkerQueryPolicy worker)
      $ lengthSMTLibPostLaunchHostDeadlineMilliseconds postLaunchExecution
    case deadlineResult of
      Left failure -> pure $ spinePairQueryRunLeft
        $ LengthSpinePairSMTLibQueryDeadlineFailure failure
      Right (LengthSMTLibEffectiveDeadline _ deadline) -> do
        acquired <- acquireLengthSpinePairSMTLibQueryGate worker deadline
        case acquired of
          Left failure@LengthSpinePairSMTLibQueryProcessFailure {} ->
            spendLengthSpinePairSMTLibQueryWorker worker failure
          Left failure@LengthSpinePairSMTLibQueryInternalFailure ->
            spendLengthSpinePairSMTLibQueryWorker worker failure
          Left failure -> pure $ spinePairQueryRunLeft failure
          Right () -> finally
            (runWithGate restore deadline)
            (atomically $ putTMVar (readyWorkerQueryGate worker) ())
 where
  postLaunchExecution = readyQueryPostLaunchExecution
    $ readyWorkerQueryPolicy worker

  runWithGate restore deadline = do
    prepared <- prepareLengthSpinePairSMTLibQueryRun
      evaluationLimits worker query deadline
    case prepared of
      Left (mustSpend, failure)
        | mustSpend -> spendLengthSpinePairSMTLibQueryWorker worker failure
        | otherwise -> pure $ spinePairQueryRunLeft failure
      Right preparation -> do
        reserved <- reservePreparedLengthSpinePairSMTLibQueryRun worker preparation
        case reserved of
          Left LengthSpinePairSMTLibQueryBarrierCollision ->
            spendLengthSpinePairSMTLibQueryWorker worker
              LengthSpinePairSMTLibQueryBarrierCollision
          Left LengthSpinePairSMTLibQueryInternalFailure ->
            spendLengthSpinePairSMTLibQueryWorker worker
              LengthSpinePairSMTLibQueryInternalFailure
          Left failure -> pure $ spinePairQueryRunLeft failure
          Right reservation -> do
            executed <- restore
              (executeReservedLengthSpinePairSMTLibQueryRun worker reservation)
              `onException` abandonLengthSpinePairSMTLibQueryWorker worker
            case executed of
              Left failure -> spendLengthSpinePairSMTLibQueryWorker worker failure
              Right run -> do
                committed <- commitLengthSpinePairSMTLibQueryRun worker reservation run
                if committed
                  then pure $ Right run
                  else spendLengthSpinePairSMTLibQueryWorker worker
                    LengthSpinePairSMTLibQueryInternalFailure

spinePairQueryRunLeft
  :: LengthSpinePairSMTLibQueryRunFailure
  -> Either LengthSpinePairSMTLibQueryRunError value
spinePairQueryRunLeft failure = Left $ LengthSpinePairSMTLibQueryRunError failure Nothing

acquireLengthSpinePairSMTLibQueryGate
  :: LengthSMTLibReadyWorker epoch
  -> LengthSMTLibProcessDeadline
  -> IO (Either LengthSpinePairSMTLibQueryRunFailure ())
acquireLengthSpinePairSMTLibQueryGate worker deadline = mask $ \_ -> loop
 where
  maximumQueries = readyQueryMaximumQueries $ readyWorkerQueryPolicy worker
  process = readyWorkerProcess worker
  cancellation = readyWorkerCancellation worker
  stateVariable = readyWorkerQueryState worker
  gate = readyWorkerQueryGate worker

  loop = do
    initial <- atomically $ spinePairQueryLeaseAdmission maximumQueries
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
            case spinePairQueryLeaseAdmission maximumQueries state of
              Left failure -> pure $ Left failure
              Right () -> Right <$> readTMVar gate
        case observed of
          Left processFailure -> do
            state <- atomically $ readTVar stateVariable
            pure $ Left $ spinePairQueryGateProcessFailure state processFailure
          Right (Left failure) -> pure $ Left failure
          Right (Right ()) -> do
            claimed <- atomically $ do
              state <- readTVar stateVariable
              case spinePairQueryLeaseAdmission maximumQueries state of
                Left failure -> pure $ Left failure
                Right () -> do
                  token <- tryTakeTMVar gate
                  pure $ Right token
            case claimed of
              Left failure -> pure $ Left failure
              Right Nothing -> loop
              Right (Just ()) -> pure $ Right ()

spinePairQueryLeaseAdmission
  :: Natural
  -> QueryLeaseState
  -> Either LengthSpinePairSMTLibQueryRunFailure ()
spinePairQueryLeaseAdmission maximumQueries
    (QueryLeaseState mode nextOrdinal _ _ _ _) = case mode of
  QueryLeaseClosing -> Left LengthSpinePairSMTLibQueryWorkerClosing
  QueryLeaseSpent -> Left LengthSpinePairSMTLibQueryWorkerSpent
  QueryLeaseAccepting
    | nextOrdinal >= maximumQueries -> Left
        $ LengthSpinePairSMTLibQueryLimitExceeded maximumQueries (maximumQueries + 1)
    | otherwise -> Right ()

spinePairQueryGateProcessFailure
  :: QueryLeaseState
  -> LengthSMTLibProcessError
  -> LengthSpinePairSMTLibQueryRunFailure
spinePairQueryGateProcessFailure (QueryLeaseState mode _ _ _ _ _) failure = case mode of
  QueryLeaseClosing -> LengthSpinePairSMTLibQueryWorkerClosing
  QueryLeaseSpent -> LengthSpinePairSMTLibQueryWorkerSpent
  _ -> spinePairQueryProcessFailure failure

prepareLengthSpinePairSMTLibQueryRun
  :: LengthEvaluationLimits
  -> LengthSMTLibReadyWorker epoch
  -> LengthSpinePairSMTLibQuery identity local
  -> LengthSMTLibProcessDeadline
  -> IO
      (Either
        (Bool, LengthSpinePairSMTLibQueryRunFailure)
        (PreparedLengthSpinePairSMTLibQueryRun epoch identity local))
prepareLengthSpinePairSMTLibQueryRun evaluationLimits worker query deadline = do
  state@(QueryLeaseState _ ordinal used inFlight _ _) <-
    atomically $ readTVar $ readyWorkerQueryState worker
  case spinePairQueryLeaseAdmission maximumQueries state of
    Left failure -> pure $ Left (False, failure)
    Right () | isJust inFlight -> pure
      $ Left (True, LengthSpinePairSMTLibQueryInternalFailure)
    Right () -> do
      controlled <- waitLengthSMTLibProcessControl process
        (readyWorkerCancellation worker) deadline LengthSMTLibProcessQueryPhase
        $ pure ()
      case controlled of
        Left failure -> pure $ Left
          ( lengthSMTLibProcessErrorClass failure /=
              LengthSMTLibProcessDeadlineExceeded
          , spinePairQueryProcessFailure failure
          )
        Right () -> prepareControlled ordinal used
 where
  policy = readyWorkerQueryPolicy worker
  maximumQueries = readyQueryMaximumQueries policy
  protocolLimits = defaultLengthSpinePairSMTLibProtocolLimits
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
          isJust (lengthSpinePairSMTLibQueryInputValueRequestBytes query)
        valueNonce
          | needsValueBarrier = Just $ BS.unpack valueBarrier
          | otherwise = Nothing
    case sealLengthSpinePairSMTLibProtocolPlan protocolLimits postLaunchExecution query
        (BS.unpack checkBarrier) valueNonce of
      Left failure -> pure $ Left
        (False, LengthSpinePairSMTLibQueryProtocolPlanFailure failure)
      Right plan
        | Set.member checkBarrier used || Set.member valueBarrier used ||
            checkBarrier == valueBarrier -> pure
              $ Left (True, LengthSpinePairSMTLibQueryBarrierCollision)
        | otherwise -> do
            observedStdout <- lengthSMTLibProcessObservedStdoutBytes process
            let required = lengthSpinePairSMTLibProtocolPlanMinimumStdoutByteCount plan
                remaining
                  | observedStdout >= transportMaximum = 0
                  | otherwise = transportMaximum - observedStdout
            if required > remaining
              then pure $ Left (False,
                LengthSpinePairSMTLibQueryProcessStdoutCapacityTooSmall
                  remaining required)
              else case admitLengthSpinePairSMTLibQueryRunIdentity
                  worker plan
                  evaluationLimits ordinal deadline
                  checkBarrier valueBarrier of
                Left failure -> pure $ Left (False, failure)
                Right () -> pure $ Right $ PreparedLengthSpinePairSMTLibQueryRun
                  ordinal checkBarrier valueBarrier plan evaluationLimits deadline

reservePreparedLengthSpinePairSMTLibQueryRun
  :: LengthSMTLibReadyWorker epoch
  -> PreparedLengthSpinePairSMTLibQueryRun epoch identity local
  -> IO
      (Either
        LengthSpinePairSMTLibQueryRunFailure
        (ReservedLengthSpinePairSMTLibQueryRun epoch identity local))
reservePreparedLengthSpinePairSMTLibQueryRun worker
    preparation@(PreparedLengthSpinePairSMTLibQueryRun
      ordinal checkBarrier valueBarrier _ _ _) = atomically $ do
  state@(QueryLeaseState mode nextOrdinal used inFlight stdoutCount stderrCount) <-
    readTVar $ readyWorkerQueryState worker
  case spinePairQueryLeaseAdmission maximumQueries state of
    Left failure -> pure $ Left failure
    Right ()
      | mode /= QueryLeaseAccepting || nextOrdinal /= ordinal ||
          isJust inFlight -> pure $ Left LengthSpinePairSMTLibQueryInternalFailure
      | Set.member checkBarrier used || Set.member valueBarrier used ||
          checkBarrier == valueBarrier -> pure
            $ Left LengthSpinePairSMTLibQueryBarrierCollision
      | otherwise -> do
          writeTVar (readyWorkerQueryState worker) $ QueryLeaseState
            QueryLeaseAccepting (ordinal + 1)
            (Set.insert valueBarrier $ Set.insert checkBarrier used)
            (Just ordinal) stdoutCount stderrCount
          pure $ Right $ ReservedLengthSpinePairSMTLibQueryRun
            preparation stdoutCount stderrCount
 where
  maximumQueries = readyQueryMaximumQueries $ readyWorkerQueryPolicy worker

commitLengthSpinePairSMTLibQueryRun
  :: LengthSMTLibReadyWorker epoch
  -> ReservedLengthSpinePairSMTLibQueryRun epoch identity local
  -> LengthSpinePairSMTLibQueryRun epoch identity local
  -> IO Bool
commitLengthSpinePairSMTLibQueryRun worker
    (ReservedLengthSpinePairSMTLibQueryRun
      (PreparedLengthSpinePairSMTLibQueryRun ordinal _ _ _ _ _)
      stdoutStart stderrStart)
    run =
  atomically $ do
    QueryLeaseState mode nextOrdinal used inFlight
        stateStdoutStart stateStderrStart <-
      readTVar $ readyWorkerQueryState worker
    if inFlight /= Just ordinal || nextOrdinal /= ordinal + 1 ||
        mode == QueryLeaseSpent ||
        stateStdoutStart /= stdoutStart || stateStderrStart /= stderrStart ||
        lengthSpinePairSMTLibQueryRunOrdinal run /= ordinal ||
        lengthSpinePairSMTLibQueryRunStdoutStart run /= stdoutStart ||
        lengthSpinePairSMTLibQueryRunStderrStart run /= stderrStart
      then pure False
      else do
        writeTVar (readyWorkerQueryState worker) $ QueryLeaseState
          mode nextOrdinal used Nothing stdoutEnd stderrEnd
        pure True
 where
  stdoutEnd = lengthSpinePairSMTLibQueryRunStdoutEnd run
  stderrEnd = lengthSpinePairSMTLibQueryRunStderrEnd run

spendLengthSpinePairSMTLibQueryWorker
  :: LengthSMTLibReadyWorker epoch
  -> LengthSpinePairSMTLibQueryRunFailure
  -> IO (Either LengthSpinePairSMTLibQueryRunError value)
spendLengthSpinePairSMTLibQueryWorker worker failure = do
  cleanup <- abandonLengthSpinePairSMTLibQueryWorker worker
  pure $ Left $ LengthSpinePairSMTLibQueryRunError failure $ Just cleanup

abandonLengthSpinePairSMTLibQueryWorker
  :: LengthSMTLibReadyWorker epoch
  -> IO LengthSMTLibProcessCleanupStatus
abandonLengthSpinePairSMTLibQueryWorker worker = do
  atomically $ do
    QueryLeaseState _ ordinal used _ stdoutCount stderrCount <-
      readTVar $ readyWorkerQueryState worker
    writeTVar (readyWorkerQueryState worker) $ QueryLeaseState
      QueryLeaseSpent ordinal used Nothing stdoutCount stderrCount
  cancelLengthSMTLibProcess $ readyWorkerCancellation worker
  closeLengthSMTLibProcess $ readyWorkerProcess worker

spinePairQueryProcessFailure
  :: LengthSMTLibProcessError
  -> LengthSpinePairSMTLibQueryRunFailure
spinePairQueryProcessFailure failure
  | lengthSMTLibProcessErrorClass failure ==
      LengthSMTLibProcessDeadlineExceeded =
        LengthSpinePairSMTLibQueryDeadlineFailure failure
  | otherwise = LengthSpinePairSMTLibQueryProcessFailure failure

executeReservedLengthSpinePairSMTLibQueryRun
  :: LengthSMTLibReadyWorker epoch
  -> ReservedLengthSpinePairSMTLibQueryRun epoch identity local
  -> IO
      (Either
        LengthSpinePairSMTLibQueryRunFailure
        (LengthSpinePairSMTLibQueryRun epoch identity local))
executeReservedLengthSpinePairSMTLibQueryRun worker
    (ReservedLengthSpinePairSMTLibQueryRun
      (PreparedLengthSpinePairSMTLibQueryRun ordinal checkBarrier valueBarrier plan
        evaluationLimits deadline)
      stdoutStart stderrStart) = do
  driven <- driveSpinePairProtocolQuery worker deadline plan
  case driven of
    Left failure -> pure $ Left failure
    Right (decoded, transcript) -> do
      firstBoundary <- observeLengthSpinePairSMTLibQueryBoundary worker deadline
      case firstBoundary of
        Left failure -> pure $ Left failure
        Right (stdoutEnd, stderrEnd) ->
          case validateLengthSpinePairSMTLibQueryAccounting
              transcript stdoutStart stdoutEnd stderrStart stderrEnd of
            Left failure -> pure $ Left failure
            Right () -> do
              replayed <- replayLengthSpinePairSMTLibQuery
                evaluationLimits worker plan deadline decoded
              case replayed of
                Left failure -> pure $ Left failure
                Right outcome -> do
                  committedBoundary <-
                    observeLengthSpinePairSMTLibQueryBoundary worker deadline
                  case committedBoundary of
                    Left failure -> pure $ Left failure
                    Right (stdoutCommitted, stderrCommitted)
                      | stdoutCommitted /= stdoutEnd -> pure $ Left
                          $ LengthSpinePairSMTLibQueryTranscriptAccountingMismatch
                              (smtLibCausalTranscriptByteCount transcript)
                              (stdoutCommitted -| stdoutStart)
                      | stderrCommitted /= stderrEnd -> pure $ Left
                          $ LengthSpinePairSMTLibQueryStderrAccountingMismatch
                              stderrStart stderrCommitted
                      | otherwise -> case buildLengthSpinePairSMTLibQueryRunIdentity
                          worker plan evaluationLimits ordinal deadline
                          checkBarrier valueBarrier outcome transcript
                          stdoutStart stdoutCommitted stderrStart stderrCommitted of
                        Left failure -> pure $ Left failure
                        Right identity -> do
                          let transcriptBytes = causalTranscriptBytes transcript
                              transcriptDigest = SHA256.hash transcriptBytes
                          finalBoundary <-
                            observeLengthSpinePairSMTLibQueryBoundary worker deadline
                          pure $ case finalBoundary of
                            Left failure -> Left failure
                            Right (stdoutFinal, stderrFinal)
                              | stdoutFinal /= stdoutCommitted -> Left
                                  $ LengthSpinePairSMTLibQueryTranscriptAccountingMismatch
                                      (smtLibCausalTranscriptByteCount
                                        transcript)
                                      (stdoutFinal -| stdoutStart)
                              | stderrFinal /= stderrCommitted -> Left
                                  $ LengthSpinePairSMTLibQueryStderrAccountingMismatch
                                      stderrStart stderrFinal
                              | otherwise -> Right $ LengthSpinePairSMTLibQueryRun
                                  ordinal
                                  (replayedLengthSpinePairSMTLibQueryObservation outcome)
                                  identity
                                  transcriptDigest
                                  stdoutStart stdoutFinal
                                  stderrStart stderrFinal

driveSpinePairProtocolQuery
  :: LengthSMTLibReadyWorker epoch
  -> LengthSMTLibProcessDeadline
  -> LengthSpinePairSMTLibProtocolPlan identity local
  -> IO
      (Either
        LengthSpinePairSMTLibQueryRunFailure
        ( LengthSpinePairSMTLibProtocolDecoded identity local
        , SMTLibCausalTranscript LengthSpinePairSMTLibProtocolWriteKind
        ))
driveSpinePairProtocolQuery worker deadline plan = do
  driven <- driveSMTLibCausalActions
    SMTLibCausalAdoptPredecessorWhitespace
    (lengthSpinePairSMTLibProtocolPlanCumulativeStdoutByteLimit plan)
    feedLengthSpinePairSMTLibProtocol finishLengthSpinePairSMTLibProtocol
    LengthSMTLibProtocolUnexpectedPostBarrierByte
    lengthSMTLibCausalTransportOps
    (lengthSMTLibCausalTransport
      (readyWorkerProcess worker) (readyWorkerCancellation worker) deadline)
    $ startLengthSpinePairSMTLibProtocol plan
  pure $ case driven of
    Left failure -> Left $ mapFailure failure
    Right value -> Right value
 where
  mapFailure failure = case failure of
    SMTLibCausalTransportFailure processFailure ->
      spinePairQueryProcessFailure processFailure
    SMTLibCausalMachineFailure protocolFailure ->
      LengthSpinePairSMTLibQueryProtocolFailure protocolFailure
    SMTLibCausalCumulativeOutputByteLimitExceeded maximumBytes observed ->
      LengthSpinePairSMTLibQueryProtocolFailure
        $ LengthSMTLibProtocolCumulativeStdoutByteLimitExceeded
            maximumBytes observed
    SMTLibCausalInternalFailure -> LengthSpinePairSMTLibQueryInternalFailure

observeLengthSpinePairSMTLibQueryBoundary
  :: LengthSMTLibReadyWorker epoch
  -> LengthSMTLibProcessDeadline
  -> IO (Either LengthSpinePairSMTLibQueryRunFailure (Natural, Natural))
observeLengthSpinePairSMTLibQueryBoundary worker deadline = do
  ready <- checkLengthSMTLibProcessReady
    (readyWorkerProcess worker) (readyWorkerCancellation worker) deadline
  case ready of
    Left failure -> pure $ Left $ spinePairQueryProcessFailure failure
    Right () -> do
      stdoutCount <- lengthSMTLibProcessObservedStdoutBytes
        $ readyWorkerProcess worker
      stderrCount <- lengthSMTLibProcessObservedStderrBytes
        $ readyWorkerProcess worker
      pure $ Right (stdoutCount, stderrCount)

validateLengthSpinePairSMTLibQueryAccounting
  :: SMTLibCausalTranscript kind
  -> Natural
  -> Natural
  -> Natural
  -> Natural
  -> Either LengthSpinePairSMTLibQueryRunFailure ()
validateLengthSpinePairSMTLibQueryAccounting transcript stdoutStart stdoutEnd
    stderrStart stderrEnd
  | stdoutEnd < stdoutStart = Left LengthSpinePairSMTLibQueryInternalFailure
  | transcriptCount /= stdoutEnd - stdoutStart = Left
      $ LengthSpinePairSMTLibQueryTranscriptAccountingMismatch
          transcriptCount (stdoutEnd - stdoutStart)
  | stderrEnd /= stderrStart = Left
      $ LengthSpinePairSMTLibQueryStderrAccountingMismatch stderrStart stderrEnd
  | otherwise = Right ()
 where
  transcriptCount = smtLibCausalTranscriptByteCount transcript

replayLengthSpinePairSMTLibQuery
  :: LengthEvaluationLimits
  -> LengthSMTLibReadyWorker epoch
  -> LengthSpinePairSMTLibProtocolPlan identity local
  -> LengthSMTLibProcessDeadline
  -> LengthSpinePairSMTLibProtocolDecoded identity local
  -> IO
      (Either
        LengthSpinePairSMTLibQueryRunFailure
        ReplayedLengthSpinePairSMTLibQueryOutcome)
replayLengthSpinePairSMTLibQuery evaluationLimits worker plan deadline decoded =
  case ( lengthSpinePairSMTLibProtocolDecodedObservation decoded
       , lengthSpinePairSMTLibProtocolPlanArtifactPolicy plan) of
    ( SatisfiableObservation (Just values)
      , LengthSMTLibInputValuesAfterSatisfiable) -> do
      replayed <- runBeforeLengthSMTLibProcessDeadline
        (readyWorkerCancellation worker) deadline
        (evaluate $ force
          $ validateLengthSpinePairSMTLibCounterexample evaluationLimits query values)
        (const $ pure ())
      pure $ case replayed of
        Left failure -> Left $ spinePairQueryProcessFailure failure
        Right (Left failure) -> Left $ LengthSpinePairSMTLibQueryModelFailure failure
        Right (Right Nothing) -> Left LengthSpinePairSMTLibQueryModelNotCounterexample
        Right (Right (Just evidence)) -> Right $ case values of
          [] -> ReplayedLengthSpinePairSMTLibSatisfiableVacuous evidence
          _ : _ -> ReplayedLengthSpinePairSMTLibSatisfiableFramed evidence
    ( SatisfiableObservation Nothing
      , LengthSMTLibInputValuesAfterSatisfiable) ->
      pure $ Left LengthSpinePairSMTLibQueryInternalFailure
    (SatisfiableObservation Nothing, LengthSMTLibStatusOnly) ->
      pure $ Right ReplayedLengthSpinePairSMTLibSatisfiableStatusOnly
    (SatisfiableObservation (Just _), LengthSMTLibStatusOnly) ->
      pure $ Left LengthSpinePairSMTLibQueryInternalFailure
    (UnsatisfiableObservation (), _) ->
      pure $ Right ReplayedLengthSpinePairSMTLibUnsatisfiable
    (UnknownObservation (), _) ->
      pure $ Right ReplayedLengthSpinePairSMTLibUnknown
 where
  query = lengthSpinePairSMTLibProtocolPlanQuery plan

-- | The five successful identity branches after independent replay. This one
-- transient owner prevents decoded status/value classification from being
-- paired independently with replay evidence while the unchanged v1 identity
-- fields are built.
data ReplayedLengthSpinePairSMTLibQueryOutcome
  = ReplayedLengthSpinePairSMTLibSatisfiableStatusOnly
  | ReplayedLengthSpinePairSMTLibSatisfiableVacuous
      !(BehavioralEvidence
        FiniteBinaryProductSpineLengthsV1
        ValidatedLengthSpinePairCounterexample)
  | ReplayedLengthSpinePairSMTLibSatisfiableFramed
      !(BehavioralEvidence
        FiniteBinaryProductSpineLengthsV1
        ValidatedLengthSpinePairCounterexample)
  | ReplayedLengthSpinePairSMTLibUnsatisfiable
  | ReplayedLengthSpinePairSMTLibUnknown

-- Generic 'SolverObservation' artifacts remain lazy. Constructing the final
-- strict run owner separately forces only the satisfiable 'Maybe' spine, as
-- the former strict evidence field did, without adding a global payload bang.
replayedLengthSpinePairSMTLibQueryObservation
  :: ReplayedLengthSpinePairSMTLibQueryOutcome
  -> LengthSpinePairSMTLibQueryRunObservation
replayedLengthSpinePairSMTLibQueryObservation outcome = case outcome of
  ReplayedLengthSpinePairSMTLibSatisfiableStatusOnly ->
    let evidence = Nothing
    in evidence `seq` SatisfiableObservation evidence
  ReplayedLengthSpinePairSMTLibSatisfiableVacuous retained ->
    let evidence = Just retained
    in evidence `seq` SatisfiableObservation evidence
  ReplayedLengthSpinePairSMTLibSatisfiableFramed retained ->
    let evidence = Just retained
    in evidence `seq` SatisfiableObservation evidence
  ReplayedLengthSpinePairSMTLibUnsatisfiable -> UnsatisfiableObservation ()
  ReplayedLengthSpinePairSMTLibUnknown -> UnknownObservation ()

buildLengthSpinePairSMTLibQueryRunIdentity
  :: LengthSMTLibReadyWorker epoch
  -> LengthSpinePairSMTLibProtocolPlan identity local
  -> LengthEvaluationLimits
  -> Natural
  -> LengthSMTLibProcessDeadline
  -> ByteString
  -> ByteString
  -> ReplayedLengthSpinePairSMTLibQueryOutcome
  -> SMTLibCausalTranscript LengthSpinePairSMTLibProtocolWriteKind
  -> Natural
  -> Natural
  -> Natural
  -> Natural
  -> Either
      LengthSpinePairSMTLibQueryRunFailure
      (Fingerprint LengthSpinePairSMTLibQueryRunIdentitySubject)
buildLengthSpinePairSMTLibQueryRunIdentity worker plan evaluationLimits ordinal deadline
    checkBarrier valueBarrier outcome transcript
    stdoutStart stdoutEnd stderrStart stderrEnd =
  case buildFingerprintWithin maximumBytes FingerprintBuilder
      { fingerprintBuilderVersion = 1
      , fingerprintBuilderRole = spinePairQueryRunFingerprintRoleForWorker worker
      , fingerprintBuilderFields =
          spinePairQueryRunIdentityPrefixFields worker plan ordinal deadline
            checkBarrier valueBarrier ++
          [spinePairQueryRunTranscriptField transcript] ++
          spinePairQueryRunIdentitySuffixFields evaluationLimits outcome
            stdoutStart stdoutEnd stderrStart stderrEnd
            (smtLibCausalTranscriptByteCount transcript)
      } of
    Left (FingerprintLimitExceeded fingerprintMaximum observed) -> Left
      $ LengthSpinePairSMTLibQueryRunIdentityFingerprintByteLimitExceeded
          fingerprintMaximum observed
    Right identity -> Right identity
 where
  maximumBytes = readyQueryRunIdentityFingerprintByteLimit
    $ readyWorkerQueryPolicy worker

admitLengthSpinePairSMTLibQueryRunIdentity
  :: LengthSMTLibReadyWorker epoch
  -> LengthSpinePairSMTLibProtocolPlan identity local
  -> LengthEvaluationLimits
  -> Natural
  -> LengthSMTLibProcessDeadline
  -> ByteString
  -> ByteString
  -> Either LengthSpinePairSMTLibQueryRunFailure ()
admitLengthSpinePairSMTLibQueryRunIdentity worker plan evaluationLimits
    ordinal deadline checkBarrier valueBarrier
  | requiredMaximum > maximumBytes = Left
      $ LengthSpinePairSMTLibQueryRunIdentityAdmissionTooSmall
          maximumBytes requiredMaximum
  | otherwise = Right ()
 where
  maximumBytes = readyQueryRunIdentityFingerprintByteLimit
    $ readyWorkerQueryPolicy worker
  processLimits = lengthSMTLibProcessLimits $ readyWorkerProcess worker
  transcriptMaximum =
    lengthSpinePairSMTLibProtocolPlanCumulativeStdoutByteLimit plan
  epochMaximum = if isJust $ lengthSpinePairSMTLibProtocolInputValueWriteBytes plan
    then 2
    else 1
  prefixCounts = map fingerprintFieldByteCount
    $ spinePairQueryRunIdentityPrefixFields worker plan ordinal deadline
        checkBarrier valueBarrier
  transcriptCount = spinePairQueryRunTranscriptMaximumFieldByteCount
    transcriptMaximum epochMaximum
  suffixCounts = map fingerprintFieldByteCount
    $ spinePairQueryRunIdentityMaximumSuffixFields evaluationLimits
        (lengthSMTLibProcessStdoutByteLimit processLimits)
        (lengthSMTLibProcessStderrByteLimit processLimits)
        transcriptMaximum
  requiredMaximum = fingerprintBuilderByteCount
    (spinePairQueryRunFingerprintRoleForWorker worker)
    $ prefixCounts ++ transcriptCount : suffixCounts

spinePairQueryRunFingerprintRole :: [Word8]
spinePairQueryRunFingerprintRole = ascii
  "finite-binary-product-spine-lengths/z3-live-query-run"

budgetedSpinePairQueryRunFingerprintRole :: [Word8]
budgetedSpinePairQueryRunFingerprintRole = ascii $ concat
  [ "finite-binary-product-spine-lengths/z3-live-query-run/"
  , "shared-usable-work-deadline"
  ]

spinePairQueryRunFingerprintRoleForWorker
  :: LengthSMTLibReadyWorker epoch
  -> [Word8]
spinePairQueryRunFingerprintRoleForWorker worker =
  case readyQueryUsableWorkDeadlinePolicy $ readyWorkerQueryPolicy worker of
    LengthSMTLibFreshPerQueryDeadline -> spinePairQueryRunFingerprintRole
    LengthSMTLibSharedUsableWorkDeadline {} ->
      budgetedSpinePairQueryRunFingerprintRole

spinePairQueryRunSchemaTagForWorker
  :: LengthSMTLibReadyWorker epoch
  -> [Word8]
spinePairQueryRunSchemaTagForWorker worker =
  case readyQueryUsableWorkDeadlinePolicy $ readyWorkerQueryPolicy worker of
    LengthSMTLibFreshPerQueryDeadline -> lengthSpinePairSMTLibQueryRunSchemaTag
    LengthSMTLibSharedUsableWorkDeadline {} ->
      lengthSpinePairSMTLibBudgetedQueryRunSchemaTag

spinePairQueryRunIdentityPrefixFields
  :: LengthSMTLibReadyWorker epoch
  -> LengthSpinePairSMTLibProtocolPlan identity local
  -> Natural
  -> LengthSMTLibProcessDeadline
  -> ByteString
  -> ByteString
  -> [FingerprintField]
spinePairQueryRunIdentityPrefixFields worker plan ordinal deadline
    checkBarrier valueBarrier =
  [ FingerprintBytes $ spinePairQueryRunSchemaTagForWorker worker
  , tagged "authority"
      [ FingerprintBytes $ ascii $ concat
          [ "live-syntactic-process-observation/"
          , "independent-counterexample-replay/"
          , "no-solver-soundness-or-executable-image-attestation/v1"
          ]
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
  , lengthSMTLibReadyWorkerIdentityFingerprintField worker
  , tagged "query-allocation"
      [ FingerprintBytes lengthSMTLibQueryBarrierSchemaTag
      , FingerprintBytes $ ascii $ concat
          [ "one-shared-zero-based-ordinal-and-session-configured-query-"
          , "budget-across-"
          , "scalar-and-binary-product-runs/v1"
          ]
      , FingerprintNatural $ readyQueryMaximumQueries
          $ readyWorkerQueryPolicy worker
      , FingerprintBytes $ ascii
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
          $ lengthSpinePairSMTLibProtocolPlanFingerprint plan
      ]
  ] ++ queryDeadlineIdentityFields worker deadline
 where
  -- Session-limit admission proves every runnable ordinal fits the chosen
  -- wire representation. Keep the Natural lease ordinal authoritative and
  -- derive its fixed-width encoding only at this identity edge.
  ordinalWord = fromIntegral ordinal

spinePairQueryRunTranscriptField
  :: SMTLibCausalTranscript LengthSpinePairSMTLibProtocolWriteKind
  -> FingerprintField
spinePairQueryRunTranscriptField transcript = tagged "causal-transcript"
  [ FingerprintBytes lengthSMTLibCausalDriverSchemaTag
  , FingerprintBytes $ ascii
      "predecessor-whitespace-observed-and-owned-by-successor/v1"
  , tagged "segment-layout"
      [ tagged "inherited-predecessor-whitespace"
          [FingerprintNatural $ byteCountBytes inherited]
      , FingerprintSequence $ map spinePairQueryRunTranscriptEpochLayout epochs
      ]
  , tagged "exact-bytes"
      [FingerprintBytes $ BS.unpack $ causalTranscriptBytes transcript]
  ]
 where
  inherited = smtLibCausalTranscriptInheritedBytes transcript
  epochs = smtLibCausalTranscriptEpochs transcript

spinePairQueryRunTranscriptEpochLayout
  :: SMTLibCausalTranscriptEpoch LengthSpinePairSMTLibProtocolWriteKind
  -> FingerprintField
spinePairQueryRunTranscriptEpochLayout epoch = tagged "write-epoch"
  [ spinePairQueryProtocolWriteKindField
      $ smtLibCausalTranscriptEpochKind epoch
  , FingerprintNatural $ byteCountBytes
      $ smtLibCausalTranscriptEpochBytes epoch
  ]

spinePairQueryRunIdentitySuffixFields
  :: LengthEvaluationLimits
  -> ReplayedLengthSpinePairSMTLibQueryOutcome
  -> Natural
  -> Natural
  -> Natural
  -> Natural
  -> Natural
  -> [FingerprintField]
spinePairQueryRunIdentitySuffixFields evaluationLimits outcome
    stdoutStart stdoutEnd stderrStart stderrEnd transcriptCount =
  [ spinePairQueryDecodedOutcomeField outcome
  , spinePairQueryReplayField evaluationLimits outcome
  , spinePairQueryTransportCommitField stdoutStart stdoutEnd stderrStart stderrEnd
      transcriptCount
  ]

spinePairQueryRunIdentityMaximumSuffixFields
  :: LengthEvaluationLimits
  -> Natural
  -> Natural
  -> Natural
  -> [FingerprintField]
spinePairQueryRunIdentityMaximumSuffixFields evaluationLimits stdoutMaximum
    stderrMaximum transcriptMaximum =
  [ tagged "decoded-branch"
      [ longestField $ map (FingerprintBytes . ascii)
          ["satisfiable", "unsatisfiable", "unknown"]
      , longestField $ map (FingerprintBytes . ascii)
          ["absent", "vacuous-zero-input", "framed-input-values"]
      ]
  , tagged "independent-replay"
      [ FingerprintBytes $ ascii
          "finite-binary-product-spine-lengths/counterexample-replay/v1"
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
  , spinePairQueryTransportCommitField stdoutMaximum stdoutMaximum
      stderrMaximum stderrMaximum transcriptMaximum
  ]

spinePairQueryDecodedOutcomeField
  :: ReplayedLengthSpinePairSMTLibQueryOutcome
  -> FingerprintField
spinePairQueryDecodedOutcomeField outcome = tagged "decoded-branch"
  [ spinePairSolverStatusField $ replayedLengthSpinePairSMTLibQueryStatus outcome
  , FingerprintBytes $ ascii valuesTag
  ]
 where
  -- The v1 protocol never emits @get-value@ for a zero-input query, while the
  -- query-aware decoder requires exact arity for every emitted nonempty
  -- request.  The decoded binding spine therefore preserves the former raw
  -- frame distinction while sealing identity, before successful run
  -- construction releases that parsed representation.
  valuesTag = case outcome of
    ReplayedLengthSpinePairSMTLibSatisfiableStatusOnly -> "absent"
    ReplayedLengthSpinePairSMTLibSatisfiableVacuous{} -> "vacuous-zero-input"
    ReplayedLengthSpinePairSMTLibSatisfiableFramed{} -> "framed-input-values"
    ReplayedLengthSpinePairSMTLibUnsatisfiable -> "absent"
    ReplayedLengthSpinePairSMTLibUnknown -> "absent"

spinePairQueryReplayField
  :: LengthEvaluationLimits
  -> ReplayedLengthSpinePairSMTLibQueryOutcome
  -> FingerprintField
spinePairQueryReplayField evaluationLimits outcome = tagged
  "independent-replay"
  [ FingerprintBytes $ ascii
      "finite-binary-product-spine-lengths/counterexample-replay/v1"
  , FingerprintNatural $ fromIntegral
      $ lengthAssignmentValueBitLimit evaluationLimits
  , FingerprintNatural $ fromIntegral
      $ lengthIntermediateValueBitLimit evaluationLimits
  , FingerprintBytes $ ascii replayTag
  ]
 where
  replayTag = case outcome of
    ReplayedLengthSpinePairSMTLibSatisfiableStatusOnly -> "not-requested-policy"
    ReplayedLengthSpinePairSMTLibSatisfiableVacuous{} -> "validated-counterexample"
    ReplayedLengthSpinePairSMTLibSatisfiableFramed{} -> "validated-counterexample"
    ReplayedLengthSpinePairSMTLibUnsatisfiable -> "not-applicable-status"
    ReplayedLengthSpinePairSMTLibUnknown -> "not-applicable-status"

replayedLengthSpinePairSMTLibQueryStatus
  :: ReplayedLengthSpinePairSMTLibQueryOutcome
  -> SolverStatus
replayedLengthSpinePairSMTLibQueryStatus outcome = case outcome of
  ReplayedLengthSpinePairSMTLibSatisfiableStatusOnly -> SolverSatisfiable
  ReplayedLengthSpinePairSMTLibSatisfiableVacuous{} -> SolverSatisfiable
  ReplayedLengthSpinePairSMTLibSatisfiableFramed{} -> SolverSatisfiable
  ReplayedLengthSpinePairSMTLibUnsatisfiable -> SolverUnsatisfiable
  ReplayedLengthSpinePairSMTLibUnknown -> SolverUnknown

spinePairQueryTransportCommitField
  :: Natural
  -> Natural
  -> Natural
  -> Natural
  -> Natural
  -> FingerprintField
spinePairQueryTransportCommitField stdoutStart stdoutEnd stderrStart stderrEnd
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

spinePairSolverStatusField :: SolverStatus -> FingerprintField
spinePairSolverStatusField status = FingerprintBytes $ ascii $ case status of
  SolverSatisfiable -> "satisfiable"
  SolverUnsatisfiable -> "unsatisfiable"
  SolverUnknown -> "unknown"

spinePairQueryProtocolWriteKindField
  :: LengthSpinePairSMTLibProtocolWriteKind
  -> FingerprintField
spinePairQueryProtocolWriteKindField kind = FingerprintBytes $ ascii $ case kind of
  LengthSMTLibProtocolInitialQueryWrite -> "initial-query"
  LengthSMTLibProtocolInputValueWrite -> "input-value"

spinePairQueryRunTranscriptMaximumFieldByteCount
  :: Natural
  -> Natural
  -> Natural
spinePairQueryRunTranscriptMaximumFieldByteCount maximumBytes maximumEpochs =
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
                    [ fingerprintFieldByteCount $ spinePairQueryProtocolWriteKindField
                        LengthSMTLibProtocolInitialQueryWrite
                    , fingerprintFieldByteCount $ spinePairQueryProtocolWriteKindField
                        LengthSMTLibProtocolInputValueWrite
                    ]
                , naturalFieldByteCount maximumBytes
                ]
        ]
    , taggedFieldByteCount "exact-bytes"
        [bytesFieldByteCount maximumBytes]
    ]


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

withLengthSMTLibReadyWorkerUnderDeadline
  :: forall budget result. LengthSMTLibSessionUsableWorkDeadline budget
  -> LengthSMTLibSessionConfig
  -> (forall epoch. LengthSMTLibReadyWorker epoch -> IO result)
  -> IO (Either LengthSMTLibSessionScopeError result)
withLengthSMTLibReadyWorkerUnderDeadline
    (LengthSMTLibSessionUsableWorkDeadline milliseconds deadline) =
  withLengthSMTLibReadyWorkerWithDeadlinePolicy
    $ LengthSMTLibSharedUsableWorkDeadline milliseconds deadline

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
          rollback <- atomically $ readTVar rolledBackWorkspace
          let cleanup = emptyCleanup
                { lengthSMTLibSessionWorkspaceCleanupStatus = case rollback of
                    Nothing -> LengthSMTLibSessionWorkspaceNotAllocated
                    Just status -> status
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
                    (openLengthSMTLibProcess processLimits cancellation deadline
                      (lengthSMTLibExecutionZ3Profile execution)
                      $ workspacePath workspace)
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
                  rolledBack <- atomically $ readTVar rolledBackInspection
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
  case policy of
    LengthSMTLibFreshPerQueryDeadline -> Right legacy
    LengthSMTLibSharedUsableWorkDeadline milliseconds deadline ->
      buildBudgetedReadyWorkerIdentity
        sessionLimits milliseconds deadline legacy

buildBudgetedReadyWorkerIdentity
  :: LengthSMTLibSessionLimits
  -> Int
  -> LengthSMTLibProcessDeadline
  -> Fingerprint LengthSMTLibReadyWorkerIdentitySubject
  -> Either
      LengthSMTLibSessionError
      (Fingerprint LengthSMTLibReadyWorkerIdentitySubject)
buildBudgetedReadyWorkerIdentity
    (LengthSMTLibSessionLimits _ _ _ maximumBytes _)
    milliseconds deadline legacy =
  case buildFingerprintWithin maximumBytes FingerprintBuilder
      { fingerprintBuilderVersion = 1
      , fingerprintBuilderRole = ascii
          "finite-list-spine-length/z3-budgeted-capability-probed-ready-worker"
      , fingerprintBuilderFields =
          [ FingerprintBytes $ ascii
              "djex-length-z3-shared-usable-work-deadline/v1"
          , tagged "legacy-ready-worker-identity"
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
      , fingerprintBuilderRole = ascii
          "finite-list-spine-length/z3-capability-probed-ready-worker"
      , fingerprintBuilderFields =
          [ FingerprintBytes lengthSMTLibSessionSchemaTag
          , FingerprintBytes lengthSMTLibReadyWorkerSchemaTag
          , tagged "execution-policy"
              [FingerprintBytes $ fingerprintCanonicalBytes
                $ lengthSMTLibExecutionPolicyFingerprint execution]
          , lengthSMTLibProcessFingerprintField process
          , tagged "snapshot-strength"
              [FingerprintBytes $ BS.unpack
                lengthSMTLibExecutableSnapshotStrengthTag]
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
