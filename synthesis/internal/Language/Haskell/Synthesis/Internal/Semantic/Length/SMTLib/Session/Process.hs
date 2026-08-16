-- | Length compatibility and identity facade for the shared raw Z3 process.
--
-- The domain-neutral runtime owns subprocess allocation, bounded pipe IO,
-- cancellation, deadlines, and cleanup.  This facade preserves the existing
-- Length-specific sanitized vocabulary and is the sole owner of the raw
-- Length process schema, fingerprint root, and process-limit wrapper tag.
module Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session.Process
  ( lengthSMTLibExecutableSnapshotStrengthTag
  , lengthSMTLibDescriptorBoundExecutableLaunchStrengthTag
  , lengthSMTLibDescriptorBoundExecutableLaunchSupported
  , lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchStrengthTag
  , lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchSupported
  , lengthSMTLibDescriptorBoundExecveCheckExecutableAccessLaunchStrengthTag
  , lengthSMTLibDescriptorBoundExecveCheckExecutableAccessLaunchSupported
  , lengthSMTLibProcessSchemaTag
  , lengthSMTLibDescriptorBoundProcessSchemaTag
  , lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessProcessSchemaTag
  , lengthSMTLibDescriptorBoundExecveCheckExecutableAccessProcessSchemaTag
  , LengthSMTLibProcessLimitSource (..)
  , defaultLengthSMTLibProcessLimitSource
  , LengthSMTLibProcessLimits
  , mkLengthSMTLibProcessLimits
  , lengthSMTLibProcessExecutableByteLimit
  , lengthSMTLibProcessStdoutByteLimit
  , lengthSMTLibProcessStderrByteLimit
  , lengthSMTLibProcessReadChunkByteLimit
  , lengthSMTLibProcessGracefulCloseMilliseconds
  , lengthSMTLibProcessTerminateMilliseconds
  , lengthSMTLibProcessKillMilliseconds
  , lengthSMTLibProcessLimitsFingerprintField
  , LengthSMTLibProcessPhase (..)
  , LengthSMTLibProcessFailureClass (..)
  , LengthSMTLibProcessError (..)
  , LengthSMTLibProcessCleanupEscalation (..)
  , LengthSMTLibProcessCleanupStatus (..)
  , LengthSMTLibProcessDeadline
  , mkLengthSMTLibProcessDeadline
  , lengthSMTLibProcessDeadlineAfterMilliseconds
  , compareLengthSMTLibProcessDeadline
  , minimumLengthSMTLibProcessDeadline
  , checkLengthSMTLibProcessDeadline
  , lengthSMTLibProcessMonotonicTimeNanoseconds
  , lengthSMTLibProcessDeadlineFingerprintField
  , LengthSMTLibProcessCancellation
  , newLengthSMTLibProcessCancellation
  , cancelLengthSMTLibProcess
  , runBeforeLengthSMTLibProcessDeadline
  , waitLengthSMTLibProcessControl
  , LengthSMTLibExecutableSnapshot
  , lengthSMTLibExecutableSnapshotSHA256
  , lengthSMTLibExecutableSnapshotByteCount
  , lengthSMTLibExecutableSnapshotFingerprintField
  , LengthSMTLibProcess
  , openLengthSMTLibProcess
  , LengthSMTLibWorkingDirectoryDescriptor
  , mkLengthSMTLibWorkingDirectoryDescriptor
  , openLengthSMTLibDescriptorBoundProcess
  , openLengthSMTLibProcessWithPreDescriptorExecHook
  , lengthSMTLibProcessUsesDescriptorBoundExecutableLaunch
  , LengthSMTLibEffectiveIDExecutableAccessCheckResult (..)
  , openLengthSMTLibDescriptorBoundEffectiveIDExecutableAccessProcess
  , openLengthSMTLibDescriptorBoundEffectiveIDExecutableAccessProcessWithHooks
  , lengthSMTLibProcessUsesDescriptorBoundEffectiveIDExecutableAccessLaunch
  , LengthSMTLibExecveCheckResult (..)
  , LengthSMTLibExecveCheckStagedImageInspection (..)
  , inspectLengthSMTLibExecveCheckStagedImage
  , openLengthSMTLibDescriptorBoundExecveCheckExecutableAccessProcess
  , openLengthSMTLibDescriptorBoundExecveCheckExecutableAccessProcessWithHooks
  , openLengthSMTLibDescriptorBoundExecveCheckExecutableAccessProcessWithTestHooks
  , lengthSMTLibProcessUsesDescriptorBoundExecveCheckExecutableAccessLaunch
  , lengthSMTLibProcessLimits
  , lengthSMTLibProcessSnapshot
  , lengthSMTLibProcessFingerprintField
  , lengthSMTLibProcessObservedStdoutBytes
  , lengthSMTLibProcessObservedStderrBytes
  , writeLengthSMTLibProcess
  , nextLengthSMTLibProcessStdoutChunk
  , drainLengthSMTLibProcessBoundaryWhitespace
  , checkLengthSMTLibProcessReady
  , closeLengthSMTLibProcess
  ) where

import Control.Concurrent.STM (STM)
import Control.Exception (mask, mask_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Int (Int64)
import Data.Word (Word8, Word32, Word64)
import Foreign.C.Types (CInt)
import Numeric.Natural (Natural)
import System.Exit (ExitCode)

import Language.Haskell.Synthesis.Internal.Fingerprint
  ( FingerprintField (..) )
import Language.Haskell.Synthesis.Internal.SMTLib.Causal.BoundaryWhitespace
  ( SMTLibCausalBoundaryWhitespace )
import Language.Haskell.Synthesis.Internal.SMTLib.Causal.StdoutChunk
  ( SMTLibCausalStdoutChunk )
import qualified Language.Haskell.Synthesis.Internal.SMTLib.Z3.Execution
  as Z3Execution
import qualified Language.Haskell.Synthesis.Internal.SMTLib.Z3.Process
  as Z3Process

-- | Strength tag of the plain path-snapshot launch: the executable is hashed
-- through its pathname before spawn and that same pathname is then handed to
-- the process creator, so the tag names a stable-namespace assumption and is
-- explicitly weaker than an executed-image attestation.
lengthSMTLibExecutableSnapshotStrengthTag :: ByteString
lengthSMTLibExecutableSnapshotStrengthTag =
  Z3Process.z3SMTLibExecutableSnapshotStrengthTag

-- | Strength tag of the descriptor-bound launch: the digest and the executed
-- main image both come from one opened source stream copied once into one
-- sealed anonymous image.  It claims nothing about set-id or capability
-- metadata, the ELF loader, shared objects, interpreters, or solver results.
lengthSMTLibDescriptorBoundExecutableLaunchStrengthTag :: ByteString
lengthSMTLibDescriptorBoundExecutableLaunchStrengthTag =
  Z3Process.z3SMTLibDescriptorBoundExecutableLaunchStrengthTag

-- | Whether this build can perform the descriptor-bound launch at all.  When
-- 'False', 'openLengthSMTLibDescriptorBoundProcess' fails with
-- 'LengthSMTLibProcessDescriptorBoundLaunchUnavailable' without spawning.
lengthSMTLibDescriptorBoundExecutableLaunchSupported :: Bool
lengthSMTLibDescriptorBoundExecutableLaunchSupported =
  Z3Process.z3SMTLibDescriptorBoundExecutableLaunchSupported

-- | Strength tag of the effective-ID descriptor-bound launch: the opened
-- source additionally passes two point-in-time effective-credential VFS
-- execute-access observations, while the sealed image still binds only the
-- main-image bytes.  Neither observation is a reservation or a complete exec
-- security decision.
lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchStrengthTag
  :: ByteString
lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchStrengthTag =
  Z3Process.z3SMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchStrengthTag

-- | Whether this build can perform the effective-ID descriptor-bound launch.
-- It is enabled by the same build flag as the plain descriptor-bound launch.
lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchSupported :: Bool
lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchSupported =
  Z3Process.z3SMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchSupported

-- | Strength tag of the execve-check descriptor-bound launch: two source
-- checks pair effective-ID VFS execute access with the Linux 6.14
-- @AT_EXECVE_CHECK@ observation, and the separately checked @MFD_EXEC@ image
-- carries a fixed mode and verified seal set.  Every check is point-in-time.
lengthSMTLibDescriptorBoundExecveCheckExecutableAccessLaunchStrengthTag
  :: ByteString
lengthSMTLibDescriptorBoundExecveCheckExecutableAccessLaunchStrengthTag =
  Z3Process.z3SMTLibDescriptorBoundExecveCheckExecutableAccessLaunchStrengthTag

-- | Whether this build can perform the execve-check descriptor-bound launch.
-- It is enabled by the same build flag as the plain descriptor-bound launch;
-- kernel support is only discovered when the launch actually runs.
lengthSMTLibDescriptorBoundExecveCheckExecutableAccessLaunchSupported :: Bool
lengthSMTLibDescriptorBoundExecveCheckExecutableAccessLaunchSupported =
  Z3Process.z3SMTLibDescriptorBoundExecveCheckExecutableAccessLaunchSupported

-- | Schema tag sealed as the first field of
-- 'lengthSMTLibProcessFingerprintField' for a process opened by
-- 'openLengthSMTLibProcess'.  This module is the sole owner of the raw
-- Length process schema.
lengthSMTLibProcessSchemaTag :: ByteString
lengthSMTLibProcessSchemaTag = asciiBytes
  "djex-length-z3-raw-process/v2"

-- | Schema tag sealed as the first field of
-- 'lengthSMTLibProcessFingerprintField' for a process opened by
-- 'openLengthSMTLibDescriptorBoundProcess'.
lengthSMTLibDescriptorBoundProcessSchemaTag :: ByteString
lengthSMTLibDescriptorBoundProcessSchemaTag = asciiBytes
  "djex-length-z3-descriptor-bound-sealed-main-image-process/v1"

-- | Schema tag sealed as the first field of
-- 'lengthSMTLibProcessFingerprintField' for a process opened by
-- 'openLengthSMTLibDescriptorBoundEffectiveIDExecutableAccessProcess'.
lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessProcessSchemaTag
  :: ByteString
lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessProcessSchemaTag =
  asciiBytes $ concat
    [ "djex-length-z3-descriptor-bound-effective-id-executable-access-"
    , "sealed-main-image-process/v1"
    ]

-- | Schema tag sealed as the first field of
-- 'lengthSMTLibProcessFingerprintField' for a process opened by
-- 'openLengthSMTLibDescriptorBoundExecveCheckExecutableAccessProcess'.
lengthSMTLibDescriptorBoundExecveCheckExecutableAccessProcessSchemaTag
  :: ByteString
lengthSMTLibDescriptorBoundExecveCheckExecutableAccessProcessSchemaTag =
  asciiBytes $ concat
    [ "djex-length-z3-descriptor-bound-execve-check-executable-access-"
    , "sealed-main-image-process/v1"
    ]

-- | Caller-supplied raw process bounds before validation.  Byte fields bound
-- the hashed executable, retained stdout, retained stderr, and one pipe read;
-- the millisecond fields are the successive cleanup waits before escalating
-- from graceful close to terminate to kill.
data LengthSMTLibProcessLimitSource = LengthSMTLibProcessLimitSource
  { lengthSMTLibProcessLimitSourceExecutableBytes :: !Natural
  , lengthSMTLibProcessLimitSourceStdoutBytes :: !Natural
  , lengthSMTLibProcessLimitSourceStderrBytes :: !Natural
  , lengthSMTLibProcessLimitSourceReadChunkBytes :: !Natural
  , lengthSMTLibProcessLimitSourceGracefulCloseMilliseconds :: !Natural
  , lengthSMTLibProcessLimitSourceTerminateMilliseconds :: !Natural
  , lengthSMTLibProcessLimitSourceKillMilliseconds :: !Natural
  }
  deriving (Eq, Ord, Show)

-- | The shared raw Z3 process defaults restated in Length vocabulary:
-- 256 MiB executable, 1 MiB stdout, 64 KiB stderr, 4 KiB read chunks, and
-- 100/500/500 millisecond graceful-close, terminate, and kill waits.
defaultLengthSMTLibProcessLimitSource :: LengthSMTLibProcessLimitSource
defaultLengthSMTLibProcessLimitSource = fromZ3ProcessLimitSource
  Z3Process.defaultZ3SMTLibProcessLimitSource

-- | Validated raw process bounds.  A value can only be obtained through
-- 'mkLengthSMTLibProcessLimits' or read back from an opened process with
-- 'lengthSMTLibProcessLimits'.
data LengthSMTLibProcessLimits = LengthSMTLibProcessLimits
  !Z3Process.Z3SMTLibProcessLimits

-- | Validate a limit source.  The executable, read-chunk, and three
-- millisecond fields must be nonzero
-- ('LengthSMTLibProcessNonPositiveLimit'), and the read chunk and
-- milliseconds must fit the native integer range used by the process
-- runtime ('LengthSMTLibProcessLimitConversionOverflow'); the stdout and
-- stderr byte fields are accepted as given.  Failures report
-- 'LengthSMTLibProcessLimitPhase' and the offending value.
mkLengthSMTLibProcessLimits
  :: LengthSMTLibProcessLimitSource
  -> Either LengthSMTLibProcessError LengthSMTLibProcessLimits
mkLengthSMTLibProcessLimits source =
  case Z3Process.mkZ3SMTLibProcessLimits $ toZ3ProcessLimitSource source of
    Left failure -> Left $ fromZ3ProcessError failure
    Right limits -> Right $ LengthSMTLibProcessLimits limits

-- | Maximum number of executable bytes hashed for the pre-spawn snapshot;
-- a larger file fails opening with
-- 'LengthSMTLibProcessExecutableByteLimitExceeded'.
lengthSMTLibProcessExecutableByteLimit
  :: LengthSMTLibProcessLimits
  -> Natural
lengthSMTLibProcessExecutableByteLimit =
  Z3Process.z3SMTLibProcessExecutableByteLimit . toZ3ProcessLimits

-- | Maximum number of stdout bytes retained over the whole process
-- lifetime.  Bytes are charged before they are queued, and the first byte
-- beyond the limit records 'LengthSMTLibProcessStdoutByteLimitExceeded' as a
-- stdout terminal condition behind the permitted prefix.
lengthSMTLibProcessStdoutByteLimit
  :: LengthSMTLibProcessLimits
  -> Natural
lengthSMTLibProcessStdoutByteLimit =
  Z3Process.z3SMTLibProcessStdoutByteLimit . toZ3ProcessLimits

-- | Maximum stderr byte count reported by
-- 'lengthSMTLibProcessObservedStderrBytes'; the observed count saturates at
-- this limit plus one.  Any stderr byte at all poisons the process.
lengthSMTLibProcessStderrByteLimit
  :: LengthSMTLibProcessLimits
  -> Natural
lengthSMTLibProcessStderrByteLimit =
  Z3Process.z3SMTLibProcessStderrByteLimit . toZ3ProcessLimits

-- | Maximum number of bytes requested from a pipe or the executable file in
-- one read, and so the maximum size of one stdout chunk.
lengthSMTLibProcessReadChunkByteLimit
  :: LengthSMTLibProcessLimits
  -> Natural
lengthSMTLibProcessReadChunkByteLimit =
  Z3Process.z3SMTLibProcessReadChunkByteLimit . toZ3ProcessLimits

-- | How long cleanup waits for the child to exit after its stdin is closed
-- before sending a terminate signal.
lengthSMTLibProcessGracefulCloseMilliseconds
  :: LengthSMTLibProcessLimits
  -> Natural
lengthSMTLibProcessGracefulCloseMilliseconds =
  Z3Process.z3SMTLibProcessGracefulCloseMilliseconds . toZ3ProcessLimits

-- | How long cleanup waits after the terminate signal before escalating to a
-- kill.
lengthSMTLibProcessTerminateMilliseconds
  :: LengthSMTLibProcessLimits
  -> Natural
lengthSMTLibProcessTerminateMilliseconds =
  Z3Process.z3SMTLibProcessTerminateMilliseconds . toZ3ProcessLimits

-- | How long cleanup waits after the kill signal for the child to be reaped,
-- and also its bound for stopping reader threads and closing pipe handles.
lengthSMTLibProcessKillMilliseconds
  :: LengthSMTLibProcessLimits
  -> Natural
lengthSMTLibProcessKillMilliseconds =
  Z3Process.z3SMTLibProcessKillMilliseconds . toZ3ProcessLimits

-- | The seven limits sealed under the Length-owned
-- @length-z3-process-limits/v1@ tag, in the order executable, stdout,
-- stderr, read-chunk, graceful-close, terminate, kill.  This is the last
-- field of 'lengthSMTLibProcessFingerprintField'.
lengthSMTLibProcessLimitsFingerprintField
  :: LengthSMTLibProcessLimits
  -> FingerprintField
lengthSMTLibProcessLimitsFingerprintField limits = FingerprintTag
  (ascii "length-z3-process-limits/v1")
  $ Z3Process.z3SMTLibProcessLimitFingerprintFields
  $ toZ3ProcessLimits limits

-- | Where in the raw process lifecycle a failure was detected.  Phases are
-- listed in lifecycle order, from limit validation through close.
data LengthSMTLibProcessPhase
  = LengthSMTLibProcessLimitPhase
  | LengthSMTLibProcessDeadlinePhase
  | LengthSMTLibProcessWorkingDirectoryPhase
  | LengthSMTLibProcessSnapshotPhase
  | LengthSMTLibProcessSpawnPhase
  | LengthSMTLibProcessConfigurePhase
  | LengthSMTLibProcessWritePhase
  | LengthSMTLibProcessStdoutPhase
  | LengthSMTLibProcessStderrPhase
  | LengthSMTLibProcessReadyPhase
  | LengthSMTLibProcessQueryPhase
  | LengthSMTLibProcessClosePhase
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Stable sanitized failure classes of the raw process.  No constructor
-- retains a path, digest, command, output byte, exception string, or
-- operating-system error text.
data LengthSMTLibProcessFailureClass
  = LengthSMTLibProcessNonPositiveLimit
  | LengthSMTLibProcessLimitConversionOverflow
  | LengthSMTLibProcessCancelled
  | LengthSMTLibProcessDeadlineExceeded
  | LengthSMTLibProcessWorkingDirectoryNotAbsolute
  | LengthSMTLibProcessWorkingDirectoryUnavailable
  | LengthSMTLibProcessWorkingDirectoryNotEmpty
  | LengthSMTLibProcessExecutableUnavailable
  | LengthSMTLibProcessExecutableNotRegular
  | LengthSMTLibProcessExecutableNotExecutable
  | LengthSMTLibProcessExecutableByteLimitExceeded
  | LengthSMTLibProcessExecutableMetadataChanged
  | LengthSMTLibProcessExecutableDigestMismatch
  | LengthSMTLibProcessDescriptorBoundLaunchUnavailable
  | LengthSMTLibProcessDescriptorBoundStagingFailed
  | LengthSMTLibProcessDescriptorBoundExecFailed
  | LengthSMTLibProcessSpawnFailed
  | LengthSMTLibProcessMissingPipe
  | LengthSMTLibProcessHandleConfigurationFailed
  | LengthSMTLibProcessWriteFailed
  | LengthSMTLibProcessStdoutByteLimitExceeded
  | LengthSMTLibProcessStdoutEOF
  | LengthSMTLibProcessStdoutReadFailed
  | LengthSMTLibProcessUnexpectedPendingStdout
  | LengthSMTLibProcessStderrObserved
  | LengthSMTLibProcessStderrEOF
  | LengthSMTLibProcessStderrReadFailed
  | LengthSMTLibProcessExited
  | LengthSMTLibProcessClosed
  | LengthSMTLibProcessInternalFailure
  | LengthSMTLibProcessEffectiveIDExecutableAccessDenied
  | LengthSMTLibProcessEffectiveIDExecutableAccessCheckUnavailable
  | LengthSMTLibProcessEffectiveIDExecutableAccessCheckFailed
  | LengthSMTLibProcessSourceExecveCheckDenied
  | LengthSMTLibProcessSourceExecveCheckUnavailable
  | LengthSMTLibProcessSourceExecveCheckFailed
  | LengthSMTLibProcessStagedExecveCheckDenied
  | LengthSMTLibProcessStagedExecveCheckUnavailable
  | LengthSMTLibProcessStagedExecveCheckFailed
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | How far cleanup had to escalate to get the child reaped: it exited
-- within the graceful-close wait, needed a terminate signal, needed a kill
-- signal, or was still not reaped (or its readers still not stopped) when
-- the kill wait ran out.
data LengthSMTLibProcessCleanupEscalation
  = LengthSMTLibProcessClosedGracefully
  | LengthSMTLibProcessTerminated
  | LengthSMTLibProcessKilled
  | LengthSMTLibProcessCleanupIncomplete
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Outcome of one cleanup of the raw child.  The escalation is
-- 'LengthSMTLibProcessCleanupIncomplete' whenever no exit code was reaped or
-- the reader threads did not stop in time, regardless of which signals were
-- sent.
data LengthSMTLibProcessCleanupStatus = LengthSMTLibProcessCleanupStatus
  { lengthSMTLibProcessCleanupEscalation
      :: !LengthSMTLibProcessCleanupEscalation
    -- ^ The final escalation level reached, as described on the type.
  , lengthSMTLibProcessCleanupExitCode :: !(Maybe ExitCode)
    -- ^ The reaped exit code, or 'Nothing' if the child was not observed
    -- exiting within the cleanup waits.
  , lengthSMTLibProcessCleanupFailureCount :: !Natural
    -- ^ Number of failed signal deliveries and handle closes, plus one if
    -- the readers did not stop.
  , lengthSMTLibProcessCleanupReadersStopped :: !Bool
    -- ^ Whether every reader and writer thread of the process was joined
    -- within the kill wait; the stdout and stderr handles are only closed
    -- when this holds.
  }
  deriving (Eq, Ord, Show)

-- | Sanitized failure of one raw process operation.  The observed count is
-- an operation-specific lower bound (for example the offending limit value,
-- or the stdout byte count at which a terminal condition was recorded); the
-- cleanup status is present only when a failing open had already acquired a
-- child which it then cleaned up.
data LengthSMTLibProcessError = LengthSMTLibProcessError
  { lengthSMTLibProcessErrorPhase :: !LengthSMTLibProcessPhase
  , lengthSMTLibProcessErrorClass :: !LengthSMTLibProcessFailureClass
  , lengthSMTLibProcessErrorObservedAtLeast :: !(Maybe Natural)
  , lengthSMTLibProcessErrorCleanupStatus
      :: !(Maybe LengthSMTLibProcessCleanupStatus)
  }
  deriving (Eq, Ord, Show)

-- | An absolute deadline on the process-runtime monotonic clock, in
-- nanoseconds.  Every blocking raw process operation is bounded by one, and
-- a deadline is exceeded once the clock reads at or past it.
data LengthSMTLibProcessDeadline = LengthSMTLibProcessDeadline
  !Z3Process.Z3SMTLibProcessDeadline

-- | Wrap an absolute monotonic-clock reading in nanoseconds, as returned by
-- 'lengthSMTLibProcessMonotonicTimeNanoseconds', as a deadline.
mkLengthSMTLibProcessDeadline :: Word64 -> LengthSMTLibProcessDeadline
mkLengthSMTLibProcessDeadline = LengthSMTLibProcessDeadline
  . Z3Process.mkZ3SMTLibProcessDeadline

-- | Read the monotonic clock now and place the deadline that many
-- milliseconds ahead.  A non-positive count fails with
-- 'LengthSMTLibProcessNonPositiveLimit' and a target beyond the clock range
-- with 'LengthSMTLibProcessLimitConversionOverflow', both in
-- 'LengthSMTLibProcessDeadlinePhase'.
lengthSMTLibProcessDeadlineAfterMilliseconds
  :: Int
  -> IO (Either LengthSMTLibProcessError LengthSMTLibProcessDeadline)
lengthSMTLibProcessDeadlineAfterMilliseconds milliseconds = do
  result <- Z3Process.z3SMTLibProcessDeadlineAfterMilliseconds milliseconds
  pure $ case result of
    Left failure -> Left $ fromZ3ProcessError failure
    Right deadline -> Right $ LengthSMTLibProcessDeadline deadline

-- | Compare two deadlines by their absolute clock instant; the earlier
-- deadline is the smaller one.
compareLengthSMTLibProcessDeadline
  :: LengthSMTLibProcessDeadline
  -> LengthSMTLibProcessDeadline
  -> Ordering
compareLengthSMTLibProcessDeadline
    (LengthSMTLibProcessDeadline left)
    (LengthSMTLibProcessDeadline right) =
      Z3Process.compareZ3SMTLibProcessDeadline left right

-- | The earlier of two deadlines; the left one is returned on a tie.
minimumLengthSMTLibProcessDeadline
  :: LengthSMTLibProcessDeadline
  -> LengthSMTLibProcessDeadline
  -> LengthSMTLibProcessDeadline
minimumLengthSMTLibProcessDeadline
    (LengthSMTLibProcessDeadline left)
    (LengthSMTLibProcessDeadline right) = LengthSMTLibProcessDeadline
      $ Z3Process.minimumZ3SMTLibProcessDeadline left right

-- | Read the monotonic clock and fail with
-- 'LengthSMTLibProcessDeadlineExceeded' in 'LengthSMTLibProcessDeadlinePhase'
-- if the deadline has been reached or passed.  This checks the deadline
-- alone, not any cancellation token.
checkLengthSMTLibProcessDeadline
  :: LengthSMTLibProcessDeadline
  -> IO (Either LengthSMTLibProcessError ())
checkLengthSMTLibProcessDeadline
    (LengthSMTLibProcessDeadline deadline) = do
  checked <- Z3Process.checkZ3SMTLibProcessDeadline deadline
  pure $ case checked of
    Left failure -> Left $ fromZ3ProcessError failure
    Right () -> Right ()

-- | The monotonic clock reading, in nanoseconds, against which every
-- 'LengthSMTLibProcessDeadline' is measured.
lengthSMTLibProcessMonotonicTimeNanoseconds :: IO Word64
lengthSMTLibProcessMonotonicTimeNanoseconds =
  Z3Process.z3SMTLibProcessMonotonicTimeNanoseconds

-- | The deadline's absolute nanosecond instant tagged with the clock schema
-- it was read from, for sealing into a Length identity.
lengthSMTLibProcessDeadlineFingerprintField
  :: LengthSMTLibProcessDeadline
  -> FingerprintField
lengthSMTLibProcessDeadlineFingerprintField =
  Z3Process.z3SMTLibProcessDeadlineFingerprintField . toZ3ProcessDeadline

-- | A shared, one-way cancellation token.  Once cancelled it stays
-- cancelled, and every raw process operation given it fails with
-- 'LengthSMTLibProcessCancelled' at its next control check.
data LengthSMTLibProcessCancellation = LengthSMTLibProcessCancellation
  !Z3Process.Z3SMTLibProcessCancellation

-- | Allocate a fresh, not yet cancelled token.
newLengthSMTLibProcessCancellation :: IO LengthSMTLibProcessCancellation
newLengthSMTLibProcessCancellation = LengthSMTLibProcessCancellation
  <$> Z3Process.newZ3SMTLibProcessCancellation

-- | Set the token to cancelled.  This only flags the token; it does not
-- itself close or signal any process, and cancelling twice is harmless.
cancelLengthSMTLibProcess :: LengthSMTLibProcessCancellation -> IO ()
cancelLengthSMTLibProcess = Z3Process.cancelZ3SMTLibProcess
  . toZ3ProcessCancellation

-- | Run pre-process allocation work under the given cancellation token and
-- absolute deadline.  The action runs unmasked in a private thread;
-- cancellation or deadline expiry interrupts and joins it before returning.
-- If the action produced a value but a final cancellation/deadline check
-- then fails, the rollback is run on that value before it is discarded.
-- Exception payloads from either callback are never retained.
runBeforeLengthSMTLibProcessDeadline
  :: LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> IO value
  -> (value -> IO ())
  -> IO (Either LengthSMTLibProcessError value)
runBeforeLengthSMTLibProcessDeadline cancellation deadline action rollback =
  mask $ \restore -> do
    result <- restore $ Z3Process.runBeforeZ3SMTLibProcessDeadline
      (toZ3ProcessCancellation cancellation)
      (toZ3ProcessDeadline deadline)
      action rollback
    retainZ3ProcessResult result

-- | The bounded executable observation taken by an opener before the child
-- was created: the SHA-256 digest and byte count of the bytes it hashed,
-- plus the fingerprint field describing how they were obtained.  For the
-- plain launch this is a pre-spawn pathname snapshot and not an attestation
-- of the executed image; the descriptor-bound launches hash the same opened
-- source that is copied into the executed sealed image.
data LengthSMTLibExecutableSnapshot = LengthSMTLibExecutableSnapshot
  !Z3Process.Z3SMTLibExecutableSnapshot

-- | The raw 32-byte SHA-256 digest of the hashed executable bytes.  When
-- the execution profile pins an expected digest, opening only succeeds if
-- this value matched it.
lengthSMTLibExecutableSnapshotSHA256
  :: LengthSMTLibExecutableSnapshot
  -> ByteString
lengthSMTLibExecutableSnapshotSHA256 =
  Z3Process.z3SMTLibExecutableSnapshotSHA256 . toZ3ExecutableSnapshot

-- | The number of executable bytes hashed into the digest; it never exceeds
-- 'lengthSMTLibProcessExecutableByteLimit'.
lengthSMTLibExecutableSnapshotByteCount
  :: LengthSMTLibExecutableSnapshot
  -> Natural
lengthSMTLibExecutableSnapshotByteCount =
  Z3Process.z3SMTLibExecutableSnapshotByteCount . toZ3ExecutableSnapshot

-- | The snapshot field retained at open time, in a schema specific to the
-- launch strategy: the strength tag, requested executable path, source
-- metadata, digest, byte count, pin outcome, argument vector, working
-- directory, and spawn policy of the launch.  It is the observation the
-- opener sealed and is not recomputed on read.
lengthSMTLibExecutableSnapshotFingerprintField
  :: LengthSMTLibExecutableSnapshot
  -> FingerprintField
lengthSMTLibExecutableSnapshotFingerprintField =
  Z3Process.z3SMTLibExecutableSnapshotFingerprintField
  . toZ3ExecutableSnapshot

-- | Length ownership facade for one exact shared raw Z3 process. The generic
-- process is the sole retained runtime and observation authority; the
-- Length-specific v2 fingerprint field is derived from that associated
-- observation and its process-owned limits.
data LengthSMTLibProcess = LengthSMTLibProcess
  !Z3Process.Z3SMTLibProcess

-- | A borrowed open descriptor for the already-owned fresh working
-- directory of a descriptor-bound launch.  The native launcher duplicates
-- it before fork and never closes it; the caller keeps ownership.
data LengthSMTLibWorkingDirectoryDescriptor =
  LengthSMTLibWorkingDirectoryDescriptor
    !Z3Process.Z3SMTLibWorkingDirectoryDescriptor

-- | Wrap a raw numeric file descriptor of the working directory.  No check
-- is made here; the descriptor-bound opener it is passed to verifies that
-- it is a directory and refers to the same inode as the working directory
-- path.
mkLengthSMTLibWorkingDirectoryDescriptor
  :: Int
  -> LengthSMTLibWorkingDirectoryDescriptor
mkLengthSMTLibWorkingDirectoryDescriptor =
  LengthSMTLibWorkingDirectoryDescriptor
  . Z3Process.mkZ3SMTLibWorkingDirectoryDescriptor

-- | Open one raw Z3 process by the portable path-snapshot launch: the
-- working directory must be an absolute, existing, empty directory, the
-- configured executable is hashed through its pathname (honouring the
-- profile's pin, if any) with its metadata checked before and after, and
-- that pathname is then spawned with piped stdin/stdout/stderr in its own
-- process group.  A failure after the child was created carries the cleanup
-- status of that child.
openLengthSMTLibProcess
  :: LengthSMTLibProcessLimits
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> Z3Execution.Z3SMTLibExecutionProfile
  -> FilePath
  -> IO (Either LengthSMTLibProcessError LengthSMTLibProcess)
openLengthSMTLibProcess limits cancellation deadline profile workingDirectory =
  openRetained limits cancellation deadline $ \z3Limits z3Cancellation
      z3Deadline ->
    Z3Process.openZ3SMTLibProcess z3Limits z3Cancellation z3Deadline
      profile workingDirectory

-- | Run one raw opener under the converted limits, cancellation, and
-- deadline, and retain its outcome in Length vocabulary at the masked
-- handoff.  Every Length opener below is this wrapper around one raw opener.
openRetained
  :: LengthSMTLibProcessLimits
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> (Z3Process.Z3SMTLibProcessLimits
      -> Z3Process.Z3SMTLibProcessCancellation
      -> Z3Process.Z3SMTLibProcessDeadline
      -> IO (Either Z3Process.Z3SMTLibProcessError Z3Process.Z3SMTLibProcess))
  -> IO (Either LengthSMTLibProcessError LengthSMTLibProcess)
openRetained limits cancellation deadline open =
  mask $ \restore -> do
    opened <- restore $ open
      (toZ3ProcessLimits limits)
      (toZ3ProcessCancellation cancellation)
      (toZ3ProcessDeadline deadline)
    retainOpenedProcess opened

-- | Retain one raw opener outcome in Length vocabulary.  On success this
-- preserves the former strict cached-root demand at the successful masked
-- handoff: only the outer FingerprintTag is demanded; its ordered observation
-- field list deliberately stays lazy.
retainOpenedProcess
  :: Either Z3Process.Z3SMTLibProcessError Z3Process.Z3SMTLibProcess
  -> IO (Either LengthSMTLibProcessError LengthSMTLibProcess)
retainOpenedProcess opened = case opened of
  Left failure ->
    let retained = fromZ3ProcessError failure
    in retained `seq` pure (Left retained)
  Right process ->
    let retained = LengthSMTLibProcess process
        transientFingerprint = processFingerprintField process
    in retained `seq` transientFingerprint `seq` pure (Right retained)

-- | Open one raw Z3 process by the Linux-only descriptor-bound launch: the
-- configured pathname is opened once without following a final symlink,
-- copied once while hashing into a sealed anonymous memory file, and that
-- sealed image rather than the pathname is executed.  On a build without
-- descriptor-bound support (see
-- 'lengthSMTLibDescriptorBoundExecutableLaunchSupported') this fails with
-- 'LengthSMTLibProcessDescriptorBoundLaunchUnavailable' before any spawn.
openLengthSMTLibDescriptorBoundProcess
  :: LengthSMTLibProcessLimits
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> Z3Execution.Z3SMTLibExecutionProfile
  -> FilePath
  -> LengthSMTLibWorkingDirectoryDescriptor
  -> IO (Either LengthSMTLibProcessError LengthSMTLibProcess)
openLengthSMTLibDescriptorBoundProcess limits cancellation deadline profile
    workingDirectory descriptor =
  openLengthSMTLibProcessWithPreDescriptorExecHook limits cancellation deadline
    profile workingDirectory descriptor $ pure ()

-- | Deterministic package-private seam used to replace the configured
-- pathname after the sealed image has been admitted and before child
-- allocation.  No executable descriptor is exposed to the hook.
openLengthSMTLibProcessWithPreDescriptorExecHook
  :: LengthSMTLibProcessLimits
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> Z3Execution.Z3SMTLibExecutionProfile
  -> FilePath
  -> LengthSMTLibWorkingDirectoryDescriptor
  -> IO ()
  -> IO (Either LengthSMTLibProcessError LengthSMTLibProcess)
openLengthSMTLibProcessWithPreDescriptorExecHook limits cancellation deadline
    profile workingDirectory
    (LengthSMTLibWorkingDirectoryDescriptor descriptor) hook =
  openRetained limits cancellation deadline $ \z3Limits z3Cancellation
      z3Deadline ->
    Z3Process.openZ3SMTLibDescriptorBoundProcessWithPreExecHook z3Limits
      z3Cancellation z3Deadline profile workingDirectory descriptor hook

-- | Whether this process was opened by the plain descriptor-bound launch
-- ('openLengthSMTLibDescriptorBoundProcess').  The three
-- @lengthSMTLibProcessUsesDescriptorBound...@ predicates are mutually
-- exclusive: each names exactly one launch strategy, and a path-snapshot
-- process answers 'False' to all of them.
lengthSMTLibProcessUsesDescriptorBoundExecutableLaunch
  :: LengthSMTLibProcess
  -> Bool
lengthSMTLibProcessUsesDescriptorBoundExecutableLaunch =
  Z3Process.z3SMTLibProcessUsesDescriptorBoundExecutableLaunch . toZ3Process

-- | Closed, sanitized result of one effective-ID source execute-access
-- observation supplied by an access checker; native errno values never
-- cross this boundary.  Only 'LengthSMTLibEffectiveIDExecutableAccessAdmitted'
-- lets the launch continue; the other three map to the failure classes of
-- the same name.
data LengthSMTLibEffectiveIDExecutableAccessCheckResult
  = LengthSMTLibEffectiveIDExecutableAccessAdmitted
  | LengthSMTLibEffectiveIDExecutableAccessDenied
  | LengthSMTLibEffectiveIDExecutableAccessCheckUnavailable
  | LengthSMTLibEffectiveIDExecutableAccessCheckFailed
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | The descriptor-bound launch which additionally observes effective-ID
-- source VFS execute access on the opened source, once before copying and
-- again after staging immediately before child allocation.  Both
-- observations are point-in-time only.  On a build without descriptor-bound
-- support this fails with
-- 'LengthSMTLibProcessEffectiveIDExecutableAccessCheckUnavailable'.
openLengthSMTLibDescriptorBoundEffectiveIDExecutableAccessProcess
  :: LengthSMTLibProcessLimits
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> Z3Execution.Z3SMTLibExecutionProfile
  -> FilePath
  -> LengthSMTLibWorkingDirectoryDescriptor
  -> IO (Either LengthSMTLibProcessError LengthSMTLibProcess)
openLengthSMTLibDescriptorBoundEffectiveIDExecutableAccessProcess
    limits cancellation deadline profile workingDirectory
    (LengthSMTLibWorkingDirectoryDescriptor descriptor) =
  openRetained limits cancellation deadline $ \z3Limits z3Cancellation
      z3Deadline ->
    Z3Process.openZ3SMTLibDescriptorBoundEffectiveIDExecutableAccessProcess
      z3Limits z3Cancellation z3Deadline profile workingDirectory descriptor

-- | Deterministic package-private seam of the effective-ID launch for
-- tests.  The access checker replaces the native check and is invoked on
-- the borrowed source descriptor exactly twice along a successful prefix;
-- the hook runs after pin and sealed-image admission but before the second
-- check or any fork, pipe, or child allocation.
openLengthSMTLibDescriptorBoundEffectiveIDExecutableAccessProcessWithHooks
  :: LengthSMTLibProcessLimits
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> Z3Execution.Z3SMTLibExecutionProfile
  -> FilePath
  -> LengthSMTLibWorkingDirectoryDescriptor
  -> (CInt -> IO LengthSMTLibEffectiveIDExecutableAccessCheckResult)
  -> IO ()
  -> IO (Either LengthSMTLibProcessError LengthSMTLibProcess)
-- The checker receives a borrowed numeric source descriptor and must never
-- retain or close it.
openLengthSMTLibDescriptorBoundEffectiveIDExecutableAccessProcessWithHooks
    limits cancellation deadline profile workingDirectory
    (LengthSMTLibWorkingDirectoryDescriptor descriptor) accessCheck hook =
  openRetained limits cancellation deadline $ \z3Limits z3Cancellation
      z3Deadline ->
    Z3Process.openZ3SMTLibDescriptorBoundEffectiveIDExecutableAccessProcessWithHooks
      z3Limits z3Cancellation z3Deadline profile workingDirectory descriptor
      (fmap toZ3EffectiveIDExecutableAccessCheckResult . accessCheck)
      hook

-- | Whether this process was opened by the effective-ID descriptor-bound
-- launch; see 'lengthSMTLibProcessUsesDescriptorBoundExecutableLaunch' for
-- the exclusivity of these predicates.
lengthSMTLibProcessUsesDescriptorBoundEffectiveIDExecutableAccessLaunch
  :: LengthSMTLibProcess
  -> Bool
lengthSMTLibProcessUsesDescriptorBoundEffectiveIDExecutableAccessLaunch =
  Z3Process.z3SMTLibProcessUsesDescriptorBoundEffectiveIDExecutableAccessLaunch
    . toZ3Process

toZ3EffectiveIDExecutableAccessCheckResult
  :: LengthSMTLibEffectiveIDExecutableAccessCheckResult
  -> Z3Process.Z3SMTLibEffectiveIDExecutableAccessCheckResult
toZ3EffectiveIDExecutableAccessCheckResult result = case result of
  LengthSMTLibEffectiveIDExecutableAccessAdmitted ->
    Z3Process.Z3SMTLibEffectiveIDExecutableAccessAdmitted
  LengthSMTLibEffectiveIDExecutableAccessDenied ->
    Z3Process.Z3SMTLibEffectiveIDExecutableAccessDenied
  LengthSMTLibEffectiveIDExecutableAccessCheckUnavailable ->
    Z3Process.Z3SMTLibEffectiveIDExecutableAccessCheckUnavailable
  LengthSMTLibEffectiveIDExecutableAccessCheckFailed ->
    Z3Process.Z3SMTLibEffectiveIDExecutableAccessCheckFailed

-- | Closed, sanitized result of one Linux @AT_EXECVE_CHECK@ observation
-- supplied by an execve checker; native errno values never cross this
-- boundary.  Only 'LengthSMTLibExecveCheckAdmitted' lets the launch
-- continue; the others become the source- or staged-execve-check failure
-- class of the same name, depending on which descriptor was checked.
data LengthSMTLibExecveCheckResult
  = LengthSMTLibExecveCheckAdmitted
  | LengthSMTLibExecveCheckDenied
  | LengthSMTLibExecveCheckUnavailable
  | LengthSMTLibExecveCheckFailed
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Sanitized observation of a borrowed staged-image descriptor.  The
-- requested creation-flag word names the production creator's policy and is
-- not a property observed on the descriptor; the regular-file flag, mode,
-- byte count, and seal set are descriptor observations.  This value
-- transfers no ownership.
data LengthSMTLibExecveCheckStagedImageInspection =
  LengthSMTLibExecveCheckStagedImageInspection
    { lengthSMTLibExecveCheckStagedImageRequestedCreationFlags :: !Word32
    , lengthSMTLibExecveCheckStagedImageRegularFile :: !Bool
    , lengthSMTLibExecveCheckStagedImageMode :: !Word32
    , lengthSMTLibExecveCheckStagedImageByteCount :: !Int64
    , lengthSMTLibExecveCheckStagedImageSeals :: !Word32
    }
  deriving (Eq, Ord, Show)

-- | Inspect a borrowed staged descriptor without retaining or closing it.
-- Requested creation flags name the production creator policy; an injected
-- test descriptor does not acquire MFD_EXEC authority from that field.
inspectLengthSMTLibExecveCheckStagedImage
  :: CInt
  -> IO (Maybe LengthSMTLibExecveCheckStagedImageInspection)
inspectLengthSMTLibExecveCheckStagedImage descriptor = do
  inspected <- Z3Process.inspectZ3SMTLibExecveCheckStagedImage descriptor
  pure $ fmap fromZ3ExecveCheckStagedImageInspection inspected

-- | The Linux 6.14 descriptor-bound launch: the opened source must pass
-- both the effective-ID VFS access check and @AT_EXECVE_CHECK@ before
-- copying and again after staging, and the sealed @MFD_EXEC@ image must
-- pass one final @AT_EXECVE_CHECK@ before child allocation.  On a build
-- without descriptor-bound support this fails with
-- 'LengthSMTLibProcessSourceExecveCheckUnavailable'.
openLengthSMTLibDescriptorBoundExecveCheckExecutableAccessProcess
  :: LengthSMTLibProcessLimits
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> Z3Execution.Z3SMTLibExecutionProfile
  -> FilePath
  -> LengthSMTLibWorkingDirectoryDescriptor
  -> IO (Either LengthSMTLibProcessError LengthSMTLibProcess)
openLengthSMTLibDescriptorBoundExecveCheckExecutableAccessProcess
    limits cancellation deadline profile workingDirectory
    (LengthSMTLibWorkingDirectoryDescriptor descriptor) =
  openRetained limits cancellation deadline $ \z3Limits z3Cancellation
      z3Deadline ->
    Z3Process.openZ3SMTLibDescriptorBoundExecveCheckExecutableAccessProcess
      z3Limits z3Cancellation z3Deadline profile workingDirectory descriptor

-- | Deterministic package-private seam of the execve-check launch for
-- tests.  Both checkers receive borrowed numeric descriptors and must never
-- retain or close them: along a complete successful prefix the access
-- checker sees the source twice, and the execve checker sees the source
-- twice and the staged image once.  The hook runs after sealing and before
-- every final check or child allocation.
openLengthSMTLibDescriptorBoundExecveCheckExecutableAccessProcessWithHooks
  :: LengthSMTLibProcessLimits
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> Z3Execution.Z3SMTLibExecutionProfile
  -> FilePath
  -> LengthSMTLibWorkingDirectoryDescriptor
  -> (CInt -> IO LengthSMTLibEffectiveIDExecutableAccessCheckResult)
  -> (CInt -> IO LengthSMTLibExecveCheckResult)
  -> IO ()
  -> IO (Either LengthSMTLibProcessError LengthSMTLibProcess)
openLengthSMTLibDescriptorBoundExecveCheckExecutableAccessProcessWithHooks
    limits cancellation deadline profile workingDirectory
    (LengthSMTLibWorkingDirectoryDescriptor descriptor) accessCheck
    execveCheck hook =
  openRetained limits cancellation deadline $ \z3Limits z3Cancellation
      z3Deadline ->
    Z3Process.openZ3SMTLibDescriptorBoundExecveCheckExecutableAccessProcessWithHooks
      z3Limits z3Cancellation z3Deadline profile workingDirectory descriptor
      (fmap toZ3EffectiveIDExecutableAccessCheckResult . accessCheck)
      (fmap toZ3ExecveCheckResult . execveCheck)
      hook

-- | Deeper package-private seam of the execve-check launch for old-kernel
-- tests.  In addition to the two checkers and the pre-final hook it injects
-- the staged-image creator and sealer (never used by the production opener)
-- and an inspection hook which borrows the staged descriptor after sealing
-- and metadata verification and must neither retain nor close it.
openLengthSMTLibDescriptorBoundExecveCheckExecutableAccessProcessWithTestHooks
  :: LengthSMTLibProcessLimits
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> Z3Execution.Z3SMTLibExecutionProfile
  -> FilePath
  -> LengthSMTLibWorkingDirectoryDescriptor
  -> (CInt -> IO LengthSMTLibEffectiveIDExecutableAccessCheckResult)
  -> (CInt -> IO LengthSMTLibExecveCheckResult)
  -> IO CInt
  -> (CInt -> Int64 -> IO CInt)
  -> (CInt -> IO ())
  -> IO ()
  -> IO (Either LengthSMTLibProcessError LengthSMTLibProcess)
openLengthSMTLibDescriptorBoundExecveCheckExecutableAccessProcessWithTestHooks
    limits cancellation deadline profile workingDirectory
    (LengthSMTLibWorkingDirectoryDescriptor descriptor) accessCheck
    execveCheck creator sealer inspectionHook hook =
  openRetained limits cancellation deadline $ \z3Limits z3Cancellation
      z3Deadline ->
    Z3Process.openZ3SMTLibDescriptorBoundExecveCheckExecutableAccessProcessWithTestHooks
      z3Limits z3Cancellation z3Deadline profile workingDirectory descriptor
      (fmap toZ3EffectiveIDExecutableAccessCheckResult . accessCheck)
      (fmap toZ3ExecveCheckResult . execveCheck)
      creator sealer inspectionHook hook

-- | Whether this process was opened by the execve-check descriptor-bound
-- launch; see 'lengthSMTLibProcessUsesDescriptorBoundExecutableLaunch' for
-- the exclusivity of these predicates.
lengthSMTLibProcessUsesDescriptorBoundExecveCheckExecutableAccessLaunch
  :: LengthSMTLibProcess
  -> Bool
lengthSMTLibProcessUsesDescriptorBoundExecveCheckExecutableAccessLaunch =
  Z3Process.z3SMTLibProcessUsesDescriptorBoundExecveCheckExecutableAccessLaunch
    . toZ3Process

toZ3ExecveCheckResult
  :: LengthSMTLibExecveCheckResult
  -> Z3Process.Z3SMTLibExecveCheckResult
toZ3ExecveCheckResult result = case result of
  LengthSMTLibExecveCheckAdmitted -> Z3Process.Z3SMTLibExecveCheckAdmitted
  LengthSMTLibExecveCheckDenied -> Z3Process.Z3SMTLibExecveCheckDenied
  LengthSMTLibExecveCheckUnavailable ->
    Z3Process.Z3SMTLibExecveCheckUnavailable
  LengthSMTLibExecveCheckFailed -> Z3Process.Z3SMTLibExecveCheckFailed

fromZ3ExecveCheckStagedImageInspection
  :: Z3Process.Z3SMTLibExecveCheckStagedImageInspection
  -> LengthSMTLibExecveCheckStagedImageInspection
fromZ3ExecveCheckStagedImageInspection inspection =
  LengthSMTLibExecveCheckStagedImageInspection
    { lengthSMTLibExecveCheckStagedImageRequestedCreationFlags =
        Z3Process.z3SMTLibExecveCheckStagedImageRequestedCreationFlags
          inspection
    , lengthSMTLibExecveCheckStagedImageRegularFile =
        Z3Process.z3SMTLibExecveCheckStagedImageRegularFile inspection
    , lengthSMTLibExecveCheckStagedImageMode =
        Z3Process.z3SMTLibExecveCheckStagedImageMode inspection
    , lengthSMTLibExecveCheckStagedImageByteCount =
        Z3Process.z3SMTLibExecveCheckStagedImageByteCount inspection
    , lengthSMTLibExecveCheckStagedImageSeals =
        Z3Process.z3SMTLibExecveCheckStagedImageSeals inspection
    }

-- | The executable snapshot this exact process was opened from.
lengthSMTLibProcessSnapshot
  :: LengthSMTLibProcess
  -> LengthSMTLibExecutableSnapshot
lengthSMTLibProcessSnapshot = LengthSMTLibExecutableSnapshot
  . Z3Process.z3SMTLibProcessSnapshot . toZ3Process

-- | Exact process limits associated with this admitted runtime.
lengthSMTLibProcessLimits
  :: LengthSMTLibProcess
  -> LengthSMTLibProcessLimits
lengthSMTLibProcessLimits = LengthSMTLibProcessLimits
  . Z3Process.z3SMTLibProcessLimits . toZ3Process

-- | The Length identity of this exact process: a tag whose role names the
-- launch strategy, containing the matching schema tag (for example
-- 'lengthSMTLibProcessSchemaTag'), then the raw launch observation fields
-- sealed by the opener, then 'lengthSMTLibProcessLimitsFingerprintField' of
-- the process-owned limits.  Only the outer tag was forced at open time; the
-- field list is encoded lazily on demand.
lengthSMTLibProcessFingerprintField
  :: LengthSMTLibProcess
  -> FingerprintField
lengthSMTLibProcessFingerprintField = processFingerprintField . toZ3Process

-- | Total stdout bytes charged against 'lengthSMTLibProcessStdoutByteLimit'
-- so far, including bytes still queued and not yet read.  It reads limit
-- plus one once the limit has been exceeded.
lengthSMTLibProcessObservedStdoutBytes
  :: LengthSMTLibProcess
  -> IO Natural
lengthSMTLibProcessObservedStdoutBytes =
  Z3Process.z3SMTLibProcessObservedStdoutBytes . toZ3Process

-- | Total stderr bytes observed so far, saturating at
-- 'lengthSMTLibProcessStderrByteLimit' plus one.  Any nonzero value means
-- the process is poisoned with 'LengthSMTLibProcessStderrObserved'.
lengthSMTLibProcessObservedStderrBytes
  :: LengthSMTLibProcess
  -> IO Natural
lengthSMTLibProcessObservedStderrBytes =
  Z3Process.z3SMTLibProcessObservedStderrBytes . toZ3Process

-- | Write exactly these bytes to the child's stdin and flush.  Writes are
-- serialized by a token; each is admitted only while the process is open,
-- unpoisoned, uncancelled, within its deadline, and no stdout terminal
-- condition has been recorded.  A write which fails or is interrupted after
-- admission poisons the process, so 'Right' means every byte was written
-- and flushed.
writeLengthSMTLibProcess
  :: LengthSMTLibProcess
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> ByteString
  -> IO (Either LengthSMTLibProcessError ())
writeLengthSMTLibProcess process cancellation deadline bytes =
  mask $ \restore -> do
    result <- restore $ Z3Process.writeZ3SMTLibProcess
      (toZ3Process process)
      (toZ3ProcessCancellation cancellation)
      (toZ3ProcessDeadline deadline)
      bytes
    retainZ3ProcessResult result

-- | Dequeue the next stdout chunk in FIFO order, waiting until one is
-- available, the process is poisoned or closed, the token is cancelled, or
-- the deadline passes.  A successful chunk is provably nonempty and no
-- larger than 'lengthSMTLibProcessReadChunkByteLimit'; a queued terminal
-- condition (EOF, read failure, or stdout limit exceeded) is delivered at
-- its exact position after all preceding chunks and poisons the process.
nextLengthSMTLibProcessStdoutChunk
  :: LengthSMTLibProcess
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> IO (Either LengthSMTLibProcessError SMTLibCausalStdoutChunk)
nextLengthSMTLibProcessStdoutChunk process cancellation deadline =
  mask $ \restore -> do
    result <- restore $ Z3Process.nextZ3SMTLibProcessStdoutChunk
      (toZ3Process process)
      (toZ3ProcessCancellation cancellation)
      (toZ3ProcessDeadline deadline)
    retainZ3ProcessResult result

-- | Nonblockingly drain the stdout chunks already queued behind the
-- preceding protocol write, admitting them only if every byte is SMT-LIB
-- whitespace, and return the opaque causal-boundary receipt.  Draining is
-- all-or-nothing: on a non-whitespace chunk every dequeued chunk is
-- restored in its original FIFO position and the process is poisoned with
-- 'LengthSMTLibProcessUnexpectedPendingStdout'; a queued terminal condition
-- is propagated at its exact position after any preceding whitespace.
drainLengthSMTLibProcessBoundaryWhitespace
  :: LengthSMTLibProcess
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> IO (Either LengthSMTLibProcessError SMTLibCausalBoundaryWhitespace)
drainLengthSMTLibProcessBoundaryWhitespace process cancellation deadline =
  mask $ \restore -> do
    result <- restore $ Z3Process.drainZ3SMTLibProcessBoundaryWhitespace
      (toZ3Process process)
      (toZ3ProcessCancellation cancellation)
      (toZ3ProcessDeadline deadline)
    retainZ3ProcessResult result

-- | Nonblocking readiness check in 'LengthSMTLibProcessReadyPhase': succeeds
-- only if the token is uncancelled, the deadline has not passed, the
-- process is open and unpoisoned, no stdout is queued, and the child has not
-- exited (checked between two such snapshots).  Queued stdout fails with
-- 'LengthSMTLibProcessUnexpectedPendingStdout' (or the queued terminal
-- condition), an exited child with 'LengthSMTLibProcessExited'; every
-- failure poisons the process.
checkLengthSMTLibProcessReady
  :: LengthSMTLibProcess
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> IO (Either LengthSMTLibProcessError ())
checkLengthSMTLibProcessReady process cancellation deadline =
  mask $ \restore -> do
    result <- restore $ Z3Process.checkZ3SMTLibProcessReady
      (toZ3Process process)
      (toZ3ProcessCancellation cancellation)
      (toZ3ProcessDeadline deadline)
    retainZ3ProcessResult result

-- | Wait for one non-owning STM admission observation under the same
-- precedence as the pipe operations: cancellation, then process poison,
-- then a closed lifecycle, then the action, all bounded by the absolute
-- deadline and re-checked after the action commits.  Failures are reported
-- in the given phase.  The action must not destructively acquire a
-- resource, because the final check may discard its result; observe a gate
-- here, then claim it nonblockingly under masking.
waitLengthSMTLibProcessControl
  :: LengthSMTLibProcess
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> LengthSMTLibProcessPhase
  -> STM value
  -> IO (Either LengthSMTLibProcessError value)
waitLengthSMTLibProcessControl process cancellation deadline phase action =
  mask $ \restore -> do
    result <- restore $ Z3Process.waitZ3SMTLibProcessControl
      (toZ3Process process)
      (toZ3ProcessCancellation cancellation)
      (toZ3ProcessDeadline deadline)
      (toZ3ProcessPhase phase)
      action
    retainZ3ProcessResult result

-- | Close the process and clean up its child: poison it with
-- 'LengthSMTLibProcessClosed', close stdin, then wait for exit with the
-- graceful-close, terminate, and kill escalation before stopping the reader
-- threads and closing the remaining pipes.  Cleanup runs once; concurrent
-- or repeated calls block on and return the same status.  Only the direct
-- child is owned and reaped; descendants are signalled best-effort.
closeLengthSMTLibProcess
  :: LengthSMTLibProcess
  -> IO LengthSMTLibProcessCleanupStatus
closeLengthSMTLibProcess process = mask_ $ do
  cleanup <- Z3Process.closeZ3SMTLibProcess $ toZ3Process process
  let retained = fromZ3ProcessCleanupStatus cleanup
  retained `seq` pure retained

processFingerprintField :: Z3Process.Z3SMTLibProcess -> FingerprintField
processFingerprintField process = FingerprintTag role
  $ FingerprintBytes (BS.unpack schema)
  : Z3Process.z3SMTLibProcessObservationFingerprintFields process
  ++ [ lengthSMTLibProcessLimitsFingerprintField
      $ LengthSMTLibProcessLimits
      $ Z3Process.z3SMTLibProcessLimits process
     ]
 where
  descriptorBound =
    Z3Process.z3SMTLibProcessUsesDescriptorBoundExecutableLaunch process
  effectiveIDAccess =
    Z3Process.z3SMTLibProcessUsesDescriptorBoundEffectiveIDExecutableAccessLaunch
      process
  execveCheckAccess =
    Z3Process.z3SMTLibProcessUsesDescriptorBoundExecveCheckExecutableAccessLaunch
      process
  role = ascii $ if execveCheckAccess
    then concat
      [ "length-z3-descriptor-bound-execve-check-executable-access-"
      , "launched-transport"
      ]
    else if effectiveIDAccess
    then concat
      [ "length-z3-descriptor-bound-effective-id-executable-access-"
      , "launched-transport"
      ]
    else if descriptorBound
      then "length-z3-descriptor-bound-launched-transport"
      else "length-z3-launched-transport"
  schema
    | execveCheckAccess =
        lengthSMTLibDescriptorBoundExecveCheckExecutableAccessProcessSchemaTag
    | effectiveIDAccess =
        lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessProcessSchemaTag
    | descriptorBound = lengthSMTLibDescriptorBoundProcessSchemaTag
    | otherwise = lengthSMTLibProcessSchemaTag

toZ3ProcessLimitSource
  :: LengthSMTLibProcessLimitSource
  -> Z3Process.Z3SMTLibProcessLimitSource
toZ3ProcessLimitSource source = Z3Process.Z3SMTLibProcessLimitSource
  { Z3Process.z3SMTLibProcessLimitSourceExecutableBytes =
      lengthSMTLibProcessLimitSourceExecutableBytes source
  , Z3Process.z3SMTLibProcessLimitSourceStdoutBytes =
      lengthSMTLibProcessLimitSourceStdoutBytes source
  , Z3Process.z3SMTLibProcessLimitSourceStderrBytes =
      lengthSMTLibProcessLimitSourceStderrBytes source
  , Z3Process.z3SMTLibProcessLimitSourceReadChunkBytes =
      lengthSMTLibProcessLimitSourceReadChunkBytes source
  , Z3Process.z3SMTLibProcessLimitSourceGracefulCloseMilliseconds =
      lengthSMTLibProcessLimitSourceGracefulCloseMilliseconds source
  , Z3Process.z3SMTLibProcessLimitSourceTerminateMilliseconds =
      lengthSMTLibProcessLimitSourceTerminateMilliseconds source
  , Z3Process.z3SMTLibProcessLimitSourceKillMilliseconds =
      lengthSMTLibProcessLimitSourceKillMilliseconds source
  }

fromZ3ProcessLimitSource
  :: Z3Process.Z3SMTLibProcessLimitSource
  -> LengthSMTLibProcessLimitSource
fromZ3ProcessLimitSource source = LengthSMTLibProcessLimitSource
  { lengthSMTLibProcessLimitSourceExecutableBytes =
      Z3Process.z3SMTLibProcessLimitSourceExecutableBytes source
  , lengthSMTLibProcessLimitSourceStdoutBytes =
      Z3Process.z3SMTLibProcessLimitSourceStdoutBytes source
  , lengthSMTLibProcessLimitSourceStderrBytes =
      Z3Process.z3SMTLibProcessLimitSourceStderrBytes source
  , lengthSMTLibProcessLimitSourceReadChunkBytes =
      Z3Process.z3SMTLibProcessLimitSourceReadChunkBytes source
  , lengthSMTLibProcessLimitSourceGracefulCloseMilliseconds =
      Z3Process.z3SMTLibProcessLimitSourceGracefulCloseMilliseconds source
  , lengthSMTLibProcessLimitSourceTerminateMilliseconds =
      Z3Process.z3SMTLibProcessLimitSourceTerminateMilliseconds source
  , lengthSMTLibProcessLimitSourceKillMilliseconds =
      Z3Process.z3SMTLibProcessLimitSourceKillMilliseconds source
  }

toZ3ProcessLimits
  :: LengthSMTLibProcessLimits
  -> Z3Process.Z3SMTLibProcessLimits
toZ3ProcessLimits (LengthSMTLibProcessLimits limits) = limits

toZ3ProcessDeadline
  :: LengthSMTLibProcessDeadline
  -> Z3Process.Z3SMTLibProcessDeadline
toZ3ProcessDeadline (LengthSMTLibProcessDeadline deadline) = deadline

toZ3ProcessCancellation
  :: LengthSMTLibProcessCancellation
  -> Z3Process.Z3SMTLibProcessCancellation
toZ3ProcessCancellation (LengthSMTLibProcessCancellation cancellation) =
  cancellation

toZ3ExecutableSnapshot
  :: LengthSMTLibExecutableSnapshot
  -> Z3Process.Z3SMTLibExecutableSnapshot
toZ3ExecutableSnapshot (LengthSMTLibExecutableSnapshot snapshot) = snapshot

toZ3Process :: LengthSMTLibProcess -> Z3Process.Z3SMTLibProcess
toZ3Process (LengthSMTLibProcess process) = process

-- Finish the compatibility mapping while the delegating operation's outer
-- mask is restored.  In particular, a successfully acquired process or
-- destructively dequeued receipt is never separated from its Length result by
-- a new asynchronous-exception window in this facade.
retainZ3ProcessResult
  :: Either Z3Process.Z3SMTLibProcessError value
  -> IO (Either LengthSMTLibProcessError value)
retainZ3ProcessResult result = case result of
  Left failure ->
    let retained = fromZ3ProcessError failure
    in retained `seq` pure (Left retained)
  Right value -> pure $ Right value

fromZ3ProcessError
  :: Z3Process.Z3SMTLibProcessError
  -> LengthSMTLibProcessError
fromZ3ProcessError failure = LengthSMTLibProcessError
  { lengthSMTLibProcessErrorPhase = fromZ3ProcessPhase
      $ Z3Process.z3SMTLibProcessErrorPhase failure
  , lengthSMTLibProcessErrorClass = fromZ3ProcessFailureClass
      $ Z3Process.z3SMTLibProcessErrorClass failure
  , lengthSMTLibProcessErrorObservedAtLeast =
      Z3Process.z3SMTLibProcessErrorObservedAtLeast failure
  , lengthSMTLibProcessErrorCleanupStatus = case
      Z3Process.z3SMTLibProcessErrorCleanupStatus failure of
        Nothing -> Nothing
        Just cleanup -> Just $ fromZ3ProcessCleanupStatus cleanup
  }

fromZ3ProcessCleanupStatus
  :: Z3Process.Z3SMTLibProcessCleanupStatus
  -> LengthSMTLibProcessCleanupStatus
fromZ3ProcessCleanupStatus cleanup = LengthSMTLibProcessCleanupStatus
  { lengthSMTLibProcessCleanupEscalation = fromZ3ProcessCleanupEscalation
      $ Z3Process.z3SMTLibProcessCleanupEscalation cleanup
  , lengthSMTLibProcessCleanupExitCode =
      Z3Process.z3SMTLibProcessCleanupExitCode cleanup
  , lengthSMTLibProcessCleanupFailureCount =
      Z3Process.z3SMTLibProcessCleanupFailureCount cleanup
  , lengthSMTLibProcessCleanupReadersStopped =
      Z3Process.z3SMTLibProcessCleanupReadersStopped cleanup
  }

toZ3ProcessPhase
  :: LengthSMTLibProcessPhase
  -> Z3Process.Z3SMTLibProcessPhase
toZ3ProcessPhase phase = case phase of
  LengthSMTLibProcessLimitPhase -> Z3Process.Z3SMTLibProcessLimitPhase
  LengthSMTLibProcessDeadlinePhase -> Z3Process.Z3SMTLibProcessDeadlinePhase
  LengthSMTLibProcessWorkingDirectoryPhase ->
    Z3Process.Z3SMTLibProcessWorkingDirectoryPhase
  LengthSMTLibProcessSnapshotPhase -> Z3Process.Z3SMTLibProcessSnapshotPhase
  LengthSMTLibProcessSpawnPhase -> Z3Process.Z3SMTLibProcessSpawnPhase
  LengthSMTLibProcessConfigurePhase -> Z3Process.Z3SMTLibProcessConfigurePhase
  LengthSMTLibProcessWritePhase -> Z3Process.Z3SMTLibProcessWritePhase
  LengthSMTLibProcessStdoutPhase -> Z3Process.Z3SMTLibProcessStdoutPhase
  LengthSMTLibProcessStderrPhase -> Z3Process.Z3SMTLibProcessStderrPhase
  LengthSMTLibProcessReadyPhase -> Z3Process.Z3SMTLibProcessReadyPhase
  LengthSMTLibProcessQueryPhase -> Z3Process.Z3SMTLibProcessQueryPhase
  LengthSMTLibProcessClosePhase -> Z3Process.Z3SMTLibProcessClosePhase

fromZ3ProcessPhase
  :: Z3Process.Z3SMTLibProcessPhase
  -> LengthSMTLibProcessPhase
fromZ3ProcessPhase phase = case phase of
  Z3Process.Z3SMTLibProcessLimitPhase -> LengthSMTLibProcessLimitPhase
  Z3Process.Z3SMTLibProcessDeadlinePhase -> LengthSMTLibProcessDeadlinePhase
  Z3Process.Z3SMTLibProcessWorkingDirectoryPhase ->
    LengthSMTLibProcessWorkingDirectoryPhase
  Z3Process.Z3SMTLibProcessSnapshotPhase -> LengthSMTLibProcessSnapshotPhase
  Z3Process.Z3SMTLibProcessSpawnPhase -> LengthSMTLibProcessSpawnPhase
  Z3Process.Z3SMTLibProcessConfigurePhase -> LengthSMTLibProcessConfigurePhase
  Z3Process.Z3SMTLibProcessWritePhase -> LengthSMTLibProcessWritePhase
  Z3Process.Z3SMTLibProcessStdoutPhase -> LengthSMTLibProcessStdoutPhase
  Z3Process.Z3SMTLibProcessStderrPhase -> LengthSMTLibProcessStderrPhase
  Z3Process.Z3SMTLibProcessReadyPhase -> LengthSMTLibProcessReadyPhase
  Z3Process.Z3SMTLibProcessQueryPhase -> LengthSMTLibProcessQueryPhase
  Z3Process.Z3SMTLibProcessClosePhase -> LengthSMTLibProcessClosePhase

fromZ3ProcessFailureClass
  :: Z3Process.Z3SMTLibProcessFailureClass
  -> LengthSMTLibProcessFailureClass
fromZ3ProcessFailureClass failure = case failure of
  Z3Process.Z3SMTLibProcessNonPositiveLimit ->
    LengthSMTLibProcessNonPositiveLimit
  Z3Process.Z3SMTLibProcessLimitConversionOverflow ->
    LengthSMTLibProcessLimitConversionOverflow
  Z3Process.Z3SMTLibProcessCancelled -> LengthSMTLibProcessCancelled
  Z3Process.Z3SMTLibProcessDeadlineExceeded ->
    LengthSMTLibProcessDeadlineExceeded
  Z3Process.Z3SMTLibProcessWorkingDirectoryNotAbsolute ->
    LengthSMTLibProcessWorkingDirectoryNotAbsolute
  Z3Process.Z3SMTLibProcessWorkingDirectoryUnavailable ->
    LengthSMTLibProcessWorkingDirectoryUnavailable
  Z3Process.Z3SMTLibProcessWorkingDirectoryNotEmpty ->
    LengthSMTLibProcessWorkingDirectoryNotEmpty
  Z3Process.Z3SMTLibProcessExecutableUnavailable ->
    LengthSMTLibProcessExecutableUnavailable
  Z3Process.Z3SMTLibProcessExecutableNotRegular ->
    LengthSMTLibProcessExecutableNotRegular
  Z3Process.Z3SMTLibProcessExecutableNotExecutable ->
    LengthSMTLibProcessExecutableNotExecutable
  Z3Process.Z3SMTLibProcessExecutableByteLimitExceeded ->
    LengthSMTLibProcessExecutableByteLimitExceeded
  Z3Process.Z3SMTLibProcessExecutableMetadataChanged ->
    LengthSMTLibProcessExecutableMetadataChanged
  Z3Process.Z3SMTLibProcessExecutableDigestMismatch ->
    LengthSMTLibProcessExecutableDigestMismatch
  Z3Process.Z3SMTLibProcessEffectiveIDExecutableAccessDenied ->
    LengthSMTLibProcessEffectiveIDExecutableAccessDenied
  Z3Process.Z3SMTLibProcessEffectiveIDExecutableAccessCheckUnavailable ->
    LengthSMTLibProcessEffectiveIDExecutableAccessCheckUnavailable
  Z3Process.Z3SMTLibProcessEffectiveIDExecutableAccessCheckFailed ->
    LengthSMTLibProcessEffectiveIDExecutableAccessCheckFailed
  Z3Process.Z3SMTLibProcessSourceExecveCheckDenied ->
    LengthSMTLibProcessSourceExecveCheckDenied
  Z3Process.Z3SMTLibProcessSourceExecveCheckUnavailable ->
    LengthSMTLibProcessSourceExecveCheckUnavailable
  Z3Process.Z3SMTLibProcessSourceExecveCheckFailed ->
    LengthSMTLibProcessSourceExecveCheckFailed
  Z3Process.Z3SMTLibProcessStagedExecveCheckDenied ->
    LengthSMTLibProcessStagedExecveCheckDenied
  Z3Process.Z3SMTLibProcessStagedExecveCheckUnavailable ->
    LengthSMTLibProcessStagedExecveCheckUnavailable
  Z3Process.Z3SMTLibProcessStagedExecveCheckFailed ->
    LengthSMTLibProcessStagedExecveCheckFailed
  Z3Process.Z3SMTLibProcessDescriptorBoundLaunchUnavailable ->
    LengthSMTLibProcessDescriptorBoundLaunchUnavailable
  Z3Process.Z3SMTLibProcessDescriptorBoundStagingFailed ->
    LengthSMTLibProcessDescriptorBoundStagingFailed
  Z3Process.Z3SMTLibProcessDescriptorBoundExecFailed ->
    LengthSMTLibProcessDescriptorBoundExecFailed
  Z3Process.Z3SMTLibProcessSpawnFailed -> LengthSMTLibProcessSpawnFailed
  Z3Process.Z3SMTLibProcessMissingPipe -> LengthSMTLibProcessMissingPipe
  Z3Process.Z3SMTLibProcessHandleConfigurationFailed ->
    LengthSMTLibProcessHandleConfigurationFailed
  Z3Process.Z3SMTLibProcessWriteFailed -> LengthSMTLibProcessWriteFailed
  Z3Process.Z3SMTLibProcessStdoutByteLimitExceeded ->
    LengthSMTLibProcessStdoutByteLimitExceeded
  Z3Process.Z3SMTLibProcessStdoutEOF -> LengthSMTLibProcessStdoutEOF
  Z3Process.Z3SMTLibProcessStdoutReadFailed ->
    LengthSMTLibProcessStdoutReadFailed
  Z3Process.Z3SMTLibProcessUnexpectedPendingStdout ->
    LengthSMTLibProcessUnexpectedPendingStdout
  Z3Process.Z3SMTLibProcessStderrObserved ->
    LengthSMTLibProcessStderrObserved
  Z3Process.Z3SMTLibProcessStderrEOF -> LengthSMTLibProcessStderrEOF
  Z3Process.Z3SMTLibProcessStderrReadFailed ->
    LengthSMTLibProcessStderrReadFailed
  Z3Process.Z3SMTLibProcessExited -> LengthSMTLibProcessExited
  Z3Process.Z3SMTLibProcessClosed -> LengthSMTLibProcessClosed
  Z3Process.Z3SMTLibProcessInternalFailure ->
    LengthSMTLibProcessInternalFailure

fromZ3ProcessCleanupEscalation
  :: Z3Process.Z3SMTLibProcessCleanupEscalation
  -> LengthSMTLibProcessCleanupEscalation
fromZ3ProcessCleanupEscalation escalation = case escalation of
  Z3Process.Z3SMTLibProcessClosedGracefully ->
    LengthSMTLibProcessClosedGracefully
  Z3Process.Z3SMTLibProcessTerminated -> LengthSMTLibProcessTerminated
  Z3Process.Z3SMTLibProcessKilled -> LengthSMTLibProcessKilled
  Z3Process.Z3SMTLibProcessCleanupIncomplete ->
    LengthSMTLibProcessCleanupIncomplete

asciiBytes :: String -> ByteString
asciiBytes = BS.pack . ascii

ascii :: String -> [Word8]
ascii = map $ fromIntegral . ord
