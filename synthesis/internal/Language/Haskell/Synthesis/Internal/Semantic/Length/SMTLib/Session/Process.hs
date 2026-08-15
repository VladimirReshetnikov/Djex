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

lengthSMTLibExecutableSnapshotStrengthTag :: ByteString
lengthSMTLibExecutableSnapshotStrengthTag =
  Z3Process.z3SMTLibExecutableSnapshotStrengthTag

lengthSMTLibDescriptorBoundExecutableLaunchStrengthTag :: ByteString
lengthSMTLibDescriptorBoundExecutableLaunchStrengthTag =
  Z3Process.z3SMTLibDescriptorBoundExecutableLaunchStrengthTag

lengthSMTLibDescriptorBoundExecutableLaunchSupported :: Bool
lengthSMTLibDescriptorBoundExecutableLaunchSupported =
  Z3Process.z3SMTLibDescriptorBoundExecutableLaunchSupported

lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchStrengthTag
  :: ByteString
lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchStrengthTag =
  Z3Process.z3SMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchStrengthTag

lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchSupported :: Bool
lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchSupported =
  Z3Process.z3SMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchSupported

lengthSMTLibDescriptorBoundExecveCheckExecutableAccessLaunchStrengthTag
  :: ByteString
lengthSMTLibDescriptorBoundExecveCheckExecutableAccessLaunchStrengthTag =
  Z3Process.z3SMTLibDescriptorBoundExecveCheckExecutableAccessLaunchStrengthTag

lengthSMTLibDescriptorBoundExecveCheckExecutableAccessLaunchSupported :: Bool
lengthSMTLibDescriptorBoundExecveCheckExecutableAccessLaunchSupported =
  Z3Process.z3SMTLibDescriptorBoundExecveCheckExecutableAccessLaunchSupported

lengthSMTLibProcessSchemaTag :: ByteString
lengthSMTLibProcessSchemaTag = asciiBytes
  "djex-length-z3-raw-process/v2"

lengthSMTLibDescriptorBoundProcessSchemaTag :: ByteString
lengthSMTLibDescriptorBoundProcessSchemaTag = asciiBytes
  "djex-length-z3-descriptor-bound-sealed-main-image-process/v1"

lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessProcessSchemaTag
  :: ByteString
lengthSMTLibDescriptorBoundEffectiveIDExecutableAccessProcessSchemaTag =
  asciiBytes $ concat
    [ "djex-length-z3-descriptor-bound-effective-id-executable-access-"
    , "sealed-main-image-process/v1"
    ]

lengthSMTLibDescriptorBoundExecveCheckExecutableAccessProcessSchemaTag
  :: ByteString
lengthSMTLibDescriptorBoundExecveCheckExecutableAccessProcessSchemaTag =
  asciiBytes $ concat
    [ "djex-length-z3-descriptor-bound-execve-check-executable-access-"
    , "sealed-main-image-process/v1"
    ]

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

defaultLengthSMTLibProcessLimitSource :: LengthSMTLibProcessLimitSource
defaultLengthSMTLibProcessLimitSource = fromZ3ProcessLimitSource
  Z3Process.defaultZ3SMTLibProcessLimitSource

data LengthSMTLibProcessLimits = LengthSMTLibProcessLimits
  !Z3Process.Z3SMTLibProcessLimits

mkLengthSMTLibProcessLimits
  :: LengthSMTLibProcessLimitSource
  -> Either LengthSMTLibProcessError LengthSMTLibProcessLimits
mkLengthSMTLibProcessLimits source =
  case Z3Process.mkZ3SMTLibProcessLimits $ toZ3ProcessLimitSource source of
    Left failure -> Left $ fromZ3ProcessError failure
    Right limits -> Right $ LengthSMTLibProcessLimits limits

lengthSMTLibProcessExecutableByteLimit
  :: LengthSMTLibProcessLimits
  -> Natural
lengthSMTLibProcessExecutableByteLimit =
  Z3Process.z3SMTLibProcessExecutableByteLimit . toZ3ProcessLimits

lengthSMTLibProcessStdoutByteLimit
  :: LengthSMTLibProcessLimits
  -> Natural
lengthSMTLibProcessStdoutByteLimit =
  Z3Process.z3SMTLibProcessStdoutByteLimit . toZ3ProcessLimits

lengthSMTLibProcessStderrByteLimit
  :: LengthSMTLibProcessLimits
  -> Natural
lengthSMTLibProcessStderrByteLimit =
  Z3Process.z3SMTLibProcessStderrByteLimit . toZ3ProcessLimits

lengthSMTLibProcessReadChunkByteLimit
  :: LengthSMTLibProcessLimits
  -> Natural
lengthSMTLibProcessReadChunkByteLimit =
  Z3Process.z3SMTLibProcessReadChunkByteLimit . toZ3ProcessLimits

lengthSMTLibProcessGracefulCloseMilliseconds
  :: LengthSMTLibProcessLimits
  -> Natural
lengthSMTLibProcessGracefulCloseMilliseconds =
  Z3Process.z3SMTLibProcessGracefulCloseMilliseconds . toZ3ProcessLimits

lengthSMTLibProcessTerminateMilliseconds
  :: LengthSMTLibProcessLimits
  -> Natural
lengthSMTLibProcessTerminateMilliseconds =
  Z3Process.z3SMTLibProcessTerminateMilliseconds . toZ3ProcessLimits

lengthSMTLibProcessKillMilliseconds
  :: LengthSMTLibProcessLimits
  -> Natural
lengthSMTLibProcessKillMilliseconds =
  Z3Process.z3SMTLibProcessKillMilliseconds . toZ3ProcessLimits

lengthSMTLibProcessLimitsFingerprintField
  :: LengthSMTLibProcessLimits
  -> FingerprintField
lengthSMTLibProcessLimitsFingerprintField limits = FingerprintTag
  (ascii "length-z3-process-limits/v1")
  $ Z3Process.z3SMTLibProcessLimitFingerprintFields
  $ toZ3ProcessLimits limits

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

data LengthSMTLibProcessCleanupEscalation
  = LengthSMTLibProcessClosedGracefully
  | LengthSMTLibProcessTerminated
  | LengthSMTLibProcessKilled
  | LengthSMTLibProcessCleanupIncomplete
  deriving (Bounded, Enum, Eq, Ord, Show)

data LengthSMTLibProcessCleanupStatus = LengthSMTLibProcessCleanupStatus
  { lengthSMTLibProcessCleanupEscalation
      :: !LengthSMTLibProcessCleanupEscalation
  , lengthSMTLibProcessCleanupExitCode :: !(Maybe ExitCode)
  , lengthSMTLibProcessCleanupFailureCount :: !Natural
  , lengthSMTLibProcessCleanupReadersStopped :: !Bool
  }
  deriving (Eq, Ord, Show)

data LengthSMTLibProcessError = LengthSMTLibProcessError
  { lengthSMTLibProcessErrorPhase :: !LengthSMTLibProcessPhase
  , lengthSMTLibProcessErrorClass :: !LengthSMTLibProcessFailureClass
  , lengthSMTLibProcessErrorObservedAtLeast :: !(Maybe Natural)
  , lengthSMTLibProcessErrorCleanupStatus
      :: !(Maybe LengthSMTLibProcessCleanupStatus)
  }
  deriving (Eq, Ord, Show)

data LengthSMTLibProcessDeadline = LengthSMTLibProcessDeadline
  !Z3Process.Z3SMTLibProcessDeadline

mkLengthSMTLibProcessDeadline :: Word64 -> LengthSMTLibProcessDeadline
mkLengthSMTLibProcessDeadline = LengthSMTLibProcessDeadline
  . Z3Process.mkZ3SMTLibProcessDeadline

lengthSMTLibProcessDeadlineAfterMilliseconds
  :: Int
  -> IO (Either LengthSMTLibProcessError LengthSMTLibProcessDeadline)
lengthSMTLibProcessDeadlineAfterMilliseconds milliseconds = do
  result <- Z3Process.z3SMTLibProcessDeadlineAfterMilliseconds milliseconds
  pure $ case result of
    Left failure -> Left $ fromZ3ProcessError failure
    Right deadline -> Right $ LengthSMTLibProcessDeadline deadline

compareLengthSMTLibProcessDeadline
  :: LengthSMTLibProcessDeadline
  -> LengthSMTLibProcessDeadline
  -> Ordering
compareLengthSMTLibProcessDeadline
    (LengthSMTLibProcessDeadline left)
    (LengthSMTLibProcessDeadline right) =
      Z3Process.compareZ3SMTLibProcessDeadline left right

minimumLengthSMTLibProcessDeadline
  :: LengthSMTLibProcessDeadline
  -> LengthSMTLibProcessDeadline
  -> LengthSMTLibProcessDeadline
minimumLengthSMTLibProcessDeadline
    (LengthSMTLibProcessDeadline left)
    (LengthSMTLibProcessDeadline right) = LengthSMTLibProcessDeadline
      $ Z3Process.minimumZ3SMTLibProcessDeadline left right

checkLengthSMTLibProcessDeadline
  :: LengthSMTLibProcessDeadline
  -> IO (Either LengthSMTLibProcessError ())
checkLengthSMTLibProcessDeadline
    (LengthSMTLibProcessDeadline deadline) = do
  checked <- Z3Process.checkZ3SMTLibProcessDeadline deadline
  pure $ case checked of
    Left failure -> Left $ fromZ3ProcessError failure
    Right () -> Right ()

lengthSMTLibProcessMonotonicTimeNanoseconds :: IO Word64
lengthSMTLibProcessMonotonicTimeNanoseconds =
  Z3Process.z3SMTLibProcessMonotonicTimeNanoseconds

lengthSMTLibProcessDeadlineFingerprintField
  :: LengthSMTLibProcessDeadline
  -> FingerprintField
lengthSMTLibProcessDeadlineFingerprintField =
  Z3Process.z3SMTLibProcessDeadlineFingerprintField . toZ3ProcessDeadline

data LengthSMTLibProcessCancellation = LengthSMTLibProcessCancellation
  !Z3Process.Z3SMTLibProcessCancellation

newLengthSMTLibProcessCancellation :: IO LengthSMTLibProcessCancellation
newLengthSMTLibProcessCancellation = LengthSMTLibProcessCancellation
  <$> Z3Process.newZ3SMTLibProcessCancellation

cancelLengthSMTLibProcess :: LengthSMTLibProcessCancellation -> IO ()
cancelLengthSMTLibProcess = Z3Process.cancelZ3SMTLibProcess
  . toZ3ProcessCancellation

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

data LengthSMTLibExecutableSnapshot = LengthSMTLibExecutableSnapshot
  !Z3Process.Z3SMTLibExecutableSnapshot

lengthSMTLibExecutableSnapshotSHA256
  :: LengthSMTLibExecutableSnapshot
  -> ByteString
lengthSMTLibExecutableSnapshotSHA256 =
  Z3Process.z3SMTLibExecutableSnapshotSHA256 . toZ3ExecutableSnapshot

lengthSMTLibExecutableSnapshotByteCount
  :: LengthSMTLibExecutableSnapshot
  -> Natural
lengthSMTLibExecutableSnapshotByteCount =
  Z3Process.z3SMTLibExecutableSnapshotByteCount . toZ3ExecutableSnapshot

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

data LengthSMTLibWorkingDirectoryDescriptor =
  LengthSMTLibWorkingDirectoryDescriptor
    !Z3Process.Z3SMTLibWorkingDirectoryDescriptor

mkLengthSMTLibWorkingDirectoryDescriptor
  :: Int
  -> LengthSMTLibWorkingDirectoryDescriptor
mkLengthSMTLibWorkingDirectoryDescriptor =
  LengthSMTLibWorkingDirectoryDescriptor
  . Z3Process.mkZ3SMTLibWorkingDirectoryDescriptor

openLengthSMTLibProcess
  :: LengthSMTLibProcessLimits
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> Z3Execution.Z3SMTLibExecutionProfile
  -> FilePath
  -> IO (Either LengthSMTLibProcessError LengthSMTLibProcess)
openLengthSMTLibProcess limits cancellation deadline profile workingDirectory =
  mask $ \restore -> do
    opened <- restore $ Z3Process.openZ3SMTLibProcess
      (toZ3ProcessLimits limits)
      (toZ3ProcessCancellation cancellation)
      (toZ3ProcessDeadline deadline)
      profile workingDirectory
    case opened of
      Left failure ->
        let retained = fromZ3ProcessError failure
        in retained `seq` pure (Left retained)
      Right process ->
        let retained = LengthSMTLibProcess process
            -- Preserve the former strict cached-root demand at the successful
            -- masked handoff. Only the outer FingerprintTag is demanded; its
            -- ordered observation field list deliberately stays lazy.
            transientFingerprint = processFingerprintField process
        in retained `seq` transientFingerprint `seq` pure (Right retained)

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
  mask $ \restore -> do
    opened <- restore
      $ Z3Process.openZ3SMTLibDescriptorBoundProcessWithPreExecHook
          (toZ3ProcessLimits limits)
          (toZ3ProcessCancellation cancellation)
          (toZ3ProcessDeadline deadline)
          profile workingDirectory descriptor hook
    case opened of
      Left failure ->
        let retained = fromZ3ProcessError failure
        in retained `seq` pure (Left retained)
      Right process ->
        let retained = LengthSMTLibProcess process
            transientFingerprint = processFingerprintField process
        in retained `seq` transientFingerprint `seq` pure (Right retained)

lengthSMTLibProcessUsesDescriptorBoundExecutableLaunch
  :: LengthSMTLibProcess
  -> Bool
lengthSMTLibProcessUsesDescriptorBoundExecutableLaunch =
  Z3Process.z3SMTLibProcessUsesDescriptorBoundExecutableLaunch . toZ3Process

data LengthSMTLibEffectiveIDExecutableAccessCheckResult
  = LengthSMTLibEffectiveIDExecutableAccessAdmitted
  | LengthSMTLibEffectiveIDExecutableAccessDenied
  | LengthSMTLibEffectiveIDExecutableAccessCheckUnavailable
  | LengthSMTLibEffectiveIDExecutableAccessCheckFailed
  deriving (Bounded, Enum, Eq, Ord, Show)

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
  mask $ \restore -> do
    opened <- restore
      $ Z3Process.openZ3SMTLibDescriptorBoundEffectiveIDExecutableAccessProcess
          (toZ3ProcessLimits limits)
          (toZ3ProcessCancellation cancellation)
          (toZ3ProcessDeadline deadline)
          profile workingDirectory descriptor
    retainOpenedProcess opened

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
  mask $ \restore -> do
    opened <- restore
      $ Z3Process.openZ3SMTLibDescriptorBoundEffectiveIDExecutableAccessProcessWithHooks
          (toZ3ProcessLimits limits)
          (toZ3ProcessCancellation cancellation)
          (toZ3ProcessDeadline deadline)
          profile workingDirectory descriptor
          (fmap toZ3EffectiveIDExecutableAccessCheckResult . accessCheck)
          hook
    retainOpenedProcess opened

lengthSMTLibProcessUsesDescriptorBoundEffectiveIDExecutableAccessLaunch
  :: LengthSMTLibProcess
  -> Bool
lengthSMTLibProcessUsesDescriptorBoundEffectiveIDExecutableAccessLaunch =
  Z3Process.z3SMTLibProcessUsesDescriptorBoundEffectiveIDExecutableAccessLaunch
    . toZ3Process

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

data LengthSMTLibExecveCheckResult
  = LengthSMTLibExecveCheckAdmitted
  | LengthSMTLibExecveCheckDenied
  | LengthSMTLibExecveCheckUnavailable
  | LengthSMTLibExecveCheckFailed
  deriving (Bounded, Enum, Eq, Ord, Show)

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
  mask $ \restore -> do
    opened <- restore
      $ Z3Process.openZ3SMTLibDescriptorBoundExecveCheckExecutableAccessProcess
          (toZ3ProcessLimits limits)
          (toZ3ProcessCancellation cancellation)
          (toZ3ProcessDeadline deadline)
          profile workingDirectory descriptor
    retainOpenedProcess opened

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
  mask $ \restore -> do
    opened <- restore
      $ Z3Process.openZ3SMTLibDescriptorBoundExecveCheckExecutableAccessProcessWithHooks
          (toZ3ProcessLimits limits)
          (toZ3ProcessCancellation cancellation)
          (toZ3ProcessDeadline deadline)
          profile workingDirectory descriptor
          (fmap toZ3EffectiveIDExecutableAccessCheckResult . accessCheck)
          (fmap toZ3ExecveCheckResult . execveCheck)
          hook
    retainOpenedProcess opened

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
  mask $ \restore -> do
    opened <- restore
      $ Z3Process.openZ3SMTLibDescriptorBoundExecveCheckExecutableAccessProcessWithTestHooks
          (toZ3ProcessLimits limits)
          (toZ3ProcessCancellation cancellation)
          (toZ3ProcessDeadline deadline)
          profile workingDirectory descriptor
          (fmap toZ3EffectiveIDExecutableAccessCheckResult . accessCheck)
          (fmap toZ3ExecveCheckResult . execveCheck)
          creator sealer inspectionHook hook
    retainOpenedProcess opened

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

lengthSMTLibProcessFingerprintField
  :: LengthSMTLibProcess
  -> FingerprintField
lengthSMTLibProcessFingerprintField = processFingerprintField . toZ3Process

lengthSMTLibProcessObservedStdoutBytes
  :: LengthSMTLibProcess
  -> IO Natural
lengthSMTLibProcessObservedStdoutBytes =
  Z3Process.z3SMTLibProcessObservedStdoutBytes . toZ3Process

lengthSMTLibProcessObservedStderrBytes
  :: LengthSMTLibProcess
  -> IO Natural
lengthSMTLibProcessObservedStderrBytes =
  Z3Process.z3SMTLibProcessObservedStderrBytes . toZ3Process

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
