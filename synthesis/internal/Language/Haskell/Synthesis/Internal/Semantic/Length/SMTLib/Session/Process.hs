-- | Length compatibility and identity facade for the shared raw Z3 process.
--
-- The domain-neutral runtime owns subprocess allocation, bounded pipe IO,
-- cancellation, deadlines, and cleanup.  This facade preserves the existing
-- Length-specific sanitized vocabulary and is the sole owner of the raw
-- Length process schema, fingerprint root, and process-limit wrapper tag.
module Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session.Process
  ( lengthSMTLibExecutableSnapshotStrengthTag
  , lengthSMTLibProcessSchemaTag
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
import Data.Word (Word8, Word64)
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

lengthSMTLibProcessSchemaTag :: ByteString
lengthSMTLibProcessSchemaTag = asciiBytes
  "djex-length-z3-raw-process/v2"

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
  | LengthSMTLibProcessExecutableByteLimitExceeded
  | LengthSMTLibProcessExecutableMetadataChanged
  | LengthSMTLibProcessExecutableDigestMismatch
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
processFingerprintField process = FingerprintTag
  (ascii "length-z3-launched-transport")
  $ FingerprintBytes (BS.unpack lengthSMTLibProcessSchemaTag)
  : Z3Process.z3SMTLibProcessObservationFingerprintFields process
  ++ [ lengthSMTLibProcessLimitsFingerprintField
      $ LengthSMTLibProcessLimits
      $ Z3Process.z3SMTLibProcessLimits process
     ]

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
  Z3Process.Z3SMTLibProcessExecutableByteLimitExceeded ->
    LengthSMTLibProcessExecutableByteLimitExceeded
  Z3Process.Z3SMTLibProcessExecutableMetadataChanged ->
    LengthSMTLibProcessExecutableMetadataChanged
  Z3Process.Z3SMTLibProcessExecutableDigestMismatch ->
    LengthSMTLibProcessExecutableDigestMismatch
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
