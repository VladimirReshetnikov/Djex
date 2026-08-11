{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

-- | Private ownership of one raw Length/Z3 subprocess.
--
-- The executable observation retained here is deliberately a bounded
-- pre-spawn pathname snapshot.  The original pathname is subsequently passed
-- to 'proc'.  A concurrent namespace or file-content mutation can therefore
-- make the executed file differ from the snapshot.  Neither the SHA-256 value
-- nor a successful pin comparison is executed-image attestation.
--
-- The owner keeps stdout and stderr separate.  Stdout is the only stream a
-- protocol layer may consume; the first observed stderr byte poisons the whole
-- process.  Stdout is charged before enqueueing, so its queue is bounded by the
-- session limit rather than consumer speed.  After stderr poison, strict chunks
-- are discarded to keep finite floods from retaining the child; its retained
-- count saturates at the configured maximum plus one.
--
-- Cleanup owns and reaps the direct child.  Process-group signalling is
-- best-effort only while that leader has not been observed reaped.  This
-- portable implementation cannot census descendants or prove ownership of a
-- numeric process-group identifier after leader exit, so it makes no claim
-- that detached or surviving descendants have been killed.
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
  , LengthSMTLibProcessCancellation
  , newLengthSMTLibProcessCancellation
  , cancelLengthSMTLibProcess
  , runBeforeLengthSMTLibProcessDeadline
  , LengthSMTLibExecutableSnapshot
  , lengthSMTLibExecutableSnapshotSHA256
  , lengthSMTLibExecutableSnapshotByteCount
  , lengthSMTLibExecutableSnapshotFingerprintField
  , LengthSMTLibProcess
  , openLengthSMTLibProcess
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

import Control.Concurrent
  ( ThreadId
  , forkIO
  , forkIOWithUnmask
  , killThread
  , threadDelay
  )
import Control.Concurrent.MVar
  ( MVar
  , modifyMVar
  , modifyMVar_
  , newEmptyMVar
  , newMVar
  , putMVar
  , readMVar
  , takeMVar
  )
import Control.Concurrent.STM
  ( STM
  , TMVar
  , TQueue
  , TVar
  , atomically
  , isEmptyTQueue
  , modifyTVar'
  , newEmptyTMVarIO
  , newTQueueIO
  , newTMVarIO
  , newTVarIO
  , orElse
  , peekTQueue
  , putTMVar
  , readTQueue
  , readTMVar
  , readTVar
  , retry
  , takeTMVar
  , tryPutTMVar
  , tryReadTMVar
  , writeTQueue
  , writeTVar
  )
import Control.Exception
  ( SomeException
  , evaluate
  , finally
  , mask
  , mask_
  , onException
  , try
  )
import Control.Monad (void, when)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Word (Word8, Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Numeric.Natural (Natural)
import System.Directory
  ( canonicalizePath
  , doesDirectoryExist
  , doesFileExist
  , doesPathExist
  , executable
  , getFileSize
  , getModificationTime
  , getPermissions
  , readable
  , searchable
  , writable
  )
import System.Exit (ExitCode)
import System.FilePath (isAbsolute)
import System.IO
  ( BufferMode (NoBuffering)
  , Handle
  , IOMode (ReadMode)
  , hClose
  , hFlush
  , hSetBinaryMode
  , hSetBuffering
  , withBinaryFile
  )
import System.IO.Error (tryIOError)
import System.Process
  ( CreateProcess
      ( close_fds
      , create_group
      , cwd
      , delegate_ctlc
      , env
      , std_err
      , std_in
      , std_out
      , use_process_jobs
      )
  , ProcessHandle
  , StdStream (CreatePipe)
  , createProcess
  , getPid
  , getProcessExitCode
  , proc
  , terminateProcess
  )
#ifdef mingw32_HOST_OS
import System.Directory (listDirectory)
import System.Process (interruptProcessGroupOf)
#endif
import System.Timeout (timeout)

#ifndef mingw32_HOST_OS
import Control.Exception (bracket)
import qualified System.Posix.Directory as PosixDirectory
import System.Posix.Files
  ( deviceID
  , fileID
  , fileMode
  , fileSize
  , getFileStatus
  , isRegularFile
  , linkCount
  , modificationTime
  , statusChangeTime
  )
import System.Posix.Signals (sigKILL, sigTERM, signalProcessGroup)
#endif

import Language.Haskell.Synthesis.Internal.Fingerprint
  ( FingerprintField (..)
  , fingerprintCanonicalBytes
  )
import Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Execution
  ( LengthSMTLibExecutionConfig
  , lengthSMTLibExecutionConfiguredArgumentVector
  , lengthSMTLibExecutionExecutablePath
  , lengthSMTLibExecutionExpectedExecutableSHA256
  , lengthSMTLibExecutionPolicyFingerprint
  )

-- | Explicitly weaker than an executed-image attestation method.
lengthSMTLibExecutableSnapshotStrengthTag :: ByteString
lengthSMTLibExecutableSnapshotStrengthTag = asciiBytes
  "path-snapshot-then-direct-spawn/stable-namespace-assumption/v1"

lengthSMTLibProcessSchemaTag :: ByteString
lengthSMTLibProcessSchemaTag = asciiBytes
  "djex-length-z3-raw-process/v1"

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
defaultLengthSMTLibProcessLimitSource = LengthSMTLibProcessLimitSource
  { lengthSMTLibProcessLimitSourceExecutableBytes = 268435456
  , lengthSMTLibProcessLimitSourceStdoutBytes = 1048576
  , lengthSMTLibProcessLimitSourceStderrBytes = 65536
  , lengthSMTLibProcessLimitSourceReadChunkBytes = 4096
  , lengthSMTLibProcessLimitSourceGracefulCloseMilliseconds = 100
  , lengthSMTLibProcessLimitSourceTerminateMilliseconds = 500
  , lengthSMTLibProcessLimitSourceKillMilliseconds = 500
  }

data LengthSMTLibProcessLimits = LengthSMTLibProcessLimits
  !Natural !Natural !Natural !Int !Int !Int !Int

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
  | LengthSMTLibProcessClosePhase
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Stable sanitized classes.  No constructor retains a path, digest, command,
-- output byte, exception string, or operating-system error text.
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

mkLengthSMTLibProcessLimits
  :: LengthSMTLibProcessLimitSource
  -> Either LengthSMTLibProcessError LengthSMTLibProcessLimits
mkLengthSMTLibProcessLimits source = do
  executableMaximum <- positive
    $ lengthSMTLibProcessLimitSourceExecutableBytes source
  chunk <- positiveInt
    $ lengthSMTLibProcessLimitSourceReadChunkBytes source
  graceful <- milliseconds
    $ lengthSMTLibProcessLimitSourceGracefulCloseMilliseconds source
  terminate <- milliseconds
    $ lengthSMTLibProcessLimitSourceTerminateMilliseconds source
  kill <- milliseconds
    $ lengthSMTLibProcessLimitSourceKillMilliseconds source
  pure $ LengthSMTLibProcessLimits
    executableMaximum
    (lengthSMTLibProcessLimitSourceStdoutBytes source)
    (lengthSMTLibProcessLimitSourceStderrBytes source)
    chunk graceful terminate kill
 where
  positive value
    | value == 0 = Left $ processError LengthSMTLibProcessLimitPhase
        LengthSMTLibProcessNonPositiveLimit $ Just value
    | otherwise = Right value
  positiveInt value = do
    retained <- positive value
    if retained > fromIntegral (maxBound :: Int)
      then Left $ processError LengthSMTLibProcessLimitPhase
        LengthSMTLibProcessLimitConversionOverflow $ Just retained
      else Right $ fromIntegral retained
  milliseconds value = do
    retained <- positive value
    if retained > fromIntegral ((maxBound :: Int) `div` 1000)
      then Left $ processError LengthSMTLibProcessLimitPhase
        LengthSMTLibProcessLimitConversionOverflow $ Just retained
      else Right $ fromIntegral retained

lengthSMTLibProcessExecutableByteLimit
  :: LengthSMTLibProcessLimits
  -> Natural
lengthSMTLibProcessExecutableByteLimit
    (LengthSMTLibProcessLimits value _ _ _ _ _ _) = value

lengthSMTLibProcessStdoutByteLimit
  :: LengthSMTLibProcessLimits
  -> Natural
lengthSMTLibProcessStdoutByteLimit
    (LengthSMTLibProcessLimits _ value _ _ _ _ _) = value

lengthSMTLibProcessStderrByteLimit
  :: LengthSMTLibProcessLimits
  -> Natural
lengthSMTLibProcessStderrByteLimit
    (LengthSMTLibProcessLimits _ _ value _ _ _ _) = value

lengthSMTLibProcessReadChunkByteLimit
  :: LengthSMTLibProcessLimits
  -> Natural
lengthSMTLibProcessReadChunkByteLimit
    (LengthSMTLibProcessLimits _ _ _ value _ _ _) = fromIntegral value

lengthSMTLibProcessGracefulCloseMilliseconds
  :: LengthSMTLibProcessLimits
  -> Natural
lengthSMTLibProcessGracefulCloseMilliseconds
    (LengthSMTLibProcessLimits _ _ _ _ value _ _) = fromIntegral value

lengthSMTLibProcessTerminateMilliseconds
  :: LengthSMTLibProcessLimits
  -> Natural
lengthSMTLibProcessTerminateMilliseconds
    (LengthSMTLibProcessLimits _ _ _ _ _ value _) = fromIntegral value

lengthSMTLibProcessKillMilliseconds
  :: LengthSMTLibProcessLimits
  -> Natural
lengthSMTLibProcessKillMilliseconds
    (LengthSMTLibProcessLimits _ _ _ _ _ _ value) = fromIntegral value

lengthSMTLibProcessLimitsFingerprintField
  :: LengthSMTLibProcessLimits
  -> FingerprintField
lengthSMTLibProcessLimitsFingerprintField limits = FingerprintTag
  (ascii "length-z3-process-limits/v1")
  [ FingerprintTag (ascii "executable-bytes")
      [FingerprintNatural $ lengthSMTLibProcessExecutableByteLimit limits]
  , FingerprintTag (ascii "stdout-bytes")
      [FingerprintNatural $ lengthSMTLibProcessStdoutByteLimit limits]
  , FingerprintTag (ascii "stderr-bytes")
      [FingerprintNatural $ lengthSMTLibProcessStderrByteLimit limits]
  , FingerprintTag (ascii "read-chunk-bytes")
      [FingerprintNatural $ lengthSMTLibProcessReadChunkByteLimit limits]
  , FingerprintTag (ascii "graceful-close-milliseconds")
      [ FingerprintNatural
          $ lengthSMTLibProcessGracefulCloseMilliseconds limits
      ]
  , FingerprintTag (ascii "terminate-milliseconds")
      [FingerprintNatural $ lengthSMTLibProcessTerminateMilliseconds limits]
  , FingerprintTag (ascii "kill-milliseconds")
      [FingerprintNatural $ lengthSMTLibProcessKillMilliseconds limits]
  ]

newtype LengthSMTLibProcessDeadline = LengthSMTLibProcessDeadline Word64

mkLengthSMTLibProcessDeadline :: Word64 -> LengthSMTLibProcessDeadline
mkLengthSMTLibProcessDeadline = LengthSMTLibProcessDeadline

lengthSMTLibProcessDeadlineAfterMilliseconds
  :: Int
  -> IO (Either LengthSMTLibProcessError LengthSMTLibProcessDeadline)
lengthSMTLibProcessDeadlineAfterMilliseconds milliseconds
  | milliseconds <= 0 = pure $ Left $ processError
      LengthSMTLibProcessDeadlinePhase
      LengthSMTLibProcessNonPositiveLimit
      (Just $ fromIntegral $ max 0 milliseconds)
  | otherwise = do
      now <- getMonotonicTimeNSec
      let delta = toInteger milliseconds * 1000000
          target = toInteger now + delta
      pure $ if target > toInteger (maxBound :: Word64)
        then Left $ processError LengthSMTLibProcessDeadlinePhase
          LengthSMTLibProcessLimitConversionOverflow
          (Just $ fromIntegral milliseconds)
        else Right $ LengthSMTLibProcessDeadline $ fromInteger target

lengthSMTLibProcessMonotonicTimeNanoseconds :: IO Word64
lengthSMTLibProcessMonotonicTimeNanoseconds = getMonotonicTimeNSec

newtype LengthSMTLibProcessCancellation =
  LengthSMTLibProcessCancellation (TVar Bool)

newLengthSMTLibProcessCancellation :: IO LengthSMTLibProcessCancellation
newLengthSMTLibProcessCancellation =
  LengthSMTLibProcessCancellation <$> newTVarIO False

cancelLengthSMTLibProcess :: LengthSMTLibProcessCancellation -> IO ()
cancelLengthSMTLibProcess (LengthSMTLibProcessCancellation cancelled) =
  atomically $ writeTVar cancelled True

-- | Run pre-process allocation work under the same absolute opener deadline.
-- The action runs unmasked in a private thread.  Cancellation or deadline
-- failure interrupts and joins that thread before returning.  If the action
-- produced a value but lost the final cancellation/deadline check, the supplied
-- rollback is run on that value before it is discarded.  Exception payloads
-- from either callback are never retained.  Completion of interruption and
-- rollback is intentionally joined; its latency therefore depends on the
-- interruptibility of these closed, package-private allocation actions rather
-- than claiming a bound for arbitrary 'IO'.
runBeforeLengthSMTLibProcessDeadline
  :: LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> IO value
  -> (value -> IO ())
  -> IO (Either LengthSMTLibProcessError value)
runBeforeLengthSMTLibProcessDeadline cancellation deadline action rollback =
  mask $ \restore -> do
    initial <- checkCancellationDeadline
      LengthSMTLibProcessDeadlinePhase cancellation deadline
    case initial of
      Left failure -> pure $ Left failure
      Right () -> do
        outcome <- newEmptyTMVarIO
        done <- newEmptyTMVarIO
        thread <- forkIOWithUnmask $ \unmask ->
          (tryAny (unmask action) >>= atomically . putTMVar outcome)
          `finally` atomically (putTMVar done ())
        let await = waitBeforeDeadline cancellation deadline outcome
            producedValue = do
              observed <- atomically $ tryReadTMVar outcome
              pure $ case observed of
                Just (Right value) -> Just value
                _ -> Nothing
            rollbackProduced = do
              produced <- producedValue
              case produced of
                Nothing -> pure ()
                Just value -> void $ tryAny $ rollback value
            stopJoinRollback = do
              killThread thread
              atomically $ readTMVar done
              rollbackProduced
        result <- restore await `onException` stopJoinRollback
        case result of
          Left failure -> do
            stopJoinRollback
            pure $ Left failure
          Right attempted -> do
            atomically $ readTMVar done
            finalControl <- checkCancellationDeadline
              LengthSMTLibProcessDeadlinePhase cancellation deadline
            case finalControl of
              Left failure -> do
                rollbackProduced
                pure $ Left failure
              Right () -> case attempted of
                Left _ -> pure $ Left $ processError
                  LengthSMTLibProcessDeadlinePhase
                  LengthSMTLibProcessInternalFailure Nothing
                Right value -> pure $ Right value

data PortableMetadata = PortableMetadata
  !Integer !Bool !Bool !Bool !Bool !String
  deriving (Eq)

#ifndef mingw32_HOST_OS
data PosixMetadata = PosixMetadata
  !Integer !Integer !Integer !Integer !Integer !String !String
  deriving (Eq)
#endif

data LengthSMTLibExecutableSnapshot = LengthSMTLibExecutableSnapshot
  !ByteString
  !Natural
  !FingerprintField

lengthSMTLibExecutableSnapshotSHA256
  :: LengthSMTLibExecutableSnapshot
  -> ByteString
lengthSMTLibExecutableSnapshotSHA256
    (LengthSMTLibExecutableSnapshot digest _ _) = digest

lengthSMTLibExecutableSnapshotByteCount
  :: LengthSMTLibExecutableSnapshot
  -> Natural
lengthSMTLibExecutableSnapshotByteCount
    (LengthSMTLibExecutableSnapshot _ count _) = count

lengthSMTLibExecutableSnapshotFingerprintField
  :: LengthSMTLibExecutableSnapshot
  -> FingerprintField
lengthSMTLibExecutableSnapshotFingerprintField
    (LengthSMTLibExecutableSnapshot _ _ field) = field

data ProcessLifecycle = ProcessOpen | ProcessClosing | ProcessClosed
  deriving (Eq)

-- Stdout terminal conditions stay in the same FIFO as chunks.  In particular,
-- EOF cannot overtake bytes which were read before it.
data StdoutEvent
  = StdoutChunk !ByteString
  | StdoutTerminal !LengthSMTLibProcessError

data ManagedThread = ManagedThread !ThreadId !(TMVar ())

data CloseState
  = CloseNotStarted
  | CloseRunning !(MVar LengthSMTLibProcessCleanupStatus)
  | CloseFinished !LengthSMTLibProcessCleanupStatus

data LengthSMTLibProcess = LengthSMTLibProcess
  { processInput :: !Handle
  , processOutput :: !Handle
  , processErrorOutput :: !Handle
  , processHandle :: !ProcessHandle
  , processGroupIdentifier :: !(Maybe Integer)
  , processLimits :: !LengthSMTLibProcessLimits
  , processSnapshot :: !LengthSMTLibExecutableSnapshot
  , processIdentityField :: !FingerprintField
  , processStdoutQueue :: !(TQueue StdoutEvent)
  , processStdoutTerminal :: !(TVar (Maybe LengthSMTLibProcessError))
  , processPoison :: !(TVar (Maybe LengthSMTLibProcessError))
  , processStdoutCount :: !(TVar Natural)
  , processStderrCount :: !(TVar Natural)
  , processLifecycle :: !(TVar ProcessLifecycle)
  , processWriteToken :: !(TMVar ())
  , processThreads :: !(TVar [ManagedThread])
  , processCloseState :: !(MVar CloseState)
  }

lengthSMTLibProcessSnapshot
  :: LengthSMTLibProcess
  -> LengthSMTLibExecutableSnapshot
lengthSMTLibProcessSnapshot = processSnapshot

lengthSMTLibProcessFingerprintField
  :: LengthSMTLibProcess
  -> FingerprintField
lengthSMTLibProcessFingerprintField = processIdentityField

lengthSMTLibProcessObservedStdoutBytes
  :: LengthSMTLibProcess
  -> IO Natural
lengthSMTLibProcessObservedStdoutBytes process =
  atomically $ readTVar $ processStdoutCount process

lengthSMTLibProcessObservedStderrBytes
  :: LengthSMTLibProcess
  -> IO Natural
lengthSMTLibProcessObservedStderrBytes process =
  atomically $ readTVar $ processStderrCount process

openLengthSMTLibProcess
  :: LengthSMTLibProcessLimits
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> LengthSMTLibExecutionConfig
  -> FilePath
  -> IO (Either LengthSMTLibProcessError LengthSMTLibProcess)
openLengthSMTLibProcess limits cancellation deadline config workingDirectory =
  mask $ \restore -> do
    initial <- checkCancellationDeadline
      LengthSMTLibProcessSnapshotPhase cancellation deadline
    case initial of
      Left failure -> pure $ Left failure
      Right () -> do
        observed <- restore $ snapshotExecutable limits cancellation deadline
          config workingDirectory
        case observed of
          Left failure -> pure $ Left failure
          Right (snapshot, canonicalWorkingDirectory) -> do
            beforeSpawn <- checkCancellationDeadline
              LengthSMTLibProcessSpawnPhase cancellation deadline
            case beforeSpawn of
              Left failure -> pure $ Left failure
              Right () -> spawn snapshot canonicalWorkingDirectory
 where
  spawn snapshot canonicalWorkingDirectory = do
    rollbackStatus <- newEmptyTMVarIO
    let executablePath = lengthSMTLibExecutionExecutablePath config
        arguments = lengthSMTLibExecutionConfiguredArgumentVector config
        specification = (proc executablePath arguments)
          { cwd = Just canonicalWorkingDirectory
          , env = Just []
          , std_in = CreatePipe
          , std_out = CreatePipe
          , std_err = CreatePipe
          , close_fds = True
          , create_group = True
          , delegate_ctlc = False
          , use_process_jobs = True
          }
        rollbackCreated attempted = case attempted of
          Left _ -> pure ()
          Right (input, output, errorOutput, handle) -> do
            pidResult <- tryIOError $ getPid handle
            let groupIdentifier = case pidResult of
                  Right (Just pid) -> Just $ toInteger pid
                  _ -> Nothing
            attemptedCleanup <- tryAny $ cleanupAcquired limits input output
              errorOutput handle groupIdentifier []
            let cleanup = case attemptedCleanup of
                  Right status -> status
                  Left _ -> incompleteCleanupStatus
            atomically $ void $ tryPutTMVar rollbackStatus cleanup
    controlledCreate <- runBeforeLengthSMTLibProcessDeadline
      cancellation deadline (tryIOError $ createProcess specification)
      rollbackCreated
    case controlledCreate of
      Left failure -> do
        cleanup <- atomically $ tryReadTMVar rollbackStatus
        pure $ Left $ case cleanup of
          Nothing -> failure
          Just status -> attachCleanup status failure
      Right created -> finishCreated snapshot canonicalWorkingDirectory created

  finishCreated snapshot canonicalWorkingDirectory created =
    case created of
      Left _ -> pure $ Left $ processError LengthSMTLibProcessSpawnPhase
        LengthSMTLibProcessSpawnFailed Nothing
      Right (input, output, errorOutput, handle) -> do
        pidResult <- tryIOError $ getPid handle
        let groupIdentifier = case pidResult of
              Right (Just pid) -> Just $ toInteger pid
              _ -> Nothing
        case (input, output, errorOutput) of
          (Just inputHandle, Just outputHandle, Just errorHandle) -> do
            allocated <- tryAny $ allocateProcess inputHandle outputHandle
              errorHandle handle groupIdentifier snapshot
              canonicalWorkingDirectory
            case allocated of
              Left _ -> do
                cleanup <- cleanupAcquired limits input output errorOutput handle
                  groupIdentifier []
                pure $ Left $ attachCleanup cleanup $ processError
                  LengthSMTLibProcessConfigurePhase
                  LengthSMTLibProcessInternalFailure Nothing
              Right process -> do
                let initialize = do
                      configured <- configureHandles process
                      case configured of
                        Left failure -> closeAfterOpenFailure process failure
                        Right () -> do
                          started <- startReaders process
                          case started of
                            Left failure -> closeAfterOpenFailure process failure
                            Right () -> do
                              ready <- checkLengthSMTLibProcessReady
                                process cancellation deadline
                              case ready of
                                Left failure ->
                                  closeAfterOpenFailure process failure
                                Right () -> pure $ Right process
                initialize `onException`
                  void (closeLengthSMTLibProcess process)
          _ -> do
            cleanup <- cleanupAcquired limits input output errorOutput handle
              groupIdentifier []
            pure $ Left $ attachCleanup cleanup $ processError
              LengthSMTLibProcessSpawnPhase
              LengthSMTLibProcessMissingPipe Nothing

  allocateProcess input output errorOutput handle groupIdentifier snapshot
      canonicalWorkingDirectory = do
    stdoutQueue <- newTQueueIO
    stdoutTerminal <- newTVarIO Nothing
    poison <- newTVarIO Nothing
    stdoutCount <- newTVarIO 0
    stderrCount <- newTVarIO 0
    lifecycle <- newTVarIO ProcessOpen
    writeToken <- newTMVarIO ()
    threads <- newTVarIO []
    closeState <- newMVar CloseNotStarted
    let pidField = case groupIdentifier of
          Nothing -> FingerprintTag (ascii "pid-unavailable") []
          Just pid -> FingerprintTag (ascii "pid-observed")
            [integerTextField pid]
        identity = processFingerprintField limits deadline snapshot
          canonicalWorkingDirectory pidField
    pure LengthSMTLibProcess
      { processInput = input
      , processOutput = output
      , processErrorOutput = errorOutput
      , processHandle = handle
      , processGroupIdentifier = groupIdentifier
      , processLimits = limits
      , processSnapshot = snapshot
      , processIdentityField = identity
      , processStdoutQueue = stdoutQueue
      , processStdoutTerminal = stdoutTerminal
      , processPoison = poison
      , processStdoutCount = stdoutCount
      , processStderrCount = stderrCount
      , processLifecycle = lifecycle
      , processWriteToken = writeToken
      , processThreads = threads
      , processCloseState = closeState
      }

snapshotExecutable
  :: LengthSMTLibProcessLimits
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> LengthSMTLibExecutionConfig
  -> FilePath
  -> IO
      (Either
        LengthSMTLibProcessError
        (LengthSMTLibExecutableSnapshot, FilePath))
snapshotExecutable limits cancellation deadline config workingDirectory = do
  cwdResult <- inspectWorkingDirectory cancellation deadline workingDirectory
  case cwdResult of
    Left failure -> pure $ Left failure
    Right canonicalWorkingDirectory -> do
      let executablePath = lengthSMTLibExecutionExecutablePath config
      canonicalBeforeResult <- tryIOError $ canonicalizePath executablePath
      case canonicalBeforeResult of
        Left _ -> pure $ Left $ processError LengthSMTLibProcessSnapshotPhase
          LengthSMTLibProcessExecutableUnavailable Nothing
        Right canonicalBefore -> do
          beforeResult <- captureMetadata executablePath
          case beforeResult of
            Left failure -> pure $ Left failure
            Right before -> do
              hashed <- hashExecutable limits cancellation deadline
                executablePath
              case hashed of
                Left failure -> pure $ Left failure
                Right (digest, count) -> do
                  afterResult <- captureMetadata executablePath
                  canonicalAfterResult <- tryIOError
                    $ canonicalizePath executablePath
                  case (afterResult, canonicalAfterResult) of
                    (Right after, Right canonicalAfter)
                      | canonicalBefore == canonicalAfter && before == after ->
                          finishSnapshot canonicalWorkingDirectory
                            canonicalBefore before digest count
                      | otherwise -> pure $ Left $ processError
                          LengthSMTLibProcessSnapshotPhase
                          LengthSMTLibProcessExecutableMetadataChanged Nothing
                    _ -> pure $ Left $ processError
                      LengthSMTLibProcessSnapshotPhase
                      LengthSMTLibProcessExecutableUnavailable Nothing
 where
  finishSnapshot canonicalWorkingDirectory canonical metadata digest count =
    let expected = BS.pack <$> lengthSMTLibExecutionExpectedExecutableSHA256 config
    in case expected of
      Just pinned | pinned /= digest -> pure $ Left $ processError
        LengthSMTLibProcessSnapshotPhase
        LengthSMTLibProcessExecutableDigestMismatch Nothing
      _ -> do
        let field = executableSnapshotField config workingDirectory
              canonicalWorkingDirectory canonical metadata digest count expected
        pure $ Right
          ( LengthSMTLibExecutableSnapshot digest count field
          , canonicalWorkingDirectory
          )

inspectWorkingDirectory
  :: LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> FilePath
  -> IO (Either LengthSMTLibProcessError FilePath)
inspectWorkingDirectory cancellation deadline path
  | not $ isAbsolute path = pure $ Left $ processError
      LengthSMTLibProcessWorkingDirectoryPhase
      LengthSMTLibProcessWorkingDirectoryNotAbsolute Nothing
  | otherwise = do
      controlled <- checkCancellationDeadline
        LengthSMTLibProcessWorkingDirectoryPhase cancellation deadline
      case controlled of
        Left failure -> pure $ Left failure
        Right () -> do
          inspected <- tryIOError $ do
            present <- doesDirectoryExist path
            empty <- if present
              then processWorkingDirectoryIsEmpty path
              else pure False
            canonical <- canonicalizePath path
            pure (present, empty, canonical)
          pure $ case inspected of
            Left _ -> Left $ processError
              LengthSMTLibProcessWorkingDirectoryPhase
              LengthSMTLibProcessWorkingDirectoryUnavailable Nothing
            Right (False, _, _) -> Left $ processError
              LengthSMTLibProcessWorkingDirectoryPhase
              LengthSMTLibProcessWorkingDirectoryUnavailable Nothing
            Right (True, False, _) -> Left $ processError
              LengthSMTLibProcessWorkingDirectoryPhase
              LengthSMTLibProcessWorkingDirectoryNotEmpty Nothing
            Right (True, True, canonical) -> Right canonical

processWorkingDirectoryIsEmpty :: FilePath -> IO Bool
#ifndef mingw32_HOST_OS
processWorkingDirectoryIsEmpty path = bracket
  (PosixDirectory.openDirStream path)
  PosixDirectory.closeDirStream
  $ \stream -> go stream (3 :: Int)
 where
  go _ 0 = pure False
  go stream remaining = do
    entry <- PosixDirectory.readDirStream stream
    if null entry
      then pure True
      else if entry == "." || entry == ".."
        then go stream $ remaining - 1
        else pure False
#else
processWorkingDirectoryIsEmpty path = null <$> listDirectory path
#endif

data CapturedMetadata = CapturedMetadata
  !PortableMetadata
#ifndef mingw32_HOST_OS
  !PosixMetadata
#endif
  deriving (Eq)

captureMetadata
  :: FilePath
  -> IO (Either LengthSMTLibProcessError CapturedMetadata)
captureMetadata path = do
  existence <- tryIOError $ (,) <$> doesPathExist path <*> doesFileExist path
  case existence of
    Left _ -> pure $ Left $ processError LengthSMTLibProcessSnapshotPhase
      LengthSMTLibProcessExecutableUnavailable Nothing
    Right (False, _) -> pure $ Left $ processError
      LengthSMTLibProcessSnapshotPhase
      LengthSMTLibProcessExecutableUnavailable Nothing
    Right (True, False) -> pure $ Left $ processError
      LengthSMTLibProcessSnapshotPhase
      LengthSMTLibProcessExecutableNotRegular Nothing
    Right (True, True) -> capture
 where
  capture = do
    captured <- tryIOError $ do
      size <- getFileSize path
      permissions <- getPermissions path
      modified <- getModificationTime path
      let portable = PortableMetadata
            (toInteger size)
            (readable permissions)
            (writable permissions)
            (executable permissions)
            (searchable permissions)
            (show modified)
#ifndef mingw32_HOST_OS
      status <- getFileStatus path
      let posix = PosixMetadata
            (toInteger $ deviceID status)
            (toInteger $ fileID status)
            (toInteger $ fileMode status)
            (toInteger $ fileSize status)
            (toInteger $ linkCount status)
            (show $ modificationTime status)
            (show $ statusChangeTime status)
      pure (portable, status, posix)
#else
      pure $ CapturedMetadata portable
#endif
    pure $ case captured of
      Left _ -> Left $ processError LengthSMTLibProcessSnapshotPhase
        LengthSMTLibProcessExecutableUnavailable Nothing
#ifndef mingw32_HOST_OS
      Right (_, status, _) | not $ isRegularFile status ->
        Left $ processError LengthSMTLibProcessSnapshotPhase
          LengthSMTLibProcessExecutableNotRegular Nothing
      Right (portable, _, posix) -> Right $ CapturedMetadata portable posix
#else
      Right metadata -> Right metadata
#endif

hashExecutable
  :: LengthSMTLibProcessLimits
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> FilePath
  -> IO (Either LengthSMTLibProcessError (ByteString, Natural))
hashExecutable (LengthSMTLibProcessLimits maximumBytes _ _ chunk _ _ _)
    cancellation deadline path = do
  attempted <- tryIOError $ withBinaryFile path ReadMode $ \handle -> do
    hSetBinaryMode handle True
    go handle SHA256.init 0
  pure $ case attempted of
    Left _ -> Left $ processError LengthSMTLibProcessSnapshotPhase
      LengthSMTLibProcessExecutableUnavailable Nothing
    Right result -> result
 where
  go handle !context !count = do
    controlled <- checkCancellationDeadline
      LengthSMTLibProcessSnapshotPhase cancellation deadline
    case controlled of
      Left failure -> pure $ Left failure
      Right () -> do
        let request = boundedReadSize chunk maximumBytes count
        bytes <- BS.hGetSome handle request
        if BS.null bytes
          then do
            let digest = SHA256.finalize context
            _ <- evaluate $ BS.length digest
            pure $ Right (digest, count)
          else do
            let observed = count + fromIntegral (BS.length bytes)
            if observed > maximumBytes
              then pure $ Left $ processError
                LengthSMTLibProcessSnapshotPhase
                LengthSMTLibProcessExecutableByteLimitExceeded
                (Just $ maximumBytes + 1)
              else go handle (SHA256.update context bytes) observed

configureHandles
  :: LengthSMTLibProcess
  -> IO (Either LengthSMTLibProcessError ())
configureHandles process = do
  configured <- tryIOError $ mapM_ configure
    [ processInput process
    , processOutput process
    , processErrorOutput process
    ]
  pure $ case configured of
    Left _ -> Left $ processError LengthSMTLibProcessConfigurePhase
      LengthSMTLibProcessHandleConfigurationFailed Nothing
    Right () -> Right ()
 where
  configure handle = do
    hSetBinaryMode handle True
    hSetBuffering handle NoBuffering

startReaders
  :: LengthSMTLibProcess
  -> IO (Either LengthSMTLibProcessError ())
startReaders process = do
  stdoutStarted <- startManagedThread process
    LengthSMTLibProcessConfigurePhase (pure Nothing) $ stdoutReader process
  case stdoutStarted of
    Left failure -> pure $ Left failure
    Right _ -> do
      stderrStarted <- startManagedThread process
        LengthSMTLibProcessConfigurePhase (pure Nothing) $ stderrReader process
      pure $ case stderrStarted of
        Left failure -> Left failure
        Right _ -> Right ()

stdoutReader :: LengthSMTLibProcess -> IO ()
stdoutReader process = loop
 where
  LengthSMTLibProcessLimits _ maximumBytes _ chunk _ _ _ =
    processLimits process
  loop = do
    lifecycle <- atomically $ readTVar $ processLifecycle process
    when (lifecycle == ProcessOpen) $ do
      count <- atomically $ readTVar $ processStdoutCount process
      received <- tryIOError $ BS.hGetSome (processOutput process)
        $ boundedReadSize chunk maximumBytes count
      case received of
        Left _ -> atomically $ setStdoutTerminalSTM process $ processError
          LengthSMTLibProcessStdoutPhase
          LengthSMTLibProcessStdoutReadFailed (Just count)
        Right bytes
          | BS.null bytes -> atomically $ setStdoutTerminalSTM process
              $ processError LengthSMTLibProcessStdoutPhase
                  LengthSMTLibProcessStdoutEOF (Just count)
          | otherwise -> do
              continue <- atomically $ do
                current <- readTVar $ processStdoutCount process
                let observed = current + fromIntegral (BS.length bytes)
                if observed > maximumBytes
                  then do
                    let remaining = maximumBytes - min current maximumBytes
                        permitted = BS.take (fromIntegral remaining) bytes
                    writeTVar (processStdoutCount process) $ maximumBytes + 1
                    when (not $ BS.null permitted) $ writeTQueue
                      (processStdoutQueue process) $ StdoutChunk permitted
                    setStdoutTerminalSTM process $ processError
                      LengthSMTLibProcessStdoutPhase
                      LengthSMTLibProcessStdoutByteLimitExceeded
                      (Just $ maximumBytes + 1)
                    pure False
                  else do
                    writeTVar (processStdoutCount process) observed
                    writeTQueue (processStdoutQueue process) $ StdoutChunk bytes
                    pure True
              when continue loop

stderrReader :: LengthSMTLibProcess -> IO ()
stderrReader process = loop
 where
  LengthSMTLibProcessLimits _ _ maximumBytes chunk _ _ _ =
    processLimits process
  loop = do
    count <- atomically $ readTVar $ processStderrCount process
    let request
          | count > maximumBytes = chunk
          | otherwise = boundedReadSize chunk maximumBytes count
    received <- tryIOError $ BS.hGetSome (processErrorOutput process) request
    case received of
      Left _ -> poisonIfOpen process $ processError
        LengthSMTLibProcessStderrPhase
        LengthSMTLibProcessStderrReadFailed (Just count)
      Right bytes
        | BS.null bytes -> poisonIfOpen process $ processError
            LengthSMTLibProcessStderrPhase
            LengthSMTLibProcessStderrEOF (Just count)
        | otherwise -> do
            atomically $ do
              current <- readTVar $ processStderrCount process
              let observed = min (maximumBytes + 1)
                    $ current + fromIntegral (BS.length bytes)
              writeTVar (processStderrCount process) observed
              setFirstPoisonSTM process $ processError
                LengthSMTLibProcessStderrPhase
                LengthSMTLibProcessStderrObserved
                (Just observed)
            -- Continue discarding even after lifecycle enters Closing.  The
            -- cleanup owner first closes stdin and then reaps or kills the
            -- child before it asks this reader to stop, so a finite flood
            -- cannot retain the child by filling its stderr pipe.
            loop

writeLengthSMTLibProcess
  :: LengthSMTLibProcess
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> ByteString
  -> IO (Either LengthSMTLibProcessError ())
writeLengthSMTLibProcess process cancellation deadline bytes =
  mask $ \restore -> do
    _ <- evaluate $ BS.length bytes
    token <- waitWriteControlled process cancellation deadline
      LengthSMTLibProcessWritePhase
      $ takeTMVar $ processWriteToken process
    case token of
      Left failure -> rejectWrite process failure
      Right () -> finally
        (do
          outcome <- newEmptyTMVarIO
          started <- startManagedThread process
            LengthSMTLibProcessWritePhase
            (readTVar $ processStdoutTerminal process) $ do
              written <- tryIOError $ do
                BS.hPut (processInput process) bytes
                hFlush $ processInput process
              atomically $ putTMVar outcome written
          case started of
            Left failure -> rejectWrite process failure
            Right managed -> do
              let stop = stopManagedThread managed
                  await = waitWriteControlled process cancellation deadline
                    LengthSMTLibProcessWritePhase $ readTMVar outcome
                  settle = waitWriteControlled process cancellation deadline
                    LengthSMTLibProcessWritePhase
                    $ managedThreadFinishedSTM managed
              result <- restore await `onException` do
                atomically $ setFirstPoisonSTM process $ processError
                  LengthSMTLibProcessWritePhase
                  LengthSMTLibProcessCancelled Nothing
                stop
              case result of
                Left failure -> stop >> rejectWrite process failure
                Right written -> do
                  settled <- settle
                  case settled of
                    Left failure -> stop >> rejectWrite process failure
                    Right () -> do
                      unregisterManagedThread process managed
                      case written of
                        Left _ -> poisonOperation process $ processError
                          LengthSMTLibProcessWritePhase
                          LengthSMTLibProcessWriteFailed Nothing
                        Right () -> pure $ Right ())
        (atomically $ putTMVar (processWriteToken process) ())

nextLengthSMTLibProcessStdoutChunk
  :: LengthSMTLibProcess
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> IO (Either LengthSMTLibProcessError ByteString)
nextLengthSMTLibProcessStdoutChunk process cancellation deadline = do
  result <- waitControlled process cancellation deadline
    LengthSMTLibProcessStdoutPhase
    $ readTQueue $ processStdoutQueue process
  case result of
    Left failure -> poisonOperation process failure
    Right event -> case event of
      StdoutChunk bytes -> pure $ Right bytes
      StdoutTerminal failure -> poisonOperation process failure

-- | Drain output which is causally attributable to the preceding protocol
-- write but arrived after that receiver completed.  Only SMT-LIB whitespace is
-- admitted.  A non-whitespace chunk is restored in its original FIFO position
-- before the process is poisoned; a queued terminal condition is propagated
-- at its exact position after any preceding whitespace.
drainLengthSMTLibProcessBoundaryWhitespace
  :: LengthSMTLibProcess
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> IO (Either LengthSMTLibProcessError ByteString)
drainLengthSMTLibProcessBoundaryWhitespace process cancellation deadline = do
  drained <- waitControlled process cancellation deadline
    LengthSMTLibProcessReadyPhase drainQueued
  case drained of
    Left failure -> poisonOperation process failure
    Right (Left failure) -> poisonOperation process failure
    Right (Right bytes) -> pure $ Right bytes
 where
  drainQueued = do
    events <- takeQueued []
    inspect events []

  takeQueued reversed = do
    empty <- isEmptyTQueue $ processStdoutQueue process
    if empty
      then pure $ reverse reversed
      else do
        event <- readTQueue $ processStdoutQueue process
        takeQueued $ event : reversed

  inspect [] reversedChunks =
    pure $ Right $ BS.concat $ reverse reversedChunks
  inspect events@(StdoutChunk bytes : remaining) reversedChunks
    | BS.all isSMTLibWhitespace bytes =
        inspect remaining $ bytes : reversedChunks
    | otherwise = do
        -- Restore all prior whitespace too; draining is all-or-nothing when a
        -- non-whitespace chunk is present.
        mapM_ (writeTQueue $ processStdoutQueue process)
          $ reverse (map StdoutChunk reversedChunks) ++ events
        pure $ Left $ processError LengthSMTLibProcessReadyPhase
          LengthSMTLibProcessUnexpectedPendingStdout Nothing
  inspect (StdoutTerminal failure : _) _ = pure $ Left failure

checkLengthSMTLibProcessReady
  :: LengthSMTLibProcess
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> IO (Either LengthSMTLibProcessError ())
checkLengthSMTLibProcessReady process cancellation deadline = do
  preflight <- checkReadySnapshot process cancellation deadline
  case preflight of
    Left failure -> poisonOperation process failure
    Right () -> do
      exited <- tryIOError $ getProcessExitCode $ processHandle process
      case exited of
        Left _ -> poisonOperation process $ processError
          LengthSMTLibProcessReadyPhase LengthSMTLibProcessExited Nothing
        Right (Just _) -> poisonOperation process $ processError
          LengthSMTLibProcessReadyPhase LengthSMTLibProcessExited Nothing
        Right Nothing -> do
          final <- checkReadySnapshot process cancellation deadline
          case final of
            Left failure -> poisonOperation process failure
            Right () -> pure $ Right ()

checkReadySnapshot
  :: LengthSMTLibProcess
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> IO (Either LengthSMTLibProcessError ())
checkReadySnapshot process cancellation deadline = do
  control <- checkCancellationDeadline
    LengthSMTLibProcessReadyPhase cancellation deadline
  case control of
    Left failure -> pure $ Left failure
    Right () -> atomically $ do
      poisoned <- readTVar $ processPoison process
      case poisoned of
        Just failure -> pure $ Left failure
        Nothing -> do
          empty <- isEmptyTQueue $ processStdoutQueue process
          lifecycle <- readTVar $ processLifecycle process
          if lifecycle /= ProcessOpen
            then pure $ Left $ processError LengthSMTLibProcessReadyPhase
              LengthSMTLibProcessClosed Nothing
            else if empty
              then pure $ Right ()
              else do
                pending <- peekTQueue $ processStdoutQueue process
                pure $ case pending of
                  StdoutTerminal failure -> Left failure
                  StdoutChunk _ -> Left $ processError
                    LengthSMTLibProcessReadyPhase
                    LengthSMTLibProcessUnexpectedPendingStdout Nothing

closeLengthSMTLibProcess
  :: LengthSMTLibProcess
  -> IO LengthSMTLibProcessCleanupStatus
closeLengthSMTLibProcess process = mask_ $ do
  gate <- modifyMVar (processCloseState process) $ \state -> case state of
    CloseNotStarted -> do
      result <- newEmptyMVar
      atomically $ do
        writeTVar (processLifecycle process) ProcessClosing
        setFirstPoisonSTM process $ processError
          LengthSMTLibProcessClosePhase LengthSMTLibProcessClosed Nothing
      _ <- forkIO $ do
        attempted <- tryAny $ cleanupProcess process
        let status = case attempted of
              Right completed -> completed
              Left _ -> LengthSMTLibProcessCleanupStatus
                { lengthSMTLibProcessCleanupEscalation =
                    LengthSMTLibProcessCleanupIncomplete
                , lengthSMTLibProcessCleanupExitCode = Nothing
                , lengthSMTLibProcessCleanupFailureCount = 1
                , lengthSMTLibProcessCleanupReadersStopped = False
                }
        atomically $ writeTVar (processLifecycle process) ProcessClosed
        modifyMVar_ (processCloseState process)
          $ const $ pure $ CloseFinished status
        putMVar result status
      pure (CloseRunning result, result)
    CloseRunning result -> pure (state, result)
    CloseFinished status -> do
      result <- newMVar status
      pure (state, result)
  readMVar gate

cleanupProcess :: LengthSMTLibProcess -> IO LengthSMTLibProcessCleanupStatus
cleanupProcess process = do
  threads <- atomically $ readTVar $ processThreads process
  status <- cleanupAcquired
    (processLimits process)
    (Just $ processInput process)
    (Just $ processOutput process)
    (Just $ processErrorOutput process)
    (processHandle process)
    (processGroupIdentifier process)
    threads
  atomically $ writeTVar (processLifecycle process) ProcessClosed
  pure status

cleanupAcquired
  :: LengthSMTLibProcessLimits
  -> Maybe Handle
  -> Maybe Handle
  -> Maybe Handle
  -> ProcessHandle
  -> Maybe Integer
  -> [ManagedThread]
  -> IO LengthSMTLibProcessCleanupStatus
cleanupAcquired limits input output errorOutput handle groupIdentifier threads =
  mask_ $ do
    inputClosed <- closeMaybe input
    graceful <- boundedWait handle gracefulMilliseconds
    (escalation, exitCode, signalFailures) <- case graceful of
      Just status -> pure
        (LengthSMTLibProcessClosedGracefully, Just status, 0)
      Nothing -> do
        terminateFailures <- terminateOwnedGroup handle groupIdentifier
        terminated <- boundedWait handle terminateMilliseconds
        case terminated of
          Just status -> pure
            (LengthSMTLibProcessTerminated, Just status, terminateFailures)
          Nothing -> do
            killFailures <- forceKill groupIdentifier
            killed <- boundedWait handle killMilliseconds
            pure $ case killed of
              Just status ->
                ( LengthSMTLibProcessKilled
                , Just status
                , terminateFailures + killFailures
                )
              Nothing ->
                ( LengthSMTLibProcessCleanupIncomplete
                , Nothing
                , terminateFailures + killFailures
                )
    mapM_ stopManagedThread threads
    readersStopped <- waitManagedThreads killMilliseconds threads
    (outputClosed, errorClosed) <- if readersStopped
      then (,) <$> closeMaybe output <*> closeMaybe errorOutput
      else pure (False, False)
    let closeFailures = boolFailure inputClosed + boolFailure outputClosed
          + boolFailure errorClosed
        totalFailures = signalFailures + closeFailures
          + if readersStopped then 0 else 1
        finalEscalation
          | exitCode == Nothing || not readersStopped =
              LengthSMTLibProcessCleanupIncomplete
          | otherwise = escalation
    pure LengthSMTLibProcessCleanupStatus
      { lengthSMTLibProcessCleanupEscalation = finalEscalation
      , lengthSMTLibProcessCleanupExitCode = exitCode
      , lengthSMTLibProcessCleanupFailureCount = totalFailures
      , lengthSMTLibProcessCleanupReadersStopped = readersStopped
      }
 where
  LengthSMTLibProcessLimits _ _ _ _ gracefulMilliseconds
      terminateMilliseconds killMilliseconds = limits
  closeMaybe Nothing = pure True
  closeMaybe (Just stream) = boundedClose killMilliseconds stream
  boolFailure True = 0
  boolFailure False = 1

-- Polling avoids 'waitForProcess': its blocking @waitpid@ can hold the sole RTS
-- capability in a non-threaded embedding, preventing the timeout/escalation
-- owner itself from running.  'getProcessExitCode' uses a nonblocking status
-- observation and reaps/caches the child when it has exited.
boundedWait :: ProcessHandle -> Int -> IO (Maybe ExitCode)
boundedWait handle milliseconds = do
  started <- getMonotonicTimeNSec
  let deadline = toInteger started + toInteger milliseconds * 1000000
  go deadline
 where
  go deadline = do
    observed <- tryIOError $ getProcessExitCode handle
    case observed of
      Left _ -> pure Nothing
      Right (Just status) -> pure $ Just status
      Right Nothing -> do
        now <- getMonotonicTimeNSec
        if toInteger now >= deadline
          then pure Nothing
          else do
            let remainingMicroseconds =
                  (deadline - toInteger now + 999) `div` 1000
                pause = fromInteger $ max 1 $ min 5000 remainingMicroseconds
            threadDelay pause
            go deadline

-- Never let a lock-contended 'hClose' hold the cleanup owner indefinitely.
-- A timed-out helper is interrupted best-effort and the incomplete close is
-- reflected in cleanup status; the owner does not wait unboundedly to join an
-- uninterruptible handle operation.
boundedClose :: Int -> Handle -> IO Bool
boundedClose milliseconds stream = mask $ \restore -> do
  result <- newEmptyTMVarIO
  thread <- forkIOWithUnmask $ \unmask -> do
    attempted <- tryAny $ unmask $ hClose stream
    atomically $ void $ tryPutTMVar result attempted
  observed <- restore $ timeout (milliseconds * 1000)
    $ atomically $ readTMVar result
  case observed of
    Just (Right ()) -> pure True
    Just (Left _) -> pure False
    Nothing -> do
      _ <- forkIO $ killThread thread
      pure False

terminateOwnedGroup :: ProcessHandle -> Maybe Integer -> IO Natural
#ifndef mingw32_HOST_OS
terminateOwnedGroup handle Nothing = do
  terminated <- tryIOError $ terminateProcess handle
  -- The additional failure records that descendant cleanup was unavailable.
  pure $ 1 + unitFailure terminated
terminateOwnedGroup handle (Just identifier) = do
  groupTerminated <- tryIOError $ signalProcessGroup sigTERM
    $ fromInteger identifier
  case groupTerminated of
    Right () -> pure 0
    Left _ -> do
      -- The ProcessHandle remains a safe leader fallback; do not retry a stale
      -- numeric group identifier after group addressing failed.
      terminated <- tryIOError $ terminateProcess handle
      pure $ 1 + unitFailure terminated
#else
terminateOwnedGroup handle _ = do
  interrupted <- tryIOError $ interruptProcessGroupOf handle
  terminated <- tryIOError $ terminateProcess handle
  pure $ unitFailure interrupted + unitFailure terminated
#endif

forceKill :: Maybe Integer -> IO Natural
#ifndef mingw32_HOST_OS
forceKill Nothing = pure 1
forceKill (Just identifier) = do
  killed <- tryIOError $ signalProcessGroup sigKILL $ fromInteger identifier
  pure $ unitFailure killed
#else
forceKill _ = pure 0
#endif

unitFailure :: Either failure () -> Natural
unitFailure (Left _) = 1
unitFailure (Right ()) = 0

waitManagedThreads :: Int -> [ManagedThread] -> IO Bool
waitManagedThreads milliseconds threads = do
  waited <- timeout (milliseconds * 1000)
    $ atomically $ mapM_ managedThreadFinishedSTM threads
  pure $ case waited of
    Nothing -> False
    Just () -> True

startManagedThread
  :: LengthSMTLibProcess
  -> LengthSMTLibProcessPhase
  -> STM (Maybe LengthSMTLibProcessError)
  -> IO ()
  -> IO (Either LengthSMTLibProcessError ManagedThread)
startManagedThread process phase extraFailure action = mask_ $ do
  start <- newEmptyMVar
  done <- newEmptyTMVarIO
  thread <- forkIOWithUnmask $ \unmask ->
    (do
      shouldRun <- takeMVar start
      when shouldRun $ unmask action)
    `finally` atomically (putTMVar done ())
  let managed = ManagedThread thread done
  admitted <- atomically $ do
    lifecycle <- readTVar $ processLifecycle process
    poisoned <- readTVar $ processPoison process
    extra <- extraFailure
    case (lifecycle, poisoned, extra) of
      (ProcessOpen, Nothing, Nothing) -> do
        modifyTVar' (processThreads process) (managed :)
        pure $ Right ()
      (_, Just failure, _) -> pure $ Left failure
      (_, _, Just failure) -> pure $ Left failure
      _ -> pure $ Left $ processError phase LengthSMTLibProcessClosed Nothing
  case admitted of
    Right () -> do
      putMVar start True
      pure $ Right managed
    Left failure -> do
      putMVar start False
      atomically $ managedThreadFinishedSTM managed
      pure $ Left failure

stopManagedThread :: ManagedThread -> IO ()
stopManagedThread (ManagedThread thread _) = void $ forkIO $ killThread thread

managedThreadFinishedSTM :: ManagedThread -> STM ()
managedThreadFinishedSTM (ManagedThread _ done) = readTMVar done

unregisterManagedThread :: LengthSMTLibProcess -> ManagedThread -> IO ()
unregisterManagedThread process target = atomically $ modifyTVar'
  (processThreads process) $ filter $ not . sameManagedThread target

sameManagedThread :: ManagedThread -> ManagedThread -> Bool
sameManagedThread (ManagedThread left _) (ManagedThread right _) = left == right

waitBeforeDeadline
  :: LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> TMVar (Either SomeException value)
  -> IO
      (Either
        LengthSMTLibProcessError
        (Either SomeException value))
waitBeforeDeadline cancellation deadline outcome = go
 where
  go = do
    control <- checkCancellationDeadline
      LengthSMTLibProcessDeadlinePhase cancellation deadline
    case control of
      Left failure -> pure $ Left failure
      Right () -> do
        remaining <- remainingDeadlineMicroseconds deadline
        case remaining of
          Nothing -> pure $ Left $ processError
            LengthSMTLibProcessDeadlinePhase
            LengthSMTLibProcessDeadlineExceeded Nothing
          Just microseconds -> do
            observed <- timeout microseconds $ atomically $
              cancellationSTM cancellation LengthSMTLibProcessDeadlinePhase
              `orElse` (Right <$> readTMVar outcome)
            case observed of
              Nothing -> go
              Just value -> do
                finalControl <- checkCancellationDeadline
                  LengthSMTLibProcessDeadlinePhase cancellation deadline
                pure $ case finalControl of
                  Left failure -> Left failure
                  Right () -> value

waitControlled
  :: LengthSMTLibProcess
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> LengthSMTLibProcessPhase
  -> STM value
  -> IO (Either LengthSMTLibProcessError value)
waitControlled process cancellation deadline phase action = go
 where
  go = do
    control <- checkCancellationDeadline phase cancellation deadline
    case control of
      Left failure -> pure $ Left failure
      Right () -> do
        remaining <- remainingDeadlineMicroseconds deadline
        case remaining of
          Nothing -> pure $ Left $ processError phase
            LengthSMTLibProcessDeadlineExceeded Nothing
          Just microseconds -> do
            observed <- timeout microseconds $ atomically $
              cancellationSTM cancellation phase
              `orElse` poisonSTM process
              `orElse` lifecycleSTM process phase
              `orElse` (Right <$> action)
            case observed of
              Nothing -> go
              Just value -> do
                finalControl <- checkCancellationDeadline
                  phase cancellation deadline
                case finalControl of
                  Left failure -> pure $ Left failure
                  Right () -> do
                    poisoned <- atomically $ readTVar $ processPoison process
                    pure $ case poisoned of
                      Just failure -> Left failure
                      Nothing -> value

-- A stdout terminal condition participates in write admission without becoming
-- global poison.  Thus a write cannot start after the reader records terminal,
-- while 'nextLengthSMTLibProcessStdoutChunk' can still deliver chunks which
-- precede that terminal in the stdout FIFO.
waitWriteControlled
  :: LengthSMTLibProcess
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> LengthSMTLibProcessPhase
  -> STM value
  -> IO (Either LengthSMTLibProcessError value)
waitWriteControlled process cancellation deadline phase action = do
  result <- waitControlled process cancellation deadline phase $ do
    terminal <- readTVar $ processStdoutTerminal process
    case terminal of
      Just failure -> pure $ Left failure
      Nothing -> Right <$> action
  pure $ result >>= id

cancellationSTM
  :: LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessPhase
  -> STM (Either LengthSMTLibProcessError value)
cancellationSTM (LengthSMTLibProcessCancellation cancelled) phase = do
  value <- readTVar cancelled
  if value
    then pure $ Left $ processError phase LengthSMTLibProcessCancelled Nothing
    else retry

poisonSTM
  :: LengthSMTLibProcess
  -> STM (Either LengthSMTLibProcessError value)
poisonSTM process = do
  poisoned <- readTVar $ processPoison process
  case poisoned of
    Nothing -> retry
    Just failure -> pure $ Left failure

lifecycleSTM
  :: LengthSMTLibProcess
  -> LengthSMTLibProcessPhase
  -> STM (Either LengthSMTLibProcessError value)
lifecycleSTM process phase = do
  lifecycle <- readTVar $ processLifecycle process
  if lifecycle == ProcessOpen
    then retry
    else pure $ Left $ processError phase LengthSMTLibProcessClosed Nothing

checkCancellationDeadline
  :: LengthSMTLibProcessPhase
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> IO (Either LengthSMTLibProcessError ())
checkCancellationDeadline phase
    (LengthSMTLibProcessCancellation cancelled)
    (LengthSMTLibProcessDeadline deadline) = do
  isCancelled <- atomically $ readTVar cancelled
  if isCancelled
    then pure $ Left $ processError phase LengthSMTLibProcessCancelled Nothing
    else do
      now <- getMonotonicTimeNSec
      pure $ if now >= deadline
        then Left $ processError phase LengthSMTLibProcessDeadlineExceeded Nothing
        else Right ()

remainingDeadlineMicroseconds
  :: LengthSMTLibProcessDeadline
  -> IO (Maybe Int)
remainingDeadlineMicroseconds (LengthSMTLibProcessDeadline deadline) = do
  now <- getMonotonicTimeNSec
  pure $ if now >= deadline
    then Nothing
    else Just $ fromIntegral $ min (toInteger (maxBound :: Int))
      $ (toInteger deadline - toInteger now + 999) `div` 1000

poisonOperation
  :: LengthSMTLibProcess
  -> LengthSMTLibProcessError
  -> IO (Either LengthSMTLibProcessError value)
poisonOperation process failure = do
  atomically $ setFirstPoisonSTM process failure
  pure $ Left failure

rejectWrite
  :: LengthSMTLibProcess
  -> LengthSMTLibProcessError
  -> IO (Either LengthSMTLibProcessError value)
rejectWrite process failure = do
  terminal <- atomically $ readTVar $ processStdoutTerminal process
  case terminal of
    Just known | known == failure -> pure $ Left failure
    _ -> poisonOperation process failure

poisonIfOpen :: LengthSMTLibProcess -> LengthSMTLibProcessError -> IO ()
poisonIfOpen process failure = atomically $ do
  lifecycle <- readTVar $ processLifecycle process
  when (lifecycle == ProcessOpen) $ setFirstPoisonSTM process failure

setFirstPoisonSTM
  :: LengthSMTLibProcess
  -> LengthSMTLibProcessError
  -> STM ()
setFirstPoisonSTM process failure = do
  current <- readTVar $ processPoison process
  case current of
    Nothing -> writeTVar (processPoison process) $ Just failure
    Just _ -> pure ()

setStdoutTerminalSTM
  :: LengthSMTLibProcess
  -> LengthSMTLibProcessError
  -> STM ()
setStdoutTerminalSTM process failure = do
  current <- readTVar $ processStdoutTerminal process
  case current of
    Just _ -> pure ()
    Nothing -> do
      writeTVar (processStdoutTerminal process) $ Just failure
      writeTQueue (processStdoutQueue process) $ StdoutTerminal failure

closeAfterOpenFailure
  :: LengthSMTLibProcess
  -> LengthSMTLibProcessError
  -> IO (Either LengthSMTLibProcessError LengthSMTLibProcess)
closeAfterOpenFailure process failure = do
  atomically $ setFirstPoisonSTM process failure
  cleanup <- closeLengthSMTLibProcess process
  pure $ Left $ attachCleanup cleanup failure

processError
  :: LengthSMTLibProcessPhase
  -> LengthSMTLibProcessFailureClass
  -> Maybe Natural
  -> LengthSMTLibProcessError
processError phase failure observed = LengthSMTLibProcessError
  { lengthSMTLibProcessErrorPhase = phase
  , lengthSMTLibProcessErrorClass = failure
  , lengthSMTLibProcessErrorObservedAtLeast = observed
  , lengthSMTLibProcessErrorCleanupStatus = Nothing
  }

attachCleanup
  :: LengthSMTLibProcessCleanupStatus
  -> LengthSMTLibProcessError
  -> LengthSMTLibProcessError
attachCleanup cleanup failure = failure
  { lengthSMTLibProcessErrorCleanupStatus = Just cleanup }

incompleteCleanupStatus :: LengthSMTLibProcessCleanupStatus
incompleteCleanupStatus = LengthSMTLibProcessCleanupStatus
  { lengthSMTLibProcessCleanupEscalation =
      LengthSMTLibProcessCleanupIncomplete
  , lengthSMTLibProcessCleanupExitCode = Nothing
  , lengthSMTLibProcessCleanupFailureCount = 1
  , lengthSMTLibProcessCleanupReadersStopped = False
  }

boundedReadSize :: Int -> Natural -> Natural -> Int
boundedReadSize chunk maximumBytes observed = fromIntegral $ max 1
  $ min (fromIntegral chunk) $ if observed >= maximumBytes
      then 1
      else maximumBytes - observed + 1

executableSnapshotField
  :: LengthSMTLibExecutionConfig
  -> FilePath
  -> FilePath
  -> FilePath
  -> CapturedMetadata
  -> ByteString
  -> Natural
  -> Maybe ByteString
  -> FingerprintField
executableSnapshotField config requestedCwd canonicalCwd canonicalExecutable
    metadata digest count expected = FingerprintTag
  (ascii "pre-spawn-path-executable-snapshot")
  [ FingerprintBytes $ BS.unpack lengthSMTLibExecutableSnapshotStrengthTag
  , FingerprintBytes $ fingerprintCanonicalBytes
      $ lengthSMTLibExecutionPolicyFingerprint config
  , FingerprintTag (ascii "requested-executable-path")
      [textField $ lengthSMTLibExecutionExecutablePath config]
  , FingerprintTag (ascii "canonical-executable-path")
      [textField canonicalExecutable]
  , metadataField metadata
  , FingerprintTag (ascii "sha256") [FingerprintBytes $ BS.unpack digest]
  , FingerprintNatural count
  , case expected of
      Nothing -> FingerprintTag (ascii "snapshot-pin-absent") []
      Just pinned -> FingerprintTag (ascii "snapshot-pin-matched")
        [FingerprintBytes $ BS.unpack pinned]
  , FingerprintTag (ascii "spawn-path-original-request")
      [textField $ lengthSMTLibExecutionExecutablePath config]
  , FingerprintTag (ascii "spawn-argv")
      [FingerprintSequence $ map textField
        $ lengthSMTLibExecutionConfiguredArgumentVector config]
  , FingerprintTag (ascii "spawn-empty-environment") []
  , FingerprintTag (ascii "requested-working-directory")
      [textField requestedCwd]
  , FingerprintTag (ascii "spawn-working-directory-exact-canonical-path")
      [textField canonicalCwd]
  , FingerprintTag (ascii "spawn-flags") $ map (FingerprintBytes . ascii)
      [ "three-create-pipes"
      , "close-fds"
      , "create-group"
      , "no-delegated-ctlc"
      , "process-jobs"
      ]
  ]

metadataField :: CapturedMetadata -> FingerprintField
metadataField (CapturedMetadata portable
#ifndef mingw32_HOST_OS
    posix
#endif
    ) = FingerprintTag (ascii "before-after-consistent-metadata")
  [portableMetadataField portable
#ifndef mingw32_HOST_OS
  , posixMetadataField posix
#endif
  ]

portableMetadataField :: PortableMetadata -> FingerprintField
portableMetadataField
    (PortableMetadata size canRead canWrite canExecute canSearch modified) =
  FingerprintTag (ascii "portable-metadata")
    [ integerTextField size
    , booleanField canRead
    , booleanField canWrite
    , booleanField canExecute
    , booleanField canSearch
    , textField modified
    ]

#ifndef mingw32_HOST_OS
posixMetadataField :: PosixMetadata -> FingerprintField
posixMetadataField (PosixMetadata device inode mode size links modified changed) =
  FingerprintTag (ascii "posix-regular-file-metadata")
    $ map integerTextField [device, inode, mode, size, links]
      ++ [textField modified, textField changed]
#endif

processFingerprintField
  :: LengthSMTLibProcessLimits
  -> LengthSMTLibProcessDeadline
  -> LengthSMTLibExecutableSnapshot
  -> FilePath
  -> FingerprintField
  -> FingerprintField
processFingerprintField limits
    (LengthSMTLibProcessDeadline deadline)
    snapshot canonicalWorkingDirectory pidField =
  FingerprintTag (ascii "length-z3-launched-transport")
    [ FingerprintBytes $ BS.unpack lengthSMTLibProcessSchemaTag
    , lengthSMTLibExecutableSnapshotFingerprintField snapshot
    , FingerprintTag (ascii "canonical-working-directory-observation")
        [textField canonicalWorkingDirectory]
    , workingDirectoryEmptinessObservationField
    , pidField
    , FingerprintTag (ascii "alive-at-open-snapshot") []
    , FingerprintTag (ascii "separate-binary-pipes") []
    , FingerprintTag (ascii "stderr-first-byte-poisons-session") []
    , FingerprintTag
        (ascii "stderr-post-poison-discard-count-capped-at-max-plus-one/v1") []
    , FingerprintTag
        (ascii "stdout-cumulative-charge-before-fifo-enqueue/v1") []
    , FingerprintTag (ascii "stdout-chunks-before-terminal-fifo/v1") []
    , FingerprintTag (ascii "writes-exact-bytes-then-flush/v1") []
    , FingerprintTag
        (ascii "control-priority-cancel-deadline-poison-output/v1") []
    , FingerprintTag
        (ascii "cleanup-leader-close-wait-term-wait-kill-wait/v1") []
    , FingerprintTag
        (ascii "descendant-cleanup-best-effort-no-post-reap-group-signal/v1")
        []
    , FingerprintTag (ascii "absolute-monotonic-deadline-nanoseconds")
        [FingerprintNatural $ fromIntegral deadline]
    , lengthSMTLibProcessLimitsFingerprintField limits
    ]

workingDirectoryEmptinessObservationField :: FingerprintField
#ifndef mingw32_HOST_OS
workingDirectoryEmptinessObservationField = FingerprintTag
  (ascii "working-directory-observed-empty-posix-dir-stream-at-most-three-reads/v1")
  []
#else
workingDirectoryEmptinessObservationField = FingerprintTag
  (ascii "working-directory-observed-empty-windows-list-fallback-no-read-bound/v1")
  []
#endif

textField :: String -> FingerprintField
textField = FingerprintSequence
  . map (FingerprintNatural . fromIntegral . ord)

integerTextField :: Integer -> FingerprintField
integerTextField = textField . show

booleanField :: Bool -> FingerprintField
booleanField value = FingerprintBytes $ ascii $ if value then "true" else "false"

asciiBytes :: String -> ByteString
asciiBytes = BS.pack . ascii

ascii :: String -> [Word8]
ascii = map $ fromIntegral . ord

isSMTLibWhitespace :: Word8 -> Bool
isSMTLibWhitespace byte =
  byte == 9 || byte == 10 || byte == 13 || byte == 32

tryAny :: IO value -> IO (Either SomeException value)
tryAny = try
