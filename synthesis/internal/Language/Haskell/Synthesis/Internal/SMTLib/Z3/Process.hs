{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InterruptibleFFI #-}

-- | Private ownership of one raw Z3 subprocess.
--
-- Launch admission is supplied by the domain-neutral opaque Z3 execution
-- profile.  This layer retains only launch and observed-process facts.  It
-- deliberately owns no semantic-domain schema tag, fingerprint root, or
-- fingerprint budget; a domain facade seals the associated observation and
-- process-limit fields into its own identity.
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
-- configured process stdout limit rather than consumer speed. Every queued
-- stdout chunk carries an opaque proof that it is nonempty, so a successful
-- generic read cannot report zero progress. After stderr poison, strict chunks
-- are discarded to keep finite floods from retaining the child; its retained
-- count saturates at the configured maximum plus one.
--
-- Cleanup owns and reaps the direct child.  Process-group signalling is
-- best-effort only while that leader has not been observed reaped.  This
-- portable implementation cannot census descendants or prove ownership of a
-- numeric process-group identifier after leader exit, so it makes no claim
-- that detached or surviving descendants have been killed.
module Language.Haskell.Synthesis.Internal.SMTLib.Z3.Process
  ( z3SMTLibExecutableSnapshotStrengthTag
  , z3SMTLibDescriptorBoundExecutableLaunchStrengthTag
  , z3SMTLibDescriptorBoundExecutableLaunchSupported
  , z3SMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchStrengthTag
  , z3SMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchSupported
  , Z3SMTLibProcessLimitSource (..)
  , defaultZ3SMTLibProcessLimitSource
  , Z3SMTLibProcessLimits
  , mkZ3SMTLibProcessLimits
  , z3SMTLibProcessExecutableByteLimit
  , z3SMTLibProcessStdoutByteLimit
  , z3SMTLibProcessStderrByteLimit
  , z3SMTLibProcessReadChunkByteLimit
  , z3SMTLibProcessGracefulCloseMilliseconds
  , z3SMTLibProcessTerminateMilliseconds
  , z3SMTLibProcessKillMilliseconds
  , z3SMTLibProcessLimitFingerprintFields
  , Z3SMTLibProcessPhase (..)
  , Z3SMTLibProcessFailureClass (..)
  , Z3SMTLibProcessError (..)
  , Z3SMTLibProcessCleanupEscalation (..)
  , Z3SMTLibProcessCleanupStatus (..)
  , Z3SMTLibProcessDeadline
  , mkZ3SMTLibProcessDeadline
  , z3SMTLibProcessDeadlineAfterMilliseconds
  , compareZ3SMTLibProcessDeadline
  , minimumZ3SMTLibProcessDeadline
  , checkZ3SMTLibProcessDeadline
  , z3SMTLibProcessMonotonicTimeNanoseconds
  , z3SMTLibProcessDeadlineFingerprintField
  , Z3SMTLibProcessCancellation
  , newZ3SMTLibProcessCancellation
  , cancelZ3SMTLibProcess
  , runBeforeZ3SMTLibProcessDeadline
  , waitZ3SMTLibProcessControl
  , Z3SMTLibExecutableSnapshot
  , z3SMTLibExecutableSnapshotSHA256
  , z3SMTLibExecutableSnapshotByteCount
  , z3SMTLibExecutableSnapshotFingerprintField
  , Z3SMTLibProcess
  , openZ3SMTLibProcess
  , Z3SMTLibWorkingDirectoryDescriptor
  , mkZ3SMTLibWorkingDirectoryDescriptor
  , openZ3SMTLibDescriptorBoundProcess
  , openZ3SMTLibDescriptorBoundProcessWithPreExecHook
  , z3SMTLibProcessUsesDescriptorBoundExecutableLaunch
  , Z3SMTLibEffectiveIDExecutableAccessCheckResult (..)
  , openZ3SMTLibDescriptorBoundEffectiveIDExecutableAccessProcess
  , openZ3SMTLibDescriptorBoundEffectiveIDExecutableAccessProcessWithHooks
  , z3SMTLibProcessUsesDescriptorBoundEffectiveIDExecutableAccessLaunch
  , z3SMTLibProcessSnapshot
  , z3SMTLibProcessObservationFingerprintFields
  , z3SMTLibProcessLimits
  , z3SMTLibProcessObservedStdoutBytes
  , z3SMTLibProcessObservedStderrBytes
  , writeZ3SMTLibProcess
  , nextZ3SMTLibProcessStdoutChunk
  , drainZ3SMTLibProcessBoundaryWhitespace
  , checkZ3SMTLibProcessReady
  , closeZ3SMTLibProcess
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
  , withMVar
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
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Int (Int64)
import Data.Word (Word8, Word64)
import Foreign.C.Types (CInt (..), CUInt (..))
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
  , SeekMode (AbsoluteSeek)
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
  ( FileStatus
  , deviceID
  , fileID
  , fileMode
  , fileSize
  , getFileStatus
  , getFdStatus
  , isDirectory
  , isRegularFile
  , linkCount
  , modificationTime
  , statusChangeTime
  )
import System.Posix.Signals
  ( sigKILL
  , sigTERM
  , signalProcess
  , signalProcessGroup
  )
#endif

#ifdef DJEX_HAVE_DESCRIPTOR_BOUND_Z3_LAUNCH
import Foreign.C.String (CString, withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray0)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import System.Posix.Internals (withFilePath)
import System.Posix.IO (closeFd, dup, fdSeek, fdToHandle)
import System.Posix.Types (CPid, Fd (..))
import qualified System.Process.Internals as ProcessInternals
#endif

import Language.Haskell.Synthesis.Internal.Fingerprint
  ( FingerprintField (..) )
import Language.Haskell.Synthesis.Internal.SMTLib.Causal.BoundaryWhitespace
  ( SMTLibCausalBoundaryWhitespace
  , admitSMTLibCausalBoundaryWhitespace
  , concatSMTLibCausalBoundaryWhitespace
  )
import Language.Haskell.Synthesis.Internal.SMTLib.Causal.StdoutChunk
  ( SMTLibCausalStdoutChunk
  , admitSMTLibCausalStdoutChunk
  , smtLibCausalStdoutChunkBytes
  )
import qualified Language.Haskell.Synthesis.Internal.SMTLib.Z3.Execution
  as Z3

-- | Additive descriptor-bound launch which also observes effective-ID source
-- VFS execute access before copying and again after staging, immediately
-- before child allocation.  These observations are point-in-time only.
openZ3SMTLibDescriptorBoundEffectiveIDExecutableAccessProcess
  :: Z3SMTLibProcessLimits
  -> Z3SMTLibProcessCancellation
  -> Z3SMTLibProcessDeadline
  -> Z3.Z3SMTLibExecutionProfile
  -> FilePath
  -> Z3SMTLibWorkingDirectoryDescriptor
  -> IO (Either Z3SMTLibProcessError Z3SMTLibProcess)
openZ3SMTLibDescriptorBoundEffectiveIDExecutableAccessProcess
    limits cancellation deadline profile workingDirectory
    workingDirectoryDescriptor =
#ifdef DJEX_HAVE_DESCRIPTOR_BOUND_Z3_LAUNCH
  openZ3SMTLibDescriptorBoundEffectiveIDExecutableAccessProcessWithHooks
    limits cancellation deadline profile workingDirectory
    workingDirectoryDescriptor nativeEffectiveIDExecutableAccessCheck $ pure ()
#else
  openZ3SMTLibDescriptorBoundEffectiveIDExecutableAccessProcessWithHooks
    limits cancellation deadline profile workingDirectory
    workingDirectoryDescriptor
    (const $ pure Z3SMTLibEffectiveIDExecutableAccessCheckUnavailable)
    $ pure ()
#endif

-- | Deterministic package-private seam.  The closed checker is invoked on the
-- borrowed numeric source descriptor exactly twice along a successful prefix;
-- it must neither retain nor close that descriptor.  The hook runs after pin
-- and sealed-image admission but before the second check or any fork/pipe/
-- child allocation.
openZ3SMTLibDescriptorBoundEffectiveIDExecutableAccessProcessWithHooks
  :: Z3SMTLibProcessLimits
  -> Z3SMTLibProcessCancellation
  -> Z3SMTLibProcessDeadline
  -> Z3.Z3SMTLibExecutionProfile
  -> FilePath
  -> Z3SMTLibWorkingDirectoryDescriptor
  -> (CInt -> IO Z3SMTLibEffectiveIDExecutableAccessCheckResult)
  -> IO ()
  -> IO (Either Z3SMTLibProcessError Z3SMTLibProcess)
#ifndef DJEX_HAVE_DESCRIPTOR_BOUND_Z3_LAUNCH
openZ3SMTLibDescriptorBoundEffectiveIDExecutableAccessProcessWithHooks
    _ _ _ _ _ _ _ _ =
  pure $ Left $ processError Z3SMTLibProcessSnapshotPhase
    Z3SMTLibProcessEffectiveIDExecutableAccessCheckUnavailable Nothing
#else
openZ3SMTLibDescriptorBoundEffectiveIDExecutableAccessProcessWithHooks
    limits cancellation deadline profile workingDirectory
    workingDirectoryDescriptor accessCheck preFinalAccessHook =
  mask $ \restore -> do
    initial <- checkCancellationDeadline
      Z3SMTLibProcessSnapshotPhase cancellation deadline
    case initial of
      Left failure -> pure $ Left failure
      Right () -> do
        admittedWorkingDirectory <- restore $
          inspectDescriptorWorkingDirectory cancellation deadline
            workingDirectory workingDirectoryFd
        case admittedWorkingDirectory of
          Left failure -> pure $ Left failure
          Right (canonicalWorkingDirectory, workingDirectoryMetadata) -> do
            controlledOpen <- restore $ runBeforeZ3SMTLibProcessDeadline
              cancellation deadline openSourceDescriptor
              rollbackOpenedDescriptor
            case controlledOpen of
              Left failure -> pure $ Left failure
              Right (Left failure) -> pure $ Left failure
              Right (Right sourceDescriptor) ->
                restore
                  (withSourceDescriptor sourceDescriptor
                    canonicalWorkingDirectory workingDirectoryMetadata)
                  `finally` closeDescriptorIgnoringFailure sourceDescriptor
 where
  Z3SMTLibWorkingDirectoryDescriptor workingDirectoryRaw =
    workingDirectoryDescriptor
  workingDirectoryFd = Fd workingDirectoryRaw

  openSourceDescriptor = do
    opened <- tryAny $ withFilePath
      (Z3.z3SMTLibExecutionExecutablePath profile) c_openExecutableDescriptor
    pure $ case opened of
      Right raw | raw >= 0 -> Right $ Fd raw
      _ -> Left $ processError Z3SMTLibProcessSnapshotPhase
        Z3SMTLibProcessExecutableUnavailable Nothing

  rollbackOpenedDescriptor (Left _) = pure ()
  rollbackOpenedDescriptor (Right descriptor) =
    closeDescriptorIgnoringFailure descriptor

  withSourceDescriptor sourceDescriptor canonicalWorkingDirectory
      workingDirectoryMetadata = do
    beforeResult <- captureDescriptorMetadata sourceDescriptor
    case beforeResult of
      Left failure -> pure $ Left failure
      Right sourceMetadata
        | not $ descriptorMetadataIsRegular sourceMetadata ->
            pure $ Left $ processError Z3SMTLibProcessSnapshotPhase
              Z3SMTLibProcessExecutableNotRegular Nothing
        | not $ descriptorMetadataHasExecuteBit sourceMetadata ->
            pure $ Left $ processError Z3SMTLibProcessSnapshotPhase
              Z3SMTLibProcessExecutableNotExecutable Nothing
        | otherwise -> do
            admitted <- observeEffectiveIDAccess sourceDescriptor
            case admitted of
              Left failure -> pure $ Left failure
              Right () -> createAndSealImage sourceDescriptor sourceMetadata
                canonicalWorkingDirectory workingDirectoryMetadata

  observeEffectiveIDAccess sourceDescriptor = do
    controlled <- runBeforeZ3SMTLibProcessDeadline cancellation deadline
      (tryAny $ accessCheck $ fdToCInt sourceDescriptor)
      $ const $ pure ()
    pure $ case controlled of
      Left failure -> Left failure
      Right (Left _) -> Left $ processError Z3SMTLibProcessSnapshotPhase
        Z3SMTLibProcessEffectiveIDExecutableAccessCheckFailed Nothing
      Right (Right result) -> case result of
        Z3SMTLibEffectiveIDExecutableAccessAdmitted -> Right ()
        Z3SMTLibEffectiveIDExecutableAccessDenied -> Left $ processError
          Z3SMTLibProcessSnapshotPhase
          Z3SMTLibProcessEffectiveIDExecutableAccessDenied Nothing
        Z3SMTLibEffectiveIDExecutableAccessCheckUnavailable -> Left
          $ processError Z3SMTLibProcessSnapshotPhase
              Z3SMTLibProcessEffectiveIDExecutableAccessCheckUnavailable
              Nothing
        Z3SMTLibEffectiveIDExecutableAccessCheckFailed -> Left $ processError
          Z3SMTLibProcessSnapshotPhase
          Z3SMTLibProcessEffectiveIDExecutableAccessCheckFailed Nothing

  createAndSealImage sourceDescriptor sourceMetadata
      canonicalWorkingDirectory workingDirectoryMetadata =
    mask $ \restoreImage -> do
      controlledImage <- restoreImage $ runBeforeZ3SMTLibProcessDeadline
        cancellation deadline createStagedDescriptor rollbackOpenedDescriptor
      case controlledImage of
        Left failure -> pure $ Left failure
        Right (Left failure) -> pure $ Left failure
        Right (Right stagedDescriptor) ->
          restoreImage
            (withStagedDescriptor sourceDescriptor sourceMetadata
              stagedDescriptor canonicalWorkingDirectory
              workingDirectoryMetadata)
            `finally` closeDescriptorIgnoringFailure stagedDescriptor

  createStagedDescriptor = do
    attempted <- tryAny c_createStagedExecutable
    pure $ case attempted of
      Right raw | raw >= 0 -> Right $ Fd raw
      _ -> Left $ processError Z3SMTLibProcessSnapshotPhase
        Z3SMTLibProcessDescriptorBoundStagingFailed Nothing

  withStagedDescriptor sourceDescriptor sourceMetadata stagedDescriptor
      canonicalWorkingDirectory workingDirectoryMetadata = do
    copied <- copyExecutableToStagedImage limits cancellation deadline
      sourceDescriptor stagedDescriptor
    case copied of
      Left failure -> pure $ Left failure
      Right (digest, count) -> do
        afterResult <- captureDescriptorMetadata sourceDescriptor
        case afterResult of
          Left failure -> pure $ Left failure
          Right afterMetadata
            | sourceMetadata /= afterMetadata -> pure $ Left $ processError
                Z3SMTLibProcessSnapshotPhase
                Z3SMTLibProcessExecutableMetadataChanged Nothing
            | otherwise -> finishStagedImage sourceDescriptor stagedDescriptor
                sourceMetadata workingDirectoryMetadata
                canonicalWorkingDirectory digest count

  finishStagedImage sourceDescriptor stagedDescriptor sourceMetadata
      workingDirectoryMetadata canonicalWorkingDirectory digest count
    | count > fromIntegral (maxBound :: Int64) =
        pure $ Left $ processError Z3SMTLibProcessSnapshotPhase
          Z3SMTLibProcessExecutableByteLimitExceeded $ Just count
    | otherwise = do
        let expected = BS.pack <$>
              Z3.z3SMTLibExecutionExpectedExecutableSHA256 profile
        case expected of
          Just pinned | pinned /= digest -> pure $ Left $ processError
            Z3SMTLibProcessSnapshotPhase
            Z3SMTLibProcessExecutableDigestMismatch Nothing
          _ -> do
            sealed <- runBeforeZ3SMTLibProcessDeadline cancellation deadline
              (sealAndVerifyEffectiveIDAccessStagedImage stagedDescriptor count)
              $ const $ pure ()
            case sealed of
              Left failure -> pure $ Left failure
              Right False -> pure $ Left $ processError
                Z3SMTLibProcessSnapshotPhase
                Z3SMTLibProcessDescriptorBoundStagingFailed Nothing
              Right True -> do
                let snapshot = Z3SMTLibExecutableSnapshot digest count
                      $ descriptorBoundEffectiveIDExecutableAccessSnapshotField
                          profile workingDirectory canonicalWorkingDirectory
                          sourceMetadata workingDirectoryMetadata digest count
                          expected
                hooked <- runBeforeZ3SMTLibProcessDeadline cancellation deadline
                  preFinalAccessHook $ const $ pure ()
                case hooked of
                  Left failure -> pure $ Left failure
                  Right () -> do
                    admitted <- observeEffectiveIDAccess sourceDescriptor
                    case admitted of
                      Left failure -> pure $ Left failure
                      Right () -> spawnStagedImage snapshot stagedDescriptor
                        canonicalWorkingDirectory

  spawnStagedImage snapshot stagedDescriptor canonicalWorkingDirectory = do
    rollbackStatus <- newEmptyTMVarIO
    let rollbackAttempt attempted = case attempted of
          Left _ -> pure ()
          Right created -> do
            cleanup <- cleanupDescriptorCreated limits created
            atomically $ void $ tryPutTMVar rollbackStatus cleanup
    controlledSpawn <- runBeforeZ3SMTLibProcessDeadline cancellation deadline
      (nativeDescriptorSpawn limits profile stagedDescriptor
        workingDirectoryFd
        ProcessDescriptorBoundEffectiveIDExecutableAccessLaunch)
      rollbackAttempt
    case controlledSpawn of
      Left failure -> do
        cleanup <- atomically $ tryReadTMVar rollbackStatus
        pure $ Left $ maybe failure (`attachCleanup` failure) cleanup
      Right (Left failure) -> pure $ Left failure
      Right (Right created) -> finishDescriptorCreated limits cancellation
        deadline snapshot canonicalWorkingDirectory created
#endif

-- | Explicitly weaker than an executed-image attestation method.
z3SMTLibExecutableSnapshotStrengthTag :: ByteString
z3SMTLibExecutableSnapshotStrengthTag = asciiBytes
  "path-snapshot-then-direct-spawn/stable-namespace-assumption/v1"

-- | The digest and the executed main image are obtained through one opened
-- source stream copied once into one sealed anonymous image.  Set-id and file
-- capability metadata are not carried.  This is deliberately not a claim
-- about an ELF loader, shared objects, script interpreter, solver semantics,
-- or results.
z3SMTLibDescriptorBoundExecutableLaunchStrengthTag :: ByteString
z3SMTLibDescriptorBoundExecutableLaunchStrengthTag = asciiBytes
  "opened-source-hash-copy-sealed-memfd-execveat/main-image-bytes/v1"

z3SMTLibDescriptorBoundExecutableLaunchSupported :: Bool
#ifdef DJEX_HAVE_DESCRIPTOR_BOUND_Z3_LAUNCH
z3SMTLibDescriptorBoundExecutableLaunchSupported = True
#else
z3SMTLibDescriptorBoundExecutableLaunchSupported = False
#endif

-- | The opened source must pass two effective-filesystem-credential VFS
-- execute-access observations, while the sealed memfd still binds only the
-- main-image bytes.  Neither observation is a reservation or a complete
-- exec/bprm security decision.
z3SMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchStrengthTag
  :: ByteString
z3SMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchStrengthTag =
  asciiBytes $ concat
    [ "opened-source-two-point-faccessat2-x-ok-at-empty-path-at-eaccess-"
    , "hash-copy-sealed-memfd-execveat/point-in-time-effective-id-source-"
    , "vfs-executable-access-and-main-image-bytes/v1"
    ]

z3SMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchSupported :: Bool
#ifdef DJEX_HAVE_DESCRIPTOR_BOUND_Z3_LAUNCH
z3SMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchSupported = True
#else
z3SMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchSupported = False
#endif

-- | Borrowed descriptor for the already-owned fresh working directory.  The
-- native launcher duplicates it before fork and never closes this authority.
newtype Z3SMTLibWorkingDirectoryDescriptor =
  Z3SMTLibWorkingDirectoryDescriptor CInt

mkZ3SMTLibWorkingDirectoryDescriptor
  :: Int
  -> Z3SMTLibWorkingDirectoryDescriptor
mkZ3SMTLibWorkingDirectoryDescriptor =
  Z3SMTLibWorkingDirectoryDescriptor . fromIntegral

data Z3SMTLibProcessLimitSource = Z3SMTLibProcessLimitSource
  { z3SMTLibProcessLimitSourceExecutableBytes :: !Natural
  , z3SMTLibProcessLimitSourceStdoutBytes :: !Natural
  , z3SMTLibProcessLimitSourceStderrBytes :: !Natural
  , z3SMTLibProcessLimitSourceReadChunkBytes :: !Natural
  , z3SMTLibProcessLimitSourceGracefulCloseMilliseconds :: !Natural
  , z3SMTLibProcessLimitSourceTerminateMilliseconds :: !Natural
  , z3SMTLibProcessLimitSourceKillMilliseconds :: !Natural
  }
  deriving (Eq, Ord, Show)

defaultZ3SMTLibProcessLimitSource :: Z3SMTLibProcessLimitSource
defaultZ3SMTLibProcessLimitSource = Z3SMTLibProcessLimitSource
  { z3SMTLibProcessLimitSourceExecutableBytes = 268435456
  , z3SMTLibProcessLimitSourceStdoutBytes = 1048576
  , z3SMTLibProcessLimitSourceStderrBytes = 65536
  , z3SMTLibProcessLimitSourceReadChunkBytes = 4096
  , z3SMTLibProcessLimitSourceGracefulCloseMilliseconds = 100
  , z3SMTLibProcessLimitSourceTerminateMilliseconds = 500
  , z3SMTLibProcessLimitSourceKillMilliseconds = 500
  }

data Z3SMTLibProcessLimits = Z3SMTLibProcessLimits
  !Natural !Natural !Natural !Int !Int !Int !Int

data Z3SMTLibProcessPhase
  = Z3SMTLibProcessLimitPhase
  | Z3SMTLibProcessDeadlinePhase
  | Z3SMTLibProcessWorkingDirectoryPhase
  | Z3SMTLibProcessSnapshotPhase
  | Z3SMTLibProcessSpawnPhase
  | Z3SMTLibProcessConfigurePhase
  | Z3SMTLibProcessWritePhase
  | Z3SMTLibProcessStdoutPhase
  | Z3SMTLibProcessStderrPhase
  | Z3SMTLibProcessReadyPhase
  | Z3SMTLibProcessQueryPhase
  | Z3SMTLibProcessClosePhase
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Stable sanitized classes.  No constructor retains a path, digest, command,
-- output byte, exception string, or operating-system error text.
data Z3SMTLibProcessFailureClass
  = Z3SMTLibProcessNonPositiveLimit
  | Z3SMTLibProcessLimitConversionOverflow
  | Z3SMTLibProcessCancelled
  | Z3SMTLibProcessDeadlineExceeded
  | Z3SMTLibProcessWorkingDirectoryNotAbsolute
  | Z3SMTLibProcessWorkingDirectoryUnavailable
  | Z3SMTLibProcessWorkingDirectoryNotEmpty
  | Z3SMTLibProcessExecutableUnavailable
  | Z3SMTLibProcessExecutableNotRegular
  | Z3SMTLibProcessExecutableNotExecutable
  | Z3SMTLibProcessExecutableByteLimitExceeded
  | Z3SMTLibProcessExecutableMetadataChanged
  | Z3SMTLibProcessExecutableDigestMismatch
  | Z3SMTLibProcessDescriptorBoundLaunchUnavailable
  | Z3SMTLibProcessDescriptorBoundStagingFailed
  | Z3SMTLibProcessDescriptorBoundExecFailed
  | Z3SMTLibProcessSpawnFailed
  | Z3SMTLibProcessMissingPipe
  | Z3SMTLibProcessHandleConfigurationFailed
  | Z3SMTLibProcessWriteFailed
  | Z3SMTLibProcessStdoutByteLimitExceeded
  | Z3SMTLibProcessStdoutEOF
  | Z3SMTLibProcessStdoutReadFailed
  | Z3SMTLibProcessUnexpectedPendingStdout
  | Z3SMTLibProcessStderrObserved
  | Z3SMTLibProcessStderrEOF
  | Z3SMTLibProcessStderrReadFailed
  | Z3SMTLibProcessExited
  | Z3SMTLibProcessClosed
  | Z3SMTLibProcessInternalFailure
  | Z3SMTLibProcessEffectiveIDExecutableAccessDenied
  | Z3SMTLibProcessEffectiveIDExecutableAccessCheckUnavailable
  | Z3SMTLibProcessEffectiveIDExecutableAccessCheckFailed
  deriving (Bounded, Enum, Eq, Ord, Show)

data Z3SMTLibProcessCleanupEscalation
  = Z3SMTLibProcessClosedGracefully
  | Z3SMTLibProcessTerminated
  | Z3SMTLibProcessKilled
  | Z3SMTLibProcessCleanupIncomplete
  deriving (Bounded, Enum, Eq, Ord, Show)

data Z3SMTLibProcessCleanupStatus = Z3SMTLibProcessCleanupStatus
  { z3SMTLibProcessCleanupEscalation
      :: !Z3SMTLibProcessCleanupEscalation
  , z3SMTLibProcessCleanupExitCode :: !(Maybe ExitCode)
  , z3SMTLibProcessCleanupFailureCount :: !Natural
  , z3SMTLibProcessCleanupReadersStopped :: !Bool
  }
  deriving (Eq, Ord, Show)

data Z3SMTLibProcessError = Z3SMTLibProcessError
  { z3SMTLibProcessErrorPhase :: !Z3SMTLibProcessPhase
  , z3SMTLibProcessErrorClass :: !Z3SMTLibProcessFailureClass
  , z3SMTLibProcessErrorObservedAtLeast :: !(Maybe Natural)
  , z3SMTLibProcessErrorCleanupStatus
      :: !(Maybe Z3SMTLibProcessCleanupStatus)
  }
  deriving (Eq, Ord, Show)

mkZ3SMTLibProcessLimits
  :: Z3SMTLibProcessLimitSource
  -> Either Z3SMTLibProcessError Z3SMTLibProcessLimits
mkZ3SMTLibProcessLimits source = do
  executableMaximum <- positive
    $ z3SMTLibProcessLimitSourceExecutableBytes source
  chunk <- positiveInt
    $ z3SMTLibProcessLimitSourceReadChunkBytes source
  graceful <- milliseconds
    $ z3SMTLibProcessLimitSourceGracefulCloseMilliseconds source
  terminate <- milliseconds
    $ z3SMTLibProcessLimitSourceTerminateMilliseconds source
  kill <- milliseconds
    $ z3SMTLibProcessLimitSourceKillMilliseconds source
  pure $ Z3SMTLibProcessLimits
    executableMaximum
    (z3SMTLibProcessLimitSourceStdoutBytes source)
    (z3SMTLibProcessLimitSourceStderrBytes source)
    chunk graceful terminate kill
 where
  positive value
    | value == 0 = Left $ processError Z3SMTLibProcessLimitPhase
        Z3SMTLibProcessNonPositiveLimit $ Just value
    | otherwise = Right value
  positiveInt value = do
    retained <- positive value
    if retained > fromIntegral (maxBound :: Int)
      then Left $ processError Z3SMTLibProcessLimitPhase
        Z3SMTLibProcessLimitConversionOverflow $ Just retained
      else Right $ fromIntegral retained
  milliseconds value = do
    retained <- positive value
    if retained > fromIntegral ((maxBound :: Int) `div` 1000)
      then Left $ processError Z3SMTLibProcessLimitPhase
        Z3SMTLibProcessLimitConversionOverflow $ Just retained
      else Right $ fromIntegral retained

z3SMTLibProcessExecutableByteLimit
  :: Z3SMTLibProcessLimits
  -> Natural
z3SMTLibProcessExecutableByteLimit
    (Z3SMTLibProcessLimits value _ _ _ _ _ _) = value

z3SMTLibProcessStdoutByteLimit
  :: Z3SMTLibProcessLimits
  -> Natural
z3SMTLibProcessStdoutByteLimit
    (Z3SMTLibProcessLimits _ value _ _ _ _ _) = value

z3SMTLibProcessStderrByteLimit
  :: Z3SMTLibProcessLimits
  -> Natural
z3SMTLibProcessStderrByteLimit
    (Z3SMTLibProcessLimits _ _ value _ _ _ _) = value

z3SMTLibProcessReadChunkByteLimit
  :: Z3SMTLibProcessLimits
  -> Natural
z3SMTLibProcessReadChunkByteLimit
    (Z3SMTLibProcessLimits _ _ _ value _ _ _) = fromIntegral value

z3SMTLibProcessGracefulCloseMilliseconds
  :: Z3SMTLibProcessLimits
  -> Natural
z3SMTLibProcessGracefulCloseMilliseconds
    (Z3SMTLibProcessLimits _ _ _ _ value _ _) = fromIntegral value

z3SMTLibProcessTerminateMilliseconds
  :: Z3SMTLibProcessLimits
  -> Natural
z3SMTLibProcessTerminateMilliseconds
    (Z3SMTLibProcessLimits _ _ _ _ _ value _) = fromIntegral value

z3SMTLibProcessKillMilliseconds
  :: Z3SMTLibProcessLimits
  -> Natural
z3SMTLibProcessKillMilliseconds
    (Z3SMTLibProcessLimits _ _ _ _ _ _ value) = fromIntegral value

-- | Ordered, schema-free process-limit facts.  A semantic-domain facade owns
-- the tag under which these fields are sealed.
z3SMTLibProcessLimitFingerprintFields
  :: Z3SMTLibProcessLimits
  -> [FingerprintField]
z3SMTLibProcessLimitFingerprintFields limits =
  [ FingerprintTag (ascii "executable-bytes")
      [FingerprintNatural $ z3SMTLibProcessExecutableByteLimit limits]
  , FingerprintTag (ascii "stdout-bytes")
      [FingerprintNatural $ z3SMTLibProcessStdoutByteLimit limits]
  , FingerprintTag (ascii "stderr-bytes")
      [FingerprintNatural $ z3SMTLibProcessStderrByteLimit limits]
  , FingerprintTag (ascii "read-chunk-bytes")
      [FingerprintNatural $ z3SMTLibProcessReadChunkByteLimit limits]
  , FingerprintTag (ascii "graceful-close-milliseconds")
      [ FingerprintNatural
          $ z3SMTLibProcessGracefulCloseMilliseconds limits
      ]
  , FingerprintTag (ascii "terminate-milliseconds")
      [FingerprintNatural $ z3SMTLibProcessTerminateMilliseconds limits]
  , FingerprintTag (ascii "kill-milliseconds")
      [FingerprintNatural $ z3SMTLibProcessKillMilliseconds limits]
  ]

newtype Z3SMTLibProcessDeadline = Z3SMTLibProcessDeadline Word64
  deriving (Eq, Ord)

mkZ3SMTLibProcessDeadline :: Word64 -> Z3SMTLibProcessDeadline
mkZ3SMTLibProcessDeadline = Z3SMTLibProcessDeadline

z3SMTLibProcessDeadlineAfterMilliseconds
  :: Int
  -> IO (Either Z3SMTLibProcessError Z3SMTLibProcessDeadline)
z3SMTLibProcessDeadlineAfterMilliseconds milliseconds
  | milliseconds <= 0 = pure $ Left $ processError
      Z3SMTLibProcessDeadlinePhase
      Z3SMTLibProcessNonPositiveLimit
      (Just $ fromIntegral $ max 0 milliseconds)
  | otherwise = do
      now <- getMonotonicTimeNSec
      let delta = toInteger milliseconds * 1000000
          target = toInteger now + delta
      pure $ if target > toInteger (maxBound :: Word64)
        then Left $ processError Z3SMTLibProcessDeadlinePhase
          Z3SMTLibProcessLimitConversionOverflow
          (Just $ fromIntegral milliseconds)
        else Right $ Z3SMTLibProcessDeadline $ fromInteger target

compareZ3SMTLibProcessDeadline
  :: Z3SMTLibProcessDeadline
  -> Z3SMTLibProcessDeadline
  -> Ordering
compareZ3SMTLibProcessDeadline = compare

minimumZ3SMTLibProcessDeadline
  :: Z3SMTLibProcessDeadline
  -> Z3SMTLibProcessDeadline
  -> Z3SMTLibProcessDeadline
minimumZ3SMTLibProcessDeadline left right
  | left <= right = left
  | otherwise = right

checkZ3SMTLibProcessDeadline
  :: Z3SMTLibProcessDeadline
  -> IO (Either Z3SMTLibProcessError ())
checkZ3SMTLibProcessDeadline (Z3SMTLibProcessDeadline deadline) = do
  now <- getMonotonicTimeNSec
  pure $ if now >= deadline
    then Left $ processError
      Z3SMTLibProcessDeadlinePhase Z3SMTLibProcessDeadlineExceeded Nothing
    else Right ()

z3SMTLibProcessMonotonicTimeNanoseconds :: IO Word64
z3SMTLibProcessMonotonicTimeNanoseconds = getMonotonicTimeNSec

z3SMTLibProcessDeadlineFingerprintField
  :: Z3SMTLibProcessDeadline
  -> FingerprintField
z3SMTLibProcessDeadlineFingerprintField
    (Z3SMTLibProcessDeadline nanoseconds) = FingerprintTag
  (ascii "absolute-monotonic-deadline")
  [ FingerprintBytes $ ascii "clock_gettime-monotonic-nanoseconds/v1"
  , FingerprintNatural $ fromIntegral nanoseconds
  ]

newtype Z3SMTLibProcessCancellation =
  Z3SMTLibProcessCancellation (TVar Bool)

newZ3SMTLibProcessCancellation :: IO Z3SMTLibProcessCancellation
newZ3SMTLibProcessCancellation =
  Z3SMTLibProcessCancellation <$> newTVarIO False

cancelZ3SMTLibProcess :: Z3SMTLibProcessCancellation -> IO ()
cancelZ3SMTLibProcess (Z3SMTLibProcessCancellation cancelled) =
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
runBeforeZ3SMTLibProcessDeadline
  :: Z3SMTLibProcessCancellation
  -> Z3SMTLibProcessDeadline
  -> IO value
  -> (value -> IO ())
  -> IO (Either Z3SMTLibProcessError value)
runBeforeZ3SMTLibProcessDeadline cancellation deadline action rollback =
  mask $ \restore -> do
    initial <- checkCancellationDeadline
      Z3SMTLibProcessDeadlinePhase cancellation deadline
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
              Z3SMTLibProcessDeadlinePhase cancellation deadline
            case finalControl of
              Left failure -> do
                rollbackProduced
                pure $ Left failure
              Right () -> case attempted of
                Left _ -> pure $ Left $ processError
                  Z3SMTLibProcessDeadlinePhase
                  Z3SMTLibProcessInternalFailure Nothing
                Right value -> pure $ Right value

data PortableMetadata = PortableMetadata
  !Integer !Bool !Bool !Bool !Bool !String
  deriving (Eq)

#ifndef mingw32_HOST_OS
data PosixMetadata = PosixMetadata
  !Integer !Integer !Integer !Integer !Integer !String !String
  deriving (Eq)
#endif

data Z3SMTLibExecutableSnapshot = Z3SMTLibExecutableSnapshot
  !ByteString
  !Natural
  !FingerprintField

z3SMTLibExecutableSnapshotSHA256
  :: Z3SMTLibExecutableSnapshot
  -> ByteString
z3SMTLibExecutableSnapshotSHA256
    (Z3SMTLibExecutableSnapshot digest _ _) = digest

z3SMTLibExecutableSnapshotByteCount
  :: Z3SMTLibExecutableSnapshot
  -> Natural
z3SMTLibExecutableSnapshotByteCount
    (Z3SMTLibExecutableSnapshot _ count _) = count

z3SMTLibExecutableSnapshotFingerprintField
  :: Z3SMTLibExecutableSnapshot
  -> FingerprintField
z3SMTLibExecutableSnapshotFingerprintField
    (Z3SMTLibExecutableSnapshot _ _ field) = field

data ProcessLifecycle = ProcessOpen | ProcessClosing | ProcessClosed
  deriving (Eq)

-- Stdout terminal conditions stay in the same FIFO as chunks.  In particular,
-- EOF cannot overtake bytes which were read before it.
data StdoutEvent
  = StdoutChunk !SMTLibCausalStdoutChunk
  | StdoutTerminal !Z3SMTLibProcessError

data ManagedThread = ManagedThread !ThreadId !(TMVar ())

data CloseState
  = CloseNotStarted
  | CloseRunning !(MVar Z3SMTLibProcessCleanupStatus)
  | CloseFinished !Z3SMTLibProcessCleanupStatus

-- The observation constructor is strict in the live process, like the former
-- domain fingerprint root, while its ordered field list deliberately remains
-- lazy.  Opening a process therefore does not encode or traverse identity
-- payloads before a domain facade asks for them.
data Z3SMTLibProcessObservation = Z3SMTLibProcessObservation
  [FingerprintField]

data ProcessExecutableLaunchStrategy
  = ProcessPathSnapshotThenDirectSpawn
  | ProcessDescriptorBoundExecutableLaunch
  | ProcessDescriptorBoundEffectiveIDExecutableAccessLaunch
  deriving (Eq)

-- | Closed, sanitized result of one effective-ID source execute-access
-- observation.  Native errno values never cross this boundary.
data Z3SMTLibEffectiveIDExecutableAccessCheckResult
  = Z3SMTLibEffectiveIDExecutableAccessAdmitted
  | Z3SMTLibEffectiveIDExecutableAccessDenied
  | Z3SMTLibEffectiveIDExecutableAccessCheckUnavailable
  | Z3SMTLibEffectiveIDExecutableAccessCheckFailed
  deriving (Bounded, Enum, Eq, Ord, Show)

data Z3SMTLibProcess = Z3SMTLibProcess
  { processInput :: !Handle
  , processOutput :: !Handle
  , processErrorOutput :: !Handle
  , processHandle :: !ProcessHandle
  , processGroupIdentifier :: !(Maybe Integer)
  , processLimits :: !Z3SMTLibProcessLimits
  , processSnapshot :: !Z3SMTLibExecutableSnapshot
  , processExecutableLaunchStrategy :: !ProcessExecutableLaunchStrategy
  , processObservation :: !Z3SMTLibProcessObservation
  , processStdoutQueue :: !(TQueue StdoutEvent)
  , processStdoutTerminal :: !(TVar (Maybe Z3SMTLibProcessError))
  , processPoison :: !(TVar (Maybe Z3SMTLibProcessError))
  , processStdoutCount :: !(TVar Natural)
  , processStderrCount :: !(TVar Natural)
  , processLifecycle :: !(TVar ProcessLifecycle)
  , processWriteToken :: !(TMVar ())
  , processThreads :: !(TVar [ManagedThread])
  , processCloseState :: !(MVar CloseState)
  }

z3SMTLibProcessSnapshot
  :: Z3SMTLibProcess
  -> Z3SMTLibExecutableSnapshot
z3SMTLibProcessSnapshot = processSnapshot

z3SMTLibProcessUsesDescriptorBoundExecutableLaunch
  :: Z3SMTLibProcess
  -> Bool
z3SMTLibProcessUsesDescriptorBoundExecutableLaunch process =
  processExecutableLaunchStrategy process ==
    ProcessDescriptorBoundExecutableLaunch

z3SMTLibProcessUsesDescriptorBoundEffectiveIDExecutableAccessLaunch
  :: Z3SMTLibProcess
  -> Bool
z3SMTLibProcessUsesDescriptorBoundEffectiveIDExecutableAccessLaunch process =
  processExecutableLaunchStrategy process ==
    ProcessDescriptorBoundEffectiveIDExecutableAccessLaunch

-- | The ordered, schema-free launch observation projected from this exact
-- process.  Domain facades should seal this projection directly with the
-- process-associated limits rather than accepting either value independently.
z3SMTLibProcessObservationFingerprintFields
  :: Z3SMTLibProcess
  -> [FingerprintField]
z3SMTLibProcessObservationFingerprintFields process =
  case processObservation process of
    Z3SMTLibProcessObservation fields -> fields

z3SMTLibProcessLimits :: Z3SMTLibProcess -> Z3SMTLibProcessLimits
z3SMTLibProcessLimits = processLimits

z3SMTLibProcessObservedStdoutBytes
  :: Z3SMTLibProcess
  -> IO Natural
z3SMTLibProcessObservedStdoutBytes process =
  atomically $ readTVar $ processStdoutCount process

z3SMTLibProcessObservedStderrBytes
  :: Z3SMTLibProcess
  -> IO Natural
z3SMTLibProcessObservedStderrBytes process =
  atomically $ readTVar $ processStderrCount process

openZ3SMTLibProcess
  :: Z3SMTLibProcessLimits
  -> Z3SMTLibProcessCancellation
  -> Z3SMTLibProcessDeadline
  -> Z3.Z3SMTLibExecutionProfile
  -> FilePath
  -> IO (Either Z3SMTLibProcessError Z3SMTLibProcess)
openZ3SMTLibProcess limits cancellation deadline profile workingDirectory =
  mask $ \restore -> do
    initial <- checkCancellationDeadline
      Z3SMTLibProcessSnapshotPhase cancellation deadline
    case initial of
      Left failure -> pure $ Left failure
      Right () -> do
        observed <- restore $ snapshotExecutable limits cancellation deadline
          profile workingDirectory
        case observed of
          Left failure -> pure $ Left failure
          Right (snapshot, canonicalWorkingDirectory) -> do
            beforeSpawn <- checkCancellationDeadline
              Z3SMTLibProcessSpawnPhase cancellation deadline
            case beforeSpawn of
              Left failure -> pure $ Left failure
              Right () -> spawn snapshot canonicalWorkingDirectory
 where
  spawn snapshot canonicalWorkingDirectory = do
    rollbackStatus <- newEmptyTMVarIO
    let executablePath = Z3.z3SMTLibExecutionExecutablePath profile
        arguments = Z3.z3SMTLibExecutionConfiguredArgumentVector profile
        specification = (proc executablePath arguments)
          { cwd = Just canonicalWorkingDirectory
          , env = Z3.z3SMTLibExecutionChildEnvironment
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
    controlledCreate <- runBeforeZ3SMTLibProcessDeadline
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
      Left _ -> pure $ Left $ processError Z3SMTLibProcessSpawnPhase
        Z3SMTLibProcessSpawnFailed Nothing
      Right (input, output, errorOutput, handle) -> do
        pidResult <- tryIOError $ getPid handle
        let groupIdentifier = case pidResult of
              Right (Just pid) -> Just $ toInteger pid
              _ -> Nothing
        case (input, output, errorOutput) of
          (Just inputHandle, Just outputHandle, Just errorHandle) -> do
            allocated <- tryAny $ allocateProcess inputHandle outputHandle
              errorHandle handle groupIdentifier
              ProcessPathSnapshotThenDirectSpawn snapshot
              canonicalWorkingDirectory
            case allocated of
              Left _ -> do
                cleanup <- cleanupAcquired limits input output errorOutput handle
                  groupIdentifier []
                pure $ Left $ attachCleanup cleanup $ processError
                  Z3SMTLibProcessConfigurePhase
                  Z3SMTLibProcessInternalFailure Nothing
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
                              ready <- checkZ3SMTLibProcessReady
                                process cancellation deadline
                              case ready of
                                Left failure ->
                                  closeAfterOpenFailure process failure
                                Right () -> pure $ Right process
                initialize `onException`
                  void (closeZ3SMTLibProcess process)
          _ -> do
            cleanup <- cleanupAcquired limits input output errorOutput handle
              groupIdentifier []
            pure $ Left $ attachCleanup cleanup $ processError
              Z3SMTLibProcessSpawnPhase
              Z3SMTLibProcessMissingPipe Nothing

  allocateProcess input output errorOutput handle groupIdentifier strategy
      snapshot canonicalWorkingDirectory = do
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
        observation = Z3SMTLibProcessObservation
          $ processObservationFingerprintFields strategy deadline snapshot
            canonicalWorkingDirectory pidField
    pure Z3SMTLibProcess
      { processInput = input
      , processOutput = output
      , processErrorOutput = errorOutput
      , processHandle = handle
      , processGroupIdentifier = groupIdentifier
      , processLimits = limits
      , processSnapshot = snapshot
      , processExecutableLaunchStrategy = strategy
      , processObservation = observation
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

-- | Linux-only descriptor-bound launcher.  The configured pathname is opened
-- once without following a final symlink, copied once while hashing into a
-- sealed anonymous file, and never used for spawn.  The hook is deliberately
-- after source admission and image sealing but before any child allocation.
openZ3SMTLibDescriptorBoundProcess
  :: Z3SMTLibProcessLimits
  -> Z3SMTLibProcessCancellation
  -> Z3SMTLibProcessDeadline
  -> Z3.Z3SMTLibExecutionProfile
  -> FilePath
  -> Z3SMTLibWorkingDirectoryDescriptor
  -> IO (Either Z3SMTLibProcessError Z3SMTLibProcess)
openZ3SMTLibDescriptorBoundProcess limits cancellation deadline profile
    workingDirectory workingDirectoryDescriptor =
  openZ3SMTLibDescriptorBoundProcessWithPreExecHook limits cancellation
    deadline profile workingDirectory workingDirectoryDescriptor $ pure ()

openZ3SMTLibDescriptorBoundProcessWithPreExecHook
  :: Z3SMTLibProcessLimits
  -> Z3SMTLibProcessCancellation
  -> Z3SMTLibProcessDeadline
  -> Z3.Z3SMTLibExecutionProfile
  -> FilePath
  -> Z3SMTLibWorkingDirectoryDescriptor
  -> IO ()
  -> IO (Either Z3SMTLibProcessError Z3SMTLibProcess)
#ifndef DJEX_HAVE_DESCRIPTOR_BOUND_Z3_LAUNCH
openZ3SMTLibDescriptorBoundProcessWithPreExecHook _ _ _ _ _ _ _ =
  pure $ Left $ processError Z3SMTLibProcessSpawnPhase
    Z3SMTLibProcessDescriptorBoundLaunchUnavailable Nothing
#else
openZ3SMTLibDescriptorBoundProcessWithPreExecHook limits cancellation deadline
    profile workingDirectory workingDirectoryDescriptor preExecHook =
  mask $ \restore -> do
    initial <- checkCancellationDeadline
      Z3SMTLibProcessSnapshotPhase cancellation deadline
    case initial of
      Left failure -> pure $ Left failure
      Right () -> do
        controlledOpen <- restore $ runBeforeZ3SMTLibProcessDeadline
          cancellation deadline openSourceDescriptor rollbackOpenedDescriptor
        case controlledOpen of
          Left failure -> pure $ Left failure
          Right (Left failure) -> pure $ Left failure
          Right (Right sourceDescriptor) ->
            restore (withSourceDescriptor sourceDescriptor)
              `finally` closeDescriptorIgnoringFailure sourceDescriptor
 where
  Z3SMTLibWorkingDirectoryDescriptor workingDirectoryRaw =
    workingDirectoryDescriptor
  workingDirectoryFd = Fd workingDirectoryRaw

  openSourceDescriptor = do
    opened <- tryAny $ withFilePath
      (Z3.z3SMTLibExecutionExecutablePath profile) c_openExecutableDescriptor
    pure $ case opened of
      Right raw | raw >= 0 -> Right $ Fd raw
      _ -> Left $ processError Z3SMTLibProcessSnapshotPhase
        Z3SMTLibProcessExecutableUnavailable Nothing

  rollbackOpenedDescriptor (Left _) = pure ()
  rollbackOpenedDescriptor (Right descriptor) =
    closeDescriptorIgnoringFailure descriptor

  withSourceDescriptor sourceDescriptor = do
    admittedWorkingDirectory <- inspectDescriptorWorkingDirectory cancellation
      deadline workingDirectory workingDirectoryFd
    case admittedWorkingDirectory of
      Left failure -> pure $ Left failure
      Right (canonicalWorkingDirectory, workingDirectoryMetadata) -> do
        beforeResult <- captureDescriptorMetadata sourceDescriptor
        case beforeResult of
          Left failure -> pure $ Left failure
          Right sourceMetadata
            | not $ descriptorMetadataIsRegular sourceMetadata ->
                pure $ Left $ processError Z3SMTLibProcessSnapshotPhase
                  Z3SMTLibProcessExecutableNotRegular Nothing
            | not $ descriptorMetadataHasExecuteBit sourceMetadata ->
                pure $ Left $ processError Z3SMTLibProcessSnapshotPhase
                  Z3SMTLibProcessExecutableNotExecutable Nothing
            | otherwise -> createAndSealImage sourceDescriptor sourceMetadata
                canonicalWorkingDirectory workingDirectoryMetadata

  createAndSealImage sourceDescriptor sourceMetadata
      canonicalWorkingDirectory workingDirectoryMetadata =
    mask $ \restoreImage -> do
      controlledImage <- restoreImage $ runBeforeZ3SMTLibProcessDeadline
        cancellation deadline createStagedDescriptor rollbackOpenedDescriptor
      case controlledImage of
        Left failure -> pure $ Left failure
        Right (Left failure) -> pure $ Left failure
        Right (Right stagedDescriptor) ->
          restoreImage
            (withStagedDescriptor sourceDescriptor sourceMetadata
              stagedDescriptor canonicalWorkingDirectory
              workingDirectoryMetadata)
            `finally` closeDescriptorIgnoringFailure stagedDescriptor

  createStagedDescriptor = do
    attempted <- tryAny c_createStagedExecutable
    pure $ case attempted of
      Right raw | raw >= 0 -> Right $ Fd raw
      _ -> Left $ processError Z3SMTLibProcessSnapshotPhase
        Z3SMTLibProcessDescriptorBoundStagingFailed Nothing

  withStagedDescriptor sourceDescriptor sourceMetadata stagedDescriptor
      canonicalWorkingDirectory workingDirectoryMetadata = do
    copied <- copyExecutableToStagedImage limits cancellation deadline
      sourceDescriptor stagedDescriptor
    case copied of
      Left failure -> pure $ Left failure
      Right (digest, count) -> do
        afterResult <- captureDescriptorMetadata sourceDescriptor
        case afterResult of
          Left failure -> pure $ Left failure
          Right afterMetadata
            | sourceMetadata /= afterMetadata -> pure $ Left $ processError
                Z3SMTLibProcessSnapshotPhase
                Z3SMTLibProcessExecutableMetadataChanged Nothing
            | otherwise -> finishStagedImage stagedDescriptor sourceMetadata
                workingDirectoryMetadata canonicalWorkingDirectory digest count

  finishStagedImage stagedDescriptor sourceMetadata workingDirectoryMetadata
      canonicalWorkingDirectory digest count
    | count > fromIntegral (maxBound :: Int64) =
        pure $ Left $ processError Z3SMTLibProcessSnapshotPhase
          Z3SMTLibProcessExecutableByteLimitExceeded $ Just count
    | otherwise = do
        let expected = BS.pack <$>
              Z3.z3SMTLibExecutionExpectedExecutableSHA256 profile
        case expected of
          Just pinned | pinned /= digest -> pure $ Left $ processError
            Z3SMTLibProcessSnapshotPhase
            Z3SMTLibProcessExecutableDigestMismatch Nothing
          _ -> do
            sealed <- runBeforeZ3SMTLibProcessDeadline cancellation deadline
              (sealAndVerifyStagedImage stagedDescriptor sourceMetadata count)
              $ const $ pure ()
            case sealed of
              Left failure -> pure $ Left failure
              Right False -> pure $ Left $ processError
                Z3SMTLibProcessSnapshotPhase
                Z3SMTLibProcessDescriptorBoundStagingFailed Nothing
              Right True -> do
                let snapshot = Z3SMTLibExecutableSnapshot digest count
                      $ descriptorBoundExecutableSnapshotField profile
                          workingDirectory canonicalWorkingDirectory
                          sourceMetadata workingDirectoryMetadata digest count
                          expected
                hooked <- runBeforeZ3SMTLibProcessDeadline
                  cancellation deadline preExecHook $ const $ pure ()
                case hooked of
                  Left failure -> pure $ Left failure
                  Right () -> spawnStagedImage snapshot stagedDescriptor
                    canonicalWorkingDirectory

  spawnStagedImage snapshot stagedDescriptor canonicalWorkingDirectory = do
    rollbackStatus <- newEmptyTMVarIO
    let rollbackAttempt attempted = case attempted of
          Left _ -> pure ()
          Right created -> do
            cleanup <- cleanupDescriptorCreated limits created
            atomically $ void $ tryPutTMVar rollbackStatus cleanup
    controlledSpawn <- runBeforeZ3SMTLibProcessDeadline cancellation deadline
      (nativeDescriptorSpawn limits profile stagedDescriptor
        workingDirectoryFd ProcessDescriptorBoundExecutableLaunch)
      rollbackAttempt
    case controlledSpawn of
      Left failure -> do
        cleanup <- atomically $ tryReadTMVar rollbackStatus
        pure $ Left $ maybe failure (`attachCleanup` failure) cleanup
      Right (Left failure) -> pure $ Left failure
      Right (Right created) -> finishDescriptorCreated limits cancellation
        deadline snapshot canonicalWorkingDirectory created
#endif

#ifdef DJEX_HAVE_DESCRIPTOR_BOUND_Z3_LAUNCH
data DescriptorCreated = DescriptorCreated
  !Handle !Handle !Handle !ProcessHandle !Integer
  !ProcessExecutableLaunchStrategy

data DescriptorMetadata = DescriptorMetadata
  !Integer !Integer !Integer !Integer !Integer !String !String
  deriving (Eq)

captureDescriptorMetadata
  :: Fd
  -> IO (Either Z3SMTLibProcessError DescriptorMetadata)
captureDescriptorMetadata descriptor = do
  attempted <- tryIOError $ getFdStatus descriptor
  pure $ case attempted of
    Left _ -> Left $ processError Z3SMTLibProcessSnapshotPhase
      Z3SMTLibProcessExecutableUnavailable Nothing
    Right status -> Right $ descriptorMetadata status

descriptorMetadata :: FileStatus -> DescriptorMetadata
descriptorMetadata status = DescriptorMetadata
  (toInteger $ deviceID status)
  (toInteger $ fileID status)
  (toInteger $ fileMode status)
  (toInteger $ fileSize status)
  (toInteger $ linkCount status)
  (show $ modificationTime status)
  (show $ statusChangeTime status)

descriptorMetadataIsRegular :: DescriptorMetadata -> Bool
descriptorMetadataIsRegular metadata =
  let DescriptorMetadata _ _ mode _ _ _ _ = metadata
  in (mode .&. 0o170000) == 0o100000

descriptorMetadataHasExecuteBit :: DescriptorMetadata -> Bool
descriptorMetadataHasExecuteBit metadata =
  let DescriptorMetadata _ _ mode _ _ _ _ = metadata
  in mode .&. 0o111 /= 0

descriptorMetadataMode :: DescriptorMetadata -> CUInt
descriptorMetadataMode (DescriptorMetadata _ _ mode _ _ _ _) =
  fromIntegral mode

inspectDescriptorWorkingDirectory
  :: Z3SMTLibProcessCancellation
  -> Z3SMTLibProcessDeadline
  -> FilePath
  -> Fd
  -> IO (Either Z3SMTLibProcessError (FilePath, DescriptorMetadata))
inspectDescriptorWorkingDirectory cancellation deadline path descriptor = do
  inspected <- inspectWorkingDirectory cancellation deadline path
  case inspected of
    Left failure -> pure $ Left failure
    Right canonical -> do
      attempted <- tryIOError $ do
        descriptorStatus <- getFdStatus descriptor
        pathStatus <- getFileStatus canonical
        pure (descriptorStatus, pathStatus)
      pure $ case attempted of
        Left _ -> Left $ processError Z3SMTLibProcessWorkingDirectoryPhase
          Z3SMTLibProcessWorkingDirectoryUnavailable Nothing
        Right (descriptorStatus, pathStatus)
          | not (isDirectory descriptorStatus) ||
              deviceID descriptorStatus /= deviceID pathStatus ||
              fileID descriptorStatus /= fileID pathStatus ->
                Left $ processError Z3SMTLibProcessWorkingDirectoryPhase
                  Z3SMTLibProcessWorkingDirectoryUnavailable Nothing
          | otherwise -> Right
              (canonical, descriptorMetadata descriptorStatus)

copyExecutableToStagedImage
  :: Z3SMTLibProcessLimits
  -> Z3SMTLibProcessCancellation
  -> Z3SMTLibProcessDeadline
  -> Fd
  -> Fd
  -> IO (Either Z3SMTLibProcessError (ByteString, Natural))
copyExecutableToStagedImage
    (Z3SMTLibProcessLimits maximumBytes _ _ chunk _ _ _)
    cancellation deadline sourceDescriptor stagedDescriptor = do
  attempted <- tryIOError $ bracket
    (duplicateDescriptorHandle sourceDescriptor)
    hClose
    $ \sourceHandle -> bracket
      (duplicateDescriptorHandle stagedDescriptor)
      hClose
      $ \stagedHandle -> do
        hSetBinaryMode sourceHandle True
        hSetBinaryMode stagedHandle True
        go sourceHandle stagedHandle SHA256.init 0
  pure $ case attempted of
    Left _ -> Left $ processError Z3SMTLibProcessSnapshotPhase
      Z3SMTLibProcessDescriptorBoundStagingFailed Nothing
    Right result -> result
 where
  go sourceHandle stagedHandle !context !count = do
    controlled <- checkCancellationDeadline
      Z3SMTLibProcessSnapshotPhase cancellation deadline
    case controlled of
      Left failure -> pure $ Left failure
      Right () -> do
        let request = boundedReadSize chunk maximumBytes count
        bytes <- BS.hGetSome sourceHandle request
        if BS.null bytes
          then do
            hFlush stagedHandle
            let digest = SHA256.finalize context
            _ <- evaluate $ BS.length digest
            pure $ Right (digest, count)
          else do
            let observed = count + fromIntegral (BS.length bytes)
            if observed > maximumBytes
              then pure $ Left $ processError Z3SMTLibProcessSnapshotPhase
                Z3SMTLibProcessExecutableByteLimitExceeded
                (Just $ maximumBytes + 1)
              else do
                BS.hPut stagedHandle bytes
                go sourceHandle stagedHandle
                  (SHA256.update context bytes) observed

duplicateDescriptorHandle :: Fd -> IO Handle
duplicateDescriptorHandle descriptor = mask_ $ do
  duplicated <- dup descriptor
  fdToHandle duplicated `onException` closeFd duplicated

sealAndVerifyStagedImage
  :: Fd
  -> DescriptorMetadata
  -> Natural
  -> IO Bool
sealAndVerifyStagedImage descriptor metadata count = do
  sealed <- c_sealStagedExecutable
    (fdToCInt descriptor)
    (descriptorMetadataMode metadata)
    (fromIntegral count)
  if sealed /= 0
    then pure False
    else do
      _ <- fdSeek descriptor AbsoluteSeek 0
      observed <- getFdStatus descriptor
      pure $ isRegularFile observed &&
        toInteger (fileSize observed) == toInteger count

sealAndVerifyEffectiveIDAccessStagedImage
  :: Fd
  -> Natural
  -> IO Bool
sealAndVerifyEffectiveIDAccessStagedImage descriptor count = do
  sealed <- c_sealEffectiveIDAccessStagedExecutable
    (fdToCInt descriptor)
    (fromIntegral count)
  if sealed /= 0
    then pure False
    else do
      _ <- fdSeek descriptor AbsoluteSeek 0
      observed <- getFdStatus descriptor
      pure $ isRegularFile observed &&
        toInteger (fileSize observed) == toInteger count &&
        toInteger (fileMode observed) .&. 0o7777 == 0o500

nativeDescriptorSpawn
  :: Z3SMTLibProcessLimits
  -> Z3.Z3SMTLibExecutionProfile
  -> Fd
  -> Fd
  -> ProcessExecutableLaunchStrategy
  -> IO (Either Z3SMTLibProcessError DescriptorCreated)
nativeDescriptorSpawn limits profile executableDescriptor
    workingDirectoryDescriptor strategy = mask $ \restore ->
  withFilePath (Z3.z3SMTLibExecutionExecutablePath profile) $ \argumentZero ->
  withMany withCString
      (Z3.z3SMTLibExecutionConfiguredArgumentVector profile) $ \arguments ->
  withArray0 nullPtr (argumentZero : arguments) $ \argumentVector ->
  withArray0 nullPtr [] $ \environmentVector ->
  alloca $ \pidPointer ->
  alloca $ \inputPointer ->
  alloca $ \outputPointer ->
  alloca $ \errorPointer ->
  alloca $ \statusPointer -> do
    spawned <- withMVar ProcessInternals.runInteractiveProcess_lock $ \() ->
      c_descriptorSpawn
        (fdToCInt executableDescriptor)
        (fdToCInt workingDirectoryDescriptor)
        argumentVector environmentVector pidPointer inputPointer outputPointer
        errorPointer statusPointer
    if spawned /= 0
      then pure $ Left $ processError Z3SMTLibProcessSpawnPhase
        Z3SMTLibProcessSpawnFailed Nothing
      else do
        pidRaw <- peek pidPointer
        inputRaw <- peek inputPointer
        outputRaw <- peek outputPointer
        errorRaw <- peek errorPointer
        statusRaw <- peek statusPointer
        let rawDescriptors = map Fd
              [inputRaw, outputRaw, errorRaw, statusRaw]
            abortRaw = do
              mapM_ closeDescriptorIgnoringFailure rawDescriptors
              c_abortDescriptorSpawn pidRaw
        processHandleResult <- tryAny $ ProcessInternals.mkProcessHandle
          (fromIntegral pidRaw :: CPid) False
        case processHandleResult of
          Left _ -> abortRaw >> pure (Left $ processError
            Z3SMTLibProcessSpawnPhase Z3SMTLibProcessInternalFailure Nothing)
          Right handle -> do
            converted <- convertDescriptorPipes limits handle
              (toInteger pidRaw) strategy rawDescriptors
            case converted of
              Left failure -> pure $ Left failure
              Right (created, statusHandle) -> do
                handshake <- restore (tryIOError $ BS.hGetSome statusHandle 1)
                  `onException` void (cleanupDescriptorCreated limits created)
                _ <- tryIOError $ hClose statusHandle
                case handshake of
                  Right bytes | BS.null bytes -> pure $ Right created
                  _ -> do
                    cleanup <- cleanupDescriptorCreated limits created
                    pure $ Left $ attachCleanup cleanup $ processError
                      Z3SMTLibProcessSpawnPhase
                      Z3SMTLibProcessDescriptorBoundExecFailed Nothing

convertDescriptorPipes
  :: Z3SMTLibProcessLimits
  -> ProcessHandle
  -> Integer
  -> ProcessExecutableLaunchStrategy
  -> [Fd]
  -> IO
      (Either
        Z3SMTLibProcessError
        (DescriptorCreated, Handle))
convertDescriptorPipes limits handle pid strategy descriptors =
  case descriptors of
    [inputFd, outputFd, errorFd, statusFd] -> do
      inputResult <- tryIOError $ fdToHandle inputFd
      case inputResult of
        Left _ -> failConversion []
          [inputFd, outputFd, errorFd, statusFd]
        Right inputHandle -> do
          outputResult <- tryIOError $ fdToHandle outputFd
          case outputResult of
            Left _ -> failConversion [inputHandle]
              [outputFd, errorFd, statusFd]
            Right outputHandle -> do
              errorResult <- tryIOError $ fdToHandle errorFd
              case errorResult of
                Left _ -> failConversion
                  [inputHandle, outputHandle] [errorFd, statusFd]
                Right errorHandle -> do
                  statusResult <- tryIOError $ fdToHandle statusFd
                  case statusResult of
                    Left _ -> failConversion
                      [inputHandle, outputHandle, errorHandle] [statusFd]
                    Right statusHandle -> pure $ Right
                      ( DescriptorCreated inputHandle outputHandle errorHandle
                          handle pid strategy
                      , statusHandle
                      )
    _ -> pure $ Left $ processError Z3SMTLibProcessSpawnPhase
      Z3SMTLibProcessInternalFailure Nothing
 where
  failConversion converted remaining = do
    mapM_ closeDescriptorIgnoringFailure remaining
    cleanup <- cleanupAcquired limits
      (atMay converted 0) (atMay converted 1) (atMay converted 2)
      handle (Just pid) []
    pure $ Left $ attachCleanup cleanup $ processError
      Z3SMTLibProcessConfigurePhase
      Z3SMTLibProcessHandleConfigurationFailed Nothing

  atMay values offset = case drop offset values of
    value : _ -> Just value
    [] -> Nothing

cleanupDescriptorCreated
  :: Z3SMTLibProcessLimits
  -> DescriptorCreated
  -> IO Z3SMTLibProcessCleanupStatus
cleanupDescriptorCreated limits
    (DescriptorCreated input output errorOutput handle pid _) =
  cleanupAcquired limits (Just input) (Just output) (Just errorOutput)
    handle (Just pid) []

finishDescriptorCreated
  :: Z3SMTLibProcessLimits
  -> Z3SMTLibProcessCancellation
  -> Z3SMTLibProcessDeadline
  -> Z3SMTLibExecutableSnapshot
  -> FilePath
  -> DescriptorCreated
  -> IO (Either Z3SMTLibProcessError Z3SMTLibProcess)
finishDescriptorCreated limits cancellation deadline snapshot
    canonicalWorkingDirectory
    created@(DescriptorCreated input output errorOutput handle pid strategy) = do
  allocated <- tryAny $ allocateDescriptorProcess limits deadline input output
    errorOutput handle pid strategy snapshot canonicalWorkingDirectory
  case allocated of
    Left _ -> do
      cleanup <- cleanupDescriptorCreated limits created
      pure $ Left $ attachCleanup cleanup $ processError
        Z3SMTLibProcessConfigurePhase Z3SMTLibProcessInternalFailure Nothing
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
                    ready <- checkZ3SMTLibProcessReady
                      process cancellation deadline
                    case ready of
                      Left failure -> closeAfterOpenFailure process failure
                      Right () -> pure $ Right process
      initialize `onException` void (closeZ3SMTLibProcess process)

allocateDescriptorProcess
  :: Z3SMTLibProcessLimits
  -> Z3SMTLibProcessDeadline
  -> Handle
  -> Handle
  -> Handle
  -> ProcessHandle
  -> Integer
  -> ProcessExecutableLaunchStrategy
  -> Z3SMTLibExecutableSnapshot
  -> FilePath
  -> IO Z3SMTLibProcess
allocateDescriptorProcess limits deadline input output errorOutput handle pid
    strategy snapshot canonicalWorkingDirectory = do
  stdoutQueue <- newTQueueIO
  stdoutTerminal <- newTVarIO Nothing
  poison <- newTVarIO Nothing
  stdoutCount <- newTVarIO 0
  stderrCount <- newTVarIO 0
  lifecycle <- newTVarIO ProcessOpen
  writeToken <- newTMVarIO ()
  threads <- newTVarIO []
  closeState <- newMVar CloseNotStarted
  let pidField = FingerprintTag (ascii "pid-observed")
        [integerTextField pid]
      observation = Z3SMTLibProcessObservation
        $ processObservationFingerprintFields strategy deadline snapshot
          canonicalWorkingDirectory pidField
  pure Z3SMTLibProcess
    { processInput = input
    , processOutput = output
    , processErrorOutput = errorOutput
    , processHandle = handle
    , processGroupIdentifier = Just pid
    , processLimits = limits
    , processSnapshot = snapshot
    , processExecutableLaunchStrategy = strategy
    , processObservation = observation
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

closeDescriptorIgnoringFailure :: Fd -> IO ()
closeDescriptorIgnoringFailure descriptor = void $ tryIOError $ closeFd descriptor

fdToCInt :: Fd -> CInt
fdToCInt (Fd raw) = raw

nativeEffectiveIDExecutableAccessCheck
  :: CInt
  -> IO Z3SMTLibEffectiveIDExecutableAccessCheckResult
nativeEffectiveIDExecutableAccessCheck descriptor = do
  observed <- c_checkEffectiveIDExecutableAccess descriptor
  pure $ case observed of
    0 -> Z3SMTLibEffectiveIDExecutableAccessAdmitted
    1 -> Z3SMTLibEffectiveIDExecutableAccessDenied
    2 -> Z3SMTLibEffectiveIDExecutableAccessCheckUnavailable
    _ -> Z3SMTLibEffectiveIDExecutableAccessCheckFailed

foreign import ccall unsafe "djex_z3_open_executable_descriptor"
  c_openExecutableDescriptor :: CString -> IO CInt

foreign import ccall unsafe "djex_z3_create_staged_executable"
  c_createStagedExecutable :: IO CInt

foreign import ccall unsafe "djex_z3_seal_staged_executable"
  c_sealStagedExecutable :: CInt -> CUInt -> Int64 -> IO CInt

foreign import ccall interruptible
  "djex_z3_check_effective_id_executable_access"
  c_checkEffectiveIDExecutableAccess :: CInt -> IO CInt

foreign import ccall unsafe
  "djex_z3_seal_effective_id_access_staged_executable"
  c_sealEffectiveIDAccessStagedExecutable :: CInt -> Int64 -> IO CInt

foreign import ccall unsafe "djex_z3_descriptor_spawn"
  c_descriptorSpawn
    :: CInt -> CInt -> Ptr CString -> Ptr CString
    -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt
    -> IO CInt

foreign import ccall unsafe "djex_z3_abort_descriptor_spawn"
  c_abortDescriptorSpawn :: CInt -> IO ()
#endif

snapshotExecutable
  :: Z3SMTLibProcessLimits
  -> Z3SMTLibProcessCancellation
  -> Z3SMTLibProcessDeadline
  -> Z3.Z3SMTLibExecutionProfile
  -> FilePath
  -> IO
      (Either
        Z3SMTLibProcessError
        (Z3SMTLibExecutableSnapshot, FilePath))
snapshotExecutable limits cancellation deadline profile workingDirectory = do
  cwdResult <- inspectWorkingDirectory cancellation deadline workingDirectory
  case cwdResult of
    Left failure -> pure $ Left failure
    Right canonicalWorkingDirectory -> do
      let executablePath = Z3.z3SMTLibExecutionExecutablePath
            profile
      canonicalBeforeResult <- tryIOError $ canonicalizePath executablePath
      case canonicalBeforeResult of
        Left _ -> pure $ Left $ processError Z3SMTLibProcessSnapshotPhase
          Z3SMTLibProcessExecutableUnavailable Nothing
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
                          Z3SMTLibProcessSnapshotPhase
                          Z3SMTLibProcessExecutableMetadataChanged Nothing
                    _ -> pure $ Left $ processError
                      Z3SMTLibProcessSnapshotPhase
                      Z3SMTLibProcessExecutableUnavailable Nothing
 where
  finishSnapshot canonicalWorkingDirectory canonical metadata digest count =
    let expected =
          BS.pack <$> Z3.z3SMTLibExecutionExpectedExecutableSHA256 profile
    in case expected of
      Just pinned | pinned /= digest -> pure $ Left $ processError
        Z3SMTLibProcessSnapshotPhase
        Z3SMTLibProcessExecutableDigestMismatch Nothing
      _ -> do
        let field = executableSnapshotField profile workingDirectory
              canonicalWorkingDirectory canonical metadata digest count expected
        pure $ Right
          ( Z3SMTLibExecutableSnapshot digest count field
          , canonicalWorkingDirectory
          )

inspectWorkingDirectory
  :: Z3SMTLibProcessCancellation
  -> Z3SMTLibProcessDeadline
  -> FilePath
  -> IO (Either Z3SMTLibProcessError FilePath)
inspectWorkingDirectory cancellation deadline path
  | not $ isAbsolute path = pure $ Left $ processError
      Z3SMTLibProcessWorkingDirectoryPhase
      Z3SMTLibProcessWorkingDirectoryNotAbsolute Nothing
  | otherwise = do
      controlled <- checkCancellationDeadline
        Z3SMTLibProcessWorkingDirectoryPhase cancellation deadline
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
              Z3SMTLibProcessWorkingDirectoryPhase
              Z3SMTLibProcessWorkingDirectoryUnavailable Nothing
            Right (False, _, _) -> Left $ processError
              Z3SMTLibProcessWorkingDirectoryPhase
              Z3SMTLibProcessWorkingDirectoryUnavailable Nothing
            Right (True, False, _) -> Left $ processError
              Z3SMTLibProcessWorkingDirectoryPhase
              Z3SMTLibProcessWorkingDirectoryNotEmpty Nothing
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
  -> IO (Either Z3SMTLibProcessError CapturedMetadata)
captureMetadata path = do
  existence <- tryIOError $ (,) <$> doesPathExist path <*> doesFileExist path
  case existence of
    Left _ -> pure $ Left $ processError Z3SMTLibProcessSnapshotPhase
      Z3SMTLibProcessExecutableUnavailable Nothing
    Right (False, _) -> pure $ Left $ processError
      Z3SMTLibProcessSnapshotPhase
      Z3SMTLibProcessExecutableUnavailable Nothing
    Right (True, False) -> pure $ Left $ processError
      Z3SMTLibProcessSnapshotPhase
      Z3SMTLibProcessExecutableNotRegular Nothing
    Right (True, True) -> capture
 where
  capture = do
    captured <- tryIOError $ do
      size <- getFileSize path
      permissions <- getPermissions path
      modified <- getModificationTime path
      let portable = PortableMetadata
            size
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
      Left _ -> Left $ processError Z3SMTLibProcessSnapshotPhase
        Z3SMTLibProcessExecutableUnavailable Nothing
#ifndef mingw32_HOST_OS
      Right (_, status, _) | not $ isRegularFile status ->
        Left $ processError Z3SMTLibProcessSnapshotPhase
          Z3SMTLibProcessExecutableNotRegular Nothing
      Right (portable, _, posix) -> Right $ CapturedMetadata portable posix
#else
      Right metadata -> Right metadata
#endif

hashExecutable
  :: Z3SMTLibProcessLimits
  -> Z3SMTLibProcessCancellation
  -> Z3SMTLibProcessDeadline
  -> FilePath
  -> IO (Either Z3SMTLibProcessError (ByteString, Natural))
hashExecutable (Z3SMTLibProcessLimits maximumBytes _ _ chunk _ _ _)
    cancellation deadline path = do
  attempted <- tryIOError $ withBinaryFile path ReadMode $ \handle -> do
    hSetBinaryMode handle True
    go handle SHA256.init 0
  pure $ case attempted of
    Left _ -> Left $ processError Z3SMTLibProcessSnapshotPhase
      Z3SMTLibProcessExecutableUnavailable Nothing
    Right result -> result
 where
  go handle !context !count = do
    controlled <- checkCancellationDeadline
      Z3SMTLibProcessSnapshotPhase cancellation deadline
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
                Z3SMTLibProcessSnapshotPhase
                Z3SMTLibProcessExecutableByteLimitExceeded
                (Just $ maximumBytes + 1)
              else go handle (SHA256.update context bytes) observed

configureHandles
  :: Z3SMTLibProcess
  -> IO (Either Z3SMTLibProcessError ())
configureHandles process = do
  configured <- tryIOError $ mapM_ configure
    [ processInput process
    , processOutput process
    , processErrorOutput process
    ]
  pure $ case configured of
    Left _ -> Left $ processError Z3SMTLibProcessConfigurePhase
      Z3SMTLibProcessHandleConfigurationFailed Nothing
    Right () -> Right ()
 where
  configure handle = do
    hSetBinaryMode handle True
    hSetBuffering handle NoBuffering

startReaders
  :: Z3SMTLibProcess
  -> IO (Either Z3SMTLibProcessError ())
startReaders process = do
  stdoutStarted <- startManagedThread process
    Z3SMTLibProcessConfigurePhase (pure Nothing) $ stdoutReader process
  case stdoutStarted of
    Left failure -> pure $ Left failure
    Right _ -> do
      stderrStarted <- startManagedThread process
        Z3SMTLibProcessConfigurePhase (pure Nothing) $ stderrReader process
      pure $ case stderrStarted of
        Left failure -> Left failure
        Right _ -> Right ()

stdoutReader :: Z3SMTLibProcess -> IO ()
stdoutReader process = loop
 where
  Z3SMTLibProcessLimits _ maximumBytes _ chunk _ _ _ =
    processLimits process
  loop = do
    lifecycle <- atomically $ readTVar $ processLifecycle process
    when (lifecycle == ProcessOpen) $ do
      count <- atomically $ readTVar $ processStdoutCount process
      received <- tryIOError $ BS.hGetSome (processOutput process)
        $ boundedReadSize chunk maximumBytes count
      case received of
        Left _ -> atomically $ setStdoutTerminalSTM process $ processError
          Z3SMTLibProcessStdoutPhase
          Z3SMTLibProcessStdoutReadFailed (Just count)
        Right bytes -> case admitSMTLibCausalStdoutChunk bytes of
          Nothing -> atomically $ setStdoutTerminalSTM process
            $ processError Z3SMTLibProcessStdoutPhase
                Z3SMTLibProcessStdoutEOF (Just count)
          Just stdoutChunk -> do
            continue <- atomically $ do
              current <- readTVar $ processStdoutCount process
              let observed = current + fromIntegral (BS.length bytes)
              if observed > maximumBytes
                then do
                  let remaining = maximumBytes - min current maximumBytes
                      permitted = BS.take (fromIntegral remaining) bytes
                  writeTVar (processStdoutCount process) $ maximumBytes + 1
                  case admitSMTLibCausalStdoutChunk permitted of
                    Nothing -> pure ()
                    Just permittedChunk -> writeTQueue
                      (processStdoutQueue process)
                      $ StdoutChunk permittedChunk
                  setStdoutTerminalSTM process $ processError
                    Z3SMTLibProcessStdoutPhase
                    Z3SMTLibProcessStdoutByteLimitExceeded
                    (Just $ maximumBytes + 1)
                  pure False
                else do
                  writeTVar (processStdoutCount process) observed
                  writeTQueue (processStdoutQueue process)
                    $ StdoutChunk stdoutChunk
                  pure True
            when continue loop

stderrReader :: Z3SMTLibProcess -> IO ()
stderrReader process = loop
 where
  Z3SMTLibProcessLimits _ _ maximumBytes chunk _ _ _ =
    processLimits process
  loop = do
    count <- atomically $ readTVar $ processStderrCount process
    let request
          | count > maximumBytes = chunk
          | otherwise = boundedReadSize chunk maximumBytes count
    received <- tryIOError $ BS.hGetSome (processErrorOutput process) request
    case received of
      Left _ -> poisonIfOpen process $ processError
        Z3SMTLibProcessStderrPhase
        Z3SMTLibProcessStderrReadFailed (Just count)
      Right bytes
        | BS.null bytes -> poisonIfOpen process $ processError
            Z3SMTLibProcessStderrPhase
            Z3SMTLibProcessStderrEOF (Just count)
        | otherwise -> do
            atomically $ do
              current <- readTVar $ processStderrCount process
              let observed = min (maximumBytes + 1)
                    $ current + fromIntegral (BS.length bytes)
              writeTVar (processStderrCount process) observed
              setFirstPoisonSTM process $ processError
                Z3SMTLibProcessStderrPhase
                Z3SMTLibProcessStderrObserved
                (Just observed)
            -- Continue discarding even after lifecycle enters Closing.  The
            -- cleanup owner first closes stdin and then reaps or kills the
            -- child before it asks this reader to stop, so a finite flood
            -- cannot retain the child by filling its stderr pipe.
            loop

writeZ3SMTLibProcess
  :: Z3SMTLibProcess
  -> Z3SMTLibProcessCancellation
  -> Z3SMTLibProcessDeadline
  -> ByteString
  -> IO (Either Z3SMTLibProcessError ())
writeZ3SMTLibProcess process cancellation deadline bytes =
  mask $ \restore -> do
    _ <- evaluate $ BS.length bytes
    token <- waitWriteControlled process cancellation deadline
      Z3SMTLibProcessWritePhase
      $ takeTMVar $ processWriteToken process
    case token of
      Left failure -> rejectWrite process failure
      Right () -> finally
        (do
          outcome <- newEmptyTMVarIO
          started <- startManagedThread process
            Z3SMTLibProcessWritePhase
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
                    Z3SMTLibProcessWritePhase $ readTMVar outcome
                  settle = waitWriteControlled process cancellation deadline
                    Z3SMTLibProcessWritePhase
                    $ managedThreadFinishedSTM managed
              result <- restore await `onException` do
                atomically $ setFirstPoisonSTM process $ processError
                  Z3SMTLibProcessWritePhase
                  Z3SMTLibProcessCancelled Nothing
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
                          Z3SMTLibProcessWritePhase
                          Z3SMTLibProcessWriteFailed Nothing
                        Right () -> pure $ Right ())
        (atomically $ putTMVar (processWriteToken process) ())

nextZ3SMTLibProcessStdoutChunk
  :: Z3SMTLibProcess
  -> Z3SMTLibProcessCancellation
  -> Z3SMTLibProcessDeadline
  -> IO (Either Z3SMTLibProcessError SMTLibCausalStdoutChunk)
nextZ3SMTLibProcessStdoutChunk process cancellation deadline = do
  result <- waitControlled process cancellation deadline
    Z3SMTLibProcessStdoutPhase
    $ readTQueue $ processStdoutQueue process
  case result of
    Left failure -> poisonOperation process failure
    Right event -> case event of
      StdoutChunk chunk -> pure $ Right chunk
      StdoutTerminal failure -> poisonOperation process failure

-- | Drain output which is causally attributable to the preceding protocol
-- write but arrived after that receiver completed.  Only SMT-LIB whitespace is
-- admitted into the opaque causal-boundary receipt.  A non-whitespace chunk is
-- restored in its original FIFO position before the process is poisoned; a
-- queued terminal condition is propagated at its exact position after any
-- preceding whitespace.
drainZ3SMTLibProcessBoundaryWhitespace
  :: Z3SMTLibProcess
  -> Z3SMTLibProcessCancellation
  -> Z3SMTLibProcessDeadline
  -> IO
      (Either Z3SMTLibProcessError SMTLibCausalBoundaryWhitespace)
drainZ3SMTLibProcessBoundaryWhitespace process cancellation deadline = do
  drained <- waitControlled process cancellation deadline
    Z3SMTLibProcessReadyPhase drainQueued
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

  inspect [] reversedWhitespace = pure $ Right
    $ concatSMTLibCausalBoundaryWhitespace
    $ map snd
    $ reverse reversedWhitespace
  inspect events@(StdoutChunk chunk : remaining) reversedWhitespace =
    case admitSMTLibCausalBoundaryWhitespace
        $ smtLibCausalStdoutChunkBytes chunk of
      Just whitespace -> inspect remaining
        $ (chunk, whitespace) : reversedWhitespace
      Nothing -> do
        -- Restore all prior whitespace too; draining is all-or-nothing when a
        -- non-whitespace chunk is present.
        mapM_ (writeTQueue $ processStdoutQueue process)
          $ reverse
              (map (StdoutChunk . fst) reversedWhitespace)
            ++ events
        pure $ Left $ processError Z3SMTLibProcessReadyPhase
          Z3SMTLibProcessUnexpectedPendingStdout Nothing
  inspect (StdoutTerminal failure : _) _ = pure $ Left failure

checkZ3SMTLibProcessReady
  :: Z3SMTLibProcess
  -> Z3SMTLibProcessCancellation
  -> Z3SMTLibProcessDeadline
  -> IO (Either Z3SMTLibProcessError ())
checkZ3SMTLibProcessReady process cancellation deadline = do
  preflight <- checkReadySnapshot process cancellation deadline
  case preflight of
    Left failure -> poisonOperation process failure
    Right () -> do
      exited <- tryIOError $ getProcessExitCode $ processHandle process
      case exited of
        Left _ -> poisonOperation process $ processError
          Z3SMTLibProcessReadyPhase Z3SMTLibProcessExited Nothing
        Right (Just _) -> poisonOperation process $ processError
          Z3SMTLibProcessReadyPhase Z3SMTLibProcessExited Nothing
        Right Nothing -> do
          final <- checkReadySnapshot process cancellation deadline
          case final of
            Left failure -> poisonOperation process failure
            Right () -> pure $ Right ()

checkReadySnapshot
  :: Z3SMTLibProcess
  -> Z3SMTLibProcessCancellation
  -> Z3SMTLibProcessDeadline
  -> IO (Either Z3SMTLibProcessError ())
checkReadySnapshot process cancellation deadline = do
  control <- checkCancellationDeadline
    Z3SMTLibProcessReadyPhase cancellation deadline
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
            then pure $ Left $ processError Z3SMTLibProcessReadyPhase
              Z3SMTLibProcessClosed Nothing
            else if empty
              then pure $ Right ()
              else do
                pending <- peekTQueue $ processStdoutQueue process
                pure $ case pending of
                  StdoutTerminal failure -> Left failure
                  StdoutChunk _ -> Left $ processError
                    Z3SMTLibProcessReadyPhase
                    Z3SMTLibProcessUnexpectedPendingStdout Nothing

closeZ3SMTLibProcess
  :: Z3SMTLibProcess
  -> IO Z3SMTLibProcessCleanupStatus
closeZ3SMTLibProcess process = mask_ $ do
  gate <- modifyMVar (processCloseState process) $ \state -> case state of
    CloseNotStarted -> do
      result <- newEmptyMVar
      atomically $ do
        writeTVar (processLifecycle process) ProcessClosing
        setFirstPoisonSTM process $ processError
          Z3SMTLibProcessClosePhase Z3SMTLibProcessClosed Nothing
      _ <- forkIO $ do
        attempted <- tryAny $ cleanupProcess process
        let status = case attempted of
              Right completed -> completed
              Left _ -> Z3SMTLibProcessCleanupStatus
                { z3SMTLibProcessCleanupEscalation =
                    Z3SMTLibProcessCleanupIncomplete
                , z3SMTLibProcessCleanupExitCode = Nothing
                , z3SMTLibProcessCleanupFailureCount = 1
                , z3SMTLibProcessCleanupReadersStopped = False
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

cleanupProcess :: Z3SMTLibProcess -> IO Z3SMTLibProcessCleanupStatus
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
  :: Z3SMTLibProcessLimits
  -> Maybe Handle
  -> Maybe Handle
  -> Maybe Handle
  -> ProcessHandle
  -> Maybe Integer
  -> [ManagedThread]
  -> IO Z3SMTLibProcessCleanupStatus
cleanupAcquired limits input output errorOutput handle groupIdentifier threads =
  mask_ $ do
    inputClosed <- closeMaybe input
    graceful <- boundedWait handle gracefulMilliseconds
    (escalation, exitCode, signalFailures) <- case graceful of
      Just status -> pure
        (Z3SMTLibProcessClosedGracefully, Just status, 0)
      Nothing -> do
        terminateFailures <- terminateOwnedGroup handle groupIdentifier
        terminated <- boundedWait handle terminateMilliseconds
        case terminated of
          Just status -> pure
            (Z3SMTLibProcessTerminated, Just status, terminateFailures)
          Nothing -> do
            killFailures <- forceKill groupIdentifier
            killed <- boundedWait handle killMilliseconds
            pure $ case killed of
              Just status ->
                ( Z3SMTLibProcessKilled
                , Just status
                , terminateFailures + killFailures
                )
              Nothing ->
                ( Z3SMTLibProcessCleanupIncomplete
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
              Z3SMTLibProcessCleanupIncomplete
          | otherwise = escalation
    pure Z3SMTLibProcessCleanupStatus
      { z3SMTLibProcessCleanupEscalation = finalEscalation
      , z3SMTLibProcessCleanupExitCode = exitCode
      , z3SMTLibProcessCleanupFailureCount = totalFailures
      , z3SMTLibProcessCleanupReadersStopped = readersStopped
      }
 where
  Z3SMTLibProcessLimits _ _ _ _ gracefulMilliseconds
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
  groupKilled <- tryIOError $ signalProcessGroup sigKILL
    $ fromInteger identifier
  case groupKilled of
    Right () -> pure 0
    Left _ -> do
      leaderKilled <- tryIOError $ signalProcess sigKILL
        $ fromInteger identifier
      pure $ 1 + unitFailure leaderKilled
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
  :: Z3SMTLibProcess
  -> Z3SMTLibProcessPhase
  -> STM (Maybe Z3SMTLibProcessError)
  -> IO ()
  -> IO (Either Z3SMTLibProcessError ManagedThread)
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
      _ -> pure $ Left $ processError phase Z3SMTLibProcessClosed Nothing
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

unregisterManagedThread :: Z3SMTLibProcess -> ManagedThread -> IO ()
unregisterManagedThread process target = atomically $ modifyTVar'
  (processThreads process) $ filter $ not . sameManagedThread target

sameManagedThread :: ManagedThread -> ManagedThread -> Bool
sameManagedThread (ManagedThread left _) (ManagedThread right _) = left == right

waitBeforeDeadline
  :: Z3SMTLibProcessCancellation
  -> Z3SMTLibProcessDeadline
  -> TMVar (Either SomeException value)
  -> IO
      (Either
        Z3SMTLibProcessError
        (Either SomeException value))
waitBeforeDeadline cancellation deadline outcome = go
 where
  go = do
    control <- checkCancellationDeadline
      Z3SMTLibProcessDeadlinePhase cancellation deadline
    case control of
      Left failure -> pure $ Left failure
      Right () -> do
        remaining <- remainingDeadlineMicroseconds deadline
        case remaining of
          Nothing -> pure $ Left $ processError
            Z3SMTLibProcessDeadlinePhase
            Z3SMTLibProcessDeadlineExceeded Nothing
          Just microseconds -> do
            observed <- timeout microseconds $ atomically $
              cancellationSTM cancellation Z3SMTLibProcessDeadlinePhase
              `orElse` (Right <$> readTMVar outcome)
            case observed of
              Nothing -> go
              Just value -> do
                finalControl <- checkCancellationDeadline
                  Z3SMTLibProcessDeadlinePhase cancellation deadline
                pure $ case finalControl of
                  Left failure -> Left failure
                  Right () -> value

waitControlled
  :: Z3SMTLibProcess
  -> Z3SMTLibProcessCancellation
  -> Z3SMTLibProcessDeadline
  -> Z3SMTLibProcessPhase
  -> STM value
  -> IO (Either Z3SMTLibProcessError value)
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
            Z3SMTLibProcessDeadlineExceeded Nothing
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

-- | Wait for one package-private, non-owning STM admission observation under
-- the same process, cancellation, poison, lifecycle, and absolute-deadline
-- precedence as pipe operations.  The action must not destructively acquire a
-- resource: the final precedence check may discard its result.  A caller may
-- first observe its query gate here, then claim it nonblockingly under masking.
waitZ3SMTLibProcessControl
  :: Z3SMTLibProcess
  -> Z3SMTLibProcessCancellation
  -> Z3SMTLibProcessDeadline
  -> Z3SMTLibProcessPhase
  -> STM value
  -> IO (Either Z3SMTLibProcessError value)
waitZ3SMTLibProcessControl = waitControlled

-- A stdout terminal condition participates in write admission without becoming
-- global poison.  Thus a write cannot start after the reader records terminal,
-- while 'nextZ3SMTLibProcessStdoutChunk' can still deliver chunks which
-- precede that terminal in the stdout FIFO.
waitWriteControlled
  :: Z3SMTLibProcess
  -> Z3SMTLibProcessCancellation
  -> Z3SMTLibProcessDeadline
  -> Z3SMTLibProcessPhase
  -> STM value
  -> IO (Either Z3SMTLibProcessError value)
waitWriteControlled process cancellation deadline phase action = do
  result <- waitControlled process cancellation deadline phase $ do
    terminal <- readTVar $ processStdoutTerminal process
    case terminal of
      Just failure -> pure $ Left failure
      Nothing -> Right <$> action
  pure $ result >>= id

cancellationSTM
  :: Z3SMTLibProcessCancellation
  -> Z3SMTLibProcessPhase
  -> STM (Either Z3SMTLibProcessError value)
cancellationSTM (Z3SMTLibProcessCancellation cancelled) phase = do
  value <- readTVar cancelled
  if value
    then pure $ Left $ processError phase Z3SMTLibProcessCancelled Nothing
    else retry

poisonSTM
  :: Z3SMTLibProcess
  -> STM (Either Z3SMTLibProcessError value)
poisonSTM process = do
  poisoned <- readTVar $ processPoison process
  case poisoned of
    Nothing -> retry
    Just failure -> pure $ Left failure

lifecycleSTM
  :: Z3SMTLibProcess
  -> Z3SMTLibProcessPhase
  -> STM (Either Z3SMTLibProcessError value)
lifecycleSTM process phase = do
  lifecycle <- readTVar $ processLifecycle process
  if lifecycle == ProcessOpen
    then retry
    else pure $ Left $ processError phase Z3SMTLibProcessClosed Nothing

checkCancellationDeadline
  :: Z3SMTLibProcessPhase
  -> Z3SMTLibProcessCancellation
  -> Z3SMTLibProcessDeadline
  -> IO (Either Z3SMTLibProcessError ())
checkCancellationDeadline phase
    (Z3SMTLibProcessCancellation cancelled)
    (Z3SMTLibProcessDeadline deadline) = do
  isCancelled <- atomically $ readTVar cancelled
  if isCancelled
    then pure $ Left $ processError phase Z3SMTLibProcessCancelled Nothing
    else do
      now <- getMonotonicTimeNSec
      pure $ if now >= deadline
        then Left $ processError phase Z3SMTLibProcessDeadlineExceeded Nothing
        else Right ()

remainingDeadlineMicroseconds
  :: Z3SMTLibProcessDeadline
  -> IO (Maybe Int)
remainingDeadlineMicroseconds (Z3SMTLibProcessDeadline deadline) = do
  now <- getMonotonicTimeNSec
  pure $ if now >= deadline
    then Nothing
    else Just $ fromIntegral $ min (toInteger (maxBound :: Int))
      $ (toInteger deadline - toInteger now + 999) `div` 1000

poisonOperation
  :: Z3SMTLibProcess
  -> Z3SMTLibProcessError
  -> IO (Either Z3SMTLibProcessError value)
poisonOperation process failure = do
  atomically $ setFirstPoisonSTM process failure
  pure $ Left failure

rejectWrite
  :: Z3SMTLibProcess
  -> Z3SMTLibProcessError
  -> IO (Either Z3SMTLibProcessError value)
rejectWrite process failure = do
  terminal <- atomically $ readTVar $ processStdoutTerminal process
  case terminal of
    Just known | known == failure -> pure $ Left failure
    _ -> poisonOperation process failure

poisonIfOpen :: Z3SMTLibProcess -> Z3SMTLibProcessError -> IO ()
poisonIfOpen process failure = atomically $ do
  lifecycle <- readTVar $ processLifecycle process
  when (lifecycle == ProcessOpen) $ setFirstPoisonSTM process failure

setFirstPoisonSTM
  :: Z3SMTLibProcess
  -> Z3SMTLibProcessError
  -> STM ()
setFirstPoisonSTM process failure = do
  current <- readTVar $ processPoison process
  case current of
    Nothing -> writeTVar (processPoison process) $ Just failure
    Just _ -> pure ()

setStdoutTerminalSTM
  :: Z3SMTLibProcess
  -> Z3SMTLibProcessError
  -> STM ()
setStdoutTerminalSTM process failure = do
  current <- readTVar $ processStdoutTerminal process
  case current of
    Just _ -> pure ()
    Nothing -> do
      writeTVar (processStdoutTerminal process) $ Just failure
      writeTQueue (processStdoutQueue process) $ StdoutTerminal failure

closeAfterOpenFailure
  :: Z3SMTLibProcess
  -> Z3SMTLibProcessError
  -> IO (Either Z3SMTLibProcessError Z3SMTLibProcess)
closeAfterOpenFailure process failure = do
  atomically $ setFirstPoisonSTM process failure
  cleanup <- closeZ3SMTLibProcess process
  pure $ Left $ attachCleanup cleanup failure

processError
  :: Z3SMTLibProcessPhase
  -> Z3SMTLibProcessFailureClass
  -> Maybe Natural
  -> Z3SMTLibProcessError
processError phase failure observed = Z3SMTLibProcessError
  { z3SMTLibProcessErrorPhase = phase
  , z3SMTLibProcessErrorClass = failure
  , z3SMTLibProcessErrorObservedAtLeast = observed
  , z3SMTLibProcessErrorCleanupStatus = Nothing
  }

attachCleanup
  :: Z3SMTLibProcessCleanupStatus
  -> Z3SMTLibProcessError
  -> Z3SMTLibProcessError
attachCleanup cleanup failure = failure
  { z3SMTLibProcessErrorCleanupStatus = Just cleanup }

incompleteCleanupStatus :: Z3SMTLibProcessCleanupStatus
incompleteCleanupStatus = Z3SMTLibProcessCleanupStatus
  { z3SMTLibProcessCleanupEscalation =
      Z3SMTLibProcessCleanupIncomplete
  , z3SMTLibProcessCleanupExitCode = Nothing
  , z3SMTLibProcessCleanupFailureCount = 1
  , z3SMTLibProcessCleanupReadersStopped = False
  }

boundedReadSize :: Int -> Natural -> Natural -> Int
boundedReadSize chunk maximumBytes observed = fromIntegral $ max 1
  $ min (fromIntegral chunk) $ if observed >= maximumBytes
      then 1
      else maximumBytes - observed + 1

executableSnapshotField
  :: Z3.Z3SMTLibExecutionProfile
  -> FilePath
  -> FilePath
  -> FilePath
  -> CapturedMetadata
  -> ByteString
  -> Natural
  -> Maybe ByteString
  -> FingerprintField
executableSnapshotField profile requestedCwd canonicalCwd canonicalExecutable
    metadata digest count expected = FingerprintTag
  (ascii "pre-spawn-path-executable-snapshot")
  [ FingerprintBytes $ BS.unpack z3SMTLibExecutableSnapshotStrengthTag
  , FingerprintTag (ascii "requested-executable-path")
      [textField $ Z3.z3SMTLibExecutionExecutablePath profile]
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
      [textField $ Z3.z3SMTLibExecutionExecutablePath profile]
  , FingerprintTag (ascii "spawn-argv")
      [FingerprintSequence $ map textField
        $ Z3.z3SMTLibExecutionConfiguredArgumentVector profile]
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

#ifdef DJEX_HAVE_DESCRIPTOR_BOUND_Z3_LAUNCH
descriptorBoundExecutableSnapshotField
  :: Z3.Z3SMTLibExecutionProfile
  -> FilePath
  -> FilePath
  -> DescriptorMetadata
  -> DescriptorMetadata
  -> ByteString
  -> Natural
  -> Maybe ByteString
  -> FingerprintField
descriptorBoundExecutableSnapshotField profile requestedCwd canonicalCwd
    sourceMetadata workingDirectoryMetadata digest count expected =
  FingerprintTag (ascii "descriptor-bound-sealed-main-image")
    [ FingerprintBytes
        $ BS.unpack z3SMTLibDescriptorBoundExecutableLaunchStrengthTag
    , FingerprintTag (ascii "requested-executable-path")
        [textField $ Z3.z3SMTLibExecutionExecutablePath profile]
    , FingerprintTag (ascii "opened-source-policy") $ map
        (FingerprintBytes . ascii)
        [ "read-only"
        , "close-on-exec"
        , "no-follow-final-symlink"
        , "nonblocking-open-before-regular-file-rejection"
        , "regular-file"
        , "at-least-one-execute-mode-bit"
        ]
    , FingerprintTag (ascii "source-before-after-consistent-fd-metadata")
        [descriptorMetadataField sourceMetadata]
    , FingerprintTag (ascii "sha256-of-bytes-copied-to-staged-image")
        [FingerprintBytes $ BS.unpack digest]
    , FingerprintNatural count
    , case expected of
        Nothing -> FingerprintTag (ascii "image-pin-absent") []
        Just pinned -> FingerprintTag (ascii "image-pin-matched")
          [FingerprintBytes $ BS.unpack pinned]
    , FingerprintTag (ascii "sealed-staged-image-policy") $ map
        (FingerprintBytes . ascii)
        [ "linux-memfd-create-close-on-exec-allow-sealing"
        , "ordinary-rwx-mode-bits-copied"
        , "setuid-setgid-and-file-capabilities-not-carried"
        , "seal-write-grow-shrink-seal-verified"
        , "regular-file-and-exact-size-verified"
        , "rewound-before-exec"
        ]
    , FingerprintTag (ascii "spawn-main-image") $ map
        (FingerprintBytes . ascii)
        [ "execveat-at-empty-path-sealed-staged-descriptor"
        , "no-pathname-exec-retry"
        , "main-image-bytes-only"
        , "loader-libraries-interpreter-and-solver-unbound"
        ]
    , FingerprintTag (ascii "spawn-argv-zero-exact-configured-path")
        [textField $ Z3.z3SMTLibExecutionExecutablePath profile]
    , FingerprintTag (ascii "spawn-arguments")
        [FingerprintSequence $ map textField
          $ Z3.z3SMTLibExecutionConfiguredArgumentVector profile]
    , FingerprintTag (ascii "spawn-empty-environment") []
    , FingerprintTag (ascii "requested-working-directory")
        [textField requestedCwd]
    , FingerprintTag (ascii "spawn-working-directory-fchdir-opened-fd")
        [ textField canonicalCwd
        , descriptorMetadataField workingDirectoryMetadata
        ]
    , FingerprintTag (ascii "spawn-pipe-and-fd-policy") $ map
        (FingerprintBytes . ascii)
        [ "three-close-on-exec-pipes"
        , "close-on-exec-error-handshake-pipe"
        , "close-every-unrelated-child-descriptor-before-exec"
        , "close-range-required-fail-closed-when-unavailable"
        , "process-package-interactive-process-lock"
        , "block-signals-across-fork-restore-parent-and-child-before-exec"
        , "child-process-group-pid"
        , "no-delegated-ctlc"
        ]
    ]

descriptorBoundEffectiveIDExecutableAccessSnapshotField
  :: Z3.Z3SMTLibExecutionProfile
  -> FilePath
  -> FilePath
  -> DescriptorMetadata
  -> DescriptorMetadata
  -> ByteString
  -> Natural
  -> Maybe ByteString
  -> FingerprintField
descriptorBoundEffectiveIDExecutableAccessSnapshotField
    profile requestedCwd canonicalCwd sourceMetadata
    workingDirectoryMetadata digest count expected =
  FingerprintTag
    (ascii "descriptor-bound-effective-id-executable-access-sealed-main-image")
    [ FingerprintBytes $ BS.unpack
        z3SMTLibDescriptorBoundEffectiveIDExecutableAccessLaunchStrengthTag
    , FingerprintTag (ascii "requested-executable-path")
        [textField $ Z3.z3SMTLibExecutionExecutablePath profile]
    , FingerprintTag (ascii "opened-source-policy") $ map
        (FingerprintBytes . ascii)
        [ "read-only"
        , "close-on-exec"
        , "no-follow-final-symlink"
        , "nonblocking-open-before-regular-file-rejection"
        , "regular-file"
        , "at-least-one-execute-mode-bit-shape-prefilter-not-authority"
        ]
    , FingerprintTag (ascii "source-effective-id-executable-access") $ map
        (FingerprintBytes . ascii)
        [ "raw-faccessat2-on-opened-source-descriptor"
        , "x-ok"
        , "at-empty-path"
        , "at-eaccess"
        , "before-copy"
        , "after-seal-and-test-hook-before-child-allocation"
        , "effective-filesystem-credentials-point-in-time-only"
        , "vfs-dac-posix-acl-source-mount-noexec-and-inode-permission-hooks"
        , "not-full-exec-bprm-lsm-ima-binfmt-interpreter-or-loader-authority"
        ]
    , FingerprintTag (ascii "source-before-after-consistent-fd-metadata")
        [descriptorMetadataField sourceMetadata]
    , FingerprintTag (ascii "sha256-of-bytes-copied-to-staged-image")
        [FingerprintBytes $ BS.unpack digest]
    , FingerprintNatural count
    , case expected of
        Nothing -> FingerprintTag (ascii "image-pin-absent") []
        Just pinned -> FingerprintTag (ascii "image-pin-matched")
          [FingerprintBytes $ BS.unpack pinned]
    , FingerprintTag (ascii "sealed-staged-image-policy") $ map
        (FingerprintBytes . ascii)
        [ "linux-memfd-create-close-on-exec-allow-sealing"
        , "fixed-owner-read-execute-mode-0500-not-source-metadata-authority"
        , "setuid-setgid-file-capabilities-acls-and-security-labels-not-carried"
        , "seal-write-grow-shrink-seal-verified"
        , "regular-file-exact-size-and-mode-0500-verified"
        , "rewound-before-exec"
        ]
    , FingerprintTag (ascii "spawn-main-image") $ map
        (FingerprintBytes . ascii)
        [ "execveat-at-empty-path-sealed-staged-descriptor"
        , "no-pathname-or-launch-strategy-retry"
        , "main-image-bytes-only"
        , "loader-libraries-interpreter-and-solver-unbound"
        ]
    , FingerprintTag (ascii "spawn-argv-zero-exact-configured-path")
        [textField $ Z3.z3SMTLibExecutionExecutablePath profile]
    , FingerprintTag (ascii "spawn-arguments")
        [FingerprintSequence $ map textField
          $ Z3.z3SMTLibExecutionConfiguredArgumentVector profile]
    , FingerprintTag (ascii "spawn-empty-environment") []
    , FingerprintTag (ascii "requested-working-directory")
        [textField requestedCwd]
    , FingerprintTag (ascii "spawn-working-directory-fchdir-opened-fd")
        [ textField canonicalCwd
        , descriptorMetadataField workingDirectoryMetadata
        ]
    , FingerprintTag (ascii "spawn-pipe-and-fd-policy") $ map
        (FingerprintBytes . ascii)
        [ "three-close-on-exec-pipes"
        , "close-on-exec-error-handshake-pipe"
        , "close-every-unrelated-child-descriptor-before-exec"
        , "close-range-required-fail-closed-when-unavailable"
        , "process-package-interactive-process-lock"
        , "block-signals-across-fork-restore-parent-and-child-before-exec"
        , "child-process-group-pid"
        , "no-delegated-ctlc"
        ]
    ]

descriptorMetadataField :: DescriptorMetadata -> FingerprintField
descriptorMetadataField
    (DescriptorMetadata device inode mode size links modified changed) =
  FingerprintTag (ascii "posix-fd-metadata")
    $ map integerTextField [device, inode, mode, size, links]
      ++ [textField modified, textField changed]
#endif

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

processObservationFingerprintFields
  :: ProcessExecutableLaunchStrategy
  -> Z3SMTLibProcessDeadline
  -> Z3SMTLibExecutableSnapshot
  -> FilePath
  -> FingerprintField
  -> [FingerprintField]
processObservationFingerprintFields
    strategy (Z3SMTLibProcessDeadline deadline)
    snapshot canonicalWorkingDirectory pidField = case strategy of
  ProcessPathSnapshotThenDirectSpawn ->
    [ z3SMTLibExecutableSnapshotFingerprintField snapshot
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
    ]
  ProcessDescriptorBoundExecutableLaunch ->
    [ z3SMTLibExecutableSnapshotFingerprintField snapshot
    , FingerprintTag (ascii "canonical-working-directory-observation")
        [textField canonicalWorkingDirectory]
    , workingDirectoryEmptinessObservationField
    , pidField
    , FingerprintTag
        (ascii "exec-error-cloexec-pipe-eof-observed-before-ready/v1") []
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
        (ascii $ concat
          [ "cleanup-wrapped-child-close-wait-group-term-with-direct-pid-"
          , "fallback-wait-group-kill-with-direct-pid-fallback-wait/v1"
          ]) []
    , FingerprintTag
        (ascii "descriptor-child-pid-is-process-group-id/v1") []
    , FingerprintTag (ascii "absolute-monotonic-deadline-nanoseconds")
        [FingerprintNatural $ fromIntegral deadline]
    ]
  ProcessDescriptorBoundEffectiveIDExecutableAccessLaunch ->
    [ z3SMTLibExecutableSnapshotFingerprintField snapshot
    , FingerprintTag (ascii "canonical-working-directory-observation")
        [textField canonicalWorkingDirectory]
    , workingDirectoryEmptinessObservationField
    , pidField
    , FingerprintTag
        (ascii "exec-error-cloexec-pipe-eof-observed-before-ready/v1") []
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
        (ascii $ concat
          [ "cleanup-wrapped-child-close-wait-group-term-with-direct-pid-"
          , "fallback-wait-group-kill-with-direct-pid-fallback-wait/v1"
          ]) []
    , FingerprintTag
        (ascii "descriptor-child-pid-is-process-group-id/v1") []
    , FingerprintTag (ascii "absolute-monotonic-deadline-nanoseconds")
        [FingerprintNatural $ fromIntegral deadline]
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

tryAny :: IO value -> IO (Either SomeException value)
tryAny = try
