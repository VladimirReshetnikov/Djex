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
  , LengthSMTLibReadyWorker
  , LengthSMTLibReadyWorkerIdentitySubject
  , withLengthSMTLibReadyWorker
  , sameLengthSMTLibReadyWorkerIdentity
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
  ( TVar
  , atomically
  , newTVarIO
  , readTVar
  , writeTVar
  )
import Control.DeepSeq (NFData (rnf))
import Control.Exception
  ( SomeException
  , mask
  , onException
  , try
  )
import qualified Crypto.Hash.SHA256 as SHA256
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (intToDigit, ord)
import Data.List (nub)
import Data.Word (Word8)
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
  ( LengthSMTLibExecutionConfig
  , lengthSMTLibExecutionPolicyFingerprint
  )
import Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol
  ( LengthSMTLibProtocolLimits
  , lengthSMTLibProtocolCumulativeStdoutByteLimit
  , lengthSMTLibProtocolStreamLimits
  )
import Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session.Capability
  ( LengthSMTLibCapabilityAction (..)
  , LengthSMTLibCapabilityError (..)
  , LengthSMTLibCapabilityLimits
  , LengthSMTLibCapabilityOutcome
  , LengthSMTLibCapabilityPlan
  , LengthSMTLibCapabilityPlanError
  , LengthSMTLibCapabilityWriteKind (..)
  , feedLengthSMTLibCapability
  , finishLengthSMTLibCapability
  , lengthSMTLibCapabilityCumulativeOutputByteLimit
  , lengthSMTLibCapabilityOutcomePlanFingerprint
  , lengthSMTLibCapabilityMinimumOutputByteCount
  , sealLengthSMTLibCapabilityPlan
  , startLengthSMTLibCapability
  )
import Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session.Process
  ( LengthSMTLibExecutableSnapshot
  , LengthSMTLibProcess
  , LengthSMTLibProcessCancellation
  , LengthSMTLibProcessCleanupEscalation (..)
  , LengthSMTLibProcessCleanupStatus (..)
  , LengthSMTLibProcessDeadline
  , LengthSMTLibProcessError (..)
  , LengthSMTLibProcessFailureClass (..)
  , LengthSMTLibProcessLimits
  , LengthSMTLibProcessPhase (..)
  , cancelLengthSMTLibProcess
  , checkLengthSMTLibProcessReady
  , closeLengthSMTLibProcess
  , drainLengthSMTLibProcessBoundaryWhitespace
  , lengthSMTLibExecutableSnapshotByteCount
  , lengthSMTLibExecutableSnapshotSHA256
  , lengthSMTLibExecutableSnapshotStrengthTag
  , lengthSMTLibProcessDeadlineAfterMilliseconds
  , lengthSMTLibProcessFingerprintField
  , lengthSMTLibProcessObservedStderrBytes
  , lengthSMTLibProcessObservedStdoutBytes
  , lengthSMTLibProcessStdoutByteLimit
  , lengthSMTLibProcessSnapshot
  , newLengthSMTLibProcessCancellation
  , nextLengthSMTLibProcessStdoutChunk
  , openLengthSMTLibProcess
  , runBeforeLengthSMTLibProcessDeadline
  , writeLengthSMTLibProcess
  )
import Language.Haskell.Synthesis.Internal.SMTLib.Stream
  ( smtLibStreamFrameByteLimit
  , smtLibStreamFramingSchemaTag
  , smtLibStreamNestingDepthLimit
  , smtLibStreamTotalByteLimit
  )

lengthSMTLibSessionSchemaTag :: [Word8]
lengthSMTLibSessionSchemaTag =
  ascii "djex-length-z3-scoped-worker-session/v2"

lengthSMTLibReadyWorkerSchemaTag :: [Word8]
lengthSMTLibReadyWorkerSchemaTag =
  ascii "djex-length-z3-capability-probed-ready-worker/v2"

-- | Each exclusive workspace attempt samples independent secret and public
-- 256-bit halves.  The public half names the directory; four barrier nonces
-- are SHA-256 domain-separated from the secret half and checked pairwise
-- distinct.  The child can observe its cwd without learning the barrier seed.
-- This is an OS-entropy/collision-check claim, not a proof of global uniqueness.
lengthSMTLibSessionEpochSchemaTag :: [Word8]
lengthSMTLibSessionEpochSchemaTag =
  ascii "os-entropy-512/split-public-label-secret-barrier-seed/v2"

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
  deriving (Bounded, Enum, Eq, Ord, Show)

data LengthSMTLibSessionLimitSource = LengthSMTLibSessionLimitSource
  { lengthSMTLibSessionLimitSourceOpenerDeadlineMilliseconds :: !Natural
  , lengthSMTLibSessionLimitSourceWorkspaceAllocationAttempts :: !Natural
  , lengthSMTLibSessionLimitSourceMaximumQueries :: !Natural
  , lengthSMTLibSessionLimitSourceIdentityFingerprintBytes :: !Natural
  }
  deriving (Eq, Ord, Show)

instance NFData LengthSMTLibSessionLimitSource where
  rnf source =
    rnf (lengthSMTLibSessionLimitSourceOpenerDeadlineMilliseconds source) `seq`
    rnf (lengthSMTLibSessionLimitSourceWorkspaceAllocationAttempts source) `seq`
    rnf (lengthSMTLibSessionLimitSourceMaximumQueries source) `seq`
    rnf (lengthSMTLibSessionLimitSourceIdentityFingerprintBytes source)

defaultLengthSMTLibSessionLimitSource :: LengthSMTLibSessionLimitSource
defaultLengthSMTLibSessionLimitSource = LengthSMTLibSessionLimitSource
  { lengthSMTLibSessionLimitSourceOpenerDeadlineMilliseconds = 5000
  , lengthSMTLibSessionLimitSourceWorkspaceAllocationAttempts = 8
  , lengthSMTLibSessionLimitSourceMaximumQueries = 64
  , lengthSMTLibSessionLimitSourceIdentityFingerprintBytes = 262144
  }

data LengthSMTLibSessionLimits = LengthSMTLibSessionLimits
  !Int !Natural !Natural !Natural

instance NFData LengthSMTLibSessionLimits where
  rnf (LengthSMTLibSessionLimits deadline attempts queries identity) =
    rnf deadline `seq` rnf attempts `seq` rnf queries `seq` rnf identity

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
  identity <- positive LengthSMTLibSessionIdentityFingerprintBytes
    $ lengthSMTLibSessionLimitSourceIdentityFingerprintBytes source
  pure $ LengthSMTLibSessionLimits deadline attempts queries identity
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
defaultLengthSMTLibSessionLimits = LengthSMTLibSessionLimits 5000 8 64 262144

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

data LengthSMTLibReadyWorkerIdentitySubject

data LengthSMTLibReadyWorker epoch = LengthSMTLibReadyWorker
  { readyWorkerProcess :: !LengthSMTLibProcess
  , readyWorkerCancellation :: !LengthSMTLibProcessCancellation
  , readyWorkerConfig :: !LengthSMTLibSessionConfig
  , readyWorkerIdentity
      :: !(Fingerprint LengthSMTLibReadyWorkerIdentitySubject)
  , readyWorkerSnapshot :: !LengthSMTLibExecutableSnapshot
  , readyWorkerTranscriptDigest :: !ByteString
  , readyWorkerTranscriptBytes :: !Natural
  , readyWorkerStdoutAtCommit :: !Natural
  , readyWorkerStderrAtCommit :: !Natural
  , readyWorkerWorkspace :: FilePath
  , readyWorkerBarrierSeed :: !ByteString
  , readyWorkerQueryCount :: !(TVar Natural)
  , readyWorkerQueryGate :: !(MVar ())
  }

type role LengthSMTLibReadyWorker nominal

sameLengthSMTLibReadyWorkerIdentity
  :: LengthSMTLibReadyWorker left
  -> LengthSMTLibReadyWorker right
  -> Bool
sameLengthSMTLibReadyWorkerIdentity left right =
  fingerprintCanonicalBytes (readyWorkerIdentity left) ==
    fingerprintCanonicalBytes (readyWorkerIdentity right)

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
  lengthSMTLibExecutableSnapshotSHA256 . readyWorkerSnapshot

lengthSMTLibReadyWorkerExecutableByteCount
  :: LengthSMTLibReadyWorker epoch
  -> Natural
lengthSMTLibReadyWorkerExecutableByteCount =
  lengthSMTLibExecutableSnapshotByteCount . readyWorkerSnapshot

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
lengthSMTLibReadyWorkerCapabilityTranscriptByteCount = readyWorkerTranscriptBytes

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

data TranscriptEpoch = TranscriptEpoch
  !LengthSMTLibCapabilityWriteKind !ByteString

-- | Open, probe, lend, and close exactly one worker.  Callback exceptions are
-- rethrown after durable cleanup has been started.  Expected runtime failures
-- retain no path, command, exception text, or barrier nonce; lexical failures
-- may retain a byte or count.  The nominal phantom separates worker epochs
-- statically; runtime lifecycle checks still reject any package-internal
-- existential wrapper used after the scope.
withLengthSMTLibReadyWorker
  :: forall result. LengthSMTLibSessionConfig
  -> (forall epoch. LengthSMTLibReadyWorker epoch -> IO result)
  -> IO (Either LengthSMTLibSessionScopeError result)
withLengthSMTLibReadyWorker config use = mask $ \restore -> do
  cancellation <- newLengthSMTLibProcessCancellation
  deadlineResult <- lengthSMTLibProcessDeadlineAfterMilliseconds openerDeadline
  case deadlineResult of
    Left failure -> pure $ Left $ scopeError
      (LengthSMTLibSessionDeadlineFailure failure) emptyCleanup
    Right deadline -> do
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
          preSpawn <- restore (inspectOwnedWorkspace workspace)
            `onException` protectWorkspace
          case preSpawn of
            Left failure -> do
              workspaceCleanup <- cleanupWorkspace sessionLimits workspace
              pure $ Left $ scopeError
                (LengthSMTLibSessionWorkspaceFailure failure)
                $ emptyCleanup
                  { lengthSMTLibSessionWorkspaceCleanupStatus = workspaceCleanup }
            Right () -> do
              opened <- restore (openLengthSMTLibProcess processLimits
                cancellation deadline execution $ workspacePath workspace)
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
 where
  LengthSMTLibSessionConfig sessionLimits processLimits capabilityLimits
      protocolLimits execution = config
  LengthSMTLibSessionLimits openerDeadline _ _ _ = sessionLimits

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
            let transcriptBytes = sum $ map transcriptEpochByteCount transcript
            if transcriptBytes /= stdoutCount
              then finishFailure workspace process
                $ LengthSMTLibSessionTranscriptAccountingMismatch
                    transcriptBytes stdoutCount
              else case buildReadyWorkerIdentity
                  sessionLimits protocolLimits execution process outcome
                  transcript workspace stdoutCount stderrCount of
              Left failure -> finishFailure workspace process failure
              Right identity -> do
                queryCount <- newTVarIO 0
                queryGate <- newMVar ()
                let transcriptDigest = SHA256.hash
                      $ BS.concat $ map transcriptEpochBytes transcript
                    worker = LengthSMTLibReadyWorker
                      { readyWorkerProcess = process
                      , readyWorkerCancellation = cancellation
                      , readyWorkerConfig = config
                      , readyWorkerIdentity = identity
                      , readyWorkerSnapshot = lengthSMTLibProcessSnapshot process
                      , readyWorkerTranscriptDigest = transcriptDigest
                      , readyWorkerTranscriptBytes = transcriptBytes
                      , readyWorkerStdoutAtCommit = stdoutCount
                      , readyWorkerStderrAtCommit = stderrCount
                      , readyWorkerWorkspace = workspacePath workspace
                      , readyWorkerBarrierSeed = workspaceEpoch workspace
                      , readyWorkerQueryCount = queryCount
                      , readyWorkerQueryGate = queryGate
                      }
                callbackResult <- use worker
                finalDeadline <- lengthSMTLibProcessDeadlineAfterMilliseconds
                  openerDeadline
                case finalDeadline of
                  Left failure -> finishFailure workspace process
                    $ LengthSMTLibSessionDeadlineFailure failure
                  Right checkedUntil -> do
                    stillReady <- checkLengthSMTLibProcessReady
                      process cancellation checkedUntil
                    case stillReady of
                      Left failure -> finishFailure workspace process
                        $ LengthSMTLibSessionProcessFailure failure
                      Right () -> finishSuccess workspace process callbackResult

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
    limits@(LengthSMTLibSessionLimits _ maximumAttempts _ _) recordCleanup = do
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
        (LengthSMTLibCapabilityOutcome epoch, [TranscriptEpoch]))
probeReadyWorker limits process cancellation deadline epoch = do
  let barriers = map (deriveBarrier epoch)
        [ "startup", "check", "input-value", "ready" ]
  if length (nub barriers) /= 4
    then pure $ Left LengthSMTLibSessionBarrierDerivationCollision
    else case barriers of
      [startup, check, value, ready] -> case sealLengthSMTLibCapabilityPlan limits
          (BS.unpack startup) (BS.unpack check) (BS.unpack value) (BS.unpack ready) of
        Left failure -> pure $ Left
          $ LengthSMTLibSessionCapabilityPlanFailure failure
        Right plan -> driveCapability limits process cancellation deadline
          $ startLengthSMTLibCapability plan
      _ -> pure $ Left LengthSMTLibSessionBarrierDerivationCollision

driveCapability
  :: LengthSMTLibCapabilityLimits
  -> LengthSMTLibProcess
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> LengthSMTLibCapabilityAction epoch
  -> IO
      (Either
        LengthSMTLibSessionError
        (LengthSMTLibCapabilityOutcome epoch, [TranscriptEpoch]))
driveCapability limits process cancellation deadline = go []
 where
  cumulativeMaximum =
    lengthSMTLibCapabilityCumulativeOutputByteLimit limits

  go completed action = case action of
    LengthSMTLibCapabilityWrite kind bytes receiver
      | null completed && kind == LengthSMTLibCapabilityStartupWrite -> do
          boundary <- checkLengthSMTLibProcessReady
            process cancellation deadline
          case boundary of
            Left failure -> pure $ Left
              $ LengthSMTLibSessionProcessFailure failure
            Right () -> issueWrite completed kind bytes receiver
      | null completed -> pure $ Left $ LengthSMTLibSessionProcessFailure
          $ internalProcessFailure LengthSMTLibProcessInternalFailure
      | otherwise -> do
          boundary <- collectBoundaryWhitespace completed
          case boundary of
            Left failure -> pure $ Left failure
            Right (whitespace, completed') ->
              case feedLengthSMTLibCapability receiver $ BS.unpack whitespace of
                Left failure -> pure $ Left
                  $ LengthSMTLibSessionCapabilityFailure failure
                Right (LengthSMTLibCapabilityAwait prepared) ->
                  issueWrite completed' kind bytes prepared
                Right _ -> pure $ Left $ LengthSMTLibSessionProcessFailure
                  $ internalProcessFailure LengthSMTLibProcessInternalFailure
    LengthSMTLibCapabilityAwait _ -> pure $ Left
      $ LengthSMTLibSessionProcessFailure $ internalProcessFailure
        LengthSMTLibProcessInternalFailure
    LengthSMTLibCapabilityComplete outcome ->
      completeAtBoundary completed outcome

  issueWrite completed kind bytes receiver = do
    written <- writeLengthSMTLibProcess process cancellation deadline
      $ BS.pack bytes
    case written of
      Left failure -> pure $ Left $ LengthSMTLibSessionProcessFailure failure
      Right () -> await completed kind [] (not $ null completed) receiver

  await completed kind chunks boundaryOpen receiver = do
    next <- nextLengthSMTLibProcessStdoutChunk process cancellation deadline
    case next of
      Left failure
        | lengthSMTLibProcessErrorClass failure == LengthSMTLibProcessStdoutEOF ->
            pure $ case finishLengthSMTLibCapability receiver of
              Left capabilityFailure -> Left
                $ LengthSMTLibSessionCapabilityFailure capabilityFailure
              Right _ -> Left $ LengthSMTLibSessionProcessFailure failure
        | otherwise -> pure $ Left $ LengthSMTLibSessionProcessFailure failure
      Right chunk -> do
        let (priorWhitespace, retainedChunk, nextBoundaryOpen) =
              splitBoundaryWhitespace boundaryOpen chunk
            nextCompleted = case appendLatest priorWhitespace completed of
              Nothing -> completed
              Just value -> value
            retained
              | BS.null retainedChunk = chunks
              | otherwise = retainedChunk : chunks
            epochRecord = TranscriptEpoch kind $ BS.concat $ reverse retained
        case feedLengthSMTLibCapability receiver $ BS.unpack chunk of
          Left failure -> pure $ Left
            $ LengthSMTLibSessionCapabilityFailure failure
          Right (LengthSMTLibCapabilityAwait nextReceiver) ->
            await nextCompleted kind retained nextBoundaryOpen nextReceiver
          Right nextAction@LengthSMTLibCapabilityWrite {} ->
            go (epochRecord : nextCompleted) nextAction
          Right (LengthSMTLibCapabilityComplete outcome) ->
            completeAtBoundary (epochRecord : nextCompleted) outcome

  completeAtBoundary completed outcome = do
    boundary <- collectBoundaryWhitespace completed
    case boundary of
      Left failure -> pure $ Left failure
      Right (_, completed') ->
        let observed = sum $ map transcriptEpochByteCount completed'
        in if observed > cumulativeMaximum
          then pure $ Left $ LengthSMTLibSessionCapabilityFailure
            $ LengthSMTLibCapabilityCumulativeOutputByteLimitExceeded
                cumulativeMaximum (cumulativeMaximum + 1)
          else pure $ Right (outcome, reverse completed')

  -- A reusable worker must delimit every final echo frame with at least one
  -- SMT-LIB whitespace byte.  If the framer completed on the closing quote,
  -- wait for that delimiter before admitting the next write.  This removes an
  -- OS-chunking race where a delayed LF could otherwise cross the write epoch.
  collectBoundaryWhitespace completed = case completed of
    [] -> pure $ Left $ LengthSMTLibSessionProcessFailure
      $ internalProcessFailure LengthSMTLibProcessInternalFailure
    TranscriptEpoch _ latest : _
      | endsInWhitespace latest -> drainMore BS.empty completed
      | otherwise -> do
          next <- nextLengthSMTLibProcessStdoutChunk
            process cancellation deadline
          case next of
            Left failure -> pure $ Left
              $ LengthSMTLibSessionProcessFailure failure
            Right bytes -> case firstNonWhitespace bytes of
              Just (offset, byte) -> pure $ Left
                $ LengthSMTLibSessionCapabilityFailure
                $ LengthSMTLibCapabilityUnexpectedPostBarrierByte
                    (sum (map transcriptEpochByteCount completed) + offset)
                    byte
              Nothing -> case appendLatest bytes completed of
                Nothing -> pure $ Left $ LengthSMTLibSessionProcessFailure
                  $ internalProcessFailure LengthSMTLibProcessInternalFailure
                Just completed' -> drainMore bytes completed'

  drainMore retained completed = do
    drained <- drainLengthSMTLibProcessBoundaryWhitespace
      process cancellation deadline
    case drained of
      Left failure -> pure $ Left $ LengthSMTLibSessionProcessFailure failure
      Right bytes -> case appendLatest bytes completed of
        Nothing -> pure $ Left $ LengthSMTLibSessionProcessFailure
          $ internalProcessFailure LengthSMTLibProcessInternalFailure
        Just completed' -> pure $ Right (retained <> bytes, completed')

  appendLatest bytes epochs = case epochs of
    [] -> Nothing
    TranscriptEpoch kind previous : rest -> Just
      $ TranscriptEpoch kind (previous <> bytes) : rest

  splitBoundaryWhitespace boundaryOpen bytes
    | not boundaryOpen = (BS.empty, bytes, False)
    | otherwise =
        let (prefix, suffix) = BS.span isSMTLibWhitespaceByte bytes
        in (prefix, suffix, BS.null suffix)

  endsInWhitespace bytes = case BS.unsnoc bytes of
    Nothing -> False
    Just (_, byte) -> isSMTLibWhitespaceByte byte

  firstNonWhitespace bytes = case BS.findIndex
      (not . isSMTLibWhitespaceByte) bytes of
    Nothing -> Nothing
    Just offset -> Just
      (fromIntegral offset, BS.index bytes offset)

buildReadyWorkerIdentity
  :: LengthSMTLibSessionLimits
  -> LengthSMTLibProtocolLimits
  -> LengthSMTLibExecutionConfig
  -> LengthSMTLibProcess
  -> LengthSMTLibCapabilityOutcome epoch
  -> [TranscriptEpoch]
  -> Workspace
  -> Natural
  -> Natural
  -> Either
      LengthSMTLibSessionError
      (Fingerprint LengthSMTLibReadyWorkerIdentitySubject)
buildReadyWorkerIdentity
    (LengthSMTLibSessionLimits opener _ maximumQueries maximumBytes)
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
              [ FingerprintBytes $ ascii
                  "leading-boundary-whitespace-to-preceding-write/v1"
              , FingerprintSequence $ map transcriptEpochField transcript
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
protocolLimitsField limits = tagged "future-query-protocol-policy"
  [ FingerprintBytes smtLibStreamFramingSchemaTag
  , FingerprintNatural $ smtLibStreamTotalByteLimit stream
  , FingerprintNatural $ smtLibStreamFrameByteLimit stream
  , FingerprintNatural $ smtLibStreamNestingDepthLimit stream
  , FingerprintNatural $ lengthSMTLibProtocolCumulativeStdoutByteLimit limits
  ]
 where
  stream = lengthSMTLibProtocolStreamLimits limits

transcriptEpochField :: TranscriptEpoch -> FingerprintField
transcriptEpochField (TranscriptEpoch kind bytes) = tagged "write-epoch"
  [ capabilityWriteKindField kind
  , FingerprintBytes $ BS.unpack bytes
  ]

capabilityWriteKindField :: LengthSMTLibCapabilityWriteKind -> FingerprintField
capabilityWriteKindField kind = FingerprintBytes $ ascii $ case kind of
  LengthSMTLibCapabilityStartupWrite -> "startup"
  LengthSMTLibCapabilityCheckWrite -> "check"
  LengthSMTLibCapabilityInputValueWrite -> "input-value"
  LengthSMTLibCapabilityReadyWrite -> "ready"

transcriptEpochBytes :: TranscriptEpoch -> ByteString
transcriptEpochBytes (TranscriptEpoch _ bytes) = bytes

transcriptEpochByteCount :: TranscriptEpoch -> Natural
transcriptEpochByteCount = fromIntegral . BS.length . transcriptEpochBytes

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

isSMTLibWhitespaceByte :: Word8 -> Bool
isSMTLibWhitespaceByte byte =
  byte == 9 || byte == 10 || byte == 13 || byte == 32
