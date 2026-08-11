{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module SMTLibLiveSpec (smtLibLiveTests) where

import Control.Concurrent (forkIO, myThreadId, throwTo)
import Control.Concurrent.MVar
  ( newEmptyMVar
  , putMVar
  , takeMVar
  )
import Control.Exception
  ( Exception
  , bracket
  , finally
  , throwIO
  , try
  )
import Control.Monad (forM, forM_)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (ord)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Word (Word8)
import Numeric.Natural (Natural)
import System.Directory
  ( canonicalizePath
  , copyFile
  , createDirectory
  , createDirectoryLink
  , doesDirectoryExist
  , doesFileExist
  , doesPathExist
  , findExecutable
  , getPermissions
  , getTemporaryDirectory
  , listDirectory
  , pathIsSymbolicLink
  , removeDirectory
  , removeFile
  , removePathForcibly
  , renameDirectory
  , setOwnerExecutable
  , setPermissions
  )
import System.FilePath ((</>), takeFileName)
import qualified System.Info as SystemInfo
import System.IO (hClose, openTempFile)
import System.IO.Error (tryIOError)
import System.Timeout (timeout)

import qualified Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Execution
  as Execution
import qualified Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol
  as Protocol
import qualified Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session
  as Session
import qualified Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session.Capability
  as Capability
import qualified Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session.Process
  as Process
import qualified Language.Haskell.Synthesis.Internal.SMTLib.Stream
  as SMTLibStream
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool
  , assertFailure
  , testCase
  , (@?=)
  )

smtLibLiveTests :: TestTree
smtLibLiveTests = testGroup "Length SMT-LIB live worker checkpoint"
  [ capabilityTests
  , rawProcessTests
  , liveSessionTests
  ]

capabilityTests :: TestTree
capabilityTests = testGroup "pure capability handshake"
  [ healthyChunkTests
  , testCase "publish exact schemas, defaults, and admission minima"
      assertCapabilitySchemasAndAdmission
  , testCase "reject malformed and every pairwise-reused nonce"
      assertCapabilityNonceAdmission
  , testCase "reject exact-response, barrier, and EOF failures by phase"
      assertCapabilityPhaseFailures
  , testCase "reject model and readiness output before their writes"
      assertCapabilityCausalBoundaries
  , testCase "enforce cumulative maximum and frame/cumulative tie precedence"
      assertCapabilityAccounting
  , testCase "stop at the first bad frame without scanning for recovery"
      assertCapabilityNoScanning
  ]

healthyChunkTests :: TestTree
healthyChunkTests = testGroup "exact four-write transcript across chunking"
  [ testCase name $ do
      fixture <- defaultCapabilityFixture
      assertHealthyCapability fixture chunker
  | (name, chunker) <-
      [ ("whole response groups", wholeChunks)
      , ("selected internal splits", selectedChunks)
      , ("singleton bytes", singletonChunks)
      , ("empty chunks interleaved", emptyInterleavedChunks)
      ]
  ]

rawProcessTests :: TestTree
rawProcessTests = testGroup "raw bounded Process transport"
  [ testCase "stdout limit preserves prefix before terminal across read chunks"
      assertRawProcessStdoutLimitPrefix
  ]

assertRawProcessStdoutLimitPrefix :: IO ()
assertRawProcessStdoutLimitPrefix = do
  observations <- forM [4, 1] $ \readChunkBytes ->
    withFakeZ3Mode "healthy" $ \executable _ ->
      withFreshTestDirectory $ \workingDirectory -> do
        execution <- liveExecutionConfig executable Nothing
        limits <- expectRight $ Process.mkLengthSMTLibProcessLimits
          $ Process.defaultLengthSMTLibProcessLimitSource
              { Process.lengthSMTLibProcessLimitSourceStdoutBytes = 3
              , Process.lengthSMTLibProcessLimitSourceReadChunkBytes =
                  readChunkBytes
              }
        cancellation <- Process.newLengthSMTLibProcessCancellation
        deadline <- expectRight =<<
          Process.lengthSMTLibProcessDeadlineAfterMilliseconds 3000
        process <- expectRight =<< Process.openLengthSMTLibProcess
          limits cancellation deadline execution workingDirectory
        (prefix, terminal) <- collectRawProcessPrefix
          process cancellation deadline
        rejected <- Process.writeLengthSMTLibProcess
          process cancellation deadline "(check-sat)\n"
        case rejected of
          Left failure -> failure @?= terminal
          Right () -> assertFailure
            "stdout-limit terminal admitted a subsequent write"
        observed <- Process.lengthSMTLibProcessObservedStdoutBytes process
        cleanup <- Process.closeLengthSMTLibProcess process
        assertBool "stdout-limit Process cleanup remained incomplete"
          $ Process.lengthSMTLibProcessCleanupEscalation cleanup /=
              Process.LengthSMTLibProcessCleanupIncomplete
        Process.lengthSMTLibProcessCleanupFailureCount cleanup @?= 0
        Process.lengthSMTLibProcessCleanupReadersStopped cleanup @?= True
        pure (prefix, terminal, observed)
  forM_ observations $ \(prefix, terminal, observed) -> do
    prefix @?= "\"ca"
    Process.lengthSMTLibProcessErrorPhase terminal @?=
      Process.LengthSMTLibProcessStdoutPhase
    Process.lengthSMTLibProcessErrorClass terminal @?=
      Process.LengthSMTLibProcessStdoutByteLimitExceeded
    Process.lengthSMTLibProcessErrorObservedAtLeast terminal @?= Just 4
    observed @?= 4
  case observations of
    [(_, wholeTerminal, _), (_, singletonTerminal, _)] ->
      wholeTerminal @?= singletonTerminal
    _ -> assertFailure "internal stdout-limit test matrix changed shape"

collectRawProcessPrefix
  :: Process.LengthSMTLibProcess
  -> Process.LengthSMTLibProcessCancellation
  -> Process.LengthSMTLibProcessDeadline
  -> IO (ByteString, Process.LengthSMTLibProcessError)
collectRawProcessPrefix process cancellation deadline = do
  written <- Process.writeLengthSMTLibProcess
    process cancellation deadline "(echo \"cap\")\n"
  _ <- expectRight written
  go []
 where
  go chunks = do
    received <- Process.nextLengthSMTLibProcessStdoutChunk
      process cancellation deadline
    case received of
      Right chunk -> go $ chunk : chunks
      Left terminal -> pure (BS.concat $ reverse chunks, terminal)

liveSessionTests :: TestTree
liveSessionTests = testGroup "live Session integration"
  [ testCase "open healthy worker with exact launch isolation and cleanup"
      assertLiveHealthyIsolation
  , testCase "accept matching executable digest pin"
      assertLiveMatchingDigest
  , testCase "complete capability probe with split and drip output"
      assertLiveChunkModes
  , testCase "reject digest mismatch before spawning"
      assertLiveDigestMismatch
  , testCase "reject exact capability fault modes"
      assertLiveCapabilityFaults
  , liveStderrFaultTests
  , liveTerminationFaultTests
  , testCase "bound cleanup of a worker stubborn after stdin EOF"
      assertLiveStubbornCleanup
  , testCase "enforce executable snapshot byte cap at exact and maximum plus one"
      assertLiveExecutableByteCap
  , testCase "distinguish independently opened worker identities"
      assertLiveIdentityFreshness
  , posixWorkspaceReplacementTest
  , callbackExceptionTests
  ]

assertLiveHealthyIsolation :: IO ()
assertLiveHealthyIsolation = withFakeZ3Mode "healthy" $ \executable image -> do
  config <- liveSessionConfig executable Nothing id id
  scoped <- runReadyWorkerBounded 6000000 config $ \worker -> do
    let workspace = Session.lengthSMTLibReadyWorkerWorkingDirectory worker
        tracePath = fakeZ3EventPath executable
    present <- doesDirectoryExist workspace
    assertBool "live callback received a missing workspace" present
    tracePresent <- doesPathExist tracePath
    assertBool "fake worker did not create its event trace" tracePresent
    events <- expectRight . parseFakeZ3Events =<< BS.readFile tracePath
    start <- expectFakeZ3Event "start" events
    assertSingleFakeField "trace-schema" "length-delimited/v1" start
    assertSingleFakeField "mode" "healthy" start
    assertSingleFakeField "executable-basename"
      (utf8Bytes $ takeFileName executable) start
    assertSingleFakeField "argv-count" "5" start
    fakeZ3FieldValues "argv" start @?= map utf8Bytes liveArgumentVector
    assertSingleFakeField "environment-count" "0" start
    fakeZ3FieldValues "environment-name" start @?= []
    fakeZ3FieldValues "environment-value" start @?= []
    assertSingleFakeField "cwd" (utf8Bytes workspace) start
    assertSingleFakeField "initial-listing-count" "0" start
    fakeZ3FieldValues "initial-listing-entry" start @?= []
    entries <- listDirectory workspace
    entries @?= []
    Session.lengthSMTLibReadyWorkerExecutableSHA256 worker @?=
      SHA256.hash image
    Session.lengthSMTLibReadyWorkerExecutableByteCount worker @?=
      byteStringLength image
    Session.lengthSMTLibReadyWorkerObservedStderrBytes worker @?= 0
    assertBool "capability probe observed no stdout"
      $ Session.lengthSMTLibReadyWorkerObservedStdoutBytes worker > 0
    Session.lengthSMTLibReadyWorkerCapabilityTranscriptByteCount worker @?=
      Session.lengthSMTLibReadyWorkerObservedStdoutBytes worker
    BS.length (Session.lengthSMTLibReadyWorkerCapabilityTranscriptSHA256 worker)
      @?= 32
    pure workspace
  workspace <- expectRight scoped
  retained <- doesPathExist workspace
  assertBool "successful Session scope retained its workspace" $ not retained

assertLiveMatchingDigest :: IO ()
assertLiveMatchingDigest = withFakeZ3Mode "healthy" $ \executable image -> do
  let digest = SHA256.hash image
  config <- liveSessionConfig executable (Just digest) id id
  scoped <- runReadyWorkerBounded 6000000 config $ \worker -> do
    Session.lengthSMTLibReadyWorkerExecutableSHA256 worker @?= digest
  _ <- expectRight scoped
  pure ()

assertLiveChunkModes :: IO ()
assertLiveChunkModes = forM_ ["split-output", "drip-output"] $ \mode ->
  withFakeZ3Mode mode $ \executable _ -> do
    config <- liveSessionConfig executable Nothing id id
    scoped <- runReadyWorkerBounded 7000000 config $ \worker -> do
      Session.lengthSMTLibReadyWorkerObservedStderrBytes worker @?= 0
      assertBool (mode ++ " produced no capability transcript")
        $ Session.lengthSMTLibReadyWorkerCapabilityTranscriptByteCount worker > 0
      Session.lengthSMTLibReadyWorkerCapabilityTranscriptByteCount worker @?=
        Session.lengthSMTLibReadyWorkerObservedStdoutBytes worker
      pure $ Session.lengthSMTLibReadyWorkerWorkingDirectory worker
    workspace <- expectRight scoped
    retained <- doesPathExist workspace
    assertBool (mode ++ " retained its workspace") $ not retained

assertLiveDigestMismatch :: IO ()
assertLiveDigestMismatch = withFakeZ3Mode "healthy" $ \executable image -> do
  let mismatched = alterDigest $ SHA256.hash image
  config <- liveSessionConfig executable (Just mismatched) id id
  scoped <- runReadyWorkerBounded 6000000 config $ \_ ->
    assertFailure "digest mismatch reached the worker callback"
  cleanup <- expectProcessScopeFailure
    Process.LengthSMTLibProcessSnapshotPhase
    Process.LengthSMTLibProcessExecutableDigestMismatch
    Nothing scoped
  spawned <- doesPathExist $ fakeZ3EventPath executable
  assertBool "digest mismatch spawned the fake worker" $ not spawned
  Session.lengthSMTLibSessionProcessCleanupStatus cleanup @?= Nothing
  Session.lengthSMTLibSessionWorkspaceCleanupStatus cleanup @?=
    Session.LengthSMTLibSessionWorkspaceRemoved 0

assertLiveCapabilityFaults :: IO ()
assertLiveCapabilityFaults = forM_ faultCases $ \(mode, expected) ->
  withFakeZ3Mode mode $ \executable _ -> do
    config <- liveSessionConfig executable Nothing id id
    scoped <- runReadyWorkerBounded 6000000 config $ \_ ->
      assertFailure $ mode ++ " reached the worker callback"
    cleanup <- expectCapabilityScopeFailure expected scoped
    assertWorkspaceWasRemoved mode cleanup
 where
  faultCases =
    [ ( "wrong-echo"
      , Capability.LengthSMTLibCapabilityBarrierMismatch
          Capability.LengthSMTLibCapabilityStartupBarrier
      )
    , ( "wrong-status"
      , Capability.LengthSMTLibCapabilityUnexpectedExactResponse
          Capability.LengthSMTLibCapabilityCheckStatusPhase
      )
    , ( "wrong-value"
      , Capability.LengthSMTLibCapabilityUnexpectedExactResponse
          Capability.LengthSMTLibCapabilityInputValuePhase
      )
    , ( "unsolicited-reset-success"
      , Capability.LengthSMTLibCapabilityUnexpectedExactResponse
          Capability.LengthSMTLibCapabilityCheckStatusPhase
      )
    ]

liveStderrFaultTests :: TestTree
liveStderrFaultTests = testGroup
  "poison on one stderr byte and a finite pipe-sized flood"
  [testCase mode $ assertLiveStderrFault mode
  | mode <- ["stderr-byte", "stderr-flood"]]

assertLiveStderrFault :: String -> IO ()
assertLiveStderrFault mode =
  withFakeZ3Mode mode $ \executable _ -> do
    config <- liveSessionConfig executable Nothing id id
    scoped <- runReadyWorkerBounded 6000000 config $ \_ ->
      assertFailure $ mode ++ " reached the worker callback"
    cleanup <- expectObservedStderrFailure mode scoped
    assertWorkspaceWasRemoved mode cleanup

liveTerminationFaultTests :: TestTree
liveTerminationFaultTests = testGroup
  "bound EOF, exit, closed stdout, and deadline failures"
  ( [ testCase mode $ assertLiveTerminationFault mode
    | mode <-
        [ "immediate-eof"
        , "immediate-exit-nonzero"
        , "stdout-eof-hang"
        ]
    ]
    ++ [testCase "deadline" assertLiveDeadlineFault]
  )

assertLiveTerminationFault :: String -> IO ()
assertLiveTerminationFault mode =
  withFakeZ3Mode mode $ \executable _ -> do
    config <- liveSessionConfig executable Nothing id id
    scoped <- runReadyWorkerBounded 6000000 config $ \_ ->
      assertFailure $ mode ++ " reached the worker callback"
    cleanup <- expectTerminationFailure mode scoped
    assertWorkspaceWasRemoved mode cleanup

assertLiveDeadlineFault :: IO ()
assertLiveDeadlineFault =
  withFakeZ3Mode "hang" $ \executable _ -> do
    config <- liveSessionConfig executable Nothing id $ \source -> source
      { Session.lengthSMTLibSessionLimitSourceOpenerDeadlineMilliseconds =
          1000 }
    scoped <- runReadyWorkerBounded 5000000 config $ \_ ->
      assertFailure "hung worker reached the callback"
    cleanup <- expectDeadlineScopeFailure scoped
    assertWorkspaceWasRemoved "hang" cleanup

assertLiveStubbornCleanup :: IO ()
assertLiveStubbornCleanup = withFakeZ3Mode "stubborn-eof"
  $ \executable _ -> do
    config <- liveSessionConfig executable Nothing id id
    scoped <- runReadyWorkerBounded 5000000 config $ \worker -> do
      present <- doesDirectoryExist
        $ Session.lengthSMTLibReadyWorkerWorkingDirectory worker
      assertBool "stubborn worker callback workspace was missing" present
    _ <- expectRight scoped
    pure ()

assertLiveExecutableByteCap :: IO ()
assertLiveExecutableByteCap = withFakeZ3Mode "healthy"
  $ \executable image -> do
    let imageBytes = byteStringLength image
        withMaximum maximumBytes source = source
          { Process.lengthSMTLibProcessLimitSourceExecutableBytes =
              maximumBytes }
    assertBool "fake executable unexpectedly had at most one byte"
      $ imageBytes > 1
    exactConfig <- liveSessionConfig executable Nothing
      (withMaximum imageBytes) id
    exact <- runReadyWorkerBounded 6000000 exactConfig $ \worker -> pure
      $ Session.lengthSMTLibReadyWorkerExecutableByteCount worker
    observed <- expectRight exact
    observed @?= imageBytes
    removeFile $ fakeZ3EventPath executable

    let admitted = imageBytes - 1
    overflowConfig <- liveSessionConfig executable Nothing
      (withMaximum admitted) id
    overflow <- runReadyWorkerBounded 6000000 overflowConfig $ \_ ->
      assertFailure "maximum-plus-one executable reached the callback"
    cleanup <- expectProcessScopeFailure
      Process.LengthSMTLibProcessSnapshotPhase
      Process.LengthSMTLibProcessExecutableByteLimitExceeded
      (Just imageBytes) overflow
    spawned <- doesPathExist $ fakeZ3EventPath executable
    assertBool "executable maximum-plus-one failure spawned the fake worker"
      $ not spawned
    Session.lengthSMTLibSessionProcessCleanupStatus cleanup @?= Nothing
    Session.lengthSMTLibSessionWorkspaceCleanupStatus cleanup @?=
      Session.LengthSMTLibSessionWorkspaceRemoved 0

assertLiveIdentityFreshness :: IO ()
assertLiveIdentityFreshness = withFakeZ3Mode "healthy"
  $ \executable _ -> do
    config <- liveSessionConfig executable Nothing id id
    first <- expectRight =<< runReadyWorkerBounded 6000000 config
      (pure . Session.lengthSMTLibReadyWorkerIdentityFingerprint)
    second <- expectRight =<< runReadyWorkerBounded 6000000 config
      (pure . Session.lengthSMTLibReadyWorkerIdentityFingerprint)
    assertBool "two independently opened workers shared an identity"
      $ first /= second

posixWorkspaceReplacementTest :: TestTree
posixWorkspaceReplacementTest
  | SystemInfo.os == "mingw32" = testGroup
      "POSIX workspace replacement (not available on Windows)" []
  | otherwise = testCase
      "retain a replaced callback root without following its symlink"
      assertLivePosixWorkspaceReplacement

assertLivePosixWorkspaceReplacement :: IO ()
assertLivePosixWorkspaceReplacement =
  withFakeZ3Mode "healthy" $ \executable _ ->
    withFreshTestDirectory $ \attackRoot -> do
      let externalDirectory = attackRoot </> "external"
          sentinelPath = externalDirectory </> "sentinel"
          movedWorkspace = attackRoot </> "moved-workspace"
      createDirectory externalDirectory
      BS.writeFile sentinelPath "external-sentinel"
      workspace <- bracket
        (newIORef Nothing)
        (cleanupWorkspaceReplacement
          movedWorkspace externalDirectory sentinelPath)
        $ \workspaceRef -> do
            config <- liveSessionConfig executable Nothing id id
            scoped <- runReadyWorkerBounded 6000000 config $ \worker -> do
              let original =
                    Session.lengthSMTLibReadyWorkerWorkingDirectory worker
              writeIORef workspaceRef $ Just original
              renameDirectory original movedWorkspace
              createDirectoryLink externalDirectory original
            original <- readIORef workspaceRef >>= maybe
              (assertFailure "workspace replacement callback was not reached")
              pure
            case scoped of
              Left scope -> do
                Session.lengthSMTLibSessionScopePrimaryError scope @?=
                  Session.LengthSMTLibSessionCleanupFailure
                Session.lengthSMTLibSessionWorkspaceCleanupStatus
                    (Session.lengthSMTLibSessionScopeCleanupStatus scope) @?=
                  Session.LengthSMTLibSessionWorkspaceRetained
                    Session.LengthSMTLibSessionWorkspacePostconditionFailed
                    Nothing
              Right () -> assertFailure
                "replaced workspace root passed Session cleanup"
            linked <- pathIsSymbolicLink original
            assertBool "Session removed the substituted workspace symlink"
              linked
            moved <- doesDirectoryExist movedWorkspace
            assertBool "Session removed the descriptor-owned moved workspace"
              moved
            sentinel <- BS.readFile sentinelPath
            sentinel @?= "external-sentinel"
            pure original
      oldPathPresent <- doesPathExist workspace
      oldPathLink <- symbolicLinkExists workspace
      movedPathPresent <- doesPathExist movedWorkspace
      externalPresent <- doesPathExist externalDirectory
      assertBool "test cleanup retained the substituted workspace path"
        $ not oldPathPresent && not oldPathLink
      assertBool "test cleanup retained the moved workspace"
        $ not movedPathPresent
      assertBool "test cleanup retained the external sentinel directory"
        $ not externalPresent

cleanupWorkspaceReplacement
  :: FilePath
  -> FilePath
  -> FilePath
  -> IORef (Maybe FilePath)
  -> IO ()
cleanupWorkspaceReplacement movedWorkspace externalDirectory sentinelPath
    workspaceRef = do
  workspace <- readIORef workspaceRef
  case workspace of
    Nothing -> pure ()
    Just original -> do
      linked <- symbolicLinkExists original
      if linked
        then ignoreIOError $ removeFile original
        else pure ()
  moved <- doesDirectoryExist movedWorkspace
  if moved
    then ignoreIOError $ removeDirectory movedWorkspace
    else pure ()
  sentinel <- doesFileExist sentinelPath
  if sentinel
    then ignoreIOError $ removeFile sentinelPath
    else pure ()
  external <- doesDirectoryExist externalDirectory
  if external
    then ignoreIOError $ removeDirectory externalDirectory
    else pure ()

symbolicLinkExists :: FilePath -> IO Bool
symbolicLinkExists path = do
  observed <- tryIOError $ pathIsSymbolicLink path
  pure $ case observed of
    Left _ -> False
    Right linked -> linked

ignoreIOError :: IO value -> IO ()
ignoreIOError action = do
  _ <- tryIOError action
  pure ()

callbackExceptionTests :: TestTree
callbackExceptionTests = testGroup "callback exception cleanup"
  [ testCase "rethrow a synchronous exception after removing the workspace"
      assertLiveSynchronousCallbackException
  , testCase "rethrow a throwTo exception after removing the workspace"
      assertLiveAsynchronousCallbackException
  ]

data LiveSynchronousCallbackException = LiveSynchronousCallbackException
  deriving (Eq, Show)

instance Exception LiveSynchronousCallbackException

data LiveAsynchronousCallbackException = LiveAsynchronousCallbackException
  deriving (Eq, Show)

instance Exception LiveAsynchronousCallbackException

assertLiveSynchronousCallbackException :: IO ()
assertLiveSynchronousCallbackException =
  withFakeZ3Mode "healthy" $ \executable _ -> do
    workspaceRef <- newIORef Nothing
    config <- liveSessionConfig executable Nothing id id
    attempted <- try $ runReadyWorkerBounded 6000000 config $ \worker -> do
      writeIORef workspaceRef $ Just
        $ Session.lengthSMTLibReadyWorkerWorkingDirectory worker
      throwIO LiveSynchronousCallbackException
    case attempted of
      Left failure -> failure @?= LiveSynchronousCallbackException
      Right _ -> assertFailure
        "synchronous callback exception was not rethrown"
    assertCapturedWorkspaceRemoved
      "synchronous callback exception" workspaceRef

assertLiveAsynchronousCallbackException :: IO ()
assertLiveAsynchronousCallbackException =
  withFakeZ3Mode "healthy" $ \executable _ -> do
    workspaceRef <- newIORef Nothing
    target <- myThreadId
    throwerFinished <- newEmptyMVar
    callbackBlock <- newEmptyMVar
    config <- liveSessionConfig executable Nothing id id
    attempted <- try $ runReadyWorkerBounded 6000000 config $ \worker -> do
      writeIORef workspaceRef $ Just
        $ Session.lengthSMTLibReadyWorkerWorkingDirectory worker
      _ <- forkIO $ throwTo target LiveAsynchronousCallbackException
        `finally` putMVar throwerFinished ()
      takeMVar callbackBlock
    case attempted of
      Left failure -> failure @?= LiveAsynchronousCallbackException
      Right _ -> assertFailure "throwTo callback exception was not rethrown"
    thrower <- timeout 1000000 $ takeMVar throwerFinished
    case thrower of
      Nothing -> assertFailure "throwTo callback helper did not terminate"
      Just () -> pure ()
    assertCapturedWorkspaceRemoved "throwTo callback exception" workspaceRef

assertCapturedWorkspaceRemoved
  :: String
  -> IORef (Maybe FilePath)
  -> IO ()
assertCapturedWorkspaceRemoved label workspaceRef = do
  workspace <- readIORef workspaceRef >>= maybe
    (assertFailure $ label ++ " callback was not reached") pure
  retained <- doesPathExist workspace
  assertBool (label ++ " retained its Session workspace") $ not retained

liveSessionConfig
  :: FilePath
  -> Maybe ByteString
  -> (Process.LengthSMTLibProcessLimitSource
      -> Process.LengthSMTLibProcessLimitSource)
  -> (Session.LengthSMTLibSessionLimitSource
      -> Session.LengthSMTLibSessionLimitSource)
  -> IO Session.LengthSMTLibSessionConfig
liveSessionConfig executable expectedDigest changeProcess changeSession = do
  execution <- liveExecutionConfig executable expectedDigest
  process <- expectRight $ Process.mkLengthSMTLibProcessLimits
    $ changeProcess Process.defaultLengthSMTLibProcessLimitSource
  session <- expectRight $ Session.mkLengthSMTLibSessionLimits
    $ changeSession
    $ Session.defaultLengthSMTLibSessionLimitSource
      { Session.lengthSMTLibSessionLimitSourceOpenerDeadlineMilliseconds =
          3000 }
  expectRight $ Session.sealLengthSMTLibSessionConfig
    session process Capability.defaultLengthSMTLibCapabilityLimits
    Protocol.defaultLengthSMTLibProtocolLimits execution

liveExecutionConfig
  :: FilePath
  -> Maybe ByteString
  -> IO Execution.LengthSMTLibExecutionConfig
liveExecutionConfig executable expectedDigest =
  expectRight $ Execution.mkLengthSMTLibExecutionConfig
    Execution.defaultLengthSMTLibExecutionLimits
    $ (Execution.defaultLengthSMTLibExecutionConfigSource executable
        $ BS.unpack <$> expectedDigest)
      { Execution.lengthSMTLibExecutionConfigSourceSolverTimeoutMilliseconds =
          liveSolverTimeoutMilliseconds
      , Execution.lengthSMTLibExecutionConfigSourceSolverResourceLimit =
          liveSolverResourceLimit
      , Execution.lengthSMTLibExecutionConfigSourceHostDeadlineMilliseconds =
          liveHostDeadlineMilliseconds
      }

runReadyWorkerBounded
  :: Int
  -> Session.LengthSMTLibSessionConfig
  -> (forall epoch. Session.LengthSMTLibReadyWorker epoch -> IO result)
  -> IO (Either Session.LengthSMTLibSessionScopeError result)
runReadyWorkerBounded microseconds config use = do
  bounded <- timeout microseconds
    $ Session.withLengthSMTLibReadyWorker config use
  case bounded of
    Nothing -> assertFailure $ "live Session exceeded outer bound of " ++
      show microseconds ++ " microseconds"
    Just result -> pure result

withFakeZ3Mode
  :: String
  -> (FilePath -> ByteString -> IO result)
  -> IO result
withFakeZ3Mode mode action = withFreshTestDirectory $ \root -> do
  source <- findExecutable "djex-fake-z3" >>= maybe
    (assertFailure "cannot locate the djex-fake-z3 test build tool")
    canonicalizePath
  let target = root </> fakeZ3ExecutableName mode
  copyFile source target
  permissions <- getPermissions source
  setPermissions target $ setOwnerExecutable True permissions
  canonicalTarget <- canonicalizePath target
  image <- BS.readFile canonicalTarget
  action canonicalTarget image

withFreshTestDirectory :: (FilePath -> IO result) -> IO result
withFreshTestDirectory = bracket create removePathForcibly
 where
  create = do
    temporaryRoot <- getTemporaryDirectory
    (path, handle) <- openTempFile temporaryRoot "djex-fake-z3-fixture"
    hClose handle
    removeFile path
    createDirectory path
    canonicalizePath path

fakeZ3ExecutableName :: String -> FilePath
fakeZ3ExecutableName mode =
  "djex-fake-z3-" ++ mode ++ executableExtension
 where
  executableExtension
    | SystemInfo.os == "mingw32" = ".exe"
    | otherwise = ""

fakeZ3EventPath :: FilePath -> FilePath
fakeZ3EventPath executable = executable ++ ".events"

liveArgumentVector :: [String]
liveArgumentVector =
  [ "-in"
  , "-smt2"
  , "smtlib2_compliant=true"
  , "timeout=" ++ show liveSolverTimeoutMilliseconds
  , "rlimit=" ++ show liveSolverResourceLimit
  ]

liveSolverTimeoutMilliseconds :: Int
liveSolverTimeoutMilliseconds = 100

liveSolverResourceLimit :: Int
liveSolverResourceLimit = 4242

liveHostDeadlineMilliseconds :: Int
liveHostDeadlineMilliseconds = 400

alterDigest :: ByteString -> ByteString
alterDigest digest = case BS.uncons digest of
  Nothing -> BS.singleton 1
  Just (first, remaining) -> BS.cons
    (if first == 0 then 1 else 0) remaining

byteStringLength :: ByteString -> Natural
byteStringLength = fromIntegral . BS.length

expectProcessScopeFailure
  :: Process.LengthSMTLibProcessPhase
  -> Process.LengthSMTLibProcessFailureClass
  -> Maybe Natural
  -> Either Session.LengthSMTLibSessionScopeError value
  -> IO Session.LengthSMTLibSessionCleanupStatus
expectProcessScopeFailure expectedPhase expectedClass expectedObserved result =
  case result of
    Left scope -> case Session.lengthSMTLibSessionScopePrimaryError scope of
      Session.LengthSMTLibSessionProcessFailure failure -> do
        Process.lengthSMTLibProcessErrorPhase failure @?= expectedPhase
        Process.lengthSMTLibProcessErrorClass failure @?= expectedClass
        Process.lengthSMTLibProcessErrorObservedAtLeast failure @?=
          expectedObserved
        pure $ Session.lengthSMTLibSessionScopeCleanupStatus scope
      other -> assertFailure $ "unexpected Session primary failure: " ++
        show other
    Right _ -> assertFailure "expected a Session process failure"

expectCapabilityScopeFailure
  :: Capability.LengthSMTLibCapabilityError
  -> Either Session.LengthSMTLibSessionScopeError value
  -> IO Session.LengthSMTLibSessionCleanupStatus
expectCapabilityScopeFailure expected result = case result of
  Left scope -> case Session.lengthSMTLibSessionScopePrimaryError scope of
    Session.LengthSMTLibSessionCapabilityFailure actual -> do
      actual @?= expected
      pure $ Session.lengthSMTLibSessionScopeCleanupStatus scope
    other -> assertFailure $ "unexpected Session primary failure: " ++
      show other
  Right _ -> assertFailure "expected a Session capability failure"

expectObservedStderrFailure
  :: String
  -> Either Session.LengthSMTLibSessionScopeError value
  -> IO Session.LengthSMTLibSessionCleanupStatus
expectObservedStderrFailure mode result = case result of
  Left scope -> case Session.lengthSMTLibSessionScopePrimaryError scope of
    Session.LengthSMTLibSessionProcessFailure failure -> do
      Process.lengthSMTLibProcessErrorPhase failure @?=
        Process.LengthSMTLibProcessStderrPhase
      Process.lengthSMTLibProcessErrorClass failure @?=
        Process.LengthSMTLibProcessStderrObserved
      case Process.lengthSMTLibProcessErrorObservedAtLeast failure of
        Just observed -> assertBool (mode ++ " retained no stderr count")
          $ observed >= 1
        Nothing -> assertFailure $ mode ++ " omitted its stderr count"
      pure $ Session.lengthSMTLibSessionScopeCleanupStatus scope
    other -> assertFailure $ mode ++ " produced the wrong failure: " ++
      show other
  Right _ -> assertFailure $ mode ++ " unexpectedly opened"

expectTerminationFailure
  :: String
  -> Either Session.LengthSMTLibSessionScopeError value
  -> IO Session.LengthSMTLibSessionCleanupStatus
expectTerminationFailure mode result = case result of
  Left scope -> do
    let primary = Session.lengthSMTLibSessionScopePrimaryError scope
        acceptable = case primary of
          Session.LengthSMTLibSessionCapabilityFailure
              (Capability.LengthSMTLibCapabilityUnexpectedEOF
                Capability.LengthSMTLibCapabilityStartupBarrierPhase) -> True
          Session.LengthSMTLibSessionProcessFailure failure ->
            Process.lengthSMTLibProcessErrorClass failure `elem`
              [ Process.LengthSMTLibProcessStdoutEOF
              , Process.LengthSMTLibProcessStderrEOF
              , Process.LengthSMTLibProcessExited
              , Process.LengthSMTLibProcessWriteFailed
              ]
          _ -> False
    assertBool (mode ++ " produced an unrelated failure: " ++ show primary)
      acceptable
    pure $ Session.lengthSMTLibSessionScopeCleanupStatus scope
  Right _ -> assertFailure $ mode ++ " unexpectedly opened"

expectDeadlineScopeFailure
  :: Either Session.LengthSMTLibSessionScopeError value
  -> IO Session.LengthSMTLibSessionCleanupStatus
expectDeadlineScopeFailure result = case result of
  Left scope -> do
    let primary = Session.lengthSMTLibSessionScopePrimaryError scope
        processFailure = case primary of
          Session.LengthSMTLibSessionProcessFailure failure -> Just failure
          Session.LengthSMTLibSessionDeadlineFailure failure -> Just failure
          _ -> Nothing
    case processFailure of
      Nothing -> assertFailure $ "hung worker produced the wrong failure: " ++
        show primary
      Just failure -> Process.lengthSMTLibProcessErrorClass failure @?=
        Process.LengthSMTLibProcessDeadlineExceeded
    pure $ Session.lengthSMTLibSessionScopeCleanupStatus scope
  Right _ -> assertFailure "hung worker unexpectedly opened"

assertWorkspaceWasRemoved
  :: String
  -> Session.LengthSMTLibSessionCleanupStatus
  -> IO ()
assertWorkspaceWasRemoved label cleanup = case
    Session.lengthSMTLibSessionWorkspaceCleanupStatus cleanup of
  Session.LengthSMTLibSessionWorkspaceRemoved _ -> pure ()
  status -> assertFailure $ label ++ " retained its Session workspace: " ++
    show status

data FakeZ3Event = FakeZ3Event
  { fakeZ3EventTag :: ByteString
  , fakeZ3EventFields :: [(ByteString, ByteString)]
  }

parseFakeZ3Events :: ByteString -> Either String [FakeZ3Event]
parseFakeZ3Events bytes
  | BS.null bytes = Right []
  | otherwise = do
      (header, afterHeader) <- takeTraceLine bytes
      (tag, fieldCount) <- parseEventHeader header
      (fields, afterFields) <- parseTraceFields fieldCount afterHeader
      (end, remaining) <- takeTraceLine afterFields
      if end /= "END"
        then Left "fake Z3 trace event omitted END"
        else (FakeZ3Event tag fields :) <$> parseFakeZ3Events remaining

parseEventHeader :: ByteString -> Either String (ByteString, Int)
parseEventHeader header = case BSC.words header of
  ["EVENT", tag, rawCount] -> do
    count <- parseTraceNatural "event field count" rawCount
    pure (tag, count)
  _ -> Left "malformed fake Z3 event header"

parseTraceFields
  :: Int
  -> ByteString
  -> Either String ([(ByteString, ByteString)], ByteString)
parseTraceFields 0 bytes = Right ([], bytes)
parseTraceFields count bytes = do
  (header, afterHeader) <- takeTraceLine bytes
  (name, payloadLength) <- case BSC.words header of
    ["FIELD", fieldName, rawLength] -> do
      parsed <- parseTraceNatural "field byte length" rawLength
      pure (fieldName, parsed)
    _ -> Left "malformed fake Z3 field header"
  if BS.length afterHeader < payloadLength + 1
    then Left "truncated fake Z3 field payload"
    else do
      let (payload, afterPayload) = BS.splitAt payloadLength afterHeader
      case BS.uncons afterPayload of
        Just (10, remaining) -> do
          (fields, rest) <- parseTraceFields (count - 1) remaining
          pure ((name, payload) : fields, rest)
        _ -> Left "fake Z3 field payload omitted delimiter"

parseTraceNatural :: String -> ByteString -> Either String Int
parseTraceNatural label bytes = case BSC.readInt bytes of
  Just (value, remaining)
    | value >= 0 && BS.null remaining -> Right value
  _ -> Left $ "invalid fake Z3 " ++ label

takeTraceLine :: ByteString -> Either String (ByteString, ByteString)
takeTraceLine bytes = case BS.elemIndex 10 bytes of
  Nothing -> Left "unterminated fake Z3 trace line"
  Just index -> Right
    (BS.take index bytes, BS.drop (index + 1) bytes)

expectFakeZ3Event :: ByteString -> [FakeZ3Event] -> IO FakeZ3Event
expectFakeZ3Event tag events = case find ((== tag) . fakeZ3EventTag) events of
  Nothing -> assertFailure $ "missing fake Z3 event: " ++ BSC.unpack tag
  Just event -> pure event

fakeZ3FieldValues :: ByteString -> FakeZ3Event -> [ByteString]
fakeZ3FieldValues name =
  map snd . filter ((== name) . fst) . fakeZ3EventFields

assertSingleFakeField
  :: ByteString
  -> ByteString
  -> FakeZ3Event
  -> IO ()
assertSingleFakeField name expected event =
  fakeZ3FieldValues name event @?= [expected]

utf8Bytes :: String -> ByteString
utf8Bytes = BS.pack . concatMap encodeUTF8Character

encodeUTF8Character :: Char -> [Word8]
encodeUTF8Character character
  | code <= 0x7f = [byte code]
  | code <= 0x7ff =
      [ byte $ 0xc0 + shiftR code 6
      , byte $ 0x80 + (code .&. 0x3f)
      ]
  | code <= 0xffff =
      [ byte $ 0xe0 + shiftR code 12
      , byte $ 0x80 + (shiftR code 6 .&. 0x3f)
      , byte $ 0x80 + (code .&. 0x3f)
      ]
  | otherwise =
      [ byte $ 0xf0 + shiftR code 18
      , byte $ 0x80 + (shiftR code 12 .&. 0x3f)
      , byte $ 0x80 + (shiftR code 6 .&. 0x3f)
      , byte $ 0x80 + (code .&. 0x3f)
      ]
 where
  code = ord character
  byte = fromIntegral

data CapabilityFixtureIdentity

type CapabilityPlan =
  Capability.LengthSMTLibCapabilityPlan CapabilityFixtureIdentity

data CapabilityFixture = CapabilityFixture
  { fixturePlan :: CapabilityPlan
  , fixtureStartupSentinel :: SMTLibStream.SMTLibEchoSentinel
  , fixtureCheckSentinel :: SMTLibStream.SMTLibEchoSentinel
  , fixtureValueSentinel :: SMTLibStream.SMTLibEchoSentinel
  , fixtureReadySentinel :: SMTLibStream.SMTLibEchoSentinel
  }

data CapabilityReceivers = CapabilityReceivers
  { receiverStartupBarrier
      :: Capability.LengthSMTLibCapabilityReceiver CapabilityFixtureIdentity
  , receiverCheckStatus
      :: Capability.LengthSMTLibCapabilityReceiver CapabilityFixtureIdentity
  , receiverCheckBarrier
      :: Capability.LengthSMTLibCapabilityReceiver CapabilityFixtureIdentity
  , receiverInputValue
      :: Capability.LengthSMTLibCapabilityReceiver CapabilityFixtureIdentity
  , receiverInputValueBarrier
      :: Capability.LengthSMTLibCapabilityReceiver CapabilityFixtureIdentity
  , receiverReadyStatus
      :: Capability.LengthSMTLibCapabilityReceiver CapabilityFixtureIdentity
  , receiverReadyBarrier
      :: Capability.LengthSMTLibCapabilityReceiver CapabilityFixtureIdentity
  }

startupNonce, checkNonce, valueNonce, readyNonce :: [Word8]
startupNonce = [0 .. 31]
checkNonce = [32 .. 63]
valueNonce = [64 .. 95]
readyNonce = [96 .. 127]

fixtureNonces :: [[Word8]]
fixtureNonces = [startupNonce, checkNonce, valueNonce, readyNonce]

fixtureBarriers :: [Capability.LengthSMTLibCapabilityBarrier]
fixtureBarriers =
  [ Capability.LengthSMTLibCapabilityStartupBarrier
  , Capability.LengthSMTLibCapabilityCheckBarrier
  , Capability.LengthSMTLibCapabilityInputValueBarrier
  , Capability.LengthSMTLibCapabilityReadyBarrier
  ]

defaultCapabilityFixture :: IO CapabilityFixture
defaultCapabilityFixture =
  capabilityFixture Capability.defaultLengthSMTLibCapabilityLimits

capabilityFixture
  :: Capability.LengthSMTLibCapabilityLimits
  -> IO CapabilityFixture
capabilityFixture limits = do
  plan <- expectRight $ sealCapability limits fixtureNonces
  startup <- expectRight $ SMTLibStream.mkSMTLibEchoSentinel startupNonce
  check <- expectRight $ SMTLibStream.mkSMTLibEchoSentinel checkNonce
  value <- expectRight $ SMTLibStream.mkSMTLibEchoSentinel valueNonce
  ready <- expectRight $ SMTLibStream.mkSMTLibEchoSentinel readyNonce
  pure CapabilityFixture
    { fixturePlan = plan
    , fixtureStartupSentinel = startup
    , fixtureCheckSentinel = check
    , fixtureValueSentinel = value
    , fixtureReadySentinel = ready
    }

sealCapability
  :: Capability.LengthSMTLibCapabilityLimits
  -> [[Word8]]
  -> Either Capability.LengthSMTLibCapabilityPlanError CapabilityPlan
sealCapability limits nonces = case nonces of
  [startup, check, value, ready] ->
    Capability.sealLengthSMTLibCapabilityPlan
      limits startup check value ready
  _ -> error "internal test error: capability requires exactly four nonces"

type ResponseChunker = Int -> [Word8] -> [[Word8]]

wholeChunks :: ResponseChunker
wholeChunks _ bytes = [bytes]

selectedChunks :: ResponseChunker
selectedChunks stage = chunksBySizes [stage + 1, 2, 7, 19]

singletonChunks :: ResponseChunker
singletonChunks _ = map (: [])

emptyInterleavedChunks :: ResponseChunker
emptyInterleavedChunks stage bytes =
  concatMap (\chunk -> [[], chunk]) $ selectedChunks stage bytes

chunksBySizes :: [Int] -> [value] -> [[value]]
chunksBySizes _ [] = []
chunksBySizes [] remaining = [remaining]
chunksBySizes (size : sizes) remaining =
  let (chunk, rest) = splitAt size remaining
  in if null chunk
    then chunksBySizes sizes rest
    else chunk : chunksBySizes sizes rest

assertHealthyCapability :: CapabilityFixture -> ResponseChunker -> IO ()
assertHealthyCapability fixture chunker = do
  assertExactCapabilityWrites fixture
  ready <- driveCapabilityToReady fixture chunker
  outcome <- expectCapabilityComplete =<< feedCapabilityChunks ready
    (chunker 3 $ readyTranscript fixture)
  Capability.lengthSMTLibCapabilityOutcomePlanFingerprint outcome @?=
    Capability.lengthSMTLibCapabilityPlanFingerprint (fixturePlan fixture)

driveCapabilityToReady
  :: CapabilityFixture
  -> ResponseChunker
  -> IO
      (Capability.LengthSMTLibCapabilityReceiver CapabilityFixtureIdentity)
driveCapabilityToReady fixture chunker = do
  startup <- expectCapabilityWrite
    Capability.LengthSMTLibCapabilityStartupWrite
    (expectedStartupWrite fixture)
    $ Capability.startLengthSMTLibCapability $ fixturePlan fixture
  Capability.lengthSMTLibCapabilityReceiverPhase startup @?=
    Capability.LengthSMTLibCapabilityStartupBarrierPhase

  check <- expectCapabilityWrite
    Capability.LengthSMTLibCapabilityCheckWrite
    (expectedCheckWrite fixture)
    =<< feedCapabilityChunks startup
      (chunker 0 $ startupTranscript fixture)
  Capability.lengthSMTLibCapabilityReceiverPhase check @?=
    Capability.LengthSMTLibCapabilityCheckStatusPhase

  value <- expectCapabilityWrite
    Capability.LengthSMTLibCapabilityInputValueWrite
    (expectedValueWrite fixture)
    =<< feedCapabilityChunks check
      (chunker 1 $ checkTranscript fixture)
  Capability.lengthSMTLibCapabilityReceiverPhase value @?=
    Capability.LengthSMTLibCapabilityInputValuePhase

  ready <- expectCapabilityWrite
    Capability.LengthSMTLibCapabilityReadyWrite
    (expectedReadyWrite fixture)
    =<< feedCapabilityChunks value
      (chunker 2 $ valueTranscript fixture)
  Capability.lengthSMTLibCapabilityReceiverPhase ready @?=
    Capability.LengthSMTLibCapabilityReadyStatusPhase
  pure ready

assertExactCapabilityWrites :: CapabilityFixture -> IO ()
assertExactCapabilityWrites fixture = do
  Capability.lengthSMTLibCapabilityStartupWriteBytes plan @?=
    expectedStartupWrite fixture
  Capability.lengthSMTLibCapabilityCheckWriteBytes plan @?=
    expectedCheckWrite fixture
  Capability.lengthSMTLibCapabilityInputValueWriteBytes plan @?=
    expectedValueWrite fixture
  Capability.lengthSMTLibCapabilityReadyWriteBytes plan @?=
    expectedReadyWrite fixture
 where
  plan = fixturePlan fixture

expectedStartupWrite :: CapabilityFixture -> [Word8]
expectedStartupWrite fixture =
  capabilityStartupCommand ++
  SMTLibStream.smtLibEchoSentinelCommandBytes
    (fixtureStartupSentinel fixture)

expectedCheckWrite :: CapabilityFixture -> [Word8]
expectedCheckWrite fixture =
  capabilityResetPrefix ++ capabilityCanonicalPreamble ++
  capabilityDeclaration ++ capabilityAssertZero ++ capabilityCheck ++
  SMTLibStream.smtLibEchoSentinelCommandBytes (fixtureCheckSentinel fixture)

expectedValueWrite :: CapabilityFixture -> [Word8]
expectedValueWrite fixture =
  capabilityValueRequest ++
  SMTLibStream.smtLibEchoSentinelCommandBytes (fixtureValueSentinel fixture)

expectedReadyWrite :: CapabilityFixture -> [Word8]
expectedReadyWrite fixture =
  capabilityResetPrefix ++ capabilityCanonicalPreamble ++
  capabilityDeclaration ++ capabilityAssertZero ++ capabilityAssertOne ++
  capabilityCheck ++
  SMTLibStream.smtLibEchoSentinelCommandBytes (fixtureReadySentinel fixture)

startupTranscript :: CapabilityFixture -> [Word8]
startupTranscript fixture = sentinelResponse
  (fixtureStartupSentinel fixture) ++ ascii "\n"

checkTranscript :: CapabilityFixture -> [Word8]
checkTranscript fixture = ascii "sat\n" ++
  sentinelResponse (fixtureCheckSentinel fixture) ++ ascii "\n"

valueTranscript :: CapabilityFixture -> [Word8]
valueTranscript fixture = capabilityValueResponse ++
  sentinelResponse (fixtureValueSentinel fixture) ++ ascii "\n"

readyTranscript :: CapabilityFixture -> [Word8]
readyTranscript fixture = ascii "unsat\n" ++
  sentinelResponse (fixtureReadySentinel fixture) ++ ascii "\n"

sentinelResponse :: SMTLibStream.SMTLibEchoSentinel -> [Word8]
sentinelResponse = SMTLibStream.smtLibEchoSentinelResponseBytes

assertCapabilitySchemasAndAdmission :: IO ()
assertCapabilitySchemasAndAdmission = do
  Capability.lengthSMTLibCapabilityPlanSchemaTag @?=
    ascii "djex-length-z3-smtlib2-capability-plan/v1"
  Capability.lengthSMTLibCapabilityPhaseMachineSchemaTag @?=
    ascii "djex-length-z3-smtlib2-capability-phase-machine/v1"
  Capability.lengthSMTLibCapabilityPostBarrierSchemaTag @?=
    ascii "djex-smtlib2-capability-post-barrier-whitespace/v1"
  Capability.lengthSMTLibCapabilityExactResponseSchemaTag @?=
    ascii "djex-length-z3-capability-exact-responses/v1"
  assertBool "validated capability defaults changed representation"
    $ Capability.mkLengthSMTLibCapabilityLimits
        Capability.defaultLengthSMTLibCapabilityLimitSource ==
      Capability.defaultLengthSMTLibCapabilityLimits
  let defaults = Capability.defaultLengthSMTLibCapabilityLimits
      stream = Capability.lengthSMTLibCapabilityStreamLimits defaults
  stream @?= SMTLibStream.defaultSMTLibStreamLimits
  SMTLibStream.smtLibStreamTotalByteLimit stream @?= 131072
  SMTLibStream.smtLibStreamFrameByteLimit stream @?= 65536
  SMTLibStream.smtLibStreamNestingDepthLimit stream @?= 64
  Capability.lengthSMTLibCapabilityCumulativeOutputByteLimit defaults @?=
    524288
  Capability.lengthSMTLibCapabilityPlanFingerprintByteLimit defaults @?=
    262144

  let tooSmall site fieldName admitted required =
        Capability.LengthSMTLibCapabilityRequiredLimitTooSmall
          site fieldName admitted required
  assertLeft
    (tooSmall Capability.LengthSMTLibCapabilityStartupBarrierFrame
      Capability.LengthSMTLibCapabilityStreamTotalBytes 86 87)
    $ sealCapability (capabilityStreamLimits $ \source -> source
        { SMTLibStream.smtLibStreamLimitSourceTotalBytes = 86 })
      fixtureNonces
  assertLeft
    (tooSmall Capability.LengthSMTLibCapabilityStartupBarrierFrame
      Capability.LengthSMTLibCapabilityStreamFrameBytes 86 87)
    $ sealCapability (capabilityStreamLimits $ \source -> source
        { SMTLibStream.smtLibStreamLimitSourceFrameBytes = 86 })
      fixtureNonces
  assertLeft
    (tooSmall Capability.LengthSMTLibCapabilityCheckBarrierFrame
      Capability.LengthSMTLibCapabilityStreamTotalBytes 87 88)
    $ sealCapability (capabilityStreamLimits $ \source -> source
        { SMTLibStream.smtLibStreamLimitSourceTotalBytes = 87 })
      fixtureNonces
  assertLeft
    (tooSmall Capability.LengthSMTLibCapabilityInputValueFrame
      Capability.LengthSMTLibCapabilityStreamNestingDepth 1 2)
    $ sealCapability (capabilityStreamLimits $ \source -> source
        { SMTLibStream.smtLibStreamLimitSourceNestingDepth = 1 })
      fixtureNonces
  assertLeft
    (Capability.LengthSMTLibCapabilityMinimumOutputByteLimitExceeded 388 389)
    $ sealCapability (capabilityLimits $ \source -> source
        { Capability.lengthSMTLibCapabilityLimitSourceCumulativeOutputBytes =
            388 }) fixtureNonces
  assertLeft
    (Capability.LengthSMTLibCapabilityPlanFingerprintByteLimitExceeded 0 1)
    $ sealCapability (capabilityLimits $ \source -> source
        { Capability.lengthSMTLibCapabilityLimitSourcePlanFingerprintBytes =
            0 }) fixtureNonces

assertCapabilityNonceAdmission :: IO ()
assertCapabilityNonceAdmission = do
  forM_ (zip [0 ..] fixtureBarriers) $ \(index, barrier) ->
    assertLeft
      (Capability.LengthSMTLibCapabilityBarrierNonceError barrier
        $ SMTLibStream.SMTLibEchoSentinelNonceLengthMismatch 32 31)
      $ sealCapability Capability.defaultLengthSMTLibCapabilityLimits
      $ replaceAt index (replicate 31 0) fixtureNonces
  forM_ pairwiseIndices $ \(firstIndex, secondIndex) ->
    assertLeft
      (Capability.LengthSMTLibCapabilityRepeatedBarrierNonce
        (fixtureBarriers !! firstIndex)
        (fixtureBarriers !! secondIndex))
      $ sealCapability Capability.defaultLengthSMTLibCapabilityLimits
      $ replaceAt secondIndex (fixtureNonces !! firstIndex) fixtureNonces
 where
  pairwiseIndices =
    [(first, second) | first <- [0 .. 3], second <- [first + 1 .. 3]]

assertCapabilityPhaseFailures :: IO ()
assertCapabilityPhaseFailures = do
  fixture <- defaultCapabilityFixture
  receivers <- capabilityReceivers fixture
  let wrongExactCases =
        [ ( receiverCheckStatus receivers
          , ascii "unknown\n"
          , Capability.LengthSMTLibCapabilityUnexpectedExactResponse
              Capability.LengthSMTLibCapabilityCheckStatusPhase
          )
        , ( receiverInputValue receivers
          , ascii "((djex_capability_input 1))"
          , Capability.LengthSMTLibCapabilityUnexpectedExactResponse
              Capability.LengthSMTLibCapabilityInputValuePhase
          )
        , ( receiverReadyStatus receivers
          , ascii "sat\n"
          , Capability.LengthSMTLibCapabilityUnexpectedExactResponse
              Capability.LengthSMTLibCapabilityReadyStatusPhase
          )
        ]
      wrongBarrierCases =
        [ ( receiverStartupBarrier receivers
          , fixtureCheckSentinel fixture
          , Capability.LengthSMTLibCapabilityStartupBarrier
          )
        , ( receiverCheckBarrier receivers
          , fixtureStartupSentinel fixture
          , Capability.LengthSMTLibCapabilityCheckBarrier
          )
        , ( receiverInputValueBarrier receivers
          , fixtureReadySentinel fixture
          , Capability.LengthSMTLibCapabilityInputValueBarrier
          )
        , ( receiverReadyBarrier receivers
          , fixtureValueSentinel fixture
          , Capability.LengthSMTLibCapabilityReadyBarrier
          )
        ]
      eofCases =
        [ ( Capability.LengthSMTLibCapabilityStartupBarrierPhase
          , receiverStartupBarrier receivers)
        , ( Capability.LengthSMTLibCapabilityCheckStatusPhase
          , receiverCheckStatus receivers)
        , ( Capability.LengthSMTLibCapabilityCheckBarrierPhase
          , receiverCheckBarrier receivers)
        , ( Capability.LengthSMTLibCapabilityInputValuePhase
          , receiverInputValue receivers)
        , ( Capability.LengthSMTLibCapabilityInputValueBarrierPhase
          , receiverInputValueBarrier receivers)
        , ( Capability.LengthSMTLibCapabilityReadyStatusPhase
          , receiverReadyStatus receivers)
        , ( Capability.LengthSMTLibCapabilityReadyBarrierPhase
          , receiverReadyBarrier receivers)
        ]
  forM_ wrongExactCases $ \(receiver, bytes, expected) ->
    assertLeft expected $ Capability.feedLengthSMTLibCapability receiver bytes
  forM_ wrongBarrierCases $ \(receiver, sentinel, barrier) ->
    assertLeft
      (Capability.LengthSMTLibCapabilityBarrierMismatch barrier)
      $ Capability.feedLengthSMTLibCapability receiver
      $ sentinelResponse sentinel ++ ascii "\n"
  forM_ eofCases $ \(phase, receiver) -> do
    Capability.lengthSMTLibCapabilityReceiverPhase receiver @?= phase
    assertLeft
      (Capability.LengthSMTLibCapabilityUnexpectedEOF phase)
      $ Capability.finishLengthSMTLibCapability receiver

assertCapabilityCausalBoundaries :: IO ()
assertCapabilityCausalBoundaries = do
  fixture <- defaultCapabilityFixture
  receivers <- capabilityReceivers fixture
  assertPostBarrierByte 40
    $ Capability.feedLengthSMTLibCapability
        (receiverCheckStatus receivers)
    $ ascii "sat\n" ++
      sentinelResponse (fixtureCheckSentinel fixture) ++ ascii "\n" ++
      capabilityValueResponse
  assertPostBarrierByte 117
    $ Capability.feedLengthSMTLibCapability
        (receiverInputValue receivers)
    $ capabilityValueResponse ++
      sentinelResponse (fixtureValueSentinel fixture) ++ ascii "\nunsat\n"

assertCapabilityAccounting :: IO ()
assertCapabilityAccounting = do
  let exactLimits = capabilityLimits $ \source -> source
        { Capability.lengthSMTLibCapabilityLimitSourceCumulativeOutputBytes =
            minimumCapabilityTranscriptBytes }
  exactFixture <- capabilityFixture exactLimits
  assertHealthyCapability exactFixture wholeChunks
  exactReady <- driveCapabilityToReady exactFixture wholeChunks
  assertLeft
    (Capability.LengthSMTLibCapabilityCumulativeOutputByteLimitExceeded
      minimumCapabilityTranscriptBytes
      (minimumCapabilityTranscriptBytes + 1))
    $ Capability.feedLengthSMTLibCapability exactReady
    $ readyTranscript exactFixture ++ ascii " "

  let tieLimits = Capability.mkLengthSMTLibCapabilityLimits
        $ Capability.defaultLengthSMTLibCapabilityLimitSource
          { Capability.lengthSMTLibCapabilityLimitSourceStreamLimits =
              SMTLibStream.defaultSMTLibStreamLimitSource
                { SMTLibStream.smtLibStreamLimitSourceTotalBytes = 88 }
          , Capability.lengthSMTLibCapabilityLimitSourceCumulativeOutputBytes =
              minimumCapabilityTranscriptBytes
          }
  tieFixture <- capabilityFixture tieLimits
  tieReady <- driveCapabilityToReady tieFixture wholeChunks
  assertLeft
    (Capability.LengthSMTLibCapabilityFramingFailure
      Capability.LengthSMTLibCapabilityReadyBarrierPhase
      $ SMTLibStream.SMTLibStreamTotalByteLimitExceeded 88 89)
    $ Capability.feedLengthSMTLibCapability tieReady
    $ ascii " unsat\n " ++
      sentinelResponse (fixtureReadySentinel tieFixture) ++ ascii "\n"

assertCapabilityNoScanning :: IO ()
assertCapabilityNoScanning = do
  fixture <- defaultCapabilityFixture
  receivers <- capabilityReceivers fixture
  assertLeft
    (Capability.LengthSMTLibCapabilityUnexpectedExactResponse
      Capability.LengthSMTLibCapabilityCheckStatusPhase)
    $ Capability.feedLengthSMTLibCapability
        (receiverCheckStatus receivers)
    $ ascii "unknown\nsat\n" ++
      sentinelResponse (fixtureCheckSentinel fixture) ++ ascii "\n"
  assertLeft
    (Capability.LengthSMTLibCapabilityBarrierMismatch
      Capability.LengthSMTLibCapabilityCheckBarrier)
    $ Capability.feedLengthSMTLibCapability
        (receiverCheckBarrier receivers)
    $ sentinelResponse (fixtureStartupSentinel fixture) ++ ascii "\n" ++
      sentinelResponse (fixtureCheckSentinel fixture) ++ ascii "\n"
  assertLeft
    (Capability.LengthSMTLibCapabilityUnexpectedExactResponse
      Capability.LengthSMTLibCapabilityInputValuePhase)
    $ Capability.feedLengthSMTLibCapability
        (receiverInputValue receivers)
    $ ascii "((djex_capability_input 1))" ++ capabilityValueResponse ++
      sentinelResponse (fixtureValueSentinel fixture) ++ ascii "\n"

capabilityReceivers :: CapabilityFixture -> IO CapabilityReceivers
capabilityReceivers fixture = do
  startup <- expectCapabilityWrite
    Capability.LengthSMTLibCapabilityStartupWrite
    (expectedStartupWrite fixture)
    $ Capability.startLengthSMTLibCapability $ fixturePlan fixture
  check <- expectCapabilityWrite
    Capability.LengthSMTLibCapabilityCheckWrite
    (expectedCheckWrite fixture)
    =<< expectRight (Capability.feedLengthSMTLibCapability startup
      $ startupTranscript fixture)
  checkBarrier <- expectCapabilityAwait =<< expectRight
    (Capability.feedLengthSMTLibCapability check $ ascii "sat\n")
  value <- expectCapabilityWrite
    Capability.LengthSMTLibCapabilityInputValueWrite
    (expectedValueWrite fixture)
    =<< expectRight (Capability.feedLengthSMTLibCapability checkBarrier
      $ sentinelResponse (fixtureCheckSentinel fixture) ++ ascii "\n")
  valueBarrier <- expectCapabilityAwait =<< expectRight
    (Capability.feedLengthSMTLibCapability value capabilityValueResponse)
  ready <- expectCapabilityWrite
    Capability.LengthSMTLibCapabilityReadyWrite
    (expectedReadyWrite fixture)
    =<< expectRight (Capability.feedLengthSMTLibCapability valueBarrier
      $ sentinelResponse (fixtureValueSentinel fixture) ++ ascii "\n")
  readyBarrier <- expectCapabilityAwait =<< expectRight
    (Capability.feedLengthSMTLibCapability ready $ ascii "unsat\n")
  pure CapabilityReceivers
    { receiverStartupBarrier = startup
    , receiverCheckStatus = check
    , receiverCheckBarrier = checkBarrier
    , receiverInputValue = value
    , receiverInputValueBarrier = valueBarrier
    , receiverReadyStatus = ready
    , receiverReadyBarrier = readyBarrier
    }

expectCapabilityWrite
  :: Capability.LengthSMTLibCapabilityWriteKind
  -> [Word8]
  -> Capability.LengthSMTLibCapabilityAction CapabilityFixtureIdentity
  -> IO
      (Capability.LengthSMTLibCapabilityReceiver CapabilityFixtureIdentity)
expectCapabilityWrite expectedKind expectedBytes action = case action of
  Capability.LengthSMTLibCapabilityWrite kind bytes receiver -> do
    kind @?= expectedKind
    bytes @?= expectedBytes
    pure receiver
  Capability.LengthSMTLibCapabilityAwait{} ->
    assertFailure "expected a capability write action, observed await"
  Capability.LengthSMTLibCapabilityComplete{} ->
    assertFailure "expected a capability write action, observed completion"

expectCapabilityAwait
  :: Capability.LengthSMTLibCapabilityAction CapabilityFixtureIdentity
  -> IO
      (Capability.LengthSMTLibCapabilityReceiver CapabilityFixtureIdentity)
expectCapabilityAwait action = case action of
  Capability.LengthSMTLibCapabilityAwait receiver -> pure receiver
  Capability.LengthSMTLibCapabilityWrite{} ->
    assertFailure "expected capability await, observed write"
  Capability.LengthSMTLibCapabilityComplete{} ->
    assertFailure "expected capability await, observed completion"

expectCapabilityComplete
  :: Capability.LengthSMTLibCapabilityAction CapabilityFixtureIdentity
  -> IO (Capability.LengthSMTLibCapabilityOutcome CapabilityFixtureIdentity)
expectCapabilityComplete action = case action of
  Capability.LengthSMTLibCapabilityComplete outcome -> pure outcome
  Capability.LengthSMTLibCapabilityWrite{} ->
    assertFailure "expected capability completion, observed write"
  Capability.LengthSMTLibCapabilityAwait{} ->
    assertFailure "expected capability completion, observed await"

feedCapabilityChunks
  :: Capability.LengthSMTLibCapabilityReceiver CapabilityFixtureIdentity
  -> [[Word8]]
  -> IO (Capability.LengthSMTLibCapabilityAction CapabilityFixtureIdentity)
feedCapabilityChunks receiver chunks = case chunks of
  [] -> pure $ Capability.LengthSMTLibCapabilityAwait receiver
  chunk : remaining -> do
    action <- expectRight
      $ Capability.feedLengthSMTLibCapability receiver chunk
    case action of
      Capability.LengthSMTLibCapabilityAwait next ->
        feedCapabilityChunks next remaining
      _
        | null remaining -> pure action
        | otherwise -> assertFailure
            "capability produced an action before all response chunks"

assertPostBarrierByte
  :: Word8
  -> Either
      Capability.LengthSMTLibCapabilityError
      (Capability.LengthSMTLibCapabilityAction CapabilityFixtureIdentity)
  -> IO ()
assertPostBarrierByte expected result = case result of
  Left (Capability.LengthSMTLibCapabilityUnexpectedPostBarrierByte _ byte) ->
    byte @?= expected
  Left other -> assertFailure $ "unexpected causal-boundary error: " ++
    show other
  Right _ -> assertFailure "pre-write capability output was accepted"

assertLeft :: (Eq failure, Show failure) => failure -> Either failure value -> IO ()
assertLeft expected result = case result of
  Left actual -> actual @?= expected
  Right _ -> assertFailure $ "expected rejection: " ++ show expected

expectRight :: Show failure => Either failure value -> IO value
expectRight result = case result of
  Left failure -> assertFailure $ "unexpected rejection: " ++ show failure
  Right value -> pure value

capabilityLimits
  :: (Capability.LengthSMTLibCapabilityLimitSource
      -> Capability.LengthSMTLibCapabilityLimitSource)
  -> Capability.LengthSMTLibCapabilityLimits
capabilityLimits change = Capability.mkLengthSMTLibCapabilityLimits
  $ change Capability.defaultLengthSMTLibCapabilityLimitSource

capabilityStreamLimits
  :: (SMTLibStream.SMTLibStreamLimitSource
      -> SMTLibStream.SMTLibStreamLimitSource)
  -> Capability.LengthSMTLibCapabilityLimits
capabilityStreamLimits change = capabilityLimits $ \source -> source
  { Capability.lengthSMTLibCapabilityLimitSourceStreamLimits =
      change SMTLibStream.defaultSMTLibStreamLimitSource }

replaceAt :: Int -> value -> [value] -> [value]
replaceAt target replacement values =
  [if index == target then replacement else value
  | (index, value) <- zip [0 ..] values]

minimumCapabilityTranscriptBytes :: Natural
minimumCapabilityTranscriptBytes = 389

capabilityStartupCommand :: [Word8]
capabilityStartupCommand = ascii "(set-option :print-success false)\n"

capabilityResetPrefix :: [Word8]
capabilityResetPrefix = ascii
  "(reset)\n(set-option :print-success false)\n"

capabilityCanonicalPreamble :: [Word8]
capabilityCanonicalPreamble = ascii $ concat
  [ "(set-option :produce-models true)\n"
  , "(set-option :random-seed 1)\n"
  , "(set-logic QF_LIA)\n"
  , "(define-fun djex_nat_monus ((x Int) (y Int)) Int "
  , "(ite (<= y x) (- x y) 0))\n"
  , "(define-fun djex_nat_min ((x Int) (y Int)) Int "
  , "(ite (<= x y) x y))\n"
  , "(define-fun djex_nat_max ((x Int) (y Int)) Int "
  , "(ite (<= x y) y x))\n"
  ]

capabilityDeclaration :: [Word8]
capabilityDeclaration =
  ascii "(declare-const djex_capability_input Int)\n"

capabilityAssertZero :: [Word8]
capabilityAssertZero = ascii "(assert (= djex_capability_input 0))\n"

capabilityAssertOne :: [Word8]
capabilityAssertOne = ascii "(assert (= djex_capability_input 1))\n"

capabilityCheck :: [Word8]
capabilityCheck = ascii "(check-sat)\n"

capabilityValueRequest :: [Word8]
capabilityValueRequest =
  ascii "(get-value (djex_capability_input))\n"

capabilityValueResponse :: [Word8]
capabilityValueResponse = ascii "((djex_capability_input 0))"

ascii :: String -> [Word8]
ascii = map $ fromIntegral . fromEnum
