{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

-- | A narrow live Length/Z3 boundary.
--
-- This module owns one capability-probed worker only for the dynamic extent of
-- 'withLengthSMTLibLiveSession'.  Public callers can submit sealed queries and
-- inspect status and heuristic strength, then consume exact query association
-- and independently replayed counterexample evidence only through
-- 'replayLengthSMTLibLiveQueryObservation'.  They cannot inspect or retain a
-- process handle, cancellation token, executable or workspace path, barrier,
-- ordinal, transcript, decoded valuation, transport counter, or reversible run
-- identity.
--
-- Solver status remains an observation.  In particular, @unsat@ is relative to
-- the checked encoding and every status is restricted to
-- 'HeuristicRankingOnly'.  Only the optional 'BehavioralEvidence' has survived
-- independent Length replay against the exact query problem.  Consumers can
-- reveal its receipt through 'replayLengthSMTLibLiveQueryObservation', which
-- checks the complete query identity before inspecting that evidence.
--
-- Private session defaults own opener and finalizer deadlines; the supplied
-- execution policy owns the host deadline for each query.  This scope does not
-- claim one hard wall-clock deadline for a whole caller-defined batch, and
-- durable cleanup latency can outlive the operation which initiated it.
-- Callback exceptions, including asynchronous exceptions, are rethrown after
-- the private owner has started durable cleanup.
module Language.Haskell.Synthesis.Semantic.Length.SMTLib.Live
  ( LengthSMTLibLiveSession
  , LengthSMTLibLiveSessionFailure (..)
  , LengthSMTLibLiveSessionError
  , lengthSMTLibLiveSessionPrimaryFailure
  , lengthSMTLibLiveSessionCleanupIncomplete
  , LengthSMTLibLiveQueryObservation
  , LengthSMTLibLiveQueryFailure (..)
  , LengthSMTLibLiveQueryError
  , lengthSMTLibLiveQueryPrimaryFailure
  , lengthSMTLibLiveQueryCleanupIncomplete
  , LengthSMTLibLiveObservationReplayError (..)
  , defaultLengthSMTLibLiveSessionMaximumQueries
  , withLengthSMTLibLiveSession
  , runLengthSMTLibLiveQuery
  , replayLengthSMTLibLiveQueryObservation
  , lengthSMTLibLiveQueryObservationSolverStatus
  , lengthSMTLibLiveQueryObservationResultStrength
  , lengthSMTLibLiveQueryObservationUse
  ) where

import Control.DeepSeq (NFData (rnf))
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Fingerprint (Fingerprint)
import qualified Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol
  as Protocol
import qualified Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Response
  as Response
import qualified Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session
  as Session
import qualified Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session.Capability
  as Capability
import qualified Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session.Process
  as Process
import qualified Language.Haskell.Synthesis.Internal.SMTLib.Response
  as SMTResponse
import qualified Language.Haskell.Synthesis.Internal.SMTLib.Stream
  as Stream
import Language.Haskell.Synthesis.Semantic.Length
  ( FiniteListSpineLengthV1 )
import Language.Haskell.Synthesis.Semantic.Length.Evaluate
  ( LengthEvaluationLimits
  , ValidatedLengthCounterexample
  )
import Language.Haskell.Synthesis.Semantic.Length.SMTLib
  ( LengthSMTLibQuery
  , LengthSMTLibQueryFingerprintSubject
  , lengthSMTLibQueryBehavioralProblem
  , lengthSMTLibQueryFingerprint
  )
import Language.Haskell.Synthesis.Semantic.Length.SMTLib.Execution
  ( LengthSMTLibExecutionConfig )
import Language.Haskell.Synthesis.Semantic.Observation
  ( SolverStatus (..) )
import Language.Haskell.Synthesis.Semantic.Problem
  ( BehavioralEvidence
  , RawObservationUse (..)
  , RawResultStrength (..)
  , ReplayMismatch
  , replayBehavioralEvidence
  )

-- | A scoped, capability-probed worker.  The constructor and all underlying
-- ownership projections are private.
data LengthSMTLibLiveSession epoch = LengthSMTLibLiveSession
  !(Session.LengthSMTLibReadyWorker epoch)

type role LengthSMTLibLiveSession nominal

-- | Stable, byte-free classes for opening, probing, and closing a live scope.
data LengthSMTLibLiveSessionFailure
  = LengthSMTLibLiveSessionDeadlineExceeded
  | LengthSMTLibLiveSessionWorkspaceUnavailable
  | LengthSMTLibLiveSessionExecutableUnavailable
  | LengthSMTLibLiveSessionExecutableRejected
  | LengthSMTLibLiveSessionLaunchFailed
  | LengthSMTLibLiveSessionCapabilityRejected
  | LengthSMTLibLiveSessionResourceLimitExceeded
  | LengthSMTLibLiveSessionTransportFailed
  | LengthSMTLibLiveSessionCleanupFailed
  | LengthSMTLibLiveSessionInternalFailure
  deriving (Bounded, Enum, Eq, Ord, Show)

instance NFData LengthSMTLibLiveSessionFailure where
  rnf failure = failure `seq` ()

-- | Sanitized scope failure.  No child output, command, marker, path, digest,
-- exit code, or cleanup escalation detail crosses this boundary.
data LengthSMTLibLiveSessionError = LengthSMTLibLiveSessionError
  !LengthSMTLibLiveSessionFailure
  !Bool
  deriving (Eq, Ord, Show)

instance NFData LengthSMTLibLiveSessionError where
  rnf (LengthSMTLibLiveSessionError failure incomplete) =
    rnf failure `seq` rnf incomplete

lengthSMTLibLiveSessionPrimaryFailure
  :: LengthSMTLibLiveSessionError
  -> LengthSMTLibLiveSessionFailure
lengthSMTLibLiveSessionPrimaryFailure
    (LengthSMTLibLiveSessionError failure _) = failure

-- | Whether owned cleanup reported an incomplete process or workspace release.
lengthSMTLibLiveSessionCleanupIncomplete
  :: LengthSMTLibLiveSessionError
  -> Bool
lengthSMTLibLiveSessionCleanupIncomplete
    (LengthSMTLibLiveSessionError _ incomplete) = incomplete

-- | Stable, byte-free classes for one query transaction.
data LengthSMTLibLiveQueryFailure
  = LengthSMTLibLiveQuerySessionUnavailable
  | LengthSMTLibLiveQueryLimitExceeded !Natural !Natural
  | LengthSMTLibLiveQueryDeadlineExceeded
  | LengthSMTLibLiveQueryConfigurationRejected
  | LengthSMTLibLiveQueryResourceLimitExceeded
  | LengthSMTLibLiveQueryTransportFailed
  | LengthSMTLibLiveQueryProtocolRejected
  | LengthSMTLibLiveQueryCounterexampleRejected
  | LengthSMTLibLiveQueryInternalFailure
  deriving (Eq, Ord, Show)

instance NFData LengthSMTLibLiveQueryFailure where
  rnf failure = case failure of
    LengthSMTLibLiveQueryLimitExceeded limit observed ->
      rnf limit `seq` rnf observed
    _ -> failure `seq` ()

-- | Sanitized query failure.  Internal protocol/model diagnostics can retain
-- bounded child-controlled payloads; this record deliberately cannot.
data LengthSMTLibLiveQueryError = LengthSMTLibLiveQueryError
  !LengthSMTLibLiveQueryFailure
  !Bool
  deriving (Eq, Ord, Show)

instance NFData LengthSMTLibLiveQueryError where
  rnf (LengthSMTLibLiveQueryError failure incomplete) =
    rnf failure `seq` rnf incomplete

lengthSMTLibLiveQueryPrimaryFailure
  :: LengthSMTLibLiveQueryError
  -> LengthSMTLibLiveQueryFailure
lengthSMTLibLiveQueryPrimaryFailure
    (LengthSMTLibLiveQueryError failure _) = failure

-- | Whether teardown following this query reported incomplete process cleanup.
lengthSMTLibLiveQueryCleanupIncomplete
  :: LengthSMTLibLiveQueryError
  -> Bool
lengthSMTLibLiveQueryCleanupIncomplete
    (LengthSMTLibLiveQueryError _ incomplete) = incomplete

-- | Why a completed live observation could not be replayed against one exact
-- query.  The query fingerprint is checked before optional evidence, so a
-- stale query fails without inspecting or replaying a retained receipt.
data LengthSMTLibLiveObservationReplayError
  = LengthSMTLibLiveObservationQueryFingerprintMismatch
  | LengthSMTLibLiveObservationEvidenceProblemMismatch !ReplayMismatch
  deriving (Eq, Ord, Show)

instance NFData LengthSMTLibLiveObservationReplayError where
  rnf failure = case failure of
    LengthSMTLibLiveObservationQueryFingerprintMismatch -> ()
    LengthSMTLibLiveObservationEvidenceProblemMismatch mismatch ->
      rnf mismatch

-- | Safe projection of one completed query.  It freshly copies only bounded
-- association and authority fields instead of wrapping the private run.  The
-- query fingerprint and optional evidence have no public projection; the
-- replay gate below is their only public semantic extraction edge.
data LengthSMTLibLiveQueryObservation epoch identity local =
  LengthSMTLibLiveQueryObservation
    !(Fingerprint LengthSMTLibQueryFingerprintSubject)
    !SolverStatus
    !(Maybe
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthCounterexample))

type role LengthSMTLibLiveQueryObservation nominal nominal nominal

instance NFData
    (LengthSMTLibLiveQueryObservation epoch identity local) where
  rnf (LengthSMTLibLiveQueryObservation query status evidence) =
    rnf query `seq` rnf status `seq` rnf evidence

-- | The fixed private session default.  A scope admits at most this many
-- serial queries and rejects maximum-plus-one before writing it.
defaultLengthSMTLibLiveSessionMaximumQueries :: Natural
defaultLengthSMTLibLiveSessionMaximumQueries =
  Session.lengthSMTLibSessionLimitSourceMaximumQueries
    Session.defaultLengthSMTLibSessionLimitSource

-- | Open, capability-probe, lend, and close one worker using validated private
-- transport/protocol defaults and the caller's public execution policy.
withLengthSMTLibLiveSession
  :: LengthSMTLibExecutionConfig
  -> (forall epoch. LengthSMTLibLiveSession epoch -> IO result)
  -> IO (Either LengthSMTLibLiveSessionError result)
withLengthSMTLibLiveSession execution use =
  case defaultLiveSessionConfig execution of
    Left failure -> pure $ Left $ LengthSMTLibLiveSessionError failure False
    Right config -> do
      result <- Session.withLengthSMTLibReadyWorker config
        $ use . LengthSMTLibLiveSession
      pure $ case result of
        Left failure -> Left $ sanitizeSessionError failure
        Right value -> Right value

-- | Run one query serially inside the scoped lease.  A satisfiable values-policy
-- result succeeds only when its model independently replays as a counterexample.
runLengthSMTLibLiveQuery
  :: LengthEvaluationLimits
  -> LengthSMTLibLiveSession epoch
  -> LengthSMTLibQuery identity local
  -> IO
      (Either
        LengthSMTLibLiveQueryError
        (LengthSMTLibLiveQueryObservation epoch identity local))
runLengthSMTLibLiveQuery evaluationLimits
    (LengthSMTLibLiveSession worker) query = do
  result <- Session.runLengthSMTLibReadyWorkerQuery
    evaluationLimits worker query
  pure $ case result of
    Left failure -> Left $ sanitizeQueryError failure
    Right run -> Right $ LengthSMTLibLiveQueryObservation
      (lengthSMTLibQueryFingerprint query)
      (Session.lengthSMTLibQueryRunSolverStatus run)
      (Session.lengthSMTLibQueryRunCounterexampleEvidence run)

-- | Replay the safe semantic payload of one completed observation against the
-- exact sealed query supplied by its consumer.
--
-- The collision-free query fingerprint is compared first.  Only after it
-- matches is optional evidence replayed against the query's retained
-- @BehavioralProblem@.  A successful 'Nothing' confirms exact association of
-- a status-only observation but grants no evidence; a successful 'Just'
-- reveals only the already independently validated, finite-spine
-- counterexample receipt.  Solver status remains 'HeuristicRankingOnly'.
replayLengthSMTLibLiveQueryObservation
  :: LengthSMTLibQuery identity local
  -> LengthSMTLibLiveQueryObservation epoch identity local
  -> Either
      LengthSMTLibLiveObservationReplayError
      (Maybe ValidatedLengthCounterexample)
replayLengthSMTLibLiveQueryObservation query observation
  | lengthSMTLibQueryFingerprint query /=
      lengthSMTLibLiveQueryObservationQueryFingerprint observation =
        Left LengthSMTLibLiveObservationQueryFingerprintMismatch
  | otherwise = case
      lengthSMTLibLiveQueryObservationCounterexampleEvidence observation of
    Nothing -> Right Nothing
    Just evidence -> case replayBehavioralEvidence
        (lengthSMTLibQueryBehavioralProblem query) evidence of
      Left mismatch -> Left
        $ LengthSMTLibLiveObservationEvidenceProblemMismatch mismatch
      Right receipt -> Right $ Just receipt

-- Private association projection used only by the checked replay gate.  It is
-- intentionally not exported: callers cannot inspect a query key without also
-- passing through evidence-consumption precedence.
lengthSMTLibLiveQueryObservationQueryFingerprint
  :: LengthSMTLibLiveQueryObservation epoch identity local
  -> Fingerprint LengthSMTLibQueryFingerprintSubject
lengthSMTLibLiveQueryObservationQueryFingerprint
    (LengthSMTLibLiveQueryObservation query _ _) = query

lengthSMTLibLiveQueryObservationSolverStatus
  :: LengthSMTLibLiveQueryObservation epoch identity local
  -> SolverStatus
lengthSMTLibLiveQueryObservationSolverStatus
    (LengthSMTLibLiveQueryObservation _ status _) = status

lengthSMTLibLiveQueryObservationResultStrength
  :: LengthSMTLibLiveQueryObservation epoch identity local
  -> RawResultStrength
lengthSMTLibLiveQueryObservationResultStrength observation =
  solverStatusStrength
    $ lengthSMTLibLiveQueryObservationSolverStatus observation

-- | Live observations have heuristic ranking authority only, including
-- satisfiable observations which also carry independently replayed evidence.
lengthSMTLibLiveQueryObservationUse
  :: LengthSMTLibLiveQueryObservation epoch identity local
  -> RawObservationUse
lengthSMTLibLiveQueryObservationUse _ = HeuristicRankingOnly

-- Private evidence projection used only after exact query association has
-- succeeded.  Keeping this selector separate preserves the gate's established
-- demand order without exposing detached evidence to public callers.
lengthSMTLibLiveQueryObservationCounterexampleEvidence
  :: LengthSMTLibLiveQueryObservation epoch identity local
  -> Maybe
      (BehavioralEvidence
        FiniteListSpineLengthV1
        ValidatedLengthCounterexample)
lengthSMTLibLiveQueryObservationCounterexampleEvidence
    (LengthSMTLibLiveQueryObservation _ _ evidence) = evidence

defaultLiveSessionConfig
  :: LengthSMTLibExecutionConfig
  -> Either LengthSMTLibLiveSessionFailure Session.LengthSMTLibSessionConfig
defaultLiveSessionConfig execution = do
  process <- case Process.mkLengthSMTLibProcessLimits
      Process.defaultLengthSMTLibProcessLimitSource of
    Left _ -> Left LengthSMTLibLiveSessionInternalFailure
    Right limits -> Right limits
  case Session.sealLengthSMTLibSessionConfig
      Session.defaultLengthSMTLibSessionLimits
      process
      Capability.defaultLengthSMTLibCapabilityLimits
      Protocol.defaultLengthSMTLibProtocolLimits
      execution of
    Left failure -> Left $ sessionConfigFailure failure
    Right config -> Right config

solverStatusStrength :: SolverStatus -> RawResultStrength
solverStatusStrength status = case status of
  SolverSatisfiable -> RawSolverModelHint
  SolverUnsatisfiable -> RawSolverUnsatRelativeToEncoding
  SolverUnknown -> RawSolverUnknown

sanitizeSessionError
  :: Session.LengthSMTLibSessionScopeError
  -> LengthSMTLibLiveSessionError
sanitizeSessionError failure = LengthSMTLibLiveSessionError
  (sessionFailure $ Session.lengthSMTLibSessionScopePrimaryError failure)
  (sessionCleanupIncomplete
    $ Session.lengthSMTLibSessionScopeCleanupStatus failure)

sanitizeQueryError
  :: Session.LengthSMTLibQueryRunError
  -> LengthSMTLibLiveQueryError
sanitizeQueryError failure = LengthSMTLibLiveQueryError
  (queryFailure $ Session.lengthSMTLibQueryRunPrimaryFailure failure)
  (maybe False processCleanupIncomplete
    $ Session.lengthSMTLibQueryRunProcessCleanupStatus failure)

sessionConfigFailure
  :: Session.LengthSMTLibSessionConfigError
  -> LengthSMTLibLiveSessionFailure
sessionConfigFailure _ = LengthSMTLibLiveSessionInternalFailure

sessionFailure
  :: Session.LengthSMTLibSessionError
  -> LengthSMTLibLiveSessionFailure
sessionFailure failure = case failure of
  Session.LengthSMTLibSessionDeadlineFailure _ ->
    LengthSMTLibLiveSessionDeadlineExceeded
  Session.LengthSMTLibSessionWorkspaceFailure workspace ->
    workspaceFailure workspace
  Session.LengthSMTLibSessionCapabilityPlanFailure _ ->
    LengthSMTLibLiveSessionInternalFailure
  Session.LengthSMTLibSessionProcessFailure process ->
    sessionProcessFailure process
  Session.LengthSMTLibSessionCapabilityFailure capability ->
    sessionCapabilityFailure capability
  Session.LengthSMTLibSessionBarrierDerivationCollision ->
    LengthSMTLibLiveSessionInternalFailure
  Session.LengthSMTLibSessionTranscriptAccountingMismatch {} ->
    LengthSMTLibLiveSessionTransportFailed
  Session.LengthSMTLibSessionIdentityFingerprintByteLimitExceeded {} ->
    LengthSMTLibLiveSessionResourceLimitExceeded
  Session.LengthSMTLibSessionCleanupFailure ->
    LengthSMTLibLiveSessionCleanupFailed

workspaceFailure
  :: Session.LengthSMTLibSessionWorkspaceFailure
  -> LengthSMTLibLiveSessionFailure
workspaceFailure failure = case failure of
  Session.LengthSMTLibSessionWorkspaceCollisionLimitExceeded {} ->
    LengthSMTLibLiveSessionResourceLimitExceeded
  Session.LengthSMTLibSessionEntropyLengthMismatch {} ->
    LengthSMTLibLiveSessionInternalFailure
  Session.LengthSMTLibSessionWorkspaceRemovalFailed ->
    LengthSMTLibLiveSessionCleanupFailed
  Session.LengthSMTLibSessionWorkspaceProcessCleanupIncomplete ->
    LengthSMTLibLiveSessionCleanupFailed
  _ -> LengthSMTLibLiveSessionWorkspaceUnavailable

sessionProcessFailure
  :: Process.LengthSMTLibProcessError
  -> LengthSMTLibLiveSessionFailure
sessionProcessFailure failure = case Process.lengthSMTLibProcessErrorClass failure of
  Process.LengthSMTLibProcessDeadlineExceeded ->
    LengthSMTLibLiveSessionDeadlineExceeded
  Process.LengthSMTLibProcessWorkingDirectoryNotAbsolute ->
    LengthSMTLibLiveSessionWorkspaceUnavailable
  Process.LengthSMTLibProcessWorkingDirectoryUnavailable ->
    LengthSMTLibLiveSessionWorkspaceUnavailable
  Process.LengthSMTLibProcessWorkingDirectoryNotEmpty ->
    LengthSMTLibLiveSessionWorkspaceUnavailable
  Process.LengthSMTLibProcessExecutableUnavailable ->
    LengthSMTLibLiveSessionExecutableUnavailable
  Process.LengthSMTLibProcessExecutableNotRegular ->
    LengthSMTLibLiveSessionExecutableRejected
  Process.LengthSMTLibProcessExecutableMetadataChanged ->
    LengthSMTLibLiveSessionExecutableRejected
  Process.LengthSMTLibProcessExecutableDigestMismatch ->
    LengthSMTLibLiveSessionExecutableRejected
  Process.LengthSMTLibProcessSpawnFailed ->
    LengthSMTLibLiveSessionLaunchFailed
  Process.LengthSMTLibProcessMissingPipe ->
    LengthSMTLibLiveSessionLaunchFailed
  Process.LengthSMTLibProcessHandleConfigurationFailed ->
    LengthSMTLibLiveSessionLaunchFailed
  Process.LengthSMTLibProcessNonPositiveLimit ->
    LengthSMTLibLiveSessionResourceLimitExceeded
  Process.LengthSMTLibProcessLimitConversionOverflow ->
    LengthSMTLibLiveSessionResourceLimitExceeded
  Process.LengthSMTLibProcessExecutableByteLimitExceeded ->
    LengthSMTLibLiveSessionResourceLimitExceeded
  Process.LengthSMTLibProcessStdoutByteLimitExceeded ->
    LengthSMTLibLiveSessionResourceLimitExceeded
  Process.LengthSMTLibProcessInternalFailure ->
    LengthSMTLibLiveSessionInternalFailure
  _ -> LengthSMTLibLiveSessionTransportFailed

queryFailure
  :: Session.LengthSMTLibQueryRunFailure
  -> LengthSMTLibLiveQueryFailure
queryFailure failure = case failure of
  Session.LengthSMTLibQueryWorkerClosing ->
    LengthSMTLibLiveQuerySessionUnavailable
  Session.LengthSMTLibQueryWorkerSpent ->
    LengthSMTLibLiveQuerySessionUnavailable
  Session.LengthSMTLibQueryLimitExceeded limit observed ->
    LengthSMTLibLiveQueryLimitExceeded limit observed
  Session.LengthSMTLibQueryProtocolPlanFailure plan ->
    queryProtocolPlanFailure plan
  Session.LengthSMTLibQueryProcessStdoutCapacityTooSmall {} ->
    LengthSMTLibLiveQueryResourceLimitExceeded
  Session.LengthSMTLibQueryBarrierCollision ->
    LengthSMTLibLiveQueryInternalFailure
  Session.LengthSMTLibQueryDeadlineFailure _ ->
    LengthSMTLibLiveQueryDeadlineExceeded
  Session.LengthSMTLibQueryProcessFailure process ->
    queryProcessFailure process
  Session.LengthSMTLibQueryProtocolFailure protocol ->
    queryProtocolFailure protocol
  Session.LengthSMTLibQueryTranscriptAccountingMismatch {} ->
    LengthSMTLibLiveQueryTransportFailed
  Session.LengthSMTLibQueryStderrAccountingMismatch {} ->
    LengthSMTLibLiveQueryTransportFailed
  Session.LengthSMTLibQueryModelFailure _ ->
    LengthSMTLibLiveQueryCounterexampleRejected
  Session.LengthSMTLibQueryModelNotCounterexample ->
    LengthSMTLibLiveQueryCounterexampleRejected
  Session.LengthSMTLibQueryRunIdentityAdmissionTooSmall {} ->
    LengthSMTLibLiveQueryResourceLimitExceeded
  Session.LengthSMTLibQueryRunIdentityFingerprintByteLimitExceeded {} ->
    LengthSMTLibLiveQueryResourceLimitExceeded
  Session.LengthSMTLibQueryInternalFailure ->
    LengthSMTLibLiveQueryInternalFailure

queryProcessFailure
  :: Process.LengthSMTLibProcessError
  -> LengthSMTLibLiveQueryFailure
queryProcessFailure failure = case Process.lengthSMTLibProcessErrorClass failure of
  Process.LengthSMTLibProcessCancelled ->
    LengthSMTLibLiveQuerySessionUnavailable
  Process.LengthSMTLibProcessClosed ->
    LengthSMTLibLiveQuerySessionUnavailable
  Process.LengthSMTLibProcessDeadlineExceeded ->
    LengthSMTLibLiveQueryDeadlineExceeded
  Process.LengthSMTLibProcessNonPositiveLimit ->
    LengthSMTLibLiveQueryResourceLimitExceeded
  Process.LengthSMTLibProcessLimitConversionOverflow ->
    LengthSMTLibLiveQueryResourceLimitExceeded
  Process.LengthSMTLibProcessExecutableByteLimitExceeded ->
    LengthSMTLibLiveQueryResourceLimitExceeded
  Process.LengthSMTLibProcessStdoutByteLimitExceeded ->
    LengthSMTLibLiveQueryResourceLimitExceeded
  Process.LengthSMTLibProcessInternalFailure ->
    LengthSMTLibLiveQueryInternalFailure
  _ -> LengthSMTLibLiveQueryTransportFailed

queryProtocolPlanFailure
  :: Protocol.LengthSMTLibProtocolPlanError
  -> LengthSMTLibLiveQueryFailure
queryProtocolPlanFailure failure = case failure of
  Protocol.LengthSMTLibProtocolRequiredLimitTooSmall {} ->
    LengthSMTLibLiveQueryConfigurationRejected
  Protocol.LengthSMTLibProtocolMinimumStdoutByteLimitExceeded {} ->
    LengthSMTLibLiveQueryResourceLimitExceeded
  Protocol.LengthSMTLibProtocolPlanFingerprintByteLimitExceeded {} ->
    LengthSMTLibLiveQueryResourceLimitExceeded
  Protocol.LengthSMTLibProtocolBarrierNonceError {} ->
    LengthSMTLibLiveQueryInternalFailure
  Protocol.LengthSMTLibProtocolMissingInputValueBarrierNonce ->
    LengthSMTLibLiveQueryInternalFailure
  Protocol.LengthSMTLibProtocolUnexpectedInputValueBarrierNonce ->
    LengthSMTLibLiveQueryInternalFailure
  Protocol.LengthSMTLibProtocolRepeatedBarrierNonce ->
    LengthSMTLibLiveQueryInternalFailure

sessionCapabilityFailure
  :: Capability.LengthSMTLibCapabilityError
  -> LengthSMTLibLiveSessionFailure
sessionCapabilityFailure failure = case failure of
  Capability.LengthSMTLibCapabilityCumulativeOutputByteLimitExceeded {} ->
    LengthSMTLibLiveSessionResourceLimitExceeded
  Capability.LengthSMTLibCapabilityFramingFailure _ framing
    | streamFramingLimitFailure framing ->
        LengthSMTLibLiveSessionResourceLimitExceeded
  _ -> LengthSMTLibLiveSessionCapabilityRejected

queryProtocolFailure
  :: Protocol.LengthSMTLibProtocolError
  -> LengthSMTLibLiveQueryFailure
queryProtocolFailure failure = case failure of
  Protocol.LengthSMTLibProtocolCumulativeStdoutByteLimitExceeded {} ->
    LengthSMTLibLiveQueryResourceLimitExceeded
  Protocol.LengthSMTLibProtocolFramingFailure _ framing
    | streamFramingLimitFailure framing ->
        LengthSMTLibLiveQueryResourceLimitExceeded
  Protocol.LengthSMTLibProtocolResponseFailure _ response
    | responseLimitFailure response ->
        LengthSMTLibLiveQueryResourceLimitExceeded
  _ -> LengthSMTLibLiveQueryProtocolRejected

streamFramingLimitFailure :: Stream.SMTLibStreamFramingError -> Bool
streamFramingLimitFailure failure = case failure of
  Stream.SMTLibStreamTotalByteLimitExceeded {} -> True
  Stream.SMTLibStreamFrameByteLimitExceeded {} -> True
  Stream.SMTLibStreamNestingDepthLimitExceeded {} -> True
  _ -> False

responseLimitFailure :: Response.LengthSMTLibResponseError -> Bool
responseLimitFailure failure = case failure of
  Response.LengthSMTLibResponseSyntaxError syntax ->
    responseSyntaxLimitFailure syntax
  _ -> False

responseSyntaxLimitFailure :: SMTResponse.SMTLibParseError -> Bool
responseSyntaxLimitFailure failure = case failure of
  SMTResponse.SMTLibResponseByteLimitExceeded {} -> True
  SMTResponse.SMTLibNestingDepthLimitExceeded {} -> True
  SMTResponse.SMTLibNodeLimitExceeded {} -> True
  SMTResponse.SMTLibTokenByteLimitExceeded {} -> True
  SMTResponse.SMTLibNumeralBitLimitExceeded {} -> True
  _ -> False

sessionCleanupIncomplete :: Session.LengthSMTLibSessionCleanupStatus -> Bool
sessionCleanupIncomplete cleanup =
  Session.lengthSMTLibSessionProcessCleanupThrew cleanup ||
  maybe False processCleanupIncomplete
    (Session.lengthSMTLibSessionProcessCleanupStatus cleanup) ||
  case Session.lengthSMTLibSessionWorkspaceCleanupStatus cleanup of
    Session.LengthSMTLibSessionWorkspaceNotAllocated -> False
    Session.LengthSMTLibSessionWorkspaceRemoved _ -> False
    Session.LengthSMTLibSessionWorkspaceRetained {} -> True
    Session.LengthSMTLibSessionWorkspaceCleanupIncomplete {} -> True

processCleanupIncomplete :: Process.LengthSMTLibProcessCleanupStatus -> Bool
processCleanupIncomplete cleanup =
  Process.lengthSMTLibProcessCleanupEscalation cleanup ==
      Process.LengthSMTLibProcessCleanupIncomplete ||
  Process.lengthSMTLibProcessCleanupFailureCount cleanup /= 0 ||
  not (Process.lengthSMTLibProcessCleanupReadersStopped cleanup)
