{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

-- | A narrow live Length/Z3 boundary.
--
-- This module owns one capability-probed worker only for the dynamic extent of
-- 'withLengthSMTLibLiveSession'.  Public callers can submit sealed scalar or
-- exact binary-product spine queries and inspect status and heuristic strength,
-- then consume a completed live observation's exact nominal query association
-- and independently replayed counterexample evidence only through the matching
-- 'replayLengthSMTLibLiveQueryObservation' or
-- 'replayLengthSpinePairSMTLibLiveQueryObservation' gate.  They cannot inspect
-- or retain a process handle, cancellation token, executable or workspace path,
-- barrier, ordinal, transcript, decoded valuation, transport counter, or
-- reversible run identity.
--
-- The common readiness probe establishes only the exact QF_LIA, reset, status,
-- input-value, framing, and transport profile.  Product protocol, run,
-- observation, and evidence authority remain nominally distinct from scalar
-- authority.  One scope admits 64 scalar-plus-product transactions in total,
-- not 64 of each.
--
-- Solver status remains an observation.  In particular, @unsat@ is relative to
-- the checked encoding and every status is restricted to
-- 'HeuristicRankingOnly'.  Only optional 'BehavioralEvidence' has survived
-- independent domain replay against the exact query problem.  The matching
-- public replay gate checks complete query identity before inspecting it.
--
-- The legacy entrance retains private opener/finalizer deadlines and one host
-- deadline per query.  The additive budgeted entrances cap opening and every
-- query by one shared absolute usable-work deadline while retaining fresh
-- finalizer/cleanup windows.  They do not interrupt arbitrary callback IO and
-- therefore make no hard whole-callback wall-clock claim.  Callback exceptions,
-- including asynchronous exceptions, are rethrown after the private owner has
-- started durable cleanup.
module Language.Haskell.Synthesis.Semantic.Length.SMTLib.Live
  ( LengthSMTLibLiveSession
  , LengthSMTLibLiveUsableWorkBudgetSource (..)
  , LengthSMTLibLiveUsableWorkBudget
  , LengthSMTLibLiveUsableWorkBudgetError (..)
  , mkLengthSMTLibLiveUsableWorkBudget
  , LengthSMTLibLiveUsableWorkDeadline
  , withLengthSMTLibLiveUsableWorkDeadline
  , withLengthSMTLibLiveSessionUnderDeadline
  , withLengthSMTLibLiveSessionWithUsableWorkBudget
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
  , LengthSpinePairSMTLibLiveQueryObservation
  , LengthSpinePairSMTLibLiveQueryFailure (..)
  , LengthSpinePairSMTLibLiveQueryError
  , lengthSpinePairSMTLibLiveQueryPrimaryFailure
  , lengthSpinePairSMTLibLiveQueryCleanupIncomplete
  , LengthSpinePairSMTLibLiveObservationReplayError (..)
  , runLengthSpinePairSMTLibLiveQuery
  , replayLengthSpinePairSMTLibLiveQueryObservation
  , lengthSpinePairSMTLibLiveQueryObservationSolverStatus
  , lengthSpinePairSMTLibLiveQueryObservationResultStrength
  , lengthSpinePairSMTLibLiveQueryObservationUse
  ) where

import Control.DeepSeq (NFData (rnf))
import Data.Word (Word64)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Fingerprint (Fingerprint)
import qualified Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol
  as Protocol
import qualified Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol.SpinePair
  as SpinePairProtocol
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
  ( FiniteBinaryProductSpineLengthsV1
  , FiniteListSpineLengthV1
  )
import Language.Haskell.Synthesis.Semantic.Length.Evaluate
  ( LengthEvaluationLimits
  , ValidatedLengthCounterexample
  , ValidatedLengthSpinePairCounterexample
  )
import Language.Haskell.Synthesis.Semantic.Length.SMTLib
  ( LengthSMTLibQuery
  , LengthSMTLibQueryFingerprintSubject
  , LengthSpinePairSMTLibQuery
  , LengthSpinePairSMTLibQueryFingerprintSubject
  , lengthSMTLibQueryBehavioralProblem
  , lengthSMTLibQueryFingerprint
  , lengthSpinePairSMTLibQueryBehavioralProblem
  , lengthSpinePairSMTLibQueryFingerprint
  )
import Language.Haskell.Synthesis.Semantic.Length.SMTLib.Execution
  ( LengthSMTLibExecutionConfig )
import Language.Haskell.Synthesis.Semantic.Observation
  ( SolverObservation (..)
  , SolverStatus (..)
  , solverObservationStatus
  )
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

-- | Pure source for one shared usable-work window.  The window starts when
-- 'withLengthSMTLibLiveUsableWorkDeadline' captures its monotonic deadline.
data LengthSMTLibLiveUsableWorkBudgetSource =
  LengthSMTLibLiveUsableWorkBudgetSource
    { lengthSMTLibLiveUsableWorkBudgetSourceMilliseconds :: Int }
  deriving (Eq, Ord, Show)

instance NFData LengthSMTLibLiveUsableWorkBudgetSource where
  rnf (LengthSMTLibLiveUsableWorkBudgetSource milliseconds) =
    rnf milliseconds

-- | Validated positive duration.  It contains no process, clock observation,
-- executable material, or cancellation authority.
newtype LengthSMTLibLiveUsableWorkBudget =
  LengthSMTLibLiveUsableWorkBudget Int
  deriving (Eq, Ord)

instance NFData LengthSMTLibLiveUsableWorkBudget where
  rnf (LengthSMTLibLiveUsableWorkBudget milliseconds) = rnf milliseconds

-- | Closed pure validation failures for the shared duration.
data LengthSMTLibLiveUsableWorkBudgetError
  = LengthSMTLibLiveUsableWorkBudgetNonPositive !Int
  | LengthSMTLibLiveUsableWorkBudgetMicrosecondsOverflow !Int
  deriving (Eq, Ord, Show)

instance NFData LengthSMTLibLiveUsableWorkBudgetError where
  rnf failure = case failure of
    LengthSMTLibLiveUsableWorkBudgetNonPositive milliseconds ->
      rnf milliseconds
    LengthSMTLibLiveUsableWorkBudgetMicrosecondsOverflow milliseconds ->
      rnf milliseconds

-- | Validate a duration without reading a clock or performing IO.  Both the
-- host wait conversion and the monotonic nanosecond delta must be representable.
mkLengthSMTLibLiveUsableWorkBudget
  :: LengthSMTLibLiveUsableWorkBudgetSource
  -> Either
      LengthSMTLibLiveUsableWorkBudgetError
      LengthSMTLibLiveUsableWorkBudget
mkLengthSMTLibLiveUsableWorkBudget
    (LengthSMTLibLiveUsableWorkBudgetSource milliseconds)
  | milliseconds <= 0 = Left
      $ LengthSMTLibLiveUsableWorkBudgetNonPositive milliseconds
  | toInteger milliseconds * 1000 > toInteger (maxBound :: Int) ||
      toInteger milliseconds * 1000000 > toInteger (maxBound :: Word64) = Left
        $ LengthSMTLibLiveUsableWorkBudgetMicrosecondsOverflow milliseconds
  | otherwise = Right $ LengthSMTLibLiveUsableWorkBudget milliseconds

-- | Generative, opaque shared absolute deadline.  Its constructor and clock
-- value cannot cross the public boundary.
data LengthSMTLibLiveUsableWorkDeadline budget =
  LengthSMTLibLiveUsableWorkDeadline
    !(Session.LengthSMTLibSessionUsableWorkDeadline budget)

type role LengthSMTLibLiveUsableWorkDeadline nominal

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

-- | The sanitized failure class that ended the session.
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

-- | The sanitized failure class that spent this query transaction.
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

-- | Nominal, byte-free failure classes for a binary-product query sharing the
-- same scoped worker.  These constructors carry no scalar problem or evidence
-- authority.
data LengthSpinePairSMTLibLiveQueryFailure
  = LengthSpinePairSMTLibLiveQuerySessionUnavailable
  | LengthSpinePairSMTLibLiveQueryLimitExceeded !Natural !Natural
  | LengthSpinePairSMTLibLiveQueryDeadlineExceeded
  | LengthSpinePairSMTLibLiveQueryConfigurationRejected
  | LengthSpinePairSMTLibLiveQueryResourceLimitExceeded
  | LengthSpinePairSMTLibLiveQueryTransportFailed
  | LengthSpinePairSMTLibLiveQueryProtocolRejected
  | LengthSpinePairSMTLibLiveQueryCounterexampleRejected
  | LengthSpinePairSMTLibLiveQueryInternalFailure
  deriving (Eq, Ord, Show)

instance NFData LengthSpinePairSMTLibLiveQueryFailure where
  rnf failure = case failure of
    LengthSpinePairSMTLibLiveQueryLimitExceeded limit observed ->
      rnf limit `seq` rnf observed
    _ -> failure `seq` ()

-- | Sanitized product-query failure.  The nominal product protocol and replay
-- diagnostics cannot be projected through the scalar error vocabulary.
data LengthSpinePairSMTLibLiveQueryError =
  LengthSpinePairSMTLibLiveQueryError
    !LengthSpinePairSMTLibLiveQueryFailure
    !Bool
  deriving (Eq, Ord, Show)

instance NFData LengthSpinePairSMTLibLiveQueryError where
  rnf (LengthSpinePairSMTLibLiveQueryError failure incomplete) =
    rnf failure `seq` rnf incomplete

-- | The sanitized failure class that spent this product query transaction.
lengthSpinePairSMTLibLiveQueryPrimaryFailure
  :: LengthSpinePairSMTLibLiveQueryError
  -> LengthSpinePairSMTLibLiveQueryFailure
lengthSpinePairSMTLibLiveQueryPrimaryFailure
    (LengthSpinePairSMTLibLiveQueryError failure _) = failure

-- | Whether teardown following this query reported incomplete process
-- cleanup.
lengthSpinePairSMTLibLiveQueryCleanupIncomplete
  :: LengthSpinePairSMTLibLiveQueryError
  -> Bool
lengthSpinePairSMTLibLiveQueryCleanupIncomplete
    (LengthSpinePairSMTLibLiveQueryError _ incomplete) = incomplete

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

-- | Why a product live observation could not be consumed with one exact
-- product query.  Scalar query fingerprints and scalar evidence cannot enter
-- this gate.
data LengthSpinePairSMTLibLiveObservationReplayError
  = LengthSpinePairSMTLibLiveObservationQueryFingerprintMismatch
  | LengthSpinePairSMTLibLiveObservationEvidenceProblemMismatch
      !ReplayMismatch
  deriving (Eq, Ord, Show)

instance NFData LengthSpinePairSMTLibLiveObservationReplayError where
  rnf failure = case failure of
    LengthSpinePairSMTLibLiveObservationQueryFingerprintMismatch -> ()
    LengthSpinePairSMTLibLiveObservationEvidenceProblemMismatch mismatch ->
      rnf mismatch

-- | Safe projection of one completed query.  It freshly copies only bounded
-- association and authority fields instead of wrapping the private run.  The
-- query fingerprint and optional evidence have no public projection; the
-- replay gate below is their only public semantic extraction edge from this
-- live observation.
data LengthSMTLibLiveQueryObservation epoch identity local =
  LengthSMTLibLiveQueryObservation
    !(Fingerprint LengthSMTLibQueryFingerprintSubject)
    !LengthSMTLibLiveSolverObservation

type LengthSMTLibLiveSolverObservation = SolverObservation
  (Maybe
    (BehavioralEvidence
      FiniteListSpineLengthV1
      ValidatedLengthCounterexample))
  ()
  ()

type role LengthSMTLibLiveQueryObservation nominal nominal nominal

instance NFData
    (LengthSMTLibLiveQueryObservation epoch identity local) where
  rnf (LengthSMTLibLiveQueryObservation query observation) =
    rnf query `seq` rnf observation

-- | Opaque product-domain observation retaining one exact product query
-- fingerprint and one status-indexed outcome.  Its optional receipt has
-- already survived independent two-component replay, but remains accessible
-- only through 'replayLengthSpinePairSMTLibLiveQueryObservation'.
data LengthSpinePairSMTLibLiveQueryObservation epoch identity local =
  LengthSpinePairSMTLibLiveQueryObservation
    !(Fingerprint LengthSpinePairSMTLibQueryFingerprintSubject)
    !LengthSpinePairSMTLibLiveSolverObservation

type LengthSpinePairSMTLibLiveSolverObservation = SolverObservation
  (Maybe
    (BehavioralEvidence
      FiniteBinaryProductSpineLengthsV1
      ValidatedLengthSpinePairCounterexample))
  ()
  ()

type role LengthSpinePairSMTLibLiveQueryObservation nominal nominal nominal

instance NFData
    (LengthSpinePairSMTLibLiveQueryObservation epoch identity local) where
  rnf (LengthSpinePairSMTLibLiveQueryObservation query observation) =
    rnf query `seq` rnf observation

-- | The fixed private session default.  A scope admits at most this many
-- serial queries in total across scalar and product domains and rejects
-- maximum-plus-one before writing it.  The current exact value is 64.
defaultLengthSMTLibLiveSessionMaximumQueries :: Natural
defaultLengthSMTLibLiveSessionMaximumQueries =
  Session.lengthSMTLibSessionLimitSourceMaximumQueries
    Session.defaultLengthSMTLibSessionLimitSource

-- | Capture one absolute monotonic deadline and lend only its generative,
-- opaque token.  This owner does not interrupt arbitrary callback IO, but it
-- rejects a normally returning callback after expiry.  An overrun can be
-- rejected earlier by the next live operation or by a budgeted session
-- immediately after its callback returns, before fresh finalizer/cleanup
-- windows begin.  Callback exceptions remain authoritative and are rethrown.
withLengthSMTLibLiveUsableWorkDeadline
  :: forall result. LengthSMTLibLiveUsableWorkBudget
  -> (forall budget. LengthSMTLibLiveUsableWorkDeadline budget -> IO result)
  -> IO (Either LengthSMTLibLiveSessionError result)
withLengthSMTLibLiveUsableWorkDeadline
    (LengthSMTLibLiveUsableWorkBudget milliseconds) use = do
  result <- Session.withLengthSMTLibSessionUsableWorkDeadline milliseconds
    $ use . LengthSMTLibLiveUsableWorkDeadline
  pure $ case result of
    Left failure -> Left $ LengthSMTLibLiveSessionError
      (sessionFailure failure) False
    Right value -> Right value

-- | Open one capability-probed worker under an already captured shared
-- deadline.  The effective opener and each effective query deadline are the
-- earlier of their fresh local deadline and this token; the shared deadline
-- wins an exact tie.  Cleanup and the final readiness observation retain their
-- established fresh private windows.
withLengthSMTLibLiveSessionUnderDeadline
  :: LengthSMTLibLiveUsableWorkDeadline budget
  -> LengthSMTLibExecutionConfig
  -> (forall epoch. LengthSMTLibLiveSession epoch -> IO result)
  -> IO (Either LengthSMTLibLiveSessionError result)
withLengthSMTLibLiveSessionUnderDeadline
    (LengthSMTLibLiveUsableWorkDeadline deadline) execution use =
  case defaultLiveSessionConfig execution of
    Left failure -> pure $ Left $ LengthSMTLibLiveSessionError failure False
    Right config -> do
      result <- Session.withLengthSMTLibReadyWorkerUnderDeadline deadline config
        $ use . LengthSMTLibLiveSession
      pure $ case result of
        Left failure -> Left $ sanitizeSessionError failure
        Right value -> Right value

-- | Convenience entrance which captures the shared deadline immediately
-- before session configuration/opening.  The two-step token API remains
-- available when pure caller work must consume part of the same window before
-- a deferred live session is opened.  This entrance relies on the session's
-- callback-return check and deliberately performs no second check after its
-- fresh final-readiness and cleanup windows.
withLengthSMTLibLiveSessionWithUsableWorkBudget
  :: LengthSMTLibLiveUsableWorkBudget
  -> LengthSMTLibExecutionConfig
  -> (forall epoch. LengthSMTLibLiveSession epoch -> IO result)
  -> IO (Either LengthSMTLibLiveSessionError result)
withLengthSMTLibLiveSessionWithUsableWorkBudget
    (LengthSMTLibLiveUsableWorkBudget milliseconds) execution use = do
  scoped <-
    Session.withLengthSMTLibSessionUsableWorkDeadlineForBudgetedSession
      milliseconds $ \deadline ->
        withLengthSMTLibLiveSessionUnderDeadline
          (LengthSMTLibLiveUsableWorkDeadline deadline) execution use
  pure $ case scoped of
    Left failure -> Left $ LengthSMTLibLiveSessionError
      (sessionFailure failure) False
    Right result -> result

-- | Open, capability-probe, lend, and close one common-QF_LIA worker using
-- validated private transport/protocol defaults and the caller's public
-- execution policy.  Readiness grants transport capability only; each query
-- path retains its own nominal protocol and behavioral authority.
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
    Right run -> Right $ retainLengthSMTLibLiveQueryObservation
      (lengthSMTLibQueryFingerprint query)
      (Session.lengthSMTLibQueryRunObservation run)

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
      lengthSMTLibLiveQueryObservationSolverObservation observation of
    SatisfiableObservation Nothing -> Right Nothing
    SatisfiableObservation (Just evidence) -> case replayBehavioralEvidence
        (lengthSMTLibQueryBehavioralProblem query) evidence of
      Left mismatch -> Left
        $ LengthSMTLibLiveObservationEvidenceProblemMismatch mismatch
      Right receipt -> Right $ Just receipt
    UnsatisfiableObservation () -> Right Nothing
    UnknownObservation () -> Right Nothing

-- Private association projection used only by the checked replay gate.  It is
-- intentionally not exported: callers cannot inspect a query key without also
-- passing through evidence-consumption precedence.
lengthSMTLibLiveQueryObservationQueryFingerprint
  :: LengthSMTLibLiveQueryObservation epoch identity local
  -> Fingerprint LengthSMTLibQueryFingerprintSubject
lengthSMTLibLiveQueryObservationQueryFingerprint
    (LengthSMTLibLiveQueryObservation query _) = query

-- | The raw solver status of this observation.  A status is a heuristic
-- report, never proof or pruning authority.
lengthSMTLibLiveQueryObservationSolverStatus
  :: LengthSMTLibLiveQueryObservation epoch identity local
  -> SolverStatus
lengthSMTLibLiveQueryObservationSolverStatus =
  solverObservationStatus . lengthSMTLibLiveQueryObservationSolverObservation

-- | The conservative strength derived from the status alone.
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

-- Private whole-outcome projection used only after exact query association has
-- succeeded. Keeping this selector separate preserves the gate's established
-- demand order without exposing detached status-specific evidence to callers.
lengthSMTLibLiveQueryObservationSolverObservation
  :: LengthSMTLibLiveQueryObservation epoch identity local
  -> LengthSMTLibLiveSolverObservation
lengthSMTLibLiveQueryObservationSolverObservation
    (LengthSMTLibLiveQueryObservation _ observation) = observation

-- Preserve the old strict optional-evidence spine at this public opaque owner
-- without changing the deliberately lazy generic observation constructors.
retainLengthSMTLibLiveQueryObservation
  :: Fingerprint LengthSMTLibQueryFingerprintSubject
  -> Session.LengthSMTLibQueryRunObservation
  -> LengthSMTLibLiveQueryObservation epoch identity local
retainLengthSMTLibLiveQueryObservation query observation = case observation of
  SatisfiableObservation evidence -> evidence `seq`
    LengthSMTLibLiveQueryObservation query observation
  UnsatisfiableObservation () ->
    LengthSMTLibLiveQueryObservation query observation
  UnknownObservation () -> LengthSMTLibLiveQueryObservation query observation

-- | Run one nominal product query in the same serial lease, 64-query budget,
-- and ordinal space as scalar queries.  It seals and records a distinct product
-- protocol and run identity.  A values-policy satisfiable result succeeds only
-- after exact product-input decoding and independent recomputation of both
-- product result components.  The returned status remains heuristic.
runLengthSpinePairSMTLibLiveQuery
  :: LengthEvaluationLimits
  -> LengthSMTLibLiveSession epoch
  -> LengthSpinePairSMTLibQuery identity local
  -> IO
      (Either
        LengthSpinePairSMTLibLiveQueryError
        (LengthSpinePairSMTLibLiveQueryObservation epoch identity local))
runLengthSpinePairSMTLibLiveQuery evaluationLimits
    (LengthSMTLibLiveSession worker) query = do
  result <- Session.runLengthSpinePairSMTLibReadyWorkerQuery
    evaluationLimits worker query
  pure $ case result of
    Left failure -> Left $ sanitizeSpinePairQueryError failure
    Right run -> Right $ retainLengthSpinePairSMTLibLiveQueryObservation
      (lengthSpinePairSMTLibQueryFingerprint query)
      (Session.lengthSpinePairSMTLibQueryRunObservation run)

-- | Consume the safe semantic payload of a completed product observation with
-- the exact sealed product query supplied by its consumer.
--
-- The product query fingerprint is compared before the hidden observation is
-- inspected.  Optional evidence is then replayed against that query's retained
-- product @BehavioralProblem@.  'Nothing' grants no evidence, including for an
-- @unsat@ or @unknown@ status; 'Just' reveals only the already independently
-- validated two-component counterexample receipt.  Solver status remains
-- 'HeuristicRankingOnly'.
replayLengthSpinePairSMTLibLiveQueryObservation
  :: LengthSpinePairSMTLibQuery identity local
  -> LengthSpinePairSMTLibLiveQueryObservation epoch identity local
  -> Either
      LengthSpinePairSMTLibLiveObservationReplayError
      (Maybe ValidatedLengthSpinePairCounterexample)
replayLengthSpinePairSMTLibLiveQueryObservation query observation
  | lengthSpinePairSMTLibQueryFingerprint query /=
      lengthSpinePairSMTLibLiveQueryObservationQueryFingerprint observation =
        Left LengthSpinePairSMTLibLiveObservationQueryFingerprintMismatch
  | otherwise = case
      lengthSpinePairSMTLibLiveQueryObservationSolverObservation observation of
    SatisfiableObservation Nothing -> Right Nothing
    SatisfiableObservation (Just evidence) -> case replayBehavioralEvidence
        (lengthSpinePairSMTLibQueryBehavioralProblem query) evidence of
      Left mismatch -> Left
        $ LengthSpinePairSMTLibLiveObservationEvidenceProblemMismatch mismatch
      Right receipt -> Right $ Just receipt
    UnsatisfiableObservation () -> Right Nothing
    UnknownObservation () -> Right Nothing

lengthSpinePairSMTLibLiveQueryObservationQueryFingerprint
  :: LengthSpinePairSMTLibLiveQueryObservation epoch identity local
  -> Fingerprint LengthSpinePairSMTLibQueryFingerprintSubject
lengthSpinePairSMTLibLiveQueryObservationQueryFingerprint
    (LengthSpinePairSMTLibLiveQueryObservation query _) = query

-- | Project the heuristic solver status without exposing product evidence.
lengthSpinePairSMTLibLiveQueryObservationSolverStatus
  :: LengthSpinePairSMTLibLiveQueryObservation epoch identity local
  -> SolverStatus
lengthSpinePairSMTLibLiveQueryObservationSolverStatus =
  solverObservationStatus .
    lengthSpinePairSMTLibLiveQueryObservationSolverObservation

-- | Derive the raw heuristic strength from the product observation's status.
lengthSpinePairSMTLibLiveQueryObservationResultStrength
  :: LengthSpinePairSMTLibLiveQueryObservation epoch identity local
  -> RawResultStrength
lengthSpinePairSMTLibLiveQueryObservationResultStrength observation =
  solverStatusStrength
    $ lengthSpinePairSMTLibLiveQueryObservationSolverStatus observation

-- | Product live observations, including those carrying separately validated
-- receipts, have heuristic ranking authority only.
lengthSpinePairSMTLibLiveQueryObservationUse
  :: LengthSpinePairSMTLibLiveQueryObservation epoch identity local
  -> RawObservationUse
lengthSpinePairSMTLibLiveQueryObservationUse _ = HeuristicRankingOnly

lengthSpinePairSMTLibLiveQueryObservationSolverObservation
  :: LengthSpinePairSMTLibLiveQueryObservation epoch identity local
  -> LengthSpinePairSMTLibLiveSolverObservation
lengthSpinePairSMTLibLiveQueryObservationSolverObservation
    (LengthSpinePairSMTLibLiveQueryObservation _ observation) = observation

retainLengthSpinePairSMTLibLiveQueryObservation
  :: Fingerprint LengthSpinePairSMTLibQueryFingerprintSubject
  -> Session.LengthSpinePairSMTLibQueryRunObservation
  -> LengthSpinePairSMTLibLiveQueryObservation epoch identity local
retainLengthSpinePairSMTLibLiveQueryObservation query observation =
  case observation of
    SatisfiableObservation evidence -> evidence `seq`
      LengthSpinePairSMTLibLiveQueryObservation query observation
    UnsatisfiableObservation () ->
      LengthSpinePairSMTLibLiveQueryObservation query observation
    UnknownObservation () ->
      LengthSpinePairSMTLibLiveQueryObservation query observation

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

sanitizeSpinePairQueryError
  :: Session.LengthSpinePairSMTLibQueryRunError
  -> LengthSpinePairSMTLibLiveQueryError
sanitizeSpinePairQueryError failure = LengthSpinePairSMTLibLiveQueryError
  (spinePairQueryFailure
    $ Session.lengthSpinePairSMTLibQueryRunPrimaryFailure failure)
  (maybe False processCleanupIncomplete
    $ Session.lengthSpinePairSMTLibQueryRunProcessCleanupStatus failure)

sessionConfigFailure
  :: Session.LengthSMTLibSessionConfigError
  -> LengthSMTLibLiveSessionFailure
sessionConfigFailure _ = LengthSMTLibLiveSessionInternalFailure

sessionFailure
  :: Session.LengthSMTLibSessionError
  -> LengthSMTLibLiveSessionFailure
sessionFailure failure = case failure of
  Session.LengthSMTLibSessionDeadlineFailure process ->
    sessionProcessFailure process
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

spinePairQueryFailure
  :: Session.LengthSpinePairSMTLibQueryRunFailure
  -> LengthSpinePairSMTLibLiveQueryFailure
spinePairQueryFailure failure = case failure of
  Session.LengthSpinePairSMTLibQueryWorkerClosing ->
    LengthSpinePairSMTLibLiveQuerySessionUnavailable
  Session.LengthSpinePairSMTLibQueryWorkerSpent ->
    LengthSpinePairSMTLibLiveQuerySessionUnavailable
  Session.LengthSpinePairSMTLibQueryLimitExceeded limit observed ->
    LengthSpinePairSMTLibLiveQueryLimitExceeded limit observed
  Session.LengthSpinePairSMTLibQueryProtocolPlanFailure plan ->
    spinePairQueryProtocolPlanFailure plan
  Session.LengthSpinePairSMTLibQueryProcessStdoutCapacityTooSmall {} ->
    LengthSpinePairSMTLibLiveQueryResourceLimitExceeded
  Session.LengthSpinePairSMTLibQueryBarrierCollision ->
    LengthSpinePairSMTLibLiveQueryInternalFailure
  Session.LengthSpinePairSMTLibQueryDeadlineFailure _ ->
    LengthSpinePairSMTLibLiveQueryDeadlineExceeded
  Session.LengthSpinePairSMTLibQueryProcessFailure process ->
    spinePairQueryProcessFailure process
  Session.LengthSpinePairSMTLibQueryProtocolFailure protocol ->
    spinePairQueryProtocolFailure protocol
  Session.LengthSpinePairSMTLibQueryTranscriptAccountingMismatch {} ->
    LengthSpinePairSMTLibLiveQueryTransportFailed
  Session.LengthSpinePairSMTLibQueryStderrAccountingMismatch {} ->
    LengthSpinePairSMTLibLiveQueryTransportFailed
  Session.LengthSpinePairSMTLibQueryModelFailure _ ->
    LengthSpinePairSMTLibLiveQueryCounterexampleRejected
  Session.LengthSpinePairSMTLibQueryModelNotCounterexample ->
    LengthSpinePairSMTLibLiveQueryCounterexampleRejected
  Session.LengthSpinePairSMTLibQueryRunIdentityAdmissionTooSmall {} ->
    LengthSpinePairSMTLibLiveQueryResourceLimitExceeded
  Session.LengthSpinePairSMTLibQueryRunIdentityFingerprintByteLimitExceeded {} ->
    LengthSpinePairSMTLibLiveQueryResourceLimitExceeded
  Session.LengthSpinePairSMTLibQueryInternalFailure ->
    LengthSpinePairSMTLibLiveQueryInternalFailure

spinePairQueryProcessFailure
  :: Process.LengthSMTLibProcessError
  -> LengthSpinePairSMTLibLiveQueryFailure
spinePairQueryProcessFailure failure =
  case Process.lengthSMTLibProcessErrorClass failure of
    Process.LengthSMTLibProcessCancelled ->
      LengthSpinePairSMTLibLiveQuerySessionUnavailable
    Process.LengthSMTLibProcessClosed ->
      LengthSpinePairSMTLibLiveQuerySessionUnavailable
    Process.LengthSMTLibProcessDeadlineExceeded ->
      LengthSpinePairSMTLibLiveQueryDeadlineExceeded
    Process.LengthSMTLibProcessNonPositiveLimit ->
      LengthSpinePairSMTLibLiveQueryResourceLimitExceeded
    Process.LengthSMTLibProcessLimitConversionOverflow ->
      LengthSpinePairSMTLibLiveQueryResourceLimitExceeded
    Process.LengthSMTLibProcessExecutableByteLimitExceeded ->
      LengthSpinePairSMTLibLiveQueryResourceLimitExceeded
    Process.LengthSMTLibProcessStdoutByteLimitExceeded ->
      LengthSpinePairSMTLibLiveQueryResourceLimitExceeded
    Process.LengthSMTLibProcessInternalFailure ->
      LengthSpinePairSMTLibLiveQueryInternalFailure
    _ -> LengthSpinePairSMTLibLiveQueryTransportFailed

spinePairQueryProtocolPlanFailure
  :: SpinePairProtocol.LengthSpinePairSMTLibProtocolPlanError
  -> LengthSpinePairSMTLibLiveQueryFailure
spinePairQueryProtocolPlanFailure failure = case failure of
  SpinePairProtocol.LengthSMTLibProtocolRequiredLimitTooSmall {} ->
    LengthSpinePairSMTLibLiveQueryConfigurationRejected
  SpinePairProtocol.LengthSMTLibProtocolMinimumStdoutByteLimitExceeded {} ->
    LengthSpinePairSMTLibLiveQueryResourceLimitExceeded
  SpinePairProtocol.LengthSMTLibProtocolPlanFingerprintByteLimitExceeded {} ->
    LengthSpinePairSMTLibLiveQueryResourceLimitExceeded
  SpinePairProtocol.LengthSMTLibProtocolBarrierNonceError {} ->
    LengthSpinePairSMTLibLiveQueryInternalFailure
  SpinePairProtocol.LengthSMTLibProtocolMissingInputValueBarrierNonce ->
    LengthSpinePairSMTLibLiveQueryInternalFailure
  SpinePairProtocol.LengthSMTLibProtocolUnexpectedInputValueBarrierNonce ->
    LengthSpinePairSMTLibLiveQueryInternalFailure
  SpinePairProtocol.LengthSMTLibProtocolRepeatedBarrierNonce ->
    LengthSpinePairSMTLibLiveQueryInternalFailure

spinePairQueryProtocolFailure
  :: SpinePairProtocol.LengthSpinePairSMTLibProtocolError
  -> LengthSpinePairSMTLibLiveQueryFailure
spinePairQueryProtocolFailure failure = case failure of
  SpinePairProtocol.LengthSMTLibProtocolCumulativeStdoutByteLimitExceeded {} ->
    LengthSpinePairSMTLibLiveQueryResourceLimitExceeded
  SpinePairProtocol.LengthSMTLibProtocolFramingFailure _ framing
    | streamFramingLimitFailure framing ->
        LengthSpinePairSMTLibLiveQueryResourceLimitExceeded
  SpinePairProtocol.LengthSMTLibProtocolResponseFailure _ response
    | responseLimitFailure response ->
        LengthSpinePairSMTLibLiveQueryResourceLimitExceeded
  _ -> LengthSpinePairSMTLibLiveQueryProtocolRejected

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
