{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Private query-specific association for raw Length SMT-LIB observations.
--
-- The generic behavioral-problem envelope is necessary but not sufficient
-- once a problem has a concrete translation: two translator schemas can
-- describe the same solver-neutral problem.  This module therefore retains
-- the exact Length SMT-LIB query fingerprint beside the generic association.
-- Neither representation constructor nor the nested generic association is
-- public, so a consumer cannot bypass the query check to reach an artifact.
module Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Observation
  ( LengthSMTLibRawSolverObservation
  , AssociatedLengthSMTLibSolverObservation
  , associateLengthSMTLibSolverObservation
  , associatedLengthSMTLibQueryFingerprint
  , associatedLengthSMTLibSolverStatus
  , associatedLengthSMTLibResultStrength
  , associatedLengthSMTLibUse
  , LengthSMTLibObservationReplayError (..)
  , replayAssociatedLengthSMTLibSolverObservation
  ) where

import Control.DeepSeq (NFData (rnf))
import Data.Bifunctor (first)
import GHC.Generics (Generic)

import Language.Haskell.Synthesis.Fingerprint (Fingerprint)
import Language.Haskell.Synthesis.Semantic.Length
  ( FiniteListSpineLengthV1 )
import Language.Haskell.Synthesis.Semantic.Length.SMTLib
  ( LengthSMTLibQuery
  , LengthSMTLibQueryFingerprintSubject
  , lengthSMTLibQueryBehavioralProblem
  , lengthSMTLibQueryFingerprint
  )
import Language.Haskell.Synthesis.Semantic.Observation
  ( SolverObservation
  , SolverStatus
  )
import Language.Haskell.Synthesis.Internal.Semantic.Problem
  ( associatedSolverObservationStatus )
import Language.Haskell.Synthesis.Semantic.Problem
  ( AssociatedObservation
  , BoundedRawArtifact
  , RawObservationUse
  , RawResultStrength
  , ReplayMismatch
  , associateSolverObservation
  , associatedObservationResultStrength
  , associatedObservationUse
  , replayAssociatedObservation
  )

-- | A raw three-valued report whose status-specific bytes have already passed
-- the generic artifact bounds.  Bounded bytes remain untrusted solver output.
type LengthSMTLibRawSolverObservation satisfiable unsatisfiable unknown =
  SolverObservation
    (BoundedRawArtifact satisfiable)
    (BoundedRawArtifact unsatisfiable)
    (BoundedRawArtifact unknown)

-- | Opaque association of one raw report with both the exact checked problem
-- and the exact canonical SMT-LIB query which was purportedly run.
--
-- This is deliberately not an execution receipt.  It does not identify a Z3
-- binary, process, protocol session, parser, deadline, or resource policy.
-- Those facts require a later, independently sealed run identity.
data AssociatedLengthSMTLibSolverObservation
    identity local satisfiable unsatisfiable unknown =
  AssociatedLengthSMTLibSolverObservation
    !(Fingerprint LengthSMTLibQueryFingerprintSubject)
    !(AssociatedObservation
        FiniteListSpineLengthV1
        (LengthSMTLibRawSolverObservation
          satisfiable unsatisfiable unknown))

type role AssociatedLengthSMTLibSolverObservation
  nominal nominal nominal nominal nominal

instance NFData
    (AssociatedLengthSMTLibSolverObservation
      identity local satisfiable unsatisfiable unknown) where
  rnf (AssociatedLengthSMTLibSolverObservation query associated) =
    rnf query `seq` rnf associated

-- | Bind a bounded raw report atomically to one exact query and its retained
-- behavioral problem.  Association does not validate the report, strengthen
-- its status, or grant pruning authority.
associateLengthSMTLibSolverObservation
  :: LengthSMTLibQuery identity local
  -> LengthSMTLibRawSolverObservation satisfiable unsatisfiable unknown
  -> AssociatedLengthSMTLibSolverObservation
      identity local satisfiable unsatisfiable unknown
associateLengthSMTLibSolverObservation query observation =
  AssociatedLengthSMTLibSolverObservation
    (lengthSMTLibQueryFingerprint query)
    (associateSolverObservation
      (lengthSMTLibQueryBehavioralProblem query)
      observation)

-- | Exact translator/query identity retained at association time.
associatedLengthSMTLibQueryFingerprint
  :: AssociatedLengthSMTLibSolverObservation
      identity local satisfiable unsatisfiable unknown
  -> Fingerprint LengthSMTLibQueryFingerprintSubject
associatedLengthSMTLibQueryFingerprint
    (AssociatedLengthSMTLibSolverObservation query _) = query

-- | Raw reported status, inspectable without exposing or forcing its artifact.
associatedLengthSMTLibSolverStatus
  :: AssociatedLengthSMTLibSolverObservation
      identity local satisfiable unsatisfiable unknown
  -> SolverStatus
associatedLengthSMTLibSolverStatus
    (AssociatedLengthSMTLibSolverObservation _ associated) =
  associatedSolverObservationStatus associated

-- | Conservative generic strength of the raw report.
associatedLengthSMTLibResultStrength
  :: AssociatedLengthSMTLibSolverObservation
      identity local satisfiable unsatisfiable unknown
  -> RawResultStrength
associatedLengthSMTLibResultStrength
    (AssociatedLengthSMTLibSolverObservation _ associated) =
  associatedObservationResultStrength associated

-- | The only permission granted to any associated raw result, including
-- @unsat@: it may inform heuristic ranking and nothing stronger.
associatedLengthSMTLibUse
  :: AssociatedLengthSMTLibSolverObservation
      identity local satisfiable unsatisfiable unknown
  -> RawObservationUse
associatedLengthSMTLibUse
    (AssociatedLengthSMTLibSolverObservation _ associated) =
  associatedObservationUse associated

-- | Exact replay rejected either the retained solver-neutral problem tuple or
-- the concrete translator/query identity.  Generic problem comparison runs
-- first and preserves its domain-to-complete-problem diagnostic precedence.
data LengthSMTLibObservationReplayError
  = LengthSMTLibObservationProblemMismatch !ReplayMismatch
  | LengthSMTLibObservationQueryFingerprintMismatch
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibObservationReplayError

-- | Reveal the still-raw bounded report only after both problem and query
-- replay succeed.
--
-- Successful replay does not validate a model and does not convert @unsat@
-- into evidence.  A satisfiable artifact must still be parsed and its decoded
-- inputs passed through 'validateLengthSMTLibCounterexample'.
replayAssociatedLengthSMTLibSolverObservation
  :: LengthSMTLibQuery identity local
  -> AssociatedLengthSMTLibSolverObservation
      identity local satisfiable unsatisfiable unknown
  -> Either
      LengthSMTLibObservationReplayError
      (LengthSMTLibRawSolverObservation
        satisfiable unsatisfiable unknown)
replayAssociatedLengthSMTLibSolverObservation query
    (AssociatedLengthSMTLibSolverObservation
      retainedQuery associated) = do
  observation <- first LengthSMTLibObservationProblemMismatch
    $ replayAssociatedObservation
        (lengthSMTLibQueryBehavioralProblem query)
        associated
  if lengthSMTLibQueryFingerprint query == retainedQuery
    then Right observation
    else Left LengthSMTLibObservationQueryFingerprintMismatch
