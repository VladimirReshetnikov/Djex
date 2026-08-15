{-# LANGUAGE DeriveGeneric #-}

-- | Solver-neutral observations for future behavioral checking.
--
-- These values record what a solver or behavioral checker reported.  They do
-- not constitute certified evidence and intentionally carry no candidate.
-- A domain-specific verifier must validate artifacts and bind exact problem,
-- inventory, encoding, and candidate fingerprints before exposing evidence.
module Language.Haskell.Synthesis.Semantic.Observation
  ( SolverStatus (..)
  , SolverObservation (..)
  , solverObservationStatus
  , BehavioralStatus (..)
  , BehavioralObservation (..)
  , behavioralObservationStatus
  ) where

import Control.DeepSeq (NFData (rnf))
import GHC.Generics (Generic)

-- | The irreducibly three-valued result of a solver check.
data SolverStatus
  = SolverSatisfiable
  | SolverUnsatisfiable
  | SolverUnknown
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData SolverStatus

-- | One raw solver status together with its status-specific artifact.
--
-- Payload types remain backend-owned.  In particular, this module provides no
-- conversion from satisfiable or unsatisfiable observations to behavioral
-- evidence: models still require validation and unsatisfiability remains
-- meaningful only for an exact checked problem.
data SolverObservation satisfiable unsatisfiable unknown
  = SatisfiableObservation satisfiable
  | UnsatisfiableObservation unsatisfiable
  | UnknownObservation unknown
  deriving (Eq, Ord, Show, Generic)

instance
    ( NFData satisfiable
    , NFData unsatisfiable
    , NFData unknown
    ) => NFData (SolverObservation satisfiable unsatisfiable unknown) where
  rnf observation = case observation of
    SatisfiableObservation artifact -> rnf artifact
    UnsatisfiableObservation artifact -> rnf artifact
    UnknownObservation reason -> rnf reason

-- | Classify an observation by its constructor without inspecting the
-- artifact payload.
solverObservationStatus
  :: SolverObservation satisfiable unsatisfiable unknown
  -> SolverStatus
solverObservationStatus observation = case observation of
  SatisfiableObservation{} -> SolverSatisfiable
  UnsatisfiableObservation{} -> SolverUnsatisfiable
  UnknownObservation{} -> SolverUnknown

-- | The four statuses a behavioral checker may report.
--
-- Establishment is kept distinct from bounded validation.  This enumeration
-- classifies a report; it does not certify that the producer used a sound
-- encoding or that an artifact belongs to a particular candidate.
data BehavioralStatus
  = BehaviorEstablished
  | BehaviorViolated
  | BehaviorValidatedWithin
  | BehaviorUnknown
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData BehavioralStatus

-- | A raw behavioral report with a status-specific artifact.
--
-- Public construction is honest because this type promises observation only.
-- It deliberately offers no candidate-pairing or candidate-only 'Functor': a
-- later certified layer must make the candidate association non-forgeable.
data BehavioralObservation established counterexample bounded unknown
  = BehaviorEstablishedObservation established
  | BehaviorViolationObservation counterexample
  | BehaviorBoundedObservation bounded
  | BehaviorUnknownObservation unknown
  deriving (Eq, Ord, Show, Generic)

instance
    ( NFData established
    , NFData counterexample
    , NFData bounded
    , NFData unknown
    ) => NFData
      (BehavioralObservation established counterexample bounded unknown) where
  rnf observation = case observation of
    BehaviorEstablishedObservation artifact -> rnf artifact
    BehaviorViolationObservation counterexample -> rnf counterexample
    BehaviorBoundedObservation bounds -> rnf bounds
    BehaviorUnknownObservation reason -> rnf reason

-- | Classify a report by its constructor without inspecting the artifact
-- payload.
behavioralObservationStatus
  :: BehavioralObservation established counterexample bounded unknown
  -> BehavioralStatus
behavioralObservationStatus observation = case observation of
  BehaviorEstablishedObservation{} -> BehaviorEstablished
  BehaviorViolationObservation{} -> BehaviorViolated
  BehaviorBoundedObservation{} -> BehaviorValidatedWithin
  BehaviorUnknownObservation{} -> BehaviorUnknown
