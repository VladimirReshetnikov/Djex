-- | Query-specific association and replay for raw Length SMT-LIB reports.
--
-- A generic 'AssociatedObservation' identifies the solver-neutral behavioral
-- problem.  This module additionally binds the concrete canonical query, so a
-- report cannot be replayed after the translator schema, typed plan, symbol
-- map, or emitted commands change.  The opaque association exposes only safe
-- status, identity, strength, and use projections before replay; neither its
-- payload nor its nested generic association is projected.
--
-- Association remains non-authoritative.  Every result is restricted to
-- 'HeuristicRankingOnly'; @unsat@ is not proof, and a satisfiable model must
-- still pass bounded parsing, exact symbol decoding, and independent Length
-- replay before it can yield model-relative evidence.
--
-- The query fingerprint is not a solver-run or cache identity.  The live
-- executor separately binds the observed Z3 file and capabilities,
-- invocation and runtime options, protocol/session state, parser schema,
-- requested artifact policy, deadlines, cancellation, and resource limits.
module Language.Haskell.Synthesis.Semantic.Length.SMTLib.Observation
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

import Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Observation
