-- | Exact association for raw solver-neutral behavioral observations.
--
-- "Language.Haskell.Synthesis.Semantic.Observation" remains the raw report
-- vocabulary.  This module adds a checked domain-owned problem envelope and
-- bounds retained artifact bytes before associating a report with exact
-- inventory, encoding, candidate, and problem fingerprints.
--
-- Association is not certification.  Every raw solver or behavioral result,
-- including @unsat@, is exposed as 'HeuristicRankingOnly'.  The opaque
-- t'BehavioralEvidence' type is a public-replay/private-build seam for
-- domain-owned authoritative verifiers; this generic module deliberately
-- exports no producer, unchecked receipt projection, or raw-observation
-- conversion.
module Language.Haskell.Synthesis.Semantic.Problem
  ( ProblemFingerprintSubject
  , InventoryFingerprintSubject
  , EncodingFingerprintSubject
  , CandidateFingerprintSubject
  , BehavioralProblem
  , behavioralProblemDomain
  , behavioralProblemInventoryFingerprint
  , behavioralProblemEncodingFingerprint
  , behavioralProblemCandidateFingerprint
  , behavioralProblemFingerprint
  , RawArtifactLimits
  , mkRawArtifactLimits
  , defaultRawArtifactLimits
  , rawArtifactFormatByteLimit
  , rawArtifactPayloadByteLimit
  , RawArtifactPart (..)
  , RawArtifactLimitError (..)
  , BoundedRawArtifact
  , mkBoundedRawArtifact
  , boundedRawArtifactFormat
  , boundedRawArtifactBytes
  , RawResultStrength (..)
  , RawObservationUse (..)
  , AssociatedObservation
  , associateSolverObservation
  , associateBehavioralObservation
  , associatedObservationDomain
  , associatedObservationInventoryFingerprint
  , associatedObservationEncodingFingerprint
  , associatedObservationCandidateFingerprint
  , associatedObservationProblemFingerprint
  , associatedObservationResultStrength
  , associatedObservationUse
  , ReplayMismatch (..)
  , replayAssociatedObservation
  , BehavioralEvidence
  , behavioralEvidenceDomain
  , behavioralEvidenceInventoryFingerprint
  , behavioralEvidenceEncodingFingerprint
  , behavioralEvidenceCandidateFingerprint
  , behavioralEvidenceProblemFingerprint
  , replayBehavioralEvidence
  ) where

import Language.Haskell.Synthesis.Internal.Semantic.Problem
  ( AssociatedObservation
  , BehavioralEvidence
  , BehavioralProblem
  , BoundedRawArtifact
  , CandidateFingerprintSubject
  , EncodingFingerprintSubject
  , InventoryFingerprintSubject
  , ProblemFingerprintSubject
  , RawArtifactLimitError (..)
  , RawArtifactLimits
  , RawArtifactPart (..)
  , RawObservationUse (..)
  , RawResultStrength (..)
  , ReplayMismatch (..)
  , associateBehavioralObservation
  , associateSolverObservation
  , associatedObservationCandidateFingerprint
  , associatedObservationDomain
  , associatedObservationEncodingFingerprint
  , associatedObservationInventoryFingerprint
  , associatedObservationProblemFingerprint
  , associatedObservationResultStrength
  , associatedObservationUse
  , behavioralEvidenceCandidateFingerprint
  , behavioralEvidenceDomain
  , behavioralEvidenceEncodingFingerprint
  , behavioralEvidenceInventoryFingerprint
  , behavioralEvidenceProblemFingerprint
  , behavioralProblemCandidateFingerprint
  , behavioralProblemDomain
  , behavioralProblemEncodingFingerprint
  , behavioralProblemFingerprint
  , behavioralProblemInventoryFingerprint
  , boundedRawArtifactBytes
  , boundedRawArtifactFormat
  , defaultRawArtifactLimits
  , mkBoundedRawArtifact
  , mkRawArtifactLimits
  , rawArtifactFormatByteLimit
  , rawArtifactPayloadByteLimit
  , replayAssociatedObservation
  , replayBehavioralEvidence
  )
