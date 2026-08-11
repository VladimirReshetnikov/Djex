{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Private construction for solver-neutral behavioral problem envelopes.
--
-- Domain modules are responsible for producing the four exact fingerprints.
-- This module seals their association and is the only construction edge for
-- future authoritative evidence.  Raw observations never cross that edge.
module Language.Haskell.Synthesis.Internal.Semantic.Problem
  ( ProblemFingerprintSubject
  , InventoryFingerprintSubject
  , EncodingFingerprintSubject
  , CandidateFingerprintSubject
  , BehavioralProblem
  , mkBehavioralProblem
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
  , mkBehavioralEvidence
  , behavioralEvidenceDomain
  , behavioralEvidenceInventoryFingerprint
  , behavioralEvidenceEncodingFingerprint
  , behavioralEvidenceCandidateFingerprint
  , behavioralEvidenceProblemFingerprint
  , replayBehavioralEvidence
  ) where

import Control.DeepSeq (NFData (rnf))
import Data.Word (Word8)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Internal.Fingerprint (Fingerprint)
import Language.Haskell.Synthesis.Semantic.Observation
  ( BehavioralObservation (..)
  , SolverObservation (..)
  )

-- These subjects are intentionally distinct even when their exact bytes
-- happen to agree.  'Fingerprint' gives its subject a nominal role.
data ProblemFingerprintSubject domain
data InventoryFingerprintSubject domain
data EncodingFingerprintSubject domain
data CandidateFingerprintSubject domain

-- | An exact domain-owned behavioral problem identity.
--
-- The domain tag is compared at replay time in addition to the phantom domain
-- parameter, because serialized observations may be decoded by trusted code.
-- The remaining values identify the exact inventory, encoding, candidate, and
-- complete problem respectively.
data BehavioralProblem domain = BehavioralProblem
  ![Word8]
  !(Fingerprint (InventoryFingerprintSubject domain))
  !(Fingerprint (EncodingFingerprintSubject domain))
  !(Fingerprint (CandidateFingerprintSubject domain))
  !(Fingerprint (ProblemFingerprintSubject domain))

type role BehavioralProblem nominal

instance NFData (BehavioralProblem domain) where
  rnf (BehavioralProblem domain inventory encoding candidate problem) =
    rnf domain `seq`
    rnf inventory `seq`
    rnf encoding `seq`
    rnf candidate `seq`
    rnf problem

-- | Package-private sealing edge used after a domain has constructed every
-- canonical identity.  Arguments follow replay precedence after the domain:
-- inventory, encoding, candidate, then complete problem.  Domain modules must
-- supply a finite canonical versioned literal; decoded or caller-owned tags
-- require a separate checked bound before they may reach this trusted edge.
mkBehavioralProblem
  :: [Word8]
  -> Fingerprint (InventoryFingerprintSubject domain)
  -> Fingerprint (EncodingFingerprintSubject domain)
  -> Fingerprint (CandidateFingerprintSubject domain)
  -> Fingerprint (ProblemFingerprintSubject domain)
  -> BehavioralProblem domain
mkBehavioralProblem = BehavioralProblem

behavioralProblemDomain :: BehavioralProblem domain -> [Word8]
behavioralProblemDomain (BehavioralProblem domain _ _ _ _) = domain

behavioralProblemInventoryFingerprint
  :: BehavioralProblem domain
  -> Fingerprint (InventoryFingerprintSubject domain)
behavioralProblemInventoryFingerprint
    (BehavioralProblem _ inventory _ _ _) = inventory

behavioralProblemEncodingFingerprint
  :: BehavioralProblem domain
  -> Fingerprint (EncodingFingerprintSubject domain)
behavioralProblemEncodingFingerprint
    (BehavioralProblem _ _ encoding _ _) = encoding

behavioralProblemCandidateFingerprint
  :: BehavioralProblem domain
  -> Fingerprint (CandidateFingerprintSubject domain)
behavioralProblemCandidateFingerprint
    (BehavioralProblem _ _ _ candidate _) = candidate

behavioralProblemFingerprint
  :: BehavioralProblem domain
  -> Fingerprint (ProblemFingerprintSubject domain)
behavioralProblemFingerprint (BehavioralProblem _ _ _ _ problem) = problem

-- | Independent bounds for the backend format tag and artifact bytes.
data RawArtifactLimits = RawArtifactLimits !Natural !Natural
  deriving (Eq, Ord, Show)

instance NFData RawArtifactLimits where
  rnf (RawArtifactLimits formatBytes payloadBytes) =
    rnf formatBytes `seq` rnf payloadBytes

-- | Construct explicit raw-artifact bounds.  Zero is meaningful and permits
-- only an empty value for the corresponding part.
mkRawArtifactLimits :: Natural -> Natural -> RawArtifactLimits
mkRawArtifactLimits = RawArtifactLimits

-- | Conservative defaults for one raw observation artifact.
--
-- Format tags are normally short backend/schema identifiers; payloads may
-- retain a model, core, reason, or bounded-check trace without becoming
-- evidence.
defaultRawArtifactLimits :: RawArtifactLimits
defaultRawArtifactLimits = RawArtifactLimits 64 65536

rawArtifactFormatByteLimit :: RawArtifactLimits -> Natural
rawArtifactFormatByteLimit (RawArtifactLimits formatBytes _) = formatBytes

rawArtifactPayloadByteLimit :: RawArtifactLimits -> Natural
rawArtifactPayloadByteLimit (RawArtifactLimits _ payloadBytes) = payloadBytes

-- | Which independently bounded part exceeded its limit.
data RawArtifactPart
  = RawArtifactFormat
  | RawArtifactPayload
  deriving (Bounded, Enum, Eq, Ord, Show)

instance NFData RawArtifactPart where
  rnf part = part `seq` ()

-- | Productive size rejection.  The observed value is capped at limit + 1.
data RawArtifactLimitError = RawArtifactLimitExceeded
  !RawArtifactPart
  !Natural
  !Natural
  deriving (Eq, Ord, Show)

instance NFData RawArtifactLimitError where
  rnf (RawArtifactLimitExceeded part maximumBytes observedAtLeast) =
    rnf part `seq` rnf maximumBytes `seq` rnf observedAtLeast

-- | Exact retained bytes for one raw, explicitly non-authoritative artifact.
data BoundedRawArtifact kind = BoundedRawArtifact [Word8] [Word8]
  deriving (Eq, Ord, Show)

type role BoundedRawArtifact nominal

instance NFData (BoundedRawArtifact kind) where
  rnf (BoundedRawArtifact format payload) = rnf format `seq` rnf payload

-- | Retain a raw artifact only after observing both lists within their bounds.
-- Format is checked before payload, which is also the fixed failure order.
mkBoundedRawArtifact
  :: RawArtifactLimits
  -> [Word8]
  -> [Word8]
  -> Either RawArtifactLimitError (BoundedRawArtifact kind)
mkBoundedRawArtifact (RawArtifactLimits formatLimit payloadLimit)
    rawFormat rawPayload = do
  format <- observeBytes RawArtifactFormat formatLimit rawFormat
  payload <- observeBytes RawArtifactPayload payloadLimit rawPayload
  pure $ BoundedRawArtifact format payload

boundedRawArtifactFormat :: BoundedRawArtifact kind -> [Word8]
boundedRawArtifactFormat (BoundedRawArtifact format _) = format

boundedRawArtifactBytes :: BoundedRawArtifact kind -> [Word8]
boundedRawArtifactBytes (BoundedRawArtifact _ payload) = payload

observeBytes
  :: RawArtifactPart
  -> Natural
  -> [Word8]
  -> Either RawArtifactLimitError [Word8]
observeBytes _ _ [] = Right []
observeBytes part 0 (_ : _) = Left
  $ RawArtifactLimitExceeded part 0 1
observeBytes part maximumBytes bytes = go maximumBytes bytes
 where
  go _ [] = Right []
  go 0 (_ : _) = Left $ RawArtifactLimitExceeded
    part maximumBytes (maximumBytes + 1)
  go remaining (byte : remainingBytes) =
    (byte :) <$> go (remaining - 1) remainingBytes

-- | Conservative semantic strength of a raw report.
--
-- In particular, unsatisfiability is relative to the associated encoding; it
-- is not a proof of candidate behavior and does not authorize exact pruning.
data RawResultStrength
  = RawSolverModelHint
  | RawSolverUnsatRelativeToEncoding
  | RawSolverUnknown
  | RawBehaviorEstablishedClaim
  | RawBehaviorCounterexampleClaim
  | RawBehaviorBoundedValidation
  | RawBehaviorUnknown
  deriving (Bounded, Enum, Eq, Ord, Show)

instance NFData RawResultStrength where
  rnf strength = strength `seq` ()

-- | The only search use granted to a raw observation.
data RawObservationUse = HeuristicRankingOnly
  deriving (Bounded, Enum, Eq, Ord, Show)

instance NFData RawObservationUse where
  rnf use = use `seq` ()

data ProblemAssociation domain = ProblemAssociation
  ![Word8]
  !(Fingerprint (InventoryFingerprintSubject domain))
  !(Fingerprint (EncodingFingerprintSubject domain))
  !(Fingerprint (CandidateFingerprintSubject domain))
  !(Fingerprint (ProblemFingerprintSubject domain))

type role ProblemAssociation nominal

instance NFData (ProblemAssociation domain) where
  rnf (ProblemAssociation domain inventory encoding candidate problem) =
    rnf domain `seq`
    rnf inventory `seq`
    rnf encoding `seq`
    rnf candidate `seq`
    rnf problem

-- | A raw observation immutably associated with one exact problem tuple.
--
-- The observation parameter is nominal so callers cannot use 'coerce' to
-- relabel status-specific artifact domains after association.
data AssociatedObservation domain observation = AssociatedObservation
  !(ProblemAssociation domain)
  !RawResultStrength
  observation

type role AssociatedObservation nominal nominal

instance NFData observation
    => NFData (AssociatedObservation domain observation) where
  rnf (AssociatedObservation association strength observation) =
    rnf association `seq` rnf strength `seq` rnf observation

associateSolverObservation
  :: BehavioralProblem domain
  -> SolverObservation
      (BoundedRawArtifact satisfiable)
      (BoundedRawArtifact unsatisfiable)
      (BoundedRawArtifact unknown)
  -> AssociatedObservation domain
      (SolverObservation
        (BoundedRawArtifact satisfiable)
        (BoundedRawArtifact unsatisfiable)
        (BoundedRawArtifact unknown))
associateSolverObservation problem observation = AssociatedObservation
  (problemAssociation problem)
  (case observation of
    SatisfiableObservation{} -> RawSolverModelHint
    UnsatisfiableObservation{} -> RawSolverUnsatRelativeToEncoding
    UnknownObservation{} -> RawSolverUnknown)
  observation

associateBehavioralObservation
  :: BehavioralProblem domain
  -> BehavioralObservation
      (BoundedRawArtifact established)
      (BoundedRawArtifact counterexample)
      (BoundedRawArtifact bounded)
      (BoundedRawArtifact unknown)
  -> AssociatedObservation domain
      (BehavioralObservation
        (BoundedRawArtifact established)
        (BoundedRawArtifact counterexample)
        (BoundedRawArtifact bounded)
        (BoundedRawArtifact unknown))
associateBehavioralObservation problem observation = AssociatedObservation
  (problemAssociation problem)
  (case observation of
    BehaviorEstablishedObservation{} -> RawBehaviorEstablishedClaim
    BehaviorViolationObservation{} -> RawBehaviorCounterexampleClaim
    BehaviorBoundedObservation{} -> RawBehaviorBoundedValidation
    BehaviorUnknownObservation{} -> RawBehaviorUnknown)
  observation

associatedObservationDomain
  :: AssociatedObservation domain observation
  -> [Word8]
associatedObservationDomain (AssociatedObservation association _ _) =
  associationDomain association

associatedObservationInventoryFingerprint
  :: AssociatedObservation domain observation
  -> Fingerprint (InventoryFingerprintSubject domain)
associatedObservationInventoryFingerprint
    (AssociatedObservation association _ _) = associationInventory association

associatedObservationEncodingFingerprint
  :: AssociatedObservation domain observation
  -> Fingerprint (EncodingFingerprintSubject domain)
associatedObservationEncodingFingerprint
    (AssociatedObservation association _ _) = associationEncoding association

associatedObservationCandidateFingerprint
  :: AssociatedObservation domain observation
  -> Fingerprint (CandidateFingerprintSubject domain)
associatedObservationCandidateFingerprint
    (AssociatedObservation association _ _) = associationCandidate association

associatedObservationProblemFingerprint
  :: AssociatedObservation domain observation
  -> Fingerprint (ProblemFingerprintSubject domain)
associatedObservationProblemFingerprint
    (AssociatedObservation association _ _) = associationProblem association

associatedObservationResultStrength
  :: AssociatedObservation domain observation
  -> RawResultStrength
associatedObservationResultStrength (AssociatedObservation _ strength _) =
  strength

associatedObservationUse
  :: AssociatedObservation domain observation
  -> RawObservationUse
associatedObservationUse _ = HeuristicRankingOnly

-- | Why replay against a current problem failed.
--
-- 'replayAssociatedObservation' checks constructors in this declaration order:
-- domain, inventory, encoding, candidate, then complete problem.  A mismatch
-- always fails closed; later matches cannot hide an earlier stale component.
data ReplayMismatch
  = ReplayDomainMismatch
  | ReplayInventoryFingerprintMismatch
  | ReplayEncodingFingerprintMismatch
  | ReplayCandidateFingerprintMismatch
  | ReplayProblemFingerprintMismatch
  deriving (Bounded, Enum, Eq, Ord, Show)

instance NFData ReplayMismatch where
  rnf mismatch = mismatch `seq` ()

replayAssociatedObservation
  :: BehavioralProblem domain
  -> AssociatedObservation domain observation
  -> Either ReplayMismatch observation
replayAssociatedObservation expected
    (AssociatedObservation actual _ observation) =
  observation <$ compareAssociation (problemAssociation expected) actual

-- | Future checker receipt associated with exactly the same problem tuple.
--
-- Construction is package-private.  In particular there is intentionally no
-- conversion from 'AssociatedObservation': a solver model, core, or @unsat@
-- response cannot manufacture this type.
data BehavioralEvidence domain receipt = BehavioralEvidence
  !(ProblemAssociation domain)
  receipt

type role BehavioralEvidence nominal nominal

instance NFData receipt => NFData (BehavioralEvidence domain receipt) where
  rnf (BehavioralEvidence association receipt) =
    rnf association `seq` rnf receipt

-- | Private seam for a future authoritative verifier such as a Lean adapter.
-- This generic checkpoint provides no producer which calls it.
mkBehavioralEvidence
  :: BehavioralProblem domain
  -> receipt
  -> BehavioralEvidence domain receipt
mkBehavioralEvidence problem = BehavioralEvidence $ problemAssociation problem

behavioralEvidenceDomain :: BehavioralEvidence domain receipt -> [Word8]
behavioralEvidenceDomain (BehavioralEvidence association _) =
  associationDomain association

behavioralEvidenceInventoryFingerprint
  :: BehavioralEvidence domain receipt
  -> Fingerprint (InventoryFingerprintSubject domain)
behavioralEvidenceInventoryFingerprint (BehavioralEvidence association _) =
  associationInventory association

behavioralEvidenceEncodingFingerprint
  :: BehavioralEvidence domain receipt
  -> Fingerprint (EncodingFingerprintSubject domain)
behavioralEvidenceEncodingFingerprint (BehavioralEvidence association _) =
  associationEncoding association

behavioralEvidenceCandidateFingerprint
  :: BehavioralEvidence domain receipt
  -> Fingerprint (CandidateFingerprintSubject domain)
behavioralEvidenceCandidateFingerprint (BehavioralEvidence association _) =
  associationCandidate association

behavioralEvidenceProblemFingerprint
  :: BehavioralEvidence domain receipt
  -> Fingerprint (ProblemFingerprintSubject domain)
behavioralEvidenceProblemFingerprint (BehavioralEvidence association _) =
  associationProblem association

replayBehavioralEvidence
  :: BehavioralProblem domain
  -> BehavioralEvidence domain receipt
  -> Either ReplayMismatch receipt
replayBehavioralEvidence expected (BehavioralEvidence actual receipt) =
  receipt <$ compareAssociation (problemAssociation expected) actual

problemAssociation :: BehavioralProblem domain -> ProblemAssociation domain
problemAssociation (BehavioralProblem domain inventory encoding candidate problem) =
  ProblemAssociation domain inventory encoding candidate problem

associationDomain :: ProblemAssociation domain -> [Word8]
associationDomain (ProblemAssociation domain _ _ _ _) = domain

associationInventory
  :: ProblemAssociation domain
  -> Fingerprint (InventoryFingerprintSubject domain)
associationInventory (ProblemAssociation _ inventory _ _ _) = inventory

associationEncoding
  :: ProblemAssociation domain
  -> Fingerprint (EncodingFingerprintSubject domain)
associationEncoding (ProblemAssociation _ _ encoding _ _) = encoding

associationCandidate
  :: ProblemAssociation domain
  -> Fingerprint (CandidateFingerprintSubject domain)
associationCandidate (ProblemAssociation _ _ _ candidate _) = candidate

associationProblem
  :: ProblemAssociation domain
  -> Fingerprint (ProblemFingerprintSubject domain)
associationProblem (ProblemAssociation _ _ _ _ problem) = problem

compareAssociation
  :: ProblemAssociation domain
  -> ProblemAssociation domain
  -> Either ReplayMismatch ()
compareAssociation expected actual
  | associationDomain expected /= associationDomain actual =
      Left ReplayDomainMismatch
  | associationInventory expected /= associationInventory actual =
      Left ReplayInventoryFingerprintMismatch
  | associationEncoding expected /= associationEncoding actual =
      Left ReplayEncodingFingerprintMismatch
  | associationCandidate expected /= associationCandidate actual =
      Left ReplayCandidateFingerprintMismatch
  | associationProblem expected /= associationProblem actual =
      Left ReplayProblemFingerprintMismatch
  | otherwise = Right ()
