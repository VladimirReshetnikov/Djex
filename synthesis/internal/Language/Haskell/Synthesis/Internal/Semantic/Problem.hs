{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Private construction for solver-neutral behavioral problem envelopes.
--
-- Domain modules are responsible for producing the four exact fingerprints.
-- This module seals their association and is the only construction edge for
-- future authoritative evidence.  Associated values retain that same opaque
-- problem directly rather than copying its identity into a parallel private
-- representation.  Raw observations never cross that edge.
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
  , associatedSolverObservationStatus
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
  , mapBehavioralEvidenceReceipt
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
  , SolverStatus
  , solverObservationStatus
  )

-- | Subject of the complete behavioral problem fingerprint for one domain.
--
-- These subjects are intentionally distinct even when their exact bytes
-- happen to agree.  'Fingerprint' gives its subject a nominal role.
data ProblemFingerprintSubject domain
-- | Subject of the exact source-inventory fingerprint for one domain.
data InventoryFingerprintSubject domain
-- | Subject of the exact encoding fingerprint for one domain.
data EncodingFingerprintSubject domain
-- | Subject of the exact candidate fingerprint for one domain.
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

-- | The exact runtime domain tag; it is the first component compared at
-- replay.
behavioralProblemDomain :: BehavioralProblem domain -> [Word8]
behavioralProblemDomain (BehavioralProblem domain _ _ _ _) = domain

-- | Identity of the exact source inventory the problem was built against.
behavioralProblemInventoryFingerprint
  :: BehavioralProblem domain
  -> Fingerprint (InventoryFingerprintSubject domain)
behavioralProblemInventoryFingerprint
    (BehavioralProblem _ inventory _ _ _) = inventory

-- | Identity of the exact encoding the problem was built with.
behavioralProblemEncodingFingerprint
  :: BehavioralProblem domain
  -> Fingerprint (EncodingFingerprintSubject domain)
behavioralProblemEncodingFingerprint
    (BehavioralProblem _ _ encoding _ _) = encoding

-- | Identity of the exact candidate the problem is about.
behavioralProblemCandidateFingerprint
  :: BehavioralProblem domain
  -> Fingerprint (CandidateFingerprintSubject domain)
behavioralProblemCandidateFingerprint
    (BehavioralProblem _ _ _ candidate _) = candidate

-- | Identity of the complete problem; it is the last component compared at
-- replay.
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

-- | Maximum retained length of the backend format tag, in bytes.
rawArtifactFormatByteLimit :: RawArtifactLimits -> Natural
rawArtifactFormatByteLimit (RawArtifactLimits formatBytes _) = formatBytes

-- | Maximum retained length of the artifact payload, in bytes.
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

-- | The retained backend format tag, already observed within its bound.
boundedRawArtifactFormat :: BoundedRawArtifact kind -> [Word8]
boundedRawArtifactFormat (BoundedRawArtifact format _) = format

-- | The retained raw payload bytes, already observed within their bound.
-- They carry no authority; see 'RawObservationUse'.
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

-- | A raw observation immutably associated with one exact problem tuple.
--
-- The observation parameter is nominal so callers cannot use 'coerce' to
-- relabel status-specific artifact domains after association.  The two
-- private constructors retain the observation family itself, so conservative
-- strength is derived from that single raw owner instead of copied beside it.
data AssociatedObservation domain observation where
  AssociatedSolverObservation
    :: !(BehavioralProblem domain)
    -> !(SolverObservation
          (BoundedRawArtifact satisfiable)
          (BoundedRawArtifact unsatisfiable)
          (BoundedRawArtifact unknown))
    -> AssociatedObservation domain
        (SolverObservation
          (BoundedRawArtifact satisfiable)
          (BoundedRawArtifact unsatisfiable)
          (BoundedRawArtifact unknown))
  AssociatedBehavioralObservation
    :: !(BehavioralProblem domain)
    -> !(BehavioralObservation
          (BoundedRawArtifact established)
          (BoundedRawArtifact counterexample)
          (BoundedRawArtifact bounded)
          (BoundedRawArtifact unknown))
    -> AssociatedObservation domain
        (BehavioralObservation
          (BoundedRawArtifact established)
          (BoundedRawArtifact counterexample)
          (BoundedRawArtifact bounded)
          (BoundedRawArtifact unknown))

type role AssociatedObservation nominal nominal

instance NFData (AssociatedObservation domain observation) where
  rnf associated = case associated of
    AssociatedSolverObservation problem observation ->
      rnf problem `seq` rnf observation
    AssociatedBehavioralObservation problem observation ->
      rnf problem `seq` rnf observation

-- | Bind a raw solver report to the exact problem it was produced for.  The
-- association is immutable and grants only 'HeuristicRankingOnly' use.
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
associateSolverObservation = AssociatedSolverObservation

-- | Bind a raw behavioral report to the exact problem it was produced for.
-- Like 'associateSolverObservation', this confers no evidence: an
-- established or counterexample claim stays a heuristic hint until a
-- domain verifier independently produces t'BehavioralEvidence'.
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
associateBehavioralObservation = AssociatedBehavioralObservation

-- | Inspect the status constructor retained inside an associated raw solver
-- report without exposing the generic association or forcing its artifact.
-- Domain-specific association layers use this package-private projection to
-- avoid storing the same status a second time.
associatedSolverObservationStatus
  :: AssociatedObservation domain
      (SolverObservation satisfiable unsatisfiable unknown)
  -> SolverStatus
associatedSolverObservationStatus
    (AssociatedSolverObservation _ observation) =
  solverObservationStatus observation

associatedObservationProblem
  :: AssociatedObservation domain observation
  -> BehavioralProblem domain
associatedObservationProblem associated = case associated of
  AssociatedSolverObservation problem _ -> problem
  AssociatedBehavioralObservation problem _ -> problem

-- | Domain tag of the problem the observation was associated with.
associatedObservationDomain
  :: AssociatedObservation domain observation
  -> [Word8]
associatedObservationDomain =
  behavioralProblemDomain . associatedObservationProblem

-- | Inventory identity of the problem the observation was associated with.
associatedObservationInventoryFingerprint
  :: AssociatedObservation domain observation
  -> Fingerprint (InventoryFingerprintSubject domain)
associatedObservationInventoryFingerprint =
  behavioralProblemInventoryFingerprint . associatedObservationProblem

-- | Encoding identity of the problem the observation was associated with.
associatedObservationEncodingFingerprint
  :: AssociatedObservation domain observation
  -> Fingerprint (EncodingFingerprintSubject domain)
associatedObservationEncodingFingerprint =
  behavioralProblemEncodingFingerprint . associatedObservationProblem

-- | Candidate identity of the problem the observation was associated with.
associatedObservationCandidateFingerprint
  :: AssociatedObservation domain observation
  -> Fingerprint (CandidateFingerprintSubject domain)
associatedObservationCandidateFingerprint =
  behavioralProblemCandidateFingerprint . associatedObservationProblem

-- | Complete problem identity the observation was associated with.
associatedObservationProblemFingerprint
  :: AssociatedObservation domain observation
  -> Fingerprint (ProblemFingerprintSubject domain)
associatedObservationProblemFingerprint =
  behavioralProblemFingerprint . associatedObservationProblem

-- | Conservative strength derived from the singly retained raw observation's
-- closed family and status constructor, without inspecting its artifact.
associatedObservationResultStrength
  :: AssociatedObservation domain observation
  -> RawResultStrength
associatedObservationResultStrength associated = case associated of
  AssociatedSolverObservation _ observation -> case observation of
    SatisfiableObservation{} -> RawSolverModelHint
    UnsatisfiableObservation{} -> RawSolverUnsatRelativeToEncoding
    UnknownObservation{} -> RawSolverUnknown
  AssociatedBehavioralObservation _ observation -> case observation of
    BehaviorEstablishedObservation{} -> RawBehaviorEstablishedClaim
    BehaviorViolationObservation{} -> RawBehaviorCounterexampleClaim
    BehaviorBoundedObservation{} -> RawBehaviorBoundedValidation
    BehaviorUnknownObservation{} -> RawBehaviorUnknown

-- | The search use granted to an associated raw observation.  This is always
-- 'HeuristicRankingOnly', regardless of the observation's family or status.
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

-- | Release the raw observation only if its associated problem matches the
-- expected one component by component, in the order documented on
-- 'ReplayMismatch'.  The first differing component is reported.
replayAssociatedObservation
  :: BehavioralProblem domain
  -> AssociatedObservation domain observation
  -> Either ReplayMismatch observation
replayAssociatedObservation expected associated = case associated of
  AssociatedSolverObservation actual observation ->
    observation <$ compareAssociation expected actual
  AssociatedBehavioralObservation actual observation ->
    observation <$ compareAssociation expected actual

-- | Future checker receipt associated with exactly the same problem tuple.
--
-- Construction is package-private.  In particular there is intentionally no
-- conversion from 'AssociatedObservation': a solver model, core, or @unsat@
-- response cannot manufacture this type.
data BehavioralEvidence domain receipt = BehavioralEvidence
  !(BehavioralProblem domain)
  receipt

type role BehavioralEvidence nominal nominal

instance NFData receipt => NFData (BehavioralEvidence domain receipt) where
  rnf (BehavioralEvidence problem receipt) =
    rnf problem `seq` rnf receipt

-- | Private seam for domain-owned authoritative verifiers.  Public generic
-- code cannot call it; each producer must independently replay a concrete
-- receipt before binding it to the exact problem tuple.
mkBehavioralEvidence
  :: BehavioralProblem domain
  -> receipt
  -> BehavioralEvidence domain receipt
mkBehavioralEvidence = BehavioralEvidence

-- | Change only the domain-owned receipt while retaining the exact opaque
-- problem association.  This package-private operation is for a verifier
-- which has independently strengthened an already authoritative receipt; it
-- cannot introduce evidence from a raw observation or substitute a different
-- problem tuple.
mapBehavioralEvidenceReceipt
  :: (receipt -> strengthened)
  -> BehavioralEvidence domain receipt
  -> BehavioralEvidence domain strengthened
mapBehavioralEvidenceReceipt strengthen (BehavioralEvidence problem receipt) =
  BehavioralEvidence problem $ strengthen receipt

-- | Domain tag of the problem the evidence is bound to.
behavioralEvidenceDomain :: BehavioralEvidence domain receipt -> [Word8]
behavioralEvidenceDomain (BehavioralEvidence problem _) =
  behavioralProblemDomain problem

-- | Inventory identity of the problem the evidence is bound to.
behavioralEvidenceInventoryFingerprint
  :: BehavioralEvidence domain receipt
  -> Fingerprint (InventoryFingerprintSubject domain)
behavioralEvidenceInventoryFingerprint (BehavioralEvidence problem _) =
  behavioralProblemInventoryFingerprint problem

-- | Encoding identity of the problem the evidence is bound to.
behavioralEvidenceEncodingFingerprint
  :: BehavioralEvidence domain receipt
  -> Fingerprint (EncodingFingerprintSubject domain)
behavioralEvidenceEncodingFingerprint (BehavioralEvidence problem _) =
  behavioralProblemEncodingFingerprint problem

-- | Candidate identity of the problem the evidence is bound to.
behavioralEvidenceCandidateFingerprint
  :: BehavioralEvidence domain receipt
  -> Fingerprint (CandidateFingerprintSubject domain)
behavioralEvidenceCandidateFingerprint (BehavioralEvidence problem _) =
  behavioralProblemCandidateFingerprint problem

-- | Complete problem identity the evidence is bound to.
behavioralEvidenceProblemFingerprint
  :: BehavioralEvidence domain receipt
  -> Fingerprint (ProblemFingerprintSubject domain)
behavioralEvidenceProblemFingerprint (BehavioralEvidence problem _) =
  behavioralProblemFingerprint problem

-- | Release the receipt only if the evidence's problem matches the expected
-- one under the same component-by-component comparison as
-- 'replayAssociatedObservation'.
replayBehavioralEvidence
  :: BehavioralProblem domain
  -> BehavioralEvidence domain receipt
  -> Either ReplayMismatch receipt
replayBehavioralEvidence expected (BehavioralEvidence actual receipt) =
  receipt <$ compareAssociation expected actual

compareAssociation
  :: BehavioralProblem domain
  -> BehavioralProblem domain
  -> Either ReplayMismatch ()
compareAssociation expected actual
  | behavioralProblemDomain expected /= behavioralProblemDomain actual =
      Left ReplayDomainMismatch
  | behavioralProblemInventoryFingerprint expected
      /= behavioralProblemInventoryFingerprint actual =
      Left ReplayInventoryFingerprintMismatch
  | behavioralProblemEncodingFingerprint expected
      /= behavioralProblemEncodingFingerprint actual =
      Left ReplayEncodingFingerprintMismatch
  | behavioralProblemCandidateFingerprint expected
      /= behavioralProblemCandidateFingerprint actual =
      Left ReplayCandidateFingerprintMismatch
  | behavioralProblemFingerprint expected
      /= behavioralProblemFingerprint actual =
      Left ReplayProblemFingerprintMismatch
  | otherwise = Right ()
