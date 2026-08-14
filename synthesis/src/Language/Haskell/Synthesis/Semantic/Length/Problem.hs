-- | Atomic sessions and behavioral problems for finite list-spine length.
--
-- The first checked layer binds one exact annotation-erased neutral inventory,
-- finite-spine model, normalized provider-law table, and closed interpretation
-- policy.  Inventory identity remains distinct from the solver-neutral
-- encoding policy.  The unified contract and problem entrances derive their
-- authority from that opaque association; compatibility wrappers retain their
-- historical projected-association behavior and signatures.  Encoding-policy
-- versions advance when their common candidate trust boundary changes, so
-- callers must treat the collision-free bytes as versioned identities rather
-- than as a permanently frozen wire format.
--
-- When a retained provider law has a nonempty context, the session privately
-- attempts to seal the restricted ground class resolver from that same
-- inventory.  An unavailable resolver is retained fail-closed and reported as
-- 'LengthAssociatedClassResolverUnavailable' if a candidate needs it.  One
-- associated certificate row may use the law only after every source-ordered
-- obligation is independently discharged, its complete certified function
-- prefix passes the occurrence-isolation audit, and interpretation reaches the
-- row's final visible-application node.  The base node remains a sentinel.
-- The new associated discharge and protected-chain failures expose only
-- canonical row/step/obligation positions and sanitized reasons; resolver
-- receipts, constraint/type payloads, and protected graph coordinates remain
-- private.  No query givens or Z3 evidence participate.
module Language.Haskell.Synthesis.Semantic.Length.Problem
  ( LengthSemanticFingerprintPart (..)
  , LengthEncodingPolicyFingerprintSubject
  , LengthSessionError (..)
  , LengthInterpretationPolicySource (..)
  , CheckedLengthInterpretationPolicy
  , CheckedLengthSession
  , sealLengthSessionWithInterpretationPolicy
  , sealLengthSession
  , sealRoleAwareLengthSession
  , sealExactSpineCaseLengthSession
  , checkedLengthSessionInterpretationPolicy
  , sealLengthContractInSession
  , checkedLengthSessionContext
  , checkedLengthSessionProviderInventory
  , lengthSessionInventoryFingerprint
  , lengthSessionEncodingPolicyFingerprint
  , LengthProblemLimits
  , LengthProblemLimitError (..)
  , mkLengthProblemLimits
  , defaultLengthProblemLimits
  , lengthProblemTermGraphLimits
  , lengthProblemGraphFingerprintByteLimit
  , lengthProblemEvaluationStepLimit
  , LengthProblemFingerprintPart (..)
  , LengthRootOpeningError (..)
  , LengthUnobservedTargetDemandSite (..)
  , LengthStepPayloadDemandSite (..)
  , LengthAssociatedConstraintDischargeReason (..)
  , LengthAssociatedProviderChainSite (..)
  , LengthAssociatedProviderChainReason (..)
  , LengthProblemError (..)
  , CheckedLengthCandidate
  , CheckedLengthProblem
  , sealLengthTypedCandidateProblem
  , sealRoleAwareLengthTypedCandidateProblem
  , sealExactSpineCaseLengthTypedCandidateProblem
  , sealLengthTypedCandidateProblemInSession
  , checkedLengthCandidateResult
  , checkedLengthCandidateUsedProviders
  , checkedLengthCandidateFingerprint
  , checkedLengthProblemCandidate
  , checkedLengthProblemInputCount
  , checkedLengthProblemPrecondition
  , checkedLengthProblemPostcondition
  , checkedLengthProblemCounterexampleCondition
  , checkedLengthProblemEncodingFingerprint
  , checkedLengthProblemBehavioralProblem
  ) where

import Language.Haskell.Synthesis.Internal.Semantic.Length.Problem
import Language.Haskell.Synthesis.Internal.Semantic.Length.Problem.Candidate
