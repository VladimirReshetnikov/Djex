-- | Atomic sessions and behavioral problems for finite list-spine length.
--
-- The first checked layer binds one exact annotation-erased neutral inventory,
-- finite-spine model, normalized provider-law table, and closed interpretation
-- policy.  Inventory identity remains distinct from the solver-neutral
-- encoding policy.  The unified contract and problem entrances derive their
-- authority from that opaque association; compatibility wrappers retain their
-- historical projected-association behavior and bytes.
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
