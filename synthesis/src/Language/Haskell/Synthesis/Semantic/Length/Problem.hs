-- | Atomic sessions and behavioral problems for finite list-spine length.
--
-- The first checked layer binds one exact annotation-erased neutral inventory,
-- finite-spine model, and normalized provider-law table.  Inventory identity
-- remains distinct from the solver-neutral encoding policy.  Candidate and
-- complete problem sealing consume the provider inventory directly from this
-- opaque association and revalidate only the separately supplied contract;
-- callers cannot combine a context checked from one inventory with providers
-- checked from another.
module Language.Haskell.Synthesis.Semantic.Length.Problem
  ( LengthSemanticFingerprintPart (..)
  , LengthEncodingPolicyFingerprintSubject
  , LengthSessionError (..)
  , CheckedLengthSession
  , sealLengthSession
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
  , LengthProblemError (..)
  , CheckedLengthCandidate
  , CheckedLengthProblem
  , sealLengthTypedCandidateProblem
  , checkedLengthCandidateResult
  , checkedLengthCandidateUsedProviders
  , checkedLengthCandidateTermGraphFingerprint
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
