{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Bounded, solver-independent evaluation for checked scalar and binary
-- product length contracts, provider summaries, and sealed candidate problems.
--
-- This is the replay authority for concrete natural-number assignments.  It
-- deliberately consumes only opaque checked values: evaluating a caller-built
-- raw syntax tree here could diverge before the length sealer's structural
-- bounds were established.  Detached contract and provider results classify
-- one assignment without evidence authority.  A constraint-conditional
-- provider summary is rejected here before its arguments are inspected;
-- occurrence-specific static discharge belongs to candidate sealing, not this
-- detached evaluator.  Whole-problem replay can consume a problem whose
-- candidate already passed that boundary and bind
-- an exact model-relative counterexample receipt to the sealed problem
-- identities; it still supplies neither universal evidence nor permission to
-- prune other candidates.  The same replay kernel can exhaust an explicitly
-- finite Cartesian input box under independent width and assignment-count
-- limits plus the existing value bounds.  A positive receipt records the
-- versioned verifier, exact box, total and precondition-applicable assignment
-- counts, and provider/model basis.  It remains bounded/model-relative and does
-- not strengthen a solver's @unsat@ report into universal evidence.
--
-- Binary-product replay has a closed sibling error vocabulary and nominally
-- distinct counterexample and positive-box receipts.  It evaluates result
-- components from an opaque checked candidate as the postcondition demands,
-- then materializes first and second for a violation; a caller still supplies
-- only compact source-ordered natural inputs.  This module has no product
-- SMT-LIB or live-solver boundary.
module Language.Haskell.Synthesis.Semantic.Length.Evaluate
  ( LengthEvaluationLimitSource (..)
  , LengthEvaluationLimits
  , LengthEvaluationLimitField (..)
  , LengthEvaluationLimitError (..)
  , mkLengthEvaluationLimits
  , defaultLengthEvaluationLimitSource
  , defaultLengthEvaluationLimits
  , lengthAssignmentValueBitLimit
  , lengthIntermediateValueBitLimit
  , LengthContractAssignment (..)
  , LengthSpinePairContractAssignment (..)
  , LengthProblemAssignment (..)
  , LengthProviderArgumentValue (..)
  , LengthEvaluationValueSite (..)
  , LengthEvaluationError (..)
  , LengthSpinePairEvaluationValueSite (..)
  , LengthSpinePairEvaluationError (..)
  , LengthContractEvaluation (..)
  , LengthCounterexampleBasis (..)
  , ValidatedLengthCounterexample
  , validatedLengthCounterexampleInputs
  , validatedLengthCounterexampleResult
  , validatedLengthCounterexampleBasis
  , ValidatedLengthSpinePairCounterexample
  , validatedLengthSpinePairCounterexampleInputs
  , validatedLengthSpinePairCounterexampleResult
  , validatedLengthSpinePairCounterexampleBasis
  , LengthCounterexampleSimplificationError (..)
  , ValidatedLengthCounterexampleSimplification
  , lengthCounterexampleSimplificationSchemaTag
  , validatedLengthCounterexampleSimplificationOriginalInputs
  , validatedLengthCounterexampleSimplificationInspectedAssignmentCount
  , validatedLengthCounterexampleSimplificationCounterexample
  , validatedLengthCounterexampleSimplificationInputs
  , validatedLengthCounterexampleSimplificationResult
  , validatedLengthCounterexampleSimplificationBasis
  , validatedLengthCounterexampleSimplificationChanged
  , LengthSpinePairCounterexampleSimplificationError (..)
  , ValidatedLengthSpinePairCounterexampleSimplification
  , lengthSpinePairCounterexampleSimplificationSchemaTag
  , validatedLengthSpinePairCounterexampleSimplificationOriginalInputs
  , validatedLengthSpinePairCounterexampleSimplificationInspectedAssignmentCount
  , validatedLengthSpinePairCounterexampleSimplificationCounterexample
  , validatedLengthSpinePairCounterexampleSimplificationInputs
  , validatedLengthSpinePairCounterexampleSimplificationResult
  , validatedLengthSpinePairCounterexampleSimplificationBasis
  , validatedLengthSpinePairCounterexampleSimplificationChanged
  , LengthInputBoxLimitSource (..)
  , LengthInputBoxLimits
  , LengthInputBoxLimitField (..)
  , LengthInputBoxLimitError (..)
  , mkLengthInputBoxLimits
  , defaultLengthInputBoxLimitSource
  , defaultLengthInputBoxLimits
  , lengthInputBoxInputLimit
  , lengthInputBoxAssignmentLimit
  , LengthBooleanFiniteUnionLimitSource (..)
  , LengthBooleanFiniteUnionLimits
  , LengthBooleanFiniteUnionLimitField (..)
  , LengthBooleanFiniteUnionLimitError (..)
  , mkLengthBooleanFiniteUnionLimits
  , defaultLengthBooleanFiniteUnionLimitSource
  , defaultLengthBooleanFiniteUnionLimits
  , lengthBooleanFiniteUnionGeneratedBranchLimit
  , lengthBooleanFiniteUnionRuleLimitPerBranch
  , lengthBooleanFiniteUnionClosureInspectionLimitPerBranch
  , lengthBooleanFiniteUnionRetainedBoxLimit
  , lengthBooleanFiniteUnionAssignmentVisitLimit
  , lengthInputBoxValidationSchemaTag
  , LengthInputBoxValidationError (..)
  , LengthInputBoxValidation (..)
  , ValidatedLengthInputBox
  , validatedLengthInputBoxInclusiveMaximums
  , validatedLengthInputBoxAssignmentCount
  , validatedLengthInputBoxApplicableAssignmentCount
  , validatedLengthInputBoxBasis
  , lengthSpinePairInputBoxValidationSchemaTag
  , LengthSpinePairInputBoxValidationError (..)
  , ValidatedLengthSpinePairInputBox
  , validatedLengthSpinePairInputBoxInclusiveMaximums
  , validatedLengthSpinePairInputBoxAssignmentCount
  , validatedLengthSpinePairInputBoxApplicableAssignmentCount
  , validatedLengthSpinePairInputBoxBasis
  , LengthApplicableDomainInapplicability (..)
  , LengthApplicableDomainValidation (..)
  , LengthApplicableDomainValidationError (..)
  , ValidatedLengthApplicableDomain
  , lengthApplicableDomainValidationSchemaTag
  , validatedLengthApplicableDomainInclusiveMaximums
  , validatedLengthApplicableDomainAssignmentCount
  , validatedLengthApplicableDomainApplicableAssignmentCount
  , validatedLengthApplicableDomainBasis
  , LengthSpinePairApplicableDomainValidationError (..)
  , ValidatedLengthSpinePairApplicableDomain
  , lengthSpinePairApplicableDomainValidationSchemaTag
  , validatedLengthSpinePairApplicableDomainInclusiveMaximums
  , validatedLengthSpinePairApplicableDomainAssignmentCount
  , validatedLengthSpinePairApplicableDomainApplicableAssignmentCount
  , validatedLengthSpinePairApplicableDomainBasis
  , ValidatedLengthPositiveAffineApplicableDomain
  , lengthPositiveAffineApplicableDomainValidationSchemaTag
  , validatedLengthPositiveAffineApplicableDomainInclusiveMaximums
  , validatedLengthPositiveAffineApplicableDomainAssignmentCount
  , validatedLengthPositiveAffineApplicableDomainApplicableAssignmentCount
  , validatedLengthPositiveAffineApplicableDomainBasis
  , ValidatedLengthSpinePairPositiveAffineApplicableDomain
  , lengthSpinePairPositiveAffineApplicableDomainValidationSchemaTag
  , validatedLengthSpinePairPositiveAffineApplicableDomainInclusiveMaximums
  , validatedLengthSpinePairPositiveAffineApplicableDomainAssignmentCount
  , validatedLengthSpinePairPositiveAffineApplicableDomainApplicableAssignmentCount
  , validatedLengthSpinePairPositiveAffineApplicableDomainBasis
  , ValidatedLengthRelationalPositiveAffineApplicableDomain
  , lengthRelationalPositiveAffineApplicableDomainValidationSchemaTag
  , validatedLengthRelationalPositiveAffineApplicableDomainInclusiveMaximums
  , validatedLengthRelationalPositiveAffineApplicableDomainAssignmentCount
  , validatedLengthRelationalPositiveAffineApplicableDomainApplicableAssignmentCount
  , validatedLengthRelationalPositiveAffineApplicableDomainBasis
  , ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomain
  , lengthSpinePairRelationalPositiveAffineApplicableDomainValidationSchemaTag
  , validatedLengthSpinePairRelationalPositiveAffineApplicableDomainInclusiveMaximums
  , validatedLengthSpinePairRelationalPositiveAffineApplicableDomainAssignmentCount
  , validatedLengthSpinePairRelationalPositiveAffineApplicableDomainApplicableAssignmentCount
  , validatedLengthSpinePairRelationalPositiveAffineApplicableDomainBasis
  , ValidatedLengthStrictRelationalPositiveAffineApplicableDomain
  , lengthStrictRelationalPositiveAffineApplicableDomainValidationSchemaTag
  , validatedLengthStrictRelationalPositiveAffineApplicableDomainInclusiveMaximums
  , validatedLengthStrictRelationalPositiveAffineApplicableDomainAssignmentCount
  , validatedLengthStrictRelationalPositiveAffineApplicableDomainApplicableAssignmentCount
  , validatedLengthStrictRelationalPositiveAffineApplicableDomainBasis
  , ValidatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomain
  , lengthSpinePairStrictRelationalPositiveAffineApplicableDomainValidationSchemaTag
  , validatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainInclusiveMaximums
  , validatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainAssignmentCount
  , validatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainApplicableAssignmentCount
  , validatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainBasis
  , ValidatedLengthStrictRelationalPositiveAffineQuotientApplicableDomain
  , lengthStrictRelationalPositiveAffineQuotientApplicableDomainValidationSchemaTag
  , validatedLengthStrictRelationalPositiveAffineQuotientApplicableDomainInclusiveMaximums
  , validatedLengthStrictRelationalPositiveAffineQuotientApplicableDomainAssignmentCount
  , validatedLengthStrictRelationalPositiveAffineQuotientApplicableDomainApplicableAssignmentCount
  , validatedLengthStrictRelationalPositiveAffineQuotientApplicableDomainBasis
  , ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomain
  , lengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainValidationSchemaTag
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainInclusiveMaximums
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainAssignmentCount
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainApplicableAssignmentCount
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainBasis
  , ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain
  , lengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainValidationSchemaTag
  , validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainInclusiveMaximums
  , validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainAssignmentCount
  , validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainApplicableAssignmentCount
  , validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainBasis
  , ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain
  , lengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainValidationSchemaTag
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainInclusiveMaximums
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainAssignmentCount
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainApplicableAssignmentCount
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainBasis
  , ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain
  , lengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainValidationSchemaTag
  , validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainInclusiveMaximums
  , validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainAssignmentCount
  , validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainApplicableAssignmentCount
  , validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainBasis
  , ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain
  , lengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainValidationSchemaTag
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainInclusiveMaximums
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainAssignmentCount
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainApplicableAssignmentCount
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainBasis
  , LengthBooleanFiniteUnionApplicableDomainValidationError (..)
  , ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
  , lengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainValidationSchemaTag
  , validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainInclusiveMaximumBoxes
  , validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainBoxCount
  , validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainAssignmentVisitCount
  , validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainAssignmentCount
  , validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainApplicableAssignmentCount
  , validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainBasis
  , LengthSpinePairBooleanFiniteUnionApplicableDomainValidationError (..)
  , ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
  , lengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainValidationSchemaTag
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainInclusiveMaximumBoxes
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainBoxCount
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainAssignmentVisitCount
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainAssignmentCount
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainApplicableAssignmentCount
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainBasis
  , ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
  , lengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainValidationSchemaTag
  , validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainInclusiveMaximumBoxes
  , validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainBoxCount
  , validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainAssignmentVisitCount
  , validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainAssignmentCount
  , validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainApplicableAssignmentCount
  , validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainBasis
  , ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
  , lengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainValidationSchemaTag
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainInclusiveMaximumBoxes
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainBoxCount
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainAssignmentVisitCount
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainAssignmentCount
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainApplicableAssignmentCount
  , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainBasis
  , evaluateLengthContractAssignment
  , evaluateLengthSpinePairContractAssignment
  , evaluateLengthProviderApplication
  , validateLengthProblemCounterexample
  , validateLengthSpinePairProblemCounterexample
  , simplifyLengthProblemCounterexample
  , simplifyLengthSpinePairProblemCounterexample
  , validateLengthProblemInputBox
  , validateLengthSpinePairProblemInputBox
  , validateLengthProblemApplicableDomain
  , validateLengthSpinePairProblemApplicableDomain
  , validateLengthProblemPositiveAffineApplicableDomain
  , validateLengthSpinePairProblemPositiveAffineApplicableDomain
  , validateLengthProblemRelationalPositiveAffineApplicableDomain
  , validateLengthSpinePairProblemRelationalPositiveAffineApplicableDomain
  , validateLengthProblemStrictRelationalPositiveAffineApplicableDomain
  , validateLengthSpinePairProblemStrictRelationalPositiveAffineApplicableDomain
  , validateLengthProblemStrictRelationalPositiveAffineQuotientApplicableDomain
  , validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientApplicableDomain
  , validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain
  , validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain
  , validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain
  , validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain
  , validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
  , validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
  , validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
  , validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
  ) where

import Control.DeepSeq (NFData (rnf))
import Control.Monad (foldM, unless)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Collection (observedListLength)
import Language.Haskell.Synthesis.Internal.Semantic.Length (ascii)
import Language.Haskell.Synthesis.Name (Name)
import Language.Haskell.Synthesis.Semantic.Length
  ( CheckedLengthContract
  , CheckedLengthProviderSummary
  , CheckedLengthSpinePairContract
  , FiniteBinaryProductSpineLengthsV1
  , FiniteListSpineLengthV1
  , LengthContractVariable (..)
  , LengthExpression (..)
  , LengthFormula (..)
  , LengthSpinePair (..)
  , LengthSpinePairComponent (..)
  , LengthSpinePairContractVariable (..)
  , LengthProviderArgumentRole (..)
  , LengthProviderTrust (..)
  , LengthProviderVariable (..)
  , checkedLengthContractInputCount
  , checkedLengthContractPostcondition
  , checkedLengthContractPrecondition
  , checkedLengthSpinePairContractInputCount
  , checkedLengthSpinePairContractPostcondition
  , checkedLengthSpinePairContractPrecondition
  , checkedLengthProviderArgumentRoles
  , checkedLengthProviderTrust
  , checkedLengthProviderTransfer
  )
import Language.Haskell.Synthesis.Semantic.Length.Problem
  ( CheckedLengthProblem
  , CheckedLengthSpinePairProblem
  , checkedLengthCandidateResult
  , checkedLengthCandidateUsedProviders
  , checkedLengthProblemBehavioralProblem
  , checkedLengthProblemCandidate
  , checkedLengthProblemInputCount
  , checkedLengthProblemPostcondition
  , checkedLengthProblemPrecondition
  , checkedLengthSpinePairCandidateResult
  , checkedLengthSpinePairCandidateUsedProviders
  , checkedLengthSpinePairProblemBehavioralProblem
  , checkedLengthSpinePairProblemCandidate
  , checkedLengthSpinePairProblemInputCount
  , checkedLengthSpinePairProblemPostcondition
  , checkedLengthSpinePairProblemPrecondition
  )
import Language.Haskell.Synthesis.Internal.Semantic.Problem
  ( BehavioralEvidence
  , mapBehavioralEvidenceReceipt
  , mkBehavioralEvidence
  , replayBehavioralEvidence
  )
import Language.Haskell.Synthesis.Count
  ( observedNaturalBits
  , saturatedSuccessor
  )

-- | Raw operational bounds for concrete replay. Zero is valid: only the
-- natural number zero has a zero-bit representation.
data LengthEvaluationLimitSource = LengthEvaluationLimitSource
  { lengthEvaluationLimitSourceAssignmentValueBits :: Int
  , lengthEvaluationLimitSourceIntermediateValueBits :: Int
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthEvaluationLimitSource

-- | Validated nonnegative replay limits. The constructor stays private so
-- every evaluator can rely on both fields being usable as finite bounds.
data LengthEvaluationLimits = LengthEvaluationLimits !Int !Int
  deriving (Eq, Ord, Show)

instance NFData LengthEvaluationLimits where
  rnf (LengthEvaluationLimits assignments intermediate) =
    rnf assignments `seq` rnf intermediate

-- | Stable field identity for limit diagnostics.
data LengthEvaluationLimitField
  = LengthAssignmentValueBits
  | LengthIntermediateValueBits
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthEvaluationLimitField

-- | Failure to construct replay limits.
data LengthEvaluationLimitError = NegativeLengthEvaluationLimit
  !LengthEvaluationLimitField !Int
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthEvaluationLimitError

-- | Validate raw limits in declaration order.
mkLengthEvaluationLimits
  :: LengthEvaluationLimitSource
  -> Either LengthEvaluationLimitError LengthEvaluationLimits
mkLengthEvaluationLimits source = do
  nonnegative LengthAssignmentValueBits
    $ lengthEvaluationLimitSourceAssignmentValueBits source
  nonnegative LengthIntermediateValueBits
    $ lengthEvaluationLimitSourceIntermediateValueBits source
  pure $ LengthEvaluationLimits
    (lengthEvaluationLimitSourceAssignmentValueBits source)
    (lengthEvaluationLimitSourceIntermediateValueBits source)
 where
  nonnegative field value
    | value < 0 = Left $ NegativeLengthEvaluationLimit field value
    | otherwise = Right ()

-- | Conservative defaults for independently replaying checked syntax.
defaultLengthEvaluationLimitSource :: LengthEvaluationLimitSource
defaultLengthEvaluationLimitSource = LengthEvaluationLimitSource
  { lengthEvaluationLimitSourceAssignmentValueBits = 4096
  , lengthEvaluationLimitSourceIntermediateValueBits = 4096
  }

-- | Validated form of 'defaultLengthEvaluationLimitSource'.
defaultLengthEvaluationLimits :: LengthEvaluationLimits
defaultLengthEvaluationLimits = LengthEvaluationLimits 4096 4096

-- | Maximum bit width of every caller-supplied spine length.
lengthAssignmentValueBitLimit :: LengthEvaluationLimits -> Int
lengthAssignmentValueBitLimit (LengthEvaluationLimits value _) = value

-- | Maximum bit width of literals and arithmetic results during replay.
lengthIntermediateValueBitLimit :: LengthEvaluationLimits -> Int
lengthIntermediateValueBitLimit (LengthEvaluationLimits _ value) = value

-- | Concrete list-spine lengths for one contract application.
--
-- Inputs remain in the checked contract's source order.  The result is kept
-- separate because preconditions cannot refer to it, while postconditions may.
data LengthContractAssignment = LengthContractAssignment
  { lengthContractAssignmentInputs :: [Natural]
  , lengthContractAssignmentResult :: Natural
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthContractAssignment

-- | Concrete inputs and exact source-ordered results for one detached binary
-- product-of-spines contract classification.
data LengthSpinePairContractAssignment = LengthSpinePairContractAssignment
  { lengthSpinePairContractAssignmentInputs :: [Natural]
  , lengthSpinePairContractAssignmentResult :: LengthSpinePair Natural
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSpinePairContractAssignment

-- | Source-ordered natural inputs decoded for one exact candidate problem.
--
-- There is deliberately no caller-supplied result.  The validator computes
-- that value from the checked candidate retained by the problem, preventing
-- a solver model decoder from pairing valid inputs with a spoofed output.
data LengthProblemAssignment = LengthProblemAssignment
  { lengthProblemAssignmentInputs :: [Natural]
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthProblemAssignment

-- | A provider call supplies a number only where the checked role exposes a
-- list spine.  Requiring the explicit unobserved marker prevents callers from
-- smuggling a semantic claim about an opaque argument into replay.
data LengthProviderArgumentValue
  = ObservedSpineLength Natural
  | UnobservedLengthArgument
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthProviderArgumentValue

-- | Exact assignment or arithmetic site which exceeded its bit bound.
data LengthEvaluationValueSite
  = LengthContractInputValue Int
  | LengthContractResultValue
  | LengthProblemInputValue Int
  | LengthProviderSpineValue Int
  | LengthIntermediateValue
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthEvaluationValueSite

-- | Deterministic failure while replaying one checked value.
data LengthEvaluationError
  = LengthContractAssignmentArityMismatch !Int !Int
  | LengthProblemAssignmentArityMismatch !Int !Int
  | LengthProviderAssignmentArityMismatch !Int !Int
  | LengthProviderArgumentRoleMismatch
      !Int !LengthProviderArgumentRole !LengthProviderArgumentValue
  -- | The checked summary retains a nonempty constraint context, but this
  -- standalone evaluator has no candidate-local dictionary authority.
  | LengthEvaluationConditionalProviderRequiresDischarge
  | LengthEvaluationValueBitLimitExceeded
      !LengthEvaluationValueSite !Int !Int
  | LengthEvaluationInternalContractReference !LengthContractVariable
  | LengthEvaluationInternalProviderReference !LengthProviderVariable
  | LengthEvaluationInternalQuotientDivisorZero
  | LengthEvaluationInternalModuloDivisorZero
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthEvaluationError

-- | Exact bounded value site in binary product replay.
data LengthSpinePairEvaluationValueSite
  = LengthSpinePairContractInputValue !Int
  | LengthSpinePairContractResultValue !LengthSpinePairComponent
  | LengthSpinePairProblemInputValue !Int
  | LengthSpinePairIntermediateValue
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSpinePairEvaluationValueSite

-- | Closed deterministic failure surface for binary product replay.  It is
-- distinct from 'LengthEvaluationError' so adding the product domain does not
-- widen exhaustive matches over the scalar API.
data LengthSpinePairEvaluationError
  = LengthSpinePairContractAssignmentArityMismatch !Int !Int
  | LengthSpinePairProblemAssignmentArityMismatch !Int !Int
  | LengthSpinePairEvaluationValueBitLimitExceeded
      !LengthSpinePairEvaluationValueSite !Int !Int
  | LengthSpinePairEvaluationInternalContractReference
      !LengthSpinePairContractVariable
  | LengthSpinePairEvaluationInternalCandidateReference
      !LengthContractVariable
  | LengthSpinePairEvaluationInternalQuotientDivisorZero
  | LengthSpinePairEvaluationInternalModuloDivisorZero
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSpinePairEvaluationError

-- | Complete classification of one concrete contract assignment.
data LengthContractEvaluation
  = LengthPreconditionNotMet
  | LengthPostconditionSatisfied
  | LengthPostconditionViolated
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthContractEvaluation

-- | Explicit semantic basis of an independently replayed Length result.
--
-- Even the provider-independent case is a result in the versioned total
-- finite-spine model, not automatically a realized counterexample in a source
-- language with bottoms or effects.  Provider-backed results additionally
-- depend on every named assumed law in the retained list.  For a conditional
-- provider, that includes the fingerprinted assumption that the law is uniform
-- over independently admitted dictionary evidence; the basis does not expose
-- or recreate a class-resolution receipt.  The historical type name remains
-- counterexample-specific for API compatibility, but bounded positive receipts
-- reuse the same exact model/provider distinction.
data LengthCounterexampleBasis
  = ProviderIndependentFiniteSpineModel
  | FiniteSpineModelUnderAssumedProviderLaws [Name]
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthCounterexampleBasis

-- | Independently replayed model-relative violation of one sealed Length
-- problem.
--
-- The constructor stays private.  The receipt is exact relative to the
-- problem's fingerprinted semantic encoding.  Its explicit basis records any
-- assumed provider laws; it is not evidence about an unverified provider
-- implementation or source-language realization.  Its enclosing
-- 'BehavioralEvidence' can reveal this value only after replay against the
-- same complete problem identity succeeds.
data ValidatedLengthCounterexample = ValidatedLengthCounterexampleReceipt
  ![Natural]
  !Natural
  !LengthCounterexampleBasis
  deriving (Eq, Ord, Show)

instance NFData ValidatedLengthCounterexample where
  rnf (ValidatedLengthCounterexampleReceipt inputs result basis) =
    rnf inputs `seq` rnf result `seq` rnf basis

-- | Source-ordered inputs which make the sealed bad-state formula true.
validatedLengthCounterexampleInputs
  :: ValidatedLengthCounterexample
  -> [Natural]
validatedLengthCounterexampleInputs
    (ValidatedLengthCounterexampleReceipt inputs _ _) = inputs

-- | Result computed from the sealed candidate, never supplied by the caller.
validatedLengthCounterexampleResult
  :: ValidatedLengthCounterexample
  -> Natural
validatedLengthCounterexampleResult
    (ValidatedLengthCounterexampleReceipt _ result _) = result

-- | Whether replay was provider-independent or conditional on named laws.
validatedLengthCounterexampleBasis
  :: ValidatedLengthCounterexample
  -> LengthCounterexampleBasis
validatedLengthCounterexampleBasis
    (ValidatedLengthCounterexampleReceipt _ _ basis) = basis

-- | Independently replayed model-relative violation for one exact binary
-- product problem.  Both results are recomputed from the checked candidate;
-- the caller supplies inputs only.
data ValidatedLengthSpinePairCounterexample =
  ValidatedLengthSpinePairCounterexampleReceipt
    ![Natural]
    !(LengthSpinePair Natural)
    !LengthCounterexampleBasis
  deriving (Eq, Ord, Show)

instance NFData ValidatedLengthSpinePairCounterexample where
  rnf (ValidatedLengthSpinePairCounterexampleReceipt inputs result basis) =
    rnf inputs `seq` rnf result `seq` rnf basis

-- | Compact source-ordered inputs which violate the exact product problem.
validatedLengthSpinePairCounterexampleInputs
  :: ValidatedLengthSpinePairCounterexample -> [Natural]
validatedLengthSpinePairCounterexampleInputs
    (ValidatedLengthSpinePairCounterexampleReceipt inputs _ _) = inputs

-- | Both source-ordered result lengths recomputed from the checked candidate.
validatedLengthSpinePairCounterexampleResult
  :: ValidatedLengthSpinePairCounterexample -> LengthSpinePair Natural
validatedLengthSpinePairCounterexampleResult
    (ValidatedLengthSpinePairCounterexampleReceipt _ result _) = result

-- | Provider-independent or assumed-provider-relative semantic basis.
validatedLengthSpinePairCounterexampleBasis
  :: ValidatedLengthSpinePairCounterexample -> LengthCounterexampleBasis
validatedLengthSpinePairCounterexampleBasis
    (ValidatedLengthSpinePairCounterexampleReceipt _ _ basis) = basis

-- | Raw independent bounds for one finite-box traversal.  Input width uses a
-- signed source so configuration mistakes can be rejected explicitly;
-- assignment count is naturally nonnegative.
data LengthInputBoxLimitSource = LengthInputBoxLimitSource
  { lengthInputBoxLimitSourceMaximumInputs :: Int
  , lengthInputBoxLimitSourceMaximumAssignments :: Natural
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthInputBoxLimitSource

-- | Validated traversal bounds.  The constructor stays private so neither a
-- very wide checked problem nor a large Cartesian product can reach allocation
-- or enumeration without explicit caller authority.
data LengthInputBoxLimits = LengthInputBoxLimits !Int !Natural
  deriving (Eq, Ord, Show)

instance NFData LengthInputBoxLimits where
  rnf (LengthInputBoxLimits inputs assignments) =
    rnf inputs `seq` rnf assignments

data LengthInputBoxLimitField = LengthInputBoxMaximumInputs
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthInputBoxLimitField

data LengthInputBoxLimitError = NegativeLengthInputBoxLimit
  !LengthInputBoxLimitField !Int
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthInputBoxLimitError

-- | Seal width before retaining the naturally nonnegative assignment cap.
-- Zero inputs is meaningful and admits only nullary problems.  Zero
-- assignments then rejects even a nullary box, which contains one assignment.
mkLengthInputBoxLimits
  :: LengthInputBoxLimitSource
  -> Either LengthInputBoxLimitError LengthInputBoxLimits
mkLengthInputBoxLimits source
  | maximumInputs < 0 = Left $ NegativeLengthInputBoxLimit
      LengthInputBoxMaximumInputs maximumInputs
  | otherwise = Right $ LengthInputBoxLimits maximumInputs
      (lengthInputBoxLimitSourceMaximumAssignments source)
 where
  maximumInputs = lengthInputBoxLimitSourceMaximumInputs source

-- | The documented default box bounds: eight inputs and 65,536 total
-- assignments.
defaultLengthInputBoxLimitSource :: LengthInputBoxLimitSource
defaultLengthInputBoxLimitSource = LengthInputBoxLimitSource
  { lengthInputBoxLimitSourceMaximumInputs = 8
  , lengthInputBoxLimitSourceMaximumAssignments = 65536
  }

-- | Conservative default for independently checking one finite input box.
defaultLengthInputBoxLimits :: LengthInputBoxLimits
defaultLengthInputBoxLimits = LengthInputBoxLimits 8 65536

-- | Maximum compact modeled-input arity admitted before bounds are demanded.
lengthInputBoxInputLimit :: LengthInputBoxLimits -> Int
lengthInputBoxInputLimit (LengthInputBoxLimits inputs _) = inputs

-- | Maximum number of assignments which may be enumerated.
lengthInputBoxAssignmentLimit :: LengthInputBoxLimits -> Natural
lengthInputBoxAssignmentLimit (LengthInputBoxLimits _ assignments) = assignments

-- | Raw operational bounds for finite Boolean expansion, consequence closure,
-- and finite-union enumeration.  Every field is signed so malformed external
-- configuration is rejected before a checked precondition is inspected.
data LengthBooleanFiniteUnionLimitSource =
  LengthBooleanFiniteUnionLimitSource
    { lengthBooleanFiniteUnionLimitSourceMaximumGeneratedBranches :: Int
    , lengthBooleanFiniteUnionLimitSourceMaximumRulesPerBranch :: Int
    , lengthBooleanFiniteUnionLimitSourceMaximumClosureInspectionsPerBranch
        :: Int
    , lengthBooleanFiniteUnionLimitSourceMaximumRetainedBoxes :: Int
    , lengthBooleanFiniteUnionLimitSourceMaximumAssignmentVisits :: Int
    }
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthBooleanFiniteUnionLimitSource

-- | Validated finite-union work limits.  The constructor is private so every
-- validator may use all five bounds as nonnegative finite machine counts.
data LengthBooleanFiniteUnionLimits = LengthBooleanFiniteUnionLimits
  !Int
  !Int
  !Int
  !Int
  !Int
  deriving (Eq, Ord, Show)

instance NFData LengthBooleanFiniteUnionLimits where
  rnf (LengthBooleanFiniteUnionLimits branches rules inspections boxes visits) =
    rnf branches `seq` rnf rules `seq` rnf inspections `seq`
    rnf boxes `seq` rnf visits

-- | Stable declaration-order identity for finite-union limit diagnostics.
data LengthBooleanFiniteUnionLimitField
  = LengthBooleanFiniteUnionMaximumGeneratedBranches
  | LengthBooleanFiniteUnionMaximumRulesPerBranch
  | LengthBooleanFiniteUnionMaximumClosureInspectionsPerBranch
  | LengthBooleanFiniteUnionMaximumRetainedBoxes
  | LengthBooleanFiniteUnionMaximumAssignmentVisits
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthBooleanFiniteUnionLimitField

-- | Failure to seal finite-union work limits.
data LengthBooleanFiniteUnionLimitError =
  NegativeLengthBooleanFiniteUnionLimit
    !LengthBooleanFiniteUnionLimitField
    !Int
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthBooleanFiniteUnionLimitError

-- | Validate all raw finite-union limits in record declaration order.
mkLengthBooleanFiniteUnionLimits
  :: LengthBooleanFiniteUnionLimitSource
  -> Either LengthBooleanFiniteUnionLimitError LengthBooleanFiniteUnionLimits
mkLengthBooleanFiniteUnionLimits source = do
  nonnegative LengthBooleanFiniteUnionMaximumGeneratedBranches branches
  nonnegative LengthBooleanFiniteUnionMaximumRulesPerBranch rules
  nonnegative
    LengthBooleanFiniteUnionMaximumClosureInspectionsPerBranch inspections
  nonnegative LengthBooleanFiniteUnionMaximumRetainedBoxes boxes
  nonnegative LengthBooleanFiniteUnionMaximumAssignmentVisits visits
  pure $ LengthBooleanFiniteUnionLimits
    branches rules inspections boxes visits
 where
  branches =
    lengthBooleanFiniteUnionLimitSourceMaximumGeneratedBranches source
  rules = lengthBooleanFiniteUnionLimitSourceMaximumRulesPerBranch source
  inspections =
    lengthBooleanFiniteUnionLimitSourceMaximumClosureInspectionsPerBranch source
  boxes = lengthBooleanFiniteUnionLimitSourceMaximumRetainedBoxes source
  visits = lengthBooleanFiniteUnionLimitSourceMaximumAssignmentVisits source

  nonnegative field value
    | value < 0 = Left $ NegativeLengthBooleanFiniteUnionLimit field value
    | otherwise = Right ()

-- | Conservative defaults for one checked normalized Boolean precondition.
defaultLengthBooleanFiniteUnionLimitSource
  :: LengthBooleanFiniteUnionLimitSource
defaultLengthBooleanFiniteUnionLimitSource = LengthBooleanFiniteUnionLimitSource
  { lengthBooleanFiniteUnionLimitSourceMaximumGeneratedBranches = 256
  , lengthBooleanFiniteUnionLimitSourceMaximumRulesPerBranch = 64
  , lengthBooleanFiniteUnionLimitSourceMaximumClosureInspectionsPerBranch = 4096
  , lengthBooleanFiniteUnionLimitSourceMaximumRetainedBoxes = 256
  , lengthBooleanFiniteUnionLimitSourceMaximumAssignmentVisits = 262144
  }

-- | Validated form of 'defaultLengthBooleanFiniteUnionLimitSource'.
defaultLengthBooleanFiniteUnionLimits :: LengthBooleanFiniteUnionLimits
defaultLengthBooleanFiniteUnionLimits =
  LengthBooleanFiniteUnionLimits 256 64 4096 256 262144

lengthBooleanFiniteUnionGeneratedBranchLimit
  :: LengthBooleanFiniteUnionLimits -> Int
lengthBooleanFiniteUnionGeneratedBranchLimit
    (LengthBooleanFiniteUnionLimits branches _ _ _ _) = branches

lengthBooleanFiniteUnionRuleLimitPerBranch
  :: LengthBooleanFiniteUnionLimits -> Int
lengthBooleanFiniteUnionRuleLimitPerBranch
    (LengthBooleanFiniteUnionLimits _ rules _ _ _) = rules

lengthBooleanFiniteUnionClosureInspectionLimitPerBranch
  :: LengthBooleanFiniteUnionLimits -> Int
lengthBooleanFiniteUnionClosureInspectionLimitPerBranch
    (LengthBooleanFiniteUnionLimits _ _ inspections _ _) = inspections

lengthBooleanFiniteUnionRetainedBoxLimit
  :: LengthBooleanFiniteUnionLimits -> Int
lengthBooleanFiniteUnionRetainedBoxLimit
    (LengthBooleanFiniteUnionLimits _ _ _ boxes _) = boxes

lengthBooleanFiniteUnionAssignmentVisitLimit
  :: LengthBooleanFiniteUnionLimits -> Int
lengthBooleanFiniteUnionAssignmentVisitLimit
    (LengthBooleanFiniteUnionLimits _ _ _ _ visits) = visits

-- | Versioned semantics of the deterministic bounded verifier.
--
-- This tag is receipt metadata, not a Length problem, SMT query, protocol, or
-- execution identity.  Input-box validation changes none of those existing
-- canonical bytes.
lengthInputBoxValidationSchemaTag :: [Word8]
lengthInputBoxValidationSchemaTag =
  ascii "finite-list-spine-length/bounded-input-box-validation/v1"

-- | Fixed-precedence failure while validating one finite input box.
--
-- Bounds are source ordered and inclusive.  The sealed problem's compact input
-- count is checked against the width cap before the raw bounds are demanded;
-- bounds arity is then observed productively before any bound value, bound
-- values are checked left-to-right against the existing assignment-value
-- limit, and the Cartesian-product size is checked before the first assignment
-- is evaluated.  Evaluation failures identify the zero-based lexicographic
-- assignment ordinal without retaining another copy of its values.
data LengthInputBoxValidationError
  = LengthInputBoxProblemInputLimitExceeded !Int !Int
  | LengthInputBoxBoundsArityMismatch !Int !Int
  | LengthInputBoxMaximumValueRejected !Int !LengthEvaluationError
  | LengthInputBoxAssignmentLimitExceeded !Natural !Natural
  | LengthInputBoxAssignmentEvaluationRejected
      !Natural !LengthEvaluationError
  | LengthInputBoxInternalEnumerationInvariant
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthInputBoxValidationError

-- | Complete result of one finite input-box validation.
--
-- This sum carries authority only through its payloads.  Its public
-- constructors are classification conveniences: callers still cannot forge a
-- 'BehavioralEvidence', 'ValidatedLengthCounterexample', or
-- 'ValidatedLengthInputBox'.
data LengthInputBoxValidation counterexample validated
  = LengthInputBoxCounterexample counterexample
  | LengthInputBoxValidated validated
  deriving (Eq, Ord, Show, Generic)

instance (NFData counterexample, NFData validated) =>
    NFData (LengthInputBoxValidation counterexample validated)

-- | Independent finite-domain validation of one exact sealed Length problem.
--
-- The receipt retains the fixed verifier schema, the inclusive source-ordered
-- box, the complete number of assignments checked, and how many satisfied the
-- contract precondition.  A zero applicable count is therefore visible rather
-- than masquerading as a non-vacuous result.  The semantic basis remains
-- explicit because validation may still be relative to assumed provider laws
-- and always remains relative to the total finite-spine model; it is neither
-- universal behavior nor a source-language totality claim.
data ValidatedLengthInputBox = ValidatedLengthInputBoxReceipt
  ![Word8]
  ![Natural]
  !Natural
  !Natural
  !LengthCounterexampleBasis
  deriving (Eq, Ord, Show)

instance NFData ValidatedLengthInputBox where
  rnf (ValidatedLengthInputBoxReceipt schema maximums assignments applicable
      basis) =
    rnf schema `seq` rnf maximums `seq` rnf assignments `seq`
    rnf applicable `seq` rnf basis

-- | Inclusive source-ordered maximum for every modeled input.
validatedLengthInputBoxInclusiveMaximums
  :: ValidatedLengthInputBox
  -> [Natural]
validatedLengthInputBoxInclusiveMaximums
    (ValidatedLengthInputBoxReceipt _ maximums _ _ _) = maximums

-- | Exact cardinality of the completely checked Cartesian product.
validatedLengthInputBoxAssignmentCount
  :: ValidatedLengthInputBox
  -> Natural
validatedLengthInputBoxAssignmentCount
    (ValidatedLengthInputBoxReceipt _ _ assignments _ _) = assignments

-- | Number of checked assignments for which the precondition held.
validatedLengthInputBoxApplicableAssignmentCount
  :: ValidatedLengthInputBox
  -> Natural
validatedLengthInputBoxApplicableAssignmentCount
    (ValidatedLengthInputBoxReceipt _ _ _ applicable _) = applicable

-- | Provider-independent or assumed-provider-relative semantic basis.
validatedLengthInputBoxBasis
  :: ValidatedLengthInputBox
  -> LengthCounterexampleBasis
validatedLengthInputBoxBasis
    (ValidatedLengthInputBoxReceipt _ _ _ _ basis) = basis

-- | Versioned deterministic verifier semantics for the nominal binary
-- product domain.
lengthSpinePairInputBoxValidationSchemaTag :: [Word8]
lengthSpinePairInputBoxValidationSchemaTag =
  ascii "finite-binary-product-spine-lengths/bounded-input-box-validation/v1"

-- | Fixed-precedence finite-box failures carrying the closed product replay
-- error vocabulary.  Bounds remain source ordered and inclusive.
data LengthSpinePairInputBoxValidationError
  = LengthSpinePairInputBoxProblemInputLimitExceeded !Int !Int
  | LengthSpinePairInputBoxBoundsArityMismatch !Int !Int
  | LengthSpinePairInputBoxMaximumValueRejected
      !Int !LengthSpinePairEvaluationError
  | LengthSpinePairInputBoxAssignmentLimitExceeded !Natural !Natural
  | LengthSpinePairInputBoxAssignmentEvaluationRejected
      !Natural !LengthSpinePairEvaluationError
  | LengthSpinePairInputBoxInternalEnumerationInvariant
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSpinePairInputBoxValidationError

-- | Positive bounded-validation receipt for one exact product problem.
data ValidatedLengthSpinePairInputBox =
  ValidatedLengthSpinePairInputBoxReceipt
    ![Word8]
    ![Natural]
    !Natural
    !Natural
    !LengthCounterexampleBasis
  deriving (Eq, Ord, Show)

instance NFData ValidatedLengthSpinePairInputBox where
  rnf (ValidatedLengthSpinePairInputBoxReceipt schema maximums assignments
      applicable basis) =
    rnf schema `seq` rnf maximums `seq` rnf assignments `seq`
    rnf applicable `seq` rnf basis

-- | Inclusive source-ordered maximum for every compact modeled input.
validatedLengthSpinePairInputBoxInclusiveMaximums
  :: ValidatedLengthSpinePairInputBox -> [Natural]
validatedLengthSpinePairInputBoxInclusiveMaximums
    (ValidatedLengthSpinePairInputBoxReceipt _ maximums _ _ _) = maximums

-- | Exact cardinality of the completely checked Cartesian product.
validatedLengthSpinePairInputBoxAssignmentCount
  :: ValidatedLengthSpinePairInputBox -> Natural
validatedLengthSpinePairInputBoxAssignmentCount
    (ValidatedLengthSpinePairInputBoxReceipt _ _ assignments _ _) = assignments

-- | Number of checked assignments for which the precondition held.
validatedLengthSpinePairInputBoxApplicableAssignmentCount
  :: ValidatedLengthSpinePairInputBox -> Natural
validatedLengthSpinePairInputBoxApplicableAssignmentCount
    (ValidatedLengthSpinePairInputBoxReceipt _ _ _ applicable _) = applicable

-- | Provider-independent or assumed-provider-relative semantic basis.
validatedLengthSpinePairInputBoxBasis
  :: ValidatedLengthSpinePairInputBox -> LengthCounterexampleBasis
validatedLengthSpinePairInputBoxBasis
    (ValidatedLengthSpinePairInputBoxReceipt _ _ _ _ basis) = basis

-- | Why an exact checked problem does not expose a finite applicable domain
-- through the deliberately narrow version-one coverage rule.
--
-- Preconditions are already bounded and normalized by contract sealing.  The
-- rule nevertheless recognizes only a direct top-level conjunct of the exact
-- form @input <= literal@ for every compact modeled input.  It does not infer
-- bounds from equality, arithmetic, negation, conditionals, or implications.
-- The first missing input is reported in compact source order.
data LengthApplicableDomainInapplicability
  = LengthApplicableDomainInputUpperBoundMissing !Int
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthApplicableDomainInapplicability

-- | Complete classification of one attempt to validate a problem's entire
-- applicable input domain.
--
-- Inapplicability is an ordinary conservative result: the version-one direct
-- coverage rule could not derive a finite box.  Counterexample and established
-- payloads retain authority only through their opaque evidence values.
data LengthApplicableDomainValidation counterexample established
  = LengthApplicableDomainInapplicable
      !LengthApplicableDomainInapplicability
  | LengthApplicableDomainCounterexample !counterexample
  | LengthApplicableDomainEstablished !established
  deriving (Eq, Ord, Show, Generic)

instance (NFData counterexample, NFData established) =>
    NFData (LengthApplicableDomainValidation counterexample established)

-- | Operational failure after an applicable-domain attempt has passed its
-- semantic coverage gate.  Width failure is intentionally represented by the
-- existing input-box error and occurs before the checked precondition is
-- scanned.
data LengthApplicableDomainValidationError
  = LengthApplicableDomainInputBoxValidationRejected
      !LengthInputBoxValidationError
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthApplicableDomainValidationError

-- | Versioned semantics of scalar applicable-domain establishment.
--
-- This tag belongs only to the new receipt.  The exact normalized precondition
-- remains owned by the already fingerprinted problem, and no problem, query,
-- protocol, or live-execution identity changes.
lengthApplicableDomainValidationSchemaTag :: [Word8]
lengthApplicableDomainValidationSchemaTag =
  ascii "finite-list-spine-length/finite-precondition-domain-establishment/v1"

-- | Complete validation of every assignment on which one exact scalar Length
-- problem can apply under the version-one direct-bound rule.
--
-- The nested box receipt owns the exact derived maxima, traversal counts, and
-- model/provider basis.  Its enclosing 'BehavioralEvidence' retains the same
-- exact problem association as the independently completed box validation.
data ValidatedLengthApplicableDomain =
  ValidatedLengthApplicableDomainReceipt
    ![Word8]
    !ValidatedLengthInputBox
  deriving (Eq, Ord, Show)

instance NFData ValidatedLengthApplicableDomain where
  rnf (ValidatedLengthApplicableDomainReceipt schema inputBox) =
    rnf schema `seq` rnf inputBox

validatedLengthApplicableDomainInclusiveMaximums
  :: ValidatedLengthApplicableDomain
  -> [Natural]
validatedLengthApplicableDomainInclusiveMaximums
    (ValidatedLengthApplicableDomainReceipt _ inputBox) =
  validatedLengthInputBoxInclusiveMaximums inputBox

validatedLengthApplicableDomainAssignmentCount
  :: ValidatedLengthApplicableDomain
  -> Natural
validatedLengthApplicableDomainAssignmentCount
    (ValidatedLengthApplicableDomainReceipt _ inputBox) =
  validatedLengthInputBoxAssignmentCount inputBox

validatedLengthApplicableDomainApplicableAssignmentCount
  :: ValidatedLengthApplicableDomain
  -> Natural
validatedLengthApplicableDomainApplicableAssignmentCount
    (ValidatedLengthApplicableDomainReceipt _ inputBox) =
  validatedLengthInputBoxApplicableAssignmentCount inputBox

validatedLengthApplicableDomainBasis
  :: ValidatedLengthApplicableDomain
  -> LengthCounterexampleBasis
validatedLengthApplicableDomainBasis
    (ValidatedLengthApplicableDomainReceipt _ inputBox) =
  validatedLengthInputBoxBasis inputBox

-- | Product-domain operational failure after direct finite coverage succeeds.
data LengthSpinePairApplicableDomainValidationError
  = LengthSpinePairApplicableDomainInputBoxValidationRejected
      !LengthSpinePairInputBoxValidationError
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSpinePairApplicableDomainValidationError

-- | Versioned semantics of binary-product applicable-domain establishment.
lengthSpinePairApplicableDomainValidationSchemaTag :: [Word8]
lengthSpinePairApplicableDomainValidationSchemaTag = ascii
  "finite-binary-product-spine-lengths/finite-precondition-domain-establishment/v1"

-- | Complete applicable-domain receipt for one exact nominal product problem.
data ValidatedLengthSpinePairApplicableDomain =
  ValidatedLengthSpinePairApplicableDomainReceipt
    ![Word8]
    !ValidatedLengthSpinePairInputBox
  deriving (Eq, Ord, Show)

instance NFData ValidatedLengthSpinePairApplicableDomain where
  rnf (ValidatedLengthSpinePairApplicableDomainReceipt schema inputBox) =
    rnf schema `seq` rnf inputBox

validatedLengthSpinePairApplicableDomainInclusiveMaximums
  :: ValidatedLengthSpinePairApplicableDomain
  -> [Natural]
validatedLengthSpinePairApplicableDomainInclusiveMaximums
    (ValidatedLengthSpinePairApplicableDomainReceipt _ inputBox) =
  validatedLengthSpinePairInputBoxInclusiveMaximums inputBox

validatedLengthSpinePairApplicableDomainAssignmentCount
  :: ValidatedLengthSpinePairApplicableDomain
  -> Natural
validatedLengthSpinePairApplicableDomainAssignmentCount
    (ValidatedLengthSpinePairApplicableDomainReceipt _ inputBox) =
  validatedLengthSpinePairInputBoxAssignmentCount inputBox

validatedLengthSpinePairApplicableDomainApplicableAssignmentCount
  :: ValidatedLengthSpinePairApplicableDomain
  -> Natural
validatedLengthSpinePairApplicableDomainApplicableAssignmentCount
    (ValidatedLengthSpinePairApplicableDomainReceipt _ inputBox) =
  validatedLengthSpinePairInputBoxApplicableAssignmentCount inputBox

validatedLengthSpinePairApplicableDomainBasis
  :: ValidatedLengthSpinePairApplicableDomain
  -> LengthCounterexampleBasis
validatedLengthSpinePairApplicableDomainBasis
    (ValidatedLengthSpinePairApplicableDomainReceipt _ inputBox) =
  validatedLengthSpinePairInputBoxBasis inputBox

-- | Versioned positive-affine coverage semantics for the scalar applicable
-- domain.  This tag belongs only to the additive receipt below; the literal
-- direct-bound v1 receipt and every problem, query, protocol, and live identity
-- remain unchanged.
lengthPositiveAffineApplicableDomainValidationSchemaTag :: [Word8]
lengthPositiveAffineApplicableDomainValidationSchemaTag = ascii
  "finite-list-spine-length/positive-affine-precondition-domain-establishment/v1"

-- | Complete scalar applicable-domain validation under the positive-affine
-- coverage rule.  The constructor is private.  Its nested box receipt owns the
-- exact derived maxima, traversal counts, and model/provider basis.
data ValidatedLengthPositiveAffineApplicableDomain =
  ValidatedLengthPositiveAffineApplicableDomainReceipt
    ![Word8]
    !ValidatedLengthInputBox
  deriving (Eq, Ord, Show)

instance NFData ValidatedLengthPositiveAffineApplicableDomain where
  rnf (ValidatedLengthPositiveAffineApplicableDomainReceipt schema inputBox) =
    rnf schema `seq` rnf inputBox

validatedLengthPositiveAffineApplicableDomainInclusiveMaximums
  :: ValidatedLengthPositiveAffineApplicableDomain
  -> [Natural]
validatedLengthPositiveAffineApplicableDomainInclusiveMaximums
    (ValidatedLengthPositiveAffineApplicableDomainReceipt _ inputBox) =
  validatedLengthInputBoxInclusiveMaximums inputBox

validatedLengthPositiveAffineApplicableDomainAssignmentCount
  :: ValidatedLengthPositiveAffineApplicableDomain
  -> Natural
validatedLengthPositiveAffineApplicableDomainAssignmentCount
    (ValidatedLengthPositiveAffineApplicableDomainReceipt _ inputBox) =
  validatedLengthInputBoxAssignmentCount inputBox

validatedLengthPositiveAffineApplicableDomainApplicableAssignmentCount
  :: ValidatedLengthPositiveAffineApplicableDomain
  -> Natural
validatedLengthPositiveAffineApplicableDomainApplicableAssignmentCount
    (ValidatedLengthPositiveAffineApplicableDomainReceipt _ inputBox) =
  validatedLengthInputBoxApplicableAssignmentCount inputBox

validatedLengthPositiveAffineApplicableDomainBasis
  :: ValidatedLengthPositiveAffineApplicableDomain
  -> LengthCounterexampleBasis
validatedLengthPositiveAffineApplicableDomainBasis
    (ValidatedLengthPositiveAffineApplicableDomainReceipt _ inputBox) =
  validatedLengthInputBoxBasis inputBox

-- | Nominal binary-product sibling of
-- 'lengthPositiveAffineApplicableDomainValidationSchemaTag'.
lengthSpinePairPositiveAffineApplicableDomainValidationSchemaTag :: [Word8]
lengthSpinePairPositiveAffineApplicableDomainValidationSchemaTag = ascii
  "finite-binary-product-spine-lengths/positive-affine-precondition-domain-establishment/v1"

-- | Complete product applicable-domain validation under the positive-affine
-- coverage rule.  Scalar and product receipts remain nominally disjoint even
-- though their private extraction kernel is shared.
data ValidatedLengthSpinePairPositiveAffineApplicableDomain =
  ValidatedLengthSpinePairPositiveAffineApplicableDomainReceipt
    ![Word8]
    !ValidatedLengthSpinePairInputBox
  deriving (Eq, Ord, Show)

instance NFData ValidatedLengthSpinePairPositiveAffineApplicableDomain where
  rnf (ValidatedLengthSpinePairPositiveAffineApplicableDomainReceipt
      schema inputBox) = rnf schema `seq` rnf inputBox

validatedLengthSpinePairPositiveAffineApplicableDomainInclusiveMaximums
  :: ValidatedLengthSpinePairPositiveAffineApplicableDomain
  -> [Natural]
validatedLengthSpinePairPositiveAffineApplicableDomainInclusiveMaximums
    (ValidatedLengthSpinePairPositiveAffineApplicableDomainReceipt
      _ inputBox) = validatedLengthSpinePairInputBoxInclusiveMaximums inputBox

validatedLengthSpinePairPositiveAffineApplicableDomainAssignmentCount
  :: ValidatedLengthSpinePairPositiveAffineApplicableDomain
  -> Natural
validatedLengthSpinePairPositiveAffineApplicableDomainAssignmentCount
    (ValidatedLengthSpinePairPositiveAffineApplicableDomainReceipt
      _ inputBox) = validatedLengthSpinePairInputBoxAssignmentCount inputBox

validatedLengthSpinePairPositiveAffineApplicableDomainApplicableAssignmentCount
  :: ValidatedLengthSpinePairPositiveAffineApplicableDomain
  -> Natural
validatedLengthSpinePairPositiveAffineApplicableDomainApplicableAssignmentCount
    (ValidatedLengthSpinePairPositiveAffineApplicableDomainReceipt
      _ inputBox) =
  validatedLengthSpinePairInputBoxApplicableAssignmentCount inputBox

validatedLengthSpinePairPositiveAffineApplicableDomainBasis
  :: ValidatedLengthSpinePairPositiveAffineApplicableDomain
  -> LengthCounterexampleBasis
validatedLengthSpinePairPositiveAffineApplicableDomainBasis
    (ValidatedLengthSpinePairPositiveAffineApplicableDomainReceipt
      _ inputBox) = validatedLengthSpinePairInputBoxBasis inputBox

-- | Versioned relational positive-affine coverage semantics for the scalar
-- applicable domain.  This additive receipt is nominally separate from both
-- the literal-direct and literal-ceiling positive-affine rules.
lengthRelationalPositiveAffineApplicableDomainValidationSchemaTag :: [Word8]
lengthRelationalPositiveAffineApplicableDomainValidationSchemaTag = ascii
  "finite-list-spine-length/relational-positive-affine-precondition-domain-establishment/v1"

-- | Complete scalar applicable-domain validation under the relational
-- positive-affine coverage rule.  The private constructor retains the exact
-- completed scalar input-box receipt beside the rule's fixed schema tag.
data ValidatedLengthRelationalPositiveAffineApplicableDomain =
  ValidatedLengthRelationalPositiveAffineApplicableDomainReceipt
    ![Word8]
    !ValidatedLengthInputBox
  deriving (Eq, Ord, Show)

instance NFData ValidatedLengthRelationalPositiveAffineApplicableDomain where
  rnf
      (ValidatedLengthRelationalPositiveAffineApplicableDomainReceipt
        schema inputBox) = rnf schema `seq` rnf inputBox

validatedLengthRelationalPositiveAffineApplicableDomainInclusiveMaximums
  :: ValidatedLengthRelationalPositiveAffineApplicableDomain
  -> [Natural]
validatedLengthRelationalPositiveAffineApplicableDomainInclusiveMaximums
    (ValidatedLengthRelationalPositiveAffineApplicableDomainReceipt
      _ inputBox) = validatedLengthInputBoxInclusiveMaximums inputBox

validatedLengthRelationalPositiveAffineApplicableDomainAssignmentCount
  :: ValidatedLengthRelationalPositiveAffineApplicableDomain
  -> Natural
validatedLengthRelationalPositiveAffineApplicableDomainAssignmentCount
    (ValidatedLengthRelationalPositiveAffineApplicableDomainReceipt
      _ inputBox) = validatedLengthInputBoxAssignmentCount inputBox

validatedLengthRelationalPositiveAffineApplicableDomainApplicableAssignmentCount
  :: ValidatedLengthRelationalPositiveAffineApplicableDomain
  -> Natural
validatedLengthRelationalPositiveAffineApplicableDomainApplicableAssignmentCount
    (ValidatedLengthRelationalPositiveAffineApplicableDomainReceipt
      _ inputBox) = validatedLengthInputBoxApplicableAssignmentCount inputBox

validatedLengthRelationalPositiveAffineApplicableDomainBasis
  :: ValidatedLengthRelationalPositiveAffineApplicableDomain
  -> LengthCounterexampleBasis
validatedLengthRelationalPositiveAffineApplicableDomainBasis
    (ValidatedLengthRelationalPositiveAffineApplicableDomainReceipt
      _ inputBox) = validatedLengthInputBoxBasis inputBox

-- | Nominal binary-product sibling of
-- 'lengthRelationalPositiveAffineApplicableDomainValidationSchemaTag'.
lengthSpinePairRelationalPositiveAffineApplicableDomainValidationSchemaTag
  :: [Word8]
lengthSpinePairRelationalPositiveAffineApplicableDomainValidationSchemaTag =
  ascii
    "finite-binary-product-spine-lengths/relational-positive-affine-precondition-domain-establishment/v1"

-- | Complete product applicable-domain validation under the relational
-- positive-affine coverage rule.  Scalar and product evidence cannot be
-- interchanged even though their private extraction kernel is shared.
data ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomain =
  ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomainReceipt
    ![Word8]
    !ValidatedLengthSpinePairInputBox
  deriving (Eq, Ord, Show)

instance NFData
    ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomain where
  rnf
      (ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomainReceipt
        schema inputBox) = rnf schema `seq` rnf inputBox

validatedLengthSpinePairRelationalPositiveAffineApplicableDomainInclusiveMaximums
  :: ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomain
  -> [Natural]
validatedLengthSpinePairRelationalPositiveAffineApplicableDomainInclusiveMaximums
    (ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomainReceipt
      _ inputBox) =
        validatedLengthSpinePairInputBoxInclusiveMaximums inputBox

validatedLengthSpinePairRelationalPositiveAffineApplicableDomainAssignmentCount
  :: ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomain
  -> Natural
validatedLengthSpinePairRelationalPositiveAffineApplicableDomainAssignmentCount
    (ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomainReceipt
      _ inputBox) = validatedLengthSpinePairInputBoxAssignmentCount inputBox

validatedLengthSpinePairRelationalPositiveAffineApplicableDomainApplicableAssignmentCount
  :: ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomain
  -> Natural
validatedLengthSpinePairRelationalPositiveAffineApplicableDomainApplicableAssignmentCount
    (ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomainReceipt
      _ inputBox) =
        validatedLengthSpinePairInputBoxApplicableAssignmentCount inputBox

validatedLengthSpinePairRelationalPositiveAffineApplicableDomainBasis
  :: ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomain
  -> LengthCounterexampleBasis
validatedLengthSpinePairRelationalPositiveAffineApplicableDomainBasis
    (ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomainReceipt
      _ inputBox) = validatedLengthSpinePairInputBoxBasis inputBox

-- | Versioned strict relational positive-affine coverage semantics for the
-- scalar applicable domain.  This additive rule recognizes the exact natural
-- complement of a top-level affine at-most clause without changing checked
-- contract normalization or any established receipt family.
lengthStrictRelationalPositiveAffineApplicableDomainValidationSchemaTag
  :: [Word8]
lengthStrictRelationalPositiveAffineApplicableDomainValidationSchemaTag =
  ascii
    "finite-list-spine-length/strict-relational-positive-affine-precondition-domain-establishment/v1"

-- | Complete scalar applicable-domain validation under the strict relational
-- positive-affine coverage rule.  Its constructor is private and its nested
-- receipt retains the existing finite-box replay authority.
data ValidatedLengthStrictRelationalPositiveAffineApplicableDomain =
  ValidatedLengthStrictRelationalPositiveAffineApplicableDomainReceipt
    ![Word8]
    !ValidatedLengthInputBox
  deriving (Eq, Ord, Show)

instance NFData
    ValidatedLengthStrictRelationalPositiveAffineApplicableDomain where
  rnf
      (ValidatedLengthStrictRelationalPositiveAffineApplicableDomainReceipt
        schema inputBox) = rnf schema `seq` rnf inputBox

validatedLengthStrictRelationalPositiveAffineApplicableDomainInclusiveMaximums
  :: ValidatedLengthStrictRelationalPositiveAffineApplicableDomain
  -> [Natural]
validatedLengthStrictRelationalPositiveAffineApplicableDomainInclusiveMaximums
    (ValidatedLengthStrictRelationalPositiveAffineApplicableDomainReceipt
      _ inputBox) = validatedLengthInputBoxInclusiveMaximums inputBox

validatedLengthStrictRelationalPositiveAffineApplicableDomainAssignmentCount
  :: ValidatedLengthStrictRelationalPositiveAffineApplicableDomain
  -> Natural
validatedLengthStrictRelationalPositiveAffineApplicableDomainAssignmentCount
    (ValidatedLengthStrictRelationalPositiveAffineApplicableDomainReceipt
      _ inputBox) = validatedLengthInputBoxAssignmentCount inputBox

validatedLengthStrictRelationalPositiveAffineApplicableDomainApplicableAssignmentCount
  :: ValidatedLengthStrictRelationalPositiveAffineApplicableDomain
  -> Natural
validatedLengthStrictRelationalPositiveAffineApplicableDomainApplicableAssignmentCount
    (ValidatedLengthStrictRelationalPositiveAffineApplicableDomainReceipt
      _ inputBox) = validatedLengthInputBoxApplicableAssignmentCount inputBox

validatedLengthStrictRelationalPositiveAffineApplicableDomainBasis
  :: ValidatedLengthStrictRelationalPositiveAffineApplicableDomain
  -> LengthCounterexampleBasis
validatedLengthStrictRelationalPositiveAffineApplicableDomainBasis
    (ValidatedLengthStrictRelationalPositiveAffineApplicableDomainReceipt
      _ inputBox) = validatedLengthInputBoxBasis inputBox

-- | Nominal binary-product sibling of
-- 'lengthStrictRelationalPositiveAffineApplicableDomainValidationSchemaTag'.
lengthSpinePairStrictRelationalPositiveAffineApplicableDomainValidationSchemaTag
  :: [Word8]
lengthSpinePairStrictRelationalPositiveAffineApplicableDomainValidationSchemaTag =
  ascii
    "finite-binary-product-spine-lengths/strict-relational-positive-affine-precondition-domain-establishment/v1"

-- | Complete product applicable-domain validation under the strict relational
-- positive-affine rule.  Scalar and product evidence remain nominally
-- disjoint while sharing only the private extraction mechanics.
data ValidatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomain =
  ValidatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainReceipt
    ![Word8]
    !ValidatedLengthSpinePairInputBox
  deriving (Eq, Ord, Show)

instance NFData
    ValidatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomain where
  rnf
      (ValidatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainReceipt
        schema inputBox) = rnf schema `seq` rnf inputBox

validatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainInclusiveMaximums
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomain
  -> [Natural]
validatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainInclusiveMaximums
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainReceipt
      _ inputBox) =
        validatedLengthSpinePairInputBoxInclusiveMaximums inputBox

validatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainAssignmentCount
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomain
  -> Natural
validatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainAssignmentCount
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainReceipt
      _ inputBox) = validatedLengthSpinePairInputBoxAssignmentCount inputBox

validatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainApplicableAssignmentCount
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomain
  -> Natural
validatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainApplicableAssignmentCount
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainReceipt
      _ inputBox) =
        validatedLengthSpinePairInputBoxApplicableAssignmentCount inputBox

validatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainBasis
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomain
  -> LengthCounterexampleBasis
validatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainBasis
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainReceipt
      _ inputBox) = validatedLengthSpinePairInputBoxBasis inputBox

-- | Versioned successor to strict relational positive-affine coverage.  It
-- adds only exact natural-number consequences for one positive-literal
-- quotient occurring at a directed relation's operand root.
lengthStrictRelationalPositiveAffineQuotientApplicableDomainValidationSchemaTag
  :: [Word8]
lengthStrictRelationalPositiveAffineQuotientApplicableDomainValidationSchemaTag =
  ascii
    "finite-list-spine-length/strict-relational-positive-affine-quotient-precondition-domain-establishment/v1"

-- | Complete scalar applicable-domain validation under the root-quotient
-- successor rule.  The constructor remains private and the nested box receipt
-- retains the exact traversal evidence.
data ValidatedLengthStrictRelationalPositiveAffineQuotientApplicableDomain =
  ValidatedLengthStrictRelationalPositiveAffineQuotientApplicableDomainReceipt
    ![Word8]
    !ValidatedLengthInputBox
  deriving (Eq, Ord, Show)

instance NFData
    ValidatedLengthStrictRelationalPositiveAffineQuotientApplicableDomain where
  rnf
      (ValidatedLengthStrictRelationalPositiveAffineQuotientApplicableDomainReceipt
        schema inputBox) = rnf schema `seq` rnf inputBox

validatedLengthStrictRelationalPositiveAffineQuotientApplicableDomainInclusiveMaximums
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientApplicableDomain
  -> [Natural]
validatedLengthStrictRelationalPositiveAffineQuotientApplicableDomainInclusiveMaximums
    (ValidatedLengthStrictRelationalPositiveAffineQuotientApplicableDomainReceipt
      _ inputBox) = validatedLengthInputBoxInclusiveMaximums inputBox

validatedLengthStrictRelationalPositiveAffineQuotientApplicableDomainAssignmentCount
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientApplicableDomain
  -> Natural
validatedLengthStrictRelationalPositiveAffineQuotientApplicableDomainAssignmentCount
    (ValidatedLengthStrictRelationalPositiveAffineQuotientApplicableDomainReceipt
      _ inputBox) = validatedLengthInputBoxAssignmentCount inputBox

validatedLengthStrictRelationalPositiveAffineQuotientApplicableDomainApplicableAssignmentCount
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientApplicableDomain
  -> Natural
validatedLengthStrictRelationalPositiveAffineQuotientApplicableDomainApplicableAssignmentCount
    (ValidatedLengthStrictRelationalPositiveAffineQuotientApplicableDomainReceipt
      _ inputBox) = validatedLengthInputBoxApplicableAssignmentCount inputBox

validatedLengthStrictRelationalPositiveAffineQuotientApplicableDomainBasis
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientApplicableDomain
  -> LengthCounterexampleBasis
validatedLengthStrictRelationalPositiveAffineQuotientApplicableDomainBasis
    (ValidatedLengthStrictRelationalPositiveAffineQuotientApplicableDomainReceipt
      _ inputBox) = validatedLengthInputBoxBasis inputBox

-- | Nominal binary-product sibling of
-- 'lengthStrictRelationalPositiveAffineQuotientApplicableDomainValidationSchemaTag'.
lengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainValidationSchemaTag
  :: [Word8]
lengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainValidationSchemaTag =
  ascii
    "finite-binary-product-spine-lengths/strict-relational-positive-affine-quotient-precondition-domain-establishment/v1"

-- | Product-domain root-quotient receipt.  Its nominal type prevents scalar
-- and product establishment evidence from being interchanged.
data ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomain =
  ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainReceipt
    ![Word8]
    !ValidatedLengthSpinePairInputBox
  deriving (Eq, Ord, Show)

instance NFData
    ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomain where
  rnf
      (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainReceipt
        schema inputBox) = rnf schema `seq` rnf inputBox

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainInclusiveMaximums
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomain
  -> [Natural]
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainInclusiveMaximums
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainReceipt
      _ inputBox) =
        validatedLengthSpinePairInputBoxInclusiveMaximums inputBox

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainAssignmentCount
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomain
  -> Natural
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainAssignmentCount
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainReceipt
      _ inputBox) = validatedLengthSpinePairInputBoxAssignmentCount inputBox

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainApplicableAssignmentCount
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomain
  -> Natural
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainApplicableAssignmentCount
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainReceipt
      _ inputBox) =
        validatedLengthSpinePairInputBoxApplicableAssignmentCount inputBox

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainBasis
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomain
  -> LengthCounterexampleBasis
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainBasis
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainReceipt
      _ inputBox) = validatedLengthSpinePairInputBoxBasis inputBox

-- | Versioned cumulative successor to root-quotient strict relational
-- positive-affine coverage.  It adds only conjunctive consequences for one
-- immediate binary minimum or maximum at a relation operand's root.
lengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainValidationSchemaTag
  :: [Word8]
lengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainValidationSchemaTag =
  ascii
    "finite-list-spine-length/strict-relational-positive-affine-quotient-root-extrema-precondition-domain-establishment/v1"

-- | Complete scalar applicable-domain validation under the cumulative
-- root-extrema successor.  Its constructor is private; the nested input-box
-- receipt retains the exhaustive traversal evidence.
data ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain =
  ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainReceipt
    ![Word8]
    !ValidatedLengthInputBox
  deriving (Eq, Ord, Show)

instance NFData
    ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain where
  rnf
      (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainReceipt
        schema inputBox) = rnf schema `seq` rnf inputBox

validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainInclusiveMaximums
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain
  -> [Natural]
validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainInclusiveMaximums
    (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainReceipt
      _ inputBox) = validatedLengthInputBoxInclusiveMaximums inputBox

validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainAssignmentCount
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain
  -> Natural
validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainAssignmentCount
    (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainReceipt
      _ inputBox) = validatedLengthInputBoxAssignmentCount inputBox

validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainApplicableAssignmentCount
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain
  -> Natural
validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainApplicableAssignmentCount
    (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainReceipt
      _ inputBox) = validatedLengthInputBoxApplicableAssignmentCount inputBox

validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainBasis
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain
  -> LengthCounterexampleBasis
validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainBasis
    (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainReceipt
      _ inputBox) = validatedLengthInputBoxBasis inputBox

-- | Nominal binary-product sibling of
-- 'lengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainValidationSchemaTag'.
lengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainValidationSchemaTag
  :: [Word8]
lengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainValidationSchemaTag =
  ascii
    "finite-binary-product-spine-lengths/strict-relational-positive-affine-quotient-root-extrema-precondition-domain-establishment/v1"

-- | Product-domain cumulative root-extrema receipt.  Its nominal type keeps
-- scalar and product establishment evidence disjoint.
data ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain =
  ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainReceipt
    ![Word8]
    !ValidatedLengthSpinePairInputBox
  deriving (Eq, Ord, Show)

instance NFData
    ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain where
  rnf
      (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainReceipt
        schema inputBox) = rnf schema `seq` rnf inputBox

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainInclusiveMaximums
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain
  -> [Natural]
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainInclusiveMaximums
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainReceipt
      _ inputBox) =
        validatedLengthSpinePairInputBoxInclusiveMaximums inputBox

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainAssignmentCount
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain
  -> Natural
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainAssignmentCount
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainReceipt
      _ inputBox) = validatedLengthSpinePairInputBoxAssignmentCount inputBox

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainApplicableAssignmentCount
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain
  -> Natural
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainApplicableAssignmentCount
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainReceipt
      _ inputBox) =
        validatedLengthSpinePairInputBoxApplicableAssignmentCount inputBox

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainBasis
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain
  -> LengthCounterexampleBasis
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainBasis
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainReceipt
      _ inputBox) = validatedLengthSpinePairInputBoxBasis inputBox

-- | Versioned cumulative successor which adds exact conjunctive consequences
-- for one immediate natural monus at a relation operand's root.
lengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainValidationSchemaTag
  :: [Word8]
lengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainValidationSchemaTag =
  ascii
    "finite-list-spine-length/strict-relational-positive-affine-quotient-root-extrema-monus-precondition-domain-establishment/v1"

-- | Complete scalar applicable-domain validation under the cumulative monus
-- successor.  Its constructor remains private and its nested input-box
-- receipt retains the exhaustive traversal evidence.
data ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain =
  ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainReceipt
    ![Word8]
    !ValidatedLengthInputBox
  deriving (Eq, Ord, Show)

instance NFData
    ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain where
  rnf
      (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainReceipt
        schema inputBox) = rnf schema `seq` rnf inputBox

validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainInclusiveMaximums
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain
  -> [Natural]
validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainInclusiveMaximums
    (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainReceipt
      _ inputBox) = validatedLengthInputBoxInclusiveMaximums inputBox

validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainAssignmentCount
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain
  -> Natural
validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainAssignmentCount
    (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainReceipt
      _ inputBox) = validatedLengthInputBoxAssignmentCount inputBox

validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainApplicableAssignmentCount
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain
  -> Natural
validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainApplicableAssignmentCount
    (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainReceipt
      _ inputBox) = validatedLengthInputBoxApplicableAssignmentCount inputBox

validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainBasis
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain
  -> LengthCounterexampleBasis
validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainBasis
    (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainReceipt
      _ inputBox) = validatedLengthInputBoxBasis inputBox

-- | Nominal binary-product sibling of
-- 'lengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainValidationSchemaTag'.
lengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainValidationSchemaTag
  :: [Word8]
lengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainValidationSchemaTag =
  ascii
    "finite-binary-product-spine-lengths/strict-relational-positive-affine-quotient-root-extrema-monus-precondition-domain-establishment/v1"

-- | Product-domain cumulative monus receipt.  Its nominal type keeps scalar
-- and product establishment evidence disjoint.
data ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain =
  ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainReceipt
    ![Word8]
    !ValidatedLengthSpinePairInputBox
  deriving (Eq, Ord, Show)

instance NFData
    ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain where
  rnf
      (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainReceipt
        schema inputBox) = rnf schema `seq` rnf inputBox

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainInclusiveMaximums
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain
  -> [Natural]
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainInclusiveMaximums
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainReceipt
      _ inputBox) =
        validatedLengthSpinePairInputBoxInclusiveMaximums inputBox

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainAssignmentCount
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain
  -> Natural
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainAssignmentCount
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainReceipt
      _ inputBox) = validatedLengthSpinePairInputBoxAssignmentCount inputBox

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainApplicableAssignmentCount
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain
  -> Natural
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainApplicableAssignmentCount
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainReceipt
      _ inputBox) =
        validatedLengthSpinePairInputBoxApplicableAssignmentCount inputBox

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainBasis
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain
  -> LengthCounterexampleBasis
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainBasis
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainReceipt
      _ inputBox) = validatedLengthSpinePairInputBoxBasis inputBox

-- | Fixed-precedence operational failures for scalar Boolean finite-union
-- establishment.  Branch indices refer to canonical post-deduplication,
-- post-subsumption DNF order; box indices refer to the canonical retained
-- componentwise-maximal antichain.
data LengthBooleanFiniteUnionApplicableDomainValidationError
  = LengthBooleanFiniteUnionProblemInputLimitExceeded !Int !Int
  | LengthBooleanFiniteUnionGeneratedBranchLimitExceeded !Int !Int
  | LengthBooleanFiniteUnionRuleLimitExceeded !Int !Int !Int
  | LengthBooleanFiniteUnionClosureInspectionLimitExceeded !Int !Int !Int
  | LengthBooleanFiniteUnionRetainedBoxLimitExceeded !Int !Int
  | LengthBooleanFiniteUnionMaximumValueRejected
      !Int !Int !LengthEvaluationError
  | LengthBooleanFiniteUnionAssignmentVisitLimitExceeded !Int !Int
  | LengthBooleanFiniteUnionAssignmentLimitExceeded !Natural !Natural
  | LengthBooleanFiniteUnionAssignmentEvaluationRejected
      !Natural !LengthEvaluationError
  | LengthBooleanFiniteUnionInternalEnumerationInvariant
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthBooleanFiniteUnionApplicableDomainValidationError

-- | Nominal binary-product failure vocabulary for the same bounded Boolean
-- finite-union algorithm.
data LengthSpinePairBooleanFiniteUnionApplicableDomainValidationError
  = LengthSpinePairBooleanFiniteUnionProblemInputLimitExceeded !Int !Int
  | LengthSpinePairBooleanFiniteUnionGeneratedBranchLimitExceeded !Int !Int
  | LengthSpinePairBooleanFiniteUnionRuleLimitExceeded !Int !Int !Int
  | LengthSpinePairBooleanFiniteUnionClosureInspectionLimitExceeded
      !Int !Int !Int
  | LengthSpinePairBooleanFiniteUnionRetainedBoxLimitExceeded !Int !Int
  | LengthSpinePairBooleanFiniteUnionMaximumValueRejected
      !Int !Int !LengthSpinePairEvaluationError
  | LengthSpinePairBooleanFiniteUnionAssignmentVisitLimitExceeded !Int !Int
  | LengthSpinePairBooleanFiniteUnionAssignmentLimitExceeded
      !Natural !Natural
  | LengthSpinePairBooleanFiniteUnionAssignmentEvaluationRejected
      !Natural !LengthSpinePairEvaluationError
  | LengthSpinePairBooleanFiniteUnionInternalEnumerationInvariant
  deriving (Eq, Ord, Show, Generic)

instance NFData
    LengthSpinePairBooleanFiniteUnionApplicableDomainValidationError

-- | Versioned scalar authority for the exact bounded Boolean DNF finite-union
-- successor.  This tag changes no checked problem, query, protocol, or live
-- execution identity.
lengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainValidationSchemaTag
  :: [Word8]
lengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainValidationSchemaTag =
  ascii
    "finite-list-spine-length/strict-relational-positive-affine-quotient-root-extrema-monus-boolean-dnf-finite-union-precondition-domain-establishment/v1"

-- | Opaque scalar receipt for one canonical finite union.  Incomparable boxes
-- remain separate; no componentwise hull is introduced.  Assignment visits
-- count every retained-box traversal, while assignment count is the exact
-- cardinality of their deduplicated union.
data ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain =
  ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainReceipt
    ![Word8]
    ![[Natural]]
    !Natural
    !Natural
    !Natural
    !LengthCounterexampleBasis
  deriving (Eq, Ord, Show)

instance NFData
    ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain where
  rnf
      (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainReceipt
        schema boxes visits assignments applicable basis) =
    rnf schema `seq` rnf boxes `seq` rnf visits `seq` rnf assignments `seq`
    rnf applicable `seq` rnf basis

validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainInclusiveMaximumBoxes
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
  -> [[Natural]]
validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainInclusiveMaximumBoxes
    (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainReceipt
      _ boxes _ _ _ _) = boxes

validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainBoxCount
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
  -> Natural
validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainBoxCount
    (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainReceipt
      _ boxes _ _ _ _) = fromIntegral $ length boxes

validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainAssignmentVisitCount
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
  -> Natural
validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainAssignmentVisitCount
    (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainReceipt
      _ _ visits _ _ _) = visits

validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainAssignmentCount
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
  -> Natural
validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainAssignmentCount
    (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainReceipt
      _ _ _ assignments _ _) = assignments

validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainApplicableAssignmentCount
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
  -> Natural
validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainApplicableAssignmentCount
    (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainReceipt
      _ _ _ _ applicable _) = applicable

validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainBasis
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
  -> LengthCounterexampleBasis
validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainBasis
    (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainReceipt
      _ _ _ _ _ basis) = basis

-- | Nominal binary-product tag for the same bounded Boolean DNF algorithm.
lengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainValidationSchemaTag
  :: [Word8]
lengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainValidationSchemaTag =
  ascii
    "finite-binary-product-spine-lengths/strict-relational-positive-affine-quotient-root-extrema-monus-boolean-dnf-finite-union-precondition-domain-establishment/v1"

-- | Opaque product-domain sibling of the scalar finite-union receipt.
data ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain =
  ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainReceipt
    ![Word8]
    ![[Natural]]
    !Natural
    !Natural
    !Natural
    !LengthCounterexampleBasis
  deriving (Eq, Ord, Show)

instance NFData
    ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain where
  rnf
      (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainReceipt
        schema boxes visits assignments applicable basis) =
    rnf schema `seq` rnf boxes `seq` rnf visits `seq` rnf assignments `seq`
    rnf applicable `seq` rnf basis

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainInclusiveMaximumBoxes
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
  -> [[Natural]]
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainInclusiveMaximumBoxes
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainReceipt
      _ boxes _ _ _ _) = boxes

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainBoxCount
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
  -> Natural
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainBoxCount
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainReceipt
      _ boxes _ _ _ _) = fromIntegral $ length boxes

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainAssignmentVisitCount
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
  -> Natural
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainAssignmentVisitCount
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainReceipt
      _ _ visits _ _ _) = visits

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainAssignmentCount
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
  -> Natural
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainAssignmentCount
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainReceipt
      _ _ _ assignments _ _) = assignments

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainApplicableAssignmentCount
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
  -> Natural
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainApplicableAssignmentCount
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainReceipt
      _ _ _ _ applicable _) = applicable

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainBasis
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
  -> LengthCounterexampleBasis
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainBasis
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainReceipt
      _ _ _ _ _ basis) = basis

-- | Versioned scalar authority for the cumulative finite-union validator
-- which additionally opens the exact disjunctions of the admitted immediate
-- root extrema and may-zero root monus atoms.  The predecessor receipt remains
-- nominally and byte-for-byte distinct.
lengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainValidationSchemaTag
  :: [Word8]
lengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainValidationSchemaTag =
  ascii
    "finite-list-spine-length/strict-relational-positive-affine-quotient-root-extrema-monus-boolean-dnf-finite-union-root-extrema-may-zero-monus-atomic-branching-precondition-domain-establishment/v1"

-- | Opaque scalar receipt for the atomic-branching finite union.  Its fresh
-- six-field payload embeds the new schema tag directly rather than wrapping
-- or reusing predecessor evidence.
data ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain =
  ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainReceipt
    ![Word8]
    ![[Natural]]
    !Natural
    !Natural
    !Natural
    !LengthCounterexampleBasis
  deriving (Eq, Ord, Show)

instance NFData
    ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain where
  rnf
      (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainReceipt
        schema boxes visits assignments applicable basis) =
    rnf schema `seq` rnf boxes `seq` rnf visits `seq` rnf assignments `seq`
    rnf applicable `seq` rnf basis

validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainInclusiveMaximumBoxes
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
  -> [[Natural]]
validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainInclusiveMaximumBoxes
    (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainReceipt
      _ boxes _ _ _ _) = boxes

validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainBoxCount
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
  -> Natural
validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainBoxCount
    (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainReceipt
      _ boxes _ _ _ _) = fromIntegral $ length boxes

validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainAssignmentVisitCount
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
  -> Natural
validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainAssignmentVisitCount
    (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainReceipt
      _ _ visits _ _ _) = visits

validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainAssignmentCount
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
  -> Natural
validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainAssignmentCount
    (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainReceipt
      _ _ _ assignments _ _) = assignments

validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainApplicableAssignmentCount
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
  -> Natural
validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainApplicableAssignmentCount
    (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainReceipt
      _ _ _ _ applicable _) = applicable

validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainBasis
  :: ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
  -> LengthCounterexampleBasis
validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainBasis
    (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainReceipt
      _ _ _ _ _ basis) = basis

-- | Nominal product-domain tag for the same atomic-branching algorithm.
lengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainValidationSchemaTag
  :: [Word8]
lengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainValidationSchemaTag =
  ascii
    "finite-binary-product-spine-lengths/strict-relational-positive-affine-quotient-root-extrema-monus-boolean-dnf-finite-union-root-extrema-may-zero-monus-atomic-branching-precondition-domain-establishment/v1"

data ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain =
  ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainReceipt
    ![Word8]
    ![[Natural]]
    !Natural
    !Natural
    !Natural
    !LengthCounterexampleBasis
  deriving (Eq, Ord, Show)

instance NFData
    ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain where
  rnf
      (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainReceipt
        schema boxes visits assignments applicable basis) =
    rnf schema `seq` rnf boxes `seq` rnf visits `seq` rnf assignments `seq`
    rnf applicable `seq` rnf basis

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainInclusiveMaximumBoxes
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
  -> [[Natural]]
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainInclusiveMaximumBoxes
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainReceipt
      _ boxes _ _ _ _) = boxes

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainBoxCount
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
  -> Natural
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainBoxCount
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainReceipt
      _ boxes _ _ _ _) = fromIntegral $ length boxes

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainAssignmentVisitCount
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
  -> Natural
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainAssignmentVisitCount
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainReceipt
      _ _ visits _ _ _) = visits

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainAssignmentCount
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
  -> Natural
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainAssignmentCount
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainReceipt
      _ _ _ assignments _ _) = assignments

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainApplicableAssignmentCount
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
  -> Natural
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainApplicableAssignmentCount
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainReceipt
      _ _ _ _ applicable _) = applicable

validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainBasis
  :: ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
  -> LengthCounterexampleBasis
validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainBasis
    (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainReceipt
      _ _ _ _ _ basis) = basis

-- | Fail-closed scalar simplification failure after the caller supplied one
-- opaque counterexample anchor.  Width and Cartesian-product admission misses
-- are deliberately absent: those are conservative @Right Nothing@ results.
-- Arity or maximum-value rejection instead means that an allegedly validated
-- anchor is stale under the exact problem or current replay limits.  Search
-- failures retain the zero-based lexicographic assignment ordinal through the
-- nested input-box error.
data LengthCounterexampleSimplificationError
  = LengthCounterexampleSimplificationInputBoxValidationRejected
      !LengthInputBoxValidationError
  | LengthCounterexampleSimplificationAnchorEvaluationRejected
      !LengthEvaluationError
  | LengthCounterexampleSimplificationAnchorNotCounterexample
  | LengthCounterexampleSimplificationInternalInvariant
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthCounterexampleSimplificationError

-- | Versioned metadata semantics for one strict scalar counterexample
-- improvement.  This is a receipt-only tag: it changes no checked problem,
-- SMT query, process protocol, or live-execution identity.
lengthCounterexampleSimplificationSchemaTag :: [Word8]
lengthCounterexampleSimplificationSchemaTag = ascii
  "finite-list-spine-length/bounded-counterexample-simplification/v1"

-- | Opaque provenance for a strict deterministic simplification.
--
-- The nested counterexample is ordinary freshly replayed evidence.  The
-- wrapper adds only the original input vector, exact number of lexicographic
-- search assignments inspected through the returned hit, and verifier schema.
-- It is not positive evidence, proof of minimality outside the admitted box,
-- or authority derived from a solver report.
data ValidatedLengthCounterexampleSimplification =
  ValidatedLengthCounterexampleSimplificationReceipt
    ![Word8]
    ![Natural]
    !Natural
    !ValidatedLengthCounterexample
  deriving (Eq, Ord, Show)

instance NFData ValidatedLengthCounterexampleSimplification where
  rnf (ValidatedLengthCounterexampleSimplificationReceipt schema original
      inspected counterexample) =
    rnf schema `seq` rnf original `seq` rnf inspected `seq`
    rnf counterexample

-- | Source-ordered inputs of the independently revalidated anchor.
validatedLengthCounterexampleSimplificationOriginalInputs
  :: ValidatedLengthCounterexampleSimplification
  -> [Natural]
validatedLengthCounterexampleSimplificationOriginalInputs
    (ValidatedLengthCounterexampleSimplificationReceipt _ original _ _) =
  original

-- | Search assignments inspected through and including the returned hit.
-- The separate anchor replay is not counted.
validatedLengthCounterexampleSimplificationInspectedAssignmentCount
  :: ValidatedLengthCounterexampleSimplification
  -> Natural
validatedLengthCounterexampleSimplificationInspectedAssignmentCount
    (ValidatedLengthCounterexampleSimplificationReceipt _ _ inspected _) =
  inspected

-- | Fresh ordinary counterexample found by exact-problem bounded replay.
validatedLengthCounterexampleSimplificationCounterexample
  :: ValidatedLengthCounterexampleSimplification
  -> ValidatedLengthCounterexample
validatedLengthCounterexampleSimplificationCounterexample
    (ValidatedLengthCounterexampleSimplificationReceipt _ _ _ value) =
  value

-- | Source-ordered inputs of the simplified ordinary counterexample.
validatedLengthCounterexampleSimplificationInputs
  :: ValidatedLengthCounterexampleSimplification
  -> [Natural]
validatedLengthCounterexampleSimplificationInputs =
  validatedLengthCounterexampleInputs .
    validatedLengthCounterexampleSimplificationCounterexample

-- | Candidate result recomputed for the simplified inputs.
validatedLengthCounterexampleSimplificationResult
  :: ValidatedLengthCounterexampleSimplification
  -> Natural
validatedLengthCounterexampleSimplificationResult =
  validatedLengthCounterexampleResult .
    validatedLengthCounterexampleSimplificationCounterexample

-- | Provider-independent or assumed-provider-relative basis of the fresh
-- ordinary counterexample.
validatedLengthCounterexampleSimplificationBasis
  :: ValidatedLengthCounterexampleSimplification
  -> LengthCounterexampleBasis
validatedLengthCounterexampleSimplificationBasis =
  validatedLengthCounterexampleBasis .
    validatedLengthCounterexampleSimplificationCounterexample

-- | Always 'True': an opaque receipt is constructed only for a strict input
-- vector change.  @Right Nothing@ represents both admission unavailability
-- and an admitted search whose first counterexample is the anchor itself.
validatedLengthCounterexampleSimplificationChanged
  :: ValidatedLengthCounterexampleSimplification
  -> Bool
validatedLengthCounterexampleSimplificationChanged receipt =
  validatedLengthCounterexampleSimplificationOriginalInputs receipt /=
    validatedLengthCounterexampleSimplificationInputs receipt

-- | Nominal binary-product sibling of
-- 'LengthCounterexampleSimplificationError'.
data LengthSpinePairCounterexampleSimplificationError
  = LengthSpinePairCounterexampleSimplificationInputBoxValidationRejected
      !LengthSpinePairInputBoxValidationError
  | LengthSpinePairCounterexampleSimplificationAnchorEvaluationRejected
      !LengthSpinePairEvaluationError
  | LengthSpinePairCounterexampleSimplificationAnchorNotCounterexample
  | LengthSpinePairCounterexampleSimplificationInternalInvariant
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSpinePairCounterexampleSimplificationError

-- | Versioned metadata semantics for strict product counterexample
-- simplification, nominally distinct from the scalar tag.
lengthSpinePairCounterexampleSimplificationSchemaTag :: [Word8]
lengthSpinePairCounterexampleSimplificationSchemaTag = ascii
  "finite-binary-product-spine-lengths/bounded-counterexample-simplification/v1"

-- | Opaque provenance for a strict product-domain simplification.  Both
-- result components remain owned by the nested freshly replayed ordinary
-- product counterexample.
data ValidatedLengthSpinePairCounterexampleSimplification =
  ValidatedLengthSpinePairCounterexampleSimplificationReceipt
    ![Word8]
    ![Natural]
    !Natural
    !ValidatedLengthSpinePairCounterexample
  deriving (Eq, Ord, Show)

instance NFData ValidatedLengthSpinePairCounterexampleSimplification where
  rnf (ValidatedLengthSpinePairCounterexampleSimplificationReceipt schema
      original inspected counterexample) =
    rnf schema `seq` rnf original `seq` rnf inspected `seq`
    rnf counterexample

validatedLengthSpinePairCounterexampleSimplificationOriginalInputs
  :: ValidatedLengthSpinePairCounterexampleSimplification
  -> [Natural]
validatedLengthSpinePairCounterexampleSimplificationOriginalInputs
    (ValidatedLengthSpinePairCounterexampleSimplificationReceipt _ original
      _ _) = original

validatedLengthSpinePairCounterexampleSimplificationInspectedAssignmentCount
  :: ValidatedLengthSpinePairCounterexampleSimplification
  -> Natural
validatedLengthSpinePairCounterexampleSimplificationInspectedAssignmentCount
    (ValidatedLengthSpinePairCounterexampleSimplificationReceipt _ _
      inspected _) = inspected

validatedLengthSpinePairCounterexampleSimplificationCounterexample
  :: ValidatedLengthSpinePairCounterexampleSimplification
  -> ValidatedLengthSpinePairCounterexample
validatedLengthSpinePairCounterexampleSimplificationCounterexample
    (ValidatedLengthSpinePairCounterexampleSimplificationReceipt _ _ _
      value) = value

validatedLengthSpinePairCounterexampleSimplificationInputs
  :: ValidatedLengthSpinePairCounterexampleSimplification
  -> [Natural]
validatedLengthSpinePairCounterexampleSimplificationInputs =
  validatedLengthSpinePairCounterexampleInputs .
    validatedLengthSpinePairCounterexampleSimplificationCounterexample

validatedLengthSpinePairCounterexampleSimplificationResult
  :: ValidatedLengthSpinePairCounterexampleSimplification
  -> LengthSpinePair Natural
validatedLengthSpinePairCounterexampleSimplificationResult =
  validatedLengthSpinePairCounterexampleResult .
    validatedLengthSpinePairCounterexampleSimplificationCounterexample

validatedLengthSpinePairCounterexampleSimplificationBasis
  :: ValidatedLengthSpinePairCounterexampleSimplification
  -> LengthCounterexampleBasis
validatedLengthSpinePairCounterexampleSimplificationBasis =
  validatedLengthSpinePairCounterexampleBasis .
    validatedLengthSpinePairCounterexampleSimplificationCounterexample

validatedLengthSpinePairCounterexampleSimplificationChanged
  :: ValidatedLengthSpinePairCounterexampleSimplification
  -> Bool
validatedLengthSpinePairCounterexampleSimplificationChanged receipt =
  validatedLengthSpinePairCounterexampleSimplificationOriginalInputs receipt
    /= validatedLengthSpinePairCounterexampleSimplificationInputs receipt

-- | Classify one concrete contract assignment.  Arity is checked before any
-- value, inputs are bounded left-to-right before the result, and a false
-- precondition does not evaluate the postcondition.
evaluateLengthContractAssignment
  :: LengthEvaluationLimits
  -> CheckedLengthContract variable
  -> LengthContractAssignment
  -> Either LengthEvaluationError LengthContractEvaluation
evaluateLengthContractAssignment limits contract assignment = do
  inputs <- exactAssignment
    LengthContractAssignmentArityMismatch
    (checkedLengthContractInputCount contract)
    $ lengthContractAssignmentInputs assignment
  mapM_ (uncurry $ checkAssignedValue limits . LengthContractInputValue)
    $ zip [0 ..] inputs
  checkAssignedValue limits LengthContractResultValue
    $ lengthContractAssignmentResult assignment
  let lookupVariable variable = case variable of
        LengthInput position -> case indexNatural position inputs of
          Just value -> Right value
          Nothing -> Left $ LengthEvaluationInternalContractReference variable
        LengthResult -> Right $ lengthContractAssignmentResult assignment
  precondition <- evaluateFormula limits lookupVariable
    $ checkedLengthContractPrecondition contract
  if not precondition
    then Right LengthPreconditionNotMet
    else do
      postcondition <- evaluateFormula limits lookupVariable
        $ checkedLengthContractPostcondition contract
      pure $ if postcondition
        then LengthPostconditionSatisfied
        else LengthPostconditionViolated

-- | Classify one detached binary product assignment.  Arity precedes all
-- values; inputs are bounded left-to-right, then the first and second result,
-- before precondition and postcondition evaluation.
evaluateLengthSpinePairContractAssignment
  :: LengthEvaluationLimits
  -> CheckedLengthSpinePairContract variable
  -> LengthSpinePairContractAssignment
  -> Either LengthSpinePairEvaluationError LengthContractEvaluation
evaluateLengthSpinePairContractAssignment limits contract assignment = do
  inputs <- exactSpinePairAssignment
    LengthSpinePairContractAssignmentArityMismatch
    (checkedLengthSpinePairContractInputCount contract)
    $ lengthSpinePairContractAssignmentInputs assignment
  mapM_ (uncurry $ checkSpinePairAssignedValue limits .
      LengthSpinePairContractInputValue)
    $ zip [0 ..] inputs
  let result = lengthSpinePairContractAssignmentResult assignment
  checkSpinePairAssignedValue limits
    (LengthSpinePairContractResultValue LengthSpinePairFirst)
    $ lengthSpinePairFirst result
  checkSpinePairAssignedValue limits
    (LengthSpinePairContractResultValue LengthSpinePairSecond)
    $ lengthSpinePairSecond result
  let lookupVariable variable = case variable of
        LengthSpinePairInput position -> case indexNatural position inputs of
          Just value -> Right value
          Nothing -> Left
            $ LengthSpinePairEvaluationInternalContractReference variable
        LengthSpinePairResult component -> Right $ case component of
          LengthSpinePairFirst -> lengthSpinePairFirst result
          LengthSpinePairSecond -> lengthSpinePairSecond result
  precondition <- evaluateSpinePairFormula limits lookupVariable
    $ checkedLengthSpinePairContractPrecondition contract
  if not precondition
    then Right LengthPreconditionNotMet
    else do
      postcondition <- evaluateSpinePairFormula limits lookupVariable
        $ checkedLengthSpinePairContractPostcondition contract
      pure $ if postcondition
        then LengthPostconditionSatisfied
        else LengthPostconditionViolated

-- | Evaluate one exact context-free provider application under its checked
-- assumed law.  The result remains conditional on that explicit assumption
-- and carries no behavioral-evidence authority.  A retained
-- constraint-conditional summary fails before assignment arity, roles, or
-- values are inspected.  This evaluator cannot discharge its context even
-- though an exact associated candidate occurrence may have done so while its
-- complete Length problem was sealed.
evaluateLengthProviderApplication
  :: LengthEvaluationLimits
  -> CheckedLengthProviderSummary variable
  -> [LengthProviderArgumentValue]
  -> Either LengthEvaluationError Natural
evaluateLengthProviderApplication limits summary rawArguments = do
  case checkedLengthProviderTrust summary of
    AssumedProviderLaw -> pure ()
    AssumedProviderLawConditionalOnConstraintDischarge ->
      Left LengthEvaluationConditionalProviderRequiresDischarge
  let roles = checkedLengthProviderArgumentRoles summary
  arguments <- exactAssignment LengthProviderAssignmentArityMismatch
    (length roles) rawArguments
  observed <- mapM validateArgument $ zip3 [0 ..] roles arguments
  evaluateExpression limits (lookupObserved observed)
    $ checkedLengthProviderTransfer summary
 where
  validateArgument (index, role, argument) = case (role, argument) of
    (LengthSpineArgument, ObservedSpineLength value) -> do
      checkAssignedValue limits (LengthProviderSpineValue index) value
      Right $ Just value
    (LengthUnobservedArgument, UnobservedLengthArgument) -> Right Nothing
    _ -> Left $ LengthProviderArgumentRoleMismatch index role argument

  lookupObserved observed variable@(LengthProviderArgument position) =
    case indexNatural position observed of
      Just (Just value) -> Right value
      _ -> Left $ LengthEvaluationInternalProviderReference variable

-- | Independently validate decoded inputs against one exact candidate
-- problem.  The checked precondition is evaluated first.  A false
-- precondition is an ordinary non-counterexample and does not force the
-- candidate result.  Otherwise one shared lazy result computation is bound
-- while evaluating the checked postcondition.  A result-independent true
-- postcondition does not force it; a false postcondition forces it before
-- constructing a problem-bound evidence receipt.
--
-- In particular, this function does not consume a raw solver observation and
-- cannot strengthen @unsat@ or @unknown@.  A satisfiable model remains a hint
-- until its decoded natural inputs pass this replay boundary.
validateLengthProblemCounterexample
  :: LengthEvaluationLimits
  -> CheckedLengthProblem identity local
  -> LengthProblemAssignment
  -> Either LengthEvaluationError
      (Maybe
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthCounterexample))
validateLengthProblemCounterexample limits problem assignment = do
  replay <- replayLengthProblemAssignment limits problem assignment
  pure $ case replay of
    LengthProblemPreconditionNotMet -> Nothing
    LengthProblemPostconditionSatisfied -> Nothing
    LengthProblemPostconditionViolated receipt -> Just
      $ mkBehavioralEvidence
          (checkedLengthProblemBehavioralProblem problem) receipt

-- | Independently replay source-ordered inputs against one exact checked
-- binary product problem.  Product evidence is nominally disjoint from scalar
-- Length evidence even though both derive authority from the same session.
validateLengthSpinePairProblemCounterexample
  :: LengthEvaluationLimits
  -> CheckedLengthSpinePairProblem identity local
  -> LengthProblemAssignment
  -> Either LengthSpinePairEvaluationError
      (Maybe
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairCounterexample))
validateLengthSpinePairProblemCounterexample limits problem assignment = do
  replay <- replayLengthSpinePairProblemAssignment limits problem assignment
  pure $ case replay of
    LengthSpinePairProblemPreconditionNotMet -> Nothing
    LengthSpinePairProblemPostconditionSatisfied -> Nothing
    LengthSpinePairProblemPostconditionViolated receipt -> Just
      $ mkBehavioralEvidence
          (checkedLengthSpinePairProblemBehavioralProblem problem) receipt

-- | Deterministically seek a strictly smaller scalar counterexample inside
-- the anchor's componentwise dominated box.
--
-- Admission reuses 'LengthInputBoxLimits'.  Target width is considered before
-- the anchor arity, values are checked left-to-right under the current replay
-- limits, and the complete Cartesian product is admitted before the anchor is
-- revalidated.  Width or product misses conservatively return @Right Nothing@.
-- Arity, value, anchor, and admitted-search defects fail closed.
--
-- After anchor replay, the existing input-box verifier supplies the
-- lexicographically first violation with the last input varying fastest.  A
-- receipt exists only when that input vector differs from the anchor.  Its
-- nested counterexample remains ordinary exact-problem behavioral evidence;
-- this operation consumes no solver status and creates no positive evidence.
simplifyLengthProblemCounterexample
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> CheckedLengthProblem identity local
  -> ValidatedLengthCounterexample
  -> Either LengthCounterexampleSimplificationError
      (Maybe
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthCounterexampleSimplification))
simplifyLengthProblemCounterexample evaluationLimits inputBoxLimits problem
    anchor = do
  admitted <- admit
  case admitted of
    Nothing -> Right Nothing
    Just maximums -> do
      anchorReplay <- either
        (Left .
          LengthCounterexampleSimplificationAnchorEvaluationRejected)
        Right
        $ replayLengthProblemAssignment evaluationLimits problem
        $ LengthProblemAssignment maximums
      case anchorReplay of
        LengthProblemPostconditionViolated _ -> pure ()
        LengthProblemPreconditionNotMet -> Left
          LengthCounterexampleSimplificationAnchorNotCounterexample
        LengthProblemPostconditionSatisfied -> Left
          LengthCounterexampleSimplificationAnchorNotCounterexample
      validation <- either
        (Left .
          LengthCounterexampleSimplificationInputBoxValidationRejected)
        Right
        $ validateLengthProblemInputBox evaluationLimits inputBoxLimits
            problem maximums
      case validation of
        LengthInputBoxValidated _ -> Left
          LengthCounterexampleSimplificationInternalInvariant
        LengthInputBoxCounterexample evidence -> do
          counterexample <- either
            (const $ Left
              LengthCounterexampleSimplificationInternalInvariant)
            Right
            $ replayBehavioralEvidence
                (checkedLengthProblemBehavioralProblem problem) evidence
          let simplifiedInputs = validatedLengthCounterexampleInputs
                counterexample
          if simplifiedInputs == maximums
            then Right Nothing
            else do
              inspected <- case inputBoxInspectedAssignmentCount
                  maximums simplifiedInputs of
                Nothing -> Left
                  LengthCounterexampleSimplificationInternalInvariant
                Just value -> Right value
              let receipt = ValidatedLengthCounterexampleSimplificationReceipt
                    lengthCounterexampleSimplificationSchemaTag
                    maximums inspected counterexample
              Right $ Just
                $ mapBehavioralEvidenceReceipt (const receipt) evidence
 where
  originalInputs = validatedLengthCounterexampleInputs anchor
  inputCount = checkedLengthProblemInputCount problem

  admit
    :: Either LengthCounterexampleSimplificationError (Maybe [Natural])
  admit
    | inputCount > lengthInputBoxInputLimit inputBoxLimits = Right Nothing
    | otherwise = do
        maximums <- either rejectInputBox Right
          $ exactInputBoxBounds inputCount originalInputs
        mapM_ checkMaximum $ zip [0 ..] maximums
        case inputBoxAssignmentCount inputBoxLimits maximums of
          Left LengthInputBoxAssignmentLimitExceeded {} -> Right Nothing
          Left failure -> rejectInputBox failure
          Right _ -> Right $ Just maximums

  checkMaximum (index, value) = either
    (rejectInputBox . LengthInputBoxMaximumValueRejected index)
    Right
    $ checkAssignedValue evaluationLimits
        (LengthProblemInputValue index) value

  rejectInputBox = Left .
    LengthCounterexampleSimplificationInputBoxValidationRejected

-- | Nominal binary-product sibling of
-- 'simplifyLengthProblemCounterexample'.  It uses the identical dominated-box
-- ordering while retaining closed product replay errors and evidence.
simplifyLengthSpinePairProblemCounterexample
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> CheckedLengthSpinePairProblem identity local
  -> ValidatedLengthSpinePairCounterexample
  -> Either LengthSpinePairCounterexampleSimplificationError
      (Maybe
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairCounterexampleSimplification))
simplifyLengthSpinePairProblemCounterexample evaluationLimits inputBoxLimits
    problem anchor = do
  admitted <- admit
  case admitted of
    Nothing -> Right Nothing
    Just maximums -> do
      anchorReplay <- either
        (Left .
          LengthSpinePairCounterexampleSimplificationAnchorEvaluationRejected)
        Right
        $ replayLengthSpinePairProblemAssignment evaluationLimits problem
        $ LengthProblemAssignment maximums
      case anchorReplay of
        LengthSpinePairProblemPostconditionViolated _ -> pure ()
        LengthSpinePairProblemPreconditionNotMet -> Left
          LengthSpinePairCounterexampleSimplificationAnchorNotCounterexample
        LengthSpinePairProblemPostconditionSatisfied -> Left
          LengthSpinePairCounterexampleSimplificationAnchorNotCounterexample
      validation <- either
        (Left .
          LengthSpinePairCounterexampleSimplificationInputBoxValidationRejected)
        Right
        $ validateLengthSpinePairProblemInputBox evaluationLimits
            inputBoxLimits problem maximums
      case validation of
        LengthInputBoxValidated _ -> Left
          LengthSpinePairCounterexampleSimplificationInternalInvariant
        LengthInputBoxCounterexample evidence -> do
          counterexample <- either
            (const $ Left
              LengthSpinePairCounterexampleSimplificationInternalInvariant)
            Right
            $ replayBehavioralEvidence
                (checkedLengthSpinePairProblemBehavioralProblem problem)
                evidence
          let simplifiedInputs = validatedLengthSpinePairCounterexampleInputs
                counterexample
          if simplifiedInputs == maximums
            then Right Nothing
            else do
              inspected <- case inputBoxInspectedAssignmentCount
                  maximums simplifiedInputs of
                Nothing -> Left
                  LengthSpinePairCounterexampleSimplificationInternalInvariant
                Just value -> Right value
              let receipt =
                    ValidatedLengthSpinePairCounterexampleSimplificationReceipt
                      lengthSpinePairCounterexampleSimplificationSchemaTag
                      maximums inspected counterexample
              Right $ Just
                $ mapBehavioralEvidenceReceipt (const receipt) evidence
 where
  originalInputs = validatedLengthSpinePairCounterexampleInputs anchor
  inputCount = checkedLengthSpinePairProblemInputCount problem

  admit
    :: Either
        LengthSpinePairCounterexampleSimplificationError
        (Maybe [Natural])
  admit
    | inputCount > lengthInputBoxInputLimit inputBoxLimits = Right Nothing
    | otherwise = do
        maximums <- either rejectInputBox Right
          $ exactSpinePairInputBoxBounds inputCount originalInputs
        mapM_ checkMaximum $ zip [0 ..] maximums
        case spinePairInputBoxAssignmentCount inputBoxLimits maximums of
          Left LengthSpinePairInputBoxAssignmentLimitExceeded {} ->
            Right Nothing
          Left failure -> rejectInputBox failure
          Right _ -> Right $ Just maximums

  checkMaximum (index, value) = either
    (rejectInputBox .
      LengthSpinePairInputBoxMaximumValueRejected index)
    Right
    $ checkSpinePairAssignedValue evaluationLimits
        (LengthSpinePairProblemInputValue index) value

  rejectInputBox = Left .
    LengthSpinePairCounterexampleSimplificationInputBoxValidationRejected

-- | Exhaustively check the Cartesian product described by source-ordered,
-- inclusive input maximums.  Enumeration is lexicographic with the last input
-- varying fastest.  The first violation stops the traversal and is returned as
-- ordinary exact-problem evidence; a positive receipt is constructed only
-- after every assignment has completed without a violation.
--
-- This verifier consumes no solver observation.  In particular it does not
-- strengthen an @unsat@ result: callers may run it after any external report,
-- but the only authority returned here comes from independent concrete replay
-- of the explicitly finite box.
validateLengthProblemInputBox
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> CheckedLengthProblem identity local
  -> [Natural]
  -> Either LengthInputBoxValidationError
      (LengthInputBoxValidation
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthCounterexample)
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthInputBox))
validateLengthProblemInputBox evaluationLimits inputBoxLimits problem
    rawMaximums = do
  let inputCount = checkedLengthProblemInputCount problem
      maximumInputs = lengthInputBoxInputLimit inputBoxLimits
  if inputCount <= maximumInputs
    then pure ()
    else Left $ LengthInputBoxProblemInputLimitExceeded
      maximumInputs inputCount
  maximums <- exactInputBoxBounds inputCount rawMaximums
  mapM_ checkMaximum $ zip [0 ..] maximums
  assignmentCount <- inputBoxAssignmentCount inputBoxLimits maximums
  enumerate maximums assignmentCount 0 0 $ replicate (length maximums) 0
 where
  checkMaximum (index, value) = either
    (Left . LengthInputBoxMaximumValueRejected index)
    Right
    $ checkAssignedValue evaluationLimits
        (LengthProblemInputValue index) value

  enumerate maximums assignmentCount !ordinal !applicable inputs = do
    replay <- either
      (Left . LengthInputBoxAssignmentEvaluationRejected ordinal)
      Right
      $ replayLengthProblemAssignment evaluationLimits problem
          $ LengthProblemAssignment inputs
    case replay of
      LengthProblemPostconditionViolated receipt -> Right
        $ LengthInputBoxCounterexample
        $ mkBehavioralEvidence
            (checkedLengthProblemBehavioralProblem problem) receipt
      LengthProblemPreconditionNotMet -> continue maximums assignmentCount
        ordinal applicable inputs
      LengthProblemPostconditionSatisfied -> continue maximums assignmentCount
        ordinal (applicable + 1) inputs

  continue maximums assignmentCount !ordinal !applicable inputs =
    case nextInputBoxAssignment maximums inputs of
      Left failure -> Left failure
      Right (Just following) -> enumerate maximums assignmentCount
        (ordinal + 1) applicable following
      Right Nothing
        | ordinal + 1 /= assignmentCount ->
            Left LengthInputBoxInternalEnumerationInvariant
        | otherwise ->
            let receipt = ValidatedLengthInputBoxReceipt
                  lengthInputBoxValidationSchemaTag maximums assignmentCount
                  applicable $ problemBasis problem
            in Right $ LengthInputBoxValidated
              $ mkBehavioralEvidence
                  (checkedLengthProblemBehavioralProblem problem) receipt

-- | Exhaustively validate one finite Cartesian input box in the nominal
-- binary product domain.  Enumeration and failure precedence mirror the
-- scalar verifier, but both positive and counterexample receipts remain
-- product-domain evidence.
validateLengthSpinePairProblemInputBox
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> CheckedLengthSpinePairProblem identity local
  -> [Natural]
  -> Either LengthSpinePairInputBoxValidationError
      (LengthInputBoxValidation
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairCounterexample)
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairInputBox))
validateLengthSpinePairProblemInputBox evaluationLimits inputBoxLimits problem
    rawMaximums = do
  let inputCount = checkedLengthSpinePairProblemInputCount problem
      maximumInputs = lengthInputBoxInputLimit inputBoxLimits
  if inputCount <= maximumInputs
    then pure ()
    else Left $ LengthSpinePairInputBoxProblemInputLimitExceeded
      maximumInputs inputCount
  maximums <- exactSpinePairInputBoxBounds inputCount rawMaximums
  mapM_ checkMaximum $ zip [0 ..] maximums
  assignmentCount <- spinePairInputBoxAssignmentCount
    inputBoxLimits maximums
  enumerate maximums assignmentCount 0 0 $ replicate (length maximums) 0
 where
  checkMaximum (index, value) = either
    (Left . LengthSpinePairInputBoxMaximumValueRejected index)
    Right
    $ checkSpinePairAssignedValue evaluationLimits
        (LengthSpinePairProblemInputValue index) value

  enumerate maximums assignmentCount !ordinal !applicable inputs = do
    replay <- either
      (Left . LengthSpinePairInputBoxAssignmentEvaluationRejected ordinal)
      Right
      $ replayLengthSpinePairProblemAssignment evaluationLimits problem
          $ LengthProblemAssignment inputs
    case replay of
      LengthSpinePairProblemPostconditionViolated receipt -> Right
        $ LengthInputBoxCounterexample
        $ mkBehavioralEvidence
            (checkedLengthSpinePairProblemBehavioralProblem problem) receipt
      LengthSpinePairProblemPreconditionNotMet ->
        continue maximums assignmentCount ordinal applicable inputs
      LengthSpinePairProblemPostconditionSatisfied ->
        continue maximums assignmentCount ordinal (applicable + 1) inputs

  continue maximums assignmentCount !ordinal !applicable inputs =
    case nextSpinePairInputBoxAssignment maximums inputs of
      Left failure -> Left failure
      Right (Just following) -> enumerate maximums assignmentCount
        (ordinal + 1) applicable following
      Right Nothing
        | ordinal + 1 /= assignmentCount ->
            Left LengthSpinePairInputBoxInternalEnumerationInvariant
        | otherwise ->
            let receipt = ValidatedLengthSpinePairInputBoxReceipt
                  lengthSpinePairInputBoxValidationSchemaTag
                  maximums assignmentCount applicable
                  $ spinePairProblemBasis problem
            in Right $ LengthInputBoxValidated
              $ mkBehavioralEvidence
                  (checkedLengthSpinePairProblemBehavioralProblem problem)
                  receipt

-- | Establish the complete applicable domain of one exact scalar problem when
-- its normalized precondition directly bounds every compact input.
--
-- Width is rejected before the precondition is scanned.  Coverage then admits
-- only top-level normalized @input <= literal@ conjuncts and chooses the
-- tightest direct bound for each input.  A missing bound is an ordinary
-- inapplicable result, not a verification failure.  Exact coverage delegates
-- to the existing solver-independent box verifier; neither construction nor
-- completion consumes a solver observation.
validateLengthProblemApplicableDomain
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> CheckedLengthProblem identity local
  -> Either LengthApplicableDomainValidationError
      (LengthApplicableDomainValidation
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthCounterexample)
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthApplicableDomain))
validateLengthProblemApplicableDomain evaluationLimits inputBoxLimits
    problem = do
  let inputCount = checkedLengthProblemInputCount problem
      maximumInputs = lengthInputBoxInputLimit inputBoxLimits
  if inputCount <= maximumInputs
    then pure ()
    else Left $ LengthApplicableDomainInputBoxValidationRejected
      $ LengthInputBoxProblemInputLimitExceeded maximumInputs inputCount
  case tightApplicableDomainMaximums inputCount scalarInputPosition
      $ checkedLengthProblemPrecondition problem of
    Left inapplicability -> Right
      $ LengthApplicableDomainInapplicable inapplicability
    Right maximums -> do
      validation <- either
        (Left . LengthApplicableDomainInputBoxValidationRejected)
        Right
        $ validateLengthProblemInputBox evaluationLimits inputBoxLimits
            problem maximums
      pure $ case validation of
        LengthInputBoxCounterexample evidence ->
          LengthApplicableDomainCounterexample evidence
        LengthInputBoxValidated evidence ->
          LengthApplicableDomainEstablished
            $ mapBehavioralEvidenceReceipt
                (ValidatedLengthApplicableDomainReceipt
                  lengthApplicableDomainValidationSchemaTag)
                evidence
 where
  scalarInputPosition variable = case variable of
    LengthInput position -> Just position
    LengthResult -> Nothing

-- | Nominal binary-product sibling of
-- 'validateLengthProblemApplicableDomain'.  The direct coverage rule examines
-- only compact inputs; result-component references cannot occur in a checked
-- precondition and grant no bound authority here.
validateLengthSpinePairProblemApplicableDomain
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> CheckedLengthSpinePairProblem identity local
  -> Either LengthSpinePairApplicableDomainValidationError
      (LengthApplicableDomainValidation
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairCounterexample)
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairApplicableDomain))
validateLengthSpinePairProblemApplicableDomain evaluationLimits inputBoxLimits
    problem = do
  let inputCount = checkedLengthSpinePairProblemInputCount problem
      maximumInputs = lengthInputBoxInputLimit inputBoxLimits
  if inputCount <= maximumInputs
    then pure ()
    else Left $ LengthSpinePairApplicableDomainInputBoxValidationRejected
      $ LengthSpinePairInputBoxProblemInputLimitExceeded
          maximumInputs inputCount
  case tightApplicableDomainMaximums inputCount spinePairInputPosition
      $ checkedLengthSpinePairProblemPrecondition problem of
    Left inapplicability -> Right
      $ LengthApplicableDomainInapplicable inapplicability
    Right maximums -> do
      validation <- either
        (Left . LengthSpinePairApplicableDomainInputBoxValidationRejected)
        Right
        $ validateLengthSpinePairProblemInputBox
            evaluationLimits inputBoxLimits problem maximums
      pure $ case validation of
        LengthInputBoxCounterexample evidence ->
          LengthApplicableDomainCounterexample evidence
        LengthInputBoxValidated evidence ->
          LengthApplicableDomainEstablished
            $ mapBehavioralEvidenceReceipt
                (ValidatedLengthSpinePairApplicableDomainReceipt
                  lengthSpinePairApplicableDomainValidationSchemaTag)
                evidence
 where
  spinePairInputPosition variable = case variable of
    LengthSpinePairInput position -> Just position
    LengthSpinePairResult _ -> Nothing

-- The formula has already passed bounded normalization.  In particular a
-- top-level conjunction is flat, sorted, and duplicate-free, but this scanner
-- depends only on its public normalized shape.  It deliberately ignores every
-- formula except an exact direct natural upper bound.
tightApplicableDomainMaximums
  :: Int
  -> (variable -> Maybe Natural)
  -> LengthFormula variable
  -> Either LengthApplicableDomainInapplicability [Natural]
tightApplicableDomainMaximums inputCount inputPosition precondition =
  mapM maximumFor [0 .. inputCount - 1]
 where
  clauses = case precondition of
    LengthAll formulas -> formulas
    formula -> [formula]

  bounds = collect Map.empty clauses

  collect !retained [] = retained
  collect !retained (formula : remaining) = collect retained' remaining
   where
    retained' = case formula of
      LengthAtMost (LengthVariable variable) (LengthLiteral maximumValue) ->
        case inputPosition variable of
          Nothing -> retained
          Just position -> Map.insertWith min position maximumValue retained
      _ -> retained

  maximumFor index = case Map.lookup (fromIntegral index) bounds of
    Just maximumValue -> Right maximumValue
    Nothing -> Left $ LengthApplicableDomainInputUpperBoundMissing index

-- | Establish the complete applicable domain of one exact scalar problem when
-- every compact input is bounded by the additive positive-affine rule.
--
-- The original direct-bound entrance remains literal-only.  This sibling also
-- recognizes a positive-affine expression bounded above by a literal, or equal
-- to one, and proves an empty applicable domain from a syntactic contradiction.
-- It still delegates all behavioral authority to the existing finite-box
-- verifier and consumes no solver observation.
validateLengthProblemPositiveAffineApplicableDomain
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> CheckedLengthProblem identity local
  -> Either LengthApplicableDomainValidationError
      (LengthApplicableDomainValidation
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthCounterexample)
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthPositiveAffineApplicableDomain))
validateLengthProblemPositiveAffineApplicableDomain
    evaluationLimits inputBoxLimits problem = do
  let inputCount = checkedLengthProblemInputCount problem
      maximumInputs = lengthInputBoxInputLimit inputBoxLimits
  if inputCount <= maximumInputs
    then pure ()
    else Left $ LengthApplicableDomainInputBoxValidationRejected
      $ LengthInputBoxProblemInputLimitExceeded maximumInputs inputCount
  case positiveAffineApplicableDomainMaximums inputCount scalarInputPosition
      $ checkedLengthProblemPrecondition problem of
    Left inapplicability -> Right
      $ LengthApplicableDomainInapplicable inapplicability
    Right maximums -> do
      validation <- either
        (Left . LengthApplicableDomainInputBoxValidationRejected)
        Right
        $ validateLengthProblemInputBox evaluationLimits inputBoxLimits
            problem maximums
      pure $ case validation of
        LengthInputBoxCounterexample evidence ->
          LengthApplicableDomainCounterexample evidence
        LengthInputBoxValidated evidence ->
          LengthApplicableDomainEstablished
            $ mapBehavioralEvidenceReceipt
                (ValidatedLengthPositiveAffineApplicableDomainReceipt
                  lengthPositiveAffineApplicableDomainValidationSchemaTag)
                evidence
 where
  scalarInputPosition variable = case variable of
    LengthInput position -> Just position
    LengthResult -> Nothing

-- | Nominal binary-product sibling of
-- 'validateLengthProblemPositiveAffineApplicableDomain'.
validateLengthSpinePairProblemPositiveAffineApplicableDomain
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> CheckedLengthSpinePairProblem identity local
  -> Either LengthSpinePairApplicableDomainValidationError
      (LengthApplicableDomainValidation
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairCounterexample)
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairPositiveAffineApplicableDomain))
validateLengthSpinePairProblemPositiveAffineApplicableDomain
    evaluationLimits inputBoxLimits problem = do
  let inputCount = checkedLengthSpinePairProblemInputCount problem
      maximumInputs = lengthInputBoxInputLimit inputBoxLimits
  if inputCount <= maximumInputs
    then pure ()
    else Left $ LengthSpinePairApplicableDomainInputBoxValidationRejected
      $ LengthSpinePairInputBoxProblemInputLimitExceeded
          maximumInputs inputCount
  case positiveAffineApplicableDomainMaximums inputCount spinePairInputPosition
      $ checkedLengthSpinePairProblemPrecondition problem of
    Left inapplicability -> Right
      $ LengthApplicableDomainInapplicable inapplicability
    Right maximums -> do
      validation <- either
        (Left . LengthSpinePairApplicableDomainInputBoxValidationRejected)
        Right
        $ validateLengthSpinePairProblemInputBox
            evaluationLimits inputBoxLimits problem maximums
      pure $ case validation of
        LengthInputBoxCounterexample evidence ->
          LengthApplicableDomainCounterexample evidence
        LengthInputBoxValidated evidence ->
          LengthApplicableDomainEstablished
            $ mapBehavioralEvidenceReceipt
                (ValidatedLengthSpinePairPositiveAffineApplicableDomainReceipt
                  lengthSpinePairPositiveAffineApplicableDomainValidationSchemaTag)
                evidence
 where
  spinePairInputPosition variable = case variable of
    LengthSpinePairInput position -> Just position
    LengthSpinePairResult _ -> Nothing

-- | Establish the complete applicable domain of one exact scalar problem
-- when top-level positive-affine relations jointly imply a finite upper bound
-- for every compact input.  This solver-free entrance is additive: neither of
-- the established applicable-domain rules changes meaning.
validateLengthProblemRelationalPositiveAffineApplicableDomain
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> CheckedLengthProblem identity local
  -> Either LengthApplicableDomainValidationError
      (LengthApplicableDomainValidation
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthCounterexample)
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthRelationalPositiveAffineApplicableDomain))
validateLengthProblemRelationalPositiveAffineApplicableDomain
    evaluationLimits inputBoxLimits problem = do
  let inputCount = checkedLengthProblemInputCount problem
      maximumInputs = lengthInputBoxInputLimit inputBoxLimits
  if inputCount <= maximumInputs
    then pure ()
    else Left $ LengthApplicableDomainInputBoxValidationRejected
      $ LengthInputBoxProblemInputLimitExceeded maximumInputs inputCount
  case relationalPositiveAffineApplicableDomainMaximums
      inputCount scalarInputPosition
      $ checkedLengthProblemPrecondition problem of
    Left inapplicability -> Right
      $ LengthApplicableDomainInapplicable inapplicability
    Right maximums -> do
      validation <- either
        (Left . LengthApplicableDomainInputBoxValidationRejected)
        Right
        $ validateLengthProblemInputBox evaluationLimits inputBoxLimits
            problem maximums
      pure $ case validation of
        LengthInputBoxCounterexample evidence ->
          LengthApplicableDomainCounterexample evidence
        LengthInputBoxValidated evidence ->
          LengthApplicableDomainEstablished
            $ mapBehavioralEvidenceReceipt
                (ValidatedLengthRelationalPositiveAffineApplicableDomainReceipt
                  lengthRelationalPositiveAffineApplicableDomainValidationSchemaTag)
                evidence
 where
  scalarInputPosition variable = case variable of
    LengthInput position -> Just position
    LengthResult -> Nothing

-- | Nominal binary-product sibling of
-- 'validateLengthProblemRelationalPositiveAffineApplicableDomain'.
validateLengthSpinePairProblemRelationalPositiveAffineApplicableDomain
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> CheckedLengthSpinePairProblem identity local
  -> Either LengthSpinePairApplicableDomainValidationError
      (LengthApplicableDomainValidation
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairCounterexample)
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomain))
validateLengthSpinePairProblemRelationalPositiveAffineApplicableDomain
    evaluationLimits inputBoxLimits problem = do
  let inputCount = checkedLengthSpinePairProblemInputCount problem
      maximumInputs = lengthInputBoxInputLimit inputBoxLimits
  if inputCount <= maximumInputs
    then pure ()
    else Left $ LengthSpinePairApplicableDomainInputBoxValidationRejected
      $ LengthSpinePairInputBoxProblemInputLimitExceeded
          maximumInputs inputCount
  case relationalPositiveAffineApplicableDomainMaximums
      inputCount spinePairInputPosition
      $ checkedLengthSpinePairProblemPrecondition problem of
    Left inapplicability -> Right
      $ LengthApplicableDomainInapplicable inapplicability
    Right maximums -> do
      validation <- either
        (Left . LengthSpinePairApplicableDomainInputBoxValidationRejected)
        Right
        $ validateLengthSpinePairProblemInputBox
            evaluationLimits inputBoxLimits problem maximums
      pure $ case validation of
        LengthInputBoxCounterexample evidence ->
          LengthApplicableDomainCounterexample evidence
        LengthInputBoxValidated evidence ->
          LengthApplicableDomainEstablished
            $ mapBehavioralEvidenceReceipt
                (ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomainReceipt
                  lengthSpinePairRelationalPositiveAffineApplicableDomainValidationSchemaTag)
                evidence
 where
  spinePairInputPosition variable = case variable of
    LengthSpinePairInput position -> Just position
    LengthSpinePairResult _ -> Nothing

-- | Establish the complete applicable domain of one exact scalar problem
-- when ordinary or strict top-level positive-affine relations jointly imply
-- a finite upper bound for every compact input.  A normalized
-- @not (left <= right)@ clause contributes the exact natural-number rule
-- @right + 1 <= left@ only through this additive entrance.
validateLengthProblemStrictRelationalPositiveAffineApplicableDomain
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> CheckedLengthProblem identity local
  -> Either LengthApplicableDomainValidationError
      (LengthApplicableDomainValidation
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthCounterexample)
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthStrictRelationalPositiveAffineApplicableDomain))
validateLengthProblemStrictRelationalPositiveAffineApplicableDomain
    evaluationLimits inputBoxLimits problem = do
  let inputCount = checkedLengthProblemInputCount problem
      maximumInputs = lengthInputBoxInputLimit inputBoxLimits
  if inputCount <= maximumInputs
    then pure ()
    else Left $ LengthApplicableDomainInputBoxValidationRejected
      $ LengthInputBoxProblemInputLimitExceeded maximumInputs inputCount
  case strictRelationalPositiveAffineApplicableDomainMaximums
      inputCount scalarInputPosition
      $ checkedLengthProblemPrecondition problem of
    Left inapplicability -> Right
      $ LengthApplicableDomainInapplicable inapplicability
    Right maximums -> do
      validation <- either
        (Left . LengthApplicableDomainInputBoxValidationRejected)
        Right
        $ validateLengthProblemInputBox evaluationLimits inputBoxLimits
            problem maximums
      pure $ case validation of
        LengthInputBoxCounterexample evidence ->
          LengthApplicableDomainCounterexample evidence
        LengthInputBoxValidated evidence ->
          LengthApplicableDomainEstablished
            $ mapBehavioralEvidenceReceipt
                (ValidatedLengthStrictRelationalPositiveAffineApplicableDomainReceipt
                  lengthStrictRelationalPositiveAffineApplicableDomainValidationSchemaTag)
                evidence
 where
  scalarInputPosition variable = case variable of
    LengthInput position -> Just position
    LengthResult -> Nothing

-- | Nominal binary-product sibling of
-- 'validateLengthProblemStrictRelationalPositiveAffineApplicableDomain'.
validateLengthSpinePairProblemStrictRelationalPositiveAffineApplicableDomain
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> CheckedLengthSpinePairProblem identity local
  -> Either LengthSpinePairApplicableDomainValidationError
      (LengthApplicableDomainValidation
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairCounterexample)
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomain))
validateLengthSpinePairProblemStrictRelationalPositiveAffineApplicableDomain
    evaluationLimits inputBoxLimits problem = do
  let inputCount = checkedLengthSpinePairProblemInputCount problem
      maximumInputs = lengthInputBoxInputLimit inputBoxLimits
  if inputCount <= maximumInputs
    then pure ()
    else Left $ LengthSpinePairApplicableDomainInputBoxValidationRejected
      $ LengthSpinePairInputBoxProblemInputLimitExceeded
          maximumInputs inputCount
  case strictRelationalPositiveAffineApplicableDomainMaximums
      inputCount spinePairInputPosition
      $ checkedLengthSpinePairProblemPrecondition problem of
    Left inapplicability -> Right
      $ LengthApplicableDomainInapplicable inapplicability
    Right maximums -> do
      validation <- either
        (Left . LengthSpinePairApplicableDomainInputBoxValidationRejected)
        Right
        $ validateLengthSpinePairProblemInputBox
            evaluationLimits inputBoxLimits problem maximums
      pure $ case validation of
        LengthInputBoxCounterexample evidence ->
          LengthApplicableDomainCounterexample evidence
        LengthInputBoxValidated evidence ->
          LengthApplicableDomainEstablished
            $ mapBehavioralEvidenceReceipt
                (ValidatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainReceipt
                  lengthSpinePairStrictRelationalPositiveAffineApplicableDomainValidationSchemaTag)
                evidence
 where
  spinePairInputPosition variable = case variable of
    LengthSpinePairInput position -> Just position
    LengthSpinePairResult _ -> Nothing

-- | Add exact root-quotient consequences to the strict relational
-- positive-affine extractor.  Every supported quotient atom is converted to
-- at most two proof-only affine rules before the established closure and
-- finite-box replay are used unchanged.
validateLengthProblemStrictRelationalPositiveAffineQuotientApplicableDomain
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> CheckedLengthProblem identity local
  -> Either LengthApplicableDomainValidationError
      (LengthApplicableDomainValidation
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthCounterexample)
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthStrictRelationalPositiveAffineQuotientApplicableDomain))
validateLengthProblemStrictRelationalPositiveAffineQuotientApplicableDomain
    evaluationLimits inputBoxLimits problem = do
  let inputCount = checkedLengthProblemInputCount problem
      maximumInputs = lengthInputBoxInputLimit inputBoxLimits
  if inputCount <= maximumInputs
    then pure ()
    else Left $ LengthApplicableDomainInputBoxValidationRejected
      $ LengthInputBoxProblemInputLimitExceeded maximumInputs inputCount
  case strictRelationalPositiveAffineQuotientApplicableDomainMaximums
      inputCount scalarInputPosition
      $ checkedLengthProblemPrecondition problem of
    Left inapplicability -> Right
      $ LengthApplicableDomainInapplicable inapplicability
    Right maximums -> do
      validation <- either
        (Left . LengthApplicableDomainInputBoxValidationRejected)
        Right
        $ validateLengthProblemInputBox evaluationLimits inputBoxLimits
            problem maximums
      pure $ case validation of
        LengthInputBoxCounterexample evidence ->
          LengthApplicableDomainCounterexample evidence
        LengthInputBoxValidated evidence ->
          LengthApplicableDomainEstablished
            $ mapBehavioralEvidenceReceipt
                (ValidatedLengthStrictRelationalPositiveAffineQuotientApplicableDomainReceipt
                  lengthStrictRelationalPositiveAffineQuotientApplicableDomainValidationSchemaTag)
                evidence
 where
  scalarInputPosition variable = case variable of
    LengthInput position -> Just position
    LengthResult -> Nothing

-- | Nominal binary-product sibling of
-- 'validateLengthProblemStrictRelationalPositiveAffineQuotientApplicableDomain'.
validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientApplicableDomain
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> CheckedLengthSpinePairProblem identity local
  -> Either LengthSpinePairApplicableDomainValidationError
      (LengthApplicableDomainValidation
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairCounterexample)
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomain))
validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientApplicableDomain
    evaluationLimits inputBoxLimits problem = do
  let inputCount = checkedLengthSpinePairProblemInputCount problem
      maximumInputs = lengthInputBoxInputLimit inputBoxLimits
  if inputCount <= maximumInputs
    then pure ()
    else Left $ LengthSpinePairApplicableDomainInputBoxValidationRejected
      $ LengthSpinePairInputBoxProblemInputLimitExceeded
          maximumInputs inputCount
  case strictRelationalPositiveAffineQuotientApplicableDomainMaximums
      inputCount spinePairInputPosition
      $ checkedLengthSpinePairProblemPrecondition problem of
    Left inapplicability -> Right
      $ LengthApplicableDomainInapplicable inapplicability
    Right maximums -> do
      validation <- either
        (Left . LengthSpinePairApplicableDomainInputBoxValidationRejected)
        Right
        $ validateLengthSpinePairProblemInputBox
            evaluationLimits inputBoxLimits problem maximums
      pure $ case validation of
        LengthInputBoxCounterexample evidence ->
          LengthApplicableDomainCounterexample evidence
        LengthInputBoxValidated evidence ->
          LengthApplicableDomainEstablished
            $ mapBehavioralEvidenceReceipt
                (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainReceipt
                  lengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainValidationSchemaTag)
                evidence
 where
  spinePairInputPosition variable = case variable of
    LengthSpinePairInput position -> Just position
    LengthSpinePairResult _ -> Nothing

-- | Cumulative scalar successor which adds sound, conjunctive consequences
-- for one immediate root minimum or maximum.  Unsupported extrema clauses
-- contribute no rule; the original checked precondition is still replayed
-- exhaustively over every derived assignment.
validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> CheckedLengthProblem identity local
  -> Either LengthApplicableDomainValidationError
      (LengthApplicableDomainValidation
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthCounterexample)
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain))
validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain
    evaluationLimits inputBoxLimits problem = do
  let inputCount = checkedLengthProblemInputCount problem
      maximumInputs = lengthInputBoxInputLimit inputBoxLimits
  if inputCount <= maximumInputs
    then pure ()
    else Left $ LengthApplicableDomainInputBoxValidationRejected
      $ LengthInputBoxProblemInputLimitExceeded maximumInputs inputCount
  case strictRelationalPositiveAffineQuotientRootExtremaApplicableDomainMaximums
      inputCount scalarInputPosition
      $ checkedLengthProblemPrecondition problem of
    Left inapplicability -> Right
      $ LengthApplicableDomainInapplicable inapplicability
    Right maximums -> do
      validation <- either
        (Left . LengthApplicableDomainInputBoxValidationRejected)
        Right
        $ validateLengthProblemInputBox evaluationLimits inputBoxLimits
            problem maximums
      pure $ case validation of
        LengthInputBoxCounterexample evidence ->
          LengthApplicableDomainCounterexample evidence
        LengthInputBoxValidated evidence ->
          LengthApplicableDomainEstablished
            $ mapBehavioralEvidenceReceipt
                (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainReceipt
                  lengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainValidationSchemaTag)
                evidence
 where
  scalarInputPosition variable = case variable of
    LengthInput position -> Just position
    LengthResult -> Nothing

-- | Nominal binary-product sibling of
-- 'validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain'.
validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> CheckedLengthSpinePairProblem identity local
  -> Either LengthSpinePairApplicableDomainValidationError
      (LengthApplicableDomainValidation
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairCounterexample)
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain))
validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain
    evaluationLimits inputBoxLimits problem = do
  let inputCount = checkedLengthSpinePairProblemInputCount problem
      maximumInputs = lengthInputBoxInputLimit inputBoxLimits
  if inputCount <= maximumInputs
    then pure ()
    else Left $ LengthSpinePairApplicableDomainInputBoxValidationRejected
      $ LengthSpinePairInputBoxProblemInputLimitExceeded
          maximumInputs inputCount
  case strictRelationalPositiveAffineQuotientRootExtremaApplicableDomainMaximums
      inputCount spinePairInputPosition
      $ checkedLengthSpinePairProblemPrecondition problem of
    Left inapplicability -> Right
      $ LengthApplicableDomainInapplicable inapplicability
    Right maximums -> do
      validation <- either
        (Left . LengthSpinePairApplicableDomainInputBoxValidationRejected)
        Right
        $ validateLengthSpinePairProblemInputBox
            evaluationLimits inputBoxLimits problem maximums
      pure $ case validation of
        LengthInputBoxCounterexample evidence ->
          LengthApplicableDomainCounterexample evidence
        LengthInputBoxValidated evidence ->
          LengthApplicableDomainEstablished
            $ mapBehavioralEvidenceReceipt
                (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainReceipt
                  lengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainValidationSchemaTag)
                evidence
 where
  spinePairInputPosition variable = case variable of
    LengthSpinePairInput position -> Just position
    LengthSpinePairResult _ -> Nothing

-- | Cumulative scalar successor which adds exact conjunctive consequences for
-- one immediate natural monus.  Unsupported monus clauses contribute no rule;
-- the original checked precondition is still replayed exhaustively over every
-- derived assignment.
validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> CheckedLengthProblem identity local
  -> Either LengthApplicableDomainValidationError
      (LengthApplicableDomainValidation
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthCounterexample)
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain))
validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain
    evaluationLimits inputBoxLimits problem = do
  let inputCount = checkedLengthProblemInputCount problem
      maximumInputs = lengthInputBoxInputLimit inputBoxLimits
  if inputCount <= maximumInputs
    then pure ()
    else Left $ LengthApplicableDomainInputBoxValidationRejected
      $ LengthInputBoxProblemInputLimitExceeded maximumInputs inputCount
  case strictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainMaximums
      inputCount scalarInputPosition
      $ checkedLengthProblemPrecondition problem of
    Left inapplicability -> Right
      $ LengthApplicableDomainInapplicable inapplicability
    Right maximums -> do
      validation <- either
        (Left . LengthApplicableDomainInputBoxValidationRejected)
        Right
        $ validateLengthProblemInputBox evaluationLimits inputBoxLimits
            problem maximums
      pure $ case validation of
        LengthInputBoxCounterexample evidence ->
          LengthApplicableDomainCounterexample evidence
        LengthInputBoxValidated evidence ->
          LengthApplicableDomainEstablished
            $ mapBehavioralEvidenceReceipt
                (ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainReceipt
                  lengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainValidationSchemaTag)
                evidence
 where
  scalarInputPosition variable = case variable of
    LengthInput position -> Just position
    LengthResult -> Nothing

-- | Nominal binary-product sibling of
-- 'validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain'.
validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> CheckedLengthSpinePairProblem identity local
  -> Either LengthSpinePairApplicableDomainValidationError
      (LengthApplicableDomainValidation
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairCounterexample)
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain))
validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain
    evaluationLimits inputBoxLimits problem = do
  let inputCount = checkedLengthSpinePairProblemInputCount problem
      maximumInputs = lengthInputBoxInputLimit inputBoxLimits
  if inputCount <= maximumInputs
    then pure ()
    else Left $ LengthSpinePairApplicableDomainInputBoxValidationRejected
      $ LengthSpinePairInputBoxProblemInputLimitExceeded
          maximumInputs inputCount
  case strictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainMaximums
      inputCount spinePairInputPosition
      $ checkedLengthSpinePairProblemPrecondition problem of
    Left inapplicability -> Right
      $ LengthApplicableDomainInapplicable inapplicability
    Right maximums -> do
      validation <- either
        (Left . LengthSpinePairApplicableDomainInputBoxValidationRejected)
        Right
        $ validateLengthSpinePairProblemInputBox
            evaluationLimits inputBoxLimits problem maximums
      pure $ case validation of
        LengthInputBoxCounterexample evidence ->
          LengthApplicableDomainCounterexample evidence
        LengthInputBoxValidated evidence ->
          LengthApplicableDomainEstablished
            $ mapBehavioralEvidenceReceipt
                (ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainReceipt
                  lengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainValidationSchemaTag)
                evidence
 where
 spinePairInputPosition variable = case variable of
    LengthSpinePairInput position -> Just position
    LengthSpinePairResult _ -> Nothing

-- | Cumulative scalar successor which expands the complete normalized
-- precondition into a bounded canonical Boolean DNF and validates the exact
-- finite union of independently derived zero-origin boxes.  Incomparable
-- boxes remain separate, and the original formula is replayed over the global
-- deduplicated assignment set.
validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> LengthBooleanFiniteUnionLimits
  -> CheckedLengthProblem identity local
  -> Either LengthBooleanFiniteUnionApplicableDomainValidationError
      (LengthApplicableDomainValidation
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthCounterexample)
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain))
validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
    evaluationLimits inputBoxLimits unionLimits problem = do
  let inputCount = checkedLengthProblemInputCount problem
      maximumInputs = lengthInputBoxInputLimit inputBoxLimits
  if inputCount <= maximumInputs
    then pure ()
    else Left $ LengthBooleanFiniteUnionProblemInputLimitExceeded
      maximumInputs inputCount
  coverage <- either (Left . preparationError) Right
    $ booleanFiniteUnionApplicableDomainMaximumBoxes
        unionLimits inputCount scalarInputPosition
        $ checkedLengthProblemPrecondition problem
  case coverage of
    Left inapplicability -> Right
      $ LengthApplicableDomainInapplicable inapplicability
    Right boxes -> do
      mapM_ checkBox $ zip [0 ..] boxes
      visits <- either (Left . enumerationError) Right
        $ booleanFiniteUnionAssignmentVisitCount unionLimits boxes
      (assignmentCount, assignments) <- either
        (Left . enumerationError) Right
        $ enumerateBooleanFiniteUnionAssignments inputBoxLimits boxes
      replay boxes visits assignmentCount 0 0 $ Set.toAscList assignments
 where
  scalarInputPosition variable = case variable of
    LengthInput position -> Just position
    LengthResult -> Nothing

  preparationError failure = case failure of
    BooleanFiniteUnionGeneratedBranchLimitExceeded limit observed ->
      LengthBooleanFiniteUnionGeneratedBranchLimitExceeded limit observed
    BooleanFiniteUnionRuleLimitExceeded branch limit observed ->
      LengthBooleanFiniteUnionRuleLimitExceeded branch limit observed
    BooleanFiniteUnionClosureInspectionLimitExceeded branch limit observed ->
      LengthBooleanFiniteUnionClosureInspectionLimitExceeded
        branch limit observed
    BooleanFiniteUnionRetainedBoxLimitExceeded limit observed ->
      LengthBooleanFiniteUnionRetainedBoxLimitExceeded limit observed

  enumerationError failure = case failure of
    BooleanFiniteUnionAssignmentVisitLimitExceeded limit observed ->
      LengthBooleanFiniteUnionAssignmentVisitLimitExceeded limit observed
    BooleanFiniteUnionAssignmentLimitExceeded limit observed ->
      LengthBooleanFiniteUnionAssignmentLimitExceeded limit observed
    BooleanFiniteUnionInternalEnumerationInvariant ->
      LengthBooleanFiniteUnionInternalEnumerationInvariant

  checkBox (boxIndex, maximums) =
    mapM_ (checkMaximum boxIndex) $ zip [0 ..] maximums

  checkMaximum boxIndex (inputIndex, maximumValue) = either
    (Left . LengthBooleanFiniteUnionMaximumValueRejected
      boxIndex inputIndex)
    Right
    $ checkAssignedValue evaluationLimits
        (LengthProblemInputValue inputIndex) maximumValue

  replay boxes visits assignmentCount !ordinal !applicable assignments =
    case assignments of
      []
        | ordinal /= assignmentCount ->
            Left LengthBooleanFiniteUnionInternalEnumerationInvariant
        | otherwise ->
            let receipt =
                  ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainReceipt
                    lengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainValidationSchemaTag
                    boxes visits assignmentCount applicable
                    $ problemBasis problem
            in Right $ LengthApplicableDomainEstablished
              $ mkBehavioralEvidence
                  (checkedLengthProblemBehavioralProblem problem) receipt
      inputs : remaining -> do
        assignmentReplay <- either
          (Left . LengthBooleanFiniteUnionAssignmentEvaluationRejected ordinal)
          Right
          $ replayLengthProblemAssignment evaluationLimits problem
          $ LengthProblemAssignment inputs
        case assignmentReplay of
          LengthProblemPostconditionViolated receipt -> Right
            $ LengthApplicableDomainCounterexample
            $ mkBehavioralEvidence
                (checkedLengthProblemBehavioralProblem problem) receipt
          LengthProblemPreconditionNotMet ->
            replay boxes visits assignmentCount (ordinal + 1) applicable
              remaining
          LengthProblemPostconditionSatisfied ->
            replay boxes visits assignmentCount (ordinal + 1)
              (applicable + 1) remaining

-- | Nominal product-domain sibling of the bounded Boolean finite-union
-- validator.  It uses the same DNF, closure, antichain, visit, and global-set
-- order while retaining product-specific errors and evidence.
validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> LengthBooleanFiniteUnionLimits
  -> CheckedLengthSpinePairProblem identity local
  -> Either
      LengthSpinePairBooleanFiniteUnionApplicableDomainValidationError
      (LengthApplicableDomainValidation
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairCounterexample)
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain))
validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
    evaluationLimits inputBoxLimits unionLimits problem = do
  let inputCount = checkedLengthSpinePairProblemInputCount problem
      maximumInputs = lengthInputBoxInputLimit inputBoxLimits
  if inputCount <= maximumInputs
    then pure ()
    else Left $ LengthSpinePairBooleanFiniteUnionProblemInputLimitExceeded
      maximumInputs inputCount
  coverage <- either (Left . preparationError) Right
    $ booleanFiniteUnionApplicableDomainMaximumBoxes
        unionLimits inputCount spinePairInputPosition
        $ checkedLengthSpinePairProblemPrecondition problem
  case coverage of
    Left inapplicability -> Right
      $ LengthApplicableDomainInapplicable inapplicability
    Right boxes -> do
      mapM_ checkBox $ zip [0 ..] boxes
      visits <- either (Left . enumerationError) Right
        $ booleanFiniteUnionAssignmentVisitCount unionLimits boxes
      (assignmentCount, assignments) <- either
        (Left . enumerationError) Right
        $ enumerateBooleanFiniteUnionAssignments inputBoxLimits boxes
      replay boxes visits assignmentCount 0 0 $ Set.toAscList assignments
 where
  spinePairInputPosition variable = case variable of
    LengthSpinePairInput position -> Just position
    LengthSpinePairResult _ -> Nothing

  preparationError failure = case failure of
    BooleanFiniteUnionGeneratedBranchLimitExceeded limit observed ->
      LengthSpinePairBooleanFiniteUnionGeneratedBranchLimitExceeded
        limit observed
    BooleanFiniteUnionRuleLimitExceeded branch limit observed ->
      LengthSpinePairBooleanFiniteUnionRuleLimitExceeded
        branch limit observed
    BooleanFiniteUnionClosureInspectionLimitExceeded branch limit observed ->
      LengthSpinePairBooleanFiniteUnionClosureInspectionLimitExceeded
        branch limit observed
    BooleanFiniteUnionRetainedBoxLimitExceeded limit observed ->
      LengthSpinePairBooleanFiniteUnionRetainedBoxLimitExceeded
        limit observed

  enumerationError failure = case failure of
    BooleanFiniteUnionAssignmentVisitLimitExceeded limit observed ->
      LengthSpinePairBooleanFiniteUnionAssignmentVisitLimitExceeded
        limit observed
    BooleanFiniteUnionAssignmentLimitExceeded limit observed ->
      LengthSpinePairBooleanFiniteUnionAssignmentLimitExceeded limit observed
    BooleanFiniteUnionInternalEnumerationInvariant ->
      LengthSpinePairBooleanFiniteUnionInternalEnumerationInvariant

  checkBox (boxIndex, maximums) =
    mapM_ (checkMaximum boxIndex) $ zip [0 ..] maximums

  checkMaximum boxIndex (inputIndex, maximumValue) = either
    (Left . LengthSpinePairBooleanFiniteUnionMaximumValueRejected
      boxIndex inputIndex)
    Right
    $ checkSpinePairAssignedValue evaluationLimits
        (LengthSpinePairProblemInputValue inputIndex) maximumValue

  replay boxes visits assignmentCount !ordinal !applicable assignments =
    case assignments of
      []
        | ordinal /= assignmentCount -> Left
            LengthSpinePairBooleanFiniteUnionInternalEnumerationInvariant
        | otherwise ->
            let receipt =
                  ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainReceipt
                    lengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainValidationSchemaTag
                    boxes visits assignmentCount applicable
                    $ spinePairProblemBasis problem
            in Right $ LengthApplicableDomainEstablished
              $ mkBehavioralEvidence
                  (checkedLengthSpinePairProblemBehavioralProblem problem)
                  receipt
      inputs : remaining -> do
        assignmentReplay <- either
          (Left .
            LengthSpinePairBooleanFiniteUnionAssignmentEvaluationRejected
              ordinal)
          Right
          $ replayLengthSpinePairProblemAssignment evaluationLimits problem
          $ LengthProblemAssignment inputs
        case assignmentReplay of
          LengthSpinePairProblemPostconditionViolated receipt -> Right
            $ LengthApplicableDomainCounterexample
            $ mkBehavioralEvidence
                (checkedLengthSpinePairProblemBehavioralProblem problem)
                receipt
          LengthSpinePairProblemPreconditionNotMet ->
            replay boxes visits assignmentCount (ordinal + 1) applicable
              remaining
          LengthSpinePairProblemPostconditionSatisfied ->
            replay boxes visits assignmentCount (ordinal + 1) (applicable + 1)
              remaining

-- | Cumulative scalar successor which counts formula and admitted atomic
-- alternatives under the existing raw branch cap, derives a canonical finite
-- union without constructing proof syntax, and exhaustively replays the
-- original checked formula over the global deduplicated assignment set.
validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> LengthBooleanFiniteUnionLimits
  -> CheckedLengthProblem identity local
  -> Either LengthBooleanFiniteUnionApplicableDomainValidationError
      (LengthApplicableDomainValidation
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthCounterexample)
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain))
validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
    evaluationLimits inputBoxLimits unionLimits problem = do
  let inputCount = checkedLengthProblemInputCount problem
      maximumInputs = lengthInputBoxInputLimit inputBoxLimits
  if inputCount <= maximumInputs
    then pure ()
    else Left $ LengthBooleanFiniteUnionProblemInputLimitExceeded
      maximumInputs inputCount
  coverage <- either (Left . preparationError) Right
    $ booleanFiniteUnionAtomicBranchingApplicableDomainMaximumBoxes
        unionLimits inputCount scalarInputPosition
        $ checkedLengthProblemPrecondition problem
  case coverage of
    Left inapplicability -> Right
      $ LengthApplicableDomainInapplicable inapplicability
    Right boxes -> do
      mapM_ checkBox $ zip [0 ..] boxes
      visits <- either (Left . enumerationError) Right
        $ booleanFiniteUnionAssignmentVisitCount unionLimits boxes
      (assignmentCount, assignments) <- either
        (Left . enumerationError) Right
        $ enumerateBooleanFiniteUnionAssignments inputBoxLimits boxes
      replay boxes visits assignmentCount 0 0 $ Set.toAscList assignments
 where
  scalarInputPosition variable = case variable of
    LengthInput position -> Just position
    LengthResult -> Nothing

  preparationError failure = case failure of
    BooleanFiniteUnionGeneratedBranchLimitExceeded limit observed ->
      LengthBooleanFiniteUnionGeneratedBranchLimitExceeded limit observed
    BooleanFiniteUnionRuleLimitExceeded branch limit observed ->
      LengthBooleanFiniteUnionRuleLimitExceeded branch limit observed
    BooleanFiniteUnionClosureInspectionLimitExceeded branch limit observed ->
      LengthBooleanFiniteUnionClosureInspectionLimitExceeded
        branch limit observed
    BooleanFiniteUnionRetainedBoxLimitExceeded limit observed ->
      LengthBooleanFiniteUnionRetainedBoxLimitExceeded limit observed

  enumerationError failure = case failure of
    BooleanFiniteUnionAssignmentVisitLimitExceeded limit observed ->
      LengthBooleanFiniteUnionAssignmentVisitLimitExceeded limit observed
    BooleanFiniteUnionAssignmentLimitExceeded limit observed ->
      LengthBooleanFiniteUnionAssignmentLimitExceeded limit observed
    BooleanFiniteUnionInternalEnumerationInvariant ->
      LengthBooleanFiniteUnionInternalEnumerationInvariant

  checkBox (boxIndex, maximums) =
    mapM_ (checkMaximum boxIndex) $ zip [0 ..] maximums

  checkMaximum boxIndex (inputIndex, maximumValue) = either
    (Left . LengthBooleanFiniteUnionMaximumValueRejected
      boxIndex inputIndex)
    Right
    $ checkAssignedValue evaluationLimits
        (LengthProblemInputValue inputIndex) maximumValue

  replay boxes visits assignmentCount !ordinal !applicable assignments =
    case assignments of
      []
        | ordinal /= assignmentCount ->
            Left LengthBooleanFiniteUnionInternalEnumerationInvariant
        | otherwise ->
            let receipt =
                  ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainReceipt
                    lengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainValidationSchemaTag
                    boxes visits assignmentCount applicable
                    $ problemBasis problem
            in Right $ LengthApplicableDomainEstablished
              $ mkBehavioralEvidence
                  (checkedLengthProblemBehavioralProblem problem) receipt
      inputs : remaining -> do
        assignmentReplay <- either
          (Left . LengthBooleanFiniteUnionAssignmentEvaluationRejected ordinal)
          Right
          $ replayLengthProblemAssignment evaluationLimits problem
          $ LengthProblemAssignment inputs
        case assignmentReplay of
          LengthProblemPostconditionViolated receipt -> Right
            $ LengthApplicableDomainCounterexample
            $ mkBehavioralEvidence
                (checkedLengthProblemBehavioralProblem problem) receipt
          LengthProblemPreconditionNotMet ->
            replay boxes visits assignmentCount (ordinal + 1) applicable
              remaining
          LengthProblemPostconditionSatisfied ->
            replay boxes visits assignmentCount (ordinal + 1)
              (applicable + 1) remaining

-- | Nominal binary-product sibling of the atomic-branching finite-union
-- validator, with the same cap and replay precedence and product-specific
-- errors and evidence.
validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> LengthBooleanFiniteUnionLimits
  -> CheckedLengthSpinePairProblem identity local
  -> Either
      LengthSpinePairBooleanFiniteUnionApplicableDomainValidationError
      (LengthApplicableDomainValidation
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairCounterexample)
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain))
validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
    evaluationLimits inputBoxLimits unionLimits problem = do
  let inputCount = checkedLengthSpinePairProblemInputCount problem
      maximumInputs = lengthInputBoxInputLimit inputBoxLimits
  if inputCount <= maximumInputs
    then pure ()
    else Left $ LengthSpinePairBooleanFiniteUnionProblemInputLimitExceeded
      maximumInputs inputCount
  coverage <- either (Left . preparationError) Right
    $ booleanFiniteUnionAtomicBranchingApplicableDomainMaximumBoxes
        unionLimits inputCount spinePairInputPosition
        $ checkedLengthSpinePairProblemPrecondition problem
  case coverage of
    Left inapplicability -> Right
      $ LengthApplicableDomainInapplicable inapplicability
    Right boxes -> do
      mapM_ checkBox $ zip [0 ..] boxes
      visits <- either (Left . enumerationError) Right
        $ booleanFiniteUnionAssignmentVisitCount unionLimits boxes
      (assignmentCount, assignments) <- either
        (Left . enumerationError) Right
        $ enumerateBooleanFiniteUnionAssignments inputBoxLimits boxes
      replay boxes visits assignmentCount 0 0 $ Set.toAscList assignments
 where
  spinePairInputPosition variable = case variable of
    LengthSpinePairInput position -> Just position
    LengthSpinePairResult _ -> Nothing

  preparationError failure = case failure of
    BooleanFiniteUnionGeneratedBranchLimitExceeded limit observed ->
      LengthSpinePairBooleanFiniteUnionGeneratedBranchLimitExceeded
        limit observed
    BooleanFiniteUnionRuleLimitExceeded branch limit observed ->
      LengthSpinePairBooleanFiniteUnionRuleLimitExceeded
        branch limit observed
    BooleanFiniteUnionClosureInspectionLimitExceeded branch limit observed ->
      LengthSpinePairBooleanFiniteUnionClosureInspectionLimitExceeded
        branch limit observed
    BooleanFiniteUnionRetainedBoxLimitExceeded limit observed ->
      LengthSpinePairBooleanFiniteUnionRetainedBoxLimitExceeded
        limit observed

  enumerationError failure = case failure of
    BooleanFiniteUnionAssignmentVisitLimitExceeded limit observed ->
      LengthSpinePairBooleanFiniteUnionAssignmentVisitLimitExceeded
        limit observed
    BooleanFiniteUnionAssignmentLimitExceeded limit observed ->
      LengthSpinePairBooleanFiniteUnionAssignmentLimitExceeded limit observed
    BooleanFiniteUnionInternalEnumerationInvariant ->
      LengthSpinePairBooleanFiniteUnionInternalEnumerationInvariant

  checkBox (boxIndex, maximums) =
    mapM_ (checkMaximum boxIndex) $ zip [0 ..] maximums

  checkMaximum boxIndex (inputIndex, maximumValue) = either
    (Left . LengthSpinePairBooleanFiniteUnionMaximumValueRejected
      boxIndex inputIndex)
    Right
    $ checkSpinePairAssignedValue evaluationLimits
        (LengthSpinePairProblemInputValue inputIndex) maximumValue

  replay boxes visits assignmentCount !ordinal !applicable assignments =
    case assignments of
      []
        | ordinal /= assignmentCount -> Left
            LengthSpinePairBooleanFiniteUnionInternalEnumerationInvariant
        | otherwise ->
            let receipt =
                  ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainReceipt
                    lengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainValidationSchemaTag
                    boxes visits assignmentCount applicable
                    $ spinePairProblemBasis problem
            in Right $ LengthApplicableDomainEstablished
              $ mkBehavioralEvidence
                  (checkedLengthSpinePairProblemBehavioralProblem problem)
                  receipt
      inputs : remaining -> do
        assignmentReplay <- either
          (Left .
            LengthSpinePairBooleanFiniteUnionAssignmentEvaluationRejected
              ordinal)
          Right
          $ replayLengthSpinePairProblemAssignment evaluationLimits problem
          $ LengthProblemAssignment inputs
        case assignmentReplay of
          LengthSpinePairProblemPostconditionViolated receipt -> Right
            $ LengthApplicableDomainCounterexample
            $ mkBehavioralEvidence
                (checkedLengthSpinePairProblemBehavioralProblem problem)
                receipt
          LengthSpinePairProblemPreconditionNotMet ->
            replay boxes visits assignmentCount (ordinal + 1) applicable
              remaining
          LengthSpinePairProblemPostconditionSatisfied ->
            replay boxes visits assignmentCount (ordinal + 1) (applicable + 1)
              remaining

data BooleanFiniteUnionPolarity
  = BooleanFiniteUnionPositive
  | BooleanFiniteUnionNegative

data BooleanFiniteUnionPreparationError
  = BooleanFiniteUnionGeneratedBranchLimitExceeded !Int !Int
  | BooleanFiniteUnionRuleLimitExceeded !Int !Int !Int
  | BooleanFiniteUnionClosureInspectionLimitExceeded !Int !Int !Int
  | BooleanFiniteUnionRetainedBoxLimitExceeded !Int !Int

data BooleanFiniteUnionEnumerationError
  = BooleanFiniteUnionAssignmentVisitLimitExceeded !Int !Int
  | BooleanFiniteUnionAssignmentLimitExceeded !Natural !Natural
  | BooleanFiniteUnionInternalEnumerationInvariant

-- The checked formula is already normalized and structurally bounded.  This
-- private expansion preserves exact Boolean meaning: positive conjunction is
-- Cartesian conjunction, negative conjunction is union, and negative equality
-- is the exact natural/order split @not (A <= B) || not (B <= A)@.  Boolean
-- syntax inside an expression-level conditional is deliberately never opened.
booleanFiniteUnionRawBranches
  :: LengthFormula variable
  -> [[LengthFormula variable]]
booleanFiniteUnionRawBranches = expand BooleanFiniteUnionPositive
 where
  expand polarity formula = case formula of
    LengthTruth value
      | value == positive polarity -> [[]]
      | otherwise -> []
    LengthNot nested -> expand (opposite polarity) nested
    LengthAll formulas -> case polarity of
      BooleanFiniteUnionPositive -> conjoin formulas
      BooleanFiniteUnionNegative ->
        concatMap (expand BooleanFiniteUnionNegative) formulas
    LengthAtMost left right -> case polarity of
      BooleanFiniteUnionPositive -> [[LengthAtMost left right]]
      BooleanFiniteUnionNegative ->
        [[LengthNot $ LengthAtMost left right]]
    LengthEqual left right -> case polarity of
      BooleanFiniteUnionPositive -> [[LengthEqual left right]]
      BooleanFiniteUnionNegative ->
        [ [LengthNot $ LengthAtMost left right]
        , [LengthNot $ LengthAtMost right left]
        ]

  conjoin [] = [[]]
  conjoin (formula : remaining) =
    [ first ++ rest
    | first <- expand BooleanFiniteUnionPositive formula
    , rest <- conjoin remaining
    ]

  positive polarity = case polarity of
    BooleanFiniteUnionPositive -> True
    BooleanFiniteUnionNegative -> False

  opposite polarity = case polarity of
    BooleanFiniteUnionPositive -> BooleanFiniteUnionNegative
    BooleanFiniteUnionNegative -> BooleanFiniteUnionPositive

-- Literal order is inherited from the normalized AST's Ord instance.  Exact
-- complements discard a branch; duplicate branches and branches strictly
-- stronger than another branch are then removed without changing the union.
canonicalBooleanFiniteUnionBranches
  :: Ord variable
  => [[LengthFormula variable]]
  -> [Set.Set (LengthFormula variable)]
canonicalBooleanFiniteUnionBranches rawBranches =
  filter notStrictlySubsumed uniqueConsistent
 where
  uniqueConsistent = Set.toAscList $ Set.fromList
    [ branch
    | rawBranch <- rawBranches
    , let branch = Set.fromList rawBranch
    , not $ hasExactComplement branch
    ]

  hasExactComplement branch = any hasComplement $ Set.toAscList branch
   where
    hasComplement literal = case literal of
      LengthNot nested -> Set.member nested branch
      _ -> Set.member (LengthNot literal) branch

  notStrictlySubsumed branch = not $ any isStrictSubset uniqueConsistent
   where
    isStrictSubset candidate =
      candidate /= branch && Set.isSubsetOf candidate branch

-- Formula-level Boolean expansion and atomic proof expansion form one lazy
-- Cartesian witness stream.  Its elements carry no reconstructed syntax or
-- proof payload: they exist only so the public generated-branch cap observes
-- the complete formula-by-atomic product before formula canonicalization.
booleanFiniteUnionAtomicBranchingRawBranchWitnesses
  :: Int
  -> (variable -> Maybe Natural)
  -> LengthFormula variable
  -> [()]
booleanFiniteUnionAtomicBranchingRawBranchWitnesses
    inputCount inputPosition precondition =
  concatMap expandBranch $ booleanFiniteUnionRawBranches precondition
 where
  expandBranch [] = [()]
  expandBranch (literal : remaining) =
    [ ()
    | _ <- strictRelationalPositiveAffineQuotientRootExtremaMonusAtomicBranchingClauseBranches
            inputCount inputPosition literal
    , _ <- expandBranch remaining
    ]

-- After the raw-product cap succeeds, the original formula branches retain
-- their exact predecessor complement/deduplication/subsumption order.  Each
-- surviving Set-ordered literal is then expanded into its proof alternatives;
-- Ignored and Contradiction remain explicit coverage values.
expandBooleanFiniteUnionAtomicBranchingBranch
  :: Int
  -> (variable -> Maybe Natural)
  -> Set.Set (LengthFormula variable)
  -> [[RelationalPositiveAffineClauseCoverage]]
expandBooleanFiniteUnionAtomicBranchingBranch
    inputCount inputPosition = expand . Set.toAscList
 where
  expand [] = [[]]
  expand (literal : remaining) =
    [ coverage : rest
    | coverage <-
        strictRelationalPositiveAffineQuotientRootExtremaMonusAtomicBranchingClauseBranches
          inputCount inputPosition literal
    , rest <- expand remaining
    ]

-- Atomic-branching sibling of the published Boolean finite-union preparation
-- pipeline.  Every downstream cap and precedence edge is inherited literally;
-- only raw branch construction and branch-local proof-rule collection differ.
booleanFiniteUnionAtomicBranchingApplicableDomainMaximumBoxes
  :: Ord variable
  => LengthBooleanFiniteUnionLimits
  -> Int
  -> (variable -> Maybe Natural)
  -> LengthFormula variable
  -> Either
      BooleanFiniteUnionPreparationError
      (Either LengthApplicableDomainInapplicability [[Natural]])
booleanFiniteUnionAtomicBranchingApplicableDomainMaximumBoxes
    limits inputCount inputPosition precondition = do
  let rawFormulaBranches = booleanFiniteUnionRawBranches precondition
      rawBranchWitnesses = booleanFiniteUnionAtomicBranchingRawBranchWitnesses
        inputCount inputPosition precondition
      branchLimit = lengthBooleanFiniteUnionGeneratedBranchLimit limits
  case observeBooleanFiniteUnionListLength branchLimit rawBranchWitnesses of
    Left observed -> Left $ BooleanFiniteUnionGeneratedBranchLimitExceeded
      branchLimit observed
    Right _ -> pure ()
  let branches = concatMap
        (expandBooleanFiniteUnionAtomicBranchingBranch
          inputCount inputPosition)
        $ canonicalBooleanFiniteUnionBranches rawFormulaBranches
  closed <- mapM closeBranch $ zip [0 ..] branches
  let liveBounds = [bounds | Just bounds <- closed]
  case firstMissingInput liveBounds of
    Just missing -> pure $ Left
      $ LengthApplicableDomainInputUpperBoundMissing missing
    Nothing -> do
      let boxes = canonicalBooleanFiniteUnionBoxes inputCount liveBounds
          boxLimit = lengthBooleanFiniteUnionRetainedBoxLimit limits
      case observeBooleanFiniteUnionListLength boxLimit boxes of
        Left observed -> Left $ BooleanFiniteUnionRetainedBoxLimitExceeded
          boxLimit observed
        Right _ -> pure $ Right boxes
 where
  closeBranch (branchIndex, branch) = do
    collected <- collectBranchRules branchIndex branch
    case collected of
      Nothing -> pure Nothing
      Just rules -> case closeRelationalPositiveAffineRulesWithin
          (lengthBooleanFiniteUnionClosureInspectionLimitPerBranch limits)
          rules of
        Left observed -> Left
          $ BooleanFiniteUnionClosureInspectionLimitExceeded
              branchIndex
              (lengthBooleanFiniteUnionClosureInspectionLimitPerBranch limits)
              observed
        Right RelationalPositiveAffineClosureContradiction -> pure Nothing
        Right (RelationalPositiveAffineClosureBounds bounds) ->
          pure $ Just bounds

  collectBranchRules branchIndex = go 0 []
   where
    ruleLimit = lengthBooleanFiniteUnionRuleLimitPerBranch limits

    go !_ retained [] = Right $ Just retained
    go !count retained (coverage : remaining) = case coverage of
      RelationalPositiveAffineClauseIgnored -> go count retained remaining
      RelationalPositiveAffineClauseContradiction -> Right Nothing
      RelationalPositiveAffineClauseRules rules ->
        let newRuleCount = length rules
        in if newRuleCount > ruleLimit - count
            then Left $ BooleanFiniteUnionRuleLimitExceeded
              branchIndex ruleLimit $ saturatedSuccessor ruleLimit
            else go (count + newRuleCount) (retained ++ rules) remaining

  firstMissingInput liveBounds = firstMissing 0
   where
    firstMissing index
      | index >= inputCount = Nothing
      | any (Map.notMember $ fromIntegral index) liveBounds = Just index
      | otherwise = firstMissing $ index + 1

-- Expand, canonicalize, close, and antichain one formula before any maximum
-- value or assignment is demanded.  All branches finish bounded closure before
-- missing input coverage is inspected, giving operational cap errors fixed
-- precedence over ordinary inapplicability.
booleanFiniteUnionApplicableDomainMaximumBoxes
  :: Ord variable
  => LengthBooleanFiniteUnionLimits
  -> Int
  -> (variable -> Maybe Natural)
  -> LengthFormula variable
  -> Either
      BooleanFiniteUnionPreparationError
      (Either LengthApplicableDomainInapplicability [[Natural]])
booleanFiniteUnionApplicableDomainMaximumBoxes
    limits inputCount inputPosition precondition = do
  let rawBranches = booleanFiniteUnionRawBranches precondition
      branchLimit = lengthBooleanFiniteUnionGeneratedBranchLimit limits
  case observeBooleanFiniteUnionListLength branchLimit rawBranches of
    Left observed -> Left $ BooleanFiniteUnionGeneratedBranchLimitExceeded
      branchLimit observed
    Right _ -> pure ()
  let branches = canonicalBooleanFiniteUnionBranches rawBranches
  closed <- mapM closeBranch $ zip [0 ..] branches
  let liveBounds = [bounds | Just bounds <- closed]
  case firstMissingInput liveBounds of
    Just missing -> pure $ Left
      $ LengthApplicableDomainInputUpperBoundMissing missing
    Nothing -> do
      let boxes = canonicalBooleanFiniteUnionBoxes inputCount liveBounds
          boxLimit = lengthBooleanFiniteUnionRetainedBoxLimit limits
      case observeBooleanFiniteUnionListLength boxLimit boxes of
        Left observed -> Left $ BooleanFiniteUnionRetainedBoxLimitExceeded
          boxLimit observed
        Right _ -> pure $ Right boxes
 where
  closeBranch (branchIndex, branch) = do
    collected <- collectBranchRules branchIndex $ Set.toAscList branch
    case collected of
      Nothing -> pure Nothing
      Just rules -> case closeRelationalPositiveAffineRulesWithin
          (lengthBooleanFiniteUnionClosureInspectionLimitPerBranch limits)
          rules of
        Left observed -> Left
          $ BooleanFiniteUnionClosureInspectionLimitExceeded
              branchIndex
              (lengthBooleanFiniteUnionClosureInspectionLimitPerBranch limits)
              observed
        Right RelationalPositiveAffineClosureContradiction -> pure Nothing
        Right (RelationalPositiveAffineClosureBounds bounds) ->
          pure $ Just bounds

  collectBranchRules branchIndex = go 0 []
   where
    ruleLimit = lengthBooleanFiniteUnionRuleLimitPerBranch limits

    go !_ retained [] = Right $ Just retained
    go !count retained (literal : remaining) = case
        strictRelationalPositiveAffineQuotientRootExtremaMonusClauseCoverage
          inputCount inputPosition literal of
      RelationalPositiveAffineClauseIgnored -> go count retained remaining
      RelationalPositiveAffineClauseContradiction -> Right Nothing
      RelationalPositiveAffineClauseRules rules ->
        let newRuleCount = length rules
        in if newRuleCount > ruleLimit - count
            then Left $ BooleanFiniteUnionRuleLimitExceeded
              branchIndex ruleLimit $ saturatedSuccessor ruleLimit
            else go (count + newRuleCount) (retained ++ rules) remaining

  firstMissingInput liveBounds = firstMissing 0
   where
    firstMissing index
      | index >= inputCount = Nothing
      | any (Map.notMember $ fromIntegral index) liveBounds = Just index
      | otherwise = firstMissing $ index + 1

-- Unlike a saturated numeric observation alone, this helper retains a
-- separate exceeded arm when the admitted limit itself is maxBound.  It
-- therefore never mistakes a cap+1 list for an admitted maxBound-length list.
observeBooleanFiniteUnionListLength :: Int -> [value] -> Either Int Int
observeBooleanFiniteUnionListLength limit = go 0
 where
  go !observed remaining = case remaining of
    [] -> Right observed
    _ : following
      | observed >= limit -> Left $ saturatedSuccessor limit
      | otherwise -> go (observed + 1) following

-- Keep only the componentwise-maximal zero-origin boxes.  This operation is
-- exact for a union of boxes and intentionally never constructs the
-- componentwise hull of incomparable maxima vectors.
canonicalBooleanFiniteUnionBoxes
  :: Int
  -> [Map.Map Natural Natural]
  -> [[Natural]]
canonicalBooleanFiniteUnionBoxes inputCount bounds =
  filter notContained uniqueBoxes
 where
  uniqueBoxes = Set.toAscList $ Set.fromList
    [ [Map.findWithDefault 0 (fromIntegral index) bound
      | index <- [0 .. inputCount - 1]
      ]
    | bound <- bounds
    ]

  notContained box = not $ any strictlyContains uniqueBoxes
   where
    strictlyContains candidate =
      candidate /= box && and (zipWith (<=) box candidate)

-- Bounded sibling of the predecessor closure.  It preserves seed partition,
-- canonical rule order, immutable-snapshot passes, and eligible-rule-once
-- removal exactly; the additive counter observes each attempted rule in a
-- closure pass and fails before attempt cap+1.
closeRelationalPositiveAffineRulesWithin
  :: Int
  -> [RelationalPositiveAffineRule]
  -> Either Int RelationalPositiveAffineClosure
closeRelationalPositiveAffineRulesWithin inspectionLimit rules = do
  let (seedRules, pendingRules) =
        partitionRelationalPositiveAffineRules rules
  (seedInspections, seedPass) <- relationalPositiveAffineRulePassWithin
    inspectionLimit 0 Map.empty seedRules
  case seedPass of
    RelationalPositiveAffineRulePassContradiction ->
      pure RelationalPositiveAffineClosureContradiction
    RelationalPositiveAffineRulePassComplete seedBounds retainedSeeds _ ->
      close seedInspections seedBounds $ retainedSeeds ++ pendingRules
 where
  close !_ !bounds [] = pure $ RelationalPositiveAffineClosureBounds bounds
  close !inspections !bounds pending = do
    (nextInspections, pass) <- relationalPositiveAffineRulePassWithin
      inspectionLimit inspections bounds pending
    case pass of
      RelationalPositiveAffineRulePassContradiction ->
        pure RelationalPositiveAffineClosureContradiction
      RelationalPositiveAffineRulePassComplete nextBounds retained fired
        | fired -> close nextInspections nextBounds retained
        | otherwise -> pure $ RelationalPositiveAffineClosureBounds nextBounds

relationalPositiveAffineRulePassWithin
  :: Int
  -> Int
  -> Map.Map Natural Natural
  -> [RelationalPositiveAffineRule]
  -> Either Int (Int, RelationalPositiveAffineRulePass)
relationalPositiveAffineRulePassWithin inspectionLimit = go
 where
  go !inspections !bounds rules = scan inspections Map.empty [] False rules
   where
    scan !observed !derived !retained !fired remaining = case remaining of
      [] -> Right
        ( observed
        , RelationalPositiveAffineRulePassComplete
            (Map.unionWith min bounds derived)
            (reverse retained)
            fired
        )
      rule : following
        | observed >= inspectionLimit ->
            Left $ saturatedSuccessor inspectionLimit
        | otherwise -> case rule of
            RelationalPositiveAffineRule leftConstant leftCoefficients
                rightConstant rightCoefficients ->
              let nextObserved = observed + 1
              in case relationalPositiveAffineRightMaximum
                  bounds rightConstant rightCoefficients of
                Nothing -> scan nextObserved derived (rule : retained) fired
                  following
                Just rightMaximum
                  | leftConstant > rightMaximum -> Right
                      ( nextObserved
                      , RelationalPositiveAffineRulePassContradiction
                      )
                  | otherwise ->
                      let numerator = rightMaximum - leftConstant
                          ruleBounds =
                            Map.map (numerator `quot`) leftCoefficients
                          nextDerived = Map.unionWith min derived ruleBounds
                      in scan nextObserved nextDerived retained True following

-- Check the sum of retained box cardinalities without constructing a single
-- assignment.  This is intentionally a visit count: overlaps are counted once
-- per box and are deduplicated only by the later union set.
booleanFiniteUnionAssignmentVisitCount
  :: LengthBooleanFiniteUnionLimits
  -> [[Natural]]
  -> Either BooleanFiniteUnionEnumerationError Natural
booleanFiniteUnionAssignmentVisitCount limits = foldM addBox 0
 where
  visitLimit = lengthBooleanFiniteUnionAssignmentVisitLimit limits
  naturalLimit = fromIntegral visitLimit
  exceeded = saturatedSuccessor visitLimit

  addBox !total maximums = do
    boxCount <- productWithin 1 maximums
    if boxCount > naturalLimit - total
      then Left $ BooleanFiniteUnionAssignmentVisitLimitExceeded
        visitLimit exceeded
      else pure $ total + boxCount

  productWithin !total [] = pure total
  productWithin !total (maximumValue : remaining) =
    let factor = maximumValue + 1
    in if factor > 0 && total > naturalLimit `quot` factor
        then Left $ BooleanFiniteUnionAssignmentVisitLimitExceeded
          visitLimit exceeded
        else productWithin (total * factor) remaining

-- Materialize the exact set union under the existing unique-assignment cap.
-- Each component box is visited lexicographically with the last source input
-- varying fastest; Set order later supplies one global lexicographic replay.
enumerateBooleanFiniteUnionAssignments
  :: LengthInputBoxLimits
  -> [[Natural]]
  -> Either
      BooleanFiniteUnionEnumerationError
      (Natural, Set.Set [Natural])
enumerateBooleanFiniteUnionAssignments inputBoxLimits =
  fmap swapCount . foldM enumerateBox (Set.empty, 0)
 where
  assignmentLimit = lengthInputBoxAssignmentLimit inputBoxLimits

  swapCount (assignments, count) = (count, assignments)

  enumerateBox (!assignments, !count) maximums =
    visit assignments count $ replicate (length maximums) 0
   where
    visit !retained !retainedCount values = do
      (nextRetained, nextCount) <- insertAssignment
        retained retainedCount values
      following <- nextBooleanFiniteUnionAssignment maximums values
      case following of
        Nothing -> pure (nextRetained, nextCount)
        Just next -> visit nextRetained nextCount next

  insertAssignment retained retainedCount values
    | Set.member values retained = pure (retained, retainedCount)
    | retainedCount >= assignmentLimit = Left
        $ BooleanFiniteUnionAssignmentLimitExceeded
            assignmentLimit (assignmentLimit + 1)
    | otherwise = pure
        (Set.insert values retained, retainedCount + 1)

nextBooleanFiniteUnionAssignment
  :: [Natural]
  -> [Natural]
  -> Either BooleanFiniteUnionEnumerationError (Maybe [Natural])
nextBooleanFiniteUnionAssignment maximums values =
  fmap (fmap reverse) $ advance (reverse maximums) (reverse values)
 where
  advance [] [] = Right Nothing
  advance (maximumValue : remainingMaximums)
      (value : remainingValues)
    | value < maximumValue = Right $ Just $ value + 1 : remainingValues
    | otherwise = fmap (fmap (0 :))
        $ advance remainingMaximums remainingValues
  advance _ _ = Left BooleanFiniteUnionInternalEnumerationInvariant

data PositiveAffineCoverage
  = PositiveAffineCoverageBounds !(Map.Map Natural Natural)
  | PositiveAffineCoverageContradiction

data PositiveAffineClauseCoverage
  = PositiveAffineClauseIgnored
  | PositiveAffineClauseBounds !(Map.Map Natural Natural)
  | PositiveAffineClauseContradiction

data PositiveAffineSummary = PositiveAffineSummary
  !Natural
  !(Map.Map Natural Natural)

-- The checked precondition is already structurally bounded and normalized.
-- Nullary validation deliberately avoids demanding it here and delegates the
-- singleton assignment directly to the box verifier.  For nonnullary problems
-- the complete canonical clause list is scanned before a missing bound is
-- reported, unless a prior clause proves the whole conjunction contradictory.
positiveAffineApplicableDomainMaximums
  :: Int
  -> (variable -> Maybe Natural)
  -> LengthFormula variable
  -> Either LengthApplicableDomainInapplicability [Natural]
positiveAffineApplicableDomainMaximums inputCount inputPosition precondition
  | inputCount == 0 = Right []
  | otherwise = case collect Map.empty clauses of
      PositiveAffineCoverageContradiction ->
        Right $ replicate inputCount 0
      PositiveAffineCoverageBounds bounds ->
        mapM (maximumFor bounds) [0 .. inputCount - 1]
 where
  clauses = case precondition of
    LengthAll formulas -> formulas
    formula -> [formula]

  collect !retained [] = PositiveAffineCoverageBounds retained
  collect !retained (formula : remaining) =
    case positiveAffineClauseCoverage inputCount inputPosition formula of
      PositiveAffineClauseIgnored -> collect retained remaining
      PositiveAffineClauseBounds bounds ->
        collect (Map.unionWith min retained bounds) remaining
      PositiveAffineClauseContradiction ->
        PositiveAffineCoverageContradiction

  maximumFor bounds index = case Map.lookup (fromIntegral index) bounds of
    Just maximumValue -> Right maximumValue
    Nothing -> Left $ LengthApplicableDomainInputUpperBoundMissing index

positiveAffineClauseCoverage
  :: Int
  -> (variable -> Maybe Natural)
  -> LengthFormula variable
  -> PositiveAffineClauseCoverage
positiveAffineClauseCoverage inputCount inputPosition formula = case formula of
  LengthTruth False -> PositiveAffineClauseContradiction
  LengthAtMost expression (LengthLiteral maximumValue) ->
    boundedExpression False expression maximumValue
  LengthEqual expression (LengthLiteral maximumValue) ->
    boundedExpression True expression maximumValue
  LengthEqual (LengthLiteral maximumValue) expression ->
    boundedExpression True expression maximumValue
  _ -> PositiveAffineClauseIgnored
 where
  boundedExpression isEquality expression maximumValue =
    case summarizePositiveAffineExpression
        maximumValue inputCount inputPosition expression of
      Nothing -> PositiveAffineClauseIgnored
      Just (PositiveAffineSummary constant coefficients)
        | constant > maximumValue -> PositiveAffineClauseContradiction
        | isEquality && Map.null coefficients && constant /= maximumValue ->
            PositiveAffineClauseContradiction
        | otherwise -> PositiveAffineClauseBounds
          $ Map.map
              ((maximumValue - constant) `quot`)
              coefficients

-- Summaries are saturated at one greater than the atom's literal ceiling.
-- Saturation preserves both contradiction detection and every derived quotient
-- because the remaining numerator is strictly below that cap.
summarizePositiveAffineExpression
  :: Natural
  -> Int
  -> (variable -> Maybe Natural)
  -> LengthExpression variable
  -> Maybe PositiveAffineSummary
summarizePositiveAffineExpression maximumValue inputCount inputPosition = go
 where
  cap = maximumValue + 1

  go expression = case expression of
    LengthVariable variable -> do
      position <- inputPosition variable
      if position < fromIntegral inputCount
        then Just $ PositiveAffineSummary 0 $ Map.singleton position 1
        else Nothing
    LengthLiteral value -> Just $ PositiveAffineSummary (min cap value) Map.empty
    LengthSum terms -> foldM add (PositiveAffineSummary 0 Map.empty) terms
    LengthScale factor nested
      | factor == 0 -> Nothing
      | otherwise -> scale factor <$> go nested
    _ -> Nothing

  add (PositiveAffineSummary leftConstant leftCoefficients) term = do
    PositiveAffineSummary rightConstant rightCoefficients <- go term
    pure $ PositiveAffineSummary
      (saturatingNaturalAdd cap leftConstant rightConstant)
      (Map.unionWith
        (saturatingNaturalAdd cap)
        leftCoefficients rightCoefficients)

  scale factor (PositiveAffineSummary constant coefficients) =
    PositiveAffineSummary
      (saturatingNaturalMultiply cap factor constant)
      (Map.map (saturatingNaturalMultiply cap factor) coefficients)

saturatingNaturalAdd :: Natural -> Natural -> Natural -> Natural
saturatingNaturalAdd cap left right = min cap $ left + right

saturatingNaturalMultiply :: Natural -> Natural -> Natural -> Natural
saturatingNaturalMultiply cap left right = min cap $ left * right

data RelationalPositiveAffineRuleCollection
  = RelationalPositiveAffineRuleCollection
      ![RelationalPositiveAffineRule]
  | RelationalPositiveAffineRuleCollectionContradiction

data RelationalPositiveAffineClauseCoverage
  = RelationalPositiveAffineClauseIgnored
  | RelationalPositiveAffineClauseRules
      ![RelationalPositiveAffineRule]
  | RelationalPositiveAffineClauseContradiction

data RelationalPositiveAffineClosure
  = RelationalPositiveAffineClosureBounds !(Map.Map Natural Natural)
  | RelationalPositiveAffineClosureContradiction

data RelationalPositiveAffineRulePass
  = RelationalPositiveAffineRulePassComplete
      !(Map.Map Natural Natural)
      ![RelationalPositiveAffineRule]
      !Bool
  | RelationalPositiveAffineRulePassContradiction

data RelationalPositiveAffineSummary = RelationalPositiveAffineSummary
  !Natural
  !(Map.Map Natural Natural)

-- Both sides have already been summarized and algebraically canceled.  This
-- subtraction is ordinary equality-preserving cancellation over naturals; it
-- is unrelated to the object language's saturating 'LengthMonus'.
data RelationalPositiveAffineRule = RelationalPositiveAffineRule
  !Natural
  !(Map.Map Natural Natural)
  !Natural
  !(Map.Map Natural Natural)

-- The checked precondition is bounded and normalized.  Nullary validation
-- deliberately bypasses extraction and delegates its singleton assignment to
-- the existing box verifier.  For nonnullary problems, exact affine summaries
-- are collected from both sides of top-level relations.  Equality contributes
-- both directed inequalities.
relationalPositiveAffineApplicableDomainMaximums
  :: Int
  -> (variable -> Maybe Natural)
  -> LengthFormula variable
  -> Either LengthApplicableDomainInapplicability [Natural]
relationalPositiveAffineApplicableDomainMaximums
    inputCount inputPosition precondition
  | inputCount == 0 = Right []
  | otherwise = case collect [] clauses of
      RelationalPositiveAffineRuleCollectionContradiction ->
        Right $ replicate inputCount 0
      RelationalPositiveAffineRuleCollection reversedRules ->
        case closeRelationalPositiveAffineRules $ reverse reversedRules of
          RelationalPositiveAffineClosureContradiction ->
            Right $ replicate inputCount 0
          RelationalPositiveAffineClosureBounds bounds ->
            mapM (maximumFor bounds) [0 .. inputCount - 1]
 where
  clauses = case precondition of
    LengthAll formulas -> formulas
    formula -> [formula]

  collect !retained [] = RelationalPositiveAffineRuleCollection retained
  collect !retained (formula : remaining) =
    case relationalPositiveAffineClauseCoverage
        inputCount inputPosition formula of
      RelationalPositiveAffineClauseIgnored -> collect retained remaining
      RelationalPositiveAffineClauseRules rules ->
        collect (prependRulesInReverse retained rules) remaining
      RelationalPositiveAffineClauseContradiction ->
        RelationalPositiveAffineRuleCollectionContradiction

  prependRulesInReverse !retained [] = retained
  prependRulesInReverse !retained (rule : remaining) =
    prependRulesInReverse (rule : retained) remaining

  maximumFor bounds index = case Map.lookup (fromIntegral index) bounds of
    Just maximumValue -> Right maximumValue
    Nothing -> Left $ LengthApplicableDomainInputUpperBoundMissing index

relationalPositiveAffineClauseCoverage
  :: Int
  -> (variable -> Maybe Natural)
  -> LengthFormula variable
  -> RelationalPositiveAffineClauseCoverage
relationalPositiveAffineClauseCoverage
    inputCount inputPosition formula = case formula of
  LengthTruth False -> RelationalPositiveAffineClauseContradiction
  LengthAtMost left right -> case summarizeBoth left right of
    Nothing -> RelationalPositiveAffineClauseIgnored
    Just (leftSummary, rightSummary) ->
      RelationalPositiveAffineClauseRules
        [relationalPositiveAffineRule leftSummary rightSummary]
  LengthEqual left right -> case summarizeBoth left right of
    Nothing -> RelationalPositiveAffineClauseIgnored
    Just (leftSummary, rightSummary) ->
      RelationalPositiveAffineClauseRules
        [ relationalPositiveAffineRule leftSummary rightSummary
        , relationalPositiveAffineRule rightSummary leftSummary
        ]
  _ -> RelationalPositiveAffineClauseIgnored
 where
  summarizeBoth left right = do
    leftSummary <- summarizeRelationalPositiveAffineExpression
      inputCount inputPosition left
    rightSummary <- summarizeRelationalPositiveAffineExpression
      inputCount inputPosition right
    pure (leftSummary, rightSummary)

-- The strict sibling deliberately leaves the established relational scanner
-- untouched.  It traverses the same normalized top-level conjunction and
-- adds only the exact natural complement of one immediate at-most clause.
strictRelationalPositiveAffineApplicableDomainMaximums
  :: Int
  -> (variable -> Maybe Natural)
  -> LengthFormula variable
  -> Either LengthApplicableDomainInapplicability [Natural]
strictRelationalPositiveAffineApplicableDomainMaximums
    inputCount inputPosition precondition
  | inputCount == 0 = Right []
  | otherwise = case collect [] clauses of
      RelationalPositiveAffineRuleCollectionContradiction ->
        Right $ replicate inputCount 0
      RelationalPositiveAffineRuleCollection reversedRules ->
        case closeRelationalPositiveAffineRules $ reverse reversedRules of
          RelationalPositiveAffineClosureContradiction ->
            Right $ replicate inputCount 0
          RelationalPositiveAffineClosureBounds bounds ->
            mapM (maximumFor bounds) [0 .. inputCount - 1]
 where
  clauses = case precondition of
    LengthAll formulas -> formulas
    formula -> [formula]

  collect !retained [] = RelationalPositiveAffineRuleCollection retained
  collect !retained (formula : remaining) =
    case strictRelationalPositiveAffineClauseCoverage
        inputCount inputPosition formula of
      RelationalPositiveAffineClauseIgnored -> collect retained remaining
      RelationalPositiveAffineClauseRules rules ->
        collect (prependRulesInReverse retained rules) remaining
      RelationalPositiveAffineClauseContradiction ->
        RelationalPositiveAffineRuleCollectionContradiction

  prependRulesInReverse !retained [] = retained
  prependRulesInReverse !retained (rule : remaining) =
    prependRulesInReverse (rule : retained) remaining

  maximumFor bounds index = case Map.lookup (fromIntegral index) bounds of
    Just maximumValue -> Right maximumValue
    Nothing -> Left $ LengthApplicableDomainInputUpperBoundMissing index

strictRelationalPositiveAffineClauseCoverage
  :: Int
  -> (variable -> Maybe Natural)
  -> LengthFormula variable
  -> RelationalPositiveAffineClauseCoverage
strictRelationalPositiveAffineClauseCoverage
    inputCount inputPosition formula = case formula of
  LengthNot (LengthAtMost left right) ->
    case summarizeBoth left right of
      Nothing -> RelationalPositiveAffineClauseIgnored
      Just (leftSummary, rightSummary) ->
        RelationalPositiveAffineClauseRules
          [ relationalPositiveAffineRule
              (incrementRelationalPositiveAffineConstant rightSummary)
              leftSummary
          ]
  _ -> relationalPositiveAffineClauseCoverage
    inputCount inputPosition formula
 where
  summarizeBoth left right = do
    leftSummary <- summarizeRelationalPositiveAffineExpression
      inputCount inputPosition left
    rightSummary <- summarizeRelationalPositiveAffineExpression
      inputCount inputPosition right
    pure (leftSummary, rightSummary)

-- The quotient successor is intentionally a separate scanner.  It delegates
-- every quotient-free clause to the strict predecessor verbatim and accepts
-- only one positive quotient at a directed relation operand's root.  The
-- rewrites below are exact over naturals and operate only on proof summaries;
-- no enlarged checked literal or expression is constructed.
strictRelationalPositiveAffineQuotientApplicableDomainMaximums
  :: Int
  -> (variable -> Maybe Natural)
  -> LengthFormula variable
  -> Either LengthApplicableDomainInapplicability [Natural]
strictRelationalPositiveAffineQuotientApplicableDomainMaximums
    inputCount inputPosition precondition
  | inputCount == 0 = Right []
  | otherwise = case collect [] clauses of
      RelationalPositiveAffineRuleCollectionContradiction ->
        Right $ replicate inputCount 0
      RelationalPositiveAffineRuleCollection reversedRules ->
        case closeRelationalPositiveAffineRules $ reverse reversedRules of
          RelationalPositiveAffineClosureContradiction ->
            Right $ replicate inputCount 0
          RelationalPositiveAffineClosureBounds bounds ->
            mapM (maximumFor bounds) [0 .. inputCount - 1]
 where
  clauses = case precondition of
    LengthAll formulas -> formulas
    formula -> [formula]

  collect !retained [] = RelationalPositiveAffineRuleCollection retained
  collect !retained (formula : remaining) =
    case strictRelationalPositiveAffineQuotientClauseCoverage
        inputCount inputPosition formula of
      RelationalPositiveAffineClauseIgnored -> collect retained remaining
      RelationalPositiveAffineClauseRules rules ->
        collect (prependRulesInReverse retained rules) remaining
      RelationalPositiveAffineClauseContradiction ->
        RelationalPositiveAffineRuleCollectionContradiction

  prependRulesInReverse !retained [] = retained
  prependRulesInReverse !retained (rule : remaining) =
    prependRulesInReverse (rule : retained) remaining

  maximumFor bounds index = case Map.lookup (fromIntegral index) bounds of
    Just maximumValue -> Right maximumValue
    Nothing -> Left $ LengthApplicableDomainInputUpperBoundMissing index

strictRelationalPositiveAffineQuotientClauseCoverage
  :: Int
  -> (variable -> Maybe Natural)
  -> LengthFormula variable
  -> RelationalPositiveAffineClauseCoverage
strictRelationalPositiveAffineQuotientClauseCoverage
    inputCount inputPosition formula = case formula of
  LengthAtMost left right
    | hasRootQuotient left || hasRootQuotient right ->
        rulesOrIgnored [quotientAtMostRule left right]
  LengthEqual left right
    | hasRootQuotient left || hasRootQuotient right ->
        rulesOrIgnored
          [ quotientAtMostRule left right
          , quotientAtMostRule right left
          ]
  LengthNot (LengthAtMost left right)
    | hasRootQuotient left || hasRootQuotient right ->
        rulesOrIgnored [quotientStrictRule left right]
  _ -> strictRelationalPositiveAffineClauseCoverage
    inputCount inputPosition formula
 where
  hasRootQuotient expression = case expression of
    LengthQuotient _ _ -> True
    _ -> False

  rulesOrIgnored ruleCandidates = case sequence ruleCandidates of
    Nothing -> RelationalPositiveAffineClauseIgnored
    Just rules -> RelationalPositiveAffineClauseRules rules

  summarize = summarizeRelationalPositiveAffineExpression
    inputCount inputPosition

  quotientAtMostRule left right = case (left, right) of
    (LengthQuotient _ _, LengthQuotient _ _) -> Nothing
    (LengthQuotient divisor dividend, opposite)
      | divisor > 0 -> do
          dividendSummary <- summarize dividend
          oppositeSummary <- summarize opposite
          pure $ relationalPositiveAffineRule
            dividendSummary
            $ addRelationalPositiveAffineConstant (divisor - 1)
            $ scaleRelationalPositiveAffineSummary divisor oppositeSummary
      | otherwise -> Nothing
    (opposite, LengthQuotient divisor dividend)
      | divisor > 0 -> do
          oppositeSummary <- summarize opposite
          dividendSummary <- summarize dividend
          pure $ relationalPositiveAffineRule
            (scaleRelationalPositiveAffineSummary divisor oppositeSummary)
            dividendSummary
      | otherwise -> Nothing
    _ -> Nothing

  quotientStrictRule left right = case (left, right) of
    (LengthQuotient _ _, LengthQuotient _ _) -> Nothing
    (LengthQuotient divisor dividend, opposite)
      | divisor > 0 -> do
          dividendSummary <- summarize dividend
          oppositeSummary <- summarize opposite
          pure $ relationalPositiveAffineRule
            (scaleRelationalPositiveAffineSummary divisor
              $ incrementRelationalPositiveAffineConstant oppositeSummary)
            dividendSummary
      | otherwise -> Nothing
    (opposite, LengthQuotient divisor dividend)
      | divisor > 0 -> do
          oppositeSummary <- summarize opposite
          dividendSummary <- summarize dividend
          pure $ relationalPositiveAffineRule
            (incrementRelationalPositiveAffineConstant dividendSummary)
            (scaleRelationalPositiveAffineSummary divisor oppositeSummary)
      | otherwise -> Nothing
    _ -> Nothing

-- The root-extrema successor preserves the quotient scanner and closure
-- literally for every clause without an immediate root minimum or maximum.
-- A supported extremum contributes two conjunctive rules atomically; no
-- component rule survives if any of its three affine operands is unsupported.
strictRelationalPositiveAffineQuotientRootExtremaApplicableDomainMaximums
  :: Int
  -> (variable -> Maybe Natural)
  -> LengthFormula variable
  -> Either LengthApplicableDomainInapplicability [Natural]
strictRelationalPositiveAffineQuotientRootExtremaApplicableDomainMaximums
    inputCount inputPosition precondition
  | inputCount == 0 = Right []
  | otherwise = case collect [] clauses of
      RelationalPositiveAffineRuleCollectionContradiction ->
        Right $ replicate inputCount 0
      RelationalPositiveAffineRuleCollection reversedRules ->
        case closeRelationalPositiveAffineRules $ reverse reversedRules of
          RelationalPositiveAffineClosureContradiction ->
            Right $ replicate inputCount 0
          RelationalPositiveAffineClosureBounds bounds ->
            mapM (maximumFor bounds) [0 .. inputCount - 1]
 where
  clauses = case precondition of
    LengthAll formulas -> formulas
    formula -> [formula]

  collect !retained [] = RelationalPositiveAffineRuleCollection retained
  collect !retained (formula : remaining) =
    case strictRelationalPositiveAffineQuotientRootExtremaClauseCoverage
        inputCount inputPosition formula of
      RelationalPositiveAffineClauseIgnored -> collect retained remaining
      RelationalPositiveAffineClauseRules rules ->
        collect (prependRulesInReverse retained rules) remaining
      RelationalPositiveAffineClauseContradiction ->
        RelationalPositiveAffineRuleCollectionContradiction

  prependRulesInReverse !retained [] = retained
  prependRulesInReverse !retained (rule : remaining) =
    prependRulesInReverse (rule : retained) remaining

  maximumFor bounds index = case Map.lookup (fromIntegral index) bounds of
    Just maximumValue -> Right maximumValue
    Nothing -> Left $ LengthApplicableDomainInputUpperBoundMissing index

strictRelationalPositiveAffineQuotientRootExtremaClauseCoverage
  :: Int
  -> (variable -> Maybe Natural)
  -> LengthFormula variable
  -> RelationalPositiveAffineClauseCoverage
strictRelationalPositiveAffineQuotientRootExtremaClauseCoverage
    inputCount inputPosition formula = case formula of
  LengthAtMost left right
    | hasRootExtrema left || hasRootExtrema right ->
        rulesOrIgnored $ extremaAtMostRules left right
  LengthEqual left right
    | hasRootExtrema left || hasRootExtrema right ->
        rulesOrIgnored $ extremaEqualityRules left right
  LengthNot (LengthAtMost left right)
    | hasRootExtrema left || hasRootExtrema right ->
        rulesOrIgnored $ extremaStrictRules left right
  _ -> strictRelationalPositiveAffineQuotientClauseCoverage
    inputCount inputPosition formula
 where
  hasRootExtrema expression = case expression of
    LengthMinimum _ _ -> True
    LengthMaximum _ _ -> True
    _ -> False

  rulesOrIgnored ruleCandidates = case ruleCandidates of
    Nothing -> RelationalPositiveAffineClauseIgnored
    Just rules -> RelationalPositiveAffineClauseRules rules

  summarize = summarizeRelationalPositiveAffineExpression
    inputCount inputPosition

  summarizeThree first second third = do
    firstSummary <- summarize first
    secondSummary <- summarize second
    thirdSummary <- summarize third
    pure (firstSummary, secondSummary, thirdSummary)

  maximumAtMostRules first second opposite = do
    (firstSummary, secondSummary, oppositeSummary) <-
      summarizeThree first second opposite
    pure
      [ relationalPositiveAffineRule firstSummary oppositeSummary
      , relationalPositiveAffineRule secondSummary oppositeSummary
      ]

  atMostMinimumRules opposite first second = do
    (oppositeSummary, firstSummary, secondSummary) <-
      summarizeThree opposite first second
    pure
      [ relationalPositiveAffineRule oppositeSummary firstSummary
      , relationalPositiveAffineRule oppositeSummary secondSummary
      ]

  strictMinimumAtMostRules first second opposite = do
    (firstSummary, secondSummary, oppositeSummary) <-
      summarizeThree first second opposite
    let incrementedOpposite =
          incrementRelationalPositiveAffineConstant oppositeSummary
    pure
      [ relationalPositiveAffineRule incrementedOpposite firstSummary
      , relationalPositiveAffineRule incrementedOpposite secondSummary
      ]

  strictAtMostMaximumRules opposite first second = do
    (oppositeSummary, firstSummary, secondSummary) <-
      summarizeThree opposite first second
    pure
      [ relationalPositiveAffineRule
          (incrementRelationalPositiveAffineConstant firstSummary)
          oppositeSummary
      , relationalPositiveAffineRule
          (incrementRelationalPositiveAffineConstant secondSummary)
          oppositeSummary
      ]

  extremaAtMostRules left right = case (left, right) of
    (LengthMaximum first second, opposite)
      | not $ hasRootExtrema opposite ->
          maximumAtMostRules first second opposite
    (opposite, LengthMinimum first second)
      | not $ hasRootExtrema opposite ->
          atMostMinimumRules opposite first second
    _ -> Nothing

  extremaEqualityRules left right = case (left, right) of
    (LengthMaximum first second, opposite)
      | not $ hasRootExtrema opposite ->
          maximumAtMostRules first second opposite
    (opposite, LengthMaximum first second)
      | not $ hasRootExtrema opposite ->
          maximumAtMostRules first second opposite
    (LengthMinimum first second, opposite)
      | not $ hasRootExtrema opposite ->
          atMostMinimumRules opposite first second
    (opposite, LengthMinimum first second)
      | not $ hasRootExtrema opposite ->
          atMostMinimumRules opposite first second
    _ -> Nothing

  extremaStrictRules left right = case (left, right) of
    (LengthMinimum first second, opposite)
      | not $ hasRootExtrema opposite ->
          strictMinimumAtMostRules first second opposite
    (opposite, LengthMaximum first second)
      | not $ hasRootExtrema opposite ->
          strictAtMostMaximumRules opposite first second
    _ -> Nothing

-- The monus successor delegates every clause without an immediate root monus
-- to the root-extrema predecessor.  Its rewrites are exact over naturals,
-- except that equality deliberately retains only its supported necessary
-- at-most half when the opposite affine expression may be zero.  Every
-- multi-rule result is admitted atomically after all three operands have been
-- summarized.
strictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainMaximums
  :: Int
  -> (variable -> Maybe Natural)
  -> LengthFormula variable
  -> Either LengthApplicableDomainInapplicability [Natural]
strictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainMaximums
    inputCount inputPosition precondition
  | inputCount == 0 = Right []
  | otherwise = case collect [] clauses of
      RelationalPositiveAffineRuleCollectionContradiction ->
        Right $ replicate inputCount 0
      RelationalPositiveAffineRuleCollection reversedRules ->
        case closeRelationalPositiveAffineRules $ reverse reversedRules of
          RelationalPositiveAffineClosureContradiction ->
            Right $ replicate inputCount 0
          RelationalPositiveAffineClosureBounds bounds ->
            mapM (maximumFor bounds) [0 .. inputCount - 1]
 where
  clauses = case precondition of
    LengthAll formulas -> formulas
    formula -> [formula]

  collect !retained [] = RelationalPositiveAffineRuleCollection retained
  collect !retained (formula : remaining) =
    case strictRelationalPositiveAffineQuotientRootExtremaMonusClauseCoverage
        inputCount inputPosition formula of
      RelationalPositiveAffineClauseIgnored -> collect retained remaining
      RelationalPositiveAffineClauseRules rules ->
        collect (prependRulesInReverse retained rules) remaining
      RelationalPositiveAffineClauseContradiction ->
        RelationalPositiveAffineRuleCollectionContradiction

  prependRulesInReverse !retained [] = retained
  prependRulesInReverse !retained (rule : remaining) =
    prependRulesInReverse (rule : retained) remaining

  maximumFor bounds index = case Map.lookup (fromIntegral index) bounds of
    Just maximumValue -> Right maximumValue
    Nothing -> Left $ LengthApplicableDomainInputUpperBoundMissing index

strictRelationalPositiveAffineQuotientRootExtremaMonusClauseCoverage
  :: Int
  -> (variable -> Maybe Natural)
  -> LengthFormula variable
  -> RelationalPositiveAffineClauseCoverage
strictRelationalPositiveAffineQuotientRootExtremaMonusClauseCoverage
    inputCount inputPosition formula = case formula of
  LengthAtMost left right
    | hasRootMonus left || hasRootMonus right ->
        rulesOrIgnored $ monusAtMostRules left right
  LengthEqual left right
    | hasRootMonus left || hasRootMonus right ->
        rulesOrIgnored $ monusEqualityRules left right
  LengthNot (LengthAtMost left right)
    | hasRootMonus left || hasRootMonus right ->
        rulesOrIgnored $ monusStrictRules left right
  _ -> strictRelationalPositiveAffineQuotientRootExtremaClauseCoverage
    inputCount inputPosition formula
 where
  hasRootMonus expression = case expression of
    LengthMonus _ _ -> True
    _ -> False

  rulesOrIgnored ruleCandidates = case ruleCandidates of
    Nothing -> RelationalPositiveAffineClauseIgnored
    Just rules -> RelationalPositiveAffineClauseRules rules

  summarize = summarizeRelationalPositiveAffineExpression
    inputCount inputPosition

  summarizeThree first second third = do
    firstSummary <- summarize first
    secondSummary <- summarize second
    thirdSummary <- summarize third
    pure (firstSummary, secondSummary, thirdSummary)

  -- A monus B <= C  <=>  A <= B + C.
  monusAtMostOppositeRules first second opposite = do
    (firstSummary, secondSummary, oppositeSummary) <-
      summarizeThree first second opposite
    pure
      [ relationalPositiveAffineRule firstSummary
          $ addRelationalPositiveAffineSummaries
              secondSummary oppositeSummary
      ]

  -- A uniformly positive affine C has no zero branch, so
  -- C <= A monus B  <=>  B + C <= A.  Identically-zero C makes the source
  -- clause tautological; a may-zero nonconstant C retains the disjunction and
  -- is deliberately ignored.
  oppositeAtMostMonusRules opposite first second = do
    (oppositeSummary, firstSummary, secondSummary) <-
      summarizeThree opposite first second
    case oppositeSummary of
      RelationalPositiveAffineSummary constant coefficients
        | constant > 0 -> pure
            [ relationalPositiveAffineRule
                (addRelationalPositiveAffineSummaries
                  secondSummary oppositeSummary)
                firstSummary
            ]
        | Map.null coefficients -> pure []
        | otherwise -> Nothing

  monusEqualityConsequences first second opposite = do
    (firstSummary, secondSummary, oppositeSummary) <-
      summarizeThree first second opposite
    let secondPlusOpposite = addRelationalPositiveAffineSummaries
          secondSummary oppositeSummary
        atMostRule = relationalPositiveAffineRule
          firstSummary secondPlusOpposite
    pure $ case oppositeSummary of
      RelationalPositiveAffineSummary constant _
        | constant > 0 ->
            [ atMostRule
            , relationalPositiveAffineRule secondPlusOpposite firstSummary
            ]
        | otherwise -> [atMostRule]

  -- not (A monus B <= C)  <=>  B + C + 1 <= A.
  strictMonusAtMostRules first second opposite = do
    (firstSummary, secondSummary, oppositeSummary) <-
      summarizeThree first second opposite
    pure
      [ relationalPositiveAffineRule
          (incrementRelationalPositiveAffineConstant
            $ addRelationalPositiveAffineSummaries
                secondSummary oppositeSummary)
          firstSummary
      ]

  -- not (C <= A monus B)  <=>
  -- 1 <= C and A + 1 <= B + C.  The boundary rule is emitted first.
  strictAtMostMonusRules opposite first second = do
    (oppositeSummary, firstSummary, secondSummary) <-
      summarizeThree opposite first second
    pure
      [ relationalPositiveAffineRule
          (RelationalPositiveAffineSummary 1 Map.empty)
          oppositeSummary
      , relationalPositiveAffineRule
          (incrementRelationalPositiveAffineConstant firstSummary)
          (addRelationalPositiveAffineSummaries
            secondSummary oppositeSummary)
      ]

  monusAtMostRules left right = case (left, right) of
    (LengthMonus first second, opposite)
      | not $ hasRootMonus opposite ->
          monusAtMostOppositeRules first second opposite
    (opposite, LengthMonus first second)
      | not $ hasRootMonus opposite ->
          oppositeAtMostMonusRules opposite first second
    _ -> Nothing

  monusEqualityRules left right = case (left, right) of
    (LengthMonus first second, opposite)
      | not $ hasRootMonus opposite ->
          monusEqualityConsequences first second opposite
    (opposite, LengthMonus first second)
      | not $ hasRootMonus opposite ->
          monusEqualityConsequences first second opposite
    _ -> Nothing

  monusStrictRules left right = case (left, right) of
    (LengthMonus first second, opposite)
      | not $ hasRootMonus opposite ->
          strictMonusAtMostRules first second opposite
    (opposite, LengthMonus first second)
      | not $ hasRootMonus opposite ->
          strictAtMostMonusRules opposite first second
    _ -> Nothing

-- The atomic-branching successor returns proof-rule alternatives rather than
-- manufacturing unchecked formula syntax.  Every new atom first summarizes
-- all three affine operands; failure leaves the whole atom to the predecessor
-- result (which is ignored for these unsupported root shapes).  Existing
-- exact predecessor leaves remain singleton alternatives in literal rule
-- order, while a predecessor contradiction contributes one explicit
-- contradictory alternative which the later branch-local collection drops.
strictRelationalPositiveAffineQuotientRootExtremaMonusAtomicBranchingClauseBranches
  :: Int
  -> (variable -> Maybe Natural)
  -> LengthFormula variable
  -> [RelationalPositiveAffineClauseCoverage]
strictRelationalPositiveAffineQuotientRootExtremaMonusAtomicBranchingClauseBranches
    inputCount inputPosition formula =
  case atomicBranches formula of
    Just branches -> map RelationalPositiveAffineClauseRules branches
    Nothing -> [predecessorCoverage]
 where
  atomicBranches source = case source of
    LengthAtMost left right ->
      extremaAtMostBranches left right `orElse`
        monusAtMostBranches left right
    LengthEqual left right ->
      extremaEqualityBranches left right `orElse`
        monusEqualityBranches left right
    LengthNot (LengthAtMost left right) ->
      extremaStrictBranches left right
    _ -> Nothing

  predecessorCoverage =
    strictRelationalPositiveAffineQuotientRootExtremaMonusClauseCoverage
      inputCount inputPosition formula

  orElse first second = case first of
    Just result -> Just result
    Nothing -> second

  summarize = summarizeRelationalPositiveAffineExpression
    inputCount inputPosition

  summarizeThree first second third = do
    firstSummary <- summarize first
    secondSummary <- summarize second
    thirdSummary <- summarize third
    pure (firstSummary, secondSummary, thirdSummary)

  hasRootExtrema expression = case expression of
    LengthMinimum _ _ -> True
    LengthMaximum _ _ -> True
    _ -> False

  hasRootMonus expression = case expression of
    LengthMonus _ _ -> True
    _ -> False

  -- C <= max(A,B) <=> C <= A or C <= B.
  oppositeAtMostMaximumBranches opposite first second = do
    (oppositeSummary, firstSummary, secondSummary) <-
      summarizeThree opposite first second
    pure
      [ [relationalPositiveAffineRule oppositeSummary firstSummary]
      , [relationalPositiveAffineRule oppositeSummary secondSummary]
      ]

  -- min(A,B) <= C <=> A <= C or B <= C.
  minimumAtMostOppositeBranches first second opposite = do
    (firstSummary, secondSummary, oppositeSummary) <-
      summarizeThree first second opposite
    pure
      [ [relationalPositiveAffineRule firstSummary oppositeSummary]
      , [relationalPositiveAffineRule secondSummary oppositeSummary]
      ]

  -- not (max(A,B) <= C) <=> C+1 <= A or C+1 <= B.
  strictMaximumAtMostBranches first second opposite = do
    (firstSummary, secondSummary, oppositeSummary) <-
      summarizeThree first second opposite
    let incrementedOpposite =
          incrementRelationalPositiveAffineConstant oppositeSummary
    pure
      [ [relationalPositiveAffineRule incrementedOpposite firstSummary]
      , [relationalPositiveAffineRule incrementedOpposite secondSummary]
      ]

  -- not (C <= min(A,B)) <=> A+1 <= C or B+1 <= C.
  strictAtMostMinimumBranches opposite first second = do
    (oppositeSummary, firstSummary, secondSummary) <-
      summarizeThree opposite first second
    pure
      [ [ relationalPositiveAffineRule
            (incrementRelationalPositiveAffineConstant firstSummary)
            oppositeSummary
        ]
      , [ relationalPositiveAffineRule
            (incrementRelationalPositiveAffineConstant secondSummary)
            oppositeSummary
        ]
      ]

  maximumEqualityBranches first second opposite = do
    (firstSummary, secondSummary, oppositeSummary) <-
      summarizeThree first second opposite
    let firstAtMost =
          relationalPositiveAffineRule firstSummary oppositeSummary
        secondAtMost =
          relationalPositiveAffineRule secondSummary oppositeSummary
    pure
      [ [ firstAtMost
        , secondAtMost
        , relationalPositiveAffineRule oppositeSummary firstSummary
        ]
      , [ firstAtMost
        , secondAtMost
        , relationalPositiveAffineRule oppositeSummary secondSummary
        ]
      ]

  minimumEqualityBranches first second opposite = do
    (firstSummary, secondSummary, oppositeSummary) <-
      summarizeThree first second opposite
    let atMostFirst =
          relationalPositiveAffineRule oppositeSummary firstSummary
        atMostSecond =
          relationalPositiveAffineRule oppositeSummary secondSummary
    pure
      [ [ atMostFirst
        , atMostSecond
        , relationalPositiveAffineRule firstSummary oppositeSummary
        ]
      , [ atMostFirst
        , atMostSecond
        , relationalPositiveAffineRule secondSummary oppositeSummary
        ]
      ]

  extremaAtMostBranches left right = case (left, right) of
    (LengthMinimum first second, opposite)
      | not $ hasRootExtrema opposite ->
          minimumAtMostOppositeBranches first second opposite
    (opposite, LengthMaximum first second)
      | not $ hasRootExtrema opposite ->
          oppositeAtMostMaximumBranches opposite first second
    _ -> Nothing

  extremaStrictBranches left right = case (left, right) of
    (LengthMaximum first second, opposite)
      | not $ hasRootExtrema opposite ->
          strictMaximumAtMostBranches first second opposite
    (opposite, LengthMinimum first second)
      | not $ hasRootExtrema opposite ->
          strictAtMostMinimumBranches opposite first second
    _ -> Nothing

  extremaEqualityBranches left right = case (left, right) of
    (LengthMaximum first second, opposite)
      | not $ hasRootExtrema opposite ->
          maximumEqualityBranches first second opposite
    (opposite, LengthMaximum first second)
      | not $ hasRootExtrema opposite ->
          maximumEqualityBranches first second opposite
    (LengthMinimum first second, opposite)
      | not $ hasRootExtrema opposite ->
          minimumEqualityBranches first second opposite
    (opposite, LengthMinimum first second)
      | not $ hasRootExtrema opposite ->
          minimumEqualityBranches first second opposite
    _ -> Nothing

  mayZeroSummary (RelationalPositiveAffineSummary constant coefficients) =
    constant == 0 && not (Map.null coefficients)

  -- For may-zero C, C <= A monus B is the exact zero-first union
  -- C <= 0 or B+C <= A.
  oppositeAtMostMonusBranches opposite first second = do
    (oppositeSummary, firstSummary, secondSummary) <-
      summarizeThree opposite first second
    if mayZeroSummary oppositeSummary
      then
        let zeroSummary = RelationalPositiveAffineSummary 0 Map.empty
            secondPlusOpposite = addRelationalPositiveAffineSummaries
              secondSummary oppositeSummary
        in Just
          [ [relationalPositiveAffineRule oppositeSummary zeroSummary]
          , [relationalPositiveAffineRule secondPlusOpposite firstSummary]
          ]
      else Nothing

  -- For may-zero C, A monus B = C is the exact zero-first union below.
  -- The predecessor's necessary A <= B+C rule remains first in both choices.
  monusMayZeroEqualityBranches first second opposite = do
    (firstSummary, secondSummary, oppositeSummary) <-
      summarizeThree first second opposite
    if mayZeroSummary oppositeSummary
      then
        let zeroSummary = RelationalPositiveAffineSummary 0 Map.empty
            secondPlusOpposite = addRelationalPositiveAffineSummaries
              secondSummary oppositeSummary
            commonRule = relationalPositiveAffineRule
              firstSummary secondPlusOpposite
        in Just
          [ [ commonRule
            , relationalPositiveAffineRule oppositeSummary zeroSummary
            ]
          , [ commonRule
            , relationalPositiveAffineRule secondPlusOpposite firstSummary
            ]
          ]
      else Nothing

  monusAtMostBranches left right = case (left, right) of
    (opposite, LengthMonus first second)
      | not $ hasRootMonus opposite ->
          oppositeAtMostMonusBranches opposite first second
    _ -> Nothing

  monusEqualityBranches left right = case (left, right) of
    (LengthMonus first second, opposite)
      | not $ hasRootMonus opposite ->
          monusMayZeroEqualityBranches first second opposite
    (opposite, LengthMonus first second)
      | not $ hasRootMonus opposite ->
          monusMayZeroEqualityBranches first second opposite
    _ -> Nothing

addRelationalPositiveAffineSummaries
  :: RelationalPositiveAffineSummary
  -> RelationalPositiveAffineSummary
  -> RelationalPositiveAffineSummary
addRelationalPositiveAffineSummaries
    (RelationalPositiveAffineSummary leftConstant leftCoefficients)
    (RelationalPositiveAffineSummary rightConstant rightCoefficients) =
  RelationalPositiveAffineSummary
    (leftConstant + rightConstant)
    (Map.unionWith (+) leftCoefficients rightCoefficients)

scaleRelationalPositiveAffineSummary
  :: Natural
  -> RelationalPositiveAffineSummary
  -> RelationalPositiveAffineSummary
scaleRelationalPositiveAffineSummary factor
    (RelationalPositiveAffineSummary constant coefficients) =
  RelationalPositiveAffineSummary
    (factor * constant)
    (Map.map (factor *) coefficients)

addRelationalPositiveAffineConstant
  :: Natural
  -> RelationalPositiveAffineSummary
  -> RelationalPositiveAffineSummary
addRelationalPositiveAffineConstant delta
    (RelationalPositiveAffineSummary constant coefficients) =
  RelationalPositiveAffineSummary (constant + delta) coefficients

-- Over natural values, @not (left <= right)@ is exactly
-- @right + 1 <= left@.  This proof-only successor is applied to the exact
-- arbitrary-precision summary; it neither constructs new checked syntax nor
-- changes the public literal bound or normalized contract identity.
incrementRelationalPositiveAffineConstant
  :: RelationalPositiveAffineSummary
  -> RelationalPositiveAffineSummary
incrementRelationalPositiveAffineConstant
    (RelationalPositiveAffineSummary constant coefficients) =
  RelationalPositiveAffineSummary (constant + 1) coefficients

-- Unlike the literal-ceiling rule above, relational cancellation requires
-- exact constants and coefficients on both sides.  Checked syntax bounds the
-- tree; arbitrary-precision naturals retain exactness without a lossy cap.
summarizeRelationalPositiveAffineExpression
  :: Int
  -> (variable -> Maybe Natural)
  -> LengthExpression variable
  -> Maybe RelationalPositiveAffineSummary
summarizeRelationalPositiveAffineExpression inputCount inputPosition = go
 where
  go expression = case expression of
    LengthVariable variable -> do
      position <- inputPosition variable
      if position < fromIntegral inputCount
        then Just $ RelationalPositiveAffineSummary 0
          $ Map.singleton position 1
        else Nothing
    LengthLiteral value ->
      Just $ RelationalPositiveAffineSummary value Map.empty
    LengthSum terms ->
      foldM add (RelationalPositiveAffineSummary 0 Map.empty) terms
    LengthScale factor nested
      | factor == 0 -> Nothing
      | otherwise -> scale factor <$> go nested
    _ -> Nothing

  add (RelationalPositiveAffineSummary leftConstant leftCoefficients) term = do
    RelationalPositiveAffineSummary rightConstant rightCoefficients <- go term
    pure $ RelationalPositiveAffineSummary
      (leftConstant + rightConstant)
      (Map.unionWith (+) leftCoefficients rightCoefficients)

  scale factor (RelationalPositiveAffineSummary constant coefficients) =
    RelationalPositiveAffineSummary
      (factor * constant)
      (Map.map (factor *) coefficients)

relationalPositiveAffineRule
  :: RelationalPositiveAffineSummary
  -> RelationalPositiveAffineSummary
  -> RelationalPositiveAffineRule
relationalPositiveAffineRule
    (RelationalPositiveAffineSummary leftConstant leftCoefficients)
    (RelationalPositiveAffineSummary rightConstant rightCoefficients) =
  let (residualLeftConstant, residualRightConstant)
        | leftConstant >= rightConstant =
            (leftConstant - rightConstant, 0)
        | otherwise = (0, rightConstant - leftConstant)
  in RelationalPositiveAffineRule
      residualLeftConstant
      (positiveCoefficientDifference leftCoefficients rightCoefficients)
      residualRightConstant
      (positiveCoefficientDifference rightCoefficients leftCoefficients)

positiveCoefficientDifference
  :: Map.Map Natural Natural
  -> Map.Map Natural Natural
  -> Map.Map Natural Natural
positiveCoefficientDifference minuend subtrahend =
  Map.foldlWithKey' retain Map.empty minuend
 where
  retain !difference position coefficient =
    let opposing = Map.findWithDefault 0 position subtrahend
    in if coefficient > opposing
        then Map.insert position (coefficient - opposing) difference
        else difference

-- Seed every rule whose residual right side is constant.  Remaining rules are
-- retried in canonical order.  A pass observes one immutable bounds snapshot;
-- all rules which become eligible in that pass fire once and are then removed.
-- Consequently even a numeric tightening cycle cannot iterate toward a least
-- fixed point: successful progress consumes at least one still-pending rule.
closeRelationalPositiveAffineRules
  :: [RelationalPositiveAffineRule]
  -> RelationalPositiveAffineClosure
closeRelationalPositiveAffineRules rules =
  let (seedRules, pendingRules) =
        partitionRelationalPositiveAffineRules rules
  in case relationalPositiveAffineRulePass Map.empty seedRules of
    RelationalPositiveAffineRulePassContradiction ->
      RelationalPositiveAffineClosureContradiction
    RelationalPositiveAffineRulePassComplete seedBounds retainedSeeds _ ->
      close seedBounds $ retainedSeeds ++ pendingRules
 where
  close !bounds [] = RelationalPositiveAffineClosureBounds bounds
  close !bounds pending = case
      relationalPositiveAffineRulePass bounds pending of
    RelationalPositiveAffineRulePassContradiction ->
      RelationalPositiveAffineClosureContradiction
    RelationalPositiveAffineRulePassComplete nextBounds retained fired
      | fired -> close nextBounds retained
      | otherwise -> RelationalPositiveAffineClosureBounds nextBounds

partitionRelationalPositiveAffineRules
  :: [RelationalPositiveAffineRule]
  -> ([RelationalPositiveAffineRule], [RelationalPositiveAffineRule])
partitionRelationalPositiveAffineRules = go [] []
 where
  go !seeds !pending [] = (reverse seeds, reverse pending)
  go !seeds !pending (rule : remaining) = case rule of
    RelationalPositiveAffineRule _ _ _ rightCoefficients
      | Map.null rightCoefficients -> go (rule : seeds) pending remaining
      | otherwise -> go seeds (rule : pending) remaining

relationalPositiveAffineRulePass
  :: Map.Map Natural Natural
  -> [RelationalPositiveAffineRule]
  -> RelationalPositiveAffineRulePass
relationalPositiveAffineRulePass bounds = go Map.empty [] False
 where
  go !derived !retained !fired [] =
    RelationalPositiveAffineRulePassComplete
      (Map.unionWith min bounds derived)
      (reverse retained)
      fired
  go !derived !retained !fired (rule : remaining) = case rule of
    RelationalPositiveAffineRule leftConstant leftCoefficients
        rightConstant rightCoefficients ->
      case relationalPositiveAffineRightMaximum
          bounds rightConstant rightCoefficients of
        Nothing -> go derived (rule : retained) fired remaining
        Just rightMaximum
          | leftConstant > rightMaximum ->
              RelationalPositiveAffineRulePassContradiction
          | otherwise ->
              let numerator = rightMaximum - leftConstant
                  ruleBounds = Map.map (numerator `quot`) leftCoefficients
                  nextDerived = Map.unionWith min derived ruleBounds
              in go nextDerived retained True remaining

relationalPositiveAffineRightMaximum
  :: Map.Map Natural Natural
  -> Natural
  -> Map.Map Natural Natural
  -> Maybe Natural
relationalPositiveAffineRightMaximum bounds rightConstant coefficients =
  go rightConstant $ Map.toAscList coefficients
 where
  go !total remainingCoefficients = case remainingCoefficients of
    [] -> Just total
    (position, coefficient) : remaining -> do
      maximumValue <- Map.lookup position bounds
      let !next = total + coefficient * maximumValue
      go next remaining

-- | Private replay classification shared by one-assignment counterexample
-- validation and complete input-box traversal.  Keeping one implementation
-- preserves their arity, value, precondition, candidate-result, and
-- postcondition demand order exactly.
data LengthProblemAssignmentReplay
  = LengthProblemPreconditionNotMet
  | LengthProblemPostconditionSatisfied
  | LengthProblemPostconditionViolated ValidatedLengthCounterexample

replayLengthProblemAssignment
  :: LengthEvaluationLimits
  -> CheckedLengthProblem identity local
  -> LengthProblemAssignment
  -> Either LengthEvaluationError LengthProblemAssignmentReplay
replayLengthProblemAssignment limits problem assignment = do
  inputs <- exactAssignment
    LengthProblemAssignmentArityMismatch
    (checkedLengthProblemInputCount problem)
    $ lengthProblemAssignmentInputs assignment
  mapM_ (uncurry $ checkAssignedValue limits . LengthProblemInputValue)
    $ zip [0 ..] inputs
  let lookupInput variable = case variable of
        LengthInput position -> case indexNatural position inputs of
          Just value -> Right value
          Nothing -> Left $ LengthEvaluationInternalContractReference variable
        LengthResult -> Left
          $ LengthEvaluationInternalContractReference LengthResult
  precondition <- evaluateFormula limits lookupInput
    $ checkedLengthProblemPrecondition problem
  if not precondition
    then Right LengthProblemPreconditionNotMet
    else do
      let resultOr = evaluateExpression limits lookupInput
            $ checkedLengthCandidateResult
            $ checkedLengthProblemCandidate problem
          lookupResult variable = case variable of
            LengthResult -> resultOr
            LengthInput position -> case indexNatural position inputs of
              Just value -> Right value
              Nothing -> Left
                $ LengthEvaluationInternalContractReference variable
      postcondition <- evaluateFormula limits lookupResult
        $ checkedLengthProblemPostcondition problem
      if postcondition
        then Right LengthProblemPostconditionSatisfied
        else do
          result <- resultOr
          pure $ LengthProblemPostconditionViolated
            $ ValidatedLengthCounterexampleReceipt
                inputs result $ problemBasis problem

problemBasis
  :: CheckedLengthProblem identity local
  -> LengthCounterexampleBasis
problemBasis problem = case checkedLengthCandidateUsedProviders
    $ checkedLengthProblemCandidate problem of
  [] -> ProviderIndependentFiniteSpineModel
  names -> FiniteSpineModelUnderAssumedProviderLaws names

data LengthSpinePairProblemAssignmentReplay
  = LengthSpinePairProblemPreconditionNotMet
  | LengthSpinePairProblemPostconditionSatisfied
  | LengthSpinePairProblemPostconditionViolated
      ValidatedLengthSpinePairCounterexample

replayLengthSpinePairProblemAssignment
  :: LengthEvaluationLimits
  -> CheckedLengthSpinePairProblem identity local
  -> LengthProblemAssignment
  -> Either
      LengthSpinePairEvaluationError
      LengthSpinePairProblemAssignmentReplay
replayLengthSpinePairProblemAssignment limits problem assignment = do
  inputs <- exactSpinePairAssignment
    LengthSpinePairProblemAssignmentArityMismatch
    (checkedLengthSpinePairProblemInputCount problem)
    $ lengthProblemAssignmentInputs assignment
  mapM_ (uncurry $ checkSpinePairAssignedValue limits .
      LengthSpinePairProblemInputValue)
    $ zip [0 ..] inputs
  let lookupContractInput variable = case variable of
        LengthSpinePairInput position -> case indexNatural position inputs of
          Just value -> Right value
          Nothing -> Left
            $ LengthSpinePairEvaluationInternalContractReference variable
        LengthSpinePairResult _ -> Left
          $ LengthSpinePairEvaluationInternalContractReference variable
  precondition <- evaluateSpinePairFormula limits lookupContractInput
    $ checkedLengthSpinePairProblemPrecondition problem
  if not precondition
    then Right LengthSpinePairProblemPreconditionNotMet
    else do
      let candidateResult = checkedLengthSpinePairCandidateResult
            $ checkedLengthSpinePairProblemCandidate problem
          lookupCandidateInput variable = case variable of
            LengthInput position -> case indexNatural position inputs of
              Just value -> Right value
              Nothing -> Left
                $ LengthSpinePairEvaluationInternalCandidateReference variable
            LengthResult -> Left
              $ LengthSpinePairEvaluationInternalCandidateReference variable
          firstOr = evaluateSpinePairExpression limits lookupCandidateInput
            $ lengthSpinePairFirst candidateResult
          secondOr = evaluateSpinePairExpression limits lookupCandidateInput
            $ lengthSpinePairSecond candidateResult
          lookupResult variable = case variable of
            LengthSpinePairInput position -> case indexNatural position inputs of
              Just value -> Right value
              Nothing -> Left
                $ LengthSpinePairEvaluationInternalContractReference variable
            LengthSpinePairResult component -> case component of
              LengthSpinePairFirst -> firstOr
              LengthSpinePairSecond -> secondOr
      postcondition <- evaluateSpinePairFormula limits lookupResult
        $ checkedLengthSpinePairProblemPostcondition problem
      if postcondition
        then Right LengthSpinePairProblemPostconditionSatisfied
        else do
          firstResult <- firstOr
          secondResult <- secondOr
          pure $ LengthSpinePairProblemPostconditionViolated
            $ ValidatedLengthSpinePairCounterexampleReceipt
                inputs
                (LengthSpinePair firstResult secondResult)
                $ spinePairProblemBasis problem

spinePairProblemBasis
  :: CheckedLengthSpinePairProblem identity local
  -> LengthCounterexampleBasis
spinePairProblemBasis problem = case
    checkedLengthSpinePairCandidateUsedProviders
      $ checkedLengthSpinePairProblemCandidate problem of
  [] -> ProviderIndependentFiniteSpineModel
  names -> FiniteSpineModelUnderAssumedProviderLaws names

exactInputBoxBounds
  :: Int
  -> [Natural]
  -> Either LengthInputBoxValidationError [Natural]
exactInputBoxBounds expected maximums =
  let observed = observedListLength expected maximums
  in if observed == expected
      then Right maximums
      else Left $ LengthInputBoxBoundsArityMismatch expected observed

inputBoxAssignmentCount
  :: LengthInputBoxLimits
  -> [Natural]
  -> Either LengthInputBoxValidationError Natural
inputBoxAssignmentCount limits = go 1
 where
  maximumAssignments = lengthInputBoxAssignmentLimit limits
  exceeded = maximumAssignments + 1

  go !total []
    | total <= maximumAssignments = Right total
    | otherwise = Left $ LengthInputBoxAssignmentLimitExceeded
        maximumAssignments exceeded
  go !total (maximumValue : remaining)
    | total > maximumAssignments = Left
        $ LengthInputBoxAssignmentLimitExceeded maximumAssignments exceeded
    | factor > 0 && total > maximumAssignments `quot` factor = Left
        $ LengthInputBoxAssignmentLimitExceeded maximumAssignments exceeded
    | otherwise = go (total * factor) remaining
   where
    factor = maximumValue + 1

-- | Convert one exact mixed-radix assignment to the number of lexicographic
-- assignments inspected through it.  The final source input is the
-- least-significant digit, matching 'nextInputBoxAssignment'.
inputBoxInspectedAssignmentCount
  :: [Natural]
  -> [Natural]
  -> Maybe Natural
inputBoxInspectedAssignmentCount = go 0
 where
  go !ordinal [] [] = Just $ ordinal + 1
  go !ordinal (maximumValue : remainingMaximums)
      (value : remainingValues)
    | value <= maximumValue = go
        (ordinal * (maximumValue + 1) + value)
        remainingMaximums remainingValues
    | otherwise = Nothing
  go _ _ _ = Nothing

-- | Advance a source-ordered mixed-radix vector.  Reversing makes the final
-- source input the least-significant digit, hence the fastest-varying one.
nextInputBoxAssignment
  :: [Natural]
  -> [Natural]
  -> Either LengthInputBoxValidationError (Maybe [Natural])
nextInputBoxAssignment maximums values =
  fmap (fmap reverse) $ advance (reverse maximums) (reverse values)
 where
  advance [] [] = Right Nothing
  advance (maximumValue : remainingMaximums)
      (value : remainingValues)
    | value < maximumValue = Right $ Just $ value + 1 : remainingValues
    | otherwise = do
        following <- advance remainingMaximums remainingValues
        pure $ fmap (0 :) following
  -- Both lists are exact-arity values constructed at checked boundaries.  The
  -- defensive mismatch branch nevertheless fails closed rather than treating
  -- an impossible truncation as successful completion.
  advance _ _ = Left LengthInputBoxInternalEnumerationInvariant

exactSpinePairInputBoxBounds
  :: Int
  -> [Natural]
  -> Either LengthSpinePairInputBoxValidationError [Natural]
exactSpinePairInputBoxBounds expected maximums =
  let observed = observedListLength expected maximums
  in if observed == expected
      then Right maximums
      else Left $ LengthSpinePairInputBoxBoundsArityMismatch expected observed

spinePairInputBoxAssignmentCount
  :: LengthInputBoxLimits
  -> [Natural]
  -> Either LengthSpinePairInputBoxValidationError Natural
spinePairInputBoxAssignmentCount limits = go 1
 where
  maximumAssignments = lengthInputBoxAssignmentLimit limits
  exceeded = maximumAssignments + 1

  go !total []
    | total <= maximumAssignments = Right total
    | otherwise = Left $ LengthSpinePairInputBoxAssignmentLimitExceeded
        maximumAssignments exceeded
  go !total (maximumValue : remaining)
    | total > maximumAssignments = Left
        $ LengthSpinePairInputBoxAssignmentLimitExceeded
            maximumAssignments exceeded
    | factor > 0 && total > maximumAssignments `quot` factor = Left
        $ LengthSpinePairInputBoxAssignmentLimitExceeded
            maximumAssignments exceeded
    | otherwise = go (total * factor) remaining
   where
    factor = maximumValue + 1

nextSpinePairInputBoxAssignment
  :: [Natural]
  -> [Natural]
  -> Either
      LengthSpinePairInputBoxValidationError
      (Maybe [Natural])
nextSpinePairInputBoxAssignment maximums values =
  fmap (fmap reverse) $ advance (reverse maximums) (reverse values)
 where
  advance [] [] = Right Nothing
  advance (maximumValue : remainingMaximums)
      (value : remainingValues)
    | value < maximumValue = Right $ Just $ value + 1 : remainingValues
    | otherwise = do
        following <- advance remainingMaximums remainingValues
        pure $ fmap (0 :) following
  advance _ _ = Left LengthSpinePairInputBoxInternalEnumerationInvariant

exactAssignment
  :: (Int -> Int -> LengthEvaluationError)
  -> Int
  -> [value]
  -> Either LengthEvaluationError [value]
exactAssignment mismatch expected values =
  let observed = observedListLength expected values
  in if observed == expected
      then Right values
      else Left $ mismatch expected observed

exactSpinePairAssignment
  :: (Int -> Int -> LengthSpinePairEvaluationError)
  -> Int
  -> [value]
  -> Either LengthSpinePairEvaluationError [value]
exactSpinePairAssignment mismatch expected values =
  let observed = observedListLength expected values
  in if observed == expected
      then Right values
      else Left $ mismatch expected observed

checkSpinePairAssignedValue
  :: LengthEvaluationLimits
  -> LengthSpinePairEvaluationValueSite
  -> Natural
  -> Either LengthSpinePairEvaluationError ()
checkSpinePairAssignedValue limits site value =
  checkSpinePairValueWithin site
    (lengthAssignmentValueBitLimit limits) value

checkSpinePairIntermediate
  :: LengthEvaluationLimits
  -> Natural
  -> Either LengthSpinePairEvaluationError Natural
checkSpinePairIntermediate limits value = value <$ checkSpinePairValueWithin
  LengthSpinePairIntermediateValue
  (lengthIntermediateValueBitLimit limits) value

checkSpinePairValueWithin
  :: LengthSpinePairEvaluationValueSite
  -> Int
  -> Natural
  -> Either LengthSpinePairEvaluationError ()
checkSpinePairValueWithin site maximumBits value =
  let observedBits = observedNaturalBits maximumBits value
  in unless (observedBits <= maximumBits) $ Left
      $ LengthSpinePairEvaluationValueBitLimitExceeded
          site maximumBits observedBits

evaluateSpinePairExpression
  :: LengthEvaluationLimits
  -> (variable -> Either LengthSpinePairEvaluationError Natural)
  -> LengthExpression variable
  -> Either LengthSpinePairEvaluationError Natural
evaluateSpinePairExpression limits lookupVariable source = case source of
  LengthVariable variable -> lookupVariable variable
  LengthLiteral value -> checkSpinePairIntermediate limits value
  LengthSum terms -> foldM add 0 terms
  LengthScale factor expression -> do
    value <- evaluateSpinePairExpression limits lookupVariable expression
    checkSpinePairIntermediate limits $ factor * value
  LengthQuotient divisor expression
    | divisor == 0 -> Left
        LengthSpinePairEvaluationInternalQuotientDivisorZero
    | otherwise -> do
        value <- evaluateSpinePairExpression limits lookupVariable expression
        checkSpinePairIntermediate limits $ value `quot` divisor
  LengthModulo divisor expression
    | divisor == 0 -> Left LengthSpinePairEvaluationInternalModuloDivisorZero
    | otherwise -> do
        value <- evaluateSpinePairExpression limits lookupVariable expression
        checkSpinePairIntermediate limits $ value `mod` divisor
  LengthMonus left right -> do
    leftValue <- evaluateSpinePairExpression limits lookupVariable left
    rightValue <- evaluateSpinePairExpression limits lookupVariable right
    checkSpinePairIntermediate limits $ leftValue `monus` rightValue
  LengthMinimum left right -> binary min left right
  LengthMaximum left right -> binary max left right
  LengthIf condition whenTrue whenFalse -> do
    selected <- evaluateSpinePairFormula limits lookupVariable condition
    evaluateSpinePairExpression limits lookupVariable
      $ if selected then whenTrue else whenFalse
 where
  add total term = do
    value <- evaluateSpinePairExpression limits lookupVariable term
    checkSpinePairIntermediate limits $ total + value

  binary operation left right = do
    leftValue <- evaluateSpinePairExpression limits lookupVariable left
    rightValue <- evaluateSpinePairExpression limits lookupVariable right
    checkSpinePairIntermediate limits $ operation leftValue rightValue

evaluateSpinePairFormula
  :: LengthEvaluationLimits
  -> (variable -> Either LengthSpinePairEvaluationError Natural)
  -> LengthFormula variable
  -> Either LengthSpinePairEvaluationError Bool
evaluateSpinePairFormula limits lookupVariable source = case source of
  LengthTruth value -> Right value
  LengthEqual left right -> compareWith (==) left right
  LengthAtMost left right -> compareWith (<=) left right
  LengthNot formula -> not <$>
    evaluateSpinePairFormula limits lookupVariable formula
  LengthAll formulas -> allM formulas
 where
  compareWith relation left right = do
    leftValue <- evaluateSpinePairExpression limits lookupVariable left
    rightValue <- evaluateSpinePairExpression limits lookupVariable right
    pure $ relation leftValue rightValue

  allM [] = Right True
  allM (formula : remaining) = do
    value <- evaluateSpinePairFormula limits lookupVariable formula
    if value then allM remaining else Right False

checkAssignedValue
  :: LengthEvaluationLimits
  -> LengthEvaluationValueSite
  -> Natural
  -> Either LengthEvaluationError ()
checkAssignedValue limits site value =
  checkValueWithin site (lengthAssignmentValueBitLimit limits) value

checkIntermediate
  :: LengthEvaluationLimits
  -> Natural
  -> Either LengthEvaluationError Natural
checkIntermediate limits value = value <$ checkValueWithin
  LengthIntermediateValue (lengthIntermediateValueBitLimit limits) value

checkValueWithin
  :: LengthEvaluationValueSite
  -> Int
  -> Natural
  -> Either LengthEvaluationError ()
checkValueWithin site maximumBits value =
  let observedBits = observedNaturalBits maximumBits value
  in unless (observedBits <= maximumBits) $ Left
      $ LengthEvaluationValueBitLimitExceeded
          site maximumBits observedBits

evaluateExpression
  :: LengthEvaluationLimits
  -> (variable -> Either LengthEvaluationError Natural)
  -> LengthExpression variable
  -> Either LengthEvaluationError Natural
evaluateExpression limits lookupVariable source = case source of
  LengthVariable variable -> lookupVariable variable
  LengthLiteral value -> checkIntermediate limits value
  LengthSum terms -> foldM add 0 terms
  LengthScale factor expression -> do
    value <- evaluateExpression limits lookupVariable expression
    checkIntermediate limits $ factor * value
  LengthQuotient divisor expression
    | divisor == 0 -> Left LengthEvaluationInternalQuotientDivisorZero
    | otherwise -> do
        value <- evaluateExpression limits lookupVariable expression
        checkIntermediate limits $ value `quot` divisor
  LengthModulo divisor expression
    | divisor == 0 -> Left LengthEvaluationInternalModuloDivisorZero
    | otherwise -> do
        value <- evaluateExpression limits lookupVariable expression
        checkIntermediate limits $ value `mod` divisor
  LengthMonus left right -> do
    leftValue <- evaluateExpression limits lookupVariable left
    rightValue <- evaluateExpression limits lookupVariable right
    checkIntermediate limits $ leftValue `monus` rightValue
  LengthMinimum left right -> binary min left right
  LengthMaximum left right -> binary max left right
  LengthIf condition whenTrue whenFalse -> do
    selected <- evaluateFormula limits lookupVariable condition
    evaluateExpression limits lookupVariable
      $ if selected then whenTrue else whenFalse
 where
  add total term = do
    value <- evaluateExpression limits lookupVariable term
    checkIntermediate limits $ total + value

  binary operation left right = do
    leftValue <- evaluateExpression limits lookupVariable left
    rightValue <- evaluateExpression limits lookupVariable right
    checkIntermediate limits $ operation leftValue rightValue

evaluateFormula
  :: LengthEvaluationLimits
  -> (variable -> Either LengthEvaluationError Natural)
  -> LengthFormula variable
  -> Either LengthEvaluationError Bool
evaluateFormula limits lookupVariable source = case source of
  LengthTruth value -> Right value
  LengthEqual left right -> compareWith (==) left right
  LengthAtMost left right -> compareWith (<=) left right
  LengthNot formula -> not <$> evaluateFormula limits lookupVariable formula
  LengthAll formulas -> allM formulas
 where
  compareWith relation left right = do
    leftValue <- evaluateExpression limits lookupVariable left
    rightValue <- evaluateExpression limits lookupVariable right
    pure $ relation leftValue rightValue

  allM [] = Right True
  allM (formula : remaining) = do
    value <- evaluateFormula limits lookupVariable formula
    if value then allM remaining else Right False

monus :: Natural -> Natural -> Natural
monus left right
  | left >= right = left - right
  | otherwise = 0

indexNatural :: Natural -> [value] -> Maybe value
indexNatural 0 (value : _) = Just value
indexNatural position (_ : remaining) = indexNatural (position - 1) remaining
indexNatural _ [] = Nothing
