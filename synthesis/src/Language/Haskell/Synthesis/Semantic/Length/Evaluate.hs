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
-- schema-bound verifier, exact box, total and precondition-applicable assignment
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
  , validatedLengthApplicableDomainInclusiveMaximumBoxes
  , validatedLengthApplicableDomainBoxCount
  , validatedLengthApplicableDomainAssignmentVisitCount
  , validatedLengthApplicableDomainAssignmentCount
  , validatedLengthApplicableDomainApplicableAssignmentCount
  , validatedLengthApplicableDomainBasis
  , LengthSpinePairApplicableDomainValidationError (..)
  , ValidatedLengthSpinePairApplicableDomain
  , validatedLengthSpinePairApplicableDomainInclusiveMaximumBoxes
  , validatedLengthSpinePairApplicableDomainBoxCount
  , validatedLengthSpinePairApplicableDomainAssignmentVisitCount
  , validatedLengthSpinePairApplicableDomainAssignmentCount
  , validatedLengthSpinePairApplicableDomainApplicableAssignmentCount
  , validatedLengthSpinePairApplicableDomainBasis
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
  ) where

import Data.Maybe (catMaybes, fromMaybe)
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
  ( BehavioralProblem
  , BehavioralEvidence
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

-- | Identity of the one signed input-box bound that sealing can reject.  The
-- assignment cap is a 'Natural' and therefore has no field here.
data LengthInputBoxLimitField = LengthInputBoxMaximumInputs
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthInputBoxLimitField

-- | Failure to seal 'LengthInputBoxLimitSource': the named field carried the
-- retained negative value.
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

-- | Maximum number of raw disjunctive branches the checked precondition may
-- expand into before any branch is closed.
lengthBooleanFiniteUnionGeneratedBranchLimit
  :: LengthBooleanFiniteUnionLimits -> Int
lengthBooleanFiniteUnionGeneratedBranchLimit
    (LengthBooleanFiniteUnionLimits branches _ _ _ _) = branches

-- | Maximum number of relational rules collected within one branch.
lengthBooleanFiniteUnionRuleLimitPerBranch
  :: LengthBooleanFiniteUnionLimits -> Int
lengthBooleanFiniteUnionRuleLimitPerBranch
    (LengthBooleanFiniteUnionLimits _ rules _ _ _) = rules

-- | Maximum number of rule inspections the bound closure of one branch may
-- attempt across all of its passes.
lengthBooleanFiniteUnionClosureInspectionLimitPerBranch
  :: LengthBooleanFiniteUnionLimits -> Int
lengthBooleanFiniteUnionClosureInspectionLimitPerBranch
    (LengthBooleanFiniteUnionLimits _ _ inspections _ _) = inspections

-- | Maximum number of componentwise-maximal boxes retained after closure.
lengthBooleanFiniteUnionRetainedBoxLimit
  :: LengthBooleanFiniteUnionLimits -> Int
lengthBooleanFiniteUnionRetainedBoxLimit
    (LengthBooleanFiniteUnionLimits _ _ _ boxes _) = boxes

-- | Maximum sum of retained-box cardinalities, counted per box before
-- overlapping assignments are deduplicated by the union set.
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
-- through the current bounded guarded recursive piecewise-affine coverage
-- rule.
--
-- Preconditions are already bounded and normalized by contract sealing. The
-- analysis expands their Boolean structure, admits its exact supported affine
-- consequences, and returns the first compact input still missing from any
-- live finite-union branch.
data LengthApplicableDomainInapplicability
  = LengthApplicableDomainInputUpperBoundMissing !Int
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthApplicableDomainInapplicability

-- | Complete classification of one attempt to validate a problem's entire
-- applicable input domain.
--
-- Inapplicability is an ordinary conservative result: the current analysis
-- could not derive a finite union of boxes. Counterexample and established
-- payloads retain authority only through their opaque evidence values.
data LengthApplicableDomainValidation counterexample established
  = LengthApplicableDomainInapplicable
      !LengthApplicableDomainInapplicability
  | LengthApplicableDomainCounterexample !counterexample
  | LengthApplicableDomainEstablished !established
  deriving (Eq, Ord, Show, Generic)

instance (NFData counterexample, NFData established) =>
    NFData (LengthApplicableDomainValidation counterexample established)

-- | Fixed-precedence operational failures for scalar applicable-domain
-- establishment. Branch indices refer to canonical post-deduplication,
-- post-subsumption DNF order; box indices refer to the canonical retained
-- componentwise-maximal antichain.
data LengthApplicableDomainValidationError
  = LengthApplicableDomainProblemInputLimitExceeded !Int !Int
  | LengthApplicableDomainGeneratedBranchLimitExceeded !Int !Int
  | LengthApplicableDomainRuleLimitExceeded !Int !Int !Int
  | LengthApplicableDomainClosureInspectionLimitExceeded !Int !Int !Int
  | LengthApplicableDomainRetainedBoxLimitExceeded !Int !Int
  | LengthApplicableDomainMaximumValueRejected
      !Int !Int !LengthEvaluationError
  | LengthApplicableDomainAssignmentVisitLimitExceeded !Int !Int
  | LengthApplicableDomainAssignmentLimitExceeded !Natural !Natural
  | LengthApplicableDomainAssignmentEvaluationRejected
      !Natural !LengthEvaluationError
  | LengthApplicableDomainInternalEnumerationInvariant
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthApplicableDomainValidationError

-- | Nominal binary-product failure vocabulary for the same bounded
-- applicable-domain algorithm.
data LengthSpinePairApplicableDomainValidationError
  = LengthSpinePairApplicableDomainProblemInputLimitExceeded !Int !Int
  | LengthSpinePairApplicableDomainGeneratedBranchLimitExceeded !Int !Int
  | LengthSpinePairApplicableDomainRuleLimitExceeded !Int !Int !Int
  | LengthSpinePairApplicableDomainClosureInspectionLimitExceeded
      !Int !Int !Int
  | LengthSpinePairApplicableDomainRetainedBoxLimitExceeded !Int !Int
  | LengthSpinePairApplicableDomainMaximumValueRejected
      !Int !Int !LengthSpinePairEvaluationError
  | LengthSpinePairApplicableDomainAssignmentVisitLimitExceeded !Int !Int
  | LengthSpinePairApplicableDomainAssignmentLimitExceeded
      !Natural !Natural
  | LengthSpinePairApplicableDomainAssignmentEvaluationRejected
      !Natural !LengthSpinePairEvaluationError
  | LengthSpinePairApplicableDomainInternalEnumerationInvariant
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSpinePairApplicableDomainValidationError

-- These receipt-only schema identities name the complete current guarded
-- recursive piecewise-affine finite-union algorithm. They are deliberately
-- private: callers consume the opaque current receipts instead.
lengthApplicableDomainValidationSchemaTag :: [Word8]
lengthApplicableDomainValidationSchemaTag = ascii
  "finite-list-spine-length/guarded-recursive-piecewise-affine-finite-union-precondition-domain-establishment/v1"

lengthSpinePairApplicableDomainValidationSchemaTag :: [Word8]
lengthSpinePairApplicableDomainValidationSchemaTag = ascii
  "finite-binary-product-spine-lengths/guarded-recursive-piecewise-affine-finite-union-precondition-domain-establishment/v1"

-- | Opaque scalar receipt for the complete current applicable-domain
-- algorithm. Incomparable boxes remain separate; assignment visits count
-- every retained-box traversal, while assignment count is the exact
-- cardinality of their deduplicated union.
data ValidatedLengthApplicableDomain =
  ValidatedLengthApplicableDomainReceipt
    ![Word8]
    ![[Natural]]
    !Natural
    !Natural
    !Natural
    !LengthCounterexampleBasis
  deriving (Eq, Ord, Show)

instance NFData ValidatedLengthApplicableDomain where
  rnf
      (ValidatedLengthApplicableDomainReceipt
        schema boxes visits assignments applicable basis) =
    rnf schema `seq` rnf boxes `seq` rnf visits `seq` rnf assignments `seq`
    rnf applicable `seq` rnf basis

-- | The retained componentwise-maximal zero-origin boxes, in ascending
-- lexicographic order.  Each box lists an inclusive source-ordered maximum for
-- every modeled input; no box is componentwise dominated by another.
validatedLengthApplicableDomainInclusiveMaximumBoxes
  :: ValidatedLengthApplicableDomain
  -> [[Natural]]
validatedLengthApplicableDomainInclusiveMaximumBoxes
    (ValidatedLengthApplicableDomainReceipt _ boxes _ _ _ _) = boxes

-- | Number of retained boxes.
validatedLengthApplicableDomainBoxCount
  :: ValidatedLengthApplicableDomain
  -> Natural
validatedLengthApplicableDomainBoxCount
    (ValidatedLengthApplicableDomainReceipt _ boxes _ _ _ _) =
  fromIntegral $ length boxes

-- | Sum of the retained box cardinalities.  An assignment shared by several
-- boxes is counted once per box, so this is at least
-- 'validatedLengthApplicableDomainAssignmentCount'.
validatedLengthApplicableDomainAssignmentVisitCount
  :: ValidatedLengthApplicableDomain
  -> Natural
validatedLengthApplicableDomainAssignmentVisitCount
    (ValidatedLengthApplicableDomainReceipt _ _ visits _ _ _) = visits

-- | Exact cardinality of the deduplicated union of the retained boxes; every
-- such assignment was replayed against the exact problem.
validatedLengthApplicableDomainAssignmentCount
  :: ValidatedLengthApplicableDomain
  -> Natural
validatedLengthApplicableDomainAssignmentCount
    (ValidatedLengthApplicableDomainReceipt _ _ _ assignments _ _) =
  assignments

-- | Number of replayed assignments for which the precondition held.
validatedLengthApplicableDomainApplicableAssignmentCount
  :: ValidatedLengthApplicableDomain
  -> Natural
validatedLengthApplicableDomainApplicableAssignmentCount
    (ValidatedLengthApplicableDomainReceipt _ _ _ _ applicable _) = applicable

-- | Provider-independent or assumed-provider-relative semantic basis.
validatedLengthApplicableDomainBasis
  :: ValidatedLengthApplicableDomain
  -> LengthCounterexampleBasis
validatedLengthApplicableDomainBasis
    (ValidatedLengthApplicableDomainReceipt _ _ _ _ _ basis) = basis

-- | Opaque nominal binary-product sibling of the current scalar receipt.
data ValidatedLengthSpinePairApplicableDomain =
  ValidatedLengthSpinePairApplicableDomainReceipt
    ![Word8]
    ![[Natural]]
    !Natural
    !Natural
    !Natural
    !LengthCounterexampleBasis
  deriving (Eq, Ord, Show)

instance NFData ValidatedLengthSpinePairApplicableDomain where
  rnf
      (ValidatedLengthSpinePairApplicableDomainReceipt
        schema boxes visits assignments applicable basis) =
    rnf schema `seq` rnf boxes `seq` rnf visits `seq` rnf assignments `seq`
    rnf applicable `seq` rnf basis

-- | The retained componentwise-maximal zero-origin boxes, in ascending
-- lexicographic order.  Each box lists an inclusive source-ordered maximum for
-- every compact modeled input; no box is componentwise dominated by another.
validatedLengthSpinePairApplicableDomainInclusiveMaximumBoxes
  :: ValidatedLengthSpinePairApplicableDomain
  -> [[Natural]]
validatedLengthSpinePairApplicableDomainInclusiveMaximumBoxes
    (ValidatedLengthSpinePairApplicableDomainReceipt _ boxes _ _ _ _) = boxes

-- | Number of retained boxes.
validatedLengthSpinePairApplicableDomainBoxCount
  :: ValidatedLengthSpinePairApplicableDomain
  -> Natural
validatedLengthSpinePairApplicableDomainBoxCount
    (ValidatedLengthSpinePairApplicableDomainReceipt _ boxes _ _ _ _) =
  fromIntegral $ length boxes

-- | Sum of the retained box cardinalities.  An assignment shared by several
-- boxes is counted once per box, so this is at least
-- 'validatedLengthSpinePairApplicableDomainAssignmentCount'.
validatedLengthSpinePairApplicableDomainAssignmentVisitCount
  :: ValidatedLengthSpinePairApplicableDomain
  -> Natural
validatedLengthSpinePairApplicableDomainAssignmentVisitCount
    (ValidatedLengthSpinePairApplicableDomainReceipt _ _ visits _ _ _) = visits

-- | Exact cardinality of the deduplicated union of the retained boxes; every
-- such assignment was replayed against the exact product problem.
validatedLengthSpinePairApplicableDomainAssignmentCount
  :: ValidatedLengthSpinePairApplicableDomain
  -> Natural
validatedLengthSpinePairApplicableDomainAssignmentCount
    (ValidatedLengthSpinePairApplicableDomainReceipt _ _ _ assignments _ _) =
  assignments

-- | Number of replayed assignments for which the precondition held.
validatedLengthSpinePairApplicableDomainApplicableAssignmentCount
  :: ValidatedLengthSpinePairApplicableDomain
  -> Natural
validatedLengthSpinePairApplicableDomainApplicableAssignmentCount
    (ValidatedLengthSpinePairApplicableDomainReceipt _ _ _ _ applicable _) =
  applicable

-- | Provider-independent or assumed-provider-relative semantic basis.
validatedLengthSpinePairApplicableDomainBasis
  :: ValidatedLengthSpinePairApplicableDomain
  -> LengthCounterexampleBasis
validatedLengthSpinePairApplicableDomainBasis
    (ValidatedLengthSpinePairApplicableDomainReceipt _ _ _ _ _ basis) = basis

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

-- | Source-ordered inputs of the independently revalidated product anchor.
validatedLengthSpinePairCounterexampleSimplificationOriginalInputs
  :: ValidatedLengthSpinePairCounterexampleSimplification
  -> [Natural]
validatedLengthSpinePairCounterexampleSimplificationOriginalInputs
    (ValidatedLengthSpinePairCounterexampleSimplificationReceipt _ original
      _ _) = original

-- | Search assignments inspected through and including the returned hit.
-- The separate anchor replay is not counted.
validatedLengthSpinePairCounterexampleSimplificationInspectedAssignmentCount
  :: ValidatedLengthSpinePairCounterexampleSimplification
  -> Natural
validatedLengthSpinePairCounterexampleSimplificationInspectedAssignmentCount
    (ValidatedLengthSpinePairCounterexampleSimplificationReceipt _ _
      inspected _) = inspected

-- | Fresh ordinary product counterexample found by exact-problem bounded
-- replay.
validatedLengthSpinePairCounterexampleSimplificationCounterexample
  :: ValidatedLengthSpinePairCounterexampleSimplification
  -> ValidatedLengthSpinePairCounterexample
validatedLengthSpinePairCounterexampleSimplificationCounterexample
    (ValidatedLengthSpinePairCounterexampleSimplificationReceipt _ _ _
      value) = value

-- | Source-ordered inputs of the simplified ordinary product counterexample.
validatedLengthSpinePairCounterexampleSimplificationInputs
  :: ValidatedLengthSpinePairCounterexampleSimplification
  -> [Natural]
validatedLengthSpinePairCounterexampleSimplificationInputs =
  validatedLengthSpinePairCounterexampleInputs .
    validatedLengthSpinePairCounterexampleSimplificationCounterexample

-- | Both source-ordered result lengths recomputed for the simplified inputs.
validatedLengthSpinePairCounterexampleSimplificationResult
  :: ValidatedLengthSpinePairCounterexampleSimplification
  -> LengthSpinePair Natural
validatedLengthSpinePairCounterexampleSimplificationResult =
  validatedLengthSpinePairCounterexampleResult .
    validatedLengthSpinePairCounterexampleSimplificationCounterexample

-- | Provider-independent or assumed-provider-relative basis of the fresh
-- ordinary product counterexample.
validatedLengthSpinePairCounterexampleSimplificationBasis
  :: ValidatedLengthSpinePairCounterexampleSimplification
  -> LengthCounterexampleBasis
validatedLengthSpinePairCounterexampleSimplificationBasis =
  validatedLengthSpinePairCounterexampleBasis .
    validatedLengthSpinePairCounterexampleSimplificationCounterexample

-- | Always 'True': an opaque receipt is constructed only for a strict input
-- vector change.  @Right Nothing@ represents both admission unavailability
-- and an admitted search whose first counterexample is the anchor itself.
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
  inputs <- exactAssignment
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

-- Shared scalar/product evaluation core -------------------------------------

-- | The three-way outcome both domains' assignment replays share, with the
-- domain's violation receipt as the only payload.
data ProblemReplayView counterexample
  = ReplayPreconditionNotMet
  | ReplayPostconditionSatisfied
  | ReplayPostconditionViolated counterexample

-- | Everything that distinguishes the scalar and binary-product domains in
-- the shared counterexample-validation, input-box, and simplification cores
-- below: problem projections, the assignment replay (already mapped onto
-- 'ProblemReplayView'), the per-input value check, and each result family's
-- error constructors and receipt makers.  The public per-domain entrances
-- are instances of this record; their nominal signatures, errors, receipts,
-- and evidence domains are unchanged.
data ProblemEvaluationDomain problem domainTag evalError counterexample
    boxError boxReceipt simpError simpReceipt
  = ProblemEvaluationDomain
  { domainProblemInputCount :: problem -> Int
  , domainBehavioralProblem :: problem -> BehavioralProblem domainTag
  , domainProblemBasis :: problem -> LengthCounterexampleBasis
  , domainReplayAssignment
      :: LengthEvaluationLimits -> problem -> LengthProblemAssignment
      -> Either evalError (ProblemReplayView counterexample)
  , domainCheckInputValue
      :: LengthEvaluationLimits -> Int -> Natural -> Either evalError ()
  , domainCounterexampleInputs :: counterexample -> [Natural]
  , domainBoxInputLimitExceeded :: Int -> Int -> boxError
  , domainBoxBoundsArityMismatch :: Int -> Int -> boxError
  , domainBoxMaximumValueRejected :: Int -> evalError -> boxError
  , domainBoxAssignmentLimitExceeded :: Natural -> Natural -> boxError
  , domainBoxAssignmentEvaluationRejected
      :: Natural -> evalError -> boxError
  , domainBoxEnumerationInvariant :: boxError
  , domainBoxReceipt
      :: [Natural] -> Natural -> Natural -> LengthCounterexampleBasis
      -> boxReceipt
  , domainSimplificationBoxRejected :: boxError -> simpError
  , domainSimplificationAnchorEvaluationRejected :: evalError -> simpError
  , domainSimplificationAnchorNotCounterexample :: simpError
  , domainSimplificationInternalInvariant :: simpError
  , domainSimplificationReceipt
      :: [Natural] -> Natural -> counterexample -> simpReceipt
  }

scalarReplayView
  :: LengthProblemAssignmentReplay
  -> ProblemReplayView ValidatedLengthCounterexample
scalarReplayView replay = case replay of
  LengthProblemPreconditionNotMet -> ReplayPreconditionNotMet
  LengthProblemPostconditionSatisfied -> ReplayPostconditionSatisfied
  LengthProblemPostconditionViolated receipt ->
    ReplayPostconditionViolated receipt

spinePairReplayView
  :: LengthSpinePairProblemAssignmentReplay
  -> ProblemReplayView ValidatedLengthSpinePairCounterexample
spinePairReplayView replay = case replay of
  LengthSpinePairProblemPreconditionNotMet -> ReplayPreconditionNotMet
  LengthSpinePairProblemPostconditionSatisfied ->
    ReplayPostconditionSatisfied
  LengthSpinePairProblemPostconditionViolated receipt ->
    ReplayPostconditionViolated receipt

scalarEvaluationDomain
  :: ProblemEvaluationDomain
      (CheckedLengthProblem identity local)
      FiniteListSpineLengthV1
      LengthEvaluationError
      ValidatedLengthCounterexample
      LengthInputBoxValidationError
      ValidatedLengthInputBox
      LengthCounterexampleSimplificationError
      ValidatedLengthCounterexampleSimplification
scalarEvaluationDomain = ProblemEvaluationDomain
  { domainProblemInputCount = checkedLengthProblemInputCount
  , domainBehavioralProblem = checkedLengthProblemBehavioralProblem
  , domainProblemBasis = problemBasis
  , domainReplayAssignment = \limits problem ->
      fmap scalarReplayView . replayLengthProblemAssignment limits problem
  , domainCheckInputValue = \limits index ->
      checkAssignedValue limits $ LengthProblemInputValue index
  , domainCounterexampleInputs = validatedLengthCounterexampleInputs
  , domainBoxInputLimitExceeded = LengthInputBoxProblemInputLimitExceeded
  , domainBoxBoundsArityMismatch = LengthInputBoxBoundsArityMismatch
  , domainBoxMaximumValueRejected = LengthInputBoxMaximumValueRejected
  , domainBoxAssignmentLimitExceeded = LengthInputBoxAssignmentLimitExceeded
  , domainBoxAssignmentEvaluationRejected =
      LengthInputBoxAssignmentEvaluationRejected
  , domainBoxEnumerationInvariant =
      LengthInputBoxInternalEnumerationInvariant
  , domainBoxReceipt =
      ValidatedLengthInputBoxReceipt lengthInputBoxValidationSchemaTag
  , domainSimplificationBoxRejected =
      LengthCounterexampleSimplificationInputBoxValidationRejected
  , domainSimplificationAnchorEvaluationRejected =
      LengthCounterexampleSimplificationAnchorEvaluationRejected
  , domainSimplificationAnchorNotCounterexample =
      LengthCounterexampleSimplificationAnchorNotCounterexample
  , domainSimplificationInternalInvariant =
      LengthCounterexampleSimplificationInternalInvariant
  , domainSimplificationReceipt =
      ValidatedLengthCounterexampleSimplificationReceipt
        lengthCounterexampleSimplificationSchemaTag
  }

spinePairEvaluationDomain
  :: ProblemEvaluationDomain
      (CheckedLengthSpinePairProblem identity local)
      FiniteBinaryProductSpineLengthsV1
      LengthSpinePairEvaluationError
      ValidatedLengthSpinePairCounterexample
      LengthSpinePairInputBoxValidationError
      ValidatedLengthSpinePairInputBox
      LengthSpinePairCounterexampleSimplificationError
      ValidatedLengthSpinePairCounterexampleSimplification
spinePairEvaluationDomain = ProblemEvaluationDomain
  { domainProblemInputCount = checkedLengthSpinePairProblemInputCount
  , domainBehavioralProblem = checkedLengthSpinePairProblemBehavioralProblem
  , domainProblemBasis = spinePairProblemBasis
  , domainReplayAssignment = \limits problem ->
      fmap spinePairReplayView
        . replayLengthSpinePairProblemAssignment limits problem
  , domainCheckInputValue = \limits index ->
      checkSpinePairAssignedValue limits
        $ LengthSpinePairProblemInputValue index
  , domainCounterexampleInputs = validatedLengthSpinePairCounterexampleInputs
  , domainBoxInputLimitExceeded =
      LengthSpinePairInputBoxProblemInputLimitExceeded
  , domainBoxBoundsArityMismatch = LengthSpinePairInputBoxBoundsArityMismatch
  , domainBoxMaximumValueRejected =
      LengthSpinePairInputBoxMaximumValueRejected
  , domainBoxAssignmentLimitExceeded =
      LengthSpinePairInputBoxAssignmentLimitExceeded
  , domainBoxAssignmentEvaluationRejected =
      LengthSpinePairInputBoxAssignmentEvaluationRejected
  , domainBoxEnumerationInvariant =
      LengthSpinePairInputBoxInternalEnumerationInvariant
  , domainBoxReceipt =
      ValidatedLengthSpinePairInputBoxReceipt
        lengthSpinePairInputBoxValidationSchemaTag
  , domainSimplificationBoxRejected =
      LengthSpinePairCounterexampleSimplificationInputBoxValidationRejected
  , domainSimplificationAnchorEvaluationRejected =
      LengthSpinePairCounterexampleSimplificationAnchorEvaluationRejected
  , domainSimplificationAnchorNotCounterexample =
      LengthSpinePairCounterexampleSimplificationAnchorNotCounterexample
  , domainSimplificationInternalInvariant =
      LengthSpinePairCounterexampleSimplificationInternalInvariant
  , domainSimplificationReceipt =
      ValidatedLengthSpinePairCounterexampleSimplificationReceipt
        lengthSpinePairCounterexampleSimplificationSchemaTag
  }

validateProblemCounterexampleWith
  :: ProblemEvaluationDomain problem domainTag evalError counterexample
      boxError boxReceipt simpError simpReceipt
  -> LengthEvaluationLimits
  -> problem
  -> LengthProblemAssignment
  -> Either evalError (Maybe (BehavioralEvidence domainTag counterexample))
validateProblemCounterexampleWith domain limits problem assignment = do
  replay <- domainReplayAssignment domain limits problem assignment
  pure $ case replay of
    ReplayPreconditionNotMet -> Nothing
    ReplayPostconditionSatisfied -> Nothing
    ReplayPostconditionViolated receipt -> Just
      $ mkBehavioralEvidence (domainBehavioralProblem domain problem) receipt

validateProblemInputBoxWith
  :: ProblemEvaluationDomain problem domainTag evalError counterexample
      boxError boxReceipt simpError simpReceipt
  -> LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> problem
  -> [Natural]
  -> Either boxError
      (LengthInputBoxValidation
        (BehavioralEvidence domainTag counterexample)
        (BehavioralEvidence domainTag boxReceipt))
validateProblemInputBoxWith domain evaluationLimits inputBoxLimits problem
    rawMaximums = do
  let inputCount = domainProblemInputCount domain problem
      maximumInputs = lengthInputBoxInputLimit inputBoxLimits
  if inputCount <= maximumInputs
    then pure ()
    else Left $ domainBoxInputLimitExceeded domain
      maximumInputs inputCount
  maximums <- exactAssignment (domainBoxBoundsArityMismatch domain)
    inputCount rawMaximums
  mapM_ checkMaximum $ zip [0 ..] maximums
  assignmentCount <- inputBoxAssignmentCountWith
    (domainBoxAssignmentLimitExceeded domain) inputBoxLimits maximums
  enumerate maximums assignmentCount 0 0 $ replicate (length maximums) 0
 where
  checkMaximum (index, value) = either
    (Left . domainBoxMaximumValueRejected domain index)
    Right
    $ domainCheckInputValue domain evaluationLimits index value

  enumerate maximums assignmentCount !ordinal !applicable inputs = do
    replay <- either
      (Left . domainBoxAssignmentEvaluationRejected domain ordinal)
      Right
      $ domainReplayAssignment domain evaluationLimits problem
          $ LengthProblemAssignment inputs
    case replay of
      ReplayPostconditionViolated receipt -> Right
        $ LengthInputBoxCounterexample
        $ mkBehavioralEvidence
            (domainBehavioralProblem domain problem) receipt
      ReplayPreconditionNotMet -> continue maximums assignmentCount
        ordinal applicable inputs
      ReplayPostconditionSatisfied -> continue maximums assignmentCount
        ordinal (applicable + 1) inputs

  continue maximums assignmentCount !ordinal !applicable inputs =
    case nextInputBoxAssignmentWith
        (domainBoxEnumerationInvariant domain) maximums inputs of
      Left failure -> Left failure
      Right (Just following) -> enumerate maximums assignmentCount
        (ordinal + 1) applicable following
      Right Nothing
        | ordinal + 1 /= assignmentCount ->
            Left $ domainBoxEnumerationInvariant domain
        | otherwise ->
            let receipt = domainBoxReceipt domain
                  maximums assignmentCount applicable
                  $ domainProblemBasis domain problem
            in Right $ LengthInputBoxValidated
              $ mkBehavioralEvidence
                  (domainBehavioralProblem domain problem) receipt

simplifyProblemCounterexampleWith
  :: ProblemEvaluationDomain problem domainTag evalError counterexample
      boxError boxReceipt simpError simpReceipt
  -> LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> problem
  -> counterexample
  -> Either simpError (Maybe (BehavioralEvidence domainTag simpReceipt))
simplifyProblemCounterexampleWith domain evaluationLimits inputBoxLimits
    problem anchor = do
  admitted <- admit
  case admitted of
    Nothing -> Right Nothing
    Just maximums -> do
      anchorReplay <- either
        (Left . domainSimplificationAnchorEvaluationRejected domain)
        Right
        $ domainReplayAssignment domain evaluationLimits problem
        $ LengthProblemAssignment maximums
      case anchorReplay of
        ReplayPostconditionViolated _ -> pure ()
        ReplayPreconditionNotMet -> Left
          $ domainSimplificationAnchorNotCounterexample domain
        ReplayPostconditionSatisfied -> Left
          $ domainSimplificationAnchorNotCounterexample domain
      validation <- either
        (Left . domainSimplificationBoxRejected domain)
        Right
        $ validateProblemInputBoxWith domain evaluationLimits
            inputBoxLimits problem maximums
      case validation of
        LengthInputBoxValidated _ -> Left
          $ domainSimplificationInternalInvariant domain
        LengthInputBoxCounterexample evidence -> do
          counterexample <- either
            (const $ Left $ domainSimplificationInternalInvariant domain)
            Right
            $ replayBehavioralEvidence
                (domainBehavioralProblem domain problem) evidence
          let simplifiedInputs =
                domainCounterexampleInputs domain counterexample
          if simplifiedInputs == maximums
            then Right Nothing
            else do
              inspected <- case inputBoxInspectedAssignmentCount
                  maximums simplifiedInputs of
                Nothing -> Left
                  $ domainSimplificationInternalInvariant domain
                Just value -> Right value
              let receipt = domainSimplificationReceipt domain
                    maximums inspected counterexample
              Right $ Just
                $ mapBehavioralEvidenceReceipt (const receipt) evidence
 where
  originalInputs = domainCounterexampleInputs domain anchor
  inputCount = domainProblemInputCount domain problem

  admit
    | inputCount > lengthInputBoxInputLimit inputBoxLimits = Right Nothing
    | otherwise = do
        maximums <- either rejectInputBox Right
          $ exactAssignment (domainBoxBoundsArityMismatch domain)
              inputCount originalInputs
        mapM_ checkMaximum $ zip [0 ..] maximums
        -- The bounded assignment counter can only fail by exceeding its
        -- limit, which is an ordinary conservative miss at admission.
        case inputBoxAssignmentCountWith (\_ _ -> ())
            inputBoxLimits maximums of
          Left () -> Right Nothing
          Right _ -> Right $ Just maximums

  checkMaximum (index, value) = either
    (rejectInputBox . domainBoxMaximumValueRejected domain index)
    Right
    $ domainCheckInputValue domain evaluationLimits index value

  rejectInputBox = Left . domainSimplificationBoxRejected domain

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
validateLengthProblemCounterexample =
  validateProblemCounterexampleWith scalarEvaluationDomain

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
validateLengthSpinePairProblemCounterexample =
  validateProblemCounterexampleWith spinePairEvaluationDomain

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
simplifyLengthProblemCounterexample =
  simplifyProblemCounterexampleWith scalarEvaluationDomain

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
simplifyLengthSpinePairProblemCounterexample =
  simplifyProblemCounterexampleWith spinePairEvaluationDomain

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
validateLengthProblemInputBox =
  validateProblemInputBoxWith scalarEvaluationDomain

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
validateLengthSpinePairProblemInputBox =
  validateProblemInputBoxWith spinePairEvaluationDomain

-- | How one domain spells the applicable-domain rejections: the input
-- limit, the four preparation caps, the three enumeration caps, and the
-- two evaluation rejections.  Everything else in the shared core below --
-- admission, preparation, enumeration, replay, and evidence construction
-- -- has one order for both domains.
data ApplicableDomainVocabulary evalError adError
  = ApplicableDomainVocabulary
  { applicableProblemInputLimitExceeded :: Int -> Int -> adError
  , applicableGeneratedBranchLimitExceeded :: Int -> Int -> adError
  , applicableRuleLimitExceeded :: Int -> Int -> Int -> adError
  , applicableClosureInspectionLimitExceeded :: Int -> Int -> Int -> adError
  , applicableRetainedBoxLimitExceeded :: Int -> Int -> adError
  , applicableAssignmentVisitLimitExceeded :: Int -> Int -> adError
  , applicableAssignmentLimitExceeded :: Natural -> Natural -> adError
  , applicableInternalEnumerationInvariant :: adError
  , applicableMaximumValueRejected :: Int -> Int -> evalError -> adError
  , applicableAssignmentEvaluationRejected
      :: Natural -> evalError -> adError
  }

scalarApplicableDomainVocabulary
  :: ApplicableDomainVocabulary
      LengthEvaluationError
      LengthApplicableDomainValidationError
scalarApplicableDomainVocabulary = ApplicableDomainVocabulary
  { applicableProblemInputLimitExceeded =
      LengthApplicableDomainProblemInputLimitExceeded
  , applicableGeneratedBranchLimitExceeded =
      LengthApplicableDomainGeneratedBranchLimitExceeded
  , applicableRuleLimitExceeded = LengthApplicableDomainRuleLimitExceeded
  , applicableClosureInspectionLimitExceeded =
      LengthApplicableDomainClosureInspectionLimitExceeded
  , applicableRetainedBoxLimitExceeded =
      LengthApplicableDomainRetainedBoxLimitExceeded
  , applicableAssignmentVisitLimitExceeded =
      LengthApplicableDomainAssignmentVisitLimitExceeded
  , applicableAssignmentLimitExceeded =
      LengthApplicableDomainAssignmentLimitExceeded
  , applicableInternalEnumerationInvariant =
      LengthApplicableDomainInternalEnumerationInvariant
  , applicableMaximumValueRejected =
      LengthApplicableDomainMaximumValueRejected
  , applicableAssignmentEvaluationRejected =
      LengthApplicableDomainAssignmentEvaluationRejected
  }

spinePairApplicableDomainVocabulary
  :: ApplicableDomainVocabulary
      LengthSpinePairEvaluationError
      LengthSpinePairApplicableDomainValidationError
spinePairApplicableDomainVocabulary = ApplicableDomainVocabulary
  { applicableProblemInputLimitExceeded =
      LengthSpinePairApplicableDomainProblemInputLimitExceeded
  , applicableGeneratedBranchLimitExceeded =
      LengthSpinePairApplicableDomainGeneratedBranchLimitExceeded
  , applicableRuleLimitExceeded =
      LengthSpinePairApplicableDomainRuleLimitExceeded
  , applicableClosureInspectionLimitExceeded =
      LengthSpinePairApplicableDomainClosureInspectionLimitExceeded
  , applicableRetainedBoxLimitExceeded =
      LengthSpinePairApplicableDomainRetainedBoxLimitExceeded
  , applicableAssignmentVisitLimitExceeded =
      LengthSpinePairApplicableDomainAssignmentVisitLimitExceeded
  , applicableAssignmentLimitExceeded =
      LengthSpinePairApplicableDomainAssignmentLimitExceeded
  , applicableInternalEnumerationInvariant =
      LengthSpinePairApplicableDomainInternalEnumerationInvariant
  , applicableMaximumValueRejected =
      LengthSpinePairApplicableDomainMaximumValueRejected
  , applicableAssignmentEvaluationRejected =
      LengthSpinePairApplicableDomainAssignmentEvaluationRejected
  }

-- | Shared implementation of the current bounded finite-union validation.
-- The domain contributes its evaluation record, its rejection vocabulary,
-- its precondition coverage analysis (already closed over the domain's
-- contract-variable input positions), and its receipt constructor;
-- admission, enumeration, replay, and evidence construction have one order.
validateProblemApplicableDomainCore
  :: ProblemEvaluationDomain problem domainTag evalError counterexample
      boxError boxReceipt simpError simpReceipt
  -> ApplicableDomainVocabulary evalError adError
  -> (problem
      -> LengthBooleanFiniteUnionLimits
      -> Int
      -> Either
          BooleanFiniteUnionPreparationError
          (Either LengthApplicableDomainInapplicability [[Natural]]))
  -> ([[Natural]] -> Natural -> Natural -> Natural
      -> LengthCounterexampleBasis -> receipt)
  -> LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> LengthBooleanFiniteUnionLimits
  -> problem
  -> Either adError
      (LengthApplicableDomainValidation
        (BehavioralEvidence domainTag counterexample)
        (BehavioralEvidence domainTag receipt))
validateProblemApplicableDomainCore domain vocabulary coverageFor mkReceipt
    evaluationLimits inputBoxLimits unionLimits problem = do
  let inputCount = domainProblemInputCount domain problem
      maximumInputs = lengthInputBoxInputLimit inputBoxLimits
  if inputCount <= maximumInputs
    then pure ()
    else Left $ applicableProblemInputLimitExceeded vocabulary
      maximumInputs inputCount
  coverage <- either (Left . preparationError) Right
    $ coverageFor problem unionLimits inputCount
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
  preparationError failure = case failure of
    BooleanFiniteUnionGeneratedBranchLimitExceeded limit observed ->
      applicableGeneratedBranchLimitExceeded vocabulary limit observed
    BooleanFiniteUnionRuleLimitExceeded branch limit observed ->
      applicableRuleLimitExceeded vocabulary branch limit observed
    BooleanFiniteUnionClosureInspectionLimitExceeded branch limit observed ->
      applicableClosureInspectionLimitExceeded vocabulary
        branch limit observed
    BooleanFiniteUnionRetainedBoxLimitExceeded limit observed ->
      applicableRetainedBoxLimitExceeded vocabulary limit observed

  enumerationError failure = case failure of
    BooleanFiniteUnionAssignmentVisitLimitExceeded limit observed ->
      applicableAssignmentVisitLimitExceeded vocabulary limit observed
    BooleanFiniteUnionAssignmentLimitExceeded limit observed ->
      applicableAssignmentLimitExceeded vocabulary limit observed
    BooleanFiniteUnionInternalEnumerationInvariant ->
      applicableInternalEnumerationInvariant vocabulary

  checkBox (boxIndex, maximums) =
    mapM_ (checkMaximum boxIndex) $ zip [0 ..] maximums

  checkMaximum boxIndex (inputIndex, maximumValue) = either
    (Left . applicableMaximumValueRejected vocabulary boxIndex inputIndex)
    Right
    $ domainCheckInputValue domain evaluationLimits inputIndex maximumValue

  replay boxes visits assignmentCount !ordinal !applicable assignments =
    case assignments of
      []
        | ordinal /= assignmentCount ->
            Left $ applicableInternalEnumerationInvariant vocabulary
        | otherwise ->
            let receipt =
                  mkReceipt
                    boxes visits assignmentCount applicable
                    $ domainProblemBasis domain problem
            in Right $ LengthApplicableDomainEstablished
              $ mkBehavioralEvidence
                  (domainBehavioralProblem domain problem) receipt
      inputs : remaining -> do
        assignmentReplay <- either
          (Left . applicableAssignmentEvaluationRejected vocabulary ordinal)
          Right
          $ domainReplayAssignment domain evaluationLimits problem
          $ LengthProblemAssignment inputs
        case assignmentReplay of
          ReplayPostconditionViolated receipt -> Right
            $ LengthApplicableDomainCounterexample
            $ mkBehavioralEvidence
                (domainBehavioralProblem domain problem) receipt
          ReplayPreconditionNotMet ->
            replay boxes visits assignmentCount (ordinal + 1) applicable
              remaining
          ReplayPostconditionSatisfied ->
            replay boxes visits assignmentCount (ordinal + 1)
              (applicable + 1) remaining

scalarContractInputPosition :: LengthContractVariable -> Maybe Natural
scalarContractInputPosition variable = case variable of
  LengthInput position -> Just position
  LengthResult -> Nothing

spinePairContractInputPosition
  :: LengthSpinePairContractVariable -> Maybe Natural
spinePairContractInputPosition variable = case variable of
  LengthSpinePairInput position -> Just position
  LengthSpinePairResult _ -> Nothing

-- | Validate the complete applicable input domain of one exact scalar
-- problem with the current bounded guarded recursive piecewise-affine
-- analysis. The original checked precondition remains the sole replay
-- authority over the global deduplicated assignment union.
validateLengthProblemApplicableDomain
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> LengthBooleanFiniteUnionLimits
  -> CheckedLengthProblem identity local
  -> Either LengthApplicableDomainValidationError
      (LengthApplicableDomainValidation
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthCounterexample)
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthApplicableDomain))
validateLengthProblemApplicableDomain =
  validateProblemApplicableDomainCore
    scalarEvaluationDomain
    scalarApplicableDomainVocabulary
    (\problem unionLimits inputCount ->
      booleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomainMaximumBoxes
        unionLimits inputCount scalarContractInputPosition
        $ checkedLengthProblemPrecondition problem)
    (ValidatedLengthApplicableDomainReceipt
      lengthApplicableDomainValidationSchemaTag)

-- | Nominal binary-product sibling of current applicable-domain validation.
-- It shares every operational cap and precedence while producing only fresh
-- product-domain evidence.
validateLengthSpinePairProblemApplicableDomain
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> LengthBooleanFiniteUnionLimits
  -> CheckedLengthSpinePairProblem identity local
  -> Either LengthSpinePairApplicableDomainValidationError
      (LengthApplicableDomainValidation
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairCounterexample)
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairApplicableDomain))
validateLengthSpinePairProblemApplicableDomain =
  validateProblemApplicableDomainCore
    spinePairEvaluationDomain
    spinePairApplicableDomainVocabulary
    (\problem unionLimits inputCount ->
      booleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomainMaximumBoxes
        unionLimits inputCount spinePairContractInputPosition
        $ checkedLengthSpinePairProblemPrecondition problem)
    (ValidatedLengthSpinePairApplicableDomainReceipt
      lengthSpinePairApplicableDomainValidationSchemaTag)

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
-- is the exact natural/order split @not (A <= B) || not (B <= A)@.  The same
-- raw expansion is reused for expression-level conditional guards;
-- their recursive atom coverage is resolved by the guarded fallback below.
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

-- Formula-level Boolean expansion and complete atomic-or-recursive proof
-- expansion form one lazy witness stream.  The public generated-branch cap
-- therefore observes the full Cartesian product before any complement,
-- duplicate, absorption, guard-contradiction, or rule cleanup.
booleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineRawBranchWitnesses
  :: Int
  -> (variable -> Maybe Natural)
  -> LengthFormula variable
  -> [()]
booleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineRawBranchWitnesses
    inputCount inputPosition precondition =
  concatMap expandBranch $ booleanFiniteUnionRawBranches precondition
 where
  expandBranch [] = [()]
  expandBranch (literal : remaining) =
    [ ()
    | _ <-
        strictRelationalPositiveAffineQuotientRootExtremaMonusAtomicBranchingRecursivePiecewiseAffineClauseBranches
          inputCount inputPosition literal
    , _ <- expandBranch remaining
    ]

-- Once raw accounting succeeds, canonicalize the original formula branches
-- exactly as the predecessor does and re-expand their Set-ordered literals.
-- Coverage alternatives retain their original rule and guard order.
expandBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineBranch
  :: Int
  -> (variable -> Maybe Natural)
  -> Set.Set (LengthFormula variable)
  -> [[RelationalPositiveAffineClauseCoverage]]
expandBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineBranch
    inputCount inputPosition = expand . Set.toAscList
 where
  expand [] = [[]]
  expand (literal : remaining) =
    [ coverage : rest
    | coverage <-
        strictRelationalPositiveAffineQuotientRootExtremaMonusAtomicBranchingRecursivePiecewiseAffineClauseBranches
          inputCount inputPosition literal
    , rest <- expand remaining
    ]

-- Recursive piecewise-affine sibling of the atomic-branching finite-union
-- preparation pipeline.  Every downstream cap, closure, missing-input, and
-- antichain edge is intentionally inherited without a new limit vocabulary.
booleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomainMaximumBoxes
  :: Ord variable
  => LengthBooleanFiniteUnionLimits
  -> Int
  -> (variable -> Maybe Natural)
  -> LengthFormula variable
  -> Either
      BooleanFiniteUnionPreparationError
      (Either LengthApplicableDomainInapplicability [[Natural]])
booleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomainMaximumBoxes
    limits inputCount inputPosition precondition = do
  let rawFormulaBranches = booleanFiniteUnionRawBranches precondition
      rawBranchWitnesses =
        booleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineRawBranchWitnesses
          inputCount inputPosition precondition
      branchLimit = lengthBooleanFiniteUnionGeneratedBranchLimit limits
  case observeBooleanFiniteUnionListLength branchLimit rawBranchWitnesses of
    Left observed -> Left $ BooleanFiniteUnionGeneratedBranchLimitExceeded
      branchLimit observed
    Right _ -> pure ()
  let branches = concatMap
        (expandBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineBranch
          inputCount inputPosition)
        $ canonicalBooleanFiniteUnionBranches rawFormulaBranches
  closed <- mapM closeBranch $ zip [0 ..] branches
  let liveBounds = catMaybes closed
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
nextBooleanFiniteUnionAssignment =
  nextInputBoxAssignmentWith BooleanFiniteUnionInternalEnumerationInvariant

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

-- Signed affine values are private proof intermediates.  Negative constants
-- and coefficients arise only by selecting the positive branch of monus; they
-- are moved across each generated inequality before entering the existing
-- positive-sided closure engine.
data RecursivePiecewiseAffineSummary = RecursivePiecewiseAffineSummary
  !Integer
  !(Map.Map Natural Integer)

-- Coverage fragments remain separate until the enclosing relation is formed.
-- In particular, an impossible conditional guard retains its selected value
-- and therefore still participates in every surrounding Cartesian selector
-- product.  The raw generated-branch cap observes that complete product before
-- the fragments collapse to one branch-local contradiction.
data RecursivePiecewiseAffineBranch = RecursivePiecewiseAffineBranch
  ![RelationalPositiveAffineClauseCoverage]
  !RecursivePiecewiseAffineSummary

-- Atomic branching remains the first authority.  Recursive interpretation is
-- attempted only for its exact singleton-Ignored result and only for a
-- relational atom which retains an extrema, monus, or conditional constructor.
-- A conditional is all-or-nothing: both selected expressions and every leaf
-- of both the condition and its complement must be supported.  Any unsupported
-- child rejects that whole fallback atom.
strictRelationalPositiveAffineQuotientRootExtremaMonusAtomicBranchingRecursivePiecewiseAffineClauseBranches
  :: Int
  -> (variable -> Maybe Natural)
  -> LengthFormula variable
  -> [RelationalPositiveAffineClauseCoverage]
strictRelationalPositiveAffineQuotientRootExtremaMonusAtomicBranchingRecursivePiecewiseAffineClauseBranches
    inputCount inputPosition formula =
  case predecessorBranches of
    [RelationalPositiveAffineClauseIgnored]
      | hasRecursivePiecewiseAffineOperation formula ->
          fromMaybe predecessorBranches (recursiveFormulaBranches formula)
    _ -> predecessorBranches
 where
  predecessorBranches =
    strictRelationalPositiveAffineQuotientRootExtremaMonusAtomicBranchingClauseBranches
      inputCount inputPosition formula

  recursiveFormulaBranches source = case source of
    LengthAtMost left right -> relationBranches atMostRule left right
    LengthNot (LengthAtMost left right) ->
      relationBranches strictRule left right
    LengthEqual left right -> relationBranches equalityRules left right
    _ -> Nothing

  relationBranches appendRelation left right = do
    leftBranches <- expressionBranches left
    rightBranches <- expressionBranches right
    pure
      [ collapseRecursivePiecewiseAffineCoverage
          $ leftGuards ++ rightGuards
          ++ [ RelationalPositiveAffineClauseRules
                $ appendRelation leftValue rightValue
             ]
      | RecursivePiecewiseAffineBranch leftGuards leftValue <- leftBranches
      , RecursivePiecewiseAffineBranch rightGuards rightValue <- rightBranches
      ]

  atMostRule left right = [recursivePiecewiseAffineRule left right]

  strictRule left right =
    [ recursivePiecewiseAffineRule
        (incrementRecursivePiecewiseAffineSummary right)
        left
    ]

  equalityRules left right =
    [ recursivePiecewiseAffineRule left right
    , recursivePiecewiseAffineRule right left
    ]

  expressionBranches expression = case expression of
    LengthVariable variable -> do
      position <- inputPosition variable
      if position < fromIntegral inputCount
        then Just
          [ RecursivePiecewiseAffineBranch []
            $ RecursivePiecewiseAffineSummary 0
            $ Map.singleton position 1
          ]
        else Nothing
    LengthLiteral value -> Just
      [ RecursivePiecewiseAffineBranch []
        $ RecursivePiecewiseAffineSummary (toInteger value) Map.empty
      ]
    LengthSum terms -> foldM appendTerm [zeroBranch] terms
    LengthScale factor nested
      | factor == 0 -> Nothing
      | otherwise -> map (scaleBranch factor) <$> expressionBranches nested
    LengthMinimum left right ->
      selectBinary minimumSelections left right
    LengthMaximum left right ->
      selectBinary maximumSelections left right
    LengthMonus left right ->
      selectBinary monusSelections left right
    LengthQuotient _ _ -> Nothing
    LengthModulo _ _ -> Nothing
    LengthIf condition whenTrue whenFalse
      | not
          ( conditionFullySupported BooleanFiniteUnionPositive condition
          && conditionFullySupported BooleanFiniteUnionNegative condition
          ) -> Nothing
      | otherwise -> do
          trueBranches <- expressionBranches whenTrue
          falseBranches <- expressionBranches whenFalse
          pure
            $ conditionalArmBranches
                BooleanFiniteUnionPositive condition trueBranches
            ++ conditionalArmBranches
                BooleanFiniteUnionNegative condition falseBranches

  zeroBranch = RecursivePiecewiseAffineBranch []
    $ RecursivePiecewiseAffineSummary 0 Map.empty

  appendTerm accumulated term = do
    termBranches <- expressionBranches term
    pure
      [ RecursivePiecewiseAffineBranch
          (leftGuards ++ rightGuards)
          (addRecursivePiecewiseAffineSummaries leftValue rightValue)
      | RecursivePiecewiseAffineBranch leftGuards leftValue <- accumulated
      , RecursivePiecewiseAffineBranch rightGuards rightValue <- termBranches
      ]

  scaleBranch factor
      (RecursivePiecewiseAffineBranch guards value) =
    RecursivePiecewiseAffineBranch guards
      $ scaleRecursivePiecewiseAffineSummary factor value

  selectBinary selections left right = do
    leftBranches <- expressionBranches left
    rightBranches <- expressionBranches right
    pure $ concat
      [ map (prependDescendantGuards leftGuards rightGuards)
          $ selections leftValue rightValue
      | RecursivePiecewiseAffineBranch leftGuards leftValue <- leftBranches
      , RecursivePiecewiseAffineBranch rightGuards rightValue <- rightBranches
      ]

  prependDescendantGuards leftGuards rightGuards
      (RecursivePiecewiseAffineBranch selectorGuards value) =
    RecursivePiecewiseAffineBranch
      (leftGuards ++ rightGuards ++ selectorGuards) value

  minimumSelections left right =
    [ RecursivePiecewiseAffineBranch
        [ RelationalPositiveAffineClauseRules
            [recursivePiecewiseAffineRule left right]
        ]
        left
    , RecursivePiecewiseAffineBranch
        [ RelationalPositiveAffineClauseRules
            [ recursivePiecewiseAffineRule
                (incrementRecursivePiecewiseAffineSummary right)
                left
            ]
        ]
        right
    ]

  maximumSelections left right =
    [ RecursivePiecewiseAffineBranch
        [ RelationalPositiveAffineClauseRules
            [recursivePiecewiseAffineRule right left]
        ]
        left
    , RecursivePiecewiseAffineBranch
        [ RelationalPositiveAffineClauseRules
            [ recursivePiecewiseAffineRule
                (incrementRecursivePiecewiseAffineSummary left)
                right
            ]
        ]
        right
    ]

  monusSelections left right =
    [ RecursivePiecewiseAffineBranch
        [ RelationalPositiveAffineClauseRules
            [recursivePiecewiseAffineRule left right]
        ]
        $ RecursivePiecewiseAffineSummary 0 Map.empty
    , RecursivePiecewiseAffineBranch
        [ RelationalPositiveAffineClauseRules
            [ recursivePiecewiseAffineRule
                (incrementRecursivePiecewiseAffineSummary right)
                left
            ]
        ]
        $ subtractRecursivePiecewiseAffineSummaries left right
    ]

  -- A conditional's true arm precedes its false arm.  Within one arm, raw
  -- condition-DNF alternatives are outermost and selected-expression
  -- alternatives are innermost.  Condition coverage therefore precedes every
  -- selected-expression guard.  These generated selector alternatives are not
  -- independently canonicalized; the enclosing raw branch witness counts them
  -- before the existing original-formula cleanup.
  conditionalArmBranches polarity condition selectedBranches =
    [ RecursivePiecewiseAffineBranch
        (conditionGuards ++ selectedGuards) selectedValue
    | conditionGuards <- conditionalGuardBranches polarity condition
    , RecursivePiecewiseAffineBranch selectedGuards selectedValue <-
        selectedBranches
    ]

  conditionalGuardBranches polarity condition =
    concatMap expandConditionConjunction
      $ booleanFiniteUnionRawBranches
      $ case polarity of
          BooleanFiniteUnionPositive -> condition
          BooleanFiniteUnionNegative -> LengthNot condition

  expandConditionConjunction [] = [[]]
  expandConditionConjunction (literal : remaining) =
    [ coverage : following
    | coverage <- recursiveClauseBranches literal
    , following <- expandConditionConjunction remaining
    ]

  -- Full support is checked structurally before the lazy DNF stream above is
  -- constructed.  This preserves all-or-nothing admission without forcing an
  -- exponentially large condition expansion before the generated-branch cap.
  conditionFullySupported polarity source = case source of
    LengthTruth _ -> True
    LengthNot nested -> conditionFullySupported
      (oppositeBooleanFiniteUnionPolarity polarity) nested
    LengthAll formulas -> all (conditionFullySupported polarity) formulas
    LengthAtMost left right -> literalCoverageSupported $ case polarity of
      BooleanFiniteUnionPositive -> LengthAtMost left right
      BooleanFiniteUnionNegative -> LengthNot $ LengthAtMost left right
    LengthEqual left right -> case polarity of
      BooleanFiniteUnionPositive ->
        literalCoverageSupported $ LengthEqual left right
      BooleanFiniteUnionNegative ->
        literalCoverageSupported
          (LengthNot $ LengthAtMost left right)
        && literalCoverageSupported
          (LengthNot $ LengthAtMost right left)

  literalCoverageSupported literal = case recursiveClauseBranches literal of
    [RelationalPositiveAffineClauseIgnored] -> False
    _ -> True

  recursiveClauseBranches literal =
    strictRelationalPositiveAffineQuotientRootExtremaMonusAtomicBranchingRecursivePiecewiseAffineClauseBranches
      inputCount inputPosition literal

  oppositeBooleanFiniteUnionPolarity polarity = case polarity of
    BooleanFiniteUnionPositive -> BooleanFiniteUnionNegative
    BooleanFiniteUnionNegative -> BooleanFiniteUnionPositive

  collapseRecursivePiecewiseAffineCoverage = collapse False []
   where
    collapse contradiction retained remaining = case remaining of
      []
        | contradiction -> RelationalPositiveAffineClauseContradiction
        | otherwise -> RelationalPositiveAffineClauseRules
            $ concat $ reverse retained
      RelationalPositiveAffineClauseIgnored : _ ->
        RelationalPositiveAffineClauseIgnored
      RelationalPositiveAffineClauseContradiction : following ->
        collapse True retained following
      RelationalPositiveAffineClauseRules rules : following ->
        collapse contradiction (rules : retained) following

  hasRecursivePiecewiseAffineOperation source = case source of
    LengthAtMost left right -> inExpression left || inExpression right
    LengthNot (LengthAtMost left right) ->
      inExpression left || inExpression right
    LengthEqual left right -> inExpression left || inExpression right
    _ -> False

  inExpression expression = case expression of
    LengthVariable _ -> False
    LengthLiteral _ -> False
    LengthSum terms -> any inExpression terms
    LengthScale _ nested -> inExpression nested
    LengthQuotient _ nested -> inExpression nested
    LengthModulo _ nested -> inExpression nested
    LengthMonus _ _ -> True
    LengthMinimum _ _ -> True
    LengthMaximum _ _ -> True
    LengthIf _ _ _ -> True

addRecursivePiecewiseAffineSummaries
  :: RecursivePiecewiseAffineSummary
  -> RecursivePiecewiseAffineSummary
  -> RecursivePiecewiseAffineSummary
addRecursivePiecewiseAffineSummaries
    (RecursivePiecewiseAffineSummary leftConstant leftCoefficients)
    (RecursivePiecewiseAffineSummary rightConstant rightCoefficients) =
  RecursivePiecewiseAffineSummary
    (leftConstant + rightConstant)
    (Map.unionWith (+) leftCoefficients rightCoefficients)

subtractRecursivePiecewiseAffineSummaries
  :: RecursivePiecewiseAffineSummary
  -> RecursivePiecewiseAffineSummary
  -> RecursivePiecewiseAffineSummary
subtractRecursivePiecewiseAffineSummaries
    (RecursivePiecewiseAffineSummary leftConstant leftCoefficients)
    (RecursivePiecewiseAffineSummary rightConstant rightCoefficients) =
  RecursivePiecewiseAffineSummary
    (leftConstant - rightConstant)
    (Map.unionWith (+) leftCoefficients $ Map.map negate rightCoefficients)

scaleRecursivePiecewiseAffineSummary
  :: Natural
  -> RecursivePiecewiseAffineSummary
  -> RecursivePiecewiseAffineSummary
scaleRecursivePiecewiseAffineSummary factor
    (RecursivePiecewiseAffineSummary constant coefficients) =
  let integerFactor = toInteger factor
  in RecursivePiecewiseAffineSummary
      (integerFactor * constant)
      (Map.map (integerFactor *) coefficients)

incrementRecursivePiecewiseAffineSummary
  :: RecursivePiecewiseAffineSummary
  -> RecursivePiecewiseAffineSummary
incrementRecursivePiecewiseAffineSummary
    (RecursivePiecewiseAffineSummary constant coefficients) =
  RecursivePiecewiseAffineSummary (constant + 1) coefficients

recursivePiecewiseAffineRule
  :: RecursivePiecewiseAffineSummary
  -> RecursivePiecewiseAffineSummary
  -> RelationalPositiveAffineRule
recursivePiecewiseAffineRule
    (RecursivePiecewiseAffineSummary leftConstant leftCoefficients)
    (RecursivePiecewiseAffineSummary rightConstant rightCoefficients) =
  relationalPositiveAffineRule
    (RelationalPositiveAffineSummary
      (positiveInteger leftConstant + negativeInteger rightConstant)
      (Map.unionWith (+)
        (positiveIntegerCoefficients leftCoefficients)
        (negativeIntegerCoefficients rightCoefficients)))
    (RelationalPositiveAffineSummary
      (positiveInteger rightConstant + negativeInteger leftConstant)
      (Map.unionWith (+)
        (positiveIntegerCoefficients rightCoefficients)
        (negativeIntegerCoefficients leftCoefficients)))
 where
  positiveInteger value
    | value > 0 = fromInteger value
    | otherwise = 0

  negativeInteger value
    | value < 0 = fromInteger $ negate value
    | otherwise = 0

  positiveIntegerCoefficients = Map.mapMaybe retainPositive
  retainPositive coefficient
    | coefficient > 0 = Just $ fromInteger coefficient
    | otherwise = Nothing

  negativeIntegerCoefficients = Map.mapMaybe retainNegative
  retainNegative coefficient
    | coefficient < 0 = Just $ fromInteger $ negate coefficient
    | otherwise = Nothing

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
  inputs <- exactAssignment
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

-- Count the assignments of one inclusive-maximum box, productively: the
-- running product stops at the caller's limit plus one and becomes the
-- caller's limit error, so a huge box is refused before it is enumerated.
inputBoxAssignmentCountWith
  :: (Natural -> Natural -> failure)
  -> LengthInputBoxLimits
  -> [Natural]
  -> Either failure Natural
inputBoxAssignmentCountWith limitExceeded limits = go 1
 where
  maximumAssignments = lengthInputBoxAssignmentLimit limits
  exceeded = limitExceeded maximumAssignments (maximumAssignments + 1)

  go !total []
    | total <= maximumAssignments = Right total
    | otherwise = Left exceeded
  go !total (maximumValue : remaining)
    | total > maximumAssignments = Left exceeded
    | factor > 0 && total > maximumAssignments `quot` factor = Left exceeded
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
nextInputBoxAssignmentWith
  :: failure
  -> [Natural]
  -> [Natural]
  -> Either failure (Maybe [Natural])
nextInputBoxAssignmentWith invariant maximums values =
  fmap reverse <$> advance (reverse maximums) (reverse values)
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
  advance _ _ = Left invariant

-- Accept a list of exactly the expected length, observing at most one
-- element past it; any mismatch becomes the caller's arity error.  Both
-- domains, the input-box bound vectors, and the provider assignments share
-- this one check.
exactAssignment
  :: (Int -> Int -> failure)
  -> Int
  -> [value]
  -> Either failure [value]
exactAssignment mismatch expected values =
  let observed = observedListLength expected values
  in if observed == expected
      then Right values
      else Left $ mismatch expected observed

checkSpinePairAssignedValue
  :: LengthEvaluationLimits
  -> LengthSpinePairEvaluationValueSite
  -> Natural
  -> Either LengthSpinePairEvaluationError ()
checkSpinePairAssignedValue limits site =
  checkValueWithin LengthSpinePairEvaluationValueBitLimitExceeded site
    (lengthAssignmentValueBitLimit limits)

-- The binary-product evaluation-error vocabulary, for the shared
-- expression and formula evaluators.
spinePairEvaluationErrors :: EvaluationErrors LengthSpinePairEvaluationError
spinePairEvaluationErrors = EvaluationErrors
  { evaluationQuotientDivisorZero =
      LengthSpinePairEvaluationInternalQuotientDivisorZero
  , evaluationModuloDivisorZero =
      LengthSpinePairEvaluationInternalModuloDivisorZero
  , evaluationIntermediateBitLimitExceeded =
      LengthSpinePairEvaluationValueBitLimitExceeded
        LengthSpinePairIntermediateValue
  }

checkAssignedValue
  :: LengthEvaluationLimits
  -> LengthEvaluationValueSite
  -> Natural
  -> Either LengthEvaluationError ()
checkAssignedValue limits site =
  checkValueWithin LengthEvaluationValueBitLimitExceeded site
    (lengthAssignmentValueBitLimit limits)

-- Refuse a value whose magnitude exceeds the bit bound, reporting the
-- caller's site through the caller's error constructor.
checkValueWithin
  :: (site -> Int -> Int -> failure)
  -> site
  -> Int
  -> Natural
  -> Either failure ()
checkValueWithin bitLimitExceeded site maximumBits value =
  let observedBits = observedNaturalBits maximumBits value
  in unless (observedBits <= maximumBits) $ Left
      $ bitLimitExceeded site maximumBits observedBits

-- The scalar evaluation-error vocabulary, for the shared expression and
-- formula evaluators.
scalarEvaluationErrors :: EvaluationErrors LengthEvaluationError
scalarEvaluationErrors = EvaluationErrors
  { evaluationQuotientDivisorZero = LengthEvaluationInternalQuotientDivisorZero
  , evaluationModuloDivisorZero = LengthEvaluationInternalModuloDivisorZero
  , evaluationIntermediateBitLimitExceeded =
      LengthEvaluationValueBitLimitExceeded LengthIntermediateValue
  }

-- What the shared expression and formula evaluators need to spell in a
-- domain's own error type: the two internal divisor-zero failures and the
-- intermediate-value bit-limit failure.  Both domains evaluate the same
-- 'LengthExpression' and 'LengthFormula' syntax under the same limits.
data EvaluationErrors failure = EvaluationErrors
  { evaluationQuotientDivisorZero :: failure
  , evaluationModuloDivisorZero :: failure
  , evaluationIntermediateBitLimitExceeded :: Int -> Int -> failure
    -- ^ maximum bits, observed bits
  }

checkIntermediate
  :: EvaluationErrors failure
  -> LengthEvaluationLimits
  -> Natural
  -> Either failure Natural
checkIntermediate errors limits value =
  let maximumBits = lengthIntermediateValueBitLimit limits
      observedBits = observedNaturalBits maximumBits value
  in if observedBits <= maximumBits
      then Right value
      else Left $ evaluationIntermediateBitLimitExceeded errors
        maximumBits observedBits

evaluateExpression
  :: LengthEvaluationLimits
  -> (variable -> Either LengthEvaluationError Natural)
  -> LengthExpression variable
  -> Either LengthEvaluationError Natural
evaluateExpression = evaluateExpressionWith scalarEvaluationErrors

evaluateFormula
  :: LengthEvaluationLimits
  -> (variable -> Either LengthEvaluationError Natural)
  -> LengthFormula variable
  -> Either LengthEvaluationError Bool
evaluateFormula = evaluateFormulaWith scalarEvaluationErrors

evaluateSpinePairExpression
  :: LengthEvaluationLimits
  -> (variable -> Either LengthSpinePairEvaluationError Natural)
  -> LengthExpression variable
  -> Either LengthSpinePairEvaluationError Natural
evaluateSpinePairExpression = evaluateExpressionWith spinePairEvaluationErrors

evaluateSpinePairFormula
  :: LengthEvaluationLimits
  -> (variable -> Either LengthSpinePairEvaluationError Natural)
  -> LengthFormula variable
  -> Either LengthSpinePairEvaluationError Bool
evaluateSpinePairFormula = evaluateFormulaWith spinePairEvaluationErrors

evaluateExpressionWith
  :: EvaluationErrors failure
  -> LengthEvaluationLimits
  -> (variable -> Either failure Natural)
  -> LengthExpression variable
  -> Either failure Natural
evaluateExpressionWith errors limits lookupVariable source = case source of
  LengthVariable variable -> lookupVariable variable
  LengthLiteral value -> intermediate value
  LengthSum terms -> foldM add 0 terms
  LengthScale factor expression -> do
    value <- evaluate expression
    intermediate $ factor * value
  LengthQuotient divisor expression
    | divisor == 0 -> Left $ evaluationQuotientDivisorZero errors
    | otherwise -> do
        value <- evaluate expression
        intermediate $ value `quot` divisor
  LengthModulo divisor expression
    | divisor == 0 -> Left $ evaluationModuloDivisorZero errors
    | otherwise -> do
        value <- evaluate expression
        intermediate $ value `mod` divisor
  LengthMonus left right -> do
    leftValue <- evaluate left
    rightValue <- evaluate right
    intermediate $ leftValue `monus` rightValue
  LengthMinimum left right -> binary min left right
  LengthMaximum left right -> binary max left right
  LengthIf condition whenTrue whenFalse -> do
    selected <- evaluateFormulaWith errors limits lookupVariable condition
    evaluate $ if selected then whenTrue else whenFalse
 where
  evaluate = evaluateExpressionWith errors limits lookupVariable
  intermediate = checkIntermediate errors limits

  add total term = do
    value <- evaluate term
    intermediate $ total + value

  binary operation left right = do
    leftValue <- evaluate left
    rightValue <- evaluate right
    intermediate $ operation leftValue rightValue

evaluateFormulaWith
  :: EvaluationErrors failure
  -> LengthEvaluationLimits
  -> (variable -> Either failure Natural)
  -> LengthFormula variable
  -> Either failure Bool
evaluateFormulaWith errors limits lookupVariable source = case source of
  LengthTruth value -> Right value
  LengthEqual left right -> compareWith (==) left right
  LengthAtMost left right -> compareWith (<=) left right
  LengthNot formula -> not <$> evaluateFormulaWith errors limits lookupVariable
    formula
  LengthAll formulas -> allM formulas
 where
  compareWith relation left right = do
    leftValue <- evaluateExpressionWith errors limits lookupVariable left
    rightValue <- evaluateExpressionWith errors limits lookupVariable right
    pure $ relation leftValue rightValue

  allM [] = Right True
  allM (formula : remaining) = do
    value <- evaluateFormulaWith errors limits lookupVariable formula
    if value then allM remaining else Right False

monus :: Natural -> Natural -> Natural
monus left right
  | left >= right = left - right
  | otherwise = 0

indexNatural :: Natural -> [value] -> Maybe value
indexNatural 0 (value : _) = Just value
indexNatural position (_ : remaining) = indexNatural (position - 1) remaining
indexNatural _ [] = Nothing
