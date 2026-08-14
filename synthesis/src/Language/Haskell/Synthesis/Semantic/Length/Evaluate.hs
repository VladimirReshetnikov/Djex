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
  , LengthInputBoxLimitSource (..)
  , LengthInputBoxLimits
  , LengthInputBoxLimitField (..)
  , LengthInputBoxLimitError (..)
  , mkLengthInputBoxLimits
  , defaultLengthInputBoxLimitSource
  , defaultLengthInputBoxLimits
  , lengthInputBoxInputLimit
  , lengthInputBoxAssignmentLimit
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
  , evaluateLengthContractAssignment
  , evaluateLengthSpinePairContractAssignment
  , evaluateLengthProviderApplication
  , validateLengthProblemCounterexample
  , validateLengthSpinePairProblemCounterexample
  , validateLengthProblemInputBox
  , validateLengthSpinePairProblemInputBox
  ) where

import Control.DeepSeq (NFData (rnf))
import Control.Monad (foldM, unless)
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
  , mkBehavioralEvidence
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

observedNaturalBits :: Int -> Natural -> Int
observedNaturalBits maximumBits = go 0
 where
  bound = max 0 maximumBits

  go !observed 0 = observed
  go !observed remaining
    | observed >= bound = saturatedSuccessor bound
    | otherwise = go (observed + 1) $ remaining `quot` 2

saturatedSuccessor :: Int -> Int
saturatedSuccessor value
  | value == maxBound = maxBound
  | otherwise = value + 1
