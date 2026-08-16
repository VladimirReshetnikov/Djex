{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Private atomic interpretation of one retained typed candidate.
--
-- This module is the only edge which combines a Length session, a checked
-- contract, an engine-owned typed-candidate association, the symbolic result,
-- and all generic behavioral-problem identities.  No raw graph or detached
-- compatibility candidate can enter the public API.
module Language.Haskell.Synthesis.Internal.Semantic.Length.Problem.Candidate
  ( LengthProblemLimits
  , LengthProblemLimitError (..)
  , mkLengthProblemLimits
  , defaultLengthProblemLimits
  , lengthProblemTermGraphLimits
  , lengthProblemGraphFingerprintByteLimit
  , lengthProblemEvaluationStepLimit
  , LengthProblemFingerprintPart (..)
  , LengthSpinePairProblemFingerprintPart (..)
  , LengthRootOpeningError (..)
  , LengthUnobservedTargetDemandSite (..)
  , LengthStepPayloadDemandSite (..)
  , LengthAssociatedConstraintDischargeReason (..)
  , LengthAssociatedProviderChainSite (..)
  , LengthAssociatedProviderChainReason (..)
  , LengthProblemError (..)
  , LengthSpinePairProblemError (..)
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
  , checkedLengthProblemCounterexampleBankScope
  , CheckedLengthSpinePairCandidate
  , CheckedLengthSpinePairProblem
  , sealLengthSpinePairTypedCandidateProblem
  , sealRoleAwareLengthSpinePairTypedCandidateProblem
  , sealExactSpineCaseLengthSpinePairTypedCandidateProblem
  , sealLengthSpinePairTypedCandidateProblemInSession
  , checkedLengthSpinePairCandidateResult
  , checkedLengthSpinePairCandidateUsedProviders
  , checkedLengthSpinePairCandidateFingerprint
  , checkedLengthSpinePairProblemCandidate
  , checkedLengthSpinePairProblemInputCount
  , checkedLengthSpinePairProblemPrecondition
  , checkedLengthSpinePairProblemPostcondition
  , checkedLengthSpinePairProblemCounterexampleCondition
  , checkedLengthSpinePairProblemEncodingFingerprint
  , checkedLengthSpinePairProblemBehavioralProblem
  , checkedLengthSpinePairProblemCounterexampleBankScope
  ) where

import Control.DeepSeq (NFData (rnf))
import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
  ( StateT
  , get
  , put
  , runStateT
  )
import Data.Bifunctor (first)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Candidate
  ( Candidate
  , candidateResidualConstraints
  )
import Language.Haskell.Synthesis.Constraint (Constraint)
import Language.Haskell.Synthesis.Fingerprint
  ( Fingerprint
  , fingerprintCanonicalBytes
  )
import Language.Haskell.Synthesis.Internal.Alpha
  ( BinderSlotPolicy (PositionalBinderSlots)
  , alphaNormalizeTypeWith
  )
import Language.Haskell.Synthesis.Internal.ClassResolution
  ( CheckedConstraintDischarge
  , ClassResolutionQueryError (..)
  , HeterogeneousClassResolutionQueryError (..)
  , dischargeHeterogeneousGroundConstraint
  )
import Language.Haskell.Synthesis.Internal.Fingerprint
  ( FingerprintBuilder (..)
  , FingerprintField (..)
  , FingerprintLimitError (..)
  , buildFingerprintWithin
  )
import Language.Haskell.Synthesis.Internal.Semantic.Length
  ( CheckedLengthContract
  , CheckedLengthProviderInventory
  , CheckedLengthProviderSummary
  , CheckedLengthSpineModel
  , CheckedLengthSpinePairContract
  , FiniteBinaryProductSpineLengthsV1
  , FiniteListSpineLengthV1
  , LengthContractError
  , LengthContractSource (..)
  , LengthContractVariable (..)
  , LengthExpression (..)
  , LengthFormula (..)
  , LengthSpinePair (..)
  , LengthSpinePairComponent (..)
  , LengthSpinePairContractError
  , LengthSpinePairContractSource (..)
  , LengthSpinePairContractVariable (..)
  , LengthProviderArgumentRole (..)
  , LengthProviderTrust (..)
  , LengthTargetArgumentRole (..)
  , LengthProviderVariable (..)
  , LengthSpineModelTrust (..)
  , LengthSyntaxError (..)
  , ascii
  , checkedLengthContractPostcondition
  , checkedLengthContractPrecondition
  , checkedLengthContractTarget
  , checkedLengthContractTargetArgumentRoles
  , checkedLengthContractInputCount
  , checkedLengthSpinePairContractPostcondition
  , checkedLengthSpinePairContractPrecondition
  , checkedLengthSpinePairContractTarget
  , checkedLengthSpinePairContractTargetArgumentRoles
  , checkedLengthSpinePairContractInputCount
  , checkedLengthProviderArgumentRoles
  , checkedLengthProviderName
  , checkedLengthProviderScheme
  , checkedLengthProviderSummaries
  , checkedLengthProviderTrust
  , checkedLengthProviderTransfer
  , checkedLengthSpineModelTrust
  , checkedLengthSpineRecursiveField
  , checkedLengthSpineStepConstructor
  , checkedLengthSpineTypeName
  , checkedLengthSpineZeroConstructor
  , contractVariableField
  , emptySyntaxUsage
  , finiteListSpineLengthDomainTag
  , finiteBinaryProductSpineLengthsDomainTag
  , inventoryTermScheme
  , isModeledSpine
  , lengthContractFingerprint
  , lengthSpinePairContractFingerprint
  , lengthContextInventory
  , lengthContextSpineModel
  , lengthExpressionField
  , lengthFingerprintByteLimit
  , lengthFormulaField
  , lookupCheckedLengthProviderSummary
  , normalizeLengthExpression
  , normalizeLengthFormula
  , providerSummaryField
  , sealRoleAwareLengthContractInContext
  , sealRoleAwareLengthSpinePairContractInContext
  , tagged
  )
import Language.Haskell.Synthesis.Internal.Semantic.Length.Problem
  ( CheckedLengthSession
  , LengthCasePolicy (..)
  , LengthTargetArgumentPolicy (..)
  , checkedLengthSessionCasePolicy
  , checkedLengthSessionClassResolutionEnvironment
  , checkedLengthSessionContext
  , checkedLengthSessionLimits
  , checkedLengthSessionProviderInventory
  , checkedLengthSessionTargetArgumentPolicy
  , checkedLengthSessionExplicitTargetRoles
  , buildFiniteBinaryProductSpineLengthsInventoryFingerprint
  , lengthSessionEncodingPolicyFingerprint
  , lengthSessionInventoryFingerprint
  , sealLengthContractInSession
  , sealLengthSpinePairContractInSession
  )
import Language.Haskell.Synthesis.Internal.Semantic.Length.CounterexampleBank
  ( LengthCounterexampleBankScope
  , LengthSpinePairCounterexampleBankScope
  , sealLengthCounterexampleBankScope
  , sealLengthSpinePairCounterexampleBankScopeWithInventory
  )
import Language.Haskell.Synthesis.Internal.Semantic.Problem
  ( BehavioralProblem
  , CandidateFingerprintSubject
  , EncodingFingerprintSubject
  , InventoryFingerprintSubject
  , ProblemFingerprintSubject
  , behavioralProblemEncodingFingerprint
  , mkBehavioralProblem
  )
import Language.Haskell.Synthesis.Internal.TypedCandidate
  ( foldTypedCandidateGraph )
import Language.Haskell.Synthesis.Internal.TypedGenerated.Certificate
  ( CheckedTypeApplicationCertificateStep
  , checkedTypeApplicationCertificateStepObligationCount
  , checkedTypeApplicationCertificateStepObligations
  )
import Language.Haskell.Synthesis.Internal.TypedGenerated.Certificate.Association
  ( CheckedTypeApplicationCertificateGraph
  , checkedTypeApplicationCertificateGraph
  , foldCheckedTypeApplicationCertificateGraph
  )
import Language.Haskell.Synthesis.Inventory
  ( Inventory
  , inventoryKindAssumptions
  )
import Language.Haskell.Synthesis.Kind (Kind (ProperTypeKind))
import Language.Haskell.Synthesis.KindInference
  ( KindInferenceError (..)
  , checkTypesKinds
  , inferSharedVariableKinds
  )
import Language.Haskell.Synthesis.Name (Name)
import Language.Haskell.Synthesis.Type
  ( Type (..)
  , Variable (..)
  , freeVariables
  , freeVariablesInFirstOccurrenceOrder
  , functionSpine
  , normalizeType
  , splitLeadingForalls
  )
import Language.Haskell.Synthesis.TypeInstantiation
  ( ContextFreeSchemeSelection
  , contextFreeSchemeSelectionFreeVariables
  , contextFreeSchemeSelectionVariable
  , contextFreeSchemeSelections
  , matchContextFreeScheme
  )
import Language.Haskell.Synthesis.TypedCandidate
  ( TypedCandidate
  , typedCandidateCompatibility
  )
import Language.Haskell.Synthesis.TypedGenerated
  ( ApplicationWitness (..)
  , OccurrenceId
  , TermGraph
  , TermGraphLimits
  , TermNode (..)
  , TermNodeForm (..)
  , TermNodeId
  , TypeApplicationWitness (..)
  , TypeStructure (..)
  , TypedPattern (..)
  , TypedPatternNode (..)
  , defaultTermGraphLimits
  , lookupTermNode
  , sharedTypeStructure
  , termGraphNodes
  , termGraphRoot
  )
import Language.Haskell.Synthesis.TypedGenerated.Fingerprint
  ( TermGraphFingerprintError
  , TermGraphFingerprintSubject
  , defaultTermGraphFingerprintByteLimit
  , fingerprintSharedTermGraph
  )
import Language.Haskell.Synthesis.Internal.TypedGenerated.Fingerprint
  ( fingerprintCheckedTypeApplicationCertificateGraphWithTypeStructure
  , fingerprintTermGraphWithTypeStructure
  )
import Language.Haskell.Synthesis.Count
  ( saturatedSuccessor
  )

-- | Candidate-specific work bounds.  Graph limits are already checked by
-- their own constructor; only the signed evaluation-step field can fail here.
data LengthProblemLimits = LengthProblemLimits
  !TermGraphLimits !Natural !Int
  deriving (Eq, Ord, Show)

instance NFData LengthProblemLimits where
  rnf (LengthProblemLimits graphLimits fingerprintBytes evaluationSteps') =
    rnf graphLimits `seq` rnf fingerprintBytes `seq` rnf evaluationSteps'

-- | Invalid signed work limit supplied to 'mkLengthProblemLimits'.
data LengthProblemLimitError
  = NegativeLengthProblemEvaluationStepLimit !Int
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthProblemLimitError

-- | Check graph, graph-fingerprint-byte, and symbolic-evaluation bounds.
mkLengthProblemLimits
  :: TermGraphLimits
  -> Natural
  -> Int
  -> Either LengthProblemLimitError LengthProblemLimits
mkLengthProblemLimits graphLimits fingerprintBytes maximumEvaluationSteps
  | maximumEvaluationSteps < 0 = Left
      $ NegativeLengthProblemEvaluationStepLimit maximumEvaluationSteps
  | otherwise = Right
      $ LengthProblemLimits graphLimits fingerprintBytes maximumEvaluationSteps

-- | Conservative limits used by ordinary typed-candidate sealing.
defaultLengthProblemLimits :: LengthProblemLimits
defaultLengthProblemLimits = LengthProblemLimits
  defaultTermGraphLimits defaultTermGraphFingerprintByteLimit 65536

-- | Shared graph reconstruction and structural-validation limits.
lengthProblemTermGraphLimits :: LengthProblemLimits -> TermGraphLimits
lengthProblemTermGraphLimits (LengthProblemLimits limits _ _) = limits

-- | Maximum canonical bytes retained for the shared graph fingerprint.
lengthProblemGraphFingerprintByteLimit :: LengthProblemLimits -> Natural
lengthProblemGraphFingerprintByteLimit
    (LengthProblemLimits _ maximumBytes _) = maximumBytes

-- | Maximum lazy symbolic-interpreter steps.
lengthProblemEvaluationStepLimit :: LengthProblemLimits -> Int
lengthProblemEvaluationStepLimit
    (LengthProblemLimits _ _ maximumSteps) = maximumSteps

-- | Candidate-sealing identity whose canonical byte bound was exceeded.
data LengthProblemFingerprintPart
  = LengthConcreteEncodingFingerprint
    -- ^ Contract, interpreter, used laws, result, and bad-state formula.
  | LengthCandidateFingerprint
    -- ^ Domain wrapper around the shared typed graph identity.
  | LengthCompleteProblemFingerprint
    -- ^ Inventory, concrete encoding, and candidate association.
  | LengthCounterexampleBankScopeFingerprint
    -- ^ Candidate-independent session, contract, and normalized target.
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthProblemFingerprintPart

-- | Product-domain identity whose bounded canonical construction failed.
data LengthSpinePairProblemFingerprintPart
  = LengthSpinePairInventoryFingerprint
  | LengthSpinePairConcreteEncodingFingerprint
  | LengthSpinePairCandidateFingerprint
  | LengthSpinePairCompleteProblemFingerprint
  | LengthSpinePairCounterexampleBankScopeFingerprint
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSpinePairProblemFingerprintPart

-- | Why a contract's quantified root cannot authorize the graph root.
data LengthRootOpeningError identity
  = LengthRootOpeningShapeMismatch
    -- ^ The graph root is not an instance of the contract target.
  | LengthRootOpeningSelectionIsNotRigid !Natural
    -- ^ A source binder selected a closed type or flexible variable.
  | LengthRootOpeningRigidIsNotInjective !Natural identity
    -- ^ A selected rigid was already present or selected by another binder.
  deriving (Eq, Ord, Show, Generic)

instance NFData identity => NFData (LengthRootOpeningError identity)

-- | The first semantic operation which tried to inspect one opaque target
-- argument.  Carrying, ignoring, or forwarding the token does not constitute
-- a demand.
data LengthUnobservedTargetDemandSite
  = LengthUnobservedTargetCallableDemand !TermNodeId
  | LengthUnobservedTargetSpineDemand !TermNodeId
  | LengthUnobservedTargetTupleDemand !OccurrenceId
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthUnobservedTargetDemandSite

-- | The first operation which tried to inspect an opaque element carried by
-- one modeled step pattern.  The occurrence identifies the exact checked
-- field pattern which introduced the token.  Binding, ignoring, reconstructing
-- a step, or forwarding it through an unobserved provider argument is not a
-- demand.
data LengthStepPayloadDemandSite
  = LengthStepPayloadCallableDemand !TermNodeId
  | LengthStepPayloadSpineDemand !TermNodeId
  | LengthStepPayloadTupleDemand !OccurrenceId
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthStepPayloadDemandSite

-- | Sanitized reason why one canonical certificate obligation could not
-- establish inventory-bound class evidence.  Raw resolver diagnostics and
-- constraint/type payloads never cross the public problem boundary.
data LengthAssociatedConstraintDischargeReason
  = LengthAssociatedClassResolverUnavailable
  | LengthAssociatedConstraintNotGround
  | LengthAssociatedConstraintQueryRejected
  | LengthAssociatedConstraintEvidenceMissing
  | LengthAssociatedDerivedConstraintRejected
  | LengthAssociatedConstraintProofLimitExceeded
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthAssociatedConstraintDischargeReason

-- | Canonical position protected inside one discharged provider chain.
data LengthAssociatedProviderChainSite
  = LengthAssociatedProviderBase
  | LengthAssociatedProviderIntermediate !Natural
    -- ^ Source-step ordinal whose result is the protected intermediate.
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthAssociatedProviderChainSite

-- | Sanitized structural reason why a protected certified chain was not an
-- occurrence-private path to its authorized final provider node.
data LengthAssociatedProviderChainReason
  = LengthAssociatedProtectedNodeIsRoot
  | LengthAssociatedProtectedNodeHasUnexpectedIncomingEdge
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthAssociatedProviderChainReason

-- | Fixed-precedence failure while sealing one complete typed candidate.
data LengthProblemError failure identity local
  = LengthProblemContractResealRejected
      (LengthContractError (Variable identity))
  | LengthProblemContractContextMismatch
  | LengthProblemMixedTargetArgumentsRequireRoleAwareSealer
  | LengthProblemCasePolicyMismatch
  | LengthProblemTargetArgumentPolicyMismatch
  | LengthProblemResidualConstraint
      (Constraint (Type (Variable identity)))
  | LengthProblemTypedGraphUnavailable failure
  -- | The named row owner is absent from the exact source inventory.  The
  -- 'Natural' is its canonical rooted-row ordinal; no certificate, node,
  -- occurrence, or raw source-slot coordinate is exposed.
  | LengthProblemAssociatedCertificateOwnerMissing !Name !Natural
  -- | The carrier's complete owner scheme is not alpha-exactly the source
  -- inventory scheme.  The 'Natural' is the canonical rooted-row ordinal;
  -- no certificate, node, occurrence, or raw source-slot coordinate is
  -- exposed.
  | LengthProblemAssociatedCertificateSourceSchemeMismatch !Name !Natural
  -- | The first source-order step with activated obligations.  The two
  -- 'Natural' values are the canonical rooted-row and source-step ordinals;
  -- the bounded 'Int' is the obligation count.  Raw certificate, slot, node,
  -- occurrence, constraint, and type payloads remain private.
  | LengthProblemAssociatedCertificateActivatedObligations
      !Name !Natural !Natural !Int
  -- | Certificate authority for a modeled zero or step constructor is outside
  -- this provider-only checkpoint.  The 'Natural' is the canonical rooted-row
  -- ordinal, never a raw certificate or graph coordinate.
  | LengthProblemAssociatedCertificateModeledConstructorUnsupported
      !Name !Natural
  -- | The exact source owner has no checked Length provider summary.  The
  -- 'Natural' is the canonical rooted-row ordinal; no raw certificate or graph
  -- coordinate is exposed.
  | LengthProblemAssociatedCertificateProviderSummaryMissing !Name !Natural
  -- | A conditional provider row unexpectedly activated no constraint.  The
  -- owner and canonical rooted-row ordinal identify the policy site without
  -- exposing a graph or certificate coordinate.
  | LengthProblemAssociatedCertificateConditionalObligationsMissing
      !Name !Natural
  -- | Independent class discharge failed for one source-ordered obligation.
  -- Coordinates are canonical rooted-row, source-step, and obligation
  -- ordinals; the reason deliberately sanitizes the resolver's raw payload.
  | LengthProblemAssociatedCertificateConstraintDischargeRejected
      !Name !Natural !Natural !Natural
      !LengthAssociatedConstraintDischargeReason
  -- | A base or intermediate node in a discharged row was reachable outside
  -- its exact certified function edge, or was itself the graph root.  No raw
  -- node, occurrence, certificate, or slot identity is exposed.
  | LengthProblemAssociatedCertificateProtectedChainRejected
      !Name !Natural !LengthAssociatedProviderChainSite
      !LengthAssociatedProviderChainReason
  -- | This exact direct, base, or partial conditional-provider occurrence is
  -- not the associated row's authorized final visible-application node.  No
  -- constraint, resolver receipt, or dictionary payload is exposed.
  | LengthProblemConditionalProviderRequiresDischarge !TermNodeId !Name
  | LengthProblemTermGraphFingerprintRejected
      (TermGraphFingerprintError identity local)
  | LengthProblemRootNodeMissing !TermNodeId
  | LengthProblemRootOpeningRejected
      (LengthRootOpeningError identity)
  | LengthProblemHole !TermNodeId local
  | LengthProblemUnsupportedCase !TermNodeId
  | LengthProblemUnsupportedConstructorPattern !OccurrenceId Name
  | LengthProblemCaseResultIsNotModeledSpine
      !TermNodeId (Type (Variable identity))
  | LengthProblemCaseScrutineeIsNotModeledSpine
      !TermNodeId (Type (Variable identity))
  | LengthProblemCaseAlternativeCountMismatch !TermNodeId !Int
  | LengthProblemCasePatternIsNotDirectConstructor !OccurrenceId
  | LengthProblemCaseConstructorRepeated !TermNodeId Name
  | LengthProblemCaseFieldPatternUnsupported !OccurrenceId
  | LengthProblemGraphKindRejected
      (KindInferenceError (Variable identity))
  | LengthProblemVisibleTypeSourceHasNoBinder !TermNodeId
  | LengthProblemVisibleTypeSelectionRejected
      !TermNodeId (Type (Variable identity))
  | LengthProblemGlobalNotInSourceInventory !TermNodeId Name
  | LengthProblemGlobalHasNoLengthSummary !TermNodeId Name
  | LengthProblemGlobalInstantiationRejected !TermNodeId Name
  | LengthProblemSemanticLocalMissing !TermNodeId local
  | LengthProblemExpectedCallable !TermNodeId
  | LengthProblemExpectedSpine !TermNodeId
  | LengthProblemExpectedTuple !OccurrenceId
  | LengthProblemUnobservedTargetArgumentDemanded
      !Natural !LengthUnobservedTargetDemandSite
  | LengthProblemStepPayloadDemanded
      !OccurrenceId !LengthStepPayloadDemandSite
  | LengthProblemTupleArityMismatch !OccurrenceId !Int !Int
  | LengthProblemEvaluationStepLimitExceeded !Int !Int
  | LengthProblemProviderTransferInvariant Name !Natural
  | LengthProblemSyntaxRejected LengthSyntaxError
  | LengthProblemFingerprintLimitExceeded
      !LengthProblemFingerprintPart !Natural !Natural
  deriving (Eq, Ord, Show, Generic)

instance
    (NFData failure, NFData identity, NFData local) =>
    NFData (LengthProblemError failure identity local)

-- | Closed, product-domain counterpart of 'LengthProblemError'.  Shared
-- interpreter failures are translated into these constructors before they
-- cross the public boundary; scalar contract errors never escape through it.
data LengthSpinePairProblemError failure identity local
  = LengthSpinePairProblemContractResealRejected
      (LengthSpinePairContractError (Variable identity))
  | LengthSpinePairProblemContractContextMismatch
  | LengthSpinePairProblemMixedTargetArgumentsRequireRoleAwareSealer
  | LengthSpinePairProblemCasePolicyMismatch
  | LengthSpinePairProblemTargetArgumentPolicyMismatch
  | LengthSpinePairProblemResidualConstraint
      (Constraint (Type (Variable identity)))
  | LengthSpinePairProblemTypedGraphUnavailable failure
  | LengthSpinePairProblemAssociatedCertificateOwnerMissing !Name !Natural
  | LengthSpinePairProblemAssociatedCertificateSourceSchemeMismatch
      !Name !Natural
  | LengthSpinePairProblemAssociatedCertificateActivatedObligations
      !Name !Natural !Natural !Int
  | LengthSpinePairProblemAssociatedCertificateModeledConstructorUnsupported
      !Name !Natural
  | LengthSpinePairProblemAssociatedCertificateProviderSummaryMissing
      !Name !Natural
  | LengthSpinePairProblemAssociatedCertificateConditionalObligationsMissing
      !Name !Natural
  | LengthSpinePairProblemAssociatedCertificateConstraintDischargeRejected
      !Name !Natural !Natural !Natural
      !LengthAssociatedConstraintDischargeReason
  | LengthSpinePairProblemAssociatedCertificateProtectedChainRejected
      !Name !Natural !LengthAssociatedProviderChainSite
      !LengthAssociatedProviderChainReason
  | LengthSpinePairProblemConditionalProviderRequiresDischarge !TermNodeId !Name
  | LengthSpinePairProblemTermGraphFingerprintRejected
      (TermGraphFingerprintError identity local)
  | LengthSpinePairProblemRootNodeMissing !TermNodeId
  | LengthSpinePairProblemRootOpeningRejected
      (LengthRootOpeningError identity)
  | LengthSpinePairProblemHole !TermNodeId local
  | LengthSpinePairProblemUnsupportedCase !TermNodeId
  | LengthSpinePairProblemUnsupportedConstructorPattern !OccurrenceId Name
  | LengthSpinePairProblemCaseResultIsNotModeledSpine
      !TermNodeId (Type (Variable identity))
  | LengthSpinePairProblemCaseScrutineeIsNotModeledSpine
      !TermNodeId (Type (Variable identity))
  | LengthSpinePairProblemCaseAlternativeCountMismatch !TermNodeId !Int
  | LengthSpinePairProblemCasePatternIsNotDirectConstructor !OccurrenceId
  | LengthSpinePairProblemCaseConstructorRepeated !TermNodeId Name
  | LengthSpinePairProblemCaseFieldPatternUnsupported !OccurrenceId
  | LengthSpinePairProblemGraphKindRejected
      (KindInferenceError (Variable identity))
  | LengthSpinePairProblemVisibleTypeSourceHasNoBinder !TermNodeId
  | LengthSpinePairProblemVisibleTypeSelectionRejected
      !TermNodeId (Type (Variable identity))
  | LengthSpinePairProblemGlobalNotInSourceInventory !TermNodeId Name
  | LengthSpinePairProblemGlobalHasNoLengthSummary !TermNodeId Name
  | LengthSpinePairProblemGlobalInstantiationRejected !TermNodeId Name
  | LengthSpinePairProblemSemanticLocalMissing !TermNodeId local
  | LengthSpinePairProblemExpectedCallable !TermNodeId
  | LengthSpinePairProblemExpectedSpine !TermNodeId
  | LengthSpinePairProblemExpectedTuple !OccurrenceId
  | LengthSpinePairProblemUnobservedTargetArgumentDemanded
      !Natural !LengthUnobservedTargetDemandSite
  | LengthSpinePairProblemStepPayloadDemanded
      !OccurrenceId !LengthStepPayloadDemandSite
  | LengthSpinePairProblemTupleArityMismatch !OccurrenceId !Int !Int
  | LengthSpinePairProblemExpectedResultTuple !TermNodeId
  | LengthSpinePairProblemResultTupleArityMismatch !TermNodeId !Int
  | LengthSpinePairProblemResultComponentExpectedSpine
      !LengthSpinePairComponent !TermNodeId
  | LengthSpinePairProblemResultTupleDemandedUnobservedTarget
      !Natural !TermNodeId
  | LengthSpinePairProblemResultTupleDemandedStepPayload
      !OccurrenceId !TermNodeId
  | LengthSpinePairProblemEvaluationStepLimitExceeded !Int !Int
  | LengthSpinePairProblemProviderTransferInvariant Name !Natural
  | LengthSpinePairProblemSyntaxRejected LengthSyntaxError
  | LengthSpinePairProblemFingerprintLimitExceeded
      !LengthSpinePairProblemFingerprintPart !Natural !Natural
  | LengthSpinePairProblemInternalSharedFailure
  deriving (Eq, Ord, Show, Generic)

instance
    (NFData failure, NFData identity, NFData local) =>
    NFData (LengthSpinePairProblemError failure identity local)

-- | Opaque interpreted result and identity for one engine-owned candidate.
--
-- This receipt is candidate-local: it deliberately carries no search-batch
-- completion or selection-status authority.
data CheckedLengthCandidate identity local = CheckedLengthCandidate
  !(LengthExpression LengthContractVariable)
  ![Name]
  !(Fingerprint
      (CandidateFingerprintSubject FiniteListSpineLengthV1))

type role CheckedLengthCandidate nominal nominal

instance NFData (CheckedLengthCandidate identity local) where
  rnf (CheckedLengthCandidate result providers candidate) =
    rnf result `seq` rnf providers `seq` rnf candidate

-- | Opaque complete Length counterexample problem for one checked candidate.
data CheckedLengthProblem identity local = CheckedLengthProblem
  !(CheckedLengthCandidate identity local)
  !Int
  !(LengthFormula LengthContractVariable)
  !(LengthFormula LengthContractVariable)
  !(LengthFormula LengthContractVariable)
  !(BehavioralProblem FiniteListSpineLengthV1)
  !(LengthCounterexampleBankScope identity)

type role CheckedLengthProblem nominal nominal

instance NFData (CheckedLengthProblem identity local) where
  rnf (CheckedLengthProblem candidate inputCount precondition postcondition
      condition problem scope) =
    rnf candidate `seq` rnf inputCount `seq` rnf precondition `seq`
    rnf postcondition `seq` rnf condition `seq`
    rnf (behavioralProblemEncodingFingerprint problem) `seq` rnf problem `seq`
    rnf scope

-- | Normalized symbolic length computed for the candidate result.
checkedLengthCandidateResult
  :: CheckedLengthCandidate identity local
  -> LengthExpression LengthContractVariable
checkedLengthCandidateResult (CheckedLengthCandidate result _ _) = result

-- | Exact provider laws reached by lazy interpretation, in name order.
checkedLengthCandidateUsedProviders
  :: CheckedLengthCandidate identity local -> [Name]
checkedLengthCandidateUsedProviders
    (CheckedLengthCandidate _ providers _) = providers

-- | Domain-wrapped candidate-only identity.
checkedLengthCandidateFingerprint
  :: CheckedLengthCandidate identity local
  -> Fingerprint
    (CandidateFingerprintSubject FiniteListSpineLengthV1)
checkedLengthCandidateFingerprint
    (CheckedLengthCandidate _ _ candidate) = candidate

-- | Recover the interpreted candidate receipt retained by the problem.
checkedLengthProblemCandidate
  :: CheckedLengthProblem identity local
  -> CheckedLengthCandidate identity local
checkedLengthProblemCandidate
    (CheckedLengthProblem candidate _ _ _ _ _ _) = candidate

-- | Number of compact source-ordered observed-spine inputs admitted by the
-- sealed contract.
--
-- This value is retained redundantly with the fingerprinted contract so a
-- model decoder can reject missing or extra assignments without receiving a
-- detachable contract from the caller.
checkedLengthProblemInputCount
  :: CheckedLengthProblem identity local
  -> Int
checkedLengthProblemInputCount
    (CheckedLengthProblem _ inputCount _ _ _ _ _) =
  inputCount

-- | Normalized contract precondition retained for ordered concrete replay.
checkedLengthProblemPrecondition
  :: CheckedLengthProblem identity local
  -> LengthFormula LengthContractVariable
checkedLengthProblemPrecondition
    (CheckedLengthProblem _ _ precondition _ _ _ _) = precondition

-- | Normalized contract postcondition before candidate-result substitution.
-- Concrete replay evaluates this only after the precondition succeeds and
-- binds 'LengthResult' to the result computed from the checked candidate.
checkedLengthProblemPostcondition
  :: CheckedLengthProblem identity local
  -> LengthFormula LengthContractVariable
checkedLengthProblemPostcondition
    (CheckedLengthProblem _ _ _ postcondition _ _ _) = postcondition

-- | Solver-neutral bad-state formula: precondition and negated postcondition.
checkedLengthProblemCounterexampleCondition
  :: CheckedLengthProblem identity local
  -> LengthFormula LengthContractVariable
checkedLengthProblemCounterexampleCondition
    (CheckedLengthProblem _ _ _ _ condition _ _) = condition

-- | Concrete identity of contract, policy, used laws, result, and bad state.
checkedLengthProblemEncodingFingerprint
  :: CheckedLengthProblem identity local
  -> Fingerprint
      (EncodingFingerprintSubject FiniteListSpineLengthV1)
checkedLengthProblemEncodingFingerprint
    (CheckedLengthProblem _ _ _ _ _ problem _) =
  behavioralProblemEncodingFingerprint problem

-- | Generic domain/inventory/encoding/candidate/problem envelope.
checkedLengthProblemBehavioralProblem
  :: CheckedLengthProblem identity local
  -> BehavioralProblem FiniteListSpineLengthV1
checkedLengthProblemBehavioralProblem
    (CheckedLengthProblem _ _ _ _ _ problem _) = problem

-- | Candidate-independent replay-input scope sealed solely from the exact
-- session, revalidated contract, and normalized target.  Its construction is
-- intentionally independent of every candidate-specific field retained by
-- this problem.
checkedLengthProblemCounterexampleBankScope
  :: CheckedLengthProblem identity local
  -> LengthCounterexampleBankScope identity
checkedLengthProblemCounterexampleBankScope
    (CheckedLengthProblem _ _ _ _ _ _ scope) = scope

-- | Opaque symbolic binary spine result for one engine-owned candidate.
data CheckedLengthSpinePairCandidate identity local =
  CheckedLengthSpinePairCandidate
    !(LengthSpinePair (LengthExpression LengthContractVariable))
    ![Name]
    !(Fingerprint
        (CandidateFingerprintSubject FiniteBinaryProductSpineLengthsV1))

type role CheckedLengthSpinePairCandidate nominal nominal

instance NFData (CheckedLengthSpinePairCandidate identity local) where
  rnf (CheckedLengthSpinePairCandidate result providers candidate) =
    rnf result `seq` rnf providers `seq` rnf candidate

-- | Opaque complete counterexample problem for a boxed binary product of
-- modeled finite spines.
data CheckedLengthSpinePairProblem identity local =
  CheckedLengthSpinePairProblem
    !(CheckedLengthSpinePairCandidate identity local)
    !Int
    !(LengthFormula LengthSpinePairContractVariable)
    !(LengthFormula LengthSpinePairContractVariable)
    !(LengthFormula LengthContractVariable)
    !(BehavioralProblem FiniteBinaryProductSpineLengthsV1)
    !(LengthSpinePairCounterexampleBankScope identity)

type role CheckedLengthSpinePairProblem nominal nominal

instance NFData (CheckedLengthSpinePairProblem identity local) where
  rnf (CheckedLengthSpinePairProblem candidate inputCount precondition
      postcondition condition problem scope) =
    rnf candidate `seq` rnf inputCount `seq` rnf precondition `seq`
    rnf postcondition `seq` rnf condition `seq`
    rnf (behavioralProblemEncodingFingerprint problem) `seq` rnf problem `seq`
    rnf scope

-- | Normalized symbolic lengths computed for both components of the
-- candidate's product result.
checkedLengthSpinePairCandidateResult
  :: CheckedLengthSpinePairCandidate identity local
  -> LengthSpinePair (LengthExpression LengthContractVariable)
checkedLengthSpinePairCandidateResult
    (CheckedLengthSpinePairCandidate result _ _) = result

-- | Exact provider laws reached by lazy interpretation, in name order.
checkedLengthSpinePairCandidateUsedProviders
  :: CheckedLengthSpinePairCandidate identity local -> [Name]
checkedLengthSpinePairCandidateUsedProviders
    (CheckedLengthSpinePairCandidate _ providers _) = providers

-- | Product-domain-wrapped candidate-only identity.
checkedLengthSpinePairCandidateFingerprint
  :: CheckedLengthSpinePairCandidate identity local
  -> Fingerprint
      (CandidateFingerprintSubject FiniteBinaryProductSpineLengthsV1)
checkedLengthSpinePairCandidateFingerprint
    (CheckedLengthSpinePairCandidate _ _ candidate) = candidate

-- | Recover the interpreted product candidate receipt retained by the
-- problem.
checkedLengthSpinePairProblemCandidate
  :: CheckedLengthSpinePairProblem identity local
  -> CheckedLengthSpinePairCandidate identity local
checkedLengthSpinePairProblemCandidate
    (CheckedLengthSpinePairProblem candidate _ _ _ _ _ _) = candidate

-- | Number of compact source-ordered observed-spine inputs admitted by the
-- sealed product contract, retained redundantly with the fingerprinted
-- contract so a model decoder can reject missing or extra assignments.
checkedLengthSpinePairProblemInputCount
  :: CheckedLengthSpinePairProblem identity local -> Int
checkedLengthSpinePairProblemInputCount
    (CheckedLengthSpinePairProblem _ inputCount _ _ _ _ _) = inputCount

-- | Normalized product contract precondition retained for ordered concrete
-- replay.
checkedLengthSpinePairProblemPrecondition
  :: CheckedLengthSpinePairProblem identity local
  -> LengthFormula LengthSpinePairContractVariable
checkedLengthSpinePairProblemPrecondition
    (CheckedLengthSpinePairProblem _ _ precondition _ _ _ _) = precondition

-- | Normalized product contract postcondition before candidate-result
-- substitution.  Concrete replay evaluates this only after the precondition
-- succeeds and binds both 'LengthSpinePairResult' components to the results
-- computed from the checked candidate.
checkedLengthSpinePairProblemPostcondition
  :: CheckedLengthSpinePairProblem identity local
  -> LengthFormula LengthSpinePairContractVariable
checkedLengthSpinePairProblemPostcondition
    (CheckedLengthSpinePairProblem _ _ _ postcondition _ _ _) = postcondition

-- | Solver-neutral bad-state formula: precondition and negated postcondition
-- with both result components substituted, so only input variables remain.
checkedLengthSpinePairProblemCounterexampleCondition
  :: CheckedLengthSpinePairProblem identity local
  -> LengthFormula LengthContractVariable
checkedLengthSpinePairProblemCounterexampleCondition
    (CheckedLengthSpinePairProblem _ _ _ _ condition _ _) = condition

-- | Concrete identity of contract, policy, used laws, results, and bad state.
checkedLengthSpinePairProblemEncodingFingerprint
  :: CheckedLengthSpinePairProblem identity local
  -> Fingerprint
      (EncodingFingerprintSubject FiniteBinaryProductSpineLengthsV1)
checkedLengthSpinePairProblemEncodingFingerprint
    (CheckedLengthSpinePairProblem _ _ _ _ _ problem _) =
  behavioralProblemEncodingFingerprint problem

-- | Generic domain/inventory/encoding/candidate/problem envelope for the
-- product domain.
checkedLengthSpinePairProblemBehavioralProblem
  :: CheckedLengthSpinePairProblem identity local
  -> BehavioralProblem FiniteBinaryProductSpineLengthsV1
checkedLengthSpinePairProblemBehavioralProblem
    (CheckedLengthSpinePairProblem _ _ _ _ _ problem _) = problem

-- | Product-domain sibling of the candidate-independent replay-input scope:
-- sealed solely from the exact session, revalidated contract, and normalized
-- target, never from a candidate-specific field of this problem.
checkedLengthSpinePairProblemCounterexampleBankScope
  :: CheckedLengthSpinePairProblem identity local
  -> LengthSpinePairCounterexampleBankScope identity
checkedLengthSpinePairProblemCounterexampleBankScope
    (CheckedLengthSpinePairProblem _ _ _ _ _ _ scope) = scope

-- | Atomically retain, interpret, identify, and envelope one engine-owned
-- typed candidate. The provider inventory is consumed directly from the
-- opaque checked session; only the separately supplied contract is revalidated
-- through that session's context. Residual constraints fail before graph
-- availability, and every semantic/global check fails closed.
--
-- The fresh graph fingerprint enforces the caller's graph limits before the
-- domain walk. Root opening then authorizes rigid variables; holes and
-- unsupported forms, exact-session kinds, visible selections, and every
-- global are checked before lazy interpretation. The generic behavioral
-- envelope is constructed only after all concrete identities succeed.
sealLengthTypedCandidateProblem
  :: (Ord identity, Ord local)
  => LengthProblemLimits
  -> CheckedLengthSession identity annotation
  -> CheckedLengthContract (Variable identity)
  -> TypedCandidate failure
      (Type (Variable identity))
      local
      (Candidate (Type (Variable identity)) details output)
  -> Either
      (LengthProblemError failure identity local)
      (CheckedLengthProblem identity local)
sealLengthTypedCandidateProblem = sealLengthTypedCandidateProblemWithMode
  LengthLegacyProblemSealer

-- | Seal a candidate under the role-aware interpreter.  An all-observed
-- contract canonicalizes to legacy identities; a mixed contract requires a
-- session sealed for mixed opaque-target semantics.
sealRoleAwareLengthTypedCandidateProblem
  :: (Ord identity, Ord local)
  => LengthProblemLimits
  -> CheckedLengthSession identity annotation
  -> CheckedLengthContract (Variable identity)
  -> TypedCandidate failure
      (Type (Variable identity))
      local
      (Candidate (Type (Variable identity)) details output)
  -> Either
      (LengthProblemError failure identity local)
      (CheckedLengthProblem identity local)
sealRoleAwareLengthTypedCandidateProblem = sealLengthTypedCandidateProblemWithMode
  LengthRoleAwareProblemSealer

-- | Seal a candidate under an explicitly case-aware checked session.
--
-- Only exact complete zero/step splits over the session's modeled spine are
-- admitted.  Target roles remain explicit and may be all observed or mixed;
-- ordinary session and problem entrances continue to reject every case.
sealExactSpineCaseLengthTypedCandidateProblem
  :: (Ord identity, Ord local)
  => LengthProblemLimits
  -> CheckedLengthSession identity annotation
  -> CheckedLengthContract (Variable identity)
  -> TypedCandidate failure
      (Type (Variable identity))
      local
      (Candidate (Type (Variable identity)) details output)
  -> Either
      (LengthProblemError failure identity local)
      (CheckedLengthProblem identity local)
sealExactSpineCaseLengthTypedCandidateProblem =
  sealLengthTypedCandidateProblemWithMode LengthExactSpineCaseProblemSealer

-- | Seal under the exact interpretation authority retained by the session.
--
-- Unlike the compatibility wrappers, an explicitly associated policy must
-- match the checked contract's complete role vector, including order and
-- arity.  The check precedes contract resealing, residuals, and graph demand.
sealLengthTypedCandidateProblemInSession
  :: (Ord identity, Ord local)
  => LengthProblemLimits
  -> CheckedLengthSession identity annotation
  -> CheckedLengthContract (Variable identity)
  -> TypedCandidate failure
      (Type (Variable identity))
      local
      (Candidate (Type (Variable identity)) details output)
  -> Either
      (LengthProblemError failure identity local)
      (CheckedLengthProblem identity local)
sealLengthTypedCandidateProblemInSession =
  sealLengthTypedCandidateProblemWithMode LengthSessionPolicyProblemSealer

-- | Product-domain sibling of 'sealLengthTypedCandidateProblem'.  The
-- candidate is interpreted to a binary spine pair, both components are
-- normalized and substituted into the bad-state formula, and a contract with
-- an unobserved target argument is rejected before any revalidation.
sealLengthSpinePairTypedCandidateProblem
  :: (Ord identity, Ord local)
  => LengthProblemLimits
  -> CheckedLengthSession identity annotation
  -> CheckedLengthSpinePairContract (Variable identity)
  -> TypedCandidate failure
      (Type (Variable identity))
      local
      (Candidate (Type (Variable identity)) details output)
  -> Either
      (LengthSpinePairProblemError failure identity local)
      (CheckedLengthSpinePairProblem identity local)
sealLengthSpinePairTypedCandidateProblem =
  sealLengthSpinePairTypedCandidateProblemWithMode LengthLegacyProblemSealer

-- | Product-domain sibling of 'sealRoleAwareLengthTypedCandidateProblem'.
-- A mixed contract requires a session sealed for mixed opaque-target
-- semantics, and the contract's mixedness must match the session's policy.
sealRoleAwareLengthSpinePairTypedCandidateProblem
  :: (Ord identity, Ord local)
  => LengthProblemLimits
  -> CheckedLengthSession identity annotation
  -> CheckedLengthSpinePairContract (Variable identity)
  -> TypedCandidate failure
      (Type (Variable identity))
      local
      (Candidate (Type (Variable identity)) details output)
  -> Either
      (LengthSpinePairProblemError failure identity local)
      (CheckedLengthSpinePairProblem identity local)
sealRoleAwareLengthSpinePairTypedCandidateProblem =
  sealLengthSpinePairTypedCandidateProblemWithMode LengthRoleAwareProblemSealer

-- | Product-domain sibling of
-- 'sealExactSpineCaseLengthTypedCandidateProblem'.  The session must have
-- been sealed with the exact zero/step case policy; the legacy and role-aware
-- product sealers instead require a session that rejects cases.
sealExactSpineCaseLengthSpinePairTypedCandidateProblem
  :: (Ord identity, Ord local)
  => LengthProblemLimits
  -> CheckedLengthSession identity annotation
  -> CheckedLengthSpinePairContract (Variable identity)
  -> TypedCandidate failure
      (Type (Variable identity))
      local
      (Candidate (Type (Variable identity)) details output)
  -> Either
      (LengthSpinePairProblemError failure identity local)
      (CheckedLengthSpinePairProblem identity local)
sealExactSpineCaseLengthSpinePairTypedCandidateProblem =
  sealLengthSpinePairTypedCandidateProblemWithMode
    LengthExactSpineCaseProblemSealer

-- | Product-domain sibling of 'sealLengthTypedCandidateProblemInSession'.
-- The session's own case policy is required, and an explicitly associated
-- target-role vector must equal the contract's roles, order and arity
-- included, before the contract is resealed through the session.
sealLengthSpinePairTypedCandidateProblemInSession
  :: (Ord identity, Ord local)
  => LengthProblemLimits
  -> CheckedLengthSession identity annotation
  -> CheckedLengthSpinePairContract (Variable identity)
  -> TypedCandidate failure
      (Type (Variable identity))
      local
      (Candidate (Type (Variable identity)) details output)
  -> Either
      (LengthSpinePairProblemError failure identity local)
      (CheckedLengthSpinePairProblem identity local)
sealLengthSpinePairTypedCandidateProblemInSession =
  sealLengthSpinePairTypedCandidateProblemWithMode
    LengthSessionPolicyProblemSealer

data LengthProblemSealer
  = LengthLegacyProblemSealer
  | LengthRoleAwareProblemSealer
  | LengthExactSpineCaseProblemSealer
  | LengthSessionPolicyProblemSealer

-- This discriminator never escapes the candidate sealer.  Empty certificate
-- carriers deliberately collapse to the legacy branch so their graph and
-- candidate identities remain byte-for-byte equal to a plain candidate.
data LengthCandidateAuthority
  = LengthPlainCandidateAuthority
  | LengthOpaqueAssociatedCertificateAuthority
  | LengthGroundDischargedAssociatedCertificateAuthority

-- Exact occurrence-local authority for one conditional provider row.  The
-- retained class receipts remain bound to the session's checked environment;
-- only the provider summary itself is projected during interpretation.
data ConditionalProviderAuthorization identity =
  ConditionalProviderAuthorization
    !(CheckedLengthProviderSummary (Variable identity))
    ![CheckedConstraintDischarge (Variable identity)]

data LengthCandidateAuthorization identity = LengthCandidateAuthorization
  !LengthCandidateAuthority
  !(Map TermNodeId (ConditionalProviderAuthorization identity))
    -- ^ Exact conditional base-global nodes, retained as sentinels only.
  !(Map TermNodeId (ConditionalProviderAuthorization identity))
    -- ^ Exact final visible-application nodes authorized for provider use.

candidateAuthorizationIdentity
  :: LengthCandidateAuthorization identity
  -> LengthCandidateAuthority
candidateAuthorizationIdentity
    (LengthCandidateAuthorization authority _ _) = authority

candidateAuthorizationBases
  :: LengthCandidateAuthorization identity
  -> Map TermNodeId (ConditionalProviderAuthorization identity)
candidateAuthorizationBases
    (LengthCandidateAuthorization _ bases _) = bases

candidateAuthorizationFinals
  :: LengthCandidateAuthorization identity
  -> Map TermNodeId (ConditionalProviderAuthorization identity)
candidateAuthorizationFinals
    (LengthCandidateAuthorization _ _ finals) = finals

emptyCandidateAuthorization
  :: LengthCandidateAuthority
  -> LengthCandidateAuthorization identity
emptyCandidateAuthorization authority = LengthCandidateAuthorization
  authority Map.empty Map.empty

data ConditionalAssociatedProviderRow identity =
  ConditionalAssociatedProviderRow
    !Name
    !Natural
    !TermNodeId
    ![(TermNodeId, CheckedTypeApplicationCertificateStep (Variable identity))]
    !(CheckedLengthProviderSummary (Variable identity))

data AssociatedProviderClassification identity =
  AssociatedProviderClassification
    !Natural
    ![ConditionalAssociatedProviderRow identity]

data ProtectedConditionalProviderNode = ProtectedConditionalProviderNode
  !Name !Natural !LengthAssociatedProviderChainSite

data ConditionalProviderChainAudit = ConditionalProviderChainAudit
  !(Map TermNodeId (Set TermNodeId))
    -- ^ Protected child to complete union of certified function parents.
  !(Map TermNodeId ProtectedConditionalProviderNode)
  ![TermNodeId]
    -- ^ First canonical row/site occurrence for deterministic diagnostics.

sealLengthTypedCandidateProblemWithMode
  :: (Ord identity, Ord local)
  => LengthProblemSealer
  -> LengthProblemLimits
  -> CheckedLengthSession identity annotation
  -> CheckedLengthContract (Variable identity)
  -> TypedCandidate failure
      (Type (Variable identity))
      local
      (Candidate (Type (Variable identity)) details output)
  -> Either
      (LengthProblemError failure identity local)
      (CheckedLengthProblem identity local)
sealLengthTypedCandidateProblemWithMode sealer problemLimits session
    suppliedContract typed = do
  let suppliedRoles = checkedLengthContractTargetArgumentRoles suppliedContract
      mixedContract = LengthUnobservedTarget `elem` suppliedRoles
      mixedSession = checkedLengthSessionTargetArgumentPolicy session
        == LengthMixedTargetPolicy
  case sealer of
    LengthLegacyProblemSealer
      | mixedContract -> Left
          LengthProblemMixedTargetArgumentsRequireRoleAwareSealer
    LengthSessionPolicyProblemSealer
      | checkedLengthSessionExplicitTargetRoles session == Nothing
      , mixedContract -> Left
          LengthProblemMixedTargetArgumentsRequireRoleAwareSealer
    _ -> pure ()
  let expectedCasePolicy = case sealer of
        LengthExactSpineCaseProblemSealer -> LengthExactZeroStepCases
        LengthSessionPolicyProblemSealer ->
          checkedLengthSessionCasePolicy session
        _ -> LengthCasesRejected
  if checkedLengthSessionCasePolicy session == expectedCasePolicy
    then pure ()
    else Left LengthProblemCasePolicyMismatch
  if mixedContract == mixedSession
    then pure ()
    else Left LengthProblemTargetArgumentPolicyMismatch
  case sealer of
    LengthSessionPolicyProblemSealer -> case
        checkedLengthSessionExplicitTargetRoles session of
      Nothing -> pure ()
      Just expectedRoles
        | suppliedRoles == expectedRoles -> pure ()
        | otherwise -> Left LengthProblemTargetArgumentPolicyMismatch
    _ -> pure ()
  let providers = checkedLengthSessionProviderInventory session
  contract <- case sealer of
    LengthSessionPolicyProblemSealer ->
      revalidateContractInSession session suppliedContract
    _ -> revalidateContract session suppliedContract
  let compatibility = typedCandidateCompatibility typed
  case candidateResidualConstraints compatibility of
    constraint : _ -> Left $ LengthProblemResidualConstraint constraint
    [] -> pure ()
  (graph, graphFingerprint, candidateAuthorization) <-
    retainLengthCandidateGraph session problemLimits typed
  rootNode <- case lookupTermNode (termGraphRoot graph) graph of
    Nothing -> Left $ LengthProblemRootNodeMissing $ termGraphRoot graph
    Just node -> Right node
  authorizedRigids <- matchRootOpening
    (checkedLengthContractTarget contract) $ termNodeType rootNode
  preflight <- preflightGraph session providers candidateAuthorization
    authorizedRigids graph
  (rawResult, evaluationState) <- runStateT
    (interpretCompleteCandidate
      InterpretationContext
        { interpretationSession = session
        , interpretationProblemLimits = problemLimits
        , interpretationGraph = graph
        , interpretationGlobals = preflightGlobals preflight
        , interpretationCases = preflightCases preflight
        , interpretationConditionalProviderFinals =
            preflightConditionalProviderFinals preflight
        , interpretationTargetArgumentRoles =
            checkedLengthContractTargetArgumentRoles contract
        , interpretationInputCount = checkedLengthContractInputCount contract
        }
      $ termGraphRoot graph)
    emptyEvaluationState
  let limits = checkedLengthSessionLimits session
      checkCandidateVariable = validateCandidateVariable
        $ checkedLengthContractInputCount contract
  (result, usage) <- first LengthProblemSyntaxRejected
    $ normalizeLengthExpression limits checkCandidateVariable
        emptySyntaxUsage rawResult
  let rawCondition = LengthAll
        [ checkedLengthContractPrecondition contract
        , LengthNot $ substituteResultFormula result
            $ checkedLengthContractPostcondition contract
        ]
  (condition, _) <- first LengthProblemSyntaxRejected
    $ normalizeLengthFormula limits checkCandidateVariable usage rawCondition
  let usedProvidersByName = evaluationUsedProviders evaluationState
      usedProviderNames = materializeProviderNames usedProvidersByName
  -- Preserve pre-fingerprint materialization without forcing name payloads.
  usedProviderNames `seq` pure ()
  encodingFingerprint <- mapFingerprintFailure
    LengthConcreteEncodingFingerprint
    $ buildConcreteEncodingFingerprint session contract
        (Map.elems usedProvidersByName)
        result condition
  candidateFingerprint <- mapFingerprintFailure LengthCandidateFingerprint
    $ buildCandidateFingerprint session
        (candidateAuthorizationIdentity candidateAuthorization)
        graphFingerprint
  problemFingerprint <- mapFingerprintFailure LengthCompleteProblemFingerprint
    $ buildCompleteProblemFingerprint session encodingFingerprint
        candidateFingerprint
  bankScope <- mapFingerprintFailure
    LengthCounterexampleBankScopeFingerprint
    $ sealLengthCounterexampleBankScope session contract
  let checkedCandidate = CheckedLengthCandidate
        result usedProviderNames candidateFingerprint
      behavioralProblem = mkBehavioralProblem
        finiteListSpineLengthDomainTag
        (lengthSessionInventoryFingerprint session)
        encodingFingerprint
        candidateFingerprint
        problemFingerprint
  pure $ CheckedLengthProblem checkedCandidate
    (checkedLengthContractInputCount contract)
    (checkedLengthContractPrecondition contract)
    (checkedLengthContractPostcondition contract)
    condition
    behavioralProblem
    bankScope

sealLengthSpinePairTypedCandidateProblemWithMode
  :: (Ord identity, Ord local)
  => LengthProblemSealer
  -> LengthProblemLimits
  -> CheckedLengthSession identity annotation
  -> CheckedLengthSpinePairContract (Variable identity)
  -> TypedCandidate failure
      (Type (Variable identity))
      local
      (Candidate (Type (Variable identity)) details output)
  -> Either
      (LengthSpinePairProblemError failure identity local)
      (CheckedLengthSpinePairProblem identity local)
sealLengthSpinePairTypedCandidateProblemWithMode sealer problemLimits session
    suppliedContract typed = do
  let suppliedRoles =
        checkedLengthSpinePairContractTargetArgumentRoles suppliedContract
      mixedContract = LengthUnobservedTarget `elem` suppliedRoles
      mixedSession = checkedLengthSessionTargetArgumentPolicy session
        == LengthMixedTargetPolicy
  case sealer of
    LengthLegacyProblemSealer
      | mixedContract -> Left
          LengthSpinePairProblemMixedTargetArgumentsRequireRoleAwareSealer
    LengthSessionPolicyProblemSealer
      | checkedLengthSessionExplicitTargetRoles session == Nothing
      , mixedContract -> Left
          LengthSpinePairProblemMixedTargetArgumentsRequireRoleAwareSealer
    _ -> pure ()
  let expectedCasePolicy = case sealer of
        LengthExactSpineCaseProblemSealer -> LengthExactZeroStepCases
        LengthSessionPolicyProblemSealer ->
          checkedLengthSessionCasePolicy session
        _ -> LengthCasesRejected
  if checkedLengthSessionCasePolicy session == expectedCasePolicy
    then pure ()
    else Left LengthSpinePairProblemCasePolicyMismatch
  if mixedContract == mixedSession
    then pure ()
    else Left LengthSpinePairProblemTargetArgumentPolicyMismatch
  case sealer of
    LengthSessionPolicyProblemSealer -> case
        checkedLengthSessionExplicitTargetRoles session of
      Nothing -> pure ()
      Just expectedRoles
        | suppliedRoles == expectedRoles -> pure ()
        | otherwise -> Left
            LengthSpinePairProblemTargetArgumentPolicyMismatch
    _ -> pure ()
  let providers = checkedLengthSessionProviderInventory session
  contract <- case sealer of
    LengthSessionPolicyProblemSealer ->
      revalidateSpinePairContractInSession session suppliedContract
    _ -> revalidateSpinePairContract session suppliedContract
  let compatibility = typedCandidateCompatibility typed
  case candidateResidualConstraints compatibility of
    constraint : _ -> Left
      $ LengthSpinePairProblemResidualConstraint constraint
    [] -> pure ()
  (graph, graphFingerprint, candidateAuthorization) <- mapSharedFailure
    $ retainLengthCandidateGraph session problemLimits typed
  rootNode <- case lookupTermNode (termGraphRoot graph) graph of
    Nothing -> Left $ LengthSpinePairProblemRootNodeMissing
      $ termGraphRoot graph
    Just node -> Right node
  authorizedRigids <- mapSharedFailure $ matchRootOpening
    (checkedLengthSpinePairContractTarget contract) $ termNodeType rootNode
  preflight <- mapSharedFailure
    $ preflightGraph session providers candidateAuthorization
        authorizedRigids graph
  (rawResultOr, evaluationState) <- mapSharedFailure $ runStateT
    (interpretCompleteSpinePairCandidate
      InterpretationContext
        { interpretationSession = session
        , interpretationProblemLimits = problemLimits
        , interpretationGraph = graph
        , interpretationGlobals = preflightGlobals preflight
        , interpretationCases = preflightCases preflight
        , interpretationConditionalProviderFinals =
            preflightConditionalProviderFinals preflight
        , interpretationTargetArgumentRoles =
            checkedLengthSpinePairContractTargetArgumentRoles contract
        , interpretationInputCount =
            checkedLengthSpinePairContractInputCount contract
        }
      $ termGraphRoot graph)
    emptyEvaluationState
  rawResult <- first mapSpinePairResultShapeError rawResultOr
  let limits = checkedLengthSessionLimits session
      checkCandidateVariable = validateCandidateVariable
        $ checkedLengthSpinePairContractInputCount contract
  (firstResult, afterFirst) <- first LengthSpinePairProblemSyntaxRejected
    $ normalizeLengthExpression limits checkCandidateVariable
        emptySyntaxUsage $ lengthSpinePairFirst rawResult
  (secondResult, usage) <- first LengthSpinePairProblemSyntaxRejected
    $ normalizeLengthExpression limits checkCandidateVariable
        afterFirst $ lengthSpinePairSecond rawResult
  let result = LengthSpinePair firstResult secondResult
      rawCondition = LengthAll
        [ substituteSpinePairResultFormula result
            $ checkedLengthSpinePairContractPrecondition contract
        , LengthNot $ substituteSpinePairResultFormula result
            $ checkedLengthSpinePairContractPostcondition contract
        ]
  (condition, _) <- first LengthSpinePairProblemSyntaxRejected
    $ normalizeLengthFormula limits checkCandidateVariable usage rawCondition
  let usedProvidersByName = evaluationUsedProviders evaluationState
      usedProviderNames = materializeProviderNames usedProvidersByName
  usedProviderNames `seq` pure ()
  productInventory <- mapSpinePairFingerprintFailure
    LengthSpinePairInventoryFingerprint
    $ buildFiniteBinaryProductSpineLengthsInventoryFingerprint session
  encodingFingerprint <- mapSpinePairFingerprintFailure
    LengthSpinePairConcreteEncodingFingerprint
    $ buildSpinePairConcreteEncodingFingerprint session contract
        (Map.elems usedProvidersByName) result condition
  candidateFingerprint <- mapSpinePairFingerprintFailure
    LengthSpinePairCandidateFingerprint
    $ buildSpinePairCandidateFingerprint session
        (candidateAuthorizationIdentity candidateAuthorization)
        graphFingerprint
  problemFingerprint <- mapSpinePairFingerprintFailure
    LengthSpinePairCompleteProblemFingerprint
    $ buildSpinePairCompleteProblemFingerprint session productInventory
        encodingFingerprint candidateFingerprint
  bankScope <- mapSpinePairFingerprintFailure
    LengthSpinePairCounterexampleBankScopeFingerprint
    $ sealLengthSpinePairCounterexampleBankScopeWithInventory
        session productInventory contract
  let checkedCandidate = CheckedLengthSpinePairCandidate
        result usedProviderNames candidateFingerprint
      behavioralProblem = mkBehavioralProblem
        finiteBinaryProductSpineLengthsDomainTag
        productInventory
        encodingFingerprint
        candidateFingerprint
        problemFingerprint
  pure $ CheckedLengthSpinePairProblem checkedCandidate
    (checkedLengthSpinePairContractInputCount contract)
    (checkedLengthSpinePairContractPrecondition contract)
    (checkedLengthSpinePairContractPostcondition contract)
    condition
    behavioralProblem
    bankScope
 where
  mapSharedFailure = first lengthProblemErrorToSpinePairProblemError

revalidateContract
  :: Ord identity
  => CheckedLengthSession identity annotation
  -> CheckedLengthContract (Variable identity)
  -> Either
      (LengthProblemError failure identity local)
      (CheckedLengthContract (Variable identity))
revalidateContract session original = revalidateContractWith
  (sealRoleAwareLengthContractInContext
    (checkedLengthSessionLimits session)
    (checkedLengthSessionContext session)
    (checkedLengthContractTargetArgumentRoles original))
  original

-- The unified entrance replays the detached contract through the session's
-- retained authority.  Compatibility entrances intentionally keep replaying
-- with the detached contract's roles after their historical loose mixedness
-- check, preserving the association behavior those wrappers exposed.
revalidateContractInSession
  :: Ord identity
  => CheckedLengthSession identity annotation
  -> CheckedLengthContract (Variable identity)
  -> Either
      (LengthProblemError failure identity local)
      (CheckedLengthContract (Variable identity))
revalidateContractInSession session = revalidateContractWith
  $ sealLengthContractInSession session

revalidateContractWith
  :: ( Type (Variable identity)
       -> LengthContractSource
       -> Either
            (LengthContractError (Variable identity))
            (CheckedLengthContract (Variable identity))
     )
  -> CheckedLengthContract (Variable identity)
  -> Either
      (LengthProblemError failure identity local)
      (CheckedLengthContract (Variable identity))
revalidateContractWith sealer original = do
  checked <- first LengthProblemContractResealRejected
    $ sealer
        (checkedLengthContractTarget original)
        LengthContractSource
          { lengthContractPrecondition =
              checkedLengthContractPrecondition original
          , lengthContractPostcondition =
              checkedLengthContractPostcondition original
          }
  if lengthContractFingerprint checked == lengthContractFingerprint original
    then Right checked
    else Left LengthProblemContractContextMismatch

revalidateSpinePairContract
  :: Ord identity
  => CheckedLengthSession identity annotation
  -> CheckedLengthSpinePairContract (Variable identity)
  -> Either
      (LengthSpinePairProblemError failure identity local)
      (CheckedLengthSpinePairContract (Variable identity))
revalidateSpinePairContract session original =
  revalidateSpinePairContractWith
    (sealRoleAwareLengthSpinePairContractInContext
      (checkedLengthSessionLimits session)
      (checkedLengthSessionContext session)
      (checkedLengthSpinePairContractTargetArgumentRoles original))
    original

revalidateSpinePairContractInSession
  :: Ord identity
  => CheckedLengthSession identity annotation
  -> CheckedLengthSpinePairContract (Variable identity)
  -> Either
      (LengthSpinePairProblemError failure identity local)
      (CheckedLengthSpinePairContract (Variable identity))
revalidateSpinePairContractInSession session =
  revalidateSpinePairContractWith
    $ sealLengthSpinePairContractInSession session

revalidateSpinePairContractWith
  :: ( Type (Variable identity)
       -> LengthSpinePairContractSource
       -> Either
            (LengthSpinePairContractError (Variable identity))
            (CheckedLengthSpinePairContract (Variable identity))
     )
  -> CheckedLengthSpinePairContract (Variable identity)
  -> Either
      (LengthSpinePairProblemError failure identity local)
      (CheckedLengthSpinePairContract (Variable identity))
revalidateSpinePairContractWith sealer original = do
  checked <- first LengthSpinePairProblemContractResealRejected
    $ sealer
        (checkedLengthSpinePairContractTarget original)
        LengthSpinePairContractSource
          { lengthSpinePairContractPrecondition =
              checkedLengthSpinePairContractPrecondition original
          , lengthSpinePairContractPostcondition =
              checkedLengthSpinePairContractPostcondition original
          }
  if lengthSpinePairContractFingerprint checked ==
      lengthSpinePairContractFingerprint original
    then Right checked
    else Left LengthSpinePairProblemContractContextMismatch

mapSpinePairResultShapeError
  :: LengthSpinePairResultShapeError
  -> LengthSpinePairProblemError failure identity local
mapSpinePairResultShapeError failure = case failure of
  LengthSpinePairExpectedResultTuple owner ->
    LengthSpinePairProblemExpectedResultTuple owner
  LengthSpinePairResultTupleArityMismatch owner observed ->
    LengthSpinePairProblemResultTupleArityMismatch owner observed
  LengthSpinePairResultComponentExpectedSpine component owner ->
    LengthSpinePairProblemResultComponentExpectedSpine component owner
  LengthSpinePairResultTupleDemandedUnobservedTarget position owner ->
    LengthSpinePairProblemResultTupleDemandedUnobservedTarget position owner
  LengthSpinePairResultTupleDemandedStepPayload occurrence owner ->
    LengthSpinePairProblemResultTupleDemandedStepPayload occurrence owner

lengthProblemErrorToSpinePairProblemError
  :: LengthProblemError failure identity local
  -> LengthSpinePairProblemError failure identity local
lengthProblemErrorToSpinePairProblemError failure = case failure of
  LengthProblemContractResealRejected{} ->
    LengthSpinePairProblemInternalSharedFailure
  LengthProblemContractContextMismatch ->
    LengthSpinePairProblemInternalSharedFailure
  LengthProblemMixedTargetArgumentsRequireRoleAwareSealer ->
    LengthSpinePairProblemMixedTargetArgumentsRequireRoleAwareSealer
  LengthProblemCasePolicyMismatch ->
    LengthSpinePairProblemCasePolicyMismatch
  LengthProblemTargetArgumentPolicyMismatch ->
    LengthSpinePairProblemTargetArgumentPolicyMismatch
  LengthProblemResidualConstraint constraint ->
    LengthSpinePairProblemResidualConstraint constraint
  LengthProblemTypedGraphUnavailable graphFailure ->
    LengthSpinePairProblemTypedGraphUnavailable graphFailure
  LengthProblemAssociatedCertificateOwnerMissing name row ->
    LengthSpinePairProblemAssociatedCertificateOwnerMissing name row
  LengthProblemAssociatedCertificateSourceSchemeMismatch name row ->
    LengthSpinePairProblemAssociatedCertificateSourceSchemeMismatch name row
  LengthProblemAssociatedCertificateActivatedObligations name row step count ->
    LengthSpinePairProblemAssociatedCertificateActivatedObligations
      name row step count
  LengthProblemAssociatedCertificateModeledConstructorUnsupported name row ->
    LengthSpinePairProblemAssociatedCertificateModeledConstructorUnsupported
      name row
  LengthProblemAssociatedCertificateProviderSummaryMissing name row ->
    LengthSpinePairProblemAssociatedCertificateProviderSummaryMissing name row
  LengthProblemAssociatedCertificateConditionalObligationsMissing name row ->
    LengthSpinePairProblemAssociatedCertificateConditionalObligationsMissing
      name row
  LengthProblemAssociatedCertificateConstraintDischargeRejected
      name row step obligation reason ->
    LengthSpinePairProblemAssociatedCertificateConstraintDischargeRejected
      name row step obligation reason
  LengthProblemAssociatedCertificateProtectedChainRejected
      name row site reason ->
    LengthSpinePairProblemAssociatedCertificateProtectedChainRejected
      name row site reason
  LengthProblemConditionalProviderRequiresDischarge node name ->
    LengthSpinePairProblemConditionalProviderRequiresDischarge node name
  LengthProblemTermGraphFingerprintRejected graphFailure ->
    LengthSpinePairProblemTermGraphFingerprintRejected graphFailure
  LengthProblemRootNodeMissing node ->
    LengthSpinePairProblemRootNodeMissing node
  LengthProblemRootOpeningRejected reason ->
    LengthSpinePairProblemRootOpeningRejected reason
  LengthProblemHole node local -> LengthSpinePairProblemHole node local
  LengthProblemUnsupportedCase node ->
    LengthSpinePairProblemUnsupportedCase node
  LengthProblemUnsupportedConstructorPattern occurrence name ->
    LengthSpinePairProblemUnsupportedConstructorPattern occurrence name
  LengthProblemCaseResultIsNotModeledSpine node resultType ->
    LengthSpinePairProblemCaseResultIsNotModeledSpine node resultType
  LengthProblemCaseScrutineeIsNotModeledSpine node scrutineeType ->
    LengthSpinePairProblemCaseScrutineeIsNotModeledSpine node scrutineeType
  LengthProblemCaseAlternativeCountMismatch node observed ->
    LengthSpinePairProblemCaseAlternativeCountMismatch node observed
  LengthProblemCasePatternIsNotDirectConstructor occurrence ->
    LengthSpinePairProblemCasePatternIsNotDirectConstructor occurrence
  LengthProblemCaseConstructorRepeated node name ->
    LengthSpinePairProblemCaseConstructorRepeated node name
  LengthProblemCaseFieldPatternUnsupported occurrence ->
    LengthSpinePairProblemCaseFieldPatternUnsupported occurrence
  LengthProblemGraphKindRejected reason ->
    LengthSpinePairProblemGraphKindRejected reason
  LengthProblemVisibleTypeSourceHasNoBinder node ->
    LengthSpinePairProblemVisibleTypeSourceHasNoBinder node
  LengthProblemVisibleTypeSelectionRejected node selected ->
    LengthSpinePairProblemVisibleTypeSelectionRejected node selected
  LengthProblemGlobalNotInSourceInventory node name ->
    LengthSpinePairProblemGlobalNotInSourceInventory node name
  LengthProblemGlobalHasNoLengthSummary node name ->
    LengthSpinePairProblemGlobalHasNoLengthSummary node name
  LengthProblemGlobalInstantiationRejected node name ->
    LengthSpinePairProblemGlobalInstantiationRejected node name
  LengthProblemSemanticLocalMissing node local ->
    LengthSpinePairProblemSemanticLocalMissing node local
  LengthProblemExpectedCallable node ->
    LengthSpinePairProblemExpectedCallable node
  LengthProblemExpectedSpine node -> LengthSpinePairProblemExpectedSpine node
  LengthProblemExpectedTuple occurrence ->
    LengthSpinePairProblemExpectedTuple occurrence
  LengthProblemUnobservedTargetArgumentDemanded position site ->
    LengthSpinePairProblemUnobservedTargetArgumentDemanded position site
  LengthProblemStepPayloadDemanded occurrence site ->
    LengthSpinePairProblemStepPayloadDemanded occurrence site
  LengthProblemTupleArityMismatch occurrence expected observed ->
    LengthSpinePairProblemTupleArityMismatch occurrence expected observed
  LengthProblemEvaluationStepLimitExceeded maximumSteps observed ->
    LengthSpinePairProblemEvaluationStepLimitExceeded maximumSteps observed
  LengthProblemProviderTransferInvariant name position ->
    LengthSpinePairProblemProviderTransferInvariant name position
  LengthProblemSyntaxRejected reason ->
    LengthSpinePairProblemSyntaxRejected reason
  LengthProblemFingerprintLimitExceeded part maximumBytes observed ->
    LengthSpinePairProblemFingerprintLimitExceeded
      (mapFingerprintPart part) maximumBytes observed
 where
  mapFingerprintPart part = case part of
    LengthConcreteEncodingFingerprint ->
      LengthSpinePairConcreteEncodingFingerprint
    LengthCandidateFingerprint -> LengthSpinePairCandidateFingerprint
    LengthCompleteProblemFingerprint -> LengthSpinePairCompleteProblemFingerprint
    LengthCounterexampleBankScopeFingerprint ->
      LengthSpinePairCounterexampleBankScopeFingerprint

matchRootOpening
  :: Ord identity
  => Type (Variable identity)
  -> Type (Variable identity)
  -> Either
      (LengthProblemError failure identity local)
      (Set identity)
matchRootOpening target actual
  | null implicitBinders && typesAlphaEqual target actual =
      Right $ rigidFreeVariables actual
  | otherwise = case matchContextFreeScheme openingTarget actual of
      Left _ -> rejected LengthRootOpeningShapeMismatch
      Right matched -> do
        _ <- foldM checkSelection (rigidFreeVariables target)
          $ zip [0 ..] $ contextFreeSchemeSelections matched
        Right $ rigidFreeVariables actual
 where
  implicitBinders = foldr retainFlexible []
    $ freeVariablesInFirstOccurrenceOrder target
  implicitTarget = case implicitBinders of
    [] -> target
    binders -> ForallType binders [] target
  openingTarget
    | null implicitBinders = target
    | otherwise = implicitTarget

  retainFlexible variable remaining = case variable of
    FlexibleVariable{} -> variable : remaining
    RigidVariable{} -> remaining

  rejected = Left . LengthProblemRootOpeningRejected

  checkSelection used (position, possibleSelection) = case possibleSelection of
    Nothing -> Right used
    Just selection -> case contextFreeSchemeSelectionVariable selection of
      Just (RigidVariable identity)
        | Set.member identity used -> rejected
            $ LengthRootOpeningRigidIsNotInjective position identity
        | otherwise -> Right $ Set.insert identity used
      _ -> rejected $ LengthRootOpeningSelectionIsNotRigid position

rigidFreeVariables
  :: Ord identity
  => Type (Variable identity)
  -> Set identity
rigidFreeVariables = Set.fromList . foldMap rigid . Set.toList . freeVariables
 where
  rigid (RigidVariable identity) = [identity]
  rigid FlexibleVariable{} = []

data ModeledGlobal identity
  = ModeledZero
  | ModeledStep !Int
  | ModeledProvider
      (CheckedLengthProviderSummary (Variable identity))
  | ModeledConditionalProviderBase
      (ConditionalProviderAuthorization identity)

-- | Canonical zero-before-step receipt produced only after the session-owned
-- constructor schema has freshly re-sealed and fingerprinted the raw graph.
data ExactSpineCase identity local = ExactSpineCase
  !TermNodeId
  !TermNodeId
  !(TypedPattern (Type (Variable identity)) local)
  !TermNodeId

data PreflightGraph identity local = PreflightGraph
  { preflightGlobals :: !(Map TermNodeId (ModeledGlobal identity))
  , preflightCases :: !(Map TermNodeId (ExactSpineCase identity local))
  , preflightConditionalProviderFinals ::
      !(Map TermNodeId (ConditionalProviderAuthorization identity))
  }

-- Consume graph retention only after contract resealing and residual
-- rejection.  The associated branch first spends the same fresh
-- session-selected graph reseal/fingerprint boundary as every other graph,
-- then authorizes every rooted semantic row against the exact session
-- inventory, and projects the bare graph only after both have succeeded.
retainLengthCandidateGraph
  :: (Ord identity, Ord local)
  => CheckedLengthSession identity annotation
  -> LengthProblemLimits
  -> TypedCandidate failure
      (Type (Variable identity))
      local
      (Candidate (Type (Variable identity)) details output)
  -> Either
      (LengthProblemError failure identity local)
      ( TermGraph (Type (Variable identity)) local
      , Fingerprint TermGraphFingerprintSubject
      , LengthCandidateAuthorization identity
      )
retainLengthCandidateGraph session limits = foldTypedCandidateGraph
  unavailable plain associated
 where
  unavailable _ failure = Left $ LengthProblemTypedGraphUnavailable failure

  plain _ graph = do
    fingerprint <- first LengthProblemTermGraphFingerprintRejected
      $ fingerprintLengthTermGraph session limits graph
    pure (graph, fingerprint
      , emptyCandidateAuthorization LengthPlainCandidateAuthority)

  associated _ checked = do
    fingerprint <- first LengthProblemTermGraphFingerprintRejected
      $ fingerprintLengthAssociatedTermGraph session limits checked
    authorization <- authorizeAssociatedCertificateGraph session checked
    let graph = checkedTypeApplicationCertificateGraph checked
    pure (graph, fingerprint, authorization)

-- Require each carrier row, in rooted structural order, to name the exact
-- session inventory scheme and checked provider law.  Legacy obligation-free
-- rows retain their v2 behavior.  Conditional rows are first classified as a
-- complete set, their protected chain edges are audited across every live or
-- dead graph node, and only then are their ground obligations discharged in
-- canonical row/step/obligation order.
authorizeAssociatedCertificateGraph
  :: Ord identity
  => CheckedLengthSession identity annotation
  -> CheckedTypeApplicationCertificateGraph (Variable identity) local
  -> Either
      (LengthProblemError failure identity local)
      (LengthCandidateAuthorization identity)
authorizeAssociatedCertificateGraph session checked = do
  AssociatedProviderClassification rowCount reversedConditionalRows <-
    foldCheckedTypeApplicationCertificateGraph classifyRow
      (Right $ AssociatedProviderClassification 0 []) checked
  let conditionalRows = reverse reversedConditionalRows
      graph = checkedTypeApplicationCertificateGraph checked
      chainAudit = buildConditionalProviderChainAudit conditionalRows
  auditConditionalProviderChains graph chainAudit
  (bases, finals) <- foldM dischargeConditionalRow
    (Map.empty, Map.empty) conditionalRows
  let authority
        | rowCount == 0 = LengthPlainCandidateAuthority
        | null conditionalRows =
            LengthOpaqueAssociatedCertificateAuthority
        | otherwise =
            LengthGroundDischargedAssociatedCertificateAuthority
  pure $ LengthCandidateAuthorization authority bases finals
 where
  inventory = lengthContextInventory $ checkedLengthSessionContext session
  providers = checkedLengthSessionProviderInventory session
  model = lengthContextSpineModel $ checkedLengthSessionContext session
  resolver = checkedLengthSessionClassResolutionEnvironment session

  classifyRow (Left failure) _ _ _ _ _ _ = Left failure
  classifyRow
      (Right (AssociatedProviderClassification rowOrdinal conditionalRows))
      _ owner scheme baseNode _ receipts = do
    if owner == checkedLengthSpineZeroConstructor model
        || owner == checkedLengthSpineStepConstructor model
      then Left $
        LengthProblemAssociatedCertificateModeledConstructorUnsupported
          owner rowOrdinal
      else pure ()
    sourceScheme <- case inventoryTermScheme inventory owner of
      Nothing -> Left $ LengthProblemAssociatedCertificateOwnerMissing
        owner rowOrdinal
      Just source -> Right source
    if typesAlphaEqual sourceScheme scheme
      then pure ()
      else Left $ LengthProblemAssociatedCertificateSourceSchemeMismatch
        owner rowOrdinal
    -- The session co-seals each checked summary from its normalized exact
    -- 'inventoryTermScheme'.  Once the row has matched that source scheme,
    -- summary-scheme equality is an opaque session invariant; only the
    -- provider-law presence remains a dynamic candidate authorization check.
    summary <- case lookupCheckedLengthProviderSummary owner providers of
      Nothing -> case firstActivatedObligations receipts of
        Just (stepOrdinal, count) -> Left $
          LengthProblemAssociatedCertificateActivatedObligations
            owner rowOrdinal stepOrdinal count
        Nothing -> Left $
          LengthProblemAssociatedCertificateProviderSummaryMissing
            owner rowOrdinal
      Just checkedSummary -> Right checkedSummary
    case checkedLengthProviderTrust summary of
      AssumedProviderLaw -> do
        case firstActivatedObligations receipts of
          Nothing -> pure ()
          Just (stepOrdinal, count) -> Left $
            LengthProblemAssociatedCertificateActivatedObligations
              owner rowOrdinal stepOrdinal count
        pure $ AssociatedProviderClassification
          (rowOrdinal + 1) conditionalRows
      AssumedProviderLawConditionalOnConstraintDischarge -> do
        case firstActivatedObligations receipts of
          Nothing -> Left $
            LengthProblemAssociatedCertificateConditionalObligationsMissing
              owner rowOrdinal
          Just _ -> pure ()
        let retainedReceipts =
              [(node, step) | (node, _, step) <- receipts]
            row = ConditionalAssociatedProviderRow owner rowOrdinal
              baseNode retainedReceipts summary
        pure $ AssociatedProviderClassification
          (rowOrdinal + 1) (row : conditionalRows)

  firstActivatedObligations = findStep 0
  findStep _ [] = Nothing
  findStep ordinal ((_, _, step) : remaining) =
    let count = checkedTypeApplicationCertificateStepObligationCount step
    in if count == 0
      then findStep (ordinal + 1) remaining
      else Just (ordinal, count)

  dischargeConditionalRow (bases, finals)
      (ConditionalAssociatedProviderRow owner rowOrdinal baseNode receipts
        summary) = do
    discharges <- dischargeSteps owner rowOrdinal 0 receipts
    finalNode <- case reverse receipts of
      (node, _) : _ -> Right node
      [] -> Left $
        LengthProblemAssociatedCertificateConditionalObligationsMissing
          owner rowOrdinal
    let authorization = ConditionalProviderAuthorization summary discharges
    pure
      ( Map.insert baseNode authorization bases
      , Map.insert finalNode authorization finals
      )

  dischargeSteps _ _ _ [] = Right []
  dischargeSteps owner rowOrdinal stepOrdinal ((_, step) : remaining) = do
    current <- dischargeObligations owner rowOrdinal stepOrdinal 0
      $ checkedTypeApplicationCertificateStepObligations step
    later <- dischargeSteps owner rowOrdinal (stepOrdinal + 1) remaining
    pure $ current ++ later

  dischargeObligations _ _ _ _ [] = Right []
  dischargeObligations owner rowOrdinal stepOrdinal obligationOrdinal
      (obligation : remaining) = do
    receipt <- dischargeObligation owner rowOrdinal stepOrdinal
      obligationOrdinal obligation
    later <- dischargeObligations owner rowOrdinal stepOrdinal
      (obligationOrdinal + 1) remaining
    pure $ receipt : later

  dischargeObligation owner rowOrdinal stepOrdinal obligationOrdinal
      obligation = case resolver of
    Nothing -> rejected LengthAssociatedClassResolverUnavailable
    Just environment -> case dischargeHeterogeneousGroundConstraint
        environment obligation of
      Left (HeterogeneousClassResolutionGroundQueryError failure) ->
        rejected $ sanitizeClassResolutionFailure failure
      Left (HeterogeneousClassResolutionProofSearchError failure) ->
        rejected $ sanitizeClassResolutionFailure failure
      Right Nothing -> rejected LengthAssociatedConstraintEvidenceMissing
      Right (Just receipt) -> Right receipt
   where
    rejected reason = Left $
      LengthProblemAssociatedCertificateConstraintDischargeRejected
        owner rowOrdinal stepOrdinal obligationOrdinal reason

buildConditionalProviderChainAudit
  :: [ConditionalAssociatedProviderRow identity]
  -> ConditionalProviderChainAudit
buildConditionalProviderChainAudit = List.foldl' addRow
  $ ConditionalProviderChainAudit Map.empty Map.empty []
 where
  addRow audit (ConditionalAssociatedProviderRow owner rowOrdinal baseNode
      receipts _) = case receipts of
    [] -> audit
    (firstNode, _) : _ ->
      List.foldl' addIntermediate
        (addProtected firstNode baseNode
          (ProtectedConditionalProviderNode owner rowOrdinal
            LengthAssociatedProviderBase) audit)
        $ zip3 [0 ..] receipts $ drop 1 receipts
   where
    addIntermediate current (stepOrdinal, (node, _), (parent, _)) =
      addProtected parent node
        (ProtectedConditionalProviderNode owner rowOrdinal
          $ LengthAssociatedProviderIntermediate stepOrdinal)
        current

  addProtected parent child site
      (ConditionalProviderChainAudit allowed protected reversedOrder) =
    let alreadyProtected = Map.member child protected
        updatedAllowed = Map.insertWith Set.union child
          (Set.singleton parent) allowed
        updatedProtected = Map.insertWith (\_ previous -> previous)
          child site protected
        updatedOrder = if alreadyProtected
          then reversedOrder else child : reversedOrder
    in ConditionalProviderChainAudit
        updatedAllowed updatedProtected updatedOrder

auditConditionalProviderChains
  :: TermGraph ty local
  -> ConditionalProviderChainAudit
  -> Either (LengthProblemError failure identity local) ()
auditConditionalProviderChains graph
    (ConditionalProviderChainAudit allowed protected reversedOrder) =
  mapM_ inspect $ reverse reversedOrder
 where
  incoming = List.foldl' collectIncoming Map.empty
    $ termGraphNodes graph

  collectIncoming current (parent, TermNode _ form) =
    List.foldl' (\edges child -> Map.insertWith Set.union child
      (Set.singleton parent) edges) current $ termNodeReferences form

  inspect node = case Map.lookup node protected of
    Nothing -> Right ()
    Just (ProtectedConditionalProviderNode owner rowOrdinal site)
      | node == termGraphRoot graph -> rejected owner rowOrdinal site
          LengthAssociatedProtectedNodeIsRoot
      | Map.findWithDefault Set.empty node incoming
          /= Map.findWithDefault Set.empty node allowed ->
          rejected owner rowOrdinal site
            LengthAssociatedProtectedNodeHasUnexpectedIncomingEdge
      | otherwise -> Right ()

  rejected owner rowOrdinal site reason = Left $
    LengthProblemAssociatedCertificateProtectedChainRejected
      owner rowOrdinal site reason

termNodeReferences :: TermNodeForm ty local -> [TermNodeId]
termNodeReferences form = case form of
  TypedLocal{} -> []
  TypedGlobal{} -> []
  TypedLambda _ body -> [body]
  TypedApply function argument _ -> [function, argument]
  TypedVisibleTypeApplication _ function _ _ -> [function]
  TypedTuple fields -> fields
  TypedHole{} -> []
  TypedLet _ binding body -> [binding, body]
  TypedCase scrutinee alternatives -> scrutinee : map snd alternatives

sanitizeClassResolutionFailure
  :: ClassResolutionQueryError variable
  -> LengthAssociatedConstraintDischargeReason
sanitizeClassResolutionFailure failure = case failure of
  InvalidClassResolutionGroundConstraint{} ->
    LengthAssociatedConstraintQueryRejected
  InvalidClassResolutionDerivedConstraint{} ->
    LengthAssociatedDerivedConstraintRejected
  ClassResolutionGroundConstraintHasFreeVariables{} ->
    LengthAssociatedConstraintNotGround
  ClassResolutionProofDepthLimitExceeded{} ->
    LengthAssociatedConstraintProofLimitExceeded
  ClassResolutionProofNodeLimitExceeded{} ->
    LengthAssociatedConstraintProofLimitExceeded

fingerprintLengthTermGraph
  :: (Ord identity, Ord local)
  => CheckedLengthSession identity annotation
  -> LengthProblemLimits
  -> TermGraph (Type (Variable identity)) local
  -> Either
      (TermGraphFingerprintError identity local)
      (Fingerprint TermGraphFingerprintSubject)
fingerprintLengthTermGraph session limits graph = case
    checkedLengthSessionCasePolicy session of
  LengthCasesRejected -> fingerprintSharedTermGraph graphLimits maximumBytes graph
  LengthExactZeroStepCases -> fingerprintTermGraphWithTypeStructure
    (lengthTermGraphTypeStructure model) graphLimits maximumBytes graph
 where
  graphLimits = lengthProblemTermGraphLimits limits
  maximumBytes = lengthProblemGraphFingerprintByteLimit limits
  model = lengthContextSpineModel $ checkedLengthSessionContext session

fingerprintLengthAssociatedTermGraph
  :: (Ord identity, Ord local)
  => CheckedLengthSession identity annotation
  -> LengthProblemLimits
  -> CheckedTypeApplicationCertificateGraph (Variable identity) local
  -> Either
      (TermGraphFingerprintError identity local)
      (Fingerprint TermGraphFingerprintSubject)
fingerprintLengthAssociatedTermGraph session limits =
  fingerprintCheckedTypeApplicationCertificateGraphWithTypeStructure
    typeStructure graphLimits maximumBytes
 where
  graphLimits = lengthProblemTermGraphLimits limits
  maximumBytes = lengthProblemGraphFingerprintByteLimit limits
  typeStructure = case checkedLengthSessionCasePolicy session of
    LengthCasesRejected -> sharedTypeStructure
    LengthExactZeroStepCases -> lengthTermGraphTypeStructure model
  model = lengthContextSpineModel $ checkedLengthSessionContext session

lengthTermGraphTypeStructure
  :: Ord identity
  => CheckedLengthSpineModel (Variable identity)
  -> TypeStructure (Type (Variable identity))
lengthTermGraphTypeStructure model = sharedTypeStructure
  { constructorPatternFieldTypes = fields
  }
 where
  fields name patternType
    | not $ isModeledSpine model patternType = Nothing
    | name == checkedLengthSpineZeroConstructor model = Just []
    | name == checkedLengthSpineStepConstructor model = do
        payload <- spinePayload model patternType
        case checkedLengthSpineRecursiveField model of
          0 -> Just [patternType, payload]
          1 -> Just [payload, patternType]
          _ -> Nothing
    | otherwise = Nothing

preflightGraph
  :: Ord identity
  => CheckedLengthSession identity annotation
  -> CheckedLengthProviderInventory (Variable identity)
  -> LengthCandidateAuthorization identity
  -> Set identity
  -> TermGraph (Type (Variable identity)) local
  -> Either
      (LengthProblemError failure identity local)
      (PreflightGraph identity local)
preflightGraph session providers candidateAuthorization authorized graph = do
  mapM_ rejectHole nodes
  cases <- foldM inspectCase Map.empty nodes
  selectedKindObligations <- fmap concat $ mapM selectedKindObligation nodes
  first LengthProblemGraphKindRejected
    $ checkTypesKinds (inventoryKindAssumptions inventory)
        $ [(ProperTypeKind, annotation)
          | annotation <- foldMap graphProperTypeAnnotations nodes]
            ++ selectedKindObligations
  mapM_ validateVisibleSelection nodes
  globals <- foldM inspectGlobal Map.empty nodes
  pure $ PreflightGraph globals cases
    $ candidateAuthorizationFinals candidateAuthorization
 where
  nodes = List.sortOn fst $ termGraphNodes graph
  context = checkedLengthSessionContext session
  inventory = lengthContextInventory context
  model = lengthContextSpineModel context

  rejectHole (nodeId, TermNode _ form) = case form of
    TypedHole _ local -> Left $ LengthProblemHole nodeId local
    _ -> Right ()

  casePolicy = checkedLengthSessionCasePolicy session

  inspectCase cases (nodeId, TermNode resultType form) = case form of
    TypedCase scrutinee alternatives -> case casePolicy of
      LengthCasesRejected -> Left $ LengthProblemUnsupportedCase nodeId
      LengthExactZeroStepCases -> do
        checked <- validateExactCase nodeId resultType scrutinee alternatives
        pure $ Map.insert nodeId checked cases
    TypedLambda patterns _ -> validatePatterns patterns >> pure cases
    TypedLet pattern _ _ -> validatePattern pattern >> pure cases
    _ -> pure cases

  validateVisibleSelection (nodeId, TermNode _ form) = case form of
    TypedVisibleTypeApplication _ _ _ witness
      | freeVariablesAuthorized authorized
          $ typeApplicationSelected witness -> Right ()
      | otherwise -> Left $ LengthProblemVisibleTypeSelectionRejected
          nodeId $ typeApplicationSelected witness
    _ -> Right ()

  selectedKindObligation (nodeId, TermNode _ form) = case form of
    TypedVisibleTypeApplication _ _ _ witness -> do
      source <- first
        (LengthProblemGraphKindRejected . InvalidKindInferenceType)
        $ normalizeType $ typeApplicationSource witness
      case source of
        ForallType (binder : remaining) constraints body -> do
          inferred <- first LengthProblemGraphKindRejected
            $ inferSharedVariableKinds
                (inventoryKindAssumptions inventory) [binder]
                [retainForall remaining constraints body]
          case lookup binder inferred of
            Just kind -> Right
              [(kind, typeApplicationSelected witness)]
            Nothing -> Left
              $ LengthProblemVisibleTypeSourceHasNoBinder nodeId
        _ -> Left $ LengthProblemVisibleTypeSourceHasNoBinder nodeId
    _ -> Right []

  retainForall [] [] body = body
  retainForall binders constraints body =
    ForallType binders constraints body

  inspectGlobal globals (nodeId, TermNode nodeType form) = case form of
    TypedGlobal _ name -> do
      semantic <- resolveGlobal inventory model providers
        (candidateAuthorizationBases candidateAuthorization) authorized
        nodeId name nodeType
      Right $ Map.insert nodeId semantic globals
    _ -> Right globals

  validatePatterns = mapM_ validatePattern

  validatePattern pattern = case typedPatternNode pattern of
    TypedConstructor name _ -> Left
      $ LengthProblemUnsupportedConstructorPattern
          (typedPatternOccurrence pattern) name
    TypedTuplePattern fields -> validatePatterns fields
    TypedAs _ nested -> validatePattern nested
    _ -> Right ()

  validateExactCase nodeId resultType scrutinee alternatives = do
    if isModeledSpine model resultType
      then pure ()
      else Left $ LengthProblemCaseResultIsNotModeledSpine nodeId resultType
    scrutineeType <- case lookupTermNode scrutinee graph of
      Nothing -> Left $ LengthProblemRootNodeMissing scrutinee
      Just (TermNode ty _) -> Right ty
    if isModeledSpine model scrutineeType
      then pure ()
      else Left $ LengthProblemCaseScrutineeIsNotModeledSpine
        nodeId scrutineeType
    case alternatives of
      [firstAlternative, secondAlternative] -> classifyAlternatives
        firstAlternative secondAlternative
      _ -> Left $ LengthProblemCaseAlternativeCountMismatch
        nodeId $ length alternatives
   where
    classifyAlternatives firstAlternative secondAlternative = do
      firstClassified <- classifyAlternative firstAlternative
      second <- classifyAlternative secondAlternative
      case (firstClassified, second) of
        (Left zero, Right step) -> assemble zero step
        (Right step, Left zero) -> assemble zero step
        (Left _, Left _) -> Left $ LengthProblemCaseConstructorRepeated
          nodeId $ checkedLengthSpineZeroConstructor model
        (Right _, Right _) -> Left $ LengthProblemCaseConstructorRepeated
          nodeId $ checkedLengthSpineStepConstructor model

    assemble (_, zeroBody) (stepPattern, stepBody) = pure
      $ ExactSpineCase scrutinee zeroBody stepPattern stepBody

    classifyAlternative alternative@(pattern, _) = case
        typedPatternNode pattern of
      TypedConstructor name fields
        | name == checkedLengthSpineZeroConstructor model -> do
            mapM_ validateCaseField fields
            pure $ Left alternative
        | name == checkedLengthSpineStepConstructor model -> do
            mapM_ validateCaseField fields
            pure $ Right alternative
        | otherwise -> Left $ LengthProblemUnsupportedConstructorPattern
            (typedPatternOccurrence pattern) name
      _ -> Left $ LengthProblemCasePatternIsNotDirectConstructor
        $ typedPatternOccurrence pattern

    validateCaseField field = case typedPatternNode field of
      TypedBind{} -> Right ()
      TypedWildcard -> Right ()
      _ -> Left $ LengthProblemCaseFieldPatternUnsupported
        $ typedPatternOccurrence field

graphProperTypeAnnotations
  :: (TermNodeId, TermNode (Type variable) local)
  -> [Type variable]
graphProperTypeAnnotations (_, TermNode nodeType form) =
  nodeType : case form of
    TypedLambda patterns _ -> foldMap patternTypeAnnotations patterns
    TypedApply _ _ witness ->
      [ applicationDomain witness
      , applicationResult witness
      ]
    TypedVisibleTypeApplication _ _ _ witness ->
      [ typeApplicationSource witness
      , typeApplicationResult witness
      ]
    TypedLet pattern _ _ -> patternTypeAnnotations pattern
    TypedCase _ branches -> foldMap
      (patternTypeAnnotations . fst) branches
    _ -> []

patternTypeAnnotations
  :: TypedPattern (Type variable) local
  -> [Type variable]
patternTypeAnnotations pattern =
  typedPatternType pattern : case typedPatternNode pattern of
      TypedConstructor _ fields -> foldMap patternTypeAnnotations fields
      TypedTuplePattern fields -> foldMap patternTypeAnnotations fields
      TypedAs _ nested -> patternTypeAnnotations nested
      _ -> []

resolveGlobal
  :: Ord identity
  => Inventory (Variable identity) annotation
  -> CheckedLengthSpineModel (Variable identity)
  -> CheckedLengthProviderInventory (Variable identity)
  -> Map TermNodeId (ConditionalProviderAuthorization identity)
  -> Set identity
  -> TermNodeId
  -> Name
  -> Type (Variable identity)
  -> Either
      (LengthProblemError failure identity local)
      (ModeledGlobal identity)
resolveGlobal inventory model providers conditionalBases authorized
    nodeId name actual
  | name == checkedLengthSpineZeroConstructor model =
      ModeledZero <$ validateConstructor False
  | name == checkedLengthSpineStepConstructor model =
      ModeledStep (checkedLengthSpineRecursiveField model)
        <$ validateConstructor True
  | Just provider <- lookupCheckedLengthProviderSummary name providers =
      case checkedLengthProviderTrust provider of
        AssumedProviderLawConditionalOnConstraintDischarge -> case
            Map.lookup nodeId conditionalBases of
          Just authorization@(ConditionalProviderAuthorization
              authorizedProvider _)
            | checkedLengthProviderName authorizedProvider == name ->
                Right $ ModeledConditionalProviderBase authorization
          _ -> Left
            $ LengthProblemConditionalProviderRequiresDischarge nodeId name
        AssumedProviderLaw ->
          if schemeAdmits authorized
              (checkedLengthProviderScheme provider) actual
            then Right $ ModeledProvider provider
            else rejectedInstantiation
  | otherwise = case inventoryTermScheme inventory name of
      Nothing -> Left $ LengthProblemGlobalNotInSourceInventory nodeId name
      Just _ -> Left $ LengthProblemGlobalHasNoLengthSummary nodeId name
 where
  rejectedInstantiation = Left
    $ LengthProblemGlobalInstantiationRejected nodeId name

  validateConstructor isStep = case checkedLengthSpineModelTrust model of
    BuiltinStructuralListSpine ->
      if builtinConstructorInstance authorized model isStep actual
        then Right ()
        else rejectedInstantiation
    DerivedFromListLikeDataDeclaration -> case inventoryTermScheme inventory name of
      Nothing -> Left $ LengthProblemGlobalNotInSourceInventory nodeId name
      Just source
        | schemeAdmits authorized source actual -> Right ()
        | otherwise -> rejectedInstantiation

schemeAdmits
  :: Ord identity
  => Set identity
  -> Type (Variable identity)
  -> Type (Variable identity)
  -> Bool
schemeAdmits authorized source actual
  | typesAlphaEqual source actual = True
  | otherwise = case matchContextFreeScheme source actual of
      Left _ -> False
      Right matched -> all (selectionAdmitted authorized)
        $ contextFreeSchemeSelections matched

selectionAdmitted
  :: Ord identity
  => Set identity
  -> Maybe (ContextFreeSchemeSelection (Variable identity))
  -> Bool
selectionAdmitted _ Nothing = True
selectionAdmitted authorized (Just selection) = all admitted
  $ Set.toList $ contextFreeSchemeSelectionFreeVariables selection
 where
  admitted (RigidVariable identity) = Set.member identity authorized
  admitted FlexibleVariable{} = False

builtinConstructorInstance
  :: Ord identity
  => Set identity
  -> CheckedLengthSpineModel (Variable identity)
  -> Bool
  -> Type (Variable identity)
  -> Bool
builtinConstructorInstance authorized model isStep rawActual =
  case normalizeType rawActual of
    Left _ -> False
    Right actual ->
      let (_, constraints, body) = splitLeadingForalls actual
          (arguments, result) = functionSpine body
      in null constraints
        && freeVariablesAuthorized authorized actual
        && if isStep
          then validStep arguments result
          else null arguments && isModeledSpine model result
 where
  validStep arguments result
    | length arguments /= 2 = False
    | recursiveIndex < 0 || recursiveIndex >= 2 = False
    | otherwise =
        let recursive = arguments !! recursiveIndex
            payload = arguments !! (1 - recursiveIndex)
        in typesAlphaEqual recursive result
          && case (spinePayload model recursive, spinePayload model result) of
            (Just recursivePayload, Just resultPayload) ->
              typesAlphaEqual recursivePayload resultPayload
                && typesAlphaEqual payload resultPayload
            _ -> False
   where
    recursiveIndex = checkedLengthSpineRecursiveField model

freeVariablesAuthorized
  :: Ord identity
  => Set identity
  -> Type (Variable identity)
  -> Bool
freeVariablesAuthorized authorized = all admitted . Set.toList . freeVariables
 where
  admitted (RigidVariable identity) = Set.member identity authorized
  admitted FlexibleVariable{} = False

spinePayload
  :: CheckedLengthSpineModel variable
  -> Type variable
  -> Maybe (Type variable)
spinePayload model source = case source of
  TypeApplication (TypeConstructor name) payload
    | name == checkedLengthSpineTypeName model -> Just payload
  _ -> Nothing

typesAlphaEqual :: Ord variable => Type variable -> Type variable -> Bool
typesAlphaEqual left right = case (normalizeType left, normalizeType right) of
  (Right normalizedLeft, Right normalizedRight) ->
    alphaNormalizeTypeWith PositionalBinderSlots normalizedLeft
      == alphaNormalizeTypeWith PositionalBinderSlots normalizedRight
  _ -> False

data SemanticEnvironment identity local = SemanticEnvironment
  !(Map local (SemanticThunk identity local))

data SemanticThunk identity local
  = EvaluatedThunk (SemanticValue identity local)
  | DeferredThunk !TermNodeId (SemanticEnvironment identity local)

data SemanticValue identity local
  = SemanticSpine !(LengthExpression LengthContractVariable)
  | SemanticOpaqueTargetArgument !Natural
  | SemanticOpaqueStepPayload !OccurrenceId
  | SemanticTuple [SemanticThunk identity local]
  | SemanticClosure
      [TypedPattern (Type (Variable identity)) local]
      !TermNodeId
      (SemanticEnvironment identity local)
  | SemanticProvider
      (CheckedLengthProviderSummary (Variable identity))
      [SemanticThunk identity local]
  | SemanticStep !Int [SemanticThunk identity local]

data EvaluationState identity = EvaluationState
  { evaluationSteps :: !Int
    -- Exact checked summaries reached by lazy evaluation, keyed canonically so
    -- the same authority supplies both the public name receipt and fingerprint.
  , evaluationUsedProviders ::
      !(Map Name (CheckedLengthProviderSummary (Variable identity)))
  }

emptyEvaluationState :: EvaluationState identity
emptyEvaluationState = EvaluationState 0 Map.empty

-- Build an independent, fully materialized ascending name list. A lazy
-- 'Map.keys' tail could otherwise keep the checked summaries reachable from a
-- candidate receipt which promises to expose names only.
materializeProviderNames
  :: Map Name (CheckedLengthProviderSummary variable)
  -> [Name]
materializeProviderNames = reverse . Map.foldlWithKey' retain []
 where
  retain names name _ = name : names

data InterpretationContext identity local annotation = InterpretationContext
  { interpretationSession :: !(CheckedLengthSession identity annotation)
  , interpretationProblemLimits :: !LengthProblemLimits
  , interpretationGraph :: !(TermGraph (Type (Variable identity)) local)
  , interpretationGlobals :: !(Map TermNodeId (ModeledGlobal identity))
  , interpretationCases :: !(Map TermNodeId (ExactSpineCase identity local))
  , interpretationConditionalProviderFinals ::
      !(Map TermNodeId (ConditionalProviderAuthorization identity))
  , interpretationTargetArgumentRoles :: ![LengthTargetArgumentRole]
  , interpretationInputCount :: !Int
  }

type Evaluation failure identity local = StateT (EvaluationState identity)
  (Either (LengthProblemError failure identity local))

interpretCompleteCandidate
  :: (Ord identity, Ord local)
  => InterpretationContext identity local annotation
  -> TermNodeId
  -> Evaluation failure identity local
      (LengthExpression LengthContractVariable)
interpretCompleteCandidate context root = do
  applied <- interpretAppliedCandidate context root
  requireSpine root applied

interpretAppliedCandidate
  :: (Ord identity, Ord local)
  => InterpretationContext identity local annotation
  -> TermNodeId
  -> Evaluation failure identity local (SemanticValue identity local)
interpretAppliedCandidate context root = do
  value <- evaluateNode context emptyEnvironment root
  (applied, observedCount) <- foldM (applyTargetArgument context root)
    (value, 0)
    $ zip [0 ..] $ interpretationTargetArgumentRoles context
  if observedCount == fromIntegral (interpretationInputCount context)
    then pure ()
    else lift $ Left $ LengthProblemTargetArgumentPolicyMismatch
  pure applied

data LengthSpinePairResultShapeError
  = LengthSpinePairExpectedResultTuple !TermNodeId
  | LengthSpinePairResultTupleArityMismatch !TermNodeId !Int
  | LengthSpinePairResultComponentExpectedSpine
      !LengthSpinePairComponent !TermNodeId
  | LengthSpinePairResultTupleDemandedUnobservedTarget
      !Natural !TermNodeId
  | LengthSpinePairResultTupleDemandedStepPayload
      !OccurrenceId !TermNodeId

interpretCompleteSpinePairCandidate
  :: (Ord identity, Ord local)
  => InterpretationContext identity local annotation
  -> TermNodeId
  -> Evaluation failure identity local
      (Either
        LengthSpinePairResultShapeError
        (LengthSpinePair (LengthExpression LengthContractVariable)))
interpretCompleteSpinePairCandidate context root = do
  applied <- interpretAppliedCandidate context root
  case applied of
    SemanticTuple fields -> case fields of
      [firstThunk, secondThunk] -> do
        firstOr <- forceSpinePairComponent
          LengthSpinePairFirst context root firstThunk
        case firstOr of
          Left failure -> pure $ Left failure
          Right firstResult -> do
            secondOr <- forceSpinePairComponent
              LengthSpinePairSecond context root secondThunk
            pure $ LengthSpinePair firstResult <$> secondOr
      _ -> pure $ Left $ LengthSpinePairResultTupleArityMismatch
        root $ length fields
    SemanticOpaqueTargetArgument position -> pure $ Left
      $ LengthSpinePairResultTupleDemandedUnobservedTarget position root
    SemanticOpaqueStepPayload occurrence -> pure $ Left
      $ LengthSpinePairResultTupleDemandedStepPayload occurrence root
    _ -> pure $ Left $ LengthSpinePairExpectedResultTuple root

forceSpinePairComponent
  :: (Ord identity, Ord local)
  => LengthSpinePairComponent
  -> InterpretationContext identity local annotation
  -> TermNodeId
  -> SemanticThunk identity local
  -> Evaluation failure identity local
      (Either
        LengthSpinePairResultShapeError
        (LengthExpression LengthContractVariable))
forceSpinePairComponent component context owner thunk = do
  value <- forceThunk context thunk
  case value of
    SemanticSpine expression -> pure $ Right expression
    SemanticOpaqueTargetArgument position -> lift $ Left
      $ LengthProblemUnobservedTargetArgumentDemanded position
      $ LengthUnobservedTargetSpineDemand owner
    SemanticOpaqueStepPayload occurrence -> lift $ Left
      $ LengthProblemStepPayloadDemanded occurrence
      $ LengthStepPayloadSpineDemand owner
    _ -> pure $ Left
      $ LengthSpinePairResultComponentExpectedSpine component owner

applyTargetArgument
  :: (Ord identity, Ord local)
  => InterpretationContext identity local annotation
  -> TermNodeId
  -> (SemanticValue identity local, Natural)
  -> (Natural, LengthTargetArgumentRole)
  -> Evaluation failure identity local
      (SemanticValue identity local, Natural)
applyTargetArgument context owner (function, observedPosition)
    (physicalPosition, role) = case role of
  LengthObservedSpine -> do
    value <- applySemantic context owner function
      $ EvaluatedThunk $ SemanticSpine
      $ LengthVariable $ LengthInput observedPosition
    pure (value, observedPosition + 1)
  LengthUnobservedTarget -> do
    value <- applySemantic context owner function
      $ EvaluatedThunk $ SemanticOpaqueTargetArgument physicalPosition
    pure (value, observedPosition)

emptyEnvironment :: SemanticEnvironment identity local
emptyEnvironment = SemanticEnvironment Map.empty

evaluateNode
  :: (Ord identity, Ord local)
  => InterpretationContext identity local annotation
  -> SemanticEnvironment identity local
  -> TermNodeId
  -> Evaluation failure identity local (SemanticValue identity local)
evaluateNode context environment nodeId = do
  spendEvaluation context
  node <- case lookupTermNode nodeId $ interpretationGraph context of
    Nothing -> lift $ Left $ LengthProblemRootNodeMissing nodeId
    Just value -> pure value
  case termNodeForm node of
    TypedLocal _ local -> case lookupSemanticLocal local environment of
      Nothing -> lift $ Left $ LengthProblemSemanticLocalMissing nodeId local
      Just thunk -> forceThunk context thunk
    TypedGlobal _ name -> case Map.lookup nodeId
        $ interpretationGlobals context of
      Just ModeledZero -> pure $ SemanticSpine $ LengthLiteral 0
      Just (ModeledStep recursiveIndex) ->
        pure $ SemanticStep recursiveIndex []
      Just (ModeledProvider provider)
        | null $ checkedLengthProviderArgumentRoles provider ->
            interpretProvider context nodeId provider []
        | otherwise -> pure $ SemanticProvider provider []
      Just ModeledConditionalProviderBase{} -> lift $ Left
        $ LengthProblemConditionalProviderRequiresDischarge nodeId name
      Nothing -> lift $ Left
        $ LengthProblemGlobalHasNoLengthSummary nodeId name
    TypedLambda [] body -> evaluateNode context environment body
    TypedLambda patterns body -> pure
      $ SemanticClosure patterns body environment
    TypedApply function argument _ -> do
      callable <- evaluateNode context environment function
      spendEvaluation context
      applySemantic context nodeId callable
        $ DeferredThunk argument environment
    TypedVisibleTypeApplication _ function _ _ -> case Map.lookup nodeId
        $ interpretationConditionalProviderFinals context of
      Just authorization -> interpretAuthorizedConditionalProvider
        context nodeId authorization
      Nothing -> evaluateNode context environment function
    TypedTuple fields -> pure $ SemanticTuple
      [DeferredThunk field environment | field <- fields]
    TypedHole _ local -> lift $ Left $ LengthProblemHole nodeId local
    TypedLet pattern binding body -> do
      extended <- bindPattern context pattern
        (DeferredThunk binding environment) environment
      evaluateNode context extended body
    TypedCase{} -> case Map.lookup nodeId $ interpretationCases context of
      Nothing -> lift $ Left $ LengthProblemUnsupportedCase nodeId
      Just checked -> interpretExactSpineCase context environment nodeId checked

interpretAuthorizedConditionalProvider
  :: (Ord identity, Ord local)
  => InterpretationContext identity local annotation
  -> TermNodeId
  -> ConditionalProviderAuthorization identity
  -> Evaluation failure identity local (SemanticValue identity local)
interpretAuthorizedConditionalProvider context owner
    (ConditionalProviderAuthorization provider _) =
  if null $ checkedLengthProviderArgumentRoles provider
    then interpretProvider context owner provider []
    else pure $ SemanticProvider provider []

interpretExactSpineCase
  :: (Ord identity, Ord local)
  => InterpretationContext identity local annotation
  -> SemanticEnvironment identity local
  -> TermNodeId
  -> ExactSpineCase identity local
  -> Evaluation failure identity local (SemanticValue identity local)
interpretExactSpineCase context environment owner
    (ExactSpineCase scrutinee zeroBody stepPattern stepBody) = do
  scrutineeValue <- evaluateNode context environment scrutinee
  scrutineeLength <- requireSpine owner scrutineeValue
  spendEvaluation context
  zeroValue <- evaluateNode context environment zeroBody
  zeroLength <- requireSpine owner zeroValue
  let model = lengthContextSpineModel $ checkedLengthSessionContext
        $ interpretationSession context
      recursiveIndex = checkedLengthSpineRecursiveField model
      tailLength = LengthMonus scrutineeLength $ LengthLiteral 1
      stepFields = case typedPatternNode stepPattern of
        TypedConstructor _ fields -> fields
        _ -> []
      fieldThunk index field
        | index == recursiveIndex = EvaluatedThunk $ SemanticSpine tailLength
        | otherwise = EvaluatedThunk $ SemanticOpaqueStepPayload
            $ typedPatternOccurrence field
  spendEvaluation context
  stepEnvironment <- foldM bindStepField environment
    $ zip stepFields $ zipWith fieldThunk [0 ..] stepFields
  stepValue <- evaluateNode context stepEnvironment stepBody
  stepLength <- requireSpine owner stepValue
  pure $ SemanticSpine $ LengthIf
    (LengthEqual scrutineeLength $ LengthLiteral 0)
    zeroLength stepLength
 where
  bindStepField current (field, thunk) =
    bindPattern context field thunk current

forceThunk
  :: (Ord identity, Ord local)
  => InterpretationContext identity local annotation
  -> SemanticThunk identity local
  -> Evaluation failure identity local (SemanticValue identity local)
forceThunk _ (EvaluatedThunk value) = pure value
forceThunk context (DeferredThunk node environment) =
  evaluateNode context environment node

applySemantic
  :: (Ord identity, Ord local)
  => InterpretationContext identity local annotation
  -> TermNodeId
  -> SemanticValue identity local
  -> SemanticThunk identity local
  -> Evaluation failure identity local (SemanticValue identity local)
applySemantic context owner function argument = case function of
  SemanticOpaqueTargetArgument position -> lift $ Left
    $ LengthProblemUnobservedTargetArgumentDemanded position
    $ LengthUnobservedTargetCallableDemand owner
  SemanticOpaqueStepPayload occurrence -> lift $ Left
    $ LengthProblemStepPayloadDemanded occurrence
    $ LengthStepPayloadCallableDemand owner
  SemanticClosure [] body environment -> do
    value <- evaluateNode context environment body
    applySemantic context owner value argument
  SemanticClosure (pattern : remaining) body environment -> do
    extended <- bindPattern context pattern argument environment
    case remaining of
      [] -> evaluateNode context extended body
      _ -> pure $ SemanticClosure remaining body extended
  SemanticProvider provider arguments ->
    let updated = arguments ++ [argument]
        expected = length $ checkedLengthProviderArgumentRoles provider
    in if length updated < expected
      then pure $ SemanticProvider provider updated
      else if length updated == expected
        then interpretProvider context owner provider updated
        else lift $ Left $ LengthProblemExpectedCallable owner
  SemanticStep recursiveIndex arguments ->
    let updated = arguments ++ [argument]
    in if length updated < 2
      then pure $ SemanticStep recursiveIndex updated
      else if length updated == 2
        then do
          recursive <- forceSpine context owner $ updated !! recursiveIndex
          pure $ SemanticSpine $ LengthSum [LengthLiteral 1, recursive]
        else lift $ Left $ LengthProblemExpectedCallable owner
  _ -> lift $ Left $ LengthProblemExpectedCallable owner

bindPattern
  :: (Ord identity, Ord local)
  => InterpretationContext identity local annotation
  -> TypedPattern (Type (Variable identity)) local
  -> SemanticThunk identity local
  -> SemanticEnvironment identity local
  -> Evaluation failure identity local (SemanticEnvironment identity local)
bindPattern context pattern thunk environment = do
  spendEvaluation context
  case typedPatternNode pattern of
    TypedBind local -> pure $ insertSemanticLocal local thunk environment
    TypedWildcard -> pure environment
    TypedAs local nested -> bindPattern context nested thunk
      $ insertSemanticLocal local thunk environment
    TypedTuplePattern patterns -> do
      value <- forceThunk context thunk
      case value of
        SemanticOpaqueTargetArgument position -> lift $ Left
          $ LengthProblemUnobservedTargetArgumentDemanded position
          $ LengthUnobservedTargetTupleDemand
          $ typedPatternOccurrence pattern
        SemanticOpaqueStepPayload occurrence -> lift $ Left
          $ LengthProblemStepPayloadDemanded occurrence
          $ LengthStepPayloadTupleDemand $ typedPatternOccurrence pattern
        SemanticTuple fields
          | length fields == length patterns -> foldM bindField environment
              $ zip patterns fields
          | otherwise -> lift $ Left $ LengthProblemTupleArityMismatch
              (typedPatternOccurrence pattern)
              (length patterns) (length fields)
        _ -> lift $ Left
          $ LengthProblemExpectedTuple $ typedPatternOccurrence pattern
    TypedConstructor name _ -> lift $ Left
      $ LengthProblemUnsupportedConstructorPattern
          (typedPatternOccurrence pattern) name
 where
  bindField current (fieldPattern, fieldThunk) =
    bindPattern context fieldPattern fieldThunk current

lookupSemanticLocal
  :: Ord local
  => local
  -> SemanticEnvironment identity local
  -> Maybe (SemanticThunk identity local)
lookupSemanticLocal local (SemanticEnvironment environment) =
  Map.lookup local environment

insertSemanticLocal
  :: Ord local
  => local
  -> SemanticThunk identity local
  -> SemanticEnvironment identity local
  -> SemanticEnvironment identity local
insertSemanticLocal local thunk (SemanticEnvironment environment) =
  SemanticEnvironment $ Map.insert local thunk environment

forceSpine
  :: (Ord identity, Ord local)
  => InterpretationContext identity local annotation
  -> TermNodeId
  -> SemanticThunk identity local
  -> Evaluation failure identity local
      (LengthExpression LengthContractVariable)
forceSpine context owner thunk = do
  value <- forceThunk context thunk
  requireSpine owner value

requireSpine
  :: TermNodeId
  -> SemanticValue identity local
  -> Evaluation failure identity local
      (LengthExpression LengthContractVariable)
requireSpine _ (SemanticSpine expression) = pure expression
requireSpine owner (SemanticOpaqueTargetArgument position) = lift $ Left
  $ LengthProblemUnobservedTargetArgumentDemanded position
  $ LengthUnobservedTargetSpineDemand owner
requireSpine owner (SemanticOpaqueStepPayload occurrence) = lift $ Left
  $ LengthProblemStepPayloadDemanded occurrence
  $ LengthStepPayloadSpineDemand owner
requireSpine owner _ = lift $ Left $ LengthProblemExpectedSpine owner

interpretProvider
  :: (Ord identity, Ord local)
  => InterpretationContext identity local annotation
  -> TermNodeId
  -> CheckedLengthProviderSummary (Variable identity)
  -> [SemanticThunk identity local]
  -> Evaluation failure identity local (SemanticValue identity local)
interpretProvider context owner provider arguments = do
  replacements <- foldM observe Map.empty
    $ zip3 [0 ..] (checkedLengthProviderArgumentRoles provider) arguments
  raw <- lift $ first (LengthProblemProviderTransferInvariant name)
    $ substituteProviderExpression replacements
        $ checkedLengthProviderTransfer provider
  normalized <- lift $ first LengthProblemSyntaxRejected
    $ fmap fst $ normalizeLengthExpression
        (checkedLengthSessionLimits $ interpretationSession context)
        (validateCandidateVariable $ interpretationInputCount context)
        emptySyntaxUsage raw
  state <- get
  put state
    { evaluationUsedProviders = Map.insert name provider
        $ evaluationUsedProviders state
    }
  pure $ SemanticSpine normalized
 where
  name = checkedLengthProviderName provider

  observe replacements (position, role, argument) = case role of
    LengthUnobservedArgument -> pure replacements
    LengthSpineArgument -> do
      expression <- forceSpine context owner argument
      pure $ Map.insert position expression replacements

substituteProviderExpression
  :: Map Natural (LengthExpression LengthContractVariable)
  -> LengthExpression LengthProviderVariable
  -> Either Natural (LengthExpression LengthContractVariable)
substituteProviderExpression replacements = go
 where
  -- Return a replacement tree verbatim. Repeated references therefore share
  -- it instead of copying it before the immediately following bounded
  -- normalization walk.
  go source = case source of
    LengthVariable (LengthProviderArgument position) -> case
        Map.lookup position replacements of
      Nothing -> Left position
      Just expression -> Right expression
    LengthLiteral value -> Right $ LengthLiteral value
    LengthSum terms -> LengthSum <$> mapM go terms
    LengthScale factor expression -> LengthScale factor <$> go expression
    LengthQuotient divisor expression ->
      LengthQuotient divisor <$> go expression
    LengthModulo divisor expression -> LengthModulo divisor <$> go expression
    LengthMonus left right -> LengthMonus <$> go left <*> go right
    LengthMinimum left right -> LengthMinimum <$> go left <*> go right
    LengthMaximum left right -> LengthMaximum <$> go left <*> go right
    LengthIf condition trueBranch falseBranch -> LengthIf
      <$> goFormula condition <*> go trueBranch <*> go falseBranch

  goFormula source = case source of
    LengthTruth value -> Right $ LengthTruth value
    LengthEqual left right -> LengthEqual <$> go left <*> go right
    LengthAtMost left right -> LengthAtMost <$> go left <*> go right
    LengthNot formula -> LengthNot <$> goFormula formula
    LengthAll formulas -> LengthAll <$> mapM goFormula formulas

spendEvaluation
  :: InterpretationContext identity local annotation
  -> Evaluation failure identity local ()
spendEvaluation context = do
  state <- get
  let maximumSteps = lengthProblemEvaluationStepLimit
        $ interpretationProblemLimits context
      observed = evaluationSteps state
  if observed >= maximumSteps
    then lift $ Left $ LengthProblemEvaluationStepLimitExceeded
      maximumSteps $ saturatedSuccessor maximumSteps
    else put state {evaluationSteps = observed + 1}

validateCandidateVariable
  :: Int
  -> LengthContractVariable
  -> Either LengthSyntaxError ()
validateCandidateVariable inputCount variable = case variable of
  LengthInput position
    | position < fromIntegral inputCount -> Right ()
    | otherwise -> Left
        $ LengthInputReferenceOutOfRange position inputCount
  LengthResult -> Left LengthResultNotAvailableInPrecondition

substituteResultFormula
  :: LengthExpression LengthContractVariable
  -> LengthFormula LengthContractVariable
  -> LengthFormula LengthContractVariable
substituteResultFormula result = goFormula
 where
  -- As with provider substitution, repeated result references share the
  -- normalized tree until the joint-budget normalization walk consumes them.
  goExpression source = case source of
    LengthVariable LengthResult -> result
    LengthVariable variable -> LengthVariable variable
    LengthLiteral value -> LengthLiteral value
    LengthSum terms -> LengthSum $ map goExpression terms
    LengthScale factor expression -> LengthScale factor $ goExpression expression
    LengthQuotient divisor expression ->
      LengthQuotient divisor $ goExpression expression
    LengthModulo divisor expression ->
      LengthModulo divisor $ goExpression expression
    LengthMonus left right -> LengthMonus
      (goExpression left) (goExpression right)
    LengthMinimum left right -> LengthMinimum
      (goExpression left) (goExpression right)
    LengthMaximum left right -> LengthMaximum
      (goExpression left) (goExpression right)
    LengthIf condition trueBranch falseBranch -> LengthIf
      (goFormula condition) (goExpression trueBranch) (goExpression falseBranch)

  goFormula source = case source of
    LengthTruth value -> LengthTruth value
    LengthEqual left right -> LengthEqual
      (goExpression left) (goExpression right)
    LengthAtMost left right -> LengthAtMost
      (goExpression left) (goExpression right)
    LengthNot formula -> LengthNot $ goFormula formula
    LengthAll formulas -> LengthAll $ map goFormula formulas

substituteSpinePairResultFormula
  :: LengthSpinePair (LengthExpression LengthContractVariable)
  -> LengthFormula LengthSpinePairContractVariable
  -> LengthFormula LengthContractVariable
substituteSpinePairResultFormula result = goFormula
 where
  goExpression source = case source of
    LengthVariable variable -> case variable of
      LengthSpinePairInput position -> LengthVariable $ LengthInput position
      LengthSpinePairResult component -> case component of
        LengthSpinePairFirst -> lengthSpinePairFirst result
        LengthSpinePairSecond -> lengthSpinePairSecond result
    LengthLiteral value -> LengthLiteral value
    LengthSum terms -> LengthSum $ map goExpression terms
    LengthScale factor expression -> LengthScale factor $ goExpression expression
    LengthQuotient divisor expression ->
      LengthQuotient divisor $ goExpression expression
    LengthModulo divisor expression ->
      LengthModulo divisor $ goExpression expression
    LengthMonus left right -> LengthMonus
      (goExpression left) (goExpression right)
    LengthMinimum left right -> LengthMinimum
      (goExpression left) (goExpression right)
    LengthMaximum left right -> LengthMaximum
      (goExpression left) (goExpression right)
    LengthIf condition trueBranch falseBranch -> LengthIf
      (goFormula condition) (goExpression trueBranch) (goExpression falseBranch)

  goFormula source = case source of
    LengthTruth value -> LengthTruth value
    LengthEqual left right -> LengthEqual
      (goExpression left) (goExpression right)
    LengthAtMost left right -> LengthAtMost
      (goExpression left) (goExpression right)
    LengthNot formula -> LengthNot $ goFormula formula
    LengthAll formulas -> LengthAll $ map goFormula formulas

mapFingerprintFailure
  :: LengthProblemFingerprintPart
  -> Either FingerprintLimitError value
  -> Either (LengthProblemError failure identity local) value
mapFingerprintFailure part = either reject Right
 where
  reject FingerprintLimitExceeded
      { fingerprintMaximumBytes = maximumBytes
      , fingerprintObservedBytesAtLeast = observedBytes
      } = Left $ LengthProblemFingerprintLimitExceeded
        part maximumBytes observedBytes

mapSpinePairFingerprintFailure
  :: LengthSpinePairProblemFingerprintPart
  -> Either FingerprintLimitError value
  -> Either (LengthSpinePairProblemError failure identity local) value
mapSpinePairFingerprintFailure part = either reject Right
 where
  reject FingerprintLimitExceeded
      { fingerprintMaximumBytes = maximumBytes
      , fingerprintObservedBytesAtLeast = observedBytes
      } = Left $ LengthSpinePairProblemFingerprintLimitExceeded
        part maximumBytes observedBytes

buildConcreteEncodingFingerprint
  :: CheckedLengthSession identity annotation
  -> CheckedLengthContract (Variable identity)
  -> [CheckedLengthProviderSummary (Variable identity)]
  -> LengthExpression LengthContractVariable
  -> LengthFormula LengthContractVariable
  -> Either FingerprintLimitError
      (Fingerprint
        (EncodingFingerprintSubject FiniteListSpineLengthV1))
buildConcreteEncodingFingerprint session contract usedProviders result condition =
  buildFingerprintWithin maximumBytes FingerprintBuilder
    { fingerprintBuilderVersion =
        (if conditionalCapable then 3 else 0) + case casePolicy of
          LengthExactZeroStepCases -> 3
          LengthCasesRejected -> if mixedRoles then 2 else 1
    , fingerprintBuilderRole = ascii
        "finite-list-spine-length/concrete-encoding"
    , fingerprintBuilderFields =
        [ tagged "dialect"
            [FingerprintBytes finiteListSpineLengthDomainTag]
        , tagged "session-policy"
            [ FingerprintBytes $ fingerprintCanonicalBytes
                $ lengthSessionEncodingPolicyFingerprint session
            ]
        , tagged "contract"
            [ FingerprintBytes $ fingerprintCanonicalBytes
                $ lengthContractFingerprint contract
            ]
        , tagged "interpreter"
            $ [ FingerprintBytes $ ascii "lazy-symbolic-interpreter/v1"
            , FingerprintBytes $ ascii "finite-total-spine/v1"
            , FingerprintBytes $ ascii "assumed-provider-laws/v1"
            ] ++ conditionalInterpreterPolicy
              ++ mixedInterpreterPolicy ++ caseInterpreterPolicy
        , tagged "used-provider-laws"
            [FingerprintSequence $ map providerSummaryField usedProviders]
        , tagged "candidate-result"
            [lengthExpressionField contractVariableField result]
        , tagged "counterexample-condition"
            [lengthFormulaField contractVariableField condition]
        ]
    }
 where
  conditionalCapable = any ((==
      AssumedProviderLawConditionalOnConstraintDischarge) .
        checkedLengthProviderTrust)
    $ checkedLengthProviderSummaries
    $ checkedLengthSessionProviderInventory session
  mixedRoles = LengthUnobservedTarget `elem`
    checkedLengthContractTargetArgumentRoles contract
  casePolicy = checkedLengthSessionCasePolicy session
  conditionalInterpreterPolicy
    | conditionalCapable =
        [FingerprintBytes $ ascii
          "constraint-conditional-provider-after-ground-discharge/v1"]
    | otherwise = []
  mixedInterpreterPolicy
    | mixedRoles =
        [ FingerprintBytes $ ascii "source-ordered-target-roles/v1"
        , FingerprintBytes $ ascii "opaque-unobserved-target/v1"
        , FingerprintBytes $ ascii "compact-observed-input-numbering/v1"
        , FingerprintBytes $ ascii "explicit-opaque-demand-rejection/v1"
        ]
    | otherwise = []
  caseInterpreterPolicy = case casePolicy of
    LengthCasesRejected -> []
    LengthExactZeroStepCases ->
      [ FingerprintBytes $ ascii "exact-zero-step-spine-case/v1"
      , FingerprintBytes $ ascii "symbolic-zero-test/v1"
      , FingerprintBytes $ ascii "recursive-tail-natural-monus-one/v1"
      , FingerprintBytes $ ascii "opaque-step-payload/v1"
      , FingerprintBytes $ ascii "whole-case-provider-law-union/v1"
      ]
  maximumBytes = fromIntegral $ lengthFingerprintByteLimit
    $ checkedLengthSessionLimits session

buildSpinePairConcreteEncodingFingerprint
  :: CheckedLengthSession identity annotation
  -> CheckedLengthSpinePairContract (Variable identity)
  -> [CheckedLengthProviderSummary (Variable identity)]
  -> LengthSpinePair (LengthExpression LengthContractVariable)
  -> LengthFormula LengthContractVariable
  -> Either FingerprintLimitError
      (Fingerprint
        (EncodingFingerprintSubject FiniteBinaryProductSpineLengthsV1))
buildSpinePairConcreteEncodingFingerprint session contract usedProviders
    result condition =
  buildFingerprintWithin maximumBytes FingerprintBuilder
    { fingerprintBuilderVersion =
        (if conditionalCapable then 3 else 0) + case casePolicy of
          LengthExactZeroStepCases -> 3
          LengthCasesRejected -> if mixedRoles then 2 else 1
    , fingerprintBuilderRole = ascii
        "finite-binary-product-spine-lengths/concrete-encoding"
    , fingerprintBuilderFields =
        [ tagged "dialect"
            [FingerprintBytes finiteBinaryProductSpineLengthsDomainTag]
        , tagged "shared-session-policy"
            [ FingerprintBytes $ fingerprintCanonicalBytes
                $ lengthSessionEncodingPolicyFingerprint session
            ]
        , tagged "contract"
            [ FingerprintBytes $ fingerprintCanonicalBytes
                $ lengthSpinePairContractFingerprint contract
            ]
        , tagged "interpreter" $
            [ FingerprintBytes $ ascii "lazy-symbolic-interpreter/v1"
            , FingerprintBytes $ ascii "finite-total-spine/v1"
            , FingerprintBytes $ ascii "assumed-provider-laws/v1"
            , FingerprintBytes $ ascii "boxed-binary-spine-pair-root/v1"
            , FingerprintBytes $ ascii
                "source-ordered-left-then-right-extraction/v1"
            , FingerprintBytes $ ascii
                "scalar-provider-law-union-across-pair-components/v1"
            , FingerprintBytes $ ascii
                "pair-result-substitution-before-smt/v1"
            ] ++ conditionalInterpreterPolicy ++ mixedInterpreterPolicy ++
              caseInterpreterPolicy
        , tagged "used-provider-laws"
            [FingerprintSequence $ map providerSummaryField usedProviders]
        , tagged "candidate-result"
            [ tagged "first"
                [ lengthExpressionField contractVariableField
                    $ lengthSpinePairFirst result
                ]
            , tagged "second"
                [ lengthExpressionField contractVariableField
                    $ lengthSpinePairSecond result
                ]
            ]
        , tagged "counterexample-condition"
            [lengthFormulaField contractVariableField condition]
        ]
    }
 where
  conditionalCapable = any ((==
      AssumedProviderLawConditionalOnConstraintDischarge) .
        checkedLengthProviderTrust)
    $ checkedLengthProviderSummaries
    $ checkedLengthSessionProviderInventory session
  mixedRoles = LengthUnobservedTarget `elem`
    checkedLengthSpinePairContractTargetArgumentRoles contract
  casePolicy = checkedLengthSessionCasePolicy session
  conditionalInterpreterPolicy
    | conditionalCapable =
        [FingerprintBytes $ ascii
          "constraint-conditional-provider-after-ground-discharge/v1"]
    | otherwise = []
  mixedInterpreterPolicy
    | mixedRoles =
        [ FingerprintBytes $ ascii "source-ordered-target-roles/v1"
        , FingerprintBytes $ ascii "opaque-unobserved-target/v1"
        , FingerprintBytes $ ascii "compact-observed-input-numbering/v1"
        , FingerprintBytes $ ascii "explicit-opaque-demand-rejection/v1"
        ]
    | otherwise = []
  caseInterpreterPolicy = case casePolicy of
    LengthCasesRejected -> []
    LengthExactZeroStepCases ->
      [ FingerprintBytes $ ascii "exact-zero-step-spine-case/v1"
      , FingerprintBytes $ ascii "symbolic-zero-test/v1"
      , FingerprintBytes $ ascii "recursive-tail-natural-monus-one/v1"
      , FingerprintBytes $ ascii "opaque-step-payload/v1"
      , FingerprintBytes $ ascii "whole-case-provider-law-union/v1"
      , FingerprintBytes $ ascii "product-valued-case-results-rejected/v1"
      ]
  maximumBytes = fromIntegral $ lengthFingerprintByteLimit
    $ checkedLengthSessionLimits session

buildCandidateFingerprint
  :: CheckedLengthSession identity annotation
  -> LengthCandidateAuthority
  -> Fingerprint TermGraphFingerprintSubject
  -> Either FingerprintLimitError
      (Fingerprint
        (CandidateFingerprintSubject FiniteListSpineLengthV1))
buildCandidateFingerprint session authority graph =
  buildFingerprintWithin maximumBytes FingerprintBuilder
    { fingerprintBuilderVersion = case authority of
        LengthPlainCandidateAuthority -> 1
        LengthOpaqueAssociatedCertificateAuthority -> 2
        LengthGroundDischargedAssociatedCertificateAuthority -> 3
    , fingerprintBuilderRole = ascii
        "finite-list-spine-length/typed-candidate"
    , fingerprintBuilderFields =
        [ tagged "dialect"
            [FingerprintBytes finiteListSpineLengthDomainTag]
        , tagged "shared-typed-term-graph"
            [FingerprintBytes $ fingerprintCanonicalBytes graph]
        , tagged "candidate-authority" $
            [ FingerprintBytes $ ascii "engine-owned-association/v1"
            , FingerprintBytes $ ascii "empty-residual-constraints/v1"
            , FingerprintBytes $ ascii "candidate-only-no-batch-status/v1"
            ] ++ associatedAuthority
        ]
    }
 where
  associatedAuthority = case authority of
    LengthPlainCandidateAuthority -> []
    LengthOpaqueAssociatedCertificateAuthority ->
      [ FingerprintBytes $ ascii "opaque-associated-certificate/v1"
      , FingerprintBytes $ ascii "activated-obligations-empty/v1"
      ]
    LengthGroundDischargedAssociatedCertificateAuthority ->
      [ FingerprintBytes $ ascii "opaque-associated-certificate/v1"
      , FingerprintBytes $ ascii "independent-ground-class-discharge/v1"
      , FingerprintBytes $ ascii "inventory-bound-discharge-receipts/v1"
      , FingerprintBytes $ ascii
          "provider-law-uniform-over-dictionary-evidence/v1"
      , FingerprintBytes $ ascii "occurrence-specific-final-provider/v1"
      , FingerprintBytes $ ascii "protected-certified-function-prefix/v1"
      , FingerprintBytes $ ascii
          "static-discharge-without-givens-or-z3/v1"
      ]
  maximumBytes = fromIntegral $ lengthFingerprintByteLimit
    $ checkedLengthSessionLimits session

buildSpinePairCandidateFingerprint
  :: CheckedLengthSession identity annotation
  -> LengthCandidateAuthority
  -> Fingerprint TermGraphFingerprintSubject
  -> Either FingerprintLimitError
      (Fingerprint
        (CandidateFingerprintSubject FiniteBinaryProductSpineLengthsV1))
buildSpinePairCandidateFingerprint session authority graph =
  buildFingerprintWithin maximumBytes FingerprintBuilder
    { fingerprintBuilderVersion = case authority of
        LengthPlainCandidateAuthority -> 1
        LengthOpaqueAssociatedCertificateAuthority -> 2
        LengthGroundDischargedAssociatedCertificateAuthority -> 3
    , fingerprintBuilderRole = ascii
        "finite-binary-product-spine-lengths/typed-candidate"
    , fingerprintBuilderFields =
        [ tagged "dialect"
            [FingerprintBytes finiteBinaryProductSpineLengthsDomainTag]
        , tagged "shared-typed-term-graph"
            [FingerprintBytes $ fingerprintCanonicalBytes graph]
        , tagged "candidate-authority" $
            [ FingerprintBytes $ ascii "engine-owned-association/v1"
            , FingerprintBytes $ ascii "empty-residual-constraints/v1"
            , FingerprintBytes $ ascii "candidate-only-no-batch-status/v1"
            ] ++ associatedAuthority
        ]
    }
 where
  associatedAuthority = case authority of
    LengthPlainCandidateAuthority -> []
    LengthOpaqueAssociatedCertificateAuthority ->
      [ FingerprintBytes $ ascii "opaque-associated-certificate/v1"
      , FingerprintBytes $ ascii "activated-obligations-empty/v1"
      ]
    LengthGroundDischargedAssociatedCertificateAuthority ->
      [ FingerprintBytes $ ascii "opaque-associated-certificate/v1"
      , FingerprintBytes $ ascii "independent-ground-class-discharge/v1"
      , FingerprintBytes $ ascii "inventory-bound-discharge-receipts/v1"
      , FingerprintBytes $ ascii
          "provider-law-uniform-over-dictionary-evidence/v1"
      , FingerprintBytes $ ascii "occurrence-specific-final-provider/v1"
      , FingerprintBytes $ ascii "protected-certified-function-prefix/v1"
      , FingerprintBytes $ ascii
          "static-discharge-without-givens-or-z3/v1"
      ]
  maximumBytes = fromIntegral $ lengthFingerprintByteLimit
    $ checkedLengthSessionLimits session

buildCompleteProblemFingerprint
  :: CheckedLengthSession identity annotation
  -> Fingerprint
      (EncodingFingerprintSubject FiniteListSpineLengthV1)
  -> Fingerprint
      (CandidateFingerprintSubject FiniteListSpineLengthV1)
  -> Either FingerprintLimitError
      (Fingerprint
        (ProblemFingerprintSubject FiniteListSpineLengthV1))
buildCompleteProblemFingerprint session encoding candidate =
  buildFingerprintWithin maximumBytes FingerprintBuilder
    { fingerprintBuilderVersion = 1
    , fingerprintBuilderRole = ascii
        "finite-list-spine-length/behavioral-problem"
    , fingerprintBuilderFields =
        [ tagged "dialect"
            [FingerprintBytes finiteListSpineLengthDomainTag]
        , tagged "inventory"
            [ FingerprintBytes $ fingerprintCanonicalBytes
                $ lengthSessionInventoryFingerprint session
            ]
        , tagged "encoding"
            [FingerprintBytes $ fingerprintCanonicalBytes encoding]
        , tagged "candidate"
            [FingerprintBytes $ fingerprintCanonicalBytes candidate]
        ]
    }
 where
  maximumBytes = fromIntegral $ lengthFingerprintByteLimit
    $ checkedLengthSessionLimits session

buildSpinePairCompleteProblemFingerprint
  :: CheckedLengthSession identity annotation
  -> Fingerprint
      (InventoryFingerprintSubject FiniteBinaryProductSpineLengthsV1)
  -> Fingerprint
      (EncodingFingerprintSubject FiniteBinaryProductSpineLengthsV1)
  -> Fingerprint
      (CandidateFingerprintSubject FiniteBinaryProductSpineLengthsV1)
  -> Either FingerprintLimitError
      (Fingerprint
        (ProblemFingerprintSubject FiniteBinaryProductSpineLengthsV1))
buildSpinePairCompleteProblemFingerprint session inventory encoding candidate =
  buildFingerprintWithin maximumBytes FingerprintBuilder
    { fingerprintBuilderVersion = 1
    , fingerprintBuilderRole = ascii
        "finite-binary-product-spine-lengths/behavioral-problem"
    , fingerprintBuilderFields =
        [ tagged "dialect"
            [FingerprintBytes finiteBinaryProductSpineLengthsDomainTag]
        , tagged "inventory"
            [FingerprintBytes $ fingerprintCanonicalBytes inventory]
        , tagged "encoding"
            [FingerprintBytes $ fingerprintCanonicalBytes encoding]
        , tagged "candidate"
            [FingerprintBytes $ fingerprintCanonicalBytes candidate]
        ]
    }
 where
  maximumBytes = fromIntegral $ lengthFingerprintByteLimit
    $ checkedLengthSessionLimits session
