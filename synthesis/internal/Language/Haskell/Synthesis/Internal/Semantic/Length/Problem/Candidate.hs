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
  , FiniteListSpineLengthV1
  , LengthContractError
  , LengthContractSource (..)
  , LengthContractVariable (..)
  , LengthExpression (..)
  , LengthFormula (..)
  , LengthProviderArgumentRole (..)
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
  , checkedLengthProviderArgumentRoles
  , checkedLengthProviderName
  , checkedLengthProviderScheme
  , checkedLengthProviderTransfer
  , checkedLengthSpineModelTrust
  , checkedLengthSpineRecursiveField
  , checkedLengthSpineStepConstructor
  , checkedLengthSpineTypeName
  , checkedLengthSpineZeroConstructor
  , contractVariableField
  , emptySyntaxUsage
  , finiteListSpineLengthDomainTag
  , inventoryTermScheme
  , isModeledSpine
  , lengthContractFingerprint
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
  , tagged
  )
import Language.Haskell.Synthesis.Internal.Semantic.Length.Problem
  ( CheckedLengthSession
  , LengthCasePolicy (..)
  , LengthTargetArgumentPolicy (..)
  , checkedLengthSessionCasePolicy
  , checkedLengthSessionContext
  , checkedLengthSessionLimits
  , checkedLengthSessionProviderInventory
  , checkedLengthSessionTargetArgumentPolicy
  , checkedLengthSessionExplicitTargetRoles
  , lengthSessionEncodingPolicyFingerprint
  , lengthSessionInventoryFingerprint
  , sealLengthContractInSession
  )
import Language.Haskell.Synthesis.Internal.Semantic.Problem
  ( BehavioralProblem
  , CandidateFingerprintSubject
  , EncodingFingerprintSubject
  , ProblemFingerprintSubject
  , behavioralProblemEncodingFingerprint
  , mkBehavioralProblem
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
  , typedCandidateTermGraph
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
  ( fingerprintTermGraphWithTypeStructure )

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
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthProblemFingerprintPart

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

type role CheckedLengthProblem nominal nominal

instance NFData (CheckedLengthProblem identity local) where
  rnf (CheckedLengthProblem candidate inputCount precondition postcondition
      condition problem) =
    rnf candidate `seq` rnf inputCount `seq` rnf precondition `seq`
    rnf postcondition `seq` rnf condition `seq`
    rnf (behavioralProblemEncodingFingerprint problem) `seq` rnf problem

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
    (CheckedLengthProblem candidate _ _ _ _ _) = candidate

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
    (CheckedLengthProblem _ inputCount _ _ _ _) =
  inputCount

-- | Normalized contract precondition retained for ordered concrete replay.
checkedLengthProblemPrecondition
  :: CheckedLengthProblem identity local
  -> LengthFormula LengthContractVariable
checkedLengthProblemPrecondition
    (CheckedLengthProblem _ _ precondition _ _ _) = precondition

-- | Normalized contract postcondition before candidate-result substitution.
-- Concrete replay evaluates this only after the precondition succeeds and
-- binds 'LengthResult' to the result computed from the checked candidate.
checkedLengthProblemPostcondition
  :: CheckedLengthProblem identity local
  -> LengthFormula LengthContractVariable
checkedLengthProblemPostcondition
    (CheckedLengthProblem _ _ _ postcondition _ _) = postcondition

-- | Solver-neutral bad-state formula: precondition and negated postcondition.
checkedLengthProblemCounterexampleCondition
  :: CheckedLengthProblem identity local
  -> LengthFormula LengthContractVariable
checkedLengthProblemCounterexampleCondition
    (CheckedLengthProblem _ _ _ _ condition _) = condition

-- | Concrete identity of contract, policy, used laws, result, and bad state.
checkedLengthProblemEncodingFingerprint
  :: CheckedLengthProblem identity local
  -> Fingerprint
      (EncodingFingerprintSubject FiniteListSpineLengthV1)
checkedLengthProblemEncodingFingerprint
    (CheckedLengthProblem _ _ _ _ _ problem) =
  behavioralProblemEncodingFingerprint problem

-- | Generic domain/inventory/encoding/candidate/problem envelope.
checkedLengthProblemBehavioralProblem
  :: CheckedLengthProblem identity local
  -> BehavioralProblem FiniteListSpineLengthV1
checkedLengthProblemBehavioralProblem
    (CheckedLengthProblem _ _ _ _ _ problem) = problem

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

data LengthProblemSealer
  = LengthLegacyProblemSealer
  | LengthRoleAwareProblemSealer
  | LengthExactSpineCaseProblemSealer
  | LengthSessionPolicyProblemSealer

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
  graph <- first LengthProblemTypedGraphUnavailable
    $ typedCandidateTermGraph typed
  graphFingerprint <- first LengthProblemTermGraphFingerprintRejected
    $ fingerprintLengthTermGraph session problemLimits graph
  rootNode <- case lookupTermNode (termGraphRoot graph) graph of
    Nothing -> Left $ LengthProblemRootNodeMissing $ termGraphRoot graph
    Just node -> Right node
  authorizedRigids <- matchRootOpening
    (checkedLengthContractTarget contract) $ termNodeType rootNode
  preflight <- preflightGraph session providers authorizedRigids graph
  (rawResult, evaluationState) <- runStateT
    (interpretCompleteCandidate
      InterpretationContext
        { interpretationSession = session
        , interpretationProblemLimits = problemLimits
        , interpretationGraph = graph
        , interpretationGlobals = preflightGlobals preflight
        , interpretationCases = preflightCases preflight
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
    $ buildCandidateFingerprint session graphFingerprint
  problemFingerprint <- mapFingerprintFailure LengthCompleteProblemFingerprint
    $ buildCompleteProblemFingerprint session encodingFingerprint
        candidateFingerprint
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

-- | Canonical zero-before-step receipt produced only after the session-owned
-- constructor schema has freshly re-sealed and fingerprinted the raw graph.
data ExactSpineCase identity local = ExactSpineCase
  !TermNodeId
  !TermNodeId
  !(TypedPattern (Type (Variable identity)) local)
  !TermNodeId

data PreflightGraph identity local = PreflightGraph
  { preflightGlobals :: !(Map Name (ModeledGlobal identity))
  , preflightCases :: !(Map TermNodeId (ExactSpineCase identity local))
  }

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
  -> Set identity
  -> TermGraph (Type (Variable identity)) local
  -> Either
      (LengthProblemError failure identity local)
      (PreflightGraph identity local)
preflightGraph session providers authorized graph = do
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
      semantic <- resolveGlobal inventory model providers authorized
        nodeId name nodeType
      Right $ Map.insert name semantic globals
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
  -> Set identity
  -> TermNodeId
  -> Name
  -> Type (Variable identity)
  -> Either
      (LengthProblemError failure identity local)
      (ModeledGlobal identity)
resolveGlobal inventory model providers authorized nodeId name actual
  | name == checkedLengthSpineZeroConstructor model =
      ModeledZero <$ validateConstructor False
  | name == checkedLengthSpineStepConstructor model =
      ModeledStep (checkedLengthSpineRecursiveField model)
        <$ validateConstructor True
  | Just provider <- lookupCheckedLengthProviderSummary name providers =
      if schemeAdmits authorized (checkedLengthProviderScheme provider) actual
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
  , interpretationGlobals :: !(Map Name (ModeledGlobal identity))
  , interpretationCases :: !(Map TermNodeId (ExactSpineCase identity local))
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
  value <- evaluateNode context emptyEnvironment root
  (applied, observedCount) <- foldM (applyTargetArgument context root)
    (value, 0)
    $ zip [0 ..] $ interpretationTargetArgumentRoles context
  if observedCount == fromIntegral (interpretationInputCount context)
    then pure ()
    else lift $ Left $ LengthProblemTargetArgumentPolicyMismatch
  requireSpine root applied

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
    TypedGlobal _ name -> case Map.lookup name $ interpretationGlobals context of
      Just ModeledZero -> pure $ SemanticSpine $ LengthLiteral 0
      Just (ModeledStep recursiveIndex) ->
        pure $ SemanticStep recursiveIndex []
      Just (ModeledProvider provider)
        | null $ checkedLengthProviderArgumentRoles provider ->
            interpretProvider context nodeId provider []
        | otherwise -> pure $ SemanticProvider provider []
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
    TypedVisibleTypeApplication _ function _ _ ->
      evaluateNode context environment function
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

saturatedSuccessor :: Int -> Int
saturatedSuccessor value
  | value == maxBound = maxBound
  | otherwise = value + 1

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
    { fingerprintBuilderVersion = case casePolicy of
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
            ] ++ mixedInterpreterPolicy ++ caseInterpreterPolicy
        , tagged "used-provider-laws"
            [FingerprintSequence $ map providerSummaryField usedProviders]
        , tagged "candidate-result"
            [lengthExpressionField contractVariableField result]
        , tagged "counterexample-condition"
            [lengthFormulaField contractVariableField condition]
        ]
    }
 where
  mixedRoles = LengthUnobservedTarget `elem`
    checkedLengthContractTargetArgumentRoles contract
  casePolicy = checkedLengthSessionCasePolicy session
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

buildCandidateFingerprint
  :: CheckedLengthSession identity annotation
  -> Fingerprint TermGraphFingerprintSubject
  -> Either FingerprintLimitError
      (Fingerprint
        (CandidateFingerprintSubject FiniteListSpineLengthV1))
buildCandidateFingerprint session graph =
  buildFingerprintWithin maximumBytes FingerprintBuilder
    { fingerprintBuilderVersion = 1
    , fingerprintBuilderRole = ascii
        "finite-list-spine-length/typed-candidate"
    , fingerprintBuilderFields =
        [ tagged "dialect"
            [FingerprintBytes finiteListSpineLengthDomainTag]
        , tagged "shared-typed-term-graph"
            [FingerprintBytes $ fingerprintCanonicalBytes graph]
        , tagged "candidate-authority"
            [ FingerprintBytes $ ascii "engine-owned-association/v1"
            , FingerprintBytes $ ascii "empty-residual-constraints/v1"
            , FingerprintBytes $ ascii "candidate-only-no-batch-status/v1"
            ]
        ]
    }
 where
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
