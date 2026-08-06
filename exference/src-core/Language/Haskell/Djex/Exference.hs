{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Checked Exference sessions behind Djex's shared query envelope.
--
-- Session construction seals the source inventory and computes Exference's
-- supported search projection once. Query execution performs every fallible
-- validation before returning a lazy sequence of total shared result batches.
-- This module is parser-neutral; Haskell source loading lives behind the
-- explicit "Language.Haskell.Djex.Exference.HaskellSrc" boundary in the same
-- library.
--
-- The checked workflow is to seal an 'ExferenceEnvironment' with
-- 'mkExferenceSession', construct an opaque 'ExferenceRequest', and pass both
-- to 'runExferenceQuery'. The returned batches use Djex's shared query and
-- search envelopes, while candidates, metrics, and rendering remain
-- Exference-specific.
module Language.Haskell.Djex.Exference
  ( -- * Sessions
    ExferenceSession
  , ExferenceEnvironment
  , ExferenceInventory
  , ExferenceSessionPolicy (..)
  , defaultExferenceSessionPolicy
  , ExferenceOmission (..)
  , ExferenceOmissionCapability (..)
  , ExferenceOmissionReason (..)
  , mkExferenceSession
  , mkExferenceSessionWithPolicy
  , exferenceSessionEnvironment
  , exferenceSessionInventory
  , exferenceSessionOmissions
  , exferenceSessionDiagnostics

    -- * Requests
  , ExferenceRequest
  , ExferenceLocal
  , ExferenceTypeVariable
  , ExferenceType
  , ExferenceOptions (..)
  , defaultExferenceOptions
  , ExferenceHeuristicsConfig (..)
  , Penalty (..)
  , mkExferenceRequest
  , exferenceRequestQuery

    -- * Results
  , ExferenceResult
  , ExferenceCandidate
  , ExferenceCandidateDetails
  , pattern ExferenceCandidateDetails
  , exferenceCandidateStatistics
  , exferenceCandidateLocalNames
  , exferenceCandidateTypeVariableNames
  , ExferenceCandidateMetrics
  , pattern ExferenceCandidateMetrics
  , exferenceCandidateSteps
  , exferenceCandidateComplexity
  , exferenceCandidateFinalQueueSize
  , ExferenceBatchMetadata
  , pattern ExferenceBatchMetadata
  , exferenceBatchBindingUsages
  , exferenceBatchQueuePruned
  , exferenceBatchDepthPruned
  , Qualification (..)
  , RenderError (..)
  , ExferenceResidualRenderError (..)
  , runExferenceQuery
  , runExferenceQueryWithInstantiationCandidates
  , runExferenceQueryWithInstantiationAssignments
  , runExferenceQueryWithKindedInstantiationAssignments
  , exferenceCandidateMetrics
  , exferenceResultBindingUsages
  , renderExferenceCandidateExpression
  , renderExferenceCandidateDefinition
  , renderExferenceResidualConstraints
  , renderExferenceResidualConstraintsWithQualification
  ) where

import Control.DeepSeq (NFData, force)
import Control.Exception
  ( SomeAsyncException
  , SomeException
  , evaluate
  , fromException
  , tryJust
  )
import Control.Monad (foldM)
import Data.Bifunctor (first)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.IO.Unsafe (unsafePerformIO)

import Language.Haskell.Exference.Core
  ( ExferenceHeuristicsConfig (..)
  , ExferenceQuery (..)
  , Penalty (..)
  )
import qualified Language.Haskell.Exference.Core as Core
import qualified Language.Haskell.Exference.Core.Candidate as CoreCandidate
import qualified Language.Haskell.Exference.Core.ExferenceStats as CoreStats
import qualified Language.Haskell.Exference.Core.Internal.Exference as CoreInternal
import qualified Language.Haskell.Exference.Core.Internal.Polytype as CorePolytype
import Language.Haskell.Exference.Core.Types
  ( HsType
  , defaultVariableName
  , fromSynthesisType
  , showVar
  )
import Language.Haskell.Exference.Core.Internal.Candidate
  ( retargetExferenceSourceTypeVariableHints
  , validateExferenceTypeVariableSpelling
  )
import Language.Haskell.Djex.Exference.Internal.Session
  ( ExferenceEnvironment
  , ExferenceInventory
  , ExferenceOmission (..)
  , ExferenceOmissionCapability (..)
  , ExferenceOmissionReason (..)
  , ExferenceSession
  , ExferenceSessionPolicy (..)
  , defaultExferenceSessionPolicy
  )
import qualified Language.Haskell.Djex.Exference.Internal.Session as Session
import Language.Haskell.Djex.Exference.Internal.Request
  ( ExferenceLocal
  , ExferenceOptions (..)
  , ExferenceRequest
  , ExferenceType
  , ExferenceTypeVariable
  , defaultExferenceOptions
  , exferenceRequestQuery
  , mkExferenceRequest
  , prepareExferenceRequestContexts
  , withExferenceRequestProvenance
  )
import Language.Haskell.Synthesis.Candidate
  ( candidateDetails
  , candidateOutput
  , candidateResidualConstraints
  , renderCandidateDefinition
  , renderCandidateExpression
  )
import qualified Language.Haskell.Synthesis.Constraint as SharedConstraint
import qualified Language.Haskell.Synthesis.Collection as SharedCollection
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error, Info, Warning)
  , contextualDiagnostic
  , shownErrorDiagnostic
  )
import Language.Haskell.Synthesis.Generated
  ( Qualification (..)
  , RenderError (..)
  , RenderOptions
  , renderOptionsWithLocalNameHints
  , specifiedVisibleTypeArgument
  )
import qualified Language.Haskell.Synthesis.Fresh as Fresh
import Language.Haskell.Synthesis.Inventory
  ( inventoryEnvironment
  , inventoryKindAssumptions
  )
import Language.Haskell.Synthesis.Kind
  ( Kind (ProperTypeKind)
  , observedKindNodeCount
  )
import qualified Language.Haskell.Synthesis.KindInference as SharedKindInference
import Language.Haskell.Synthesis.Name
  ( Name
  , renderCanonical
  )
import Language.Haskell.Synthesis.Query
  ( KindedProviderInstantiationAssignment (..)
  , ProviderInstantiationCandidate (..)
  , ProviderInstantiationAssignment (..)
  , QueryRequest (..)
  , maximumProviderInstantiationArguments
  , maximumProviderInstantiationAssignments
  , maximumProviderInstantiationCandidates
  , maximumProviderInstantiationKindNodes
  , resultSearch
  )
import Language.Haskell.Synthesis.Search
  ( batchMetadata
  )
import qualified Language.Haskell.Synthesis.Type as SharedType
import qualified Language.Haskell.Synthesis.TypeAtom as SharedTypeAtom
import qualified Language.Haskell.Synthesis.TypeRender as SharedRender
import Language.Haskell.Synthesis.TypeSynonym
  ( TypeElaborationError (..)
  )

-- | The result payload is owned by the search core.  This stable module only
-- supplies compatibility spellings for its historical public selectors.
type ExferenceCandidate = Core.ExferenceCandidate

-- | One checked Exference search batch in Djex's shared result envelope.
-- Inspect its progress, candidates, and exact batch metadata through the
-- selectors exported by "Language.Haskell.Synthesis.Query" and
-- "Language.Haskell.Synthesis.Search".
type ExferenceResult = Core.ExferenceResult

-- | Per-candidate search measurements retained by the checked result.
type ExferenceCandidateMetrics = CoreStats.ExferenceStats

-- | Bidirectional record-pattern view of Exference's candidate measurements.
pattern ExferenceCandidateMetrics
  :: Int
  -- ^ Search steps completed when the candidate was found.
  -> Penalty
  -- ^ Final heuristic complexity rating; lower values rank ahead.
  -> Int
  -- ^ Search-queue size immediately after the producing step.
  -> ExferenceCandidateMetrics
pattern ExferenceCandidateMetrics
  { -- | Search steps completed when the candidate was found.
    exferenceCandidateSteps
  , -- | Final heuristic complexity rating; lower values rank ahead.
    exferenceCandidateComplexity
  , -- | Search-queue size immediately after the producing step.
    exferenceCandidateFinalQueueSize
  } = CoreStats.ExferenceStats
    { CoreStats.exference_steps = exferenceCandidateSteps
    , CoreStats.exference_complexityRating = exferenceCandidateComplexity
    , CoreStats.exference_finalSize = exferenceCandidateFinalQueueSize
    }

{-# COMPLETE ExferenceCandidateMetrics #-}

-- | Stable rendering hints and metrics attached to one checked candidate.
-- These names are a zero-cost view of the core-owned details record.
type ExferenceCandidateDetails = CoreCandidate.ExferenceCandidateDetails

-- | Bidirectional record-pattern view of candidate metrics and rendering
-- hints. The hint maps are preferences, not semantic parts of the candidate.
pattern ExferenceCandidateDetails
  :: ExferenceCandidateMetrics
  -- ^ Operational measurements for this candidate.
  -> Map.Map ExferenceLocal String
  -- ^ Preferred source spellings for generated term binders.
  -> Map.Map ExferenceTypeVariable String
  -- ^ Preferred source spellings for residual type variables.
  -> ExferenceCandidateDetails
pattern ExferenceCandidateDetails
  { -- | Operational measurements for this candidate.
    exferenceCandidateStatistics
  , -- | Preferred source spellings for generated term binders.
    exferenceCandidateLocalNames
  , -- | Preferred source spellings for residual type variables.
    exferenceCandidateTypeVariableNames
  } = CoreCandidate.ExferenceCandidateDetails
    { CoreCandidate.exferenceCandidateStats = exferenceCandidateStatistics
    , CoreCandidate.exferenceLocalNameHints = exferenceCandidateLocalNames
    , CoreCandidate.exferenceTypeVariableHints =
        exferenceCandidateTypeVariableNames
    }

{-# COMPLETE ExferenceCandidateDetails #-}

-- | Stable, lossless operational metadata for one Exference result batch.
-- Binding counts are exact non-negative totals, just like pruning counts.  The
-- pattern keeps the stable vocabulary without copying the core-owned record.
type ExferenceBatchMetadata = CoreStats.ExferenceBatchMetadata

-- | Bidirectional record-pattern view of exact cumulative batch statistics.
pattern ExferenceBatchMetadata
  :: Map.Map Name Natural
  -- ^ Uses of source bindings observed through this batch.
  -> Natural
  -- ^ Queue entries discarded through this batch.
  -> Natural
  -- ^ Over-depth entries discarded through this batch.
  -> ExferenceBatchMetadata
pattern ExferenceBatchMetadata
  { -- | Uses of source bindings observed through this batch.
    exferenceBatchBindingUsages
  , -- | Queue entries discarded through this batch.
    exferenceBatchQueuePruned
  , -- | Over-depth entries discarded through this batch.
    exferenceBatchDepthPruned
  } = CoreStats.ExferenceBatchMetadata
    { CoreStats.exferenceBindingUsages = exferenceBatchBindingUsages
    , CoreStats.exferenceQueuePruned = exferenceBatchQueuePruned
    , CoreStats.exferenceDepthPruned = exferenceBatchDepthPruned
    }

{-# COMPLETE ExferenceBatchMetadata #-}

-- | Kind-check, elaborate, and lower a parser-independent declaration
-- environment, then seal the resulting reusable Exference session.
mkExferenceSession
  :: ExferenceEnvironment
  -> Either Diagnostic ExferenceSession
mkExferenceSession = mkExferenceSessionWithPolicy
  defaultExferenceSessionPolicy

-- | Policy-aware neutral session construction. Exact-name rating overrides
-- affect only the private search projection; neither declaration order nor
-- the annotation-free inventory exposed by the session changes.
mkExferenceSessionWithPolicy
  :: ExferenceSessionPolicy
  -> ExferenceEnvironment
  -> Either Diagnostic ExferenceSession
mkExferenceSessionWithPolicy =
  Session.sealNeutralExferenceSessionWithPolicy

-- | Recover the exact neutral declaration environment sealed into this
-- session. Policy exclusions and rating overrides affect only Exference's
-- private search projection; they never rewrite the authoritative source
-- environment.
exferenceSessionEnvironment :: ExferenceSession -> ExferenceEnvironment
exferenceSessionEnvironment = inventoryEnvironment
  . exferenceSessionInventory

-- | Recover the checked, annotation-free inventory sealed into the session.
exferenceSessionInventory :: ExferenceSession -> ExferenceInventory
exferenceSessionInventory = Session.exferenceSessionInventory

-- | Capabilities omitted while projecting the neutral inventory into the
-- Exference search environment, in deterministic projection order.
exferenceSessionOmissions :: ExferenceSession -> [ExferenceOmission]
exferenceSessionOmissions = Session.sessionOmissions

-- | Render every session omission as a structured diagnostic. Unsupported
-- capabilities are warnings; explicit policy exclusions are informational.
exferenceSessionDiagnostics :: ExferenceSession -> [Diagnostic]
exferenceSessionDiagnostics = map omissionDiagnostic
  . exferenceSessionOmissions

-- | Recover the search measurements attached to a checked candidate.
exferenceCandidateMetrics
  :: ExferenceCandidate
  -> ExferenceCandidateMetrics
exferenceCandidateMetrics = exferenceCandidateStatistics . candidateDetails

-- | Exact cumulative nominal source-binding usage at this result batch.
exferenceResultBindingUsages :: ExferenceResult -> Map.Map Name Natural
exferenceResultBindingUsages = exferenceBatchBindingUsages
  . batchMetadata . resultSearch

-- | Render only the candidate's right-hand-side expression, honoring the
-- requested qualification policy and its local-name hints.
renderExferenceCandidateExpression
  :: Qualification
  -> ExferenceCandidate
  -> Either RenderError String
renderExferenceCandidateExpression qualification candidate =
  renderCandidateExpression
    (candidateRenderOptions qualification candidate) candidate

-- | Render the candidate as a complete top-level definition, honoring the
-- requested qualification policy and its local-name hints.
renderExferenceCandidateDefinition
  :: Qualification
  -> ExferenceCandidate
  -> Either RenderError String
renderExferenceCandidateDefinition qualification candidate =
  renderCandidateDefinition
    (candidateRenderOptions qualification candidate) candidate

-- | A caller-built candidate contained an invalid residual obligation.
--
-- Constraint and argument positions are zero-based source-list indexes. Class
-- identity is checked before any argument, and arguments are checked from
-- left to right, so the first error is deterministic without sorting or
-- deduplicating the caller's input first.
data ExferenceResidualRenderError
  = InvalidResidualConstraintClass
      Natural
      -- ^ Index of the residual constraint in the candidate.
      SharedConstraint.ConstraintError
      -- ^ Invalid nominal class identity.
  | InvalidResidualConstraintArgument
      Natural
      -- ^ Index of the residual constraint in the candidate.
      Natural
      -- ^ Index of the invalid argument in that constraint.
      (SharedType.TypeError ExferenceTypeVariable)
      -- ^ Complete shared-type validation failure.
  deriving (Eq, Ord, Show, Generic)

instance NFData ExferenceResidualRenderError

-- | Validate and render distinct residual constraints in structural order.
-- Invalid, duplicate, or unavailable source type-variable hints receive
-- deterministic fresh fallback names. This entry point retains the historical
-- fully qualified rendering policy, but now returns a structured error like
-- the expression and definition renderers because the public @Candidate@
-- constructor permits caller-built residuals.
renderExferenceResidualConstraints
  :: ExferenceCandidate
  -> Either ExferenceResidualRenderError [String]
renderExferenceResidualConstraints =
  renderExferenceResidualConstraintsWithQualification FullyQualified

-- | Validate and render residual obligations under one qualification policy.
-- The class name and every constructor nested in its arguments follow the
-- same policy as the candidate term. Validation precedes sorting and
-- deduplication, preserving the first failure in the candidate's original
-- constraint and argument order.
renderExferenceResidualConstraintsWithQualification
  :: Qualification
  -> ExferenceCandidate
  -> Either ExferenceResidualRenderError [String]
renderExferenceResidualConstraintsWithQualification qualification candidate = do
  validateResidualConstraints candidate
  pure $ map
    (SharedRender.renderConstraintWithQualification qualification variableName)
    constraints
 where
  constraints = Set.toAscList $ Set.fromList
    $ candidateResidualConstraints candidate
  variables = foldMap (foldMap $ foldMap Set.singleton) constraints
  variableNames = residualTypeVariableNames candidate variables
  variableName variable = Map.findWithDefault
    (defaultVariableName variable) variable variableNames

validateResidualConstraints
  :: ExferenceCandidate
  -> Either ExferenceResidualRenderError ()
validateResidualConstraints candidate = mapM_ validateIndexedConstraint
  $ zip [0 ..] $ candidateResidualConstraints candidate
 where
  validateIndexedConstraint (constraintIndex, constraint) = do
    first (InvalidResidualConstraintClass constraintIndex)
      $ SharedConstraint.validateConstraint constraint
    mapM_ (validateIndexedArgument constraintIndex)
      $ zip [0 ..] $ SharedConstraint.constraintArguments constraint

  validateIndexedArgument constraintIndex (argumentIndex, argument) =
    first
      (InvalidResidualConstraintArgument constraintIndex argumentIndex)
      $ SharedType.validateType argument

candidateRenderOptions
  :: Qualification
  -> ExferenceCandidate
  -> RenderOptions ExferenceLocal
candidateRenderOptions qualification candidate =
  renderOptionsWithLocalNameHints qualification
    (boundedLocalNameHints candidate)
    showVar
    []

-- Public compatibility constructors can still attach caller-created hint
-- maps to a candidate. Treat both local and type-variable maps as untrusted at
-- the final stable rendering boundary: inspect only bounded, fully evaluated
-- copies and fall back on partial, infinite, or oversized values. Type names
-- are additionally validated and deduplicated here; finite malformed local
-- names retain the generated renderer's established structured error. Canonical
-- checked-query candidates take the same path, so this defense does not depend
-- on a value's unobservable provenance.
boundedLocalNameHints
  :: ExferenceCandidate
  -> Map.Map ExferenceLocal String
boundedLocalNameHints candidate = Map.fromAscList
  [ (local, spelling)
  | local <- Set.toAscList locals
  , Just spelling <- [safeBoundedHint $ Map.lookup local rawHints]
  ]
 where
  locals = foldMap Set.singleton $ candidateOutput candidate
  rawHints = exferenceCandidateLocalNames $ candidateDetails candidate

residualTypeVariableNames
  :: ExferenceCandidate
  -> Set.Set ExferenceTypeVariable
  -> Map.Map ExferenceTypeVariable String
residualTypeVariableNames candidate variables = fst
  $ List.foldl' assignFallback
      (acceptedHints, acceptedSpellings) orderedVariables
 where
  orderedVariables = Set.toAscList variables
  rawHints = exferenceCandidateTypeVariableNames $ candidateDetails candidate

  (acceptedHints, acceptedSpellings) = List.foldl' acceptHint
    (Map.empty, Set.empty) orderedVariables

  acceptHint state@(names, used) variable = case
      safeTypeVariableHint $ Map.lookup variable rawHints of
    Just spelling
      | Set.notMember spelling used ->
          strictNameState
            (Map.insert variable spelling names)
            (Set.insert spelling used)
    _ -> state

  assignFallback state@(names, used) variable
    | Map.member variable acceptedHints = state
    | otherwise =
        let spelling = freshFallback used $ defaultVariableName variable
        in strictNameState
          (Map.insert variable spelling names)
          (Set.insert spelling used)

  -- 'foldl'' forces only the pair constructor. Materialize both indexes on
  -- every step so wide caller-built residuals cannot retain deferred map or
  -- set updates until the final rendering pass.
  strictNameState names used = names `seq` used `seq` (names, used)

-- Appending a prime preserves the lexical class of both historical fallback
-- forms (the legacy renderer's 'defaultVariableName' policy). The finite set
-- of rendered variables guarantees termination.
freshFallback :: Set.Set String -> String -> String
freshFallback = Fresh.selectFresh (++ "'")

-- Haskell has no intrinsic identifier-length limit, but a pure renderer
-- cannot prove that a caller-created lazy String is finite. A generous fixed
-- observation budget makes rejection total for cyclic/infinite hints and also
-- bounds presentation work on compatibility values.
maximumRenderedHintLength :: Int
maximumRenderedHintLength = 4096

safeTypeVariableHint :: Maybe String -> Maybe String
safeTypeVariableHint source = do
  spelling <- safeBoundedHint source
  case validateExferenceTypeVariableSpelling spelling of
    Right () -> Just spelling
    Left _ -> Nothing

safeBoundedHint :: Maybe String -> Maybe String
safeBoundedHint source = unsafePerformIO $ do
  copied <- tryJust synchronousException $ evaluate $ force $ case source of
    Nothing -> Nothing
    Just spelling -> Just
      $ take (maximumRenderedHintLength + 1) spelling
  pure $ case copied of
    Left () -> Nothing
    Right Nothing -> Nothing
    Right (Just spelling)
      | length spelling > maximumRenderedHintLength -> Nothing
      | otherwise -> Just spelling
 where
  -- Cancellation and runtime resource exceptions must retain their ordinary
  -- asynchronous semantics; only exceptions raised while evaluating a
  -- caller-owned pure hint are converted to a missing preference.
  synchronousException :: SomeException -> Maybe ()
  synchronousException exception = case
      fromException exception :: Maybe SomeAsyncException of
    Just _ -> Nothing
    Nothing -> Just ()

{-# NOINLINE safeBoundedHint #-}

-- | Elaborate and lower the checked request against a sealed session, then
-- return its lazy batch trace. Query, option, and lowering failures are
-- reported before the 'Right' result is exposed.
runExferenceQuery
  :: ExferenceSession
  -> ExferenceRequest
  -> Either Diagnostic [ExferenceResult]
runExferenceQuery session =
  runExferenceQueryWithProviderEvidence session []
    $ InferredProviderInstantiationAssignments []

-- | Run a checked request with a bounded collection of closed proper-type
-- choices established for exact retained global providers.  Candidate
-- associations are checked against this session after the request goal, and
-- never become evidence for local values or another global provider.
runExferenceQueryWithInstantiationCandidates
  :: ExferenceSession
  -> [ProviderInstantiationCandidate ExferenceTypeVariable]
  -> ExferenceRequest
  -> Either Diagnostic [ExferenceResult]
runExferenceQueryWithInstantiationCandidates session rawCandidates =
  runExferenceQueryWithProviderEvidence session rawCandidates
    $ InferredProviderInstantiationAssignments []

-- | Run a checked request with complete ordered leading-binder assignments
-- established for exact retained globals. Assignment vectors are checked in
-- this session and consumed directly; they are never reconstructed as a
-- Cartesian product or donated to another provider.
runExferenceQueryWithInstantiationAssignments
  :: ExferenceSession
  -> [ProviderInstantiationAssignment ExferenceTypeVariable]
  -> ExferenceRequest
  -> Either Diagnostic [ExferenceResult]
runExferenceQueryWithInstantiationAssignments session rawAssignments =
  runExferenceQueryWithProviderEvidence session []
    $ InferredProviderInstantiationAssignments rawAssignments

-- | Run a checked request with complete assignments whose exact leading-binder
-- ground kinds were retained by the caller. Supplied kinds are checked against
-- every occurrence in the retained provider body, while a vacuous binder keeps
-- the caller-established kind instead of taking the compatibility default of
-- @Type@.
runExferenceQueryWithKindedInstantiationAssignments
  :: ExferenceSession
  -> [KindedProviderInstantiationAssignment ExferenceTypeVariable]
  -> ExferenceRequest
  -> Either Diagnostic [ExferenceResult]
runExferenceQueryWithKindedInstantiationAssignments session rawAssignments =
  runExferenceQueryWithProviderEvidence session []
    $ KindedProviderInstantiationAssignments rawAssignments

data ProviderInstantiationAssignmentEvidence
  = InferredProviderInstantiationAssignments
      [ProviderInstantiationAssignment ExferenceTypeVariable]
  | KindedProviderInstantiationAssignments
      [KindedProviderInstantiationAssignment ExferenceTypeVariable]

data ProviderInstantiationAssignmentInput
  = InferredProviderInstantiationAssignmentInput
      (ProviderInstantiationAssignment ExferenceTypeVariable)
  | KindedProviderInstantiationAssignmentInput
      (KindedProviderInstantiationAssignment ExferenceTypeVariable)

runExferenceQueryWithProviderEvidence
  :: ExferenceSession
  -> [ProviderInstantiationCandidate ExferenceTypeVariable]
  -> ProviderInstantiationAssignmentEvidence
  -> ExferenceRequest
  -> Either Diagnostic [ExferenceResult]
runExferenceQueryWithProviderEvidence
    session rawCandidates assignmentEvidence request = do
  let query = exferenceRequestQuery request
      target = requestTarget query
      requestDiagnostic = withExferenceRequestProvenance request
      optionFailure = shownErrorDiagnostic
        "DJEX_EXF_OPTIONS" "invalid Exference search options"
  -- Options are session-independent request policy. Check them before goal
  -- elaboration so reusing a request against an incompatible session cannot
  -- give a malformed option source provenance or a synonym/kind diagnostic.
  checkedOptions <- first optionFailure
    $ CoreInternal.checkExferenceOptions $ requestOptions query
  (sharedGoal, checkedSourceHints) <-
    prepareExferenceRequestContexts
      (Session.sessionClassArity session) request
  elaboratedGoal <- first (requestDiagnostic . elaborationFailure)
    $ Session.elaborateSessionGoal session sharedGoal
  backendGoal <- first
    (requestDiagnostic . shownErrorDiagnostic
      "DJEX_EXF_LOWER"
      "Exference rejected the shared query type"
    )
    $ fromSynthesisType elaboratedGoal
  providerCandidates <- prepareProviderInstantiationCandidates
    session rawCandidates
  providerAssignments <- prepareProviderInstantiationAssignments
    session assignmentEvidence
  let sourceHints = retargetExferenceSourceTypeVariableHints
        elaboratedGoal checkedSourceHints
  -- The direct result boundary owns exact target exclusion and result naming,
  -- so query validation and rigid-instantiation planning happen only once.
  -- Option failures stay source-free like Djinn's: separately supplied
  -- options never carry type-source provenance.
  let input = searchQuery backendGoal
        $ requestOptions query
      searchFailure failure
        | Core.isExferenceOptionError failure = optionFailure failure
        | otherwise = requestDiagnostic $ shownErrorDiagnostic
            "DJEX_EXF_QUERY" "Exference rejected the query" failure
  first searchFailure $ if Map.null providerAssignments
    then CoreInternal.findQueryResultsInEnvironmentWithCheckedOptionsAndCandidates
      providerCandidates target sourceHints
      (Session.sessionSearchEnvironment session) input checkedOptions
    else CoreInternal.findQueryResultsInEnvironmentWithCheckedOptionsAndAssignments
      providerAssignments target sourceHints
      (Session.sessionSearchEnvironment session) input checkedOptions

prepareProviderInstantiationCandidates
  :: ExferenceSession
  -> [ProviderInstantiationCandidate ExferenceTypeVariable]
  -> Either Diagnostic (Map.Map Name [HsType])
prepareProviderInstantiationCandidates session rawCandidates
  | observed > maximumProviderInstantiationCandidates =
      Left $ shownErrorDiagnostic
        "DJEX_EXF_CANDIDATE_LIMIT"
        "too many Exference provider instantiation candidates"
        (maximumProviderInstantiationCandidates, observed)
  | otherwise = do
      checked <- traverse prepareCandidate rawCandidates
      pure $ Map.map
        (SharedCollection.distinctOn SharedTypeAtom.alphaTypeKey)
        $ List.foldl' retainCandidate Map.empty checked
 where
  observed = SharedCollection.observedListLength
    maximumProviderInstantiationCandidates rawCandidates
  searchEnvironment = Session.sessionSearchEnvironment session

  prepareCandidate candidate = do
    let provider = providerInstantiationCandidateProvider candidate
        candidateType = providerInstantiationCandidateType candidate
    if CoreInternal.exferenceEnvironmentContainsBinding
        provider searchEnvironment
      then pure ()
      else Left $ contextualDiagnostic Error
        "DJEX_EXF_CANDIDATE_PROVIDER"
        "Exference provider instantiation candidate names an unavailable binding"
        (renderCanonical provider)
    elaborated <- first (candidateElaborationFailure provider)
      $ Session.elaborateSessionGoal session candidateType
    if CorePolytype.isVisibleTypeCandidate elaborated
      then pure ()
      else Left $ shownErrorDiagnostic
        "DJEX_EXF_CANDIDATE_TYPE"
        "invalid Exference provider instantiation candidate type"
        (provider, elaborated)
    lowered <- first
      (\failure -> shownErrorDiagnostic
        "DJEX_EXF_CANDIDATE_LOWER"
        "Exference could not lower a provider instantiation candidate"
        (provider, failure))
      $ fromSynthesisType elaborated
    pure (provider, lowered)

  retainCandidate candidates (provider, candidate) = Map.insertWith
    (\new old -> old ++ new)
    provider [candidate] candidates

prepareProviderInstantiationAssignments
  :: ExferenceSession
  -> ProviderInstantiationAssignmentEvidence
  -> Either Diagnostic (Map.Map Name [[HsType]])
prepareProviderInstantiationAssignments session evidence
  | observed > maximumProviderInstantiationAssignments =
      Left $ shownErrorDiagnostic
        "DJEX_EXF_ASSIGNMENT_LIMIT"
        "too many Exference provider instantiation assignments"
        (maximumProviderInstantiationAssignments, observed)
  | otherwise = do
      (_, _, retained) <- foldM prepareAssignment
        (Map.empty, Map.empty, Map.empty) $
        zip [0 :: Int ..] rawAssignments
      pure retained
 where
  rawAssignments = case evidence of
    InferredProviderInstantiationAssignments assignments ->
      map InferredProviderInstantiationAssignmentInput assignments
    KindedProviderInstantiationAssignments assignments ->
      map KindedProviderInstantiationAssignmentInput assignments
  observed = SharedCollection.observedListLength
    maximumProviderInstantiationAssignments rawAssignments
  searchEnvironment = Session.sessionSearchEnvironment session

  prepareAssignment
      (seenKinds, seenArguments, retained) (assignmentIndex, assignment) = do
    let (provider, argumentCount, suppliedKinds, rawArguments) =
          case assignment of
          InferredProviderInstantiationAssignmentInput inferred ->
            let arguments = providerInstantiationAssignmentArguments inferred
            in
            ( providerInstantiationAssignmentProvider inferred
            , SharedCollection.observedListLength
                maximumProviderInstantiationArguments arguments
            , Nothing
            , arguments
            )
          KindedProviderInstantiationAssignmentInput kinded ->
            let arguments =
                  kindedProviderInstantiationAssignmentArguments kinded
            in
            ( kindedProviderInstantiationAssignmentProvider kinded
            , SharedCollection.observedListLength
                maximumProviderInstantiationArguments arguments
            , Just $ map fst arguments
            , map snd arguments
            )
        label = "provider instantiation assignment #" ++
          show assignmentIndex ++ " for " ++ renderCanonical provider
    if argumentCount == 0
      then Left $ shownErrorDiagnostic
        "DJEX_EXF_ASSIGNMENT_ARITY"
        "Exference provider instantiation assignment is empty"
        label
      else if argumentCount > maximumProviderInstantiationArguments
        then Left $ shownErrorDiagnostic
          "DJEX_EXF_ASSIGNMENT_ARGUMENT_LIMIT"
          "too many Exference provider instantiation arguments"
          (label, maximumProviderInstantiationArguments, argumentCount)
        else pure ()
    scheme <- case CoreInternal.exferenceEnvironmentBindingScheme
        provider searchEnvironment of
      Nothing -> Left $ contextualDiagnostic Error
        "DJEX_EXF_ASSIGNMENT_PROVIDER"
        "Exference provider instantiation assignment names no retained polymorphic binding"
        (renderCanonical provider)
      Just retainedScheme -> Right retainedScheme
    let (binders, constraints, schemeBody) =
          SharedType.splitLeadingForalls scheme
    if length binders == argumentCount
      then pure ()
      else Left $ shownErrorDiagnostic
        "DJEX_EXF_ASSIGNMENT_ARITY"
        "Exference provider instantiation assignment has the wrong arity"
        (label, length binders, argumentCount)
    if null constraints
      then pure ()
      else Left $ shownErrorDiagnostic
        "DJEX_EXF_ASSIGNMENT_CONTEXT"
        "Exference provider instantiation assignment targets a contextual scheme"
        (label, constraints)
    binderKinds <- case suppliedKinds of
      Nothing -> do
        inferredBinderKinds <- first
          (\failure -> shownErrorDiagnostic
            "DJEX_EXF_ASSIGNMENT_KIND"
            "Exference could not infer provider binder kinds"
            (label, failure))
          $ SharedKindInference.inferSharedVariableKinds
              kindAssumptions binders [schemeBody]
        pure $ map snd inferredBinderKinds
      Just supplied -> do
        mapM_ (validateSuppliedKind label) $
          zip [0 :: Int ..] supplied
        first
          (\failure -> shownErrorDiagnostic
            "DJEX_EXF_ASSIGNMENT_KIND"
            "Exference rejected supplied provider binder kinds"
            (label, failure))
          $ SharedKindInference.checkTypesKinds kindAssumptions
              $ (ProperTypeKind, schemeBody) : zip supplied
                  (map SharedType.TypeVariable binders)
        pure supplied
    case Map.lookup provider seenKinds of
      Just established
        | established /= binderKinds -> Left $ shownErrorDiagnostic
            "DJEX_EXF_ASSIGNMENT_KIND"
            "Exference provider instantiation assignments disagree on binder kinds"
            (label, established, binderKinds)
      _ -> pure ()
    arguments <- traverse
      (prepareArgument label) $
        zip3 [0 :: Int ..] binderKinds rawArguments
    validateSpecializationKind label scheme arguments
    let key = map SharedTypeAtom.alphaTypeKey arguments
        providerKeys = Map.findWithDefault Set.empty provider seenArguments
        retainedKinds = Map.insert provider binderKinds seenKinds
    if key `Set.member` providerKeys
      then pure (retainedKinds, seenArguments, retained)
      else pure
        ( retainedKinds
        , Map.insert provider (Set.insert key providerKeys) seenArguments
        , Map.insertWith (\new old -> old ++ new)
            provider [arguments] retained
        )

  validateSuppliedKind label (argumentIndex, kind) =
    let observedNodes = observedKindNodeCount
          maximumProviderInstantiationKindNodes kind
    in if observedNodes <= maximumProviderInstantiationKindNodes
      then Right ()
      else Left $ shownErrorDiagnostic
        "DJEX_EXF_ASSIGNMENT_KIND_LIMIT"
        "Exference provider instantiation kind exceeds the structural bound"
        ( label
        , argumentIndex
        , maximumProviderInstantiationKindNodes
        , observedNodes
        )

  prepareArgument label (argumentIndex, binderKind, source) = do
    elaborated <- first (assignmentElaborationFailure label argumentIndex)
      $ Session.elaborateSessionTypeAtKind session binderKind source
    if Set.null (SharedType.freeVariables elaborated)
        && null (SharedType.typeConstraints elaborated)
      then pure ()
      else Left $ shownErrorDiagnostic
        "DJEX_EXF_ASSIGNMENT_TYPE"
        "Exference provider instantiation argument is not closed and context-free"
        (label, argumentIndex, elaborated)
    _ <- first
      (\failure -> shownErrorDiagnostic
        "DJEX_EXF_ASSIGNMENT_TYPE"
        "invalid Exference provider instantiation argument"
        (label, argumentIndex, failure))
      $ specifiedVisibleTypeArgument elaborated
    lowered <- first
      (\failure -> shownErrorDiagnostic
        "DJEX_EXF_ASSIGNMENT_LOWER"
        "Exference could not lower a provider instantiation argument"
        (label, argumentIndex, failure))
      $ fromSynthesisType elaborated
    if CorePolytype.isProviderAssignmentArgument lowered
      then Right lowered
      else Left $ shownErrorDiagnostic
        "DJEX_EXF_ASSIGNMENT_TYPE"
        "invalid lowered Exference provider instantiation argument"
        (label, argumentIndex, lowered)

  -- Every assignment argument has already been elaborated at the inferred or
  -- supplied kind of its exact provider-binder position. Reuse the core's
  -- exact ordered instantiation worker, then kind-check the complete
  -- substituted body against the session inventory as an independent
  -- specialization guard. This checks the whole scheme without reconstructing
  -- a Cartesian product or trusting the first-order lowering.
  validateSpecializationKind label scheme arguments = case
      CorePolytype.assignmentProviderInstantiations [arguments] scheme of
    [instantiation] -> first
      (\failure -> shownErrorDiagnostic
        "DJEX_EXF_ASSIGNMENT_KIND"
        "Exference rejected a provider instantiation assignment kind"
        (label, failure))
      $ SharedKindInference.checkTypesKinds kindAssumptions
          [(ProperTypeKind, CorePolytype.groundProviderType instantiation)]
    _ -> Left $ shownErrorDiagnostic
      "DJEX_EXF_ASSIGNMENT_TYPE"
      "Exference could not validate a provider instantiation assignment"
      (label, scheme, arguments)

  kindAssumptions = inventoryKindAssumptions
    $ Session.exferenceSessionInventory session

assignmentElaborationFailure
  :: String
  -> Int
  -> TypeElaborationError ExferenceTypeVariable
  -> Diagnostic
assignmentElaborationFailure label argumentIndex failure = case failure of
  IllKindedType _ _ -> shownErrorDiagnostic
    "DJEX_EXF_ASSIGNMENT_KIND"
    "Exference rejected a provider instantiation argument kind"
    (label, argumentIndex, failure)
  SynonymExpansionFailed _ -> shownErrorDiagnostic
    "DJEX_EXF_ASSIGNMENT_SYNONYM"
    "Exference could not expand a provider instantiation argument"
    (label, argumentIndex, failure)
  InvalidElaborationType _ _ -> shownErrorDiagnostic
    "DJEX_EXF_ASSIGNMENT_TYPE"
    "Exference rejected a provider instantiation argument type"
    (label, argumentIndex, failure)

candidateElaborationFailure
  :: Name
  -> TypeElaborationError ExferenceTypeVariable
  -> Diagnostic
candidateElaborationFailure provider failure = case failure of
  IllKindedType _ _ -> shownErrorDiagnostic
    "DJEX_EXF_CANDIDATE_KIND"
    "Exference rejected a provider instantiation candidate kind"
    (provider, failure)
  SynonymExpansionFailed _ -> shownErrorDiagnostic
    "DJEX_EXF_CANDIDATE_SYNONYM"
    "Exference could not expand a provider instantiation candidate"
    (provider, failure)
  InvalidElaborationType _ _ -> shownErrorDiagnostic
    "DJEX_EXF_CANDIDATE_TYPE"
    "Exference rejected a provider instantiation candidate type"
    (provider, failure)

elaborationFailure
  :: TypeElaborationError ExferenceTypeVariable
  -> Diagnostic
elaborationFailure failure = case failure of
  IllKindedType _ _ -> shownErrorDiagnostic
    "DJEX_EXF_KIND"
    "Exference rejected the query kind"
    failure
  SynonymExpansionFailed _ -> shownErrorDiagnostic
    "DJEX_EXF_SYNONYM"
    "Exference could not expand the query's type synonyms"
    failure
  InvalidElaborationType _ _ -> shownErrorDiagnostic
    "DJEX_EXF_QUERY"
    "Exference rejected the shared query type"
    failure

searchQuery
  :: HsType
  -> ExferenceOptions
  -> ExferenceQuery
searchQuery goal options = ExferenceQuery
  { queryGoalType = goal
  , queryExcludedBindings = mempty
  , querySearchOptions = options
  }

omissionDiagnostic :: ExferenceOmission -> Diagnostic
omissionDiagnostic omission = contextualDiagnostic severity code message
  (renderCanonical $ omittedName omission)
 where
  (severity, code, message) = case omittedReason omission of
    UnsupportedNestedForall ->
      ( Warning
      , "DJEX_EXF_OMISSION"
      , "Exference omitted a source capability containing a nested forall"
      )
    RecursiveDataEliminationUnsupported ->
      ( Warning
      , "DJEX_EXF_RECURSIVE_OMISSION"
      , "Exference omitted recursive datatype elimination"
      )
    ExcludedByPolicy ->
      ( Info
      , "DJEX_EXF_POLICY_OMISSION"
      , "Exference omitted a source binding disabled by session policy"
      )
