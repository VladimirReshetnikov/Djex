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
  , runExferenceQuery
  , exferenceCandidateMetrics
  , exferenceResultBindingUsages
  , renderExferenceCandidateExpression
  , renderExferenceCandidateDefinition
  , renderExferenceResidualConstraints
  ) where

import Control.DeepSeq (force)
import Control.Exception
  ( SomeAsyncException
  , SomeException
  , evaluate
  , fromException
  , tryJust
  )
import Data.Bifunctor (first)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
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
  ( ExferenceOmission (..)
  , ExferenceOmissionCapability (..)
  , ExferenceOmissionReason (..)
  , ExferenceSession
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
  , requestContextualGoal
  , requestSourceTypeVariableHints
  , withExferenceRequestProvenance
  )
import Language.Haskell.Synthesis.Candidate
  ( candidateDetails
  , candidateOutput
  , candidateResidualConstraints
  , renderCandidateDefinition
  , renderCandidateExpression
  )
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Info, Warning)
  , contextualDiagnostic
  , shownErrorDiagnostic
  )
import Language.Haskell.Synthesis.Generated
  ( Qualification (..)
  , RenderError (..)
  , RenderOptions
  , renderOptionsWithLocalNameHints
  )
import Language.Haskell.Synthesis.Environment (Environment)
import qualified Language.Haskell.Synthesis.Fresh as Fresh
import Language.Haskell.Synthesis.Inventory
  ( Inventory
  , inventoryEnvironment
  )
import Language.Haskell.Synthesis.Name
  ( Name
  , renderCanonical
  )
import Language.Haskell.Synthesis.Query
  ( QueryRequest (..)
  , resultSearch
  )
import Language.Haskell.Synthesis.Search
  ( batchMetadata
  )
import qualified Language.Haskell.Synthesis.TypeRender as SharedRender
import Language.Haskell.Synthesis.TypeSynonym
  ( TypeElaborationError (..)
  )

-- | Environment capabilities disabled before a reusable session is sealed.
-- Names are structural and exact: excluding @Data.Function.fix@ never hides
-- an unrelated qualified binding whose occurrence also happens to be @fix@.
data ExferenceSessionPolicy = ExferenceSessionPolicy
  { exferenceExcludedBindings :: [Name]
    -- ^ Exact binding names to remove from the search projection. Names not
    -- present in the environment are harmless no-ops.
  , exferenceRatingOverrides :: Map.Map Name Penalty
    -- ^ Finite replacement ratings for available bindings. An unavailable
    -- name is rejected instead of being silently ignored.
  }
  deriving (Eq, Show)

-- | Unrestricted session policy: retain every supported binding and its
-- source rating.
defaultExferenceSessionPolicy :: ExferenceSessionPolicy
defaultExferenceSessionPolicy = ExferenceSessionPolicy
  { exferenceExcludedBindings = []
  , exferenceRatingOverrides = Map.empty
  }

-- | The parser-independent declaration environment accepted by the stable
-- Exference adapter. Search ratings are session policy rather than source
-- semantics, so the neutral declarations carry no backend annotation.
type ExferenceEnvironment = Environment ExferenceTypeVariable Void ()

-- | The annotation-erased neutral inventory retained by a stable session.
-- Search ratings remain in the private backend projection, where they belong.
type ExferenceInventory = Inventory ExferenceTypeVariable ()

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
mkExferenceSessionWithPolicy policy =
  Session.sealNeutralExferenceSessionWithPolicy
    (exferenceExcludedBindings policy)
    (exferenceRatingOverrides policy)

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

-- | Render distinct residual constraints in structural order. Invalid,
-- duplicate, or unavailable source type-variable hints receive deterministic
-- fresh fallback names.
renderExferenceResidualConstraints
  :: ExferenceCandidate
  -> [String]
renderExferenceResidualConstraints candidate =
  map (SharedRender.renderConstraint variableName) constraints
 where
  constraints = Set.toAscList $ Set.fromList
    $ candidateResidualConstraints candidate
  variables = foldMap (foldMap $ foldMap Set.singleton) constraints
  variableNames = residualTypeVariableNames candidate variables
  variableName variable = Map.findWithDefault
    (defaultVariableName variable) variable variableNames

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
          (Map.insert variable spelling names, Set.insert spelling used)
    _ -> state

  assignFallback state@(names, used) variable
    | Map.member variable acceptedHints = state
    | otherwise =
        let spelling = freshFallback used $ defaultVariableName variable
        in (Map.insert variable spelling names, Set.insert spelling used)

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
runExferenceQuery session request = do
  let query = exferenceRequestQuery request
      target = requestTarget query
      sharedGoal = requestContextualGoal request
      requestDiagnostic = withExferenceRequestProvenance request
      optionFailure = shownErrorDiagnostic
        "DJEX_EXF_OPTIONS" "invalid Exference search options"
  -- Options are session-independent request policy. Check them before goal
  -- elaboration so reusing a request against an incompatible session cannot
  -- give a malformed option source provenance or a synonym/kind diagnostic.
  first optionFailure
    $ CoreInternal.validateExferenceOptions $ requestOptions query
  elaboratedGoal <- first (requestDiagnostic . elaborationFailure)
    $ Session.elaborateSessionGoal session sharedGoal
  backendGoal <- first
    (requestDiagnostic . shownErrorDiagnostic
      "DJEX_EXF_LOWER"
      "Exference rejected the shared query type"
    )
    $ fromSynthesisType elaboratedGoal
  let sourceHints = retargetExferenceSourceTypeVariableHints
        elaboratedGoal $ requestSourceTypeVariableHints request
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
  first searchFailure
    $ Core.findQueryResultsInEnvironmentEither
        target
        sourceHints
        (Session.sessionSearchEnvironment session)
        input

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
