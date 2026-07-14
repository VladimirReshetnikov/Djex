-- | Checked Exference sessions behind Djex's shared query envelope.
--
-- Session construction seals the source inventory and computes Exference's
-- supported search projection once. Query execution performs every fallible
-- validation before returning a lazy sequence of total shared result batches.
-- This module is parser-neutral; Haskell source loading lives in the explicit
-- @exference-frontend@ component.
module Language.Haskell.Djex.Exference
  ( ExferenceSession
  , ExferenceEnvironment
  , ExferenceSessionPolicy (..)
  , defaultExferenceSessionPolicy
  , ExferenceOptions (..)
  , defaultExferenceOptions
  , ExferenceHeuristicsConfig (..)
  , Penalty (..)
  , Qualification (..)
  , ExferenceOmission (..)
  , ExferenceOmissionCapability (..)
  , ExferenceOmissionReason (..)
  , ExferenceRequest
  , ExferenceLocal
  , ExferenceTypeVariable
  , ExferenceType
  , ExferenceInventory
  , ExferenceCandidate
  , ExferenceCandidateDetails (..)
  , ExferenceCandidateMetrics (..)
  , ExferenceBatchMetadata (..)
  , RenderError (..)
  , ExferenceResult
  , mkExferenceSession
  , mkExferenceSessionWithPolicy
  , exferenceSessionInventory
  , exferenceSessionOmissions
  , exferenceSessionDiagnostics
  , mkExferenceRequest
  , exferenceRequestQuery
  , runExferenceQuery
  , exferenceCandidateMetrics
  , exferenceResultBindingUsages
  , renderExferenceCandidateExpression
  , renderExferenceCandidateDefinition
  , renderExferenceResidualConstraints
  ) where

import Control.DeepSeq (NFData (rnf))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import Numeric.Natural (Natural)

import Language.Haskell.Exference.Core
  ( ExferenceGeneratedSearchBatch
  , ExferenceHeuristicsConfig (..)
  , ExferenceInputError (..)
  , ExferenceQuery (..)
  , Penalty (..)
  , findGeneratedSearchBatchesWithHintsInEnvironmentEither
  , typeVariableHintsInEnvironment
  )
import qualified Language.Haskell.Exference.Core.Candidate as CoreCandidate
import qualified Language.Haskell.Exference.Core.ExferenceStats as CoreStats
import Language.Haskell.Exference.Core.Declaration
  ( freshSynthesisVariable )
import Language.Haskell.Exference.Core.Types
  ( HsType
  , fromSynthesisType
  , showVar
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
  , requestSourceLocation
  , requestSourceTypeVariables
  )
import Language.Haskell.Synthesis.Candidate
  ( Candidate (Candidate)
  , candidateDetails
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
  , withOptionalLocation
  )
import Language.Haskell.Synthesis.Generated
  ( DefinitionName
  , FunctionClause (FunctionClause)
  , Qualification (..)
  , RenderError (..)
  , RenderOptions
  , definitionName
  , renderOptionsWithLocalNameHints
  )
import Language.Haskell.Synthesis.Environment (Environment)
import Language.Haskell.Synthesis.Inventory
  ( Inventory )
import Language.Haskell.Synthesis.Kind (Kind (ProperTypeKind))
import Language.Haskell.Synthesis.Name
  ( Name
  , renderCanonical
  )
import Language.Haskell.Synthesis.Query
  ( QueryRequest (..)
  , QueryResult
  , queryResultFromCandidates
  , resultSearch
  )
import Language.Haskell.Synthesis.Search
  ( SearchBatch (SearchBatch)
  , batchCandidates
  , batchMetadata
  , batchProgress
  )
import Language.Haskell.Synthesis.Type
  ( Variable (FlexibleVariable, RigidVariable)
  )
import qualified Language.Haskell.Synthesis.TypeRender as SharedRender
import Language.Haskell.Synthesis.TypeSynonym
  ( TypeElaborationError (..)
  , elaborateType
  )

-- | Environment capabilities disabled before a reusable session is sealed.
-- Names are structural and exact: excluding @Data.Function.fix@ never hides
-- an unrelated qualified binding whose occurrence also happens to be @fix@.
data ExferenceSessionPolicy = ExferenceSessionPolicy
  { exferenceExcludedBindings :: [Name]
  , exferenceRatingOverrides :: Map.Map Name Penalty
  }
  deriving (Eq, Show)

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

type ExferenceCandidate =
  Candidate ExferenceType ExferenceCandidateDetails
    (FunctionClause ExferenceLocal)

type ExferenceResult =
  QueryResult ExferenceBatchMetadata ExferenceCandidate

data ExferenceCandidateMetrics = ExferenceCandidateMetrics
  { exferenceCandidateSteps :: Int
  , exferenceCandidateComplexity :: Penalty
  , exferenceCandidateFinalQueueSize :: Int
  }
  deriving (Eq, Show)

instance NFData ExferenceCandidateMetrics where
  rnf metrics =
    rnf (exferenceCandidateSteps metrics) `seq`
    rnf (exferenceCandidateComplexity metrics) `seq`
    rnf (exferenceCandidateFinalQueueSize metrics)

-- | Stable rendering hints and metrics attached to one checked candidate.
-- These are presentation data only; changing them cannot alter search output.
data ExferenceCandidateDetails = ExferenceCandidateDetails
  { exferenceCandidateStatistics :: ExferenceCandidateMetrics
  , exferenceCandidateLocalNames :: Map.Map ExferenceLocal String
  , exferenceCandidateTypeVariableNames ::
      Map.Map ExferenceTypeVariable String
  }
  deriving (Eq, Show)

instance NFData ExferenceCandidateDetails where
  rnf details =
    rnf (exferenceCandidateStatistics details) `seq`
    rnf (exferenceCandidateLocalNames details) `seq`
    rnf (exferenceCandidateTypeVariableNames details)

-- | Stable, lossless operational metadata for one Exference result batch.
-- Binding counts are exact non-negative totals, just like pruning counts.
data ExferenceBatchMetadata = ExferenceBatchMetadata
  { exferenceBatchBindingUsages :: Map.Map Name Natural
  , exferenceBatchQueuePruned :: Natural
  , exferenceBatchDepthPruned :: Natural
  }
  deriving (Eq, Show)

instance NFData ExferenceBatchMetadata where
  rnf metadata =
    rnf (exferenceBatchBindingUsages metadata) `seq`
    rnf (exferenceBatchQueuePruned metadata) `seq`
    rnf (exferenceBatchDepthPruned metadata)

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

exferenceSessionInventory :: ExferenceSession -> ExferenceInventory
exferenceSessionInventory = Session.exferenceSessionInventory

exferenceSessionOmissions :: ExferenceSession -> [ExferenceOmission]
exferenceSessionOmissions = Session.sessionOmissions

exferenceSessionDiagnostics :: ExferenceSession -> [Diagnostic]
exferenceSessionDiagnostics = map omissionDiagnostic
  . exferenceSessionOmissions

exferenceCandidateMetrics
  :: ExferenceCandidate
  -> ExferenceCandidateMetrics
exferenceCandidateMetrics = exferenceCandidateStatistics . candidateDetails

-- | Exact cumulative nominal source-binding usage at this result batch.
exferenceResultBindingUsages :: ExferenceResult -> Map.Map Name Natural
exferenceResultBindingUsages = exferenceBatchBindingUsages
  . batchMetadata . resultSearch

renderExferenceCandidateExpression
  :: Qualification
  -> ExferenceCandidate
  -> Either RenderError String
renderExferenceCandidateExpression qualification candidate =
  renderCandidateExpression
    (candidateRenderOptions qualification candidate) candidate

renderExferenceCandidateDefinition
  :: Qualification
  -> ExferenceCandidate
  -> Either RenderError String
renderExferenceCandidateDefinition qualification candidate =
  renderCandidateDefinition
    (candidateRenderOptions qualification candidate) candidate

renderExferenceResidualConstraints
  :: ExferenceCandidate
  -> [String]
renderExferenceResidualConstraints candidate =
  map (SharedRender.renderConstraint variableName)
  $ Set.toAscList
  $ Set.fromList
  $ candidateResidualConstraints candidate
 where
  variableName = candidateTypeVariableName candidate

candidateRenderOptions
  :: Qualification
  -> ExferenceCandidate
  -> RenderOptions ExferenceLocal
candidateRenderOptions qualification candidate =
  renderOptionsWithLocalNameHints qualification
    (exferenceCandidateLocalNames $ candidateDetails candidate)
    showVar
    []

candidateTypeVariableName
  :: ExferenceCandidate
  -> ExferenceTypeVariable
  -> String
candidateTypeVariableName candidate variable = Map.findWithDefault fallback
  variable $ exferenceCandidateTypeVariableNames $ candidateDetails candidate
 where
  fallback = case variable of
    FlexibleVariable identifier -> showVar identifier
    RigidVariable identifier -> "C" ++ showVar identifier

runExferenceQuery
  :: ExferenceSession
  -> ExferenceRequest
  -> Either Diagnostic [ExferenceResult]
runExferenceQuery session request = do
  let query = exferenceRequestQuery request
      target = requestTarget query
      sharedGoal = requestContextualGoal request
      requestDiagnostic = withOptionalLocation
        $ requestSourceLocation request
  elaboratedGoal <- either
    (Left . requestDiagnostic . elaborationFailure)
    Right
    $ elaborateType freshSynthesisVariable
        (Session.sessionTypeSynonyms session)
        ProperTypeKind
        sharedGoal
  backendGoal <- either
    (Left . requestDiagnostic . shownErrorDiagnostic
      "DJEX_EXF_LOWER"
      "cannot lower the shared query to Exference"
    )
    Right
    $ fromSynthesisType elaboratedGoal
  -- Only Exference's private exclusion set needs the raw structural name.
  -- Result projection retains the exact checked target from the request.
  let input = searchQuery (Just $ definitionName target) backendGoal
        $ requestOptions query
      searchFailure failure = requestDiagnostic $ shownErrorDiagnostic
        (if optionFailure failure
          then "DJEX_EXF_OPTIONS"
          else "DJEX_EXF_QUERY")
        (if optionFailure failure
          then "invalid Exference search options"
          else "Exference rejected the query")
        failure
  hints <- either (Left . searchFailure) Right
    $ typeVariableHintsInEnvironment
        (Session.sessionSearchEnvironment session)
        input
        (requestSourceTypeVariables request)
  batches <- either
    (Left . searchFailure)
    Right
    $ findGeneratedSearchBatchesWithHintsInEnvironmentEither
        hints (Session.sessionSearchEnvironment session) input
  pure $ map (resultBatch target) batches

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

optionFailure :: ExferenceInputError -> Bool
optionFailure failure = case failure of
  InvalidMaxSteps{} -> True
  InvalidConstraintDeferralSteps{} -> True
  InvalidMaxQueueSize{} -> True
  InvalidMaxDepth{} -> True
  InvalidHeuristic{} -> True
  _ -> False

resultBatch
  :: DefinitionName
  -> ExferenceGeneratedSearchBatch
  -> ExferenceResult
resultBatch target batch = queryResultFromCandidates $ SearchBatch
  (batchProgress batch)
  (projectBatchMetadata $ batchMetadata batch)
  (map (projectCandidate target) $ batchCandidates batch)

projectBatchMetadata
  :: CoreStats.ExferenceBatchMetadata
  -> ExferenceBatchMetadata
projectBatchMetadata metadata = ExferenceBatchMetadata
  { exferenceBatchBindingUsages = CoreStats.exferenceBindingUsages metadata
  , exferenceBatchQueuePruned = CoreStats.exferenceQueuePruned metadata
  , exferenceBatchDepthPruned = CoreStats.exferenceDepthPruned metadata
  }

projectCandidate
  :: DefinitionName
  -> CoreCandidate.ExferenceGeneratedCandidate
  -> ExferenceCandidate
projectCandidate target candidate = Candidate
  { candidateOutput = FunctionClause target [] $ candidateOutput candidate
  , candidateResidualConstraints = candidateResidualConstraints candidate
  , candidateDetails = ExferenceCandidateDetails
      { exferenceCandidateStatistics = ExferenceCandidateMetrics
          { exferenceCandidateSteps = CoreStats.exference_steps statistics
          , exferenceCandidateComplexity =
              CoreStats.exference_complexityRating statistics
          , exferenceCandidateFinalQueueSize =
              CoreStats.exference_finalSize statistics
          }
      , exferenceCandidateLocalNames =
          CoreCandidate.exferenceLocalNameHints details
      , exferenceCandidateTypeVariableNames =
          CoreCandidate.exferenceTypeVariableHints details
      }
  }
 where
  details = candidateDetails candidate
  statistics = CoreCandidate.exferenceCandidateStats details

searchQuery
  :: Maybe Name
  -> HsType
  -> ExferenceOptions
  -> ExferenceQuery
searchQuery excludedTarget goal options = ExferenceQuery
  { queryGoalType = goal
  , queryExcludedBindings = maybe Set.empty Set.singleton excludedTarget
  , queryAllowUnused = exferenceAllowUnused options
  , queryAllowConstraints = exferenceAllowResidualConstraints options
  , queryConstraintDeferralSteps = exferenceConstraintDeferralSteps options
  , queryMultiConstructorPatterns = exferenceMultiConstructorPatterns options
  , queryMaximumSteps = exferenceMaximumSteps options
  , queryMaximumQueueSize = exferenceMaximumQueueSize options
  , queryMaximumDepth = exferenceMaximumDepth options
  , queryHeuristics = exferenceHeuristics options
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
