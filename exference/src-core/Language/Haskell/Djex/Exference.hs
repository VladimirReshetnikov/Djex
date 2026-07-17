{-# LANGUAGE PatternSynonyms #-}

-- | Checked Exference sessions behind Djex's shared query envelope.
--
-- Session construction seals the source inventory and computes Exference's
-- supported search projection once. Query execution performs every fallible
-- validation before returning a lazy sequence of total shared result batches.
-- This module is parser-neutral; Haskell source loading lives behind the
-- explicit "Language.Haskell.Djex.Exference.HaskellSrc" boundary in the same
-- library.
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
  , RenderError (..)
  , ExferenceResult
  , mkExferenceSession
  , mkExferenceSessionWithPolicy
  , exferenceSessionEnvironment
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

import Control.DeepSeq (force)
import Control.Exception
  ( SomeAsyncException
  , SomeException
  , evaluate
  , fromException
  , tryJust
  )
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import Numeric.Natural (Natural)
import System.IO.Unsafe (unsafePerformIO)

import Language.Haskell.Exference.Core
  ( ExferenceHeuristicsConfig (..)
  , ExferenceInputError (..)
  , ExferenceQuery (..)
  , Penalty (..)
  )
import qualified Language.Haskell.Exference.Core as Core
import qualified Language.Haskell.Exference.Core.Candidate as CoreCandidate
import qualified Language.Haskell.Exference.Core.ExferenceStats as CoreStats
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

-- | The result payload is owned by the search core.  This stable module only
-- supplies compatibility spellings for its historical public selectors.
type ExferenceCandidate = Core.ExferenceCandidate

type ExferenceResult = Core.ExferenceResult

type ExferenceCandidateMetrics = CoreStats.ExferenceStats

pattern ExferenceCandidateMetrics
  :: Int -> Penalty -> Int -> ExferenceCandidateMetrics
pattern ExferenceCandidateMetrics
  { exferenceCandidateSteps
  , exferenceCandidateComplexity
  , exferenceCandidateFinalQueueSize
  } = CoreStats.ExferenceStats
    { CoreStats.exference_steps = exferenceCandidateSteps
    , CoreStats.exference_complexityRating = exferenceCandidateComplexity
    , CoreStats.exference_finalSize = exferenceCandidateFinalQueueSize
    }

{-# COMPLETE ExferenceCandidateMetrics #-}

-- | Stable rendering hints and metrics attached to one checked candidate.
-- These names are a zero-cost view of the core-owned details record.
type ExferenceCandidateDetails = CoreCandidate.ExferenceCandidateDetails

pattern ExferenceCandidateDetails
  :: ExferenceCandidateMetrics
  -> Map.Map ExferenceLocal String
  -> Map.Map ExferenceTypeVariable String
  -> ExferenceCandidateDetails
pattern ExferenceCandidateDetails
  { exferenceCandidateStatistics
  , exferenceCandidateLocalNames
  , exferenceCandidateTypeVariableNames
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

pattern ExferenceBatchMetadata
  :: Map.Map Name Natural
  -> Natural
  -> Natural
  -> ExferenceBatchMetadata
pattern ExferenceBatchMetadata
  { exferenceBatchBindingUsages
  , exferenceBatchQueuePruned
  , exferenceBatchDepthPruned
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
-- environment, matching the @djinnSessionEnvironment@ projection.
exferenceSessionEnvironment :: ExferenceSession -> ExferenceEnvironment
exferenceSessionEnvironment = inventoryEnvironment
  . exferenceSessionInventory

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

runExferenceQuery
  :: ExferenceSession
  -> ExferenceRequest
  -> Either Diagnostic [ExferenceResult]
runExferenceQuery session request = do
  let query = exferenceRequestQuery request
      target = requestTarget query
      sharedGoal = requestContextualGoal request
      requestDiagnostic = withExferenceRequestProvenance request
  elaboratedGoal <- either
    (Left . requestDiagnostic . elaborationFailure)
    Right
    $ Session.elaborateSessionGoal session sharedGoal
  backendGoal <- either
    (Left . requestDiagnostic . shownErrorDiagnostic
      "DJEX_EXF_LOWER"
      "Exference rejected the shared query type"
    )
    Right
    $ fromSynthesisType elaboratedGoal
  let sourceHints = retargetExferenceSourceTypeVariableHints
        elaboratedGoal $ requestSourceTypeVariableHints request
  -- The direct result boundary owns exact target exclusion and result naming,
  -- so query validation and rigid-instantiation planning happen only once.
  let input = searchQuery backendGoal
        $ requestOptions query
      searchFailure failure
        | optionFailure failure = shownErrorDiagnostic
            "DJEX_EXF_OPTIONS" "invalid Exference search options" failure
        | otherwise = requestDiagnostic $ shownErrorDiagnostic
            "DJEX_EXF_QUERY" "Exference rejected the query" failure
  either
    (Left . searchFailure)
    Right
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

optionFailure :: ExferenceInputError -> Bool
optionFailure failure = case failure of
  InvalidMaxSteps{} -> True
  InvalidConstraintDeferralSteps{} -> True
  InvalidMaxQueueSize{} -> True
  InvalidMaxDepth{} -> True
  InvalidHeuristic{} -> True
  _ -> False

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
