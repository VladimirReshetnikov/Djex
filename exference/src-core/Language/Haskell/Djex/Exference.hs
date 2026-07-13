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
  , mkExferenceRequestWithSourceInfo
  , validateExferenceTarget
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
  , toSynthesisName
  , showVar
  )
import Language.Haskell.Djex.Exference.Internal.Session
  ( ExferenceOmission (..)
  , ExferenceOmissionCapability (..)
  , ExferenceOmissionReason (..)
  , ExferenceSession
  )
import qualified Language.Haskell.Djex.Exference.Internal.Session as Session
import Language.Haskell.Exference.SimpleDict (defaultHeuristicsConfig)
import Language.Haskell.Synthesis.Candidate
  ( Candidate (Candidate)
  , candidateDetails
  , candidateOutput
  , candidateResidualConstraints
  , renderCandidateDefinition
  , renderCandidateExpression
  )
import Language.Haskell.Synthesis.Constraint
  ( constraintArguments )
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error, Info, Warning)
  , SourceSpan
  , contextualDiagnostic
  , withLocation
  )
import Language.Haskell.Synthesis.Generated
  ( FunctionClause (FunctionClause)
  , Qualification (..)
  , RenderError (..)
  , RenderOptions (renderQualification)
  , defaultRenderOptions
  , validateDefinitionName
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
  ( QueryEvidence (NoEvidence, ValidatedCandidates)
  , QueryRequest (..)
  , QueryResult (..)
  )
import Language.Haskell.Synthesis.Search
  ( SearchBatch (SearchBatch)
  , batchCandidates
  , batchMetadata
  , batchProgress
  )
import qualified Language.Haskell.Synthesis.Type as SharedType
import Language.Haskell.Synthesis.Type
  ( Type
  , Variable (FlexibleVariable, RigidVariable)
  )
import qualified Language.Haskell.Synthesis.TypeRender as SharedRender
import Language.Haskell.Synthesis.TypeSynonym
  ( TypeElaborationError (..)
  , elaborateType
  )

data ExferenceOptions = ExferenceOptions
  { exferenceAllowUnused :: Bool
  , exferenceAllowResidualConstraints :: Bool
  , exferenceConstraintDeferralSteps :: Int
  , exferenceMultiConstructorPatterns :: Bool
  , exferenceMaximumSteps :: Int
  , exferenceMaximumQueueSize :: Maybe Int
  , exferenceMaximumDepth :: Maybe Penalty
  , exferenceHeuristics :: ExferenceHeuristicsConfig
  }
  deriving (Eq, Show)

defaultExferenceOptions :: ExferenceOptions
defaultExferenceOptions = ExferenceOptions
  { exferenceAllowUnused = False
  , exferenceAllowResidualConstraints = False
  , exferenceConstraintDeferralSteps = 8192
  , exferenceMultiConstructorPatterns = False
  , exferenceMaximumSteps = 65536
  , exferenceMaximumQueueSize = Just 8192
  , exferenceMaximumDepth = Nothing
  , exferenceHeuristics = defaultHeuristicsConfig
  }

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

-- Keep the frontend caches private: they are meaningful only when paired with
-- the exact parsed goal. In particular, the spelling index must be converted
-- after explicit contexts are merged, because that operation can change
-- Exference's rigid-ID allocation.
data ExferenceRequest = ExferenceRequest
  { requestQuery :: QueryRequest ExferenceType ExferenceOptions
  , requestSourceTypeVariables :: Map.Map String ExferenceLocal
  , requestSourceLocation :: Maybe (FilePath, SourceSpan)
  }

-- Source spellings and locations are deterministic presentation caches, not
-- part of the stable request value. This matches Djinn's opaque request: both
-- backends expose equality and display solely through the neutral query.
instance Eq ExferenceRequest where
  left == right = requestQuery left == requestQuery right

instance Show ExferenceRequest where
  showsPrec precedence = showsPrec precedence . requestQuery

-- | Generated-expression binder identities used by Exference.
type ExferenceLocal = Int

-- | Shared flexible/rigid source-type variables used by Exference.
type ExferenceTypeVariable = SharedType.Variable ExferenceLocal

-- | Exference's checked type surface, expressed entirely in the neutral IR.
type ExferenceType = Type ExferenceTypeVariable

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

-- | Stable operational metadata for one Exference result batch.
data ExferenceBatchMetadata = ExferenceBatchMetadata
  { exferenceBatchBindingUsages :: Map.Map Name Int
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

mkExferenceRequest
  :: QueryRequest ExferenceType ExferenceOptions
  -> Either Diagnostic ExferenceRequest
mkExferenceRequest = mkExferenceRequestWithSourceInfo Map.empty Nothing

-- | Construct a request while retaining source-frontend metadata used for
-- stable variable spelling and diagnostics. Parser-neutral clients should use
-- 'mkExferenceRequest'; source frontends can preserve their richer boundary
-- information without exposing parser-specific types to the search core.
mkExferenceRequestWithSourceInfo
  :: Map.Map String ExferenceLocal
  -> Maybe (FilePath, SourceSpan)
  -> QueryRequest ExferenceType ExferenceOptions
  -> Either Diagnostic ExferenceRequest
mkExferenceRequestWithSourceInfo sourceVariables sourceLocation query = do
  validateRequest query
  pure $ ExferenceRequest query sourceVariables sourceLocation

exferenceRequestQuery
  :: ExferenceRequest
  -> QueryRequest ExferenceType ExferenceOptions
exferenceRequestQuery = requestQuery

exferenceCandidateMetrics
  :: ExferenceCandidate
  -> ExferenceCandidateMetrics
exferenceCandidateMetrics = exferenceCandidateStatistics . candidateDetails

-- | Cumulative nominal source-binding usage at this result batch.
exferenceResultBindingUsages :: ExferenceResult -> Map.Map Name Int
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
  (defaultRenderOptions preferred)
    {renderQualification = qualification}
 where
  hints = exferenceCandidateLocalNames $ candidateDetails candidate
  preferred local = Map.findWithDefault (showVar local) local hints

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
  let query = requestQuery request
      target = requestTarget query
      sharedGoal = contextualGoal query
  elaboratedGoal <- either
    (Left . attachRequestSource request . elaborationFailure)
    Right
    $ elaborateType freshSynthesisVariable
        (Session.sessionTypeSynonyms session)
        ProperTypeKind
        sharedGoal
  backendGoal <- either
    (Left . failureDiagnostic
      "DJEX_EXF_LOWER"
      "cannot lower the shared query to Exference"
    )
    Right
    $ fromSynthesisType elaboratedGoal
  let input = searchQuery (Just target) backendGoal
        $ requestOptions query
      searchFailure failure = failureDiagnostic
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

validateRequest
  :: QueryRequest ExferenceType ExferenceOptions
  -> Either Diagnostic ()
validateRequest query = do
  validateExferenceTarget $ requestTarget query
  validateRequestGoalAndContexts query

validateRequestGoalAndContexts
  :: QueryRequest ExferenceType ExferenceOptions
  -> Either Diagnostic ()
validateRequestGoalAndContexts query = do
  either
    (Left . failureDiagnostic
      "DJEX_EXF_REQUEST"
      "invalid shared Exference request"
    )
    Right
    $ SharedType.validateType $ contextualGoal query
  let goalVariables = inScopeContextVariables $ requestGoal query
      contextVariables = Set.unions
        [ SharedType.freeVariables argument
        | constraint <- requestContexts query
        , argument <- constraintArguments constraint
        ]
      extraneous = Set.toAscList $ contextVariables Set.\\ goalVariables
  case extraneous of
    [] -> Right ()
    _ -> Left $ failureDiagnostic
      "DJEX_EXF_REQUEST"
      "explicit Exference contexts contain variables not in scope"
      extraneous

-- Explicit contexts are inserted beneath only the leading prenex chain.
-- Free goal variables remain usable there, as do binders from that chain;
-- a binder below an arrow, tuple, or application is not in context scope.
inScopeContextVariables
  :: ExferenceType
  -> Set.Set ExferenceTypeVariable
inScopeContextVariables goal = SharedType.freeVariables goal
  `Set.union` leadingForallVariables goal
 where
  leadingForallVariables (SharedType.ForallType variables _ body) =
    Set.fromList variables `Set.union` leadingForallVariables body
  leadingForallVariables _ = Set.empty

-- | Validate the source-level name of an Exference result definition.
-- Frontends may call this before parsing to give command-usage errors stable
-- precedence over malformed source text.
validateExferenceTarget :: Name -> Either Diagnostic ()
validateExferenceTarget target = case validateDefinitionName target of
  Right () -> Right ()
  Left failure -> Left $ failureDiagnostic
    "DJEX_EXF_TARGET"
    "Exference targets must be unqualified value identifiers or operators"
    (renderCanonical target, failure)

contextualGoal
  :: QueryRequest ExferenceType options
  -> ExferenceType
contextualGoal query
  | null contexts = requestGoal query
  | otherwise = insertUnderLeadingForalls $ requestGoal query
 where
  contexts = requestContexts query

  -- Explicit contexts are scoped by the complete leading quantifier chain.
  -- Attaching them to only the first forall makes a variable bound by a later
  -- leading forall free in the outer context, then binds the same identity a
  -- second time below it.
  insertUnderLeadingForalls (SharedType.ForallType variables embedded body)
    | SharedType.ForallType{} <- body = SharedType.ForallType
        variables embedded $ insertUnderLeadingForalls body
    | otherwise = SharedType.ForallType
        variables (contexts ++ embedded) body
  insertUnderLeadingForalls goal =
    SharedType.ForallType [] contexts goal

elaborationFailure
  :: TypeElaborationError ExferenceTypeVariable
  -> Diagnostic
elaborationFailure failure = case failure of
  IllKindedType _ _ -> failureDiagnostic
    "DJEX_EXF_KIND"
    "Exference rejected the query kind"
    failure
  SynonymExpansionFailed _ -> failureDiagnostic
    "DJEX_EXF_SYNONYM"
    "Exference could not expand the query's type synonyms"
    failure
  InvalidElaborationType _ _ -> failureDiagnostic
    "DJEX_EXF_QUERY"
    "Exference rejected the shared query type"
    failure

attachRequestSource :: ExferenceRequest -> Diagnostic -> Diagnostic
attachRequestSource request value = case requestSourceLocation request of
  Nothing -> value
  Just (sourceName, sourceSpan) ->
    withLocation sourceName sourceSpan value

optionFailure :: ExferenceInputError -> Bool
optionFailure failure = case failure of
  InvalidMaxSteps{} -> True
  InvalidConstraintDeferralSteps{} -> True
  InvalidMaxQueueSize{} -> True
  InvalidMaxDepth{} -> True
  InvalidHeuristic{} -> True
  _ -> False

resultBatch
  :: Name
  -> ExferenceGeneratedSearchBatch
  -> ExferenceResult
resultBatch target batch = QueryResult evidence $ SearchBatch
  (batchProgress batch)
  (projectBatchMetadata $ batchMetadata batch)
  (map (projectCandidate target) $ batchCandidates batch)
 where
  evidence
    | null $ batchCandidates batch = NoEvidence
    | otherwise = ValidatedCandidates

projectBatchMetadata
  :: CoreStats.ExferenceBatchMetadata
  -> ExferenceBatchMetadata
projectBatchMetadata metadata = ExferenceBatchMetadata
  { exferenceBatchBindingUsages = Map.fromList
      [ (toSynthesisName backendName, count)
      | (backendName, count) <- Map.toList
          $ CoreStats.exferenceBindingUsages metadata
      ]
  , exferenceBatchQueuePruned = CoreStats.exferenceQueuePruned metadata
  , exferenceBatchDepthPruned = CoreStats.exferenceDepthPruned metadata
  }

projectCandidate
  :: Name
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

failureDiagnostic
  :: Show detail
  => String
  -> String
  -> detail
  -> Diagnostic
failureDiagnostic code message detail =
  contextualDiagnostic Error code message (show detail)
