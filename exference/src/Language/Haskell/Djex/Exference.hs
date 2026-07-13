-- | Checked Exference sessions behind Djex's shared query envelope.
--
-- Session construction seals the source inventory and computes Exference's
-- supported search projection once. Query execution performs every fallible
-- validation before returning a lazy sequence of total shared result batches.
module Language.Haskell.Djex.Exference
  ( ExferenceSession
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
  , ExferenceSessionLoadReport (..)
  , loadExferenceSession
  , loadExferenceSessionWithPolicy
  , exferenceSessionInventory
  , exferenceSessionOmissions
  , exferenceSessionDiagnostics
  , mkExferenceRequest
  , exferenceRequestQuery
  , parseExferenceRequest
  , runExferenceQuery
  , exferenceCandidateMetrics
  , exferenceResultBindingUsages
  , renderExferenceCandidateExpression
  , renderExferenceCandidateDefinition
  , renderExferenceResidualConstraints
  ) where

import Control.DeepSeq (NFData (rnf))
import Control.Monad.Trans.Except (runExceptT)
import Data.Bifunctor (first)
import Data.Functor.Identity (runIdentity)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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
import Language.Haskell.Exference.Core.Types
  ( HsType
  , TypeVarIndex
  , fromSynthesisType
  , sClassEnv_tclasses
  , toSynthesisType
  , toSynthesisName
  , showVar
  )
import Language.Haskell.Djex.Exference.Internal.Session
  ( ExferenceSession )
import qualified Language.Haskell.Djex.Exference.Internal.Session as Session
import Language.Haskell.Exference.EnvironmentParser
  ( LoadReport (..)
  , SourceEnvironment (..)
  , environmentFromPath
  , environmentLoadErrorDiagnostics
  , haskellSrcExtsParseMode
  )
import Language.Haskell.Exference.SimpleDict (defaultHeuristicsConfig)
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc
  ( parseTypeWithKinds )
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
  , diagnostic
  , withCode
  , withContext
  )
import Language.Haskell.Synthesis.Generated
  ( FunctionClause (FunctionClause)
  , Qualification (..)
  , RenderError (..)
  , RenderOptions (renderQualification)
  , defaultRenderOptions
  , validateDefinitionName
  )
import Language.Haskell.Synthesis.Inventory
  ( Inventory
  , inventoryKindAssumptions
  )
import Language.Haskell.Synthesis.Kind (Kind (ProperTypeKind))
import Language.Haskell.Synthesis.KindInference
  ( checkTypesKinds )
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

data ExferenceOmissionCapability
  = BindingIntroduction
  | DataElimination
  deriving (Eq, Ord, Show)

data ExferenceOmissionReason
  = UnsupportedNestedForall
  | ExcludedByPolicy
  deriving (Eq, Ord, Show)

-- | Environment capabilities disabled before a reusable session is sealed.
-- Names are structural and exact: excluding @Data.Function.fix@ never hides
-- an unrelated qualified binding whose occurrence also happens to be @fix@.
data ExferenceSessionPolicy = ExferenceSessionPolicy
  { exferenceExcludedBindings :: [Name]
  }
  deriving (Eq, Show)

defaultExferenceSessionPolicy :: ExferenceSessionPolicy
defaultExferenceSessionPolicy = ExferenceSessionPolicy
  { exferenceExcludedBindings = []
  }

data ExferenceOmission = ExferenceOmission
  { omittedName :: Name
  , omittedCapability :: ExferenceOmissionCapability
  , omittedReason :: ExferenceOmissionReason
  }
  deriving (Eq, Ord, Show)

-- | A fully sealed session or structured fatal diagnostics, paired with all
-- non-fatal source-loader and backend-projection diagnostics in production
-- order.  No parser-specific environment or error type crosses this stable
-- boundary.
data ExferenceSessionLoadReport = ExferenceSessionLoadReport
  { exferenceSessionLoadResult
      :: Either (NonEmpty Diagnostic) ExferenceSession
  , exferenceSessionLoadDiagnostics :: [Diagnostic]
  }

-- Keep the frontend spelling index private: it is meaningful only when paired
-- with the exact parsed goal and must be converted after explicit contexts are
-- merged, because that operation can change Exference's rigid-ID allocation.
data ExferenceRequest = ExferenceRequest
  { requestQuery :: QueryRequest ExferenceType ExferenceOptions
  , requestSourceTypeVariables :: TypeVarIndex
  }
  deriving (Eq, Show)

-- | Generated-expression binder identities used by Exference.
type ExferenceLocal = Int

-- | Shared flexible/rigid source-type variables used by Exference.
type ExferenceTypeVariable = SharedType.Variable ExferenceLocal

-- | Exference's checked type surface, expressed entirely in the neutral IR.
type ExferenceType = Type ExferenceTypeVariable

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

-- | Load a directory of source modules and ratings, validate its complete
-- inventory, and seal an Exference session with the default policy.
loadExferenceSession :: FilePath -> IO ExferenceSessionLoadReport
loadExferenceSession = loadExferenceSessionWithPolicy
  defaultExferenceSessionPolicy

-- | Policy-aware counterpart of 'loadExferenceSession'.  Session omission
-- diagnostics follow source-loader diagnostics, matching the order in which
-- the two phases run.
loadExferenceSessionWithPolicy
  :: ExferenceSessionPolicy
  -> FilePath
  -> IO ExferenceSessionLoadReport
loadExferenceSessionWithPolicy policy path = do
  LoadReport sourceResult sourceDiagnostics <- environmentFromPath path
  pure $ case sourceResult of
    Left failure -> ExferenceSessionLoadReport
      { exferenceSessionLoadResult = Left
          $ environmentLoadErrorDiagnostics failure
      , exferenceSessionLoadDiagnostics = sourceDiagnostics
      }
    Right checked -> case Session.sealExferenceSessionWithExclusions
        (exferenceExcludedBindings policy) checked of
      Left failure -> ExferenceSessionLoadReport
        { exferenceSessionLoadResult = Left
            $ NonEmpty.singleton failure
        , exferenceSessionLoadDiagnostics = sourceDiagnostics
        }
      Right session -> ExferenceSessionLoadReport
        { exferenceSessionLoadResult = Right session
        , exferenceSessionLoadDiagnostics = sourceDiagnostics
            ++ exferenceSessionDiagnostics session
        }

exferenceSessionInventory :: ExferenceSession -> ExferenceInventory
exferenceSessionInventory = Session.exferenceSessionInventory

exferenceSessionOmissions :: ExferenceSession -> [ExferenceOmission]
exferenceSessionOmissions = map projectOmission . Session.sessionOmissions

exferenceSessionDiagnostics :: ExferenceSession -> [Diagnostic]
exferenceSessionDiagnostics = map omissionDiagnostic
  . exferenceSessionOmissions

mkExferenceRequest
  :: QueryRequest ExferenceType ExferenceOptions
  -> Either Diagnostic ExferenceRequest
mkExferenceRequest query = do
  validateRequest query
  pure $ ExferenceRequest query Map.empty

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

parseExferenceRequest
  :: ExferenceSession
  -> ExferenceOptions
  -> Name
  -> FilePath
  -> String
  -> Either Diagnostic ExferenceRequest
parseExferenceRequest session options target sourceName source = do
  -- Preserve command-boundary precedence: an invalid output name is a usage
  -- error even when the source text is also malformed.
  validateTarget target
  let environment = Session.sessionSource session
      parsed = runIdentity $ runExceptT $ parseTypeWithKinds
        (inventoryKindAssumptions $ Session.exferenceSessionInventory session)
        (sClassEnv_tclasses $ sourceClasses environment)
        Nothing
        (sourceTypeNames environment)
        (Session.sessionTypeDeclarations session)
        (haskellSrcExtsParseMode sourceName)
        source
  -- The HSE compatibility frontend predates structured diagnostic codes.
  -- Seal every failure at this stable adapter boundary while preserving its
  -- exact message, source, and span.
  (backendType, sourceVariables) <- first
    (withCode "DJEX_EXF_PARSE") parsed
  sharedType <- either
    (Left . failureDiagnostic
      "DJEX_EXF_PARSE"
      "cannot project the parsed Exference type"
    )
    Right
    $ toSynthesisType backendType
  let query = QueryRequest
        { requestTarget = target
        , requestGoal = sharedType
        , requestContexts = []
        , requestOptions = options
        }
  validateRequestGoalAndContexts query
  pure $ ExferenceRequest query sourceVariables

runExferenceQuery
  :: ExferenceSession
  -> ExferenceRequest
  -> Either Diagnostic [ExferenceResult]
runExferenceQuery session request = do
  let query = requestQuery request
      target = requestTarget query
      sharedGoal = contextualGoal query
  either
    (Left . failureDiagnostic
      "DJEX_EXF_KIND"
      "Exference rejected the query kind"
    )
    Right
    $ checkTypesKinds
        (inventoryKindAssumptions $ Session.exferenceSessionInventory session)
        [(ProperTypeKind, sharedGoal)]
  backendGoal <- either
    (Left . failureDiagnostic
      "DJEX_EXF_LOWER"
      "cannot lower the shared query to Exference"
    )
    Right
    $ fromSynthesisType sharedGoal
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
  validateTarget $ requestTarget query
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

validateTarget :: Name -> Either Diagnostic ()
validateTarget target = case validateDefinitionName target of
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

projectOmission :: Session.SessionOmission -> ExferenceOmission
projectOmission omission = ExferenceOmission
  { omittedName = Session.sessionOmissionName omission
  , omittedCapability = case Session.sessionOmissionCapability omission of
      Session.BindingIntroduction -> BindingIntroduction
      Session.DataElimination -> DataElimination
  , omittedReason = case Session.sessionOmissionReason omission of
      Session.UnsupportedNestedForall -> UnsupportedNestedForall
      Session.ExcludedByPolicy -> ExcludedByPolicy
  }

omissionDiagnostic :: ExferenceOmission -> Diagnostic
omissionDiagnostic omission = withContext
  (renderCanonical $ omittedName omission)
  $ withCode code
  $ diagnostic severity message
 where
  (severity, code, message) = case omittedReason omission of
    UnsupportedNestedForall ->
      ( Warning
      , "DJEX_EXF_OMISSION"
      , "Exference omitted a source capability containing a nested forall"
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
failureDiagnostic code message detail = withContext (show detail)
  $ withCode code
  $ diagnostic Error message
