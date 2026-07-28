{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MonadComprehensions #-}

module Language.Haskell.Exference.Core.Internal.Exference
  ( findExpressions
  , findExpressionsWithAllocators
  , findQueryResultsInEnvironmentEither
  , findQueryResultsInEnvironmentWithCheckedOptions
  , findQueryResultsWithAllocators
  , queryProjectionStrictnessForTesting
  , prepareExferenceInput
  , prepareExferenceQuery
  , ExferenceInput (..)
  , ExferenceEnvironment
  , ExferenceQuery (..)
  , CheckedExferenceOptions
  , ExferenceOutputElement
  , ExferenceChunkElement (..)
  , ExferenceBatchMetadata (..)
  , ExferenceCandidate
  , ExferenceResult
  , SearchCompletion (..)
  , SearchStatus (..)
  , constraintsRelaxedAtStep
  , mergeQueueWithCapacity
  , naturalPruningReasons
  , projectCompatibilityBindingUsages
  , typeComplexity
  , ExferenceInputError (..)
  , isExferenceOptionError
  , mkExferenceEnvironment
  , checkExferenceOptions
  , validateExferenceOptions
  , validateExferenceQuery
  , validateExferenceInput
  )
where



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.Core.Expression
import Language.Haskell.Exference.Core.Internal.Candidate
  ( ExferenceCandidate
  , ExferenceSourceTypeVariableHintError
  , ExferenceSourceTypeVariableHints
  , ExferenceTypeVariableHints
  , projectValidatedCandidate
  , typeVariableHintsWithPlan
  )
import Language.Haskell.Exference.Core.ExpressionCheck
import Language.Haskell.Exference.Core.Score
import Language.Haskell.Exference.Core.ExferenceStats
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.Core.RigidInstantiation
import Language.Haskell.Exference.Core.Internal.FlexibleIds
import Language.Haskell.Exference.Core.Internal.VariableSupply
import Language.Haskell.Exference.Core.Unify
import Language.Haskell.Exference.Core.ConstraintSolver
import Language.Haskell.Exference.Core.Internal.ExferenceNode
import Language.Haskell.Exference.Core.Internal.ExferenceNodeBuilder
import Language.Haskell.Exference.Core.Internal.Polytype
import Language.Haskell.Exference.Core.Internal.SearchControl
import Language.Haskell.Exference.Core.Internal.Options
  ( ExferenceHeuristicsConfig (..)
  , ExferenceOptions (..)
  , heuristicFields
  )
import qualified Language.Haskell.Synthesis.Count as SharedCount
import qualified Language.Haskell.Synthesis.Search as SharedSearch
import qualified Language.Haskell.Synthesis.Generated as SharedGenerated
import qualified Language.Haskell.Synthesis.Name as SynthesisName
import qualified Language.Haskell.Synthesis.Candidate as SharedCandidate
import qualified Language.Haskell.Synthesis.Query as SharedQuery
import qualified Language.Haskell.Synthesis.Type as SharedType

import qualified Data.PQueue.Prio.Max as Q
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import qualified Data.List as L

import Data.Maybe ( maybeToList, listToMaybe )
import Control.Monad ( mzero, forM )
import Control.Applicative ( (<|>) )
import Data.List ( find, partition, sortBy, unfoldr )
import Data.Monoid ( Any(..) )
import Data.Foldable ( traverse_ )
import Data.List.NonEmpty (NonEmpty ((:|)))
import Numeric.Natural (Natural)
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State.Lazy
  ( StateT(..), gets, modify, state
  , execStateT, runStateT
  )

data ExferenceInput = ExferenceInput
  { input_goalType    :: HsType                 -- ^ try to find a expression
                                                -- of this type
  , input_envFuncs    :: [FunctionBinding]      -- ^ the list of functions
                                                -- that may be used
  , input_envDeconsS  :: [DeconstructorBinding] -- ^ the list of deconstructors
                                                -- that may be used for pattern
                                                -- matching
  , input_envClasses  :: StaticClassEnv
  , input_allowUnused :: Bool                   -- ^ if false, forbid solutions
                                                -- where any bind is unused
  , input_allowConstraints :: Bool              -- ^ if true, allow solutions
                                                -- that have unproven
                                                -- constraints remaining.
  , input_allowConstraintsStopStep :: Int       -- ^ stop ignoring
                                                -- tc-constraints after this
                                                -- step to have some chance to
                                                -- find some solution.
  , input_multiPM     :: Bool                   -- ^ pattern match on
                                                -- multi-constructor data types
                                                -- if true. serverly increases
                                                -- search space (decreases
                                                -- performance).
  , input_maxSteps    :: Int                    -- ^ maximum processed nodes
  , input_maxQueueSize :: Maybe Int             -- ^ keep the best N queued nodes
  , input_maxDepth    :: Maybe Penalty          -- ^ optional heuristic-depth cap
  , input_heuristicsConfig :: ExferenceHeuristicsConfig
  }
  deriving (Show)

-- | A finite search environment whose names, types, constraints, ratings, and
-- generated syntax have been checked once.  The constructor remains private:
-- removing a binding for one query is safe, but adding or replacing one must
-- cross 'mkExferenceEnvironment' again.
data ExferenceEnvironment = ExferenceEnvironment
  !EnvDictionary
  !RigidInstantiationContext

-- | The query-varying half of an Exference search input.
--
-- Exact shared names in 'queryExcludedBindings' are unavailable to both proof
-- search and the independent result checker.  This lets a frontend prevent a
-- generated definition from accidentally referring to the binding it shadows
-- without revalidating the otherwise unchanged environment.
data ExferenceQuery = ExferenceQuery
  { queryGoalType :: HsType
  , queryExcludedBindings :: S.Set SynthesisName.Name
  , querySearchOptions :: ExferenceOptions
      -- ^ Exact grouped search policy retained from the checked request.
  }
  deriving (Eq, Show)

-- | Search controls that crossed their complete validation boundary.  The
-- constructor stays private even inside the package: checked adapters can
-- preserve option-before-elaboration diagnostics without giving preparation
-- a second authority over the same fields.
data CheckedExferenceOptions = CheckedExferenceOptions !ExferenceOptions

-- | A query sealed together with the exact environment and rigid-variable
-- plan against which it was validated.  Keeping this artifact private makes
-- it impossible for the raw lazy engine to recompute fallible derived state,
-- or to run a checked query against a different environment by mistake.
data CheckedExferenceQuery = CheckedExferenceQuery
  !ExferenceEnvironment
  !ExferenceQuery
  !RigidInstantiationPlan

data ExferenceInputError
  = NestedForallInGoal HsType
  | NestedForallInBinding QualifiedName HsType
  | NestedForallInDeconstructor HsType
  | NestedForallInConstraint ConstraintSite HsConstraint
  | InvalidInputType HsType SynthesisTypeError
  | DeconstructorInputWithoutNominalHead HsType
  | UnsupportedDeconstructorTypeHead QualifiedName
  | UnboundDeconstructorFieldVariables
      QualifiedName -- ^ Nominal datatype head.
      QualifiedName -- ^ Constructor whose fields escape the parameter scope.
      [TVarId]      -- ^ Escaping flexible IDs, in ascending order.
  | InvalidGeneratedBinding QualifiedName SharedGenerated.RenderError
  | InvalidGeneratedConstructor QualifiedName SharedGenerated.RenderError
  | DuplicateFunctionNames [QualifiedName]
  | DuplicateDeconstructorNames [QualifiedName]
  | DuplicateConstructorNames [QualifiedName]
  | InvalidClassConstraint ClassEnvError
  | InvalidMaxSteps Int
  | InvalidConstraintDeferralSteps Int
  | InvalidMaxQueueSize Int
  | InvalidMaxDepth Penalty
  | InvalidHeuristic String Penalty
  | RigidIdentifierExhaustion RigidInstantiationError
  | InvalidSourceTypeVariableHints ExferenceSourceTypeVariableHintError
  deriving (Eq)

-- | Whether a rejected input failed on its search options rather than the
-- query or environment. Living beside the type with a complete case, this
-- classification cannot silently drift when a constructor is added; the
-- stable adapter uses it to keep option failures source-free, mirroring
-- Djinn's structural options/query error split.
isExferenceOptionError :: ExferenceInputError -> Bool
isExferenceOptionError failure = case failure of
  NestedForallInGoal{} -> False
  NestedForallInBinding{} -> False
  NestedForallInDeconstructor{} -> False
  NestedForallInConstraint{} -> False
  InvalidInputType{} -> False
  DeconstructorInputWithoutNominalHead{} -> False
  UnsupportedDeconstructorTypeHead{} -> False
  UnboundDeconstructorFieldVariables{} -> False
  InvalidGeneratedBinding{} -> False
  InvalidGeneratedConstructor{} -> False
  DuplicateFunctionNames{} -> False
  DuplicateDeconstructorNames{} -> False
  DuplicateConstructorNames{} -> False
  InvalidClassConstraint{} -> False
  InvalidMaxSteps{} -> True
  InvalidConstraintDeferralSteps{} -> True
  InvalidMaxQueueSize{} -> True
  InvalidMaxDepth{} -> True
  InvalidHeuristic{} -> True
  RigidIdentifierExhaustion{} -> False
  InvalidSourceTypeVariableHints{} -> False

instance Show ExferenceInputError where
  showsPrec precedence failure = showParen (precedence > 10)
    $ showString $ renderExferenceInputError failure

renderExferenceInputError :: ExferenceInputError -> String
renderExferenceInputError failure = case failure of
  NestedForallInGoal typeExpression ->
    constructor "NestedForallInGoal" [sourceType typeExpression]
  NestedForallInBinding name typeExpression -> constructor
    "NestedForallInBinding" [show name, sourceType typeExpression]
  NestedForallInDeconstructor typeExpression -> constructor
    "NestedForallInDeconstructor" [sourceType typeExpression]
  NestedForallInConstraint site constraint -> constructor
    "NestedForallInConstraint" [show site, sourceConstraint constraint]
  InvalidInputType typeExpression typeFailure -> constructor
    "InvalidInputType" [sourceType typeExpression, show typeFailure]
  DeconstructorInputWithoutNominalHead typeExpression -> constructor
    "DeconstructorInputWithoutNominalHead" [sourceType typeExpression]
  UnsupportedDeconstructorTypeHead name -> constructor
    "UnsupportedDeconstructorTypeHead" [show name]
  UnboundDeconstructorFieldVariables typeName fieldConstructor variables ->
    constructor "UnboundDeconstructorFieldVariables"
      [show typeName, show fieldConstructor, show variables]
  InvalidGeneratedBinding name renderFailure -> constructor
    "InvalidGeneratedBinding" [show name, show renderFailure]
  InvalidGeneratedConstructor name renderFailure -> constructor
    "InvalidGeneratedConstructor" [show name, show renderFailure]
  DuplicateFunctionNames names ->
    constructor "DuplicateFunctionNames" [show names]
  DuplicateDeconstructorNames names ->
    constructor "DuplicateDeconstructorNames" [show names]
  DuplicateConstructorNames names ->
    constructor "DuplicateConstructorNames" [show names]
  InvalidClassConstraint classFailure ->
    constructor "InvalidClassConstraint" [show classFailure]
  InvalidMaxSteps maximumSteps ->
    constructor "InvalidMaxSteps" [show maximumSteps]
  InvalidConstraintDeferralSteps steps ->
    constructor "InvalidConstraintDeferralSteps" [show steps]
  InvalidMaxQueueSize maximumSize ->
    constructor "InvalidMaxQueueSize" [show maximumSize]
  InvalidMaxDepth maximumDepth ->
    constructor "InvalidMaxDepth" [show maximumDepth]
  InvalidHeuristic fieldName value ->
    constructor "InvalidHeuristic" [show fieldName, show value]
  RigidIdentifierExhaustion rigidFailure ->
    constructor "RigidIdentifierExhaustion" [show rigidFailure]
  InvalidSourceTypeVariableHints hintFailure ->
    constructor "InvalidSourceTypeVariableHints" [show hintFailure]
 where
  constructor name fields = unwords $ name : fields
  sourceType typeExpression = "(" ++ showHsType M.empty typeExpression ++ ")"
  sourceConstraint constraint =
    "(" ++ showHsConstraint M.empty constraint ++ ")"

type ExferenceOutputElement = (Expression, [HsConstraint], ExferenceStats)
data SearchCompletion
  = SearchRunning
    -- ^ Retained search nodes remain after this chunk.
  | SearchExhausted
    -- ^ No retained nodes remain and no node was discarded by a configured
    -- bound. Failure is conclusive for this search calculus and environment.
  | SearchStepLimitReached
    -- ^ Retained nodes remain, but the configured step budget is spent.
  | SearchPruned
    -- ^ No retained nodes remain, but queue or depth bounds discarded nodes;
    -- absence of an answer is therefore not a non-inhabitation result.
  | SearchIdentifierSpaceExhausted
    -- ^ One or more branches could not be represented in a finite internal
    -- identifier namespace, so exhaustion of the retained queue is not
    -- conclusive.
  deriving (Eq, Show)

data SearchStatus = SearchStatus
  { searchCompletion :: SearchCompletion
  -- These historical counters cannot represent every exact engine total.
  -- Engine-produced values saturate; modern batch metadata remains lossless.
  , searchQueuePruned :: Int
  , searchDepthPruned :: Int
  }
  deriving (Eq, Show)

data ExferenceChunkElement = ExferenceChunkElement
  { chunkStatus :: SearchStatus
  -- Historical compatibility metadata remains machine-sized. Engine-produced
  -- counts saturate here. Canonical results retain exact totals independently.
  , chunkBindingUsages :: M.Map QualifiedName Int
  , chunkElements :: [ExferenceOutputElement]
  }

-- Only independently checked search output can inhabit the engine batch.
-- Keep the constructor private so the total canonical projector cannot be
-- applied accidentally before type, scope, completeness, and syntax checks.
data ValidatedEngineCandidate = ValidatedEngineCandidate
  Expression [HsConstraint] ExferenceStats

-- The engine stores the lossless shared batch natively. Historical chunks are
-- a saturating compatibility projection, never an intermediate authority for
-- canonical query results.
type EngineBatch =
  SharedSearch.SearchBatch ExferenceBatchMetadata ValidatedEngineCandidate

-- | One Exference engine batch in the backend-neutral query envelope.
type ExferenceResult =
  SharedQuery.QueryResult ExferenceBatchMetadata ExferenceCandidate

-- | A search node paired with the next goal already removed from its goal
-- sequence. The queue can therefore contain only work that 'stateStep' may
-- execute; solved nodes never have a representation here.
data ScheduledNode = ScheduledNode !TGoal SearchNode

type RatedNodes = Q.MaxPQueue Priority ScheduledNode
data FindExpressionsState = FindExpressionsState
  { findSteps :: Int -- number of steps already performed
  , findQueuePruned :: !Natural
  , findDepthPruned :: !Natural
  , findIdentifierSpaceExhausted :: Bool
  , findBindingUsages :: BindingUsages
  , findQueue :: RatedNodes
  }

-- Keep the search trace productive by retaining the historical lazy state
-- transformer. An empty priority queue terminates the unfold through Maybe.
popBestNode :: StateT FindExpressionsState Maybe ScheduledNode
popBestNode = StateT $ \searchState -> do
  (node, remaining) <- Q.maxView $ findQueue searchState
  return (node, searchState { findQueue = remaining })

-- Return the old step number: search budgets and statistics have always used
-- post-increment semantics here.
advanceStep :: StateT FindExpressionsState Maybe Int
advanceStep = state $ \searchState ->
  ( findSteps searchState
  , searchState { findSteps = findSteps searchState + 1 }
  )

recordBindingUsage :: SearchNode -> FindExpressionsState -> FindExpressionsState
recordBindingUsage node searchState = case nodeLastStepBinding node of
  Nothing -> searchState
  Just binding -> searchState
    { findBindingUsages = M.insertWith (+) binding 1
        $ findBindingUsages searchState
    }

-- Entry-point and main function of the algorithm.
-- Takes input, produces list of outputs. Output is basically a
-- [[Solution]], plus some statistics and stuff.
-- Nested list to allow executing n steps even when no solutions are found
-- (e.g. you take 1000, and get only []'s).
--
-- Basic implementation idea: We traverse a search tree. A step (`stateStep`
-- function) evaluates one node, and returns
-- a) new search nodes b) potential solutions.
-- findExpressions does the following stuff:
--   - determine what searchnode to use next (using a priority queue)
--   - call stateStep repeatedly
--   - convert stuff
--   - consider some special abort conditions
findEngineBatchesWith
  :: SearchAllocators
  -> CheckedExferenceQuery
  -> [EngineBatch]
findEngineBatchesWith allocators
    (CheckedExferenceQuery
      (ExferenceEnvironment EnvDictionary
      { environmentFunctions = allFunctions
      , environmentDeconstructors = deconss'
      , environmentClasses = sClassEnv
      } _)
      ExferenceQuery
      { queryGoalType = rawType
      , queryExcludedBindings = excludedBindings
      , querySearchOptions = ExferenceOptions
          { exferenceAllowUnused = allowUnused
          , exferenceAllowResidualConstraints = allowConstraints
          , exferenceConstraintDeferralSteps = allowConstraintsStopStep
          , exferenceMultiConstructorPatterns = multiPM
          , exferenceMaximumSteps = maxSteps
          , exferenceMaximumQueueSize = maxQueueSize
          , exferenceMaximumDepth = maxDepth
          , exferenceHeuristics = heuristics
          }
      }
      rigidPlan) =
  unfoldr helper rootFindExpressionState
 where
  -- Removing an already checked binding cannot invalidate the environment.
  -- Use the same exact projection for search and independent result checking,
  -- otherwise a generated definition could regain the binding it shadows.
  funcs = filter bindingAvailable allFunctions
  bindingAvailable binding = functionName binding
    `S.notMember` excludedBindings

  rootClassEnvironment = mkQueryClassEnv sClassEnv []
  preparedCheckContext = prepareExpressionCheckContext
    rigidPlan rootClassEnvironment funcs deconss' t

  rootFindExpressionState = FindExpressionsState
    { findSteps = 0
    , findQueuePruned = 0
    , findDepthPruned = 0
    , findIdentifierSpaceExhausted = False
    , findBindingUsages = M.empty
    , findQueue = Q.singleton 0 $ ScheduledNode rootGoal rootSearchNode
    }
  t = forallify rawType
  rootGoal = TGoal (VarBinding 0 t) initialScopeId OpenLeadingForalls
  rootSearchNode = SearchNode
    { nodeGoals           = Seq.empty
    , nodeConstraintGoals = []
    , nodeProvidedScopes  = initialScopes
    , nodeVarUses         = IntMap.empty
    , nodeFunctions       = funcs
    , nodeDeconstructors  = deconss'
    , nodeQueryClassEnv   = rootClassEnvironment
    , nodeExpression      = ExpHole 0
      -- The root goal and expression already own hole 0.
    , nodeNextVarId       = 1
    , nodeFlexibleIds     = supplyFromIdentifiers
        $ IntSet.toAscList $ flexibleIdentifiers t
    , nodeRigidInstantiations = rigidInstantiations rigidPlan
    , nodeDepth           = 0.0
    , nodeLastStepBinding = Nothing
    }
  transformSolutions :: [SearchNode] -> FindExpressionsState -> EngineBatch
  transformSolutions potentialSolutions searchState = SharedSearch.SearchBatch
      progress
      (ExferenceBatchMetadata
        { exferenceBindingUsages = newBindingUsages
        , exferenceQueuePruned = totalQueuePruned
        , exferenceDepthPruned = totalDepthPruned
        })
      [ ValidatedEngineCandidate
          e
          remainingConstraints
          (ExferenceStats n' d
            $ SharedCount.saturatingNaturalToInt
            $ queueSizeNatural newNodes)
      | solution <- potentialSolutions
      , let contxt = nodeQueryClassEnv solution
      , remainingConstraints <- maybeToList
                              $ filterUnresolved contxt
                              $ nodeConstraintGoals solution
        -- if allowConstraints, unresolved constraints are allowed;
        -- otherwise we discard this solution.
      , allowConstraints || null remainingConstraints
      , let unusedVarCount = getUnusedVarCount solution
        -- similarly:
        -- if allowUnused, there may be unused variables in the
        -- output. Otherwise the solution is discarded.
      , allowUnused || unusedVarCount==0
      , rawExpression <- [nodeExpression solution]
      , e <- maybeToList $ checkedSimplification
          remainingConstraints rawExpression
      , let d = normalizePenalty $ sumScores
              [ nodeDepth solution
              , multiplyScore (heuristics_unusedVar heuristics)
                  $ fromIntegral unusedVarCount
              , multiplyScore (heuristics_solutionLength heuristics)
                  $ fromIntegral (SharedGenerated.expressionSizeNatural
                      $ toGeneratedExpression e)
              ]
      ]
    where
      n' = findSteps searchState
      totalQueuePruned = findQueuePruned searchState
      totalDepthPruned = findDepthPruned searchState
      identifierSpaceExhausted = findIdentifierSpaceExhausted searchState
      newBindingUsages = findBindingUsages searchState
      newNodes = findQueue searchState
      progress
        | Q.null newNodes
        , reason : remaining <- truncationReasons =
            SharedSearch.Completed $ SharedSearch.Truncated
              $ reason :| remaining
        | Q.null newNodes =
            SharedSearch.Completed SharedSearch.Finished
        | n' >= maxSteps =
            SharedSearch.Completed $ SharedSearch.Truncated
              $ SharedSearch.StepLimitReached :| truncationReasons
        | otherwise = SharedSearch.Continuing
      truncationReasons =
        [ SharedSearch.IdentifierSpaceExhausted
        | identifierSpaceExhausted
        ] ++
        naturalPruningReasons totalQueuePruned totalDepthPruned
      -- Validate the exact tree returned to callers. The independent checker
      -- owns type reconstruction, local scope, and generated syntax together;
      -- repeating one of those tree traversals here would let the two result
      -- boundaries drift again. The raw term remains a safe fallback, but it
      -- too must pass that complete checker independently.
      checkedSimplification constraints rawExpression = case
          preparedCheckContext of
        Left _ -> Nothing
        Right context -> firstChecked context candidates
       where
        simplified = simplifyExpression rawExpression
        candidates
          | simplified == rawExpression = [rawExpression]
          | otherwise = [simplified, rawExpression]
        firstChecked _ [] = Nothing
        firstChecked context (candidate : remainingCandidates) =
          case checkExpressionInContext context constraints candidate of
            Right () -> Just candidate
            Left _ -> firstChecked context remainingCandidates
  helper :: FindExpressionsState -> Maybe (EngineBatch, FindExpressionsState)
  helper searchState | findSteps searchState >= maxSteps = Nothing
  helper searchState = runStateT (do
    ScheduledNode nextGoal s <- popBestNode
    n' <- advanceStep
    let
      -- actual work happens in stateStep
      -- Constraint checks are deliberately relaxed only during the configured
      -- warm-up window. Afterwards unresolved constraints are allowed solely
      -- when the caller explicitly requested constrained results.
      relaxConstraints = constraintsRelaxedAtStep
        allowConstraints allowConstraintsStopStep n'
      stepResults = runSearchBranches $ (`execStateT` s)
        $ stateStep allocators
                    multiPM
                    relaxConstraints
                    heuristics
                    nextGoal
      (rNodes, stepIdentifierSpaceExhausted) =
        foldr collectStepResult ([], False) stepResults
      (withinDepth, tooDeep) = partition depthAllowed rNodes
      (potentialSolutions, futures) = classifySearchNodes withinDepth
      ratedNew =
        [ ( normalizePriority $ addPriority
              (rateNode heuristics $ restoreScheduledNode newS)
              (Priority $ 4.5 * f (fromIntegral n'))
          , newS)
        | newS <- futures
        , let f :: Double -> Double
              f x | x > 900 = 0.0
                  | otherwise = let k = 1.111e-3*x
                                 in 1 + 2*k**3 - 3*k**2
        ]
      depthAllowed node = maybe True (nodeDepth node <=) maxDepth
      collectStepResult result (nodes, exhausted) = case result of
        Right node -> (node : nodes, exhausted)
        Left BranchIdentifierSpaceExhausted -> (nodes, True)
    -- Account for generated branches rather than only nodes eventually popped
    -- from the queue.  This includes applications that immediately solve the
    -- current goal and branches discarded by the configured bounds.
    traverse_ (modify . recordBindingUsage) rNodes
    let !depthDiscarded = SharedCount.naturalLength tooDeep
    modify $ \current -> current
      { findDepthPruned = findDepthPruned current + depthDiscarded
      , findIdentifierSpaceExhausted =
          findIdentifierSpaceExhausted current
            || stepIdentifierSpaceExhausted
      }
    queued <- gets findQueue
    let (retained, queueDiscarded) = mergeQueueWithCapacity
          maximumPQueueSize maxQueueSize queued ratedNew
    modify $ \current -> current
      { findQueue = retained
      , findQueuePruned = findQueuePruned current + queueDiscarded
      }
    gets $ transformSolutions potentialSolutions) searchState

-- Partition generated nodes while extracting the next goal from every future
-- before it can enter the priority queue. This preserves the historical node
-- order of 'partition' but makes the scheduler invariant structural.
classifySearchNodes :: [SearchNode] -> ([SearchNode], [ScheduledNode])
classifySearchNodes = foldr classify ([], [])
 where
  classify node (solutions, scheduled) = case Seq.viewl $ nodeGoals node of
    Seq.EmptyL -> (node : solutions, scheduled)
    nextGoal Seq.:< remainingGoals ->
      ( solutions
      , ScheduledNode nextGoal (node {nodeGoals = remainingGoals}) : scheduled
      )

-- Rating observes the complete pending goal sequence. Only execution consumes
-- the extracted head.
restoreScheduledNode :: ScheduledNode -> SearchNode
restoreScheduledNode (ScheduledNode nextGoal node) = node
  { nodeGoals = nextGoal Seq.<| nodeGoals node }

-- | Historical status-bearing view of an already checked engine trace.
findExpressions :: CheckedExferenceQuery -> [ExferenceChunkElement]
findExpressions = findExpressionsWithAllocators defaultSearchAllocators

findExpressionsWithAllocators
  :: SearchAllocators
  -> CheckedExferenceQuery
  -> [ExferenceChunkElement]
findExpressionsWithAllocators allocators' =
  map projectCompatibilityChunk . findEngineBatchesWith allocators'

projectCompatibilityChunk :: EngineBatch -> ExferenceChunkElement
projectCompatibilityChunk chunk = ExferenceChunkElement
  compatibilityStatus
  (projectCompatibilityBindingUsages
    $ exferenceBindingUsages metadata)
  (map projectCompatibilityCandidate $ SharedSearch.batchCandidates chunk)
 where
  metadata = SharedSearch.batchMetadata chunk
  compatibilityStatus = SearchStatus
    compatibilityCompletion
    (SharedCount.saturatingNaturalToInt $ exferenceQueuePruned metadata)
    (SharedCount.saturatingNaturalToInt $ exferenceDepthPruned metadata)
  compatibilityCompletion = case SharedSearch.batchProgress chunk of
    SharedSearch.Continuing -> SearchRunning
    SharedSearch.Completed SharedSearch.Finished -> SearchExhausted
    SharedSearch.Completed (SharedSearch.Truncated reasons)
      | SharedSearch.IdentifierSpaceExhausted `elem` reasons ->
          SearchIdentifierSpaceExhausted
      | SharedSearch.StepLimitReached `elem` reasons ->
          SearchStepLimitReached
      | otherwise -> SearchPruned

  projectCompatibilityCandidate
    (ValidatedEngineCandidate expression constraints statistics) =
      (expression, constraints, statistics)

-- | Project exact engine totals into the historical chunk API without
-- allowing a large count to wrap into a misleading non-positive value.
projectCompatibilityBindingUsages
  :: BindingUsages
  -> M.Map QualifiedName Int
projectCompatibilityBindingUsages =
  M.map SharedCount.saturatingNaturalToInt

-- | Validate one query against a sealed environment, then expose its result
-- trace lazily in the common query envelope.  The exact target is excluded
-- before validation so search and the independent candidate checker see the
-- same environment.  The retained rigid-variable plan supplies rendering
-- hints without preparing the query a second time. The opaque hint value must
-- retain the same canonical goal; a value sealed for another local integer
-- namespace is rejected before the lazy trace is exposed.
findQueryResultsInEnvironmentEither
  :: SharedGenerated.DefinitionName
  -> ExferenceSourceTypeVariableHints
  -> ExferenceEnvironment
  -> ExferenceQuery
  -> Either ExferenceInputError [ExferenceResult]
findQueryResultsInEnvironmentEither =
  findQueryResultsWithAllocators defaultSearchAllocators

-- | Run a query whose options were checked before adapter-specific work.
-- The witness is authoritative: replacing the query field also means this
-- entrance cannot accidentally inspect or trust a second, unchecked copy.
-- This operation is exported only from the Cabal-private implementation
-- module and therefore does not weaken the public checked core boundary.
findQueryResultsInEnvironmentWithCheckedOptions
  :: SharedGenerated.DefinitionName
  -> ExferenceSourceTypeVariableHints
  -> ExferenceEnvironment
  -> ExferenceQuery
  -> CheckedExferenceOptions
  -> Either ExferenceInputError [ExferenceResult]
findQueryResultsInEnvironmentWithCheckedOptions =
  findQueryResultsWithCheckedOptionsAndAllocators defaultSearchAllocators

-- The allocator-parametric form is an internal test seam for exercising
-- finite identifier exhaustion.  Keeping preparation here guarantees that
-- production and those tests share the exact one-validation result path.
findQueryResultsWithAllocators
  :: SearchAllocators
  -> SharedGenerated.DefinitionName
  -> ExferenceSourceTypeVariableHints
  -> ExferenceEnvironment
  -> ExferenceQuery
  -> Either ExferenceInputError [ExferenceResult]
findQueryResultsWithAllocators allocators' target sourceHints environment query = do
  checkedOptions <- checkExferenceOptions $ querySearchOptions query
  findQueryResultsWithCheckedOptionsAndAllocators
    allocators' target sourceHints environment query checkedOptions

findQueryResultsWithCheckedOptionsAndAllocators
  :: SearchAllocators
  -> SharedGenerated.DefinitionName
  -> ExferenceSourceTypeVariableHints
  -> ExferenceEnvironment
  -> ExferenceQuery
  -> CheckedExferenceOptions
  -> Either ExferenceInputError [ExferenceResult]
findQueryResultsWithCheckedOptionsAndAllocators
    allocators' target sourceHints environment query checkedOptions = do
  checked@(CheckedExferenceQuery _ checkedQuery rigidPlan) <-
    prepareExferenceQueryWithCheckedOptions
      environment checkedOptions queryWithTargetExcluded
  -- The opaque value is paired with the exact canonical goal for which its
  -- spelling scope was checked. Stable adapters retarget it only while
  -- performing origin-safe synonym elaboration; direct core callers cannot
  -- accidentally reuse a same-numbered hint with an unrelated query.
  typeHints <- either
    (Left . InvalidSourceTypeVariableHints)
    Right
    $ typeVariableHintsWithPlan
        (queryGoalType checkedQuery) rigidPlan sourceHints
  pure $ map (projectQueryResult target typeHints)
    $ findEngineBatchesWith allocators' checked
 where
  queryWithTargetExcluded = query
    { queryExcludedBindings = S.insert
        (SharedGenerated.definitionName target)
        (queryExcludedBindings query)
    }

-- Keep this projection lazy in both the chunk and candidate dimensions.  The
-- shared smart constructor observes only whether the candidate list is empty,
-- so it neither invents logical evidence nor evaluates the candidate tail.
projectQueryResult
  :: SharedGenerated.DefinitionName
  -> ExferenceTypeVariableHints
  -> EngineBatch
  -> ExferenceResult
projectQueryResult target typeHints =
  SharedQuery.queryResultFromCandidates
    . fmap projectCandidate
 where
  projectCandidate
      (ValidatedEngineCandidate candidateExpression constraints statistics) =
    projectValidatedCandidate
      target typeHints candidateExpression constraints statistics

-- | Closed strictness probe compiled only by @exference-engine-tests@. It
-- returns observations rather than accepting raw candidates, so the test seam
-- cannot bypass the checked engine-candidate constructor.
queryProjectionStrictnessForTesting
  :: SharedGenerated.DefinitionName
  -> ExferenceTypeVariableHints
  -> ( Bool
     , SharedSearch.Progress
     , ExferenceBatchMetadata
     , SharedGenerated.DefinitionName
     )
queryProjectionStrictnessForTesting target typeHints =
  ( SharedQuery.resultEvidence poisonedResult
      == SharedQuery.ValidatedCandidates
  , SharedSearch.batchProgress $ SharedQuery.resultSearch poisonedResult
  , SharedSearch.batchMetadata $ SharedQuery.resultSearch poisonedResult
  , SharedGenerated.clauseName
      $ SharedCandidate.candidateOutput firstCandidate
  )
 where
  metadata = ExferenceBatchMetadata M.empty 2 3
  poisonedResult = projectQueryResult target typeHints
    $ SharedSearch.SearchBatch SharedSearch.Continuing metadata
      ( error "query projection forced a candidate head"
      : error "query projection forced a candidate tail"
      )
  expression = ExpLambda 1 (TypeVar 0) (ExpVar 1 $ TypeVar 0)
  validResult = projectQueryResult target typeHints
    $ SharedSearch.SearchBatch SharedSearch.Continuing metadata
      ( ValidatedEngineCandidate expression [] (ExferenceStats 1 0 0)
      : error "query projection forced the mapped candidate tail"
      )
  firstCandidate = case SharedSearch.batchCandidates
      $ SharedQuery.resultSearch validResult of
    candidate : _ -> candidate
    [] -> error "query projection lost a present candidate"

constraintsRelaxedAtStep :: Bool -> Int -> Int -> Bool
constraintsRelaxedAtStep allowConstraints stopStep currentStep =
  allowConstraints || currentStep <= stopStep

-- | Validate a compatibility input eagerly and retain every fallible value
-- needed by its lazy trace.  The validation order is the historical public
-- error precedence; constructing the checked artifact adds no later checks.
prepareExferenceInput
  :: ExferenceInput
  -> Either ExferenceInputError CheckedExferenceQuery
prepareExferenceInput input = do
  -- Keep this order identical to the historical guard chain.  Besides being
  -- more useful to callers, the first error is part of the compatibility API.
  validateQueryLimits query
  validateEnvironmentDuplicates environment
  validateQueryHeuristics query
  validateEnvironmentRatingsAndSyntax environment
  validateQueryInputWidths environment query
  validateEnvironmentMonotypeWidths environment
  validateEnvironmentConstraintWidths environment
  validateQueryClassConstraints environment query
  validateBindingClassConstraints environment
  validateInputTypes
    $ [queryGoalType query]
    ++ environmentBindingMonotypes environment
    ++ concatMap (constraint_params . snd) allConstraints
  validateEnvironmentDeconstructors environment
  let canonicalEnvironment = canonicalizeEnvironment environment
      canonicalQuery = canonicalizeQuery query
      rigidContext = mkRigidInstantiationContext canonicalEnvironment
  rigidPlan <- prepareRigidInstantiation rigidContext canonicalQuery
  pure $ CheckedExferenceQuery
    (ExferenceEnvironment canonicalEnvironment rigidContext)
    canonicalQuery
    rigidPlan
 where
  environment = inputEnvironment input
  query = inputQuery input
  allConstraints = queryConstraints query
    ++ environmentConstraints environment

-- | Compatibility projection retaining the established validation API.
validateExferenceInput :: ExferenceInput -> Either ExferenceInputError ()
validateExferenceInput input = () <$ prepareExferenceInput input

-- | Seal a reusable environment after validating everything independent of a
-- particular query.  The abstract result can subsequently be paired with many
-- cheap query validations without repeating these checks.
mkExferenceEnvironment
  :: EnvDictionary
  -> Either ExferenceInputError ExferenceEnvironment
mkExferenceEnvironment environment = do
  validateExferenceEnvironment environment
  let canonicalEnvironment = canonicalizeEnvironment environment
  pure $ ExferenceEnvironment canonicalEnvironment
    $ mkRigidInstantiationContext canonicalEnvironment

-- | Validate the varying part of a search against an already sealed
-- environment and retain its exact rigid-instantiation plan. Excluding
-- bindings only removes capabilities and therefore cannot invalidate the
-- environment.
prepareExferenceQuery
  :: ExferenceEnvironment
  -> ExferenceQuery
  -> Either ExferenceInputError CheckedExferenceQuery
prepareExferenceQuery sealed query = do
  checkedOptions <- checkExferenceOptions $ querySearchOptions query
  prepareExferenceQueryWithCheckedOptions sealed checkedOptions query

-- The checked options replace rather than merely justify the public record
-- field. Thus even a future internal caller cannot validate one value and
-- make search consume another.
prepareExferenceQueryWithCheckedOptions
  :: ExferenceEnvironment
  -> CheckedExferenceOptions
  -> ExferenceQuery
  -> Either ExferenceInputError CheckedExferenceQuery
prepareExferenceQueryWithCheckedOptions
    sealed@(ExferenceEnvironment environment rigidContext)
    (CheckedExferenceOptions options) uncheckedQuery = do
  validateQueryInputWidths environment query
  validateQueryClassConstraints environment query
  validateInputTypes
    $ queryGoalType query
    : concatMap (constraint_params . snd) constraints
  let canonicalQuery = canonicalizeQuery query
  rigidPlan <- prepareRigidInstantiation rigidContext canonicalQuery
  pure $ CheckedExferenceQuery sealed canonicalQuery rigidPlan
 where
  query = uncheckedQuery {querySearchOptions = options}
  constraints = queryConstraints query

-- | Compatibility projection retaining the established validation API.
validateExferenceQuery
  :: ExferenceEnvironment
  -> ExferenceQuery
  -> Either ExferenceInputError ()
validateExferenceQuery environment query =
  () <$ prepareExferenceQuery environment query

-- | Validate only query-varying search controls, without inspecting a goal or
-- environment. Checked adapters use this before session-dependent elaboration
-- so an invalid independently supplied option cannot be mislabeled as a
-- source-derived kind, synonym, or lowering failure.
validateExferenceOptions
  :: ExferenceOptions
  -> Either ExferenceInputError ()
validateExferenceOptions options = () <$ checkExferenceOptions options

-- | Validate every search-control field once and retain the exact accepted
-- record for later preparation. This remains package-private; public core
-- callers continue through 'validateExferenceOptions' or a checked search.
checkExferenceOptions
  :: ExferenceOptions
  -> Either ExferenceInputError CheckedExferenceOptions
checkExferenceOptions options = do
  validateOptionsLimits options
  validateOptionsHeuristics options
  pure $ CheckedExferenceOptions options

validateExferenceEnvironment
  :: EnvDictionary
  -> Either ExferenceInputError ()
validateExferenceEnvironment environment = do
  validateEnvironmentDuplicates environment
  validateEnvironmentRatingsAndSyntax environment
  validateEnvironmentMonotypeWidths environment
  validateEnvironmentConstraintWidths environment
  validateBindingClassConstraints environment
  validateInputTypes
    $ environmentBindingMonotypes environment
    ++ concatMap (constraint_params . snd) constraints
  validateEnvironmentDeconstructors environment
 where
  constraints = environmentConstraints environment

validateQueryLimits
  :: ExferenceQuery
  -> Either ExferenceInputError ()
validateQueryLimits = validateOptionsLimits . querySearchOptions

validateOptionsLimits
  :: ExferenceOptions
  -> Either ExferenceInputError ()
validateOptionsLimits options
  | exferenceMaximumSteps options <= 0 =
      Left $ InvalidMaxSteps $ exferenceMaximumSteps options
  | exferenceConstraintDeferralSteps options < 0 =
      Left $ InvalidConstraintDeferralSteps
        $ exferenceConstraintDeferralSteps options
  | Just limit <- exferenceMaximumQueueSize options, limit < 0 =
      Left $ InvalidMaxQueueSize limit
  | Just limit <- exferenceMaximumDepth options, not $ isFinitePenalty limit =
      Left $ InvalidMaxDepth limit
  | otherwise = Right ()

validateEnvironmentDuplicates
  :: EnvDictionary
  -> Either ExferenceInputError ()
validateEnvironmentDuplicates environment = case
    validateEnvironmentBindingIdentities environment of
  Right () -> Right ()
  Left failure -> Left $ case failure of
    DuplicateDeconstructorIdentities names ->
      DuplicateDeconstructorNames names
    DuplicateConstructorIdentities names ->
      DuplicateConstructorNames names
    DuplicateFunctionIdentities names ->
      DuplicateFunctionNames names

validateQueryHeuristics
  :: ExferenceQuery
  -> Either ExferenceInputError ()
validateQueryHeuristics = validateOptionsHeuristics . querySearchOptions

validateOptionsHeuristics
  :: ExferenceOptions
  -> Either ExferenceInputError ()
validateOptionsHeuristics options
  | Just (field, invalid) <- find (not . isFinitePenalty . snd)
      (heuristicFields $ exferenceHeuristics options) =
      Left $ InvalidHeuristic field invalid
  | otherwise = Right ()

validateEnvironmentRatingsAndSyntax
  :: EnvDictionary
  -> Either ExferenceInputError ()
validateEnvironmentRatingsAndSyntax environment =
  case validateEnvironmentBindingRatings environment of
    Left (NonFiniteFunctionRating name penalty) ->
      Left $ InvalidHeuristic (show name) penalty
    Right () -> case validateEnvironmentBindingSyntax environment of
      Left (InvalidFunctionBindingSyntax name syntaxError) ->
        Left $ InvalidGeneratedBinding name syntaxError
      Left (InvalidConstructorBindingSyntax name syntaxError) ->
        Left $ InvalidGeneratedConstructor name syntaxError
      Right () -> Right ()

-- Width preflights run before forall discovery because both tuple elements and
-- class arguments are public lazy lists. They own only the first impossible
-- cell; complete native-type and class validation remains in the established
-- later phases. A width error intentionally wins over an error hidden beyond
-- that impossible cell, since discovering whether such a tail is finite would
-- itself require the nontermination this boundary prevents.
validateQueryInputWidths
  :: EnvDictionary
  -> ExferenceQuery
  -> Either ExferenceInputError ()
validateQueryInputWidths environment query = validateTypeInputWidths
  environment QueryConstraint $ queryGoalType query

validateEnvironmentMonotypeWidths
  :: EnvDictionary
  -> Either ExferenceInputError ()
validateEnvironmentMonotypeWidths environment = do
  traverse_ validateFunctionWidth $ environmentFunctions environment
  traverse_ validateDeconstructorWidth $ environmentDeconstructors environment
 where
  validateFunctionWidth binding = validateTypeInputWidths environment
    (BindingConstraint $ functionName binding)
    $ functionBindingType binding

  validateDeconstructorWidth binding = do
    validateTypeInputWidths environment inputSite
      $ deconstructorInput binding
    traverse_ validateConstructor $ deconstructorConstructors binding
   where
    inputSite = maybe QueryConstraint BindingConstraint
      $ typeConstructorHead $ deconstructorInput binding
    validateConstructor constructor = traverse_
      (validateTypeInputWidths environment
        $ BindingConstraint $ constructorName constructor)
      $ constructorFields constructor

validateEnvironmentConstraintWidths
  :: EnvDictionary
  -> Either ExferenceInputError ()
validateEnvironmentConstraintWidths environment = traverse_
  (uncurry $ validateConstraintInputWidths environment)
  $ environmentConstraints environment

validateConstraintInputWidths
  :: EnvDictionary
  -> ConstraintSite
  -> HsConstraint
  -> Either ExferenceInputError ()
validateConstraintInputWidths environment site constraint = do
  validateKnownArity environment site constraint
  traverse_ validateArgument $ constraint_params constraint
 where
  validateArgument argument = SharedType.validateTypeWidthsWith
    (InvalidInputType argument . InvalidSynthesisType)
    (validateKnownArity environment site)
    argument

validateTypeInputWidths
  :: EnvDictionary
  -> ConstraintSite
  -> HsType
  -> Either ExferenceInputError ()
validateTypeInputWidths environment site original =
  SharedType.validateTypeWidthsWith
    (InvalidInputType original . InvalidSynthesisType)
    (validateKnownArity environment site)
    original

validateKnownArity
  :: EnvDictionary
  -> ConstraintSite
  -> HsConstraint
  -> Either ExferenceInputError ()
validateKnownArity environment site constraint = either
  (Left . InvalidClassConstraint)
  Right
  $ validateKnownConstraintArityInEnv
      (environmentClasses environment) site constraint

validateQueryClassConstraints
  :: EnvDictionary
  -> ExferenceQuery
  -> Either ExferenceInputError ()
validateQueryClassConstraints environment query =
  case listToMaybe
      [ classError
      | constraint <- typeConstraints $ queryGoalType query
      , Left classError <-
          [validateKnownConstraintInEnv classes QueryConstraint constraint]
      ] of
    Just classError -> Left $ InvalidClassConstraint classError
    Nothing -> Right ()
 where
  classes = environmentClasses environment

validateBindingClassConstraints
  :: EnvDictionary
  -> Either ExferenceInputError ()
validateBindingClassConstraints environment =
  case listToMaybe
      [ classError
      | (site, constraint) <- bindingConstraints
      , Left classError <-
          [validateKnownConstraintInEnv classes site constraint]
      ] of
    Just classError -> Left $ InvalidClassConstraint classError
    Nothing -> Right ()
 where
  classes = environmentClasses environment
  bindingConstraints =
    [ (BindingConstraint $ functionName binding, constraint)
    | binding <- environmentFunctions environment
    , constraint <- functionConstraints binding
        ++ typeConstraints (functionBindingType binding)
    ] ++
    [ (deconstructorSite deconstructor, constraint)
    | deconstructor <- environmentDeconstructors environment
    , constraint <- typeConstraints $ deconstructorInput deconstructor
    ] ++
    [ (BindingConstraint $ constructorName constructor, constraint)
    | deconstructor <- environmentDeconstructors environment
    , constructor <- deconstructorConstructors deconstructor
    , field <- constructorFields constructor
    , constraint <- typeConstraints field
    ]
  deconstructorSite deconstructor = maybe QueryConstraint BindingConstraint
    $ typeConstructorHead $ deconstructorInput deconstructor

validateInputTypes
  :: [HsType]
  -> Either ExferenceInputError ()
validateInputTypes types = case listToMaybe
    [ (typeExpression, typeError)
    | typeExpression <- types
    , Left typeError <- [toSynthesisType typeExpression]
    ] of
  Just (typeExpression, typeError) ->
    Left $ InvalidInputType typeExpression typeError
  Nothing -> Right ()

validateEnvironmentDeconstructors
  :: EnvDictionary
  -> Either ExferenceInputError ()
validateEnvironmentDeconstructors environment =
  traverse_ validateDeconstructor $ environmentDeconstructors environment
 where
  validateDeconstructor = either
    (Left . projectDeconstructorFailure)
    Right
    . validateDeconstructorBinding

  projectDeconstructorFailure failure = case failure of
    MissingDeconstructorNominalHead input ->
      DeconstructorInputWithoutNominalHead input
    FunctionDeconstructorHead name -> UnsupportedDeconstructorTypeHead name
    UnboundDeconstructorFields typeName constructor variables ->
      UnboundDeconstructorFieldVariables typeName constructor variables

prepareRigidInstantiation
  :: RigidInstantiationContext
  -> ExferenceQuery
  -> Either ExferenceInputError RigidInstantiationPlan
prepareRigidInstantiation context query = either
  (Left . RigidIdentifierExhaustion)
  Right
  $ planRigidInstantiation context [] $ queryGoalType query

inputEnvironment :: ExferenceInput -> EnvDictionary
inputEnvironment input = EnvDictionary
  { environmentFunctions = input_envFuncs input
  , environmentDeconstructors = input_envDeconsS input
  , environmentClasses = input_envClasses input
  }

inputQuery :: ExferenceInput -> ExferenceQuery
inputQuery input = ExferenceQuery
  { queryGoalType = input_goalType input
  , queryExcludedBindings = S.empty
  , querySearchOptions = inputSearchOptions input
  }

-- This is the sole projection from the historical flat compatibility record.
-- Every checked query after this boundary owns the canonical grouped value.
inputSearchOptions :: ExferenceInput -> ExferenceOptions
inputSearchOptions input = ExferenceOptions
  { exferenceAllowUnused = input_allowUnused input
  , exferenceAllowResidualConstraints = input_allowConstraints input
  , exferenceConstraintDeferralSteps = input_allowConstraintsStopStep input
  , exferenceMultiConstructorPatterns = input_multiPM input
  , exferenceMaximumSteps = input_maxSteps input
  , exferenceMaximumQueueSize = input_maxQueueSize input
  , exferenceMaximumDepth = input_maxDepth input
  , exferenceHeuristics = input_heuristicsConfig input
  }

-- Checked values retain the same canonical representation that validation
-- observes.  In particular, saturated function/tuple constructor applications
-- must not survive in a sealed session beside equal structural nodes.
canonicalizeQuery :: ExferenceQuery -> ExferenceQuery
canonicalizeQuery query = query
  { queryGoalType = SharedType.canonicalizeType $ queryGoalType query
  }

canonicalizeEnvironment :: EnvDictionary -> EnvDictionary
canonicalizeEnvironment environment = environment
  { environmentFunctions = map canonicalizeFunctionBinding
      $ environmentFunctions environment
  , environmentDeconstructors = map canonicalizeDeconstructorBinding
      $ environmentDeconstructors environment
  }

canonicalizeFunctionBinding :: FunctionBinding -> FunctionBinding
canonicalizeFunctionBinding = mapFunctionBindingTypes canonicalize

canonicalizeDeconstructorBinding
  :: DeconstructorBinding
  -> DeconstructorBinding
canonicalizeDeconstructorBinding = mapDeconstructorBindingTypes canonicalize

canonicalize :: HsType -> HsType
canonicalize = SharedType.canonicalizeType

-- Binding monotypes only: constraint argument types are validated beside
-- their sites through 'environmentConstraints', unlike the rigid planner's
-- complete environment scan.
environmentBindingMonotypes :: EnvDictionary -> [HsType]
environmentBindingMonotypes environment =
  map functionBindingType (environmentFunctions environment)
  ++ map deconstructorBindingType (environmentDeconstructors environment)

queryConstraints :: ExferenceQuery -> [(ConstraintSite, HsConstraint)]
queryConstraints query =
  [ (QueryConstraint, constraint)
  | constraint <- typeConstraints $ queryGoalType query
  ]

-- | Merge a generated frontier without ever overflowing pqueue's internal
-- Int size. The ordinary branch deliberately retains the historical
-- construction and tie behavior. The fallback is reachable in tests through
-- a small injected capacity; production reaches it only at Int-sized
-- representation exhaustion, where it keeps the best representable nodes and
-- reports every omitted node as queue pruning.
mergeQueueWithCapacity
  :: Ord priority
  => Natural
  -> Maybe Int
  -> Q.MaxPQueue priority value
  -> [(priority, value)]
  -> (Q.MaxPQueue priority value, Natural)
mergeQueueWithCapacity requestedCapacity maximumSize queued newEntries
  | combinedSize <= capacity =
      let combined = Q.union queued (Q.fromList newEntries)
      in limitQueue normalizedMaximumSize combined
  | otherwise =
      ( Q.fromList
          $ take (fromIntegral retainedSize)
          $ sortBy descendingPriority
          $ Q.toDescList queued ++ newEntries
      , combinedSize - retainedSize
      )
 where
  capacity = min requestedCapacity maximumPQueueSize
  combinedSize = queueSizeNatural queued + SharedCount.naturalLength newEntries
  normalizedMaximumSize = max 0 <$> maximumSize
  configuredCapacity = maybe capacity
    (min capacity . fromIntegral) normalizedMaximumSize
  retainedSize = min combinedSize configuredCapacity
  descendingPriority (left, _) (right, _) = compare right left

limitQueue
  :: Ord priority
  => Maybe Int
  -> Q.MaxPQueue priority value
  -> (Q.MaxPQueue priority value, Natural)
limitQueue Nothing queue = (queue, 0)
limitQueue (Just maximumSize) queue
  -- A queue already within its bound is returned untouched; rebuilding it
  -- from a sorted list on every step would make each search step cost
  -- O(n log n) whenever a maximum size is configured.
  | queueSize <= retentionLimit = (queue, 0)
  | otherwise =
      ( Q.fromList $ take retentionLimit $ Q.toDescList queue
      , fromIntegral $ queueSize - retentionLimit
      )
 where
  retentionLimit = max 0 maximumSize
  queueSize = Q.size queue

maximumPQueueSize :: Natural
maximumPQueueSize = fromIntegral (maxBound :: Int)

queueSizeNatural :: Q.MaxPQueue priority value -> Natural
queueSizeNatural = fromIntegral . Q.size

naturalPruningReasons
  :: Natural
  -> Natural
  -> [SharedSearch.TruncationReason]
naturalPruningReasons queuePruned depthPruned =
  [ SharedSearch.QueueLimitPruned queuePruned
  | queuePruned > 0
  ] ++
  [ SharedSearch.DepthLimitPruned depthPruned
  | depthPruned > 0
  ]

rateNode :: ExferenceHeuristicsConfig -> SearchNode -> Priority
rateNode h s = priorityFromPenalty
  $ addScore
      (negateScore $ addScore (rateGoals h $ nodeGoals s) $ nodeDepth s)
      (rateUsage h s)

rateGoals :: ExferenceHeuristicsConfig -> Seq.Seq TGoal -> Penalty
rateGoals h = sumScores . fmap rateGoal
  where
    rateGoal (TGoal (VarBinding _ t) _ _) = typeComplexity h t

-- TODO: actually measure performance with different values and derive these
-- weights instead of relying on historically chosen defaults.
typeComplexity :: ExferenceHeuristicsConfig -> HsType -> Penalty
typeComplexity h = complexity
 where
  complexity TypeVar{} = heuristics_goalVar h
  complexity TypeConstant{} = heuristics_goalCons h -- TODO distinct heuristic?
  complexity TypeCons{} = heuristics_goalCons h
  complexity (TypeArrow parameter result) = sumScores
    [heuristics_goalArrow h, complexity parameter, complexity result]
  complexity (TypeApp function argument) = sumScores
    [heuristics_goalApp h, complexity function, complexity argument]
  -- Reproduce the former left-associated tuple-constructor application one
  -- node at a time. 'Penalty' addition saturates and Double addition is not
  -- associative, so multiplying/grouping equal weights can alter queue order.
  complexity (TypeTuple _ elements) = L.foldl' applyElement
    (heuristics_goalCons h) elements
   where
    applyElement functionCost element = sumScores
      [heuristics_goalApp h, functionCost, complexity element]
  -- Nested quantified values are indivisible atoms to the search heuristic,
  -- just as they are to unification. The root prenex wrapper is scheduled at
  -- priority zero and opened separately, so this cost applies to rank-N goals.
  complexity TypeForallNative{} = heuristics_goalCons h

rateUsage :: ExferenceHeuristicsConfig -> SearchNode -> Penalty
rateUsage h = sumScores . map f . IntMap.elems . nodeVarUses where
  f :: Natural -> Penalty
  f 0 = negateScore $ heuristics_tempUnusedVarPenalty h
  f 1 = 0
  f k = negateScore $ multiplyScore
    (fromIntegral (k - 1)) (heuristics_tempMultiVarUsePenalty h)

getUnusedVarCount :: SearchNode -> Natural
getUnusedVarCount = IntMap.foldl' countUnused 0 . nodeVarUses
 where
  countUnused count uses
    | uses == 0 = count + 1
    | otherwise = count

-- Take one SearchNode, return some amount of sub-SearchNodes. Some of the
-- returned SearchNodes may in fact be (potential) solutions that do not
-- require further evaluation.
--
-- Basic implementation idea:
-- Take the first goal for this SearchNode. Its type determines what the next
-- step is (and which sub-function to use).
stateStep :: SearchAllocators
          -> Bool
          -> Bool
          -> ExferenceHeuristicsConfig
          -> TGoal
          -> StateT SearchNode SearchBranches ()
stateStep allocators multiPM allowConstrs h
    (TGoal (VarBinding var goalType) scopeId forallMode) = do
  -- This paragraph is evil, and hopefully temporary. (Scoping issues make it necessary.)
  contxt <- gets nodeQueryClassEnv
  constraintGoals' <- gets nodeConstraintGoals

  let
    -- if type is TypeArrow, transform to lambda expression.
    arrowStep
      :: HsType
      -> [VarBinding]
      -> StateT SearchNode SearchBranches ()
    arrowStep g ts
      -- descend until no more TypeArrows, accumulating what is seen.
      | TypeArrow t1 t2 <- g = do
          nextId <- builderAllocVar allocators
          arrowStep t2 (VarBinding nextId t1 : ts)
      -- finally, do the goal/expression transformation.
      | otherwise = do
          nextId <- builderAllocHole allocators
          newScopeId <- builderAddScope allocators scopeId
          modify $ \node -> node
            { nodeExpression = fillExprHole var
                (foldl' (\e (VarBinding v ty) -> ExpLambda v ty e)
                  (ExpHole nextId) ts)
                (nodeExpression node)
            , nodeDepth = addScore (nodeDepth node)
                $ heuristics_functionGoalTransform h
            , nodeLastStepBinding = Nothing
            }
          -- for each parameter introduced in the lambda-expression above,
          -- it may be possible to pattern-match. and pattern-matching
          -- may cause duplication of the goals (e.g. for the different cases
          -- in the pattern match).
          additionalGoals <- addScopePatternMatch
            allocators multiPM g nextId newScopeId
            $ map splitBinding
            $ reverse ts
          modify $ \node -> node
            { nodeGoals = nodeGoals node <> Seq.fromList additionalGoals }

    -- if type is TypeForall, fix the forall-variables, i.e. invent a fresh
    -- set of constants that replace the relevant forall-variables.
    forallStep
      :: [TVarId]
      -> [HsConstraint]
      -> HsType
      -> StateT SearchNode SearchBranches ()
    forallStep vs cs t = do
      instantiations <- state $ \node ->
        let (current, remaining) = splitRigidInstantiationLayer vs
              $ nodeRigidInstantiations node
        in if map fst current == vs
          then (current, node {nodeRigidInstantiations = remaining})
          else error $ "rigid-instantiation plan disagrees with forall layer: "
            ++ show vs ++ " /= " ++ show (map fst current)
      modify $ \node -> node
        { nodeDepth = addScore (nodeDepth node)
            $ heuristics_functionGoalTransform h
          -- TODO: consider a distinct forall-opening heuristic.
        , nodeLastStepBinding = Nothing
        }
      let substs = IntMap.fromList
            [(binder, TypeConstant rigid) | (binder, rigid) <- instantiations]
      modify $ \node -> node
        { nodeGoals = TGoal
            (VarBinding var $ snd $ applySubsts substs t)
            scopeId OpenLeadingForalls
            Seq.<| nodeGoals node
        , nodeQueryClassEnv = addQueryClassEnv
            (snd . constraintApplySubsts substs <$> cs)
            (nodeQueryClassEnv node)
        }

    -- try to resolve the goal by looking at the parameters in scope, i.e.
    -- the parameters accumulated by building the expression so far.
    -- e.g. for (\x -> (_ :: Int)), the goal can be filled by `x` if
    -- `x :: Int`.

    byProvided :: StateT SearchNode SearchBranches ()
    byProvided = do
      provided <- lift . chooseBranches =<< gets
        (scopeGetAllBindings scopeId . nodeProvidedScopes)
      let
        provId = varPVariable provided
        provType = varPResult provided
        dependencies = varPParameters provided
        scheme = SharedType.functionType dependencies provType
        exactUnification = unifyShared goalType provType
        useProvider annotation providedType constraints parameters unification =
          byGenericUnify
            (Right (provId, annotation))
            providedType
            constraints
            parameters
            (heuristics_stepProvidedGood h)
            (heuristics_stepProvidedBad h)
            -- Scoped bindings and the goal inhabit one flexible-variable
            -- namespace. A disjoint unifier would incorrectly accept a
            -- recursive equation such as @a ~ F a@.
            ((\substs -> (substs, substs)) <$> unification)
      case classifyProviderUse scheme goalType of
        -- Both semantic roots explicitly request an opaque polytype
        -- comparison. Merely unifying a flexible monotype goal with the
        -- provider atom is not forwarding and must not suppress per-use
        -- instantiation.
        OpaqueProviderForwarding ->
          useProvider scheme provType [] dependencies exactUnification
        InstantiateProviderUse -> do
          supply <- gets nodeFlexibleIds
          case instantiateLeadingForallsWith
              (searchAllocateFlexibleNamespace allocators)
              supply
              scheme of
            Nothing -> lift $ truncateBranch BranchIdentifierSpaceExhausted
            Just (instantiated, constraints, nextSupply) -> do
              modify $ \node -> node {nodeFlexibleIds = nextSupply}
              let (instantiatedResult, instantiatedParameters) =
                    splitArrowChain instantiated
              useProvider
                instantiated
                instantiatedResult
                constraints
                instantiatedParameters
                (unifyShared goalType instantiatedResult)
        OrdinaryProviderUse ->
          useProvider scheme provType [] dependencies exactUnification

    -- try to resolve the goal by looking at functions from the environment.
    byFunctionSimple :: StateT SearchNode SearchBranches ()
    byFunctionSimple = do
      binding <- lift . chooseBranches =<< gets nodeFunctions
      renaming <- builderFreshenTVarNamespace allocators
        $ IntSet.toAscList $ IntSet.unions
        $ map flexibleIdentifiers $ functionBindingTypes binding
      let
        rename = renameFlexibleType renaming
        provType = rename $ functionResult binding
      byGenericUnify
        (Left $ functionName binding)
        provType
        (map (renameFlexibleConstraint renaming) $ functionConstraints binding)
        (map rename $ functionParameters binding)
        (addScore (heuristics_stepEnvGood h) $ functionPenalty binding)
        (addScore (heuristics_stepEnvBad h) $ functionPenalty binding)
        (unifyDisjoint goalType provType)

    -- on code for byProvided and byFunctionSimple
    byGenericUnify :: Either QualifiedName (TVarId, HsType)
                   -> HsType
                   -> [HsConstraint]
                   -> [HsType]
                   -> Penalty
                   -> Penalty
                   -> Maybe (Substs, Substs)
                   -> StateT SearchNode SearchBranches ()
    byGenericUnify applier
                   provided
                   provConstrs
                   dependencies
                   depthModMatch
                   depthModNoMatch
      = maybe noUnify $ uncurry byUnified
     where
      (applierName, applierVariable) = case applier of
        Left name -> (Just name, Nothing)
        Right variable -> (Nothing, Just variable)
      coreExp = either ExpName (uncurry ExpVar) applier

      noUnify :: StateT SearchNode SearchBranches ()
      noUnify = case dependencies of
        [] -> mzero -- we can't (randomly) partially apply a non-function
        (d:ds) -> do
          vResult <- builderAllocVar allocators
          vParam <- builderAllocHole allocators
          modify $ \node -> node
            { nodeExpression = fillExprHole var (ExpLet
                vResult
                -- The let binds a one-argument application, so its type keeps
                -- the remaining parameters. The scope entry below records the
                -- same arrow type; annotating with the bare result type would
                -- make the independent checker reject every candidate that
                -- retains this let.
                (SharedType.functionType ds provided)
                (ExpApply coreExp $ ExpHole vParam)
                (ExpHole var))
                (nodeExpression node)
            , nodeGoals = TGoal (VarBinding vParam d)
                scopeId KeepForallsOpaque
                Seq.<| nodeGoals node
            }
          newScopeId <- builderAddScope allocators scopeId
          modify $ \node -> node
            { nodeConstraintGoals = nodeConstraintGoals node <> provConstrs
            , nodeDepth = addScore (nodeDepth node) depthModNoMatch
            , nodeLastStepBinding = applierName
            }
          traverse_ (builderRecordVarUse . fst) applierVariable
          additionalGoals <- addScopePatternMatch
            allocators
            multiPM
            goalType
            var
            newScopeId
            [splitBindingWithParameters ds $ VarBinding vResult provided]
          modify $ \node -> node
            { nodeGoals = nodeGoals node <> Seq.fromList additionalGoals }

      byUnified :: Substs -> Substs -> StateT SearchNode SearchBranches ()
      byUnified goalSS provSS = do
        let allSS = IntMap.union goalSS provSS
            substs = case applier of
              Left _  -> goalSS
              Right _ -> allSS
            (applied1, constrs1) = mapM (constraintApplySubsts substs)
                                        constraintGoals'
            constrs2 = map (snd . constraintApplySubsts provSS)
              provConstrs
        newConstraints <- lift $ maybeBranch $ if allowConstrs
          then Just $ constrs1 ++ constrs2
          else if getAny applied1
            then                   isPossible contxt (constrs1 ++ constrs2)
            else (constrs1 ++) <$> isPossible contxt constrs2
        vars <- forM dependencies $ \_ -> builderAllocHole allocators
        let newGoals = mkGoals scopeId $ zipWith VarBinding vars dependencies
            applyProviderSubstitution = case applier of
              Left _ -> goalApplySubst provSS
              Right _ -> id
        modify $ \node -> node
          { nodeGoals = nodeGoals node
              <> Seq.fromList (map applyProviderSubstitution newGoals) }
        builderApplySubst substs
        modify $ \node -> node
          { nodeExpression = fillExprHole var
              (foldl' ExpApply coreExp (map ExpHole vars))
              (nodeExpression node)
          , nodeConstraintGoals = newConstraints
          , nodeDepth = addScore (nodeDepth node) depthModMatch
          , nodeLastStepBinding = applierName
          }
        traverse_ (builderRecordVarUse . fst) applierVariable

  case goalType of
    TypeArrow _ _ -> arrowStep goalType []
    TypeForall is cs t | forallMode == OpenLeadingForalls ->
      forallStep is cs t
    _ -> byProvided <|> byFunctionSimple


{-# INLINE addScopePatternMatch #-}
-- Insert pattern-matching on newly introduced VarPBindings where
-- possible/necessary. Note that this also effectively transforms a goal
-- (into potentially multiple goals), as goal id + HsType + ScopeId = TGoal.
-- So the input describes one single TGoal.
-- TGoals are duplicated when the pattern-matching involves more than one case,
-- as the goals for different cases are distinct because their scopes are
-- modified when new bindings are added by the pattern-matching.
addScopePatternMatch :: SearchAllocators
                     -> Bool -- should p-m on anything but newtypes?
                     -> HsType -- the current goal (should be returned in one
                               --  form or another)
                     -> Int    -- goal id (hole id)
                     -> ScopeId -- scope for this goal
                     -> [VarPBinding]
                     -> StateT SearchNode SearchBranches [TGoal]
addScopePatternMatch allocators multiPM goalType vid sid bindings = case bindings of
  [] -> return
    [TGoal (VarBinding vid goalType) sid KeepForallsOpaque]
  (b : bindingRest) -> do
    let v = varPVariable b
        vtResult = varPResult b
        vtParams = varPParameters b
    let expVar = ExpVar v $ SharedType.functionType vtParams vtResult
    modify $ \node -> node
      { nodeProvidedScopes = scopesAddPBinding sid b
          $ nodeProvidedScopes node }
    let defaultHandleRest = addScopePatternMatch
          allocators multiPM goalType vid sid bindingRest
    case vtResult of
      TypeVar {}    -> defaultHandleRest -- dont pattern-match on variables, even if it unifies
      TypeArrow {}  ->
        error $ "addScopePatternMatch: TypeArrow: " ++ show vtResult  -- should never happen, given a pbinding..
      -- A quantified result has no nominal head visible to pattern matching.
      -- It remains usable as one opaque scoped value.
      TypeForallNative {} -> defaultHandleRest
      _ | not $ null vtParams -> defaultHandleRest
        | otherwise -> do
            selectDeconstructor =<< gets nodeDeconstructors
         where
          -- Preserve the historical first-applicable deconstructor policy.
          selectDeconstructor [] = defaultHandleRest
          selectDeconstructor (deconstructor : remaining) =
            maybe (selectDeconstructor remaining) id $ mapFunc deconstructor

          -- Sealing guarantees that every field variable occurs in the
          -- datatype head. 'unifyRight' gives that head a temporary tagged
          -- namespace and returns substitutions keyed by its original IDs,
          -- so applying those substitutions directly to the validated fields
          -- is both capture-safe and allocation-free.
          mapFunc
            :: DeconstructorBinding
            -> Maybe (StateT SearchNode SearchBranches [TGoal])
          mapFunc (DeconstructorBinding matchParam [] False) =
            let eliminateEmpty = do
                  -- An empty case evaluates its scrutinee once and has no
                  -- branch goals. Recording that use is also what lets a
                  -- strict no-unused-variable search retain the proof.
                  builderRecordVarUse v
                  modify $ \node -> node
                    { nodeExpression = fillExprHole vid
                        (ExpCaseMatch expVar [])
                        (nodeExpression node) }
                  pure []
            -- No deconstructor variable escapes an empty match. 'unifyRight'
            -- already gives its right operand a disjoint tagged namespace, so
            -- reserving persistent flexible IDs here could only introduce a
            -- spurious identifier-space truncation.
            in eliminateEmpty <$ unifyRight vtResult matchParam
          mapFunc (DeconstructorBinding matchParam
                    [ConstructorBinding matchId matchRs] False) =
            fmap mapFunc1 $ unifyRight vtResult matchParam
           where
            mapFunc1 substs = do
              vars <- forM matchRs $ \_ ->
                builderAllocVar allocators
              builderRecordVarUse v
              let newProvTypes = map (snd . applySubsts substs) matchRs
                  newBinds = zipWith
                    (\x y -> splitBinding $ VarBinding x y)
                    vars
                    newProvTypes
                  expr = ExpLetMatch matchId
                    (zip vars newProvTypes)
                    expVar
                    (ExpHole vid)
              modify $ \node -> node
                { nodeExpression = fillExprHole vid expr
                    $ nodeExpression node }
              addScopePatternMatch
                allocators
                multiPM
                goalType
                vid
                sid
                (reverse newBinds ++ bindingRest)
          mapFunc (DeconstructorBinding matchParam
              matchers@(_ : _) False)
            | multiPM = fmap mapFunc2 $ unifyRight vtResult matchParam
           where
            mapFunc2 substs = do
              -- The case expression evaluates its scrutinee once. Its
              -- alternatives do not constitute additional uses of that
              -- variable; charging one use per constructor biases the queue
              -- against datatypes merely for having more constructors.
              builderRecordVarUse v
              matchData <- matchers `forM` \matcher -> do
                let matchId = constructorName matcher
                    matchRs = constructorFields matcher
                newSid <- builderAddScope allocators sid
                vars <- forM matchRs $ \_ ->
                  builderAllocVar allocators
                newVid <- builderAllocHole allocators
                let newProvTypes = map (snd . applySubsts substs) matchRs
                    newBinds = zipWith
                      (\x y -> splitBinding $ VarBinding x y)
                      vars
                      newProvTypes
                return
                  ( (matchId, zip vars newProvTypes, ExpHole newVid)
                  , (newVid, reverse newBinds, newSid)
                  )
              modify $ \node -> node
                { nodeExpression = fillExprHole vid
                    (ExpCaseMatch expVar $ map fst matchData)
                    (nodeExpression node) }
              fmap concat $ map snd matchData `forM`
                \(newVid, newBinds, newSid) ->
                  addScopePatternMatch
                    allocators
                    multiPM
                    goalType
                    newVid
                    newSid
                    (newBinds ++ bindingRest)
          mapFunc _ = Nothing
            -- TODO: deconstructors for recursive data types.
  -- where
  --  (<&>) = flip (<$>)
