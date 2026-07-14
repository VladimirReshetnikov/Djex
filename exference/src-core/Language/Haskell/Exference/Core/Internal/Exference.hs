{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MonadComprehensions #-}

module Language.Haskell.Exference.Core.Internal.Exference
  ( findExpressions
  , findGeneratedSearchBatches
  , findGeneratedSearchBatchesInEnvironment
  , ExferenceHeuristicsConfig (..)
  , ExferenceInput (..)
  , ExferenceEnvironment
  , ExferenceQuery (..)
  , ExferenceOutputElement
  , ExferenceChunkElement (..)
  , ExferenceBatchMetadata (..)
  , ExferenceSearchBatch
  , ExferenceGeneratedOutputElement
  , ExferenceGeneratedSearchBatch
  , ExferenceProjectionError (..)
  , SearchCompletion (..)
  , SearchStatus (..)
  , SearchStatusError (..)
  , toSearchProgress
  , toSearchBatch
  , toGeneratedSearchBatch
  , toGeneratedSearchBatchWithHints
  , constraintsRelaxedAtStep
  , ExferenceInputError (..)
  , mkExferenceEnvironment
  , validateExferenceQuery
  , validateExferenceInput
  , typeVariableHintsInEnvironment
  )
where



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.Core.Expression
import Language.Haskell.Exference.Core.Candidate
import Language.Haskell.Exference.Core.Internal.Candidate
  (projectValidatedCandidate, typeVariableHintsWithPlan)
import Language.Haskell.Exference.Core.ExpressionCheck
import Language.Haskell.Exference.Core.ExpressionSimplify
import Language.Haskell.Exference.Core.Score
import Language.Haskell.Exference.Core.ExferenceStats
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.Core.RigidInstantiation
import Language.Haskell.Exference.Core.Internal.FlexibleIds
import Language.Haskell.Exference.Core.Internal.Unify
import Language.Haskell.Exference.Core.Internal.ConstraintSolver
import Language.Haskell.Exference.Core.Internal.ExferenceNode
import Language.Haskell.Exference.Core.Internal.ExferenceNodeBuilder
import qualified Language.Haskell.Synthesis.Search as SharedSearch
import qualified Language.Haskell.Synthesis.Generated as SharedGenerated
import qualified Language.Haskell.Synthesis.Name as SynthesisName

import qualified Data.PQueue.Prio.Max as Q
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as S
import qualified Data.Sequence as Seq

import Data.Maybe ( maybeToList, fromMaybe, listToMaybe )
import Control.Monad ( mzero, replicateM, forM, liftM )
import Control.Applicative ( (<|>) )
import Data.List ( find, partition, unfoldr )
import Data.Monoid ( Any(..) )
import Data.Foldable ( asum, traverse_ )
import Data.List.NonEmpty (NonEmpty ((:|)))
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State.Lazy
  ( StateT(..), gets, modify, state
  , execStateT, runStateT, mapStateT
  )

data ExferenceHeuristicsConfig = ExferenceHeuristicsConfig
  { heuristics_goalVar                :: Penalty
  , heuristics_goalCons               :: Penalty
  , heuristics_goalArrow              :: Penalty
  , heuristics_goalApp                :: Penalty
  , heuristics_stepProvidedGood       :: Penalty
  , heuristics_stepProvidedBad        :: Penalty
  , heuristics_stepEnvGood            :: Penalty
  , heuristics_stepEnvBad             :: Penalty
  , heuristics_tempUnusedVarPenalty   :: Penalty
  , heuristics_tempMultiVarUsePenalty :: Penalty
  , heuristics_functionGoalTransform  :: Penalty
  , heuristics_unusedVar              :: Penalty
  , heuristics_solutionLength         :: Penalty
  }
  deriving (Eq, Show)

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
  , queryAllowUnused :: Bool
  , queryAllowConstraints :: Bool
  , queryConstraintDeferralSteps :: Int
  , queryMultiConstructorPatterns :: Bool
  , queryMaximumSteps :: Int
  , queryMaximumQueueSize :: Maybe Int
  , queryMaximumDepth :: Maybe Penalty
  , queryHeuristics :: ExferenceHeuristicsConfig
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data SearchStatus = SearchStatus
  { searchCompletion :: SearchCompletion
  , searchQueuePruned :: Int
  , searchDepthPruned :: Int
  }
  deriving (Eq, Show)

data SearchStatusError
  = NegativeQueuePruningCount Int
  | NegativeDepthPruningCount Int
  | ExhaustedWithDiscardedNodes Int Int
  | PrunedWithoutDiscardedNodes
  deriving (Eq, Show)

-- | Project Exference's compatibility status into the common operational
-- vocabulary without turning heuristic exhaustion into a logical claim.
-- Queue and depth pruning remain separately visible even when a later step
-- limit is what stopped the retained frontier.
toSearchProgress
  :: SearchStatus
  -> Either SearchStatusError SharedSearch.Progress
toSearchProgress (SearchStatus completion queuePruned depthPruned)
  | queuePruned < 0 = Left $ NegativeQueuePruningCount queuePruned
  | depthPruned < 0 = Left $ NegativeDepthPruningCount depthPruned
  | otherwise = case completion of
      SearchRunning -> Right SharedSearch.Continuing
      SearchExhausted
        | queuePruned > 0 || depthPruned > 0 ->
            Left $ ExhaustedWithDiscardedNodes queuePruned depthPruned
        | otherwise -> Right $ SharedSearch.Completed SharedSearch.Finished
      SearchStepLimitReached -> Right $ SharedSearch.Completed
        $ SharedSearch.Truncated
        $ SharedSearch.StepLimitReached :| pruningReasons
      SearchPruned -> case pruningReasons of
        reason : remaining -> Right $ SharedSearch.Completed
          $ SharedSearch.Truncated $ reason :| remaining
        [] -> Left PrunedWithoutDiscardedNodes
 where
  pruningReasons =
    [ SharedSearch.QueueLimitPruned $ fromIntegral queuePruned
    | queuePruned > 0
    ] ++
    [ SharedSearch.DepthLimitPruned $ fromIntegral depthPruned
    | depthPruned > 0
    ]

data ExferenceChunkElement = ExferenceChunkElement
  { chunkStatus :: SearchStatus
  , chunkBindingUsages :: BindingUsages
  , chunkElements :: [ExferenceOutputElement]
  }

-- Internal chunks carry the shared progress chosen by the engine alongside
-- the historical status projection.  Modern results therefore never need to
-- reinterpret a caller-constructible compatibility value.
data EngineChunk = EngineChunk
  { engineStatus :: SearchStatus
  , engineProgress :: SharedSearch.Progress
  , engineMetadata :: ExferenceBatchMetadata
  , engineCandidates :: [ExferenceOutputElement]
  }

type ExferenceSearchBatch =
  SharedSearch.SearchBatch ExferenceBatchMetadata ExferenceOutputElement

type ExferenceGeneratedOutputElement =
  ExferenceGeneratedCandidate

type ExferenceGeneratedSearchBatch =
  SharedSearch.SearchBatch ExferenceBatchMetadata ExferenceGeneratedOutputElement

data ExferenceProjectionError
  = InvalidSearchStatus SearchStatusError
  | InvalidCandidate ExferenceCandidateError
  deriving (Eq, Show)

toSearchBatch
  :: ExferenceChunkElement
  -> Either SearchStatusError ExferenceSearchBatch
toSearchBatch chunk = do
  progress <- toSearchProgress $ chunkStatus chunk
  return $ SharedSearch.SearchBatch
    progress (chunkMetadata chunk) (chunkElements chunk)

chunkMetadata :: ExferenceChunkElement -> ExferenceBatchMetadata
chunkMetadata chunk = ExferenceBatchMetadata
  (chunkBindingUsages chunk)
  (fromIntegral $ searchQueuePruned status)
  (fromIntegral $ searchDepthPruned status)
 where
  status = chunkStatus chunk

toGeneratedSearchBatch
  :: ExferenceChunkElement
  -> Either ExferenceProjectionError ExferenceGeneratedSearchBatch
toGeneratedSearchBatch = toGeneratedSearchBatchWithHints M.empty

toGeneratedSearchBatchWithHints
  :: ExferenceTypeVariableHints
  -> ExferenceChunkElement
  -> Either ExferenceProjectionError ExferenceGeneratedSearchBatch
toGeneratedSearchBatchWithHints typeHints chunk = do
  batch <- either (Left . InvalidSearchStatus) Right $ toSearchBatch chunk
  traverse convertCandidate batch
  where
    convertCandidate (candidateExpression, constraints, statistics) =
      either (Left . InvalidCandidate) Right
        $ mkExferenceGeneratedCandidate
            typeHints candidateExpression constraints statistics

type RatedNodes = Q.MaxPQueue Priority SearchNode
data FindExpressionsState = FindExpressionsState
  { findSteps :: Int -- number of steps already performed
  , findQueuePruned :: Int
  , findDepthPruned :: Int
  , findBindingUsages :: BindingUsages
  , findQueue :: RatedNodes
  }

-- Keep the search trace productive by retaining the historical lazy state
-- transformer. An empty priority queue terminates the unfold through Maybe.
popBestNode :: StateT FindExpressionsState Maybe SearchNode
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
findEngineChunks
  :: ExferenceEnvironment
  -> ExferenceQuery
  -> [EngineChunk]
findEngineChunks
    (ExferenceEnvironment EnvDictionary
      { environmentFunctions = allFunctions
      , environmentDeconstructors = deconss'
      , environmentClasses = sClassEnv
      } rigidContext)
    ExferenceQuery
      { queryGoalType = rawType
      , queryExcludedBindings = excludedBindings
      , queryAllowUnused = allowUnused
      , queryAllowConstraints = allowConstraints
      , queryConstraintDeferralSteps = allowConstraintsStopStep
      , queryMultiConstructorPatterns = multiPM
      , queryMaximumSteps = maxSteps
      , queryMaximumQueueSize = maxQueueSize
      , queryMaximumDepth = maxDepth
      , queryHeuristics = heuristics
      } =
  unfoldr helper rootFindExpressionState
 where
  -- Removing an already checked binding cannot invalidate the environment.
  -- Use the same exact projection for search and independent result checking,
  -- otherwise a generated definition could regain the binding it shadows.
  funcs = filter bindingAvailable allFunctions
  bindingAvailable binding = toSynthesisName (functionName binding)
    `S.notMember` excludedBindings

  rootFindExpressionState = FindExpressionsState
    { findSteps = 0
    , findQueuePruned = 0
    , findDepthPruned = 0
    , findBindingUsages = M.empty
    , findQueue = Q.singleton 0 rootSearchNode
    }
  t = forallify rawType
  -- Query validation constructed this same finite plan before exposing the
  -- lazy trace. Recomputing only the goal-sized suffix from the cached
  -- environment maximum is cheap and keeps the raw engine total thereafter.
  rigidPlan = case planRigidInstantiation rigidContext [] rawType of
    Right plan -> plan
    Left failure -> error $ "validated rigid-instantiation invariant violated: "
      ++ show failure
  rootSearchNode = SearchNode
    { nodeGoals           = Seq.singleton
        (TGoal (VarBinding 0 t) initialScopeId)
    , nodeConstraintGoals = []
    , nodeProvidedScopes  = initialScopes
    , nodeVarUses         = IntMap.empty
    , nodeFunctions       = funcs
    , nodeDeconstructors  = deconss'
    , nodeQueryClassEnv   = mkQueryClassEnv sClassEnv []
    , nodeExpression      = ExpHole 0
      -- The root goal and expression already own hole 0.
    , nodeNextVarId       = 1
    , nodeFlexibleIds     = supplyFromIdentifiers
        $ IntSet.toAscList $ flexibleIdentifiers t
    , nodeRigidInstantiations = rigidInstantiations rigidPlan
    , nodeDepth           = 0.0
    , nodeLastStepBinding = Nothing
    }
  transformSolutions :: [SearchNode] -> FindExpressionsState -> EngineChunk
  transformSolutions potentialSolutions searchState = EngineChunk
      (SearchStatus compatibilityCompletion totalQueuePruned totalDepthPruned)
      progress
      (ExferenceBatchMetadata
        { exferenceBindingUsages = newBindingUsages
        , exferenceQueuePruned = fromIntegral totalQueuePruned
        , exferenceDepthPruned = fromIntegral totalDepthPruned
        })
      [ (e, remainingConstraints, ExferenceStats n' d $ Q.size newNodes)
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
          contxt remainingConstraints rawExpression
      , let d = normalizePenalty $ sumScores
              [ nodeDepth solution
              , multiplyScore (heuristics_unusedVar heuristics)
                  $ fromIntegral unusedVarCount
              , multiplyScore (heuristics_solutionLength heuristics)
                  $ fromIntegral (SharedGenerated.expressionSize
                      $ toGeneratedExpression e)
              ]
      ]
    where
      n' = findSteps searchState
      totalQueuePruned = findQueuePruned searchState
      totalDepthPruned = findDepthPruned searchState
      newBindingUsages = findBindingUsages searchState
      newNodes = findQueue searchState
      (compatibilityCompletion, progress)
        | Q.null newNodes
        , Just reasons <- pruningReasons =
            (SearchPruned, SharedSearch.Completed
              $ SharedSearch.Truncated reasons)
        | Q.null newNodes =
            (SearchExhausted, SharedSearch.Completed SharedSearch.Finished)
        | n' >= maxSteps =
            ( SearchStepLimitReached
            , SharedSearch.Completed $ SharedSearch.Truncated
                $ SharedSearch.StepLimitReached :| maybe [] nonEmptyReasons
                    pruningReasons
            )
        | otherwise = (SearchRunning, SharedSearch.Continuing)
      pruningReasons = case
          (totalQueuePruned > 0, totalDepthPruned > 0) of
        (True, True) -> Just
          ( SharedSearch.QueueLimitPruned (fromIntegral totalQueuePruned)
          :| [SharedSearch.DepthLimitPruned $ fromIntegral totalDepthPruned]
          )
        (True, False) -> Just
          (SharedSearch.QueueLimitPruned (fromIntegral totalQueuePruned) :| [])
        (False, True) -> Just
          (SharedSearch.DepthLimitPruned (fromIntegral totalDepthPruned) :| [])
        (False, False) -> Nothing
      nonEmptyReasons (reason :| remaining) = reason : remaining
      -- Validate the exact tree returned to callers.  This used to check the
      -- raw search result and let the CLI rewrite it afterwards, so a
      -- simplifier bug could invalidate an already-approved candidate.  The
      -- raw term remains a safe fallback, but it too must pass independently.
      checkedSimplification contxt constraints rawExpression =
        firstChecked candidates
       where
        simplified = simplifyExpression rawExpression
        candidates
          | simplified == rawExpression = [rawExpression]
          | otherwise = [simplified, rawExpression]
        firstChecked [] = Nothing
        firstChecked (candidate : remainingCandidates) =
          case checkExpressionWithRigidInstantiation
              rigidPlan contxt funcs deconss' t constraints candidate of
            Right () -> case SharedGenerated.validateExpressionSyntax
                $ toGeneratedExpression candidate of
              Right () -> Just candidate
              Left _ -> firstChecked remainingCandidates
            Left _ -> firstChecked remainingCandidates
  helper :: FindExpressionsState -> Maybe (EngineChunk, FindExpressionsState)
  helper searchState | findSteps searchState >= maxSteps = Nothing
  helper searchState = runStateT (do
    s <- popBestNode
    n' <- advanceStep
    let
      -- actual work happens in stateStep
      -- Constraint checks are deliberately relaxed only during the configured
      -- warm-up window. Afterwards unresolved constraints are allowed solely
      -- when the caller explicitly requested constrained results.
      relaxConstraints = constraintsRelaxedAtStep
        allowConstraints allowConstraintsStopStep n'
      rNodes = (`execStateT` s)
        $ stateStep multiPM
                    relaxConstraints
                    heuristics
      (withinDepth, tooDeep) = partition depthAllowed rNodes
      (potentialSolutions, futures) = partition
        (Seq.null . nodeGoals) withinDepth
      ratedNew =
        [ ( normalizePriority $ addPriority
              (rateNode heuristics newS)
              (Priority $ 4.5 * f (fromIntegral n'))
          , newS)
        | newS <- futures
        , let f :: Double -> Double
              f x | x > 900 = 0.0
                  | otherwise = let k = 1.111e-3*x
                                 in 1 + 2*k**3 - 3*k**2
        ]
      depthAllowed node = maybe True (nodeDepth node <=) maxDepth
    -- Account for generated branches rather than only nodes eventually popped
    -- from the queue.  This includes applications that immediately solve the
    -- current goal and branches discarded by the configured bounds.
    traverse_ (modify . recordBindingUsage) rNodes
    modify $ \current -> current
      { findDepthPruned = findDepthPruned current + length tooDeep }
    queued <- gets findQueue
    let combined = Q.union queued (Q.fromList ratedNew)
        (retained, queueDiscarded) = limitQueue maxQueueSize combined
    modify $ \current -> current
      { findQueue = retained
      , findQueuePruned = findQueuePruned current + queueDiscarded
      }
    gets $ transformSolutions potentialSolutions) searchState

-- | Historical status-bearing view of the engine trace.
findExpressions :: ExferenceInput -> [ExferenceChunkElement]
findExpressions input = map projectCompatibilityChunk
  $ uncurry findEngineChunks $ validatedInputParts input

projectCompatibilityChunk :: EngineChunk -> ExferenceChunkElement
projectCompatibilityChunk chunk = ExferenceChunkElement
  (engineStatus chunk)
  (exferenceBindingUsages $ engineMetadata chunk)
  (engineCandidates chunk)

-- | Project the validated engine trace lazily.  Candidate conversion is total
-- here: input validation established the shared type invariants, and search
-- substitutions preserve them.  The fallible adapter above remains for
-- caller-constructed compatibility chunks.
findGeneratedSearchBatches
  :: ExferenceTypeVariableHints
  -> ExferenceInput
  -> [ExferenceGeneratedSearchBatch]
findGeneratedSearchBatches typeHints input =
  uncurry (findGeneratedSearchBatchesInEnvironment typeHints)
    $ validatedInputParts input

findGeneratedSearchBatchesInEnvironment
  :: ExferenceTypeVariableHints
  -> ExferenceEnvironment
  -> ExferenceQuery
  -> [ExferenceGeneratedSearchBatch]
findGeneratedSearchBatchesInEnvironment typeHints environment =
  map (projectGeneratedBatch typeHints) . findEngineChunks environment

-- | Propagate frontend spellings through the exact rigid-variable plan of a
-- checked query. Validation happens first so this helper preserves the same
-- first-error precedence as the search entry point.
typeVariableHintsInEnvironment
  :: ExferenceEnvironment
  -> ExferenceQuery
  -> TypeVarIndex
  -> Either ExferenceInputError ExferenceTypeVariableHints
typeVariableHintsInEnvironment environment@(ExferenceEnvironment _ context)
    query sourceNames = do
  validateExferenceQuery environment query
  plan <- either (Left . RigidIdentifierExhaustion) Right
    $ planRigidInstantiation context [] $ queryGoalType query
  pure $ typeVariableHintsWithPlan plan sourceNames

projectGeneratedBatch
  :: ExferenceTypeVariableHints
  -> EngineChunk
  -> ExferenceGeneratedSearchBatch
projectGeneratedBatch typeHints chunk = SharedSearch.SearchBatch
  (engineProgress chunk)
  (engineMetadata chunk)
  (map projectCandidate $ engineCandidates chunk)
 where
  projectCandidate (candidateExpression, constraints, statistics) =
    projectValidatedCandidate
      typeHints candidateExpression constraints statistics

constraintsRelaxedAtStep :: Bool -> Int -> Int -> Bool
constraintsRelaxedAtStep allowConstraints stopStep currentStep =
  allowConstraints || currentStep <= stopStep

validateExferenceInput :: ExferenceInput -> Either ExferenceInputError ()
validateExferenceInput input = do
  -- Keep this order identical to the historical guard chain.  Besides being
  -- more useful to callers, the first error is part of the compatibility API.
  validateQueryLimits query
  validateEnvironmentDuplicates environment
  validateQueryHeuristics query
  validateEnvironmentRatingsAndSyntax environment
  validateQueryForall query
  validateEnvironmentForalls environment
  validateConstraintForalls allConstraints
  validateQueryClassConstraints environment query
  validateBindingClassConstraints environment
  validateInputTypes
    $ [queryGoalType query]
    ++ environmentTypes environment
    ++ concatMap (constraint_params . snd) allConstraints
  validateEnvironmentDeconstructors environment
  validateRigidInstantiation
    (mkRigidInstantiationContext environment) query
 where
  environment = inputEnvironment input
  query = inputQuery input
  allConstraints = queryConstraints query
    ++ environmentConstraints environment

-- | Seal a reusable environment after validating everything independent of a
-- particular query.  The abstract result can subsequently be paired with many
-- cheap query validations without repeating these checks.
mkExferenceEnvironment
  :: EnvDictionary
  -> Either ExferenceInputError ExferenceEnvironment
mkExferenceEnvironment environment = do
  validateExferenceEnvironment environment
  pure $ ExferenceEnvironment environment
    $ mkRigidInstantiationContext environment

-- | Validate the varying part of a search against an already sealed class
-- environment.  Excluding bindings only removes capabilities and therefore
-- cannot invalidate the environment.
validateExferenceQuery
  :: ExferenceEnvironment
  -> ExferenceQuery
  -> Either ExferenceInputError ()
validateExferenceQuery (ExferenceEnvironment environment rigidContext) query = do
  validateQueryLimits query
  validateQueryHeuristics query
  validateQueryForall query
  validateConstraintForalls constraints
  validateQueryClassConstraints environment query
  validateInputTypes
    $ queryGoalType query
    : concatMap (constraint_params . snd) constraints
  validateRigidInstantiation rigidContext query
 where
  constraints = queryConstraints query

validateExferenceEnvironment
  :: EnvDictionary
  -> Either ExferenceInputError ()
validateExferenceEnvironment environment = do
  validateEnvironmentDuplicates environment
  validateEnvironmentRatingsAndSyntax environment
  validateEnvironmentForalls environment
  validateConstraintForalls constraints
  validateBindingClassConstraints environment
  validateInputTypes
    $ environmentTypes environment
    ++ concatMap (constraint_params . snd) constraints
  validateEnvironmentDeconstructors environment
 where
  constraints = environmentConstraints environment

validateQueryLimits
  :: ExferenceQuery
  -> Either ExferenceInputError ()
validateQueryLimits query
  | queryMaximumSteps query <= 0 =
      Left $ InvalidMaxSteps $ queryMaximumSteps query
  | queryConstraintDeferralSteps query < 0 =
      Left $ InvalidConstraintDeferralSteps
        $ queryConstraintDeferralSteps query
  | Just limit <- queryMaximumQueueSize query, limit < 0 =
      Left $ InvalidMaxQueueSize limit
  | Just limit <- queryMaximumDepth query, not $ isFinitePenalty limit =
      Left $ InvalidMaxDepth limit
  | otherwise = Right ()

validateEnvironmentDuplicates
  :: EnvDictionary
  -> Either ExferenceInputError ()
validateEnvironmentDuplicates environment
  | duplicates@(_ : _) <- repeatedValues
      [ name
      | deconstructor <- environmentDeconstructors environment
      , Just name <- [deconstructorTypeName deconstructor]
      ] = Left $ DuplicateDeconstructorNames duplicates
  | duplicates@(_ : _) <- repeatedValues
      [ constructorName constructor
      | deconstructor <- environmentDeconstructors environment
      , constructor <- deconstructorConstructors deconstructor
      ] = Left $ DuplicateConstructorNames duplicates
  | duplicates@(_ : _) <- repeatedValues
      (map functionName $ environmentFunctions environment) =
      Left $ DuplicateFunctionNames duplicates
  | otherwise = Right ()

validateQueryHeuristics
  :: ExferenceQuery
  -> Either ExferenceInputError ()
validateQueryHeuristics query
  | Just (field, invalid) <- find (not . isFinitePenalty . snd)
      (heuristicFields $ queryHeuristics query) =
      Left $ InvalidHeuristic field invalid
  | otherwise = Right ()

validateEnvironmentRatingsAndSyntax
  :: EnvDictionary
  -> Either ExferenceInputError ()
validateEnvironmentRatingsAndSyntax environment
  -- Historical function ratings are signed: negative values are bonuses.
  -- Query heuristic penalties remain non-negative, but conflating the two
  -- policies would make the shipped environment fail validation.
  | Just binding <- find (not . isFiniteScore . functionPenalty)
      (environmentFunctions environment) = Left $ InvalidHeuristic
        (show $ functionName binding) (functionPenalty binding)
  | Just (binding, syntaxError) <- firstInvalidGeneratedBinding environment =
      Left $ InvalidGeneratedBinding binding syntaxError
  | Just (constructor, syntaxError) <-
      firstInvalidGeneratedConstructor environment =
      Left $ InvalidGeneratedConstructor constructor syntaxError
  | otherwise = Right ()

validateQueryForall
  :: ExferenceQuery
  -> Either ExferenceInputError ()
validateQueryForall query
  | containsNestedForall $ queryGoalType query =
      Left $ NestedForallInGoal $ queryGoalType query
  | otherwise = Right ()

validateEnvironmentForalls
  :: EnvDictionary
  -> Either ExferenceInputError ()
validateEnvironmentForalls environment
  | Just binding <- find (containsForall . functionBindingType)
      (environmentFunctions environment) =
      Left $ NestedForallInBinding (functionName binding) $ functionBindingType binding
  | Just deconstructor <- find (containsForall . deconstructorBindingType)
      (environmentDeconstructors environment) =
      Left $ NestedForallInDeconstructor $ deconstructorBindingType deconstructor
  | otherwise = Right ()

validateConstraintForalls
  :: [(ConstraintSite, HsConstraint)]
  -> Either ExferenceInputError ()
validateConstraintForalls constraints
  | Just (site, constraint) <- find (constraintContainsForall . snd)
      constraints =
      Left $ NestedForallInConstraint site constraint
  | otherwise = Right ()

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
      | binding <- environmentFunctions environment
      , constraint <- functionConstraints binding
          ++ typeConstraints (functionBindingType binding)
      , Left classError <- [validateKnownConstraintInEnv classes
          (BindingConstraint $ functionName binding)
          constraint]
      ] of
    Just classError -> Left $ InvalidClassConstraint classError
    Nothing -> Right ()
 where
  classes = environmentClasses environment

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

-- A deconstructor is an elimination rule for one nominal datatype.  Search
-- unifies its input with arbitrary goals, so accepting a variable or function
-- here would manufacture a pattern match for a type that has no such data
-- constructor.  Constructor fields may mention only the datatype parameters:
-- fresh flexible IDs would otherwise act like undeclared existential types.
validateEnvironmentDeconstructors
  :: EnvDictionary
  -> Either ExferenceInputError ()
validateEnvironmentDeconstructors environment =
  traverse_ validateDeconstructor $ environmentDeconstructors environment
 where
  validateDeconstructor deconstructor = do
    headName <- case typeConstructorHead input of
      Nothing -> Left $ DeconstructorInputWithoutNominalHead input
      Just name
        | SynthesisName.nameSpecial (toSynthesisName name)
            == Just SynthesisName.FunctionConstructor ->
              Left $ UnsupportedDeconstructorTypeHead name
        | otherwise -> Right name
    traverse_ (validateConstructor headName parameters)
      $ deconstructorConstructors deconstructor
   where
    input = deconstructorInput deconstructor
    parameters = flexibleIdentifiers input

  validateConstructor headName parameters constructor
    | IntSet.null unbound = Right ()
    | otherwise = Left $ UnboundDeconstructorFieldVariables
        headName (constructorName constructor) $ IntSet.toAscList unbound
   where
    unbound = IntSet.unions
      (map flexibleIdentifiers $ constructorFields constructor)
      `IntSet.difference` parameters

validateRigidInstantiation
  :: RigidInstantiationContext
  -> ExferenceQuery
  -> Either ExferenceInputError ()
validateRigidInstantiation context query = either
  (Left . RigidIdentifierExhaustion)
  (const $ Right ())
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
  , queryAllowUnused = input_allowUnused input
  , queryAllowConstraints = input_allowConstraints input
  , queryConstraintDeferralSteps = input_allowConstraintsStopStep input
  , queryMultiConstructorPatterns = input_multiPM input
  , queryMaximumSteps = input_maxSteps input
  , queryMaximumQueueSize = input_maxQueueSize input
  , queryMaximumDepth = input_maxDepth input
  , queryHeuristics = input_heuristicsConfig input
  }

-- This conversion is internal and deliberately skips validation.  Every
-- compatibility entry point validates first, just as it did before the split.
validatedInputParts
  :: ExferenceInput
  -> (ExferenceEnvironment, ExferenceQuery)
validatedInputParts input =
  ( ExferenceEnvironment environment $ mkRigidInstantiationContext environment
  , inputQuery input
  )
 where
  environment = inputEnvironment input

-- Report the complete stable duplicate set.  Search explores every raw
-- binding while the independent checker historically selected the first one,
-- so accepting duplicates made both results and penalties list-order
-- dependent.
repeatedValues :: Ord value => [value] -> [value]
repeatedValues values =
  [ value
  | (value, count) <- M.toAscList $ M.fromListWith (+)
      [(value, 1 :: Int) | value <- values]
  , count > 1
  ]

-- Duplicate detection deliberately precedes the dedicated deconstructor-shape
-- validation, preserving the public first-error order while projecting every
-- input that already exposes a nominal head.
deconstructorTypeName :: DeconstructorBinding -> Maybe QualifiedName
deconstructorTypeName = typeConstructorHead . deconstructorInput

firstInvalidGeneratedConstructor
  :: EnvDictionary
  -> Maybe (QualifiedName, SharedGenerated.RenderError)
firstInvalidGeneratedConstructor environment = listToMaybe
  [ (name, syntaxError)
  | deconstructor <- environmentDeconstructors environment
  , constructor <- deconstructorConstructors deconstructor
  , let name = constructorName constructor
        generatedPattern = SharedGenerated.Constructor
          (toSynthesisName name)
          (replicate (length $ constructorFields constructor)
            SharedGenerated.Wildcard)
        probe = SharedGenerated.Lambda [generatedPattern]
          $ SharedGenerated.Hole ()
  , Left syntaxError <- [SharedGenerated.validateExpressionSyntax probe]
  ]

firstInvalidGeneratedBinding
  :: EnvDictionary
  -> Maybe (QualifiedName, SharedGenerated.RenderError)
firstInvalidGeneratedBinding environment = listToMaybe
  [ (name, syntaxError)
  | binding <- environmentFunctions environment
  , let name = functionName binding
  , Left syntaxError <- [SharedGenerated.validateExpressionSyntax
      $ SharedGenerated.Global $ toSynthesisName name]
  ]

environmentTypes :: EnvDictionary -> [HsType]
environmentTypes environment =
  map functionBindingType (environmentFunctions environment)
  ++ map deconstructorBindingType (environmentDeconstructors environment)

queryConstraints :: ExferenceQuery -> [(ConstraintSite, HsConstraint)]
queryConstraints query =
  [ (QueryConstraint, constraint)
  | constraint <- typeConstraints $ queryGoalType query
  ]

-- Associate every explicit constraint with the site already used by class
-- validation. StaticClassEnv is opaque, but its public observations let the
-- search boundary also check superclass and instance argument types rather
-- than assuming that nominal environment validation implies rank support.
environmentConstraints :: EnvDictionary -> [(ConstraintSite, HsConstraint)]
environmentConstraints environment =
  [ (BindingConstraint $ functionName binding, constraint)
  | binding <- environmentFunctions environment
  , constraint <- functionConstraints binding
  ] ++
  [ (ClassSuperclass $ tclass_name declaration, constraint)
  | declaration <- M.elems
      $ sClassEnv_tclasses $ environmentClasses environment
  , constraint <- tclass_constraints declaration
  ] ++ concatMap instanceConstraints
    (sClassEnv_explicitInstances $ environmentClasses environment)
 where
  instanceConstraints instanceDeclaration =
    (InstanceHead, instance_head instanceDeclaration)
    : [ (InstancePrerequisite headName, prerequisite)
      | prerequisite <- instance_constraints instanceDeclaration
      ]
   where
    headName = constraint_tclass $ instance_head instanceDeclaration

limitQueue :: Maybe Int -> RatedNodes -> (RatedNodes, Int)
limitQueue Nothing queue = (queue, 0)
limitQueue (Just maximumSize) queue =
  let entries = Q.toDescList queue
      retained = take maximumSize entries
  in (Q.fromList retained, length entries - length retained)

functionBindingType :: FunctionBinding -> HsType
functionBindingType binding =
  foldr TypeArrow (functionResult binding) (functionParameters binding)

heuristicFields :: ExferenceHeuristicsConfig -> [(String, Penalty)]
heuristicFields config =
  [ ("goalVar", heuristics_goalVar config)
  , ("goalCons", heuristics_goalCons config)
  , ("goalArrow", heuristics_goalArrow config)
  , ("goalApp", heuristics_goalApp config)
  , ("stepProvidedGood", heuristics_stepProvidedGood config)
  , ("stepProvidedBad", heuristics_stepProvidedBad config)
  , ("stepEnvGood", heuristics_stepEnvGood config)
  , ("stepEnvBad", heuristics_stepEnvBad config)
  , ("tempUnusedVarPenalty", heuristics_tempUnusedVarPenalty config)
  , ("tempMultiVarUsePenalty", heuristics_tempMultiVarUsePenalty config)
  , ("functionGoalTransform", heuristics_functionGoalTransform config)
  , ("unusedVar", heuristics_unusedVar config)
  , ("solutionLength", heuristics_solutionLength config)
  ]

deconstructorBindingType :: DeconstructorBinding -> HsType
deconstructorBindingType binding =
  foldr TypeArrow (deconstructorInput binding)
    $ concatMap constructorFields (deconstructorConstructors binding)

constraintContainsForall :: HsConstraint -> Bool
constraintContainsForall = any containsForall . constraint_params

rateNode :: ExferenceHeuristicsConfig -> SearchNode -> Priority
rateNode h s = priorityFromPenalty
  $ addScore
      (negateScore $ addScore (rateGoals h $ nodeGoals s) $ nodeDepth s)
      (rateUsage h s)

rateGoals :: ExferenceHeuristicsConfig -> Seq.Seq TGoal -> Penalty
rateGoals h = sumScores . fmap rateGoal
  where
    rateGoal (TGoal (VarBinding _ t) _) = tComplexity t
    -- TODO: actually measure performance with different values,
    --       use derived values instead of (arbitrarily) chosen ones.
    tComplexity (TypeVar _)         = heuristics_goalVar h
    tComplexity (TypeConstant _)    = heuristics_goalCons h -- TODO different heuristic?
    tComplexity (TypeCons _)        = heuristics_goalCons h
    tComplexity (TypeArrow t1 t2)   = sumScores
      [heuristics_goalArrow h, tComplexity t1, tComplexity t2]
    tComplexity (TypeApp   t1 t2)   = sumScores
      [heuristics_goalApp h, tComplexity t1, tComplexity t2]
    tComplexity (TypeForall _ _ t1) = tComplexity t1

rateUsage :: ExferenceHeuristicsConfig -> SearchNode -> Penalty
rateUsage h = sumScores . map f . IntMap.elems . nodeVarUses where
  f :: Int -> Penalty
  f 0 = negateScore $ heuristics_tempUnusedVarPenalty h
  f 1 = 0
  f k = negateScore $ multiplyScore
    (fromIntegral $ k-1) (heuristics_tempMultiVarUsePenalty h)

getUnusedVarCount :: SearchNode -> Int
getUnusedVarCount = length . filter (== 0) . IntMap.elems . nodeVarUses

-- Take one SearchNode, return some amount of sub-SearchNodes. Some of the
-- returned SearchNodes may in fact be (potential) solutions that do not
-- require further evaluation.
--
-- Basic implementation idea:
-- Take the first goal for this SearchNode. Its type determines what the next
-- step is (and which sub-function to use).
stateStep :: Bool
          -> Bool
          -> ExferenceHeuristicsConfig
          -> StateT SearchNode [] ()
stateStep multiPM allowConstrs h = do
  -- This paragraph is evil, and hopefully temporary. (Scoping issues make it necessary.)
  contxt <- gets nodeQueryClassEnv
  constraintGoals' <- gets nodeConstraintGoals

  (TGoal (VarBinding var goalType) scopeId Seq.:< remainingGoals) <-
    gets $ Seq.viewl . nodeGoals
  modify $ \node -> node { nodeGoals = remainingGoals }

  let
    -- if type is TypeArrow, transform to lambda expression.
    arrowStep
      :: Monad m
      => HsType
      -> [VarBinding]
      -> StateT SearchNode m ()
    arrowStep g ts
      -- descend until no more TypeArrows, accumulating what is seen.
      | TypeArrow t1 t2 <- g = do
          nextId <- builderAllocVar
          arrowStep t2 (VarBinding nextId t1 : ts)
      -- finally, do the goal/expression transformation.
      | otherwise = do
          nextId <- builderAllocHole
          newScopeId <- builderAddScope scopeId
          modify $ \node -> node
            { nodeExpression = fillExprHole var
                (foldl (\e (VarBinding v ty) -> ExpLambda v ty e)
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
          additionalGoals <- addScopePatternMatch multiPM g nextId newScopeId
            $ map splitBinding
            $ reverse ts
          modify $ \node -> node
            { nodeGoals = nodeGoals node <> Seq.fromList additionalGoals }

    -- if type is TypeForall, fix the forall-variables, i.e. invent a fresh
    -- set of constants that replace the relevant forall-variables.
    forallStep
      :: Monad m
      => [TVarId]
      -> [HsConstraint]
      -> HsType
      -> StateT SearchNode m ()
    forallStep vs cs t = do
      instantiations <- state $ \node ->
        let (current, remaining) = splitAt (length vs)
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
            (VarBinding var $ snd $ applySubsts substs t) scopeId
            Seq.<| nodeGoals node
        , nodeQueryClassEnv = addQueryClassEnv
            (snd . constraintApplySubsts substs <$> cs)
            (nodeQueryClassEnv node)
        }
    -- try to resolve the goal by looking at the parameters in scope, i.e.
    -- the parameters accumulated by building the expression so far.
    -- e.g. for (\x -> (_ :: Int)), the goal can be filled by `x` if
    -- `x :: Int`.

    byProvided :: StateT SearchNode [] ()
    byProvided = do
      provided <- lift =<< gets
        (scopeGetAllBindings scopeId . nodeProvidedScopes)
      let
        provId = varPVariable provided
        provType = varPResult provided
        dependencies = varPParameters provided
        -- Scoped values are monotypes. Constraints introduced while partially
        -- applying an environment function already live on the search node.
        provConstrs = S.toList $ qClassEnv_constraints contxt
      mapStateT maybeToList $ byGenericUnify
        (Right (provId, foldr TypeArrow provType dependencies))
        provType
        provConstrs
        dependencies
        (heuristics_stepProvidedGood h)
        (heuristics_stepProvidedBad h)
        -- Scoped bindings and the goal already inhabit the search node's
        -- shared flexible-variable namespace; only the binding's quantified
        -- variables were freshened above.  A disjoint-namespace unifier
        -- would incorrectly accept a recursive equation such as @a ~ F a@.
        ((\substs -> (substs, substs)) <$> unifyShared goalType provType)

    -- try to resolve the goal by looking at functions from the environment.
    byFunctionSimple :: StateT SearchNode [] ()
    byFunctionSimple = do
      binding <- lift =<< gets nodeFunctions
      renaming <- builderFreshenTVarNamespace $ IntSet.toAscList $ IntSet.unions
        $ flexibleIdentifiers (functionResult binding)
        : ( map flexibleIdentifiers (functionParameters binding)
          ++ map constraintFlexibleIdentifiers (functionConstraints binding)
          )
      let
        rename = renameFlexibleType renaming
        provType = rename $ functionResult binding
      mapStateT maybeToList $ byGenericUnify
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
                   -> StateT SearchNode Maybe ()
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

      noUnify :: StateT SearchNode Maybe ()
      noUnify = case dependencies of
        [] -> mzero -- we can't (randomly) partially apply a non-function
        (d:ds) -> do
          vResult <- builderAllocVar
          vParam <- builderAllocHole
          modify $ \node -> node
            { nodeExpression = fillExprHole var (ExpLet
                vResult
                provided
                (ExpApply coreExp $ ExpHole vParam)
                (ExpHole var))
                (nodeExpression node)
            , nodeGoals = TGoal (VarBinding vParam d) scopeId
                Seq.<| nodeGoals node
            }
          newScopeId <- builderAddScope scopeId
          modify $ \node -> node
            { nodeConstraintGoals = nodeConstraintGoals node <> provConstrs
            , nodeDepth = addScore (nodeDepth node) depthModNoMatch
            , nodeLastStepBinding = applierName
            }
          traverse_ (builderRecordVarUse . fst) applierVariable
          additionalGoals <- addScopePatternMatch
            multiPM
            goalType
            var
            newScopeId
            [splitBindingWithParameters ds $ VarBinding vResult provided]
          modify $ \node -> node
            { nodeGoals = nodeGoals node <> Seq.fromList additionalGoals }

      byUnified :: Substs -> Substs -> StateT SearchNode Maybe ()
      byUnified goalSS provSS = do
        let allSS = IntMap.union goalSS provSS
            substs = case applier of
              Left _  -> goalSS
              Right _ -> allSS
            (applied1, constrs1) = mapM (constraintApplySubsts substs)
                                        constraintGoals'
            constrs2 = map (snd . constraintApplySubsts provSS)
              provConstrs
        newConstraints <- lift $ if allowConstrs
          then Just $ constrs1 ++ constrs2
          else if getAny applied1
            then                   isPossible contxt (constrs1 ++ constrs2)
            else (constrs1 ++) <$> isPossible contxt constrs2
        let paramN = length dependencies
        vars <- replicateM paramN builderAllocHole
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
              (foldl ExpApply coreExp (map ExpHole vars))
              (nodeExpression node)
          , nodeConstraintGoals = newConstraints
          , nodeDepth = addScore (nodeDepth node) depthModMatch
          , nodeLastStepBinding = applierName
          }
        traverse_ (builderRecordVarUse . fst) applierVariable

  case goalType of
    TypeArrow _ _ -> arrowStep goalType []
    TypeForall is cs t -> forallStep is cs t
    _ -> byProvided <|> byFunctionSimple


{-# INLINE addScopePatternMatch #-}
-- Insert pattern-matching on newly introduced VarPBindings where
-- possible/necessary. Note that this also effectively transforms a goal
-- (into potentially multiple goals), as goal id + HsType + ScopeId = TGoal.
-- So the input describes one single TGoal.
-- TGoals are duplicated when the pattern-matching involves more than one case,
-- as the goals for different cases are distinct because their scopes are
-- modified when new bindings are added by the pattern-matching.
addScopePatternMatch :: Monad m
                     => Bool -- should p-m on anything but newtypes?
                     -> HsType -- the current goal (should be returned in one
                               --  form or another)
                     -> Int    -- goal id (hole id)
                     -> ScopeId -- scope for this goal
                     -> [VarPBinding]
                     -> StateT SearchNode m [TGoal]
addScopePatternMatch multiPM goalType vid sid bindings = case bindings of
  [] -> return [TGoal (VarBinding vid goalType) sid]
  (b : bindingRest) -> do
    let v = varPVariable b
        vtResult = varPResult b
        vtParams = varPParameters b
    let expVar = ExpVar v (foldr TypeArrow vtResult vtParams)
    modify $ \node -> node
      { nodeProvidedScopes = scopesAddPBinding sid b
          $ nodeProvidedScopes node }
    let defaultHandleRest = addScopePatternMatch multiPM goalType vid sid bindingRest
    case vtResult of
      TypeVar {}    -> defaultHandleRest -- dont pattern-match on variables, even if it unifies
      TypeArrow {}  ->
        error $ "addScopePatternMatch: TypeArrow: " ++ show vtResult  -- should never happen, given a pbinding..
      TypeForall {} ->
        error $ "addScopePatternMatch: TypeForall (RankNTypes not yet implemented)" -- todo when we do RankNTypes
                ++ show vtResult
      _ | not $ null vtParams -> defaultHandleRest
        | otherwise -> do
            supply <- gets nodeFlexibleIds
            fromMaybe defaultHandleRest . asum . map (mapFunc supply)
              =<< gets nodeDeconstructors
         where
          mapFunc
            :: Monad m
            => FlexibleIdSupply
            -> DeconstructorBinding
            -> Maybe (StateT SearchNode m [TGoal])
          mapFunc supply deconstructor@(DeconstructorBinding matchParam
                    [ConstructorBinding matchId matchRs] False) = let
            (renaming, nextSupply) = freshenDeconstructor supply deconstructor
            resultTypes = map (renameFlexibleType renaming) matchRs
            unifyResult = unifyRight vtResult
              $ renameFlexibleType renaming matchParam
            mapFunc1 substs = do -- m
              modify $ \node -> node {nodeFlexibleIds = nextSupply}
              vars <- replicateM (length matchRs) builderAllocVar
              builderRecordVarUse v
              let newProvTypes = map (snd . applySubsts substs) resultTypes
                  newBinds = zipWith (\x y -> splitBinding (VarBinding x y))
                                     vars
                                     newProvTypes
                  expr = ExpLetMatch matchId
                                     (zip vars newProvTypes)
                                     expVar
                                     (ExpHole vid)
              modify $ \node -> node
                { nodeExpression = fillExprHole vid expr
                    $ nodeExpression node }
              addScopePatternMatch multiPM
                                   goalType
                                   vid
                                   sid
                                   (reverse newBinds ++ bindingRest)
            in liftM mapFunc1 unifyResult
          mapFunc supply deconstructor@(DeconstructorBinding matchParam
              matchers@(_ : _) False)
            | multiPM = let
            (renaming, nextSupply) = freshenDeconstructor supply deconstructor
            unifyResult = unifyRight vtResult
              $ renameFlexibleType renaming matchParam
            mapFunc2 substs = do -- m
              modify $ \node -> node {nodeFlexibleIds = nextSupply}
              -- The case expression evaluates its scrutinee once.  Its
              -- alternatives do not constitute additional uses of that
              -- variable; charging one use per constructor biases the queue
              -- against datatypes merely for having more constructors.
              builderRecordVarUse v
              mData <- matchers `forM` \matcher -> do -- m
                let matchId = constructorName matcher
                    matchRs = constructorFields matcher
                newSid <- builderAddScope sid
                let resultTypes = map (renameFlexibleType renaming) matchRs
                vars <- replicateM (length matchRs) builderAllocVar
                newVid <- builderAllocHole
                let newProvTypes = map (snd . applySubsts substs) resultTypes
                    newBinds = zipWith (\x y -> splitBinding (VarBinding x y)) vars newProvTypes
                return ( (matchId, zip vars newProvTypes, ExpHole newVid)
                       , (newVid, reverse newBinds, newSid) )
              modify $ \node -> node
                { nodeExpression = fillExprHole vid
                    (ExpCaseMatch expVar $ map fst mData)
                    (nodeExpression node) }
              liftM concat $ map snd mData `forM` \(newVid, newBinds, newSid) ->
                addScopePatternMatch multiPM goalType newVid newSid (newBinds++bindingRest)
            in liftM mapFunc2 unifyResult
          mapFunc _ _ = Nothing -- TODO: decons for recursive data types

          freshenDeconstructor supply deconstructor = case allocateNamespace
              (IntSet.toAscList $ deconstructorFlexibleIdentifiers deconstructor)
              supply of
            Just result -> result
            Nothing -> error
              "Exference exhausted the finite flexible-variable identifier supply"

          deconstructorFlexibleIdentifiers deconstructor = IntSet.unions
            $ flexibleIdentifiers (deconstructorInput deconstructor)
            : [ flexibleIdentifiers field
              | constructor <- deconstructorConstructors deconstructor
              , field <- constructorFields constructor
              ]
  -- where
  --  (<&>) = flip (<$>)
