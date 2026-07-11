{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Language.Haskell.Exference.Core.Internal.Exference
  ( findExpressions
  , ExferenceHeuristicsConfig (..)
  , ExferenceInput (..)
  , ExferenceOutputElement
  , ExferenceChunkElement (..)
  , SearchCompletion (..)
  , SearchStatus (..)
  , constraintsRelaxedAtStep
  , ExferenceInputError (..)
  , validateExferenceInput
  )
where



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.Core.Expression
import Language.Haskell.Exference.Core.ExpressionCheck
import Language.Haskell.Exference.Core.Score
import Language.Haskell.Exference.Core.ExferenceStats
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.Core.Internal.Unify
import Language.Haskell.Exference.Core.Internal.ConstraintSolver
import Language.Haskell.Exference.Core.Internal.ExferenceNode
import Language.Haskell.Exference.Core.Internal.ExferenceNodeBuilder

import qualified Data.PQueue.Prio.Max as Q
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Sequence as Seq

import Data.Maybe ( maybeToList, fromMaybe )
import Control.Monad ( unless, mzero, replicateM, forM, liftM )
import Control.Applicative ( (<|>) )
import Data.List ( find, partition, unfoldr, intercalate )
import Data.Functor ( ($>) )
import Data.Monoid ( Any(..), Endo(..) )
import Data.Foldable ( sum, asum, traverse_ )
import Control.Monad.Morph ( lift )
import Control.Lens
import Control.Monad.State ( StateT(..), gets, execStateT, runStateT, mapStateT )
import Control.Monad.State ( MonadState )

import Data.Data ( Data )

import Prelude hiding ( sum )



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
  deriving (Show, Data)

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
  deriving (Show, Data)

data ExferenceInputError
  = NestedForallInGoal HsType
  | NestedForallInBinding QualifiedName HsType
  | NestedForallInDeconstructor HsType
  | InvalidMaxSteps Int
  | InvalidMaxQueueSize Int
  | InvalidMaxDepth Penalty
  | InvalidHeuristic String Penalty
  deriving (Eq, Show)

type ExferenceOutputElement = (Expression, [HsConstraint], ExferenceStats)
data SearchCompletion
  = SearchRunning
  | SearchExhausted
  | SearchStepLimitReached
  deriving (Eq, Show)

data SearchStatus = SearchStatus
  { searchCompletion :: SearchCompletion
  , searchQueuePruned :: Int
  , searchDepthPruned :: Int
  }
  deriving (Eq, Show)

data ExferenceChunkElement = ExferenceChunkElement
  { chunkStatus :: SearchStatus
  , chunkBindingUsages :: BindingUsages
  , chunkElements :: [ExferenceOutputElement]
  }

type RatedNodes = Q.MaxPQueue Priority SearchNode
data FindExpressionsState = FindExpressionsState
  { _findExpressionsStateN :: Int    -- number of steps already performed
  , _findExpressionsStateQueuePruned :: Int
  , _findExpressionsStateDepthPruned :: Int
  , _findExpressionsStateBindingUsages :: BindingUsages
  , _findExpressionsStateStates :: RatedNodes -- pqueue
  }
makeFields ''FindExpressionsState

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
findExpressions :: ExferenceInput
                -> [ExferenceChunkElement]
findExpressions (ExferenceInput rawType
                                funcs
                                deconss'
                                sClassEnv
                                allowUnused
                                allowConstraints
                                allowConstraintsStopStep
                                multiPM
                                maxSteps
                                maxQueueSize
                                maxDepth
                                heuristics) =
  unfoldr helper rootFindExpressionState
 where
  rootFindExpressionState = FindExpressionsState
    0
    0
    0
    M.empty
    (Q.singleton 0 rootSearchNode)
  t = forallify rawType
  rootSearchNode = SearchNode
    { _searchNodeGoals           = Seq.singleton
        (TGoal (VarBinding 0 t) initialScopeId)
    , _searchNodeConstraintGoals = []
    , _searchNodeProvidedScopes  = initialScopes
    , _searchNodeVarUses         = IntMap.empty
    , _searchNodeFunctions       = (V.fromList funcs) -- TODO: lift this further up?
    , _searchNodeDeconss         = deconss'
    , _searchNodeQueryClassEnv   = (mkQueryClassEnv sClassEnv [])
    , _searchNodeExpression      = (ExpHole 0)
    , _searchNodeNextVarId       = 1 -- TODO: change to 0?
    , _searchNodeMaxTVarId       = (largestId t)
    , _searchNodeNextNVarId      = 0
    , _searchNodeDepth           = 0.0
    , _searchNodeLastStepReason  = ""
    , _searchNodeLastStepBinding = Nothing
    }
  transformSolutions :: [SearchNode] -> FindExpressionsState -> ExferenceChunkElement
  transformSolutions potentialSolutions (FindExpressionsState
      n'
      totalQueuePruned
      totalDepthPruned
      newBindingUsages
      newNodes
    ) = ExferenceChunkElement
      (SearchStatus completion totalQueuePruned totalDepthPruned)
      newBindingUsages
      [ (e, remainingConstraints, ExferenceStats n' d $ Q.size newNodes)
      | solution <- potentialSolutions
      , let contxt = view queryClassEnv solution
      , remainingConstraints <- maybeToList
                              $ filterUnresolved contxt
                              $ view constraintGoals solution
        -- if allowConstraints, unresolved constraints are allowed;
        -- otherwise we discard this solution.
      , allowConstraints || null remainingConstraints
      , let unusedVarCount = getUnusedVarCount solution
        -- similarly:
        -- if allowUnused, there may be unused variables in the
        -- output. Otherwise the solution is discarded.
      , allowUnused || unusedVarCount==0
      , let e = view expression solution
      , Right () <- [checkExpression contxt funcs deconss' t remainingConstraints e]
      , let d = view depth solution
              + ( heuristics_unusedVar heuristics
                * fromIntegral unusedVarCount
                )
              -- + ( heuristics_solutionLength heuristics
              --   * fromIntegral (length $ show e)<
              --   )
      ]
    where
      completion
        | Q.null newNodes = SearchExhausted
        | n' >= maxSteps = SearchStepLimitReached
        | otherwise = SearchRunning
  helper :: FindExpressionsState -> Maybe (ExferenceChunkElement, FindExpressionsState)
  helper state | view n state >= maxSteps = Nothing
  helper state = runStateT (do
    s <- zoom states $ StateT Q.maxView
    n' <- n <<+= 1
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
      (potentialSolutions, futures) = partition (views goals Seq.null) withinDepth
      ratedNew =
        [ ( rateNode heuristics newS + Priority (4.5*f (fromIntegral n'))
          , newS)
        | newS <- futures
        , let f :: Double -> Double
              f x | x > 900 = 0.0
                  | otherwise = let k = 1.111e-3*x
                                 in 1 + 2*k**3 - 3*k**2
        ]
      depthAllowed node = maybe True (view depth node <=) maxDepth
    bindingUsages . maybe ignored at (view lastStepBinding s) . non 0 += 1
    depthPruned += length tooDeep
    queued <- use states
    let combined = Q.union queued (Q.fromList ratedNew)
        (retained, queueDiscarded) = limitQueue maxQueueSize combined
    states .= retained
    queuePruned += queueDiscarded
    gets $ transformSolutions potentialSolutions) state

constraintsRelaxedAtStep :: Bool -> Int -> Int -> Bool
constraintsRelaxedAtStep allowConstraints stopStep currentStep =
  allowConstraints || currentStep <= stopStep

validateExferenceInput :: ExferenceInput -> Either ExferenceInputError ()
validateExferenceInput input
  | input_maxSteps input <= 0 = Left $ InvalidMaxSteps $ input_maxSteps input
  | Just limit <- input_maxQueueSize input, limit < 0 =
      Left $ InvalidMaxQueueSize limit
  | Just limit <- input_maxDepth input, not $ isFinitePenalty limit =
      Left $ InvalidMaxDepth limit
  | Just (field, invalid) <- find (not . isFinitePenalty . snd)
      (heuristicFields $ input_heuristicsConfig input) =
      Left $ InvalidHeuristic field invalid
  -- Historical function ratings are signed: negative values are bonuses.
  -- Heuristic penalties above remain non-negative, but conflating the two
  -- policies makes the shipped environment fail validation.
  | Just binding <- find (not . isFiniteRating . functionPenalty)
      (input_envFuncs input) = Left $ InvalidHeuristic
        (show $ functionName binding) (functionPenalty binding)
  | containsNestedForall $ input_goalType input =
      Left $ NestedForallInGoal $ input_goalType input
  | Just binding <- find (containsForall . functionBindingType)
      (input_envFuncs input) =
      Left $ NestedForallInBinding (functionName binding) $ functionBindingType binding
  | Just deconstructor <- find (containsForall . deconstructorBindingType)
      (input_envDeconsS input) =
      Left $ NestedForallInDeconstructor $ deconstructorBindingType deconstructor
  | otherwise = Right ()

isFiniteRating :: Penalty -> Bool
isFiniteRating = \rating -> let value = penaltyValue rating
  in not (isNaN value || isInfinite value)

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

containsNestedForall :: HsType -> Bool
containsNestedForall ty@TypeForall{} = containsForall $ stripOuterForalls ty
  where
    stripOuterForalls (TypeForall _ _ body) = stripOuterForalls body
    stripOuterForalls other = other
containsNestedForall ty = containsForall ty

containsForall :: HsType -> Bool
containsForall TypeForall{} = True
containsForall (TypeArrow parameter result) =
  containsForall parameter || containsForall result
containsForall (TypeApp function parameter) =
  containsForall function || containsForall parameter
containsForall _ = False

rateNode :: ExferenceHeuristicsConfig -> SearchNode -> Priority
rateNode h s = Priority
  $ negate (penaltyValue (rateGoals h $ view goals s)
            + penaltyValue (view depth s))
  + priorityValue (rateUsage h s)

rateGoals :: ExferenceHeuristicsConfig -> Seq.Seq TGoal -> Penalty
rateGoals h = sum . fmap rateGoal
  where
    rateGoal (TGoal (VarBinding _ t) _) = tComplexity t
    -- TODO: actually measure performance with different values,
    --       use derived values instead of (arbitrarily) chosen ones.
    tComplexity (TypeVar _)         = heuristics_goalVar h
    tComplexity (TypeConstant _)    = heuristics_goalCons h -- TODO different heuristic?
    tComplexity (TypeCons _)        = heuristics_goalCons h
    tComplexity (TypeArrow t1 t2)   = heuristics_goalArrow h + tComplexity t1 + tComplexity t2
    tComplexity (TypeApp   t1 t2)   = heuristics_goalApp h   + tComplexity t1 + tComplexity t2
    tComplexity (TypeForall _ _ t1) = tComplexity t1

rateUsage :: ExferenceHeuristicsConfig -> SearchNode -> Priority
rateUsage h = Priority . sumOf (varUses . folded . to f) where
  f :: Int -> Double
  f 0 = negate $ penaltyValue $ heuristics_tempUnusedVarPenalty h
  f 1 = 0
  f k = negate $ fromIntegral (k-1)
    * penaltyValue (heuristics_tempMultiVarUsePenalty h)

getUnusedVarCount :: SearchNode -> Int
getUnusedVarCount s = length $ filter (==0) $ s ^.. varUses . folded

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
  contxt <- use queryClassEnv
  constraintGoals' <- use constraintGoals

  (TGoal (VarBinding var goalType) scopeId Seq.:< gr) <- Seq.viewl <$> use goals
  goals .= gr

  let
    -- if type is TypeArrow, transform to lambda expression.
    arrowStep :: MonadState SearchNode m => HsType -> [VarBinding] -> m ()
    arrowStep g ts
      -- descend until no more TypeArrows, accumulating what is seen.
      | TypeArrow t1 t2 <- g = do
          nextId <- builderAllocVar
          arrowStep t2 (VarBinding nextId t1 : ts)
      -- finally, do the goal/expression transformation.
      | otherwise = do
          nextId <- nextVarId <<+= 1
          newScopeId <- builderAddScope scopeId
          expression %= fillExprHole var
            (foldl (\e (VarBinding v ty) -> ExpLambda v ty e) (ExpHole nextId) ts)
          depth += heuristics_functionGoalTransform h
          builderSetReason "function goal transform"
          lastStepBinding .= Nothing
          -- for each parameter introduced in the lambda-expression above,
          -- it may be possible to pattern-match. and pattern-matching
          -- may cause duplication of the goals (e.g. for the different cases
          -- in the pattern match).
          additionalGoals <- addScopePatternMatch multiPM g nextId newScopeId
            $ map splitBinding
            $ reverse ts
          goals <>= Seq.fromList additionalGoals

    -- if type is TypeForall, fix the forall-variables, i.e. invent a fresh
    -- set of constants that replace the relevant forall-variables.
    forallStep :: MonadState SearchNode m => [TVarId] -> [HsConstraint] -> HsType -> m ()
    forallStep vs cs t = do
      dataIds <- mapM (const $ nextNVarId <<+= 1) vs
      depth += heuristics_functionGoalTransform h -- TODO: different heuristic?
      builderSetReason "forall-type goal transformation"
      lastStepBinding .= Nothing
      let substs = IntMap.fromList $ zip vs $ TypeConstant <$> dataIds
      goals %= (TGoal (VarBinding var $ snd $ applySubsts substs t) scopeId <|)
      queryClassEnv %= addQueryClassEnv (snd . constraintApplySubsts substs <$> cs)
    -- try to resolve the goal by looking at the parameters in scope, i.e.
    -- the parameters accumulated by building the expression so far.
    -- e.g. for (\x -> (_ :: Int)), the goal can be filled by `x` if
    -- `x :: Int`.

    byProvided :: StateT SearchNode [] ()
    byProvided = do
      provided <- lift =<< uses providedScopes (scopeGetAllBindings scopeId)
      offset <- uses maxTVarId (+1)
      let
        provId      = varPVariable provided
        provT       = varPResult provided
        provPs      = varPParameters provided
        forallTypes = varPForallVariables provided
        constraints = varPConstraints provided
        incF        = incVarIds (+offset)
        ss          = IntMap.fromList $ zip forallTypes (incF . TypeVar <$> forallTypes)
        provType    = snd $ applySubsts ss provT
        provConstrs = S.toList $ S.union
          (qClassEnv_constraints contxt)
          (S.fromList (snd . constraintApplySubsts ss <$> constraints))
      mapStateT maybeToList $ byGenericUnify
        (Right (provId, foldr TypeArrow provT provPs))
        provType
        provConstrs
        (snd . applySubsts ss <$> provPs)
        (heuristics_stepProvidedGood h)
        (heuristics_stepProvidedBad h)
        ("inserting given value " ++ show provId ++ "::" ++ show provT)
        (unify goalType provType)

    -- try to resolve the goal by looking at functions from the environment.
    byFunctionSimple :: StateT SearchNode [] ()
    byFunctionSimple = do
      binding <- lift =<< uses functions V.toList
      offset <- uses maxTVarId (+1)
      let
        incF     = incVarIds (+offset)
        provType = incF $ functionResult binding
      mapStateT maybeToList $ byGenericUnify
        (Left $ functionName binding)
        provType
        (map (constraintMapTypes incF) $ functionConstraints binding)
        (map incF $ functionParameters binding)
        (heuristics_stepEnvGood h + functionPenalty binding)
        (heuristics_stepEnvBad h + functionPenalty binding)
        ("applying function " ++ show (functionName binding))
        (unifyOffset goalType
          $ HsTypeOffset (functionResult binding) offset)

    -- on code for byProvided and byFunctionSimple
    byGenericUnify :: Either QualifiedName (TVarId, HsType)
                   -> HsType
                   -> [HsConstraint]
                   -> [HsType]
                   -> Penalty
                   -> Penalty
                   -> String
                   -> Maybe (Substs, Substs)
                   -> StateT SearchNode Maybe ()
    byGenericUnify applier
                   provided
                   provConstrs
                   dependencies
                   depthModMatch
                   depthModNoMatch
                   reasonPart
      = maybe noUnify $ uncurry byUnified
     where
      applierl = applier ^? _Left
      applierr = applier ^? _Right
      coreExp = either ExpName (uncurry ExpVar) applier

      noUnify :: StateT SearchNode Maybe ()
      noUnify = case dependencies of
        [] -> mzero -- we can't (randomly) partially apply a non-function
        (d:ds) -> do
          vResult <- builderAllocVar
          vParam  <- nextVarId <<+= 1
          expression %= fillExprHole var (ExpLet
            vResult
            provided
            (ExpApply coreExp $ ExpHole vParam)
            (ExpHole var))
          goals %= (TGoal (VarBinding vParam d) scopeId <|)
          newScopeId <- builderAddScope scopeId
          constraintGoals <>= provConstrs
          traverse_ (\r -> varUses . singular (ix $ fst r) += 1) applierr
          maxTVarId %= max (maximum $ map largestId dependencies)
          depth += depthModNoMatch
          builderSetReason $ "randomly trying to apply function "
                            ++ showExpression coreExp
          additionalGoals <- addScopePatternMatch
            multiPM
            goalType
            var
            newScopeId
            (let (r, ps, fs, cs) = splitArrowResultParams provided
              in [VarPBinding vResult r (ds ++ ps) fs cs])
          goals <>= Seq.fromList additionalGoals

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
        vars <- replicateM paramN $ nextVarId <<+= 1
        let newGoals = mkGoals scopeId $ zipWith VarBinding vars dependencies
        goals <>= Seq.fromList
          (ala Endo foldMap (applierl $> goalApplySubst provSS)
          <$> newGoals)
        builderApplySubst substs
        expression %= fillExprHole var
          (foldl ExpApply coreExp (map ExpHole vars))
        traverse_ (\r -> varUses . singular (ix $ fst r) += 1) applierr
        constraintGoals .= newConstraints
        maxTVarId %= max (maximum
          $ largestSubstsId goalSS : map largestId dependencies)
        depth += depthModMatch
        let substsTxt   = show (IntMap.union goalSS provSS)
                          ++ " unifies "
                          ++ show goalType
                          ++ " and "
                          ++ show provided
        let provableTxt = "constraints (" ++ show (constrs1++constrs2)
                                          ++ ") are provable"
        builderSetReason $ reasonPart ++ ", because " ++ substsTxt
                          ++ " and because " ++ provableTxt
        lastStepBinding .= fmap show applierl

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
addScopePatternMatch :: MonadState SearchNode m
                     => Bool -- should p-m on anything but newtypes?
                     -> HsType -- the current goal (should be returned in one
                               --  form or another)
                     -> Int    -- goal id (hole id)
                     -> ScopeId -- scope for this goal
                     -> [VarPBinding]
                     -> m [TGoal]
addScopePatternMatch multiPM goalType vid sid bindings = case bindings of
  [] -> return [TGoal (VarBinding vid goalType) sid]
  (b : bindingRest) -> do
    let v = varPVariable b
        vtResult = varPResult b
        vtParams = varPParameters b
    offset <- builderGetTVarOffset
    let incF = incVarIds (+offset)
    let expVar = ExpVar v (foldr TypeArrow vtResult vtParams)
    providedScopes %= scopesAddPBinding sid b
    let defaultHandleRest = addScopePatternMatch multiPM goalType vid sid bindingRest
    case vtResult of
      TypeVar {}    -> defaultHandleRest -- dont pattern-match on variables, even if it unifies
      TypeArrow {}  ->
        error $ "addScopePatternMatch: TypeArrow: " ++ show vtResult  -- should never happen, given a pbinding..
      TypeForall {} ->
        error $ "addScopePatternMatch: TypeForall (RankNTypes not yet implemented)" -- todo when we do RankNTypes
                ++ show vtResult
      _ | not $ null vtParams -> defaultHandleRest
        | otherwise -> fromMaybe defaultHandleRest . asum . map mapFunc =<< use deconss
         where
          mapFunc :: MonadState SearchNode m => DeconstructorBinding -> Maybe (m [TGoal])
          mapFunc (DeconstructorBinding matchParam
                    [ConstructorBinding matchId matchRs] False) = let
            resultTypes = map incF matchRs
            unifyResult = unifyRightOffset vtResult
                                           (HsTypeOffset matchParam offset)
            -- inputType = incF matchParam
            mapFunc1 substs = do -- m
              vars <- replicateM (length matchRs) builderAllocVar
              varUses . singular (ix v) += 1
              builderAppendReason $ "pattern matching on " ++ showVar v
                ++ "\n" ++ intercalate "\n" 
                  [ show bindings
                  , show offset
                  , show (matchParam, matchId, matchRs)
                  , show (vtResult, matchParam, offset)
                  , show unifyResult
                  ]
              let newProvTypes = map (snd . applySubsts substs) resultTypes
                  newBinds = zipWith (\x y -> splitBinding (VarBinding x y))
                                     vars
                                     newProvTypes
                  expr = ExpLetMatch matchId
                                     (zip vars matchRs)
                                     expVar
                                     (ExpHole vid)
              expression %= fillExprHole vid expr
              unless (null matchRs) $
                maxTVarId %= max (maximum $ map largestId newProvTypes)
              addScopePatternMatch multiPM
                                   goalType
                                   vid
                                   sid
                                   (reverse newBinds ++ bindingRest)
            in liftM mapFunc1 unifyResult
          mapFunc (DeconstructorBinding matchParam matchers@(_ : _) False)
            | multiPM = let
            unifyResult = unifyRightOffset vtResult
                                           (HsTypeOffset matchParam offset)
            -- inputType = incF matchParam
            mapFunc2 substs = do -- m
              mData <- matchers `forM` \matcher -> do -- m
                let matchId = constructorName matcher
                    matchRs = constructorFields matcher
                newSid <- builderAddScope sid
                let resultTypes = map incF matchRs
                vars <- replicateM (length matchRs) builderAllocVar
                varUses . singular (ix v) += 1
                newVid <- nextVarId <<+= 1
                let newProvTypes = map (snd . applySubsts substs) resultTypes
                    newBinds = zipWith (\x y -> splitBinding (VarBinding x y)) vars newProvTypes
                unless (null matchRs) $
                  maxTVarId %= max (maximum $ map largestId newProvTypes)
                return ( (matchId, zip vars newProvTypes, ExpHole newVid)
                       , (newVid, reverse newBinds, newSid) )
              builderAppendReason $ "pattern matching on " ++ showVar v
              expression %= fillExprHole vid (ExpCaseMatch expVar $ map fst mData)
              liftM concat $ map snd mData `forM` \(newVid, newBinds, newSid) ->
                addScopePatternMatch multiPM goalType newVid newSid (newBinds++bindingRest)
            in liftM mapFunc2 unifyResult
          mapFunc _ = Nothing -- TODO: decons for recursive data types
  -- where
  --  (<&>) = flip (<$>)
