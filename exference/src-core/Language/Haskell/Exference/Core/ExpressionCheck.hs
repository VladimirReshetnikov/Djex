module Language.Haskell.Exference.Core.ExpressionCheck
  ( ExpressionCheckError (..)
  , checkExpression
  , checkExpressionWithRigidInstantiation
  )
where

import Control.Monad (foldM, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, gets, modify', runStateT)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

import Language.Haskell.Exference.Core.Expression
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.Core.Internal.ConstraintSolver
import Language.Haskell.Exference.Core.Internal.FlexibleIds
import Language.Haskell.Exference.Core.RigidInstantiation
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.Core.Types

data ExpressionCheckError
  = UnknownVariable TVarId
  | UnknownBinding QualifiedName
  | UnknownConstructor QualifiedName
  | ExpressionHole TVarId
  | PatternArity QualifiedName Int Int
  | TypeMismatch HsType HsType
  | InfiniteType TVarId HsType
  | UnsupportedNestedForall HsType
  | RefutableConstraints [HsConstraint]
  | ConstraintMismatch [HsConstraint] [HsConstraint]
  | RigidInstantiationFailure RigidInstantiationError
  | RigidInstantiationPlanMismatch [TVarId] [TVarId]
  | FlexibleIdentifierSupplyExhausted
  deriving (Eq, Show)

data CheckState = CheckState
  { checkFlexibleIds :: !FlexibleIdSupply
  , checkSubstitutions :: !Substs
  , checkConstraints :: [HsConstraint]
  }

type VariableEnvironment = IntMap.IntMap HsType
type Check a = StateT CheckState (Either ExpressionCheckError) a

-- | Independently reconstruct and check a generated expression. This checker
-- does not reuse the search unifier or node transformations; it sees only the
-- final expression, the declared environment, and the requested type.
checkExpression
  :: QueryClassEnv
  -> [FunctionBinding]
  -> [DeconstructorBinding]
  -> HsType
  -> [HsConstraint]
  -> Expression
  -> Either ExpressionCheckError ()
checkExpression classEnvironment functions deconstructors goal expected expression = do
  plan <- either (Left . RigidInstantiationFailure) Right
    $ planRigidInstantiation
        (mkRigidInstantiationContext $ EnvDictionary
          functions deconstructors $ qClassEnv_env classEnvironment)
        (Set.toList $ qClassEnv_constraints classEnvironment)
        goal
  checkExpressionWithRigidInstantiation plan classEnvironment functions
    deconstructors goal expected expression

-- | Check using the same precomputed forall-opening plan as live search.
-- Generated annotations, residual constraints, and the query assumptions in
-- the live 'QueryClassEnv' may already contain this plan's rigid variables, so
-- none of them may be mistaken for pre-existing input when checking a result.
checkExpressionWithRigidInstantiation
  :: RigidInstantiationPlan
  -> QueryClassEnv
  -> [FunctionBinding]
  -> [DeconstructorBinding]
  -> HsType
  -> [HsConstraint]
  -> Expression
  -> Either ExpressionCheckError ()
checkExpressionWithRigidInstantiation plan classEnvironment functions
    deconstructors goal expected expression = do
  checkedGoal <- instantiateGoal plan goal
  let
      initialState = CheckState
        { checkFlexibleIds = supplyFromIdentifiers $ IntSet.toAscList
            $ IntSet.union
                (flexibleIdentifiers checkedGoal)
                (expressionFlexibleIdentifiers expression)
        , checkSubstitutions = IntMap.empty
        , checkConstraints = []
        }
  (_, finalState) <- runStateT
    (infer IntMap.empty expression >>= (`unifyTypes` checkedGoal))
    initialState
  let substitutions = checkSubstitutions finalState
      inferredConstraints = map (snd . constraintApplySubsts substitutions)
        $ checkConstraints finalState
      normalizedExpected = Set.toAscList $ Set.fromList expected
  unresolved <- maybe
    (Left $ RefutableConstraints inferredConstraints)
    (Right . Set.toAscList . Set.fromList)
    (filterUnresolved classEnvironment inferredConstraints)
  unless (unresolved == normalizedExpected)
    $ Left (ConstraintMismatch normalizedExpected unresolved)
  where
    infer :: VariableEnvironment -> Expression -> Check HsType
    infer variables (ExpVar variable annotation) = do
      declared <- maybe (throwCheck $ UnknownVariable variable) pure
        $ IntMap.lookup variable variables
      unifyTypes declared annotation
      zonk declared
    infer _ (ExpName name) = instantiateBinding name
    infer variables (ExpLambda variable annotation body) =
      TypeArrow annotation <$> infer (IntMap.insert variable annotation variables) body
    infer variables (ExpApply function argument) = do
      functionType <- infer variables function
      argumentType <- infer variables argument
      resultType <- freshTypeVariable
      unifyTypes functionType (TypeArrow argumentType resultType)
      zonk resultType
    infer _ (ExpHole variable) = throwCheck $ ExpressionHole variable
    infer variables (ExpLetMatch constructor patternVariables binding body) = do
      bindingType <- infer variables binding
      fieldTypes <- instantiateConstructor constructor bindingType
      when (length patternVariables /= length fieldTypes)
        $ throwCheck $ PatternArity constructor
            (length fieldTypes) (length patternVariables)
      checkedVariables <- foldM addPatternVariable variables
        $ zip patternVariables fieldTypes
      infer checkedVariables body
    infer variables (ExpLet variable annotation binding body) = do
      bindingType <- infer variables binding
      unifyTypes annotation bindingType
      infer (IntMap.insert variable annotation variables) body
    infer variables (ExpCaseMatch scrutinee alternatives) = do
      scrutineeType <- infer variables scrutinee
      resultType <- freshTypeVariable
      mapM_ (checkAlternative variables scrutineeType resultType) alternatives
      zonk resultType

    addPatternVariable environment ((variable, annotation), inferredType) = do
      unifyTypes annotation inferredType
      pure $ IntMap.insert variable inferredType environment

    checkAlternative variables scrutineeType resultType
        (constructor, patternVariables, body) = do
      fieldTypes <- instantiateConstructor constructor scrutineeType
      when (length patternVariables /= length fieldTypes)
        $ throwCheck $ PatternArity constructor
            (length fieldTypes) (length patternVariables)
      checkedVariables <- foldM addPatternVariable variables
        $ zip patternVariables fieldTypes
      alternativeType <- infer checkedVariables body
      unifyTypes resultType alternativeType

    instantiateBinding name = case
        [binding | binding <- functions, functionName binding == name] of
      [] -> throwCheck $ UnknownBinding name
      binding : _ -> do
        let result = functionResult binding
            constraints = functionConstraints binding
            parameters = functionParameters binding
        (freshTypes, freshConstraints) <- freshenTypes
          [foldr TypeArrow result parameters] constraints
        freshType <- case freshTypes of
          [ty] -> pure ty
          _ -> throwCheck $ UnknownBinding name
        modify' $ \current -> current
          { checkConstraints = freshConstraints ++ checkConstraints current }
        pure freshType

    instantiateConstructor name scrutineeType = case
        [ (deconstructorInput deconstructor, constructorFields alternative)
        | deconstructor <- deconstructors
        , alternative <- deconstructorConstructors deconstructor
        , constructorName alternative == name
        ] of
      [] -> throwCheck $ UnknownConstructor name
      (input, fields) : _ -> do
        (freshTypes, _) <- freshenTypes (input : fields) []
        (freshInput, freshFields) <- case freshTypes of
          firstType : remainingTypes -> pure (firstType, remainingTypes)
          [] -> throwCheck $ UnknownConstructor name
        unifyTypes scrutineeType freshInput
        mapM zonk freshFields

throwCheck :: ExpressionCheckError -> Check a
throwCheck = lift . Left

freshTypeVariable :: Check HsType
freshTypeVariable = do
  supply <- gets checkFlexibleIds
  case allocateFreshIdentifier supply of
    Nothing -> throwCheck FlexibleIdentifierSupplyExhausted
    Just (variable, nextSupply) -> do
      modify' $ \current -> current {checkFlexibleIds = nextSupply}
      pure $ TypeVar variable

freshenTypes :: [HsType] -> [HsConstraint] -> Check ([HsType], [HsConstraint])
freshenTypes types constraints = do
  let variables = Set.toAscList
        $ foldMap freeVars types
        `Set.union` foldMap (foldMap freeVars . constraint_params) constraints
  replacements <- mapM (const freshTypeVariable) variables
  let substitutions = IntMap.fromList $ zip variables replacements
  pure
    ( map (snd . applySubsts substitutions) types
    , map (snd . constraintApplySubsts substitutions) constraints
    )

unifyTypes :: HsType -> HsType -> Check ()
unifyTypes left right = do
  left' <- zonk left
  right' <- zonk right
  case (left', right') of
    _ | left' == right' -> pure ()
    (TypeVar variable, ty) -> bindVariable variable ty
    (ty, TypeVar variable) -> bindVariable variable ty
    (TypeArrow leftParameter leftResult, TypeArrow rightParameter rightResult) ->
      unifyTypes leftParameter rightParameter >> unifyTypes leftResult rightResult
    (TypeApp leftFunction leftArgument, TypeApp rightFunction rightArgument) ->
      unifyTypes leftFunction rightFunction >> unifyTypes leftArgument rightArgument
    (TypeForall{}, _) -> throwCheck $ UnsupportedNestedForall left'
    (_, TypeForall{}) -> throwCheck $ UnsupportedNestedForall right'
    _ -> throwCheck $ TypeMismatch left' right'

bindVariable :: TVarId -> HsType -> Check ()
bindVariable variable ty
  | containsVar variable ty = throwCheck $ InfiniteType variable ty
  | otherwise = modify' $ \current -> current
      { checkSubstitutions = IntMap.insert variable ty
          $ IntMap.map (applySubst $ Subst variable ty)
          $ checkSubstitutions current
      }

zonk :: HsType -> Check HsType
zonk ty = do
  substitutions <- gets checkSubstitutions
  let (_, applied) = applySubsts substitutions ty
  if applied == ty then pure ty else zonk applied

instantiateGoal
  :: RigidInstantiationPlan
  -> HsType
  -> Either ExpressionCheckError HsType
instantiateGoal plan goal
  | plannedBinders /= actualBinders = Left
      $ RigidInstantiationPlanMismatch plannedBinders actualBinders
  | otherwise = Right $ instantiateFrom instantiations quantifiedGoal
 where
  instantiations = rigidInstantiations plan
  plannedBinders = map fst instantiations
  quantifiedGoal = forallify goal
  actualBinders = collectBinders quantifiedGoal

  -- Validation permits a chain of prenex quantifiers.  Search consumes one
  -- layer per step, so consume the same ordered segment for each layer.  A
  -- single IntMap for the whole chain would collapse legal shadowed IDs.
  instantiateFrom remaining (TypeForall variables _ body) =
    let (current, rest) = splitAt (length variables) remaining
        substitutions = IntMap.fromList
          [(variable, TypeConstant rigid) | (variable, rigid) <- current]
        instantiatedBody = snd $ applySubsts substitutions body
    in instantiateFrom rest instantiatedBody
  instantiateFrom _ instantiated = instantiated

  collectBinders (TypeForall variables _ body) =
    variables ++ collectBinders body
  collectBinders _ = []

expressionFlexibleIdentifiers :: Expression -> IntSet.IntSet
expressionFlexibleIdentifiers expression = case expression of
  ExpVar _ ty -> flexibleIdentifiers ty
  ExpName{} -> IntSet.empty
  ExpLambda _ ty body -> flexibleIdentifiers ty
    `IntSet.union` expressionFlexibleIdentifiers body
  ExpApply function argument -> expressionFlexibleIdentifiers function
    `IntSet.union` expressionFlexibleIdentifiers argument
  ExpHole{} -> IntSet.empty
  ExpLetMatch _ variables binding body -> IntSet.unions
    $ map (flexibleIdentifiers . snd) variables
    ++ [ expressionFlexibleIdentifiers binding
       , expressionFlexibleIdentifiers body
       ]
  ExpLet _ ty binding body -> IntSet.unions
    [ flexibleIdentifiers ty
    , expressionFlexibleIdentifiers binding
    , expressionFlexibleIdentifiers body
    ]
  ExpCaseMatch scrutinee alternatives -> IntSet.unions
    $ expressionFlexibleIdentifiers scrutinee
    : [ IntSet.unions
          $ map (flexibleIdentifiers . snd) variables
          ++ [expressionFlexibleIdentifiers body]
      | (_, variables, body) <- alternatives
      ]
