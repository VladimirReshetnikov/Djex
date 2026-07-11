module Language.Haskell.Exference.Core.ExpressionCheck
  ( ExpressionCheckError (..)
  , checkExpression
  )
where

import Control.Monad (foldM, unless, when)
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set

import Language.Haskell.Exference.Core.Expression
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.Core.Internal.ConstraintSolver
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
  deriving (Eq, Show)

data CheckState = CheckState
  { checkNextVariable :: !TVarId
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
  let checkedGoal = instantiateGoal goal
      initialState = CheckState
        { checkNextVariable = 1 + maximum (largestId checkedGoal : expressionTypeIds expression)
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
  variable <- gets checkNextVariable
  modify' $ \current -> current {checkNextVariable = variable + 1}
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

instantiateGoal :: HsType -> HsType
instantiateGoal (TypeForall variables _ body) =
  let substitutions = IntMap.fromList
        $ zip variables (map TypeConstant [0 ..])
  in snd $ applySubsts substitutions body
instantiateGoal goal = goal

expressionTypeIds :: Expression -> [TVarId]
expressionTypeIds (ExpVar _ ty) = [largestId ty]
expressionTypeIds ExpName{} = []
expressionTypeIds (ExpLambda _ ty body) = largestId ty : expressionTypeIds body
expressionTypeIds (ExpApply function argument) =
  expressionTypeIds function ++ expressionTypeIds argument
expressionTypeIds ExpHole{} = []
expressionTypeIds (ExpLetMatch _ variables binding body) =
  map (largestId . snd) variables ++ expressionTypeIds binding ++ expressionTypeIds body
expressionTypeIds (ExpLet _ ty binding body) =
  largestId ty : expressionTypeIds binding ++ expressionTypeIds body
expressionTypeIds (ExpCaseMatch scrutinee alternatives) =
  expressionTypeIds scrutinee
  ++ [ identifier
     | (_, variables, body) <- alternatives
     , identifier <- map (largestId . snd) variables ++ expressionTypeIds body
     ]
