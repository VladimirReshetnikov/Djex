-- | Independent validation of Exference's typed generated expressions.
--
-- This module deliberately reconstructs types without trusting the search
-- tree. Raw environments cross the same binding-identity and deconstructor
-- checks as live search before any name lookup, so a successful check cannot
-- depend on which duplicate declaration happened to appear first.
module Language.Haskell.Exference.Core.ExpressionCheck
  ( ExpressionCheckError (..)
  , checkExpression
  , checkExpressionWithRigidInstantiation
  )
where

import Control.Monad (foldM, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT (..), gets, modify', runStateT)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set

import Language.Haskell.Exference.Core.Expression
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.Core.ConstraintSolver
import Language.Haskell.Exference.Core.Internal.FlexibleIds
import Language.Haskell.Exference.Core.Internal.VariableSupply
import Language.Haskell.Exference.Core.RigidInstantiation
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.Core.Types
import qualified Language.Haskell.Synthesis.Type as SharedType

data ExpressionCheckError
  = UnknownVariable TVarId
  | UnknownBinding QualifiedName
  | UnknownConstructor QualifiedName
  | EmptyCaseWithoutMatchingDeconstructor HsType
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
  | InvalidCheckType HsType SynthesisTypeError
  | InvalidCheckConstraint HsConstraint SynthesisTypeError
  | InvalidCheckClassConstraint ClassEnvError
  | InvalidCheckEnvironmentBindings EnvironmentDuplicateError
  | InvalidCheckDeconstructor DeconstructorValidationError
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
  validateCheckInputs classEnvironment functions deconstructors goal expected
    expression
  (checkedGoal, openedConstraints) <- instantiateGoal plan goal
  let
      -- Live search adds each opened forall layer's rigid-instantiated
      -- constraints to its query assumptions in forallStep; an independent
      -- caller checking a constrained goal must receive the same assumptions
      -- or it would falsely reject search's own accepted results.
      augmentedEnvironment =
        addQueryClassEnv openedConstraints classEnvironment
      initialState = CheckState
        { checkFlexibleIds = supplyFromIdentifierSet
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
      -- The inferred side is canonicalized whenever the unifier bound a
      -- variable, so the caller-supplied side must be canonicalized too or a
      -- semantically equal application-form spelling would fail comparison.
      normalizedExpected = Set.toAscList $ Set.fromList
        $ map (fmap SharedType.canonicalizeType) expected
  unresolved <- maybe
    (Left $ RefutableConstraints inferredConstraints)
    (Right . Set.toAscList . Set.fromList)
    (filterUnresolved augmentedEnvironment inferredConstraints)
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
    infer variables (ExpCaseMatch scrutinee []) = do
      scrutineeType <- infer variables scrutinee >>= zonk
      matchEmptyDeconstructor scrutineeType
      -- Empty elimination proves every result type. Keep that result fresh so
      -- the surrounding expression, rather than the deconstructor, fixes it.
      freshTypeVariable
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
        let constraints = functionConstraints binding
        (freshType :| _, freshConstraints) <- freshenTypes
          (functionBindingType binding :| []) constraints
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
        (freshInput :| freshFields, _) <- freshenTypes (input :| fields) []
        unifyTypes scrutineeType freshInput
        mapM zonk freshFields

    -- Trying declarations from one unchanged state gives empty datatypes the
    -- same independent unification semantics as ordinary constructors while
    -- allowing more than one empty datatype in a raw checker environment.
    matchEmptyDeconstructor scrutineeType = StateT $ \initialState ->
      tryDeconstructors initialState
        [ deconstructor
        | deconstructor <- deconstructors
        , null $ deconstructorConstructors deconstructor
        ]
     where
      tryDeconstructors _ [] = Left
        $ EmptyCaseWithoutMatchingDeconstructor scrutineeType
      tryDeconstructors initialState (deconstructor : remaining) =
        case runStateT (instantiateEmpty deconstructor) initialState of
          Right matched -> Right matched
          Left FlexibleIdentifierSupplyExhausted ->
            Left FlexibleIdentifierSupplyExhausted
          Left _ -> tryDeconstructors initialState remaining

      instantiateEmpty deconstructor = do
        (freshInput :| _, _) <- freshenTypes
          (deconstructorInput deconstructor :| []) []
        unifyTypes scrutineeType freshInput

-- The independent checker is also a public raw-input boundary. Validate every
-- native type reachable from its arguments before equal malformed values can
-- short-circuit unification or a total-shaped compatibility helper observes
-- an invariant that only the sealed live-search path had established.
validateCheckInputs
  :: QueryClassEnv
  -> [FunctionBinding]
  -> [DeconstructorBinding]
  -> HsType
  -> [HsConstraint]
  -> Expression
  -> Either ExpressionCheckError ()
validateCheckInputs classEnvironment functions deconstructors goal expected
    expression = do
  case validateEnvironmentBindingIdentities $ EnvDictionary
      functions deconstructors $ qClassEnv_env classEnvironment of
    Left failure -> Left $ InvalidCheckEnvironmentBindings failure
    Right () -> Right ()
  validateType goal
  validateClassConstraints QueryConstraint $ typeConstraints goal
  mapM_ (validateConstraint QueryConstraint) expected
  mapM_ (validateConstraint QueryConstraint) $ Set.toAscList
    $ qClassEnv_constraints classEnvironment
  mapM_ validateFunction functions
  mapM_ validateDeconstructorTypes deconstructors
  mapM_ (validateType . snd) $ expressionTypedLocals expression
  mapM_ validateDeconstructor deconstructors
 where
  validateType typeExpression = case toSynthesisType typeExpression of
    Left failure -> Left $ InvalidCheckType typeExpression failure
    Right _ -> Right ()

  validateConstraint site constraint = do
    case toSynthesisConstraint constraint of
      Left failure -> Left $ InvalidCheckConstraint constraint failure
      Right _ -> Right ()
    validateClassConstraint site constraint

  validateClassConstraint site constraint = case validateKnownConstraintInEnv
      (qClassEnv_env classEnvironment) site constraint of
    Left failure -> Left $ InvalidCheckClassConstraint failure
    Right () -> Right ()

  validateClassConstraints site = mapM_ $ validateClassConstraint site

  validateFunction binding = do
    validateType $ functionResult binding
    mapM_ validateType $ functionParameters binding
    let site = BindingConstraint $ functionName binding
    mapM_ (validateConstraint site) $ functionConstraints binding
    validateClassConstraints site $ typeConstraints $ functionBindingType binding

  validateDeconstructorTypes = mapM_ validateType . deconstructorBindingTypes

  validateDeconstructor deconstructor = case
      validateDeconstructorBinding deconstructor of
    Left failure -> Left $ InvalidCheckDeconstructor failure
    Right () -> Right ()

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

-- Substitution is applied pointwise, so the nonempty output shape is the
-- input shape; callers destructure it without an impossible empty case.
freshenTypes
  :: NonEmpty HsType
  -> [HsConstraint]
  -> Check (NonEmpty HsType, [HsConstraint])
freshenTypes types constraints = do
  let variables = Set.toAscList
        $ foldMap freeVars types
        `Set.union` foldMap (foldMap freeVars . constraint_params) constraints
  replacements <- mapM (const freshTypeVariable) variables
  let substitutions = IntMap.fromList $ zip variables replacements
  pure
    ( fmap (snd . applySubsts substitutions) types
    , map (snd . constraintApplySubsts substitutions) constraints
    )

unifyTypes :: HsType -> HsType -> Check ()
unifyTypes left right = do
  left' <- zonk $ SharedType.canonicalizeType left
  right' <- zonk $ SharedType.canonicalizeType right
  case SharedType.firstForallType left' of
    Just quantified -> throwCheck $ UnsupportedNestedForall quantified
    Nothing -> pure ()
  case SharedType.firstForallType right' of
    Just quantified -> throwCheck $ UnsupportedNestedForall quantified
    Nothing -> pure ()
  case ( SharedType.constructorApplicationForm left'
       , SharedType.constructorApplicationForm right'
       ) of
    (leftForm, rightForm) | leftForm == rightForm -> pure ()
    (TypeVar variable, ty) -> bindVariable variable ty
    (ty, TypeVar variable) -> bindVariable variable ty
    (TypeApp leftFunction leftArgument, TypeApp rightFunction rightArgument) ->
      unifyTypes leftFunction rightFunction >> unifyTypes leftArgument rightArgument
    (TypeTuple leftBoxity leftElements, TypeTuple rightBoxity rightElements)
      | leftBoxity == rightBoxity
      , length leftElements == length rightElements ->
          mapM_ (uncurry unifyTypes) $ zip leftElements rightElements
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
      canonical = SharedType.canonicalizeType applied
  if canonical == ty then pure canonical else zonk canonical

-- | Open the goal's leading prenex chain exactly as live search does,
-- returning the instantiated body together with every opened layer's
-- rigid-instantiated constraints in outer-to-inner order.
instantiateGoal
  :: RigidInstantiationPlan
  -> HsType
  -> Either ExpressionCheckError (HsType, [HsConstraint])
instantiateGoal plan goal
  | plannedBinders /= actualBinders = Left
      $ RigidInstantiationPlanMismatch plannedBinders actualBinders
  | otherwise = Right $ instantiateFrom instantiations quantifiedGoal
 where
  instantiations = rigidInstantiations plan
  plannedBinders = map fst instantiations
  quantifiedGoal = forallify goal
  actualBinders = maybe [] id $ traverse SharedType.flexibleVariableIdentity
    $ SharedType.leadingForallVariables quantifiedGoal

  -- Validation permits a chain of prenex quantifiers.  Search consumes one
  -- layer per step, so consume the same ordered segment for each layer.  A
  -- single IntMap for the whole chain would collapse legal shadowed IDs.
  -- Outer substitutions rewrite the remaining body before the next layer
  -- opens, so inner layers' constraints already carry them, exactly as in
  -- the engine's forallStep.
  instantiateFrom remaining (TypeForall variables constraints body) =
    let (current, rest) =
          splitRigidInstantiationLayer variables remaining
        substitutions = IntMap.fromList
          [(variable, TypeConstant rigid) | (variable, rigid) <- current]
        layerConstraints = map
          (snd . constraintApplySubsts substitutions) constraints
        (instantiated, deeper) = instantiateFrom rest
          $ snd $ applySubsts substitutions body
    in (instantiated, layerConstraints ++ deeper)
  instantiateFrom _ instantiated = (instantiated, [])

expressionFlexibleIdentifiers :: Expression -> IntSet.IntSet
expressionFlexibleIdentifiers =
  foldMap (flexibleIdentifiers . snd) . expressionTypedLocals
