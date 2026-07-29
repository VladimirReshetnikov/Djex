-- | Independent validation of Exference's typed generated expressions.
--
-- This module reconstructs types without trusting the search tree. It shares
-- the pure unification kernel so opaque-polytype semantics cannot drift, while
-- raw environments still cross binding-identity and deconstructor checks
-- independently before any name lookup.
module Language.Haskell.Exference.Core.ExpressionCheck
  ( ExpressionCheckError (..)
  , ExpressionCheckContext
  , NestedRigidProvenance
  , prepareExpressionCheckContext
  , checkExpressionInContext
  , checkExpressionInContextWithNestedRigidProvenance
  , checkExpression
  , checkExpressionWithRigidInstantiation
  )
where

import Control.Monad (foldM, unless, when, zipWithM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT (..), gets, modify', runStateT)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Numeric.Natural (Natural)

import Language.Haskell.Exference.Core.Expression
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.Core.ConstraintSolver
import Language.Haskell.Exference.Core.Internal.FlexibleIds
import Language.Haskell.Exference.Core.Internal.RigidScope
  ( RigidScope
  , NestedRigidProvenance
  , escapingRigidConstraints
  , emptyRigidScope
  , provenanceRigidIdentifiers
  , registerRigidScope
  , validateRigidSubstitutions
  )
import Language.Haskell.Exference.Core.Internal.ScopedConstraint
  ( ScopedConstraint (..)
  , resolveScopedConstraints
  , scopedConstraintApplySubsts
  , scopedConstraintObligations
  , scopedConstraints
  )
import Language.Haskell.Exference.Core.Internal.VariableSupply
import Language.Haskell.Exference.Core.Internal.Polytype
import Language.Haskell.Exference.Core.RigidInstantiation
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.Unify (unifyShared)
import qualified Language.Haskell.Synthesis.Collection as SharedCollection
import qualified Language.Haskell.Synthesis.Generated as SharedGenerated
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
  -- | Legacy compatibility constructor. Rank-N types are accepted and the
  -- checker no longer produces this error.
  | UnsupportedNestedForall HsType
  | RefutableConstraints [HsConstraint]
  | ConstraintMismatch [HsConstraint] [HsConstraint]
  | RigidInstantiationFailure RigidInstantiationError
  | RigidInstantiationPlanMismatch [TVarId] [TVarId]
  | RigidInstantiationTargetCollision [TVarId]
  | RigidInstantiationPlanAlreadyAdvanced Natural
  | UnmatchedNestedRigidVariables [TVarId]
  | EscapingRigidConstraints [HsConstraint]
  | FlexibleIdentifierSupplyExhausted
  | InvalidCheckType HsType SynthesisTypeError
  | InvalidCheckConstraint HsConstraint SynthesisTypeError
  | InvalidCheckClassConstraint ClassEnvError
  | InvalidCheckEnvironmentBindings EnvironmentDuplicateError
  | InvalidCheckEnvironmentRatings EnvironmentRatingError
  | InvalidCheckEnvironmentSyntax EnvironmentSyntaxError
  | InvalidCheckExpressionScope (SharedGenerated.ScopeError TVarId)
  | InvalidCheckExpressionSyntax SharedGenerated.RenderError
  | InvalidCheckDeconstructor DeconstructorValidationError
  deriving (Eq, Show)

data CheckState = CheckState
  { checkFlexibleIds :: !FlexibleIdSupply
  , checkAliveFlexibleIds :: !IntSet.IntSet
  , checkSubstitutions :: !Substs
  , checkLocalGivens :: [HsConstraint]
  , checkConstraints :: [ScopedConstraint]
  , checkRigidPlan :: !RigidInstantiationPlan
  , checkRigidScope :: !RigidScope
  , checkCandidateRigidIds :: !IntSet.IntSet
  , checkIntroducedRigidIds :: !IntSet.IntSet
  , checkRigidAlpha :: !(IntMap.IntMap TVarId)
  , checkRigidAlphaInverse :: !(IntMap.IntMap TVarId)
  }

-- | Fixed, independently validated inputs for checking many candidates from
-- one query. The constructor is hidden so candidate checking can rely on the
-- cached goal instantiation and class assumptions without trusting a search
-- node or repeatedly scanning the complete environment.
data ExpressionCheckContext = ExpressionCheckContext
  HsType
  QueryClassEnv
  [FunctionBinding]
  [DeconstructorBinding]
  (Map.Map QualifiedName Int)
  RigidInstantiationPlan

type VariableEnvironment = IntMap.IntMap HsType
type Check a = StateT CheckState (Either ExpressionCheckError) a

-- | Independently reconstruct and check a generated expression. This checker
-- sees only the final expression, declared environment, and requested type; it
-- reuses no search node or transformation state.
checkExpression
  :: QueryClassEnv
  -> [FunctionBinding]
  -> [DeconstructorBinding]
  -> HsType
  -> [HsConstraint]
  -> Expression
  -> Either ExpressionCheckError ()
checkExpression classEnvironment functions deconstructors goal expected expression = do
  validateCheckInputs classEnvironment functions deconstructors goal expected
    expression
  plan <- either (Left . RigidInstantiationFailure) Right
    $ planRigidInstantiation
        (mkRigidInstantiationContext $ EnvDictionary
          functions deconstructors $ qClassEnv_env classEnvironment)
        (Set.toList $ qClassEnv_constraints classEnvironment)
        goal
  context <- prepareExpressionCheckContextUnchecked plan classEnvironment
    functions deconstructors goal
  checkValidatedExpression IntSet.empty context expected expression

-- | Check using a precomputed forall-opening plan.
--
-- The class environment must describe the original query assumptions, before
-- this plan's opened constraints are added. The checker rejects any target
-- which collides with that environment, those assumptions, or the goal even
-- when the plan's flexible binder IDs happen to match.
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
  context <- prepareValidatedExpressionCheckContext plan classEnvironment
    functions deconstructors goal
  checkValidatedExpression IntSet.empty context expected expression

-- | Validate the query-stable half of an independent expression check once.
--
-- Supply the original query assumptions, not a class environment already
-- augmented with the plan's opened constraints. The returned opaque context
-- may safely check every candidate produced for that exact environment and
-- goal: construction verifies the binder spine and proves every supplied rigid
-- target fresh for those inputs. A safe plan made against a conservative
-- environment superset remains valid.
prepareExpressionCheckContext
  :: RigidInstantiationPlan
  -> QueryClassEnv
  -> [FunctionBinding]
  -> [DeconstructorBinding]
  -> HsType
  -> Either ExpressionCheckError ExpressionCheckContext
prepareExpressionCheckContext plan classEnvironment functions deconstructors
    goal = do
  validateCheckContextInputs classEnvironment functions deconstructors goal
  prepareValidatedExpressionCheckContext plan classEnvironment functions
    deconstructors goal

-- Raw public entrances have already established the complete fixed-input
-- invariant before reaching this worker. Instantiate first so the historical
-- binder-spine mismatch keeps precedence, then prove that none of the opaque
-- plan's rigid targets collide with this environment or query. Requiring the
-- locally minimal plan would be too strict: live search plans against a sealed
-- environment before safely removing excluded capabilities.
prepareValidatedExpressionCheckContext
  :: RigidInstantiationPlan
  -> QueryClassEnv
  -> [FunctionBinding]
  -> [DeconstructorBinding]
  -> HsType
  -> Either ExpressionCheckError ExpressionCheckContext
prepareValidatedExpressionCheckContext plan classEnvironment functions
    deconstructors goal = do
  context <- prepareExpressionCheckContextUnchecked plan classEnvironment
    functions deconstructors goal
  let planningContext = mkRigidInstantiationContext $ EnvDictionary
        functions deconstructors $ qClassEnv_env classEnvironment
      collisions = rigidInstantiationTargetCollisions planningContext
        (Set.toList $ qClassEnv_constraints classEnvironment) goal plan
  unless (null collisions) $ Left
    $ RigidInstantiationTargetCollision collisions
  let advanced = nestedRigidInstantiationCount plan
  unless (advanced == 0) $ Left
    $ RigidInstantiationPlanAlreadyAdvanced advanced
  pure context

-- | Validate and check only the residual constraints and generated tree that
-- vary from candidate to candidate.
checkExpressionInContext
  :: ExpressionCheckContext
  -> [HsConstraint]
  -> Expression
  -> Either ExpressionCheckError ()
checkExpressionInContext context expected expression = do
  validateCheckCandidateInputs context expected expression
  checkValidatedExpression IntSet.empty context expected expression

-- | Check a live search candidate while treating only the rigid spellings
-- owned by that branch's nested scopes as alpha-renamable annotation names.
-- Type reconstruction, scope registration, substitutions, and residual
-- validation are still repeated independently; the search scope supplies
-- provenance, not typing evidence. Standalone callers use
-- 'checkExpressionInContext', where every annotation-only rigid stays nominal.
checkExpressionInContextWithNestedRigidProvenance
  :: ExpressionCheckContext
  -> NestedRigidProvenance
  -> [HsConstraint]
  -> Expression
  -> Either ExpressionCheckError ()
checkExpressionInContextWithNestedRigidProvenance context provenance expected
    expression = do
  validateCheckCandidateInputs context expected expression
  checkValidatedExpression
    (provenanceRigidIdentifiers provenance) context expected expression

-- A standalone entrance either computed its own plan from these inputs or
-- called 'prepareValidatedExpressionCheckContext'. Live search likewise keeps
-- this unchecked constructor private and reaches it only through the validated
-- reusable entrance.
prepareExpressionCheckContextUnchecked
  :: RigidInstantiationPlan
  -> QueryClassEnv
  -> [FunctionBinding]
  -> [DeconstructorBinding]
  -> HsType
  -> Either ExpressionCheckError ExpressionCheckContext
prepareExpressionCheckContextUnchecked plan classEnvironment functions
    deconstructors goal = do
  (checkedGoal, openedConstraints) <- instantiateGoal plan goal
  pure $ ExpressionCheckContext
    checkedGoal
    (addQueryClassEnv openedConstraints classEnvironment)
    functions
    deconstructors
    (constructorArityIndex deconstructors)
    plan

-- Both public entrances establish the complete raw-input invariant before
-- reaching this worker. Keeping planning outside it lets live search supply
-- its exact sealed plan without making the standalone entrance inspect
-- malformed raw values before their typed checker diagnostics are selected.
checkValidatedExpression
  :: IntSet.IntSet
  -> ExpressionCheckContext
  -> [HsConstraint]
  -> Expression
  -> Either ExpressionCheckError ()
checkValidatedExpression provenCandidateRigids
    (ExpressionCheckContext checkedGoal augmentedEnvironment
      functions deconstructors _ rigidPlan)
    expected expression = do
  let candidateRigids = IntSet.filter
        (not . (`rigidInstantiationIdentifierIsReserved` rigidPlan))
        $ IntSet.intersection provenCandidateRigids
        $ expressionRigidIdentifiers expression
      initialState = CheckState
        { checkFlexibleIds = supplyFromIdentifierSet
            $ IntSet.union
                (flexibleIdentifiers checkedGoal)
                (expressionFlexibleIdentifiers expression)
        , checkAliveFlexibleIds = flexibleFreeIdentifiers checkedGoal
        , checkSubstitutions = IntMap.empty
        , checkLocalGivens = []
        , checkConstraints = []
        , checkRigidPlan = rigidPlan
        , checkRigidScope = emptyRigidScope
        , checkCandidateRigidIds = candidateRigids
        , checkIntroducedRigidIds = IntSet.empty
        , checkRigidAlpha = IntMap.empty
        , checkRigidAlphaInverse = IntMap.empty
        }
  (_, finalState) <- runStateT
    (checkAgainst IntMap.empty expression checkedGoal)
    initialState
  let substitutions = checkSubstitutions finalState
      rigidAlpha = checkRigidAlpha finalState
      unmatchedRigids = IntSet.toAscList $ IntSet.difference
        (checkCandidateRigidIds finalState)
        (IntMap.keysSet rigidAlpha)
      normalizeConstraint = fmap
        ( SharedType.canonicalizeType
        . applyRigidAlpha rigidAlpha
        )
      inferredScopedConstraints = map
        ( normalizeScopedConstraint normalizeConstraint
        . snd
        . scopedConstraintApplySubsts substitutions
        )
        $ checkConstraints finalState
      inferredConstraints = scopedConstraintObligations
        inferredScopedConstraints
      -- The inferred side is canonicalized whenever the unifier bound a
      -- variable, so the caller-supplied side must be canonicalized too or a
      -- semantically equal application-form spelling would fail comparison.
      normalizedExpected = Set.toAscList $ Set.fromList
        $ map
            (fmap
              ( SharedType.canonicalizeType
              . applyRigidAlpha rigidAlpha
              ))
            expected
  unless (null unmatchedRigids) $ Left
    $ UnmatchedNestedRigidVariables unmatchedRigids
  unresolvedScoped <- maybe
    (Left $ RefutableConstraints inferredConstraints)
    Right
    (resolveScopedConstraints filterUnresolved augmentedEnvironment
      inferredScopedConstraints)
  let unresolved = Set.toAscList $ Set.fromList
        $ scopedConstraintObligations unresolvedScoped
  let escaping = escapingRigidConstraints
        (checkRigidScope finalState) unresolved
  unless (null escaping) $ Left $ EscapingRigidConstraints escaping
  unless (unresolved == normalizedExpected)
    $ Left (ConstraintMismatch normalizedExpected unresolved)
  where
    -- Checking is deliberately bidirectional only where the expected type
    -- carries information which synthesis cannot recover. In particular, an
    -- expected forall first gets one ordinary, transactional synthesis pass:
    -- exact opaque forwarding and checked shallow subsumption must retain
    -- priority over structural introduction, just as they do in search.
    checkAgainst
      :: VariableEnvironment
      -> Expression
      -> HsType
      -> Check ()
    checkAgainst variables checkedExpression rawExpected = do
      expectedType <- zonk $ SharedType.canonicalizeType rawExpected
      recordAliveType expectedType
      case (checkedExpression, expectedType) of
        (_, TypeForall _ _ _) ->
          orElseTransactionally
            (infer variables checkedExpression
              >>= (`unifyTypes` expectedType))
            (introduceExpectedForallChain
              variables checkedExpression expectedType)
        (ExpLambda variable annotation body, TypeArrow parameter result) -> do
          unifyTypes annotation parameter
          checkAgainst
            (IntMap.insert variable annotation variables)
            body
            result
        _ -> infer variables checkedExpression
          >>= (`unifyTypes` expectedType)

    -- Choosing introduction commits to the complete leading chain, matching
    -- search's continuation mode. Every layer's substituted contexts are
    -- lexical givens only while checking its body; generated obligations keep
    -- a snapshot of those givens so a sibling cannot consume them later.
    introduceExpectedForallChain variables checkedExpression source =
      case source of
        TypeForall binders constraints body -> do
          instantiations <- mapM allocateCanonicalNestedRigid binders
          alive <- gets checkAliveFlexibleIds
          rigidScope <- gets checkRigidScope
          let rigids = map snd instantiations
              substitutions = IntMap.fromList
                [ (binder, TypeConstant rigid)
                | (binder, rigid) <- instantiations
                ]
              instantiatedConstraints = map
                (snd . constraintApplySubsts substitutions) constraints
          modify' $ \current -> current
            { checkRigidScope = registerRigidScope alive rigids rigidScope
            , checkIntroducedRigidIds = IntSet.union
                (IntSet.fromList rigids)
                (checkIntroducedRigidIds current)
            }
          withLocalGivens instantiatedConstraints
            $ introduceExpectedForallChain variables checkedExpression
            $ snd $ applySubsts substitutions body
        body -> checkAgainst variables checkedExpression body

    infer :: VariableEnvironment -> Expression -> Check HsType
    infer variables (ExpVar variable annotation) = do
      declared <- maybe (throwCheck $ UnknownVariable variable) pure
        $ IntMap.lookup variable variables
      declared' <- zonk declared
      annotation' <- zonk annotation
      case classifyProviderUse declared' annotation' of
        OpaqueProviderForwarding -> do
          -- Exact opaque forwarding has priority over elimination, matching
          -- search and preserving explicitly polymorphic occurrences. Merely
          -- being unifiable is not enough: a fresh monotype annotation can
          -- bind to the whole opaque atom, but denotes an instantiated use.
          unifyTypes declared' annotation'
          zonk declared'
        -- Search records the requested context-free scheme on a
        -- shallow-subsummed occurrence. Classification independently rechecks
        -- that the local provider can instantiate to it without solving free
        -- flexible variables; no temporary matcher substitution is part of
        -- the generated expression.
        SubsumedProviderForwarding -> zonk annotation'
        InstantiateProviderUse -> do
          instantiated <- instantiateScopedProvider declared'
          unifyTypes instantiated annotation'
          zonk annotation'
        OrdinaryProviderUse ->
          unifyTypes declared' annotation' >> zonk declared'
    infer _ (ExpName name) = instantiateBinding name
    infer variables (ExpLambda variable annotation body) = do
      recordAliveType annotation
      TypeArrow annotation
        <$> infer (IntMap.insert variable annotation variables) body
    infer variables (ExpApply function argument) = do
      functionType <- infer variables function
      functionType' <- zonk functionType
      case functionType' of
        -- A known arrow is a checking boundary for its argument. This admits
        -- a structurally introduced polymorphic argument without guessing a
        -- polytype during ordinary synthesis.
        TypeArrow parameter result ->
          checkAgainst variables argument parameter >> zonk result
        _ -> do
          argumentType <- infer variables argument
          resultType <- freshTypeVariable
          unifyTypes functionType' (TypeArrow argumentType resultType)
          zonk resultType
    infer _ (ExpHole variable) = throwCheck $ ExpressionHole variable
    infer variables (ExpLetMatch constructor patternVariables binding body) = do
      bindingType <- infer variables binding
      fieldTypes <- instantiateConstructor constructor bindingType
      let expectedArity = length fieldTypes
          actualArity = SharedCollection.observedListLength
            expectedArity patternVariables
      when (actualArity /= expectedArity)
        $ throwCheck $ PatternArity constructor
            expectedArity actualArity
      checkedVariables <- foldM addPatternVariable variables
        $ zip patternVariables fieldTypes
      infer checkedVariables body
    infer variables (ExpLet variable annotation binding body) = do
      checkAgainst variables binding annotation
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
      let expectedArity = length fieldTypes
          actualArity = SharedCollection.observedListLength
            expectedArity patternVariables
      when (actualArity /= expectedArity)
        $ throwCheck $ PatternArity constructor
            expectedArity actualArity
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
        localGivens <- gets checkLocalGivens
        modify' $ \current -> current
          { checkConstraints =
              scopedConstraints localGivens freshConstraints
                ++ checkConstraints current
          }
        pure freshType

    -- Local polymorphic values are instantiated independently at every use.
    -- Their direct forall contexts become ordinary checker obligations. The
    -- generated occurrence annotation contains search's instantiated
    -- monotype, so checking does not need to reproduce search's fresh IDs.
    instantiateScopedProvider declared = do
      supply <- gets checkFlexibleIds
      case instantiateLeadingForallsWith allocateNamespace supply declared of
        Nothing -> throwCheck FlexibleIdentifierSupplyExhausted
        Just (instantiated, constraints, nextSupply) -> do
          localGivens <- gets checkLocalGivens
          modify' $ \current -> current
            { checkFlexibleIds = nextSupply
            , checkAliveFlexibleIds = IntSet.unions
                $ checkAliveFlexibleIds current
                : flexibleFreeIdentifiers instantiated
                : map (foldMap flexibleFreeIdentifiers . constraint_params)
                    constraints
            , checkConstraints =
                scopedConstraints localGivens constraints
                  ++ checkConstraints current
            }
          pure instantiated

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
  validateCheckEnvironmentIdentity rawEnvironment
  validateCheckEnvironmentRating rawEnvironment
  validateCheckEnvironmentSyntax rawEnvironment
  validateCheckType classEnvironment QueryConstraint goal
  validateCheckClassConstraints classEnvironment QueryConstraint
    $ typeConstraints goal
  mapM_ (validateCheckConstraint classEnvironment QueryConstraint) expected
  mapM_ (validateCheckConstraint classEnvironment QueryConstraint)
    $ Set.toAscList
    $ qClassEnv_constraints classEnvironment
  mapM_ (validateCheckFunction classEnvironment) functions
  mapM_ (validateCheckDeconstructorTypes classEnvironment) deconstructors
  validateExpressionPatternArities
    (constructorArityIndex deconstructors) expression
  mapM_ (validateCheckType classEnvironment QueryConstraint . snd)
    $ expressionTypedLocals expression
  mapM_ validateCheckDeconstructor deconstructors
  validateGeneratedExpression expression
 where
  rawEnvironment = EnvDictionary
    functions deconstructors $ qClassEnv_env classEnvironment

-- Fixed validation used by the reusable context. Unlike the compatibility
-- entrances, candidate constraints and annotations are deliberately absent.
validateCheckContextInputs
  :: QueryClassEnv
  -> [FunctionBinding]
  -> [DeconstructorBinding]
  -> HsType
  -> Either ExpressionCheckError ()
validateCheckContextInputs classEnvironment functions deconstructors goal = do
  validateCheckEnvironmentIdentity rawEnvironment
  validateCheckEnvironmentRating rawEnvironment
  validateCheckEnvironmentSyntax rawEnvironment
  validateCheckType classEnvironment QueryConstraint goal
  validateCheckClassConstraints classEnvironment QueryConstraint
    $ typeConstraints goal
  mapM_ (validateCheckConstraint classEnvironment QueryConstraint)
    $ Set.toAscList $ qClassEnv_constraints classEnvironment
  mapM_ (validateCheckFunction classEnvironment) functions
  mapM_ (validateCheckDeconstructorTypes classEnvironment) deconstructors
  mapM_ validateCheckDeconstructor deconstructors
 where
  rawEnvironment = EnvDictionary
    functions deconstructors $ qClassEnv_env classEnvironment

validateCheckCandidateInputs
  :: ExpressionCheckContext
  -> [HsConstraint]
  -> Expression
  -> Either ExpressionCheckError ()
validateCheckCandidateInputs
    (ExpressionCheckContext _ classEnvironment _ _ constructorArities _)
    expected expression = do
  mapM_ (validateCheckConstraint classEnvironment QueryConstraint) expected
  validateExpressionPatternArities constructorArities expression
  mapM_ (validateCheckType classEnvironment QueryConstraint . snd)
    $ expressionTypedLocals expression
  validateGeneratedExpression expression

validateCheckEnvironmentIdentity
  :: EnvDictionary
  -> Either ExpressionCheckError ()
validateCheckEnvironmentIdentity environment = case
    validateEnvironmentBindingIdentities environment of
  Left failure -> Left $ InvalidCheckEnvironmentBindings failure
  Right () -> Right ()

validateCheckEnvironmentRating
  :: EnvDictionary
  -> Either ExpressionCheckError ()
validateCheckEnvironmentRating environment = case
    validateEnvironmentBindingRatings environment of
  Left failure -> Left $ InvalidCheckEnvironmentRatings failure
  Right () -> Right ()

validateCheckEnvironmentSyntax
  :: EnvDictionary
  -> Either ExpressionCheckError ()
validateCheckEnvironmentSyntax environment = case
    validateEnvironmentBindingSyntax environment of
  Left failure -> Left $ InvalidCheckEnvironmentSyntax failure
  Right () -> Right ()

validateCheckType
  :: QueryClassEnv
  -> ConstraintSite
  -> HsType
  -> Either ExpressionCheckError ()
validateCheckType classEnvironment site typeExpression = do
  SharedType.validateTypeWidthsWith
    (InvalidCheckType typeExpression . InvalidSynthesisType)
    (validateCheckKnownArity classEnvironment site)
    typeExpression
  case toSynthesisType typeExpression of
    Left failure -> Left $ InvalidCheckType typeExpression failure
    Right _ -> Right ()

validateCheckConstraint
  :: QueryClassEnv
  -> ConstraintSite
  -> HsConstraint
  -> Either ExpressionCheckError ()
validateCheckConstraint classEnvironment site constraint = do
  validateCheckKnownArity classEnvironment site constraint
  case toSynthesisConstraint constraint of
    Left failure -> Left $ InvalidCheckConstraint constraint failure
    Right _ -> Right ()
  validateCheckClassConstraint classEnvironment site constraint

validateCheckKnownArity
  :: QueryClassEnv
  -> ConstraintSite
  -> HsConstraint
  -> Either ExpressionCheckError ()
validateCheckKnownArity classEnvironment site constraint = either
  (Left . InvalidCheckClassConstraint)
  Right
  $ validateKnownConstraintArityInEnv
      (qClassEnv_env classEnvironment) site constraint

validateCheckClassConstraint
  :: QueryClassEnv
  -> ConstraintSite
  -> HsConstraint
  -> Either ExpressionCheckError ()
validateCheckClassConstraint classEnvironment site constraint = case
    validateKnownConstraintInEnv
      (qClassEnv_env classEnvironment) site constraint of
  Left failure -> Left $ InvalidCheckClassConstraint failure
  Right () -> Right ()

validateCheckClassConstraints
  :: QueryClassEnv
  -> ConstraintSite
  -> [HsConstraint]
  -> Either ExpressionCheckError ()
validateCheckClassConstraints classEnvironment site =
  mapM_ $ validateCheckClassConstraint classEnvironment site

validateCheckFunction
  :: QueryClassEnv
  -> FunctionBinding
  -> Either ExpressionCheckError ()
validateCheckFunction classEnvironment binding = do
  validateCheckType classEnvironment site $ functionResult binding
  mapM_ (validateCheckType classEnvironment site)
    $ functionParameters binding
  mapM_ (validateCheckConstraint classEnvironment site)
    $ functionConstraints binding
  validateCheckClassConstraints classEnvironment site
    $ typeConstraints $ functionBindingType binding
 where
  site = BindingConstraint $ functionName binding

validateCheckDeconstructorTypes
  :: QueryClassEnv
  -> DeconstructorBinding
  -> Either ExpressionCheckError ()
validateCheckDeconstructorTypes classEnvironment deconstructor = do
  validateType inputSite $ deconstructorInput deconstructor
  mapM_ validateConstructor $ deconstructorConstructors deconstructor
 where
  inputSite = maybe QueryConstraint BindingConstraint
    $ typeConstructorHead $ deconstructorInput deconstructor
  validateConstructor constructor = mapM_
    (validateType $ BindingConstraint $ constructorName constructor)
    $ constructorFields constructor
  validateType site typeExpression = do
    validateCheckType classEnvironment site typeExpression
    validateCheckClassConstraints classEnvironment site
      $ typeConstraints typeExpression

validateCheckDeconstructor
  :: DeconstructorBinding
  -> Either ExpressionCheckError ()
validateCheckDeconstructor deconstructor = case
    validateDeconstructorBinding deconstructor of
  Left failure -> Left $ InvalidCheckDeconstructor failure
  Right () -> Right ()

validateGeneratedExpression
  :: Expression
  -> Either ExpressionCheckError ()
validateGeneratedExpression expression = do
  let generated = toGeneratedExpression expression
  case SharedGenerated.validateExpressionScope generated of
    Left failure -> Left $ InvalidCheckExpressionScope failure
    Right () -> Right ()
  case SharedGenerated.validateExpressionSyntax generated of
    Left failure -> Left $ InvalidCheckExpressionSyntax failure
    Right () -> Right ()

constructorArityIndex
  :: [DeconstructorBinding]
  -> Map.Map QualifiedName Int
constructorArityIndex deconstructors = Map.fromList
  [ (constructorName constructor, length $ constructorFields constructor)
  | deconstructor <- deconstructors
  , constructor <- deconstructorConstructors deconstructor
  ]

-- Inspect the erased shared tree directly. The historical ExpLetMatch and
-- ExpCaseMatch pattern views must traverse every binder before matching and
-- therefore cannot safely diagnose a cyclic malformed binder list.
validateExpressionPatternArities
  :: Map.Map QualifiedName Int
  -> Expression
  -> Either ExpressionCheckError ()
validateExpressionPatternArities constructorArities = inspectExpression
  . toGeneratedExpression
 where
  inspectExpression generated = case generated of
    SharedGenerated.Local{} -> Right ()
    SharedGenerated.Global{} -> Right ()
    SharedGenerated.Lambda patterns body ->
      mapM_ inspectPattern patterns >> inspectExpression body
    SharedGenerated.Apply function argument ->
      inspectExpression function >> inspectExpression argument
    SharedGenerated.Tuple elements -> mapM_ inspectExpression elements
    SharedGenerated.Hole{} -> Right ()
    SharedGenerated.Let pattern binding body ->
      inspectPattern pattern >> inspectExpression binding
        >> inspectExpression body
    SharedGenerated.Case scrutinee alternatives -> do
      inspectExpression scrutinee
      mapM_ inspectAlternative alternatives

  inspectAlternative (pattern, body) =
    inspectPattern pattern >> inspectExpression body

  inspectPattern pattern = case pattern of
    SharedGenerated.Bind{} -> Right ()
    SharedGenerated.Wildcard -> Right ()
    SharedGenerated.Constructor name arguments -> do
      expected <- maybe (Left $ UnknownConstructor name) Right
        $ Map.lookup name constructorArities
      let actual = SharedCollection.observedListLength expected arguments
      unless (actual == expected) $ Left $ PatternArity name expected actual
      mapM_ inspectPattern arguments
    SharedGenerated.TuplePattern elements -> mapM_ inspectPattern elements
    SharedGenerated.As _ nested -> inspectPattern nested

throwCheck :: ExpressionCheckError -> Check a
throwCheck = lift . Left

-- Run a preferred typing rule without publishing any of its allocations,
-- constraints, or substitutions when it fails. This is what lets opaque
-- forwarding keep precedence over forall introduction without contaminating
-- the introduction branch with a half-finished inference attempt.
orElseTransactionally :: Check a -> Check a -> Check a
orElseTransactionally preferred fallback = StateT $ \initialState ->
  case runStateT preferred initialState of
    Right result -> Right result
    Left _ -> runStateT fallback initialState

-- Nested forall contexts are evidence assumptions for exactly their body.
-- Restore only the lexical-given component after checking; every substitution,
-- rigid allocation, and scoped obligation produced by the body remains part
-- of the successful checker state.
withLocalGivens :: [HsConstraint] -> Check a -> Check a
withLocalGivens givens action = do
  outerGivens <- gets checkLocalGivens
  modify' $ \current -> current
    {checkLocalGivens = outerGivens ++ givens}
  result <- action
  modify' $ \current -> current {checkLocalGivens = outerGivens}
  pure result

normalizeScopedConstraint
  :: (HsConstraint -> HsConstraint)
  -> ScopedConstraint
  -> ScopedConstraint
normalizeScopedConstraint normalize
    (ScopedConstraint givens obligation) = ScopedConstraint
  (map normalize givens)
  (normalize obligation)

recordAliveType :: HsType -> Check ()
recordAliveType ty = modify' $ \current -> current
  { checkAliveFlexibleIds = IntSet.union
      (flexibleFreeIdentifiers ty)
      (checkAliveFlexibleIds current)
  }

flexibleFreeIdentifiers :: HsType -> IntSet.IntSet
flexibleFreeIdentifiers = IntSet.fromList . Set.toAscList . freeVars

expressionRigidIdentifiers :: Expression -> IntSet.IntSet
expressionRigidIdentifiers =
  foldMap (rigidIdentifiers . snd) . expressionTypedLocals

rigidIdentifiers :: HsType -> IntSet.IntSet
rigidIdentifiers = foldMap
  $ SharedType.foldRigidVariable IntSet.singleton

-- Candidate annotations retain search's fresh skolem spellings. Reserve those
-- spellings as a disjoint foreign namespace and allocate a checker-local
-- canonical skolem instead; later type comparisons establish an injective
-- alpha-renaming between the two. Skipping is finite because an expression
-- contains only finitely many annotations.
allocateCanonicalNestedRigid :: TVarId -> Check (TVarId, TVarId)
allocateCanonicalNestedRigid binder = do
  plan <- gets checkRigidPlan
  case allocateNestedRigidInstantiations [binder] plan of
    Left failure -> throwCheck $ RigidInstantiationFailure failure
    Right ([(pairedBinder, rigid)], nextPlan)
      | pairedBinder == binder -> do
          modify' $ \current -> current {checkRigidPlan = nextPlan}
          candidates <- gets checkCandidateRigidIds
          if IntSet.member rigid candidates
            then allocateCanonicalNestedRigid binder
            else pure (binder, rigid)
    Right (instantiations, _) -> throwCheck $ RigidInstantiationPlanMismatch
      [binder] $ map fst instantiations

applyRigidAlpha :: IntMap.IntMap TVarId -> HsType -> HsType
applyRigidAlpha renaming = fmap rename
 where
  rename variable = case variable of
    SharedType.FlexibleVariable{} -> variable
    SharedType.RigidVariable identifier -> SharedType.RigidVariable
      $ IntMap.findWithDefault identifier identifier renaming

freshTypeVariable :: Check HsType
freshTypeVariable = do
  supply <- gets checkFlexibleIds
  case allocateFreshIdentifier supply of
    Nothing -> throwCheck FlexibleIdentifierSupplyExhausted
    Just (variable, nextSupply) -> do
      modify' $ \current -> current
        { checkFlexibleIds = nextSupply
        , checkAliveFlexibleIds = IntSet.insert variable
            $ checkAliveFlexibleIds current
        }
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

-- Search and the checker may encounter independent nested goals in different
-- orders. Their dynamically fresh rigid spellings are therefore compared up
-- to one injective alpha-renaming, while every rigid reserved by the sealed
-- root plan remains nominal. Structural alignment only discovers mappings;
-- the ordinary unifier still owns all type compatibility decisions.
alignRigidAlpha :: HsType -> HsType -> Check ()
alignRigidAlpha originalLeft originalRight = go originalLeft originalRight
 where
  go left right = case (left, right) of
    (TypeConstant candidateRigid, TypeConstant canonical) ->
      alignPair candidateRigid canonical
    (TypeArrow leftParameter leftResult,
        TypeArrow rightParameter rightResult) ->
      go leftParameter rightParameter >> go leftResult rightResult
    (TypeApp leftFunction leftArgument,
        TypeApp rightFunction rightArgument) ->
      go leftFunction rightFunction >> go leftArgument rightArgument
    (TypeTuple leftBoxity leftElements,
        TypeTuple rightBoxity rightElements)
      | leftBoxity == rightBoxity
      , length leftElements == length rightElements ->
          zipWithM_ go leftElements rightElements
    (TypeForallNative _ leftConstraints leftBody,
        TypeForallNative _ rightConstraints rightBody)
      | length leftConstraints == length rightConstraints -> do
          zipWithM_ alignConstraint leftConstraints rightConstraints
          go leftBody rightBody
    _ -> pure ()

  alignConstraint left right
    | length leftArguments == length rightArguments =
        zipWithM_ go leftArguments rightArguments
    | otherwise = pure ()
   where
    leftArguments = constraint_params left
    rightArguments = constraint_params right

  alignPair leftIdentifier rightIdentifier = do
    candidates <- gets checkCandidateRigidIds
    introduced <- gets checkIntroducedRigidIds
    case
        ( IntSet.member leftIdentifier candidates
        , IntSet.member rightIdentifier candidates
        , IntSet.member leftIdentifier introduced
        , IntSet.member rightIdentifier introduced
        ) of
      (True, _, _, True) ->
        bindRigidAlpha leftIdentifier rightIdentifier
      (_, True, True, _) ->
        bindRigidAlpha rightIdentifier leftIdentifier
      _ -> pure ()

  bindRigidAlpha candidateRigid canonical = do
    forward <- gets checkRigidAlpha
    backward <- gets checkRigidAlphaInverse
    case
        ( IntMap.lookup candidateRigid forward
        , IntMap.lookup canonical backward
        ) of
      (Just existing, _) | existing /= canonical -> mismatch
      (_, Just existing) | existing /= candidateRigid -> mismatch
      (Just _, Just _) -> pure ()
      _ -> do
        let forward' = IntMap.insert candidateRigid canonical forward
            backward' = IntMap.insert canonical candidateRigid backward
        substitutions <- gets checkSubstitutions
        rigidScope <- gets checkRigidScope
        let substitutions' = IntMap.map
              (applyRigidAlpha forward') substitutions
        case validateRigidSubstitutions rigidScope substitutions' of
          Left _ -> mismatch
          Right nextRigidScope -> modify' $ \current -> current
            { checkRigidAlpha = forward'
            , checkRigidAlphaInverse = backward'
            , checkSubstitutions = substitutions'
            , checkRigidScope = nextRigidScope
            }

  mismatch = throwCheck $ TypeMismatch originalLeft originalRight

unifyTypes :: HsType -> HsType -> Check ()
unifyTypes left right = do
  left' <- zonk $ SharedType.canonicalizeType left
  right' <- zonk $ SharedType.canonicalizeType right
  recordAliveType left'
  recordAliveType right'
  alignRigidAlpha left' right'
  rigidAlpha <- gets checkRigidAlpha
  let left'' = applyRigidAlpha rigidAlpha left'
      right'' = applyRigidAlpha rigidAlpha right'
  case unifyShared left'' right'' of
    Nothing -> throwCheck $ TypeMismatch left'' right''
    Just substitutions -> do
      rigidScope <- gets checkRigidScope
      case validateRigidSubstitutions rigidScope substitutions of
        Left _ -> throwCheck $ TypeMismatch left'' right''
        Right nextRigidScope -> do
          modify' $ \current -> current
            {checkRigidScope = nextRigidScope}
          mapM_ (uncurry bindVariable) $ IntMap.toAscList substitutions

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
  rigidAlpha <- gets checkRigidAlpha
  let (_, applied) = applySubsts substitutions ty
      canonical = SharedType.canonicalizeType
        $ applyRigidAlpha rigidAlpha applied
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
