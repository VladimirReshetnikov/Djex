{-# LANGUAGE DeriveGeneric #-}

-- | Independent validation of Exference's typed generated expressions.
--
-- This module reconstructs types without trusting the search tree. It shares
-- the pure unification kernel so opaque-polytype semantics cannot drift, while
-- raw environments still cross binding-identity and deconstructor checks
-- independently before any name lookup.
module Language.Haskell.Exference.Core.Internal.ExpressionCheck
  ( ExpressionCheckError (..)
  , ExpressionCheckContext
  , NestedRigidProvenance
  , CheckedExpressionEvidence
  , checkedExpressionVisibleInstantiationRepair
  , CheckedTypeApplicationOrigin
  , CheckedTypeApplicationOriginStep
  , checkedExpressionTypeApplicationOrigins
  , checkedExpressionTypeApplicationOriginReferences
  , checkedTypeApplicationOriginId
  , checkedTypeApplicationOriginOwner
  , checkedTypeApplicationOriginSource
  , checkedTypeApplicationOriginSteps
  , checkedTypeApplicationOriginStepSlot
  , checkedTypeApplicationOriginStepSource
  , checkedTypeApplicationOriginStepSelected
  , checkedTypeApplicationOriginStepResult
  , checkedTypeApplicationOriginStepObligations
  , ExferenceTermGraphAbsence (..)
  , ExferenceTermGraphCertificateAssociationFailure (..)
  , ExferenceTermGraphConstructionLimit (..)
  , ExferenceTermGraphAvailability (..)
  , checkedExpressionTermGraph
  , prepareExpressionCheckContext
  , prepareExpressionCheckContextWithSchemes
  , checkExpressionInContext
  , checkExpressionInContextWithNestedRigidProvenance
  , checkExpressionInContextWithNestedRigidProvenanceEvidence
  , checkExpression
  , checkExpressionWithRigidInstantiation
  )
where

import Control.DeepSeq (NFData (rnf))
import Control.Monad (foldM, forM, replicateM, unless, when, zipWithM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
  ( StateT (..), gets, modify', runStateT )
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import GHC.Generics (Generic)

import Language.Haskell.Exference.Core.Expression
import Language.Haskell.Exference.Core.Declaration
  ( validatePreparedFunctionSchemes )
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
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Type as SharedType
import qualified Language.Haskell.Synthesis.TypeAtom as SharedTypeAtom
import qualified Language.Haskell.Synthesis.Internal.TypedGenerated.Certificate
  as SharedCertificate
import qualified Language.Haskell.Synthesis.Internal.TypedGenerated.Certificate.Association
  as SharedAssociation
import qualified Language.Haskell.Synthesis.TypedGenerated as SharedTyped

-- | Why the independent checker rejected an expression, its expected type or
-- constraints, or the environment it was checked against.  Checking stops at
-- the first failure, so a result carries exactly one reason.
data ExpressionCheckError
  = UnknownVariable TVarId
  | VisibleTypeApplicationOccurrenceTraceMismatch
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
  | VisibleTypeApplicationToMonotype HsType
  | VisibleTypeApplicationRigidBinder SynthesisVariable
  | InvalidCheckType HsType SynthesisTypeError
  | InvalidCheckConstraint HsConstraint SynthesisTypeError
  | InvalidCheckClassConstraint ClassEnvError
  | InvalidCheckEnvironmentBindings EnvironmentDuplicateError
  | InvalidCheckEnvironmentRatings EnvironmentRatingError
  | InvalidCheckEnvironmentSyntax EnvironmentSyntaxError
  | InvalidCheckFunctionScheme QualifiedName
  | InvalidCheckExpressionScope (SharedGenerated.ScopeError TVarId)
  | InvalidCheckExpressionSyntax SharedGenerated.RenderError
  | InvalidCheckDeconstructor DeconstructorValidationError
  deriving (Eq, Show)

-- | A successful independent check can still decline to claim a typed graph.
-- Each constructor names the exact typed-graph or certificate-association
-- boundary which declined retention, rather than silently fabricating evidence
-- or dropping a checked candidate.
data ExferenceTermGraphAbsence
  = ImplicitLocalSpecialization TVarId HsType HsType
  | SubsumedLocalSpecialization TVarId HsType HsType
  | NestedForallIntroduction HsType
  | NominalConstructorPattern QualifiedName
  | UnsupportedStructuralConstructorPattern QualifiedName
  | UnsupportedContextualVisibleApplication HsType HsType HsType
  | UnsupportedContextEvidence HsType
  | UnsupportedContextualCertificateGraph
  | TermGraphEvidenceMismatch
  | TermGraphConstructionLimit ExferenceTermGraphConstructionLimit
  | TermGraphSealingFailure (SharedTyped.TermGraphError HsType TVarId)
  | TermGraphCertificateAssociationFailure
      ExferenceTermGraphCertificateAssociationFailure
  | TermGraphProjectionMismatch
  deriving (Eq, Show, Generic)

instance NFData ExferenceTermGraphAbsence

-- | Sanitized reason an origin-bearing graph failed the private atomic
-- association gate.  Raw plan and association errors retain checker types,
-- owner names, and graph coordinates; none of those payloads are a stable
-- public diagnostic or authority channel.
data ExferenceTermGraphCertificateAssociationFailure
  = TermGraphCertificatePlanLimitFailure
  | TermGraphCertificatePlanValidationFailure
  | TermGraphCertificateOccurrenceAssociationFailure
  deriving (Eq, Ord, Show, Generic)

instance NFData ExferenceTermGraphCertificateAssociationFailure

-- | Productive bounds applied while lowering a checker draft, before the raw
-- graph exists for 'SharedTyped.sealTermGraph' to inspect.
data ExferenceTermGraphConstructionLimit
  = TermGraphConstructionNodeLimitExceeded Natural Natural
  | TermGraphConstructionEdgeLimitExceeded Natural Natural
  | TermGraphConstructionPatternLimitExceeded Natural Natural
  | TermGraphConstructionCollectionLimitExceeded
      SharedTyped.GraphCollectionSite Int Int
  | TermGraphConstructionOccurrenceLimitExceeded Natural Natural
  deriving (Eq, Show, Generic)

instance NFData ExferenceTermGraphConstructionLimit

-- | Lazy engine payload for Exference's typed-candidate graph boundary.
-- Stable candidate facades do not expose it, and compatibility projections
-- intentionally ignore the private candidate field which retains this value.
data ExferenceTermGraphAvailability
  = ExferenceTermGraphAvailable (SharedTyped.TermGraph HsType TVarId)
  | ExferenceTermGraphAssociated
      (SharedAssociation.CheckedTypeApplicationCertificateGraph
        SynthesisVariable TVarId)
  | ExferenceTermGraphUnavailable ExferenceTermGraphAbsence

instance Eq ExferenceTermGraphAvailability where
  left == right = availabilityProjection left == availabilityProjection right

instance Show ExferenceTermGraphAvailability where
  showsPrec precedence availability = case availability of
    ExferenceTermGraphAvailable graph ->
      showUnary "ExferenceTermGraphAvailable" graph
    ExferenceTermGraphAssociated checked ->
      showUnary "ExferenceTermGraphAvailable"
        $ SharedAssociation.checkedTypeApplicationCertificateGraph checked
    ExferenceTermGraphUnavailable failure ->
      showUnary "ExferenceTermGraphUnavailable" failure
   where
    showUnary constructor value = showParen (precedence > 10) $
      showString constructor . showChar ' ' . showsPrec 11 value

instance NFData ExferenceTermGraphAvailability where
  rnf availability = case availability of
    ExferenceTermGraphAvailable graph -> rnf graph
    ExferenceTermGraphAssociated checked -> rnf checked
    ExferenceTermGraphUnavailable failure -> rnf failure

availabilityProjection
  :: ExferenceTermGraphAvailability
  -> Either ExferenceTermGraphAbsence
      (SharedTyped.TermGraph HsType TVarId)
availabilityProjection availability = case availability of
  ExferenceTermGraphAvailable graph -> Right graph
  ExferenceTermGraphAssociated checked -> Right $
    SharedAssociation.checkedTypeApplicationCertificateGraph checked
  ExferenceTermGraphUnavailable failure -> Left failure

-- | Checker-owned proof draft. Its constructor stays hidden so only a complete
-- validation run can request graph sealing.
data CheckedExpressionEvidence = CheckedExpressionEvidence
  (SharedGenerated.Expression TVarId)
  CheckedTermResult
  [CheckedTypeApplicationOrigin]
  (Maybe Expression)

-- | A proposed emission repair derived from completed type reconstruction.
-- This is not checked evidence for the modified expression: callers must run
-- the complete checker again before publishing that tree.
checkedExpressionVisibleInstantiationRepair
  :: CheckedExpressionEvidence -> Maybe Expression
checkedExpressionVisibleInstantiationRepair
    (CheckedExpressionEvidence _ _ _ repair) = repair

-- Failed transactional inference branches discard this source-order trace
-- with the other checker state. Keep it independent of optional term graphs
-- so patterns and quantified introductions preserve delayed choices too.
type CheckedProviderOccurrence =
  (Either TVarId QualifiedName, HsType, HsType)

repairInferredProviderApplications
  :: Substs -> [CheckedProviderOccurrence] -> Expression -> Maybe Expression
repairInferredProviderApplications substitutions occurrences expression = do
  (repaired, remaining) <- runStateT (go expression) occurrences
  if null remaining then Just repaired else Nothing
 where
  go :: Expression -> StateT [CheckedProviderOccurrence] Maybe Expression
  go original = case original of
    ExpVar variable _ -> occurrence (Left variable) original
    ExpName name -> occurrence (Right name) original
    ExpLambda variable annotation body -> ExpLambda variable annotation <$> go body
    ExpApply function argument -> ExpApply <$> go function <*> go argument
    -- Direct global visible spines have a separate checker entrance and do
    -- not generate ordinary occurrence records. A local visible base is
    -- inferred normally and therefore consumes its (exact-source) record.
    ExpTypeApply function argument
      | directGlobal original -> pure original
      | otherwise -> (`ExpTypeApply` argument) <$> go function
    ExpTuple elements -> ExpTuple <$> mapM go elements
    ExpHole{} -> pure original
    ExpLetMatch constructor variables binding body ->
      ExpLetMatch constructor variables <$> go binding <*> go body
    ExpLet variable annotation binding body ->
      ExpLet variable annotation <$> go binding <*> go body
    ExpCaseMatch scrutinee alternatives -> ExpCaseMatch <$> go scrutinee <*>
      mapM (\(constructor, variables, body) -> do
        repaired <- go body
        pure (constructor, variables, repaired)) alternatives

  directGlobal (ExpTypeApply function _) = directGlobal function
  directGlobal ExpName{} = True
  directGlobal _ = False

  occurrence identity original = StateT $ \remaining -> case remaining of
    (recorded, source, selected) : rest | identity == recorded ->
      let source' = snd $ applySubsts substitutions source
          selected' = snd $ applySubsts substitutions selected
          arguments = inferredProviderVisibleArguments source' selected'
          base = case (arguments, original) of
            (_ : _, ExpVar variable _) -> ExpVar variable source'
            _ -> original
      in Just (foldl ExpTypeApply base arguments, rest)
    _ -> Nothing

-- | Checker-owned identity for one exact global specialization.  The
-- constructor remains private: these records are observations retained after
-- a complete independent check, not caller-constructible certificates.  They
-- carry no prepared-declaration provenance, kind proof, instance or discharge
-- identity, graph occurrence, or fingerprint authority.  A later behavioral
-- consumer must independently match the owner and exact scheme against its
-- own prepared inventory before interpreting this observation.
data CheckedTypeApplicationOrigin = CheckedTypeApplicationOrigin
  !Natural
  QualifiedName
  HsType
  [CheckedTypeApplicationOriginStep]

-- | One source-telescope selection in structural source order.  Obligations
-- are exactly the source contexts which became unconditional at this step;
-- successful checking proves only that they participated in the ordinary
-- resolver and residual gates, not which instance discharged them.
data CheckedTypeApplicationOriginStep = CheckedTypeApplicationOriginStep
  !Natural
  HsType
  HsType
  HsType
  [HsConstraint]

-- | Every exact global specialization the checker recorded for a checked
-- expression, in allocation order (ascending 'checkedTypeApplicationOriginId').
checkedExpressionTypeApplicationOrigins
  :: CheckedExpressionEvidence
  -> [CheckedTypeApplicationOrigin]
checkedExpressionTypeApplicationOrigins
    (CheckedExpressionEvidence _ _ origins _) = origins

-- | Origin coordinates attached to checked visible applications, in source
-- order.  Coordinates are lookup identities only; this projection grants no
-- occurrence, graph, or fingerprint authority.  A result is emitted only
-- when the retained global base carries the same origin and the annotated
-- applications form its contiguous zero-based prefix.
checkedExpressionTypeApplicationOriginReferences
  :: CheckedExpressionEvidence
  -> [(Natural, Natural)]
checkedExpressionTypeApplicationOriginReferences
    (CheckedExpressionEvidence _ checkedResult origins _) =
  case checkedResult of
    CheckedTermResult _ (Right term) -> concatMap referencesForOrigin origins
     where
      referencesForOrigin origin = case matchingSpines origin term of
        [slots]
          | not $ null slots
          , slots == map checkedTypeApplicationOriginStepSlot
              (checkedTypeApplicationOriginSteps origin) ->
              map (\slot -> (checkedTypeApplicationOriginId origin, slot))
                slots
        _ -> []
    _ -> []
 where
  matchingSpines origin = collect
   where
    identifier = checkedTypeApplicationOriginId origin
    owner = checkedTypeApplicationOriginOwner origin

    collect current@(CheckedTerm _ form) = case originSpine [] current of
      Just (baseOwner, baseOrigin, slots)
        | baseOwner == owner
        , baseOrigin == Just identifier -> [slots]
      _ -> collectChildren form

    collectChildren form = case form of
      CheckedLocal{} -> []
      CheckedGlobal{} -> []
      CheckedLambda _ _ body -> collect body
      CheckedApply function argument -> collect function ++ collect argument
      CheckedVisibleTypeApplication _ _ _ _ function -> collect function
      CheckedImplicitTypeApplication _ function -> collect function
      CheckedForallIntroduction _ body -> collect body
      CheckedContextIntroduction _ body -> collect body
      CheckedContextApplication _ function -> collect function
      CheckedTuple elements -> concatMap collect elements
      CheckedLet _ _ binding body -> collect binding ++ collect body
      CheckedEmptyCase scrutinee -> collect scrutinee
      CheckedConstructorMatch _ scrutinee alternatives ->
        collect scrutinee
          ++ concatMap (\(CheckedCaseAlternative _ _ _ body) -> collect body)
              alternatives

    originSpine slots (CheckedTerm _ form) = case form of
      CheckedVisibleTypeApplication _ _ _ (Just (candidate, slot)) function
        | candidate == identifier -> originSpine (slot : slots) function
      CheckedVisibleTypeApplication _ _ _ Nothing function ->
        if null slots then originSpine slots function else Nothing
      CheckedGlobal name candidate -> Just (name, candidate, slots)
      _ -> Nothing

-- | The origin's identity within one checked expression: a counter allocated
-- in checking order, unique among that expression's origins only.
checkedTypeApplicationOriginId :: CheckedTypeApplicationOrigin -> Natural
checkedTypeApplicationOriginId
    (CheckedTypeApplicationOrigin identifier _ _ _) = identifier

-- | The global binding whose specified scheme the visible type applications
-- instantiate.
checkedTypeApplicationOriginOwner
  :: CheckedTypeApplicationOrigin
  -> QualifiedName
checkedTypeApplicationOriginOwner
    (CheckedTypeApplicationOrigin _ owner _ _) = owner

-- | The owner's exact leading-forall scheme as retained by the checker
-- context, before any visible argument was applied.
checkedTypeApplicationOriginSource
  :: CheckedTypeApplicationOrigin
  -> HsType
checkedTypeApplicationOriginSource
    (CheckedTypeApplicationOrigin _ _ source _) = source

-- | The recorded visible applications, in structural source order.  Their
-- slots form the contiguous zero-based prefix of the owner's specified
-- binders that the expression instantiated.
checkedTypeApplicationOriginSteps
  :: CheckedTypeApplicationOrigin
  -> [CheckedTypeApplicationOriginStep]
checkedTypeApplicationOriginSteps
    (CheckedTypeApplicationOrigin _ _ _ steps) = steps

-- | The zero-based position of this visible application in the owner's
-- direct type-application spine.
checkedTypeApplicationOriginStepSlot
  :: CheckedTypeApplicationOriginStep
  -> Natural
checkedTypeApplicationOriginStepSlot
    (CheckedTypeApplicationOriginStep slot _ _ _ _) = slot

-- | The quantified type the function had immediately before this visible
-- application consumed its next binder.
checkedTypeApplicationOriginStepSource
  :: CheckedTypeApplicationOriginStep
  -> HsType
checkedTypeApplicationOriginStepSource
    (CheckedTypeApplicationOriginStep _ source _ _ _) = source

-- | The type substituted for the consumed binder: the closed argument as
-- written, or a fresh variable for an inferred (@\@_@) argument.
checkedTypeApplicationOriginStepSelected
  :: CheckedTypeApplicationOriginStep
  -> HsType
checkedTypeApplicationOriginStepSelected
    (CheckedTypeApplicationOriginStep _ _ selected _ _) = selected

-- | The function's type after this visible application.
checkedTypeApplicationOriginStepResult
  :: CheckedTypeApplicationOriginStep
  -> HsType
checkedTypeApplicationOriginStepResult
    (CheckedTypeApplicationOriginStep _ _ _ result _) = result

-- | The source contexts which became unconditional obligations at this step;
-- empty unless the step consumed the last binder of its forall layer.
checkedTypeApplicationOriginStepObligations
  :: CheckedTypeApplicationOriginStep
  -> [HsConstraint]
checkedTypeApplicationOriginStepObligations
    (CheckedTypeApplicationOriginStep _ _ _ _ obligations) = obligations

data CheckedTermResult = CheckedTermResult
  HsType
  (Either ExferenceTermGraphAbsence CheckedTerm)

data CheckedTerm = CheckedTerm HsType CheckedTermForm

-- These coordinates belong to the independent check, not the search tree.
-- Graph lowering replaces the introduction coordinate with its actual graph
-- occurrence; the ordered source slot is retained unchanged.
data CheckedGiven = CheckedGiven !Natural !Natural HsConstraint

data CheckedOpening
  = CheckedOpenForall HsType HsType
  | CheckedOpenContext HsType

data CheckedCaseAlternative = CheckedCaseAlternative
  QualifiedName
  HsType
  [(TVarId, HsType)]
  CheckedTerm

data CheckedMatchSyntax = CheckedCaseExpression | CheckedPatternBinding

data CheckedTermForm
  = CheckedLocal TVarId
  | CheckedGlobal QualifiedName (Maybe Natural)
  | CheckedLambda TVarId HsType CheckedTerm
  | CheckedApply CheckedTerm CheckedTerm
  | CheckedVisibleTypeApplication
      SharedGenerated.VisibleTypeArgument HsType Bool
      (Maybe (Natural, Natural)) CheckedTerm
  | CheckedImplicitTypeApplication HsType CheckedTerm
  | CheckedForallIntroduction HsType CheckedTerm
  | CheckedContextIntroduction !Natural CheckedTerm
  | CheckedContextApplication [CheckedGiven] CheckedTerm
  | CheckedTuple [CheckedTerm]
  | CheckedLet TVarId HsType CheckedTerm CheckedTerm
  | CheckedEmptyCase CheckedTerm
  | CheckedConstructorMatch CheckedMatchSyntax CheckedTerm [CheckedCaseAlternative]

data CheckState = CheckState
  { checkFlexibleIds :: !FlexibleIdSupply
  , checkAliveFlexibleIds :: !IntSet.IntSet
  , checkSubstitutions :: !Substs
  , checkLocalGivens :: [HsConstraint]
  , checkEvidenceGivens :: [CheckedGiven]
  , checkNextContextIntroduction :: !Natural
  , checkConstraints :: [ScopedConstraint]
  , checkGivenInference :: [(IntSet.IntSet, [ScopedConstraint])]
  , checkRigidPlan :: !RigidInstantiationPlan
  , checkRigidScope :: !RigidScope
  , checkCandidateRigidIds :: !IntSet.IntSet
  , checkIntroducedRigidIds :: !IntSet.IntSet
  , checkRigidAlpha :: !(IntMap.IntMap TVarId)
  , checkRigidAlphaInverse :: !(IntMap.IntMap TVarId)
  , checkNextTypeApplicationOrigin :: !Natural
  , checkTypeApplicationOrigins :: [CheckedTypeApplicationOrigin]
  , checkProviderOccurrences :: [CheckedProviderOccurrence]
  }

-- | Fixed, independently validated inputs for checking many candidates from
-- one query. The constructor is hidden so candidate checking can rely on the
-- cached goal instantiation and class assumptions without trusting a search
-- node or repeatedly scanning the complete environment.
data ExpressionCheckContext = ExpressionCheckContext
  HsType
  [CheckedOpening]
  QueryClassEnv
  [FunctionBinding]
  [DeconstructorBinding]
  (Map.Map QualifiedName HsType)
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
    functions deconstructors Map.empty goal
  () <$ checkValidatedExpression IntSet.empty context expected expression

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
    functions deconstructors Map.empty goal
  () <$ checkValidatedExpression IntSet.empty context expected expression

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
    goal = prepareExpressionCheckContextWithSchemes plan classEnvironment
      functions deconstructors Map.empty goal

-- | Stable-session counterpart retaining exact specified forall schemes for
-- global visible type application.  The map is derived from the same checked
-- inventory as the flattened search bindings; compatibility callers, which
-- cannot prove binder order, continue through 'prepareExpressionCheckContext'
-- with an empty map.
prepareExpressionCheckContextWithSchemes
  :: RigidInstantiationPlan
  -> QueryClassEnv
  -> [FunctionBinding]
  -> [DeconstructorBinding]
  -> Map.Map QualifiedName HsType
  -> HsType
  -> Either ExpressionCheckError ExpressionCheckContext
prepareExpressionCheckContextWithSchemes plan classEnvironment functions
    deconstructors schemes goal = do
  validateCheckContextInputs classEnvironment functions deconstructors goal
  case validatePreparedFunctionSchemes functions schemes of
    Left name -> Left $ InvalidCheckFunctionScheme name
    Right () -> Right ()
  prepareValidatedExpressionCheckContext plan classEnvironment functions
    deconstructors schemes goal

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
  -> Map.Map QualifiedName HsType
  -> HsType
  -> Either ExpressionCheckError ExpressionCheckContext
prepareValidatedExpressionCheckContext plan classEnvironment functions
    deconstructors schemes goal = do
  context <- prepareExpressionCheckContextUnchecked plan classEnvironment
    functions deconstructors schemes goal
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
  () <$ checkValidatedExpression IntSet.empty context expected expression

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
  () <$ checkExpressionInContextWithNestedRigidProvenanceEvidence
    context provenance expected expression

-- | Low-level evidence-producing counterpart used by the engine after the
-- same complete candidate validation as the compatibility checker. The
-- evidence is intentionally opaque; graph availability is computed lazily
-- only if an internal consumer asks for it.
checkExpressionInContextWithNestedRigidProvenanceEvidence
  :: ExpressionCheckContext
  -> NestedRigidProvenance
  -> [HsConstraint]
  -> Expression
  -> Either ExpressionCheckError CheckedExpressionEvidence
checkExpressionInContextWithNestedRigidProvenanceEvidence
    context provenance expected expression = do
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
  -> Map.Map QualifiedName HsType
  -> HsType
  -> Either ExpressionCheckError ExpressionCheckContext
prepareExpressionCheckContextUnchecked plan classEnvironment functions
    deconstructors schemes goal = do
  (checkedGoal, openedConstraints, openings) <- instantiateGoal plan goal
  pure $ ExpressionCheckContext
    checkedGoal
    -- Preserve the exact source-owned opening plan for graph construction.
    -- Reconstructing closure from free variables later would lose the original
    -- binder order and its association with these checked rigid identities.
    openings
    (addQueryClassEnv openedConstraints classEnvironment)
    functions
    deconstructors
    schemes
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
  -> Either ExpressionCheckError CheckedExpressionEvidence
checkValidatedExpression provenCandidateRigids
    (ExpressionCheckContext checkedGoal rootOpenings augmentedEnvironment
      functions deconstructors functionSchemes _ rigidPlan)
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
        , checkEvidenceGivens = []
        , checkNextContextIntroduction = 0
        , checkConstraints = []
        , checkGivenInference = []
        , checkRigidPlan = rigidPlan
        , checkRigidScope = emptyRigidScope
        , checkCandidateRigidIds = candidateRigids
        , checkIntroducedRigidIds = IntSet.empty
        , checkRigidAlpha = IntMap.empty
        , checkRigidAlphaInverse = IntMap.empty
        , checkNextTypeApplicationOrigin = 0
        , checkTypeApplicationOrigins = []
        , checkProviderOccurrences = []
        }
  (checkedResult, finalState) <- runStateT (do
    checked <- checkOpenedTelescope rootOpenings $
      checkAgainst IntMap.empty expression checkedGoal
    inferLexicalProviderSelections
    pure checked) initialState
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
  repaired <- maybe (Left VisibleTypeApplicationOccurrenceTraceMismatch) Right $
    repairInferredProviderApplications substitutions
      (reverse $ checkProviderOccurrences finalState) expression
  pure $ CheckedExpressionEvidence
    ( SharedGenerated.discardUnusedPatternBindingsBy id
    $ toGeneratedExpression expression
    )
    (normalizeCheckedTermResult substitutions rigidAlpha checkedResult)
    ( map (normalizeCheckedTypeApplicationOrigin substitutions rigidAlpha)
    $ reverse $ checkTypeApplicationOrigins finalState
    )
    (if repaired == expression then Nothing else Just repaired)
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
      -> Check CheckedTermResult
    checkAgainst variables checkedExpression rawExpected = do
      expectedType <- zonk $ SharedType.canonicalizeType rawExpected
      recordAliveType expectedType
      case (checkedExpression, expectedType) of
        (_, TypeForall _ _ _) ->
          orElseTransactionally
            (do
              inferred <- infer variables checkedExpression
              unifyTypes (checkedResultType inferred) expectedType
              pure inferred)
            (do
              introduced <- introduceExpectedForallChain
                variables checkedExpression expectedType
              -- The independently checked source telescope owns every
              -- opening, including context-free layers. Keep its witnesses;
              -- sealing still checks fresh rigid scope and exact erasure.
              pure introduced)
        (ExpLambda variable annotation body, TypeArrow parameter result) -> do
          unifyTypes annotation parameter
          checkedBody <- checkAgainst
            (IntMap.insert variable annotation variables)
            body
            result
          pure $ unaryCheckedTerm expectedType
            (CheckedLambda variable annotation) checkedBody
        -- A let does not hide the expected result type from its body. This
        -- matters when a partial application exposes a polymorphic provider:
        -- infer-only traversal would choose a monomorphic type for a fresh
        -- lambda argument before learning that the surrounding result requires
        -- a polytype. The binding is still checked independently against its
        -- retained declaration; only the trusted outer expectation flows into
        -- the body, exactly as for a lambda or application below.
        (ExpLet variable annotation binding body, _) -> do
          checkedBinding <- checkAgainst variables binding annotation
          checkedBody <- checkAgainst
            (IntMap.insert variable annotation variables) body expectedType
          pure $ binaryCheckedTerm expectedType
            (CheckedLet variable annotation) checkedBinding checkedBody
        -- Checking an application from its result propagates the trusted
        -- expected type back through a partial application spine.  Pure
        -- bottom-up inference instantiates a constructor's parameters before
        -- it can see that, for example, one tuple component must itself be a
        -- quantified value; a lambda in that position is then irreversibly
        -- inferred as a monotype.  The fresh parameter below is still solved
        -- solely by the function's independently reconstructed type, and the
        -- argument is checked only after that solution is zonked, so this
        -- admits no guessed polytype or unchecked annotation.
        (ExpApply function argument, _) -> do
          parameter <- freshTypeVariable
          checkedFunction <- checkAgainst variables function
            (TypeArrow parameter expectedType)
          parameter' <- zonk parameter
          checkedArgument <- checkAgainst variables argument parameter'
          pure $ binaryCheckedTerm expectedType CheckedApply
            checkedFunction checkedArgument
        -- A structural tuple carries its arity but not independently
        -- inferable polytypes for its fields.  Check each field against the
        -- corresponding expected component so rank-N lambdas and forwarded
        -- schemes retain the same bidirectional boundary as applications.
        (ExpTuple elements, TypeTuple Boxed expectedElements)
          | length elements == length expectedElements -> do
              checkedElements <- sequence
                $ zipWith (checkAgainst variables) elements expectedElements
              pure $ manyCheckedTerms expectedType CheckedTuple checkedElements
        _ -> do
          inferred <- infer variables checkedExpression
          unifyTypes (checkedResultType inferred) expectedType
          pure inferred

    -- Choosing introduction commits to the complete leading chain, matching
    -- search's continuation mode. Every layer's substituted contexts are
    -- lexical givens only while checking its body; generated obligations keep
    -- a snapshot of those givens so a sibling cannot consume them later.
    introduceExpectedForallChain variables checkedExpression source =
      case source of
        TypeForall binders _ body -> do
          instantiations <- mapM allocateCanonicalNestedRigid binders
          alive <- gets checkAliveFlexibleIds
          rigidScope <- gets checkRigidScope
          let rigids = map snd instantiations
              substitutions = IntMap.fromList
                [ (binder, TypeConstant rigid)
                | (binder, rigid) <- instantiations
                ]
          modify' $ \current -> current
            { checkRigidScope = registerRigidScope alive rigids rigidScope
            , checkIntroducedRigidIds = IntSet.union
                (IntSet.fromList rigids)
                (checkIntroducedRigidIds current)
            }
          -- Keep the established simultaneous substitution for checking.
          -- Its graph witnesses replay that same layer one binder at a time.
          checkOpenedTelescope (checkedLayerOpenings source instantiations) $
            introduceExpectedForallChain variables checkedExpression $
              snd $ applySubsts substitutions body
        body -> checkAgainst variables checkedExpression body

    infer :: VariableEnvironment -> Expression -> Check CheckedTermResult
    infer variables (ExpVar variable annotation) = do
      declared <- maybe (throwCheck $ UnknownVariable variable) pure
        $ IntMap.lookup variable variables
      declared' <- zonk declared
      annotation' <- zonk annotation
      checked <- case classifyProviderUse declared' annotation' of
        OpaqueProviderForwarding -> do
          -- Exact opaque forwarding has priority over elimination, matching
          -- search and preserving explicitly polymorphic occurrences. Merely
          -- being unifiable is not enough: a fresh monotype annotation can
          -- bind to the whole opaque atom, but denotes an instantiated use.
          unifyTypes declared' annotation'
          availableCheckedTerm <$> zonk declared' <*> pure (CheckedLocal variable)
        -- Search records the requested context-free scheme on a
        -- shallow-subsummed occurrence. Classification independently rechecks
        -- that the local provider can instantiate to it without solving free
        -- flexible variables; no temporary matcher substitution is part of
        -- the generated expression.
        SubsumedProviderForwarding -> do
          result <- zonk annotation'
          pure $ unavailableCheckedTerm result
            $ SubsumedLocalSpecialization variable declared' result
        InstantiateProviderUse -> do
          instantiated <- instantiateImplicitLocalProvider
            $ availableCheckedTerm declared' $ CheckedLocal variable
          unifyTypes (checkedResultType instantiated) annotation'
          pure instantiated
        OrdinaryProviderUse -> do
          unifyTypes declared' annotation'
          availableCheckedTerm <$> zonk declared' <*> pure (CheckedLocal variable)
      recordProviderOccurrence (Left variable) declared' $ checkedResultType checked
      pure checked
    infer _ (ExpName name) = do
      checked <- case Map.lookup name functionSchemes of
        Just scheme | Set.null $ SharedType.freeVariables scheme ->
          instantiateImplicitLocalProvider $
            availableCheckedTerm scheme $ CheckedGlobal name Nothing
        _ | Just scheme <- implicitConstructorScheme name ->
          instantiateImplicitLocalProvider $
            availableCheckedTerm scheme $ CheckedGlobal name Nothing
        _ -> do
          instantiated <- instantiateBinding name
          let contextual = any
                (\binding -> functionName binding == name
                  && not (null $ functionConstraints binding)) functions
          pure $ if contextual
            then unavailableCheckedTerm instantiated $ UnsupportedContextEvidence instantiated
            else availableCheckedTerm instantiated $ CheckedGlobal name Nothing
      let instantiated = checkedResultType checked
      recordProviderOccurrence (Right name)
        (Map.findWithDefault instantiated name functionSchemes) instantiated
      pure checked
    infer variables (ExpLambda variable annotation body) = do
      recordAliveType annotation
      checkedBody <- infer (IntMap.insert variable annotation variables) body
      pure $ unaryCheckedTerm
        (TypeArrow annotation $ checkedResultType checkedBody)
        (CheckedLambda variable annotation) checkedBody
    infer variables (ExpApply function argument) = do
      checkedFunction <- infer variables function
      functionType' <- zonk $ checkedResultType checkedFunction
      case functionType' of
        -- A known arrow is a checking boundary for its argument. This admits
        -- a structurally introduced polymorphic argument without guessing a
        -- polytype during ordinary synthesis.
        TypeArrow parameter result -> do
          checkedArgument <- checkAgainst variables argument parameter
          result' <- zonk result
          pure $ binaryCheckedTerm result' CheckedApply
            checkedFunction checkedArgument
        _ -> do
          checkedArgument <- infer variables argument
          resultType <- freshTypeVariable
          unifyTypes functionType'
            (TypeArrow (checkedResultType checkedArgument) resultType)
          result <- zonk resultType
          pure $ binaryCheckedTerm result CheckedApply
            checkedFunction checkedArgument
    infer variables visibleExpression@(ExpTypeApply function argument) =
      case collectVisibleTypeApplicationSpine visibleExpression of
        (ExpName name, arguments) ->
          inferDirectGlobalVisibleTypeApplication name arguments
        _ -> inferVisibleTypeApplication variables function argument

    infer variables (ExpTuple elements) = do
      checkedElements <- mapM (infer variables) elements
      pure $ manyCheckedTerms
        (TypeTuple Boxed $ map checkedResultType checkedElements)
        CheckedTuple checkedElements
    infer _ (ExpHole variable) = throwCheck $ ExpressionHole variable
    infer variables (ExpLetMatch constructor patternVariables binding body) = do
      checkedBinding <- infer variables binding
      fieldTypes <- instantiateConstructor constructor
        $ checkedResultType checkedBinding
      let expectedArity = length fieldTypes
          actualArity = SharedCollection.observedListLength
            expectedArity patternVariables
      when (actualArity /= expectedArity)
        $ throwCheck $ PatternArity constructor
            expectedArity actualArity
      checkedVariables <- foldM addPatternVariable variables
        $ zip patternVariables fieldTypes
      checkedBody <- infer checkedVariables body
      patternType <- zonk $ checkedResultType checkedBinding
      fields <- mapM zonk fieldTypes
      let alternatives = [(constructor, zip (map fst patternVariables) fields, checkedBody)]
      complete <- completeConstructorCase patternType alternatives
      pure $ if complete
        then checkedConstructorMatch CheckedPatternBinding
          (checkedResultType checkedBody) patternType checkedBinding alternatives
        else unavailableCheckedTerm (checkedResultType checkedBody)
          $ constructorPatternAbsence constructor
    infer variables (ExpLet variable annotation binding body) = do
      checkedBinding <- checkAgainst variables binding annotation
      checkedBody <- infer (IntMap.insert variable annotation variables) body
      pure $ binaryCheckedTerm (checkedResultType checkedBody)
        (CheckedLet variable annotation) checkedBinding checkedBody
    infer variables (ExpCaseMatch scrutinee []) = do
      checkedScrutinee <- infer variables scrutinee
      scrutineeType <- zonk $ checkedResultType checkedScrutinee
      matchEmptyDeconstructor scrutineeType
      -- Empty elimination proves every result type. Keep that result fresh so
      -- the surrounding expression, rather than the deconstructor, fixes it.
      result <- freshTypeVariable
      pure $ unaryCheckedTerm result CheckedEmptyCase checkedScrutinee
    infer variables (ExpCaseMatch scrutinee
        alternatives@((firstConstructor, _, _) : _)) = do
      checkedScrutinee <- infer variables scrutinee
      resultType <- freshTypeVariable
      checkedAlternatives <- mapM (checkAlternative variables
        (checkedResultType checkedScrutinee) resultType) alternatives
      scrutineeType <- zonk $ checkedResultType checkedScrutinee
      result <- zonk resultType
      normalizedAlternatives <- mapM normalizeAlternative checkedAlternatives
      complete <- completeConstructorCase scrutineeType normalizedAlternatives
      pure $ if complete
        then checkedConstructorMatch CheckedCaseExpression result scrutineeType
          checkedScrutinee normalizedAlternatives
        else unavailableCheckedTerm result
          $ constructorPatternAbsence firstConstructor

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
      checkedAlternative <- infer checkedVariables body
      unifyTypes resultType $ checkedResultType checkedAlternative
      pure
        ( constructor
        , zipWith (\(variable, _) fieldType -> (variable, fieldType))
            patternVariables fieldTypes
        , checkedAlternative
        )

    normalizeAlternative (constructor, fields, checkedAlternative) = do
      normalizedFields <- mapM
        (\(variable, fieldType) -> do
          normalized <- zonk fieldType
          pure (variable, normalized))
        fields
      pure (constructor, normalizedFields, checkedAlternative)

    -- Every alternative was independently typechecked above. Graph authority
    -- additionally requires the complete constructor inventory of one matching
    -- declaration, instantiated together so field correlations are preserved.
    -- Duplicate or partial alternatives never acquire total-case evidence.
    completeConstructorCase scrutineeType alternatives = StateT $ \initialState ->
      trySchemas initialState matchingSchemas
     where
      names = [constructor | (constructor, _, _) <- alternatives]
      uniqueNames = Set.fromList names
      completeNames constructors =
        length names == Set.size uniqueNames
          && length constructors == length names
          && Set.fromList (map constructorName constructors) == uniqueNames
      matchingSchemas =
        [ deconstructor
        | deconstructor <- deconstructors
        , completeNames $ deconstructorConstructors deconstructor
        ] ++ case (scrutineeType, alternatives) of
          (TypeTuple boxity fields, [(constructor, _, _)])
            | SharedName.nameSpecial constructor ==
                Just (SharedName.TupleConstructor boxity $ length fields) ->
              [DeconstructorBinding scrutineeType
                [ConstructorBinding constructor fields] False]
          _ -> []
      trySchemas initialState [] = Right (False, initialState)
      trySchemas initialState (schema : rest) =
        case runStateT (checkSchema schema) initialState of
          Right checked -> Right checked
          Left FlexibleIdentifierSupplyExhausted ->
            Left FlexibleIdentifierSupplyExhausted
          Left _ -> trySchemas initialState rest
      checkSchema schema = do
        let constructors = deconstructorConstructors schema
        (freshInput :| freshFields, _) <- freshenTypes
          (deconstructorInput schema :| concatMap constructorFields constructors) []
        unifyTypes scrutineeType freshInput
        checkFields freshFields constructors
        pure True
      checkFields _ [] = pure ()
      checkFields fields (constructor : rest) = do
        let (current, remaining) = splitAt (length $ constructorFields constructor) fields
        case [patternFields | (name, patternFields, _) <- alternatives
                            , name == constructorName constructor] of
          [patternFields] | length patternFields == length current ->
            zipWithM_ unifyTypes (map snd patternFields) current
          _ -> throwCheck $ PatternArity (constructorName constructor)
            (length current) 0
        checkFields remaining rest

    checkedConstructorMatch syntax result patternType checkedScrutinee alternatives =
      CheckedTermResult result $ do
        scrutineeTerm <- checkedTermDraft checkedScrutinee
        checked <- traverse (checkedAlternativeTerm patternType) alternatives
        pure $ CheckedTerm result
          $ CheckedConstructorMatch syntax scrutineeTerm checked

    checkedAlternativeTerm patternType (constructor, fields, checkedBody) = do
      body <- checkedTermDraft checkedBody
      pure $ CheckedCaseAlternative constructor patternType fields body

    checkedTermDraft (CheckedTermResult _ draft) = draft

    -- Preserve the historical one-step recursion for arbitrary functions.
    -- Only a syntactically direct global spine can be associated with an
    -- exact checker-context scheme; search routes and inferred function
    -- values are deliberately not treated as provenance.
    inferVisibleTypeApplication variables function argument = do
      (functionType, checkedFunction) <- case function of
        -- Ordinary global inference intentionally keeps using the flattened
        -- binding.  A visible application, however, must start from the exact
        -- specified scheme or it would try to apply a type argument to the
        -- already-instantiated monotype.
        ExpName name -> do
          scheme <- visibleBindingScheme name >>= zonk
          pure
            ( scheme
            , availableCheckedTerm scheme $ CheckedGlobal name Nothing
            )
        _ -> do
          checked <- infer variables function
          checkedType <- zonk $ checkedResultType checked
          pure (checkedType, checked)
      (selected, result, contextualApplication, _) <-
        instantiateVisibleTypeArgument argument functionType
      pure $ unaryCheckedTerm result
        (CheckedVisibleTypeApplication argument selected
          contextualApplication Nothing)
        checkedFunction

    -- Consume a maximal direct-global spine in source order.  An origin is
    -- allocated only when the exact retained scheme is lexically closed and
    -- the expression supplies a complete, bounded, specified source
    -- telescope.  A returned polymorphic result may accept further visible
    -- arguments, but those suffix applications never inherit this origin.
    inferDirectGlobalVisibleTypeApplication name arguments = do
      scheme <- visibleBindingScheme name >>= zonk
      let eligibleArity = Map.lookup name functionSchemes
            >>= (`eligibleTypeApplicationOriginArity` arguments)
      origin <- case eligibleArity of
        Nothing -> pure Nothing
        Just arity -> do
          identifier <- allocateTypeApplicationOrigin
          pure $ Just (identifier, arity)
      let globalOrigin = fmap fst origin
          checkedGlobal = availableCheckedTerm scheme
            $ CheckedGlobal name globalOrigin
      (_, checked, reversedSteps) <- foldM
        (applyDirectGlobalVisibleTypeArgument origin)
        (0, checkedGlobal, []) arguments
      case origin of
        Nothing -> pure ()
        Just (identifier, _) -> modify' $ \current -> current
          { checkTypeApplicationOrigins = CheckedTypeApplicationOrigin
              identifier name scheme (reverse reversedSteps)
              : checkTypeApplicationOrigins current
          }
      pure checked

    applyDirectGlobalVisibleTypeArgument origin
        (slot, checkedFunction, steps) argument = do
      source <- zonk $ checkedResultType checkedFunction
      (selected, result, contextualApplication, activated) <-
        instantiateVisibleTypeArgument argument source
      let annotation = case origin of
            Just (identifier, arity)
              | slot < fromIntegral arity -> Just (identifier, slot)
            _ -> Nothing
          checked = unaryCheckedTerm result
            (CheckedVisibleTypeApplication argument selected
              contextualApplication annotation)
            checkedFunction
          retainedSteps = case annotation of
            Nothing -> steps
            Just _ -> CheckedTypeApplicationOriginStep
              slot source selected result activated : steps
      pure (slot + 1, checked, retainedSteps)

    collectVisibleTypeApplicationSpine = collect []
     where
      collect arguments (ExpTypeApply function argument) =
        collect (argument : arguments) function
      collect arguments function = (function, arguments)

    eligibleTypeApplicationOriginArity source arguments
      | observedArity == 0 = Nothing
      | not $ specifiedPrefix observedArity arguments = Nothing
      | not $ Set.null $ SharedType.freeVariables source = Nothing
      | otherwise = Just observedArity
     where
      -- The source came from the checked retained environment. Its exact
      -- finite prefix, rather than the heuristic tuple frontier, owns the
      -- arity of this evidence origin.
      observedArity = length $ SharedType.leadingForallVariables source

    specifiedPrefix 0 _ = True
    specifiedPrefix _ [] = False
    specifiedPrefix remaining (argument : arguments) =
      case SharedGenerated.visibleTypeArgumentClosedType argument of
        Nothing -> False
        Just _ -> specifiedPrefix (remaining - 1) arguments

    allocateTypeApplicationOrigin = do
      identifier <- gets checkNextTypeApplicationOrigin
      modify' $ \current -> current
        { checkNextTypeApplicationOrigin = identifier + 1 }
      pure identifier

    recordProviderOccurrence identity source selected = modify' $ \current -> current
      { checkProviderOccurrences = (identity, source, selected) :
          checkProviderOccurrences current }

    -- Constructors have no specified-binder sidecar: their source authority
    -- is the validated datatype input and ordered fields. A root/nested
    -- skolem must enter through a checked implicit application, never be
    -- baked into a global leaf. Match the corresponding flat binding before
    -- using that declaration; raw checker inputs need not have passed the
    -- shared inventory's constructor/function correspondence gate.
    -- This closure supplies no visible-application scheme or certificate.
    implicitConstructorScheme name = case
        [ source
        | deconstructor <- deconstructors
        , constructor <- deconstructorConstructors deconstructor
        , constructorName constructor == name
        , binding <- functions
        , functionName binding == name
        , null $ functionConstraints binding
        , let source = forallify $ SharedType.functionType
                (constructorFields constructor) (deconstructorInput deconstructor)
        , Set.null $ SharedType.freeVariables source
        , SharedTypeAtom.alphaEquivalentTypes source
            $ forallify $ functionBindingType binding
        ] of
      -- forallify closes even ground types with an empty wrapper. That
      -- synthetic no-op must not hide a monomorphic constructor's arrow;
      -- there is no binder or dictionary to instantiate in this case.
      TypeForallNative [] [] body : _ -> Just body
      source : _ -> Just source
      [] -> Nothing

    instantiateBinding name = case
        [binding | binding <- functions, functionName binding == name] of
      [] -> throwCheck $ UnknownBinding name
      binding : _ -> do
        let constraints = functionConstraints binding
        previousSupply <- gets checkFlexibleIds
        (freshType :| _, freshConstraints) <- freshenTypes
          (functionBindingType binding :| []) constraints
        localGivens <- gets checkLocalGivens
        let fresh = IntSet.filter
              (not . (`identifierIsReserved` previousSupply)) $ IntSet.unions $
                flexibleFreeIdentifiers freshType :
                  map (IntSet.unions . map flexibleFreeIdentifiers . constraint_params) freshConstraints
            obligations = scopedConstraints localGivens freshConstraints
        modify' $ \current -> current
          { checkConstraints =
              obligations ++ checkConstraints current
          , checkGivenInference = (fresh, obligations) : checkGivenInference current
          }
        pure freshType

    visibleBindingScheme name = case
        [binding | binding <- functions, functionName binding == name] of
      [] -> throwCheck $ UnknownBinding name
      _ : _ -> case Map.lookup name functionSchemes of
        Nothing -> instantiateBinding name
        Just scheme -> recordAliveType scheme >> pure scheme

    -- Local polymorphic values are instantiated independently at every use.
    -- Their direct forall contexts become ordinary checker obligations. The
    -- generated occurrence annotation contains search's instantiated
    -- monotype, so checking does not need to reproduce search's fresh IDs.
    -- Keep the checker's own selections at this local occurrence. No search
    -- annotation is accepted as an instantiation witness: fresh variables are
    -- introduced here, constrained by the independently checked use, and
    -- normalized with the complete check's substitutions before graph sealing.
    -- A type application preserves the qualified intermediate type. Only a
    -- separate context application consumes its dictionaries, carrying the
    -- exact lexical snapshot until final substitutions are known.
    instantiateImplicitLocalProvider original = do
      -- A constructor-derived local scheme may contain source binder IDs
      -- absent from the initial goal. Reserve its whole namespace before any
      -- binder is erased, as in instantiateLeadingForallsWith.
      modify' $ \current -> current
        { checkFlexibleIds = reserveIdentifiers
            (IntSet.toAscList $ flexibleIdentifiers $ checkedResultType original)
            (checkFlexibleIds current)
        }
      consume IntSet.empty [] original
     where
      consume fresh obligations checked = case checkedResultType checked of
        TypeForallNative (binder : remaining) contexts body -> do
          selected <- freshTypeVariable
          let result = SharedType.canonicalizeType $
                substituteScopedVariable binder selected $
                  if null remaining && null contexts then body
                    else TypeForallNative remaining contexts body
          recordAliveType result
          consume (fresh `IntSet.union` flexibleFreeIdentifiers selected) obligations $
            unaryCheckedTerm result
            (CheckedImplicitTypeApplication selected) checked
        source@(TypeForallNative [] constraints@(_ : _) body) -> do
          localGivens <- gets checkLocalGivens
          evidenceGivens <- gets checkEvidenceGivens
          modify' $ \current -> current
            { checkConstraints = scopedConstraints localGivens constraints
                ++ checkConstraints current }
          recordAliveType source
          consume fresh (obligations ++ scopedConstraints localGivens constraints) $
            unaryCheckedTerm body
            (CheckedContextApplication evidenceGivens) checked
        _ -> do
          unless (null obligations) $ modify' $ \current -> current
            { checkGivenInference = (fresh, obligations) : checkGivenInference current }
          pure checked

    -- Visible application consumes exactly one binder. Contexts attached to
    -- a multi-binder layer become obligations only after its last binder has
    -- been selected, matching GHC's specified-binder application order.
    instantiateVisibleTypeArgument argument = consume []
     where
      -- Binderless contextual wrappers are semantically outside the next
      -- quantified layer. Preserve them without substituting the inner binder
      -- into an unrelated free variable that happens to reuse its identity.
      consume outerContexts
          (TypeForallNative [] contexts body) =
        consume (outerContexts ++ contexts) body
      consume outerContexts
          (TypeForallNative (binder : remainingBinders) contexts body) = do
        maybe
          (throwCheck $ VisibleTypeApplicationRigidBinder binder)
          (const $ pure ())
          $ SharedType.flexibleVariableIdentity binder
        replacement <- case
            ( SharedGenerated.isInferredVisibleTypeArgument argument
            , SharedGenerated.visibleTypeArgumentPatternType argument
            ) of
          (True, Nothing) -> freshTypeVariable
          (False, Just patternType) ->
            instantiatePatternVisibleTypeArgument patternType
          -- The shared constructor is abstract, so the discriminator and
          -- structural view cannot disagree. Fail closed if that contract is
          -- ever extended rather than silently changing explicit evidence
          -- into inference.
          _ -> throwCheck FlexibleIdentifierSupplyExhausted
        let substitute = substituteScopedVariable binder replacement
            instantiatedContexts = map (fmap substitute) contexts
            instantiatedBody = substitute body
        if null remainingBinders
          then do
            (result, contextualApplication, activatedContexts) <- finishLayer
              substitute outerContexts instantiatedContexts body
            pure
              ( replacement
              , result
              , contextualApplication
              , activatedContexts
              )
          else do
            let quantified = TypeForallNative remainingBinders
                  instantiatedContexts instantiatedBody
                remaining = case outerContexts of
                  [] -> quantified
                  _ -> TypeForallNative [] outerContexts quantified
            recordAliveType remaining
            pure
              ( replacement
              , remaining
              , not $ null outerContexts
              , []
              )
      consume _ source =
        throwCheck $ VisibleTypeApplicationToMonotype source

      -- A binderless wrapper after the final binder belongs to the same
      -- leading contextual chain. If another binder follows in the
      -- unsubstituted source body, retain every accumulated context outside
      -- it for the next visible application. If no source binder follows,
      -- discharge the complete chain and only then substitute its result.
      -- Constraints attached to the binder's own forall layer remain directly
      -- representable; only separately wrapped contexts make the retained
      -- visible application contextual.
      finishLayer substitute outerContexts layerContexts sourceBody =
        case peelBinderlessContexts [] sourceBody of
          (trailingContexts, TypeForallNative (_ : _) _ _) -> do
            let retainedContexts = outerContexts ++ layerContexts
                remaining = case retainedContexts of
                  [] -> substitute sourceBody
                  _ -> TypeForallNative [] retainedContexts
                      $ substitute sourceBody
            recordAliveType remaining
            pure
              ( remaining
              , not (null outerContexts && null trailingContexts)
              , []
              )
          (trailingContexts, sourceResult) -> do
            localGivens <- gets checkLocalGivens
            let dischargedContexts =
                  outerContexts ++ layerContexts
                    ++ map (fmap substitute) trailingContexts
                result = substitute sourceResult
            modify' $ \current -> current
              { checkConstraints =
                  scopedConstraints localGivens dischargedContexts
                    ++ checkConstraints current
              }
            recordAliveType result
            pure
              ( result
              , not $ null dischargedContexts
              , dischargedContexts
              )

      peelBinderlessContexts contexts
          (TypeForallNative [] nestedContexts nestedBody) =
        peelBinderlessContexts (contexts ++ nestedContexts) nestedBody
      peelBinderlessContexts contexts body = (contexts, body)

    -- A specified visible argument owns a separate, alpha-normalized binder
    -- namespace. Reserve one checker-local flexible identity for every binder
    -- before translating any occurrence. Unlike an inference metavariable,
    -- these identities are lexically bound by the translated forall tree, so
    -- they must not enter 'checkAliveFlexibleIds' as live free variables.
    -- Mapping the complete type preserves nested shadowing and contexts: the
    -- shared closed identities distinguish every lexical scope and slot.
    instantiatePatternVisibleTypeArgument source = do
      binders <- maybe (throwCheck FlexibleIdentifierSupplyExhausted) pure $
        sequence $ SharedType.typeBinderVariables source
      replacements <- mapM (const reserveClosedVisibleTypeBinder) binders
      let renaming = Map.fromList $ zip binders replacements
          occurrence Nothing = reserveClosedVisibleTypeBinder
          occurrence (Just variable) = maybe
            (throwCheck FlexibleIdentifierSupplyExhausted) pure $
              Map.lookup variable renaming
      translated <- traverse occurrence source
      -- Every wildcard is a distinct metavariable. Record only its free
      -- identity as live; lexical forall identities remain bound. Subsequent
      -- checking solves the complete application against its actual expected
      -- type, retaining all correlations in the checked term evidence.
      recordAliveType translated
      pure translated

    reserveClosedVisibleTypeBinder = do
      supply <- gets checkFlexibleIds
      case allocateFreshIdentifier supply of
        Nothing -> throwCheck FlexibleIdentifierSupplyExhausted
        Just (identifier, nextSupply) -> do
          modify' $ \current -> current {checkFlexibleIds = nextSupply}
          pure $ SharedType.FlexibleVariable identifier

    instantiateConstructor name scrutineeType = case
        SharedName.nameSpecial name of
      Just (SharedName.TupleConstructor boxity arity) -> do
        freshFields <- replicateM arity freshTypeVariable
        unifyTypes scrutineeType $ TypeTuple boxity freshFields
        mapM zonk freshFields
      _ -> case
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

checkedResultType :: CheckedTermResult -> HsType
checkedResultType (CheckedTermResult ty _) = ty

constructorPatternAbsence
  :: QualifiedName
  -> ExferenceTermGraphAbsence
constructorPatternAbsence constructor = case SharedName.nameSpecial constructor of
  Nothing -> NominalConstructorPattern constructor
  Just _ -> UnsupportedStructuralConstructorPattern constructor

availableCheckedTerm :: HsType -> CheckedTermForm -> CheckedTermResult
availableCheckedTerm ty form = CheckedTermResult ty
  $ Right $ CheckedTerm ty form

unavailableCheckedTerm
  :: HsType
  -> ExferenceTermGraphAbsence
  -> CheckedTermResult
unavailableCheckedTerm ty reason = CheckedTermResult ty $ Left reason

unaryCheckedTerm
  :: HsType
  -> (CheckedTerm -> CheckedTermForm)
  -> CheckedTermResult
  -> CheckedTermResult
unaryCheckedTerm ty form (CheckedTermResult _ child) = CheckedTermResult ty $ do
  child' <- child
  pure $ CheckedTerm ty $ form child'

binaryCheckedTerm
  :: HsType
  -> (CheckedTerm -> CheckedTerm -> CheckedTermForm)
  -> CheckedTermResult
  -> CheckedTermResult
  -> CheckedTermResult
binaryCheckedTerm ty form (CheckedTermResult _ left)
    (CheckedTermResult _ right) = CheckedTermResult ty $ do
  left' <- left
  right' <- right
  pure $ CheckedTerm ty $ form left' right'

manyCheckedTerms
  :: HsType
  -> ([CheckedTerm] -> CheckedTermForm)
  -> [CheckedTermResult]
  -> CheckedTermResult
manyCheckedTerms ty form children = CheckedTermResult ty $ do
  children' <- traverse checkedChild children
  pure $ CheckedTerm ty $ form children'
 where
  checkedChild (CheckedTermResult _ child) = child

-- Apply the checker's complete final substitution and rigid-alpha state to
-- every retained annotation. Repeating to a fixed point mirrors 'zonk' and
-- prevents the later lazy graph boundary from observing stale metavariables.
normalizeCheckedTermResult
  :: Substs
  -> IntMap.IntMap TVarId
  -> CheckedTermResult
  -> CheckedTermResult
normalizeCheckedTermResult substitutions rigidAlpha
    (CheckedTermResult ty draft) = CheckedTermResult
      (normalizeCheckedType substitutions rigidAlpha ty)
      (either (Left . normalizeAbsence) (Right . normalizeTerm) draft)
 where
  normalize = normalizeCheckedType substitutions rigidAlpha

  normalizeAbsence reason = case reason of
    ImplicitLocalSpecialization variable declared selected ->
      ImplicitLocalSpecialization variable
        (normalize declared) (normalize selected)
    SubsumedLocalSpecialization variable declared selected ->
      SubsumedLocalSpecialization variable
        (normalize declared) (normalize selected)
    NestedForallIntroduction introduced ->
      NestedForallIntroduction $ normalize introduced
    NominalConstructorPattern{} -> reason
    UnsupportedStructuralConstructorPattern{} -> reason
    UnsupportedContextualVisibleApplication source selected result ->
      UnsupportedContextualVisibleApplication
        (normalize source) (normalize selected) (normalize result)
    UnsupportedContextEvidence source -> UnsupportedContextEvidence $ normalize source
    UnsupportedContextualCertificateGraph -> reason
    TermGraphEvidenceMismatch -> reason
    TermGraphConstructionLimit{} -> reason
    TermGraphSealingFailure{} -> reason
    TermGraphCertificateAssociationFailure{} -> reason
    TermGraphProjectionMismatch -> reason

  normalizeTerm (CheckedTerm termType form) = CheckedTerm
    (normalize termType) $ case form of
      CheckedLocal{} -> form
      CheckedGlobal{} -> form
      CheckedLambda variable annotation body -> CheckedLambda
        variable (normalize annotation) (normalizeTerm body)
      CheckedApply function argument -> CheckedApply
        (normalizeTerm function) (normalizeTerm argument)
      CheckedVisibleTypeApplication argument selected contextualApplication
          origin function -> CheckedVisibleTypeApplication argument
            (normalize selected) contextualApplication origin
            (normalizeTerm function)
      CheckedImplicitTypeApplication selected function ->
        CheckedImplicitTypeApplication (normalize selected) (normalizeTerm function)
      CheckedForallIntroduction selected body ->
        CheckedForallIntroduction (normalize selected) (normalizeTerm body)
      CheckedContextIntroduction introduction body ->
        CheckedContextIntroduction introduction (normalizeTerm body)
      CheckedContextApplication givens function -> CheckedContextApplication
        [CheckedGiven introduction slot (fmap normalize constraint)
        | CheckedGiven introduction slot constraint <- givens]
        (normalizeTerm function)
      CheckedTuple elements -> CheckedTuple $ map normalizeTerm elements
      CheckedLet variable annotation binding body -> CheckedLet
        variable (normalize annotation)
        (normalizeTerm binding) (normalizeTerm body)
      CheckedEmptyCase scrutinee -> CheckedEmptyCase $ normalizeTerm scrutinee
      CheckedConstructorMatch syntax scrutinee alternatives ->
        CheckedConstructorMatch syntax
          (normalizeTerm scrutinee)
          (map normalizeAlternative alternatives)

  normalizeAlternative
      (CheckedCaseAlternative constructor patternType fields body) =
    CheckedCaseAlternative constructor (normalize patternType)
      [(variable, normalize fieldType) | (variable, fieldType) <- fields]
      (normalizeTerm body)

-- Origin annotations cross the same final substitution and rigid-alpha gate
-- as the checked term.  Keeping normalization here, after constraint
-- resolution and residual comparison, prevents a later association layer
-- from observing provisional checker metavariables.
normalizeCheckedTypeApplicationOrigin
  :: Substs
  -> IntMap.IntMap TVarId
  -> CheckedTypeApplicationOrigin
  -> CheckedTypeApplicationOrigin
normalizeCheckedTypeApplicationOrigin substitutions rigidAlpha
    (CheckedTypeApplicationOrigin identifier owner source steps) =
  CheckedTypeApplicationOrigin identifier owner (normalize source)
    $ map normalizeStep steps
 where
  normalize = normalizeCheckedType substitutions rigidAlpha

  normalizeStep
      (CheckedTypeApplicationOriginStep slot stepSource selected result
        obligations) = CheckedTypeApplicationOriginStep
      slot
      (normalize stepSource)
      (normalize selected)
      (normalize result)
      (map (fmap normalize) obligations)

normalizeCheckedType
  :: Substs
  -> IntMap.IntMap TVarId
  -> HsType
  -> HsType
normalizeCheckedType substitutions rigidAlpha = normalize
 where
  normalize source =
    let applied = snd $ applySubsts substitutions source
        canonical = SharedType.canonicalizeType
          $ applyRigidAlpha rigidAlpha applied
    in if canonical == source then canonical else normalize canonical

-- | Seal a checker-produced draft under a caller-owned deterministic candidate
-- key. This work remains lazy when stored in 'ValidatedEngineCandidate'.
checkedExpressionTermGraph
  :: Natural
  -> CheckedExpressionEvidence
  -> ExferenceTermGraphAvailability
checkedExpressionTermGraph candidateKey
    (CheckedExpressionEvidence compatibility checkedResult origins _) =
  case checkedResult of
    CheckedTermResult _ (Left reason) ->
      ExferenceTermGraphUnavailable reason
    CheckedTermResult _ (Right checkedTerm)
      | not (null origins), checkedTermHasContext checkedTerm ->
          ExferenceTermGraphUnavailable UnsupportedContextualCertificateGraph
      | otherwise ->
          case buildCheckedTermGraph candidateKey checkedTerm of
            Left reason -> ExferenceTermGraphUnavailable reason
            Right source -> case origins of
              [] -> case SharedTyped.sealTermGraphWithContextAndProjection
                  SharedTyped.PreserveLambdaBoundaries
                  SharedTyped.sharedContextTypeStructure
                  (checkedTermTypeStructure checkedTerm)
                  SharedTyped.defaultTermGraphLimits
                  source of
                Left failure -> ExferenceTermGraphUnavailable
                  $ TermGraphSealingFailure failure
                Right graph -> retainPlain compatibility graph
              _ -> case SharedAssociation.sealCheckedTypeApplicationCertificateGraphWithProjection
                  SharedTyped.PreserveLambdaBoundaries
                  SharedCertificate.defaultTypeApplicationCertificateLimits
                  (checkedTermTypeStructure checkedTerm)
                  SharedTyped.defaultTermGraphLimits
                  source
                  (map lowerTypeApplicationOrigin origins) of
                Left failure -> ExferenceTermGraphUnavailable
                  $ associationAbsence failure
                Right checked ->
                  let graph =
                        SharedAssociation.checkedTypeApplicationCertificateGraph
                          checked
                  in if SharedTyped.eraseTermGraph graph == compatibility
                      then ExferenceTermGraphAssociated checked
                      else ExferenceTermGraphUnavailable TermGraphProjectionMismatch

retainPlain
  :: SharedGenerated.Expression TVarId
  -> SharedTyped.TermGraph HsType TVarId
  -> ExferenceTermGraphAvailability
retainPlain compatibility graph
  | SharedTyped.eraseTermGraph graph == compatibility =
      ExferenceTermGraphAvailable graph
  | otherwise = ExferenceTermGraphUnavailable TermGraphProjectionMismatch

lowerTypeApplicationOrigin
  :: CheckedTypeApplicationOrigin
  -> SharedAssociation.TypeApplicationCertificateOrigin SynthesisVariable
lowerTypeApplicationOrigin
    (CheckedTypeApplicationOrigin identifier owner source steps) =
  SharedAssociation.TypeApplicationCertificateOrigin
    (SharedTyped.certificateId identifier)
    owner
    source
    (map lowerStep steps)
 where
  lowerStep (CheckedTypeApplicationOriginStep slot stepSource selected result
      obligations) = SharedAssociation.TypeApplicationCertificateObservation
    slot stepSource selected result obligations

associationAbsence
  :: SharedAssociation.TypeApplicationCertificateAssociationError
      SynthesisVariable TVarId
  -> ExferenceTermGraphAbsence
associationAbsence failure = case failure of
  SharedAssociation.TypeApplicationCertificateAssociationGraphError graph ->
    TermGraphSealingFailure graph
  SharedAssociation.TypeApplicationCertificateAssociationPlanError plan ->
    TermGraphCertificateAssociationFailure $ planAssociationFailure plan
  SharedAssociation.TypeApplicationCertificateContextEvidenceUnsupported owner ->
    TermGraphSealingFailure $ SharedTyped.ContextTypeStructureUnavailable owner
  SharedAssociation.DuplicateGraphTypeApplicationCertificateUse{} ->
    occurrenceFailure
  SharedAssociation.UnexpectedGraphTypeApplicationCertificateUse{} ->
    occurrenceFailure
  SharedAssociation.MissingGraphTypeApplicationCertificateUse{} ->
    occurrenceFailure
  SharedAssociation.MissingAssociatedSealedTermNode{} -> occurrenceFailure
  SharedAssociation.ExpectedTypeApplicationCertificateGlobalBase{} ->
    occurrenceFailure
  SharedAssociation.TypeApplicationCertificateGlobalOwnerMismatch{} ->
    occurrenceFailure
  SharedAssociation.TypeApplicationCertificateGlobalSchemeMismatch{} ->
    occurrenceFailure
  SharedAssociation.TypeApplicationCertificateChildChainMismatch{} ->
    occurrenceFailure
  SharedAssociation.TypeApplicationCertificateChildSourceMismatch{} ->
    occurrenceFailure
  SharedAssociation.TypeApplicationCertificateWitnessSourceMismatch{} ->
    occurrenceFailure
  SharedAssociation.TypeApplicationCertificateVisibleArgumentMismatch{} ->
    occurrenceFailure
  SharedAssociation.TypeApplicationCertificateWitnessSelectedMismatch{} ->
    occurrenceFailure
  SharedAssociation.TypeApplicationCertificateNodeResultMismatch{} ->
    occurrenceFailure
  SharedAssociation.TypeApplicationCertificateWitnessResultMismatch{} ->
    occurrenceFailure
  SharedAssociation.TypeApplicationCertificateInternalStepArityMismatch{} ->
    occurrenceFailure
 where
  occurrenceFailure = TermGraphCertificateAssociationFailure
    TermGraphCertificateOccurrenceAssociationFailure

planAssociationFailure
  :: SharedCertificate.TypeApplicationCertificateError SynthesisVariable
  -> ExferenceTermGraphCertificateAssociationFailure
planAssociationFailure failure = case failure of
  SharedCertificate.TypeApplicationCertificateEntryLimitExceeded{} -> limited
  SharedCertificate.TypeApplicationCertificateSelectionLimitExceeded{} ->
    limited
  SharedCertificate.TypeApplicationCertificateTypeNodeLimitExceeded{} -> limited
  SharedCertificate.TypeApplicationCertificateTypeCollectionLimitExceeded{} ->
    limited
  SharedCertificate.TypeApplicationCertificateTelescopeLimitExceeded{} -> limited
  SharedCertificate.TypeApplicationCertificateObligationLimitExceeded{} ->
    limited
  SharedCertificate.TypeApplicationCertificateObservationLimitExceeded{} ->
    limited
  SharedCertificate.TypeApplicationCertificateObservedObligationLimitExceeded{} ->
    limited
  SharedCertificate.DuplicateTypeApplicationCertificateId{} -> invalid
  SharedCertificate.InvalidTypeApplicationCertificateType{} -> invalid
  SharedCertificate.TypeApplicationCertificateHasNoLeadingBinder{} -> invalid
  SharedCertificate.TypeApplicationCertificateSelectionArityMismatch{} -> invalid
  SharedCertificate.TypeApplicationCertificateReplayEndedEarly{} -> invalid
  SharedCertificate.TypeApplicationCertificateObservationArityMismatch{} ->
    invalid
  SharedCertificate.TypeApplicationCertificateObservationMissingPlan{} -> invalid
  SharedCertificate.TypeApplicationCertificateObservationSlotMismatch{} ->
    invalid
  SharedCertificate.TypeApplicationCertificateObservationSourceMismatch{} ->
    invalid
  SharedCertificate.TypeApplicationCertificateObservationSelectedMismatch{} ->
    invalid
  SharedCertificate.TypeApplicationCertificateObservationResultMismatch{} ->
    invalid
  SharedCertificate.TypeApplicationCertificateObservedObligationCountMismatch{} ->
    invalid
  SharedCertificate.TypeApplicationCertificateObservedObligationClassMismatch{} ->
    invalid
  SharedCertificate.TypeApplicationCertificateObservedObligationArgumentCountMismatch{} ->
    invalid
  SharedCertificate.TypeApplicationCertificateObservedObligationArgumentMismatch{} ->
    invalid
 where
  limited = TermGraphCertificatePlanLimitFailure
  invalid = TermGraphCertificatePlanValidationFailure

-- Constructor-pattern authority is retained only inside the checker-owned
-- draft which proved the complete constructor match. Ordinary terms therefore use
-- the unchanged shared structure, while a retained case admits exactly the
-- constructor/type/field triples independently reconstructed above. The
-- resulting schema is consumed atomically by sealing and is never exposed
-- beside the graph.
checkedTermTypeStructure :: CheckedTerm -> SharedTyped.TypeStructure HsType
checkedTermTypeStructure checked = SharedTyped.sharedTypeStructure
  { SharedTyped.constructorPatternFieldTypes = resolve
  , SharedTyped.forallTypeStructure = Just SharedTyped.sharedContextualForallTypeStructure
  }
 where
  schemas = checkedTermConstructorSchemas checked
  resolve name patternType = case
      [fields | (schemaName, schemaType, fields) <- schemas
              , schemaName == name
              , schemaType == patternType] of
    [] -> Nothing
    fields : remaining
      | all (== fields) remaining -> Just fields
      | otherwise -> Nothing

checkedTermConstructorSchemas
  :: CheckedTerm
  -> [(QualifiedName, HsType, [HsType])]
checkedTermConstructorSchemas (CheckedTerm _ form) = case form of
  CheckedLocal{} -> []
  CheckedGlobal{} -> []
  CheckedLambda _ _ body -> checkedTermConstructorSchemas body
  CheckedApply function argument ->
    checkedTermConstructorSchemas function
      ++ checkedTermConstructorSchemas argument
  CheckedVisibleTypeApplication _ _ _ _ function ->
    checkedTermConstructorSchemas function
  CheckedImplicitTypeApplication _ function -> checkedTermConstructorSchemas function
  CheckedForallIntroduction _ body -> checkedTermConstructorSchemas body
  CheckedContextIntroduction _ body -> checkedTermConstructorSchemas body
  CheckedContextApplication _ function -> checkedTermConstructorSchemas function
  CheckedTuple elements -> concatMap checkedTermConstructorSchemas elements
  CheckedLet _ _ binding body ->
    checkedTermConstructorSchemas binding
      ++ checkedTermConstructorSchemas body
  CheckedEmptyCase scrutinee -> checkedTermConstructorSchemas scrutinee
  CheckedConstructorMatch _ scrutinee alternatives ->
    checkedTermConstructorSchemas scrutinee
      ++ concatMap alternativeSchemas alternatives
 where
  alternativeSchemas
      (CheckedCaseAlternative name patternType fields body) =
    (name, patternType, map snd fields)
      : checkedTermConstructorSchemas body

checkedTermHasContext :: CheckedTerm -> Bool
-- Only explicit lexical evidence needs the context-aware association entrance.
-- Existing certificate-bearing visible applications retain their separately
-- checked source constraints and legacy result/obligation observations; their
-- contextual flag does not introduce a lexical dictionary node.
checkedTermHasContext (CheckedTerm _ form) = case form of
  CheckedLocal{} -> False
  CheckedGlobal{} -> False
  CheckedLambda _ _ body -> checkedTermHasContext body
  CheckedApply function argument -> checkedTermHasContext function || checkedTermHasContext argument
  CheckedVisibleTypeApplication _ _ _ _ function -> checkedTermHasContext function
  CheckedImplicitTypeApplication _ function -> checkedTermHasContext function
  CheckedForallIntroduction _ body -> checkedTermHasContext body
  CheckedContextIntroduction{} -> True
  CheckedContextApplication{} -> True
  CheckedTuple elements -> any checkedTermHasContext elements
  CheckedLet _ _ binding body -> checkedTermHasContext binding || checkedTermHasContext body
  CheckedEmptyCase scrutinee -> checkedTermHasContext scrutinee
  CheckedConstructorMatch _ scrutinee alternatives -> checkedTermHasContext scrutinee
    || any (\(CheckedCaseAlternative _ _ _ body) -> checkedTermHasContext body) alternatives

data TermGraphBuildState = TermGraphBuildState
  { termGraphBuildCandidateKey :: !Natural
  , termGraphBuildLimits :: !SharedTyped.TermGraphLimits
  , termGraphBuildNextNode :: !Natural
  , termGraphBuildNextOccurrence :: !Natural
  , termGraphBuildEdges :: !Natural
  , termGraphBuildPatterns :: !Natural
  , termGraphBuildEvidenceIntroductions :: !(Map.Map Natural SharedTyped.OccurrenceId)
  , termGraphBuildNodes ::
      [(SharedTyped.TermNodeId, SharedTyped.TermNode HsType TVarId)]
  }

type TermGraphBuild a = StateT TermGraphBuildState
  (Either ExferenceTermGraphAbsence) a

buildCheckedTermGraph
  :: Natural
  -> CheckedTerm
  -> Either ExferenceTermGraphAbsence
      (SharedTyped.TermGraphSource HsType TVarId)
buildCheckedTermGraph candidateKey checkedTerm = do
  (root, finalState) <- runStateT (buildCheckedTerm checkedTerm)
    TermGraphBuildState
      { termGraphBuildCandidateKey = candidateKey
      , termGraphBuildLimits = SharedTyped.defaultTermGraphLimits
      , termGraphBuildNextNode = 0
      , termGraphBuildNextOccurrence = 0
      , termGraphBuildEdges = 0
      , termGraphBuildPatterns = 0
      , termGraphBuildEvidenceIntroductions = Map.empty
      , termGraphBuildNodes = []
      }
  pure $ SharedTyped.TermGraphSource root $ termGraphBuildNodes finalState

buildCheckedTerm :: CheckedTerm -> TermGraphBuild SharedTyped.TermNodeId
buildCheckedTerm (CheckedTerm ty checkedForm) = do
  nodeId' <- allocateCheckedTermNode
  form <- case checkedForm of
    CheckedLocal variable -> SharedTyped.TypedLocal
      <$> allocateCheckedOccurrence <*> pure variable
    CheckedGlobal name _ -> SharedTyped.TypedGlobal
      <$> allocateCheckedOccurrence <*> pure name
    CheckedLambda variable annotation body -> do
      _ <- observeTermGraphCollection
        (SharedTyped.LambdaPatternList nodeId') [()]
      reserveTermGraphPatterns 1
      reserveTermGraphEdges 1
      occurrence <- allocateCheckedOccurrence
      bodyId <- buildCheckedTerm body
      let pattern' = SharedTyped.TypedPattern occurrence annotation
            $ if IntSet.member variable $ checkedTermLocalUses body
                then SharedTyped.TypedBind variable
                else SharedTyped.TypedWildcard
      pure $ SharedTyped.TypedLambda [pattern'] bodyId
    CheckedApply function argument -> do
      reserveTermGraphEdges 2
      functionId <- buildCheckedTerm function
      argumentId <- buildCheckedTerm argument
      witness <- case function of
        CheckedTerm (TypeArrow domain result) _ -> pure
          $ SharedTyped.ApplicationWitness domain result
        _ -> lift $ Left TermGraphEvidenceMismatch
      pure $ SharedTyped.TypedApply functionId argumentId witness
    CheckedVisibleTypeApplication argument selected contextualApplication
        annotation function -> do
      reserveTermGraphEdges 1
      occurrence <- allocateCheckedOccurrence
      functionId <- buildCheckedTerm function
      let source = case function of CheckedTerm functionType _ -> functionType
      case annotation of
        Nothing ->
          unless (SharedTypeAtom.isLeadingForallInstantiation source selected ty)
            $ lift $ Left $ if contextualApplication
                then UnsupportedContextualVisibleApplication source selected ty
                else TermGraphEvidenceMismatch
        Just _ -> pure ()
      pure $ SharedTyped.TypedVisibleTypeApplication
        occurrence functionId argument
        SharedTyped.TypeApplicationWitness
          { SharedTyped.typeApplicationSource = source
          , SharedTyped.typeApplicationSelected = selected
          , SharedTyped.typeApplicationResult = ty
          , SharedTyped.typeApplicationCertificate = fmap
              (\(identifier, slot) ->
                (SharedTyped.certificateId identifier, slot))
              annotation
          }
    CheckedImplicitTypeApplication selected function -> do
      reserveTermGraphEdges 1
      occurrence <- allocateCheckedOccurrence
      functionId <- buildCheckedTerm function
      let source = case function of CheckedTerm functionType _ -> functionType
      unless (SharedTypeAtom.isLeadingForallInstantiation source selected ty)
        $ lift $ Left TermGraphEvidenceMismatch
      pure $ SharedTyped.TypedImplicitTypeApplication occurrence functionId
        $ SharedTyped.ImplicitTypeApplicationWitness source selected ty
    CheckedForallIntroduction selected body -> do
      reserveTermGraphEdges 1
      occurrence <- allocateCheckedOccurrence
      bodyId <- buildCheckedTerm body
      let bodyType = case body of CheckedTerm childType _ -> childType
      pure $ SharedTyped.TypedForallIntroduction occurrence bodyId $
        SharedTyped.ForallIntroductionWitness ty selected bodyType
    CheckedContextIntroduction introduction body -> do
      reserveTermGraphEdges 1
      occurrence <- allocateCheckedOccurrence
      witness <- maybe (lift $ Left $ UnsupportedContextEvidence ty) pure $
        SharedTyped.contextIntroductionWitness SharedTyped.sharedContextTypeStructure ty
      _ <- observeTermGraphCollection (SharedTyped.ContextConstraintList nodeId') $
        SharedTyped.contextIntroductionConstraints witness
      outerIntroductions <- gets termGraphBuildEvidenceIntroductions
      when (Map.member introduction outerIntroductions) $
        lift $ Left TermGraphEvidenceMismatch
      modify' $ \current -> current
        { termGraphBuildEvidenceIntroductions = Map.insert introduction occurrence outerIntroductions }
      bodyId <- buildCheckedTerm body
      modify' $ \current -> current
        { termGraphBuildEvidenceIntroductions = outerIntroductions }
      pure $ SharedTyped.TypedContextIntroduction occurrence bodyId witness
    CheckedContextApplication givens function -> do
      reserveTermGraphEdges 1
      occurrence <- allocateCheckedOccurrence
      let source = case function of CheckedTerm functionType _ -> functionType
      sourceWitness <- maybe (lift $ Left $ UnsupportedContextEvidence source) pure $
        SharedTyped.contextIntroductionWitness SharedTyped.sharedContextTypeStructure source
      let constraints = SharedTyped.contextIntroductionConstraints sourceWitness
      _ <- observeTermGraphCollection (SharedTyped.ContextConstraintList nodeId') constraints
      evidence <- mapM (resolveCheckedGiven source givens) constraints
      witness <- maybe (lift $ Left $ UnsupportedContextEvidence source) pure $
        SharedTyped.contextApplicationWitness SharedTyped.sharedContextTypeStructure source evidence
      functionId <- buildCheckedTerm function
      pure $ SharedTyped.TypedContextApplication occurrence functionId witness
    CheckedTuple elements -> do
      elementCount <- observeTermGraphCollection
        (SharedTyped.TupleElementList nodeId') elements
      reserveTermGraphEdges elementCount
      SharedTyped.TypedTuple <$> mapM buildCheckedTerm elements
    CheckedLet variable annotation binding body -> do
      reserveTermGraphPatterns 1
      reserveTermGraphEdges 2
      occurrence <- allocateCheckedOccurrence
      bindingId <- buildCheckedTerm binding
      bodyId <- buildCheckedTerm body
      let pattern' = SharedTyped.TypedPattern occurrence annotation
            $ if IntSet.member variable $ checkedTermLocalUses body
                then SharedTyped.TypedBind variable
                else SharedTyped.TypedWildcard
      pure $ SharedTyped.TypedLet pattern' bindingId bodyId
    CheckedEmptyCase scrutinee -> do
      _ <- observeTermGraphCollection
        (SharedTyped.CaseAlternativeList nodeId') ([] :: [()])
      reserveTermGraphEdges 1
      scrutineeId <- buildCheckedTerm scrutinee
      pure $ SharedTyped.TypedCase scrutineeId []
    CheckedConstructorMatch syntax scrutinee alternatives -> do
      alternativeCount <- observeTermGraphCollection
        (SharedTyped.CaseAlternativeList nodeId') alternatives
      reserveTermGraphEdges $ 1 + alternativeCount
      scrutineeId <- buildCheckedTerm scrutinee
      checkedAlternatives <- mapM buildCheckedCaseAlternative alternatives
      case (syntax, checkedAlternatives) of
        (CheckedCaseExpression, _) ->
          pure $ SharedTyped.TypedCase scrutineeId checkedAlternatives
        (CheckedPatternBinding, [(pattern', bodyId)]) ->
          pure $ SharedTyped.TypedLet pattern' scrutineeId bodyId
        _ -> lift $ Left TermGraphEvidenceMismatch
  modify' $ \current -> current
    { termGraphBuildNodes =
        (nodeId', SharedTyped.TermNode ty form)
          : termGraphBuildNodes current
    }
  pure nodeId'

buildCheckedCaseAlternative
  :: CheckedCaseAlternative
  -> TermGraphBuild
      (SharedTyped.TypedPattern HsType TVarId, SharedTyped.TermNodeId)
buildCheckedCaseAlternative
    (CheckedCaseAlternative constructor patternType fields body) = do
  occurrence <- allocateCheckedOccurrence
  fieldCount <- observeTermGraphCollection
    (SharedTyped.ConstructorPatternFieldList occurrence) fields
  reserveTermGraphPatterns $ 1 + fieldCount
  let used = checkedTermLocalUses body
  checkedFields <- forM fields $ \(variable, fieldType) -> do
    fieldOccurrence <- allocateCheckedOccurrence
    pure $ SharedTyped.TypedPattern fieldOccurrence fieldType
      $ if IntSet.member variable used
          then SharedTyped.TypedBind variable
          else SharedTyped.TypedWildcard
  bodyId <- buildCheckedTerm body
  pure
    ( SharedTyped.TypedPattern occurrence patternType
        $ SharedTyped.TypedConstructor constructor checkedFields
    , bodyId
    )

-- Resolve after the complete check has zonked both the obligation and its
-- lexical snapshot. Successful instance/superclass resolution in the legacy
-- checker cannot enter this rule: only an exact original given is considered.
-- Equal dictionaries retain distinct slots; source order selects one stable
-- identity, which the shared sealer then checks against the actual scope.
resolveCheckedGiven :: HsType -> [CheckedGiven] -> HsConstraint
  -> TermGraphBuild SharedTyped.ContextEvidence
resolveCheckedGiven source givens required = case
    [(introduction, slot)
    | CheckedGiven introduction slot actual <- givens
    , sameConstraint required actual] of
  [] -> lift $ Left $ UnsupportedContextEvidence source
  (introduction, slot) : _ -> do
    active <- gets termGraphBuildEvidenceIntroductions
    occurrence <- maybe (lift $ Left $ UnsupportedContextEvidence source) pure $
      Map.lookup introduction active
    pure $ SharedTyped.givenContextEvidence occurrence slot
 where
  sameConstraint (HsConstraint left leftArguments) (HsConstraint right rightArguments) =
    left == right && length leftArguments == length rightArguments
      && and (zipWith SharedTypeAtom.alphaEquivalentTypes leftArguments rightArguments)

checkedTermLocalUses :: CheckedTerm -> IntSet.IntSet
checkedTermLocalUses (CheckedTerm _ form) = case form of
  CheckedLocal variable -> IntSet.singleton variable
  CheckedGlobal{} -> IntSet.empty
  CheckedLambda variable _ body ->
    IntSet.delete variable $ checkedTermLocalUses body
  CheckedApply function argument ->
    checkedTermLocalUses function `IntSet.union` checkedTermLocalUses argument
  CheckedVisibleTypeApplication _ _ _ _ function ->
    checkedTermLocalUses function
  CheckedImplicitTypeApplication _ function -> checkedTermLocalUses function
  CheckedForallIntroduction _ body -> checkedTermLocalUses body
  CheckedContextIntroduction _ body -> checkedTermLocalUses body
  CheckedContextApplication _ function -> checkedTermLocalUses function
  CheckedTuple elements -> IntSet.unions $ map checkedTermLocalUses elements
  CheckedLet variable _ binding body ->
    checkedTermLocalUses binding `IntSet.union`
      IntSet.delete variable (checkedTermLocalUses body)
  CheckedEmptyCase scrutinee -> checkedTermLocalUses scrutinee
  CheckedConstructorMatch _ scrutinee alternatives -> IntSet.unions
    $ checkedTermLocalUses scrutinee
    : [ foldr IntSet.delete (checkedTermLocalUses body)
          $ map fst fields
      | CheckedCaseAlternative _ _ fields body <- alternatives]

allocateCheckedTermNode :: TermGraphBuild SharedTyped.TermNodeId
allocateCheckedTermNode = do
  candidateKey <- gets termGraphBuildCandidateKey
  next <- gets termGraphBuildNextNode
  maximumNodes <- gets
    $ fromIntegral
    . SharedTyped.maximumTermGraphNodes
    . termGraphBuildLimits
  when (next >= maximumNodes) $ constructionLimit
    $ TermGraphConstructionNodeLimitExceeded maximumNodes (next + 1)
  modify' $ \current -> current {termGraphBuildNextNode = next + 1}
  pure $ SharedTyped.termNodeId $ pairNatural candidateKey next

allocateCheckedOccurrence :: TermGraphBuild SharedTyped.OccurrenceId
allocateCheckedOccurrence = do
  candidateKey <- gets termGraphBuildCandidateKey
  next <- gets termGraphBuildNextOccurrence
  maximumOccurrences <- gets $ \current ->
    let limits = termGraphBuildLimits current
    in fromIntegral (SharedTyped.maximumTermGraphNodes limits)
      + fromIntegral (SharedTyped.maximumTermGraphPatternNodes limits)
  when (next >= maximumOccurrences) $ constructionLimit
    $ TermGraphConstructionOccurrenceLimitExceeded
        maximumOccurrences (next + 1)
  modify' $ \current -> current
    {termGraphBuildNextOccurrence = next + 1}
  pure $ SharedTyped.occurrenceId $ pairNatural candidateKey next

reserveTermGraphEdges :: Natural -> TermGraphBuild ()
reserveTermGraphEdges added = do
  current <- gets termGraphBuildEdges
  maximumEdges <- gets
    $ fromIntegral
    . SharedTyped.maximumTermGraphEdges
    . termGraphBuildLimits
  let observed = current + added
  when (observed > maximumEdges) $ constructionLimit
    $ TermGraphConstructionEdgeLimitExceeded maximumEdges observed
  modify' $ \state -> state {termGraphBuildEdges = observed}

reserveTermGraphPatterns :: Natural -> TermGraphBuild ()
reserveTermGraphPatterns added = do
  current <- gets termGraphBuildPatterns
  maximumPatterns <- gets
    $ fromIntegral
    . SharedTyped.maximumTermGraphPatternNodes
    . termGraphBuildLimits
  let observed = current + added
  when (observed > maximumPatterns) $ constructionLimit
    $ TermGraphConstructionPatternLimitExceeded maximumPatterns observed
  modify' $ \state -> state {termGraphBuildPatterns = observed}

observeTermGraphCollection
  :: SharedTyped.GraphCollectionSite
  -> [value]
  -> TermGraphBuild Natural
observeTermGraphCollection site values = do
  maximumWidth <- gets
    $ SharedTyped.maximumTermGraphCollectionWidth
    . termGraphBuildLimits
  let observed = SharedCollection.observedListLength maximumWidth values
  when (observed > maximumWidth) $ constructionLimit
    $ TermGraphConstructionCollectionLimitExceeded
        site maximumWidth observed
  pure $ fromIntegral observed

constructionLimit
  :: ExferenceTermGraphConstructionLimit
  -> TermGraphBuild value
constructionLimit = lift . Left . TermGraphConstructionLimit

-- Cantor pairing gives globally disjoint deterministic ranges without an
-- Int-sized allocation or a backend-global mutable identity supply.
pairNatural :: Natural -> Natural -> Natural
pairNatural left right = ((sum' * (sum' + 1)) `div` 2) + right
 where
  sum' = left + right

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
  validateExpressionPatternArities classEnvironment
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
    (ExpressionCheckContext _ _ classEnvironment _ _ _ constructorArities _)
    expected expression = do
  mapM_ (validateCheckConstraint classEnvironment QueryConstraint) expected
  validateExpressionPatternArities
    classEnvironment constructorArities expression
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
  :: QueryClassEnv
  -> Map.Map QualifiedName Int
  -> Expression
  -> Either ExpressionCheckError ()
validateExpressionPatternArities classEnvironment constructorArities =
  inspectExpression . toGeneratedExpression
 where
  inspectExpression generated = case generated of
    SharedGenerated.Local{} -> Right ()
    SharedGenerated.Global{} -> Right ()
    SharedGenerated.Lambda patterns body ->
      mapM_ inspectPattern patterns >> inspectExpression body
    SharedGenerated.Apply function argument ->
      inspectExpression function >> inspectExpression argument
    SharedGenerated.VisibleTypeApplication function argument ->
      inspectExpression function >> inspectVisibleTypeArgument argument
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

  -- Explicit type applications are raw candidate input just like local
  -- annotations. Reconstruct the closed argument in a checker-local binder
  -- namespace, then validate both its type structure and every contextual
  -- class occurrence before expression checking can consume it.
  inspectVisibleTypeArgument argument = case
      SharedGenerated.visibleTypeArgumentPatternType argument of
    Nothing -> Right ()
    Just patternType -> do
      binders <- maybe (Left FlexibleIdentifierSupplyExhausted) Right $
        sequence $ SharedType.typeBinderVariables patternType
      let
          renaming = Map.fromList $ zip binders
            $ map SharedType.FlexibleVariable [0 ..]
          occurrence Nothing = Just $ SharedType.FlexibleVariable $ length binders
          occurrence (Just variable) = Map.lookup variable renaming
      translated <- maybe
        (Left FlexibleIdentifierSupplyExhausted)
        Right
        $ traverse occurrence patternType
      validateCheckType classEnvironment QueryConstraint translated
      mapM_ validateDeclaredClass $ typeConstraints translated

  -- Query signatures deliberately permit nominal constraints from a partial
  -- external class inventory. A specified visible argument is executable
  -- evidence, however, so its embedded classes must be present in the sealed
  -- checker environment rather than passing that compatibility policy.
  validateDeclaredClass constraint = case validateConstraintInEnv
      (qClassEnv_env classEnvironment) QueryConstraint constraint of
    Left failure -> Left $ InvalidCheckClassConstraint failure
    Right () -> Right ()

  inspectPattern pattern = case pattern of
    SharedGenerated.Bind{} -> Right ()
    SharedGenerated.Wildcard -> Right ()
    SharedGenerated.Constructor name arguments -> do
      expected <- maybe (Left $ UnknownConstructor name) Right
        $ case SharedName.nameSpecial name of
          Just (SharedName.TupleConstructor _ arity) -> Just arity
          _ -> Map.lookup name constructorArities
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
withLocalGivens :: Natural -> [HsConstraint] -> Check a -> Check a
withLocalGivens introduction givens action = do
  outerGivens <- gets checkLocalGivens
  outerEvidence <- gets checkEvidenceGivens
  modify' $ \current -> current
    { checkLocalGivens = outerGivens ++ givens
    , checkEvidenceGivens = outerEvidence ++
        [CheckedGiven introduction slot constraint
        | (slot, constraint) <- zip [0 ..] givens]
    }
  result <- action
  modify' $ \current -> current
    { checkLocalGivens = outerGivens, checkEvidenceGivens = outerEvidence }
  pure result

-- Root and nested telescopes use the same lexical bracket. The root class
-- environment still supplies the historical checker acceptance policy; these
-- frames add source identities without reconstructing them from its sets.
checkOpenedTelescope :: [CheckedOpening] -> Check CheckedTermResult
  -> Check CheckedTermResult
checkOpenedTelescope [] action = action
checkOpenedTelescope (opening : remaining) action = case opening of
  CheckedOpenForall source selected ->
    unaryCheckedTerm source (CheckedForallIntroduction selected)
      <$> checkOpenedTelescope remaining action
  CheckedOpenContext source -> case source of
    TypeForallNative [] constraints@(_ : _) _ -> do
      introduction <- gets checkNextContextIntroduction
      modify' $ \current -> current
        { checkNextContextIntroduction = introduction + 1 }
      checked <- withLocalGivens introduction constraints $
        checkOpenedTelescope remaining action
      pure $ unaryCheckedTerm source (CheckedContextIntroduction introduction) checked
    _ -> do
      checked <- checkOpenedTelescope remaining action
      pure $ unavailableCheckedTerm (checkedResultType checked) $
        UnsupportedContextEvidence source

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

-- Run after ordinary expression/result inference, while retaining each use's
-- lexical snapshot. Fresh provider variables are the only assignable ones;
-- the unifier's existing rigid-scope validation still checks every selection.
-- A newly determined shared parameter can resolve another provider group, so
-- repeat only while the substitution grows. No dictionary evidence is added
-- here: final constraint solving and graph sealing independently discharge it.
inferLexicalProviderSelections :: Check ()
inferLexicalProviderSelections = do
  before <- gets checkSubstitutions
  groups <- gets checkGivenInference
  mapM_ inferGroup groups
  after <- gets checkSubstitutions
  unless (before == after) inferLexicalProviderSelections
 where
  inferGroup (fresh, obligations) = do
    substitutions <- gets checkSubstitutions
    let eligible = fresh `IntSet.difference` IntMap.keysSet substitutions
    unless (IntSet.null eligible) $ do
      normalized <- mapM normalize obligations
      case uniqueGivenInstantiation 4096 eligible normalized of
        Nothing -> pure ()
        Just selected -> mapM_
          (\(variable, image) -> unifyTypes (TypeVar variable) image) $
            IntMap.toAscList selected

  normalize (ScopedConstraint givens required) = (,)
    <$> mapM (traverse zonk) givens <*> traverse zonk required

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
  -> Either ExpressionCheckError (HsType, [HsConstraint], [CheckedOpening])
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
        (instantiated, deeper, deeperOpenings) = instantiateFrom rest
          $ snd $ applySubsts substitutions body
        openings = checkedLayerOpenings
          (TypeForall variables constraints body) current
    in (instantiated, layerConstraints ++ deeper, openings ++ deeperOpenings)
  instantiateFrom _ instantiated = (instantiated, [], [])

-- Replace occurrences owned by this forall layer while respecting a nested
-- layer that deliberately shadows the same nominal binder. Both expression
-- checking and root telescope reconstruction use this scoped operation.
substituteScopedVariable :: SynthesisVariable -> HsType -> HsType -> HsType
substituteScopedVariable binder replacement typeExpression =
  case typeExpression of
    SharedType.TypeVariable variable
      | variable == binder -> replacement
      | otherwise -> typeExpression
    SharedType.TypeConstructor{} -> typeExpression
    SharedType.TypeApplication typeFunction typeArgument ->
      SharedType.TypeApplication
        (substituteScopedVariable binder replacement typeFunction)
        (substituteScopedVariable binder replacement typeArgument)
    SharedType.FunctionType parameter result -> SharedType.FunctionType
      (substituteScopedVariable binder replacement parameter)
      (substituteScopedVariable binder replacement result)
    SharedType.TupleType boxity elements -> SharedType.TupleType boxity
      $ map (substituteScopedVariable binder replacement) elements
    SharedType.ForallType binders nestedContexts nestedBody
      | binder `elem` binders -> typeExpression
      | otherwise -> SharedType.ForallType binders
          (map (fmap $ substituteScopedVariable binder replacement) nestedContexts)
          (substituteScopedVariable binder replacement nestedBody)

-- Retain precisely one original layer. In particular, a context outside a
-- subsequent forall is never merged into the later binder's scope. The caller
-- retains the established simultaneous substitution for ordinary checking;
-- graph sealing independently verifies these finer one-binder witnesses.
checkedLayerOpenings :: HsType -> [(TVarId, TVarId)] -> [CheckedOpening]
checkedLayerOpenings source instantiations = case (source, instantiations) of
  (TypeForallNative (binder : remainingBinders) constraints body,
      (_, rigid) : remaining) ->
    let selected = TypeConstant rigid
        remainder = if null remainingBinders && null constraints then body
          else TypeForallNative remainingBinders constraints body
        opened = SharedType.canonicalizeType $
          substituteScopedVariable binder selected remainder
        innerOpenings
          | null remaining =
              [CheckedOpenContext opened | null remainingBinders, not $ null constraints]
          | otherwise = checkedLayerOpenings opened remaining
    in CheckedOpenForall source selected : innerOpenings
  (TypeForallNative [] (_ : _) _, []) -> [CheckedOpenContext source]
  _ -> []

expressionFlexibleIdentifiers :: Expression -> IntSet.IntSet
expressionFlexibleIdentifiers =
  foldMap (flexibleIdentifiers . snd) . expressionTypedLocals
