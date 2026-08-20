-- | Structural utilities over Exference's 'HsType': flexible-variable
-- renumbering and bounds, forall quantification and alpha-normalization,
-- arrow-chain splitting, and forall/head queries.  Many are thin
-- compatibility names over "Language.Haskell.Synthesis.Type"; the class-
-- environment names in the export list are re-exported from
-- "Language.Haskell.Exference.Core.Types" for historical callers.
module Language.Haskell.Exference.Core.TypeUtils
  ( incVarIds
  , maximumFlexibleId
  , largestId
  , forallify
  , ConstraintSite (..)
  , ClassEnvError (..)
  , mkStaticClassEnv
  , validateConstraintInEnv
  , validateKnownConstraintInEnv
  , constraintContainsVariables
  , inflateInstances
  , ForallNormalizationError (..)
  , alphaNormalizeForalls
  , splitArrowChain
  , splitArrowResultParams
  , containsForall
  , containsNestedForall
  , constraintContainsForall
  , typeConstructorHead
  )
where



import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.IntSet as IntSet

import Language.Haskell.Exference.Core.Internal.FlexibleIds
  ( flexibleIdentifiers )
import Language.Haskell.Exference.Core.Internal.VariableSupply
  ( allocateFreshIdentifier
  , supplyFromIdentifierSet
  )
import Language.Haskell.Exference.Core.Types
import qualified Language.Haskell.Synthesis.Type as SharedType



-- | Bind every free flexible variable at the outermost forall while leaving
-- rigid skolems free. This compatibility name preserves Exference's canonical
-- ascending integer binder order.
forallify :: HsType -> HsType
forallify = SharedType.quantifyFreeVariables SharedType.isFlexibleVariable

-- | Transform the complete flexible namespace, including forall binders and
-- constraints, without changing rigid search constants. The shared functor
-- also keeps structural tuple elements in the traversal automatically.
incVarIds :: (TVarId -> TVarId) -> HsType -> HsType
incVarIds transform = fmap $ SharedType.mapFlexibleVariable transform

-- | The actual greatest flexible ID, including forall binders and context
-- arguments.  'Nothing' represents a ground type without stealing an 'Int'
-- value from the public identity domain.
maximumFlexibleId :: HsType -> Maybe TVarId
maximumFlexibleId typeExpression
  | IntSet.null identifiers = Nothing
  | otherwise = Just $ IntSet.findMax identifiers
 where
  identifiers = flexibleIdentifiers typeExpression

-- | Compatibility projection for callers that historically used @-1@ as a
-- ground sentinel.  It now returns the true maximum when variables exist,
-- including a negative-only domain; use 'maximumFlexibleId' to distinguish a
-- ground type from a real @TypeVar (-1)@.
largestId :: HsType -> TVarId
largestId = fromMaybe (-1) . maximumFlexibleId
{-# DEPRECATED largestId "Use maximumFlexibleId; every Int is a valid TVarId." #-}

-- | Whether any argument of a constraint has a free flexible variable (see
-- 'freeVars'); rigid constants do not count.  The constraint solver defers
-- such constraints instead of resolving them against instances.
constraintContainsVariables :: HsConstraint -> Bool
constraintContainsVariables =
  any (not . S.null . freeVars) . constraint_params

-- | Why a lexical forall namespace could not be alpha-normalized.
data ForallNormalizationError
  = DuplicateForallBinder TVarId
  | RigidForallBinderCannotBeNormalized TVarId
  | ForallNormalizationSupplyExhausted
  deriving (Eq, Show)

-- | Give every explicit forall binder a globally distinct flexible identity
-- while respecting lexical shadowing. External IDs and the type's true free
-- variables are claimed before traversal. Every ID occurring anywhere in the
-- source is reserved up front, so a fresh binder cannot capture a later free,
-- bound, or constraint occurrence. A rigid binder is not an inference
-- variable and is rejected explicitly rather than being retagged as one.
--
-- The returned set is the complete final namespace. Parser adapters must
-- reserve it exactly because alpha-renamed binders do not have an unambiguous
-- source spelling to add to a 'TypeVarIndex'; a greatest ID of 'maxBound' still
-- leaves genuine gaps available elsewhere in the finite domain.
alphaNormalizeForalls
  :: IntSet.IntSet
  -> HsType
  -> Either ForallNormalizationError (HsType, IntSet.IntSet)
alphaNormalizeForalls externalVariables source = case
    SharedType.uniquifyTypeBinders rejectRigidBinder freshFlexibleBinder
      protectedVariables source of
  Left failure -> Left $ normalizationError failure
  Right (normalized, reserved) -> Right
    (normalized, flexibleIdentifierSet reserved)
 where
  protectedVariables = S.fromList
    [ SharedType.FlexibleVariable identifier
    | identifier <- IntSet.toAscList externalVariables
    ]

  rejectRigidBinder = SharedType.rigidVariableIdentity

  -- Preserve the historical greatest-plus-one path in logarithmic time.
  -- Only the maxBound boundary needs the complete IntSet gap search. Rigid
  -- occurrences deliberately do not occupy flexible IDs.
  freshFlexibleBinder reserved _ = SharedType.FlexibleVariable <$> case
      greatestFlexibleIdentifier reserved of
    Nothing -> Just 0
    Just greatest
      | greatest < maxBound -> Just $ greatest + 1
      | otherwise -> fst <$> allocateFreshIdentifier
          (supplyFromIdentifierSet $ flexibleIdentifierSet reserved)

  normalizationError failure = case failure of
    SharedType.RejectedTypeBinder identifier ->
      RigidForallBinderCannotBeNormalized identifier
    SharedType.DuplicateTypeBinder variable -> case variable of
      SharedType.FlexibleVariable identifier -> DuplicateForallBinder identifier
      SharedType.RigidVariable identifier ->
        RigidForallBinderCannotBeNormalized identifier
    SharedType.TypeBinderFresheningError{} ->
      ForallNormalizationSupplyExhausted

flexibleIdentifierSet :: S.Set SynthesisVariable -> IntSet.IntSet
flexibleIdentifierSet variables = IntSet.fromList
  [ identifier
  | SharedType.FlexibleVariable identifier <- S.toAscList variables
  ]

greatestFlexibleIdentifier :: S.Set SynthesisVariable -> Maybe TVarId
greatestFlexibleIdentifier variables = case
    S.lookupLE (SharedType.FlexibleVariable maxBound) variables of
  Just (SharedType.FlexibleVariable identifier) -> Just identifier
  _ -> Nothing

flexibleBinderIdentifiers
  :: [SynthesisVariable]
  -> Either TVarId [TVarId]
flexibleBinderIdentifiers = traverse flexibleIdentifier
 where
  flexibleIdentifier variable = maybe
    (Left $ SharedType.variableIdentity variable)
    Right
    $ SharedType.flexibleVariableIdentity variable

-- | Normalize the complete type, peel only its leading prenex chain, then
-- split consecutive arrows. A forall reached after an arrow remains in the
-- result. Malformed binder lists or namespace exhaustion retain the entire
-- source type, ensuring checked callers fail closed instead of flattening it.
splitArrowResultParams :: HsType -> (HsType, [HsType], [TVarId], [HsConstraint])
splitArrowResultParams source = case alphaNormalizeForalls IntSet.empty source of
  Left _ -> (source, [], [], [])
  Right (normalized, _) ->
    let (nativeVariables, constraints, body) =
          SharedType.splitLeadingForalls normalized
    in case flexibleBinderIdentifiers nativeVariables of
      Left _ -> (source, [], [], [])
      Right variables ->
        let (result, parameters) = splitArrowChain body
        in (result, parameters, variables, constraints)


-- | Split the consecutive outer arrow chain without inspecting or rewriting
-- its component types. In particular, a forall below an arrow remains the
-- result rather than being opened. This is the total monotype decomposition
-- used when substitutions expose more function parameters during search.
splitArrowChain :: HsType -> (HsType, [HsType])
splitArrowChain source =
  let (parameters, result) = SharedType.functionSpine source
  in (result, parameters)

-- | Whether a type contains explicit quantification at any depth.
containsForall :: HsType -> Bool
containsForall = SharedType.containsForall

-- | Whether quantification occurs below the complete leading prenex chain or
-- inside an outer constraint argument.
containsNestedForall :: HsType -> Bool
containsNestedForall = SharedType.containsNestedForall

-- | Whether explicit quantification occurs at any depth inside any argument
-- of a constraint; the per-argument test is 'containsForall'.
constraintContainsForall :: HsConstraint -> Bool
constraintContainsForall = SharedType.constraintContainsForall

-- | Find the nominal head beneath foralls and type applications.
typeConstructorHead :: HsType -> Maybe QualifiedName
typeConstructorHead = SharedType.typeConstructorHead
