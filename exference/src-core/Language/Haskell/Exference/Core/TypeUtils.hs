{-# LANGUAGE PatternGuards #-}

module Language.Haskell.Exference.Core.TypeUtils
  ( incVarIds
  , maximumFlexibleId
  , maximumSubstitutionFlexibleId
  , largestId
  , largestSubstsId
  , forallify -- unused atm
  , ConstraintSite (..)
  , ClassEnvError (..)
  , mkStaticClassEnv
  , validateConstraintInEnv
  , validateKnownConstraintInEnv
  , constraintMapTypes
  , constraintContainsVariables
  , inflateInstances
  , ForallNormalizationError (..)
  , alphaNormalizeForalls
  , splitArrowResultParams
  , containsForall
  , containsNestedForall
  , typeConstructorHead
  )
where



import qualified Data.Set as S
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import Language.Haskell.Exference.Core.Internal.FlexibleIds
  ( IdentifierSupply
  , allocateFreshIdentifier
  , flexibleIdentifiers
  , supplyFromIdentifiers
  )
import Language.Haskell.Exference.Core.Types



-- binds everything in Foralls, so there are no free variables anymore.
forallify :: HsType -> HsType
forallify t = case t of
  TypeForall is cs t' -> TypeForall (S.toList frees++is) cs t'
  _                   -> TypeForall (S.toList frees) [] t
 where frees = freeVars t

incVarIds :: (TVarId -> TVarId) -> HsType -> HsType
incVarIds f (TypeVar i) = TypeVar (f i)
incVarIds f (TypeArrow t1 t2) = TypeArrow (incVarIds f t1) (incVarIds f t2)
incVarIds f (TypeApp t1 t2) = TypeApp (incVarIds f t1) (incVarIds f t2)
incVarIds f (TypeForall is cs t) = TypeForall
                                     (f <$> is)
                                     (g <$> cs) 
                                     (incVarIds f t)
  where
    g (HsConstraint cls params) = HsConstraint cls (incVarIds f <$> params)
incVarIds _ t = t

-- | The actual greatest flexible ID, including forall binders and context
-- arguments.  'Nothing' represents a ground type without stealing an 'Int'
-- value from the public identity domain.
maximumFlexibleId :: HsType -> Maybe TVarId
maximumFlexibleId typeExpression
  | IntSet.null identifiers = Nothing
  | otherwise = Just $ IntSet.findMax identifiers
 where
  identifiers = flexibleIdentifiers typeExpression

-- | The greatest flexible ID occurring in a substitution range.
maximumSubstitutionFlexibleId :: Substs -> Maybe TVarId
maximumSubstitutionFlexibleId = IntMap.foldl' combine Nothing
 where
  combine current typeExpression = case
      (current, maximumFlexibleId typeExpression) of
    (Nothing, result) -> result
    (result, Nothing) -> result
    (Just left, Just right) -> Just $ max left right

-- | Compatibility projection for callers that historically used @-1@ as a
-- ground sentinel.  It now returns the true maximum when variables exist,
-- including a negative-only domain; use 'maximumFlexibleId' to distinguish a
-- ground type from a real @TypeVar (-1)@.
largestId :: HsType -> TVarId
largestId = maybe (-1) id . maximumFlexibleId
{-# DEPRECATED largestId "Use maximumFlexibleId; every Int is a valid TVarId." #-}

-- | Compatibility projection retaining the historical empty-map sentinel.
largestSubstsId :: Substs -> TVarId
largestSubstsId = maybe 0 id . maximumSubstitutionFlexibleId
{-# DEPRECATED largestSubstsId
  "Use maximumSubstitutionFlexibleId; every Int is a valid TVarId." #-}

constraintMapTypes :: (HsType -> HsType) -> HsConstraint -> HsConstraint
constraintMapTypes f (HsConstraint a ts) = HsConstraint a (map f ts)

constraintContainsVariables :: HsConstraint -> Bool
constraintContainsVariables =
  any (not . S.null . freeVars) . constraint_params

-- | Why a lexical forall namespace could not be alpha-normalized.
data ForallNormalizationError
  = DuplicateForallBinder TVarId
  | ForallNormalizationSupplyExhausted
  deriving (Eq, Show)

data ForallNormalizationState = ForallNormalizationState
  { normalizationClaimed :: IntSet.IntSet
  , normalizationReserved :: IntSet.IntSet
  , normalizationSupply :: IdentifierSupply
  }

-- | Give every explicit forall binder a globally distinct flexible identity
-- while respecting lexical shadowing. External IDs and the type's true free
-- variables are claimed before traversal. Every ID occurring anywhere in the
-- source is reserved up front, so a fresh binder cannot capture a later free,
-- bound, or constraint occurrence.
--
-- The returned set is the complete final namespace. Parser adapters must
-- reserve it exactly because alpha-renamed binders do not have an unambiguous
-- source spelling to add to a 'TypeVarIndex'; a greatest ID of 'maxBound' still
-- leaves genuine gaps available elsewhere in the finite domain.
alphaNormalizeForalls
  :: IntSet.IntSet
  -> HsType
  -> Either ForallNormalizationError (HsType, IntSet.IntSet)
alphaNormalizeForalls externalVariables source = do
  (normalized, finalState) <- normalizeForallsInType initialState source
  pure (normalized, normalizationReserved finalState)
 where
  sourceVariables = flexibleIdentifiers source
  reserved = externalVariables `IntSet.union` sourceVariables
  claimed = externalVariables `IntSet.union`
    IntSet.fromList (S.toAscList $ freeVars source)
  initialState = ForallNormalizationState
    { normalizationClaimed = claimed
    , normalizationReserved = reserved
    , normalizationSupply = supplyFromIdentifiers $ IntSet.toAscList reserved
    }

normalizeForallsInType
  :: ForallNormalizationState
  -> HsType
  -> Either ForallNormalizationError (HsType, ForallNormalizationState)
normalizeForallsInType state typeExpression = case typeExpression of
  TypeVar{} -> pure (typeExpression, state)
  TypeConstant{} -> pure (typeExpression, state)
  TypeCons{} -> pure (typeExpression, state)
  TypeArrow parameter result -> do
    (normalizedParameter, parameterState) <-
      normalizeForallsInType state parameter
    (normalizedResult, resultState) <-
      normalizeForallsInType parameterState result
    pure (TypeArrow normalizedParameter normalizedResult, resultState)
  TypeApp function argument -> do
    (normalizedFunction, functionState) <-
      normalizeForallsInType state function
    (normalizedArgument, argumentState) <-
      normalizeForallsInType functionState argument
    pure (TypeApp normalizedFunction normalizedArgument, argumentState)
  TypeForall variables constraints body -> do
    case firstDuplicateVariable variables of
      Just duplicate -> Left $ DuplicateForallBinder duplicate
      Nothing -> pure ()
    (normalizedVariables, renaming, binderState) <-
      normalizeForallBinders state variables
    let substitutions = IntMap.map TypeVar renaming
        renamedConstraints = map
          (snd . constraintApplySubsts substitutions) constraints
        renamedBody = snd $ applySubsts substitutions body
    (normalizedConstraints, constraintState) <-
      normalizeForallConstraints binderState renamedConstraints
    (normalizedBody, bodyState) <-
      normalizeForallsInType constraintState renamedBody
    pure
      ( TypeForall normalizedVariables normalizedConstraints normalizedBody
      , bodyState
      )

normalizeForallConstraints
  :: ForallNormalizationState
  -> [HsConstraint]
  -> Either ForallNormalizationError
      ([HsConstraint], ForallNormalizationState)
normalizeForallConstraints state [] = pure ([], state)
normalizeForallConstraints state (HsConstraint name parameters : remaining) = do
  (normalizedParameters, parameterState) <-
    normalizeForallTypes state parameters
  (normalizedRemaining, finalState) <-
    normalizeForallConstraints parameterState remaining
  pure
    (HsConstraint name normalizedParameters : normalizedRemaining, finalState)

normalizeForallTypes
  :: ForallNormalizationState
  -> [HsType]
  -> Either ForallNormalizationError ([HsType], ForallNormalizationState)
normalizeForallTypes state [] = pure ([], state)
normalizeForallTypes state (typeExpression : remaining) = do
  (normalizedType, typeState) <- normalizeForallsInType state typeExpression
  (normalizedRemaining, finalState) <-
    normalizeForallTypes typeState remaining
  pure (normalizedType : normalizedRemaining, finalState)

normalizeForallBinders
  :: ForallNormalizationState
  -> [TVarId]
  -> Either ForallNormalizationError
      ([TVarId], IntMap.IntMap TVarId, ForallNormalizationState)
normalizeForallBinders initialState = go initialState [] IntMap.empty
 where
  go state reversed renaming [] =
    pure (reverse reversed, renaming, state)
  go state reversed renaming (variable : remaining)
    | IntSet.notMember variable $ normalizationClaimed state =
        go (claim variable state) (variable : reversed) renaming remaining
    | otherwise = case allocateFreshIdentifier $ normalizationSupply state of
        Nothing -> Left ForallNormalizationSupplyExhausted
        Just (replacement, nextSupply) -> go
          ( (claim replacement state)
              {normalizationSupply = nextSupply}
          )
          (replacement : reversed)
          (IntMap.insert variable replacement renaming)
          remaining

  claim variable state = state
    { normalizationClaimed = IntSet.insert variable
        $ normalizationClaimed state
    , normalizationReserved = IntSet.insert variable
        $ normalizationReserved state
    }

firstDuplicateVariable :: [TVarId] -> Maybe TVarId
firstDuplicateVariable = go IntSet.empty
 where
  go _ [] = Nothing
  go seen (variable : remaining)
    | IntSet.member variable seen = Just variable
    | otherwise = go (IntSet.insert variable seen) remaining

-- | Normalize the complete type, peel only its leading prenex chain, then
-- split consecutive arrows. A forall reached after an arrow remains in the
-- result. Malformed binder lists or namespace exhaustion retain the entire
-- source type, ensuring checked callers fail closed instead of flattening it.
splitArrowResultParams :: HsType -> (HsType, [HsType], [TVarId], [HsConstraint])
splitArrowResultParams source = case alphaNormalizeForalls IntSet.empty source of
  Left _ -> (source, [], [], [])
  Right (normalized, _) ->
    let (body, variables, constraints) = splitForalls normalized
        (result, parameters) = splitArrows body
    in (result, parameters, variables, constraints)
 where
  splitForalls (TypeForall variables constraints body) =
    let (result, nestedVariables, nestedConstraints) = splitForalls body
    in ( result
       , variables ++ nestedVariables
       , constraints ++ nestedConstraints
       )
  splitForalls body = (body, [], [])

  splitArrows (TypeArrow parameter result) =
    let (finalResult, parameters) = splitArrows result
    in (finalResult, parameter : parameters)
  splitArrows result = (result, [])

-- | Whether a type contains explicit quantification at any depth.
containsForall :: HsType -> Bool
containsForall TypeForall{} = True
containsForall (TypeArrow parameter result) =
  containsForall parameter || containsForall result
containsForall (TypeApp function argument) =
  containsForall function || containsForall argument
containsForall _ = False

-- | Whether quantification occurs below the complete leading prenex chain or
-- inside an outer constraint argument.
containsNestedForall :: HsType -> Bool
containsNestedForall ty@TypeForall{} =
  any constraintContainsForall outerConstraints || containsForall body
  where
    (outerConstraints, body) = stripOuterForalls ty
    stripOuterForalls (TypeForall _ constraints inner) =
      let (nestedConstraints, result) = stripOuterForalls inner
      in (constraints ++ nestedConstraints, result)
    stripOuterForalls other = ([], other)
containsNestedForall ty = containsForall ty

constraintContainsForall :: HsConstraint -> Bool
constraintContainsForall = any containsForall . constraint_params

-- | Find the nominal head beneath foralls and type applications.
typeConstructorHead :: HsType -> Maybe QualifiedName
typeConstructorHead typeExpression = case typeExpression of
  TypeForall _ _ body -> typeConstructorHead body
  TypeApp function _ -> typeConstructorHead function
  TypeCons name -> Just name
  _ -> Nothing
