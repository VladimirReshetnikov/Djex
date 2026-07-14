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
  , splitArrowChain
  , splitArrowResultParams
  , containsForall
  , containsNestedForall
  , typeConstructorHead
  )
where



import qualified Data.Set as S
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as M

import Language.Haskell.Exference.Core.Internal.FlexibleIds
  ( IdentifierSupply
  , allocateFreshIdentifier
  , flexibleIdentifiers
  , supplyFromIdentifiers
  )
import Language.Haskell.Exference.Core.Types
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Type as SharedType



-- binds everything in Foralls, so there are no free variables anymore.
forallify :: HsType -> HsType
forallify t = case t of
  TypeForallNative variables constraints body -> TypeForallNative
    (map SharedType.FlexibleVariable (S.toList frees) ++ variables)
    constraints
    body
  _ -> TypeForall (S.toList frees) [] t
 where
  frees = freeVars t

-- | Transform the complete flexible namespace, including forall binders and
-- constraints, without changing rigid search constants. The shared functor
-- also keeps structural tuple elements in the traversal automatically.
incVarIds :: (TVarId -> TVarId) -> HsType -> HsType
incVarIds transform = fmap transformVariable
 where
  transformVariable variable = case variable of
    SharedType.FlexibleVariable identifier ->
      SharedType.FlexibleVariable $ transform identifier
    SharedType.RigidVariable{} -> variable

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
constraintMapTypes = fmap

constraintContainsVariables :: HsConstraint -> Bool
constraintContainsVariables =
  any (not . S.null . freeVars) . constraint_params

-- | Why a lexical forall namespace could not be alpha-normalized.
data ForallNormalizationError
  = DuplicateForallBinder TVarId
  | RigidForallBinderCannotBeNormalized TVarId
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
  TypeTuple boxity elements -> do
    (normalizedElements, finalState) <- normalizeForallTypes state elements
    pure (TypeTuple boxity normalizedElements, finalState)
  TypeForallNative nativeVariables constraints body -> do
    variables <- case flexibleBinderIdentifiers nativeVariables of
      Left rigid -> Left $ RigidForallBinderCannotBeNormalized rigid
      Right flexible -> Right flexible
    case firstDuplicateVariable variables of
      Just duplicate -> Left $ DuplicateForallBinder duplicate
      Nothing -> pure ()
    (normalizedVariables, renaming, binderState) <-
      normalizeForallBinders state variables
    let sharedRenaming = M.fromList
          [ ( SharedType.FlexibleVariable source
            , SharedType.FlexibleVariable target
            )
          | (source, target) <- IntMap.toList renaming
          ]
        renameOwnedOccurrences =
          SharedType.renameScopedVariables sharedRenaming
        renamedConstraints = map
          (fmap renameOwnedOccurrences) constraints
        renamedBody = renameOwnedOccurrences body
    (normalizedConstraints, constraintState) <-
      normalizeForallConstraints binderState renamedConstraints
    (normalizedBody, bodyState) <-
      normalizeForallsInType constraintState renamedBody
    pure
      ( TypeForall normalizedVariables normalizedConstraints normalizedBody
      , bodyState
      )

flexibleBinderIdentifiers
  :: [SynthesisVariable]
  -> Either TVarId [TVarId]
flexibleBinderIdentifiers = traverse flexibleIdentifier
 where
  flexibleIdentifier variable = case variable of
    SharedType.FlexibleVariable identifier -> Right identifier
    SharedType.RigidVariable identifier -> Left identifier

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
        (result, parameters) = splitArrowChain body
    in (result, parameters, variables, constraints)
 where
  splitForalls (TypeForall variables constraints body) =
    let (result, nestedVariables, nestedConstraints) = splitForalls body
    in ( result
       , variables ++ nestedVariables
       , constraints ++ nestedConstraints
       )
  splitForalls body = (body, [], [])


-- | Split the consecutive outer arrow chain without inspecting or rewriting
-- its component types. In particular, a forall below an arrow remains the
-- result rather than being opened. This is the total monotype decomposition
-- used when substitutions expose more function parameters during search.
splitArrowChain :: HsType -> (HsType, [HsType])
splitArrowChain (TypeArrow parameter result) =
  let (finalResult, parameters) = splitArrowChain result
  in (finalResult, parameter : parameters)
splitArrowChain result = (result, [])

-- | Whether a type contains explicit quantification at any depth.
containsForall :: HsType -> Bool
containsForall TypeForallNative{} = True
containsForall (TypeArrow parameter result) =
  containsForall parameter || containsForall result
containsForall (TypeApp function argument) =
  containsForall function || containsForall argument
containsForall (TypeTuple _ elements) = any containsForall elements
containsForall _ = False

-- | Whether quantification occurs below the complete leading prenex chain or
-- inside an outer constraint argument.
containsNestedForall :: HsType -> Bool
containsNestedForall ty@TypeForallNative{} =
  any constraintContainsForall outerConstraints || containsForall body
  where
    (outerConstraints, body) = stripOuterForalls ty
    stripOuterForalls (TypeForallNative _ constraints inner) =
      let (nestedConstraints, result) = stripOuterForalls inner
      in (constraints ++ nestedConstraints, result)
    stripOuterForalls other = ([], other)
containsNestedForall ty = containsForall ty

constraintContainsForall :: HsConstraint -> Bool
constraintContainsForall = any containsForall . constraint_params

-- | Find the nominal head beneath foralls and type applications.
typeConstructorHead :: HsType -> Maybe QualifiedName
typeConstructorHead typeExpression = case typeExpression of
  TypeForallNative _ _ body -> typeConstructorHead body
  TypeApp function _ -> typeConstructorHead function
  TypeCons name -> Just name
  TypeTuple boxity elements -> either (const Nothing) Just
    $ SharedName.tupleName boxity $ length elements
  _ -> Nothing
