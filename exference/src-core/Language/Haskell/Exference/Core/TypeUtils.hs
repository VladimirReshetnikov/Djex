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
  (flexibleIdentifiers)
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

splitArrowResultParams :: HsType -> (HsType, [HsType], [TVarId], [HsConstraint])
splitArrowResultParams t
  | TypeArrow t1 t2 <- t
  , (rt,pts,fvs,cs) <- splitArrowResultParams t2
  = (rt, t1:pts, fvs, cs)
  | TypeForall vs cs t1 <- t
  , (rt, pts, fvs, cs') <- splitArrowResultParams t1
  = (rt, pts, vs++fvs, cs++cs')
  | otherwise
  = (t, [], [], [])

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
