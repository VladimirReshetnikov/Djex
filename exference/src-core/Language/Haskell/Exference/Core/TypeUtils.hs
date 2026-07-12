{-# LANGUAGE PatternGuards #-}

module Language.Haskell.Exference.Core.TypeUtils
  ( incVarIds
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
  )
where



import qualified Data.Set as S
import qualified Data.IntMap.Strict as IntMap

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

largestId :: HsType -> TVarId
largestId (TypeVar i)       = i
largestId (TypeConstant _)  = -1
largestId (TypeCons _)      = -1
largestId (TypeArrow t1 t2) = largestId t1 `max` largestId t2
largestId (TypeApp t1 t2)   = largestId t1 `max` largestId t2
largestId (TypeForall ids cs t) = maximum
  (largestId t : ids ++ [ largestId p | c <- cs, p <- constraint_params c ])

largestSubstsId :: Substs -> TVarId
largestSubstsId = IntMap.foldl' (\a b -> a `max` largestId b) 0

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
