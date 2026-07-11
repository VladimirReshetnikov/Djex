{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Haskell.Exference.Core.Types
  ( TVarId
  , QualifiedName(..)
  , qualifiedNameOperator
  , HsType (..)
  , HsTypeOffset (..)
  , Subst (..)
  , Substs
  , HsTypeClass (..)
  , HsInstance (..)
  , HsConstraint (..)
  , StaticClassEnv (..)
  , QueryClassEnv ( qClassEnv_env
                  , qClassEnv_constraints
                  , qClassEnv_inflatedConstraints
                  , qClassEnv_varConstraints )
  , constraintApplySubsts
  , inflateHsConstraints
  , applySubst
  , applySubsts
  -- , typeParser
  , containsVar
  , showVar
  , showTypedVar
  , mkQueryClassEnv
  , addQueryClassEnv
  , freeVars
  , showHsConstraint
  , TypeVarIndex
  , showHsType
  )
where



import Data.Char ( ord, chr, isPunctuation, isSymbol, toLower )
import Data.List ( intercalate, intersperse )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( Any(..) )
import Control.Monad ( liftM2 )

import qualified Data.Set as S
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as L

import Language.Haskell.Exference.Core.Internal.Closure ( closure )

import Control.DeepSeq.Generics
import Control.DeepSeq
import GHC.Generics
import Data.Data ( Data )
import Control.Monad.Trans.MultiState
import Safe




type TVarId = Int
data Subst  = Subst {-# UNPACK #-} !TVarId !HsType
type Substs = IntMap.IntMap HsType

data QualifiedName
  = QualifiedName [String] String
  | ListCon
  | TupleCon Int
  | Cons
  deriving (Eq, Ord, Generic, Data)

data HsType = TypeVar      {-# UNPACK #-} !TVarId
            | TypeConstant {-# UNPACK #-} !TVarId
              -- like TypeCons, for exference-internal purposes.
            | TypeCons     QualifiedName
            | TypeArrow    !HsType !HsType
            | TypeApp      !HsType !HsType
            | TypeForall   [TVarId] [HsConstraint] !HsType
  deriving (Ord, Eq, Generic, Data)

data HsTypeOffset = HsTypeOffset !HsType {-# UNPACK #-} !Int

-- Source locations do not belong in variable identity. Keeping only the
-- spelling also decouples the search core from a particular parser AST.
type TypeVarIndex = M.Map String Int

data HsTypeClass = HsTypeClass
  { tclass_name :: QualifiedName
  , tclass_params :: [TVarId]
  , tclass_constraints :: [HsConstraint]
  }
  deriving (Show, Generic, Data)

-- Class identity is nominal. Besides matching Haskell's class namespace, this
-- keeps equality and superclass closure finite for mutually recursive class
-- declarations; structurally comparing their recursively tied definitions
-- would diverge.
instance Eq HsTypeClass where
  left == right = tclass_name left == tclass_name right

instance Ord HsTypeClass where
  compare left right = compare (tclass_name left) (tclass_name right)

data HsInstance = HsInstance
  { instance_constraints :: [HsConstraint]
  , instance_tclass :: HsTypeClass
  , instance_params :: [HsType]
  }
  deriving (Eq, Show, Ord, Generic, Data)

data HsConstraint = HsConstraint
  { constraint_tclass :: HsTypeClass
  , constraint_params :: [HsType]
  }
  deriving (Eq, Ord, Generic, Data)

data StaticClassEnv = StaticClassEnv
  { sClassEnv_tclasses :: [HsTypeClass]
  , sClassEnv_instances :: M.Map QualifiedName [HsInstance]
  }
  deriving (Show, Generic, Data)

data QueryClassEnv = QueryClassEnv
  { qClassEnv_env :: StaticClassEnv
  , qClassEnv_constraints :: S.Set HsConstraint
  , qClassEnv_inflatedConstraints :: S.Set HsConstraint
  , qClassEnv_varConstraints :: IntMap.IntMap (S.Set HsConstraint)
  }
  deriving (Generic)

instance NFData QualifiedName  where rnf = genericRnf
instance NFData HsType         where rnf = genericRnf
instance NFData HsTypeClass    where rnf = genericRnf
instance NFData HsInstance     where rnf = genericRnf
instance NFData HsConstraint   where rnf = genericRnf
instance NFData StaticClassEnv where rnf = genericRnf
instance NFData QueryClassEnv  where rnf = genericRnf

instance Show QualifiedName where
  show name@(QualifiedName ns rawName) = intercalate "."
    $ ns ++ [maybe rawName (\operator -> '(' : operator ++ ")")
        (qualifiedNameOperator name)]
  show ListCon              = "[]"
  show (TupleCon 0)         = "()"
  show (TupleCon i)         = "(" ++ replicate (i-1) ',' ++ ")"
  show Cons                 = "(:)"

-- | Return the bare spelling of a symbolic ordinary name.  Symbols are kept
-- bare in 'QualifiedName'; parentheses belong to Haskell's prefix syntax and
-- are added only by renderers.  Accepting the historical parenthesized payload
-- here keeps hand-constructed values readable while frontend constructors
-- maintain the canonical representation.
qualifiedNameOperator :: QualifiedName -> Maybe String
qualifiedNameOperator (QualifiedName _ rawName) = case rawName of
  '(' : rest -> case reverse rest of
    ')' : reversedOperator
      | isOperator (reverse reversedOperator) -> Just (reverse reversedOperator)
    _ -> Nothing
  operator
    | isOperator operator -> Just operator
  _ -> Nothing
  where
    isOperator [] = False
    isOperator characters = all isOperatorCharacter characters
    isOperatorCharacter character =
      character `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)
      || ((isSymbol character || isPunctuation character)
          && character `notElem` ("(),;[]`{}_\"'" :: String))
qualifiedNameOperator _ = Nothing

instance Show HsType where
  showsPrec _ (TypeVar i) = showString $ showVar i
  showsPrec _ (TypeConstant i) = showString $ "C" ++ showVar i
  showsPrec d (TypeCons s) = showsPrec d s
  showsPrec d (TypeArrow t1 t2) =
    showParen (d> -2) $ showsPrec (-1) t1 . showString " -> " . showsPrec (-1) t2
  showsPrec d (TypeApp t1 t2) =
    showParen (d> -1) $ showsPrec 0 t1 . showString " " . showsPrec 0 t2
  showsPrec d (TypeForall [] [] t) = showsPrec d t
  showsPrec d (TypeForall is cs t) =
    showParen (d>0)
    $ showString ("forall " ++ intercalate ", " (showVar <$> is) ++ " . ")
    . showParen True (\x -> foldr (++) x $ intersperse ", " $ map show cs)
    . showString " => "
    . showsPrec (-2) t

showHsType :: TypeVarIndex -> HsType -> String
showHsType convMap t = h 0 t ""
 where
  h :: Int -> HsType -> ShowS
  h _ (TypeVar i)      = showString
                       $ maybe "badNameInternalError"
                               fst
                       $ L.find ((i ==) .  snd)
                       $ M.toList convMap
  h _ (TypeConstant i) = showString
                       $ maybe "badNameInternalError"
                               fst
                       $ L.find ((i ==) .  snd)
                       $ M.toList convMap
  h _ (TypeCons s) = shows s
  h d (TypeArrow t1 t2) =
    showParen (d> -2) $ t1Shows . showString " -> " . t2Shows
    where
      t1Shows = h (-1) t1
      t2Shows = h (-1) t2
  h d (TypeApp t1 t2) =
    showParen (d> -1) $ t1Shows . showString " " . t2Shows
    where
      t1Shows = h 0 t1
      t2Shows = h 0 t2
  h d (TypeForall [] [] ty) = h d ty
  h d (TypeForall is cs ty) =
    showParen (d>0)
      $ showString ("forall " ++ intercalate ", " (showVar <$> is) ++ " . ")
      . showParen True (\x -> foldr (++) x $ intersperse ", " $ map show cs)
      . showString " => "
      . tShows
    where
      tShows = h (-2) ty

-- instance Read HsType where
--   readsPrec _ = maybeToList . parseType

instance Show HsConstraint where
  show (HsConstraint c ps) = unwords $ show (tclass_name c) : map show ps

showHsConstraint :: TypeVarIndex
                 -> HsConstraint
                 -> String
showHsConstraint convMap (HsConstraint c ps) =
  unwords $ show name : tyStrs  
 where
  name = tclass_name c
  tyStrs = showHsType convMap <$> ps
  

instance Show QueryClassEnv where
  show (QueryClassEnv _ cs _ _) = "(QueryClassEnv _ " ++ show cs ++ " _)"
filterHsConstraintsByVarId :: TVarId
                           -> S.Set HsConstraint
                           -> S.Set HsConstraint
filterHsConstraintsByVarId i = S.filter
                             $ any (containsVar i) . constraint_params

containsVar :: TVarId -> HsType -> Bool
containsVar i = S.member i . freeVars

mkQueryClassEnv :: StaticClassEnv -> [HsConstraint] -> QueryClassEnv
mkQueryClassEnv sClassEnv constrs = addQueryClassEnv constrs $ QueryClassEnv {
  qClassEnv_env = sClassEnv,
  qClassEnv_constraints = S.empty,
  qClassEnv_inflatedConstraints = S.empty,
  qClassEnv_varConstraints = IntMap.empty
}

addQueryClassEnv :: [HsConstraint] -> QueryClassEnv -> QueryClassEnv
addQueryClassEnv constrs env = env {
  qClassEnv_constraints = csSet,
  qClassEnv_inflatedConstraints = inflated,
  qClassEnv_varConstraints = helper csSet
}
  where
    csSet = S.fromList constrs `S.union` qClassEnv_constraints env
    inflated = inflateHsConstraints csSet
    helper :: S.Set HsConstraint -> IntMap.IntMap (S.Set HsConstraint)
    helper cs =
      let ids :: IntSet.IntSet
          ids = IntSet.fromList . S.toList . foldMap freeVars
              $ constraint_params =<< S.toList cs
      in IntMap.fromSet (flip filterHsConstraintsByVarId
                        inflated) ids

inflateHsConstraints :: S.Set HsConstraint -> S.Set HsConstraint
inflateHsConstraints = closure (S.fromList . superclasses)
  where
    superclasses :: HsConstraint -> [HsConstraint]
    superclasses (HsConstraint (HsTypeClass _ ids constrs) ps) =
      map (snd . constraintApplySubsts (IntMap.fromList $ zip ids ps)) constrs

constraintApplySubst :: Subst -> HsConstraint -> HsConstraint
constraintApplySubst s (HsConstraint c ps) =
  HsConstraint c $ map (applySubst s) ps

-- returns if any change was necessary,
-- plus the (potentially changed) constraint
-- constraintApplySubst' :: Subst -> HsConstraint -> (Bool, HsConstraint)
-- constraintApplySubst' s (HsConstraint c ps) =
--   let applied = map (applySubst' s) ps
--   in (any fst applied, HsConstraint c $ snd <$> applied)

-- returns if any change was necessary,
-- plus the (potentially changed) constraint
{-# INLINE constraintApplySubsts #-}
constraintApplySubsts :: Substs -> HsConstraint -> (Any, HsConstraint)
constraintApplySubsts ss c
  | IntMap.null ss = return c
  | HsConstraint cl ps <- c =
    HsConstraint cl <$> mapM (applySubsts ss) ps

showVar :: TVarId -> String
showVar 0 = "v0"
showVar i | i<27      = [chr (ord 'a' + i - 1)]
          | otherwise = "t"++show (i-27)

showTypedVar :: forall m
              . ( MonadMultiState (M.Map TVarId HsType) m )
             => TVarId
             -> m String
showTypedVar i = do
  m <- mGet
  fromJustNote "missing collectVarTypes before showTypedVar"
    $ h <$> M.lookup i m
 where
  -- h t | traceShow (i, t) False = undefined
  h TypeVar{}          = return $ showVar i
  h TypeConstant{}     = return $ showVar i
  h (TypeCons qName) = do
    return $ case qName of
      QualifiedName _ (c:_) -> toLower c : show i
      QualifiedName{}       -> showVar i
      ListCon               -> showVar i ++ "s"
      TupleCon{}            -> showVar i
      Cons                  -> showVar i
  h TypeArrow{}        = return $ "f" ++ show i
  h (TypeApp t _)      = h t
  h (TypeForall _ _ t) = h t

applySubst :: Subst -> HsType -> HsType
applySubst (Subst i t) v@(TypeVar j) = if i==j then t else v
applySubst _ c@(TypeConstant _) = c
applySubst _ c@(TypeCons _)     = c
applySubst s (TypeArrow t1 t2)  = TypeArrow (applySubst s t1) (applySubst s t2)
applySubst s (TypeApp t1 t2)    = TypeApp (applySubst s t1) (applySubst s t2)
applySubst s@(Subst i _) f@(TypeForall js cs t) = if i `elem` js
  then f
  else TypeForall js (constraintApplySubst s <$> cs) (applySubst s t)

applySubsts :: Substs -> HsType -> (Any, HsType)
applySubsts s v@(TypeVar i)      = fromMaybe (return v)
                                  $ (,) (Any True) <$> IntMap.lookup i s
applySubsts _ c@(TypeConstant _) = return c
applySubsts _ c@(TypeCons _)     = return c
applySubsts s (TypeArrow t1 t2)  = liftM2 TypeArrow (applySubsts s t1) (applySubsts s t2)
applySubsts s (TypeApp t1 t2)    = liftM2 TypeApp   (applySubsts s t1) (applySubsts s t2)
applySubsts s (TypeForall js cs t) = liftM2 (TypeForall js)
  (sequence $ constraintApplySubsts unbound <$> cs)
  (applySubsts unbound t)
  where
    -- Bound variables are protected in both the context and the body.
    unbound = foldr IntMap.delete s js

freeVars :: HsType -> S.Set TVarId
freeVars (TypeVar i)         = S.singleton i
freeVars (TypeConstant _)    = S.empty
freeVars (TypeCons _)        = S.empty
freeVars (TypeArrow t1 t2)   = S.union (freeVars t1) (freeVars t2)
freeVars (TypeApp t1 t2)     = S.union (freeVars t1) (freeVars t2)
freeVars (TypeForall is cs t) = foldr S.delete allVars is
  where
    allVars = freeVars t `S.union` foldMap (foldMap freeVars . constraint_params) cs
