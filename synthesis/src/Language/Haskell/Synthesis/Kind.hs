{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Language.Haskell.Synthesis.Kind
  ( Kind (..)
  , freeKindVariables
  , groundKind
  ) where

import Control.DeepSeq (NFData)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Void (Void)
import GHC.Generics (Generic)

data Kind variable
  = ProperTypeKind
  | KindVariable variable
  | FunctionKind (Kind variable) (Kind variable)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData variable => NFData (Kind variable)

freeKindVariables :: Ord variable => Kind variable -> Set variable
freeKindVariables kind = case kind of
  ProperTypeKind -> Set.empty
  KindVariable variable -> Set.singleton variable
  FunctionKind parameter result ->
    freeKindVariables parameter `Set.union` freeKindVariables result

-- | Eliminate the kind-variable parameter only when the kind is fully
-- solved. The first remaining variable is retained as a precise diagnostic.
groundKind :: Kind variable -> Either variable (Kind Void)
groundKind kind = case kind of
  ProperTypeKind -> Right ProperTypeKind
  KindVariable variable -> Left variable
  FunctionKind parameter result -> FunctionKind
    <$> groundKind parameter <*> groundKind result
