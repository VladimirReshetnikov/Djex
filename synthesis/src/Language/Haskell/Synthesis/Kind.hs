{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Language.Haskell.Synthesis.Kind
  ( Kind (..)
  , freeKindVariables
  ) where

import Control.DeepSeq (NFData)
import qualified Data.Set as Set
import Data.Set (Set)
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
