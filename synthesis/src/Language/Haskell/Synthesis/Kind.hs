{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

-- | The shared, backend-neutral kind tree.
--
-- The variable parameter lets frontends retain unresolved kind variables while
-- editing declarations. Checked inventories use @'Kind' 'Void'@ so an
-- unresolved variable cannot survive sealing.
module Language.Haskell.Synthesis.Kind
  ( Kind (..)
  , freeKindVariables
  , groundKind
  , observedKindNodeCount
  ) where

import Control.DeepSeq (NFData)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Void (Void)
import GHC.Generics (Generic)

-- | A proper-type kind, a frontend-owned kind variable, or a kind arrow.
data Kind variable
  = ProperTypeKind
  | KindVariable variable
  | FunctionKind (Kind variable) (Kind variable)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData variable => NFData (Kind variable)

-- | Collect every variable occurring in a kind.
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

-- | Observe a kind tree's exact constructor count through a finite bound.
--
-- A result at or below the nonnegative bound is exact. A result one greater
-- means only that the tree is larger; no remaining subtree or kind-variable
-- payload is inspected. The explicit worklist makes even a cyclic caller-built
-- tree terminate, while counting nodes rather than depth also bounds balanced
-- trees whose size grows exponentially with their depth. The observation
-- saturates at 'maxBound' when no larger sentinel can be represented.
observedKindNodeCount :: Int -> Kind variable -> Int
observedKindNodeCount maximumExpected root = go 0 [root]
 where
  bound = max 0 maximumExpected

  go !observed [] = observed
  go !observed (kind : rest)
    | observed >= bound =
        if observed == maxBound then maxBound else observed + 1
    | otherwise = case kind of
        ProperTypeKind -> go (observed + 1) rest
        KindVariable _ -> go (observed + 1) rest
        FunctionKind parameter result ->
          go (observed + 1) (parameter : result : rest)
