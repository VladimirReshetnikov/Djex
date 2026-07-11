module Language.Haskell.Exference.Core.Internal.Closure
  ( closure
  )
where

import qualified Data.Set as Set

-- | Compute the finite transitive closure of a relation. Keeping a separate
-- frontier is important: iterating the relation itself never terminates when
-- the graph contains a cycle, even after no new elements can be discovered.
closure :: Ord a => (a -> Set.Set a) -> Set.Set a -> Set.Set a
closure expand initial = go initial initial
  where
    go seen frontier
      | Set.null frontier = seen
      | otherwise =
          let discovered = foldMap expand frontier `Set.difference` seen
          in go (seen `Set.union` discovered) discovered
