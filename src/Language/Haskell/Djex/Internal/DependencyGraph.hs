-- | Stable dependency ordering for source-module graphs.
--
-- Both the filesystem workspace and the in-memory source loader need the same
-- depth-first contract: dependencies precede their users, missing external
-- nodes are ignored, and the first cycle is reported in caller/dependency
-- order. Keeping that small policy here prevents their graph semantics from
-- drifting while leaving parsing and diagnostics at their respective edges.
module Language.Haskell.Djex.Internal.DependencyGraph
  ( DependencyCycle (..)
  , stableDependencyOrder
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map

-- | The first cycle found by 'stableDependencyOrder', described as the
-- node whose dependency edge closed it and the closed path in traversal
-- order.
data DependencyCycle key = DependencyCycle
  { dependencyCycleSource :: key
    -- ^ The node containing the edge that returned to an active ancestor.
  , dependencyCyclePath :: NonEmpty key
    -- ^ The repeated ancestor, intervening nodes, and ancestor again.
  }
  deriving (Eq, Show)

data VisitState = Visiting | Visited
  deriving (Eq, Show)

-- | Order the reachable nodes by a stable depth-first traversal. Roots and
-- each node's dependencies are visited in the supplied order. References not
-- present in the map are external leaves and contribute no result.
stableDependencyOrder
  :: Ord key
  => [key]
  -> Map.Map key node
  -> (node -> [key])
  -> Either (DependencyCycle key) [node]
stableDependencyOrder roots nodes dependencies =
  reverse . snd <$> visitKeys [] (Map.empty, []) roots
 where
  visitKeys _ state [] = Right state
  visitKeys stack state (key : remaining) = do
    next <- visitKey stack state key
    visitKeys stack next remaining

  visitKey stack state@(marks, ordered) key = case Map.lookup key marks of
    Just Visited -> Right state
    Just Visiting -> Left DependencyCycle
      { dependencyCycleSource = case stack of
          source : _ -> source
          [] -> key
      , dependencyCyclePath = key :|
          (reverse (takeWhile (/= key) stack) ++ [key])
      }
    Nothing -> case Map.lookup key nodes of
      Nothing -> Right state
      Just node -> do
        (afterDependencies, accumulated) <- visitKeys (key : stack)
          (Map.insert key Visiting marks, ordered)
          $ dependencies node
        pure
          ( Map.insert key Visited afterDependencies
          , node : accumulated
          )
