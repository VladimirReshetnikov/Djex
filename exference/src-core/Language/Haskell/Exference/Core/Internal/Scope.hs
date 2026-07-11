{-# LANGUAGE DeriveGeneric #-}

-- | A small, parser-independent lexical-scope forest.
--
-- Scope IDs and the forest representation are deliberately abstract.  A child
-- can only be added below an existing scope, and existing parent links cannot
-- be changed, so the public constructors preserve a rooted, single-parent
-- forest.  Checked traversal remains defensive: it diagnoses dangling links
-- and cycles should a malformed value ever arrive through future persistence,
-- deserialization, or unsafe internal code.
module Language.Haskell.Exference.Core.Internal.Scope
  ( ScopeId
  , Scopes
  , ScopeInvariantError (..)
  , initialScopeId
  , initialScopes
  , scopeGetAllBindings
  , scopesAddBinding
  , addScope
  , scopesMapBindings
  , scopesToAscList
  , validateScopeParentGraph
  )
where

import Control.DeepSeq (NFData (..))
import Data.Foldable (traverse_)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (intercalate)
import GHC.Generics (Generic)


-- | An opaque reference into a particular 'Scopes' value.
--
-- IDs returned by a newer forest may not be used with an older snapshot: the
-- checked operations report such stale references as 'MissingScopeId'.
newtype ScopeId = ScopeId Int
  deriving (Eq, Ord, Generic)

instance Show ScopeId where
  show (ScopeId scopeId) = show scopeId

instance NFData ScopeId where
  rnf (ScopeId scopeId) = rnf scopeId

data Scope binding = Scope
  { scopeBindings :: [binding]
  , scopeParent :: !(Maybe ScopeId)
  }
  deriving Generic

instance NFData binding => NFData (Scope binding) where
  rnf (Scope bindings parent) = rnf bindings `seq` rnf parent

-- The next ID is kept separate from the map so allocation stays O(log n).
-- Constructors are hidden: in particular, callers cannot reparent a scope or
-- insert an edge to a scope that does not already exist.
data Scopes binding = Scopes !Int !(IntMap.IntMap (Scope binding))
  deriving Generic

instance NFData binding => NFData (Scopes binding) where
  rnf (Scopes nextId scopeMap) = rnf nextId `seq` rnf scopeMap

instance Show binding => Show (Scopes binding) where
  show scopes = "Scopes\n" ++ intercalate "\n"
    [ "  " ++ show scopeId ++ ": Scope " ++ show bindings
        ++ " " ++ show (maybe [] pure parent)
    | (scopeId, bindings, parent) <- scopesToAscList scopes
    ]

-- | A corrupt reference or parent relation discovered by checked traversal.
-- Paths are ordered from the traversal root towards the failure.  Cycle paths
-- repeat their first ID at the end, for example @[1, 4, 1]@.
data ScopeInvariantError
  = MissingScopeId
      { missingScopeId :: !Int
      , scopeTraversalPath :: [Int]
      }
  | ScopeParentCycle
      { scopeCyclePath :: [Int]
      }
  | ScopeIdCollision
      { collidingScopeId :: !Int
      }
  deriving (Eq, Generic)

instance Show ScopeInvariantError where
  show (MissingScopeId scopeId path) =
    "missing scope " ++ show scopeId ++ " on parent path " ++ renderPath path
  show (ScopeParentCycle path) =
    "cyclic scope parents: " ++ renderPath path
  show (ScopeIdCollision scopeId) =
    "scope allocator attempted to reuse scope " ++ show scopeId

instance NFData ScopeInvariantError where
  rnf (MissingScopeId scopeId path) = rnf scopeId `seq` rnf path
  rnf (ScopeParentCycle path) = rnf path
  rnf (ScopeIdCollision scopeId) = rnf scopeId

renderPath :: [Int] -> String
renderPath = intercalate " -> " . map show

initialScopeId :: ScopeId
initialScopeId = ScopeId 0

-- | A forest containing the empty root scope.
initialScopes :: Scopes binding
initialScopes = Scopes 1 (IntMap.singleton 0 $ Scope [] Nothing)

-- | Return bindings visible from a scope, innermost scope first.
--
-- Although safe construction makes malformed parent relations impossible,
-- every lookup is checked.  This prevents a corrupt future representation
-- from turning a missing scope into an empty environment or a cycle into
-- nontermination.
scopeGetAllBindings
  :: ScopeId
  -> Scopes binding
  -> Either ScopeInvariantError [binding]
scopeGetAllBindings (ScopeId startId) (Scopes _ scopeMap) =
  fmap (concatMap snd) $ walkParentChain resolve startId
  where
    resolve scopeId = case IntMap.lookup scopeId scopeMap of
      Nothing -> Nothing
      Just (Scope bindings parent) ->
        Just (bindings, fmap scopeIdValue parent)

-- | Prepend a binding to an existing lexical scope.
scopesAddBinding
  :: ScopeId
  -> binding
  -> Scopes binding
  -> Either ScopeInvariantError (Scopes binding)
scopesAddBinding (ScopeId scopeId) binding (Scopes nextId scopeMap) =
  case IntMap.lookup scopeId scopeMap of
    Nothing -> Left $ MissingScopeId scopeId [scopeId]
    Just scope -> Right $ Scopes nextId
      $ IntMap.insert scopeId
          scope {scopeBindings = binding : scopeBindings scope}
          scopeMap

-- | Add an empty child of an existing scope.
--
-- A parent must already be present and the allocated ID must be fresh.  Since
-- this is the only operation that adds parent edges, those two checks make a
-- parent cycle unconstructible through the safe API.
addScope
  :: ScopeId
  -> Scopes binding
  -> Either ScopeInvariantError (ScopeId, Scopes binding)
addScope (ScopeId parentId) (Scopes nextId scopeMap)
  | not (IntMap.member parentId scopeMap) =
      Left $ MissingScopeId parentId [parentId]
  | IntMap.member nextId scopeMap = Left $ ScopeIdCollision nextId
  | otherwise = Right
      ( ScopeId nextId
      , Scopes (nextId + 1)
          $ IntMap.insert nextId (Scope [] $ Just $ ScopeId parentId) scopeMap
      )

-- | Transform every binding without changing scope identity or ancestry.
scopesMapBindings :: (a -> b) -> Scopes a -> Scopes b
scopesMapBindings f (Scopes nextId scopeMap) = Scopes nextId
  $ IntMap.map mapScope scopeMap
  where
    mapScope (Scope bindings parent) = Scope (map f bindings) parent

-- | Ascending debug view of the otherwise opaque forest.
scopesToAscList :: Scopes binding -> [(ScopeId, [binding], Maybe ScopeId)]
scopesToAscList (Scopes _ scopeMap) =
  [ (ScopeId scopeId, scopeBindings scope, scopeParent scope)
  | (scopeId, scope) <- IntMap.toAscList scopeMap
  ]

-- | Validate a raw @scope -> parent@ graph.
--
-- This diagnostic boundary intentionally accepts raw integer IDs while
-- 'Scopes' itself remains abstract.  It supports import/debug tooling and lets
-- regression tests exercise malformed graphs without adding an unsafe forest
-- constructor to the production API.
validateScopeParentGraph
  :: IntMap.IntMap (Maybe Int)
  -> Either ScopeInvariantError ()
validateScopeParentGraph parents =
  traverse_ (fmap (const ()) . walkParentChain resolve) (IntMap.keys parents)
  where
    resolve scopeId = case IntMap.lookup scopeId parents of
      Nothing -> Nothing
      Just parent -> Just ((), parent)

-- Walk from a scope towards its root while retaining payloads in traversal
-- order.  This single implementation backs both production binding lookup and
-- raw-graph validation, so their missing-link and cycle diagnostics agree.
walkParentChain
  :: (Int -> Maybe (payload, Maybe Int))
  -> Int
  -> Either ScopeInvariantError [(Int, payload)]
walkParentChain resolve = go IntSet.empty []
  where
    go visited reversePath scopeId
      | IntSet.member scopeId visited =
          let fullPath = reverse reversePath ++ [scopeId]
          in Left $ ScopeParentCycle $ dropWhile (/= scopeId) fullPath
      | otherwise = case resolve scopeId of
          Nothing -> Left $ MissingScopeId scopeId
            (reverse reversePath ++ [scopeId])
          Just (payload, Nothing) -> Right [(scopeId, payload)]
          Just (payload, Just parentId) -> do
            rest <- go
              (IntSet.insert scopeId visited)
              (scopeId : reversePath)
              parentId
            pure $ (scopeId, payload) : rest

scopeIdValue :: ScopeId -> Int
scopeIdValue (ScopeId scopeId) = scopeId
