module Language.Haskell.Exference.Core.Internal.ExferenceNodeBuilder
  ( builderAddScope
  , builderApplySubst
  , builderAllocVar
  , builderAllocHole
  , builderFreshenTVarNamespace
  , builderRecordVarUse
  )
where

import Language.Haskell.Exference.Core.Internal.FlexibleIds
import Language.Haskell.Exference.Core.Internal.ExferenceNode
import Language.Haskell.Exference.Core.Types

import Control.Monad.Trans.State.Lazy (StateT, gets, modify, state)
import qualified Data.IntMap.Strict as IntMap

-- Allocate an expression hole without treating it as a variable introduced
-- into scope. The returned identifier is the value before the increment.
builderAllocHole :: Monad m => StateT SearchNode m TVarId
builderAllocHole = state $ \node ->
  let vid = nodeNextVarId node
  in (vid, node { nodeNextVarId = vid + 1 })

-- Allocate a variable whose usage must be tracked by the search heuristic.
builderAllocVar :: Monad m => StateT SearchNode m TVarId
builderAllocVar = state $ \node ->
  let vid = nodeNextVarId node
  in
  ( vid
  , node
      { nodeNextVarId = vid + 1
      , nodeVarUses = IntMap.insert vid 0 (nodeVarUses node)
      }
  )

builderRecordVarUse :: Monad m => TVarId -> StateT SearchNode m ()
builderRecordVarUse vid = do
  usage <- gets (IntMap.lookup vid . nodeVarUses)
  case usage of
    Nothing -> error
      ("Exference internal variable-use invariant violated: untracked variable "
        ++ showVar vid)
    Just usageCount -> modify $ \node -> node
      { nodeVarUses = IntMap.insert vid (usageCount + 1) (nodeVarUses node) }

-- | Allocate one injective spelling for every ID in a source polymorphic
-- namespace and reserve the complete namespace in this search branch.
builderFreshenTVarNamespace
  :: Monad m
  => [TVarId]
  -> StateT SearchNode m FlexibleRenaming
builderFreshenTVarNamespace identifiers = state $ \node ->
  case allocateNamespace identifiers $ nodeFlexibleIds node of
    Just (renaming, supply) ->
      (renaming, node {nodeFlexibleIds = supply})
    Nothing -> error
      "Exference exhausted the finite flexible-variable identifier supply"

-- Take the current scope, add a child scope, and return its identifier.
builderAddScope :: Monad m => ScopeId -> StateT SearchNode m ScopeId
builderAddScope parentId = do
  scopes <- gets nodeProvidedScopes
  let (newId, newScopes) = addScope parentId scopes
  modify $ \node -> node { nodeProvidedScopes = newScopes }
  pure newId

-- Apply substitutions to goals and scopes. Constraint goals are handled by
-- the caller because their admissibility depends on the search branch.
builderApplySubst :: Monad m => Substs -> StateT SearchNode m ()
builderApplySubst substs =
  modify $ \node -> node
    { nodeGoals = fmap (goalApplySubst substs) (nodeGoals node)
    , nodeProvidedScopes = scopesApplySubsts substs
        (nodeProvidedScopes node)
    }
