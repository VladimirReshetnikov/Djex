module Language.Haskell.Exference.Core.Internal.ExferenceNodeBuilder
  ( builderSetReason
  , builderAppendReason
  , builderGetTVarOffset
  , builderAddScope
  , builderApplySubst
  , builderAllocVar
  , builderAllocHole
  , builderAllocNVar
  , builderRecordVarUse
  , builderRaiseMaxTVarId
  )
where

import Language.Haskell.Exference.Core.Internal.ExferenceNode
import Language.Haskell.Exference.Core.Types

import Control.Monad.Trans.State.Lazy (StateT, gets, modify, state)
import qualified Data.IntMap.Strict as IntMap

-- Record why the node was produced. This feeds diagnostics and usage reports;
-- search ancestry is deliberately not retained in production nodes.
builderSetReason :: Monad m => String -> StateT SearchNode m ()
builderSetReason reason =
  modify $ \node -> node { nodeLastStepReason = reason }

builderAppendReason :: Monad m => String -> StateT SearchNode m ()
builderAppendReason reason =
  modify $ \node -> node
    { nodeLastStepReason = nodeLastStepReason node ++ ", " ++ reason }

builderGetTVarOffset :: Monad m => StateT SearchNode m TVarId
builderGetTVarOffset = (+ 1) <$> gets nodeMaxTVarId
-- TODO: is (+1) really necessary? It was in the pre-transformation code,
-- but there is no documented reason for it yet.

-- Allocate an expression hole without treating it as a variable introduced
-- into scope. The returned identifier is the value before the increment.
builderAllocHole :: Monad m => StateT SearchNode m TVarId
builderAllocHole = state $ \node ->
  let vid = nodeNextVarId node
  in (vid, node { nodeNextVarId = vid + 1 })

-- Allocate the fresh rigid identifier used while opening a rank-N type.
-- As with expression holes, allocation returns the pre-increment value.
builderAllocNVar :: Monad m => StateT SearchNode m TVarId
builderAllocNVar = state $ \node ->
  let vid = nodeNextNVarId node
  in (vid, node { nodeNextNVarId = vid + 1 })

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

builderRaiseMaxTVarId :: Monad m => TVarId -> StateT SearchNode m ()
builderRaiseMaxTVarId candidate =
  modify $ \node -> node
    { nodeMaxTVarId = max candidate (nodeMaxTVarId node) }

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
