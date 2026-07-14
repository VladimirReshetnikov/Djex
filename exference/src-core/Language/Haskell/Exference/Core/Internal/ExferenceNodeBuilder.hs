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
  (FlexibleRenaming)
import Language.Haskell.Exference.Core.Internal.ExferenceNode
import Language.Haskell.Exference.Core.Internal.SearchControl
import Language.Haskell.Exference.Core.Types
import qualified Language.Haskell.Exference.Core.Internal.Scope as Scope

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (StateT, gets, modify)
import qualified Data.IntMap.Strict as IntMap

-- Allocate an expression hole without treating it as a variable introduced
-- into scope. The returned identifier is the value before the increment.
builderAllocHole
  :: SearchAllocators
  -> StateT SearchNode SearchBranches TVarId
builderAllocHole = builderAllocTermIdentifier

-- Allocate a variable whose usage must be tracked by the search heuristic.
builderAllocVar
  :: SearchAllocators
  -> StateT SearchNode SearchBranches TVarId
builderAllocVar allocators = do
  identifier <- builderAllocTermIdentifier allocators
  modify $ \node -> node
    { nodeVarUses = IntMap.insert identifier 0 $ nodeVarUses node }
  pure identifier

builderAllocTermIdentifier
  :: SearchAllocators
  -> StateT SearchNode SearchBranches TVarId
builderAllocTermIdentifier allocators = do
  next <- gets nodeNextVarId
  case searchAllocateTermIdentifier allocators next of
    Nothing -> lift $ truncateBranch BranchIdentifierSpaceExhausted
    Just (identifier, following) -> do
      modify $ \node -> node {nodeNextVarId = following}
      pure identifier

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
  :: SearchAllocators
  -> [TVarId]
  -> StateT SearchNode SearchBranches FlexibleRenaming
builderFreshenTVarNamespace allocators identifiers = do
  supply <- gets nodeFlexibleIds
  case searchAllocateFlexibleNamespace allocators identifiers supply of
    Just (renaming, nextSupply) ->
      modify (\node -> node {nodeFlexibleIds = nextSupply}) >> pure renaming
    Nothing -> lift $ truncateBranch BranchIdentifierSpaceExhausted

-- Take the current scope, add a child scope, and return its identifier.
builderAddScope
  :: SearchAllocators
  -> ScopeId
  -> StateT SearchNode SearchBranches ScopeId
builderAddScope allocators parentId = do
  scopes <- gets nodeProvidedScopes
  case searchAddScope allocators parentId scopes of
    Left (Scope.ScopeIdCollision _) ->
      lift $ truncateBranch BranchIdentifierSpaceExhausted
    Left failure -> error
      $ "Exference internal scope invariant violated: " ++ show failure
    Right (newId, newScopes) -> do
      modify $ \node -> node {nodeProvidedScopes = newScopes}
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
