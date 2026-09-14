-- | Stateful primitives for extending a t'SearchNode' inside one search
-- branch: allocating holes, tracked variables, and fresh flexible
-- namespaces, opening child scopes, recording variable uses, and applying
-- substitutions.  Each runs in @StateT SearchNode SearchBranches@ and turns
-- identifier or scope-ID exhaustion into a truncated branch through the
-- supplied t'SearchAllocators' rather than an error.
module Language.Haskell.Exference.Core.Internal.ExferenceNodeBuilder
  ( builderAddScope
  , builderApplySubst
  , builderAllocVar
  , builderAllocHole
  , builderFreshenTVarNamespace
  , builderRecordVarUse
  , builderRetainKindOpenings
  , builderTransportKinds
  , builderCheckKindEquality
  , builderFreshenKindValueTypes
  )
where

import Language.Haskell.Exference.Core.Internal.FlexibleIds
  (FlexibleRenaming)
import Language.Haskell.Exference.Core.Internal.ExferenceNode
import Language.Haskell.Exference.Core.Internal.SearchControl
import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.Internal.RigidScope
  (validateRigidSubstitutions)
import qualified Language.Haskell.Exference.Core.Internal.Scope as Scope
import qualified Language.Haskell.Exference.Core.Internal.KindScope as KindScope
import Language.Haskell.Exference.Core.Internal.Polytype (LeadingForallOpening)
import Language.Haskell.Exference.Core.Internal.VariableSupply
  (reserveIdentifiers, reservedIdentifierSet)
import qualified Language.Haskell.Synthesis.Type as SharedType

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (StateT, gets, modify)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Allocate an expression hole without treating it as a variable introduced
-- into scope. The returned identifier is the value before the increment.
builderAllocHole
  :: SearchAllocators
  -> StateT SearchNode SearchBranches TVarId
builderAllocHole = builderAllocTermIdentifier

-- | Allocate a variable whose usage must be tracked by the search heuristic.
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

-- | Count one more use of a tracked variable in 'nodeVarUses'.  The variable
-- must have been allocated with 'builderAllocVar' (or otherwise registered);
-- an untracked identifier is an internal invariant violation and raises an
-- error.
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

-- | Take the current scope, add a child scope, and return its identifier.
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

-- | Apply substitutions to goals and scopes. Constraint goals are handled by
-- the caller because their admissibility depends on the search branch.
builderApplySubst
  :: Substs
  -- ^ Complete simultaneous unifier result, including temporary provider
  -- variables whose age edges still matter.
  -> Substs
  -- ^ Substitutions which belong to the persistent search node.
  -> StateT SearchNode SearchBranches ()
builderApplySubst checkedSubstitutions appliedSubstitutions = do
  builderTransportKinds checkedSubstitutions
  rigidScope <- gets nodeRigidScope
  case validateRigidSubstitutions rigidScope checkedSubstitutions of
    Left _ -> lift $ maybeBranch Nothing
    Right nextRigidScope -> modify $ \node -> node
      { nodeGoals = fmap (goalApplySubst appliedSubstitutions)
          (nodeGoals node)
      , nodeProvidedScopes = scopesApplySubsts appliedSubstitutions
          (nodeProvidedScopes node)
      , nodeRigidScope = nextRigidScope
      }

builderRetainKindOpenings
  :: [LeadingForallOpening] -> StateT SearchNode SearchBranches ()
builderRetainKindOpenings openings = builderUpdateKinds $
  \scope -> KindScope.retainLeadingForallKinds scope openings

builderTransportKinds :: Substs -> StateT SearchNode SearchBranches ()
builderTransportKinds substitutions = builderUpdateKinds $ \scope ->
  snd <$> KindScope.substituteKindScope scope
    (Map.fromList [(SharedType.FlexibleVariable variable, image)
      | (variable, image) <- IntMap.toList substitutions]) []

builderCheckKindEquality
  :: HsType -> HsType -> StateT SearchNode SearchBranches ()
builderCheckKindEquality left right = builderUpdateKinds $ \scope -> do
  KindScope.checkKindScopeEquality scope left right
  pure scope

-- Declaration batches share their implicit parameters, but each use owns
-- fresh lexical binders. The ordinary engine keeps its existing allocation.
builderFreshenKindValueTypes
  :: [HsType] -> [HsConstraint]
  -> StateT SearchNode SearchBranches ([HsType], [HsConstraint])
builderFreshenKindValueTypes types constraints = do
  current <- gets nodeKindScope
  case current of
    Nothing -> pure (types, constraints)
    Just scope -> do
      reserved <- gets $ reservedIdentifierSet . nodeFlexibleIds
      case KindScope.freshenKindScopeTypes
          (Set.fromList $ map SharedType.FlexibleVariable $ IntSet.toList reserved)
          scope types constraints of
        Left _ -> lift $ maybeBranch Nothing
        Right (freshTypes, freshConstraints, updated) -> do
          builderUpdateKinds $ const $ Right updated
          pure (freshTypes, freshConstraints)

builderUpdateKinds
  :: (KindScope.KindScope -> Either String KindScope.KindScope)
  -> StateT SearchNode SearchBranches ()
builderUpdateKinds update = do
  current <- gets nodeKindScope
  case current of
    Nothing -> pure ()
    Just scope -> case update scope of
      Left _ -> lift $ maybeBranch Nothing
      Right updated -> modify $ \node -> node
        { nodeKindScope = Just updated
        , nodeFlexibleIds = reserveIdentifiers
            [variable | SharedType.FlexibleVariable variable <-
              Set.toList $ KindScope.kindScopeVariables updated]
            (nodeFlexibleIds node)
        }
