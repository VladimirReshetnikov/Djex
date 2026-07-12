{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Haskell.Exference.Core.Internal.ExferenceNodeBuilder
  ( SearchNodeBuilder
  , modifyNodeBy
  , builderSetReason
  , builderAppendReason
  , builderGetTVarOffset
  , builderAddScope
  , builderApplySubst
  , builderAllocVar
  )
where



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.Internal.ExferenceNode

import Control.Monad.State ( State
                           , execState
                           )
import Control.Monad.State.Lazy ( MonadState )
import Control.Monad ( liftM )

import Control.Lens



type SearchNodeBuilder a = State SearchNode a

modifyNodeBy :: SearchNode -> SearchNodeBuilder () -> SearchNode
modifyNodeBy = flip execState

-- Record why the node was produced. This feeds diagnostics and usage reports;
-- search ancestry is deliberately not retained in production nodes.
builderSetReason :: MonadState SearchNode m => String -> m ()
builderSetReason r = lastStepReason .= r

builderAppendReason :: MonadState SearchNode m => String -> m ()
builderAppendReason r = do
  lastStepReason %= (++ (", " ++ r))

builderGetTVarOffset :: MonadState SearchNode m => m TVarId
builderGetTVarOffset = liftM (+1) $ use maxTVarId
 -- TODO: is (+1) really necessary? it was in pre-transformation code,
 --       but i cannot find good reason now.. test?

builderAllocVar :: MonadState SearchNode m => m TVarId
builderAllocVar = do
  vid <- use nextVarId
  varUses . at vid ?= 0
  nextVarId <<+= 1

-- take the current scope, add new scope, return new id
builderAddScope :: MonadState SearchNode m => ScopeId -> m ScopeId
builderAddScope parentId = do
  (newId, newScopes) <- uses providedScopes $ addScope parentId
  providedScopes .= newScopes
  return newId

-- apply substs in goals and scopes
-- not contraintGoals, because that's handled by caller
builderApplySubst :: MonadState SearchNode m => Substs -> m ()
builderApplySubst substs = do
  goals . mapped %= goalApplySubst substs
  providedScopes %= scopesApplySubsts substs
