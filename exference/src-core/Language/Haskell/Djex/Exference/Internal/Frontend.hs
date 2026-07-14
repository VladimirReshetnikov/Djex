-- | Unstable, parser-neutral seam for Exference source frontends.
--
-- This module intentionally exposes only the operations a checked source
-- adapter needs. Session representation and search invariants remain owned by
-- the private implementation module.
module Language.Haskell.Djex.Exference.Internal.Frontend
  ( sealPreparedExferenceSessionWithPolicy
  , sessionTypeNames
  , sessionClasses
  , mkExferenceRequestWithSourceInfo
  , validateExferenceTarget
  , allocateFreshTypeVariableId
  ) where

import qualified Data.IntSet as IntSet

import Language.Haskell.Exference.Core.Internal.FlexibleIds
  ( allocateFreshIdentifier
  , supplyFromIdentifierSet
  )
import Language.Haskell.Exference.Core.Types (TVarId)
import Language.Haskell.Djex.Exference.Internal.Request
  ( mkExferenceRequestWithSourceInfo
  , validateExferenceTarget
  )
import Language.Haskell.Djex.Exference.Internal.Session
  ( sealPreparedExferenceSessionWithPolicy
  , sessionClasses
  , sessionTypeNames
  )

-- | Allocate from an exact parser-neutral namespace. This narrow wrapper lets
-- source frontends share the core's boundary-safe allocator without exposing
-- its supply representation as part of either public API.
allocateFreshTypeVariableId :: IntSet.IntSet -> Maybe TVarId
allocateFreshTypeVariableId reserved = fst
  <$> allocateFreshIdentifier
    (supplyFromIdentifierSet reserved)
