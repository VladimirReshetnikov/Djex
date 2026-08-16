-- | Exference operations that interpret raw identifiers as flexible type
-- variables. Finite-domain reservation and allocation live in
-- "Language.Haskell.Exference.Core.Internal.VariableSupply"; this module
-- owns only namespace renaming and shared-type traversal.
module Language.Haskell.Exference.Core.Internal.FlexibleIds
  ( FlexibleRenaming
  , allocateNamespace
  , allocateCanonicalIdentifiers
  , flexibleIdentifiers
  , renameFlexibleType
  , renameFlexibleConstraint
  ) where

import Control.Monad (foldM, guard)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import Language.Haskell.Exference.Core.Internal.VariableSupply
import Language.Haskell.Exference.Core.Types
import qualified Language.Haskell.Synthesis.Collection as SharedCollection
import qualified Language.Haskell.Synthesis.Type as SharedType

-- | A map from source flexible identifiers to their fresh spellings.
-- Identifiers absent from the map are left unchanged by
-- 'renameFlexibleType' and 'renameFlexibleConstraint'.
type FlexibleRenaming = IntMap.IntMap TVarId

-- | Freshen one local polymorphic namespace. For ordinary parser-produced
-- IDs this deliberately retains Exference's historical translation
-- @source + maximumReserved + 1@, keeping existing traces stable. If that
-- translation overflows or intersects the live set, allocate actual gaps.
allocateNamespace
  :: [TVarId]
  -> FlexibleIdSupply
  -> Maybe (FlexibleRenaming, FlexibleIdSupply)
allocateNamespace rawSources supply
  | null sources = Just (IntMap.empty, supply)
  | otherwise = SharedCollection.firstPresent [translated, gapAllocated]
 where
  sources = IntSet.toAscList $ IntSet.fromList rawSources
  offset = case maximumReservedIdentifier supply of
    Nothing -> Just 0
    Just greatest -> checkedAddIdentifier greatest 1

  translated = do
    shift <- offset
    targets <- mapM (`checkedAddIdentifier` shift) sources
    guard $ all
      (\identifier -> not $ identifierIsReserved identifier supply) targets
    let renaming = IntMap.fromList $ zip sources targets
    pure (renaming, reserveIdentifiers targets supply)

  gapAllocated = do
    (pairs, finalSupply) <- foldM allocate ([], supply) sources
    pure (IntMap.fromList pairs, finalSupply)

  allocate (pairs, currentSupply) source = do
    (target, nextSupply) <- allocateFreshIdentifier currentSupply
    pure ((source, target) : pairs, nextSupply)

-- | Give a second unifier namespace canonical external spellings. Requested
-- IDs that do not collide are retained. Collisions are allocated above all
-- requested IDs when possible, matching the historical projection, or from
-- a genuine set gap at the bounds.
allocateCanonicalIdentifiers
  :: [TVarId]
  -> FlexibleIdSupply
  -> Maybe (FlexibleRenaming, FlexibleIdSupply)
allocateCanonicalIdentifiers rawRequested initialSupply = do
  (pairs, finalSupply, _) <- foldM allocate
    ([], initialSupply, reserveIdentifiers requested initialSupply)
    requested
  pure (IntMap.fromList pairs, finalSupply)
 where
  requested = IntSet.toAscList $ IntSet.fromList rawRequested

  allocate (pairs, resultSupply, freshSupply) identifier
    | not $ identifierIsReserved identifier resultSupply = pure
        ( (identifier, identifier) : pairs
        , reserveIdentifiers [identifier] resultSupply
        , freshSupply
        )
    | otherwise = do
        (fresh, nextFreshSupply) <- allocateFreshIdentifier freshSupply
        pure
          ( (identifier, fresh) : pairs
          , reserveIdentifiers [fresh] resultSupply
          , nextFreshSupply
          )

-- | Every flexible type-variable identifier occurring anywhere in a type.
-- The shared fold visits both binder declarations and occurrences, including
-- constraint arguments and structural tuple elements. Filtering by the tag
-- keeps the flexible and rigid integer namespaces distinct.
flexibleIdentifiers :: HsType -> IntSet.IntSet
flexibleIdentifiers = foldMap
  $ SharedType.foldFlexibleVariable IntSet.singleton

-- | Rename every flexible identifier of a type through the renaming, leaving
-- unmapped identifiers and all rigid variables as they are.
-- This is a whole-namespace rename, not a substitution originating outside a
-- lexical scope: binder declarations and their owned occurrences move
-- together. The shared functor performs exactly that traversal while
-- preserving rigid identities.
renameFlexibleType :: FlexibleRenaming -> HsType -> HsType
renameFlexibleType renaming = fmap $ SharedType.mapFlexibleVariable
  $ \identifier -> IntMap.findWithDefault identifier identifier renaming

-- | 'renameFlexibleType' applied to every type argument of a constraint;
-- the class name is untouched.
renameFlexibleConstraint :: FlexibleRenaming -> HsConstraint -> HsConstraint
renameFlexibleConstraint renaming = fmap $ renameFlexibleType renaming
