{-# LANGUAGE DeriveGeneric #-}

-- | Collision-free allocation for Exference's finite type-variable IDs.
--
-- Public IDs occupy the complete 'Int' domain.  Freshness must therefore be
-- based on membership, not on unchecked @maximum + 1@ arithmetic. Flexible
-- search variables, rigid query skolems, independent checking, and unifier
-- projection share this supply so boundary cases have one implementation.
module Language.Haskell.Exference.Core.Internal.FlexibleIds
  ( IdentifierSupply
  , FlexibleIdSupply
  , FlexibleRenaming
  , supplyFromIdentifiers
  , supplyFromIdentifierSet
  , identifierSupplySize
  , reserveIdentifiers
  , allocateFreshIdentifier
  , allocateFreshNonNegativeIdentifier
  , allocateNamespace
  , allocateCanonicalIdentifiers
  , flexibleIdentifiers
  , constraintFlexibleIdentifiers
  , renameFlexibleType
  , renameFlexibleConstraint
  , checkedAddIdentifier
  ) where

import Control.DeepSeq (NFData (..))
import Control.Monad (foldM, guard)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Exference.Core.Types
import qualified Language.Haskell.Synthesis.Type as SharedType

newtype IdentifierSupply = IdentifierSupply IntSet.IntSet
  deriving (Eq, Show, Generic)

instance NFData IdentifierSupply where
  rnf (IdentifierSupply identifiers) = rnf $ IntSet.toAscList identifiers

-- | Compatibility name for the generic supply where it tracks flexible IDs.
type FlexibleIdSupply = IdentifierSupply

type FlexibleRenaming = IntMap.IntMap TVarId

supplyFromIdentifiers :: Foldable collection => collection TVarId -> IdentifierSupply
supplyFromIdentifiers = IdentifierSupply . foldr IntSet.insert IntSet.empty

-- | Wrap an already canonical identifier set without rebuilding it.
--
-- This remains an internal operation: public boundaries expose allocation,
-- not the supply representation. Source conversion calls it for every new
-- spelling, so preserving the existing 'IntSet.IntSet' makes each ordinary
-- allocation logarithmic instead of repeatedly traversing the whole scope.
supplyFromIdentifierSet :: IntSet.IntSet -> IdentifierSupply
supplyFromIdentifierSet = IdentifierSupply

-- | Number of currently reserved spellings.  Production allocation needs
-- only membership; this observation supports a small finite-capacity test
-- allocator without exposing the supply representation.
identifierSupplySize :: IdentifierSupply -> Natural
identifierSupplySize (IdentifierSupply identifiers) =
  fromIntegral $ IntSet.size identifiers

reserveIdentifiers
  :: Foldable collection
  => collection TVarId
  -> IdentifierSupply
  -> IdentifierSupply
reserveIdentifiers identifiers (IdentifierSupply reserved) = IdentifierSupply
  $ foldr IntSet.insert reserved identifiers

-- | Allocate the historical next ID when that is representable.  If the
-- greatest ID is already 'maxBound', choose a real gap instead of wrapping.
allocateFreshIdentifier
  :: IdentifierSupply
  -> Maybe (TVarId, IdentifierSupply)
allocateFreshIdentifier supply@(IdentifierSupply reserved) = do
  identifier <- if IntSet.null reserved
    then Just 0
    else let greatest = IntSet.findMax reserved
      in if greatest < maxBound
        then Just $ greatest + 1
        else firstGapFrom 0 (dropWhile (< 0) identifiers)
          `orElse` firstGapFrom minBound identifiers
  guard $ not $ IntSet.member identifier reserved
  pure (identifier, reserveIdentifiers [identifier] supply)
 where
  identifiers = IntSet.toAscList reserved

  orElse (Just result) _ = Just result
  orElse Nothing fallback = fallback

-- | Allocate above the non-negative portion of the live namespace when that
-- is representable, matching Exference's historical rigid-skolem spelling.
-- At 'maxBound', reuse a genuine non-negative gap and then the negative half
-- instead of reporting exhaustion while almost the whole 'Int' domain is
-- still available.
allocateFreshNonNegativeIdentifier
  :: IdentifierSupply
  -> Maybe (TVarId, IdentifierSupply)
allocateFreshNonNegativeIdentifier supply@(IdentifierSupply reserved) = do
  identifier <- case dropWhile (< 0) identifiers of
    [] -> Just 0
    nonNegative ->
      let greatest = last nonNegative
      in if greatest < maxBound
          then Just $ greatest + 1
          else firstGapFrom 0 nonNegative
            `orElse` firstGapFrom minBound identifiers
  guard $ not $ IntSet.member identifier reserved
  pure (identifier, reserveIdentifiers [identifier] supply)
 where
  identifiers = IntSet.toAscList reserved

  orElse (Just result) _ = Just result
  orElse Nothing fallback = fallback

-- | Freshen one local polymorphic namespace.  For ordinary parser-produced
-- IDs this deliberately retains Exference's historical translation
-- @source + maximumReserved + 1@, keeping existing traces stable.  If that
-- translation overflows or intersects the live set, allocate actual gaps.
allocateNamespace
  :: [TVarId]
  -> FlexibleIdSupply
  -> Maybe (FlexibleRenaming, FlexibleIdSupply)
allocateNamespace rawSources supply@(IdentifierSupply reserved)
  | null sources = Just (IntMap.empty, supply)
  | otherwise = translated `orElse` gapAllocated
 where
  sources = IntSet.toAscList $ IntSet.fromList rawSources
  offset
    | IntSet.null reserved = Just 0
    | otherwise = checkedAddIdentifier (IntSet.findMax reserved) 1

  translated = do
    shift <- offset
    targets <- mapM (`checkedAddIdentifier` shift) sources
    guard $ all (`IntSet.notMember` reserved) targets
    let renaming = IntMap.fromList $ zip sources targets
    pure (renaming, reserveIdentifiers targets supply)

  gapAllocated = do
    (pairs, finalSupply) <- foldM allocate ([], supply) sources
    pure (IntMap.fromList pairs, finalSupply)

  allocate (pairs, currentSupply) source = do
    (target, nextSupply) <- allocateFreshIdentifier currentSupply
    pure ((source, target) : pairs, nextSupply)

  orElse (Just result) _ = Just result
  orElse Nothing fallback = fallback

-- | Give a second unifier namespace canonical external spellings.  Requested
-- IDs that do not collide are retained.  Collisions are allocated above all
-- requested IDs when possible, matching the historical projection, or from a
-- genuine set gap at the bounds.
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

  allocate (pairs, resultSupply@(IdentifierSupply resultReserved), freshSupply)
      identifier
    | IntSet.notMember identifier resultReserved = pure
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

-- The shared fold visits both binder declarations and occurrences, including
-- constraint arguments and structural tuple elements. Filtering by the tag
-- keeps the flexible and rigid integer namespaces distinct.
flexibleIdentifiers :: HsType -> IntSet.IntSet
flexibleIdentifiers = foldMap flexibleIdentifier
 where
  flexibleIdentifier variable = case variable of
    SharedType.FlexibleVariable identifier -> IntSet.singleton identifier
    SharedType.RigidVariable{} -> IntSet.empty

constraintFlexibleIdentifiers :: HsConstraint -> IntSet.IntSet
constraintFlexibleIdentifiers = foldMap flexibleIdentifiers

-- This is a whole-namespace rename, not a substitution originating outside a
-- lexical scope: binder declarations and their owned occurrences must move
-- together.  The shared functor performs exactly that traversal, including
-- constraints and structural tuples, while preserving rigid identities.
renameFlexibleType :: FlexibleRenaming -> HsType -> HsType
renameFlexibleType renaming = fmap renameVariable
 where
  renameVariable variable = case variable of
    SharedType.FlexibleVariable identifier -> SharedType.FlexibleVariable
      $ IntMap.findWithDefault identifier identifier renaming
    SharedType.RigidVariable{} -> variable

renameFlexibleConstraint :: FlexibleRenaming -> HsConstraint -> HsConstraint
renameFlexibleConstraint renaming = fmap $ renameFlexibleType renaming

checkedAddIdentifier :: TVarId -> TVarId -> Maybe TVarId
checkedAddIdentifier left right
  | total < toInteger (minBound :: TVarId) = Nothing
  | total > toInteger (maxBound :: TVarId) = Nothing
  | otherwise = Just $ fromInteger total
 where
  total = toInteger left + toInteger right

firstGapFrom :: TVarId -> [TVarId] -> Maybe TVarId
firstGapFrom candidate [] = Just candidate
firstGapFrom candidate (identifier : remaining)
  | identifier < candidate = firstGapFrom candidate remaining
  | identifier > candidate = Just candidate
  | candidate == maxBound = Nothing
  | otherwise = firstGapFrom (candidate + 1) remaining
