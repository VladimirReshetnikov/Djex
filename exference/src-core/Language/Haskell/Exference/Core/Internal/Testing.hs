-- | Narrow test seam for otherwise unreachable finite identifier exhaustion.
--
-- This module is explicitly internal: production searches always use the full
-- 'Int' namespaces.  Small capacities let regression tests exercise truthful
-- operational truncation without attempting to materialize billions of IDs.
module Language.Haskell.Exference.Core.Internal.Testing
  ( IdentifierCapacities (..)
  , findExpressionsWithIdentifierCapacitiesEither
  , findGeneratedSearchBatchesWithIdentifierCapacitiesEither
  , compatibilityPruningCount
  , compatibilityBindingUsageCounts
  , mergePriorityQueueAtCapacity
  , pruningReasonsFromNaturalTotals
  )
where

import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.PQueue.Prio.Max as Q
import Numeric.Natural (Natural)

import qualified Language.Haskell.Exference.Core.Internal.Exference as E
import Language.Haskell.Exference.Core.Name (QualifiedName)
import Language.Haskell.Exference.Core.Internal.FlexibleIds
  ( identifierSupplySize )
import qualified Language.Haskell.Exference.Core.Internal.Scope as Scope
import Language.Haskell.Exference.Core.Internal.SearchControl
import qualified Language.Haskell.Synthesis.Search as SharedSearch

-- | Total capacities for the three independent dynamic search namespaces.
-- Term capacity excludes root hole zero; flexible and scope capacities include
-- identifiers already reserved by the checked root node.
data IdentifierCapacities = IdentifierCapacities
  { termIdentifierCapacity :: Natural
  , flexibleIdentifierCapacity :: Natural
  , scopeIdentifierCapacity :: Natural
  }
  deriving (Eq, Show)

findExpressionsWithIdentifierCapacitiesEither
  :: IdentifierCapacities
  -> E.ExferenceInput
  -> Either E.ExferenceInputError [E.ExferenceChunkElement]
findExpressionsWithIdentifierCapacitiesEither capacities input = do
  checked <- E.prepareExferenceInput input
  pure $ E.findExpressionsWithAllocators
    (finiteSearchAllocators capacities) checked

findGeneratedSearchBatchesWithIdentifierCapacitiesEither
  :: IdentifierCapacities
  -> E.ExferenceInput
  -> Either E.ExferenceInputError [E.ExferenceGeneratedSearchBatch]
findGeneratedSearchBatchesWithIdentifierCapacitiesEither capacities input = do
  checked <- E.prepareExferenceInput input
  pure $ E.findGeneratedSearchBatchesWithAllocators
    (finiteSearchAllocators capacities) Map.empty checked

-- | Exercise the queue representation boundary with tiny payloads rather
-- than attempting to allocate an Int-sized frontier.
mergePriorityQueueAtCapacity
  :: Natural
  -> Maybe Int
  -> [(Int, Int)]
  -> [(Int, Int)]
  -> ([(Int, Int)], Natural)
mergePriorityQueueAtCapacity capacity maximumSize queued newEntries =
  (Q.toDescList retained, discarded)
 where
  (retained, discarded) = E.mergeQueueWithCapacity
    capacity maximumSize (Q.fromList queued) newEntries

compatibilityPruningCount :: Natural -> Int
compatibilityPruningCount = E.saturatingNaturalToInt

-- | Exercise the historical binding-count projection without constructing an
-- impossibly large search tree.
compatibilityBindingUsageCounts
  :: Map.Map QualifiedName Natural
  -> Map.Map QualifiedName Int
compatibilityBindingUsageCounts = E.projectCompatibilityBindingUsages

pruningReasonsFromNaturalTotals
  :: Natural
  -> Natural
  -> [SharedSearch.TruncationReason]
pruningReasonsFromNaturalTotals = E.naturalPruningReasons

finiteSearchAllocators :: IdentifierCapacities -> SearchAllocators
finiteSearchAllocators capacities = defaultSearchAllocators
  { searchAllocateTermIdentifier = allocateTerm
  , searchAllocateFlexibleNamespace = allocateFlexible
  , searchAddScope = allocateScope
  }
 where
  allocateTerm next
    | allocatedTermIdentifiers next
        < toInteger (termIdentifierCapacity capacities) =
          searchAllocateTermIdentifier defaultSearchAllocators next
    | otherwise = Nothing

  allocateFlexible identifiers supply
    | identifierSupplySize supply + requested
        <= flexibleIdentifierCapacity capacities =
          searchAllocateFlexibleNamespace
            defaultSearchAllocators identifiers supply
    | otherwise = Nothing
   where
    requested = fromIntegral $ IntSet.size $ IntSet.fromList identifiers

  allocateScope parent scopes
    | occupied < scopeIdentifierCapacity capacities =
        searchAddScope defaultSearchAllocators parent scopes
    | otherwise = Left $ Scope.ScopeIdCollision 0
   where
    occupied = fromIntegral $ length $ Scope.scopesToAscList scopes

-- Count the sequential identifiers preceding the current counter.  The
-- production allocator traverses positive IDs first, then the negative half,
-- and stops before revisiting root hole zero.
allocatedTermIdentifiers :: Int -> Integer
allocatedTermIdentifiers next
  | next > 0 = toInteger next - 1
  | next < 0 = toInteger (maxBound :: Int)
      + toInteger next - toInteger (minBound :: Int)
  | otherwise = toInteger (maxBound :: Int) * 2 + 1
