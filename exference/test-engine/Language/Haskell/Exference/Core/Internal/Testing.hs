-- | Cabal-private test seam for otherwise unreachable engine boundaries.
--
-- The dedicated @exference-engine-tests@ component compiles this module and
-- the parser-neutral core as home modules. Production searches always use the
-- full 'Int' namespaces; tiny capacities let regressions exercise truthful
-- truncation without making a testing hook part of the library API.
module Language.Haskell.Exference.Core.Internal.Testing
  ( IdentifierCapacities (..)
  , findExpressionsWithIdentifierCapacitiesEither
  , findTypedEngineCandidatesWithIdentifierCapacitiesEither
  , findQueryResultsWithIdentifierCapacitiesEither
  , queryProjectionStrictnessForTesting
  , compatibilityProjectionStrictnessForTesting
  , singleOptionValidationStrictnessForTesting
  , compatibilityPruningCount
  , compatibilityBindingUsageCounts
  , mergePriorityQueueAtCapacity
  , pruningReasonsFromNaturalTotals
  , typeComplexityForTesting
  )
where

import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.PQueue.Prio.Max as Q
import Numeric.Natural (Natural)

import qualified Language.Haskell.Exference.Core.Internal.Exference as E
import Language.Haskell.Exference.Core.Expression (Expression)
import Language.Haskell.Exference.Core.ExferenceStats (ExferenceStats)
import Language.Haskell.Exference.Core.Internal.ExpressionCheck
  ( ExferenceTermGraphAvailability )
import Language.Haskell.Exference.Core.Candidate
  ( ExferenceSourceTypeVariableHints
  , ExferenceTypeVariableHints
  )
import Language.Haskell.Exference.Core.Name (QualifiedName)
import Language.Haskell.Exference.Core.Internal.Options
  ( ExferenceHeuristicsConfig )
import Language.Haskell.Exference.Core.Score (Penalty)
import Language.Haskell.Exference.Core.Types (HsConstraint, HsType)
import Language.Haskell.Exference.Core.Internal.VariableSupply
  ( identifierSupplySize )
import Language.Haskell.Exference.Core.RigidInstantiation
  (nestedRigidInstantiationCount)
import qualified Language.Haskell.Exference.Core.Internal.Scope as Scope
import Language.Haskell.Exference.Core.Internal.SearchControl
import qualified Language.Haskell.Synthesis.Count as SharedCount
import qualified Language.Haskell.Synthesis.Search as SharedSearch
import Language.Haskell.Synthesis.Generated (DefinitionName)

-- | Total capacities for the four independent dynamic search namespaces.
-- Term capacity excludes root hole zero; flexible and scope capacities include
-- identifiers already reserved by the checked root node. Rigid capacity counts
-- only nested skolems because the root plan is validated before search starts.
data IdentifierCapacities = IdentifierCapacities
  { termIdentifierCapacity :: Natural
  , flexibleIdentifierCapacity :: Natural
  , nestedRigidIdentifierCapacity :: Natural
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

-- | Retain the exact private engine-candidate association instead of passing
-- through either legacy projection.
findTypedEngineCandidatesWithIdentifierCapacitiesEither
  :: IdentifierCapacities
  -> E.ExferenceInput
  -> Either E.ExferenceInputError
      [(Expression, ExferenceStats, ExferenceTermGraphAvailability)]
findTypedEngineCandidatesWithIdentifierCapacitiesEither capacities input = do
  checked <- E.prepareExferenceInput input
  pure $ E.findEngineCandidatesWithAllocatorsForTesting
    (finiteSearchAllocators capacities) checked

-- | Exercise the canonical result path with bounded internal namespaces.
-- Unlike the compatibility-input helpers above, this retains the checked
-- definition target and shared logical-evidence envelope.
findQueryResultsWithIdentifierCapacitiesEither
  :: IdentifierCapacities
  -> DefinitionName
  -> ExferenceSourceTypeVariableHints
  -> E.ExferenceEnvironment
  -> E.ExferenceQuery
  -> Either E.ExferenceInputError [E.ExferenceResult]
findQueryResultsWithIdentifierCapacitiesEither capacities =
  E.findQueryResultsWithAllocators $ finiteSearchAllocators capacities

-- | Closed observations from the final engine-to-query strictness probe.
-- Production callers use 'E.findQueryResultsInEnvironmentEither'.
queryProjectionStrictnessForTesting
  :: DefinitionName
  -> ExferenceTypeVariableHints
  -> ( Bool
     , SharedSearch.Progress
     , E.ExferenceBatchMetadata
     , DefinitionName
     )
queryProjectionStrictnessForTesting = E.queryProjectionStrictnessForTesting

compatibilityProjectionStrictnessForTesting
  :: ( E.SearchStatus
     , Expression
     , [HsConstraint]
     , ExferenceStats
     )
compatibilityProjectionStrictnessForTesting =
  E.compatibilityProjectionStrictnessForTesting

-- | Check options, poison the original public query field, and enter the
-- package-private checked-options runner. Success proves that preparation
-- consumes the retained witness instead of evaluating the options again.
singleOptionValidationStrictnessForTesting
  :: DefinitionName
  -> ExferenceSourceTypeVariableHints
  -> E.ExferenceEnvironment
  -> E.ExferenceQuery
  -> Either E.ExferenceInputError ()
singleOptionValidationStrictnessForTesting
    target sourceHints environment query = do
  checkedOptions <- E.checkExferenceOptions $ E.querySearchOptions query
  () <$ E.findQueryResultsInEnvironmentWithCheckedOptions
    target sourceHints environment poisonedQuery checkedOptions
 where
  poisonedQuery = query
    { E.querySearchOptions =
        error "checked Exference options were evaluated a second time"
    }

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
compatibilityPruningCount = SharedCount.saturatingNaturalToInt

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

-- | Compare native structural types with their historical engine spellings
-- without exposing search-node internals to the regression suite.
typeComplexityForTesting
  :: ExferenceHeuristicsConfig
  -> HsType
  -> Penalty
typeComplexityForTesting = E.typeComplexity

finiteSearchAllocators :: IdentifierCapacities -> SearchAllocators
finiteSearchAllocators capacities = defaultSearchAllocators
  { searchAllocateTermIdentifier = allocateTerm
  , searchAllocateFlexibleNamespace = allocateFlexible
  , searchAllocateNestedRigidInstantiations = allocateRigid
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

  allocateRigid binders plan
    | nestedRigidInstantiationCount plan + requested
        <= nestedRigidIdentifierCapacity capacities =
          searchAllocateNestedRigidInstantiations
            defaultSearchAllocators binders plan
    | otherwise = Nothing
   where
    requested = SharedCount.naturalLength binders

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
