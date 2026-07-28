{-# LANGUAGE BangPatterns #-}

-- | Finite collection policies shared by both synthesis backends.
--
-- Backends need several views of duplicate input: reusable membership checks,
-- a stable set for sorted diagnostics, and the order in which values first
-- become known duplicates.  t'DuplicateSummary' computes those views together
-- without a machine-sized occurrence count. This module also owns finite
-- relation closure so backend graphs share the same cycle-safe frontier rule.
module Language.Haskell.Synthesis.Collection
  ( Multiplicity (..)
  , DuplicateSummary
  , observedListLength
  , distinctOn
  , repetitionsWithFirstOn
  , firstPresent
  , maximumPresent
  , transitiveClosure
  , firstDuplicate
  , summarizeDuplicates
  , multiplicityOf
  , repeatedValueSet
  , repeatedValuesInFirstRepetitionOrder
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Observe a list's exact length through a supplied finite bound.
--
-- A result at or below the nonnegative bound is exact. A result one greater
-- means only that the list is wider; no later cell is inspected. This is the
-- appropriate comparison primitive when a public raw value is checked
-- against a finite tuple, constructor, or class arity, because a cyclic tail
-- can then be rejected without attempting to traverse it. Negative bounds
-- are treated as zero, and the result saturates at 'maxBound'.
observedListLength :: Int -> [value] -> Int
observedListLength maximumExpected = go 0
 where
  bound = max 0 maximumExpected

  go !observed [] = observed
  go !observed (_ : remaining)
    | observed >= bound =
        if observed == maxBound then maxBound else observed + 1
    | otherwise = go (observed + 1) remaining

-- | Keep the first value for each key, preserving source order.
--
-- Emitting a fresh value does not inspect the remaining input, so consumers
-- can obtain a finite prefix even when the input is infinite or partial.
distinctOn :: Ord key => (value -> key) -> [value] -> [value]
distinctOn project = go Set.empty
 where
  go !_ [] = []
  go !seen (value : remaining)
    | key `Set.member` seen = go seen remaining
    | otherwise = value : go (Set.insert key seen) remaining
   where
    key = project value

-- | Pair every repeated occurrence with the first value that had the same
-- key, preserving the order in which repetitions are encountered.
--
-- A third or later occurrence is still paired with the original value. This
-- is useful for diagnostics that should point at each later declaration while
-- naming the one that first claimed the key. Emitting a pair does not inspect
-- the remaining input.
repetitionsWithFirstOn
  :: Ord key
  => (value -> key)
  -> [value]
  -> [(value, value)]
repetitionsWithFirstOn project = go Map.empty
 where
  go !_ [] = []
  go !firsts (value : remaining) = case Map.lookup key firsts of
    Nothing -> go (Map.insert key value firsts) remaining
    Just original -> (original, value) : go firsts remaining
   where
    key = project value

-- | Return the first present value in traversal order.
--
-- Once a 'Just' is encountered, neither the rest of the collection nor the
-- contained value is forced. This makes the operation suitable for ordered
-- diagnostics over infinite or partial inputs.
firstPresent :: Foldable collection => collection (Maybe value) -> Maybe value
firstPresent = foldr choose Nothing
 where
  choose Nothing remaining = remaining
  choose present@Just{} _ = present

-- | Return the greatest present value in a finite collection.
--
-- 'Nothing' elements are ignored, and an all-absent collection returns
-- 'Nothing'. The strict left fold avoids retaining the traversed collection.
maximumPresent
  :: (Foldable collection, Ord value)
  => collection (Maybe value)
  -> Maybe value
maximumPresent = foldl' combine Nothing
 where
  combine Nothing candidate = candidate
  combine current Nothing = current
  combine (Just current) (Just candidate) = Just $ max current candidate

-- | Compute the finite transitive closure of a relation.
--
-- A separate frontier ensures that each discovered value is expanded once.
-- In particular, cycles terminate as soon as they stop discovering values.
transitiveClosure
  :: Ord value
  => (value -> Set.Set value)
  -> Set.Set value
  -> Set.Set value
transitiveClosure expand initial = go initial initial
 where
  go !seen !frontier
    | Set.null frontier = seen
    | otherwise =
        let discovered = foldMap expand frontier `Set.difference` seen
        in go (seen `Set.union` discovered) discovered

-- | Exact classification of a value relative to a summarized collection.
data Multiplicity
  = NotPresent
  | OccursOnce
  | OccursMultipleTimes
  deriving (Eq, Ord, Show)

-- | Duplicate information for a finite collection.
--
-- The first set contains every value, the second contains exactly the values
-- seen more than once, and the list orders repeated values by the position of
-- their second occurrence.  The constructor stays private so these views
-- cannot drift apart.
data DuplicateSummary value = DuplicateSummary
  !(Set.Set value)
  !(Set.Set value)
  [value]

-- | Return the value whose second occurrence is encountered first.
--
-- Unlike a complete t'DuplicateSummary', this query short-circuits once its
-- answer is known and can therefore succeed on an infinite input.
firstDuplicate :: Ord value => [value] -> Maybe value
firstDuplicate = go Set.empty
 where
  go !_ [] = Nothing
  go !seen (value : remaining)
    | value `Set.member` seen = Just value
    | otherwise = go (Set.insert value seen) remaining

-- | Summarize duplicates in one strict traversal, without occurrence counts.
summarizeDuplicates :: Ord value => [value] -> DuplicateSummary value
summarizeDuplicates = go Set.empty Set.empty []
 where
  go !seen !repeated reversedOrder [] =
    DuplicateSummary seen repeated $ reverse reversedOrder
  go !seen !repeated reversedOrder (value : rest)
    | value `Set.member` repeated = go seen repeated reversedOrder rest
    | value `Set.member` seen = go
        seen
        (Set.insert value repeated)
        (value : reversedOrder)
        rest
    | otherwise = go
        (Set.insert value seen)
        repeated
        reversedOrder
        rest

-- | Classify one value as absent, unique, or duplicated.
multiplicityOf
  :: Ord value
  => value
  -> DuplicateSummary value
  -> Multiplicity
multiplicityOf value (DuplicateSummary seen repeated _)
  | value `Set.member` repeated = OccursMultipleTimes
  | value `Set.member` seen = OccursOnce
  | otherwise = NotPresent

-- | The set of values that occur more than once.
repeatedValueSet :: DuplicateSummary value -> Set.Set value
repeatedValueSet (DuplicateSummary _ repeated _) = repeated

-- | Every repeated value once, ordered by its first repetition.
repeatedValuesInFirstRepetitionOrder
  :: DuplicateSummary value
  -> [value]
repeatedValuesInFirstRepetitionOrder (DuplicateSummary _ _ values) = values
