{-# LANGUAGE BangPatterns #-}

-- | Stable deduplication and counter-free duplicate classification.
--
-- Backends need several views of duplicate input: reusable membership checks,
-- a stable set for sorted diagnostics, and the order in which values first
-- become known duplicates.  t'DuplicateSummary' computes those views together
-- without a machine-sized occurrence count.
module Language.Haskell.Synthesis.Collection
  ( Multiplicity (..)
  , DuplicateSummary
  , distinctOn
  , firstPresent
  , maximumPresent
  , firstDuplicate
  , summarizeDuplicates
  , multiplicityOf
  , repeatedValueSet
  , repeatedValuesInFirstRepetitionOrder
  ) where

import qualified Data.Set as Set

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
-- Unlike a complete 'DuplicateSummary', this query short-circuits once its
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
