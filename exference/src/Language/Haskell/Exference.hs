{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}

module Language.Haskell.Exference
  ( findExpressions
  , findOneExpression
  , selectOneExpression
  , findSortNExpressions
  , selectSortNExpressions
  , findBestNExpressions
  , selectBestNExpressions
  , findFirstBestExpressions
  , selectFirstBestExpressions
  , takeFindSortNExpressions
  , findFirstExpressionLookahead
  , selectFirstExpressionLookahead
  , findFirstBestExpressionsLookahead
  , selectFirstBestExpressionsLookahead
  , findFirstBestExpressionsLookaheadPreferNoConstraints
  , selectFirstBestExpressionsLookaheadPreferNoConstraints
  , SearchSelection (..)
  , ExferenceInput ( .. )
  , ExferenceHeuristicsConfig (..)
  , ExferenceOutputElement
  , ExferenceChunkElement (..)
  , ExferenceSearchBatch
  , ExferenceGeneratedOutputElement
  , ExferenceGeneratedSearchBatch
  , ExferenceStats (..)
  , ExferenceInputError (..)
  , findExpressionsEither
  , findExpressionsWithStatsEither
  , SearchCompletion (..)
  , SearchStatus (..)
  , SearchStatusError (..)
  , toSearchProgress
  , toSearchBatch
  , toGeneratedSearchBatch
  , Penalty (..)
  , Priority (..)
  )
where



import Language.Haskell.Exference.Core

import Language.Haskell.Exference.Core.ExferenceStats
import Language.Haskell.Exference.Core.Score

import Data.Maybe ( listToMaybe, maybeToList )
import Data.List ( sortBy, groupBy, minimumBy )
import qualified Data.List as List
import Data.Ord ( comparing )



-- | The result of applying a presentation policy to a lazy search trace.
-- 'selectionStatus' is the status of the last chunk that the policy actually
-- inspected; it is terminal when 'selectionResult' is empty after consuming
-- the whole trace. Keeping the status in the fold avoids retaining the
-- already-consumed chunk prefix merely to explain an empty result later.
data SearchSelection result = SearchSelection
  { selectionStatus :: Maybe SearchStatus
  , selectionResult :: result
  }
  deriving (Eq)

mapSelection
  :: (first -> second)
  -> SearchSelection first
  -> SearchSelection second
mapSelection transform selection = SearchSelection
  (selectionStatus selection)
  (transform $ selectionResult selection)



-- returns the first found solution (not necessarily the best overall)
findOneExpression :: ExferenceInput
                  -> Maybe ExferenceOutputElement
findOneExpression = listToMaybe
  . selectionResult
  . selectOneExpression
  . findExpressionsWithStats

selectOneExpression
  :: [ExferenceChunkElement]
  -> SearchSelection [ExferenceOutputElement]
selectOneExpression = go Nothing
 where
  go status [] = SearchSelection status []
  go _ (chunk : chunks) = case listToMaybe $ chunkElements chunk of
    Just result -> SearchSelection (Just $ chunkStatus chunk) [result]
    Nothing -> go (Just $ chunkStatus chunk) chunks

-- calculates at most n solutions, sorts by rating, returns the first m
takeFindSortNExpressions :: Int
                         -> Int
                         -> ExferenceInput
                         -> [ExferenceOutputElement]
takeFindSortNExpressions m n =
  take m . findSortNExpressions n

-- calculates at most n solutions, and returns them sorted by their rating
findSortNExpressions :: Int
                     -> ExferenceInput
                     -> [ExferenceOutputElement]
findSortNExpressions n = selectionResult
  . selectSortNExpressions n
  . findExpressionsWithStats

selectSortNExpressions
  :: Int
  -> [ExferenceChunkElement]
  -> SearchSelection [ExferenceOutputElement]
selectSortNExpressions n = mapSelection (sortBy $ comparing rating)
  . takeSelection n
  where
    rating (_, _, ExferenceStats _ value _) = value

-- returns the first expressions with the best rating.
-- best explained on examples:
--   []      -> []
--   [2,5,5] -> [2]
--   [3,3,3,4,4,5,6,7] -> [3,3,3]
--   [2,5,2] -> [2] -- will not look past worse ratings
--   [4,3,2,2,2,3] -> [2,2,2] -- if directly next is better, switch to that
findFirstBestExpressions :: ExferenceInput
                         -> [ExferenceOutputElement]
findFirstBestExpressions = selectionResult
  . selectFirstBestExpressions
  . findExpressionsWithStats

selectFirstBestExpressions
  :: [ExferenceChunkElement]
  -> SearchSelection [ExferenceOutputElement]
selectFirstBestExpressions = go Nothing Nothing []
 where
  go status _ selected [] = SearchSelection status selected
  go _ previousRating selected (chunk : chunks) = inspect
    (Just $ chunkStatus chunk)
    previousRating
    selected
    (chunkElements chunk)
    chunks

  inspect status previousRating selected [] chunks =
    go status previousRating selected chunks
  inspect status Nothing _ (candidate@(_, _, stats) : candidates) chunks =
    inspect status
      (Just $ exference_complexityRating stats)
      [candidate]
      candidates
      chunks
  inspect status (Just previousRating) selected
      (candidate@(_, _, stats) : candidates) chunks
    | candidateRating < previousRating =
        inspect status (Just candidateRating) [candidate] candidates chunks
    | candidateRating == previousRating =
        inspect status (Just previousRating)
          (candidate : selected) candidates chunks
    | otherwise = SearchSelection status selected
   where
    candidateRating = exference_complexityRating stats

-- tries to find the best solution by performing a limitted amount of steps,
-- resetting the count whenever a better solution is found.
-- "finds the "first" (by some metric) local maximum"
-- advantages:
--   - might be able to find a "best" solution quicker than other approaches
--   - does not calculate the maximum amount of steps when there is no
--     solution left.
-- disadvantages:
--   - might find only a local optimum
findFirstExpressionLookahead :: Int
                             -> ExferenceInput
                             -> Maybe ExferenceOutputElement
findFirstExpressionLookahead n = listToMaybe
  . selectionResult
  . selectFirstExpressionLookahead n
  . findExpressionsWithStats

selectFirstExpressionLookahead
  :: Int
  -> [ExferenceChunkElement]
  -> SearchSelection [ExferenceOutputElement]
selectFirstExpressionLookahead n = go Nothing maxBound Nothing
  where
    go :: Maybe SearchStatus
      -> Int
      -> Maybe ExferenceOutputElement
      -> [ExferenceChunkElement]
      -> SearchSelection [ExferenceOutputElement]
    go status _ best [] = SearchSelection status $ maybeToList best
    go status 0 best _ = SearchSelection status $ maybeToList best
    go _ remaining best (chunk : chunks) =
      let status = Just $ chunkStatus chunk
      in case chunkElements chunk of
        [] -> go status (remaining - 1) best chunks
        elements -> case best of
          Nothing -> go status n (Just $ minElem elements) chunks
          Just (current@(_, _, currentStats))
            | candidate@(_, _, candidateStats) <- minElem elements
            , exference_complexityRating candidateStats
                < exference_complexityRating currentStats ->
                  go status n (Just candidate) chunks
            | otherwise -> go status (remaining - 1) (Just current) chunks
    minElem :: [ExferenceOutputElement] -> ExferenceOutputElement
    minElem = minimumBy (\(~(_, _, stats1)) (~(_, _, stats2)) ->
                           compare (exference_complexityRating stats1)
                                   (exference_complexityRating stats2))

-- a combination of the return-multiple-if-same-rating and the
-- look-some-steps-ahead-for-better-solution functionalities.
-- for example,
-- [2,3,2,2,4,5,6,7] -> [2,2,2]
--  does not stop at 3, but looks ahead, then returns all the 2-rated solutions
findFirstBestExpressionsLookahead :: Int
                                  -> ExferenceInput
                                  -> [ExferenceOutputElement]
findFirstBestExpressionsLookahead n = selectionResult
  . selectFirstBestExpressionsLookahead n
  . findExpressionsWithStats

selectFirstBestExpressionsLookahead
  :: Int
  -> [ExferenceChunkElement]
  -> SearchSelection [ExferenceOutputElement]
selectFirstBestExpressionsLookahead n =
  go Nothing maxBound maxPenalty []
 where
  go :: Maybe SearchStatus
    -> Int
    -> Penalty
    -> [ExferenceOutputElement]
    -> [ExferenceChunkElement]
    -> SearchSelection [ExferenceOutputElement]
  go status _ _ selected [] = SearchSelection status selected
  go status 0 _ selected _ = SearchSelection status selected
  go _ remaining rating selected (chunk : chunks) = inspect
    (Just $ chunkStatus chunk)
    remaining
    rating
    selected
    (chunkElements chunk)
    chunks

  inspect :: Maybe SearchStatus
    -> Int
    -> Penalty
    -> [ExferenceOutputElement]
    -> [ExferenceOutputElement]
    -> [ExferenceChunkElement]
    -> SearchSelection [ExferenceOutputElement]
  inspect status 0 _ selected _ _ = SearchSelection status selected
  inspect status remaining rating selected [] chunks =
    go status (remaining - 1) rating selected chunks
  inspect status remaining rating selected
      (candidate@(_, _, stats) : candidates) chunks
    | candidateRating <- exference_complexityRating stats
    = if
      | candidateRating < rating ->
          inspect status n candidateRating [candidate] candidates chunks
      | candidateRating == rating ->
          inspect status n rating (candidate : selected) candidates chunks
      | otherwise ->
          inspect status remaining rating selected candidates chunks

-- a combination of the return-multiple-if-same-rating and the
-- look-some-steps-ahead-for-better-solution functionalities.
-- for example,
-- [2,3,2,2,4,5,6,7] -> [2,2,2]
--  does not stop at 3, but looks ahead, then returns all the 2-rated solutions
findFirstBestExpressionsLookaheadPreferNoConstraints :: Int
                                                     -> ExferenceInput
                                                     -> [ExferenceOutputElement]
findFirstBestExpressionsLookaheadPreferNoConstraints n = selectionResult
  . selectFirstBestExpressionsLookaheadPreferNoConstraints n
  . findExpressionsWithStats

selectFirstBestExpressionsLookaheadPreferNoConstraints
  :: Int
  -> [ExferenceChunkElement]
  -> SearchSelection [ExferenceOutputElement]
selectFirstBestExpressionsLookaheadPreferNoConstraints n =
  go Nothing maxBound maxPenalty [] []
 where
  go :: Maybe SearchStatus
    -> Int
    -> Penalty
    -> [ExferenceOutputElement] -- solutions without constraints
    -> [ExferenceOutputElement] -- solution(s) with constraints
    -> [ExferenceChunkElement]
    -> SearchSelection [ExferenceOutputElement]
  -- out of potential solutions, nothing constraint-free found
  go status _ _ [] constrained [] = SearchSelection status constrained
  -- out of potential solutions, found good stuff
  go status _ _ unconstrained _ [] =
    SearchSelection status unconstrained
  -- out of lookahead, return what we have (unconstrained is nonempty)
  go status 0 _ unconstrained _ _ =
    SearchSelection status unconstrained
  go _ remaining rating unconstrained constrained (chunk : chunks) = inspect
    (Just $ chunkStatus chunk)
    remaining
    rating
    unconstrained
    constrained
    (chunkElements chunk)
    chunks

  inspect :: Maybe SearchStatus
    -> Int
    -> Penalty
    -> [ExferenceOutputElement]
    -> [ExferenceOutputElement]
    -> [ExferenceOutputElement]
    -> [ExferenceChunkElement]
    -> SearchSelection [ExferenceOutputElement]
  inspect status 0 _ unconstrained _ _ _ =
    SearchSelection status unconstrained
  -- Empty chunks count as lookahead only after a constraint-free result.
  inspect status remaining rating [] constrained [] chunks =
    go status remaining rating [] constrained chunks
  inspect status remaining rating unconstrained constrained [] chunks =
    go status (remaining - 1) rating unconstrained constrained chunks
  -- Finding a constraint-free result replaces the constrained fallback.
  inspect status remaining rating unconstrained _
      (candidate@(_, [], stats) : candidates) chunks
    | candidateRating <- exference_complexityRating stats
    = if
      | null unconstrained ->
          inspect status n candidateRating [candidate] [] candidates chunks
      | candidateRating < rating ->
          inspect status n candidateRating [candidate] [] candidates chunks
      | candidateRating == rating ->
          inspect status n rating
            (candidate : unconstrained) [] candidates chunks
      | otherwise ->
          inspect status remaining rating
            unconstrained [] candidates chunks
  -- Until then, retain the best constrained fallback.
  inspect status remaining rating [] constrained
      (candidate@(_, _, stats) : candidates) chunks
    | candidateRating <- exference_complexityRating stats
    = if
      | candidateRating < rating ->
          inspect status remaining candidateRating
            [] [candidate] candidates chunks
      | candidateRating == rating ->
          inspect status remaining rating
            [] (candidate : constrained) candidates chunks
      | otherwise ->
          inspect status remaining rating [] constrained candidates chunks
  -- Constrained results consume lookahead after a good result exists.
  inspect status remaining rating unconstrained _ (_ : candidates) chunks =
    inspect status (remaining - 1) rating
      unconstrained [] candidates chunks

-- like findSortNExpressions, but retains only the best rating
findBestNExpressions :: Int
                     -> ExferenceInput
                     -> [ExferenceOutputElement]
findBestNExpressions n = selectionResult
  . selectBestNExpressions n
  . findExpressionsWithStats

selectBestNExpressions
  :: Int
  -> [ExferenceChunkElement]
  -> SearchSelection [ExferenceOutputElement]
selectBestNExpressions n = mapSelection chooseBest
  . selectSortNExpressions n
 where
  chooseBest r = case r of
    [] -> []
    _  -> firstGroupBy (\(~(_, _, stats1)) (~(_, _, stats2)) ->
                              exference_complexityRating stats1
                           >= exference_complexityRating stats2)
                         r

takeSelection
  :: Int
  -> [ExferenceChunkElement]
  -> SearchSelection [ExferenceOutputElement]
takeSelection maximumCount
  | maximumCount <= 0 = const $ SearchSelection Nothing []
  | otherwise = go Nothing maximumCount []
 where
  go status _ reversed [] = SearchSelection status $ reverse reversed
  go _ remaining reversed (chunk : chunks) =
    let status = Just $ chunkStatus chunk
        taken = take remaining $ chunkElements chunk
        takenCount = length taken
        reversed' = List.foldl' (flip (:)) reversed taken
    in if takenCount == remaining
      then SearchSelection status $ reverse reversed'
      else go status (remaining - takenCount) reversed' chunks

firstGroupBy :: (a -> a -> Bool) -> [a] -> [a]
firstGroupBy _ [] = []
firstGroupBy relation values = case groupBy relation values of
  group : _ -> group
  [] -> []
