{-# LANGUAGE DeriveGeneric #-}

-- | Structural preferences for already admitted synthesis candidates.
--
-- These scores are heuristics, never typing or behavioral evidence. Ranking
-- retains whole candidate handles and never rewrites their output or refunds
-- search work. Callers must supply a finite, work-bounded collection.
module Language.Haskell.Synthesis.CandidateQuality
  ( CandidateRankingPolicy (..)
  , CandidateQualityWeights (..)
  , CandidateQuality (..)
  , defaultCandidateRankingPolicy
  , compactCandidateRankingPolicy
  , diverseCandidateRankingPolicy
  , parseCandidateRankingPolicy
  , candidateRankingPolicyName
  , candidateQuality
  , defaultCandidateProviderCost
  , candidateQualityCost
  , rankCandidatesByQuality
  , selectQualityQueryResults
  , selectQualityQueryResultsM
  ) where

import Control.DeepSeq (NFData)
import Data.List (minimumBy)
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Generated
  ( Expression (..), Pattern (..), VisibleTypeArgument )
import Language.Haskell.Synthesis.Name (Name, LexicalClass (..), nameLexicalClass)
import Language.Haskell.Synthesis.Query (QueryResult, resultSearch)
import Language.Haskell.Synthesis.Search
  ( SearchBatch (..), Progress (..), Completion (..), TruncationReason (..) )
import Language.Haskell.Synthesis.Selection (Selection (..))

-- | Legacy preserves the order supplied by the backend. Structural policies
-- use nonnegative, arbitrary-precision weights; zero disables a component.
data CandidateRankingPolicy
  = LegacyCandidateRanking
  | StructuralCandidateRanking CandidateQualityWeights
  deriving (Eq, Ord, Show, Generic)

instance NFData CandidateRankingPolicy

data CandidateQualityWeights = CandidateQualityWeights
  { candidateSizeWeight :: !Natural
  , candidateEliminationWeight :: !Natural
  , candidateProviderWeight :: !Natural
  , candidateDiversityWeight :: !Natural
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData CandidateQualityWeights

-- | Inspectable structural measurements. Let-bound expressions are counted
-- once, so the metric does not charge a shared computation per use. Provider
-- costs use exact qualified names supplied by the caller, not printed length.
data CandidateQuality = CandidateQuality
  { qualityTermSize :: !Natural
  , qualityEliminations :: !Natural
  , qualityProviderCost :: !Natural
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData CandidateQuality

defaultCandidateRankingPolicy :: CandidateRankingPolicy
defaultCandidateRankingPolicy =
  StructuralCandidateRanking $ CandidateQualityWeights 1 3 2 4

compactCandidateRankingPolicy :: CandidateRankingPolicy
compactCandidateRankingPolicy =
  StructuralCandidateRanking $ CandidateQualityWeights 1 1 1 0

diverseCandidateRankingPolicy :: CandidateRankingPolicy
diverseCandidateRankingPolicy =
  StructuralCandidateRanking $ CandidateQualityWeights 1 3 2 12

parseCandidateRankingPolicy :: String -> Either String CandidateRankingPolicy
parseCandidateRankingPolicy spelling = case spelling of
  "legacy" -> Right LegacyCandidateRanking
  "balanced" -> Right defaultCandidateRankingPolicy
  "compact" -> Right compactCandidateRankingPolicy
  "diverse" -> Right diverseCandidateRankingPolicy
  _ -> Left "expected legacy, balanced, compact, or diverse"

candidateRankingPolicyName :: CandidateRankingPolicy -> String
candidateRankingPolicyName policy
  | policy == LegacyCandidateRanking = "legacy"
  | policy == defaultCandidateRankingPolicy = "balanced"
  | policy == compactCandidateRankingPolicy = "compact"
  | policy == diverseCandidateRankingPolicy = "diverse"
  | otherwise = "custom"

-- | Structural constructors have no provider surcharge; a named value costs
-- one. A caller's exact-name overrides take precedence over this fallback.
defaultCandidateProviderCost :: Name -> Natural
defaultCandidateProviderCost name
  | nameLexicalClass name == ConstructorLike = 0
  | otherwise = 1

candidateQuality :: (Name -> Natural) -> Expression local -> CandidateQuality
candidateQuality providerCost = measure
 where
  measure expression = case expression of
    Local _ -> unit
    Global name -> CandidateQuality 1 0 (providerCost name)
    Hole _ -> unit
    -- Counting binder sites, rather than lambda constructors, makes grouped
    -- and successive lambdas equally expensive without erasing either form.
    Lambda patterns body -> addEliminations (sum $ map patternEliminations patterns)
      $ addSize (sum $ map patternSize patterns) $ measure body
    Apply function argument -> addSize 1 $ plus (measure function) (measure argument)
    VisibleTypeApplication function _ -> addSize 1 $ measure function
    Tuple elements -> addSize 1 $ combine $ map measure elements
    Let pattern binding body -> addEliminations (patternEliminations pattern)
      $ addSize (1 + patternSize pattern)
      $ plus (measure binding) (measure body)
    Case scrutinee alternatives ->
      let children = combine $ measure scrutinee : map (measure . snd) alternatives
          sites = sum $ map (patternSize . fst) alternatives
      in (addSize (1 + sites) children)
          { qualityEliminations = qualityEliminations children
              + 1 + fromIntegral (length alternatives) }
  unit = CandidateQuality 1 0 0
  combine = foldl' plus (CandidateQuality 0 0 0)
  plus (CandidateQuality a b c) (CandidateQuality x y z) =
    CandidateQuality (a + x) (b + y) (c + z)
  addSize n quality = quality { qualityTermSize = n + qualityTermSize quality }
  addEliminations n quality = quality
    { qualityEliminations = n + qualityEliminations quality }

patternSize :: Pattern local -> Natural
patternSize pattern = case pattern of
  Bind _ -> 1
  Wildcard -> 1
  Constructor _ fields -> 1 + sum (map patternSize fields)
  TuplePattern fields -> 1 + sum (map patternSize fields)
  As _ nested -> 1 + patternSize nested

patternEliminations :: Pattern local -> Natural
patternEliminations pattern = case pattern of
  Constructor _ fields -> 1 + sum (map patternEliminations fields)
  TuplePattern fields -> 1 + sum (map patternEliminations fields)
  As _ nested -> patternEliminations nested
  _ -> 0

candidateQualityCost
  :: CandidateRankingPolicy -> (Name -> Natural) -> Expression local -> Natural
candidateQualityCost LegacyCandidateRanking _ _ = 0
candidateQualityCost (StructuralCandidateRanking weights) providerCost source = measure source
 where
  -- Frontier scoring visits partial trees frequently. Compute the scalar
  -- directly rather than allocating three measurements at every syntax site.
  -- The inspectable candidateQuality API retains the same component counts.
  size = candidateSizeWeight weights
  elimination = candidateEliminationWeight weights
  provider = candidateProviderWeight weights
  measure expression = case expression of
    Local _ -> size
    Global name -> size + provider * providerCost name
    Hole _ -> size
    Lambda patterns body -> patternCosts True patterns + measure body
    Apply function argument -> size + measure function + measure argument
    VisibleTypeApplication function _ -> size + measure function
    Tuple elements -> size + foldl' (\cost element -> cost + measure element) 0 elements
    Let pattern binding body ->
      size + patternCost True pattern + measure binding + measure body
    Case scrutinee alternatives -> size + elimination + measure scrutinee
      + foldl' (\cost (pattern, body) -> cost + elimination
          + patternCost False pattern + measure body) 0 alternatives
  patternCosts eliminations =
    foldl' (\cost pattern -> cost + patternCost eliminations pattern) 0
  patternCost eliminations pattern = case pattern of
    Bind _ -> size
    Wildcard -> size
    Constructor _ fields -> destructuring fields
    TuplePattern fields -> destructuring fields
    As _ nested -> size + patternCost eliminations nested
   where
    destructuring fields = size + (if eliminations then elimination else 0)
      + patternCosts eliminations fields

-- | Stably order a finite collection before the caller's output quota.
-- Diversity adds a cost for each previously selected member of the same
-- structural family. It changes order only: no candidate or provider/type
-- assignment is discarded or equated. Equal costs retain encounter order.
-- The legacy path is the identity and does not inspect the candidate tail.
rankCandidatesByQuality
  :: Ord local
  => CandidateRankingPolicy
  -> (Name -> Natural)
  -> (candidate -> Expression local)
  -> [candidate]
  -> [candidate]
rankCandidatesByQuality LegacyCandidateRanking _ _ candidates = candidates
rankCandidatesByQuality policy@(StructuralCandidateRanking weights) providerCost project candidates =
  select Map.empty $ zipWith decorate [0 :: Int ..] candidates
 where
  decorate ordinal candidate =
    let expression = project candidate
    in (ordinal, candidateQualityCost policy providerCost expression,
        structuralFamily expression, candidate)
  select _ [] = []
  select counts remaining =
    let (ordinal, _, family, candidate) = minimumBy (comparing $ priority counts) remaining
        nextCounts = Map.insertWith (+) family (1 :: Natural) counts
    in candidate : select nextCounts
        [entry | entry@(index, _, _, _) <- remaining, index /= ordinal]
  priority counts (ordinal, cost, family, _) =
    ( cost + candidateDiversityWeight weights
        * Map.findWithDefault 0 family counts
    , ordinal )

-- | Observe at most the supplied number of backend candidates, then rank the
-- admitted pool before any output quota. Each inspected candidate consumes a
-- slot, including a rejected or duplicate candidate. The function does not
-- deduplicate: callers retain their exact identity/evidence-specific rule.
-- Empty batches still consume the backend's independently recorded work.
-- At the cap, the tail is not inspected even to test for exhaustion, so the
-- conservative progress is truncation, never negative logical evidence.
selectQualityQueryResults
  :: Ord local
  => Int
  -> CandidateRankingPolicy
  -> (Name -> Natural)
  -> (candidate -> Expression local)
  -> (candidate -> Bool)
  -> [QueryResult metadata candidate]
  -> Selection candidate
selectQualityQueryResults limit policy providerCost project admissible results =
  runIdentity $ selectQualityQueryResultsM limit policy providerCost project
    (Identity . admissible) results

-- | Effectful admission with the same raw observation bound. Checks run once
-- in encounter order; ranking retains their original candidate handles.
selectQualityQueryResultsM
  :: (Monad action, Ord local)
  => Int
  -> CandidateRankingPolicy
  -> (Name -> Natural)
  -> (candidate -> Expression local)
  -> (candidate -> action Bool)
  -> [QueryResult metadata candidate]
  -> action (Selection candidate)
selectQualityQueryResultsM limit policy providerCost project admissible =
  batches (max 0 limit) [] Nothing
 where
  finish reversed progress = pure $ Selection progress
    $ rankCandidatesByQuality policy providerCost project $ reverse reversed
  batches 0 reversed progress _ = finish reversed $ Just $ capped progress
  batches _ reversed progress [] = finish reversed progress
  batches remaining reversed _ (result : rest) =
    let batch = resultSearch result
    in candidates remaining reversed (Just $ batchProgress batch)
        (batchCandidates batch) rest
  candidates 0 reversed progress _ _ = finish reversed $ Just $ capped progress
  candidates remaining reversed progress [] rest =
    batches remaining reversed progress rest
  candidates remaining reversed progress (candidate : rest) results = do
    accepted <- admissible candidate
    candidates (remaining - 1)
      (if accepted then candidate : reversed else reversed) progress rest results
  capped (Just (Completed (Truncated reasons)))
    | CandidateLimitReached `elem` reasons = Completed $ Truncated reasons
    | otherwise = Completed $ Truncated $ reasons <> (CandidateLimitReached :| [])
  capped _ = Completed $ Truncated $ CandidateLimitReached :| []

-- A family describes dependencies and operations, ignoring their repetition.
-- Thus f x and f (f x) are related, while the two projections, distinct
-- constructors/providers, and explicit type choices remain different. This
-- deliberately coarse relation is NOT used for normalization or deduplication.
data StructuralFeature local
  = BoundInput Int
  | FreeInput local
  | GlobalInput Name
  | TypeChoice VisibleTypeArgument
  | ApplicationShape
  | LambdaShape
  | TupleShape Int
  | LetShape
  | CaseShape
  | PatternConstructor Name
  | HoleInput local
  deriving (Eq, Ord)

structuralFamily :: Ord local => Expression local -> Set.Set (StructuralFeature local)
structuralFamily = go [] 0
 where
  go scope next expression = case expression of
    Local local -> Set.singleton $ maybe (FreeInput local) BoundInput $ lookup local scope
    Global name -> Set.singleton $ GlobalInput name
    Hole local -> Set.singleton $ HoleInput local
    Lambda patterns body ->
      let (inner, after) = bindPatterns scope next patterns
      in Set.insert LambdaShape $ patternFeatures patterns `Set.union` go inner after body
    Apply function argument -> Set.insert ApplicationShape
      $ go scope next function `Set.union` go scope next argument
    VisibleTypeApplication function argument ->
      Set.insert (TypeChoice argument) $ go scope next function
    Tuple elements -> Set.insert (TupleShape $ length elements)
      $ Set.unions $ map (go scope next) elements
    Let pattern binding body ->
      let (inner, after) = bindPatterns scope next [pattern]
      in Set.insert LetShape $ Set.unions
          [patternFeatures [pattern], go scope next binding, go inner after body]
    Case scrutinee alternatives -> Set.insert CaseShape $ Set.unions
      (go scope next scrutinee :
        [ let (inner, after) = bindPatterns scope next [pattern]
          in patternFeatures [pattern] `Set.union` go inner after body
        | (pattern, body) <- alternatives ])
  bindPatterns scope next = foldl' bindPattern (scope, next)
  bindPattern (scope, next) pattern = case pattern of
    Bind local -> ((local, next) : scope, next + 1)
    Wildcard -> (scope, next + 1)
    Constructor _ fields -> bindPatterns scope next fields
    TuplePattern fields -> bindPatterns scope next fields
    As local nested -> bindPattern ((local, next) : scope, next + 1) nested
  patternFeatures = Set.unions . map features
  features pattern = case pattern of
    Constructor name fields -> Set.insert (PatternConstructor name) $ patternFeatures fields
    TuplePattern fields -> Set.insert (TupleShape $ length fields) $ patternFeatures fields
    As _ nested -> features nested
    _ -> Set.empty
