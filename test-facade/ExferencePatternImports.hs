{-# LANGUAGE PatternSynonyms #-}

-- Compile guard for the explicit-import form required by the zero-copy stable
-- record views. This module imports only the curated facade, so the check pins
-- that module's self-contained public surface even though lower-level modules
-- now live in the same Cabal library.
module ExferencePatternImports (patternViewsRoundTrip) where

import Language.Haskell.Djex
  ( ExferenceBatchMetadata
  , pattern ExferenceBatchMetadata
  , ExferenceCandidateDetails
  , pattern ExferenceCandidateDetails
  , ExferenceCandidateMetrics
  , pattern ExferenceCandidateMetrics
  , exferenceBatchBindingUsages
  , exferenceBatchDepthPruned
  , exferenceBatchQueuePruned
  , exferenceCandidateComplexity
  , exferenceCandidateFinalQueueSize
  , exferenceCandidateLocalNames
  , exferenceCandidateStatistics
  , exferenceCandidateSteps
  , exferenceCandidateTypeVariableNames
  )

patternViewsRoundTrip :: Bool
patternViewsRoundTrip =
  exferenceCandidateSteps metrics == 3
    && exferenceCandidateComplexity metrics == 2
    && exferenceCandidateFinalQueueSize metrics == 5
    && exferenceCandidateStatistics details == metrics
    && exferenceCandidateLocalNames details == mempty
    && exferenceCandidateTypeVariableNames details == mempty
    && exferenceBatchBindingUsages metadata == mempty
    && exferenceBatchQueuePruned metadata == 7
    && exferenceBatchDepthPruned metadata == 11
    && case (details, metadata) of
      ( ExferenceCandidateDetails actualMetrics localNames typeNames
        , ExferenceBatchMetadata usages queuePruned depthPruned
        ) -> actualMetrics == metrics
          && localNames == mempty
          && typeNames == mempty
          && usages == mempty
          && queuePruned == 7
          && depthPruned == 11
 where
  metrics :: ExferenceCandidateMetrics
  metrics = ExferenceCandidateMetrics 3 2 5

  details :: ExferenceCandidateDetails
  details = ExferenceCandidateDetails metrics mempty mempty

  metadata :: ExferenceBatchMetadata
  metadata = ExferenceBatchMetadata mempty 7 11
