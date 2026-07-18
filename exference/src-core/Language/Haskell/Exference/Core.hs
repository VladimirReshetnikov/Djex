module Language.Haskell.Exference.Core
  ( findExpressions
  , findExpressionsChunked
  , findExpressionsChunkedEither
  , findExpressionsWithStats
  , findExpressionsWithStatsEither
  , E.findQueryResultsInEnvironmentEither
  , O.ExferenceHeuristicsConfig (..)
  , O.defaultHeuristicsConfig
  , O.ExferenceOptions (..)
  , O.defaultExferenceOptions
  , E.ExferenceInput (..)
  , E.ExferenceEnvironment
  , E.ExferenceQuery (..)
  , E.ExferenceOutputElement
  , E.ExferenceChunkElement (..)
  , E.ExferenceBatchMetadata (..)
  , C.ExferenceCandidate
  , E.ExferenceResult
  , C.ExferenceCandidateDetails (..)
  , C.ExferenceTypeVariableHints
  , C.ExferenceSourceTypeVariableHints
  , C.ExferenceSourceTypeVariableHintError (..)
  , C.emptyExferenceSourceTypeVariableHints
  , C.mkExferenceSourceTypeVariableHints
  , E.SearchCompletion (..)
  , E.SearchStatus (..)
  , E.constraintsRelaxedAtStep
  , E.ExferenceInputError (..)
  , E.isExferenceOptionError
  , E.mkExferenceEnvironment
  , E.validateExferenceQuery
  , E.validateExferenceInput
  , findExpressionsEither
  , Score.Penalty (..)
  , Score.Priority (..)
  )
where

import qualified Language.Haskell.Exference.Core.Internal.Exference as E
import qualified Language.Haskell.Exference.Core.Internal.Options as O
import qualified Language.Haskell.Exference.Core.Candidate as C
import qualified Language.Haskell.Exference.Core.Score as Score

-- These compatibility projections predate the checked error boundary and
-- make malformed input observationally identical to a valid empty search.
-- Retain them for source compatibility, but direct every new caller to an
-- 'Either'-returning operation.
{-# DEPRECATED findExpressions, findExpressionsChunked, findExpressionsWithStats
  "These compatibility functions discard ExferenceInputError; use findExpressionsEither or findExpressionsWithStatsEither." #-}

findExpressions :: E.ExferenceInput -> [E.ExferenceOutputElement]
findExpressions = either (const []) id . findExpressionsEither

findExpressionsEither
  :: E.ExferenceInput
  -> Either E.ExferenceInputError [E.ExferenceOutputElement]
findExpressionsEither input = do
  chunks <- runSearch input
  pure $ concatMap E.chunkElements chunks

findExpressionsChunked :: E.ExferenceInput
                   -> [[E.ExferenceOutputElement]]
findExpressionsChunked = either (const []) id . findExpressionsChunkedEither

-- | Validate an input and retain the historical chunk grouping without
-- erasing the exact 'E.ExferenceInputError'.
findExpressionsChunkedEither
  :: E.ExferenceInput
  -> Either E.ExferenceInputError [[E.ExferenceOutputElement]]
findExpressionsChunkedEither = fmap (map E.chunkElements) . runSearch

findExpressionsWithStats :: E.ExferenceInput
                         -> [E.ExferenceChunkElement]
findExpressionsWithStats = either (const []) id . findExpressionsWithStatsEither

-- | Validate an input without discarding either the error or the operational
-- completion/pruning information carried by its chunks.
findExpressionsWithStatsEither
  :: E.ExferenceInput
  -> Either E.ExferenceInputError [E.ExferenceChunkElement]
findExpressionsWithStatsEither = runSearch

-- Keep validation at the public boundary and run it exactly once. The raw
-- engine assumes a checked input; list-returning compatibility functions
-- deliberately preserve their historical "invalid means empty" behavior.
runSearch
  :: E.ExferenceInput
  -> Either E.ExferenceInputError [E.ExferenceChunkElement]
runSearch input = E.findExpressions <$> E.prepareExferenceInput input
