-- | The public facade of the Exference search core.  It re-exports the input,
-- environment, query, option, candidate, and result vocabulary of the
-- internal engine and offers the historical @findExpressions*@ entry points,
-- each validating its 'E.ExferenceInput' exactly once through
-- 'E.prepareExferenceInput' before the raw lazy search runs.  The
-- list-returning variants are deprecated: prefer the @Either@ forms.
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

import Data.Either (fromRight)
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

-- | Run a search and return every solution in discovery order.  A malformed
-- input yields the empty list, indistinguishable from a search without
-- results; prefer 'findExpressionsEither'.
findExpressions :: E.ExferenceInput -> [E.ExferenceOutputElement]
findExpressions = fromRight [] . findExpressionsEither

-- | Validate an input once and run the search, returning every solution in
-- discovery order (all chunks concatenated) or the exact
-- 'E.ExferenceInputError'.
findExpressionsEither
  :: E.ExferenceInput
  -> Either E.ExferenceInputError [E.ExferenceOutputElement]
findExpressionsEither input = do
  chunks <- runSearch input
  pure $ concatMap E.chunkElements chunks

-- | Run a search and return its solutions grouped by the search step that
-- produced them (many groups are empty).  A malformed input yields the empty
-- list; prefer 'findExpressionsChunkedEither'.
findExpressionsChunked :: E.ExferenceInput
                   -> [[E.ExferenceOutputElement]]
findExpressionsChunked = fromRight [] . findExpressionsChunkedEither

-- | Validate an input and retain the historical chunk grouping without
-- erasing the exact 'E.ExferenceInputError'.
findExpressionsChunkedEither
  :: E.ExferenceInput
  -> Either E.ExferenceInputError [[E.ExferenceOutputElement]]
findExpressionsChunkedEither = fmap (map E.chunkElements) . runSearch

-- | Run a search and return one 'E.ExferenceChunkElement' per search step,
-- carrying the search status, cumulative binding usages, and that step's
-- solutions.  A malformed input yields the empty list; prefer
-- 'findExpressionsWithStatsEither'.
findExpressionsWithStats :: E.ExferenceInput
                         -> [E.ExferenceChunkElement]
findExpressionsWithStats = fromRight [] . findExpressionsWithStatsEither

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
