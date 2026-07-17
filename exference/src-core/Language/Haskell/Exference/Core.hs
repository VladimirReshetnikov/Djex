module Language.Haskell.Exference.Core
  ( findExpressions
  , findExpressionsChunked
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
findExpressionsChunked = either (const []) (map E.chunkElements) . runSearch

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
