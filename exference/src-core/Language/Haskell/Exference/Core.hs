module Language.Haskell.Exference.Core
  ( findExpressions
  , findExpressionsChunked
  , findExpressionsWithStats
  , findExpressionsWithStatsEither
  , E.findQueryResultsInEnvironmentEither
  , E.ExferenceHeuristicsConfig (..)
  , defaultHeuristicsConfig
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
  , E.mkExferenceEnvironment
  , E.validateExferenceQuery
  , E.validateExferenceInput
  , findExpressionsEither
  , Score.Penalty (..)
  , Score.Priority (..)
  )
where

import qualified Language.Haskell.Exference.Core.Internal.Exference as E
import qualified Language.Haskell.Exference.Core.Candidate as C
import qualified Language.Haskell.Exference.Core.Score as Score

-- | Parser-neutral heuristic defaults for reusable core and stable queries.
-- The historical command-line frontend intentionally keeps its own ranking
-- profile, which is tuned for that interactive presentation boundary.
defaultHeuristicsConfig :: E.ExferenceHeuristicsConfig
defaultHeuristicsConfig = E.ExferenceHeuristicsConfig
  { E.heuristics_goalVar = Score.Penalty 4.0
  , E.heuristics_goalCons = Score.Penalty 0.55
  , E.heuristics_goalArrow = Score.Penalty 5.0
  , E.heuristics_goalApp = Score.Penalty 1.9
  , E.heuristics_stepProvidedGood = Score.Penalty 0.2
  , E.heuristics_stepProvidedBad = Score.Penalty 5.0
  , E.heuristics_stepEnvGood = Score.Penalty 6.0
  , E.heuristics_stepEnvBad = Score.Penalty 22.0
  , E.heuristics_tempUnusedVarPenalty = Score.Penalty 5.0
  , E.heuristics_tempMultiVarUsePenalty = Score.Penalty 3.0
  , E.heuristics_functionGoalTransform = Score.Penalty 0.0
  , E.heuristics_unusedVar = Score.Penalty 20.0
  , E.heuristics_solutionLength = Score.Penalty 0.0153
  }

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
