module Language.Haskell.Exference.Core
  ( findExpressions
  , findExpressionsChunked
  , findExpressionsWithStats
  , findExpressionsWithStatsEither
  , findGeneratedSearchBatchesEither
  , findGeneratedSearchBatchesWithHintsEither
  , findGeneratedSearchBatchesInEnvironmentEither
  , findGeneratedSearchBatchesWithHintsInEnvironmentEither
  , E.findQueryResultsInEnvironmentEither
  , E.ExferenceHeuristicsConfig (..)
  , defaultHeuristicsConfig
  , E.ExferenceInput (..)
  , E.ExferenceEnvironment
  , E.ExferenceQuery (..)
  , E.ExferenceOutputElement
  , E.ExferenceChunkElement (..)
  , E.ExferenceBatchMetadata (..)
  , E.ExferenceSearchBatch
  , E.ExferenceGeneratedOutputElement
  , E.ExferenceGeneratedSearchBatch
  , E.ExferenceCandidate
  , E.ExferenceResult
  , C.ExferenceCandidateDetails (..)
  , C.ExferenceCandidateError (..)
  , C.ExferenceTypeVariableHints
  , C.ExferenceSourceTypeVariableHints
  , C.ExferenceSourceTypeVariableHintError (..)
  , C.ExferenceGeneratedCandidate
  , C.mkExferenceGeneratedCandidate
  , C.emptyExferenceSourceTypeVariableHints
  , C.mkExferenceSourceTypeVariableHints
  , C.typeVariableHints
  , E.typeVariableHintsInEnvironment
  , E.ExferenceProjectionError (..)
  , E.SearchCompletion (..)
  , E.SearchStatus (..)
  , E.SearchStatusError (..)
  , E.toSearchProgress
  , E.toSearchBatch
  , E.toGeneratedSearchBatch
  , E.toGeneratedSearchBatchWithHints
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
import qualified Data.Map.Strict as Map

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

-- | Validate the finite input eagerly, then expose the generated batch trace
-- lazily.  The empty hint map gives deterministic fallback names to one-shot
-- core callers; reusable frontends should seal an environment and use the
-- corresponding @InEnvironmentEither@ entry point below.
findGeneratedSearchBatchesEither
  :: E.ExferenceInput
  -> Either E.ExferenceInputError [E.ExferenceGeneratedSearchBatch]
findGeneratedSearchBatchesEither =
  findGeneratedSearchBatchesWithHintsEither Map.empty

findGeneratedSearchBatchesWithHintsEither
  :: C.ExferenceTypeVariableHints
  -> E.ExferenceInput
  -> Either E.ExferenceInputError [E.ExferenceGeneratedSearchBatch]
findGeneratedSearchBatchesWithHintsEither typeHints input = do
  checked <- E.prepareExferenceInput input
  pure $ E.findGeneratedSearchBatches typeHints checked

-- | Validate only a new query, then search a reusable sealed environment.
-- Environment validation happened once at 'E.mkExferenceEnvironment'.
findGeneratedSearchBatchesInEnvironmentEither
  :: E.ExferenceEnvironment
  -> E.ExferenceQuery
  -> Either E.ExferenceInputError [E.ExferenceGeneratedSearchBatch]
findGeneratedSearchBatchesInEnvironmentEither =
  findGeneratedSearchBatchesWithHintsInEnvironmentEither Map.empty

findGeneratedSearchBatchesWithHintsInEnvironmentEither
  :: C.ExferenceTypeVariableHints
  -> E.ExferenceEnvironment
  -> E.ExferenceQuery
  -> Either E.ExferenceInputError [E.ExferenceGeneratedSearchBatch]
findGeneratedSearchBatchesWithHintsInEnvironmentEither
    typeHints environment query = do
  checked <- E.prepareExferenceQuery environment query
  pure $ E.findGeneratedSearchBatches typeHints checked

-- Keep validation at the public boundary and run it exactly once. The raw
-- engine assumes a checked input; list-returning compatibility functions
-- deliberately preserve their historical "invalid means empty" behavior.
runSearch
  :: E.ExferenceInput
  -> Either E.ExferenceInputError [E.ExferenceChunkElement]
runSearch input = E.findExpressions <$> E.prepareExferenceInput input
