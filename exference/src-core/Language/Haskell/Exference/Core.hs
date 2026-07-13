module Language.Haskell.Exference.Core
  ( findExpressions
  , findExpressionsChunked
  , findExpressionsWithStats
  , findExpressionsWithStatsEither
  , findGeneratedSearchBatchesEither
  , findGeneratedSearchBatchesWithHintsEither
  , findGeneratedSearchBatchesInEnvironmentEither
  , findGeneratedSearchBatchesWithHintsInEnvironmentEither
  , E.ExferenceHeuristicsConfig (..)
  , E.ExferenceInput (..)
  , E.ExferenceEnvironment
  , E.ExferenceQuery (..)
  , E.ExferenceOutputElement
  , E.ExferenceChunkElement (..)
  , E.ExferenceBatchMetadata (..)
  , E.ExferenceSearchBatch
  , E.ExferenceGeneratedOutputElement
  , E.ExferenceGeneratedSearchBatch
  , C.ExferenceCandidateDetails (..)
  , C.ExferenceCandidateError (..)
  , C.ExferenceTypeVariableHints
  , C.ExferenceGeneratedCandidate
  , C.mkExferenceGeneratedCandidate
  , C.typeVariableHints
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
  E.validateExferenceInput input
  pure $ E.findGeneratedSearchBatches typeHints input

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
  E.validateExferenceQuery environment query
  pure $ E.findGeneratedSearchBatchesInEnvironment
    typeHints environment query

-- Keep validation at the public boundary and run it exactly once. The raw
-- engine assumes a checked input; list-returning compatibility functions
-- deliberately preserve their historical "invalid means empty" behavior.
runSearch
  :: E.ExferenceInput
  -> Either E.ExferenceInputError [E.ExferenceChunkElement]
runSearch input = E.validateExferenceInput input >> pure (E.findExpressions input)
