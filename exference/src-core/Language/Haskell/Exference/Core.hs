module Language.Haskell.Exference.Core
  ( findExpressions
  , findExpressionsChunked
  , findExpressionsWithStats
  , E.ExferenceHeuristicsConfig (..)
  , E.ExferenceInput (..)
  , E.ExferenceOutputElement
  , E.ExferenceChunkElement (..)
  , E.SearchCompletion (..)
  , E.SearchStatus (..)
  , E.constraintsRelaxedAtStep
  , E.ExferenceInputError (..)
  , E.validateExferenceInput
  , findExpressionsEither
  , Score.Penalty (..)
  , Score.Priority (..)
  )
where



import qualified Language.Haskell.Exference.Core.Internal.Exference as E
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
findExpressionsWithStats = either (const []) id . runSearch

-- Keep validation at the public boundary and run it exactly once. The raw
-- engine assumes a checked input; list-returning compatibility functions
-- deliberately preserve their historical "invalid means empty" behavior.
runSearch
  :: E.ExferenceInput
  -> Either E.ExferenceInputError [E.ExferenceChunkElement]
runSearch input = E.validateExferenceInput input >> pure (E.findExpressions input)
