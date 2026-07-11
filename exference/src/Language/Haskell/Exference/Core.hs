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
findExpressions input = either (const [])
  (const $ concatMap E.chunkElements $ E.findExpressions input)
  (E.validateExferenceInput input)

findExpressionsEither
  :: E.ExferenceInput
  -> Either E.ExferenceInputError [E.ExferenceOutputElement]
findExpressionsEither input = do
  E.validateExferenceInput input
  pure $ findExpressions input

findExpressionsChunked :: E.ExferenceInput
                   -> [[E.ExferenceOutputElement]]
findExpressionsChunked input = either (const [])
  (const $ map E.chunkElements $ E.findExpressions input)
  (E.validateExferenceInput input)

findExpressionsWithStats :: E.ExferenceInput
                         -> [E.ExferenceChunkElement]
findExpressionsWithStats input = either (const [])
  (const $ E.findExpressions input)
  (E.validateExferenceInput input)
