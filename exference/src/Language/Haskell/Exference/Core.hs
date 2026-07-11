module Language.Haskell.Exference.Core
  ( findExpressions
  , findExpressionsChunked
  , findExpressionsWithStats
  , E.ExferenceHeuristicsConfig (..)
  , E.ExferenceInput (..)
  , E.ExferenceOutputElement
  , E.ExferenceChunkElement (..)
  )
where



import qualified Language.Haskell.Exference.Core.Internal.Exference as E



findExpressions :: E.ExferenceInput -> [E.ExferenceOutputElement]
findExpressions = concatMap E.chunkElements . E.findExpressions

findExpressionsChunked :: E.ExferenceInput
                   -> [[E.ExferenceOutputElement]]
findExpressionsChunked = map E.chunkElements . E.findExpressions

findExpressionsWithStats :: E.ExferenceInput
                         -> [E.ExferenceChunkElement]
findExpressionsWithStats = E.findExpressions
