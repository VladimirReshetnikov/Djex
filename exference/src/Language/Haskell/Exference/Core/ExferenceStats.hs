module Language.Haskell.Exference.Core.ExferenceStats
  ( ExferenceStats (..)
  , BindingUsages
  )
where

import Data.Map.Strict as M
import Language.Haskell.Exference.Core.Score
type BindingUsages = M.Map String Int

data ExferenceStats = ExferenceStats
  { exference_steps :: Int
  , exference_complexityRating :: Penalty
  , exference_finalSize :: Int
  }
  deriving (Show, Eq)
