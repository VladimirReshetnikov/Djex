{-# LANGUAGE DeriveGeneric #-}

module Language.Haskell.Exference.Core.ExferenceStats
  ( ExferenceStats (..)
  , BindingUsages
  , ExferenceBatchMetadata (..)
  )
where

import Control.DeepSeq (NFData)
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Language.Haskell.Exference.Core.Name (QualifiedName)
import Language.Haskell.Exference.Core.Score

-- | Exact cumulative environment-use counts, keyed by nominal binding
-- identity. Cumulative operational totals have no semantic machine-word
-- bound, so they use 'Natural' rather than a wrapping counter.
-- Rendering belongs at presentation boundaries, not in the search core.
type BindingUsages = M.Map QualifiedName Natural

-- | Lossless operational metadata for one Exference search batch.  Pruning
-- counts remain visible on continuing batches, where shared
-- 'Language.Haskell.Synthesis.Search.Progress' has no
-- terminal truncation reasons yet.
data ExferenceBatchMetadata = ExferenceBatchMetadata
  { exferenceBindingUsages :: BindingUsages
  , exferenceQueuePruned :: Natural
  , exferenceDepthPruned :: Natural
  }
  deriving (Eq, Show, Generic)

instance NFData ExferenceBatchMetadata

data ExferenceStats = ExferenceStats
  { exference_steps :: Int
  , exference_complexityRating :: Penalty
  , exference_finalSize :: Int
  }
  deriving (Eq, Show, Generic)

instance NFData ExferenceStats
