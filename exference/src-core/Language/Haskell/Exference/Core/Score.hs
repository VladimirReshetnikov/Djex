{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Haskell.Exference.Core.Score
  ( Penalty (..)
  , Priority (..)
  , isFinitePenalty
  , maxPenalty
  )
where

import Control.DeepSeq (NFData)
import Data.Data (Data)
import GHC.Generics (Generic)

-- | A non-negative search cost. Inputs are validated before search; the
-- constructor remains available for concise configuration and serialization.
newtype Penalty = Penalty { penaltyValue :: Double }
  deriving (Data, Eq, Fractional, NFData, Num, Ord, Real, RealFrac, Generic)

instance Show Penalty where
  show = show . penaltyValue

-- | An ordered queue priority. Larger priorities are explored first.
newtype Priority = Priority { priorityValue :: Double }
  deriving (Data, Eq, Fractional, NFData, Num, Ord, Real, RealFrac, Generic)

instance Show Priority where
  show = show . priorityValue

isFinitePenalty :: Penalty -> Bool
isFinitePenalty (Penalty value) = value >= 0 && not (isNaN value || isInfinite value)

maxPenalty :: Penalty
maxPenalty = Penalty 1.7976931348623157e308
