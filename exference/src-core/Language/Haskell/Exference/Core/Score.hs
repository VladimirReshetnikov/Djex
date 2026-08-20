{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Exference's numeric score carriers: the signed t'Penalty' used for
-- heuristics, ratings, and depth limits, and the queue-key t'Priority'.
-- Both wrap a raw 'Double' with a total 'Ord' even for NaN, and the
-- exported arithmetic saturates within the finite domain, so search never
-- has to reason about overflow or a broken queue ordering; the finiteness
-- predicates are what checked boundaries use to reject malformed input.
module Language.Haskell.Exference.Core.Score
  ( Penalty (..)
  , Priority (..)
  , isFiniteScore
  , isFinitePenalty
  , isFinitePriority
  , addScore
  , multiplyScore
  , negateScore
  , sumScores
  , addPriority
  , normalizePenalty
  , normalizePriority
  , priorityFromPenalty
  , maxPenalty
  )
where

import Control.DeepSeq (NFData)
import Data.Data (Data)
import qualified Data.Foldable as Foldable
import GHC.Generics (Generic)

-- | Exference's compatibility score carrier. Query heuristics and depth
-- limits are validated as non-negative penalties, while environment binding
-- ratings deliberately remain signed. The public 'Num' and 'Fractional'
-- instances retain their historical raw-'Double' behavior so input validation
-- can still reject NaN and infinities; search uses the explicit saturating
-- operations below. 'Real' and 'RealFrac' are deliberately absent: their
-- inherited conversions are partial for the non-finite values this public
-- compatibility constructor accepts.
--
-- The constructor remains public for compatibility and diagnostics. Checked
-- boundaries inspect its raw value, while ordering is total even for NaN.
newtype Penalty = Penalty { penaltyValue :: Double }
  deriving (Data, Fractional, NFData, Num, Generic)

instance Eq Penalty where
  Penalty left == Penalty right = scoreValuesEqual left right

instance Ord Penalty where
  compare (Penalty left) (Penalty right) =
    compareScoreValues GT left right

instance Show Penalty where
  show = show . penaltyValue

-- | An ordered queue priority. Larger priorities are explored first. A raw
-- NaN sorts below every ordinary priority, so even caller-constructed
-- compatibility values retain a lawful total order.
newtype Priority = Priority { priorityValue :: Double }
  deriving (Data, Fractional, NFData, Num, Generic)

instance Eq Priority where
  Priority left == Priority right = scoreValuesEqual left right

instance Ord Priority where
  compare (Priority left) (Priority right) =
    compareScoreValues LT left right

instance Show Priority where
  show = show . priorityValue

-- | Whether a compatibility score has a finite raw representation. Unlike
-- 'isFinitePenalty', signed binding ratings are accepted here.
isFiniteScore :: Penalty -> Bool
isFiniteScore (Penalty value) = not $ isNaN value || isInfinite value

-- | Whether a value is a valid penalty: finite and non-negative.  This is
-- the check applied to query heuristics and depth limits at the checked
-- search boundary; use 'isFiniteScore' where signed ratings are allowed.
isFinitePenalty :: Penalty -> Bool
isFinitePenalty score = penaltyValue score >= 0 && isFiniteScore score

-- | Whether a priority has a finite raw representation (neither NaN nor an
-- infinity); the sign is unconstrained.
isFinitePriority :: Priority -> Bool
isFinitePriority (Priority value) = not $ isNaN value || isInfinite value

-- | Saturating addition in the finite signed score domain.
addScore :: Penalty -> Penalty -> Penalty
addScore = penaltyBinary (+)

-- | Saturating multiplication in the finite signed score domain.
multiplyScore :: Penalty -> Penalty -> Penalty
multiplyScore = penaltyBinary (*)

-- | Saturating negation in the finite signed score domain.
negateScore :: Penalty -> Penalty
negateScore = penaltyUnary negate

-- | Strict saturating accumulation. This is used instead of 'sum' anywhere a
-- finite input collection may exceed @Double@'s representable range.
sumScores :: Foldable collection => collection Penalty -> Penalty
sumScores = Foldable.foldl' addScore 0

-- | Saturating addition for queue keys.
addPriority :: Priority -> Priority -> Priority
addPriority = priorityBinary (+)

-- | Clamp a possibly malformed compatibility value into the finite signed
-- score domain. NaN is the worst non-negative penalty; infinities saturate in
-- their respective direction.
normalizePenalty :: Penalty -> Penalty
normalizePenalty = saturatingPenalty . penaltyValue

-- | Clamp a queue key into the finite priority domain. NaN sinks to the lowest
-- priority rather than being allowed to violate the queue's 'Ord' invariant.
normalizePriority :: Priority -> Priority
normalizePriority = saturatingPriority . priorityValue

-- | Preserve a finite signed score while crossing into the queue-key domain.
-- A malformed raw score is conservatively assigned the lowest priority.
priorityFromPenalty :: Penalty -> Priority
priorityFromPenalty = saturatingPriority . penaltyValue

-- | The largest finite penalty, i.e. the value at which the saturating
-- operations ('addScore', 'multiplyScore', 'normalizePenalty') clamp
-- overflow and to which 'normalizePenalty' maps NaN.
maxPenalty :: Penalty
maxPenalty = Penalty maxFiniteDouble

penaltyUnary :: (Double -> Double) -> Penalty -> Penalty
penaltyUnary operation = saturatingPenalty
  . operation . normalizedPenaltyValue

penaltyBinary
  :: (Double -> Double -> Double)
  -> Penalty
  -> Penalty
  -> Penalty
penaltyBinary operation left right = saturatingPenalty
  $ operation (normalizedPenaltyValue left) (normalizedPenaltyValue right)

priorityBinary
  :: (Double -> Double -> Double)
  -> Priority
  -> Priority
  -> Priority
priorityBinary operation left right = saturatingPriority
  $ operation (normalizedPriorityValue left) (normalizedPriorityValue right)

saturatingPenalty :: Double -> Penalty
saturatingPenalty = Penalty . boundedValue maxFiniteDouble

saturatingPriority :: Double -> Priority
saturatingPriority = Priority . boundedValue minFiniteDouble

normalizedPenaltyValue :: Penalty -> Double
normalizedPenaltyValue = boundedValue maxFiniteDouble . penaltyValue

normalizedPriorityValue :: Priority -> Double
normalizedPriorityValue = boundedValue minFiniteDouble . priorityValue

boundedValue :: Double -> Double -> Double
boundedValue nanValue value
  | isNaN value = nanValue
  | value > maxFiniteDouble = maxFiniteDouble
  | value < minFiniteDouble = minFiniteDouble
  | otherwise = value

maxFiniteDouble :: Double
maxFiniteDouble = 1.7976931348623157e308

minFiniteDouble :: Double
minFiniteDouble = negate maxFiniteDouble

scoreValuesEqual :: Double -> Double -> Bool
scoreValuesEqual left right = left == right || isNaN left && isNaN right

-- The supplied ordering places a NaN on the left of an ordinary value.
-- Penalties treat it as worst (GT), while priorities sink it (LT).
compareScoreValues :: Ordering -> Double -> Double -> Ordering
compareScoreValues nanOrdering left right
  | isNaN left = if isNaN right then EQ else nanOrdering
  | isNaN right = invertOrdering nanOrdering
  | otherwise = compare left right

invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering EQ = EQ
invertOrdering GT = LT
