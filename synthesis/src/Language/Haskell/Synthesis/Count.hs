{-# LANGUAGE BangPatterns #-}

-- | Exact collection counts, explicit compatibility projections, and the
-- small bounded-arithmetic helpers the semantic layer's limit checks share.
--
-- Shared synthesis data uses 'Natural' whenever a count is intrinsically
-- non-negative and may outgrow a machine word. Historical APIs that expose
-- 'Int' cross that boundary through the saturating projection below rather
-- than relying on wrapping conversion.
module Language.Haskell.Synthesis.Count
  ( naturalLength
  , saturatingNaturalToInt
  , saturatedSuccessor
  , observedNaturalBits
  ) where

import qualified Data.Foldable as Foldable
import Numeric.Natural (Natural)

-- | Count a finite foldable structure strictly and without machine-sized
-- overflow.
naturalLength :: Foldable collection => collection value -> Natural
naturalLength = Foldable.foldl' (\count _ -> count + 1) 0

-- | Project an exact non-negative count into a compatibility 'Int',
-- saturating at the largest representable value.
saturatingNaturalToInt :: Natural -> Int
saturatingNaturalToInt = fromIntegral . min maximumIntNatural
 where
  maximumIntNatural = fromIntegral (maxBound :: Int)

-- | The successor of an 'Int' bound, saturating at 'maxBound' so a
-- maximum-plus-one observation can never wrap.
saturatedSuccessor :: Int -> Int
saturatedSuccessor value
  | value == maxBound = maxBound
  | otherwise = value + 1

-- | The bit width of a 'Natural', observed productively: counting stops at
-- @maximumBits@ and reports @maximumBits + 1@ (saturating), so an
-- unbounded value costs at most a bounded number of halvings.
observedNaturalBits :: Int -> Natural -> Int
observedNaturalBits maximumBits = go 0
 where
  bound = max 0 maximumBits

  go !observed 0 = observed
  go !observed remaining
    | observed >= bound = saturatedSuccessor bound
    | otherwise = go (observed + 1) $ remaining `quot` 2
