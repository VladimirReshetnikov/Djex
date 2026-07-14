-- | Exact collection counts and explicit compatibility projections.
--
-- Shared synthesis data uses 'Natural' whenever a count is intrinsically
-- non-negative and may outgrow a machine word. Historical APIs that expose
-- 'Int' cross that boundary through the saturating projection below rather
-- than relying on wrapping conversion.
module Language.Haskell.Synthesis.Count
  ( naturalLength
  , saturatingNaturalToInt
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
