module Data.Map where

import qualified Data.Maybe (Maybe)
import qualified Data.Ord (Ord)


data Map k a

lookup :: Data.Ord.Ord k => k -> Map k a -> Data.Maybe.Maybe a
