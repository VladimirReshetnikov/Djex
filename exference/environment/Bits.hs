module Data.Bits where

import qualified Data.Eq (Eq)


class Data.Eq.Eq a => Bits a where
class Bits b => FiniteBits b where
