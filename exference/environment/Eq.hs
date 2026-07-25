module Data.Eq where

import qualified Data.Bool (Bool)
import qualified Data.Monoid (Dual, First, Last, Product, Sum)
import GHC.Generics (K1)


class Eq a where
  (==) :: a -> a -> Data.Bool.Bool
  (/=) :: a -> a -> Data.Bool.Bool

instance Eq a => Eq [a]
-- instance Eq p => Eq (Par1 p)
instance Eq a => Eq (Data.Monoid.Last a)
instance Eq a => Eq (Data.Monoid.First a)
instance Eq a => Eq (Data.Monoid.Product a)
instance Eq a => Eq (Data.Monoid.Sum a)
instance Eq a => Eq (Data.Monoid.Dual a)
-- instance Eq (f p) => Eq (Rec1 f p)
instance (Eq a, Eq b) => Eq (a, b)
instance Eq c => Eq (K1 i c p)
-- instance (Eq (f p), Eq (g p)) => Eq ((:+:) f g p)
-- instance (Eq (f p), Eq (g p)) => Eq ((:*:) f g p)
-- instance Eq (f (g p)) => Eq ((:.:) f g p)
instance (Eq a, Eq b, Eq c) => Eq (a, b, c)
-- instance Eq ((:~:) k a b)
-- instance Eq (f p) => Eq (M1 i c f p)
instance (Eq a, Eq b, Eq c, Eq d) => Eq (a, b, c, d)
instance (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq (a, b, c, d, e)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) => Eq (a, b, c, d, e, f)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) => Eq (a, b, c, d, e, f, g)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) => Eq (a, b, c, d, e, f, g, h)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) => Eq (a, b, c, d, e, f, g, h, i)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) => Eq (a, b, c, d, e, f, g, h, i, j)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) => Eq (a, b, c, d, e, f, g, h, i, j, k)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l) => Eq (a, b, c, d, e, f, g, h, i, j, k, l)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m) => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n) => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o) => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
