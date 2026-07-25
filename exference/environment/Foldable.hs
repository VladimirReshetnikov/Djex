module Data.Foldable where

import qualified Control.Applicative (Alternative)
import qualified Data.Functor.Const (Const)
import qualified Data.Functor.Identity (Identity)
import qualified Data.Monoid (Monoid)


class Foldable t where
  fold :: Data.Monoid.Monoid m => t m -> m
  foldMap :: Data.Monoid.Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b

asum :: (Foldable t, Control.Applicative.Alternative f)
     => t (f a)
     -> f a 

instance Foldable []
instance Foldable Data.Functor.Identity.Identity
instance Foldable ((,) a)
-- instance Foldable (Proxy *)
instance Foldable (Data.Functor.Const.Const m)
