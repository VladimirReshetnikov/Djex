module Data.Traversable where

import qualified Control.Applicative (Applicative)
import qualified Data.Foldable (Foldable)
import qualified Data.Functor (Functor)
import qualified Data.Functor.Const (Const)
import qualified Data.Functor.Identity (Identity)


class (Data.Functor.Functor t, Data.Foldable.Foldable t) => Traversable t where
  traverse :: Control.Applicative.Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Control.Applicative.Applicative f => t (f a) -> f (t a)
  -- mapM :: Control.Monad.Monad m => (a -> m b) -> t a -> m (t b)
  -- sequence :: Control.Monad.Monad m => t (m a) -> m (t a)

instance Traversable []
instance Traversable Data.Functor.Identity.Identity
-- instance Traversable (Data.Either.Either a)  -- cause "unused" problems
-- instance Traversable ((,) a)     -- cause "unused" problems
-- instance Traversable (Proxy *)
instance Traversable (Data.Functor.Const.Const m)
