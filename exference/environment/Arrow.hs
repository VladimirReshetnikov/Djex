module Control.Arrow where

import qualified Control.Category (Category)
-- Instance-only SOURCE imports keep the curated module graph acyclic.
import {-# SOURCE #-} qualified Control.Monad (Monad)


class Control.Category.Category a => Arrow a where
  arr :: (b -> c) -> a b c
  first :: a b c -> a (b, d) (c, d)
  second :: a b c -> a (d, b) (d, c)
  (***) :: a b c -> a b' c' -> a (b, b') (c, c')
  (&&&) :: a b c -> a b c' -> a b (c, c')

instance Control.Monad.Monad m => Arrow (Kleisli m)

newtype Kleisli m a b = Kleisli (a -> m b)
newtype ArrowMonad a b = ArrowMonad (a () b)

class Arrow a => ArrowApply a where
  app :: a (a b c, b) c

class Arrow a => ArrowZero a where
  zeroArrow :: a b c

class ArrowZero a => ArrowPlus a where
  (<+>) :: a b c -> a b c -> a b c

-- TODO: there is more stuff in base:Control.Arrow
