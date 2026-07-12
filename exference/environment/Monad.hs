module Control.Monad where




class Control.Applicative.Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b

class (Control.Applicative.Alternative m, Monad m) => MonadPlus m where
  -- mzero :: m a -- this is a critical case: too generic, yet we might need it
                  -- at times
  -- more specific than (<|>)
  -- mplus :: m a -> m a -> m a

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
join :: Monad m => m (m a) -> m a
msum :: (Foldable t, MonadPlus m) => t (m a) -> m a
mfilter :: MonadPlus m => (a -> Data.Bool.Bool) -> m a -> m a 
zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c] 
foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b 
forever :: Monad m => m () -> m Data.Void.Void
(>>) :: Monad m => m () -> m b -> m b

instance Monad []
instance Monad Text.ParserCombinators.ReadP.ReadP
instance Monad Text.ParserCombinators.ReadPrec.ReadPrec
instance Monad Control.Concurrent.STM.STM
-- instance Monad ((->) r)
instance Monad (Control.Monad.ST.ST s)
-- instance Monad (Proxy *)
instance ArrowApply a => Monad (Control.Arrow.ArrowMonad a)
instance Monad m => Monad (Control.Applicative.WrappedMonad m)
