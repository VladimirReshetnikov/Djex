module Control.Monad.Trans.Class where

import qualified Control.Monad (Monad)


class MonadTrans t where
    lift :: (Control.Monad.Monad m) => m a -> t m a
