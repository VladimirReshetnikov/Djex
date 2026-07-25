module Control.Monad.IO.Class where

import qualified Control.Monad (Monad)
import qualified System.IO (IO)


class Control.Monad.Monad m => MonadIO m where
  liftIO :: System.IO.IO a -> m a 
