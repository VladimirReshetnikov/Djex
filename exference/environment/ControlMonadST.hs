module Control.Monad.ST where

-- ST is abstract to source clients, so model its kind and identity without a
-- synthesizable constructor.
data ST s a
