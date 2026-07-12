module Control.Concurrent.STM where

-- Opaque declaration: the environment needs nominal identity for standard
-- instances, but should not synthesize STM internals as constructors.
data STM a
