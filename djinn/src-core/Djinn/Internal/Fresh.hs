{-# LANGUAGE BangPatterns #-}

-- | Collision-free allocation shared by Djinn's private name namespaces.
module Djinn.Internal.Fresh (allocateFresh) where

import qualified Data.Set as Set

-- | Select the first unused name from a deterministic candidate sequence.
--
-- The step function returns both the candidate for the current state and the
-- state for the following candidate. Returning that continuation state keeps
-- allocation order explicit and lets callers use arbitrary-precision states
-- such as 'Numeric.Natural.Natural', or extend a spelling directly without
-- first counting suffixes.
-- The caller must provide a sequence that does not cycle.
allocateFresh
    :: Ord name
    => (state -> (name, state))
    -> Set.Set name
    -> state
    -> (name, Set.Set name, state)
allocateFresh step used = go
  where
    go !state =
        let (candidate, next) = step state
        in if candidate `Set.member` used then
               go next
           else
               (candidate, Set.insert candidate used, next)
