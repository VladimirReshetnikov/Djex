{-# LANGUAGE BangPatterns #-}

-- | Collision-free allocation from deterministic candidate generators.
--
-- Backends retain ownership of their name domains and candidate order. This
-- module owns the common operation of advancing past reserved candidates,
-- publishing the chosen value in the reservation set, and returning the
-- generator's continuation state.
module Language.Haskell.Synthesis.Fresh
  ( allocateFresh
  , allocateFreshMaybe
  ) where

import qualified Data.Set as Set
import Data.Void (absurd)

-- | Select the first unused value from a total candidate generator.
--
-- The caller must provide a generator that does not cycle within the reserved
-- set. Arbitrary-precision states make this suitable for genuinely unbounded
-- namespaces such as numeric suffixes and repeatedly primed identifiers.
allocateFresh
  :: Ord value
  => (state -> (value, state))
  -> Set.Set value
  -> state
  -> (value, Set.Set value, state)
allocateFresh step reserved = either absurd id
  . allocateFreshEither (Right . step) reserved

-- | Select the first unused value from an exhaustible candidate generator.
--
-- 'Nothing' means that the finite namespace ended before an available value
-- was found. A successful allocation returns the updated reservation set and
-- the state immediately following the selected candidate.
allocateFreshMaybe
  :: Ord value
  => (state -> Maybe (value, state))
  -> Set.Set value
  -> state
  -> Maybe (value, Set.Set value, state)
allocateFreshMaybe step reserved = either (const Nothing) Just
  . allocateFreshEither (maybe (Left ()) Right . step) reserved

allocateFreshEither
  :: Ord value
  => (state -> Either failure (value, state))
  -> Set.Set value
  -> state
  -> Either failure (value, Set.Set value, state)
allocateFreshEither step reserved = go
 where
  go !state = do
    (candidate, next) <- step state
    if candidate `Set.member` reserved
      then go next
      else Right
        (candidate, Set.insert candidate reserved, next)
