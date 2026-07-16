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
  , allocateFreshBy
  , allocateFreshMaybeBy
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
  . allocateFreshEitherBy Set.member Set.insert (Right . step) reserved

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
  . allocateFreshEitherBy Set.member Set.insert
      (maybe (Left ()) Right . step) reserved

-- | Select from a total generator using a caller-owned reservation store.
--
-- This is the container-polymorphic form of 'allocateFresh'. It lets a
-- backend retain a specialized store such as @IntSet@ without rebuilding it
-- as a boxed 'Set'. The membership and insertion operations must describe
-- the same notion of identity.
allocateFreshBy
  :: (value -> reservation -> Bool)
  -> (value -> reservation -> reservation)
  -> (state -> (value, state))
  -> reservation
  -> state
  -> (value, reservation, state)
allocateFreshBy member insert step reserved = either absurd id
  . allocateFreshEitherBy member insert (Right . step) reserved

-- | Exhaustible, container-polymorphic counterpart of 'allocateFreshMaybe'.
allocateFreshMaybeBy
  :: (value -> reservation -> Bool)
  -> (value -> reservation -> reservation)
  -> (state -> Maybe (value, state))
  -> reservation
  -> state
  -> Maybe (value, reservation, state)
allocateFreshMaybeBy member insert step reserved = either (const Nothing) Just
  . allocateFreshEitherBy member insert (maybe (Left ()) Right . step) reserved

allocateFreshEitherBy
  :: (value -> reservation -> Bool)
  -> (value -> reservation -> reservation)
  -> (state -> Either failure (value, state))
  -> reservation
  -> state
  -> Either failure (value, reservation, state)
allocateFreshEitherBy member insert step reserved = go
 where
  go !state = do
    (candidate, next) <- step state
    if candidate `member` reserved
      then go next
      else Right
        (candidate, insert candidate reserved, next)
