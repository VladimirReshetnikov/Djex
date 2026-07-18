{-# LANGUAGE BangPatterns #-}

-- | Collision-free selection and allocation from deterministic candidates.
--
-- Backends retain ownership of their name domains and candidate order. This
-- module owns the common operation of advancing past reserved candidates.
-- Stateful allocation additionally publishes the chosen value in the
-- reservation set and returns the generator's continuation state.
module Language.Haskell.Synthesis.Fresh
  ( selectFresh
  , selectFreshBy
  , allocateFresh
  , allocateFreshMaybe
  , allocateFreshBy
  , allocateFreshMaybeBy
  ) where

import qualified Data.Set as Set
import Data.Void (absurd)

-- | Select the first unused value from a deterministic successor chain.
--
-- Use this when the caller already owns reservation updates or merely needs
-- one collision-free presentation spelling. 'allocateFresh' is the stateful
-- counterpart that publishes the selected value in the returned set.
selectFresh
  :: Ord value
  => (value -> value)
  -> Set.Set value
  -> value
  -> value
selectFresh = selectFreshBy Set.member

-- | Container-polymorphic counterpart of 'selectFresh'.
--
-- The successor must not cycle entirely within the reserved identities.
selectFreshBy
  :: (value -> reservation -> Bool)
  -> (value -> value)
  -> reservation
  -> value
  -> value
selectFreshBy member next reserved initial = selected
 where
  (selected, _, _) = allocateFreshBy member keepReserved step reserved initial
  keepReserved _ = id
  step candidate = (candidate, next candidate)

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
-- as a boxed @Set@. The membership and insertion operations must describe
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
