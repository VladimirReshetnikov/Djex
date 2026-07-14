-- | Deterministic allocation in Exference's complete tagged identifier
-- namespace.  This module deliberately depends only on the shared variable
-- tag, so low-level type operations and declaration lowering can use one
-- allocator without introducing an import cycle.
module Language.Haskell.Exference.Core.Internal.VariableSupply
  ( freshSynthesisVariable
  , synthesisIdentifierNamespace
  ) where

import Data.List (find)
import qualified Data.Set as Set
import qualified Language.Haskell.Synthesis.Type as SharedType

-- | Preserve the flexible or rigid tag and search all non-negative IDs before
-- the negative half of 'Int'.  Enumerating the two closed ranges avoids the
-- overflow bug in endpoint arithmetic such as @maximumReserved + 1@.
freshSynthesisVariable
  :: Set.Set (SharedType.Variable Int)
  -> SharedType.Variable Int
  -> Maybe (SharedType.Variable Int)
freshSynthesisVariable reserved old = case old of
  SharedType.FlexibleVariable _ ->
    SharedType.FlexibleVariable <$> available SharedType.FlexibleVariable
  SharedType.RigidVariable _ ->
    SharedType.RigidVariable <$> available SharedType.RigidVariable
 where
  available tag = find ((`Set.notMember` reserved) . tag)
    synthesisIdentifierNamespace

-- Use every 'Int' value exactly once.  The list remains lazy, so ordinary
-- allocation examines only the short prefix ending at the first free ID.
synthesisIdentifierNamespace :: [Int]
synthesisIdentifierNamespace = [0 .. maxBound] ++ [minBound .. (-1)]
