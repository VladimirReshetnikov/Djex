-- | Deterministic allocation in Exference's complete tagged identifier
-- namespace.  This module deliberately depends only on the shared variable
-- tag, so low-level type operations and declaration lowering can use one
-- allocator without introducing an import cycle.
module Language.Haskell.Exference.Core.Internal.VariableSupply
  ( freshSynthesisVariable
  , synthesisIdentifierNamespace
  ) where

import qualified Data.Set as Set
import qualified Language.Haskell.Synthesis.Fresh as SharedFresh
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
    available SharedType.FlexibleVariable
  SharedType.RigidVariable _ ->
    available SharedType.RigidVariable
 where
  available tag =
    (\(variable, _, _) -> variable) <$> SharedFresh.allocateFreshMaybe
      (nextSynthesisVariable tag) reserved (NonNegativeIdentifier 0)

-- The explicit phases traverse every 'Int' exactly once without endpoint
-- arithmetic overflow. Ordinary allocation retains only its current state.
data IdentifierState
  = NonNegativeIdentifier !Int
  | NegativeIdentifier !Int
  | IdentifierNamespaceExhausted

nextSynthesisVariable
  :: (Int -> SharedType.Variable Int)
  -> IdentifierState
  -> Maybe (SharedType.Variable Int, IdentifierState)
nextSynthesisVariable tag state = do
  (identifier, next) <- nextIdentifier state
  pure (tag identifier, next)

nextIdentifier :: IdentifierState -> Maybe (Int, IdentifierState)
nextIdentifier state = case state of
  NonNegativeIdentifier identifier -> Just
    ( identifier
    , if identifier == maxBound
        then NegativeIdentifier minBound
        else NonNegativeIdentifier $ identifier + 1
    )
  NegativeIdentifier identifier -> Just
    ( identifier
    , if identifier == -1
        then IdentifierNamespaceExhausted
        else NegativeIdentifier $ identifier + 1
    )
  IdentifierNamespaceExhausted -> Nothing

-- | Lazy identifier view used only to canonically renumber a finite batch.
-- Fresh allocation above consumes the state machine directly, so it does not
-- retain this conceptually enormous namespace list.
synthesisIdentifierNamespace :: [Int]
synthesisIdentifierNamespace = enumerate $ NonNegativeIdentifier 0
 where
  enumerate state = case nextIdentifier state of
    Just (identifier, next) -> identifier : enumerate next
    Nothing -> []
