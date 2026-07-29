-- | Exference's single authority for the complete finite 'Int' namespace.
--
-- Search variables and rigid query skolems use an @IntSet@-backed supply,
-- while shared declaration and synonym operations use tagged variables in an
-- ordinary @Set@. Both paths delegate collision skipping and reservation
-- publication to the common synthesis allocator and share the same
-- overflow-safe full-domain state machine.
module Language.Haskell.Exference.Core.Internal.VariableSupply
  ( IdentifierSupply
  , FlexibleIdSupply
  , supplyFromIdentifiers
  , supplyFromIdentifierSet
  , identifierSupplySize
  , reservedIdentifierSet
  , identifierIsReserved
  , maximumReservedIdentifier
  , reserveIdentifiers
  , allocateFreshIdentifier
  , allocateFreshNonNegativeIdentifier
  , checkedAddIdentifier
  , freshSynthesisVariable
  , synthesisIdentifierNamespace
  ) where

import Control.DeepSeq (NFData (..))
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Numeric.Natural (Natural)

import qualified Language.Haskell.Synthesis.Fresh as SharedFresh
import qualified Language.Haskell.Synthesis.Type as SharedType

newtype IdentifierSupply = IdentifierSupply IntSet.IntSet
  deriving (Eq, Show)

instance NFData IdentifierSupply where
  rnf (IdentifierSupply identifiers) = rnf $ IntSet.toAscList identifiers

-- | Compatibility name for the generic supply where it tracks flexible IDs.
type FlexibleIdSupply = IdentifierSupply

supplyFromIdentifiers :: Foldable collection => collection Int -> IdentifierSupply
supplyFromIdentifiers = IdentifierSupply . foldr IntSet.insert IntSet.empty

-- | Wrap an already canonical identifier set without rebuilding it.
supplyFromIdentifierSet :: IntSet.IntSet -> IdentifierSupply
supplyFromIdentifierSet = IdentifierSupply

-- | Number of currently reserved spellings.
identifierSupplySize :: IdentifierSupply -> Natural
identifierSupplySize (IdentifierSupply identifiers) =
  fromIntegral $ IntSet.size identifiers

-- | The complete set of already allocated identifiers. Search uses this as
-- the set of flexible variables alive before opening a nested rigid scope;
-- the supply never reserves future variables, and retaining substituted-away
-- IDs makes the escape check conservatively stable.
reservedIdentifierSet :: IdentifierSupply -> IntSet.IntSet
reservedIdentifierSet (IdentifierSupply identifiers) = identifiers

identifierIsReserved :: Int -> IdentifierSupply -> Bool
identifierIsReserved identifier (IdentifierSupply reserved) =
  IntSet.member identifier reserved

maximumReservedIdentifier :: IdentifierSupply -> Maybe Int
maximumReservedIdentifier (IdentifierSupply reserved)
  | IntSet.null reserved = Nothing
  | otherwise = Just $ IntSet.findMax reserved

reserveIdentifiers
  :: Foldable collection
  => collection Int
  -> IdentifierSupply
  -> IdentifierSupply
reserveIdentifiers identifiers (IdentifierSupply reserved) = IdentifierSupply
  $ foldr IntSet.insert reserved identifiers

-- | Allocate the historical next ID when that is representable. If the
-- greatest ID is already 'maxBound', search the actual namespace instead of
-- wrapping. The shared allocator updates the @IntSet@ in place conceptually;
-- no boxed-set conversion or enormous intermediate list is involved.
allocateFreshIdentifier
  :: IdentifierSupply
  -> Maybe (Int, IdentifierSupply)
allocateFreshIdentifier = allocateIdentifier initialIdentifierState
 where
  initialIdentifierState supply = case maximumReservedIdentifier supply of
    Nothing -> SingleIdentifier 0
    Just greatest
      | greatest < maxBound -> SingleIdentifier $ greatest + 1
      | otherwise -> completeIdentifierState

-- | Allocate above the non-negative portion of the live namespace where
-- possible, matching Exference's historical rigid-skolem spelling. At
-- 'maxBound', search genuine gaps across the complete domain.
allocateFreshNonNegativeIdentifier
  :: IdentifierSupply
  -> Maybe (Int, IdentifierSupply)
allocateFreshNonNegativeIdentifier = allocateIdentifier initialIdentifierState
 where
  initialIdentifierState (IdentifierSupply reserved) =
    let nonNegative = snd $ IntSet.split (-1) reserved
    in if IntSet.null nonNegative
      then SingleIdentifier 0
      else
        let greatest = IntSet.findMax nonNegative
        in if greatest < maxBound
          then SingleIdentifier $ greatest + 1
          else completeIdentifierState

allocateIdentifier
  :: (IdentifierSupply -> IdentifierState)
  -> IdentifierSupply
  -> Maybe (Int, IdentifierSupply)
allocateIdentifier initial supply@(IdentifierSupply reserved) = do
  (identifier, reserved', _) <- SharedFresh.allocateFreshMaybeBy
    IntSet.member IntSet.insert nextIdentifier reserved $ initial supply
  pure (identifier, IdentifierSupply reserved')

-- | Checked addition over the externally visible finite identity domain.
checkedAddIdentifier :: Int -> Int -> Maybe Int
checkedAddIdentifier left right
  | total < toInteger (minBound :: Int) = Nothing
  | total > toInteger (maxBound :: Int) = Nothing
  | otherwise = Just $ fromInteger total
 where
  total = toInteger left + toInteger right

-- | Preserve the flexible or rigid tag and search all non-negative IDs before
-- the negative half. Tags are disjoint namespaces, so reservations carrying
-- the other tag do not affect the result.
freshSynthesisVariable
  :: Set.Set (SharedType.Variable Int)
  -> SharedType.Variable Int
  -> Maybe (SharedType.Variable Int)
freshSynthesisVariable reserved old = case old of
  SharedType.FlexibleVariable _ -> available SharedType.FlexibleVariable
  SharedType.RigidVariable _ -> available SharedType.RigidVariable
 where
  available tag =
    (\(variable, _, _) -> variable) <$> SharedFresh.allocateFreshMaybe
      (nextSynthesisVariable tag) reserved completeIdentifierState

-- A singleton state preserves the historical greatest-plus-one fast path.
-- The complete phases traverse every 'Int' exactly once without overflowing
-- either endpoint.
data IdentifierState
  = SingleIdentifier !Int
  | NonNegativeIdentifier !Int
  | NegativeIdentifier !Int
  | IdentifierNamespaceExhausted

completeIdentifierState :: IdentifierState
completeIdentifierState = NonNegativeIdentifier 0

nextSynthesisVariable
  :: (Int -> SharedType.Variable Int)
  -> IdentifierState
  -> Maybe (SharedType.Variable Int, IdentifierState)
nextSynthesisVariable tag state = do
  (identifier, next) <- nextIdentifier state
  pure (tag identifier, next)

nextIdentifier :: IdentifierState -> Maybe (Int, IdentifierState)
nextIdentifier state = case state of
  SingleIdentifier identifier -> Just
    (identifier, IdentifierNamespaceExhausted)
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
synthesisIdentifierNamespace :: [Int]
synthesisIdentifierNamespace = enumerate completeIdentifierState
 where
  enumerate state = case nextIdentifier state of
    Just (identifier, next) -> identifier : enumerate next
    Nothing -> []
