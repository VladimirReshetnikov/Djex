{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

-- | One structurally and kind-checked source inventory.
--
-- Frontends should retain this value through query elaboration rather than
-- sealing an 'Environment' and then discarding the kind assumptions computed
-- from the same declarations. Backend search indexes remain lowerings of the
-- checked environment.
module Language.Haskell.Synthesis.Inventory
  ( Inventory
  , InventoryError (..)
  , mkInventory
  , mkInventoryWithClassPolicy
  , mkInventoryFromEnvironmentWithClassPolicy
  , inventoryEnvironment
  , inventoryKindAssumptions
  ) where

import Data.Void (Void)
import GHC.Generics (Generic)

import Language.Haskell.Synthesis.Declaration
import Language.Haskell.Synthesis.Environment (Environment, EnvironmentError)
import qualified Language.Haskell.Synthesis.Environment as Environment
import Language.Haskell.Synthesis.KindInference

data Inventory typeVariable annotation = Inventory
  { inventoryEnvironment ::
      Environment typeVariable Void annotation
  , inventoryKindAssumptions :: KindAssumptions
  }
  deriving (Eq, Show, Functor, Generic)

data InventoryError typeVariable kindVariable
  = InvalidInventoryEnvironment (EnvironmentError typeVariable)
  | UngroundedInventoryKind kindVariable
  | InvalidInventoryKinds (KindInferenceError typeVariable)
  deriving (Eq, Ord, Show, Generic)

mkInventory
  :: Ord typeVariable
  => KindInventoryPolicy
  -> [Declaration typeVariable kindVariable annotation]
  -> Either (InventoryError typeVariable kindVariable)
      (Inventory typeVariable annotation)
mkInventory policy = mkInventoryWithClassPolicy
  policy GeneralizeClassKinds

-- | Declaration-list constructor with an explicit class-kind finalization
-- policy. Structural errors retain precedence over unsolved or invalid kinds.
mkInventoryWithClassPolicy
  :: Ord typeVariable
  => KindInventoryPolicy
  -> ClassKindPolicy
  -> [Declaration typeVariable kindVariable annotation]
  -> Either (InventoryError typeVariable kindVariable)
      (Inventory typeVariable annotation)
mkInventoryWithClassPolicy policy classKindPolicy declarations = do
  -- Validate first so an ordinary declaration/namespace error is not masked
  -- by an unrelated unsolved kind elsewhere in the same editable inventory.
  environment <- either (Left . InvalidInventoryEnvironment) Right
    $ Environment.mkEnvironment declarations
  groundedEnvironment <- either (Left . UngroundedInventoryKind) Right
    $ Environment.groundEnvironmentKinds environment
  either (Left . InvalidInventoryKinds) Right
    $ inferInventory policy classKindPolicy groundedEnvironment

-- | Add kind assumptions to an already grounded, opaque environment without
-- repeating structural validation or rebuilding its indexes. Consequently,
-- this constructor can only return 'InvalidInventoryKinds'; the other
-- 'InventoryError' alternatives are uninhabited at this boundary.
mkInventoryFromEnvironmentWithClassPolicy
  :: Ord typeVariable
  => KindInventoryPolicy
  -> ClassKindPolicy
  -> Environment typeVariable Void annotation
  -> Either (InventoryError typeVariable Void)
      (Inventory typeVariable annotation)
mkInventoryFromEnvironmentWithClassPolicy policy classKindPolicy environment = do
  either (Left . InvalidInventoryKinds) Right
    $ inferInventory policy classKindPolicy environment

inferInventory
  :: Ord typeVariable
  => KindInventoryPolicy
  -> ClassKindPolicy
  -> Environment typeVariable Void annotation
  -> Either (KindInferenceError typeVariable)
      (Inventory typeVariable annotation)
inferInventory policy classKindPolicy environment = do
  assumptions <- inferDeclarationKindsWithClassPolicy
    policy classKindPolicy environment
  pure $ Inventory environment assumptions
