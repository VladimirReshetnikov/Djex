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
mkInventory policy declarations = do
  -- Validate first so an ordinary declaration/namespace error is not masked
  -- by an unrelated unsolved kind elsewhere in the same editable inventory.
  _ <- either (Left . InvalidInventoryEnvironment) Right
    $ Environment.mkEnvironment declarations
  grounded <- either (Left . UngroundedInventoryKind) Right
    $ mapM groundDeclarationKinds declarations
  environment <- either (Left . InvalidInventoryEnvironment) Right
    $ Environment.mkEnvironment grounded
  assumptions <- either (Left . InvalidInventoryKinds) Right
    $ inferDeclarationKindsWith policy grounded
  pure $ Inventory environment assumptions
