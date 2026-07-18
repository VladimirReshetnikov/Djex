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
  , adjustInventoryDataTypeAnnotations
  , inventoryEnvironment
  , inventoryKindAssumptions
  ) where

import Data.Void (Void)
import GHC.Generics (Generic)

import Language.Haskell.Synthesis.Declaration
import Language.Haskell.Synthesis.Environment (Environment, EnvironmentError)
import qualified Language.Haskell.Synthesis.Environment as Environment
import Language.Haskell.Synthesis.KindInference
import Language.Haskell.Synthesis.Name (Name)

-- | A coherent pair of one sealed environment and the kind assumptions
-- inferred from exactly that environment.
data Inventory typeVariable annotation = Inventory
  (Environment typeVariable Void annotation)
  KindAssumptions
  deriving (Eq, Show, Functor)

-- Ordinary projections, rather than record fields, are deliberate. A record
-- update does not require the hidden constructor to be in scope and would let
-- callers pair an environment with unrelated inferred assumptions.
-- | Recover the authoritative declaration environment.
inventoryEnvironment
  :: Inventory typeVariable annotation
  -> Environment typeVariable Void annotation
inventoryEnvironment (Inventory environment _) = environment

-- | Recover the assumptions inferred while sealing the inventory.
inventoryKindAssumptions :: Inventory typeVariable annotation -> KindAssumptions
inventoryKindAssumptions (Inventory _ assumptions) = assumptions

-- | The phase at which inventory construction failed.
data InventoryError typeVariable kindVariable
  = InvalidInventoryEnvironment (EnvironmentError typeVariable)
  | UngroundedInventoryKind kindVariable
  | InvalidInventoryKinds (KindInferenceError typeVariable)
  deriving (Eq, Ord, Show, Generic)

-- | Validate, ground, and kind-check a declaration list using generalized
-- class kinds.
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

-- | Attach derived datatype metadata without repeating structural validation
-- or kind inference.  Only top-level datatype annotations can change; the
-- sealed environment's names, declarations, indexes, and assumptions remain
-- authoritative.
adjustInventoryDataTypeAnnotations
  :: (Name -> annotation -> annotation)
  -> Inventory typeVariable annotation
  -> Inventory typeVariable annotation
adjustInventoryDataTypeAnnotations adjust (Inventory environment assumptions) =
  Inventory
    (Environment.adjustEnvironmentDataTypeAnnotations adjust environment)
    assumptions
