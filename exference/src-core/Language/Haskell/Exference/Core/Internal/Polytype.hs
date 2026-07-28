-- | Explicit forall elimination at scoped-value use sites.
--
-- The first-order unifier deliberately treats quantified subtrees as atoms.
-- Opening a provider is a typing rule, not a unification rule, so search and
-- the independent expression checker share that operation here.
module Language.Haskell.Exference.Core.Internal.Polytype
  ( ProviderUseMode (..)
  , classifyProviderUse
  , instantiateLeadingForallsWith
  )
where

import Control.Monad (guard)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map

import Language.Haskell.Exference.Core.Internal.FlexibleIds
  ( FlexibleRenaming
  , flexibleIdentifiers
  )
import Language.Haskell.Exference.Core.Internal.VariableSupply
  ( FlexibleIdSupply
  , reserveIdentifiers
  )
import Language.Haskell.Exference.Core.Types
import qualified Language.Haskell.Synthesis.Type as SharedType

-- | How a scoped provider participates at one occurrence. Keeping this
-- classification beside forall instantiation prevents search and independent
-- checking from assigning different meanings to the same annotation shape.
data ProviderUseMode
  = OrdinaryProviderUse
  | OpaqueProviderForwarding
  | InstantiateProviderUse
  deriving (Eq, Show)

-- | Classify a provider against the type requested at its occurrence.
--
-- Empty-binder, empty-context forall wrappers are semantic no-ops to opaque
-- atom construction and unification. Peel exactly those wrappers before
-- inspecting either root, so a vacuously wrapped monotype does not accidentally
-- request opaque forwarding. A binderless wrapper with a context remains
-- significant and is not peeled.
classifyProviderUse :: HsType -> HsType -> ProviderUseMode
classifyProviderUse rawProvider rawRequested =
  case peelVacuousForalls rawProvider of
    TypeForallNative{} -> case peelVacuousForalls rawRequested of
      TypeForallNative{} -> OpaqueProviderForwarding
      _ -> InstantiateProviderUse
    _ -> OrdinaryProviderUse
 where
  peelVacuousForalls (TypeForallNative [] [] body) =
    peelVacuousForalls body
  peelVacuousForalls ty = ty

-- | Replace every binder in the complete leading forall chain with a fresh
-- flexible variable. Direct contexts are returned as proof obligations in
-- outer-to-inner order; a forall below an arrow or other type boundary stays
-- opaque.
--
-- The caller supplies the namespace allocator so live search keeps its finite
-- test seam while the checker uses the production allocator. All source IDs
-- are reserved before allocation: after a binder is erased, reusing its old
-- spelling could capture a free occurrence or conflate shadowed layers.
-- Checked Exference inputs guarantee flexible, duplicate-free binder lists;
-- 'Nothing' therefore denotes identifier-space exhaustion at call sites.
instantiateLeadingForallsWith
  :: ([TVarId]
      -> FlexibleIdSupply
      -> Maybe (FlexibleRenaming, FlexibleIdSupply))
  -> FlexibleIdSupply
  -> HsType
  -> Maybe (HsType, [HsConstraint], FlexibleIdSupply)
instantiateLeadingForallsWith allocate initialSupply source =
  go reservedSupply [] source
 where
  reservedSupply = reserveIdentifiers
    (IntSet.toAscList $ flexibleIdentifiers source)
    initialSupply

  go supply contextChunks (TypeForallNative binders contexts body) = do
    identifiers <- traverse SharedType.flexibleVariableIdentity binders
    guard $ IntSet.size (IntSet.fromList identifiers) == length identifiers
    (renaming, nextSupply) <- allocate identifiers supply
    -- This must be a lexical rename, not a whole-namespace traversal: an
    -- inner forall may deliberately shadow the same nominal binder ID.
    let scopedRenaming = Map.fromList
          [ ( SharedType.FlexibleVariable old
            , SharedType.FlexibleVariable fresh
            )
          | (old, fresh) <- IntMap.toAscList renaming
          ]
        rename = SharedType.renameScopedVariables scopedRenaming
    go nextSupply
      (map (fmap rename) contexts : contextChunks)
      (rename body)
  go supply contextChunks body = Just
    (body, concat $ reverse contextChunks, supply)
