-- | Scope levels for dynamically opened rank-N goals.
--
-- A flexible variable which already exists when a forall is opened must not
-- later be solved to a type containing one of that forall's fresh rigid
-- constants.  Otherwise the constant would escape its quantified scope.  The
-- relation is kept as forbidden rigid IDs per flexible ID.  Substitution edges
-- propagate those restrictions: if an older variable is solved in terms of a
-- younger one, the younger variable inherits every scope that the older one
-- may not escape.
module Language.Haskell.Exference.Core.Internal.RigidScope
  ( RigidEscape (..)
  , RigidScope
  , NestedRigidProvenance
  , emptyRigidScope
  , registerRigidScope
  , nestedRigidProvenance
  , provenanceRigidIdentifiers
  , escapingRigidConstraints
  , validateRigidSubstitutions
  ) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Control.DeepSeq (NFData (..))

import Language.Haskell.Exference.Core.Types
import qualified Language.Haskell.Synthesis.Type as SharedType

data RigidEscape = RigidEscape
  { escapingFlexibleVariable :: !TVarId
  , escapingRigidVariable :: !TVarId
  }
  deriving (Eq, Show)

data RigidScope = RigidScope
  (IntMap.IntMap IntSet.IntSet)
  IntSet.IntSet
  deriving (Eq, Show)

-- | Opaque capability identifying the annotation skolems allocated by one
-- live search branch. The stable checker re-exports only the abstract type;
-- construction remains behind the internal search boundary.
newtype NestedRigidProvenance = NestedRigidProvenance IntSet.IntSet

instance NFData RigidEscape where
  rnf (RigidEscape flexible rigid) = rnf flexible `seq` rnf rigid

instance NFData RigidScope where
  rnf (RigidScope restrictions owned) =
    rnf (IntMap.toAscList restrictions)
      `seq` rnf (IntSet.toAscList owned)

emptyRigidScope :: RigidScope
emptyRigidScope = RigidScope IntMap.empty IntSet.empty

-- | Record one newly opened scope against precisely the flexible variables
-- which are alive before its rigid constants are allocated.
registerRigidScope
  :: IntSet.IntSet
  -> [TVarId]
  -> RigidScope
  -> RigidScope
registerRigidScope alive rigids (RigidScope restrictions owned)
  | IntSet.null rigidSet = RigidScope restrictions owned
  | otherwise = RigidScope
      (IntSet.foldl'
        (\current variable ->
          IntMap.insertWith IntSet.union variable rigidSet current)
        restrictions
        alive)
      (IntSet.union rigidSet owned)
 where
  rigidSet = IntSet.fromList rigids

-- | Seal the rigid spellings introduced by nested scopes in this branch for
-- the independent candidate checker.
nestedRigidProvenance :: RigidScope -> NestedRigidProvenance
nestedRigidProvenance (RigidScope _ owned) = NestedRigidProvenance owned

-- | Unwrap provenance only inside the checker implementation.
provenanceRigidIdentifiers :: NestedRigidProvenance -> IntSet.IntSet
provenanceRigidIdentifiers (NestedRigidProvenance identifiers) = identifiers

-- | Residual obligations cannot carry a skolem owned by a dynamically opened
-- scope to the top-level candidate boundary.  Keep ownership separately from
-- old-flexible restrictions: a scope opened when no flexible variable is
-- alive can still leak through a deferred class constraint.
escapingRigidConstraints :: RigidScope -> [HsConstraint] -> [HsConstraint]
escapingRigidConstraints (RigidScope _ owned) = filter escapes
 where
  escapes constraint = not $ IntSet.null $ IntSet.intersection owned
    $ foldMap rigidIds $ constraint_params constraint
  rigidIds = foldMap $ SharedType.foldRigidVariable IntSet.singleton

-- | Validate one simultaneous unifier result and retain the age information
-- implied by its flexible-variable edges.  Propagation reaches a fixed point
-- before checking rigid images, so the result is independent of the map's
-- traversal order (for example, both @old := young, young := rigid@ and its
-- already-zonked form are rejected).
validateRigidSubstitutions
  :: RigidScope
  -> Substs
  -> Either RigidEscape RigidScope
validateRigidSubstitutions (RigidScope initial owned) substitutions = do
  let propagated = closeRestrictions initial
  mapM_ (validateImage propagated) $ IntMap.toAscList substitutions
  pure $ RigidScope propagated owned
 where
  edges = IntMap.map (Set.toAscList . freeVars) substitutions

  closeRestrictions current =
    let next = IntMap.foldlWithKey' propagateFrom current edges
    in if next == current then current else closeRestrictions next

  propagateFrom current source targets =
    let inherited = IntMap.findWithDefault IntSet.empty source current
    in foldr
        (\target -> IntMap.insertWith IntSet.union target inherited)
        current
        targets

  validateImage restrictions (variable, image)
    | IntSet.null escaping = Right ()
    | otherwise = Left $ RigidEscape variable $ IntSet.findMin escaping
   where
    forbidden = IntMap.findWithDefault IntSet.empty variable restrictions
    escaping = IntSet.intersection forbidden $ rigidIds image

  rigidIds = foldMap $ SharedType.foldRigidVariable IntSet.singleton
