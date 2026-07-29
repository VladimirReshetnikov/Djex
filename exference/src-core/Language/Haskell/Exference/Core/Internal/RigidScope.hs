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
  , emptyRigidScope
  , registerRigidScope
  , validateRigidSubstitutions
  ) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

import Language.Haskell.Exference.Core.Types
import qualified Language.Haskell.Synthesis.Type as SharedType

data RigidEscape = RigidEscape
  { escapingFlexibleVariable :: !TVarId
  , escapingRigidVariable :: !TVarId
  }
  deriving (Eq, Show)

newtype RigidScope = RigidScope (IntMap.IntMap IntSet.IntSet)
  deriving (Eq, Show)

emptyRigidScope :: RigidScope
emptyRigidScope = RigidScope IntMap.empty

-- | Record one newly opened scope against precisely the flexible variables
-- which are alive before its rigid constants are allocated.
registerRigidScope
  :: IntSet.IntSet
  -> [TVarId]
  -> RigidScope
  -> RigidScope
registerRigidScope alive rigids (RigidScope restrictions)
  | IntSet.null rigidSet = RigidScope restrictions
  | otherwise = RigidScope $ IntSet.foldl'
      (\current variable ->
        IntMap.insertWith IntSet.union variable rigidSet current)
      restrictions
      alive
 where
  rigidSet = IntSet.fromList rigids

-- | Validate one simultaneous unifier result and retain the age information
-- implied by its flexible-variable edges.  Propagation reaches a fixed point
-- before checking rigid images, so the result is independent of the map's
-- traversal order (for example, both @old := young, young := rigid@ and its
-- already-zonked form are rejected).
validateRigidSubstitutions
  :: RigidScope
  -> Substs
  -> Either RigidEscape RigidScope
validateRigidSubstitutions (RigidScope initial) substitutions = do
  let propagated = closeRestrictions initial
  mapM_ (validateImage propagated) $ IntMap.toAscList substitutions
  pure $ RigidScope propagated
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
