{-# LANGUAGE DeriveGeneric #-}

-- | Collision-free rigid variables for opening Exference query quantifiers.
--
-- A rigid identifier is observable in generated type annotations and residual
-- constraints.  It therefore cannot be allocated from a private counter that
-- ignores rigid variables already present in a caller-supplied goal or search
-- environment.  This module computes one finite, ordered plan which search,
-- independent checking, and renderer hints can all consume verbatim.
module Language.Haskell.Exference.Core.RigidInstantiation
  ( RigidInstantiationError (..)
  , RigidInstantiationContext
  , mkRigidInstantiationContext
  , RigidInstantiationPlan
  , rigidInstantiations
  , planRigidInstantiation
  , splitRigidInstantiationLayer
  ) where

import Control.DeepSeq (NFData (rnf))
import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.Core.Internal.VariableSupply
  ( IdentifierSupply
  , allocateFreshNonNegativeIdentifier
  , reserveIdentifiers
  , supplyFromIdentifiers
  )
import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils (forallify)
import qualified Language.Haskell.Synthesis.Collection as SharedCollection
import qualified Language.Haskell.Synthesis.Count as SharedCount
import qualified Language.Haskell.Synthesis.Type as SharedType

-- | Rigid-instantiation planning either exhausts the finite 'Int' identity
-- space or encounters an already-rigid forall binder, which Exference cannot
-- instantiate as one of its flexible source quantifiers. For exhaustion, the
-- maximum is reported as 'Nothing' only when the environment and goal contain
-- no pre-existing rigid variables. The requested count retains its historical
-- 'Int' API and saturates at 'maxBound' if a larger binder list is ever
-- supplied; it never wraps into a misleading non-positive value.
data RigidInstantiationError
  = RigidIdentifierSupplyExhausted
      (Maybe TVarId) -- ^ Greatest pre-existing rigid ID, when one exists.
      Int -- ^ Saturating number of requested skolems.
  | RigidForallBinderCannotBeInstantiated TVarId
  deriving (Eq, Show, Generic)

instance NFData RigidInstantiationError

-- | The rigid namespace of a sealed environment. Retaining the finite set,
-- rather than only its maximum, lets boundary queries allocate real gaps.
-- 'Generic' is deliberately omitted because the constructor is not public.
data RigidInstantiationContext = RigidInstantiationContext
  { maximumEnvironmentRigidIdentifier :: Maybe TVarId
  , environmentRigidIdentifiers :: IdentifierSupply
  , environmentRigidForallBinder :: Maybe TVarId
  }
  deriving (Eq, Show)

instance NFData RigidInstantiationContext where
  rnf (RigidInstantiationContext maximumIdentifier identifiers binder) =
    rnf maximumIdentifier `seq` rnf identifiers `seq` rnf binder

-- | Cache the environment-wide rigid maximum used by every later query.
mkRigidInstantiationContext :: EnvDictionary -> RigidInstantiationContext
mkRigidInstantiationContext environment = RigidInstantiationContext
  (SharedCollection.maximumPresent $ map maximumRigidInType types)
  (supplyFromIdentifiers $ concatMap rigidIdentifiersInType types)
  (SharedCollection.firstPresent $ map firstRigidForallBinder types)
 where
  types = environmentTypes environment

-- | Quantified flexible binder IDs paired with their fresh rigid IDs, in the
-- exact lexical order in which Exference opens the leading forall chain.
--
-- The constructor is private so the pairing cannot drift from 'forallify'.
newtype RigidInstantiationPlan = RigidInstantiationPlan
  [(TVarId, TVarId)]
  deriving (Eq, Show)

instance NFData RigidInstantiationPlan where
  rnf (RigidInstantiationPlan instantiations) = rnf instantiations

-- An ordinary projection prevents record-update syntax from replacing the
-- checked lexical pairing while the constructor remains hidden.
rigidInstantiations :: RigidInstantiationPlan -> [(TVarId, TVarId)]
rigidInstantiations (RigidInstantiationPlan instantiations) = instantiations

-- | Plan every rigid variable needed to open a goal.
--
-- The extra constraints are for callers such as the standalone independent
-- checker which receive assumptions separately from the goal.  Live search
-- passes an empty list because query contexts are embedded in the goal before
-- it reaches the core. A native forall containing a rigid binder is rejected
-- before allocation so it cannot be silently treated as a flexible binder.
planRigidInstantiation
  :: RigidInstantiationContext
  -> [HsConstraint]
  -> HsType
  -> Either RigidInstantiationError RigidInstantiationPlan
planRigidInstantiation context extraConstraints goal = do
  case SharedCollection.firstPresent
      (map firstRigidForallBinder queryTypes
        ++ [environmentRigidForallBinder context]) of
    Just identifier -> Left
      $ RigidForallBinderCannotBeInstantiated identifier
    Nothing -> Right ()
  binders <- leadingBinders $ forallify goal
  case allocateRigidInstantiations binders initialSupply of
    Nothing -> Left $ RigidIdentifierSupplyExhausted
      maximumRigid
      (SharedCount.saturatingNaturalToInt $ SharedCount.naturalLength binders)
    Just instantiations -> Right $ RigidInstantiationPlan instantiations
 where
  queryTypes = goal : concatMap constraint_params extraConstraints
  maximumRigid = SharedCollection.maximumPresent
    $ maximumEnvironmentRigidIdentifier context
    : map maximumRigidInType queryTypes
  initialSupply = reserveIdentifiers
    (concatMap rigidIdentifiersInType queryTypes)
    (environmentRigidIdentifiers context)

allocateRigidInstantiations
  :: [TVarId]
  -> IdentifierSupply
  -> Maybe [(TVarId, TVarId)]
allocateRigidInstantiations binders initialSupply = fmap (reverse . fst)
  $ foldM allocate ([], initialSupply) binders
 where
  allocate (instantiations, supply) binder = do
    (identifier, nextSupply) <- allocateFreshNonNegativeIdentifier supply
    pure ((binder, identifier) : instantiations, nextSupply)

-- | Split off the instantiations belonging to one forall layer by following
-- the binder spine directly.  This has the semantics of splitting at the
-- binder count without first projecting that count into a machine-sized
-- 'Int'.  Callers remain responsible for checking that the paired binder IDs
-- agree with the layer they are opening.
splitRigidInstantiationLayer
  :: [TVarId]
  -> [(TVarId, TVarId)]
  -> ([(TVarId, TVarId)], [(TVarId, TVarId)])
splitRigidInstantiationLayer [] remaining = ([], remaining)
splitRigidInstantiationLayer _ [] = ([], [])
splitRigidInstantiationLayer (_ : binders) (current : remaining) =
  let (selected, rest) = splitRigidInstantiationLayer binders remaining
  in (current : selected, rest)

leadingBinders
  :: HsType
  -> Either RigidInstantiationError [TVarId]
leadingBinders = traverse flexibleBinder . SharedType.leadingForallVariables
 where
  flexibleBinder variable = maybe
    (Left $ RigidForallBinderCannotBeInstantiated
      $ SharedType.variableIdentity variable)
    Right
    $ SharedType.flexibleVariableIdentity variable

-- Locate forbidden rigid variables specifically in binder position. The
-- shared observation distinguishes declarations from ordinary occurrences
-- and preserves the historical structural failure order.
firstRigidForallBinder :: HsType -> Maybe TVarId
firstRigidForallBinder = SharedCollection.firstPresent
  . map SharedType.rigidVariableIdentity
  . SharedType.typeBinderVariables

environmentTypes :: EnvDictionary -> [HsType]
environmentTypes environment =
  environmentBindingTypes environment
  ++ concatMap (concatMap constraint_params . tclass_constraints)
      (Map.elems $ sClassEnv_tclasses classes)
  ++ concatMap instanceTypes
      (concat $ Map.elems $ sClassEnv_instances classes)
 where
  classes = environmentClasses environment

  instanceTypes instanceDeclaration = concatMap constraint_params
    $ instance_head instanceDeclaration
      : instance_constraints instanceDeclaration

maximumRigidInType :: HsType -> Maybe TVarId
maximumRigidInType = SharedCollection.maximumPresent
  . map Just
  . rigidIdentifiersInType

rigidIdentifiersInType :: HsType -> [TVarId]
rigidIdentifiersInType = foldMap $ SharedType.foldRigidVariable (: [])
