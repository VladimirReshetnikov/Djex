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

import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.Core.Internal.FlexibleIds
  ( IdentifierSupply
  , allocateFreshNonNegativeIdentifier
  , reserveIdentifiers
  , supplyFromIdentifiers
  )
import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils (forallify)
import qualified Language.Haskell.Synthesis.Type as SharedType

-- | The finite 'Int' identity space cannot hold all skolems required by a
-- query.  The maximum is reported as 'Nothing' only when the environment and
-- goal contain no pre-existing rigid variables. The requested count retains
-- its historical 'Int' API and saturates at 'maxBound' if a larger binder list
-- is ever supplied; it never wraps into a misleading non-positive value.
data RigidInstantiationError = RigidIdentifierSupplyExhausted
  { maximumPreexistingRigidIdentifier :: Maybe TVarId
  , requestedRigidIdentifierCount :: Int
  }
  deriving (Eq, Show, Generic)

instance NFData RigidInstantiationError

-- | The rigid namespace of a sealed environment. Retaining the finite set,
-- rather than only its maximum, lets boundary queries allocate real gaps.
data RigidInstantiationContext = RigidInstantiationContext
  { maximumEnvironmentRigidIdentifier :: Maybe TVarId
  , environmentRigidIdentifiers :: IdentifierSupply
  }
  deriving (Eq, Show, Generic)

instance NFData RigidInstantiationContext

-- | Cache the environment-wide rigid maximum used by every later query.
mkRigidInstantiationContext :: EnvDictionary -> RigidInstantiationContext
mkRigidInstantiationContext environment = RigidInstantiationContext
  (Foldable.foldl' maximumMaybe Nothing
    $ map maximumRigidInType types)
  (supplyFromIdentifiers $ concatMap rigidIdentifiersInType types)
 where
  types = environmentTypes environment

-- | Quantified flexible binder IDs paired with their fresh rigid IDs, in the
-- exact lexical order in which Exference opens the leading forall chain.
--
-- The constructor is private so the pairing cannot drift from 'forallify'.
newtype RigidInstantiationPlan = RigidInstantiationPlan
  { rigidInstantiations :: [(TVarId, TVarId)]
  }
  deriving (Eq, Show, Generic)

instance NFData RigidInstantiationPlan

-- | Plan every rigid variable needed to open a goal.
--
-- The extra constraints are for callers such as the standalone independent
-- checker which receive assumptions separately from the goal.  Live search
-- passes an empty list because query contexts are embedded in the goal before
-- it reaches the core.
planRigidInstantiation
  :: RigidInstantiationContext
  -> [HsConstraint]
  -> HsType
  -> Either RigidInstantiationError RigidInstantiationPlan
planRigidInstantiation context extraConstraints goal =
  case allocateRigidInstantiations binders initialSupply of
    Nothing -> Left $ RigidIdentifierSupplyExhausted
      maximumRigid (saturatingBinderCount binders)
    Just instantiations -> Right $ RigidInstantiationPlan instantiations
 where
  binders = leadingBinders $ forallify goal
  queryTypes = goal : concatMap constraint_params extraConstraints
  maximumRigid = Foldable.foldl' maximumMaybe
    (maximumEnvironmentRigidIdentifier context)
    $ map maximumRigidInType
    queryTypes
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

-- Count only for the compatibility diagnostic after allocation has failed.
-- The allocation path itself follows binders directly and cannot disagree
-- with this machine-sized projection.
saturatingBinderCount :: [value] -> Int
saturatingBinderCount = Foldable.foldl' increment 0
 where
  increment count _
    | count == maxBound = count
    | otherwise = count + 1

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

leadingBinders :: HsType -> [TVarId]
leadingBinders (TypeForall variables _ body) =
  variables ++ leadingBinders body
leadingBinders _ = []

environmentTypes :: EnvDictionary -> [HsType]
environmentTypes environment =
  concatMap functionTypes (environmentFunctions environment)
  ++ concatMap deconstructorTypes (environmentDeconstructors environment)
  ++ concatMap (concatMap constraint_params . tclass_constraints)
      (Map.elems $ sClassEnv_tclasses classes)
  ++ concatMap instanceTypes
      (concat $ Map.elems $ sClassEnv_instances classes)
 where
  classes = environmentClasses environment

  functionTypes binding =
    functionResult binding
      : functionParameters binding
      ++ concatMap constraint_params (functionConstraints binding)

  deconstructorTypes binding =
    deconstructorInput binding
      : [ field
        | constructor <- deconstructorConstructors binding
        , field <- constructorFields constructor
        ]

  instanceTypes instanceDeclaration = concatMap constraint_params
    $ instance_head instanceDeclaration
      : instance_constraints instanceDeclaration

maximumRigidInType :: HsType -> Maybe TVarId
maximumRigidInType = Foldable.foldl' maximumMaybe Nothing
  . map Just . rigidIdentifiersInType

rigidIdentifiersInType :: HsType -> [TVarId]
rigidIdentifiersInType = Foldable.foldr collect [] . toSynthesisTypeStructure
 where
  collect variable identifiers = case variable of
    SharedType.FlexibleVariable{} -> identifiers
    SharedType.RigidVariable identifier -> identifier : identifiers

maximumMaybe :: Ord value => Maybe value -> Maybe value -> Maybe value
maximumMaybe Nothing right = right
maximumMaybe left Nothing = left
maximumMaybe (Just left) (Just right) = Just $ max left right
