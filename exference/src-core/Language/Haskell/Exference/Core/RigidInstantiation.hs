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
  (Foldable.foldl' maximumMaybe Nothing
    $ map maximumRigidInType types)
  (supplyFromIdentifiers $ concatMap rigidIdentifiersInType types)
  (firstJust $ map firstRigidForallBinder types)
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
  case firstJust
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
leadingBinders (TypeForallNative variables _ body) = do
  identifiers <- traverse flexibleBinder variables
  remaining <- leadingBinders body
  pure $ identifiers ++ remaining
 where
  flexibleBinder variable = case variable of
    SharedType.FlexibleVariable identifier -> Right identifier
    SharedType.RigidVariable identifier -> Left
      $ RigidForallBinderCannotBeInstantiated identifier
leadingBinders _ = Right []

-- Locate forbidden rigid variables specifically in binder position.  A
-- generic fold cannot distinguish a rigid occurrence from a rigid binder.
firstRigidForallBinder :: HsType -> Maybe TVarId
firstRigidForallBinder typeExpression = case typeExpression of
  TypeVar{} -> Nothing
  TypeConstant{} -> Nothing
  TypeCons{} -> Nothing
  TypeArrow parameter result -> firstJust
    [firstRigidForallBinder parameter, firstRigidForallBinder result]
  TypeApp function argument -> firstJust
    [firstRigidForallBinder function, firstRigidForallBinder argument]
  TypeTuple _ elements -> firstJust $ map firstRigidForallBinder elements
  TypeForallNative variables constraints body ->
    firstJust
      ( map rigidBinder variables
        ++ map (firstJust . map firstRigidForallBinder . constraint_params)
          constraints
        ++ [firstRigidForallBinder body]
      )
 where
  rigidBinder variable = case variable of
    SharedType.FlexibleVariable _ -> Nothing
    SharedType.RigidVariable identifier -> Just identifier

firstJust :: [Maybe value] -> Maybe value
firstJust [] = Nothing
firstJust (Just value : _) = Just value
firstJust (Nothing : remaining) = firstJust remaining

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
rigidIdentifiersInType = Foldable.foldr collect []
 where
  collect variable identifiers = case variable of
    SharedType.FlexibleVariable{} -> identifiers
    SharedType.RigidVariable identifier -> identifier : identifiers

maximumMaybe :: Ord value => Maybe value -> Maybe value -> Maybe value
maximumMaybe Nothing right = right
maximumMaybe left Nothing = left
maximumMaybe (Just left) (Just right) = Just $ max left right
