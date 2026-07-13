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
  ) where

import Control.DeepSeq (NFData)
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils (forallify)
import qualified Language.Haskell.Synthesis.Type as SharedType

-- | The finite 'Int' identity space cannot hold all skolems required by a
-- query.  The maximum is reported as 'Nothing' only when the environment and
-- goal contain no pre-existing rigid variables.
data RigidInstantiationError = RigidIdentifierSupplyExhausted
  { maximumPreexistingRigidIdentifier :: Maybe TVarId
  , requestedRigidIdentifierCount :: Int
  }
  deriving (Eq, Show, Generic)

instance NFData RigidInstantiationError

-- | The greatest rigid identifier in a sealed environment.  Computing this
-- once keeps repeated session queries independent of environment size.
newtype RigidInstantiationContext = RigidInstantiationContext
  { maximumEnvironmentRigidIdentifier :: Maybe TVarId
  }
  deriving (Eq, Show, Generic)

instance NFData RigidInstantiationContext

-- | Cache the environment-wide rigid maximum used by every later query.
mkRigidInstantiationContext :: EnvDictionary -> RigidInstantiationContext
mkRigidInstantiationContext = RigidInstantiationContext
  . Foldable.foldl' maximumMaybe Nothing
  . map maximumRigidInType
  . environmentTypes

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
planRigidInstantiation context extraConstraints goal
  | null binders = Right $ RigidInstantiationPlan []
  | lastIdentifier > toInteger (maxBound :: TVarId) =
      Left $ RigidIdentifierSupplyExhausted maximumRigid binderCount
  | otherwise = Right $ RigidInstantiationPlan
      $ zip binders $ map fromInteger [firstIdentifier .. lastIdentifier]
 where
  binders = leadingBinders $ forallify goal
  binderCount = length binders
  maximumRigid = Foldable.foldl' maximumMaybe
    (maximumEnvironmentRigidIdentifier context)
    $ map maximumRigidInType
    $ goal : concatMap constraint_params extraConstraints
  firstIdentifier = max 0 $ maybe 0 ((+ 1) . toInteger) maximumRigid
  lastIdentifier = firstIdentifier + toInteger binderCount - 1

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
maximumRigidInType = Foldable.foldl' collect Nothing . toSynthesisTypeStructure
 where
  collect current variable = case variable of
    SharedType.FlexibleVariable{} -> current
    SharedType.RigidVariable identifier -> maximumMaybe current
      $ Just identifier

maximumMaybe :: Ord value => Maybe value -> Maybe value -> Maybe value
maximumMaybe Nothing right = right
maximumMaybe left Nothing = left
maximumMaybe (Just left) (Just right) = Just $ max left right
