{-# LANGUAGE DeriveGeneric #-}

module Language.Haskell.Exference.Core.FunctionBinding
  ( ConstructorBinding (..)
  , DeconstructorBinding (..)
  , DeconstructorValidationError (..)
  , EnvDictionary (..)
  , FunctionBinding (..)
  , functionBindingFromType
  , functionBindingType
  , functionBindingSignature
  , functionBindingTypes
  , deconstructorBindingType
  , deconstructorBindingTypes
  , environmentBindingTypes
  , mapFunctionBindingTypes
  , mapDeconstructorBindingTypes
  , validateDeconstructorBinding
  )
where

import Control.DeepSeq (NFData (..))
import Data.Foldable (traverse_)
import qualified Data.IntSet as IntSet
import GHC.Generics (Generic)

import Language.Haskell.Exference.Core.Internal.FlexibleIds
  ( flexibleIdentifiers )
import Language.Haskell.Exference.Core.Score
import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
  ( splitArrowResultParams
  , typeConstructorHead
  )
import qualified Language.Haskell.Synthesis.Name as SynthesisName
import qualified Language.Haskell.Synthesis.Type as SharedType

data FunctionBinding = FunctionBinding
  { functionResult :: HsType
  , functionName :: QualifiedName
  , functionPenalty :: Penalty
  , functionConstraints :: [HsConstraint]
  , functionParameters :: [HsType]
  }
  deriving (Eq, Generic, Show)

instance NFData FunctionBinding

-- | Lower one fully quantified source signature into the search engine's
-- flat binding shape. Only its complete leading prenex chain is opened;
-- a forall below an arrow remains visible in 'functionResult' for the checked
-- environment boundary to reject. Quantifier IDs are local to the signature;
-- cross-layer shadows are alpha-normalized before their binders disappear.
-- Leading constraints and arrow parameters remain explicit search inputs,
-- while the caller-owned penalty is attached without changing the remaining
-- type structure.
functionBindingFromType
  :: QualifiedName
  -> Penalty
  -> HsType
  -> FunctionBinding
functionBindingFromType name penalty signature = FunctionBinding
  result name penalty constraints parameters
 where
  (result, parameters, _, constraints) = splitArrowResultParams signature

-- | Reconstruct the monotype consumed when applying a search binding.
functionBindingType :: FunctionBinding -> HsType
functionBindingType binding = SharedType.functionType
  (functionParameters binding) (functionResult binding)

-- | Reconstruct the complete implicitly quantified source signature retained
-- by the flat search record. Leading binder identities are intentionally not
-- recovered: they were opened by 'functionBindingFromType', while their free
-- occurrences remain available to the shared declaration checker.
functionBindingSignature :: FunctionBinding -> HsType
functionBindingSignature binding = TypeForall []
  (functionConstraints binding) (functionBindingType binding)

-- | Every independently stored type in a function binding, in historical
-- result, parameter, then constraint-argument order.
functionBindingTypes :: FunctionBinding -> [HsType]
functionBindingTypes binding = functionResult binding
  : functionParameters binding
  ++ concatMap constraint_params (functionConstraints binding)

-- | Transform every independently stored type in a function binding exactly
-- once. This includes types nested in the separately stored constraints.
mapFunctionBindingTypes
  :: (HsType -> HsType)
  -> FunctionBinding
  -> FunctionBinding
mapFunctionBindingTypes transform binding = binding
  { functionResult = transform $ functionResult binding
  , functionConstraints = map (fmap transform)
      $ functionConstraints binding
  , functionParameters = map transform $ functionParameters binding
  }

data ConstructorBinding = ConstructorBinding
  { constructorName :: QualifiedName
  , constructorFields :: [HsType]
  }
  deriving (Eq, Generic, Show)

instance NFData ConstructorBinding

data DeconstructorBinding = DeconstructorBinding
  { deconstructorInput :: HsType
  , deconstructorConstructors :: [ConstructorBinding]
  , deconstructorRecursive :: Bool
  }
  deriving (Eq, Generic, Show)

instance NFData DeconstructorBinding

-- | Structural failures that would let a deconstructor manufacture a pattern
-- match for an unrelated nominal type or introduce undeclared existential
-- field variables.
data DeconstructorValidationError
  = MissingDeconstructorNominalHead HsType
  | FunctionDeconstructorHead QualifiedName
  | UnboundDeconstructorFields
      QualifiedName -- ^ Nominal datatype head.
      QualifiedName -- ^ Constructor whose fields escape the parameter scope.
      [TVarId]      -- ^ Escaping flexible IDs, in ascending order.
  deriving (Eq, Generic, Show)

instance NFData DeconstructorValidationError

-- | Validate the elimination invariant shared by search-environment sealing
-- and independent expression checking.
validateDeconstructorBinding
  :: DeconstructorBinding
  -> Either DeconstructorValidationError ()
validateDeconstructorBinding binding = do
  headName <- case typeConstructorHead input of
    Nothing -> Left $ MissingDeconstructorNominalHead input
    Just name
      | SynthesisName.nameSpecial name
          == Just SynthesisName.FunctionConstructor ->
            Left $ FunctionDeconstructorHead name
      | otherwise -> Right name
  traverse_ (validateConstructor headName parameters)
    $ deconstructorConstructors binding
 where
  input = deconstructorInput binding
  parameters = flexibleIdentifiers input

  validateConstructor headName parameters' constructor
    | IntSet.null unbound = Right ()
    | otherwise = Left $ UnboundDeconstructorFields
        headName (constructorName constructor) $ IntSet.toAscList unbound
   where
    unbound = IntSet.unions
      (map flexibleIdentifiers $ constructorFields constructor)
      `IntSet.difference` parameters'

-- | Reconstruct the synthetic elimination type used by validation. Fields of
-- every constructor precede the datatype result in declaration order.
deconstructorBindingType :: DeconstructorBinding -> HsType
deconstructorBindingType binding = SharedType.functionType
  (concatMap constructorFields $ deconstructorConstructors binding)
  (deconstructorInput binding)

-- | Every independently stored type in a deconstructor binding.
deconstructorBindingTypes :: DeconstructorBinding -> [HsType]
deconstructorBindingTypes binding = deconstructorInput binding
  : [ field
    | constructor <- deconstructorConstructors binding
    , field <- constructorFields constructor
    ]

-- | Transform the datatype input and every constructor field exactly once.
mapDeconstructorBindingTypes
  :: (HsType -> HsType)
  -> DeconstructorBinding
  -> DeconstructorBinding
mapDeconstructorBindingTypes transform binding = binding
  { deconstructorInput = transform $ deconstructorInput binding
  , deconstructorConstructors = map transformConstructor
      $ deconstructorConstructors binding
  }
 where
  transformConstructor constructor = constructor
    { constructorFields = map transform $ constructorFields constructor }

data EnvDictionary = EnvDictionary
  { environmentFunctions :: [FunctionBinding]
  , environmentDeconstructors :: [DeconstructorBinding]
  , environmentClasses :: StaticClassEnv
  }
  deriving (Generic, Show)

instance NFData EnvDictionary

-- | Every independently stored search-capability type. Class and instance
-- assumptions are deliberately excluded because they belong to
-- 'StaticClassEnv', not to these binding records.
environmentBindingTypes :: EnvDictionary -> [HsType]
environmentBindingTypes environment =
  concatMap functionBindingTypes (environmentFunctions environment)
  ++ concatMap deconstructorBindingTypes
      (environmentDeconstructors environment)
