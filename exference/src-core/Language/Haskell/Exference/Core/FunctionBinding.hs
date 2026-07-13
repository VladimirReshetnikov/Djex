{-# LANGUAGE DeriveGeneric #-}

module Language.Haskell.Exference.Core.FunctionBinding
  ( ConstructorBinding (..)
  , DeconstructorBinding (..)
  , EnvDictionary (..)
  , FunctionBinding (..)
  , functionBindingFromType
  )
where

import Control.DeepSeq (NFData (..))
import GHC.Generics (Generic)

import Language.Haskell.Exference.Core.Score
import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils (splitArrowResultParams)

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
-- flat binding shape. Quantifier IDs are local to the signature; constraints
-- and arrow parameters remain explicit search inputs, while the caller-owned
-- penalty is attached without changing the type's structure.
functionBindingFromType
  :: QualifiedName
  -> Penalty
  -> HsType
  -> FunctionBinding
functionBindingFromType name penalty signature = FunctionBinding
  result name penalty constraints parameters
 where
  (result, parameters, _, constraints) = splitArrowResultParams signature

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

data EnvDictionary = EnvDictionary
  { environmentFunctions :: [FunctionBinding]
  , environmentDeconstructors :: [DeconstructorBinding]
  , environmentClasses :: StaticClassEnv
  }
  deriving (Generic, Show)

instance NFData EnvDictionary
