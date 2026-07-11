{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Haskell.Exference.Core.FunctionBinding
  ( ConstructorBinding (..)
  , DeconstructorBinding (..)
  , EnvDictionary (..)
  , FunctionBinding (..)
  )
where

import Control.DeepSeq (NFData (..))
import Data.Data (Data)
import GHC.Generics (Generic)

import Language.Haskell.Exference.Core.Score
import Language.Haskell.Exference.Core.Types

data FunctionBinding = FunctionBinding
  { functionResult :: HsType
  , functionName :: QualifiedName
  , functionPenalty :: Penalty
  , functionConstraints :: [HsConstraint]
  , functionParameters :: [HsType]
  }
  deriving (Data, Eq, Generic, Show)

instance NFData FunctionBinding

data ConstructorBinding = ConstructorBinding
  { constructorName :: QualifiedName
  , constructorFields :: [HsType]
  }
  deriving (Data, Eq, Generic, Show)

instance NFData ConstructorBinding

data DeconstructorBinding = DeconstructorBinding
  { deconstructorInput :: HsType
  , deconstructorConstructors :: [ConstructorBinding]
  , deconstructorRecursive :: Bool
  }
  deriving (Data, Eq, Generic, Show)

instance NFData DeconstructorBinding

data EnvDictionary = EnvDictionary
  { environmentFunctions :: [FunctionBinding]
  , environmentDeconstructors :: [DeconstructorBinding]
  , environmentClasses :: StaticClassEnv
  }
  deriving (Data, Generic, Show)

instance NFData EnvDictionary
