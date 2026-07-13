{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Opaque, structurally validated source-declaration environment.
--
-- This layer owns cross-declaration namespaces and indexes. Kind inference,
-- dependency cycles, unknown-name policy, and backend class/instance semantics
-- are deliberately separate checks layered over this stable inventory.
module Language.Haskell.Synthesis.Environment
  ( Environment
  , EnvironmentError (..)
  , mkEnvironment
  , environmentDeclarations
  , typeDeclarationMap
  , valueSignatureMap
  , dataConstructorMap
  , classDeclarationMap
  , instanceDeclarationMap
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Generics (Generic)
import Language.Haskell.Synthesis.Constraint
import Language.Haskell.Synthesis.Declaration
import Language.Haskell.Synthesis.Name
import Language.Haskell.Synthesis.Type

data Environment typeVariable kindVariable annotation = Environment
  { reversedDeclarations ::
      [Declaration typeVariable kindVariable annotation]
  , typeDeclarationsByName ::
      Map Name (Declaration typeVariable kindVariable annotation)
  , valuesByName :: Map Name (ValueSignature typeVariable annotation)
  , constructorsByName ::
      Map Name (DataConstructor typeVariable annotation)
  , classesByName ::
      Map Name (Declaration typeVariable kindVariable annotation)
  , instancesByHead ::
      Map (Constraint (Type typeVariable))
        (Declaration typeVariable kindVariable annotation)
  , occupiedValueNames :: Set Name
  }
  deriving (Eq, Show, Functor, Generic)

instance
    (NFData typeVariable, NFData kindVariable, NFData annotation) =>
    NFData (Environment typeVariable kindVariable annotation)

data EnvironmentError typeVariable
  = InvalidEnvironmentDeclaration Int (DeclarationError typeVariable)
  | DuplicateTypeDeclaration Name
  | DuplicateValueDeclaration Name
  | DuplicateInstanceDeclaration (Constraint (Type typeVariable))
  deriving (Eq, Ord, Show, Generic)

instance NFData typeVariable => NFData (EnvironmentError typeVariable)

mkEnvironment
  :: Ord typeVariable
  => [Declaration typeVariable kindVariable annotation]
  -> Either (EnvironmentError typeVariable)
      (Environment typeVariable kindVariable annotation)
mkEnvironment declarations = foldM insert emptyEnvironment
  $ zip [0 ..] declarations

emptyEnvironment :: Environment typeVariable kindVariable annotation
emptyEnvironment = Environment [] Map.empty Map.empty Map.empty
  Map.empty Map.empty Set.empty

insert
  :: Ord typeVariable
  => Environment typeVariable kindVariable annotation
  -> (Int, Declaration typeVariable kindVariable annotation)
  -> Either (EnvironmentError typeVariable)
      (Environment typeVariable kindVariable annotation)
insert environment (index, declaration) = do
  either (Left . InvalidEnvironmentDeclaration index) Right
    $ validateDeclaration declaration
  indexed <- case declaration of
    TypeSynonymDeclaration _ name _ _ -> insertType name declaration environment
    DataTypeDeclaration _ name _ constructors -> do
      withType <- insertType name declaration environment
      foldM (flip insertConstructor) withType constructors
    AbstractTypeDeclaration _ name _ -> insertType name declaration environment
    ValueDeclaration signature -> insertValue signature environment
    ClassDeclaration _ name _ _ methods -> do
      withType <- insertType name declaration environment
      withClass <- Right withType
        { classesByName = Map.insert name declaration
            $ classesByName withType
        }
      foldM (flip insertValue) withClass methods
    InstanceDeclaration _ _ _ headConstraint ->
      insertInstance headConstraint declaration environment
  Right indexed
    { reversedDeclarations = declaration : reversedDeclarations indexed }

insertType
  :: Name
  -> Declaration typeVariable kindVariable annotation
  -> Environment typeVariable kindVariable annotation
  -> Either (EnvironmentError typeVariable)
      (Environment typeVariable kindVariable annotation)
insertType name declaration environment
  | Map.member name $ typeDeclarationsByName environment =
      Left $ DuplicateTypeDeclaration name
  | otherwise = Right environment
      { typeDeclarationsByName = Map.insert name declaration
          $ typeDeclarationsByName environment
      }

insertValue
  :: ValueSignature typeVariable annotation
  -> Environment typeVariable kindVariable annotation
  -> Either (EnvironmentError typeVariable)
      (Environment typeVariable kindVariable annotation)
insertValue signature environment = do
  reserveValueName (valueName signature) environment
    { valuesByName = Map.insert (valueName signature) signature
        $ valuesByName environment
    }

insertConstructor
  :: DataConstructor typeVariable annotation
  -> Environment typeVariable kindVariable annotation
  -> Either (EnvironmentError typeVariable)
      (Environment typeVariable kindVariable annotation)
insertConstructor constructor environment = do
  reserveValueName (constructorName constructor) environment
    { constructorsByName = Map.insert (constructorName constructor) constructor
        $ constructorsByName environment
    }

reserveValueName
  :: Name
  -> Environment typeVariable kindVariable annotation
  -> Either (EnvironmentError typeVariable)
      (Environment typeVariable kindVariable annotation)
reserveValueName name environment
  | name `Set.member` occupiedValueNames environment =
      Left $ DuplicateValueDeclaration name
  | otherwise = Right environment
      { occupiedValueNames = Set.insert name $ occupiedValueNames environment }

insertInstance
  :: Ord typeVariable
  => Constraint (Type typeVariable)
  -> Declaration typeVariable kindVariable annotation
  -> Environment typeVariable kindVariable annotation
  -> Either (EnvironmentError typeVariable)
      (Environment typeVariable kindVariable annotation)
insertInstance headConstraint declaration environment
  | Map.member headConstraint $ instancesByHead environment =
      Left $ DuplicateInstanceDeclaration headConstraint
  | otherwise = Right environment
      { instancesByHead = Map.insert headConstraint declaration
          $ instancesByHead environment
      }

environmentDeclarations
  :: Environment typeVariable kindVariable annotation
  -> [Declaration typeVariable kindVariable annotation]
environmentDeclarations = reverse . reversedDeclarations

typeDeclarationMap
  :: Environment typeVariable kindVariable annotation
  -> Map Name (Declaration typeVariable kindVariable annotation)
typeDeclarationMap = typeDeclarationsByName

valueSignatureMap
  :: Environment typeVariable kindVariable annotation
  -> Map Name (ValueSignature typeVariable annotation)
valueSignatureMap = valuesByName

dataConstructorMap
  :: Environment typeVariable kindVariable annotation
  -> Map Name (DataConstructor typeVariable annotation)
dataConstructorMap = constructorsByName

classDeclarationMap
  :: Environment typeVariable kindVariable annotation
  -> Map Name (Declaration typeVariable kindVariable annotation)
classDeclarationMap = classesByName

instanceDeclarationMap
  :: Environment typeVariable kindVariable annotation
  -> Map (Constraint (Type typeVariable))
      (Declaration typeVariable kindVariable annotation)
instanceDeclarationMap = instancesByHead
