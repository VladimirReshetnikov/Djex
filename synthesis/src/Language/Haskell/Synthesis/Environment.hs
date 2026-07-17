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
  , groundEnvironmentKinds
  , mapEnvironmentKindVariables
  , adjustEnvironmentDataTypeAnnotations
  , environmentDeclarations
  , typeDeclarationMap
  , valueSignatureMap
  , dataConstructorMap
  , classDeclarationMap
  , instanceDeclarationMap
  , repeatedInstanceHeadsInFirstRepetitionOrder
  ) where

import Control.DeepSeq (NFData (rnf))
import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Void (Void)
import GHC.Generics (Generic)
import Language.Haskell.Synthesis.Constraint
import Language.Haskell.Synthesis.Declaration
import Language.Haskell.Synthesis.Internal.InstanceHead
  ( InstanceHeadKey
  , instanceHeadKey
  , repeatedInstanceHeadsInFirstRepetitionOrder
  )
import Language.Haskell.Synthesis.Name
import Language.Haskell.Synthesis.Type

-- 'Generic' is intentionally absent: although the constructor is private,
-- 'GHC.Generics.to' would otherwise let downstream callers rebuild mutually
-- inconsistent declaration indexes.
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
  , canonicalInstanceHeads :: Set (InstanceHeadKey typeVariable)
  , occupiedValueNames :: Set Name
  }
  deriving (Eq, Show, Functor)

instance
    (NFData typeVariable, NFData kindVariable, NFData annotation) =>
    NFData (Environment typeVariable kindVariable annotation) where
  rnf (Environment declarations types values constructors classes instances
      canonicalHeads occupiedNames) =
    rnf declarations `seq`
    rnf types `seq`
    rnf values `seq`
    rnf constructors `seq`
    rnf classes `seq`
    rnf instances `seq`
    rnf canonicalHeads `seq`
    rnf occupiedNames

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

-- | Ground every explicit kind while preserving the environment's validated
-- declaration order and indexes. The source-ordered declaration pass fixes
-- which unsolved kind is reported first; after it succeeds, grounding the
-- declaration-valued indexes is total because they contain the same sealed
-- declarations. No namespace validation or reindexing is repeated.
groundEnvironmentKinds
  :: Environment typeVariable kindVariable annotation
  -> Either kindVariable (Environment typeVariable Void annotation)
groundEnvironmentKinds environment = do
  declarations <- mapM groundDeclarationKinds
    $ environmentDeclarations environment
  groundedTypes <- traverse groundDeclarationKinds
    $ typeDeclarationsByName environment
  groundedClasses <- traverse groundDeclarationKinds
    $ classesByName environment
  groundedInstances <- traverse groundDeclarationKinds
    $ instancesByHead environment
  pure Environment
    { reversedDeclarations = reverse declarations
    , typeDeclarationsByName = groundedTypes
    , valuesByName = valuesByName environment
    , constructorsByName = constructorsByName environment
    , classesByName = groundedClasses
    , instancesByHead = groundedInstances
    , canonicalInstanceHeads = canonicalInstanceHeads environment
    , occupiedValueNames = occupiedValueNames environment
    }

-- | Change only explicit kind-variable identities while preserving the
-- validated declaration order and every structural index. This is total and
-- does not repeat namespace validation; in particular, a grounded
-- @Environment typeVariable Void annotation@ can be weakened to any
-- kind-variable representation with 'Data.Void.absurd'.
mapEnvironmentKindVariables
  :: (kindVariable -> kindVariable')
  -> Environment typeVariable kindVariable annotation
  -> Environment typeVariable kindVariable' annotation
mapEnvironmentKindVariables transform environment = Environment
  { reversedDeclarations = map mappedDeclaration
      $ reversedDeclarations environment
  , typeDeclarationsByName = Map.map mappedDeclaration
      $ typeDeclarationsByName environment
  , valuesByName = valuesByName environment
  , constructorsByName = constructorsByName environment
  , classesByName = Map.map mappedDeclaration
      $ classesByName environment
  , instancesByHead = Map.map mappedDeclaration
      $ instancesByHead environment
  , canonicalInstanceHeads = canonicalInstanceHeads environment
  , occupiedValueNames = occupiedValueNames environment
  }
 where
  mappedDeclaration = mapDeclarationKindVariables transform

-- | Change only the top-level annotation of every datatype declaration.
--
-- The declaration list and type index contain the same sealed declarations,
-- so both views must be updated together.  Constructor annotations and every
-- structural field remain untouched; consequently no namespace validation or
-- index reconstruction is necessary.
adjustEnvironmentDataTypeAnnotations
  :: (Name -> annotation -> annotation)
  -> Environment typeVariable kindVariable annotation
  -> Environment typeVariable kindVariable annotation
adjustEnvironmentDataTypeAnnotations adjust environment = environment
  { reversedDeclarations = map adjustDeclaration
      $ reversedDeclarations environment
  , typeDeclarationsByName = Map.map adjustDeclaration
      $ typeDeclarationsByName environment
  }
 where
  adjustDeclaration declaration = case declaration of
    DataTypeDeclaration annotation name parameters constructors ->
      DataTypeDeclaration (adjust name annotation) name parameters constructors
    _ -> declaration

emptyEnvironment :: Environment typeVariable kindVariable annotation
emptyEnvironment = Environment [] Map.empty Map.empty Map.empty
  Map.empty Map.empty Set.empty Set.empty

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
    InstanceDeclaration _ variables _ headConstraint ->
      insertInstance variables headConstraint declaration environment
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
  => [typeVariable]
  -> Constraint (Type typeVariable)
  -> Declaration typeVariable kindVariable annotation
  -> Environment typeVariable kindVariable annotation
  -> Either (EnvironmentError typeVariable)
      (Environment typeVariable kindVariable annotation)
insertInstance variables headConstraint declaration environment
  | canonicalHead `Set.member` canonicalInstanceHeads environment =
      Left $ DuplicateInstanceDeclaration headConstraint
  | otherwise = Right environment
      { instancesByHead = Map.insert headConstraint declaration
          $ instancesByHead environment
      , canonicalInstanceHeads = Set.insert canonicalHead
          $ canonicalInstanceHeads environment
      }
 where
  canonicalHead = instanceHeadKey variables headConstraint

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
