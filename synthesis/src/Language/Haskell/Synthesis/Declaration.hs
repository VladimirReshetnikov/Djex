{-# LANGUAGE DeriveGeneric #-}

-- | Parser-independent source declarations shared by synthesis frontends.
-- Backend caches, ratings, proof premises, and instance-solving indexes are
-- lowerings of these values rather than alternate declaration syntax.
module Language.Haskell.Synthesis.Declaration
  ( TypeParameter (..)
  , DataConstructor (..)
  , ValueSignature (..)
  , Declaration (..)
  , DeclarationError (..)
  , validateDeclaration
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Language.Haskell.Synthesis.Constraint
import Language.Haskell.Synthesis.Kind
import Language.Haskell.Synthesis.Name
import Language.Haskell.Synthesis.Type

data TypeParameter typeVariable kindVariable = TypeParameter
  { parameterVariable :: typeVariable
  , parameterKind :: Maybe (Kind kindVariable)
  }
  deriving (Eq, Ord, Show, Generic)

instance (NFData typeVariable, NFData kindVariable) =>
    NFData (TypeParameter typeVariable kindVariable)

data DataConstructor typeVariable annotation = DataConstructor
  { constructorAnnotation :: annotation
  , constructorName :: Name
  , constructorFields :: [Type typeVariable]
  }
  deriving (Eq, Ord, Show, Generic)

instance (NFData typeVariable, NFData annotation) =>
    NFData (DataConstructor typeVariable annotation)

data ValueSignature typeVariable annotation = ValueSignature
  { valueAnnotation :: annotation
  , valueName :: Name
  , valueType :: Type typeVariable
  }
  deriving (Eq, Ord, Show, Generic)

instance (NFData typeVariable, NFData annotation) =>
    NFData (ValueSignature typeVariable annotation)

data Declaration typeVariable kindVariable annotation
  = TypeSynonymDeclaration
      annotation Name [TypeParameter typeVariable kindVariable]
      (Type typeVariable)
  | DataTypeDeclaration
      annotation Name [TypeParameter typeVariable kindVariable]
      [DataConstructor typeVariable annotation]
  | AbstractTypeDeclaration annotation Name (Kind kindVariable)
  | ValueDeclaration (ValueSignature typeVariable annotation)
  | ClassDeclaration
      annotation Name [TypeParameter typeVariable kindVariable]
      [Constraint (Type typeVariable)]
      [ValueSignature typeVariable annotation]
  | InstanceDeclaration
      annotation [typeVariable]
      [Constraint (Type typeVariable)]
      (Constraint (Type typeVariable))
  deriving (Eq, Ord, Show, Generic)

instance
    (NFData typeVariable, NFData kindVariable, NFData annotation) =>
    NFData (Declaration typeVariable kindVariable annotation)

data DeclarationError typeVariable
  = InvalidDeclaredTypeName Name
  | InvalidDeclaredValueName Name
  | InvalidDataConstructorName Name
  | InvalidClassName Name
  | DuplicateTypeParameter typeVariable
  | DuplicateConstructorName Name
  | DuplicateMethodName Name
  | InvalidDeclarationType (TypeError typeVariable)
  deriving (Eq, Ord, Show, Generic)

instance NFData typeVariable => NFData (DeclarationError typeVariable)

validateDeclaration
  :: Ord typeVariable
  => Declaration typeVariable kindVariable annotation
  -> Either (DeclarationError typeVariable) ()
validateDeclaration declaration = case declaration of
  TypeSynonymDeclaration _ name parameters body -> do
    validateTypeName name
    validateParameters parameters
    validateDeclaredType body
  DataTypeDeclaration _ name parameters constructors -> do
    validateTypeName name
    validateParameters parameters
    validateDistinct DuplicateConstructorName $ map constructorName constructors
    mapM_ validateConstructor constructors
  AbstractTypeDeclaration _ name _ -> validateTypeName name
  ValueDeclaration signature -> validateValue signature
  ClassDeclaration _ name parameters superclasses methods -> do
    validateClassName name
    validateParameters parameters
    mapM_ validateConstraint superclasses
    validateDistinct DuplicateMethodName $ map valueName methods
    mapM_ validateValue methods
  InstanceDeclaration _ variables prerequisites headConstraint -> do
    validateDistinct DuplicateTypeParameter variables
    mapM_ validateConstraint prerequisites
    validateConstraint headConstraint
 where
  validateTypeName name = unless (isTypeConstructorName name) $
    Left $ InvalidDeclaredTypeName name

  validateClassName name = unless (isOrdinaryConstructor name) $
    Left $ InvalidClassName name

  validateParameters parameters = validateDistinct DuplicateTypeParameter
    $ map parameterVariable parameters

  validateConstructor constructor = do
    unless (isConstructorName $ constructorName constructor) $
      Left $ InvalidDataConstructorName $ constructorName constructor
    mapM_ validateDeclaredType $ constructorFields constructor

  validateValue signature = do
    unless (isValueName $ valueName signature) $
      Left $ InvalidDeclaredValueName $ valueName signature
    validateDeclaredType $ valueType signature

  validateConstraint constraint = do
    validateClassName $ constraintClass constraint
    mapM_ validateDeclaredType $ constraintArguments constraint

  validateDeclaredType = either (Left . InvalidDeclarationType) Right
    . validateType

isOrdinaryConstructor :: Name -> Bool
isOrdinaryConstructor name =
  nameLexicalClass name == ConstructorLike && nameSpecial name == Nothing

isTypeConstructorName :: Name -> Bool
isTypeConstructorName name =
  nameLexicalClass name == ConstructorLike &&
    nameSpecial name /= Just ConsConstructor

isConstructorName :: Name -> Bool
isConstructorName name =
  nameLexicalClass name == ConstructorLike &&
    nameSpecial name /= Just FunctionConstructor

isValueName :: Name -> Bool
isValueName name = nameLexicalClass name == VariableLike &&
  nameSpecial name == Nothing && nameSpelling name /= Just "_"

validateDistinct
  :: Ord value
  => (value -> error)
  -> [value]
  -> Either error ()
validateDistinct makeError = go Set.empty
  where
    go _ [] = Right ()
    go seen (value : remaining)
      | value `Set.member` seen = Left $ makeError value
      | otherwise = go (Set.insert value seen) remaining
