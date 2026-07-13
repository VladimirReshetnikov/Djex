{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

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
  , recursiveDataTypeNames
  , groundDeclarationKinds
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Data.Graph (SCC (..), stronglyConnComp)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
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
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (NFData typeVariable, NFData annotation) =>
    NFData (DataConstructor typeVariable annotation)

data ValueSignature typeVariable annotation = ValueSignature
  { valueAnnotation :: annotation
  , valueName :: Name
  , valueType :: Type typeVariable
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

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
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

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
  | UndeclaredSynonymVariables Name [typeVariable]
  | UndeclaredDataVariables Name [typeVariable]
  | UndeclaredSuperclassVariables Name [typeVariable]
  | UndeclaredInstanceVariables Name [typeVariable]
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
    validateBoundVariables (UndeclaredSynonymVariables name)
      parameters [body]
  DataTypeDeclaration _ name parameters constructors -> do
    validateTypeName name
    validateParameters parameters
    validateDistinct DuplicateConstructorName $ map constructorName constructors
    mapM_ validateConstructor constructors
    validateBoundVariables (UndeclaredDataVariables name) parameters
      $ concatMap constructorFields constructors
  AbstractTypeDeclaration _ name _ -> validateTypeName name
  ValueDeclaration signature -> validateValue signature
  ClassDeclaration _ name parameters superclasses methods -> do
    validateClassName name
    validateParameters parameters
    mapM_ validateDeclarationConstraint superclasses
    validateBoundVariables (UndeclaredSuperclassVariables name) parameters
      [ argument
      | superclass <- superclasses
      , argument <- constraintArguments superclass
      ]
    validateDistinct DuplicateMethodName $ map valueName methods
    mapM_ validateValue methods
  InstanceDeclaration _ variables prerequisites headConstraint -> do
    validateDistinct DuplicateTypeParameter variables
    mapM_ validateDeclarationConstraint prerequisites
    validateDeclarationConstraint headConstraint
    let unbound = referencedVariables
          [ argument
          | constraint <- headConstraint : prerequisites
          , argument <- constraintArguments constraint
          ] `Set.difference` Set.fromList variables
    unless (Set.null unbound) $ Left $ UndeclaredInstanceVariables
      (constraintClass headConstraint) $ Set.toAscList unbound
 where
  validateTypeName name = unless (isTypeConstructorName name) $
    Left $ InvalidDeclaredTypeName name

  validateClassName name = case validateConstraintClassName name of
    Left _ -> Left $ InvalidClassName name
    Right () -> Right ()

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

  validateBoundVariables failure parameters types =
    let unbound = referencedVariables types `Set.difference`
          Set.fromList (map parameterVariable parameters)
    in unless (Set.null unbound) $ Left $ failure $ Set.toAscList unbound

  referencedVariables = Set.unions . map freeVariables

  validateDeclarationConstraint constraint = do
    validateClassName $ constraintClass constraint
    mapM_ validateDeclaredType $ constraintArguments constraint

  validateDeclaredType = either (Left . InvalidDeclarationType) Right
    . validateType

-- | Return every datatype in a recursive strongly connected component.
-- Constructor fields must already have type synonyms expanded: classifying
-- raw aliases can both hide real edges and invent edges through phantom
-- parameters. Duplicate datatype heads are merged deterministically so this
-- structural query remains total; 'Language.Haskell.Synthesis.Environment'
-- is the separate boundary that rejects such duplicates.
recursiveDataTypeNames
  :: [Declaration typeVariable kindVariable annotation]
  -> Set.Set Name
recursiveDataTypeNames declarations = Set.fromList
  [ name
  | CyclicSCC names <- stronglyConnComp nodes
  , name <- names
  ]
 where
  constructorsByName = Map.fromListWith (++)
    [ (name, constructors)
    | DataTypeDeclaration _ name _ constructors <- declarations
    ]
  dataNames = Map.keysSet constructorsByName
  nodes =
    [ (name, name, Set.toAscList $ dependencies constructors)
    | (name, constructors) <- Map.toAscList constructorsByName
    ]
  dependencies constructors = Set.intersection dataNames $ Set.unions
    [ typeConstructors field
    | constructor <- constructors
    , field <- constructorFields constructor
    ]

-- | Ground every explicit declaration kind without changing source-type
-- variables, annotations, or declaration shape.
groundDeclarationKinds
  :: Declaration typeVariable kindVariable annotation
  -> Either kindVariable (Declaration typeVariable Void annotation)
groundDeclarationKinds declaration = case declaration of
  TypeSynonymDeclaration annotation name parameters body ->
    TypeSynonymDeclaration annotation name
      <$> mapM groundParameter parameters <*> pure body
  DataTypeDeclaration annotation name parameters constructors ->
    DataTypeDeclaration annotation name
      <$> mapM groundParameter parameters <*> pure constructors
  AbstractTypeDeclaration annotation name kind ->
    AbstractTypeDeclaration annotation name <$> groundKind kind
  ValueDeclaration signature -> Right $ ValueDeclaration signature
  ClassDeclaration annotation name parameters superclasses methods ->
    ClassDeclaration annotation name
      <$> mapM groundParameter parameters
      <*> pure superclasses <*> pure methods
  InstanceDeclaration annotation variables prerequisites headConstraint ->
    Right $ InstanceDeclaration annotation variables prerequisites
      headConstraint
 where
  groundParameter parameter = TypeParameter
    (parameterVariable parameter)
    <$> traverse groundKind (parameterKind parameter)

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
