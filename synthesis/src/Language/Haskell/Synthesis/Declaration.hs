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
  , declarationSubjectName
  , declarationLookupNames
  , declarationTermSignatures
  , declarationTypeVariables
  , validateDeclaration
  , recursiveDataTypeNames
  , mapDeclarationTypeVariables
  , mapDeclarationKindVariables
  , groundDeclarationKinds
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Data.Foldable (toList)
import Data.Graph (SCC (..), stronglyConnComp)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import GHC.Generics (Generic)
import Language.Haskell.Synthesis.Collection (distinctOn, firstDuplicate)
import Language.Haskell.Synthesis.Constraint
import Language.Haskell.Synthesis.Kind
import Language.Haskell.Synthesis.Name
import Language.Haskell.Synthesis.Type

-- | One declared type parameter and its optional explicit kind.
data TypeParameter typeVariable kindVariable = TypeParameter
  { parameterVariable :: typeVariable
  , parameterKind :: Maybe (Kind kindVariable)
  }
  deriving (Eq, Ord, Show, Generic)

instance (NFData typeVariable, NFData kindVariable) =>
    NFData (TypeParameter typeVariable kindVariable)

-- | One constructor belonging to a shared datatype declaration.
data DataConstructor typeVariable annotation = DataConstructor
  { constructorAnnotation :: annotation
  , constructorName :: Name
  , constructorFields :: [Type typeVariable]
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (NFData typeVariable, NFData annotation) =>
    NFData (DataConstructor typeVariable annotation)

-- | A named value or class method and its source type.
data ValueSignature typeVariable annotation = ValueSignature
  { valueAnnotation :: annotation
  , valueName :: Name
  , valueType :: Type typeVariable
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (NFData typeVariable, NFData annotation) =>
    NFData (ValueSignature typeVariable annotation)

-- | Source declarations understood by both checked backends.
--
-- Annotations are deliberately opaque to the foundation; frontends commonly
-- use them for locations or ratings and can erase them after reporting.
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

-- | A structural or lexical failure within one declaration.
--
-- Cross-declaration namespace conflicts belong to
-- 'Language.Haskell.Synthesis.Environment.EnvironmentError'.
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

-- | The nominal subject used to identify a declaration in diagnostics.
-- Instances have no declared value or type name, so their subject is the
-- class at the head of the instance.
declarationSubjectName
  :: Declaration typeVariable kindVariable annotation
  -> Name
declarationSubjectName declaration = case declaration of
  TypeSynonymDeclaration _ name _ _ -> name
  DataTypeDeclaration _ name _ _ -> name
  AbstractTypeDeclaration _ name _ -> name
  ValueDeclaration signature -> valueName signature
  ClassDeclaration _ name _ _ _ -> name
  InstanceDeclaration _ _ _ headConstraint -> constraintClass headConstraint

-- | Every name by which a declaration can be found: its nominal subject plus
-- any data constructors or class methods it owns. Instance lookup uses the
-- class at its head, consistently with 'declarationSubjectName', without
-- claiming that the instance owns that class. A declaration such as
-- @data T = T@ exposes its shared type/value spelling only once.
declarationLookupNames
  :: Declaration typeVariable kindVariable annotation
  -> [Name]
declarationLookupNames declaration = distinctOn id
  $ declarationSubjectName declaration : case declaration of
      DataTypeDeclaration _ _ _ constructors ->
        map constructorName constructors
      ClassDeclaration _ _ _ _ methods -> map valueName methods
      _ -> []

-- | Every term introduced by one declaration, with its complete callable
-- type.  Keeping this derivation beside the declaration grammar gives
-- browsers, interactive type inspection, and backend lowerings one authority
-- for constructor results and the implicit owner constraint on class methods.
--
-- Record selectors and ordinary bindings already enter the neutral inventory
-- as 'ValueDeclaration's. Type declarations and instances introduce no term
-- names of their own.
declarationTermSignatures
  :: Declaration typeVariable kindVariable annotation
  -> [ValueSignature typeVariable annotation]
declarationTermSignatures declaration = case declaration of
  ValueDeclaration signature -> [signature]
  DataTypeDeclaration _ owner parameters constructors ->
    map constructorSignature constructors
   where
    result = applyTypeArguments (TypeConstructor owner)
      [ TypeVariable $ parameterVariable parameter
      | parameter <- parameters
      ]
    constructorSignature constructor = ValueSignature
      (constructorAnnotation constructor)
      (constructorName constructor)
      (functionType (constructorFields constructor) result)
  ClassDeclaration _ owner parameters _ methods ->
    map addOwnerConstraint methods
   where
    ownerConstraint = Constraint owner
      [ TypeVariable $ parameterVariable parameter
      | parameter <- parameters
      ]
    addOwnerConstraint signature = signature
      { valueType = addConstraint ownerConstraint $ valueType signature }
    addConstraint constraint typeExpression = case typeExpression of
      ForallType variables constraints body ->
        ForallType variables (constraint : constraints) body
      _ -> ForallType [] [constraint] typeExpression
  _ -> []

-- | Every explicit type-variable occurrence in structural source order.
-- Declaration parameters or instance binders precede the syntax they scope;
-- superclass and instance prerequisites precede methods and the instance
-- head respectively. Duplicates are retained so callers can impose their own
-- identity or occurrence policy.
declarationTypeVariables
  :: Declaration typeVariable kindVariable annotation
  -> [typeVariable]
declarationTypeVariables declaration = case declaration of
  TypeSynonymDeclaration _ _ parameters body ->
    parameterVariables parameters ++ toList body
  DataTypeDeclaration _ _ parameters constructors ->
    parameterVariables parameters
      ++ concatMap (concatMap toList . constructorFields) constructors
  AbstractTypeDeclaration{} -> []
  ValueDeclaration signature -> toList $ valueType signature
  ClassDeclaration _ _ parameters superclasses methods ->
    parameterVariables parameters
      ++ concatMap constraintVariables superclasses
      ++ concatMap (toList . valueType) methods
  InstanceDeclaration _ variables prerequisites headConstraint ->
    variables ++ concatMap constraintVariables prerequisites
      ++ constraintVariables headConstraint
 where
  parameterVariables = map parameterVariable
  constraintVariables = concatMap toList . constraintArguments

-- | Validate one declaration without consulting any surrounding inventory.
--
-- The check covers lexical roles, duplicate binders and members, type syntax,
-- and variables escaping the declaration scope. It deliberately does not
-- perform kind inference or resolve nominal references.
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

-- | Rename every type variable without changing kinds, annotations, names,
-- or declaration shape. Binder and occurrence identities are transformed by
-- the same function; callers remain responsible for choosing a scope-safe
-- renaming.
mapDeclarationTypeVariables
  :: (typeVariable -> typeVariable')
  -> Declaration typeVariable kindVariable annotation
  -> Declaration typeVariable' kindVariable annotation
mapDeclarationTypeVariables convert declaration = case declaration of
  TypeSynonymDeclaration annotation name parameters body ->
    TypeSynonymDeclaration annotation name
      (map convertParameter parameters) (convertType body)
  DataTypeDeclaration annotation name parameters constructors ->
    DataTypeDeclaration annotation name
      (map convertParameter parameters) (map convertConstructor constructors)
  AbstractTypeDeclaration annotation name kind ->
    AbstractTypeDeclaration annotation name kind
  ValueDeclaration signature -> ValueDeclaration $ convertSignature signature
  ClassDeclaration annotation name parameters superclasses methods ->
    ClassDeclaration annotation name
      (map convertParameter parameters)
      (map convertConstraint superclasses)
      (map convertSignature methods)
  InstanceDeclaration annotation variables prerequisites headConstraint ->
    InstanceDeclaration annotation (map convert variables)
      (map convertConstraint prerequisites) (convertConstraint headConstraint)
 where
  convertType = fmap convert
  convertConstraint = fmap convertType
  convertParameter parameter = TypeParameter
    (convert $ parameterVariable parameter) (parameterKind parameter)
  convertConstructor constructor = DataConstructor
    (constructorAnnotation constructor) (constructorName constructor)
    (map convertType $ constructorFields constructor)
  convertSignature signature = ValueSignature
    (valueAnnotation signature) (valueName signature)
    (convertType $ valueType signature)

-- | Rename every explicit kind variable without changing source types,
-- annotations, or declaration shape.
mapDeclarationKindVariables
  :: (kindVariable -> kindVariable')
  -> Declaration typeVariable kindVariable annotation
  -> Declaration typeVariable kindVariable' annotation
mapDeclarationKindVariables convert declaration = case declaration of
  TypeSynonymDeclaration annotation name parameters body ->
    TypeSynonymDeclaration annotation name
      (map convertParameter parameters) body
  DataTypeDeclaration annotation name parameters constructors ->
    DataTypeDeclaration annotation name
      (map convertParameter parameters) constructors
  AbstractTypeDeclaration annotation name kind ->
    AbstractTypeDeclaration annotation name $ fmap convert kind
  ValueDeclaration signature -> ValueDeclaration signature
  ClassDeclaration annotation name parameters superclasses methods ->
    ClassDeclaration annotation name (map convertParameter parameters)
      superclasses methods
  InstanceDeclaration annotation variables prerequisites headConstraint ->
    InstanceDeclaration annotation variables prerequisites headConstraint
 where
  convertParameter parameter = TypeParameter
    (parameterVariable parameter)
    (fmap (fmap convert) $ parameterKind parameter)

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
validateDistinct makeError values = case firstDuplicate values of
  Nothing -> Right ()
  Just duplicate -> Left $ makeError duplicate
