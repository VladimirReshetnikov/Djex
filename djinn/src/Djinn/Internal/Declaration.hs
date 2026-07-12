-- | Djinn declaration compatibility values and their shared-IR adapter.
module Djinn.Internal.Declaration
  ( Constructor
  , Declaration (..)
  , SynthesisDeclaration
  , SynthesisDeclarationError (..)
  , toSynthesisKind
  , fromSynthesisKind
  , toSynthesisDeclaration
  , fromSynthesisDeclaration
  ) where

import qualified Language.Haskell.Synthesis.Declaration as SharedDeclaration
import qualified Language.Haskell.Synthesis.Kind as SharedKind
import qualified Language.Haskell.Synthesis.Name as SharedName

import Djinn.Internal.HTypes (HKind (..), HSymbol, HType)
import Djinn.Internal.Type
  ( SynthesisTypeError
  , fromSynthesisType
  , toSynthesisType
  )

type Constructor = (HSymbol, [HType])

data Declaration
  = TypeSynonym HSymbol [HSymbol] HType
  | DataType HSymbol [HSymbol] [Constructor]
  | AbstractType HSymbol HKind
  | ClassDecl HSymbol [HSymbol] [(HSymbol, HType)]
  | Function HSymbol HType
  deriving (Eq, Show)

type SynthesisDeclaration =
  SharedDeclaration.Declaration HSymbol Int ()

data SynthesisDeclarationError
  = InvalidDjinnDeclarationName HSymbol SharedName.NameError
  | DeclarationTypeConversionError SynthesisTypeError
  | InvalidSharedDeclaration
      (SharedDeclaration.DeclarationError HSymbol)
  | ExplicitParameterKindUnsupported HSymbol
  | ClassSuperclassesUnsupported
  | InstanceDeclarationUnsupported
  | UnsupportedSharedDeclarationName SharedName.Name
  deriving (Eq, Show)

toSynthesisKind :: HKind -> SharedKind.Kind Int
toSynthesisKind kind = case kind of
  KStar -> SharedKind.ProperTypeKind
  KArrow parameter result -> SharedKind.FunctionKind
    (toSynthesisKind parameter) (toSynthesisKind result)
  KVar variable -> SharedKind.KindVariable variable

fromSynthesisKind :: SharedKind.Kind Int -> HKind
fromSynthesisKind kind = case kind of
  SharedKind.ProperTypeKind -> KStar
  SharedKind.FunctionKind parameter result -> KArrow
    (fromSynthesisKind parameter) (fromSynthesisKind result)
  SharedKind.KindVariable variable -> KVar variable

toSynthesisDeclaration
  :: Declaration
  -> Either SynthesisDeclarationError SynthesisDeclaration
toSynthesisDeclaration declaration = do
  converted <- case declaration of
    TypeSynonym sourceName parameters body ->
      SharedDeclaration.TypeSynonymDeclaration ()
        <$> checkedName sourceName
        <*> pure (map unkindedParameter parameters)
        <*> convertedType body
    DataType sourceName parameters constructors ->
      SharedDeclaration.DataTypeDeclaration ()
        <$> checkedName sourceName
        <*> pure (map unkindedParameter parameters)
        <*> mapM convertedConstructor constructors
    AbstractType sourceName kind ->
      SharedDeclaration.AbstractTypeDeclaration ()
        <$> checkedName sourceName
        <*> pure (toSynthesisKind kind)
    ClassDecl sourceName parameters methods ->
      SharedDeclaration.ClassDeclaration ()
        <$> checkedName sourceName
        <*> pure (map unkindedParameter parameters)
        <*> pure []
        <*> mapM convertedMethod methods
    Function sourceName functionType ->
      SharedDeclaration.ValueDeclaration
        <$> (SharedDeclaration.ValueSignature ()
          <$> checkedName sourceName <*> convertedType functionType)
  either (Left . InvalidSharedDeclaration) Right
    $ SharedDeclaration.validateDeclaration converted
  return converted
 where
  unkindedParameter variable = SharedDeclaration.TypeParameter variable Nothing

  convertedConstructor (sourceName, fields) =
    SharedDeclaration.DataConstructor ()
      <$> checkedName sourceName <*> mapM convertedType fields

  convertedMethod (sourceName, methodType) =
    SharedDeclaration.ValueSignature ()
      <$> checkedName sourceName <*> convertedType methodType

  convertedType = either (Left . DeclarationTypeConversionError) Right
    . toSynthesisType

  checkedName sourceName = case SharedName.parseName sourceName of
    Left nameError -> Left $ InvalidDjinnDeclarationName sourceName nameError
    Right name -> Right name

fromSynthesisDeclaration
  :: SynthesisDeclaration
  -> Either SynthesisDeclarationError Declaration
fromSynthesisDeclaration declaration = do
  either (Left . InvalidSharedDeclaration) Right
    $ SharedDeclaration.validateDeclaration declaration
  case declaration of
    SharedDeclaration.TypeSynonymDeclaration _ name parameters body ->
      TypeSynonym <$> sharedSymbol name
        <*> mapM plainParameter parameters <*> convertedType body
    SharedDeclaration.DataTypeDeclaration _ name parameters constructors ->
      DataType <$> sharedSymbol name
        <*> mapM plainParameter parameters
        <*> mapM convertedConstructor constructors
    SharedDeclaration.AbstractTypeDeclaration _ name kind ->
      AbstractType <$> sharedSymbol name <*> pure (fromSynthesisKind kind)
    SharedDeclaration.ValueDeclaration signature ->
      Function <$> sharedSymbol (SharedDeclaration.valueName signature)
        <*> convertedType (SharedDeclaration.valueType signature)
    SharedDeclaration.ClassDeclaration _ name parameters superclasses methods
      | not (null superclasses) -> Left ClassSuperclassesUnsupported
      | otherwise -> ClassDecl <$> sharedSymbol name
          <*> mapM plainParameter parameters
          <*> mapM convertedMethod methods
    SharedDeclaration.InstanceDeclaration{} ->
      Left InstanceDeclarationUnsupported
 where
  plainParameter parameter = case SharedDeclaration.parameterKind parameter of
    Nothing -> Right $ SharedDeclaration.parameterVariable parameter
    Just _ -> Left $ ExplicitParameterKindUnsupported
      $ SharedDeclaration.parameterVariable parameter

  convertedConstructor constructor = (,)
    <$> sharedSymbol (SharedDeclaration.constructorName constructor)
    <*> mapM convertedType (SharedDeclaration.constructorFields constructor)

  convertedMethod method = (,)
    <$> sharedSymbol (SharedDeclaration.valueName method)
    <*> convertedType (SharedDeclaration.valueType method)

  convertedType = either (Left . DeclarationTypeConversionError) Right
    . fromSynthesisType

  sharedSymbol name = case
      (SharedName.nameSpecial name, SharedName.nameSpelling name) of
    (Nothing, Just spelling) -> Right $ maybe spelling
      (\namespace -> SharedName.renderModuleName namespace ++ "." ++ spelling)
      $ SharedName.nameModule name
    _ -> Left $ UnsupportedSharedDeclarationName name
