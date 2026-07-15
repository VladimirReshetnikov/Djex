-- | Djinn declaration compatibility values and their shared-IR adapter.
module Djinn.Internal.Declaration
  ( Constructor
  , Declaration (..)
  , SynthesisDeclaration
  , DjinnDeclarationNameRole (..)
  , SynthesisDeclarationError (..)
  , isDjinnDeclarationName
  , toSynthesisKind
  , fromSynthesisKind
  , toSynthesisDeclaration
  , fromSynthesisDeclaration
  ) where

import qualified Language.Haskell.Synthesis.Declaration as SharedDeclaration
import qualified Language.Haskell.Synthesis.Name as SharedName

import Djinn.Internal.HIdentifier
  ( isConId
  , isQualifiedVarId
  , isVarId
  , isVarOperator
  )
import Djinn.Internal.HTypes
  ( HKind
  , HSymbol
  , HType
  , fromSynthesisKind
  , toSynthesisKind
  )
import Djinn.Internal.Type
  ( SynthesisTypeError
  , checkedDjinnTypeVariable
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

-- | The declaration position a name owns in Djinn's source grammar.
-- Keeping the role explicit prevents a broadly valid shared
-- t'SharedName.Name' from
-- silently crossing into a narrower local namespace.
data DjinnDeclarationNameRole
  = TypeOwner
  | DataConstructorOwner
  | ClassOwner
  | MethodOwner
  | FunctionOwner
  deriving (Eq, Show)

data SynthesisDeclarationError
  = InvalidDjinnDeclarationName HSymbol SharedName.NameError
  | UnsupportedDjinnDeclarationName
      DjinnDeclarationNameRole SharedName.Name
  | DeclarationTypeConversionError SynthesisTypeError
  | InvalidSharedDeclaration
      (SharedDeclaration.DeclarationError HSymbol)
  | ExplicitParameterKindUnsupported HSymbol
  | ClassSuperclassesUnsupported
  | InstanceDeclarationUnsupported
  | NonCanonicalUnitDeclaration
  deriving (Eq, Show)

-- | The exact lexical policy used by 'Djinn.Core.declare'.  Type, class,
-- and constructor owners are local ConIds; methods are local value names;
-- only free function assumptions retain Djinn's historical qualified-VarId
-- allowance.
isDjinnDeclarationName :: DjinnDeclarationNameRole -> HSymbol -> Bool
isDjinnDeclarationName role name = case role of
  TypeOwner -> isConId name
  DataConstructorOwner -> isConId name
  ClassOwner -> isConId name
  MethodOwner -> isVarId name || isVarOperator name
  FunctionOwner -> isQualifiedVarId name || isVarOperator name

toSynthesisDeclaration
  :: Declaration
  -> Either SynthesisDeclarationError SynthesisDeclaration
toSynthesisDeclaration declaration = do
  converted <- case djinnUnitStatus declaration of
    CanonicalUnit -> canonicalSynthesisUnit
    NonCanonicalUnit -> Left NonCanonicalUnitDeclaration
    NoUnitOwnership -> convertDeclaration declaration
  either (Left . InvalidSharedDeclaration) Right
    $ SharedDeclaration.validateDeclaration converted
  return converted
 where
  convertDeclaration source = case source of
    TypeSynonym sourceName parameters body ->
      SharedDeclaration.TypeSynonymDeclaration ()
        <$> checkedName TypeOwner sourceName
        <*> mapM unkindedParameter parameters
        <*> convertedType body
    DataType sourceName parameters constructors ->
      SharedDeclaration.DataTypeDeclaration ()
        <$> checkedName TypeOwner sourceName
        <*> mapM unkindedParameter parameters
        <*> mapM convertedConstructor constructors
    AbstractType sourceName kind ->
      SharedDeclaration.AbstractTypeDeclaration ()
        <$> checkedName TypeOwner sourceName
        <*> pure (toSynthesisKind kind)
    ClassDecl sourceName parameters methods ->
      SharedDeclaration.ClassDeclaration ()
        <$> checkedName ClassOwner sourceName
        <*> mapM unkindedParameter parameters
        <*> pure []
        <*> mapM convertedMethod methods
    Function sourceName functionType ->
      SharedDeclaration.ValueDeclaration
        <$> (SharedDeclaration.ValueSignature ()
          <$> checkedName FunctionOwner sourceName
          <*> convertedType functionType)

  unkindedParameter variable = SharedDeclaration.TypeParameter
    <$> convertedVariable variable <*> pure Nothing

  convertedConstructor (sourceName, fields) =
    SharedDeclaration.DataConstructor ()
      <$> checkedName DataConstructorOwner sourceName
      <*> mapM convertedType fields

  convertedMethod (sourceName, methodType) =
    SharedDeclaration.ValueSignature ()
      <$> checkedName MethodOwner sourceName <*> convertedType methodType

  convertedType = either (Left . DeclarationTypeConversionError) Right
    . toSynthesisType

  convertedVariable = either
    (Left . DeclarationTypeConversionError) Right .
    checkedDjinnTypeVariable

  checkedName role sourceName = case SharedName.parseName sourceName of
    Left nameError -> Left $ InvalidDjinnDeclarationName sourceName nameError
    Right name
      | isDjinnDeclarationName role sourceName -> Right name
      | otherwise -> Left $ UnsupportedDjinnDeclarationName role name

fromSynthesisDeclaration
  :: SynthesisDeclaration
  -> Either SynthesisDeclarationError Declaration
fromSynthesisDeclaration declaration = do
  either (Left . InvalidSharedDeclaration) Right
    $ SharedDeclaration.validateDeclaration declaration
  case synthesisUnitStatus declaration of
    CanonicalUnit -> Right $ DataType "()" [] [("()", [])]
    NonCanonicalUnit -> Left NonCanonicalUnitDeclaration
    NoUnitOwnership -> convertDeclaration declaration
 where
  convertDeclaration source = case source of
    SharedDeclaration.TypeSynonymDeclaration _ name parameters body ->
      TypeSynonym <$> sharedSymbol TypeOwner name
        <*> mapM plainParameter parameters <*> convertedType body
    SharedDeclaration.DataTypeDeclaration _ name parameters constructors ->
      DataType <$> sharedSymbol TypeOwner name
        <*> mapM plainParameter parameters
        <*> mapM convertedConstructor constructors
    SharedDeclaration.AbstractTypeDeclaration _ name kind ->
      AbstractType <$> sharedSymbol TypeOwner name
        <*> pure (fromSynthesisKind kind)
    SharedDeclaration.ValueDeclaration signature ->
      Function
        <$> sharedSymbol FunctionOwner
          (SharedDeclaration.valueName signature)
        <*> convertedType (SharedDeclaration.valueType signature)
    SharedDeclaration.ClassDeclaration _ name parameters superclasses methods
      | not (null superclasses) -> Left ClassSuperclassesUnsupported
      | otherwise -> ClassDecl <$> sharedSymbol ClassOwner name
          <*> mapM plainParameter parameters
          <*> mapM convertedMethod methods
    SharedDeclaration.InstanceDeclaration{} ->
      Left InstanceDeclarationUnsupported

  plainParameter parameter = case SharedDeclaration.parameterKind parameter of
    Nothing -> convertedVariable $ SharedDeclaration.parameterVariable parameter
    Just _ -> Left $ ExplicitParameterKindUnsupported
      $ SharedDeclaration.parameterVariable parameter

  convertedConstructor constructor = (,)
    <$> sharedSymbol DataConstructorOwner
      (SharedDeclaration.constructorName constructor)
    <*> mapM convertedType (SharedDeclaration.constructorFields constructor)

  convertedMethod method = (,)
    <$> sharedSymbol MethodOwner (SharedDeclaration.valueName method)
    <*> convertedType (SharedDeclaration.valueType method)

  convertedType = either (Left . DeclarationTypeConversionError) Right
    . fromSynthesisType

  convertedVariable = either
    (Left . DeclarationTypeConversionError) Right .
    checkedDjinnTypeVariable

  sharedSymbol role name =
    case djinnDeclarationSpelling name of
      Just spelling
        | isDjinnDeclarationName role spelling -> Right spelling
      _ -> Left $ UnsupportedDjinnDeclarationName role name

-- Symbolic value names are stored without prefix parentheses in Djinn's
-- compatibility AST.  Identifiers use their canonical (possibly qualified)
-- spelling; built-ins have no declaration-owner spelling here.
djinnDeclarationSpelling :: SharedName.Name -> Maybe HSymbol
djinnDeclarationSpelling name = case SharedName.nameIdentifier name of
  Just _ -> Just $ SharedName.renderCanonical name
  Nothing -> case (SharedName.nameModule name, SharedName.nameOperator name) of
    (Nothing, Just operator) -> Just operator
    _ -> Nothing

data UnitStatus = NoUnitOwnership | CanonicalUnit | NonCanonicalUnit

djinnUnitStatus :: Declaration -> UnitStatus
djinnUnitStatus declaration
  | declaration == DataType "()" [] [("()", [])] = CanonicalUnit
  | ownsUnit = NonCanonicalUnit
  | otherwise = NoUnitOwnership
  where
    ownsUnit = case declaration of
      TypeSynonym name _ _ -> name == "()"
      DataType name _ constructors ->
        name == "()" || any ((== "()") . fst) constructors
      AbstractType name _ -> name == "()"
      ClassDecl name _ methods ->
        name == "()" || any ((== "()") . fst) methods
      Function name _ -> name == "()"

synthesisUnitStatus :: SynthesisDeclaration -> UnitStatus
synthesisUnitStatus declaration
  | isCanonicalSynthesisUnit declaration = CanonicalUnit
  | ownsUnit = NonCanonicalUnit
  | otherwise = NoUnitOwnership
  where
    ownsUnit = case declaration of
      SharedDeclaration.TypeSynonymDeclaration _ name _ _ -> isUnitName name
      SharedDeclaration.DataTypeDeclaration _ name _ constructors ->
        isUnitName name || any
          (isUnitName . SharedDeclaration.constructorName) constructors
      SharedDeclaration.AbstractTypeDeclaration _ name _ -> isUnitName name
      SharedDeclaration.ValueDeclaration signature ->
        isUnitName $ SharedDeclaration.valueName signature
      SharedDeclaration.ClassDeclaration _ name _ _ methods ->
        isUnitName name || any
          (isUnitName . SharedDeclaration.valueName) methods
      SharedDeclaration.InstanceDeclaration{} -> False

isCanonicalSynthesisUnit :: SynthesisDeclaration -> Bool
isCanonicalSynthesisUnit declaration = case declaration of
  SharedDeclaration.DataTypeDeclaration _ name [] [constructor] ->
    isUnitName name &&
      isUnitName (SharedDeclaration.constructorName constructor) &&
      null (SharedDeclaration.constructorFields constructor)
  _ -> False

isUnitName :: SharedName.Name -> Bool
isUnitName name = SharedName.nameSpecial name ==
  Just (SharedName.TupleConstructor SharedName.Boxed 0)

canonicalSynthesisUnit
  :: Either SynthesisDeclarationError SynthesisDeclaration
canonicalSynthesisUnit = do
  unitName <- case SharedName.tupleName SharedName.Boxed 0 of
    Left nameError -> Left $ InvalidDjinnDeclarationName "()" nameError
    Right name -> Right name
  return $ SharedDeclaration.DataTypeDeclaration () unitName []
    [SharedDeclaration.DataConstructor () unitName []]
