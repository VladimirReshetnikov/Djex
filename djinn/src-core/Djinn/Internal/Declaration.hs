-- | Djinn declaration compatibility values and their shared-IR adapter.
module Djinn.Internal.Declaration
  ( Constructor
  , Declaration (..)
  , SynthesisDeclaration
  , DjinnDeclarationNameRole (..)
  , SynthesisDeclarationError (..)
  , canonicalUnitDeclaration
  , canonicalSynthesisListParameter
  , isDjinnDeclarationName
  , djinnDeclarationSpelling
  , djinnDeclarationSymbol
  , toSynthesisKind
  , fromSynthesisKind
  , toSynthesisDeclaration
  , fromSynthesisDeclaration
  ) where

import qualified Language.Haskell.Synthesis.Declaration as SharedDeclaration
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Kind as SharedKind
import qualified Language.Haskell.Synthesis.Type as SharedType

import Djinn.Internal.HIdentifier
  ( isConId
  , isQualifiedVarId
  , isVarId
  , isVarOperator
  )
import Djinn.Internal.HTypes
  ( HKind
  , HSymbol
  , HType (..)
  , fromSynthesisKind
  , toSynthesisKind
  )
import Djinn.Internal.Type
  ( SynthesisTypeError
  , checkedDjinnTypeVariable
  , fromSynthesisType
  , toSynthesisType
  )

-- | A data constructor in Djinn's compatibility AST: its name paired with
-- its field types in order.
type Constructor = (HSymbol, [HType])

-- | A top-level declaration accepted by 'Djinn.Core.declare': a type
-- synonym, data type, or abstract type (each with its parameter names or
-- kind), a class with its parameters and method signatures, or a free
-- function assumption.
data Declaration
  = TypeSynonym HSymbol [HSymbol] HType
  | DataType HSymbol [HSymbol] [Constructor]
  | AbstractType HSymbol HKind
  | ClassDecl HSymbol [HSymbol] [(HSymbol, HType)]
  | Function HSymbol HType
  deriving (Eq, Show)

-- | The shared-IR declaration Djinn exchanges with the synthesis library:
-- type variables are Djinn symbols, kind variables are 'Int's, and no
-- annotation is carried.
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

-- | Why a declaration could not cross between Djinn's compatibility AST and
-- the shared IR in either direction: an unparsable or role-inappropriate
-- name, a type conversion failure, a shared-side validation failure, or a
-- shared feature (explicit parameter kinds, superclasses, instances, or a
-- non-canonical @()@ or list owner) that Djinn does not represent.
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
  | NonCanonicalListDeclaration
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

-- | Convert a Djinn declaration into the shared IR, checking every name
-- against its 'DjinnDeclarationNameRole' and validating the result with
-- 'SharedDeclaration.validateDeclaration'.  The canonical unit declaration
-- maps to the shared boxed 0-tuple. Canonical list ownership has its own exact
-- family check; other declarations owning either structural family fail.
toSynthesisDeclaration
  :: Declaration
  -> Either SynthesisDeclarationError SynthesisDeclaration
toSynthesisDeclaration declaration = do
  converted <- case djinnUnitStatus declaration of
    CanonicalUnit -> canonicalSynthesisUnit
    NonCanonicalUnit -> Left NonCanonicalUnitDeclaration
    NoUnitOwnership -> case djinnListStatus declaration of
      CanonicalList parameter -> canonicalSynthesisList <$> convertedVariable parameter
      NonCanonicalList -> Left NonCanonicalListDeclaration
      NoListOwnership -> convertDeclaration declaration
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

-- | Inverse of 'toSynthesisDeclaration': validate a shared declaration and
-- project it back into Djinn's compatibility AST.  Instances, class
-- superclasses, and explicitly kinded type parameters have no Djinn form and
-- are reported as errors; the shared unit type maps to
-- 'canonicalUnitDeclaration'. The exact intrinsic list family also projects,
-- permitting its already-proper element kind without changing the source.
fromSynthesisDeclaration
  :: SynthesisDeclaration
  -> Either SynthesisDeclarationError Declaration
fromSynthesisDeclaration declaration = do
  either (Left . InvalidSharedDeclaration) Right
    $ SharedDeclaration.validateDeclaration declaration
  case synthesisUnitStatus declaration of
    CanonicalUnit -> Right canonicalUnitDeclaration
    NonCanonicalUnit -> Left NonCanonicalUnitDeclaration
    NoUnitOwnership -> case synthesisListStatus declaration of
      CanonicalList parameter -> canonicalListDeclaration <$> convertedVariable parameter
      NonCanonicalList -> Left NonCanonicalListDeclaration
      NoListOwnership -> convertDeclaration declaration
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

  sharedSymbol role name = maybe
    (Left $ UnsupportedDjinnDeclarationName role name) Right
    $ djinnDeclarationSymbol role name

-- | Project a validated shared name into the spelling Djinn's compatibility
-- AST stores for the given declaration role, when one exists. Every checked
-- name projection composes this single authority with its own error wording.
djinnDeclarationSymbol
  :: DjinnDeclarationNameRole
  -> SharedName.Name
  -> Maybe HSymbol
djinnDeclarationSymbol role name = case djinnDeclarationSpelling name of
  Just spelling | isDjinnDeclarationName role spelling -> Just spelling
  _ -> Nothing

-- | The one canonical unit declaration that trusted built-in paths install;
-- every other declaration owning @()@ is rejected before conversion.
canonicalUnitDeclaration :: Declaration
canonicalUnitDeclaration = DataType "()" [] [("()", [])]

-- | The spelling Djinn's compatibility AST stores for a shared name,
-- independent of any declaration role, or 'Nothing' when it has none.
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
  | declaration == canonicalUnitDeclaration = CanonicalUnit
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

-- List syntax has one exact constructor family. This exception belongs to
-- the complete declaration, never to the general identifier-role predicate:
-- a nominal datatype cannot steal either constructor and (:) is not a type.
data ListStatus = NoListOwnership | CanonicalList HSymbol | NonCanonicalList

canonicalListDeclaration :: HSymbol -> Declaration
canonicalListDeclaration parameter = DataType "[]" [parameter]
  [("[]", []), (":", [HTVar parameter, HTApp (HTCon "[]") (HTVar parameter)])]

canonicalSynthesisList :: HSymbol -> SynthesisDeclaration
canonicalSynthesisList parameter = SharedDeclaration.DataTypeDeclaration ()
  SharedName.listName [SharedDeclaration.TypeParameter parameter Nothing]
  [ SharedDeclaration.DataConstructor () SharedName.listName []
  , SharedDeclaration.DataConstructor () SharedName.consName
      [ SharedType.TypeVariable parameter
      , SharedType.TypeApplication (SharedType.TypeConstructor SharedName.listName)
          (SharedType.TypeVariable parameter)
      ]
  ]

-- | Recognize the retained source declaration without changing its binder,
-- field identities, or explicit proper-kind annotation. Formula projection
-- uses this same guard before treating (:) as a constructor symbol.
canonicalSynthesisListParameter
  :: Eq variable
  => SharedDeclaration.Declaration variable kindVariable annotation
  -> Maybe variable
canonicalSynthesisListParameter declaration = case declaration of
  SharedDeclaration.DataTypeDeclaration _ owner
      [SharedDeclaration.TypeParameter parameter explicitKind]
      [ SharedDeclaration.DataConstructor _ zero []
      , SharedDeclaration.DataConstructor _ step
          [ SharedType.TypeVariable element
          , SharedType.TypeApplication (SharedType.TypeConstructor recursiveOwner)
              (SharedType.TypeVariable recursiveElement)
          ]
      ]
    | owner == SharedName.listName
    , zero == SharedName.listName
    , step == SharedName.consName
    , properParameterKind explicitKind
    , element == parameter
    , recursiveOwner == owner
    , recursiveElement == parameter -> Just parameter
  _ -> Nothing
 where
  properParameterKind Nothing = True
  properParameterKind (Just SharedKind.ProperTypeKind) = True
  properParameterKind _ = False

djinnListStatus :: Declaration -> ListStatus
djinnListStatus declaration = case declaration of
  DataType "[]" [parameter] _
    | declaration == canonicalListDeclaration parameter -> CanonicalList parameter
  _ | ownsList -> NonCanonicalList
    | otherwise -> NoListOwnership
 where
  listOwner name = name == "[]" || name == ":"
  ownsList = case declaration of
    TypeSynonym name _ _ -> listOwner name
    DataType name _ constructors -> listOwner name || any (listOwner . fst) constructors
    AbstractType name _ -> listOwner name
    ClassDecl name _ methods -> listOwner name || any (listOwner . fst) methods
    Function name _ -> listOwner name

synthesisListStatus :: SynthesisDeclaration -> ListStatus
synthesisListStatus declaration = case canonicalSynthesisListParameter declaration of
  Just parameter -> CanonicalList parameter
  Nothing | ownsList -> NonCanonicalList
          | otherwise -> NoListOwnership
 where
  listOwner name = name == SharedName.listName || name == SharedName.consName
  ownsList = case declaration of
    SharedDeclaration.TypeSynonymDeclaration _ name _ _ -> listOwner name
    SharedDeclaration.DataTypeDeclaration _ name _ constructors ->
      listOwner name || any (listOwner . SharedDeclaration.constructorName) constructors
    SharedDeclaration.AbstractTypeDeclaration _ name _ -> listOwner name
    SharedDeclaration.ValueDeclaration signature -> listOwner $ SharedDeclaration.valueName signature
    SharedDeclaration.ClassDeclaration _ name _ _ methods ->
      listOwner name || any (listOwner . SharedDeclaration.valueName) methods
    SharedDeclaration.InstanceDeclaration{} -> False
