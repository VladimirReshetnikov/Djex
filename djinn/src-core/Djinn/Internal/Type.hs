-- | Checked boundary between Djinn's historical constructor vocabulary and
-- the shared source-type vocabulary it now stores natively.
module Djinn.Internal.Type
  ( SynthesisTypeError (..)
  , checkedDjinnTypeVariable
  , djinnTypeConstructorSymbol
  , normalizeSynthesisType
  , renderSynthesisType
  , toSynthesisType
  , fromSynthesisType
  ) where

import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Type as SharedType
import qualified Language.Haskell.Synthesis.TypeRender as SharedTypeRender

import Djinn.Internal.HIdentifier (isQualifiedConId, isVarId)
import Djinn.Internal.HTypes
  ( HType (..)
  , HSymbol
  , fromHTypeSynthesisStructure
  , hTypeSynthesisStructure
  )

data SynthesisTypeError
  = InvalidHTypeName HSymbol SharedName.NameError
  | InvalidDjinnTypeVariable HSymbol
  | UnsupportedDjinnTypeConstructorName SharedName.Name
  | DeclarationBodyIsNotSourceType HType
  | InvalidSynthesisType (SharedType.TypeError HSymbol)
  | SynthesisForallUnsupported
  | SynthesisUnboxedTupleUnsupported Int
  | PartialTupleConstructorUnsupported SharedName.Boxity Int
  deriving (Eq, Show)

-- | Djinn's type variables are source-level unqualified @varid@s. The shared
-- IR leaves variable identity abstract, so this String-specialized boundary
-- reasserts that backend invariant.
checkedDjinnTypeVariable
  :: HSymbol
  -> Either SynthesisTypeError HSymbol
checkedDjinnTypeVariable variable
  | isVarId variable = Right variable
  | otherwise = Left $ InvalidDjinnTypeVariable variable

toSynthesisType
  :: HType
  -> Either SynthesisTypeError (SharedType.Type HSymbol)
toSynthesisType source = case hTypeSynthesisStructure source of
  Just native -> normalizeSynthesisType native
  Nothing -> convert source >>= normalizeSynthesisType
 where
  convert typeExpression = case typeExpression of
    HTVar variable -> SharedType.TypeVariable
      <$> checkedDjinnTypeVariable variable
    HTCon sourceName -> SharedType.TypeConstructor
      <$> checkedName sourceName
    HTApp function argument -> SharedType.TypeApplication
      <$> convert function <*> convert argument
    HTArrow parameter result -> SharedType.FunctionType
      <$> convert parameter <*> convert result
    HTTuple elements -> SharedType.TupleType SharedName.Boxed
      <$> mapM convert elements
    declaration@HTUnion{} ->
      Left $ DeclarationBodyIsNotSourceType declaration
    declaration@HTAbstract{} ->
      Left $ DeclarationBodyIsNotSourceType declaration

  checkedName sourceName = case SharedName.parseName sourceName of
    Left nameError -> Left $ InvalidHTypeName sourceName nameError
    Right name
      | supportedTypeConstructor sourceName name -> Right name
      | otherwise -> Left $ UnsupportedDjinnTypeConstructorName name

  -- Djinn has dedicated syntax for these built-ins.  Every ordinary type
  -- constructor must otherwise be a (possibly qualified) ConId; constructor
  -- operators are valid shared names but Djinn's type parser cannot read
  -- their prefix form back.
  supportedTypeConstructor sourceName name =
    case SharedName.nameSpecial name of
      Just SharedName.ListConstructor -> sourceName == "[]"
      Just SharedName.FunctionConstructor ->
        sourceName == "->" || sourceName == "(->)"
      Just (SharedName.TupleConstructor SharedName.Boxed 0) ->
        sourceName == "()"
      Just _ -> False
      Nothing -> isQualifiedConId sourceName

-- | Validate and canonicalize a shared type. This is the checked entrance
-- used by both Djex's native path and ordinary compatibility 'HType' values;
-- only declaration forms and malformed raw spellings require the recursive
-- fallback in 'toSynthesisType'.
normalizeSynthesisType
  :: SharedType.Type HSymbol
  -> Either SynthesisTypeError (SharedType.Type HSymbol)
normalizeSynthesisType source = do
  canonical <- either (Left . InvalidSynthesisType) Right
    $ SharedType.normalizeType source
  validateSupported canonical
  return canonical
 where
  validateSupported typeExpression = case typeExpression of
    SharedType.TypeVariable variable ->
      () <$ checkedDjinnTypeVariable variable
    SharedType.TypeConstructor name -> () <$ djinnTypeConstructorSymbol name
    SharedType.TypeApplication function argument ->
      validateSupported function >> validateSupported argument
    SharedType.FunctionType parameter result ->
      validateSupported parameter >> validateSupported result
    SharedType.TupleType SharedName.Boxed elements ->
      mapM_ validateSupported elements
    SharedType.TupleType SharedName.Unboxed elements ->
      Left $ SynthesisUnboxedTupleUnsupported $ length elements
    SharedType.ForallType{} -> Left SynthesisForallUnsupported

-- | Render a checked shared type with Djinn's historical source-like syntax.
-- Keeping this projection native avoids constructing an 'HType' merely to
-- label a kind or synonym diagnostic.
renderSynthesisType :: SharedType.Type HSymbol -> String
renderSynthesisType = SharedTypeRender.renderType id

fromSynthesisType
  :: SharedType.Type HSymbol
  -> Either SynthesisTypeError HType
fromSynthesisType source = do
  normalized <- normalizeSynthesisType source
  case fromHTypeSynthesisStructure normalized of
    Just native -> Right native
    -- 'normalizeSynthesisType' has already rejected both unsupported
    -- alternatives, so retain the old checked projection as a defensive
    -- total fallback rather than asserting that invariant with 'error'.
    Nothing -> convert normalized
 where
  convert typeExpression = case typeExpression of
    SharedType.TypeVariable variable -> Right $ HTVar variable
    SharedType.TypeConstructor name -> HTCon <$>
      djinnTypeConstructorSymbol name
    SharedType.TypeApplication function argument -> HTApp
      <$> convert function <*> convert argument
    SharedType.FunctionType parameter result -> HTArrow
      <$> convert parameter <*> convert result
    SharedType.TupleType SharedName.Boxed [] -> Right $ HTCon "()"
    SharedType.TupleType SharedName.Boxed elements ->
      HTTuple <$> mapM convert elements
    -- 'normalizeSynthesisType' rejects both alternatives before conversion.
    SharedType.TupleType SharedName.Unboxed elements ->
      Left $ SynthesisUnboxedTupleUnsupported $ length elements
    SharedType.ForallType{} -> Left SynthesisForallUnsupported

-- | Project one validated shared type-constructor name into Djinn's spelling.
-- Unit is included because declaration owners use the nominal name directly;
-- canonical ordinary types represent it structurally as a zero boxed tuple.
djinnTypeConstructorSymbol
  :: SharedName.Name
  -> Either SynthesisTypeError HSymbol
djinnTypeConstructorSymbol name = case SharedName.nameSpecial name of
  Just SharedName.ListConstructor -> Right "[]"
  Just SharedName.FunctionConstructor -> Right "->"
  Just (SharedName.TupleConstructor SharedName.Boxed 0) -> Right "()"
  Just (SharedName.TupleConstructor boxity arity) ->
    Left $ PartialTupleConstructorUnsupported boxity arity
  Just SharedName.ConsConstructor ->
    -- 'SharedType.validateType' rejects this before this projection.
    Left $ InvalidSynthesisType $ SharedType.InvalidTypeConstructor name
  Nothing ->
    let spelling = SharedName.renderCanonical name
    in if isQualifiedConId spelling then
         Right spelling
       else
         Left $ UnsupportedDjinnTypeConstructorName name
