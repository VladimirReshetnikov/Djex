-- | Lossless boundary between Djinn's legacy type/search representation and
-- the shared source-type vocabulary.
module Djinn.Internal.Type
  ( SynthesisTypeError (..)
  , isDjinnTypeVariable
  , checkedDjinnTypeVariable
  , toSynthesisType
  , fromSynthesisType
  ) where

import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Type as SharedType

import Djinn.Internal.HIdentifier (isQualifiedConId, isVarId)
import Djinn.Internal.HTypes (HType (..), HSymbol)

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

-- | Djinn's type variables are source-level unqualified @varid@s.  The
-- shared IR deliberately leaves its variable identity type abstract, so a
-- @String@-specialized adapter must reassert this backend invariant.
isDjinnTypeVariable :: HSymbol -> Bool
isDjinnTypeVariable = isVarId

checkedDjinnTypeVariable
  :: HSymbol
  -> Either SynthesisTypeError HSymbol
checkedDjinnTypeVariable variable
  | isDjinnTypeVariable variable = Right variable
  | otherwise = Left $ InvalidDjinnTypeVariable variable

toSynthesisType
  :: HType
  -> Either SynthesisTypeError (SharedType.Type HSymbol)
toSynthesisType source = do
  converted <- convert source
  let canonical = SharedType.canonicalizeType converted
  either (Left . InvalidSynthesisType) Right
    $ SharedType.validateType canonical
  return canonical
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

fromSynthesisType
  :: SharedType.Type HSymbol
  -> Either SynthesisTypeError HType
fromSynthesisType source = do
  let canonical = SharedType.canonicalizeType source
  either (Left . InvalidSynthesisType) Right
    $ SharedType.validateType canonical
  convert canonical
 where
  convert typeExpression = case typeExpression of
    SharedType.TypeVariable variable -> HTVar
      <$> checkedDjinnTypeVariable variable
    SharedType.TypeConstructor name -> HTCon <$> djinnName name
    SharedType.TypeApplication function argument -> HTApp
      <$> convert function <*> convert argument
    SharedType.FunctionType parameter result -> HTArrow
      <$> convert parameter <*> convert result
    SharedType.TupleType SharedName.Boxed [] -> Right $ HTCon "()"
    SharedType.TupleType SharedName.Boxed elements ->
      HTTuple <$> mapM convert elements
    SharedType.TupleType SharedName.Unboxed elements ->
      Left $ SynthesisUnboxedTupleUnsupported $ length elements
    SharedType.ForallType{} -> Left SynthesisForallUnsupported

  djinnName name = case SharedName.nameSpecial name of
    Just SharedName.ListConstructor -> Right "[]"
    Just SharedName.FunctionConstructor -> Right "->"
    Just (SharedName.TupleConstructor boxity arity) ->
      Left $ PartialTupleConstructorUnsupported boxity arity
    Just SharedName.ConsConstructor ->
      -- 'validateType' rejects this before conversion.
      Left $ InvalidSynthesisType $ SharedType.InvalidTypeConstructor name
    Nothing ->
      let spelling = SharedName.renderCanonical name
      in if isQualifiedConId spelling then
           Right spelling
         else
           Left $ UnsupportedDjinnTypeConstructorName name
