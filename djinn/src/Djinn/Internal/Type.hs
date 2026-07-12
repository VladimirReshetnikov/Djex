-- | Lossless boundary between Djinn's legacy type/search representation and
-- the shared source-type vocabulary.
module Djinn.Internal.Type
  ( SynthesisTypeError (..)
  , toSynthesisType
  , fromSynthesisType
  ) where

import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Type as SharedType

import Djinn.Internal.HTypes (HType (..), HSymbol)

data SynthesisTypeError
  = InvalidHTypeName HSymbol SharedName.NameError
  | DeclarationBodyIsNotSourceType HType
  | InvalidSynthesisType (SharedType.TypeError HSymbol)
  | SynthesisForallUnsupported
  | SynthesisUnboxedTupleUnsupported Int
  | PartialTupleConstructorUnsupported SharedName.Boxity Int
  deriving (Eq, Show)

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
    HTVar variable -> Right $ SharedType.TypeVariable variable
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
    Right name -> Right name

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
    SharedType.TypeVariable variable -> Right $ HTVar variable
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
    Nothing -> case SharedName.nameSpelling name of
      Nothing -> Left $ InvalidSynthesisType
        $ SharedType.InvalidTypeConstructor name
      Just spelling -> Right $ maybe spelling
        (\namespace -> SharedName.renderModuleName namespace ++ "." ++ spelling)
        $ SharedName.nameModule name
