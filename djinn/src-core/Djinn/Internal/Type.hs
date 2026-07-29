-- | Checked boundary between Djinn's historical constructor vocabulary and
-- the shared source-type vocabulary it now stores natively.
module Djinn.Internal.Type
  ( SynthesisTypeError (..)
  , checkedDjinnTypeVariable
  , djinnTypeConstructorSymbol
  , freshPrimedVariable
  , sealSynthesisSignature
  , validateSynthesisConstraintHeader
  , normalizeSynthesisType
  , renderSynthesisType
  , toSynthesisType
  , fromSynthesisType
  ) where

import qualified Data.Set as Set

import qualified Language.Haskell.Synthesis.Collection as SharedCollection
import Language.Haskell.Synthesis.Constraint
  ( Constraint (..)
  , validateConstraintClassName
  )
import qualified Language.Haskell.Synthesis.Fresh as Fresh
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Type as SharedType
import qualified Language.Haskell.Synthesis.TypeRender as SharedTypeRender

import Djinn.Internal.HIdentifier (isConId, isQualifiedConId, isVarId)
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
  | UnsupportedDjinnConstraintClassName SharedName.Name
  | DeclarationBodyIsNotSourceType HType
  | InvalidSynthesisType (SharedType.TypeError HSymbol)
  -- Kept so downstream matches on the formerly public error vocabulary keep
  -- compiling; forall-capable boundaries no longer emit this alternative.
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
    HTForall binders constraints body -> SharedType.ForallType binders
      <$> traverse (traverse convert) constraints
      <*> convert body
    HTTuple elements -> do
      let arity = SharedCollection.observedListLength
            SharedName.maximumTupleArity elements
      if arity == 1 || arity > SharedName.maximumTupleArity
        then Left $ InvalidSynthesisType
          $ SharedType.InvalidTupleTypeArity SharedName.Boxed arity
        else SharedType.TupleType SharedName.Boxed <$> mapM convert elements
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
    SharedType.ForallType binders constraints body -> do
      mapM_ checkedDjinnTypeVariable binders
      mapM_ validateConstraint constraints
      validateSupported body

  validateConstraint constraint = do
    validateSynthesisConstraintHeader constraint
    mapM_ validateSupported $ constraintArguments constraint

-- | Perform the session-independent, bounded validation of a Djinn signature.
-- Leading quantifiers are later implicitized by the request adapter; nested
-- quantifiers remain in the shared tree. Checked formula compilation can open
-- context-free positive occurrences, while unsupported occurrences remain
-- alpha-aware opaque atoms.
-- Constraint argument spines deliberately remain untouched here because only
-- a prepared session owns the finite class arities needed to inspect them
-- productively.
sealSynthesisSignature
  :: SharedType.Type HSymbol
  -> Either SynthesisTypeError (SharedType.Type HSymbol)
sealSynthesisSignature source = do
  validateSignature source
  pure source
 where
  -- Constraint arguments stay deliberately opaque at this session-independent
  -- boundary. A session first bounds every known class application, then the
  -- request worker performs the complete recursive normalization. This keeps
  -- nested rank-N contexts as productive as the historical prenex context.
  validateSignature typeExpression = case typeExpression of
    SharedType.TypeVariable variable ->
      () <$ checkedDjinnTypeVariable variable
    SharedType.TypeConstructor name ->
      () <$ djinnTypeConstructorSymbol name
    SharedType.TypeApplication function argument ->
      validateSignature function >> validateSignature argument
    SharedType.FunctionType parameter result ->
      validateSignature parameter >> validateSignature result
    SharedType.TupleType SharedName.Boxed elements -> do
      let arity = SharedCollection.observedListLength
            SharedName.maximumTupleArity elements
      if arity == 1 || arity > SharedName.maximumTupleArity
        then Left $ InvalidSynthesisType
          $ SharedType.InvalidTupleTypeArity SharedName.Boxed arity
        else mapM_ validateSignature elements
    SharedType.TupleType SharedName.Unboxed elements ->
      Left $ SynthesisUnboxedTupleUnsupported
        $ SharedCollection.observedListLength
            SharedName.maximumTupleArity elements
    SharedType.ForallType binders constraints body -> do
      validateBinders binders
      mapM_ validateSynthesisConstraintHeader constraints
      validateSignature body

  validateBinders binders = do
    mapM_ checkedDjinnTypeVariable binders
    case SharedCollection.firstDuplicate binders of
      Just duplicate -> Left $ InvalidSynthesisType
        $ SharedType.DuplicateForallVariable duplicate
      Nothing -> Right ()

-- | Validate only the nominal header of a Djinn constraint. Do not enter its
-- argument spine here: a programmatic caller may construct a cyclic list, and
-- only a sealed session knows the class arity with which that list can be
-- inspected productively. Argument types are normalized after that bounded
-- preflight in the request adapter.
--
-- The shared constraint vocabulary admits qualified class names, while
-- Djinn's declarations and historical parser admit local constructor
-- identifiers only. Keeping this backend policy beside signature sealing
-- makes explicit and embedded contexts cross the same checked boundary.
validateSynthesisConstraintHeader
  :: Constraint (SharedType.Type HSymbol)
  -> Either SynthesisTypeError ()
validateSynthesisConstraintHeader (Constraint className _) = do
  either
    (Left . InvalidSynthesisType . SharedType.InvalidTypeConstraint)
    Right
    $ validateConstraintClassName className
  if isConId $ SharedName.renderCanonical className then
    Right ()
  else
    Left $ UnsupportedDjinnConstraintClassName className

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
    SharedType.ForallType binders constraints body -> HTForall binders
      <$> traverse (traverse convert) constraints
      <*> convert body

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

-- | The historical primed fresh-spelling policy shared by Djinn's type,
-- synonym, and method namespaces: prime the source spelling until it avoids
-- every reserved name, publishing the chosen spelling for later allocations.
freshPrimedVariable
  :: Set.Set HSymbol
  -> HSymbol
  -> (HSymbol, Set.Set HSymbol)
freshPrimedVariable reserved variable = (fresh, reserved')
 where
  (fresh, reserved', _) = Fresh.allocateFresh
    (\candidate -> (candidate, candidate ++ "'"))
    reserved (variable ++ "'")
