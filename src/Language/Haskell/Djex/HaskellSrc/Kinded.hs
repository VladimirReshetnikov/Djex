-- | Checked conversion of explicit ground-kind forall annotations. This
-- adapter retains annotations alongside the converted type; the legacy
-- type-only converter continues to refuse them. Public command admission
-- must retain this checked result through engine selection and rendering.
module Language.Haskell.Djex.HaskellSrc.Kinded
  ( convertKindedSourceType
  ) where

import Control.Monad.Trans.Except (ExceptT, throwE)
import qualified Language.Haskell.Exts.Syntax as HSE
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exference.Core.Types (TypeVarIndex)
import Language.Haskell.Exference.HaskellSrcUtils (splitClassApplication)
import Language.Haskell.Exference.TypeFromHaskellSrc
  ( TypeResolver, convertTypeNoDeclWithResolver )
import Language.Haskell.Synthesis.Kind (Kind (..))
import Language.Haskell.Synthesis.Collection (observedListLength)
import Language.Haskell.Synthesis.Name (maximumTupleArity)
import Language.Haskell.Synthesis.KindInference (KindAssumptions, GroundKind)
import Language.Haskell.Synthesis.SourceKind
import Language.Haskell.Synthesis.Type (Variable)

-- | Accept star/arrow/parenthesized ground kinds on forall binders and
-- check them jointly with every source use. This operates before synonym
-- expansion, including phantom applications. Use 'expandSourceTypeKinds'
-- from the shared expansion module for the next transformation; projecting
-- and expanding only the type loses the annotation authority.
convertKindedSourceType
  :: Monad m
  => KindAssumptions
  -> TypeResolver
  -> Maybe (HSE.ModuleName SrcSpanInfo)
  -> HSE.Type SrcSpanInfo
  -> ExceptT String m (SourceTypeKinds (Variable Int), TypeVarIndex)
convertKindedSourceType assumptions resolver currentModule source = do
  (erased, annotations) <- either throwE pure $ detach [] source
  (ty, variables) <- convertTypeNoDeclWithResolver resolver currentModule erased
  checked <- either (throwE . show) pure $ prepareSourceTypeKinds assumptions ty annotations
  pure (checked, variables)

-- Paths describe the raw converter's structural output. Parentheses vanish,
-- tuple syntax becomes an application spine, and each context argument is
-- converted independently. Later canonicalization must transport these paths.
detach
  :: [SourceTypeStep]
  -> HSE.Type SrcSpanInfo
  -> Either String (HSE.Type SrcSpanInfo, [SourceKindAnnotation])
detach path source = case source of
  HSE.TyFun location parameter result -> binary (HSE.TyFun location)
    FunctionParameter parameter FunctionResult result
  HSE.TyApp location function argument -> binary (HSE.TyApp location)
    ApplicationFunction function ApplicationArgument argument
  HSE.TyList location element -> do
    (clean, annotations) <- below ApplicationArgument element
    pure (HSE.TyList location clean, annotations)
  HSE.TyParen location inner -> do
    (clean, annotations) <- detach path inner
    pure (HSE.TyParen location clean, annotations)
  HSE.TyTuple location HSE.Boxed fields -> do
    let width = observedListLength maximumTupleArity fields
    if width < 2 || width > maximumTupleArity
      then Left $ "invalid boxed tuple arity " ++ show width
      else pure ()
    parts <- sequence
      [ detach (path ++ replicate (width - index - 1) ApplicationFunction ++ [ApplicationArgument]) field
      | (index, field) <- zip [0..] fields ]
    pure (HSE.TyTuple location HSE.Boxed $ map fst parts, concatMap snd parts)
  HSE.TyInfix location left operator@HSE.UnpromotedName{} right -> do
    (cleanLeft, leftAnnotations) <- detach
      (path ++ [ApplicationFunction,ApplicationArgument]) left
    (cleanRight, rightAnnotations) <- below ApplicationArgument right
    pure (HSE.TyInfix location cleanLeft operator cleanRight, leftAnnotations ++ rightAnnotations)
  HSE.TyForall location variables context body -> do
    binderParts <- traverse (sequence . zipWith binder [0..]) variables
    contextParts <- traverse (detachContext path) context
    (cleanBody, bodyAnnotations) <- below ForallBody body
    pure (HSE.TyForall location (map fst <$> binderParts) (fst <$> contextParts) cleanBody,
      maybe [] (concatMap snd) binderParts ++ maybe [] snd contextParts ++ bodyAnnotations)
  -- Unsupported type vocabulary is left for the existing resolver/converter
  -- to reject; this adapter never erases a whole-type kind annotation.
  _ -> pure (source, [])
 where
  below step = detach (path ++ [step])
  binary constructor leftStep left rightStep right = do
    (cleanLeft, leftAnnotations) <- below leftStep left
    (cleanRight, rightAnnotations) <- below rightStep right
    pure (constructor cleanLeft cleanRight, leftAnnotations ++ rightAnnotations)
  binder slot original = case original of
    HSE.UnkindedVar{} -> pure (original, [])
    HSE.KindedVar location name kind -> do
      checkedKind <- convertGroundKind kind
      pure (HSE.UnkindedVar location name, [SourceKindAnnotation path slot checkedKind])

detachContext
  :: [SourceTypeStep]
  -> HSE.Context SrcSpanInfo
  -> Either String (HSE.Context SrcSpanInfo, [SourceKindAnnotation])
detachContext path context = case context of
  HSE.CxEmpty{} -> pure (context, [])
  HSE.CxSingle location assertion -> do
    (clean, annotations) <- assertionAt 0 assertion
    pure (HSE.CxSingle location clean, annotations)
  HSE.CxTuple location assertions -> do
    parts <- sequence $ zipWith assertionAt [0..] assertions
    pure (HSE.CxTuple location $ map fst parts, concatMap snd parts)
 where
  assertionAt index (HSE.ParenA location inner) = do
    (clean, annotations) <- assertionAt index inner
    pure (HSE.ParenA location clean, annotations)
  assertionAt index original@(HSE.TypeA location ty) = case splitClassApplication ty of
    Nothing -> pure (original, [])
    Just (name, arguments) -> do
      parts <- sequence
        [detach (path ++ [ForallConstraintArgument index argumentIndex]) argument
        | (argumentIndex, argument) <- zip [0..] arguments]
      pure (HSE.TypeA location $ foldl (HSE.TyApp location)
        (HSE.TyCon location name) (map fst parts), concatMap snd parts)
  assertionAt _ original = pure (original, [])

convertGroundKind :: HSE.Kind SrcSpanInfo -> Either String GroundKind
convertGroundKind kind = case kind of
  HSE.TyStar{} -> Right ProperTypeKind
  HSE.TyFun _ parameter result -> FunctionKind
    <$> convertGroundKind parameter <*> convertGroundKind result
  HSE.TyParen _ inner -> convertGroundKind inner
  _ -> Left "unsupported source binder kind; expected a ground star/arrow kind"
