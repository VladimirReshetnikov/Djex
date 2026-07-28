module Language.Haskell.Exference.HaskellSrcUtils
  ( contextConstraints
  , haskellSrcLocation
  , haskellSrcSpanLocation
  , haskellSrcSpanStartLocation
  , moduleNameAndDecls
  , splitClassApplication
  , splitDeclHead
  , splitInstRule
  , withHaskellSrcLocation
  , withHaskellSrcSpan
  )
where

import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , SourceLocation
  , SourceLocationError
  , mkSourcePosition
  , mkSourceSpan
  , sourceLocation
  , withContext
  , withSource
  , withSourceLocation
  )
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc (SrcLoc, SrcSpan)
import qualified Language.Haskell.Exts.SrcLoc as HSE

contextConstraints :: Maybe (Context l) -> [Asst l]
contextConstraints Nothing = []
contextConstraints (Just (CxSingle _ constraint)) = [constraint]
contextConstraints (Just (CxTuple _ constraints)) = constraints
contextConstraints (Just (CxEmpty _)) = []

moduleNameAndDecls :: Module l -> Maybe (ModuleName l, [Decl l])
moduleNameAndDecls (Module _ (Just (ModuleHead _ name _ _)) _ _ declarations) =
  Just (name, declarations)
moduleNameAndDecls (Module location Nothing _ _ declarations) =
  Just (ModuleName location "Main", declarations)
-- XML page and hybrid modules have different declaration semantics and are
-- deliberately outside the ordinary Haskell-module extractor.
moduleNameAndDecls _ = Nothing

splitClassApplication :: Type l -> Maybe (QName l, [Type l])
splitClassApplication = go []
  where
    go arguments (TyApp _ function argument) = go (argument : arguments) function
    go arguments (TyParen _ inner) = go arguments inner
    -- HSE keeps an infix type operator as a dedicated node rather than an
    -- application spine.  Normalize it here so signatures, superclasses, and
    -- instance prerequisites all accept the same symbolic class spelling as
    -- the already-supported prefix form @(:==) left right@.  A promoted
    -- operator is a type-level constructor occurrence, not a class head.
    go arguments (TyInfix _ left (UnpromotedName _ name) right) =
      Just (name, left : right : arguments)
    go arguments (TyCon _ name) = Just (name, arguments)
    go _ _ = Nothing

splitDeclHead :: DeclHead l -> (Name l, [TyVarBind l])
splitDeclHead = go []
  where
    go variables (DHead _ name) = (name, variables)
    go variables (DHInfix _ variable name) = (name, variable : variables)
    go variables (DHParen _ head') = go variables head'
    go variables (DHApp _ head' variable) = go (variable : variables) head'

splitInstRule
  :: InstRule l
  -> Maybe (Maybe [TyVarBind l], Maybe (Context l), QName l, [Type l])
splitInstRule (IParen _ rule) = splitInstRule rule
splitInstRule (IRule _ variables context head') = do
  (name, arguments) <- splitInstHead head'
  pure (variables, context, name, arguments)

splitInstHead :: InstHead l -> Maybe (QName l, [Type l])
splitInstHead = go []
  where
    go arguments (IHApp _ function argument) = go (argument : arguments) function
    go arguments (IHParen _ inner) = go arguments inner
    go arguments (IHCon _ name) = Just (name, arguments)
    go arguments (IHInfix _ argument name) = Just (name, argument : arguments)

-- | Convert an HSE point into the checked neutral location vocabulary.
haskellSrcLocation :: SrcLoc -> Either SourceLocationError SourceLocation
haskellSrcLocation location = do
  point <- mkSourcePosition
    (HSE.srcLine location)
    (HSE.srcColumn location)
  span' <- mkSourceSpan point point
  pure $ sourceLocation (HSE.srcFilename location) span'

-- | Convert a complete HSE half-open span without weakening invalid input.
haskellSrcSpanLocation
  :: SrcSpan
  -> Either SourceLocationError SourceLocation
haskellSrcSpanLocation span' = do
  start <- mkSourcePosition
    (HSE.srcSpanStartLine span')
    (HSE.srcSpanStartColumn span')
  end <- mkSourcePosition
    (HSE.srcSpanEndLine span')
    (HSE.srcSpanEndColumn span')
  checkedSpan <- mkSourceSpan start end
  pure $ sourceLocation (HSE.srcSpanFilename span') checkedSpan

-- | Convert only the valid start of an HSE span into a point location.
-- This is the common best-effort fallback when an end coordinate is invalid.
haskellSrcSpanStartLocation
  :: SrcSpan
  -> Either SourceLocationError SourceLocation
haskellSrcSpanStartLocation span' = do
  start <- mkSourcePosition
    (HSE.srcSpanStartLine span')
    (HSE.srcSpanStartColumn span')
  point <- mkSourceSpan start start
  pure $ sourceLocation (HSE.srcSpanFilename span') point

-- | Attach an HSE point location after validating its one-based coordinates.
withHaskellSrcLocation :: SrcLoc -> Diagnostic -> Diagnostic
withHaskellSrcLocation location value = case haskellSrcLocation location of
  Right checked -> withSourceLocation checked value
  Left failure -> invalidLocation
    (HSE.srcFilename location) failure value

-- | Attach an HSE half-open span after validating its coordinates and order.
withHaskellSrcSpan :: SrcSpan -> Diagnostic -> Diagnostic
withHaskellSrcSpan span' value = case haskellSrcSpanLocation span' of
  Right checked -> withSourceLocation checked value
  Left failure ->
    withContext (invalidLocationContext failure)
      $ either
          (const $ withSource source value)
          (`withSourceLocation` value)
          (haskellSrcSpanStartLocation span')
 where
  source = HSE.srcSpanFilename span'

-- HSE promises positive, ordered coordinates. If a constructed or future
-- parser value violates that contract, preserve a valid start as a point
-- location when possible and record the adapter failure as structured
-- context. An invalid start still retains its source filename.
invalidLocation
  :: FilePath
  -> SourceLocationError
  -> Diagnostic
  -> Diagnostic
invalidLocation source failure =
  withContext (invalidLocationContext failure) . withSource source

invalidLocationContext :: SourceLocationError -> String
invalidLocationContext failure =
  "haskell-src-exts supplied an invalid source location: " ++ show failure
