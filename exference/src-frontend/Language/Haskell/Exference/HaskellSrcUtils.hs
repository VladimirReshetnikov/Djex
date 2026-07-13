module Language.Haskell.Exference.HaskellSrcUtils
  ( contextConstraints
  , moduleNameAndDecls
  , splitClassApplication
  , splitDeclHead
  , splitInstRule
  , withHaskellSrcLocation
  , withHaskellSrcSpan
  )
where

import Language.Haskell.Exference.Diagnostic
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
    go arguments (TyCon _ name) = Just (name, arguments)
    go _ _ = Nothing

splitDeclHead :: DeclHead l -> (Name l, [TyVarBind l])
splitDeclHead (DHead _ name) = (name, [])
splitDeclHead (DHInfix _ variable name) = (name, [variable])
splitDeclHead (DHParen _ head') = splitDeclHead head'
splitDeclHead (DHApp _ head' variable) =
  let (name, variables) = splitDeclHead head'
  in (name, variables ++ [variable])

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

-- | Attach an HSE point location after validating its one-based coordinates.
withHaskellSrcLocation :: SrcLoc -> Diagnostic -> Diagnostic
withHaskellSrcLocation location = withHaskellSrcCoordinates
  (HSE.srcFilename location)
  (HSE.srcLine location)
  (HSE.srcColumn location)
  (HSE.srcLine location)
  (HSE.srcColumn location)

-- | Attach an HSE half-open span after validating its coordinates and order.
withHaskellSrcSpan :: SrcSpan -> Diagnostic -> Diagnostic
withHaskellSrcSpan span' = withHaskellSrcCoordinates
  (HSE.srcSpanFilename span')
  (HSE.srcSpanStartLine span')
  (HSE.srcSpanStartColumn span')
  (HSE.srcSpanEndLine span')
  (HSE.srcSpanEndColumn span')

-- HSE promises positive, ordered coordinates. If a constructed or future
-- parser value violates that contract, preserve a valid start as a point
-- location when possible and record the adapter failure as structured
-- context. An invalid start still retains its source filename.
withHaskellSrcCoordinates
  :: FilePath
  -> Int
  -> Int
  -> Int
  -> Int
  -> Diagnostic
  -> Diagnostic
withHaskellSrcCoordinates source startLine startColumn endLine endColumn value =
  case nativeSpan of
    Right span' -> withLocation source span' value
    Left failure ->
      withContext ("haskell-src-exts supplied an invalid source location: "
        ++ show failure)
      $ fallbackLocation
 where
  nativeSpan = do
    start <- mkSourcePosition startLine startColumn
    end <- mkSourcePosition endLine endColumn
    mkSourceSpan start end

  fallbackLocation = case do
      start <- mkSourcePosition startLine startColumn
      mkSourceSpan start start of
    Right point -> withLocation source point value
    Left _ -> withSource source value
