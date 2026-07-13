module Language.Haskell.Exference.HaskellSrcUtils
  ( contextConstraints
  , moduleNameAndDecls
  , splitClassApplication
  , splitDeclHead
  , splitInstRule
  )
where

import Language.Haskell.Exts.Syntax

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
