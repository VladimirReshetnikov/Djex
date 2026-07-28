-- | Shared source-order semantics for the extension switches that control
-- Haskell's implicit @Prelude@ import.
--
-- Both source elaboration and the interactive module scope must make the same
-- decision. Keeping the small state machine here prevents a later enabling
-- pragma from being interpreted differently by the two views of a workspace.
module Language.Haskell.Djex.Internal.ImplicitPrelude
  ( implicitPreludeEnabled
  ) where

import qualified Language.Haskell.Exts.Extension as HSE
import qualified Language.Haskell.Exts.Syntax as HSE

-- | Apply parse-mode extensions first, followed by source pragmas and the
-- switches inside each pragma from left to right. The initial state matches
-- Haskell 2010: implicit Prelude is enabled and rebindable syntax is disabled.
implicitPreludeEnabled
  :: [HSE.Extension]
  -> [HSE.ModulePragma annotation]
  -> Bool
implicitPreludeEnabled extensions pragmas =
    preludeEnabled && not rebindableSyntax
 where
  modeState = foldl' updateExtension (True, False) extensions
  (preludeEnabled, rebindableSyntax) =
    foldl' updatePragma modeState pragmas

  updatePragma state pragma = case pragma of
    HSE.LanguagePragma _ names ->
      foldl' updateSpelling state $ map nameText names
    HSE.OptionsPragma _ _ options ->
      foldl' updateOption state $ words options
    _ -> state

  updateOption state option = case option of
    '-':'X':spelling -> updateSpelling state spelling
    "-fimplicit-prelude" -> updateSpelling state "ImplicitPrelude"
    "-fno-implicit-prelude" -> updateSpelling state "NoImplicitPrelude"
    _ -> state

  updateSpelling (implicit, rebindable) spelling = case spelling of
    "ImplicitPrelude" -> (True, rebindable)
    "NoImplicitPrelude" -> (False, rebindable)
    "RebindableSyntax" -> (implicit, True)
    "NoRebindableSyntax" -> (implicit, False)
    _ -> (implicit, rebindable)

  updateExtension (implicit, rebindable) extension = case extension of
    HSE.EnableExtension HSE.ImplicitPrelude -> (True, rebindable)
    HSE.DisableExtension HSE.ImplicitPrelude -> (False, rebindable)
    HSE.EnableExtension HSE.RebindableSyntax -> (implicit, True)
    HSE.DisableExtension HSE.RebindableSyntax -> (implicit, False)
    HSE.UnknownExtension "ImplicitPrelude" -> (True, rebindable)
    HSE.UnknownExtension "NoImplicitPrelude" -> (False, rebindable)
    HSE.UnknownExtension "RebindableSyntax" -> (implicit, True)
    HSE.UnknownExtension "NoRebindableSyntax" -> (implicit, False)
    _ -> (implicit, rebindable)

nameText :: HSE.Name annotation -> String
nameText name = case name of
  HSE.Ident _ source -> source
  HSE.Symbol _ source -> source
