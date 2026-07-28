{-# LANGUAGE ScopedTypeVariables #-}

-- | Real-GHC expression evaluation for the shared REPL, built on hint.
--
-- Synthesis queries never execute code, and the synthesis environment is
-- deliberately parser-level pseudo-Haskell that real GHC cannot compile.
-- Evaluation therefore targets the real package universe: loaded files are
-- compiled, the checked prompt context selects the bindings that enter scope,
-- and a load or context failure falls back to Prelude with one advisory saying
-- why. Every call runs a fresh interpreter session, so evaluation always sees
-- the current workspace and leaves no state behind.
module Language.Haskell.Djex.REPL.Eval
  ( EvalOutcome (..)
  , evaluateExpression
  ) where

import qualified Control.Monad.Catch as Catch
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import qualified Language.Haskell.Exts.Pretty as HSE
import qualified Language.Haskell.Exts.Syntax as HSE
import qualified Language.Haskell.Interpreter as Hint

import Language.Haskell.Djex.REPL.Scope
  ( ReplScope
  , ScopeEntry (..)
  , parseScopeImport
  , scopeEntries
  )
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error, Warning)
  , contextualDiagnostic
  )
import qualified Language.Haskell.Synthesis.Name as SharedName

-- | One evaluation against the real GHC package universe.
data EvalOutcome = EvalOutcome
  { evalResult :: Either Diagnostic String
    -- ^ The shown value, or why the expression was rejected.
  , evalAdvisories :: [Diagnostic]
    -- ^ Scope compromises, reported even when evaluation succeeds.
  }

-- | Evaluate one expression. Every workspace module is compiled so prompt
-- imports can refer to its local dependency closure, but only the supplied
-- prompt context is opened: starred modules expose their full top-level scope,
-- while ordinary modules and imports expose exports exactly as GHCi does. A
-- workspace or context failure degrades to Prelude-only scope rather than
-- failing an otherwise independent expression.
evaluateExpression
  :: Maybe ReplScope
  -> [(String, FilePath)]
  -> String
  -> IO EvalOutcome
evaluateExpression promptScope workspaceModules expression = do
  outcome <- Hint.runInterpreter $ do
    scopeFailure <- establishScope
    evaluation <- Catch.try (Hint.eval expression)
      :: Hint.Interpreter (Either Hint.InterpreterError String)
    pure (scopeFailure, evaluation)
  pure $ case outcome of
    Left failure -> EvalOutcome
      { evalResult = Left $ evaluationFailure failure
      , evalAdvisories = []
      }
    Right (scopeFailure, evaluation) -> EvalOutcome
      { evalResult = case evaluation of
          Left failure -> Left $ evaluationFailure failure
          Right value -> Right value
      , evalAdvisories = map scopeAdvisory $ toList scopeFailure
      }
 where
  toList = maybe [] pure

  establishScope = Catch.try installPromptScope >>= \attempt -> case attempt of
    Right () -> pure Nothing
    Left (failure :: Hint.InterpreterError) -> do
      Hint.reset
      Hint.setImports ["Prelude"]
      pure $ Just failure

  installPromptScope = do
    -- Hint renders qualified and restricted imports through a temporary
    -- module. These extensions let that module reproduce modern import specs
    -- such as @type T@ and @pattern P@.
    Hint.set
      [ Hint.languageExtensions Hint.:=
          [Hint.ExplicitNamespaces, Hint.PatternSynonyms]
      ]
    (topLevel, imports) <- either Catch.throwM pure
      $ interpreterContext promptScope
    -- Validate the context before compiling any workspace module. In
    -- particular, a safe import that Hint cannot represent must not be
    -- weakened after loading the module it was meant to guard.
    if null workspaceModules
      then pure ()
      else Hint.loadModules $ map snd workspaceModules
    Hint.setTopLevelModules topLevel
    Hint.setImportsF imports

-- | Translate the checked prompt scope to Hint's GHC context. The installed
-- Prelude is implicit only in the same context shape where GHCi adds it: no
-- starred module and no explicit entry for Prelude. This deliberately does
-- not consult Djex's source-level implicit-Prelude projection, which can only
-- mention a locally loaded Prelude module.
interpreterContext
  :: Maybe ReplScope
  -> Either Hint.InterpreterError ([String], [Hint.ModuleImport])
interpreterContext promptScope = do
  explicitImports <- catMaybes <$> traverse entryImport entries
  pure (topLevel, implicitPrelude ++ explicitImports)
 where
  entries = maybe [] scopeEntries promptScope
  topLevel =
    [ SharedName.renderModuleName moduleName
    | ScopeModule _ True moduleName <- entries
    ]
  implicitPrelude
    | any isStarred entries || any mentionsPrelude entries = []
    | otherwise = [plainImport "Prelude"]

  isStarred (ScopeModule _ True _) = True
  isStarred _ = False

  mentionsPrelude (ScopeModule _ _ moduleName) =
    SharedName.renderModuleName moduleName == "Prelude"
  mentionsPrelude (ScopeImport source) = case parseScopeImport source of
    Right declaration ->
      hseModuleName (HSE.importModule declaration) == "Prelude"
    Left _ -> False

entryImport
  :: ScopeEntry
  -> Either Hint.InterpreterError (Maybe Hint.ModuleImport)
entryImport entry = case entry of
  ScopeModule _ True _ -> Right Nothing
  ScopeModule _ False moduleName -> Right $ Just $ plainImport
    $ SharedName.renderModuleName moduleName
  ScopeImport source -> case parseScopeImport source of
    Left _ -> Left $ Hint.UnknownError
      "the retained REPL import no longer parses"
    Right declaration
      | HSE.importSafe declaration -> Left $ Hint.UnknownError
          "the evaluator cannot preserve import safe; refusing to weaken it"
      | otherwise -> Right $ Just $ Hint.ModuleImport
          { Hint.modName = hseModuleName $ HSE.importModule declaration
          , Hint.modQual = importQualification declaration
          , Hint.modImp = importSurface declaration
          }

plainImport :: String -> Hint.ModuleImport
plainImport moduleName = Hint.ModuleImport
  { Hint.modName = moduleName
  , Hint.modQual = Hint.NotQualified
  , Hint.modImp = Hint.NoImportList
  }

importQualification
  :: HSE.ImportDecl annotation
  -> Hint.ModuleQualification
importQualification declaration = case
    (HSE.importQualified declaration, HSE.importAs declaration) of
  (True, Nothing) -> Hint.QualifiedAs Nothing
  (True, Just alias) -> Hint.QualifiedAs $ Just $ hseModuleName alias
  (False, Just alias) -> Hint.ImportAs $ hseModuleName alias
  (False, Nothing) -> Hint.NotQualified

importSurface :: HSE.ImportDecl annotation -> Hint.ImportList
importSurface declaration = case HSE.importSpecs declaration of
  Nothing -> Hint.NoImportList
  Just (HSE.ImportSpecList _ hiding specifications)
    | hiding -> Hint.HidingList $ map HSE.prettyPrint specifications
    | otherwise -> Hint.ImportList $ map HSE.prettyPrint specifications

hseModuleName :: HSE.ModuleName annotation -> String
hseModuleName (HSE.ModuleName _ moduleName) = moduleName

evaluationFailure :: Hint.InterpreterError -> Diagnostic
evaluationFailure failure = contextualDiagnostic Error "DJEX_REPL_EVAL"
  "cannot evaluate the expression" $ renderInterpreterError failure

-- One advisory with the first compiler message explains a degraded scope
-- without drowning the prompt in errors for every pseudo-Haskell module or
-- every rejected context entry.
scopeAdvisory :: Hint.InterpreterError -> Diagnostic
scopeAdvisory failure = contextualDiagnostic Warning "DJEX_REPL_EVAL_SCOPE"
  "loaded sources or prompt context are not evaluable; using Prelude scope only"
  $ firstLines $ renderInterpreterError failure
 where
  firstLines = intercalate "; " . take 2 . filter (not . null) . lines

renderInterpreterError :: Hint.InterpreterError -> String
renderInterpreterError failure = case failure of
  Hint.WontCompile problems ->
    intercalate "\n" $ map Hint.errMsg problems
  Hint.UnknownError message -> message
  Hint.NotAllowed message -> message
  Hint.GhcException message -> message
