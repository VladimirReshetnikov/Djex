{-# LANGUAGE ScopedTypeVariables #-}

-- | Real-GHC expression evaluation for the shared REPL, built on hint.
--
-- Synthesis queries never execute code, and the synthesis environment is
-- deliberately parser-level pseudo-Haskell that real GHC cannot compile.
-- Evaluation therefore targets the real package universe: the loaded file
-- targets join the interpreter scope when they compile, and otherwise the
-- expression is evaluated against Prelude alone with one advisory saying
-- why. Every call runs a fresh interpreter session, so evaluation always
-- sees the current workspace and leaves no state behind.
module Language.Haskell.Djex.REPL.Eval
  ( EvalOutcome (..)
  , evaluateExpression
  ) where

import qualified Control.Monad.Catch as Catch
import Data.List (intercalate)
import qualified Language.Haskell.Interpreter as Hint

import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error, Warning)
  , contextualDiagnostic
  )

-- | One evaluation against the real GHC package universe.
data EvalOutcome = EvalOutcome
  { evalResult :: Either Diagnostic String
    -- ^ The shown value, or why the expression was rejected.
  , evalAdvisories :: [Diagnostic]
    -- ^ Scope compromises, reported even when evaluation succeeds.
  }

-- | Evaluate one expression. The named workspace modules are compiled into
-- the interpreter and opened at top level (their whole source scope, like
-- GHCi's @*M@) when they all load; a failing workspace degrades to
-- Prelude-only scope rather than failing the evaluation.
evaluateExpression
  :: [(String, FilePath)]
  -> String
  -> IO EvalOutcome
evaluateExpression workspaceModules expression = do
  outcome <- Hint.runInterpreter $ do
    scopeFailure <- loadWorkspace
    Hint.setImports ["Prelude"]
    value <- Hint.eval expression
    pure (scopeFailure, value)
  pure $ case outcome of
    Left failure -> EvalOutcome
      { evalResult = Left $ evaluationFailure failure
      , evalAdvisories = []
      }
    Right (scopeFailure, value) -> EvalOutcome
      { evalResult = Right value
      , evalAdvisories = map scopeAdvisory $ toList scopeFailure
      }
 where
  toList = maybe [] pure

  loadWorkspace
    | null workspaceModules = pure Nothing
    | otherwise = Catch.try loader >>= \attempt -> case attempt of
        Right () -> pure Nothing
        Left (failure :: Hint.InterpreterError) -> do
          Hint.reset
          pure $ Just failure
  loader = do
    Hint.loadModules $ map snd workspaceModules
    Hint.setTopLevelModules $ map fst workspaceModules

evaluationFailure :: Hint.InterpreterError -> Diagnostic
evaluationFailure failure = contextualDiagnostic Error "DJEX_REPL_EVAL"
  "cannot evaluate the expression" $ renderInterpreterError failure

-- The synthesis environment fails wholesale under real GHC, so one advisory
-- with the first compiler message explains the degraded scope without
-- drowning the prompt in errors for every pseudo-Haskell module.
scopeAdvisory :: Hint.InterpreterError -> Diagnostic
scopeAdvisory failure = contextualDiagnostic Warning "DJEX_REPL_EVAL_SCOPE"
  "loaded sources are not evaluable; using Prelude scope only"
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
