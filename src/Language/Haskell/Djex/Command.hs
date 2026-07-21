-- | Private presentation boundary shared by the one-shot command and REPL.
--
-- Search sessions and request parsing remain frontend concerns. This module
-- owns the policy-neutral act of selecting, rendering, and reporting a checked
-- result so interactive and one-shot invocations cannot drift in output,
-- residual-constraint handling, or completion diagnostics.
module Language.Haskell.Djex.Command
  ( RenderMode (..)
  , PresentationOptions (..)
  , presentDjinn
  , presentExference
  , diagnosticFailure
  , emitDiagnostic
  , runtimeFailure
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Bifunctor (first)
import Data.List (intercalate)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (hFlush, hPutStrLn, stderr, stdout)

import Language.Haskell.Djex

-- | Whether a candidate is presented as a complete binding or an expression.
data RenderMode = RenderDefinition | RenderExpression
  deriving (Eq, Show)

-- | Frontend-owned selection and rendering policy for one checked result.
data PresentationOptions = PresentationOptions
  { presentationSelection :: SelectionMode
  , presentationRenderMode :: RenderMode
  , presentationQualification :: Qualification
  }
  deriving (Eq, Show)

-- | Select, render, and report one terminal Djinn result.
presentDjinn :: PresentationOptions -> DjinnResult -> IO ExitCode
presentDjinn options result = case traverse (renderDjinn options) candidates of
  Left failure -> renderFailure "DJEX_DJINN_RENDER" failure
  Right rendered -> do
    printCandidates rendered
    reportDjinnOutcome evidence progress
    reportTruncation progress
    pure ExitSuccess
 where
  evidence = resultEvidence result
  selection = selectQueryResults
    (presentationSelection options)
    candidateDetails
    (const True)
    [result]
  candidates = selectionCandidates selection
  progress = selectionProgress selection

-- | Select, render, and report Exference's lazy result sequence.
presentExference :: PresentationOptions -> [ExferenceResult] -> IO ExitCode
presentExference options results
  | presentationSelection options == SelectAll =
      presentAllExference options results
presentExference options results = case traverse
    (renderExferenceBlock options) candidates of
  Left failure -> renderFailure "DJEX_EXF_RENDER" failure
  Right rendered -> do
    printCandidates rendered
    when (null candidates) $ reportNoExferenceResult progress
    reportTruncation progress
    pure ExitSuccess
 where
  selection = selectQueryResults
    (presentationSelection options)
    (exferenceCandidateComplexity . exferenceCandidateMetrics)
    (const True)
    results
  candidates = selectionCandidates selection
  progress = selectionProgress selection

presentAllExference :: PresentationOptions -> [ExferenceResult] -> IO ExitCode
presentAllExference options results = do
  outcome <- runExceptT $ foldAllQueryResultsM
    (const True) printOne False results
  case outcome of
    Left failure -> renderFailure "DJEX_EXF_RENDER" failure
    Right (progress, foundAny) -> do
      when (not foundAny) $ reportNoExferenceResult progress
      reportTruncation progress
      pure ExitSuccess
 where
  printOne printed candidate = do
    rendered <- ExceptT $ pure $ renderExferenceBlock options candidate
    liftIO $ do
      when printed $ putStrLn "\n-- or\n"
      putStrLn rendered
      hFlush stdout
    pure True

renderDjinn
  :: PresentationOptions
  -> DjinnCandidate
  -> Either RenderError String
renderDjinn = renderCandidate
  renderDjinnCandidateDefinition renderDjinnCandidateExpression

renderExference
  :: PresentationOptions
  -> ExferenceCandidate
  -> Either RenderError String
renderExference = renderCandidate
  renderExferenceCandidateDefinition renderExferenceCandidateExpression

renderCandidate
  :: (Qualification -> candidate -> Either RenderError String)
  -> (Qualification -> candidate -> Either RenderError String)
  -> PresentationOptions
  -> candidate
  -> Either RenderError String
renderCandidate definitionRenderer expressionRenderer options =
  case presentationRenderMode options of
    RenderDefinition -> definitionRenderer qualification
    RenderExpression -> expressionRenderer qualification
 where
  qualification = presentationQualification options

-- A residual obligation is part of the candidate's generated interface, not
-- an out-of-band runtime warning. Keeping it as a Haskell comment beside the
-- expression also preserves candidate association in all-results output.
renderExferenceBlock
  :: PresentationOptions
  -> ExferenceCandidate
  -> Either ExferenceBlockRenderError String
renderExferenceBlock options candidate = do
  rendered <- first ExferenceTermRenderError
    $ renderExference options candidate
  constraints <- first ExferenceConstraintRenderError
    $ renderExferenceResidualConstraintsWithQualification
        (presentationQualification options) candidate
  pure $ case constraints of
    [] -> rendered
    _ -> "-- requires: " ++ intercalate ", " constraints ++ "\n" ++ rendered

data ExferenceBlockRenderError
  = ExferenceTermRenderError RenderError
  | ExferenceConstraintRenderError ExferenceResidualRenderError
  deriving (Eq, Show)

printCandidates :: [String] -> IO ()
printCandidates [] = pure ()
printCandidates rendered = putStrLn $ intercalate "\n\n-- or\n\n" rendered

reportDjinnOutcome :: QueryEvidence -> Maybe Progress -> IO ()
reportDjinnOutcome ValidatedCandidates _ = pure ()
reportDjinnOutcome ProvedUninhabitable _ = emitDiagnostic
  $ codedDiagnostic Info "DJEX_DJINN_UNINHABITABLE"
      "Djinn proved that the requested type has no inhabitant"
reportDjinnOutcome RequiresTargetReference _ = emitDiagnostic
  $ codedDiagnostic Info "DJEX_DJINN_TARGET_REFERENCE"
      "Djinn found no safe inhabitant without referring to the generated target"
reportDjinnOutcome NoEvidence progress = emitDiagnostic
  $ contextualDiagnostic Info "DJEX_DJINN_UNDECIDED"
      "Djinn established no inhabitation result"
      (maybe "no search batch" show progress)

reportNoExferenceResult :: Maybe Progress -> IO ()
reportNoExferenceResult progress = emitDiagnostic
  $ contextualDiagnostic Info "DJEX_EXF_NO_RESULT" message
      (maybe "no search batch" show progress)
 where
  message = case observeProgress progress of
    ObservedFinished ->
      "Exference exhausted its configured search without finding a candidate"
    _ -> "Exference found no candidate in the inspected search"

reportTruncation :: Maybe Progress -> IO ()
reportTruncation = mapM_ emitDiagnostic . progressTruncationDiagnostic

-- | Print one structured diagnostic and return the command's runtime failure.
diagnosticFailure :: Diagnostic -> IO ExitCode
diagnosticFailure failure = emitDiagnostic failure >> pure runtimeFailure

renderFailure :: Show failure => String -> failure -> IO ExitCode
renderFailure code failure = internalFailure code $ show failure

internalFailure :: String -> String -> IO ExitCode
internalFailure code context = do
  emitDiagnostic
    $ contextualDiagnostic Error code
        "cannot present the checked search result" context
  pure runtimeFailure

-- | Emit a shared diagnostic in its compiler-like text form.
emitDiagnostic :: Diagnostic -> IO ()
emitDiagnostic = hPutStrLn stderr . renderDiagnostic

-- | Exit status for a checked load, query, or rendering failure.
runtimeFailure :: ExitCode
runtimeFailure = ExitFailure 1
