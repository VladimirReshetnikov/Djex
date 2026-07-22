-- | Private command policy shared by the one-shot command and REPL.
--
-- This module owns common option-value parsing plus checked query execution,
-- selection, rendering, and reporting. Environment loading and exit policy
-- remain frontend concerns because interactive replacement is transactional
-- while one-shot loading is terminal.
module Language.Haskell.Djex.Command
  ( RenderMode (..)
  , PresentationOptions (..)
  , defaultOneShotPresentationOptions
  , defaultInteractivePresentationOptions
  , defaultResultTargetSpelling
  , parseResultTarget
  , parseSelectionMode
  , parseRenderMode
  , parseQualification
  , positiveInt
  , nonNegativeInt
  , nonNegativeInteger
  , boundedNonNegativeInt
  , boundedPenalty
  , selectionModeName
  , renderModeName
  , qualificationName
  , renderBounded
  , prepareDjinnQueryOptions
  , executeDjinnCommand
  , executeExferenceCommand
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
import Data.Char (isSpace, toLower)
import Data.List (intercalate)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Text.Read (readMaybe)

import Language.Haskell.Djex
import Language.Haskell.Djex.Exference.HaskellSrc
  ( parseExferenceRequestWithCheckedTarget
  )

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

-- | Selection and rendering defaults for deterministic one-shot commands.
defaultOneShotPresentationOptions :: PresentationOptions
defaultOneShotPresentationOptions = PresentationOptions
  { presentationSelection = SelectBest
  , presentationRenderMode = RenderDefinition
  , presentationQualification = FullyQualified
  }

-- | Latency-oriented defaults for an interactive session.
defaultInteractivePresentationOptions :: PresentationOptions
defaultInteractivePresentationOptions = defaultOneShotPresentationOptions
  {presentationSelection = SelectFirst}

-- | Built-in generated binding name shared by both frontends.
defaultResultTargetSpelling :: String
defaultResultTargetSpelling = "djexResult"

-- | Parse and validate a generated binding name without backend-specific work.
parseResultTarget :: String -> Either String DefinitionName
parseResultTarget source = do
  name <- either (Left . renderNameError) Right $ parseName source
  either (Left . show) Right $ mkDefinitionName name

-- | Parse a candidate-selection setting with a caller-owned option name.
parseSelectionMode :: String -> String -> Either String SelectionMode
parseSelectionMode subject source = case normalize source of
  "first" -> Right SelectFirst
  "best" -> Right SelectBest
  "all" -> Right SelectAll
  _ -> Left $ subject ++ " must be first, best, or all"

-- | Parse a result-rendering setting with a caller-owned option name.
parseRenderMode :: String -> String -> Either String RenderMode
parseRenderMode subject source = case normalize source of
  "definition" -> Right RenderDefinition
  "expression" -> Right RenderExpression
  _ -> Left $ subject ++ " must be definition or expression"

-- | Parse a shared name-qualification setting.
parseQualification :: String -> String -> Either String Qualification
parseQualification subject source = case normalize source of
  "none" -> Right Unqualified
  "identifiers" -> Right QualifyIdentifiers
  "full" -> Right FullyQualified
  _ -> Left $ subject ++ " must be none, identifiers, or full"

-- Parse machine-sized values through Integer first. Reading an out-of-range
-- literal directly as Int silently wraps modulo the host Int range.
positiveInt :: String -> String -> Either String Int
positiveInt subject = checkedInt 1
  $ subject ++ " must be a positive integer"

nonNegativeInt :: String -> String -> Either String Int
nonNegativeInt subject = checkedInt 0
  $ subject ++ " must be a non-negative integer"

checkedInt :: Integer -> String -> String -> Either String Int
checkedInt lowerBound failure source = case readMaybe $ trim source of
  Just value
    | value >= lowerBound
    , value <= toInteger (maxBound :: Int) -> Right $ fromInteger value
  _ -> Left failure

nonNegativeInteger :: String -> String -> Either String Integer
nonNegativeInteger subject source = case readMaybe $ trim source of
  Just value | value >= 0 -> Right value
  _ -> Left $ subject ++ " must be a non-negative integer"

boundedNonNegativeInt :: String -> String -> Either String (Maybe Int)
boundedNonNegativeInt _ source | normalize source == "unbounded" = Right Nothing
boundedNonNegativeInt subject source = Just <$> nonNegativeInt subject source

boundedPenalty :: String -> String -> Either String (Maybe Penalty)
boundedPenalty _ source | normalize source == "unbounded" = Right Nothing
boundedPenalty subject source = case readMaybe $ trim source of
  Just value
    | value >= 0
    , not $ isNaN value || isInfinite value -> Right $ Just $ Penalty value
  _ -> Left $ subject
    ++ " must be a finite non-negative number or unbounded"

selectionModeName :: SelectionMode -> String
selectionModeName SelectFirst = "first"
selectionModeName SelectBest = "best"
selectionModeName (SelectBestLookahead count) = "best-lookahead=" ++ show count
selectionModeName SelectAll = "all"

renderModeName :: RenderMode -> String
renderModeName RenderDefinition = "definition"
renderModeName RenderExpression = "expression"

qualificationName :: Qualification -> String
qualificationName Unqualified = "none"
qualificationName QualifyIdentifiers = "identifiers"
qualificationName FullyQualified = "full"

renderBounded :: Show value => Maybe value -> String
renderBounded = maybe "unbounded" show

-- | Apply presentation-driven proof enumeration without changing the caller's
-- resource limits. Djinn never needs its historical internal sorting because
-- the shared selection layer owns result ordering.
prepareDjinnQueryOptions
  :: PresentationOptions
  -> QueryOptions
  -> QueryOptions
prepareDjinnQueryOptions presentation options = options
  { optionAlternatives = presentationSelection presentation /= SelectFirst
  , optionSorted = False
  }

-- | Parse, execute, and present one checked Djinn query.
executeDjinnCommand
  :: PresentationOptions
  -> DjinnSession
  -> QueryOptions
  -> DefinitionName
  -> FilePath
  -> String
  -> IO ExitCode
executeDjinnCommand presentation session options target sourceName source =
  case parseDjinnRequestWithCheckedTarget
      session
      (prepareDjinnQueryOptions presentation options)
      target
      sourceName
      source of
    Left failure -> diagnosticFailure failure
    Right request -> case runDjinnQuery session request of
      Left failure -> diagnosticFailure failure
      Right result -> presentDjinn presentation result

-- | Parse, execute, and present one checked Exference query.
executeExferenceCommand
  :: PresentationOptions
  -> ExferenceSession
  -> ExferenceOptions
  -> DefinitionName
  -> FilePath
  -> String
  -> IO ExitCode
executeExferenceCommand presentation session options target sourceName source =
  case parseExferenceRequestWithCheckedTarget
      session options target sourceName source of
    Left failure -> diagnosticFailure failure
    Right request -> case runExferenceQuery session request of
      Left failure -> diagnosticFailure failure
      Right results -> presentExference presentation results

normalize :: String -> String
normalize = map toLower . trim

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

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
