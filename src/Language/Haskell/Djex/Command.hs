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
  , FieldSelectors
  , noFieldSelectors
  , prepareDjinnQueryOptions
  , executeDjinnCommand
  , executeExferenceCommand
  , executeExferenceCommandInScope
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
import qualified Data.Map.Strict as Map
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Text.Read (readMaybe)

import Language.Haskell.Djex
import Language.Haskell.Djex.Exference.HaskellSrc
  ( ExferenceQueryScope
  , parseExferenceRequestWithCheckedTarget
  , parseExferenceRequestWithCheckedTargetInScope
  )
import Language.Haskell.Djex.Text (normalize, trim)

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

-- | Parse an 'Int' setting that must be at least @1@, with a caller-owned
-- option name for the failure message. Values above 'maxBound' are
-- rejected rather than wrapped.
--
-- Parse machine-sized values through Integer first. Reading an out-of-range
-- literal directly as Int silently wraps modulo the host Int range.
positiveInt :: String -> String -> Either String Int
positiveInt subject = checkedInt 1
  $ subject ++ " must be a positive integer"

-- | Parse an 'Int' setting that must be at least @0@, with a caller-owned
-- option name for the failure message. Values above 'maxBound' are
-- rejected rather than wrapped.
nonNegativeInt :: String -> String -> Either String Int
nonNegativeInt subject = checkedInt 0
  $ subject ++ " must be a non-negative integer"

checkedInt :: Integer -> String -> String -> Either String Int
checkedInt lowerBound failure source = case readMaybe $ trim source of
  Just value
    | value >= lowerBound
    , value <= toInteger (maxBound :: Int) -> Right $ fromInteger value
  _ -> Left failure

-- | Parse an arbitrary-precision setting that must be at least @0@, with a
-- caller-owned option name for the failure message.
nonNegativeInteger :: String -> String -> Either String Integer
nonNegativeInteger subject source = case readMaybe $ trim source of
  Just value | value >= 0 -> Right value
  _ -> Left $ subject ++ " must be a non-negative integer"

-- | Parse an optional limit: the word @unbounded@ (case-insensitively)
-- yields 'Nothing', anything else must satisfy 'nonNegativeInt'.
boundedNonNegativeInt :: String -> String -> Either String (Maybe Int)
boundedNonNegativeInt _ source | normalize source == "unbounded" = Right Nothing
boundedNonNegativeInt subject source = Just <$> nonNegativeInt subject source

-- | Parse an optional penalty bound: the word @unbounded@
-- (case-insensitively) yields 'Nothing', anything else must be a finite
-- non-negative number.
boundedPenalty :: String -> String -> Either String (Maybe Penalty)
boundedPenalty _ source | normalize source == "unbounded" = Right Nothing
boundedPenalty subject source = case readMaybe $ trim source of
  Just value
    | value >= 0
    , not $ isNaN value || isInfinite value -> Right $ Just $ Penalty value
  _ -> Left $ subject
    ++ " must be a finite non-negative number or unbounded"

-- | The setting spelling of a selection mode, as accepted by
-- 'parseSelectionMode'; a lookahead mode renders as @best-lookahead=N@,
-- which that parser does not accept.
selectionModeName :: SelectionMode -> String
selectionModeName SelectFirst = "first"
selectionModeName SelectBest = "best"
selectionModeName (SelectBestLookahead count) = "best-lookahead=" ++ show count
selectionModeName SelectAll = "all"

-- | The setting spelling of a render mode, as accepted by
-- 'parseRenderMode'.
renderModeName :: RenderMode -> String
renderModeName RenderDefinition = "definition"
renderModeName RenderExpression = "expression"

-- | The setting spelling of a qualification level, as accepted by
-- 'parseQualification'.
qualificationName :: Qualification -> String
qualificationName Unqualified = "none"
qualificationName QualifyIdentifiers = "identifiers"
qualificationName FullyQualified = "full"

-- | Render an optional limit for display: 'Nothing' as @unbounded@,
-- otherwise the shown value, matching the spellings accepted by
-- 'boundedNonNegativeInt' and 'boundedPenalty'.
renderBounded :: Show value => Maybe value -> String
renderBounded = maybe "unbounded" show

-- | Selector spellings for @(constructor, field index)@ positions, used to
-- present a candidate's field projections by selector name.
type FieldSelectors = Map.Map (Name, Int) Name

-- | The empty selector table used by frontends without a source workspace.
noFieldSelectors :: FieldSelectors
noFieldSelectors = Map.empty

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
  -> FieldSelectors
  -> DjinnSession
  -> QueryOptions
  -> DefinitionName
  -> FilePath
  -> String
  -> IO ExitCode
executeDjinnCommand presentation fieldSelectors session options target
    sourceName source =
  executeParsedQuery
    (parseDjinnRequestWithCheckedTarget
      session
      (prepareDjinnQueryOptions presentation options)
      target
      sourceName
      source)
    (runDjinnQuery session)
    (presentDjinn presentation fieldSelectors)

-- | Parse, execute, and present one checked Exference query.
executeExferenceCommand
  :: PresentationOptions
  -> FieldSelectors
  -> ExferenceSession
  -> ExferenceOptions
  -> DefinitionName
  -> FilePath
  -> String
  -> IO ExitCode
executeExferenceCommand presentation fieldSelectors session options target
    sourceName source =
  executeParsedQuery
    (parseExferenceRequestWithCheckedTarget
      session options target sourceName source)
    (runExferenceQuery session)
    (presentExference presentation fieldSelectors)

-- | Parse, execute, and present an Exference query in an interactive module
-- scope. The supplied session may already have its search dictionary narrowed;
-- its complete inventory is still retained for qualified type elaboration.
executeExferenceCommandInScope
  :: PresentationOptions
  -> FieldSelectors
  -> ExferenceSession
  -> ExferenceOptions
  -> DefinitionName
  -> ExferenceQueryScope
  -> FilePath
  -> String
  -> IO ExitCode
executeExferenceCommandInScope presentation fieldSelectors session options
    target scope sourceName source =
  executeParsedQuery
    (parseExferenceRequestWithCheckedTargetInScope
      session options target scope sourceName source)
    (runExferenceQuery session)
    (presentExference presentation fieldSelectors)

-- Run one parsed request and present its result, reporting a parse or query
-- failure as a diagnostic.  Every command above is this shape.
executeParsedQuery
  :: Either Diagnostic request
  -> (request -> Either Diagnostic result)
  -> (result -> IO ExitCode)
  -> IO ExitCode
executeParsedQuery parsed run present = case parsed of
  Left failure -> diagnosticFailure failure
  Right request -> case run request of
    Left failure -> diagnosticFailure failure
    Right result -> present result

-- | Select, render, and report one terminal Djinn result.
presentDjinn
  :: PresentationOptions
  -> FieldSelectors
  -> DjinnResult
  -> IO ExitCode
presentDjinn options fieldSelectors result = case
    traverse (renderDjinn options) candidates of
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
  candidates = map
    (fmap $ projectFieldSelectorsWithoutEta fieldSelectors)
    $ selectionCandidates selection
  progress = selectionProgress selection

-- | Select, render, and report Exference's lazy result sequence.
presentExference
  :: PresentationOptions
  -> FieldSelectors
  -> [ExferenceResult]
  -> IO ExitCode
presentExference options fieldSelectors results
  | presentationSelection options == SelectAll =
      presentAllExference options fieldSelectors results
presentExference options fieldSelectors results = case traverse
    (renderExferenceBlock options) candidates of
  Left failure -> renderFailure "DJEX_EXF_RENDER" failure
  Right rendered -> do
    printCandidates rendered
    when (null candidates) $ reportNoExferenceResult progress
    reportTruncation progress
    pure ExitSuccess
 where
  -- When record selectors are in scope, a first-candidate request looks a
  -- few results ahead and shows the one whose selector-normalized spelling
  -- is smallest: search order distinguishes deconstruct-and-rebuild
  -- spellings that presentation renders identically simple or not at all.
  selection = case presentationSelection options of
    SelectFirst
      | not $ Map.null fieldSelectors -> selectQueryResults
          (SelectBestLookahead simplificationLookahead)
          (expressionSize . functionClauseExpression
            . projectFieldSelectors fieldSelectors . candidateOutput)
          (const True)
          results
    mode -> selectQueryResults mode
      (exferenceCandidateComplexity . exferenceCandidateMetrics)
      (const True)
      results
  picked = case presentationSelection options of
    SelectFirst -> take 1 $ selectionCandidates selection
    _ -> selectionCandidates selection
  candidates = map (fmap $ projectFieldSelectors fieldSelectors) picked
  progress = selectionProgress selection

simplificationLookahead :: Int
simplificationLookahead = 3

presentAllExference
  :: PresentationOptions
  -> FieldSelectors
  -> [ExferenceResult]
  -> IO ExitCode
presentAllExference options fieldSelectors results = do
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
    rendered <- ExceptT $ pure $ renderExferenceBlock options
      $ fmap (projectFieldSelectors fieldSelectors) candidate
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
