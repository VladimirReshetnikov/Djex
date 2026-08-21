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
  , parseSearchStrategy
  , parseRenderMode
  , parseQualification
  , positiveInt
  , nonNegativeInt
  , nonNegativeInteger
  , boundedNonNegativeInt
  , boundedPenalty
  , parseHeuristicAssignment
  , heuristicNames
  , renderHeuristics
  , selectionModeName
  , searchStrategyName
  , searchStrategyNames
  , renderModeName
  , qualificationName
  , renderBounded
  , FieldSelectors
  , noFieldSelectors
  , prepareDjinnQueryOptions
  , executeDjinnCommand
  , executeExferenceCommand
  , executeExferenceCommandInScope
  , prepareDjinnPresentation
  , prepareExferencePresentation
  , prepareDiagnosticFailure
  , presentDjinn
  , presentAssessedExference
  , presentExference
  , QueryTimeout
  , noQueryTimeout
  , parseQueryTimeout
  , renderQueryTimeout
  , queryTimeoutSeconds
  , queryTimeoutDiagnostic
  , withinQueryTimeout
  , diagnosticFailure
  , emitDiagnostic
  , runtimeFailure
  ) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Bifunctor (first)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Timeout (timeout)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Text.Read (readMaybe)

import Language.Haskell.Djex
import Language.Haskell.Djex.Command.Output
  ( CommandOutput (..)
  , CommandOutputEvent (..)
  , replayCommandOutput
  )
import Language.Haskell.Exference.Core.Internal.Options
  (heuristicAssignments, heuristicFields)
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

-- | Parse a Djinn search-strategy setting with a caller-owned option name.
parseSearchStrategy :: String -> String -> Either String Strategy
parseSearchStrategy subject source = case normalize source of
  "depth-first" -> Right DepthFirst
  "interleave" -> Right Interleave
  _ -> Left $ subject ++ " must be depth-first or interleave"

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

-- | Parse one @NAME VALUE@ heuristic assignment with a caller-owned option
-- name, resolving the weight through the table that lives beside the record
-- it writes.  The two words are separated by whitespace rather than @=@,
-- because the enclosing @:set@ grammar already gives the first @=@ to the
-- setting name.  Names are matched case-insensitively and exactly; the
-- weight must be finite and non-negative, as search-option validation would
-- otherwise reject the whole query.
parseHeuristicAssignment
  :: String -> String
  -> Either String
      (ExferenceHeuristicsConfig -> ExferenceHeuristicsConfig)
parseHeuristicAssignment subject source = case words (trim source) of
  [name, weight] -> case filter (named name) heuristicAssignments of
    (_, assign) : _ -> do
      value <- weightOf (subject ++ " " ++ name) weight
      Right $ assign value
    [] -> Left $ subject ++ " name must be one of "
      ++ intercalate ", " heuristicNames
  _ -> Left $ subject ++ " expects NAME VALUE, for example goalVar 4.0"
 where
  named wanted (name, _) = normalize wanted == normalize name
  weightOf owner weight = case readMaybe weight of
    Just value
      | value >= 0
      , not $ isNaN value || isInfinite value -> Right $ Penalty value
    _ -> Left $ owner ++ " must be a finite non-negative number"

-- | Every heuristic weight name, for completion and diagnostics.
heuristicNames :: [String]
heuristicNames = map fst heuristicAssignments

-- | Every heuristic weight as @name=value@ in declaration order: the whole
-- configuration is one setting, so its report shows every weight.
renderHeuristics :: ExferenceHeuristicsConfig -> String
renderHeuristics config = unwords
  [ name ++ "=" ++ show weight | (name, weight) <- heuristicFields config ]

-- | The setting spelling of a selection mode, as accepted by
-- 'parseSelectionMode'; a lookahead mode renders as @best-lookahead=N@,
-- which that parser does not accept.
selectionModeName :: SelectionMode -> String
selectionModeName SelectFirst = "first"
selectionModeName SelectBest = "best"
selectionModeName (SelectBestLookahead count) = "best-lookahead=" ++ show count
selectionModeName SelectAll = "all"

-- | The setting spelling of a search strategy, as accepted by
-- 'parseSearchStrategy'.
searchStrategyName :: Strategy -> String
searchStrategyName DepthFirst = "depth-first"
searchStrategyName Interleave = "interleave"

-- | Every search-strategy spelling, for value completion.
searchStrategyNames :: [String]
searchStrategyNames = map searchStrategyName [DepthFirst, Interleave]

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
presentDjinn options fieldSelectors =
  replayCommandOutput . prepareDjinnPresentation options fieldSelectors

-- | Select and render one Djinn result without touching process handles.
-- Fully forcing this plan performs exactly the work demanded by the existing
-- selection policy and makes it safe to move that work to a search worker.
prepareDjinnPresentation
  :: PresentationOptions
  -> FieldSelectors
  -> DjinnResult
  -> CommandOutput
prepareDjinnPresentation options fieldSelectors result = case
    traverse (renderDjinn options) candidates of
  Left failure -> prepareRenderFailure "DJEX_DJINN_RENDER" failure
  Right rendered -> successfulPresentation
    (candidateOutputEvents rendered)
    $ map diagnosticOutputEvent
      $ djinnOutcomeDiagnostics evidence progress
        ++ maybe [] pure (progressTruncationDiagnostic progress)
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
presentExference options fieldSelectors results = replayCommandOutput
  $ prepareExferencePresentation options fieldSelectors results

-- | Check typed Exference candidates effectfully before selecting, rendering,
-- and reporting them through the established presentation policy.
--
-- The admission action runs exactly once for every candidate the policy
-- inspects. Rejected candidates cannot participate in ranking. 'SelectAll'
-- retains the existing one-pass output behavior instead of materializing the
-- complete admitted trace.
presentAssessedExference
  :: PresentationOptions
  -> FieldSelectors
  -> (ExferenceTypedCandidate -> IO Bool)
  -> [ExferenceTypedResult]
  -> IO ExitCode
presentAssessedExference options fieldSelectors admit results
  | presentationSelection options == SelectAll =
      presentAllAssessedExference options fieldSelectors admit results
presentAssessedExference options fieldSelectors admit results = do
  selected <- case presentationSelection options of
    SelectFirst
      | not $ Map.null fieldSelectors -> selectQueryResultsM
          (SelectBestLookahead simplificationLookahead)
          (expressionSize . functionClauseExpression
            . projectFieldSelectors fieldSelectors . candidateOutput
            . typedCandidateCompatibility)
          admit
          results
    mode -> selectQueryResultsM mode
      (exferenceCandidateComplexity . exferenceCandidateMetrics
        . typedCandidateCompatibility)
      admit
      results
  presentExferenceSelection options fieldSelectors
    $ fmap typedCandidateCompatibility selected

presentExferenceSelection
  :: PresentationOptions
  -> FieldSelectors
  -> Selection ExferenceCandidate
  -> IO ExitCode
presentExferenceSelection options fieldSelectors =
  replayCommandOutput . prepareExferenceSelection options fieldSelectors

-- | Select and render a non-streaming Exference result sequence without
-- touching process handles.  The caller must retain the streaming presenter
-- for 'SelectAll'.
prepareExferencePresentation
  :: PresentationOptions
  -> FieldSelectors
  -> [ExferenceResult]
  -> CommandOutput
prepareExferencePresentation options fieldSelectors results =
  prepareExferenceSelection options fieldSelectors selection
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

prepareExferenceSelection
  :: PresentationOptions
  -> FieldSelectors
  -> Selection ExferenceCandidate
  -> CommandOutput
prepareExferenceSelection options fieldSelectors selection = case traverse
    (renderExferenceBlock options) candidates of
  Left failure -> prepareRenderFailure "DJEX_EXF_RENDER" failure
  Right rendered -> successfulPresentation
    (candidateOutputEvents rendered)
    $ map diagnosticOutputEvent
      $ (if null candidates then [noExferenceResultDiagnostic progress] else [])
        ++ maybe [] pure (progressTruncationDiagnostic progress)
 where
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
      unless foundAny $ reportNoExferenceResult progress
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

presentAllAssessedExference
  :: PresentationOptions
  -> FieldSelectors
  -> (ExferenceTypedCandidate -> IO Bool)
  -> [ExferenceTypedResult]
  -> IO ExitCode
presentAllAssessedExference options fieldSelectors admit results = do
  outcome <- runExceptT $ foldAllQueryResultsM
    (const True) printOne False results
  case outcome of
    Left failure -> renderFailure "DJEX_EXF_RENDER" failure
    Right (progress, foundAny) -> do
      unless foundAny $ reportNoExferenceResult progress
      reportTruncation progress
      pure ExitSuccess
 where
  printOne printed typedCandidate = do
    admitted <- liftIO $ admit typedCandidate
    if not admitted
      then pure printed
      else do
        rendered <- ExceptT $ pure $ renderExferenceBlock options
          $ fmap (projectFieldSelectors fieldSelectors)
          $ typedCandidateCompatibility typedCandidate
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

djinnOutcomeDiagnostics :: QueryEvidence -> Maybe Progress -> [Diagnostic]
djinnOutcomeDiagnostics ValidatedCandidates _ = []
djinnOutcomeDiagnostics ProvedUninhabitable _ =
  [ codedDiagnostic Info "DJEX_DJINN_UNINHABITABLE"
      "Djinn proved that the requested type has no inhabitant"
  ]
djinnOutcomeDiagnostics RequiresTargetReference _ =
  [ codedDiagnostic Info "DJEX_DJINN_TARGET_REFERENCE"
      "Djinn found no safe inhabitant without referring to the generated target"
  ]
djinnOutcomeDiagnostics NoEvidence progress =
  [ contextualDiagnostic Info "DJEX_DJINN_UNDECIDED"
      "Djinn established no inhabitation result"
      (maybe "no search batch" show progress)
  ]

reportNoExferenceResult :: Maybe Progress -> IO ()
reportNoExferenceResult = emitDiagnostic . noExferenceResultDiagnostic

noExferenceResultDiagnostic :: Maybe Progress -> Diagnostic
noExferenceResultDiagnostic progress = contextualDiagnostic
  Info "DJEX_EXF_NO_RESULT" message $ maybe "no search batch" show progress
 where
  message = case observeProgress progress of
    ObservedFinished ->
      "Exference exhausted its configured search without finding a candidate"
    _ -> "Exference found no candidate in the inspected search"

reportTruncation :: Maybe Progress -> IO ()
reportTruncation = mapM_ emitDiagnostic . progressTruncationDiagnostic

candidateOutputEvents :: [String] -> [CommandOutputEvent]
candidateOutputEvents [] = []
candidateOutputEvents rendered =
  [CommandStandardOutputLine $ intercalate "\n\n-- or\n\n" rendered]

diagnosticOutputEvent :: Diagnostic -> CommandOutputEvent
diagnosticOutputEvent = CommandStandardErrorLine . renderDiagnostic

successfulPresentation
  :: [CommandOutputEvent]
  -> [CommandOutputEvent]
  -> CommandOutput
successfulPresentation output diagnostics =
  CommandOutput (output ++ diagnostics) ExitSuccess

-- | Prepare one ordinary checked-query failure without touching stderr.
prepareDiagnosticFailure :: Diagnostic -> CommandOutput
prepareDiagnosticFailure failure =
  CommandOutput [diagnosticOutputEvent failure] runtimeFailure

prepareRenderFailure :: Show failure => String -> failure -> CommandOutput
prepareRenderFailure code failure = prepareDiagnosticFailure
  $ contextualDiagnostic Error code
      "cannot present the checked search result" $ show failure

-- | An optional per-query wall-clock budget in whole seconds.
-- 'noQueryTimeout' runs a query to completion, as every release before this
-- setting did.
newtype QueryTimeout = QueryTimeout (Maybe Int)
  deriving (Eq, Show)

-- | Search without a wall-clock budget.
noQueryTimeout :: QueryTimeout
noQueryTimeout = QueryTimeout Nothing

-- | Parse a wall-clock query budget in whole seconds with a caller-owned
-- option name; @0@ is no budget.  A value whose microsecond form would not
-- fit in an 'Int' is rejected rather than silently wrapped.
parseQueryTimeout :: String -> String -> Either String QueryTimeout
parseQueryTimeout subject source = case readMaybe $ trim source of
  Just seconds
    | seconds >= 0
    , seconds <= toInteger (maxBound :: Int) `div` microsecondsPerSecond ->
        Right $ QueryTimeout
          $ if seconds == 0 then Nothing else Just $ fromInteger seconds
  _ -> Left $ subject ++ " must be a non-negative whole number of seconds"

-- | The setting spelling of a query budget, as accepted by
-- 'parseQueryTimeout'.
renderQueryTimeout :: QueryTimeout -> String
renderQueryTimeout (QueryTimeout seconds) = maybe "0" show seconds

-- | The validated positive whole-second budget, when one is active.  This
-- projection stays private to frontend policy; checked library requests do
-- not acquire a timeout field.
queryTimeoutSeconds :: QueryTimeout -> Maybe Int
queryTimeoutSeconds (QueryTimeout seconds) = seconds

microsecondsPerSecond :: Integer
microsecondsPerSecond = 1000000

-- | Run one command under a wall-clock budget.  Without a budget the action
-- runs unchanged, so laziness, output interleaving and Ctrl+C behaviour stay
-- exactly what they were before the setting existed.  Both engines produce
-- their results lazily, in different places -- Djinn searches when its result
-- is matched, Exference when the rendered candidates are printed -- so the
-- budget covers the whole command rather than the search function alone.
-- An expired budget abandons the search where it stands and reports a
-- bounded stop; whatever the engines had already printed stays printed, and
-- the query is never reported as answered.
withinQueryTimeout :: QueryTimeout -> IO ExitCode -> IO ExitCode
withinQueryTimeout (QueryTimeout Nothing) action = action
withinQueryTimeout (QueryTimeout (Just seconds)) action = do
  finished <- timeout
    (fromInteger $ toInteger seconds * microsecondsPerSecond) action
  case finished of
    Just code -> pure code
    Nothing -> do
      emitDiagnostic $ queryTimeoutDiagnostic seconds
      pure runtimeFailure

-- | The single timeout diagnostic shared by serial timeout handling and the
-- ordered parent replay of a timed backend pair.
queryTimeoutDiagnostic :: Int -> Diagnostic
queryTimeoutDiagnostic seconds = contextualDiagnostic Error
  "DJEX_SEARCH_TIMEOUT"
  "the search did not finish within the query budget"
  $ "budget " ++ show seconds
    ++ "s; ':set timeout N' chooses another budget and ':set timeout 0'"
    ++ " searches without one"

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
