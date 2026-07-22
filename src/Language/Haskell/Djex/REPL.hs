{-# LANGUAGE LambdaCase #-}

-- | Persistent, GHCi-style access to both Djex synthesis backends.
--
-- One REPL owns independent immutable Djinn and Exference sessions. Backend
-- switching changes only the active query target; it does not reload either
-- engine or pretend their environment and type-variable representations are
-- interchangeable. Exference environment and policy replacement is explicit
-- and transactional, so a failed load leaves the last usable session intact.
module Language.Haskell.Djex.REPL
  ( ReplBackend (..)
  , ReplOptions (..)
  , defaultReplOptions
  , runRepl
  ) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM_, when)
import Data.Char (isSpace, toLower)
import Data.Foldable (toList)
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)
import Data.Void (Void, absurd)
import System.Directory
  ( canonicalizePath
  , getCurrentDirectory
  , setCurrentDirectory
  )
import System.Exit (ExitCode (ExitSuccess))
import System.IO.Error (tryIOError)
import System.Process (callCommand)
import Text.Read (readMaybe)

import Language.Haskell.Djex
import Language.Haskell.Djex.Command
import Language.Haskell.Djex.Exference.HaskellSrc
  ( ExferenceSessionLoadReport (..)
  , defaultExferenceEnvironmentPath
  , exferenceCommandSessionPolicy
  , loadExferenceSessionWithPolicy
  , parseExferenceRequestWithCheckedTarget
  )
import Language.Haskell.Djex.REPL.Command
import Language.Haskell.Djex.REPL.Driver
import qualified Language.Haskell.Exference.Core.Types as ExferenceType
import Paths_djex (version)

-- | Startup configuration for the shared interactive frontend.
data ReplOptions = ReplOptions
  { replInitialBackend :: ReplBackend
    -- ^ Backend selection shown by the first prompt.
  , replEnvironmentPath :: Maybe FilePath
    -- ^ Exference source directory; 'Nothing' uses installed package data.
  , replAllowFix :: Bool
    -- ^ Whether known recursion helpers are retained while sealing Exference.
  , replHistoryFile :: Maybe FilePath
    -- ^ Optional Haskeline history file. 'Nothing' keeps in-session history.
  }
  deriving (Eq, Show)

-- | Responsive interactive defaults. The first admissible candidate is shown
-- unless the user requests global ranking or all candidates with ':set'.
defaultReplOptions :: ReplOptions
defaultReplOptions = ReplOptions
  { replInitialBackend = OneBackend DjinnBackend
  , replEnvironmentPath = Nothing
  , replAllowFix = False
  , replHistoryFile = Nothing
  }

data ReplState = ReplState
  { activeBackends :: ReplBackend
  , djinnRuntimeSession :: DjinnSession
  , exferenceRuntime :: ExferenceRuntime
  , resultTarget :: DefinitionName
  , presentation :: PresentationOptions
  , djinnSearchOptions :: QueryOptions
  , exferenceSearchOptions :: ExferenceOptions
  , promptTemplate :: String
  , lastQuery :: Maybe (ReplBackend, String)
  , scriptStack :: [FilePath]
  }

data ExferenceRuntime = ExferenceRuntime
  { exferenceRuntimePath :: FilePath
  , exferenceRuntimeAllowsFix :: Bool
  , exferenceRuntimeSession :: Maybe ExferenceSession
  , exferenceRuntimeDiagnostics :: [Diagnostic]
  }

data LoadAttempt = LoadAttempt
  { attemptedEnvironmentPath :: FilePath
  , attemptedSession :: Maybe ExferenceSession
  , attemptedDiagnostics :: [Diagnostic]
  }

-- | Start the interactive frontend and return without terminating the host
-- process. EOF and ':quit' are successful exits; individual load, parse,
-- search, and setting failures are reported and leave the REPL running.
runRepl :: ReplOptions -> IO ExitCode
runRepl options = case standardDjinnSession of
  Left failure -> diagnosticFailure failure
  Right djinnSession -> case defaultTarget of
    Left failure -> diagnosticFailure failure
    Right target -> do
      resolvedHistory <- resolveOptionalPath $ replHistoryFile options
      case resolvedHistory of
        Left failure -> diagnosticFailure failure
        Right historyPath -> do
          requestedPath <- maybe defaultExferenceEnvironmentPath pure
            $ replEnvironmentPath options
          attempt <- loadExference requestedPath $ replAllowFix options
          let exference = ExferenceRuntime
                { exferenceRuntimePath = attemptedEnvironmentPath attempt
                , exferenceRuntimeAllowsFix = replAllowFix options
                , exferenceRuntimeSession = attemptedSession attempt
                , exferenceRuntimeDiagnostics = attemptedDiagnostics attempt
                }
              initial = ReplState
                { activeBackends = replInitialBackend options
                , djinnRuntimeSession = djinnSession
                , exferenceRuntime = exference
                , resultTarget = target
                , presentation = PresentationOptions
                    { presentationSelection = SelectFirst
                    , presentationRenderMode = RenderDefinition
                    , presentationQualification = FullyQualified
                    }
                , djinnSearchOptions = defaultQueryOptions
                , exferenceSearchOptions = defaultExferenceOptions
                , promptTemplate = "djex[%b]> "
                , lastQuery = Nothing
                , scriptStack = []
                }
          putStrLn $ "Djex REPL " ++ showVersion version
          putStrLn "Djinn session ready (standard checked environment)."
          case attemptedSession attempt of
            Just _ -> putStrLn $ "Exference environment: "
              ++ attemptedEnvironmentPath attempt
            Nothing -> putStrLn
              "Exference is unavailable; use :load DIR after fixing its environment."
          putStrLn "Type :help for help."
          _ <- runReplDriver historyPath initial renderPrompt
            $ executeSource "<interactive>"
          pure ExitSuccess

defaultTarget :: Either Diagnostic DefinitionName
defaultTarget = do
  name <- either (Left . targetFailure) Right $ parseName "djexResult"
  either (Left . targetFailure) Right $ mkDefinitionName name
 where
  targetFailure failure = shownErrorDiagnostic
    "DJEX_REPL_INTERNAL" "invalid built-in REPL result name" failure

renderPrompt :: ReplState -> String
renderPrompt state = replace "%b" (replBackendName $ activeBackends state)
  $ promptTemplate state

replace :: String -> String -> String -> String
replace needle replacement = go
 where
  go source
    | needle `isPrefixOf` source = replacement ++ go (drop (length needle) source)
    | character : rest <- source = character : go rest
    | otherwise = []

executeSource
  :: FilePath
  -> ReplState
  -> [String]
  -> String
  -> IO (ReplStep ReplState)
executeSource sourceName state history source = case parseReplInput source of
  Left failure -> replFailure "DJEX_REPL_COMMAND" "invalid REPL command" failure
    >> pure (ContinueRepl state)
  Right ReplNoInput -> pure $ ContinueRepl state
  Right (ReplQuery target typeSource) ->
    runQuery sourceName target typeSource state
  Right (ReplCommand command) -> runCommand sourceName history command state

runCommand
  :: FilePath
  -> [String]
  -> ReplCommand
  -> ReplState
  -> IO (ReplStep ReplState)
runCommand sourceName history command state = case command of
  Browse -> browseState state >> continue state
  ChangeBackend Nothing -> do
    putStrLn $ replBackendName $ activeBackends state
    continue state
  ChangeBackend (Just source) -> case parseReplBackend source of
    Left failure -> settingFailure failure >> continue state
    Right selected -> do
      putStrLn $ "Active backend: " ++ replBackendName selected
      continue state {activeBackends = selected}
  ChangeDirectory path -> do
    outcome <- tryIOError $ setCurrentDirectory path
    case outcome of
      Left failure -> ioFailure "cannot change directory" path failure
        >> continue state
      Right () -> getCurrentDirectory >>= putStrLn >> continue state
  CompareBackends typeSource ->
    runQuery sourceName (ExplicitBackends BothBackends) typeSource state
  Help Nothing -> putStr shortHelp >> continue state
  Help (Just name) -> case commandHelp name of
    Left failure -> settingFailure failure >> continue state
    Right help -> putStr help >> continue state
  History countSource -> do
    showHistory countSource history
    continue state
  InspectDeclaration nameSource -> showInfo state nameSource >> continue state
  LoadEnvironment path -> do
    next <- replaceExferenceEnvironment path
      (exferenceRuntimeAllowsFix $ exferenceRuntime state) state
    continue next
  Quit -> pure $ ExitRepl state
  ReloadEnvironment -> do
    let runtime = exferenceRuntime state
    next <- replaceExferenceEnvironment
      (exferenceRuntimePath runtime)
      (exferenceRuntimeAllowsFix runtime)
      state
    continue next
  RepeatQuery -> case lastQuery state of
    Nothing -> replFailure "DJEX_REPL_HISTORY" "no query to repeat"
        "run a type query before using :" >> continue state
    Just (selected, typeSource) ->
      runResolvedQuery sourceName selected typeSource state
  RunScript path -> runScript path state
  RunShell shellCommand -> do
    outcome <- tryIOError $ callCommand shellCommand
    case outcome of
      Left failure -> ioFailure "shell command failed" shellCommand failure
      Right () -> pure ()
    continue state
  SetOption source -> setOption source state >>= continue
  ShowState subject -> showState subject state >> continue state
  UnsetOption source -> unsetOption source state >>= continue
  Version -> putStrLn ("djex version " ++ showVersion version) >> continue state
 where
  continue = pure . ContinueRepl

runQuery
  :: FilePath
  -> ReplQueryTarget
  -> String
  -> ReplState
  -> IO (ReplStep ReplState)
runQuery sourceName target typeSource state =
  runResolvedQuery sourceName selected typeSource state
 where
  selected = case target of
    ActiveBackends -> activeBackends state
    ExplicitBackends backends -> backends

runResolvedQuery
  :: FilePath
  -> ReplBackend
  -> String
  -> ReplState
  -> IO (ReplStep ReplState)
runResolvedQuery sourceName selected typeSource state = do
  case selected of
    OneBackend selectedBackend -> runBackend False selectedBackend
    BothBackends -> do
      runBackend True DjinnBackend
      runBackend True ExferenceBackend
  pure $ ContinueRepl state
    { lastQuery = Just (selected, typeSource) }
 where
  runBackend labelled selectedBackend = do
    when labelled $ putStrLn
      $ "-- " ++ backendName (backendInfo selectedBackend)
    case selectedBackend of
      DjinnBackend -> runDjinnInteractive sourceName typeSource state
      ExferenceBackend -> runExferenceInteractive sourceName typeSource state

runDjinnInteractive :: FilePath -> String -> ReplState -> IO ()
runDjinnInteractive sourceName typeSource state = case
    parseDjinnRequestWithCheckedTarget
      (djinnRuntimeSession state)
      queryOptions
      (resultTarget state)
      sourceName
      typeSource of
  Left failure -> emitDiagnostic failure
  Right request -> case runDjinnQuery (djinnRuntimeSession state) request of
    Left failure -> emitDiagnostic failure
    Right result -> ignoreExit $ presentDjinn (presentation state) result
 where
  queryOptions = (djinnSearchOptions state)
    { optionAlternatives = presentationSelection (presentation state)
        /= SelectFirst
    , optionSorted = False
    }

runExferenceInteractive :: FilePath -> String -> ReplState -> IO ()
runExferenceInteractive sourceName typeSource state = case
    exferenceRuntimeSession $ exferenceRuntime state of
  Nothing -> replFailure "DJEX_REPL_EXFERENCE_UNAVAILABLE"
    "Exference has no loaded environment"
    $ "use :load DIR; last attempted path: "
      ++ exferenceRuntimePath (exferenceRuntime state)
  Just session -> case parseExferenceRequestWithCheckedTarget
      session
      (exferenceSearchOptions state)
      (resultTarget state)
      sourceName
      typeSource of
    Left failure -> emitDiagnostic failure
    Right request -> case runExferenceQuery session request of
      Left failure -> emitDiagnostic failure
      Right results -> ignoreExit $ presentExference (presentation state) results

ignoreExit :: IO ExitCode -> IO ()
ignoreExit action = action >> pure ()

loadExference :: FilePath -> Bool -> IO LoadAttempt
loadExference requestedPath allowFix = do
  resolved <- tryIOError $ canonicalizePath requestedPath
  case resolved of
    Left failure -> do
      let loadFailure = ioDiagnostic
            "cannot resolve Exference environment" requestedPath failure
      emitDiagnostic loadFailure
      pure LoadAttempt
        { attemptedEnvironmentPath = requestedPath
        , attemptedSession = Nothing
        , attemptedDiagnostics = [loadFailure]
        }
    Right path -> case exferenceCommandSessionPolicy allowFix of
      Left failure -> do
        emitDiagnostic failure
        pure LoadAttempt
          { attemptedEnvironmentPath = path
          , attemptedSession = Nothing
          , attemptedDiagnostics = [failure]
          }
      Right policy -> do
        report <- loadExferenceSessionWithPolicy policy path
        let advisory = exferenceSessionLoadDiagnostics report
            fatal = either toList (const [])
              $ exferenceSessionLoadResult report
            diagnostics = advisory ++ fatal
        mapM_ emitDiagnostic $ filter ((/= Info) . diagnosticSeverity) advisory
        mapM_ emitDiagnostic fatal
        pure LoadAttempt
          { attemptedEnvironmentPath = path
          , attemptedSession = either (const Nothing) Just
              $ exferenceSessionLoadResult report
          , attemptedDiagnostics = diagnostics
          }

replaceExferenceEnvironment
  :: FilePath
  -> Bool
  -> ReplState
  -> IO ReplState
replaceExferenceEnvironment path allowFix state = do
  attempt <- loadExference path allowFix
  case attemptedSession attempt of
    Nothing -> do
      putStrLn
        "Exference load failed; retaining the previous session and settings."
      pure state
        { exferenceRuntime = (exferenceRuntime state)
            { exferenceRuntimeDiagnostics = attemptedDiagnostics attempt }
        }
    Just session -> do
      putStrLn $ "Loaded Exference environment: "
        ++ attemptedEnvironmentPath attempt
      pure state
        { exferenceRuntime = ExferenceRuntime
            { exferenceRuntimePath = attemptedEnvironmentPath attempt
            , exferenceRuntimeAllowsFix = allowFix
            , exferenceRuntimeSession = Just session
            , exferenceRuntimeDiagnostics = attemptedDiagnostics attempt
            }
        }

setOption :: String -> ReplState -> IO ReplState
setOption source state
  | null $ trim source = showSettings state >> pure state
  | otherwise = case settingInvocation source of
      Left failure -> settingFailure failure >> pure state
      Right (name, value) -> applySetting name value state

applySetting :: String -> Maybe String -> ReplState -> IO ReplState
applySetting name value state = case name of
  "backend" -> withValue "backend" value $ \source ->
    case parseReplBackend source of
      Left failure -> reject failure
      Right selected -> pure state {activeBackends = selected}
  "target" -> withValue "target" value $ \source -> case checkedTarget source of
    Left failure -> reject failure
    Right target -> pure state {resultTarget = target}
  "select" -> withValue "select" value $ \source -> case parseSelection source of
    Left failure -> reject failure
    Right selected -> pure state
      { presentation = (presentation state)
          { presentationSelection = selected }
      }
  "render" -> withValue "render" value $ \source -> case parseRenderMode source of
    Left failure -> reject failure
    Right mode -> pure state
      { presentation = (presentation state)
          { presentationRenderMode = mode }
      }
  "qualification" -> withValue "qualification" value $ \source ->
    case parseQualification source of
      Left failure -> reject failure
      Right qualification -> pure state
        { presentation = (presentation state)
            { presentationQualification = qualification }
        }
  "prompt" -> withValue "prompt" value $ \source -> pure state
    { promptTemplate = decodeString source }
  "candidate-limit" -> withValue "candidate-limit" value $ \source ->
    case positiveInt "candidate-limit" source of
      Left failure -> reject failure
      Right limit -> pure state
        { djinnSearchOptions = (djinnSearchOptions state)
            { optionCutoff = limit }
        }
  "choice-budget" -> withValue "choice-budget" value $ \source ->
    case nonNegativeInteger "choice-budget" source of
      Left failure -> reject failure
      Right budget -> pure state
        { djinnSearchOptions = (djinnSearchOptions state)
            { optionBudget = if budget == 0 then Nothing else Just budget }
        }
  "allow-unused" -> setExferenceBool name value state $ \enabled options ->
    options {exferenceAllowUnused = enabled}
  "allow-constraints" -> setExferenceBool name value state $ \enabled options ->
    options {exferenceAllowResidualConstraints = enabled}
  "multi-constructor-patterns" ->
    setExferenceBool name value state $ \enabled options ->
      options {exferenceMultiConstructorPatterns = enabled}
  "constraint-deferral-steps" -> withValue name value $ \source ->
    case nonNegativeInt name source of
      Left failure -> reject failure
      Right count -> pure state
        { exferenceSearchOptions = (exferenceSearchOptions state)
            { exferenceConstraintDeferralSteps = count }
        }
  "max-steps" -> withValue name value $ \source ->
    case positiveInt name source of
      Left failure -> reject failure
      Right count -> pure state
        { exferenceSearchOptions = (exferenceSearchOptions state)
            { exferenceMaximumSteps = count }
        }
  "max-queue" -> withValue name value $ \source ->
    case boundedNonNegativeInt name source of
      Left failure -> reject failure
      Right count -> pure state
        { exferenceSearchOptions = (exferenceSearchOptions state)
            { exferenceMaximumQueueSize = count }
        }
  "max-depth" -> withValue name value $ \source ->
    case boundedPenalty name source of
      Left failure -> reject failure
      Right depth -> pure state
        { exferenceSearchOptions = (exferenceSearchOptions state)
            { exferenceMaximumDepth = depth }
        }
  "fix" -> case parseBoolean name value of
    Left failure -> reject failure
    Right allowFix
      | allowFix == exferenceRuntimeAllowsFix (exferenceRuntime state) ->
          pure state
      | otherwise -> replaceExferenceEnvironment
          (exferenceRuntimePath $ exferenceRuntime state) allowFix state
  _ -> reject $ "unknown setting " ++ show name
 where
  reject failure = settingFailure failure >> pure state
  withValue setting supplied action = case supplied of
    Nothing -> reject $ "setting " ++ setting ++ " requires a value"
    Just suppliedValue -> action suppliedValue

setExferenceBool
  :: String
  -> Maybe String
  -> ReplState
  -> (Bool -> ExferenceOptions -> ExferenceOptions)
  -> IO ReplState
setExferenceBool name value state update = case parseBoolean name value of
  Left failure -> settingFailure failure >> pure state
  Right enabled -> pure state
    { exferenceSearchOptions = update enabled $ exferenceSearchOptions state }

unsetOption :: String -> ReplState -> IO ReplState
unsetOption source state = case words $ map toLower $ trim source of
  [name] -> reset name
  [] -> settingFailure "expected a setting name" >> pure state
  _ -> settingFailure "expected exactly one setting name" >> pure state
 where
  reset name = case name of
    "backend" -> pure state
      {activeBackends = replInitialBackend defaultReplOptions}
    "target" -> case defaultTarget of
      Left failure -> emitDiagnostic failure >> pure state
      Right target -> pure state {resultTarget = target}
    "select" -> pure state
      { presentation = (presentation state)
          { presentationSelection = SelectFirst }
      }
    "render" -> pure state
      { presentation = (presentation state)
          { presentationRenderMode = RenderDefinition }
      }
    "qualification" -> pure state
      { presentation = (presentation state)
          { presentationQualification = FullyQualified }
      }
    "prompt" -> pure state {promptTemplate = "djex[%b]> "}
    "candidate-limit" -> pure state
      { djinnSearchOptions = (djinnSearchOptions state)
          { optionCutoff = optionCutoff defaultQueryOptions }
      }
    "choice-budget" -> pure state
      { djinnSearchOptions = (djinnSearchOptions state)
          { optionBudget = optionBudget defaultQueryOptions }
      }
    "allow-unused" -> resetExference $ \defaults options -> options
      {exferenceAllowUnused = exferenceAllowUnused defaults}
    "allow-constraints" -> resetExference $ \defaults options -> options
      { exferenceAllowResidualConstraints =
          exferenceAllowResidualConstraints defaults }
    "multi-constructor-patterns" -> resetExference $ \defaults options -> options
      { exferenceMultiConstructorPatterns =
          exferenceMultiConstructorPatterns defaults }
    "constraint-deferral-steps" -> resetExference $ \defaults options -> options
      { exferenceConstraintDeferralSteps =
          exferenceConstraintDeferralSteps defaults }
    "max-steps" -> resetExference $ \defaults options -> options
      {exferenceMaximumSteps = exferenceMaximumSteps defaults}
    "max-queue" -> resetExference $ \defaults options -> options
      {exferenceMaximumQueueSize = exferenceMaximumQueueSize defaults}
    "max-depth" -> resetExference $ \defaults options -> options
      {exferenceMaximumDepth = exferenceMaximumDepth defaults}
    "fix"
      | exferenceRuntimeAllowsFix (exferenceRuntime state) ->
          replaceExferenceEnvironment
            (exferenceRuntimePath $ exferenceRuntime state) False state
      | otherwise -> pure state
    _ -> settingFailure ("unknown setting " ++ show name) >> pure state

  resetExference update = pure state
    { exferenceSearchOptions = update defaultExferenceOptions
        $ exferenceSearchOptions state }

settingInvocation :: String -> Either String (String, Maybe String)
settingInvocation source = case trim source of
  sign : rest
    | sign `elem` "+-"
    , not (null rest)
    , all (not . isSpace) rest ->
        Right (map toLower rest, Just $ if sign == '+' then "on" else "off")
  value -> case break (== '=') value of
    (name, '=' : settingValue) -> checked name $ Just $ trim settingValue
    _ -> case words value of
      [] -> Left "expected a setting name"
      name : _ -> checked name $ optionalRemainder name value
 where
  checked rawName value
    | null normalized = Left "expected a setting name"
    | otherwise = Right (normalized, value)
   where
    normalized = map toLower $ trim rawName

  optionalRemainder name value = case trim $ drop (length name) value of
    "" -> Nothing
    remainder -> Just remainder

parseBoolean :: String -> Maybe String -> Either String Bool
parseBoolean name source = case fmap (map toLower . trim) source of
  Just "on" -> Right True
  Just "true" -> Right True
  Just "yes" -> Right True
  Just "off" -> Right False
  Just "false" -> Right False
  Just "no" -> Right False
  Nothing -> Left $ "setting " ++ name ++ " requires on or off"
  _ -> Left $ "setting " ++ name ++ " must be on or off"

checkedTarget :: String -> Either String DefinitionName
checkedTarget source = do
  name <- either (Left . renderNameError) Right $ parseName $ trim source
  either (Left . show) Right $ mkDefinitionName name

parseSelection :: String -> Either String SelectionMode
parseSelection source = case map toLower $ trim source of
  "first" -> Right SelectFirst
  "best" -> Right SelectBest
  "all" -> Right SelectAll
  _ -> Left "select must be first, best, or all"

parseRenderMode :: String -> Either String RenderMode
parseRenderMode source = case map toLower $ trim source of
  "definition" -> Right RenderDefinition
  "expression" -> Right RenderExpression
  _ -> Left "render must be definition or expression"

parseQualification :: String -> Either String Qualification
parseQualification source = case map toLower $ trim source of
  "none" -> Right Unqualified
  "identifiers" -> Right QualifyIdentifiers
  "full" -> Right FullyQualified
  _ -> Left "qualification must be none, identifiers, or full"

positiveInt :: String -> String -> Either String Int
positiveInt name = checkedInt 1 $ name ++ " must be a positive integer"

nonNegativeInt :: String -> String -> Either String Int
nonNegativeInt name = checkedInt 0 $ name ++ " must be a non-negative integer"

checkedInt :: Integer -> String -> String -> Either String Int
checkedInt lowerBound failure source = case readMaybe source :: Maybe Integer of
  Just value
    | value >= lowerBound
    , value <= toInteger (maxBound :: Int) -> Right $ fromInteger value
  _ -> Left failure

nonNegativeInteger :: String -> String -> Either String Integer
nonNegativeInteger name source = case readMaybe source of
  Just value | value >= 0 -> Right value
  _ -> Left $ name ++ " must be a non-negative integer"

boundedNonNegativeInt :: String -> String -> Either String (Maybe Int)
boundedNonNegativeInt _ source | map toLower (trim source) == "unbounded" =
  Right Nothing
boundedNonNegativeInt name source = Just <$> nonNegativeInt name source

boundedPenalty :: String -> String -> Either String (Maybe Penalty)
boundedPenalty _ source | map toLower (trim source) == "unbounded" = Right Nothing
boundedPenalty name source = case readMaybe source of
  Just value
    | value >= 0
    , not $ isNaN value || isInfinite value -> Right $ Just $ Penalty value
  _ -> Left $ name ++ " must be a finite non-negative number or unbounded"

showState :: Maybe String -> ReplState -> IO ()
showState Nothing = showSettings
showState (Just rawSubject) = case words $ map toLower $ trim rawSubject of
  ["settings"] -> showSettings
  ["backends"] -> showBackends
  ["environment"] -> showEnvironmentSummary
  ["omissions"] -> showOmissions
  ["diagnostics"] -> showLoadDiagnostics
  ["directory"] -> const $ getCurrentDirectory >>= putStrLn
  [] -> showSettings
  _ -> \_ -> settingFailure $ "unknown :show subject " ++ show rawSubject

showSettings :: ReplState -> IO ()
showSettings state = putStr $ unlines
  [ "backend = " ++ replBackendName (activeBackends state)
  , "target = " ++ definitionSpelling (resultTarget state)
  , "select = " ++ selectionName (presentationSelection $ presentation state)
  , "render = " ++ renderModeName
      (presentationRenderMode $ presentation state)
  , "qualification = " ++ qualificationName
      (presentationQualification $ presentation state)
  , "prompt = " ++ show (promptTemplate state)
  , "candidate-limit = " ++ show (optionCutoff $ djinnSearchOptions state)
  , "choice-budget = " ++ maybe "0" show
      (optionBudget $ djinnSearchOptions state)
  , "allow-unused = " ++ booleanName
      (exferenceAllowUnused $ exferenceSearchOptions state)
  , "allow-constraints = " ++ booleanName
      (exferenceAllowResidualConstraints $ exferenceSearchOptions state)
  , "constraint-deferral-steps = " ++ show
      (exferenceConstraintDeferralSteps $ exferenceSearchOptions state)
  , "multi-constructor-patterns = " ++ booleanName
      (exferenceMultiConstructorPatterns $ exferenceSearchOptions state)
  , "max-steps = " ++ show
      (exferenceMaximumSteps $ exferenceSearchOptions state)
  , "max-queue = " ++ renderBounded
      (exferenceMaximumQueueSize $ exferenceSearchOptions state)
  , "max-depth = " ++ renderBounded
      (exferenceMaximumDepth $ exferenceSearchOptions state)
  , "fix = " ++ booleanName
      (exferenceRuntimeAllowsFix $ exferenceRuntime state)
  , "environment = " ++ exferenceRuntimePath (exferenceRuntime state)
  ]

selectionName :: SelectionMode -> String
selectionName SelectFirst = "first"
selectionName SelectBest = "best"
selectionName (SelectBestLookahead count) = "best-lookahead=" ++ show count
selectionName SelectAll = "all"

renderModeName :: RenderMode -> String
renderModeName RenderDefinition = "definition"
renderModeName RenderExpression = "expression"

qualificationName :: Qualification -> String
qualificationName Unqualified = "none"
qualificationName QualifyIdentifiers = "identifiers"
qualificationName FullyQualified = "full"

booleanName :: Bool -> String
booleanName True = "on"
booleanName False = "off"

renderBounded :: Show value => Maybe value -> String
renderBounded = maybe "unbounded" show

showBackends :: ReplState -> IO ()
showBackends state = forM_ availableBackends $ \information ->
  putStrLn $ marker information ++ backendName information ++ " ("
    ++ intercalate ", " (map show $ backendCapabilities information) ++ ")"
 where
  marker information
    | backendSelected (backend information) $ activeBackends state = "* "
    | otherwise = "  "

backendSelected :: Backend -> ReplBackend -> Bool
backendSelected selectedBackend (OneBackend selected) =
  selectedBackend == selected
backendSelected _ BothBackends = True

showEnvironmentSummary :: ReplState -> IO ()
showEnvironmentSummary state = do
  putStrLn $ "Djinn: " ++ declarationCount
      (djinnSessionEnvironment $ djinnRuntimeSession state)
    ++ " declarations (standard checked environment)"
  let runtime = exferenceRuntime state
  putStrLn $ "Exference: " ++ case exferenceRuntimeSession runtime of
    Nothing -> "unavailable (last attempted " ++ exferenceRuntimePath runtime
      ++ ")"
    Just session -> declarationCount (exferenceSessionEnvironment session)
      ++ " declarations from " ++ exferenceRuntimePath runtime
 where
  declarationCount = show . length . environmentDeclarations

showOmissions :: ReplState -> IO ()
showOmissions state = case exferenceRuntimeSession $ exferenceRuntime state of
  Nothing -> putStrLn "Exference is unavailable."
  Just session -> case exferenceSessionOmissions session of
    [] -> putStrLn "No Exference capabilities were omitted."
    omissions -> forM_ omissions $ \omission -> putStrLn
      $ renderCanonical (omittedName omission) ++ ": "
        ++ show (omittedCapability omission) ++ " ("
        ++ show (omittedReason omission) ++ ")"

showLoadDiagnostics :: ReplState -> IO ()
showLoadDiagnostics state = case
    exferenceRuntimeDiagnostics $ exferenceRuntime state of
  [] -> putStrLn "No Exference load diagnostics."
  diagnostics -> mapM_ (putStrLn . renderDiagnostic) diagnostics

browseState :: ReplState -> IO ()
browseState state = forSelectedBackends state $ \selectedBackend ->
  case selectedBackend of
    DjinnBackend -> browse "Djinn" id
      $ djinnSessionEnvironment $ djinnRuntimeSession state
    ExferenceBackend -> case exferenceRuntimeSession
        $ exferenceRuntime state of
      Nothing -> putStrLn "Exference is unavailable."
      Just session -> browse "Exference" ExferenceType.defaultVariableName
        $ exferenceSessionEnvironment session

showInfo :: ReplState -> String -> IO ()
showInfo state source = case parseName $ trim source of
  Left failure -> settingFailure (renderNameError failure)
  Right name -> forSelectedBackends state $ \selectedBackend ->
    case selectedBackend of
      DjinnBackend -> info "Djinn" id name
        $ djinnSessionEnvironment $ djinnRuntimeSession state
      ExferenceBackend -> case exferenceRuntimeSession
          $ exferenceRuntime state of
        Nothing -> putStrLn "Exference is unavailable."
        Just session -> info
          "Exference" ExferenceType.defaultVariableName name
          $ exferenceSessionEnvironment session

forSelectedBackends :: ReplState -> (Backend -> IO ()) -> IO ()
forSelectedBackends state action = case activeBackends state of
  OneBackend selectedBackend -> action selectedBackend
  BothBackends -> action DjinnBackend >> action ExferenceBackend

browse
  :: String
  -> (variable -> String)
  -> Environment variable Void ()
  -> IO ()
browse label variableName environment = do
  putStrLn $ "-- " ++ label
  case environmentDeclarations environment of
    [] -> putStrLn "(no declarations)"
    declarations -> mapM_ (putStrLn . renderDeclaration variableName)
      declarations

info
  :: String
  -> (variable -> String)
  -> Name
  -> Environment variable Void ()
  -> IO ()
info label variableName name environment = do
  putStrLn $ "-- " ++ label
  case filter (declarationDefines name)
      $ environmentDeclarations environment of
    [] -> putStrLn $ "No declaration for " ++ renderCanonical name
    declarations -> mapM_ (putStrLn . renderDeclaration variableName)
      declarations

-- Constructors and class methods are usable search names even though the
-- neutral environment indexes them beneath their owning declaration.
declarationDefines
  :: Name
  -> Declaration variable kind annotation
  -> Bool
declarationDefines name declaration =
  declarationSubjectName declaration == name || case declaration of
    DataTypeDeclaration _ _ _ constructors ->
      any ((== name) . constructorName) constructors
    ClassDeclaration _ _ _ _ methods ->
      any ((== name) . valueName) methods
    _ -> False

renderDeclaration
  :: (variable -> String)
  -> Declaration variable Void ()
  -> String
renderDeclaration variableName declaration = case declaration of
  TypeSynonymDeclaration _ name parameters body ->
    "type " ++ headWithParameters name parameters ++ " = "
      ++ renderSharedType body
  DataTypeDeclaration _ name parameters constructors ->
    "data " ++ headWithParameters name parameters ++ case constructors of
      [] -> ""
      _ -> " = " ++ intercalate " | " (map renderConstructor constructors)
  AbstractTypeDeclaration _ name kind ->
    "type " ++ renderCanonical name ++ " :: " ++ renderKind kind
  ValueDeclaration signature -> renderSignature signature
  ClassDeclaration _ name parameters superclasses methods ->
    "class " ++ contextPrefix superclasses
      ++ headWithParameters name parameters ++ case methods of
        [] -> ""
        _ -> " where " ++ intercalate "; " (map renderSignature methods)
  InstanceDeclaration _ _ prerequisites headConstraint ->
    "instance " ++ contextPrefix prerequisites
      ++ renderSharedConstraint headConstraint
 where
  renderSharedType = renderTypeWithQualification FullyQualified variableName
  renderSharedConstraint = renderConstraintWithQualification
    FullyQualified variableName
  renderParameter parameter = case parameterKind parameter of
    Nothing -> variableName $ parameterVariable parameter
    Just kind -> "(" ++ variableName (parameterVariable parameter)
      ++ " :: " ++ renderKind kind ++ ")"
  headWithParameters name parameters = unwords
    $ renderCanonical name : map renderParameter parameters
  renderConstructor constructor = unwords
    $ renderCanonical (constructorName constructor)
      : map renderSharedType (constructorFields constructor)
  renderSignature signature = renderCanonical (valueName signature)
    ++ " :: " ++ renderSharedType (valueType signature)
  contextPrefix [] = ""
  contextPrefix constraints = case constraints of
    [constraint] -> renderSharedConstraint constraint ++ " => "
    _ -> "(" ++ intercalate ", "
      (map renderSharedConstraint constraints) ++ ") => "

renderKind :: Kind Void -> String
renderKind kind = case kind of
  ProperTypeKind -> "Type"
  KindVariable impossible -> absurd impossible
  FunctionKind parameter result -> renderKindParameter parameter
    ++ " -> " ++ renderKind result
 where
  renderKindParameter parameter@(FunctionKind _ _) =
    "(" ++ renderKind parameter ++ ")"
  renderKindParameter parameter = renderKind parameter

showHistory :: Maybe String -> [String] -> IO ()
showHistory countSource history = case traverse parseCount countSource of
  Left failure -> settingFailure failure
  Right maximumCount -> forM_ (zip [firstIndex ..] selected) $ \(index, line) ->
    putStrLn $ show index ++ "  " ++ line
   where
    selected = maybe history (`takeLast` history) maximumCount
    firstIndex = length history - length selected + 1
 where
  parseCount source = case readMaybe source :: Maybe Integer of
    Just count
      | count >= 0
      , count <= toInteger (maxBound :: Int) -> Right $ fromInteger count
    _ -> Left "history count must be a non-negative integer"

takeLast :: Int -> [value] -> [value]
takeLast count = reverse . take count . reverse

runScript :: FilePath -> ReplState -> IO (ReplStep ReplState)
runScript path state = do
  resolved <- tryIOError $ canonicalizePath path
  case resolved of
    Left failure -> ioFailure "cannot resolve script" path failure
      >> pure (ContinueRepl state)
    Right canonical
      | canonical `elem` scriptStack state -> do
          replFailure "DJEX_REPL_SCRIPT_CYCLE" "recursive REPL script"
            $ intercalate " -> " $ reverse $ canonical : scriptStack state
          pure $ ContinueRepl state
      | otherwise -> do
          contents <- strictReadFile canonical
          case contents of
            Left failure -> ioFailure "cannot read script" canonical failure
              >> pure (ContinueRepl state)
            Right source -> case scriptInputs $ lines source of
              Left failure -> replFailure "DJEX_REPL_SCRIPT"
                  "invalid REPL script" (canonical ++ ": " ++ failure)
                >> pure (ContinueRepl state)
              Right inputs -> runInputs canonical
                state {scriptStack = canonical : scriptStack state} inputs

runInputs
  :: FilePath
  -> ReplState
  -> [String]
  -> IO (ReplStep ReplState)
runInputs _ state [] = pure $ ContinueRepl state
  {scriptStack = drop 1 $ scriptStack state}
runInputs sourceName state (source : remaining) = do
  outcome <- executeSource sourceName state [] source
  case outcome of
    ExitRepl final -> pure $ ExitRepl final
      {scriptStack = drop 1 $ scriptStack final}
    ContinueRepl next -> runInputs sourceName next remaining

scriptInputs :: [String] -> Either String [String]
scriptInputs = go []
 where
  go result [] = Right $ reverse result
  go result (line : remaining)
    | trimmed == ":{" = collect result [] remaining
    | trimmed == ":}" = Left "unexpected :}"
    | otherwise = go (line : result) remaining
   where
    trimmed = trim line

  collect _ _ [] = Left "unterminated multiline input (expected :})"
  collect result body (line : remaining)
    | trim line == ":}" = go (unlines (reverse body) : result) remaining
    | otherwise = collect result (line : body) remaining

strictReadFile :: FilePath -> IO (Either IOError String)
strictReadFile path = tryIOError $ do
  source <- readFile path
  evaluate $ force source

resolveOptionalPath
  :: Maybe FilePath
  -> IO (Either Diagnostic (Maybe FilePath))
resolveOptionalPath Nothing = pure $ Right Nothing
resolveOptionalPath (Just path) = do
  resolved <- tryIOError $ canonicalizePath path
  pure $ case resolved of
    Left failure -> Left $ ioDiagnostic
      "cannot resolve REPL history file" path failure
    Right canonical -> Right $ Just canonical

decodeString :: String -> String
decodeString source = fromMaybe source $ readMaybe source

replFailure :: String -> String -> String -> IO ()
replFailure code summary detail = emitDiagnostic
  $ contextualDiagnostic Error code summary detail

settingFailure :: String -> IO ()
settingFailure = replFailure "DJEX_REPL_SETTING" "invalid REPL setting"

ioFailure :: String -> FilePath -> IOError -> IO ()
ioFailure summary path failure = emitDiagnostic
  $ ioDiagnostic summary path failure

ioDiagnostic :: String -> FilePath -> IOError -> Diagnostic
ioDiagnostic summary path failure = contextualDiagnostic
  Error "DJEX_REPL_IO" summary $ path ++ ": " ++ show failure

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
