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
import Control.Exception
  ( AsyncException (UserInterrupt)
  , evaluate
  , handleJust
  )
import Control.Monad (forM_, when)
import Data.Char (isSpace, toLower)
import Data.Foldable (toList)
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)
import Data.Void (Void, absurd)
import System.Directory
  ( Permissions (readable, searchable, writable)
  , canonicalizePath
  , doesDirectoryExist
  , doesPathExist
  , getPermissions
  , getCurrentDirectory
  , setCurrentDirectory
  )
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath (takeDirectory)
import System.IO (IOMode (ReadMode), hGetContents, withFile)
import System.IO.Error (tryIOError)
import System.Process
  ( CreateProcess (delegate_ctlc)
  , shell
  , waitForProcess
  , withCreateProcess
  )
import Text.Read (readMaybe)

import Language.Haskell.Djex
import Language.Haskell.Djex.Command
import Language.Haskell.Djex.Exference.HaskellSrc
  ( ExferenceSessionLoadReport (..)
  , defaultExferenceEnvironmentPath
  , exferenceCommandSessionPolicy
  , loadExferenceSessionWithPolicy
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
                , presentation = defaultInteractivePresentationOptions
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
defaultTarget = case parseResultTarget defaultResultTargetSpelling of
  Left failure -> Left $ contextualDiagnostic Error
    "DJEX_REPL_INTERNAL" "invalid built-in REPL result name" failure
  Right target -> Right target

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
  Left failure -> replFailure "DJEX_REPL_COMMAND" "invalid REPL command"
    (sourceName ++ ": " ++ failure)
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
  RunScript path -> runScript path history state
  RunShell shellCommand -> runShellCommand shellCommand >> continue state
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
runDjinnInteractive sourceName typeSource state = ignoreExit
  $ executeDjinnCommand
      (presentation state)
      (djinnRuntimeSession state)
      (djinnSearchOptions state)
      (resultTarget state)
      sourceName
      typeSource

runExferenceInteractive :: FilePath -> String -> ReplState -> IO ()
runExferenceInteractive sourceName typeSource state = case
    exferenceRuntimeSession $ exferenceRuntime state of
  Nothing -> replFailure "DJEX_REPL_EXFERENCE_UNAVAILABLE"
    "Exference has no loaded environment"
    $ "use :load DIR; last attempted path: "
      ++ exferenceRuntimePath (exferenceRuntime state)
  Just session -> ignoreExit $ executeExferenceCommand
    (presentation state)
    session
    (exferenceSearchOptions state)
    (resultTarget state)
    sourceName
    typeSource

ignoreExit :: IO ExitCode -> IO ()
ignoreExit action = action >> pure ()

data ShellOutcome
  = ShellCompleted ExitCode
  | ShellInterrupted

runShellCommand :: String -> IO ()
runShellCommand command = do
  outcome <- tryIOError $ handleJust onlyUserInterrupt
    (const $ pure ShellInterrupted)
    $ withCreateProcess ((shell command) {delegate_ctlc = True})
      $ \_ _ _ process ->
        ShellCompleted <$> waitForProcess process
  case outcome of
    Left failure -> ioFailure "cannot run shell command" command failure
    Right ShellInterrupted -> putStrLn "Interrupted."
    Right (ShellCompleted ExitSuccess) -> pure ()
    Right (ShellCompleted (ExitFailure status)) -> replFailure
      "DJEX_REPL_SHELL" "shell command failed"
      $ command ++ ": exit status " ++ show status
 where
  onlyUserInterrupt UserInterrupt = Just ()
  onlyUserInterrupt _ = Nothing

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
      Right (setting, value) -> applySetting setting value state

applySetting :: ReplSetting -> Maybe String -> ReplState -> IO ReplState
applySetting setting value state = case setting of
  BackendSetting -> withValue setting $ \source ->
    case parseReplBackend source of
      Left failure -> reject failure
      Right selected -> pure state {activeBackends = selected}
  TargetSetting -> withValue setting $ \source -> case checkedTarget source of
    Left failure -> reject failure
    Right target -> pure state {resultTarget = target}
  SelectionSetting -> withValue setting $ \source ->
    case parseSelectionMode (replSettingName setting) source of
      Left failure -> reject failure
      Right selected -> pure state
        { presentation = (presentation state)
            { presentationSelection = selected }
        }
  RenderingSetting -> withValue setting $ \source ->
    case parseRenderMode (replSettingName setting) source of
      Left failure -> reject failure
      Right mode -> pure state
        { presentation = (presentation state)
            { presentationRenderMode = mode }
        }
  QualificationSetting -> withValue setting $ \source ->
    case parseQualification (replSettingName setting) source of
      Left failure -> reject failure
      Right qualification -> pure state
        { presentation = (presentation state)
            { presentationQualification = qualification }
        }
  PromptSetting -> withValue setting $ \source -> pure state
    {promptTemplate = decodeString source}
  CandidateLimitSetting -> withValue setting $ \source ->
    case positiveInt (replSettingName setting) source of
      Left failure -> reject failure
      Right limit -> pure state
        { djinnSearchOptions = (djinnSearchOptions state)
            { optionCutoff = limit }
        }
  ChoiceBudgetSetting -> withValue setting $ \source ->
    case nonNegativeInteger (replSettingName setting) source of
      Left failure -> reject failure
      Right budget -> pure state
        { djinnSearchOptions = (djinnSearchOptions state)
            { optionBudget = if budget == 0 then Nothing else Just budget }
        }
  AllowUnusedSetting -> setExferenceBool setting value state
    $ \enabled options -> options {exferenceAllowUnused = enabled}
  AllowConstraintsSetting -> setExferenceBool setting value state
    $ \enabled options -> options
        {exferenceAllowResidualConstraints = enabled}
  MultiConstructorPatternsSetting -> setExferenceBool setting value state
    $ \enabled options -> options
        {exferenceMultiConstructorPatterns = enabled}
  ConstraintDeferralStepsSetting -> withValue setting $ \source ->
    case nonNegativeInt (replSettingName setting) source of
      Left failure -> reject failure
      Right count -> pure state
        { exferenceSearchOptions = (exferenceSearchOptions state)
            { exferenceConstraintDeferralSteps = count }
        }
  MaximumStepsSetting -> withValue setting $ \source ->
    case positiveInt (replSettingName setting) source of
      Left failure -> reject failure
      Right count -> pure state
        { exferenceSearchOptions = (exferenceSearchOptions state)
            { exferenceMaximumSteps = count }
        }
  MaximumQueueSetting -> withValue setting $ \source ->
    case boundedNonNegativeInt (replSettingName setting) source of
      Left failure -> reject failure
      Right count -> pure state
        { exferenceSearchOptions = (exferenceSearchOptions state)
            { exferenceMaximumQueueSize = count }
        }
  MaximumDepthSetting -> withValue setting $ \source ->
    case boundedPenalty (replSettingName setting) source of
      Left failure -> reject failure
      Right depth -> pure state
        { exferenceSearchOptions = (exferenceSearchOptions state)
            { exferenceMaximumDepth = depth }
        }
  FixSetting -> case parseBoolean (replSettingName setting) value of
    Left failure -> reject failure
    Right allowFix
      | allowFix == exferenceRuntimeAllowsFix (exferenceRuntime state) ->
          pure state
      | otherwise -> replaceExferenceEnvironment
          (exferenceRuntimePath $ exferenceRuntime state) allowFix state
 where
  reject failure = settingFailure failure >> pure state
  withValue requiredSetting action = case value of
    Nothing -> reject $ "setting " ++ replSettingName requiredSetting
      ++ " requires a value"
    Just suppliedValue -> action suppliedValue

setExferenceBool
  :: ReplSetting
  -> Maybe String
  -> ReplState
  -> (Bool -> ExferenceOptions -> ExferenceOptions)
  -> IO ReplState
setExferenceBool setting value state update = case
    parseBoolean (replSettingName setting) value of
  Left failure -> settingFailure failure >> pure state
  Right enabled -> pure state
    { exferenceSearchOptions = update enabled $ exferenceSearchOptions state }

unsetOption :: String -> ReplState -> IO ReplState
unsetOption source state = case words $ map toLower $ trim source of
  [rawName] -> case parseReplSetting rawName of
    Left failure -> settingFailure failure >> pure state
    Right setting -> reset setting
  [] -> settingFailure "expected a setting name" >> pure state
  _ -> settingFailure "expected exactly one setting name" >> pure state
 where
  reset setting = case setting of
    BackendSetting -> pure state
      {activeBackends = replInitialBackend defaultReplOptions}
    TargetSetting -> case defaultTarget of
      Left failure -> emitDiagnostic failure >> pure state
      Right target -> pure state {resultTarget = target}
    SelectionSetting -> pure state
      { presentation = (presentation state)
          { presentationSelection = presentationSelection
              defaultInteractivePresentationOptions }
      }
    RenderingSetting -> pure state
      { presentation = (presentation state)
          { presentationRenderMode = presentationRenderMode
              defaultInteractivePresentationOptions }
      }
    QualificationSetting -> pure state
      { presentation = (presentation state)
          { presentationQualification = presentationQualification
              defaultInteractivePresentationOptions }
      }
    PromptSetting -> pure state {promptTemplate = "djex[%b]> "}
    CandidateLimitSetting -> pure state
      { djinnSearchOptions = (djinnSearchOptions state)
          { optionCutoff = optionCutoff defaultQueryOptions }
      }
    ChoiceBudgetSetting -> pure state
      { djinnSearchOptions = (djinnSearchOptions state)
          { optionBudget = optionBudget defaultQueryOptions }
      }
    AllowUnusedSetting -> resetExference $ \defaults options -> options
      {exferenceAllowUnused = exferenceAllowUnused defaults}
    AllowConstraintsSetting -> resetExference $ \defaults options -> options
      { exferenceAllowResidualConstraints =
          exferenceAllowResidualConstraints defaults }
    MultiConstructorPatternsSetting -> resetExference
      $ \defaults options -> options
      { exferenceMultiConstructorPatterns =
          exferenceMultiConstructorPatterns defaults }
    ConstraintDeferralStepsSetting -> resetExference
      $ \defaults options -> options
      { exferenceConstraintDeferralSteps =
          exferenceConstraintDeferralSteps defaults }
    MaximumStepsSetting -> resetExference $ \defaults options -> options
      {exferenceMaximumSteps = exferenceMaximumSteps defaults}
    MaximumQueueSetting -> resetExference $ \defaults options -> options
      {exferenceMaximumQueueSize = exferenceMaximumQueueSize defaults}
    MaximumDepthSetting -> resetExference $ \defaults options -> options
      {exferenceMaximumDepth = exferenceMaximumDepth defaults}
    FixSetting
      | exferenceRuntimeAllowsFix (exferenceRuntime state) ->
          replaceExferenceEnvironment
            (exferenceRuntimePath $ exferenceRuntime state) False state
      | otherwise -> pure state

  resetExference update = pure state
    { exferenceSearchOptions = update defaultExferenceOptions
        $ exferenceSearchOptions state }

settingInvocation :: String -> Either String (ReplSetting, Maybe String)
settingInvocation source = case trim source of
  sign : rest
    | sign `elem` "+-"
    , not (null rest)
    , all (not . isSpace) rest ->
        checked rest $ Just $ if sign == '+' then "on" else "off"
  value -> case break (== '=') value of
    (name, '=' : settingValue) -> checked name $ Just $ trim settingValue
    _ -> case words value of
      [] -> Left "expected a setting name"
      name : _ -> checked name $ optionalRemainder name value
 where
  checked rawName value
    | null normalized = Left "expected a setting name"
    | otherwise = do
        setting <- parseReplSetting normalized
        Right (setting, value)
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
checkedTarget = parseResultTarget . trim

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
  $ map render [minBound .. maxBound]
  ++ ["environment = " ++ exferenceRuntimePath (exferenceRuntime state)]
 where
  render setting = replSettingName setting ++ " = "
    ++ renderSettingValue state setting

renderSettingValue :: ReplState -> ReplSetting -> String
renderSettingValue state setting = case setting of
  BackendSetting -> replBackendName $ activeBackends state
  TargetSetting -> definitionSpelling $ resultTarget state
  SelectionSetting -> selectionModeName
    $ presentationSelection $ presentation state
  RenderingSetting -> renderModeName
    $ presentationRenderMode $ presentation state
  QualificationSetting -> qualificationName
    $ presentationQualification $ presentation state
  PromptSetting -> show $ promptTemplate state
  CandidateLimitSetting -> show $ optionCutoff $ djinnSearchOptions state
  ChoiceBudgetSetting -> maybe "0" show
    $ optionBudget $ djinnSearchOptions state
  AllowUnusedSetting -> booleanName
    $ exferenceAllowUnused $ exferenceSearchOptions state
  AllowConstraintsSetting -> booleanName
    $ exferenceAllowResidualConstraints $ exferenceSearchOptions state
  ConstraintDeferralStepsSetting -> show
    $ exferenceConstraintDeferralSteps $ exferenceSearchOptions state
  MultiConstructorPatternsSetting -> booleanName
    $ exferenceMultiConstructorPatterns $ exferenceSearchOptions state
  MaximumStepsSetting -> show
    $ exferenceMaximumSteps $ exferenceSearchOptions state
  MaximumQueueSetting -> renderBounded
    $ exferenceMaximumQueueSize $ exferenceSearchOptions state
  MaximumDepthSetting -> renderBounded
    $ exferenceMaximumDepth $ exferenceSearchOptions state
  FixSetting -> booleanName
    $ exferenceRuntimeAllowsFix $ exferenceRuntime state

booleanName :: Bool -> String
booleanName True = "on"
booleanName False = "off"

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
    omissions -> mapM_ (putStrLn . renderOmission) omissions

renderOmission :: ExferenceOmission -> String
renderOmission omission = renderCanonical (omittedName omission) ++ ": "
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
      Just session -> do
        browse "Exference loaded declarations"
          ExferenceType.defaultVariableName
          $ exferenceSessionEnvironment session
        when (not $ null $ exferenceSessionOmissions session) $ putStrLn
          "-- Some loaded capabilities are not searchable; use :show omissions."

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
        Just session -> do
          let environment = exferenceSessionEnvironment session
              matchingNames = name : map declarationSubjectName
                (matchingDeclarations name environment)
          info "Exference loaded declarations"
            ExferenceType.defaultVariableName name
            environment
          mapM_ (putStrLn . ("-- search omission: " ++) . renderOmission)
            $ filter ((`elem` matchingNames) . omittedName)
            $ exferenceSessionOmissions session

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
  case matchingDeclarations name environment of
    [] -> putStrLn $ "No declaration for " ++ renderCanonical name
    declarations -> mapM_ (putStrLn . renderDeclaration variableName)
      declarations

matchingDeclarations
  :: Name
  -> Environment variable kind annotation
  -> [Declaration variable kind annotation]
matchingDeclarations name = filter (declarationDefines name)
  . environmentDeclarations

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

runScript :: FilePath -> [String] -> ReplState -> IO (ReplStep ReplState)
runScript path history state = do
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
                history state {scriptStack = canonical : scriptStack state} inputs

runInputs
  :: FilePath
  -> [String]
  -> ReplState
  -> [(Int, String)]
  -> IO (ReplStep ReplState)
runInputs _ _ state [] = pure $ ContinueRepl state
  {scriptStack = drop 1 $ scriptStack state}
runInputs sourceName history state ((lineNumber, source) : remaining) = do
  outcome <- executeSource (scriptSourceName sourceName lineNumber)
    state history source
  case outcome of
    ExitRepl final -> pure $ ExitRepl final
      {scriptStack = drop 1 $ scriptStack final}
    ContinueRepl next -> runInputs sourceName history next remaining

scriptSourceName :: FilePath -> Int -> FilePath
scriptSourceName path lineNumber = path ++ " (line " ++ show lineNumber ++ ")"

scriptInputs :: [String] -> Either String [(Int, String)]
scriptInputs = go 1 []
 where
  go _ result [] = Right $ reverse result
  go lineNumber result (line : remaining)
    | trimmed == ":{" = collect lineNumber (lineNumber + 1)
        result [] remaining
    | trimmed == ":}" = Left $ "line " ++ show lineNumber
        ++ ": unexpected :}"
    | otherwise = go (lineNumber + 1) ((lineNumber, line) : result) remaining
   where
    trimmed = trim line

  collect startLine _ _ _ [] = Left $ "line " ++ show startLine
    ++ ": unterminated multiline input (expected :})"
  collect startLine lineNumber result body (line : remaining)
    | trim line == ":}" = go (lineNumber + 1)
        ((startLine, unlines $ reverse body) : result) remaining
    | otherwise = collect startLine (lineNumber + 1)
        result (line : body) remaining

strictReadFile :: FilePath -> IO (Either IOError String)
strictReadFile path = tryIOError $ withFile path ReadMode $ \handle ->
  hGetContents handle >>= evaluate . force

resolveOptionalPath
  :: Maybe FilePath
  -> IO (Either Diagnostic (Maybe FilePath))
resolveOptionalPath Nothing = pure $ Right Nothing
resolveOptionalPath (Just path) = do
  resolved <- tryIOError $ canonicalizePath path
  case resolved of
    Left failure -> pure $ Left $ ioDiagnostic
      "cannot resolve REPL history file" path failure
    Right canonical -> do
      validated <- tryIOError $ validateHistoryPath canonical
      pure $ case validated of
        Left failure -> Left $ ioDiagnostic
          "cannot inspect REPL history file" canonical failure
        Right result -> result

validateHistoryPath :: FilePath -> IO (Either Diagnostic (Maybe FilePath))
validateHistoryPath path = do
  targetExists <- doesPathExist path
  targetIsDirectory <- doesDirectoryExist path
  if targetIsDirectory
    then pure $ rejected "history path names a directory"
    else if targetExists
      then do
        permissions <- getPermissions path
        pure $ if readable permissions && writable permissions
          then Right $ Just path
          else rejected "history file must be readable and writable"
      else validateParent
 where
  validateParent = do
    let parent = takeDirectory path
    parentExists <- doesDirectoryExist parent
    if not parentExists
      then pure $ rejected "history file parent directory does not exist"
      else do
        permissions <- getPermissions parent
        pure $ if writable permissions && searchable permissions
          then Right $ Just path
          else rejected "history file parent directory is not writable"

  rejected message = Left $ contextualDiagnostic Error
    "DJEX_REPL_HISTORY_FILE" message path

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
