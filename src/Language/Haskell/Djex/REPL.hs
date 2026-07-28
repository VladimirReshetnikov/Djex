{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

-- | Persistent, GHCi-style access to both Djex synthesis backends.
--
-- One REPL owns immutable Djinn and Exference sessions built from the same
-- loaded module scope: Exference seals the source workspace directly, and
-- Djinn receives a checked projection of the visible declarations with its
-- own representation and omission policy. Backend switching changes only the
-- active query target. Environment replacement is explicit and
-- transactional, so a failed load leaves both backends' last usable sessions
-- intact.
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
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.List (intercalate, isPrefixOf)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Version (showVersion)
import Data.Void (Void)
import System.Directory
  ( Permissions (readable, searchable, writable)
  , canonicalizePath
  , doesDirectoryExist
  , doesFileExist
  , doesPathExist
  , getHomeDirectory
  , getPermissions
  , getCurrentDirectory
  , setCurrentDirectory
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath (takeDirectory)
import qualified System.FilePath as FilePath
import System.IO (IOMode (ReadMode), hGetContents, withFile)
import System.IO.Error (isDoesNotExistError, tryIOError)
import System.Process
  ( CreateProcess (delegate_ctlc)
  , proc
  , shell
  , waitForProcess
  , withCreateProcess
  )
import Text.Read (readMaybe)

#if !defined(mingw32_HOST_OS)
import System.Posix.Files
  ( fileMode
  , fileOwner
  , getFileStatus
  , groupWriteMode
  , intersectFileModes
  , otherWriteMode
  )
import System.Posix.User (getRealUserID)
#endif

import Language.Haskell.Djex
import Language.Haskell.Djex.Command
import Language.Haskell.Djex.Exference.HaskellSrc
  ( ExferenceSessionLoadReport (..)
  , defaultExferenceEnvironmentPath
  , exferenceCommandSessionPolicy
  , loadExferenceSessionFromSourcesWithPolicy
  , mkExferenceRequestWithCheckedTargetFromParsed
  )
import Language.Haskell.Djex.HaskellSrc
  ( ExferenceQueryScope (..)
  , ParsedSourceType
  , parseSourceTypeInScope
  , parsedSourceType
  )
import Language.Haskell.Djex.Package
  ( PackageOperation (DownloadOperation, InstallOperation)
  , runPackageOperation
  )
import Language.Haskell.Djex.REPL.Command
import Language.Haskell.Djex.REPL.DjinnScope
import Language.Haskell.Djex.REPL.Driver
import Language.Haskell.Djex.REPL.Eval
import Language.Haskell.Djex.REPL.Kind
import Language.Haskell.Djex.REPL.Scope
import Language.Haskell.Djex.REPL.Type
import Language.Haskell.Djex.REPL.Workspace
import Language.Haskell.Djex.Text (normalize, trim)
import qualified Language.Haskell.Djex.Exference.Internal.Session
  as ExferenceSession
import qualified Language.Haskell.Exference.Core.Types as ExferenceType
import Paths_djex (version)

-- | Startup configuration for the shared interactive frontend.
data ReplOptions = ReplOptions
  { replInitialBackend :: ReplBackend
    -- ^ Backend selection shown by the first prompt.
  , replEnvironmentPath :: Maybe FilePath
    -- ^ Source-workspace directory; 'Nothing' uses installed package data.
  , replAllowFix :: Bool
    -- ^ Whether known recursion helpers are retained while sealing Exference.
  , replHistoryFile :: Maybe FilePath
    -- ^ Optional Haskeline history file. 'Nothing' keeps in-session history.
  , replIgnoreStartupFiles :: Bool
    -- ^ Whether @.djexrc@ startup files are skipped, as GHCi's
    -- @-ignore-dot-ghci@ skips @.ghci@.
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
  , replIgnoreStartupFiles = False
  }

data ReplState = ReplState
  { activeBackends :: ReplBackend
  , djinnRuntime :: DjinnRuntime
  , exferenceRuntime :: ExferenceRuntime
  , scopeFieldSelectors :: FieldSelectors
    -- ^ Canonical, unqualified-visible selector spellings per constructor
    -- field position, for presenting Exference candidates; Djinn's renamed
    -- table lives in its projection. Hidden selectors deliberately leave
    -- structural eliminations unchanged.
  , resultTarget :: DefinitionName
  , presentation :: PresentationOptions
  , djinnSearchOptions :: QueryOptions
  , exferenceSearchOptions :: ExferenceOptions
  , promptTemplate :: String
  , lastQuery :: Maybe (ReplBackend, String)
  , scriptStack :: [FilePath]
  }

-- | Djinn's session state. When a source workspace is loaded, Djinn tracks
-- the same module scope Exference searches through a checked projection;
-- without one it falls back to its historical standard environment.
data DjinnRuntime = DjinnRuntime
  { djinnStandardSession :: DjinnSession
  , djinnAxiomPolicy :: DjinnAxiomPolicy
  , djinnProjection :: Maybe DjinnProjection
  }

currentDjinnSession :: ReplState -> DjinnSession
currentDjinnSession state = case djinnProjection djinn of
  Just projection -> djinnProjectionSession projection
  Nothing -> djinnStandardSession djinn
 where
  djinn = djinnRuntime state

-- | Recompute Djinn's view of the module scope after any change to the
-- loaded workspace, the scope, or the axiom policy. The caller decides how to
-- recover from failure: startup has no earlier source state and may use the
-- standard session, while interactive mutations must reject the whole
-- candidate so Exference and Djinn cannot publish different scopes.
refreshDjinnProjection :: ReplState -> Either Diagnostic ReplState
refreshDjinnProjection state = case
    ( exferenceRuntimeBaseSession runtime
    , exferenceRuntimeScope runtime
    ) of
  (Just baseSession, Just context) ->
    let visibleTypes = Set.fromList $ scopeUnqualifiedTypeNames context
        visibleValues = Set.fromList $ scopeUnqualifiedValueNames context
    in case projectDjinnScope
        (djinnAxiomPolicy djinn)
        records
        (typeConstructorKinds $ inventoryKindAssumptions
          $ exferenceSessionInventory baseSession)
        (scopeProjectionDeclarations baseSession)
        visibleTypes
        visibleValues of
      Left failure -> Left failure
      Right projection -> Right $ withProjection visibleValues $ Just projection
  _ -> Right $ withProjection Set.empty Nothing
 where
  runtime = exferenceRuntime state
  djinn = djinnRuntime state
  records = case
      ( exferenceRuntimeBaseSession runtime
      , exferenceRuntimeWorkspace runtime
      ) of
    (Just baseSession, Just workspace) -> workspaceRecordProjections
      (exferenceSessionInventory baseSession) workspace
    _ -> []
  withProjection visible projection = state
    { djinnRuntime = djinn {djinnProjection = projection}
    , scopeFieldSelectors = Map.fromList
        [ ((constructor, index), selector)
        | (_, constructors) <- records
        , (constructor, selectorNames) <- constructors
        , (index, selector) <- zip [0 ..] selectorNames
        , selector `Set.member` visible
        ]
    }

retainAfterDjinnProjectionFailure :: ReplState -> Diagnostic -> IO ReplState
retainAfterDjinnProjectionFailure state failure = do
  emitDiagnostic failure
  putStrLn
    "Djinn projection failed; retaining the previous sessions and settings."
  pure state

scopeProjectionDeclarations :: ExferenceSession -> [Declaration String Void ()]
scopeProjectionDeclarations = map
  (mapDeclarationTypeVariables ExferenceType.defaultVariableName)
  . environmentDeclarations
  . exferenceSessionEnvironment

djinnEnvironmentSummary :: ReplState -> String
djinnEnvironmentSummary state = show
    (length $ environmentDeclarations
      $ djinnSessionEnvironment $ currentDjinnSession state)
  ++ " declarations " ++ case djinnProjection $ djinnRuntime state of
    Just projection -> "(projected from the module scope, "
      ++ show (length $ djinnProjectionOmissions projection)
      ++ " omissions)"
    Nothing -> "(standard checked environment)"

data ExferenceRuntime = ExferenceRuntime
  { exferenceRuntimeRequestedTargets :: [String]
  , exferenceRuntimeWorkspace :: Maybe SourceWorkspace
  , exferenceRuntimeScope :: Maybe ReplScope
  , exferenceRuntimeAllowsFix :: Bool
  , exferenceRuntimeBaseSession :: Maybe ExferenceSession
  , exferenceRuntimeSession :: Maybe ExferenceSession
  , exferenceRuntimeDiagnostics :: [Diagnostic]
  }

data LoadAttempt = LoadAttempt
  { attemptedTargets :: [String]
  , attemptedWorkspace :: Maybe SourceWorkspace
  , attemptedScope :: Maybe ReplScope
  , attemptedBaseSession :: Maybe ExferenceSession
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
          attempt <- loadExference [requestedPath] $ replAllowFix options
          let exference = ExferenceRuntime
                { exferenceRuntimeRequestedTargets = attemptedTargets attempt
                , exferenceRuntimeWorkspace = attemptedWorkspace attempt
                , exferenceRuntimeScope = attemptedScope attempt
                , exferenceRuntimeAllowsFix = replAllowFix options
                , exferenceRuntimeBaseSession = attemptedBaseSession attempt
                , exferenceRuntimeSession = attemptedSession attempt
                , exferenceRuntimeDiagnostics = attemptedDiagnostics attempt
                }
              initial = ReplState
                { activeBackends = replInitialBackend options
                , djinnRuntime = DjinnRuntime
                    { djinnStandardSession = djinnSession
                    , djinnAxiomPolicy = ExcludeDjinnAxioms
                    , djinnProjection = Nothing
                    }
                , exferenceRuntime = exference
                , scopeFieldSelectors = noFieldSelectors
                , resultTarget = target
                , presentation = defaultInteractivePresentationOptions
                , djinnSearchOptions = defaultQueryOptions
                , exferenceSearchOptions = defaultExferenceOptions
                , promptTemplate = defaultPromptTemplate
                , lastQuery = Nothing
                , scriptStack = []
                }
          putStrLn $ "Djex REPL " ++ showVersion version
          ready <- case refreshDjinnProjection initial of
            Left failure -> do
              emitDiagnostic failure
              putStrLn "Djinn falls back to its standard checked environment."
              pure initial
            Right projected -> pure projected
          putStrLn $ "Djinn environment: " ++ djinnEnvironmentSummary ready
          case attemptedSession attempt of
            Just _ -> putStrLn $ "Source workspace: "
              ++ renderRequestedTargets (attemptedTargets attempt)
            Nothing -> putStrLn
              "Source workspace unavailable; use :load TARGET after fixing it."
          putStrLn "Type :help for help."
          started <- if replIgnoreStartupFiles options
            then pure $ ContinueRepl ready
            else runStartupFiles ready
          case started of
            ExitRepl _ -> pure ExitSuccess
            ContinueRepl configured -> do
              _ <- runReplDriver historyPath configured renderPrompt
                replCompletions $ executeSource "<interactive>"
              pure ExitSuccess

defaultPromptTemplate :: String
defaultPromptTemplate = "djex[%b]> "

-- | Run @.djexrc@ from the home directory and then the current directory
-- through the ordinary script machinery, following GHCi's trust boundary for
-- @.ghci@: on POSIX, both the file and its containing directory must be owned
-- by the current user (or root) and must not be group- or world-writable.
-- Missing files are skipped silently; a broken line reports and continues.
runStartupFiles :: ReplState -> IO (ReplStep ReplState)
runStartupFiles initial = do
  home <- startupDirectory "home directory" getHomeDirectory
  current <- startupDirectory "current directory" getCurrentDirectory
  candidates <- startupCandidates $ home ++ current
  go initial candidates
 where
  go state [] = pure $ ContinueRepl state
  go state (path : remaining) = do
    outcome <- runScriptWithAnnouncement path [] state
    case outcome of
      ExitRepl final -> pure $ ExitRepl final
      ContinueRepl next -> go next remaining

-- Failure to locate a configured startup directory is different from an
-- absent .djexrc inside a successfully located directory. Keep absence quiet,
-- but do not silently discard an I/O failure that prevents discovery.
startupDirectory :: String -> IO FilePath -> IO [FilePath]
startupDirectory description locate = do
  resolved <- tryIOError locate
  case resolved of
    Left failure -> startupPathFailure
        ("cannot locate the " ++ description) description failure
      >> pure []
    Right path -> pure [path]

-- Canonical deduplication keeps one run when the home and current
-- directories coincide.
startupCandidates :: [FilePath] -> IO [FilePath]
startupCandidates directories = go Set.empty directories
 where
  go _ [] = pure []
  go seen (directory : remaining) = do
    let candidate = directory FilePath.</> ".djexrc"
    inspected <- tryIOError $ getPermissions candidate
    case inspected of
      Left failure
        | isDoesNotExistError failure -> go seen remaining
        | otherwise -> startupPathFailure "cannot inspect startup file"
            candidate failure >> go seen remaining
      Right _ -> do
        present <- doesFileExist candidate
        if not present
          then go seen remaining
          else do
            resolved <- tryIOError $ canonicalizePath candidate
            case resolved of
              Left failure -> startupPathFailure "cannot resolve startup file"
                  candidate failure >> go seen remaining
              Right canonical
                | canonical `Set.member` seen -> go seen remaining
                | otherwise -> do
                    trusted <- trustedStartupPath canonical
                    if trusted
                      then (canonical :)
                        <$> go (Set.insert canonical seen) remaining
                      else go (Set.insert canonical seen) remaining

startupPathFailure :: String -> FilePath -> IOError -> IO ()
startupPathFailure summary path failure = emitDiagnostic
  $ contextualDiagnostic Warning "DJEX_REPL_STARTUP_PATH" summary
  $ path ++ ": " ++ show failure

-- Match GHCi's deliberate platform split. POSIX exposes the ownership and
-- group/other mode bits needed for a meaningful check; Windows' Directory
-- permission abstraction does not, so GHCi accepts the file there as well.
trustedStartupPath :: FilePath -> IO Bool
#if defined(mingw32_HOST_OS)
trustedStartupPath _ = pure True
#else
trustedStartupPath path = do
  inspected <- tryIOError $ do
    user <- getRealUserID
    fileTrusted <- trustedBy user path
    if fileTrusted
      then trustedBy user $ takeDirectory path
      else pure False
  case inspected of
    Left failure -> do
      emitDiagnostic $ contextualDiagnostic Warning
        "DJEX_REPL_STARTUP_TRUST"
        "cannot verify startup-file ownership and permissions"
        $ path ++ ": " ++ show failure
      pure False
    Right True -> pure True
    Right False -> do
      emitDiagnostic $ contextualDiagnostic Warning
        "DJEX_REPL_STARTUP_UNTRUSTED"
        "ignored an untrusted startup file"
        $ path ++ ": require current-user or root ownership and remove "
          ++ "group/other write permission from the file and its directory"
      pure False
 where
  trustedBy user candidate = do
    status <- getFileStatus candidate
    let mode = fileMode status
        owner = fileOwner status
        writableByGroup =
          mode `intersectFileModes` groupWriteMode == groupWriteMode
        writableByOthers =
          mode `intersectFileModes` otherWriteMode == otherWriteMode
    pure $ (owner == user || owner == 0)
      && not writableByGroup
      && not writableByOthers
#endif

-- | Project the completion candidates GHCi would offer: loaded module names
-- for module-oriented commands and namespace-appropriate identifier spellings
-- at query positions, both qualified and unqualified.
replCompletions :: ReplState -> ReplCompletions
replCompletions state = ReplCompletions
  { completionModules = maybe []
      (map workspaceModuleName . workspaceModules)
      $ exferenceRuntimeWorkspace runtime
  , completionIdentifiers = allIdentifiers
  , completionTypeIdentifiers = typeIdentifiers
  }
 where
  runtime = exferenceRuntime state
  (allIdentifiers, typeIdentifiers) = case exferenceRuntimeScope runtime of
    Nothing -> ([], [])
    Just context ->
      ( scopeSpellings
          (scopeUnqualifiedNames context)
          (scopeQualifiedNames context)
      , scopeSpellings
          (scopeUnqualifiedTypeNames context)
          (scopeQualifiedTypeNames context)
      )

  scopeSpellings unqualified qualified = Set.toList $ Set.fromList
    $ mapMaybe nameSpelling unqualified
    ++ [ renderModuleName qualifier ++ "." ++ spelling
       | (qualifier, names) <- qualified
       , Just spelling <- map nameSpelling names
       ]

-- | Open the configured editor, defaulting to the most recently loaded
-- explicit file target as GHCi's @:edit@ defaults to the current module.
editTargetFile :: Maybe FilePath -> ReplState -> IO ()
editTargetFile requested state = do
  visual <- lookupEnv "VISUAL"
  fallback <- lookupEnv "EDITOR"
  case filter (not . null . trim) $ catMaybes [visual, fallback] of
    [] -> replFailure "DJEX_REPL_EDITOR" "no editor is configured"
      "set the VISUAL or EDITOR environment variable"
    editor : _ -> case parseCommandWords editor of
      Left failure -> replFailure "DJEX_REPL_EDITOR"
        "invalid editor command" failure
      Right [] -> replFailure "DJEX_REPL_EDITOR"
        "invalid editor command" "VISUAL or EDITOR contains no executable"
      Right (executable : arguments) -> do
        target <- maybe (latestFileTarget state) (pure . Just) requested
        case target of
          Nothing -> replFailure "DJEX_REPL_EDITOR" "no file target to edit"
            "load a source file or name one with :edit FILE"
          Just path -> runEditorCommand executable arguments path

latestFileTarget :: ReplState -> IO (Maybe FilePath)
latestFileTarget state = case exferenceRuntimeWorkspace
    $ exferenceRuntime state of
  Nothing -> pure Nothing
  Just workspace -> firstExisting
    [ path
    | target <- reverse $ workspaceTargets workspace
    , path <- reverse $ workspaceTargetModuleFiles target
    ]
 where
  firstExisting [] = pure Nothing
  firstExisting (path : remaining) = do
    present <- doesFileExist path
    if present then pure $ Just path else firstExisting remaining

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
  Right (ReplImport importSource) -> ContinueRepl
    <$> addImportToScope sourceName importSource state
  Right (ReplCommand command) -> runCommand sourceName history command state

runCommand
  :: FilePath
  -> [String]
  -> ReplCommand
  -> ReplState
  -> IO (ReplStep ReplState)
runCommand sourceName history command state = case command of
  AddEnvironment [] -> continue state
  AddEnvironment targets -> updateExferenceWorkspace
    (addWorkspaceTargetsFor state targets)
    ResetScope
    ("Added source targets: " ++ renderRequestedTargets targets)
    state >>= continue
  Browse selectedModule -> browseState selectedModule state >> continue state
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
  ChangeModules change modules -> changeModuleScope change modules state
    >>= continue
  CompareBackends typeSource ->
    runQuery sourceName (ExplicitBackends BothBackends) typeSource state
  DownloadPackages packages ->
    runPackageOperation DownloadOperation packages >> continue state
  EditFile requested -> editTargetFile requested state >> continue state
  Evaluate expression -> evaluateInteractive expression state >> continue state
  Help Nothing -> putStr shortHelp >> continue state
  Help (Just name) -> case commandHelp name of
    Left failure -> settingFailure failure >> continue state
    Right help -> putStr help >> continue state
  History countSource -> do
    showHistory countSource history
    continue state
  InspectDeclaration nameSource -> showInfo state nameSource >> continue state
  InspectKind normalization typeSource ->
    showTypeKind sourceName normalization typeSource state >> continue state
  InspectType defaulting expression ->
    showExpressionType sourceName defaulting expression state >> continue state
  InstallPackages mode packages ->
    runPackageOperation (InstallOperation mode) packages >> continue state
  LoadEnvironment targets -> updateExferenceWorkspace
    (loadWorkspace targets)
    ResetScope
    ("Loaded source workspace: " ++ renderRequestedTargets targets)
    state >>= continue
  Quit -> pure $ ExitRepl state
  ReloadEnvironment -> do
    next <- reloadExferenceWorkspace state
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
  UnaddEnvironment [] -> continue state
  UnaddEnvironment targets -> updateExferenceWorkspace
    (removeWorkspaceTargetsFor state targets)
    ResetScope
    ("Removed source targets: " ++ renderRequestedTargets targets)
    state >>= continue
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
  case sharedRuntime of
    Just (session, context)
      | sharedProjectionAvailable -> case parseSourceTypeInScope
          (exferenceSessionInventory session)
          (queryScope context) sourceName typeSource of
        Left failure -> emitDiagnostic failure
        Right parsed -> runParsedSelection session parsed
    _ -> runLegacySelection
  pure $ ContinueRepl state
    { lastQuery = Just (selected, typeSource) }
 where
  runtime = exferenceRuntime state
  sharedRuntime =
    (,) <$> exferenceRuntimeSession runtime <*> exferenceRuntimeScope runtime
  sharedProjectionAvailable = case selected of
    OneBackend ExferenceBackend -> True
    _ -> case djinnProjection $ djinnRuntime state of
      Just _ -> True
      Nothing -> False

  runParsedSelection session parsed = case selected of
    OneBackend selectedBackend ->
      runParsedBackend False session parsed selectedBackend
    BothBackends -> do
      runParsedBackend True session parsed DjinnBackend
      runParsedBackend True session parsed ExferenceBackend

  runLegacySelection = case selected of
    OneBackend selectedBackend -> runLegacyBackend False selectedBackend
    BothBackends -> do
      runLegacyBackend True DjinnBackend
      runLegacyBackend True ExferenceBackend

  labelBackend labelled selectedBackend = when labelled $ putStrLn
    $ "-- " ++ backendName (backendInfo selectedBackend)

  runLegacyBackend labelled selectedBackend = do
    labelBackend labelled selectedBackend
    case selectedBackend of
      DjinnBackend -> runDjinnInteractive sourceName typeSource state
      ExferenceBackend -> runExferenceInteractive sourceName typeSource state

  runParsedBackend labelled session parsed selectedBackend = do
    labelBackend labelled selectedBackend
    case selectedBackend of
      DjinnBackend -> runParsedDjinn parsed
      ExferenceBackend -> runParsedExference session parsed

  runParsedDjinn parsed = case mkDjinnRequest QueryRequest
      { requestTarget = resultTarget state
      , requestGoal = projectParsedTypeToDjinn state parsed
      , requestContexts = []
      , requestOptions = prepareDjinnQueryOptions
          (presentation state) (djinnSearchOptions state)
      } of
    Left failure -> emitDiagnostic failure
    Right request -> case runDjinnQuery (currentDjinnSession state) request of
      Left failure -> emitDiagnostic failure
      Right result -> ignoreExit $ presentDjinn
        (presentation state)
        (maybe noFieldSelectors djinnProjectionFieldSelectors
          $ djinnProjection $ djinnRuntime state)
        result

  runParsedExference session parsed = case
      mkExferenceRequestWithCheckedTargetFromParsed
        (exferenceSearchOptions state) (resultTarget state) parsed of
    Left failure -> emitDiagnostic failure
    Right request -> case runExferenceQuery session request of
      Left failure -> emitDiagnostic failure
      Right results -> ignoreExit $ presentExference
        (presentation state) (scopeFieldSelectors state) results

projectParsedTypeToDjinn
  :: ReplState
  -> ParsedSourceType
  -> DjinnType
projectParsedTypeToDjinn state = mapTypeNames projectName
  . fmap ExferenceType.defaultVariableName
  . parsedSourceType
 where
  promptNames = maybe Map.empty djinnProjectionPromptNames
    $ djinnProjection $ djinnRuntime state
  projectName name = Map.findWithDefault name name promptNames

queryScope :: ReplScope -> ExferenceQueryScope
queryScope context = ExferenceQueryScope
  { exferenceQueryCurrentModule = scopeCurrentModule context
  , exferenceQueryVisibleNames = scopeUnqualifiedTypeNames context
  , exferenceQueryModuleAliases = scopeQualifierAliases context
  , exferenceQueryQualifiedNames = scopeQualifiedTypeNames context
  }

-- The legacy runners remain the fallback when no shared source inventory is
-- available. With a loaded workspace, 'runResolvedQuery' parses once above
-- and feeds the exact checked type to both engines.

runDjinnInteractive :: FilePath -> String -> ReplState -> IO ()
runDjinnInteractive sourceName typeSource state = ignoreExit
  $ executeDjinnCommand
      (presentation state)
      (maybe noFieldSelectors djinnProjectionFieldSelectors
        $ djinnProjection $ djinnRuntime state)
      (currentDjinnSession state)
      (djinnSearchOptions state)
      (resultTarget state)
      sourceName
      typeSource

runExferenceInteractive :: FilePath -> String -> ReplState -> IO ()
runExferenceInteractive sourceName typeSource state = case runtimeState of
  (Just session, Just context) -> ignoreExit
    $ executeExferenceCommandInScope
        (presentation state)
        (scopeFieldSelectors state)
        session
        (exferenceSearchOptions state)
        (resultTarget state)
        (queryScope context)
        sourceName
        typeSource
  _ -> replFailure "DJEX_REPL_EXFERENCE_UNAVAILABLE"
    "Exference has no loaded source workspace"
    $ "use :load TARGET; last attempted targets: "
      ++ renderRequestedTargets
        (exferenceRuntimeRequestedTargets runtime)
 where
  runtime = exferenceRuntime state
  runtimeState =
    (exferenceRuntimeSession runtime, exferenceRuntimeScope runtime)

ignoreExit :: IO ExitCode -> IO ()
ignoreExit action = action >> pure ()

showExpressionType
  :: FilePath
  -> TypeDefaulting
  -> String
  -> ReplState
  -> IO ()
showExpressionType sourceName defaulting expression state = case
    ( exferenceRuntimeBaseSession runtime
    , exferenceRuntimeScope runtime
    ) of
  (Just session, Just scope) -> case inferExpressionType
      session scope (resultTarget state) defaulting sourceName expression of
    Left failure -> emitDiagnostic failure
    Right inferred -> putStrLn $ inferredExpressionSource inferred ++ " :: "
      ++ renderInferredType
          (presentationQualification $ presentation state) inferred
  _ -> replFailure "DJEX_REPL_TYPE_UNAVAILABLE"
    "type inference has no loaded source workspace"
    "use :load TARGET before :type"
 where
  runtime = exferenceRuntime state

showTypeKind
  :: FilePath
  -> KindNormalization
  -> String
  -> ReplState
  -> IO ()
showTypeKind sourceName normalization typeSource state = case
    ( exferenceRuntimeBaseSession runtime
    , exferenceRuntimeScope runtime
    ) of
  (Just session, Just scope) -> case inspectKind
      session scope sourceName normalization typeSource of
    Left failure -> emitDiagnostic failure
    Right inspection -> mapM_ putStrLn $ renderKindInspection
      (presentationQualification $ presentation state)
      normalization
      inspection
  _ -> replFailure "DJEX_REPL_KIND_UNAVAILABLE"
    "kind inference has no loaded source workspace"
    "use :load TARGET before :kind"
 where
  runtime = exferenceRuntime state

data ShellOutcome
  = ShellCompleted ExitCode
  | ShellInterrupted

runShellCommand :: String -> IO ()
runShellCommand command = runExternalCommand
  "DJEX_REPL_SHELL"
  "cannot run shell command"
  "shell command failed"
  command
  (shell command)

-- Editor configuration is parsed once and launched directly. In particular,
-- a source path is one argv element and can never become shell syntax.
runEditorCommand :: FilePath -> [String] -> FilePath -> IO ()
runEditorCommand executable arguments path = runExternalCommand
  "DJEX_REPL_EDITOR"
  "cannot launch editor"
  "editor command failed"
  rendered
  (proc executable $ arguments ++ [path])
 where
  rendered = unwords $ map show $ executable : arguments ++ [path]

runExternalCommand
  :: String
  -> String
  -> String
  -> String
  -> CreateProcess
  -> IO ()
runExternalCommand code launchSummary failureSummary rendered processSpec = do
  outcome <- tryIOError $ handleJust onlyUserInterrupt
    (const $ pure ShellInterrupted)
    $ withCreateProcess (processSpec {delegate_ctlc = True})
      $ \_ _ _ processHandle ->
        ShellCompleted <$> waitForProcess processHandle
  case outcome of
    Left failure -> ioFailure launchSummary rendered failure
    Right ShellInterrupted -> putStrLn "Interrupted."
    Right (ShellCompleted ExitSuccess) -> pure ()
    Right (ShellCompleted (ExitFailure status)) ->
      replFailure code failureSummary
        $ rendered ++ ": exit status " ++ show status

onlyUserInterrupt :: AsyncException -> Maybe ()
onlyUserInterrupt UserInterrupt = Just ()
onlyUserInterrupt _ = Nothing

-- | Evaluate one expression with real GHC. Unlike every synthesis command,
-- this runs code; a diverging expression is interruptible with Ctrl-C.
evaluateInteractive :: String -> ReplState -> IO ()
evaluateInteractive expression state = do
  outcome <- handleJust onlyUserInterrupt (const $ pure Nothing)
    $ Just <$> evaluateExpression
        (exferenceRuntimeScope runtime)
        evaluableModules
        expression
  case outcome of
    Nothing -> putStrLn "Interrupted."
    Just result -> do
      mapM_ emitDiagnostic $ evalAdvisories result
      case evalResult result of
        Left failure -> emitDiagnostic failure
        Right value -> putStrLn value
 where
  runtime = exferenceRuntime state
  evaluableModules = maybe []
    (map moduleEntry . workspaceModules)
    $ exferenceRuntimeWorkspace runtime
  moduleEntry loaded =
    (workspaceModuleName loaded, workspaceModulePath loaded)

data ScopeRetention = ResetScope | RefreshAutomaticScope

loadExference :: [String] -> Bool -> IO LoadAttempt
loadExference targets allowFix = do
  loaded <- loadWorkspace targets
  case loaded of
    Left failures -> do
      mapM_ emitDiagnostic failures
      pure $ failedLoadAttempt targets Nothing $ toList failures
    Right workspace -> attemptWorkspaceLoad allowFix ResetScope Nothing workspace

attemptWorkspaceLoad
  :: Bool
  -> ScopeRetention
  -> Maybe ReplScope
  -> SourceWorkspace
  -> IO LoadAttempt
attemptWorkspaceLoad allowFix retention previousScope workspace =
  case exferenceCommandSessionPolicy allowFix of
    Left failure -> do
      emitDiagnostic failure
      pure $ failedLoadAttempt targets (Just workspace) [failure]
    Right policy -> do
      report <- loadExferenceSessionFromSourcesWithPolicy
        policy
        (workspaceModuleSources workspace)
        (workspaceRatingSources workspace)
      let advisory = workspaceImportDiagnostics workspace
            ++ exferenceSessionLoadDiagnostics report
          fatal = either toList (const [])
            $ exferenceSessionLoadResult report
          diagnostics = advisory ++ fatal
      mapM_ emitDiagnostic $ filter ((/= Info) . diagnosticSeverity) advisory
      mapM_ emitDiagnostic fatal
      case exferenceSessionLoadResult report of
        Left _ -> pure $ failedLoadAttempt targets (Just workspace) diagnostics
        Right baseSession -> case buildScope
            (exferenceSessionInventory baseSession) of
          Left failure -> do
            emitDiagnostic failure
            pure $ failedLoadAttempt targets (Just workspace)
              $ diagnostics ++ [failure]
          Right context -> case ExferenceSession.scopeExferenceSession
              (Set.fromList $ scopeSearchNames context) baseSession of
            Left failure -> do
              emitDiagnostic failure
              pure $ failedLoadAttempt targets (Just workspace)
                $ diagnostics ++ [failure]
            Right scopedSession -> pure LoadAttempt
              { attemptedTargets = targets
              , attemptedWorkspace = Just workspace
              , attemptedScope = Just context
              , attemptedBaseSession = Just baseSession
              , attemptedSession = Just scopedSession
              , attemptedDiagnostics = diagnostics
              }
 where
  targets = map workspaceTargetDisplay $ workspaceTargets workspace
  buildScope inventory = case (retention, previousScope) of
    (RefreshAutomaticScope, Just context) ->
      revalidateScope inventory workspace context
    _ -> scopeFromWorkspace inventory workspace

-- A bare import that cannot be resolved beneath any admitted source root may
-- intentionally refer to an installed package, but this source-only session
-- has no package database from which to obtain its declarations. Make that
-- boundary visible instead of silently omitting a dependency that the user
-- expected to edit and reload.
workspaceImportDiagnostics :: SourceWorkspace -> [Diagnostic]
workspaceImportDiagnostics workspace =
  [ withSource path
      $ contextualDiagnostic Warning "DJEX_REPL_IMPORT_UNRESOLVED"
          "source import has no loaded local module"
          (importer ++ " imports " ++ imported
            ++ "; declarations from that module are unavailable")
  | (path, importer, imported) <-
      workspaceUnresolvedImportsWithSources workspace
  ]

failedLoadAttempt
  :: [String]
  -> Maybe SourceWorkspace
  -> [Diagnostic]
  -> LoadAttempt
failedLoadAttempt targets workspace diagnostics = LoadAttempt
  { attemptedTargets = targets
  , attemptedWorkspace = workspace
  , attemptedScope = Nothing
  , attemptedBaseSession = Nothing
  , attemptedSession = Nothing
  , attemptedDiagnostics = diagnostics
  }

updateExferenceWorkspace
  :: IO (Either (NonEmpty Diagnostic) SourceWorkspace)
  -> ScopeRetention
  -> String
  -> ReplState
  -> IO ReplState
updateExferenceWorkspace action retention successMessage state =
  updateExferenceWorkspaceWithPolicy
    (exferenceRuntimeAllowsFix $ exferenceRuntime state)
    action retention successMessage state

updateExferenceWorkspaceWithPolicy
  :: Bool
  -> IO (Either (NonEmpty Diagnostic) SourceWorkspace)
  -> ScopeRetention
  -> String
  -> ReplState
  -> IO ReplState
updateExferenceWorkspaceWithPolicy allowFix action retention successMessage
    state = do
  workspaceResult <- action
  case workspaceResult of
    Left failures -> do
      mapM_ emitDiagnostic failures
      retainAfterFailure $ toList failures
    Right workspace -> do
      attempt <- attemptWorkspaceLoad allowFix retention
        (exferenceRuntimeScope $ exferenceRuntime state) workspace
      case attemptedSession attempt of
        Nothing -> retainAfterFailure $ attemptedDiagnostics attempt
        Just session -> case refreshDjinnProjection state
            { exferenceRuntime = ExferenceRuntime
                { exferenceRuntimeRequestedTargets = attemptedTargets attempt
                , exferenceRuntimeWorkspace = attemptedWorkspace attempt
                , exferenceRuntimeScope = attemptedScope attempt
                , exferenceRuntimeAllowsFix = allowFix
                , exferenceRuntimeBaseSession = attemptedBaseSession attempt
                , exferenceRuntimeSession = Just session
                , exferenceRuntimeDiagnostics = attemptedDiagnostics attempt
                }
            } of
          Left failure -> retainAfterDjinnProjectionFailure state failure
          Right projected -> putStrLn successMessage >> pure projected
 where
  retainAfterFailure diagnostics = do
    putStrLn
      "Source workspace load failed; retaining previous sessions and settings."
    pure state
      { exferenceRuntime = (exferenceRuntime state)
          { exferenceRuntimeDiagnostics = diagnostics }
      }

addWorkspaceTargetsFor
  :: ReplState
  -> [String]
  -> IO (Either (NonEmpty Diagnostic) SourceWorkspace)
addWorkspaceTargetsFor state targets = case
    exferenceRuntimeWorkspace $ exferenceRuntime state of
  Nothing -> loadWorkspace targets
  Just workspace -> addWorkspaceTargets workspace targets

removeWorkspaceTargetsFor
  :: ReplState
  -> [String]
  -> IO (Either (NonEmpty Diagnostic) SourceWorkspace)
removeWorkspaceTargetsFor state targets = case
    exferenceRuntimeWorkspace $ exferenceRuntime state of
  Just workspace -> removeWorkspaceTargets workspace targets
  Nothing -> do
    empty <- loadWorkspace []
    case empty of
      Left failures -> pure $ Left failures
      Right workspace -> removeWorkspaceTargets workspace targets

reloadExferenceWorkspace :: ReplState -> IO ReplState
reloadExferenceWorkspace state = reloadExferenceWorkspaceWithPolicy
  (exferenceRuntimeAllowsFix $ exferenceRuntime state) state

reloadExferenceWorkspaceWithPolicy :: Bool -> ReplState -> IO ReplState
reloadExferenceWorkspaceWithPolicy allowFix state =
  updateExferenceWorkspaceWithPolicy allowFix
    action RefreshAutomaticScope
      ("Loaded source workspace: " ++ renderRequestedTargets
        (exferenceRuntimeRequestedTargets runtime))
      state
 where
  runtime = exferenceRuntime state
  action = case exferenceRuntimeWorkspace runtime of
    Just workspace -> reloadWorkspace workspace
    Nothing -> loadWorkspace $ exferenceRuntimeRequestedTargets runtime

renderRequestedTargets :: [String] -> String
renderRequestedTargets [] = "(no targets)"
renderRequestedTargets targets = unwords $ map show targets

addImportToScope :: FilePath -> String -> ReplState -> IO ReplState
addImportToScope sourceName source = modifyExferenceScope
  $ \inventory workspace context -> case
      addScopeImport inventory workspace source context of
    Left failure -> Left $ withSource sourceName failure
    Right next -> Right next

changeModuleScope
  :: ModuleChange
  -> [String]
  -> ReplState
  -> IO ReplState
changeModuleScope change modules = modifyExferenceScope
  $ \inventory workspace context ->
      changeScopeModules inventory workspace change modules context

-- Scope edits never reparse source and never mutate the retained base
-- session. Reprojection starts from that policy-filtered base, so a later
-- import can restore a binding hidden by an earlier context change.
modifyExferenceScope
  :: (ExferenceInventory
      -> SourceWorkspace
      -> ReplScope
      -> Either Diagnostic ReplScope)
  -> ReplState
  -> IO ReplState
modifyExferenceScope change state = case
    ( exferenceRuntimeWorkspace runtime
    , exferenceRuntimeBaseSession runtime
    ) of
  (Just workspace, Just baseSession) -> case initialScope workspace baseSession of
    Left failure -> reject failure
    Right context -> case change
        (exferenceSessionInventory baseSession) workspace context of
      Left failure -> reject failure
      Right next -> case ExferenceSession.scopeExferenceSession
          (Set.fromList $ scopeSearchNames next) baseSession of
        Left failure -> reject failure
        Right session -> case refreshDjinnProjection state
            { exferenceRuntime = runtime
                { exferenceRuntimeScope = Just next
                , exferenceRuntimeSession = Just session
                }
            } of
          Left failure -> retainAfterDjinnProjectionFailure state failure
          Right projected -> pure projected
  _ -> replFailure "DJEX_REPL_MODULE" "no source workspace is loaded"
      "use :load TARGET before changing imports or modules"
    >> pure state
 where
  runtime = exferenceRuntime state
  initialScope workspace baseSession = case exferenceRuntimeScope runtime of
    Just context -> Right context
    Nothing -> scopeFromWorkspace
      (exferenceSessionInventory baseSession) workspace
  reject failure = emitDiagnostic failure >> pure state

setOption :: String -> ReplState -> IO ReplState
setOption source state
  | null $ trim source = showSettings state >> pure state
  | otherwise = case settingInvocation source of
      Left failure -> settingFailure failure >> pure state
      Right (setting, value) -> applySetting setting value state

applySetting :: ReplSetting -> Maybe String -> ReplState -> IO ReplState
applySetting setting value state = case
    settingApply (settingBehavior setting) value of
  Left failure -> settingFailure failure >> pure state
  Right update -> update state

unsetOption :: String -> ReplState -> IO ReplState
unsetOption source state = case words $ normalize source of
  [rawName] -> case parseReplSetting rawName of
    Left failure -> settingFailure failure >> pure state
    Right setting -> settingReset (settingBehavior setting) state
  [] -> settingFailure "expected a setting name" >> pure state
  _ -> settingFailure "expected exactly one setting name" >> pure state

-- | One setting's complete behavior. Application, reset, and display are
-- projections of the same descriptor, so the three views cannot drift apart.
data SettingBehavior = SettingBehavior
  { settingApply :: Maybe String -> Either String (ReplState -> IO ReplState)
  , settingReset :: ReplState -> IO ReplState
  , settingRender :: ReplState -> String
  }

settingBehavior :: ReplSetting -> SettingBehavior
settingBehavior setting = case setting of
  BackendSetting -> fieldSetting setting (const parseReplBackend)
    activeBackends
    (\value state -> state {activeBackends = value})
    (replInitialBackend defaultReplOptions)
    replBackendName
  -- The built-in target spelling is validated at startup, but restoring it
  -- still reports the impossible failure instead of inventing a fallback.
  TargetSetting -> SettingBehavior
    { settingApply = requiredValue setting $ \source ->
        (\target state -> pure state {resultTarget = target})
          <$> checkedTarget source
    , settingReset = \state -> case defaultTarget of
        Left failure -> emitDiagnostic failure >> pure state
        Right target -> pure state {resultTarget = target}
    , settingRender = definitionSpelling . resultTarget
    }
  SelectionSetting -> presentationSetting setting parseSelectionMode
    presentationSelection
    (\value options -> options {presentationSelection = value})
    selectionModeName
  RenderingSetting -> presentationSetting setting parseRenderMode
    presentationRenderMode
    (\value options -> options {presentationRenderMode = value})
    renderModeName
  QualificationSetting -> presentationSetting setting parseQualification
    presentationQualification
    (\value options -> options {presentationQualification = value})
    qualificationName
  PromptSetting -> fieldSetting setting
    (\_ source -> Right $ decodeString source)
    promptTemplate
    (\value state -> state {promptTemplate = value})
    defaultPromptTemplate
    show
  CandidateLimitSetting -> fieldSetting setting positiveInt
    (optionCutoff . djinnSearchOptions)
    (\value -> onDjinnOptions $ \options -> options {optionCutoff = value})
    (optionCutoff defaultQueryOptions)
    show
  ChoiceBudgetSetting -> fieldSetting setting parseChoiceBudget
    (optionBudget . djinnSearchOptions)
    (\value -> onDjinnOptions $ \options -> options {optionBudget = value})
    (optionBudget defaultQueryOptions)
    (maybe "0" show)
  -- Axiom projection changes the sealed Djinn session, so applying or
  -- resetting it reprojects the scope instead of updating a plain field.
  DjinnAxiomsSetting -> SettingBehavior
    { settingApply = fmap applyDjinnAxiomPolicy
        . parseBoolean (replSettingName setting)
    , settingReset = applyDjinnAxiomPolicy False
    , settingRender = booleanName . (== IncludeDjinnAxioms)
        . djinnAxiomPolicy . djinnRuntime
    }
  AllowUnusedSetting -> exferenceBooleanSetting setting
    exferenceAllowUnused
    (\value options -> options {exferenceAllowUnused = value})
  AllowConstraintsSetting -> exferenceBooleanSetting setting
    exferenceAllowResidualConstraints
    (\value options -> options {exferenceAllowResidualConstraints = value})
  MultiConstructorPatternsSetting -> exferenceBooleanSetting setting
    exferenceMultiConstructorPatterns
    (\value options -> options {exferenceMultiConstructorPatterns = value})
  ConstraintDeferralStepsSetting -> exferenceFieldSetting setting
    nonNegativeInt
    exferenceConstraintDeferralSteps
    (\value options -> options {exferenceConstraintDeferralSteps = value})
    show
  MaximumStepsSetting -> exferenceFieldSetting setting positiveInt
    exferenceMaximumSteps
    (\value options -> options {exferenceMaximumSteps = value})
    show
  MaximumQueueSetting -> exferenceFieldSetting setting boundedNonNegativeInt
    exferenceMaximumQueueSize
    (\value options -> options {exferenceMaximumQueueSize = value})
    renderBounded
  MaximumDepthSetting -> exferenceFieldSetting setting boundedPenalty
    exferenceMaximumDepth
    (\value options -> options {exferenceMaximumDepth = value})
    renderBounded
  -- The fix policy is baked into the sealed Exference session, so changing
  -- or resetting it reloads the workspace instead of updating a plain field.
  FixSetting -> SettingBehavior
    { settingApply = fmap applyFixPolicy
        . parseBoolean (replSettingName setting)
    , settingReset = applyFixPolicy False
    , settingRender =
        booleanName . exferenceRuntimeAllowsFix . exferenceRuntime
    }

applyFixPolicy :: Bool -> ReplState -> IO ReplState
applyFixPolicy allowFix state
  | allowFix == exferenceRuntimeAllowsFix (exferenceRuntime state) = pure state
  | otherwise = reloadExferenceWorkspaceWithPolicy allowFix state

applyDjinnAxiomPolicy :: Bool -> ReplState -> IO ReplState
applyDjinnAxiomPolicy enabled state
  | policy == djinnAxiomPolicy djinn = pure state
  | otherwise = case refreshDjinnProjection state
      {djinnRuntime = djinn {djinnAxiomPolicy = policy}} of
        Left failure -> retainAfterDjinnProjectionFailure state failure
        Right projected -> pure projected
 where
  djinn = djinnRuntime state
  policy = if enabled then IncludeDjinnAxioms else ExcludeDjinnAxioms

parseChoiceBudget :: String -> String -> Either String (Maybe Integer)
parseChoiceBudget name source = do
  budget <- nonNegativeInteger name source
  Right $ if budget == 0 then Nothing else Just budget

-- | The common setting shape: parse a required value, store it in a state
-- field, restore a built-in default, and render the current value.
fieldSetting
  :: ReplSetting
  -> (String -> String -> Either String value)
  -> (ReplState -> value)
  -> (value -> ReplState -> ReplState)
  -> value
  -> (value -> String)
  -> SettingBehavior
fieldSetting setting parser get set defaultValue render = SettingBehavior
  { settingApply = requiredValue setting $ \source ->
      (\parsed -> pure . set parsed) <$> parser (replSettingName setting) source
  , settingReset = pure . set defaultValue
  , settingRender = render . get
  }

-- | Reject a missing setting value before invoking a value parser.
requiredValue
  :: ReplSetting
  -> (String -> Either String (ReplState -> IO ReplState))
  -> Maybe String
  -> Either String (ReplState -> IO ReplState)
requiredValue setting _ Nothing = Left
  $ "setting " ++ replSettingName setting ++ " requires a value"
requiredValue _ parse (Just source) = parse source

presentationSetting
  :: ReplSetting
  -> (String -> String -> Either String value)
  -> (PresentationOptions -> value)
  -> (value -> PresentationOptions -> PresentationOptions)
  -> (value -> String)
  -> SettingBehavior
presentationSetting setting parser get set = fieldSetting setting parser
  (get . presentation)
  (\value state -> state {presentation = set value $ presentation state})
  (get defaultInteractivePresentationOptions)

exferenceFieldSetting
  :: ReplSetting
  -> (String -> String -> Either String value)
  -> (ExferenceOptions -> value)
  -> (value -> ExferenceOptions -> ExferenceOptions)
  -> (value -> String)
  -> SettingBehavior
exferenceFieldSetting setting parser get set = fieldSetting setting parser
  (get . exferenceSearchOptions)
  (\value -> onExferenceOptions $ set value)
  (get defaultExferenceOptions)

exferenceBooleanSetting
  :: ReplSetting
  -> (ExferenceOptions -> Bool)
  -> (Bool -> ExferenceOptions -> ExferenceOptions)
  -> SettingBehavior
exferenceBooleanSetting setting get set = SettingBehavior
  { settingApply = \value -> (\enabled -> pure . update enabled)
      <$> parseBoolean (replSettingName setting) value
  , settingReset = pure . update (get defaultExferenceOptions)
  , settingRender = booleanName . get . exferenceSearchOptions
  }
 where
  update value = onExferenceOptions $ set value

onDjinnOptions :: (QueryOptions -> QueryOptions) -> ReplState -> ReplState
onDjinnOptions update state =
  state {djinnSearchOptions = update $ djinnSearchOptions state}

onExferenceOptions
  :: (ExferenceOptions -> ExferenceOptions) -> ReplState -> ReplState
onExferenceOptions update state =
  state {exferenceSearchOptions = update $ exferenceSearchOptions state}

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
    normalized = normalize rawName

  optionalRemainder name value = case trim $ drop (length name) value of
    "" -> Nothing
    remainder -> Just remainder

parseBoolean :: String -> Maybe String -> Either String Bool
parseBoolean name source = case fmap normalize source of
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
showState (Just rawSubject) = case words $ normalize rawSubject of
  ["settings"] -> showSettings
  ["backends"] -> showBackends
  ["environment"] -> showEnvironmentSummary
  ["imports"] -> showImports
  ["modules"] -> showModules
  ["targets"] -> showTargets
  ["omissions"] -> showOmissions
  ["diagnostics"] -> showLoadDiagnostics
  ["directory"] -> const $ getCurrentDirectory >>= putStrLn
  [] -> showSettings
  _ -> \_ -> settingFailure $ "unknown :show subject " ++ show rawSubject

showSettings :: ReplState -> IO ()
showSettings state = putStr $ unlines
  $ map render [minBound .. maxBound]
  ++ ["targets = " ++ renderRequestedTargets
        (exferenceRuntimeRequestedTargets $ exferenceRuntime state)]
 where
  render setting = replSettingName setting ++ " = "
    ++ renderSettingValue state setting

renderSettingValue :: ReplState -> ReplSetting -> String
renderSettingValue state setting = settingRender (settingBehavior setting) state

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
  putStrLn $ "Djinn: " ++ djinnEnvironmentSummary state
  let runtime = exferenceRuntime state
  putStrLn $ "Exference: " ++ case exferenceRuntimeSession runtime of
    Nothing -> "unavailable (last attempted "
      ++ renderRequestedTargets (exferenceRuntimeRequestedTargets runtime)
      ++ ")"
    Just session -> declarationCount (exferenceSessionEnvironment session)
      ++ " declarations from "
      ++ renderRequestedTargets (exferenceRuntimeRequestedTargets runtime)
 where
  declarationCount = show . length . environmentDeclarations

showImports :: ReplState -> IO ()
showImports state = case exferenceRuntimeScope $ exferenceRuntime state of
  Nothing -> putStrLn "No source module context."
  Just context -> case renderScopeImports context of
    [] -> putStrLn "(no imports)"
    imports -> mapM_ putStrLn imports

showModules :: ReplState -> IO ()
showModules state = case exferenceRuntimeWorkspace $ exferenceRuntime state of
  Nothing -> putStrLn "No source modules are loaded."
  Just workspace -> case workspaceModules workspace of
    [] -> putStrLn "(no modules loaded)"
    modules -> forM_ modules $ \modul -> putStrLn
      $ workspaceModuleName modul ++ " (" ++ workspaceModulePath modul ++ ")"

showTargets :: ReplState -> IO ()
showTargets state = case exferenceRuntimeWorkspace $ exferenceRuntime state of
  Nothing -> putStrLn "No source targets are loaded."
  Just workspace -> case workspaceTargets workspace of
    [] -> putStrLn "(no targets)"
    targets -> mapM_ (putStrLn . workspaceTargetDisplay) targets

showOmissions :: ReplState -> IO ()
showOmissions state = do
  case djinnProjection $ djinnRuntime state of
    Nothing -> putStrLn
      "-- Djinn: standard checked environment (nothing projected)"
    Just projection -> do
      putStrLn "-- Djinn scope projection"
      case djinnProjectionOmissions projection of
        [] -> putStrLn "(no omissions)"
        omissions -> mapM_ (putStrLn . renderDjinnScopeOmission) omissions
  putStrLn "-- Exference"
  case exferenceRuntimeSession $ exferenceRuntime state of
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
  [] -> putStrLn "No source load diagnostics."
  diagnostics -> mapM_ (putStrLn . renderDiagnostic) diagnostics

browseState :: Maybe String -> ReplState -> IO ()
browseState (Just reference) state = browseWorkspaceModule reference state
browseState Nothing state = forSelectedBackends state $ \selectedBackend ->
  case selectedBackend of
    DjinnBackend -> browse "Djinn" id
      $ djinnSessionEnvironment $ currentDjinnSession state
    ExferenceBackend -> case
        ( exferenceRuntimeBaseSession runtime
        , exferenceRuntimeScope runtime
        ) of
      (Just session, Just context) -> do
        browseNames "Current source scope"
          ExferenceType.defaultVariableName
          (scopeBrowseNames context)
          $ exferenceSessionEnvironment session
        when (not $ null $ exferenceSessionOmissions session) $ putStrLn
          "-- Some loaded capabilities are not searchable; use :show omissions."
      _ -> putStrLn "Exference is unavailable."
 where
  runtime = exferenceRuntime state
  scopeBrowseNames context = scopeUnqualifiedNames context ++
    [ name
    | name <- scopeSearchNames context
        ++ concatMap snd (scopeQualifiedNames context)
    , name `notElem` scopeUnqualifiedNames context
    ]

browseWorkspaceModule :: String -> ReplState -> IO ()
browseWorkspaceModule reference state = case
    ( exferenceRuntimeWorkspace runtime
    , exferenceRuntimeBaseSession runtime
    ) of
  (Just workspace, Just session) -> case splitModuleStar reference of
    (starred, moduleSource) -> case moduleNamesForBrowse
        (exferenceSessionInventory session) workspace starred moduleSource of
      Left failure -> emitDiagnostic failure
      Right names -> browseNames
        ("Source module " ++ (if starred then "*" else "") ++ moduleSource)
        ExferenceType.defaultVariableName names
        $ exferenceSessionEnvironment session
  _ -> replFailure "DJEX_REPL_MODULE" "no source workspace is loaded"
    "use :load TARGET before browsing a module"
 where
  runtime = exferenceRuntime state

splitModuleStar :: String -> (Bool, String)
splitModuleStar ('*' : source) = (True, source)
splitModuleStar source = (False, source)

showInfo :: ReplState -> String -> IO ()
showInfo state source = case parseName $ trim source of
  Left failure -> settingFailure (renderNameError failure)
  Right parsedName -> case
      ( exferenceRuntimeSession runtime
      , exferenceRuntimeScope runtime
      , djinnProjection $ djinnRuntime state
      ) of
    (Just session, Just context, Just projection) ->
      -- Interactive scoping narrows only Exference's private search view;
      -- this public environment remains the complete checked source
      -- inventory. It is therefore the authoritative candidate set for both
      -- backends. Djinn-only repair stubs are projection details rather than
      -- prompt-scope declarations and remain visible through :show
      -- environment, not as manufactured ReplScope bindings.
      showResolvedProjection projection session context parsedName
    (Just session, Just context, Nothing) ->
      -- Initial loading may retain the historical standard Djinn session if
      -- its source projection fails. In that recovery state Djinn has no
      -- canonical-to-prompt map, so it must keep direct standard-session name
      -- lookup while Exference continues to follow the loaded prompt scope.
      forSelectedBackends state $ \case
        DjinnBackend -> showDjinnInfo parsedName
        ExferenceBackend -> withResolvedScope session context parsedName
          $ showExferenceInfo session
    _ -> forSelectedBackends state $ \case
      -- With no source scope, Djinn retains its historical standard session
      -- and therefore its ordinary unqualified spelling rules.
      DjinnBackend -> showDjinnInfo parsedName
      ExferenceBackend -> putStrLn "Exference is unavailable."
 where
  runtime = exferenceRuntime state

  showResolvedProjection projection session context parsedName =
    let environment = exferenceSessionEnvironment session
    in case resolveScopeNameAmong
        AnyScope (declarationNameSet environment) context parsedName of
      Left failure -> case activeBackends state of
        -- The historical Djinn inspector reports an ordinary missing
        -- declaration on an unknown name. Keep that contract in Djinn-only
        -- mode; aliases and qualified source identities still take the shared
        -- successful-resolution path below. Exference and both mode retain
        -- prompt-scope errors because they have a shared source scope to
        -- enforce.
        OneBackend DjinnBackend -> showDjinnInfo parsedName
        _ -> settingFailure failure
      Right canonicalName -> forSelectedBackends state
        $ showProjectedInfo projection session environment canonicalName

  withResolvedScope session context parsedName action =
    let environment = exferenceSessionEnvironment session
    in case resolveScopeNameAmong
        AnyScope (declarationNameSet environment) context parsedName of
      Left failure -> settingFailure failure
      Right canonicalName -> action environment canonicalName

  showProjectedInfo projection session environment canonicalName = \case
    DjinnBackend -> showDjinnInfo $ Map.findWithDefault canonicalName
      canonicalName $ djinnProjectionPromptNames projection
    ExferenceBackend -> showExferenceInfo session environment canonicalName

  showExferenceInfo session environment canonicalName = do
    let matchingNames = canonicalName : map declarationSubjectName
          (matchingDeclarations canonicalName environment)
    info "Exference loaded declarations"
      ExferenceType.defaultVariableName canonicalName
      environment
    mapM_ (putStrLn . ("-- search omission: " ++) . renderOmission)
      $ filter ((`elem` matchingNames) . omittedName)
      $ exferenceSessionOmissions session

  showDjinnInfo name = info "Djinn" id name
    $ djinnSessionEnvironment $ currentDjinnSession state

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

browseNames
  :: String
  -> (variable -> String)
  -> [Name]
  -> Environment variable Void ()
  -> IO ()
browseNames label variableName names environment = do
  putStrLn $ "-- " ++ label
  case filter definesVisible $ environmentDeclarations environment of
    [] -> putStrLn "(no declarations)"
    declarations -> mapM_ (putStrLn . renderVisibleDeclaration)
      declarations
 where
  visible = Set.fromList names
  definesVisible declaration = any (`declarationMatchesName` declaration)
    $ Set.toList visible
  renderVisibleDeclaration declaration = renderDeclaration variableName
    $ case declaration of
      DataTypeDeclaration annotation name parameters constructors ->
        DataTypeDeclaration annotation name parameters
          $ filter ((`Set.member` visible) . constructorName) constructors
      ClassDeclaration annotation name parameters superclasses methods ->
        ClassDeclaration annotation name parameters superclasses
          $ filter ((`Set.member` visible) . valueName) methods
      _ -> declaration

info
  :: String
  -> (variable -> String)
  -> Name
  -> Environment variable Void ()
  -> IO ()
info label variableName name environment = do
  putStrLn $ "-- " ++ label
  case (declarations, relatedInstances) of
    ([], []) -> putStrLn $ "No declaration for " ++ renderCanonical name
    (matching, instances) -> do
      mapM_ (putStrLn . renderDeclaration variableName) matching
      mapM_ (putStrLn . renderDeclaration variableName) instances
 where
  declarations = matchingDeclarations name environment
  owners = Set.fromList $ mapMaybe declarationOwner declarations

  -- Asking about a constructor or class method finds its owning declaration.
  -- Instances participate through that datatype or class, not through the
  -- member's value name. Owner queries already include class instances in
  -- 'declarations', so the lookup predicate also prevents duplicate output.
  declarationOwner declaration = case declaration of
    TypeSynonymDeclaration _ owner _ _ -> Just owner
    DataTypeDeclaration _ owner _ _ -> Just owner
    AbstractTypeDeclaration _ owner _ -> Just owner
    ClassDeclaration _ owner _ _ _ -> Just owner
    ValueDeclaration _ -> Nothing
    InstanceDeclaration _ _ _ _ -> Nothing

  -- GHCi's :info lists the instances a name participates in. Instances whose
  -- subject class is the name itself already match as declarations.
  relatedInstances = filter mentions $ environmentDeclarations environment
  mentions declaration = case declaration of
    InstanceDeclaration _ _ _ headConstraint ->
      not (declarationMatchesName name declaration)
        && ( constraintClass headConstraint `Set.member` owners
          || any (not . Set.disjoint owners . typeConstructors)
              (constraintArguments headConstraint)
           )
    _ -> False

matchingDeclarations
  :: Name
  -> Environment variable kind annotation
  -> [Declaration variable kind annotation]
matchingDeclarations name = filter (declarationMatchesName name)
  . environmentDeclarations

-- Constructors and class methods find their owner, while an instance is found
-- through its head class. The neutral environment indexes all three beneath
-- one declaration rather than manufacturing separate lookup records.
declarationMatchesName
  :: Name
  -> Declaration variable kind annotation
  -> Bool
declarationMatchesName name declaration =
  name `elem` declarationLookupNames declaration

declarationNameSet
  :: Environment variable kind annotation
  -> Set.Set Name
declarationNameSet = Set.fromList
  . concatMap declarationLookupNames
  . environmentDeclarations

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
    "type " ++ renderCanonical name ++ " :: " ++ renderGroundKind kind
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
      ++ " :: " ++ renderGroundKind kind ++ ")"
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
runScript = runScriptWith $ const $ pure ()

-- Announce startup files only after path resolution, strict reading, and
-- whole-script parsing have succeeded. This keeps a failed candidate from
-- being presented as loaded while retaining the useful pre-execution notice.
runScriptWithAnnouncement
  :: FilePath -> [String] -> ReplState -> IO (ReplStep ReplState)
runScriptWithAnnouncement = runScriptWith $ \canonical ->
  putStrLn $ "Loaded startup commands from " ++ canonical

runScriptWith
  :: (FilePath -> IO ())
  -> FilePath
  -> [String]
  -> ReplState
  -> IO (ReplStep ReplState)
runScriptWith announce path history state = do
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
              Right inputs -> do
                announce canonical
                runInputs canonical history
                  state {scriptStack = canonical : scriptStack state} inputs

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
