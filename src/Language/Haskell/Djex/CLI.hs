-- | The shared interactive and one-shot @djex@ command.
--
-- Living in the library keeps this frontend in-process testable and matches
-- the two historical launchers, whose executables are equally thin wrappers
-- over "Djinn" and "Language.Haskell.Exference.CLI".
module Language.Haskell.Djex.CLI
  ( main
  , runArguments
  ) where

import Data.Foldable (toList)
import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.Version (showVersion)
import System.Console.GetOpt
  ( ArgDescr (NoArg, ReqArg)
  , ArgOrder (Permute)
  , OptDescr (Option)
  , getOpt
  , usageInfo
  )
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (tryIOError)

import Language.Haskell.Djex
import Language.Haskell.Djex.Command
import Language.Haskell.Djex.HaskellSrc (parseSourceTypeInScope)
import Language.Haskell.Djex.REPL.DjinnScope
  ( DjinnAxiomPolicy (IncludeDjinnAxioms), DjinnProjection (..)
  , projectDjinnScope, renderDjinnScopeOmission )
import qualified Language.Haskell.Djex.Exference.Internal.Session as ExferenceSession
import qualified Language.Haskell.Exference.Core.Types as ExferenceType
import qualified Language.Haskell.Djex.REPL.Scope as Scope
import qualified Language.Haskell.Djex.REPL.Workspace as Workspace
import Language.Haskell.Djex.Exference.HaskellSrc
  ( ExferenceSessionLoadReport (..)
  , defaultExferenceEnvironmentPath
  , exferenceCommandSessionPolicy
  , loadExferenceSessionWithPolicy
  , loadExferenceSessionFromSourcesWithTypeVisibilityWithPolicy
  )
import Language.Haskell.Djex.Package
  ( PackageOperation (..)
  , PackageInstallMode (..)
  , packageOperationName
  , parsePackageInstall
  , runPackageOperation
  , validatePackageTargets
  )
import Language.Haskell.Djex.REPL
import Language.Haskell.Djex.REPL.BehavioralWorker (runBehavioralWorker)
import Language.Haskell.Djex.REPL.Command
  ( ModuleChange (ReplaceModules)
  , parseReplBackend
  , replBackendName
  )
import Paths_djex (version)

data Flag
  = TargetFlag String
  | SelectionFlag String
  | RankingFlag String
  | ProviderCostFlag String
  | QualityWindowFlag String
  | RenderFlag String
  | QualificationFlag String
  | CandidateLimitFlag String
  | ChoiceBudgetFlag String
  | EnvironmentFlag FilePath
  | AllowUnusedFlag
  | AllowConstraintsFlag
  | ConstraintDeferralStepsFlag String
  | MultiConstructorPatternsFlag
  | MaximumStepsFlag String
  | MaximumQueueFlag String
  | MaximumDepthFlag String
  | AllowFixFlag
  | ReplBackendFlag String
  | HistoryFlag FilePath
  | IgnoreStartupFlag
  deriving (Eq, Show)

data CommonOptions = CommonOptions
  { commonTarget :: DefinitionName
  , commonPresentation :: PresentationOptions
  , commonInput :: String
  }

data DjinnOptions = DjinnOptions
  { djinnCommon :: CommonOptions
  , djinnEnvironment :: Maybe FilePath
  , djinnCandidateLimit :: Int
  , djinnChoiceBudget :: Maybe Integer
  }

data ExferenceCliOptions = ExferenceCliOptions
  { exferenceCommon :: CommonOptions
  , exferenceEnvironment :: Maybe FilePath
  , exferenceAllowFix :: Bool
  , exferenceSearchOptions :: ExferenceOptions
  }

-- | Process entry point. This delegates to 'runArguments' and terminates the
-- process with the returned status.
main :: IO ()
main = getArgs >>= runArguments >>= exitWith

-- | Run the complete @djex@ command without calling 'exitWith'. The returned
-- status has the same meaning as the executable's status.
--
-- This is an in-process command frontend, not a pure argument parser. It uses
-- the process streams and working directory and, depending on the arguments
-- or interactive input, may read startup files, evaluate Haskell with GHC,
-- launch an editor or shell, and run Cabal. Embed the checked backend adapters
-- instead when the host must own those effects.
runArguments :: [String] -> IO ExitCode
runArguments arguments = case arguments of
  ["--internal-behavioral-worker"] -> runBehavioralWorker
  ["--help"] -> putStrLn fullUsage >> pure ExitSuccess
  ["-h"] -> putStrLn fullUsage >> pure ExitSuccess
  ["--version"] -> do
    putStrLn $ "djex version " ++ showVersion version
    pure ExitSuccess
  ["-V"] -> do
    putStrLn $ "djex version " ++ showVersion version
    pure ExitSuccess
  ["djinn", "--help"] ->
    putStrLn (backendUsage DjinnBackend) >> pure ExitSuccess
  ["djinn", "-h"] ->
    putStrLn (backendUsage DjinnBackend) >> pure ExitSuccess
  ["exference", "--help"] ->
    putStrLn (backendUsage ExferenceBackend) >> pure ExitSuccess
  ["exference", "-h"] ->
    putStrLn (backendUsage ExferenceBackend) >> pure ExitSuccess
  ["download", "--help"] ->
    putStrLn (packageUsage DownloadOperation) >> pure ExitSuccess
  ["download", "-h"] ->
    putStrLn (packageUsage DownloadOperation) >> pure ExitSuccess
  ["install", "--help"] ->
    putStrLn (packageUsage $ InstallOperation InstallExecutables)
      >> pure ExitSuccess
  ["install", "-h"] ->
    putStrLn (packageUsage $ InstallOperation InstallExecutables)
      >> pure ExitSuccess
  ["repl", "--help"] -> putStrLn replUsage >> pure ExitSuccess
  ["repl", "-h"] -> putStrLn replUsage >> pure ExitSuccess
  [] -> runRepl defaultReplOptions
  "repl" : replArguments ->
    runParsed (parseReplOptions replArguments) runRepl
  "djinn" : backendArguments ->
    runParsed (parseDjinnOptions backendArguments) runDjinn
  "exference" : backendArguments ->
    runParsed (parseExferenceOptions backendArguments) runExference
  "download" : packageArguments ->
    runDownloadArguments packageArguments
  "install" : packageArguments ->
    runInstallArguments packageArguments
  commandArgument : _ ->
    usageFailure $ "unknown command " ++ show commandArgument

runParsed :: Either String options -> (options -> IO ExitCode) -> IO ExitCode
runParsed parsed action = either usageFailure action parsed

runDownloadArguments :: [String] -> IO ExitCode
runDownloadArguments rawArguments = case validatePackageTargets arguments of
  Left failure -> packageUsageFailure DownloadOperation failure
  Right targets -> runPackageOperation DownloadOperation targets
 where
  arguments = case rawArguments of
    "--" : targets -> targets
    targets -> targets

runInstallArguments :: [String] -> IO ExitCode
runInstallArguments arguments = case parsePackageInstall arguments of
  Left failure -> packageUsageFailure
    (InstallOperation InstallExecutables) failure
  Right (mode, targets) -> runPackageOperation (InstallOperation mode) targets

packageUsageFailure :: PackageOperation -> String -> IO ExitCode
packageUsageFailure operation failure = usageFailure
  $ packageOperationName operation ++ ": " ++ failure

parseReplOptions :: [String] -> Either String ReplOptions
parseReplOptions arguments = case
    getOpt Permute replOptionDescriptors arguments of
  (flags, [], []) -> do
    rawBackend <- uniqueValue "--backend" replBackendValue
      (replBackendName $ replInitialBackend defaultReplOptions) flags
    selectedBackend <- parseReplBackend rawBackend
    environment <- optionalUniqueValue
      "--environment" environmentValue flags
    allowFix <- uniqueSwitch "--fix" (== AllowFixFlag) flags
    history <- optionalUniqueValue "--history" historyValue flags
    ignoreStartup <- uniqueSwitch "--ignore-startup"
      (== IgnoreStartupFlag) flags
    pure defaultReplOptions
      { replInitialBackend = selectedBackend
      , replEnvironmentPath = environment
      , replAllowFix = allowFix
      , replHistoryFile = history
      , replIgnoreStartupFiles = ignoreStartup
      }
  (_, positional, []) -> Left $ "repl takes no positional arguments, but got "
    ++ show (length positional)
  (_, _, errors) -> Left $ concat errors

runDjinn :: DjinnOptions -> IO ExitCode
runDjinn options | Just directory <- djinnEnvironment options =
  withCommandEnvironment False directory $ \workspace sourceSession context -> do
    let inventory = exferenceSessionInventory sourceSession
        declarations = map (mapDeclarationTypeVariables ExferenceType.defaultVariableName) $
          environmentDeclarations $ exferenceSessionEnvironment sourceSession
        units = Set.fromList
          [name | DataTypeDeclaration _ name [] [DataConstructor _ constructor []] <- declarations,
            name == constructor, nameSpecial name == Just (TupleConstructor Boxed 0)]
        lists = [(name, [zero, step]) |
          DataTypeDeclaration _ name [TypeParameter parameter explicitKind]
            [DataConstructor _ zero [], DataConstructor _ step
              [TypeVariable element, TypeApplication (TypeConstructor recursiveHead)
                (TypeVariable recursiveElement)]] <- declarations,
          name == listName, zero == listName, step == consName,
          maybe True (== ProperTypeKind) explicitKind,
          element == parameter, recursiveHead == name, recursiveElement == parameter]
        typeNames = Set.unions [units, Set.fromList $ map fst lists,
          Set.fromList $ Scope.scopeUnqualifiedTypeNames context]
        valueNames = Set.unions [units, Set.fromList $ concatMap snd lists,
          Set.fromList $ Scope.scopeUnqualifiedValueNames context]
    case projectDjinnScope IncludeDjinnAxioms
        (Scope.workspaceRecordProjections inventory workspace)
        (typeConstructorKinds $ inventoryKindAssumptions inventory)
        (ExferenceSession.sessionRecursiveDataTypeNames sourceSession)
        declarations typeNames valueNames of
      Left failure -> diagnosticFailure failure
      Right projection -> do
        mapM_ (hPutStrLn stderr . ("Djinn source omission: " ++) . renderDjinnScopeOmission) $
          djinnProjectionOmissions projection
        case parseSourceTypeInScope inventory (Scope.scopeExferenceQueryScope context)
            "<command-line>" $ commonInput common of
          Left failure -> diagnosticFailure failure
          Right parsed -> executeDjinnSourceCommand
            (commonPresentation common) (djinnProjectionFieldSelectors projection)
            (djinnProjectionSession projection) (djinnQueryOptions options)
            (commonTarget common) (djinnProjectionPromptNames projection)
            (commonInput common) parsed
 where
  common = djinnCommon options
runDjinn options = case standardDjinnSession of
  Left failure -> diagnosticFailure failure
  Right session -> executeDjinnCommand
    (commonPresentation common)
    noFieldSelectors
    session
    (djinnQueryOptions options)
    (commonTarget common)
    "<command-line>"
    source
 where
  common = djinnCommon options
  source = commonInput common

runExference :: ExferenceCliOptions -> IO ExitCode
runExference options | Just directory <- exferenceEnvironment options =
  withCommandEnvironment (exferenceAllowFix options) directory $ \_ base context ->
    case ExferenceSession.scopeExferenceSession
        (Set.fromList $ Scope.scopeSearchNames context) base of
      Left failure -> diagnosticFailure failure
      Right session -> executeExferenceCommandInScope
        (commonPresentation common) noFieldSelectors session
        (exferenceSearchOptions options) (commonTarget common)
        (Scope.scopeExferenceQueryScope context) "<command-line>" (commonInput common)
 where
  common = exferenceCommon options
runExference options = case
    exferenceCommandSessionPolicy (exferenceAllowFix options) of
  Left failure -> diagnosticFailure failure
  Right policy -> do
    environmentPath <- maybe defaultExferenceEnvironmentPath pure
      $ exferenceEnvironment options
    report <- loadExferenceSessionWithPolicy policy environmentPath
    -- The compatibility loader records progress counters as Info. A one-shot
    -- compiler-like command stays quiet on success while retaining warnings
    -- about omissions, defaults, and recoverable source problems.
    mapM_ emitDiagnostic
      $ filter ((/= Info) . diagnosticSeverity)
      $ exferenceSessionLoadDiagnostics report
    case exferenceSessionLoadResult report of
      Left failures -> do
        mapM_ emitDiagnostic $ toList failures
        pure runtimeFailure
      Right session -> executeExferenceCommand
        (commonPresentation common)
        noFieldSelectors
        session
        (exferenceSearchOptions options)
        (commonTarget common)
        "<command-line>"
        source
 where
  common = exferenceCommon options
  source = commonInput common

-- A custom source environment is a set of imported modules. Use their checked
-- export/import scope, including type-visibility snapshots, just as the REPL
-- does. Loading declarations alone would expose private constructors/methods.
withCommandEnvironment
  :: Bool -> FilePath
  -> (Workspace.SourceWorkspace -> ExferenceSession -> Scope.ReplScope -> IO ExitCode)
  -> IO ExitCode
withCommandEnvironment allowFix directory action = do
  inspected <- tryIOError $ listDirectory directory
  case inspected of
    Left failure -> diagnosticFailure $ withSource directory $
      contextualDiagnostic Error "EXF_ENV_DIRECTORY_READ"
        ("cannot read environment directory: " ++ directory) (show failure)
    Right _ -> do
      loaded <- Workspace.loadWorkspace [directory]
      case loaded of
        Left failures -> mapM_ emitDiagnostic (toList failures) >> pure runtimeFailure
        Right workspace -> case exferenceCommandSessionPolicy allowFix of
          Left failure -> diagnosticFailure failure
          Right policy -> do
            report <- loadExferenceSessionFromSourcesWithTypeVisibilityWithPolicy policy
              (Workspace.workspaceModuleSources workspace)
              (Workspace.workspaceRatingSources workspace)
              (Workspace.workspaceTypeVisibilitySources workspace)
            mapM_ emitDiagnostic $ filter ((/= Info) . diagnosticSeverity) $
              exferenceSessionLoadDiagnostics report
            case exferenceSessionLoadResult report of
              Left failures -> mapM_ emitDiagnostic (toList failures) >> pure runtimeFailure
              Right session -> case Scope.scopeFromWorkspace (exferenceSessionInventory session) workspace of
                Left failure -> diagnosticFailure failure
                Right initial -> case Scope.changeScopeModules (exferenceSessionInventory session)
                    workspace ReplaceModules
                    (map Workspace.workspaceModuleName $ Workspace.workspaceModules workspace) initial of
                  Left failure -> diagnosticFailure failure
                  Right context -> action workspace session context

djinnQueryOptions :: DjinnOptions -> QueryOptions
djinnQueryOptions options = defaultQueryOptions
  { optionCutoff = djinnCandidateLimit options
  , optionBudget = djinnChoiceBudget options
  , optionRanking = presentationRanking $ commonPresentation $ djinnCommon options
  , optionProviderCosts = presentationProviderCosts $ commonPresentation $ djinnCommon options
  }

parseDjinnOptions :: [String] -> Either String DjinnOptions
parseDjinnOptions arguments = do
  (flags, source) <- parseOptions DjinnBackend arguments
  common <- parseCommonOptions flags source
  environment <- optionalUniqueValue "--environment" environmentValue flags
  candidateLimit <- uniqueValue "--candidate-limit" candidateLimitValue
    (show defaultDjinnCandidateLimit) flags >>= positiveInt "--candidate-limit"
  rawBudget <- uniqueValue "--choice-budget" choiceBudgetValue
    defaultDjinnChoiceBudget flags
  budget <- nonNegativeInteger "--choice-budget" rawBudget
  pure DjinnOptions
    { djinnCommon = common
    , djinnEnvironment = environment
    , djinnCandidateLimit = candidateLimit
    , djinnChoiceBudget = if budget == 0 then Nothing else Just budget
    }

parseExferenceOptions :: [String] -> Either String ExferenceCliOptions
parseExferenceOptions arguments = do
  (flags, source) <- parseOptions ExferenceBackend arguments
  common <- parseCommonOptions flags source
  environment <- optionalUniqueValue "--environment" environmentValue flags
  allowUnused <- uniqueSwitch "--allow-unused" (== AllowUnusedFlag) flags
  allowConstraints <- uniqueSwitch
    "--allow-constraints" (== AllowConstraintsFlag) flags
  multiConstructorPatterns <- uniqueSwitch
    "--multi-constructor-patterns" (== MultiConstructorPatternsFlag) flags
  allowFix <- uniqueSwitch "--fix" (== AllowFixFlag) flags
  deferral <- uniqueValue
    "--constraint-deferral-steps" constraintDeferralValue
    (show $ exferenceConstraintDeferralSteps defaults) flags
    >>= nonNegativeInt "--constraint-deferral-steps"
  maximumSteps <- uniqueValue "--max-steps" maximumStepsValue
    (show $ exferenceMaximumSteps defaults) flags
    >>= positiveInt "--max-steps"
  maximumQueue <- uniqueValue "--max-queue" maximumQueueValue
    (maybe "unbounded" show $ exferenceMaximumQueueSize defaults) flags
    >>= boundedNonNegativeInt "--max-queue"
  maximumDepth <- uniqueValue "--max-depth" maximumDepthValue
    (maybe "unbounded" show $ exferenceMaximumDepth defaults) flags
    >>= boundedPenalty "--max-depth"
  pure ExferenceCliOptions
    { exferenceCommon = common
    , exferenceEnvironment = environment
    , exferenceAllowFix = allowFix
    , exferenceSearchOptions = defaults
        { exferenceAllowUnused = allowUnused
        , exferenceAllowResidualConstraints = allowConstraints
        , exferenceConstraintDeferralSteps = deferral
        , exferenceMultiConstructorPatterns = multiConstructorPatterns
        , exferenceMaximumSteps = maximumSteps
        , exferenceMaximumQueueSize = maximumQueue
        , exferenceMaximumDepth = maximumDepth
        , exferenceCandidateRanking = presentationRanking $ commonPresentation common
        , exferenceProviderCosts = presentationProviderCosts $ commonPresentation common
        }
    }
 where
  defaults = defaultExferenceOptions

parseOptions :: Backend -> [String] -> Either String ([Flag], String)
parseOptions selectedBackend arguments = case
    getOpt Permute (optionsFor selectedBackend) arguments of
  (flags, [source], []) -> Right (flags, source)
  (_, [], []) -> Left "exactly one input type is required"
  (_, sources, []) -> Left $ "exactly one input type is required, but got "
    ++ show (length sources)
  (_, _, errors) -> Left $ concat errors

parseCommonOptions :: [Flag] -> String -> Either String CommonOptions
parseCommonOptions flags source = do
  rawTarget <- uniqueValue
    "--target" targetValue defaultResultTargetSpelling flags
  target <- checkedTarget rawTarget
  selection <- uniqueValue "--select" selectionValue defaultSelectionSpelling flags
    >>= parseSelectionMode "--select"
  renderMode <- uniqueValue "--render" renderValue defaultRenderSpelling flags
    >>= parseRenderMode "--render"
  qualification <- uniqueValue
    "--qualification" qualificationValue defaultQualificationSpelling flags
    >>= parseQualification "--qualification"
  ranking <- uniqueValue "--ranking" rankingValue "balanced" flags
    >>= parseCandidateRankingPolicy
  qualityWindow <- uniqueValue "--quality-window" qualityWindowValue "60" flags
    >>= positiveInt "--quality-window"
  providerCosts <- traverse parseProviderCostAssignment
    [assignment | ProviderCostFlag assignment <- flags]
  unless (length providerCosts == Map.size (Map.fromList providerCosts))
    $ Left "--provider-cost may assign each exact name only once"
  pure CommonOptions
    { commonTarget = target
    , commonPresentation = PresentationOptions
        { presentationSelection = selection
        , presentationRenderMode = renderMode
        , presentationQualification = qualification
        , presentationRanking = ranking
        , presentationProviderCosts = Map.fromList providerCosts
        , presentationQualityWindow = qualityWindow
        }
    , commonInput = source
    }

checkedTarget :: String -> Either String DefinitionName
checkedTarget source = case parseResultTarget source of
  Left failure -> Left $ "invalid --target: " ++ failure
  Right target -> Right target

uniqueValue
  :: String
  -> (Flag -> Maybe value)
  -> value
  -> [Flag]
  -> Either String value
uniqueValue option project fallback flags = case mapMaybe project flags of
  [] -> Right fallback
  [value] -> Right value
  _ -> Left $ option ++ " may be specified only once"

optionalUniqueValue
  :: String
  -> (Flag -> Maybe value)
  -> [Flag]
  -> Either String (Maybe value)
optionalUniqueValue option project flags = case mapMaybe project flags of
  [] -> Right Nothing
  [value] -> Right $ Just value
  _ -> Left $ option ++ " may be specified only once"

uniqueSwitch :: String -> (Flag -> Bool) -> [Flag] -> Either String Bool
uniqueSwitch option predicate flags = case length $ filter predicate flags of
  0 -> Right False
  1 -> Right True
  _ -> Left $ option ++ " may be specified only once"

targetValue, selectionValue, renderValue, qualificationValue
  :: Flag -> Maybe String
targetValue (TargetFlag value) = Just value
targetValue _ = Nothing
selectionValue (SelectionFlag value) = Just value
selectionValue _ = Nothing
renderValue (RenderFlag value) = Just value
renderValue _ = Nothing
qualificationValue (QualificationFlag value) = Just value
qualificationValue _ = Nothing

rankingValue, qualityWindowValue :: Flag -> Maybe String
rankingValue (RankingFlag value) = Just value
rankingValue _ = Nothing
qualityWindowValue (QualityWindowFlag value) = Just value
qualityWindowValue _ = Nothing

candidateLimitValue, choiceBudgetValue, constraintDeferralValue
  , maximumStepsValue, maximumQueueValue, maximumDepthValue
  :: Flag -> Maybe String
candidateLimitValue (CandidateLimitFlag value) = Just value
candidateLimitValue _ = Nothing
choiceBudgetValue (ChoiceBudgetFlag value) = Just value
choiceBudgetValue _ = Nothing
constraintDeferralValue (ConstraintDeferralStepsFlag value) = Just value
constraintDeferralValue _ = Nothing
maximumStepsValue (MaximumStepsFlag value) = Just value
maximumStepsValue _ = Nothing
maximumQueueValue (MaximumQueueFlag value) = Just value
maximumQueueValue _ = Nothing
maximumDepthValue (MaximumDepthFlag value) = Just value
maximumDepthValue _ = Nothing

environmentValue :: Flag -> Maybe FilePath
environmentValue (EnvironmentFlag value) = Just value
environmentValue _ = Nothing

replBackendValue :: Flag -> Maybe String
replBackendValue (ReplBackendFlag value) = Just value
replBackendValue _ = Nothing

historyValue :: Flag -> Maybe FilePath
historyValue (HistoryFlag value) = Just value
historyValue _ = Nothing

optionsFor :: Backend -> [OptDescr Flag]
optionsFor selectedBackend =
  commonOptions ++ backendSpecificOptions selectedBackend

backendSpecificOptions :: Backend -> [OptDescr Flag]
backendSpecificOptions selectedBackend = case selectedBackend of
  DjinnBackend -> djinnOptions
  ExferenceBackend -> exferenceOptions

commonOptions :: [OptDescr Flag]
commonOptions =
  [ Option [] ["target"] (ReqArg TargetFlag "NAME")
      $ defaulted "generated definition name" defaultResultTargetSpelling
  , Option [] ["select"] (ReqArg SelectionFlag "first|best|all")
      $ defaulted "candidate selection policy" defaultSelectionSpelling
  , Option [] ["ranking"] (ReqArg RankingFlag "legacy|balanced|compact|diverse")
      "candidate quality policy (default balanced)"
  , Option [] ["provider-cost"] (ReqArg ProviderCostFlag "NAME=COST")
      "nonnegative structural cost for one exact provider; repeat for distinct names"
  , Option [] ["quality-window"] (ReqArg QualityWindowFlag "N")
      "Exference raw candidate observation window for structural all-selection (default 60)"
  , Option [] ["render"] (ReqArg RenderFlag "definition|expression")
      $ defaulted "render a definition or expression" defaultRenderSpelling
  , Option [] ["qualification"]
      (ReqArg QualificationFlag "none|identifiers|full")
      $ defaulted "name qualification policy" defaultQualificationSpelling
  ]

djinnOptions :: [OptDescr Flag]
djinnOptions =
  [ Option [] ["environment"] (ReqArg EnvironmentFlag "DIR")
      "source environment directory, including supplied providers"
  , Option [] ["candidate-limit"] (ReqArg CandidateLimitFlag "N")
      $ defaulted "positive proof-candidate limit"
          $ show defaultDjinnCandidateLimit
  , Option [] ["choice-budget"] (ReqArg ChoiceBudgetFlag "N")
      $ defaulted "non-negative choice-point budget; 0 is unlimited"
          defaultDjinnChoiceBudget
  ]

exferenceOptions :: [OptDescr Flag]
exferenceOptions =
  [ Option [] ["environment"] (ReqArg EnvironmentFlag "DIR")
      "source environment directory"
  , Option [] ["allow-unused"] (NoArg AllowUnusedFlag)
      "allow unused input variables"
  , Option [] ["allow-constraints"] (NoArg AllowConstraintsFlag)
      "allow residual constraints"
  , Option [] ["constraint-deferral-steps"]
      (ReqArg ConstraintDeferralStepsFlag "N")
      $ defaulted "non-negative constraint-deferral step count"
          $ show $ exferenceConstraintDeferralSteps defaultExferenceOptions
  , Option [] ["multi-constructor-patterns"]
      (NoArg MultiConstructorPatternsFlag)
      "allow matches on multi-constructor datatypes"
  , Option [] ["max-steps"] (ReqArg MaximumStepsFlag "N")
      $ defaulted "positive search-step limit"
          $ show $ exferenceMaximumSteps defaultExferenceOptions
  , Option [] ["max-queue"] (ReqArg MaximumQueueFlag "N|unbounded")
      $ defaulted "non-negative queue limit or unbounded"
          $ renderBounded $ exferenceMaximumQueueSize defaultExferenceOptions
  , Option [] ["max-depth"] (ReqArg MaximumDepthFlag "N|unbounded")
      $ defaulted "non-negative search cost or unbounded"
          $ renderBounded $ exferenceMaximumDepth defaultExferenceOptions
  , Option [] ["fix"] (NoArg AllowFixFlag)
      "allow known nonterminating recursion helpers"
  ]

replOptionDescriptors :: [OptDescr Flag]
replOptionDescriptors =
  [ Option [] ["backend"]
      (ReqArg ReplBackendFlag "djinn|exference|both")
      $ defaulted "initial backend selection"
          $ replBackendName $ replInitialBackend defaultReplOptions
  , Option [] ["environment"] (ReqArg EnvironmentFlag "DIR")
      "initial source workspace directory"
  , Option [] ["fix"] (NoArg AllowFixFlag)
      "retain known nonterminating Exference recursion helpers"
  , Option [] ["history"] (ReqArg HistoryFlag "FILE")
      "read and write persistent REPL history"
  , Option [] ["ignore-startup"] (NoArg IgnoreStartupFlag)
      "skip .djexrc startup files"
  ]

-- Parser fallbacks and help text share these spellings. Backend-owned numeric
-- defaults are projected directly from their public option records so changing
-- a search policy cannot leave a stale command-line promise behind.
defaultSelectionSpelling, defaultRenderSpelling
  , defaultQualificationSpelling, defaultDjinnChoiceBudget :: String
defaultSelectionSpelling = selectionModeName
  $ presentationSelection defaultOneShotPresentationOptions
defaultRenderSpelling = renderModeName
  $ presentationRenderMode defaultOneShotPresentationOptions
defaultQualificationSpelling = qualificationName
  $ presentationQualification defaultOneShotPresentationOptions
defaultDjinnChoiceBudget = maybe "0" show $ optionBudget defaultQueryOptions

defaultDjinnCandidateLimit :: Int
defaultDjinnCandidateLimit = optionCutoff defaultQueryOptions

defaulted :: String -> String -> String
defaulted description value = description ++ " (default: " ++ value ++ ")"

fullUsage :: String
fullUsage = intercalate "\n"
  [ "Djex: checked Haskell expression synthesis with Djinn and Exference"
  , ""
  , "Usage:"
  , "  djex --help"
  , "  djex --version"
  , "  djex"
  , "  djex repl [OPTION...]"
  , "  djex djinn [OPTION...] TYPE"
  , "  djex exference [OPTION...] TYPE"
  , "  djex download CABAL_TARGET ..."
  , "  djex install [--lib] CABAL_TARGET ..."
  , ""
  , usageInfo "REPL options:" replOptionDescriptors
  , usageInfo "Common options:" commonOptions
  , usageInfo "Djinn options:" djinnOptions
  , usageInfo "Exference options:" exferenceOptions
  ]

replUsage :: String
replUsage = intercalate "\n"
  [ "Usage: djex repl [OPTION...]"
  , "       djex"
  , ""
  , "Start a persistent GHCi-style synthesis session."
  , "Bare type queries use the active backend selection; type :help inside"
  , "the session for commands and settings."
  , ""
  , usageInfo "REPL options:" replOptionDescriptors
  ]

backendUsage :: Backend -> String
backendUsage selectedBackend = intercalate "\n"
  [ "Usage: djex " ++ commandName ++ " [OPTION...] TYPE"
  , ""
  , usageInfo "Common options:" commonOptions
  , usageInfo (backendTitle ++ " options:") backendOptions
  ]
 where
  commandName = case selectedBackend of
    DjinnBackend -> "djinn"
    ExferenceBackend -> "exference"
  backendTitle = backendName $ backendInfo selectedBackend
  backendOptions = backendSpecificOptions selectedBackend

packageUsage :: PackageOperation -> String
packageUsage operation = intercalate "\n"
  [ "Usage: djex " ++ packageOperationName operation ++ arguments operation
  , ""
  , summary operation
  , "Cabal targets are passed after -- and cannot become Cabal options."
  , "This command does not load compiled package modules into the Djex REPL."
  ]
 where
  summary DownloadOperation =
    "Ask Cabal to fetch targets and dependencies into its configured source cache."
  summary (InstallOperation _) =
    "Build and install package executables, or libraries with --lib."
  arguments DownloadOperation = " CABAL_TARGET ..."
  arguments (InstallOperation _) = " [--lib] CABAL_TARGET ..."

usageFailure :: String -> IO ExitCode
usageFailure message = do
  hPutStrLn stderr $ "djex: " ++ message
  hPutStrLn stderr "Try 'djex --help' for usage."
  pure usageExit

usageExit :: ExitCode
usageExit = ExitFailure 2
