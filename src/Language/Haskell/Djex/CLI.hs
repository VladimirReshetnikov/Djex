-- | The merged one-shot @djex@ command.
--
-- Living in the library keeps this frontend in-process testable and matches
-- the two historical launchers, whose executables are equally thin wrappers
-- over "Djinn" and "Language.Haskell.Exference.CLI".
module Language.Haskell.Djex.CLI
  ( main
  , runArguments
  ) where

import Data.Foldable (toList)
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
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.IO (hPutStrLn, stderr)
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
import Paths_djex (version)

data Flag
  = TargetFlag String
  | SelectionFlag String
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
  deriving (Eq, Show)

data CommonOptions = CommonOptions
  { commonTarget :: DefinitionName
  , commonSelection :: SelectionMode
  , commonRenderMode :: RenderMode
  , commonQualification :: Qualification
  , commonInput :: String
  }

data DjinnOptions = DjinnOptions
  { djinnCommon :: CommonOptions
  , djinnCandidateLimit :: Int
  , djinnChoiceBudget :: Maybe Integer
  }

data ExferenceCliOptions = ExferenceCliOptions
  { exferenceCommon :: CommonOptions
  , exferenceEnvironment :: Maybe FilePath
  , exferenceAllowFix :: Bool
  , exferenceSearchOptions :: ExferenceOptions
  }

main :: IO ()
main = getArgs >>= runArguments >>= exitWith

-- | Run the command without terminating the process.  Keeping argument
-- dispatch here makes the process-level CLI straightforward to exercise.
runArguments :: [String] -> IO ExitCode
runArguments arguments = case arguments of
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
  "djinn" : backendArguments ->
    runParsed (parseDjinnOptions backendArguments) runDjinn
  "exference" : backendArguments ->
    runParsed (parseExferenceOptions backendArguments) runExference
  backendArgument : _ ->
    usageFailure $ "unknown backend " ++ show backendArgument
  [] -> usageFailure "a backend is required"

runParsed :: Either String options -> (options -> IO ExitCode) -> IO ExitCode
runParsed parsed action = either usageFailure action parsed

runDjinn :: DjinnOptions -> IO ExitCode
runDjinn options = case standardDjinnSession of
  Left failure -> diagnosticFailure failure
  Right session -> case parseDjinnRequestWithCheckedTarget
      session
      (djinnQueryOptions options)
      (commonTarget common)
      "<command-line>"
      source of
    Left failure -> diagnosticFailure failure
    Right request -> case runDjinnQuery session request of
      Left failure -> diagnosticFailure failure
      Right result -> presentDjinn (presentationOptions common) result
 where
  common = djinnCommon options
  source = commonInput common

runExference :: ExferenceCliOptions -> IO ExitCode
runExference options = case
    exferenceCommandSessionPolicy (exferenceAllowFix options) of
  Left failure -> diagnosticFailure failure
  Right policy -> do
    environmentPath <- case exferenceEnvironment options of
      Just path -> pure path
      Nothing -> defaultExferenceEnvironmentPath
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
      Right session -> case parseExferenceRequestWithCheckedTarget
          session
          (exferenceSearchOptions options)
          (commonTarget common)
          "<command-line>"
          source of
        Left failure -> diagnosticFailure failure
        Right request -> case runExferenceQuery session request of
          Left failure -> diagnosticFailure failure
          Right results -> presentExference (presentationOptions common) results
 where
  common = exferenceCommon options
  source = commonInput common

presentationOptions :: CommonOptions -> PresentationOptions
presentationOptions options = PresentationOptions
  { presentationSelection = commonSelection options
  , presentationRenderMode = commonRenderMode options
  , presentationQualification = commonQualification options
  }

djinnQueryOptions :: DjinnOptions -> QueryOptions
djinnQueryOptions options = defaultQueryOptions
  { optionAlternatives = selection /= SelectFirst
  , optionSorted = False
  , optionCutoff = djinnCandidateLimit options
  , optionBudget = djinnChoiceBudget options
  }
 where
  selection = commonSelection $ djinnCommon options

parseDjinnOptions :: [String] -> Either String DjinnOptions
parseDjinnOptions arguments = do
  (flags, source) <- parseOptions DjinnBackend arguments
  common <- parseCommonOptions flags source
  candidateLimit <- uniqueValue "--candidate-limit" candidateLimitValue
    (show defaultDjinnCandidateLimit) flags >>= positiveInt "--candidate-limit"
  rawBudget <- uniqueValue "--choice-budget" choiceBudgetValue
    defaultDjinnChoiceBudget flags
  budget <- nonNegativeInteger "--choice-budget" rawBudget
  pure DjinnOptions
    { djinnCommon = common
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
  rawTarget <- uniqueValue "--target" targetValue defaultTargetSpelling flags
  target <- checkedTarget rawTarget
  selection <- uniqueValue "--select" selectionValue defaultSelectionSpelling flags
    >>= selectionMode
  renderMode <- uniqueValue "--render" renderValue defaultRenderSpelling flags
    >>= checkedRenderMode
  qualification <- uniqueValue
    "--qualification" qualificationValue defaultQualificationSpelling flags
    >>= checkedQualification
  pure CommonOptions
    { commonTarget = target
    , commonSelection = selection
    , commonRenderMode = renderMode
    , commonQualification = qualification
    , commonInput = source
    }

checkedTarget :: String -> Either String DefinitionName
checkedTarget source = case parseName source of
  Left failure -> Left $ "invalid --target: " ++ renderNameError failure
  Right target -> case mkDefinitionName target of
    Left failure -> Left $ "invalid --target: " ++ show failure
    Right checked -> Right checked

selectionMode :: String -> Either String SelectionMode
selectionMode source = case source of
  "first" -> Right SelectFirst
  "best" -> Right SelectBest
  "all" -> Right SelectAll
  _ -> Left "--select must be first, best, or all"

checkedRenderMode :: String -> Either String RenderMode
checkedRenderMode source = case source of
  "definition" -> Right RenderDefinition
  "expression" -> Right RenderExpression
  _ -> Left "--render must be definition or expression"

checkedQualification :: String -> Either String Qualification
checkedQualification source = case source of
  "none" -> Right Unqualified
  "identifiers" -> Right QualifyIdentifiers
  "full" -> Right FullyQualified
  _ -> Left "--qualification must be none, identifiers, or full"

positiveInt :: String -> String -> Either String Int
positiveInt option = checkedInt 1
  $ option ++ " must be a positive integer"

nonNegativeInt :: String -> String -> Either String Int
nonNegativeInt option = checkedInt 0
  $ option ++ " must be a non-negative integer"

-- Parse through the unbounded representation before converting. Reading an
-- out-of-range literal directly as Int silently wraps modulo the Int range.
checkedInt :: Integer -> String -> String -> Either String Int
checkedInt lowerBound failure source = case readMaybe source :: Maybe Integer of
  Just value
    | value >= lowerBound
    , value <= toInteger (maxBound :: Int) -> Right $ fromInteger value
  _ -> Left failure

nonNegativeInteger :: String -> String -> Either String Integer
nonNegativeInteger option source = case readMaybe source of
  Just value | value >= 0 -> Right value
  _ -> Left $ option ++ " must be a non-negative integer"

boundedNonNegativeInt :: String -> String -> Either String (Maybe Int)
boundedNonNegativeInt _ "unbounded" = Right Nothing
boundedNonNegativeInt option source = Just <$> nonNegativeInt option source

boundedPenalty :: String -> String -> Either String (Maybe Penalty)
boundedPenalty _ "unbounded" = Right Nothing
boundedPenalty option source = case readMaybe source of
  Just value
    | value >= 0
    , not $ isNaN value || isInfinite value -> Right $ Just $ Penalty value
  _ -> Left $ option ++ " must be a finite non-negative number or unbounded"

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
      $ defaulted "generated definition name" defaultTargetSpelling
  , Option [] ["select"] (ReqArg SelectionFlag "first|best|all")
      $ defaulted "candidate selection policy" defaultSelectionSpelling
  , Option [] ["render"] (ReqArg RenderFlag "definition|expression")
      $ defaulted "render a definition or expression" defaultRenderSpelling
  , Option [] ["qualification"]
      (ReqArg QualificationFlag "none|identifiers|full")
      $ defaulted "name qualification policy" defaultQualificationSpelling
  ]

djinnOptions :: [OptDescr Flag]
djinnOptions =
  [ Option [] ["candidate-limit"] (ReqArg CandidateLimitFlag "N")
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

-- Parser fallbacks and help text share these spellings. Backend-owned numeric
-- defaults are projected directly from their public option records so changing
-- a search policy cannot leave a stale command-line promise behind.
defaultTargetSpelling, defaultSelectionSpelling, defaultRenderSpelling
  , defaultQualificationSpelling, defaultDjinnChoiceBudget :: String
defaultTargetSpelling = "djexResult"
defaultSelectionSpelling = "best"
defaultRenderSpelling = "definition"
defaultQualificationSpelling = "full"
defaultDjinnChoiceBudget = maybe "0" show $ optionBudget defaultQueryOptions

defaultDjinnCandidateLimit :: Int
defaultDjinnCandidateLimit = optionCutoff defaultQueryOptions

defaulted :: String -> String -> String
defaulted description value = description ++ " (default: " ++ value ++ ")"

renderBounded :: Show value => Maybe value -> String
renderBounded = maybe "unbounded" show

fullUsage :: String
fullUsage = intercalate "\n"
  [ "Djex: checked Haskell expression synthesis with an explicit backend"
  , ""
  , "Usage:"
  , "  djex --help"
  , "  djex --version"
  , "  djex djinn [OPTION...] TYPE"
  , "  djex exference [OPTION...] TYPE"
  , ""
  , usageInfo "Common options:" commonOptions
  , usageInfo "Djinn options:" djinnOptions
  , usageInfo "Exference options:" exferenceOptions
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

usageFailure :: String -> IO ExitCode
usageFailure message = do
  hPutStrLn stderr $ "djex: " ++ message
  hPutStrLn stderr "Try 'djex --help' for usage."
  pure usageExit

usageExit :: ExitCode
usageExit = ExitFailure 2
