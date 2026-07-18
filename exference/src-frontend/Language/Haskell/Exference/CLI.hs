{-# LANGUAGE MultiWayIf #-}

module Language.Haskell.Exference.CLI (main) where

import Control.Monad (forM_, unless, when)
import Data.List (intercalate, sortBy)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Version (showVersion)
import Numeric.Natural (Natural)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
  ( BufferMode (LineBuffering)
  , hPutStrLn
  , hSetBuffering
  , stderr
  , stdout
  )
import Text.Read (readMaybe)

import Language.Haskell.Djex.Exference
import Language.Haskell.Djex.Exference.HaskellSrc
  ( defaultExferenceEnvironmentPath
  , exferenceCommandSessionPolicy
  , parseExferenceRequestWithCheckedTarget
  )
import qualified Language.Haskell.Exference.Session as Session
import Language.Haskell.Exference.Core.FunctionBinding
  ( functionName )
import Language.Haskell.Exference.Core.Types
  ( sClassEnv_instances
  , sClassEnv_tclasses
  )
import Language.Haskell.Exference.EnvironmentParser
  ( LoadReport (..)
  , SourceEnvironment (..)
  , checkedSourceProjection
  , environmentLoadErrorDiagnostics
  , environmentFromPath
  , sourceFunctions
  )
import Language.Haskell.Synthesis.Candidate
  ( candidateResidualConstraints )
import Language.Haskell.Synthesis.Diagnostic (renderDiagnostic)
import Language.Haskell.Synthesis.Generated
  ( DefinitionName
  , mkDefinitionName
  )
import Language.Haskell.Synthesis.Name (mkIdentifier)
import Language.Haskell.Synthesis.Query (resultSearch)
import Language.Haskell.Synthesis.Search
  ( ObservedProgress (..)
  , Progress
  , TruncationReason (IdentifierSpaceExhausted, StepLimitReached)
  , batchProgress
  , observeProgress
  , progressTruncationDiagnostic
  )
import Language.Haskell.Synthesis.Selection
  ( Selection (..)
  , SelectionMode (..)
  , foldAllQueryResultsM
  , selectPreferredQueryResults
  , selectQueryResults
  )

import Paths_djex (version)

data Flag
  = Verbose (Maybe String)
  | Version
  | Help
  | PrintEnv
  | EnvDir FilePath
  | Input String
  | PrintAll
  | EnvUsage
  | Shortest
  | FirstSol
  | Best
  | Unused
  | PatternMatchMC
  | QualificationLevel Int
  | Constraints
  | AllowFix
  deriving (Eq, Show)

-- This is the historical command-line ranking profile. It intentionally
-- remains distinct from the library default until differential benchmarks
-- justify changing established CLI search order.
cliHeuristicsConfig :: ExferenceHeuristicsConfig
cliHeuristicsConfig = ExferenceHeuristicsConfig
  { heuristics_goalVar = 0.8
  , heuristics_goalCons = 0.7
  , heuristics_goalArrow = 4.3
  , heuristics_goalApp = 1.9
  , heuristics_stepProvidedGood = 0.22
  , heuristics_stepProvidedBad = 5.0
  , heuristics_stepEnvGood = 6.0
  , heuristics_stepEnvBad = 22.0
  , heuristics_tempUnusedVarPenalty = 1.1
  , heuristics_tempMultiVarUsePenalty = 6.7
  , heuristics_functionGoalTransform = 0.1
  , heuristics_unusedVar = 20.0
  , heuristics_solutionLength = 0.0153
  }

options :: [OptDescr Flag]
options =
  [ Option [] ["version"] (NoArg Version) "print version and exit"
  , Option [] ["help"] (NoArg Help) "print basic program information"
  , Option ['p'] ["printenv"] (NoArg PrintEnv)
      "print the environment used for queries"
  , Option ['e'] ["envdir"] (ReqArg EnvDir "PATH")
      "path to the environment directory"
  , Option ['v'] ["verbose"] (OptArg Verbose "INT") "verbosity"
  , Option ['i'] ["input"] (ReqArg Input "HSTYPE")
      "a type for which to generate an expression (repeatable)"
  , Option ['a'] ["all"] (NoArg PrintAll)
      "print all solutions up to the search step limit"
  , Option [] ["envUsage"] (NoArg EnvUsage)
      "print the most-used source bindings after a complete search"
  , Option ['o'] ["short"] (NoArg Shortest)
      "prefer shorter solutions"
  , Option ['f'] ["first"] (NoArg FirstSol)
      "stop after the first solution"
  , Option [] ["fix"] (NoArg AllowFix)
      "allow known nonterminating recursion helpers"
  , Option ['b'] ["best"] (NoArg Best)
      "inspect the complete search and print every globally best solution"
  , Option ['u'] ["allowUnused"] (NoArg Unused)
      "allow unused input variables"
  , Option ['c'] ["patternMatchMC"] (NoArg PatternMatchMC)
      "pattern match on multi-constructor datatypes"
  , Option ['q'] ["fullqualification"]
      (NoArg $ QualificationLevel 2)
      "fully qualify identifiers and operators in output"
  , Option [] ["somequalification"]
      (NoArg $ QualificationLevel 1)
      "qualify non-operator identifiers in output"
  , Option ['w'] ["allowConstraints"] (NoArg Constraints)
      "allow additional unresolved constraints in solutions"
  ]

fullUsageInfo :: String
fullUsageInfo = usageInfo "Usage: exference [OPTION...] [HSTYPE...]" options

mainOpts :: [String] -> Either String ([Flag], [String])
mainOpts arguments = case getOpt (ReturnInOrder Input) options arguments of
  (flags, [], [])
    | null flags -> Right ([Help], [])
    | otherwise -> Right
        (flags, [source | Input source <- flags])
  (_, _, errors) -> Left $ concat errors ++ fullUsageInfo

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  arguments <- getArgs
  (flags, inputs) <- either usageFailure pure $ mainOpts arguments
  if
    | Version `elem` flags ->
        putStrLn $ "exference version " ++ showVersion version
    | Help `elem` flags -> putStrLn fullUsageInfo
    | otherwise -> run flags inputs

run :: [Flag] -> [String] -> IO ()
run flags inputs = do
  verbosity <- either usageFailure pure $ parseVerbosity flags
  validateFlagCombinations flags inputs
  defaultEnvironmentPath <- defaultExferenceEnvironmentPath
  let environmentPath = case [path | EnvDir path <- flags] of
        [] -> defaultEnvironmentPath
        path : _ -> path
  when (verbosity > 0) $ do
    putStrLn $ "exference version " ++ showVersion version
    putStrLn "[Environment]"
    putStrLn $ "reading environment from " ++ environmentPath
  LoadReport environmentResult loaderDiagnostics <-
    environmentFromPath environmentPath
  checkedEnvironment <- case environmentResult of
    Left failure -> fatal $ intercalate "\n"
      $ "could not load source environment:"
      : map renderDiagnostic
          (NonEmpty.toList $ environmentLoadErrorDiagnostics failure)
    Right value -> pure value
  when (verbosity > 0) $ forM_ loaderDiagnostics $ \value ->
    putStrLn $ "environment " ++ renderDiagnostic value
  let sourceEnvironment = checkedSourceProjection checkedEnvironment

  policy <- either
    (fatal . ("invalid Exference command policy: " ++) . renderDiagnostic)
    pure
    $ exferenceCommandSessionPolicy (AllowFix `elem` flags)
  session <- either
    (fatal . ("could not seal Exference session: " ++) . renderDiagnostic)
    pure
    $ Session.mkExferenceSessionWithPolicy policy checkedEnvironment
  when (verbosity > 0) $ forM_ (exferenceSessionDiagnostics session) $ \value ->
    putStrLn $ "session " ++ renderDiagnostic value

  when (PrintEnv `elem` flags) $
    printEnvironment verbosity sourceEnvironment
  target <- freshTarget sourceEnvironment
  forM_ inputs $ runQuery verbosity flags session target

runQuery
  :: Int
  -> [Flag]
  -> ExferenceSession
  -> DefinitionName
  -> String
  -> IO ()
runQuery verbosity flags session target source = do
  when (verbosity > 0) $ do
    putStrLn "[Custom Input]"
    putStrLn $ "input type: " ++ source
  let queryOptions = optionsFor flags
      searchOptions
        | prefersConstraintFreeFallback flags = queryOptions
            {exferenceAllowResidualConstraints = True}
        | otherwise = queryOptions
  request <- case parseExferenceRequestWithCheckedTarget
      session searchOptions target "inputtype.hs" source of
    Left failure -> fatal
      $ "could not parse input type: " ++ renderDiagnostic failure
    Right value -> pure value
  when (verbosity > 0) $ do
    putStrLn "full shared request:"
    print $ exferenceRequestQuery request
  results <- case runExferenceQuery session request of
    Left failure -> fatal $ "invalid search input: " ++ renderDiagnostic failure
    Right value -> pure value
  presentResults verbosity flags results

optionsFor :: [Flag] -> ExferenceOptions
optionsFor flags = defaultExferenceOptions
  { exferenceAllowUnused = Unused `elem` flags
  , exferenceAllowResidualConstraints = Constraints `elem` flags
  , exferenceMultiConstructorPatterns = PatternMatchMC `elem` flags
  , exferenceHeuristics = if Shortest `elem` flags
      then cliHeuristicsConfig
      else cliHeuristicsConfig {heuristics_solutionLength = 0}
  }

prefersConstraintFreeFallback :: [Flag] -> Bool
prefersConstraintFreeFallback flags = not $ any (`elem` flags)
  [PrintAll, EnvUsage, FirstSol, Best, Constraints]

presentResults :: Int -> [Flag] -> [ExferenceResult] -> IO ()
presentResults verbosity flags results = if
  | PrintAll `elem` flags -> do
      when (verbosity > 0) $ putStrLn "[running complete search ..]"
      printAllResults qualification results
  | EnvUsage `elem` flags -> do
      when (verbosity > 0) $ putStrLn "[running complete search ..]"
      let finalResult = lastMaybe results
          usages = maybe Map.empty exferenceResultBindingUsages finalResult
          highest = take 8 $ sortBy (flip $ comparing snd)
            $ Map.toList usages
      print [(show binding, count) | (binding, count) <- highest]
      reportTruncation
        $ batchProgress . resultSearch <$> finalResult
  | otherwise -> do
      when (verbosity > 0) $ putStrLn
        $ "[selecting " ++ selectionDescription flags ++ " ..]"
      let selection
            | FirstSol `elem` flags =
                selectQueryResults SelectFirst rank (const True) results
            | Best `elem` flags =
                selectQueryResults SelectBest rank (const True) results
            | Constraints `elem` flags = selectQueryResults
                (SelectBestLookahead 256) rank (const True) results
            | otherwise = selectPreferredQueryResults
                256 rank (const True)
                (null . candidateResidualConstraints) results
      printSelection qualification selection
 where
  qualification = qualificationFor flags
  rank = exferenceCandidateComplexity . exferenceCandidateMetrics
selectionDescription :: [Flag] -> String
selectionDescription flags
  | FirstSol `elem` flags = "first expression"
  | Best `elem` flags = "globally best expressions"
  | Constraints `elem` flags = "constrained lookahead"
  | otherwise = "constraint-free preference"

qualificationFor :: [Flag] -> Qualification
qualificationFor flags = case [level | QualificationLevel level <- flags] of
  [] -> Unqualified
  1 : _ -> QualifyIdentifiers
  _ : _ -> FullyQualified

printSelection :: Qualification -> Selection ExferenceCandidate -> IO ()
printSelection qualification (Selection progress candidates) = do
  case candidates of
    [] -> putStrLn $ noResultsMessage progress
    _ -> mapM_ (printCandidate qualification) candidates
  reportTruncation progress

printAllResults :: Qualification -> [ExferenceResult] -> IO ()
printAllResults qualification results = do
  (progress, foundAny) <- foldAllQueryResultsM
    (const True) printOne False results
  unless foundAny $ putStrLn $ noResultsMessage progress
  reportTruncation progress
 where
  printOne _ candidate = do
    printCandidate qualification candidate
    pure True

reportTruncation :: Maybe Progress -> IO ()
reportTruncation = mapM_
  (hPutStrLn stderr . renderDiagnostic)
  . progressTruncationDiagnostic

printCandidate :: Qualification -> ExferenceCandidate -> IO ()
printCandidate qualification candidate = do
  rendered <- either
    (fatal . ("cannot render checked search result: " ++) . show)
    pure
    $ renderExferenceCandidateExpression qualification candidate
  putStrLn rendered
  let constraints = renderExferenceResidualConstraints candidate
  unless (null constraints) $ putStrLn
    $ "but only with additional constraints: "
    ++ intercalate ", " constraints
  let metrics = exferenceCandidateMetrics candidate
  -- The core snapshots the remaining queue when it emits this candidate; it
  -- does not retain a high-water mark for the priority queue.
  putStrLn $ replicate 40 ' '
    ++ "(depth " ++ show (exferenceCandidateComplexity metrics)
    ++ ", " ++ show (exferenceCandidateSteps metrics) ++ " steps, "
    ++ show (exferenceCandidateFinalQueueSize metrics)
    ++ " final queue size)"

noResultsMessage :: Maybe Progress -> String
noResultsMessage progress = case observeProgress progress of
  NoProgressObserved -> "[no search states were produced]"
  ObservedFinished -> "[no results: search space exhausted]"
  ObservedTruncated reasons
    | StepLimitReached `elem` NonEmpty.toList reasons ->
        "[no results found before the step limit; inhabitation is undecided]"
    | IdentifierSpaceExhausted `elem` NonEmpty.toList reasons ->
        "[no results found before an internal identifier-space limit; inhabitation is undecided]"
    | otherwise ->
        "[no results found after pruning; inhabitation is undecided]"
  ObservedContinuing ->
    "[no results in the inspected search prefix; inhabitation is undecided]"

printEnvironment :: Int -> SourceEnvironment -> IO ()
printEnvironment verbosity environment = do
  when (verbosity > 0) $ putStrLn "[Environment]"
  mapM_ print $ sourceTypeSynonyms environment
  mapM_ print $ Map.elems classes
  mapM_ print
    [(name, instanceValue) | (name, values) <- Map.toList instances,
      instanceValue <- values]
  mapM_ print $ sourceFunctions environment
  mapM_ print $ sourceDeconstructors environment
 where
  classes = sClassEnv_tclasses $ sourceClasses environment
  instances = sClassEnv_instances $ sourceClasses environment

-- The shared session correctly excludes an environment binding equal to its
-- generated definition target. This compatibility CLI prints only the clause
-- body, so choose a target outside the loaded source namespace instead of
-- needlessly making such a binding unavailable to expression search.
freshTarget :: SourceEnvironment -> IO DefinitionName
freshTarget environment = go 0
 where
  occupied = Set.fromList
    [functionName binding
    | binding <- sourceFunctions environment]
  go :: Natural -> IO DefinitionName
  go suffix = do
    let spelling = "_djexResult" ++ if suffix == 0 then "" else show suffix
    candidate <- either
      (fatal . ("invalid generated result target: " ++) . show)
      pure
      $ mkIdentifier spelling
    if Set.member candidate occupied
      then go $ suffix + 1
      else either
        (fatal . ("invalid generated result target: " ++) . show)
        pure
        $ mkDefinitionName candidate

parseVerbosity :: [Flag] -> Either String Int
parseVerbosity flags = do
  values <- mapM parseValue [value | Verbose value <- flags]
  -- Every individual option now fits in Int, but their repeated sum may not.
  let total = sum values
  if total <= maximumVerbosity
    then Right $ fromInteger total
    else Left $ "combined verbosity exceeds maximum "
      ++ show maximumVerbosity
 where
  maximumVerbosity = toInteger (maxBound :: Int)

  parseValue :: Maybe String -> Either String Integer
  parseValue Nothing = Right 1
  parseValue (Just source) = case readMaybe source :: Maybe Integer of
    Just value
      | value >= 0
      , value <= maximumVerbosity -> Right value
    _ -> Left $ "invalid verbosity " ++ show source

validateFlagCombinations :: [Flag] -> [String] -> IO ()
validateFlagCombinations flags inputs = do
  requireAtMostOne "selection mode"
    [ flag
    | flag <- [PrintAll, EnvUsage, FirstSol, Best]
    , flag `elem` flags
    ]
  requireAtMostOne "qualification mode"
    [flag | flag@QualificationLevel{} <- flags]
  requireAtMostOne "environment directory"
    [flag | flag@EnvDir{} <- flags]
  when (null inputs && any queryOnlyFlag flags) $
    usageFailure "a search or selection option requires an input type"
 where
  requireAtMostOne description selected = case selected of
    _ : _ : _ -> usageFailure $ "conflicting " ++ description ++ " options: "
      ++ intercalate ", " (map flagOption selected)
    _ -> pure ()

  queryOnlyFlag flag = case flag of
    PrintAll -> True
    EnvUsage -> True
    Shortest -> True
    FirstSol -> True
    Best -> True
    Unused -> True
    PatternMatchMC -> True
    QualificationLevel{} -> True
    Constraints -> True
    _ -> False

  flagOption flag = case flag of
    Verbose{} -> "--verbose"
    Version -> "--version"
    Help -> "--help"
    PrintEnv -> "--printenv"
    EnvDir{} -> "--envdir"
    Input{} -> "--input"
    PrintAll -> "--all"
    EnvUsage -> "--envUsage"
    Shortest -> "--short"
    FirstSol -> "--first"
    Best -> "--best"
    Unused -> "--allowUnused"
    PatternMatchMC -> "--patternMatchMC"
    QualificationLevel 1 -> "--somequalification"
    QualificationLevel{} -> "--fullqualification"
    Constraints -> "--allowConstraints"
    AllowFix -> "--fix"

lastMaybe :: [value] -> Maybe value
lastMaybe = List.foldl' (\_ value -> Just value) Nothing

usageFailure :: String -> IO value
usageFailure message = fatal $ message ++ "\n" ++ fullUsageInfo

fatal :: String -> IO value
fatal message = hPutStrLn stderr message >> exitFailure
