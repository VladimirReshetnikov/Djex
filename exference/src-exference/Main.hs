{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternGuards #-}

module Main
  ( main
  )
where



import Language.Haskell.Exference
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc
import Language.Haskell.Exference.Diagnostic (diagnosticMessage)
import Language.Haskell.Exference.Core.FunctionBinding
import qualified Language.Haskell.Exference.Core.Expression as CoreExpression
import Language.Haskell.Exference.EnvironmentParser

import Language.Haskell.Exference.Core.Types
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Search as SharedSearch
import qualified Language.Haskell.Synthesis.Inventory as SharedInventory

import Control.Monad ( when, forM_ )
import Data.List ( sortBy, intercalate, nub )
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List as List
import Data.Ord ( comparing )
import Data.Maybe ( listToMaybe, fromMaybe )
import Control.Monad.Writer.Strict
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Language.Haskell.Exts.Parser ( parseModuleWithMode
                                    , ParseResult (..)
                                    , ParseMode (..) )
import Language.Haskell.Exts.Extension ( Language (..)
                                       , Extension (..)
                                       , KnownExtension (..) )
import Control.Monad.Trans.MultiRWS
import Control.Monad.Trans.Except


import MainConfig

import Paths_exference

import System.Environment ( getArgs )
import System.Console.GetOpt
import Data.Version ( showVersion )
import System.IO ( hSetBuffering, BufferMode(..), stdout, stderr )

data Flag = Verbose Int
          | Version
          | Help
          | PrintEnv
          | EnvDir String
          | Input String
          | PrintAll
          | EnvUsage -- TODO: option to specify dictionary to use
          | Shortest
          | FirstSol
          | Best
          | Unused
          | PatternMatchMC
          | Qualification Int
          | Constraints
          | AllowFix
  deriving (Show, Eq)

options :: [OptDescr Flag]
options =
  [ Option []    ["version"]     (NoArg Version)       ""
  , Option []    ["help"]        (NoArg Help)          "prints basic program info"
  , Option ['p'] ["printenv"]    (NoArg PrintEnv)      "print the environment to be used for queries"
  , Option ['e'] ["envdir"]      (ReqArg EnvDir "PATH") "path to environment directory"
  , Option ['v'] ["verbose"]     (OptArg (Verbose . maybe 1 read) "INT") "verbosity"
  , Option ['i'] ["input"]       (ReqArg Input "HSTYPE") "the type for which to generate an expression"
  , Option ['a'] ["all"]         (NoArg PrintAll)      "print all solutions (up to search step limit)"
  , Option []    ["envUsage"]    (NoArg EnvUsage)      "print a list of functions that got inserted at some point (regardless if successful or not), and how often"
  , Option ['o'] ["short"]       (NoArg Shortest)      "prefer shorter solutions"
  , Option ['f'] ["first"]       (NoArg FirstSol)      "stop after finding the first solution"
  , Option []    ["fix"]         (NoArg AllowFix)      "allow the `fix` function in the environment"
  , Option ['b'] ["best"]        (NoArg Best)          "calculate all solutions, and print the best one"
  , Option ['u'] ["allowUnused"] (NoArg Unused)        "allow unused input variables"
  , Option ['c'] ["patternMatchMC"] (NoArg PatternMatchMC) "pattern match on multi-constructor data types (might lead to hang-ups at the moment)"
  , Option ['q'] ["fullqualification"] (NoArg $ Qualification 2) "fully qualify the identifiers in the output"
  , Option []    ["somequalification"] (NoArg $ Qualification 1) "fully qualify non-operator-identifiers in the output"
  , Option ['w'] ["allowConstraints"] (NoArg Constraints) "allow additional (unproven) constraints in solutions"
  ]

mainOpts :: [String] -> IO ([Flag], [String])
mainOpts argv =
  case getOpt Permute options argv of
    (o, n, []  )  | inputs <- [x | (Input x) <- o] ++ n
                  -> if null o && null inputs
                    then return ([Help], inputs)
                    else return (o, inputs)
    (_,  _, errs) -> ioError (userError (concat errs ++ fullUsageInfo))

fullUsageInfo :: String
fullUsageInfo = usageInfo header options
  where
    header = "Usage: exference [OPTION...]"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  argv <- getArgs
  defaultEnvPath <- getDataFileName "environment"
  (flags, inputs) <- mainOpts argv
  let verbosity = sum $ [x | Verbose x <- flags ]
  let qualification = fromMaybe 0 $ listToMaybe [x | Qualification x <- flags]
  let
    printVersion = do
      putStrLn $ "exference version " ++ showVersion version
  if | [Version] == flags   -> printVersion
     | Help    `elem` flags -> putStrLn fullUsageInfo
     | otherwise -> runMultiRWSTNil_ $ do
        when (Version `elem` flags || verbosity>0) $ lift printVersion
        -- ((eSignatures, StaticClassEnv clss insts), messages) <- runWriter <$> parseExternal testBaseInput'
        let envDir = fromMaybe defaultEnvPath $ listToMaybe [d | EnvDir d <- flags]
        when (verbosity>0) $ lift $ do
          putStrLn $ "[Environment]"
          putStrLn $ "reading environment from " ++ envDir
        (sourceEnvironment, messages :: [String]) <-
          withMultiWriterAW $ environmentFromPath envDir
        let eSignatures = sourceFunctions sourceEnvironment
            eDeconss = sourceDeconstructors sourceEnvironment
            sEnv = sourceClasses sourceEnvironment
            validNames = sourceTypeNames sourceEnvironment
            tdeclMap = sourceTypeSynonyms sourceEnvironment
        let clss = sClassEnv_tclasses sEnv
            insts = sClassEnv_instances sEnv
            inventoryResult = toSynthesisSourceInventory sourceEnvironment
        when (verbosity>0 && not (null messages)) $ lift $
          forM_ messages $ \m -> putStrLn $ "environment warning: " ++ m
        case inventoryResult of
          Left failure -> lift $ putStrLn $
            "could not validate source environment: " ++ show failure
          Right _ -> pure ()
        when (PrintEnv `elem` flags) $ lift $ do
          when (verbosity>0) $ putStrLn "[Environment]"
          mapM_ print $ M.elems tdeclMap
          mapM_ print $ M.elems clss
          mapM_ print $ [(i,x)| (i,xs) <- M.toList insts, x <- xs]
          mapM_ print $ eSignatures
          mapM_ print $ eDeconss
        case (inputs, either (const Nothing) Just inventoryResult) of
          ([], _) -> return ()
          (_, Nothing) -> return ()
          (x:_, Just inventory) -> do
            when (verbosity>0) $ lift $ putStrLn "[Custom Input]"
            eParsedType <- runExceptT $ parseTypeWithKinds
              (SharedInventory.inventoryKindAssumptions inventory)
              (sClassEnv_tclasses sEnv)
              Nothing
              validNames
              tdeclMap
              (haskellSrcExtsParseMode "inputtype")
              x
            case eParsedType of
              Left err -> lift $ do
                putStrLn $ "could not parse input type: " ++ diagnosticMessage err
              Right (parsedType, tVarIndex) -> do
                let typeStr = showHsType tVarIndex parsedType
                when (verbosity>0) $ lift $ putStrLn $ "input type parsed as: " ++ typeStr
                let unresolvedIdents = findInvalidNames validNames parsedType
                when (not $ null unresolvedIdents) $ lift $ do
                  putStrLn $ "warning: unresolved idents in input: "
                           ++ intercalate ", " (nub $ show <$> unresolvedIdents)
                  putStrLn $ "(this may be harmless, but no instances will be connected to these.)"
                let hidden = if AllowFix `elem` flags then [] else ["fix", "forever", "iterateM_"]
                let namedBindings = filterBindingsSimple hidden eSignatures
                    filteredBindings = filter bindingIsSupported namedBindings
                    filteredDeconstructors = filter deconstructorIsSupported eDeconss
                    omitted = length namedBindings - length filteredBindings
                      + length eDeconss - length filteredDeconstructors
                when (verbosity > 0 && omitted > 0) $ lift $ putStrLn $
                  "omitting " ++ show omitted
                    ++ " environment bindings with unsupported nested foralls"
                let input = ExferenceInput
                      { input_goalType = parsedType
                      , input_envFuncs = filteredBindings
                      , input_envDeconsS = filteredDeconstructors
                      , input_envClasses = sEnv
                      , input_allowUnused = Unused `elem` flags
                      , input_allowConstraints = Constraints `elem` flags
                      , input_allowConstraintsStopStep = 8192
                      , input_multiPM = PatternMatchMC `elem` flags
                      , input_maxSteps = 65536
                      , input_maxQueueSize = Just 8192
                      , input_maxDepth = Nothing
                      , input_heuristicsConfig = if Shortest `elem` flags
                          then testHeuristicsConfig
                          else testHeuristicsConfig
                            { heuristics_solutionLength = 0.0 }
                      }
                when (verbosity>0) $ lift $ do
                  putStrLn $ "full input:"
                  print input
                -- The default selector searches with constraints enabled so it
                -- can use them as a fallback, then prefers constraint-free
                -- answers.  Every presentation mode consumes the same lazy
                -- chunk trace while retaining validation and completion
                -- information that the historical list adapters discarded.
                let preferNoConstraints = not $ any (`elem` flags)
                      [PrintAll, EnvUsage, FirstSol, Best, Constraints]
                    searchInput
                      | preferNoConstraints = input
                          {input_allowConstraints = True}
                      | otherwise = input
                case findExpressionsWithStatsEither searchInput of
                  Left err -> lift $ putStrLn $
                    "invalid search input: " ++ show err
                  Right chunks -> do
                    if
                      | PrintAll `elem` flags -> lift $ do
                          when (verbosity>0) $
                            putStrLn "[running complete search ..]"
                          printAllResults qualification tVarIndex chunks
                      | EnvUsage `elem` flags -> lift $ do
                          when (verbosity>0) $
                            putStrLn "[running complete search ..]"
                          let stats = maybe M.empty chunkBindingUsages
                                $ lastMaybe chunks
                              highest = take 8
                                $ sortBy (flip $ comparing snd)
                                $ M.toList stats
                          print highest
                      | otherwise -> do
                          selection <- lift $ if
                            | FirstSol `elem` flags -> do
                                when (verbosity>0) $
                                  putStrLn "[selecting first expression ..]"
                                pure $ selectOneExpression chunks
                            | Best `elem` flags -> do
                                when (verbosity>0) $
                                  putStrLn "[selecting best expressions ..]"
                                pure $ selectBestNExpressions 999 chunks
                            | Constraints `elem` flags -> do
                                when (verbosity>0) $ putStrLn
                                  "[selecting with constrained lookahead ..]"
                                pure $ selectFirstBestExpressionsLookahead
                                  256 chunks
                            | otherwise -> do
                                when (verbosity>0) $ putStrLn
                                  "[selecting with constraint-free preference ..]"
                                pure $
                                  selectFirstBestExpressionsLookaheadPreferNoConstraints
                                    256 chunks
                          lift $ printSelection qualification tVarIndex
                            selection

        -- printChecks     testHeuristicsConfig env
        -- printStatistics testHeuristicsConfig env

        -- print $ compileDict testDictRatings $ eSignatures
        -- print $ parseConstrainedType defaultClassEnv $ "(Show a) => [a] -> String"
        -- print $ inflateHsConstraints a b
        {-
        let t :: HsType
            t = read "m a->( ( a->m b)->( m b))"
        print $ t
        -}

printSelection
  :: Int
  -> TypeVarIndex
  -> SearchSelection [ExferenceOutputElement]
  -> IO ()
printSelection _ _ (SearchSelection status []) =
  putStrLn $ noResultsMessage status
printSelection qualification tVarIndex (SearchSelection _ results) =
  mapM_ (printResult qualification tVarIndex) results

printAllResults
  :: Int
  -> TypeVarIndex
  -> [ExferenceChunkElement]
  -> IO ()
printAllResults qualification tVarIndex = go Nothing False
 where
  go status foundAny [] = when (not foundAny) $
    putStrLn $ noResultsMessage status
  go _ foundAny (chunk : chunks) = case chunkElements chunk of
    [] -> go (Just $ chunkStatus chunk) foundAny chunks
    results -> do
      mapM_ (printResult qualification tVarIndex) results
      go (Just $ chunkStatus chunk) True chunks

printResult :: Int -> TypeVarIndex -> ExferenceOutputElement -> IO ()
printResult qualification tVarIndex
    (expression, constraints, ExferenceStats steps depth queueSize) = do
  rendered <- either
    (fail . ("cannot render checked search result: " ++) . show)
    pure
    $ CoreExpression.renderExpression
        (CoreExpression.qualificationFromLevel qualification) expression
  putStrLn rendered
  when (not $ null constraints) $ do
    let constraintStrings = map (showHsConstraint tVarIndex)
          $ S.toList
          $ S.fromList constraints
    putStrLn $ "but only with additional constraints: "
      ++ intercalate ", " constraintStrings
  putStrLn $ replicate 40 ' '
    ++ "(depth " ++ show depth
    ++ ", " ++ show steps ++ " steps, "
    ++ show queueSize ++ " max pqueue size)"

noResultsMessage :: Maybe SearchStatus -> String
noResultsMessage Nothing = "[no search states were produced]"
noResultsMessage (Just status) = case toSearchProgress status of
  Right (SharedSearch.Completed SharedSearch.Finished) ->
    "[no results: search space exhausted]"
  Right (SharedSearch.Completed (SharedSearch.Truncated reasons))
    | SharedSearch.StepLimitReached `elem` NonEmpty.toList reasons ->
        "[no results found before the step limit; inhabitation is undecided]"
    | otherwise ->
        "[no results found after pruning; inhabitation is undecided]"
  Right SharedSearch.Continuing ->
    "[no results in the inspected search prefix; inhabitation is undecided]"
  Left statusError ->
    "[invalid internal search status: " ++ show statusError ++ "]"

lastMaybe :: [a] -> Maybe a
lastMaybe = List.foldl' (\_ value -> Just value) Nothing

filterBindingsSimple :: [String] -> [FunctionBinding] -> [FunctionBinding]
filterBindingsSimple excluded = filter $ \binding ->
  case qualifiedNameOccurrence $ functionName binding of
    SharedName.IdentifierOccurrence _ spelling -> spelling `notElem` excluded
    SharedName.OperatorOccurrence _ spelling -> spelling `notElem` excluded
    SharedName.SpecialOccurrence SharedName.FunctionConstructor ->
      "->" `notElem` excluded
    SharedName.SpecialOccurrence _ -> True

bindingIsSupported :: FunctionBinding -> Bool
bindingIsSupported binding = all (not . containsForall)
  $ functionResult binding : functionParameters binding

deconstructorIsSupported :: DeconstructorBinding -> Bool
deconstructorIsSupported binding = all (not . containsForall)
  $ deconstructorInput binding
  : concatMap constructorFields (deconstructorConstructors binding)

containsForall :: HsType -> Bool
containsForall TypeForall{} = True
containsForall (TypeArrow parameter result) =
  containsForall parameter || containsForall result
containsForall (TypeApp function argument) =
  containsForall function || containsForall argument
containsForall _ = False

_tryParse :: Bool -> String -> IO ()
_tryParse shouldBangPattern s = do
  content <- readFile $ "/home/lsp/asd/prog/haskell/exference/BaseContext/preprocessed/"++s++".hs"
  let exts1 = (if shouldBangPattern then (BangPatterns:) else id)
              [ UnboxedTuples
              , TypeOperators
              , MagicHash
              , NPlusKPatterns
              , ExplicitForAll
              , ExistentialQuantification
              , TypeFamilies
              , PolyKinds
              , DataKinds ]
      exts2 = map EnableExtension exts1
  case parseModuleWithMode (ParseMode (s++".hs")
                                      Haskell2010
                                      exts2
                                      False
                                      False
                                      Nothing
                                      False
                           )
                           content of
    f@(ParseFailed _ _) -> do
      print f
    ParseOk _modul -> do
      putStrLn s
      --mapM_ putStrLn $ map (either id show)
      --               $ getBindings defaultClassEnv mod
      --mapM_ putStrLn $ map (either id show)
      --               $ getDataConss mod
      --mapM_ putStrLn $ map (either id show)
      --               $ getClassMethods defaultClassEnv mod
