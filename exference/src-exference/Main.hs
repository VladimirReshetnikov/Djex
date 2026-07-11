{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternGuards #-}

module Main
  ( main
  )
where



import Language.Haskell.Exference.Core ( ExferenceChunkElement(..)
                                       , ExferenceHeuristicsConfig(..)
                                       , findExpressionsWithStats
                                       , validateExferenceInput )
import Language.Haskell.Exference
import Language.Haskell.Exference.ExpressionToHaskellSrc
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc
import Language.Haskell.Exference.Diagnostic (diagnosticMessage)
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.EnvironmentParser

import Language.Haskell.Exference.Core.Types
import qualified Language.Haskell.Synthesis.Name as SharedName
import Language.Haskell.Exference.Core.ExpressionSimplify

import Control.Monad ( when, forM_ )
import Data.List ( sortBy, intercalate, nub )
import Data.Ord ( comparing )
import Data.Maybe ( listToMaybe, fromMaybe, maybeToList )
import Control.Monad.Writer.Strict
import qualified Data.Map as M
import qualified Data.Set as S

import Language.Haskell.Exts.Parser ( parseModuleWithMode
                                    , ParseResult (..)
                                    , ParseMode (..) )
import Language.Haskell.Exts.Extension ( Language (..)
                                       , Extension (..)
                                       , KnownExtension (..) )
import Language.Haskell.Exts.Pretty

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
        ( (eSignatures
          , eDeconss
          , sEnv@(StaticClassEnv clss insts)
          , validNames
          , tdeclMap )
         ,messages :: [String] ) <- withMultiWriterAW $ environmentFromPath envDir
        when (verbosity>0 && not (null messages)) $ lift $
          forM_ messages $ \m -> putStrLn $ "environment warning: " ++ m
        when (PrintEnv `elem` flags) $ lift $ do
          when (verbosity>0) $ putStrLn "[Environment]"
          mapM_ print $ M.elems tdeclMap
          mapM_ print $ clss
          mapM_ print $ [(i,x)| (i,xs) <- M.toList insts, x <- xs]
          mapM_ print $ eSignatures
          mapM_ print $ eDeconss
        case inputs of
          []    -> return () -- probably impossible..
          (x:_) -> do
            when (verbosity>0) $ lift $ putStrLn "[Custom Input]"
            eParsedType <- runExceptT $ parseType (sClassEnv_tclasses sEnv)
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
                case validateExferenceInput input of
                  Left err -> lift $ putStrLn $ "invalid search input: " ++ show err
                  Right () -> return ()
                if
                  | PrintAll `elem` flags -> do
                      when (verbosity>0) $ lift $ putStrLn "[running findExpressions ..]"
                      let rs = findExpressions input
                      if null rs
                        then lift $ putStrLn "[no results]"
                        else forM_ rs
                          $ \(e, constrs, ExferenceStats n d m) -> do
                            let hsE = convert qualification $ simplifyExpression e
                            lift $ putStrLn $ prettyPrint hsE
                            when (not $ null constrs) $ do
                              let constrStrs = map (showHsConstraint tVarIndex)
                                             $ S.toList
                                             $ S.fromList
                                             $ constrs
                              lift $ putStrLn $ "but only with additional contraints: " ++ intercalate ", " constrStrs
                            lift $ putStrLn $ replicate 40 ' ' ++ "(depth " ++ show d
                                        ++ ", " ++ show n ++ " steps, " ++ show m ++ " max pqueue size)"
                  | EnvUsage `elem` flags -> lift $ do
                      when (verbosity>0) $ putStrLn "[running findExpressionsWithStats ..]"
                      let stats = chunkBindingUsages $ last $ findExpressionsWithStats input
                          highest = take 8 $ sortBy (flip $ comparing snd) $ M.toList stats
                      putStrLn $ show $ highest
                  | otherwise -> do
                      r <- lift $ if
                        | FirstSol `elem` flags -> do
                            when (verbosity>0) $ putStrLn "[running findOneExpression ..]"
                            return $ maybeToList $ findOneExpression input
                        | Best `elem` flags -> do
                            when (verbosity>0) $ putStrLn "[running findBestNExpressions ..]"
                            return $ findBestNExpressions 999 input
                        | Constraints `elem` flags -> do
                            when (verbosity>0) $ putStrLn "[running findFirstBestExpressionsLookahead ..]"
                            return $ findFirstBestExpressionsLookahead 256 input
                        | otherwise -> do
                            when (verbosity>0) $ putStrLn "[running findFirstBestExpressionsLookaheadPreferNoConstraints ..]"
                            return $ findFirstBestExpressionsLookaheadPreferNoConstraints 256
                              input {input_allowConstraints = True}
                      case r :: [ExferenceOutputElement] of
                        [] -> lift $ putStrLn "[no results]"
                        rs -> rs `forM_` \(e, constrs, ExferenceStats n d m) -> do
                            let hsE = convert qualification $ simplifyExpression e
                            lift $ putStrLn $ prettyPrint hsE
                            when (not $ null constrs) $ do
                              let constrStrs = map (showHsConstraint tVarIndex)
                                             $ S.toList
                                             $ S.fromList
                                             $ constrs
                              lift $ putStrLn $ "but only with additional contraints: " ++ intercalate ", " constrStrs
                            lift $ putStrLn $ replicate 40 ' ' ++ "(depth " ++ show d
                                       ++ ", " ++ show n ++ " steps, " ++ show m ++ " max pqueue size)"

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
