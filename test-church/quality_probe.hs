-- Focused public-API quality acceptance. Search receives signatures only;
-- the total provider bodies below are used exclusively by external GHC replay.
module Main (main) where

import Control.Exception (SomeException, evaluate, throwIO, try)
import Control.Monad (forM, unless, when)
import Data.List (intercalate, nub)
import qualified Data.Map.Strict as Map
import Language.Haskell.Djex
import Language.Haskell.Djex.Exference.HaskellSrc (parseExferenceRequest)
import GHC.Clock (getMonotonicTimeNSec)
import System.CPUTime (getCPUTime)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitSuccess), exitFailure)
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

data Example = Example String String
data Observed = Observed
  { emitted :: String
  , measured :: CandidateQuality
  , providers :: [Name]
  }

examples :: [Example]
examples =
  [ Example "projection" "forall a. a -> a -> a"
  , Example "nil" "forall a r. (a -> r -> r) -> r -> r"
  , Example "tuple" "forall a b. a -> b -> (a, b)"
  , Example "repeat" "forall a. a -> (a, a)"
  , Example "impredicative" "forall f. (forall a. a -> f a) -> f (forall b. b -> b)"
  , Example "ambient" "forall x f. (forall a. a -> f a) -> f (forall b. x -> b -> b)"
  , Example "provider" "Token"
  ]

-- Every policy receives the same limits. The frontier is finite before
-- ranking; the ordinary positive search bound is not refunded by deduplication.
window, shown, steps, seconds :: Int
window = 12
shown = 4
steps = 10000
seconds = 15

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  let directory = case args of
        [path] -> path
        [] -> "test-church/results/quality"
        _ -> error "usage: quality_probe [REPORT_DIRECTORY]"
  createDirectoryIfMissing True directory
  let metricsPath = directory </> "query-metrics.tsv"
      validationPath = directory </> "validation-status.txt"
  writeFile metricsPath "engine\tpolicy\tcase\tstatus\tretained\twall_seconds\tcpu_seconds\n"
  writeFile validationPath $ unlines
    [ "Synthesis matrix incomplete."
    , "External GHC replay and projection evaluation have not run in this invocation."
    , "Partial query metrics record synthesis checks only, not compiler acceptance."
    ]
  rows <- forM [(engine, policy, example)
               | engine <- ["djinn", "exference"]
               , policy <- ["legacy", "balanced", "compact", "diverse"]
               , example <- examples] $ \(engine, policyName, example@(Example label signature)) -> do
    policy <- right $ parseCandidateRankingPolicy policyName
    wallStart <- getMonotonicTimeNSec
    cpuStart <- getCPUTime
    attempted <- try (timeout (seconds * 1000000) $ do
      results <- synthesize engine policy example
      _ <- evaluate $ length $ show
        [(emitted result, measured result, providers result) | result <- results]
      first <- case results of
        candidate : _ -> pure candidate
        [] -> fail $ "no candidate: " ++ show (engine, policyName, label)
      when (policyName /= "legacy" && label == "provider") $ do
        cheapName <- right $ mkIdentifier "cheap"
        expensiveName <- right $ mkIdentifier "expensive"
        unless (cheapName `elem` providers first
                && expensiveName `notElem` providers first) $
          fail $ "exact provider costs did not prefer cheap: " ++ show (engine, policyName, map emitted results)
      when (policyName /= "legacy" && label == "nil") $
        unless (qualityEliminations (measured first) == 0) $
          fail $ "nil retains avoidable elimination: " ++ show (engine, policyName, map emitted results)
      pure results) :: IO (Either SomeException (Maybe [Observed]))
    cpuEnd <- getCPUTime
    wallEnd <- getMonotonicTimeNSec
    let (status, retained) = case attempted of
          Left _ -> ("failure", "")
          Right Nothing -> ("timeout", "")
          Right (Just results) -> ("synthesis_checks_pass", show $ length results)
        metrics = intercalate "\t"
          [ engine, policyName, label, status, retained
          , show (fromIntegral (wallEnd - wallStart) / 1e9 :: Double)
          , show (fromIntegral (cpuEnd - cpuStart) / 1e12 :: Double)
          ]
    -- appendFile closes and flushes this diagnostic row before any failure
    -- escapes. Compiler acceptance is recorded separately, after the matrix.
    appendFile metricsPath $ metrics ++ "\n"
    putStrLn metrics
    results <- case attempted of
      Left failure -> throwIO failure
      Right Nothing -> fail $ "query timeout: " ++ show (engine, policyName, label)
      Right (Just candidates) -> pure candidates
    let prefix = engine ++ "_" ++ policyName ++ "_" ++ label
        declarations = [(prefix ++ "_" ++ show index, signature, result)
                       | (index, result) <- zip [1 :: Int ..] results]
    pure (engine, policyName, label, declarations)
  writeFile validationPath $ unlines
    [ "Synthesis matrix complete: " ++ show (length rows) ++ " queries."
    , "External GHC replay and projection evaluation have not completed."
    ]
  let declarations = concat [items | (_, _, _, items) <- rows]
      source = unlines $
        [ "{-# LANGUAGE RankNTypes, ImpredicativeTypes, ScopedTypeVariables, TypeApplications #-}"
        , "module Main where"
        , "data Token = Token"
        , "cheap :: () -> Token"
        , "cheap _ = Token"
        , "expensive :: Token"
        , "expensive = Token"
        ] ++ concat
        [[name ++ " :: " ++ signature, name ++ " = " ++ emitted result]
        | (name, signature, result) <- declarations] ++
        [ "main :: IO ()"
        , "main = if and [" ++ intercalate ", "
            ["length (nubInt [" ++ intercalate ", "
                [name ++ " (11 :: Int) 29" | (name, _, _) <- items] ++ "]) == 2"
            | (_, policyName, label, items) <- rows
            , label == "projection", policyName == "diverse"] ++ "]"
        , "  then putStrLn \"projection diversity PASS\" else fail \"projection diversity collapsed\""
        , "nubInt :: [Int] -> [Int]"
        , "nubInt [] = []"
        , "nubInt (x:xs) = x : nubInt (filter (/= x) xs)"
        ]
      sourcePath = directory </> "QualityCandidates.hs"
  writeFile sourcePath source
  writeFile (directory </> "candidates.tsv") $ unlines $
    ["name\ttype\tsize\teliminations\tprovider_cost\texpression"] ++
    [intercalate "\t" [name, signature, show $ qualityTermSize quality,
      show $ qualityEliminations quality, show $ qualityProviderCost quality,
      oneLine $ emitted result]
    | (name, signature, result) <- declarations, let quality = measured result]
  (compilerExit, compilerOut, compilerErrors) <- readProcessWithExitCode "ghc"
    ["-v0", "-fno-code", "-fno-write-interface", "-fforce-recomp", sourcePath] ""
  writeFile (directory </> "ghc.txt") $ compilerOut ++ compilerErrors
  unless (compilerExit == ExitSuccess) $ do
    writeFile validationPath "Synthesis matrix complete; external GHC replay FAILED; projection evaluation not run.\n"
    putStrLn compilerErrors
    exitFailure
  writeFile validationPath "Synthesis matrix complete; external GHC replay PASSED; projection evaluation pending.\n"
  (runtimeExit, runtimeOut, runtimeErrors) <- readProcessWithExitCode "runghc" [sourcePath] ""
  writeFile (directory </> "projection-diversity.txt") $ runtimeOut ++ runtimeErrors
  unless (runtimeExit == ExitSuccess) $ do
    writeFile validationPath "Synthesis matrix complete; external GHC replay PASSED; projection evaluation FAILED.\n"
    putStrLn runtimeErrors
    exitFailure
  writeFile validationPath "Synthesis matrix, external GHC replay, and projection evaluation PASSED.\n"
  putStrLn $ "PASS: " ++ show (length rows) ++ " queries, " ++ show (length declarations)
    ++ " independently GHC-checked terms; diverse projections evaluated at distinct inputs."
 where
  oneLine = map (\character -> if character `elem` "\r\n\t" then ' ' else character)

synthesize :: String -> CandidateRankingPolicy -> Example -> IO [Observed]
synthesize engine policy (Example label signature) = do
  target <- right $ mkIdentifier "qualityCandidate"
  tokenName <- right $ mkIdentifier "Token"
  cheapName <- right $ mkIdentifier "cheap"
  expensiveName <- right $ mkIdentifier "expensive"
  unitName <- right $ tupleName Boxed 0
  let token = TypeConstructor tokenName
      declarations = if label /= "provider" then [] else
        [ AbstractTypeDeclaration () tokenName ProperTypeKind
        -- A neutral inventory contains only its explicit declarations. Supply
        -- the canonical unit constructor so cheap's argument is inhabitable.
        , DataTypeDeclaration () unitName [] [DataConstructor () unitName []]
        , ValueDeclaration $ ValueSignature () expensiveName token
        , ValueDeclaration $ ValueSignature () cheapName $ FunctionType (TypeConstructor unitName) token
        ]
      costs = Map.fromList [(cheapName, 0), (expensiveName, 40)]
      cost name = Map.findWithDefault (defaultCandidateProviderCost name) name costs
      observation renderer candidate = do
        expression <- right $ renderer Unqualified candidate
        let body = functionClauseExpression $ candidateOutput candidate
        pure $ Observed expression (candidateQuality cost body) (nub $ expressionGlobals body)
  if engine == "djinn" then do
    environment <- right $ mkEnvironment declarations
    session <- right $ mkDjinnSession environment
    request <- right $ parseDjinnRequest session defaultQueryOptions
      { optionAlternatives = True, optionSorted = True, optionCutoff = window
      , optionBudget = Just $ toInteger steps
      , optionRanking = policy, optionProviderCosts = costs
      } target "quality-probe" signature
    result <- right $ runDjinnQuery session request
    traverse (observation renderDjinnCandidateExpression) $
      take shown $ batchCandidates $ resultSearch result
  else do
    environment <- right $ mkEnvironment declarations
    session <- right $ mkExferenceSession environment
    request <- right $ parseExferenceRequest session defaultExferenceOptions
      { exferenceAllowUnused = True, exferenceMaximumSteps = steps
      , exferenceCandidateRanking = policy, exferenceProviderCosts = costs
      } target "quality-probe" signature
    results <- right $ runExferenceQuery session request
    -- Even legacy observes the same raw pool before the output cap. Its
    -- shared ranking policy is the identity, retaining encounter order.
    let candidates = selectionCandidates $ selectQualityQueryResults window policy cost
          (functionClauseExpression . candidateOutput)
          (null . candidateResidualConstraints) results
    traverse (observation renderExferenceCandidateExpression) $ take shown candidates

right :: Show failure => Either failure value -> IO value
right = either (fail . show) pure
