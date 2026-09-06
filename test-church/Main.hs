-- Whole-corpus acceptance through the public Djex API.  The only search input
-- is cases.tsv: fully expanded types and explicitly classified assumptions.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM, unless, when)
import Data.List (intercalate)
import Language.Haskell.Djex
import Language.Haskell.Djex.Exference.HaskellSrc (parseExferenceRequest)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitSuccess), exitFailure)
import System.FilePath ((</>), takeDirectory)
import System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import Text.Read (readMaybe)

data CorpusCase = CorpusCase
  { caseId :: String
  , caseName :: String
  , caseClassification :: String
  , caseSignature :: String
  , caseAcceptanceSignature :: String
  , caseResolvedSignature :: String
  }

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetBuffering stdout LineBuffering
  args <- getArgs
  let corpusPath = flag "--cases" "test-church/cases.tsv" args
      reportDirectory = flag "--report-dir" "test-church/results" args
      selectedBackend = flag "--backend" "both" args
      backendNames = if selectedBackend == "both" then ["djinn", "exference"] else [selectedBackend]
      selectedNames = splitOn ',' $ flag "--names" "" args
      seconds = numericFlag "--timeout" 5 args
      steps = numericFlag "--steps" 50000 args
  unless (all (`elem` ["djinn", "exference"]) backendNames) $
    fail "--backend must be djinn, exference, or both"
  (inventoryExit, inventoryOutput, inventoryErrors) <- readProcessWithExitCode "python"
    [takeDirectory corpusPath </> "extract_corpus.py", "--check", "--output", takeDirectory corpusPath] ""
  unless (inventoryExit == ExitSuccess) $ fail $ inventoryOutput ++ inventoryErrors
  aliases <- readFile $ takeDirectory corpusPath </> "aliases.hs.inc"
  cases <- traverse parseCase . lines =<< readFile corpusPath
  let selectedCases = filter (\entry -> null selectedNames || caseName entry `elem` selectedNames) cases
  when (null selectedCases) $ fail "no corpus cases selected"
  createDirectoryIfMissing True reportDirectory
  failures <- forM backendNames $ \engine -> do
    putStrLn $ "ENGINE " ++ engine ++ ": " ++ show (length selectedCases) ++ " signatures"
    results <- forM selectedCases $ \entry -> do
      attempted <- try $ timeout (seconds * 1000000) $ do
        definition <- synthesize engine steps entry
        _ <- evaluate $ length definition
        pure definition
      let result = case attempted :: Either SomeException (Maybe String) of
            Left exception -> Left $ "ERROR " ++ show exception
            Right Nothing -> Left "TIMEOUT"
            Right (Just definition) -> Right definition
      putStrLn $ intercalate "\t"
        [either (const "FAIL") (const "SYNTHESIZED") result, caseId entry,
         caseName entry, caseClassification entry, either oneLine (const "") result]
      pure (entry, result)
    let successes = [(entry, definition) | (entry, Right definition) <- results]
        source = sourceModule aliases successes
        sourcePath = reportDirectory </> engine ++ "-generated.hs"
        resultPath = reportDirectory </> engine ++ "-results.tsv"
    writeFile sourcePath source
    writeFile resultPath $ unlines
      [intercalate "\t" [caseId entry, caseName entry, caseClassification entry,
        either (const "failed") (const "synthesized") result,
        either oneLine oneLine result] | (entry, result) <- results]
    (compilerExit, compilerOut, compilerErrors) <- readProcessWithExitCode "ghc"
      ["-v0", "-fno-code", "-fno-write-interface", "-fforce-recomp", sourcePath] ""
    writeFile (reportDirectory </> engine ++ "-ghc.txt") $ compilerOut ++ compilerErrors
    putStrLn $ "GHC " ++ engine ++ ": " ++ show compilerExit
    when (compilerExit /= ExitSuccess) $ putStrLn compilerErrors
    let failedCount = length selectedCases - length successes
    putStrLn $ "RESULT " ++ engine ++ ": synthesized=" ++ show (length successes)
      ++ " failed=" ++ show failedCount ++ " compiler=" ++ show compilerExit
    pure $ failedCount /= 0 || compilerExit /= ExitSuccess
  when (or failures) exitFailure

synthesize :: String -> Int -> CorpusCase -> IO String
synthesize engine steps entry = do
  integerName <- expectRight $ mkIdentifier "Int"
  zeroName <- expectRight $ mkIdentifier "churchZero"
  target <- expectRight $ mkIdentifier $ caseId entry
  let declarations =
        [AbstractTypeDeclaration () integerName ProperTypeKind]
        ++ [ValueDeclaration $ ValueSignature () zeroName $ TypeConstructor integerName
           | caseClassification entry == "needs_int_provider"]
  if engine == "djinn"
    then do
      environment <- expectRight $ mkEnvironment declarations
      session <- expectRight $ mkDjinnSession environment
      request <- expectRight $ parseDjinnRequest session
        defaultQueryOptions
          { optionSorted = False
          , optionCutoff = 1
          , optionBudget = Just $ toInteger steps
          }
        target "church-corpus" $ caseSignature entry
      result <- expectRight $ runDjinnTypedQuery session request
      case batchCandidates $ resultSearch result of
        candidate : _ -> do
          -- The selected clause must carry its own checked source graph.
          -- Render the unchanged compatibility clause only after validating
          -- that the graph preserves that exact clause and full source type.
          graph <- expectRight $ typedCandidateTermGraph candidate
          let compatibility = typedCandidateCompatibility candidate
              sourceQuery = djinnRequestQuery request
              sourceType = fmap FlexibleVariable $ requestContextualType sourceQuery
          unless (eraseTermGraphToFunctionClause (requestTarget sourceQuery) graph
              == candidateOutput compatibility) $
            fail "Djinn source graph does not erase to its exact selected clause"
          root <- maybe (fail "Djinn source graph has no root") pure $
            lookupTermNode (termGraphRoot graph) graph
          -- Preserve genuinely free request identities as well as lexical
          -- forall binders; closing both sides would reject valid open goals.
          unless (alphaEquivalentTypes sourceType $ termNodeType root) $
            fail "Djinn source graph root differs from the full request source type"
          unless (null $ expressionHoles $ eraseTermGraph graph) $
            fail "Djinn source graph contains an unresolved term hole"
          expectRight $ renderDjinnCandidateDefinition Unqualified compatibility
        [] -> fail $ "no candidate; " ++ show (resultEvidence result)
          ++ "; " ++ show (batchProgress $ resultSearch result)
    else do
      environment <- expectRight $ mkEnvironment declarations
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ parseExferenceRequest session
        defaultExferenceOptions
          { exferenceAllowUnused = True
          , exferenceMaximumSteps = steps
          }
        target "church-corpus" $ caseSignature entry
      results <- expectRight $ runExferenceQuery session request
      case concatMap (batchCandidates . resultSearch) results of
        candidate : _ -> do
          unless (null $ candidateResidualConstraints candidate) $
            fail "candidate has unresolved constraints"
          expectRight $ renderExferenceCandidateDefinition Unqualified candidate
        [] -> fail "no candidate within Exference search bounds"

sourceModule :: String -> [(CorpusCase, String)] -> String
sourceModule aliases successes = unlines $
  [ "{-# LANGUAGE RankNTypes, ImpredicativeTypes, ScopedTypeVariables, TypeApplications #-}"
  , "{-# LANGUAGE NoImplicitPrelude #-}"
  , "{-# LANGUAGE UnicodeSyntax, TypeOperators #-}"
  , "module ChurchCorpusGenerated where"
  , "import Prelude (Int, fromInteger, undefined)"
  , "churchZero :: Int"
  , "churchZero = 0"
  , aliases
  , ""
  ] ++ concat
    [ [ "-- " ++ caseName entry ++ ": " ++ caseClassification entry
      , caseId entry ++ " :: " ++ caseSignature entry
      , definition
      , caseId entry ++ "_original :: " ++ caseAcceptanceSignature entry
      , caseId entry ++ "_original = " ++ caseId entry
      , ""
      ] ++
      [ unlines
          [ "-- The sole explicit partial-value boundary; synthesis never sees undefined."
          , caseId entry ++ "_partial :: " ++ caseResolvedSignature entry
          , caseId entry ++ "_partial = " ++ caseId entry ++ " undefined"
          ]
      | caseClassification entry == "requires_partiality"
      ]
    | (entry, definition) <- successes
    ]

parseCase :: String -> IO CorpusCase
parseCase line = case splitOn '\t' line of
  [identifier, sourceName, classification, signature, original, resolved] ->
    pure $ CorpusCase identifier sourceName classification signature original resolved
  _ -> fail $ "invalid corpus row: " ++ line

flag :: String -> String -> [String] -> String
flag _ fallback [] = fallback
flag key _ (name : value : _) | key == name = value
flag key fallback (_ : rest) = flag key fallback rest

numericFlag :: String -> Int -> [String] -> Int
numericFlag key fallback args = case readMaybe $ flag key (show fallback) args of
  Just number | number > 0 -> number
  _ -> error $ key ++ " requires a positive integer"

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn delimiter source = case break (== delimiter) source of
  (part, []) -> [part]
  (part, _ : rest) -> part : splitOn delimiter rest

oneLine :: String -> String
oneLine = map (\character -> if character `elem` "\t\r\n" then ' ' else character)

expectRight :: Show failure => Either failure value -> IO value
expectRight = either (fail . show) pure
