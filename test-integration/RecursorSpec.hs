-- | Ordinary recursive programs from one generic, explicitly supplied fold.
-- Search sees the native List declaration and the fold's type, never its body
-- or implementations of map, append, or length. GHC independently compiles and
-- executes exact checked candidate expressions under their full signatures.
module RecursorSpec (tests) where

import Control.Exception (bracket, try)
import Control.Monad (forM, forM_)
import Data.List (intercalate, nub)
import Language.Haskell.Djex hiding (Backend (..), backend, backendName, consName, listName)
import Language.Haskell.Djex.Exference.HaskellSrc (parseExferenceRequest)
import qualified Language.Haskell.Synthesis.TypedGenerated.Haskell as TypedHaskell
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hClose, hPutStr, openTempFile)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Text.Read (readMaybe)


data Backend = DjinnBackend | ExferenceBackend
  deriving (Eq, Show)

data Specification = Specification
  { operation :: String
  , signature :: String
  , observations :: String -> String
  , contradictoryObservations :: String -> String
  }

data SearchOutput = SearchOutput
  { checkedTerms :: [(String, Bool)]
    -- ^ Exact source expression and whether it uses the supplied fold.
  , searchDescription :: String
  }

tests :: TestTree
tests = testGroup "Supplied generic recursors"
  [ testGroup (backendName backend) $
      [ testCase ("synthesize and execute " ++ operation specification) $
          executeSpecification backend specification
      | specification <- specifications
      ] ++
      [ testCase "do not invent an element for the empty recursive input" $ do
          result <- synthesize backend "uninhabited" "forall a. [a] -> a"
          assertEqual (searchDescription result) [] $ checkedTerms result
      ]
  | backend <- [DjinnBackend, ExferenceBackend]
  ]

backendName :: Backend -> String
backendName DjinnBackend = "djinn"
backendName ExferenceBackend = "exference"

specifications :: [Specification]
specifications =
  [ Specification
      { operation = "map"
      , signature = "forall a b. (a -> b) -> [a] -> [b]"
      , observations = \f -> conjunction
          [ f ++ " (== (11 :: Int)) [] == ([] :: [Bool])"
          , f ++ " (== (11 :: Int)) [11] == [True]"
          , f ++ " (== (11 :: Int)) [11, 29, 37, 41, 53] == [True, False, False, False, False]"
          , f ++ " (\\b -> if b then (7 :: Int) else 11) [True, False, True, False, True] == [7, 11, 7, 11, 7]"
          , f ++ " (+ (3 :: Int)) [11, 29, 37, 41, 53] == [14, 32, 40, 44, 56]"
          ]
      , contradictoryObservations = \f -> conjunction
          [ f ++ " (\\b -> b) ([] :: [Bool]) == []"
          , f ++ " (\\b -> b) ([] :: [Bool]) == [True]"
          ]
      }
  , Specification
      { operation = "append"
      , signature = "forall a. [a] -> [a] -> [a]"
      , observations = \f -> conjunction
          [ f ++ " ([] :: [Int]) [] == []"
          , f ++ " [] [11, 29 :: Int] == [11, 29]"
          , f ++ " [11, 29 :: Int] [] == [11, 29]"
          , f ++ " [11, 29 :: Int] [37, 41, 53] == [11, 29, 37, 41, 53]"
          , f ++ " [True, False] [True, False, True] == [True, False, True, False, True]"
          ]
      , contradictoryObservations = \f -> conjunction
          [ f ++ " ([] :: [Bool]) [] == []"
          , f ++ " ([] :: [Bool]) [] == [True]"
          ]
      }
  , Specification
      { operation = "length"
      , signature = "forall a r. r -> (r -> r) -> [a] -> r"
      , observations = \f -> conjunction
          [ f ++ " (0 :: Int) (+ 1) ([] :: [Int]) == 0"
          , f ++ " (0 :: Int) (+ 1) [11 :: Int] == 1"
          , f ++ " (0 :: Int) (+ 1) [11, 29 :: Int] == 2"
          , f ++ " (0 :: Int) (+ 1) [11, 29, 37, 41, 53 :: Int] == 5"
          , f ++ " (0 :: Int) (+ 1) [True, False, True] == 3"
          , f ++ " True not [11, 29 :: Int] == True"
          , f ++ " True not [11, 29, 37 :: Int] == False"
          , f ++ " (7 :: Int) (\\x -> 2 * x + 1) [True, False] == 31"
          ]
      , contradictoryObservations = \f -> conjunction
          [ f ++ " (0 :: Int) (+ 1) ([] :: [Bool]) == 0"
          , f ++ " (0 :: Int) (+ 1) ([] :: [Bool]) == 1"
          ]
      }
  ]

conjunction :: [String] -> String
conjunction = intercalate " && " . map (\value -> "(" ++ value ++ ")")

-- The complete source inventory has exactly one datatype and one value.
-- Keeping a and r distinct in both backends is essential: r must specialize
-- to [b] for map and to independent accumulator types for generalized length.
declarations :: IO [Declaration String kindVariable ()]
declarations = do
  list <- expectRight $ parseName "[]"
  cons <- expectRight $ parseName ":"
  foldName <- expectRight $ mkIdentifier "foldList"
  let a = TypeVariable "a"
      r = TypeVariable "r"
      listOf ty = TypeApplication (TypeConstructor list) ty
      foldType = ForallType ["a", "r"] [] $
        FunctionType (FunctionType a $ FunctionType r r) $
          FunctionType r $ FunctionType (listOf a) r
  pure
    [ DataTypeDeclaration () list [TypeParameter "a" Nothing]
        [DataConstructor () list [], DataConstructor () cons [a, listOf a]]
    , ValueDeclaration $ ValueSignature () foldName foldType
    ]

-- Resource limits are explicit acceptance bounds, not completeness claims.
synthesize :: Backend -> String -> String -> IO SearchOutput
synthesize backend label source = do
  sourceDeclarations <- declarations
  foldName <- expectRight $ mkIdentifier "foldList"
  listName <- expectRight $ parseName "[]"
  consName <- expectRight $ parseName ":"
  target <- expectRight $ mkIdentifier $ "recursor_" ++ backendName backend ++ "_" ++ label
  let allowedGlobals = [foldName, listName, consName]
      checkGlobals expression = do
        let globals = nub $ expressionGlobals expression
        assertBool ("candidate acquired a target or undeclared provider: " ++ show globals) $
          all (\global -> elem global allowedGlobals) globals
        pure $ elem foldName globals
      checkInventory environment = do
        assertEqual "the source environment gained declarations" 2 $
          length $ environmentDeclarations environment
        assertEqual "the source environment gained a non-generic value provider" [foldName]
          [valueName value | ValueDeclaration value <- environmentDeclarations environment]
  case backend of
    DjinnBackend -> do
      environment <- expectRight (mkEnvironment sourceDeclarations :: Either
        (EnvironmentError DjinnTypeVariable) DjinnEnvironment)
      checkInventory environment
      session <- expectRight $ mkDjinnSession environment
      checkInventory $ djinnSessionEnvironment session
      request <- expectRight $ parseDjinnRequest session
        defaultQueryOptions
          { optionCutoff = 1024, optionAlternatives = True, optionSorted = False
          , optionStrategy = Interleave, optionBudget = Just 100000
          }
        target "supplied-recursors" source
      result <- expectRight $ runDjinnTypedQuery session request
      terms <- forM (batchCandidates $ resultSearch result) $ \candidate -> do
        let clause = candidateOutput $ typedCandidateCompatibility candidate
        graph <- either
          (\failure -> fail $ show backend ++ " " ++ label ++ ": " ++ show failure ++ "; " ++ show clause)
          pure $ typedCandidateTermGraph candidate
        assertEqual "Djinn source graph changed its associated expression"
          clause $ eraseTermGraphToFunctionClause (clauseName clause) graph
        usesFold <- checkGlobals $ eraseTermGraph graph
        rendered <- expectRight $ TypedHaskell.renderHaskellTermGraph
          (defaultRenderOptions id) graph
        pure (rendered, usesFold)
      pure $ SearchOutput terms $ show
        (backend, label, resultEvidence result, batchProgress $ resultSearch result)
    ExferenceBackend -> do
      let variable "a" = FlexibleVariable 0
          variable _ = FlexibleVariable 1
      environment <- expectRight (mkEnvironment
        (map (mapDeclarationTypeVariables variable) sourceDeclarations) :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      checkInventory environment
      session <- expectRight $ mkExferenceSession environment
      checkInventory $ exferenceSessionEnvironment session
      request <- expectRight $ parseExferenceRequest session
        defaultExferenceOptions
          { exferenceMaximumSteps = 100000, exferenceMaximumQueueSize = Just 1024
          , exferenceAllowUnused = True, exferenceMultiConstructorPatterns = True
          }
        target "supplied-recursors" source
      results <- expectRight $ runExferenceTypedQuery session request
      let candidates = take 1024 $ concatMap (batchCandidates . resultSearch) results
      terms <- forM candidates $ \candidate -> do
        let clause = candidateOutput $ typedCandidateCompatibility candidate
        graph <- either
          (\failure -> fail $ show backend ++ " " ++ label ++ ": " ++ show failure ++ "; " ++ show clause)
          pure $ typedCandidateTermGraph candidate
        assertEqual "Exference source graph changed its associated expression"
          clause $ eraseTermGraphToFunctionClause (clauseName clause) graph
        usesFold <- checkGlobals $ eraseTermGraph graph
        -- Exference retains the opened outer quantifiers. These goals have
        -- rank-1 bodies, so check the exact graph erasure at the original full
        -- signature instead of inventing a fresh closed graph root.
        rendered <- expectRight $ renderExpression
          (defaultRenderOptions $ \typeVariable -> "v" ++ show typeVariable)
          $ eraseTermGraph graph
        pure (rendered, usesFold)
      pure $ SearchOutput terms $
        show (backend, label) ++ ": at most 1024 candidates, 100000 steps, queue 1024"

executeSpecification :: Backend -> Specification -> IO ()
executeSpecification backend specification = do
  result <- synthesize backend (operation specification) (signature specification)
  let candidates = checkedTerms result
  assertBool ("no candidates: " ++ searchDescription result) $ not $ null candidates
  assertBool ("no candidate applied the generic fold: " ++ searchDescription result) $
    any snd candidates
  let named = zipWith
        (\index (term, usesFold) -> ("recursor_impl_" ++ show index, term, usesFold))
        [0 :: Int ..] candidates
      fixture = unlines $
        [ "{-# LANGUAGE RankNTypes, ImpredicativeTypes, ScopedTypeVariables, TypeApplications #-}"
        , "module Main where"
        , "foldList :: forall a r. (a -> r -> r) -> r -> [a] -> r"
        , "foldList step zero [] = zero"
        , "foldList step zero (x : xs) = step x (foldList step zero xs)"
        ] ++ concat
        [ [name ++ " :: " ++ signature specification, name ++ " = " ++ term]
        | (name, term, _) <- named
        ] ++
        [ "accepted :: [Int]"
        , "accepted = [index | (index, passes) <- [" ++ intercalate ", "
            [ "(" ++ show index ++ ", " ++ show usesFold ++ " && (" ++ observations specification name ++ "))"
            | (index, (name, _, usesFold)) <- zip [0 :: Int ..] named
            ] ++ "], passes]"
        , "falseControls :: [Bool]"
        , "falseControls = [" ++ intercalate ", "
            ["(" ++ contradictoryObservations specification name ++ ")" | (name, _, _) <- named] ++ "]"
        , "main :: IO ()"
        , "main = print (accepted, length falseControls, or falseControls)"
        ]
  withTemporaryModule fixture $ \sourcePath -> do
    replay <- timeout 60000000 $ readProcessWithExitCode "runghc" [sourcePath] ""
    case replay of
      Just (ExitSuccess, output, errors) -> case
          readMaybe output :: Maybe ([Int], Int, Bool) of
        Just (accepted, observedFalseControls, anyFalsePassed) -> do
          assertBool (unlines
            [ "no checked implementation passed the recursive observations"
            , searchDescription result, errors, fixture ]) $ not $ null accepted
          assertEqual "the independent process skipped candidate false controls"
            (length candidates) observedFalseControls
          assertEqual "a contradictory behavioral predicate accepted a candidate"
            False anyFalsePassed
          forM_ accepted $ \index -> assertBool "GHC reported an invalid candidate index" $
            index >= 0 && index < length candidates
        Nothing -> fail $ "malformed GHC execution result: " ++ output ++ errors
      _ -> fail $ "independent recursor compilation/execution failed: " ++ show replay ++ "\n" ++ fixture

expectRight :: Show error => Either error value -> IO value
expectRight = either (fail . show) pure

withTemporaryModule :: String -> (FilePath -> IO value) -> IO value
withTemporaryModule source action = do
  temporaryDirectory <- getTemporaryDirectory
  bracket (openTempFile temporaryDirectory "djex-supplied-recursors.hs") cleanup $
    \(path, handle) -> do
      hPutStr handle source
      hClose handle
      action path
 where
  cleanup (path, handle) = do
    _ <- try (hClose handle) :: IO (Either IOError ())
    removeFile path
