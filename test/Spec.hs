module Main (main) where

import Data.Either (isRight)
import Data.List (nub)
import Djinn.Core
  ( Declaration (Function)
  , declare
  , defaultQueryOptions
  , optionBudget
  , parseHType
  , standardEnvironment
  )
import Language.Haskell.Djex
import Language.Haskell.Exference.Core.Name (mkQualifiedName)
import Language.Haskell.Synthesis.Diagnostic (diagnosticCode)
import Language.Haskell.Synthesis.Generated
  ( defaultRenderOptions
  , renderFunctionClause
  )
import Language.Haskell.Synthesis.Name
  ( mkIdentifier
  , mkModuleName
  , mkQualifiedIdentifier
  , parseName
  )
import Language.Haskell.Synthesis.Search
  ( Completion (Finished, Truncated)
  , Progress (Completed)
  , TruncationReason (ChoicePointLimitReached)
  , batchCandidates
  , batchMetadata
  , batchProgress
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Djex facade"
  [ testCase "enumerate backends in stable presentation order" $
      map backend availableBackends @?= [DjinnBackend, ExferenceBackend]
  , testCase "backend metadata is the canonical projection" $
      availableBackends @?= map backendInfo [DjinnBackend, ExferenceBackend]
  , testCase "advertise only the checked backend capabilities" $ do
      backendCapabilities (backendInfo DjinnBackend) @?=
        [DecidingInhabitation, TypeClassConstraints]
      backendCapabilities (backendInfo ExferenceBackend) @?=
        [ HeuristicSearch
        , PrenexPolymorphism
        , RankedCandidates
        , TypeClassConstraints
        ]
  , testCase "do not duplicate capabilities" $
      map backendCapabilities availableBackends @?=
        map (nub . backendCapabilities) availableBackends
  , testCase "stable backend APIs are reachable through a djex dependency" $ do
      assertBool "Djinn API was not reexported" $ isRight $ parseHType "a -> a"
      assertBool "Exference API was not reexported" $
        isRight $ mkQualifiedName [] "id"
      assertBool "synthesis API was not reexported" $ isRight $ parseName "id"
  , testCase "run a checked Djinn session through the shared envelope" $ do
      session <- expectRight $ mkDjinnSession standardEnvironment
      target <- expectRight $ mkIdentifier "swap"
      goal <- expectRight $ parseHType "(a, b) -> (b, a)"
      result <- expectRight $ runDjinnQuery session QueryRequest
        { requestTarget = target
        , requestGoal = goal
        , requestContexts = []
        , requestOptions = defaultQueryOptions
        }
      resultEvidence result @?= ValidatedCandidates
      batchProgress (resultSearch result) @?= Completed Finished
      assertBool "translated formula metadata was lost" $
        not $ null $ djinnTranslatedFormula $ batchMetadata $ resultSearch result
      assertBool "first proof metadata was lost" $
        case djinnFirstExploredProof $ batchMetadata $ resultSearch result of
          Just _ -> True
          Nothing -> False
      case batchCandidates $ resultSearch result of
        candidate : _ ->
          renderFunctionClause (defaultRenderOptions id) candidate @?=
            Right "swap (a, b) = (b, a)"
        [] -> fail "Djinn reported candidate evidence without a candidate"
  , testCase "keep Djinn evidence independent of search completion" $ do
      session <- expectRight $ mkDjinnSession standardEnvironment
      target <- expectRight $ mkIdentifier "peirce"
      goal <- expectRight $ parseHType "((a -> b) -> a) -> a"
      refutation <- expectRight $ runDjinnQuery session QueryRequest
        { requestTarget = target
        , requestGoal = goal
        , requestContexts = []
        , requestOptions = defaultQueryOptions
        }
      resultEvidence refutation @?= ProvedUninhabitable
      batchProgress (resultSearch refutation) @?= Completed Finished
      undecided <- expectRight $ runDjinnQuery session QueryRequest
        { requestTarget = target
        , requestGoal = goal
        , requestContexts = []
        , requestOptions = defaultQueryOptions { optionBudget = Just 0 }
        }
      resultEvidence undecided @?= NoEvidence
      case batchProgress $ resultSearch undecided of
        Completed (Truncated reasons) ->
          assertBool "choice-point truncation reason was lost" $
            ChoicePointLimitReached `elem` reasons
        completion -> fail $ "expected a truncated search, got " ++ show completion
  , testCase "preserve Djinn's target-reference evidence" $ do
      variable <- expectRight $ parseHType "a"
      environment <- expectRight $
        declare (Function "token" variable) standardEnvironment
      session <- expectRight $ mkDjinnSession environment
      target <- expectRight $ mkIdentifier "token"
      result <- expectRight $ runDjinnQuery session QueryRequest
        { requestTarget = target
        , requestGoal = variable
        , requestContexts = []
        , requestOptions = defaultQueryOptions
        }
      resultEvidence result @?= RequiresTargetReference
      batchCandidates (resultSearch result) @?= []
  , testCase "reject targets outside Djinn's output namespace" $ do
      session <- expectRight $ mkDjinnSession standardEnvironment
      qualifier <- expectRight $ mkModuleName "External"
      target <- expectRight $ mkQualifiedIdentifier qualifier "answer"
      goal <- expectRight $ parseHType "a -> a"
      case runDjinnQuery session QueryRequest
          { requestTarget = target
          , requestGoal = goal
          , requestContexts = []
          , requestOptions = defaultQueryOptions
          } of
        Left failure -> diagnosticCode failure @?= Just "DJEX_DJINN_TARGET"
        Right _ -> fail "Djinn accepted a qualified generated definition"
  ]

expectRight :: Show error => Either error value -> IO value
expectRight result = case result of
  Left failure -> fail $ show failure
  Right value -> pure value
