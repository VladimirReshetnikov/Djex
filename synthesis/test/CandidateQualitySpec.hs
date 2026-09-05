module CandidateQualitySpec (candidateQualityTests) where

import Control.Exception (evaluate)
import qualified Data.Map.Strict as Map
import Data.IORef (newIORef, modifyIORef', readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Language.Haskell.Synthesis.CandidateQuality
import Language.Haskell.Synthesis.Generated
import Language.Haskell.Synthesis.Name
import Language.Haskell.Synthesis.Query (queryResultFromCandidates)
import Language.Haskell.Synthesis.Search
import Language.Haskell.Synthesis.Selection (Selection (..))

candidateQualityTests :: TestTree
candidateQualityTests = testGroup "candidate structural quality"
  [ testCase "presets parse and preserve their distinct policies" $
      mapM_ (\policy -> parseCandidateRankingPolicy (candidateRankingPolicyName policy)
        @?= Right policy)
        [LegacyCandidateRanking, defaultCandidateRankingPolicy,
         compactCandidateRankingPolicy, diverseCandidateRankingPolicy]
  , testCase "invalid profile is rejected" $
      assertBool "unknown policy" $ case parseCandidateRankingPolicy "fastest" of
        Left _ -> True
        Right _ -> False
  , testCase "frontier scalar score agrees with inspectable metrics" $ do
      let local = Local "x"
          aggregate = As "whole" $ Constructor (name "Box") [TuplePattern [Bind "x", Bind "y"]]
          patterns = [Bind "x", Wildcard, aggregate]
          expressions = [local, Hole "hole", Global (name "provider"),
            Lambda patterns local, Apply local local,
            VisibleTypeApplication local inferredVisibleTypeArgument,
            Tuple [local, local], Let aggregate local local,
            Case local [(pattern, local) | pattern <- patterns]]
          weights = [CandidateQualityWeights 0 0 0 0,
            CandidateQualityWeights 1 3 2 4, CandidateQualityWeights 1 1 1 0,
            CandidateQualityWeights (10 ^ (80 :: Int)) 7 13 12]
      mapM_ (\profile -> mapM_ (\expression -> do
        let quality = candidateQuality (const 17) expression
            expected = candidateSizeWeight profile * qualityTermSize quality
              + candidateEliminationWeight profile * qualityEliminations quality
              + candidateProviderWeight profile * qualityProviderCost quality
        candidateQualityCost (StructuralCandidateRanking profile) (const 17) expression
          @?= expected) expressions) weights
  , testCase "grouping and alpha names do not change structural cost" $
      candidateQualityCost defaultCandidateRankingPolicy (const 0)
        (Lambda [Bind "x", Bind "y"] (Apply (Local "x") (Local "y")))
      @?= candidateQualityCost defaultCandidateRankingPolicy (const 0)
        (Lambda [Bind "f"] (Lambda [Bind "a"] (Apply (Local "f") (Local "a"))))
  , testCase "constructor identity and provider spelling length are not size" $
      qualityTermSize (candidateQuality (const 0) (Global (name "a") :: Expression String))
        @?= qualityTermSize (candidateQuality (const 0)
          (Global (name "aVeryLongProviderName") :: Expression String))
  , testCase "an unnecessary elimination costs more than its simple value" $ do
      let value = Local "x"
          wrapped = Case (Apply (Global (name "Left")) value)
            [(Constructor (name "Left") [Bind "a"], Local "a"),
             (Constructor (name "Right") [Bind "b"], Local "b")]
      rank compactCandidateRankingPolicy [("case", wrapped), ("value", value)]
        @?= ["value", "case"]
  , testCase "provider overrides use exact qualified identity" $ do
      let expensive = name "Slow.provide"
          cheap = name "Fast.provide"
          costs = Map.fromList [(expensive, 50), (cheap, 0)]
      map fst (rankCandidatesByQuality defaultCandidateRankingPolicy
        (\provider -> Map.findWithDefault 1 provider costs) snd
        [("slow", Global expensive :: Expression String), ("fast", Global cheap)])
        @?= ["fast", "slow"]
  , testCase "a shared provider computation is charged once" $ do
      let call = Apply (Global (name "expensive")) (Local "x")
          shared = Let (Bind "y") call (Tuple [Local "y", Local "y"])
          duplicated = Tuple [call, call]
      qualityProviderCost (candidateQuality (const 100) shared) @?= 100
      qualityProviderCost (candidateQuality (const 100) duplicated) @?= 200
      rank compactCandidateRankingPolicy [("duplicate", duplicated), ("shared", shared)]
        @?= ["shared", "duplicate"]
  , testCase "equal costs retain encounter order and handles" $
      rank compactCandidateRankingPolicy
        [("second", Local "b"), ("first", Local "a")]
        @?= ["second", "first"]
  , testCase "diversity advances different arguments over repeated structure" $ do
      let first = Lambda [Bind "x", Bind "y"] (Local "x")
          renamed = Lambda [Bind "a", Bind "b"] (Local "a")
          second = Lambda [Bind "x", Bind "y"] (Local "y")
      rank compactCandidateRankingPolicy
        [("first", first), ("rename", renamed), ("second", second)]
        @?= ["first", "rename", "second"]
      rank diverseCandidateRankingPolicy
        [("first", first), ("rename", renamed), ("second", second)]
        @?= ["first", "second", "rename"]
  , testCase "quality ranking does not refill a caller's raw window" $ do
      let raw = [("one", Local "x"), ("two", Local "x")]
            ++ error "raw candidate allowance over-read"
      evaluate (length (rankCandidatesByQuality defaultCandidateRankingPolicy
        (const 0) snd (take 2 raw))) >>= (@?= 2)
  , testCase "bounded selection charges rejected and duplicate candidates" $ do
      inspected <- newIORef ([] :: [String])
      let raw = [("reject", Local "x"), ("duplicate1", Local "x"),
                  ("duplicate2", Local "x")]
                ++ error "candidate tail beyond allowance forced"
          result = queryResultFromCandidates $ SearchBatch Continuing () raw
          admit (label, _) = do
            modifyIORef' inspected (++ [label])
            pure $ label /= "reject"
      selected <- selectQualityQueryResultsM 3 defaultCandidateRankingPolicy
        (const 0) snd admit [result]
      map fst (selectionCandidates selected) @?= ["duplicate1", "duplicate2"]
      readIORef inspected >>= (@?= ["reject", "duplicate1", "duplicate2"])
      selectionProgress selected @?=
        Just (Completed $ Truncated $ CandidateLimitReached :| [])
  , testCase "a bounded pool ranks a later simpler term before selection" $ do
      let raw = [("large", Apply (Local "f") (Local "x")),
                 ("small", Local "x")] ++ error "over-read"
          result = queryResultFromCandidates $ SearchBatch Continuing () raw
          selected = selectQualityQueryResults 2 compactCandidateRankingPolicy
            (const 0) snd (const True) [result]
      map fst (selectionCandidates selected) @?= ["small", "large"]
  , testCase "exhausted trace retains real progress without a logical claim" $ do
      let result = queryResultFromCandidates $ SearchBatch (Completed Finished) ()
            [("value", Local "x")]
          selected = selectQualityQueryResults 2 defaultCandidateRankingPolicy
            (const 0) snd (const True) [result]
      selectionProgress selected @?= Just (Completed Finished)
      map fst (selectionCandidates selected) @?= ["value"]
  , testCase "zero observation allowance does not inspect the trace" $ do
      let selected = selectQualityQueryResults 0 compactCandidateRankingPolicy
            (const 0) (id :: Expression String -> Expression String) (const True)
            (error "zero-window trace forced")
      selectionCandidates selected @?= []
      selectionProgress selected @?=
        Just (Completed $ Truncated $ CandidateLimitReached :| [])
  , testCase "destructuring binders are charged as eliminations" $ do
      qualityEliminations (candidateQuality (const 0)
        (Lambda [TuplePattern [Bind "a", Bind "b"]] (Local "a"))) @?= 1
      qualityEliminations (candidateQuality (const 0)
        (Let (Constructor (name "Box") [Bind "a"]) (Local "x") (Local "a"))) @?= 1
  , testCase "legacy mode retains lazy search order" $
      take 1 (rankCandidatesByQuality LegacyCandidateRanking
        (error "legacy provider score forced")
        (error "legacy expression forced" :: Int -> Expression Int)
        ([42] ++ error "legacy tail forced") :: [Int]) @?= [42]
  ]
 where
  rank policy = map fst . rankCandidatesByQuality policy (const 100) snd
  name spelling = either (error . show) id $ parseName spelling
