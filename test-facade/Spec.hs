module Main (main) where

import Data.Either (isRight)

import Language.Haskell.Djex
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

main :: IO ()
main = defaultMain $ testGroup "public Djex facade"
  [ testCase "enumerates both checked backends" $
      map backend availableBackends @?= [DjinnBackend, ExferenceBackend]
  , testCase "exports the shared name vocabulary" $
      assertBool "qualified name was rejected" $
        isRight $ parseName "Data.Function.fix"
  , testCase "exports generated-code rendering" $ do
      target <- expectRight $ mkIdentifier "identity"
      renderFunctionClause (defaultRenderOptions id)
          (FunctionClause target [Bind "value"] $ Local "value") @?=
        Right "identity value = value"
  , testCase "exports checked Exference options" $
      exferenceMaximumSteps defaultExferenceOptions @?= 65536
  ]

expectRight :: Show error => Either error value -> IO value
expectRight result = case result of
  Left failure -> fail $ show failure
  Right value -> pure value
