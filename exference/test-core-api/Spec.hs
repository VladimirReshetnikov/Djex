module Main (main) where

import Data.Void (Void)

import Language.Haskell.Djex.Exference
import Language.Haskell.Djex.Exference.FrontendSupport
  ( allocateFreshTypeVariableId
  , mkExferenceRequestWithSourceInfo
  , sealPreparedExferenceSessionWithPolicy
  , sessionClasses
  , sessionTypeNames
  , validateExferenceTarget
  )
import Language.Haskell.Exference.Core.Declaration
  ( prepareNeutralSynthesisInventory )
import Language.Haskell.Synthesis.Inventory
  ( InventoryError
  , mkInventory
  )
import Language.Haskell.Synthesis.KindInference
  ( KindInventoryPolicy (OpenKindInventory) )
import Language.Haskell.Synthesis.Name (mkIdentifier)
import Language.Haskell.Synthesis.Query
  ( QueryRequest (..)
  , resultSearch
  )
import Language.Haskell.Synthesis.Search (batchCandidates)
import Language.Haskell.Synthesis.Type
  ( Type (FunctionType, TypeVariable)
  , Variable (FlexibleVariable)
  )
import Test.Tasty (defaultMain)
import Test.Tasty.HUnit
  ( (@?=)
  , assertBool
  , testCase
  )

main :: IO ()
main = defaultMain $ testCase
    "the parser-neutral frontend-support boundary is complete" $ do
  inventory <- expectRight
    (mkInventory OpenKindInventory []
      :: Either
          (InventoryError ExferenceTypeVariable Void)
          ExferenceInventory)
  prepared <- expectRight $ prepareNeutralSynthesisInventory inventory
  session <- expectRight
    $ sealPreparedExferenceSessionWithPolicy [] mempty prepared

  sessionTypeNames session @?= []
  sessionClasses session @?= mempty
  allocateFreshTypeVariableId mempty @?= Just 0

  target <- expectRight $ mkIdentifier "identity"
  checkedTarget <- expectRight $ validateExferenceTarget target
  let variable = FlexibleVariable 0
      goal = FunctionType (TypeVariable variable) (TypeVariable variable)
  request <- expectRight
    $ mkExferenceRequestWithSourceInfo mempty Nothing QueryRequest
      { requestTarget = checkedTarget
      , requestGoal = goal
      , requestContexts = []
      , requestOptions = defaultExferenceOptions
      }
  results <- expectRight $ runExferenceQuery session request
  assertBool "the core-only adapter found no identity candidate"
    $ any (not . null . batchCandidates . resultSearch) results

expectRight :: Show error => Either error value -> IO value
expectRight result = case result of
  Left failure -> fail $ show failure
  Right value -> pure value
