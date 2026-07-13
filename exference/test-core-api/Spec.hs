module Main (main) where

import Language.Haskell.Djex.Exference
import Language.Haskell.Synthesis.Environment
  ( EnvironmentError
  , mkEnvironment
  )
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
import Test.Tasty.HUnit (assertBool, testCase)

main :: IO ()
main = defaultMain $ testCase
    "seal and query Exference without a source frontend" $ do
  environment <- expectRight
    (mkEnvironment []
      :: Either
          (EnvironmentError ExferenceTypeVariable)
          ExferenceEnvironment)
  session <- expectRight $ mkExferenceSession environment
  target <- expectRight $ mkIdentifier "identity"
  let variable = FlexibleVariable 0
      goal = FunctionType (TypeVariable variable) (TypeVariable variable)
  request <- expectRight $ mkExferenceRequest QueryRequest
    { requestTarget = target
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
