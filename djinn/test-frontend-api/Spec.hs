module Main (main) where

import Data.Either (isRight)

import Djinn.Core (parseHType)
import Language.Haskell.Djex.Djinn (standardDjinnSession)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

main :: IO ()
main = defaultMain $ testGroup "Djinn frontend import surface"
  [ testCase "reexports the historical core facade" $
      assertBool "the reexported Djinn parser rejected a valid type"
        $ isRight $ parseHType "a -> a"
  , testCase "reexports the checked Djex adapter" $
      assertBool "the reexported standard Djinn session did not seal"
        $ isRight standardDjinnSession
  ]
