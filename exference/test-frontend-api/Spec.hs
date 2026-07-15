module Main (main) where

import Language.Haskell.Djex.Exference (defaultExferenceOptions)
import qualified Language.Haskell.Exference.CLI as CLI
import Language.Haskell.Exference.Core (findExpressions)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

main :: IO ()
main = defaultMain $ testGroup "Exference frontend import surface"
  [ testCase "owns the compatibility CLI entry point" $
      CLI.main `seq` pure ()
  , testCase "reexports the merged parser-free API" $
      defaultExferenceOptions `seq` findExpressions `seq` pure ()
  ]
