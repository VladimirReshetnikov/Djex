module Main (main) where

import qualified Language.Haskell.Exference.CLI as CLI
import Test.Tasty (defaultMain)
import Test.Tasty.HUnit (testCase)

main :: IO ()
main = defaultMain $ testCase
  "the frontend owns the compatibility CLI entry point" $
    CLI.main `seq` pure ()
