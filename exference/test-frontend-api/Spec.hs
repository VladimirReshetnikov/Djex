module Main (main) where

import Language.Haskell.Djex.Exference (defaultExferenceOptions)
import Language.Haskell.Djex.Exference.HaskellSrc
  ( loadExferenceSession
  , loadExferenceSessionWithPolicy
  )
import qualified Language.Haskell.Exference.CLI as CLI
import Language.Haskell.Exference.Core (findExpressions)
import qualified Language.Haskell.Exference.Session as Session
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

main :: IO ()
main = defaultMain $ testGroup "Exference frontend import surface"
  [ testCase "owns the compatibility CLI entry point" $
      CLI.main `seq` pure ()
  , testCase "exposes the checked Haskell-source loader" $
      loadExferenceSession `seq`
      loadExferenceSessionWithPolicy `seq`
      pure ()
  , testCase "retains the checked-source compatibility session bridge" $
      Session.mkExferenceSession `seq`
      Session.mkExferenceSessionWithPolicy `seq`
      pure ()
  , testCase "exposes the merged core API" $
      defaultExferenceOptions `seq` findExpressions `seq` pure ()
  ]
