module SMTLibLexicalSpec (smtLibLexicalTests) where

import Data.Word (Word8)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Language.Haskell.Synthesis.Internal.SMTLib.Lexical
  ( isSMTLibWhitespaceByte
  , smtLibWhitespaceBytes
  )

smtLibLexicalTests :: TestTree
smtLibLexicalTests = testGroup "SMT-LIB lexical vocabulary"
  [ testCase "own the exact ordered whitespace vocabulary" $ do
      smtLibWhitespaceBytes @?= [9, 10, 13, 32]
      filter isSMTLibWhitespaceByte everyByte @?= smtLibWhitespaceBytes
  ]

everyByte :: [Word8]
everyByte = [minBound .. maxBound]
