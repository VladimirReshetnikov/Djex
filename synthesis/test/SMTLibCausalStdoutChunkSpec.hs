module SMTLibCausalStdoutChunkSpec
  ( smtLibCausalStdoutChunkTests
  ) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertFailure
  , testCase
  , (@?=)
  )

import Language.Haskell.Synthesis.Internal.SMTLib.Causal.StdoutChunk
  ( SMTLibCausalStdoutChunk
  , admitSMTLibCausalStdoutChunk
  , smtLibCausalStdoutChunkBytes
  )

smtLibCausalStdoutChunkTests :: TestTree
smtLibCausalStdoutChunkTests = testGroup "causal SMT-LIB stdout chunks"
  [ testCase "reject the empty strict byte string" $
      case admitSMTLibCausalStdoutChunk BS.empty of
        Nothing -> pure ()
        Just _ -> assertFailure "empty stdout chunk was admitted"
  , testCase "admit every singleton byte without inspecting vocabulary" $ do
      admitted <- mapM admitSingleton everyByte
      map smtLibCausalStdoutChunkBytes admitted @?=
        map BS.singleton everyByte
  , testCase "retain and deeply force one multi-byte chunk" $ do
      let expected = BS.pack [0, 9, 10, 127, 128, 255]
      admitted <- admit expected
      smtLibCausalStdoutChunkBytes admitted @?= expected
      _ <- evaluate $ force admitted
      pure ()
  ]

admitSingleton :: Word8 -> IO SMTLibCausalStdoutChunk
admitSingleton = admit . BS.singleton

admit :: BS.ByteString -> IO SMTLibCausalStdoutChunk
admit bytes = case admitSMTLibCausalStdoutChunk bytes of
  Nothing -> assertFailure "nonempty stdout chunk was rejected"
  Just admitted -> pure admitted

everyByte :: [Word8]
everyByte = [minBound .. maxBound]
