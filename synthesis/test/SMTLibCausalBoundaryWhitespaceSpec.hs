module SMTLibCausalBoundaryWhitespaceSpec
  ( smtLibCausalBoundaryWhitespaceTests
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

import Language.Haskell.Synthesis.Internal.SMTLib.Causal.BoundaryWhitespace
  ( SMTLibCausalBoundaryWhitespace
  , admitSMTLibCausalBoundaryWhitespace
  , concatSMTLibCausalBoundaryWhitespace
  , smtLibCausalBoundaryWhitespaceBytes
  )

smtLibCausalBoundaryWhitespaceTests :: TestTree
smtLibCausalBoundaryWhitespaceTests = testGroup
  "causal SMT-LIB boundary whitespace"
  [ testCase "admit exactly the four lexical whitespace bytes" $ do
      admittedBytes <- mapM admitSingleton everyWhitespaceByte
      map smtLibCausalBoundaryWhitespaceBytes admittedBytes @?=
        map BS.singleton everyWhitespaceByte
      mapM_ rejectSingleton everyNonWhitespaceByte
  , testCase "reject a mixed boundary snapshot" $
      case admitSMTLibCausalBoundaryWhitespace $ BS.pack [32, 10, 40, 9] of
        Nothing -> pure ()
        Just _ -> assertFailure "non-whitespace boundary byte was admitted"
  , testCase "compose admitted FIFO chunks without revalidation authority" $ do
      chunks <- mapM admit
        [BS.pack [9, 10], BS.empty, BS.pack [13], BS.singleton 32]
      let combined = concatSMTLibCausalBoundaryWhitespace chunks
      smtLibCausalBoundaryWhitespaceBytes combined @?=
        BS.pack everyWhitespaceByte
      smtLibCausalBoundaryWhitespaceBytes
        (concatSMTLibCausalBoundaryWhitespace []) @?= BS.empty
      _ <- evaluate $ force combined
      pure ()
  ]

admitSingleton :: Word8 -> IO SMTLibCausalBoundaryWhitespace
admitSingleton = admit . BS.singleton

rejectSingleton :: Word8 -> IO ()
rejectSingleton byte = case
    admitSMTLibCausalBoundaryWhitespace $ BS.singleton byte of
  Nothing -> pure ()
  Just _ -> assertFailure $ "unexpected boundary byte admitted: " ++ show byte

admit :: BS.ByteString -> IO SMTLibCausalBoundaryWhitespace
admit bytes = case admitSMTLibCausalBoundaryWhitespace bytes of
  Nothing -> assertFailure "valid boundary whitespace was rejected"
  Just admitted -> pure admitted

everyWhitespaceByte :: [Word8]
everyWhitespaceByte = [9, 10, 13, 32]

everyNonWhitespaceByte :: [Word8]
everyNonWhitespaceByte = filter (`notElem` everyWhitespaceByte)
  [minBound .. maxBound]
