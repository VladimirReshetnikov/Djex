{-# LANGUAGE EmptyDataDecls #-}

module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.List (nub)
import Data.Word (Word8)
import Numeric.Natural (Natural)

import qualified Language.Haskell.Synthesis.Internal.Fingerprint as Fingerprint
import Language.Haskell.Synthesis.Name
  ( Boxity (..)
  , Name
  , parseName
  , tupleName
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
  ( assertBool
  , assertFailure
  , testCase
  , (@?=)
  )

data FixtureFingerprint

main :: IO ()
main = defaultMain fingerprintTests

fingerprintTests :: TestTree
fingerprintTests = testGroup "canonical structural fingerprints"
  [ testCase "retain exact bytes and render deterministic lowercase hex" $ do
      let fingerprint = fixture 1 [0x72]
            [ Fingerprint.FingerprintNatural 0
            , Fingerprint.FingerprintBytes []
            ]
          expectedBytes =
            [ 0x44, 0x4a, 0x45, 0x58, 0x46, 0x50, 0x01
            , 0x01, 0x01, 0x01
            , 0x02, 0x01, 0x72
            , 0x03, 0x05, 0x01, 0x01, 0x00, 0x02, 0x00
            ]
      Fingerprint.fingerprintCanonicalBytes fingerprint @?= expectedBytes
      Fingerprint.fingerprintCode fingerprint @?=
        "444a455846500101010102017203050101000200"
      show fingerprint @?=
        "Fingerprint \"444a455846500101010102017203050101000200\""
      _ <- evaluate $ force fingerprint
      pure ()
  , testCase "make adjacent byte and tag boundaries unambiguous" $ do
      let separatedBytes = fixture 1 [0x72]
            [ Fingerprint.FingerprintBytes [0x01]
            , Fingerprint.FingerprintBytes [0x02]
            ]
          joinedBytes = fixture 1 [0x72]
            [Fingerprint.FingerprintBytes [0x01, 0x02]]
          firstTag = fixture 1 [0x72]
            [ Fingerprint.FingerprintTag [0x01]
                [Fingerprint.FingerprintBytes [0x02, 0x03]]
            ]
          secondTag = fixture 1 [0x72]
            [ Fingerprint.FingerprintTag [0x01, 0x02]
                [Fingerprint.FingerprintBytes [0x03]]
            ]
      assertBool "field concatenation erased a sequence boundary" $
        separatedBytes /= joinedBytes
      assertBool "tag concatenation erased a tag/payload boundary" $
        firstTag /= secondTag
  , testCase "retain field order and order fingerprints by exact bytes" $ do
      let left = fixture 1 [0x72]
            [ Fingerprint.FingerprintNatural 1
            , Fingerprint.FingerprintNatural 2
            ]
          right = fixture 1 [0x72]
            [ Fingerprint.FingerprintNatural 2
            , Fingerprint.FingerprintNatural 1
            ]
      assertBool "ordered fields were canonicalized as an unordered bag" $
        left /= right
      compare left right @?= compare
        (Fingerprint.fingerprintCanonicalBytes left)
        (Fingerprint.fingerprintCanonicalBytes right)
  , testCase "encode arbitrary-precision Natural fields without Int wrapping" $ do
      let maximumInt = fromIntegral (maxBound :: Int) :: Natural
          beyondInt = maximumInt + 1
          maximumFingerprint = fixture 1 [0x72]
            [Fingerprint.FingerprintNatural maximumInt]
          beyondFingerprint = fixture 1 [0x72]
            [Fingerprint.FingerprintNatural beyondInt]
          twoHundredFiftySix = fixture 1 [0x72]
            [Fingerprint.FingerprintNatural 256]
      assertBool "Natural encoding wrapped at the Int boundary" $
        maximumFingerprint /= beyondFingerprint
      assertBool "multi-byte Natural magnitude was not retained" $
        [0x01, 0x02, 0x01, 0x00] `isSuffixOfBytes`
          Fingerprint.fingerprintCanonicalBytes twoHundredFiftySix
  , testCase "encode payload lengths canonically across the LEB128 boundary" $ do
      let length127 = Fingerprint.fingerprintCanonicalBytes $
            fixture 1 (replicate 127 0) []
          length128 = Fingerprint.fingerprintCanonicalBytes $
            fixture 1 (replicate 128 0) []
      take 2 (drop 10 length127) @?= [0x02, 0x7f]
      take 3 (drop 10 length128) @?= [0x02, 0x80, 0x01]
  , testCase "include both semantic version and role in identity" $ do
      let versionOne = fixture 1 [0x72] []
          versionTwo = fixture 2 [0x72] []
          anotherRole = fixture 1 [0x73] []
      assertBool "semantic schema version was omitted" $
        versionOne /= versionTwo
      assertBool "identity role was omitted" $
        versionOne /= anotherRole
  , testCase "encode validated names structurally rather than through Show" $ do
      qualified <- expectRight $ parseName "Fixture.value"
      unqualified <- expectRight $ parseName "value"
      operator <- expectRight $ parseName "(+)"
      boxedTuple <- expectRight $ tupleName Boxed 2
      unboxedTuple <- expectRight $ tupleName Unboxed 2
      let names =
            [qualified, unqualified, operator, boxedTuple, unboxedTuple]
          fingerprints = map nameFingerprint names
      length (nub fingerprints) @?= length names
      assertBool "qualified and unqualified names shared an identity" $
        nameFingerprint qualified /= nameFingerprint unqualified
      assertBool "tuple boxity was erased" $
        nameFingerprint boxedTuple /= nameFingerprint unboxedTuple
  ]

fixture
  :: Natural
  -> [Word8]
  -> [Fingerprint.FingerprintField]
  -> Fingerprint.Fingerprint FixtureFingerprint
fixture version role fields = Fingerprint.buildFingerprint $
  Fingerprint.FingerprintBuilder
    { Fingerprint.fingerprintBuilderVersion = version
    , Fingerprint.fingerprintBuilderRole = role
    , Fingerprint.fingerprintBuilderFields = fields
    }

nameFingerprint :: Name -> Fingerprint.Fingerprint FixtureFingerprint
nameFingerprint name = fixture 1 [0x6e, 0x61, 0x6d, 0x65]
  [Fingerprint.FingerprintName name]

isSuffixOfBytes :: Eq value => [value] -> [value] -> Bool
isSuffixOfBytes suffix source = reverse suffix `isPrefixOfBytes` reverse source

isPrefixOfBytes :: Eq value => [value] -> [value] -> Bool
isPrefixOfBytes [] _ = True
isPrefixOfBytes _ [] = False
isPrefixOfBytes (expected : remainingExpected) (actual : remainingActual) =
  expected == actual
  && isPrefixOfBytes remainingExpected remainingActual

expectRight :: Show error => Either error value -> IO value
expectRight result = case result of
  Left failure -> assertFailure $ "unexpected rejection: " ++ show failure
  Right value -> pure value
