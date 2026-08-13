module SMTLibQFLIASpec (smtLibQFLIATests) where

import Data.Word (Word8)

import Language.Haskell.Synthesis.Internal.Fingerprint
  ( FingerprintField (..)
  )
import Language.Haskell.Synthesis.Internal.SMTLib.QFLIA
  ( QFLIABooleanExpression (..)
  , QFLIACommand (..)
  , QFLIAIntegerExpression (..)
  , qfliaBooleanExpressionFingerprintField
  , qfliaCommandFingerprintField
  , qfliaIntegerExpressionFingerprintField
  , qfliaLogicBytes
  , renderQFLIABooleanExpression
  , renderQFLIACommands
  , renderQFLIAIntegerExpression
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool
  , testCase
  , (@?=)
  )

smtLibQFLIATests :: TestTree
smtLibQFLIATests = testGroup "package-private typed QF_LIA foundation"
  [ testCase "render every command and expression constructor canonically"
      assertCanonicalRendering
  , testCase "encode every command and expression constructor structurally"
      assertStructuralFingerprintFields
  , testCase "keep render simplification distinct from structural identity"
      assertSimplifiedRenderingRetainsStructure
  ]

assertCanonicalRendering :: IO ()
assertCanonicalRendering = do
  qfliaLogicBytes @?= asciiBytes "QF_LIA"
  renderQFLIACommands completeCommands @?= asciiBytes completeScript
  renderQFLIAIntegerExpression completeInteger @?= asciiBytes
    "(ite (not (<= x 9)) (djex_nat_min (* 2 x) (- 9 x)) (+ x 9))"
  renderQFLIABooleanExpression completeBoolean @?= asciiBytes
    "(and (= x 9) (not (<= x 9)) true false)"

assertStructuralFingerprintFields :: IO ()
assertStructuralFingerprintFields = do
  map qfliaCommandFingerprintField completeCommands @?=
    [ tagged "set-logic" [bytes "QF_LIA"]
    , tagged "set-option" [bytes ":random-seed", bytes "1"]
    , tagged "define-int2"
        [ bytes "djex_nat_min"
        , bytes "x"
        , bytes "y"
        , tagged "ite"
            [ tagged "at-most"
                [tagged "symbol" [bytes "x"], tagged "symbol" [bytes "y"]]
            , tagged "symbol" [bytes "x"]
            , tagged "symbol" [bytes "y"]
            ]
        ]
    , tagged "declare-int" [bytes "x"]
    , tagged "assert" [completeBooleanField]
    , tagged "check-sat" []
    , tagged "get-value"
        [ tagged "symbol" [bytes "x"]
        , tagged "natural" [FingerprintNatural 9]
        ]
    ]
  qfliaIntegerExpressionFingerprintField completeInteger @?=
    tagged "ite"
      [ tagged "not"
          [ tagged "at-most"
              [tagged "symbol" [bytes "x"], tagged "natural" [FingerprintNatural 9]]
          ]
      , tagged "helper"
          [ bytes "djex_nat_min"
          , tagged "scale"
              [FingerprintNatural 2, tagged "symbol" [bytes "x"]]
          , tagged "difference"
              [tagged "natural" [FingerprintNatural 9], tagged "symbol" [bytes "x"]]
          ]
      , tagged "sum"
          [tagged "symbol" [bytes "x"], tagged "natural" [FingerprintNatural 9]]
      ]
  qfliaBooleanExpressionFingerprintField completeBoolean @?=
    completeBooleanField

assertSimplifiedRenderingRetainsStructure :: IO ()
assertSimplifiedRenderingRetainsStructure = do
  let symbol = QFLIAIntegerSymbol $ asciiBytes "x"
      singletonSum = QFLIAIntegerSum [symbol]
      true = QFLIABooleanTruth True
      singletonAll = QFLIABooleanAll [true]
  renderQFLIAIntegerExpression (QFLIAIntegerSum []) @?= asciiBytes "0"
  renderQFLIAIntegerExpression singletonSum @?=
    renderQFLIAIntegerExpression symbol
  assertBool "singleton sum lost its structural node"
    $ qfliaIntegerExpressionFingerprintField singletonSum /=
        qfliaIntegerExpressionFingerprintField symbol
  renderQFLIABooleanExpression (QFLIABooleanAll []) @?= asciiBytes "true"
  renderQFLIABooleanExpression singletonAll @?=
    renderQFLIABooleanExpression true
  assertBool "singleton conjunction lost its structural node"
    $ qfliaBooleanExpressionFingerprintField singletonAll /=
        qfliaBooleanExpressionFingerprintField true

completeCommands :: [QFLIACommand]
completeCommands =
  [ QFLIASetLogic $ asciiBytes "QF_LIA"
  , QFLIASetOption (asciiBytes ":random-seed") $ asciiBytes "1"
  , QFLIADefineBinaryInteger
      (asciiBytes "djex_nat_min")
      (asciiBytes "x")
      (asciiBytes "y")
      (QFLIAIntegerIf
        (QFLIAIntegerAtMost symbolX symbolY)
        symbolX
        symbolY)
  , QFLIADeclareInteger $ asciiBytes "x"
  , QFLIAAssert completeBoolean
  , QFLIACheckSatisfiable
  , QFLIAGetValues [symbolX, numeralNine]
  ]

completeScript :: String
completeScript = concat
  [ "(set-logic QF_LIA)\n"
  , "(set-option :random-seed 1)\n"
  , "(define-fun djex_nat_min ((x Int) (y Int)) Int (ite (<= x y) x y))\n"
  , "(declare-const x Int)\n"
  , "(assert (and (= x 9) (not (<= x 9)) true false))\n"
  , "(check-sat)\n"
  , "(get-value (x 9))\n"
  ]

completeInteger :: QFLIAIntegerExpression
completeInteger = QFLIAIntegerIf
  (QFLIABooleanNot $ QFLIAIntegerAtMost symbolX numeralNine)
  (QFLIAIntegerBinaryApplication
    (asciiBytes "djex_nat_min")
    (QFLIAIntegerScale 2 symbolX)
    (QFLIAIntegerDifference numeralNine symbolX))
  (QFLIAIntegerSum [symbolX, numeralNine])

completeBoolean :: QFLIABooleanExpression
completeBoolean = QFLIABooleanAll
  [ QFLIAIntegerEqual symbolX numeralNine
  , QFLIABooleanNot $ QFLIAIntegerAtMost symbolX numeralNine
  , QFLIABooleanTruth True
  , QFLIABooleanTruth False
  ]

completeBooleanField :: FingerprintField
completeBooleanField = tagged "all"
  [ tagged "equal"
      [tagged "symbol" [bytes "x"], tagged "natural" [FingerprintNatural 9]]
  , tagged "not"
      [ tagged "at-most"
          [tagged "symbol" [bytes "x"], tagged "natural" [FingerprintNatural 9]]
      ]
  , tagged "true" []
  , tagged "false" []
  ]

symbolX, symbolY, numeralNine :: QFLIAIntegerExpression
symbolX = QFLIAIntegerSymbol $ asciiBytes "x"
symbolY = QFLIAIntegerSymbol $ asciiBytes "y"
numeralNine = QFLIANaturalNumeral 9

tagged :: String -> [FingerprintField] -> FingerprintField
tagged name = FingerprintTag $ asciiBytes name

bytes :: String -> FingerprintField
bytes = FingerprintBytes . asciiBytes

asciiBytes :: String -> [Word8]
asciiBytes = map $ fromIntegral . fromEnum
