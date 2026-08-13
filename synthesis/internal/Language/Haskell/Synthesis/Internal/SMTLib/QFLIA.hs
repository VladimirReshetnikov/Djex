{-# LANGUAGE DeriveGeneric #-}

-- | Package-private typed syntax, canonical rendering, and structural
-- fingerprint fields for the quantifier-free linear integer arithmetic
-- fragment used by SMT-LIB backends.
--
-- This module deliberately has no knowledge of a semantic domain, solver,
-- query limits, generated-name policy, or model replay.  A domain translator
-- chooses exact symbol bytes and builds this typed syntax; the two projections
-- below then render and fingerprint the same structure independently.
module Language.Haskell.Synthesis.Internal.SMTLib.QFLIA
  ( QFLIAIntegerExpression (..)
  , QFLIABooleanExpression (..)
  , QFLIACommand (..)
  , qfliaLogicBytes
  , renderQFLIACommands
  , renderQFLIACommand
  , renderQFLIAIntegerExpression
  , renderQFLIABooleanExpression
  , qfliaCommandFingerprintField
  , qfliaIntegerExpressionFingerprintField
  , qfliaBooleanExpressionFingerprintField
  ) where

import Control.DeepSeq (NFData)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Internal.Fingerprint
  ( FingerprintField (..)
  )

-- | Typed integer expressions admitted by the shared QF_LIA renderer.
-- Function application retains an exact package-chosen symbol.  Callers are
-- responsible for providing a matching definition and for ensuring that the
-- chosen application remains in QF_LIA.
data QFLIAIntegerExpression
  = QFLIAIntegerSymbol [Word8]
  | QFLIANaturalNumeral !Natural
  | QFLIAIntegerSum [QFLIAIntegerExpression]
  | QFLIAIntegerScale !Natural QFLIAIntegerExpression
  | QFLIAIntegerDifference
      QFLIAIntegerExpression QFLIAIntegerExpression
  | QFLIAIntegerBinaryApplication
      ![Word8] QFLIAIntegerExpression QFLIAIntegerExpression
  | QFLIAIntegerIf
      QFLIABooleanExpression
      QFLIAIntegerExpression
      QFLIAIntegerExpression
  deriving (Eq, Ord, Show, Generic)

instance NFData QFLIAIntegerExpression

-- | Typed Boolean expressions admitted by the shared QF_LIA renderer.
data QFLIABooleanExpression
  = QFLIABooleanTruth !Bool
  | QFLIAIntegerEqual QFLIAIntegerExpression QFLIAIntegerExpression
  | QFLIAIntegerAtMost QFLIAIntegerExpression QFLIAIntegerExpression
  | QFLIABooleanNot QFLIABooleanExpression
  | QFLIABooleanAll [QFLIABooleanExpression]
  deriving (Eq, Ord, Show, Generic)

instance NFData QFLIABooleanExpression

-- | Complete commands used by the shared QF_LIA fragment.  Commands render
-- with one trailing newline; expressions never do.
data QFLIACommand
  = QFLIASetLogic [Word8]
  | QFLIASetOption [Word8] [Word8]
  | QFLIADefineBinaryInteger
      [Word8] [Word8] [Word8] QFLIAIntegerExpression
  | QFLIADeclareInteger [Word8]
  | QFLIAAssert QFLIABooleanExpression
  | QFLIACheckSatisfiable
  | QFLIAGetValues [QFLIAIntegerExpression]
  deriving (Eq, Ord, Show, Generic)

instance NFData QFLIACommand

-- | Exact SMT-LIB logic spelling shared by QF_LIA translators.
qfliaLogicBytes :: [Word8]
qfliaLogicBytes = ascii "QF_LIA"

-- | Canonically render an ordered command sequence.
renderQFLIACommands :: [QFLIACommand] -> [Word8]
renderQFLIACommands = concatMap renderQFLIACommand

-- | Canonically render one command with its trailing newline.
renderQFLIACommand :: QFLIACommand -> [Word8]
renderQFLIACommand command = case command of
  QFLIASetLogic logic -> line $ parenthesized
    [ascii "set-logic", logic]
  QFLIASetOption name value -> line $ parenthesized
    [ascii "set-option", name, value]
  QFLIADefineBinaryInteger name left right body -> line $ parenthesized
    [ ascii "define-fun"
    , name
    , parenthesized
        [ parenthesized [left, ascii "Int"]
        , parenthesized [right, ascii "Int"]
        ]
    , ascii "Int"
    , renderQFLIAIntegerExpression body
    ]
  QFLIADeclareInteger symbol -> line $ parenthesized
    [ascii "declare-const", symbol, ascii "Int"]
  QFLIAAssert formula -> line $ parenthesized
    [ascii "assert", renderQFLIABooleanExpression formula]
  QFLIACheckSatisfiable -> line $ parenthesized [ascii "check-sat"]
  QFLIAGetValues terms -> line $ parenthesized
    [ ascii "get-value"
    , parenthesized $ map renderQFLIAIntegerExpression terms
    ]

-- | Canonically render one integer expression without a trailing newline.
renderQFLIAIntegerExpression :: QFLIAIntegerExpression -> [Word8]
renderQFLIAIntegerExpression expression = case expression of
  QFLIAIntegerSymbol symbol -> symbol
  QFLIANaturalNumeral value -> ascii $ show value
  QFLIAIntegerSum [] -> ascii "0"
  QFLIAIntegerSum [term] -> renderQFLIAIntegerExpression term
  QFLIAIntegerSum terms -> parenthesized
    $ ascii "+" : map renderQFLIAIntegerExpression terms
  QFLIAIntegerScale factor term -> parenthesized
    [ascii "*", ascii $ show factor, renderQFLIAIntegerExpression term]
  QFLIAIntegerDifference left right -> parenthesized
    [ ascii "-"
    , renderQFLIAIntegerExpression left
    , renderQFLIAIntegerExpression right
    ]
  QFLIAIntegerBinaryApplication symbol left right -> parenthesized
    [ symbol
    , renderQFLIAIntegerExpression left
    , renderQFLIAIntegerExpression right
    ]
  QFLIAIntegerIf condition whenTrue whenFalse -> parenthesized
    [ ascii "ite"
    , renderQFLIABooleanExpression condition
    , renderQFLIAIntegerExpression whenTrue
    , renderQFLIAIntegerExpression whenFalse
    ]

-- | Canonically render one Boolean expression without a trailing newline.
renderQFLIABooleanExpression :: QFLIABooleanExpression -> [Word8]
renderQFLIABooleanExpression formula = case formula of
  QFLIABooleanTruth True -> ascii "true"
  QFLIABooleanTruth False -> ascii "false"
  QFLIAIntegerEqual left right -> parenthesized
    [ ascii "="
    , renderQFLIAIntegerExpression left
    , renderQFLIAIntegerExpression right
    ]
  QFLIAIntegerAtMost left right -> parenthesized
    [ ascii "<="
    , renderQFLIAIntegerExpression left
    , renderQFLIAIntegerExpression right
    ]
  QFLIABooleanNot nested -> parenthesized
    [ascii "not", renderQFLIABooleanExpression nested]
  QFLIABooleanAll [] -> ascii "true"
  QFLIABooleanAll [nested] -> renderQFLIABooleanExpression nested
  QFLIABooleanAll nested -> parenthesized
    $ ascii "and" : map renderQFLIABooleanExpression nested

-- | Project one command into the stable structural field vocabulary used by
-- package-owned query fingerprints.
qfliaCommandFingerprintField :: QFLIACommand -> FingerprintField
qfliaCommandFingerprintField command = case command of
  QFLIASetLogic logic -> tagged "set-logic" [FingerprintBytes logic]
  QFLIASetOption name value -> tagged "set-option"
    [FingerprintBytes name, FingerprintBytes value]
  QFLIADefineBinaryInteger name left right body -> tagged "define-int2"
    [ FingerprintBytes name
    , FingerprintBytes left
    , FingerprintBytes right
    , qfliaIntegerExpressionFingerprintField body
    ]
  QFLIADeclareInteger symbol -> tagged "declare-int"
    [FingerprintBytes symbol]
  QFLIAAssert formula -> tagged "assert"
    [qfliaBooleanExpressionFingerprintField formula]
  QFLIACheckSatisfiable -> tagged "check-sat" []
  QFLIAGetValues terms -> tagged "get-value"
    $ map qfliaIntegerExpressionFingerprintField terms

-- | Project one integer expression into stable structural fingerprint fields.
qfliaIntegerExpressionFingerprintField
  :: QFLIAIntegerExpression
  -> FingerprintField
qfliaIntegerExpressionFingerprintField expression = case expression of
  QFLIAIntegerSymbol symbol -> tagged "symbol" [FingerprintBytes symbol]
  QFLIANaturalNumeral value -> tagged "natural" [FingerprintNatural value]
  QFLIAIntegerSum terms -> tagged "sum"
    $ map qfliaIntegerExpressionFingerprintField terms
  QFLIAIntegerScale factor term -> tagged "scale"
    [ FingerprintNatural factor
    , qfliaIntegerExpressionFingerprintField term
    ]
  QFLIAIntegerDifference left right -> tagged "difference"
    [ qfliaIntegerExpressionFingerprintField left
    , qfliaIntegerExpressionFingerprintField right
    ]
  QFLIAIntegerBinaryApplication symbol left right -> tagged "helper"
    [ FingerprintBytes symbol
    , qfliaIntegerExpressionFingerprintField left
    , qfliaIntegerExpressionFingerprintField right
    ]
  QFLIAIntegerIf condition whenTrue whenFalse -> tagged "ite"
    [ qfliaBooleanExpressionFingerprintField condition
    , qfliaIntegerExpressionFingerprintField whenTrue
    , qfliaIntegerExpressionFingerprintField whenFalse
    ]

-- | Project one Boolean expression into stable structural fingerprint fields.
qfliaBooleanExpressionFingerprintField
  :: QFLIABooleanExpression
  -> FingerprintField
qfliaBooleanExpressionFingerprintField formula = case formula of
  QFLIABooleanTruth value -> tagged (if value then "true" else "false") []
  QFLIAIntegerEqual left right -> tagged "equal"
    [ qfliaIntegerExpressionFingerprintField left
    , qfliaIntegerExpressionFingerprintField right
    ]
  QFLIAIntegerAtMost left right -> tagged "at-most"
    [ qfliaIntegerExpressionFingerprintField left
    , qfliaIntegerExpressionFingerprintField right
    ]
  QFLIABooleanNot nested -> tagged "not"
    [qfliaBooleanExpressionFingerprintField nested]
  QFLIABooleanAll nested -> tagged "all"
    $ map qfliaBooleanExpressionFingerprintField nested

tagged :: String -> [FingerprintField] -> FingerprintField
tagged name = FingerprintTag (ascii name)

parenthesized :: [[Word8]] -> [Word8]
parenthesized fields = [openParen] ++ separated fields ++ [closeParen]
 where
  separated [] = []
  separated [field] = field
  separated (field : remaining) = field ++ [space] ++ separated remaining

line :: [Word8] -> [Word8]
line bytes = bytes ++ [newline]

ascii :: String -> [Word8]
ascii = map $ fromIntegral . fromEnum

openParen, closeParen, space, newline :: Word8
openParen = 40
closeParen = 41
space = 32
newline = 10
