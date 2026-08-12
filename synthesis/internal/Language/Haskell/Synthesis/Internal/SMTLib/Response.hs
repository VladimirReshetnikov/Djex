{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Bounded parsing for one complete SMT-LIB 2.x response S-expression.
--
-- This module is deliberately transport-neutral.  It does not read a process,
-- split a stream into requests, or decide whether a response belongs to a
-- query.  A future executor must establish those protocol and execution
-- identities before using a parsed value.  Parsing also grants no semantic
-- authority: even a well-formed @unsat@ response is still a raw solver report.
--
-- Input is retained under the total byte bound before tokenization.  This
-- makes rejection productive for cyclic and infinite lazy byte lists.  The
-- parser then uses an explicit list-frame stack, so hostile nesting consumes
-- the configured depth budget rather than the Haskell call stack.
module Language.Haskell.Synthesis.Internal.SMTLib.Response
  ( SMTLibResponseLimitSource (..)
  , SMTLibResponseLimits
  , mkSMTLibResponseLimits
  , defaultSMTLibResponseLimitSource
  , defaultSMTLibResponseLimits
  , smtLibResponseByteLimit
  , smtLibResponseNestingDepthLimit
  , smtLibResponseNodeLimit
  , smtLibResponseTokenByteLimit
  , smtLibResponseNumeralBitLimit
  , SMTLibTokenPart (..)
  , SMTLibParseError (..)
  , SMTLibAtom (..)
  , smtLibSymbolBytes
  , SMTLibSExpression (..)
  , parseSMTLibSExpression
  ) where

import Control.DeepSeq (NFData (rnf))
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Internal.SMTLib.Lexical
  ( isSMTLibWhitespaceByte )

-- | Raw independent limits for one response slice.  All fields are natural;
-- zero is meaningful and rejects the first corresponding unit of work.
data SMTLibResponseLimitSource = SMTLibResponseLimitSource
  { smtLibResponseLimitSourceBytes :: Natural
  , smtLibResponseLimitSourceNestingDepth :: Natural
  , smtLibResponseLimitSourceNodes :: Natural
  , smtLibResponseLimitSourceTokenBytes :: Natural
  , smtLibResponseLimitSourceNumeralBits :: Natural
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData SMTLibResponseLimitSource

-- | Validated-by-construction response limits.
data SMTLibResponseLimits = SMTLibResponseLimits
  !Natural !Natural !Natural !Natural !Natural
  deriving (Eq, Ord, Show)

instance NFData SMTLibResponseLimits where
  rnf (SMTLibResponseLimits bytes depth nodes tokenBytes numeralBits) =
    rnf bytes `seq` rnf depth `seq` rnf nodes `seq`
    rnf tokenBytes `seq` rnf numeralBits

mkSMTLibResponseLimits :: SMTLibResponseLimitSource -> SMTLibResponseLimits
mkSMTLibResponseLimits source = SMTLibResponseLimits
  (smtLibResponseLimitSourceBytes source)
  (smtLibResponseLimitSourceNestingDepth source)
  (smtLibResponseLimitSourceNodes source)
  (smtLibResponseLimitSourceTokenBytes source)
  (smtLibResponseLimitSourceNumeralBits source)

-- | Conservative limits for one status, valuation, error, or diagnostic
-- response.  Process-level transcript limits remain a separate obligation.
defaultSMTLibResponseLimitSource :: SMTLibResponseLimitSource
defaultSMTLibResponseLimitSource = SMTLibResponseLimitSource
  { smtLibResponseLimitSourceBytes = 65536
  , smtLibResponseLimitSourceNestingDepth = 64
  , smtLibResponseLimitSourceNodes = 4096
  , smtLibResponseLimitSourceTokenBytes = 4096
  , smtLibResponseLimitSourceNumeralBits = 4096
  }

defaultSMTLibResponseLimits :: SMTLibResponseLimits
defaultSMTLibResponseLimits =
  mkSMTLibResponseLimits defaultSMTLibResponseLimitSource

smtLibResponseByteLimit :: SMTLibResponseLimits -> Natural
smtLibResponseByteLimit (SMTLibResponseLimits value _ _ _ _) = value

smtLibResponseNestingDepthLimit :: SMTLibResponseLimits -> Natural
smtLibResponseNestingDepthLimit
    (SMTLibResponseLimits _ value _ _ _) = value

smtLibResponseNodeLimit :: SMTLibResponseLimits -> Natural
smtLibResponseNodeLimit (SMTLibResponseLimits _ _ value _ _) = value

smtLibResponseTokenByteLimit :: SMTLibResponseLimits -> Natural
smtLibResponseTokenByteLimit (SMTLibResponseLimits _ _ _ value _) = value

smtLibResponseNumeralBitLimit :: SMTLibResponseLimits -> Natural
smtLibResponseNumeralBitLimit (SMTLibResponseLimits _ _ _ _ value) = value

-- | Source token whose content first exceeded the shared token byte limit.
-- String counts include both bytes of each doubled-quote escape; delimiters
-- themselves are excluded.  Bare tokens count their complete lexeme.
data SMTLibTokenPart
  = SMTLibBareToken
  | SMTLibStringToken
  | SMTLibQuotedSymbolToken
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData SMTLibTokenPart

-- | Fixed-precedence failure for one complete response slice.  Byte offsets
-- are zero-based.  Every observed limit value is capped at maximum + 1.
data SMTLibParseError
  = SMTLibResponseByteLimitExceeded !Natural !Natural
  | SMTLibNestingDepthLimitExceeded !Natural !Natural
  | SMTLibNodeLimitExceeded !Natural !Natural
  | SMTLibTokenByteLimitExceeded
      !SMTLibTokenPart !Natural !Natural
  | SMTLibNumeralBitLimitExceeded !Natural !Natural
  | SMTLibEmptyResponse
  | SMTLibUnexpectedClosingParenthesis !Natural
  | SMTLibUnterminatedList !Natural
  | SMTLibUnterminatedString !Natural
  | SMTLibUnterminatedQuotedSymbol !Natural
  | SMTLibInvalidStringByte !Natural !Word8
  | SMTLibInvalidQuotedSymbolByte !Natural !Word8
  | SMTLibInvalidBareToken !Natural [Word8]
  | SMTLibTrailingExpression !Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData SMTLibParseError

-- | One lexically checked SMT-LIB atom.
--
-- Decimal, hexadecimal, binary, reserved, and keyword constructors retain
-- their exact lexemes.  Strings contain decoded content (a doubled quote is
-- one byte).  Quoted symbols contain their delimiter-free content; SMT-LIB
-- defines that content as the same symbol as an equal simple spelling.
data SMTLibAtom
  = SMTLibNumeral !Natural
  | SMTLibDecimal [Word8]
  | SMTLibHexadecimal [Word8]
  | SMTLibBinary [Word8]
  | SMTLibString [Word8]
  | SMTLibSimpleSymbol [Word8]
  | SMTLibQuotedSymbol [Word8]
  | SMTLibReserved [Word8]
  | SMTLibKeyword [Word8]
  deriving (Eq, Ord, Show, Generic)

instance NFData SMTLibAtom

-- | Normalize either legal symbol spelling.  Reserved words, keywords, and
-- special constants are not symbols and return 'Nothing'.
smtLibSymbolBytes :: SMTLibAtom -> Maybe [Word8]
smtLibSymbolBytes atom = case atom of
  SMTLibSimpleSymbol bytes -> Just bytes
  SMTLibQuotedSymbol bytes -> Just bytes
  _ -> Nothing

-- | A bounded, lexically checked S-expression.  The representation stays
-- package-private so downstream callers commit only to command-specific
-- decoders; internal semantic consumers must still check exact response shape
-- and expected symbols/sorts.
data SMTLibSExpression
  = SMTLibAtomExpression !SMTLibAtom
  | SMTLibListExpression [SMTLibSExpression]
  deriving (Eq, Ord, Show, Generic)

instance NFData SMTLibSExpression

data ParseFrame = ParseFrame !Natural [SMTLibSExpression]

-- | Parse exactly one S-expression, allowing only SMT-LIB whitespace and
-- comments before and after it.  The total byte bound is checked first.
parseSMTLibSExpression
  :: SMTLibResponseLimits
  -> [Word8]
  -> Either SMTLibParseError SMTLibSExpression
parseSMTLibSExpression limits rawBytes = do
  bytes <- retainResponseBytes (smtLibResponseByteLimit limits) rawBytes
  parseRetained limits bytes

retainResponseBytes
  :: Natural
  -> [Word8]
  -> Either SMTLibParseError [Word8]
retainResponseBytes maximumBytes = go maximumBytes []
 where
  go !_ reversed [] = Right $ reverse reversed
  go 0 _ (_ : _) = Left $ SMTLibResponseByteLimitExceeded
    maximumBytes (maximumBytes + 1)
  go !remaining reversed (byte : bytes) =
    go (remaining - 1) (byte : reversed) bytes

parseRetained
  :: SMTLibResponseLimits
  -> [Word8]
  -> Either SMTLibParseError SMTLibSExpression
parseRetained limits = go 0 0 0 [] Nothing
 where
  go !offset !depth !nodes frames root input =
    let (nextOffset, remaining) = skipTrivia offset input
    in case remaining of
      [] -> case frames of
        ParseFrame opening _ : _ -> Left $ SMTLibUnterminatedList opening
        [] -> case root of
          Nothing -> Left SMTLibEmptyResponse
          Just expression -> Right expression
      byte : bytes
        | null frames, Just _ <- root ->
            if byte == closeParen
              then Left $ SMTLibUnexpectedClosingParenthesis nextOffset
              else Left $ SMTLibTrailingExpression nextOffset
        | byte == openParen -> do
            nextDepth <- consumeDepth limits depth
            nextNodes <- consumeNode limits nodes
            go (nextOffset + 1) nextDepth nextNodes
              (ParseFrame nextOffset [] : frames) root bytes
        | byte == closeParen -> case frames of
            [] -> Left $ SMTLibUnexpectedClosingParenthesis nextOffset
            ParseFrame _ reversed : outerFrames -> do
              let expression = SMTLibListExpression $ reverse reversed
              (updatedFrames, updatedRoot) <- addExpression
                expression outerFrames root nextOffset
              go (nextOffset + 1) (depth - 1) nodes
                updatedFrames updatedRoot bytes
        | otherwise -> do
            nextNodes <- consumeNode limits nodes
            (atom, afterOffset, afterBytes) <- parseAtom
              limits nextOffset remaining
            (updatedFrames, updatedRoot) <- addExpression
              (SMTLibAtomExpression atom) frames root nextOffset
            go afterOffset depth nextNodes
              updatedFrames updatedRoot afterBytes

addExpression
  :: SMTLibSExpression
  -> [ParseFrame]
  -> Maybe SMTLibSExpression
  -> Natural
  -> Either SMTLibParseError
      ([ParseFrame], Maybe SMTLibSExpression)
addExpression expression frames root offset = case frames of
  ParseFrame opening reversed : outer -> Right
    (ParseFrame opening (expression : reversed) : outer, root)
  [] -> case root of
    Nothing -> Right ([], Just expression)
    Just _ -> Left $ SMTLibTrailingExpression offset

consumeDepth
  :: SMTLibResponseLimits
  -> Natural
  -> Either SMTLibParseError Natural
consumeDepth limits observed
  | observed < maximumDepth = Right $ observed + 1
  | otherwise = Left $ SMTLibNestingDepthLimitExceeded
      maximumDepth (maximumDepth + 1)
 where
  maximumDepth = smtLibResponseNestingDepthLimit limits

consumeNode
  :: SMTLibResponseLimits
  -> Natural
  -> Either SMTLibParseError Natural
consumeNode limits observed
  | observed < maximumNodes = Right $ observed + 1
  | otherwise = Left $ SMTLibNodeLimitExceeded
      maximumNodes (maximumNodes + 1)
 where
  maximumNodes = smtLibResponseNodeLimit limits

skipTrivia :: Natural -> [Word8] -> (Natural, [Word8])
skipTrivia = go
 where
  go !offset [] = (offset, [])
  go !offset input@(byte : bytes)
    | isSMTLibWhitespaceByte byte = go (offset + 1) bytes
    | byte == semicolon = skipComment (offset + 1) bytes
    | otherwise = (offset, input)

  skipComment !offset [] = (offset, [])
  skipComment !offset (byte : bytes)
    | byte == lineFeed || byte == carriageReturn =
        go (offset + 1) bytes
    | otherwise = skipComment (offset + 1) bytes

parseAtom
  :: SMTLibResponseLimits
  -> Natural
  -> [Word8]
  -> Either SMTLibParseError (SMTLibAtom, Natural, [Word8])
parseAtom limits offset input = case input of
  byte : bytes
    | byte == doubleQuote -> scanString limits offset
        (offset + 1) 0 [] bytes
    | byte == verticalBar -> scanQuotedSymbol limits offset
        (offset + 1) 0 [] bytes
  _ -> do
    (token, afterOffset, afterBytes) <- scanBareToken limits offset input
    atom <- classifyBareToken limits offset token
    pure (atom, afterOffset, afterBytes)

scanBareToken
  :: SMTLibResponseLimits
  -> Natural
  -> [Word8]
  -> Either SMTLibParseError ([Word8], Natural, [Word8])
scanBareToken limits = go 0 []
 where
  maximumBytes = smtLibResponseTokenByteLimit limits

  go !_ reversed !offset [] = Right (reverse reversed, offset, [])
  go !observed reversed !offset input@(byte : bytes)
    | isBareDelimiter byte = Right (reverse reversed, offset, input)
    | observed >= maximumBytes = Left $ SMTLibTokenByteLimitExceeded
        SMTLibBareToken maximumBytes (maximumBytes + 1)
    | otherwise = go (observed + 1) (byte : reversed)
        (offset + 1) bytes

scanString
  :: SMTLibResponseLimits
  -> Natural
  -> Natural
  -> Natural
  -> [Word8]
  -> [Word8]
  -> Either SMTLibParseError (SMTLibAtom, Natural, [Word8])
scanString limits opening = go
 where
  maximumBytes = smtLibResponseTokenByteLimit limits

  go !_ !_ _ [] = Left $ SMTLibUnterminatedString opening
  go !offset !observed reversed (byte : bytes)
    | byte == doubleQuote = case bytes of
        next : remaining | next == doubleQuote -> do
          observedOnce <- consumeTokenByte
            SMTLibStringToken maximumBytes observed
          observedTwice <- consumeTokenByte
            SMTLibStringToken maximumBytes observedOnce
          go (offset + 2) observedTwice
            (doubleQuote : reversed) remaining
        _ -> Right
          (SMTLibString $ reverse reversed, offset + 1, bytes)
    | not $ isStringCharacter byte =
        Left $ SMTLibInvalidStringByte offset byte
    | otherwise = do
        nextObserved <- consumeTokenByte
          SMTLibStringToken maximumBytes observed
        go (offset + 1) nextObserved (byte : reversed) bytes

scanQuotedSymbol
  :: SMTLibResponseLimits
  -> Natural
  -> Natural
  -> Natural
  -> [Word8]
  -> [Word8]
  -> Either SMTLibParseError (SMTLibAtom, Natural, [Word8])
scanQuotedSymbol limits opening = go
 where
  maximumBytes = smtLibResponseTokenByteLimit limits

  go !_ !_ _ [] = Left $ SMTLibUnterminatedQuotedSymbol opening
  go !offset !observed reversed (byte : bytes)
    | byte == verticalBar = Right
        (SMTLibQuotedSymbol $ reverse reversed, offset + 1, bytes)
    | byte == backslash || not (isQuotedSymbolCharacter byte) =
        Left $ SMTLibInvalidQuotedSymbolByte offset byte
    | otherwise = do
        nextObserved <- consumeTokenByte
          SMTLibQuotedSymbolToken maximumBytes observed
        go (offset + 1) nextObserved (byte : reversed) bytes

consumeTokenByte
  :: SMTLibTokenPart
  -> Natural
  -> Natural
  -> Either SMTLibParseError Natural
consumeTokenByte part maximumBytes observed
  | observed < maximumBytes = Right $ observed + 1
  | otherwise = Left $ SMTLibTokenByteLimitExceeded
      part maximumBytes (maximumBytes + 1)

classifyBareToken
  :: SMTLibResponseLimits
  -> Natural
  -> [Word8]
  -> Either SMTLibParseError SMTLibAtom
classifyBareToken limits offset token
  | validNumeral token = SMTLibNumeral
      <$> parseNumeralWithin
          (smtLibResponseNumeralBitLimit limits) token
  | validDecimal token = Right $ SMTLibDecimal token
  | validHexadecimal token = Right $ SMTLibHexadecimal token
  | validBinary token = Right $ SMTLibBinary token
  | validKeyword token = Right $ SMTLibKeyword token
  | validSimpleSymbol token = Right $
      if token `elem` reservedWords
        then SMTLibReserved token
        else SMTLibSimpleSymbol token
  | otherwise = Left $ SMTLibInvalidBareToken offset token

validNumeral :: [Word8] -> Bool
validNumeral token = case token of
  [digit] -> isDigit digit
  first : _ -> first /= digitZero && all isDigit token
  [] -> False

validDecimal :: [Word8] -> Bool
validDecimal token = case break (== period) token of
  (whole, _ : fractional) ->
    validNumeral whole && not (null fractional) && all isDigit fractional
  _ -> False

validHexadecimal :: [Word8] -> Bool
validHexadecimal token = case token of
  hash : x : digits -> hash == numberSign && x == lowerX &&
    not (null digits) && all isHexDigit digits
  _ -> False

validBinary :: [Word8] -> Bool
validBinary token = case token of
  hash : b : digits -> hash == numberSign && b == lowerB &&
    not (null digits) && all isBinaryDigit digits
  _ -> False

validKeyword :: [Word8] -> Bool
validKeyword token = case token of
  -- The leading colon satisfies the token's non-digit start condition; the
  -- standard's own examples include :56.  Reserved-word comparison likewise
  -- applies to the complete colon-prefixed token, not the body alone.
  colon : body -> colon == keywordColon &&
    not (null body) && all isSimpleSymbolCharacter body
  _ -> False

validSimpleSymbol :: [Word8] -> Bool
validSimpleSymbol token = case token of
  first : _ -> not (isDigit first) && all isSimpleSymbolCharacter token
  [] -> False

parseNumeralWithin
  :: Natural
  -> [Word8]
  -> Either SMTLibParseError Natural
parseNumeralWithin maximumBits token = go 0 token
 where
  -- Every decimal digit carries fewer than four bits.  Avoid constructing an
  -- enormous 2^maximumBits threshold when the bounded token itself proves the
  -- numeral cannot reach that width.
  tokenBytes = foldl' (\count _ -> count + 1) 0 token
  needsThreshold = maximumBits < 4 * tokenBytes
  exclusiveMaximum
    | needsThreshold = 2 ^ maximumBits
    | otherwise = 0

  go !value [] = Right value
  go !value (byte : bytes) =
    let next = value * 10 + fromIntegral (byte - digitZero)
    in if needsThreshold && next >= exclusiveMaximum
        then Left $ SMTLibNumeralBitLimitExceeded
          maximumBits (maximumBits + 1)
        else go next bytes

isBareDelimiter :: Word8 -> Bool
isBareDelimiter byte =
  isSMTLibWhitespaceByte byte ||
  byte == openParen || byte == closeParen ||
  byte == semicolon || byte == doubleQuote || byte == verticalBar

isStringCharacter :: Word8 -> Bool
isStringCharacter byte = isSMTLibWhitespaceByte byte || isPrintable byte

isQuotedSymbolCharacter :: Word8 -> Bool
isQuotedSymbolCharacter byte =
  isSMTLibWhitespaceByte byte || isPrintable byte

isPrintable :: Word8 -> Bool
isPrintable byte = (byte >= 32 && byte <= 126) || byte >= 128

isDigit :: Word8 -> Bool
isDigit byte = byte >= digitZero && byte <= digitNine

isHexDigit :: Word8 -> Bool
isHexDigit byte = isDigit byte ||
  (byte >= upperA && byte <= upperF) ||
  (byte >= lowerA && byte <= lowerF)

isBinaryDigit :: Word8 -> Bool
isBinaryDigit byte = byte == digitZero || byte == digitOne

isSimpleSymbolCharacter :: Word8 -> Bool
isSimpleSymbolCharacter byte =
  (byte >= upperA && byte <= upperZ) ||
  (byte >= lowerA && byte <= lowerZ) ||
  isDigit byte || byte `elem` simpleSymbolPunctuation

reservedWords :: [[Word8]]
reservedWords = map ascii
  [ "BINARY", "DECIMAL", "HEXADECIMAL", "NUMERAL", "STRING"
  , "_", "!", "as", "lambda", "let", "exists", "forall", "match", "par"
  , "assert", "check-sat", "check-sat-assuming", "declare-const"
  , "declare-datatype", "declare-datatypes", "declare-fun", "declare-sort"
  , "declare-sort-parameter", "define-const", "define-fun"
  , "define-fun-rec", "define-funs-rec", "define-sort", "echo", "exit"
  , "get-assertions", "get-assignment", "get-info", "get-model"
  , "get-option", "get-proof", "get-unsat-assumptions", "get-unsat-core"
  , "get-value", "pop", "push", "reset", "reset-assertions"
  , "set-info", "set-logic", "set-option"
  ]

simpleSymbolPunctuation :: [Word8]
simpleSymbolPunctuation = ascii "~!@$%^&*_-+=<>.?/"

ascii :: String -> [Word8]
ascii = map $ fromIntegral . fromEnum

openParen, closeParen, semicolon, doubleQuote, verticalBar, backslash,
  lineFeed, carriageReturn, period, numberSign,
  keywordColon, digitZero, digitOne, digitNine, upperA, upperF, upperZ,
  lowerA, lowerB, lowerF, lowerX, lowerZ :: Word8
openParen = 40
closeParen = 41
semicolon = 59
doubleQuote = 34
verticalBar = 124
backslash = 92
lineFeed = 10
carriageReturn = 13
period = 46
numberSign = 35
keywordColon = 58
digitZero = 48
digitOne = 49
digitNine = 57
upperA = 65
upperF = 70
upperZ = 90
lowerA = 97
lowerB = 98
lowerF = 102
lowerX = 120
lowerZ = 122
