{-# LANGUAGE BangPatterns #-}
-- | Package-private SMT-LIB lexical facts shared by framing, parsing, causal
-- accounting, transport-boundary validation, and domain plan identities.
--
-- This module owns no parser, framing algorithm, or schema identity.  The
-- explicit byte order is nevertheless canonical.  A vocabulary change must
-- revisit shared framing, bounded response parsing, transport-boundary
-- draining, and every protocol or capability schema which fingerprints the
-- ordered bytes.
module Language.Haskell.Synthesis.Internal.SMTLib.Lexical
  ( isSMTLibWhitespaceByte
  , smtLibWhitespaceBytes
  , isSMTLibBareDelimiterByte
  , isSMTLibPrintableByte
  , isSMTLibStringCharacterByte
  , isSMTLibQuotedSymbolCharacterByte
  , retainExactByteCount
  ) where

import Data.Word (Word8)
import Numeric.Natural (Natural)

-- | The four whitespace bytes admitted by SMT-LIB 2.7, in canonical
-- fingerprint order.
isSMTLibWhitespaceByte :: Word8 -> Bool
isSMTLibWhitespaceByte byte =
  byte == horizontalTab || byte == lineFeed ||
  byte == carriageReturn || byte == space

-- | Exactly the bytes accepted by 'isSMTLibWhitespaceByte' (tab, line feed,
-- carriage return, space) in the canonical order that protocol and
-- capability schema fingerprints embed; the order is part of those
-- identities.
smtLibWhitespaceBytes :: [Word8]
smtLibWhitespaceBytes = [horizontalTab, lineFeed, carriageReturn, space]

horizontalTab, lineFeed, carriageReturn, space :: Word8
horizontalTab = 9
lineFeed = 10
carriageReturn = 13
space = 32

-- | Whether a byte ends a bare (unquoted) token: whitespace or one of the
-- five structural delimiters.
isSMTLibBareDelimiterByte :: Word8 -> Bool
isSMTLibBareDelimiterByte byte =
  isSMTLibWhitespaceByte byte ||
  byte == openParen || byte == closeParen ||
  byte == semicolon || byte == doubleQuote || byte == verticalBar

-- | The printable byte class shared by string and quoted-symbol contents:
-- printable ASCII plus every byte above 127.
isSMTLibPrintableByte :: Word8 -> Bool
isSMTLibPrintableByte byte = (byte >= 32 && byte <= 126) || byte >= 128

-- | Bytes admitted inside a string literal.
isSMTLibStringCharacterByte :: Word8 -> Bool
isSMTLibStringCharacterByte byte =
  isSMTLibWhitespaceByte byte || isSMTLibPrintableByte byte

-- | Bytes admitted inside a quoted @|...|@ symbol.
isSMTLibQuotedSymbolCharacterByte :: Word8 -> Bool
isSMTLibQuotedSymbolCharacterByte byte =
  isSMTLibWhitespaceByte byte || isSMTLibPrintableByte byte

openParen, closeParen, semicolon, doubleQuote, verticalBar :: Word8
openParen = 40
closeParen = 41
semicolon = 59
doubleQuote = 34
verticalBar = 124

-- | Retain a byte string only if it has exactly the expected length,
-- reporting the first surplus byte as one more than expected rather than
-- traversing the remainder.  The caller supplies its own mismatch error,
-- taking the expected and observed counts.
retainExactByteCount
  :: Natural
  -> (Natural -> Natural -> failure)
  -> [Word8]
  -> Either failure [Word8]
retainExactByteCount expected mismatch = go 0 []
 where
  go !observed reversed []
    | observed == expected = Right $ reverse reversed
    | otherwise = Left $ mismatch expected observed
  go !observed _ (_ : _)
    | observed == expected = Left $ mismatch expected (expected + 1)
  go !observed reversed (byte : bytes) =
    go (observed + 1) (byte : reversed) bytes
