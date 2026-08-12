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
  ) where

import Data.Word (Word8)

-- | The four whitespace bytes admitted by SMT-LIB 2.7, in canonical
-- fingerprint order.
isSMTLibWhitespaceByte :: Word8 -> Bool
isSMTLibWhitespaceByte byte =
  byte == horizontalTab || byte == lineFeed ||
  byte == carriageReturn || byte == space

smtLibWhitespaceBytes :: [Word8]
smtLibWhitespaceBytes = [horizontalTab, lineFeed, carriageReturn, space]

horizontalTab, lineFeed, carriageReturn, space :: Word8
horizontalTab = 9
lineFeed = 10
carriageReturn = 13
space = 32
