-- | Package-private proof that a strict byte string contains only canonical
-- SMT-LIB whitespace.
--
-- The causal transport driver deliberately receives this opaque receipt
-- rather than trusting a raw successful drain.  Concrete transports must mint
-- every nonempty drained receipt while they can still restore a rejected FIFO
-- snapshot and apply their own failure policy; the driver may construct the
-- canonical empty receipt for an already-checked empty boundary.  This module
-- owns no transport operation, byte limit, transcript, or schema identity.
module Language.Haskell.Synthesis.Internal.SMTLib.Causal.BoundaryWhitespace
  ( SMTLibCausalBoundaryWhitespace
  , admitSMTLibCausalBoundaryWhitespace
  , concatSMTLibCausalBoundaryWhitespace
  , smtLibCausalBoundaryWhitespaceBytes
  ) where

import Control.DeepSeq (NFData (rnf))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Language.Haskell.Synthesis.Internal.SMTLib.Lexical
  ( isSMTLibWhitespaceByte )

-- | Opaque successful admission of finite strict boundary bytes. The field is
-- deliberately lazy: Process previously left successful FIFO concatenation
-- lazy, and Driver must not allocate or project adopted bytes before the next
-- exact write succeeds.
data SMTLibCausalBoundaryWhitespace = SMTLibCausalBoundaryWhitespace
  ByteString

instance NFData SMTLibCausalBoundaryWhitespace where
  rnf (SMTLibCausalBoundaryWhitespace bytes) = rnf bytes

-- | Admit exactly the strict bytes accepted by the shared lexical owner.
admitSMTLibCausalBoundaryWhitespace
  :: ByteString
  -> Maybe SMTLibCausalBoundaryWhitespace
admitSMTLibCausalBoundaryWhitespace bytes
  | BS.all isSMTLibWhitespaceByte bytes =
      Just $ SMTLibCausalBoundaryWhitespace bytes
  | otherwise = Nothing

-- | Concatenate already admitted chunks without reopening raw-byte
-- validation.  The empty list proves the empty boundary.
concatSMTLibCausalBoundaryWhitespace
  :: [SMTLibCausalBoundaryWhitespace]
  -> SMTLibCausalBoundaryWhitespace
concatSMTLibCausalBoundaryWhitespace =
  SMTLibCausalBoundaryWhitespace
  . BS.concat
  . map smtLibCausalBoundaryWhitespaceBytes

smtLibCausalBoundaryWhitespaceBytes
  :: SMTLibCausalBoundaryWhitespace
  -> ByteString
smtLibCausalBoundaryWhitespaceBytes
    (SMTLibCausalBoundaryWhitespace bytes) = bytes
