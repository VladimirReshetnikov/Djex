-- | Package-private proof that one strict stdout chunk is nonempty.
--
-- A causal transport cannot make progress from a successful empty read.  The
-- hidden constructor therefore lets concrete transports admit bytes at their
-- read/enqueue boundary and lets the generic driver consume only successful
-- chunks which contain at least one byte.  The receipt proves neither FIFO
-- origin, a byte bound, transport association, nor any schema identity.
module Language.Haskell.Synthesis.Internal.SMTLib.Causal.StdoutChunk
  ( SMTLibCausalStdoutChunk
  , admitSMTLibCausalStdoutChunk
  , smtLibCausalStdoutChunkBytes
  ) where

import Control.DeepSeq (NFData (rnf))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- | Opaque admission of one nonempty strict stdout chunk.  The strict field
-- preserves the existing Process queue demand: a queued chunk already forced
-- its strict 'ByteString' constructor before this receipt was introduced.
data SMTLibCausalStdoutChunk = SMTLibCausalStdoutChunk !ByteString

instance NFData SMTLibCausalStdoutChunk where
  rnf (SMTLibCausalStdoutChunk bytes) = rnf bytes

-- | Admit exactly nonempty strict bytes.
admitSMTLibCausalStdoutChunk
  :: ByteString
  -> Maybe SMTLibCausalStdoutChunk
admitSMTLibCausalStdoutChunk bytes
  | BS.null bytes = Nothing
  | otherwise = Just $ SMTLibCausalStdoutChunk bytes

-- | Project the admitted chunk; the result is never empty.
smtLibCausalStdoutChunkBytes
  :: SMTLibCausalStdoutChunk
  -> ByteString
smtLibCausalStdoutChunkBytes (SMTLibCausalStdoutChunk bytes) = bytes
