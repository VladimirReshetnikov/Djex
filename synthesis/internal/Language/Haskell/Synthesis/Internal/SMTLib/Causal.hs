{-# LANGUAGE RoleAnnotations #-}

-- | Package-private causal action algebra for pure SMT-LIB state machines.
--
-- A write action couples exact bytes with the receiver which may consume their
-- response; an owning driver must complete that write before feeding the
-- receiver.  An await action keeps receiving bytes for the preceding write,
-- while completion returns the machine's decoded outcome.  The algebra
-- performs no IO and makes no claim about solver execution.
module Language.Haskell.Synthesis.Internal.SMTLib.Causal
  ( SMTLibCausalAction (..)
  ) where

import Control.DeepSeq (NFData (rnf))
import Data.Word (Word8)

data SMTLibCausalAction kind receiver outcome
  = SMTLibCausalWrite !kind [Word8] !receiver
  | SMTLibCausalAwait !receiver
  | SMTLibCausalComplete !outcome

-- Write kinds, receivers, and outcomes may each carry private association
-- tokens.  Keep all three parameters nominal so the shared transport shape
-- cannot create a Coercible path between independently sealed machines.
type role SMTLibCausalAction nominal nominal nominal

instance
    (NFData kind, NFData receiver, NFData outcome) =>
    NFData (SMTLibCausalAction kind receiver outcome) where
  rnf action = case action of
    SMTLibCausalWrite kind bytes receiver ->
      rnf kind `seq` rnf bytes `seq` rnf receiver
    SMTLibCausalAwait receiver -> rnf receiver
    SMTLibCausalComplete outcome -> rnf outcome
