-- | Length/Z3 binding for the domain-neutral causal SMT-LIB driver.
--
-- The private handle keeps one process, cancellation token, and absolute
-- deadline together. Generic driver operations can therefore never mix those
-- authorities across readiness checks, exact writes, or stdout reads.
module Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session.Transport
  ( LengthSMTLibCausalTransport
  , lengthSMTLibCausalTransport
  , lengthSMTLibCausalTransportOps
  ) where

import Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session.Process
  ( LengthSMTLibProcess
  , LengthSMTLibProcessCancellation
  , LengthSMTLibProcessDeadline
  , LengthSMTLibProcessError
  , LengthSMTLibProcessFailureClass (LengthSMTLibProcessStdoutEOF)
  , checkLengthSMTLibProcessReady
  , drainLengthSMTLibProcessBoundaryWhitespace
  , lengthSMTLibProcessErrorClass
  , nextLengthSMTLibProcessStdoutChunk
  , writeLengthSMTLibProcess
  )
import Language.Haskell.Synthesis.Internal.SMTLib.Causal.Driver
  ( SMTLibCausalTransportOps (..) )

-- | An opaque handle binding one raw Length Z3 process to the cancellation
-- token and absolute deadline under which the causal driver may operate on
-- it.  Every driver operation dispatched through
-- 'lengthSMTLibCausalTransportOps' uses exactly these three authorities.
data LengthSMTLibCausalTransport = LengthSMTLibCausalTransport
  LengthSMTLibProcess
  LengthSMTLibProcessCancellation
  LengthSMTLibProcessDeadline

-- | Bind a process to the cancellation token and deadline for one driven
-- causal transaction.  The handle borrows the process; it does not close it.
lengthSMTLibCausalTransport
  :: LengthSMTLibProcess
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> LengthSMTLibCausalTransport
lengthSMTLibCausalTransport = LengthSMTLibCausalTransport

-- | The causal-driver operations for a Length transport: readiness check,
-- boundary-whitespace drain, exact write, and next stdout chunk are the raw
-- process operations of the same name applied to the handle's process,
-- cancellation, and deadline, and the EOF predicate recognizes exactly the
-- 'LengthSMTLibProcessStdoutEOF' failure class.
lengthSMTLibCausalTransportOps
  :: SMTLibCausalTransportOps
      LengthSMTLibCausalTransport LengthSMTLibProcessError
lengthSMTLibCausalTransportOps = SMTLibCausalTransportOps
  { smtLibCausalTransportCheckReady =
      \(LengthSMTLibCausalTransport process cancellation deadline) ->
        checkLengthSMTLibProcessReady process cancellation deadline
  , smtLibCausalTransportDrainBoundaryWhitespace =
      \(LengthSMTLibCausalTransport process cancellation deadline) ->
        drainLengthSMTLibProcessBoundaryWhitespace
          process cancellation deadline
  , smtLibCausalTransportWrite =
      \(LengthSMTLibCausalTransport process cancellation deadline) ->
        writeLengthSMTLibProcess process cancellation deadline
  , smtLibCausalTransportNextStdoutChunk =
      \(LengthSMTLibCausalTransport process cancellation deadline) ->
        nextLengthSMTLibProcessStdoutChunk process cancellation deadline
  , smtLibCausalTransportFailureIsStdoutEOF =
      (== LengthSMTLibProcessStdoutEOF) . lengthSMTLibProcessErrorClass
  }
