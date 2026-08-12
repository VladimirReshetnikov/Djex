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

data LengthSMTLibCausalTransport = LengthSMTLibCausalTransport
  LengthSMTLibProcess
  LengthSMTLibProcessCancellation
  LengthSMTLibProcessDeadline

lengthSMTLibCausalTransport
  :: LengthSMTLibProcess
  -> LengthSMTLibProcessCancellation
  -> LengthSMTLibProcessDeadline
  -> LengthSMTLibCausalTransport
lengthSMTLibCausalTransport = LengthSMTLibCausalTransport

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
