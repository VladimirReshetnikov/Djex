{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Package-private cumulative ownership around the single-frame SMT-LIB
-- framer.
--
-- A policy keeps one configured per-frame policy and one transaction-wide
-- byte maximum.  Its opaque cursor owns the absolute frame start together
-- with the effective framer whose total bound is the smaller of the
-- configured frame total and the remaining transaction budget.  Completed
-- frames retain their untouched tail and absolute end behind one opaque
-- continuation, so a caller can neither restart that tail at a detached
-- offset nor charge it under a different policy.
module Language.Haskell.Synthesis.Internal.SMTLib.Causal.Stream
  ( SMTLibCausalStreamPolicy
  , mkSMTLibCausalStreamPolicy
  , smtLibCausalStreamPolicyStreamLimits
  , smtLibCausalStreamPolicyCumulativeByteLimit
  , SMTLibCausalStreamFailure (..)
  , SMTLibCausalStreamCursor
  , startSMTLibCausalStreamCursor
  , SMTLibCausalStreamStep (..)
  , SMTLibCausalStreamCompletedFrame
  , smtLibCausalStreamCompletedFrameBytes
  , feedSMTLibCausalStreamCursor
  , finishSMTLibCausalStreamCursor
  , continueSMTLibCausalStreamCompletedFrame
  , SMTLibCausalStreamBoundary
  , consumeSMTLibCausalStreamBoundaryWhitespace
  , startSMTLibCausalStreamCursorAtBoundary
  ) where

import Control.DeepSeq (NFData (rnf))
import Data.Bifunctor (first)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Internal.SMTLib.Stream
  ( SMTLibStreamFramer
  , SMTLibStreamFramingError (..)
  , SMTLibStreamFramingStep (..)
  , SMTLibStreamLimitSource (..)
  , SMTLibStreamLimits
  , feedSMTLibStreamFramer
  , finishSMTLibStreamFramer
  , isSMTLibWhitespaceByte
  , mkSMTLibStreamLimits
  , smtLibStreamFrameByteLimit
  , smtLibStreamFramerTotalByteLimit
  , smtLibStreamNestingDepthLimit
  , smtLibStreamTotalByteLimit
  , startSMTLibStreamFramer
  )

-- | One immutable frame policy and cumulative transaction maximum.
data SMTLibCausalStreamPolicy = SMTLibCausalStreamPolicy
  !SMTLibStreamLimits !Natural
  deriving (Eq, Ord)

instance NFData SMTLibCausalStreamPolicy where
  rnf (SMTLibCausalStreamPolicy stream cumulative) =
    rnf stream `seq` rnf cumulative

mkSMTLibCausalStreamPolicy
  :: SMTLibStreamLimits
  -> Natural
  -> SMTLibCausalStreamPolicy
mkSMTLibCausalStreamPolicy = SMTLibCausalStreamPolicy

smtLibCausalStreamPolicyStreamLimits
  :: SMTLibCausalStreamPolicy
  -> SMTLibStreamLimits
smtLibCausalStreamPolicyStreamLimits
    (SMTLibCausalStreamPolicy value _) = value

smtLibCausalStreamPolicyCumulativeByteLimit
  :: SMTLibCausalStreamPolicy
  -> Natural
smtLibCausalStreamPolicyCumulativeByteLimit
    (SMTLibCausalStreamPolicy _ value) = value

-- | Closed transaction-level classification.  A configured per-frame total
-- wins an exact tie; cumulative failure is selected only when the remaining
-- transaction budget is the strictly smaller effective total.
data SMTLibCausalStreamFailure
  = SMTLibCausalStreamFramingFailure !SMTLibStreamFramingError
  | SMTLibCausalStreamCumulativeByteLimitExceeded !Natural !Natural
  | SMTLibCausalStreamUnexpectedBoundaryByte !Natural !Word8
  deriving (Eq, Ord, Show, Generic)

instance NFData SMTLibCausalStreamFailure

-- | Opaque in-progress frame with an absolute transaction offset.
data SMTLibCausalStreamCursor = SMTLibCausalStreamCursor
  !SMTLibCausalStreamPolicy
  !Natural
  !SMTLibStreamFramer

instance NFData SMTLibCausalStreamCursor where
  rnf (SMTLibCausalStreamCursor policy frameStart framer) =
    rnf policy `seq` rnf frameStart `seq` rnf framer

-- | A feed either retains the exact cursor or completes one frame with its
-- policy, absolute end, and untouched tail still associated.
data SMTLibCausalStreamStep
  = SMTLibCausalStreamPending SMTLibCausalStreamCursor
  | SMTLibCausalStreamComplete SMTLibCausalStreamCompletedFrame

-- All fields deliberately remain lazy.  The underlying framing step also
-- leaves frame bytes, charged count, and untouched tail lazy; opening this
-- transient continuation must not move parsing or boundary demand earlier.
data SMTLibCausalStreamCompletedFrame = SMTLibCausalStreamCompletedFrame
  [Word8]
  SMTLibCausalStreamPolicy
  Natural
  [Word8]

-- | Opaque successful causal boundary.  Construction has already validated
-- and charged every retained tail byte; only this value can start the next
-- write's first frame at that exact offset and under that exact policy.
data SMTLibCausalStreamBoundary = SMTLibCausalStreamBoundary
  !SMTLibCausalStreamPolicy !Natural

instance NFData SMTLibCausalStreamBoundary where
  rnf (SMTLibCausalStreamBoundary policy offset) =
    rnf policy `seq` rnf offset

startSMTLibCausalStreamCursor
  :: SMTLibCausalStreamPolicy
  -> SMTLibCausalStreamCursor
startSMTLibCausalStreamCursor policy = startCursorAtOffset policy 0

startCursorAtOffset
  :: SMTLibCausalStreamPolicy
  -> Natural
  -> SMTLibCausalStreamCursor
startCursorAtOffset policy frameStart = SMTLibCausalStreamCursor
  policy frameStart $ startSMTLibStreamFramer effectiveLimits
 where
  configured = smtLibCausalStreamPolicyStreamLimits policy
  cumulativeMaximum =
    smtLibCausalStreamPolicyCumulativeByteLimit policy
  remaining
    | frameStart >= cumulativeMaximum = 0
    | otherwise = cumulativeMaximum - frameStart
  configuredTotal = smtLibStreamTotalByteLimit configured
  effectiveLimits = mkSMTLibStreamLimits SMTLibStreamLimitSource
    { smtLibStreamLimitSourceTotalBytes = min configuredTotal remaining
    , smtLibStreamLimitSourceFrameBytes =
        smtLibStreamFrameByteLimit configured
    , smtLibStreamLimitSourceNestingDepth =
        smtLibStreamNestingDepthLimit configured
    }

feedSMTLibCausalStreamCursor
  :: SMTLibCausalStreamCursor
  -> [Word8]
  -> Either SMTLibCausalStreamFailure SMTLibCausalStreamStep
feedSMTLibCausalStreamCursor cursor bytes = case
    feedSMTLibStreamFramer (cursorFramer cursor) bytes of
  Left failure -> Left $ classifyFramingFailure cursor failure
  Right (SMTLibStreamFramingPending next) -> Right
    $ SMTLibCausalStreamPending $ replaceCursorFramer cursor next
  Right (SMTLibStreamFramingComplete frame tailBytes consumed) -> Right
    $ SMTLibCausalStreamComplete
    $ SMTLibCausalStreamCompletedFrame frame
        (cursorPolicy cursor) (cursorFrameStart cursor + consumed) tailBytes

finishSMTLibCausalStreamCursor
  :: SMTLibCausalStreamCursor
  -> Either SMTLibCausalStreamFailure (Maybe [Word8])
finishSMTLibCausalStreamCursor cursor = first
  (classifyFramingFailure cursor)
  $ finishSMTLibStreamFramer $ cursorFramer cursor

smtLibCausalStreamCompletedFrameBytes
  :: SMTLibCausalStreamCompletedFrame
  -> [Word8]
smtLibCausalStreamCompletedFrameBytes
    (SMTLibCausalStreamCompletedFrame frame _ _ _) = frame

-- | Continue within the same completed write.  The untouched tail is fed to
-- a fresh frame at the associated absolute end; it may itself complete the
-- next frame without waiting for another transport chunk.
continueSMTLibCausalStreamCompletedFrame
  :: SMTLibCausalStreamCompletedFrame
  -> Either SMTLibCausalStreamFailure SMTLibCausalStreamStep
continueSMTLibCausalStreamCompletedFrame
    (SMTLibCausalStreamCompletedFrame _ policy frameEnd tailBytes) =
  feedSMTLibCausalStreamCursor
    (startCursorAtOffset policy frameEnd) tailBytes

-- | Validate a write boundary without exposing the completed frame's tail or
-- absolute offset.  Limit failure precedes byte classification at maximum,
-- and traversal stops at maximum plus one for finite or cyclic input.
consumeSMTLibCausalStreamBoundaryWhitespace
  :: SMTLibCausalStreamCompletedFrame
  -> Either SMTLibCausalStreamFailure SMTLibCausalStreamBoundary
consumeSMTLibCausalStreamBoundaryWhitespace
    (SMTLibCausalStreamCompletedFrame _ policy frameEnd tailBytes) =
  SMTLibCausalStreamBoundary policy <$> go frameEnd tailBytes
 where
  maximumBytes = smtLibCausalStreamPolicyCumulativeByteLimit policy

  go !consumed [] = Right consumed
  go !consumed (byte : bytes)
    | consumed >= maximumBytes = Left $
        SMTLibCausalStreamCumulativeByteLimitExceeded
          maximumBytes (maximumBytes + 1)
    | isSMTLibWhitespaceByte byte = go (consumed + 1) bytes
    | otherwise = Left $
        SMTLibCausalStreamUnexpectedBoundaryByte consumed byte

startSMTLibCausalStreamCursorAtBoundary
  :: SMTLibCausalStreamBoundary
  -> SMTLibCausalStreamCursor
startSMTLibCausalStreamCursorAtBoundary
    (SMTLibCausalStreamBoundary policy offset) =
  startCursorAtOffset policy offset

classifyFramingFailure
  :: SMTLibCausalStreamCursor
  -> SMTLibStreamFramingError
  -> SMTLibCausalStreamFailure
classifyFramingFailure cursor failure = case failure of
  SMTLibStreamTotalByteLimitExceeded _ _
    | smtLibStreamFramerTotalByteLimit (cursorFramer cursor) <
        configuredTotal ->
        SMTLibCausalStreamCumulativeByteLimitExceeded
          cumulativeMaximum (cumulativeMaximum + 1)
  _ -> SMTLibCausalStreamFramingFailure failure
 where
  policy = cursorPolicy cursor
  configuredTotal = smtLibStreamTotalByteLimit
    $ smtLibCausalStreamPolicyStreamLimits policy
  cumulativeMaximum =
    smtLibCausalStreamPolicyCumulativeByteLimit policy

cursorPolicy :: SMTLibCausalStreamCursor -> SMTLibCausalStreamPolicy
cursorPolicy (SMTLibCausalStreamCursor value _ _) = value

cursorFrameStart :: SMTLibCausalStreamCursor -> Natural
cursorFrameStart (SMTLibCausalStreamCursor _ value _) = value

cursorFramer :: SMTLibCausalStreamCursor -> SMTLibStreamFramer
cursorFramer (SMTLibCausalStreamCursor _ _ value) = value

replaceCursorFramer
  :: SMTLibCausalStreamCursor
  -> SMTLibStreamFramer
  -> SMTLibCausalStreamCursor
replaceCursorFramer
    (SMTLibCausalStreamCursor policy frameStart _) framer =
  SMTLibCausalStreamCursor policy frameStart framer
