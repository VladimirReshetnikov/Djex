{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Package-private, bounded framing of one SMT-LIB 2.7 response.
--
-- This module is deliberately smaller than an S-expression parser.  It finds
-- one exact top-level lexical frame across arbitrary chunks, preserving every
-- byte inside that frame and returning the untouched tail after it.  Complete
-- frames must still pass the existing bounded parser and a command-specific
-- decoder before they can become raw solver observations.
--
-- SMT-LIB strings may contain newlines and use a doubled quote as their only
-- escape.  Quoted symbols may contain whitespace, parentheses, semicolons,
-- and quotes.  Comments end at either CR or LF.  Consequently neither line
-- reads nor parenthesis counting alone can frame solver output safely.
--
-- The standard also requires every response which is neither double-quoted
-- nor parenthesized to be followed by a whitespace character.  V1 therefore
-- never accepts a bare or quoted-symbol response merely because a chunk or
-- the process ended.
module Language.Haskell.Synthesis.Internal.SMTLib.Stream
  ( smtLibStreamFramingSchemaTag
  , smtLibEchoSentinelNonceByteCount
  , SMTLibEchoSentinel
  , SMTLibEchoSentinelError (..)
  , mkSMTLibEchoSentinel
  , smtLibEchoSentinelCommandBytes
  , smtLibEchoSentinelResponseBytes
  , isExactSMTLibEchoSentinelResponse
  , SMTLibStreamLimitSource (..)
  , SMTLibStreamLimits
  , mkSMTLibStreamLimits
  , defaultSMTLibStreamLimitSource
  , defaultSMTLibStreamLimits
  , smtLibStreamTotalByteLimit
  , smtLibStreamFrameByteLimit
  , smtLibStreamNestingDepthLimit
  , SMTLibStreamFramingError (..)
  , SMTLibStreamFramingStep (..)
  , SMTLibStreamFramer
  , startSMTLibStreamFramer
  , feedSMTLibStreamFramer
  , finishSMTLibStreamFramer
  , smtLibStreamFramerTotalByteLimit
  , smtLibStreamFramerConsumedBytes
  ) where

import Control.DeepSeq (NFData (rnf))
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Internal.SMTLib.Lexical
  ( isSMTLibBareDelimiterByte
  , isSMTLibQuotedSymbolCharacterByte
  , isSMTLibStringCharacterByte
  , isSMTLibWhitespaceByte
  , retainExactByteCount )

-- | Lexical and charged-byte accounting policy bound by higher protocol and
-- future live-session identities.  V2 adds the exact completion count without
-- changing accepted frame syntax.
smtLibStreamFramingSchemaTag :: [Word8]
smtLibStreamFramingSchemaTag =
  ascii "djex-smtlib2-stream-framing/v2"

-- | Exact number of nonce bytes 'mkSMTLibEchoSentinel' requires (256 bits);
-- any other length is rejected with 'SMTLibEchoSentinelNonceLengthMismatch'.
smtLibEchoSentinelNonceByteCount :: Natural
smtLibEchoSentinelNonceByteCount = 32

-- | One package-owned echo marker.  Its fixed safe spelling needs no SMT-LIB
-- escaping.  The retained exact response includes the surrounding double
-- quotes required by the standard's @echo@ command; the command is its fixed
-- canonical wrapper.
data SMTLibEchoSentinel = SMTLibEchoSentinel [Word8]

instance Eq SMTLibEchoSentinel where
  SMTLibEchoSentinel left == SMTLibEchoSentinel right = left == right

instance NFData SMTLibEchoSentinel where
  rnf (SMTLibEchoSentinel response) = rnf response

-- | Why 'mkSMTLibEchoSentinel' rejected a nonce.  The mismatch carries the
-- expected count followed by the observed count, which stops at expected
-- plus one for over-long input.
data SMTLibEchoSentinelError
  = SMTLibEchoSentinelNonceLengthMismatch !Natural !Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData SMTLibEchoSentinelError

-- | Build one sentinel from exactly 256 bits of caller-supplied barrier nonce.
-- Length observation stops at 33 bytes, so cyclic input is rejected
-- productively and no error retains nonce material.  Construction proves
-- length, not freshness or entropy; a live session must supply a distinct
-- nonce for every positional protocol barrier and bind it into run identity.
mkSMTLibEchoSentinel
  :: [Word8]
  -> Either SMTLibEchoSentinelError SMTLibEchoSentinel
mkSMTLibEchoSentinel rawNonce = do
  nonce <- retainExactNonce rawNonce
  let content = ascii "djex-smtlib-frame/v1/" ++ concatMap hexadecimalByte nonce
      response = doubleQuote : content ++ [doubleQuote]
  pure $ SMTLibEchoSentinel response

-- | Exact @echo@ command bytes to send for this sentinel: the response
-- string wrapped in @(echo ...)@ and terminated by a single line feed.
smtLibEchoSentinelCommandBytes :: SMTLibEchoSentinel -> [Word8]
smtLibEchoSentinelCommandBytes (SMTLibEchoSentinel response) =
  ascii "(echo " ++ response ++ [closeParen, lineFeed]

-- | Exact frame bytes the solver must echo back, including the surrounding
-- double quotes.  A frame equal to these bytes satisfies
-- 'isExactSMTLibEchoSentinelResponse'.
smtLibEchoSentinelResponseBytes :: SMTLibEchoSentinel -> [Word8]
smtLibEchoSentinelResponseBytes (SMTLibEchoSentinel response) = response

-- | Exact positional comparison only.  The future protocol layer must call
-- this for the frame where its state machine expects a sentinel; it must not
-- scan later frames for a matching substring.
isExactSMTLibEchoSentinelResponse
  :: SMTLibEchoSentinel
  -> [Word8]
  -> Bool
isExactSMTLibEchoSentinelResponse sentinel bytes =
  bytes == smtLibEchoSentinelResponseBytes sentinel

-- | Independent bounds for one expected response frame.  The total bound
-- charges discarded leading trivia and every retained expression byte.  A
-- whitespace lookahead which establishes the end of a bare response belongs
-- to the untouched tail and is charged by the next frame or live transcript.
-- The frame bound charges only retained expression bytes.
-- Both are required: an infinite leading comment must not evade the retained
-- frame limit, while a compact frame must not retain more than its own bound.
data SMTLibStreamLimitSource = SMTLibStreamLimitSource
  { smtLibStreamLimitSourceTotalBytes :: Natural
  , smtLibStreamLimitSourceFrameBytes :: Natural
  , smtLibStreamLimitSourceNestingDepth :: Natural
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData SMTLibStreamLimitSource

-- | Retained framing bounds, in the order total bytes, frame bytes, nesting
-- depth.  Constructed only through 'mkSMTLibStreamLimits' and read back
-- through the three @smtLibStream...Limit@ projections.
data SMTLibStreamLimits = SMTLibStreamLimits
  !Natural !Natural !Natural
  deriving (Eq, Ord, Show)

instance NFData SMTLibStreamLimits where
  rnf (SMTLibStreamLimits total frame depth) =
    rnf total `seq` rnf frame `seq` rnf depth

-- | Retain the three source bounds verbatim; no validation is performed, so
-- any 'Natural' values are accepted.
mkSMTLibStreamLimits :: SMTLibStreamLimitSource -> SMTLibStreamLimits
mkSMTLibStreamLimits source = SMTLibStreamLimits
  (smtLibStreamLimitSourceTotalBytes source)
  (smtLibStreamLimitSourceFrameBytes source)
  (smtLibStreamLimitSourceNestingDepth source)

-- | Default raw bounds: 131072 total bytes, 65536 retained frame bytes, and
-- a nesting depth of 64.
defaultSMTLibStreamLimitSource :: SMTLibStreamLimitSource
defaultSMTLibStreamLimitSource = SMTLibStreamLimitSource
  { smtLibStreamLimitSourceTotalBytes = 131072
  , smtLibStreamLimitSourceFrameBytes = 65536
  , smtLibStreamLimitSourceNestingDepth = 64
  }

-- | 'defaultSMTLibStreamLimitSource' retained through
-- 'mkSMTLibStreamLimits'.
defaultSMTLibStreamLimits :: SMTLibStreamLimits
defaultSMTLibStreamLimits =
  mkSMTLibStreamLimits defaultSMTLibStreamLimitSource

-- | Maximum number of bytes one frame may charge, counting discarded leading
-- trivia as well as retained frame bytes; the framer fails with
-- 'SMTLibStreamTotalByteLimitExceeded' on the first byte past it.
smtLibStreamTotalByteLimit :: SMTLibStreamLimits -> Natural
smtLibStreamTotalByteLimit (SMTLibStreamLimits value _ _) = value

-- | Maximum number of retained expression bytes in one frame; the framer
-- fails with 'SMTLibStreamFrameByteLimitExceeded' on the first byte past it.
smtLibStreamFrameByteLimit :: SMTLibStreamLimits -> Natural
smtLibStreamFrameByteLimit (SMTLibStreamLimits _ value _) = value

-- | Maximum number of simultaneously open parenthesised lists; opening one
-- more fails with 'SMTLibStreamNestingDepthLimitExceeded'.
smtLibStreamNestingDepthLimit :: SMTLibStreamLimits -> Natural
smtLibStreamNestingDepthLimit (SMTLibStreamLimits _ _ value) = value

-- | Fixed-precedence framing failures.  Global offsets are zero-based and
-- observed limit counts stop at maximum plus one.
data SMTLibStreamFramingError
  = SMTLibStreamTotalByteLimitExceeded !Natural !Natural
  | SMTLibStreamFrameByteLimitExceeded !Natural !Natural
  | SMTLibStreamNestingDepthLimitExceeded !Natural !Natural
  | SMTLibStreamUnexpectedClosingParenthesis !Natural
  | SMTLibStreamInvalidBareByte !Natural !Word8
  | SMTLibStreamInvalidStringByte !Natural !Word8
  | SMTLibStreamInvalidQuotedSymbolByte !Natural !Word8
  | SMTLibStreamNonWhitespaceAfterAtom !Natural !Word8
  | SMTLibStreamMissingWhitespaceAfterAtom !Natural
  | SMTLibStreamUnterminatedList !Natural
  | SMTLibStreamUnterminatedString !Natural
  | SMTLibStreamUnterminatedQuotedSymbol !Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData SMTLibStreamFramingError

-- | A chunk either leaves an opaque continuation or completes exactly one
-- frame.  The tail is the original suffix following that frame.  The final
-- field of 'SMTLibStreamFramingComplete' is the exact number of bytes charged
-- to this frame, including discarded leading trivia and retained frame bytes.
-- Completion may inspect the tail's first byte as bounded lexical lookahead,
-- but that byte and the rest of the untouched tail are not charged.
data SMTLibStreamFramingStep
  = SMTLibStreamFramingPending SMTLibStreamFramer
  | SMTLibStreamFramingComplete [Word8] [Word8] Natural

data StreamMode
  = StreamNormal
  | StreamBare !Natural
  | StreamString !Natural
  | StreamStringAfterQuote !Natural
  | StreamQuotedSymbol !Natural
  | StreamQuotedSymbolNeedsWhitespace !Natural
  | StreamComment

instance NFData StreamMode where
  rnf StreamNormal = ()
  rnf (StreamBare opening) = rnf opening
  rnf (StreamString opening) = rnf opening
  rnf (StreamStringAfterQuote opening) = rnf opening
  rnf (StreamQuotedSymbol opening) = rnf opening
  rnf (StreamQuotedSymbolNeedsWhitespace opening) = rnf opening
  rnf StreamComment = ()

-- | The opening-offset stack is bounded by depth and the reversed frame by
-- retained bytes.  No constructor or raw buffer projection escapes the
-- package boundary.
data SMTLibStreamFramer = SMTLibStreamFramer
  { streamLimits :: !SMTLibStreamLimits
  , streamConsumed :: !Natural
  , streamOpenLists :: [Natural]
  , streamDepth :: !Natural
  , streamMode :: !StreamMode
  , streamFrameReversed :: [Word8]
  , streamFrameBytes :: !Natural
  }

instance NFData SMTLibStreamFramer where
  rnf (SMTLibStreamFramer limits consumed openLists depth mode frame bytes) =
    rnf limits `seq`
    rnf consumed `seq`
    rnf openLists `seq`
    rnf depth `seq`
    rnf mode `seq`
    rnf frame `seq`
    rnf bytes

-- | Fresh framer for one expected response frame: nothing consumed, no open
-- lists, no retained bytes.  Feed chunks with 'feedSMTLibStreamFramer' and
-- resolve end of input with 'finishSMTLibStreamFramer'.
startSMTLibStreamFramer :: SMTLibStreamLimits -> SMTLibStreamFramer
startSMTLibStreamFramer limits = SMTLibStreamFramer
  { streamLimits = limits
  , streamConsumed = 0
  , streamOpenLists = []
  , streamDepth = 0
  , streamMode = StreamNormal
  , streamFrameReversed = []
  , streamFrameBytes = 0
  }

-- | Feed any finite or cyclic lazy byte chunk.  Completion stops at the first
-- top-level frame.  It may inspect the returned tail's first byte to establish
-- a lexical boundary, but never evaluates beyond that byte.
feedSMTLibStreamFramer
  :: SMTLibStreamFramer
  -> [Word8]
  -> Either SMTLibStreamFramingError SMTLibStreamFramingStep
feedSMTLibStreamFramer = go
 where
  go !framer input = case input of
    [] -> Right $ SMTLibStreamFramingPending framer
    byte : remaining -> case streamMode framer of
      StreamStringAfterQuote _
        | not (insideList framer), byte /= doubleQuote ->
            Right $ completeFrame framer input
      StreamBare _
        | not (insideList framer), isSMTLibWhitespaceByte byte ->
            Right $ completeFrame framer input
      StreamQuotedSymbolNeedsWhitespace _
        | isSMTLibWhitespaceByte byte -> Right $ completeFrame framer input
      _ -> do
        (offset, charged) <- chargeTotalByte framer
        transition <- processChargedByte offset charged byte
        case transition of
          StreamContinue next -> go next remaining
          StreamComplete next -> Right $ completeFrame next remaining

-- | Resolve EOF lexically.  A final quote can close a top-level string, but a
-- bare or quoted-symbol response still lacks the whitespace required by
-- SMT-LIB 2.7.  Clean trivia produces 'Nothing'; the live transport layer then
-- decides whether EOF means a missing response or sentinel.
finishSMTLibStreamFramer
  :: SMTLibStreamFramer
  -> Either SMTLibStreamFramingError (Maybe [Word8])
finishSMTLibStreamFramer framer = case streamMode framer of
  StreamString opening -> Left $ SMTLibStreamUnterminatedString opening
  StreamQuotedSymbol opening ->
    Left $ SMTLibStreamUnterminatedQuotedSymbol opening
  StreamStringAfterQuote _
    | insideList framer -> unterminatedList framer
    | otherwise -> Right $ Just $ currentFrame framer
  StreamBare _
    | insideList framer -> unterminatedList framer
    | otherwise -> Left $ SMTLibStreamMissingWhitespaceAfterAtom
        $ streamConsumed framer
  StreamQuotedSymbolNeedsWhitespace _ ->
    Left $ SMTLibStreamMissingWhitespaceAfterAtom $ streamConsumed framer
  StreamNormal
    | insideList framer -> unterminatedList framer
    | otherwise -> Right Nothing
  StreamComment
    | insideList framer -> unterminatedList framer
    | otherwise -> Right Nothing

-- | Number of bytes charged so far against the total bound, including
-- discarded leading trivia; a completion lookahead byte is not counted.
smtLibStreamFramerConsumedBytes :: SMTLibStreamFramer -> Natural
smtLibStreamFramerConsumedBytes = streamConsumed

-- | Effective total bound retained by this package-private continuation.
smtLibStreamFramerTotalByteLimit :: SMTLibStreamFramer -> Natural
smtLibStreamFramerTotalByteLimit = smtLibStreamTotalByteLimit . streamLimits

data StreamTransition
  = StreamContinue !SMTLibStreamFramer
  | StreamComplete !SMTLibStreamFramer

processChargedByte
  :: Natural
  -> SMTLibStreamFramer
  -> Word8
  -> Either SMTLibStreamFramingError StreamTransition
processChargedByte offset framer byte = case streamMode framer of
  StreamNormal -> processNormalByte offset framer byte
  StreamBare opening -> processBareByte offset opening framer byte
  StreamString opening -> processStringByte offset opening framer byte
  StreamStringAfterQuote opening ->
    processAfterStringQuote offset opening framer byte
  StreamQuotedSymbol opening ->
    processQuotedSymbolByte offset opening framer byte
  StreamQuotedSymbolNeedsWhitespace opening ->
    processAtomWhitespace offset opening framer byte
  StreamComment -> processCommentByte framer byte

processNormalByte
  :: Natural
  -> SMTLibStreamFramer
  -> Word8
  -> Either SMTLibStreamFramingError StreamTransition
processNormalByte offset framer byte
  | isSMTLibWhitespaceByte byte = continueInsideListOrIgnore framer byte
  | byte == semicolon = do
      retained <- retainInsideListOrIgnore framer byte
      pure $ StreamContinue retained { streamMode = StreamComment }
  | byte == openParen = do
      retained <- appendFrameByte framer byte
      opened <- pushList offset retained
      pure $ StreamContinue opened
  | byte == closeParen = if insideList framer
      then closeList offset framer
      else do
        _ <- appendFrameByte framer byte
        Left $ SMTLibStreamUnexpectedClosingParenthesis offset
  | byte == doubleQuote = do
      retained <- appendFrameByte framer byte
      pure $ StreamContinue retained { streamMode = StreamString offset }
  | byte == verticalBar = do
      retained <- appendFrameByte framer byte
      pure $ StreamContinue retained
        { streamMode = StreamQuotedSymbol offset }
  | otherwise = do
      retained <- appendFrameByte framer byte
      if isPotentialBareByte byte
        then pure $ StreamContinue retained { streamMode = StreamBare offset }
        else Left $ SMTLibStreamInvalidBareByte offset byte

processBareByte
  :: Natural
  -> Natural
  -> SMTLibStreamFramer
  -> Word8
  -> Either SMTLibStreamFramingError StreamTransition
processBareByte offset opening framer byte
  | isSMTLibBareDelimiterByte byte =
      if insideList framer
        then processNormalByte offset
          framer { streamMode = StreamNormal } byte
        else if isSMTLibWhitespaceByte byte
          then pure $ StreamComplete framer { streamMode = StreamNormal }
          else Left $ SMTLibStreamNonWhitespaceAfterAtom offset byte
  | otherwise = do
      retained <- appendFrameByte framer byte
      if isPotentialBareByte byte
        then pure $ StreamContinue retained { streamMode = StreamBare opening }
        else Left $ SMTLibStreamInvalidBareByte offset byte

processStringByte
  :: Natural
  -> Natural
  -> SMTLibStreamFramer
  -> Word8
  -> Either SMTLibStreamFramingError StreamTransition
processStringByte offset opening framer byte = do
  retained <- appendFrameByte framer byte
  if byte == doubleQuote
    then pure $ StreamContinue retained
      { streamMode = StreamStringAfterQuote opening }
    else if isSMTLibStringCharacterByte byte
      then pure $ StreamContinue retained
    else Left $ SMTLibStreamInvalidStringByte offset byte

processAfterStringQuote
  :: Natural
  -> Natural
  -> SMTLibStreamFramer
  -> Word8
  -> Either SMTLibStreamFramingError StreamTransition
processAfterStringQuote offset opening framer byte
  | byte == doubleQuote = do
      retained <- appendFrameByte framer byte
      pure $ StreamContinue retained { streamMode = StreamString opening }
  | insideList framer = processNormalByte offset
      framer { streamMode = StreamNormal } byte
  | otherwise = pure $ StreamComplete framer

processQuotedSymbolByte
  :: Natural
  -> Natural
  -> SMTLibStreamFramer
  -> Word8
  -> Either SMTLibStreamFramingError StreamTransition
processQuotedSymbolByte offset opening framer byte = do
  retained <- appendFrameByte framer byte
  if byte == verticalBar
    then if insideList retained
      then pure $ StreamContinue retained { streamMode = StreamNormal }
      else pure $ StreamContinue retained
        { streamMode = StreamQuotedSymbolNeedsWhitespace opening }
    else if byte /= backslash && isSMTLibQuotedSymbolCharacterByte byte
      then pure $ StreamContinue retained
      else Left $ SMTLibStreamInvalidQuotedSymbolByte offset byte

processAtomWhitespace
  :: Natural
  -> Natural
  -> SMTLibStreamFramer
  -> Word8
  -> Either SMTLibStreamFramingError StreamTransition
processAtomWhitespace offset _opening framer byte
  | isSMTLibWhitespaceByte byte = pure $ StreamComplete framer
      { streamMode = StreamNormal }
  | otherwise = Left $ SMTLibStreamNonWhitespaceAfterAtom offset byte

processCommentByte
  :: SMTLibStreamFramer
  -> Word8
  -> Either SMTLibStreamFramingError StreamTransition
processCommentByte framer byte = do
  retained <- retainInsideListOrIgnore framer byte
  pure $ StreamContinue $ if byte == lineFeed || byte == carriageReturn
    then retained { streamMode = StreamNormal }
    else retained

continueInsideListOrIgnore
  :: SMTLibStreamFramer
  -> Word8
  -> Either SMTLibStreamFramingError StreamTransition
continueInsideListOrIgnore framer byte =
  StreamContinue <$> retainInsideListOrIgnore framer byte

retainInsideListOrIgnore
  :: SMTLibStreamFramer
  -> Word8
  -> Either SMTLibStreamFramingError SMTLibStreamFramer
retainInsideListOrIgnore framer byte
  | insideList framer = appendFrameByte framer byte
  | otherwise = Right framer

pushList
  :: Natural
  -> SMTLibStreamFramer
  -> Either SMTLibStreamFramingError SMTLibStreamFramer
pushList offset framer
  | observed < maximumDepth = Right framer
      { streamOpenLists = offset : streamOpenLists framer
      , streamDepth = observed + 1
      }
  | otherwise = Left $ SMTLibStreamNestingDepthLimitExceeded
      maximumDepth (maximumDepth + 1)
 where
  observed = streamDepth framer
  maximumDepth = smtLibStreamNestingDepthLimit $ streamLimits framer

closeList
  :: Natural
  -> SMTLibStreamFramer
  -> Either SMTLibStreamFramingError StreamTransition
closeList offset framer = case streamOpenLists framer of
  [] -> Left $ SMTLibStreamUnexpectedClosingParenthesis offset
  _ : remaining -> do
    retained <- appendFrameByte framer closeParen
    let closed = retained
          { streamOpenLists = remaining
          , streamDepth = streamDepth retained - 1
          }
    pure $ if null remaining
      then StreamComplete closed
      else StreamContinue closed

chargeTotalByte
  :: SMTLibStreamFramer
  -> Either SMTLibStreamFramingError (Natural, SMTLibStreamFramer)
chargeTotalByte framer
  | observed < maximumBytes = Right
      ( observed
      , framer { streamConsumed = observed + 1 }
      )
  | otherwise = Left $ SMTLibStreamTotalByteLimitExceeded
      maximumBytes (maximumBytes + 1)
 where
  observed = streamConsumed framer
  maximumBytes = smtLibStreamTotalByteLimit $ streamLimits framer

appendFrameByte
  :: SMTLibStreamFramer
  -> Word8
  -> Either SMTLibStreamFramingError SMTLibStreamFramer
appendFrameByte framer byte
  | observed < maximumBytes = Right framer
      { streamFrameReversed = byte : streamFrameReversed framer
      , streamFrameBytes = observed + 1
      }
  | otherwise = Left $ SMTLibStreamFrameByteLimitExceeded
      maximumBytes (maximumBytes + 1)
 where
  observed = streamFrameBytes framer
  maximumBytes = smtLibStreamFrameByteLimit $ streamLimits framer

completeFrame :: SMTLibStreamFramer -> [Word8] -> SMTLibStreamFramingStep
completeFrame framer tailBytes = SMTLibStreamFramingComplete
  (currentFrame framer) tailBytes (streamConsumed framer)

currentFrame :: SMTLibStreamFramer -> [Word8]
currentFrame = reverse . streamFrameReversed

unterminatedList
  :: SMTLibStreamFramer
  -> Either SMTLibStreamFramingError value
unterminatedList framer = case streamOpenLists framer of
  opening : _ -> Left $ SMTLibStreamUnterminatedList opening
  [] -> Left $ SMTLibStreamUnterminatedList $ streamConsumed framer

retainExactNonce
  :: [Word8]
  -> Either SMTLibEchoSentinelError [Word8]
retainExactNonce = retainExactByteCount smtLibEchoSentinelNonceByteCount
  SMTLibEchoSentinelNonceLengthMismatch

hexadecimalByte :: Word8 -> [Word8]
hexadecimalByte byte =
  [ hexadecimalDigit $ byte `div` 16
  , hexadecimalDigit $ byte `mod` 16
  ]

hexadecimalDigit :: Word8 -> Word8
hexadecimalDigit value
  | value < 10 = digitZero + value
  | otherwise = lowerA + value - 10

insideList :: SMTLibStreamFramer -> Bool
insideList framer = streamDepth framer /= 0

isPotentialBareByte :: Word8 -> Bool
isPotentialBareByte byte = byte >= 33 && byte <= 126 &&
  not (byte == openParen || byte == closeParen || byte == semicolon ||
    byte == doubleQuote || byte == verticalBar)

ascii :: String -> [Word8]
ascii = map $ fromIntegral . fromEnum

openParen, closeParen, semicolon, doubleQuote, verticalBar, backslash,
  lineFeed, carriageReturn, digitZero, lowerA :: Word8
openParen = 40
closeParen = 41
semicolon = 59
doubleQuote = 34
verticalBar = 124
backslash = 92
lineFeed = 10
carriageReturn = 13
digitZero = 48
lowerA = 97
