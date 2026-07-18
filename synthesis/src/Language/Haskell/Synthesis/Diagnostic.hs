-- | Parser-independent structured diagnostics and deterministic rendering.
module Language.Haskell.Synthesis.Diagnostic
  ( Severity (..)
  , SourcePosition
  , sourceLine
  , sourceColumn
  , SourceSpan
  , sourceStart
  , sourceEnd
  , SourceLocation
  , sourceLocation
  , locationSource
  , locationSpan
  , SourceLocationError (..)
  , mkSourcePosition
  , mkSourceSpan
  , Diagnostic (..)
  , diagnostic
  , codedDiagnostic
  , contextualDiagnostic
  , shownErrorDiagnostic
  , withCode
  , withSource
  , withSpan
  , withLocation
  , withSourceLocation
  , withOptionalLocation
  , withContext
  , sourceTextSpan
  , sourceTextLocation
  , renderDiagnostic
  ) where

import Control.DeepSeq (NFData (rnf))
import Data.List (intercalate)
import qualified Data.List as List

-- | Presentation severity, ordered from errors through informational notes.
data Severity = Error | Warning | Info
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A one-based line and column in a source file or input buffer.
data SourcePosition = SourcePosition !Int !Int
  deriving (Eq, Ord)

-- | A half-open source range.  The representation is deliberately neutral;
-- parser adapters decide how their native locations map into it.
data SourceSpan = SourceSpan !SourcePosition !SourcePosition
  deriving (Eq, Ord)

-- | A complete parser-independent source location.
--
-- Diagnostics intentionally keep source names and spans independent because
-- filesystem and parser failures can know only one of them. Checked requests,
-- however, always have either no provenance or one complete location. This
-- strict value records that stronger invariant and ensures a span computed
-- from an input buffer is evaluated before a reusable request can retain the
-- buffer accidentally.
data SourceLocation = SourceLocation !FilePath !SourceSpan
  deriving (Eq, Ord, Show)

-- | Why a source position or span could not be represented. Positions are
-- one-based, and a half-open span cannot finish before it starts.
data SourceLocationError
  = NonPositiveSourceLine !Int
  | NonPositiveSourceColumn !Int
  | SourceSpanEndBeforeStart !SourcePosition !SourcePosition
  deriving (Eq, Ord, Show)

-- Ordinary functions, rather than exported record labels, keep record-update
-- syntax from bypassing the smart constructors.
-- | One-based line number.
sourceLine :: SourcePosition -> Int
sourceLine (SourcePosition line _) = line

-- | One-based column number.
sourceColumn :: SourcePosition -> Int
sourceColumn (SourcePosition _ column) = column

-- | Inclusive start of a half-open span.
sourceStart :: SourceSpan -> SourcePosition
sourceStart (SourceSpan start _) = start

-- | Exclusive end of a half-open span.
sourceEnd :: SourceSpan -> SourcePosition
sourceEnd (SourceSpan _ end) = end

-- | Pair a source name with an already validated span.
sourceLocation :: FilePath -> SourceSpan -> SourceLocation
sourceLocation = SourceLocation

-- | Source name retained by a complete location.
locationSource :: SourceLocation -> FilePath
locationSource (SourceLocation source _) = source

-- | Validated span retained by a complete location.
locationSpan :: SourceLocation -> SourceSpan
locationSpan (SourceLocation _ span') = span'

instance Show SourcePosition where
  showsPrec precedence (SourcePosition line column) =
    showParen (precedence > 10)
      $ showString "SourcePosition {sourceLine = "
      . shows line
      . showString ", sourceColumn = "
      . shows column
      . showString "}"

instance Show SourceSpan where
  showsPrec precedence (SourceSpan start end) =
    showParen (precedence > 10)
      $ showString "SourceSpan {sourceStart = "
      . shows start
      . showString ", sourceEnd = "
      . shows end
      . showString "}"

-- | A structured compiler-style diagnostic.
--
-- Codes are stable machine-facing classifications. Context entries retain
-- explanatory detail in production order and render on indented lines below
-- the primary message.
data Diagnostic = Diagnostic
  { diagnosticSeverity :: !Severity
  , diagnosticCode :: Maybe String
  , diagnosticSource :: Maybe FilePath
  , diagnosticSpan :: Maybe SourceSpan
  , diagnosticMessage :: String
  , diagnosticContext :: [String]
  }
  deriving (Eq, Show)

instance NFData Severity where
  rnf Error = ()
  rnf Warning = ()
  rnf Info = ()

instance NFData SourcePosition where
  rnf (SourcePosition line column) = rnf line `seq` rnf column

instance NFData SourceSpan where
  rnf (SourceSpan start end) = rnf start `seq` rnf end

instance NFData SourceLocation where
  rnf (SourceLocation source span') = rnf source `seq` rnf span'

instance NFData SourceLocationError where
  rnf (NonPositiveSourceLine line) = rnf line
  rnf (NonPositiveSourceColumn column) = rnf column
  rnf (SourceSpanEndBeforeStart start end) = rnf start `seq` rnf end

instance NFData Diagnostic where
  rnf value =
    rnf (diagnosticSeverity value) `seq`
    rnf (diagnosticCode value) `seq`
    rnf (diagnosticSource value) `seq`
    rnf (diagnosticSpan value) `seq`
    rnf (diagnosticMessage value) `seq`
    rnf (diagnosticContext value)

-- | Construct a one-based source position.
mkSourcePosition :: Int -> Int -> Either SourceLocationError SourcePosition
mkSourcePosition line column
  | line <= 0 = Left $ NonPositiveSourceLine line
  | column <= 0 = Left $ NonPositiveSourceColumn column
  | otherwise = Right $ SourcePosition line column

-- | Construct a half-open source span whose end is not before its start.
-- Equal endpoints represent a point location.
mkSourceSpan
  :: SourcePosition
  -> SourcePosition
  -> Either SourceLocationError SourceSpan
mkSourceSpan start end
  | end < start = Left $ SourceSpanEndBeforeStart start end
  | otherwise = Right $ SourceSpan start end

-- | Start a diagnostic without optional code, source, span, or context.
diagnostic :: Severity -> String -> Diagnostic
diagnostic severity message = Diagnostic
  { diagnosticSeverity = severity
  , diagnosticCode = Nothing
  , diagnosticSource = Nothing
  , diagnosticSpan = Nothing
  , diagnosticMessage = message
  , diagnosticContext = []
  }

-- | Start a diagnostic with a stable machine-readable code.
codedDiagnostic :: Severity -> String -> String -> Diagnostic
codedDiagnostic severity code = withCode code . diagnostic severity

-- | Start a coded diagnostic with one explanatory context entry.
contextualDiagnostic :: Severity -> String -> String -> String -> Diagnostic
contextualDiagnostic severity code message context =
  withContext context $ codedDiagnostic severity code message

-- | Start an error diagnostic whose explanatory context is the 'Show'
-- representation of a structured failure. Backend adapters use this at
-- representation boundaries where the shared diagnostic vocabulary should
-- retain a backend-specific error without defining another error type.
shownErrorDiagnostic
  :: Show detail
  => String
  -> String
  -> detail
  -> Diagnostic
shownErrorDiagnostic code message detail =
  contextualDiagnostic Error code message (show detail)

-- | Replace or attach the machine-readable diagnostic code.
withCode :: String -> Diagnostic -> Diagnostic
withCode code value = value { diagnosticCode = Just code }

-- | Replace or attach a source name without changing the optional span.
withSource :: FilePath -> Diagnostic -> Diagnostic
withSource source value = value { diagnosticSource = Just source }

-- | Replace or attach a span without changing the optional source name.
withSpan :: SourceSpan -> Diagnostic -> Diagnostic
withSpan span' value = value { diagnosticSpan = Just span' }

-- | Attach a complete source location in one operation.
withLocation :: FilePath -> SourceSpan -> Diagnostic -> Diagnostic
withLocation source span' = withSpan span' . withSource source

-- | Attach one complete source location to a diagnostic.
withSourceLocation :: SourceLocation -> Diagnostic -> Diagnostic
withSourceLocation location =
  withLocation (locationSource location) (locationSpan location)

-- | Attach a complete source location when one is available.
--
-- Programmatic requests deliberately carry no location, whereas source
-- frontends retain the exact filename and span as one optional value. Keeping
-- that distinction here prevents backend adapters from implementing subtly
-- different @Nothing@ behavior.
withOptionalLocation
  :: Maybe SourceLocation
  -> Diagnostic
  -> Diagnostic
withOptionalLocation Nothing = id
withOptionalLocation (Just location) = withSourceLocation location

-- | Add outer-to-inner explanatory context.  Rendering preserves insertion
-- order so adapters can build a readable trail such as module, declaration,
-- and query.
withContext :: String -> Diagnostic -> Diagnostic
withContext context value =
  value { diagnosticContext = diagnosticContext value ++ [context] }

-- | Cover a complete text buffer with a one-based half-open source span.
-- Newlines reset the ending column to one, matching the parser adapters.
sourceTextSpan :: String -> SourceSpan
sourceTextSpan = SourceSpan (SourcePosition 1 1)
  . List.foldl' advance (SourcePosition 1 1)
 where
  -- Saturation is observable only for buffers too large to materialize, but
  -- it keeps this total constructor inside the positive-coordinate invariant
  -- even at the bounds of 'Int'.
  advance (SourcePosition line _) '\n' = SourcePosition (increment line) 1
  advance (SourcePosition line column) _ =
    SourcePosition line $ increment column
  increment value
    | value == maxBound = maxBound
    | otherwise = value + 1

-- | Name and eagerly span a complete source buffer.
--
-- t'SourceLocation' is strict in the span, so evaluating this value traverses
-- the buffer immediately instead of leaving that traversal inside a retained
-- request cache.
sourceTextLocation :: FilePath -> String -> SourceLocation
sourceTextLocation source = SourceLocation source . sourceTextSpan

-- | Render in a compiler-style, single-header format.
--
-- Examples include @file.hs:3:7-12: error [SYN001]: message@ and, without a
-- source, @warning: message@.  Context entries follow on indented lines.
renderDiagnostic :: Diagnostic -> String
renderDiagnostic value =
  renderLocation value ++
  renderSeverity (diagnosticSeverity value) ++
  renderCode (diagnosticCode value) ++
  ": " ++ diagnosticMessage value ++
  concatMap ("\n  context: " ++) (diagnosticContext value)

renderLocation :: Diagnostic -> String
renderLocation value =
  case locationParts of
    [] -> ""
    parts -> intercalate ":" parts ++ ": "
  where
    locationParts =
      maybe [] (: []) (diagnosticSource value) ++
      maybe [] ((: []) . renderSpan) (diagnosticSpan value)

renderSpan :: SourceSpan -> String
renderSpan (SourceSpan start end)
  | sourceLine start == sourceLine end =
      show (sourceLine start) ++ ":" ++ show (sourceColumn start) ++
      renderSameLineEnd start end
  | otherwise =
      show (sourceLine start) ++ ":" ++ show (sourceColumn start) ++
      "-" ++ show (sourceLine end) ++ ":" ++ show (sourceColumn end)

renderSameLineEnd :: SourcePosition -> SourcePosition -> String
renderSameLineEnd start end
  | sourceColumn start == sourceColumn end = ""
  | otherwise = "-" ++ show (sourceColumn end)

renderSeverity :: Severity -> String
renderSeverity Error = "error"
renderSeverity Warning = "warning"
renderSeverity Info = "info"

renderCode :: Maybe String -> String
renderCode Nothing = ""
renderCode (Just "") = ""
renderCode (Just code) = " [" ++ code ++ "]"
