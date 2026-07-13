-- | Parser-independent structured diagnostics and deterministic rendering.
module Language.Haskell.Synthesis.Diagnostic
  ( Severity (..)
  , SourcePosition (..)
  , SourceSpan (..)
  , Diagnostic (..)
  , diagnostic
  , codedDiagnostic
  , contextualDiagnostic
  , withCode
  , withSource
  , withSpan
  , withLocation
  , withContext
  , sourceTextSpan
  , renderDiagnostic
  ) where

import Control.DeepSeq (NFData (rnf))
import Data.List (intercalate)

data Severity = Error | Warning | Info
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A one-based line and column in a source file or input buffer.
data SourcePosition = SourcePosition
  { sourceLine :: !Int
  , sourceColumn :: !Int
  }
  deriving (Eq, Ord, Show)

-- | A half-open source range.  The representation is deliberately neutral;
-- parser adapters decide how their native locations map into it.
data SourceSpan = SourceSpan
  { sourceStart :: !SourcePosition
  , sourceEnd :: !SourcePosition
  }
  deriving (Eq, Ord, Show)

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

instance NFData Diagnostic where
  rnf value =
    rnf (diagnosticSeverity value) `seq`
    rnf (diagnosticCode value) `seq`
    rnf (diagnosticSource value) `seq`
    rnf (diagnosticSpan value) `seq`
    rnf (diagnosticMessage value) `seq`
    rnf (diagnosticContext value)

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

withCode :: String -> Diagnostic -> Diagnostic
withCode code value = value { diagnosticCode = Just code }

withSource :: FilePath -> Diagnostic -> Diagnostic
withSource source value = value { diagnosticSource = Just source }

withSpan :: SourceSpan -> Diagnostic -> Diagnostic
withSpan span' value = value { diagnosticSpan = Just span' }

-- | Attach a complete source location in one operation.
withLocation :: FilePath -> SourceSpan -> Diagnostic -> Diagnostic
withLocation source span' = withSpan span' . withSource source

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
  . foldl advance (SourcePosition 1 1)
 where
  advance (SourcePosition line _) '\n' = SourcePosition (line + 1) 1
  advance (SourcePosition line column) _ = SourcePosition line (column + 1)

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
