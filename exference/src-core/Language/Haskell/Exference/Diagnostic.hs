-- | Compatibility facade for the shared synthesis diagnostic vocabulary.
--
-- Exference historically exposed a one-argument 'diagnostic' constructor.
-- Keep that convenience while using the same structured value, source span,
-- rendering, and context model as every other synthesis backend.
module Language.Haskell.Exference.Diagnostic
  ( Diagnostic (..)
  , Severity (..)
  , SourcePosition
  , sourceLine
  , sourceColumn
  , SourceSpan
  , sourceStart
  , sourceEnd
  , SourceLocationError (..)
  , mkSourcePosition
  , mkSourceSpan
  , diagnostic
  , codedDiagnostic
  , contextualDiagnostic
  , shownErrorDiagnostic
  , withCode
  , withSource
  , withSpan
  , withLocation
  , withContext
  , sourceTextSpan
  , renderDiagnostic
  )
where

import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic (..)
  , Severity (..)
  , SourcePosition
  , sourceLine
  , sourceColumn
  , SourceSpan
  , sourceStart
  , sourceEnd
  , SourceLocationError (..)
  , mkSourcePosition
  , mkSourceSpan
  , codedDiagnostic
  , contextualDiagnostic
  , shownErrorDiagnostic
  , renderDiagnostic
  , sourceTextSpan
  , withCode
  , withContext
  , withLocation
  , withSource
  , withSpan
  )
import qualified Language.Haskell.Synthesis.Diagnostic as Shared

-- | Construct an error diagnostic with no code, source, span, or context.
-- This is the source-compatible form of Exference's historical helper.
diagnostic :: String -> Diagnostic
diagnostic = Shared.diagnostic Error
