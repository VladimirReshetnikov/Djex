-- | One extraction-phase failure with its owning declaration's source
-- location, when the parse tree provides a representable one.
--
-- The HSE extraction phases historically reported bare strings; loader
-- diagnostics therefore carried no source span even though every failing
-- declaration was matched from a located parse tree. This value pairs the
-- exact historical message with an optional location, so the string-typed
-- compatibility extractors remain byte-identical projections while the
-- loader's diagnostics gain real positions. Codes, severities, and generic
-- messages stay owned by the loader's diagnostic adapter.
module Language.Haskell.Exference.ExtractionError
  ( ExtractionError (..)
  , extractionError
  , extractionErrorAt
  , mapExtractionMessage
  , srcSpanInfoLocation
  , withExtractionLocation
  ) where

import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import qualified Language.Haskell.Exts.SrcLoc as HSE

import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , SourceLocation
  , mkSourcePosition
  , mkSourceSpan
  , sourceLocation
  , withOptionalLocation
  )

data ExtractionError = ExtractionError
  { extractionErrorLocation :: Maybe SourceLocation
  , extractionErrorMessage :: String
  }
  deriving (Eq, Show)

-- | A failure with no representable source position. Genuinely locationless
-- phases (for example the hard-coded built-in environment) and hand-built
-- parse trees with synthetic coordinates use this form.
extractionError :: String -> ExtractionError
extractionError = ExtractionError Nothing

extractionErrorAt :: SrcSpanInfo -> String -> ExtractionError
extractionErrorAt info = ExtractionError (srcSpanInfoLocation info)

-- | Rewrite the historical message while keeping the location. Extraction
-- phases use this for their established prefix and suffix decorations.
mapExtractionMessage
  :: (String -> String)
  -> ExtractionError
  -> ExtractionError
mapExtractionMessage transform failure =
  failure {extractionErrorMessage = transform $ extractionErrorMessage failure}

-- | Validate HSE coordinates with the same degradation ladder as the
-- module-level span adapter: the full span when representable, otherwise a
-- point at its start, otherwise no location. HSE promises positive ordered
-- coordinates, so the fallbacks matter only for constructed parse trees.
srcSpanInfoLocation :: SrcSpanInfo -> Maybe SourceLocation
srcSpanInfoLocation info = case fullSpan of
  Right span' -> Just $ sourceLocation source span'
  Left _ -> case pointSpan of
    Right span' -> Just $ sourceLocation source span'
    Left _ -> Nothing
 where
  nativeSpan = HSE.srcInfoSpan info
  source = HSE.srcSpanFilename nativeSpan
  start = mkSourcePosition
    (HSE.srcSpanStartLine nativeSpan)
    (HSE.srcSpanStartColumn nativeSpan)
  fullSpan = do
    startPosition <- start
    end <- mkSourcePosition
      (HSE.srcSpanEndLine nativeSpan)
      (HSE.srcSpanEndColumn nativeSpan)
    mkSourceSpan startPosition end
  pointSpan = do
    startPosition <- start
    mkSourceSpan startPosition startPosition

-- | Attach the failure's location to a diagnostic, when one exists.
withExtractionLocation :: ExtractionError -> Diagnostic -> Diagnostic
withExtractionLocation = withOptionalLocation . extractionErrorLocation
