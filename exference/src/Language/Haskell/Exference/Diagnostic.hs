module Language.Haskell.Exference.Diagnostic
  ( Diagnostic (..)
  , SourcePosition (..)
  , SourceSpan (..)
  , diagnostic
  )
where

-- | A source position deliberately independent of haskell-src-exts.  Keeping
-- diagnostics in the shared frontend vocabulary will make them reusable by a
-- future Djinn/Exference parser without leaking either parser's AST types.
data SourcePosition = SourcePosition
  { sourceLine :: !Int
  , sourceColumn :: !Int
  }
  deriving (Eq, Show)

data SourceSpan = SourceSpan
  { sourceStart :: SourcePosition
  , sourceEnd :: SourcePosition
  }
  deriving (Eq, Show)

data Diagnostic = Diagnostic
  { diagnosticSource :: Maybe FilePath
  , diagnosticSpan :: Maybe SourceSpan
  , diagnosticMessage :: String
  }
  deriving (Eq, Show)

diagnostic :: String -> Diagnostic
diagnostic = Diagnostic Nothing Nothing
