-- | Haskell-src-exts loading and query parsing for the Exference backend.
--
-- The stable "Language.Haskell.Djex.Exference" module is deliberately
-- parser-neutral. Applications that consume Haskell source import this
-- frontend explicitly, keeping parser and filesystem dependencies at the
-- command boundary.
module Language.Haskell.Djex.Exference.HaskellSrc
  ( ExferenceSessionLoadReport (..)
  , exferenceCommandSessionPolicy
  , loadExferenceSession
  , loadExferenceSessionWithPolicy
  , parseExferenceRequest
  , parseExferenceRequestWithCheckedTarget
  ) where

import Control.Monad.Trans.Except (runExceptT)
import Data.Bifunctor (first)
import Data.Functor.Identity (runIdentity)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Language.Haskell.Exts.Parser as HSE

import Language.Haskell.Djex.Exference
  ( ExferenceOptions
  , ExferenceRequest
  , ExferenceSession
  , ExferenceSessionPolicy (..)
  , defaultExferenceSessionPolicy
  , exferenceSessionDiagnostics
  , exferenceSessionInventory
  )
import qualified Language.Haskell.Djex.Exference.FrontendSupport as Frontend
import qualified Language.Haskell.Exference.Session as CompatibilitySession
import Language.Haskell.Exference.Core.Types (toSynthesisType)
import Language.Haskell.Exference.EnvironmentParser
  ( LoadReport (..)
  , environmentFromPath
  , environmentLoadErrorDiagnostics
  , haskellSrcExtsParseMode
  )
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc
  ( parseTypeWithInventory )
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , shownErrorDiagnostic
  , sourceTextLocation
  , withCode
  , withSourceLocation
  )
import Language.Haskell.Synthesis.Generated (DefinitionName)
import Language.Haskell.Synthesis.Name (Name, parseName)
import Language.Haskell.Synthesis.Query (QueryRequest (..))

-- | A fully sealed session or structured fatal diagnostics, paired with all
-- non-fatal source-loader and backend-projection diagnostics in production
-- order. No parser-specific environment or error type crosses this boundary.
data ExferenceSessionLoadReport = ExferenceSessionLoadReport
  { exferenceSessionLoadResult
      :: Either (NonEmpty Diagnostic) ExferenceSession
  , exferenceSessionLoadDiagnostics :: [Diagnostic]
  }

-- | The exact session policy shared by the historical @exference@ command
-- and the merged @djex exference@ command.  Programmatic sessions deliberately
-- keep 'defaultExferenceSessionPolicy' unrestricted; command-line synthesis is
-- conservative by default because these well-known helpers can manufacture
-- inhabitants only by introducing general recursion or nontermination.
--
-- Passing 'True' is the explicit command-line opt-in used by @--fix@.  Parsing
-- the structural names here, rather than maintaining occurrence-text filters
-- in each command, keeps qualification exact and turns a malformed built-in
-- policy entry into a controlled diagnostic.
exferenceCommandSessionPolicy
  :: Bool
  -> Either Diagnostic ExferenceSessionPolicy
exferenceCommandSessionPolicy allowRecursionHelpers
  | allowRecursionHelpers = Right defaultExferenceSessionPolicy
  | otherwise = do
      exclusions <- traverse checkedName recursionHelperNames
      pure defaultExferenceSessionPolicy
        { exferenceExcludedBindings = exclusions }
 where
  checkedName source = first
    (shownErrorDiagnostic
      "DJEX_EXF_COMMAND_POLICY"
      "invalid built-in Exference recursion-helper name")
    $ parseName source

  recursionHelperNames =
    [ "Data.Function.fix"
    , "Control.Monad.forever"
    , "Control.Monad.Loops.iterateM_"
    ]

-- | Load a directory of source modules and ratings, validate its complete
-- inventory, and seal an Exference session with the default policy.
loadExferenceSession :: FilePath -> IO ExferenceSessionLoadReport
loadExferenceSession = loadExferenceSessionWithPolicy
  defaultExferenceSessionPolicy

-- | Policy-aware counterpart of 'loadExferenceSession'. Session omission
-- diagnostics follow source-loader diagnostics, matching the order in which
-- the two phases run.
loadExferenceSessionWithPolicy
  :: ExferenceSessionPolicy
  -> FilePath
  -> IO ExferenceSessionLoadReport
loadExferenceSessionWithPolicy policy path = do
  LoadReport sourceResult sourceDiagnostics <- environmentFromPath path
  pure $ case sourceResult of
    Left failure -> ExferenceSessionLoadReport
      { exferenceSessionLoadResult = Left
          $ environmentLoadErrorDiagnostics failure
      , exferenceSessionLoadDiagnostics = sourceDiagnostics
      }
    Right checked -> case
        CompatibilitySession.mkExferenceSessionWithPolicy policy checked of
      Left failure -> ExferenceSessionLoadReport
        { exferenceSessionLoadResult = Left
            $ NonEmpty.singleton failure
        , exferenceSessionLoadDiagnostics = sourceDiagnostics
        }
      Right session -> ExferenceSessionLoadReport
        { exferenceSessionLoadResult = Right session
        , exferenceSessionLoadDiagnostics = sourceDiagnostics
            ++ exferenceSessionDiagnostics session
        }

-- | Parse a Haskell-src-exts type against a sealed session and retain source
-- spellings and location information for later search diagnostics.
parseExferenceRequest
  :: ExferenceSession
  -> ExferenceOptions
  -> Name
  -> FilePath
  -> String
  -> Either Diagnostic ExferenceRequest
parseExferenceRequest session options target sourceName source = do
  -- Preserve command-boundary precedence: an invalid output name is a usage
  -- error even when the source text is also malformed.
  checkedTarget <- Frontend.validateExferenceTarget target
  parseExferenceRequestWithCheckedTarget
    session options checkedTarget sourceName source

-- | Parse a Haskell source type for a target already checked at an outer
-- command boundary. The hidden 'DefinitionName' constructor makes repeating
-- the backend-specific target preflight unnecessary.
parseExferenceRequestWithCheckedTarget
  :: ExferenceSession
  -> ExferenceOptions
  -> DefinitionName
  -> FilePath
  -> String
  -> Either Diagnostic ExferenceRequest
parseExferenceRequestWithCheckedTarget session options checkedTarget
    sourceName source = do
  let mode = haskellSrcExtsParseMode sourceName
      location = sourceTextLocation (HSE.parseFilename mode) source
      parsed = runIdentity $ runExceptT $ parseTypeWithInventory
        (exferenceSessionInventory session)
        Nothing
        mode
        source
  -- The HSE compatibility frontend predates structured diagnostic codes.
  -- Seal every failure at this boundary while preserving its exact message,
  -- source, and span.
  (backendType, sourceVariables) <- first
    (withCode "DJEX_EXF_PARSE") parsed
  sharedType <- either
    (Left . withSourceLocation location . shownErrorDiagnostic
      "DJEX_EXF_PARSE" "parsed Exference type failed shared validation"
    )
    Right
    $ toSynthesisType backendType
  let query = QueryRequest
        { requestTarget = checkedTarget
        , requestGoal = sharedType
        , requestContexts = []
        , requestOptions = options
        }
  Frontend.mkExferenceRequestWithSourceInfo
    sourceVariables location query
