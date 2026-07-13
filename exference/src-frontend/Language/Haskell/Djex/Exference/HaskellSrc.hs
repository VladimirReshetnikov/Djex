-- | Haskell-src-exts loading and query parsing for the Exference backend.
--
-- The stable "Language.Haskell.Djex.Exference" module is deliberately
-- parser-neutral. Applications that consume Haskell source import this
-- frontend explicitly, keeping parser and filesystem dependencies at the
-- command boundary.
module Language.Haskell.Djex.Exference.HaskellSrc
  ( ExferenceSessionLoadReport (..)
  , loadExferenceSession
  , loadExferenceSessionWithPolicy
  , parseExferenceRequest
  ) where

import Control.Monad.Trans.Except (runExceptT)
import Data.Bifunctor (first)
import Data.Functor.Identity (runIdentity)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map

import Language.Haskell.Djex.Exference
  ( ExferenceLocal
  , ExferenceOptions
  , ExferenceRequest
  , ExferenceSession
  , ExferenceSessionPolicy
  , defaultExferenceSessionPolicy
  , exferenceSessionDiagnostics
  , exferenceSessionInventory
  )
import qualified Language.Haskell.Djex.Exference.Internal.Frontend as Frontend
import qualified Language.Haskell.Exference.Session as CompatibilitySession
import Language.Haskell.Exference.Core.Types (toSynthesisType)
import Language.Haskell.Exference.EnvironmentParser
  ( LoadReport (..)
  , environmentFromPath
  , environmentLoadErrorDiagnostics
  , haskellSrcExtsParseMode
  )
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc
  ( parseTypeWithKinds )
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error)
  , SourceSpan
  , contextualDiagnostic
  , sourceTextSpan
  , withCode
  )
import Language.Haskell.Synthesis.Inventory
  ( inventoryKindAssumptions )
import Language.Haskell.Synthesis.Name (Name)
import Language.Haskell.Synthesis.Query (QueryRequest (..))

-- | A fully sealed session or structured fatal diagnostics, paired with all
-- non-fatal source-loader and backend-projection diagnostics in production
-- order. No parser-specific environment or error type crosses this boundary.
data ExferenceSessionLoadReport = ExferenceSessionLoadReport
  { exferenceSessionLoadResult
      :: Either (NonEmpty Diagnostic) ExferenceSession
  , exferenceSessionLoadDiagnostics :: [Diagnostic]
  }

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
  let parsed = runIdentity $ runExceptT $ parseTypeWithKinds
        (inventoryKindAssumptions $ exferenceSessionInventory session)
        (Frontend.sessionClasses session)
        Nothing
        (Frontend.sessionTypeNames session)
        Map.empty
        (haskellSrcExtsParseMode sourceName)
        source
  -- The HSE compatibility frontend predates structured diagnostic codes.
  -- Seal every failure at this boundary while preserving its exact message,
  -- source, and span.
  (backendType, sourceVariables) <- first
    (withCode "DJEX_EXF_PARSE") parsed
  sharedType <- either
    (Left . failureDiagnostic
      "DJEX_EXF_PARSE"
      "cannot project the parsed Exference type"
    )
    Right
    $ toSynthesisType backendType
  let query = QueryRequest
        { requestTarget = checkedTarget
        , requestGoal = sharedType
        , requestContexts = []
        , requestOptions = options
        }
      sourceVariables' :: Map.Map String ExferenceLocal
      sourceVariables' = sourceVariables
      sourceLocation :: Maybe (FilePath, SourceSpan)
      sourceLocation = Just (sourceName, sourceTextSpan source)
  Frontend.mkExferenceRequestWithSourceInfo
    sourceVariables' sourceLocation query

failureDiagnostic :: Show detail => String -> String -> detail -> Diagnostic
failureDiagnostic code message detail =
  contextualDiagnostic Error code message (show detail)
