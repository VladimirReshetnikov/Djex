-- | Haskell-src-exts loading and query parsing for the Exference backend.
--
-- The stable "Language.Haskell.Djex.Exference" module is deliberately
-- parser-neutral. Applications that consume Haskell source import this
-- frontend explicitly, keeping parser and filesystem dependencies at the
-- command boundary.
module Language.Haskell.Djex.Exference.HaskellSrc
  ( ExferenceSessionLoadReport (..)
  , ExferenceQueryScope (..)
  , exferenceCommandSessionPolicy
  , defaultExferenceEnvironmentPath
  , loadDefaultExferenceSession
  , loadDefaultExferenceSessionWithPolicy
  , loadExferenceSession
  , loadExferenceSessionWithPolicy
  , loadExferenceSessionFromFiles
  , loadExferenceSessionFromFilesWithPolicy
  , loadExferenceSessionFromSources
  , loadExferenceSessionFromSourcesWithPolicy
  , parseExferenceRequest
  , parseExferenceRequestInScope
  , parseExferenceRequestWithCheckedTarget
  , parseExferenceRequestWithCheckedTargetInScope
  , mkExferenceRequestWithCheckedTargetFromParsed
  ) where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

import Language.Haskell.Djex.Exference
  ( ExferenceOptions
  , ExferenceRequest
  , ExferenceSession
  , ExferenceSessionPolicy (..)
  , defaultExferenceSessionPolicy
  , exferenceSessionDiagnostics
  , exferenceSessionInventory
  )
import qualified Language.Haskell.Djex.Exference.Internal.Request as Request
import qualified Language.Haskell.Djex.Exference.Internal.Session as Session
import Language.Haskell.Exference.EnvironmentParser
  ( CheckedSourceEnvironment
  , EnvironmentLoadError
  , LoadReport (..)
  , checkedSourcePreparedInventory
  , environmentFromFiles
  , environmentFromPath
  , environmentFromSources
  , environmentLoadErrorDiagnostics
  )
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , shownErrorDiagnostic
  , withCode
  )
import Language.Haskell.Synthesis.Generated (DefinitionName)
import Language.Haskell.Synthesis.Name
  ( Name
  , parseName
  )
import Language.Haskell.Synthesis.Query (QueryRequest (..))
import Language.Haskell.Djex.HaskellSrc
  ( ExferenceQueryScope (..)
  , ParsedSourceType
  , parseSourceType
  , parseSourceTypeInScope
  , parsedSourceType
  , parsedSourceTypeLocation
  , parsedSourceTypeVariableNames
  )
import Paths_djex (getDataFileName)

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

-- | Locate the source environment installed as Djex package data.
--
-- Cabal's generated @Paths_djex@ module is private to this library. Keeping
-- that detail behind the checked source facade gives downstream applications
-- a supported path lookup without making them depend on generated internals
-- or on the layout of a source checkout.
defaultExferenceEnvironmentPath :: IO FilePath
defaultExferenceEnvironmentPath =
  getDataFileName "exference/environment"

-- | Load Djex's installed Exference environment with the unrestricted
-- programmatic session policy.
loadDefaultExferenceSession :: IO ExferenceSessionLoadReport
loadDefaultExferenceSession =
  defaultExferenceEnvironmentPath >>= loadExferenceSession

-- | Policy-aware counterpart of 'loadDefaultExferenceSession'.
loadDefaultExferenceSessionWithPolicy
  :: ExferenceSessionPolicy
  -> IO ExferenceSessionLoadReport
loadDefaultExferenceSessionWithPolicy policy =
  defaultExferenceEnvironmentPath >>= loadExferenceSessionWithPolicy policy

-- | Load a directory of source modules, ratings, and an optional path-local
-- constructorless-type visibility manifest; validate its complete inventory;
-- and seal an Exference session with the default policy.
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
  pure $ sealSourceLoadReport policy sourceResult sourceDiagnostics

-- | Load explicitly ordered Haskell module and rating files, validate their
-- complete inventory, and seal an Exference session with the default policy.
-- An empty module list is valid and retains Exference's built-in constructor
-- inventory. Dependency discovery remains the caller's responsibility.
loadExferenceSessionFromFiles
  :: [FilePath]
  -> [FilePath]
  -> IO ExferenceSessionLoadReport
loadExferenceSessionFromFiles = loadExferenceSessionFromFilesWithPolicy
  defaultExferenceSessionPolicy

-- | Policy-aware explicit-file counterpart of 'loadExferenceSessionWithPolicy'.
-- Both path collections are consumed in caller order and use the same
-- read/parse/rating/check/seal pipeline as directory loading.
loadExferenceSessionFromFilesWithPolicy
  :: ExferenceSessionPolicy
  -> [FilePath]
  -> [FilePath]
  -> IO ExferenceSessionLoadReport
loadExferenceSessionFromFilesWithPolicy policy modulePaths ratingPaths = do
  LoadReport sourceResult sourceDiagnostics <-
    environmentFromFiles modulePaths ratingPaths
  pure $ sealSourceLoadReport policy sourceResult sourceDiagnostics

-- | Seal an Exference session from exact, explicitly ordered module and
-- rating snapshots. Paths remain attached to parser and rating diagnostics;
-- the filesystem is not consulted by this entry point.
loadExferenceSessionFromSources
  :: [(FilePath, String)]
  -> [(FilePath, String)]
  -> IO ExferenceSessionLoadReport
loadExferenceSessionFromSources = loadExferenceSessionFromSourcesWithPolicy
  defaultExferenceSessionPolicy

-- | Policy-aware counterpart of 'loadExferenceSessionFromSources'. The
-- in-memory inputs enter the same parse/rate/check/seal pipeline used by the
-- file loaders.
loadExferenceSessionFromSourcesWithPolicy
  :: ExferenceSessionPolicy
  -> [(FilePath, String)]
  -> [(FilePath, String)]
  -> IO ExferenceSessionLoadReport
loadExferenceSessionFromSourcesWithPolicy policy moduleSources ratingSources = do
  LoadReport sourceResult sourceDiagnostics <-
    environmentFromSources moduleSources ratingSources
  pure $ sealSourceLoadReport policy sourceResult sourceDiagnostics

sealSourceLoadReport
  :: ExferenceSessionPolicy
  -> Either EnvironmentLoadError CheckedSourceEnvironment
  -> [Diagnostic]
  -> ExferenceSessionLoadReport
sealSourceLoadReport policy sourceResult sourceDiagnostics =
  case sourceResult of
    Left failure -> ExferenceSessionLoadReport
      { exferenceSessionLoadResult = Left
          $ environmentLoadErrorDiagnostics failure
      , exferenceSessionLoadDiagnostics = sourceDiagnostics
      }
    Right checked -> case
        Session.sealPreparedExferenceSessionWithPolicy
          policy
          (checkedSourcePreparedInventory checked) of
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
  checkedTarget <- Request.validateExferenceTarget target
  parseExferenceRequestWithCheckedTarget
    session options checkedTarget sourceName source

-- | Scoped counterpart of 'parseExferenceRequest'. This is the source-level
-- boundary used by the shared REPL after applying @import@ and @:module@.
parseExferenceRequestInScope
  :: ExferenceSession
  -> ExferenceOptions
  -> Name
  -> ExferenceQueryScope
  -> FilePath
  -> String
  -> Either Diagnostic ExferenceRequest
parseExferenceRequestInScope session options target scope sourceName source = do
  checkedTarget <- Request.validateExferenceTarget target
  parseExferenceRequestWithCheckedTargetInScope
    session options checkedTarget scope sourceName source

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
    sourceName source = parseCheckedRequest
      session options checkedTarget Nothing sourceName source

-- | Scoped counterpart of 'parseExferenceRequestWithCheckedTarget'.
parseExferenceRequestWithCheckedTargetInScope
  :: ExferenceSession
  -> ExferenceOptions
  -> DefinitionName
  -> ExferenceQueryScope
  -> FilePath
  -> String
  -> Either Diagnostic ExferenceRequest
parseExferenceRequestWithCheckedTargetInScope session options checkedTarget
    scope sourceName source = parseCheckedRequest
      session options checkedTarget (Just scope) sourceName source

parseCheckedRequest
  :: ExferenceSession
  -> ExferenceOptions
  -> DefinitionName
  -> Maybe ExferenceQueryScope
  -> FilePath
  -> String
  -> Either Diagnostic ExferenceRequest
parseCheckedRequest session options checkedTarget maybeScope sourceName source = do
  parsed <- first (withCode "DJEX_EXF_PARSE") $ case maybeScope of
    Nothing -> parseSourceType inventory sourceName source
    Just scope -> parseSourceTypeInScope inventory scope sourceName source
  mkExferenceRequestWithCheckedTargetFromParsed
    options checkedTarget parsed
 where
  inventory = exferenceSessionInventory session

-- | Seal an Exference request from a source type parsed once by Djex.
-- Other backends can lower the same 'ParsedSourceType' before this function
-- attaches Exference's search options and source-name hints.
mkExferenceRequestWithCheckedTargetFromParsed
  :: ExferenceOptions
  -> DefinitionName
  -> ParsedSourceType
  -> Either Diagnostic ExferenceRequest
mkExferenceRequestWithCheckedTargetFromParsed options checkedTarget parsed = do
  let query = QueryRequest
        { requestTarget = checkedTarget
        , requestGoal = parsedSourceType parsed
        , requestContexts = []
        , requestOptions = options
        }
  Request.mkExferenceRequestWithSourceInfo
    (parsedSourceTypeVariableNames parsed)
    (parsedSourceTypeLocation parsed)
    query
