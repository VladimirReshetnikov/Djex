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
  , loadLegacyExferenceSessionFromSourcesWithPolicy
  , parseExferenceRequest
  , parseExferenceRequestInScope
  , parseExferenceRequestWithCheckedTarget
  , parseExferenceRequestWithCheckedTargetInScope
  ) where

import Control.Monad.Trans.Except (runExceptT)
import Data.Bifunctor (first)
import Data.Functor.Identity (runIdentity)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Language.Haskell.Exts.Parser as HSE
import qualified Language.Haskell.Exts.SrcLoc as HSEL
import qualified Language.Haskell.Exts.Syntax as HSES

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
import Language.Haskell.Exference.Core.Types (toSynthesisType)
import Language.Haskell.Exference.EnvironmentParser
  ( CheckedSourceEnvironment
  , EnvironmentLoadError
  , LoadReport (..)
  , checkedSourcePreparedInventory
  , environmentFromFiles
  , environmentFromPath
  , environmentFromSources
  , environmentFromLegacySources
  , environmentLoadErrorDiagnostics
  , haskellSrcExtsParseMode
  )
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc
  ( parseTypeWithInventory
  , parseTypeWithInventoryInScope
  , parseTypeWithInventoryInQualifiedScope
  )
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , shownErrorDiagnostic
  , sourceTextLocation
  , withCode
  , withSourceLocation
  )
import Language.Haskell.Synthesis.Generated (DefinitionName)
import Language.Haskell.Synthesis.Name
  ( ModuleName
  , Name
  , parseName
  , renderModuleName
  )
import Language.Haskell.Synthesis.Query (QueryRequest (..))
import Paths_djex (getDataFileName)

-- | A fully sealed session or structured fatal diagnostics, paired with all
-- non-fatal source-loader and backend-projection diagnostics in production
-- order. No parser-specific environment or error type crosses this boundary.
data ExferenceSessionLoadReport = ExferenceSessionLoadReport
  { exferenceSessionLoadResult
      :: Either (NonEmpty Diagnostic) ExferenceSession
  , exferenceSessionLoadDiagnostics :: [Diagnostic]
  }

-- | Name-resolution state supplied by an interactive source workspace.
-- Exact visible names control only unqualified lookup; every name in the
-- sealed inventory remains available through its canonical qualifier.
data ExferenceQueryScope = ExferenceQueryScope
  { exferenceQueryCurrentModule :: Maybe ModuleName
    -- ^ Optional full-top-level module whose local names take precedence.
  , exferenceQueryVisibleNames :: [Name]
    -- ^ Exact canonical names admitted for unqualified query syntax.
  , exferenceQueryModuleAliases :: [(ModuleName, ModuleName)]
    -- ^ Prompt qualifier paired with its canonical loaded module.
  , exferenceQueryQualifiedNames :: [(ModuleName, [Name])]
    -- ^ Exact canonical names admitted through each written qualifier. An
    -- empty outer list retains the permissive behavior of the original scoped
    -- API; a present qualifier with an empty inner list admits no names.
  }
  deriving (Eq, Show)

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
-- compatibility file loaders.
loadExferenceSessionFromSourcesWithPolicy
  :: ExferenceSessionPolicy
  -> [(FilePath, String)]
  -> [(FilePath, String)]
  -> IO ExferenceSessionLoadReport
loadExferenceSessionFromSourcesWithPolicy policy moduleSources ratingSources = do
  LoadReport sourceResult sourceDiagnostics <-
    environmentFromSources moduleSources ratingSources
  pure $ sealSourceLoadReport policy sourceResult sourceDiagnostics

-- | Internal compatibility route used when the unified REPL combines the
-- bundled import-less environment corpus with ordinary workspace snapshots.
-- Public exact-snapshot loading above remains strictly import-aware.
loadLegacyExferenceSessionFromSourcesWithPolicy
  :: ExferenceSessionPolicy
  -> [(FilePath, String)]
  -> [(FilePath, String)]
  -> IO ExferenceSessionLoadReport
loadLegacyExferenceSessionFromSourcesWithPolicy
    policy moduleSources ratingSources = do
  LoadReport sourceResult sourceDiagnostics <-
    environmentFromLegacySources moduleSources ratingSources
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
  let mode = haskellSrcExtsParseMode sourceName
      location = sourceTextLocation (HSE.parseFilename mode) source
      inventory = exferenceSessionInventory session
      parsed = runIdentity $ runExceptT $ case maybeScope of
        Nothing -> parseTypeWithInventory inventory Nothing mode source
        Just scope
          | null $ exferenceQueryQualifiedNames scope ->
              parseTypeWithInventoryInScope
                inventory
                (toHseModuleName <$> exferenceQueryCurrentModule scope)
                (exferenceQueryVisibleNames scope)
                (exferenceQueryModuleAliases scope)
                mode
                source
          | otherwise -> parseTypeWithInventoryInQualifiedScope
              inventory
              (toHseModuleName <$> exferenceQueryCurrentModule scope)
              (exferenceQueryVisibleNames scope)
              (exferenceQueryModuleAliases scope)
              (exferenceQueryQualifiedNames scope)
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
  Request.mkExferenceRequestWithSourceInfo
    sourceVariables location query
 where
  toHseModuleName moduleName = HSES.ModuleName HSEL.noSrcSpan
    $ renderModuleName moduleName
