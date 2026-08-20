{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Filesystem-backed source targets for the shared REPL.
--
-- This module deliberately stops at source discovery.  It owns the GHCi-like
-- distinction between explicit targets and their local dependency closure,
-- while the Exference source frontend remains the authority that validates and
-- seals the declarations in those files.  Every admitted path is canonical,
-- so reloading a workspace is independent of later @:cd@ commands.
module Language.Haskell.Djex.REPL.Workspace
  ( SourceWorkspace
  , WorkspaceTarget
  , WorkspaceModule
  , loadWorkspace
  , reloadWorkspace
  , addWorkspaceTargets
  , removeWorkspaceTargets
  , workspaceTargets
  , workspaceModules
  , workspaceModuleSources
  , workspaceRatingSources
  , workspaceTypeVisibilitySources
  , workspaceUnresolvedImportsWithSources
  , workspaceModuleName
  , workspaceModulePath
  , workspaceModuleSyntax
  , workspaceTargetDisplay
  , workspaceTargetModuleFiles
  , workspaceAutomaticTargetModules
  ) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Either (fromRight, partitionEithers)
import qualified Data.Foldable as Foldable
import Data.List (intercalate, sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Semigroup (sconcat)
import Data.Sequence (Seq, ViewL (..), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import System.Directory
  ( canonicalizePath
  , doesDirectoryExist
  , doesFileExist
  , doesPathExist
  , getCurrentDirectory
  , listDirectory
  , makeAbsolute
  , pathIsSymbolicLink
  )
import System.FilePath
  ( (<.>)
  , (</>)
  , dropExtension
  , isAbsolute
  , joinPath
  , normalise
  , takeDirectory
  , takeExtension
  , takeFileName
  )
import System.IO.Error (tryIOError)

import qualified Language.Haskell.Exts as HSEFile
import qualified Language.Haskell.Exts.Parser as HSE
import qualified Language.Haskell.Exts.SrcLoc as HSE
import qualified Language.Haskell.Exts.Syntax as HSE

import Language.Haskell.Exference.HaskellSrcUtils
  ( withHaskellSrcLocation )
import Language.Haskell.Exference.TypeFromHaskellSrc
  ( haskellSrcExtsParseMode )
import Language.Haskell.Djex.Internal.DependencyGraph
  ( DependencyCycle (..)
  , stableDependencyOrder
  )
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error)
  , contextualDiagnostic
  , withSource
  )
import qualified Language.Haskell.Synthesis.Collection as SharedCollection
import qualified Language.Haskell.Synthesis.Name as SharedName

-- | An immutable snapshot of explicit source targets and their local import
-- closure.  Accessors preserve target admission order and dependency order.
data SourceWorkspace = SourceWorkspace
  { sourceWorkspaceTargets :: [WorkspaceTarget]
  , sourceWorkspaceModules :: [WorkspaceModule]
  , sourceWorkspaceRatings :: [(FilePath, String)]
  , sourceWorkspaceTypeVisibilities :: [(FilePath, String)]
  , sourceWorkspaceAutomaticTarget :: Maybe TargetKey
  }

-- | One explicit @:load@ or @:add@ target.  A directory is one target even
-- though the Djex directory extension expands it to several source files.
data WorkspaceTarget = WorkspaceTarget
  { targetLocator :: TargetLocator
  , targetStarred :: Bool
  , targetModuleExpectations :: [String]
  , targetModuleSpellings :: [String]
  , targetSourceFiles :: [FilePath]
  , targetRatings :: [FilePath]
  , targetTypeVisibilities :: [FilePath]
  }

-- | A parsed ordinary Haskell module.  Dependency metadata stays private so
-- callers cannot make the checked ordering disagree with the syntax tree.
data WorkspaceModule = WorkspaceModule
  { parsedModuleName :: String
  , parsedModulePath :: FilePath
  , parsedModuleSource :: String
  , parsedModuleSyntax :: HSE.Module HSE.SrcSpanInfo
  , parsedModuleImports :: [WorkspaceImport]
  }

data WorkspaceImport = WorkspaceImport
  { importedModuleName :: String
  , importedFromPackage :: Bool
  , importedAsSource :: Bool
  }

data TargetKind
  = SourceFileTarget
  | ModuleNameTarget
  | DirectoryTarget
  deriving (Eq, Ord, Show)

type TargetKey = (TargetKind, FilePath)

-- All locator fields are total.  A module spelling is present only for a
-- target admitted through its hierarchical module name; the admission root is
-- retained for dependency lookup after the process working directory changes.
data TargetLocator = TargetLocator
  { locatorKind :: TargetKind
  , locatorPath :: FilePath
  , locatorModuleName :: Maybe String
  , locatorAdmissionRoot :: FilePath
  }

-- | Resolve a fresh target list relative to the current directory.
loadWorkspace
  :: [String]
  -> IO (Either (NonEmpty Diagnostic) SourceWorkspace)
loadWorkspace arguments = do
  admitted <- admitTargets arguments
  case admitted of
    Left failures -> pure $ Left failures
    Right targets -> buildWorkspace (map targetKey targets) targets

-- | Re-read the canonical targets retained by a previous snapshot.
reloadWorkspace
  :: SourceWorkspace
  -> IO (Either (NonEmpty Diagnostic) SourceWorkspace)
reloadWorkspace workspace = buildWorkspace preferences targets
 where
  targets = sourceWorkspaceTargets workspace
  preferences = retainedAutomaticPreference workspace
    ++ reverse (map targetKey targets)

-- | Resolve and atomically add targets.  Re-adding the same canonical file or
-- directory is idempotent; a later starred spelling upgrades the retained
-- target without changing its position.
addWorkspaceTargets
  :: SourceWorkspace
  -> [String]
  -> IO (Either (NonEmpty Diagnostic) SourceWorkspace)
addWorkspaceTargets workspace [] = pure $ Right workspace
addWorkspaceTargets workspace arguments = do
  admitted <- admitTargets arguments
  case admitted of
    Left failures -> pure $ Left failures
    Right added -> do
      let targets = mergeTargets (sourceWorkspaceTargets workspace) added
          preferences = reverse (map targetKey added)
            ++ retainedAutomaticPreference workspace
            ++ reverse (map targetKey targets)
      buildWorkspace preferences targets

-- | Atomically remove explicit targets.  Arguments match a retained canonical
-- target path or an admitted single-module spelling; dependency-only modules
-- are intentionally not targets and therefore cannot be unadded directly.
removeWorkspaceTargets
  :: SourceWorkspace
  -> [String]
  -> IO (Either (NonEmpty Diagnostic) SourceWorkspace)
removeWorkspaceTargets workspace [] = pure $ Right workspace
removeWorkspaceTargets workspace arguments = do
  resolved <- traverse (removalMatches $ sourceWorkspaceTargets workspace)
    arguments
  case collectResults resolved of
    Left failures -> pure $ Left failures
    Right keys -> do
      let removed = Set.unions keys
          targets =
            [ target
            | target <- sourceWorkspaceTargets workspace
            , targetKey target `Set.notMember` removed
            ]
          preferences = retainedAutomaticPreference workspace
            ++ reverse (map targetKey targets)
      buildWorkspace preferences targets

retainedAutomaticPreference :: SourceWorkspace -> [TargetKey]
retainedAutomaticPreference workspace = case
    sourceWorkspaceAutomaticTarget workspace of
  Just key -> [key]
  Nothing -> []

-- | Explicit targets in admission order.
workspaceTargets :: SourceWorkspace -> [WorkspaceTarget]
workspaceTargets = sourceWorkspaceTargets

-- | Loaded source modules in deterministic dependency-first order.
workspaceModules :: SourceWorkspace -> [WorkspaceModule]
workspaceModules = sourceWorkspaceModules

-- | The exact, fully evaluated source texts parsed while constructing this
-- workspace, in dependency-first session-loader order.
workspaceModuleSources :: SourceWorkspace -> [(FilePath, String)]
workspaceModuleSources = map snapshot . workspaceModules
 where
  snapshot modul = (parsedModulePath modul, parsedModuleSource modul)

-- | Exact, fully evaluated rating texts read with the workspace snapshot, in
-- deterministic target/directory order.
workspaceRatingSources :: SourceWorkspace -> [(FilePath, String)]
workspaceRatingSources = sourceWorkspaceRatings

-- | Exact, fully evaluated constructorless-type visibility sidecars read with
-- the workspace snapshot, in deterministic target/directory order. Only
-- directory targets discover sidecars; loading a source file or module name
-- never gives an adjacent manifest implicit authority.
workspaceTypeVisibilitySources :: SourceWorkspace -> [(FilePath, String)]
workspaceTypeVisibilitySources = sourceWorkspaceTypeVisibilities

-- | Non-package imports for which dependency discovery found no local source
-- module. Path/importer/imported triples retain dependency-first module order
-- and source import order, making the source-workspace boundary and diagnostic
-- provenance visible without exposing private graph metadata.
workspaceUnresolvedImportsWithSources
  :: SourceWorkspace
  -> [(FilePath, String, String)]
workspaceUnresolvedImportsWithSources workspace =
  [ ( parsedModulePath modul
    , parsedModuleName modul
    , importedModuleName imported
    )
  | modul <- workspaceModules workspace
  , imported <- parsedModuleImports modul
  , not $ importedFromPackage imported
  , importedModuleName imported `Set.notMember` loadedNames
  ]
 where
  loadedNames = Set.fromList
    $ map parsedModuleName $ workspaceModules workspace

-- | The module name declared by the module header, or @Main@ when the file
-- has none.
workspaceModuleName :: WorkspaceModule -> String
workspaceModuleName = parsedModuleName

-- | The canonical path of the source file the module was parsed from.
workspaceModulePath :: WorkspaceModule -> FilePath
workspaceModulePath = parsedModulePath

-- | The parsed HSE syntax tree of the module.
workspaceModuleSyntax
  :: WorkspaceModule
  -> HSE.Module HSE.SrcSpanInfo
workspaceModuleSyntax = parsedModuleSyntax

-- | Stable target spelling for @:show targets@.  Files and directories use
-- their canonical path; named targets retain their module spelling.
workspaceTargetDisplay :: WorkspaceTarget -> String
workspaceTargetDisplay target = starPrefix ++ base
 where
  starPrefix = if targetStarred target then "*" else ""
  locator = targetLocator target
  base = case locatorModuleName locator of
    Just name | locatorKind locator == ModuleNameTarget -> name
    _ -> locatorPath locator

-- | The canonical source files supplied directly by a target as of the
-- last workspace snapshot: exactly one for a file or named-module target,
-- and every discovered source file, in deterministic order, for a
-- directory target. Modules pulled in only as dependencies are excluded.
workspaceTargetModuleFiles :: WorkspaceTarget -> [FilePath]
workspaceTargetModuleFiles = targetSourceFiles

-- | Source modules supplied by the history-selected automatic target. A file
-- or named-module target contributes one entry; Djex's directory extension
-- contributes every source file in deterministic target order so the default
-- environment remains fully searchable. The flags are always 'True': Djex
-- source-interprets every loaded module, giving each entry GHCi's @*M@
-- semantics regardless of the target's written star.
workspaceAutomaticTargetModules
  :: SourceWorkspace
  -> [(WorkspaceModule, Bool)]
workspaceAutomaticTargetModules workspace = case
    sourceWorkspaceAutomaticTarget workspace of
  Nothing -> []
  Just key -> case filter ((== key) . targetKey)
      $ sourceWorkspaceTargets workspace of
    target : _ -> targetContribution
      (modulesByPath $ sourceWorkspaceModules workspace) target
    [] -> []

admitTargets
  :: [String]
  -> IO (Either (NonEmpty Diagnostic) [WorkspaceTarget])
admitTargets [] = pure $ Right []
admitTargets arguments = do
  rootResult <- canonicalCurrentDirectory
  case rootResult of
    Left failure -> pure $ Left $ failure :| []
    Right root -> do
      results <- mapM (admitTarget root) arguments
      pure $ deduplicateTargets <$> collectResults results

canonicalCurrentDirectory :: IO (Either Diagnostic FilePath)
canonicalCurrentDirectory = do
  result <- tryIOError $ getCurrentDirectory >>= canonicalizePath
  pure $ case result of
    Left failure -> Left $ workspaceFailure
      "DJEX_REPL_TARGET_IO" "cannot resolve the current source root"
      $ show failure
    Right path -> Right path

admitTarget
  :: FilePath
  -> String
  -> IO (Either (NonEmpty Diagnostic) WorkspaceTarget)
admitTarget root raw = case splitStar raw of
  Left failure -> pure $ Left $ failure :| []
  Right (starred, source) -> do
    let candidate = if isAbsolute source then source else root </> source
    inspected <- tryIOError $ do
      exists <- doesPathExist candidate
      isFile <- doesFileExist candidate
      isDirectory <- doesDirectoryExist candidate
      pure (exists, isFile, isDirectory)
    case inspected of
      Left failure -> pure $ singletonFailure
        "DJEX_REPL_TARGET_IO" "cannot inspect source target"
        source (show failure)
      Right (_, True, _) -> admitFile root starred candidate
      Right (_, _, True) -> admitDirectory starred candidate
      Right (True, _, _) -> pure $ singletonFailure
        "DJEX_REPL_TARGET_KIND" "unsupported source target"
        source "expected a regular .hs/.lhs file or a directory"
      Right (False, _, _)
        | looksLikePath source -> pure $ singletonFailure
            "DJEX_REPL_TARGET_NOT_FOUND" "source target does not exist"
            source "expected an existing .hs/.lhs file or directory"
        | otherwise -> admitNamedModule root starred source

splitStar :: String -> Either Diagnostic (Bool, String)
splitStar ('*' : source)
  | null source = Left $ workspaceFailure
      "DJEX_REPL_TARGET" "invalid source target" "'*' requires a target"
  | otherwise = Right (True, source)
splitStar "" = Left $ workspaceFailure
  "DJEX_REPL_TARGET" "invalid source target" "target is empty"
splitStar source = Right (False, source)

looksLikePath :: String -> Bool
looksLikePath source =
  takeExtension source `elem` [".hs", ".lhs"]
    || isAbsolute source
    || any (`elem` ("/\\" :: String)) source
    || case source of
      '.' : _ -> True
      _ -> False

admitFile
  :: FilePath
  -> Bool
  -> FilePath
  -> IO (Either (NonEmpty Diagnostic) WorkspaceTarget)
admitFile root starred source
  | not $ isSourceFile source = pure $ singletonFailure
      "DJEX_REPL_TARGET_KIND" "unsupported source file"
      source "expected a .hs or .lhs source file"
  | otherwise = do
      canonical <- canonicalPath "cannot resolve source file" source
      pure $ fmap (fileTarget root starred) canonical

fileTarget :: FilePath -> Bool -> FilePath -> WorkspaceTarget
fileTarget root starred path = WorkspaceTarget
  { targetLocator = TargetLocator
      { locatorKind = SourceFileTarget
      , locatorPath = path
      , locatorModuleName = Nothing
      , locatorAdmissionRoot = root
      }
  , targetStarred = starred
  , targetModuleExpectations = []
  , targetModuleSpellings = []
  , targetSourceFiles = [path]
  , targetRatings = []
  , targetTypeVisibilities = []
  }

admitDirectory
  :: Bool
  -> FilePath
  -> IO (Either (NonEmpty Diagnostic) WorkspaceTarget)
admitDirectory starred source = do
  canonical <- canonicalPath "cannot resolve source directory" source
  -- 'buildWorkspace' refreshes every admitted target immediately. Deferring
  -- traversal until that common pass prevents a new directory from being
  -- walked twice before its first immutable snapshot is returned.
  pure $ fmap (\path -> directoryTarget starred path ([], [], [])) canonical

directoryTarget
  :: Bool
  -> FilePath
  -> ([FilePath], [FilePath], [FilePath])
  -> WorkspaceTarget
directoryTarget starred path (sources, ratings, visibilities) = WorkspaceTarget
  { targetLocator = TargetLocator
      { locatorKind = DirectoryTarget
      , locatorPath = path
      , locatorModuleName = Nothing
      , locatorAdmissionRoot = path
      }
  , targetStarred = starred
  , targetModuleExpectations = []
  , targetModuleSpellings = []
  , targetSourceFiles = sources
  , targetRatings = ratings
  , targetTypeVisibilities = visibilities
  }

admitNamedModule
  :: FilePath
  -> Bool
  -> String
  -> IO (Either (NonEmpty Diagnostic) WorkspaceTarget)
admitNamedModule root starred source = case checkedModuleName source of
  Left failure -> pure $ Left $ failure :| []
  Right segments -> do
    resolved <- resolveModuleFile [root] segments
    case resolved of
      Left failures -> pure $ Left failures
      Right Nothing -> pure $ singletonFailure
        "DJEX_REPL_TARGET_NOT_FOUND" "cannot find source module"
        source $ "searched from " ++ root
      Right (Just path) -> pure $ Right WorkspaceTarget
        { targetLocator = TargetLocator
            { locatorKind = ModuleNameTarget
            , locatorPath = path
            , locatorModuleName = Just source
            , locatorAdmissionRoot = root
            }
        , targetStarred = starred
        , targetModuleExpectations = [source]
        , targetModuleSpellings = [source]
        , targetSourceFiles = [path]
        , targetRatings = []
        , targetTypeVisibilities = []
        }

checkedModuleName :: String -> Either Diagnostic [String]
checkedModuleName source = case SharedName.mkModuleName source of
  Left failure -> Left $ workspaceFailure
    "DJEX_REPL_MODULE_NAME" "invalid Haskell module name"
    $ source ++ ": " ++ SharedName.renderNameError failure
  Right name -> Right $ SharedName.moduleNameSegments name

canonicalPath
  :: String
  -> FilePath
  -> IO (Either (NonEmpty Diagnostic) FilePath)
canonicalPath summary path = do
  result <- tryIOError $ canonicalizePath path
  pure $ case result of
    Left failure -> singletonFailure
      "DJEX_REPL_TARGET_IO" summary path $ show failure
    Right canonical -> Right canonical

directoryContents
  :: FilePath
  -> IO
      (Either
        (NonEmpty Diagnostic)
        ([FilePath], [FilePath], [FilePath]))
directoryContents root = do
  result <- go Set.empty root
  pure $ fmap finish result
 where
  finish (_, sources, ratings, visibilities) =
    ( stableNub sources
    , stableNub ratings
    , stableNub visibilities
    )

  go visited directory
    | directory `Set.member` visited = pure $ Right (visited, [], [], [])
    | otherwise = do
        listed <- tryIOError $ listDirectory directory
        case listed of
          Left failure -> pure $ singletonFailure
            "DJEX_REPL_TARGET_IO" "cannot read source directory"
            directory $ show failure
          Right entries -> walk (Set.insert directory visited)
            [] [] [] $ map (directory </>) $ sort entries

  walk visited sources ratings visibilities [] = pure $ Right
    (visited, sources, ratings, visibilities)
  walk visited sources ratings visibilities (path : remaining) = do
    inspected <- tryIOError $ do
      symbolic <- pathIsSymbolicLink path
      isDirectory <- doesDirectoryExist path
      isFile <- doesFileExist path
      pure (symbolic, isDirectory, isFile)
    case inspected of
      Left failure -> pure $ singletonFailure
        "DJEX_REPL_TARGET_IO" "cannot inspect source-directory entry"
        path $ show failure
      -- Directory links are never part of an admitted tree. Besides keeping
      -- traversal bounded by the canonical root, this makes cycles harmless.
      -- Links to regular source/rating/visibility files remain valid explicit
      -- contents.
      Right (True, True, _) ->
        walk visited sources ratings visibilities remaining
      Right (_, True, _) -> do
        canonical <- canonicalPath "cannot resolve source subdirectory" path
        case canonical of
          Left failures -> pure $ Left failures
          Right directory -> do
            nested <- go visited directory
            case nested of
              Left failures -> pure $ Left failures
              Right (afterNested, nestedSources, nestedRatings,
                  nestedVisibilities) -> walk
                afterNested
                (sources ++ nestedSources)
                (ratings ++ nestedRatings)
                (visibilities ++ nestedVisibilities)
                remaining
      Right (_, _, True) -> do
        canonical <- canonicalPath "cannot resolve source-directory file" path
        case canonical of
          Left failures -> pure $ Left failures
          Right file
            | isSourceFile file -> walk visited
                (sources ++ [file]) ratings visibilities remaining
            | isRatingFile file -> walk visited sources
                (ratings ++ [file]) visibilities remaining
            | isTypeVisibilityFile file -> walk visited sources ratings
                (visibilities ++ [file]) remaining
            | otherwise -> walk visited sources ratings visibilities remaining
      Right _ -> walk visited sources ratings visibilities remaining

isSourceFile :: FilePath -> Bool
isSourceFile path = takeExtension path `elem` [".hs", ".lhs"]

isRatingFile :: FilePath -> Bool
isRatingFile path = takeExtension path == ".ratings"

isTypeVisibilityFile :: FilePath -> Bool
isTypeVisibilityFile path = takeExtension path == ".visibility"

refreshTarget
  :: WorkspaceTarget
  -> IO (Either (NonEmpty Diagnostic) WorkspaceTarget)
refreshTarget target = case locatorKind locator of
  DirectoryTarget -> do
    exists <- tryIOError $ doesDirectoryExist $ locatorPath locator
    case exists of
      Left failure -> pure $ singletonFailure
        "DJEX_REPL_TARGET_IO" "cannot inspect source directory"
        (locatorPath locator) $ show failure
      Right False -> pure $ singletonFailure
        "DJEX_REPL_TARGET_NOT_FOUND" "source directory no longer exists"
        (locatorPath locator) "reload retains canonical target paths"
      Right True -> do
        contents <- directoryContents $ locatorPath locator
        pure $ fmap (updatedDirectory target) contents
  _ -> do
    exists <- tryIOError $ doesFileExist $ locatorPath locator
    pure $ case exists of
      Left failure -> singletonFailure
        "DJEX_REPL_TARGET_IO" "cannot inspect source file"
        (locatorPath locator) $ show failure
      Right False -> singletonFailure
        "DJEX_REPL_TARGET_NOT_FOUND" "source file no longer exists"
        (locatorPath locator) "reload retains canonical target paths"
      Right True -> Right target
        { targetSourceFiles = [locatorPath locator]
        , targetRatings = []
        , targetTypeVisibilities = []
        }
 where
  locator = targetLocator target
  updatedDirectory value (sources, ratings, visibilities) = value
    { targetSourceFiles = sources
    , targetRatings = ratings
    , targetTypeVisibilities = visibilities
    }

buildWorkspace
  :: [TargetKey]
  -> [WorkspaceTarget]
  -> IO (Either (NonEmpty Diagnostic) SourceWorkspace)
buildWorkspace automaticPreferences targets = do
  refreshedResults <- mapM refreshTarget targets
  case collectResults refreshedResults of
    Left failures -> pure $ Left failures
    Right refreshed -> do
      let sourcePaths = stableNub $ concatMap targetSourceFiles refreshed
      parsedResults <- mapM parseWorkspaceModule sourcePaths
      case collectResults parsedResults of
        Left failures -> pure $ Left failures
        Right explicitModules -> case
            targetModuleMismatchDiagnostics refreshed explicitModules
              <> duplicateModuleDiagnostics explicitModules of
          Just failures -> pure $ Left failures
          Nothing -> do
            discovered <- discoverDependencies refreshed explicitModules
            case discovered of
              Left failures -> pure $ Left failures
              Right modules -> finishWorkspace
                automaticPreferences refreshed sourcePaths modules

finishWorkspace
  :: [TargetKey]
  -> [WorkspaceTarget]
  -> [FilePath]
  -> [WorkspaceModule]
  -> IO (Either (NonEmpty Diagnostic) SourceWorkspace)
finishWorkspace automaticPreferences targets explicitPaths discovered = case
    dependencyOrder explicitPaths discovered of
  Left failures -> pure $ Left failures
  Right ordered -> do
    let annotatedTargets = annotateTargets targets discovered
        automatic = automaticTargetKey
          automaticPreferences annotatedTargets discovered
        ratingPaths = stableNub $ concatMap targetRatings annotatedTargets
        visibilityPaths = stableNub
          $ concatMap targetTypeVisibilities annotatedTargets
    ratingResults <- mapM readRatingSnapshot ratingPaths
    visibilityResults <- mapM readTypeVisibilitySnapshot visibilityPaths
    pure $ case (collectResults ratingResults,
        collectResults visibilityResults) of
      (Left failures, _) -> Left failures
      (_, Left failures) -> Left failures
      (Right ratings, Right visibilities) -> Right SourceWorkspace
        { sourceWorkspaceTargets = annotatedTargets
        , sourceWorkspaceModules = ordered
        , sourceWorkspaceRatings = ratings
        , sourceWorkspaceTypeVisibilities = visibilities
        , sourceWorkspaceAutomaticTarget = automatic
        }

parseWorkspaceModule
  :: FilePath
  -> IO (Either (NonEmpty Diagnostic) WorkspaceModule)
parseWorkspaceModule path = do
  loaded <- strictReadWorkspaceText path
  pure $ case loaded of
    Left failure -> singletonFailure
      "DJEX_REPL_MODULE_READ" "cannot read source module" path $ show failure
    -- The file-content parser reads LANGUAGE pragmas from this same retained
    -- text. Using the lower-level module parser here would silently drop
    -- per-file extensions such as PackageImports.
    Right source -> case HSEFile.parseFileContentsWithMode
        (haskellSrcExtsParseMode path) source of
      HSE.ParseFailed location detail -> Left
        $ withHaskellSrcLocation location
            (workspaceFailure "DJEX_REPL_MODULE_PARSE"
              "could not parse source module" detail)
        :| []
      HSE.ParseOk syntax -> workspaceModuleFromSyntax path source syntax

workspaceModuleFromSyntax
  :: FilePath
  -> String
  -> HSE.Module HSE.SrcSpanInfo
  -> Either (NonEmpty Diagnostic) WorkspaceModule
workspaceModuleFromSyntax path source syntax = case syntax of
  HSE.Module _ moduleHead _ imports _ -> do
    name <- declaredModuleName path moduleHead
    convertedImports <- traverse (workspaceImport path) imports
    pure WorkspaceModule
      { parsedModuleName = name
      , parsedModulePath = path
      , parsedModuleSource = source
      , parsedModuleSyntax = syntax
      , parsedModuleImports = convertedImports
      }
  _ -> singletonFailure
    "DJEX_REPL_MODULE_FORM" "unsupported source module form" path
    "expected an ordinary Haskell module"

readRatingSnapshot
  :: FilePath
  -> IO (Either (NonEmpty Diagnostic) (FilePath, String))
readRatingSnapshot = readAuxiliarySnapshot
  "DJEX_REPL_RATING_READ" "cannot read rating file"

readTypeVisibilitySnapshot
  :: FilePath
  -> IO (Either (NonEmpty Diagnostic) (FilePath, String))
readTypeVisibilitySnapshot = readAuxiliarySnapshot
  "DJEX_REPL_VISIBILITY_READ" "cannot read type visibility manifest"

readAuxiliarySnapshot
  :: String
  -> String
  -> FilePath
  -> IO (Either (NonEmpty Diagnostic) (FilePath, String))
readAuxiliarySnapshot code summary path = do
  loaded <- strictReadWorkspaceText path
  pure $ case loaded of
    Left failure -> singletonFailure
      code summary path $ show failure
    Right source -> Right (path, source)

-- Keep lazy String IO entirely inside the IOException boundary. The returned
-- text is therefore one immutable snapshot even if the file is edited before
-- the Exference inventory is sealed.
strictReadWorkspaceText :: FilePath -> IO (Either IOError String)
strictReadWorkspaceText path = tryIOError
  $ readFile path >>= evaluate . force

declaredModuleName
  :: FilePath
  -> Maybe (HSE.ModuleHead HSE.SrcSpanInfo)
  -> Either (NonEmpty Diagnostic) String
declaredModuleName _ Nothing = Right "Main"
declaredModuleName path (Just (HSE.ModuleHead _ (HSE.ModuleName _ source) _ _)) =
  case checkedModuleName source of
    Left failure -> Left $ withSource path failure :| []
    Right _ -> Right source

workspaceImport
  :: FilePath
  -> HSE.ImportDecl HSE.SrcSpanInfo
  -> Either (NonEmpty Diagnostic) WorkspaceImport
workspaceImport path declaration =
  let HSE.ModuleName _ source = HSE.importModule declaration
  in case checkedModuleName source of
    Left failure -> Left $ withSource path failure :| []
    Right _ -> Right WorkspaceImport
      { importedModuleName = source
      , importedFromPackage = case HSE.importPkg declaration of
          Nothing -> False
          Just _ -> True
      , importedAsSource = HSE.importSrc declaration
      }

duplicateModuleDiagnostics
  :: [WorkspaceModule]
  -> Maybe (NonEmpty Diagnostic)
-- Targets carry caller order. Keep the first declaration authoritative and
-- report every later occurrence at the point where it was encountered.
duplicateModuleDiagnostics modules = NonEmpty.nonEmpty
  [ withSource (parsedModulePath duplicate)
      $ workspaceFailure
          "DJEX_REPL_MODULE_DUPLICATE"
          "duplicate source module"
          ( parsedModuleName duplicate ++ " is declared by both "
              ++ parsedModulePath original ++ " and "
              ++ parsedModulePath duplicate
          )
  | (original, duplicate) <- SharedCollection.repetitionsWithFirstOn
      parsedModuleName modules
  ]

targetModuleMismatchDiagnostics
  :: [WorkspaceTarget]
  -> [WorkspaceModule]
  -> Maybe (NonEmpty Diagnostic)
targetModuleMismatchDiagnostics targets modules = NonEmpty.nonEmpty
  [ withSource path $ workspaceFailure
      "DJEX_REPL_MODULE_MISMATCH"
      "resolved source has the wrong module name"
      $ expected ++ " was requested, but the file declares " ++ actual
  | target <- targets
  , expected <- targetModuleExpectations target
  , path <- targetSourceFiles target
  , Just modul <- [Map.lookup path byPath]
  , let actual = parsedModuleName modul
  , actual /= expected
  ]
 where
  byPath = modulesByPath modules

discoverDependencies
  :: [WorkspaceTarget]
  -> [WorkspaceModule]
  -> IO (Either (NonEmpty Diagnostic) [WorkspaceModule])
discoverDependencies targets initial = go
    (modulesByName initial)
    (modulesByPath initial)
    initialQueue
    initialQueue
 where
  initialQueue = Seq.fromList initial
  roots = sourceRoots targets initial

  go byName byPath discovered pending = case Seq.viewl pending of
    EmptyL -> pure $ Right $ toList discovered
    modul :< remaining -> do
      expanded <- foldEitherM (discoverImport modul)
        (byName, byPath, discovered, remaining)
        $ parsedModuleImports modul
      case expanded of
        Left failures -> pure $ Left failures
        Right (nextByName, nextByPath, nextDiscovered, nextPending) ->
          go nextByName nextByPath nextDiscovered nextPending

  discoverImport _ state WorkspaceImport {importedFromPackage = True} =
    pure $ Right state
  discoverImport importer state@(byName, byPath, discovered, pending) imported
    | importedModuleName imported `Map.member` byName = pure $ Right state
    | otherwise = do
        resolved <- resolveModuleFile roots
          (moduleSegments $ importedModuleName imported)
        case resolved of
          Left failures -> pure $ Left failures
          Right Nothing -> pure $ Right state
          Right (Just path) -> case Map.lookup path byPath of
            Just dependency -> pure $ state
              <$ validateDependency importer imported byName dependency
            Nothing -> do
              parsed <- parseWorkspaceModule path
              pure $ parsed >>= \dependency -> do
                validateDependency importer imported byName dependency
                Right
                  ( Map.insert (parsedModuleName dependency) dependency byName
                  , Map.insert path dependency byPath
                  , discovered |> dependency
                  , pending |> dependency
                  )

  validateDependency importer imported byName dependency
    | parsedModuleName dependency /= importedModuleName imported =
        singletonFailure
          "DJEX_REPL_MODULE_MISMATCH"
          "resolved source has the wrong module name"
          (parsedModulePath dependency)
          ( importedModuleName imported ++ " was imported by "
              ++ parsedModuleName importer ++ ", but the file declares "
              ++ parsedModuleName dependency
          )
    | otherwise = case Map.lookup (parsedModuleName dependency) byName of
      Just original
        | parsedModulePath original /= parsedModulePath dependency ->
            singletonFailure
              "DJEX_REPL_MODULE_DUPLICATE"
              "duplicate source module"
              (parsedModulePath dependency)
              (parsedModuleName dependency ++ " is also declared by "
                ++ parsedModulePath original)
      _ -> Right ()

  toList :: Seq value -> [value]
  toList = Foldable.toList

moduleSegments :: String -> [String]
moduleSegments source = case SharedName.mkModuleName source of
  Left _ -> []
  Right name -> SharedName.moduleNameSegments name

modulesByName :: [WorkspaceModule] -> Map.Map String WorkspaceModule
modulesByName = Map.fromList . map (\modul -> (parsedModuleName modul, modul))

modulesByPath :: [WorkspaceModule] -> Map.Map FilePath WorkspaceModule
modulesByPath = Map.fromList . map (\modul -> (parsedModulePath modul, modul))

-- Derive hierarchical module roots only from explicit file/module targets.
-- A directory target already owns one bounded lookup root. Its source list may
-- contain a regular-file symlink whose canonical path is outside that root;
-- treating every loaded module's canonical directory as another root would let
-- imports beside that linked file escape the admitted directory transitively.
sourceRoots :: [WorkspaceTarget] -> [WorkspaceModule] -> [FilePath]
sourceRoots targets modules = stableNub
  $ concatMap targetRoots targets
 where
  byPath = modulesByPath modules

  targetRoots target = case locatorKind locator of
    DirectoryTarget -> [locatorPath locator]
    _ ->
      [locatorAdmissionRoot locator, takeDirectory $ locatorPath locator]
        ++ [ moduleSourceRoot modul
           | path <- targetSourceFiles target
           , Just modul <- [Map.lookup path byPath]
           ]
   where
    locator = targetLocator target

moduleSourceRoot :: WorkspaceModule -> FilePath
moduleSourceRoot modul
  | pathMatchesModule (parsedModulePath modul) segments =
      iterate takeDirectory (takeDirectory $ parsedModulePath modul)
        !! max 0 (length segments - 1)
  | otherwise = takeDirectory $ parsedModulePath modul
 where
  segments = moduleSegments $ parsedModuleName modul

pathMatchesModule :: FilePath -> [String] -> Bool
pathMatchesModule _ [] = False
pathMatchesModule path segments = reverse segments == take (length segments)
  (pathComponents path)
 where
  pathComponents current
    | parent == current = []
    | otherwise = dropExtension (takeFileName current) : pathComponents parent
   where
    parent = takeDirectory current

resolveModuleFile
  :: [FilePath]
  -> [String]
  -> IO (Either (NonEmpty Diagnostic) (Maybe FilePath))
resolveModuleFile _ [] = pure $ Right Nothing
resolveModuleFile roots segments = search roots
 where
  relative = joinPath segments
  search [] = pure $ Right Nothing
  search (root : remaining) = tryCandidates
    [root </> relative <.> "hs", root </> relative <.> "lhs"]
    >>= \case
      Left failures -> pure $ Left failures
      Right Nothing -> search remaining
      Right found -> pure $ Right found

  tryCandidates [] = pure $ Right Nothing
  tryCandidates (candidate : remaining) = do
    inspected <- tryIOError $ doesFileExist candidate
    case inspected of
      Left failure -> pure $ singletonFailure
        "DJEX_REPL_TARGET_IO" "cannot inspect imported source module"
        candidate $ show failure
      Right False -> tryCandidates remaining
      Right True -> do
        canonical <- canonicalPath "cannot resolve imported source module"
          candidate
        pure $ Just <$> canonical

dependencyOrder
  :: [FilePath]
  -> [WorkspaceModule]
  -> Either (NonEmpty Diagnostic) [WorkspaceModule]
dependencyOrder explicitPaths modules = do
  let byName = modulesByName modules
      byPath = modulesByPath modules
      roots = stableNub
        $ [parsedModuleName modul | path <- explicitPaths, Just modul <- [Map.lookup path byPath]]
        ++ map parsedModuleName modules
  case stableDependencyOrder roots byName localDependencies of
    Right ordered -> Right ordered
    Left DependencyCycle
        { dependencyCycleSource
        , dependencyCyclePath
        } ->
      let cycleFailure =
            workspaceFailure
              "DJEX_REPL_MODULE_CYCLE"
              "cyclic non-SOURCE module imports"
              $ intercalate " -> " $ NonEmpty.toList dependencyCyclePath
      in Left $ maybe cycleFailure
          (\current -> withSource (parsedModulePath current) cycleFailure)
          (Map.lookup dependencyCycleSource byName)
        :| []
 where
  localDependencies modul =
    [ importedModuleName imported
    | imported <- parsedModuleImports modul
    , not $ importedFromPackage imported
    , not $ importedAsSource imported
    ]

annotateTargets
  :: [WorkspaceTarget]
  -> [WorkspaceModule]
  -> [WorkspaceTarget]
annotateTargets targets modules = map annotate targets
 where
  byPath = modulesByPath modules
  annotate target
    | locatorKind (targetLocator target) == DirectoryTarget = target
        {targetModuleSpellings = []}
    | [path] <- targetSourceFiles target
    , Just modul <- Map.lookup path byPath = target
        { targetModuleSpellings = stableNub
            $ targetModuleExpectations target ++ [parsedModuleName modul]
        }
    | otherwise = target
        {targetModuleSpellings = targetModuleExpectations target}

automaticTargetKey
  :: [TargetKey]
  -> [WorkspaceTarget]
  -> [WorkspaceModule]
  -> Maybe TargetKey
automaticTargetKey preferences targets modules = firstContributing
  $ stableNub preferences
 where
  byPath = modulesByPath modules
  targetsByKey = Map.fromList [(targetKey target, target) | target <- targets]
  firstContributing [] = Nothing
  firstContributing (key : remaining) = case Map.lookup key targetsByKey of
    Just target | not $ null $ targetContribution byPath target -> Just key
    _ -> firstContributing remaining

targetContribution
  :: Map.Map FilePath WorkspaceModule
  -> WorkspaceTarget
  -> [(WorkspaceModule, Bool)]
targetContribution byPath target =
  [ (modul, True)
  | path <- targetSourceFiles target
  , Just modul <- [Map.lookup path byPath]
  ]

removalMatches
  :: [WorkspaceTarget]
  -> String
  -> IO (Either (NonEmpty Diagnostic) (Set.Set (TargetKind, FilePath)))
removalMatches targets raw = do
  let source = case raw of
        '*' : rest -> rest
        _ -> raw
      byModule = Set.fromList
        [ targetKey target
        | target <- targets
        , source `elem` targetModuleSpellings target
        ]
  byPath <- removalPath source
  let pathMatches = case byPath of
        Nothing -> Set.empty
        Just path -> Set.fromList
          [targetKey target | target <- targets, locatorPath (targetLocator target) == path]
      matches = byModule `Set.union` pathMatches
  pure $ if Set.null matches
    then singletonFailure
      "DJEX_REPL_TARGET_NOT_LOADED" "source target is not loaded"
      source "expected an admitted module spelling or canonical target path"
    else Right matches

removalPath :: FilePath -> IO (Maybe FilePath)
removalPath source
  | null source = pure Nothing
  | otherwise = do
      inspected <- tryIOError $ doesPathExist source
      if fromRight False inspected
        then either (const Nothing) Just <$> tryIOError (canonicalizePath source)
        else if isAbsolute source || looksLikePath source
          then either (const Nothing) (Just . normalise)
            <$> tryIOError (makeAbsolute source)
          else pure Nothing

targetKey :: WorkspaceTarget -> (TargetKind, FilePath)
targetKey target =
  (identityKind $ locatorKind locator, locatorPath locator)
 where
  locator = targetLocator target
  identityKind DirectoryTarget = DirectoryTarget
  identityKind _ = SourceFileTarget

deduplicateTargets :: [WorkspaceTarget] -> [WorkspaceTarget]
deduplicateTargets = foldl' insert []
 where
  insert retained candidate = case break ((== targetKey candidate) . targetKey)
      retained of
    (_, []) -> retained ++ [candidate]
    (before, existing : after) -> before ++ [merge existing candidate] ++ after
  merge existing candidate = existing
    { targetStarred = targetStarred existing || targetStarred candidate
    , targetModuleExpectations = stableNub
        $ targetModuleExpectations existing
        ++ targetModuleExpectations candidate
    , targetModuleSpellings = stableNub
        $ targetModuleSpellings existing
        ++ targetModuleSpellings candidate
    }

mergeTargets :: [WorkspaceTarget] -> [WorkspaceTarget] -> [WorkspaceTarget]
mergeTargets old new = deduplicateTargets $ old ++ new

stableNub :: Ord value => [value] -> [value]
stableNub = SharedCollection.distinctOn id

foldEitherM
  :: (state -> value -> IO (Either error state))
  -> state
  -> [value]
  -> IO (Either error state)
foldEitherM _ state [] = pure $ Right state
foldEitherM action state (value : remaining) = do
  result <- action state value
  case result of
    Left failure -> pure $ Left failure
    Right next -> foldEitherM action next remaining

collectResults
  :: [Either (NonEmpty error) value]
  -> Either (NonEmpty error) [value]
collectResults results = case partitionEithers results of
  ([], values) -> Right values
  (failure : failures, _) -> Left $ sconcat $ failure :| failures

workspaceFailure :: String -> String -> String -> Diagnostic
workspaceFailure code summary detail =
  contextualDiagnostic Error code summary detail

singletonFailure
  :: String
  -> String
  -> FilePath
  -> String
  -> Either (NonEmpty Diagnostic) value
singletonFailure code summary path detail = Left
  $ withSource path (workspaceFailure code summary detail) :| []
