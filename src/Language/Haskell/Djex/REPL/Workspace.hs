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
  , workspaceExplicitTargets
  , workspaceTargets
  , workspaceLoadedModules
  , workspaceModules
  , workspaceModuleFiles
  , workspaceRatingFiles
  , workspaceModuleName
  , workspaceModulePath
  , workspaceModuleSyntax
  , workspaceTargetDisplay
  , workspaceTargetIsStarred
  , workspaceTargetModuleFiles
  , workspaceTargetRatingFiles
  , workspaceTargetModuleName
  , workspaceAutomaticTargetModule
  ) where

import Data.Either (partitionEithers)
import Data.List (intercalate, sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Directory
  ( canonicalizePath
  , doesDirectoryExist
  , doesFileExist
  , doesPathExist
  , getCurrentDirectory
  , listDirectory
  , makeAbsolute
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
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error)
  , contextualDiagnostic
  , withSource
  )
import qualified Language.Haskell.Synthesis.Name as SharedName

-- | An immutable snapshot of explicit source targets and their local import
-- closure.  Accessors preserve target admission order and dependency order.
data SourceWorkspace = SourceWorkspace
  { sourceWorkspaceTargets :: [WorkspaceTarget]
  , sourceWorkspaceModules :: [WorkspaceModule]
  , sourceWorkspaceRatings :: [FilePath]
  , sourceWorkspaceAutomatic :: Maybe (WorkspaceModule, Bool)
  }

-- | One explicit @:load@ or @:add@ target.  A directory is one target even
-- though the Djex directory extension expands it to several source files.
data WorkspaceTarget = WorkspaceTarget
  { targetLocator :: TargetLocator
  , targetStarred :: Bool
  , targetModuleSpellings :: [String]
  , targetSourceFiles :: [FilePath]
  , targetRatings :: [FilePath]
  }

-- | A parsed ordinary Haskell module.  Dependency metadata stays private so
-- callers cannot make the checked ordering disagree with the syntax tree.
data WorkspaceModule = WorkspaceModule
  { parsedModuleName :: String
  , parsedModulePath :: FilePath
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

-- All locator fields are total.  A module spelling is present only for a
-- target admitted through its hierarchical module name; the admission root is
-- retained for dependency lookup after the process working directory changes.
data TargetLocator = TargetLocator
  { locatorKind :: TargetKind
  , locatorPath :: FilePath
  , locatorModuleName :: Maybe String
  , locatorAdmissionRoot :: FilePath
  }

data VisitState = Visiting | Visited
  deriving (Eq, Show)

-- | Resolve a fresh target list relative to the current directory.
loadWorkspace
  :: [String]
  -> IO (Either (NonEmpty Diagnostic) SourceWorkspace)
loadWorkspace arguments = do
  admitted <- admitTargets arguments
  case admitted of
    Left failures -> pure $ Left failures
    Right targets -> buildWorkspace targets

-- | Re-read the canonical targets retained by a previous snapshot.
reloadWorkspace
  :: SourceWorkspace
  -> IO (Either (NonEmpty Diagnostic) SourceWorkspace)
reloadWorkspace = buildWorkspace . sourceWorkspaceTargets

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
    Right targets -> buildWorkspace $ mergeTargets
      (sourceWorkspaceTargets workspace) targets

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
    Right keys -> buildWorkspace
      [ target
      | target <- sourceWorkspaceTargets workspace
      , targetKey target `Set.notMember` Set.unions keys
      ]

-- | Explicit targets in admission order.
workspaceExplicitTargets :: SourceWorkspace -> [WorkspaceTarget]
workspaceExplicitTargets = sourceWorkspaceTargets

-- | Short alias useful to the REPL state renderer.
workspaceTargets :: SourceWorkspace -> [WorkspaceTarget]
workspaceTargets = workspaceExplicitTargets

-- | Loaded source modules in deterministic dependency-first order.
workspaceLoadedModules :: SourceWorkspace -> [WorkspaceModule]
workspaceLoadedModules = sourceWorkspaceModules

-- | Short alias useful to source-session construction.
workspaceModules :: SourceWorkspace -> [WorkspaceModule]
workspaceModules = workspaceLoadedModules

-- | Canonical module files in dependency-first loader order.
workspaceModuleFiles :: SourceWorkspace -> [FilePath]
workspaceModuleFiles = map workspaceModulePath . workspaceLoadedModules

-- | Canonical rating files contributed by explicit directory targets.
workspaceRatingFiles :: SourceWorkspace -> [FilePath]
workspaceRatingFiles = sourceWorkspaceRatings

workspaceModuleName :: WorkspaceModule -> String
workspaceModuleName = parsedModuleName

workspaceModulePath :: WorkspaceModule -> FilePath
workspaceModulePath = parsedModulePath

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

workspaceTargetIsStarred :: WorkspaceTarget -> Bool
workspaceTargetIsStarred = targetStarred

workspaceTargetModuleFiles :: WorkspaceTarget -> [FilePath]
workspaceTargetModuleFiles = targetSourceFiles

workspaceTargetRatingFiles :: WorkspaceTarget -> [FilePath]
workspaceTargetRatingFiles = targetRatings

-- | The declared/admitted module name for a single-source target.  Directory
-- targets deliberately return 'Nothing', even when they currently contain one
-- file, because their membership may change on reload.
workspaceTargetModuleName :: WorkspaceTarget -> Maybe String
workspaceTargetModuleName target = case targetModuleSpellings target of
  [name] | locatorKind (targetLocator target) /= DirectoryTarget -> Just name
  _ -> Nothing

-- | Most recent explicit source module eligible for GHCi's automatic context.
-- The accompanying flag is always 'True': Djex source-interprets every loaded
-- module, so its automatic context has the semantics of GHCi's @*M@ even when
-- the target was written without a leading star.  The original spelling
-- remains available through 'workspaceTargetIsStarred'.
workspaceAutomaticTargetModule
  :: SourceWorkspace
  -> Maybe (WorkspaceModule, Bool)
workspaceAutomaticTargetModule = sourceWorkspaceAutomatic

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
  , targetModuleSpellings = []
  , targetSourceFiles = [path]
  , targetRatings = []
  }

admitDirectory
  :: Bool
  -> FilePath
  -> IO (Either (NonEmpty Diagnostic) WorkspaceTarget)
admitDirectory starred source = do
  canonical <- canonicalPath "cannot resolve source directory" source
  case canonical of
    Left failures -> pure $ Left failures
    Right path -> do
      contents <- directoryContents path
      pure $ fmap (directoryTarget starred path) contents

directoryTarget
  :: Bool
  -> FilePath
  -> ([FilePath], [FilePath])
  -> WorkspaceTarget
directoryTarget starred path (sources, ratings) = WorkspaceTarget
  { targetLocator = TargetLocator
      { locatorKind = DirectoryTarget
      , locatorPath = path
      , locatorModuleName = Nothing
      , locatorAdmissionRoot = path
      }
  , targetStarred = starred
  , targetModuleSpellings = []
  , targetSourceFiles = sources
  , targetRatings = ratings
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
        , targetModuleSpellings = [source]
        , targetSourceFiles = [path]
        , targetRatings = []
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
  -> IO (Either (NonEmpty Diagnostic) ([FilePath], [FilePath]))
directoryContents root = go Set.empty root
 where
  go visited directory
    | directory `Set.member` visited = pure $ Right ([], [])
    | otherwise = do
        listed <- tryIOError $ listDirectory directory
        case listed of
          Left failure -> pure $ singletonFailure
            "DJEX_REPL_TARGET_IO" "cannot read source directory"
            directory $ show failure
          Right entries -> walk (Set.insert directory visited)
            [] [] $ map (directory </>) $ sort entries

  walk _ sources ratings [] = pure $ Right
    (stableNub sources, stableNub ratings)
  walk visited sources ratings (path : remaining) = do
    inspected <- tryIOError $ do
      isDirectory <- doesDirectoryExist path
      isFile <- doesFileExist path
      pure (isDirectory, isFile)
    case inspected of
      Left failure -> pure $ singletonFailure
        "DJEX_REPL_TARGET_IO" "cannot inspect source-directory entry"
        path $ show failure
      Right (True, _) -> do
        canonical <- canonicalPath "cannot resolve source subdirectory" path
        case canonical of
          Left failures -> pure $ Left failures
          Right directory -> do
            nested <- go visited directory
            case nested of
              Left failures -> pure $ Left failures
              Right (nestedSources, nestedRatings) -> walk
                (Set.insert directory visited)
                (sources ++ nestedSources)
                (ratings ++ nestedRatings)
                remaining
      Right (_, True) -> do
        canonical <- canonicalPath "cannot resolve source-directory file" path
        case canonical of
          Left failures -> pure $ Left failures
          Right file
            | isSourceFile file -> walk visited
                (sources ++ [file]) ratings remaining
            | isRatingFile file -> walk visited sources
                (ratings ++ [file]) remaining
            | otherwise -> walk visited sources ratings remaining
      Right _ -> walk visited sources ratings remaining

isSourceFile :: FilePath -> Bool
isSourceFile path = takeExtension path `elem` [".hs", ".lhs"]

isRatingFile :: FilePath -> Bool
isRatingFile path = takeExtension path == ".ratings"

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
        {targetSourceFiles = [locatorPath locator], targetRatings = []}
 where
  locator = targetLocator target
  updatedDirectory value (sources, ratings) = value
    {targetSourceFiles = sources, targetRatings = ratings}

buildWorkspace
  :: [WorkspaceTarget]
  -> IO (Either (NonEmpty Diagnostic) SourceWorkspace)
buildWorkspace targets = do
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
            pure $ discovered >>= finishWorkspace refreshed sourcePaths

finishWorkspace
  :: [WorkspaceTarget]
  -> [FilePath]
  -> [WorkspaceModule]
  -> Either (NonEmpty Diagnostic) SourceWorkspace
finishWorkspace targets explicitPaths discovered = do
  ordered <- dependencyOrder explicitPaths discovered
  let annotatedTargets = annotateTargets targets discovered
      automatic = automaticTarget annotatedTargets discovered
      ratings = stableNub $ concatMap targetRatings annotatedTargets
  pure SourceWorkspace
    { sourceWorkspaceTargets = annotatedTargets
    , sourceWorkspaceModules = ordered
    , sourceWorkspaceRatings = ratings
    , sourceWorkspaceAutomatic = automatic
    }

parseWorkspaceModule
  :: FilePath
  -> IO (Either (NonEmpty Diagnostic) WorkspaceModule)
parseWorkspaceModule path = do
  parsed <- tryIOError $ HSEFile.parseFileWithMode
    (haskellSrcExtsParseMode path) path
  pure $ case parsed of
    Left failure -> singletonFailure
      "DJEX_REPL_MODULE_READ" "cannot read source module" path $ show failure
    Right parseResult -> case parseResult of
      HSE.ParseFailed location detail -> Left
        $ withHaskellSrcLocation location
            (workspaceFailure "DJEX_REPL_MODULE_PARSE"
              "could not parse source module" detail)
        :| []
      HSE.ParseOk syntax -> workspaceModuleFromSyntax path syntax

workspaceModuleFromSyntax
  :: FilePath
  -> HSE.Module HSE.SrcSpanInfo
  -> Either (NonEmpty Diagnostic) WorkspaceModule
workspaceModuleFromSyntax path syntax = case syntax of
  HSE.Module _ moduleHead _ imports _ -> do
    name <- declaredModuleName path moduleHead
    convertedImports <- traverse (workspaceImport path) imports
    pure WorkspaceModule
      { parsedModuleName = name
      , parsedModulePath = path
      , parsedModuleSyntax = syntax
      , parsedModuleImports = convertedImports
      }
  _ -> singletonFailure
    "DJEX_REPL_MODULE_FORM" "unsupported source module form" path
    "expected an ordinary Haskell module"

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
duplicateModuleDiagnostics modules = NonEmpty.nonEmpty
  [ withSource (parsedModulePath duplicate)
      $ workspaceFailure
          "DJEX_REPL_MODULE_DUPLICATE"
          "duplicate source module"
          ( parsedModuleName duplicate ++ " is declared by both "
              ++ parsedModulePath original ++ " and "
              ++ parsedModulePath duplicate
          )
  | group <- Map.elems $ Map.fromListWith (++)
      [(parsedModuleName modul, [modul]) | modul <- modules]
  , original : duplicates <- [group]
  , duplicate <- duplicates
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
  , locatorKind (targetLocator target) == ModuleNameTarget
  , Just expected <- [locatorModuleName $ targetLocator target]
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
discoverDependencies targets initial = go initial 0
 where
  go modules index
    | index >= length modules = pure $ Right modules
    | otherwise = do
        let modul = modules !! index
        expanded <- foldEitherM (discoverImport modul) modules
          $ parsedModuleImports modul
        case expanded of
          Left failures -> pure $ Left failures
          Right next -> go next $ index + 1

  discoverImport _ modules WorkspaceImport {importedFromPackage = True} =
    pure $ Right modules
  discoverImport importer modules imported
    | importedModuleName imported `Map.member` modulesByName modules =
        pure $ Right modules
    | otherwise = do
        resolved <- resolveModuleFile (sourceRoots targets modules)
          (moduleSegments $ importedModuleName imported)
        case resolved of
          Left failures -> pure $ Left failures
          Right Nothing -> pure $ Right modules
          Right (Just path)
            | path `Map.member` modulesByPath modules -> pure $ Right modules
            | otherwise -> do
                parsed <- parseWorkspaceModule path
                pure $ parsed >>= \dependency ->
                  if parsedModuleName dependency /= importedModuleName imported
                    then singletonFailure
                      "DJEX_REPL_MODULE_MISMATCH"
                      "resolved source has the wrong module name"
                      path
                      ( importedModuleName imported ++ " was imported by "
                          ++ parsedModuleName importer ++ ", but the file declares "
                          ++ parsedModuleName dependency
                      )
                    else case Map.lookup (parsedModuleName dependency)
                        $ modulesByName modules of
                      Just original
                        | parsedModulePath original /= parsedModulePath dependency ->
                            singletonFailure
                              "DJEX_REPL_MODULE_DUPLICATE"
                              "duplicate source module"
                              path
                              (parsedModuleName dependency ++ " is also declared by "
                                ++ parsedModulePath original)
                      _ -> Right $ modules ++ [dependency]

moduleSegments :: String -> [String]
moduleSegments source = case SharedName.mkModuleName source of
  Left _ -> []
  Right name -> SharedName.moduleNameSegments name

modulesByName :: [WorkspaceModule] -> Map.Map String WorkspaceModule
modulesByName = Map.fromList . map (\modul -> (parsedModuleName modul, modul))

modulesByPath :: [WorkspaceModule] -> Map.Map FilePath WorkspaceModule
modulesByPath = Map.fromList . map (\modul -> (parsedModulePath modul, modul))

sourceRoots :: [WorkspaceTarget] -> [WorkspaceModule] -> [FilePath]
sourceRoots targets modules = stableNub
  $ concatMap targetRoots targets ++ map moduleSourceRoot modules
 where
  targetRoots target = case locatorKind locator of
    DirectoryTarget -> [locatorPath locator]
    _ -> [locatorAdmissionRoot locator, takeDirectory $ locatorPath locator]
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
    >>= \caseResult -> case caseResult of
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
  (_, ordered) <- foldlVisit byName (Map.empty, []) roots
  pure ordered

foldlVisit
  :: Map.Map String WorkspaceModule
  -> (Map.Map String VisitState, [WorkspaceModule])
  -> [String]
  -> Either (NonEmpty Diagnostic) (Map.Map String VisitState, [WorkspaceModule])
foldlVisit _ state [] = Right state
foldlVisit modules state (name : remaining) = do
  next <- visitModule modules [] state name
  foldlVisit modules next remaining

visitModule
  :: Map.Map String WorkspaceModule
  -> [String]
  -> (Map.Map String VisitState, [WorkspaceModule])
  -> String
  -> Either (NonEmpty Diagnostic) (Map.Map String VisitState, [WorkspaceModule])
visitModule modules stack state@(marks, ordered) name = case Map.lookup name marks of
  Just Visited -> Right state
  Just Visiting -> case Map.lookup currentName modules of
    Nothing -> Right state
    Just current -> Left $ withSource (parsedModulePath current)
      (workspaceFailure
        "DJEX_REPL_MODULE_CYCLE"
        "cyclic non-SOURCE module imports"
        $ intercalate " -> " cycleNames)
      :| []
  Nothing -> case Map.lookup name modules of
    Nothing -> Right state
    Just modul -> do
      let marked = (Map.insert name Visiting marks, ordered)
          dependencies =
            [ importedModuleName imported
            | imported <- parsedModuleImports modul
            , not $ importedFromPackage imported
            , not $ importedAsSource imported
            , importedModuleName imported `Map.member` modules
            ]
      (afterDependencies, accumulated) <- foldlVisitWithStack modules
        (name : stack) marked dependencies
      pure
        ( Map.insert name Visited afterDependencies
        , accumulated ++ [modul]
        )
 where
  currentName = case stack of
    current : _ -> current
    [] -> name
  cycleNames = name : reverse (takeWhile (/= name) stack) ++ [name]

foldlVisitWithStack
  :: Map.Map String WorkspaceModule
  -> [String]
  -> (Map.Map String VisitState, [WorkspaceModule])
  -> [String]
  -> Either (NonEmpty Diagnostic) (Map.Map String VisitState, [WorkspaceModule])
foldlVisitWithStack _ _ state [] = Right state
foldlVisitWithStack modules stack state (name : remaining) = do
  next <- visitModule modules stack state name
  foldlVisitWithStack modules stack next remaining

annotateTargets
  :: [WorkspaceTarget]
  -> [WorkspaceModule]
  -> [WorkspaceTarget]
annotateTargets targets modules = map annotate targets
 where
  byPath = modulesByPath modules
  annotate target
    | locatorKind (targetLocator target) == DirectoryTarget = target
    | [path] <- targetSourceFiles target
    , Just modul <- Map.lookup path byPath = target
        { targetModuleSpellings = stableNub
            $ targetModuleSpellings target ++ [parsedModuleName modul]
        }
    | otherwise = target

automaticTarget
  :: [WorkspaceTarget]
  -> [WorkspaceModule]
  -> Maybe (WorkspaceModule, Bool)
automaticTarget targets modules = firstJust
  [ fmap (\modul -> (modul, True)) $ Map.lookup path byPath
  | target <- reverse targets
  , path <- reverse $ targetSourceFiles target
  ]
 where
  byPath = modulesByPath modules

firstJust :: [Maybe value] -> Maybe value
firstJust [] = Nothing
firstJust (Just value : _) = Just value
firstJust (Nothing : remaining) = firstJust remaining

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
      if either (const False) id inspected
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
    , targetModuleSpellings = stableNub
        $ targetModuleSpellings existing ++ targetModuleSpellings candidate
    }

mergeTargets :: [WorkspaceTarget] -> [WorkspaceTarget] -> [WorkspaceTarget]
mergeTargets old new = deduplicateTargets $ old ++ new

stableNub :: Ord value => [value] -> [value]
stableNub = go Set.empty
 where
  go _ [] = []
  go seen (value : remaining)
    | value `Set.member` seen = go seen remaining
    | otherwise = value : go (Set.insert value seen) remaining

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
  (failures, _) -> case NonEmpty.nonEmpty $ concatMap NonEmpty.toList failures of
    Just combined -> Left combined
    Nothing -> error "collectResults: partition reported empty failures"

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
