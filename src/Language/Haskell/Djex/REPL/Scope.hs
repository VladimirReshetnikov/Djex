{-# LANGUAGE NamedFieldPuns #-}

-- | Pure, transactional name-scope management for the Djex REPL.
--
-- A 'SourceWorkspace' says which modules are loaded, while an 'Inventory'
-- remains the authority for the declarations that actually survived parsing,
-- structural validation, and kind checking.  This module deliberately joins
-- those two views without manufacturing names from the source syntax tree.
module Language.Haskell.Djex.REPL.Scope
  ( ReplScope
  , ScopeEntry (..)
  , ScopeOrigin (..)
  , scopeFromWorkspace
  , resetScopeForWorkspace
  , revalidateScope
  , addScopeImport
  , changeScopeModules
  , scopeEntries
  , scopeUnqualifiedNames
  , scopeVisibleNames
  , scopeSearchNames
  , scopeQualifiedNames
  , scopeQualifierAliases
  , scopeCurrentModule
  , renderScopeImports
  , renderScopeModules
  , moduleNamesForBrowse
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, modify, runStateT)
import Data.Char (isSpace)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set)

import qualified Language.Haskell.Exts.Extension as HSE
import qualified Language.Haskell.Exts.Parser as HSE
import qualified Language.Haskell.Exts.Pretty as HSE
import qualified Language.Haskell.Exts.SrcLoc as HSE
import qualified Language.Haskell.Exts.Syntax as HSE

import Language.Haskell.Djex.REPL.Command (ModuleChange (..))
import Language.Haskell.Djex.REPL.Workspace
  ( SourceWorkspace
  , WorkspaceModule
  , workspaceAutomaticTargetModules
  , workspaceModuleName
  , workspaceModulePath
  , workspaceModuleSyntax
  , workspaceModules
  )
import Language.Haskell.Synthesis.Declaration
  ( DataConstructor (constructorName)
  , Declaration (..)
  , ValueSignature (valueName)
  )
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error)
  , contextualDiagnostic
  , withSource
  )
import Language.Haskell.Synthesis.Environment (environmentDeclarations)
import Language.Haskell.Synthesis.Inventory
  ( Inventory
  , inventoryEnvironment
  )
import Language.Haskell.Synthesis.Name (ModuleName, Name)
import qualified Language.Haskell.Synthesis.Name as SharedName

-- | Whether a module-context entry came from the user's @:module@ command or
-- from the most recent @:load@ target.
data ScopeOrigin = ExplicitScope | AutomaticScope
  deriving (Eq, Ord, Show)

-- | One ordered prompt-context entry.  Imports retain source text rather than
-- exposing a @haskell-src-exts@ tree in the REPL state API; every constructor
-- is revalidated transactionally before it can enter a 'ReplScope'.
data ScopeEntry
  = ScopeModule ScopeOrigin Bool ModuleName
    -- ^ Origin, starred/full-top-level flag, and canonical module.
  | ScopeImport String
    -- ^ A complete Haskell import declaration.
  deriving (Eq, Ord, Show)

-- The separate projections are intentional. A qualified import adds bindings
-- to backend search, but it must not make a type constructor legal bare at the
-- prompt. Keeping both sets avoids recreating that distinction downstream.
data ReplScope = ReplScope
  { replScopeEntries :: [ScopeEntry]
  , replScopeUnqualifiedNames :: [Name]
  , replScopeSearchNames :: [Name]
  , replScopeQualifiedNames :: [(ModuleName, [Name])]
  , replScopeAliases :: [(ModuleName, ModuleName)]
  , replScopeCurrentModule :: Maybe ModuleName
  , replScopeHasImplicitPrelude :: Bool
  }
  deriving (Eq, Show)

scopeEntries :: ReplScope -> [ScopeEntry]
scopeEntries = replScopeEntries

-- | Exact canonical names that may be written without a qualifier.
scopeUnqualifiedNames :: ReplScope -> [Name]
scopeUnqualifiedNames = replScopeUnqualifiedNames

-- | Compatibility alias for 'scopeUnqualifiedNames'.
scopeVisibleNames :: ReplScope -> [Name]
scopeVisibleNames = scopeUnqualifiedNames

-- | Exact canonical values and constructors available to synthesis, whether
-- admitted unqualified or through a written qualifier.
scopeSearchNames :: ReplScope -> [Name]
scopeSearchNames = replScopeSearchNames

-- | Exact names admitted under each written qualifier.  This is separate from
-- 'scopeQualifierAliases': an alias alone cannot encode an explicit import
-- list or @hiding@ clause.
scopeQualifiedNames :: ReplScope -> [(ModuleName, [Name])]
scopeQualifiedNames = replScopeQualifiedNames

-- | Non-canonical written qualifier to canonical defining module.
scopeQualifierAliases :: ReplScope -> [(ModuleName, ModuleName)]
scopeQualifierAliases = replScopeAliases

-- | The sole starred current module, when there is exactly one.  Returning
-- 'Nothing' for several starred modules preserves genuine ambiguity instead
-- of giving one module an arbitrary lookup priority.
scopeCurrentModule :: ReplScope -> Maybe ModuleName
scopeCurrentModule = replScopeCurrentModule

-- | Construct the initial prompt scope, including GHCi's automatic starred
-- context for the most recent source-interpreted target.
scopeFromWorkspace
  :: Inventory typeVariable annotation
  -> SourceWorkspace
  -> Either Diagnostic ReplScope
scopeFromWorkspace inventory workspace = do
  entries <- automaticEntries workspace
  compileScope inventory workspace entries

-- | Discard explicit imports and module changes after a replacing @:load@.
resetScopeForWorkspace
  :: Inventory typeVariable annotation
  -> SourceWorkspace
  -> Either Diagnostic ReplScope
resetScopeForWorkspace = scopeFromWorkspace

-- | Revalidate retained entries after @:reload@. Entries whose modules
-- disappeared are pruned, old automatic entries are replaced, and the fresh
-- automatic context is appended after explicit entries. This mirrors GHCi:
-- an explicit @M@ may coexist with automatic @*M@, while an already-explicit
-- @*M@ suppresses an exact duplicate automatic entry.
revalidateScope
  :: Inventory typeVariable annotation
  -> SourceWorkspace
  -> ReplScope
  -> Either Diagnostic ReplScope
revalidateScope inventory workspace scope = do
  modules <- workspaceModuleMap workspace
  let surviving = filter (entryIsLoaded modules)
        $ filter (not . isAutomaticEntry) $ scopeEntries scope
  automatic <- automaticEntries workspace
  compileScope inventory workspace
    $ surviving ++ filter (not . alreadyExplicit surviving) automatic
 where
  isAutomaticEntry (ScopeModule AutomaticScope _ _) = True
  isAutomaticEntry _ = False
  alreadyExplicit entries (ScopeModule _ starred moduleName) = any
    (sameModuleEntry starred moduleName) entries
  alreadyExplicit _ _ = False
  sameModuleEntry wantedStar wantedModule (ScopeModule _ starred moduleName) =
    wantedStar == starred && wantedModule == moduleName
  sameModuleEntry _ _ _ = False

entryIsLoaded :: Map ModuleName WorkspaceModule -> ScopeEntry -> Bool
entryIsLoaded modules entry = case entry of
  ScopeModule _ _ moduleName -> Map.member moduleName modules
  ScopeImport source -> case parseImport source of
    Right declaration -> case importModuleName declaration of
      Right moduleName -> Map.member moduleName modules
      Left _ -> False
    Left _ -> False

-- | Parse and append one complete Haskell import declaration.
addScopeImport
  :: Inventory typeVariable annotation
  -> SourceWorkspace
  -> String
  -> ReplScope
  -> Either Diagnostic ReplScope
addScopeImport inventory workspace source scope = do
  declaration <- parseImport source
  let normalized = HSE.prettyPrint declaration
  compileScope inventory workspace
    $ scopeEntries scope ++ [ScopeImport normalized]

-- | Apply @:module@ replacement, addition, or subtraction atomically.
changeScopeModules
  :: Inventory typeVariable annotation
  -> SourceWorkspace
  -> ModuleChange
  -> [String]
  -> ReplScope
  -> Either Diagnostic ReplScope
changeScopeModules inventory workspace change references scope = do
  parsed <- traverse parseModuleReference references
  modules <- workspaceModuleMap workspace
  mapM_ (requireLoaded modules . snd) parsed
  let requested =
        [ ScopeModule ExplicitScope starred moduleName
        | (starred, moduleName) <- parsed
        ]
      candidate = case change of
        ReplaceModules -> requested
        AddModules -> foldl' addModuleEntry (scopeEntries scope) requested
        RemoveModules -> filter (not . removedBy parsed)
          $ scopeEntries scope
  compileScope inventory workspace candidate

-- | Resolve declarations shown by @:browse M@ or @:browse *M@.
moduleNamesForBrowse
  :: Inventory typeVariable annotation
  -> SourceWorkspace
  -> Bool
  -> String
  -> Either Diagnostic [Name]
moduleNamesForBrowse inventory workspace starred source = do
  moduleName <- checkedModuleName source
  modules <- workspaceModuleMap workspace
  _ <- requireLoaded modules moduleName
  let index = symbolIndex inventory modules
  views <- buildModuleViews index modules $ workspaceModules workspace
  view <- requireModuleView views moduleName
  pure $ if starred then moduleViewSearchNames view else moduleViewExports view

-- | Stable, human-readable import context for @:show imports@.
renderScopeImports :: ReplScope -> [String]
renderScopeImports scope = implicitLine ++ map render (scopeEntries scope)
 where
  implicitLine
    | replScopeHasImplicitPrelude scope = ["import Prelude -- implicit"]
    | otherwise = []
  render (ScopeImport source) = source
  render (ScopeModule origin starred moduleName) =
    "import " ++ (if starred then "*" else "")
    ++ SharedName.renderModuleName moduleName ++ suffix origin
  suffix ExplicitScope = ""
  suffix AutomaticScope = " -- automatic"

-- | Current @:module@ entries, preserving context order and star markers.
renderScopeModules :: ReplScope -> [String]
renderScopeModules = mapMaybe render . scopeEntries
 where
  render (ScopeImport _) = Nothing
  render (ScopeModule _ starred moduleName) = Just
    $ (if starred then "*" else "")
    ++ SharedName.renderModuleName moduleName

automaticEntries :: SourceWorkspace -> Either Diagnostic [ScopeEntry]
automaticEntries workspace = traverse automatic
  $ workspaceAutomaticTargetModules workspace
 where
  automatic (target, starred) = do
    moduleName <- checkedModuleName $ workspaceModuleName target
    Right $ ScopeModule AutomaticScope starred moduleName

addModuleEntry :: [ScopeEntry] -> ScopeEntry -> [ScopeEntry]
addModuleEntry entries incoming@(ScopeModule _ _ wanted) =
  case break (isModule wanted) entries of
    (before, _ : after) -> before ++ incoming : after
    _ -> entries ++ [incoming]
 where
  isModule moduleName (ScopeModule _ _ candidate) = moduleName == candidate
  isModule _ _ = False
addModuleEntry entries incoming = entries ++ [incoming]

removedBy :: [(Bool, ModuleName)] -> ScopeEntry -> Bool
removedBy references entry = case entry of
  ScopeModule _ _ moduleName -> moduleName `Set.member` removed
  ScopeImport source -> case parseImport source of
    Right declaration -> case importModuleName declaration of
      Right moduleName -> moduleName `Set.member` removed
      Left _ -> False
    Left _ -> False
 where
  removed = Set.fromList $ map snd references

parseModuleReference :: String -> Either Diagnostic (Bool, ModuleName)
parseModuleReference source = case dropWhile isSpace source of
  '*' : rest -> (,) True <$> checkedModuleName rest
  rest -> (,) False <$> checkedModuleName rest

checkedModuleName :: String -> Either Diagnostic ModuleName
checkedModuleName source
  | any isSpace token || null token = Left $ scopeDiagnostic
      "DJEX_REPL_MODULE_NAME" "invalid module name" (show source)
  | otherwise = case SharedName.mkModuleName token of
      Left failure -> Left $ scopeDiagnostic "DJEX_REPL_MODULE_NAME"
        "invalid module name" $ SharedName.renderNameError failure
      Right name -> Right name
 where
  token = trim source

parseImport :: String -> Either Diagnostic (HSE.ImportDecl HSE.SrcSpanInfo)
parseImport source = case HSE.parseImportDeclWithMode importParseMode source of
  HSE.ParseOk declaration -> do
    _ <- importModuleName declaration
    _ <- traverse checkedHseModuleName $ HSE.importAs declaration
    Right declaration
  HSE.ParseFailed location message -> Left $ scopeDiagnostic
    "DJEX_REPL_IMPORT_PARSE" "cannot parse import declaration"
    $ show location ++ ": " ++ message

importParseMode :: HSE.ParseMode
importParseMode = HSE.defaultParseMode
  { HSE.parseFilename = "<interactive>"
  , HSE.extensions = map HSE.EnableExtension
      [ HSE.PackageImports
      , HSE.ImportQualifiedPost
      , HSE.ExplicitNamespaces
      , HSE.SafeImports
      , HSE.PatternSynonyms
      ]
  }

compileScope
  :: Inventory typeVariable annotation
  -> SourceWorkspace
  -> [ScopeEntry]
  -> Either Diagnostic ReplScope
compileScope inventory workspace entries = do
  modules <- workspaceModuleMap workspace
  let index = symbolIndex inventory modules
  views <- buildModuleViews index modules $ workspaceModules workspace
  contributions <- traverse (entryContribution index modules views) entries
  implicit <- implicitPreludeContribution modules views entries
  let allContributions = maybe contributions (: contributions) implicit
      unqualified = ordNub
        $ concatMap contributionUnqualified allContributions
      search = filter (isSearchName index) $ ordNub
        $ concatMap contributionSearch allContributions
      qualified = mergeQualified
        $ concatMap contributionQualified allContributions
      aliases = ordNub $ concatMap contributionAliases allContributions
      starredModules = ordNub
        [ moduleName
        | ScopeModule _ True moduleName <- entries
        ]
      current = case starredModules of
        [moduleName] -> Just moduleName
        _ -> Nothing
  pure ReplScope
    { replScopeEntries = entries
    , replScopeUnqualifiedNames = unqualified
    , replScopeSearchNames = search
    , replScopeQualifiedNames = qualified
    , replScopeAliases = aliases
    , replScopeCurrentModule = current
    , replScopeHasImplicitPrelude = maybe False (const True) implicit
    }

data ScopeContribution = ScopeContribution
  { contributionUnqualified :: [Name]
  , contributionSearch :: [Name]
  , contributionQualified :: [(ModuleName, [Name])]
  , contributionAliases :: [(ModuleName, ModuleName)]
  }

entryContribution
  :: SymbolIndex
  -> Map ModuleName WorkspaceModule
  -> Map ModuleName ModuleView
  -> ScopeEntry
  -> Either Diagnostic ScopeContribution
entryContribution index modules views entry = case entry of
  ScopeModule _ starred moduleName -> do
    _ <- requireLoaded modules moduleName
    view <- requireModuleView views moduleName
    pure $ if starred
      then ScopeContribution
        (moduleViewUnqualified view)
        (moduleViewSearchNames view)
        (moduleViewQualified view)
        (moduleViewAliases view)
      else ScopeContribution
        (moduleViewExports view)
        (moduleViewExports view)
        [(moduleName, moduleViewExports view)]
        []
  ScopeImport source -> do
    declaration <- parseImport source
    rejectPackageImport declaration
    canonical <- importModuleName declaration
    _ <- requireLoaded modules canonical
    imported <- requireModuleView views canonical
    selected <- applyImportSpecs index (moduleViewExports imported)
      $ HSE.importSpecs declaration
    qualifier <- maybe (Right canonical) checkedHseModuleName
      $ HSE.importAs declaration
    let
        aliases
          | qualifier == canonical = []
          | otherwise = [(qualifier, canonical)]
    pure ScopeContribution
      { contributionUnqualified =
          if HSE.importQualified declaration then [] else selected
      , contributionSearch = selected
      , contributionQualified = [(qualifier, selected)]
      , contributionAliases = aliases
      }

-- GHCi implicitly imports Prelude only when no starred module supplies the
-- prompt's complete source scope. Djex can reproduce that import only when
-- Prelude is itself a loaded source module with checked declarations; it never
-- turns a same-named external/package inventory entry into prompt scope.
implicitPreludeContribution
  :: Map ModuleName WorkspaceModule
  -> Map ModuleName ModuleView
  -> [ScopeEntry]
  -> Either Diagnostic (Maybe ScopeContribution)
implicitPreludeContribution modules views entries = case
    SharedName.mkModuleName "Prelude" of
  Left _ -> pure Nothing
  Right prelude
    | any isStarredModule entries -> pure Nothing
    | any (mentionsModule prelude) entries -> pure Nothing
    | otherwise -> case Map.lookup prelude modules of
        Nothing -> pure Nothing
        Just _ -> do
          exports <- moduleViewExports <$> requireModuleView views prelude
          pure $ if null exports
            then Nothing
            else Just ScopeContribution
              { contributionUnqualified = exports
              , contributionSearch = exports
              , contributionQualified = [(prelude, exports)]
              , contributionAliases = []
              }
 where
  isStarredModule (ScopeModule _ True _) = True
  isStarredModule _ = False
  mentionsModule wanted (ScopeModule _ _ candidate) = wanted == candidate
  mentionsModule wanted (ScopeImport source) = case parseImport source of
    Right declaration -> importModuleName declaration == Right wanted
    Left _ -> False

workspaceModuleMap
  :: SourceWorkspace
  -> Either Diagnostic (Map ModuleName WorkspaceModule)
workspaceModuleMap workspace = fmap Map.fromList
  $ traverse pair $ workspaceModules workspace
 where
  pair target = do
    moduleName <- checkedModuleName $ workspaceModuleName target
    pure (moduleName, target)

requireLoaded
  :: Map ModuleName WorkspaceModule
  -> ModuleName
  -> Either Diagnostic WorkspaceModule
requireLoaded modules moduleName = case Map.lookup moduleName modules of
  Just target -> Right target
  Nothing -> Left $ scopeDiagnostic "DJEX_REPL_MODULE_NOT_LOADED"
    "module is not loaded" $ SharedName.renderModuleName moduleName

requireModuleView
  :: Map ModuleName ModuleView
  -> ModuleName
  -> Either Diagnostic ModuleView
requireModuleView views moduleName = case Map.lookup moduleName views of
  Just view -> Right view
  Nothing -> Left $ scopeDiagnostic "DJEX_REPL_SCOPE_INTERNAL"
    "loaded module has no validated scope"
    $ SharedName.renderModuleName moduleName

-- Shared 'Name' deliberately identifies a canonical spelling, not GHC's
-- separate type/value namespace entities. The role set recovers enough source
-- information for @type@, @pattern@, constructors, and class methods, but a
-- legal same-spelled type and constructor remain one exact shared identity.
-- Filtering that identity more finely would require changing the common
-- declaration model rather than guessing from syntax here.
data SymbolRole
  = TypeSymbol
  | BundledOwnerSymbol
  | ValueSymbol
  | ConstructorSymbol
  deriving (Eq, Ord, Show)

data SymbolIndex = SymbolIndex
  { symbolsByModule :: Map ModuleName [Name]
  , symbolRoles :: Map Name (Set SymbolRole)
  , symbolChildren :: Map Name [Name]
  }

symbolIndex
  :: Inventory typeVariable annotation
  -> Map ModuleName WorkspaceModule
  -> SymbolIndex
symbolIndex inventory modules = foldl' addRecordFields declarationsIndex
  $ Map.toList modules
 where
  emptyIndex = SymbolIndex Map.empty Map.empty Map.empty
  declarationsIndex = foldl' addDeclaration emptyIndex
    $ environmentDeclarations $ inventoryEnvironment inventory

  addDeclaration index declaration = case declaration of
    TypeSynonymDeclaration _ name _ _ -> add TypeSymbol name index
    AbstractTypeDeclaration _ name _ -> add TypeSymbol name index
    ValueDeclaration signature -> add ValueSymbol (valueName signature) index
    DataTypeDeclaration _ parent _ constructors ->
      addChildren parent (map constructorName constructors)
        $ foldl' (flip $ addRoles [ValueSymbol, ConstructorSymbol])
            (addRoles [TypeSymbol, BundledOwnerSymbol] parent index)
            $ map constructorName constructors
    ClassDeclaration _ parent _ _ methods ->
      addChildren parent (map valueName methods)
        $ foldl' (flip $ addRoles [ValueSymbol])
            (addRoles [TypeSymbol, BundledOwnerSymbol] parent index)
            $ map valueName methods
    InstanceDeclaration{} -> index

  add role = addRoles [role]
  addRoles roles name index = index
    { symbolsByModule = case SharedName.nameModule name of
        Nothing -> symbolsByModule index
        Just moduleName -> Map.insertWith appendOld moduleName [name]
          $ symbolsByModule index
    , symbolRoles = Map.insertWith Set.union name (Set.fromList roles)
        $ symbolRoles index
    }
  addChildren parent children index = index
    { symbolChildren = Map.insertWith appendOld parent children
        $ symbolChildren index
    }

  -- The neutral declaration layer deliberately models record selectors as
  -- ordinary values. Retain that backend-independent representation and use
  -- the already parsed source snapshot only to recover the parent relation
  -- needed by Haskell import/export forms such as @Record(..)@ and
  -- @Record(field)@. Every parent and field is intersected with the checked
  -- inventory, so rejected syntax can never leak into prompt scope.
  addRecordFields index (moduleName, target) = foldl' addRecord index
    $ recordFieldGroups $ workspaceModuleSyntax target
   where
    available = moduleSymbols index moduleName
    matches role spelling =
      [ name
      | name <- available
      , SharedName.nameSpelling name == Just spelling
      , role `Set.member` Map.findWithDefault Set.empty name
          (symbolRoles index)
      ]
    addRecord current (parentSpelling, fieldSpellings) = case
        matches TypeSymbol parentSpelling of
      [parent] -> addChildren parent
        (ordNub $ concatMap (matches ValueSymbol) fieldSpellings) current
      _ -> current
  appendOld new old = old ++ new

recordFieldGroups
  :: HSE.Module HSE.SrcSpanInfo
  -> [(String, [String])]
recordFieldGroups syntax = concatMap declarationGroups declarations
 where
  declarations = case syntax of
    HSE.Module _ _ _ _ items -> items
    HSE.XmlHybrid _ _ _ _ items _ _ _ _ -> items
    HSE.XmlPage{} -> []

  declarationGroups declaration = case declaration of
    HSE.DataDecl _ _ _ headSyntax constructors _ ->
      oneGroup headSyntax $ concatMap constructorFields constructors
    HSE.GDataDecl _ _ _ headSyntax _ constructors _ ->
      oneGroup headSyntax $ concatMap gadtFields constructors
    _ -> []

  oneGroup headSyntax fields = case ordNub fields of
    [] -> []
    names -> [(hseNameText $ declarationHeadName headSyntax, names)]

  constructorFields (HSE.QualConDecl _ _ _ constructor) = case constructor of
    HSE.RecDecl _ _ fields -> concatMap fieldNames fields
    _ -> []

  gadtFields (HSE.GadtDecl _ _ _ _ fields _) =
    maybe [] (concatMap fieldNames) fields

  fieldNames (HSE.FieldDecl _ names _) = map hseNameText names

declarationHeadName :: HSE.DeclHead annotation -> HSE.Name annotation
declarationHeadName headSyntax = case headSyntax of
  HSE.DHead _ name -> name
  HSE.DHInfix _ _ name -> name
  HSE.DHParen _ nested -> declarationHeadName nested
  HSE.DHApp _ nested _ -> declarationHeadName nested

moduleSymbols :: SymbolIndex -> ModuleName -> [Name]
moduleSymbols index moduleName = ordNub
  $ Map.findWithDefault [] moduleName $ symbolsByModule index

isSearchName :: SymbolIndex -> Name -> Bool
isSearchName index name = not $ Set.null $ Set.intersection searchable
  $ Map.findWithDefault Set.empty name $ symbolRoles index
 where
  searchable = Set.fromList [ValueSymbol, ConstructorSymbol]

data ModuleView = ModuleView
  { moduleViewLocal :: [Name]
  , moduleViewUnqualified :: [Name]
  , moduleViewSearchNames :: [Name]
  , moduleViewQualified :: [(ModuleName, [Name])]
  , moduleViewAliases :: [(ModuleName, ModuleName)]
  , moduleViewExports :: [Name]
  }

data ImportView = ImportView
  { importViewCanonical :: ModuleName
  , importViewQualifier :: ModuleName
  , importViewIsQualified :: Bool
  , importViewNames :: [Name]
  }

buildModuleViews
  :: SymbolIndex
  -> Map ModuleName WorkspaceModule
  -> [WorkspaceModule]
  -> Either Diagnostic (Map ModuleName ModuleView)
buildModuleViews index modules targets = snd
  <$> runStateT
      (mapM_ (buildModuleViewCached index modules []) targets)
      Map.empty

type ModuleViewBuilder =
  StateT (Map ModuleName ModuleView) (Either Diagnostic)

-- Module views form a dependency DAG after Workspace has validated ordinary
-- cycles. Memoizing completed nodes keeps a diamond-shaped import graph linear
-- instead of recursively rebuilding the same export surface along every path.
buildModuleViewCached
  :: SymbolIndex
  -> Map ModuleName WorkspaceModule
  -> [ModuleName]
  -> WorkspaceModule
  -> ModuleViewBuilder ModuleView
buildModuleViewCached index modules stack target = do
  moduleName <- lift $ checkedModuleName $ workspaceModuleName target
  cached <- get
  case Map.lookup moduleName cached of
    Just view -> pure view
    Nothing -> do
      if moduleName `elem` stack
        then lift $ Left $ withSource (workspaceModulePath target)
          $ scopeDiagnostic "DJEX_REPL_IMPORT_CYCLE"
              "cannot construct a scope through an import cycle"
              $ intercalate " -> "
              $ map SharedName.renderModuleName $ reverse $ moduleName : stack
        else pure ()
      (moduleHead, pragmas, sourceImports) <- lift $ moduleParts target
      imports <- traverse
        (resolveSourceImport index modules (workspaceModulePath target)
          $ moduleName : stack)
        sourceImports
      implicit <- sourceImplicitPrelude index modules (moduleName : stack)
        moduleName pragmas sourceImports
      let allImports = maybe imports (: imports) implicit
          local = moduleSymbols index moduleName
          unqualified = ordNub $ local ++ concat
            [ importViewNames item
            | item <- allImports
            , not $ importViewIsQualified item
            ]
          search = ordNub $ local ++ concatMap importViewNames allImports
          qualified = mergeQualified $ (moduleName, local) :
            [ (importViewQualifier item, importViewNames item)
            | item <- allImports
            ]
          aliases = ordNub
            [ (importViewQualifier item, importViewCanonical item)
            | item <- allImports
            , importViewQualifier item /= importViewCanonical item
            ]
          provisional = ModuleView local unqualified search
            qualified aliases []
      exports <- lift
        $ atSource (workspaceModulePath target)
        $ resolveModuleExports index provisional allImports moduleHead
      let uniqueExports = ordNub exports
      lift $ validateExportSurface index target uniqueExports
      let completed = provisional {moduleViewExports = uniqueExports}
      modify $ Map.insert moduleName completed
      pure completed

moduleParts
  :: WorkspaceModule
  -> Either Diagnostic
      ( Maybe (HSE.ModuleHead HSE.SrcSpanInfo)
      , [HSE.ModulePragma HSE.SrcSpanInfo]
      , [HSE.ImportDecl HSE.SrcSpanInfo]
      )
moduleParts target = case workspaceModuleSyntax target of
  HSE.Module _ header pragmas imports _ -> Right (header, pragmas, imports)
  HSE.XmlHybrid _ header pragmas imports _ _ _ _ _ ->
    Right (header, pragmas, imports)
  HSE.XmlPage{} -> Left $ withSource (workspaceModulePath target)
    $ scopeDiagnostic "DJEX_REPL_XML_MODULE"
        "XML page modules have no supported interactive scope" ""

resolveSourceImport
  :: SymbolIndex
  -> Map ModuleName WorkspaceModule
  -> FilePath
  -> [ModuleName]
  -> HSE.ImportDecl HSE.SrcSpanInfo
  -> ModuleViewBuilder ImportView
resolveSourceImport index modules importerPath stack declaration = do
  -- Package identity is not represented by the shared canonical Name. If a
  -- local module has the same spelling as the package module, falling back to
  -- that local inventory would silently bind the wrong declarations.
  lift $ atSource importerPath $ rejectPackageImport declaration
  canonical <- lift $ atSource importerPath $ importModuleName declaration
  qualifier <- lift $ atSource importerPath
    $ maybe (Right canonical) checkedHseModuleName $ HSE.importAs declaration
  available <- case Map.lookup canonical modules of
    Just target
      -- A SOURCE import is an interface edge used specifically to break a
      -- source cycle. Workspace does not load @.hs-boot@ files, so following
      -- the ordinary module here would recreate the cycle that SOURCE broke.
      | not (HSE.importSrc declaration) ->
          moduleViewExports
            <$> buildModuleViewCached index modules stack target
    Just _ -> pure $ moduleSymbols index canonical
    -- This source-only session has no package database. An unresolved import
    -- therefore contributes no declarations; the workspace warning explains
    -- that boundary without manufacturing a package export surface.
    Nothing -> pure []
  selected <- lift $ atSource importerPath
    $ applyImportSpecs index available $ HSE.importSpecs declaration
  pure ImportView
    { importViewCanonical = canonical
    , importViewQualifier = qualifier
    , importViewIsQualified = HSE.importQualified declaration
    , importViewNames = selected
    }

rejectPackageImport
  :: HSE.ImportDecl annotation
  -> Either Diagnostic ()
rejectPackageImport declaration = case HSE.importPkg declaration of
  Just packageName -> Left $ scopeDiagnostic "DJEX_REPL_IMPORT_PACKAGE"
    "package-qualified imports are not supported by the source workspace"
    $ "package " ++ show packageName
  Nothing -> Right ()

atSource :: FilePath -> Either Diagnostic value -> Either Diagnostic value
atSource source = either (Left . withSource source) Right

sourceImplicitPrelude
  :: SymbolIndex
  -> Map ModuleName WorkspaceModule
  -> [ModuleName]
  -> ModuleName
  -> [HSE.ModulePragma HSE.SrcSpanInfo]
  -> [HSE.ImportDecl HSE.SrcSpanInfo]
  -> ModuleViewBuilder (Maybe ImportView)
sourceImplicitPrelude index modules stack current pragmas imports = case
    SharedName.mkModuleName "Prelude" of
  Left _ -> pure Nothing
  Right prelude
    | current == prelude
        || any ((== Right prelude) . importModuleName) imports
        || any disablesImplicitPrelude pragmas -> pure Nothing
    | otherwise -> case Map.lookup prelude modules of
        Nothing -> pure Nothing
        Just target -> do
          available <- moduleViewExports
            <$> buildModuleViewCached index modules stack target
          pure $ if null available then Nothing else Just ImportView
            { importViewCanonical = prelude
            , importViewQualifier = prelude
            , importViewIsQualified = False
            , importViewNames = available
            }
 where
  disablesImplicitPrelude (HSE.LanguagePragma _ names) =
    any ((== "NoImplicitPrelude") . hseNameText) names
  disablesImplicitPrelude _ = False

resolveModuleExports
  :: SymbolIndex
  -> ModuleView
  -> [ImportView]
  -> Maybe (HSE.ModuleHead HSE.SrcSpanInfo)
  -> Either Diagnostic [Name]
resolveModuleExports _ view _ Nothing = Right $ moduleViewLocal view
resolveModuleExports _ view _
    (Just (HSE.ModuleHead _ _ _ Nothing)) = Right $ moduleViewLocal view
resolveModuleExports index view imports
    (Just (HSE.ModuleHead _ _ _ (Just (HSE.ExportSpecList _ specs)))) =
  fmap concat $ traverse resolve specs
 where
  resolve spec = case spec of
    HSE.EVar _ qname -> selectQName index [ValueSymbol] view qname
      $ HSE.prettyPrint spec
    HSE.EAbs _ namespace qname -> selectQName index
      (namespaceRoles namespace) view qname $ HSE.prettyPrint spec
    HSE.EThingWith _ wildcard qname children -> do
      parent <- selectQName index [BundledOwnerSymbol] view qname
        $ HSE.prettyPrint spec
      case parent of
        [oneParent] -> do
          candidateNames <- qNameCandidates view qname
          selectedChildren <- selectChildren index candidateNames
            oneParent children $ HSE.prettyPrint spec
          let wildcardChildren = case wildcard of
                HSE.NoWildcard _ -> []
                HSE.EWildcard _ _ -> availableChildren index
                  candidateNames oneParent
          pure $ oneParent : ordNub (wildcardChildren ++ selectedChildren)
        _ -> pure parent
    HSE.EModuleContents _ syntaxModule -> do
      wanted <- checkedHseModuleName syntaxModule
      let matching =
            [ importViewNames item
            | item <- imports
            , wanted == importViewCanonical item
                || wanted == importViewQualifier item
            ]
      if null matching
        then Left $ scopeDiagnostic "DJEX_REPL_EXPORT_NOT_IN_SCOPE"
          "module re-export is not in scope" $ HSE.prettyPrint spec
        else Right $ ordNub $ concat matching

-- Haskell has separate type and value export namespaces, but two different
-- entities in the same namespace may not share one exported occurrence. This
-- most often arises through two @module X@ re-exports whose aliases expose
-- colliding declarations. Exact duplicate exports remain harmless.
validateExportSurface
  :: SymbolIndex
  -> WorkspaceModule
  -> [Name]
  -> Either Diagnostic ()
validateExportSurface index target exports = case firstConflict of
  Nothing -> Right ()
  Just (occurrence, left, right) -> Left
    $ withSource (workspaceModulePath target)
    $ scopeDiagnostic "DJEX_REPL_EXPORT_AMBIGUOUS"
        "module export is ambiguous"
        ( occurrence ++ " could denote "
            ++ SharedName.renderCanonical left ++ " or "
            ++ SharedName.renderCanonical right
        )
 where
  groups = Map.elems $ Map.fromListWith appendOld
    [ (occurrence, [name])
    | name <- exports
    , Just occurrence <- [SharedName.nameSpelling name]
    ]
  firstConflict = firstPair
    [ (occurrence, left, right)
    | names <- groups
    , (left, right) <- unorderedPairs names
    , left /= right
    , sameNamespace left right
    , Just occurrence <- [SharedName.nameSpelling left]
    ]
  firstPair [] = Nothing
  firstPair (pair : _) = Just pair
  unorderedPairs [] = []
  unorderedPairs (name : names) =
    [(name, other) | other <- names] ++ unorderedPairs names
  sameNamespace left right = typeOverlap || valueOverlap
   where
    leftRoles = Map.findWithDefault Set.empty left $ symbolRoles index
    rightRoles = Map.findWithDefault Set.empty right $ symbolRoles index
    typeOverlap = TypeSymbol `Set.member` leftRoles
      && TypeSymbol `Set.member` rightRoles
    valueOverlap = hasValueRole leftRoles && hasValueRole rightRoles
    hasValueRole = not . Set.null
      . Set.intersection valueNamespaceRoles
  valueNamespaceRoles = Set.fromList [ValueSymbol, ConstructorSymbol]
  appendOld new old = old ++ new

selectQName
  :: SymbolIndex
  -> [SymbolRole]
  -> ModuleView
  -> HSE.QName HSE.SrcSpanInfo
  -> String
  -> Either Diagnostic [Name]
selectQName index roles view qname rendered = case qname of
  HSE.UnQual _ name -> select (hseNameText name)
  HSE.Qual _ _ name -> select (hseNameText name)
  HSE.Special _ special -> select $ HSE.prettyPrint special
 where
  select occurrence = do
    candidates <- qNameCandidates view qname
    selectUnique index roles candidates occurrence rendered

qNameCandidates
  :: ModuleView
  -> HSE.QName HSE.SrcSpanInfo
  -> Either Diagnostic [Name]
qNameCandidates view qname = case qname of
  HSE.UnQual _ _ -> Right $ moduleViewUnqualified view
  HSE.Qual _ syntaxModule _ -> do
    wanted <- checkedHseModuleName syntaxModule
    Right $ concat
      [ names
      | (qualifier, names) <- moduleViewQualified view
      , qualifier == wanted
      ]
  HSE.Special _ _ -> Right $ moduleViewSearchNames view

applyImportSpecs
  :: SymbolIndex
  -> [Name]
  -> Maybe (HSE.ImportSpecList HSE.SrcSpanInfo)
  -> Either Diagnostic [Name]
applyImportSpecs _ available Nothing = Right available
applyImportSpecs index available
    (Just (HSE.ImportSpecList _ hiding specs)) = do
  selected <- ordNub . concat <$> traverse select specs
  let selectedSet = Set.fromList selected
  pure $ if hiding
    then filter (`Set.notMember` selectedSet) available
    else filter (`Set.member` selectedSet) available
 where
  select spec = case spec of
    HSE.IVar _ name -> selectUnique index [ValueSymbol] available
      (hseNameText name) $ HSE.prettyPrint spec
    HSE.IAbs _ namespace name -> selectUnique index
      (namespaceRoles namespace) available (hseNameText name)
      $ HSE.prettyPrint spec
    HSE.IThingAll _ name -> do
      parents <- selectUnique index [BundledOwnerSymbol] available
        (hseNameText name)
        $ HSE.prettyPrint spec
      pure $ parents ++ concatMap (availableChildren index available) parents
    HSE.IThingWith _ name children -> do
      parents <- selectUnique index [BundledOwnerSymbol] available
        (hseNameText name)
        $ HSE.prettyPrint spec
      case parents of
        [parent] -> (parent :) <$> selectChildren index available parent children
          (HSE.prettyPrint spec)
        _ -> pure parents

selectUnique
  :: SymbolIndex
  -> [SymbolRole]
  -> [Name]
  -> String
  -> String
  -> Either Diagnostic [Name]
selectUnique index roles available occurrence rendered = case matches of
  [] -> Left $ scopeDiagnostic "DJEX_REPL_IMPORT_NAME"
    "import or export item is not available" rendered
  [name] -> Right [name]
  names -> Left $ scopeDiagnostic "DJEX_REPL_IMPORT_AMBIGUOUS"
    "import or export item is ambiguous"
    $ rendered ++ " could denote "
    ++ intercalate ", " (map SharedName.renderCanonical names)
 where
  accepted = Set.fromList roles
  matches = ordNub
    [ name
    | name <- available
    , SharedName.nameSpelling name == Just occurrence
    , not $ Set.null $ Set.intersection accepted
        $ Map.findWithDefault Set.empty name $ symbolRoles index
    ]

selectChildren
  :: SymbolIndex
  -> [Name]
  -> Name
  -> [HSE.CName HSE.SrcSpanInfo]
  -> String
  -> Either Diagnostic [Name]
selectChildren index available parent children rendered =
  traverse select children
 where
  candidates = availableChildren index available parent
  select child = case
      [ name
      | name <- candidates
      , SharedName.nameSpelling name == Just (childNameText child)
      ] of
    [name] -> Right name
    [] -> Left $ scopeDiagnostic "DJEX_REPL_IMPORT_CHILD"
      "constructor or class method is not available from its parent"
      $ rendered ++ ": " ++ childNameText child
    names -> Left $ scopeDiagnostic "DJEX_REPL_IMPORT_AMBIGUOUS"
      "constructor or class method is ambiguous"
      $ intercalate ", " $ map SharedName.renderCanonical names

availableChildren :: SymbolIndex -> [Name] -> Name -> [Name]
availableChildren index available parent = filter (`Set.member` availableSet)
  $ Map.findWithDefault [] parent $ symbolChildren index
 where
  availableSet = Set.fromList available

namespaceRoles :: HSE.Namespace annotation -> [SymbolRole]
namespaceRoles namespace = case namespace of
  HSE.PatternNamespace _ -> [ConstructorSymbol]
  HSE.TypeNamespace _ -> [TypeSymbol]
  HSE.NoNamespace _ -> [TypeSymbol]

mergeQualified :: [(ModuleName, [Name])] -> [(ModuleName, [Name])]
mergeQualified = foldl' insert []
 where
  insert [] pair = [pair]
  insert ((key, old) : rest) pair@(wanted, names)
    | key == wanted = (key, ordNub $ old ++ names) : rest
    | otherwise = (key, old) : insert rest pair

importModuleName
  :: HSE.ImportDecl annotation
  -> Either Diagnostic ModuleName
importModuleName = checkedHseModuleName . HSE.importModule

checkedHseModuleName
  :: HSE.ModuleName annotation
  -> Either Diagnostic ModuleName
checkedHseModuleName (HSE.ModuleName _ source) = checkedModuleName source

hseNameText :: HSE.Name annotation -> String
hseNameText name = case name of
  HSE.Ident _ source -> source
  HSE.Symbol _ source -> source

childNameText :: HSE.CName annotation -> String
childNameText child = case child of
  HSE.VarName _ name -> hseNameText name
  HSE.ConName _ name -> hseNameText name

scopeDiagnostic :: String -> String -> String -> Diagnostic
scopeDiagnostic code message detail =
  contextualDiagnostic Error code message detail

ordNub :: Ord value => [value] -> [value]
ordNub = reverse . snd . foldl' step (Set.empty, [])
 where
  step (seen, values) value
    | value `Set.member` seen = (seen, values)
    | otherwise = (Set.insert value seen, value : values)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
