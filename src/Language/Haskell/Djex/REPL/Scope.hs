
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
  , ScopeNamespace (..)
  , scopeFromWorkspace
  , revalidateScope
  , addScopeImport
  , changeScopeModules
  , scopeEntries
  , parseScopeImport
  , scopeUnqualifiedNames
  , scopeUnqualifiedTypeNames
  , scopeUnqualifiedValueNames
  , scopeSearchNames
  , scopeQualifiedNames
  , scopeQualifiedTypeNames
  , scopeQualifierAliases
  , scopeCurrentModule
  , scopeExferenceQueryScope
  , resolveScopeNameAmong
  , renderScopeImports
  , moduleNamesForBrowse
  , workspaceRecordProjections
  ) where

import Data.Foldable (traverse_)
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, modify, runStateT)
import Data.Char (isSpace)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set)

import qualified Language.Haskell.Exts.Extension as HSE
import qualified Language.Haskell.Exts.Parser as HSE
import qualified Language.Haskell.Exts.Pretty as HSE
import qualified Language.Haskell.Exts.SrcLoc as HSE
import qualified Language.Haskell.Exts.Syntax as HSE

import Language.Haskell.Djex.Internal.ImplicitPrelude
  ( implicitPreludeEnabled )
import Language.Haskell.Djex.HaskellSrc (ExferenceQueryScope (..))
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
import Language.Haskell.Djex.Text (trim)
import qualified Language.Haskell.Synthesis.Collection as SharedCollection
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
-- is revalidated transactionally before it can enter a t'ReplScope'.
data ScopeEntry
  = ScopeModule ScopeOrigin Bool ModuleName
    -- ^ Origin, starred/full-top-level flag, and canonical module.
  | ScopeImport String
    -- ^ A complete Haskell import declaration.
  deriving (Eq, Ord, Show)

-- | A Haskell occurrence may denote distinct entities in the type and value
-- namespaces. 'AnyScope' is reserved for polymorphic inspection commands such
-- as @:info@; type parsing, expression typing, and synthesis always request an
-- exact namespace.
data ScopeNamespace
  = TypeScope
  | ValueScope
  | AnyScope
  deriving (Eq, Ord, Show)

-- | The validated prompt name scope: the ordered module-context and import
-- entries together with the name surfaces they admit (unqualified at the
-- prompt, visible to backend search, and per written qualifier), the
-- qualifier aliases, and the current module. Construct it only through
-- 'scopeFromWorkspace' and the transactional update functions.
--
-- The separate projections are intentional. A qualified import adds bindings
-- to backend search, but it must not make a type constructor legal bare at the
-- prompt. Keeping both sets avoids recreating that distinction downstream.
data ReplScope = ReplScope
  { replScopeEntries :: [ScopeEntry]
  , replScopeUnqualifiedSurface :: NameSurface
  , replScopeSearchSurface :: NameSurface
  , replScopeQualifiedSurfaces :: [(ModuleName, NameSurface)]
  , replScopeAliases :: [(ModuleName, ModuleName)]
  , replScopeCurrentModule :: Maybe ModuleName
  , replScopeHasImplicitPrelude :: Bool
  }
  deriving (Eq, Show)

-- | The module-context and import entries of the scope, in prompt order.
scopeEntries :: ReplScope -> [ScopeEntry]
scopeEntries = replScopeEntries

-- | Exact canonical names that may be written without a qualifier.
scopeUnqualifiedNames :: ReplScope -> [Name]
scopeUnqualifiedNames = surfaceNames . replScopeUnqualifiedSurface

-- | Canonical type and class identities admitted without a qualifier.
scopeUnqualifiedTypeNames :: ReplScope -> [Name]
scopeUnqualifiedTypeNames = surfaceNamesIn TypeScope
  . replScopeUnqualifiedSurface

-- | Canonical values and constructors admitted without a qualifier.
scopeUnqualifiedValueNames :: ReplScope -> [Name]
scopeUnqualifiedValueNames = surfaceNamesIn ValueScope
  . replScopeUnqualifiedSurface

-- | Exact canonical values and constructors available to synthesis, whether
-- admitted unqualified or through a written qualifier.
scopeSearchNames :: ReplScope -> [Name]
scopeSearchNames = surfaceNamesIn ValueScope . replScopeSearchSurface

-- | Exact names admitted under each written qualifier.  This is separate from
-- 'scopeQualifierAliases': an alias alone cannot encode an explicit import
-- list or @hiding@ clause.
scopeQualifiedNames :: ReplScope -> [(ModuleName, [Name])]
scopeQualifiedNames = map (fmap surfaceNames) . replScopeQualifiedSurfaces

-- | Exact type/class identities admitted under each written qualifier.
scopeQualifiedTypeNames :: ReplScope -> [(ModuleName, [Name])]
scopeQualifiedTypeNames = map (fmap $ surfaceNamesIn TypeScope)
  . replScopeQualifiedSurfaces

-- | Non-canonical written qualifier to canonical defining module.
scopeQualifierAliases :: ReplScope -> [(ModuleName, ModuleName)]
scopeQualifierAliases = replScopeAliases

-- | The sole starred current module, when there is exactly one.  Returning
-- 'Nothing' for several starred modules preserves genuine ambiguity instead
-- of giving one module an arbitrary lookup priority.
scopeCurrentModule :: ReplScope -> Maybe ModuleName
scopeCurrentModule = replScopeCurrentModule

-- | Project the prompt's exact type namespace once for every scoped parser.
-- Keeping this beside 'ReplScope' prevents synthesis queries and @:type@
-- annotations from drifting to subtly different import/qualification rules.
scopeExferenceQueryScope :: ReplScope -> ExferenceQueryScope
scopeExferenceQueryScope context = ExferenceQueryScope
  { exferenceQueryCurrentModule = scopeCurrentModule context
  , exferenceQueryVisibleNames = scopeUnqualifiedTypeNames context
  , exferenceQueryModuleAliases = scopeQualifierAliases context
  , exferenceQueryQualifiedNames = scopeQualifiedTypeNames context
  }

-- | Resolve one prompt spelling inside a caller-selected Haskell namespace.
--
-- t'ReplScope' carries both namespaces in one route surface but retains the
-- admitted namespace set for each identity. Consumers such as @:info@ and
-- @:type@ select the namespace they own before ambiguity is decided; otherwise
-- the legal pair @data T = T@ would make the constructor leak through a
-- type-only import.
-- Canonically qualified names may address any loaded declaration in the
-- selected namespace, while aliases remain limited by their exact import
-- surfaces.
resolveScopeNameAmong
  :: ScopeNamespace
  -> Set Name
  -> ReplScope
  -> Name
  -> Either String Name
resolveScopeNameAmong namespace available context source = case
    SharedName.nameModule source of
  Nothing -> case SharedName.nameSpecial source of
    Just _ -> Right source
    Nothing -> chooseUnqualified
  Just qualifier -> case qualifiedCandidates qualifier of
    [name] -> Right name
    _ : _ : _ -> Left $ "ambiguous qualified name "
      ++ SharedName.renderCanonical source ++ "; matches "
      ++ intercalate ", "
          (map SharedName.renderCanonical $ qualifiedCandidates qualifier)
    [] -> case
        [ canonical
        | (alias, canonical) <- scopeQualifierAliases context
        , alias == qualifier
        ] of
      []
        | source `Set.member` available -> Right source
        | otherwise -> Left $ "qualified name "
            ++ SharedName.renderCanonical source ++ " is not loaded"
      [_] -> chooseAlias qualifier
      modules -> Left $ "ambiguous module qualifier "
        ++ SharedName.renderModuleName qualifier ++ "; matches "
        ++ intercalate ", " (map SharedName.renderModuleName modules)
 where
  sameOccurrence candidate =
    SharedName.nameOccurrence candidate == SharedName.nameOccurrence source
  selected = filter (`Set.member` available)
  unqualified = selected $ filter sameOccurrence
    $ surfaceNamesIn namespace $ replScopeUnqualifiedSurface context
  local = case scopeCurrentModule context of
    Nothing -> []
    Just current -> filter ((== Just current) . SharedName.nameModule)
      unqualified
  chooseUnqualified = case if null local then unqualified else local of
    [name] -> Right name
    [] -> Left $ "name " ++ SharedName.renderCanonical source
      ++ " is not in scope"
    names -> Left $ "ambiguous unqualified name "
      ++ SharedName.renderCanonical source ++ "; matches "
      ++ intercalate ", " (map SharedName.renderCanonical names)
  chooseAlias qualifier = case qualifiedCandidates qualifier of
    [name] -> Right name
    [] -> Left $ "qualified name " ++ SharedName.renderCanonical source
      ++ " is not in scope"
    names -> Left $ "ambiguous qualified name "
      ++ SharedName.renderCanonical source ++ "; matches "
      ++ intercalate ", " (map SharedName.renderCanonical names)
  qualifiedCandidates qualifier = selected
    [ name
    | (written, surface) <- replScopeQualifiedSurfaces context
    , written == qualifier
    , name <- surfaceNamesIn namespace surface
    , sameOccurrence name
    ]

-- | Construct the initial prompt scope, including GHCi's automatic starred
-- context for the most recent source-interpreted target.
scopeFromWorkspace
  :: Inventory typeVariable annotation
  -> SourceWorkspace
  -> Either Diagnostic ReplScope
scopeFromWorkspace inventory workspace = do
  entries <- automaticEntries workspace
  compileScope inventory workspace entries

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
  ScopeImport source -> case parseScopeImport source of
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
  declaration <- parseScopeImport source
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
  pure $ surfaceNames $ if starred
    then moduleViewSearch view
    else moduleViewExports view

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
  ScopeImport source -> case parseScopeImport source of
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

-- | Parse the normalized import text retained by 'ScopeImport'. The parser is
-- shared with real-GHC evaluation so synthesis and execution cannot disagree
-- about qualified aliases or explicit import surfaces.
parseScopeImport
  :: String
  -> Either Diagnostic (HSE.ImportDecl HSE.SrcSpanInfo)
parseScopeImport source = case
    HSE.parseImportDeclWithMode importParseMode source of
  HSE.ParseOk declaration -> do
    _ <- importModuleName declaration
    _ <- traverse_ checkedHseModuleName $ HSE.importAs declaration
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
      unqualified = mergeSurfaces
        $ map contributionUnqualified allContributions
      search = mergeSurfaces $ map contributionSearch allContributions
      qualified = mergeQualifiedSurfaces
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
    , replScopeUnqualifiedSurface = unqualified
    , replScopeSearchSurface = search
    , replScopeQualifiedSurfaces = qualified
    , replScopeAliases = aliases
    , replScopeCurrentModule = current
    , replScopeHasImplicitPrelude = isJust implicit
    }

data ScopeContribution = ScopeContribution
  { contributionUnqualified :: NameSurface
  , contributionSearch :: NameSurface
  , contributionQualified :: [(ModuleName, NameSurface)]
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
        (moduleViewSearch view)
        (moduleViewQualified view)
        (moduleViewAliases view)
      else ScopeContribution
        (moduleViewExports view)
        (moduleViewExports view)
        [(moduleName, moduleViewExports view)]
        []
  ScopeImport source -> do
    declaration <- parseScopeImport source
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
          if HSE.importQualified declaration
            then emptySurface
            else selected
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
          pure $ if null $ surfaceNames exports
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
  mentionsModule wanted (ScopeImport source) = case parseScopeImport source of
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

-- Shared 'Name' deliberately identifies a canonical spelling rather than a
-- GHC namespace entity. Source roles recover which namespaces that identity
-- inhabits; 'NameSurface' below retains the admitted subset on every route so
-- an import of @T@ cannot accidentally expose a same-named constructor.
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

  addRecordFields index (moduleName, target) = foldl' addRecord index
    $ moduleRecordSelectorGroups index moduleName target
   where
    addRecord current (parent, selectors) = addChildren parent selectors current
  appendOld new old = old ++ new

-- The neutral declaration layer deliberately models record selectors as
-- ordinary values. Retain that backend-independent representation and use
-- the already parsed source snapshot only to recover the parent relation
-- needed by Haskell import/export forms such as @Record(..)@ and
-- @Record(field)@, or by consumers that must distinguish field projections
-- from ordinary values. Every parent, constructor, and field is intersected
-- with the checked inventory, so rejected syntax can never leak names.
moduleRecordSelectorGroups
  :: SymbolIndex
  -> ModuleName
  -> WorkspaceModule
  -> [(Name, [Name])]
moduleRecordSelectorGroups index moduleName target =
  [ ( parent
    , ordNub $ concatMap (symbolMatches index moduleName ValueSymbol)
        $ concatMap snd constructors
    )
  | (parentSpelling, constructors) <-
      recordConstructorGroups $ workspaceModuleSyntax target
  , [parent] <- [symbolMatches index moduleName TypeSymbol parentSpelling]
  ]

symbolMatches :: SymbolIndex -> ModuleName -> SymbolRole -> String -> [Name]
symbolMatches index moduleName role spelling =
  [ name
  | name <- moduleSymbols index moduleName
  , SharedName.nameSpelling name == Just spelling
  , role `Set.member` Map.findWithDefault Set.empty name (symbolRoles index)
  ]

-- | Every record datatype in the workspace as canonical
-- @(parent, [(constructor, selectors in field order)])@ groups. A field
-- whose selector cannot be uniquely matched in the checked inventory drops
-- its whole constructor, so reported positions never silently shift. The
-- shared declaration model keeps selectors as plain values; the source
-- snapshot is the only witness of their record provenance.
workspaceRecordProjections
  :: Inventory typeVariable annotation
  -> SourceWorkspace
  -> [(Name, [(Name, [Name])])]
workspaceRecordProjections inventory workspace = case
    workspaceModuleMap workspace of
  Left _ -> []
  Right modules ->
    let index = symbolIndex inventory modules
    in
      [ (parent, positional)
      | (moduleName, target) <- Map.toList modules
      , (parentSpelling, constructors) <-
          recordConstructorGroups $ workspaceModuleSyntax target
      , [parent] <-
          [symbolMatches index moduleName TypeSymbol parentSpelling]
      , let positional =
              [ (constructor, selectors)
              | (constructorSpelling, fieldSpellings) <- constructors
              , [constructor] <- [symbolMatches index moduleName
                  ConstructorSymbol constructorSpelling]
              , Just selectors <- [traverse
                  (unique . symbolMatches index moduleName ValueSymbol)
                  fieldSpellings]
              ]
      ]
 where
  unique [selector] = Just selector
  unique _ = Nothing

-- | Record constructors and their field spellings per datatype, in source
-- field order.
recordConstructorGroups
  :: HSE.Module HSE.SrcSpanInfo
  -> [(String, [(String, [String])])]
recordConstructorGroups syntax = concatMap declarationGroups declarations
 where
  declarations = case syntax of
    HSE.Module _ _ _ _ items -> items
    HSE.XmlHybrid _ _ _ _ items _ _ _ _ -> items
    HSE.XmlPage{} -> []

  declarationGroups declaration = case declaration of
    HSE.DataDecl _ _ _ headSyntax constructors _ ->
      oneGroup headSyntax $ mapMaybe constructorFields constructors
    HSE.GDataDecl _ _ _ headSyntax _ constructors _ ->
      oneGroup headSyntax $ mapMaybe gadtFields constructors
    _ -> []

  oneGroup headSyntax constructors = case constructors of
    [] -> []
    _ -> [(hseNameText $ declarationHeadName headSyntax, constructors)]

  constructorFields (HSE.QualConDecl _ _ _ constructor) = case constructor of
    HSE.RecDecl _ name fields ->
      Just (hseNameText name, concatMap fieldNames fields)
    _ -> Nothing

  gadtFields (HSE.GadtDecl _ name _ _ fields _) = case fields of
    Just fieldList ->
      Just (hseNameText name, concatMap fieldNames fieldList)
    Nothing -> Nothing

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

-- Child maps deliberately mirror each name projection. A flat export set
-- cannot distinguish @module A (T, field)@ from @module A (T(field))@, yet
-- only the latter permits a downstream bundled import. Keeping provenance per
-- unqualified/qualified route also prevents one permissive alias from
-- widening another alias's restricted surface.
data ModuleView = ModuleView
  { moduleViewLocal :: NameSurface
  , moduleViewUnqualified :: NameSurface
  , moduleViewSearch :: NameSurface
  , moduleViewQualified :: [(ModuleName, NameSurface)]
  , moduleViewAliases :: [(ModuleName, ModuleName)]
  , moduleViewExports :: NameSurface
  }

data ImportView = ImportView
  { importViewCanonical :: ModuleName
  , importViewQualifier :: ModuleName
  , importViewIsQualified :: Bool
  , importViewSurface :: NameSurface
  }

data NameSurface = NameSurface
  { surfaceNames :: [Name]
  , surfaceNamespaces :: Map Name (Set ScopeNamespace)
  , surfaceChildren :: Map Name [Name]
  }
  deriving (Eq, Show)

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
      when (moduleName `elem` stack)
        $ lift $ Left $ withSource (workspaceModulePath target)
          $ scopeDiagnostic "DJEX_REPL_IMPORT_CYCLE"
              "cannot construct a scope through an import cycle"
              $ intercalate " -> "
              $ map SharedName.renderModuleName $ reverse $ moduleName : stack
      (moduleHead, pragmas, sourceImports) <- lift $ moduleParts target
      imports <- traverse
        (resolveSourceImport index modules (workspaceModulePath target)
          $ moduleName : stack)
        sourceImports
      implicit <- sourceImplicitPrelude index modules (moduleName : stack)
        moduleName pragmas sourceImports
      let allImports = maybe imports (: imports) implicit
          local = surfaceFromNames index $ moduleSymbols index moduleName
          unqualified = mergeSurfaces $ local :
            [ importViewSurface item
            | item <- allImports
            , not $ importViewIsQualified item
            ]
          search = mergeSurfaces $ local : map importViewSurface allImports
          qualified = mergeQualifiedSurfaces $ (moduleName, local) :
            [ (importViewQualifier item, importViewSurface item)
            | item <- allImports
            ]
          aliases = ordNub
            [ (importViewQualifier item, importViewCanonical item)
            | item <- allImports
            , importViewQualifier item /= importViewCanonical item
            ]
          provisional = ModuleView
            { moduleViewLocal = local
            , moduleViewUnqualified = unqualified
            , moduleViewSearch = search
            , moduleViewQualified = qualified
            , moduleViewAliases = aliases
            , moduleViewExports = emptySurface
            }
      rawExports <- lift
        $ atSource (workspaceModulePath target)
        $ resolveModuleExports index provisional moduleHead
      let exports = normalizeSurface rawExports
      lift $ validateExportSurface target exports
      let completed = provisional {moduleViewExports = exports}
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
  selected <- case Map.lookup canonical modules of
    Just target
      -- A SOURCE import is an interface edge used specifically to break a
      -- source cycle. Workspace does not load @.hs-boot@ files, so following
      -- the ordinary module here would recreate the cycle that SOURCE broke.
      | not (HSE.importSrc declaration) -> do
          imported <- buildModuleViewCached index modules stack target
          lift $ atSource importerPath $ applyImportSpecs index
            (moduleViewExports imported)
            $ HSE.importSpecs declaration
    Just _ -> do
      let available = moduleSymbols index canonical
      lift $ atSource importerPath $ applyImportSpecs index
        (surfaceFromNames index available)
        $ HSE.importSpecs declaration
    -- This source-only session has no package database. An unresolved import
    -- therefore contributes no declarations; the workspace warning explains
    -- that boundary without manufacturing a package export surface.
    -- An import list cannot be checked without that missing module's export
    -- surface. Keep the whole import advisory and empty instead of upgrading
    -- it to a misleading item-level failure.
    Nothing -> pure emptySurface
  pure ImportView
    { importViewCanonical = canonical
    , importViewQualifier = qualifier
    , importViewIsQualified = HSE.importQualified declaration
    , importViewSurface = selected
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
        || not (implicitPreludeEnabled [] pragmas) -> pure Nothing
    | otherwise -> case Map.lookup prelude modules of
        Nothing -> pure Nothing
        Just target -> do
          imported <- buildModuleViewCached index modules stack target
          let available = moduleViewExports imported
          pure $ if null (surfaceNames available) then Nothing else Just ImportView
            { importViewCanonical = prelude
            , importViewQualifier = prelude
            , importViewIsQualified = False
            , importViewSurface = available
            }
resolveModuleExports
  :: SymbolIndex
  -> ModuleView
  -> Maybe (HSE.ModuleHead HSE.SrcSpanInfo)
  -> Either Diagnostic NameSurface
resolveModuleExports index view Nothing = Right $ localSurface index view
resolveModuleExports index view
    (Just (HSE.ModuleHead _ _ _ Nothing)) = Right $ localSurface index view
resolveModuleExports index view
    (Just (HSE.ModuleHead _ _ _ (Just (HSE.ExportSpecList _ specs)))) =
  mergeSurfaces <$> traverse resolve specs
 where
  resolve spec = case spec of
    HSE.EVar _ qname -> surfaceForRoles index [ValueSymbol]
      <$> selectQName index [ValueSymbol] view qname (HSE.prettyPrint spec)
    HSE.EAbs _ namespace qname ->
      let roles = namespaceRoles namespace
      in surfaceForRoles index roles
          <$> selectQName index roles view qname (HSE.prettyPrint spec)
    HSE.EThingWith _ wildcard qname children -> do
      parent <- selectQName index [BundledOwnerSymbol] view qname
        $ HSE.prettyPrint spec
      case parent of
        [oneParent] -> do
          candidates <- qNameCandidates view qname
          let candidateNames = surfaceNames candidates
              childGroups = surfaceChildren candidates
          _ <- requireChildGroup childGroups oneParent $ HSE.prettyPrint spec
          selectedChildren <- selectChildren childGroups candidateNames
            oneParent children $ HSE.prettyPrint spec
          let wildcardChildren = case wildcard of
                HSE.NoWildcard _ -> []
                HSE.EWildcard _ _ -> availableChildren childGroups
                  candidateNames oneParent
              exportedChildren = ordNub
                $ wildcardChildren ++ selectedChildren
          pure $ bundledSurface index oneParent exportedChildren
        _ -> pure $ surfaceForRoles index [BundledOwnerSymbol] parent
    HSE.EModuleContents _ syntaxModule -> do
      wanted <- checkedHseModuleName syntaxModule
      let matchingQualified =
            [ surface
            | (qualifier, surface) <- moduleViewQualified view
            , qualifier == wanted
            ]
          unqualified = moduleViewUnqualified view
      if null matchingQualified
        then Left $ scopeDiagnostic "DJEX_REPL_EXPORT_NOT_IN_SCOPE"
          "module re-export is not in scope" $ HSE.prettyPrint spec
        -- Per the Haskell Report, @module M@ denotes identities available
        -- both unqualified and through the written qualifier @M@. This keeps
        -- the current module's locals, honors @as@ aliases, and makes a
        -- qualified-only import a valid but empty module export.
        else Right $ intersectSurfaces unqualified
          $ mergeSurfaces matchingQualified


localSurface :: SymbolIndex -> ModuleView -> NameSurface
localSurface _ = moduleViewLocal

-- Haskell has separate type and value export namespaces, but two different
-- entities in the same namespace may not share one exported occurrence. This
-- most often arises through two @module X@ re-exports whose aliases expose
-- colliding declarations. Exact duplicate exports remain harmless.
validateExportSurface
  :: WorkspaceModule
  -> NameSurface
  -> Either Diagnostic ()
validateExportSurface target exports = case firstConflict of
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
    [ (occurrence, [(name, namespaces)])
    | name <- surfaceNames exports
    , let namespaces = Map.findWithDefault Set.empty name
            $ surfaceNamespaces exports
    , Just occurrence <- [SharedName.nameSpelling name]
    ]
  firstConflict = firstPair
    [ (occurrence, left, right)
    | claims <- groups
    , ((left, leftNamespaces), (right, rightNamespaces)) <-
        unorderedPairs claims
    , left /= right
    , not $ Set.null $ Set.intersection leftNamespaces rightNamespaces
    , Just occurrence <- [SharedName.nameSpelling left]
    ]
  firstPair [] = Nothing
  firstPair (pair : _) = Just pair
  unorderedPairs [] = []
  unorderedPairs (name : names) =
    [(name, other) | other <- names] ++ unorderedPairs names
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
  -> Either Diagnostic NameSurface
qNameCandidates view qname = case qname of
  HSE.UnQual _ _ -> Right $ moduleViewUnqualified view
  HSE.Qual _ syntaxModule _ -> do
    wanted <- checkedHseModuleName syntaxModule
    Right $ mergeSurfaces
      [ surface
      | (qualifier, surface) <- moduleViewQualified view
      , qualifier == wanted
      ]
  HSE.Special _ _ -> Right $ moduleViewSearch view

applyImportSpecs
  :: SymbolIndex
  -> NameSurface
  -> Maybe (HSE.ImportSpecList HSE.SrcSpanInfo)
  -> Either Diagnostic NameSurface
applyImportSpecs _ available Nothing = Right $ normalizeSurface available
applyImportSpecs index available
    (Just (HSE.ImportSpecList _ hiding specs)) = do
  selected <- mergeSurfaces <$> traverse select specs
  pure $ if hiding then subtractSurface available selected else selected
 where
  availableNames = surfaceNames available
  childGroups = surfaceChildren available
  select spec = case spec of
    HSE.IVar _ name -> surfaceForRoles index [ValueSymbol]
      <$> selectUnique index [ValueSymbol]
      available (hseNameText name) (HSE.prettyPrint spec)
    HSE.IAbs _ namespace name ->
      let roles = namespaceRoles namespace
      in surfaceForRoles index roles <$> selectUnique index
          roles available (hseNameText name) (HSE.prettyPrint spec)
    HSE.IThingAll _ name -> do
      parents <- selectUnique index [BundledOwnerSymbol] available
        (hseNameText name)
        $ HSE.prettyPrint spec
      case parents of
        [parent] -> do
          _ <- requireChildGroup childGroups parent $ HSE.prettyPrint spec
          let children = availableChildren childGroups availableNames parent
          pure $ bundledSurface index parent children
        _ -> pure $ surfaceForRoles index [BundledOwnerSymbol] parents
    HSE.IThingWith _ name children -> do
      parents <- selectUnique index [BundledOwnerSymbol] available
        (hseNameText name)
        $ HSE.prettyPrint spec
      case parents of
        [parent] -> do
          _ <- requireChildGroup childGroups parent $ HSE.prettyPrint spec
          selectedChildren <- selectChildren childGroups availableNames parent
            children $ HSE.prettyPrint spec
          pure $ bundledSurface index parent selectedChildren
        _ -> pure $ surfaceForRoles index [BundledOwnerSymbol] parents

selectUnique
  :: SymbolIndex
  -> [SymbolRole]
  -> NameSurface
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
  acceptedNamespaces = namespacesForRoles accepted
  matches = ordNub
    [ name
    | name <- surfaceNames available
    , SharedName.nameSpelling name == Just occurrence
    , not $ Set.null $ Set.intersection accepted
        $ Map.findWithDefault Set.empty name $ symbolRoles index
    , not $ Set.null $ Set.intersection acceptedNamespaces
        $ Map.findWithDefault Set.empty name $ surfaceNamespaces available
    ]

selectChildren
  :: Map Name [Name]
  -> [Name]
  -> Name
  -> [HSE.CName HSE.SrcSpanInfo]
  -> String
  -> Either Diagnostic [Name]
selectChildren childGroups available parent children rendered =
  traverse select children
 where
  candidates = availableChildren childGroups available parent
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

availableChildren :: Map Name [Name] -> [Name] -> Name -> [Name]
availableChildren childGroups available parent =
  filter (`Set.member` availableSet)
    $ Map.findWithDefault [] parent childGroups
 where
  availableSet = Set.fromList available

requireChildGroup
  :: Map Name [Name]
  -> Name
  -> String
  -> Either Diagnostic [Name]
requireChildGroup childGroups parent rendered = case
    Map.lookup parent childGroups of
  Just children -> Right children
  Nothing -> Left $ scopeDiagnostic "DJEX_REPL_IMPORT_CHILD"
    "type or class is not available with its bundled children" rendered

emptySurface :: NameSurface
emptySurface = NameSurface [] Map.empty Map.empty

surfaceFromNames :: SymbolIndex -> [Name] -> NameSurface
surfaceFromNames index names = normalizeSurface $ NameSurface
  { surfaceNames = names
  , surfaceNamespaces = Map.fromList
      [ (name, namespacesForRoles roles)
      | name <- names
      , let roles = Map.findWithDefault Set.empty name $ symbolRoles index
      ]
  , surfaceChildren = symbolChildren index
  }

surfaceForRoles :: SymbolIndex -> [SymbolRole] -> [Name] -> NameSurface
surfaceForRoles index requested names = normalizeSurface $ NameSurface
  { surfaceNames = names
  , surfaceNamespaces = Map.fromList
      [ (name, namespacesForRoles $ Set.intersection accepted actual)
      | name <- names
      , let actual = Map.findWithDefault Set.empty name $ symbolRoles index
      ]
  , surfaceChildren = Map.empty
  }
 where
  accepted = Set.fromList requested

bundledSurface :: SymbolIndex -> Name -> [Name] -> NameSurface
bundledSurface index parent children = normalizeSurface $ NameSurface
  { surfaceNames = parent : children
  , surfaceNamespaces = Map.unionWith Set.union
      (surfaceNamespaces
        $ surfaceForRoles index [TypeSymbol, BundledOwnerSymbol] [parent])
      (surfaceNamespaces
        $ surfaceForRoles index [ValueSymbol, ConstructorSymbol] children)
  , surfaceChildren = Map.singleton parent children
  }

surfaceNamesIn :: ScopeNamespace -> NameSurface -> [Name]
surfaceNamesIn AnyScope surface = surfaceNames surface
surfaceNamesIn namespace surface =
  [ name
  | name <- surfaceNames surface
  , namespace `Set.member` Map.findWithDefault Set.empty name
      (surfaceNamespaces surface)
  ]

namespacesForRoles :: Set SymbolRole -> Set ScopeNamespace
namespacesForRoles roles = Set.fromList
  $ [TypeScope | not $ Set.null $ Set.intersection typeRoles roles]
  ++ [ValueScope | not $ Set.null $ Set.intersection valueRoles roles]
 where
  typeRoles = Set.fromList [TypeSymbol, BundledOwnerSymbol]
  valueRoles = Set.fromList [ValueSymbol, ConstructorSymbol]

normalizeSurface :: NameSurface -> NameSurface
normalizeSurface surface = NameSurface names namespaces children
 where
  names = ordNub
    [ name
    | name <- surfaceNames surface
    , not $ Set.null $ namespaceOf name
    ]
  nameSet = Set.fromList names
  namespaces = Map.restrictKeys
    (Map.map (Set.delete AnyScope) $ surfaceNamespaces surface)
    nameSet
  namespaceOf name = Set.delete AnyScope
    $ Map.findWithDefault Set.empty name $ surfaceNamespaces surface
  children = Map.mapMaybeWithKey retain $ surfaceChildren surface
  retain parent childNames
    | TypeScope `Set.member` namespaceOf parent = Just
        [ child
        | child <- childNames
        , ValueScope `Set.member` namespaceOf child
        ]
    | otherwise = Nothing

mergeSurfaces :: [NameSurface] -> NameSurface
mergeSurfaces surfaces = normalizeSurface $ NameSurface
  (concatMap surfaceNames surfaces)
  (Map.unionsWith Set.union $ map surfaceNamespaces surfaces)
  (mergeChildGroups $ map surfaceChildren surfaces)

intersectSurfaces :: NameSurface -> NameSurface -> NameSurface
intersectSurfaces left right = normalizeSurface
  $ NameSurface (surfaceNames left) namespaces children
 where
  namespaces = Map.mergeWithKey intersectNamespaces
    (const Map.empty) (const Map.empty)
    (surfaceNamespaces left) (surfaceNamespaces right)
  intersectNamespaces _ leftNamespaces rightNamespaces =
    let shared = Set.intersection leftNamespaces rightNamespaces
    in if Set.null shared then Nothing else Just shared
  children = Map.mergeWithKey intersectChildren
    (const Map.empty) (const Map.empty)
    (surfaceChildren left) (surfaceChildren right)
  intersectChildren _ leftChildren rightChildren = Just
    $ filter (`Set.member` Set.fromList rightChildren) leftChildren

subtractSurface :: NameSurface -> NameSurface -> NameSurface
subtractSurface available removed = normalizeSurface available
  { surfaceNamespaces = Map.mapWithKey subtractNamespaces
      $ surfaceNamespaces available
  }
 where
  subtractNamespaces name namespaces = Set.difference namespaces
    $ Map.findWithDefault Set.empty name $ surfaceNamespaces removed

mergeChildGroups :: [Map Name [Name]] -> Map Name [Name]
mergeChildGroups = foldl' (Map.unionWith appendChildren) Map.empty
 where
  appendChildren left right = ordNub $ left ++ right

namespaceRoles :: HSE.Namespace annotation -> [SymbolRole]
namespaceRoles namespace = case namespace of
  HSE.PatternNamespace _ -> [ConstructorSymbol]
  HSE.TypeNamespace _ -> [TypeSymbol]
  HSE.NoNamespace _ -> [TypeSymbol]

mergeQualifiedSurfaces
  :: [(ModuleName, NameSurface)]
  -> [(ModuleName, NameSurface)]
mergeQualifiedSurfaces = foldl' insert []
 where
  insert [] pair = [pair]
  insert ((key, old) : rest) pair@(wanted, surface)
    | key == wanted = (key, mergeSurfaces [old, surface]) : rest
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
ordNub = SharedCollection.distinctOn id
