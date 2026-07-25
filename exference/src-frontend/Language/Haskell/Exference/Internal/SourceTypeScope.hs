-- | Private construction of the type/class scope used while elaborating each
-- parsed Haskell module. This module owns import filtering, export/reexport
-- surfaces, qualifier aliases, and implicit-Prelude policy so the environment
-- loader remains an orchestration boundary.
module Language.Haskell.Exference.Internal.SourceTypeScope
  ( sourceTypeResolvers
  , sourceClassArities
  )
where

import Data.Maybe (isJust, mapMaybe, maybeToList)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Language.Haskell.Exts.Parser (ParseMode (..))
import qualified Language.Haskell.Exts.Extension as HSEExtension
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax (Module)
import qualified Language.Haskell.Exts.Syntax as HSE
import Language.Haskell.Exference.Core.Types
  ( QualifiedName
  , qualifiedNameModule
  , qualifiedNameOccurrence
  )
import Language.Haskell.Exference.HaskellSrcUtils
  ( moduleNameAndDecls
  , splitDeclHead
  )
import Language.Haskell.Exference.TypeFromHaskellSrc
  ( TypeResolver (..)
  , convertModuleName
  , convertName
  )
import qualified Language.Haskell.Synthesis.Name as SharedName

-- | The type/class namespace exported by one loaded module. Values and data
-- constructors are intentionally absent: this scope is used only while
-- elaborating source types.
data NominalSurface = NominalSurface
  { nominalTypes :: [QualifiedName]
  , nominalClasses :: [QualifiedName]
  }
  deriving (Eq)

emptyNominalSurface :: NominalSurface
emptyNominalSurface = NominalSurface [] []

surfaceNames :: NominalSurface -> [QualifiedName]
surfaceNames surface = S.toAscList $ S.fromList
  $ nominalTypes surface ++ nominalClasses surface

data NominalImport = NominalImport
  { nominalImportCanonical :: SharedName.ModuleName
  , nominalImportQualifier :: SharedName.ModuleName
  , nominalImportIsQualified :: Bool
  , nominalImportSurface :: NominalSurface
  , nominalImportSurfaceIsExact :: Bool
  }

-- | Construct one strict resolver for every parsed module. All source-loading
-- entry points share this path; the bundled environment declares its own
-- dependencies just like an ordinary source workspace.
sourceTypeResolvers
  :: [QualifiedName]
  -> M.Map QualifiedName Int
  -> [(ParseMode, Module SrcSpanInfo)]
  -> M.Map String TypeResolver
sourceTypeResolvers typeNames classArities parsedModules = M.fromList
  [ (moduleText moduleName, resolverFor mode modul moduleName)
  | (mode, modul) <- parsedModules
  , (moduleName, _) <- maybeToList $ moduleNameAndDecls modul
  ]
 where
  modules = map snd parsedModules
  baseResolver = TypeResolver
    { resolverTypeNames = typeNames
    , resolverClassArities = classArities
    , resolverUnqualifiedTypeNames = typeNames
    , resolverUnqualifiedClassNames = M.keys classArities
    , resolverModuleAliases = []
    , resolverQualifiedNames = Nothing
    }
  localSurfaces = M.fromListWith mergeSurface
    [ (canonical, localSurface canonical)
    | modul <- modules
    , (moduleName, _) <- maybeToList $ moduleNameAndDecls modul
    , canonical <- maybeToList $ checkedModuleName moduleName
    ]
  moduleSyntax = M.fromList
    [ (canonical, modul)
    | modul <- modules
    , (moduleName, _) <- maybeToList $ moduleNameAndDecls modul
    , canonical <- maybeToList $ checkedModuleName moduleName
    ]
  surfaces = stabilizeExports (length modules + 1) initialExports
  initialExports = M.mapWithKey (moduleExports M.empty) moduleSyntax
  loadedModules = M.keys localSurfaces

  localSurface canonical = NominalSurface
    { nominalTypes = namesInModule canonical typeNames
    , nominalClasses = namesInModule canonical $ M.keys classArities
    }

  namesInModule canonical = filter
    ((== Just canonical) . qualifiedNameModule)

  resolverFor mode modul moduleName = case moduleImportsAndPragmas modul of
    Just (pragmas, imports) -> strictResolver moduleName
      (modeDisablesImplicitPrelude mode) pragmas imports
    _ -> baseResolver

  strictResolver syntaxModule modeDisablesPrelude pragmas imports = case
      checkedModuleName syntaxModule of
    Nothing -> baseResolver
    Just current -> baseResolver
      { resolverUnqualifiedTypeNames = nominalTypes unqualified
      , resolverUnqualifiedClassNames = nominalClasses unqualified
      , resolverModuleAliases = blockers ++ importAliases
      , resolverQualifiedNames = Just $ M.fromListWith S.union
          [ (qualifier, S.fromList $ surfaceNames surface)
          | (qualifier, surface) <- (current, local) : qualifiedSurfaces
          ]
      }
     where
      local = M.findWithDefault emptyNominalSurface current localSurfaces
      explicitImports = mapMaybe nominalImport imports
      importsWithPrelude = explicitImports
        ++ maybeToList
          (implicitPrelude modeDisablesPrelude pragmas imports current)
      unqualified = foldr mergeSurface local
        [ nominalImportSurface imported
        | imported <- importsWithPrelude
        , not $ nominalImportIsQualified imported
        ]
      qualifiedSurfaces =
        [ (nominalImportQualifier imported, nominalImportSurface imported)
        | imported <- importsWithPrelude
        , nominalImportSurfaceIsExact imported
        ]
      importAliases =
        [ ( nominalImportQualifier imported
          , nominalImportCanonical imported
          )
        | imported <- importsWithPrelude
        ]
      -- Self aliases make the exact qualified-name map authoritative for all
      -- loaded modules. Thus an unimported @A.T@ is rejected, while a truly
      -- external qualifier remains representable under the open-world policy.
      blockers = [(loaded, loaded) | loaded <- loadedModules]

  nominalImport = nominalImportFrom surfaces

  nominalImportFrom available declaration = do
    canonical <- checkedModuleName $ HSE.importModule declaration
    qualifier <- case HSE.importAs declaration of
      Nothing -> Just canonical
      Just syntaxAlias -> checkedModuleName syntaxAlias
    let packageImport = isJust $ HSE.importPkg declaration
        targetIsLoaded = M.member canonical available && not packageImport
        externalListedSurface
          | packageImport = Nothing
          | otherwise = externalImportListSurface declaration
        restricted = packageImport || isJust externalListedSurface
        targetSurface
          | targetIsLoaded = M.findWithDefault emptyNominalSurface
              canonical available
          | Just listed <- externalListedSurface = listed
          | otherwise = emptyNominalSurface
    pure NominalImport
      { nominalImportCanonical = canonical
      , nominalImportQualifier = qualifier
      , nominalImportIsQualified = HSE.importQualified declaration
      , nominalImportSurface = applyNominalImportSpecs
          (HSE.importSpecs declaration) targetSurface
      , nominalImportSurfaceIsExact = targetIsLoaded || restricted
      }

  -- An explicit positive import list is itself enough interface information
  -- to preserve an unloaded module's canonical nominal identities. A hiding
  -- list describes a complement that cannot be enumerated without the target
  -- interface, so it remains open-world rather than pretending to be exact.
  externalImportListSurface declaration = case HSE.importSpecs declaration of
    Just (HSE.ImportSpecList _ False specs) ->
      let names = concatMap (externalImportSpecNames declaration) specs
      in Just $ NominalSurface names names
    _ -> Nothing

  externalImportSpecNames declaration spec = case spec of
    HSE.IAbs _ namespace syntaxName
      | exportTypeNamespace namespace -> converted syntaxName
    HSE.IThingAll _ syntaxName -> converted syntaxName
    HSE.IThingWith _ syntaxName _ -> converted syntaxName
    _ -> []
   where
    converted syntaxName = either (const []) (: [])
      $ convertModuleName (HSE.importModule declaration) syntaxName

  implicitPrelude modeDisablesPrelude pragmas imports current = do
    prelude <- either (const Nothing) Just
      $ SharedName.mkModuleName "Prelude"
    if current == prelude
        || any ((== Just prelude) . checkedModuleName . HSE.importModule) imports
        || modeDisablesPrelude
        || any disablesImplicitPrelude pragmas
      then Nothing
      else do
        surface <- M.lookup prelude surfaces
        if null $ surfaceNames surface
          then Nothing
          else Just NominalImport
            { nominalImportCanonical = prelude
            , nominalImportQualifier = prelude
            , nominalImportIsQualified = False
            , nominalImportSurface = surface
            , nominalImportSurfaceIsExact = True
            }

  stabilizeExports 0 current = current
  stabilizeExports remaining current =
    let next = M.mapWithKey (moduleExports current) moduleSyntax
    in if next == current
        then current
        else stabilizeExports (remaining - 1) next

  moduleExports available canonical modul = case moduleExportSpecs modul of
    Nothing -> M.findWithDefault emptyNominalSurface canonical localSurfaces
    Just specs -> foldr mergeSurface emptyNominalSurface
      $ map (resolveExport available canonical modul) specs

  resolveExport available canonical modul spec = case spec of
    HSE.EAbs _ namespace syntaxName
      | exportTypeNamespace namespace -> selectNamedExport
          canonical syntaxName
    HSE.EThingWith _ _ syntaxName _ -> selectNamedExport
      canonical syntaxName
    HSE.EModuleContents _ syntaxModule ->
      maybe emptyNominalSurface reexport $ checkedModuleName syntaxModule
    _ -> emptyNominalSurface
   where
    local = M.findWithDefault emptyNominalSurface canonical localSurfaces
    importedViews = mapMaybe (nominalImportFrom available)
      $ moduleExplicitImports modul
    reexport wanted = foldr mergeSurface emptyNominalSurface
      [ nominalImportSurface imported
      | imported <- importedViews
      , nominalImportQualifier imported == wanted
      ]
    selectNamedExport current syntaxName = case syntaxName of
      HSE.UnQual _ occurrence ->
        let localMatch = selectOccurrence occurrence local
            importedMatch = selectUniqueSurface occurrence
              [ nominalImportSurface imported
              | imported <- importedViews
              , not $ nominalImportIsQualified imported
              ]
        in if null $ surfaceNames localMatch
            then importedMatch
            else localMatch
      HSE.Qual _ syntaxQualifier occurrence
        | checkedModuleName syntaxQualifier == Just current ->
            selectOccurrence occurrence local
        | otherwise -> maybe emptyNominalSurface
            (\wanted -> selectUniqueSurface occurrence
              [ nominalImportSurface imported
              | imported <- importedViews
              , nominalImportQualifier imported == wanted
              ])
            $ checkedModuleName syntaxQualifier
      _ -> emptyNominalSurface

    -- Ambiguous imported export items fail closed instead of widening the
    -- downstream surface. The workspace scope layer reports the corresponding
    -- located ambiguity before this loader is called in interactive use.
    selectUniqueSurface occurrence candidates =
      let selected = foldr mergeSurface emptyNominalSurface
            $ map (selectOccurrence occurrence) candidates
      in case surfaceNames selected of
          [_] -> selected
          _ -> emptyNominalSurface

  selectOccurrence syntaxName surface = case convertName syntaxName of
    Left _ -> emptyNominalSurface
    Right occurrence -> NominalSurface
      { nominalTypes = matching occurrence $ nominalTypes surface
      , nominalClasses = matching occurrence $ nominalClasses surface
      }
   where
    matching occurrence = filter
      ((== qualifiedNameOccurrence occurrence) . qualifiedNameOccurrence)

  exportTypeNamespace namespace = case namespace of
    HSE.NoNamespace _ -> True
    HSE.TypeNamespace _ -> True
    HSE.PatternNamespace _ -> False

  moduleExportSpecs modul = case modul of
    HSE.Module _ maybeHead _ _ _ -> case maybeHead of
      Just (HSE.ModuleHead _ _ _
          (Just (HSE.ExportSpecList _ specs))) -> Just specs
      _ -> Nothing
    _ -> Nothing

  moduleExplicitImports modul = case modul of
    HSE.Module _ _ _ imports _ -> imports
    _ -> []

  moduleText (HSE.ModuleName _ source) = source
  checkedModuleName (HSE.ModuleName _ source) = either (const Nothing) Just
    $ SharedName.mkModuleName source

  moduleImportsAndPragmas modul = case modul of
    HSE.Module _ _ pragmas imports _ -> Just (pragmas, imports)
    _ -> Nothing

  disablesImplicitPrelude pragma = case pragma of
    HSE.LanguagePragma _ names -> any
      ((`elem` ["NoImplicitPrelude", "RebindableSyntax"])
        . sourceNameText)
      names
    HSE.OptionsPragma _ _ options -> any
      (`elem` ["-XNoImplicitPrelude", "-XRebindableSyntax"])
      $ words options
    _ -> False

  modeDisablesImplicitPrelude mode = not preludeEnabled || rebindableSyntax
   where
    (preludeEnabled, rebindableSyntax) = foldl' update (True, False)
      $ extensions mode
    update (implicit, rebindable) extension = case extension of
      HSEExtension.EnableExtension HSEExtension.ImplicitPrelude ->
        (True, rebindable)
      HSEExtension.DisableExtension HSEExtension.ImplicitPrelude ->
        (False, rebindable)
      HSEExtension.EnableExtension HSEExtension.RebindableSyntax ->
        (implicit, True)
      HSEExtension.DisableExtension HSEExtension.RebindableSyntax ->
        (implicit, False)
      HSEExtension.UnknownExtension "ImplicitPrelude" ->
        (True, rebindable)
      HSEExtension.UnknownExtension "NoImplicitPrelude" ->
        (False, rebindable)
      HSEExtension.UnknownExtension "RebindableSyntax" ->
        (implicit, True)
      HSEExtension.UnknownExtension "NoRebindableSyntax" ->
        (implicit, False)
      _ -> (implicit, rebindable)

mergeSurface :: NominalSurface -> NominalSurface -> NominalSurface
mergeSurface new old = NominalSurface
  { nominalTypes = mergeNames (nominalTypes old) (nominalTypes new)
  , nominalClasses = mergeNames (nominalClasses old) (nominalClasses new)
  }
 where
  mergeNames left right = S.toAscList
    $ S.fromList left `S.union` S.fromList right

applyNominalImportSpecs
  :: Maybe (HSE.ImportSpecList SrcSpanInfo)
  -> NominalSurface
  -> NominalSurface
applyNominalImportSpecs Nothing surface = surface
applyNominalImportSpecs (Just (HSE.ImportSpecList _ hiding specs)) surface =
  NominalSurface
    { nominalTypes = select $ nominalTypes surface
    , nominalClasses = select $ nominalClasses surface
    }
 where
  selectedOccurrences = S.fromList
    $ map qualifiedNameOccurrence
    $ concatMap importSpecOccurrences specs
  select = filter $ \candidate ->
    (qualifiedNameOccurrence candidate `S.member` selectedOccurrences)
      /= hiding

importSpecOccurrences :: HSE.ImportSpec SrcSpanInfo -> [QualifiedName]
importSpecOccurrences spec = case spec of
  HSE.IAbs _ namespace syntaxName
    | isTypeNamespace namespace -> converted syntaxName
  HSE.IThingAll _ syntaxName -> converted syntaxName
  HSE.IThingWith _ syntaxName _ -> converted syntaxName
  _ -> []
 where
  converted = either (const []) (: []) . convertName
  isTypeNamespace namespace = case namespace of
    HSE.NoNamespace _ -> True
    HSE.TypeNamespace _ -> True
    HSE.PatternNamespace _ -> False

sourceNameText :: HSE.Name annotation -> String
sourceNameText syntaxName = case syntaxName of
  HSE.Ident _ source -> source
  HSE.Symbol _ source -> source

sourceClassArities
  :: [Module SrcSpanInfo]
  -> M.Map QualifiedName Int
sourceClassArities modules = M.fromList
  [ (className, length variables)
  | modul <- modules
  , (moduleName, declarations) <- maybeToList $ moduleNameAndDecls modul
  , HSE.ClassDecl _ _ rawHead _ _ <- declarations
  , let (syntaxName, variables) = splitDeclHead rawHead
  , className <- either (const []) (: [])
      $ convertModuleName moduleName syntaxName
  ]
