{-# LANGUAGE MonadComprehensions #-}

module Language.Haskell.Exference.TypeDeclsFromHaskellSrc
  ( HsTypeDecl (..)
  , TypeDeclMap
  , uniqueTypeDeclMap
  , applyTypeDecls
  , getTypeDecls
  , getTypeDeclsLocated
  , getTypeDeclsLocatedWithResolvers
  , convertType
  , convertTypeWithResolver
  , convertTypeInternal
  , convertTypeInternalWithResolver
  , parseType
  , parseTypeWithKinds
  , parseTypeWithInventory
  , parseTypeWithInventoryInScope
  , parseTypeWithInventoryInQualifiedScope
  , toSynthesisTypeDeclaration
  , fromSynthesisTypeDeclaration
  )
where



import Language.Haskell.Exference.Core.Types
import qualified Language.Haskell.Exference.Core.TypeUtils as TypeUtils
import Language.Haskell.Exference.Core.Declaration
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.HaskellSrcUtils
import Language.Haskell.Exference.ExtractionError
import Language.Haskell.Exference.Internal.TypeParsing
  ( parseHaskellSrcType
  , parseTypeWithResolver
  , typeResolverFromInventory
  )
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error)
  , codedDiagnostic
  , diagnostic
  , sourceTextSpan
  , withLocation
  )
import qualified Language.Haskell.Synthesis.Kind as SharedKind
import qualified Language.Haskell.Synthesis.KindInference as SharedKindInference
import qualified Language.Haskell.Synthesis.Inventory as SharedInventory
import qualified Language.Haskell.Synthesis.Name as SharedName

import Language.Haskell.Exts.Syntax hiding (TypeApp)
import qualified Language.Haskell.Exts.Parser as P
import Language.Haskell.Exts.SrcLoc
  ( SrcSpanInfo )

import Control.Monad.Trans.Except ( runExceptT
                                  , ExceptT(..)
                                  , throwE
                                  )
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (get)

import Control.Monad ( forM )
import Data.Either ( rights )
import Data.Bifunctor ( bimap, first )
import Data.Maybe ( maybeToList )
import Data.List ( intercalate )
import qualified Data.List.NonEmpty as NonEmpty

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import qualified Data.IntSet as IntSet
import qualified Language.Haskell.Synthesis.Type as SharedType
import qualified Language.Haskell.Synthesis.TypeSynonym as SharedTypeSynonym



data HsTypeDecl = HsTypeDecl
  { tdecl_name :: QualifiedName
  , tdecl_params :: [TVarId]
  , tdecl_result :: HsType
  } deriving (Eq, Show) -- (Data, Show, Generic, Typeable)

type TypeDeclMap = Map QualifiedName HsTypeDecl

-- | Build the legacy synonym-expansion index without choosing an arbitrary
-- winner for duplicate declarations.  The ordered declarations remain the
-- source of truth and are later sealed by the shared inventory, which can
-- report the nominal duplicate precisely.
uniqueTypeDeclMap :: [HsTypeDecl] -> TypeDeclMap
uniqueTypeDeclMap declarations = M.fromList
  [ (name, declaration)
  | (name, [declaration]) <- M.toAscList $ M.fromListWith (++)
      [ (tdecl_name declaration, [declaration])
      | declaration <- declarations
      ]
  ]

toSynthesisTypeDeclaration
  :: HsTypeDecl
  -> Either SynthesisDeclarationError SynthesisDeclaration
toSynthesisTypeDeclaration declaration = toSynthesisTypeSynonym
  (tdecl_name declaration)
  (tdecl_params declaration)
  (tdecl_result declaration)

fromSynthesisTypeDeclaration
  :: SynthesisDeclaration
  -> Either SynthesisDeclarationError HsTypeDecl
fromSynthesisTypeDeclaration declaration = do
  (name, parameters, body) <- fromSynthesisTypeSynonym declaration
  Right $ HsTypeDecl name parameters body

applyTypeDecls :: Map QualifiedName (Either String HsTypeDecl)
               -> HsType 
               -> Either String HsType
applyTypeDecls declarations source = do
  sharedSource <- first show $ toSynthesisType source
  expanded <- first renderExpansionError
    $ SharedTypeSynonym.expandTypeSynonymDefinitions
        freshSynthesisVariable definitions sharedSource
  first show $ fromSynthesisType expanded
 where
  -- Failed declarations are reported separately by 'getTypeDecls'. Excluding
  -- them here retains their applications as ordinary nominal constructors;
  -- the shared traversal still expands aliases inside their arguments.
  definitions = M.fromList
    [ ( alias
      , ( map SharedType.FlexibleVariable parameters
        , body
        )
      )
    | (alias, Right (HsTypeDecl _ parameters body)) <- M.toAscList declarations
    ]

  renderExpansionError failure = case failure of
    SharedTypeSynonym.IntrinsicTypeSynonym name ->
      "intrinsic type synonym: " ++ show name
    SharedTypeSynonym.DuplicateTypeSynonymParameter name variable ->
      "duplicate parameter "
        ++ show (SharedType.variableIdentity variable)
        ++ " for type declaration " ++ show name
    SharedTypeSynonym.UnsaturatedTypeSynonym name _ _ ->
      "wrong number of parameters for type declaration " ++ show name
    SharedTypeSynonym.RecursiveTypeSynonyms names ->
      "cyclic type synonym: "
        ++ intercalate " -> " (map show $ NonEmpty.toList names)
    SharedTypeSynonym.FreshVariableUnavailable variable ->
      "cannot freshen type synonym binder " ++ show variable
    SharedTypeSynonym.FreshVariableCollision old replacement ->
      "invalid fresh type synonym binder " ++ show replacement
        ++ " for " ++ show old

getTypeDecls :: Monad m
             => [QualifiedName]
             -> [Module SrcSpanInfo]
             -> m [Either String HsTypeDecl]
getTypeDecls ds = fmap (map (first extractionErrorMessage))
  . getTypeDeclsLocated ds

-- | Located core of 'getTypeDecls': every failure carries its owning
-- declaration's source span. The string entry point above is its exact
-- message projection, so the historical diagnostics cannot drift.
getTypeDeclsLocated :: Monad m
                    => [QualifiedName]
                    -> [Module SrcSpanInfo]
                    -> m [Either ExtractionError HsTypeDecl]
getTypeDeclsLocated ds = getTypeDeclsLocatedWithResolvers
  (const $ legacyTypeResolver M.empty ds)

-- | Elaborate synonym bodies in the source scope of their owning module.
-- The compatibility entry point above intentionally retains its historical
-- unique-global lookup; complete source loaders use this variant so an import
-- list, hiding clause, or qualifier alias applies before synonym expansion.
getTypeDeclsLocatedWithResolvers
  :: Monad m
  => (ModuleName SrcSpanInfo -> TypeResolver)
  -> [Module SrcSpanInfo]
  -> m [Either ExtractionError HsTypeDecl]
getTypeDeclsLocatedWithResolvers resolverFor modules = do
  rawList <- sequence $ do
    modul <- modules
    (mn, decls) <- maybeToList $ moduleNameAndDecls modul
    TypeDecl declSpan rawHead rawTy <- decls
    let (name, rawVars) = splitDeclHead rawHead
    pure $ fmap (bimap
          (extractionErrorAt declSpan
            . (("when parsing type declaration "++show name++": ")++))
          ((,) declSpan))
         $ runExceptT
         $ runConversionT emptyConvData
         $ do
      -- Keep RHS conversion and head binding in one exact namespace. Hidden
      -- alpha-renamed RHS binders have no spelling-map entry, so rebuilding a
      -- state from that map could otherwise reuse one for a phantom parameter.
      ty <- convertTypeNoDeclInternalWithResolver
        (resolverFor mn) (Just mn) rawTy
      -- Retain the historical failure precedence: RHS conversion precedes
      -- validation of the declaration name.
      qname <- either throwE pure $ convertModuleName mn name
      vars <- rawVars `forM` tyVarTransform
      normalized <- normalizeConvertedForalls
        (IntSet.fromList vars) ty
      pure $ HsTypeDecl qname vars normalized
  let validDeclarations = rights rawList
      declarationMap = M.map Right
        $ uniqueTypeDeclMap $ map snd validDeclarations
      -- Validate every reachable expansion now so the compatibility loader
      -- retains its historical cycle and saturation diagnostics.  Keep the
      -- raw declaration, however: the shared Inventory must see applications
      -- before phantom parameters can erase kind errors, and its backend
      -- lowering will perform the one authoritative expansion afterwards.
      validate (declSpan, declaration) =
        first (extractionErrorAt declSpan)
          (applyTypeDecls declarationMap $ tdecl_result declaration)
          >> pure declaration
  -- Semantic validation needs the complete declaration map, but its results
  -- still belong in the source slot of the raw declaration that produced
  -- them. Grouping raw failures before validated declarations would invert a
  -- later conversion error with an earlier cycle or saturation error.
  return $ map (either Left validate) rawList

convertType :: Monad m
            => Map QualifiedName HsTypeClass
            -> Maybe (ModuleName SrcSpanInfo)
            -> [QualifiedName]
            -> TypeDeclMap
            -> Type SrcSpanInfo
            -> ExceptT String m (HsType, TypeVarIndex)
convertType tcs mn ds = convertTypeWithResolver
  (legacyTypeResolver tcs ds) mn

-- | Resolver-aware counterpart of 'convertType'.
convertTypeWithResolver
  :: Monad m
  => TypeResolver
  -> Maybe (ModuleName SrcSpanInfo)
  -> TypeDeclMap
  -> Type SrcSpanInfo
  -> ExceptT String m (HsType, TypeVarIndex)
convertTypeWithResolver resolver mn declMap t = do
  (ty, index) <- convertTypeNoDeclWithResolver resolver mn t
  expanded <- either throwE pure $ applyTypeDecls (M.map Right declMap) ty
  -- The returned index describes this type's own source spellings; treating
  -- those IDs as an enclosing namespace would needlessly rename an ordinary
  -- @forall a@ and leave its hint pointing at a dead identity.
  normalized <- either (throwE . show) (pure . fst)
    $ TypeUtils.alphaNormalizeForalls IntSet.empty expanded
  return (normalized, index)

convertTypeInternal
  :: Monad m
  => Map QualifiedName HsTypeClass
  -> Maybe (ModuleName SrcSpanInfo) -- default (for unqualified stuff)
                      -- Nothing uses a broad search for lookups
  -> [QualifiedName] -- list of fully qualified data types
                                         -- (to keep things unique)
  -> TypeDeclMap
  -> Type SrcSpanInfo
  -> ConversionT String m HsType
convertTypeInternal tcs defModuleName ds = convertTypeInternalWithResolver
  (legacyTypeResolver tcs ds) defModuleName

-- | Stateful resolver-aware counterpart of 'convertTypeInternal'.
convertTypeInternalWithResolver
  :: Monad m
  => TypeResolver
  -> Maybe (ModuleName SrcSpanInfo)
  -> TypeDeclMap
  -> Type SrcSpanInfo
  -> ConversionT String m HsType
convertTypeInternalWithResolver resolver defModuleName declMap t = do
  ambientVariables <- convDataReservedIds <$> lift get
  ty <- convertTypeNoDeclInternalWithResolver resolver defModuleName t
  expanded <- either throwE pure $ applyTypeDecls (M.map Right declMap) ty
  normalizeConvertedForalls ambientVariables expanded

parseType
  :: (Monad m)
  => Map QualifiedName HsTypeClass
  -> Maybe (ModuleName SrcSpanInfo)
  -> [QualifiedName]
  -> TypeDeclMap
  -> P.ParseMode
  -> String
  -> ExceptT Diagnostic m (HsType, TypeVarIndex)
parseType tcs mn ds tDeclMap mode source = parseHaskellSrcType
  (convertType tcs mn ds tDeclMap) mode source

-- | Parse, lower, and kind-check a query against the assumptions retained by
-- the source inventory that will supply its search environment.
parseTypeWithKinds
  :: Monad m
  => SharedKindInference.KindAssumptions
  -> Map QualifiedName HsTypeClass
  -> Maybe (ModuleName SrcSpanInfo)
  -> [QualifiedName]
  -> TypeDeclMap
  -> P.ParseMode
  -> String
  -> ExceptT Diagnostic m (HsType, TypeVarIndex)
parseTypeWithKinds assumptions tcs mn ds = parseTypeWithResolverKinds
  assumptions (legacyTypeResolver tcs ds) mn

-- | Parse a query against one checked shared inventory. Both nominal lookup
-- and kind checking are derived from that same opaque value; source-projection
-- lookup caches and backend class dictionaries cannot affect elaboration.
-- Type synonyms deliberately remain nominal here and are expanded later by
-- the stable session from this inventory's own prepared synonym table.
parseTypeWithInventory
  :: Monad m
  => SharedInventory.Inventory typeVariable annotation
  -> Maybe (ModuleName SrcSpanInfo)
  -> P.ParseMode
  -> String
  -> ExceptT Diagnostic m (HsType, TypeVarIndex)
parseTypeWithInventory inventory mn = parseTypeWithResolverKinds
  (SharedInventory.inventoryKindAssumptions inventory)
  (typeResolverFromInventory inventory)
  mn
  M.empty

-- | Parse a query with GHCi-style prompt visibility. The complete inventory
-- still supplies kinds and explicit qualified lookup, while only the supplied
-- exact names participate in unqualified resolution. Qualifier aliases map a
-- prompt spelling such as @M.T@ back to its canonical loaded module.
parseTypeWithInventoryInScope
  :: Monad m
  => SharedInventory.Inventory typeVariable annotation
  -> Maybe (ModuleName SrcSpanInfo)
  -> [QualifiedName]
  -> [(SharedName.ModuleName, SharedName.ModuleName)]
  -> P.ParseMode
  -> String
  -> ExceptT Diagnostic m (HsType, TypeVarIndex)
parseTypeWithInventoryInScope inventory currentModule visible aliases =
  parseTypeWithResolverKinds
    (SharedInventory.inventoryKindAssumptions inventory)
    (scopeTypeResolver visible aliases $ typeResolverFromInventory inventory)
    currentModule
    M.empty

-- | Fully qualified interactive counterpart of
-- 'parseTypeWithInventoryInScope'. In addition to restricting bare names, it
-- records the exact canonical names admitted by every written import
-- qualifier, so an alias cannot bypass an explicit list or @hiding@ clause.
parseTypeWithInventoryInQualifiedScope
  :: Monad m
  => SharedInventory.Inventory typeVariable annotation
  -> Maybe (ModuleName SrcSpanInfo)
  -> [QualifiedName]
  -> [(SharedName.ModuleName, SharedName.ModuleName)]
  -> [(SharedName.ModuleName, [QualifiedName])]
  -> P.ParseMode
  -> String
  -> ExceptT Diagnostic m (HsType, TypeVarIndex)
parseTypeWithInventoryInQualifiedScope inventory currentModule visible aliases
    qualified = parseTypeWithResolverKinds
  (SharedInventory.inventoryKindAssumptions inventory)
  (scopeTypeResolverWithQualifiedNames visible aliases qualified
    $ typeResolverFromInventory inventory)
  currentModule
  M.empty

parseTypeWithResolverKinds
  :: Monad m
  => SharedKindInference.KindAssumptions
  -> TypeResolver
  -> Maybe (ModuleName SrcSpanInfo)
  -> TypeDeclMap
  -> P.ParseMode
  -> String
  -> ExceptT Diagnostic m (HsType, TypeVarIndex)
parseTypeWithResolverKinds assumptions resolver mn declarations mode source = do
  (rawType, variableIndex) <- parseTypeWithResolver resolver mn mode source
  -- Haskell synonym parameters are kind-checked even when their RHS does not
  -- mention them. Checking the raw application first prevents a phantom
  -- parameter from erasing an invalid higher-kinded argument.
  checkKind rawType
  expanded <- either (throwE . conversionDiagnostic) pure
    $ applyTypeDecls (M.map Right declarations) rawType
  -- As in 'convertType', query hints are not ambient binders. True free
  -- variables are claimed by the normalizer itself.
  normalized <- either (throwE . conversionDiagnostic . show) (pure . fst)
    $ TypeUtils.alphaNormalizeForalls IntSet.empty expanded
  -- Expansion is expected to preserve kind, but this defensive obligation
  -- also guards parser-adapter tables assembled by compatibility callers.
  checkKind normalized
  pure (normalized, variableIndex)
 where
  checkKind typeExpression = do
    shared <- either (throwE . kindDiagnostic . show) pure
      $ toSynthesisType typeExpression
    either (throwE . kindDiagnostic . show) pure
      $ SharedKindInference.checkTypesKinds assumptions
        [(SharedKind.ProperTypeKind, shared)]

  conversionDiagnostic message =
    withLocation (P.parseFilename mode) (sourceTextSpan source)
    $ diagnostic Error message

  kindDiagnostic message =
    withLocation (P.parseFilename mode) (sourceTextSpan source)
    $ codedDiagnostic Error "EXF_KIND" $ "ill-kinded input type: " ++ message
