{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE PatternGuards #-}

module Language.Haskell.Exference.TypeDeclsFromHaskellSrc
  ( HsTypeDecl (..)
  , TypeDeclMap
  , uniqueTypeDeclMap
  , applyTypeDecls
  , getTypeDecls
  , convertType
  , convertTypeInternal
  , parseType
  , parseTypeWithKinds
  , toSynthesisTypeDeclaration
  , fromSynthesisTypeDeclaration
  )
where



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.Declaration
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.HaskellSrcUtils
import Language.Haskell.Exference.Diagnostic
import qualified Language.Haskell.Synthesis.Kind as SharedKind
import qualified Language.Haskell.Synthesis.KindInference as SharedKindInference

import Language.Haskell.Exts.Syntax hiding (TypeApp)
import qualified Language.Haskell.Exts.Parser as P
import Language.Haskell.Exts.SrcLoc
  ( SrcLoc (..)
  , SrcSpanInfo
  )

import Control.Monad.Trans.Except ( runExceptT
                                  , ExceptT(..)
                                  , throwE
                                  )

import Control.Monad ( forM, liftM )
import Data.Either ( rights )
import Data.Bifunctor ( bimap, first )
import Data.Maybe ( maybeToList )
import Data.List ( intercalate )
import qualified Data.List.NonEmpty as NonEmpty

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import qualified Language.Haskell.Synthesis.Declaration as SharedDeclaration
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
toSynthesisTypeDeclaration declaration = do
  body <- either (Left . DeclarationTypeConversionError) Right
    $ toSynthesisType $ tdecl_result declaration
  let shared = SharedDeclaration.TypeSynonymDeclaration
        NoDeclarationMetadata
        (toSynthesisName $ tdecl_name declaration)
        [ SharedDeclaration.TypeParameter
            (SharedType.FlexibleVariable parameter) Nothing
        | parameter <- tdecl_params declaration
        ]
        body
  either (Left . InvalidSharedDeclaration) (const $ Right shared)
    $ SharedDeclaration.validateDeclaration shared

fromSynthesisTypeDeclaration
  :: SynthesisDeclaration
  -> Either SynthesisDeclarationError HsTypeDecl
fromSynthesisTypeDeclaration declaration = do
  either (Left . InvalidSharedDeclaration) Right
    $ SharedDeclaration.validateDeclaration declaration
  case declaration of
    SharedDeclaration.TypeSynonymDeclaration _ name parameters body ->
      HsTypeDecl
        <$> either (Left . DeclarationNameConversionError) Right
              (fromSynthesisName name)
        <*> mapM plainParameter parameters
        <*> either (Left . DeclarationTypeConversionError) Right
              (fromSynthesisType body)
    _ -> Left ExpectedTypeSynonymDeclaration
 where
  plainParameter parameter = case SharedDeclaration.parameterKind parameter of
    Just _ -> Left $ ExplicitParameterKindUnsupported
      $ SharedDeclaration.parameterVariable parameter
    Nothing -> case SharedDeclaration.parameterVariable parameter of
      SharedType.FlexibleVariable variable -> Right variable
      SharedType.RigidVariable variable -> Left $ RigidDataParameter variable

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
    [ ( toSynthesisName alias
      , ( map SharedType.FlexibleVariable parameters
        , toSynthesisTypeStructure body
        )
      )
    | (alias, Right (HsTypeDecl _ parameters body)) <- M.toAscList declarations
    ]

  renderExpansionError failure = case failure of
    SharedTypeSynonym.IntrinsicTypeSynonym name ->
      "intrinsic type synonym: " ++ show name
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
getTypeDecls ds modules = do
  rawList <- sequence $ do
    modul <- modules
    (mn, decls) <- maybeToList $ moduleNameAndDecls modul
    TypeDecl _ rawHead rawTy <- decls
    let (name, rawVars) = splitDeclHead rawHead
    return $ liftM (bimap (("when parsing type declaration "++show name++": ")++) id)
           $ runExceptT
           $ do
      (ty, tyVarIndex) <- convertTypeNoDecl M.empty (Just mn) ds rawTy
      qname <- either throwE pure $ convertModuleName mn name
      -- RHS conversion allocates a dense zero-based namespace. Begin after it
      -- so legal phantom parameters receive fresh adjacent IDs rather than an
      -- arbitrary sentinel that can eventually collide with a large RHS.
      vars <- runConversionT (ConvData (M.size tyVarIndex) tyVarIndex)
        $ rawVars `forM` tyVarTransform
      return $ HsTypeDecl qname vars ty
  let validDeclarations = rights rawList
      declarationMap = M.map Right $ uniqueTypeDeclMap validDeclarations
      resolve declaration = HsTypeDecl
        (tdecl_name declaration)
        (tdecl_params declaration)
        <$> applyTypeDecls declarationMap (tdecl_result declaration)
  return $ [ e | e@(Left _) <- rawList ] ++ map resolve validDeclarations

convertType :: Monad m
            => Map QualifiedName HsTypeClass
            -> Maybe (ModuleName SrcSpanInfo)
            -> [QualifiedName]
            -> TypeDeclMap
            -> Type SrcSpanInfo
            -> ExceptT String m (HsType, TypeVarIndex)
convertType tcs mn ds declMap t = do
  (ty, index) <- convertTypeNoDecl tcs mn ds t
  ty' <- either throwE pure $ applyTypeDecls (M.map Right declMap) ty
  return $ (ty', index)

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
convertTypeInternal tcs defModuleName ds declMap t = do
  ty <- convertTypeNoDeclInternal tcs defModuleName ds t
  ty' <- either throwE pure $ applyTypeDecls (M.map Right declMap) ty
  return $ ty'

parseType
  :: (Monad m)
  => Map QualifiedName HsTypeClass
  -> Maybe (ModuleName SrcSpanInfo)
  -> [QualifiedName]
  -> TypeDeclMap
  -> P.ParseMode
  -> String
  -> ExceptT Diagnostic m (HsType, TypeVarIndex)
parseType tcs mn ds tDeclMap m s = case P.parseTypeWithMode m s of
  P.ParseFailed location message -> throwE
    $ withSpan (let position = SourcePosition
                      (srcLine location) (srcColumn location)
                in SourceSpan position position)
    $ withSource (srcFilename location)
    $ diagnostic message
  P.ParseOk ty -> ExceptT $ first conversionDiagnostic
    <$> runExceptT (convertType tcs mn ds tDeclMap ty)
  where
    conversionDiagnostic message =
      withSpan (sourceTextSpan s)
      $ withSource (P.parseFilename m)
      $ diagnostic message

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
parseTypeWithKinds assumptions tcs mn ds declarations mode source = do
  result@(typeExpression, _) <-
    parseType tcs mn ds declarations mode source
  shared <- either (throwE . kindDiagnostic . show) pure
    $ toSynthesisType typeExpression
  either (throwE . kindDiagnostic . show) pure
    $ SharedKindInference.checkTypesKinds assumptions
      [(SharedKind.ProperTypeKind, shared)]
  pure result
 where
  kindDiagnostic message =
    withCode "EXF_KIND"
    $ withSpan (sourceTextSpan source)
    $ withSource (P.parseFilename mode)
    $ diagnostic $ "ill-kinded input type: " ++ message
