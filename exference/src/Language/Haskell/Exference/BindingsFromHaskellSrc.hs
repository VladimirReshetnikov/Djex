{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadComprehensions #-}

module Language.Haskell.Exference.BindingsFromHaskellSrc
  ( getDecls
  , declToBinding
  , getDataConss
  , getClassMethods
  , getDataTypes
  , getDataTypesChecked
  )
where



import Language.Haskell.Exts.Syntax hiding (TypeApp)
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc ( SrcSpanInfo )
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc
import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.FunctionDecl
import Language.Haskell.Exference.HaskellSrcUtils

import Control.Monad ( join )
import Control.Monad.Trans.Except
import qualified Data.Map as M
import Data.List ( find )
import Data.Maybe ( fromMaybe, maybeToList )

import Control.Monad.Trans.MultiRWS




getDecls
  :: (Monad m, Functor m)
  => [QualifiedName]
  -> [HsTypeClass]
  -> TypeDeclMap
  -> [Module SrcSpanInfo]
  -> MultiRWST r w s m [Either String HsFunctionDecl]
getDecls ds tcs tDeclMap modules = fmap (>>= either (return.Left) (map Right))
                                $ sequence
                                $ do
  modul <- modules
  (mn, decls) <- maybeToList $ moduleNameAndDecls modul
  d <- decls
  return $ runExceptT $ transformDecl tcs ds mn tDeclMap d

transformDecl
  :: (Monad m, Functor m)
  => [HsTypeClass]
  -> [QualifiedName]
  -> ModuleName SrcSpanInfo
  -> TypeDeclMap
  -> Decl SrcSpanInfo
  -> ExceptT String (MultiRWST r w s m) [HsFunctionDecl]
transformDecl tcs ds mn tDeclMap (TypeSig _loc names qtype)
  = insName qtype $ do
      (ctype, _) <- convertType tcs (Just mn) ds tDeclMap qtype
      mapM (either throwE pure . helper mn ctype) names
transformDecl _ _ _ _ _ = return []

transformDecl'
  :: (MonadMultiState ConvData m, Monad m, Functor m)
  => [HsTypeClass]
  -> [QualifiedName]
  -> ModuleName SrcSpanInfo
  -> TypeDeclMap
  -> Decl SrcSpanInfo
  -> ExceptT String m [HsFunctionDecl]
transformDecl' tcs ds mn tDeclMap (TypeSig _loc names qtype)
  = insName qtype $ do
      ctype <- convertTypeInternal tcs (Just mn) ds tDeclMap qtype
      mapM (either throwE pure . helper mn ctype) names
transformDecl' _ _ _ _ _ = return []

insName :: (Functor m, Monad m)
        => Type SrcSpanInfo -> ExceptT String m a -> ExceptT String m a
insName qtype = withExceptT (\x -> x ++ " in " ++ prettyPrint qtype)

helper
  :: ModuleName SrcSpanInfo
  -> HsType
  -> Name SrcSpanInfo
  -> Either String HsFunctionDecl
helper mn t syntaxName = (, forallify t)
  <$> convertModuleNameChecked mn syntaxName

getDataConss
  :: (Monad m)
  => [HsTypeClass]
  -> [QualifiedName]
  -> TypeDeclMap
  -> [Module SrcSpanInfo]
  -> MultiRWST
       r
       w
       s
       m
       [Either String ([HsFunctionDecl], DeconstructorBinding)]
getDataConss tcs ds tDeclMap modules = sequence $ do
  modul <- modules
  (moduleName, decls) <- maybeToList $ moduleNameAndDecls modul
  DataDecl _ _ context rawHead conss _ <- decls
  let (name, params) = splitDeclHead rawHead
  let
    rTypeM :: ( MonadMultiState ConvData m )
           => ExceptT String m HsType
    rTypeM = do
      rName <- either throwE pure $ convertModuleNameChecked moduleName name
      ps  <- mapM pTransform params
      return $ (forallify . foldl TypeApp (TypeCons rName)) ps
    pTransform :: MonadMultiState ConvData m
               => TyVarBind SrcSpanInfo
               -> ExceptT String m HsType
    pTransform (KindedVar _ _ _) = throwE "kinded type variable"
    pTransform (UnkindedVar _ n) = TypeVar <$> getVar n
  --let
  --  tTransform (UnBangedTy t) = convertTypeInternal t
  --  tTransform x              = lift $ left $ "unknown Type: " ++ show x
  let
    typeM :: ( MonadMultiState ConvData m )
          => QualConDecl SrcSpanInfo
          -> ExceptT String m (QualifiedName, [HsType])
    typeM (QualConDecl _ cbindings constructorContext conDecl) = do
      case contextConstraints context of
        [] -> pure ()
        _  -> throwE "context in data type"
      case (fromMaybe [] cbindings, contextConstraints constructorContext) of
        ([], []) -> pure ()
        _       -> throwE "constraint or existential type for constructor"
      (cname,tys) <- case conDecl of
        ConDecl _ c t -> pure (c, t)
        x           -> throwE $ "unknown ConDecl: " ++ show x
      convTs <- convertTypeInternal tcs (Just moduleName) ds tDeclMap `mapM` tys
      qName <- either throwE pure $ convertModuleNameChecked moduleName cname
      return $ (qName, convTs)
  let
    addConsMsg = (++) $ show name ++ ": "
  let
    convAction :: ( MonadMultiState ConvData m )
               => ExceptT String m ([HsFunctionDecl], DeconstructorBinding)
    convAction = do
      rtype  <- rTypeM
      consDatas <- mapM typeM conss
      return $ ( [ (n, foldr TypeArrow rtype ts)
                 | (n, ts) <- consDatas
                 ]
               , DeconstructorBinding
                   rtype
                   [ConstructorBinding constructor fields
                   | (constructor, fields) <- consDatas]
                   False
               )
        -- TODO: actually determine if stuff is recursive or not
  return $ do
    convResult <- withMultiStateA (ConvData 0 M.empty) $ runExceptT convAction
    return $ either (Left . addConsMsg) Right convResult
    -- TODO: replace this by bimap..

getClassMethods
  :: (Monad m, Functor m)
  => [HsTypeClass]
  -> [QualifiedName]
  -> TypeDeclMap
  -> [Module SrcSpanInfo]
  -> MultiRWST r w s m [Either String HsFunctionDecl]
getClassMethods tcs ds tDeclMap modules = fmap join $ sequence $ do
  modul <- modules
  (moduleName, decls) <- maybeToList $ moduleNameAndDecls modul
  ClassDecl _ _ rawHead _ maybeClassDecls <- decls
  let (name, vars) = splitDeclHead rawHead
  let cdecls = fromMaybe [] maybeClassDecls
  return $ do
    let errorMod = (++) ("class method for "++show name++": ")
    case convertModuleNameChecked moduleName name of
      Left conversionError -> return [Left $ errorMod conversionError]
      Right className -> case find ((== className) . tclass_name) tcs of
        Nothing -> return [Left $ "unknown type class: " ++ show className]
        Just cls -> do
          let cnstrA = HsConstraint cls
                <$> mapM ((TypeVar <$>) . tyVarTransform) vars
          -- action :: MonadMultiState ConvData m
          --        => m [Either String [HsFunctionDecl]]
          rEithers <- withMultiStateA (ConvData 0 M.empty) $ do
            cnstrE <- runExceptT cnstrA
            case cnstrE of
              Left x -> return [Left x]
              Right cnstr ->
                mapM ( runExceptT
                     . fmap (map (addConstraint cnstr))
                     . transformDecl' tcs ds moduleName tDeclMap)
                  $ [ d | ClsDecl _ d <- cdecls ]
          let _ = rEithers :: [Either String [HsFunctionDecl]]
          return $ concatMap (either (return . Left . errorMod) (map Right))
                 $ rEithers
  where
    addConstraint :: HsConstraint -> HsFunctionDecl -> HsFunctionDecl
    addConstraint c (name, ty) = case forallify ty of
      TypeForall variables constraints body ->
        (name, TypeForall variables (c : constraints) body)
      -- 'forallify' always produces 'TypeForall'; retaining a total fallback
      -- makes this boundary robust if its normalization policy later changes.
      body -> (name, TypeForall [] [c] body)

-- | Checked extraction used by Exference itself.  Keeping construction
-- failures explicit matters because HSE syntax constructors are public and can
-- carry malformed module or occurrence spellings.
getDataTypesChecked
  :: [Module SrcSpanInfo]
  -> Either String [QualifiedName]
getDataTypesChecked modules = mapM (uncurry convertModuleNameChecked) $ d1 ++ d2
 where
  d1 = do
    modul <- modules
    (moduleName, decls) <- maybeToList $ moduleNameAndDecls modul
    DataDecl _ _ _ rawHead _ _ <- decls
    let (name, _) = splitDeclHead rawHead
    return (moduleName, name)
  d2 = do
    modul <- modules
    (moduleName, decls) <- maybeToList $ moduleNameAndDecls modul
    TypeDecl _ rawHead _ <- decls
    let (name, _) = splitDeclHead rawHead
    return (moduleName, name)

-- | Historical unchecked facade.  New code should use
-- 'getDataTypesChecked'; malformed hand-built HSE nodes cannot be represented
-- by the validated core name type.
getDataTypes :: [Module SrcSpanInfo] -> [QualifiedName]
getDataTypes = either (error . ("invalid data-type name: " ++)) id
  . getDataTypesChecked
