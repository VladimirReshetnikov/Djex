{-# LANGUAGE TupleSections #-}

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
import Control.Monad.Trans.State.Lazy (evalStateT)
import Control.Monad.Trans.Except
import Data.Graph ( SCC (..), stronglyConnComp )
import qualified Data.Map.Strict as M
import Data.Maybe ( fromMaybe, maybeToList )
import qualified Data.Set as S
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Type as SharedType




getDecls
  :: Monad m
  => [QualifiedName]
  -> M.Map QualifiedName HsTypeClass
  -> TypeDeclMap
  -> [Module SrcSpanInfo]
  -> m [Either String HsFunctionDecl]
getDecls ds tcs tDeclMap modules = fmap (>>= either (return.Left) (map Right))
                                $ sequence
                                $ do
  modul <- modules
  (mn, decls) <- maybeToList $ moduleNameAndDecls modul
  d <- decls
  return $ runExceptT $ transformDecl tcs ds mn tDeclMap d

transformDecl
  :: Monad m
  => M.Map QualifiedName HsTypeClass
  -> [QualifiedName]
  -> ModuleName SrcSpanInfo
  -> TypeDeclMap
  -> Decl SrcSpanInfo
  -> ExceptT String m [HsFunctionDecl]
transformDecl tcs ds mn tDeclMap (TypeSig _loc names qtype)
  = insName qtype $ do
      (ctype, _) <- convertType tcs (Just mn) ds tDeclMap qtype
      mapM (either throwE pure . helper mn ctype) names
transformDecl _ _ _ _ _ = return []

transformDecl'
  :: Monad m
  => M.Map QualifiedName HsTypeClass
  -> [QualifiedName]
  -> ModuleName SrcSpanInfo
  -> TypeDeclMap
  -> Decl SrcSpanInfo
  -> ConversionT String m [HsFunctionDecl]
transformDecl' tcs ds mn tDeclMap (TypeSig _loc names qtype)
  = insName qtype $ do
      ctype <- convertTypeInternal tcs (Just mn) ds tDeclMap qtype
      mapM (either throwE pure . helper mn ctype) names
transformDecl' _ _ _ _ _ = return []

insName :: Monad m
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
  :: Monad m
  => M.Map QualifiedName HsTypeClass
  -> [QualifiedName]
  -> TypeDeclMap
  -> [Module SrcSpanInfo]
  -> m [Either String ([HsFunctionDecl], DeconstructorBinding)]
getDataConss tcs ds tDeclMap modules =
  fmap markRecursiveDeconstructors $ sequence $ do
  modul <- modules
  (moduleName, decls) <- maybeToList $ moduleNameAndDecls modul
  DataDecl _ _ context rawHead conss _ <- decls
  let (name, params) = splitDeclHead rawHead
  let
    rTypeM :: Monad m => ConversionT String m HsType
    rTypeM = do
      rName <- either throwE pure $ convertModuleNameChecked moduleName name
      ps  <- mapM pTransform params
      return $ foldl TypeApp (TypeCons rName) ps
    pTransform
      :: Monad m
      => TyVarBind SrcSpanInfo
      -> ConversionT String m HsType
    pTransform (KindedVar _ _ _) = throwE "kinded type variable"
    pTransform (UnkindedVar _ n) = TypeVar <$> getVar n
  --let
  --  tTransform (UnBangedTy t) = convertTypeInternal t
  --  tTransform x              = lift $ left $ "unknown Type: " ++ show x
  let
    typeM
      :: Monad m
      => QualConDecl SrcSpanInfo
      -> ConversionT String m (QualifiedName, [HsType])
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
    convAction
      :: Monad m
      => ConversionT String m ([HsFunctionDecl], DeconstructorBinding)
    convAction = do
      rtype  <- rTypeM
      consDatas <- mapM typeM conss
      -- The deconstructor records one use-site instance of the datatype, so
      -- its parameters must remain free for search-time unification.  Each
      -- constructor value is polymorphic independently; quantify only after
      -- assembling its complete field-to-result arrow.
      return $ ( [ (n, forallify $ foldr TypeArrow rtype ts)
                 | (n, ts) <- consDatas
                 ]
               , DeconstructorBinding
                   rtype
                   [ConstructorBinding constructor fields
                   | (constructor, fields) <- consDatas]
                   False
               )
  return $ fmap (either (Left . addConsMsg) Right)
    $ runExceptT $ runConversionT (ConvData 0 M.empty) convAction

-- | Annotate recursion only after conversion: failures retain their original
-- positions and cannot create phantom vertices in the datatype graph.
markRecursiveDeconstructors
  :: [Either String ([HsFunctionDecl], DeconstructorBinding)]
  -> [Either String ([HsFunctionDecl], DeconstructorBinding)]
markRecursiveDeconstructors converted = map mark converted
 where
  successful =
    [ (toSynthesisName headName, binding)
    | Right (_, binding) <- converted
    , Just headName <- [typeConstructorHead $ deconstructorInput binding]
    ]
  knownHeads = S.fromList $ map fst successful
  dependenciesByHead = M.fromListWith S.union
    [ ( headName
      , S.intersection knownHeads $ constructorTypeHeads binding
      )
    | (headName, binding) <- successful
    ]
  graphNodes =
    [ (headName, headName, S.toList dependencies)
    | (headName, dependencies) <- M.toList dependenciesByHead
    ]
  recursiveHeads = S.fromList
    [headName | CyclicSCC component <- stronglyConnComp graphNodes
              , headName <- component]

  mark failed@(Left _) = failed
  mark (Right (constructors, binding)) = Right
    ( constructors
    , binding
        { deconstructorRecursive = maybe False
            ((`S.member` recursiveHeads) . toSynthesisName)
            $ typeConstructorHead $ deconstructorInput binding
        }
    )

-- The common traversal includes arrows, applications, forall bodies, and
-- constraint arguments, so frontend recursion follows the shared type model.
constructorTypeHeads :: DeconstructorBinding -> S.Set SharedName.Name
constructorTypeHeads = foldMap
    (foldMap (SharedType.typeConstructors . toSynthesisTypeStructure)
      . constructorFields)
  . deconstructorConstructors

getClassMethods
  :: Monad m
  => M.Map QualifiedName HsTypeClass
  -> [QualifiedName]
  -> TypeDeclMap
  -> [Module SrcSpanInfo]
  -> m [Either String HsFunctionDecl]
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
      Right className -> case M.lookup className tcs of
        Nothing -> return [Left $ "unknown type class: " ++ show className]
        Just _ -> do
          let cnstrA = HsConstraint className
                <$> mapM ((TypeVar <$>) . tyVarTransform) vars
          -- Keep the class head and every method in one state scope so each
          -- class parameter retains the same variable ID throughout.
          rEithers <- flip evalStateT (ConvData 0 M.empty) $ do
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
