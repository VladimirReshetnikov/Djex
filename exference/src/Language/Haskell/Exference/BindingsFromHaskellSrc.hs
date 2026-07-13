{-# LANGUAGE TupleSections #-}

module Language.Haskell.Exference.BindingsFromHaskellSrc
  ( getDecls
  , getDataConss
  , getDataTypes
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
import Language.Haskell.Exference.HaskellSrcUtils

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
  -> m [Either String FunctionBinding]
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
  -> ExceptT String m [FunctionBinding]
transformDecl tcs ds mn tDeclMap declaration = case declaration of
  TypeSig _ names qtype -> lowerSignature names qtype
  -- A foreign import introduces an ordinary Haskell binding.  Its calling
  -- convention and external symbol affect execution, not the type-directed
  -- search inventory, so it crosses the same checked lowering path as a
  -- one-name source signature.  A foreign export merely refers to an existing
  -- binding and deliberately remains outside this extractor.
  ForImp _ _ _ _ name qtype -> lowerSignature [name] qtype
  _ -> pure []
 where
  lowerSignature names qtype = insName qtype $ do
    (ctype, _) <- convertType tcs (Just mn) ds tDeclMap qtype
    mapM (either throwE pure . helper mn ctype) names

insName :: Monad m
        => Type SrcSpanInfo -> ExceptT String m a -> ExceptT String m a
insName qtype = withExceptT (\x -> x ++ " in " ++ prettyPrint qtype)

helper
  :: ModuleName SrcSpanInfo
  -> HsType
  -> Name SrcSpanInfo
  -> Either String FunctionBinding
helper mn signature syntaxName = do
  name <- convertModuleName mn syntaxName
  pure $ functionBindingFromType name 0 $ forallify signature

getDataConss
  :: Monad m
  => M.Map QualifiedName HsTypeClass
  -> [QualifiedName]
  -> TypeDeclMap
  -> [Module SrcSpanInfo]
  -> m [Either String ([FunctionBinding], DeconstructorBinding)]
getDataConss tcs ds tDeclMap modules =
  fmap markRecursiveDeconstructors $ sequence $ do
  modul <- modules
  (moduleName, decls) <- maybeToList $ moduleNameAndDecls modul
  DataDecl _ _ context rawHead conss _ <- decls
  let (name, params) = splitDeclHead rawHead
  let
    rTypeM :: Monad m => ConversionT String m HsType
    rTypeM = do
      rName <- either throwE pure $ convertModuleName moduleName name
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
      case (fromMaybe [] cbindings, contextConstraints constructorContext) of
        ([], []) -> pure ()
        _       -> throwE "constraint or existential type for constructor"
      (cname,tys) <- case conDecl of
        ConDecl _ c t -> pure (c, t)
        x           -> throwE $ "unknown ConDecl: " ++ show x
      convTs <- convertTypeInternal tcs (Just moduleName) ds tDeclMap `mapM` tys
      qName <- either throwE pure $ convertModuleName moduleName cname
      return $ (qName, convTs)
  let
    addConsMsg = (++) $ show name ++ ": "
  let
    convAction
      :: Monad m
      => ConversionT String m ([FunctionBinding], DeconstructorBinding)
    convAction = do
      rtype  <- rTypeM
      -- A datatype context belongs to the declaration, not to each
      -- constructor. Checking it outside 'mapM typeM' also rejects contextual
      -- empty datatypes instead of accepting them vacuously.
      case contextConstraints context of
        [] -> pure ()
        _  -> throwE "context in data type"
      consDatas <- mapM typeM conss
      -- The deconstructor records one use-site instance of the datatype, so
      -- its parameters must remain free for search-time unification.  Each
      -- constructor value is polymorphic independently; quantify only after
      -- assembling its complete field-to-result arrow.
      return $ ( [ functionBindingFromType n 0
                    $ forallify $ foldr TypeArrow rtype ts
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
  :: [Either String ([FunctionBinding], DeconstructorBinding)]
  -> [Either String ([FunctionBinding], DeconstructorBinding)]
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

-- | Total extraction used by Exference itself. Keeping construction failures
-- explicit matters because HSE syntax constructors are public and can carry
-- malformed module or occurrence spellings.
getDataTypes
  :: [Module SrcSpanInfo]
  -> Either String [QualifiedName]
getDataTypes modules = mapM (uncurry convertModuleName) $ d1 ++ d2
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
