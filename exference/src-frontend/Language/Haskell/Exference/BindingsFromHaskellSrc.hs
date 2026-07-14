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
import Language.Haskell.Exference.Core.Declaration
  (deriveRecursiveDataMetadata)
import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.HaskellSrcUtils

import Control.Monad (foldM)
import Control.Monad.Trans.Except
import qualified Data.Map.Strict as M
import Data.Maybe ( fromMaybe, maybeToList )



-- | One constructor after source syntax has been lowered. Record selectors
-- remain attached long enough to create their ordinary value bindings without
-- confusing them with constructors in the shared declaration inventory.
data LoweredConstructor = LoweredConstructor
  { loweredConstructorName :: QualifiedName
  , loweredConstructorFields :: [HsType]
  , loweredRecordSelectors :: [(QualifiedName, HsType)]
  }


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

-- | Extract each data declaration's value-level constructor bindings followed
-- by its record selectors, plus the pattern-matching shape. A selector shared
-- by several constructors appears once at its first source occurrence.
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
      -> ConversionT String m LoweredConstructor
    typeM (QualConDecl _ cbindings constructorContext conDecl) = do
      case (fromMaybe [] cbindings, contextConstraints constructorContext) of
        ([], []) -> pure ()
        _       -> throwE "constraint or existential type for constructor"
      (syntaxConstructorName, fieldTypes, selectors) <- case conDecl of
        ConDecl _ occurrenceName types -> do
          converted <- mapM convertFieldType types
          pure (occurrenceName, converted, [])
        InfixConDecl _ left occurrenceName right -> do
          converted <- mapM convertFieldType [left, right]
          pure (occurrenceName, converted, [])
        RecDecl _ occurrenceName fields -> do
          convertedFields <- mapM convertRecordField fields
          pure
            ( occurrenceName
            , concatMap fst convertedFields
            , concatMap snd convertedFields
            )
      qualifiedConstructor <- either throwE pure
        $ convertModuleName moduleName syntaxConstructorName
      pure LoweredConstructor
        { loweredConstructorName = qualifiedConstructor
        , loweredConstructorFields = fieldTypes
        , loweredRecordSelectors = selectors
        }
     where
      -- Strictness and unpacking govern representation and evaluation, not a
      -- field's source type. Exference's inventory models the latter only.
      convertFieldType = convertTypeInternal
        tcs (Just moduleName) ds tDeclMap . eraseFieldAnnotations

      convertRecordField (FieldDecl _ names fieldType) = do
        convertedType <- convertFieldType fieldType
        qualifiedSelectors <- mapM
          (either throwE pure . convertModuleName moduleName) names
        pure
          ( convertedType <$ qualifiedSelectors
          , [(selector, convertedType) | selector <- qualifiedSelectors]
          )
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
      selectors <- deduplicateRecordSelectors
        $ concatMap loweredRecordSelectors consDatas
      -- The deconstructor records one use-site instance of the datatype, so
      -- its parameters must remain free for search-time unification.  Each
      -- constructor value is polymorphic independently; quantify only after
      -- assembling its complete field-to-result arrow.
      return $ ( [ functionBindingFromType
                    (loweredConstructorName constructor) 0
                    $ forallify
                    $ foldr TypeArrow rtype
                    $ loweredConstructorFields constructor
                 | constructor <- consDatas
                 ]
                 ++ [ functionBindingFromType selector 0
                        $ forallify $ TypeArrow rtype fieldType
                    | (selector, fieldType) <- selectors
                    ]
               , DeconstructorBinding
                   rtype
                   [ ConstructorBinding
                       (loweredConstructorName constructor)
                       (loweredConstructorFields constructor)
                   | constructor <- consDatas
                   ]
                   False
               )
  return $ fmap (either (Left . addConsMsg) Right)
    $ runExceptT $ runConversionT emptyConvData convAction

-- | HSE retains strictness and unpack annotations in the field's 'Type' node.
-- They are operational metadata, so remove any outer wrappers before the
-- parser-independent type conversion. Repeating the match also handles
-- hand-constructed ASTs without relying on the parser's normalization.
eraseFieldAnnotations :: Type SrcSpanInfo -> Type SrcSpanInfo
eraseFieldAnnotations (TyBang _ _ _ fieldType) =
  eraseFieldAnnotations fieldType
eraseFieldAnnotations (TyParen location fieldType) =
  TyParen location $ eraseFieldAnnotations fieldType
eraseFieldAnnotations fieldType = fieldType

-- | A selector shared by several record constructors denotes one top-level
-- function. Keep its first source occurrence and reject inconsistent types
-- instead of silently allowing map insertion order to choose a meaning.
deduplicateRecordSelectors
  :: Monad m
  => [(QualifiedName, HsType)]
  -> ConversionT String m [(QualifiedName, HsType)]
deduplicateRecordSelectors selectors = reverse . snd <$> foldM step
  (M.empty, []) selectors
 where
  step (seen, ordered) selector@(name, fieldType) = case M.lookup name seen of
    Nothing -> pure (M.insert name fieldType seen, selector : ordered)
    Just priorType
      | priorType == fieldType -> pure (seen, ordered)
      | otherwise -> throwE
          $ "record selector " ++ show name
          ++ " has inconsistent field types"

-- | Annotate recursion only after conversion: failures retain their original
-- positions and cannot create phantom vertices in the datatype graph.
markRecursiveDeconstructors
  :: [Either String ([FunctionBinding], DeconstructorBinding)]
  -> [Either String ([FunctionBinding], DeconstructorBinding)]
markRecursiveDeconstructors converted = map mark converted
 where
  classified = deriveRecursiveDataMetadata
    [binding | Right (_, binding) <- converted]
  recursiveByHead = M.fromList
    [ (headName, deconstructorRecursive binding)
    | binding <- classified
    , Just headName <- [typeConstructorHead $ deconstructorInput binding]
    ]

  mark failed@(Left _) = failed
  mark (Right (constructors, binding)) = Right
    ( constructors
    , binding
        { deconstructorRecursive = maybe False
            (\headName -> M.findWithDefault False headName recursiveByHead)
            $ typeConstructorHead $ deconstructorInput binding
        }
    )

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
