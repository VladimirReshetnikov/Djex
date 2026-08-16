{-# LANGUAGE TupleSections #-}

module Language.Haskell.Exference.BindingsFromHaskellSrc
  ( getDecls
  , getDeclsLocated
  , getDeclsSourced
  , getDeclsSourcedWithResolver
  , getDataConss
  , getDataConssLocated
  , getDataConssSourced
  , getDataConssSourcedWithResolver
  , getDataTypes
  , getDataTypesLocated
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
import Language.Haskell.Exference.ExtractionError
import qualified Language.Haskell.Synthesis.Type as SharedType

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Control.Monad.Trans.Except
import qualified Data.Map.Strict as M
import Data.Maybe ( fromMaybe, maybeToList )
import Numeric.Natural (Natural)



-- | One constructor after source syntax has been lowered. Record selectors
-- remain attached long enough to create their ordinary value bindings without
-- confusing them with constructors in the shared declaration inventory.
data LoweredConstructor = LoweredConstructor
  { loweredConstructorName :: QualifiedName
  , loweredConstructorFields :: [HsType]
  , loweredRecordSelectors :: [(QualifiedName, HsType)]
  }


-- | Extract a 'FunctionBinding' for every name in every top-level type
-- signature and foreign import of the given modules, resolving names with
-- the unique-global 'legacyTypeResolver' built from the data type list and
-- class map. Failures are reported as plain messages; use
-- 'getDeclsLocated' to keep the source span of the offending signature.
getDecls
  :: Monad m
  => [QualifiedName]
  -> M.Map QualifiedName HsTypeClass
  -> TypeDeclMap
  -> [Module SrcSpanInfo]
  -> m [Either String FunctionBinding]
getDecls ds tcs tDeclMap = fmap (map (first extractionErrorMessage))
  . getDeclsLocated ds tcs tDeclMap

-- | Located core of 'getDecls': every failure carries the owning
-- signature's source span. The string entry point is its exact message
-- projection.
getDeclsLocated
  :: Monad m
  => [QualifiedName]
  -> M.Map QualifiedName HsTypeClass
  -> TypeDeclMap
  -> [Module SrcSpanInfo]
  -> m [Either ExtractionError FunctionBinding]
getDeclsLocated ds tcs tDeclMap modules = fmap
  (concatMap flattenSourcedExtraction . concat)
  $ mapM (getDeclsSourced ds tcs tDeclMap) modules

-- | Extract ordinary signatures as module-local source batches. Keeping all
-- names from one signature together makes its source position authoritative
-- even when conversion produces several function bindings.
getDeclsSourced
  :: Monad m
  => [QualifiedName]
  -> M.Map QualifiedName HsTypeClass
  -> TypeDeclMap
  -> Module SrcSpanInfo
  -> m [SourcedExtraction [FunctionBinding]]
getDeclsSourced ds tcs = getDeclsSourcedWithResolver
  (legacyTypeResolver tcs ds)

-- | Extract signatures using the exact source scope of this module.
getDeclsSourcedWithResolver
  :: Monad m
  => TypeResolver
  -> TypeDeclMap
  -> Module SrcSpanInfo
  -> m [SourcedExtraction [FunctionBinding]]
getDeclsSourcedWithResolver resolver tDeclMap modul = sequence $ do
  (mn, declarations) <- maybeToList $ moduleNameAndDecls modul
  (slot, declaration) <- zip [0 :: Natural ..] declarations
  case declaration of
    TypeSig{} -> extract slot mn declaration
    ForImp{} -> extract slot mn declaration
    _ -> []
 where
  extract slot moduleName declaration = pure $ do
    result <- fmap (first $ extractionErrorAt $ ann declaration)
      $ runExceptT
      $ transformDeclWithResolver resolver moduleName tDeclMap declaration
    pure $ SourcedExtraction (SourceSlot slot 0) result

transformDeclWithResolver
  :: Monad m
  => TypeResolver
  -> ModuleName SrcSpanInfo
  -> TypeDeclMap
  -> Decl SrcSpanInfo
  -> ExceptT String m [FunctionBinding]
transformDeclWithResolver resolver mn tDeclMap declaration = case declaration of
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
    (ctype, _) <- convertTypeWithResolver resolver (Just mn) tDeclMap qtype
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
getDataConss tcs ds tDeclMap = fmap (map (first extractionErrorMessage))
  . getDataConssLocated tcs ds tDeclMap

-- | Located core of 'getDataConss': every failure carries the owning data
-- declaration's source span. The string entry point is its exact message
-- projection.
getDataConssLocated
  :: Monad m
  => M.Map QualifiedName HsTypeClass
  -> [QualifiedName]
  -> TypeDeclMap
  -> [Module SrcSpanInfo]
  -> m [Either ExtractionError ([FunctionBinding], DeconstructorBinding)]
getDataConssLocated tcs ds tDeclMap modules = do
  sourced <- concat <$> mapM (getDataConssSourced tcs ds tDeclMap) modules
  pure $ markRecursiveDeconstructors
    $ map sourcedExtractionResult sourced

-- | Extract datatype constructor batches with their module-local top-level
-- declaration slots. Recursive metadata is classified across the supplied
-- module exactly as it was at the historical loader boundary.
getDataConssSourced
  :: Monad m
  => M.Map QualifiedName HsTypeClass
  -> [QualifiedName]
  -> TypeDeclMap
  -> Module SrcSpanInfo
  -> m
       [ SourcedExtraction
           ([FunctionBinding], DeconstructorBinding)
       ]
getDataConssSourced tcs ds = getDataConssSourcedWithResolver
  (legacyTypeResolver tcs ds)

-- | Extract datatype fields and selectors using the module's source scope.
getDataConssSourcedWithResolver
  :: Monad m
  => TypeResolver
  -> TypeDeclMap
  -> Module SrcSpanInfo
  -> m
       [ SourcedExtraction
           ([FunctionBinding], DeconstructorBinding)
       ]
getDataConssSourcedWithResolver resolver tDeclMap modul = do
  sourced <- sequence $ do
    (moduleName, declarations) <- maybeToList $ moduleNameAndDecls modul
    (slot, declaration@(DataDecl _ _ context rawHead conss _)) <-
      zip [0 :: Natural ..] declarations
    pure $ extractDataDeclarationWithResolver
      resolver tDeclMap slot moduleName declaration
      context rawHead conss
  let marked = markRecursiveDeconstructors
        $ map sourcedExtractionResult sourced
  pure $ zipWith
    (\entry result -> entry {sourcedExtractionResult = result})
    sourced marked

extractDataDeclarationWithResolver
  :: Monad m
  => TypeResolver
  -> TypeDeclMap
  -> Natural
  -> ModuleName SrcSpanInfo
  -> Decl SrcSpanInfo
  -> Maybe (Context SrcSpanInfo)
  -> DeclHead SrcSpanInfo
  -> [QualConDecl SrcSpanInfo]
  -> m
       ( SourcedExtraction
           ([FunctionBinding], DeconstructorBinding)
       )
extractDataDeclarationWithResolver resolver tDeclMap slot moduleName declaration
    context rawHead conss = do
  let (name, params) = splitDeclHead rawHead
  let
    rTypeM :: Monad m => ConversionT String m HsType
    rTypeM = do
      rName <- either throwE pure $ convertModuleName moduleName name
      ps  <- mapM pTransform params
      return $ SharedType.applyTypeArguments (TypeCons rName) ps
    pTransform
      :: Monad m
      => TyVarBind SrcSpanInfo
      -> ConversionT String m HsType
    pTransform binder = TypeVar <$> tyVarTransform binder
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
      convertFieldType = convertTypeInternalWithResolver
        resolver (Just moduleName) tDeclMap . eraseFieldAnnotations

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
                    $ SharedType.functionType
                        (loweredConstructorFields constructor)
                        rtype
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
  result <- fmap
      (either (Left . extractionErrorAt (ann declaration) . addConsMsg) Right)
    $ runExceptT $ runConversionT emptyConvData convAction
  pure $ SourcedExtraction (SourceSlot slot 0) result

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
  :: [Either failure ([FunctionBinding], DeconstructorBinding)]
  -> [Either failure ([FunctionBinding], DeconstructorBinding)]
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
getDataTypes = first extractionErrorMessage . getDataTypesLocated

-- | Located core of 'getDataTypes': a malformed head name is reported at the
-- name's own source span. The string entry point is its exact message
-- projection.
getDataTypesLocated
  :: [Module SrcSpanInfo]
  -> Either ExtractionError [QualifiedName]
getDataTypesLocated modules = mapM convert $ d1 ++ d2
 where
  convert (moduleName, name) = first (extractionErrorAt $ ann name)
    $ convertModuleName moduleName name
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
