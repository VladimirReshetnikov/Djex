module Language.Haskell.Exference.ClassEnvFromHaskellSrc
  ( ClassEnvironmentLoadError (..)
  , ClassMethodDeclaration (..)
  , getClassEnv
  , getClassEnvWithMethods
  , getClassMethodsForEnvironment
  )
where

import Control.Monad (forM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (evalStateT, get)
import Control.Monad.Trans.Except (runExceptT, throwE, withExceptT)
import Data.Either (lefts, rights)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils (forallify)
import Language.Haskell.Exference.Core.Declaration
  (addClassMethodConstraint, classMethodConstraint)
import Language.Haskell.Exference.FunctionDecl (HsFunctionDecl)
import Language.Haskell.Exference.HaskellSrcUtils
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc
import Language.Haskell.Exference.TypeFromHaskellSrc
import qualified Language.Haskell.Synthesis.Name as SharedName

-- Keeping the parser syntax alongside the checked nominal name lets the two
-- elaboration passes share one deterministic declaration inventory without
-- constructing a recursive graph of class values.
data RawTypeClass = RawTypeClass
  { rawClassName :: QualifiedName
  , rawClassModule :: ModuleName SrcSpanInfo
  , rawClassVariables :: [TyVarBind SrcSpanInfo]
  , rawClassContext :: Maybe (Context SrcSpanInfo)
  , rawClassDeclarations :: [ClassDecl SrcSpanInfo]
  }

-- | A source method paired with its exact, qualified owning class.  The flat
-- compatibility type already carries the derived owner constraint, so the tag
-- stores only nominal ownership rather than duplicating its parameter IDs.
data ClassMethodDeclaration = ClassMethodDeclaration
  { classMethodOwner :: QualifiedName
  , classMethodFunction :: HsFunctionDecl
  }
  deriving (Eq, Show)

-- | Fatal phases of source class-environment construction.  Returning these
-- explicitly prevents an observed-but-invalid class or instance from being
-- erased and later reinterpreted as an unknown external declaration.
data ClassEnvironmentLoadError
  = ClassDeclarationErrors (NonEmpty String)
  | InstanceDeclarationErrors (NonEmpty String)
  | InvalidClassEnvironment ClassEnvError
  deriving (Eq, Show)

-- | Return the environment and the number of valid source instances found
-- before superclass inflation.  Construction is transactional: no partial or
-- empty recovery environment is returned for malformed source declarations.
getClassEnv
  :: Monad m
  => [QualifiedName]
  -> TypeDeclMap
  -> [Module SrcSpanInfo]
  -> m (Either ClassEnvironmentLoadError (StaticClassEnv, Int))
getClassEnv dataTypes typeDeclarations modules = fmap
  (fmap $ \(environment, count, _) -> (environment, count))
  $ getClassEnvWithMethods dataTypes typeDeclarations modules

-- | Build the nominal class graph and elaborate each class body from the same
-- collected declarations.  Method results remain grouped by input module so
-- the loader can preserve its historical per-module binding/error order.
getClassEnvWithMethods
  :: Monad m
  => [QualifiedName]
  -> TypeDeclMap
  -> [Module SrcSpanInfo]
  -> m (Either ClassEnvironmentLoadError
      (StaticClassEnv, Int, [[Either String ClassMethodDeclaration]]))
getClassEnvWithMethods dataTypes typeDeclarations modules = do
  (classResults, rawClassesByModule) <-
    getTypeClasses dataTypes typeDeclarations modules
  case NonEmpty.nonEmpty $ lefts classResults of
    Just classErrors -> pure $ Left $ ClassDeclarationErrors classErrors
    Nothing -> do
      let classes = Map.fromList
            [ (tclass_name typeClass, typeClass)
            | typeClass <- rights classResults
            ]
      instanceResults <- getInstances classes dataTypes typeDeclarations modules
      case NonEmpty.nonEmpty $ lefts instanceResults of
        Just instanceErrors ->
          pure $ Left $ InstanceDeclarationErrors instanceErrors
        Nothing -> do
          let instances = rights instanceResults
          case first InvalidClassEnvironment
              (mkStaticClassEnv (Map.elems classes) instances) of
            Left failure -> pure $ Left failure
            Right environment -> do
              methods <- getClassMethodsFromRaw classes dataTypes
                typeDeclarations rawClassesByModule
              pure $ Right (environment, length instances, methods)

getTypeClasses
  :: Monad m
  => [QualifiedName]
  -> TypeDeclMap
  -> [Module SrcSpanInfo]
  -> m ([Either String HsTypeClass], [[Either String RawTypeClass]])
getTypeClasses dataTypes typeDeclarations modules = do
  let rawDeclarationsByModule = map rawTypeClasses modules
      namedDeclarationsByModule = map rights rawDeclarationsByModule
      namedDeclarations = concat namedDeclarationsByModule
      invalidNames = map Left $ concatMap lefts rawDeclarationsByModule
      declarationsByName = Map.fromListWith (++)
        [ (rawClassName rawClass, [rawClass])
        | rawClass <- namedDeclarations
        ]
      duplicateNames = Map.keys $ Map.filter ((> 1) . length) declarationsByName
      duplicateErrors = map (Left . duplicateClassMessage) duplicateNames
      uniqueDeclarations =
        [ rawClass
        | [rawClass] <- Map.elems declarationsByName
        ]

  -- Pass one elaborates only class heads.  The resulting strict map provides
  -- nominal identity and arity information to every superclass conversion in
  -- pass two; no lazy-map fixed point is involved.
  headerResults <- forM uniqueDeclarations $ \rawClass -> do
    result <- runExceptT $ runConversionT emptyConversionState $ do
      parameters <- mapM tyVarTransform $ rawClassVariables rawClass
      pure $ HsTypeClass (rawClassName rawClass) parameters []
    pure $ (,) rawClass <$> result
  let headerErrors = [Left errorMessage | Left errorMessage <- headerResults]
      successfulHeaders = rights headerResults
      headers = Map.fromList
        [ (tclass_name header, header)
        | (_, header) <- successfulHeaders
        ]

  -- Pass two deliberately binds every head variable before touching the
  -- superclass context.  Thus parameter IDs follow declaration order even if
  -- superclasses mention those variables in a different order (or not at all).
  elaborated <- forM successfulHeaders $ \(rawClass, _) ->
    runExceptT $ runConversionT emptyConversionState $ do
      parameters <- mapM tyVarTransform $ rawClassVariables rawClass
      superclasses <- mapM
        (convertClassConstraint headers
          (Just $ rawClassModule rawClass)
          dataTypes
          typeDeclarations)
        (contextConstraints $ rawClassContext rawClass)
      pure $ HsTypeClass (rawClassName rawClass) parameters superclasses

  pure
    ( invalidNames ++ duplicateErrors ++ headerErrors ++ elaborated
    , rawDeclarationsByModule
    )
 where
  emptyConversionState = ConvData 0 Map.empty

rawTypeClasses
  :: Module SrcSpanInfo
  -> [Either String RawTypeClass]
rawTypeClasses modul = do
  (moduleName, declarations) <- maybeToList $ moduleNameAndDecls modul
  ClassDecl _ context rawHead _ maybeClassDecls <- declarations
  let (syntaxName, variables) = splitDeclHead rawHead
  pure $ case convertModuleNameChecked moduleName syntaxName of
    Left conversionError ->
      Left $ "invalid type-class name: " ++ conversionError
    Right checkedName -> Right RawTypeClass
      { rawClassName = checkedName
      , rawClassModule = moduleName
      , rawClassVariables = variables
      , rawClassContext = context
      , rawClassDeclarations = fromMaybe [] maybeClassDecls
      }

-- | Compatibility extraction over an already validated nominal class table.
-- The production loader receives these results directly from
-- 'getClassEnvWithMethods', avoiding a second class-body traversal.
getClassMethodsForEnvironment
  :: Monad m
  => Map.Map QualifiedName HsTypeClass
  -> [QualifiedName]
  -> TypeDeclMap
  -> [Module SrcSpanInfo]
  -> m [[Either String ClassMethodDeclaration]]
getClassMethodsForEnvironment classes dataTypes typeDeclarations =
  getClassMethodsFromRaw classes dataTypes typeDeclarations
    . map rawTypeClasses

getClassMethodsFromRaw
  :: Monad m
  => Map.Map QualifiedName HsTypeClass
  -> [QualifiedName]
  -> TypeDeclMap
  -> [[Either String RawTypeClass]]
  -> m [[Either String ClassMethodDeclaration]]
getClassMethodsFromRaw classes dataTypes typeDeclarations =
  mapM $ fmap concat . mapM elaborate
 where
  elaborate (Left failure) = pure [Left failure]
  elaborate (Right rawClass) = elaborateRawClass
    classes dataTypes typeDeclarations rawClass

elaborateRawClass
  :: Monad m
  => Map.Map QualifiedName HsTypeClass
  -> [QualifiedName]
  -> TypeDeclMap
  -> RawTypeClass
  -> m [Either String ClassMethodDeclaration]
elaborateRawClass classes dataTypes typeDeclarations rawClass =
  case Map.lookup (rawClassName rawClass) classes of
    Nothing -> pure
      [Left $ "unknown type class: " ++ show (rawClassName rawClass)]
    Just typeClass -> flip evalStateT (ConvData 0 Map.empty) $ do
      parameterResult <- runExceptT
        $ mapM tyVarTransform $ rawClassVariables rawClass
      case parameterResult of
        Left failure -> pure [Left failure]
        Right parameters -> do
          let ownerConstraint = classMethodConstraint typeClass
              expectedOwner = HsConstraint (rawClassName rawClass)
                $ map TypeVar parameters
          if ownerConstraint /= expectedOwner
            then pure
              [Left "class parameter allocation disagrees with its header"]
            else do
              results <- mapM
                (runExceptT . transformClassDeclaration ownerConstraint)
                $ rawClassDeclarations rawClass
              let prefix = "class method for "
                    ++ unqualifiedClassName (rawClassName rawClass) ++ ": "
              pure $ concatMap
                (either (pure . Left . (prefix ++)) (map Right)) results
 where
  transformClassDeclaration owner (ClsDecl _ declaration) =
    transformMethodDeclaration owner declaration
  transformClassDeclaration _ _ = pure []

  transformMethodDeclaration owner (TypeSig _ names signature) =
    withExceptT (\failure -> failure ++ " in " ++ prettyPrint signature) $ do
      converted <- convertTypeInternal classes
        (Just $ rawClassModule rawClass) dataTypes typeDeclarations signature
      mapM (convertedMethod owner converted) names
  transformMethodDeclaration _ _ = pure []

  convertedMethod owner converted syntaxName = do
    methodName <- either throwE pure
      $ convertModuleNameChecked (rawClassModule rawClass) syntaxName
    let methodType = addClassMethodConstraint owner $ forallify converted
    pure $ ClassMethodDeclaration
      (rawClassName rawClass) (methodName, methodType)

getInstances
  :: Monad m
  => Map.Map QualifiedName HsTypeClass
  -> [QualifiedName]
  -> TypeDeclMap
  -> [Module SrcSpanInfo]
  -> m [Either String HsInstance]
getInstances classes dataTypes typeDeclarations modules = sequence $ do
  modul <- modules
  (moduleName, declarations) <- maybeToList $ moduleNameAndDecls modul
  InstDecl _ _ rule _ <- declarations
  (explicitVariables, context, syntaxName, argumentSyntax) <-
    maybeToList $ splitInstRule rule
  pure $ runExceptT $ runConversionT (ConvData 0 Map.empty) $ do
    explicitIds <- case explicitVariables of
      Nothing -> pure Nothing
      Just variables -> do
        ids <- mapM tyVarTransform variables
        when (Set.size (Set.fromList ids) /= length ids)
          $ throwE "duplicate explicitly quantified instance variable"
        pure $ Just $ Set.fromList ids
    className <- either throwE pure
      $ convertQName (Just moduleName) (Map.keys classes) syntaxName
    case Map.lookup className classes of
      Nothing -> throwE $ "unknown type class: " ++ show className
      Just _ -> pure ()
    prerequisites <- mapM
      (convertClassConstraint classes (Just moduleName)
        dataTypes typeDeclarations)
      (contextConstraints context)
    arguments <- mapM
      (convertTypeInternal classes (Just moduleName)
        dataTypes typeDeclarations)
      argumentSyntax
    either throwE pure
      $ validateConstraintArity classes className (length arguments)
    case explicitIds of
      Nothing -> pure ()
      Just declaredIds -> do
        ConvData _ variables <- lift get
        let undeclared = Set.fromList (Map.elems variables)
              Set.\\ declaredIds
        when (not $ Set.null undeclared) $ throwE
          $ "instance uses variables outside its explicit forall: "
          ++ show (Set.toAscList undeclared)
    pure $ HsInstance prerequisites $ HsConstraint className arguments

-- | Convert a class application against the complete closed class inventory.
-- Both superclass edges and instance prerequisites must name a declaration;
-- ordinary function signatures retain the frontend's open-world policy.
convertClassConstraint
  :: Monad m
  => Map.Map QualifiedName HsTypeClass
  -> Maybe (ModuleName SrcSpanInfo)
  -> [QualifiedName]
  -> TypeDeclMap
  -> Asst SrcSpanInfo
  -> ConversionT String m HsConstraint
convertClassConstraint classes defaultModule dataTypes
    typeDeclarations (TypeA _ classType) = do
  (syntaxName, argumentSyntax) <- maybe
    (throwE $ "invalid class constraint: " ++ prettyPrint classType)
    pure
    (splitClassApplication classType)
  className <- either throwE pure
    $ convertQName defaultModule (Map.keys classes) syntaxName
  case Map.lookup className classes of
    Nothing -> throwE $ "unknown type class: " ++ show className
    _ -> pure ()
  arguments <- mapM
    (convertTypeInternal classes defaultModule dataTypes typeDeclarations)
    argumentSyntax
  either throwE pure
    $ validateConstraintArity classes className (length arguments)
  pure $ HsConstraint className arguments
convertClassConstraint classes defaultModule dataTypes
    typeDeclarations (ParenA _ constraint) =
  convertClassConstraint classes defaultModule dataTypes
    typeDeclarations constraint
convertClassConstraint _ _ _ _ constraint =
  throwE $ "unknown class constraint: " ++ show constraint

duplicateClassMessage :: QualifiedName -> String
duplicateClassMessage name =
  "duplicate type class: " ++ unqualifiedClassName name
    ++ qualification
 where
  qualification
    | show name == unqualifiedClassName name = ""
    | otherwise = " (" ++ show name ++ ")"

unqualifiedClassName :: QualifiedName -> String
unqualifiedClassName name = fromMaybe (show name)
  $ SharedName.nameSpelling
  $ toSynthesisName name
