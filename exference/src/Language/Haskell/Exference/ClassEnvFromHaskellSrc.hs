{-# LANGUAGE FlexibleContexts #-}

module Language.Haskell.Exference.ClassEnvFromHaskellSrc
  ( ClassEnvironmentLoadError (..)
  , getClassEnv
  )
where

import Control.Monad (forM, when)
import Control.Monad.State.Lazy (MonadState, evalStateT, get)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
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
  }

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
getClassEnv dataTypes typeDeclarations modules = do
  classResults <- getTypeClasses dataTypes typeDeclarations modules
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
          pure $ (\environment -> (environment, length instances))
            <$> first InvalidClassEnvironment
              (mkStaticClassEnv (Map.elems classes) instances)

getTypeClasses
  :: Monad m
  => [QualifiedName]
  -> TypeDeclMap
  -> [Module SrcSpanInfo]
  -> m [Either String HsTypeClass]
getTypeClasses dataTypes typeDeclarations modules = do
  let namedDeclarations =
        [ RawTypeClass
            { rawClassName = checkedName
            , rawClassModule = moduleName
            , rawClassVariables = variables
            , rawClassContext = context
            }
        | modul <- modules
        , Just (moduleName, declarations) <- [moduleNameAndDecls modul]
        , ClassDecl _ context rawHead _ _ <- declarations
        , let (syntaxName, variables) = splitDeclHead rawHead
        , let checkedNameResult = convertModuleNameChecked moduleName syntaxName
        , checkedName <- either (const []) pure checkedNameResult
        ]
      invalidNames =
        [ Left $ "invalid type-class name: " ++ conversionError
        | modul <- modules
        , Just (moduleName, declarations) <- [moduleNameAndDecls modul]
        , ClassDecl _ _ rawHead _ _ <- declarations
        , let (syntaxName, _) = splitDeclHead rawHead
        , Left conversionError <- [convertModuleNameChecked moduleName syntaxName]
        ]
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
    result <- evalStateT (runExceptT $ do
      parameters <- mapM tyVarTransform $ rawClassVariables rawClass
      pure $ HsTypeClass (rawClassName rawClass) parameters [])
      emptyConversionState
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
    evalStateT (runExceptT (do
      parameters <- mapM tyVarTransform $ rawClassVariables rawClass
      superclasses <- mapM
        (convertClassConstraint headers
          (Just $ rawClassModule rawClass)
          dataTypes
          typeDeclarations)
        (contextConstraints $ rawClassContext rawClass)
      pure $ HsTypeClass (rawClassName rawClass) parameters superclasses))
      emptyConversionState

  pure $ invalidNames ++ duplicateErrors ++ headerErrors ++ elaborated
 where
  emptyConversionState = ConvData 0 Map.empty

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
  pure $ evalStateT (runExceptT $ do
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
        ConvData _ variables <- get
        let undeclared = Set.fromList (Map.elems variables)
              Set.\\ declaredIds
        when (not $ Set.null undeclared) $ throwE
          $ "instance uses variables outside its explicit forall: "
          ++ show (Set.toAscList undeclared)
    pure $ HsInstance prerequisites $ HsConstraint className arguments)
    (ConvData 0 Map.empty)

-- | Convert a class application against the complete closed class inventory.
-- Both superclass edges and instance prerequisites must name a declaration;
-- ordinary function signatures retain the frontend's open-world policy.
convertClassConstraint
  :: MonadState ConvData m
  => Map.Map QualifiedName HsTypeClass
  -> Maybe (ModuleName SrcSpanInfo)
  -> [QualifiedName]
  -> TypeDeclMap
  -> Asst SrcSpanInfo
  -> ExceptT String m HsConstraint
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
