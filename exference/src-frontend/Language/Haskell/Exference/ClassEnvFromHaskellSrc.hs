module Language.Haskell.Exference.ClassEnvFromHaskellSrc
  ( ClassEnvironmentLoadError (..)
  , ClassMethodDeclaration (..)
  , LoadedClassEnvironment (..)
  , loadClassEnvironment
  , loadClassEnvironmentSourced
  )
where

import Control.Monad (forM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (evalStateT, get)
import Control.Monad.Trans.Except (runExceptT, throwE, withExceptT)
import Data.Either (lefts, rights)
import Data.Bifunctor (bimap, first)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe, maybeToList)
import Numeric.Natural (Natural)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils (forallify)
import Language.Haskell.Exference.Core.Declaration
  (addClassMethodConstraint, classMethodConstraint)
import Language.Haskell.Exference.ExtractionError
import Language.Haskell.Exference.HaskellSrcUtils
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc
import Language.Haskell.Exference.TypeFromHaskellSrc
import qualified Language.Haskell.Synthesis.Collection as SharedCollection
import qualified Language.Haskell.Synthesis.Count as SharedCount
import qualified Language.Haskell.Synthesis.Name as SharedName

-- Keeping the parser syntax alongside the checked nominal name lets the two
-- elaboration passes share one deterministic declaration inventory without
-- constructing a recursive graph of class values.
data RawTypeClass = RawTypeClass
  { rawClassName :: QualifiedName
  , rawClassSlot :: SourceSlot
  , rawClassSpan :: SrcSpanInfo
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
  , classMethodFunction :: FunctionBinding
  }
  deriving (Eq, Show)

-- | Fatal phases of source class-environment construction.  Returning these
-- explicitly prevents an observed-but-invalid class or instance from being
-- erased and later reinterpreted as an unknown external declaration.
data ClassEnvironmentLoadError
  = ClassDeclarationErrors (NonEmpty ExtractionError)
  | InstanceDeclarationErrors (NonEmpty ExtractionError)
  | InvalidClassEnvironment ClassEnvError
  deriving (Eq, Show)

-- | The complete result of one transactional class-declaration pass. Source
-- instance count deliberately precedes superclass inflation, and method
-- groups align one-for-one with the input modules so the outer loader can
-- retain declaration and diagnostic order.
data LoadedClassEnvironment = LoadedClassEnvironment
  { loadedStaticClassEnvironment :: StaticClassEnv
  , loadedSourceInstanceCount :: Natural
  , loadedClassMethodsByModule
      :: [[Either ExtractionError ClassMethodDeclaration]]
  }
  deriving (Eq, Show)

-- | Build the nominal class graph and elaborate each class body from the same
-- collected declarations.  Method results remain grouped by input module so
-- the loader can preserve its historical per-module binding/error order.
loadClassEnvironment
  :: Monad m
  => [QualifiedName]
  -> TypeDeclMap
  -> [Module SrcSpanInfo]
  -> m (Either ClassEnvironmentLoadError
      LoadedClassEnvironment)
loadClassEnvironment dataTypes typeDeclarations modules =
  fmap (fmap fst)
    $ loadClassEnvironmentSourced dataTypes typeDeclarations modules

-- | Class loading with method extraction batches retained at their
-- module-local declaration slots. The first component is the exact historical
-- flat projection; the second is consumed by the complete source loader when
-- merging methods with datatype and ordinary-signature extraction.
loadClassEnvironmentSourced
  :: Monad m
  => [QualifiedName]
  -> TypeDeclMap
  -> [Module SrcSpanInfo]
  -> m
       ( Either ClassEnvironmentLoadError
           ( LoadedClassEnvironment
           , [[SourcedExtraction [ClassMethodDeclaration]]]
           )
       )
loadClassEnvironmentSourced dataTypes typeDeclarations modules = do
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
              methodBatches <- getClassMethodsFromRaw classes dataTypes
                typeDeclarations rawClassesByModule
              let methods = map
                    (concatMap flattenSourcedExtraction) methodBatches
              pure $ Right
                ( LoadedClassEnvironment
                    { loadedStaticClassEnvironment = environment
                    , loadedSourceInstanceCount =
                        SharedCount.naturalLength instances
                    , loadedClassMethodsByModule = methods
                    }
                , methodBatches
                )

getTypeClasses
  :: Monad m
  => [QualifiedName]
  -> TypeDeclMap
  -> [Module SrcSpanInfo]
  -> m ( [Either ExtractionError HsTypeClass]
       , [[Either ExtractionError RawTypeClass]]
       )
getTypeClasses dataTypes typeDeclarations modules = do
  let rawDeclarationsByModule = map rawTypeClasses modules
      -- Number every declaration before the global nominal passes.  The maps
      -- below still provide order-independent lookup, while these slots let us
      -- project each result back to exact module/declaration source order.
      declarationSlots = zip [0 :: Natural ..]
        $ concat rawDeclarationsByModule
      namedDeclarations =
        [ (slot, rawClass)
        | (slot, Right rawClass) <- declarationSlots
        ]
      invalidNames = Map.fromList
        [ (slot, Left failure)
        | (slot, Left failure) <- declarationSlots
        ]
      -- fromListWith calls its combining function as new-then-old, so flip
      -- keeps each occurrence list in source order.
      declarationsByName = Map.fromListWith (flip (++))
        [ (rawClassName rawClass, [(slot, rawClass)])
        | (slot, rawClass) <- namedDeclarations
        ]
      duplicateNames = Set.fromList
        [ name
        | (name, _ : _ : _) <- Map.toAscList declarationsByName
        ]
      duplicateErrors = Map.fromList
        [ (firstSlot, Left $ extractionErrorAt (rawClassSpan firstClass)
            (duplicateClassMessage name)
          )
        | (name, (firstSlot, firstClass) : _ : _) <-
            Map.toAscList declarationsByName
        ]
      uniqueDeclarations =
        [ (slot, rawClass)
        | (slot, rawClass) <- namedDeclarations
        , Set.notMember (rawClassName rawClass) duplicateNames
        ]

  -- Pass one elaborates only class heads.  The resulting strict map provides
  -- nominal identity and arity information to every superclass conversion in
  -- pass two; no lazy-map fixed point is involved.
  headerResults <- forM uniqueDeclarations $ \(slot, rawClass) -> do
    result <- runExceptT $ runConversionT emptyConversionState $ do
      parameters <- mapM tyVarTransform $ rawClassVariables rawClass
      pure $ HsTypeClass (rawClassName rawClass) parameters []
    pure (slot, bimap (extractionErrorAt $ rawClassSpan rawClass)
      ((,) rawClass) result)
  let headerErrors = Map.fromList
        [ (slot, Left errorMessage)
        | (slot, Left errorMessage) <- headerResults
        ]
      successfulHeaders =
        [ (slot, rawClass, header)
        | (slot, Right (rawClass, header)) <- headerResults
        ]
      headers = Map.fromList
        [ (tclass_name header, header)
        | (_, _, header) <- successfulHeaders
        ]

  -- Pass two deliberately binds every head variable before touching the
  -- superclass context.  Thus parameter IDs follow declaration order even if
  -- superclasses mention those variables in a different order (or not at all).
  elaborated <- forM successfulHeaders $ \(slot, rawClass, _) -> do
    result <- fmap (first $ extractionErrorAt $ rawClassSpan rawClass)
      $ runExceptT $ runConversionT emptyConversionState $ do
        parameters <- mapM tyVarTransform $ rawClassVariables rawClass
        superclasses <- mapM
          (convertClassConstraint headers
            (Just $ rawClassModule rawClass)
            dataTypes
            typeDeclarations)
          (contextConstraints $ rawClassContext rawClass)
        pure $ HsTypeClass (rawClassName rawClass) parameters superclasses
    pure (slot, result)

  let resultsBySourceSlot = Map.unions
        [ invalidNames
        , duplicateErrors
        , headerErrors
        , Map.fromList elaborated
        ]
  pure (Map.elems resultsBySourceSlot, rawDeclarationsByModule)
 where
  emptyConversionState = emptyConvData

rawTypeClasses
  :: Module SrcSpanInfo
  -> [Either ExtractionError RawTypeClass]
rawTypeClasses modul = do
  (moduleName, declarations) <- maybeToList $ moduleNameAndDecls modul
  (slot, ClassDecl declSpan context rawHead _ maybeClassDecls) <-
    zip [0 :: Natural ..] declarations
  let (syntaxName, variables) = splitDeclHead rawHead
  pure $ case convertModuleName moduleName syntaxName of
    Left conversionError -> Left $ extractionErrorAt declSpan
      $ "invalid type-class name: " ++ conversionError
    Right checkedName -> Right RawTypeClass
      { rawClassName = checkedName
      , rawClassSlot = SourceSlot slot 0
      , rawClassSpan = declSpan
      , rawClassModule = moduleName
      , rawClassVariables = variables
      , rawClassContext = context
      , rawClassDeclarations = fromMaybe [] maybeClassDecls
      }

getClassMethodsFromRaw
  :: Monad m
  => Map.Map QualifiedName HsTypeClass
  -> [QualifiedName]
  -> TypeDeclMap
  -> [[Either ExtractionError RawTypeClass]]
  -> m [[SourcedExtraction [ClassMethodDeclaration]]]
getClassMethodsFromRaw classes dataTypes typeDeclarations =
  mapM $ fmap concat . mapM elaborate
 where
  -- Invalid raw class names abort the earlier class-declaration phase, so this
  -- branch is unreachable for a successful load. Retaining no invented slot
  -- is preferable to assigning such a failure to another declaration.
  elaborate (Left _) = pure []
  elaborate (Right rawClass) = elaborateRawClass
    classes dataTypes typeDeclarations rawClass

elaborateRawClass
  :: Monad m
  => Map.Map QualifiedName HsTypeClass
  -> [QualifiedName]
  -> TypeDeclMap
  -> RawTypeClass
  -> m [SourcedExtraction [ClassMethodDeclaration]]
elaborateRawClass classes dataTypes typeDeclarations rawClass =
  case Map.lookup (rawClassName rawClass) classes of
    Nothing -> pure
      [ classFailure
          $ "unknown type class: " ++ show (rawClassName rawClass)
      ]
    Just typeClass -> flip evalStateT emptyConvData $ do
      parameterResult <- runExceptT
        $ mapM tyVarTransform $ rawClassVariables rawClass
      case parameterResult of
        Left failure -> pure [classFailure failure]
        Right parameters -> do
          let ownerConstraint = classMethodConstraint typeClass
              expectedOwner = HsConstraint (rawClassName rawClass)
                $ map TypeVar parameters
          if ownerConstraint /= expectedOwner
            then pure
              [ classFailure
                  "class parameter allocation disagrees with its header"
              ]
            else do
              -- Method failures are located at their own class-body
              -- declaration rather than the whole class head.
              results <- mapM
                (\(nestedSlot, bodyDeclaration) ->
                  fmap (SourcedExtraction nestedSlot
                    . first (extractionErrorAt $ ann bodyDeclaration))
                    $ runExceptT
                    $ transformClassDeclaration ownerConstraint
                        bodyDeclaration)
                [ (SourceSlot topLevelSlot $ nestedIndex + 1, declaration)
                | (nestedIndex, declaration) <-
                    zip [0 :: Natural ..] $ rawClassDeclarations rawClass
                ]
              let prefix = "class method for "
                    ++ unqualifiedClassName (rawClassName rawClass) ++ ": "
              pure $ map
                (\result -> result
                  { sourcedExtractionResult = first
                      (mapExtractionMessage (prefix ++))
                      $ sourcedExtractionResult result
                  })
                results
 where
  classError = extractionErrorAt $ rawClassSpan rawClass
  SourceSlot topLevelSlot _ = rawClassSlot rawClass
  classFailure = SourcedExtraction (rawClassSlot rawClass)
    . Left . classError

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
      $ convertModuleName (rawClassModule rawClass) syntaxName
    let methodType = addClassMethodConstraint owner $ forallify converted
    pure $ ClassMethodDeclaration
      (rawClassName rawClass)
      (functionBindingFromType methodName 0 methodType)

getInstances
  :: Monad m
  => Map.Map QualifiedName HsTypeClass
  -> [QualifiedName]
  -> TypeDeclMap
  -> [Module SrcSpanInfo]
  -> m [Either ExtractionError HsInstance]
getInstances classes dataTypes typeDeclarations modules = sequence $ do
  modul <- modules
  (moduleName, declarations) <- maybeToList $ moduleNameAndDecls modul
  declaration@(InstDecl _ _ rule _) <- declarations
  (explicitVariables, context, syntaxName, argumentSyntax) <-
    maybeToList $ splitInstRule rule
  pure $ fmap (first $ extractionErrorAt $ ann declaration)
    $ runExceptT $ runConversionT emptyConvData $ do
    explicitIds <- case explicitVariables of
      Nothing -> pure Nothing
      Just variables -> do
        ids <- mapM tyVarTransform variables
        case SharedCollection.repeatedValuesInFirstRepetitionOrder
            $ SharedCollection.summarizeDuplicates ids of
          _ : _ -> throwE "duplicate explicitly quantified instance variable"
          [] -> pure ()
        pure $ Just $ Set.fromList ids
    -- The head's class is resolved before prerequisites so an unknown head
    -- keeps diagnostic precedence over a malformed prerequisite.
    className <- resolveKnownClass classes (Just moduleName) syntaxName
    prerequisites <- mapM
      (convertClassConstraint classes (Just moduleName)
        dataTypes typeDeclarations)
      (contextConstraints context)
    headConstraint <- checkedClassApplication classes (Just moduleName)
      dataTypes typeDeclarations className argumentSyntax
    case explicitIds of
      Nothing -> pure ()
      Just declaredIds -> do
        variables <- convDataTypeVarIndex <$> lift get
        let undeclared = Set.fromList (Map.elems variables)
              Set.\\ declaredIds
        when (not $ Set.null undeclared) $ throwE
          $ "instance uses variables outside its explicit forall: "
          ++ show (Set.toAscList undeclared)
    pure $ HsInstance prerequisites headConstraint

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
  className <- resolveKnownClass classes defaultModule syntaxName
  checkedClassApplication classes defaultModule dataTypes
    typeDeclarations className argumentSyntax
convertClassConstraint classes defaultModule dataTypes
    typeDeclarations (ParenA _ constraint) =
  convertClassConstraint classes defaultModule dataTypes
    typeDeclarations constraint
convertClassConstraint _ _ _ _ constraint =
  throwE $ "unknown class constraint: " ++ show constraint

-- Closed-world class-name resolution shared by superclass edges, instance
-- prerequisites, and instance heads. The signature frontend's open-world
-- resolver conversion deliberately stays separate: unknown external classes
-- remain representable there.
resolveKnownClass
  :: Monad m
  => Map.Map QualifiedName HsTypeClass
  -> Maybe (ModuleName SrcSpanInfo)
  -> QName SrcSpanInfo
  -> ConversionT String m QualifiedName
resolveKnownClass classes defaultModule syntaxName = do
  className <- either throwE pure
    $ convertQName defaultModule (Map.keys classes) syntaxName
  case Map.lookup className classes of
    Nothing -> throwE $ "unknown type class: " ++ show className
    _ -> pure ()
  pure className

-- Argument conversion, arity validation, and construction for one resolved
-- class application, shared by constraint conversion and instance heads.
checkedClassApplication
  :: Monad m
  => Map.Map QualifiedName HsTypeClass
  -> Maybe (ModuleName SrcSpanInfo)
  -> [QualifiedName]
  -> TypeDeclMap
  -> QualifiedName
  -> [Type SrcSpanInfo]
  -> ConversionT String m HsConstraint
checkedClassApplication classes defaultModule dataTypes typeDeclarations
    className argumentSyntax = do
  arguments <- mapM
    (convertTypeInternal classes defaultModule dataTypes typeDeclarations)
    argumentSyntax
  either throwE pure
    $ validateConstraintArity classes className (length arguments)
  pure $ HsConstraint className arguments

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
  $ SharedName.nameSpelling name
