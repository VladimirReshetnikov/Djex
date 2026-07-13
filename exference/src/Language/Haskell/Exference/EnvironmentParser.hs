{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}

module Language.Haskell.Exference.EnvironmentParser
  ( SourceBinding (..)
  , sourceBindingFunction
  , SourceEnvironment (..)
  , sourceFunctions
  , CheckedSourceEnvironment
  , checkedSourceProjection
  , checkedSourceInventory
  , checkSourceEnvironment
  , LoadReport (..)
  , EnvironmentLoadError (..)
  , parseModules
  , parseModulesSimple
  , environmentFromModuleAndRatings
  , environmentFromPath
  , toSynthesisSourceEnvironment
  , toSynthesisSourceInventory
  , sourceTypeSynonymMap
  , haskellSrcExtsParseMode
  , compileWithDict
  , parseRatings
  )
where



import Language.Haskell.Exference
import Language.Haskell.Exference.BindingsFromHaskellSrc
import Language.Haskell.Exference.ClassEnvFromHaskellSrc
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.Core.Declaration
import Language.Haskell.Exference.FunctionDecl

import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Diagnostic

import Control.DeepSeq

import Control.Monad ( forM_, forM, zipWithM )
import Data.List ( sort, find, isSuffixOf )
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Either ( lefts, rights )
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.Strict (WriterT, runWriterT, tell)
import System.Directory ( listDirectory )
import Control.Exception ( evaluate )
import Data.Bifunctor ( first )
import System.FilePath ( (</>) )
import System.IO.Error ( tryIOError )

import Language.Haskell.Exts.Syntax ( Module(..) )
import Language.Haskell.Exts.Parser ( parseModuleWithMode
                                    , ParseResult (..)
                                    , ParseMode (..)
                                    )
import Language.Haskell.Exts.Extension ( Language (..)
                                       , Extension (..)
                                       , KnownExtension (..) )
import Language.Haskell.Exts.SrcLoc ( SrcSpanInfo )

import Control.Monad.Trans.Except (runExceptT, throwE)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Void (absurd)
import Text.Read ( readMaybe )
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Declaration as SharedDeclaration
import qualified Language.Haskell.Synthesis.Environment as SharedEnvironment
import qualified Language.Haskell.Synthesis.KindInference as SharedKindInference
import qualified Language.Haskell.Synthesis.Inventory as SharedInventory

-- | One ordered source binding. Class methods retain their exactly qualified
-- owning class while exposing the same flat function projection as the
-- historical frontend API. Their compatibility types carry the derived
-- implicit constraint; duplicating its parameter IDs in the tag would let the
-- two representations drift.
data SourceBinding function
  = SourceFunction function
  | SourceClassMethod QualifiedName function
  deriving (Functor, Show)

sourceBindingFunction :: SourceBinding function -> function
sourceBindingFunction binding = case binding of
  SourceFunction function -> function
  SourceClassMethod _ function -> function

-- | The complete checked source inventory produced by the HSE frontend.
-- Parameterizing only the function representation lets parsing, rating, and
-- core lowering share one shape without repeatedly packing positional tuples
-- or dropping the declarations needed by later kind validation.
data SourceEnvironment function = SourceEnvironment
  { sourceBindings :: [SourceBinding function]
  , sourceDeconstructors :: [DeconstructorBinding]
  , sourceClasses :: StaticClassEnv
  , sourceTypeNames :: [QualifiedName]
  , sourceTypeSynonyms :: [HsTypeDecl]
  }
  deriving (Show)

-- | Historical flat function view. Ownership remains available through
-- 'sourceBindings', while search and compatibility clients see every method
-- exactly once in its original list position.
sourceFunctions :: SourceEnvironment function -> [function]
sourceFunctions = map sourceBindingFunction . sourceBindings

-- | A backend projection paired with the exact shared inventory that validated
-- it.  The constructor is private so CLI and library loaders cannot expose a
-- searchable source environment before structural and kind sealing succeeds.
data CheckedSourceEnvironment = CheckedSourceEnvironment
  { checkedSourceProjection :: SourceEnvironment FunctionBinding
  , checkedSourceInventory :: SynthesisInventory
  }

-- | The result of an environment-loading operation together with its
-- non-fatal diagnostics.  Keeping the report in 'IO' hides the loader's
-- historical heterogeneous writer implementation from callers while still
-- preserving every advisory and progress message in production order.
data LoadReport a = LoadReport
  { loadResult :: Either EnvironmentLoadError a
  , loadDiagnostics :: [Diagnostic]
  }

-- | Fatal source-loading phases.  Warnings remain in the load report, but
-- a failed class graph cannot produce a searchable recovery environment.
data EnvironmentLoadError
  = EnvironmentDirectoryReadError Diagnostic
  | ModuleReadErrors (NonEmpty Diagnostic)
  | ModuleParseErrors (NonEmpty String)
  | DataTypeNameError String
  | TypeDeclarationErrors (NonEmpty String)
  | ClassEnvironmentLoadFailure ClassEnvironmentLoadError
  | BindingDeclarationErrors (NonEmpty String)
  | BuiltInEnvironmentErrors (NonEmpty String)
  | InvalidSourceInventory SynthesisDeclarationError
  deriving (Eq, Show)

warningDiagnostic :: String -> Diagnostic
warningDiagnostic message =
  (diagnostic message) { diagnosticSeverity = Warning }

infoDiagnostic :: String -> Diagnostic
infoDiagnostic message =
  (diagnostic message) { diagnosticSeverity = Info }

-- | Catch only filesystem failures, preserving asynchronous cancellation and
-- programming exceptions.  The source path is structural diagnostic data, so
-- callers never need to recover it from platform-specific exception text.
captureIO :: FilePath -> IO value -> IO (Either Diagnostic value)
captureIO path action = first
  (withSource path . diagnostic . show) <$> tryIOError action

-- | Force lazy text while the IOException boundary is still active.  Merely
-- wrapping 'readFile' would let decoding and deferred reads escape later.
readTextFile :: FilePath -> IO (Either Diagnostic String)
readTextFile path = captureIO path
  $ readFile path >>= evaluate . force

checkSourceEnvironment
  :: SourceEnvironment FunctionBinding
  -> Either EnvironmentLoadError CheckedSourceEnvironment
checkSourceEnvironment environment = do
  inventory <- first InvalidSourceInventory
    $ toSynthesisSourceInventory environment
  projection <- first InvalidSourceInventory
    $ normalizeBackendProjection inventory environment
  pure $ CheckedSourceEnvironment projection inventory

-- | Rebuild declaration-owned backend bindings from the checked shared
-- declarations. Ordinary functions retain source order; constructor and class
-- method entries are replaced in place so equal-cost search ordering does not
-- change. The Inventory is authoritative for ownership, shape, and penalty
-- without imposing declaration-category order on the search environment.
normalizeBackendProjection
  :: SynthesisInventory
  -> SourceEnvironment FunctionBinding
  -> Either SynthesisDeclarationError
      (SourceEnvironment FunctionBinding)
normalizeBackendProjection inventory environment = do
  converted <- mapM fromSynthesisRatedDataDeclaration
    [ declaration
    | declaration@SharedDeclaration.DataTypeDeclaration{} <-
        SharedEnvironment.environmentDeclarations
        $ SharedInventory.inventoryEnvironment inventory
    ]
  convertedClasses <- mapM fromSynthesisClassDeclarationWithMethods
    [ declaration
    | declaration@SharedDeclaration.ClassDeclaration{} <-
        SharedEnvironment.environmentDeclarations
          $ SharedInventory.inventoryEnvironment inventory
    ]
  let constructorFunctions = concatMap fst converted
      functionsByName = M.fromList
        [ (functionName binding, binding)
        | binding <- constructorFunctions
        ]
      replaceConstructor binding = M.findWithDefault binding
        (functionName binding) functionsByName
      inventoryMethodGroups = M.fromListWith (++)
        [ (functionName binding,
            [(tclass_name typeClass, binding)])
        | (typeClass, methods) <- convertedClasses
        , binding <- methods
        ]
      sourceMethodGroups = M.fromListWith (++)
        [ (functionName binding, [(owner, binding)])
        | SourceClassMethod owner binding <- sourceBindings environment
        ]
      duplicateMethodNames = S.toAscList $ S.fromList
        [ methodName
        | (methodName, occurrences) <-
            M.toAscList inventoryMethodGroups ++ M.toAscList sourceMethodGroups
        , length occurrences /= 1
        ]
      missingMethodNames = S.toAscList
        $ M.keysSet inventoryMethodGroups S.\\ M.keysSet sourceMethodGroups
      orphanMethodNames = S.toAscList
        $ M.keysSet sourceMethodGroups S.\\ M.keysSet inventoryMethodGroups
      mismatchedMethodOwners =
        [ (methodName, expectedOwner, actualOwner)
        | (methodName, [(expectedOwner, _)]) <-
            M.toAscList inventoryMethodGroups
        , [(actualOwner, _)] <- [M.findWithDefault [] methodName
            sourceMethodGroups]
        , expectedOwner /= actualOwner
        ]
      methodsByName = M.mapMaybe onlyOccurrence inventoryMethodGroups
      onlyOccurrence occurrences = case occurrences of
        [occurrence] -> Just occurrence
        _ -> Nothing
      normalizeBinding sourceBinding = case sourceBinding of
        SourceFunction binding ->
          Right $ SourceFunction $ replaceConstructor binding
        SourceClassMethod _ binding -> case
            M.lookup (functionName binding) methodsByName of
          Just (normalizedOwner, normalized) ->
            Right $ SourceClassMethod normalizedOwner normalized
          Nothing -> Left $ MissingClassMethodBindings [functionName binding]
  if null duplicateMethodNames
    then pure ()
    else Left $ DuplicateClassMethodBindings duplicateMethodNames
  if null missingMethodNames
    then pure ()
    else Left $ MissingClassMethodBindings missingMethodNames
  if null orphanMethodNames
    then pure ()
    else Left $ OrphanClassMethodBindings orphanMethodNames
  if null mismatchedMethodOwners
    then pure ()
    else Left $ MismatchedClassMethodOwners mismatchedMethodOwners
  normalizedBindings <- mapM normalizeBinding $ sourceBindings environment
  pure environment
    { sourceBindings = normalizedBindings
    , sourceDeconstructors = map snd converted
    }

-- | Unique-only compatibility index used by the historical type elaborator.
-- The ordered field remains authoritative so duplicate declarations reach the
-- shared inventory instead of being silently resolved by map insertion order.
sourceTypeSynonymMap :: SourceEnvironment function -> TypeDeclMap
sourceTypeSynonymMap = uniqueTypeDeclMap . sourceTypeSynonyms

-- | Seal the complete frontend inventory in the common environment IR.
-- Unlike the search-core 'EnvDictionary' adapter, this boundary retains type
-- synonyms so later validation does not have to rediscover declarations from
-- the HSE modules or a parallel tuple field.
toSynthesisSourceEnvironment
  :: SourceEnvironment FunctionBinding
  -> Either SynthesisDeclarationError SynthesisEnvironment
toSynthesisSourceEnvironment environment =
  SharedInventory.inventoryEnvironment
    <$> toSynthesisSourceInventory environment

-- | Seal and kind-check the frontend inventory while retaining the inferred
-- assumptions needed to elaborate subsequent queries against the same source
-- declarations.
toSynthesisSourceInventory
  :: SourceEnvironment FunctionBinding
  -> Either SynthesisDeclarationError SynthesisInventory
toSynthesisSourceInventory environment = do
  let functions = sourceFunctions environment
      constructorDefinitions = M.fromListWith (++)
        [ ( constructorName constructor
          , [(deconstructorInput deconstructor,
              constructorFields constructor)]
          )
        | deconstructor <- sourceDeconstructors environment
        , constructor <- deconstructorConstructors deconstructor
        ]
      constructorNames = M.keysSet constructorDefinitions
      isConstructorBinding binding =
        SharedName.nameLexicalClass
          (toSynthesisName $ functionName binding)
          == SharedName.ConstructorLike
      constructorFunctionGroups = M.fromListWith (++)
        [ (functionName binding, [binding])
        | binding <- functions
        , functionName binding `S.member` constructorNames
        ]
      missingFunctions = S.toAscList
        $ constructorNames S.\\ M.keysSet constructorFunctionGroups
      duplicateFunctions =
        [ name
        | (name, bindings) <- M.toAscList constructorFunctionGroups
        , length bindings > 1
        ]
      orphanConstructors = S.toAscList $ S.fromList
        [ functionName binding
        | binding <- functions
        , isConstructorBinding binding
        , functionName binding `S.notMember` constructorNames
        ]
      mismatchedFunctions =
        [ name
        | (name, [(result, parameters)]) <-
            M.toAscList constructorDefinitions
        , [binding] <- [M.findWithDefault [] name constructorFunctionGroups]
        , functionResult binding /= result
            || functionParameters binding /= parameters
            || not (null $ functionConstraints binding)
        ]
      constructorPenalties = M.fromList
        [ (name, functionPenalty binding)
        | (name, [binding]) <- M.toAscList constructorFunctionGroups
        ]
      valueBindings =
        [ binding
        | SourceFunction binding <- sourceBindings environment
        , functionName binding `S.notMember` constructorNames
        ]
      classMethods = M.fromListWith (flip (++))
        [ (owner, [binding])
        | SourceClassMethod owner binding <- sourceBindings environment
        ]
  if null missingFunctions
    then pure ()
    else Left $ MissingConstructorFunctionBindings
      $ map toSynthesisName missingFunctions
  if null duplicateFunctions
    then pure ()
    else Left $ DuplicateConstructorFunctionBindings
      $ map toSynthesisName duplicateFunctions
  if null orphanConstructors
    then pure ()
    else Left $ OrphanConstructorBindings
      $ map toSynthesisName orphanConstructors
  if null mismatchedFunctions
    then pure ()
    else Left $ MismatchedConstructorFunctionBindings
      $ map toSynthesisName mismatchedFunctions
  synonyms <- mapM toSynthesisTypeDeclaration
    $ sourceTypeSynonyms environment
  core <- toSynthesisEnvironmentWithConstructorPenaltiesAndClassMethods
    constructorPenalties classMethods $ EnvDictionary
    valueBindings
    (sourceDeconstructors environment)
    (sourceClasses environment)
  let declarations =
        synonyms ++ SharedEnvironment.environmentDeclarations core
  case SharedInventory.mkInventory
      SharedKindInference.OpenKindInventory declarations of
    Left (SharedInventory.InvalidInventoryEnvironment failure) ->
      Left $ InvalidSharedEnvironment failure
    Left (SharedInventory.UngroundedInventoryKind impossible) ->
      absurd impossible
    Left (SharedInventory.InvalidInventoryKinds failure) ->
      Left $ InvalidSourceEnvironmentKinds failure
    Right inventory -> Right inventory


builtInDecls :: Either QualifiedNameError [HsFunctionDecl]
builtInDecls = do
  consName <- fromSynthesisName SharedName.consName
  listName <- fromSynthesisName SharedName.listName
  unitConstructor <- do
    unitName <- mkBoxedTupleName 0
    pure (unitName, TypeCons unitName)
  tupleConstructors <- mapM tupleConstructor [2 .. 7]
  pure $ listConstructors consName listName
    ++ (unitConstructor : tupleConstructors)
 where
  listConstructors consName listName =
    [ (listName, listType listName)
    , (consName, TypeArrow (TypeVar 0)
        $ TypeArrow (listType listName) (listType listName))
    ]
  listType listName = TypeApp (TypeCons listName) (TypeVar 0)
  tupleConstructor arity = do
    tupleName <- mkBoxedTupleName arity
    pure (tupleName,
      foldr TypeArrow (tupleType tupleName arity) $ typeVariables arity)

builtInDeconstructors :: Either QualifiedNameError [DeconstructorBinding]
builtInDeconstructors = do
  listName <- fromSynthesisName SharedName.listName
  consName <- fromSynthesisName SharedName.consName
  unitName <- mkBoxedTupleName 0
  tuples <- mapM tupleDeconstructor [2 .. 7]
  let listType = TypeApp (TypeCons listName) (TypeVar 0)
  -- These declarations are not merely pattern-match conveniences: they make
  -- intrinsic constructor bindings members of the shared constructor
  -- namespace instead of invalid ordinary values.
  pure $
    DeconstructorBinding listType
      [ ConstructorBinding listName []
      , ConstructorBinding consName [TypeVar 0, listType]
      ] True
    : DeconstructorBinding (TypeCons unitName)
        [ConstructorBinding unitName []] False
    : tuples
 where
  tupleDeconstructor arity = do
    tupleName <- mkBoxedTupleName arity
    pure $ DeconstructorBinding
      (tupleType tupleName arity)
      [ConstructorBinding tupleName (typeVariables arity)]
      False

typeVariables :: Int -> [HsType]
typeVariables arity = map TypeVar [0 .. arity - 1]

tupleType :: QualifiedName -> Int -> HsType
tupleType tupleName arity = foldl TypeApp (TypeCons tupleName)
  $ typeVariables arity

-- | Takes a list of bindings, and a dictionary of desired
-- functions and their rating, and compiles a list of
-- RatedFunctionBindings.
--
-- If a function in the dictionary is not in the list of bindings,
-- Left is returned with the corresponding name.
--
-- Otherwise, the result is Right.
compileWithDict :: [(QualifiedName, Penalty)]
                -> [HsFunctionDecl]
                -> Either String [RatedHsFunctionDecl]
                -- function_not_found or all bindings
compileWithDict ratings binds =
  ratings `forM` \(name, rating) ->
    case find ((name==).fst) binds of
      Nothing    -> Left $ show name
      Just (_,t) -> Right (name, rating, t)

-- | Load source modules in the supplied order.  The returned diagnostics are
-- deterministic: messages within a module retain source order, while sets of
-- unknown names are rendered in nominal sort order.
parseModules
  :: [(ParseMode, FilePath)]
  -> IO (LoadReport (SourceEnvironment HsFunctionDecl))
parseModules inputs = do
  (result, diagnostics) <- runWriterT $ parseModulesM inputs
  pure $ LoadReport result diagnostics

type Loader = WriterT [Diagnostic] IO

parseModulesM
  :: [(ParseMode, FilePath)]
  -> Loader (Either EnvironmentLoadError
              (SourceEnvironment HsFunctionDecl))
parseModulesM inputs = do
  readResults <- lift $ mapM hRead inputs
  case NonEmpty.nonEmpty $ lefts readResults of
    Just errors -> pure $ Left $ ModuleReadErrors errors
    Nothing -> parseLoadedModules $ rights readResults
  where
    hRead
      :: (ParseMode, FilePath)
      -> IO (Either Diagnostic (ParseMode, String))
    hRead (mode, path) = fmap ((,) mode) <$> readTextFile path

    hParse :: (ParseMode, String) -> Either String (Module SrcSpanInfo)
    hParse (mode, content) = case parseModuleWithMode mode content of
      f@(ParseFailed _ _) -> Left $ show f
      ParseOk modul       -> Right modul

    -- Each phase observes only a successful predecessor.  Errors are still
    -- aggregated within one phase, but an invalid partial inventory is never
    -- fed into the next extractor as an invented recovery environment.
    parseLoadedModules
      :: [(ParseMode, String)]
      -> Loader (Either EnvironmentLoadError
                  (SourceEnvironment HsFunctionDecl))
    parseLoadedModules rawTuples = runExceptT $ do
      let parsedModules = map hParse rawTuples
          parseErrors = lefts parsedModules
      case NonEmpty.nonEmpty parseErrors of
        Just errors -> throwE $ ModuleParseErrors errors
        Nothing -> pure ()
      let modules = rights parsedModules

      dataTypes <- case getDataTypesChecked modules of
        Left conversionError ->
          let message =
                "could not extract data-type names: " ++ conversionError
          in throwE $ DataTypeNameError message
        Right result -> pure result

      typeDeclarationResults <- lift $ getTypeDecls dataTypes modules
      let typeDeclarationErrors = lefts typeDeclarationResults
      case NonEmpty.nonEmpty typeDeclarationErrors of
        Just errors -> throwE $ TypeDeclarationErrors errors
        Nothing -> pure ()
      let typeDeclarations = rights typeDeclarationResults
          typeDeclarationMap = uniqueTypeDeclMap typeDeclarations

      classResult <- lift
        $ getClassEnvWithMethods dataTypes typeDeclarationMap modules
      (classEnvironment, instanceCount, methodsByModule) <- either
        (throwE . ClassEnvironmentLoadFailure)
        pure
        classResult

      extracted <- lift $ zipWithM
        (hExtractBinds classEnvironment dataTypes typeDeclarationMap)
        modules methodsByModule
      let (bindingLists, deconstructorLists, errorLists) = unzip3 extracted
          declarations = concat bindingLists
          deconstructors = concat deconstructorLists
          bindingErrors = concat errorLists
      case NonEmpty.nonEmpty bindingErrors of
        Just errors -> throwE $ BindingDeclarationErrors errors
        Nothing -> pure ()

      let builtInDeclarationsResult = builtInDecls
          builtInDeconstructorsResult = builtInDeconstructors
      (builtInDeclarations, builtInDeconstructorValues) <-
        case (builtInDeclarationsResult, builtInDeconstructorsResult) of
          (Right declarationsResult, Right deconstructorsResult) ->
            pure (declarationsResult, deconstructorsResult)
          (Left declarationFailure, Right _) ->
            failBuiltIns
              $ ("could not construct built-in bindings: "
                  ++ show declarationFailure) NonEmpty.:| []
          (Right _, Left deconstructorFailure) ->
            failBuiltIns
              $ ("could not construct built-in deconstructors: "
                  ++ show deconstructorFailure) NonEmpty.:| []
          (Left declarationFailure, Left deconstructorFailure) ->
            failBuiltIns
              $ ("could not construct built-in bindings: "
                  ++ show declarationFailure) NonEmpty.:|
                [ "could not construct built-in deconstructors: "
                    ++ show deconstructorFailure
                ]

      let classes = sClassEnv_tclasses classEnvironment
          instances = sClassEnv_instances classEnvironment
          allValidNames = dataTypes ++ M.keys classes

          warnUnknownTypeConstructors :: String -> [HsType] -> Loader ()
          warnUnknownTypeConstructors context types = forM_
            (S.toAscList $ S.fromList
              $ concatMap (findInvalidNames allValidNames) types)
            $ \unknownName -> tell
                [ warningDiagnostic
                    $ "unknown type constructor '" ++ show unknownName
                    ++ "' used in " ++ context
                ]

          warnBindingConstraints :: QualifiedName -> HsType -> Loader ()
          warnBindingConstraints bindingName bindingType = forM_
            (S.toAscList $ S.fromList
              [ renderConstraintFailure bindingName constraint failure
              | constraint <- typeConstraints bindingType
              , Left failure <-
                  [ validateConstraintInEnv classEnvironment
                      (BindingConstraint bindingName) constraint
                  ]
              ])
            (tell . (: []) . warningDiagnostic)

          renderConstraintFailure
            :: QualifiedName
            -> HsConstraint
            -> ClassEnvError
            -> String
          renderConstraintFailure bindingName _
              (UnknownConstraintClass _ className) =
            "unknown constraint class '" ++ show className
              ++ "' used in the binding " ++ show bindingName
          renderConstraintFailure bindingName constraint failure =
            "invalid class constraint '" ++ show constraint
              ++ "' used in the binding " ++ show bindingName
              ++ ": " ++ show failure

          instanceTypes =
            [ parameter
            | indexedInstances <- M.elems instances
            , instanceDeclaration <- indexedInstances
            , constraint <- instance_head instanceDeclaration
                : instance_constraints instanceDeclaration
            , parameter <- constraint_params constraint
            ]

      lift $ do
        -- Instance inflation can place the same source type under several
        -- implied class heads.  Diagnose its combined constructor set once.
        warnUnknownTypeConstructors "class instances" instanceTypes
        forM_ (M.elems classes) $ \typeClass ->
          warnUnknownTypeConstructors
            ("the superclass data for " ++ show (tclass_name typeClass))
            [ parameter
            | constraint <- tclass_constraints typeClass
            , parameter <- constraint_params constraint
            ]
        forM_ (map sourceBindingFunction declarations)
            $ \(bindingName, bindingType) -> do
          warnUnknownTypeConstructors
            ("the binding " ++ show bindingName) [bindingType]
          -- The loader has a complete class inventory, unlike public ad-hoc
          -- search input, so binding constraints are checked nominally here.
          warnBindingConstraints bindingName bindingType
        tell [infoDiagnostic $ "got " ++ show (length classes) ++ " classes"]
        tell [infoDiagnostic $ "and " ++ show instanceCount ++ " instances"]
        tell
          [ infoDiagnostic
              $ "(-> " ++ show (length $ concat $ M.elems instances)
                ++ " instances after inflation)"
          ]
        tell
          [ infoDiagnostic
              $ "and " ++ show (length declarations) ++ " function decls"
          ]

      pure SourceEnvironment
        { sourceBindings = map SourceFunction builtInDeclarations ++ declarations
        , sourceDeconstructors = builtInDeconstructorValues ++ deconstructors
        , sourceClasses = classEnvironment
        , sourceTypeNames = allValidNames
        , sourceTypeSynonyms = typeDeclarations
        }

    failBuiltIns = throwE . BuiltInEnvironmentErrors

    hExtractBinds :: StaticClassEnv
                  -> [QualifiedName]
                  -> TypeDeclMap
                  -> Module SrcSpanInfo
                  -> [Either String ClassMethodDeclaration]
                  -> Loader
                       ( [SourceBinding HsFunctionDecl]
                       , [DeconstructorBinding]
                       , [String]
                       )
    hExtractBinds cntxt ds tDeclMap modul methodResults = do
      eFromData <- getDataConss (sClassEnv_tclasses cntxt) ds tDeclMap [modul]
      eDecls <- getDecls ds (sClassEnv_tclasses cntxt) tDeclMap [modul]
      let errors = lefts eFromData ++ lefts eDecls ++ lefts methodResults
      let (binds1s, deconss) = unzip $ rights eFromData
          binds2 = rights eDecls
          methods =
            [ SourceClassMethod owner binding
            | ClassMethodDeclaration owner binding <- rights methodResults
            ]
      return
        ( map SourceFunction (concat binds1s ++ binds2) ++ methods
        , deconss
        , errors
        )

-- | Load one module with the default parse mode and assign every declaration
-- a neutral rating.
parseModulesSimple
  :: FilePath
  -> IO (LoadReport (SourceEnvironment RatedHsFunctionDecl))
parseModulesSimple path = do
  (result, diagnostics) <- runWriterT $ parseModulesSimpleM path
  pure $ LoadReport result diagnostics

parseModulesSimpleM
  :: FilePath
  -> Loader (Either EnvironmentLoadError
              (SourceEnvironment RatedHsFunctionDecl))
parseModulesSimpleM path = fmap helper
                   <$> parseModulesM
                     [(haskellSrcExtsParseMode path, path)]
 where
  addRating (a,b) = (a,0.0,b)
  helper environment = environment
    { sourceBindings = fmap addRating <$> sourceBindings environment }

parseRatings :: String -> Either Diagnostic [(QualifiedName, Penalty)]
parseRatings = go . words
  where
    go [] = Right []
    go [_] = Left $ diagnostic
      "rating file ends with a name but no numeric rating"
    go (name : value : rest) = case readMaybe value :: Maybe Double of
      Nothing -> Left $ diagnostic
        $ "invalid rating for " ++ name ++ ": " ++ value
      Just rating | isNaN rating || isInfinite rating ->
        Left $ diagnostic
          $ "rating for " ++ name ++ " must be finite: " ++ value
      Just rating -> do
        qualifiedName <- parseQualifiedName name
        ((qualifiedName, Penalty rating) :) <$> go rest

ratingsFromFile :: String -> IO (Either Diagnostic [(QualifiedName, Penalty)])
ratingsFromFile path = do
  contents <- readTextFile path
  pure $ contents >>= first (withSource path) . parseRatings

ratingFailureDiagnostic :: Diagnostic -> Diagnostic
ratingFailureDiagnostic failure = failure
  { diagnosticSeverity = Warning
  , diagnosticMessage =
      "could not parse rating file: " ++ diagnosticMessage failure
  }

-- | Apply heuristic metadata once by nominal name.  Duplicate ratings are
-- deliberately ignored rather than acquiring an accidental file-order
-- meaning, and duplicate declarations remain fatal at shared inventory
-- sealing after receiving their neutral defaults.
applyRatings
  :: [(QualifiedName, Penalty)]
  -> SourceEnvironment HsFunctionDecl
  -> (SourceEnvironment FunctionBinding, [Diagnostic])
applyRatings ratings environment =
  ( environment
      { sourceBindings = fmap rateDeclaration
          <$> sourceBindings environment
      }
  , lefts ratingResults
  )
 where
  declarationCounts = M.fromListWith (+)
    [ (name, 1 :: Int)
    | (name, _) <- sourceFunctions environment
    ]
  ratingsByName = M.fromListWith (++)
    [ (name, [rating])
    | (name, rating) <- ratings
    ]
  ratingResults = map validateRating $ M.toAscList ratingsByName
  effectiveRatings = M.fromList $ rights ratingResults

  validateRating (name, values)
    | M.findWithDefault 0 name declarationCounts == 0 =
        Left $ warningDiagnostic
          $ "rating could not be applied: " ++ show name
    | M.findWithDefault 0 name declarationCounts > 1 =
        Left $ warningDiagnostic $ "duplicate function: " ++ show name
    | [rating] <- values = Right (name, rating)
    | otherwise = Left $ warningDiagnostic
        $ "duplicate rating: " ++ show name

  rateDeclaration (name, bindingType) = declToBinding
    (name, M.findWithDefault 0 name effectiveRatings, bindingType)


environmentFromModuleAndRatings
  :: FilePath
  -> FilePath
  -> IO (LoadReport CheckedSourceEnvironment)
environmentFromModuleAndRatings modulePath ratingPath = do
  (result, diagnostics) <- runWriterT
    $ environmentFromModuleAndRatingsM modulePath ratingPath
  pure $ LoadReport result diagnostics

environmentFromModuleAndRatingsM
  :: FilePath
  -> FilePath
  -> Loader (Either EnvironmentLoadError CheckedSourceEnvironment)
environmentFromModuleAndRatingsM modulePath ratingPath = do
  let exts1 = [ TypeOperators
              , ExplicitForAll
              , ExistentialQuantification
              , TypeFamilies
              , FunctionalDependencies
              , FlexibleContexts
              , MultiParamTypeClasses ]
      exts2 = map EnableExtension exts1
      mode = ParseMode modulePath
                       Haskell2010
                       exts2
                       False
                       False
                       Nothing
                       False
  environmentResult <- parseModulesM [(mode, modulePath)]
  case environmentResult of
    Left failure -> pure $ Left failure
    Right environment -> do
      ratingsResult <- lift $ ratingsFromFile ratingPath
      ratings <- case ratingsResult of
        Left failure -> do
          tell [ratingFailureDiagnostic failure]
          pure []
        Right parsedRatings -> pure parsedRatings
      let (ratedEnvironment, warnings) = applyRatings ratings environment
      forM_ warnings $ tell . (: [])
      pure $ checkSourceEnvironment ratedEnvironment


environmentFromPath
  :: FilePath
  -> IO (LoadReport CheckedSourceEnvironment)
environmentFromPath path = do
  (result, diagnostics) <- runWriterT $ environmentFromPathM path
  pure $ LoadReport result diagnostics

environmentFromPathM
  :: FilePath
  -> Loader (Either EnvironmentLoadError CheckedSourceEnvironment)
environmentFromPathM p = do
  directoryResult <- lift $ captureIO p
    $ listDirectory p >>= evaluate . force
  case directoryResult of
    Left failure -> pure $ Left $ EnvironmentDirectoryReadError failure
    Right files -> do
      -- Enumeration order is platform-dependent; sorting stabilizes module
      -- diagnostics and rating conflict reports across filesystems.
      let modules = map (p </>)
            $ sort $ filter (".hs" `isSuffixOf`) files
          ratingPaths = map (p </>)
            $ sort $ filter (".ratings" `isSuffixOf`) files
      environmentResult <- parseModulesM
        [ (haskellSrcExtsParseMode modulePath, modulePath)
        | modulePath <- modules
        ]
      case environmentResult of
        Left failure -> pure $ Left failure
        Right environment -> do
          ratingResults <- lift $ mapM ratingsFromFile ratingPaths
          forM_ (lefts ratingResults)
            $ tell . (: []) . ratingFailureDiagnostic
          let ratings = concat $ rights ratingResults
              (ratedEnvironment, warnings) =
                applyRatings ratings environment
          forM_ warnings $ tell . (: [])
          pure $ checkSourceEnvironment ratedEnvironment
