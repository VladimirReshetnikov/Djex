{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeOperators #-}

module Language.Haskell.Exference.EnvironmentParser
  ( SourceEnvironment (..)
  , CheckedSourceEnvironment
  , checkedSourceProjection
  , checkedSourceInventory
  , checkSourceEnvironment
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

import Control.Monad ( forM_, guard, forM, join )
import Data.List ( sort, find, isSuffixOf )
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe ( fromMaybe )
import Data.Either ( lefts, rights )
import Control.Monad.Writer.Strict
import System.Directory ( getDirectoryContents )
import Control.Exception ( evaluate, try, SomeException )
import Data.Bifunctor ( first )

import Language.Haskell.Exts.Syntax ( Module(..) )
import Language.Haskell.Exts.Parser ( parseModuleWithMode
                                    , ParseResult (..)
                                    , ParseMode (..)
                                    )
import Language.Haskell.Exts.Extension ( Language (..)
                                       , Extension (..)
                                       , KnownExtension (..) )
import Language.Haskell.Exts.SrcLoc ( SrcSpanInfo )

import Control.Monad.Trans.MultiRWS
import Data.HList.ContainsType

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Void (absurd)
import Text.Read ( readMaybe )
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Environment as SharedEnvironment
import qualified Language.Haskell.Synthesis.KindInference as SharedKindInference
import qualified Language.Haskell.Synthesis.Inventory as SharedInventory

-- | The complete checked source inventory produced by the HSE frontend.
-- Parameterizing only the function representation lets parsing, rating, and
-- core lowering share one shape without repeatedly packing positional tuples
-- or dropping the declarations needed by later kind validation.
data SourceEnvironment function = SourceEnvironment
  { sourceFunctions :: [function]
  , sourceDeconstructors :: [DeconstructorBinding]
  , sourceClasses :: StaticClassEnv
  , sourceTypeNames :: [QualifiedName]
  , sourceTypeSynonyms :: [HsTypeDecl]
  }
  deriving (Show)

-- | A backend projection paired with the exact shared inventory that validated
-- it.  The constructor is private so CLI and library loaders cannot expose a
-- searchable source environment before structural and kind sealing succeeds.
data CheckedSourceEnvironment = CheckedSourceEnvironment
  { checkedSourceProjection :: SourceEnvironment FunctionBinding
  , checkedSourceInventory :: SynthesisInventory
  }

-- | Fatal source-loading phases.  Warnings remain in the writer channel, but
-- a failed class graph cannot produce a searchable recovery environment.
data EnvironmentLoadError
  = ModuleParseErrors (NonEmpty String)
  | DataTypeNameError String
  | TypeDeclarationErrors (NonEmpty String)
  | ClassEnvironmentLoadFailure ClassEnvironmentLoadError
  | BindingDeclarationErrors (NonEmpty String)
  | BuiltInEnvironmentErrors (NonEmpty String)
  | InvalidSourceInventory SynthesisDeclarationError
  deriving (Eq, Show)

checkSourceEnvironment
  :: SourceEnvironment FunctionBinding
  -> Either EnvironmentLoadError CheckedSourceEnvironment
checkSourceEnvironment environment = CheckedSourceEnvironment environment
  <$> first InvalidSourceInventory (toSynthesisSourceInventory environment)

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
  let constructorNames = S.fromList
        [ constructorName constructor
        | deconstructor <- sourceDeconstructors environment
        , constructor <- deconstructorConstructors deconstructor
        ]
      isConstructorBinding binding =
        SharedName.nameLexicalClass
          (toSynthesisName $ functionName binding)
          == SharedName.ConstructorLike
      orphanConstructors =
        [ binding
        | binding <- sourceFunctions environment
        , isConstructorBinding binding
        , functionName binding `S.notMember` constructorNames
        ]
      valueBindings =
        [ binding
        | binding <- sourceFunctions environment
        , functionName binding `S.notMember` constructorNames
        ]
  case orphanConstructors of
    binding : _ -> Left $ OrphanConstructorBinding
      $ toSynthesisName $ functionName binding
    [] -> pure ()
  synonyms <- mapM toSynthesisTypeDeclaration
    $ sourceTypeSynonyms environment
  core <- toSynthesisEnvironment $ EnvDictionary
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


builtInDeclsM
  :: Monad m
  => MultiRWST r w s m (Either QualifiedNameError [HsFunctionDecl])
builtInDeclsM = pure $ do
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

builtInDeconstructorsM
  :: Monad m
  => MultiRWST r w s m (Either QualifiedNameError [DeconstructorBinding])
builtInDeconstructorsM = pure $ do
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

-- | input: a list of filenames for haskell modules and the
-- parsemode to use for it.
--
-- output: the environment extracted from these modules, wrapped
-- in a Writer that contains warnings/errors.
parseModules :: forall m r w s
              . ( m ~ MultiRWST r w s IO
                , ContainsType [String] w
                )
             => [(ParseMode, String)]
             -> m (Either EnvironmentLoadError
                    (SourceEnvironment HsFunctionDecl))
parseModules l = do
  rawTuples <- lift $ mapM hRead l
  let eParsed = map hParse rawTuples
  {-
  let h :: Decl -> IO ()
      h i@(InstDecl _ _ _ _ _ _ _) = do
        pprint i >>= print
      h _ = return ()
  forM_ (rights eParsed) $ \(Module _ _ _ _ _ _ ds) ->
    forM_ ds h
  -}
  -- forM_ (rights eParsed) $ \m -> pprintTo 10000 m >>= print
  let parseErrors = lefts eParsed
  mapM_ (mTell . (:[])) parseErrors
  let mods = rights eParsed
  (ds, dataTypeErrors) <- case getDataTypesChecked mods of
    Left conversionError -> do
      let message = "could not extract data-type names: " ++ conversionError
      mTell [message]
      pure ([], [message])
    Right dataTypes -> pure (dataTypes, [])
  typeDeclsE <- getTypeDecls ds mods
  let typeDeclarationErrors = lefts typeDeclsE
  typeDeclarationErrors `forM_` (mTell . (:[]))
  let validTypeDecls = rights typeDeclsE
      typeDecls = uniqueTypeDeclMap validTypeDecls
  classResult <- getClassEnv ds typeDecls mods
  let (cntxt, n_insts) = either
        (const (emptyStaticClassEnv, 0)) id classResult
  let clss = sClassEnv_tclasses cntxt
      insts = sClassEnv_instances cntxt
  -- TODO: try to exfere this stuff
  (decls, deconss, bindingErrors) <- do
    stuff <- mapM (hExtractBinds cntxt ds typeDecls) mods
    let (bindingLists, deconstructorLists, errorLists) = unzip3 stuff
    return
      ( concat bindingLists
      , concat deconstructorLists
      , concat errorLists
      )
  let clssNames = M.keys clss
  let allValidNames = ds ++ clssNames
  let
    warnUnknownTypeConstructors :: String -> [HsType] -> m ()
    warnUnknownTypeConstructors context types = forM_
      (S.toAscList $ S.fromList $ concatMap (findInvalidNames allValidNames) types)
      $ \unknownName -> mTell
          [ "unknown type constructor '" ++ show unknownName
            ++ "' used in " ++ context
          ]

    warnBindingConstraints :: QualifiedName -> HsType -> m ()
    warnBindingConstraints bindingName bindingType = forM_
      (S.toAscList $ S.fromList
        [ renderConstraintFailure bindingName constraint failure
        | constraint <- typeConstraints bindingType
        , Left failure <-
            [validateConstraintInEnv cntxt
              (BindingConstraint bindingName) constraint]
        ])
      (mTell . (: []))

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
      | indexedInstances <- M.elems insts
      , instanceDeclaration <- indexedInstances
      , constraint <- instance_head instanceDeclaration
          : instance_constraints instanceDeclaration
      , parameter <- constraint_params constraint
      ]

  -- Instance inflation can place the same source type under several implied
  -- class heads.  Validate their combined type-constructor set once so an
  -- external constructor produces one useful warning rather than a cascade.
  warnUnknownTypeConstructors "class instances" instanceTypes
  forM_ (M.elems clss) $ \typeClass -> warnUnknownTypeConstructors
    ("the superclass data for " ++ show (tclass_name typeClass))
    [ parameter
    | constraint <- tclass_constraints typeClass
    , parameter <- constraint_params constraint
    ]
  forM_ decls $ \(bindingName, bindingType) -> do
    warnUnknownTypeConstructors
      ("the binding " ++ show bindingName) [bindingType]
    -- Module loading has a complete class inventory and therefore uses the
    -- strict validator.  Public ad-hoc search input deliberately retains its
    -- separate open-world policy for unknown external classes.
    warnBindingConstraints bindingName bindingType
  mTell ["got " ++ show (length clss) ++ " classes"]
  mTell ["and " ++ show (n_insts) ++ " instances"]
  mTell ["(-> " ++ show (length $ concat $ M.elems $ insts) ++ " instances after inflation)"]
  mTell ["and " ++ show (length decls) ++ " function decls"]
  builtInDeclsResult          <- builtInDeclsM
  builtInDeconstructorsResult <- builtInDeconstructorsM
  let reportBuiltInFailure kind failure =
        mTell ["could not construct built-in " ++ kind ++ ": " ++ show failure]
  (builtInDecls, builtInBindingErrors) <- case builtInDeclsResult of
    Left failure -> do
      let message = "could not construct built-in bindings: " ++ show failure
      reportBuiltInFailure "bindings" failure
      pure ([], [message])
    Right bindings -> pure (bindings, [])
  (builtInDeconstructors, builtInDeconstructorErrors) <-
    case builtInDeconstructorsResult of
      Left failure -> do
        let message = "could not construct built-in deconstructors: "
              ++ show failure
        reportBuiltInFailure "deconstructors" failure
        pure ([], [message])
      Right deconstructors -> pure (deconstructors, [])
  let environment = SourceEnvironment
        { sourceFunctions = builtInDecls ++ decls
        , sourceDeconstructors = builtInDeconstructors ++ deconss
        , sourceClasses = cntxt
        , sourceTypeNames = allValidNames
        , sourceTypeSynonyms = validTypeDecls
        }
      loadError = case () of
        _ | Just errors <- NonEmpty.nonEmpty parseErrors ->
              Just $ ModuleParseErrors errors
          | errorMessage : _ <- dataTypeErrors ->
              Just $ DataTypeNameError errorMessage
          | Just errors <- NonEmpty.nonEmpty typeDeclarationErrors ->
              Just $ TypeDeclarationErrors errors
          | Left failure <- classResult ->
              Just $ ClassEnvironmentLoadFailure failure
          | Just errors <- NonEmpty.nonEmpty bindingErrors ->
              Just $ BindingDeclarationErrors errors
          | Just errors <- NonEmpty.nonEmpty
              (builtInBindingErrors ++ builtInDeconstructorErrors) ->
              Just $ BuiltInEnvironmentErrors errors
          | otherwise -> Nothing
  return $ maybe (Right environment) Left loadError
  where
    hRead :: (ParseMode, String) -> IO (ParseMode, String)
    hRead (mode, s) = (,) mode <$> readFile s
    hParse :: (ParseMode, String) -> Either String (Module SrcSpanInfo)
    hParse (mode, content) = case parseModuleWithMode mode content of
      f@(ParseFailed _ _) -> Left $ show f
      ParseOk modul       -> Right modul
    hExtractBinds :: StaticClassEnv
                  -> [QualifiedName]
                  -> TypeDeclMap
                  -> Module SrcSpanInfo
                  -> m ([HsFunctionDecl], [DeconstructorBinding], [String])
    hExtractBinds cntxt ds tDeclMap modul = do
      -- tell $ return $ mname
      eFromData <- getDataConss (sClassEnv_tclasses cntxt) ds tDeclMap [modul]
      eDecls <- (++)
        <$> getDecls ds (sClassEnv_tclasses cntxt) tDeclMap [modul]
        <*> getClassMethods (sClassEnv_tclasses cntxt) ds tDeclMap [modul]
      let errors = lefts eFromData ++ lefts eDecls
      mapM_ (mTell . (:[])) errors
      -- tell $ map show $ rights ebinds
      let (binds1s, deconss) = unzip $ rights eFromData
          binds2 = rights eDecls
      return (concat binds1s ++ binds2, deconss, errors)

-- | A simplified version of environmentFromModules where the input
-- is just one module, parsed with some default ParseMode;
-- the output is transformed so that all functionsbindings get
-- a rating of 0.0.
parseModulesSimple :: ( ContainsType [String] w
                      )
                   => String
                   -> MultiRWST r w s IO
                        (Either EnvironmentLoadError
                          (SourceEnvironment RatedHsFunctionDecl))
parseModulesSimple s = fmap helper
                   <$> parseModules [(haskellSrcExtsParseMode s, s)]
 where
  addRating (a,b) = (a,0.0,b)
  helper environment = environment
    { sourceFunctions = addRating <$> sourceFunctions environment }

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
  contents <- try (readFile path >>= evaluate . force)
    :: IO (Either SomeException String)
  return $ first (withSource path . diagnostic . show) contents
    >>= first (\result -> result { diagnosticSource = Just path }) . parseRatings


-- TODO: add warnings for ratings not applied
environmentFromModuleAndRatings :: ( ContainsType [String] w
                                   )
                                => String
                                -> String
                                -> MultiRWST r w s IO
                                    (Either EnvironmentLoadError
                                      CheckedSourceEnvironment)
environmentFromModuleAndRatings modulePath ratingPath = do
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
  environmentResult <- parseModules [(mode, modulePath)]
  ratingsResult <- lift $ ratingsFromFile ratingPath
  ratings <- case ratingsResult of
    Left e -> do
      mTell ["could not parse rating file", show e]
      pure []
    Right parsedRatings -> pure parsedRatings
  let addRating (name, bindingType) = declToBinding
        (name, fromMaybe 0.0 $ lookup name ratings, bindingType)
      addRatings environment = environment
        { sourceFunctions = map addRating $ sourceFunctions environment }
  return $ environmentResult >>= checkSourceEnvironment . addRatings


environmentFromPath :: ( ContainsType [String] w
                       )
                    => FilePath
                    -> MultiRWST r w s IO
                         (Either EnvironmentLoadError
                           CheckedSourceEnvironment)
environmentFromPath p = do
  files <- lift $ getDirectoryContents p
  -- Directory enumeration order is platform-dependent; stable ordering keeps
  -- diagnostics and duplicate resolution reproducible.
  let modules = ((p ++ "/")++) <$> sort (filter (".hs" `isSuffixOf`) files)
  let ratings = ((p ++ "/")++) <$> sort (filter (".ratings" `isSuffixOf`) files)
  environmentResult <- parseModules
    [ (mode, m)
    | m <- modules
    , let mode = haskellSrcExtsParseMode m]
  rResult <- lift $ ratingsFromFile `mapM` ratings
  let rs = [x | Right xs <- rResult, x <- xs]
  sequence_ $ do
    Left err <- rResult
    return $ mTell ["could not parse rating file", show err]
  (rs' :: [(QualifiedName, Penalty)]) <- fmap join $ sequence $ do
    (rName, rVal) <- rs
    return $ do
      dIds <- fmap join $ sequence $ do
        environment <- either (const []) pure environmentResult
        (dName, _) <- sourceFunctions environment
        return $ do
          return $ do
            guard (rName == dName)
            return (dName, rVal)
      case dIds of
        [] -> do
          mTell ["rating could not be applied: " ++ show rName]
          return []
        [x] ->
          return [x]
        _ -> do
          mTell ["duplicate function: " ++ show rName]
          return []
  let f (a,b) = declToBinding
              $ ( a
                , fromMaybe 0.0 (lookup a rs')
                , b
                )
      addRatings environment = environment
        { sourceFunctions = map f $ sourceFunctions environment }
  return $ environmentResult >>= checkSourceEnvironment . addRatings
