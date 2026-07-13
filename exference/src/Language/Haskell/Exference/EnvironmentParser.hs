{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeOperators #-}

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
  , UnsupportedVocabularyForm (..)
  , UnsupportedVocabularyOccurrence (..)
  , unsupportedVocabularyOccurrences
  , EnvironmentLoadError (..)
  , environmentLoadErrorDiagnostics
  , parseModules
  , environmentFromModule
  , environmentFromModuleAndRatings
  , environmentFromPath
  , toSynthesisSourceEnvironment
  , toSynthesisSourceInventory
  , sourceTypeSynonymMap
  , haskellSrcExtsParseMode
  , parseRatings
  )
where



import Language.Haskell.Exference
import Language.Haskell.Exference.BindingsFromHaskellSrc
import Language.Haskell.Exference.ClassEnvFromHaskellSrc
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.HaskellSrcUtils
  (contextConstraints, splitDeclHead)
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.Core.Declaration

import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Diagnostic

import Control.DeepSeq

import Control.Monad ( forM_, zipWithM )
import Data.List ( sort, isSuffixOf )
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

import Language.Haskell.Exts.Syntax ( Module )
import qualified Language.Haskell.Exts.Syntax as HSE
import Language.Haskell.Exts.Parser ( parseModuleWithMode
                                    , ParseResult (..)
                                    , ParseMode
                                    )
import Language.Haskell.Exts.SrcLoc ( SrcSpanInfo )
import qualified Language.Haskell.Exts.SrcLoc as HSE

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
data SourceBinding
  = SourceFunction FunctionBinding
  | SourceClassMethod QualifiedName FunctionBinding
  deriving (Show)

sourceBindingFunction :: SourceBinding -> FunctionBinding
sourceBindingFunction binding = case binding of
  SourceFunction function -> function
  SourceClassMethod _ function -> function

-- | The complete source inventory produced by the HSE frontend. Signatures
-- enter the backend's named 'FunctionBinding' shape during extraction, so
-- rating and checked lowering update one representation rather than carrying
-- a parallel raw tuple layer.
data SourceEnvironment = SourceEnvironment
  { sourceBindings :: [SourceBinding]
  , sourceDeconstructors :: [DeconstructorBinding]
  , sourceClasses :: StaticClassEnv
  , sourceTypeNames :: [QualifiedName]
  , sourceTypeSynonyms :: [HsTypeDecl]
  }
  deriving (Show)

-- | Historical flat function view. Ownership remains available through
-- 'sourceBindings', while search and compatibility clients see every method
-- exactly once in its original list position.
sourceFunctions :: SourceEnvironment -> [FunctionBinding]
sourceFunctions = map sourceBindingFunction . sourceBindings

-- | A backend projection paired with the exact shared inventory that validated
-- it.  The constructor is private so CLI and library loaders cannot expose a
-- searchable source environment before structural and kind sealing succeeds.
data CheckedSourceEnvironment = CheckedSourceEnvironment
  { checkedSourceProjection :: SourceEnvironment
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

-- | Haskell source constructs whose type, class, or instance meaning cannot
-- yet be represented by Exference's nominal source inventory. Keeping this
-- distinction separate from parser failures lets callers report an exact,
-- stable reason without pretending that accepted syntax was loaded.
data UnsupportedVocabularyForm
  = ExplicitExportList
  | OpenTypeFamily
  | ClosedTypeFamily
  | DataFamily
  | GadtDeclaration
  | TypeFamilyInstance
  | DataFamilyInstance
  | GadtDataFamilyInstance
  | StandaloneDeriving
  | DeclarationSplice
  | TypedDeclarationSplice
  | RoleAnnotation
  | DerivingClause
  | DataTypeContext
  | KindedDataBinder
  | ExistentialConstructor
  | ConstrainedConstructor
  | FunctionalDependency
  | InstanceOverlapMode
  | AssociatedDataFamily
  | AssociatedTypeFamily
  | AssociatedTypeDefault
  | DefaultMethodSignature
  | PatternSynonymSignature
  | AssociatedTypeInstance
  | AssociatedDataInstance
  | AssociatedGadtDataInstance
  | XmlPageModule
  | XmlHybridModule
  deriving (Eq, Ord, Show)

-- | One rejected source occurrence paired with a precise shared diagnostic.
-- Occurrences retain module and source order in 'EnvironmentLoadError'.
data UnsupportedVocabularyOccurrence = UnsupportedVocabularyOccurrence
  { unsupportedVocabularyForm :: UnsupportedVocabularyForm
  , unsupportedVocabularyDiagnostic :: Diagnostic
  }
  deriving (Eq, Show)

-- | Fatal source-loading phases.  Warnings remain in the load report, but
-- a failed class graph cannot produce a searchable recovery environment.
data EnvironmentLoadError
  = EnvironmentDirectoryReadError Diagnostic
  | ModuleReadErrors (NonEmpty Diagnostic)
  | ModuleParseErrors (NonEmpty String)
  | UnsupportedSourceVocabulary
      (NonEmpty UnsupportedVocabularyOccurrence)
  | DataTypeNameError String
  | TypeDeclarationErrors (NonEmpty String)
  | ClassEnvironmentLoadFailure ClassEnvironmentLoadError
  | BindingDeclarationErrors (NonEmpty String)
  | BuiltInEnvironmentErrors (NonEmpty String)
  | InvalidSourceInventory SynthesisDeclarationError
  deriving (Eq, Show)

-- | Project every fatal loader phase into the shared diagnostic vocabulary.
-- Diagnostics that already came from a parser or IO boundary retain their
-- severity, message, context, and exact source span; only a stable phase code
-- is attached.  Older extraction phases still report strings, so this adapter
-- keeps their original detail as structured context.
environmentLoadErrorDiagnostics
  :: EnvironmentLoadError
  -> NonEmpty Diagnostic
environmentLoadErrorDiagnostics failure = case failure of
  EnvironmentDirectoryReadError value ->
    withCode "EXF_ENV_DIRECTORY_READ" value NonEmpty.:| []
  ModuleReadErrors values -> fmap (withCode "EXF_MODULE_READ") values
  ModuleParseErrors errors -> diagnostics
    "EXF_MODULE_PARSE"
    "could not parse a Haskell source module"
    errors
  UnsupportedSourceVocabulary occurrences ->
    fmap unsupportedVocabularyDiagnostic occurrences
  DataTypeNameError detail -> oneDiagnostic
    "EXF_DATA_TYPE_NAME"
    "could not extract source data-type names"
    detail
  TypeDeclarationErrors errors -> diagnostics
    "EXF_TYPE_DECLARATION"
    "could not load a source type declaration"
    errors
  ClassEnvironmentLoadFailure classFailure ->
    classEnvironmentLoadErrorDiagnostics classFailure
  BindingDeclarationErrors errors -> diagnostics
    "EXF_BINDING_DECLARATION"
    "could not load a source binding declaration"
    errors
  BuiltInEnvironmentErrors errors -> diagnostics
    "EXF_BUILTIN_ENVIRONMENT"
    "could not construct Exference's built-in source environment"
    errors
  InvalidSourceInventory inventoryFailure -> oneDiagnostic
    "EXF_SOURCE_INVENTORY"
    "the source environment failed shared inventory validation"
    (show inventoryFailure)
 where
  diagnostics code message = fmap $ structuredDiagnostic code message
  oneDiagnostic code message detail =
    structuredDiagnostic code message detail NonEmpty.:| []

classEnvironmentLoadErrorDiagnostics
  :: ClassEnvironmentLoadError
  -> NonEmpty Diagnostic
classEnvironmentLoadErrorDiagnostics failure = case failure of
  ClassDeclarationErrors errors -> diagnostics
    "EXF_CLASS_DECLARATION"
    "could not load a source class declaration"
    errors
  InstanceDeclarationErrors errors -> diagnostics
    "EXF_INSTANCE_DECLARATION"
    "could not load a source instance declaration"
    errors
  InvalidClassEnvironment classFailure -> oneDiagnostic
    "EXF_CLASS_ENVIRONMENT"
    "the source class environment failed nominal validation"
    (show classFailure)
 where
  diagnostics code message = fmap $ structuredDiagnostic code message
  oneDiagnostic code message detail =
    structuredDiagnostic code message detail NonEmpty.:| []

structuredDiagnostic :: String -> String -> String -> Diagnostic
structuredDiagnostic code message detail = withContext detail
  $ withCode code
  $ diagnostic message

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
  :: SourceEnvironment
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
  -> SourceEnvironment
  -> Either SynthesisDeclarationError
      SourceEnvironment
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
sourceTypeSynonymMap :: SourceEnvironment -> TypeDeclMap
sourceTypeSynonymMap = uniqueTypeDeclMap . sourceTypeSynonyms

-- | Seal the complete frontend inventory in the common environment IR.
-- Unlike the search-core 'EnvDictionary' adapter, this boundary retains type
-- synonyms so later validation does not have to rediscover declarations from
-- the HSE modules or a parallel tuple field.
toSynthesisSourceEnvironment
  :: SourceEnvironment
  -> Either SynthesisDeclarationError SynthesisEnvironment
toSynthesisSourceEnvironment environment =
  SharedInventory.inventoryEnvironment
    <$> toSynthesisSourceInventory environment

-- | Seal and kind-check the frontend inventory while retaining the inferred
-- assumptions needed to elaborate subsequent queries against the same source
-- declarations.
toSynthesisSourceInventory
  :: SourceEnvironment
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


builtInBindings :: Either QualifiedNameError [FunctionBinding]
builtInBindings = do
  consName <- fromSynthesisName SharedName.consName
  listName <- fromSynthesisName SharedName.listName
  unitConstructor <- do
    unitName <- mkBoxedTupleName 0
    pure $ functionBindingFromType unitName 0 $ TypeCons unitName
  tupleConstructors <- mapM tupleConstructor [2 .. 7]
  pure $ listConstructors consName listName
    ++ (unitConstructor : tupleConstructors)
 where
  listConstructors consName listName =
    [ functionBindingFromType listName 0 $ listType listName
    , functionBindingFromType consName 0 $ TypeArrow (TypeVar 0)
        $ TypeArrow (listType listName) (listType listName)
    ]
  listType listName = TypeApp (TypeCons listName) (TypeVar 0)
  tupleConstructor arity = do
    tupleName <- mkBoxedTupleName arity
    pure $ functionBindingFromType tupleName 0
      $ foldr TypeArrow (tupleType tupleName arity) $ typeVariables arity

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

-- | Find every declaration whose source-level type/class meaning would be
-- lost by the current extractor. Value definitions, imports, fixities,
-- default declarations, untyped pattern bodies, and benign pragmas
-- deliberately remain outside this scan.
unsupportedVocabularyOccurrences
  :: [Module SrcSpanInfo]
  -> [UnsupportedVocabularyOccurrence]
unsupportedVocabularyOccurrences = concatMap unsupportedModule
 where
  unsupportedModule modul = case modul of
    HSE.Module _ moduleHead _ _ declarations ->
      maybe [] unsupportedModuleHead moduleHead
        ++ concatMap unsupportedDecl declarations
    -- An XML page is an executable module form, not an ordinary module with
    -- no declarations.  The declaration extractors deliberately return
    -- 'Nothing' for it, so accepting it here would manufacture an empty
    -- source inventory.
    HSE.XmlPage location _ _ _ _ _ _ ->
      [unsupportedOccurrence XmlPageModule location]
    -- Hybrid HSP modules do carry ordinary declarations, but the loader's
    -- module/name projection deliberately has no semantics for the XML half.
    -- Reject the boundary once instead of silently discarding all declarations
    -- or emitting a cascade for children we cannot load in context.
    HSE.XmlHybrid location _ _ _ _ _ _ _ _ ->
      [unsupportedOccurrence XmlHybridModule location]

  unsupportedModuleHead (HSE.ModuleHead _ _ _ maybeExports) =
    maybe [] unsupportedExports maybeExports

  unsupportedExports (HSE.ExportSpecList location _) =
    one ExplicitExportList location

  unsupportedDecl declaration = case declaration of
    HSE.TypeFamDecl location _ _ _ ->
      one OpenTypeFamily location
    HSE.ClosedTypeFamDecl location _ _ _ _ ->
      one ClosedTypeFamily location
    HSE.DataFamDecl location _ _ _ ->
      one DataFamily location
    HSE.GDataDecl location _ _ _ _ _ _ ->
      one GadtDeclaration location
    HSE.TypeInsDecl location _ _ ->
      one TypeFamilyInstance location
    HSE.DataInsDecl location _ _ _ _ ->
      one DataFamilyInstance location
    HSE.GDataInsDecl location _ _ _ _ _ ->
      one GadtDataFamilyInstance location
    HSE.DerivDecl location _ _ _ ->
      one StandaloneDeriving location
    HSE.SpliceDecl location _ ->
      one DeclarationSplice location
    HSE.TSpliceDecl location _ ->
      one TypedDeclarationSplice location
    HSE.PatSynSig location _ _ _ _ _ _ ->
      one PatternSynonymSignature location
    HSE.RoleAnnotDecl location _ _ ->
      one RoleAnnotation location
    HSE.DataDecl _ _ context rawHead constructors derivings ->
      maybe [] (unsupportedContext DataTypeContext) context
        ++ concatMap unsupportedDataBinder (snd $ splitDeclHead rawHead)
        ++ concatMap unsupportedConstructor constructors
        ++ concatMap unsupportedDeriving derivings
    HSE.ClassDecl _ _ _ dependencies declarations ->
      concatMap unsupportedDependency dependencies
        ++ concatMap unsupportedClassDecl (maybe [] id declarations)
    HSE.InstDecl _ overlap _ declarations ->
      maybe [] unsupportedOverlap overlap
        ++ concatMap unsupportedInstanceDecl (maybe [] id declarations)
    _ -> []

  unsupportedDeriving (HSE.Deriving location _ _) =
    one DerivingClause location

  -- The common inventory has a place for parameter kinds, but Exference's
  -- current type-variable index does not. Reject these declarations before
  -- either projection can observe a differently shaped data constructor.
  unsupportedDataBinder binder = case binder of
    HSE.KindedVar location _ _ -> one KindedDataBinder location
    HSE.UnkindedVar _ _ -> []

  unsupportedConstructor
      (HSE.QualConDecl location maybeBinders context _) =
    (case maybeBinders of
      Just (_ : _) -> one ExistentialConstructor location
      _ -> [])
      ++ maybe [] (unsupportedContext ConstrainedConstructor) context

  unsupportedContext form context
    | null $ contextConstraints $ Just context = []
    | otherwise = one form $ contextLocation context

  contextLocation context = case context of
    HSE.CxSingle location _ -> location
    HSE.CxTuple location _ -> location
    HSE.CxEmpty location -> location

  unsupportedDependency (HSE.FunDep location _ _) =
    one FunctionalDependency location

  unsupportedOverlap overlap = case overlap of
    HSE.NoOverlap location -> one InstanceOverlapMode location
    HSE.Overlap location -> one InstanceOverlapMode location
    HSE.Overlapping location -> one InstanceOverlapMode location
    HSE.Overlaps location -> one InstanceOverlapMode location
    HSE.Overlappable location -> one InstanceOverlapMode location
    HSE.Incoherent location -> one InstanceOverlapMode location

  unsupportedClassDecl declaration = case declaration of
    HSE.ClsDataFam location _ _ _ ->
      one AssociatedDataFamily location
    HSE.ClsTyFam location _ _ _ ->
      one AssociatedTypeFamily location
    HSE.ClsTyDef location _ ->
      one AssociatedTypeDefault location
    HSE.ClsDefSig location _ _ ->
      one DefaultMethodSignature location
    -- Inspect the wrapped declaration as well: ordinary signatures and value
    -- bodies remain irrelevant, while a publicly constructed or extension-
    -- accepted pattern-synonym signature must not bypass the same boundary.
    HSE.ClsDecl _ wrappedDeclaration -> unsupportedDecl wrappedDeclaration

  unsupportedInstanceDecl declaration = case declaration of
    HSE.InsType location _ _ ->
      one AssociatedTypeInstance location
    HSE.InsData location _ _ _ _ ->
      one AssociatedDataInstance location
    HSE.InsGData location _ _ _ _ _ ->
      one AssociatedGadtDataInstance location
    -- Method signatures, implementations, and benign pragmas still produce
    -- no occurrences; recurse so unsupported typed pattern vocabulary cannot
    -- hide inside the generic declaration wrapper.
    HSE.InsDecl _ wrappedDeclaration -> unsupportedDecl wrappedDeclaration

  one form location = [unsupportedOccurrence form location]

unsupportedOccurrence
  :: UnsupportedVocabularyForm
  -> SrcSpanInfo
  -> UnsupportedVocabularyOccurrence
unsupportedOccurrence form location = UnsupportedVocabularyOccurrence form
  $ withCode "EXF_UNSUPPORTED_VOCABULARY"
  $ withSpan (SourceSpan
      (SourcePosition
        (HSE.srcSpanStartLine sourceSpan)
        (HSE.srcSpanStartColumn sourceSpan))
      (SourcePosition
        (HSE.srcSpanEndLine sourceSpan)
        (HSE.srcSpanEndColumn sourceSpan)))
  $ withSource (HSE.srcSpanFilename sourceSpan)
  $ diagnostic $ "unsupported source vocabulary: "
      ++ unsupportedVocabularyDescription form
 where
  sourceSpan = HSE.srcInfoSpan location

unsupportedVocabularyDescription :: UnsupportedVocabularyForm -> String
unsupportedVocabularyDescription form = case form of
  ExplicitExportList -> "explicit module export list"
  OpenTypeFamily -> "open type-family declaration"
  ClosedTypeFamily -> "closed type-family declaration"
  DataFamily -> "data-family declaration"
  GadtDeclaration -> "GADT-style data declaration"
  TypeFamilyInstance -> "type-family instance"
  DataFamilyInstance -> "data-family instance"
  GadtDataFamilyInstance -> "GADT-style data-family instance"
  StandaloneDeriving -> "standalone deriving declaration"
  DeclarationSplice -> "Template Haskell declaration splice"
  TypedDeclarationSplice -> "typed Template Haskell declaration splice"
  RoleAnnotation -> "type-role annotation"
  DerivingClause -> "derived class instances"
  DataTypeContext -> "datatype context"
  KindedDataBinder -> "explicitly kinded datatype parameter"
  ExistentialConstructor -> "constructor with existential type variables"
  ConstrainedConstructor -> "constructor context"
  FunctionalDependency -> "class functional dependency"
  InstanceOverlapMode -> "instance overlap mode"
  AssociatedDataFamily -> "associated data-family declaration"
  AssociatedTypeFamily -> "associated type-family declaration"
  AssociatedTypeDefault -> "default associated-type equation"
  DefaultMethodSignature -> "default method signature"
  PatternSynonymSignature -> "pattern-synonym signature"
  AssociatedTypeInstance -> "associated type-family instance"
  AssociatedDataInstance -> "associated data-family instance"
  AssociatedGadtDataInstance ->
    "GADT-style associated data-family instance"
  XmlPageModule -> "XML page module"
  XmlHybridModule -> "XML hybrid module"

-- | Load source modules in the supplied order.  The returned diagnostics are
-- deterministic: messages within a module retain source order, while sets of
-- unknown names are rendered in nominal sort order.
parseModules
  :: [(ParseMode, FilePath)]
  -> IO (LoadReport SourceEnvironment)
parseModules inputs = do
  (result, diagnostics) <- runWriterT $ parseModulesM inputs
  pure $ LoadReport result diagnostics

type Loader = WriterT [Diagnostic] IO

parseModulesM
  :: [(ParseMode, FilePath)]
  -> Loader (Either EnvironmentLoadError SourceEnvironment)
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
      -> Loader (Either EnvironmentLoadError SourceEnvironment)
    parseLoadedModules rawTuples = runExceptT $ do
      let parsedModules = map hParse rawTuples
          parseErrors = lefts parsedModules
      case NonEmpty.nonEmpty parseErrors of
        Just errors -> throwE $ ModuleParseErrors errors
        Nothing -> pure ()
      let modules = rights parsedModules

      case NonEmpty.nonEmpty $ unsupportedVocabularyOccurrences modules of
        Just occurrences ->
          throwE $ UnsupportedSourceVocabulary occurrences
        Nothing -> pure ()

      dataTypes <- case getDataTypes modules of
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
        $ loadClassEnvironment dataTypes typeDeclarationMap modules
      loadedClasses <- either
        (throwE . ClassEnvironmentLoadFailure)
        pure
        classResult
      let classEnvironment = loadedStaticClassEnvironment loadedClasses
          instanceCount = loadedSourceInstanceCount loadedClasses
          methodsByModule = loadedClassMethodsByModule loadedClasses

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

      let builtInDeclarationsResult = builtInBindings
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

          warnBindingConstraints
            :: QualifiedName
            -> [HsConstraint]
            -> Loader ()
          warnBindingConstraints bindingName constraints = forM_
            (S.toAscList $ S.fromList
              [ renderConstraintFailure bindingName constraint failure
              | constraint <- constraints
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
        forM_ (map sourceBindingFunction declarations) $ \binding -> do
          let bindingName = functionName binding
              outerConstraints = functionConstraints binding
              bindingTypes = functionResult binding
                : ( functionParameters binding
                    ++ concatMap constraint_params outerConstraints
                  )
              -- The source signature has already been split into a binding,
              -- but nested foralls can still carry constraints in its result,
              -- parameters, or the arguments of an outer constraint. Preserve
              -- the complete recursive coverage of 'typeConstraints'.
              bindingConstraints = outerConstraints
                ++ concatMap typeConstraints bindingTypes
          warnUnknownTypeConstructors
            ("the binding " ++ show bindingName) bindingTypes
          -- The loader has a complete class inventory, unlike public ad-hoc
          -- search input, so binding constraints are checked nominally here.
          warnBindingConstraints bindingName bindingConstraints
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
                       ( [SourceBinding]
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
  -> SourceEnvironment
  -> (SourceEnvironment, [Diagnostic])
applyRatings ratings environment =
  ( environment
      { sourceBindings = map rateSourceBinding $ sourceBindings environment }
  , lefts ratingResults
  )
 where
  declarationCounts = M.fromListWith (+)
    [ (functionName binding, 1 :: Int)
    | binding <- sourceFunctions environment
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

  rateSourceBinding sourceBinding = case sourceBinding of
    SourceFunction binding -> SourceFunction $ rateBinding binding
    SourceClassMethod owner binding ->
      SourceClassMethod owner $ rateBinding binding

  rateBinding binding = binding
    { functionPenalty = M.findWithDefault 0
        (functionName binding) effectiveRatings
    }

-- | Load and seal one source module with neutral search penalties. This is the
-- checked convenience counterpart of 'parseModules'; it shares every parsing,
-- declaration, rating, and inventory-validation phase with the file loaders.
environmentFromModule
  :: FilePath
  -> IO (LoadReport CheckedSourceEnvironment)
environmentFromModule modulePath = do
  (result, diagnostics) <- runWriterT
    $ environmentFromModuleM modulePath
  pure $ LoadReport result diagnostics

environmentFromModuleM
  :: FilePath
  -> Loader (Either EnvironmentLoadError CheckedSourceEnvironment)
environmentFromModuleM modulePath = do
  environmentResult <- parseModulesM
    [(haskellSrcExtsParseMode modulePath, modulePath)]
  case environmentResult of
    Left failure -> pure $ Left failure
    Right environment -> rateAndCheckEnvironment [] environment

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
  environmentResult <- parseModulesM
    [(haskellSrcExtsParseMode modulePath, modulePath)]
  case environmentResult of
    Left failure -> pure $ Left failure
    Right environment -> do
      ratingsResult <- lift $ ratingsFromFile ratingPath
      ratings <- case ratingsResult of
        Left failure -> do
          tell [ratingFailureDiagnostic failure]
          pure []
        Right parsedRatings -> pure parsedRatings
      rateAndCheckEnvironment ratings environment


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
          rateAndCheckEnvironment ratings environment

rateAndCheckEnvironment
  :: [(QualifiedName, Penalty)]
  -> SourceEnvironment
  -> Loader (Either EnvironmentLoadError CheckedSourceEnvironment)
rateAndCheckEnvironment ratings environment = do
  let (ratedEnvironment, warnings) = applyRatings ratings environment
  tell warnings
  pure $ checkSourceEnvironment ratedEnvironment
