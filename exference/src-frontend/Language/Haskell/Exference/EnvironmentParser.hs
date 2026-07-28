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
  , checkedSourcePreparedInventory
  , checkSourceEnvironment
  , LoadReport (..)
  , UnsupportedVocabularyForm (..)
  , UnsupportedVocabularyOccurrence (..)
  , unsupportedVocabularyOccurrences
  , EnvironmentLoadError (..)
  , environmentLoadErrorDiagnostics
  , parseModules
  , parseModuleSources
  , environmentFromModule
  , environmentFromModuleAndRatings
  , environmentFromFiles
  , environmentFromSources
  , environmentFromPath
  , toSynthesisSourceEnvironment
  , toSynthesisSourceInventory
  , sourceTypeSynonymMap
  , haskellSrcExtsParseMode
  , parseRatings
  , maximumBuiltInTupleArity
  )
where



import Language.Haskell.Exference.BindingsFromHaskellSrc
import Language.Haskell.Exference.ClassEnvFromHaskellSrc
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.ExtractionError
import Language.Haskell.Exference.Internal.SourceTypeScope
  ( sourceClassArities
  , sourceTypeResolvers
  )
import Language.Haskell.Exference.HaskellSrcUtils
  ( contextConstraints
  , moduleNameAndDecls
  , splitDeclHead
  , splitInstRule
  , withHaskellSrcLocation
  , withHaskellSrcSpan
  )
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.Core.Declaration
import Language.Haskell.Exference.Core.Score (Penalty (..))
import Language.Haskell.Djex.Internal.DependencyGraph
  ( DependencyCycle (..)
  , stableDependencyOrder
  )

import Language.Haskell.Exference.Core.Types
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic (..)
  , Severity (Error, Info, Warning)
  , codedDiagnostic
  , contextualDiagnostic
  , diagnostic
  , withCode
  , withSource
  )

import Control.DeepSeq

import Control.Monad ( forM_ )
import Data.List ( intercalate, sort, sortOn, isSuffixOf )
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Either ( lefts, rights )
import Data.Maybe ( maybeToList )
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.Strict (WriterT, runWriterT, tell)
import System.Directory ( listDirectory )
import Control.Exception ( evaluate )
import Data.Bifunctor ( first )
import System.FilePath ( (</>) )
import System.IO.Error ( tryIOError )

import Language.Haskell.Exts (parseFileContentsWithMode)
import Language.Haskell.Exts.Syntax ( Module )
import qualified Language.Haskell.Exts.Syntax as HSE
import Language.Haskell.Exts.Parser ( ParseResult (..)
                                    , ParseMode
                                    )
import Language.Haskell.Exts.SrcLoc ( SrcSpanInfo )
import qualified Language.Haskell.Exts.SrcLoc as HSE

import Control.Monad.Trans.Except (runExceptT, throwE)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Void (absurd)
import Text.Read ( readMaybe )
import qualified Language.Haskell.Synthesis.Collection as SharedCollection
import qualified Language.Haskell.Synthesis.Count as SharedCount
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Declaration as SharedDeclaration
import qualified Language.Haskell.Synthesis.Environment as SharedEnvironment
import qualified Language.Haskell.Synthesis.KindInference as SharedKindInference
import qualified Language.Haskell.Synthesis.Inventory as SharedInventory
import qualified Language.Haskell.Synthesis.Type as SharedType
import qualified Language.Haskell.Synthesis.TypeSynonym as SharedTypeSynonym

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
-- enter the backend's named t'FunctionBinding' shape during extraction, so
-- rating and checked lowering update one representation rather than carrying
-- a parallel raw tuple layer.
data SourceEnvironment = SourceEnvironment
  { sourceBindings :: [SourceBinding]
  , sourceDeconstructors :: [DeconstructorBinding]
  , sourceClasses :: StaticClassEnv
  , sourceTypeNames :: [QualifiedName]
      -- ^ Compatibility lookup cache. Raw callers may populate it, but
      -- 'checkSourceEnvironment' ignores it; 'checkedSourceProjection'
      -- derives a fresh view from declarations that survived validation and
      -- backend normalization.
  , sourceTypeSynonyms :: [HsTypeDecl]
  }
  deriving (Show)

-- | Historical flat function view. Ownership remains available through
-- 'sourceBindings', while search and compatibility clients see every method
-- exactly once in its original list position.
sourceFunctions :: SourceEnvironment -> [FunctionBinding]
sourceFunctions = map sourceBindingFunction . sourceBindings

-- | One authoritative prepared source inventory plus the historical synonym
-- spellings needed by the compatibility frontend. Every other source view is
-- derived from the witness's ordered, rated backend and checked declarations.
-- The constructor remains private so callers cannot recombine these spellings
-- with an unrelated semantic environment.
data CheckedSourceEnvironment = CheckedSourceEnvironment
  (PreparedSynthesisInventory DeclarationMetadata)
  [HsTypeDecl]

-- | The exact annotated Inventory owned by the prepared source witness.
checkedSourceInventory :: CheckedSourceEnvironment -> SynthesisInventory
checkedSourceInventory (CheckedSourceEnvironment prepared _) =
  SharedTypeSynonym.preparedInventory $ preparedSynthesisWitness prepared

-- | An annotation-free view for the stable core session. This shares the
-- already prepared synonyms and backend; it never repeats lowering.
checkedSourcePreparedInventory
  :: CheckedSourceEnvironment
  -> PreparedSynthesisInventory ()
checkedSourcePreparedInventory (CheckedSourceEnvironment prepared _) =
  erasePreparedSynthesisAnnotations prepared

-- | Reconstruct the historical flat frontend view from the one prepared
-- witness. Only source synonym spellings are retained separately; function
-- order, ratings, datatype recursion, and classes come from the backend, while
-- method ownership comes from the checked shared declarations.
checkedSourceProjection :: CheckedSourceEnvironment -> SourceEnvironment
checkedSourceProjection (CheckedSourceEnvironment prepared synonyms) =
  SourceEnvironment
    { sourceBindings = map tagBinding $ environmentFunctions backend
    , sourceDeconstructors = environmentDeconstructors backend
    , sourceClasses = classes
    , sourceTypeNames = filter ordinaryTypeName dataNames
        ++ map tdecl_name synonyms
        ++ M.keys (sClassEnv_tclasses classes)
    , sourceTypeSynonyms = synonyms
    }
 where
  backend = preparedSynthesisBackend prepared
  classes = environmentClasses backend
  shared = SharedInventory.inventoryEnvironment
    $ SharedTypeSynonym.preparedInventory
    $ preparedSynthesisWitness prepared
  dataNames =
    [ name
    | SharedDeclaration.DataTypeDeclaration _ name _ _ <-
        SharedEnvironment.environmentDeclarations shared
    ]
  methodOwners = M.fromList
    [ (SharedDeclaration.valueName method, owner)
    | SharedDeclaration.ClassDeclaration _ owner _ _ methods <-
        M.elems $ SharedEnvironment.classDeclarationMap shared
    , method <- methods
    ]
  tagBinding binding = case M.lookup (functionName binding) methodOwners of
    Nothing -> SourceFunction binding
    Just owner -> SourceClassMethod owner binding

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
    -- ^ Retained as a compatibility spelling, but no longer emitted. The
    -- source loader keeps every declaration in the checked inventory while a
    -- module-aware caller applies the export list to its visibility scope.
  | PackageQualifiedImport
  | UnparenthesizedTypeOperatorChain
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
  | KindedClassBinder
  | KindedSynonymBinder
  | KindedInstanceBinder
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
  | ModuleParseErrors (NonEmpty Diagnostic)
  | DuplicateModuleDeclarations (NonEmpty Diagnostic)
    -- ^ Later declarations of an already loaded logical module, including
    -- headerless modules that all declare @Main@.
  | CyclicModuleImports (NonEmpty Diagnostic)
    -- ^ Ordinary imports form a cycle. SOURCE imports are interface edges and
    -- deliberately do not participate in this source-dependency graph.
  | UnsupportedSourceVocabulary
      (NonEmpty UnsupportedVocabularyOccurrence)
  | DataTypeNameError ExtractionError
  | TypeDeclarationErrors (NonEmpty ExtractionError)
  | ClassEnvironmentLoadFailure ClassEnvironmentLoadError
  | BindingDeclarationErrors (NonEmpty ExtractionError)
  -- Built-in environment failures come from the hard-coded constructor
  -- table, which has no source location even in principle; they keep the
  -- bare string channel deliberately.
  | BuiltInEnvironmentErrors (NonEmpty String)
  | InvalidSourceInventory SynthesisEnvironmentError
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
  ModuleParseErrors values -> fmap (withCode "EXF_MODULE_PARSE") values
  DuplicateModuleDeclarations values -> values
  CyclicModuleImports values -> values
  UnsupportedSourceVocabulary occurrences ->
    fmap unsupportedVocabularyDiagnostic occurrences
  DataTypeNameError detail -> locatedDiagnostic
    "EXF_DATA_TYPE_NAME"
    "could not extract source data-type names"
    detail NonEmpty.:| []
  TypeDeclarationErrors errors -> locatedDiagnostics
    "EXF_TYPE_DECLARATION"
    "could not load a source type declaration"
    errors
  ClassEnvironmentLoadFailure classFailure ->
    classEnvironmentLoadErrorDiagnostics classFailure
  BindingDeclarationErrors errors -> locatedDiagnostics
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
  locatedDiagnostics code message = fmap $ locatedDiagnostic code message

classEnvironmentLoadErrorDiagnostics
  :: ClassEnvironmentLoadError
  -> NonEmpty Diagnostic
classEnvironmentLoadErrorDiagnostics failure = case failure of
  ClassDeclarationErrors errors -> locatedDiagnostics
    "EXF_CLASS_DECLARATION"
    "could not load a source class declaration"
    errors
  InstanceDeclarationErrors errors -> locatedDiagnostics
    "EXF_INSTANCE_DECLARATION"
    "could not load a source instance declaration"
    errors
  InvalidClassEnvironment classFailure -> oneDiagnostic
    "EXF_CLASS_ENVIRONMENT"
    "the source class environment failed nominal validation"
    (show classFailure)
 where
  locatedDiagnostics code message = fmap $ locatedDiagnostic code message
  oneDiagnostic code message detail =
    structuredDiagnostic code message detail NonEmpty.:| []

structuredDiagnostic :: String -> String -> String -> Diagnostic
structuredDiagnostic code message =
  contextualDiagnostic Error code message

-- One extraction failure rendered with its historical message and, when the
-- parse tree provided one, the owning declaration's source location.
locatedDiagnostic :: String -> String -> ExtractionError -> Diagnostic
locatedDiagnostic code message failure = withExtractionLocation failure
  $ structuredDiagnostic code message
  $ extractionErrorMessage failure

warningDiagnostic :: String -> Diagnostic
warningDiagnostic = diagnostic Warning

infoDiagnostic :: String -> Diagnostic
infoDiagnostic = diagnostic Info

-- | Catch only filesystem failures, preserving asynchronous cancellation and
-- programming exceptions.  The source path is structural diagnostic data, so
-- callers never need to recover it from platform-specific exception text.
captureIO :: FilePath -> IO value -> IO (Either Diagnostic value)
captureIO path action = first
  (withSource path . diagnostic Error . show) <$> tryIOError action

-- | Force lazy text while the IOException boundary is still active.  Merely
-- wrapping 'readFile' would let decoding and deferred reads escape later.
readTextFile :: FilePath -> IO (Either Diagnostic String)
readTextFile path = captureIO path
  $ readFile path >>= evaluate . force

checkSourceEnvironment
  :: SourceEnvironment
  -> Either EnvironmentLoadError CheckedSourceEnvironment
checkSourceEnvironment environment = do
  sourceInventory <- first InvalidSourceInventory
    $ toSynthesisSourceInventory environment
  prepared <- first InvalidSourceInventory
    $ prepareSourceSynthesisInventory sourceInventory
  projected <- first InvalidSourceInventory
    $ normalizeBackendProjection prepared environment
  pure $ CheckedSourceEnvironment projected
    (sourceTypeSynonyms environment)

-- | Lower the checked, alias-preserving inventory exactly once, then reconcile
-- its backend records with source order and ratings. The neutral lowerer owns
-- capture-safe synonym expansion, declaration-local variable normalization,
-- and global recursive-datatype classification; the HSE projection contributes
-- only presentation order and heuristic penalties. Method ownership is later
-- recovered from the prepared witness's checked class declarations.
normalizeBackendProjection
  :: PreparedSynthesisInventory annotation
  -> SourceEnvironment
  -> Either SynthesisEnvironmentError
      (PreparedSynthesisInventory annotation)
normalizeBackendProjection prepared environment =
  projectSynthesisInventory
    [ (functionName binding, functionPenalty binding)
    | tagged <- sourceBindings environment
    , let binding = sourceBindingFunction tagged
    ]
    (sourceDeconstructors environment)
    prepared

ordinaryTypeName :: QualifiedName -> Bool
ordinaryTypeName name = case SharedName.nameSpecial name of
  Nothing -> True
  Just _ -> False

-- | Unique-only compatibility index used by the historical type elaborator.
-- The ordered field remains authoritative so duplicate declarations reach the
-- shared inventory instead of being silently resolved by map insertion order.
sourceTypeSynonymMap :: SourceEnvironment -> TypeDeclMap
sourceTypeSynonymMap = uniqueTypeDeclMap . sourceTypeSynonyms

-- | Seal the complete frontend inventory in the common environment IR.
-- Unlike the search-core t'EnvDictionary' adapter, this boundary retains type
-- synonyms so later validation does not have to rediscover declarations from
-- the HSE modules or a parallel tuple field.
toSynthesisSourceEnvironment
  :: SourceEnvironment
  -> Either SynthesisEnvironmentError SynthesisEnvironment
toSynthesisSourceEnvironment environment =
  SharedInventory.inventoryEnvironment
    <$> toSynthesisSourceInventory environment

-- | Seal and kind-check the frontend inventory while retaining the inferred
-- assumptions needed to elaborate subsequent queries against the same source
-- declarations.
toSynthesisSourceInventory
  :: SourceEnvironment
  -> Either SynthesisEnvironmentError SynthesisInventory
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
          (functionName binding)
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
        | (name, _ : _ : _) <- M.toAscList constructorFunctionGroups
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
        , canonicalType (functionResult binding) /= canonicalType result
            || map canonicalType (functionParameters binding)
                /= map canonicalType parameters
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
      missingFunctions
  if null duplicateFunctions
    then pure ()
    else Left $ DuplicateConstructorFunctionBindings
      duplicateFunctions
  if null orphanConstructors
    then pure ()
    else Left $ OrphanConstructorBindings
      orphanConstructors
  if null mismatchedFunctions
    then pure ()
    else Left $ MismatchedConstructorFunctionBindings
      mismatchedFunctions
  synonyms <- first SynthesisEnvironmentDeclarationError
    $ mapM toSynthesisTypeDeclaration
    $ sourceTypeSynonyms environment
  core <- toSynthesisEnvironmentWithConstructorPenaltiesAndClassMethods
    constructorPenalties classMethods $ EnvDictionary
    valueBindings
    (sourceDeconstructors environment)
    (sourceClasses environment)
  let declarations =
        synonyms ++ SharedEnvironment.environmentDeclarations core
  sealSynthesisInventory declarations
 where
  -- Compatibility callers can construct the shared type through either its
  -- structural arrow/tuple forms or the equivalent saturated constructor
  -- applications. The checked declaration boundary stores only the former,
  -- so constructor reconciliation must compare the same canonical view.
  canonicalType = SharedType.canonicalizeType

sealSynthesisInventory
  :: [SynthesisDeclaration]
  -> Either SynthesisEnvironmentError SynthesisInventory
sealSynthesisInventory declarations =
  case SharedInventory.mkInventory
      SharedKindInference.OpenKindInventory declarations of
    Left (SharedInventory.InvalidInventoryEnvironment failure) ->
      Left $ InvalidSharedEnvironment failure
    Left (SharedInventory.UngroundedInventoryKind impossible) ->
      absurd impossible
    Left (SharedInventory.InvalidInventoryKinds failure) ->
      Left $ InvalidSourceEnvironmentKinds failure
    Right inventory -> Right inventory


-- | Largest boxed tuple constructor materialized in Exference's default
-- search inventory. This is an operational cap, not the shared
-- representational limit: eagerly adding higher arities creates a
-- partial-application branch for each tuple at every non-arrow search goal.
maximumBuiltInTupleArity :: Int
maximumBuiltInTupleArity = 7

-- Constructor functions are the flat search projection of the authoritative
-- datatype records. Defining them here once prevents introduction and
-- elimination capabilities from drifting in shape, multiplicity, or order.
builtInConstructorEnvironment
  :: Either QualifiedNameError
       ([FunctionBinding], [DeconstructorBinding])
builtInConstructorEnvironment = do
  deconstructors <- builtInDeconstructors
  pure (concatMap constructorFunctions deconstructors, deconstructors)
 where
  constructorFunctions deconstructor =
    [ FunctionBinding
        { functionResult = deconstructorInput deconstructor
        , functionName = constructorName constructor
        , functionPenalty = 0
        , functionConstraints = []
        , functionParameters = constructorFields constructor
        }
    | constructor <- deconstructorConstructors deconstructor
    ]

builtInDeconstructors :: Either QualifiedNameError [DeconstructorBinding]
builtInDeconstructors = do
  let listName = SharedName.listName
      consName = SharedName.consName
  unitName <- mkBoxedTupleName 0
  tuples <- mapM tupleDeconstructor [2 .. maximumBuiltInTupleArity]
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
tupleType tupleName arity = SharedType.applyTypeArguments (TypeCons tupleName)
  $ typeVariables arity

-- | Find every source construct whose type/class meaning would be lost by the
-- current extractor. Value definitions, ordinary imports, individual fixity
-- declarations, default declarations, untyped pattern bodies, and benign
-- pragmas deliberately remain outside this scan. A fixity-sensitive type
-- operator chain is different: unless its grouping is explicit, retaining the
-- parser's provisional tree while dropping the declarations could change its
-- meaning.
unsupportedVocabularyOccurrences
  :: [Module SrcSpanInfo]
  -> [UnsupportedVocabularyOccurrence]
unsupportedVocabularyOccurrences = concatMap unsupportedModule
 where
  unsupportedModule modul = case modul of
    HSE.Module _ _ _ imports declarations ->
      concatMap unsupportedImport imports
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

  -- Package identity is not part of the neutral nominal Name vocabulary.
  -- Treating @import "one" M@ as either the loaded source module @M@ or an
  -- unqualified open-world fallback would silently change which declaration
  -- a type denotes. Reject the import even when no declaration happens to use
  -- it, keeping every loader entry point on the same fail-closed boundary.
  unsupportedImport declaration = case HSE.importPkg declaration of
    Just _ -> one PackageQualifiedImport $ HSE.importAnn declaration
    Nothing -> []

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
        ++ concatMap (unsupportedBinder KindedDataBinder)
            (snd $ splitDeclHead rawHead)
        ++ concatMap unsupportedConstructorVocabulary constructors
        ++ concatMap unsupportedDeriving derivings
    -- The parse mode accepts kind signatures (TypeFamilies implies
    -- KindSignatures in HSE), so class, synonym, and explicit instance
    -- binders must cross the same boundary as datatype binders; otherwise
    -- they would only fail later in an extractor with a span-free string.
    HSE.TypeDecl _ rawHead resultType ->
      concatMap (unsupportedBinder KindedSynonymBinder)
        (snd $ splitDeclHead rawHead)
        ++ unsupportedType resultType
    HSE.TypeSig _ _ signature -> unsupportedType signature
    HSE.ForImp _ _ _ _ _ signature -> unsupportedType signature
    HSE.ClassDecl _ context rawHead dependencies declarations ->
      unsupportedMaybeContext context
        ++ concatMap (unsupportedBinder KindedClassBinder)
          (snd $ splitDeclHead rawHead)
        ++ concatMap unsupportedDependency dependencies
        ++ concatMap unsupportedClassDecl (maybe [] id declarations)
    HSE.InstDecl _ overlap rule declarations ->
      maybe [] unsupportedOverlap overlap
        ++ concatMap (unsupportedBinder KindedInstanceBinder)
            (maybe [] (maybe [] id . instRuleVariables) (splitInstRule rule))
        ++ unsupportedInstanceRule rule
        ++ concatMap unsupportedInstanceDecl (maybe [] id declarations)
    _ -> []

  instRuleVariables (variables, _, _, _) = variables

  unsupportedDeriving (HSE.Deriving location _ _) =
    one DerivingClause location

  -- The common inventory has a place for parameter kinds, but Exference's
  -- current type-variable index does not. Reject these declarations before
  -- either projection can observe a differently shaped head.
  unsupportedBinder form binder = case binder of
    HSE.KindedVar location _ _ -> one form location
    HSE.UnkindedVar _ _ -> []

  unsupportedConstructor
      (HSE.QualConDecl location maybeBinders context _) =
    (case maybeBinders of
      Just (_ : _) -> one ExistentialConstructor location
      _ -> [])
      ++ maybe [] (unsupportedContext ConstrainedConstructor) context

  unsupportedConstructorVocabulary constructor =
    unsupportedConstructor constructor
      ++ unsupportedConstructorTypes constructor

  unsupportedConstructorTypes (HSE.QualConDecl _ _ _ declaration) =
    concatMap unsupportedType $ case declaration of
      HSE.ConDecl _ _ fieldTypes -> fieldTypes
      HSE.InfixConDecl _ left _ right -> [left, right]
      HSE.RecDecl _ _ fields ->
        [ fieldType
        | HSE.FieldDecl _ _ fieldType <- fields
        ]

  unsupportedInstanceRule rule = case splitInstRule rule of
    Nothing -> []
    Just (_, context, _, argumentTypes) ->
      unsupportedMaybeContext context
        ++ concatMap unsupportedType argumentTypes

  unsupportedMaybeContext =
    concatMap unsupportedConstraint . contextConstraints

  unsupportedConstraint constraint = case constraint of
    HSE.TypeA _ constraintType -> unsupportedType constraintType
    HSE.IParam _ _ constraintType -> unsupportedType constraintType
    HSE.ParenA _ inner -> unsupportedConstraint inner

  -- HSE represents an operator chain as nested 'TyInfix' nodes. The neutral
  -- inventory does not retain the fixities needed to justify that grouping,
  -- so only a single application or a chain whose nested applications sit
  -- behind explicit 'TyParen' nodes is stable.
  -- Report the maximal unparenthesized chain once, at its complete source
  -- span; resetting at every other type constructor also finds chains nested
  -- in arrows, applications, tuples, fields, and constraints.
  unsupportedType = go False
   where
    go insideChain syntaxType = case syntaxType of
      HSE.TyForall _ _ context body ->
        unsupportedMaybeContext context ++ go False body
      HSE.TyFun _ argument result ->
        go False argument ++ go False result
      HSE.TyTuple _ _ elements -> concatMap (go False) elements
      HSE.TyUnboxedSum _ elements -> concatMap (go False) elements
      HSE.TyList _ element -> go False element
      HSE.TyParArray _ element -> go False element
      HSE.TyApp _ function argument ->
        go False function ++ go False argument
      HSE.TyParen _ inner -> go False inner
      HSE.TyInfix location left _ right ->
        (if not insideChain && any isInfixType [left, right]
          then one UnparenthesizedTypeOperatorChain location
          else [])
          ++ go True left
          ++ go True right
      HSE.TyKind _ inner kind -> go False inner ++ go False kind
      HSE.TyEquals _ left right -> go False left ++ go False right
      HSE.TyBang _ _ _ inner -> go False inner
      _ -> []

    isInfixType typeExpression = case typeExpression of
      HSE.TyInfix{} -> True
      _ -> False

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
    HSE.InsDecl _ wrappedDeclaration -> case wrappedDeclaration of
      HSE.PatSynSig{} -> unsupportedDecl wrappedDeclaration
      _ -> []

  one form location = [unsupportedOccurrence form location]

unsupportedOccurrence
  :: UnsupportedVocabularyForm
  -> SrcSpanInfo
  -> UnsupportedVocabularyOccurrence
unsupportedOccurrence form location = UnsupportedVocabularyOccurrence form
  $ withHaskellSrcSpan sourceSpan
  $ codedDiagnostic Error "EXF_UNSUPPORTED_VOCABULARY"
      $ "unsupported source vocabulary: "
      ++ unsupportedVocabularyDescription form
 where
  sourceSpan = HSE.srcInfoSpan location

unsupportedVocabularyDescription :: UnsupportedVocabularyForm -> String
unsupportedVocabularyDescription form = case form of
  ExplicitExportList -> "explicit module export list"
  PackageQualifiedImport -> "package-qualified import"
  UnparenthesizedTypeOperatorChain ->
    "unparenthesized type-operator chain"
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
  KindedClassBinder -> "explicitly kinded class parameter"
  KindedSynonymBinder -> "explicitly kinded type-synonym parameter"
  KindedInstanceBinder -> "explicitly kinded instance variable"
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
parseModules = runLoader . parseModulesM

-- | Parse already-read module snapshots in caller order. Each path remains
-- the parser filename and therefore the source attached to diagnostics; the
-- text is consumed directly and is never reopened from the filesystem.
parseModuleSources
  :: [(FilePath, String)]
  -> IO (LoadReport SourceEnvironment)
parseModuleSources = runLoader . parseModuleSourcesM

type Loader = WriterT [Diagnostic] IO

-- Every public IO entry point shares this one projection from a loader
-- action to its result and accumulated diagnostics.
runLoader
  :: Loader (Either EnvironmentLoadError value)
  -> IO (LoadReport value)
runLoader action = uncurry LoadReport <$> runWriterT action

parseModulesM
  :: [(ParseMode, FilePath)]
  -> Loader (Either EnvironmentLoadError SourceEnvironment)
parseModulesM = parseModuleInputsM . map toFileInput
 where
  toFileInput (mode, path) = ModuleFileInput mode path

parseModuleSourcesM
  :: [(FilePath, String)]
  -> Loader (Either EnvironmentLoadError SourceEnvironment)
parseModuleSourcesM = parseModuleInputsM . map toSourceInput
 where
  toSourceInput (path, source) = ModuleSourceInput
    (haskellSrcExtsParseMode path) source

data ModuleInput
  = ModuleFileInput ParseMode FilePath
  | ModuleSourceInput ParseMode String

-- Reject duplicate logical modules before any type/class scope is indexed by
-- module name. Headerless Haskell modules all declare @Main@, so they follow
-- the same rule as repeated explicit module headers. Only later occurrences
-- are diagnosed, in caller order, and each diagnostic identifies the first
-- declaration as context.
duplicateModuleDiagnostics
  :: [Module SrcSpanInfo]
  -> [Diagnostic]
duplicateModuleDiagnostics modules = go M.empty occurrences
 where
  occurrences =
    [ (source, HSE.srcInfoSpan location)
    | modul <- modules
    , (HSE.ModuleName location source, _) <-
        maybeToList $ moduleNameAndDecls modul
    ]

  go _ [] = []
  go seen ((source, currentSpan) : remaining) = case
      M.lookup source seen of
    Nothing -> go (M.insert source currentSpan seen) remaining
    Just originalSpan ->
      duplicateDiagnostic source originalSpan currentSpan
        : go seen remaining

  duplicateDiagnostic source originalSpan currentSpan =
    withHaskellSrcSpan currentSpan
      $ contextualDiagnostic
          Error
          "EXF_MODULE_DUPLICATE"
          "duplicate source module"
          ( source ++ " is declared by both "
              ++ HSE.srcSpanFilename originalSpan ++ " and "
              ++ HSE.srcSpanFilename currentSpan
          )

-- The export-surface resolver is a finite fixed-point calculation only for an
-- ordinary acyclic module graph. Reject cycles explicitly instead of returning
-- whichever surface happens to exist after an iteration cutoff; otherwise an
-- unrelated module can change the cutoff parity and therefore source meaning.
-- SOURCE imports remain valid interface edges and are excluded, matching the
-- workspace loader and the bundled environment's boot-style imports.
cyclicModuleImportDiagnostics
  :: [Module SrcSpanInfo]
  -> [Diagnostic]
cyclicModuleImportDiagnostics modules = case
    stableDependencyOrder moduleOrder modulesByName ordinaryImports of
  Right _ -> []
  Left cycleResult -> [locatedCycleDiagnostic cycleResult]
 where
  namedModules =
    [ (source, modul)
    | modul <- modules
    , (HSE.ModuleName _ source, _) <- maybeToList $ moduleNameAndDecls modul
    ]
  moduleOrder = map fst namedModules
  modulesByName = M.fromList namedModules

  ordinaryImports modul = case modul of
    HSE.Module _ _ _ imports _ ->
      [ source
      | declaration <- imports
      , HSE.importPkg declaration == Nothing
      , not $ HSE.importSrc declaration
      , let HSE.ModuleName _ source = HSE.importModule declaration
      ]
    _ -> []

  locatedCycleDiagnostic cycleResult = maybe baseDiagnostic
      (\location -> withHaskellSrcSpan location baseDiagnostic)
      closingImportSpan
   where
    source = dependencyCycleSource cycleResult
    path = NonEmpty.toList $ dependencyCyclePath cycleResult
    target = case path of
      firstName : _ -> firstName
      [] -> source
    closingImportSpan = case M.lookup source modulesByName of
      Just (HSE.Module _ _ _ imports _) -> case
          [ HSE.srcInfoSpan $ HSE.importAnn declaration
          | declaration <- imports
          , HSE.importPkg declaration == Nothing
          , not $ HSE.importSrc declaration
          , let HSE.ModuleName _ imported = HSE.importModule declaration
          , imported == target
          ] of
        location : _ -> Just location
        [] -> Nothing
      _ -> Nothing
    baseDiagnostic = contextualDiagnostic
      Error
      "EXF_MODULE_CYCLE"
      "cyclic non-SOURCE module imports"
      $ intercalate " -> " path

-- File-backed and in-memory entry points converge before parsing, so source
-- extraction, warning order, checked lowering, and sealing cannot drift.
parseModuleInputsM
  :: [ModuleInput]
  -> Loader (Either EnvironmentLoadError SourceEnvironment)
parseModuleInputsM inputs = do
  readResults <- lift $ mapM hRead inputs
  case NonEmpty.nonEmpty $ lefts readResults of
    Just errors -> pure $ Left $ ModuleReadErrors errors
    Nothing -> parseLoadedModules $ rights readResults
  where
    hRead
      :: ModuleInput
      -> IO (Either Diagnostic (ParseMode, String))
    hRead input = case input of
      ModuleFileInput mode path -> fmap ((,) mode) <$> readTextFile path
      ModuleSourceInput mode source -> do
        strictSource <- evaluate $ force source
        pure $ Right (mode, strictSource)

    hParse :: (ParseMode, String) -> Either Diagnostic (Module SrcSpanInfo)
    -- Unlike the lower-level module parser, the file-content parser enables
    -- LANGUAGE pragmas found in the supplied text without reopening its path.
    hParse (mode, content) = case parseFileContentsWithMode mode content of
      ParseFailed location detail -> Left $ moduleParseDiagnostic location detail
      ParseOk modul -> Right modul

    moduleParseDiagnostic :: HSE.SrcLoc -> String -> Diagnostic
    moduleParseDiagnostic location detail =
      withHaskellSrcLocation location
        $ contextualDiagnostic
            Error
            "EXF_MODULE_PARSE"
            "could not parse a Haskell source module"
            detail

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

      case NonEmpty.nonEmpty $ duplicateModuleDiagnostics modules of
        Just errors -> throwE $ DuplicateModuleDeclarations errors
        Nothing -> pure ()

      case NonEmpty.nonEmpty $ unsupportedVocabularyOccurrences modules of
        Just occurrences ->
          throwE $ UnsupportedSourceVocabulary occurrences
        Nothing -> pure ()

      case NonEmpty.nonEmpty $ cyclicModuleImportDiagnostics modules of
        Just errors -> throwE $ CyclicModuleImports errors
        Nothing -> pure ()

      dataTypes <- case getDataTypesLocated modules of
        Left conversionError -> throwE $ DataTypeNameError
          $ mapExtractionMessage
              ("could not extract data-type names: " ++) conversionError
        Right result -> pure result

      let classArities = sourceClassArities modules
          resolvers = sourceTypeResolvers dataTypes classArities
            $ zip (map fst rawTuples) modules
          globalResolver = TypeResolver
            { resolverTypeNames = dataTypes
            , resolverClassArities = classArities
            , resolverUnqualifiedTypeNames = dataTypes
            , resolverUnqualifiedClassNames = M.keys classArities
            , resolverModuleAliases = []
            , resolverQualifiedNames = Nothing
            }
          resolverFor (HSE.ModuleName _ source) =
            M.findWithDefault globalResolver source resolvers

      typeDeclarationResults <- lift
        $ getTypeDeclsLocatedWithResolvers resolverFor modules
      let typeDeclarationErrors = lefts typeDeclarationResults
      case NonEmpty.nonEmpty typeDeclarationErrors of
        Just errors -> throwE $ TypeDeclarationErrors errors
        Nothing -> pure ()
      let typeDeclarations = rights typeDeclarationResults

      -- Keep every operational source type unexpanded until the common
      -- Inventory has checked the original applications.  Passing the empty
      -- compatibility map still performs all HSE name/shape conversion; the
      -- checked backend projection expands the retained synonym declarations
      -- later through 'prepareSourceSynthesisInventory'.
      classResult <- lift
        $ loadClassEnvironmentSourcedWithResolvers
            (\_ -> resolverFor) M.empty modules
      (loadedClasses, methodsByModule) <- either
        (throwE . ClassEnvironmentLoadFailure)
        pure
        classResult
      let classEnvironment = loadedStaticClassEnvironment loadedClasses
          instanceCount = loadedSourceInstanceCount loadedClasses

      extracted <- lift $ sequence
        [ hExtractBinds
            (resolverFor moduleName)
            M.empty
            modul
            methodResults
        | (modul, methodResults) <- zip modules methodsByModule
        , (moduleName, _) <- maybeToList $ moduleNameAndDecls modul
        ]
      let (bindingLists, deconstructorLists, errorLists) = unzip3 extracted
          declarations = concat bindingLists
          deconstructors = concat deconstructorLists
          bindingErrors = concat errorLists
      case NonEmpty.nonEmpty bindingErrors of
        Just errors -> throwE $ BindingDeclarationErrors errors
        Nothing -> pure ()

      (builtInDeclarations, builtInDeconstructorValues) <-
        either
          (failBuiltIns . NonEmpty.singleton
            . ("could not construct the built-in constructor environment: " ++)
            . show)
          pure
          builtInConstructorEnvironment

      let classes = sClassEnv_tclasses classEnvironment
          instances = sClassEnv_instances classEnvironment
          allValidNames = dataTypes ++ M.keys classes
          classCount = SharedCount.naturalLength classes
          inflatedInstanceCount = sum
            $ map SharedCount.naturalLength
            $ M.elems instances
          functionDeclarationCount =
            SharedCount.naturalLength declarations

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
            "invalid class constraint '" ++ showHsConstraint mempty constraint
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
        tell [infoDiagnostic $ "got " ++ show classCount ++ " classes"]
        tell [infoDiagnostic $ "and " ++ show instanceCount ++ " instances"]
        tell
          [ infoDiagnostic
              $ "(-> " ++ show inflatedInstanceCount
                ++ " instances after inflation)"
          ]
        tell
          [ infoDiagnostic
              $ "and " ++ show functionDeclarationCount ++ " function decls"
          ]

      pure SourceEnvironment
        { sourceBindings = map SourceFunction builtInDeclarations ++ declarations
        , sourceDeconstructors = builtInDeconstructorValues ++ deconstructors
        , sourceClasses = classEnvironment
        , sourceTypeNames = allValidNames
        , sourceTypeSynonyms = typeDeclarations
        }

    failBuiltIns = throwE . BuiltInEnvironmentErrors

    hExtractBinds :: TypeResolver
                  -> TypeDeclMap
                  -> Module SrcSpanInfo
                  -> [SourcedExtraction [ClassMethodDeclaration]]
                  -> Loader
                       ( [SourceBinding]
                       , [DeconstructorBinding]
                       , [ExtractionError]
                       )
    hExtractBinds resolver tDeclMap modul methodResults = do
      fromData <- getDataConssSourcedWithResolver
        resolver tDeclMap modul
      declarations <- getDeclsSourcedWithResolver
        resolver tDeclMap modul
      let ordered = sortOn orderedBindingSlot
            $ map dataBindingExtraction fromData
            ++ map ordinaryBindingExtraction declarations
            ++ map methodBindingExtraction methodResults
      pure
        ( concatMap orderedSourceBindings ordered
        , concatMap orderedDeconstructors ordered
        , concatMap orderedBindingErrors ordered
        )

data OrderedBindingExtraction = OrderedBindingExtraction
  { orderedBindingSlot :: !SourceSlot
  , orderedSourceBindings :: [SourceBinding]
  , orderedDeconstructors :: [DeconstructorBinding]
  , orderedBindingErrors :: [ExtractionError]
  }

dataBindingExtraction
  :: SourcedExtraction ([FunctionBinding], DeconstructorBinding)
  -> OrderedBindingExtraction
dataBindingExtraction (SourcedExtraction slot result) = case result of
  Left failure -> OrderedBindingExtraction slot [] [] [failure]
  Right (bindings, deconstructor) -> OrderedBindingExtraction slot
    (map SourceFunction bindings) [deconstructor] []

ordinaryBindingExtraction
  :: SourcedExtraction [FunctionBinding]
  -> OrderedBindingExtraction
ordinaryBindingExtraction (SourcedExtraction slot result) = case result of
  Left failure -> OrderedBindingExtraction slot [] [] [failure]
  Right bindings -> OrderedBindingExtraction slot
    (map SourceFunction bindings) [] []

methodBindingExtraction
  :: SourcedExtraction [ClassMethodDeclaration]
  -> OrderedBindingExtraction
methodBindingExtraction (SourcedExtraction slot result) = case result of
  Left failure -> OrderedBindingExtraction slot [] [] [failure]
  Right methods -> OrderedBindingExtraction slot
    [ SourceClassMethod owner binding
    | ClassMethodDeclaration owner binding <- methods
    ] [] []

parseRatings :: String -> Either Diagnostic [(QualifiedName, Penalty)]
parseRatings = go . words
  where
    go [] = Right []
    go [_] = Left $ diagnostic Error
      "rating file ends with a name but no numeric rating"
    go (name : value : rest) = case readMaybe value :: Maybe Double of
      Nothing -> Left $ diagnostic Error
        $ "invalid rating for " ++ name ++ ": " ++ value
      Just rating | isNaN rating || isInfinite rating ->
        Left $ diagnostic Error
          $ "rating for " ++ name ++ " must be finite: " ++ value
      Just rating -> do
        qualifiedName <- parseQualifiedName name
        ((qualifiedName, Penalty rating) :) <$> go rest

ratingsFromFile :: String -> IO (Either Diagnostic [(QualifiedName, Penalty)])
ratingsFromFile path = do
  contents <- readTextFile path
  pure $ contents >>= first (withSource path) . parseRatings

ratingsFromSource
  :: (FilePath, String)
  -> Either Diagnostic [(QualifiedName, Penalty)]
ratingsFromSource (path, source) = first (withSource path) $ parseRatings source

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
  declarations = SharedCollection.summarizeDuplicates
    [ functionName binding
    | binding <- sourceFunctions environment
    ]
  ratingsByName = M.fromListWith (++)
    [ (name, [rating])
    | (name, rating) <- ratings
    ]
  ratingResults = map validateRating $ M.toAscList ratingsByName
  effectiveRatings = M.fromList $ rights ratingResults

  validateRating (name, values) =
    case SharedCollection.multiplicityOf name declarations of
      SharedCollection.NotPresent -> Left $ warningDiagnostic
        $ "rating could not be applied: " ++ show name
      SharedCollection.OccursMultipleTimes -> Left $ warningDiagnostic
        $ "duplicate function: " ++ show name
      SharedCollection.OccursOnce -> case values of
        [rating] -> Right (name, rating)
        _ -> Left $ warningDiagnostic
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
environmentFromModule = runLoader . environmentFromModuleM

environmentFromModuleM
  :: FilePath
  -> Loader (Either EnvironmentLoadError CheckedSourceEnvironment)
environmentFromModuleM modulePath = environmentFromFilesM [modulePath] []

environmentFromModuleAndRatings
  :: FilePath
  -> FilePath
  -> IO (LoadReport CheckedSourceEnvironment)
environmentFromModuleAndRatings modulePath ratingPath = runLoader
  $ environmentFromModuleAndRatingsM modulePath ratingPath

environmentFromModuleAndRatingsM
  :: FilePath
  -> FilePath
  -> Loader (Either EnvironmentLoadError CheckedSourceEnvironment)
environmentFromModuleAndRatingsM modulePath ratingPath =
  environmentFromFilesM [modulePath] [ratingPath]

-- | Load and seal an explicitly ordered collection of Haskell modules and
-- rating files. Module read, parse, and extraction diagnostics retain the
-- supplied module order; rating-file IO diagnostics retain the supplied
-- rating order. An empty module collection is valid and produces the checked
-- built-in constructor inventory.
--
-- This is the file-oriented foundation used by interactive module loading.
-- It deliberately accepts paths rather than discovering dependencies: callers
-- that own a module graph can resolve it once and pass its deterministic order
-- here without copying sources into a temporary directory.
environmentFromFiles
  :: [FilePath]
  -> [FilePath]
  -> IO (LoadReport CheckedSourceEnvironment)
environmentFromFiles modulePaths ratingPaths = runLoader
  $ environmentFromFilesM modulePaths ratingPaths

environmentFromFilesM
  :: [FilePath]
  -> [FilePath]
  -> Loader (Either EnvironmentLoadError CheckedSourceEnvironment)
environmentFromFilesM modulePaths ratingPaths = do
  environmentResult <- parseModulesM
    [ (haskellSrcExtsParseMode modulePath, modulePath)
    | modulePath <- modulePaths
    ]
  finishEnvironmentLoad environmentResult
    $ lift $ mapM ratingsFromFile ratingPaths

-- | Parse, rate, check, and seal exact in-memory source snapshots. Paths are
-- retained solely as diagnostic/parser identities; neither modules nor
-- ratings are reopened. This is the single-snapshot boundary used by the
-- module-aware REPL.
environmentFromSources
  :: [(FilePath, String)]
  -> [(FilePath, String)]
  -> IO (LoadReport CheckedSourceEnvironment)
environmentFromSources moduleSources ratingSources = runLoader
  $ environmentFromSourcesM moduleSources ratingSources

environmentFromSourcesM
  :: [(FilePath, String)]
  -> [(FilePath, String)]
  -> Loader (Either EnvironmentLoadError CheckedSourceEnvironment)
environmentFromSourcesM moduleSources ratingSources = do
  environmentResult <- parseModuleSourcesM moduleSources
  finishEnvironmentLoad environmentResult
    $ pure $ map ratingsFromSource ratingSources

finishEnvironmentLoad
  :: Either EnvironmentLoadError SourceEnvironment
  -> Loader [Either Diagnostic [(QualifiedName, Penalty)]]
  -> Loader (Either EnvironmentLoadError CheckedSourceEnvironment)
finishEnvironmentLoad environmentResult loadRatings = case environmentResult of
  Left failure -> pure $ Left failure
  Right environment -> do
    ratingResults <- loadRatings
    forM_ (lefts ratingResults)
      $ tell . (: []) . ratingFailureDiagnostic
    rateAndCheckEnvironment (concat $ rights ratingResults) environment


environmentFromPath
  :: FilePath
  -> IO (LoadReport CheckedSourceEnvironment)
environmentFromPath = runLoader . environmentFromPathM

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
      environmentFromFilesM modules ratingPaths

rateAndCheckEnvironment
  :: [(QualifiedName, Penalty)]
  -> SourceEnvironment
  -> Loader (Either EnvironmentLoadError CheckedSourceEnvironment)
rateAndCheckEnvironment ratings environment = do
  let (ratedEnvironment, warnings) = applyRatings ratings environment
  tell warnings
  pure $ checkSourceEnvironment ratedEnvironment
