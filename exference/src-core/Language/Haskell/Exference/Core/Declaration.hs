{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

-- | Checked adapters between Exference's search-oriented declaration records
-- and the shared source-declaration vocabulary.
module Language.Haskell.Exference.Core.Declaration
  ( DeclarationMetadata (..)
  , SynthesisDeclaration
  , SynthesisEnvironment
  , SynthesisInventory
  , PreparedSynthesisInventory
  , SynthesisDeclarationError (..)
  , SynthesisEnvironmentError (..)
  , freshSynthesisVariable
  , prepareSynthesisInventory
  , prepareSourceSynthesisInventory
  , projectSynthesisInventory
  , preparedSynthesisWitness
  , preparedSynthesisBackend
  , erasePreparedSynthesisAnnotations
  , deriveRecursiveDataMetadata
  , toSynthesisTypeSynonym
  , fromSynthesisTypeSynonym
  , toSynthesisFunctionBinding
  , fromSynthesisFunctionBinding
  , toSynthesisClassDeclaration
  , toSynthesisClassDeclarationWithMethods
  , fromSynthesisClassDeclaration
  , fromSynthesisClassDeclarationWithMethods
  , classMethodConstraint
  , addClassMethodConstraint
  , toSynthesisInstanceDeclaration
  , fromSynthesisInstanceDeclaration
  , toSynthesisDataDeclaration
  , fromSynthesisDataDeclaration
  , toSynthesisRatedDataDeclaration
  , fromSynthesisRatedDataDeclaration
  , toSynthesisEnvironment
  , toSynthesisEnvironmentWithConstructorPenaltiesAndClassMethods
  , fromSynthesisEnvironment
  , fromSynthesisEnvironmentWithClassMethods
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.List (find, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Language.Haskell.Synthesis.Collection as SharedCollection
import qualified Language.Haskell.Synthesis.Declaration as SharedDeclaration
import qualified Language.Haskell.Synthesis.Environment as SharedEnvironment
import qualified Language.Haskell.Synthesis.KindInference as SharedKindInference
import qualified Language.Haskell.Synthesis.Inventory as SharedInventory
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Type as SharedType
import qualified Language.Haskell.Synthesis.TypeSynonym as SharedTypeSynonym

import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.Core.Internal.VariableSupply
  ( freshSynthesisVariable
  , synthesisIdentifierNamespace
  )
import Language.Haskell.Exference.Core.Name
import Language.Haskell.Exference.Core.Score
  ( Penalty
  , isFiniteScore
  )
import Language.Haskell.Exference.Core.Types

data DeclarationMetadata
  = NoDeclarationMetadata
  | SearchPenaltyMetadata Penalty
  | RecursiveDataMetadata Bool
  deriving (Eq, Show, Generic)

instance NFData DeclarationMetadata

type SynthesisDeclaration = SharedDeclaration.Declaration
  SynthesisVariable Void DeclarationMetadata

type SynthesisEnvironment = SharedEnvironment.Environment
  SynthesisVariable Void DeclarationMetadata

type SynthesisInventory = SharedInventory.Inventory
  SynthesisVariable DeclarationMetadata

-- | One checked inventory and the exact synonym table and backend lowering
-- prepared from it. Keeping the constructor private prevents frontends from
-- pairing a checked inventory with an unrelated search dictionary. The
-- annotation parameter lets a source frontend retain presentation metadata
-- without creating a second semantic inventory.
data PreparedSynthesisInventory annotation = PreparedSynthesisInventory
  (SharedTypeSynonym.PreparedInventory SynthesisVariable annotation)
  EnvDictionary

-- | Per-declaration conversion failures. Whole-environment, inventory, and
-- projection failures live in 'SynthesisEnvironmentError', which nests this
-- vocabulary exactly as Djinn's environment adapter nests its declaration
-- errors.
data SynthesisDeclarationError
  = DeclarationTypeConversionError SynthesisTypeError
  | InvalidSharedDeclaration
      (SharedDeclaration.DeclarationError SynthesisVariable)
  | ExpectedValueDeclaration
  | ExpectedClassDeclaration
  | ExpectedInstanceDeclaration
  | ExpectedDataDeclaration
  | ExpectedTypeSynonymDeclaration
  | MissingSearchPenaltyMetadata SharedName.Name
  | MissingConstructorPenalty QualifiedName
  | MissingRecursiveDataMetadata
  | ExplicitFunctionForallUnsupported [TVarId]
  | NonImplicitInstanceForall [SynthesisVariable]
  | ClassMethodsUnsupported [SharedName.Name]
  | MissingClassMethodConstraint QualifiedName
  | MismatchedClassMethodConstraint
      QualifiedName HsConstraint HsConstraint
  | ExplicitParameterKindUnsupported SynthesisVariable
  | InvalidDeconstructorHead HsType
  | NonVariableDataParameter HsType
  | RigidDataParameter TVarId
  | DeconstructorForallMismatch [TVarId] [TVarId]
  | VariableNamespaceExhausted SynthesisVariable
  deriving (Eq, Show)

-- | Whole-environment, inventory, and projection failures. Declaration-level
-- failures cross into environment operations through exactly one
-- 'SynthesisEnvironmentDeclarationError' boundary per operation, so failure
-- precedence within each phase is unchanged by the split.
data SynthesisEnvironmentError
  = SynthesisEnvironmentDeclarationError SynthesisDeclarationError
  | UnknownClassMethodOwner QualifiedName
  | PreparedBindingNamesMismatch
      [QualifiedName] -- ^ Ordered source multiset.
      [QualifiedName] -- ^ Ordered prepared-backend multiset.
  | PreparedDataTypeNamesMismatch
      [QualifiedName] -- ^ Ordered source multiset.
      [QualifiedName] -- ^ Ordered prepared-backend multiset.
  | InvalidPreparedBindingPenalty QualifiedName Penalty
  | PreparedDataMetadataMissing SharedName.Name
  | InvalidSharedEnvironment
      (SharedEnvironment.EnvironmentError SynthesisVariable)
  | ClassEnvironmentConversionError ClassEnvError
  | UnsupportedCoreEnvironmentDeclaration SharedName.Name
  | MissingConstructorFunctionBindings [SharedName.Name]
  | DuplicateConstructorFunctionBindings [SharedName.Name]
  | OrphanConstructorBindings [SharedName.Name]
  | MismatchedConstructorFunctionBindings [SharedName.Name]
  | InvalidSourceEnvironmentKinds
      (SharedKindInference.KindInferenceError SynthesisVariable)
  | SynonymExpansionError
      Int -- ^ Zero-based source declaration index.
      SharedName.Name -- ^ Declaration name or instance class.
      (SharedTypeSynonym.SynonymExpansionError SynthesisVariable)
  | SynonymPreparationError
      (SharedTypeSynonym.SynonymExpansionError SynthesisVariable)
  deriving (Eq, Show)

-- | Prepare an already checked shared inventory for Exference. Type aliases
-- and explicit kinds remain authoritative in that inventory; annotations are
-- retained in the shared witness but never participate in backend lowering.
prepareSynthesisInventory
  :: SharedInventory.Inventory SynthesisVariable annotation
  -> Either SynthesisEnvironmentError
      (PreparedSynthesisInventory annotation)
prepareSynthesisInventory inventory = do
  expansion <- first promoteExpansionError
    $ SharedTypeSynonym.prepareInventoryExpansion
        freshSynthesisVariable inventory
  backend <- prepareSearchEnvironment expansion
  let prepared =
        SharedTypeSynonym.inventoryExpansionPreparedInventory expansion
  -- Evaluate the projection before storing it. Otherwise its thunk would keep
  -- the transient expanded declarations alive through this long-lived wrapper.
  prepared `seq` pure (PreparedSynthesisInventory prepared backend)
 where
  promoteExpansionError failure = case failure of
    SharedTypeSynonym.InventorySynonymPreparationError cause ->
      SynonymPreparationError cause
    SharedTypeSynonym.InventoryDeclarationExpansionError
        index subject cause ->
      SynonymExpansionError index subject cause

-- | Prepare the metadata-bearing Inventory produced by Exference's source
-- compatibility frontend. Alias-aware recursion is foundation-derived and
-- then copied into the retained annotations so historical source projections
-- see the same classification as search.
prepareSourceSynthesisInventory
  :: SynthesisInventory
  -> Either SynthesisEnvironmentError
      (PreparedSynthesisInventory DeclarationMetadata)
prepareSourceSynthesisInventory inventory =
  prepareSynthesisInventory inventory >>= normalizePreparedDataMetadata

-- | Reorder a canonical backend to match a source frontend and attach its
-- finite heuristic ratings. Function names and deconstructor heads form an
-- exact inventory: callers may choose order and ratings, but the supplied
-- records cannot replace types, constraints, classes, instances,
-- constructors, or datatype metadata from the prepared witness.
projectSynthesisInventory
  :: [(QualifiedName, Penalty)]
  -> [DeconstructorBinding]
  -> PreparedSynthesisInventory annotation
  -> Either SynthesisEnvironmentError
      (PreparedSynthesisInventory annotation)
projectSynthesisInventory functionProjection dataProjection
    (PreparedSynthesisInventory prepared backend) = do
  let preparedFunctions = environmentFunctions backend
      sourceBindingNames = sort $ map fst functionProjection
      preparedBindingNames = sort $ map functionName preparedFunctions
  if sourceBindingNames == preparedBindingNames
    then pure ()
    else Left $ PreparedBindingNamesMismatch
      sourceBindingNames preparedBindingNames
  sourceDataNames <- first SynthesisEnvironmentDeclarationError
    $ mapM deconstructorTypeName dataProjection
  preparedDataNames <- first SynthesisEnvironmentDeclarationError
    $ mapM deconstructorTypeName
    $ environmentDeconstructors backend
  let sortedSourceDataNames = sort sourceDataNames
      canonicalDataNames = sort preparedDataNames
  if sortedSourceDataNames == canonicalDataNames
    then pure ()
    else Left $ PreparedDataTypeNamesMismatch
      sortedSourceDataNames canonicalDataNames
  case find (not . isFiniteScore . snd) functionProjection of
    Just (name, penalty) -> Left $ InvalidPreparedBindingPenalty name penalty
    Nothing -> pure ()
  let functionsByName = Map.fromList
        [ (functionName binding, binding)
        | binding <- preparedFunctions
        ]
      deconstructorsByName = Map.fromList
        $ zip preparedDataNames $ environmentDeconstructors backend
      projectFunction (name, penalty) = case Map.lookup name functionsByName of
        Just binding -> Right binding {functionPenalty = penalty}
        -- Exact multiset validation above makes this branch unreachable, but
        -- retaining a total lookup keeps the invariant local to this function.
        Nothing -> Left $ PreparedBindingNamesMismatch
          sourceBindingNames preparedBindingNames
      projectDeconstructor name = maybe
        (Left $ PreparedDataTypeNamesMismatch
          sortedSourceDataNames canonicalDataNames)
        Right
        $ Map.lookup name deconstructorsByName
  functions <- mapM projectFunction functionProjection
  deconstructors <- mapM projectDeconstructor sourceDataNames
  pure $ PreparedSynthesisInventory prepared
    (backend
      { environmentFunctions = functions
      , environmentDeconstructors = deconstructors
      })

-- | The shared inventory/alias witness retained after the backend projection
-- has been consumed. The foundation's 'SharedTypeSynonym.preparedInventory'
-- and 'SharedTypeSynonym.preparedTypeSynonyms' are the sole projections from
-- that witness, while its prepared operations let callers use the witness
-- without exposing or independently pairing its table. This wrapper does not
-- duplicate either projection under backend-specific names. Session sealing
-- uses the witness so the complete unfiltered search dictionary cannot remain
-- live beside its filtered view.
preparedSynthesisWitness
  :: PreparedSynthesisInventory annotation
  -> SharedTypeSynonym.PreparedInventory SynthesisVariable annotation
preparedSynthesisWitness (PreparedSynthesisInventory prepared _) = prepared

-- | The canonical or safely reordered/rated backend owned by the witness.
preparedSynthesisBackend
  :: PreparedSynthesisInventory annotation
  -> EnvDictionary
preparedSynthesisBackend
    (PreparedSynthesisInventory _ backend) = backend

-- | Erase frontend annotations without rebuilding the inventory, synonym
-- table, or projected backend. The resulting witness has exactly the same
-- semantic declarations and kind assumptions.
erasePreparedSynthesisAnnotations
  :: PreparedSynthesisInventory annotation
  -> PreparedSynthesisInventory ()
erasePreparedSynthesisAnnotations
    (PreparedSynthesisInventory prepared backend) =
  PreparedSynthesisInventory (fmap (const ()) prepared) backend

-- Attach alias-aware recursion flags derived by the canonical core lowerer to
-- the opaque prepared inventory. Every concrete datatype must have one backend
-- deconstructor; abstract types deliberately have none.
normalizePreparedDataMetadata
  :: PreparedSynthesisInventory DeclarationMetadata
  -> Either SynthesisEnvironmentError
      (PreparedSynthesisInventory DeclarationMetadata)
normalizePreparedDataMetadata
    (PreparedSynthesisInventory prepared backend) = do
  metadata <- Map.fromList <$> mapM entry
    (environmentDeconstructors backend)
  mapM_ (requireMetadata metadata)
    $ SharedEnvironment.environmentDeclarations
    $ SharedInventory.inventoryEnvironment
    $ SharedTypeSynonym.preparedInventory prepared
  let adjusted =
        SharedTypeSynonym.adjustPreparedInventoryDataTypeAnnotations
          (attachMetadata metadata) prepared
  pure $ PreparedSynthesisInventory adjusted backend
 where
  entry deconstructor = do
    name <- first SynthesisEnvironmentDeclarationError
      $ deconstructorTypeName deconstructor
    pure (name, deconstructorRecursive deconstructor)

  requireMetadata metadata declaration = case declaration of
    SharedDeclaration.DataTypeDeclaration _ name _ _ ->
      case Map.lookup name metadata of
        Nothing -> Left $ PreparedDataMetadataMissing name
        Just _ -> Right ()
    _ -> Right ()

  -- 'requireMetadata' has checked every datatype in source order, so the
  -- default cannot be observed for a well-formed prepared witness.
  attachMetadata metadata name _ = RecursiveDataMetadata
    $ Map.findWithDefault False name metadata

deconstructorTypeName
  :: DeconstructorBinding
  -> Either SynthesisDeclarationError QualifiedName
deconstructorTypeName declaration = fst
  <$> deconstructorHead (deconstructorInput declaration)

-- Keep the actual lowering private once the shared transient witness has been
-- assembled. Variable-ID normalization and Exference's recursive-elimination
-- policy are backend concerns; alias expansion, declaration attribution, and
-- the recursive graph are already fixed by that witness.
prepareSearchEnvironment
  :: SharedTypeSynonym.PreparedInventoryExpansion
      SynthesisVariable annotation
  -> Either SynthesisEnvironmentError EnvDictionary
prepareSearchEnvironment expansion = do
  -- Renaming variables and erasing annotations cannot change a nominal
  -- datatype edge, so the shared pre-normalization SCC set is exact here.
  let normalized = map
        (normalizeDeclarationVariables . fmap (const ()))
        $ SharedTypeSynonym.inventoryExpansionDeclarations expansion
      recursiveNames =
        SharedTypeSynonym.inventoryExpansionRecursiveDataTypeNames expansion
  prepared <- first SynthesisEnvironmentDeclarationError
    $ mapM (prepareSearchDeclaration recursiveNames) normalized
  fmap fst $ lowerSynthesisDeclarations IncludeDerivedBindings
    fromSynthesisClassDeclarationWithMethods
    [declaration | Just declaration <- prepared]

type SearchSynthesisDeclaration = SharedDeclaration.Declaration
  SynthesisVariable Void ()

-- Variable identities are local to a source declaration. Repacking flexible
-- IDs makes negative class parameters acceptable to the historical core and
-- gives every method in a class the same coherent owner namespace. Parameters
-- are visited first, followed by the declaration's remaining source order.
normalizeDeclarationVariables
  :: SearchSynthesisDeclaration
  -> SearchSynthesisDeclaration
normalizeDeclarationVariables declaration =
  SharedDeclaration.mapDeclarationTypeVariables renameVariable declaration
 where
  flexibleVariables = SharedCollection.distinctOn id
    [ variable
    | variable@SharedType.FlexibleVariable{} <-
        SharedDeclaration.declarationTypeVariables declaration
    ]
  replacements = Map.fromList $ zip flexibleVariables
    (map SharedType.FlexibleVariable synthesisIdentifierNamespace)

  renameVariable variable = Map.findWithDefault variable variable replacements

prepareSearchDeclaration
  :: Set.Set SharedName.Name
  -> SearchSynthesisDeclaration
  -> Either SynthesisDeclarationError (Maybe SynthesisDeclaration)
prepareSearchDeclaration recursiveNames declaration = case declaration of
  SharedDeclaration.TypeSynonymDeclaration{} -> Right Nothing
  SharedDeclaration.AbstractTypeDeclaration{} -> Right Nothing
  SharedDeclaration.ValueDeclaration signature -> do
    functionType <- implicitizeExferenceForalls Set.empty
      $ SharedDeclaration.valueType signature
    pure $ Just $ SharedDeclaration.ValueDeclaration
      $ SharedDeclaration.ValueSignature (SearchPenaltyMetadata 0)
          (SharedDeclaration.valueName signature) functionType
  SharedDeclaration.DataTypeDeclaration _ name parameters constructors ->
    pure $ Just $ SharedDeclaration.DataTypeDeclaration
      (RecursiveDataMetadata $ name `Set.member` recursiveNames)
      name (map eraseParameterKind parameters)
      [ SharedDeclaration.DataConstructor (SearchPenaltyMetadata 0)
          (SharedDeclaration.constructorName constructor)
          (SharedDeclaration.constructorFields constructor)
      | constructor <- constructors
      ]
  SharedDeclaration.ClassDeclaration _ name parameters superclasses methods -> do
    let ownerVariables = Set.fromList
          $ map SharedDeclaration.parameterVariable parameters
    preparedMethods <- mapM (prepareMethod ownerVariables) methods
    pure $ Just $ SharedDeclaration.ClassDeclaration NoDeclarationMetadata
      name (map eraseParameterKind parameters) superclasses preparedMethods
  SharedDeclaration.InstanceDeclaration _ _ prerequisites headConstraint ->
    pure $ Just $ SharedDeclaration.InstanceDeclaration NoDeclarationMetadata
      (Set.toAscList $ instanceConstraintVariables $ headConstraint : prerequisites)
      prerequisites headConstraint
 where
  eraseParameterKind parameter = SharedDeclaration.TypeParameter
    (SharedDeclaration.parameterVariable parameter) Nothing

  prepareMethod ownerVariables signature = do
    methodType <- implicitizeExferenceForalls ownerVariables
      $ SharedDeclaration.valueType signature
    pure $ SharedDeclaration.ValueSignature (SearchPenaltyMetadata 0)
      (SharedDeclaration.valueName signature) methodType

-- Exference quantifies every free flexible binding variable implicitly. The
-- shared operation owns prenex traversal, capture avoidance, and context
-- order; this adapter supplies only the flexible-binder policy, finite tagged
-- allocator, and historical error vocabulary. A forall below another type
-- boundary remains visible to the existing rank-N omission boundary.
implicitizeExferenceForalls
  :: Set.Set SynthesisVariable
  -> SharedType.Type SynthesisVariable
  -> Either SynthesisDeclarationError
      (SharedType.Type SynthesisVariable)
implicitizeExferenceForalls outerVariables source =
  first lowerError $ fmap fst $ SharedType.implicitizeLeadingForalls
    rejectRigidBinder freshSynthesisVariable outerVariables source
 where
  rejectRigidBinder = SharedType.rigidVariableIdentity

  lowerError failure = case failure of
    SharedType.RejectedTypeBinder identifier ->
      DeclarationTypeConversionError $ RigidForallBinder identifier
    SharedType.DuplicateTypeBinder variable ->
      DeclarationTypeConversionError $ InvalidSynthesisType
        $ SharedType.DuplicateForallVariable variable
    SharedType.TypeBinderFresheningError fresheningFailure ->
      VariableNamespaceExhausted $ failedBinder fresheningFailure

  failedBinder fresheningFailure = case fresheningFailure of
    SharedType.FreshVariableSupplyExhausted binder -> binder
    SharedType.FreshVariableAlreadyReserved binder _ -> binder

toSynthesisFunctionBinding
  :: FunctionBinding
  -> Either SynthesisDeclarationError SynthesisDeclaration
toSynthesisFunctionBinding binding = checked $
  SharedDeclaration.ValueDeclaration <$> valueSignature binding

valueSignature
  :: FunctionBinding
  -> Either SynthesisDeclarationError
      (SharedDeclaration.ValueSignature
        SynthesisVariable DeclarationMetadata)
valueSignature binding = SharedDeclaration.ValueSignature
  (SearchPenaltyMetadata $ functionPenalty binding)
  (functionName binding)
  <$> checkedType (functionBindingSignature binding)

fromSynthesisFunctionBinding
  :: SynthesisDeclaration
  -> Either SynthesisDeclarationError FunctionBinding
fromSynthesisFunctionBinding declaration = do
  validateShared declaration
  case declaration of
    SharedDeclaration.ValueDeclaration signature -> do
      penalty <- case SharedDeclaration.valueAnnotation signature of
        SearchPenaltyMetadata value -> Right value
        _ -> Left $ MissingSearchPenaltyMetadata
          $ SharedDeclaration.valueName signature
      let name = SharedDeclaration.valueName signature
      functionType <- checkedType $ SharedDeclaration.valueType signature
      let (variables, constraints, body) = case functionType of
            TypeForall binders context nested -> (binders, context, nested)
            nested -> ([], [], nested)
      if null variables
        then let (parameters, result) = SharedType.functionSpine body
             in Right $ FunctionBinding result name penalty constraints parameters
        else Left $ ExplicitFunctionForallUnsupported variables
    _ -> Left ExpectedValueDeclaration

toSynthesisClassDeclaration
  :: HsTypeClass
  -> Either SynthesisDeclarationError SynthesisDeclaration
toSynthesisClassDeclaration declaration =
  toSynthesisClassDeclarationWithMethods declaration []

-- | Preserve class ownership in the shared declaration while accepting the
-- flat binding shape consumed by Exference search. The environment adapter
-- has already selected the owning class by qualified name; this boundary
-- derives, checks, and removes the one leading owner constraint, preventing
-- ordinary constrained functions from being mistaken for class selectors.
toSynthesisClassDeclarationWithMethods
  :: HsTypeClass
  -> [FunctionBinding]
  -> Either SynthesisDeclarationError SynthesisDeclaration
toSynthesisClassDeclarationWithMethods declaration methods = checked $
  SharedDeclaration.ClassDeclaration NoDeclarationMetadata
    (tclass_name declaration)
    (map flexibleParameter $ tclass_params declaration)
    <$> mapM checkedConstraint (tclass_constraints declaration)
    <*> mapM convertedMethod methods
 where
  expectedConstraint = classMethodConstraint declaration

  convertedMethod binding = case functionConstraints binding of
    actual : remaining
      | actual == expectedConstraint -> valueSignature
          (binding { functionConstraints = remaining })
      | otherwise -> Left $ MismatchedClassMethodConstraint
          (functionName binding) expectedConstraint actual
    _ -> Left $ MissingClassMethodConstraint $ functionName binding

fromSynthesisClassDeclaration
  :: SynthesisDeclaration
  -> Either SynthesisDeclarationError HsTypeClass
fromSynthesisClassDeclaration declaration = do
  validateShared declaration
  case declaration of
    SharedDeclaration.ClassDeclaration _ name parameters superclasses methods
      | null methods -> HsTypeClass name
          <$> mapM plainFlexibleParameter parameters
          <*> mapM checkedConstraint superclasses
      | otherwise -> Left $ ClassMethodsUnsupported
          $ map SharedDeclaration.valueName methods
    _ -> Left ExpectedClassDeclaration

-- | Lower a shared class and all its owned method signatures to Exference's
-- method-free class graph plus the flat bindings required by search.  The
-- owner constraint is derived here, rather than stored redundantly in the
-- shared method type.
fromSynthesisClassDeclarationWithMethods
  :: SynthesisDeclaration
  -> Either SynthesisDeclarationError (HsTypeClass, [FunctionBinding])
fromSynthesisClassDeclarationWithMethods declaration = do
  validateShared declaration
  case declaration of
    SharedDeclaration.ClassDeclaration _ name parameters superclasses methods -> do
      typeClass <- HsTypeClass name
        <$> mapM plainFlexibleParameter parameters
        <*> mapM checkedConstraint superclasses
      bindings <- mapM (lowerMethod $ classMethodConstraint typeClass) methods
      Right (typeClass, bindings)
    _ -> Left ExpectedClassDeclaration
 where
  lowerMethod owner signature = do
    binding <- fromSynthesisFunctionBinding
      $ SharedDeclaration.ValueDeclaration signature
    Right binding
      { functionConstraints = owner : functionConstraints binding }

-- | The implicit selector constraint for a class declaration.  Class and
-- method elaboration share the declaration's parameter IDs, so this value is
-- also the exact ownership witness carried by the frontend's tagged binding.
classMethodConstraint :: HsTypeClass -> HsConstraint
classMethodConstraint declaration = HsConstraint
  (tclass_name declaration) (map TypeVar $ tclass_params declaration)

-- | Add one implicit class-method constraint to a prenex source type.
addClassMethodConstraint :: HsConstraint -> HsType -> HsType
addClassMethodConstraint constraint typeExpression = case typeExpression of
  TypeForallNative variables constraints body ->
    TypeForallNative variables (constraint : constraints) body
  body -> TypeForall [] [constraint] body

toSynthesisInstanceDeclaration
  :: HsInstance
  -> Either SynthesisDeclarationError SynthesisDeclaration
toSynthesisInstanceDeclaration declaration = checked $
  do
    prerequisites <- mapM checkedConstraint
      $ instance_constraints declaration
    headConstraint <- checkedConstraint $ instance_head declaration
    -- HsInstance quantifies these variables implicitly. Materialize that
    -- binder set before crossing the explicit shared declaration boundary.
    let variables = Set.toAscList $ instanceConstraintVariables
          $ headConstraint : prerequisites
    Right $ SharedDeclaration.InstanceDeclaration NoDeclarationMetadata
      variables prerequisites headConstraint

fromSynthesisInstanceDeclaration
  :: SynthesisDeclaration
  -> Either SynthesisDeclarationError HsInstance
fromSynthesisInstanceDeclaration declaration = do
  validateShared declaration
  case declaration of
    SharedDeclaration.InstanceDeclaration _ variables prerequisites headConstraint
      | Set.fromList variables == instanceConstraintVariables
          (headConstraint : prerequisites)
      , all SharedType.isFlexibleVariable variables -> HsInstance
          <$> mapM checkedConstraint prerequisites
          <*> checkedConstraint headConstraint
      | otherwise -> Left $ NonImplicitInstanceForall variables
    _ -> Left ExpectedInstanceDeclaration

toSynthesisDataDeclaration
  :: DeconstructorBinding
  -> Either SynthesisDeclarationError SynthesisDeclaration
toSynthesisDataDeclaration =
  toSynthesisDataDeclarationWith $ const $ Right NoDeclarationMetadata

-- | Convert a search datatype while retaining each constructor's cost in
-- the shared constructor annotation.  The map is deliberately keyed by
-- constructor name: ratings are a backend policy attached to constructors,
-- while their types continue to come from the checked datatype declaration.
toSynthesisRatedDataDeclaration
  :: Map.Map QualifiedName Penalty
  -> DeconstructorBinding
  -> Either SynthesisDeclarationError SynthesisDeclaration
toSynthesisRatedDataDeclaration penalties =
  toSynthesisDataDeclarationWith $ \constructor ->
    case Map.lookup (constructorName constructor) penalties of
      Just penalty -> Right $ SearchPenaltyMetadata penalty
      Nothing -> Left $ MissingConstructorPenalty
        $ constructorName constructor

toSynthesisDataDeclarationWith
  :: (ConstructorBinding
      -> Either SynthesisDeclarationError DeclarationMetadata)
  -> DeconstructorBinding
  -> Either SynthesisDeclarationError SynthesisDeclaration
toSynthesisDataDeclarationWith constructorMetadata declaration = do
  (name, parameters) <- deconstructorHead $ deconstructorInput declaration
  checked $ SharedDeclaration.DataTypeDeclaration
    (RecursiveDataMetadata $ deconstructorRecursive declaration)
    name
    (map flexibleParameter parameters)
    <$> mapM (convertedConstructorWith constructorMetadata)
          (deconstructorConstructors declaration)

fromSynthesisDataDeclaration
  :: SynthesisDeclaration
  -> Either SynthesisDeclarationError DeconstructorBinding
fromSynthesisDataDeclaration declaration = do
  validateShared declaration
  case declaration of
    SharedDeclaration.DataTypeDeclaration metadata name parameters constructors -> do
      recursive <- case metadata of
        RecursiveDataMetadata value -> Right value
        _ -> Left MissingRecursiveDataMetadata
      variables <- mapM plainFlexibleParameter parameters
      convertedConstructors <- mapM loweredConstructor constructors
      let input = SharedType.applyTypeArguments (TypeCons name)
            $ map TypeVar variables
      Right $ DeconstructorBinding input convertedConstructors recursive
    _ -> Left ExpectedDataDeclaration

-- | Derive every recursion flag from the complete alias-free datatype set.
-- Incoming flags are ignored. Malformed heads are left nonrecursive here and
-- remain explicit errors at the checked environment boundary; this structural
-- pass never lets one bad compatibility record create a graph vertex.
deriveRecursiveDataMetadata
  :: [DeconstructorBinding]
  -> [DeconstructorBinding]
deriveRecursiveDataMetadata declarations = map attach declarations
 where
  recursiveNames = SharedDeclaration.recursiveDataTypeNames
    sharedDeclarations
  sharedDeclarations
    :: [SharedDeclaration.Declaration SynthesisVariable Void ()]
  sharedDeclarations =
    [ SharedDeclaration.DataTypeDeclaration ()
        name []
        [ SharedDeclaration.DataConstructor ()
            (constructorName constructor)
            (constructorFields constructor)
        | constructor <- deconstructorConstructors declaration
        ]
    | declaration <- declarations
    , Right (name, _) <- [deconstructorHead $ deconstructorInput declaration]
    ]
  attach declaration = declaration
    { deconstructorRecursive = case deconstructorHead
        (deconstructorInput declaration) of
        Left _ -> False
        Right (name, _) -> name `Set.member` recursiveNames
    }

-- | Lower a rated shared datatype to the two records consumed by Exference
-- search.  Constructor functions have the declaration's result, their own
-- fields as parameters, and no constraints; source frontends must reject
-- constrained or existential constructors before reaching this core IR.
fromSynthesisRatedDataDeclaration
  :: SynthesisDeclaration
  -> Either SynthesisDeclarationError
      ([FunctionBinding], DeconstructorBinding)
fromSynthesisRatedDataDeclaration declaration = do
  deconstructor <- fromSynthesisDataDeclaration declaration
  case declaration of
    SharedDeclaration.DataTypeDeclaration _ _ _ constructors -> do
      functions <- mapM
        (loweredRatedConstructor $ deconstructorInput deconstructor)
        constructors
      Right (functions, deconstructor)
    _ -> Left ExpectedDataDeclaration

toSynthesisEnvironment
  :: EnvDictionary
  -> Either SynthesisEnvironmentError SynthesisEnvironment
toSynthesisEnvironment =
  toSynthesisEnvironmentWith toSynthesisDataDeclaration Map.empty

-- | Seal the frontend projection while nesting tagged class methods under
-- their owning shared declarations.  The backend dictionary intentionally
-- contains only ordinary values here; selectors are lowered back to flat
-- bindings after the shared inventory has validated them.
toSynthesisEnvironmentWithConstructorPenaltiesAndClassMethods
  :: Map.Map QualifiedName Penalty
  -> Map.Map QualifiedName [FunctionBinding]
  -> EnvDictionary
  -> Either SynthesisEnvironmentError SynthesisEnvironment
toSynthesisEnvironmentWithConstructorPenaltiesAndClassMethods
    penalties methods =
  toSynthesisEnvironmentWith
    (toSynthesisRatedDataDeclaration penalties) methods

toSynthesisEnvironmentWith
  :: (DeconstructorBinding
      -> Either SynthesisDeclarationError SynthesisDeclaration)
  -> Map.Map QualifiedName [FunctionBinding]
  -> EnvDictionary
  -> Either SynthesisEnvironmentError SynthesisEnvironment
toSynthesisEnvironmentWith convertDataDeclaration methods environment = do
  let classes = sClassEnv_tclasses $ environmentClasses environment
  case Set.toAscList $ Map.keysSet methods `Set.difference` Map.keysSet classes of
    owner : _ -> Left $ UnknownClassMethodOwner owner
    [] -> pure ()
  declarations <- first SynthesisEnvironmentDeclarationError $ sequence $
    map toSynthesisFunctionBinding (environmentFunctions environment) ++
    map convertDataDeclaration (environmentDeconstructors environment) ++
    [ toSynthesisClassDeclarationWithMethods declaration
        $ Map.findWithDefault [] (tclass_name declaration) methods
    | declaration <- Map.elems classes
    ] ++
    map toSynthesisInstanceDeclaration
      (sClassEnv_explicitInstances $ environmentClasses environment)
  either (Left . InvalidSharedEnvironment) Right
    $ SharedEnvironment.mkEnvironment declarations

fromSynthesisEnvironment
  :: SynthesisEnvironment
  -> Either SynthesisEnvironmentError EnvDictionary
fromSynthesisEnvironment = fmap fst . lowerSynthesisEnvironment
  (fmap (, []) . fromSynthesisClassDeclaration)

-- | Lower an environment while retaining class-method ownership explicitly.
-- Ordinary values remain in the backend dictionary; methods are grouped by
-- their exact owning class and remain in declaration order.  Callers that do
-- not have somewhere to retain the second component must use the legacy
-- 'fromSynthesisEnvironment', which rejects method-bearing classes.
fromSynthesisEnvironmentWithClassMethods
  :: SynthesisEnvironment
  -> Either SynthesisEnvironmentError
      (EnvDictionary, Map.Map QualifiedName [FunctionBinding])
fromSynthesisEnvironmentWithClassMethods =
  lowerSynthesisEnvironment fromSynthesisClassDeclarationWithMethods

-- Stable inventory lowering derives constructor and class-method bindings
-- from their owning declarations. Historical compatibility environments
-- already store their ordinary bindings separately, so deriving them again
-- would duplicate search entries and lose explicit source ordering.
data BindingProjection
  = IncludeDerivedBindings
  | ExplicitBindingsOnly

lowerSynthesisEnvironment
  :: (SynthesisDeclaration
      -> Either SynthesisDeclarationError (HsTypeClass, [FunctionBinding]))
  -> SynthesisEnvironment
  -> Either SynthesisEnvironmentError
      (EnvDictionary, Map.Map QualifiedName [FunctionBinding])
lowerSynthesisEnvironment convertClass environment =
  lowerSynthesisDeclarations ExplicitBindingsOnly convertClass
    $ SharedEnvironment.environmentDeclarations environment

-- Traverse the shared declaration grammar once for both lowering surfaces.
-- The projection policy controls only ownership of derived search bindings;
-- declaration order, validation, class assembly, and failure precedence stay
-- common.
lowerSynthesisDeclarations
  :: BindingProjection
  -> (SynthesisDeclaration
      -> Either SynthesisDeclarationError (HsTypeClass, [FunctionBinding]))
  -> [SynthesisDeclaration]
  -> Either SynthesisEnvironmentError
      (EnvDictionary, Map.Map QualifiedName [FunctionBinding])
lowerSynthesisDeclarations projection convertClass declarations = do
  (functions, deconstructors, classes, instances, methods) <- foldM collect
    ([], [], [], [], Map.empty)
    declarations
  classEnvironment <- either (Left . ClassEnvironmentConversionError) Right
    $ mkStaticClassEnv (reverse classes) (reverse instances)
  Right
    ( EnvDictionary
        (reverse functions) (reverse deconstructors) classEnvironment
    , methods
    )
 where
  collect (functions, deconstructors, classes, instances, methods)
      declaration =
    case declaration of
      SharedDeclaration.ValueDeclaration{} -> do
        binding <- declarationLevel $ fromSynthesisFunctionBinding declaration
        Right
          ( binding : functions, deconstructors, classes, instances, methods)
      SharedDeclaration.DataTypeDeclaration{} -> do
        (constructors, deconstructor) <- declarationLevel $ case projection of
          IncludeDerivedBindings ->
            fromSynthesisRatedDataDeclaration declaration
          ExplicitBindingsOnly ->
            ([],) <$> fromSynthesisDataDeclaration declaration
        Right
          ( reverse constructors ++ functions
          , deconstructor : deconstructors, classes, instances
          , methods
          )
      SharedDeclaration.ClassDeclaration{} -> do
        (classDeclaration, classMethods) <- declarationLevel
          $ convertClass declaration
        let (derivedMethods, retainedMethods) = case projection of
              IncludeDerivedBindings -> (reverse classMethods, methods)
              ExplicitBindingsOnly
                | null classMethods -> ([], methods)
                | otherwise ->
                    ( []
                    , Map.insert
                        (tclass_name classDeclaration) classMethods methods
                    )
        Right
          ( derivedMethods ++ functions
          , deconstructors, classDeclaration : classes, instances
          , retainedMethods
          )
      SharedDeclaration.InstanceDeclaration{} -> do
        instanceDeclaration <- declarationLevel
          $ fromSynthesisInstanceDeclaration declaration
        Right
          ( functions, deconstructors, classes
          , instanceDeclaration : instances, methods
          )
      SharedDeclaration.TypeSynonymDeclaration _ name _ _ ->
        Left $ UnsupportedCoreEnvironmentDeclaration name
      SharedDeclaration.AbstractTypeDeclaration _ name _ ->
        Left $ UnsupportedCoreEnvironmentDeclaration name

  declarationLevel
    :: Either SynthesisDeclarationError result
    -> Either SynthesisEnvironmentError result
  declarationLevel = first SynthesisEnvironmentDeclarationError

checked
  :: Either SynthesisDeclarationError SynthesisDeclaration
  -> Either SynthesisDeclarationError SynthesisDeclaration
checked conversion = do
  declaration <- conversion
  validateShared declaration
  return declaration

validateShared
  :: SynthesisDeclaration
  -> Either SynthesisDeclarationError ()
validateShared = either (Left . InvalidSharedDeclaration) Right
  . SharedDeclaration.validateDeclaration

-- | Build a checked shared synonym declaration from Exference's historical
-- type vocabulary.  Keeping this alongside the other declaration adapters
-- makes the parser-free core the sole authority for which parameter shapes
-- the compatibility model can represent.
toSynthesisTypeSynonym
  :: QualifiedName
  -> [TVarId]
  -> HsType
  -> Either SynthesisDeclarationError SynthesisDeclaration
toSynthesisTypeSynonym name parameters body = checked $ do
  checkedBody <- checkedType body
  Right $ SharedDeclaration.TypeSynonymDeclaration
    NoDeclarationMetadata name (map flexibleParameter parameters) checkedBody

-- | Lower a checked shared synonym to the lossless fields represented by the
-- historical frontend record.  Explicit parameter kinds and rigid parameters
-- are rejected here consistently with class and datatype lowering.
fromSynthesisTypeSynonym
  :: SynthesisDeclaration
  -> Either SynthesisDeclarationError (QualifiedName, [TVarId], HsType)
fromSynthesisTypeSynonym declaration = do
  validateShared declaration
  case declaration of
    SharedDeclaration.TypeSynonymDeclaration _ name parameters body ->
      (name,,)
        <$> mapM plainFlexibleParameter parameters
        <*> checkedType body
    _ -> Left ExpectedTypeSynonymDeclaration

checkedType
  :: HsType
  -> Either SynthesisDeclarationError HsType
checkedType = first DeclarationTypeConversionError . toSynthesisType

checkedConstraint
  :: HsConstraint
  -> Either SynthesisDeclarationError HsConstraint
checkedConstraint = traverse checkedType

flexibleParameter
  :: TVarId
  -> SharedDeclaration.TypeParameter SynthesisVariable Void
flexibleParameter variable = SharedDeclaration.TypeParameter
  (SharedType.FlexibleVariable variable) Nothing

plainFlexibleParameter
  :: SharedDeclaration.TypeParameter SynthesisVariable Void
  -> Either SynthesisDeclarationError TVarId
plainFlexibleParameter parameter = case
    SharedDeclaration.parameterKind parameter of
  Just _ -> Left $ ExplicitParameterKindUnsupported
    $ SharedDeclaration.parameterVariable parameter
  Nothing -> case SharedDeclaration.parameterVariable parameter of
    SharedType.FlexibleVariable variable -> Right variable
    SharedType.RigidVariable variable -> Left $ RigidDataParameter variable

convertedConstructorWith
  :: (ConstructorBinding
      -> Either SynthesisDeclarationError DeclarationMetadata)
  -> ConstructorBinding
  -> Either SynthesisDeclarationError
      (SharedDeclaration.DataConstructor
        SynthesisVariable DeclarationMetadata)
convertedConstructorWith metadata constructor = SharedDeclaration.DataConstructor
  <$> metadata constructor
  <*> pure (constructorName constructor)
  <*> mapM checkedType (constructorFields constructor)

loweredConstructor
  :: SharedDeclaration.DataConstructor
      SynthesisVariable DeclarationMetadata
  -> Either SynthesisDeclarationError ConstructorBinding
loweredConstructor constructor = ConstructorBinding
  (SharedDeclaration.constructorName constructor)
  <$> mapM checkedType (SharedDeclaration.constructorFields constructor)

loweredRatedConstructor
  :: HsType
  -> SharedDeclaration.DataConstructor
      SynthesisVariable DeclarationMetadata
  -> Either SynthesisDeclarationError FunctionBinding
loweredRatedConstructor result constructor = do
  penalty <- case SharedDeclaration.constructorAnnotation constructor of
    SearchPenaltyMetadata value -> Right value
    _ -> Left $ MissingSearchPenaltyMetadata
      $ SharedDeclaration.constructorName constructor
  lowered <- loweredConstructor constructor
  Right $ FunctionBinding
    result
    (constructorName lowered)
    penalty
    []
    (constructorFields lowered)

deconstructorHead
  :: HsType
  -> Either SynthesisDeclarationError (QualifiedName, [TVarId])
deconstructorHead typeExpression = do
  (binders, body) <- case SharedType.splitLeadingForalls typeExpression of
    (nativeBinders, [], body) -> case traverse
        SharedType.flexibleVariableIdentity nativeBinders of
      Just binders -> Right (binders, body)
      Nothing -> Left $ InvalidDeconstructorHead typeExpression
    _ -> Left $ InvalidDeconstructorHead typeExpression
  (name, arguments) <- nominalApplication body
  parameters <- mapM typeVariable arguments
  if null binders || Set.fromList binders == Set.fromList parameters
    then Right (name, parameters)
    else Left $ DeconstructorForallMismatch binders parameters
 where
  nominalApplication (TypeTuple boxity elements) = do
    name <- either (const $ Left $ InvalidDeconstructorHead typeExpression)
      Right $ SharedName.tupleName boxity $ length elements
    Right (name, elements)
  nominalApplication body = case SharedType.applicationSpine body of
    (TypeCons name, arguments) -> Right (name, arguments)
    _ -> Left $ InvalidDeconstructorHead typeExpression

  typeVariable (TypeVar variable) = Right variable
  typeVariable argument = Left $ NonVariableDataParameter argument
