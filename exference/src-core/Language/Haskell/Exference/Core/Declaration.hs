{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

-- | Checked adapters between Exference's search-oriented declaration records
-- and the shared source-declaration vocabulary.
module Language.Haskell.Exference.Core.Declaration
  ( DeclarationMetadata (..)
  , SynthesisDeclaration
  , SynthesisEnvironment
  , SynthesisInventory
  , NeutralSynthesisInventory
  , PreparedSynthesisInventory
  , PreparedNeutralSynthesisInventory
  , SynthesisDeclarationError (..)
  , freshSynthesisVariable
  , prepareSynthesisInventory
  , prepareNeutralSynthesisInventory
  , projectSynthesisInventory
  , projectNeutralSynthesisInventory
  , preparedSynthesisInventory
  , preparedSynthesisTypeSynonyms
  , preparedSynthesisWitness
  , preparedSynthesisBackend
  , preparedNeutralInventory
  , preparedNeutralTypeSynonyms
  , preparedNeutralBackend
  , erasePreparedSynthesisAnnotations
  , deriveRecursiveDataMetadata
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
  , toSynthesisEnvironmentWithConstructorPenalties
  , toSynthesisEnvironmentWithConstructorPenaltiesAndClassMethods
  , fromSynthesisEnvironment
  , fromSynthesisEnvironmentWithClassMethods
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.List (find, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Language.Haskell.Synthesis.Collection as SharedCollection
import qualified Language.Haskell.Synthesis.Constraint as SharedConstraint
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

-- | Annotation-free, already checked shared inventory accepted by the core
-- lowerer. Ratings and recursive-datatype flags are derived while lowering;
-- they are backend policy, not source declaration syntax.
type NeutralSynthesisInventory = SharedInventory.Inventory SynthesisVariable ()

-- | One checked inventory and the exact synonym table and backend lowering
-- prepared from it. Keeping the constructor private prevents frontends from
-- pairing a checked inventory with an unrelated search dictionary. The
-- annotation parameter lets a source frontend retain presentation metadata
-- without creating a second semantic inventory.
data PreparedSynthesisInventory annotation = PreparedSynthesisInventory
  (SharedTypeSynonym.PreparedInventory SynthesisVariable annotation)
  EnvDictionary

-- | The annotation-free prepared witness accepted by stable core sessions.
type PreparedNeutralSynthesisInventory = PreparedSynthesisInventory ()

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
  | UnknownClassMethodOwner QualifiedName
  | MissingClassMethodBindings [QualifiedName]
  | DuplicateClassMethodBindings [QualifiedName]
  | OrphanClassMethodBindings [QualifiedName]
  | MismatchedClassMethodOwners
      [(QualifiedName, QualifiedName, QualifiedName)]
  | PreparedBindingNamesMismatch
      [QualifiedName] -- ^ Ordered source multiset.
      [QualifiedName] -- ^ Ordered prepared-backend multiset.
  | PreparedDataTypeNamesMismatch
      [QualifiedName] -- ^ Ordered source multiset.
      [QualifiedName] -- ^ Ordered prepared-backend multiset.
  | InvalidPreparedBindingPenalty QualifiedName Penalty
  | PreparedDataMetadataMissing SharedName.Name
  | ExplicitParameterKindUnsupported SynthesisVariable
  | InvalidDeconstructorHead HsType
  | NonVariableDataParameter HsType
  | RigidDataParameter TVarId
  | DeconstructorForallMismatch [TVarId] [TVarId]
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
  | NeutralSynonymExpansionError
      Int -- ^ Zero-based source declaration index.
      SharedName.Name -- ^ Declaration name or instance class.
      (SharedTypeSynonym.SynonymExpansionError SynthesisVariable)
  | NeutralSynonymPreparationError
      (SharedTypeSynonym.SynonymExpansionError SynthesisVariable)
  | NeutralVariableNamespaceExhausted SynthesisVariable
  deriving (Eq, Show)

-- | Lower an already checked inventory to Exference's search dictionary.
-- Type aliases and explicit kinds remain authoritative in that inventory;
-- annotations are frontend metadata and never participate in lowering.
prepareSynthesisInventory
  :: SynthesisInventory
  -> Either SynthesisDeclarationError
      (PreparedSynthesisInventory DeclarationMetadata)
prepareSynthesisInventory inventory =
  prepareInventory inventory >>= normalizePreparedDataMetadata

prepareInventory
  :: SharedInventory.Inventory SynthesisVariable annotation
  -> Either SynthesisDeclarationError
      (PreparedSynthesisInventory annotation)
prepareInventory inventory = do
  prepared <- first NeutralSynonymPreparationError
    $ SharedTypeSynonym.prepareInventory freshSynthesisVariable inventory
  let synonyms = SharedTypeSynonym.preparedTypeSynonyms prepared
  backend <- lowerNeutralSynthesisEnvironment synonyms
    $ fmap (const ())
    $ SharedInventory.inventoryEnvironment
    $ SharedTypeSynonym.preparedInventory prepared
  pure $ PreparedSynthesisInventory prepared backend

-- | Compatibility specialization for annotation-free callers.
prepareNeutralSynthesisInventory
  :: NeutralSynthesisInventory
  -> Either SynthesisDeclarationError PreparedNeutralSynthesisInventory
prepareNeutralSynthesisInventory = prepareInventory

-- | Reorder a canonical backend to match a source frontend and attach its
-- finite heuristic ratings.  Names are an exact inventory: callers may choose
-- order and ratings, but cannot replace types, constraints, classes,
-- instances, constructors, or datatype metadata with independently prepared
-- values.
projectSynthesisInventory
  :: [(QualifiedName, Penalty)]
  -> [QualifiedName]
  -> PreparedSynthesisInventory annotation
  -> Either SynthesisDeclarationError
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
  preparedDataNames <- mapM deconstructorTypeName
    $ environmentDeconstructors backend
  let sourceDataNames = sort dataProjection
      canonicalDataNames = sort preparedDataNames
  if sourceDataNames == canonicalDataNames
    then pure ()
    else Left $ PreparedDataTypeNamesMismatch
      sourceDataNames canonicalDataNames
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
          sourceDataNames canonicalDataNames)
        Right
        $ Map.lookup name deconstructorsByName
  functions <- mapM projectFunction functionProjection
  deconstructors <- mapM projectDeconstructor dataProjection
  pure $ PreparedSynthesisInventory prepared
    (backend
      { environmentFunctions = functions
      , environmentDeconstructors = deconstructors
      })

-- | Compatibility name retained for annotation-free callers. The operation is
-- actually annotation-polymorphic because it changes only backend order and
-- penalties.
projectNeutralSynthesisInventory
  :: [(QualifiedName, Penalty)]
  -> [QualifiedName]
  -> PreparedNeutralSynthesisInventory
  -> Either SynthesisDeclarationError PreparedNeutralSynthesisInventory
projectNeutralSynthesisInventory = projectSynthesisInventory

-- | The authoritative checked inventory owned by a prepared lowering.
preparedSynthesisInventory
  :: PreparedSynthesisInventory annotation
  -> SharedInventory.Inventory SynthesisVariable annotation
preparedSynthesisInventory
    (PreparedSynthesisInventory prepared _) =
  SharedTypeSynonym.preparedInventory prepared

-- | The authoritative checked inventory owned by a prepared lowering.
preparedNeutralInventory
  :: PreparedNeutralSynthesisInventory
  -> NeutralSynthesisInventory
preparedNeutralInventory = preparedSynthesisInventory

-- | The alias table prepared from the witness's own checked inventory.
preparedSynthesisTypeSynonyms
  :: PreparedSynthesisInventory annotation
  -> SharedTypeSynonym.TypeSynonyms SynthesisVariable
preparedSynthesisTypeSynonyms
    (PreparedSynthesisInventory prepared _) =
  SharedTypeSynonym.preparedTypeSynonyms prepared

-- | The shared inventory/alias witness retained after the backend projection
-- has been consumed. Session sealing uses this projection so the complete
-- unfiltered search dictionary cannot remain live beside its filtered view.
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

-- | Compatibility accessor specialized to an annotation-free witness.
preparedNeutralTypeSynonyms
  :: PreparedNeutralSynthesisInventory
  -> SharedTypeSynonym.TypeSynonyms SynthesisVariable
preparedNeutralTypeSynonyms = preparedSynthesisTypeSynonyms

-- | Compatibility accessor specialized to an annotation-free witness.
preparedNeutralBackend
  :: PreparedNeutralSynthesisInventory
  -> EnvDictionary
preparedNeutralBackend = preparedSynthesisBackend

-- | Erase frontend annotations without rebuilding the inventory, synonym
-- table, or projected backend. The resulting witness has exactly the same
-- semantic declarations and kind assumptions.
erasePreparedSynthesisAnnotations
  :: PreparedSynthesisInventory annotation
  -> PreparedNeutralSynthesisInventory
erasePreparedSynthesisAnnotations
    (PreparedSynthesisInventory prepared backend) =
  PreparedSynthesisInventory (fmap (const ()) prepared) backend

-- Attach alias-aware recursion flags derived by the canonical core lowerer to
-- the opaque prepared inventory. Every concrete datatype must have one backend
-- deconstructor; abstract types deliberately have none.
normalizePreparedDataMetadata
  :: PreparedSynthesisInventory DeclarationMetadata
  -> Either SynthesisDeclarationError
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
    name <- deconstructorTypeName deconstructor
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

-- Keep the actual lowering private once the witness has been assembled:
-- accepting an independently prepared alias table here would allow a
-- mismatched table to turn an alias into a fictitious nominal constructor.
lowerNeutralSynthesisEnvironment
  :: SharedTypeSynonym.TypeSynonyms SynthesisVariable
  -> SharedEnvironment.Environment SynthesisVariable Void ()
  -> Either SynthesisDeclarationError EnvDictionary
lowerNeutralSynthesisEnvironment synonyms environment = do
  expanded <- mapM expandOperationalDeclaration
    $ zip [0 ..] $ SharedEnvironment.environmentDeclarations environment
  let normalized = map normalizeDeclarationVariables expanded
      recursiveNames = SharedDeclaration.recursiveDataTypeNames normalized
  prepared <- mapM (prepareSearchDeclaration recursiveNames) normalized
  (functions, deconstructors, classes, instances) <- foldM lowerDeclaration
    ([], [], [], []) [declaration | Just declaration <- prepared]
  classEnvironment <- either (Left . ClassEnvironmentConversionError) Right
    $ mkStaticClassEnv (reverse classes) (reverse instances)
  pure $ EnvDictionary
    (reverse functions) (reverse deconstructors) classEnvironment
 where
  expandOperationalDeclaration (index, declaration) = case declaration of
    SharedDeclaration.TypeSynonymDeclaration{} -> Right declaration
    SharedDeclaration.AbstractTypeDeclaration{} -> Right declaration
    _ -> first
      (NeutralSynonymExpansionError index $ declarationName declaration)
      $ SharedTypeSynonym.expandDeclarationTypeSynonyms
          freshSynthesisVariable synonyms declaration

  lowerDeclaration
      (functions, deconstructors, classes, instances) declaration =
    case declaration of
      SharedDeclaration.ValueDeclaration{} -> do
        binding <- fromSynthesisFunctionBinding declaration
        pure (binding : functions, deconstructors, classes, instances)
      SharedDeclaration.DataTypeDeclaration{} -> do
        (constructors, deconstructor) <-
          fromSynthesisRatedDataDeclaration declaration
        pure
          ( reverse constructors ++ functions
          , deconstructor : deconstructors
          , classes
          , instances
          )
      SharedDeclaration.ClassDeclaration{} -> do
        (typeClass, methods) <-
          fromSynthesisClassDeclarationWithMethods declaration
        pure
          ( reverse methods ++ functions
          , deconstructors
          , typeClass : classes
          , instances
          )
      SharedDeclaration.InstanceDeclaration{} -> do
        instanceDeclaration <- fromSynthesisInstanceDeclaration declaration
        pure
          ( functions
          , deconstructors
          , classes
          , instanceDeclaration : instances
          )
      SharedDeclaration.TypeSynonymDeclaration{} -> pure
        (functions, deconstructors, classes, instances)
      SharedDeclaration.AbstractTypeDeclaration{} -> pure
        (functions, deconstructors, classes, instances)

type NeutralSynthesisDeclaration = SharedDeclaration.Declaration
  SynthesisVariable Void ()

declarationName :: NeutralSynthesisDeclaration -> SharedName.Name
declarationName declaration = case declaration of
  SharedDeclaration.TypeSynonymDeclaration _ name _ _ -> name
  SharedDeclaration.DataTypeDeclaration _ name _ _ -> name
  SharedDeclaration.AbstractTypeDeclaration _ name _ -> name
  SharedDeclaration.ValueDeclaration signature ->
    SharedDeclaration.valueName signature
  SharedDeclaration.ClassDeclaration _ name _ _ _ -> name
  SharedDeclaration.InstanceDeclaration _ _ _ headConstraint ->
    SharedConstraint.constraintClass headConstraint

-- Variable identities are local to a source declaration. Repacking flexible
-- IDs makes negative class parameters acceptable to the historical core and
-- gives every method in a class the same coherent owner namespace. Parameters
-- are visited first, followed by the declaration's remaining source order.
normalizeDeclarationVariables
  :: NeutralSynthesisDeclaration
  -> NeutralSynthesisDeclaration
normalizeDeclarationVariables declaration = case declaration of
  SharedDeclaration.TypeSynonymDeclaration annotation name parameters body ->
    SharedDeclaration.TypeSynonymDeclaration annotation name
      (map renameParameter parameters) (renameType body)
  SharedDeclaration.DataTypeDeclaration annotation name parameters constructors ->
    SharedDeclaration.DataTypeDeclaration annotation name
      (map renameParameter parameters) (map renameConstructor constructors)
  SharedDeclaration.AbstractTypeDeclaration{} -> declaration
  SharedDeclaration.ValueDeclaration signature ->
    SharedDeclaration.ValueDeclaration $ renameSignature signature
  SharedDeclaration.ClassDeclaration annotation name parameters
      superclasses methods -> SharedDeclaration.ClassDeclaration annotation name
        (map renameParameter parameters)
        (map renameConstraint superclasses)
        (map renameSignature methods)
  SharedDeclaration.InstanceDeclaration annotation variables
      prerequisites headConstraint -> SharedDeclaration.InstanceDeclaration
        annotation (map renameVariable variables)
        (map renameConstraint prerequisites) (renameConstraint headConstraint)
 where
  flexibleVariables = SharedCollection.distinctOn id
    [ variable
    | variable@SharedType.FlexibleVariable{} <- declarationVariables declaration
    ]
  replacements = Map.fromList $ zip flexibleVariables
    (map SharedType.FlexibleVariable synthesisIdentifierNamespace)

  renameVariable variable = Map.findWithDefault variable variable replacements
  renameType = fmap renameVariable
  renameConstraint = fmap renameType
  renameParameter parameter = SharedDeclaration.TypeParameter
    (renameVariable $ SharedDeclaration.parameterVariable parameter)
    (SharedDeclaration.parameterKind parameter)
  renameConstructor constructor = SharedDeclaration.DataConstructor
    (SharedDeclaration.constructorAnnotation constructor)
    (SharedDeclaration.constructorName constructor)
    (map renameType $ SharedDeclaration.constructorFields constructor)
  renameSignature signature = SharedDeclaration.ValueSignature
    (SharedDeclaration.valueAnnotation signature)
    (SharedDeclaration.valueName signature)
    (renameType $ SharedDeclaration.valueType signature)

declarationVariables :: NeutralSynthesisDeclaration -> [SynthesisVariable]
declarationVariables declaration = case declaration of
  SharedDeclaration.TypeSynonymDeclaration _ _ parameters body ->
    parameterVariables parameters ++ toList body
  SharedDeclaration.DataTypeDeclaration _ _ parameters constructors ->
    parameterVariables parameters
      ++ concatMap (concatMap toList . SharedDeclaration.constructorFields)
          constructors
  SharedDeclaration.AbstractTypeDeclaration{} -> []
  SharedDeclaration.ValueDeclaration signature ->
    toList $ SharedDeclaration.valueType signature
  SharedDeclaration.ClassDeclaration _ _ parameters superclasses methods ->
    parameterVariables parameters
      ++ concatMap constraintTypeVariables superclasses
      ++ concatMap (toList . SharedDeclaration.valueType) methods
  SharedDeclaration.InstanceDeclaration _ variables prerequisites headConstraint ->
    variables ++ concatMap constraintTypeVariables
      (headConstraint : prerequisites)
 where
  parameterVariables = map SharedDeclaration.parameterVariable
  constraintTypeVariables = concatMap toList
    . SharedConstraint.constraintArguments

prepareSearchDeclaration
  :: Set.Set SharedName.Name
  -> NeutralSynthesisDeclaration
  -> Either SynthesisDeclarationError (Maybe SynthesisDeclaration)
prepareSearchDeclaration recursiveNames declaration = case declaration of
  SharedDeclaration.TypeSynonymDeclaration{} -> Right Nothing
  SharedDeclaration.AbstractTypeDeclaration{} -> Right Nothing
  SharedDeclaration.ValueDeclaration signature -> do
    functionType <- implicitizeLeadingForalls Set.empty
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
      (Set.toAscList $ constraintVariables $ headConstraint : prerequisites)
      prerequisites headConstraint
 where
  eraseParameterKind parameter = SharedDeclaration.TypeParameter
    (SharedDeclaration.parameterVariable parameter) Nothing

  prepareMethod ownerVariables signature = do
    methodType <- implicitizeLeadingForalls ownerVariables
      $ SharedDeclaration.valueType signature
    pure $ SharedDeclaration.ValueSignature (SearchPenaltyMetadata 0)
      (SharedDeclaration.valueName signature) methodType

-- Exference quantifies every free flexible binding variable implicitly. Drop
-- only the complete leading prenex chain and retain its contexts in order;
-- any forall below a type constructor, arrow, tuple, or constraint remains
-- visible to the existing rank-N omission/validation boundary.
implicitizeLeadingForalls
  :: Set.Set SynthesisVariable
  -> SharedType.Type SynthesisVariable
  -> Either SynthesisDeclarationError
      (SharedType.Type SynthesisVariable)
implicitizeLeadingForalls outerVariables source =
  fmap snd $ go initiallyReserved [] source
 where
  -- Every erased binder becomes free in Exference's implicit representation.
  -- Allocate it outside the complete source namespace, not merely outside the
  -- binders seen so far: otherwise flattening could capture a free variable
  -- deeper in the type. Class parameters are also outside a method's explicit
  -- forall and must remain distinct from a shadowing method binder.
  initiallyReserved = outerVariables `Set.union` Set.fromList (toList source)

  go reserved contexts (SharedType.ForallType binders embedded body) = do
    mapM_ requireFlexible binders
    (reserved', renaming) <- foldM freshen
      (reserved, Map.empty) binders
    let renamedEmbedded = map
          (fmap $ SharedType.renameScopedVariables renaming) embedded
        renamedBody = SharedType.renameScopedVariables renaming body
    go reserved' (contexts ++ renamedEmbedded) renamedBody
  go reserved contexts body
    | null contexts = Right (reserved, body)
    | otherwise = Right
        (reserved, SharedType.ForallType [] contexts body)

  freshen (reserved, renaming) binder =
    case freshSynthesisVariable reserved binder of
      Nothing -> Left $ NeutralVariableNamespaceExhausted binder
      Just replacement -> Right
        ( Set.insert replacement reserved
        , Map.insert binder replacement renaming
        )

  requireFlexible SharedType.FlexibleVariable{} = Right ()
  requireFlexible (SharedType.RigidVariable variable) = Left
    $ DeclarationTypeConversionError $ RigidForallBinder variable

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
  <$> convertedType (TypeForall [] (functionConstraints binding)
        $ foldr TypeArrow (functionResult binding)
        $ functionParameters binding)

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
      functionType <- loweredType $ SharedDeclaration.valueType signature
      let (variables, constraints, body) = case functionType of
            TypeForall binders context nested -> (binders, context, nested)
            nested -> ([], [], nested)
      if null variables
        then let (parameters, result) = splitFunctionType body
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
    <$> mapM convertedConstraint (tclass_constraints declaration)
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
          <*> mapM loweredConstraint superclasses
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
        <*> mapM loweredConstraint superclasses
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
    prerequisites <- mapM convertedConstraint
      $ instance_constraints declaration
    headConstraint <- convertedConstraint $ instance_head declaration
    -- HsInstance quantifies these variables implicitly. Materialize that
    -- binder set before crossing the explicit shared declaration boundary.
    let variables = Set.toAscList $ constraintVariables
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
      | Set.fromList variables == constraintVariables
          (headConstraint : prerequisites)
      , all isFlexibleVariable variables -> HsInstance
          <$> mapM loweredConstraint prerequisites
          <*> loweredConstraint headConstraint
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
      let input = foldl TypeApp (TypeCons name)
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
  -> Either SynthesisDeclarationError SynthesisEnvironment
toSynthesisEnvironment =
  toSynthesisEnvironmentWith toSynthesisDataDeclaration Map.empty

-- | Seal a complete core environment without discarding constructor search
-- costs.  Keeping the common declaration assembly here prevents frontends
-- from maintaining a second class/instance/value conversion path.
toSynthesisEnvironmentWithConstructorPenalties
  :: Map.Map QualifiedName Penalty
  -> EnvDictionary
  -> Either SynthesisDeclarationError SynthesisEnvironment
toSynthesisEnvironmentWithConstructorPenalties penalties =
  toSynthesisEnvironmentWithConstructorPenaltiesAndClassMethods
    penalties Map.empty

-- | Seal the frontend projection while nesting tagged class methods under
-- their owning shared declarations.  The backend dictionary intentionally
-- contains only ordinary values here; selectors are lowered back to flat
-- bindings after the shared inventory has validated them.
toSynthesisEnvironmentWithConstructorPenaltiesAndClassMethods
  :: Map.Map QualifiedName Penalty
  -> Map.Map QualifiedName [FunctionBinding]
  -> EnvDictionary
  -> Either SynthesisDeclarationError SynthesisEnvironment
toSynthesisEnvironmentWithConstructorPenaltiesAndClassMethods
    penalties methods =
  toSynthesisEnvironmentWith
    (toSynthesisRatedDataDeclaration penalties) methods

toSynthesisEnvironmentWith
  :: (DeconstructorBinding
      -> Either SynthesisDeclarationError SynthesisDeclaration)
  -> Map.Map QualifiedName [FunctionBinding]
  -> EnvDictionary
  -> Either SynthesisDeclarationError SynthesisEnvironment
toSynthesisEnvironmentWith convertDataDeclaration methods environment = do
  let classes = sClassEnv_tclasses $ environmentClasses environment
  case Set.toAscList $ Map.keysSet methods `Set.difference` Map.keysSet classes of
    owner : _ -> Left $ UnknownClassMethodOwner owner
    [] -> pure ()
  declarations <- sequence $
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
  -> Either SynthesisDeclarationError EnvDictionary
fromSynthesisEnvironment = fmap fst . lowerSynthesisEnvironment
  (fmap (, []) . fromSynthesisClassDeclaration)

-- | Lower an environment while retaining class-method ownership explicitly.
-- Ordinary values remain in the backend dictionary; methods are grouped by
-- their exact owning class and remain in declaration order.  Callers that do
-- not have somewhere to retain the second component must use the legacy
-- 'fromSynthesisEnvironment', which rejects method-bearing classes.
fromSynthesisEnvironmentWithClassMethods
  :: SynthesisEnvironment
  -> Either SynthesisDeclarationError
      (EnvDictionary, Map.Map QualifiedName [FunctionBinding])
fromSynthesisEnvironmentWithClassMethods =
  lowerSynthesisEnvironment fromSynthesisClassDeclarationWithMethods

lowerSynthesisEnvironment
  :: (SynthesisDeclaration
      -> Either SynthesisDeclarationError (HsTypeClass, [FunctionBinding]))
  -> SynthesisEnvironment
  -> Either SynthesisDeclarationError
      (EnvDictionary, Map.Map QualifiedName [FunctionBinding])
lowerSynthesisEnvironment convertClass environment = do
  (functions, deconstructors, classes, instances, methods) <- foldM collect
    ([], [], [], [], Map.empty)
    $ SharedEnvironment.environmentDeclarations environment
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
        binding <- fromSynthesisFunctionBinding declaration
        Right
          ( binding : functions, deconstructors, classes, instances, methods)
      SharedDeclaration.DataTypeDeclaration{} -> do
        deconstructor <- fromSynthesisDataDeclaration declaration
        Right
          ( functions, deconstructor : deconstructors, classes, instances
          , methods
          )
      SharedDeclaration.ClassDeclaration{} -> do
        (classDeclaration, classMethods) <- convertClass declaration
        let retainedMethods
              | null classMethods = methods
              | otherwise = Map.insert
                  (tclass_name classDeclaration) classMethods methods
        Right
          ( functions, deconstructors, classDeclaration : classes, instances
          , retainedMethods
          )
      SharedDeclaration.InstanceDeclaration{} -> do
        instanceDeclaration <- fromSynthesisInstanceDeclaration declaration
        Right
          ( functions, deconstructors, classes
          , instanceDeclaration : instances, methods
          )
      SharedDeclaration.TypeSynonymDeclaration _ name _ _ ->
        Left $ UnsupportedCoreEnvironmentDeclaration name
      SharedDeclaration.AbstractTypeDeclaration _ name _ ->
        Left $ UnsupportedCoreEnvironmentDeclaration name

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

convertedType
  :: HsType
  -> Either SynthesisDeclarationError
      (SharedType.Type SynthesisVariable)
convertedType typeExpression = either
  (Left . DeclarationTypeConversionError) Right
  $ toSynthesisType typeExpression

loweredType
  :: SharedType.Type SynthesisVariable
  -> Either SynthesisDeclarationError HsType
loweredType = convertedType

constraintVariables
  :: [SharedConstraint.Constraint (SharedType.Type SynthesisVariable)]
  -> Set.Set SynthesisVariable
constraintVariables = foldMap (foldMap SharedType.freeVariables)

isFlexibleVariable :: SynthesisVariable -> Bool
isFlexibleVariable SharedType.FlexibleVariable{} = True
isFlexibleVariable SharedType.RigidVariable{} = False

convertedConstraint
  :: HsConstraint
  -> Either SynthesisDeclarationError
      (SharedConstraint.Constraint (SharedType.Type SynthesisVariable))
convertedConstraint = traverse convertedType

loweredConstraint
  :: SharedConstraint.Constraint (SharedType.Type SynthesisVariable)
  -> Either SynthesisDeclarationError HsConstraint
loweredConstraint = traverse loweredType

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
  <*> mapM convertedType (constructorFields constructor)

loweredConstructor
  :: SharedDeclaration.DataConstructor
      SynthesisVariable DeclarationMetadata
  -> Either SynthesisDeclarationError ConstructorBinding
loweredConstructor constructor = ConstructorBinding
  (SharedDeclaration.constructorName constructor)
  <$> mapM loweredType (SharedDeclaration.constructorFields constructor)

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
  (binders, body) <- stripForalls [] typeExpression
  (name, arguments) <- nominalApplication body
  parameters <- mapM typeVariable arguments
  if null binders || Set.fromList binders == Set.fromList parameters
    then Right (name, parameters)
    else Left $ DeconstructorForallMismatch binders parameters
 where
  stripForalls binders (TypeForall variables [] body) =
    stripForalls (binders ++ variables) body
  stripForalls _ TypeForallNative{} =
    Left $ InvalidDeconstructorHead typeExpression
  stripForalls binders body = Right (binders, body)

  nominalApplication (TypeTuple boxity elements) = do
    name <- either (const $ Left $ InvalidDeconstructorHead typeExpression)
      Right $ SharedName.tupleName boxity $ length elements
    Right (name, elements)
  nominalApplication body = case typeApplicationSpine body of
    (TypeCons name, arguments) -> Right (name, arguments)
    _ -> Left $ InvalidDeconstructorHead typeExpression

  typeVariable (TypeVar variable) = Right variable
  typeVariable argument = Left $ NonVariableDataParameter argument

typeApplicationSpine :: HsType -> (HsType, [HsType])
typeApplicationSpine = SharedType.applicationSpine

splitFunctionType :: HsType -> ([HsType], HsType)
splitFunctionType (TypeArrow parameter result) =
  let (parameters, finalResult) = splitFunctionType result
  in (parameter : parameters, finalResult)
splitFunctionType result = ([], result)
