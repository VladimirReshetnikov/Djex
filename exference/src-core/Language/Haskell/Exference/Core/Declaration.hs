{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

-- | Checked adapters between Exference's search-oriented declaration records
-- and the shared source-declaration vocabulary.
module Language.Haskell.Exference.Core.Declaration
  ( DeclarationMetadata (..)
  , SynthesisDeclaration
  , SynthesisEnvironment
  , SynthesisInventory
  , SynthesisDeclarationError (..)
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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Language.Haskell.Synthesis.Constraint as SharedConstraint
import qualified Language.Haskell.Synthesis.Declaration as SharedDeclaration
import qualified Language.Haskell.Synthesis.Environment as SharedEnvironment
import qualified Language.Haskell.Synthesis.KindInference as SharedKindInference
import qualified Language.Haskell.Synthesis.Inventory as SharedInventory
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Type as SharedType

import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.Core.Name
import Language.Haskell.Exference.Core.Score (Penalty)
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

data SynthesisDeclarationError
  = DeclarationTypeConversionError SynthesisTypeError
  | DeclarationNameConversionError QualifiedNameError
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
  deriving (Eq, Show)

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
  (toSynthesisName $ functionName binding)
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
      name <- convertedName $ SharedDeclaration.valueName signature
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
    (toSynthesisName $ tclass_name declaration)
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
      | null methods -> HsTypeClass <$> convertedName name
          <*> mapM plainFlexibleParameter parameters
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
      typeClass <- HsTypeClass <$> convertedName name
        <*> mapM plainFlexibleParameter parameters
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
  TypeForall variables constraints body ->
    TypeForall variables (constraint : constraints) body
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
    (toSynthesisName name)
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
      convertedTypeName <- convertedName name
      variables <- mapM plainFlexibleParameter parameters
      convertedConstructors <- mapM loweredConstructor constructors
      let input = foldl TypeApp (TypeCons convertedTypeName)
            $ map TypeVar variables
      Right $ DeconstructorBinding input convertedConstructors recursive
    _ -> Left ExpectedDataDeclaration

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
loweredType typeExpression = either
  (Left . DeclarationTypeConversionError) Right
  $ fromSynthesisType typeExpression

constraintVariables
  :: [SharedConstraint.Constraint (SharedType.Type SynthesisVariable)]
  -> Set.Set SynthesisVariable
constraintVariables = foldMap (foldMap SharedType.freeVariables)

isFlexibleVariable :: SynthesisVariable -> Bool
isFlexibleVariable SharedType.FlexibleVariable{} = True
isFlexibleVariable SharedType.RigidVariable{} = False

convertedName
  :: SharedName.Name
  -> Either SynthesisDeclarationError QualifiedName
convertedName = either (Left . DeclarationNameConversionError) Right
  . fromSynthesisName

convertedConstraint
  :: HsConstraint
  -> Either SynthesisDeclarationError
      (SharedConstraint.Constraint (SharedType.Type SynthesisVariable))
convertedConstraint (HsConstraint className arguments) =
  SharedConstraint.Constraint (toSynthesisName className)
    <$> mapM convertedType arguments

loweredConstraint
  :: SharedConstraint.Constraint (SharedType.Type SynthesisVariable)
  -> Either SynthesisDeclarationError HsConstraint
loweredConstraint (SharedConstraint.Constraint className arguments) =
  HsConstraint <$> convertedName className <*> mapM loweredType arguments

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
  <*> pure (toSynthesisName $ constructorName constructor)
  <*> mapM convertedType (constructorFields constructor)

loweredConstructor
  :: SharedDeclaration.DataConstructor
      SynthesisVariable DeclarationMetadata
  -> Either SynthesisDeclarationError ConstructorBinding
loweredConstructor constructor = ConstructorBinding
  <$> convertedName (SharedDeclaration.constructorName constructor)
  <*> mapM loweredType (SharedDeclaration.constructorFields constructor)

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
  let (headType, arguments) = typeApplicationSpine body
  case headType of
    TypeCons name -> do
      parameters <- mapM typeVariable arguments
      if null binders || Set.fromList binders == Set.fromList parameters
        then Right (name, parameters)
        else Left $ DeconstructorForallMismatch binders parameters
    _ -> Left $ InvalidDeconstructorHead typeExpression
 where
  stripForalls binders (TypeForall variables [] body) =
    stripForalls (binders ++ variables) body
  stripForalls _ TypeForall{} = Left $ InvalidDeconstructorHead typeExpression
  stripForalls binders body = Right (binders, body)

  typeVariable (TypeVar variable) = Right variable
  typeVariable argument = Left $ NonVariableDataParameter argument

typeApplicationSpine :: HsType -> (HsType, [HsType])
typeApplicationSpine = collect []
  where
    collect arguments (TypeApp function argument) =
      collect (argument : arguments) function
    collect arguments function = (function, arguments)

splitFunctionType :: HsType -> ([HsType], HsType)
splitFunctionType (TypeArrow parameter result) =
  let (parameters, finalResult) = splitFunctionType result
  in (parameter : parameters, finalResult)
splitFunctionType result = ([], result)
