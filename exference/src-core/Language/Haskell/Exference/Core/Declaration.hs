{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

-- | Checked adapters between Exference's search-oriented declaration records
-- and the shared source-declaration vocabulary.
module Language.Haskell.Exference.Core.Declaration
  ( DeclarationMetadata (..)
  , SynthesisDeclaration
  , SynthesisEnvironment
  , SynthesisDeclarationError (..)
  , toSynthesisFunctionBinding
  , fromSynthesisFunctionBinding
  , toSynthesisClassDeclaration
  , fromSynthesisClassDeclaration
  , toSynthesisInstanceDeclaration
  , fromSynthesisInstanceDeclaration
  , toSynthesisDataDeclaration
  , fromSynthesisDataDeclaration
  , toSynthesisEnvironment
  , fromSynthesisEnvironment
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)
import qualified Language.Haskell.Synthesis.Constraint as SharedConstraint
import qualified Language.Haskell.Synthesis.Declaration as SharedDeclaration
import qualified Language.Haskell.Synthesis.Environment as SharedEnvironment
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Type as SharedType

import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.Core.Name
import Language.Haskell.Exference.Core.Score (Penalty)
import Language.Haskell.Exference.Core.Types

data DeclarationMetadata
  = NoDeclarationMetadata
  | FunctionPenaltyMetadata Penalty
  | RecursiveDataMetadata Bool
  deriving (Eq, Show, Generic)

instance NFData DeclarationMetadata

type SynthesisDeclaration = SharedDeclaration.Declaration
  SynthesisVariable Int DeclarationMetadata

type SynthesisEnvironment = SharedEnvironment.Environment
  SynthesisVariable Int DeclarationMetadata

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
  | MissingFunctionPenaltyMetadata
  | MissingRecursiveDataMetadata
  | ExplicitFunctionForallUnsupported [TVarId]
  | NonImplicitInstanceForall [SynthesisVariable]
  | ClassMethodsUnsupported [SharedName.Name]
  | ExplicitParameterKindUnsupported SynthesisVariable
  | InvalidDeconstructorHead HsType
  | NonVariableDataParameter HsType
  | RigidDataParameter TVarId
  | DeconstructorForallMismatch [TVarId] [TVarId]
  | InvalidSharedEnvironment
      (SharedEnvironment.EnvironmentError SynthesisVariable)
  | ClassEnvironmentConversionError ClassEnvError
  | UnsupportedCoreEnvironmentDeclaration SharedName.Name
  | OrphanConstructorBinding SharedName.Name
  deriving (Eq, Show)

toSynthesisFunctionBinding
  :: FunctionBinding
  -> Either SynthesisDeclarationError SynthesisDeclaration
toSynthesisFunctionBinding binding = checked $
  SharedDeclaration.ValueDeclaration
    <$> (SharedDeclaration.ValueSignature
        (FunctionPenaltyMetadata $ functionPenalty binding)
        (toSynthesisName $ functionName binding)
    <$> convertedType (TypeForall [] (functionConstraints binding)
          $ foldr TypeArrow (functionResult binding)
          $ functionParameters binding))

fromSynthesisFunctionBinding
  :: SynthesisDeclaration
  -> Either SynthesisDeclarationError FunctionBinding
fromSynthesisFunctionBinding declaration = do
  validateShared declaration
  case declaration of
    SharedDeclaration.ValueDeclaration signature -> do
      penalty <- case SharedDeclaration.valueAnnotation signature of
        FunctionPenaltyMetadata value -> Right value
        _ -> Left MissingFunctionPenaltyMetadata
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
toSynthesisClassDeclaration declaration = checked $
  SharedDeclaration.ClassDeclaration NoDeclarationMetadata
    (toSynthesisName $ tclass_name declaration)
    (map flexibleParameter $ tclass_params declaration)
    <$> mapM convertedConstraint (tclass_constraints declaration)
    <*> pure []

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
toSynthesisDataDeclaration declaration = do
  (name, parameters) <- deconstructorHead $ deconstructorInput declaration
  checked $ SharedDeclaration.DataTypeDeclaration
    (RecursiveDataMetadata $ deconstructorRecursive declaration)
    (toSynthesisName name)
    (map flexibleParameter parameters)
    <$> mapM convertedConstructor (deconstructorConstructors declaration)

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

toSynthesisEnvironment
  :: EnvDictionary
  -> Either SynthesisDeclarationError SynthesisEnvironment
toSynthesisEnvironment environment = do
  declarations <- sequence $
    map toSynthesisFunctionBinding (environmentFunctions environment) ++
    map toSynthesisDataDeclaration (environmentDeconstructors environment) ++
    map toSynthesisClassDeclaration
      (Map.elems $ sClassEnv_tclasses $ environmentClasses environment) ++
    map toSynthesisInstanceDeclaration
      (sClassEnv_explicitInstances $ environmentClasses environment)
  either (Left . InvalidSharedEnvironment) Right
    $ SharedEnvironment.mkEnvironment declarations

fromSynthesisEnvironment
  :: SynthesisEnvironment
  -> Either SynthesisDeclarationError EnvDictionary
fromSynthesisEnvironment environment = do
  (functions, deconstructors, classes, instances) <- foldM collect
    ([], [], [], []) $ SharedEnvironment.environmentDeclarations environment
  classEnvironment <- either (Left . ClassEnvironmentConversionError) Right
    $ mkStaticClassEnv (reverse classes) (reverse instances)
  Right $ EnvDictionary
    (reverse functions) (reverse deconstructors) classEnvironment
 where
  collect (functions, deconstructors, classes, instances) declaration =
    case declaration of
      SharedDeclaration.ValueDeclaration{} -> do
        binding <- fromSynthesisFunctionBinding declaration
        Right (binding : functions, deconstructors, classes, instances)
      SharedDeclaration.DataTypeDeclaration{} -> do
        deconstructor <- fromSynthesisDataDeclaration declaration
        Right (functions, deconstructor : deconstructors, classes, instances)
      SharedDeclaration.ClassDeclaration{} -> do
        classDeclaration <- fromSynthesisClassDeclaration declaration
        Right (functions, deconstructors,
          classDeclaration : classes, instances)
      SharedDeclaration.InstanceDeclaration{} -> do
        instanceDeclaration <- fromSynthesisInstanceDeclaration declaration
        Right (functions, deconstructors, classes,
          instanceDeclaration : instances)
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
  -> SharedDeclaration.TypeParameter SynthesisVariable Int
flexibleParameter variable = SharedDeclaration.TypeParameter
  (SharedType.FlexibleVariable variable) Nothing

plainFlexibleParameter
  :: SharedDeclaration.TypeParameter SynthesisVariable Int
  -> Either SynthesisDeclarationError TVarId
plainFlexibleParameter parameter = case
    SharedDeclaration.parameterKind parameter of
  Just _ -> Left $ ExplicitParameterKindUnsupported
    $ SharedDeclaration.parameterVariable parameter
  Nothing -> case SharedDeclaration.parameterVariable parameter of
    SharedType.FlexibleVariable variable -> Right variable
    SharedType.RigidVariable variable -> Left $ RigidDataParameter variable

convertedConstructor
  :: ConstructorBinding
  -> Either SynthesisDeclarationError
      (SharedDeclaration.DataConstructor
        SynthesisVariable DeclarationMetadata)
convertedConstructor constructor = SharedDeclaration.DataConstructor
  NoDeclarationMetadata
  (toSynthesisName $ constructorName constructor)
  <$> mapM convertedType (constructorFields constructor)

loweredConstructor
  :: SharedDeclaration.DataConstructor
      SynthesisVariable DeclarationMetadata
  -> Either SynthesisDeclarationError ConstructorBinding
loweredConstructor constructor = ConstructorBinding
  <$> convertedName (SharedDeclaration.constructorName constructor)
  <*> mapM loweredType (SharedDeclaration.constructorFields constructor)

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
