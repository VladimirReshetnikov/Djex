{-# LANGUAGE DeriveGeneric #-}

module Language.Haskell.Exference.Core.FunctionBinding
  ( ConstructorBinding (..)
  , DeconstructorBinding (..)
  , DeconstructorValidationError (..)
  , EnvironmentDuplicateError (..)
  , EnvironmentRatingError (..)
  , EnvironmentSyntaxError (..)
  , EnvDictionary (..)
  , FunctionBinding (..)
  , functionBindingFromType
  , functionBindingType
  , functionBindingSignature
  , functionBindingTypes
  , deconstructorBindingType
  , deconstructorBindingTypes
  , environmentBindingTypes
  , environmentConstraints
  , mapFunctionBindingTypes
  , mapDeconstructorBindingTypes
  , validateEnvironmentBindingIdentities
  , validateEnvironmentBindingRatings
  , validateEnvironmentBindingSyntax
  , validateDeconstructorBinding
  )
where

import Control.DeepSeq (NFData (..))
import Data.Foldable (traverse_)
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import GHC.Generics (Generic)

import Language.Haskell.Exference.Core.Internal.FlexibleIds
  ( flexibleIdentifiers )
import Language.Haskell.Exference.Core.Score
import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
  ( splitArrowResultParams
  , typeConstructorHead
  )
import qualified Language.Haskell.Synthesis.Collection as SharedCollection
import qualified Language.Haskell.Synthesis.Generated as SharedGenerated
import qualified Language.Haskell.Synthesis.Name as SynthesisName
import qualified Language.Haskell.Synthesis.Type as SharedType

data FunctionBinding = FunctionBinding
  { functionResult :: HsType
  , functionName :: QualifiedName
  , functionPenalty :: Penalty
  , functionConstraints :: [HsConstraint]
  , functionParameters :: [HsType]
  }
  deriving (Eq, Generic, Show)

instance NFData FunctionBinding

-- | Lower one fully quantified source signature into the search engine's
-- flat binding shape. Only its complete leading prenex chain is opened;
-- a forall below an arrow remains visible in 'functionResult' for the checked
-- environment boundary to reject. Quantifier IDs are local to the signature;
-- cross-layer shadows are alpha-normalized before their binders disappear.
-- Leading constraints and arrow parameters remain explicit search inputs,
-- while the caller-owned penalty is attached without changing the remaining
-- type structure.
functionBindingFromType
  :: QualifiedName
  -> Penalty
  -> HsType
  -> FunctionBinding
functionBindingFromType name penalty signature = FunctionBinding
  result name penalty constraints parameters
 where
  (result, parameters, _, constraints) = splitArrowResultParams signature

-- | Reconstruct the monotype consumed when applying a search binding.
functionBindingType :: FunctionBinding -> HsType
functionBindingType binding = SharedType.functionType
  (functionParameters binding) (functionResult binding)

-- | Reconstruct the complete implicitly quantified source signature retained
-- by the flat search record. Leading binder identities are intentionally not
-- recovered: they were opened by 'functionBindingFromType', while their free
-- occurrences remain available to the shared declaration checker.
functionBindingSignature :: FunctionBinding -> HsType
functionBindingSignature binding = TypeForall []
  (functionConstraints binding) (functionBindingType binding)

-- | Every independently stored type in a function binding, in historical
-- result, parameter, then constraint-argument order.
functionBindingTypes :: FunctionBinding -> [HsType]
functionBindingTypes binding = functionResult binding
  : functionParameters binding
  ++ concatMap constraint_params (functionConstraints binding)

-- | Transform every independently stored type in a function binding exactly
-- once. This includes types nested in the separately stored constraints.
mapFunctionBindingTypes
  :: (HsType -> HsType)
  -> FunctionBinding
  -> FunctionBinding
mapFunctionBindingTypes transform binding = binding
  { functionResult = transform $ functionResult binding
  , functionConstraints = map (fmap transform)
      $ functionConstraints binding
  , functionParameters = map transform $ functionParameters binding
  }

data ConstructorBinding = ConstructorBinding
  { constructorName :: QualifiedName
  , constructorFields :: [HsType]
  }
  deriving (Eq, Generic, Show)

instance NFData ConstructorBinding

data DeconstructorBinding = DeconstructorBinding
  { deconstructorInput :: HsType
  , deconstructorConstructors :: [ConstructorBinding]
  , deconstructorRecursive :: Bool
  }
  deriving (Eq, Generic, Show)

instance NFData DeconstructorBinding

-- | Structural failures that would let a deconstructor manufacture a pattern
-- match for an unrelated nominal type or introduce undeclared existential
-- field variables.
data DeconstructorValidationError
  = MissingDeconstructorNominalHead HsType
  | FunctionDeconstructorHead QualifiedName
  | UnboundDeconstructorFields
      QualifiedName -- ^ Nominal datatype head.
      QualifiedName -- ^ Constructor whose fields escape the parameter scope.
      [TVarId]      -- ^ Escaping flexible IDs, in ascending order.
  deriving (Eq, Generic, Show)

instance NFData DeconstructorValidationError

-- | Validate the elimination invariant shared by search-environment sealing
-- and independent expression checking.
validateDeconstructorBinding
  :: DeconstructorBinding
  -> Either DeconstructorValidationError ()
validateDeconstructorBinding binding = do
  headName <- case typeConstructorHead input of
    Nothing -> Left $ MissingDeconstructorNominalHead input
    Just name
      | SynthesisName.nameSpecial name
          == Just SynthesisName.FunctionConstructor ->
            Left $ FunctionDeconstructorHead name
      | otherwise -> Right name
  traverse_ (validateConstructor headName parameters)
    $ deconstructorConstructors binding
 where
  input = deconstructorInput binding
  parameters = flexibleIdentifiers input

  validateConstructor headName parameters' constructor
    | IntSet.null unbound = Right ()
    | otherwise = Left $ UnboundDeconstructorFields
        headName (constructorName constructor) $ IntSet.toAscList unbound
   where
    unbound = IntSet.unions
      (map flexibleIdentifiers $ constructorFields constructor)
      `IntSet.difference` parameters'

-- | Reconstruct the synthetic elimination type used by validation. Fields of
-- every constructor precede the datatype result in declaration order.
deconstructorBindingType :: DeconstructorBinding -> HsType
deconstructorBindingType binding = SharedType.functionType
  (concatMap constructorFields $ deconstructorConstructors binding)
  (deconstructorInput binding)

-- | Every independently stored type in a deconstructor binding.
deconstructorBindingTypes :: DeconstructorBinding -> [HsType]
deconstructorBindingTypes binding = deconstructorInput binding
  : [ field
    | constructor <- deconstructorConstructors binding
    , field <- constructorFields constructor
    ]

-- | Transform the datatype input and every constructor field exactly once.
mapDeconstructorBindingTypes
  :: (HsType -> HsType)
  -> DeconstructorBinding
  -> DeconstructorBinding
mapDeconstructorBindingTypes transform binding = binding
  { deconstructorInput = transform $ deconstructorInput binding
  , deconstructorConstructors = map transformConstructor
      $ deconstructorConstructors binding
  }
 where
  transformConstructor constructor = constructor
    { constructorFields = map transform $ constructorFields constructor }

data EnvDictionary = EnvDictionary
  { environmentFunctions :: [FunctionBinding]
  , environmentDeconstructors :: [DeconstructorBinding]
  , environmentClasses :: StaticClassEnv
  }
  deriving (Generic, Show)

instance NFData EnvDictionary

-- | An ambiguous identity in a raw search/checking environment.
--
-- Search and independent expression checking both look bindings up by name.
-- Keeping the complete duplicate policy here prevents either consumer from
-- silently choosing a different declaration when handed the same raw lists.
data EnvironmentDuplicateError
  = DuplicateDeconstructorIdentities [QualifiedName]
    -- ^ More than one datatype eliminator has the same nominal input head.
  | DuplicateConstructorIdentities [QualifiedName]
    -- ^ A constructor name occurs in more than one alternative or datatype.
  | DuplicateFunctionIdentities [QualifiedName]
    -- ^ More than one value binding has the same global name.
  deriving (Eq, Generic, Show)

instance NFData EnvironmentDuplicateError

-- | A global name whose generated expression or pattern cannot be emitted as
-- valid Haskell. Keeping this policy beside t'EnvDictionary' makes raw checker
-- inputs obey the same lexical contract as search-environment sealing.
data EnvironmentSyntaxError
  = InvalidFunctionBindingSyntax
      QualifiedName SharedGenerated.RenderError
  | InvalidConstructorBindingSyntax
      QualifiedName SharedGenerated.RenderError
  deriving (Eq, Generic, Show)

instance NFData EnvironmentSyntaxError

-- | A non-finite search rating attached to a raw function capability.
--
-- Function ratings are deliberately signed: negative values are bonuses.
-- Only NaN and infinities are invalid, because either would destroy the
-- total ordering required by the search queue.
data EnvironmentRatingError
  = NonFiniteFunctionRating QualifiedName Penalty
  deriving (Eq, Generic, Show)

instance NFData EnvironmentRatingError

-- | Require every name used for environment lookup to be unambiguous.
--
-- The ordering is part of the checked-boundary contract: datatype heads are
-- considered before constructor names and functions, and each reported list
-- is sorted. Deconstructors without a nominal head are left for
-- 'validateDeconstructorBinding', which owns the more precise shape error.
validateEnvironmentBindingIdentities
  :: EnvDictionary
  -> Either EnvironmentDuplicateError ()
validateEnvironmentBindingIdentities environment
  | duplicates@(_ : _) <- repeated
      [ name
      | deconstructor <- environmentDeconstructors environment
      , Just name <- [typeConstructorHead $ deconstructorInput deconstructor]
      ] = Left $ DuplicateDeconstructorIdentities duplicates
  | duplicates@(_ : _) <- repeated
      [ constructorName constructor
      | deconstructor <- environmentDeconstructors environment
      , constructor <- deconstructorConstructors deconstructor
      ] = Left $ DuplicateConstructorIdentities duplicates
  | duplicates@(_ : _) <- repeated
      (map functionName $ environmentFunctions environment) =
      Left $ DuplicateFunctionIdentities duplicates
  | otherwise = Right ()
 where
  repeated = Set.toAscList
    . SharedCollection.repeatedValueSet
    . SharedCollection.summarizeDuplicates

-- | Require every raw function rating to be finite, in source order.
validateEnvironmentBindingRatings
  :: EnvDictionary
  -> Either EnvironmentRatingError ()
validateEnvironmentBindingRatings environment = case listToMaybe
    [ NonFiniteFunctionRating (functionName binding)
        (functionPenalty binding)
    | binding <- environmentFunctions environment
    , not $ isFiniteScore $ functionPenalty binding
    ] of
  Just failure -> Left failure
  Nothing -> Right ()

-- | Validate every environment name through the shared generated-syntax
-- boundary. Function names are checked before constructor patterns to retain
-- the live search boundary's established error ordering.
validateEnvironmentBindingSyntax
  :: EnvDictionary
  -> Either EnvironmentSyntaxError ()
validateEnvironmentBindingSyntax environment =
  case firstInvalidFunction of
    Just (name, failure) -> Left
      $ InvalidFunctionBindingSyntax name failure
    Nothing -> case firstInvalidConstructor of
      Just (name, failure) -> Left
        $ InvalidConstructorBindingSyntax name failure
      Nothing -> Right ()
 where
  firstInvalidFunction = listToMaybe
    [ (name, failure)
    | binding <- environmentFunctions environment
    , let name = functionName binding
    , Left failure <- [SharedGenerated.validateExpressionSyntax
        $ SharedGenerated.Global name]
    ]

  firstInvalidConstructor = listToMaybe
    [ (name, failure)
    | deconstructor <- environmentDeconstructors environment
    , constructor <- deconstructorConstructors deconstructor
    , let name = constructorName constructor
          patternProbe = SharedGenerated.Constructor name
            (SharedGenerated.Wildcard <$ constructorFields constructor)
          expressionProbe = SharedGenerated.Lambda [patternProbe]
            $ SharedGenerated.Hole ()
    , Left failure <-
        [SharedGenerated.validateExpressionSyntax expressionProbe]
    ]

-- | Every independently stored search-capability type. Class and instance
-- assumptions are deliberately excluded because they belong to
-- 'StaticClassEnv', not to these binding records.
environmentBindingTypes :: EnvDictionary -> [HsType]
environmentBindingTypes environment =
  concatMap functionBindingTypes (environmentFunctions environment)
  ++ concatMap deconstructorBindingTypes
      (environmentDeconstructors environment)

-- | Every explicit constraint in an environment, paired with the lookup site
-- used by class validation and diagnostics.
--
-- Function constraints are followed by class superclasses and then instance
-- heads/prerequisites. Keeping this projection beside t'EnvDictionary' gives
-- search sealing and independent expression checking the same complete view;
-- neither has to assume that nominal 'StaticClassEnv' validation also enforces
-- its own rank restrictions.
environmentConstraints
  :: EnvDictionary
  -> [(ConstraintSite, HsConstraint)]
environmentConstraints environment =
  [ (BindingConstraint $ functionName binding, constraint)
  | binding <- environmentFunctions environment
  , constraint <- functionConstraints binding
  ] ++
  [ (ClassSuperclass $ tclass_name declaration, constraint)
  | declaration <- Map.elems
      $ sClassEnv_tclasses $ environmentClasses environment
  , constraint <- tclass_constraints declaration
  ] ++ concatMap instanceConstraints
    (sClassEnv_explicitInstances $ environmentClasses environment)
 where
  instanceConstraints instanceDeclaration =
    (InstanceHead, instance_head instanceDeclaration)
    : [ (InstancePrerequisite headName, prerequisite)
      | prerequisite <- instance_constraints instanceDeclaration
      ]
   where
    headName = constraint_tclass $ instance_head instanceDeclaration
