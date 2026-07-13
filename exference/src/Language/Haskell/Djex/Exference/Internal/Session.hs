-- | Private ownership of the checked Exference session invariant.
--
-- A session retains only parser-independent state.  The HSE compatibility
-- bridge and the neutral Djex constructor both prepare the same inventory,
-- synonym table, and core dictionary before policy is applied here.
module Language.Haskell.Djex.Exference.Internal.Session
  ( ExferenceSession
  , SessionOmission (..)
  , SessionOmissionCapability (..)
  , SessionOmissionReason (..)
  , sealNeutralExferenceSession
  , sealNeutralExferenceSessionWithPolicy
  , sealProjectedExferenceSessionWithPolicy
  , sessionSearchEnvironment
  , sessionTypeSynonyms
  , sessionTypeNames
  , sessionClasses
  , exferenceSessionInventory
  , sessionOmissions
  ) where

import Data.Bifunctor (first)
import Data.List (partition)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Void (Void, absurd)

import Language.Haskell.Exference.Core
  ( ExferenceInputError
  , mkExferenceEnvironment
  )
import qualified Language.Haskell.Exference.Core as Core
import Language.Haskell.Exference.Core.Declaration
  ( freshSynthesisVariable
  , prepareNeutralSynthesisInventory
  )
import Language.Haskell.Exference.Core.FunctionBinding
  ( ConstructorBinding (constructorFields)
  , DeconstructorBinding (..)
  , EnvDictionary (..)
  , FunctionBinding (..)
  )
import Language.Haskell.Exference.Core.Score
  ( Penalty
  , isFiniteScore
  )
import Language.Haskell.Exference.Core.TypeUtils
  ( containsForall
  , typeConstructorHead
  )
import Language.Haskell.Exference.Core.Types
  ( HsTypeClass
  , QualifiedName
  , SynthesisVariable
  , constraint_params
  , fromSynthesisName
  , sClassEnv_tclasses
  , toSynthesisName
  )
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error)
  , diagnostic
  , withCode
  , withContext
  )
import qualified Language.Haskell.Synthesis.Environment as SharedEnvironment
import Language.Haskell.Synthesis.Environment (Environment)
import Language.Haskell.Synthesis.Inventory
  ( Inventory
  , InventoryError (..)
  , inventoryEnvironment
  , mkInventory
  )
import Language.Haskell.Synthesis.KindInference
  ( KindInventoryPolicy (OpenKindInventory) )
import Language.Haskell.Synthesis.Name
  ( Name
  , renderCanonical
  )
import Language.Haskell.Synthesis.TypeSynonym
  ( TypeSynonyms
  , prepareTypeSynonyms
  )

data SessionOmissionCapability
  = BindingIntroduction
  | DataElimination
  deriving (Eq, Ord, Show)

data SessionOmissionReason
  = UnsupportedNestedForall
  | RecursiveDataEliminationUnsupported
  | ExcludedByPolicy
  deriving (Eq, Ord, Show)

data SessionOmission = SessionOmission
  { sessionOmissionName :: Name
  , sessionOmissionCapability :: SessionOmissionCapability
  , sessionOmissionReason :: SessionOmissionReason
  }
  deriving (Eq, Ord, Show)

-- | All reusable session state is independent of the parser that supplied the
-- declarations.  The legacy name and class indexes contain Exference core
-- values, not HSE syntax; they are retained only to elaborate Haskell type
-- text without reconstructing them for every request.
data ExferenceSession = ExferenceSession
  { searchView :: Core.ExferenceEnvironment
  , inventoryView :: Inventory SynthesisVariable ()
  , synonymView :: TypeSynonyms SynthesisVariable
  , typeNameView :: [QualifiedName]
  , classView :: Map QualifiedName HsTypeClass
  , omissionView :: [SessionOmission]
  }

type NeutralEnvironment = Environment SynthesisVariable Void ()

sealNeutralExferenceSession
  :: NeutralEnvironment
  -> Either Diagnostic ExferenceSession
sealNeutralExferenceSession = sealNeutralExferenceSessionWithPolicy
  [] Map.empty

sealNeutralExferenceSessionWithPolicy
  :: [Name]
  -> Map Name Penalty
  -> NeutralEnvironment
  -> Either Diagnostic ExferenceSession
sealNeutralExferenceSessionWithPolicy exclusions overrides environment = do
  inventory <- case mkInventory OpenKindInventory
      $ SharedEnvironment.environmentDeclarations environment of
    Left (UngroundedInventoryKind impossible) -> absurd impossible
    Left failure -> Left $ preparationFailure
      "cannot validate the neutral Exference inventory" failure
    Right value -> Right value
  (synonyms, backend) <- first
    (preparationFailure "cannot prepare the neutral Exference environment")
    $ prepareNeutralSynthesisInventory inventory
  sealPreparedEnvironment exclusions overrides inventory synonyms backend

-- | Seal a checked parser projection without retaining its source-specific
-- representation. The caller supplies the authoritative neutral inventory
-- together with the rated backend dictionary whose order controls equal-cost
-- search. This is the deliberately narrow seam used by source frontends.
sealProjectedExferenceSessionWithPolicy
  :: [Name]
  -> Map Name Penalty
  -> Inventory SynthesisVariable ()
  -> EnvDictionary
  -> Either Diagnostic ExferenceSession
sealProjectedExferenceSessionWithPolicy
    exclusions overrides inventory backend = do
  synonyms <- prepareSynonyms inventory
  sealPreparedEnvironment exclusions overrides inventory synonyms backend

prepareSynonyms
  :: Inventory SynthesisVariable ()
  -> Either Diagnostic (TypeSynonyms SynthesisVariable)
prepareSynonyms = first
  (preparationFailure "cannot prepare Exference type synonyms")
  . prepareTypeSynonyms freshSynthesisVariable

sealPreparedEnvironment
  :: [Name]
  -> Map Name Penalty
  -> Inventory SynthesisVariable ()
  -> TypeSynonyms SynthesisVariable
  -> EnvDictionary
  -> Either Diagnostic ExferenceSession
sealPreparedEnvironment exclusions overrides inventory synonyms backend = do
  ratedFunctions <- applyRatingOverrides overrides
    $ environmentFunctions backend
  let excludedBindings = Set.fromList exclusions
      functionExcluded binding = Set.member
        (toSynthesisName $ functionName binding) excludedBindings
      supportedFunctions =
        [ binding
        | binding <- ratedFunctions
        , not $ functionExcluded binding
        , functionSupported binding
        ]
      deconstructors = environmentDeconstructors backend
      (supportedDeconstructors, omittedDeconstructors) =
        partition deconstructorSupported deconstructors
      supportedBackend = backend
        { environmentFunctions = supportedFunctions
        , environmentDeconstructors = supportedDeconstructors
        }
      omissions =
        [ SessionOmission
            (toSynthesisName $ functionName binding)
            BindingIntroduction
            reason
        | binding <- ratedFunctions
        , reason <- if functionExcluded binding
            then [ExcludedByPolicy]
            else [UnsupportedNestedForall | not $ functionSupported binding]
        ] ++ mapMaybe deconstructorOmission omittedDeconstructors
  searchEnvironment <- first sessionFailureDiagnostic
    $ mkExferenceEnvironment supportedBackend
  typeNames <- first
    (preparationFailure "cannot lower Exference's parser type-name index")
    $ traverse fromSynthesisName
    $ Map.keys
    $ SharedEnvironment.typeDeclarationMap
    $ inventoryEnvironment inventory
  pure ExferenceSession
    { searchView = searchEnvironment
    , inventoryView = inventory
    , synonymView = synonyms
    , typeNameView = typeNames
    , classView = sClassEnv_tclasses $ environmentClasses backend
    , omissionView = omissions
    }

applyRatingOverrides
  :: Map Name Penalty
  -> [FunctionBinding]
  -> Either Diagnostic [FunctionBinding]
applyRatingOverrides overrides bindings = do
  case
      [ (name, penalty)
      | (name, penalty) <- Map.toAscList overrides
      , not $ isFiniteScore penalty
      ] of
    [] -> pure ()
    invalid -> Left $ policyFailure
      "Exference rating overrides must be finite" invalid
  let available = Set.fromList
        $ map (toSynthesisName . functionName) bindings
      unknown = Map.keysSet overrides Set.\\ available
  if Set.null unknown
    then pure ()
    else Left $ policyFailure
      "Exference rating overrides name unavailable bindings"
      $ map renderCanonical $ Set.toAscList unknown
  pure $ map applyOverride bindings
 where
  applyOverride binding = binding
    { functionPenalty = Map.findWithDefault
        (functionPenalty binding)
        (toSynthesisName $ functionName binding)
        overrides
    }

sessionSearchEnvironment :: ExferenceSession -> Core.ExferenceEnvironment
sessionSearchEnvironment = searchView

sessionTypeSynonyms
  :: ExferenceSession
  -> TypeSynonyms SynthesisVariable
sessionTypeSynonyms = synonymView

sessionTypeNames :: ExferenceSession -> [QualifiedName]
sessionTypeNames = typeNameView

sessionClasses :: ExferenceSession -> Map QualifiedName HsTypeClass
sessionClasses = classView

exferenceSessionInventory
  :: ExferenceSession
  -> Inventory SynthesisVariable ()
exferenceSessionInventory = inventoryView

sessionOmissions :: ExferenceSession -> [SessionOmission]
sessionOmissions = omissionView

functionSupported :: FunctionBinding -> Bool
functionSupported binding = all (not . containsForall)
  $ functionResult binding
  : functionParameters binding
  ++ concatMap constraint_params (functionConstraints binding)

deconstructorSupported :: DeconstructorBinding -> Bool
deconstructorSupported binding = not (deconstructorRecursive binding)
  && all (not . containsForall)
      (deconstructorInput binding
        : concatMap constructorFields (deconstructorConstructors binding))

deconstructorOmission :: DeconstructorBinding -> Maybe SessionOmission
deconstructorOmission binding = do
  name <- typeConstructorHead $ deconstructorInput binding
  pure $ SessionOmission
    (toSynthesisName name)
    DataElimination
    reason
 where
  reason
    | deconstructorRecursive binding = RecursiveDataEliminationUnsupported
    | otherwise = UnsupportedNestedForall

preparationFailure
  :: Show detail
  => String
  -> detail
  -> Diagnostic
preparationFailure message detail = withContext (show detail)
  $ withCode "DJEX_EXF_ENV"
  $ diagnostic Error message

policyFailure
  :: Show detail
  => String
  -> detail
  -> Diagnostic
policyFailure message detail = withContext (show detail)
  $ withCode "DJEX_EXF_POLICY_RATING"
  $ diagnostic Error message

sessionFailureDiagnostic :: ExferenceInputError -> Diagnostic
sessionFailureDiagnostic detail = withContext (show detail)
  $ withCode "DJEX_EXF_ENV"
  $ diagnostic Error "cannot seal the Exference session environment"
