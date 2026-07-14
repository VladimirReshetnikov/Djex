-- | Private ownership of the parser-neutral Exference session invariant.
--
-- A session retains only parser-independent state.  The HSE compatibility
-- bridge and the neutral Djex constructor both prepare the same inventory,
-- synonym table, and core dictionary before policy is applied here.
module Language.Haskell.Djex.Exference.Internal.Session
  ( ExferenceSession
  , ExferenceOmission (..)
  , ExferenceOmissionCapability (..)
  , ExferenceOmissionReason (..)
  , sealNeutralExferenceSessionWithPolicy
  , sealPreparedExferenceSessionWithPolicy
  , sessionSearchEnvironment
  , sessionTypeSynonyms
  , exferenceSessionInventory
  , sessionOmissions
  ) where

import Data.Bifunctor (first)
import Data.List (partition)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Void (Void)

import Language.Haskell.Exference.Core (mkExferenceEnvironment)
import qualified Language.Haskell.Exference.Core as Core
import Language.Haskell.Exference.Core.Declaration
  ( PreparedNeutralSynthesisInventory
  , prepareNeutralSynthesisInventory
  , preparedNeutralBackend
  , preparedNeutralInventory
  , preparedNeutralTypeSynonyms
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
  ( SynthesisVariable
  , constraint_params
  )
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , shownErrorDiagnostic
  )
import Language.Haskell.Synthesis.Environment (Environment)
import Language.Haskell.Synthesis.Inventory
  ( Inventory
  , mkInventoryFromEnvironmentWithClassPolicy
  )
import Language.Haskell.Synthesis.KindInference
  ( ClassKindPolicy (GeneralizeClassKinds)
  , KindInventoryPolicy (OpenKindInventory)
  )
import Language.Haskell.Synthesis.Name
  ( Name
  , renderCanonical
  )
import Language.Haskell.Synthesis.TypeSynonym
  ( TypeSynonyms
  )

data ExferenceOmissionCapability
  = BindingIntroduction
  | DataElimination
  deriving (Eq, Ord, Show)

data ExferenceOmissionReason
  = UnsupportedNestedForall
  | RecursiveDataEliminationUnsupported
  | ExcludedByPolicy
  deriving (Eq, Ord, Show)

data ExferenceOmission = ExferenceOmission
  { omittedName :: Name
  , omittedCapability :: ExferenceOmissionCapability
  , omittedReason :: ExferenceOmissionReason
  }
  deriving (Eq, Ord, Show)

-- | All reusable session state is independent of the parser that supplied the
-- declarations.  The prepared inventory is the authority for declarations,
-- synonyms, and the unfiltered backend projection; retaining its individual
-- views here as well would allow those immutable values to drift apart.
data ExferenceSession = ExferenceSession
  { searchView :: Core.ExferenceEnvironment
  , preparedView :: PreparedNeutralSynthesisInventory
  , omissionView :: [ExferenceOmission]
  }

type NeutralEnvironment = Environment SynthesisVariable Void ()

sealNeutralExferenceSessionWithPolicy
  :: [Name]
  -> Map Name Penalty
  -> NeutralEnvironment
  -> Either Diagnostic ExferenceSession
sealNeutralExferenceSessionWithPolicy exclusions overrides environment = do
  inventory <- first
    (preparationFailure "cannot validate the neutral Exference inventory")
    $ mkInventoryFromEnvironmentWithClassPolicy
        OpenKindInventory GeneralizeClassKinds environment
  prepared <- first
    (preparationFailure "cannot prepare the neutral Exference environment")
    $ prepareNeutralSynthesisInventory inventory
  sealPreparedEnvironment exclusions overrides prepared

-- | Seal a checked source projection without retaining its parser-specific
-- representation. The opaque prepared inventory proves that the synonym
-- table and rated, ordered backend are projections of the same neutral
-- inventory; exposed source-frontend seams cannot recombine those views.
sealPreparedExferenceSessionWithPolicy
  :: [Name]
  -> Map Name Penalty
  -> PreparedNeutralSynthesisInventory
  -> Either Diagnostic ExferenceSession
sealPreparedExferenceSessionWithPolicy = sealPreparedEnvironment

sealPreparedEnvironment
  :: [Name]
  -> Map Name Penalty
  -> PreparedNeutralSynthesisInventory
  -> Either Diagnostic ExferenceSession
sealPreparedEnvironment exclusions overrides prepared = do
  let backend = preparedNeutralBackend prepared
  ratedFunctions <- applyRatingOverrides overrides
    $ environmentFunctions backend
  let excludedBindings = Set.fromList exclusions
      functionExcluded binding = Set.member
        (functionName binding) excludedBindings
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
        [ ExferenceOmission
            (functionName binding)
            BindingIntroduction
            reason
        | binding <- ratedFunctions
        , reason <- if functionExcluded binding
            then [ExcludedByPolicy]
            else [UnsupportedNestedForall | not $ functionSupported binding]
        ] ++ mapMaybe deconstructorOmission omittedDeconstructors
  searchEnvironment <- first
    (shownErrorDiagnostic "DJEX_EXF_ENV"
      "cannot seal the Exference session environment")
    $ mkExferenceEnvironment supportedBackend
  pure ExferenceSession
    { searchView = searchEnvironment
    , preparedView = prepared
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
  let available = Set.fromList $ map functionName bindings
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
        (functionName binding)
        overrides
    }

sessionSearchEnvironment :: ExferenceSession -> Core.ExferenceEnvironment
sessionSearchEnvironment = searchView

sessionTypeSynonyms
  :: ExferenceSession
  -> TypeSynonyms SynthesisVariable
sessionTypeSynonyms = preparedNeutralTypeSynonyms . preparedView

exferenceSessionInventory
  :: ExferenceSession
  -> Inventory SynthesisVariable ()
exferenceSessionInventory = preparedNeutralInventory . preparedView

sessionOmissions :: ExferenceSession -> [ExferenceOmission]
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

deconstructorOmission :: DeconstructorBinding -> Maybe ExferenceOmission
deconstructorOmission binding = do
  name <- typeConstructorHead $ deconstructorInput binding
  pure $ ExferenceOmission
    name
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
preparationFailure = shownErrorDiagnostic "DJEX_EXF_ENV"

policyFailure
  :: Show detail
  => String
  -> detail
  -> Diagnostic
policyFailure = shownErrorDiagnostic "DJEX_EXF_POLICY_RATING"
