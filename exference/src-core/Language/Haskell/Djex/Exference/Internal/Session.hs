{-# LANGUAGE DeriveGeneric #-}

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
  , elaborateSessionGoal
  , exferenceSessionInventory
  , sessionOmissions
  ) where

import Control.DeepSeq (NFData, deepseq)
import Data.Bifunctor (first)
import Data.List (partition)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Void (Void)
import GHC.Generics (Generic)

import Language.Haskell.Exference.Core (mkExferenceEnvironment)
import qualified Language.Haskell.Exference.Core as Core
import Language.Haskell.Exference.Core.Declaration
  ( PreparedSynthesisInventory
  , freshSynthesisVariable
  , prepareSynthesisInventory
  , preparedSynthesisBackend
  , preparedSynthesisWitness
  )
import Language.Haskell.Exference.Core.FunctionBinding
  ( DeconstructorBinding (..)
  , EnvDictionary (..)
  , FunctionBinding (..)
  , deconstructorBindingTypes
  , functionBindingTypes
  )
import Language.Haskell.Exference.Core.Score
  ( Penalty
  , isFiniteScore
  )
import Language.Haskell.Exference.Core.TypeUtils
  ( containsForall
  , typeConstructorHead
  )
import Language.Haskell.Exference.Core.Types (SynthesisVariable)
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
import Language.Haskell.Synthesis.Kind (Kind (ProperTypeKind))
import Language.Haskell.Synthesis.Name
  ( Name
  , renderCanonical
  )
import Language.Haskell.Synthesis.Type (Type)
import Language.Haskell.Synthesis.TypeSynonym
  ( PreparedInventory
  , TypeElaborationError
  , elaboratePreparedType
  , preparedInventory
  )

-- | The search capability removed for one declaration during session
-- projection.
data ExferenceOmissionCapability
  = BindingIntroduction
    -- ^ Introducing a declared value as part of a generated expression.
  | DataElimination
    -- ^ Eliminating a declared datatype through constructor matching.
  deriving (Eq, Ord, Show, Generic)

instance NFData ExferenceOmissionCapability

-- | Why Exference could not or should not retain a search capability.
data ExferenceOmissionReason
  = UnsupportedNestedForall
    -- ^ The capability's type contains a nested universal quantifier.
  | RecursiveDataEliminationUnsupported
    -- ^ Exference cannot safely eliminate this recursive datatype.
  | ExcludedByPolicy
    -- ^ The session policy explicitly disabled this binding.
  deriving (Eq, Ord, Show, Generic)

instance NFData ExferenceOmissionReason

-- | One declaration capability omitted from the private search projection.
-- The neutral inventory retained by the session is unchanged.
data ExferenceOmission = ExferenceOmission
  { omittedName :: Name
    -- ^ Exact nominal identity of the affected declaration.
  , omittedCapability :: ExferenceOmissionCapability
    -- ^ Search operation that was removed.
  , omittedReason :: ExferenceOmissionReason
    -- ^ Backend limitation or explicit policy decision behind the omission.
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData ExferenceOmission

-- | All reusable session state is independent of the parser that supplied the
-- declarations. The shared prepared inventory remains the authority for
-- declarations and synonyms, while only the policy-filtered search lowering
-- survives sealing. In particular, the complete backend-bearing frontend
-- witness is deliberately not retained here.
data ExferenceSession = ExferenceSession
  { searchView :: Core.ExferenceEnvironment
  , preparedView :: PreparedInventory SynthesisVariable ()
  , omissionView :: [ExferenceOmission]
  }

type CoreEnvironment = Environment SynthesisVariable Void ()

sealNeutralExferenceSessionWithPolicy
  :: [Name]
  -> Map Name Penalty
  -> CoreEnvironment
  -> Either Diagnostic ExferenceSession
sealNeutralExferenceSessionWithPolicy exclusions overrides environment = do
  inventory <- first
    (preparationFailure "cannot validate the neutral Exference inventory")
    $ mkInventoryFromEnvironmentWithClassPolicy
        OpenKindInventory GeneralizeClassKinds environment
  prepared <- first
    (preparationFailure "cannot prepare the neutral Exference environment")
    $ prepareSynthesisInventory inventory
  sealPreparedEnvironment exclusions overrides prepared

-- | Seal a prepared checked-source witness without retaining parser-specific
-- representation. The opaque prepared inventory proves that the synonym
-- table and rated, ordered backend are projections of the same neutral
-- inventory; exposed source-frontend seams cannot recombine those views.
sealPreparedExferenceSessionWithPolicy
  :: [Name]
  -> Map Name Penalty
  -> PreparedSynthesisInventory ()
  -> Either Diagnostic ExferenceSession
sealPreparedExferenceSessionWithPolicy = sealPreparedEnvironment

sealPreparedEnvironment
  :: [Name]
  -> Map Name Penalty
  -> PreparedSynthesisInventory ()
  -> Either Diagnostic ExferenceSession
sealPreparedEnvironment exclusions overrides prepared = do
  let backend = preparedSynthesisBackend prepared
  ratedFunctions <- applyRatingOverrides overrides
    $ environmentFunctions backend
  -- Policy validation is deliberately asymmetric. A rating override that
  -- names an unavailable binding is rejected: it claims to change search
  -- behavior, so silently ignoring it would hide a typo. An exclusion only
  -- removes a capability, and the command frontends pass fixed structural
  -- names (for example Data.Function.fix) whether or not the loaded
  -- environment defines them, so an unknown exclusion is a harmless no-op.
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
  let foundation = preparedSynthesisWitness prepared
      session = ExferenceSession
        { searchView = searchEnvironment
        , preparedView = foundation
        , omissionView = omissions
        }
  -- Evaluate the selector and the complete omission summary before the
  -- backend-bearing witness leaves scope. Otherwise a lazy selector or list
  -- comprehension could keep the unfiltered EnvDictionary reachable from an
  -- otherwise parser-neutral session.
  foundation `seq` (omissions `deepseq` pure session)

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

-- | Elaborate a proper query type through the exact alias and kind witness
-- retained by this session. Request provenance and diagnostic presentation
-- remain the public adapter's responsibility.
elaborateSessionGoal
  :: ExferenceSession
  -> Type SynthesisVariable
  -> Either (TypeElaborationError SynthesisVariable) (Type SynthesisVariable)
elaborateSessionGoal session = elaboratePreparedType
  freshSynthesisVariable (preparedView session) ProperTypeKind

exferenceSessionInventory
  :: ExferenceSession
  -> Inventory SynthesisVariable ()
exferenceSessionInventory = preparedInventory . preparedView

sessionOmissions :: ExferenceSession -> [ExferenceOmission]
sessionOmissions = omissionView

functionSupported :: FunctionBinding -> Bool
functionSupported = all (not . containsForall) . functionBindingTypes

deconstructorSupported :: DeconstructorBinding -> Bool
deconstructorSupported binding = not (deconstructorRecursive binding)
  && all (not . containsForall) (deconstructorBindingTypes binding)

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
