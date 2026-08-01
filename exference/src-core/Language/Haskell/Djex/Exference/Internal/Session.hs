{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_HADDOCK not-home #-}

-- | Private ownership of the parser-neutral Exference session invariant.
--
-- A session retains only parser-independent state.  The HSE compatibility
-- bridge and the neutral Djex constructor both prepare the same inventory,
-- synonym table, and core dictionary before policy is applied here.
module Language.Haskell.Djex.Exference.Internal.Session
  ( ExferenceSession
  , ExferenceEnvironment
  , ExferenceInventory
  , ExferenceSessionPolicy (..)
  , defaultExferenceSessionPolicy
  , ExferenceOmission (..)
  , ExferenceOmissionCapability (..)
  , ExferenceOmissionReason (..)
  , sealNeutralExferenceSessionWithPolicy
  , sealPreparedExferenceSessionWithPolicy
  , scopeExferenceSession
  , sessionSearchEnvironment
  , sessionClassArity
  , elaborateSessionGoal
  , checkSessionTypeSynonymInspectionSaturation
  , normalizeSessionTypeSynonyms
  , exferenceSessionInventory
  , sessionRecursiveDataTypeNames
  , sessionInspectionTermSchemes
  , sessionInspectionClasses
  , sessionOmissions
  ) where

import Control.DeepSeq (NFData, deepseq)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Void (Void)
import GHC.Generics (Generic)

import qualified Language.Haskell.Exference.Core as Core
import qualified Language.Haskell.Exference.Core.Internal.Exference as CoreInternal
import Language.Haskell.Exference.Core.Declaration
  ( PreparedSynthesisInventory
  , freshSynthesisVariable
  , prepareSynthesisInventory
  , preparedSynthesisBackend
  , preparedSynthesisSchemes
  , preparedSynthesisWitness
  )
import Language.Haskell.Exference.Core.FunctionBinding
  ( ConstructorBinding (..)
  , DeconstructorBinding (..)
  , EnvDictionary (..)
  , FunctionBinding (..)
  )
import Language.Haskell.Exference.Core.Score
  ( Penalty
  , isFiniteScore
  )
import Language.Haskell.Exference.Core.Types
  ( HsType
  , QueryClassEnv
  , SynthesisVariable
  , mkQueryClassEnv
  )
import Language.Haskell.Exference.Core.TypeUtils (typeConstructorHead)
import Language.Haskell.Synthesis.Declaration
  ( ValueSignature (..)
  , declarationTermSignatures
  )
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , shownErrorDiagnostic
  )
import Language.Haskell.Synthesis.Environment
  ( Environment
  , environmentDeclarations
  )
import Language.Haskell.Synthesis.Count (naturalLength)
import Language.Haskell.Synthesis.Inventory
  ( Inventory
  , inventoryClassArity
  , inventoryEnvironment
  , mkInventoryFromEnvironmentWithClassPolicy
  )
import Language.Haskell.Synthesis.KindInference
  ( ClassKindPolicy (GeneralizeClassKinds)
  , KindInventoryPolicy (OpenKindInventory)
  )
import Language.Haskell.Synthesis.Kind (Kind (ProperTypeKind))
import Language.Haskell.Synthesis.Name
  ( Name
  , nameSpecial
  , renderCanonical
  )
import Language.Haskell.Synthesis.Type
  ( Type (ForallType, TypeConstructor)
  , applicationSpine
  )
import Language.Haskell.Synthesis.TypeSynonym
  ( PreparedInventory
  , SynonymExpansionError (UnsaturatedTypeSynonym)
  , TypeElaborationError
  , checkPreparedTypeSynonymApplicationSaturation
  , checkPreparedTypeSynonymSaturation
  , elaboratePreparedType
  , normalizePreparedTypeSynonyms
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
    -- ^ Legacy compatibility value. Current sessions retain nested universal
    -- quantifiers as opaque type atoms and never produce this omission.
  | RecursiveDataEliminationUnsupported
    -- ^ Legacy compatibility reason. Current sessions retain recursive
    -- datatypes for bounded one-layer elimination and do not produce it.
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
-- declarations and synonyms. The policy-filtered backend lowering is retained
-- solely so an interactive frontend can derive reversible search scopes; it
-- is parser-independent and cannot add or replace declarations after sealing.
data ExferenceSession = ExferenceSession
  { searchView :: Core.ExferenceEnvironment
  , reusableSearchView :: EnvDictionary
  , reusableSearchSchemesView :: Map Name HsType
  , preparedView :: PreparedInventory SynthesisVariable ()
  , inspectionTermSchemesView :: Map Name HsType
  , inspectionClassesView :: QueryClassEnv
  , omissionView :: [ExferenceOmission]
  }

-- | The parser-independent declaration environment accepted by Exference.
type ExferenceEnvironment = Environment SynthesisVariable Void ()

-- | The annotation-erased neutral inventory retained by a session.
type ExferenceInventory = Inventory SynthesisVariable ()

-- | Policy applied once while projecting the authoritative inventory into
-- Exference's supported search capabilities. Names are structural and exact:
-- excluding @Data.Function.fix@ never hides an unrelated qualified @fix@.
data ExferenceSessionPolicy = ExferenceSessionPolicy
  { exferenceExcludedBindings :: [Name]
    -- ^ Exact binding names to remove. Unknown exclusions are harmless no-ops.
  , exferenceRatingOverrides :: Map Name Penalty
    -- ^ Finite ratings for supported, non-excluded search bindings. An
    -- override that cannot affect search is rejected.
  }
  deriving (Eq, Show)

-- | Retain every supported binding and its source rating.
defaultExferenceSessionPolicy :: ExferenceSessionPolicy
defaultExferenceSessionPolicy = ExferenceSessionPolicy
  { exferenceExcludedBindings = []
  , exferenceRatingOverrides = Map.empty
  }

sealNeutralExferenceSessionWithPolicy
  :: ExferenceSessionPolicy
  -> ExferenceEnvironment
  -> Either Diagnostic ExferenceSession
sealNeutralExferenceSessionWithPolicy policy environment = do
  inventory <- first
    (preparationFailure "cannot validate the neutral Exference inventory")
    $ mkInventoryFromEnvironmentWithClassPolicy
        OpenKindInventory GeneralizeClassKinds environment
  prepared <- first
    (preparationFailure "cannot prepare the neutral Exference environment")
    $ prepareSynthesisInventory inventory
  sealPreparedEnvironment policy prepared

-- | Seal a prepared checked-source witness without retaining parser-specific
-- representation. The opaque prepared inventory proves that the synonym
-- table and rated, ordered backend are projections of the same neutral
-- inventory; exposed source-frontend seams cannot recombine those views.
sealPreparedExferenceSessionWithPolicy
  :: ExferenceSessionPolicy
  -> PreparedSynthesisInventory ()
  -> Either Diagnostic ExferenceSession
sealPreparedExferenceSessionWithPolicy = sealPreparedEnvironment

sealPreparedEnvironment
  :: ExferenceSessionPolicy
  -> PreparedSynthesisInventory ()
  -> Either Diagnostic ExferenceSession
sealPreparedEnvironment policy prepared = do
  let backend = preparedSynthesisBackend prepared
  -- Policy validation is deliberately asymmetric. A rating override that
  -- cannot reach the retained search projection is rejected: it claims to
  -- change search behavior, so silently ignoring it would hide a typo or a
  -- contradictory policy. An exclusion only removes a capability, and the
  -- command frontends pass fixed structural names (for example
  -- Data.Function.fix) whether or not the environment defines them, so an
  -- unknown exclusion remains a harmless no-op.
  let exclusions = exferenceExcludedBindings policy
      overrides = exferenceRatingOverrides policy
      sourceFunctions = environmentFunctions backend
      excludedBindings = Set.fromList exclusions
      functionExcluded binding = Set.member
        (functionName binding) excludedBindings
      retainedFunctions =
        [ binding
        | binding <- sourceFunctions
        , not $ functionExcluded binding
        ]
  supportedFunctions <- applyRatingOverrides overrides retainedFunctions
  let supportedBackend = backend
        { environmentFunctions = supportedFunctions
        }
      omissions =
        [ ExferenceOmission
            (functionName binding)
            BindingIntroduction
            reason
        | binding <- sourceFunctions
        , reason <- if functionExcluded binding
            then [ExcludedByPolicy]
            else []
        ]
  let foundation = preparedSynthesisWitness prepared
      inspectionEnvironment = inventoryEnvironment
        $ preparedInventory foundation
      inspectionSignatures = concatMap declarationTermSignatures
        $ environmentDeclarations inspectionEnvironment
      inspectionTermSchemes = Map.fromList
        [ (valueName signature, valueType signature)
        | signature <- inspectionSignatures
        ]
      supportedNames = Set.fromList
        $ map functionName supportedFunctions
      supportedSchemes = Map.filterWithKey
        (\name _ -> name `Set.member` supportedNames)
        $ preparedSynthesisSchemes prepared
      inspectionClasses = mkQueryClassEnv (environmentClasses backend) []
  searchEnvironment <- first
    (shownErrorDiagnostic "DJEX_EXF_ENV"
      "cannot seal the Exference session environment")
    $ CoreInternal.mkExferenceEnvironmentWithSchemes
        supportedBackend supportedSchemes
  let
      session = ExferenceSession
        { searchView = searchEnvironment
        , reusableSearchView = supportedBackend
        , reusableSearchSchemesView = supportedSchemes
        , preparedView = foundation
        , inspectionTermSchemesView = inspectionTermSchemes
        , inspectionClassesView = inspectionClasses
        , omissionView = omissions
        }
  -- Materialize the inspection indexes and complete omission summary before
  -- the backend-bearing witness leaves scope. Otherwise their lazy builders
  -- could retain the unfiltered EnvDictionary or the declaration list behind
  -- an otherwise parser-neutral session. Map values deliberately remain the
  -- shared type trees owned by the retained inventory.
  foundation `seq`
    Map.size inspectionTermSchemes `seq`
    inspectionClasses `deepseq`
    omissions `deepseq`
    pure session

-- | Rebuild only the query-facing search projection for an interactive
-- scope. The complete checked inventory remains available for qualified type
-- lookup, synonym elaboration, kinds, classes, and instances; only value
-- introduction and datatype elimination are narrowed.
--
-- Reprojection always starts from the policy-filtered source view retained at
-- session construction, rather than from the current projection. Repeated
-- @import@ and @:module@ changes can therefore both remove and restore names
-- without reparsing source or losing source ratings.
scopeExferenceSession
  :: Set.Set Name
  -> ExferenceSession
  -> Either Diagnostic ExferenceSession
scopeExferenceSession visible session = do
  let source = reusableSearchView session
      sourceSchemes = reusableSearchSchemesView session
      functions = filter
        (nameVisible . functionName)
        $ environmentFunctions source
      -- Exference eliminates a datatype as one exhaustive operation. Keeping
      -- it when only some constructors are imported would let synthesis emit
      -- a constructor that is outside the interactive scope, or build an
      -- incomplete case split. Require every constructor to be visible.
      deconstructors = filter constructorsVisible
        $ environmentDeconstructors source
      constructorsVisible deconstructor = all
        (nameVisible . constructorName)
        $ deconstructorConstructors deconstructor
      -- Lists, tuples, unit, and the function constructor are syntax-level
      -- identities. They have no importable defining module, so hiding every
      -- source module must not make those constructors disappear from search.
      nameVisible name = Set.member name visible
        || case nameSpecial name of
          Just _ -> True
          Nothing -> False
      scoped = source
        { environmentFunctions = functions
        , environmentDeconstructors = deconstructors
        }
      scopedNames = Set.fromList $ map functionName functions
      scopedSchemes = Map.filterWithKey
        (\name _ -> name `Set.member` scopedNames) sourceSchemes
  searchEnvironment <- first
    (shownErrorDiagnostic "DJEX_EXF_SCOPE"
      "cannot seal the interactive Exference scope")
    $ CoreInternal.mkExferenceEnvironmentWithSchemes scoped scopedSchemes
  scoped `deepseq` scopedSchemes `deepseq` pure session
    { searchView = searchEnvironment }

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

-- | Look up the exact argument width inferred for a declared or external
-- class retained by this session. Query preparation uses this finite width
-- before entering a caller-built constraint argument spine.
sessionClassArity :: ExferenceSession -> Name -> Maybe Int
sessionClassArity = inventoryClassArity . preparedInventory . preparedView

-- | Elaborate a proper query type through the exact alias and kind witness
-- retained by this session. Request provenance and diagnostic presentation
-- remain the public adapter's responsibility.
elaborateSessionGoal
  :: ExferenceSession
  -> Type SynthesisVariable
  -> Either (TypeElaborationError SynthesisVariable) (Type SynthesisVariable)
elaborateSessionGoal session = elaboratePreparedType
  freshSynthesisVariable (preparedView session) ProperTypeKind

-- | Validate the @:kind@ saturation rule without constructing an expanded
-- normal form. The complete operational head beneath context-free prenex
-- foralls may remain partial; every argument and all other positions retain
-- ordinary strict Haskell synonym saturation. Keeping this distinct from
-- normalization prevents a duplicating alias from imposing exponential work
-- on a command that will not print the expanded tree.
checkSessionTypeSynonymInspectionSaturation
  :: ExferenceSession
  -> Type SynthesisVariable
  -> Either (SynonymExpansionError SynthesisVariable) ()
checkSessionTypeSynonymInspectionSaturation session = checkOuter
 where
  prepared = preparedView session
  checkOuter typeExpression = case typeExpression of
    ForallType _ [] body -> checkOuter body
    _ -> case applicationSpine typeExpression of
      (TypeConstructor name, arguments) -> case
          checkPreparedTypeSynonymApplicationSaturation
            prepared name (naturalLength arguments) of
        Left UnsaturatedTypeSynonym{} ->
          mapM_ (checkPreparedTypeSynonymSaturation prepared) arguments
        Left failure -> Left failure
        Right () -> checkPreparedTypeSynonymSaturation prepared typeExpression
      _ -> checkPreparedTypeSynonymSaturation prepared typeExpression

-- | Normalize the type synonyms retained by this exact session without
-- imposing a proper-type result kind. The operation is intentionally lenient
-- only at the complete operational head beneath leading context-free forall
-- layers, matching GHCi's @:kind!@ treatment of a partially applied synonym;
-- nested and constrained unsaturated aliases remain invalid.
normalizeSessionTypeSynonyms
  :: ExferenceSession
  -> Type SynthesisVariable
  -> Either (SynonymExpansionError SynthesisVariable)
      (Type SynthesisVariable)
normalizeSessionTypeSynonyms session = normalizePreparedTypeSynonyms
  freshSynthesisVariable $ preparedView session

exferenceSessionInventory
  :: ExferenceSession
  -> ExferenceInventory
exferenceSessionInventory = preparedInventory . preparedView

-- | Exact datatype heads classified as recursive after synonym expansion.
-- The reusable source projection carries the flags derived from the prepared
-- inventory's transient expanded view, so interactive frontends can apply
-- elimination policy without repeating or approximating that graph analysis.
sessionRecursiveDataTypeNames :: ExferenceSession -> Set.Set Name
sessionRecursiveDataTypeNames session = Set.fromList
  [ name
  | deconstructor <- environmentDeconstructors $ reusableSearchView session
  , deconstructorRecursive deconstructor
  , Just name <- [typeConstructorHead $ deconstructorInput deconstructor]
  ]

-- | Complete term schemes retained for non-synthesizing inspection. This view
-- is derived once from the authoritative inventory, before search policy can
-- exclude a binding, so @:type@ can inspect every declaration without
-- rebuilding Exference's complete lowering for each expression.
sessionInspectionTermSchemes :: ExferenceSession -> Map Name HsType
sessionInspectionTermSchemes = inspectionTermSchemesView

-- | Class resolution context paired with 'sessionInspectionTermSchemes'.
-- Search scoping and binding exclusions do not change classes or instances,
-- so every scoped session can safely reuse this sealed empty-query view.
sessionInspectionClasses :: ExferenceSession -> QueryClassEnv
sessionInspectionClasses = inspectionClassesView

sessionOmissions :: ExferenceSession -> [ExferenceOmission]
sessionOmissions = omissionView

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
