{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Private construction for inventory-bound length semantic sessions.
--
-- A session is deliberately sealed from raw authority in one call.  In
-- particular, it never accepts a checked spine context and a checked provider
-- inventory as independent inputs: those values could have been produced by
-- different source inventories.  Candidate interpretation and complete
-- behavioral-problem construction are layered on this association below.
module Language.Haskell.Synthesis.Internal.Semantic.Length.Problem
  ( LengthSemanticFingerprintPart (..)
  , LengthEncodingPolicyFingerprintSubject
  , LengthSessionError (..)
  , LengthInterpretationPolicySource (..)
  , CheckedLengthInterpretationPolicy
  , LengthTargetArgumentPolicy (..)
  , LengthCasePolicy (..)
  , CheckedLengthSession
  , sealLengthSessionWithInterpretationPolicy
  , sealLengthSession
  , sealRoleAwareLengthSession
  , sealExactSpineCaseLengthSession
  , checkedLengthSessionInterpretationPolicy
  , checkedLengthSessionExplicitTargetRoles
  , sealLengthContractInSession
  , checkedLengthSessionContext
  , checkedLengthSessionProviderInventory
  , lengthSessionInventoryFingerprint
  , lengthSessionEncodingPolicyFingerprint
  , checkedLengthSessionLimits
  , checkedLengthSessionTargetArgumentPolicy
  , checkedLengthSessionCasePolicy
  ) where

import Control.DeepSeq (NFData (rnf))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Collection (observedListLength)
import Language.Haskell.Synthesis.Constraint (Constraint (..))
import Language.Haskell.Synthesis.Declaration
  ( DataConstructor (..)
  , Declaration (..)
  , TypeParameter (..)
  , ValueSignature (..)
  , declarationTermSchemes
  , declarationTypeVariables
  )
import Language.Haskell.Synthesis.Environment (environmentDeclarations)
import Language.Haskell.Synthesis.Fingerprint (Fingerprint)
import Language.Haskell.Synthesis.Internal.Alpha
  ( AlphaVariable (..)
  , BinderSlotPolicy (PositionalBinderSlots)
  , alphaNormalizeTypeWith
  )
import Language.Haskell.Synthesis.Internal.Fingerprint
  ( FingerprintBuilder (..)
  , FingerprintField (..)
  , FingerprintLimitError (..)
  , buildFingerprintWithin
  )
import qualified Language.Haskell.Synthesis.Internal.Fingerprint.Type
  as FingerprintType
import Language.Haskell.Synthesis.Internal.Semantic.Length
  ( CheckedLengthContract
  , CheckedLengthContext
  , CheckedLengthProviderInventory
  , FiniteListSpineLengthV1
  , LengthContractError
  , LengthContractSource
  , LengthLimits
  , LengthTargetArgumentRole (..)
  , LengthProviderInventoryError
  , LengthProviderSummarySource
  , LengthSpineModelError
  , LengthSpineModelSource
  , ascii
  , checkedLengthProviderName
  , checkedLengthSpineModelField
  , checkedLengthSpineStepConstructor
  , checkedLengthSpineZeroConstructor
  , checkedLengthProviderSummaries
  , finiteListSpineLengthDomainTag
  , lengthContextSpineModel
  , lengthContextInventory
  , lengthFingerprintByteLimit
  , lengthContractInputLimit
  , providerSummaryField
  , sealLengthContext
  , sealLengthContractInContext
  , sealLengthProviderInventoryInContext
  , sealRoleAwareLengthContractInContext
  , tagged
  )
import Language.Haskell.Synthesis.Inventory
  ( Inventory
  , inventoryEnvironment
  , inventoryKindAssumptions
  )
import Language.Haskell.Synthesis.Kind (Kind (..))
import Language.Haskell.Synthesis.KindInference
  ( KindAssumptions (..)
  )
import Language.Haskell.Synthesis.Name (Name)
import Language.Haskell.Synthesis.Semantic.Problem
  ( InventoryFingerprintSubject )
import Language.Haskell.Synthesis.Type
  ( Type
  , Variable (..)
  , freeVariablesInFirstOccurrenceOrder
  )

-- | Which independently constructed session identity exceeded its byte bound.
data LengthSemanticFingerprintPart
  = LengthSemanticInventoryFingerprint
  | LengthSemanticEncodingPolicyFingerprint
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSemanticFingerprintPart

-- | Identity of the solver-neutral policy selected for a session.
--
-- This is intentionally not an 'EncodingFingerprintSubject': the complete
-- behavioral encoding also depends on a re-sealed contract, the interpreter
-- version, and the normalized candidate-specific formula.
data LengthEncodingPolicyFingerprintSubject

-- | Fixed-precedence rejection while atomically sealing one session.
data LengthSessionError identity
  = LengthSessionSpineModelRejected
      (LengthSpineModelError (Variable identity))
  | LengthSessionProviderInventoryRejected
      (LengthProviderInventoryError (Variable identity))
  | LengthSessionProviderConflictsWithSpineConstructor !Name
  | LengthSessionTargetArgumentRoleLimitExceeded !Int !Int
  | LengthSessionFingerprintLimitExceeded
      !LengthSemanticFingerprintPart !Natural !Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData identity => NFData (LengthSessionError identity)

-- | Interpreter policy selected by a sealed session.
--
-- Compatibility behavior retains only the semantic distinction that changes
-- interpretation.  The unified checked policy additionally retains an
-- optional exact role association.  Session encoding-policy identities are
-- deliberately versioned whenever this common candidate trust boundary
-- changes, even when the public interpretation signatures stay compatible.
data LengthTargetArgumentPolicy
  = LengthLegacyObservedTargetPolicy
  | LengthMixedTargetPolicy
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthTargetArgumentPolicy

-- | Closed candidate-case semantics retained by a checked session.
--
-- Ordinary sessions preserve the historical fail-closed case boundary.  The
-- additive exact policy admits only the modeled spine's complete zero/step
-- split; it is never inferred from a candidate graph.
data LengthCasePolicy
  = LengthCasesRejected
  | LengthExactZeroStepCases
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthCasePolicy

-- | Closed public request for one Length interpretation boundary.
--
-- Legacy contracts derive an all-observed vector from their target.  Both
-- explicit forms retain the supplied vector exactly; exact zero/step case
-- authority therefore cannot be constructed without target-role authority.
data LengthInterpretationPolicySource
  = LengthLegacyCasesRejected
  | LengthExplicitTargetRolesCasesRejected [LengthTargetArgumentRole]
  | LengthExplicitTargetRolesExactZeroStepCases [LengthTargetArgumentRole]
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthInterpretationPolicySource

-- | Checked interpretation authority retained by one exact session.
--
-- The constructor and all component projections stay private.  Fingerprints
-- consume only the mixed-target and case projections; the exact optional
-- vector remains association authority rather than a distinct identity input.
-- This checkpoint nevertheless advances every common policy version because
-- the candidate trust boundary now admits one exact associated-provider case.
data CheckedLengthInterpretationPolicy = CheckedLengthInterpretationPolicy
  !(Maybe [LengthTargetArgumentRole])
  !LengthTargetArgumentPolicy
  !LengthCasePolicy

instance NFData CheckedLengthInterpretationPolicy where
  rnf (CheckedLengthInterpretationPolicy roles targetPolicy casePolicy) =
    rnf roles `seq` rnf targetPolicy `seq` rnf casePolicy

-- | Exact neutral inventory, checked finite-spine model, and provider laws
-- retained under distinct complete structural identities.
data CheckedLengthSession identity annotation = CheckedLengthSession
  !LengthLimits
  !CheckedLengthInterpretationPolicy
  !(CheckedLengthContext (Variable identity) annotation)
  !(CheckedLengthProviderInventory (Variable identity))
  !(Fingerprint
      (InventoryFingerprintSubject FiniteListSpineLengthV1))
  !(Fingerprint
      LengthEncodingPolicyFingerprintSubject)

type role CheckedLengthSession nominal nominal

instance (NFData identity, NFData annotation)
    => NFData (CheckedLengthSession identity annotation) where
  rnf (CheckedLengthSession limits policy context providers
      inventory encoding) =
    rnf limits `seq`
    rnf policy `seq`
    rnf context `seq`
    rnf providers `seq`
    rnf inventory `seq`
    rnf encoding

-- | Seal all session authority from one raw source inventory.
--
-- Failure order is spine schema, provider summaries, a provider name reserved
-- by either modeled constructor, target-role admission on the role-aware path,
-- exact inventory identity, then solver-neutral encoding-policy identity.  The
-- provider sealer resolves every named scheme from the context constructed
-- immediately before it, so a successful value cannot contain cross-inventory
-- checked projections. The legacy path never receives or traverses roles.
sealLengthSession
  :: Ord identity
  => LengthLimits
  -> Inventory (Variable identity) annotation
  -> LengthSpineModelSource
  -> [LengthProviderSummarySource (Variable identity)]
  -> Either
      (LengthSessionError identity)
      (CheckedLengthSession identity annotation)
sealLengthSession limits inventory modelSource providerSources =
  sealLengthSessionWithInterpretationPolicy limits
    LengthLegacyCasesRejected inventory modelSource providerSources

-- | Seal a session for one bounded target-role vector.
--
-- The checked policy retains the vector as strict association authority.  An
-- all-observed vector still canonicalizes to the current legacy-policy
-- identity (v5), and the compatibility problem wrapper keeps its historical
-- loose mixedness-only association behavior.  This does not preserve obsolete
-- v2 policy bytes.
sealRoleAwareLengthSession
  :: Ord identity
  => LengthLimits
  -> [LengthTargetArgumentRole]
  -> Inventory (Variable identity) annotation
  -> LengthSpineModelSource
  -> [LengthProviderSummarySource (Variable identity)]
  -> Either
      (LengthSessionError identity)
      (CheckedLengthSession identity annotation)
sealRoleAwareLengthSession limits roles inventory modelSource providerSources =
  sealLengthSessionWithInterpretationPolicy limits
    (LengthExplicitTargetRolesCasesRejected roles)
    inventory modelSource providerSources

-- | Seal an explicitly role-associated session for exact modeled-spine cases.
--
-- The checked policy retains the vector as strict association authority in
-- addition to the mixed-target distinction and strict case policy.  Supplying
-- an all-observed vector does not collapse to a legacy session because
-- accepting a zero/step split changes the interpreter trust boundary.
sealExactSpineCaseLengthSession
  :: Ord identity
  => LengthLimits
  -> [LengthTargetArgumentRole]
  -> Inventory (Variable identity) annotation
  -> LengthSpineModelSource
  -> [LengthProviderSummarySource (Variable identity)]
  -> Either
      (LengthSessionError identity)
      (CheckedLengthSession identity annotation)
sealExactSpineCaseLengthSession limits roles inventory modelSource
    providerSources =
  sealLengthSessionWithInterpretationPolicy limits
    (LengthExplicitTargetRolesExactZeroStepCases roles)
    inventory modelSource providerSources

-- | Seal inventory, provider, and interpretation authority atomically.
--
-- Policy roles are deliberately inspected only after the historical spine,
-- provider, and constructor-name checks.  This preserves wrapper failure and
-- demand precedence while giving new callers one closed policy entrance.
sealLengthSessionWithInterpretationPolicy
  :: Ord identity
  => LengthLimits
  -> LengthInterpretationPolicySource
  -> Inventory (Variable identity) annotation
  -> LengthSpineModelSource
  -> [LengthProviderSummarySource (Variable identity)]
  -> Either
      (LengthSessionError identity)
      (CheckedLengthSession identity annotation)
sealLengthSessionWithInterpretationPolicy limits policySource inventory modelSource
    providerSources = do
  context <- either (Left . LengthSessionSpineModelRejected) Right
    $ sealLengthContext limits inventory modelSource
  providers <- either
    (Left . LengthSessionProviderInventoryRejected) Right
    $ sealLengthProviderInventoryInContext limits context providerSources
  rejectSpineConstructorProviders context providers
  policy <- case policySource of
    LengthLegacyCasesRejected -> Right $ CheckedLengthInterpretationPolicy
      Nothing LengthLegacyObservedTargetPolicy LengthCasesRejected
    LengthExplicitTargetRolesCasesRejected roles ->
      checkExplicitPolicy roles LengthCasesRejected
    LengthExplicitTargetRolesExactZeroStepCases roles ->
      checkExplicitPolicy roles LengthExactZeroStepCases
  let targetPolicy = checkedPolicyTargetArgumentPolicy policy
      casePolicy = checkedPolicyCasePolicy policy
  inventoryFingerprint <- mapFingerprintFailure
    LengthSemanticInventoryFingerprint
    $ buildLengthInventoryFingerprint limits context providers
  encodingFingerprint <- mapFingerprintFailure
    LengthSemanticEncodingPolicyFingerprint
    $ buildLengthEncodingFingerprint limits targetPolicy casePolicy context
  pure $ CheckedLengthSession
    limits policy context providers inventoryFingerprint encodingFingerprint
 where
  checkExplicitPolicy roles casePolicy = do
      let maximumRoles = lengthContractInputLimit limits
          observedRoles = observedListLength maximumRoles roles
      if observedRoles > maximumRoles
        then Left $ LengthSessionTargetArgumentRoleLimitExceeded
          maximumRoles observedRoles
        else Right $ CheckedLengthInterpretationPolicy
          (Just roles)
          (if LengthUnobservedTarget `elem` roles
            then LengthMixedTargetPolicy
            else LengthLegacyObservedTargetPolicy)
          casePolicy

checkedLengthSessionContext
  :: CheckedLengthSession identity annotation
  -> CheckedLengthContext (Variable identity) annotation
checkedLengthSessionContext (CheckedLengthSession _ _ context _ _ _) = context

checkedLengthSessionProviderInventory
  :: CheckedLengthSession identity annotation
  -> CheckedLengthProviderInventory (Variable identity)
checkedLengthSessionProviderInventory
    (CheckedLengthSession _ _ _ providers _ _) = providers

lengthSessionInventoryFingerprint
  :: CheckedLengthSession identity annotation
  -> Fingerprint (InventoryFingerprintSubject FiniteListSpineLengthV1)
lengthSessionInventoryFingerprint
    (CheckedLengthSession _ _ _ _ inventory _) = inventory

lengthSessionEncodingPolicyFingerprint
  :: CheckedLengthSession identity annotation
  -> Fingerprint LengthEncodingPolicyFingerprintSubject
lengthSessionEncodingPolicyFingerprint
    (CheckedLengthSession _ _ _ _ _ encoding) = encoding

-- | Opaque checked interpretation authority retained by this session.
checkedLengthSessionInterpretationPolicy
  :: CheckedLengthSession identity annotation
  -> CheckedLengthInterpretationPolicy
checkedLengthSessionInterpretationPolicy
    (CheckedLengthSession _ policy _ _ _ _) = policy

-- Package-private exact association authority.  'Nothing' is the legacy
-- implicit-all-observed entrance; every explicit source retains 'Just', even
-- for an empty vector.
checkedLengthSessionExplicitTargetRoles
  :: CheckedLengthSession identity annotation
  -> Maybe [LengthTargetArgumentRole]
checkedLengthSessionExplicitTargetRoles = checkedPolicyExplicitTargetRoles
  . checkedLengthSessionInterpretationPolicy

-- | Seal a contract directly under the interpretation authority retained by
-- one session.  Explicit roles have a single source of truth; the legacy
-- policy continues to derive its all-observed vector from the target spine.
sealLengthContractInSession
  :: Ord identity
  => CheckedLengthSession identity annotation
  -> Type (Variable identity)
  -> LengthContractSource
  -> Either
      (LengthContractError (Variable identity))
      (CheckedLengthContract (Variable identity))
sealLengthContractInSession session = case
    checkedLengthSessionExplicitTargetRoles session of
  Nothing -> sealLengthContractInContext limits context
  Just roles -> sealRoleAwareLengthContractInContext
    limits context roles
 where
  limits = checkedLengthSessionLimits session
  context = checkedLengthSessionContext session

-- | Original finite bounds used for every authority sealed into the session.
-- Kept package-private so candidate construction can revalidate a detachable
-- contract, normalize interpreted syntax, and build bounded identities without
-- allowing a caller to substitute a different policy.
checkedLengthSessionLimits
  :: CheckedLengthSession identity annotation
  -> LengthLimits
checkedLengthSessionLimits (CheckedLengthSession limits _ _ _ _ _) = limits

checkedLengthSessionTargetArgumentPolicy
  :: CheckedLengthSession identity annotation
  -> LengthTargetArgumentPolicy
checkedLengthSessionTargetArgumentPolicy = checkedPolicyTargetArgumentPolicy
  . checkedLengthSessionInterpretationPolicy

checkedLengthSessionCasePolicy
  :: CheckedLengthSession identity annotation
  -> LengthCasePolicy
checkedLengthSessionCasePolicy = checkedPolicyCasePolicy
  . checkedLengthSessionInterpretationPolicy

checkedPolicyExplicitTargetRoles
  :: CheckedLengthInterpretationPolicy
  -> Maybe [LengthTargetArgumentRole]
checkedPolicyExplicitTargetRoles
    (CheckedLengthInterpretationPolicy roles _ _) = roles

checkedPolicyTargetArgumentPolicy
  :: CheckedLengthInterpretationPolicy
  -> LengthTargetArgumentPolicy
checkedPolicyTargetArgumentPolicy
    (CheckedLengthInterpretationPolicy _ policy _) = policy

checkedPolicyCasePolicy
  :: CheckedLengthInterpretationPolicy
  -> LengthCasePolicy
checkedPolicyCasePolicy
    (CheckedLengthInterpretationPolicy _ _ policy) = policy

rejectSpineConstructorProviders
  :: CheckedLengthContext variable annotation
  -> CheckedLengthProviderInventory variable
  -> Either (LengthSessionError identity) ()
rejectSpineConstructorProviders context providers = case List.find conflicts
    $ checkedLengthProviderSummaries providers of
  Nothing -> Right ()
  Just provider -> Left $ LengthSessionProviderConflictsWithSpineConstructor
    $ checkedLengthProviderName provider
 where
  model = lengthContextSpineModel context
  zeroName = checkedLengthSpineZeroConstructor model
  stepName = checkedLengthSpineStepConstructor model
  conflicts provider = let name = checkedLengthProviderName provider
    in name == zeroName || name == stepName

mapFingerprintFailure
  :: LengthSemanticFingerprintPart
  -> Either FingerprintLimitError value
  -> Either (LengthSessionError identity) value
mapFingerprintFailure part = either reject Right
 where
  reject FingerprintLimitExceeded
      { fingerprintMaximumBytes = maximumBytes
      , fingerprintObservedBytesAtLeast = observedBytes
      } = Left $ LengthSessionFingerprintLimitExceeded
        part maximumBytes observedBytes

buildLengthInventoryFingerprint
  :: Ord identity
  => LengthLimits
  -> CheckedLengthContext (Variable identity) annotation
  -> CheckedLengthProviderInventory (Variable identity)
  -> Either FingerprintLimitError
      (Fingerprint
        (InventoryFingerprintSubject FiniteListSpineLengthV1))
buildLengthInventoryFingerprint limits context providers =
  buildFingerprintWithin (fromIntegral $ lengthFingerprintByteLimit limits)
    FingerprintBuilder
      { fingerprintBuilderVersion = 1
      , fingerprintBuilderRole = ascii
          "finite-list-spine-length/semantic-inventory"
      , fingerprintBuilderFields =
          [ tagged "dialect"
              [FingerprintBytes finiteListSpineLengthDomainTag]
          , tagged "annotation-policy"
              [FingerprintBytes $ ascii "erase-source-annotations/v1"]
          , tagged "neutral-source-inventory"
              [inventoryField $ contextInventory context]
          , tagged "spine-model"
              [checkedLengthSpineModelField $ lengthContextSpineModel context]
          , tagged "assumed-provider-laws"
              [ FingerprintSequence $ map providerSummaryField
                  $ checkedLengthProviderSummaries providers
              ]
          ]
      }

buildLengthEncodingFingerprint
  :: LengthLimits
  -> LengthTargetArgumentPolicy
  -> LengthCasePolicy
  -> CheckedLengthContext (Variable identity) annotation
  -> Either FingerprintLimitError
      (Fingerprint
        LengthEncodingPolicyFingerprintSubject)
buildLengthEncodingFingerprint limits targetPolicy casePolicy context =
  buildFingerprintWithin (fromIntegral $ lengthFingerprintByteLimit limits)
    FingerprintBuilder
      { fingerprintBuilderVersion = case casePolicy of
          LengthExactZeroStepCases -> 7
          LengthCasesRejected -> case targetPolicy of
            LengthLegacyObservedTargetPolicy -> 5
            LengthMixedTargetPolicy -> 6
      , fingerprintBuilderRole = ascii
          "finite-list-spine-length/solver-neutral-encoding"
      , fingerprintBuilderFields =
          [ tagged "dialect"
              [FingerprintBytes finiteListSpineLengthDomainTag]
          , tagged "semantic-policy" $
              [ FingerprintBytes $ ascii "finite-spine-total/v1"
              , FingerprintBytes $ ascii "opaque-payload/v1"
              , FingerprintBytes $ ascii "unbounded-natural/v1"
              , FingerprintBytes $ ascii "exact-truncated-monus/v1"
              , FingerprintBytes $ ascii "exact-conditional/v1"
              , FingerprintBytes $ ascii "assumed-provider-laws/v1"
              ] ++ mixedSemanticPolicy ++ caseSemanticPolicy
          , tagged "normalization"
              [FingerprintBytes $ ascii "length-normalizer/v1"]
          , tagged "candidate-policy" $
              [ FingerprintBytes $ ascii "complete-typed-term-graph/v1"
              , FingerprintBytes $ ascii "lazy-symbolic-interpreter/v1"
              , FingerprintBytes $ ascii "rigid-target-opening/v1"
              , FingerprintBytes $ ascii
                  "implicit-flexible-generalization/v1"
              , FingerprintBytes $ ascii
                  "authorized-provider-instantiation/v1"
              , FingerprintBytes $ ascii
                  "exact-session-graph-kinds/v1"
              , FingerprintBytes $ ascii
                  "binder-kind-checked-visible-selection/v1"
              , FingerprintBytes $ ascii
                  "authorized-visible-selection/v1"
              , FingerprintBytes $ ascii
                  "reject-detached-certified-visible-application/v1"
              , FingerprintBytes $ ascii
                  "admit-exact-obligation-free-associated-provider-visible-application/v1"
              ] ++ caseCandidatePolicy ++
              [ FingerprintBytes $ ascii "reject-unknown-semantics/v1"
              ] ++ mixedCandidatePolicy
          , tagged "spine-model"
              [checkedLengthSpineModelField $ lengthContextSpineModel context]
          ]
      }
 where
  mixedSemanticPolicy = case targetPolicy of
    LengthLegacyObservedTargetPolicy -> []
    LengthMixedTargetPolicy ->
      [ FingerprintBytes $ ascii "opaque-unobserved-target/v1"
      , FingerprintBytes $ ascii "forward-only-unobserved-target/v1"
      ]
  mixedCandidatePolicy = case targetPolicy of
    LengthLegacyObservedTargetPolicy -> []
    LengthMixedTargetPolicy ->
      [ FingerprintBytes $ ascii "source-ordered-target-roles/v1"
      , FingerprintBytes $ ascii "compact-observed-input-numbering/v1"
      , FingerprintBytes $ ascii "explicit-opaque-demand-rejection/v1"
      ]
  caseSemanticPolicy = case casePolicy of
    LengthCasesRejected -> []
    LengthExactZeroStepCases ->
      [ FingerprintBytes $ ascii "exact-zero-step-spine-case/v1"
      , FingerprintBytes $ ascii "symbolic-zero-test/v1"
      , FingerprintBytes $ ascii "recursive-tail-natural-monus-one/v1"
      , FingerprintBytes $ ascii "opaque-step-payload/v1"
      , FingerprintBytes $ ascii "whole-case-provider-law-union/v1"
      ]
  caseCandidatePolicy = case casePolicy of
    LengthCasesRejected ->
      [FingerprintBytes $ ascii "reject-case-and-constructor-pattern/v1"]
    LengthExactZeroStepCases ->
      [ FingerprintBytes $ ascii "exact-modeled-zero-step-patterns/v1"
      , FingerprintBytes $ ascii "canonical-zero-then-step-analysis/v1"
      ]

contextInventory
  :: CheckedLengthContext variable annotation
  -> Inventory variable annotation
contextInventory = lengthContextInventory

inventoryField
  :: Ord identity
  => Inventory (Variable identity) annotation
  -> FingerprintField
inventoryField inventory = tagged "inventory"
  [ tagged "declarations"
      [ FingerprintSequence $ map declarationField
          $ environmentDeclarations $ inventoryEnvironment inventory
      ]
  , kindAssumptionsField $ inventoryKindAssumptions inventory
  ]

declarationField
  :: Ord identity
  => Declaration (Variable identity) Void annotation
  -> FingerprintField
declarationField declaration = case declaration of
  TypeSynonymDeclaration _ name parameters body -> tagged "type-synonym"
    [ FingerprintName name
    , parameterListField slots parameters
    , typeField slots body
    ]
  DataTypeDeclaration _ name parameters constructors -> tagged "data-type"
    [ FingerprintName name
    , parameterListField slots parameters
    , FingerprintSequence $ map (constructorField slots) constructors
    ]
  AbstractTypeDeclaration _ name kind -> tagged "abstract-type"
    [FingerprintName name, kindField kind]
  ValueDeclaration _ -> tagged "value"
    [FingerprintSequence $ map (closedSignatureField slots) schemes]
  ClassDeclaration _ name parameters superclasses _ -> tagged "class"
    [ FingerprintName name
    , parameterListField slots parameters
    , FingerprintSequence $ map (constraintField slots) superclasses
    , tagged "closed-method-schemes"
        [FingerprintSequence $ map (closedSignatureField slots) schemes]
    ]
  InstanceDeclaration _ variables prerequisites headConstraint ->
    let instanceSlots = inventoryVariableSlots
          $ concatMap constraintVariables
              $ prerequisites ++ [headConstraint]
    in tagged "instance"
      [ tagged "binders"
          [ FingerprintNatural $ fromIntegral $ length variables
          ]
      , FingerprintSequence $ map (constraintField instanceSlots) prerequisites
      , constraintField instanceSlots headConstraint
      ]
 where
  slots = inventoryVariableSlots $ declarationTypeVariables declaration
  schemes = declarationTermSchemes declaration
  constraintVariables (Constraint _ arguments) = concatMap
    freeVariablesInFirstOccurrenceOrder arguments

parameterListField
  :: Ord identity
  => InventoryVariableSlots identity
  -> [TypeParameter (Variable identity) Void]
  -> FingerprintField
parameterListField slots parameters = tagged "parameters"
  [ FingerprintNatural $ fromIntegral $ length parameters
  , FingerprintSequence $ map parameterField parameters
  ]
 where
  parameterField (TypeParameter variable possibleKind) = tagged "parameter"
    [ inventoryVariableField slots variable
    , case possibleKind of
      Nothing -> tagged "implicit-kind" []
      Just kind -> tagged "explicit-kind" [kindField kind]
    ]

constructorField
  :: Ord identity
  => InventoryVariableSlots identity
  -> DataConstructor (Variable identity) annotation
  -> FingerprintField
constructorField slots constructor = tagged "constructor"
  [ FingerprintName $ constructorName constructor
  , FingerprintSequence $ map (typeField slots)
      $ constructorFields constructor
  ]

closedSignatureField
  :: Ord identity
  => InventoryVariableSlots identity
  -> ValueSignature (Variable identity) annotation
  -> FingerprintField
closedSignatureField slots signature = tagged "closed-term-scheme"
  [ FingerprintName $ valueName signature
  , typeField slots $ valueType signature
  ]

constraintField
  :: Ord identity
  => InventoryVariableSlots identity
  -> Constraint (Type (Variable identity))
  -> FingerprintField
constraintField slots (Constraint className arguments) = tagged "constraint"
  [ FingerprintName className
  , FingerprintSequence $ map (typeField slots) arguments
  ]

typeField
  :: Ord identity
  => InventoryVariableSlots identity
  -> Type (Variable identity)
  -> FingerprintField
typeField slots = FingerprintType.typeFingerprintField
    (alphaVariableField slots)
  . FingerprintType.canonicalTypeFingerprintForm
  . alphaNormalizeTypeWith PositionalBinderSlots

data InventoryVariableSlots identity = InventoryVariableSlots
  !(Map identity Natural)
  !(Map identity Natural)

inventoryVariableSlots
  :: Ord identity
  => [Variable identity]
  -> InventoryVariableSlots identity
inventoryVariableSlots = List.foldl' insert
  $ InventoryVariableSlots Map.empty Map.empty
 where
  insert (InventoryVariableSlots flexible rigid) variable = case variable of
    FlexibleVariable identity -> InventoryVariableSlots
      (insertNext identity flexible) rigid
    RigidVariable identity -> InventoryVariableSlots
      flexible (insertNext identity rigid)

  insertNext identity slots
    | Map.member identity slots = slots
    | otherwise = Map.insert identity (fromIntegral $ Map.size slots) slots

inventoryVariableField
  :: Ord identity
  => InventoryVariableSlots identity
  -> Variable identity
  -> FingerprintField
inventoryVariableField (InventoryVariableSlots flexible rigid) variable =
  case variable of
    FlexibleVariable identity -> tagged "flexible"
      [slotField $ Map.lookup identity flexible]
    RigidVariable identity -> tagged "rigid"
      [slotField $ Map.lookup identity rigid]
 where
  slotField Nothing = tagged "missing-inventory-variable-slot" []
  slotField (Just slot) = tagged "slot" [FingerprintNatural slot]

alphaVariableField
  :: Ord identity
  => InventoryVariableSlots identity
  -> AlphaVariable (Variable identity)
  -> FingerprintField
alphaVariableField slots variable = case variable of
  AlphaBoundVariable scope slot -> tagged "bound"
    [FingerprintNatural scope, FingerprintNatural slot]
  AlphaFreeVariable free -> tagged "free"
    [inventoryVariableField slots free]

kindAssumptionsField :: KindAssumptions -> FingerprintField
kindAssumptionsField assumptions = tagged "inferred-kind-assumptions"
  [ tagged "type-constructors"
      [ FingerprintSequence
          [ tagged "type-constructor-kind"
              [FingerprintName name, kindField kind]
          | (name, kind) <- Map.toAscList
              $ typeConstructorKinds assumptions
          ]
      ]
  , tagged "classes"
      [ FingerprintSequence
          [ tagged "class-parameter-kinds"
              [ FingerprintName name
              , FingerprintSequence $ map possibleKindField kinds
              ]
          | (name, kinds) <- Map.toAscList
              $ classParameterKinds assumptions
          ]
      ]
  ]
 where
  possibleKindField Nothing = tagged "generalized-kind" []
  possibleKindField (Just kind) = tagged "fixed-kind" [kindField kind]

kindField :: Kind Void -> FingerprintField
kindField kind = case kind of
  ProperTypeKind -> tagged "proper-type-kind" []
  KindVariable impossible -> absurd impossible
  FunctionKind parameter result -> tagged "function-kind"
    [kindField parameter, kindField result]
