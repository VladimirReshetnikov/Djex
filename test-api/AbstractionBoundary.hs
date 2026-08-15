{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors -Wno-deferred-out-of-scope-variables -Wno-unused-top-binds #-}

-- | Negative API fixtures for opaque invariant-bearing values.
--
-- Missing dictionaries are deliberately deferred so the ordinary test runner
-- can assert that they remain missing. If an abstract type regains 'Generic',
-- or an ordinary projection becomes a record field again, the corresponding
-- thunk evaluates successfully and turns the API regression into a test
-- failure. This module is a separate Cabal component, so it sees exactly the
-- public @djex@ surface rather than home-module constructors.
module AbstractionBoundary
  ( allowedConstructionAttempts
  , forbiddenConstructionAttempts
  ) where

import Data.Coerce (Coercible, coerce)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import Data.Word (Word8)
import Djinn.Internal.LJTFormula (Formula, Symbol)
-- Whole-module imports are deliberate for modules whose former record labels
-- are probed below. GHC solves built-in 'HasField' constraints only when the
-- corresponding field selector is in scope; importing just the owner type
-- would let a record-field regression pass this test unnoticed.
import Djinn.Internal.ProofEnv
import GHC.Generics (Generic, Rep, from)
import GHC.Records (HasField, getField)
import qualified GHC.TypeLits as TypeLits
import qualified Language.Haskell.Djex as PublicDjex
import Language.Haskell.Djex.Djinn (DjinnRequest, DjinnSession)
import Language.Haskell.Djex.Exference
  ( ExferenceEnvironment
  , ExferenceRequest
  , ExferenceSession
  )
import Language.Haskell.Exference.Core
  ( ExferenceSourceTypeVariableHints )
import qualified Language.Haskell.Exference.Core as ExferenceCore
import Language.Haskell.Exference.Core.Declaration
  ( PreparedSynthesisInventory
  , preparedSynthesisBackend
  , preparedSynthesisWitness
  )
import Language.Haskell.Exference.Core.FunctionBinding (EnvDictionary)
import Language.Haskell.Exference.Core.Internal.Scope (ScopeId, Scopes)
import Language.Haskell.Exference.Core.RigidInstantiation
import Language.Haskell.Exference.Core.Types
  ( HsConstraint
  , HsInstance
  , HsTypeClass
  , QueryClassEnv
  , SynthesisVariable
  , StaticClassEnv
  , qClassEnv_constraints
  , qClassEnv_env
  , qClassEnv_inflatedConstraints
  , sClassEnv_explicitInstances
  , sClassEnv_instances
  , sClassEnv_tclasses
  )
import Language.Haskell.Synthesis.Class
import Language.Haskell.Synthesis.Collection (DuplicateSummary)
import Language.Haskell.Synthesis.Constraint (Constraint)
import Language.Haskell.Synthesis.Declaration (Declaration)
import Language.Haskell.Synthesis.Diagnostic
  ( SourceLocation
  , SourcePosition
  , SourceSpan
  )
import Language.Haskell.Synthesis.Environment (Environment)
import Language.Haskell.Synthesis.Fingerprint (Fingerprint)
import Language.Haskell.Synthesis.Generated (DefinitionName)
import Language.Haskell.Synthesis.Inventory
import Language.Haskell.Synthesis.KindInference
  ( GroundKind, KindAssumptions )
import Language.Haskell.Synthesis.Name (ModuleName, Name)
import Language.Haskell.Synthesis.Query
import Language.Haskell.Synthesis.Semantic.Length
import Language.Haskell.Synthesis.Semantic.Length.Evaluate
import Language.Haskell.Synthesis.Semantic.Length.Problem
import Language.Haskell.Synthesis.Semantic.Length.SMTLib
import Language.Haskell.Synthesis.Semantic.Length.SMTLib.Execution
import Language.Haskell.Synthesis.Semantic.Length.SMTLib.Live
import Language.Haskell.Synthesis.Semantic.Length.SMTLib.Observation
import Language.Haskell.Synthesis.Semantic.Length.SMTLib.Response
import Language.Haskell.Synthesis.Semantic.Observation
  ( SolverObservation
  , SolverStatus
  )
import Language.Haskell.Synthesis.Semantic.Problem
import Language.Haskell.Synthesis.Search (SearchBatch)
import Language.Haskell.Synthesis.Type (Type)
import Language.Haskell.Synthesis.TypeSynonym
  ( PreparedInventory
  , PreparedInventoryExpansion
  , TypeSynonyms
  , inventoryExpansionDeclarations
  , inventoryExpansionPreparedInventory
  , inventoryExpansionRecursiveDataTypeNames
  , preparedInventory
  , preparedTypeSynonyms
  )
import Language.Haskell.Synthesis.TypedCandidate
import Language.Haskell.Synthesis.TypedGenerated (TermGraph)
import Language.Haskell.Synthesis.TypedGenerated.Fingerprint
import Language.Haskell.TH (lookupTypeName, lookupValueName)

-- Keep the qualified facade import visible to GHC as well as to the TH name
-- lookup below; the latter deliberately probes only the curated public edge.
type PublicFacadeBackendProbe = PublicDjex.Backend

-- Importing an abstract type with the same spelling as its hidden constructor
-- makes an ordinary deferred term probe a hard namespace error.  Ask the value
-- namespace directly instead.  The same check pins private observation
-- projections whose public visibility would bypass the checked replay gate,
-- plus retired candidate-problem failures that attempted to reconstruct or
-- rediscover authority already carried by opaque checked values. Any hit means
-- downstream code can name an intentionally unavailable edge and the API-test
-- component must stop compiling.
$(do
    let hiddenTypes =
          [ "PublicDjex.CheckedTypeApplicationCertificateTable"
          , "PublicDjex.CheckedTypeApplicationCertificatePlan"
          , "PublicDjex.CheckedTypeApplicationCertificateStep"
          , "PublicDjex.CheckedTypeApplicationCertificateGraph"
          , "PublicDjex.TypedCandidateGraph"
          , "PublicDjex.TypeApplicationCertificateOrigin"
          , "PublicDjex.TypeApplicationCertificateObservation"
          , "PublicDjex.TypeApplicationCertificateAssociationError"
          , "PublicDjex.CheckedTypeApplicationOrigin"
          , "PublicDjex.CheckedTypeApplicationOriginStep"
          , "PublicDjex.ExferenceTermGraphAvailability"
          , "PublicDjex.CheckedClassResolutionEnvironment"
          , "PublicDjex.ClassResolutionProof"
          , "PublicDjex.CheckedConstraintDischarge"
          , "PublicDjex.HeterogeneousClassResolutionQueryError"
          , "PublicDjex.LengthCandidateAuthority"
          , "PublicDjex.ConditionalProviderAuthorization"
          , "PublicDjex.LengthCandidateAuthorization"
          ]
        hiddenValues =
          [ "LengthSMTLibLiveSession"
          , "LengthSMTLibLiveUsableWorkBudget"
          , "LengthSMTLibLiveUsableWorkDeadline"
          , "LengthSMTLibLiveQueryObservation"
          , "LengthSpinePairSMTLibLiveQueryObservation"
          , "LengthSMTLibLiveSessionError"
          , "LengthSMTLibLiveQueryError"
          , "LengthSpinePairSMTLibLiveQueryError"
          , "PublicDjex.LengthInputBoxLimits"
          , "CheckedLengthSpinePairContract"
          , "CheckedLengthSpinePairCandidate"
          , "CheckedLengthSpinePairProblem"
          , "LengthSpinePairSMTLibQuery"
          , "ValidatedLengthSpinePairCounterexampleReceipt"
          , "ValidatedLengthCounterexampleSimplificationReceipt"
          , "ValidatedLengthSpinePairCounterexampleSimplificationReceipt"
          , "ValidatedLengthSpinePairInputBoxReceipt"
          , "ValidatedLengthApplicableDomainReceipt"
          , "ValidatedLengthSpinePairApplicableDomainReceipt"
          , "ValidatedLengthPositiveAffineApplicableDomainReceipt"
          , "ValidatedLengthSpinePairPositiveAffineApplicableDomainReceipt"
          , "ValidatedLengthRelationalPositiveAffineApplicableDomainReceipt"
          , "ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomainReceipt"
          , "CheckedLengthInterpretationPolicy"
          , "CheckedLengthSession"
          , "CheckedLengthProviderInventory"
          , "lengthSMTLibLiveQueryObservationQueryFingerprint"
          , "lengthSMTLibLiveQueryObservationCounterexampleEvidence"
          , "lengthSMTLibLiveQueryObservationSolverObservation"
          , "lengthSpinePairSMTLibLiveQueryObservationQueryFingerprint"
          , "lengthSpinePairSMTLibLiveQueryObservationCounterexampleEvidence"
          , "lengthSpinePairSMTLibLiveQueryObservationSolverObservation"
          , "associatedSolverObservationStatus"
          , "LengthProblemProviderResealRejected"
          , "LengthProblemProviderContextMismatch"
          , "LengthProblemUsedProviderMissing"
          , "checkedLengthCandidateTermGraphFingerprint"
          , "checkedLengthSessionCasePolicy"
          , "checkedLengthSessionExplicitTargetRoles"
          , "PublicDjex.checkedLengthSessionClassResolutionEnvironment"
          , "checkedPolicyExplicitTargetRoles"
          , "checkedPolicyTargetArgumentPolicy"
          , "checkedPolicyCasePolicy"
          , "fingerprintTermGraphWithTypeStructure"
          , "fingerprintCheckedTypeApplicationCertificateGraphWithTypeStructure"
          , "PublicDjex.sealTypeApplicationCertificateTable"
          , "PublicDjex.checkedTypeApplicationCertificateCount"
          , "PublicDjex.lookupCheckedTypeApplicationCertificatePlan"
          , "PublicDjex.checkedTypeApplicationCertificateStepCount"
          , "PublicDjex.checkedTypeApplicationCertificateSteps"
          , "PublicDjex.checkedTypeApplicationCertificateStepSlot"
          , "PublicDjex.checkedTypeApplicationCertificateStepSource"
          , "PublicDjex.checkedTypeApplicationCertificateStepSelected"
          , "PublicDjex.checkedTypeApplicationCertificateStepResult"
          , "PublicDjex.checkedTypeApplicationCertificateStepObligations"
          , "PublicDjex.checkedTypeApplicationCertificateStepObligationCount"
          , "PublicDjex.checkedTypeApplicationCertificateObligationCount"
          , "PublicDjex.matchCheckedTypeApplicationCertificateObservations"
          , "PublicDjex.TypedCandidate"
          , "PublicDjex.mkTypedCandidate"
          , "PublicDjex.sealCheckedTypeApplicationCertificateGraph"
          , "PublicDjex.checkedTypeApplicationCertificateGraph"
          , "PublicDjex.foldCheckedTypeApplicationCertificateGraph"
          , "PublicDjex.mkCertificateCapableTypedCandidate"
          , "PublicDjex.mkCertificateAssociatedTypedCandidate"
          , "PublicDjex.foldTypedCandidateGraph"
          , "PublicDjex.TypedCandidateGraphUnavailable"
          , "PublicDjex.TypedCandidatePlainGraph"
          , "PublicDjex.TypedCandidateCertificateGraph"
          , "PublicDjex.typedCandidateGraphProjection"
          , "PublicDjex.rnfTypedCandidateGraph"
          , "PublicDjex.typeApplicationCertificateOriginId"
          , "PublicDjex.typeApplicationCertificateOriginOwner"
          , "PublicDjex.typeApplicationCertificateOriginScheme"
          , "PublicDjex.typeApplicationCertificateOriginObservations"
          , "PublicDjex.typeApplicationCertificateObservationSlot"
          , "PublicDjex.typeApplicationCertificateObservationSource"
          , "PublicDjex.typeApplicationCertificateObservationSelected"
          , "PublicDjex.typeApplicationCertificateObservationResult"
          , "PublicDjex.typeApplicationCertificateObservationObligations"
          , "PublicDjex.checkedExpressionTypeApplicationOrigins"
          , "PublicDjex.checkedExpressionTypeApplicationOriginReferences"
          , "PublicDjex.checkedTypeApplicationOriginId"
          , "PublicDjex.checkedTypeApplicationOriginOwner"
          , "PublicDjex.checkedTypeApplicationOriginSource"
          , "PublicDjex.checkedTypeApplicationOriginSteps"
          , "PublicDjex.checkedTypeApplicationOriginStepSlot"
          , "PublicDjex.checkedTypeApplicationOriginStepSource"
          , "PublicDjex.checkedTypeApplicationOriginStepSelected"
          , "PublicDjex.checkedTypeApplicationOriginStepResult"
          , "PublicDjex.checkedTypeApplicationOriginStepObligations"
          , "PublicDjex.ExferenceTermGraphAvailable"
          , "PublicDjex.ExferenceTermGraphAssociated"
          , "PublicDjex.ExferenceTermGraphUnavailable"
          , "PublicDjex.sealClassResolutionEnvironment"
          , "PublicDjex.dischargeGroundConstraint"
          , "PublicDjex.dischargeHeterogeneousGroundConstraint"
          , "PublicDjex.replayCheckedConstraintDischarge"
          ]
    resolvedTypes <- mapM lookupTypeName hiddenTypes
    resolvedValues <- mapM lookupValueName hiddenValues
    case [name | (name, Just _) <-
            zip (hiddenTypes ++ hiddenValues)
              (resolvedTypes ++ resolvedValues)] of
      [] -> pure []
      visible -> fail $ "public representation internals: " ++ show visible
 )

data FingerprintProbe
data OtherFingerprintProbe
newtype TypedCandidateProbe = TypedCandidateProbe Int
newtype OtherTypedCandidateProbe = OtherTypedCandidateProbe Int
newtype TypedCandidateFailureProbe = TypedCandidateFailureProbe Int
newtype OtherTypedCandidateFailureProbe = OtherTypedCandidateFailureProbe Int
newtype TypedCandidateTypeProbe = TypedCandidateTypeProbe Int
newtype OtherTypedCandidateTypeProbe = OtherTypedCandidateTypeProbe Int
newtype TypedCandidateLocalProbe = TypedCandidateLocalProbe Int
newtype OtherTypedCandidateLocalProbe = OtherTypedCandidateLocalProbe Int
newtype BehavioralDomainProbe = BehavioralDomainProbe Int
newtype OtherBehavioralDomainProbe = OtherBehavioralDomainProbe Int
newtype ObservationProbe = ObservationProbe Int
newtype OtherObservationProbe = OtherObservationProbe Int
newtype EvidenceReceiptProbe = EvidenceReceiptProbe Int
newtype OtherEvidenceReceiptProbe = OtherEvidenceReceiptProbe Int
newtype ArtifactKindProbe = ArtifactKindProbe Int
newtype OtherArtifactKindProbe = OtherArtifactKindProbe Int
newtype LengthVariableProbe = LengthVariableProbe Int
newtype OtherLengthVariableProbe = OtherLengthVariableProbe Int
newtype LengthAnnotationProbe = LengthAnnotationProbe Int
newtype OtherLengthAnnotationProbe = OtherLengthAnnotationProbe Int
newtype LengthLocalProbe = LengthLocalProbe Int
newtype OtherLengthLocalProbe = OtherLengthLocalProbe Int
data OtherCheckedLengthInterpretationPolicy
newtype LiveEpochProbe = LiveEpochProbe Int
newtype OtherLiveEpochProbe = OtherLiveEpochProbe Int
newtype LiveIdentityProbe = LiveIdentityProbe Int
newtype OtherLiveIdentityProbe = OtherLiveIdentityProbe Int
newtype LiveLocalProbe = LiveLocalProbe Int
newtype OtherLiveLocalProbe = OtherLiveLocalProbe Int
newtype LiveBudgetProbe = LiveBudgetProbe Int
newtype OtherLiveBudgetProbe = OtherLiveBudgetProbe Int

forbiddenConstructionAttempts :: [(String, ())]
forbiddenConstructionAttempts =
  [ noGeneric @(Environment Int Void ()) "Environment"
  , noGeneric @(Fingerprint FingerprintProbe) "Fingerprint"
  , noGeneric @(Fingerprint TermGraphFingerprintSubject)
      "TermGraphFingerprint"
  , ( "Fingerprint subject identity unexpectedly permits Coercible"
    , forbiddenFingerprintCoercion `seq` ()
    )
  , noGeneric @(TypedCandidate () (Type Int) Int ()) "TypedCandidate"
  , noGeneric @(TermGraph Int Int) "TermGraph"
  , ( "TypedCandidate payload unexpectedly permits Coercible"
    , forbiddenTypedCandidateCoercion `seq` ()
    )
  , ( "TypedCandidate failure domain unexpectedly permits Coercible"
    , forbiddenTypedCandidateFailureCoercion `seq` ()
    )
  , ( "TypedCandidate graph type domain unexpectedly permits Coercible"
    , forbiddenTypedCandidateTypeCoercion `seq` ()
    )
  , ( "TypedCandidate graph local domain unexpectedly permits Coercible"
    , forbiddenTypedCandidateLocalCoercion `seq` ()
    )
  , ( "TermGraph type domain unexpectedly permits Coercible"
    , forbiddenTermGraphTypeCoercion `seq` ()
    )
  , ( "TermGraph local domain unexpectedly permits Coercible"
    , forbiddenTermGraphLocalCoercion `seq` ()
    )
  , noGeneric @(BehavioralProblem BehavioralDomainProbe)
      "BehavioralProblem"
  , ( "Behavioral fingerprint roles unexpectedly permit Coercible"
    , forbiddenBehavioralFingerprintRoleCoercion `seq` ()
    )
  , ( "BehavioralProblem domain unexpectedly permits Coercible"
    , forbiddenBehavioralProblemCoercion `seq` ()
    )
  , noGeneric
      @(AssociatedObservation BehavioralDomainProbe ObservationProbe)
      "AssociatedObservation"
  , ( "AssociatedObservation domain unexpectedly permits Coercible"
    , forbiddenAssociatedObservationDomainCoercion `seq` ()
    )
  , ( "AssociatedObservation payload unexpectedly permits Coercible"
    , forbiddenAssociatedObservationPayloadCoercion `seq` ()
    )
  , ( "AssociatedObservation regained an unchecked payload projection"
    , forbiddenAssociatedObservationProjection `seq` ()
    )
  , noGeneric
      @(BehavioralEvidence BehavioralDomainProbe EvidenceReceiptProbe)
      "BehavioralEvidence"
  , ( "BehavioralEvidence regained a public construction edge"
    , forbiddenBehavioralEvidenceConstruction `seq` ()
    )
  , ( "BehavioralEvidence domain unexpectedly permits Coercible"
    , forbiddenBehavioralEvidenceDomainCoercion `seq` ()
    )
  , ( "BehavioralEvidence receipt unexpectedly permits Coercible"
    , forbiddenBehavioralEvidenceReceiptCoercion `seq` ()
    )
  , ( "BehavioralEvidence regained an unchecked receipt projection"
    , forbiddenBehavioralEvidenceReceiptProjection `seq` ()
    )
  , noGeneric @(BoundedRawArtifact ArtifactKindProbe)
      "BoundedRawArtifact"
  , ( "BoundedRawArtifact kind unexpectedly permits Coercible"
    , forbiddenBoundedRawArtifactCoercion `seq` ()
    )
  , noGeneric @(CheckedLengthContract LengthVariableProbe)
      "CheckedLengthContract"
  , noGeneric @(CheckedLengthSpinePairContract LengthVariableProbe)
      "CheckedLengthSpinePairContract"
  , noGeneric
      @(CheckedLengthContext LengthVariableProbe LengthAnnotationProbe)
      "CheckedLengthContext"
  , noGeneric @(CheckedLengthSpineModel LengthVariableProbe)
      "CheckedLengthSpineModel"
  , noGeneric @LengthLimits "LengthLimits"
  , noGeneric @LengthEvaluationLimits "LengthEvaluationLimits"
  , noGeneric @LengthInputBoxLimits "LengthInputBoxLimits"
  , noGeneric @ValidatedLengthCounterexample
      "ValidatedLengthCounterexample"
  , ( "ValidatedLengthCounterexample constructor became public"
    , forbiddenValidatedLengthCounterexampleConstruction `seq` ()
    )
  , noGeneric @ValidatedLengthSpinePairCounterexample
      "ValidatedLengthSpinePairCounterexample"
  , ( "ValidatedLengthSpinePairCounterexample constructor became public"
    , forbiddenValidatedLengthSpinePairCounterexampleConstruction `seq` ()
    )
  , noGeneric @ValidatedLengthCounterexampleSimplification
      "ValidatedLengthCounterexampleSimplification"
  , ( "ValidatedLengthCounterexampleSimplification constructor became public"
    , forbiddenValidatedLengthCounterexampleSimplificationConstruction `seq` ()
    )
  , noGeneric @ValidatedLengthSpinePairCounterexampleSimplification
      "ValidatedLengthSpinePairCounterexampleSimplification"
  , ( "ValidatedLengthSpinePairCounterexampleSimplification constructor became public"
    , forbiddenValidatedLengthSpinePairCounterexampleSimplificationConstruction
        `seq` ()
    )
  , ( "Counterexample-simplification scalar and product receipts unexpectedly permit Coercible"
    , forbiddenValidatedLengthCounterexampleSimplificationCoercion `seq` ()
    )
  , noGeneric @ValidatedLengthInputBox "ValidatedLengthInputBox"
  , ( "ValidatedLengthInputBox constructor became public"
    , forbiddenValidatedLengthInputBoxConstruction `seq` ()
    )
  , noGeneric @ValidatedLengthSpinePairInputBox
      "ValidatedLengthSpinePairInputBox"
  , ( "ValidatedLengthSpinePairInputBox constructor became public"
    , forbiddenValidatedLengthSpinePairInputBoxConstruction `seq` ()
    )
  , noGeneric @ValidatedLengthApplicableDomain
      "ValidatedLengthApplicableDomain"
  , ( "ValidatedLengthApplicableDomain constructor became public"
    , forbiddenValidatedLengthApplicableDomainConstruction `seq` ()
    )
  , noGeneric @ValidatedLengthSpinePairApplicableDomain
      "ValidatedLengthSpinePairApplicableDomain"
  , ( "ValidatedLengthSpinePairApplicableDomain constructor became public"
    , forbiddenValidatedLengthSpinePairApplicableDomainConstruction `seq` ()
    )
  , ( "Applicable-domain scalar and product receipts unexpectedly permit Coercible"
    , forbiddenValidatedLengthApplicableDomainCoercion `seq` ()
    )
  , noGeneric @ValidatedLengthPositiveAffineApplicableDomain
      "ValidatedLengthPositiveAffineApplicableDomain"
  , ( "ValidatedLengthPositiveAffineApplicableDomain constructor became public"
    , forbiddenValidatedLengthPositiveAffineApplicableDomainConstruction
        `seq` ()
    )
  , noGeneric @ValidatedLengthSpinePairPositiveAffineApplicableDomain
      "ValidatedLengthSpinePairPositiveAffineApplicableDomain"
  , ( "ValidatedLengthSpinePairPositiveAffineApplicableDomain constructor became public"
    , forbiddenValidatedLengthSpinePairPositiveAffineApplicableDomainConstruction
        `seq` ()
    )
  , ( "Positive-affine scalar and product receipts unexpectedly permit Coercible"
    , forbiddenValidatedLengthPositiveAffineApplicableDomainCoercion `seq` ()
    )
  , noGeneric @ValidatedLengthRelationalPositiveAffineApplicableDomain
      "ValidatedLengthRelationalPositiveAffineApplicableDomain"
  , ( "ValidatedLengthRelationalPositiveAffineApplicableDomain constructor became public"
    , forbiddenValidatedLengthRelationalPositiveAffineApplicableDomainConstruction
        `seq` ()
    )
  , noGeneric @ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomain
      "ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomain"
  , ( "ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomain constructor became public"
    , forbiddenValidatedLengthSpinePairRelationalPositiveAffineApplicableDomainConstruction
        `seq` ()
    )
  , ( "Relational-positive-affine scalar and product receipts unexpectedly permit Coercible"
    , forbiddenValidatedLengthRelationalPositiveAffineApplicableDomainCoercion
        `seq` ()
    )
  , noGeneric @(CheckedLengthProviderSummary LengthVariableProbe)
      "CheckedLengthProviderSummary"
  , noGeneric @(CheckedLengthProviderInventory LengthVariableProbe)
      "CheckedLengthProviderInventory"
  , noGeneric @LengthProblemLimits "LengthProblemLimits"
  , noGeneric @CheckedLengthInterpretationPolicy
      "CheckedLengthInterpretationPolicy"
  , noEq @CheckedLengthInterpretationPolicy
      "CheckedLengthInterpretationPolicy"
  , noOrd @CheckedLengthInterpretationPolicy
      "CheckedLengthInterpretationPolicy"
  , noShow @CheckedLengthInterpretationPolicy
      "CheckedLengthInterpretationPolicy"
  , ( "CheckedLengthInterpretationPolicy unexpectedly permits Coercible"
    , forbiddenCheckedLengthInterpretationPolicyCoercion `seq` ()
    )
  , noGeneric
      @(CheckedLengthSession LengthVariableProbe LengthAnnotationProbe)
      "CheckedLengthSession"
  , noGeneric
      @(CheckedLengthCandidate LengthVariableProbe LengthLocalProbe)
      "CheckedLengthCandidate"
  , noGeneric
      @(CheckedLengthProblem LengthVariableProbe LengthLocalProbe)
      "CheckedLengthProblem"
  , noGeneric
      @(CheckedLengthSpinePairCandidate LengthVariableProbe LengthLocalProbe)
      "CheckedLengthSpinePairCandidate"
  , noGeneric
      @(CheckedLengthSpinePairProblem LengthVariableProbe LengthLocalProbe)
      "CheckedLengthSpinePairProblem"
  , noGeneric @LengthSMTLibLimits "LengthSMTLibLimits"
  , noGeneric
      @(LengthSMTLibQuery LengthVariableProbe LengthLocalProbe)
      "LengthSMTLibQuery"
  , noGeneric
      @(LengthSpinePairSMTLibQuery LengthVariableProbe LengthLocalProbe)
      "LengthSpinePairSMTLibQuery"
  , noGeneric
      @(AssociatedLengthSMTLibSolverObservation
          LengthVariableProbe LengthLocalProbe
          ArtifactKindProbe ArtifactKindProbe ArtifactKindProbe)
      "AssociatedLengthSMTLibSolverObservation"
  , noGeneric @LengthSMTLibResponseLimits
      "LengthSMTLibResponseLimits"
  , noGeneric @LengthSMTLibExecutionLimits
      "LengthSMTLibExecutionLimits"
  , noGeneric @LengthSMTLibExecutionConfig
      "LengthSMTLibExecutionConfig"
  , noOrd @LengthSMTLibExecutionConfig
      "LengthSMTLibExecutionConfig"
  , noShow @LengthSMTLibExecutionConfig
      "LengthSMTLibExecutionConfig"
  , ( "LengthSMTLibExecutionConfig exposed its executable path"
    , forbiddenLengthSMTLibExecutionPathProjection `seq` ()
    )
  , ( "LengthSMTLibExecutionConfig exposed its executable digest pin"
    , forbiddenLengthSMTLibExecutionDigestProjection `seq` ()
    )
  , ( "LengthSMTLibExecutionConfig exposed its reversible fingerprint"
    , forbiddenLengthSMTLibExecutionFingerprintProjection `seq` ()
    )
  , ( "LengthSMTLibExecutionConfig exposed its generic Z3 profile"
    , forbiddenLengthSMTLibExecutionZ3ProfileProjection `seq` ()
    )
  , noGeneric @(LengthSMTLibLiveSession LiveEpochProbe)
      "LengthSMTLibLiveSession"
  , noEq @(LengthSMTLibLiveSession LiveEpochProbe)
      "LengthSMTLibLiveSession"
  , noOrd @(LengthSMTLibLiveSession LiveEpochProbe)
      "LengthSMTLibLiveSession"
  , noShow @(LengthSMTLibLiveSession LiveEpochProbe)
      "LengthSMTLibLiveSession"
  , noGeneric @LengthSMTLibLiveUsableWorkBudget
      "LengthSMTLibLiveUsableWorkBudget"
  , noShow @LengthSMTLibLiveUsableWorkBudget
      "LengthSMTLibLiveUsableWorkBudget"
  , noGeneric
      @(LengthSMTLibLiveUsableWorkDeadline LiveBudgetProbe)
      "LengthSMTLibLiveUsableWorkDeadline"
  , noEq
      @(LengthSMTLibLiveUsableWorkDeadline LiveBudgetProbe)
      "LengthSMTLibLiveUsableWorkDeadline"
  , noOrd
      @(LengthSMTLibLiveUsableWorkDeadline LiveBudgetProbe)
      "LengthSMTLibLiveUsableWorkDeadline"
  , noShow
      @(LengthSMTLibLiveUsableWorkDeadline LiveBudgetProbe)
      "LengthSMTLibLiveUsableWorkDeadline"
  , noGeneric
      @(LengthSMTLibLiveQueryObservation
        LiveEpochProbe LiveIdentityProbe LiveLocalProbe)
      "LengthSMTLibLiveQueryObservation"
  , noEq
      @(LengthSMTLibLiveQueryObservation
        LiveEpochProbe LiveIdentityProbe LiveLocalProbe)
      "LengthSMTLibLiveQueryObservation"
  , noOrd
      @(LengthSMTLibLiveQueryObservation
        LiveEpochProbe LiveIdentityProbe LiveLocalProbe)
      "LengthSMTLibLiveQueryObservation"
  , noShow
      @(LengthSMTLibLiveQueryObservation
        LiveEpochProbe LiveIdentityProbe LiveLocalProbe)
      "LengthSMTLibLiveQueryObservation"
  , noGeneric
      @(LengthSpinePairSMTLibLiveQueryObservation
        LiveEpochProbe LiveIdentityProbe LiveLocalProbe)
      "LengthSpinePairSMTLibLiveQueryObservation"
  , noEq
      @(LengthSpinePairSMTLibLiveQueryObservation
        LiveEpochProbe LiveIdentityProbe LiveLocalProbe)
      "LengthSpinePairSMTLibLiveQueryObservation"
  , noOrd
      @(LengthSpinePairSMTLibLiveQueryObservation
        LiveEpochProbe LiveIdentityProbe LiveLocalProbe)
      "LengthSpinePairSMTLibLiveQueryObservation"
  , noShow
      @(LengthSpinePairSMTLibLiveQueryObservation
        LiveEpochProbe LiveIdentityProbe LiveLocalProbe)
      "LengthSpinePairSMTLibLiveQueryObservation"
  , noGeneric @LengthSMTLibLiveSessionError
      "LengthSMTLibLiveSessionError"
  , noGeneric @LengthSMTLibLiveQueryError
      "LengthSMTLibLiveQueryError"
  , noGeneric @LengthSMTLibLiveObservationReplayError
      "LengthSMTLibLiveObservationReplayError"
  , noGeneric @LengthSpinePairSMTLibLiveQueryError
      "LengthSpinePairSMTLibLiveQueryError"
  , noGeneric @LengthSpinePairSMTLibLiveObservationReplayError
      "LengthSpinePairSMTLibLiveObservationReplayError"
  , ( "LengthSMTLibLiveSession epoch unexpectedly permits Coercible"
    , forbiddenLengthSMTLibLiveSessionCoercion `seq` ()
    )
  , ( "LengthSMTLibLiveUsableWorkDeadline budget unexpectedly permits Coercible"
    , forbiddenLengthSMTLibLiveUsableWorkDeadlineCoercion `seq` ()
    )
  , ( "LengthSMTLibLiveUsableWorkBudget exposed its newtype representation"
    , forbiddenLengthSMTLibLiveUsableWorkBudgetCoercion `seq` ()
    )
  , ( "LengthSMTLibLiveQueryObservation epoch unexpectedly permits Coercible"
    , forbiddenLengthSMTLibLiveObservationEpochCoercion `seq` ()
    )
  , ( "LengthSMTLibLiveQueryObservation identity unexpectedly permits Coercible"
    , forbiddenLengthSMTLibLiveObservationIdentityCoercion `seq` ()
    )
  , ( "LengthSMTLibLiveQueryObservation local unexpectedly permits Coercible"
    , forbiddenLengthSMTLibLiveObservationLocalCoercion `seq` ()
    )
  , ( "LengthSpinePairSMTLibLiveQueryObservation epoch unexpectedly permits Coercible"
    , forbiddenLengthSpinePairSMTLibLiveObservationEpochCoercion `seq` ()
    )
  , ( "LengthSpinePairSMTLibLiveQueryObservation identity unexpectedly permits Coercible"
    , forbiddenLengthSpinePairSMTLibLiveObservationIdentityCoercion `seq` ()
    )
  , ( "LengthSpinePairSMTLibLiveQueryObservation local unexpectedly permits Coercible"
    , forbiddenLengthSpinePairSMTLibLiveObservationLocalCoercion `seq` ()
    )
  , ( "LengthSMTLibLiveSession exposed its private worker"
    , forbiddenLengthSMTLibLiveSessionWorkerProjection `seq` ()
    )
  , ( "LengthSMTLibLiveQueryObservation exposed its ordinal"
    , forbiddenLengthSMTLibLiveObservationOrdinalProjection `seq` ()
    )
  , ( "LengthSMTLibLiveQueryObservation exposed decoded input values"
    , forbiddenLengthSMTLibLiveObservationInputValuesProjection `seq` ()
    )
  , ( "LengthSMTLibLiveQueryObservation exposed its whole solver observation"
    , forbiddenLengthSMTLibLiveObservationSolverObservationProjection `seq` ()
    )
  , ( "LengthSMTLibLiveQueryObservation exposed its reversible run identity"
    , forbiddenLengthSMTLibLiveObservationRunIdentityProjection `seq` ()
    )
  , ( "LengthSMTLibLiveQueryObservation exposed its transcript digest"
    , forbiddenLengthSMTLibLiveObservationTranscriptDigestProjection `seq` ()
    )
  , ( "LengthSMTLibLiveQueryObservation exposed transcript bytes"
    , forbiddenLengthSMTLibLiveObservationTranscriptBytesProjection `seq` ()
    )
  , ( "LengthSMTLibLiveQueryObservation exposed stdout counters"
    , forbiddenLengthSMTLibLiveObservationStdoutProjection `seq` ()
    )
  , ( "LengthSMTLibLiveQueryObservation exposed stderr counters"
    , forbiddenLengthSMTLibLiveObservationStderrProjection `seq` ()
    )
  , ( "LengthSpinePairSMTLibLiveQueryObservation exposed its ordinal"
    , forbiddenLengthSpinePairSMTLibLiveObservationOrdinalProjection `seq` ()
    )
  , ( "LengthSpinePairSMTLibLiveQueryObservation exposed decoded input values"
    , forbiddenLengthSpinePairSMTLibLiveObservationInputValuesProjection `seq` ()
    )
  , ( "LengthSpinePairSMTLibLiveQueryObservation exposed its whole solver observation"
    , forbiddenLengthSpinePairSMTLibLiveObservationSolverObservationProjection
        `seq` ()
    )
  , ( "LengthSpinePairSMTLibLiveQueryObservation exposed its reversible run identity"
    , forbiddenLengthSpinePairSMTLibLiveObservationRunIdentityProjection `seq` ()
    )
  , ( "LengthSpinePairSMTLibLiveQueryObservation exposed its transcript digest"
    , forbiddenLengthSpinePairSMTLibLiveObservationTranscriptDigestProjection
        `seq` ()
    )
  , ( "LengthSpinePairSMTLibLiveQueryObservation exposed transcript bytes"
    , forbiddenLengthSpinePairSMTLibLiveObservationTranscriptBytesProjection
        `seq` ()
    )
  , ( "LengthSpinePairSMTLibLiveQueryObservation exposed stdout counters"
    , forbiddenLengthSpinePairSMTLibLiveObservationStdoutProjection `seq` ()
    )
  , ( "LengthSpinePairSMTLibLiveQueryObservation exposed stderr counters"
    , forbiddenLengthSpinePairSMTLibLiveObservationStderrProjection `seq` ()
    )
  , ( "LengthSMTLibLiveSessionError exposed child-controlled bytes"
    , forbiddenLengthSMTLibLiveSessionErrorBytesProjection `seq` ()
    )
  , ( "LengthSMTLibLiveQueryError exposed child-controlled bytes"
    , forbiddenLengthSMTLibLiveQueryErrorBytesProjection `seq` ()
    )
  , ( "LengthSpinePairSMTLibLiveQueryError exposed child-controlled bytes"
    , forbiddenLengthSpinePairSMTLibLiveQueryErrorBytesProjection `seq` ()
    )
  , ( "LengthSMTLibLive facade exposed the internal ready worker type"
    , forbiddenLengthSMTLibReadyWorkerTypeExposure `seq` ()
    )
  , ( "LengthSMTLibLive facade exposed the internal session config type"
    , forbiddenLengthSMTLibSessionConfigTypeExposure `seq` ()
    )
  , ( "LengthSMTLibLive facade exposed the internal query run type"
    , forbiddenLengthSMTLibQueryRunTypeExposure `seq` ()
    )
  , ( "LengthSMTLibLive facade exposed the internal pair query run type"
    , forbiddenLengthSpinePairSMTLibQueryRunTypeExposure `seq` ()
    )
  , ( "LengthSMTLibLive facade exposed the internal process type"
    , forbiddenLengthSMTLibProcessTypeExposure `seq` ()
    )
  , ( "CheckedLengthContract variable unexpectedly permits Coercible"
    , forbiddenCheckedLengthContractCoercion `seq` ()
    )
  , ( "CheckedLengthSpinePairContract variable unexpectedly permits Coercible"
    , forbiddenCheckedLengthSpinePairContractCoercion `seq` ()
    )
  , ( "CheckedLengthContext variable unexpectedly permits Coercible"
    , forbiddenCheckedLengthContextVariableCoercion `seq` ()
    )
  , ( "CheckedLengthContext annotation unexpectedly permits Coercible"
    , forbiddenCheckedLengthContextAnnotationCoercion `seq` ()
    )
  , ( "CheckedLengthSpineModel variable unexpectedly permits Coercible"
    , forbiddenCheckedLengthSpineModelCoercion `seq` ()
    )
  , ( "CheckedLengthProviderSummary variable unexpectedly permits Coercible"
    , forbiddenCheckedLengthProviderSummaryCoercion `seq` ()
    )
  , ( "CheckedLengthProviderInventory variable unexpectedly permits Coercible"
    , forbiddenCheckedLengthProviderInventoryCoercion `seq` ()
    )
  , ( "CheckedLengthSession identity unexpectedly permits Coercible"
    , forbiddenCheckedLengthSessionIdentityCoercion `seq` ()
    )
  , ( "CheckedLengthSession annotation unexpectedly permits Coercible"
    , forbiddenCheckedLengthSessionAnnotationCoercion `seq` ()
    )
  , ( "CheckedLengthCandidate identity unexpectedly permits Coercible"
    , forbiddenCheckedLengthCandidateIdentityCoercion `seq` ()
    )
  , ( "CheckedLengthCandidate local unexpectedly permits Coercible"
    , forbiddenCheckedLengthCandidateLocalCoercion `seq` ()
    )
  , ( "CheckedLengthProblem identity unexpectedly permits Coercible"
    , forbiddenCheckedLengthProblemIdentityCoercion `seq` ()
    )
  , ( "CheckedLengthProblem local unexpectedly permits Coercible"
    , forbiddenCheckedLengthProblemLocalCoercion `seq` ()
    )
  , ( "CheckedLengthSpinePairCandidate identity unexpectedly permits Coercible"
    , forbiddenCheckedLengthSpinePairCandidateIdentityCoercion `seq` ()
    )
  , ( "CheckedLengthSpinePairCandidate local unexpectedly permits Coercible"
    , forbiddenCheckedLengthSpinePairCandidateLocalCoercion `seq` ()
    )
  , ( "CheckedLengthSpinePairProblem identity unexpectedly permits Coercible"
    , forbiddenCheckedLengthSpinePairProblemIdentityCoercion `seq` ()
    )
  , ( "CheckedLengthSpinePairProblem local unexpectedly permits Coercible"
    , forbiddenCheckedLengthSpinePairProblemLocalCoercion `seq` ()
    )
  , ( "LengthSMTLibQuery identity unexpectedly permits Coercible"
    , forbiddenLengthSMTLibQueryIdentityCoercion `seq` ()
    )
  , ( "LengthSMTLibQuery local unexpectedly permits Coercible"
    , forbiddenLengthSMTLibQueryLocalCoercion `seq` ()
    )
  , ( "LengthSpinePairSMTLibQuery identity unexpectedly permits Coercible"
    , forbiddenLengthSpinePairSMTLibQueryIdentityCoercion `seq` ()
    )
  , ( "LengthSpinePairSMTLibQuery local unexpectedly permits Coercible"
    , forbiddenLengthSpinePairSMTLibQueryLocalCoercion `seq` ()
    )
  , ( "AssociatedLengthSMTLibSolverObservation identity unexpectedly permits Coercible"
    , forbiddenAssociatedLengthSMTLibIdentityCoercion `seq` ()
    )
  , ( "AssociatedLengthSMTLibSolverObservation local unexpectedly permits Coercible"
    , forbiddenAssociatedLengthSMTLibLocalCoercion `seq` ()
    )
  , ( "AssociatedLengthSMTLibSolverObservation satisfiable artifact unexpectedly permits Coercible"
    , forbiddenAssociatedLengthSMTLibSatisfiableCoercion `seq` ()
    )
  , ( "AssociatedLengthSMTLibSolverObservation unsatisfiable artifact unexpectedly permits Coercible"
    , forbiddenAssociatedLengthSMTLibUnsatisfiableCoercion `seq` ()
    )
  , ( "AssociatedLengthSMTLibSolverObservation unknown artifact unexpectedly permits Coercible"
    , forbiddenAssociatedLengthSMTLibUnknownCoercion `seq` ()
    )
  , ( "AssociatedLengthSMTLibSolverObservation regained a raw payload projection"
    , forbiddenAssociatedLengthSMTLibObservationProjection `seq` ()
    )
  , ( "AssociatedLengthSMTLibSolverObservation regained a nested problem projection"
    , forbiddenAssociatedLengthSMTLibProblemObservationProjection `seq` ()
    )
  , noGeneric @(Inventory Int ()) "Inventory"
  , noGeneric @(PreparedClassIndex Int) "PreparedClassIndex"
  , noGeneric @(PreparedClass Int) "PreparedClass"
  , noGeneric @(PreparedInstance Int) "PreparedInstance"
  , noGeneric @(TypeSynonyms Int) "TypeSynonyms"
  , noGeneric @(PreparedInventory Int ()) "PreparedInventory"
  , noGeneric @(PreparedInventoryExpansion Int ())
      "PreparedInventoryExpansion"
  , noGeneric @(QueryResult () ()) "QueryResult"
  , noGeneric @(CachedQuery () () ()) "CachedQuery"
  , noGeneric @DefinitionName "DefinitionName"
  , noGeneric @Name "Name"
  , noGeneric @ModuleName "ModuleName"
  , noGeneric @SourcePosition "SourcePosition"
  , noGeneric @SourceSpan "SourceSpan"
  , noGeneric @SourceLocation "SourceLocation"
  , noGeneric @(DuplicateSummary Int) "DuplicateSummary"
  , noGeneric @DjinnSession "DjinnSession"
  , noGeneric @DjinnRequest "DjinnRequest"
  , noGeneric @ExferenceSession "ExferenceSession"
  , noGeneric @ExferenceEnvironment "ExferenceEnvironment"
  , noGeneric @ExferenceRequest "ExferenceRequest"
  , noGeneric @ExferenceCore.ExferenceEnvironment
      "Core.ExferenceEnvironment"
  , noGeneric @ExferenceSourceTypeVariableHints
      "ExferenceSourceTypeVariableHints"
  , noGeneric @(PreparedSynthesisInventory ())
      "PreparedSynthesisInventory"
  , noGeneric @RigidInstantiationContext "RigidInstantiationContext"
  , noGeneric @RigidInstantiationPlan "RigidInstantiationPlan"
  , noGeneric @StaticClassEnv "StaticClassEnv"
  , noGeneric @QueryClassEnv "QueryClassEnv"
  , noGeneric @ScopeId "ScopeId"
  , noGeneric @(Scopes ()) "Scopes"
  , noGeneric @ProofEnvironment "ProofEnvironment"
  , noField
      @"inventoryEnvironment"
      @(Inventory Int ())
      @(Environment Int Void ())
      "Inventory.inventoryEnvironment"
  , noField
      @"inventoryKindAssumptions"
      @(Inventory Int ())
      @KindAssumptions
      "Inventory.inventoryKindAssumptions"
  , noField
      @"preparedClasses"
      @(PreparedClassIndex Int)
      @[PreparedClass Int]
      "PreparedClassIndex.preparedClasses"
  , noField
      @"preparedClassParameters"
      @(PreparedClass Int)
      @[(Int, Maybe GroundKind)]
      "PreparedClass.preparedClassParameters"
  , noField
      @"preparedInstanceHead"
      @(PreparedInstance Int)
      @(Constraint (Type Int))
      "PreparedInstance.preparedInstanceHead"
  , noField
      @"resultEvidence"
      @(QueryResult () ())
      @QueryEvidence
      "QueryResult.resultEvidence"
  , noField
      @"resultSearch"
      @(QueryResult () ())
      @(SearchBatch () ())
      "QueryResult.resultSearch"
  , noField
      @"typedCandidateCompatibility"
      @(TypedCandidate () (Type Int) Int ())
      @()
      "TypedCandidate.typedCandidateCompatibility"
  , noField
      @"typedCandidateTermGraph"
      @(TypedCandidate () (Type Int) Int ())
      @(Either () (TermGraph (Type Int) Int))
      "TypedCandidate.typedCandidateTermGraph"
  , noField
      @"rigidInstantiations"
      @RigidInstantiationPlan
      @[(Int, Int)]
      "RigidInstantiationPlan.rigidInstantiations"
  , noField
      @"preparedInventory"
      @(PreparedInventory Int ())
      @(Inventory Int ())
      "PreparedInventory.preparedInventory"
  , noField
      @"preparedTypeSynonyms"
      @(PreparedInventory Int ())
      @(TypeSynonyms Int)
      "PreparedInventory.preparedTypeSynonyms"
  , noField
      @"inventoryExpansionPreparedInventory"
      @(PreparedInventoryExpansion Int ())
      @(PreparedInventory Int ())
      "PreparedInventoryExpansion.inventoryExpansionPreparedInventory"
  , noField
      @"inventoryExpansionDeclarations"
      @(PreparedInventoryExpansion Int ())
      @[Declaration Int Void ()]
      "PreparedInventoryExpansion.inventoryExpansionDeclarations"
  , noField
      @"inventoryExpansionRecursiveDataTypeNames"
      @(PreparedInventoryExpansion Int ())
      @(Set.Set Name)
      "PreparedInventoryExpansion.inventoryExpansionRecursiveDataTypeNames"
  , noField
      @"preparedSynthesisWitness"
      @(PreparedSynthesisInventory ())
      @(PreparedInventory SynthesisVariable ())
      "PreparedSynthesisInventory.preparedSynthesisWitness"
  , noField
      @"preparedSynthesisBackend"
      @(PreparedSynthesisInventory ())
      @EnvDictionary
      "PreparedSynthesisInventory.preparedSynthesisBackend"
  , noField
      @"sClassEnv_tclasses"
      @StaticClassEnv
      @(Map.Map Name HsTypeClass)
      "StaticClassEnv.sClassEnv_tclasses"
  , noField
      @"sClassEnv_explicitInstances"
      @StaticClassEnv
      @[HsInstance]
      "StaticClassEnv.sClassEnv_explicitInstances"
  , noField
      @"sClassEnv_instances"
      @StaticClassEnv
      @(Map.Map Name [HsInstance])
      "StaticClassEnv.sClassEnv_instances"
  , noField
      @"qClassEnv_env"
      @QueryClassEnv
      @StaticClassEnv
      "QueryClassEnv.qClassEnv_env"
  , noField
      @"qClassEnv_constraints"
      @QueryClassEnv
      @(Set.Set HsConstraint)
      "QueryClassEnv.qClassEnv_constraints"
  , noField
      @"qClassEnv_inflatedConstraints"
      @QueryClassEnv
      @(Set.Set HsConstraint)
      "QueryClassEnv.qClassEnv_inflatedConstraints"
  , noField
      @"proofBindings"
      @ProofEnvironment
      @[(Symbol, Formula)]
      "ProofEnvironment.proofBindings"
  , noField
      @"proofBindingsIncludingTarget"
      @ProofEnvironment
      @[(Symbol, Formula)]
      "ProofEnvironment.proofBindingsIncludingTarget"
  , noField
      @"targetWasExcluded"
      @ProofEnvironment
      @Bool
      "ProofEnvironment.targetWasExcluded"
  , noField
      @"checkedLengthContractTarget"
      @(CheckedLengthContract LengthVariableProbe)
      @(Type LengthVariableProbe)
      "CheckedLengthContract.checkedLengthContractTarget"
  , noField
      @"checkedLengthContractTargetArgumentRoles"
      @(CheckedLengthContract LengthVariableProbe)
      @[LengthTargetArgumentRole]
      "CheckedLengthContract.checkedLengthContractTargetArgumentRoles"
  , noField
      @"lengthContextInventory"
      @(CheckedLengthContext LengthVariableProbe LengthAnnotationProbe)
      @(Inventory LengthVariableProbe LengthAnnotationProbe)
      "CheckedLengthContext.lengthContextInventory"
  , noField
      @"lengthContextSpineModel"
      @(CheckedLengthContext LengthVariableProbe LengthAnnotationProbe)
      @(CheckedLengthSpineModel LengthVariableProbe)
      "CheckedLengthContext.lengthContextSpineModel"
  , noField
      @"checkedLengthSpineTypeName"
      @(CheckedLengthSpineModel LengthVariableProbe)
      @Name
      "CheckedLengthSpineModel.checkedLengthSpineTypeName"
  , noField
      @"checkedLengthSpineZeroConstructor"
      @(CheckedLengthSpineModel LengthVariableProbe)
      @Name
      "CheckedLengthSpineModel.checkedLengthSpineZeroConstructor"
  , noField
      @"checkedLengthSpineStepConstructor"
      @(CheckedLengthSpineModel LengthVariableProbe)
      @Name
      "CheckedLengthSpineModel.checkedLengthSpineStepConstructor"
  , noField
      @"checkedLengthSpineRecursiveField"
      @(CheckedLengthSpineModel LengthVariableProbe)
      @Int
      "CheckedLengthSpineModel.checkedLengthSpineRecursiveField"
  , noField
      @"checkedLengthSpineModelTrust"
      @(CheckedLengthSpineModel LengthVariableProbe)
      @LengthSpineModelTrust
      "CheckedLengthSpineModel.checkedLengthSpineModelTrust"
  , noField
      @"lengthTypeNodeLimit"
      @LengthLimits
      @Int
      "LengthLimits.lengthTypeNodeLimit"
  , noField
      @"lengthContractInputLimit"
      @LengthLimits
      @Int
      "LengthLimits.lengthContractInputLimit"
  , noField
      @"lengthSyntaxNodeLimit"
      @LengthLimits
      @Int
      "LengthLimits.lengthSyntaxNodeLimit"
  , noField
      @"lengthFormulaClauseLimit"
      @LengthLimits
      @Int
      "LengthLimits.lengthFormulaClauseLimit"
  , noField
      @"lengthCollectionWidthLimit"
      @LengthLimits
      @Int
      "LengthLimits.lengthCollectionWidthLimit"
  , noField
      @"lengthProviderSummaryLimit"
      @LengthLimits
      @Int
      "LengthLimits.lengthProviderSummaryLimit"
  , noField
      @"lengthProviderArgumentLimit"
      @LengthLimits
      @Int
      "LengthLimits.lengthProviderArgumentLimit"
  , noField
      @"lengthLiteralBitLimit"
      @LengthLimits
      @Int
      "LengthLimits.lengthLiteralBitLimit"
  , noField
      @"lengthFingerprintByteLimit"
      @LengthLimits
      @Int
      "LengthLimits.lengthFingerprintByteLimit"
  , noField
      @"lengthAssignmentValueBitLimit"
      @LengthEvaluationLimits
      @Int
      "LengthEvaluationLimits.lengthAssignmentValueBitLimit"
  , noField
      @"lengthIntermediateValueBitLimit"
      @LengthEvaluationLimits
      @Int
      "LengthEvaluationLimits.lengthIntermediateValueBitLimit"
  , noField
      @"checkedLengthContractInputCount"
      @(CheckedLengthContract LengthVariableProbe)
      @Int
      "CheckedLengthContract.checkedLengthContractInputCount"
  , noField
      @"checkedLengthContractPrecondition"
      @(CheckedLengthContract LengthVariableProbe)
      @(LengthFormula LengthContractVariable)
      "CheckedLengthContract.checkedLengthContractPrecondition"
  , noField
      @"checkedLengthContractPostcondition"
      @(CheckedLengthContract LengthVariableProbe)
      @(LengthFormula LengthContractVariable)
      "CheckedLengthContract.checkedLengthContractPostcondition"
  , noField
      @"lengthContractFingerprint"
      @(CheckedLengthContract LengthVariableProbe)
      @(Fingerprint LengthContractFingerprintSubject)
      "CheckedLengthContract.lengthContractFingerprint"
  , noField
      @"checkedLengthSpinePairContractTarget"
      @(CheckedLengthSpinePairContract LengthVariableProbe)
      @(Type LengthVariableProbe)
      "CheckedLengthSpinePairContract.checkedLengthSpinePairContractTarget"
  , noField
      @"checkedLengthSpinePairContractTargetArgumentRoles"
      @(CheckedLengthSpinePairContract LengthVariableProbe)
      @[LengthTargetArgumentRole]
      "CheckedLengthSpinePairContract.targetArgumentRoles"
  , noField
      @"checkedLengthSpinePairContractInputCount"
      @(CheckedLengthSpinePairContract LengthVariableProbe)
      @Int
      "CheckedLengthSpinePairContract.inputCount"
  , noField
      @"checkedLengthSpinePairContractPrecondition"
      @(CheckedLengthSpinePairContract LengthVariableProbe)
      @(LengthFormula LengthSpinePairContractVariable)
      "CheckedLengthSpinePairContract.precondition"
  , noField
      @"checkedLengthSpinePairContractPostcondition"
      @(CheckedLengthSpinePairContract LengthVariableProbe)
      @(LengthFormula LengthSpinePairContractVariable)
      "CheckedLengthSpinePairContract.postcondition"
  , noField
      @"lengthSpinePairContractFingerprint"
      @(CheckedLengthSpinePairContract LengthVariableProbe)
      @(Fingerprint LengthSpinePairContractFingerprintSubject)
      "CheckedLengthSpinePairContract.fingerprint"
  , noField
      @"checkedLengthProviderName"
      @(CheckedLengthProviderSummary LengthVariableProbe)
      @Name
      "CheckedLengthProviderSummary.checkedLengthProviderName"
  , noField
      @"checkedLengthProviderScheme"
      @(CheckedLengthProviderSummary LengthVariableProbe)
      @(Type LengthVariableProbe)
      "CheckedLengthProviderSummary.checkedLengthProviderScheme"
  , noField
      @"checkedLengthProviderArgumentRoles"
      @(CheckedLengthProviderSummary LengthVariableProbe)
      @[LengthProviderArgumentRole]
      "CheckedLengthProviderSummary.checkedLengthProviderArgumentRoles"
  , noField
      @"checkedLengthProviderTransfer"
      @(CheckedLengthProviderSummary LengthVariableProbe)
      @(LengthExpression LengthProviderVariable)
      "CheckedLengthProviderSummary.checkedLengthProviderTransfer"
  , noField
      @"checkedLengthProviderTrust"
      @(CheckedLengthProviderSummary LengthVariableProbe)
      @LengthProviderTrust
      "CheckedLengthProviderSummary.checkedLengthProviderTrust"
  , noField
      @"checkedLengthProviderSummaries"
      @(CheckedLengthProviderInventory LengthVariableProbe)
      @[CheckedLengthProviderSummary LengthVariableProbe]
      "CheckedLengthProviderInventory.checkedLengthProviderSummaries"
  , noField
      @"lengthProviderInventoryFingerprint"
      @(CheckedLengthProviderInventory LengthVariableProbe)
      @(Fingerprint LengthProviderInventoryFingerprintSubject)
      "CheckedLengthProviderInventory.lengthProviderInventoryFingerprint"
  , noField
      @"lengthSMTLibLiveSessionPrimaryFailure"
      @LengthSMTLibLiveSessionError
      @LengthSMTLibLiveSessionFailure
      "LengthSMTLibLiveSessionError.lengthSMTLibLiveSessionPrimaryFailure"
  , noField
      @"lengthSMTLibLiveSessionCleanupIncomplete"
      @LengthSMTLibLiveSessionError
      @Bool
      "LengthSMTLibLiveSessionError.lengthSMTLibLiveSessionCleanupIncomplete"
  , noField
      @"lengthSMTLibLiveQueryPrimaryFailure"
      @LengthSMTLibLiveQueryError
      @LengthSMTLibLiveQueryFailure
      "LengthSMTLibLiveQueryError.lengthSMTLibLiveQueryPrimaryFailure"
  , noField
      @"lengthSMTLibLiveQueryCleanupIncomplete"
      @LengthSMTLibLiveQueryError
      @Bool
      "LengthSMTLibLiveQueryError.lengthSMTLibLiveQueryCleanupIncomplete"
  , noField
      @"lengthSMTLibLiveQueryObservationQueryFingerprint"
      @(LengthSMTLibLiveQueryObservation
        LiveEpochProbe LiveIdentityProbe LiveLocalProbe)
      @(Fingerprint LengthSMTLibQueryFingerprintSubject)
      "LengthSMTLibLiveQueryObservation.queryFingerprint"
  , noField
      @"lengthSMTLibLiveQueryObservationSolverStatus"
      @(LengthSMTLibLiveQueryObservation
        LiveEpochProbe LiveIdentityProbe LiveLocalProbe)
      @SolverStatus
      "LengthSMTLibLiveQueryObservation.solverStatus"
  , noField
      @"lengthSMTLibLiveQueryObservationResultStrength"
      @(LengthSMTLibLiveQueryObservation
        LiveEpochProbe LiveIdentityProbe LiveLocalProbe)
      @RawResultStrength
      "LengthSMTLibLiveQueryObservation.resultStrength"
  , noField
      @"lengthSMTLibLiveQueryObservationUse"
      @(LengthSMTLibLiveQueryObservation
        LiveEpochProbe LiveIdentityProbe LiveLocalProbe)
      @RawObservationUse
      "LengthSMTLibLiveQueryObservation.use"
  , noField
      @"lengthSMTLibLiveQueryObservationCounterexampleEvidence"
      @(LengthSMTLibLiveQueryObservation
        LiveEpochProbe LiveIdentityProbe LiveLocalProbe)
      @(Maybe
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthCounterexample))
      "LengthSMTLibLiveQueryObservation.counterexampleEvidence"
  , noField
      @"lengthSMTLibLiveQueryObservationSolverObservation"
      @(LengthSMTLibLiveQueryObservation
        LiveEpochProbe LiveIdentityProbe LiveLocalProbe)
      @(SolverObservation
        (Maybe
          (BehavioralEvidence
            FiniteListSpineLengthV1
            ValidatedLengthCounterexample))
        ()
        ())
      "LengthSMTLibLiveQueryObservation.solverObservation"
  , noField
      @"lengthSpinePairSMTLibLiveQueryPrimaryFailure"
      @LengthSpinePairSMTLibLiveQueryError
      @LengthSpinePairSMTLibLiveQueryFailure
      "LengthSpinePairSMTLibLiveQueryError.lengthSpinePairSMTLibLiveQueryPrimaryFailure"
  , noField
      @"lengthSpinePairSMTLibLiveQueryCleanupIncomplete"
      @LengthSpinePairSMTLibLiveQueryError
      @Bool
      "LengthSpinePairSMTLibLiveQueryError.lengthSpinePairSMTLibLiveQueryCleanupIncomplete"
  , noField
      @"lengthSpinePairSMTLibLiveQueryObservationQueryFingerprint"
      @(LengthSpinePairSMTLibLiveQueryObservation
        LiveEpochProbe LiveIdentityProbe LiveLocalProbe)
      @(Fingerprint LengthSpinePairSMTLibQueryFingerprintSubject)
      "LengthSpinePairSMTLibLiveQueryObservation.queryFingerprint"
  , noField
      @"lengthSpinePairSMTLibLiveQueryObservationSolverStatus"
      @(LengthSpinePairSMTLibLiveQueryObservation
        LiveEpochProbe LiveIdentityProbe LiveLocalProbe)
      @SolverStatus
      "LengthSpinePairSMTLibLiveQueryObservation.solverStatus"
  , noField
      @"lengthSpinePairSMTLibLiveQueryObservationResultStrength"
      @(LengthSpinePairSMTLibLiveQueryObservation
        LiveEpochProbe LiveIdentityProbe LiveLocalProbe)
      @RawResultStrength
      "LengthSpinePairSMTLibLiveQueryObservation.resultStrength"
  , noField
      @"lengthSpinePairSMTLibLiveQueryObservationUse"
      @(LengthSpinePairSMTLibLiveQueryObservation
        LiveEpochProbe LiveIdentityProbe LiveLocalProbe)
      @RawObservationUse
      "LengthSpinePairSMTLibLiveQueryObservation.use"
  , noField
      @"lengthSpinePairSMTLibLiveQueryObservationCounterexampleEvidence"
      @(LengthSpinePairSMTLibLiveQueryObservation
        LiveEpochProbe LiveIdentityProbe LiveLocalProbe)
      @(Maybe
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairCounterexample))
      "LengthSpinePairSMTLibLiveQueryObservation.counterexampleEvidence"
  , noField
      @"lengthSpinePairSMTLibLiveQueryObservationSolverObservation"
      @(LengthSpinePairSMTLibLiveQueryObservation
        LiveEpochProbe LiveIdentityProbe LiveLocalProbe)
      @(SolverObservation
        (Maybe
          (BehavioralEvidence
            FiniteBinaryProductSpineLengthsV1
            ValidatedLengthSpinePairCounterexample))
        ()
        ())
      "LengthSpinePairSMTLibLiveQueryObservation.solverObservation"
  ]

-- Positive controls prove that both dictionary-forcing helpers work and that
-- a public record label is visible to the built-in 'HasField' solver.
allowedConstructionAttempts :: [(String, ())]
allowedConstructionAttempts =
  [ ("QueryRequest Generic", genericMethod @(QueryRequest () ()))
  , ( "Fingerprint same-domain Coercible"
    , coercibleMethod
        @(Fingerprint FingerprintProbe)
        @(Fingerprint FingerprintProbe)
    )
  , ( "QueryRequest.requestGoal HasField"
    , fieldMethod @"requestGoal" @(QueryRequest () ()) @()
    )
  ]

noGeneric :: forall value. Generic value => String -> (String, ())
noGeneric label =
  (label ++ " unexpectedly has Generic", genericMethod @value)

noOrd :: forall value. Ord value => String -> (String, ())
noOrd label =
  (label ++ " unexpectedly has Ord", ordMethod @value)

noEq :: forall value. Eq value => String -> (String, ())
noEq label =
  (label ++ " unexpectedly has Eq", eqMethod @value)

noShow :: forall value. Show value => String -> (String, ())
noShow label =
  (label ++ " unexpectedly has Show", showMethod @value)

-- Selecting the method forces the instance dictionary without needing a
-- value of the abstract type or forcing a representation.
genericMethod :: forall value. Generic value => ()
genericMethod = (from :: value -> Rep value ()) `seq` ()

ordMethod :: forall value. Ord value => ()
ordMethod = (compare :: value -> value -> Ordering) `seq` ()

eqMethod :: forall value. Eq value => ()
eqMethod = ((==) :: value -> value -> Bool) `seq` ()

showMethod :: forall value. Show value => ()
showMethod = (show :: value -> String) `seq` ()

-- Keep the deferred role error in this binding's RHS.  The surrounding
-- negative fixture can then force it inside its exception handler; if the
-- subject role ever becomes phantom, this turns into a real coercion and the
-- negative test fails.
forbiddenFingerprintCoercion
  :: Fingerprint FingerprintProbe
  -> Fingerprint OtherFingerprintProbe
forbiddenFingerprintCoercion = coerce

-- Every parameter of a retained typed-candidate association is nominal.  In
-- particular, a representationally equal compatibility payload cannot be
-- substituted while keeping the graph checked for the original candidate.
forbiddenTypedCandidateCoercion
  :: TypedCandidate () (Type Int) Int TypedCandidateProbe
  -> TypedCandidate () (Type Int) Int OtherTypedCandidateProbe
forbiddenTypedCandidateCoercion = coerce

forbiddenTypedCandidateFailureCoercion
  :: TypedCandidate TypedCandidateFailureProbe (Type Int) Int ()
  -> TypedCandidate OtherTypedCandidateFailureProbe (Type Int) Int ()
forbiddenTypedCandidateFailureCoercion = coerce

forbiddenTypedCandidateTypeCoercion
  :: TypedCandidate () TypedCandidateTypeProbe Int ()
  -> TypedCandidate () OtherTypedCandidateTypeProbe Int ()
forbiddenTypedCandidateTypeCoercion = coerce

forbiddenTypedCandidateLocalCoercion
  :: TypedCandidate () (Type Int) TypedCandidateLocalProbe ()
  -> TypedCandidate () (Type Int) OtherTypedCandidateLocalProbe ()
forbiddenTypedCandidateLocalCoercion = coerce

-- A projected graph remains bound to the exact type and local-identity
-- domains under which it was sealed. TypedCandidate's nominal roles cannot
-- protect those domains after 'typedCandidateTermGraph' projects the graph.
forbiddenTermGraphTypeCoercion
  :: TermGraph LengthVariableProbe Int
  -> TermGraph OtherLengthVariableProbe Int
forbiddenTermGraphTypeCoercion = coerce

forbiddenTermGraphLocalCoercion
  :: TermGraph Int LengthLocalProbe
  -> TermGraph Int OtherLengthLocalProbe
forbiddenTermGraphLocalCoercion = coerce

forbiddenBehavioralProblemCoercion
  :: BehavioralProblem BehavioralDomainProbe
  -> BehavioralProblem OtherBehavioralDomainProbe
forbiddenBehavioralProblemCoercion = coerce

forbiddenBehavioralFingerprintRoleCoercion
  :: Fingerprint (InventoryFingerprintSubject BehavioralDomainProbe)
  -> Fingerprint (EncodingFingerprintSubject BehavioralDomainProbe)
forbiddenBehavioralFingerprintRoleCoercion = coerce

forbiddenAssociatedObservationDomainCoercion
  :: AssociatedObservation BehavioralDomainProbe ObservationProbe
  -> AssociatedObservation OtherBehavioralDomainProbe ObservationProbe
forbiddenAssociatedObservationDomainCoercion = coerce

forbiddenAssociatedObservationPayloadCoercion
  :: AssociatedObservation BehavioralDomainProbe ObservationProbe
  -> AssociatedObservation BehavioralDomainProbe OtherObservationProbe
forbiddenAssociatedObservationPayloadCoercion = coerce

-- These names must remain absent from the downstream public module.  If a
-- direct payload selector is reintroduced, either binding becomes an ordinary
-- function and its negative construction attempt unexpectedly succeeds.
forbiddenAssociatedObservationProjection
  :: AssociatedObservation BehavioralDomainProbe ObservationProbe
  -> ObservationProbe
forbiddenAssociatedObservationProjection = associatedObservation

forbiddenBehavioralEvidenceReceiptCoercion
  :: BehavioralEvidence BehavioralDomainProbe EvidenceReceiptProbe
  -> BehavioralEvidence BehavioralDomainProbe OtherEvidenceReceiptProbe
forbiddenBehavioralEvidenceReceiptCoercion = coerce

forbiddenBehavioralEvidenceDomainCoercion
  :: BehavioralEvidence BehavioralDomainProbe EvidenceReceiptProbe
  -> BehavioralEvidence OtherBehavioralDomainProbe EvidenceReceiptProbe
forbiddenBehavioralEvidenceDomainCoercion = coerce

forbiddenBehavioralEvidenceReceiptProjection
  :: BehavioralEvidence BehavioralDomainProbe EvidenceReceiptProbe
  -> EvidenceReceiptProbe
forbiddenBehavioralEvidenceReceiptProjection = behavioralEvidenceReceipt

forbiddenBehavioralEvidenceConstruction
  :: BehavioralProblem BehavioralDomainProbe
  -> EvidenceReceiptProbe
  -> BehavioralEvidence BehavioralDomainProbe EvidenceReceiptProbe
forbiddenBehavioralEvidenceConstruction = mkBehavioralEvidence

forbiddenValidatedLengthCounterexampleConstruction
  :: ValidatedLengthCounterexample
forbiddenValidatedLengthCounterexampleConstruction =
  ValidatedLengthCounterexampleReceipt
    [] 0 ProviderIndependentFiniteSpineModel

forbiddenValidatedLengthSpinePairCounterexampleConstruction
  :: ValidatedLengthSpinePairCounterexample
forbiddenValidatedLengthSpinePairCounterexampleConstruction =
  ValidatedLengthSpinePairCounterexampleReceipt
    [] (LengthSpinePair 0 0) ProviderIndependentFiniteSpineModel

forbiddenValidatedLengthCounterexampleSimplificationConstruction
  :: ValidatedLengthCounterexampleSimplification
forbiddenValidatedLengthCounterexampleSimplificationConstruction =
  ValidatedLengthCounterexampleSimplificationReceipt
    [] [] 0 forbiddenValidatedLengthCounterexampleConstruction

forbiddenValidatedLengthSpinePairCounterexampleSimplificationConstruction
  :: ValidatedLengthSpinePairCounterexampleSimplification
forbiddenValidatedLengthSpinePairCounterexampleSimplificationConstruction =
  ValidatedLengthSpinePairCounterexampleSimplificationReceipt
    [] [] 0 forbiddenValidatedLengthSpinePairCounterexampleConstruction

forbiddenValidatedLengthCounterexampleSimplificationCoercion
  :: ValidatedLengthCounterexampleSimplification
  -> ValidatedLengthSpinePairCounterexampleSimplification
forbiddenValidatedLengthCounterexampleSimplificationCoercion = coerce

forbiddenValidatedLengthInputBoxConstruction :: ValidatedLengthInputBox
forbiddenValidatedLengthInputBoxConstruction =
  ValidatedLengthInputBoxReceipt
    [] [] 0 0 ProviderIndependentFiniteSpineModel

forbiddenValidatedLengthSpinePairInputBoxConstruction
  :: ValidatedLengthSpinePairInputBox
forbiddenValidatedLengthSpinePairInputBoxConstruction =
  ValidatedLengthSpinePairInputBoxReceipt
    [] [] 0 0 ProviderIndependentFiniteSpineModel

forbiddenValidatedLengthApplicableDomainConstruction
  :: ValidatedLengthApplicableDomain
forbiddenValidatedLengthApplicableDomainConstruction =
  ValidatedLengthApplicableDomainReceipt
    [] forbiddenValidatedLengthInputBoxConstruction

forbiddenValidatedLengthSpinePairApplicableDomainConstruction
  :: ValidatedLengthSpinePairApplicableDomain
forbiddenValidatedLengthSpinePairApplicableDomainConstruction =
  ValidatedLengthSpinePairApplicableDomainReceipt
    [] forbiddenValidatedLengthSpinePairInputBoxConstruction

forbiddenValidatedLengthApplicableDomainCoercion
  :: ValidatedLengthApplicableDomain
  -> ValidatedLengthSpinePairApplicableDomain
forbiddenValidatedLengthApplicableDomainCoercion = coerce

forbiddenValidatedLengthPositiveAffineApplicableDomainConstruction
  :: ValidatedLengthPositiveAffineApplicableDomain
forbiddenValidatedLengthPositiveAffineApplicableDomainConstruction =
  ValidatedLengthPositiveAffineApplicableDomainReceipt
    [] forbiddenValidatedLengthInputBoxConstruction

forbiddenValidatedLengthSpinePairPositiveAffineApplicableDomainConstruction
  :: ValidatedLengthSpinePairPositiveAffineApplicableDomain
forbiddenValidatedLengthSpinePairPositiveAffineApplicableDomainConstruction =
  ValidatedLengthSpinePairPositiveAffineApplicableDomainReceipt
    [] forbiddenValidatedLengthSpinePairInputBoxConstruction

forbiddenValidatedLengthPositiveAffineApplicableDomainCoercion
  :: ValidatedLengthPositiveAffineApplicableDomain
  -> ValidatedLengthSpinePairPositiveAffineApplicableDomain
forbiddenValidatedLengthPositiveAffineApplicableDomainCoercion = coerce

forbiddenValidatedLengthRelationalPositiveAffineApplicableDomainConstruction
  :: ValidatedLengthRelationalPositiveAffineApplicableDomain
forbiddenValidatedLengthRelationalPositiveAffineApplicableDomainConstruction =
  ValidatedLengthRelationalPositiveAffineApplicableDomainReceipt
    [] forbiddenValidatedLengthInputBoxConstruction

forbiddenValidatedLengthSpinePairRelationalPositiveAffineApplicableDomainConstruction
  :: ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomain
forbiddenValidatedLengthSpinePairRelationalPositiveAffineApplicableDomainConstruction =
  ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomainReceipt
    [] forbiddenValidatedLengthSpinePairInputBoxConstruction

forbiddenValidatedLengthRelationalPositiveAffineApplicableDomainCoercion
  :: ValidatedLengthRelationalPositiveAffineApplicableDomain
  -> ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomain
forbiddenValidatedLengthRelationalPositiveAffineApplicableDomainCoercion =
  coerce

forbiddenBoundedRawArtifactCoercion
  :: BoundedRawArtifact ArtifactKindProbe
  -> BoundedRawArtifact OtherArtifactKindProbe
forbiddenBoundedRawArtifactCoercion = coerce

forbiddenCheckedLengthContractCoercion
  :: CheckedLengthContract LengthVariableProbe
  -> CheckedLengthContract OtherLengthVariableProbe
forbiddenCheckedLengthContractCoercion = coerce

forbiddenCheckedLengthSpinePairContractCoercion
  :: CheckedLengthSpinePairContract LengthVariableProbe
  -> CheckedLengthSpinePairContract OtherLengthVariableProbe
forbiddenCheckedLengthSpinePairContractCoercion = coerce

forbiddenCheckedLengthContextVariableCoercion
  :: CheckedLengthContext LengthVariableProbe LengthAnnotationProbe
  -> CheckedLengthContext OtherLengthVariableProbe LengthAnnotationProbe
forbiddenCheckedLengthContextVariableCoercion = coerce

forbiddenCheckedLengthContextAnnotationCoercion
  :: CheckedLengthContext LengthVariableProbe LengthAnnotationProbe
  -> CheckedLengthContext LengthVariableProbe OtherLengthAnnotationProbe
forbiddenCheckedLengthContextAnnotationCoercion = coerce

forbiddenCheckedLengthSpineModelCoercion
  :: CheckedLengthSpineModel LengthVariableProbe
  -> CheckedLengthSpineModel OtherLengthVariableProbe
forbiddenCheckedLengthSpineModelCoercion = coerce

forbiddenCheckedLengthProviderSummaryCoercion
  :: CheckedLengthProviderSummary LengthVariableProbe
  -> CheckedLengthProviderSummary OtherLengthVariableProbe
forbiddenCheckedLengthProviderSummaryCoercion = coerce

forbiddenCheckedLengthProviderInventoryCoercion
  :: CheckedLengthProviderInventory LengthVariableProbe
  -> CheckedLengthProviderInventory OtherLengthVariableProbe
forbiddenCheckedLengthProviderInventoryCoercion = coerce

forbiddenCheckedLengthInterpretationPolicyCoercion
  :: CheckedLengthInterpretationPolicy
  -> OtherCheckedLengthInterpretationPolicy
forbiddenCheckedLengthInterpretationPolicyCoercion = coerce

forbiddenCheckedLengthSessionIdentityCoercion
  :: CheckedLengthSession LengthVariableProbe LengthAnnotationProbe
  -> CheckedLengthSession OtherLengthVariableProbe LengthAnnotationProbe
forbiddenCheckedLengthSessionIdentityCoercion = coerce

forbiddenCheckedLengthSessionAnnotationCoercion
  :: CheckedLengthSession LengthVariableProbe LengthAnnotationProbe
  -> CheckedLengthSession LengthVariableProbe OtherLengthAnnotationProbe
forbiddenCheckedLengthSessionAnnotationCoercion = coerce

forbiddenCheckedLengthCandidateIdentityCoercion
  :: CheckedLengthCandidate LengthVariableProbe LengthLocalProbe
  -> CheckedLengthCandidate OtherLengthVariableProbe LengthLocalProbe
forbiddenCheckedLengthCandidateIdentityCoercion = coerce

forbiddenCheckedLengthCandidateLocalCoercion
  :: CheckedLengthCandidate LengthVariableProbe LengthLocalProbe
  -> CheckedLengthCandidate LengthVariableProbe OtherLengthLocalProbe
forbiddenCheckedLengthCandidateLocalCoercion = coerce

forbiddenCheckedLengthProblemIdentityCoercion
  :: CheckedLengthProblem LengthVariableProbe LengthLocalProbe
  -> CheckedLengthProblem OtherLengthVariableProbe LengthLocalProbe
forbiddenCheckedLengthProblemIdentityCoercion = coerce

forbiddenCheckedLengthProblemLocalCoercion
  :: CheckedLengthProblem LengthVariableProbe LengthLocalProbe
  -> CheckedLengthProblem LengthVariableProbe OtherLengthLocalProbe
forbiddenCheckedLengthProblemLocalCoercion = coerce

forbiddenCheckedLengthSpinePairCandidateIdentityCoercion
  :: CheckedLengthSpinePairCandidate LengthVariableProbe LengthLocalProbe
  -> CheckedLengthSpinePairCandidate OtherLengthVariableProbe LengthLocalProbe
forbiddenCheckedLengthSpinePairCandidateIdentityCoercion = coerce

forbiddenCheckedLengthSpinePairCandidateLocalCoercion
  :: CheckedLengthSpinePairCandidate LengthVariableProbe LengthLocalProbe
  -> CheckedLengthSpinePairCandidate LengthVariableProbe OtherLengthLocalProbe
forbiddenCheckedLengthSpinePairCandidateLocalCoercion = coerce

forbiddenCheckedLengthSpinePairProblemIdentityCoercion
  :: CheckedLengthSpinePairProblem LengthVariableProbe LengthLocalProbe
  -> CheckedLengthSpinePairProblem OtherLengthVariableProbe LengthLocalProbe
forbiddenCheckedLengthSpinePairProblemIdentityCoercion = coerce

forbiddenCheckedLengthSpinePairProblemLocalCoercion
  :: CheckedLengthSpinePairProblem LengthVariableProbe LengthLocalProbe
  -> CheckedLengthSpinePairProblem LengthVariableProbe OtherLengthLocalProbe
forbiddenCheckedLengthSpinePairProblemLocalCoercion = coerce

forbiddenLengthSMTLibQueryIdentityCoercion
  :: LengthSMTLibQuery LengthVariableProbe LengthLocalProbe
  -> LengthSMTLibQuery OtherLengthVariableProbe LengthLocalProbe
forbiddenLengthSMTLibQueryIdentityCoercion = coerce

forbiddenLengthSMTLibQueryLocalCoercion
  :: LengthSMTLibQuery LengthVariableProbe LengthLocalProbe
  -> LengthSMTLibQuery LengthVariableProbe OtherLengthLocalProbe
forbiddenLengthSMTLibQueryLocalCoercion = coerce

forbiddenLengthSpinePairSMTLibQueryIdentityCoercion
  :: LengthSpinePairSMTLibQuery LengthVariableProbe LengthLocalProbe
  -> LengthSpinePairSMTLibQuery OtherLengthVariableProbe LengthLocalProbe
forbiddenLengthSpinePairSMTLibQueryIdentityCoercion = coerce

forbiddenLengthSpinePairSMTLibQueryLocalCoercion
  :: LengthSpinePairSMTLibQuery LengthVariableProbe LengthLocalProbe
  -> LengthSpinePairSMTLibQuery LengthVariableProbe OtherLengthLocalProbe
forbiddenLengthSpinePairSMTLibQueryLocalCoercion = coerce

forbiddenAssociatedLengthSMTLibIdentityCoercion
  :: AssociatedLengthSMTLibSolverObservation
      LengthVariableProbe LengthLocalProbe
      ArtifactKindProbe ArtifactKindProbe ArtifactKindProbe
  -> AssociatedLengthSMTLibSolverObservation
      OtherLengthVariableProbe LengthLocalProbe
      ArtifactKindProbe ArtifactKindProbe ArtifactKindProbe
forbiddenAssociatedLengthSMTLibIdentityCoercion = coerce

forbiddenAssociatedLengthSMTLibLocalCoercion
  :: AssociatedLengthSMTLibSolverObservation
      LengthVariableProbe LengthLocalProbe
      ArtifactKindProbe ArtifactKindProbe ArtifactKindProbe
  -> AssociatedLengthSMTLibSolverObservation
      LengthVariableProbe OtherLengthLocalProbe
      ArtifactKindProbe ArtifactKindProbe ArtifactKindProbe
forbiddenAssociatedLengthSMTLibLocalCoercion = coerce

forbiddenAssociatedLengthSMTLibSatisfiableCoercion
  :: AssociatedLengthSMTLibSolverObservation
      LengthVariableProbe LengthLocalProbe
      ArtifactKindProbe ArtifactKindProbe ArtifactKindProbe
  -> AssociatedLengthSMTLibSolverObservation
      LengthVariableProbe LengthLocalProbe
      OtherArtifactKindProbe ArtifactKindProbe ArtifactKindProbe
forbiddenAssociatedLengthSMTLibSatisfiableCoercion = coerce

forbiddenAssociatedLengthSMTLibUnsatisfiableCoercion
  :: AssociatedLengthSMTLibSolverObservation
      LengthVariableProbe LengthLocalProbe
      ArtifactKindProbe ArtifactKindProbe ArtifactKindProbe
  -> AssociatedLengthSMTLibSolverObservation
      LengthVariableProbe LengthLocalProbe
      ArtifactKindProbe OtherArtifactKindProbe ArtifactKindProbe
forbiddenAssociatedLengthSMTLibUnsatisfiableCoercion = coerce

forbiddenAssociatedLengthSMTLibUnknownCoercion
  :: AssociatedLengthSMTLibSolverObservation
      LengthVariableProbe LengthLocalProbe
      ArtifactKindProbe ArtifactKindProbe ArtifactKindProbe
  -> AssociatedLengthSMTLibSolverObservation
      LengthVariableProbe LengthLocalProbe
      ArtifactKindProbe ArtifactKindProbe OtherArtifactKindProbe
forbiddenAssociatedLengthSMTLibUnknownCoercion = coerce

forbiddenAssociatedLengthSMTLibObservationProjection
  :: AssociatedLengthSMTLibSolverObservation
      LengthVariableProbe LengthLocalProbe
      ArtifactKindProbe ArtifactKindProbe ArtifactKindProbe
  -> LengthSMTLibRawSolverObservation
      ArtifactKindProbe ArtifactKindProbe ArtifactKindProbe
forbiddenAssociatedLengthSMTLibObservationProjection =
  associatedLengthSMTLibObservation

forbiddenAssociatedLengthSMTLibProblemObservationProjection
  :: AssociatedLengthSMTLibSolverObservation
      LengthVariableProbe LengthLocalProbe
      ArtifactKindProbe ArtifactKindProbe ArtifactKindProbe
  -> AssociatedObservation FiniteListSpineLengthV1
      (LengthSMTLibRawSolverObservation
        ArtifactKindProbe ArtifactKindProbe ArtifactKindProbe)
forbiddenAssociatedLengthSMTLibProblemObservationProjection =
  associatedLengthSMTLibProblemObservation

-- Launch spellings and the reversible complete fingerprint stay private.
-- Future process execution belongs inside the checked package boundary rather
-- than in downstream code which could leak these policy details.
forbiddenLengthSMTLibExecutionPathProjection
  :: LengthSMTLibExecutionConfig
  -> FilePath
forbiddenLengthSMTLibExecutionPathProjection =
  lengthSMTLibExecutionExecutablePath

forbiddenLengthSMTLibExecutionDigestProjection
  :: LengthSMTLibExecutionConfig
  -> Maybe [Word8]
forbiddenLengthSMTLibExecutionDigestProjection =
  lengthSMTLibExecutionExpectedExecutableSHA256

forbiddenLengthSMTLibExecutionFingerprintProjection
  :: LengthSMTLibExecutionConfig
  -> ()
forbiddenLengthSMTLibExecutionFingerprintProjection =
  lengthSMTLibExecutionPolicyFingerprint `seq` const ()

forbiddenLengthSMTLibExecutionZ3ProfileProjection
  :: LengthSMTLibExecutionConfig
  -> ()
forbiddenLengthSMTLibExecutionZ3ProfileProjection =
  lengthSMTLibExecutionZ3Profile `seq` const ()

forbiddenLengthSMTLibLiveSessionCoercion
  :: LengthSMTLibLiveSession LiveEpochProbe
  -> LengthSMTLibLiveSession OtherLiveEpochProbe
forbiddenLengthSMTLibLiveSessionCoercion = coerce

forbiddenLengthSMTLibLiveUsableWorkDeadlineCoercion
  :: LengthSMTLibLiveUsableWorkDeadline LiveBudgetProbe
  -> LengthSMTLibLiveUsableWorkDeadline OtherLiveBudgetProbe
forbiddenLengthSMTLibLiveUsableWorkDeadlineCoercion = coerce

forbiddenLengthSMTLibLiveUsableWorkBudgetCoercion
  :: LengthSMTLibLiveUsableWorkBudget
  -> Int
forbiddenLengthSMTLibLiveUsableWorkBudgetCoercion = coerce

forbiddenLengthSMTLibLiveObservationEpochCoercion
  :: LengthSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe LiveLocalProbe
  -> LengthSMTLibLiveQueryObservation
      OtherLiveEpochProbe LiveIdentityProbe LiveLocalProbe
forbiddenLengthSMTLibLiveObservationEpochCoercion = coerce

forbiddenLengthSMTLibLiveObservationIdentityCoercion
  :: LengthSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe LiveLocalProbe
  -> LengthSMTLibLiveQueryObservation
      LiveEpochProbe OtherLiveIdentityProbe LiveLocalProbe
forbiddenLengthSMTLibLiveObservationIdentityCoercion = coerce

forbiddenLengthSMTLibLiveObservationLocalCoercion
  :: LengthSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe LiveLocalProbe
  -> LengthSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe OtherLiveLocalProbe
forbiddenLengthSMTLibLiveObservationLocalCoercion = coerce

forbiddenLengthSpinePairSMTLibLiveObservationEpochCoercion
  :: LengthSpinePairSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe LiveLocalProbe
  -> LengthSpinePairSMTLibLiveQueryObservation
      OtherLiveEpochProbe LiveIdentityProbe LiveLocalProbe
forbiddenLengthSpinePairSMTLibLiveObservationEpochCoercion = coerce

forbiddenLengthSpinePairSMTLibLiveObservationIdentityCoercion
  :: LengthSpinePairSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe LiveLocalProbe
  -> LengthSpinePairSMTLibLiveQueryObservation
      LiveEpochProbe OtherLiveIdentityProbe LiveLocalProbe
forbiddenLengthSpinePairSMTLibLiveObservationIdentityCoercion = coerce

forbiddenLengthSpinePairSMTLibLiveObservationLocalCoercion
  :: LengthSpinePairSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe LiveLocalProbe
  -> LengthSpinePairSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe OtherLiveLocalProbe
forbiddenLengthSpinePairSMTLibLiveObservationLocalCoercion = coerce

forbiddenLengthSMTLibLiveSessionWorkerProjection
  :: LengthSMTLibLiveSession LiveEpochProbe
  -> ()
forbiddenLengthSMTLibLiveSessionWorkerProjection =
  lengthSMTLibLiveSessionWorker `seq` const ()

forbiddenLengthSMTLibLiveObservationOrdinalProjection
  :: LengthSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe LiveLocalProbe
  -> ()
forbiddenLengthSMTLibLiveObservationOrdinalProjection =
  lengthSMTLibLiveQueryObservationOrdinal `seq` const ()

forbiddenLengthSMTLibLiveObservationInputValuesProjection
  :: LengthSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe LiveLocalProbe
  -> ()
forbiddenLengthSMTLibLiveObservationInputValuesProjection =
  lengthSMTLibLiveQueryObservationInputValues `seq` const ()

forbiddenLengthSMTLibLiveObservationSolverObservationProjection
  :: LengthSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe LiveLocalProbe
  -> ()
forbiddenLengthSMTLibLiveObservationSolverObservationProjection =
  lengthSMTLibLiveQueryObservationSolverObservation `seq` const ()

forbiddenLengthSMTLibLiveObservationRunIdentityProjection
  :: LengthSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe LiveLocalProbe
  -> ()
forbiddenLengthSMTLibLiveObservationRunIdentityProjection =
  lengthSMTLibLiveQueryObservationRunIdentityFingerprint `seq` const ()

forbiddenLengthSMTLibLiveObservationTranscriptDigestProjection
  :: LengthSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe LiveLocalProbe
  -> ()
forbiddenLengthSMTLibLiveObservationTranscriptDigestProjection =
  lengthSMTLibLiveQueryObservationTranscriptSHA256 `seq` const ()

forbiddenLengthSMTLibLiveObservationTranscriptBytesProjection
  :: LengthSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe LiveLocalProbe
  -> ()
forbiddenLengthSMTLibLiveObservationTranscriptBytesProjection =
  lengthSMTLibLiveQueryObservationTranscriptByteCount `seq` const ()

forbiddenLengthSMTLibLiveObservationStdoutProjection
  :: LengthSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe LiveLocalProbe
  -> ()
forbiddenLengthSMTLibLiveObservationStdoutProjection =
  lengthSMTLibLiveQueryObservationStdoutCounters `seq` const ()

forbiddenLengthSMTLibLiveObservationStderrProjection
  :: LengthSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe LiveLocalProbe
  -> ()
forbiddenLengthSMTLibLiveObservationStderrProjection =
  lengthSMTLibLiveQueryObservationStderrCounters `seq` const ()

forbiddenLengthSpinePairSMTLibLiveObservationOrdinalProjection
  :: LengthSpinePairSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe LiveLocalProbe
  -> ()
forbiddenLengthSpinePairSMTLibLiveObservationOrdinalProjection =
  lengthSpinePairSMTLibLiveQueryObservationOrdinal `seq` const ()

forbiddenLengthSpinePairSMTLibLiveObservationInputValuesProjection
  :: LengthSpinePairSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe LiveLocalProbe
  -> ()
forbiddenLengthSpinePairSMTLibLiveObservationInputValuesProjection =
  lengthSpinePairSMTLibLiveQueryObservationInputValues `seq` const ()

forbiddenLengthSpinePairSMTLibLiveObservationSolverObservationProjection
  :: LengthSpinePairSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe LiveLocalProbe
  -> ()
forbiddenLengthSpinePairSMTLibLiveObservationSolverObservationProjection =
  lengthSpinePairSMTLibLiveQueryObservationSolverObservation `seq` const ()

forbiddenLengthSpinePairSMTLibLiveObservationRunIdentityProjection
  :: LengthSpinePairSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe LiveLocalProbe
  -> ()
forbiddenLengthSpinePairSMTLibLiveObservationRunIdentityProjection =
  lengthSpinePairSMTLibLiveQueryObservationRunIdentityFingerprint `seq` const ()

forbiddenLengthSpinePairSMTLibLiveObservationTranscriptDigestProjection
  :: LengthSpinePairSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe LiveLocalProbe
  -> ()
forbiddenLengthSpinePairSMTLibLiveObservationTranscriptDigestProjection =
  lengthSpinePairSMTLibLiveQueryObservationTranscriptSHA256 `seq` const ()

forbiddenLengthSpinePairSMTLibLiveObservationTranscriptBytesProjection
  :: LengthSpinePairSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe LiveLocalProbe
  -> ()
forbiddenLengthSpinePairSMTLibLiveObservationTranscriptBytesProjection =
  lengthSpinePairSMTLibLiveQueryObservationTranscriptByteCount `seq` const ()

forbiddenLengthSpinePairSMTLibLiveObservationStdoutProjection
  :: LengthSpinePairSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe LiveLocalProbe
  -> ()
forbiddenLengthSpinePairSMTLibLiveObservationStdoutProjection =
  lengthSpinePairSMTLibLiveQueryObservationStdoutCounters `seq` const ()

forbiddenLengthSpinePairSMTLibLiveObservationStderrProjection
  :: LengthSpinePairSMTLibLiveQueryObservation
      LiveEpochProbe LiveIdentityProbe LiveLocalProbe
  -> ()
forbiddenLengthSpinePairSMTLibLiveObservationStderrProjection =
  lengthSpinePairSMTLibLiveQueryObservationStderrCounters `seq` const ()

forbiddenLengthSMTLibLiveSessionErrorBytesProjection
  :: LengthSMTLibLiveSessionError
  -> ()
forbiddenLengthSMTLibLiveSessionErrorBytesProjection =
  lengthSMTLibLiveSessionErrorChildBytes `seq` const ()

forbiddenLengthSMTLibLiveQueryErrorBytesProjection
  :: LengthSMTLibLiveQueryError
  -> ()
forbiddenLengthSMTLibLiveQueryErrorBytesProjection =
  lengthSMTLibLiveQueryErrorChildBytes `seq` const ()

forbiddenLengthSpinePairSMTLibLiveQueryErrorBytesProjection
  :: LengthSpinePairSMTLibLiveQueryError
  -> ()
forbiddenLengthSpinePairSMTLibLiveQueryErrorBytesProjection =
  lengthSpinePairSMTLibLiveQueryErrorChildBytes `seq` const ()

forbiddenLengthSMTLibReadyWorkerTypeExposure
  :: ()
forbiddenLengthSMTLibReadyWorkerTypeExposure =
  LengthSMTLibReadyWorker `seq` ()

forbiddenLengthSMTLibSessionConfigTypeExposure
  :: ()
forbiddenLengthSMTLibSessionConfigTypeExposure =
  LengthSMTLibSessionConfig `seq` ()

forbiddenLengthSMTLibQueryRunTypeExposure
  :: ()
forbiddenLengthSMTLibQueryRunTypeExposure =
  LengthSMTLibQueryRun `seq` ()

forbiddenLengthSpinePairSMTLibQueryRunTypeExposure
  :: ()
forbiddenLengthSpinePairSMTLibQueryRunTypeExposure =
  LengthSpinePairSMTLibQueryRun `seq` ()

forbiddenLengthSMTLibProcessTypeExposure
  :: ()
forbiddenLengthSMTLibProcessTypeExposure =
  LengthSMTLibProcess `seq` ()

-- Selecting 'coerce' forces the built-in coercion evidence without requiring
-- a value of either abstract type.
coercibleMethod
  :: forall source target
   . Coercible source target
  => ()
coercibleMethod = (coerce :: source -> target) `seq` ()

noField
  :: forall (label :: TypeLits.Symbol) record field
   . HasField label record field
  => String
  -> (String, ())
noField description =
  ( description ++ " unexpectedly remains a record field"
  , fieldMethod @label @record @field
  )

-- Selecting a method forces its instance dictionary without requiring a
-- record value. Keeping every probed projection in this expression is also
-- semantically significant: a built-in 'HasField' constraint is solvable only
-- when the corresponding selector is in scope.
fieldMethod
  :: forall (label :: TypeLits.Symbol) record field
   . HasField label record field
  => ()
fieldMethod = selectorNamesInScope `seq`
  getField @label @record @field `seq`
  (Proxy @label `seq` Proxy @record `seq` Proxy @field `seq` ())

selectorNamesInScope :: ()
selectorNamesInScope =
  inventoryEnvironment `seq`
  inventoryKindAssumptions `seq`
  resultEvidence `seq`
  resultSearch `seq`
  typedCandidateCompatibility `seq`
  typedCandidateTermGraph `seq`
  rigidInstantiations `seq`
  preparedInventory `seq`
  preparedTypeSynonyms `seq`
  inventoryExpansionPreparedInventory `seq`
  inventoryExpansionDeclarations `seq`
  inventoryExpansionRecursiveDataTypeNames `seq`
  preparedSynthesisWitness `seq`
  preparedSynthesisBackend `seq`
  sClassEnv_tclasses `seq`
  sClassEnv_explicitInstances `seq`
  sClassEnv_instances `seq`
  qClassEnv_env `seq`
  qClassEnv_constraints `seq`
  qClassEnv_inflatedConstraints `seq`
  proofBindings `seq`
  proofBindingsIncludingTarget `seq`
  targetWasExcluded `seq`
  lengthSMTLibLiveSessionPrimaryFailure `seq`
  lengthSMTLibLiveSessionCleanupIncomplete `seq`
  lengthSMTLibLiveQueryPrimaryFailure `seq`
  lengthSMTLibLiveQueryCleanupIncomplete `seq`
  lengthSMTLibLiveQueryObservationSolverStatus `seq`
  lengthSMTLibLiveQueryObservationResultStrength `seq`
  lengthSMTLibLiveQueryObservationUse `seq`
  lengthSpinePairSMTLibLiveQueryPrimaryFailure `seq`
  lengthSpinePairSMTLibLiveQueryCleanupIncomplete `seq`
  lengthSpinePairSMTLibLiveQueryObservationSolverStatus `seq`
  lengthSpinePairSMTLibLiveQueryObservationResultStrength `seq`
  lengthSpinePairSMTLibLiveQueryObservationUse `seq`
  ()
