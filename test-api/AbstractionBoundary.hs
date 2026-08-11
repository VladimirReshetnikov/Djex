{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Djinn.Internal.LJTFormula (Formula, Symbol)
-- Whole-module imports are deliberate for modules whose former record labels
-- are probed below. GHC solves built-in 'HasField' constraints only when the
-- corresponding field selector is in scope; importing just the owner type
-- would let a record-field regression pass this test unnoticed.
import Djinn.Internal.ProofEnv
import GHC.Generics (Generic, Rep, from)
import GHC.Records (HasField, getField)
import qualified GHC.TypeLits as TypeLits
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
  ( TermGraphFingerprintSubject )

data FingerprintProbe
data OtherFingerprintProbe
newtype TypedCandidateProbe = TypedCandidateProbe Int
newtype OtherTypedCandidateProbe = OtherTypedCandidateProbe Int
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
  , ( "TypedCandidate payload unexpectedly permits Coercible"
    , forbiddenTypedCandidateCoercion `seq` ()
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
  , noGeneric
      @(CheckedLengthContext LengthVariableProbe LengthAnnotationProbe)
      "CheckedLengthContext"
  , noGeneric @(CheckedLengthSpineModel LengthVariableProbe)
      "CheckedLengthSpineModel"
  , noGeneric @LengthLimits "LengthLimits"
  , noGeneric @LengthEvaluationLimits "LengthEvaluationLimits"
  , noGeneric @(CheckedLengthProviderSummary LengthVariableProbe)
      "CheckedLengthProviderSummary"
  , noGeneric @(CheckedLengthProviderInventory LengthVariableProbe)
      "CheckedLengthProviderInventory"
  , ( "CheckedLengthContract variable unexpectedly permits Coercible"
    , forbiddenCheckedLengthContractCoercion `seq` ()
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

-- Selecting the method forces the instance dictionary without needing a
-- value of the abstract type or forcing a representation.
genericMethod :: forall value. Generic value => ()
genericMethod = (from :: value -> Rep value ()) `seq` ()

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

forbiddenBoundedRawArtifactCoercion
  :: BoundedRawArtifact ArtifactKindProbe
  -> BoundedRawArtifact OtherArtifactKindProbe
forbiddenBoundedRawArtifactCoercion = coerce

forbiddenCheckedLengthContractCoercion
  :: CheckedLengthContract LengthVariableProbe
  -> CheckedLengthContract OtherLengthVariableProbe
forbiddenCheckedLengthContractCoercion = coerce

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
  ()
