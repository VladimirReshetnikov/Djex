{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Private representation and construction edge for list-length contracts.
module Language.Haskell.Synthesis.Internal.Semantic.Length
  ( FiniteListSpineLengthV1
  , LengthContractFingerprintSubject
  , LengthProviderInventoryFingerprintSubject
  , finiteListSpineLengthDomainTag
  , LengthExpression (..)
  , LengthFormula (..)
  , LengthContractVariable (..)
  , LengthContractSource (..)
  , LengthProviderArgumentRole (..)
  , LengthProviderVariable (..)
  , LengthProviderSummarySource (..)
  , LengthProviderTrust (..)
  , LengthLimitSource (..)
  , LengthLimits
  , LengthLimitField (..)
  , LengthLimitError (..)
  , mkLengthLimits
  , defaultLengthLimitSource
  , defaultLengthLimits
  , lengthTypeNodeLimit
  , lengthContractInputLimit
  , lengthSyntaxNodeLimit
  , lengthFormulaClauseLimit
  , lengthCollectionWidthLimit
  , lengthProviderSummaryLimit
  , lengthProviderArgumentLimit
  , lengthLiteralBitLimit
  , lengthFingerprintByteLimit
  , LengthSpineModelSource (..)
  , LengthSpineModelTrust (..)
  , LengthSpineModelError (..)
  , CheckedLengthSpineModel
  , checkedLengthSpineTypeName
  , checkedLengthSpineZeroConstructor
  , checkedLengthSpineStepConstructor
  , checkedLengthSpineRecursiveField
  , checkedLengthSpineModelTrust
  , CheckedLengthContext
  , sealLengthContext
  , lengthContextInventory
  , lengthContextSpineModel
  , LengthTypeCollectionSite (..)
  , LengthTypeBoundError (..)
  , LengthSyntaxCollectionSite (..)
  , LengthSyntaxError (..)
  , LengthContractError (..)
  , LengthProviderSummaryError (..)
  , LengthProviderInventoryError (..)
  , CheckedLengthContract
  , sealLengthContract
  , sealLengthContractInContext
  , checkedLengthContractTarget
  , checkedLengthContractInputCount
  , checkedLengthContractPrecondition
  , checkedLengthContractPostcondition
  , lengthContractFingerprint
  , CheckedLengthProviderSummary
  , checkedLengthProviderName
  , checkedLengthProviderScheme
  , checkedLengthProviderArgumentRoles
  , checkedLengthProviderTransfer
  , checkedLengthProviderTrust
  , CheckedLengthProviderInventory
  , sealLengthProviderInventory
  , sealLengthProviderInventoryInContext
  , checkedLengthProviderSummaries
  , lookupCheckedLengthProviderSummary
  , lengthProviderInventoryFingerprint
  ) where

import Control.DeepSeq (NFData (rnf))
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Char (ord)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Collection (observedListLength)
import Language.Haskell.Synthesis.Constraint
  ( Constraint (..)
  , constraintArguments
  )
import Language.Haskell.Synthesis.Declaration
  ( DataConstructor (..)
  , Declaration (..)
  , TypeParameter (..)
  , ValueSignature (..)
  , declarationTermSchemes
  )
import Language.Haskell.Synthesis.Environment
  ( dataConstructorMap
  , environmentDeclarations
  , typeDeclarationMap
  , valueSignatureMap
  )
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
import Language.Haskell.Synthesis.Inventory
  ( Inventory
  , inventoryEnvironment
  , inventoryKindAssumptions
  )
import Language.Haskell.Synthesis.Kind (Kind (ProperTypeKind))
import Language.Haskell.Synthesis.KindInference
  ( KindInferenceError
  , checkTypesKinds
  )
import Language.Haskell.Synthesis.Name
  ( Boxity (..)
  , Name
  , consName
  , listName
  )
import Language.Haskell.Synthesis.Type
  ( Type (..)
  , TypeError
  , freeVariables
  , functionSpine
  , normalizeType
  , splitLeadingForalls
  )

-- | Nominal marker for the exact semantic dialect.
data FiniteListSpineLengthV1

-- | Contract identity is deliberately not an encoding identity.
data LengthContractFingerprintSubject

-- | Identity of the assumed length summaries only.
--
-- This is deliberately not the generic behavioral inventory subject.  A
-- future domain builder must combine this key with the exact opaque source
-- inventory before constructing that complete identity.
data LengthProviderInventoryFingerprintSubject

-- | Exact finite runtime tag compared before every generic problem identity.
finiteListSpineLengthDomainTag :: [Word8]
finiteListSpineLengthDomainTag = ascii "finite-list-spine-length/v1"

data LengthExpression variable
  = LengthVariable variable
  | LengthLiteral Natural
  | LengthSum [LengthExpression variable]
  | LengthScale Natural (LengthExpression variable)
  | LengthMonus (LengthExpression variable) (LengthExpression variable)
  | LengthMinimum (LengthExpression variable) (LengthExpression variable)
  | LengthMaximum (LengthExpression variable) (LengthExpression variable)
  | LengthIf
      (LengthFormula variable)
      (LengthExpression variable)
      (LengthExpression variable)
  deriving (Eq, Ord, Show, Generic)

instance NFData variable => NFData (LengthExpression variable)

data LengthFormula variable
  = LengthTruth Bool
  | LengthEqual (LengthExpression variable) (LengthExpression variable)
  | LengthAtMost (LengthExpression variable) (LengthExpression variable)
  | LengthNot (LengthFormula variable)
  | LengthAll [LengthFormula variable]
  deriving (Eq, Ord, Show, Generic)

instance NFData variable => NFData (LengthFormula variable)

data LengthContractVariable
  = LengthInput Natural
  | LengthResult
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthContractVariable

data LengthContractSource = LengthContractSource
  { lengthContractPrecondition :: LengthFormula LengthContractVariable
  , lengthContractPostcondition :: LengthFormula LengthContractVariable
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthContractSource

-- | Whether the assumed transfer may reference an argument's list spine.
--
-- 'LengthSpineArgument' makes the length available; it does not require the
-- normalized transfer to mention it. 'LengthUnobservedArgument' says nothing
-- about purity, totality, strictness, effects, type reflection, or evaluation
-- of that argument by the provider.
data LengthProviderArgumentRole
  = LengthSpineArgument
  | LengthUnobservedArgument
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthProviderArgumentRole

newtype LengthProviderVariable = LengthProviderArgument Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthProviderVariable

-- | An explicitly assumed provider law, uniform over all admitted instances
-- of the exact closed scheme retained in the checked inventory.
data LengthProviderSummarySource variable = AssumedProviderSummary
  { lengthProviderName :: Name
  , lengthProviderScheme :: Type variable
  , lengthProviderArgumentRoles :: [LengthProviderArgumentRole]
  , lengthProviderTransfer :: LengthExpression LengthProviderVariable
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData variable => NFData (LengthProviderSummarySource variable)

data LengthProviderTrust = AssumedProviderLaw
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthProviderTrust

data LengthLimitSource = LengthLimitSource
  { lengthLimitSourceTypeNodes :: Int
  , lengthLimitSourceContractInputs :: Int
  , lengthLimitSourceSyntaxNodes :: Int
  , lengthLimitSourceFormulaClauses :: Int
  , lengthLimitSourceCollectionWidth :: Int
  , lengthLimitSourceProviderSummaries :: Int
  , lengthLimitSourceProviderArguments :: Int
  , lengthLimitSourceLiteralBits :: Int
  , lengthLimitSourceFingerprintBytes :: Int
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthLimitSource

data LengthLimits = LengthLimits
  !Int !Int !Int !Int !Int !Int !Int !Int !Int
  deriving (Eq, Ord, Show)

instance NFData LengthLimits where
  rnf (LengthLimits typeNodes contractInputs syntaxNodes formulaClauses
      collectionWidth providerSummaries providerArguments literalBits
      fingerprintBytes) =
    rnf typeNodes `seq`
    rnf contractInputs `seq`
    rnf syntaxNodes `seq`
    rnf formulaClauses `seq`
    rnf collectionWidth `seq`
    rnf providerSummaries `seq`
    rnf providerArguments `seq`
    rnf literalBits `seq`
    rnf fingerprintBytes

data LengthLimitField
  = LengthTypeNodes
  | LengthContractInputs
  | LengthSyntaxNodes
  | LengthFormulaClauses
  | LengthCollectionWidth
  | LengthProviderSummaries
  | LengthProviderArguments
  | LengthLiteralBits
  | LengthFingerprintBytes
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthLimitField

data LengthLimitError = NegativeLengthLimit LengthLimitField Int
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthLimitError

mkLengthLimits :: LengthLimitSource -> Either LengthLimitError LengthLimits
mkLengthLimits source = do
  nonnegative LengthTypeNodes $ lengthLimitSourceTypeNodes source
  nonnegative LengthContractInputs $ lengthLimitSourceContractInputs source
  nonnegative LengthSyntaxNodes $ lengthLimitSourceSyntaxNodes source
  nonnegative LengthFormulaClauses $ lengthLimitSourceFormulaClauses source
  nonnegative LengthCollectionWidth $ lengthLimitSourceCollectionWidth source
  nonnegative LengthProviderSummaries
    $ lengthLimitSourceProviderSummaries source
  nonnegative LengthProviderArguments
    $ lengthLimitSourceProviderArguments source
  nonnegative LengthLiteralBits $ lengthLimitSourceLiteralBits source
  nonnegative LengthFingerprintBytes
    $ lengthLimitSourceFingerprintBytes source
  pure $ LengthLimits
    (lengthLimitSourceTypeNodes source)
    (lengthLimitSourceContractInputs source)
    (lengthLimitSourceSyntaxNodes source)
    (lengthLimitSourceFormulaClauses source)
    (lengthLimitSourceCollectionWidth source)
    (lengthLimitSourceProviderSummaries source)
    (lengthLimitSourceProviderArguments source)
    (lengthLimitSourceLiteralBits source)
    (lengthLimitSourceFingerprintBytes source)
 where
  nonnegative field value
    | value < 0 = Left $ NegativeLengthLimit field value
    | otherwise = Right ()

defaultLengthLimitSource :: LengthLimitSource
defaultLengthLimitSource = LengthLimitSource
  { lengthLimitSourceTypeNodes = 4096
  , lengthLimitSourceContractInputs = 8
  , lengthLimitSourceSyntaxNodes = 1024
  , lengthLimitSourceFormulaClauses = 32
  , lengthLimitSourceCollectionWidth = 64
  , lengthLimitSourceProviderSummaries = 256
  , lengthLimitSourceProviderArguments = 16
  , lengthLimitSourceLiteralBits = 256
  , lengthLimitSourceFingerprintBytes = 65536
  }

defaultLengthLimits :: LengthLimits
defaultLengthLimits = LengthLimits 4096 8 1024 32 64 256 16 256 65536

lengthTypeNodeLimit, lengthContractInputLimit, lengthSyntaxNodeLimit,
  lengthFormulaClauseLimit, lengthCollectionWidthLimit,
  lengthProviderSummaryLimit, lengthProviderArgumentLimit,
  lengthLiteralBitLimit, lengthFingerprintByteLimit :: LengthLimits -> Int
lengthTypeNodeLimit (LengthLimits value _ _ _ _ _ _ _ _) = value
lengthContractInputLimit (LengthLimits _ value _ _ _ _ _ _ _) = value
lengthSyntaxNodeLimit (LengthLimits _ _ value _ _ _ _ _ _) = value
lengthFormulaClauseLimit (LengthLimits _ _ _ value _ _ _ _ _) = value
lengthCollectionWidthLimit (LengthLimits _ _ _ _ value _ _ _ _) = value
lengthProviderSummaryLimit (LengthLimits _ _ _ _ _ value _ _ _) = value
lengthProviderArgumentLimit (LengthLimits _ _ _ _ _ _ value _ _) = value
lengthLiteralBitLimit (LengthLimits _ _ _ _ _ _ _ value _) = value
lengthFingerprintByteLimit (LengthLimits _ _ _ _ _ _ _ _ value) = value

data LengthTypeCollectionSite
  = LengthTupleFields
  | LengthForallBinders
  | LengthForallConstraints
  | LengthConstraintArguments
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthTypeCollectionSite

data LengthTypeBoundError
  = LengthTypeNodeLimitExceeded Int Int
  | LengthTypeCollectionLimitExceeded LengthTypeCollectionSite Int Int
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthTypeBoundError

-- | Which exact unary list-like family supplies the finite spine measured by
-- this dialect.
--
-- The built-in model is the structural Haskell @[]@/@(:)@ pair.  A declared
-- model names one checked datatype and its zero and successor constructors;
-- 'sealLengthContext' derives and validates the complete two-constructor
-- schema from the same opaque source inventory retained by the context.
data LengthSpineModelSource
  = BuiltinListSpine
  | DeclaredListSpine Name Name Name
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSpineModelSource

-- | Why a checked spine schema may be treated structurally.
data LengthSpineModelTrust
  = BuiltinStructuralListSpine
  | DerivedFromListLikeDataDeclaration
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSpineModelTrust

-- | A declared spine is deliberately narrow: one proper-type parameter,
-- exactly a nullary zero constructor, and one binary successor constructor
-- whose fields are that opaque parameter and the recursive family occurrence
-- in either order.
data LengthSpineModelError variable
  = LengthSpineTypeDeclarationMissing Name
  | LengthSpineTypeIsNotData Name
  | LengthSpineParameterArityMismatch Name Int
  | LengthSpineConstructorArityMismatch Name Int
  | LengthSpineConstructorsMustDiffer Name
  | LengthSpineZeroConstructorMissing Name
  | LengthSpineStepConstructorMissing Name
  | LengthSpineZeroFieldArityMismatch Name Int
  | LengthSpineStepFieldArityMismatch Name Int
  | LengthSpineFieldBoundError Name Int LengthTypeBoundError
  | LengthSpineFieldTypeError Name Int (TypeError variable)
  | LengthSpineStepFieldsDoNotMatch
      Name (Type variable) (Type variable)
  deriving (Eq, Ord, Show, Generic)

instance NFData variable => NFData (LengthSpineModelError variable)

-- | Opaque, exact structural schema for one finite unary spine family.
data CheckedLengthSpineModel variable = CheckedLengthSpineModel
  !Name
  !Name
  !Name
  !Int
  !LengthSpineModelTrust
  !FingerprintField

type role CheckedLengthSpineModel nominal

instance NFData (CheckedLengthSpineModel variable) where
  rnf (CheckedLengthSpineModel typeName zero step recursive trust field) =
    rnf typeName `seq`
    rnf zero `seq`
    rnf step `seq`
    rnf recursive `seq`
    rnf trust `seq`
    rnfFingerprintField field

-- | Exact unary type constructor recognized as the modeled spine.
checkedLengthSpineTypeName :: CheckedLengthSpineModel variable -> Name
checkedLengthSpineTypeName (CheckedLengthSpineModel name _ _ _ _ _) = name

-- | Exact nullary constructor for the zero-length spine.
checkedLengthSpineZeroConstructor
  :: CheckedLengthSpineModel variable -> Name
checkedLengthSpineZeroConstructor
    (CheckedLengthSpineModel _ name _ _ _ _) = name

-- | Exact binary constructor for one payload and a recursive tail.
checkedLengthSpineStepConstructor
  :: CheckedLengthSpineModel variable -> Name
checkedLengthSpineStepConstructor
    (CheckedLengthSpineModel _ _ name _ _ _) = name

-- | Zero-based field containing the recursive tail in the step constructor.
checkedLengthSpineRecursiveField :: CheckedLengthSpineModel variable -> Int
checkedLengthSpineRecursiveField
    (CheckedLengthSpineModel _ _ _ field _ _) = field

-- | Recover the structural authority established while sealing the model.
checkedLengthSpineModelTrust
  :: CheckedLengthSpineModel variable -> LengthSpineModelTrust
checkedLengthSpineModelTrust
    (CheckedLengthSpineModel _ _ _ _ trust _) = trust

-- | One source inventory retained together with the exact finite-spine model
-- derived from it.  Keeping this association opaque prevents later contract
-- and provider sealing from accidentally consulting a different inventory.
data CheckedLengthContext variable annotation = CheckedLengthContext
  !(Inventory variable annotation)
  !(CheckedLengthSpineModel variable)

type role CheckedLengthContext nominal nominal

instance (NFData variable, NFData annotation) =>
    NFData (CheckedLengthContext variable annotation) where
  rnf (CheckedLengthContext inventory model) =
    rnf inventory `seq` rnf model

-- | Recover the exact source inventory retained by a checked context.
lengthContextInventory
  :: CheckedLengthContext variable annotation
  -> Inventory variable annotation
lengthContextInventory (CheckedLengthContext inventory _) = inventory

-- | Recover the spine model derived or selected for a checked context.
lengthContextSpineModel
  :: CheckedLengthContext variable annotation
  -> CheckedLengthSpineModel variable
lengthContextSpineModel (CheckedLengthContext _ model) = model

sealLengthContext
  :: Ord variable
  => LengthLimits
  -> Inventory variable annotation
  -> LengthSpineModelSource
  -> Either
      (LengthSpineModelError variable)
      (CheckedLengthContext variable annotation)
sealLengthContext _ inventory BuiltinListSpine = Right
  $ CheckedLengthContext inventory builtinListSpineModel
sealLengthContext limits inventory
    (DeclaredListSpine typeName zeroName stepName) = do
  declaration <- case Map.lookup typeName
      $ typeDeclarationMap $ inventoryEnvironment inventory of
    Nothing -> Left $ LengthSpineTypeDeclarationMissing typeName
    Just value -> Right value
  (parameters, constructors) <- case declaration of
    DataTypeDeclaration _ _ declaredParameters declaredConstructors ->
      Right (declaredParameters, declaredConstructors)
    _ -> Left $ LengthSpineTypeIsNotData typeName
  let parameterCount = observedListLength 1 parameters
  if parameterCount == 1
    then pure ()
    else Left $ LengthSpineParameterArityMismatch typeName parameterCount
  let constructorCount = observedListLength 2 constructors
  if constructorCount == 2
    then pure ()
    else Left $ LengthSpineConstructorArityMismatch typeName constructorCount
  if zeroName == stepName
    then Left $ LengthSpineConstructorsMustDiffer zeroName
    else pure ()
  zeroConstructor <- case List.find ((== zeroName) . constructorName) constructors of
    Nothing -> Left $ LengthSpineZeroConstructorMissing zeroName
    Just constructor -> Right constructor
  stepConstructor <- case List.find ((== stepName) . constructorName) constructors of
    Nothing -> Left $ LengthSpineStepConstructorMissing stepName
    Just constructor -> Right constructor
  let zeroFields = constructorFields zeroConstructor
      zeroFieldCount = observedListLength 0 zeroFields
  if zeroFieldCount == 0
    then pure ()
    else Left $ LengthSpineZeroFieldArityMismatch zeroName zeroFieldCount
  let stepFields = constructorFields stepConstructor
      stepFieldCount = observedListLength 2 stepFields
  if stepFieldCount == 2
    then pure ()
    else Left $ LengthSpineStepFieldArityMismatch stepName stepFieldCount
  parameter <- case parameters of
    [TypeParameter variable _] -> Right variable
    _ -> Left $ LengthSpineParameterArityMismatch typeName parameterCount
  normalizedFields <- mapM normalizeField $ zip [0 ..] stepFields
  let payload = TypeVariable parameter
      recursive = TypeApplication (TypeConstructor typeName) payload
  recursiveField <- case normalizedFields of
    [firstField, secondField]
      | firstField == payload && secondField == recursive -> Right 1
      | firstField == recursive && secondField == payload -> Right 0
      | otherwise -> Left $ LengthSpineStepFieldsDoNotMatch
          stepName firstField secondField
    _ -> Left $ LengthSpineStepFieldArityMismatch stepName stepFieldCount
  let model = CheckedLengthSpineModel
        typeName zeroName stepName recursiveField
        DerivedFromListLikeDataDeclaration
        $ tagged "declared-list-spine-model"
            [ FingerprintBytes $ ascii "declared-unary-list-schema/v1"
            , FingerprintName typeName
            , FingerprintName zeroName
            , FingerprintName stepName
            , FingerprintNatural $ fromIntegral recursiveField
            ]
  pure $ CheckedLengthContext inventory model
 where
  normalizeField (index, field) = do
    _ <- first (LengthSpineFieldBoundError stepName index)
      $ observeTypeWithin limits 0 field
    first (LengthSpineFieldTypeError stepName index) $ normalizeType field

builtinListSpineModel :: CheckedLengthSpineModel variable
builtinListSpineModel = CheckedLengthSpineModel
  listName listName consName 1 BuiltinStructuralListSpine
  $ tagged "builtin-list-spine-model"
      [ FingerprintBytes $ ascii "haskell-structural-list/v1"
      , FingerprintName listName
      , FingerprintName consName
      , FingerprintNatural 1
      ]

data LengthSyntaxCollectionSite
  = LengthSumTerms
  | LengthConjunctionClauses
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSyntaxCollectionSite

data LengthSyntaxError
  = LengthSyntaxNodeLimitExceeded Int Int
  | LengthFormulaClauseLimitExceeded Int Int
  | LengthSyntaxCollectionLimitExceeded
      LengthSyntaxCollectionSite Int Int
  | LengthLiteralBitLimitExceeded Int Int
  | LengthResultNotAvailableInPrecondition
  | LengthInputReferenceOutOfRange Natural Int
  | LengthProviderReferenceOutOfRange Natural Int
  | LengthProviderReferenceIsUnobserved Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSyntaxError

data LengthContractError variable
  = LengthContractTargetBoundError LengthTypeBoundError
  | LengthContractTargetTypeError (TypeError variable)
  | LengthContractTargetKindError (KindInferenceError variable)
  | LengthContractConstrainedTarget
  | LengthContractInputLimitExceeded Int Int
  | LengthContractInputIsNotList Int (Type variable)
  | LengthContractResultIsNotList (Type variable)
  | LengthContractPreconditionError LengthSyntaxError
  | LengthContractPostconditionError LengthSyntaxError
  | LengthContractFingerprintLimitExceeded Natural Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData variable => NFData (LengthContractError variable)

data LengthProviderSummaryError variable
  = LengthProviderNotInSourceInventory Name
  | LengthProviderSourceSchemeBoundError LengthTypeBoundError
  | LengthProviderSourceSchemeTypeError (TypeError variable)
  | LengthProviderSourceSchemeMismatch
      (Type variable) (Type variable)
  | LengthProviderSchemeBoundError LengthTypeBoundError
  | LengthProviderSchemeTypeError (TypeError variable)
  | LengthProviderSchemeKindError (KindInferenceError variable)
  | LengthProviderConstrainedScheme
  | LengthProviderOpenScheme [variable]
  | LengthProviderArgumentLimitExceeded Int Int
  | LengthProviderRoleArityMismatch Int Int
  | LengthProviderResultIsNotList (Type variable)
  | LengthProviderSpineArgumentIsNotList Int (Type variable)
  | LengthProviderTransferError LengthSyntaxError
  deriving (Eq, Ord, Show, Generic)

instance NFData variable => NFData (LengthProviderSummaryError variable)

data LengthProviderInventoryError variable
  = LengthProviderSummaryLimitExceeded Int Int
  | LengthProviderSummaryRejected
      Int Name (LengthProviderSummaryError variable)
  | DuplicateLengthProvider Name
  | LengthProviderInventoryFingerprintLimitExceeded Natural Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData variable => NFData (LengthProviderInventoryError variable)

-- | A bounded, normalized contract whose target has kind @Type@ in the exact
-- opaque source inventory supplied directly or retained by its checked
-- context. The value carries the exact spine model in its fingerprint but
-- does not retain the inventory; the later behavioral problem boundary must
-- bind its exact context and target identities before replay.
data CheckedLengthContract variable = CheckedLengthContract
  !(Type variable)
  !Int
  !(LengthFormula LengthContractVariable)
  !(LengthFormula LengthContractVariable)
  !(Fingerprint LengthContractFingerprintSubject)

type role CheckedLengthContract nominal

instance NFData variable => NFData (CheckedLengthContract variable) where
  rnf (CheckedLengthContract target count precondition postcondition fingerprint) =
    rnf target `seq`
    rnf count `seq`
    rnf precondition `seq`
    rnf postcondition `seq`
    rnf fingerprint

checkedLengthContractTarget :: CheckedLengthContract variable -> Type variable
checkedLengthContractTarget (CheckedLengthContract target _ _ _ _) = target

checkedLengthContractInputCount :: CheckedLengthContract variable -> Int
checkedLengthContractInputCount (CheckedLengthContract _ count _ _ _) = count

checkedLengthContractPrecondition
  :: CheckedLengthContract variable -> LengthFormula LengthContractVariable
checkedLengthContractPrecondition (CheckedLengthContract _ _ pre _ _) = pre

checkedLengthContractPostcondition
  :: CheckedLengthContract variable -> LengthFormula LengthContractVariable
checkedLengthContractPostcondition (CheckedLengthContract _ _ _ post _) = post

lengthContractFingerprint
  :: CheckedLengthContract variable
  -> Fingerprint LengthContractFingerprintSubject
lengthContractFingerprint (CheckedLengthContract _ _ _ _ fingerprint) = fingerprint

-- | One closed, context-free, proper-kinded assumed provider law.
data CheckedLengthProviderSummary variable = CheckedLengthProviderSummary
  !Name
  !(Type variable)
  ![LengthProviderArgumentRole]
  !(LengthExpression LengthProviderVariable)
  !FingerprintField

type role CheckedLengthProviderSummary nominal

instance NFData variable => NFData (CheckedLengthProviderSummary variable) where
  rnf (CheckedLengthProviderSummary name scheme roles transfer schemeField) =
    rnf name `seq`
    rnf scheme `seq`
    rnf roles `seq`
    rnf transfer `seq`
    rnfFingerprintField schemeField

checkedLengthProviderName :: CheckedLengthProviderSummary variable -> Name
checkedLengthProviderName (CheckedLengthProviderSummary name _ _ _ _) = name

checkedLengthProviderScheme
  :: CheckedLengthProviderSummary variable -> Type variable
checkedLengthProviderScheme (CheckedLengthProviderSummary _ scheme _ _ _) = scheme

checkedLengthProviderArgumentRoles
  :: CheckedLengthProviderSummary variable -> [LengthProviderArgumentRole]
checkedLengthProviderArgumentRoles
    (CheckedLengthProviderSummary _ _ roles _ _) = roles

checkedLengthProviderTransfer
  :: CheckedLengthProviderSummary variable
  -> LengthExpression LengthProviderVariable
checkedLengthProviderTransfer
    (CheckedLengthProviderSummary _ _ _ transfer _) = transfer

checkedLengthProviderTrust
  :: CheckedLengthProviderSummary variable -> LengthProviderTrust
checkedLengthProviderTrust _ = AssumedProviderLaw

-- | A deterministic finite index of assumed provider laws checked against one
-- source inventory.  Its fingerprint identifies the assumptions, not the
-- declarations or implementations in the source inventory.
data CheckedLengthProviderInventory variable = CheckedLengthProviderInventory
  !(Map Name (CheckedLengthProviderSummary variable))
  !(Fingerprint LengthProviderInventoryFingerprintSubject)

type role CheckedLengthProviderInventory nominal

instance NFData variable => NFData (CheckedLengthProviderInventory variable) where
  rnf (CheckedLengthProviderInventory summaries fingerprint) =
    rnf summaries `seq` rnf fingerprint

checkedLengthProviderSummaries
  :: CheckedLengthProviderInventory variable
  -> [CheckedLengthProviderSummary variable]
checkedLengthProviderSummaries (CheckedLengthProviderInventory summaries _) =
  Map.elems summaries

lookupCheckedLengthProviderSummary
  :: Name
  -> CheckedLengthProviderInventory variable
  -> Maybe (CheckedLengthProviderSummary variable)
lookupCheckedLengthProviderSummary name
    (CheckedLengthProviderInventory summaries _) = Map.lookup name summaries

lengthProviderInventoryFingerprint
  :: CheckedLengthProviderInventory variable
  -> Fingerprint LengthProviderInventoryFingerprintSubject
lengthProviderInventoryFingerprint
    (CheckedLengthProviderInventory _ fingerprint) = fingerprint

-- | Compatibility entry point for Haskell's structural @[]@ spine.
sealLengthContract
  :: Ord variable
  => LengthLimits
  -> Inventory variable annotation
  -> Type variable
  -> LengthContractSource
  -> Either (LengthContractError variable) (CheckedLengthContract variable)
sealLengthContract limits inventory = sealLengthContractInContext limits
  $ CheckedLengthContext inventory builtinListSpineModel

-- | Seal a contract against the exact inventory and spine model retained by
-- one checked context.  The productive structural bound is checked before
-- ordinary type normalization and kind inference.
sealLengthContractInContext
  :: Ord variable
  => LengthLimits
  -> CheckedLengthContext variable annotation
  -> Type variable
  -> LengthContractSource
  -> Either (LengthContractError variable) (CheckedLengthContract variable)
sealLengthContractInContext limits (CheckedLengthContext inventory model)
    rawTarget source = do
  _ <- first LengthContractTargetBoundError
    $ observeTypeWithin limits 0 rawTarget
  target <- first LengthContractTargetTypeError $ normalizeType rawTarget
  first LengthContractTargetKindError
    $ checkTypesKinds (inventoryKindAssumptions inventory)
        [(ProperTypeKind, target)]
  let (_, constraints, body) = splitLeadingForalls target
  case constraints of
    [] -> pure ()
    _ -> Left LengthContractConstrainedTarget
  let (inputs, result) = functionSpine body
      maximumInputs = lengthContractInputLimit limits
      observedInputs = observedListLength maximumInputs inputs
  if observedInputs > maximumInputs
    then Left $ LengthContractInputLimitExceeded
      maximumInputs observedInputs
    else pure ()
  mapM_ requireInputList $ zip [0 ..] inputs
  if isModeledSpine model result
    then pure ()
    else Left $ LengthContractResultIsNotList result
  let inputCount = length inputs
      contractReference phase variable = case variable of
        LengthResult
          | phase == ContractPrecondition ->
              Left LengthResultNotAvailableInPrecondition
          | otherwise -> Right ()
        LengthInput position
          | position < fromIntegral inputCount -> Right ()
          | otherwise -> Left
              $ LengthInputReferenceOutOfRange position inputCount
  (precondition, afterPrecondition) <- first
    LengthContractPreconditionError
    $ normalizeLengthFormula limits
        (contractReference ContractPrecondition)
        emptySyntaxUsage
        $ lengthContractPrecondition source
  (postcondition, _) <- first LengthContractPostconditionError
    $ normalizeLengthFormula limits
        (contractReference ContractPostcondition)
        afterPrecondition
        $ lengthContractPostcondition source
  fingerprint <- first contractFingerprintError
    $ buildLengthContractFingerprint
        limits model inputCount precondition postcondition
  pure $ CheckedLengthContract
    target inputCount precondition postcondition fingerprint
 where
  requireInputList (index, input)
    | isModeledSpine model input = Right ()
    | otherwise = Left $ LengthContractInputIsNotList index input

  contractFingerprintError FingerprintLimitExceeded
      { fingerprintMaximumBytes = maximumBytes
      , fingerprintObservedBytesAtLeast = observedBytes
      } = LengthContractFingerprintLimitExceeded maximumBytes observedBytes

-- | Compatibility entry point for Haskell's structural @[]@ spine.
sealLengthProviderInventory
  :: Ord variable
  => LengthLimits
  -> Inventory variable annotation
  -> [LengthProviderSummarySource variable]
  -> Either
      (LengthProviderInventoryError variable)
      (CheckedLengthProviderInventory variable)
sealLengthProviderInventory limits inventory =
  sealLengthProviderInventoryInContext limits
    $ CheckedLengthContext inventory builtinListSpineModel

-- | Seal assumed provider summaries against one retained source inventory and
-- spine model. Source order fixes diagnostics; successful identity and
-- projections use exact structural provider-name order.
sealLengthProviderInventoryInContext
  :: Ord variable
  => LengthLimits
  -> CheckedLengthContext variable annotation
  -> [LengthProviderSummarySource variable]
  -> Either
      (LengthProviderInventoryError variable)
      (CheckedLengthProviderInventory variable)
sealLengthProviderInventoryInContext limits
    (CheckedLengthContext inventory model) sources = do
  let maximumSummaries = lengthProviderSummaryLimit limits
      observedSummaries = observedListLength maximumSummaries sources
  if observedSummaries > maximumSummaries
    then Left $ LengthProviderSummaryLimitExceeded
      maximumSummaries observedSummaries
    else pure ()
  (summaries, _, _) <- foldM sealOne
    (Map.empty, 0, emptySyntaxUsage)
    $ zip [0 ..] sources
  fingerprint <- first inventoryFingerprintError
    $ buildLengthProviderInventoryFingerprint limits model summaries
  pure $ CheckedLengthProviderInventory summaries fingerprint
 where
  sealOne (summaries, typeNodes, syntaxUsage) (index, source) = do
    let name = lengthProviderName source
    if Map.member name summaries
      then Left $ DuplicateLengthProvider name
      else do
        (checked, afterTypeNodes, afterSyntaxUsage) <- first
          (LengthProviderSummaryRejected index name)
          $ sealLengthProviderSummary
              limits inventory model typeNodes syntaxUsage source
        Right
          ( Map.insert name checked summaries
          , afterTypeNodes
          , afterSyntaxUsage
          )

  inventoryFingerprintError FingerprintLimitExceeded
      { fingerprintMaximumBytes = maximumBytes
      , fingerprintObservedBytesAtLeast = observedBytes
      } = LengthProviderInventoryFingerprintLimitExceeded
        maximumBytes observedBytes

data ContractPhase = ContractPrecondition | ContractPostcondition
  deriving (Eq)

sealLengthProviderSummary
  :: Ord variable
  => LengthLimits
  -> Inventory variable annotation
  -> CheckedLengthSpineModel variable
  -> Int
  -> SyntaxUsage
  -> LengthProviderSummarySource variable
  -> Either
      (LengthProviderSummaryError variable)
      (CheckedLengthProviderSummary variable, Int, SyntaxUsage)
sealLengthProviderSummary limits inventory model
    typeNodes syntaxUsage source = do
  sourceScheme <- case inventoryTermScheme
      inventory $ lengthProviderName source of
    Nothing -> Left $ LengthProviderNotInSourceInventory
      $ lengthProviderName source
    Just scheme -> Right scheme
  let roles = lengthProviderArgumentRoles source
      maximumArguments = lengthProviderArgumentLimit limits
      observedRoles = observedListLength maximumArguments roles
  if observedRoles > maximumArguments
    then Left $ LengthProviderArgumentLimitExceeded
      maximumArguments observedRoles
    else pure ()
  _ <- first LengthProviderSchemeBoundError
    $ observeTypeWithin limits 0 $ lengthProviderScheme source
  suppliedScheme <- first LengthProviderSchemeTypeError
    $ normalizeType $ lengthProviderScheme source
  afterTypeNodes <- first LengthProviderSourceSchemeBoundError
    $ observeTypeWithin limits typeNodes sourceScheme
  scheme <- first LengthProviderSourceSchemeTypeError
    $ normalizeType sourceScheme
  if alphaNormalizeTypeWith PositionalBinderSlots scheme
      == alphaNormalizeTypeWith PositionalBinderSlots suppliedScheme
    then pure ()
    else Left $ LengthProviderSourceSchemeMismatch scheme suppliedScheme
  first LengthProviderSchemeKindError
    $ checkTypesKinds (inventoryKindAssumptions inventory)
        [(ProperTypeKind, scheme)]
  let (_, constraints, body) = splitLeadingForalls scheme
  case constraints of
    [] -> pure ()
    _ -> Left LengthProviderConstrainedScheme
  case Set.toAscList $ freeVariables scheme of
    [] -> pure ()
    variables -> Left $ LengthProviderOpenScheme variables
  let (arguments, result) = functionSpine body
      argumentCount = length arguments
      roleCount = length roles
  if roleCount == argumentCount
    then pure ()
    else Left $ LengthProviderRoleArityMismatch argumentCount roleCount
  if isModeledSpine model result
    then pure ()
    else Left $ LengthProviderResultIsNotList result
  mapM_ requireSpineList $ zip3 [0 ..] roles arguments
  let indexedRoles = zip [0 ..] roles
      providerReference (LengthProviderArgument position) =
        case lookup position indexedRoles of
          Nothing -> Left
            $ LengthProviderReferenceOutOfRange position argumentCount
          Just LengthUnobservedArgument -> Left
            $ LengthProviderReferenceIsUnobserved position
          Just LengthSpineArgument -> Right ()
  (transfer, afterSyntaxUsage) <- first LengthProviderTransferError
    $ normalizeLengthExpression limits providerReference syntaxUsage
    $ lengthProviderTransfer source
  let schemeField = closedProviderSchemeField
        $ alphaNormalizeTypeWith PositionalBinderSlots scheme
      checked = CheckedLengthProviderSummary
        (lengthProviderName source) scheme roles transfer schemeField
  pure (checked, afterTypeNodes, afterSyntaxUsage)
 where
  requireSpineList (index, role, argument) = case role of
    LengthUnobservedArgument -> Right ()
    LengthSpineArgument
      | isModeledSpine model argument -> Right ()
      | otherwise -> Left
          $ LengthProviderSpineArgumentIsNotList index argument

-- | Resolve only the requested term scheme. The environment's already sealed
-- indexes reject a missing name without scanning declarations; a present name
-- then walks source declarations only until its unique owner is reached.
-- Crucially, this does not close or normalize unrelated schemes merely to
-- validate one provider assumption.
inventoryTermScheme
  :: Ord variable
  => Inventory variable annotation
  -> Name
  -> Maybe (Type variable)
inventoryTermScheme inventory name
  | not present = Nothing
  | otherwise = findScheme declarations
 where
  environment = inventoryEnvironment inventory
  declarations = environmentDeclarations environment
  present = Map.member name (valueSignatureMap environment)
    || Map.member name (dataConstructorMap environment)

  findScheme [] = Nothing
  findScheme (declaration : remaining) =
    case List.find ((== name) . valueName)
        $ declarationTermSchemes declaration of
      Just signature -> Just $ valueType signature
      Nothing -> findScheme remaining

isModeledSpine
  :: CheckedLengthSpineModel variable
  -> Type variable
  -> Bool
isModeledSpine model source = case source of
  TypeApplication (TypeConstructor name) _ ->
    name == checkedLengthSpineTypeName model
  _ -> False

observeTypeWithin
  :: LengthLimits
  -> Int
  -> Type variable
  -> Either LengthTypeBoundError Int
observeTypeWithin limits = inspect
 where
  maximumNodes = lengthTypeNodeLimit limits
  maximumWidth = lengthCollectionWidthLimit limits

  inspect !count source
    | count >= maximumNodes = Left $ LengthTypeNodeLimitExceeded
        maximumNodes $ saturatedSuccessor maximumNodes
    | otherwise = case source of
        TypeVariable{} -> Right next
        TypeConstructor{} -> Right next
        TypeApplication function argument ->
          inspect next function >>= (`inspect` argument)
        FunctionType parameter result ->
          inspect next parameter >>= (`inspect` result)
        TupleType _ fields -> do
          observeCollection LengthTupleFields fields
          foldM inspect next fields
        ForallType binders constraints body -> do
          observeCollection LengthForallBinders binders
          observeCollection LengthForallConstraints constraints
          afterConstraints <- foldM inspectConstraint next constraints
          inspect afterConstraints body
   where
    next = count + 1

  inspectConstraint !count constraint = do
    let arguments = constraintArguments constraint
    observeCollection LengthConstraintArguments arguments
    foldM inspect count arguments

  observeCollection site values =
    let observed = observedListLength maximumWidth values
    in if observed <= maximumWidth
        then Right ()
        else Left $ LengthTypeCollectionLimitExceeded
          site maximumWidth observed

saturatedSuccessor :: Int -> Int
saturatedSuccessor value
  | value == maxBound = maxBound
  | otherwise = value + 1

data SyntaxUsage = SyntaxUsage !Int !Int

emptySyntaxUsage :: SyntaxUsage
emptySyntaxUsage = SyntaxUsage 0 0

consumeSyntaxNode
  :: LengthLimits
  -> SyntaxUsage
  -> Either LengthSyntaxError SyntaxUsage
consumeSyntaxNode limits (SyntaxUsage nodes clauses)
  | nodes >= maximumNodes = Left $ LengthSyntaxNodeLimitExceeded
      maximumNodes $ saturatedSuccessor maximumNodes
  | otherwise = Right $ SyntaxUsage (nodes + 1) clauses
 where
  maximumNodes = lengthSyntaxNodeLimit limits

consumeFormulaClause
  :: LengthLimits
  -> SyntaxUsage
  -> Either LengthSyntaxError SyntaxUsage
consumeFormulaClause limits (SyntaxUsage nodes clauses)
  | clauses >= maximumClauses = Left $ LengthFormulaClauseLimitExceeded
      maximumClauses $ saturatedSuccessor maximumClauses
  | otherwise = Right $ SyntaxUsage nodes (clauses + 1)
 where
  maximumClauses = lengthFormulaClauseLimit limits

observeSyntaxCollection
  :: LengthLimits
  -> LengthSyntaxCollectionSite
  -> [value]
  -> Either LengthSyntaxError ()
observeSyntaxCollection limits site values =
  let maximumWidth = lengthCollectionWidthLimit limits
      observed = observedListLength maximumWidth values
  in if observed <= maximumWidth
      then Right ()
      else Left $ LengthSyntaxCollectionLimitExceeded
        site maximumWidth observed

validateLiteral
  :: LengthLimits
  -> Natural
  -> Either LengthSyntaxError Natural
validateLiteral limits value =
  let maximumBits = lengthLiteralBitLimit limits
      observedBits = observedNaturalBits maximumBits value
  in if observedBits <= maximumBits
      then Right value
      else Left $ LengthLiteralBitLimitExceeded maximumBits observedBits

observedNaturalBits :: Int -> Natural -> Int
observedNaturalBits maximumBits = go 0
 where
  bound = max 0 maximumBits

  go !observed 0 = observed
  go !observed remaining
    | observed >= bound = saturatedSuccessor bound
    | otherwise = go (observed + 1) $ remaining `quot` 2

normalizeLengthExpression
  :: Ord variable
  => LengthLimits
  -> (variable -> Either LengthSyntaxError ())
  -> SyntaxUsage
  -> LengthExpression variable
  -> Either LengthSyntaxError (LengthExpression variable, SyntaxUsage)
normalizeLengthExpression limits checkVariable usage source = do
  afterNode <- consumeSyntaxNode limits usage
  case source of
    LengthVariable variable -> do
      checkVariable variable
      pure (LengthVariable variable, afterNode)
    LengthLiteral value -> do
      checked <- validateLiteral limits value
      pure (LengthLiteral checked, afterNode)
    LengthSum terms -> do
      observeSyntaxCollection limits LengthSumTerms terms
      (normalizedTerms, afterTerms) <- normalizeExpressions afterNode terms
      normalized <- normalizeSum limits normalizedTerms
      pure (normalized, afterTerms)
    LengthScale factor expression -> do
      checkedFactor <- validateLiteral limits factor
      (normalizedExpression, afterExpression) <- normalizeLengthExpression
        limits checkVariable afterNode expression
      normalized <- normalizeScale limits checkedFactor normalizedExpression
      pure (normalized, afterExpression)
    LengthMonus left right -> do
      (normalizedLeft, afterLeft) <- normalizeLengthExpression
        limits checkVariable afterNode left
      (normalizedRight, afterRight) <- normalizeLengthExpression
        limits checkVariable afterLeft right
      normalized <- normalizeMonus limits normalizedLeft normalizedRight
      pure (normalized, afterRight)
    LengthMinimum left right -> normalizeExtremum
      flattenMinimum LengthMinimum min afterNode left right
    LengthMaximum left right -> normalizeExtremum
      flattenMaximum LengthMaximum max afterNode left right
    LengthIf condition trueBranch falseBranch -> do
      (normalizedCondition, afterCondition) <- normalizeLengthFormula
        limits checkVariable afterNode condition
      (normalizedTrue, afterTrue) <- normalizeLengthExpression
        limits checkVariable afterCondition trueBranch
      (normalizedFalse, afterFalse) <- normalizeLengthExpression
        limits checkVariable afterTrue falseBranch
      pure
        ( normalizeIf normalizedCondition normalizedTrue normalizedFalse
        , afterFalse
        )
 where
  normalizeExpressions !current [] = Right ([], current)
  normalizeExpressions !current (expression : remaining) = do
    (normalized, afterExpression) <- normalizeLengthExpression
      limits checkVariable current expression
    (normalizedRemaining, afterRemaining) <- normalizeExpressions
      afterExpression remaining
    pure (normalized : normalizedRemaining, afterRemaining)

  normalizeExtremum flatten constructor literalOperation
      !current left right = do
    (normalizedLeft, afterLeft) <- normalizeLengthExpression
      limits checkVariable current left
    (normalizedRight, afterRight) <- normalizeLengthExpression
      limits checkVariable afterLeft right
    let flattened = flatten normalizedLeft ++ flatten normalizedRight
        literalValues =
          [value | LengthLiteral value <- flattened]
        nonLiterals = filter (not . isLiteral) flattened
        combinedLiteral = case literalValues of
          [] -> []
          firstLiteral : remainingLiterals ->
            [ LengthLiteral $ List.foldl' literalOperation
                firstLiteral remainingLiterals
            ]
        terms = Set.toAscList $ Set.fromList
          $ combinedLiteral ++ nonLiterals
        normalized = case terms of
          [] -> normalizedLeft
          firstTerm : remainingTerms ->
            List.foldl' constructor firstTerm remainingTerms
    pure (normalized, afterRight)
   where
    isLiteral LengthLiteral{} = True
    isLiteral _ = False

  flattenMinimum expression = case expression of
    LengthMinimum left right ->
      flattenMinimum left ++ flattenMinimum right
    _ -> [expression]

  flattenMaximum expression = case expression of
    LengthMaximum left right ->
      flattenMaximum left ++ flattenMaximum right
    _ -> [expression]

normalizeLengthFormula
  :: Ord variable
  => LengthLimits
  -> (variable -> Either LengthSyntaxError ())
  -> SyntaxUsage
  -> LengthFormula variable
  -> Either LengthSyntaxError (LengthFormula variable, SyntaxUsage)
normalizeLengthFormula limits checkVariable usage source = do
  afterNode <- consumeSyntaxNode limits usage
  case source of
    LengthTruth value -> do
      afterClause <- consumeFormulaClause limits afterNode
      pure (LengthTruth value, afterClause)
    LengthEqual left right -> do
      afterClause <- consumeFormulaClause limits afterNode
      (normalizedLeft, afterLeft) <- normalizeLengthExpression
        limits checkVariable afterClause left
      (normalizedRight, afterRight) <- normalizeLengthExpression
        limits checkVariable afterLeft right
      pure (normalizeEqual normalizedLeft normalizedRight, afterRight)
    LengthAtMost left right -> do
      afterClause <- consumeFormulaClause limits afterNode
      (normalizedLeft, afterLeft) <- normalizeLengthExpression
        limits checkVariable afterClause left
      (normalizedRight, afterRight) <- normalizeLengthExpression
        limits checkVariable afterLeft right
      pure (normalizeAtMost normalizedLeft normalizedRight, afterRight)
    LengthNot formula -> do
      (normalized, afterFormula) <- normalizeLengthFormula
        limits checkVariable afterNode formula
      pure (normalizeNot normalized, afterFormula)
    LengthAll formulas -> do
      observeSyntaxCollection limits LengthConjunctionClauses formulas
      (normalizedFormulas, afterFormulas) <- normalizeFormulas afterNode formulas
      normalized <- normalizeAll limits normalizedFormulas
      pure (normalized, afterFormulas)
 where
  normalizeFormulas !current [] = Right ([], current)
  normalizeFormulas !current (formula : remaining) = do
    (normalized, afterFormula) <- normalizeLengthFormula
      limits checkVariable current formula
    (normalizedRemaining, afterRemaining) <- normalizeFormulas
      afterFormula remaining
    pure (normalized : normalizedRemaining, afterRemaining)

normalizeSum
  :: Ord variable
  => LengthLimits
  -> [LengthExpression variable]
  -> Either LengthSyntaxError (LengthExpression variable)
normalizeSum limits source = do
  let flattened = concatMap flatten source
  observeSyntaxCollection limits LengthSumTerms flattened
  (literal, nonLiterals) <- foldM collect (0, []) flattened
  checkedLiteral <- validateLiteral limits literal
  let retainedLiteral
        | checkedLiteral == 0 = []
        | otherwise = [LengthLiteral checkedLiteral]
      terms = List.sort $ retainedLiteral ++ nonLiterals
  pure $ case terms of
    [] -> LengthLiteral 0
    [term] -> term
    _ -> LengthSum terms
 where
  flatten (LengthSum nested) = nested
  flatten expression = [expression]

  collect (!literal, expressions) expression = case expression of
    LengthLiteral value -> do
      combined <- validateLiteral limits $ literal + value
      pure (combined, expressions)
    _ -> pure (literal, expression : expressions)

normalizeScale
  :: Ord variable
  => LengthLimits
  -> Natural
  -> LengthExpression variable
  -> Either LengthSyntaxError (LengthExpression variable)
normalizeScale limits factor expression
  | factor == 0 = Right $ LengthLiteral 0
  | factor == 1 = Right expression
  | otherwise = case expression of
      LengthLiteral value -> LengthLiteral
        <$> validateLiteral limits (factor * value)
      LengthScale nestedFactor nested -> do
        combined <- validateLiteral limits $ factor * nestedFactor
        normalizeScale limits combined nested
      _ -> Right $ LengthScale factor expression

normalizeMonus
  :: Eq variable
  => LengthLimits
  -> LengthExpression variable
  -> LengthExpression variable
  -> Either LengthSyntaxError (LengthExpression variable)
normalizeMonus limits left right = case (left, right) of
  (LengthLiteral leftValue, LengthLiteral rightValue) ->
    LengthLiteral <$> validateLiteral limits
      (if leftValue >= rightValue then leftValue - rightValue else 0)
  (_, LengthLiteral 0) -> Right left
  _
    | left == right -> Right $ LengthLiteral 0
    | otherwise -> Right $ LengthMonus left right

normalizeIf
  :: Eq variable
  => LengthFormula variable
  -> LengthExpression variable
  -> LengthExpression variable
  -> LengthExpression variable
normalizeIf condition trueBranch falseBranch = case condition of
  LengthTruth True -> trueBranch
  LengthTruth False -> falseBranch
  _
    | trueBranch == falseBranch -> trueBranch
    | otherwise -> LengthIf condition trueBranch falseBranch

normalizeEqual
  :: Ord variable
  => LengthExpression variable
  -> LengthExpression variable
  -> LengthFormula variable
normalizeEqual left right
  | left == right = LengthTruth True
  | otherwise = case (left, right) of
      (LengthLiteral leftValue, LengthLiteral rightValue) ->
        LengthTruth $ leftValue == rightValue
      _
        | left < right -> LengthEqual left right
        | otherwise -> LengthEqual right left

normalizeAtMost
  :: Eq variable
  => LengthExpression variable
  -> LengthExpression variable
  -> LengthFormula variable
normalizeAtMost left right
  | left == right = LengthTruth True
  | otherwise = case (left, right) of
      (LengthLiteral leftValue, LengthLiteral rightValue) ->
        LengthTruth $ leftValue <= rightValue
      _ -> LengthAtMost left right

normalizeNot :: LengthFormula variable -> LengthFormula variable
normalizeNot source = case source of
  LengthTruth value -> LengthTruth $ not value
  LengthNot nested -> nested
  _ -> LengthNot source

normalizeAll
  :: Ord variable
  => LengthLimits
  -> [LengthFormula variable]
  -> Either LengthSyntaxError (LengthFormula variable)
normalizeAll limits source = do
  let flattened = concatMap flatten source
  observeSyntaxCollection limits LengthConjunctionClauses flattened
  if LengthTruth False `elem` flattened
    then Right $ LengthTruth False
    else pure $ case Set.toAscList
        $ Set.fromList
        $ filter (/= LengthTruth True) flattened of
      [] -> LengthTruth True
      [formula] -> formula
      formulas -> LengthAll formulas
 where
  flatten (LengthAll nested) = nested
  flatten formula = [formula]

buildLengthContractFingerprint
  :: LengthLimits
  -> CheckedLengthSpineModel variable
  -> Int
  -> LengthFormula LengthContractVariable
  -> LengthFormula LengthContractVariable
  -> Either
      FingerprintLimitError
      (Fingerprint LengthContractFingerprintSubject)
buildLengthContractFingerprint limits model inputCount precondition postcondition =
  buildFingerprintWithin (fromIntegral $ lengthFingerprintByteLimit limits)
    FingerprintBuilder
      { fingerprintBuilderVersion = 2
      , fingerprintBuilderRole = ascii
          "finite-list-spine-length/contract"
      , fingerprintBuilderFields =
          [ tagged "dialect"
              [FingerprintBytes finiteListSpineLengthDomainTag]
          , tagged "semantic-policy"
              [ FingerprintBytes $ ascii "finite-spine-total/v1"
              , FingerprintBytes $ ascii "unbounded-natural/v1"
              , FingerprintBytes $ ascii "exact-truncated-monus/v1"
              , FingerprintBytes $ ascii "exact-conditional/v1"
              ]
          , tagged "normalizer"
              [FingerprintBytes $ ascii "length-normalizer/v1"]
          , tagged "spine-model" [checkedLengthSpineModelField model]
          , tagged "ordered-spine-inputs"
              [ FingerprintNatural $ fromIntegral inputCount
              , FingerprintSequence $ replicate inputCount
                  $ FingerprintBytes $ ascii "list-spine"
              ]
          , tagged "precondition"
              [lengthFormulaField contractVariableField precondition]
          , tagged "postcondition"
              [lengthFormulaField contractVariableField postcondition]
          ]
      }

buildLengthProviderInventoryFingerprint
  :: LengthLimits
  -> CheckedLengthSpineModel variable
  -> Map Name (CheckedLengthProviderSummary variable)
  -> Either
      FingerprintLimitError
      (Fingerprint LengthProviderInventoryFingerprintSubject)
buildLengthProviderInventoryFingerprint limits model summaries =
  buildFingerprintWithin (fromIntegral $ lengthFingerprintByteLimit limits)
    FingerprintBuilder
      { fingerprintBuilderVersion = 2
      , fingerprintBuilderRole = ascii
          "finite-list-spine-length/provider-inventory"
      , fingerprintBuilderFields =
          [ tagged "dialect"
              [FingerprintBytes finiteListSpineLengthDomainTag]
          , tagged "model"
              [ FingerprintBytes $ ascii "finite-spine-total/v1"
              , FingerprintBytes $ ascii "unbounded-natural/v1"
              , FingerprintBytes $ ascii "length-normalizer/v1"
              ]
          , tagged "spine-model" [checkedLengthSpineModelField model]
          , tagged "summaries"
              [FingerprintSequence $ map providerSummaryField
                $ Map.elems summaries]
          ]
      }

checkedLengthSpineModelField
  :: CheckedLengthSpineModel variable
  -> FingerprintField
checkedLengthSpineModelField
    (CheckedLengthSpineModel _ _ _ _ _ field) = field

providerSummaryField
  :: CheckedLengthProviderSummary variable
  -> FingerprintField
providerSummaryField
    (CheckedLengthProviderSummary name _ roles transfer schemeField) =
  tagged "assumed-provider-summary"
    [ FingerprintName name
    , tagged "closed-alpha-normal-scheme" [schemeField]
    , tagged "ordered-term-argument-roles"
        [FingerprintSequence $ map providerRoleField roles]
    , tagged "length-transfer"
        [lengthExpressionField providerVariableField transfer]
    , tagged "trust"
        [ FingerprintBytes $ ascii "assumed-provider-law/v1"
        , FingerprintBytes $ ascii
            "uniform-over-admitted-closed-instantiations/v1"
        , FingerprintBytes $ ascii
            "unobserved-means-no-length-read-only/v1"
        ]
    ]

providerRoleField :: LengthProviderArgumentRole -> FingerprintField
providerRoleField role = FingerprintBytes $ ascii $ case role of
  LengthSpineArgument -> "spine-length-argument"
  LengthUnobservedArgument -> "length-unobserved-argument"

contractVariableField :: LengthContractVariable -> FingerprintField
contractVariableField variable = case variable of
  LengthInput position -> tagged "input" [FingerprintNatural position]
  LengthResult -> tagged "result" []

providerVariableField :: LengthProviderVariable -> FingerprintField
providerVariableField (LengthProviderArgument position) =
  tagged "argument" [FingerprintNatural position]

lengthExpressionField
  :: (variable -> FingerprintField)
  -> LengthExpression variable
  -> FingerprintField
lengthExpressionField variableField source = case source of
  LengthVariable variable -> tagged "variable" [variableField variable]
  LengthLiteral value -> tagged "literal" [FingerprintNatural value]
  LengthSum terms -> tagged "sum"
    [FingerprintSequence $ map (lengthExpressionField variableField) terms]
  LengthScale factor expression -> tagged "scale"
    [ FingerprintNatural factor
    , lengthExpressionField variableField expression
    ]
  LengthMonus left right -> tagged "monus"
    [ lengthExpressionField variableField left
    , lengthExpressionField variableField right
    ]
  LengthMinimum left right -> tagged "minimum"
    [ lengthExpressionField variableField left
    , lengthExpressionField variableField right
    ]
  LengthMaximum left right -> tagged "maximum"
    [ lengthExpressionField variableField left
    , lengthExpressionField variableField right
    ]
  LengthIf condition trueBranch falseBranch -> tagged "if"
    [ lengthFormulaField variableField condition
    , lengthExpressionField variableField trueBranch
    , lengthExpressionField variableField falseBranch
    ]

lengthFormulaField
  :: (variable -> FingerprintField)
  -> LengthFormula variable
  -> FingerprintField
lengthFormulaField variableField source = case source of
  LengthTruth value -> tagged "truth"
    [FingerprintNatural $ if value then 1 else 0]
  LengthEqual left right -> tagged "equal"
    [ lengthExpressionField variableField left
    , lengthExpressionField variableField right
    ]
  LengthAtMost left right -> tagged "at-most"
    [ lengthExpressionField variableField left
    , lengthExpressionField variableField right
    ]
  LengthNot formula -> tagged "not"
    [lengthFormulaField variableField formula]
  LengthAll formulas -> tagged "all"
    [FingerprintSequence $ map (lengthFormulaField variableField) formulas]

closedProviderSchemeField
  :: Type (AlphaVariable variable)
  -> FingerprintField
closedProviderSchemeField source = case source of
  TypeVariable variable -> tagged "type-variable"
    [alphaVariableField variable]
  TypeConstructor name -> tagged "type-constructor" [FingerprintName name]
  TypeApplication function argument -> tagged "type-application"
    [ closedProviderSchemeField function
    , closedProviderSchemeField argument
    ]
  FunctionType parameter result -> tagged "function"
    [ closedProviderSchemeField parameter
    , closedProviderSchemeField result
    ]
  TupleType boxity fields -> tagged "tuple"
    [ boxityField boxity
    , FingerprintSequence $ map closedProviderSchemeField fields
    ]
  ForallType binders constraints body -> tagged "forall"
    [ FingerprintSequence $ map alphaVariableField binders
    , FingerprintSequence $ map closedConstraintField constraints
    , closedProviderSchemeField body
    ]

closedConstraintField
  :: Constraint (Type (AlphaVariable variable))
  -> FingerprintField
closedConstraintField (Constraint className arguments) = tagged "constraint"
  [ FingerprintName className
  , FingerprintSequence $ map closedProviderSchemeField arguments
  ]

alphaVariableField :: AlphaVariable variable -> FingerprintField
alphaVariableField variable = case variable of
  AlphaBoundVariable scope slot -> tagged "bound"
    [FingerprintNatural scope, FingerprintNatural slot]
  -- Accepted provider schemes are closed before alpha-normalization.  Keeping
  -- this impossible branch distinct avoids a partial private encoder without
  -- granting any useful identity to an open scheme.
  AlphaFreeVariable{} -> tagged "rejected-free-variable" []

boxityField :: Boxity -> FingerprintField
boxityField boxity = FingerprintBytes $ ascii $ case boxity of
  Boxed -> "boxed"
  Unboxed -> "unboxed"

tagged :: String -> [FingerprintField] -> FingerprintField
tagged tag = FingerprintTag $ ascii tag

rnfFingerprintField :: FingerprintField -> ()
rnfFingerprintField field = case field of
  FingerprintNatural value -> rnf value
  FingerprintBytes bytes -> rnf bytes
  FingerprintSequence fields -> foldr
    (\nested remaining -> rnfFingerprintField nested `seq` remaining)
    ()
    fields
  FingerprintTag tag fields -> rnf tag `seq` foldr
    (\nested remaining -> rnfFingerprintField nested `seq` remaining)
    ()
    fields
  FingerprintName name -> rnf name

ascii :: String -> [Word8]
ascii = map $ fromIntegral . ord
