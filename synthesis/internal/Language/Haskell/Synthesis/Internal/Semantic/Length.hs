{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Private representation and construction edge for list-length contracts.
module Language.Haskell.Synthesis.Internal.Semantic.Length
  ( FiniteListSpineLengthV1
  , FiniteBinaryProductSpineLengthsV1
  , LengthContractFingerprintSubject
  , LengthSpinePairContractFingerprintSubject
  , LengthProviderInventoryFingerprintSubject
  , finiteListSpineLengthDomainTag
  , finiteBinaryProductSpineLengthsDomainTag
  , LengthExpression (..)
  , LengthFormula (..)
  , LengthContractVariable (..)
  , LengthContractSource (..)
  , LengthSpinePair (..)
  , LengthSpinePairComponent (..)
  , LengthSpinePairContractVariable (..)
  , LengthSpinePairContractSource (..)
  , LengthTargetArgumentRole (..)
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
  , LengthSpinePairContractError (..)
  , LengthProviderSummaryError (..)
  , LengthProviderInventoryError (..)
  , CheckedLengthContract
  , sealLengthContract
  , sealLengthContractInContext
  , sealRoleAwareLengthContract
  , sealRoleAwareLengthContractInContext
  , checkedLengthContractTarget
  , checkedLengthContractTargetArgumentRoles
  , checkedLengthContractInputCount
  , checkedLengthContractPrecondition
  , checkedLengthContractPostcondition
  , lengthContractFingerprint
  , CheckedLengthSpinePairContract
  , sealLengthSpinePairContract
  , sealLengthSpinePairContractInContext
  , sealRoleAwareLengthSpinePairContract
  , sealRoleAwareLengthSpinePairContractInContext
  , checkedLengthSpinePairContractTarget
  , checkedLengthSpinePairContractTargetArgumentRoles
  , checkedLengthSpinePairContractInputCount
  , checkedLengthSpinePairContractPrecondition
  , checkedLengthSpinePairContractPostcondition
  , lengthSpinePairContractFingerprint
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
  , checkedLengthSpineModelField
  , providerSummaryField
  , contractVariableField
  , lengthSpinePairContractVariableField
  , lengthExpressionField
  , lengthFormulaField
  , tagged
  , ascii
  , SyntaxUsage
  , emptySyntaxUsage
  , normalizeLengthExpression
  , normalizeLengthFormula
  , inventoryTermScheme
  , isModeledSpine
  ) where

import Control.DeepSeq (NFData (rnf))
import Control.Monad (foldM, unless, when)
import Data.Bifunctor (first)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Collection (observedListLength)
import Language.Haskell.Synthesis.Constraint (constraintArguments)
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
import qualified Language.Haskell.Synthesis.Internal.Fingerprint.Type
  as FingerprintType
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
  ( Boxity (Boxed)
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
import Language.Haskell.Synthesis.Count
  ( saturatedSuccessor
  , observedNaturalBits
  )

-- | Nominal marker for the exact semantic dialect.
data FiniteListSpineLengthV1

-- | Nominal marker for exact binary products whose two components are total
-- finite spine lengths.  Inventory and interpretation authority are derived
-- from a checked scalar Length session, but behavioral evidence never crosses
-- the scalar domain boundary.
data FiniteBinaryProductSpineLengthsV1

-- | Contract identity is deliberately not an encoding identity.
data LengthContractFingerprintSubject

-- | Binary spine-product contract identity is deliberately distinct from the
-- historical scalar contract identity.
data LengthSpinePairContractFingerprintSubject

-- | Identity of the assumed length summaries only.
--
-- This is deliberately not the generic behavioral inventory subject.  A
-- future domain builder must combine this key with the exact opaque source
-- inventory before constructing that complete identity.
data LengthProviderInventoryFingerprintSubject

-- | Exact finite runtime tag compared before every generic problem identity.
finiteListSpineLengthDomainTag :: [Word8]
finiteListSpineLengthDomainTag = ascii "finite-list-spine-length/v1"

-- | Exact runtime domain tag compared before every product problem identity.
finiteBinaryProductSpineLengthsDomainTag :: [Word8]
finiteBinaryProductSpineLengthsDomainTag =
  ascii "finite-binary-product-spine-lengths/v1"

-- | Symbolic natural-valued length term over the variables of one contract,
-- provider law, or candidate.  Arithmetic is exact over unbounded naturals:
-- 'LengthMonus' is truncated subtraction, 'LengthQuotient' and 'LengthModulo'
-- take a positive literal divisor, and 'LengthIf' selects by a formula.
data LengthExpression variable
  = LengthVariable variable
  | LengthLiteral Natural
  | LengthSum [LengthExpression variable]
  | LengthScale Natural (LengthExpression variable)
  -- Both raw divisors are deliberately lazy.  Sealing validates that each is
  -- positive and within the shared literal bound before traversing its child.
  | LengthQuotient Natural (LengthExpression variable)
  | LengthModulo Natural (LengthExpression variable)
  | LengthMonus (LengthExpression variable) (LengthExpression variable)
  | LengthMinimum (LengthExpression variable) (LengthExpression variable)
  | LengthMaximum (LengthExpression variable) (LengthExpression variable)
  | LengthIf
      (LengthFormula variable)
      (LengthExpression variable)
      (LengthExpression variable)
  deriving (Eq, Ord, Show, Generic)

instance NFData variable => NFData (LengthExpression variable)

-- | Boolean assertion over length expressions.  Only equality, at-most,
-- negation, and finite conjunction are primitive; strict comparison is
-- written as @'LengthNot' ('LengthAtMost' ...)@.
data LengthFormula variable
  = LengthTruth Bool
  | LengthEqual (LengthExpression variable) (LengthExpression variable)
  | LengthAtMost (LengthExpression variable) (LengthExpression variable)
  | LengthNot (LengthFormula variable)
  | LengthAll [LengthFormula variable]
  deriving (Eq, Ord, Show, Generic)

instance NFData variable => NFData (LengthFormula variable)

-- | Variables admitted by a scalar contract.  'LengthInput' indexes observed
-- spine arguments compactly from zero in source order; 'LengthResult' names
-- the result spine and is admitted only in the postcondition.
data LengthContractVariable
  = LengthInput Natural
  | LengthResult
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthContractVariable

-- | Unchecked scalar contract as supplied by the caller.  Both formulas are
-- normalized and bounded by 'sealLengthContract' and its variants before any
-- authority is granted; the precondition may not mention 'LengthResult'.
data LengthContractSource = LengthContractSource
  { lengthContractPrecondition :: LengthFormula LengthContractVariable
  , lengthContractPostcondition :: LengthFormula LengthContractVariable
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthContractSource

-- | A source-ordered binary product.  This carrier is shared by symbolic
-- candidate results and independently replayed concrete results; it grants no
-- checked authority by itself.
data LengthSpinePair value = LengthSpinePair
  { lengthSpinePairFirst :: !value
  , lengthSpinePairSecond :: !value
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData value => NFData (LengthSpinePair value)

-- | Stable source-order identity for one component of a binary spine result.
data LengthSpinePairComponent
  = LengthSpinePairFirst
  | LengthSpinePairSecond
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSpinePairComponent

-- | Variables admitted by a binary spine-product contract.  Inputs retain
-- the scalar domain's compact observed-argument numbering; each result
-- component is independently addressable only in the postcondition.
data LengthSpinePairContractVariable
  = LengthSpinePairInput !Natural
  | LengthSpinePairResult !LengthSpinePairComponent
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSpinePairContractVariable

-- | Unchecked binary product-of-spines contract as supplied by the caller.
-- Sealing normalizes and bounds both formulas; the precondition may not
-- mention either 'LengthSpinePairResult' component.
data LengthSpinePairContractSource = LengthSpinePairContractSource
  { lengthSpinePairContractPrecondition
      :: LengthFormula LengthSpinePairContractVariable
  , lengthSpinePairContractPostcondition
      :: LengthFormula LengthSpinePairContractVariable
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSpinePairContractSource

-- | How one source-ordered target argument participates in Length semantics.
--
-- Observed arguments must have the configured finite-spine type and receive
-- compact 'LengthInput' indices in observed-role order.  Unobserved arguments
-- remain opaque to Length semantics: the candidate interpreter may carry or
-- forward them only along non-demanding paths.
data LengthTargetArgumentRole
  = LengthObservedSpine
  | LengthUnobservedTarget
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthTargetArgumentRole

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

-- | Variable admitted by a provider transfer: the zero-based source position
-- of one provider argument.  Sealing rejects positions beyond the scheme's
-- arity and positions whose role is 'LengthUnobservedArgument'.
newtype LengthProviderVariable = LengthProviderArgument Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthProviderVariable

-- | An explicitly assumed provider law over one exact closed inventory
-- scheme.  The legacy constructor remains context-free.  The conditional
-- constructor retains a nonempty leading context and grants behavioral-use
-- authority only when the Length problem boundary associates independent
-- ground discharge with one complete certified occurrence.
data LengthProviderSummarySource variable
  = AssumedProviderSummary
      { lengthProviderName :: Name
      , lengthProviderScheme :: Type variable
      , lengthProviderArgumentRoles :: [LengthProviderArgumentRole]
      , lengthProviderTransfer :: LengthExpression LengthProviderVariable
      }
    -- ^ Historical context-free provider assumption.  A nonempty leading
    -- context is rejected with 'LengthProviderConstrainedScheme'.
  | AssumedConstraintConditionalProviderSummary
      { lengthProviderName :: Name
      , lengthProviderScheme :: Type variable
      , lengthProviderArgumentRoles :: [LengthProviderArgumentRole]
      , lengthProviderTransfer :: LengthExpression LengthProviderVariable
      }
    -- ^ Retain an exact closed scheme with a nonempty leading context.  This
    -- asserts a law uniform over independently admitted dictionary evidence
    -- and conditional on candidate-local discharge; it does not itself provide
    -- dictionary or behavioral authority.
  deriving (Eq, Ord, Show, Generic)

instance NFData variable => NFData (LengthProviderSummarySource variable)

-- | Retained classifier of how far an assumed provider law may be used.  It
-- is fixed by the 'LengthProviderSummarySource' constructor at sealing time
-- and participates in the provider-inventory fingerprint.
data LengthProviderTrust
  = AssumedProviderLaw
    -- ^ Context-free assumed law, directly usable by the Length interpreter.
  | AssumedProviderLawConditionalOnConstraintDischarge
    -- ^ Retained constrained law assumed uniform over admitted dictionary
    -- evidence and unusable until an independent candidate-local discharge
    -- authority is associated.
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthProviderTrust

-- | Caller-supplied structural bounds for the Length semantic core, one field
-- per 'LengthLimitField'.  Values are validated by 'mkLengthLimits', which
-- rejects any negative field.
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

-- | Validated, nonnegative Length bounds.  Construct with 'mkLengthLimits'
-- or use 'defaultLengthLimits'; read individual bounds with the
-- @length...Limit@ projections.
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

-- | Which bound of a 'LengthLimitSource' a limit diagnostic refers to, in
-- the source record's field order.
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

-- | Rejection by 'mkLengthLimits': the named field carried the given
-- negative value.
data LengthLimitError = NegativeLengthLimit LengthLimitField Int
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthLimitError

-- | Validate every field of a 'LengthLimitSource', checking them in field
-- order and reporting the first negative one.  All nonnegative values are
-- accepted unchanged.
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

-- | Source form of the default bounds; 'mkLengthLimits' accepts it and
-- yields 'defaultLengthLimits'.
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

-- | Default validated bounds, field for field the same values as
-- 'defaultLengthLimitSource'.
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

-- | Which list inside a type exceeded the collection-width bound while a
-- target or scheme was being measured.
data LengthTypeCollectionSite
  = LengthTupleFields
  | LengthForallBinders
  | LengthForallConstraints
  | LengthConstraintArguments
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthTypeCollectionSite

-- | Structural rejection of a type before it is normalized or kind-checked.
-- Both constructors carry the maximum followed by the observed count, where
-- the observed count is capped at one past the maximum.
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

-- | Bind one source inventory to its finite-spine model.  'BuiltinListSpine'
-- always succeeds without consulting the inventory; a 'DeclaredListSpine' is
-- looked up in the inventory and validated against the narrow two-constructor
-- schema, failing on the first violation in declaration-shape order.
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
  unless (parameterCount == 1)
    $ Left $ LengthSpineParameterArityMismatch typeName parameterCount
  let constructorCount = observedListLength 2 constructors
  unless (constructorCount == 2)
    $ Left $ LengthSpineConstructorArityMismatch typeName constructorCount
  when (zeroName == stepName)
    $ Left $ LengthSpineConstructorsMustDiffer zeroName
  zeroConstructor <- case List.find ((== zeroName) . constructorName) constructors of
    Nothing -> Left $ LengthSpineZeroConstructorMissing zeroName
    Just constructor -> Right constructor
  stepConstructor <- case List.find ((== stepName) . constructorName) constructors of
    Nothing -> Left $ LengthSpineStepConstructorMissing stepName
    Just constructor -> Right constructor
  let zeroFields = constructorFields zeroConstructor
      zeroFieldCount = observedListLength 0 zeroFields
  unless (zeroFieldCount == 0)
    $ Left $ LengthSpineZeroFieldArityMismatch zeroName zeroFieldCount
  let stepFields = constructorFields stepConstructor
      stepFieldCount = observedListLength 2 stepFields
  unless (stepFieldCount == 2)
    $ Left $ LengthSpineStepFieldArityMismatch stepName stepFieldCount
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

-- | Which list inside a length expression or formula exceeded the
-- collection-width bound during normalization.
data LengthSyntaxCollectionSite
  = LengthSumTerms
  | LengthConjunctionClauses
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSyntaxCollectionSite

-- | Rejection of a length expression or formula by 'normalizeLengthExpression'
-- or 'normalizeLengthFormula'.  Limit constructors carry the maximum then the
-- observed count; the variable-reference constructors are produced by the
-- caller-supplied variable check and name the offending position.
data LengthSyntaxError
  = LengthSyntaxNodeLimitExceeded Int Int
  | LengthFormulaClauseLimitExceeded Int Int
  | LengthSyntaxCollectionLimitExceeded
      LengthSyntaxCollectionSite Int Int
  | LengthLiteralBitLimitExceeded Int Int
  | LengthQuotientDivisorZero
  | LengthModuloDivisorZero
  | LengthResultNotAvailableInPrecondition
  | LengthInputReferenceOutOfRange Natural Int
  | LengthProviderReferenceOutOfRange Natural Int
  | LengthProviderReferenceIsUnobserved Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSyntaxError

-- | Fixed-precedence rejection while sealing a scalar contract.  Constructors
-- are listed in the order the checks run: target bound, normalization, kind,
-- constraint-freedom, input and role bounds, spine shape of inputs then
-- result, precondition, postcondition, and finally the fingerprint budget.
data LengthContractError variable
  = LengthContractTargetBoundError LengthTypeBoundError
  | LengthContractTargetTypeError (TypeError variable)
  | LengthContractTargetKindError (KindInferenceError variable)
  | LengthContractConstrainedTarget
  | LengthContractInputLimitExceeded Int Int
  | LengthContractTargetArgumentRoleLimitExceeded Int Int
  | LengthContractTargetArgumentRoleArityMismatch Int Int
  | LengthContractInputIsNotList Int (Type variable)
  | LengthContractResultIsNotList (Type variable)
  | LengthContractPreconditionError LengthSyntaxError
  | LengthContractPostconditionError LengthSyntaxError
  | LengthContractFingerprintLimitExceeded Natural Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData variable => NFData (LengthContractError variable)

-- | Fixed-precedence rejection while sealing a binary product-of-spines
-- contract.  This is an additive sibling of 'LengthContractError'; keeping a
-- closed product error type avoids widening the historical scalar API.
data LengthSpinePairContractError variable
  = LengthSpinePairContractTargetBoundError LengthTypeBoundError
  | LengthSpinePairContractTargetTypeError (TypeError variable)
  | LengthSpinePairContractTargetKindError (KindInferenceError variable)
  | LengthSpinePairContractConstrainedTarget
  | LengthSpinePairContractInputLimitExceeded !Int !Int
  | LengthSpinePairContractTargetArgumentRoleLimitExceeded !Int !Int
  | LengthSpinePairContractTargetArgumentRoleArityMismatch !Int !Int
  | LengthSpinePairContractInputIsNotList !Int (Type variable)
  | LengthSpinePairContractResultIsNotBoxedTuple (Type variable)
  | LengthSpinePairContractResultTupleArityMismatch !Int
  | LengthSpinePairContractResultComponentIsNotList
      !LengthSpinePairComponent (Type variable)
  | LengthSpinePairContractPreconditionError LengthSyntaxError
  | LengthSpinePairContractPostconditionError LengthSyntaxError
  | LengthSpinePairContractFingerprintLimitExceeded !Natural !Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData variable => NFData (LengthSpinePairContractError variable)

-- | Rejection of one assumed provider summary.  The named provider must have
-- a term scheme in the source inventory that alpha-matches the supplied
-- closed scheme; the scheme's constraint context must agree with the source
-- constructor's trust; and every argument role must line up with the
-- scheme's spine before the transfer expression is normalized.
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
    -- ^ The historical source constructor was given a constrained scheme.
  | LengthProviderConditionalSchemeHasNoConstraints
    -- ^ The conditional source constructor was given no leading constraints.
  | LengthProviderOpenScheme [variable]
  | LengthProviderArgumentLimitExceeded Int Int
  | LengthProviderRoleArityMismatch Int Int
  | LengthProviderResultIsNotList (Type variable)
  | LengthProviderSpineArgumentIsNotList Int (Type variable)
  | LengthProviderTransferError LengthSyntaxError
  deriving (Eq, Ord, Show, Generic)

instance NFData variable => NFData (LengthProviderSummaryError variable)

-- | Rejection of a whole provider inventory.  A rejected summary is reported
-- with its zero-based source index and provider name; a duplicate name is
-- detected before the later duplicate is sealed.
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
  ![LengthTargetArgumentRole]
  !Int
  !(LengthFormula LengthContractVariable)
  !(LengthFormula LengthContractVariable)
  !(Fingerprint LengthContractFingerprintSubject)

type role CheckedLengthContract nominal

instance NFData variable => NFData (CheckedLengthContract variable) where
  rnf (CheckedLengthContract target roles count precondition postcondition fingerprint) =
    rnf target `seq`
    rnf roles `seq`
    rnf count `seq`
    rnf precondition `seq`
    rnf postcondition `seq`
    rnf fingerprint

-- | The normalized, constraint-free target type the contract was sealed for.
checkedLengthContractTarget :: CheckedLengthContract variable -> Type variable
checkedLengthContractTarget (CheckedLengthContract target _ _ _ _ _) = target

-- | Exact source-ordered argument-role authority retained by the contract.
checkedLengthContractTargetArgumentRoles
  :: CheckedLengthContract variable -> [LengthTargetArgumentRole]
checkedLengthContractTargetArgumentRoles
    (CheckedLengthContract _ roles _ _ _ _) = roles

-- | Number of observed spine inputs, i.e. the number of admitted
-- 'LengthInput' positions.  Unobserved target arguments are not counted.
checkedLengthContractInputCount :: CheckedLengthContract variable -> Int
checkedLengthContractInputCount
    (CheckedLengthContract _ _ count _ _ _) = count

-- | The normalized precondition; it never mentions 'LengthResult'.
checkedLengthContractPrecondition
  :: CheckedLengthContract variable -> LengthFormula LengthContractVariable
checkedLengthContractPrecondition (CheckedLengthContract _ _ _ pre _ _) = pre

-- | The normalized postcondition, which may mention 'LengthResult'.
checkedLengthContractPostcondition
  :: CheckedLengthContract variable -> LengthFormula LengthContractVariable
checkedLengthContractPostcondition (CheckedLengthContract _ _ _ _ post _) = post

-- | Contract identity built at sealing time from the dialect, semantic policy,
-- spine model, argument roles and observed input count, and both normalized
-- formulas.  It does not cover the target type or the inventory.
lengthContractFingerprint
  :: CheckedLengthContract variable
  -> Fingerprint LengthContractFingerprintSubject
lengthContractFingerprint
    (CheckedLengthContract _ _ _ _ _ fingerprint) = fingerprint

-- | A bounded normalized contract for one exact boxed binary product of the
-- session's modeled finite spine.  The inventory remains owned by the checked
-- context/session and is rebound at problem construction.
data CheckedLengthSpinePairContract variable = CheckedLengthSpinePairContract
  !(Type variable)
  ![LengthTargetArgumentRole]
  !Int
  !(LengthFormula LengthSpinePairContractVariable)
  !(LengthFormula LengthSpinePairContractVariable)
  !(Fingerprint LengthSpinePairContractFingerprintSubject)

type role CheckedLengthSpinePairContract nominal

instance NFData variable => NFData (CheckedLengthSpinePairContract variable) where
  rnf (CheckedLengthSpinePairContract target roles count precondition
      postcondition fingerprint) =
    rnf target `seq`
    rnf roles `seq`
    rnf count `seq`
    rnf precondition `seq`
    rnf postcondition `seq`
    rnf fingerprint

-- | The normalized, constraint-free target type; its result is a boxed pair
-- of modeled spines.
checkedLengthSpinePairContractTarget
  :: CheckedLengthSpinePairContract variable -> Type variable
checkedLengthSpinePairContractTarget
    (CheckedLengthSpinePairContract target _ _ _ _ _) = target

-- | Exact source-ordered argument-role authority retained by the contract,
-- one role per physical target argument.
checkedLengthSpinePairContractTargetArgumentRoles
  :: CheckedLengthSpinePairContract variable -> [LengthTargetArgumentRole]
checkedLengthSpinePairContractTargetArgumentRoles
    (CheckedLengthSpinePairContract _ roles _ _ _ _) = roles

-- | Number of observed spine inputs, i.e. the number of admitted
-- 'LengthSpinePairInput' positions.
checkedLengthSpinePairContractInputCount
  :: CheckedLengthSpinePairContract variable -> Int
checkedLengthSpinePairContractInputCount
    (CheckedLengthSpinePairContract _ _ count _ _ _) = count

-- | The normalized precondition; it never mentions a result component.
checkedLengthSpinePairContractPrecondition
  :: CheckedLengthSpinePairContract variable
  -> LengthFormula LengthSpinePairContractVariable
checkedLengthSpinePairContractPrecondition
    (CheckedLengthSpinePairContract _ _ _ precondition _ _) = precondition

-- | The normalized postcondition, which may address either result component.
checkedLengthSpinePairContractPostcondition
  :: CheckedLengthSpinePairContract variable
  -> LengthFormula LengthSpinePairContractVariable
checkedLengthSpinePairContractPostcondition
    (CheckedLengthSpinePairContract _ _ _ _ postcondition _) = postcondition

-- | Product-contract identity built at sealing time from the product dialect,
-- semantic policy, spine model, argument roles, result shape, and both
-- normalized formulas.  It does not cover the target type or the inventory.
lengthSpinePairContractFingerprint
  :: CheckedLengthSpinePairContract variable
  -> Fingerprint LengthSpinePairContractFingerprintSubject
lengthSpinePairContractFingerprint
    (CheckedLengthSpinePairContract _ _ _ _ _ fingerprint) = fingerprint

-- | One closed, proper-kinded assumed provider law.  Its retained trust
-- classifier distinguishes the historical context-free law from a constrained
-- law which remains unusable without candidate-local discharge authority.
data CheckedLengthProviderSummary variable = CheckedLengthProviderSummary
  !Name
  !(Type variable)
  ![LengthProviderArgumentRole]
  !(LengthExpression LengthProviderVariable)
  !LengthProviderTrust
  !FingerprintField

type role CheckedLengthProviderSummary nominal

instance NFData variable => NFData (CheckedLengthProviderSummary variable) where
  rnf (CheckedLengthProviderSummary name scheme roles transfer trust
      schemeField) =
    rnf name `seq`
    rnf scheme `seq`
    rnf roles `seq`
    rnf transfer `seq`
    rnf trust `seq`
    rnfFingerprintField schemeField

-- | The provider's term name, which is unique within its checked inventory.
checkedLengthProviderName :: CheckedLengthProviderSummary variable -> Name
checkedLengthProviderName (CheckedLengthProviderSummary name _ _ _ _ _) = name

-- | The provider's closed, normalized term scheme as resolved from the source
-- inventory (not the caller-supplied copy, which was only alpha-matched).
checkedLengthProviderScheme
  :: CheckedLengthProviderSummary variable -> Type variable
checkedLengthProviderScheme
    (CheckedLengthProviderSummary _ scheme _ _ _ _) = scheme

-- | Source-ordered argument roles, exactly one per argument of the scheme.
checkedLengthProviderArgumentRoles
  :: CheckedLengthProviderSummary variable -> [LengthProviderArgumentRole]
checkedLengthProviderArgumentRoles
    (CheckedLengthProviderSummary _ _ roles _ _ _) = roles

-- | The normalized transfer giving the result spine length; it references
-- only positions whose role is 'LengthSpineArgument'.
checkedLengthProviderTransfer
  :: CheckedLengthProviderSummary variable
  -> LengthExpression LengthProviderVariable
checkedLengthProviderTransfer
    (CheckedLengthProviderSummary _ _ _ transfer _ _) = transfer

-- | Trust classifier fixed by the source constructor at sealing time; the
-- interpreter consults it before applying the law.
checkedLengthProviderTrust
  :: CheckedLengthProviderSummary variable -> LengthProviderTrust
checkedLengthProviderTrust
    (CheckedLengthProviderSummary _ _ _ _ trust _) = trust

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

-- | All checked summaries in ascending provider-name order, not source order.
checkedLengthProviderSummaries
  :: CheckedLengthProviderInventory variable
  -> [CheckedLengthProviderSummary variable]
checkedLengthProviderSummaries (CheckedLengthProviderInventory summaries _) =
  Map.elems summaries

-- | Find the checked summary for one provider name, if it was sealed.
lookupCheckedLengthProviderSummary
  :: Name
  -> CheckedLengthProviderInventory variable
  -> Maybe (CheckedLengthProviderSummary variable)
lookupCheckedLengthProviderSummary name
    (CheckedLengthProviderInventory summaries _) = Map.lookup name summaries

-- | Identity of the assumed laws only: dialect, spine model, and every
-- checked summary in provider-name order.  Its version rises when any
-- summary carries conditional trust.
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

-- | Role-aware compatibility entry point for Haskell's structural @[]@
-- spine.  An all-observed role vector canonicalizes to the exact legacy
-- contract identity; only a vector containing an unobserved target selects
-- the role-aware identity policy.
sealRoleAwareLengthContract
  :: Ord variable
  => LengthLimits
  -> Inventory variable annotation
  -> [LengthTargetArgumentRole]
  -> Type variable
  -> LengthContractSource
  -> Either (LengthContractError variable) (CheckedLengthContract variable)
sealRoleAwareLengthContract limits inventory roles =
  sealRoleAwareLengthContractInContext limits
    (CheckedLengthContext inventory builtinListSpineModel) roles

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
    rawTarget source = sealLengthContractInContextWithRoles limits inventory
      model Nothing rawTarget source

-- | Seal a role-aware contract against one exact checked context.
--
-- The role vector is bounded and matched to the complete physical target
-- argument spine.  Contract variables number only observed spine arguments,
-- compactly and in source order.
sealRoleAwareLengthContractInContext
  :: Ord variable
  => LengthLimits
  -> CheckedLengthContext variable annotation
  -> [LengthTargetArgumentRole]
  -> Type variable
  -> LengthContractSource
  -> Either (LengthContractError variable) (CheckedLengthContract variable)
sealRoleAwareLengthContractInContext limits
    (CheckedLengthContext inventory model) roles rawTarget source =
  sealLengthContractInContextWithRoles limits inventory model
    (Just roles) rawTarget source

sealLengthContractInContextWithRoles
  :: Ord variable
  => LengthLimits
  -> Inventory variable annotation
  -> CheckedLengthSpineModel variable
  -> Maybe [LengthTargetArgumentRole]
  -> Type variable
  -> LengthContractSource
  -> Either (LengthContractError variable) (CheckedLengthContract variable)
sealLengthContractInContextWithRoles limits inventory model suppliedRoles
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
  when (observedInputs > maximumInputs)
    $ Left $ LengthContractInputLimitExceeded
      maximumInputs observedInputs
  roles <- case suppliedRoles of
    Nothing -> Right $ replicate (length inputs) LengthObservedSpine
    Just rawRoles -> do
      let observedRoles = observedListLength maximumInputs rawRoles
      when (observedRoles > maximumInputs)
        $ Left $ LengthContractTargetArgumentRoleLimitExceeded
          maximumInputs observedRoles
      if observedRoles == length inputs
        then Right rawRoles
        else Left $ LengthContractTargetArgumentRoleArityMismatch
          (length inputs) observedRoles
  mapM_ requireObservedInput $ zip3 [0 ..] roles inputs
  unless (isModeledSpine model result)
    $ Left $ LengthContractResultIsNotList result
  let inputCount = length $ filter (== LengthObservedSpine) roles
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
        limits model roles inputCount precondition postcondition
  pure $ CheckedLengthContract
    target roles inputCount precondition postcondition fingerprint
 where
  requireObservedInput (index, role, input) = case role of
    LengthUnobservedTarget -> Right ()
    LengthObservedSpine
      | isModeledSpine model input -> Right ()
      | otherwise -> Left $ LengthContractInputIsNotList index input

  contractFingerprintError FingerprintLimitExceeded
      { fingerprintMaximumBytes = maximumBytes
      , fingerprintObservedBytesAtLeast = observedBytes
      } = LengthContractFingerprintLimitExceeded maximumBytes observedBytes

-- | Compatibility entry point for a boxed binary product of Haskell's
-- structural @[]@ spine.
sealLengthSpinePairContract
  :: Ord variable
  => LengthLimits
  -> Inventory variable annotation
  -> Type variable
  -> LengthSpinePairContractSource
  -> Either
      (LengthSpinePairContractError variable)
      (CheckedLengthSpinePairContract variable)
sealLengthSpinePairContract limits inventory =
  sealLengthSpinePairContractInContext limits
    $ CheckedLengthContext inventory builtinListSpineModel

-- | Role-aware compatibility entry point for a boxed binary product of
-- Haskell's structural @[]@ spine.
sealRoleAwareLengthSpinePairContract
  :: Ord variable
  => LengthLimits
  -> Inventory variable annotation
  -> [LengthTargetArgumentRole]
  -> Type variable
  -> LengthSpinePairContractSource
  -> Either
      (LengthSpinePairContractError variable)
      (CheckedLengthSpinePairContract variable)
sealRoleAwareLengthSpinePairContract limits inventory roles =
  sealRoleAwareLengthSpinePairContractInContext limits
    (CheckedLengthContext inventory builtinListSpineModel) roles

-- | Seal a binary product-of-spines contract against one exact checked
-- context.  Target validation retains the scalar contract's precedence up to
-- the result, then checks boxed tuple shape, exact arity, and components in
-- source order.
sealLengthSpinePairContractInContext
  :: Ord variable
  => LengthLimits
  -> CheckedLengthContext variable annotation
  -> Type variable
  -> LengthSpinePairContractSource
  -> Either
      (LengthSpinePairContractError variable)
      (CheckedLengthSpinePairContract variable)
sealLengthSpinePairContractInContext limits
    (CheckedLengthContext inventory model) rawTarget source =
  sealLengthSpinePairContractInContextWithRoles limits inventory model
    Nothing rawTarget source

-- | Seal a role-aware binary product-of-spines contract against one exact
-- checked context.  The role vector is bounded and must match the physical
-- target argument count; only observed spine arguments receive compact
-- 'LengthSpinePairInput' indices, and any unobserved role selects the
-- role-aware fingerprint identity.
sealRoleAwareLengthSpinePairContractInContext
  :: Ord variable
  => LengthLimits
  -> CheckedLengthContext variable annotation
  -> [LengthTargetArgumentRole]
  -> Type variable
  -> LengthSpinePairContractSource
  -> Either
      (LengthSpinePairContractError variable)
      (CheckedLengthSpinePairContract variable)
sealRoleAwareLengthSpinePairContractInContext limits
    (CheckedLengthContext inventory model) roles rawTarget source =
  sealLengthSpinePairContractInContextWithRoles limits inventory model
    (Just roles) rawTarget source

sealLengthSpinePairContractInContextWithRoles
  :: Ord variable
  => LengthLimits
  -> Inventory variable annotation
  -> CheckedLengthSpineModel variable
  -> Maybe [LengthTargetArgumentRole]
  -> Type variable
  -> LengthSpinePairContractSource
  -> Either
      (LengthSpinePairContractError variable)
      (CheckedLengthSpinePairContract variable)
sealLengthSpinePairContractInContextWithRoles limits inventory model
    suppliedRoles rawTarget source = do
  _ <- first LengthSpinePairContractTargetBoundError
    $ observeTypeWithin limits 0 rawTarget
  target <- first LengthSpinePairContractTargetTypeError
    $ normalizeType rawTarget
  first LengthSpinePairContractTargetKindError
    $ checkTypesKinds (inventoryKindAssumptions inventory)
        [(ProperTypeKind, target)]
  let (_, constraints, body) = splitLeadingForalls target
  case constraints of
    [] -> pure ()
    _ -> Left LengthSpinePairContractConstrainedTarget
  let (inputs, result) = functionSpine body
      maximumInputs = lengthContractInputLimit limits
      observedInputs = observedListLength maximumInputs inputs
  when (observedInputs > maximumInputs)
    $ Left $ LengthSpinePairContractInputLimitExceeded
      maximumInputs observedInputs
  roles <- case suppliedRoles of
    Nothing -> Right $ replicate (length inputs) LengthObservedSpine
    Just rawRoles -> do
      let observedRoles = observedListLength maximumInputs rawRoles
      when (observedRoles > maximumInputs)
        $ Left $ LengthSpinePairContractTargetArgumentRoleLimitExceeded
          maximumInputs observedRoles
      if observedRoles == length inputs
        then Right rawRoles
        else Left $ LengthSpinePairContractTargetArgumentRoleArityMismatch
          (length inputs) observedRoles
  mapM_ requireObservedInput $ zip3 [0 ..] roles inputs
  components <- case result of
    TupleType Boxed fields -> case fields of
      [firstComponent, secondComponent] ->
        Right $ LengthSpinePair firstComponent secondComponent
      _ -> Left $ LengthSpinePairContractResultTupleArityMismatch
        $ length fields
    _ -> Left $ LengthSpinePairContractResultIsNotBoxedTuple result
  requireResultComponent LengthSpinePairFirst
    $ lengthSpinePairFirst components
  requireResultComponent LengthSpinePairSecond
    $ lengthSpinePairSecond components
  let inputCount = length $ filter (== LengthObservedSpine) roles
      contractReference phase variable = case variable of
        LengthSpinePairResult _
          | phase == ContractPrecondition ->
              Left LengthResultNotAvailableInPrecondition
          | otherwise -> Right ()
        LengthSpinePairInput position
          | position < fromIntegral inputCount -> Right ()
          | otherwise -> Left
              $ LengthInputReferenceOutOfRange position inputCount
  (precondition, afterPrecondition) <- first
    LengthSpinePairContractPreconditionError
    $ normalizeLengthFormula limits
        (contractReference ContractPrecondition)
        emptySyntaxUsage
        $ lengthSpinePairContractPrecondition source
  (postcondition, _) <- first LengthSpinePairContractPostconditionError
    $ normalizeLengthFormula limits
        (contractReference ContractPostcondition)
        afterPrecondition
        $ lengthSpinePairContractPostcondition source
  fingerprint <- first contractFingerprintError
    $ buildLengthSpinePairContractFingerprint
        limits model roles inputCount precondition postcondition
  pure $ CheckedLengthSpinePairContract
    target roles inputCount precondition postcondition fingerprint
 where
  requireObservedInput (index, role, input) = case role of
    LengthUnobservedTarget -> Right ()
    LengthObservedSpine
      | isModeledSpine model input -> Right ()
      | otherwise -> Left
          $ LengthSpinePairContractInputIsNotList index input

  requireResultComponent component result
    | isModeledSpine model result = Right ()
    | otherwise = Left
        $ LengthSpinePairContractResultComponentIsNotList component result

  contractFingerprintError FingerprintLimitExceeded
      { fingerprintMaximumBytes = maximumBytes
      , fingerprintObservedBytesAtLeast = observedBytes
      } = LengthSpinePairContractFingerprintLimitExceeded
        maximumBytes observedBytes

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
  when (observedSummaries > maximumSummaries)
    $ Left $ LengthProviderSummaryLimitExceeded
      maximumSummaries observedSummaries
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
  when (observedRoles > maximumArguments)
    $ Left $ LengthProviderArgumentLimitExceeded
      maximumArguments observedRoles
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
      trust = providerSummarySourceTrust source
  case (trust, constraints) of
    (AssumedProviderLaw, []) -> pure ()
    (AssumedProviderLaw, _ : _) -> Left LengthProviderConstrainedScheme
    (AssumedProviderLawConditionalOnConstraintDischarge, []) ->
      Left LengthProviderConditionalSchemeHasNoConstraints
    (AssumedProviderLawConditionalOnConstraintDischarge, _ : _) -> pure ()
  case Set.toAscList $ freeVariables scheme of
    [] -> pure ()
    variables -> Left $ LengthProviderOpenScheme variables
  let (arguments, result) = functionSpine body
      argumentCount = length arguments
      roleCount = length roles
  unless (roleCount == argumentCount)
    $ Left $ LengthProviderRoleArityMismatch argumentCount roleCount
  unless (isModeledSpine model result)
    $ Left $ LengthProviderResultIsNotList result
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
        (lengthProviderName source) scheme roles transfer trust schemeField
  pure (checked, afterTypeNodes, afterSyntaxUsage)
 where
  requireSpineList (index, role, argument) = case role of
    LengthUnobservedArgument -> Right ()
    LengthSpineArgument
      | isModeledSpine model argument -> Right ()
      | otherwise -> Left
          $ LengthProviderSpineArgumentIsNotList index argument

providerSummarySourceTrust
  :: LengthProviderSummarySource variable -> LengthProviderTrust
providerSummarySourceTrust source = case source of
  AssumedProviderSummary{} -> AssumedProviderLaw
  AssumedConstraintConditionalProviderSummary{} ->
    AssumedProviderLawConditionalOnConstraintDischarge

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

-- | Whether a type is exactly one application of the modeled spine's type
-- constructor.  Only the head constructor name is compared; the element type
-- is not inspected.
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

-- | Running count of syntax nodes and formula clauses consumed so far by the
-- normalizers.  Threading one value through several formulas makes them
-- share a single node and clause budget.
data SyntaxUsage = SyntaxUsage !Int !Int

-- | Fresh usage with nothing consumed.
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

-- | Bound and canonicalize one length expression.  Every node consumes syntax
-- budget, every literal is checked against the literal bit bound, and every
-- variable is passed to the supplied check; sums and extrema are then
-- flattened, literals folded, and terms sorted, while trivial scales, monus,
-- quotients, moduli, and conditionals are reduced.  Children are normalized
-- before any reduction, so a reducible node cannot hide an invalid child.
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
    LengthQuotient divisor expression -> do
      when (divisor == 0) $ Left LengthQuotientDivisorZero
      checkedDivisor <- validateLiteral limits divisor
      (normalizedExpression, afterExpression) <- normalizeLengthExpression
        limits checkVariable afterNode expression
      pure
        ( normalizeQuotient checkedDivisor normalizedExpression
        , afterExpression
        )
    LengthModulo divisor expression -> do
      when (divisor == 0) $ Left LengthModuloDivisorZero
      checkedDivisor <- validateLiteral limits divisor
      (normalizedExpression, afterExpression) <- normalizeLengthExpression
        limits checkVariable afterNode expression
      pure
        ( normalizeModulo checkedDivisor normalizedExpression
        , afterExpression
        )
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

-- | Bound and canonicalize one length formula.  Each formula node consumes
-- syntax budget and each atomic clause additionally consumes clause budget;
-- operands go through 'normalizeLengthExpression'.  Equalities are oriented
-- by 'Ord', double negation and literal comparisons are folded, and
-- conjunctions are flattened, deduplicated, sorted, and collapsed to
-- @LengthTruth False@ when any clause is false.
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

-- The divisor has already been checked positive and within the literal bound.
-- Normalize the child before applying either reduction so quotient-by-one or
-- a literal expression cannot hide an invalid variable or an over-budget tree.
normalizeQuotient
  :: Natural
  -> LengthExpression variable
  -> LengthExpression variable
normalizeQuotient divisor expression = case expression of
  LengthLiteral value -> LengthLiteral $ value `quot` divisor
  _
    | divisor == 1 -> expression
    | otherwise -> LengthQuotient divisor expression

-- The divisor has already been checked positive and within the literal bound.
-- Normalize the child before applying either reduction so a modulo-by-one or
-- literal expression cannot hide an invalid variable or an over-budget tree.
normalizeModulo
  :: Natural
  -> LengthExpression variable
  -> LengthExpression variable
normalizeModulo divisor expression = case expression of
  LengthLiteral value -> LengthLiteral $ value `mod` divisor
  _
    | divisor == 1 -> LengthLiteral 0
    | otherwise -> LengthModulo divisor expression

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
  -> [LengthTargetArgumentRole]
  -> Int
  -> LengthFormula LengthContractVariable
  -> LengthFormula LengthContractVariable
  -> Either
      FingerprintLimitError
      (Fingerprint LengthContractFingerprintSubject)
buildLengthContractFingerprint limits model roles inputCount
    precondition postcondition =
  buildFingerprintWithin (fromIntegral $ lengthFingerprintByteLimit limits)
    FingerprintBuilder
      { fingerprintBuilderVersion = if mixedRoles then 3 else 2
      , fingerprintBuilderRole = ascii
          "finite-list-spine-length/contract"
      , fingerprintBuilderFields = if mixedRoles
          then roleAwareFields
          else legacyFields
      }
 where
  mixedRoles = LengthUnobservedTarget `elem` roles
  sharedPolicy =
    [ FingerprintBytes $ ascii "finite-spine-total/v1"
    , FingerprintBytes $ ascii "unbounded-natural/v1"
    , FingerprintBytes $ ascii "exact-truncated-monus/v1"
    , FingerprintBytes $ ascii "exact-conditional/v1"
    ]
  prefixFields policy =
          [ tagged "dialect"
              [FingerprintBytes finiteListSpineLengthDomainTag]
          , tagged "semantic-policy"
              policy
          , tagged "normalizer"
              [FingerprintBytes $ ascii "length-normalizer/v1"]
          , tagged "spine-model" [checkedLengthSpineModelField model]
          ]
  suffixFields =
    [ tagged "precondition"
        [lengthFormulaField contractVariableField precondition]
    , tagged "postcondition"
        [lengthFormulaField contractVariableField postcondition]
    ]
  legacyFields = prefixFields sharedPolicy ++
    [ tagged "ordered-spine-inputs"
        [ FingerprintNatural $ fromIntegral inputCount
        , FingerprintSequence $ replicate inputCount
            $ FingerprintBytes $ ascii "list-spine"
        ]
    ] ++ suffixFields
  roleAwareFields = prefixFields (sharedPolicy ++
      [ FingerprintBytes $ ascii "role-aware-target-arguments/v1"
      , FingerprintBytes $ ascii "opaque-unobserved-target/v1"
      , FingerprintBytes $ ascii "compact-observed-input-numbering/v1"
      ]) ++
    [ tagged "ordered-target-arguments"
        [ FingerprintNatural $ fromIntegral $ length roles
        , FingerprintSequence $ map targetArgumentRoleField roles
        ]
    , tagged "observed-spine-input-count"
        [FingerprintNatural $ fromIntegral inputCount]
    ] ++ suffixFields

buildLengthSpinePairContractFingerprint
  :: LengthLimits
  -> CheckedLengthSpineModel variable
  -> [LengthTargetArgumentRole]
  -> Int
  -> LengthFormula LengthSpinePairContractVariable
  -> LengthFormula LengthSpinePairContractVariable
  -> Either
      FingerprintLimitError
      (Fingerprint LengthSpinePairContractFingerprintSubject)
buildLengthSpinePairContractFingerprint limits model roles inputCount
    precondition postcondition =
  buildFingerprintWithin (fromIntegral $ lengthFingerprintByteLimit limits)
    FingerprintBuilder
      { fingerprintBuilderVersion = if mixedRoles then 2 else 1
      , fingerprintBuilderRole = ascii
          "finite-binary-product-spine-lengths/contract"
      , fingerprintBuilderFields = prefixFields ++ argumentFields ++
          [ tagged "result-shape"
              [ FingerprintBytes $ ascii "boxed-binary-product/v1"
              , FingerprintBytes $ ascii "first-finite-spine-length/v1"
              , FingerprintBytes $ ascii "second-finite-spine-length/v1"
              , FingerprintBytes $ ascii "source-ordered-components/v1"
              ]
          , tagged "precondition"
              [ lengthFormulaField lengthSpinePairContractVariableField
                  precondition
              ]
          , tagged "postcondition"
              [ lengthFormulaField lengthSpinePairContractVariableField
                  postcondition
              ]
          ]
      }
 where
  mixedRoles = LengthUnobservedTarget `elem` roles
  prefixFields =
    [ tagged "dialect"
        [FingerprintBytes finiteBinaryProductSpineLengthsDomainTag]
    , tagged "semantic-policy" $
        [ FingerprintBytes $ ascii "finite-spine-total/v1"
        , FingerprintBytes $ ascii "unbounded-natural/v1"
        , FingerprintBytes $ ascii "exact-truncated-monus/v1"
        , FingerprintBytes $ ascii "exact-conditional/v1"
        , FingerprintBytes $ ascii "boxed-binary-product-of-spines/v1"
        ] ++ if mixedRoles
          then
            [ FingerprintBytes $ ascii "role-aware-target-arguments/v1"
            , FingerprintBytes $ ascii "opaque-unobserved-target/v1"
            , FingerprintBytes $ ascii
                "compact-observed-input-numbering/v1"
            ]
          else []
    , tagged "normalizer"
        [FingerprintBytes $ ascii "length-normalizer/v1"]
    , tagged "spine-model" [checkedLengthSpineModelField model]
    ]
  argumentFields
    | mixedRoles =
        [ tagged "ordered-target-arguments"
            [ FingerprintNatural $ fromIntegral $ length roles
            , FingerprintSequence $ map targetArgumentRoleField roles
            ]
        , tagged "observed-spine-input-count"
            [FingerprintNatural $ fromIntegral inputCount]
        ]
    | otherwise =
        [ tagged "ordered-spine-inputs"
            [ FingerprintNatural $ fromIntegral inputCount
            , FingerprintSequence $ replicate inputCount
                $ FingerprintBytes $ ascii "list-spine"
            ]
        ]

targetArgumentRoleField :: LengthTargetArgumentRole -> FingerprintField
targetArgumentRoleField role = FingerprintBytes $ ascii $ case role of
  LengthObservedSpine -> "observed-spine"
  LengthUnobservedTarget -> "unobserved-target"

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
      { fingerprintBuilderVersion =
          if any isConditional $ Map.elems summaries then 3 else 2
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

-- | The spine model's precomputed identity field, fixed when the model was
-- sealed; every Length contract, provider-inventory, and problem fingerprint
-- embeds it under a @spine-model@ tag.
checkedLengthSpineModelField
  :: CheckedLengthSpineModel variable
  -> FingerprintField
checkedLengthSpineModelField
    (CheckedLengthSpineModel _ _ _ _ _ field) = field

-- | Identity field of one checked provider summary: name, closed
-- alpha-normal scheme, ordered argument roles, transfer, and trust.
providerSummaryField
  :: CheckedLengthProviderSummary variable
  -> FingerprintField
providerSummaryField
    (CheckedLengthProviderSummary name _ roles transfer trust schemeField) =
  tagged "assumed-provider-summary"
    [ FingerprintName name
    , tagged "closed-alpha-normal-scheme" [schemeField]
    , tagged "ordered-term-argument-roles"
        [FingerprintSequence $ map providerRoleField roles]
    , tagged "length-transfer"
        [lengthExpressionField providerVariableField transfer]
    , tagged "trust" $ providerTrustFields trust
    ]

providerTrustFields :: LengthProviderTrust -> [FingerprintField]
providerTrustFields trust = case trust of
  AssumedProviderLaw ->
    [ FingerprintBytes $ ascii "assumed-provider-law/v1"
    , FingerprintBytes $ ascii
        "uniform-over-admitted-closed-instantiations/v1"
    , FingerprintBytes $ ascii
        "unobserved-means-no-length-read-only/v1"
    ]
  AssumedProviderLawConditionalOnConstraintDischarge ->
    [ FingerprintBytes $ ascii "assumed-provider-law/v1"
    , FingerprintBytes $ ascii
        "uniform-over-admitted-closed-instantiations/v1"
    , FingerprintBytes $ ascii
        "conditional-on-independent-ground-constraint-discharge/v1"
    , FingerprintBytes $ ascii
        "unobserved-means-no-length-read-only/v1"
    ]

isConditional :: CheckedLengthProviderSummary variable -> Bool
isConditional summary = checkedLengthProviderTrust summary ==
  AssumedProviderLawConditionalOnConstraintDischarge

providerRoleField :: LengthProviderArgumentRole -> FingerprintField
providerRoleField role = FingerprintBytes $ ascii $ case role of
  LengthSpineArgument -> "spine-length-argument"
  LengthUnobservedArgument -> "length-unobserved-argument"

-- | Identity field of a scalar contract variable, for use as the variable
-- encoder of 'lengthExpressionField' and 'lengthFormulaField'.
contractVariableField :: LengthContractVariable -> FingerprintField
contractVariableField variable = case variable of
  LengthInput position -> tagged "input" [FingerprintNatural position]
  LengthResult -> tagged "result" []

-- | Identity field of a product-contract variable.  Inputs share the scalar
-- @input@ encoding; each result component is tagged distinctly.
lengthSpinePairContractVariableField
  :: LengthSpinePairContractVariable -> FingerprintField
lengthSpinePairContractVariableField variable = case variable of
  LengthSpinePairInput position ->
    tagged "input" [FingerprintNatural position]
  LengthSpinePairResult component -> tagged "result"
    [FingerprintBytes $ ascii $ case component of
      LengthSpinePairFirst -> "first"
      LengthSpinePairSecond -> "second"]

providerVariableField :: LengthProviderVariable -> FingerprintField
providerVariableField (LengthProviderArgument position) =
  tagged "argument" [FingerprintNatural position]

-- | Structural identity field of a length expression, tagging each
-- constructor and encoding variables with the supplied encoder.  Callers pass
-- normalized expressions so that canonically equal terms fingerprint alike.
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
  LengthQuotient divisor expression -> tagged
    "positive-literal-natural-quotient/v1"
    [ FingerprintNatural divisor
    , lengthExpressionField variableField expression
    ]
  LengthModulo divisor expression -> tagged
    "positive-literal-natural-modulo/v1"
    [ FingerprintNatural divisor
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

-- | Structural identity field of a length formula, tagging each constructor
-- and delegating operands to 'lengthExpressionField' with the same encoder.
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
closedProviderSchemeField =
  FingerprintType.typeFingerprintField alphaVariableField

alphaVariableField :: AlphaVariable variable -> FingerprintField
alphaVariableField variable = case variable of
  AlphaBoundVariable scope slot -> tagged "bound"
    [FingerprintNatural scope, FingerprintNatural slot]
  -- Accepted provider schemes are closed before alpha-normalization.  Keeping
  -- this impossible branch distinct avoids a partial private encoder without
  -- granting any useful identity to an open scheme.
  AlphaFreeVariable{} -> tagged "rejected-free-variable" []

-- | Wrap fields in a 'FingerprintTag' whose tag is the ASCII bytes of the
-- given label.
tagged :: String -> [FingerprintField] -> FingerprintField
tagged = FingerprintType.taggedFingerprintField

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

-- | Encode a label as fingerprint bytes, one byte per character by code
-- point; callers pass only ASCII literals.
ascii :: String -> [Word8]
ascii = FingerprintType.asciiFingerprintBytes
