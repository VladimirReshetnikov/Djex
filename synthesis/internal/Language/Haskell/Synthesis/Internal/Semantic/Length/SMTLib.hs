{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Private construction of canonical SMT-LIB queries for checked scalar and
-- binary-product Length problems.
--
-- The typed query plan is constructed transiently beside its rendering and
-- bound structurally into the sealed fingerprint.  Raw solver statuses do not
-- enter this module: a decoded input assignment can acquire evidence only by
-- passing the independent concrete replay boundary. Product queries share the
-- input-only QF_LIA lowering and untrusted binding representation, but retain
-- distinct schemas, fingerprints, errors, checked problems, and evidence.
module Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib
  ( LengthSMTLibQueryFingerprintSubject
  , lengthSMTLibQuerySchemaTag
  , lengthSMTLibQueryLogic
  , LengthSMTLibLimitSource (..)
  , LengthSMTLibLimits
  , LengthSMTLibLimitField (..)
  , LengthSMTLibLimitError (..)
  , mkLengthSMTLibLimits
  , defaultLengthSMTLibLimitSource
  , defaultLengthSMTLibLimits
  , lengthSMTLibCommandByteLimit
  , lengthSMTLibFingerprintByteLimit
  , lengthSMTLibNumeralBitLimit
  , LengthSMTLibCommandPart (..)
  , LengthSMTLibNumeralSite (..)
  , LengthSMTLibQueryError (..)
  , LengthSMTLibQuery
  , sealLengthSMTLibQuery
  , lengthSMTLibQueryInputSymbols
  , lengthSMTLibQueryCheckBytes
  , lengthSMTLibQueryInputValueRequestBytes
  , lengthSMTLibQueryFingerprint
  , lengthSMTLibQueryBehavioralProblem
  , lengthSMTLibQueryCounterexampleBankScope
  , LengthSMTLibIntegerBinding (..)
  , LengthSMTLibModelError (..)
  , validateLengthSMTLibCounterexample
  , LengthSMTLibInputReplayError (..)
  , replayLengthSMTLibCounterexampleInputs
  , probeLengthSMTLibCounterexampleAtOrigin
  , LengthSMTLibCounterexampleSimplificationError (..)
  , simplifyLengthSMTLibQueryCounterexample
  , LengthSMTLibInputBoxValidationError (..)
  , validateLengthSMTLibQueryInputBox
  , LengthSMTLibApplicableDomainValidationError (..)
  , validateLengthSMTLibQueryApplicableDomain
  , LengthSpinePairSMTLibQueryFingerprintSubject
  , lengthSpinePairSMTLibQuerySchemaTag
  , lengthSpinePairSMTLibQueryLogic
  , LengthSpinePairSMTLibQueryError (..)
  , LengthSpinePairSMTLibQuery
  , sealLengthSpinePairSMTLibQuery
  , lengthSpinePairSMTLibQueryInputSymbols
  , lengthSpinePairSMTLibQueryCheckBytes
  , lengthSpinePairSMTLibQueryInputValueRequestBytes
  , lengthSpinePairSMTLibQueryFingerprint
  , lengthSpinePairSMTLibQueryBehavioralProblem
  , lengthSpinePairSMTLibQueryCounterexampleBankScope
  , LengthSpinePairSMTLibModelError (..)
  , validateLengthSpinePairSMTLibCounterexample
  , LengthSpinePairSMTLibInputReplayError (..)
  , replayLengthSpinePairSMTLibCounterexampleInputs
  , probeLengthSpinePairSMTLibCounterexampleAtOrigin
  , LengthSpinePairSMTLibCounterexampleSimplificationError (..)
  , simplifyLengthSpinePairSMTLibQueryCounterexample
  , LengthSpinePairSMTLibInputBoxValidationError (..)
  , validateLengthSpinePairSMTLibQueryInputBox
  , LengthSpinePairSMTLibApplicableDomainValidationError (..)
  , validateLengthSpinePairSMTLibQueryApplicableDomain
  ) where

import Control.DeepSeq (NFData (rnf))
import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Collection (observedListLength)
import Language.Haskell.Synthesis.Fingerprint (Fingerprint)
import Language.Haskell.Synthesis.Internal.Fingerprint
  ( FingerprintBuilder (..)
  , FingerprintField (..)
  , FingerprintLimitError (..)
  , buildFingerprintWithin
  , fingerprintCanonicalBytes
  )
import Language.Haskell.Synthesis.Internal.SMTLib.QFLIA
  ( QFLIABooleanExpression (..)
  , QFLIACommand (..)
  , QFLIAIntegerExpression (..)
  , qfliaBooleanExpressionFingerprintField
  , qfliaCommandFingerprintField
  , qfliaLogicBytes
  , renderQFLIACommand
  , renderQFLIACommands
  )
import Language.Haskell.Synthesis.Semantic.Length
  ( FiniteBinaryProductSpineLengthsV1
  , FiniteListSpineLengthV1
  , LengthContractVariable (..)
  , LengthExpression (..)
  , LengthFormula (..)
  )
import Language.Haskell.Synthesis.Semantic.Length.Evaluate
  ( LengthApplicableDomainValidation (..)
  , LengthApplicableDomainValidationError
  , LengthBooleanFiniteUnionLimits
  , LengthCounterexampleSimplificationError
  , LengthEvaluationError
  , LengthEvaluationLimits
  , LengthInputBoxLimits
  , LengthInputBoxValidation (..)
  , LengthInputBoxValidationError
  , LengthProblemAssignment (..)
  , LengthSpinePairApplicableDomainValidationError
  , LengthSpinePairCounterexampleSimplificationError
  , LengthSpinePairEvaluationError
  , LengthSpinePairInputBoxValidationError
  , ValidatedLengthApplicableDomain
  , ValidatedLengthCounterexample
  , ValidatedLengthCounterexampleSimplification
  , ValidatedLengthInputBox
  , ValidatedLengthSpinePairApplicableDomain
  , ValidatedLengthSpinePairCounterexample
  , ValidatedLengthSpinePairCounterexampleSimplification
  , ValidatedLengthSpinePairInputBox
  , validateLengthProblemApplicableDomain
  , validateLengthProblemInputBox
  , validateLengthProblemCounterexample
  , validateLengthSpinePairProblemApplicableDomain
  , validateLengthSpinePairProblemInputBox
  , validateLengthSpinePairProblemCounterexample
  , simplifyLengthProblemCounterexample
  , simplifyLengthSpinePairProblemCounterexample
  )
import Language.Haskell.Synthesis.Semantic.Length.Problem
  ( CheckedLengthProblem
  , CheckedLengthSpinePairProblem
  , checkedLengthProblemBehavioralProblem
  , checkedLengthProblemCounterexampleBankScope
  , checkedLengthProblemCounterexampleCondition
  , checkedLengthProblemInputCount
  , checkedLengthSpinePairProblemBehavioralProblem
  , checkedLengthSpinePairProblemCounterexampleBankScope
  , checkedLengthSpinePairProblemCounterexampleCondition
  , checkedLengthSpinePairProblemInputCount
  )
import Language.Haskell.Synthesis.Semantic.Length.CounterexampleBank
  ( LengthCounterexampleBankScope
  , LengthSpinePairCounterexampleBankScope
  )
import Language.Haskell.Synthesis.Semantic.Problem
  ( BehavioralEvidence
  , BehavioralProblem
  , ReplayMismatch
  , behavioralProblemCandidateFingerprint
  , behavioralProblemDomain
  , behavioralProblemEncodingFingerprint
  , behavioralProblemFingerprint
  , behavioralProblemInventoryFingerprint
  , replayBehavioralEvidence
  )
import Language.Haskell.Synthesis.Count (observedNaturalBits)

-- | Identity of the fixed translator, typed plan, exact problem, and rendered
-- request bytes.  A future execution identity must additionally bind the Z3
-- build, executable, process protocol, resource limits, and runtime options.
data LengthSMTLibQueryFingerprintSubject

-- | Nominally separate identity for translating one exact binary-product
-- spine problem.  The rendered input-only QF_LIA program may coincide with a
-- scalar query, but its domain, problem envelope, schema, and complete query
-- fingerprint never do.
data LengthSpinePairSMTLibQueryFingerprintSubject

-- | Stable translator schema.  This is deliberately distinct from the
-- solver-neutral Length encoding fingerprint.
lengthSMTLibQuerySchemaTag :: [Word8]
lengthSMTLibQuerySchemaTag = ascii "djex-length-z3-qf-lia-smtlib2/v2"

-- | Stable translator schema for the finite binary-product spine domain.
-- This is distinct from the scalar schema even though both use the same
-- checked input-only lowering kernel.
lengthSpinePairSMTLibQuerySchemaTag :: [Word8]
lengthSpinePairSMTLibQuerySchemaTag =
  ascii "djex-length-spine-pair-z3-qf-lia-smtlib2/v1"

-- | The only logic emitted by this translator.  Positive-literal natural
-- quotient and modulo are lowered to existential quotient/remainder
-- constraints containing only linear integer arithmetic; this translator
-- never emits SMT-LIB @div@ or @mod@.
lengthSMTLibQueryLogic :: [Word8]
lengthSMTLibQueryLogic = qfliaLogicBytes

-- | The product translator emits the same QF_LIA logic as the scalar
-- translator.  This projection is named separately so callers never need a
-- scalar query value to inspect product-query policy.
lengthSpinePairSMTLibQueryLogic :: [Word8]
lengthSpinePairSMTLibQueryLogic = qfliaLogicBytes

-- | Raw independent bounds for one canonical query.  Natural-valued byte
-- fields admit zero; only the signed bit limit requires validation.
data LengthSMTLibLimitSource = LengthSMTLibLimitSource
  { lengthSMTLibLimitSourceCommandBytes :: Natural
  , lengthSMTLibLimitSourceFingerprintBytes :: Natural
  , lengthSMTLibLimitSourceNumeralBits :: Int
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibLimitSource

-- | Validated query construction limits.
data LengthSMTLibLimits = LengthSMTLibLimits !Natural !Natural !Int
  deriving (Eq, Ord, Show)

instance NFData LengthSMTLibLimits where
  rnf (LengthSMTLibLimits commands fingerprint numerals) =
    rnf commands `seq` rnf fingerprint `seq` rnf numerals

data LengthSMTLibLimitField = LengthSMTLibNumeralBits
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibLimitField

data LengthSMTLibLimitError = NegativeLengthSMTLibLimit
  !LengthSMTLibLimitField !Int
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibLimitError

mkLengthSMTLibLimits
  :: LengthSMTLibLimitSource
  -> Either LengthSMTLibLimitError LengthSMTLibLimits
mkLengthSMTLibLimits source
  | numeralBits < 0 = Left $ NegativeLengthSMTLibLimit
      LengthSMTLibNumeralBits numeralBits
  | otherwise = Right $ LengthSMTLibLimits
      (lengthSMTLibLimitSourceCommandBytes source)
      (lengthSMTLibLimitSourceFingerprintBytes source)
      numeralBits
 where
  numeralBits = lengthSMTLibLimitSourceNumeralBits source

defaultLengthSMTLibLimitSource :: LengthSMTLibLimitSource
defaultLengthSMTLibLimitSource = LengthSMTLibLimitSource
  { lengthSMTLibLimitSourceCommandBytes = 65536
  , lengthSMTLibLimitSourceFingerprintBytes = 262144
  , lengthSMTLibLimitSourceNumeralBits = 4096
  }

defaultLengthSMTLibLimits :: LengthSMTLibLimits
defaultLengthSMTLibLimits = LengthSMTLibLimits 65536 262144 4096

lengthSMTLibCommandByteLimit :: LengthSMTLibLimits -> Natural
lengthSMTLibCommandByteLimit (LengthSMTLibLimits value _ _) = value

lengthSMTLibFingerprintByteLimit :: LengthSMTLibLimits -> Natural
lengthSMTLibFingerprintByteLimit (LengthSMTLibLimits _ value _) = value

lengthSMTLibNumeralBitLimit :: LengthSMTLibLimits -> Int
lengthSMTLibNumeralBitLimit (LengthSMTLibLimits _ _ value) = value

-- | Independently bounded command artifact.
data LengthSMTLibCommandPart
  = LengthSMTLibCheckCommand
  | LengthSMTLibInputValueRequest
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibCommandPart

-- | Semantic numeral whose decimal rendering was rejected first.
data LengthSMTLibNumeralSite
  = LengthSMTLibLiteralNumeral
  | LengthSMTLibScaleNumeral
  | LengthSMTLibModuloDivisorNumeral
  | LengthSMTLibQuotientDivisorNumeral
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibNumeralSite

-- | Fixed-precedence failure while constructing one exact query.
data LengthSMTLibQueryError
  = LengthSMTLibUnexpectedResultVariable
  | LengthSMTLibInputVariableOutOfRange !Natural !Int
  | LengthSMTLibQuotientDivisorZero
  | LengthSMTLibModuloDivisorZero
  | LengthSMTLibNumeralBitLimitExceeded
      !LengthSMTLibNumeralSite !Int !Int
  | LengthSMTLibCommandByteLimitExceeded
      !LengthSMTLibCommandPart !Natural !Natural
  | LengthSMTLibFingerprintByteLimitExceeded !Natural !Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibQueryError

-- | Fixed-precedence product-query failure.  Shared resource-policy enums are
-- reused, but the error itself is nominally disjoint from the scalar query so
-- no scalar construction failure can be mistaken for product authority.
data LengthSpinePairSMTLibQueryError
  = LengthSpinePairSMTLibUnexpectedResultVariable
  | LengthSpinePairSMTLibInputVariableOutOfRange !Natural !Int
  | LengthSpinePairSMTLibQuotientDivisorZero
  | LengthSpinePairSMTLibModuloDivisorZero
  | LengthSpinePairSMTLibNumeralBitLimitExceeded
      !LengthSMTLibNumeralSite !Int !Int
  | LengthSpinePairSMTLibCommandByteLimitExceeded
      !LengthSMTLibCommandPart !Natural !Natural
  | LengthSpinePairSMTLibFingerprintByteLimitExceeded !Natural !Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSpinePairSMTLibQueryError

-- | Which Euclidean witness component is the value of one normalized source
-- expression.  The distinction also selects operation-specific private names
-- and the conditional lowering-policy tag.
data SMTEuclideanProjection
  = SMTNaturalQuotientProjection
  | SMTNaturalModuloProjection
  deriving (Eq, Ord, Show, Generic)

instance NFData SMTEuclideanProjection

-- | One private existential lowering receipt shared by positive-literal
-- natural quotient and modulo.  Both projections use the unique Euclidean
-- pair @e = k*q + r@ with @0 <= r < k@.  Names are allocated in deterministic
-- expression preorder, and only original input symbols are requested back.
data SMTEuclideanWitness = SMTEuclideanWitness
  !SMTEuclideanProjection
  !Natural
  ![Word8]
  ![Word8]
  QFLIAIntegerExpression
  deriving (Eq, Ord, Show, Generic)

instance NFData SMTEuclideanWitness

data SMTTranslationState = SMTTranslationState
  !Natural
  !(Map Natural SMTEuclideanWitness)

data SMTBinaryHelper = SMTNaturalMonus | SMTIntegerMinimum | SMTIntegerMaximum
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData SMTBinaryHelper

-- The complete typed plan remains local through bounded rendering and
-- structural fingerprinting.  Rendering therefore never becomes the semantic
-- source of truth, while the sealed query can retain only the runtime/replay
-- material which has a post-seal consumer.
data LengthSMTLibPlan = LengthSMTLibPlan
  ![[Word8]]
  !QFLIABooleanExpression
  ![QFLIACommand]
  !(Maybe QFLIACommand)
  ![SMTEuclideanWitness]

-- | Opaque association of one checked problem, bounded check commands, and
-- collision-free structural translation identity.  Exact input symbols and
-- the canonical value request are derived from the problem's sealed arity.
data LengthSMTLibQuery identity local = LengthSMTLibQuery
  !(CheckedLengthProblem identity local)
  ![Word8]
  !(Fingerprint LengthSMTLibQueryFingerprintSubject)

type role LengthSMTLibQuery nominal nominal

instance NFData (LengthSMTLibQuery identity local) where
  rnf (LengthSMTLibQuery problem check fingerprint) =
    rnf problem `seq` rnf check `seq` rnf fingerprint

-- | Opaque association of one checked binary-product problem, its bounded
-- canonical check commands, and the structurally complete product-query
-- identity.  Phantom roles remain nominal just as they do for scalar queries.
data LengthSpinePairSMTLibQuery identity local =
  LengthSpinePairSMTLibQuery
    !(CheckedLengthSpinePairProblem identity local)
    ![Word8]
    !(Fingerprint LengthSpinePairSMTLibQueryFingerprintSubject)

type role LengthSpinePairSMTLibQuery nominal nominal

instance NFData (LengthSpinePairSMTLibQuery identity local) where
  rnf (LengthSpinePairSMTLibQuery problem check fingerprint) =
    rnf problem `seq` rnf check `seq` rnf fingerprint

-- | Translate and seal one exact checked problem.  The combined bad-state
-- formula is never accepted separately from its problem authority.
sealLengthSMTLibQuery
  :: LengthSMTLibLimits
  -> CheckedLengthProblem identity local
  -> Either LengthSMTLibQueryError (LengthSMTLibQuery identity local)
sealLengthSMTLibQuery limits problem = do
  let inputCount = checkedLengthProblemInputCount problem
      symbols = inputSymbolsForCount inputCount
      valueRequest = inputValueRequestForSymbols symbols
  (condition, translation) <- translateFormula limits inputCount
    emptySMTTranslationState
    $ checkedLengthProblemCounterexampleCondition problem
  let witnesses = orderedEuclideanWitnesses translation
      witnessSymbols = concatMap euclideanWitnessSymbols witnesses
  let checkCommands = fixedPreamble
        ++ map QFLIADeclareInteger symbols
        ++ map QFLIADeclareInteger witnessSymbols
        ++ map (QFLIAAssert . nonnegative . QFLIAIntegerSymbol) symbols
        ++ concatMap euclideanWitnessCommands witnesses
        ++ [QFLIAAssert condition, QFLIACheckSatisfiable]
      plan = LengthSMTLibPlan
        symbols condition checkCommands valueRequest witnesses
  checkBytes <- retainCommand limits LengthSMTLibCheckCommand
    $ renderQFLIACommands checkCommands
  valueRequestBytes <- case valueRequest of
    Nothing -> Right Nothing
    Just command -> Just <$> retainCommand limits
      LengthSMTLibInputValueRequest (renderQFLIACommand command)
  fingerprint <- buildQueryFingerprint limits problem plan
    checkBytes valueRequestBytes
  pure $ LengthSMTLibQuery problem checkBytes fingerprint

lengthSMTLibQueryInputSymbols
  :: LengthSMTLibQuery identity local
  -> [[Word8]]
lengthSMTLibQueryInputSymbols = fst . lengthSMTLibQueryInputArtifacts

lengthSMTLibQueryCheckBytes
  :: LengthSMTLibQuery identity local
  -> [Word8]
lengthSMTLibQueryCheckBytes (LengthSMTLibQuery _ bytes _) = bytes

lengthSMTLibQueryInputValueRequestBytes
  :: LengthSMTLibQuery identity local
  -> Maybe [Word8]
lengthSMTLibQueryInputValueRequestBytes =
  snd . lengthSMTLibQueryInputArtifacts

lengthSMTLibQueryInputArtifacts
  :: LengthSMTLibQuery identity local
  -> ([[Word8]], Maybe [Word8])
lengthSMTLibQueryInputArtifacts (LengthSMTLibQuery problem _ _) =
  let symbols = inputSymbolsForCount
        $ checkedLengthProblemInputCount problem
  in (symbols, fmap renderQFLIACommand $ inputValueRequestForSymbols symbols)

lengthSMTLibQueryFingerprint
  :: LengthSMTLibQuery identity local
  -> Fingerprint LengthSMTLibQueryFingerprintSubject
lengthSMTLibQueryFingerprint (LengthSMTLibQuery _ _ fingerprint) =
  fingerprint

lengthSMTLibQueryBehavioralProblem
  :: LengthSMTLibQuery identity local
  -> BehavioralProblem FiniteListSpineLengthV1
lengthSMTLibQueryBehavioralProblem (LengthSMTLibQuery problem _ _) =
  checkedLengthProblemBehavioralProblem problem

-- | Project the candidate-independent replay-input scope retained by this
-- query's exact checked problem.  Query translation and execution identity do
-- not enter the scope.
lengthSMTLibQueryCounterexampleBankScope
  :: LengthSMTLibQuery identity local
  -> LengthCounterexampleBankScope identity
lengthSMTLibQueryCounterexampleBankScope
    (LengthSMTLibQuery problem _ _) =
  checkedLengthProblemCounterexampleBankScope problem

-- | Translate and seal the solver-neutral bad state retained by one exact
-- checked binary-product problem.  Its substituted formula already contains
-- only compact input variables, so the canonical program requests inputs and
-- never exposes either modeled result component as a solver binding.
sealLengthSpinePairSMTLibQuery
  :: LengthSMTLibLimits
  -> CheckedLengthSpinePairProblem identity local
  -> Either
      LengthSpinePairSMTLibQueryError
      (LengthSpinePairSMTLibQuery identity local)
sealLengthSpinePairSMTLibQuery limits problem = do
  let inputCount = checkedLengthSpinePairProblemInputCount problem
      symbols = inputSymbolsForCount inputCount
      valueRequest = inputValueRequestForSymbols symbols
  (condition, translation) <- mapSpinePairQueryFailure
    $ translateFormula limits inputCount emptySMTTranslationState
    $ checkedLengthSpinePairProblemCounterexampleCondition problem
  let witnesses = orderedEuclideanWitnesses translation
      witnessSymbols = concatMap euclideanWitnessSymbols witnesses
      checkCommands = fixedPreamble
        ++ map QFLIADeclareInteger symbols
        ++ map QFLIADeclareInteger witnessSymbols
        ++ map (QFLIAAssert . nonnegative . QFLIAIntegerSymbol) symbols
        ++ concatMap euclideanWitnessCommands witnesses
        ++ [QFLIAAssert condition, QFLIACheckSatisfiable]
      plan = LengthSMTLibPlan
        symbols condition checkCommands valueRequest witnesses
  checkBytes <- mapSpinePairQueryFailure
    $ retainCommand limits LengthSMTLibCheckCommand
    $ renderQFLIACommands checkCommands
  valueRequestBytes <- case valueRequest of
    Nothing -> Right Nothing
    Just command -> Just <$> mapSpinePairQueryFailure
      (retainCommand limits LengthSMTLibInputValueRequest
        $ renderQFLIACommand command)
  fingerprint <- buildSpinePairQueryFingerprint limits problem plan
    checkBytes valueRequestBytes
  pure $ LengthSpinePairSMTLibQuery problem checkBytes fingerprint

lengthSpinePairSMTLibQueryInputSymbols
  :: LengthSpinePairSMTLibQuery identity local
  -> [[Word8]]
lengthSpinePairSMTLibQueryInputSymbols =
  fst . lengthSpinePairSMTLibQueryInputArtifacts

lengthSpinePairSMTLibQueryCheckBytes
  :: LengthSpinePairSMTLibQuery identity local
  -> [Word8]
lengthSpinePairSMTLibQueryCheckBytes
    (LengthSpinePairSMTLibQuery _ bytes _) = bytes

lengthSpinePairSMTLibQueryInputValueRequestBytes
  :: LengthSpinePairSMTLibQuery identity local
  -> Maybe [Word8]
lengthSpinePairSMTLibQueryInputValueRequestBytes =
  snd . lengthSpinePairSMTLibQueryInputArtifacts

lengthSpinePairSMTLibQueryInputArtifacts
  :: LengthSpinePairSMTLibQuery identity local
  -> ([[Word8]], Maybe [Word8])
lengthSpinePairSMTLibQueryInputArtifacts
    (LengthSpinePairSMTLibQuery problem _ _) =
  let symbols = inputSymbolsForCount
        $ checkedLengthSpinePairProblemInputCount problem
  in (symbols, fmap renderQFLIACommand $ inputValueRequestForSymbols symbols)

lengthSpinePairSMTLibQueryFingerprint
  :: LengthSpinePairSMTLibQuery identity local
  -> Fingerprint LengthSpinePairSMTLibQueryFingerprintSubject
lengthSpinePairSMTLibQueryFingerprint
    (LengthSpinePairSMTLibQuery _ _ fingerprint) = fingerprint

lengthSpinePairSMTLibQueryBehavioralProblem
  :: LengthSpinePairSMTLibQuery identity local
  -> BehavioralProblem FiniteBinaryProductSpineLengthsV1
lengthSpinePairSMTLibQueryBehavioralProblem
    (LengthSpinePairSMTLibQuery problem _ _) =
  checkedLengthSpinePairProblemBehavioralProblem problem

lengthSpinePairSMTLibQueryCounterexampleBankScope
  :: LengthSpinePairSMTLibQuery identity local
  -> LengthSpinePairCounterexampleBankScope identity
lengthSpinePairSMTLibQueryCounterexampleBankScope
    (LengthSpinePairSMTLibQuery problem _ _) =
  checkedLengthSpinePairProblemCounterexampleBankScope problem

-- | One parser-decoded integer associated with its exact returned symbol.
-- Construction is intentionally public: this value is untrusted input and
-- cannot produce evidence without exact symbol checks and semantic replay.
data LengthSMTLibIntegerBinding = LengthSMTLibIntegerBinding
  { lengthSMTLibIntegerBindingSymbol :: [Word8]
  , lengthSMTLibIntegerBindingValue :: Integer
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibIntegerBinding

-- | Structural model rejection or independent replay failure.  Symbols
-- retained in errors have already passed the generated-symbol byte bound.
data LengthSMTLibModelError
  = LengthSMTLibBindingArityMismatch !Int !Int
  | LengthSMTLibBindingSymbolByteLimitExceeded
      !Int !Natural !Natural
  | LengthSMTLibUnknownInputSymbol !Int [Word8]
  | LengthSMTLibDuplicateInputSymbol !Int [Word8]
  | LengthSMTLibNegativeInputValue !Int [Word8] !Integer
  | LengthSMTLibMissingInputSymbol [Word8]
  | LengthSMTLibCounterexampleReplayRejected !LengthEvaluationError
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibModelError

-- | Decode exactly the tracked input symbols, restore source order, and ask
-- the existing domain evaluator to recompute the candidate result.  @Nothing@
-- means that the supplied assignment did not satisfy the sealed bad-state
-- semantics; it is not evidence and normally indicates a stale or spurious
-- solver model.
validateLengthSMTLibCounterexample
  :: LengthEvaluationLimits
  -> LengthSMTLibQuery identity local
  -> [LengthSMTLibIntegerBinding]
  -> Either LengthSMTLibModelError
      (Maybe
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthCounterexample))
validateLengthSMTLibCounterexample evaluationLimits query rawBindings = do
  let symbols = lengthSMTLibQueryInputSymbols query
      expected = length symbols
      observed = observedListLength expected rawBindings
  if observed == expected
    then pure ()
    else Left $ LengthSMTLibBindingArityMismatch expected observed
  let maximumSymbolBytes = fromIntegral $ maximum
        $ 0 : map length symbols
      expectedSymbols = Map.fromList $ zip symbols [0 :: Int ..]
  decoded <- foldM
    (decodeBinding maximumSymbolBytes expectedSymbols)
    Map.empty
    $ zip [0 :: Int ..] rawBindings
  ordered <- mapM (lookupDecoded decoded) symbols
  either (Left . LengthSMTLibCounterexampleReplayRejected) Right
    $ validateLengthProblemCounterexample evaluationLimits
        (queryProblem query)
        $ LengthProblemAssignment ordered

-- | Why direct input replay against a sealed query was rejected.  Evaluation
-- failures describe only bounded concrete replay; association failures expose
-- only the sanitized mismatch class from the generic evidence boundary.
data LengthSMTLibInputReplayError
  = LengthSMTLibInputReplayEvaluationRejected !LengthEvaluationError
  | LengthSMTLibInputReplayAssociationRejected !ReplayMismatch
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibInputReplayError

-- | Replay source-ordered natural inputs against the checked problem retained
-- by this exact query.  Input arity is observed productively by the Length
-- evaluator before any value is inspected.  A counterexample receipt is
-- released only after the freshly constructed evidence is associated back to
-- the same behavioral problem; @Nothing@ remains an ordinary non-counterexample.
replayLengthSMTLibCounterexampleInputs
  :: LengthEvaluationLimits
  -> LengthSMTLibQuery identity local
  -> [Natural]
  -> Either LengthSMTLibInputReplayError
      (Maybe ValidatedLengthCounterexample)
replayLengthSMTLibCounterexampleInputs evaluationLimits query inputs = do
  evidence <- either
    (Left . LengthSMTLibInputReplayEvaluationRejected)
    Right
    $ validateLengthProblemCounterexample evaluationLimits
        (queryProblem query)
        $ LengthProblemAssignment inputs
  traverse replay evidence
 where
  replay = either
    (Left . LengthSMTLibInputReplayAssociationRejected)
    Right
    . replayBehavioralEvidence (lengthSMTLibQueryBehavioralProblem query)

-- | Probe the canonical all-zero assignment for the compact modeled inputs
-- privately retained by this exact query.  The caller supplies neither arity,
-- symbols, nor values: those zeros are derived from the sealed checked
-- problem, then pass through the ordinary query-owned replay and association
-- gate above.  @Nothing@ is only a probe miss and carries no positive evidence.
probeLengthSMTLibCounterexampleAtOrigin
  :: LengthEvaluationLimits
  -> LengthSMTLibQuery identity local
  -> Either LengthSMTLibInputReplayError
      (Maybe ValidatedLengthCounterexample)
probeLengthSMTLibCounterexampleAtOrigin evaluationLimits query =
  replayLengthSMTLibCounterexampleInputs evaluationLimits query
    $ replicate (checkedLengthProblemInputCount $ queryProblem query) 0

-- | Fail-closed query-owned scalar simplification error.  The nested domain
-- error preserves bounded replay detail; association failure reveals only the
-- sanitized exact-problem mismatch class.
data LengthSMTLibCounterexampleSimplificationError
  = LengthSMTLibCounterexampleSimplificationRejected
      !LengthCounterexampleSimplificationError
  | LengthSMTLibCounterexampleSimplificationAssociationRejected
      !ReplayMismatch
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibCounterexampleSimplificationError

-- | Seek a strict deterministic improvement of one opaque scalar
-- counterexample through the checked problem retained by this exact query.
--
-- The query contributes only problem ownership and final evidence
-- association.  This function emits no commands, consumes no solver status,
-- and changes no query bytes or fingerprint.  @Right Nothing@ makes no claim:
-- either the dominated box was not admitted or its canonical first
-- counterexample was the revalidated anchor itself.
simplifyLengthSMTLibQueryCounterexample
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> LengthSMTLibQuery identity local
  -> ValidatedLengthCounterexample
  -> Either LengthSMTLibCounterexampleSimplificationError
      (Maybe ValidatedLengthCounterexampleSimplification)
simplifyLengthSMTLibQueryCounterexample evaluationLimits inputBoxLimits query
    counterexample = do
  evidence <- either
    (Left . LengthSMTLibCounterexampleSimplificationRejected)
    Right
    $ simplifyLengthProblemCounterexample evaluationLimits inputBoxLimits
        (queryProblem query) counterexample
  traverse replay evidence
 where
  replay = either
    (Left .
      LengthSMTLibCounterexampleSimplificationAssociationRejected)
    Right
    . replayBehavioralEvidence (lengthSMTLibQueryBehavioralProblem query)

-- | Why exhaustive finite-box validation through a sealed query failed.
-- Validation failures come only from the solver-independent Length verifier;
-- association failures expose the same sanitized exact-problem mismatch class
-- as one-assignment query-owned replay.
data LengthSMTLibInputBoxValidationError
  = LengthSMTLibInputBoxValidationRejected
      !LengthInputBoxValidationError
  | LengthSMTLibInputBoxValidationAssociationRejected !ReplayMismatch
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibInputBoxValidationError

-- | Exhaustively validate an explicitly finite, source-ordered input box
-- against the checked problem privately retained by this exact query.
--
-- Inclusive maximums are interpreted by the solver-independent Length
-- verifier.  The query supplies association authority only: this function
-- emits no SMT-LIB, consumes no live observation, and gives no authority to
-- @sat@, @unsat@, or @unknown@.  The first counterexample or the completed
-- bounded-validation receipt is released only after replay against the same
-- behavioral problem succeeds.
validateLengthSMTLibQueryInputBox
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> LengthSMTLibQuery identity local
  -> [Natural]
  -> Either LengthSMTLibInputBoxValidationError
      (LengthInputBoxValidation
        ValidatedLengthCounterexample
        ValidatedLengthInputBox)
validateLengthSMTLibQueryInputBox evaluationLimits inputBoxLimits query
    maximums = do
  validation <- either
    (Left . LengthSMTLibInputBoxValidationRejected)
    Right
    $ validateLengthProblemInputBox evaluationLimits inputBoxLimits
        (queryProblem query) maximums
  case validation of
    LengthInputBoxCounterexample evidence ->
      LengthInputBoxCounterexample <$> replay evidence
    LengthInputBoxValidated evidence ->
      LengthInputBoxValidated <$> replay evidence
 where
  replay
    :: BehavioralEvidence FiniteListSpineLengthV1 receipt
    -> Either LengthSMTLibInputBoxValidationError receipt
  replay = either
    (Left . LengthSMTLibInputBoxValidationAssociationRejected)
    Right
    . replayBehavioralEvidence (lengthSMTLibQueryBehavioralProblem query)

-- | Why current applicable-domain validation through one exact scalar query
-- failed. Semantic inapplicability remains a successful result; failures are
-- bounded validation rejection or exact evidence/problem mismatch.
data LengthSMTLibApplicableDomainValidationError
  = LengthSMTLibApplicableDomainValidationRejected
      !LengthApplicableDomainValidationError
  | LengthSMTLibApplicableDomainValidationAssociationRejected
      !ReplayMismatch
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibApplicableDomainValidationError

-- | Validate the entire applicable input domain of the scalar problem
-- retained by this query. The query supplies association authority only: no
-- command is emitted and no solver observation is consumed.
validateLengthSMTLibQueryApplicableDomain
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> LengthBooleanFiniteUnionLimits
  -> LengthSMTLibQuery identity local
  -> Either LengthSMTLibApplicableDomainValidationError
      (LengthApplicableDomainValidation
        ValidatedLengthCounterexample
        ValidatedLengthApplicableDomain)
validateLengthSMTLibQueryApplicableDomain
    evaluationLimits inputBoxLimits unionLimits query = do
  validation <- either
    (Left . LengthSMTLibApplicableDomainValidationRejected)
    Right
    $ validateLengthProblemApplicableDomain
        evaluationLimits inputBoxLimits unionLimits
        $ queryProblem query
  case validation of
    LengthApplicableDomainInapplicable inapplicability -> Right
      $ LengthApplicableDomainInapplicable inapplicability
    LengthApplicableDomainCounterexample evidence ->
      LengthApplicableDomainCounterexample <$> replay evidence
    LengthApplicableDomainEstablished evidence ->
      LengthApplicableDomainEstablished <$> replay evidence
 where
  replay
    :: BehavioralEvidence FiniteListSpineLengthV1 receipt
    -> Either LengthSMTLibApplicableDomainValidationError receipt
  replay = either
    (Left . LengthSMTLibApplicableDomainValidationAssociationRejected)
    Right
    . replayBehavioralEvidence (lengthSMTLibQueryBehavioralProblem query)

-- | Structural model rejection or independent product replay failure.
-- Parser-decoded bindings remain the shared, authority-free input type, while
-- every rejection and released receipt is product-domain specific.
data LengthSpinePairSMTLibModelError
  = LengthSpinePairSMTLibBindingArityMismatch !Int !Int
  | LengthSpinePairSMTLibBindingSymbolByteLimitExceeded
      !Int !Natural !Natural
  | LengthSpinePairSMTLibUnknownInputSymbol !Int [Word8]
  | LengthSpinePairSMTLibDuplicateInputSymbol !Int [Word8]
  | LengthSpinePairSMTLibNegativeInputValue !Int [Word8] !Integer
  | LengthSpinePairSMTLibMissingInputSymbol [Word8]
  | LengthSpinePairSMTLibCounterexampleReplayRejected
      !LengthSpinePairEvaluationError
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSpinePairSMTLibModelError

-- | Decode exactly the product query's tracked input symbols, restore source
-- order, and independently recompute both modeled result components before a
-- counterexample can become product-domain behavioral evidence.
validateLengthSpinePairSMTLibCounterexample
  :: LengthEvaluationLimits
  -> LengthSpinePairSMTLibQuery identity local
  -> [LengthSMTLibIntegerBinding]
  -> Either LengthSpinePairSMTLibModelError
      (Maybe
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairCounterexample))
validateLengthSpinePairSMTLibCounterexample evaluationLimits query
    rawBindings = do
  let symbols = lengthSpinePairSMTLibQueryInputSymbols query
      expected = length symbols
      observed = observedListLength expected rawBindings
  if observed == expected
    then pure ()
    else Left $ LengthSpinePairSMTLibBindingArityMismatch expected observed
  let maximumSymbolBytes = fromIntegral $ maximum
        $ 0 : map length symbols
      expectedSymbols = Map.fromList $ zip symbols [0 :: Int ..]
  decoded <- foldM
    (decodeSpinePairBinding maximumSymbolBytes expectedSymbols)
    Map.empty
    $ zip [0 :: Int ..] rawBindings
  ordered <- mapM (lookupSpinePairDecoded decoded) symbols
  either
    (Left . LengthSpinePairSMTLibCounterexampleReplayRejected)
    Right
    $ validateLengthSpinePairProblemCounterexample evaluationLimits
        (spinePairQueryProblem query)
        $ LengthProblemAssignment ordered

-- | Why query-owned direct input replay was rejected for the product domain.
data LengthSpinePairSMTLibInputReplayError
  = LengthSpinePairSMTLibInputReplayEvaluationRejected
      !LengthSpinePairEvaluationError
  | LengthSpinePairSMTLibInputReplayAssociationRejected !ReplayMismatch
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSpinePairSMTLibInputReplayError

-- | Replay source-ordered natural inputs through the exact checked product
-- problem retained by this query, then associate freshly constructed evidence
-- back to that same nominal product problem.
replayLengthSpinePairSMTLibCounterexampleInputs
  :: LengthEvaluationLimits
  -> LengthSpinePairSMTLibQuery identity local
  -> [Natural]
  -> Either LengthSpinePairSMTLibInputReplayError
      (Maybe ValidatedLengthSpinePairCounterexample)
replayLengthSpinePairSMTLibCounterexampleInputs evaluationLimits query
    inputs = do
  evidence <- either
    (Left . LengthSpinePairSMTLibInputReplayEvaluationRejected)
    Right
    $ validateLengthSpinePairProblemCounterexample evaluationLimits
        (spinePairQueryProblem query)
        $ LengthProblemAssignment inputs
  traverse replay evidence
 where
  replay = either
    (Left . LengthSpinePairSMTLibInputReplayAssociationRejected)
    Right
    . replayBehavioralEvidence
        (lengthSpinePairSMTLibQueryBehavioralProblem query)

-- | Query-owned all-zero probe for the product problem's compact modeled
-- inputs.  A miss is ordinary @Nothing@ and supplies no positive evidence.
probeLengthSpinePairSMTLibCounterexampleAtOrigin
  :: LengthEvaluationLimits
  -> LengthSpinePairSMTLibQuery identity local
  -> Either LengthSpinePairSMTLibInputReplayError
      (Maybe ValidatedLengthSpinePairCounterexample)
probeLengthSpinePairSMTLibCounterexampleAtOrigin evaluationLimits query =
  replayLengthSpinePairSMTLibCounterexampleInputs evaluationLimits query
    $ replicate
        (checkedLengthSpinePairProblemInputCount
          $ spinePairQueryProblem query)
        0

-- | Nominal product-domain failure for query-owned bounded counterexample
-- simplification.
data LengthSpinePairSMTLibCounterexampleSimplificationError
  = LengthSpinePairSMTLibCounterexampleSimplificationRejected
      !LengthSpinePairCounterexampleSimplificationError
  | LengthSpinePairSMTLibCounterexampleSimplificationAssociationRejected
      !ReplayMismatch
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSpinePairSMTLibCounterexampleSimplificationError

-- | Product-domain sibling of
-- 'simplifyLengthSMTLibQueryCounterexample'.  It retains nominal product
-- evidence and metadata while using the same deterministic dominated-box
-- ordering and exact-query association boundary.
simplifyLengthSpinePairSMTLibQueryCounterexample
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> LengthSpinePairSMTLibQuery identity local
  -> ValidatedLengthSpinePairCounterexample
  -> Either LengthSpinePairSMTLibCounterexampleSimplificationError
      (Maybe ValidatedLengthSpinePairCounterexampleSimplification)
simplifyLengthSpinePairSMTLibQueryCounterexample evaluationLimits
    inputBoxLimits query counterexample = do
  evidence <- either
    (Left . LengthSpinePairSMTLibCounterexampleSimplificationRejected)
    Right
    $ simplifyLengthSpinePairProblemCounterexample
        evaluationLimits inputBoxLimits
        (spinePairQueryProblem query) counterexample
  traverse replay evidence
 where
  replay = either
    (Left .
      LengthSpinePairSMTLibCounterexampleSimplificationAssociationRejected)
    Right
    . replayBehavioralEvidence
        (lengthSpinePairSMTLibQueryBehavioralProblem query)

-- | Why exhaustive finite-box validation through a product query failed.
data LengthSpinePairSMTLibInputBoxValidationError
  = LengthSpinePairSMTLibInputBoxValidationRejected
      !LengthSpinePairInputBoxValidationError
  | LengthSpinePairSMTLibInputBoxValidationAssociationRejected
      !ReplayMismatch
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSpinePairSMTLibInputBoxValidationError

-- | Exhaustively validate an explicitly finite input box through the exact
-- product problem retained by this query.  The query contributes association
-- authority only; no raw solver status participates.
validateLengthSpinePairSMTLibQueryInputBox
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> LengthSpinePairSMTLibQuery identity local
  -> [Natural]
  -> Either LengthSpinePairSMTLibInputBoxValidationError
      (LengthInputBoxValidation
        ValidatedLengthSpinePairCounterexample
        ValidatedLengthSpinePairInputBox)
validateLengthSpinePairSMTLibQueryInputBox evaluationLimits inputBoxLimits
    query maximums = do
  validation <- either
    (Left . LengthSpinePairSMTLibInputBoxValidationRejected)
    Right
    $ validateLengthSpinePairProblemInputBox evaluationLimits inputBoxLimits
        (spinePairQueryProblem query) maximums
  case validation of
    LengthInputBoxCounterexample evidence ->
      LengthInputBoxCounterexample <$> replay evidence
    LengthInputBoxValidated evidence ->
      LengthInputBoxValidated <$> replay evidence
 where
  replay
    :: BehavioralEvidence FiniteBinaryProductSpineLengthsV1 receipt
    -> Either LengthSpinePairSMTLibInputBoxValidationError receipt
  replay = either
    (Left . LengthSpinePairSMTLibInputBoxValidationAssociationRejected)
    Right
    . replayBehavioralEvidence
        (lengthSpinePairSMTLibQueryBehavioralProblem query)

-- | Nominal product-domain failure for current query-owned applicable-domain
-- validation.
data LengthSpinePairSMTLibApplicableDomainValidationError
  = LengthSpinePairSMTLibApplicableDomainValidationRejected
      !LengthSpinePairApplicableDomainValidationError
  | LengthSpinePairSMTLibApplicableDomainValidationAssociationRejected
      !ReplayMismatch
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSpinePairSMTLibApplicableDomainValidationError

-- | Query-owned current applicable-domain validation for the exact binary
-- product problem. Raw solver statuses remain authority-free; either evidence
-- arm is replayed independently against the query association.
validateLengthSpinePairSMTLibQueryApplicableDomain
  :: LengthEvaluationLimits
  -> LengthInputBoxLimits
  -> LengthBooleanFiniteUnionLimits
  -> LengthSpinePairSMTLibQuery identity local
  -> Either LengthSpinePairSMTLibApplicableDomainValidationError
      (LengthApplicableDomainValidation
        ValidatedLengthSpinePairCounterexample
        ValidatedLengthSpinePairApplicableDomain)
validateLengthSpinePairSMTLibQueryApplicableDomain
    evaluationLimits inputBoxLimits unionLimits query = do
  validation <- either
    (Left . LengthSpinePairSMTLibApplicableDomainValidationRejected)
    Right
    $ validateLengthSpinePairProblemApplicableDomain
        evaluationLimits inputBoxLimits unionLimits
        $ spinePairQueryProblem query
  case validation of
    LengthApplicableDomainInapplicable inapplicability -> Right
      $ LengthApplicableDomainInapplicable inapplicability
    LengthApplicableDomainCounterexample evidence ->
      LengthApplicableDomainCounterexample <$> replay evidence
    LengthApplicableDomainEstablished evidence ->
      LengthApplicableDomainEstablished <$> replay evidence
 where
  replay
    :: BehavioralEvidence FiniteBinaryProductSpineLengthsV1 receipt
    -> Either LengthSpinePairSMTLibApplicableDomainValidationError receipt
  replay = either
    (Left .
      LengthSpinePairSMTLibApplicableDomainValidationAssociationRejected)
    Right
    . replayBehavioralEvidence
        (lengthSpinePairSMTLibQueryBehavioralProblem query)

queryProblem
  :: LengthSMTLibQuery identity local
  -> CheckedLengthProblem identity local
queryProblem (LengthSMTLibQuery problem _ _) = problem

spinePairQueryProblem
  :: LengthSpinePairSMTLibQuery identity local
  -> CheckedLengthSpinePairProblem identity local
spinePairQueryProblem (LengthSpinePairSMTLibQuery problem _ _) = problem

decodeBinding
  :: Natural
  -> Map [Word8] Int
  -> Map [Word8] Natural
  -> (Int, LengthSMTLibIntegerBinding)
  -> Either LengthSMTLibModelError (Map [Word8] Natural)
decodeBinding maximumBytes expected decoded
    (bindingIndex, LengthSMTLibIntegerBinding rawSymbol rawValue) = do
  symbol <- retainModelSymbol bindingIndex maximumBytes rawSymbol
  case Map.lookup symbol expected of
    Nothing -> Left $ LengthSMTLibUnknownInputSymbol bindingIndex symbol
    Just _ -> pure ()
  if Map.member symbol decoded
    then Left $ LengthSMTLibDuplicateInputSymbol bindingIndex symbol
    else pure ()
  if rawValue < 0
    then Left $ LengthSMTLibNegativeInputValue bindingIndex symbol rawValue
    else pure $ Map.insert symbol (fromInteger rawValue) decoded

lookupDecoded
  :: Map [Word8] Natural
  -> [Word8]
  -> Either LengthSMTLibModelError Natural
lookupDecoded decoded symbol = case Map.lookup symbol decoded of
  Nothing -> Left $ LengthSMTLibMissingInputSymbol symbol
  Just value -> Right value

retainModelSymbol
  :: Int
  -> Natural
  -> [Word8]
  -> Either LengthSMTLibModelError [Word8]
retainModelSymbol bindingIndex maximumBytes = go maximumBytes
 where
  go _ [] = Right []
  go 0 (_ : _) = Left $ LengthSMTLibBindingSymbolByteLimitExceeded
    bindingIndex maximumBytes (maximumBytes + 1)
  go remaining (byte : bytes) =
    (byte :) <$> go (remaining - 1) bytes

decodeSpinePairBinding
  :: Natural
  -> Map [Word8] Int
  -> Map [Word8] Natural
  -> (Int, LengthSMTLibIntegerBinding)
  -> Either
      LengthSpinePairSMTLibModelError
      (Map [Word8] Natural)
decodeSpinePairBinding maximumBytes expected decoded
    (bindingIndex, LengthSMTLibIntegerBinding rawSymbol rawValue) = do
  symbol <- retainSpinePairModelSymbol bindingIndex maximumBytes rawSymbol
  case Map.lookup symbol expected of
    Nothing -> Left $ LengthSpinePairSMTLibUnknownInputSymbol
      bindingIndex symbol
    Just _ -> pure ()
  if Map.member symbol decoded
    then Left $ LengthSpinePairSMTLibDuplicateInputSymbol
      bindingIndex symbol
    else pure ()
  if rawValue < 0
    then Left $ LengthSpinePairSMTLibNegativeInputValue
      bindingIndex symbol rawValue
    else pure $ Map.insert symbol (fromInteger rawValue) decoded

lookupSpinePairDecoded
  :: Map [Word8] Natural
  -> [Word8]
  -> Either LengthSpinePairSMTLibModelError Natural
lookupSpinePairDecoded decoded symbol = case Map.lookup symbol decoded of
  Nothing -> Left $ LengthSpinePairSMTLibMissingInputSymbol symbol
  Just value -> Right value

retainSpinePairModelSymbol
  :: Int
  -> Natural
  -> [Word8]
  -> Either LengthSpinePairSMTLibModelError [Word8]
retainSpinePairModelSymbol bindingIndex maximumBytes = go maximumBytes
 where
  go _ [] = Right []
  go 0 (_ : _) = Left
    $ LengthSpinePairSMTLibBindingSymbolByteLimitExceeded
        bindingIndex maximumBytes (maximumBytes + 1)
  go remaining (byte : bytes) =
    (byte :) <$> go (remaining - 1) bytes

fixedPreamble :: [QFLIACommand]
fixedPreamble =
  [ QFLIASetOption (ascii ":produce-models") (ascii "true")
  , QFLIASetOption (ascii ":random-seed") (ascii "1")
  , QFLIASetLogic lengthSMTLibQueryLogic
  , helperDefinition SMTNaturalMonus
  , helperDefinition SMTIntegerMinimum
  , helperDefinition SMTIntegerMaximum
  ]

helperDefinition :: SMTBinaryHelper -> QFLIACommand
helperDefinition helper = QFLIADefineBinaryInteger
  (helperSymbol helper) x y body
 where
  x = ascii "x"
  y = ascii "y"
  left = QFLIAIntegerSymbol x
  right = QFLIAIntegerSymbol y
  body = case helper of
    SMTNaturalMonus -> QFLIAIntegerIf
      (QFLIAIntegerAtMost right left)
      (QFLIAIntegerDifference left right)
      (QFLIANaturalNumeral 0)
    SMTIntegerMinimum -> QFLIAIntegerIf
      (QFLIAIntegerAtMost left right) left right
    SMTIntegerMaximum -> QFLIAIntegerIf
      (QFLIAIntegerAtMost left right) right left

nonnegative :: QFLIAIntegerExpression -> QFLIABooleanExpression
nonnegative expression = QFLIAIntegerAtMost (QFLIANaturalNumeral 0) expression

translateExpression
  :: LengthSMTLibLimits
  -> Int
  -> SMTTranslationState
  -> LengthExpression LengthContractVariable
  -> Either
      LengthSMTLibQueryError
      (QFLIAIntegerExpression, SMTTranslationState)
translateExpression limits inputCount state source = case source of
  LengthVariable variable -> case variable of
    LengthResult -> Left LengthSMTLibUnexpectedResultVariable
    LengthInput position
      | position < fromIntegral inputCount ->
          Right (QFLIAIntegerSymbol $ inputSymbolNatural position, state)
      | otherwise -> Left $ LengthSMTLibInputVariableOutOfRange
          position inputCount
  LengthLiteral value -> do
    checkNumeral limits LengthSMTLibLiteralNumeral value
    pure (QFLIANaturalNumeral value, state)
  LengthSum terms -> do
    (translated, afterTerms) <- translateExpressions state terms
    pure (QFLIAIntegerSum translated, afterTerms)
  LengthScale factor expression -> do
    checkNumeral limits LengthSMTLibScaleNumeral factor
    (translated, afterExpression) <- translateExpression
      limits inputCount state expression
    pure (QFLIAIntegerScale factor translated, afterExpression)
  LengthQuotient divisor expression -> translateEuclideanProjection
    limits inputCount state
    SMTNaturalQuotientProjection
    LengthSMTLibQuotientDivisorNumeral
    LengthSMTLibQuotientDivisorZero
    divisor expression
  LengthModulo divisor expression -> translateEuclideanProjection
    limits inputCount state
    SMTNaturalModuloProjection
    LengthSMTLibModuloDivisorNumeral
    LengthSMTLibModuloDivisorZero
    divisor expression
  LengthMonus left right -> binary SMTNaturalMonus state left right
  LengthMinimum left right -> binary SMTIntegerMinimum state left right
  LengthMaximum left right -> binary SMTIntegerMaximum state left right
  LengthIf condition whenTrue whenFalse -> do
    (translatedCondition, afterCondition) <- translateFormula
      limits inputCount state condition
    (translatedTrue, afterTrue) <- translateExpression
      limits inputCount afterCondition whenTrue
    (translatedFalse, afterFalse) <- translateExpression
      limits inputCount afterTrue whenFalse
    pure
      ( QFLIAIntegerIf translatedCondition translatedTrue translatedFalse
      , afterFalse
      )
 where
  translateExpressions current terms = case terms of
    [] -> Right ([], current)
    term : remaining -> do
      (translated, afterTerm) <- translateExpression
        limits inputCount current term
      (following, afterFollowing) <- translateExpressions afterTerm remaining
      pure (translated : following, afterFollowing)

  binary helper current left right = do
    (translatedLeft, afterLeft) <- translateExpression
      limits inputCount current left
    (translatedRight, afterRight) <- translateExpression
      limits inputCount afterLeft right
    pure
      ( QFLIAIntegerBinaryApplication
          (helperSymbol helper) translatedLeft translatedRight
      , afterRight
      )

translateFormula
  :: LengthSMTLibLimits
  -> Int
  -> SMTTranslationState
  -> LengthFormula LengthContractVariable
  -> Either
      LengthSMTLibQueryError
      (QFLIABooleanExpression, SMTTranslationState)
translateFormula limits inputCount state source = case source of
  LengthTruth value -> Right (QFLIABooleanTruth value, state)
  LengthEqual left right -> comparison QFLIAIntegerEqual left right
  LengthAtMost left right -> comparison QFLIAIntegerAtMost left right
  LengthNot formula -> do
    (translated, afterFormula) <- translateFormula
      limits inputCount state formula
    pure (QFLIABooleanNot translated, afterFormula)
  LengthAll formulas -> do
    (translated, afterFormulas) <- translateFormulas state formulas
    pure (QFLIABooleanAll translated, afterFormulas)
 where
  comparison constructor left right = do
    (translatedLeft, afterLeft) <- translateExpression
      limits inputCount state left
    (translatedRight, afterRight) <- translateExpression
      limits inputCount afterLeft right
    pure (constructor translatedLeft translatedRight, afterRight)

  translateFormulas current formulas = case formulas of
    [] -> Right ([], current)
    formula : remaining -> do
      (translated, afterFormula) <- translateFormula
        limits inputCount current formula
      (following, afterFollowing) <- translateFormulas afterFormula remaining
      pure (translated : following, afterFollowing)

emptySMTTranslationState :: SMTTranslationState
emptySMTTranslationState = SMTTranslationState 0 Map.empty

translateEuclideanProjection
  :: LengthSMTLibLimits
  -> Int
  -> SMTTranslationState
  -> SMTEuclideanProjection
  -> LengthSMTLibNumeralSite
  -> LengthSMTLibQueryError
  -> Natural
  -> LengthExpression LengthContractVariable
  -> Either
      LengthSMTLibQueryError
      (QFLIAIntegerExpression, SMTTranslationState)
translateEuclideanProjection limits inputCount state projection numeralSite
    zeroError divisor expression = do
  if divisor == 0
    then Left zeroError
    else pure ()
  checkNumeral limits numeralSite divisor
  let (ordinal, quotient, remainder, reserved) =
        reserveEuclideanWitness projection state
  (translated, afterExpression) <- translateExpression
    limits inputCount reserved expression
  let witness = SMTEuclideanWitness
        projection divisor quotient remainder translated
      completed = retainEuclideanWitness ordinal witness afterExpression
      projected = case projection of
        SMTNaturalQuotientProjection -> quotient
        SMTNaturalModuloProjection -> remainder
  pure (QFLIAIntegerSymbol projected, completed)

reserveEuclideanWitness
  :: SMTEuclideanProjection
  -> SMTTranslationState
  -> (Natural, [Word8], [Word8], SMTTranslationState)
reserveEuclideanWitness projection
    (SMTTranslationState ordinal witnesses) =
  ( ordinal
  , euclideanQuotientSymbol projection ordinal
  , euclideanRemainderSymbol projection ordinal
  , SMTTranslationState (ordinal + 1) witnesses
  )

retainEuclideanWitness
  :: Natural
  -> SMTEuclideanWitness
  -> SMTTranslationState
  -> SMTTranslationState
retainEuclideanWitness ordinal witness (SMTTranslationState next witnesses) =
  SMTTranslationState next $ Map.insert ordinal witness witnesses

orderedEuclideanWitnesses :: SMTTranslationState -> [SMTEuclideanWitness]
orderedEuclideanWitnesses (SMTTranslationState _ witnesses) =
  Map.elems witnesses

euclideanWitnessSymbols :: SMTEuclideanWitness -> [[Word8]]
euclideanWitnessSymbols
    (SMTEuclideanWitness _ _ quotient remainder _) =
  [quotient, remainder]

euclideanWitnessCommands :: SMTEuclideanWitness -> [QFLIACommand]
euclideanWitnessCommands
    (SMTEuclideanWitness _ divisor quotientSymbol remainderSymbol expression) =
  [ QFLIAAssert $ nonnegative quotient
  , QFLIAAssert $ nonnegative remainder
  , QFLIAAssert $ QFLIAIntegerAtMost remainder
      $ QFLIANaturalNumeral $ divisor - 1
  , QFLIAAssert $ QFLIAIntegerEqual expression $ QFLIAIntegerSum
      [QFLIAIntegerScale divisor quotient, remainder]
  ]
 where
  quotient = QFLIAIntegerSymbol quotientSymbol
  remainder = QFLIAIntegerSymbol remainderSymbol

checkNumeral
  :: LengthSMTLibLimits
  -> LengthSMTLibNumeralSite
  -> Natural
  -> Either LengthSMTLibQueryError ()
checkNumeral limits site value =
  let maximumBits = lengthSMTLibNumeralBitLimit limits
      observedBits = observedNaturalBits maximumBits value
  in if observedBits <= maximumBits
      then Right ()
      else Left $ LengthSMTLibNumeralBitLimitExceeded
        site maximumBits observedBits

retainCommand
  :: LengthSMTLibLimits
  -> LengthSMTLibCommandPart
  -> [Word8]
  -> Either LengthSMTLibQueryError [Word8]
retainCommand limits part = go maximumBytes
 where
  maximumBytes = lengthSMTLibCommandByteLimit limits

  go _ [] = Right []
  go 0 (_ : _) = Left $ LengthSMTLibCommandByteLimitExceeded
    part maximumBytes (maximumBytes + 1)
  go remaining (byte : bytes) =
    (byte :) <$> go (remaining - 1) bytes

mapSpinePairQueryFailure
  :: Either LengthSMTLibQueryError value
  -> Either LengthSpinePairSMTLibQueryError value
mapSpinePairQueryFailure = either (Left . go) Right
 where
  go failure = case failure of
    LengthSMTLibUnexpectedResultVariable ->
      LengthSpinePairSMTLibUnexpectedResultVariable
    LengthSMTLibInputVariableOutOfRange position inputCount ->
      LengthSpinePairSMTLibInputVariableOutOfRange position inputCount
    LengthSMTLibQuotientDivisorZero ->
      LengthSpinePairSMTLibQuotientDivisorZero
    LengthSMTLibModuloDivisorZero ->
      LengthSpinePairSMTLibModuloDivisorZero
    LengthSMTLibNumeralBitLimitExceeded site maximumLimit observed ->
      LengthSpinePairSMTLibNumeralBitLimitExceeded
        site maximumLimit observed
    LengthSMTLibCommandByteLimitExceeded part maximumLimit observed ->
      LengthSpinePairSMTLibCommandByteLimitExceeded
        part maximumLimit observed
    LengthSMTLibFingerprintByteLimitExceeded maximumLimit observed ->
      LengthSpinePairSMTLibFingerprintByteLimitExceeded
        maximumLimit observed

buildQueryFingerprint
  :: LengthSMTLibLimits
  -> CheckedLengthProblem identity local
  -> LengthSMTLibPlan
  -> [Word8]
  -> Maybe [Word8]
  -> Either LengthSMTLibQueryError
      (Fingerprint LengthSMTLibQueryFingerprintSubject)
buildQueryFingerprint limits problem plan checkBytes valueRequestBytes =
  case buildFingerprintWithin
      (lengthSMTLibFingerprintByteLimit limits) builder of
    Left FingerprintLimitExceeded
        { fingerprintMaximumBytes = maximumBytes
        , fingerprintObservedBytesAtLeast = observedBytes
        } -> Left $ LengthSMTLibFingerprintByteLimitExceeded
          maximumBytes observedBytes
    Right fingerprint -> Right fingerprint
 where
  behavioral = checkedLengthProblemBehavioralProblem problem
  builder = FingerprintBuilder
    { fingerprintBuilderVersion = 1
    , fingerprintBuilderRole = ascii
        "finite-list-spine-length/z3-qf-lia-query"
    , fingerprintBuilderFields =
        [ tagged "schema" [FingerprintBytes lengthSMTLibQuerySchemaTag]
        , tagged "logic" [FingerprintBytes lengthSMTLibQueryLogic]
        , tagged "fixed-options"
            [ FingerprintBytes $ ascii ":produce-models=true"
            , FingerprintBytes $ ascii ":random-seed=1"
            ]
        , tagged "problem-domain"
            [FingerprintBytes $ behavioralProblemDomain behavioral]
        , tagged "problem-inventory"
            [ FingerprintBytes $ fingerprintCanonicalBytes
                $ behavioralProblemInventoryFingerprint behavioral
            ]
        , tagged "problem-encoding"
            [ FingerprintBytes $ fingerprintCanonicalBytes
                $ behavioralProblemEncodingFingerprint behavioral
            ]
        , tagged "problem-candidate"
            [ FingerprintBytes $ fingerprintCanonicalBytes
                $ behavioralProblemCandidateFingerprint behavioral
            ]
        , tagged "complete-problem"
            [ FingerprintBytes $ fingerprintCanonicalBytes
                $ behavioralProblemFingerprint behavioral
            ]
        , tagged "typed-plan" [planField plan]
        , tagged "check-command" [FingerprintBytes checkBytes]
        , tagged "input-value-request" [case valueRequestBytes of
            Nothing -> tagged "absent" []
            Just bytes -> tagged "present" [FingerprintBytes bytes]]
        ]
    }

buildSpinePairQueryFingerprint
  :: LengthSMTLibLimits
  -> CheckedLengthSpinePairProblem identity local
  -> LengthSMTLibPlan
  -> [Word8]
  -> Maybe [Word8]
  -> Either LengthSpinePairSMTLibQueryError
      (Fingerprint LengthSpinePairSMTLibQueryFingerprintSubject)
buildSpinePairQueryFingerprint limits problem plan checkBytes
    valueRequestBytes =
  case buildFingerprintWithin
      (lengthSMTLibFingerprintByteLimit limits) builder of
    Left FingerprintLimitExceeded
        { fingerprintMaximumBytes = maximumBytes
        , fingerprintObservedBytesAtLeast = observedBytes
        } -> Left $ LengthSpinePairSMTLibFingerprintByteLimitExceeded
          maximumBytes observedBytes
    Right fingerprint -> Right fingerprint
 where
  behavioral = checkedLengthSpinePairProblemBehavioralProblem problem
  builder = FingerprintBuilder
    { fingerprintBuilderVersion = 1
    , fingerprintBuilderRole = ascii
        "finite-binary-product-spine-lengths/z3-qf-lia-query"
    , fingerprintBuilderFields =
        [ tagged "schema"
            [FingerprintBytes lengthSpinePairSMTLibQuerySchemaTag]
        , tagged "logic" [FingerprintBytes lengthSpinePairSMTLibQueryLogic]
        , tagged "fixed-options"
            [ FingerprintBytes $ ascii ":produce-models=true"
            , FingerprintBytes $ ascii ":random-seed=1"
            ]
        , tagged "problem-domain"
            [FingerprintBytes $ behavioralProblemDomain behavioral]
        , tagged "problem-inventory"
            [ FingerprintBytes $ fingerprintCanonicalBytes
                $ behavioralProblemInventoryFingerprint behavioral
            ]
        , tagged "problem-encoding"
            [ FingerprintBytes $ fingerprintCanonicalBytes
                $ behavioralProblemEncodingFingerprint behavioral
            ]
        , tagged "problem-candidate"
            [ FingerprintBytes $ fingerprintCanonicalBytes
                $ behavioralProblemCandidateFingerprint behavioral
            ]
        , tagged "complete-problem"
            [ FingerprintBytes $ fingerprintCanonicalBytes
                $ behavioralProblemFingerprint behavioral
            ]
        , tagged "typed-plan" [planField plan]
        , tagged "check-command" [FingerprintBytes checkBytes]
        , tagged "input-value-request" [case valueRequestBytes of
            Nothing -> tagged "absent" []
            Just bytes -> tagged "present" [FingerprintBytes bytes]]
        ]
    }

planField :: LengthSMTLibPlan -> FingerprintField
planField (LengthSMTLibPlan symbols condition commands request witnesses) =
  tagged "plan" $
    [ tagged "input-symbols" $ map FingerprintBytes symbols
    , tagged "condition" [qfliaBooleanExpressionFingerprintField condition]
    , tagged "check-commands" $ map qfliaCommandFingerprintField commands
    , tagged "value-request" [case request of
        Nothing -> tagged "absent" []
        Just command -> tagged "present"
          [qfliaCommandFingerprintField command]]
    ] ++ expressionLoweringFields witnesses

expressionLoweringFields :: [SMTEuclideanWitness] -> [FingerprintField]
expressionLoweringFields witnesses = case loweringTags of
  [] -> []
  _ -> [tagged "expression-lowering" $ map FingerprintBytes loweringTags]
 where
  projections = map euclideanWitnessProjection witnesses
  loweringTags =
    [ tag
    | (projection, tag) <-
        [ ( SMTNaturalModuloProjection
          , positiveLiteralNaturalModuloWitnessSchemaTag
          )
        , ( SMTNaturalQuotientProjection
          , positiveLiteralNaturalQuotientWitnessSchemaTag
          )
        ]
    , projection `elem` projections
    ]

euclideanWitnessProjection :: SMTEuclideanWitness -> SMTEuclideanProjection
euclideanWitnessProjection (SMTEuclideanWitness projection _ _ _ _) =
  projection

tagged :: String -> [FingerprintField] -> FingerprintField
tagged name = FingerprintTag (ascii name)

helperSymbol :: SMTBinaryHelper -> [Word8]
helperSymbol helper = ascii $ case helper of
  SMTNaturalMonus -> "djex_nat_monus"
  SMTIntegerMinimum -> "djex_nat_min"
  SMTIntegerMaximum -> "djex_nat_max"

-- | Canonical decoder symbols derived from the exact sealed contract arity.
-- Query sealing uses this list before rendering and fingerprinting; public
-- projections later recompute the same bounded values without retaining a
-- parallel cache inside the opaque query.
inputSymbolsForCount :: Int -> [[Word8]]
inputSymbolsForCount inputCount = map inputSymbol [0 .. inputCount - 1]

inputValueRequestForSymbols :: [[Word8]] -> Maybe QFLIACommand
inputValueRequestForSymbols symbols = case symbols of
  [] -> Nothing
  _ -> Just $ QFLIAGetValues $ map QFLIAIntegerSymbol symbols

inputSymbol :: Int -> [Word8]
inputSymbol = inputSymbolNatural . fromIntegral

inputSymbolNatural :: Natural -> [Word8]
inputSymbolNatural position = ascii "djex_length_input_" ++ ascii (show position)

moduloQuotientSymbol :: Natural -> [Word8]
moduloQuotientSymbol ordinal =
  ascii "djex_length_modulo_quotient_" ++ ascii (show ordinal)

moduloRemainderSymbol :: Natural -> [Word8]
moduloRemainderSymbol ordinal =
  ascii "djex_length_modulo_remainder_" ++ ascii (show ordinal)

quotientQuotientSymbol :: Natural -> [Word8]
quotientQuotientSymbol ordinal =
  ascii "djex_length_quotient_quotient_" ++ ascii (show ordinal)

quotientRemainderSymbol :: Natural -> [Word8]
quotientRemainderSymbol ordinal =
  ascii "djex_length_quotient_remainder_" ++ ascii (show ordinal)

euclideanQuotientSymbol :: SMTEuclideanProjection -> Natural -> [Word8]
euclideanQuotientSymbol projection = case projection of
  SMTNaturalQuotientProjection -> quotientQuotientSymbol
  SMTNaturalModuloProjection -> moduloQuotientSymbol

euclideanRemainderSymbol :: SMTEuclideanProjection -> Natural -> [Word8]
euclideanRemainderSymbol projection = case projection of
  SMTNaturalQuotientProjection -> quotientRemainderSymbol
  SMTNaturalModuloProjection -> moduloRemainderSymbol

positiveLiteralNaturalModuloWitnessSchemaTag :: [Word8]
positiveLiteralNaturalModuloWitnessSchemaTag = ascii
  "djex-length-z3-qf-lia-positive-literal-modulo-witness/v1"

positiveLiteralNaturalQuotientWitnessSchemaTag :: [Word8]
positiveLiteralNaturalQuotientWitnessSchemaTag = ascii
  "djex-length-z3-qf-lia-positive-literal-natural-quotient-witness/v1"

ascii :: String -> [Word8]
ascii = map $ fromIntegral . fromEnum
