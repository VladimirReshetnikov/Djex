{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Private construction and shared bounded kernel for nominal Length
-- counterexample replay-input banks.
module Language.Haskell.Synthesis.Internal.Semantic.Length.CounterexampleBank
where

import Control.Monad (when)
import Control.DeepSeq (NFData (rnf), force)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Collection (observedListLength)
import Language.Haskell.Synthesis.Count
  ( naturalLength
  , observedNaturalBits
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
  , FingerprintLimitError
  , buildFingerprintWithin
  , fingerprintCanonicalBytes
  )
import qualified Language.Haskell.Synthesis.Internal.Fingerprint.Type
  as FingerprintType
import Language.Haskell.Synthesis.Internal.Semantic.Length
  ( CheckedLengthContract
  , CheckedLengthSpinePairContract
  , FiniteBinaryProductSpineLengthsV1
  , FiniteListSpineLengthV1
  , LengthContractFingerprintSubject
  , LengthSpinePairContractFingerprintSubject
  , ascii
  , checkedLengthContractTarget
  , checkedLengthSpinePairContractTarget
  , lengthContractFingerprint
  , lengthFingerprintByteLimit
  , lengthSpinePairContractFingerprint
  , tagged
  )
import Language.Haskell.Synthesis.Internal.Semantic.Length.Problem
  ( CheckedLengthSession
  , LengthEncodingPolicyFingerprintSubject
  , buildFiniteBinaryProductSpineLengthsInventoryFingerprint
  , checkedLengthSessionLimits
  , lengthSessionEncodingPolicyFingerprint
  , lengthSessionInventoryFingerprint
  )
import Language.Haskell.Synthesis.Semantic.Problem
  ( InventoryFingerprintSubject )
import Language.Haskell.Synthesis.Type
  ( Type
  , Variable (..)
  , freeVariablesInFirstOccurrenceOrder
  )

-- Scope identity ------------------------------------------------------------

data LengthCounterexampleBankScopeFingerprintSubject
data LengthCounterexampleBankTargetFingerprintSubject

data LengthSpinePairCounterexampleBankScopeFingerprintSubject
data LengthSpinePairCounterexampleBankTargetFingerprintSubject

lengthCounterexampleBankScopeSchemaTag :: [Word8]
lengthCounterexampleBankScopeSchemaTag =
  ascii "djex-length-counterexample-bank-scope/v1"

lengthSpinePairCounterexampleBankScopeSchemaTag :: [Word8]
lengthSpinePairCounterexampleBankScopeSchemaTag =
  ascii "djex-length-spine-pair-counterexample-bank-scope/v1"

-- | Candidate-independent scalar replay scope.
--
-- The inventory key is the session's complete annotation-erased source
-- inventory, checked spine model, and admitted provider-law/trust table.  The
-- policy key is the session's solver-neutral interpretation/model policy.
-- The contract and normalized target are retained as separate exact keys
-- because the historical contract fingerprint deliberately omits the target.
data LengthCounterexampleBankScope identity =
  LengthCounterexampleBankScope
    !(Fingerprint
        (InventoryFingerprintSubject FiniteListSpineLengthV1))
    !(Fingerprint LengthEncodingPolicyFingerprintSubject)
    !(Fingerprint LengthContractFingerprintSubject)
    !(Fingerprint LengthCounterexampleBankTargetFingerprintSubject)
    !(Fingerprint LengthCounterexampleBankScopeFingerprintSubject)

type role LengthCounterexampleBankScope nominal

instance NFData (LengthCounterexampleBankScope identity) where
  rnf (LengthCounterexampleBankScope inventory policy contract target scope) =
    rnf inventory `seq` rnf policy `seq` rnf contract `seq`
    rnf target `seq` rnf scope

-- | Candidate-independent product replay scope, nominally disjoint from the
-- scalar scope even when all concrete replay inputs happen to agree.
data LengthSpinePairCounterexampleBankScope identity =
  LengthSpinePairCounterexampleBankScope
    !(Fingerprint
        (InventoryFingerprintSubject FiniteBinaryProductSpineLengthsV1))
    !(Fingerprint LengthEncodingPolicyFingerprintSubject)
    !(Fingerprint LengthSpinePairContractFingerprintSubject)
    !(Fingerprint LengthSpinePairCounterexampleBankTargetFingerprintSubject)
    !(Fingerprint LengthSpinePairCounterexampleBankScopeFingerprintSubject)

type role LengthSpinePairCounterexampleBankScope nominal

instance NFData (LengthSpinePairCounterexampleBankScope identity) where
  rnf (LengthSpinePairCounterexampleBankScope inventory policy contract target
      scope) =
    rnf inventory `seq` rnf policy `seq` rnf contract `seq`
    rnf target `seq` rnf scope

lengthCounterexampleBankScopeFingerprint
  :: LengthCounterexampleBankScope identity
  -> Fingerprint LengthCounterexampleBankScopeFingerprintSubject
lengthCounterexampleBankScopeFingerprint
    (LengthCounterexampleBankScope _ _ _ _ fingerprint) = fingerprint

lengthCounterexampleBankScopeTargetFingerprint
  :: LengthCounterexampleBankScope identity
  -> Fingerprint LengthCounterexampleBankTargetFingerprintSubject
lengthCounterexampleBankScopeTargetFingerprint
    (LengthCounterexampleBankScope _ _ _ target _) = target

lengthSpinePairCounterexampleBankScopeFingerprint
  :: LengthSpinePairCounterexampleBankScope identity
  -> Fingerprint LengthSpinePairCounterexampleBankScopeFingerprintSubject
lengthSpinePairCounterexampleBankScopeFingerprint
    (LengthSpinePairCounterexampleBankScope _ _ _ _ fingerprint) =
  fingerprint

lengthSpinePairCounterexampleBankScopeTargetFingerprint
  :: LengthSpinePairCounterexampleBankScope identity
  -> Fingerprint LengthSpinePairCounterexampleBankTargetFingerprintSubject
lengthSpinePairCounterexampleBankScopeTargetFingerprint
    (LengthSpinePairCounterexampleBankScope _ _ _ target _) = target

-- Package-private sealing edge.  It is called only after the supplied
-- contract has been revalidated through this exact session.
sealLengthCounterexampleBankScope
  :: Ord identity
  => CheckedLengthSession identity annotation
  -> CheckedLengthContract (Variable identity)
  -> Either
      FingerprintLimitError
      (LengthCounterexampleBankScope identity)
sealLengthCounterexampleBankScope session contract = do
  let inventory = lengthSessionInventoryFingerprint session
      policy = lengthSessionEncodingPolicyFingerprint session
      contractFingerprint = lengthContractFingerprint contract
      maximumBytes = fromIntegral $ lengthFingerprintByteLimit
        $ checkedLengthSessionLimits session
  target <- buildScalarTargetFingerprint maximumBytes
    $ checkedLengthContractTarget contract
  scope <- buildFingerprintWithin maximumBytes FingerprintBuilder
    { fingerprintBuilderVersion = 1
    , fingerprintBuilderRole = lengthCounterexampleBankScopeSchemaTag
    , fingerprintBuilderFields =
        [ tagged "dialect"
            [FingerprintBytes $ ascii "finite-list-spine-length/v1"]
        , tagged "scope-schema"
            [FingerprintBytes lengthCounterexampleBankScopeSchemaTag]
        , tagged "complete-session-inventory-and-provider-law-basis"
            [FingerprintBytes $ fingerprintCanonicalBytes inventory]
        , tagged "solver-neutral-interpretation-model-policy"
            [FingerprintBytes $ fingerprintCanonicalBytes policy]
        , tagged "exact-contract"
            [FingerprintBytes $ fingerprintCanonicalBytes contractFingerprint]
        , tagged "exact-normalized-target"
            [FingerprintBytes $ fingerprintCanonicalBytes target]
        , tagged "explicit-exclusions"
            [ FingerprintBytes $ ascii "no-candidate-graph-result-or-condition"
            , FingerprintBytes $ ascii "no-candidate-used-provider-subset"
            , FingerprintBytes $ ascii "no-query-solver-or-execution-identity"
            , FingerprintBytes $ ascii "no-preferences-receipts-or-verdicts"
            ]
        ]
    }
  pure $ LengthCounterexampleBankScope
    inventory policy contractFingerprint target scope

sealLengthSpinePairCounterexampleBankScope
  :: Ord identity
  => CheckedLengthSession identity annotation
  -> CheckedLengthSpinePairContract (Variable identity)
  -> Either
      FingerprintLimitError
      (LengthSpinePairCounterexampleBankScope identity)
sealLengthSpinePairCounterexampleBankScope session contract = do
  inventory <- buildFiniteBinaryProductSpineLengthsInventoryFingerprint session
  sealLengthSpinePairCounterexampleBankScopeWithInventory
    session inventory contract

-- Candidate construction already needs this exact product inventory for the
-- generic problem envelope.  This additive entrance avoids rebuilding it or
-- accepting any independently supplied scalar/product coercion.
sealLengthSpinePairCounterexampleBankScopeWithInventory
  :: Ord identity
  => CheckedLengthSession identity annotation
  -> Fingerprint
      (InventoryFingerprintSubject FiniteBinaryProductSpineLengthsV1)
  -> CheckedLengthSpinePairContract (Variable identity)
  -> Either
      FingerprintLimitError
      (LengthSpinePairCounterexampleBankScope identity)
sealLengthSpinePairCounterexampleBankScopeWithInventory session inventory
    contract = do
  let policy = lengthSessionEncodingPolicyFingerprint session
      contractFingerprint = lengthSpinePairContractFingerprint contract
      maximumBytes = fromIntegral $ lengthFingerprintByteLimit
        $ checkedLengthSessionLimits session
  target <- buildSpinePairTargetFingerprint maximumBytes
    $ checkedLengthSpinePairContractTarget contract
  scope <- buildFingerprintWithin maximumBytes FingerprintBuilder
    { fingerprintBuilderVersion = 1
    , fingerprintBuilderRole =
        lengthSpinePairCounterexampleBankScopeSchemaTag
    , fingerprintBuilderFields =
        [ tagged "dialect"
            [ FingerprintBytes $ ascii
                "finite-binary-product-spine-lengths/v1"
            ]
        , tagged "scope-schema"
            [ FingerprintBytes
                lengthSpinePairCounterexampleBankScopeSchemaTag
            ]
        , tagged "complete-session-inventory-and-provider-law-basis"
            [FingerprintBytes $ fingerprintCanonicalBytes inventory]
        , tagged "solver-neutral-interpretation-model-policy"
            [FingerprintBytes $ fingerprintCanonicalBytes policy]
        , tagged "exact-contract"
            [FingerprintBytes $ fingerprintCanonicalBytes contractFingerprint]
        , tagged "exact-normalized-target"
            [FingerprintBytes $ fingerprintCanonicalBytes target]
        , tagged "explicit-exclusions"
            [ FingerprintBytes $ ascii "no-candidate-graph-result-or-condition"
            , FingerprintBytes $ ascii "no-candidate-used-provider-subset"
            , FingerprintBytes $ ascii "no-query-solver-or-execution-identity"
            , FingerprintBytes $ ascii "no-preferences-receipts-or-verdicts"
            ]
        ]
    }
  pure $ LengthSpinePairCounterexampleBankScope
    inventory policy contractFingerprint target scope

buildScalarTargetFingerprint
  :: Ord identity
  => Natural
  -> Type (Variable identity)
  -> Either
      FingerprintLimitError
      (Fingerprint LengthCounterexampleBankTargetFingerprintSubject)
buildScalarTargetFingerprint maximumBytes = buildTargetFingerprint
  maximumBytes
  "finite-list-spine-length/counterexample-bank-normalized-target"

buildSpinePairTargetFingerprint
  :: Ord identity
  => Natural
  -> Type (Variable identity)
  -> Either
      FingerprintLimitError
      (Fingerprint LengthSpinePairCounterexampleBankTargetFingerprintSubject)
buildSpinePairTargetFingerprint maximumBytes = buildTargetFingerprint
  maximumBytes
  "finite-binary-product-spine-lengths/counterexample-bank-normalized-target"

buildTargetFingerprint
  :: Ord identity
  => Natural
  -> String
  -> Type (Variable identity)
  -> Either FingerprintLimitError (Fingerprint subject)
buildTargetFingerprint maximumBytes role target =
  buildFingerprintWithin maximumBytes FingerprintBuilder
    { fingerprintBuilderVersion = 1
    , fingerprintBuilderRole = ascii role
    , fingerprintBuilderFields =
        [ tagged "normalization"
            [ FingerprintBytes $ ascii "checked-normalize-type/v1"
            , FingerprintBytes $ ascii
                "lexical-alpha-positional-binder-slots/v1"
            , FingerprintBytes $ ascii
                "free-variable-first-occurrence-slots/v1"
            ]
        , tagged "target"
            [ FingerprintType.typeFingerprintField
                (targetAlphaVariableField slots)
                $ FingerprintType.canonicalTypeFingerprintForm
                $ alphaNormalizeTypeWith PositionalBinderSlots target
            ]
        ]
    }
 where
  slots = targetVariableSlots target

newtype TargetVariableSlots identity = TargetVariableSlots
  (Map (Variable identity) Natural)

targetVariableSlots
  :: Ord identity
  => Type (Variable identity)
  -> TargetVariableSlots identity
targetVariableSlots target = TargetVariableSlots $ snd $ List.foldl'
  insert (0, Map.empty) $ freeVariablesInFirstOccurrenceOrder target
 where
  insert (!next, !slots) variable
    | Map.member variable slots = (next, slots)
    | otherwise = (next + 1, Map.insert variable next slots)

targetAlphaVariableField
  :: Ord identity
  => TargetVariableSlots identity
  -> AlphaVariable (Variable identity)
  -> FingerprintField
targetAlphaVariableField _ (AlphaBoundVariable scope slot) = tagged "bound"
  [FingerprintNatural scope, FingerprintNatural slot]
targetAlphaVariableField (TargetVariableSlots slots)
    (AlphaFreeVariable variable) = tagged "free"
  [ case variable of
      FlexibleVariable{} -> FingerprintBytes $ ascii "flexible"
      RigidVariable{} -> FingerprintBytes $ ascii "rigid"
  , case Map.lookup variable slots of
      Nothing -> tagged "missing-target-variable-slot" []
      Just slot -> tagged "slot" [FingerprintNatural slot]
  ]

-- Shared bounded kernel -----------------------------------------------------

data BankKernelLimits = BankKernelLimits !Int !Int !Int !Natural !Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData BankKernelLimits

data BankKernelOrigin
  = BankKernelLiveModelReplay
  | BankKernelSolverIndependentReplay
  | BankKernelSimplificationReplay
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData BankKernelOrigin

data BankKernelSample = BankKernelSample
  !BankKernelOrigin ![Natural] !Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData BankKernelSample

data BankKernelStats = BankKernelStats
  !Natural -- retained entries
  !Natural -- retained encoded bytes
  !Natural -- successful records, including duplicate promotions
  !Natural -- duplicate promotions
  !Natural -- tail evictions
  !Natural -- cumulative replay attempts
  deriving (Eq, Ord, Show, Generic)

instance NFData BankKernelStats

data BankKernel = BankKernel
  !BankKernelLimits ![BankKernelSample] !BankKernelStats

instance NFData BankKernel where
  rnf (BankKernel limits samples stats) =
    rnf limits `seq` rnf samples `seq` rnf stats

data BankKernelError
  = BankKernelEntryLimitExceeded !Int !Int
  | BankKernelSampleWidthLimitExceeded !Int !Int
  | BankKernelNaturalBitLimitExceeded !Int !Int !Int
  | BankKernelSampleEncodedByteLimitExceeded !Natural !Natural
  | BankKernelReplayAttemptLimitExceeded !Natural !Natural

emptyBankKernel :: BankKernelLimits -> BankKernel
emptyBankKernel limits = BankKernel limits []
  $ BankKernelStats 0 0 0 0 0 0

insertBankKernelSample
  :: BankKernelOrigin
  -> [Natural]
  -> BankKernel
  -> Either BankKernelError BankKernel
insertBankKernelSample origin rawInputs
    (BankKernel limits@(BankKernelLimits maximumEntries _ _ maximumBytes _)
      samples stats) = do
  -- Capacity is checked before origin or input demand.  In particular, a
  -- zero-entry bank rejects a poisoned/cyclic sample without touching it.
  when (maximumEntries <= 0)
    $ Left $ BankKernelEntryLimitExceeded maximumEntries 1
  sample <- validateBankKernelSample limits origin rawInputs
  let inputs = bankKernelSampleInputs sample
      duplicate = any ((== inputs) . bankKernelSampleInputs) samples
      promoted = sample : filter
        ((/= inputs) . bankKernelSampleInputs) samples
      retained = retainNewestSamples maximumEntries maximumBytes promoted
      retainedCount = naturalLength retained
      retainedBytes = List.foldl'
        (\total entry -> total + bankKernelSampleEncodedBytes entry)
        0 retained
      candidateCount = naturalLength promoted
      evicted = candidateCount - retainedCount
      BankKernelStats _ _ recorded promotions previousEvictions attempts =
        stats
      retainedStats = BankKernelStats
        retainedCount
        retainedBytes
        (recorded + 1)
        (promotions + if duplicate then 1 else 0)
        (previousEvictions + evicted)
        attempts
      strictSamples = force retained
      strictStats = force retainedStats
  strictSamples `seq` strictStats `seq`
    pure (BankKernel limits strictSamples strictStats)

recordBankKernelReplayAttempt
  :: BankKernel
  -> Either BankKernelError BankKernel
recordBankKernelReplayAttempt
    (BankKernel limits@(BankKernelLimits _ _ _ _ maximumAttempts)
      samples (BankKernelStats entries bytes recorded promotions evictions
        attempts))
  | attempts >= maximumAttempts = Left
      $ BankKernelReplayAttemptLimitExceeded maximumAttempts
          (maximumAttempts + 1)
  | otherwise =
      let retainedStats = force $ BankKernelStats entries bytes recorded
            promotions evictions (attempts + 1)
      in retainedStats `seq`
          Right (BankKernel limits samples retainedStats)

validateBankKernelSample
  :: BankKernelLimits
  -> BankKernelOrigin
  -> [Natural]
  -> Either BankKernelError BankKernelSample
validateBankKernelSample (BankKernelLimits _ maximumWidth maximumBits
    maximumBytes _) origin rawInputs = do
  let observedWidth = observedListLength maximumWidth rawInputs
  when (observedWidth > maximumWidth)
    $ Left $ BankKernelSampleWidthLimitExceeded
      maximumWidth observedWidth
  (inputs, encodedElements) <- retainElements 0 rawInputs
  let encodedBytes = 1
        + encodedVariableNaturalByteCount (fromIntegral observedWidth)
        + encodedElements
      observedBytes = min (maximumBytes + 1) encodedBytes
  if encodedBytes > maximumBytes
    then Left $ BankKernelSampleEncodedByteLimitExceeded
      maximumBytes observedBytes
    else let sample = force $ BankKernelSample origin inputs encodedBytes
      in sample `seq` Right sample
 where
  retainElements !_ [] = Right ([], 0)
  retainElements !index (value : remaining) = do
    let observedBits = observedNaturalBits maximumBits value
    when (observedBits > maximumBits)
      $ Left $ BankKernelNaturalBitLimitExceeded
        index maximumBits observedBits
    let magnitudeBytes = max 1
          $ (fromIntegral observedBits + 7) `quot` 8
        encodedValueBytes = encodedVariableNaturalByteCount magnitudeBytes
          + magnitudeBytes
    (retained, remainingBytes) <- retainElements (index + 1) remaining
    pure (value : retained, encodedValueBytes + remainingBytes)

retainNewestSamples
  :: Int
  -> Natural
  -> [BankKernelSample]
  -> [BankKernelSample]
retainNewestSamples maximumEntries maximumBytes = go 0 0
 where
  go !_ !_ [] = []
  go !retainedCount !retainedBytes (sample : remaining)
    | retainedCount >= maximumEntries = []
    | nextBytes > maximumBytes = []
    | otherwise = sample : go
        (retainedCount + 1) nextBytes remaining
   where
    nextBytes = retainedBytes + bankKernelSampleEncodedBytes sample

encodedVariableNaturalByteCount :: Natural -> Natural
encodedVariableNaturalByteCount value = go 1 value
 where
  go !count remaining
    | remaining < 128 = count
    | otherwise = go (count + 1) $ remaining `quot` 128

bankKernelSampleOrigin :: BankKernelSample -> BankKernelOrigin
bankKernelSampleOrigin (BankKernelSample origin _ _) = origin

bankKernelSampleInputs :: BankKernelSample -> [Natural]
bankKernelSampleInputs (BankKernelSample _ inputs _) = inputs

bankKernelSampleEncodedBytes :: BankKernelSample -> Natural
bankKernelSampleEncodedBytes (BankKernelSample _ _ bytes) = bytes

-- Scalar public wrappers ----------------------------------------------------

data LengthCounterexampleBankLimits = LengthCounterexampleBankLimits
  !BankKernelLimits
  deriving (Eq, Ord, Show)

instance NFData LengthCounterexampleBankLimits where
  rnf (LengthCounterexampleBankLimits limits) = rnf limits

data LengthCounterexampleBankLimitField
  = LengthCounterexampleBankEntries
  | LengthCounterexampleBankSampleWidth
  | LengthCounterexampleBankNaturalBits
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthCounterexampleBankLimitField

data LengthCounterexampleBankLimitError
  = NegativeLengthCounterexampleBankLimit
      !LengthCounterexampleBankLimitField !Int
  | UnobservableLengthCounterexampleBankFirstExcess
      !LengthCounterexampleBankLimitField !Int
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthCounterexampleBankLimitError

mkLengthCounterexampleBankLimits
  :: Int
  -> Int
  -> Int
  -> Natural
  -> Natural
  -> Either
      LengthCounterexampleBankLimitError
      LengthCounterexampleBankLimits
mkLengthCounterexampleBankLimits entries width bits bytes attempts
  | entries < 0 = Left $ NegativeLengthCounterexampleBankLimit
      LengthCounterexampleBankEntries entries
  | width < 0 = Left $ NegativeLengthCounterexampleBankLimit
      LengthCounterexampleBankSampleWidth width
  | bits < 0 = Left $ NegativeLengthCounterexampleBankLimit
      LengthCounterexampleBankNaturalBits bits
  | width == maxBound = Left
      $ UnobservableLengthCounterexampleBankFirstExcess
          LengthCounterexampleBankSampleWidth width
  | bits == maxBound = Left
      $ UnobservableLengthCounterexampleBankFirstExcess
          LengthCounterexampleBankNaturalBits bits
  | otherwise = Right $ LengthCounterexampleBankLimits
      $ BankKernelLimits entries width bits bytes attempts

defaultLengthCounterexampleBankLimits :: LengthCounterexampleBankLimits
defaultLengthCounterexampleBankLimits = LengthCounterexampleBankLimits
  $ BankKernelLimits 4 8 256 4096 256

lengthCounterexampleBankEntryLimit
  :: LengthCounterexampleBankLimits -> Int
lengthCounterexampleBankEntryLimit
    (LengthCounterexampleBankLimits (BankKernelLimits value _ _ _ _)) = value

lengthCounterexampleBankSampleWidthLimit
  :: LengthCounterexampleBankLimits -> Int
lengthCounterexampleBankSampleWidthLimit
    (LengthCounterexampleBankLimits (BankKernelLimits _ value _ _ _)) = value

lengthCounterexampleBankNaturalBitLimit
  :: LengthCounterexampleBankLimits -> Int
lengthCounterexampleBankNaturalBitLimit
    (LengthCounterexampleBankLimits (BankKernelLimits _ _ value _ _)) = value

lengthCounterexampleBankEncodedByteLimit
  :: LengthCounterexampleBankLimits -> Natural
lengthCounterexampleBankEncodedByteLimit
    (LengthCounterexampleBankLimits (BankKernelLimits _ _ _ value _)) = value

lengthCounterexampleBankReplayAttemptLimit
  :: LengthCounterexampleBankLimits -> Natural
lengthCounterexampleBankReplayAttemptLimit
    (LengthCounterexampleBankLimits (BankKernelLimits _ _ _ _ value)) = value

data LengthCounterexampleBankOrigin = LengthCounterexampleBankOrigin
  !BankKernelOrigin
  deriving (Eq, Ord, Show)

instance NFData LengthCounterexampleBankOrigin where
  rnf (LengthCounterexampleBankOrigin origin) = rnf origin

lengthCounterexampleBankLiveModelReplayOrigin
  :: LengthCounterexampleBankOrigin
lengthCounterexampleBankLiveModelReplayOrigin =
  LengthCounterexampleBankOrigin BankKernelLiveModelReplay

lengthCounterexampleBankSolverIndependentReplayOrigin
  :: LengthCounterexampleBankOrigin
lengthCounterexampleBankSolverIndependentReplayOrigin =
  LengthCounterexampleBankOrigin BankKernelSolverIndependentReplay

lengthCounterexampleBankSimplificationReplayOrigin
  :: LengthCounterexampleBankOrigin
lengthCounterexampleBankSimplificationReplayOrigin =
  LengthCounterexampleBankOrigin BankKernelSimplificationReplay

data LengthCounterexampleBankSample = LengthCounterexampleBankSample
  !BankKernelSample
  deriving (Eq, Ord, Show)

instance NFData LengthCounterexampleBankSample where
  rnf (LengthCounterexampleBankSample sample) = rnf sample

lengthCounterexampleBankSampleInputs
  :: LengthCounterexampleBankSample -> [Natural]
lengthCounterexampleBankSampleInputs
    (LengthCounterexampleBankSample sample) = bankKernelSampleInputs sample

lengthCounterexampleBankSampleOrigin
  :: LengthCounterexampleBankSample -> LengthCounterexampleBankOrigin
lengthCounterexampleBankSampleOrigin
    (LengthCounterexampleBankSample sample) = LengthCounterexampleBankOrigin
      $ bankKernelSampleOrigin sample

lengthCounterexampleBankSampleEncodedByteCount
  :: LengthCounterexampleBankSample -> Natural
lengthCounterexampleBankSampleEncodedByteCount
    (LengthCounterexampleBankSample sample) =
  bankKernelSampleEncodedBytes sample

data LengthCounterexampleBankStats = LengthCounterexampleBankStats
  !BankKernelStats
  deriving (Eq, Ord, Show)

instance NFData LengthCounterexampleBankStats where
  rnf (LengthCounterexampleBankStats stats) = rnf stats

lengthCounterexampleBankStatsRetainedEntryCount
  :: LengthCounterexampleBankStats -> Natural
lengthCounterexampleBankStatsRetainedEntryCount
    (LengthCounterexampleBankStats (BankKernelStats value _ _ _ _ _)) = value

lengthCounterexampleBankStatsRetainedEncodedByteCount
  :: LengthCounterexampleBankStats -> Natural
lengthCounterexampleBankStatsRetainedEncodedByteCount
    (LengthCounterexampleBankStats (BankKernelStats _ value _ _ _ _)) = value

lengthCounterexampleBankStatsRecordedSampleCount
  :: LengthCounterexampleBankStats -> Natural
lengthCounterexampleBankStatsRecordedSampleCount
    (LengthCounterexampleBankStats (BankKernelStats _ _ value _ _ _)) = value

lengthCounterexampleBankStatsDuplicatePromotionCount
  :: LengthCounterexampleBankStats -> Natural
lengthCounterexampleBankStatsDuplicatePromotionCount
    (LengthCounterexampleBankStats (BankKernelStats _ _ _ value _ _)) = value

lengthCounterexampleBankStatsEvictedSampleCount
  :: LengthCounterexampleBankStats -> Natural
lengthCounterexampleBankStatsEvictedSampleCount
    (LengthCounterexampleBankStats (BankKernelStats _ _ _ _ value _)) = value

lengthCounterexampleBankStatsReplayAttemptCount
  :: LengthCounterexampleBankStats -> Natural
lengthCounterexampleBankStatsReplayAttemptCount
    (LengthCounterexampleBankStats (BankKernelStats _ _ _ _ _ value)) = value

data LengthCounterexampleBankError
  = LengthCounterexampleBankEntryLimitExceeded !Int !Int
  | LengthCounterexampleBankSampleWidthLimitExceeded !Int !Int
  | LengthCounterexampleBankNaturalBitLimitExceeded !Int !Int !Int
  | LengthCounterexampleBankSampleEncodedByteLimitExceeded !Natural !Natural
  | LengthCounterexampleBankReplayAttemptLimitExceeded !Natural !Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthCounterexampleBankError

data LengthCounterexampleBank identity = LengthCounterexampleBank
  !(LengthCounterexampleBankScope identity) !BankKernel

type role LengthCounterexampleBank nominal

instance NFData (LengthCounterexampleBank identity) where
  rnf (LengthCounterexampleBank scope kernel) = rnf scope `seq` rnf kernel

emptyLengthCounterexampleBank
  :: LengthCounterexampleBankLimits
  -> LengthCounterexampleBankScope identity
  -> LengthCounterexampleBank identity
emptyLengthCounterexampleBank (LengthCounterexampleBankLimits limits) scope =
  LengthCounterexampleBank scope $ emptyBankKernel limits

lengthCounterexampleBankScope
  :: LengthCounterexampleBank identity
  -> LengthCounterexampleBankScope identity
lengthCounterexampleBankScope (LengthCounterexampleBank scope _) = scope

-- | Compare only the complete candidate-independent scope identity.
--
-- A match authorizes at most attempting fresh replay of stored input vectors;
-- it never upgrades a sample into a verdict or evidence receipt.  The bank's
-- limits, entries, origins, and statistics are not inspected.
lengthCounterexampleBankMatchesScope
  :: LengthCounterexampleBankScope identity
  -> LengthCounterexampleBank identity
  -> Bool
lengthCounterexampleBankMatchesScope expected
    (LengthCounterexampleBank actual _) =
  lengthCounterexampleBankScopeFingerprint expected ==
    lengthCounterexampleBankScopeFingerprint actual

lengthCounterexampleBankLimits
  :: LengthCounterexampleBank identity
  -> LengthCounterexampleBankLimits
lengthCounterexampleBankLimits
    (LengthCounterexampleBank _ (BankKernel limits _ _)) =
  LengthCounterexampleBankLimits limits

lengthCounterexampleBankSamples
  :: LengthCounterexampleBank identity
  -> [LengthCounterexampleBankSample]
lengthCounterexampleBankSamples
    (LengthCounterexampleBank _ (BankKernel _ samples _)) =
  map LengthCounterexampleBankSample samples

lengthCounterexampleBankStats
  :: LengthCounterexampleBank identity -> LengthCounterexampleBankStats
lengthCounterexampleBankStats
    (LengthCounterexampleBank _ (BankKernel _ _ stats)) =
  LengthCounterexampleBankStats stats

insertLengthCounterexampleBankSample
  :: LengthCounterexampleBankOrigin
  -> [Natural]
  -> LengthCounterexampleBank identity
  -> Either
      LengthCounterexampleBankError
      (LengthCounterexampleBank identity)
insertLengthCounterexampleBankSample
    ~(LengthCounterexampleBankOrigin origin) inputs
    (LengthCounterexampleBank scope kernel) =
  LengthCounterexampleBank scope
    <$> either (Left . mapLengthBankError) Right
      (insertBankKernelSample origin inputs kernel)

recordLengthCounterexampleBankReplayAttempt
  :: LengthCounterexampleBank identity
  -> Either
      LengthCounterexampleBankError
      (LengthCounterexampleBank identity)
recordLengthCounterexampleBankReplayAttempt
    (LengthCounterexampleBank scope kernel) =
  LengthCounterexampleBank scope
    <$> either (Left . mapLengthBankError) Right
      (recordBankKernelReplayAttempt kernel)

mapLengthBankError :: BankKernelError -> LengthCounterexampleBankError
mapLengthBankError source = case source of
  BankKernelEntryLimitExceeded limit observed ->
    LengthCounterexampleBankEntryLimitExceeded limit observed
  BankKernelSampleWidthLimitExceeded limit observed ->
    LengthCounterexampleBankSampleWidthLimitExceeded limit observed
  BankKernelNaturalBitLimitExceeded index limit observed ->
    LengthCounterexampleBankNaturalBitLimitExceeded index limit observed
  BankKernelSampleEncodedByteLimitExceeded limit observed ->
    LengthCounterexampleBankSampleEncodedByteLimitExceeded limit observed
  BankKernelReplayAttemptLimitExceeded limit observed ->
    LengthCounterexampleBankReplayAttemptLimitExceeded limit observed

-- Product public wrappers ---------------------------------------------------

data LengthSpinePairCounterexampleBankLimits =
  LengthSpinePairCounterexampleBankLimits !BankKernelLimits
  deriving (Eq, Ord, Show)

instance NFData LengthSpinePairCounterexampleBankLimits where
  rnf (LengthSpinePairCounterexampleBankLimits limits) = rnf limits

data LengthSpinePairCounterexampleBankLimitField
  = LengthSpinePairCounterexampleBankEntries
  | LengthSpinePairCounterexampleBankSampleWidth
  | LengthSpinePairCounterexampleBankNaturalBits
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSpinePairCounterexampleBankLimitField

data LengthSpinePairCounterexampleBankLimitError
  = NegativeLengthSpinePairCounterexampleBankLimit
      !LengthSpinePairCounterexampleBankLimitField !Int
  | UnobservableLengthSpinePairCounterexampleBankFirstExcess
      !LengthSpinePairCounterexampleBankLimitField !Int
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSpinePairCounterexampleBankLimitError

mkLengthSpinePairCounterexampleBankLimits
  :: Int
  -> Int
  -> Int
  -> Natural
  -> Natural
  -> Either
      LengthSpinePairCounterexampleBankLimitError
      LengthSpinePairCounterexampleBankLimits
mkLengthSpinePairCounterexampleBankLimits entries width bits bytes attempts
  | entries < 0 = Left $ NegativeLengthSpinePairCounterexampleBankLimit
      LengthSpinePairCounterexampleBankEntries entries
  | width < 0 = Left $ NegativeLengthSpinePairCounterexampleBankLimit
      LengthSpinePairCounterexampleBankSampleWidth width
  | bits < 0 = Left $ NegativeLengthSpinePairCounterexampleBankLimit
      LengthSpinePairCounterexampleBankNaturalBits bits
  | width == maxBound = Left
      $ UnobservableLengthSpinePairCounterexampleBankFirstExcess
          LengthSpinePairCounterexampleBankSampleWidth width
  | bits == maxBound = Left
      $ UnobservableLengthSpinePairCounterexampleBankFirstExcess
          LengthSpinePairCounterexampleBankNaturalBits bits
  | otherwise = Right $ LengthSpinePairCounterexampleBankLimits
      $ BankKernelLimits entries width bits bytes attempts

defaultLengthSpinePairCounterexampleBankLimits
  :: LengthSpinePairCounterexampleBankLimits
defaultLengthSpinePairCounterexampleBankLimits =
  LengthSpinePairCounterexampleBankLimits
    $ BankKernelLimits 4 8 256 4096 256

lengthSpinePairCounterexampleBankEntryLimit
  :: LengthSpinePairCounterexampleBankLimits -> Int
lengthSpinePairCounterexampleBankEntryLimit
    (LengthSpinePairCounterexampleBankLimits
      (BankKernelLimits value _ _ _ _)) = value

lengthSpinePairCounterexampleBankSampleWidthLimit
  :: LengthSpinePairCounterexampleBankLimits -> Int
lengthSpinePairCounterexampleBankSampleWidthLimit
    (LengthSpinePairCounterexampleBankLimits
      (BankKernelLimits _ value _ _ _)) = value

lengthSpinePairCounterexampleBankNaturalBitLimit
  :: LengthSpinePairCounterexampleBankLimits -> Int
lengthSpinePairCounterexampleBankNaturalBitLimit
    (LengthSpinePairCounterexampleBankLimits
      (BankKernelLimits _ _ value _ _)) = value

lengthSpinePairCounterexampleBankEncodedByteLimit
  :: LengthSpinePairCounterexampleBankLimits -> Natural
lengthSpinePairCounterexampleBankEncodedByteLimit
    (LengthSpinePairCounterexampleBankLimits
      (BankKernelLimits _ _ _ value _)) = value

lengthSpinePairCounterexampleBankReplayAttemptLimit
  :: LengthSpinePairCounterexampleBankLimits -> Natural
lengthSpinePairCounterexampleBankReplayAttemptLimit
    (LengthSpinePairCounterexampleBankLimits
      (BankKernelLimits _ _ _ _ value)) = value

data LengthSpinePairCounterexampleBankOrigin =
  LengthSpinePairCounterexampleBankOrigin !BankKernelOrigin
  deriving (Eq, Ord, Show)

instance NFData LengthSpinePairCounterexampleBankOrigin where
  rnf (LengthSpinePairCounterexampleBankOrigin origin) = rnf origin

lengthSpinePairCounterexampleBankLiveModelReplayOrigin
  :: LengthSpinePairCounterexampleBankOrigin
lengthSpinePairCounterexampleBankLiveModelReplayOrigin =
  LengthSpinePairCounterexampleBankOrigin BankKernelLiveModelReplay

lengthSpinePairCounterexampleBankSolverIndependentReplayOrigin
  :: LengthSpinePairCounterexampleBankOrigin
lengthSpinePairCounterexampleBankSolverIndependentReplayOrigin =
  LengthSpinePairCounterexampleBankOrigin
    BankKernelSolverIndependentReplay

lengthSpinePairCounterexampleBankSimplificationReplayOrigin
  :: LengthSpinePairCounterexampleBankOrigin
lengthSpinePairCounterexampleBankSimplificationReplayOrigin =
  LengthSpinePairCounterexampleBankOrigin BankKernelSimplificationReplay

data LengthSpinePairCounterexampleBankSample =
  LengthSpinePairCounterexampleBankSample !BankKernelSample
  deriving (Eq, Ord, Show)

instance NFData LengthSpinePairCounterexampleBankSample where
  rnf (LengthSpinePairCounterexampleBankSample sample) = rnf sample

lengthSpinePairCounterexampleBankSampleInputs
  :: LengthSpinePairCounterexampleBankSample -> [Natural]
lengthSpinePairCounterexampleBankSampleInputs
    (LengthSpinePairCounterexampleBankSample sample) =
  bankKernelSampleInputs sample

lengthSpinePairCounterexampleBankSampleOrigin
  :: LengthSpinePairCounterexampleBankSample
  -> LengthSpinePairCounterexampleBankOrigin
lengthSpinePairCounterexampleBankSampleOrigin
    (LengthSpinePairCounterexampleBankSample sample) =
  LengthSpinePairCounterexampleBankOrigin $ bankKernelSampleOrigin sample

lengthSpinePairCounterexampleBankSampleEncodedByteCount
  :: LengthSpinePairCounterexampleBankSample -> Natural
lengthSpinePairCounterexampleBankSampleEncodedByteCount
    (LengthSpinePairCounterexampleBankSample sample) =
  bankKernelSampleEncodedBytes sample

data LengthSpinePairCounterexampleBankStats =
  LengthSpinePairCounterexampleBankStats !BankKernelStats
  deriving (Eq, Ord, Show)

instance NFData LengthSpinePairCounterexampleBankStats where
  rnf (LengthSpinePairCounterexampleBankStats stats) = rnf stats

lengthSpinePairCounterexampleBankStatsRetainedEntryCount
  :: LengthSpinePairCounterexampleBankStats -> Natural
lengthSpinePairCounterexampleBankStatsRetainedEntryCount
    (LengthSpinePairCounterexampleBankStats
      (BankKernelStats value _ _ _ _ _)) = value

lengthSpinePairCounterexampleBankStatsRetainedEncodedByteCount
  :: LengthSpinePairCounterexampleBankStats -> Natural
lengthSpinePairCounterexampleBankStatsRetainedEncodedByteCount
    (LengthSpinePairCounterexampleBankStats
      (BankKernelStats _ value _ _ _ _)) = value

lengthSpinePairCounterexampleBankStatsRecordedSampleCount
  :: LengthSpinePairCounterexampleBankStats -> Natural
lengthSpinePairCounterexampleBankStatsRecordedSampleCount
    (LengthSpinePairCounterexampleBankStats
      (BankKernelStats _ _ value _ _ _)) = value

lengthSpinePairCounterexampleBankStatsDuplicatePromotionCount
  :: LengthSpinePairCounterexampleBankStats -> Natural
lengthSpinePairCounterexampleBankStatsDuplicatePromotionCount
    (LengthSpinePairCounterexampleBankStats
      (BankKernelStats _ _ _ value _ _)) = value

lengthSpinePairCounterexampleBankStatsEvictedSampleCount
  :: LengthSpinePairCounterexampleBankStats -> Natural
lengthSpinePairCounterexampleBankStatsEvictedSampleCount
    (LengthSpinePairCounterexampleBankStats
      (BankKernelStats _ _ _ _ value _)) = value

lengthSpinePairCounterexampleBankStatsReplayAttemptCount
  :: LengthSpinePairCounterexampleBankStats -> Natural
lengthSpinePairCounterexampleBankStatsReplayAttemptCount
    (LengthSpinePairCounterexampleBankStats
      (BankKernelStats _ _ _ _ _ value)) = value

data LengthSpinePairCounterexampleBankError
  = LengthSpinePairCounterexampleBankEntryLimitExceeded !Int !Int
  | LengthSpinePairCounterexampleBankSampleWidthLimitExceeded !Int !Int
  | LengthSpinePairCounterexampleBankNaturalBitLimitExceeded !Int !Int !Int
  | LengthSpinePairCounterexampleBankSampleEncodedByteLimitExceeded
      !Natural !Natural
  | LengthSpinePairCounterexampleBankReplayAttemptLimitExceeded
      !Natural !Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSpinePairCounterexampleBankError

data LengthSpinePairCounterexampleBank identity =
  LengthSpinePairCounterexampleBank
    !(LengthSpinePairCounterexampleBankScope identity) !BankKernel

type role LengthSpinePairCounterexampleBank nominal

instance NFData (LengthSpinePairCounterexampleBank identity) where
  rnf (LengthSpinePairCounterexampleBank scope kernel) =
    rnf scope `seq` rnf kernel

emptyLengthSpinePairCounterexampleBank
  :: LengthSpinePairCounterexampleBankLimits
  -> LengthSpinePairCounterexampleBankScope identity
  -> LengthSpinePairCounterexampleBank identity
emptyLengthSpinePairCounterexampleBank
    (LengthSpinePairCounterexampleBankLimits limits) scope =
  LengthSpinePairCounterexampleBank scope $ emptyBankKernel limits

lengthSpinePairCounterexampleBankScope
  :: LengthSpinePairCounterexampleBank identity
  -> LengthSpinePairCounterexampleBankScope identity
lengthSpinePairCounterexampleBankScope
    (LengthSpinePairCounterexampleBank scope _) = scope

-- | Product-domain counterpart of 'lengthCounterexampleBankMatchesScope'.
-- A match grants replay-attempt association only and inspects no bank payload.
lengthSpinePairCounterexampleBankMatchesScope
  :: LengthSpinePairCounterexampleBankScope identity
  -> LengthSpinePairCounterexampleBank identity
  -> Bool
lengthSpinePairCounterexampleBankMatchesScope expected
    (LengthSpinePairCounterexampleBank actual _) =
  lengthSpinePairCounterexampleBankScopeFingerprint expected ==
    lengthSpinePairCounterexampleBankScopeFingerprint actual

lengthSpinePairCounterexampleBankLimits
  :: LengthSpinePairCounterexampleBank identity
  -> LengthSpinePairCounterexampleBankLimits
lengthSpinePairCounterexampleBankLimits
    (LengthSpinePairCounterexampleBank _ (BankKernel limits _ _)) =
  LengthSpinePairCounterexampleBankLimits limits

lengthSpinePairCounterexampleBankSamples
  :: LengthSpinePairCounterexampleBank identity
  -> [LengthSpinePairCounterexampleBankSample]
lengthSpinePairCounterexampleBankSamples
    (LengthSpinePairCounterexampleBank _ (BankKernel _ samples _)) =
  map LengthSpinePairCounterexampleBankSample samples

lengthSpinePairCounterexampleBankStats
  :: LengthSpinePairCounterexampleBank identity
  -> LengthSpinePairCounterexampleBankStats
lengthSpinePairCounterexampleBankStats
    (LengthSpinePairCounterexampleBank _ (BankKernel _ _ stats)) =
  LengthSpinePairCounterexampleBankStats stats

insertLengthSpinePairCounterexampleBankSample
  :: LengthSpinePairCounterexampleBankOrigin
  -> [Natural]
  -> LengthSpinePairCounterexampleBank identity
  -> Either
      LengthSpinePairCounterexampleBankError
      (LengthSpinePairCounterexampleBank identity)
insertLengthSpinePairCounterexampleBankSample
    ~(LengthSpinePairCounterexampleBankOrigin origin) inputs
    (LengthSpinePairCounterexampleBank scope kernel) =
  LengthSpinePairCounterexampleBank scope
    <$> either (Left . mapSpinePairBankError) Right
      (insertBankKernelSample origin inputs kernel)

recordLengthSpinePairCounterexampleBankReplayAttempt
  :: LengthSpinePairCounterexampleBank identity
  -> Either
      LengthSpinePairCounterexampleBankError
      (LengthSpinePairCounterexampleBank identity)
recordLengthSpinePairCounterexampleBankReplayAttempt
    (LengthSpinePairCounterexampleBank scope kernel) =
  LengthSpinePairCounterexampleBank scope
    <$> either (Left . mapSpinePairBankError) Right
      (recordBankKernelReplayAttempt kernel)

mapSpinePairBankError
  :: BankKernelError -> LengthSpinePairCounterexampleBankError
mapSpinePairBankError source = case source of
  BankKernelEntryLimitExceeded limit observed ->
    LengthSpinePairCounterexampleBankEntryLimitExceeded limit observed
  BankKernelSampleWidthLimitExceeded limit observed ->
    LengthSpinePairCounterexampleBankSampleWidthLimitExceeded limit observed
  BankKernelNaturalBitLimitExceeded index limit observed ->
    LengthSpinePairCounterexampleBankNaturalBitLimitExceeded
      index limit observed
  BankKernelSampleEncodedByteLimitExceeded limit observed ->
    LengthSpinePairCounterexampleBankSampleEncodedByteLimitExceeded
      limit observed
  BankKernelReplayAttemptLimitExceeded limit observed ->
    LengthSpinePairCounterexampleBankReplayAttemptLimitExceeded
      limit observed
