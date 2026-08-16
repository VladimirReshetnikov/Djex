-- | Candidate-independent, bounded replay-input banks for the scalar and
-- binary-product finite-spine Length domains.
--
-- A bank scope is sealed only by the Length problem constructor while its
-- exact checked session, revalidated checked contract, and normalized target
-- coexist.  Its collision-free identity includes the complete session
-- inventory/provider-law basis, solver-neutral interpretation policy, exact
-- contract, and normalized target.  Candidate graphs, interpreted results,
-- counterexample conditions, candidate-used provider subsets, SMT queries,
-- execution identities, preferences, and receipts are deliberately absent.
--
-- Samples contain source-ordered natural inputs and coarse provenance only.
-- They never contain a verdict or evidence receipt: every later use must
-- independently replay the vector against the current checked problem.
-- Scalar and product scopes, samples, limits, origins, statistics, and banks
-- are nominally distinct even though their bounded kernels have the same
-- policy.
module Language.Haskell.Synthesis.Semantic.Length.CounterexampleBank
  ( LengthCounterexampleBankScopeFingerprintSubject
  , LengthCounterexampleBankTargetFingerprintSubject
  , lengthCounterexampleBankScopeSchemaTag
  , LengthCounterexampleBankScope
  , lengthCounterexampleBankScopeFingerprint
  , lengthCounterexampleBankScopeTargetFingerprint
  , LengthCounterexampleBankLimits
  , LengthCounterexampleBankLimitField (..)
  , LengthCounterexampleBankLimitError (..)
  , mkLengthCounterexampleBankLimits
  , defaultLengthCounterexampleBankLimits
  , lengthCounterexampleBankEntryLimit
  , lengthCounterexampleBankSampleWidthLimit
  , lengthCounterexampleBankNaturalBitLimit
  , lengthCounterexampleBankEncodedByteLimit
  , lengthCounterexampleBankReplayAttemptLimit
  , LengthCounterexampleBankOrigin
  , lengthCounterexampleBankLiveModelReplayOrigin
  , lengthCounterexampleBankSolverIndependentReplayOrigin
  , lengthCounterexampleBankSimplificationReplayOrigin
  , LengthCounterexampleBankSample
  , lengthCounterexampleBankSampleInputs
  , lengthCounterexampleBankSampleOrigin
  , lengthCounterexampleBankSampleEncodedByteCount
  , LengthCounterexampleBankStats
  , lengthCounterexampleBankStatsRetainedEntryCount
  , lengthCounterexampleBankStatsRetainedEncodedByteCount
  , lengthCounterexampleBankStatsRecordedSampleCount
  , lengthCounterexampleBankStatsDuplicatePromotionCount
  , lengthCounterexampleBankStatsEvictedSampleCount
  , lengthCounterexampleBankStatsReplayAttemptCount
  , LengthCounterexampleBankError (..)
  , LengthCounterexampleBank
  , emptyLengthCounterexampleBank
  , lengthCounterexampleBankScope
  , lengthCounterexampleBankMatchesScope
  , lengthCounterexampleBankLimits
  , lengthCounterexampleBankSamples
  , lengthCounterexampleBankStats
  , insertLengthCounterexampleBankSample
  , recordLengthCounterexampleBankReplayAttempt
  , LengthSpinePairCounterexampleBankScopeFingerprintSubject
  , LengthSpinePairCounterexampleBankTargetFingerprintSubject
  , lengthSpinePairCounterexampleBankScopeSchemaTag
  , LengthSpinePairCounterexampleBankScope
  , lengthSpinePairCounterexampleBankScopeFingerprint
  , lengthSpinePairCounterexampleBankScopeTargetFingerprint
  , LengthSpinePairCounterexampleBankLimits
  , LengthSpinePairCounterexampleBankLimitField (..)
  , LengthSpinePairCounterexampleBankLimitError (..)
  , mkLengthSpinePairCounterexampleBankLimits
  , defaultLengthSpinePairCounterexampleBankLimits
  , lengthSpinePairCounterexampleBankEntryLimit
  , lengthSpinePairCounterexampleBankSampleWidthLimit
  , lengthSpinePairCounterexampleBankNaturalBitLimit
  , lengthSpinePairCounterexampleBankEncodedByteLimit
  , lengthSpinePairCounterexampleBankReplayAttemptLimit
  , LengthSpinePairCounterexampleBankOrigin
  , lengthSpinePairCounterexampleBankLiveModelReplayOrigin
  , lengthSpinePairCounterexampleBankSolverIndependentReplayOrigin
  , lengthSpinePairCounterexampleBankSimplificationReplayOrigin
  , LengthSpinePairCounterexampleBankSample
  , lengthSpinePairCounterexampleBankSampleInputs
  , lengthSpinePairCounterexampleBankSampleOrigin
  , lengthSpinePairCounterexampleBankSampleEncodedByteCount
  , LengthSpinePairCounterexampleBankStats
  , lengthSpinePairCounterexampleBankStatsRetainedEntryCount
  , lengthSpinePairCounterexampleBankStatsRetainedEncodedByteCount
  , lengthSpinePairCounterexampleBankStatsRecordedSampleCount
  , lengthSpinePairCounterexampleBankStatsDuplicatePromotionCount
  , lengthSpinePairCounterexampleBankStatsEvictedSampleCount
  , lengthSpinePairCounterexampleBankStatsReplayAttemptCount
  , LengthSpinePairCounterexampleBankError (..)
  , LengthSpinePairCounterexampleBank
  , emptyLengthSpinePairCounterexampleBank
  , lengthSpinePairCounterexampleBankScope
  , lengthSpinePairCounterexampleBankMatchesScope
  , lengthSpinePairCounterexampleBankLimits
  , lengthSpinePairCounterexampleBankSamples
  , lengthSpinePairCounterexampleBankStats
  , insertLengthSpinePairCounterexampleBankSample
  , recordLengthSpinePairCounterexampleBankReplayAttempt
  ) where

import Language.Haskell.Synthesis.Internal.Semantic.Length.CounterexampleBank
