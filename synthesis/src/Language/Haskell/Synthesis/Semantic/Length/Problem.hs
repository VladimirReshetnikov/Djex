-- | Atomic sessions and nominally separate scalar and binary-product
-- behavioral problems for finite list-spine length.
--
-- The first checked layer binds one exact annotation-erased neutral inventory,
-- finite-spine model, normalized provider-law table, and closed interpretation
-- policy.  Inventory identity remains distinct from the solver-neutral
-- encoding policy.  The unified contract and problem entrances derive their
-- authority from that opaque association; compatibility wrappers retain their
-- historical projected-association behavior and signatures.  Encoding-policy
-- versions advance when their common candidate trust boundary changes, so
-- callers must treat the collision-free bytes as versioned identities rather
-- than as a permanently frozen wire format.
--
-- When a retained provider law has a nonempty context, the session privately
-- attempts to seal the restricted ground class resolver from that same
-- inventory.  An unavailable resolver is retained fail-closed and reported as
-- 'LengthAssociatedClassResolverUnavailable' if a candidate needs it.  One
-- associated certificate row may use the law only after every source-ordered
-- obligation is independently discharged, its complete certified function
-- prefix passes the occurrence-isolation audit, and interpretation reaches the
-- row's final visible-application node.  The base node remains a sentinel.
-- The new associated discharge and protected-chain failures expose only
-- canonical row/step/obligation positions and sanitized reasons; resolver
-- receipts, constraint/type payloads, and protected graph coordinates remain
-- private.  No query givens or Z3 evidence participate.
--
-- The additive product entrances accept only a final boxed two-field tuple and
-- force its modeled-spine fields in source order.  They reuse the session's
-- scalar provider and exact spine-case mechanisms inside those fields, but
-- grant no product-valued provider or case authority.  Product inventory,
-- contract, candidate, encoding, and complete-problem identities are distinct;
-- the product inventory structurally wraps the exact scalar session inventory
-- rather than coercing its nominal evidence.
module Language.Haskell.Synthesis.Semantic.Length.Problem
  ( LengthSemanticFingerprintPart (..)
  , LengthEncodingPolicyFingerprintSubject
  , LengthSessionError (..)
  , LengthInterpretationPolicySource (..)
  , CheckedLengthInterpretationPolicy
  , CheckedLengthSession
  , sealLengthSessionWithInterpretationPolicy
  , sealLengthSession
  , sealRoleAwareLengthSession
  , sealExactSpineCaseLengthSession
  , checkedLengthSessionInterpretationPolicy
  , sealLengthContractInSession
  , sealLengthSpinePairContractInSession
  , checkedLengthSessionContext
  , checkedLengthSessionProviderInventory
  , lengthSessionInventoryFingerprint
  , lengthSessionEncodingPolicyFingerprint
  , LengthProblemLimits
  , LengthProblemLimitError (..)
  , mkLengthProblemLimits
  , defaultLengthProblemLimits
  , lengthProblemTermGraphLimits
  , lengthProblemGraphFingerprintByteLimit
  , lengthProblemEvaluationStepLimit
  , LengthProblemFingerprintPart (..)
  , LengthSpinePairProblemFingerprintPart (..)
  , LengthRootOpeningError (..)
  , LengthUnobservedTargetDemandSite (..)
  , LengthStepPayloadDemandSite (..)
  , LengthAssociatedConstraintDischargeReason (..)
  , LengthAssociatedProviderChainSite (..)
  , LengthAssociatedProviderChainReason (..)
  , LengthProblemError (..)
  , LengthSpinePairProblemError (..)
  , CheckedLengthCandidate
  , CheckedLengthProblem
  , sealLengthTypedCandidateProblem
  , sealRoleAwareLengthTypedCandidateProblem
  , sealExactSpineCaseLengthTypedCandidateProblem
  , sealLengthTypedCandidateProblemInSession
  , checkedLengthCandidateResult
  , checkedLengthCandidateUsedProviders
  , checkedLengthCandidateFingerprint
  , checkedLengthProblemCandidate
  , checkedLengthProblemInputCount
  , checkedLengthProblemPrecondition
  , checkedLengthProblemPostcondition
  , checkedLengthProblemCounterexampleCondition
  , checkedLengthProblemEncodingFingerprint
  , checkedLengthProblemBehavioralProblem
  , CheckedLengthSpinePairCandidate
  , CheckedLengthSpinePairProblem
  , sealLengthSpinePairTypedCandidateProblem
  , sealRoleAwareLengthSpinePairTypedCandidateProblem
  , sealExactSpineCaseLengthSpinePairTypedCandidateProblem
  , sealLengthSpinePairTypedCandidateProblemInSession
  , checkedLengthSpinePairCandidateResult
  , checkedLengthSpinePairCandidateUsedProviders
  , checkedLengthSpinePairCandidateFingerprint
  , checkedLengthSpinePairProblemCandidate
  , checkedLengthSpinePairProblemInputCount
  , checkedLengthSpinePairProblemPrecondition
  , checkedLengthSpinePairProblemPostcondition
  , checkedLengthSpinePairProblemCounterexampleCondition
  , checkedLengthSpinePairProblemEncodingFingerprint
  , checkedLengthSpinePairProblemBehavioralProblem
  ) where

import Language.Haskell.Synthesis.Internal.Semantic.Length.Problem
import Language.Haskell.Synthesis.Internal.Semantic.Length.Problem.Candidate
