-- | Canonical QF_LIA queries and independently replayed input assignments for
-- the checked scalar and binary-product finite-list-spine Length domains.
--
-- A scalar query can be built only from one opaque 'CheckedLengthProblem'; its
-- binary-product sibling requires one opaque 'CheckedLengthSpinePairProblem'.
-- Each structurally fingerprints a typed, versioned translation before
-- retaining only its checked problem, bounded canonical check bytes, and
-- complete fingerprint needed by execution and replay. Exact input symbols
-- and the optional canonical @get-value@ bytes are rederived from the
-- problem's sealed arity; both were already bounded and structurally
-- fingerprinted during the seal. It does not launch Z3 or assign authority to
-- raw @sat@, @unsat@, or
-- @unknown@ reports.
-- Solver build and protocol identity belong to the separate live execution
-- envelope layered over the query fingerprint.
--
-- Model bindings contain inputs only.  'validateLengthSMTLibCounterexample'
-- reorders an exact symbol set, rejects negative integers, recomputes the
-- candidate result from the retained problem, and delegates to independent
-- concrete replay. Callers which already hold source-ordered natural inputs
-- can instead pass only those values to
-- 'replayLengthSMTLibCounterexampleInputs'; the sealed query owns their checked
-- problem and symbol association. Every call evaluates afresh and returns a
-- fresh receipt after exact same-query/problem association or 'Nothing', never
-- a cached verdict. The @LengthSpinePairSMTLib@ entrances provide the same
-- boundaries while producing only nominal product-domain evidence.
-- 'probeLengthSMTLibCounterexampleAtOrigin' is the canonical
-- query-owned specialization for the all-zero vector: the caller supplies no
-- arity, symbols, or assignment. 'validateLengthSMTLibQueryInputBox' similarly
-- uses the query only as exact association authority while the
-- solver-independent evaluator exhausts an explicitly finite Cartesian box.
-- It returns either the first independently replayed counterexample or a
-- bounded positive receipt and never upgrades an external @unsat@ result. Any
-- resulting evidence remains finite-spine/model-relative and explicitly
-- conditional on the provider laws recorded by its receipt. None of these
-- validation or replay entrances gives authority to a raw solver status,
-- including @unsat@.
module Language.Haskell.Synthesis.Semantic.Length.SMTLib
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

import Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib
