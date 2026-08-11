-- | Canonical QF_LIA queries and independently replayed input models for the
-- checked finite-list-spine Length domain.
--
-- A query can be built only from one opaque 'CheckedLengthProblem'.  It
-- retains a typed, versioned translation and emits bounded canonical SMT-LIB
-- commands, but does not launch Z3 or assign authority to raw @sat@, @unsat@,
-- or @unknown@ reports.  Solver build and protocol identity belong to a later
-- execution envelope layered over the query fingerprint.
--
-- Model bindings contain inputs only.  'validateLengthSMTLibCounterexample'
-- reorders an exact symbol set, rejects negative integers, recomputes the
-- candidate result from the retained problem, and delegates to independent
-- concrete replay.  Any resulting evidence remains finite-spine/model-relative
-- and explicitly conditional on the provider laws recorded by its receipt.
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
  ) where

import Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib
