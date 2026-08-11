-- | Checked contracts for the @finite-list-spine-length/v1@ semantic dialect.
--
-- This module describes only total finite list-spine lengths over unbounded
-- natural numbers.  List payloads remain opaque.  Provider summaries are
-- explicit assumptions for search guidance; they are not behavioral evidence.
-- Both sealing operations require the exact opaque @Inventory@ that supplies
-- kind authority, so an ill-kinded list payload or provider scheme cannot be
-- labeled checked.  The resulting values intentionally do not retain or
-- fingerprint that source inventory: a later behavioral-problem constructor
-- must bind its exact inventory, typed target/candidate, and encoding identities
-- before any observation may be replayed.
module Language.Haskell.Synthesis.Semantic.Length
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
  , LengthTypeCollectionSite (..)
  , LengthTypeBoundError (..)
  , LengthSyntaxCollectionSite (..)
  , LengthSyntaxError (..)
  , LengthContractError (..)
  , LengthProviderSummaryError (..)
  , LengthProviderInventoryError (..)
  , CheckedLengthContract
  , sealLengthContract
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
  , checkedLengthProviderSummaries
  , lookupCheckedLengthProviderSummary
  , lengthProviderInventoryFingerprint
  ) where

import Language.Haskell.Synthesis.Internal.Semantic.Length
