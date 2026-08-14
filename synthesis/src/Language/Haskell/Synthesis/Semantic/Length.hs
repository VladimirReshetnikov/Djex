-- | Checked contracts for the @finite-list-spine-length/v1@ semantic dialect.
--
-- This module describes only total finite list-spine lengths over unbounded
-- natural numbers.  List payloads remain opaque.  Provider summaries are
-- explicit assumptions for search guidance; they are not behavioral evidence.
-- The historical context-free source remains directly usable under that
-- assumption.  The additive conditional source retains one exact closed
-- scheme with a nonempty leading constraint context, but neither candidate
-- interpretation nor standalone provider evaluation may use it until a later
-- candidate-local discharge authority is added.  Length does not consume or
-- associate checked class-resolution receipts here; Z3 is never dictionary
-- authority.
-- A checked context retains the exact opaque @Inventory@ that supplies kind
-- and declaration authority together with either the versioned built-in list
-- spine or an exactly named, structurally validated unary datatype spine.
-- Provider schemes, including retained contexts, are resolved from that
-- inventory rather than trusted from caller input.  A later
-- behavioral-problem constructor must still bind this
-- context to its exact typed target/candidate and encoding identities before
-- any observation may be replayed as evidence. The smaller checked contract
-- and provider-inventory values retain fingerprints rather than the complete
-- context. The atomic session therefore owns the checked provider
-- inventory directly, while a later problem boundary must revalidate any
-- separately supplied contract through that session's context.
module Language.Haskell.Synthesis.Semantic.Length
  ( FiniteListSpineLengthV1
  , LengthContractFingerprintSubject
  , LengthProviderInventoryFingerprintSubject
  , finiteListSpineLengthDomainTag
  , LengthExpression (..)
  , LengthFormula (..)
  , LengthContractVariable (..)
  , LengthContractSource (..)
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

import Language.Haskell.Synthesis.Internal.Semantic.Length
