-- | Checked contracts for the @finite-list-spine-length/v1@ dialect and its
-- distinct @finite-binary-product-spine-lengths/v1@ sibling.
--
-- This module describes only total finite list-spine lengths over unbounded
-- natural numbers.  The sibling domain admits an exact boxed binary product
-- whose two source-ordered result fields are modeled spines; its
-- postcondition can relate either result component to the compact modeled
-- inputs.  The pair carrier and syntax grant no evidence authority by
-- themselves, and scalar contracts, identities, and public APIs remain
-- unchanged.
--
-- List payloads remain opaque.  Provider summaries are
-- explicit assumptions for search guidance; they are not behavioral evidence.
-- The historical context-free source remains directly usable under that
-- assumption.  The additive conditional source retains one exact closed
-- scheme with a nonempty leading constraint context and assumes its provider
-- law is uniform over independently admitted dictionary evidence.  Standalone
-- provider evaluation still cannot use it.  The Length problem boundary may
-- use it only at the final node of its own exact associated certificate chain
-- after every alias-free, forall-free ground obligation has been discharged
-- against the session's exact inventory without query givens.  Z3 is never
-- dictionary authority.
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
-- Product contracts reuse that exact checked spine, target-role, and provider
-- authority through a structurally wrapped product-inventory identity rather
-- than a representational cast from scalar evidence.
module Language.Haskell.Synthesis.Semantic.Length
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
  ) where

import Language.Haskell.Synthesis.Internal.Semantic.Length
