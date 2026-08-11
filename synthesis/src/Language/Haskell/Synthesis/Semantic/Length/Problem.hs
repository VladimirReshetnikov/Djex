-- | Atomic sessions and behavioral problems for finite list-spine length.
--
-- The first checked layer binds one exact annotation-erased neutral inventory,
-- finite-spine model, and normalized provider-law table.  Inventory identity
-- remains distinct from the solver-neutral encoding policy.  Candidate and
-- complete problem sealing build on this opaque association; callers cannot
-- combine a context checked from one inventory with providers checked from
-- another.
module Language.Haskell.Synthesis.Semantic.Length.Problem
  ( LengthSemanticFingerprintPart (..)
  , LengthEncodingPolicyFingerprintSubject
  , LengthSessionError (..)
  , CheckedLengthSession
  , sealLengthSession
  , checkedLengthSessionContext
  , checkedLengthSessionProviderInventory
  , lengthSessionInventoryFingerprint
  , lengthSessionEncodingPolicyFingerprint
  ) where

import Language.Haskell.Synthesis.Internal.Semantic.Length.Problem
