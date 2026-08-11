-- | Canonical structural identity for checked typed term graphs.
--
-- This module deliberately identifies only the shared typed graph.  It does
-- not claim that globals belong to a particular inventory, that holes or
-- residual obligations are absent, or that a behavioral interpretation is
-- sound.  Domain-specific session sealers must establish those properties
-- before embedding this identity in a problem or cache key.
--
-- The shared type checker has no constructor-family schema authority, so this
-- generic boundary also rejects constructor-pattern graphs.  A future
-- inventory-bound sealer may admit them only after checking their exact family
-- descriptor.  Certificate-bearing visible applications fail closed for the
-- same reason: allocation IDs alone are not semantic identities.
module Language.Haskell.Synthesis.TypedGenerated.Fingerprint
  ( TermGraphFingerprintSubject
  , defaultTermGraphFingerprintByteLimit
  , TermGraphFingerprintError (..)
  , fingerprintSharedTermGraph
  ) where

import Language.Haskell.Synthesis.Internal.TypedGenerated.Fingerprint
  ( TermGraphFingerprintError (..)
  , TermGraphFingerprintSubject
  , defaultTermGraphFingerprintByteLimit
  , fingerprintSharedTermGraph
  )
