-- | Opaque, stable structural fingerprints.
--
-- A 'Fingerprint' contains the complete versioned canonical encoding of an
-- identity, not a lossy hash.  Its nominal subject parameter keeps distinct
-- identity domains separate at compile time.  Construction is package-private,
-- so a public caller cannot manufacture a claimed semantic identity from
-- arbitrary bytes; domain modules expose checked constructors for their own
-- identities.
module Language.Haskell.Synthesis.Fingerprint
  ( Fingerprint
  , fingerprintCanonicalBytes
  , fingerprintCode
  ) where

import Language.Haskell.Synthesis.Internal.Fingerprint
  ( Fingerprint
  , fingerprintCanonicalBytes
  , fingerprintCode
  )
