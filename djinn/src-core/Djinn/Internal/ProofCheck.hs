--
-- | Stable compatibility facade for independent LJT proof checking.
--
-- The Cabal-private implementation retains a checker-owned typed proof tree
-- for future Djinn typed-candidate emission.  This exposed module deliberately
-- keeps the historical surface and result type unchanged.
module Djinn.Internal.ProofCheck
    ( checkProofEnvironment
    , checkProof
    ) where

import Djinn.Internal.LJTFormula (Formula, Symbol, Term)
import qualified Djinn.Internal.ProofCheck.Evidence as Evidence

-- | Reject duplicate external proof identities exactly as before.
checkProofEnvironment :: [(Symbol, Formula)] -> Either String ()
checkProofEnvironment = Evidence.checkProofEnvironment

-- | Check one proof while discarding the private typed evidence.
--
-- This is intentionally an exact projection of the one authoritative checker
-- path: errors and success are not recomputed by a compatibility-only pass.
checkProof :: [(Symbol, Formula)] -> Formula -> Term -> Either String ()
checkProof environment expected term =
    () <$ Evidence.checkProofWithEvidence environment expected term
