-- | Opaque associations between checked compatibility candidates and typed
-- term graphs.
--
-- Engine adapters retain a typed graph before erasing to their historical
-- candidate syntax.  This module lets consumers choose the richer path when
-- available and fall back explicitly when it is not, without permitting a
-- graph to be detached from the compatibility candidate whose erasure it was
-- checked against.
module Language.Haskell.Synthesis.TypedCandidate
  ( TypedCandidate
  , typedCandidateCompatibility
  , typedCandidateTermGraph
  ) where

import Language.Haskell.Synthesis.Internal.TypedCandidate
  ( TypedCandidate
  , typedCandidateCompatibility
  , typedCandidateTermGraph
  )
