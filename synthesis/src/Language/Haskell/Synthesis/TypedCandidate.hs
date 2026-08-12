-- | Opaque associations between checked compatibility candidates and typed
-- term graphs.
--
-- Engine adapters retain a typed graph before erasing to their historical
-- candidate syntax.  This module lets consumers choose the richer path when
-- available and fall back explicitly when it is not, without permitting a
-- caller to mint a forged checked association between a graph and another
-- compatibility candidate.
module Language.Haskell.Synthesis.TypedCandidate
  ( TypedCandidate
  , typedCandidateCompatibility
  , typedQueryResultCompatibility
  , typedCandidateTermGraph
  ) where

import Language.Haskell.Synthesis.Internal.TypedCandidate
  ( TypedCandidate
  , typedCandidateCompatibility
  , typedQueryResultCompatibility
  , typedCandidateTermGraph
  )
