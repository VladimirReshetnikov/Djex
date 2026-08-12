{-# LANGUAGE RoleAnnotations #-}

-- | Representation of the checked typed-candidate association.
--
-- This module is Cabal-private.  Engine adapters may construct an association
-- only after independently checking both the compatibility candidate and its
-- typed graph result.  Public consumers receive the abstract type through
-- "Language.Haskell.Synthesis.TypedCandidate".
module Language.Haskell.Synthesis.Internal.TypedCandidate
  ( TypedCandidate
  , mkTypedCandidate
  , typedCandidateCompatibility
  , typedQueryResultCompatibility
  , typedCandidateTermGraph
  ) where

import Control.DeepSeq (NFData (rnf))

import Language.Haskell.Synthesis.Query (QueryResult)
import Language.Haskell.Synthesis.TypedGenerated (TermGraph)

-- | One compatibility candidate paired with the result of retaining its
-- checked typed graph.
--
-- Both payloads are deliberately lazy.  Inspecting the compatibility value
-- must not construct a graph, while asking for one graph must not inspect a
-- sibling candidate or a later search batch.  Nominal roles prevent
-- downstream 'coerce' calls from changing any identity or evidence domain
-- while retaining the checked association.
data TypedCandidate failure ty local candidate = TypedCandidate
  candidate
  (Either failure (TermGraph ty local))
  deriving (Eq, Ord, Show)

type role TypedCandidate nominal nominal nominal nominal

instance
    ( NFData failure
    , NFData ty
    , NFData local
    , NFData candidate
    ) => NFData (TypedCandidate failure ty local candidate) where
  rnf (TypedCandidate compatibility graph) =
    rnf compatibility `seq` rnf graph

-- | Package one engine-checked compatibility candidate with its exact graph
-- availability result.  Kept private so the two projections cannot be
-- replaced independently by a downstream caller.
mkTypedCandidate
  :: candidate
  -> Either failure (TermGraph ty local)
  -> TypedCandidate failure ty local candidate
mkTypedCandidate = TypedCandidate

-- | Recover the unchanged compatibility candidate.
typedCandidateCompatibility
  :: TypedCandidate failure ty local candidate
  -> candidate
typedCandidateCompatibility (TypedCandidate compatibility _) = compatibility

-- | Erase typed-candidate retention from one checked query result without
-- revalidating or changing its envelope.  The result's logical evidence,
-- operational progress, metadata, candidate cardinality, and ordering are
-- therefore unchanged.  Graph availability remains unobserved and candidate
-- tails stay lazy.
typedQueryResultCompatibility
  :: QueryResult metadata (TypedCandidate failure ty local candidate)
  -> QueryResult metadata candidate
typedQueryResultCompatibility = fmap typedCandidateCompatibility

-- | Recover either the explicit reason typed retention was unavailable or the
-- independently sealed typed graph.
typedCandidateTermGraph
  :: TypedCandidate failure ty local candidate
  -> Either failure (TermGraph ty local)
typedCandidateTermGraph (TypedCandidate _ graph) = graph
