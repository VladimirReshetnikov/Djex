{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Backend-neutral synthesis queries and their checked result envelope.
--
-- This module shares only language-facing query structure and operational
-- result shape.  Goal types, search options, result metadata, and candidates
-- remain backend-owned parameters, so an adapter does not have to weaken its
-- type system, search policy, logical claims, or candidate representation to
-- participate in a common session API.
module Language.Haskell.Synthesis.Query
  ( QueryRequest (..)
  , QueryEvidence (..)
  , QueryResult
  , resultEvidence
  , resultSearch
  , QueryResultInvariantError (..)
  , mkQueryResult
  , queryResultFromCandidates
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

import Language.Haskell.Synthesis.Constraint (Constraint)
import Language.Haskell.Synthesis.Generated (DefinitionName)
import Language.Haskell.Synthesis.Search
  ( SearchBatch
  , batchCandidates
  )

-- | One synthesis request, parameterized by a backend's goal type and search
-- options.
--
-- Contexts use the same goal-type representation as the requested type.  The
-- target is already checked for the shared generated-definition namespace:
-- an unqualified variable identifier or operator other than the wildcard.
-- Backends therefore cannot disagree about target validity after accepting
-- the same request.
data QueryRequest ty options = QueryRequest
  { requestTarget :: DefinitionName
  , requestGoal :: ty
  , requestContexts :: [Constraint ty]
  , requestOptions :: options
  }
  deriving (Eq, Ord, Show, Generic)

instance (NFData ty, NFData options) =>
    NFData (QueryRequest ty options)

-- | Logical evidence established by a backend, kept separate from operational
-- search completion in the accompanying 'SearchBatch'.
--
-- In particular, a truncated search may still contain validated candidates,
-- while an operationally finished heuristic search need not constitute a
-- proof of uninhabitability.
data QueryEvidence
  = ValidatedCandidates
    -- ^ The result contains one or more independently validated candidates.
  | ProvedUninhabitable
    -- ^ The backend established that no admissible inhabitant exists.
  | RequiresTargetReference
    -- ^ No admissible candidate exists, but using the target itself would
    -- produce one; rendering that use would introduce general recursion.
  | NoEvidence
    -- ^ The search established no backend-independent logical conclusion.
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData QueryEvidence

-- | A mismatch between a result's logical claim and its candidate payload.
--
-- Every returned candidate has crossed a backend's validation boundary, so a
-- nonempty batch must carry 'ValidatedCandidates'.  Conversely, that evidence
-- is meaningful only when the same result actually retains a candidate.
data QueryResultInvariantError
  = EmptyValidatedCandidates
    -- ^ 'ValidatedCandidates' was paired with an empty candidate batch.
  | CandidatesWithoutValidatedEvidence QueryEvidence
    -- ^ A nonempty candidate batch was paired with the recorded evidence.
  deriving (Eq, Ord, Show, Generic)

instance NFData QueryResultInvariantError

-- | A checked query result with backend-owned metadata and candidates.
--
-- The candidate parameter is last so 'fmap', 'foldMap', and 'traverse' operate
-- on candidates without disturbing logical evidence, progress, or metadata.
data QueryResult metadata candidate = QueryResult
  { resultEvidence :: QueryEvidence
  , resultSearch :: SearchBatch metadata candidate
  }
  deriving
    ( Eq
    , Ord
    , Show
    , Functor
    , Foldable
    , Traversable
    , Generic
    )

instance (NFData metadata, NFData candidate) =>
    NFData (QueryResult metadata candidate)

-- | Construct a result only when its evidence agrees with whether the batch
-- contains candidates.
--
-- This inspects only the outer constructor of the candidate list.  In
-- particular, accepting a nonempty batch does not force its tail, preserving
-- incremental backend traces and streaming candidate payloads.
mkQueryResult
  :: QueryEvidence
  -> SearchBatch metadata candidate
  -> Either QueryResultInvariantError (QueryResult metadata candidate)
mkQueryResult evidence search = case batchCandidates search of
  []
    | evidence == ValidatedCandidates -> Left EmptyValidatedCandidates
    | otherwise -> Right $ QueryResult evidence search
  _ : _
    | evidence == ValidatedCandidates -> Right $ QueryResult evidence search
    | otherwise -> Left $ CandidatesWithoutValidatedEvidence evidence

-- | Construct the ordinary heuristic-search result: a nonempty batch records
-- validated candidates, while an empty batch makes no logical claim.
--
-- Like 'mkQueryResult', this observes only whether the list is empty.
queryResultFromCandidates
  :: SearchBatch metadata candidate
  -> QueryResult metadata candidate
queryResultFromCandidates search = QueryResult evidence search
 where
  evidence = case batchCandidates search of
    [] -> NoEvidence
    _ : _ -> ValidatedCandidates
