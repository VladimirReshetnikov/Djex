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
  , QueryResult (..)
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

import Language.Haskell.Synthesis.Constraint (Constraint)
import Language.Haskell.Synthesis.Name (Name)
import Language.Haskell.Synthesis.Search (SearchBatch)

-- | One synthesis request, parameterized by a backend's goal type and search
-- options.
--
-- Contexts use the same goal-type representation as the requested type.  The
-- target is a validated structural 'Name'; a backend may impose a narrower
-- namespace when lowering the request (for example, an unqualified function
-- name for a generated top-level clause).
data QueryRequest ty options = QueryRequest
  { requestTarget :: Name
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
