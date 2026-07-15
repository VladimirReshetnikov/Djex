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
  , RequestTypeSite (..)
  , traverseRequestTypes
  , requestContextualType
  , RequestProvenance (..)
  , withRequestProvenance
  , CachedQuery
  , mkCachedQuery
  , mkCachedQueryWithProvenance
  , sealCachedQueryWithProvenance
  , cachedQueryRequest
  , cachedQueryProvenance
  , cachedQueryCache
  , withCachedQueryProvenance
  , QueryEvidence (..)
  , QueryResult
  , resultEvidence
  , resultSearch
  , QueryResultInvariantError (..)
  , mkQueryResult
  , queryResultFromCandidates
  ) where

import Control.DeepSeq (NFData (rnf))
import Data.Bifunctor (first)
import GHC.Generics (Generic)

import Language.Haskell.Synthesis.Constraint (Constraint)
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , SourceLocation
  , withSourceLocation
  )
import Language.Haskell.Synthesis.Generated (DefinitionName)
import Language.Haskell.Synthesis.Search
  ( SearchBatch
  , batchCandidates
  )
import Language.Haskell.Synthesis.Type (Type (ForallType))

-- | One synthesis request, parameterized by a backend's goal type and search
-- options.
--
-- Contexts use the same goal-type representation as the requested type.  The
-- target is already checked for the shared generated-definition namespace:
-- an unqualified variable identifier or operator other than the wildcard.
-- Backends therefore cannot disagree about target validity after accepting
-- the same request. For shared 'Type' goals, 'requestContextualType' defines
-- the structural scope of the separate context list.
data QueryRequest ty options = QueryRequest
  { requestTarget :: DefinitionName
  , requestGoal :: ty
  , requestContexts :: [Constraint ty]
  , requestOptions :: options
  }
  deriving (Eq, Ord, Show, Generic)

instance (NFData ty, NFData options) =>
    NFData (QueryRequest ty options)

-- | The role of one type visited inside a 'QueryRequest'.
--
-- A backend can use this distinction to retain goal-versus-context diagnostic
-- wording while sharing the envelope's traversal order.
data RequestTypeSite
  = RequestGoal
  | RequestContextArgument
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData RequestTypeSite

-- | Traverse the goal followed by every explicit context argument in source
-- order, without changing the checked target or search options. The second
-- callback can validate or normalize each complete context after its
-- arguments and before the next context is inspected.
--
-- Owning this order in the neutral envelope keeps adapters from independently
-- rebuilding requests during normalization. In particular, ordinary
-- left-biased monads such as 'Either' observe goal failures before the
-- context list, and a context-level failure before any later context.
traverseRequestTypes
  :: Monad effect
  => (RequestTypeSite -> source -> effect target)
  -> (Constraint target -> effect (Constraint target))
  -> QueryRequest source options
  -> effect (QueryRequest target options)
traverseRequestTypes transform finishContext request = QueryRequest
  (requestTarget request)
  <$> transform RequestGoal (requestGoal request)
  <*> traverse
      (\context -> traverse (transform RequestContextArgument) context
        >>= finishContext)
      (requestContexts request)
  <*> pure (requestOptions request)

-- | Whether a checked request came from an in-memory API value or a complete
-- source location.
--
-- Provenance affects only diagnostics. It is deliberately separate from
-- 'QueryRequest', whose equality and display describe synthesis semantics.
-- The source constructor is strict so a 'SourceLocation' derived from a text
-- buffer is materialized while the request is sealed.
data RequestProvenance
  = ProgrammaticRequest
  | SourceRequest !SourceLocation
  deriving (Eq, Ord, Show, Generic)

instance NFData RequestProvenance

-- | Attach source provenance to a diagnostic when the request came from text.
withRequestProvenance
  :: RequestProvenance
  -> Diagnostic
  -> Diagnostic
withRequestProvenance ProgrammaticRequest = id
withRequestProvenance (SourceRequest location) = withSourceLocation location

-- | A stable neutral request paired with an adapter-owned projection or
-- presentation cache.
--
-- The constructor is hidden so request, provenance, and cache cannot be
-- replaced independently after an adapter seals its own opaque request. Equality and
-- display deliberately observe only the neutral request: cached backend
-- values are implementation details and need not even
-- have 'Eq' or 'Show' instances.
data CachedQuery ty options cache = CachedQuery
  (QueryRequest ty options)
  !RequestProvenance
  cache

-- | Pair a neutral request with the exact cache derived from it.
mkCachedQuery
  :: QueryRequest ty options
  -> cache
  -> CachedQuery ty options cache
mkCachedQuery = mkCachedQueryWithProvenance ProgrammaticRequest

-- | Pair a neutral request with its exact provenance and derived cache.
--
-- Keeping provenance outside the backend cache gives every adapter the same
-- source-location lifetime and diagnostic contract without making it part of
-- semantic request equality.
mkCachedQueryWithProvenance
  :: RequestProvenance
  -> QueryRequest ty options
  -> cache
  -> CachedQuery ty options cache
mkCachedQueryWithProvenance provenance request =
  CachedQuery request provenance

-- | Validate and seal an adapter-owned request projection under one shared
-- provenance protocol.
--
-- The checked action returns the exact neutral request to publish together
-- with the private cache derived from it. Every failure receives source
-- provenance, while successful programmatic requests remain source-less.
-- The provenance is forced before inspecting the action so a sourced span is
-- materialized at the sealing boundary instead of retaining the caller's
-- source buffer behind a lazy 'CachedQuery'.
sealCachedQueryWithProvenance
  :: RequestProvenance
  -> Either Diagnostic (QueryRequest ty options, cache)
  -> Either Diagnostic (CachedQuery ty options cache)
sealCachedQueryWithProvenance provenance checked =
  provenance `seq` first (withRequestProvenance provenance) (do
    (request, cache) <- checked
    pure $ mkCachedQueryWithProvenance provenance request cache)

-- | Recover the stable neutral request.
cachedQueryRequest
  :: CachedQuery ty options cache
  -> QueryRequest ty options
cachedQueryRequest (CachedQuery request _ _) = request

-- | Recover diagnostic provenance without exposing the backend cache.
cachedQueryProvenance
  :: CachedQuery ty options cache
  -> RequestProvenance
cachedQueryProvenance (CachedQuery _ provenance _) = provenance

-- | Recover the adapter-owned cache.
cachedQueryCache :: CachedQuery ty options cache -> cache
cachedQueryCache (CachedQuery _ _ cache) = cache

-- | Attach this checked request's provenance to a diagnostic.
withCachedQueryProvenance
  :: CachedQuery ty options cache
  -> Diagnostic
  -> Diagnostic
withCachedQueryProvenance = withRequestProvenance . cachedQueryProvenance

instance (Eq ty, Eq options) => Eq (CachedQuery ty options cache) where
  left == right = cachedQueryRequest left == cachedQueryRequest right

instance (Show ty, Show options) =>
    Show (CachedQuery ty options cache) where
  showsPrec precedence = showsPrec precedence . cachedQueryRequest

-- | Combine a shared source-type goal with its explicit request contexts.
--
-- Contexts belong beneath the complete leading quantifier chain: each
-- leading binder is in scope for them, whereas a quantifier below an arrow,
-- tuple, or application is not. Existing contexts retain their lexical
-- level and follow the explicit request contexts at the insertion point.
-- A context-free request returns its goal unchanged. This operation is purely
-- structural: adapters remain responsible for validating whether every
-- context variable is admissible for their backend.
requestContextualType
  :: QueryRequest (Type variable) options
  -> Type variable
requestContextualType request
  | null contexts = requestGoal request
  | otherwise = insertUnderLeadingForalls $ requestGoal request
 where
  contexts = requestContexts request

  insertUnderLeadingForalls (ForallType variables embedded body)
    | ForallType{} <- body = ForallType variables embedded
        $ insertUnderLeadingForalls body
    | otherwise = ForallType variables (contexts ++ embedded) body
  insertUnderLeadingForalls goal = ForallType [] contexts goal

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
  QueryEvidence
  (SearchBatch metadata candidate)
  deriving
    ( Eq
    , Ord
    , Show
    , Functor
    , Foldable
    , Traversable
    )

instance (NFData metadata, NFData candidate) =>
    NFData (QueryResult metadata candidate) where
  rnf (QueryResult evidence search) = rnf evidence `seq` rnf search

-- These are ordinary projections rather than record fields. Otherwise a
-- downstream record update could bypass 'mkQueryResult' even though the
-- 'QueryResult' constructor is hidden.
resultEvidence :: QueryResult metadata candidate -> QueryEvidence
resultEvidence (QueryResult evidence _) = evidence

resultSearch
  :: QueryResult metadata candidate
  -> SearchBatch metadata candidate
resultSearch (QueryResult _ search) = search

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
