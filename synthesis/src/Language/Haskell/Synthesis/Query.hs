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
  , ProviderInstantiationCandidate (..)
  , maximumProviderInstantiationCandidates
  , ProviderInstantiationAssignment (..)
  , KindedProviderInstantiationAssignment (..)
  , maximumProviderInstantiationAssignments
  , maximumProviderInstantiationArguments
  , maximumProviderInstantiationKindNodes
  , validateRequestTarget
  , RequestTypeSite (..)
  , requestTypeSiteLabel
  , traverseRequestTypes
  , traverseRequestContextsWithKnownArity
  , requestContextualType
  , requestContextVariablesNotInScope
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
import qualified Data.Set as Set
import GHC.Generics (Generic)

import Language.Haskell.Synthesis.Constraint
  ( Constraint
  , validateKnownConstraintArityWith
  )
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , SourceLocation
  , shownErrorDiagnostic
  , withSourceLocation
  )
import Language.Haskell.Synthesis.Generated
  ( DefinitionName
  , mkDefinitionName
  )
import Language.Haskell.Synthesis.Name
  ( Name
  , maximumTupleArity
  , renderCanonical
  )
import Language.Haskell.Synthesis.KindInference (GroundKind)
import Language.Haskell.Synthesis.Search
  ( SearchBatch
  , batchCandidates
  )
import Language.Haskell.Synthesis.Type (Type (ForallType))
import qualified Language.Haskell.Synthesis.Type as Type

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

-- | One externally established type choice for an exact named provider.
--
-- This is deliberately separate from t'QueryRequest': it describes evidence
-- available in the provider's originating environment rather than syntax or
-- policy intrinsic to the query. Backends validate the provider against the
-- sealed session, elaborate the candidate in that session's exact synonym and
-- kind scope, and keep the association provider-local during search.
--
-- The constructor remains public because compiler and theorem-prover
-- frontends commonly obtain this evidence from a richer source environment.
-- Constructing a value does not itself certify the claim; checked backend
-- runners are the trust boundary.
data ProviderInstantiationCandidate variable =
  ProviderInstantiationCandidate
    { providerInstantiationCandidateProvider :: Name
    , providerInstantiationCandidateType :: Type variable
    }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData variable =>
    NFData (ProviderInstantiationCandidate variable)

-- | Maximum number of externally supplied provider/type associations accepted
-- by one checked query execution.
--
-- Runners observe at most one list cell beyond this bound before entering any
-- candidate value. Thus a cyclic caller-built list is rejected finitely and
-- cannot reach search code whose fairness schedules require finite inputs.
maximumProviderInstantiationCandidates :: Int
maximumProviderInstantiationCandidates = 32

-- | One externally established complete leading-binder assignment for an
-- exact named provider.
--
-- Unlike 'ProviderInstantiationCandidate', whose type contributes to a
-- provider-local pool, this value retains the caller's argument order and
-- correlation. A richer frontend can therefore preserve the complete choice
-- established by one source-language instance head without asking a backend
-- to reconstruct it through a Cartesian product. Constructing the value is
-- still only an assertion; checked backend runners remain responsible for
-- session identity, arity, type, and finite-width validation.
data ProviderInstantiationAssignment variable =
  ProviderInstantiationAssignment
    { providerInstantiationAssignmentProvider :: Name
    , providerInstantiationAssignmentArguments :: [Type variable]
    }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData variable =>
    NFData (ProviderInstantiationAssignment variable)

-- | One externally established complete assignment whose exact leading-binder
-- ground kinds are also known to the caller.
--
-- The unkinded 'ProviderInstantiationAssignment' remains the compatibility
-- boundary: adapters infer every constrained position from the retained body
-- and default a vacuous position to @Type@. This richer form lets a frontend
-- preserve source kind facts for those otherwise unconstrained positions.
-- Checked runners still validate every supplied kind against all uses in the
-- exact provider body before elaborating the paired argument at that kind.
data KindedProviderInstantiationAssignment variable =
  KindedProviderInstantiationAssignment
    { kindedProviderInstantiationAssignmentProvider :: Name
    , kindedProviderInstantiationAssignmentArguments ::
        [(GroundKind, Type variable)]
    }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData variable =>
    NFData (KindedProviderInstantiationAssignment variable)

-- | Maximum number of complete provider assignments accepted by one checked
-- query execution.
--
-- As with the historical candidate bound, adapters must observe at most one
-- outer-list cell beyond this limit before entering an assignment value.
maximumProviderInstantiationAssignments :: Int
maximumProviderInstantiationAssignments = 32

-- | Maximum number of ordered leading-forall arguments in one complete
-- provider assignment.
--
-- Checked adapters must bound an argument-list spine before entering any
-- argument, so an over-wide or cyclic caller-built list fails finitely.
maximumProviderInstantiationArguments :: Int
maximumProviderInstantiationArguments = 4

-- | Maximum number of constructors in one caller-supplied provider kind.
--
-- A fully saturated 64-argument constructor has a right-associated kind tree
-- with @2 * 64 + 1@ nodes, so this admits the complete shared tuple arity while
-- bounding arbitrary higher-order shapes. Checked runners use a productive
-- node observer and therefore reject cyclic kind values without traversing
-- them indefinitely.
maximumProviderInstantiationKindNodes :: Int
maximumProviderInstantiationKindNodes = 2 * maximumTupleArity + 1

-- | Validate the shared generated-definition namespace and attach one
-- adapter-specific code and summary to the common failure detail. Keeping the
-- exact rejected spelling and
-- t'Language.Haskell.Synthesis.Generated.RenderError' together prevents
-- backend request boundaries from drifting in either accepted names or
-- diagnostic context.
validateRequestTarget
  :: String
  -> String
  -> Name
  -> Either Diagnostic DefinitionName
validateRequestTarget code summary target = case mkDefinitionName target of
  Right checked -> Right checked
  Left failure -> Left $ shownErrorDiagnostic code summary
    (renderCanonical target, failure)

-- | The role of one type visited inside a t'QueryRequest'.
--
-- A backend can use this distinction to retain goal-versus-context diagnostic
-- wording while sharing the envelope's traversal order.
data RequestTypeSite
  = RequestGoal
  | RequestContextArgument
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData RequestTypeSite

-- | Stable diagnostic wording for each shared request-type role.
requestTypeSiteLabel :: RequestTypeSite -> String
requestTypeSiteLabel RequestGoal = "goal"
requestTypeSiteLabel RequestContextArgument = "context"

-- | Traverse the goal followed by every explicit context argument in source
-- order, without changing the checked target or search options. The second
-- callback can validate or normalize each complete context after its
-- arguments and before the next context is inspected.
--
-- Owning this order in the neutral envelope keeps adapters from independently
-- rebuilding requests during normalization. In particular, ordinary
-- left-biased monads such as 'Either' observe goal failures before the
-- context list, and a context-level failure before any later context.
--
-- This general traversal assumes that every context argument spine is
-- finite. Stable request constructors receiving caller-built lazy values
-- should defer those spines until an environment can use
-- 'traverseRequestContextsWithKnownArity' instead.
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

-- | Traverse context arguments only after checking every class whose arity
-- is known to the supplied environment.
--
-- A known argument spine is observed through at most one cell beyond its
-- declared width before any element is entered. This makes a session's class
-- table the structural bound for cyclic and over-applied caller-built lists;
-- no library-wide maximum class arity is imposed. Unknown classes retain the
-- caller's existing open-world policy and therefore require an ordinary
-- finite argument spine.
--
-- Contexts and arguments retain source order. The complete-context callback
-- runs after its arguments and before the next context, matching
-- 'traverseRequestTypes'.
traverseRequestContextsWithKnownArity
  :: (Name -> Maybe Int)
  -> (Name -> Int -> Int -> error)
  -> (RequestTypeSite -> source -> Either error target)
  -> (Constraint target -> Either error (Constraint target))
  -> [Constraint source]
  -> Either error [Constraint target]
traverseRequestContextsWithKnownArity lookupArity arityFailure transform
    finishContext = traverse traverseContext
 where
  traverseContext context = do
    validateKnownConstraintArityWith lookupArity arityFailure context
    traverse (transform RequestContextArgument) context >>= finishContext

-- | Whether a checked request came from an in-memory API value or a complete
-- source location.
--
-- Provenance affects only diagnostics. It is deliberately separate from
-- t'QueryRequest', whose equality and display describe synthesis semantics.
-- The source constructor is strict so a t'SourceLocation' derived from a text
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
-- source buffer behind a lazy t'CachedQuery'.
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

-- | Explicit-context variables that are outside the query goal's lexical
-- scope, in deterministic identity order.
--
-- A variable is available when it occurs free anywhere in the supplied goal
-- or is bound by its complete leading prenex chain. A binder below an arrow,
-- tuple, or application is not visible to the separate context list. Existing
-- contexts embedded in the goal remain part of that goal; this operation
-- validates only the separately supplied 'requestContexts'.
requestContextVariablesNotInScope
  :: Ord variable
  => QueryRequest (Type variable) options
  -> [variable]
requestContextVariablesNotInScope request = Set.toAscList
  $ contextVariables Set.\\ inScopeVariables
 where
  contextVariables = foldMap Type.constraintFreeVariables
    $ requestContexts request
  inScopeVariables = Type.freeVariables (requestGoal request)
    `Set.union` Set.fromList
      (Type.leadingForallVariables $ requestGoal request)

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
-- | Recover the backend-independent logical claim attached to a result.
resultEvidence :: QueryResult metadata candidate -> QueryEvidence
resultEvidence (QueryResult evidence _) = evidence

-- | Recover operational progress, metadata, and candidates.
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
