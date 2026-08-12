{-# LANGUAGE RoleAnnotations #-}

-- | Cabal-private associations retained between Djinn proof checking and the
-- historical generated-candidate projection.
--
-- The proof checker and generated converter deliberately run in two phases:
-- every raw proof is checked before any proof is converted, preserving the
-- established diagnostic precedence.  The opaque intermediate below keeps
-- each raw proof attached to the evidence produced by that exact check, so a
-- later conversion cannot accidentally zip clauses onto a different proof's
-- evidence.
module Djinn.Internal.CheckedCandidate
    ( CheckedCandidateProof
    , checkCandidateProofWith
    , ValidatedCandidate
    , convertCheckedCandidate
    , validatedCandidateOutput
    , validatedCandidateDetails
    , validatedCandidateProofEvidence
    , sortValidatedCandidates
    , ValidatedResult
    , mkValidatedResult
    , validatedResultCandidates
    , projectValidatedResultWith
    , projectValidatedResult
    ) where

import Data.List (sortOn)
import Numeric.Natural (Natural)

import Djinn.Internal.ProofCheck.Evidence (CheckedProofEvidence)
import qualified Language.Haskell.Synthesis.Candidate as SharedCandidate
import qualified Language.Haskell.Synthesis.Query as SharedQuery
import qualified Language.Haskell.Synthesis.Search as SharedSearch

-- | One raw proof paired with the evidence returned by checking that same
-- occurrence.  Both fields stay lazy: merely retaining or later discarding a
-- compatibility candidate must not traverse the checked proof tree.
data CheckedCandidateProof raw = CheckedCandidateProof
    raw
    CheckedProofEvidence

type role CheckedCandidateProof nominal

-- | Check one raw proof and seal its exact evidence beside it.
--
-- The checker is an argument so the association owns no formula-planning
-- policy.  The Djinn core supplies 'checkProofWithEvidence' specialized to
-- the exact environment and formula for the current search plan.
checkCandidateProofWith
    :: (raw -> Either failure CheckedProofEvidence)
    -> raw
    -> Either failure (CheckedCandidateProof raw)
checkCandidateProofWith check raw =
    CheckedCandidateProof raw <$> check raw

-- | One converted generated output retaining the evidence from its exact raw
-- proof.  Constructors and representation stay Cabal-private, with nominal
-- roles preventing representational coercions from changing any association.
data ValidatedCandidate details output = ValidatedCandidate
    output
    details
    CheckedProofEvidence

type role ValidatedCandidate nominal nominal

-- | Convert the exact raw proof retained by a successful check.
--
-- Candidate details remain lazy, matching the historical path: an
-- eta-equivalent duplicate can be discarded without computing its ranking
-- details, while a sorted query computes details only for surviving values.
convertCheckedCandidate
    :: (raw -> Either failure output)
    -> (output -> details)
    -> CheckedCandidateProof raw
    -> Either failure (ValidatedCandidate details output)
convertCheckedCandidate convert makeDetails
        (CheckedCandidateProof raw evidence) = do
    output <- convert raw
    return $ ValidatedCandidate output (makeDetails output) evidence

-- | Observe the converted generated output without entering proof evidence.
validatedCandidateOutput
    :: ValidatedCandidate details output
    -> output
validatedCandidateOutput (ValidatedCandidate output _ _) = output

-- | Observe the established candidate-ranking details.
validatedCandidateDetails
    :: ValidatedCandidate details output
    -> details
validatedCandidateDetails (ValidatedCandidate _ details _) = details

-- | Traverse the checker-owned evidence attached to this exact output.
validatedCandidateProofEvidence
    :: ValidatedCandidate details output
    -> CheckedProofEvidence
validatedCandidateProofEvidence (ValidatedCandidate _ _ evidence) = evidence

-- | Stable ranking of whole candidate/evidence associations.
--
-- 'sortOn' is stable, so equal details retain proof-search and formula-plan
-- order exactly as in the historical public-candidate sort.
sortValidatedCandidates
    :: Ord details
    => [ValidatedCandidate details output]
    -> [ValidatedCandidate details output]
sortValidatedCandidates = sortOn validatedCandidateDetails

-- | The final private Djinn result before compatibility erasure.  It retains
-- every surviving candidate sidecar through cross-plan de-duplication and
-- any configured stable ranking.  The result constructor is hidden so the
-- historical shared result and typed result can be built only through the
-- projections below.
data ValidatedResult metadata details output = ValidatedResult
    SharedQuery.QueryEvidence
    SharedSearch.Completion
    metadata
    [ValidatedCandidate details output]

type role ValidatedResult nominal nominal nominal

-- | Package final metadata, operational completion, logical evidence, and
-- sidecar-retaining candidates before the compatibility projection.
mkValidatedResult
    :: SharedQuery.QueryEvidence
    -> SharedSearch.Completion
    -> metadata
    -> [ValidatedCandidate details output]
    -> ValidatedResult metadata details output
mkValidatedResult = ValidatedResult

-- | Inspect the final checked associations without rebuilding candidates.
validatedResultCandidates
    :: ValidatedResult metadata details output
    -> [ValidatedCandidate details output]
validatedResultCandidates (ValidatedResult _ _ _ candidates) = candidates

-- | One-way projection to Djinn's historical shared query result.
--
-- Mapping is deliberately lazy.  'mkQueryResult' observes only whether the
-- candidate list is empty, while projecting a candidate does not inspect its
-- proof evidence.  Compatibility callers therefore do not pay for evidence
-- tree traversal and cannot detach or replace the retained sidecar.
projectValidatedResult
    :: ValidatedResult metadata details output
    -> Either SharedQuery.QueryResultInvariantError
        (SharedQuery.QueryResult metadata
            (SharedCandidate.Candidate ty details output))
projectValidatedResult = projectValidatedResultWith projectCandidate
  where
    projectCandidate _ (ValidatedCandidate output details _) =
        SharedCandidate.Candidate
            { SharedCandidate.candidateOutput = output
            , SharedCandidate.candidateResidualConstraints = []
            , SharedCandidate.candidateDetails = details
            }

-- | Project a complete result while retaining each candidate as one opaque
-- association until the supplied candidate projection consumes it.
--
-- This is the package-private typed-result seam.  In particular, the caller
-- receives a deterministic final candidate key plus the output, details, and
-- checker evidence from one constructor at a time; it never zips a separately
-- projected compatibility list onto a sidecar list after de-duplication or an
-- optional sort. The key is allocated only after the configured final ordering
-- step, so a future graph builder can derive disjoint node identities without
-- trusting search-plan ordinals discarded by de-duplication.
projectValidatedResultWith
    :: (Natural -> ValidatedCandidate details output -> candidate)
    -> ValidatedResult metadata details output
    -> Either SharedQuery.QueryResultInvariantError
        (SharedQuery.QueryResult metadata candidate)
projectValidatedResultWith projectCandidate
        (ValidatedResult evidence completion metadata candidates) =
    SharedQuery.mkQueryResult evidence $
        SharedSearch.SearchBatch
            (SharedSearch.Completed completion)
            metadata
            (zipWith projectCandidate [0 ..] candidates)
