-- | Checked Djinn sessions behind Djex's backend-neutral query envelope.
--
-- A session seals the exact Djinn environment once and retains one prepared
-- shared inventory together with every private search index derived from it.
-- Queries use the shared source-type vocabulary while retaining Djinn's
-- proof-search options and backend-specific evidence.
--
-- The curated session is immutable, matching the Exference adapter: change a
-- declaration by constructing a replacement neutral 'DjinnEnvironment' and
-- sealing it with 'mkDjinnSession'. Raw declaration edits, declaration-table
-- snapshots, and instance-method projection remain compatibility frontend
-- implementation details and do not cross this module.
module Language.Haskell.Djex.Djinn
  ( -- * Sessions
    DjinnSession
  , DjinnEnvironment
  , DjinnInventory
  , mkDjinnSession
  , standardDjinnSession
  , djinnSessionEnvironment
  , djinnSessionInventory

    -- * Requests
  , DjinnTypeVariable
  , DjinnLocal
  , DjinnType
  , QueryOptions (..)
  , defaultQueryOptions
  , DjinnRequest
  , mkDjinnRequest
  , djinnRequestQuery
  , parseDjinnRequest
  , parseDjinnRequestWithCheckedTarget
  , validateDjinnTarget

    -- * Results
  , DjinnCandidate
  , DjinnCandidateDetails (..)
  , Qualification (..)
  , RenderError (..)
  , DjinnQueryMetadata (..)
  , DjinnResult
  , runDjinnQuery
  , runDjinnQueryWithInstantiationCandidates
  , renderDjinnCandidateExpression
  , renderDjinnCandidateDefinition
  ) where

import Djinn.Core
  ( DjinnCandidateDetails (..)
  , DjinnCandidate
  , DjinnQueryError (..)
  , DjinnQueryMetadata (..)
  , DjinnResult
  )
import qualified Djinn.Core as Core
import Language.Haskell.Djex.Djinn.Internal.Request
  ( DjinnLocal
  , DjinnRequest
  , DjinnType
  , DjinnTypeVariable
  , QueryOptions (..)
  , defaultQueryOptions
  , djinnRequestQuery
  , mkDjinnRequest
  , validateDjinnTarget
  )
import qualified Language.Haskell.Djex.Djinn.Internal.Request as Request
import Language.Haskell.Djex.Djinn.Internal.Session
  ( DjinnEnvironment
  , DjinnInventory
  , DjinnSession
  , djinnSessionEnvironment
  , djinnSessionInventory
  , mkDjinnSession
  , standardDjinnSession
  )
import qualified Language.Haskell.Djex.Djinn.Internal.Session as Session
import Language.Haskell.Synthesis.Candidate
  ( candidateResidualConstraints
  , renderCandidateDefinition
  , renderCandidateExpression
  )
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error)
  , contextualDiagnostic
  , shownErrorDiagnostic
  , sourceTextLocation
  )
import Language.Haskell.Synthesis.Generated
  ( DefinitionName
  , Qualification (..)
  , RenderError (..)
  , RenderOptions (renderQualification)
  , defaultRenderOptions
  )
import Language.Haskell.Synthesis.Name (Name)
import Language.Haskell.Synthesis.Query
  ( ProviderInstantiationCandidate
  , QueryEvidence (..)
  , QueryRequest (..)
  , RequestProvenance (..)
  , mkQueryResult
  , resultEvidence
  , resultSearch
  , withRequestProvenance
  )

-- | Parse the type portion of a Djinn query.  The accepted context grammar is
-- exactly the historical one: either one constraint or a comma-separated
-- parenthesized list, followed by @=>@ and the goal.  The session argument
-- keeps this boundary parallel with Exference's request parser; Djinn's parser
-- itself is environment-independent, while kind and class lookup remain part
-- of 'runDjinnQuery'.
parseDjinnRequest
  :: DjinnSession
  -> QueryOptions
  -> Name
  -> FilePath
  -> String
  -> Either Diagnostic DjinnRequest
parseDjinnRequest session options target sourceName source = do
  -- Preserve command-boundary precedence: an invalid output name is a usage
  -- error even when the source text is also malformed.
  checkedTarget <- validateDjinnTarget target
  parseDjinnRequestWithCheckedTarget
    session options checkedTarget sourceName source

-- | Parse a Djinn query for a target already checked at an outer command
-- boundary. This avoids repeating target validation while retaining the same
-- source parser and opaque request construction as 'parseDjinnRequest'.
parseDjinnRequestWithCheckedTarget
  :: DjinnSession
  -> QueryOptions
  -> DefinitionName
  -> FilePath
  -> String
  -> Either Diagnostic DjinnRequest
parseDjinnRequestWithCheckedTarget _session options checkedTarget
    sourceName source = do
  let provenance = SourceRequest $ sourceTextLocation sourceName source
  (rawContexts, rawGoal) <- case Core.parseContextualHType source of
    Right parsed -> Right parsed
    Left failure -> Left $ withRequestProvenance provenance
      $ contextualDiagnostic Error "DJEX_DJINN_PARSE"
          "cannot parse the Djinn query type" failure
  goal <- Request.validateDjinnQueryTypeWithProvenance
    provenance "goal" rawGoal
  contexts <- traverse
    (traverse $ Request.validateDjinnQueryTypeWithProvenance
      provenance "context")
    rawContexts
  let query = QueryRequest
        { requestTarget = checkedTarget
        , requestGoal = goal
        , requestContexts = contexts
        , requestOptions = options
        }
  Request.mkDjinnRequestWithProvenance provenance query

-- | Run one complete configured Djinn search and package it into a single
-- terminal shared batch.  Logical evidence stays independent of operational
-- completion: a validated candidate found before a budget expires remains a
-- candidate, while an empty truncated search remains undecided.
runDjinnQuery
  :: DjinnSession
  -> DjinnRequest
  -> Either Diagnostic DjinnResult
runDjinnQuery session =
  runDjinnQueryWithInstantiationCandidates session []

-- | Run one checked Djinn search with externally established type choices for
-- exact named providers. Ordinary request validation retains precedence; the
-- evidence list is then bounded, elaborated, kind-checked, and kept
-- provider-local against this exact session before it can add a separate
-- proof-producing search tail.
runDjinnQueryWithInstantiationCandidates
  :: DjinnSession
  -> [ProviderInstantiationCandidate DjinnTypeVariable]
  -> DjinnRequest
  -> Either Diagnostic DjinnResult
runDjinnQueryWithInstantiationCandidates session candidates request = do
  let query = djinnRequestQuery request
  (contexts, goal) <- Request.prepareDjinnRequest
    (Session.sessionClassArity session) request
  case Core.inhabitSynthesisResultPreparedWithInstantiationCandidates
      (requestOptions query)
      (Session.sessionPreparedEnvironment session)
      contexts
      candidates
      (requestTarget query)
      goal of
    Left failure -> Left $
      djinnQueryFailure request failure
    Right result
      | Session.sessionContextualProvidersOmitted session ->
          weakenCandidateFreeEvidence result
      | otherwise -> Right result
 where
  -- A projected contextual provider is absent from proof search, not proved
  -- unusable. Preserve every checked candidate, but make all candidate-free
  -- conclusions explicitly inconclusive, including the self-reference-only
  -- diagnostic which an omitted ordinary provider could have avoided.
  weakenCandidateFreeEvidence result = case resultEvidence result of
    ValidatedCandidates -> Right result
    _ -> case mkQueryResult NoEvidence $ resultSearch result of
      Left invariant -> Left $ djinnQueryFailure request
        $ DjinnResultInvariantFailure invariant
      Right weakened -> Right weakened

-- | Render only a Djinn candidate's generated expression.
--
-- Genuine Djinn results are closed and therefore have no residual
-- constraints. Because the compatibility
-- 'Language.Haskell.Synthesis.Candidate.Candidate' constructor is public,
-- this boundary also rejects a caller-built candidate that attaches an
-- obligation. Generated-syntax validation deliberately runs first, preserving
-- the existing scope and lexical error precedence when both parts are forged.
renderDjinnCandidateExpression
  :: Qualification
  -> DjinnCandidate
  -> Either RenderError String
renderDjinnCandidateExpression qualification candidate = do
  rendered <- renderCandidateExpression
    (candidateRenderOptions qualification) candidate
  validateClosedDjinnCandidate candidate
  pure rendered

-- | Render a complete definition using the target retained by the candidate.
-- Scope and lexical failures take precedence over
-- 'UnexpectedResidualConstraints', as in 'renderDjinnCandidateExpression'.
renderDjinnCandidateDefinition
  :: Qualification
  -> DjinnCandidate
  -> Either RenderError String
renderDjinnCandidateDefinition qualification candidate = do
  rendered <- renderCandidateDefinition
    (candidateRenderOptions qualification) candidate
  validateClosedDjinnCandidate candidate
  pure rendered

validateClosedDjinnCandidate :: DjinnCandidate -> Either RenderError ()
validateClosedDjinnCandidate candidate = case
    candidateResidualConstraints candidate of
  [] -> Right ()
  _ : _ -> Left UnexpectedResidualConstraints

candidateRenderOptions :: Qualification -> RenderOptions DjinnLocal
candidateRenderOptions qualification =
  (defaultRenderOptions id) {renderQualification = qualification}

djinnQueryFailure
  :: DjinnRequest
  -> DjinnQueryError
  -> Diagnostic
djinnQueryFailure request failure = case failure of
  DjinnQueryOptionsFailure optionsFailure -> shownErrorDiagnostic
    "DJEX_DJINN_OPTIONS" "invalid Djinn search options" optionsFailure
  DjinnQueryFailure message -> Request.withDjinnRequestProvenance request
    $ contextualDiagnostic Error
        "DJEX_DJINN_QUERY" "Djinn rejected the query" message
  DjinnInstantiationCandidateFailure message -> contextualDiagnostic Error
    "DJEX_DJINN_CANDIDATE"
    "Djinn rejected provider instantiation evidence"
    message
  DjinnInternalQueryFailure message -> contextualDiagnostic Error
    "DJEX_DJINN_INTERNAL"
    "Djinn violated an internal query invariant"
    message
  DjinnResultInvariantFailure invariant -> shownErrorDiagnostic
    "DJEX_DJINN_RESULT"
    "Djinn produced inconsistent logical evidence"
    invariant
