-- | Checked Djinn sessions behind Djex's backend-neutral query envelope.
--
-- A session seals the exact Djinn environment once and retains one prepared
-- shared inventory together with every private search index derived from it.
-- Queries use the shared source-type vocabulary while retaining Djinn's
-- proof-search options and backend-specific evidence.
module Language.Haskell.Djex.Djinn
  ( DjinnSession
  , DjinnEnvironment
  , DjinnInventory
  , DjinnTypeVariable
  , DjinnLocal
  , DjinnType
  , QueryOptions (..)
  , defaultQueryOptions
  , DjinnCandidate
  , DjinnCandidateDetails (..)
  , Qualification (..)
  , RenderError (..)
  , DjinnQueryMetadata (..)
  , DjinnDeclarationSnapshot
  , DjinnRequest
  , DjinnResult
  , mkDjinnSession
  , standardDjinnSession
  , djinnSessionEnvironment
  , djinnSessionInventory
  , djinnSessionDeclarationSnapshot
  , djinnSnapshotTypeDeclarations
  , djinnSnapshotFunctionDeclarations
  , djinnSnapshotClassDeclarations
  , declareDjinnDeclaration
  , removeDjinnDeclaration
  , djinnSessionTypeDeclarations
  , djinnSessionFunctionDeclarations
  , djinnSessionClassDeclarations
  , resolveDjinnInstanceMethods
  , mkDjinnRequest
  , djinnRequestQuery
  , parseDjinnRequest
  , parseDjinnRequestWithCheckedTarget
  , validateDjinnTarget
  , validateDjinnQueryType
  , runDjinnQuery
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
  , validateDjinnQueryType
  , validateDjinnTarget
  )
import qualified Language.Haskell.Djex.Djinn.Internal.Request as Request
import Language.Haskell.Djex.Djinn.Internal.Session
  ( DjinnDeclarationSnapshot
  , DjinnEnvironment
  , DjinnInventory
  , DjinnSession
  , declareDjinnDeclaration
  , djinnSessionClassDeclarations
  , djinnSessionDeclarationSnapshot
  , djinnSessionEnvironment
  , djinnSessionFunctionDeclarations
  , djinnSessionInventory
  , djinnSessionTypeDeclarations
  , djinnSnapshotClassDeclarations
  , djinnSnapshotFunctionDeclarations
  , djinnSnapshotTypeDeclarations
  , mkDjinnSession
  , removeDjinnDeclaration
  , resolveDjinnInstanceMethods
  , standardDjinnSession
  )
import qualified Language.Haskell.Djex.Djinn.Internal.Session as Session
import Language.Haskell.Synthesis.Candidate
  ( renderCandidateDefinition
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
  ( QueryRequest (..)
  , RequestProvenance (..)
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
runDjinnQuery session request = do
  let query = djinnRequestQuery request
  case Core.inhabitSynthesisResultPrepared
      (requestOptions query)
      (Session.sessionPreparedEnvironment session)
      (Request.requestPlanContexts request)
      (requestTarget query)
      (Request.requestPlanGoal request) of
    Left failure -> Left $
      djinnQueryFailure request failure
    Right result -> Right result

-- | Render only a Djinn candidate's generated expression.
renderDjinnCandidateExpression
  :: Qualification
  -> DjinnCandidate
  -> Either RenderError String
renderDjinnCandidateExpression qualification =
  renderCandidateExpression $ candidateRenderOptions qualification

-- | Render a complete definition using the target retained by the candidate.
renderDjinnCandidateDefinition
  :: Qualification
  -> DjinnCandidate
  -> Either RenderError String
renderDjinnCandidateDefinition qualification =
  renderCandidateDefinition $ candidateRenderOptions qualification

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
  DjinnInternalQueryFailure message -> contextualDiagnostic Error
    "DJEX_DJINN_INTERNAL"
    "Djinn violated an internal query invariant"
    message
  DjinnResultInvariantFailure invariant -> shownErrorDiagnostic
    "DJEX_DJINN_RESULT"
    "Djinn produced inconsistent logical evidence"
    invariant
