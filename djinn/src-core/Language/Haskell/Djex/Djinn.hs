{-# LANGUAGE DerivingVia #-}

-- | Checked Djinn sessions behind Djex's backend-neutral query envelope.
--
-- A session seals the exact Djinn environment once and retains both the shared
-- inventory that justified it and the alias table prepared from that same
-- inventory. Queries use the shared source-type vocabulary while retaining
-- Djinn's proof-search options and backend-specific evidence.
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
  , DjinnRequest
  , DjinnResult
  , mkDjinnSession
  , standardDjinnSession
  , djinnSessionInventory
  , mkDjinnRequest
  , djinnRequestQuery
  , parseDjinnRequest
  , parseDjinnRequestWithCheckedTarget
  , runDjinnQuery
  , renderDjinnCandidateExpression
  , renderDjinnCandidateDefinition
  ) where

import Data.Bifunctor (first)

import Djinn.Core
  ( DjinnCandidateDetails (..)
  , PreparedEnvironment
  , QueryOptions (..)
  , defaultQueryOptions
  , generatedReportCandidates
  , generatedReportCompletion
  , generatedReportEvidence
  , generatedReportFormula
  , generatedReportProof
  , inhabitGeneratedPrepared
  , prepareSynthesisEnvironment
  , preparedEnvironmentInventory
  , standardEnvironment
  , toSynthesisEnvironment
  )
import qualified Djinn.Core as Core
import Language.Haskell.Synthesis.Candidate
  ( Candidate (..)
  , renderCandidateDefinition
  , renderCandidateExpression
  )
import Language.Haskell.Synthesis.Constraint
  ( Constraint
  , constraintArguments
  , constraintClass
  )
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error)
  , SourceSpan
  , contextualDiagnostic
  , shownErrorDiagnostic
  , sourceTextSpan
  , withLocation
  , withSource
  )
import Language.Haskell.Synthesis.Generated
  ( DefinitionName
  , FunctionClause
  , Qualification (..)
  , RenderError (..)
  , RenderOptions (renderQualification)
  , defaultRenderOptions
  , definitionSpelling
  , mkDefinitionName
  )
import Language.Haskell.Synthesis.Name
  ( Name
  , renderCanonical
  )
import Language.Haskell.Synthesis.Query
  ( CachedQuery
  , QueryRequest (..)
  , QueryResult
  , QueryResultInvariantError
  , cachedQueryCache
  , cachedQueryRequest
  , mkCachedQuery
  , mkQueryResult
  )
import Language.Haskell.Synthesis.Search
  ( Progress (Completed)
  , SearchBatch (SearchBatch)
  )
import Language.Haskell.Synthesis.Environment (Environment)
import Language.Haskell.Synthesis.Inventory (Inventory)
import Language.Haskell.Synthesis.Type (Type)

-- | The neutral declaration environment accepted by the Djinn adapter.
-- Djinn currently uses textual source variables and integer kind variables;
-- neither choice leaks its mutable compatibility environment through Djex.
type DjinnEnvironment = Environment DjinnTypeVariable Int ()

-- | The checked neutral inventory sealed into a Djinn session.
type DjinnInventory = Inventory DjinnTypeVariable ()

-- | Djinn's source-level type-variable identity.
--
-- It currently shares the same representation as generated binders, but the
-- distinct alias keeps those unrelated namespaces separate in the public API.
type DjinnTypeVariable = String

-- | Djinn's generated-expression binder identity.
type DjinnLocal = String

-- | Source types accepted and returned by the stable Djinn adapter.
-- They are lowered to backend-specific proof types only while sealing a
-- checked t'DjinnRequest'.
type DjinnType = Type DjinnTypeVariable

-- | A validated Djinn environment paired with its exact shared inventory and
-- prepared synonym table. The constructor is private so those views cannot
-- drift apart.
newtype DjinnSession = DjinnSession PreparedEnvironment

-- | Djinn-specific explanatory data that does not belong in the common
-- operational search status.
data DjinnQueryMetadata = DjinnQueryMetadata
  { djinnTranslatedFormula :: String
  , djinnFirstExploredProof :: Maybe String
  }
  deriving (Eq, Show)

-- | The raw projection and optional source provenance derived while sealing
-- a request. The shared 'CachedQuery' envelope keeps both details out of the
-- request's stable equality and display contract.
data DjinnRequestCache = DjinnRequestCache
  { cachedTargetSymbol :: Core.HSymbol
  , cachedCoreGoal :: Core.HType
  , cachedCoreContexts :: [Core.Context]
  , cachedSourceLocation :: Maybe (FilePath, SourceSpan)
  }

-- | A checked query whose neutral spelling and backend projection cannot
-- drift apart.  The constructor and cached values stay private; callers can
-- inspect the original neutral query with 'djinnRequestQuery'.
newtype DjinnRequest = DjinnRequest
  (CachedQuery DjinnType QueryOptions DjinnRequestCache)
  deriving (Eq, Show)
    via (CachedQuery DjinnType QueryOptions DjinnRequestCache)

-- Spell out the shared candidate shape here instead of re-exporting Djinn's
-- identical alias, whose historical @HSymbol@ name is intentionally private
-- to the raw compatibility API.
type DjinnCandidate =
  Candidate DjinnType DjinnCandidateDetails (FunctionClause DjinnLocal)

type DjinnResult = QueryResult DjinnQueryMetadata DjinnCandidate

-- | Lower a shared declaration environment through Djinn's stricter lexical,
-- dependency, and kind checks, then seal it into a reusable session.
mkDjinnSession :: DjinnEnvironment -> Either Diagnostic DjinnSession
mkDjinnSession sharedEnvironment = DjinnSession <$>
  first environmentFailure (prepareSynthesisEnvironment sharedEnvironment)

environmentFailure
  :: Core.SynthesisEnvironmentError
  -> Diagnostic
environmentFailure = shownErrorDiagnostic "DJEX_DJINN_ENV"
  "cannot lower the shared environment to Djinn"

-- | The historical checked Djinn prelude, sealed for facade-only clients.
-- Advanced clients can convert an editable raw environment with
-- @Djinn.Core.toSynthesisEnvironment@ before calling 'mkDjinnSession'.
standardDjinnSession :: Either Diagnostic DjinnSession
standardDjinnSession =
  first environmentFailure (toSynthesisEnvironment standardEnvironment)
    >>= mkDjinnSession

djinnSessionInventory :: DjinnSession -> DjinnInventory
djinnSessionInventory (DjinnSession prepared) =
  preparedEnvironmentInventory prepared

-- | Check and lower the session-independent portion of a neutral Djinn
-- query.  Target spelling, the goal, and context arguments are converted
-- exactly once and retained behind the opaque request boundary.  Search
-- options and all environment-dependent kind, class, and synonym checks
-- deliberately remain the responsibility of 'runDjinnQuery'. A request can
-- therefore be run against another compatible session without retaining the
-- first session's alias meanings.
mkDjinnRequest
  :: QueryRequest DjinnType QueryOptions
  -> Either Diagnostic DjinnRequest
mkDjinnRequest = mkDjinnRequestWithSource Nothing

mkDjinnRequestWithSource
  :: Maybe (FilePath, SourceSpan)
  -> QueryRequest DjinnType QueryOptions
  -> Either Diagnostic DjinnRequest
mkDjinnRequestWithSource sourceLocation query = do
  let target = definitionSpelling $ requestTarget query
  goal <- lowerRequestType "goal" $ requestGoal query
  contexts <- traverse lowerRequestContext $ requestContexts query
  pure $ DjinnRequest $ mkCachedQuery query DjinnRequestCache
    { cachedTargetSymbol = target
    , cachedCoreGoal = goal
    , cachedCoreContexts = contexts
    , cachedSourceLocation = sourceLocation
    }

-- | Recover the exact neutral query from which this checked request was
-- sealed.  Modifications must be passed back through 'mkDjinnRequest'.
djinnRequestQuery
  :: DjinnRequest
  -> QueryRequest DjinnType QueryOptions
djinnRequestQuery (DjinnRequest query) = cachedQueryRequest query

djinnRequestCache :: DjinnRequest -> DjinnRequestCache
djinnRequestCache (DjinnRequest query) = cachedQueryCache query

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
  checkedTarget <- checkDefinitionTarget target
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
  (rawContexts, rawGoal) <- case Core.parseContextualHType source of
    Right parsed -> Right parsed
    Left failure -> Left $ withSource sourceName
      $ contextualDiagnostic Error "DJEX_DJINN_PARSE"
          "cannot parse the Djinn query type" failure
  goal <- first (parsedTypeFailure sourceName "goal")
    $ Core.toSynthesisType rawGoal
  contexts <- first (parsedTypeFailure sourceName "context")
    $ traverse (traverse Core.toSynthesisType) rawContexts
  let query = QueryRequest
        { requestTarget = checkedTarget
        , requestGoal = goal
        , requestContexts = contexts
        , requestOptions = options
        }
  mkDjinnRequestWithSource
    (Just (sourceName, sourceTextSpan source)) query

-- | Run one complete configured Djinn search and project it into a single
-- terminal shared batch.  Logical evidence stays independent of operational
-- completion: a validated candidate found before a budget expires remains a
-- candidate, while an empty truncated search remains undecided.
runDjinnQuery
  :: DjinnSession
  -> DjinnRequest
  -> Either Diagnostic DjinnResult
runDjinnQuery (DjinnSession prepared) request = do
  let cache = djinnRequestCache request
  report <- case inhabitGeneratedPrepared
      (requestOptions $ djinnRequestQuery request)
      prepared
      (cachedCoreContexts cache)
      (cachedTargetSymbol cache)
      (cachedCoreGoal cache) of
    Left failure -> Left $ attachRequestSource request
      $ contextualDiagnostic Error "DJEX_DJINN_QUERY"
        "Djinn rejected the query" failure
    Right value -> Right value
  candidates <- first candidateProjectionFailure
    $ traverse projectCandidate
    $ generatedReportCandidates report
  let metadata = DjinnQueryMetadata
        { djinnTranslatedFormula = generatedReportFormula report
        , djinnFirstExploredProof = generatedReportProof report
        }
      batch = SearchBatch
        (Completed $ generatedReportCompletion report)
        metadata
        candidates
  first queryResultFailure
    $ mkQueryResult (generatedReportEvidence report) batch

-- Programmatic requests deliberately carry no source. Parsed requests retain
-- their complete input range so only environment-dependent proof-search
-- rejection acquires that location; eager parser/lowering diagnostics and
-- adapter-internal projection/invariant failures retain their established
-- source-less shape.
attachRequestSource :: DjinnRequest -> Diagnostic -> Diagnostic
attachRequestSource request diagnostic = case cachedSourceLocation
    $ djinnRequestCache request of
  Nothing -> diagnostic
  Just (sourceName, sourceSpan) ->
    withLocation sourceName sourceSpan diagnostic

-- The core currently proves every obligation and therefore emits no residual
-- constraints. Keep this projection checked nevertheless: it preserves the
-- stable candidate type if the backend later starts returning obligations.
projectCandidate
  :: Core.DjinnCandidate
  -> Either Core.SynthesisTypeError DjinnCandidate
projectCandidate candidate = do
  residualConstraints <- traverse (traverse Core.toSynthesisType)
    $ candidateResidualConstraints candidate
  pure Candidate
    { candidateOutput = candidateOutput candidate
    , candidateResidualConstraints = residualConstraints
    , candidateDetails = candidateDetails candidate
    }

renderDjinnCandidateExpression
  :: Qualification
  -> DjinnCandidate
  -> Either RenderError String
renderDjinnCandidateExpression qualification =
  renderCandidateExpression $ candidateRenderOptions qualification

renderDjinnCandidateDefinition
  :: Qualification
  -> DjinnCandidate
  -> Either RenderError String
renderDjinnCandidateDefinition qualification =
  renderCandidateDefinition $ candidateRenderOptions qualification

candidateRenderOptions :: Qualification -> RenderOptions DjinnLocal
candidateRenderOptions qualification =
  (defaultRenderOptions id) {renderQualification = qualification}

lowerRequestType :: String -> DjinnType -> Either Diagnostic Core.HType
lowerRequestType role = first (loweringFailure role)
  . Core.fromSynthesisType

-- Constraint is intentionally a more permissive neutral node than Djinn's
-- historical grammar.  Rebuild it with the core smart constructor so a
-- qualified or otherwise non-Djinn class name cannot cross the sealed
-- request boundary even when a caller constructed the shared node directly.
lowerRequestContext
  :: Constraint DjinnType
  -> Either Diagnostic Core.Context
lowerRequestContext context = do
  arguments <- traverse (lowerRequestType "context")
    $ constraintArguments context
  first contextLoweringFailure $ Core.mkContext
    (renderCanonical $ constraintClass context)
    arguments

parsedTypeFailure
  :: FilePath
  -> String
  -> Core.SynthesisTypeError
  -> Diagnostic
parsedTypeFailure sourceName role failure = withSource sourceName
  $ contextualDiagnostic Error "DJEX_DJINN_PARSE"
      "cannot project the parsed Djinn query type"
      (role ++ ": " ++ show failure)

loweringFailure :: String -> Core.SynthesisTypeError -> Diagnostic
loweringFailure role failure = contextualDiagnostic Error "DJEX_DJINN_LOWER"
  "cannot lower the shared query to Djinn" (role ++ ": " ++ show failure)

contextLoweringFailure :: String -> Diagnostic
contextLoweringFailure failure = contextualDiagnostic Error
  "DJEX_DJINN_LOWER" "cannot lower the shared query to Djinn"
  ("context: " ++ failure)

candidateProjectionFailure :: Core.SynthesisTypeError -> Diagnostic
candidateProjectionFailure = shownErrorDiagnostic
  "DJEX_DJINN_PROJECT" "cannot project a Djinn candidate to shared types"

queryResultFailure :: QueryResultInvariantError -> Diagnostic
queryResultFailure = shownErrorDiagnostic
  "DJEX_DJINN_RESULT" "Djinn produced inconsistent logical evidence"

checkDefinitionTarget :: Name -> Either Diagnostic DefinitionName
checkDefinitionTarget target = case mkDefinitionName target of
  Right checked -> Right checked
  Left _ -> Left $ contextualDiagnostic Error "DJEX_DJINN_TARGET"
      "Djinn targets must be unqualified value identifiers or operators"
      (renderCanonical target)
