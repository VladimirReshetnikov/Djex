{-# LANGUAGE DerivingVia #-}

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
  , DjinnRequest
  , DjinnResult
  , mkDjinnSession
  , standardDjinnSession
  , djinnSessionEnvironment
  , djinnSessionInventory
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
  , runDjinnQuery
  , renderDjinnCandidateExpression
  , renderDjinnCandidateDefinition
  ) where

import Data.Bifunctor (first)
import Data.Void (absurd)

import Djinn.Core
  ( DjinnCandidateDetails (..)
  , DjinnCandidate
  , Declaration
  , DjinnQueryError (..)
  , DjinnQueryMetadata (..)
  , DjinnResult
  , PreparedEnvironment
  , QueryOptions (..)
  , defaultQueryOptions
  , declareSynthesisEnvironment
  , removeSynthesisDeclaration
  , prepareSynthesisEnvironment
  , preparedEnvironmentSource
  , preparedEnvironmentInventory
  , resolvePreparedInstanceMethods
  )
import qualified Djinn.Core as Core
import Language.Haskell.Synthesis.Candidate
  ( renderCandidateDefinition
  , renderCandidateExpression
  )
import Language.Haskell.Synthesis.Constraint
  ( Constraint (Constraint)
  , constraintArguments
  , constraintClass
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
  , mkDefinitionName
  )
import Language.Haskell.Synthesis.Name
  ( Name
  , renderCanonical
  )
import Language.Haskell.Synthesis.Query
  ( CachedQuery
  , QueryRequest (..)
  , RequestProvenance (..)
  , cachedQueryCache
  , cachedQueryProvenance
  , cachedQueryRequest
  , mkCachedQueryWithProvenance
  , withRequestProvenance
  )
import Language.Haskell.Synthesis.Environment (Environment)
import qualified Language.Haskell.Synthesis.Environment as SharedEnvironment
import Language.Haskell.Synthesis.Inventory (Inventory)
import qualified Language.Haskell.Synthesis.Inventory as SharedInventory
import Language.Haskell.Synthesis.Type (Type)

-- | The neutral declaration environment accepted by the Djinn adapter.
-- Djinn currently uses textual source variables and integer kind variables;
-- neither choice leaks its mutable compatibility environment through Djex.
type DjinnEnvironment = Environment DjinnTypeVariable Int ()

-- | The checked neutral inventory sealed into a Djinn session.
type DjinnInventory = Inventory DjinnTypeVariable ()

-- | Djinn's source-level type-variable identity.
--
-- The vocabulary distinguishes this role from generated binders in signatures,
-- but both compatibility aliases remain 'String' and are not nominally distinct.
type DjinnTypeVariable = String

-- | Djinn's generated-expression binder identity.
type DjinnLocal = String

-- | Source types accepted and returned by the stable Djinn adapter.
-- Checked requests retain this shared representation through kind checking
-- and synonym elaboration; only the proof-formula boundary uses Djinn's
-- historical raw type.
type DjinnType = Type DjinnTypeVariable

-- | One prepared shared inventory and the exact backend indexes projected
-- from it. The constructor is private; compatibility editing derives its
-- neutral source view on demand and publishes a replacement only after the
-- complete environment has been sealed transactionally.
newtype DjinnSession = DjinnSession PreparedEnvironment

-- | The canonical shared query projection consumed by the proof core.
--
-- Keep this separate from the stable request: callers can recover their exact
-- (possibly noncanonical) neutral spelling with 'djinnRequestQuery', while a
-- reusable request never retains a second recursive type representation.
data DjinnRequestPlan = DjinnRequestPlan
  { plannedGoal :: DjinnType
  , plannedContexts :: [Constraint DjinnType]
  }

-- | A checked query whose exact neutral spelling and canonical shared plan
-- cannot drift apart. The constructor and plan stay private; callers can
-- inspect the original neutral query with 'djinnRequestQuery'.
newtype DjinnRequest = DjinnRequest
  (CachedQuery DjinnType QueryOptions DjinnRequestPlan)
  deriving (Eq, Show)
    via (CachedQuery DjinnType QueryOptions DjinnRequestPlan)

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

-- | The historical checked Djinn prelude. Its raw declaration spelling is a
-- compatibility source only: convert it once and use the same neutral session
-- constructor as every other caller.
standardDjinnSession :: Either Diagnostic DjinnSession
standardDjinnSession = do
  environment <- first environmentFailure
    $ Core.toSynthesisEnvironment Core.standardEnvironment
  mkDjinnSession environment

djinnSessionEnvironment :: DjinnSession -> DjinnEnvironment
djinnSessionEnvironment =
  SharedEnvironment.mapEnvironmentKindVariables absurd
    . SharedInventory.inventoryEnvironment
    . djinnSessionInventory

djinnSessionInventory :: DjinnSession -> DjinnInventory
djinnSessionInventory (DjinnSession prepared) =
  preparedEnvironmentInventory prepared

-- | Apply one historical declaration to the authoritative shared environment
-- and publish the replacement session only after complete validation.
declareDjinnDeclaration
  :: Declaration
  -> DjinnSession
  -> Either Diagnostic DjinnSession
declareDjinnDeclaration declaration session = do
  (_, prepared) <- first environmentEditFailure
    $ declareSynthesisEnvironment declaration
    $ djinnSessionEnvironment session
  pure $ DjinnSession prepared

-- | Transactional counterpart of the historical @:delete@ command.
removeDjinnDeclaration
  :: String
  -> DjinnSession
  -> Either Diagnostic DjinnSession
removeDjinnDeclaration name session = do
  (_, prepared) <- first environmentEditFailure
    $ removeSynthesisDeclaration name
    $ djinnSessionEnvironment session
  pure $ DjinnSession prepared

-- The following projections keep the compatibility frontend's exact display
-- and instance-generation behavior without making it retain a raw Environment.
-- Display views are reconstructed from the authoritative Inventory on demand;
-- instance lookup separately uses the sealed nominal class index.
djinnSessionTypeDeclarations
  :: DjinnSession
  -> [(Core.HSymbol, ([Core.HSymbol], Core.HType, Core.HKind))]
djinnSessionTypeDeclarations (DjinnSession prepared) =
  Core.typeDeclarations $ preparedEnvironmentSource prepared

djinnSessionFunctionDeclarations
  :: DjinnSession
  -> [(Core.HSymbol, Core.HType)]
djinnSessionFunctionDeclarations (DjinnSession prepared) =
  Core.functionDeclarations $ preparedEnvironmentSource prepared

djinnSessionClassDeclarations
  :: DjinnSession
  -> [(Core.HSymbol,
      ([(Core.HSymbol, Core.HKind)], [(Core.HSymbol, Core.HType)]))]
djinnSessionClassDeclarations (DjinnSession prepared) =
  Core.classDeclarations $ preparedEnvironmentSource prepared

resolveDjinnInstanceMethods
  :: DjinnSession
  -> [Core.Context]
  -> Core.Context
  -> Either Diagnostic [(Core.HSymbol, Core.HType)]
resolveDjinnInstanceMethods (DjinnSession prepared) prerequisites target =
  first instanceResolutionFailure
    $ resolvePreparedInstanceMethods prepared prerequisites target

-- | Check the session-independent portion of a neutral Djinn query.
-- Goal and context arguments are canonicalized once into a shared plan, while
-- 'djinnRequestQuery' retains the caller's exact neutral value. Search options
-- and all environment-dependent kind, class, and synonym checks deliberately
-- remain the responsibility of 'runDjinnQuery'. A request can therefore be
-- run against another compatible session without retaining the first
-- session's alias meanings.
mkDjinnRequest
  :: QueryRequest DjinnType QueryOptions
  -> Either Diagnostic DjinnRequest
mkDjinnRequest = mkDjinnRequestWithProvenance ProgrammaticRequest

mkDjinnRequestWithProvenance
  :: RequestProvenance
  -> QueryRequest DjinnType QueryOptions
  -> Either Diagnostic DjinnRequest
mkDjinnRequestWithProvenance provenance query =
  -- Force a sourced span at the sealing boundary.  Merely storing it in the
  -- strict field of a newtype-wrapped 'CachedQuery' is insufficient: a caller
  -- can inspect the outer 'Right' without forcing that payload.
  provenance `seq` first (withRequestProvenance provenance) (do
    goal <- normalizeRequestType "goal" $ requestGoal query
    contexts <- traverse normalizeRequestContext $ requestContexts query
    pure $ DjinnRequest $
      mkCachedQueryWithProvenance provenance query DjinnRequestPlan
        { plannedGoal = goal
        , plannedContexts = contexts
        })

-- | Recover the exact neutral query from which this checked request was
-- sealed.  Modifications must be passed back through 'mkDjinnRequest'.
djinnRequestQuery
  :: DjinnRequest
  -> QueryRequest DjinnType QueryOptions
djinnRequestQuery (DjinnRequest query) = cachedQueryRequest query

djinnRequestPlan :: DjinnRequest -> DjinnRequestPlan
djinnRequestPlan (DjinnRequest query) = cachedQueryCache query

djinnRequestProvenance :: DjinnRequest -> RequestProvenance
djinnRequestProvenance (DjinnRequest query) = cachedQueryProvenance query

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
  let provenance = SourceRequest $ sourceTextLocation sourceName source
  (rawContexts, rawGoal) <- case Core.parseContextualHType source of
    Right parsed -> Right parsed
    Left failure -> Left $ withRequestProvenance provenance
      $ contextualDiagnostic Error "DJEX_DJINN_PARSE"
          "cannot parse the Djinn query type" failure
  goal <- first (parsedTypeFailure provenance "goal")
    $ Core.toSynthesisType rawGoal
  contexts <- first (parsedTypeFailure provenance "context")
    $ traverse (traverse Core.toSynthesisType) rawContexts
  let query = QueryRequest
        { requestTarget = checkedTarget
        , requestGoal = goal
        , requestContexts = contexts
        , requestOptions = options
        }
  mkDjinnRequestWithProvenance provenance query

-- | Run one complete configured Djinn search and package it into a single
-- terminal shared batch.  Logical evidence stays independent of operational
-- completion: a validated candidate found before a budget expires remains a
-- candidate, while an empty truncated search remains undecided.
runDjinnQuery
  :: DjinnSession
  -> DjinnRequest
  -> Either Diagnostic DjinnResult
runDjinnQuery (DjinnSession prepared) request = do
  let query = djinnRequestQuery request
      plan = djinnRequestPlan request
  case Core.inhabitSynthesisResultPrepared
      (requestOptions query)
      prepared
      (plannedContexts plan)
      (requestTarget query)
      (plannedGoal plan) of
    Left failure -> Left $
      djinnQueryFailure (djinnRequestProvenance request) failure
    Right result -> Right result

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

normalizeRequestType :: String -> DjinnType -> Either Diagnostic DjinnType
normalizeRequestType role = first (loweringFailure role)
  . Core.normalizeSynthesisType

-- Constraint is intentionally a more permissive neutral node than Djinn's
-- historical grammar. Validate its name with the core smart constructor so a
-- qualified or otherwise non-Djinn class cannot cross the sealed request
-- boundary, then retain its canonical arguments in the shared representation.
normalizeRequestContext
  :: Constraint DjinnType
  -> Either Diagnostic (Constraint DjinnType)
normalizeRequestContext context = do
  arguments <- traverse (normalizeRequestType "context")
    $ constraintArguments context
  -- No raw type is retained: the empty context is only the historical
  -- namespace validator for the exact structural class name.
  _ <- first contextLoweringFailure $ Core.mkContext
    (renderCanonical $ constraintClass context)
    []
  pure $ Constraint (constraintClass context) arguments

parsedTypeFailure
  :: RequestProvenance
  -> String
  -> Core.SynthesisTypeError
  -> Diagnostic
parsedTypeFailure provenance role failure = withRequestProvenance provenance
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

checkDefinitionTarget :: Name -> Either Diagnostic DefinitionName
checkDefinitionTarget target = case mkDefinitionName target of
  Right checked -> Right checked
  Left _ -> Left $ contextualDiagnostic Error "DJEX_DJINN_TARGET"
      "Djinn targets must be unqualified value identifiers or operators"
      (renderCanonical target)

djinnQueryFailure
  :: RequestProvenance
  -> DjinnQueryError
  -> Diagnostic
djinnQueryFailure provenance failure = case failure of
  DjinnQueryOptionsFailure optionsFailure -> shownErrorDiagnostic
    "DJEX_DJINN_OPTIONS" "invalid Djinn search options" optionsFailure
  DjinnQueryFailure message -> withRequestProvenance provenance
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

environmentEditFailure
  :: Core.SynthesisEnvironmentError
  -> Diagnostic
environmentEditFailure failure = case failure of
  Core.SynthesisDeclarationNotFound name -> contextualDiagnostic Error
    "DJEX_DJINN_ENV" "cannot update the shared Djinn environment"
    (name ++ " is not defined")
  Core.ProtectedSynthesisUnit -> contextualDiagnostic Error
    "DJEX_DJINN_ENV" "cannot update the shared Djinn environment"
    "() is a built-in type and cannot be removed"
  _ -> shownErrorDiagnostic "DJEX_DJINN_ENV"
    "cannot update the shared Djinn environment" failure

instanceResolutionFailure :: String -> Diagnostic
instanceResolutionFailure = contextualDiagnostic Error
  "DJEX_DJINN_QUERY" "Djinn rejected the instance context"
