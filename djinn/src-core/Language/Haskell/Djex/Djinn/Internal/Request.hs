{-# LANGUAGE DerivingVia #-}

{-# OPTIONS_HADDOCK not-home #-}

-- | Private ownership of Djinn's checked request and execution plan.
--
-- The stable facade re-exports only the opaque request, its checked
-- constructor, and the caller's exact neutral query. Canonical execution data
-- and diagnostic provenance remain inseparable inside this module.
module Language.Haskell.Djex.Djinn.Internal.Request
  ( QueryOptions (..)
  , defaultQueryOptions
  , DjinnRequest
  , DjinnTypeVariable
  , DjinnLocal
  , DjinnType
  , mkDjinnRequest
  , mkDjinnRequestWithProvenance
  , djinnRequestQuery
  , requestPlanGoal
  , requestPlanContexts
  , withDjinnRequestProvenance
  , validateDjinnQueryType
  , validateDjinnQueryTypeWithProvenance
  , validateDjinnTarget
  ) where

import Data.Bifunctor (first)

import Djinn.Core
  ( QueryOptions (..)
  , defaultQueryOptions
  )
import qualified Djinn.Core as Core
import Language.Haskell.Synthesis.Constraint
  ( Constraint
  , constraintClass
  )
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error)
  , contextualDiagnostic
  )
import Language.Haskell.Synthesis.Generated
  ( DefinitionName
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
  , RequestTypeSite (..)
  , cachedQueryCache
  , cachedQueryRequest
  , requestTypeSiteLabel
  , sealCachedQueryWithProvenance
  , traverseRequestTypes
  , withCachedQueryProvenance
  , withRequestProvenance
  )
import Language.Haskell.Synthesis.Type (Type)

-- | Djinn's source-level type-variable identity.
--
-- The vocabulary distinguishes this role from generated binders in signatures,
-- but both compatibility aliases remain 'String' and are not nominally distinct.
type DjinnTypeVariable = String

-- | Djinn's generated-expression binder identity.
type DjinnLocal = String

-- | Source types accepted and returned by the stable Djinn adapter.
-- Checked requests retain this shared representation through kind checking,
-- synonym elaboration, class-method instantiation, and formula compilation.
-- Djinn's historical raw type remains only at compatibility API boundaries.
type DjinnType = Type DjinnTypeVariable

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

-- | Check the session-independent portion of a neutral Djinn query.
-- Goal and context arguments are canonicalized once into a shared plan, while
-- 'djinnRequestQuery' retains the caller's exact neutral value. Search options
-- and all environment-dependent kind, class, and synonym checks deliberately
-- remain the responsibility of the facade's query worker. A request can
-- therefore run against another compatible session without retaining the
-- first session's alias meanings.
mkDjinnRequest
  :: QueryRequest DjinnType QueryOptions
  -> Either Diagnostic DjinnRequest
mkDjinnRequest = mkDjinnRequestWithProvenance ProgrammaticRequest

-- | Seal a checked request while retaining trusted diagnostic provenance.
mkDjinnRequestWithProvenance
  :: RequestProvenance
  -> QueryRequest DjinnType QueryOptions
  -> Either Diagnostic DjinnRequest
mkDjinnRequestWithProvenance provenance query = DjinnRequest <$>
  sealCachedQueryWithProvenance provenance (do
    normalized <- traverseRequestTypes
      normalizeRequestType validateRequestContext query
    pure
      ( query
      , DjinnRequestPlan
        { plannedGoal = requestGoal normalized
        , plannedContexts = requestContexts normalized
        }
      ))

-- | Recover the exact neutral query from which this checked request was
-- sealed. Modifications must be passed back through 'mkDjinnRequest'.
djinnRequestQuery
  :: DjinnRequest
  -> QueryRequest DjinnType QueryOptions
djinnRequestQuery (DjinnRequest query) = cachedQueryRequest query

-- | Recover the canonical goal consumed by the private query worker.
requestPlanGoal :: DjinnRequest -> DjinnType
requestPlanGoal = plannedGoal . djinnRequestPlan

-- | Recover the canonical contexts consumed by the private query worker.
requestPlanContexts :: DjinnRequest -> [Constraint DjinnType]
requestPlanContexts = plannedContexts . djinnRequestPlan

-- | Attach a request's sealed provenance to a diagnostic.
withDjinnRequestProvenance
  :: DjinnRequest
  -> Diagnostic
  -> Diagnostic
withDjinnRequestProvenance (DjinnRequest query) =
  withCachedQueryProvenance query

-- | Validate one compatibility-parsed raw type into the shared query
-- vocabulary. The Haskeline REPL and the facade's string parser share this
-- boundary, so a declaration-only or malformed node receives the same
-- diagnostic on either path.
validateDjinnQueryType
  :: String
  -> Core.HType
  -> Either Diagnostic DjinnType
validateDjinnQueryType role =
  first (parsedTypeDiagnostic role) . Core.toSynthesisType

-- | Provenance-aware counterpart used by the facade's source parser.
validateDjinnQueryTypeWithProvenance
  :: RequestProvenance
  -> String
  -> Core.HType
  -> Either Diagnostic DjinnType
validateDjinnQueryTypeWithProvenance provenance role =
  first (withRequestProvenance provenance . parsedTypeDiagnostic role)
    . Core.toSynthesisType

-- | Check the source-level name of a Djinn result definition. Frontends use
-- this before parsing so command-usage errors retain precedence over malformed
-- source text, mirroring Exference's checked request boundary.
validateDjinnTarget :: Name -> Either Diagnostic DefinitionName
validateDjinnTarget target = case mkDefinitionName target of
  Right checked -> Right checked
  Left _ -> Left $ contextualDiagnostic Error "DJEX_DJINN_TARGET"
      "Djinn targets must be unqualified value identifiers or operators"
      (renderCanonical target)

djinnRequestPlan :: DjinnRequest -> DjinnRequestPlan
djinnRequestPlan (DjinnRequest query) = cachedQueryCache query

normalizeRequestType
  :: RequestTypeSite
  -> DjinnType
  -> Either Diagnostic DjinnType
normalizeRequestType site = first
  (loweringFailure $ requestTypeSiteLabel site)
  . Core.normalizeSynthesisType

-- Constraint is intentionally a more permissive neutral node than Djinn's
-- historical grammar. Validate its name with the core smart constructor so a
-- qualified or otherwise non-Djinn class cannot cross the sealed request
-- boundary, then retain its canonical arguments in the shared representation.
validateRequestContext
  :: Constraint DjinnType
  -> Either Diagnostic (Constraint DjinnType)
validateRequestContext context = do
  -- No raw type is retained: the empty context is only the historical
  -- namespace validator for the exact structural class name.
  _ <- first contextLoweringFailure $ Core.mkContext
    (renderCanonical $ constraintClass context)
    []
  pure context

parsedTypeDiagnostic :: String -> Core.SynthesisTypeError -> Diagnostic
parsedTypeDiagnostic role failure = contextualDiagnostic Error
  "DJEX_DJINN_PARSE" "cannot validate the parsed Djinn query type"
  (role ++ ": " ++ show failure)

loweringFailure :: String -> Core.SynthesisTypeError -> Diagnostic
loweringFailure role failure = contextualDiagnostic Error "DJEX_DJINN_LOWER"
  "cannot lower the shared query to Djinn" (role ++ ": " ++ show failure)

contextLoweringFailure :: String -> Diagnostic
contextLoweringFailure failure = contextualDiagnostic Error
  "DJEX_DJINN_LOWER" "cannot lower the shared query to Djinn"
  ("context: " ++ failure)
