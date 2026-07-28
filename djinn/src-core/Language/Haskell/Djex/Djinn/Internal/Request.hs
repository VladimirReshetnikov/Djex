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
  , prepareDjinnRequest
  , withDjinnRequestProvenance
  , validateDjinnQueryType
  , validateDjinnQueryTypeWithProvenance
  , validateDjinnTarget
  ) where

import Data.Bifunctor (first)

import Djinn.Internal.Type (freshPrimedVariable)
import Djinn.Core
  ( QueryOptions (..)
  , defaultQueryOptions
  )
import qualified Djinn.Core as Core
import Language.Haskell.Synthesis.Constraint
  ( Constraint
  , constraintClass
  , validateKnownConstraintArityWith
  )
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error)
  , contextualDiagnostic
  )
import Language.Haskell.Synthesis.Generated
  ( DefinitionName )
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
  , requestContextualType
  , sealCachedQueryWithProvenance
  , validateRequestTarget
  , withCachedQueryProvenance
  , withRequestProvenance
  )
import Language.Haskell.Synthesis.Type (Type)
import qualified Language.Haskell.Synthesis.Type as SharedType

-- | Djinn's source-level type-variable identity.
--
-- The vocabulary distinguishes this role from generated binders in signatures,
-- but both compatibility aliases remain 'String' and are not nominally distinct.
type DjinnTypeVariable = String

-- | Djinn's generated-expression binder identity.
type DjinnLocal = String

-- | Source types accepted and returned by the stable Djinn adapter.
-- Checked requests retain this shared representation through kind checking,
-- synonym elaboration, context validation, and formula compilation.
-- Djinn's historical raw type remains only at compatibility API boundaries.
type DjinnType = Type DjinnTypeVariable

-- | The canonical shared query projection consumed by the proof core.
--
-- Keep this separate from the stable request: callers can recover their exact
-- (possibly noncanonical) neutral spelling with 'djinnRequestQuery', while a
-- reusable request never retains a second recursive type representation.
data DjinnRequestPlan = DjinnRequestPlan
  { plannedGoal :: DjinnType
  }

-- | A checked query whose exact neutral spelling and canonical shared plan
-- cannot drift apart. The constructor and plan stay private; callers can
-- inspect the original neutral query with 'djinnRequestQuery'.
newtype DjinnRequest = DjinnRequest
  (CachedQuery DjinnType QueryOptions DjinnRequestPlan)
  deriving (Eq, Show)
    via (CachedQuery DjinnType QueryOptions DjinnRequestPlan)

-- | Check the session-independent portion of a neutral Djinn query.
-- The goal is canonicalized once into a shared plan, while
-- 'djinnRequestQuery' retains the caller's exact neutral value. Context class
-- names are checked here, but their argument spines are deliberately deferred:
-- only a session's declared class arity can bound a cyclic caller-built list
-- without imposing an arbitrary library-wide maximum. Search options and all
-- environment-dependent kind, class, synonym, and context-argument checks
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
    normalizedGoal <- normalizeRequestType RequestGoal $ requestGoal query
    mapM_ validateRequestContext $ requestContexts query
    pure
      ( query
      , DjinnRequestPlan
        { plannedGoal = normalizedGoal }
      ))

-- | Recover the exact neutral query from which this checked request was
-- sealed. Modifications must be passed back through 'mkDjinnRequest'.
djinnRequestQuery
  :: DjinnRequest
  -> QueryRequest DjinnType QueryOptions
djinnRequestQuery (DjinnRequest query) = cachedQueryRequest query

-- | Prepare the complete contextual signature against one sealed session.
-- Known class widths are checked before entering caller-built argument
-- spines. The explicit request contexts are then inserted beneath every
-- leading forall in the goal, and those binders are erased to fresh implicit
-- Djinn variables without conflating shadowed source binders.
--
-- The returned contexts are validation obligations only. Djinn deliberately
-- does not turn class methods into monomorphic proof premises: doing so made
-- inhabitation depend on method-local variable spellings and claimed a form
-- of polymorphic method instantiation that the propositional core cannot
-- represent.
prepareDjinnRequest
  :: (Name -> Maybe Int)
  -> DjinnRequest
  -> Either Diagnostic ([Constraint DjinnType], DjinnType)
prepareDjinnRequest lookupClassArity request =
  first (withDjinnRequestProvenance request) $ do
    let query = djinnRequestQuery request
        plan = djinnRequestPlan request
        contextual = requestContextualType query
          { requestGoal = plannedGoal plan
          }
        (_, combinedContexts, _) =
          SharedType.splitLeadingForalls contextual
    -- 'implicitizeLeadingForalls' collects every variable in the signature.
    -- Bound all context spines first so that collection remains productive
    -- even for adversarial caller-built cyclic argument lists.
    mapM_ validateContextWidth combinedContexts
    implicit <- first signatureLoweringFailure
      $ fmap fst
      $ SharedType.implicitizeLeadingForalls
          (const (Nothing :: Maybe ())) freshVariable mempty contextual
    let (_, embeddedContexts, body) =
          SharedType.splitLeadingForalls implicit
    contexts <- traverse prepareContext embeddedContexts
    goal <- normalizeMonotype RequestGoal body
    pure (contexts, goal)
 where
  freshVariable reserved variable = Just
    $ fst $ freshPrimedVariable reserved variable

  validateContextWidth context = do
    _ <- validateRequestContext context
    let className = constraintClass context
    expected <- maybe (Left $ unknownContextClass className) Right
      $ lookupClassArity className
    validateKnownConstraintArityWith
      (const $ Just expected)
      contextArityFailure
      context

  prepareContext context = do
    validateContextWidth context
    normalized <- traverse
      (normalizeMonotype RequestContextArgument)
      context
    validateRequestContext normalized

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
validateDjinnTarget = validateRequestTarget "DJEX_DJINN_TARGET"
  "Djinn targets must be unqualified value identifiers or operators"

djinnRequestPlan :: DjinnRequest -> DjinnRequestPlan
djinnRequestPlan (DjinnRequest query) = cachedQueryCache query

normalizeRequestType
  :: RequestTypeSite
  -> DjinnType
  -> Either Diagnostic DjinnType
normalizeRequestType site = first
  (loweringFailure $ requestTypeSiteLabel site)
  . Core.normalizeSynthesisSignature

normalizeMonotype
  :: RequestTypeSite
  -> DjinnType
  -> Either Diagnostic DjinnType
normalizeMonotype site = first
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

unknownContextClass :: Name -> Diagnostic
unknownContextClass name = contextualDiagnostic Error
  "DJEX_DJINN_QUERY" "Djinn rejected the query"
  ("Class not found: " ++ renderCanonical name)

contextArityFailure :: Name -> Int -> Int -> Diagnostic
contextArityFailure name expected actual = contextualDiagnostic Error
  "DJEX_DJINN_QUERY" "Djinn rejected the query"
  ( "Class " ++ renderCanonical name ++ " expects " ++ show expected
      ++ " type argument(s), but got " ++ show actual
  )

signatureLoweringFailure
  :: SharedType.BinderNormalizationError String ()
  -> Diagnostic
signatureLoweringFailure = contextualDiagnostic Error
  "DJEX_DJINN_LOWER" "cannot lower the shared query to Djinn"
  . ("signature: " ++) . show
