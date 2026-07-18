{-# LANGUAGE DerivingVia #-}

{-# OPTIONS_HADDOCK not-home #-}

-- | Hidden request representation shared with Exference source frontends.
--
-- The stable adapter re-exports only the opaque request type, its checked
-- constructor, and its neutral query projection. This internal module owns
-- the frontend provenance needed for source diagnostics and rendering.
module Language.Haskell.Djex.Exference.Internal.Request
  ( ExferenceOptions (..)
  , defaultExferenceOptions
  , ExferenceRequest
  , ExferenceLocal
  , ExferenceTypeVariable
  , ExferenceType
  , mkExferenceRequest
  , mkExferenceRequestWithSourceInfo
  , exferenceRequestQuery
  , prepareExferenceRequestContexts
  , withExferenceRequestProvenance
  , validateExferenceTarget
  ) where

import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Language.Haskell.Exference.Core.Internal.Options
  ( ExferenceOptions (..)
  , defaultExferenceOptions
  )
import Language.Haskell.Exference.Core.Types (toSynthesisType)
import Language.Haskell.Exference.Core.Internal.Candidate
  ( ExferenceSourceTypeVariableHintError
  , ExferenceSourceTypeVariableHints
  , ExferenceSourceTypeVariableNames
  , bindExferenceSourceTypeVariableHints
  , mkExferenceSourceTypeVariableNames
  )
import qualified Language.Haskell.Synthesis.Constraint as SharedConstraint
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error)
  , SourceLocation
  , contextualDiagnostic
  , shownErrorDiagnostic
  )
import Language.Haskell.Synthesis.Generated
  ( DefinitionName )
import Language.Haskell.Synthesis.Name
  ( Name )
import Language.Haskell.Synthesis.Query
  ( CachedQuery
  , QueryRequest (..)
  , RequestProvenance (..)
  , RequestTypeSite (..)
  , cachedQueryCache
  , cachedQueryRequest
  , requestTypeSiteLabel
  , sealCachedQueryWithProvenance
  , requestContextualType
  , traverseRequestContextsWithKnownArity
  , validateRequestTarget
  , withCachedQueryProvenance
  )
import qualified Language.Haskell.Synthesis.Type as SharedType
import Language.Haskell.Synthesis.Type (Type)
import Language.Haskell.Synthesis.KindInference
  ( KindInferenceError (ClassArityMismatch) )
import Language.Haskell.Synthesis.TypeSynonym
  ( ElaborationPhase (BeforeExpansion)
  , TypeElaborationError (IllKindedType)
  )

-- | Generated-expression binder identities used by Exference.
type ExferenceLocal = Int

-- | Shared flexible/rigid source-type variables used by Exference.
type ExferenceTypeVariable = SharedType.Variable ExferenceLocal

-- | Exference's checked type surface, expressed entirely in the neutral IR.
type ExferenceType = Type ExferenceTypeVariable

-- | An opaque, validated Exference request. Equality and display observe the
-- caller's exact neutral query; normalized goals, source-name hints, and
-- diagnostic provenance remain private execution data.
--
-- Checked source spellings are a deterministic presentation cache, not part
-- of the stable request value. Location provenance is owned separately by the
-- shared envelope, which gives both adapters the same query-only equality and
-- display contract.
data ExferenceRequestPlan = ExferenceRequestPlan
  { plannedGoal :: ExferenceType
  , plannedSourceTypeVariableNames :: ExferenceSourceTypeVariableNames
  }

newtype ExferenceRequest = ExferenceRequest
  (CachedQuery ExferenceType ExferenceOptions ExferenceRequestPlan)
  deriving (Eq, Show)
    via (CachedQuery ExferenceType ExferenceOptions ExferenceRequestPlan)

-- | Validate and seal a programmatic request. The goal and every context
-- class name are checked in request order, and any diagnostic is
-- intentionally source-less. Context argument spines remain deferred until a
-- session can supply their known class arities.
mkExferenceRequest
  :: QueryRequest ExferenceType ExferenceOptions
  -> Either Diagnostic ExferenceRequest
mkExferenceRequest = mkExferenceRequestWithProvenance
  Map.empty ProgrammaticRequest

-- | Construct a checked request with parser-neutral rendering hints and
-- source provenance. This internal operation is the only such entry point:
-- callers cannot rewrite either after validation.
mkExferenceRequestWithSourceInfo
  :: Map.Map String ExferenceLocal
  -> SourceLocation
  -> QueryRequest ExferenceType ExferenceOptions
  -> Either Diagnostic ExferenceRequest
mkExferenceRequestWithSourceInfo sourceVariables location =
  mkExferenceRequestWithProvenance sourceVariables
    $ SourceRequest location

mkExferenceRequestWithProvenance
  :: Map.Map String ExferenceLocal
  -> RequestProvenance
  -> QueryRequest ExferenceType ExferenceOptions
  -> Either Diagnostic ExferenceRequest
mkExferenceRequestWithProvenance sourceVariables provenance query =
  ExferenceRequest <$> sealCachedQueryWithProvenance provenance (do
    canonicalGoal <- normalizeRequestType RequestGoal $ requestGoal query
    mapM_ validateRequestConstraint $ requestContexts query
    sourceNames <- first sourceHintFailure
      $ mkExferenceSourceTypeVariableNames sourceVariables
    -- Publish the caller's exact neutral request while retaining only the
    -- canonical goal and detached checked spellings. This matches Djinn's
    -- exact-request/private-plan contract: equality and display describe the
    -- supplied request, whereas execution consumes only cache data derived
    -- from it.
    pure (query, ExferenceRequestPlan canonicalGoal sourceNames))

-- Store exactly the canonical native representation that the checked
-- Exference core consumes. Context arguments take this same path later, once
-- their owning session has bounded each class application.
normalizeRequestType
  :: RequestTypeSite
  -> ExferenceType
  -> Either Diagnostic ExferenceType
normalizeRequestType site = first (invalidRequestType site) . toSynthesisType

-- Validate the nominal header without inspecting its argument spine. The
-- same check finishes a session-bounded normalized constraint before the next
-- context is entered.
validateRequestConstraint
  :: SharedConstraint.Constraint ExferenceType
  -> Either Diagnostic (SharedConstraint.Constraint ExferenceType)
validateRequestConstraint constraint = case
    SharedConstraint.validateConstraint constraint of
  Left failure -> Left $ invalidRequest failure
  Right () -> Right constraint

invalidRequestType
  :: Show failure
  => RequestTypeSite
  -> failure
  -> Diagnostic
invalidRequestType site failure = contextualDiagnostic Error
  "DJEX_EXF_REQUEST" "invalid shared Exference request"
  (requestTypeSiteLabel site ++ ": " ++ show failure)

-- | Recover the exact neutral query supplied when the request was sealed.
exferenceRequestQuery
  :: ExferenceRequest
  -> QueryRequest ExferenceType ExferenceOptions
exferenceRequestQuery (ExferenceRequest query) = cachedQueryRequest query

withExferenceRequestProvenance
  :: ExferenceRequest
  -> Diagnostic
  -> Diagnostic
withExferenceRequestProvenance (ExferenceRequest query) =
  withCachedQueryProvenance query

-- | Normalize and scope-check the deferred context arguments against the
-- class arities known by one session, then bind detached source spellings to
-- the resulting contextual goal.
--
-- Known arity is checked before entering an argument spine. An over-applied
-- or cyclic list therefore produces the same kind diagnostic after a bounded
-- observation, while finite unknown external constraints retain Exference's
-- open-world elaboration policy.
prepareExferenceRequestContexts
  :: (Name -> Maybe Int)
  -> ExferenceRequest
  -> Either Diagnostic
       (ExferenceType, ExferenceSourceTypeVariableHints)
prepareExferenceRequestContexts lookupClassArity request =
  first (withExferenceRequestProvenance request) $ do
    let query = exferenceRequestQuery request
        plan = exferenceRequestPlan request
    contexts <- traverseRequestContextsWithKnownArity
      lookupClassArity contextArityFailure normalizeRequestType
      validateRequestConstraint $ requestContexts query
    let canonicalQuery = query
          { requestGoal = plannedGoal plan
          , requestContexts = contexts
          }
    validateRequest canonicalQuery
    let contextualGoal = requestContextualType canonicalQuery
    sourceHints <- first sourceHintFailure
      $ bindExferenceSourceTypeVariableHints contextualGoal
      $ plannedSourceTypeVariableNames plan
    pure (contextualGoal, sourceHints)

contextArityFailure :: Name -> Int -> Int -> Diagnostic
contextArityFailure name expected actual = shownErrorDiagnostic
  "DJEX_EXF_KIND" "Exference rejected the query kind"
  (IllKindedType BeforeExpansion
    (ClassArityMismatch name expected actual)
      :: TypeElaborationError ExferenceTypeVariable)

validateRequest
  :: QueryRequest ExferenceType ExferenceOptions
  -> Either Diagnostic ()
validateRequest query = do
  let goalVariables = inScopeContextVariables $ requestGoal query
      contextVariables = foldMap SharedType.constraintFreeVariables
        $ requestContexts query
      extraneous = Set.toAscList $ contextVariables Set.\\ goalVariables
  case extraneous of
    [] -> Right ()
    _ -> Left $ shownErrorDiagnostic
      "DJEX_EXF_REQUEST"
      "explicit Exference contexts contain variables not in scope"
      extraneous

invalidRequest :: Show failure => failure -> Diagnostic
invalidRequest = shownErrorDiagnostic
  "DJEX_EXF_REQUEST"
  "invalid shared Exference request"

sourceHintFailure
  :: ExferenceSourceTypeVariableHintError
  -> Diagnostic
sourceHintFailure = shownErrorDiagnostic
  "DJEX_EXF_SOURCE_HINT"
  "invalid Exference source type-variable rendering hint"

-- Explicit contexts are inserted beneath only the leading prenex chain.
-- Free goal variables remain usable there, as do binders from that chain;
-- a binder below an arrow, tuple, or application is not in context scope.
inScopeContextVariables
  :: ExferenceType
  -> Set.Set ExferenceTypeVariable
inScopeContextVariables goal = SharedType.freeVariables goal
  `Set.union` Set.fromList (SharedType.leadingForallVariables goal)

exferenceRequestPlan :: ExferenceRequest -> ExferenceRequestPlan
exferenceRequestPlan (ExferenceRequest query) = cachedQueryCache query

-- | Check the source-level name of an Exference result definition.
-- Frontends use this before parsing so command-usage errors retain precedence
-- over malformed source text, and retain the checked value in the request.
validateExferenceTarget :: Name -> Either Diagnostic DefinitionName
validateExferenceTarget = validateRequestTarget "DJEX_EXF_TARGET"
  "Exference targets must be unqualified value identifiers or operators"
