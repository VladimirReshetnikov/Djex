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
  , requestContextualGoal
  , requestSourceTypeVariableHints
  , withExferenceRequestProvenance
  , validateExferenceTarget
  ) where

import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Language.Haskell.Exference.Core
  ( ExferenceHeuristicsConfig
  , ExferenceSourceTypeVariableHintError
  , ExferenceSourceTypeVariableHints
  , Penalty
  , mkExferenceSourceTypeVariableHints
  )
import Language.Haskell.Exference.Core.Types (toSynthesisType)
import Language.Haskell.Exference.SimpleDict (defaultHeuristicsConfig)
import Language.Haskell.Synthesis.Constraint (constraintArguments)
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , SourceLocation
  , shownErrorDiagnostic
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
  , cachedQueryCache
  , cachedQueryRequest
  , sealCachedQueryWithProvenance
  , requestContextualType
  , withCachedQueryProvenance
  )
import qualified Language.Haskell.Synthesis.Type as SharedType
import Language.Haskell.Synthesis.Type (Type)

data ExferenceOptions = ExferenceOptions
  { exferenceAllowUnused :: Bool
  , exferenceAllowResidualConstraints :: Bool
  , exferenceConstraintDeferralSteps :: Int
  , exferenceMultiConstructorPatterns :: Bool
  , exferenceMaximumSteps :: Int
  , exferenceMaximumQueueSize :: Maybe Int
  , exferenceMaximumDepth :: Maybe Penalty
  , exferenceHeuristics :: ExferenceHeuristicsConfig
  }
  deriving (Eq, Show)

defaultExferenceOptions :: ExferenceOptions
defaultExferenceOptions = ExferenceOptions
  { exferenceAllowUnused = False
  , exferenceAllowResidualConstraints = False
  , exferenceConstraintDeferralSteps = 8192
  , exferenceMultiConstructorPatterns = False
  , exferenceMaximumSteps = 65536
  , exferenceMaximumQueueSize = Just 8192
  , exferenceMaximumDepth = Nothing
  , exferenceHeuristics = defaultHeuristicsConfig
  }

-- | Generated-expression binder identities used by Exference.
type ExferenceLocal = Int

-- | Shared flexible/rigid source-type variables used by Exference.
type ExferenceTypeVariable = SharedType.Variable ExferenceLocal

-- | Exference's checked type surface, expressed entirely in the neutral IR.
type ExferenceType = Type ExferenceTypeVariable

-- Checked source spellings are a deterministic presentation cache, not part
-- of the stable request value. Location provenance is owned separately by the
-- shared envelope, which gives both adapters the same query-only equality and
-- display contract.
newtype ExferenceRequest = ExferenceRequest
  (CachedQuery
    ExferenceType
    ExferenceOptions
    ExferenceSourceTypeVariableHints)
  deriving (Eq, Show)
    via (CachedQuery
      ExferenceType
      ExferenceOptions
      ExferenceSourceTypeVariableHints)

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
    canonicalQuery <- normalizeRequest query
    validateRequest canonicalQuery
    let contextualGoal = requestContextualType canonicalQuery
    sourceHints <- first sourceHintFailure
      $ mkExferenceSourceTypeVariableHints contextualGoal sourceVariables
    pure (canonicalQuery, sourceHints))

-- Store exactly the canonical native representation that the checked
-- Exference core consumes.  Normalizing each context argument separately
-- also rejects rigid forall binders before contexts are inserted into the
-- contextual goal by the shared query envelope.
normalizeRequest
  :: QueryRequest ExferenceType ExferenceOptions
  -> Either Diagnostic (QueryRequest ExferenceType ExferenceOptions)
normalizeRequest query = do
  goal <- normalizeType $ requestGoal query
  contexts <- traverse (traverse normalizeType) $ requestContexts query
  pure query {requestGoal = goal, requestContexts = contexts}
 where
  normalizeType source = either (Left . invalidRequest) Right
    $ toSynthesisType source

exferenceRequestQuery
  :: ExferenceRequest
  -> QueryRequest ExferenceType ExferenceOptions
exferenceRequestQuery (ExferenceRequest query) = cachedQueryRequest query

requestSourceTypeVariableHints
  :: ExferenceRequest
  -> ExferenceSourceTypeVariableHints
requestSourceTypeVariableHints (ExferenceRequest query) =
  cachedQueryCache query

withExferenceRequestProvenance
  :: ExferenceRequest
  -> Diagnostic
  -> Diagnostic
withExferenceRequestProvenance (ExferenceRequest query) =
  withCachedQueryProvenance query

requestContextualGoal :: ExferenceRequest -> ExferenceType
requestContextualGoal = requestContextualType . exferenceRequestQuery

validateRequest
  :: QueryRequest ExferenceType ExferenceOptions
  -> Either Diagnostic ()
validateRequest query = do
  either
    (Left . invalidRequest)
    Right
    $ SharedType.validateType $ requestContextualType query
  let goalVariables = inScopeContextVariables $ requestGoal query
      contextVariables = Set.unions
        [ SharedType.freeVariables argument
        | constraint <- requestContexts query
        , argument <- constraintArguments constraint
        ]
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

-- | Check the source-level name of an Exference result definition.
-- Frontends use this before parsing so command-usage errors retain precedence
-- over malformed source text, and retain the checked value in the request.
validateExferenceTarget :: Name -> Either Diagnostic DefinitionName
validateExferenceTarget target = case mkDefinitionName target of
  Right checked -> Right checked
  Left failure -> Left $ shownErrorDiagnostic "DJEX_EXF_TARGET"
    "Exference targets must be unqualified value identifiers or operators"
    (renderCanonical target, failure)
