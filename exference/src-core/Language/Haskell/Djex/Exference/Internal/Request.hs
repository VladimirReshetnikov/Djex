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
  , requestSourceTypeVariables
  , requestSourceLocation
  , validateExferenceTarget
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Language.Haskell.Exference.Core
  ( ExferenceHeuristicsConfig
  , Penalty
  )
import Language.Haskell.Exference.SimpleDict (defaultHeuristicsConfig)
import Language.Haskell.Synthesis.Constraint (constraintArguments)
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error)
  , SourceSpan
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
  , cachedQueryCache
  , cachedQueryRequest
  , mkCachedQuery
  , requestContextualType
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

-- Keep the frontend caches private: they are meaningful only when paired with
-- the exact parsed goal. In particular, the spelling index must be converted
-- after explicit contexts are merged, because that operation can change
-- Exference's rigid-ID allocation.
data ExferenceRequestCache = ExferenceRequestCache
  { cachedSourceTypeVariables :: Map.Map String ExferenceLocal
  , cachedSourceLocation :: Maybe (FilePath, SourceSpan)
  }

-- Source spellings and locations are deterministic presentation caches, not
-- part of the stable request value. The shared envelope gives both adapters
-- the same query-only equality and display contract.
newtype ExferenceRequest = ExferenceRequest
  (CachedQuery ExferenceType ExferenceOptions ExferenceRequestCache)
  deriving (Eq, Show)
    via (CachedQuery ExferenceType ExferenceOptions ExferenceRequestCache)

mkExferenceRequest
  :: QueryRequest ExferenceType ExferenceOptions
  -> Either Diagnostic ExferenceRequest
mkExferenceRequest = mkExferenceRequestWithSourceInfo Map.empty Nothing

-- | Construct a checked request with parser-neutral source caches. This
-- internal operation is the only provenance entry point: callers cannot
-- rewrite the caches of an already validated request.
mkExferenceRequestWithSourceInfo
  :: Map.Map String ExferenceLocal
  -> Maybe (FilePath, SourceSpan)
  -> QueryRequest ExferenceType ExferenceOptions
  -> Either Diagnostic ExferenceRequest
mkExferenceRequestWithSourceInfo sourceVariables sourceLocation query = do
  validateRequest query
  pure $ ExferenceRequest $ mkCachedQuery query ExferenceRequestCache
    { cachedSourceTypeVariables = sourceVariables
    , cachedSourceLocation = sourceLocation
    }

exferenceRequestQuery
  :: ExferenceRequest
  -> QueryRequest ExferenceType ExferenceOptions
exferenceRequestQuery (ExferenceRequest query) = cachedQueryRequest query

requestSourceTypeVariables
  :: ExferenceRequest
  -> Map.Map String ExferenceLocal
requestSourceTypeVariables = cachedSourceTypeVariables . exferenceRequestCache

requestSourceLocation
  :: ExferenceRequest
  -> Maybe (FilePath, SourceSpan)
requestSourceLocation = cachedSourceLocation . exferenceRequestCache

exferenceRequestCache :: ExferenceRequest -> ExferenceRequestCache
exferenceRequestCache (ExferenceRequest query) = cachedQueryCache query

requestContextualGoal :: ExferenceRequest -> ExferenceType
requestContextualGoal = requestContextualType . exferenceRequestQuery

validateRequest
  :: QueryRequest ExferenceType ExferenceOptions
  -> Either Diagnostic ()
validateRequest query = do
  either
    (Left . failureDiagnostic
      "DJEX_EXF_REQUEST"
      "invalid shared Exference request"
    )
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
    _ -> Left $ failureDiagnostic
      "DJEX_EXF_REQUEST"
      "explicit Exference contexts contain variables not in scope"
      extraneous

-- Explicit contexts are inserted beneath only the leading prenex chain.
-- Free goal variables remain usable there, as do binders from that chain;
-- a binder below an arrow, tuple, or application is not in context scope.
inScopeContextVariables
  :: ExferenceType
  -> Set.Set ExferenceTypeVariable
inScopeContextVariables goal = SharedType.freeVariables goal
  `Set.union` leadingForallVariables goal
 where
  leadingForallVariables (SharedType.ForallType variables _ body) =
    Set.fromList variables `Set.union` leadingForallVariables body
  leadingForallVariables _ = Set.empty

-- | Check the source-level name of an Exference result definition.
-- Frontends use this before parsing so command-usage errors retain precedence
-- over malformed source text, and retain the checked value in the request.
validateExferenceTarget :: Name -> Either Diagnostic DefinitionName
validateExferenceTarget target = case mkDefinitionName target of
  Right checked -> Right checked
  Left failure -> Left $ contextualDiagnostic Error "DJEX_EXF_TARGET"
    "Exference targets must be unqualified value identifiers or operators"
    (show (renderCanonical target, failure))

failureDiagnostic
  :: Show detail
  => String
  -> String
  -> detail
  -> Diagnostic
failureDiagnostic code message detail =
  contextualDiagnostic Error code message (show detail)
