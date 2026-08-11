-- |
-- Module      : Language.Haskell.Djex
-- Description : Backend identities and capabilities for Djex.
--
-- This module is the backend-neutral entry point. Checked Djinn and Exference
-- session adapters share its query envelope and the common types needed to
-- inspect, select, diagnose, and render their results. The single @djex@
-- library also exposes the lower-level search and compatibility APIs, while
-- this module remains the curated entry point:
--
-- * "Language.Haskell.Djex.Djinn" is the checked, parser-neutral Djinn
--   adapter.
-- * "Djinn.Core" is the validated historical Djinn compatibility API.
-- * "Language.Haskell.Djex.Exference" is the checked, parser-neutral adapter.
-- * "Language.Haskell.Exference.Core" is the lower-level search API.
-- * "Language.Haskell.Djex.HaskellSrc" parses a checked source type once for
--   either engine; "Language.Haskell.Djex.Exference.HaskellSrc" retains the
--   source-loader and legacy Exference parser facade.
-- * @Language.Haskell.Synthesis.*@ modules hold the shared neutral vocabulary.
--
-- In particular, both adapters use the same @Constraint (Type variable)@
-- representation for class obligations.  Capabilities describe acceptance at
-- a checked backend edge; they do not imply identical evidence semantics.
-- Exference resolves givens, superclasses, and explicit instances.  Djinn
-- validates contexts and supports only dictionary-independent inhabitants,
-- without adding class methods as proof premises.
--
-- Compatibility CLI modules and modules containing @.Internal.@ are not part
-- of this module's stable surface or stability contract.
module Language.Haskell.Djex
  ( module Language.Haskell.Djex.Djinn
  , module Language.Haskell.Djex.Exference
  , module Language.Haskell.Synthesis.Candidate
  , module Language.Haskell.Synthesis.Class
  , module Language.Haskell.Synthesis.Collection
  , module Language.Haskell.Synthesis.Count
  , module Language.Haskell.Synthesis.Constraint
  , module Language.Haskell.Synthesis.Declaration
  , module Language.Haskell.Synthesis.Diagnostic
  , module Language.Haskell.Synthesis.Environment
  , module Language.Haskell.Synthesis.Fingerprint
  , module Language.Haskell.Synthesis.Fresh
  , module Language.Haskell.Synthesis.Generated
  , module Language.Haskell.Synthesis.Inventory
  , module Language.Haskell.Synthesis.Kind
  , module Language.Haskell.Synthesis.KindInference
  , module Language.Haskell.Synthesis.Name
  , module Language.Haskell.Synthesis.Observability
  , module Language.Haskell.Synthesis.Qualification
  , module Language.Haskell.Synthesis.Query
  , module Language.Haskell.Synthesis.Search
  , module Language.Haskell.Synthesis.Selection
  , module Language.Haskell.Synthesis.Semantic.Length
  , module Language.Haskell.Synthesis.Semantic.Length.Evaluate
  , module Language.Haskell.Synthesis.Semantic.Length.Problem
  , module Language.Haskell.Synthesis.Semantic.Length.SMTLib
  , module Language.Haskell.Synthesis.Semantic.Length.SMTLib.Execution
  , module Language.Haskell.Synthesis.Semantic.Length.SMTLib.Observation
  , module Language.Haskell.Synthesis.Semantic.Length.SMTLib.Response
  , module Language.Haskell.Synthesis.Semantic.Observation
  , module Language.Haskell.Synthesis.Semantic.Problem
  , module Language.Haskell.Synthesis.Type
  , module Language.Haskell.Synthesis.TypeAtom
  , module Language.Haskell.Synthesis.TypeInstantiation
  , module Language.Haskell.Synthesis.TypeRender
  , module Language.Haskell.Synthesis.TypeSynonym
  , module Language.Haskell.Synthesis.TypedCandidate
  , module Language.Haskell.Synthesis.TypedGenerated
  , module Language.Haskell.Synthesis.TypedGenerated.Fingerprint
  , Backend (..)
  , Capability (..)
  , BackendInfo (..)
  , backendInfo
  , availableBackends
  ) where

import Language.Haskell.Djex.Djinn
import Language.Haskell.Djex.Exference
import Language.Haskell.Synthesis.Candidate
import Language.Haskell.Synthesis.Class
import Language.Haskell.Synthesis.Collection
import Language.Haskell.Synthesis.Count
import Language.Haskell.Synthesis.Constraint
import Language.Haskell.Synthesis.Declaration
import Language.Haskell.Synthesis.Diagnostic
import Language.Haskell.Synthesis.Environment
import Language.Haskell.Synthesis.Fingerprint
import Language.Haskell.Synthesis.Fresh
import Language.Haskell.Synthesis.Generated
import Language.Haskell.Synthesis.Inventory
import Language.Haskell.Synthesis.Kind
import Language.Haskell.Synthesis.KindInference
import Language.Haskell.Synthesis.Name
import Language.Haskell.Synthesis.Observability
import Language.Haskell.Synthesis.Qualification
import Language.Haskell.Synthesis.Query
import Language.Haskell.Synthesis.Search
import Language.Haskell.Synthesis.Selection
import Language.Haskell.Synthesis.Semantic.Length
import Language.Haskell.Synthesis.Semantic.Length.Evaluate
import Language.Haskell.Synthesis.Semantic.Length.Problem
import Language.Haskell.Synthesis.Semantic.Length.SMTLib
import Language.Haskell.Synthesis.Semantic.Length.SMTLib.Execution
import Language.Haskell.Synthesis.Semantic.Length.SMTLib.Observation
import Language.Haskell.Synthesis.Semantic.Length.SMTLib.Response
import Language.Haskell.Synthesis.Semantic.Observation
import Language.Haskell.Synthesis.Semantic.Problem
import Language.Haskell.Synthesis.Type
import Language.Haskell.Synthesis.TypeAtom
import Language.Haskell.Synthesis.TypeInstantiation
import Language.Haskell.Synthesis.TypeRender
import Language.Haskell.Synthesis.TypeSynonym
import Language.Haskell.Synthesis.TypedCandidate
import Language.Haskell.Synthesis.TypedGenerated
import Language.Haskell.Synthesis.TypedGenerated.Fingerprint

-- | A search engine shipped by Djex.
data Backend
  = DjinnBackend
  | ExferenceBackend
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | User-visible distinctions relevant when selecting an engine.  These are
-- deliberately conservative: they describe checked behavior available now,
-- rather than planned convergence features.
data Capability
  = DecidingInhabitation
    -- ^ Terminating proof search in unbudgeted mode. Accepted types that
    -- retain an opaque higher-rank boundary may still yield no logical
    -- evidence, rather than an unsound refutation.
  | HeuristicSearch -- ^ Bounded, heuristic exploration of expressions.
  | PrenexPolymorphism
    -- ^ Leading binders are accepted at the checked search edge.
  | RankNIntroduction
    -- ^ A quantified type at a supported goal boundary can be constructed by
    -- introducing its binders and synthesizing its body. Exference scopes
    -- direct contexts as lexical givens; Djinn admits only
    -- dictionary-independent bodies.
  | RankNElimination
    -- ^ A quantified local value can be instantiated at a supported
    -- non-quantified use site.
  | OpaqueRankNTypes
    -- ^ Quantified subterms outside an active inference rule are retained as
    -- alpha-aware atoms instead of being rejected or flattened unsafely.
  | ImpredicativeTypes
    -- ^ Ordinary constructors may contain opaque polymorphic arguments.
  | RankedCandidates -- ^ Multiple results carry heuristic rankings.
  | TypeClassConstraints
    -- ^ Shared nominal class constraints are accepted and validated.  An
    -- engine may resolve evidence or require a dictionary-independent result.
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Static metadata for one backend.
data BackendInfo = BackendInfo
  { backend :: Backend
  , backendName :: String
  , backendCapabilities :: [Capability]
  }
  deriving (Eq, Show)

-- | Describe a backend without loading either search engine.
backendInfo :: Backend -> BackendInfo
backendInfo DjinnBackend = BackendInfo
  { backend = DjinnBackend
  , backendName = "Djinn"
  , backendCapabilities =
      [ DecidingInhabitation
      , PrenexPolymorphism
      , RankNIntroduction
      , OpaqueRankNTypes
      , ImpredicativeTypes
      , TypeClassConstraints
      ]
  }
backendInfo ExferenceBackend = BackendInfo
  { backend = ExferenceBackend
  , backendName = "Exference"
  , backendCapabilities =
      [ HeuristicSearch
      , PrenexPolymorphism
      , RankNIntroduction
      , RankNElimination
      , OpaqueRankNTypes
      , ImpredicativeTypes
      , RankedCandidates
      , TypeClassConstraints
      ]
  }

-- | All currently available engines, in stable presentation order.
availableBackends :: [BackendInfo]
availableBackends = map backendInfo [minBound .. maxBound]
