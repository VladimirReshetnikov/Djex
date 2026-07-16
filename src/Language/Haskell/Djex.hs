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
-- * "Djinn.Core" is the stable, validated Djinn API.
-- * "Language.Haskell.Djex.Exference" is the checked, parser-neutral adapter.
-- * "Language.Haskell.Exference.Core" is the lower-level search API.
-- * @Language.Haskell.Djex.Exference.HaskellSrc@ is the checked HSE
--   source-loader and type-parser boundary.
-- * @Language.Haskell.Synthesis.*@ modules hold the shared neutral vocabulary.
--
-- Compatibility CLI modules and modules containing @.Internal.@ are not part
-- of this module's stable surface or stability contract.
module Language.Haskell.Djex
  ( module Language.Haskell.Djex.Djinn
  , module Language.Haskell.Djex.Exference
  , module Language.Haskell.Synthesis.Candidate
  , module Language.Haskell.Synthesis.Collection
  , module Language.Haskell.Synthesis.Count
  , module Language.Haskell.Synthesis.Constraint
  , module Language.Haskell.Synthesis.Declaration
  , module Language.Haskell.Synthesis.Diagnostic
  , module Language.Haskell.Synthesis.Environment
  , module Language.Haskell.Synthesis.Fresh
  , module Language.Haskell.Synthesis.Generated
  , module Language.Haskell.Synthesis.Inventory
  , module Language.Haskell.Synthesis.Kind
  , module Language.Haskell.Synthesis.KindInference
  , module Language.Haskell.Synthesis.Name
  , module Language.Haskell.Synthesis.Query
  , module Language.Haskell.Synthesis.Search
  , module Language.Haskell.Synthesis.Selection
  , module Language.Haskell.Synthesis.Type
  , module Language.Haskell.Synthesis.TypeRender
  , module Language.Haskell.Synthesis.TypeSynonym
  , Backend (..)
  , Capability (..)
  , BackendInfo (..)
  , backendInfo
  , availableBackends
  ) where

import Language.Haskell.Djex.Djinn
import Language.Haskell.Djex.Exference
import Language.Haskell.Synthesis.Candidate
import Language.Haskell.Synthesis.Collection
import Language.Haskell.Synthesis.Count
import Language.Haskell.Synthesis.Constraint
import Language.Haskell.Synthesis.Declaration
import Language.Haskell.Synthesis.Diagnostic
import Language.Haskell.Synthesis.Environment
import Language.Haskell.Synthesis.Fresh
import Language.Haskell.Synthesis.Generated
import Language.Haskell.Synthesis.Inventory
import Language.Haskell.Synthesis.Kind
import Language.Haskell.Synthesis.KindInference
import Language.Haskell.Synthesis.Name
import Language.Haskell.Synthesis.Query
import Language.Haskell.Synthesis.Search
import Language.Haskell.Synthesis.Selection
import Language.Haskell.Synthesis.Type
import Language.Haskell.Synthesis.TypeRender
import Language.Haskell.Synthesis.TypeSynonym

-- | A search engine shipped by Djex.
data Backend
  = DjinnBackend
  | ExferenceBackend
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | User-visible distinctions relevant when selecting an engine.  These are
-- deliberately conservative: they describe checked behavior available now,
-- rather than planned convergence features.
data Capability
  = DecidingInhabitation -- ^ Terminating proof search in unbudgeted mode.
  | HeuristicSearch -- ^ Bounded, heuristic exploration of expressions.
  | PrenexPolymorphism -- ^ Polymorphism accepted at the checked search edge.
  | RankedCandidates -- ^ Multiple results carry heuristic rankings.
  | TypeClassConstraints -- ^ Nominal class constraints participate in search.
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
      , TypeClassConstraints
      ]
  }
backendInfo ExferenceBackend = BackendInfo
  { backend = ExferenceBackend
  , backendName = "Exference"
  , backendCapabilities =
      [ HeuristicSearch
      , PrenexPolymorphism
      , RankedCandidates
      , TypeClassConstraints
      ]
  }

-- | All currently available engines, in stable presentation order.
availableBackends :: [BackendInfo]
availableBackends = map backendInfo [minBound .. maxBound]
