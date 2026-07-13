-- |
-- Module      : Language.Haskell.Djex
-- Description : Backend identities and capabilities for Djex.
--
-- This module is the backend-neutral entry point. Checked Djinn and Exference
-- session adapters share its query envelope and the common types needed to
-- inspect, select, diagnose, and render their results. Lower-level search APIs
-- remain available from explicit named sublibrary dependencies:
--
-- * "Djinn.Core" is the stable, validated Djinn API.
-- * "Language.Haskell.Exference" is the Exference frontend API.
-- * "Language.Haskell.Exference.Core" is its parser-independent search API.
-- * @Language.Haskell.Synthesis.*@ modules hold the shared neutral vocabulary.
--
-- Compatibility CLI modules and modules containing @.Internal.@ are not part
-- of the stable library surface.
module Language.Haskell.Djex
  ( module Language.Haskell.Djex.Djinn
  , module Language.Haskell.Djex.Exference
  , module Language.Haskell.Synthesis.Candidate
  , module Language.Haskell.Synthesis.Constraint
  , module Language.Haskell.Synthesis.Diagnostic
  , module Language.Haskell.Synthesis.Generated
  , module Language.Haskell.Synthesis.Name
  , module Language.Haskell.Synthesis.Query
  , module Language.Haskell.Synthesis.Search
  , module Language.Haskell.Synthesis.Selection
  , module Language.Haskell.Synthesis.Type
  , Backend (..)
  , Capability (..)
  , BackendInfo (..)
  , backendInfo
  , availableBackends
  ) where

import Language.Haskell.Djex.Djinn
import Language.Haskell.Djex.Exference
import Language.Haskell.Synthesis.Candidate
import Language.Haskell.Synthesis.Constraint
import Language.Haskell.Synthesis.Diagnostic
import Language.Haskell.Synthesis.Generated
import Language.Haskell.Synthesis.Name
import Language.Haskell.Synthesis.Query
import Language.Haskell.Synthesis.Search
import Language.Haskell.Synthesis.Selection
import Language.Haskell.Synthesis.Type

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
