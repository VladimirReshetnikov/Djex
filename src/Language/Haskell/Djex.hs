-- |
-- Module      : Language.Haskell.Djex
-- Description : Backend identities and capabilities for Djex.
--
-- This module is the small backend-neutral entry point. Checked Djinn and
-- Exference session adapters share its query envelope, while lower-level
-- search APIs remain available through their established imports:
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
  , module Language.Haskell.Synthesis.Query
  , Backend (..)
  , Capability (..)
  , BackendInfo (..)
  , backendInfo
  , availableBackends
  ) where

import Language.Haskell.Djex.Djinn
import Language.Haskell.Djex.Exference
import Language.Haskell.Synthesis.Query

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
