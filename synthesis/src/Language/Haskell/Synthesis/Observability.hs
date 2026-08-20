{-# LANGUAGE DeriveTraversable #-}

-- | Exact, deterministic observations shared across synthesis layers.
--
-- Search engines, renderers, and proof frontends need a common vocabulary for
-- measuring semantic work without changing the records that already carry
-- backend-specific details.  The counter representation is intentionally
-- opaque: every stored entry is positive, counts are exact 'Natural' values,
-- and inspection always uses ascending key order.  These properties make
-- snapshots suitable for reproducible benchmarks and future solver traces.
module Language.Haskell.Synthesis.Observability
  ( ObservationCounts
  , noObservations
  , recordObservation
  , recordObservations
  , observationCount
  , observationEntries
  , mapObservationKeys
  , SynthesisMetric (..)
  , synthesisMetricCode
  , ObservationSnapshot
  , observationSnapshot
  , snapshotObservations
  , snapshotValue
  ) where

import Control.DeepSeq (NFData (rnf))
import qualified Data.Map.Strict as Map
import Numeric.Natural (Natural)

-- | A finite map of positive, exact observation counts.
--
-- The constructor stays private so absent and zero-valued entries have one
-- canonical representation.  In particular, adding zero never creates a key.
newtype ObservationCounts key = ObservationCounts (Map.Map key Natural)
  deriving (Eq, Ord, Show)

instance NFData key => NFData (ObservationCounts key) where
  rnf (ObservationCounts counts) = rnf counts

-- | An observation set containing no entries.
noObservations :: ObservationCounts key
noObservations = ObservationCounts Map.empty

-- | Increment one observation by one.
recordObservation
  :: Ord key
  => key
  -> ObservationCounts key
  -> ObservationCounts key
recordObservation key = recordObservations key 1

-- | Add an exact number of observations for one key.
--
-- A zero addition is a no-op and therefore preserves the canonical absence of
-- zero-valued entries.  'Natural' addition cannot wrap at a machine-word
-- boundary.
recordObservations
  :: Ord key
  => key
  -> Natural
  -> ObservationCounts key
  -> ObservationCounts key
recordObservations _ 0 counts = counts
recordObservations key additions (ObservationCounts counts) =
  ObservationCounts $ Map.insertWith (+) key additions counts

-- | Return the exact count for one key, or zero when it was never observed.
observationCount
  :: Ord key
  => key
  -> ObservationCounts key
  -> Natural
observationCount key (ObservationCounts counts) =
  Map.findWithDefault 0 key counts

-- | Enumerate positive observations in ascending key order.
observationEntries :: ObservationCounts key -> [(key, Natural)]
observationEntries (ObservationCounts counts) = Map.toAscList counts

-- | Change the observation vocabulary, summing keys that map together.
--
-- Because all source entries are positive, mapped results retain the same
-- canonical representation even when several source keys collide.
mapObservationKeys
  :: Ord target
  => (source -> target)
  -> ObservationCounts source
  -> ObservationCounts target
mapObservationKeys project (ObservationCounts counts) = ObservationCounts
  $ Map.fromListWith (+)
  [ (project key, count)
  | (key, count) <- Map.toAscList counts
  ]

instance Ord key => Semigroup (ObservationCounts key) where
  ObservationCounts left <> ObservationCounts right =
    ObservationCounts $ Map.unionWith (+) left right

instance Ord key => Monoid (ObservationCounts key) where
  mempty = noObservations

-- | Stable shared counters for the first observability baseline.
--
-- This vocabulary is deliberately fixed and backend-neutral.  More detailed
-- engine, renderer, verifier, or solver observations can use their own key
-- type and later be projected into an enclosing snapshot.
data SynthesisMetric
  = RankNPlanProposed
  | RankNPlanCompiled
  | FormulaFingerprintRetained
  | PreparedPremiseFingerprintRetained
  | PreparedPremiseCacheHit
  | EngineStateVisited
  | RawDerivationProduced
  | RenderedGroupProduced
  | SemanticDuplicateDiscarded
  | ExactDuplicateDiscarded
  deriving (Bounded, Enum, Eq, Ord, Show)

instance NFData SynthesisMetric where
  rnf metric = metric `seq` ()

-- | Stable machine-readable spelling of a shared synthesis metric.
--
-- These codes are an interchange contract for benchmark output.  Constructor
-- names may be pleasant Haskell vocabulary, but serialized observations
-- should use this explicit mapping rather than deriving text from 'Show'.
synthesisMetricCode :: SynthesisMetric -> String
synthesisMetricCode metric = case metric of
  RankNPlanProposed -> "rank-n-plan-proposed"
  RankNPlanCompiled -> "rank-n-plan-compiled"
  FormulaFingerprintRetained -> "formula-fingerprint-retained"
  PreparedPremiseFingerprintRetained ->
    "prepared-premise-fingerprint-retained"
  PreparedPremiseCacheHit -> "prepared-premise-cache-hit"
  EngineStateVisited -> "engine-state-visited"
  RawDerivationProduced -> "raw-derivation-produced"
  RenderedGroupProduced -> "rendered-group-produced"
  SemanticDuplicateDiscarded -> "semantic-duplicate-discarded"
  ExactDuplicateDiscarded -> "exact-duplicate-discarded"

-- | A value paired with the observations accumulated while producing it.
--
-- Both fields are deliberately non-strict.  A consumer can inspect the value
-- without evaluating metrics, or inspect metrics without evaluating a result
-- that may be expensive or partial.  'NFData' remains available when a caller
-- explicitly wants to force the complete snapshot.
data ObservationSnapshot value = ObservationSnapshot
  (ObservationCounts SynthesisMetric)
  value
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance NFData value => NFData (ObservationSnapshot value) where
  rnf (ObservationSnapshot observations value) =
    rnf observations `seq` rnf value

-- | Pair shared observations with a result without forcing either field.
observationSnapshot
  :: ObservationCounts SynthesisMetric
  -> value
  -> ObservationSnapshot value
observationSnapshot = ObservationSnapshot

-- | Recover the shared observations from a snapshot.
snapshotObservations
  :: ObservationSnapshot value
  -> ObservationCounts SynthesisMetric
snapshotObservations (ObservationSnapshot observations _) = observations

-- | Recover the result from a snapshot.
snapshotValue :: ObservationSnapshot value -> value
snapshotValue (ObservationSnapshot _ value) = value
