-- | Canonical Exference search controls shared by the checked Djex adapter
-- and the raw compatibility core.
--
-- Keeping this representation below both public facades lets a checked
-- request carry one options value all the way into search.  The historical
-- @ExferenceInput@ record remains a compatibility boundary and is projected
-- into this type exactly once.
module Language.Haskell.Exference.Core.Internal.Options
  ( ExferenceHeuristicsConfig (..)
  , defaultHeuristicsConfig
  , ExferenceOptions (..)
  , defaultExferenceOptions
  , heuristicFields
  , heuristicAssignments
  ) where

import Language.Haskell.Exference.Core.Score (Penalty)

-- | Penalties used to rank Exference's heuristic search frontier.
data ExferenceHeuristicsConfig = ExferenceHeuristicsConfig
  { heuristics_goalVar                :: Penalty
  , heuristics_goalCons               :: Penalty
  , heuristics_goalArrow              :: Penalty
  , heuristics_goalApp                :: Penalty
  , heuristics_stepProvidedGood       :: Penalty
  , heuristics_stepProvidedBad        :: Penalty
  , heuristics_stepEnvGood            :: Penalty
  , heuristics_stepEnvBad             :: Penalty
  , heuristics_tempUnusedVarPenalty   :: Penalty
  , heuristics_tempMultiVarUsePenalty :: Penalty
  , heuristics_functionGoalTransform  :: Penalty
  , heuristics_unusedVar              :: Penalty
  , heuristics_solutionLength         :: Penalty
  }
  deriving (Eq, Show)

-- | Parser-neutral heuristic defaults for reusable core and stable queries.
-- The historical command-line frontend intentionally keeps its own ranking
-- profile, which is tuned for that interactive presentation boundary.
defaultHeuristicsConfig :: ExferenceHeuristicsConfig
defaultHeuristicsConfig = ExferenceHeuristicsConfig
  { heuristics_goalVar = 4.0
  , heuristics_goalCons = 0.55
  , heuristics_goalArrow = 5.0
  , heuristics_goalApp = 1.9
  , heuristics_stepProvidedGood = 0.2
  , heuristics_stepProvidedBad = 5.0
  , heuristics_stepEnvGood = 6.0
  , heuristics_stepEnvBad = 22.0
  , heuristics_tempUnusedVarPenalty = 5.0
  , heuristics_tempMultiVarUsePenalty = 3.0
  , heuristics_functionGoalTransform = 0.0
  , heuristics_unusedVar = 20.0
  , heuristics_solutionLength = 0.0153
  }

-- | Query-varying operational controls shared by the stable request and the
-- search core.  Goal types and binding exclusions live in @ExferenceQuery@
-- because they are not reusable search policy.
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

-- | Parser-neutral limits and search policy used by checked requests.
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

-- | Every heuristic weight paired with its field name (without the
-- @heuristics_@ prefix), in declaration order; option validation reports the
-- first non-finite weight under that name.
-- Keep names beside the record that owns them, so validation cannot silently
-- drift when a heuristic field is added.
heuristicFields :: ExferenceHeuristicsConfig -> [(String, Penalty)]
heuristicFields config =
  [ ("goalVar", heuristics_goalVar config)
  , ("goalCons", heuristics_goalCons config)
  , ("goalArrow", heuristics_goalArrow config)
  , ("goalApp", heuristics_goalApp config)
  , ("stepProvidedGood", heuristics_stepProvidedGood config)
  , ("stepProvidedBad", heuristics_stepProvidedBad config)
  , ("stepEnvGood", heuristics_stepEnvGood config)
  , ("stepEnvBad", heuristics_stepEnvBad config)
  , ("tempUnusedVarPenalty", heuristics_tempUnusedVarPenalty config)
  , ("tempMultiVarUsePenalty", heuristics_tempMultiVarUsePenalty config)
  , ("functionGoalTransform", heuristics_functionGoalTransform config)
  , ("unusedVar", heuristics_unusedVar config)
  , ("solutionLength", heuristics_solutionLength config)
  ]

-- | Every heuristic weight paired with its field name and an assignment, in
-- the declaration order of 'heuristicFields'; a frontend that sets one weight
-- by name resolves it here.
-- Keep this beside 'heuristicFields' so a heuristic field cannot gain a
-- reader without also gaining a writer.
heuristicAssignments
  :: [( String
      , Penalty -> ExferenceHeuristicsConfig -> ExferenceHeuristicsConfig )]
heuristicAssignments =
  [ ("goalVar", \value config -> config {heuristics_goalVar = value})
  , ("goalCons", \value config -> config {heuristics_goalCons = value})
  , ("goalArrow", \value config -> config {heuristics_goalArrow = value})
  , ("goalApp", \value config -> config {heuristics_goalApp = value})
  , ("stepProvidedGood"
    , \value config -> config {heuristics_stepProvidedGood = value})
  , ("stepProvidedBad"
    , \value config -> config {heuristics_stepProvidedBad = value})
  , ("stepEnvGood", \value config -> config {heuristics_stepEnvGood = value})
  , ("stepEnvBad", \value config -> config {heuristics_stepEnvBad = value})
  , ("tempUnusedVarPenalty"
    , \value config -> config {heuristics_tempUnusedVarPenalty = value})
  , ("tempMultiVarUsePenalty"
    , \value config -> config {heuristics_tempMultiVarUsePenalty = value})
  , ("functionGoalTransform"
    , \value config -> config {heuristics_functionGoalTransform = value})
  , ("unusedVar", \value config -> config {heuristics_unusedVar = value})
  , ("solutionLength"
    , \value config -> config {heuristics_solutionLength = value})
  ]
