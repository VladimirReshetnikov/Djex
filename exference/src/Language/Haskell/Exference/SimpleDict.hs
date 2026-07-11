module Language.Haskell.Exference.SimpleDict
  ( emptyClassEnv
  , defaultHeuristicsConfig
  )
where

import qualified Data.Map.Strict as Map

import Language.Haskell.Exference.Core (ExferenceHeuristicsConfig (..))
import Language.Haskell.Exference.Core.Types (StaticClassEnv (..))
import Language.Haskell.Exference.Core.Score (Penalty (..))

emptyClassEnv :: StaticClassEnv
emptyClassEnv = StaticClassEnv
  { sClassEnv_tclasses = []
  , sClassEnv_instances = Map.empty
  }

defaultHeuristicsConfig :: ExferenceHeuristicsConfig
defaultHeuristicsConfig = ExferenceHeuristicsConfig
  { heuristics_goalVar = Penalty 4.0
  , heuristics_goalCons = Penalty 0.55
  , heuristics_goalArrow = Penalty 5.0
  , heuristics_goalApp = Penalty 1.9
  , heuristics_stepProvidedGood = Penalty 0.2
  , heuristics_stepProvidedBad = Penalty 5.0
  , heuristics_stepEnvGood = Penalty 6.0
  , heuristics_stepEnvBad = Penalty 22.0
  , heuristics_tempUnusedVarPenalty = Penalty 5.0
  , heuristics_tempMultiVarUsePenalty = Penalty 3.0
  , heuristics_functionGoalTransform = Penalty 0.0
  , heuristics_unusedVar = Penalty 20.0
  , heuristics_solutionLength = Penalty 0.0153
  }
