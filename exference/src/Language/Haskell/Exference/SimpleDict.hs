module Language.Haskell.Exference.SimpleDict
  ( emptyClassEnv
  , defaultHeuristicsConfig
  )
where

import qualified Data.Map.Strict as Map

import Language.Haskell.Exference.Core (ExferenceHeuristicsConfig (..))
import Language.Haskell.Exference.Core.Types (StaticClassEnv (..))

emptyClassEnv :: StaticClassEnv
emptyClassEnv = StaticClassEnv
  { sClassEnv_tclasses = []
  , sClassEnv_instances = Map.empty
  }

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
