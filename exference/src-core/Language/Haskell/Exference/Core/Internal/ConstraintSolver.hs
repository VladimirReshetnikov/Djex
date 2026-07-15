module Language.Haskell.Exference.Core.Internal.ConstraintSolver
  ( filterUnresolved
  , isPossible
  )
where

import Control.Monad (guard)
import Data.List (minimumBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Language.Haskell.Exference.Core.Internal.Unify
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.Core.Types
import qualified Language.Haskell.Synthesis.Collection as SharedCollection

-- | Reject refutable constraints and retain constraints whose variables make
-- them undecidable at the current search node.
isPossible :: QueryClassEnv -> [HsConstraint] -> Maybe [HsConstraint]
isPossible = checkConstraints (Just . pure) (const Nothing)

-- | Discharge every ground constraint that can be proven. Variable-bearing
-- constraints are not valid final obligations, while ground constraints with
-- no matching evidence remain explicit in the result.
filterUnresolved :: QueryClassEnv -> [HsConstraint] -> Maybe [HsConstraint]
filterUnresolved = checkConstraints (const Nothing) (Just . pure)

checkConstraints
  :: (HsConstraint -> Maybe [HsConstraint])
  -> (HsConstraint -> Maybe [HsConstraint])
  -> QueryClassEnv
  -> [HsConstraint]
  -> Maybe [HsConstraint]
checkConstraints variableResult noEvidenceResult environment = solve Set.empty
  where
    solve visiting = fmap concat . traverse (resolve visiting)

    resolve visiting constraint
      | constraintContainsVariables constraint = variableResult constraint
      | constraint `Set.member` qClassEnv_inflatedConstraints environment = Just []
      | constraint `Set.member` visiting = Just [constraint]
      | otherwise =
          SharedCollection.firstPresent
            [ bestResult
                $ map (fromInstance $ Set.insert constraint visiting) instances
            , noEvidenceResult constraint
            ]
      where
        HsConstraint className parameters = constraint
        instances = Map.findWithDefault []
          className
          (sClassEnv_instances $ qClassEnv_env environment)

        fromInstance nextVisiting
            (HsInstance prerequisites (HsConstraint instanceClass instanceParameters)) = do
          guard $ className == instanceClass
          guard $ length parameters == length instanceParameters
          substitutions <- unifyRightEqs $ zipWith TypeEq parameters instanceParameters
          solve nextVisiting
            $ map (snd . constraintApplySubsts substitutions) prerequisites

bestResult :: [Maybe [a]] -> Maybe [a]
bestResult results = case catMaybes results of
  [] -> Nothing
  candidates -> Just $ minimumBy (comparing length) candidates
