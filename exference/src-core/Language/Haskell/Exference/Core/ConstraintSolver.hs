-- | Exference's canonical class-constraint discharge operations.
--
-- Search and the independent expression checker consume this same public
-- implementation, preventing their ground-evidence policy from drifting.
module Language.Haskell.Exference.Core.ConstraintSolver
  ( filterUnresolved
  , isPossible
  , uniqueGivenInstantiation
  )
where

import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (minimumBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Language.Haskell.Exference.Core.Unify
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.Core.Types
import qualified Language.Haskell.Synthesis.Collection as SharedCollection
import qualified Language.Haskell.Synthesis.Type as SharedType
import qualified Language.Haskell.Synthesis.TypeAtom as SharedTypeAtom

-- | Infer a unique coherent assignment to fresh provider variables from
-- lexical Givens. Each obligation carries its own lexical snapshot. No class
-- instances or superclass closure participate, and variables on the Given
-- side (or outside the caller's fresh set) cannot be assigned. This produces
-- type selections only: exact dictionary discharge still runs independently.
--
-- A finite work guard bounds matching attempts. Exhaustion, no solution and
-- multiple distinct solutions all return Nothing; in particular, reaching the
-- guard after finding one solution does not certify that solution as unique.
-- Callers must retain their ordinary unresolved-constraint behavior on Nothing.
uniqueGivenInstantiation
  :: Int -> IntSet.IntSet -> [([HsConstraint], HsConstraint)] -> Maybe Substs
uniqueGivenInstantiation limit fresh obligations = do
  solutions <- evalStateT (collect obligations [] []) limit
  case solutions of
    [selected] -> Just selected
    _ -> Nothing
 where
  givenVariables = IntSet.fromList $ Set.toList $ foldMap
    (foldMap (foldMap freeVars . constraint_params) . fst) obligations
  eligible = fresh `IntSet.difference` givenVariables

  collect :: [([HsConstraint], HsConstraint)] -> [TypeEq] -> [Substs]
    -> StateT Int Maybe [Substs]
  collect [] equations found = do
    selected <- lift $ match equations
    pure $ if any (sameSelection selected) found then found else selected : found
  collect ((givens, required) : rest) equations found = candidates givens found
   where
    candidates _ solutions@(_ : _ : _) = pure solutions
    candidates [] solutions = pure solutions
    candidates (given : remaining) solutions = do
      fuel <- get
      guard $ fuel > 0
      put $ fuel - 1
      if constraint_tclass given /= constraint_tclass required ||
          length (constraint_params given) /= length (constraint_params required)
        then candidates remaining solutions
        else do
          let nextEquations = equations ++ zipWith TypeEq
                (constraint_params given) (constraint_params required)
          case match nextEquations of
            Nothing -> candidates remaining solutions
            Just _ -> do
              next <- collect rest nextEquations solutions
              candidates remaining next

  match :: [TypeEq] -> Maybe Substs
  match equations = do
    substitutions <- IntMap.filterWithKey (\variable image -> image /= TypeVar variable)
      <$> unifyRightEqs equations
    guard $ IntMap.keysSet substitutions `IntSet.isSubsetOf` eligible
    pure substitutions

  sameSelection left right = IntMap.keysSet left == IntMap.keysSet right &&
    and (IntMap.elems $ IntMap.intersectionWith SharedTypeAtom.alphaEquivalentTypes left right)

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

    resolve visiting sourceConstraint = case validateKnownConstraintInEnv
        (qClassEnv_env environment) QueryConstraint constraint of
      Left _ -> Nothing
      Right ()
        -- A matching local dictionary is conclusive even while its type
        -- arguments contain flexible variables. Deferring first would reject
        -- an exact given such as @C a => ... C a ...@ at final checking time.
        | constraint `Set.member` qClassEnv_inflatedConstraints environment ->
            Just []
        | constraintContainsVariables constraint -> variableResult constraint
        -- Revisiting the same ground goal proves nothing on this instance
        -- branch. Search mode rejects it; final residual mode retains the
        -- obligation through its ordinary no-evidence policy.
        | constraint `Set.member` visiting -> noEvidenceResult constraint
        | otherwise ->
            SharedCollection.firstPresent
              [ bestResult
                  $ map (fromInstance $ Set.insert constraint visiting) instances
              , noEvidenceResult constraint
              ]
      where
        constraint = fmap SharedType.canonicalizeType sourceConstraint
        HsConstraint className parameters = constraint
        instances = Map.findWithDefault []
          className
          (sClassEnv_instances $ qClassEnv_env environment)

        fromInstance nextVisiting
            (HsInstance prerequisites (HsConstraint instanceClass instanceParameters)) = do
          guard $ className == instanceClass
          let expected = length instanceParameters
          guard $ SharedCollection.observedListLength expected parameters
            == expected
          substitutions <- unifyRightEqs $ zipWith TypeEq parameters instanceParameters
          solve nextVisiting
            $ map (snd . constraintApplySubsts substitutions) prerequisites

bestResult :: [Maybe [a]] -> Maybe [a]
bestResult results = case catMaybes results of
  [] -> Nothing
  candidates -> Just $ minimumBy (comparing length) candidates
