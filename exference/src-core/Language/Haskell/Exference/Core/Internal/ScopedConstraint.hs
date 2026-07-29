{-# LANGUAGE DeriveGeneric #-}

-- | A proof obligation together with the lexical class assumptions under
-- which it arose.
--
-- Nested contextual foralls make this provenance semantically significant:
-- a local dictionary may discharge work in the quantified body, but must not
-- solve an unrelated sibling goal which happens to remain in the same search
-- node.  Keeping assumptions on each deferred obligation also lets later
-- substitutions update both sides before the common class solver runs.
module Language.Haskell.Exference.Core.Internal.ScopedConstraint
  ( ScopedConstraint (..)
  , scopedConstraints
  , scopedConstraintApplySubsts
  , resolveScopedConstraints
  , scopedConstraintObligations
  )
where

import Control.DeepSeq (NFData)
import Data.Monoid (Any (..))
import GHC.Generics (Generic)

import Language.Haskell.Exference.Core.Types

data ScopedConstraint = ScopedConstraint
  { scopedConstraintGivens :: [HsConstraint]
  , scopedConstraintObligation :: HsConstraint
  }
  deriving (Eq, Generic, Show)

instance NFData ScopedConstraint

-- | Attach one lexical assumption set to a collection of new obligations.
scopedConstraints :: [HsConstraint] -> [HsConstraint] -> [ScopedConstraint]
scopedConstraints givens = map $ ScopedConstraint givens

-- | Apply one simultaneous substitution to the local givens and obligation.
-- The flag reports a change to either half, because changing a given can alter
-- whether the obligation is solvable even when the obligation itself stayed
-- syntactically fixed.
scopedConstraintApplySubsts
  :: Substs
  -> ScopedConstraint
  -> (Any, ScopedConstraint)
scopedConstraintApplySubsts substitutions
    (ScopedConstraint givens obligation) =
  (givensAffected <> obligationAffected, ScopedConstraint givens' obligation')
 where
  givenResults = map (constraintApplySubsts substitutions) givens
  givensAffected = foldMap fst givenResults
  givens' = map snd givenResults
  (obligationAffected, obligation') =
    constraintApplySubsts substitutions obligation

-- | Resolve each obligation independently under the root class environment
-- extended by precisely its own lexical givens. Any prerequisites returned by
-- the solver remain in that same scope.
resolveScopedConstraints
  :: (QueryClassEnv -> [HsConstraint] -> Maybe [HsConstraint])
  -> QueryClassEnv
  -> [ScopedConstraint]
  -> Maybe [ScopedConstraint]
resolveScopedConstraints solver rootEnvironment = fmap concat . traverse resolve
 where
  resolve (ScopedConstraint givens obligation) =
    scopedConstraints givens
      <$> solver (addQueryClassEnv givens rootEnvironment) [obligation]

scopedConstraintObligations :: [ScopedConstraint] -> [HsConstraint]
scopedConstraintObligations = map scopedConstraintObligation
