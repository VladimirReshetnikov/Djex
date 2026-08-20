{-# LANGUAGE DeriveGeneric #-}

-- | Private alpha identity for instance heads.
--
-- The public environment retains the caller's variables and source head for
-- indexing and diagnostics.  This module owns only the opaque comparison key
-- shared by environment construction and compatibility validators.
module Language.Haskell.Synthesis.Internal.InstanceHead
  ( InstanceHeadKey
  , instanceHeadKey
  , repeatedInstanceHeadsInFirstRepetitionOrder
  , overlappingInstanceHeadPairsInSourceOrder
  ) where

import Control.DeepSeq (NFData)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Language.Haskell.Synthesis.Constraint
import Language.Haskell.Synthesis.Internal.Alpha
  ( AlphaVariable (..)
  , BinderSlotPolicy (FirstOccurrenceBinderSlots)
  , EquationPolicy (..)
  , ForallEquations (..)
  , alphaNormalizeConstraintWithOuter
  , constraintEquations
  , emptyEquationSolution
  , solveTypeEquations
  )
import Language.Haskell.Synthesis.Type

-- | Opaque alpha-normal identity for an instance head.  Bound variables are
-- identified by lexical scope and first occurrence within that scope;
-- genuinely free variables retain their source identity.
newtype InstanceHeadKey typeVariable = InstanceHeadKey
  (Constraint (Type (AlphaVariable typeVariable)))
  deriving (Eq, Ord, Show, Generic)

instance NFData typeVariable => NFData (InstanceHeadKey typeVariable)

-- | Construct the private comparison identity for one explicitly scoped
-- instance head. Saturated function and tuple constructor applications are
-- normalized before alpha identities are assigned, while the caller's source
-- head remains available for storage and diagnostics. The order of the
-- implicit outer binders is immaterial.
instanceHeadKey
  :: Ord typeVariable
  => [typeVariable]
  -> Constraint (Type typeVariable)
  -> InstanceHeadKey typeVariable
instanceHeadKey variables = InstanceHeadKey
  . canonicalizeInstanceHead variables

-- | Return the source head that first repeats each alpha-equivalence class.
-- Results follow first-repetition order, and a class repeated three or more
-- times is still reported once.  Keeping the source head rather than the
-- private key makes diagnostics exact and useful.
repeatedInstanceHeadsInFirstRepetitionOrder
  :: Ord typeVariable
  => [([typeVariable], Constraint (Type typeVariable))]
  -> [Constraint (Type typeVariable)]
repeatedInstanceHeadsInFirstRepetitionOrder sources = reverse repeatedHeads
 where
  RepetitionState _ _ repeatedHeads = foldl' inspect emptyState sources

  emptyState = RepetitionState Set.empty Set.empty []

  inspect state (variables, headConstraint)
    | key `Set.member` repeatedKeys = state
    | key `Set.member` seenKeys = RepetitionState seenKeys
        (Set.insert key repeatedKeys) (headConstraint : repeated)
    | otherwise = RepetitionState
        (Set.insert key seenKeys) repeatedKeys repeated
   where
    key = instanceHeadKey variables headConstraint
    RepetitionState seenKeys repeatedKeys repeated = state

data RepetitionState typeVariable = RepetitionState
  !(Set (InstanceHeadKey typeVariable))
  !(Set (InstanceHeadKey typeVariable))
  [Constraint (Type typeVariable)]

-- | Return every pair of explicit instance heads that can match the same
-- ground constraint. Pairs retain declaration order, both within the pair and
-- across the result, so a backend can produce stable source diagnostics.
--
-- Each declaration's outer variables form an independent unification scope.
-- Genuinely free variables remain nominal constants; nested forall variables
-- are alpha-normalized and rigid. This is deliberately a classification
-- helper rather than an environment rule: a backend may reject overlap, or it
-- may implement a language-specific overlap-selection policy.
overlappingInstanceHeadPairsInSourceOrder
  :: Ord typeVariable
  => [([typeVariable], Constraint (Type typeVariable))]
  -> [( Constraint (Type typeVariable)
      , Constraint (Type typeVariable)
      )]
overlappingInstanceHeadPairsInSourceOrder = comparePrepared . prepareSources 0
 where
  -- Preserve the historical left-major pair order without repeatedly taking
  -- indexed suffixes. Class identity is checked before invoking unification,
  -- so unrelated class partitions pay no type-traversal cost.
  comparePrepared [] = []
  comparePrepared (left : remaining) =
    [ (preparedSourceHead left, preparedSourceHead right)
    | right <- remaining
    , constraintClass (preparedSourceHead left)
        == constraintClass (preparedSourceHead right)
    , preparedHeadsOverlap left right
    ] ++ comparePrepared remaining

  prepareSources _ [] = []
  prepareSources source (current : remaining) =
    prepareOverlapHead (InstanceHeadSource source) current
      : prepareSources (source + 1) remaining

newtype InstanceHeadSource = InstanceHeadSource Natural
  deriving (Eq, Ord)

-- Only implicitly quantified outer variables are bindable. Lexical forall
-- binders remain side-local until two forall nodes are compared, at which
-- point corresponding binders are replaced by fresh shared skolems.
data OverlapVariable typeVariable
  = BindableInstanceVariable !InstanceHeadSource !Natural
  | LexicalForallVariable !InstanceHeadSource !Natural !Natural
  | PairedForallSkolem !Natural
  | FreeInstanceVariable typeVariable
  deriving (Eq, Ord)

data PreparedOverlapHead typeVariable = PreparedOverlapHead
  { preparedSourceHead :: Constraint (Type typeVariable)
  , preparedCanonicalHead :: Constraint (Type (OverlapVariable typeVariable))
  }

prepareOverlapHead
  :: Ord typeVariable
  => InstanceHeadSource
  -> ([typeVariable], Constraint (Type typeVariable))
  -> PreparedOverlapHead typeVariable
prepareOverlapHead source (outerVariables, sourceHead) = PreparedOverlapHead
  sourceHead
  $ fmap prepareVariable
      <$> canonicalizeInstanceHead outerVariables sourceHead
 where
  -- Reuse the exact alpha identity used for duplicate instance heads. In
  -- particular, commuting forall binders receive slots by first occurrence
  -- across their constraints and body, not by incidental declaration order.
  prepareVariable variable = case variable of
    AlphaBoundVariable 0 slot ->
      BindableInstanceVariable source slot
    AlphaBoundVariable scope slot ->
      LexicalForallVariable source scope slot
    AlphaFreeVariable free -> FreeInstanceVariable free

preparedHeadsOverlap
  :: Ord typeVariable
  => PreparedOverlapHead typeVariable
  -> PreparedOverlapHead typeVariable
  -> Bool
preparedHeadsOverlap leftHead rightHead = case constraintEquations
    (preparedCanonicalHead leftHead) (preparedCanonicalHead rightHead) of
  Nothing -> False
  Just equations -> case
      solveTypeEquations overlapPolicy equations emptyEquationSolution of
    Nothing -> False
    Just _ -> True

-- Overlap is symmetric and spans every argument in a multi-parameter head, so
-- it is solved as one equation system rather than argument by argument:
-- per-argument matching would incorrectly classify @C a a@ and @C Int Bool@
-- as overlapping.  Either side's implicitly quantified instance variables may
-- be bound; nested forall binders are paired with fresh shared skolems.
overlapPolicy :: EquationPolicy (OverlapVariable typeVariable)
overlapPolicy = EquationPolicy
  { equationBinding = \left right -> case (left, right) of
      (TypeVariable variable, _) | isBindable variable -> Just (variable, right)
      (_, TypeVariable variable) | isBindable variable -> Just (variable, left)
      _ -> Nothing
  , equationForalls = PairedForallEquations PairedForallSkolem isPairedSkolem
  }
 where
  isBindable variable = case variable of
    BindableInstanceVariable{} -> True
    LexicalForallVariable{} -> False
    PairedForallSkolem{} -> False
    FreeInstanceVariable{} -> False

  isPairedSkolem PairedForallSkolem{} = True
  isPairedSkolem _ = False

canonicalizeInstanceHead
  :: Ord typeVariable
  => [typeVariable]
  -> Constraint (Type typeVariable)
  -> Constraint (Type (AlphaVariable typeVariable))
canonicalizeInstanceHead variables =
  alphaNormalizeConstraintWithOuter FirstOccurrenceBinderSlots variables
    . fmap canonicalizeType
