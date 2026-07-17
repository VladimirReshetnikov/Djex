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
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Language.Haskell.Synthesis.Constraint
import Language.Haskell.Synthesis.Type

-- | Opaque alpha-normal identity for an instance head.  Bound variables are
-- identified by lexical scope and first occurrence within that scope;
-- genuinely free variables retain their source identity.
newtype InstanceHeadKey typeVariable = InstanceHeadKey
  (Constraint (Type (CanonicalInstanceVariable typeVariable)))
  deriving (Eq, Ord, Show, Generic)

instance NFData typeVariable => NFData (InstanceHeadKey typeVariable)

data CanonicalInstanceVariable typeVariable
  = CanonicalBoundVariable !Natural !Natural
  | CanonicalFreeVariable typeVariable
  deriving (Eq, Ord, Show, Generic)

instance NFData typeVariable =>
    NFData (CanonicalInstanceVariable typeVariable)

-- Scope and slot identities exist only while constructing a private
-- alpha-normal form. They use arbitrary-precision counters so sufficiently
-- deep or wide generated types cannot wrap and conflate distinct binders;
-- declaration indices and source-facing arities remain machine-sized.
data CanonicalizationState typeVariable = CanonicalizationState
  { canonicalVariableSlots :: Map (Natural, typeVariable) Natural
  , nextCanonicalSlotByScope :: Map Natural Natural
  , nextCanonicalScope :: !Natural
  }

-- | Construct the private comparison identity for one explicitly scoped
-- instance head.  The order of the implicit outer binders is immaterial.
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

canonicalizeInstanceHead
  :: Ord typeVariable
  => [typeVariable]
  -> Constraint (Type typeVariable)
  -> Constraint (Type (CanonicalInstanceVariable typeVariable))
canonicalizeInstanceHead variables headConstraint =
  evalState (canonicalizeInstanceConstraint bindings headConstraint) initialState
 where
  -- Instance binders form an implicit outer scope. Their declaration order is
  -- not semantically significant, so slots are allocated when the head first
  -- mentions each variable rather than from the binder list.
  bindings = Map.fromList [(variable, 0) | variable <- variables]
  initialState = CanonicalizationState Map.empty Map.empty 1

canonicalizeInstanceConstraint
  :: Ord typeVariable
  => Map typeVariable Natural
  -> Constraint (Type typeVariable)
  -> State (CanonicalizationState typeVariable)
      (Constraint (Type (CanonicalInstanceVariable typeVariable)))
canonicalizeInstanceConstraint bindings constraint = Constraint
  (constraintClass constraint)
  <$> mapM (canonicalizeInstanceType bindings) (constraintArguments constraint)

canonicalizeInstanceType
  :: Ord typeVariable
  => Map typeVariable Natural
  -> Type typeVariable
  -> State (CanonicalizationState typeVariable)
      (Type (CanonicalInstanceVariable typeVariable))
canonicalizeInstanceType bindings source = case source of
  TypeVariable variable -> TypeVariable
    <$> canonicalizeVariable bindings variable
  TypeConstructor name -> pure $ TypeConstructor name
  TypeApplication function argument -> TypeApplication
    <$> canonicalizeInstanceType bindings function
    <*> canonicalizeInstanceType bindings argument
  FunctionType parameter result -> FunctionType
    <$> canonicalizeInstanceType bindings parameter
    <*> canonicalizeInstanceType bindings result
  TupleType boxity elements -> TupleType boxity
    <$> mapM (canonicalizeInstanceType bindings) elements
  ForallType variables constraints body -> do
    scope <- allocateCanonicalScope
    let nestedBindings = Map.fromList
          [(variable, scope) | variable <- variables] `Map.union` bindings
    canonicalConstraints <- mapM
      (canonicalizeInstanceConstraint nestedBindings) constraints
    canonicalBody <- canonicalizeInstanceType nestedBindings body
    pure $ ForallType
      [ CanonicalBoundVariable scope slot
      | (slot, _) <- zip [0 ..] variables
      ] canonicalConstraints canonicalBody

canonicalizeVariable
  :: Ord typeVariable
  => Map typeVariable Natural
  -> typeVariable
  -> State (CanonicalizationState typeVariable)
      (CanonicalInstanceVariable typeVariable)
canonicalizeVariable bindings variable = case Map.lookup variable bindings of
  Nothing -> pure $ CanonicalFreeVariable variable
  Just scope -> do
    state <- get
    case Map.lookup (scope, variable) $ canonicalVariableSlots state of
      Just slot -> pure $ CanonicalBoundVariable scope slot
      Nothing -> do
        let slot = Map.findWithDefault 0 scope
              $ nextCanonicalSlotByScope state
        put state
          { canonicalVariableSlots = Map.insert (scope, variable) slot
              $ canonicalVariableSlots state
          , nextCanonicalSlotByScope = Map.insert scope (slot + 1)
              $ nextCanonicalSlotByScope state
          }
        pure $ CanonicalBoundVariable scope slot

allocateCanonicalScope
  :: State (CanonicalizationState typeVariable) Natural
allocateCanonicalScope = do
  state <- get
  let scope = nextCanonicalScope state
  put state { nextCanonicalScope = scope + 1 }
  pure scope
