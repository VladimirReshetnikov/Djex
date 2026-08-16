{-# LANGUAGE DeriveGeneric #-}

-- | Shared lexical alpha-normalization machinery.
--
-- Explicit forall syntax uses binder positions: declaration order is part of
-- the type, while spelling is not. Instance declarations have a different
-- historical rule for their implicit outer scope, where binder declaration
-- order is immaterial and slots are assigned by first occurrence. Keeping the
-- policy explicit lets both identities share one scope-correct traversal.
module Language.Haskell.Synthesis.Internal.Alpha
  ( AlphaVariable (..)
  , BinderSlotPolicy (..)
  , alphaNormalizeTypeWith
  , alphaNormalizeConstraintWithOuter
  , eraseVacuousForalls
  , ForallRewrite (..)
  , rewriteTypeVariables
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Constraint (Constraint (..))
import Language.Haskell.Synthesis.Type (Type (..))

-- | A spelling-independent variable identity.
data AlphaVariable variable
  = AlphaBoundVariable !Natural !Natural
    -- ^ Lexical scope and slot within that scope.
  | AlphaFreeVariable variable
  deriving (Eq, Ord, Show, Generic)

instance NFData variable => NFData (AlphaVariable variable)

-- | How occurrences acquire slots within each bound scope.
data BinderSlotPolicy
  = PositionalBinderSlots
    -- ^ Binder positions are significant, as for explicit @forall a b@.
  | FirstOccurrenceBinderSlots
    -- ^ Positions commute and are assigned when first mentioned.

data BoundReference
  = PositionedReference !Natural !Natural
  | FirstOccurrenceReference !Natural

data AlphaState variable = AlphaState
  { variableSlots :: !(Map (Natural, variable) Natural)
  , nextSlotByScope :: !(Map Natural Natural)
  , nextScope :: !Natural
  }

-- | Alpha-normalize a type with no implicit surrounding binders.
alphaNormalizeTypeWith
  :: Ord variable
  => BinderSlotPolicy
  -> Type variable
  -> Type (AlphaVariable variable)
alphaNormalizeTypeWith policy source = evalState
  (normalizeType policy Map.empty source)
  $ AlphaState Map.empty Map.empty 0

-- | Alpha-normalize a constraint inside one implicit outer binder scope.
--
-- The outer binder declarations are not present in the returned syntax. This
-- is the shape required by instance-head keys.
alphaNormalizeConstraintWithOuter
  :: Ord variable
  => BinderSlotPolicy
  -> [variable]
  -> Constraint (Type variable)
  -> Constraint (Type (AlphaVariable variable))
alphaNormalizeConstraintWithOuter policy variables source = evalState
  -- Only the synthetic outer scope is allowed to commute.  Any explicit
  -- forall reached inside the instance head remains ordinary lexical syntax,
  -- whose declaration positions are significant under alpha-renaming.
  (normalizeConstraint PositionalBinderSlots bindings source)
  $ AlphaState Map.empty Map.empty 1
 where
  bindings = scopeBindings policy 0 variables

normalizeType
  :: Ord variable
  => BinderSlotPolicy
  -> Map variable BoundReference
  -> Type variable
  -> State (AlphaState variable) (Type (AlphaVariable variable))
normalizeType policy bindings source = case source of
  TypeVariable variable -> TypeVariable
    <$> normalizeVariable bindings variable
  TypeConstructor name -> pure $ TypeConstructor name
  TypeApplication function argument -> TypeApplication
    <$> normalizeType policy bindings function
    <*> normalizeType policy bindings argument
  FunctionType parameter result -> FunctionType
    <$> normalizeType policy bindings parameter
    <*> normalizeType policy bindings result
  TupleType boxity elements -> TupleType boxity
    <$> mapM (normalizeType policy bindings) elements
  ForallType variables constraints body -> do
    scope <- allocateScope
    let nestedBindings = scopeBindings policy scope variables
          `Map.union` bindings
        normalizedBinders =
          [ AlphaBoundVariable scope position
          | (position, _) <- zip [0 ..] variables
          ]
    normalizedConstraints <- mapM
      (normalizeConstraint policy nestedBindings) constraints
    normalizedBody <- normalizeType policy nestedBindings body
    pure $ ForallType normalizedBinders normalizedConstraints normalizedBody

normalizeConstraint
  :: Ord variable
  => BinderSlotPolicy
  -> Map variable BoundReference
  -> Constraint (Type variable)
  -> State
      (AlphaState variable)
      (Constraint (Type (AlphaVariable variable)))
normalizeConstraint policy bindings (Constraint className arguments) =
  Constraint className <$> mapM (normalizeType policy bindings) arguments

scopeBindings
  :: Ord variable
  => BinderSlotPolicy
  -> Natural
  -> [variable]
  -> Map variable BoundReference
scopeBindings policy scope variables = Map.fromList $ case policy of
  PositionalBinderSlots ->
    [ (variable, PositionedReference scope position)
    | (position, variable) <- zip [0 ..] variables
    ]
  FirstOccurrenceBinderSlots ->
    [(variable, FirstOccurrenceReference scope) | variable <- variables]

normalizeVariable
  :: Ord variable
  => Map variable BoundReference
  -> variable
  -> State (AlphaState variable) (AlphaVariable variable)
normalizeVariable bindings variable = case Map.lookup variable bindings of
  Nothing -> pure $ AlphaFreeVariable variable
  Just (PositionedReference scope slot) ->
    pure $ AlphaBoundVariable scope slot
  Just (FirstOccurrenceReference scope) -> do
    state <- get
    case Map.lookup (scope, variable) $ variableSlots state of
      Just slot -> pure $ AlphaBoundVariable scope slot
      Nothing -> do
        let slot = Map.findWithDefault 0 scope $ nextSlotByScope state
        put state
          { variableSlots = Map.insert (scope, variable) slot
              $ variableSlots state
          , nextSlotByScope = Map.insert scope (slot + 1)
              $ nextSlotByScope state
          }
        pure $ AlphaBoundVariable scope slot

allocateScope :: State (AlphaState variable) Natural
allocateScope = do
  state <- get
  let scope = nextScope state
  put state { nextScope = scope + 1 }
  pure scope

-- | Erase binderless, context-free foralls throughout a type.  Such a node
-- contributes no semantic scope: text rendering elides it, the checked type
-- structure's equality ignores it, and every canonical form (atom keys, type
-- and graph fingerprints, certificate scope coordinates, instantiation
-- plans) erases it before assigning identities.  This is the one shared
-- definition of that erasure.
eraseVacuousForalls :: Type variable -> Type variable
eraseVacuousForalls source = case source of
  TypeVariable{} -> source
  TypeConstructor{} -> source
  TypeApplication function argument -> TypeApplication
    (eraseVacuousForalls function) (eraseVacuousForalls argument)
  FunctionType parameter result -> FunctionType
    (eraseVacuousForalls parameter) (eraseVacuousForalls result)
  TupleType boxity fields -> TupleType boxity
    $ map eraseVacuousForalls fields
  ForallType [] [] body -> eraseVacuousForalls body
  ForallType binders constraints body -> ForallType binders
    (map (fmap eraseVacuousForalls) constraints)
    (eraseVacuousForalls body)

-- | How 'rewriteTypeVariables' treats a forall it meets.
data ForallRewrite
  = OpaqueForalls
    -- ^ Leave the whole forall untouched: the rewrite is a first-order
    -- operation and quantified structure is not part of it.
  | ThroughForalls
    -- ^ Rewrite the forall's constraints and body while leaving its binders
    -- alone.  Callers guarantee no capture, typically because the rewritten
    -- variables and bound variables are distinct constructors of the variable
    -- type.

-- | Rewrite every variable occurrence of a type by @atVariable@, rebuilding
-- applications, arrows and tuples around the results, and treating foralls
-- according to the policy.  The first-order substitution and zonk
-- operations of the solvers are all instances of this walk.
rewriteTypeVariables
  :: ForallRewrite
  -> (variable -> Type variable)
  -> Type variable
  -> Type variable
rewriteTypeVariables foralls atVariable = go
 where
  go source = case source of
    TypeVariable variable -> atVariable variable
    TypeConstructor{} -> source
    TypeApplication function argument ->
      TypeApplication (go function) (go argument)
    FunctionType parameter result -> FunctionType (go parameter) (go result)
    TupleType boxity fields -> TupleType boxity $ map go fields
    ForallType binders constraints body -> case foralls of
      OpaqueForalls -> source
      ThroughForalls ->
        ForallType binders (map (fmap go) constraints) (go body)
