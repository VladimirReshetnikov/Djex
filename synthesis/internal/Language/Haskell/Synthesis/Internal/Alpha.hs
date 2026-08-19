{-# LANGUAGE DeriveGeneric #-}

-- | Shared lexical alpha-normalization machinery, the two binder-aware
-- 'Type' walks that the canonical forms built on it all share
-- ('eraseVacuousForalls' and 'rewriteTypeVariables'), and the one first-order
-- structural equation solver ('solveTypeEquations') behind instance-head
-- overlap, context-free scheme matching, and class-resolution overlap.
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
  , replaceFreeTypeVariable
  , EquationPolicy (..)
  , ForallEquations (..)
  , EquationSolution (..)
  , emptyEquationSolution
  , solveTypeEquations
  , zonkSolution
  , constraintEquations
  , constraintListEquations
  , zipExactly
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

-- | Replace every free occurrence of one variable by a type, stopping at any
-- forall that rebinds that variable so its bound occurrences stay bound.
-- This is the binder-aware instantiation step shared by type-atom
-- specialization and certificate selection replay: the caller guarantees
-- that no binder the walk passes through captures a free variable of the
-- replacement (typically the replacement is closed or uses fresh names).
replaceFreeTypeVariable
  :: Eq variable
  => variable
  -> Type variable
  -> Type variable
  -> Type variable
replaceFreeTypeVariable selected replacement = go
 where
  go source = case source of
    TypeVariable variable
      | variable == selected -> replacement
      | otherwise -> source
    TypeConstructor{} -> source
    TypeApplication function argument ->
      TypeApplication (go function) (go argument)
    FunctionType parameter result -> FunctionType (go parameter) (go result)
    TupleType boxity elements -> TupleType boxity $ map go elements
    ForallType binders constraints body
      | selected `elem` binders -> source
      | otherwise -> ForallType binders (map (fmap go) constraints) (go body)

-- First-order equation solving ---------------------------------------------

-- | What a first-order structural solver may bind, and how it treats
-- foralls.  Three solvers share 'solveTypeEquations': instance-head overlap
-- binds either side's instance variables and pairs nested forall binders,
-- context-free scheme matching binds only the source's prefix binders and
-- pairs nested binders, and class-resolution overlap binds either side's
-- binders while treating every forall as an opaque atom.
data EquationPolicy variable = EquationPolicy
  { equationBinding
      :: Type variable -> Type variable -> Maybe (variable, Type variable)
    -- ^ Given the two zonked, unequal sides of one equation, the
    -- metavariable to bind and its replacement, or 'Nothing' when neither
    -- side is a bindable variable.  A symmetric policy inspects both sides;
    -- a one-way policy inspects only the pattern side.
  , equationForalls :: ForallEquations variable
  }

-- | How equations between two foralls are solved.
data ForallEquations variable
  = OpaqueForallEquations
    -- ^ A forall is an atom: two foralls must be equal, a variable is never
    -- substituted inside one, and the occurs check does not look inside one.
  | PairedForallEquations (Natural -> variable) (variable -> Bool)
    -- ^ Two foralls with the same binder count are opened together: each
    -- binder pair is renamed to one fresh skolem (the first function, given a
    -- fresh ordinal), their contexts must have the same length and classes,
    -- and the renamed bodies and context arguments become equations.  The
    -- second function recognizes those skolems, which no binding may
    -- capture: a metavariable quantified outside the comparison may capture a
    -- whole closed forall but never a skolem exposed by opening one.

-- | Accumulated bindings and the next fresh skolem ordinal.
data EquationSolution variable = EquationSolution
  { solutionSubstitutions :: !(Map variable (Type variable))
  , solutionNextSkolem :: !Natural
  }

-- | No bindings and skolem ordinals from zero.
emptyEquationSolution :: EquationSolution variable
emptyEquationSolution = EquationSolution Map.empty 0

-- | Solve a list of type equations by first-order structural decomposition
-- under the policy, extending the supplied solution.  Both sides of every
-- equation are zonked before inspection; a binding first passes the occurs
-- check and the policy's skolem check, is substituted into the remaining
-- equations and into every earlier binding, and is then recorded.
-- Constructors must be the same name; applications, arrows, and same-boxity
-- tuples decompose positionally; foralls follow 'ForallEquations'.
-- 'Nothing' is a structural mismatch.
solveTypeEquations
  :: Ord variable
  => EquationPolicy variable
  -> [(Type variable, Type variable)]
  -> EquationSolution variable
  -> Maybe (EquationSolution variable)
solveTypeEquations policy = go
 where
  foralls = forallRewriteOf $ equationForalls policy

  go [] solution = Just solution
  go ((rawLeft, rawRight) : equations) solution
    | left == right = go equations solution
    | Just (variable, replacement) <- equationBinding policy left right =
        bind variable replacement
    | TypeConstructor leftName <- left
    , TypeConstructor rightName <- right
    , leftName == rightName = go equations solution
    | TypeApplication leftFunction leftArgument <- left
    , TypeApplication rightFunction rightArgument <- right =
        go
          ((leftFunction, rightFunction) :
            (leftArgument, rightArgument) : equations)
          solution
    | FunctionType leftParameter leftResult <- left
    , FunctionType rightParameter rightResult <- right =
        go
          ((leftParameter, rightParameter) :
            (leftResult, rightResult) : equations)
          solution
    | TupleType leftBoxity leftElements <- left
    , TupleType rightBoxity rightElements <- right
    , leftBoxity == rightBoxity
    , Just elementEquations <- zipExactly leftElements rightElements =
        go (elementEquations ++ equations) solution
    | ForallType leftBinders leftConstraints leftBody <- left
    , ForallType rightBinders rightConstraints rightBody <- right
    , PairedForallEquations skolem _ <- equationForalls policy
    , Just (openedEquations, openedSolution) <- openForalls skolem
        leftBinders leftConstraints leftBody
        rightBinders rightConstraints rightBody solution =
        go (openedEquations ++ equations) openedSolution
    | otherwise = Nothing
   where
    substitutions = solutionSubstitutions solution
    left = zonkSolutionWith foralls substitutions rawLeft
    right = zonkSolutionWith foralls substitutions rawRight

    bind variable replacement
      | occursUnder foralls variable replacement = Nothing
      | capturesPairedSkolem replacement = Nothing
      | otherwise = go
          [ ( substituteVariable foralls variable replacement equationLeft
            , substituteVariable foralls variable replacement equationRight
            )
          | (equationLeft, equationRight) <- equations
          ]
          solution
            { solutionSubstitutions = Map.insert variable replacement
                $ Map.map (substituteVariable foralls variable replacement)
                  substitutions
            }

  capturesPairedSkolem replacement = case equationForalls policy of
    OpaqueForallEquations -> False
    PairedForallEquations _ isSkolem -> any isSkolem replacement

-- | Open two foralls together: pair their binders with fresh skolems, turn
-- their contexts into equations, and return the body equation first.
openForalls
  :: Ord variable
  => (Natural -> variable)
  -> [variable]
  -> [Constraint (Type variable)]
  -> Type variable
  -> [variable]
  -> [Constraint (Type variable)]
  -> Type variable
  -> EquationSolution variable
  -> Maybe ([(Type variable, Type variable)], EquationSolution variable)
openForalls skolem leftBinders leftConstraints leftBody
    rightBinders rightConstraints rightBody solution = do
  (leftRenaming, rightRenaming, nextSkolem) <-
    pairBinders (solutionNextSkolem solution) leftBinders rightBinders
  contextEquations <- constraintListEquations
    (map (fmap $ renameVariables leftRenaming) leftConstraints)
    (map (fmap $ renameVariables rightRenaming) rightConstraints)
  pure
    ( ( renameVariables leftRenaming leftBody
      , renameVariables rightRenaming rightBody
      ) : contextEquations
    , solution { solutionNextSkolem = nextSkolem }
    )
 where
  pairBinders next [] [] = Just (Map.empty, Map.empty, next)
  pairBinders next (left : leftRest) (right : rightRest) = do
    (leftRenaming, rightRenaming, finalNext) <-
      pairBinders (next + 1) leftRest rightRest
    let paired = skolem next
    pure
      ( Map.insert left paired leftRenaming
      , Map.insert right paired rightRenaming
      , finalNext
      )
  pairBinders _ _ _ = Nothing

  renameVariables renaming = fmap $ \variable ->
    Map.findWithDefault variable variable renaming

-- | Apply the solution's bindings everywhere, repeatedly, under the
-- policy's forall treatment.
zonkSolution
  :: Ord variable
  => EquationPolicy variable
  -> EquationSolution variable
  -> Type variable
  -> Type variable
zonkSolution policy =
  zonkSolutionWith (forallRewriteOf $ equationForalls policy)
    . solutionSubstitutions

zonkSolutionWith
  :: Ord variable
  => ForallRewrite
  -> Map variable (Type variable)
  -> Type variable
  -> Type variable
zonkSolutionWith foralls substitutions =
  rewriteTypeVariables foralls $ \variable ->
    case Map.lookup variable substitutions of
      Nothing -> TypeVariable variable
      Just replacement -> zonkSolutionWith foralls substitutions replacement

substituteVariable
  :: Eq variable
  => ForallRewrite
  -> variable
  -> Type variable
  -> Type variable
  -> Type variable
substituteVariable foralls variable replacement =
  rewriteTypeVariables foralls $ \candidate ->
    if candidate == variable then replacement else TypeVariable candidate

-- Whether the variable occurs in the type, looking inside foralls only when
-- substitution does.
occursUnder :: Eq variable => ForallRewrite -> variable -> Type variable -> Bool
occursUnder foralls variable = go
 where
  go source = case source of
    TypeVariable candidate -> candidate == variable
    TypeConstructor{} -> False
    TypeApplication function argument -> go function || go argument
    FunctionType parameter result -> go parameter || go result
    TupleType _ elements -> any go elements
    ForallType{} -> case foralls of
      OpaqueForalls -> False
      ThroughForalls -> variable `elem` source

forallRewriteOf :: ForallEquations variable -> ForallRewrite
forallRewriteOf foralls = case foralls of
  OpaqueForallEquations -> OpaqueForalls
  PairedForallEquations{} -> ThroughForalls

-- | Equations between the arguments of two constraints of the same class
-- and arity; 'Nothing' when the classes or arities differ.
constraintEquations
  :: Constraint (Type variable)
  -> Constraint (Type variable)
  -> Maybe [(Type variable, Type variable)]
constraintEquations (Constraint leftClass leftArguments)
    (Constraint rightClass rightArguments)
  | leftClass /= rightClass = Nothing
  | otherwise = zipExactly leftArguments rightArguments

-- | 'constraintEquations' over two contexts of the same length, in order.
constraintListEquations
  :: [Constraint (Type variable)]
  -> [Constraint (Type variable)]
  -> Maybe [(Type variable, Type variable)]
constraintListEquations [] [] = Just []
constraintListEquations (left : leftRest) (right : rightRest) = do
  equations <- constraintEquations left right
  rest <- constraintListEquations leftRest rightRest
  pure $ equations ++ rest
constraintListEquations _ _ = Nothing

-- | Pair two lists positionally; 'Nothing' when their lengths differ.
zipExactly :: [left] -> [right] -> Maybe [(left, right)]
zipExactly [] [] = Just []
zipExactly (left : leftRest) (right : rightRest) =
  ((left, right) :) <$> zipExactly leftRest rightRest
zipExactly _ _ = Nothing
