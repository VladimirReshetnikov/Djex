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
  , constraintListEquations
  , zipExactly
  ) where

import Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Language.Haskell.Synthesis.Constraint
import Language.Haskell.Synthesis.Internal.Alpha
  ( AlphaVariable (..)
  , BinderSlotPolicy (FirstOccurrenceBinderSlots)
  , alphaNormalizeConstraintWithOuter
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
  $ fmap (fmap prepareVariable)
  $ canonicalizeInstanceHead outerVariables sourceHead
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
  Just equations -> case solveOverlap equations emptyOverlapSolverState of
    Nothing -> False
    Just _ -> True

constraintEquations
  :: Constraint (Type variable)
  -> Constraint (Type variable)
  -> Maybe [(Type variable, Type variable)]
constraintEquations (Constraint leftClass leftArguments)
    (Constraint rightClass rightArguments)
  | leftClass /= rightClass = Nothing
  | otherwise = zipExactly leftArguments rightArguments

-- | Argument equations for two same-length constraint lists whose classes
-- agree pairwise; 'Nothing' on any length or class mismatch.
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

-- | Zip two lists, or 'Nothing' when their lengths differ.
zipExactly :: [left] -> [right] -> Maybe [(left, right)]
zipExactly [] [] = Just []
zipExactly (left : leftRest) (right : rightRest) =
  ((left, right) :) <$> zipExactly leftRest rightRest
zipExactly _ _ = Nothing

type OverlapSubstitutions typeVariable =
  Map (OverlapVariable typeVariable) (Type (OverlapVariable typeVariable))

data OverlapSolverState typeVariable = OverlapSolverState
  { overlapSubstitutions :: OverlapSubstitutions typeVariable
  , nextPairedForallSkolem :: !Natural
  }

emptyOverlapSolverState :: OverlapSolverState typeVariable
emptyOverlapSolverState = OverlapSolverState Map.empty 0

-- A small first-order unifier is kept here because overlap is symmetric and
-- spans every argument in a multi-parameter head. Per-argument matching would
-- incorrectly classify @C a a@ and @C Int Bool@ as overlapping.
solveOverlap
  :: Ord typeVariable
  => [( Type (OverlapVariable typeVariable)
      , Type (OverlapVariable typeVariable)
      )]
  -> OverlapSolverState typeVariable
  -> Maybe (OverlapSolverState typeVariable)
solveOverlap [] state = Just state
solveOverlap ((rawLeft, rawRight) : equations) state
  | left == right = solveOverlap equations state
  | TypeVariable variable <- left
  , isBindable variable = bind variable right
  | TypeVariable variable <- right
  , isBindable variable = bind variable left
  | TypeConstructor leftName <- left
  , TypeConstructor rightName <- right
  , leftName == rightName = solveOverlap equations state
  | TypeApplication leftFunction leftArgument <- left
  , TypeApplication rightFunction rightArgument <- right =
      solveOverlap
        ((leftFunction, rightFunction) :
          (leftArgument, rightArgument) : equations)
        state
  | FunctionType leftParameter leftResult <- left
  , FunctionType rightParameter rightResult <- right =
      solveOverlap
        ((leftParameter, rightParameter) :
          (leftResult, rightResult) : equations)
        state
  | TupleType leftBoxity leftElements <- left
  , TupleType rightBoxity rightElements <- right
  , leftBoxity == rightBoxity
  , Just elementEquations <- zipExactly leftElements rightElements =
      solveOverlap (elementEquations ++ equations) state
  | ForallType leftBinders leftConstraints leftBody <- left
  , ForallType rightBinders rightConstraints rightBody <- right
  , Just (openedEquations, openedState) <- openForalls
      leftBinders leftConstraints leftBody
      rightBinders rightConstraints rightBody state =
      solveOverlap (openedEquations ++ equations) openedState
  | otherwise = Nothing
 where
  substitutions = overlapSubstitutions state
  left = zonkOverlap substitutions rawLeft
  right = zonkOverlap substitutions rawRight

  bind variable replacement
    | occursInOverlap variable replacement = Nothing
    -- A metavariable quantified outside this comparison may capture a whole
    -- closed forall, but never a skolem exposed by opening one of its bodies.
    | containsPairedForallSkolem replacement = Nothing
    | otherwise = solveOverlap
        [ ( substituteOverlap variable replacement equationLeft
          , substituteOverlap variable replacement equationRight
          )
        | (equationLeft, equationRight) <- equations
        ]
        state
          { overlapSubstitutions = Map.insert variable replacement
              $ Map.map (substituteOverlap variable replacement) substitutions
          }

openForalls
  :: Ord typeVariable
  => [OverlapVariable typeVariable]
  -> [Constraint (Type (OverlapVariable typeVariable))]
  -> Type (OverlapVariable typeVariable)
  -> [OverlapVariable typeVariable]
  -> [Constraint (Type (OverlapVariable typeVariable))]
  -> Type (OverlapVariable typeVariable)
  -> OverlapSolverState typeVariable
  -> Maybe
      ( [( Type (OverlapVariable typeVariable)
         , Type (OverlapVariable typeVariable)
         )]
      , OverlapSolverState typeVariable
      )
openForalls leftBinders leftConstraints leftBody
    rightBinders rightConstraints rightBody state = do
  (leftRenaming, rightRenaming, nextSkolem) <- pairBinders
    (nextPairedForallSkolem state) leftBinders rightBinders
  contextEquations <- constraintListEquations
    (map (fmap $ renameOverlapVariables leftRenaming) leftConstraints)
    (map (fmap $ renameOverlapVariables rightRenaming) rightConstraints)
  pure
    ( ( renameOverlapVariables leftRenaming leftBody
      , renameOverlapVariables rightRenaming rightBody
      ) : contextEquations
    , state { nextPairedForallSkolem = nextSkolem }
    )

pairBinders
  :: Ord typeVariable
  => Natural
  -> [OverlapVariable typeVariable]
  -> [OverlapVariable typeVariable]
  -> Maybe
      ( Map (OverlapVariable typeVariable) (OverlapVariable typeVariable)
      , Map (OverlapVariable typeVariable) (OverlapVariable typeVariable)
      , Natural
      )
pairBinders next [] [] = Just (Map.empty, Map.empty, next)
pairBinders next (left : leftRest) (right : rightRest) = do
  (leftRenaming, rightRenaming, finalNext) <-
    pairBinders (next + 1) leftRest rightRest
  let skolem = PairedForallSkolem next
  pure
    ( Map.insert left skolem leftRenaming
    , Map.insert right skolem rightRenaming
    , finalNext
    )
pairBinders _ _ _ = Nothing

renameOverlapVariables
  :: Ord typeVariable
  => Map (OverlapVariable typeVariable) (OverlapVariable typeVariable)
  -> Type (OverlapVariable typeVariable)
  -> Type (OverlapVariable typeVariable)
renameOverlapVariables renaming = fmap $ \variable ->
  Map.findWithDefault variable variable renaming

isBindable :: OverlapVariable typeVariable -> Bool
isBindable variable = case variable of
  BindableInstanceVariable{} -> True
  LexicalForallVariable{} -> False
  PairedForallSkolem{} -> False
  FreeInstanceVariable{} -> False

containsPairedForallSkolem
  :: Type (OverlapVariable typeVariable)
  -> Bool
containsPairedForallSkolem source = case source of
  TypeVariable PairedForallSkolem{} -> True
  TypeVariable{} -> False
  TypeConstructor{} -> False
  TypeApplication function argument ->
    containsPairedForallSkolem function
      || containsPairedForallSkolem argument
  FunctionType parameter result ->
    containsPairedForallSkolem parameter
      || containsPairedForallSkolem result
  TupleType _ elements -> any containsPairedForallSkolem elements
  ForallType binders constraints body ->
    any isPairedForallSkolem binders
      || any (any containsPairedForallSkolem . constraintArguments) constraints
      || containsPairedForallSkolem body
 where
  isPairedForallSkolem PairedForallSkolem{} = True
  isPairedForallSkolem _ = False

occursInOverlap
  :: Eq typeVariable
  => OverlapVariable typeVariable
  -> Type (OverlapVariable typeVariable)
  -> Bool
occursInOverlap variable source = case source of
  TypeVariable candidate -> variable == candidate
  TypeConstructor{} -> False
  TypeApplication function argument ->
    occursInOverlap variable function || occursInOverlap variable argument
  FunctionType parameter result ->
    occursInOverlap variable parameter || occursInOverlap variable result
  TupleType _ elements -> any (occursInOverlap variable) elements
  ForallType _ constraints body ->
    any (any (occursInOverlap variable) . constraintArguments) constraints
      || occursInOverlap variable body

substituteOverlap
  :: Eq typeVariable
  => OverlapVariable typeVariable
  -> Type (OverlapVariable typeVariable)
  -> Type (OverlapVariable typeVariable)
  -> Type (OverlapVariable typeVariable)
substituteOverlap variable replacement source = case source of
  TypeVariable candidate
    | variable == candidate -> replacement
    | otherwise -> source
  TypeConstructor{} -> source
  TypeApplication function argument -> TypeApplication
    (substituteOverlap variable replacement function)
    (substituteOverlap variable replacement argument)
  FunctionType parameter result -> FunctionType
    (substituteOverlap variable replacement parameter)
    (substituteOverlap variable replacement result)
  TupleType boxity elements -> TupleType boxity
    $ map (substituteOverlap variable replacement) elements
  ForallType binders constraints body -> ForallType binders
    (map (fmap $ substituteOverlap variable replacement) constraints)
    $ substituteOverlap variable replacement body

zonkOverlap
  :: Ord typeVariable
  => OverlapSubstitutions typeVariable
  -> Type (OverlapVariable typeVariable)
  -> Type (OverlapVariable typeVariable)
zonkOverlap substitutions source = case source of
  TypeVariable variable -> case Map.lookup variable substitutions of
    Nothing -> source
    Just replacement -> zonkOverlap substitutions replacement
  TypeConstructor{} -> source
  TypeApplication function argument -> TypeApplication
    (zonkOverlap substitutions function)
    (zonkOverlap substitutions argument)
  FunctionType parameter result -> FunctionType
    (zonkOverlap substitutions parameter)
    (zonkOverlap substitutions result)
  TupleType boxity elements -> TupleType boxity
    $ map (zonkOverlap substitutions) elements
  ForallType binders constraints body -> ForallType binders
    (map (fmap $ zonkOverlap substitutions) constraints)
    $ zonkOverlap substitutions body

canonicalizeInstanceHead
  :: Ord typeVariable
  => [typeVariable]
  -> Constraint (Type typeVariable)
  -> Constraint (Type (AlphaVariable typeVariable))
canonicalizeInstanceHead variables =
  alphaNormalizeConstraintWithOuter FirstOccurrenceBinderSlots variables
    . fmap canonicalizeType
