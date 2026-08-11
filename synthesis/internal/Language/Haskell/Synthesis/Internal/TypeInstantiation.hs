{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Capture-safe one-way matching of a leading context-free scheme.
--
-- This is deliberately smaller than a unifier. Only binders from the source's
-- complete leading forall prefix may be solved; every variable in the actual
-- type remains a constant. Nested forall binders are paired with fresh private
-- skolems, preventing an outer selection from capturing a binder exposed only
-- while comparing a nested body.
module Language.Haskell.Synthesis.Internal.TypeInstantiation
  ( ContextFreeSchemeMatchError (..)
  , ContextFreeSchemeMatch
  , ContextFreeSchemeSelection
  , matchContextFreeScheme
  , contextFreeSchemeSelections
  , contextFreeSchemeSelectionFreeVariables
  , contextFreeSchemeSelectionVariable
  ) where

import Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Constraint (Constraint (..))
import Language.Haskell.Synthesis.Internal.Alpha
  ( AlphaVariable (..)
  , BinderSlotPolicy (PositionalBinderSlots)
  , alphaNormalizeTypeWith
  )
import Language.Haskell.Synthesis.Type
  ( Type (..)
  , TypeError
  , normalizeType
  )

-- | Failure precedence is source syntax, actual syntax, a contextual leading
-- scheme, then structural mismatch.
data ContextFreeSchemeMatchError variable
  = InvalidContextFreeSchemeSource (TypeError variable)
  | InvalidContextFreeSchemeActual (TypeError variable)
  | ContextualLeadingScheme
  | ContextFreeSchemeShapeMismatch
  deriving (Eq, Ord, Show, Generic)

instance NFData variable => NFData (ContextFreeSchemeMatchError variable)

-- | One inferred source-prefix selection in an intentionally private variable
-- namespace. Its public observations cannot reveal or manufacture that tree.
newtype ContextFreeSchemeSelection variable = ContextFreeSchemeSelection
  (Type (InstantiationVariable variable))
  deriving (Eq, Ord, Generic)

type role ContextFreeSchemeSelection nominal

instance NFData variable => NFData (ContextFreeSchemeSelection variable)

-- | Source-order selection vector. A vacuous prefix binder has 'Nothing'.
newtype ContextFreeSchemeMatch variable = ContextFreeSchemeMatch
  [Maybe (ContextFreeSchemeSelection variable)]
  deriving (Eq, Ord, Generic)

type role ContextFreeSchemeMatch nominal

instance NFData variable => NFData (ContextFreeSchemeMatch variable)

-- | Infer one exact instantiation of every used binder in the source's
-- complete context-free leading forall prefix.
--
-- Both inputs are structurally normalized first. The actual side is rigid:
-- its flexible variables are ordinary constants, never inference holes. A
-- source binder may select a whole closed polymorphic subtree, and repeated
-- selections compare modulo nested binder spelling. It may not select a
-- skolem introduced by opening an enclosing nested forall.
matchContextFreeScheme
  :: Ord variable
  => Type variable
  -> Type variable
  -> Either
      (ContextFreeSchemeMatchError variable)
      (ContextFreeSchemeMatch variable)
matchContextFreeScheme rawSource rawActual = do
  source <- either (Left . InvalidContextFreeSchemeSource) Right
    $ normalizeType rawSource
  actual <- either (Left . InvalidContextFreeSchemeActual) Right
    $ normalizeType rawActual
  let alphaSource = alphaNormalizeTypeWith PositionalBinderSlots
        $ canonicalInstantiationForm source
      alphaActual = alphaNormalizeTypeWith PositionalBinderSlots
        $ canonicalInstantiationForm actual
  (binderCount, patternBody) <- preparePattern alphaSource
  let actualBody = fmap actualVariable alphaActual
  solved <- maybe (Left ContextFreeSchemeShapeMismatch) Right
    $ solveEquations [(patternBody, actualBody)] emptySolverState
  let substitutions = solverSubstitutions solved
      selection slot = ContextFreeSchemeSelection . zonk substitutions
        <$> Map.lookup slot substitutions
  pure $ ContextFreeSchemeMatch
    [selection slot | slot <- naturalPrefix binderCount]

contextFreeSchemeSelections
  :: ContextFreeSchemeMatch variable
  -> [Maybe (ContextFreeSchemeSelection variable)]
contextFreeSchemeSelections (ContextFreeSchemeMatch selections) = selections

-- | Free actual-side variables in one selected type. Nested lexical binders
-- and the matcher's paired skolems are excluded.
contextFreeSchemeSelectionFreeVariables
  :: Ord variable
  => ContextFreeSchemeSelection variable
  -> Set variable
contextFreeSchemeSelectionFreeVariables
    (ContextFreeSchemeSelection selection) = foldMap freeVariable selection
 where
  freeVariable variable = case variable of
    InstantiationFree free -> Set.singleton free
    _ -> Set.empty

-- | Recognize the exact selection of one free actual-side type variable.
contextFreeSchemeSelectionVariable
  :: ContextFreeSchemeSelection variable
  -> Maybe variable
contextFreeSchemeSelectionVariable
    (ContextFreeSchemeSelection (TypeVariable variable)) = case variable of
  InstantiationFree free -> Just free
  _ -> Nothing
contextFreeSchemeSelectionVariable _ = Nothing

data InstantiationVariable variable
  = InstantiationBindable !Natural
  | InstantiationSourceLexical !Natural !Natural
  | InstantiationActualLexical !Natural !Natural
  | InstantiationFree variable
  | InstantiationPairedSkolem !Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData variable => NFData (InstantiationVariable variable)

type Substitutions variable =
  Map Natural (Type (InstantiationVariable variable))

data SolverState variable = SolverState
  { solverSubstitutions :: !(Substitutions variable)
  , solverNextPairedSkolem :: !Natural
  }

emptySolverState :: SolverState variable
emptySolverState = SolverState Map.empty 0

preparePattern
  :: Ord variable
  => Type (AlphaVariable variable)
  -> Either
      (ContextFreeSchemeMatchError variable)
      (Natural, Type (InstantiationVariable variable))
preparePattern = consume 0 Map.empty
 where
  consume next bindables source = case source of
    ForallType binders constraints body
      | not $ null constraints -> Left ContextualLeadingScheme
      | otherwise ->
          let slots = naturalPrefixFrom next $ fromIntegral $ length binders
              additions = Map.fromList $ zip binders slots
              updated = Map.union additions bindables
          in consume (next + fromIntegral (length binders)) updated body
    _ -> Right (next, fmap (patternVariable bindables) source)

patternVariable
  :: Ord variable
  => Map (AlphaVariable variable) Natural
  -> AlphaVariable variable
  -> InstantiationVariable variable
patternVariable bindables variable = case Map.lookup variable bindables of
  Just slot -> InstantiationBindable slot
  Nothing -> case variable of
    AlphaBoundVariable scope slot -> InstantiationSourceLexical scope slot
    AlphaFreeVariable free -> InstantiationFree free

actualVariable
  :: AlphaVariable variable
  -> InstantiationVariable variable
actualVariable variable = case variable of
  AlphaBoundVariable scope slot -> InstantiationActualLexical scope slot
  AlphaFreeVariable free -> InstantiationFree free

solveEquations
  :: Ord variable
  => [( Type (InstantiationVariable variable)
      , Type (InstantiationVariable variable)
      )]
  -> SolverState variable
  -> Maybe (SolverState variable)
solveEquations [] state = Just state
solveEquations ((rawPattern, rawActual) : equations) state
  | patternType == actualType = solveEquations equations state
  | TypeVariable (InstantiationBindable slot) <- patternType =
      bind slot actualType
  | TypeConstructor patternName <- patternType
  , TypeConstructor actualName <- actualType
  , patternName == actualName = solveEquations equations state
  | TypeApplication patternFunction patternArgument <- patternType
  , TypeApplication actualFunction actualArgument <- actualType =
      solveEquations
        ((patternFunction, actualFunction) :
          (patternArgument, actualArgument) : equations)
        state
  | FunctionType patternParameter patternResult <- patternType
  , FunctionType actualParameter actualResult <- actualType =
      solveEquations
        ((patternParameter, actualParameter) :
          (patternResult, actualResult) : equations)
        state
  | TupleType patternBoxity patternFields <- patternType
  , TupleType actualBoxity actualFields <- actualType
  , patternBoxity == actualBoxity
  , Just fieldEquations <- zipExactly patternFields actualFields =
      solveEquations (fieldEquations ++ equations) state
  | ForallType patternBinders patternConstraints patternBody <- patternType
  , ForallType actualBinders actualConstraints actualBody <- actualType
  , Just (openedEquations, openedState) <- openForalls
      patternBinders patternConstraints patternBody
      actualBinders actualConstraints actualBody state =
      solveEquations (openedEquations ++ equations) openedState
  | otherwise = Nothing
 where
  substitutions = solverSubstitutions state
  patternType = zonk substitutions rawPattern
  actualType = zonk substitutions rawActual

  bind slot replacement
    | occursIn slot replacement = Nothing
    | containsPairedSkolem replacement = Nothing
    | otherwise = solveEquations
        [ ( substitute slot replacement patternEquation
          , substitute slot replacement actualEquation
          )
        | (patternEquation, actualEquation) <- equations
        ]
        state
          { solverSubstitutions = Map.insert slot replacement
              $ Map.map (substitute slot replacement) substitutions
          }

openForalls
  :: Ord variable
  => [InstantiationVariable variable]
  -> [Constraint (Type (InstantiationVariable variable))]
  -> Type (InstantiationVariable variable)
  -> [InstantiationVariable variable]
  -> [Constraint (Type (InstantiationVariable variable))]
  -> Type (InstantiationVariable variable)
  -> SolverState variable
  -> Maybe
      ( [( Type (InstantiationVariable variable)
         , Type (InstantiationVariable variable)
         )]
      , SolverState variable
      )
openForalls patternBinders patternConstraints patternBody
    actualBinders actualConstraints actualBody state = do
  (patternRenaming, actualRenaming, nextSkolem) <- pairBinders
    (solverNextPairedSkolem state) patternBinders actualBinders
  constraintEquations <- constraintListEquations
    (map (fmap $ renameType patternRenaming) patternConstraints)
    (map (fmap $ renameType actualRenaming) actualConstraints)
  pure
    ( ( renameType patternRenaming patternBody
      , renameType actualRenaming actualBody
      ) : constraintEquations
    , state {solverNextPairedSkolem = nextSkolem}
    )

pairBinders
  :: Ord variable
  => Natural
  -> [InstantiationVariable variable]
  -> [InstantiationVariable variable]
  -> Maybe
      ( Map (InstantiationVariable variable)
          (InstantiationVariable variable)
      , Map (InstantiationVariable variable)
          (InstantiationVariable variable)
      , Natural
      )
pairBinders next [] [] = Just (Map.empty, Map.empty, next)
pairBinders next (patternBinder : patternRest)
    (actualBinder : actualRest) = do
  (patternRenaming, actualRenaming, finalNext) <- pairBinders
    (next + 1) patternRest actualRest
  let skolem = InstantiationPairedSkolem next
  pure
    ( Map.insert patternBinder skolem patternRenaming
    , Map.insert actualBinder skolem actualRenaming
    , finalNext
    )
pairBinders _ _ _ = Nothing

constraintListEquations
  :: [Constraint (Type variable)]
  -> [Constraint (Type variable)]
  -> Maybe [(Type variable, Type variable)]
constraintListEquations [] [] = Just []
constraintListEquations (Constraint patternClass patternArguments : patternRest)
    (Constraint actualClass actualArguments : actualRest)
  | patternClass == actualClass = do
      current <- zipExactly patternArguments actualArguments
      remaining <- constraintListEquations patternRest actualRest
      pure $ current ++ remaining
constraintListEquations _ _ = Nothing

zipExactly :: [left] -> [right] -> Maybe [(left, right)]
zipExactly [] [] = Just []
zipExactly (left : leftRest) (right : rightRest) =
  ((left, right) :) <$> zipExactly leftRest rightRest
zipExactly _ _ = Nothing

renameType
  :: Ord variable
  => Map variable variable
  -> Type variable
  -> Type variable
renameType renaming = fmap $ rename renaming

rename :: Ord variable => Map variable variable -> variable -> variable
rename renaming variable = Map.findWithDefault variable variable renaming

zonk
  :: Ord variable
  => Substitutions variable
  -> Type (InstantiationVariable variable)
  -> Type (InstantiationVariable variable)
zonk substitutions source = case source of
  TypeVariable (InstantiationBindable slot) -> case Map.lookup slot substitutions of
    Nothing -> source
    Just replacement -> zonk substitutions replacement
  TypeVariable{} -> source
  TypeConstructor{} -> source
  TypeApplication function argument -> TypeApplication
    (zonk substitutions function) (zonk substitutions argument)
  FunctionType parameter result -> FunctionType
    (zonk substitutions parameter) (zonk substitutions result)
  TupleType boxity fields -> TupleType boxity $ map (zonk substitutions) fields
  ForallType binders constraints body -> ForallType binders
    (map (fmap $ zonk substitutions) constraints)
    $ zonk substitutions body

substitute
  :: Eq variable
  => Natural
  -> Type (InstantiationVariable variable)
  -> Type (InstantiationVariable variable)
  -> Type (InstantiationVariable variable)
substitute selected replacement source = case source of
  TypeVariable (InstantiationBindable slot)
    | slot == selected -> replacement
    | otherwise -> source
  TypeVariable{} -> source
  TypeConstructor{} -> source
  TypeApplication function argument -> TypeApplication
    (substitute selected replacement function)
    (substitute selected replacement argument)
  FunctionType parameter result -> FunctionType
    (substitute selected replacement parameter)
    (substitute selected replacement result)
  TupleType boxity fields -> TupleType boxity
    $ map (substitute selected replacement) fields
  ForallType binders constraints body -> ForallType binders
    (map (fmap $ substitute selected replacement) constraints)
    $ substitute selected replacement body

occursIn
  :: Eq variable
  => Natural
  -> Type (InstantiationVariable variable)
  -> Bool
occursIn slot = any (== InstantiationBindable slot)

containsPairedSkolem
  :: Type (InstantiationVariable variable)
  -> Bool
containsPairedSkolem = any isPaired
 where
  isPaired InstantiationPairedSkolem{} = True
  isPaired _ = False

canonicalInstantiationForm :: Type variable -> Type variable
canonicalInstantiationForm source = case source of
  TypeVariable{} -> source
  TypeConstructor{} -> source
  TypeApplication function argument -> TypeApplication
    (canonicalInstantiationForm function)
    (canonicalInstantiationForm argument)
  FunctionType parameter result -> FunctionType
    (canonicalInstantiationForm parameter)
    (canonicalInstantiationForm result)
  TupleType boxity fields -> TupleType boxity
    $ map canonicalInstantiationForm fields
  ForallType [] [] body -> canonicalInstantiationForm body
  ForallType binders constraints body -> ForallType binders
    (map (fmap canonicalInstantiationForm) constraints)
    $ canonicalInstantiationForm body

naturalPrefix :: Natural -> [Natural]
naturalPrefix count = naturalPrefixFrom 0 count

naturalPrefixFrom :: Natural -> Natural -> [Natural]
naturalPrefixFrom _ 0 = []
naturalPrefixFrom start count = start : naturalPrefixFrom (start + 1) (count - 1)
