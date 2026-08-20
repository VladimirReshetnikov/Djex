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

import Language.Haskell.Synthesis.Internal.Alpha
  ( AlphaVariable (..)
  , BinderSlotPolicy (PositionalBinderSlots)
  , EquationPolicy (..)
  , EquationSolution (..)
  , ForallEquations (..)
  , alphaNormalizeTypeWith
  , emptyEquationSolution
  , eraseVacuousForalls
  , solveTypeEquations
  , zonkSolution
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
    $ solveTypeEquations matchPolicy [(patternBody, actualBody)]
        emptyEquationSolution
  let selection slot =
        ContextFreeSchemeSelection . zonkSolution matchPolicy solved
          <$> Map.lookup (InstantiationBindable slot)
                (solutionSubstitutions solved)
  pure $ ContextFreeSchemeMatch
    [selection slot | slot <- naturalPrefix binderCount]

-- | One entry per binder of the source's complete leading forall prefix, in
-- source binder order (outer forall first): @Just@ the inferred selection for
-- a binder the match solved, or @Nothing@ for a vacuous prefix binder that
-- the source body never mentions.
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

-- Deliberately smaller than a unifier: only the source's prefix binders may
-- be solved, every actual-side variable stays a constant, and nested forall
-- binders are paired with fresh private skolems.
matchPolicy :: EquationPolicy (InstantiationVariable variable)
matchPolicy = EquationPolicy
  { equationBinding = \patternType actualType -> case patternType of
      TypeVariable variable@InstantiationBindable{} ->
        Just (variable, actualType)
      _ -> Nothing
  , equationForalls =
      PairedForallEquations InstantiationPairedSkolem isPairedSkolem
  }
 where
  isPairedSkolem InstantiationPairedSkolem{} = True
  isPairedSkolem _ = False

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

canonicalInstantiationForm :: Type variable -> Type variable
canonicalInstantiationForm = eraseVacuousForalls

naturalPrefix :: Natural -> [Natural]
naturalPrefix count = naturalPrefixFrom 0 count

naturalPrefixFrom :: Natural -> Natural -> [Natural]
naturalPrefixFrom _ 0 = []
naturalPrefixFrom start count = start : naturalPrefixFrom (start + 1) (count - 1)
