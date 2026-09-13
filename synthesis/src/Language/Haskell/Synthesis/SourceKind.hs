-- | Exact kinds attached to lexical forall occurrences in a source type.
-- An annotation belongs to a structural site and binder slot, never merely
-- to a spelling. The checked value retains the original type, annotations,
-- inferred binder kinds and nominal kind assumptions together. Projecting
-- its type alone does not transport its explicit kind authority.
module Language.Haskell.Synthesis.SourceKind
  ( SourceTypeStep (..)
  , SourceKindAnnotation (..)
  , SourceKindVariable (..)
  , SourceKindError (..)
  , SourceTypeKinds
  , prepareSourceTypeKinds
  , sourceKindsType
  , sourceKindAnnotations
  , sourceBinderKinds
  , sourceKindAssumptions
  ) where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Language.Haskell.Synthesis.Constraint (Constraint (..))
import Language.Haskell.Synthesis.Kind (Kind (ProperTypeKind))
import Language.Haskell.Synthesis.KindInference
  ( GroundKind, KindAssumptions, KindInferenceError
  , inferVariableKindsForObligations )
import Language.Haskell.Synthesis.Type (Type (..), TypeError, validateType)

data SourceTypeStep
  = ApplicationFunction | ApplicationArgument
  | FunctionParameter | FunctionResult
  | TupleElement Int
  | ForallBody
  | ForallConstraintArgument Int Int
  deriving (Eq, Ord, Show)

data SourceKindAnnotation = SourceKindAnnotation
  { sourceKindPath :: [SourceTypeStep]
  , sourceKindSlot :: Int
  , sourceKindExpected :: GroundKind
  } deriving (Eq, Ord, Show)

-- Free source identities and opened lexical binders occupy disjoint spaces.
data SourceKindVariable variable
  = FreeSourceKindVariable variable
  | BoundSourceKindVariable [SourceTypeStep] Int
  deriving (Eq, Ord, Show)

data SourceKindError variable
  = InvalidSourceKindType (TypeError variable)
  | MissingSourceKindBinder [SourceTypeStep] Int
  | DuplicateSourceKindAnnotation [SourceTypeStep] Int
  | MissingInferredSourceKind (SourceKindVariable variable)
  | SourceKindInferenceFailure (KindInferenceError (SourceKindVariable variable))
  deriving (Eq, Ord, Show)

data SourceTypeKinds variable = SourceTypeKinds
  (Type variable)
  [SourceKindAnnotation]
  (Map.Map ([SourceTypeStep], Int) GroundKind)
  KindAssumptions
  deriving (Eq, Show)

sourceKindsType :: SourceTypeKinds variable -> Type variable
sourceKindsType (SourceTypeKinds ty _ _ _) = ty

sourceKindAnnotations :: SourceTypeKinds variable -> [SourceKindAnnotation]
sourceKindAnnotations (SourceTypeKinds _ annotations _ _) = annotations

sourceBinderKinds :: SourceTypeKinds variable -> Map.Map ([SourceTypeStep], Int) GroundKind
sourceBinderKinds (SourceTypeKinds _ _ kinds _) = kinds

sourceKindAssumptions :: SourceTypeKinds variable -> KindAssumptions
sourceKindAssumptions (SourceTypeKinds _ _ _ assumptions) = assumptions

prepareSourceTypeKinds
  :: Ord variable
  => KindAssumptions
  -> Type variable
  -> [SourceKindAnnotation]
  -> Either (SourceKindError variable) (SourceTypeKinds variable)
prepareSourceTypeKinds assumptions ty annotations = do
  first InvalidSourceKindType $ validateType ty
  let (opened, sites) = open [] Map.empty ty
      owners = Map.fromList [(site, BoundSourceKindVariable path slot) | site@(path, slot) <- sites]
  exact <- foldM (retain owners) Map.empty annotations
  let obligations = (ProperTypeKind, opened) :
        [(kind, TypeVariable variable) | (site, kind) <- Map.toList exact,
          Just variable <- [Map.lookup site owners]]
  inferred <- first SourceKindInferenceFailure $
    inferVariableKindsForObligations assumptions (Map.elems owners) obligations
  let solved = Map.fromList inferred
  kinds <- traverse (lookupSolved solved) owners
  pure $ SourceTypeKinds ty annotations kinds assumptions
 where
  retain owners retained (SourceKindAnnotation path slot kind)
    | Map.notMember site owners = Left $ MissingSourceKindBinder path slot
    | Map.member site retained = Left $ DuplicateSourceKindAnnotation path slot
    | otherwise = Right $ Map.insert site kind retained
   where site = (path, slot)
  lookupSolved solved variable = case Map.lookup variable solved of
    Just kind -> Right kind
    Nothing -> Left $ MissingInferredSourceKind variable

-- Retain every forall/context node while replacing only its bound variables
-- by lexical tokens. All occurrences, including class arguments and nested
-- forall bodies, then share the exact source binder's kind obligation.
open
  :: Ord variable
  => [SourceTypeStep]
  -> Map.Map variable (SourceKindVariable variable)
  -> Type variable
  -> (Type (SourceKindVariable variable), [([SourceTypeStep], Int)])
open path scope ty = case ty of
  TypeVariable variable ->
    (TypeVariable $ Map.findWithDefault (FreeSourceKindVariable variable) variable scope, [])
  TypeConstructor name -> (TypeConstructor name, [])
  TypeApplication function argument -> binary TypeApplication
    (ApplicationFunction, function) (ApplicationArgument, argument)
  FunctionType parameter result -> binary FunctionType
    (FunctionParameter, parameter) (FunctionResult, result)
  TupleType boxity fields ->
    let parts = [descend (TupleElement index) field | (index, field) <- zip [0..] fields]
    in (TupleType boxity $ map fst parts, concatMap snd parts)
  ForallType binders constraints body ->
    let sites = [(path, slot) | slot <- [0 .. length binders - 1]]
        nested = Map.union (Map.fromList
          [(binder, BoundSourceKindVariable path slot) | (binder, (_, slot)) <- zip binders sites]) scope
        argument constraintIndex argumentIndex = open
          (path ++ [ForallConstraintArgument constraintIndex argumentIndex]) nested
        contextParts =
          [ let parts = zipWith (argument index) [0..] arguments
            in (Constraint name $ map fst parts, concatMap snd parts)
          | (index, Constraint name arguments) <- zip [0..] constraints ]
        (openedBody, bodySites) = open (path ++ [ForallBody]) nested body
    in (ForallType [] (map fst contextParts) openedBody,
        sites ++ concatMap snd contextParts ++ bodySites)
 where
  descend step = open (path ++ [step]) scope
  binary constructor (leftStep, left) (rightStep, right) =
    let (leftType, leftSites) = descend leftStep left
        (rightType, rightSites) = descend rightStep right
    in (constructor leftType rightType, leftSites ++ rightSites)
