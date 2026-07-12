{-# LANGUAGE DeriveGeneric #-}

-- | Backend-neutral kind checking for shared source types.
--
-- Inference variables are private to this module. Public assumptions and
-- results use 'GroundKind', whose uninhabited variable parameter makes an
-- accidentally unsolved kind unrepresentable.
module Language.Haskell.Synthesis.KindInference
  ( GroundKind
  , KindAssumptions (..)
  , TypeKindDeclaration (..)
  , KindInferenceError (..)
  , emptyKindAssumptions
  , checkTypesKinds
  , inferSharedVariableKinds
  , inferAcyclicTypeConstructorKinds
  ) where

import Control.Monad (foldM, unless, zipWithM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
  ( StateT
  , evalStateT
  , get
  , modify'
  )
import qualified Data.IntMap.Strict as IntMap
import Data.Graph (SCC (..), stronglyConnComp)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Void (Void, absurd)
import GHC.Generics (Generic)

import Language.Haskell.Synthesis.Constraint
import Language.Haskell.Synthesis.Kind
import Language.Haskell.Synthesis.Name
import Language.Haskell.Synthesis.Type

type GroundKind = Kind Void

data KindAssumptions = KindAssumptions
  { typeConstructorKinds :: Map Name GroundKind
  , classParameterKinds :: Map Name [GroundKind]
  }
  deriving (Eq, Show, Generic)

emptyKindAssumptions :: KindAssumptions
emptyKindAssumptions = KindAssumptions Map.empty Map.empty

-- | A declaration reduced to the information needed for kind inference.
-- Every inferred body must be a proper type. A synonym contributes its one
-- right-hand side; a datatype contributes all constructor fields; an empty
-- datatype contributes no bodies and consequently defaults unconstrained
-- parameters to @*@.
data TypeKindDeclaration variable
  = InferredTypeKind Name [variable] [Type variable]
  | DeclaredTypeKind Name GroundKind
  deriving (Eq, Ord, Show, Generic)

data KindInferenceError variable
  = DuplicateSharedVariable variable
  | DuplicateTypeConstructor Name
  | InvalidKindInferenceType (TypeError variable)
  | UnknownTypeVariable variable
  | UnknownTypeConstructor Name
  | UnknownClass Name
  | ClassArityMismatch Name Int Int
  | KindMismatch GroundKind GroundKind
  | InfiniteKind
  | RecursiveTypeDeclarations [Name]
  deriving (Eq, Ord, Show, Generic)

data InferenceKind
  = InferenceProper
  | InferenceFunction InferenceKind InferenceKind
  | InferenceVariable Int
  deriving (Eq, Show)

data InferenceState = InferenceState
  { nextVariable :: !Int
  , kindSolutions :: !(IntMap.IntMap InferenceKind)
  }

type Inference variable a =
  StateT InferenceState (Either (KindInferenceError variable)) a

initialState :: InferenceState
initialState = InferenceState 0 IntMap.empty

-- | Check several obligations in one inference scope. Free variables with the
-- same identity therefore receive one kind across every supplied type.
checkTypesKinds
  :: Ord variable
  => KindAssumptions
  -> [(GroundKind, Type variable)]
  -> Either (KindInferenceError variable) ()
checkTypesKinds assumptions obligations = do
  mapM_ (validateInferenceType . snd) obligations
  flip evalStateT initialState $ do
    variables <- allocateVariables $ Set.toAscList $ Set.unions
      [freeVariables typeExpression | (_, typeExpression) <- obligations]
    mapM_ (checkObligation assumptions variables) obligations

-- | Infer kinds for variables shared by several independently quantified
-- types. Variables outside the supplied shared set are fresh in each type;
-- this models class parameters shared across method signatures without
-- conflating each method's implicit local quantifiers.
inferSharedVariableKinds
  :: Ord variable
  => KindAssumptions
  -> [variable]
  -> [Type variable]
  -> Either (KindInferenceError variable) [(variable, GroundKind)]
inferSharedVariableKinds assumptions sharedVariables types = do
  mapM_ validateInferenceType types
  flip evalStateT initialState $ do
    case firstDuplicate sharedVariables of
      Just duplicate -> lift $ Left $ DuplicateSharedVariable duplicate
      Nothing -> pure ()
    shared <- allocateVariables sharedVariables
    mapM_ (checkTypeWithFreshLocals assumptions shared) types
    mapM (groundBinding shared) sharedVariables

validateInferenceType
  :: Ord variable
  => Type variable
  -> Either (KindInferenceError variable) ()
validateInferenceType = either (Left . InvalidKindInferenceType) Right
  . validateType

-- | Infer an acyclic declaration graph in dependency order. Rejecting every
-- recursive SCC is an explicit compatibility policy shared by Djinn's legacy
-- declarations and Exference's synonym expander; a future richer data layer
-- can admit recursive datatypes without weakening this operation.
inferAcyclicTypeConstructorKinds
  :: Ord variable
  => [TypeKindDeclaration variable]
  -> Either (KindInferenceError variable) (Map Name GroundKind)
inferAcyclicTypeConstructorKinds declarations = do
  case firstDuplicate $ map declarationName declarations of
    Just duplicate -> Left $ DuplicateTypeConstructor duplicate
    Nothing -> pure ()
  let components = stronglyConnComp
        [ (declaration, declarationName declaration,
            Set.toAscList $ declarationDependencies declaration)
        | declaration <- declarations
        ]
  case [map declarationName recursive |
      CyclicSCC recursive <- components] of
    cycleNames : _ -> Left $ RecursiveTypeDeclarations cycleNames
    [] -> foldM inferOne Map.empty
      [declaration | AcyclicSCC declaration <- components]
 where
  inferOne known declaration = case declaration of
    DeclaredTypeKind name kind -> pure $ Map.insert name kind known
    InferredTypeKind name parameters bodies -> do
      inferred <- inferSharedVariableKinds
        (emptyKindAssumptions {typeConstructorKinds = known})
        parameters bodies
      let kind = foldr (FunctionKind . snd) ProperTypeKind inferred
      pure $ Map.insert name kind known

declarationName :: TypeKindDeclaration variable -> Name
declarationName declaration = case declaration of
  InferredTypeKind name _ _ -> name
  DeclaredTypeKind name _ -> name

declarationDependencies :: TypeKindDeclaration variable -> Set.Set Name
declarationDependencies declaration = case declaration of
  InferredTypeKind _ _ bodies -> Set.unions $ map typeConstructors bodies
  DeclaredTypeKind{} -> Set.empty

checkObligation
  :: Ord variable
  => KindAssumptions
  -> Map variable InferenceKind
  -> (GroundKind, Type variable)
  -> Inference variable ()
checkObligation assumptions variables (expected, typeExpression) = do
  actual <- inferType assumptions variables typeExpression
  unify actual $ fromGroundKind expected

checkTypeWithFreshLocals
  :: Ord variable
  => KindAssumptions
  -> Map variable InferenceKind
  -> Type variable
  -> Inference variable ()
checkTypeWithFreshLocals assumptions shared typeExpression = do
  let locals = Set.toAscList $
        freeVariables typeExpression `Set.difference` Map.keysSet shared
  localBindings <- allocateVariables locals
  actual <- inferType assumptions (localBindings `Map.union` shared)
    typeExpression
  unify actual InferenceProper

inferType
  :: Ord variable
  => KindAssumptions
  -> Map variable InferenceKind
  -> Type variable
  -> Inference variable InferenceKind
inferType assumptions variables typeExpression = case typeExpression of
  TypeVariable variable -> case Map.lookup variable variables of
    Just kind -> pure kind
    Nothing -> lift $ Left $ UnknownTypeVariable variable
  TypeConstructor name -> case intrinsicKind name of
    Just kind -> pure $ fromGroundKind kind
    Nothing -> case Map.lookup name $ typeConstructorKinds assumptions of
      Just kind -> pure $ fromGroundKind kind
      Nothing -> lift $ Left $ UnknownTypeConstructor name
  TypeApplication function argument -> do
    functionKind <- inferType assumptions variables function
    argumentKind <- inferType assumptions variables argument
    resultKind <- freshKind
    unify functionKind $ InferenceFunction argumentKind resultKind
    pure resultKind
  FunctionType parameter result -> do
    checkProper parameter
    checkProper result
    pure InferenceProper
  TupleType _ elements -> do
    mapM_ checkProper elements
    pure InferenceProper
  ForallType binders constraints body -> do
    binderKinds <- allocateVariables binders
    let quantified = binderKinds `Map.union` variables
    mapM_ (checkConstraint assumptions quantified) constraints
    inferType assumptions quantified body
 where
  checkProper nested = do
    kind <- inferType assumptions variables nested
    unify kind InferenceProper

checkConstraint
  :: Ord variable
  => KindAssumptions
  -> Map variable InferenceKind
  -> Constraint (Type variable)
  -> Inference variable ()
checkConstraint assumptions variables constraint = do
  parameterKinds <- case Map.lookup (constraintClass constraint)
      (classParameterKinds assumptions) of
    Just kinds -> pure kinds
    Nothing -> lift $ Left $ UnknownClass $ constraintClass constraint
  let arguments = constraintArguments constraint
  unless (length parameterKinds == length arguments) $ lift $ Left $
    ClassArityMismatch (constraintClass constraint)
      (length parameterKinds) (length arguments)
  zipWithM_ checkArgument parameterKinds arguments
 where
  checkArgument expected argument = do
    actual <- inferType assumptions variables argument
    unify actual $ fromGroundKind expected

allocateVariables
  :: Ord variable
  => [variable]
  -> Inference variable (Map variable InferenceKind)
allocateVariables = foldM allocate Map.empty
  where
    allocate bindings variable = do
      kind <- freshKind
      pure $ Map.insert variable kind bindings

freshKind :: Inference variable InferenceKind
freshKind = do
  state <- get
  let variable = nextVariable state
  modify' $ \current -> current
    { nextVariable = variable + 1
    , kindSolutions = IntMap.insert variable
        (InferenceVariable variable) $ kindSolutions current
    }
  pure $ InferenceVariable variable

follow :: InferenceKind -> Inference variable InferenceKind
follow kind@(InferenceVariable variable) = do
  solutions <- kindSolutions <$> get
  case IntMap.lookup variable solutions of
    Nothing -> pure kind
    Just solution
      | solution == kind -> pure kind
      | otherwise -> do
          result <- follow solution
          modify' $ \state -> state
            { kindSolutions = IntMap.insert variable result
                $ kindSolutions state
            }
          pure result
follow kind = pure kind

unify
  :: InferenceKind
  -> InferenceKind
  -> Inference variable ()
unify left right = do
  resolvedLeft <- follow left
  resolvedRight <- follow right
  case (resolvedLeft, resolvedRight) of
    (InferenceProper, InferenceProper) -> pure ()
    (InferenceFunction leftParameter leftResult,
        InferenceFunction rightParameter rightResult) ->
      unify leftParameter rightParameter >> unify leftResult rightResult
    (InferenceVariable leftVariable, InferenceVariable rightVariable)
      | leftVariable == rightVariable -> pure ()
    (InferenceVariable variable, kind) -> bind variable kind
    (kind, InferenceVariable variable) -> bind variable kind
    _ -> do
      leftKind <- ground resolvedLeft
      rightKind <- ground resolvedRight
      lift $ Left $ KindMismatch leftKind rightKind

bind :: Int -> InferenceKind -> Inference variable ()
bind variable kind = do
  cyclic <- occurs variable kind
  if cyclic
    then lift $ Left InfiniteKind
    else modify' $ \state -> state
      { kindSolutions = IntMap.insert variable kind $ kindSolutions state }

occurs :: Int -> InferenceKind -> Inference variable Bool
occurs variable kind = do
  resolved <- follow kind
  case resolved of
    InferenceProper -> pure False
    InferenceFunction parameter result -> (||)
      <$> occurs variable parameter <*> occurs variable result
    InferenceVariable other -> pure $ variable == other

groundBinding
  :: Ord variable
  => Map variable InferenceKind
  -> variable
  -> Inference variable (variable, GroundKind)
groundBinding bindings variable = case Map.lookup variable bindings of
  Nothing -> lift $ Left $ UnknownTypeVariable variable
  Just kind -> do
    grounded <- ground kind
    pure (variable, grounded)

ground :: InferenceKind -> Inference variable GroundKind
ground kind = do
  resolved <- follow kind
  case resolved of
    InferenceProper -> pure ProperTypeKind
    InferenceFunction parameter result ->
      FunctionKind <$> ground parameter <*> ground result
    -- Haskell 98 defaults unconstrained kind variables to @*@.
    InferenceVariable _ -> pure ProperTypeKind

fromGroundKind :: GroundKind -> InferenceKind
fromGroundKind kind = case kind of
  ProperTypeKind -> InferenceProper
  FunctionKind parameter result -> InferenceFunction
    (fromGroundKind parameter) (fromGroundKind result)
  KindVariable impossible -> absurd impossible

intrinsicKind :: Name -> Maybe GroundKind
intrinsicKind name = case nameSpecial name of
  Just ListConstructor -> Just $ arrow ProperTypeKind ProperTypeKind
  Just FunctionConstructor -> Just $
    arrow ProperTypeKind $ arrow ProperTypeKind ProperTypeKind
  Just (TupleConstructor _ arity) -> Just $
    foldr arrow ProperTypeKind $ replicate arity ProperTypeKind
  _ -> Nothing
  where
    arrow = FunctionKind

firstDuplicate :: Ord value => [value] -> Maybe value
firstDuplicate = go Set.empty
  where
    go _ [] = Nothing
    go seen (value : remaining)
      | value `Set.member` seen = Just value
      | otherwise = go (Set.insert value seen) remaining
