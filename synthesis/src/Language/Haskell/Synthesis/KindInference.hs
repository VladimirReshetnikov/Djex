{-# LANGUAGE DeriveGeneric #-}

-- | Backend-neutral kind checking for shared source types.
--
-- Inference variables are private to this module. Public assumptions and
-- results use 'GroundKind', whose uninhabited variable parameter makes an
-- accidentally unsolved kind unrepresentable.
module Language.Haskell.Synthesis.KindInference
  ( GroundKind
  , KindAssumptions (..)
  , KindInventoryPolicy (..)
  , TypeKindDeclaration (..)
  , KindInferenceError (..)
  , emptyKindAssumptions
  , checkTypesKinds
  , inferSharedVariableKinds
  , inferAcyclicTypeConstructorKinds
  , inferDeclarationKinds
  , inferDeclarationKindsWith
  ) where

import Control.Monad (foldM, unless, zipWithM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
  ( StateT (..)
  , evalStateT
  , get
  , modify'
  )
import qualified Data.IntMap.Strict as IntMap
import Data.Graph (SCC (..), stronglyConnComp)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Void (Void, absurd)
import GHC.Generics (Generic)

import Language.Haskell.Synthesis.Constraint
import Language.Haskell.Synthesis.Declaration
import Language.Haskell.Synthesis.Kind
import Language.Haskell.Synthesis.Name
import Language.Haskell.Synthesis.Type

type GroundKind = Kind Void

data KindAssumptions = KindAssumptions
  { typeConstructorKinds :: Map Name GroundKind
  -- | 'Nothing' denotes a generalized kind parameter, as used by modern
  -- poly-kinded classes such as @Typeable@.  This deliberately records only
  -- wholly generalized parameters: the current IR cannot express a partial
  -- scheme such as @k -> Type@ without fixing @k@.
  , classParameterKinds :: Map Name [Maybe GroundKind]
  }
  deriving (Eq, Show, Generic)

emptyKindAssumptions :: KindAssumptions
emptyKindAssumptions = KindAssumptions Map.empty Map.empty

data KindInventoryPolicy
  = ClosedKindInventory
  | OpenKindInventory
  deriving (Eq, Ord, Show, Generic)

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
  | DeclarationKindError Name (KindInferenceError variable)
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

data InferenceAssumptions = InferenceAssumptions
  { inferredTypeConstructorKinds :: Map Name InferenceKind
  , inferredClassParameterKinds :: Map Name [Maybe InferenceKind]
  }

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
    mapM_ (checkObligation (toInferenceAssumptions assumptions) variables)
      obligations

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
    mapM_ (checkTypeWithFreshLocals
      (toInferenceAssumptions assumptions) shared) types
    mapM (groundBinding shared) sharedVariables

-- | Infer and check every kind obligation in a sealed source inventory.
-- Datatypes are allocated before their fields are checked, so recursive and
-- mutually recursive datatype groups are valid. Type synonyms are also
-- allocated up front for joint kind inference, but cycles consisting only of
-- synonym expansion remain invalid.
--
-- The kind-variable parameter is 'Void' because every fixed kind produced by
-- this operation is ground. Generalized class parameters remain explicit as
-- 'Nothing'. Frontends with explicit kind syntax must resolve it before
-- crossing this boundary; frontends without such syntax use 'Nothing' on
-- their 'TypeParameter's.
inferDeclarationKinds
  :: Ord variable
  => [Declaration variable Void annotation]
  -> Either (KindInferenceError variable) KindAssumptions
inferDeclarationKinds = inferDeclarationKindsWith ClosedKindInventory

inferDeclarationKindsWith
  :: Ord variable
  => KindInventoryPolicy
  -> [Declaration variable Void annotation]
  -> Either (KindInferenceError variable) KindAssumptions
inferDeclarationKindsWith policy declarations = do
  mapM_ validateDeclarationTypes declarations
  rejectRecursiveSynonyms declarations
  externalClassKinds <- collectExternalClassKinds policy declarations
  flip evalStateT initialState $ do
    externalTypeKinds <- allocateExternalTypeKinds policy declarations
    (typeKinds, typeParameters) <-
      allocateTypeDeclarations externalTypeKinds declarations
    (classKinds, classParameters) <- allocateClassDeclarations declarations
    generalizedClasses <- stabilizeDefiningClassKinds
      externalClassKinds typeKinds typeParameters classParameters
      classKinds declarations
    let allClassKinds = generalizedClasses `Map.union` externalClassKinds
        assumptions = InferenceAssumptions typeKinds allClassKinds
    mapM_ (checkOperationalDeclaration assumptions
      typeParameters classParameters) declarations
    KindAssumptions
      <$> traverse ground typeKinds
      <*> traverse (mapM (traverse ground)) allClassKinds

-- | Infer class kinds without letting one use monomorphize an otherwise
-- generalized class parameter.  Each round snapshots the kinds currently
-- known from class-local structure. Unresolved parameters are exposed as
-- polymorphic ('Nothing'), while resolved shapes propagate through
-- superclass and method-constraint edges on the next round.
--
-- The process is monotone: unification can only replace an unresolved class
-- parameter by a fixed outer shape, so at most one observable transition per
-- parameter is required. Sharing the underlying inference variables also
-- propagates refinements beneath an already-known function-kind shape without
-- another observable map change.
stabilizeDefiningClassKinds
  :: Ord variable
  => Map Name [Maybe InferenceKind]
  -> Map Name InferenceKind
  -> Map Name (Map variable InferenceKind)
  -> Map Name (Map variable InferenceKind)
  -> Map Name [InferenceKind]
  -> [Declaration variable Void annotation]
  -> Inference variable (Map Name [Maybe InferenceKind])
stabilizeDefiningClassKinds externalClassKinds typeKinds typeParameters
    classParameters classKinds declarations = go
 where
  go = do
    before <- generalized
    let assumptions = InferenceAssumptions typeKinds
          $ before `Map.union` externalClassKinds
    mapM_ (checkDefiningDeclaration assumptions
      typeParameters classParameters) declarations
    after <- generalized
    -- Only the unresolved-root -> fixed-root transition changes whether a
    -- class use can constrain its argument. Variables nested in an exposed
    -- function-kind shape are shared and therefore refine immediately.
    if fmap (map isJust) after == fmap (map isJust) before
      then pure after
      else go

  generalized = traverse (mapM generalizeKind) classKinds

allocateExternalTypeKinds
  :: KindInventoryPolicy
  -> [Declaration variable Void annotation]
  -> Inference variable (Map Name InferenceKind)
allocateExternalTypeKinds ClosedKindInventory _ = pure Map.empty
allocateExternalTypeKinds OpenKindInventory declarations = do
  let declared = Set.fromList
        [ name
        | declaration <- declarations
        , name <- case declaration of
            TypeSynonymDeclaration _ typeName _ _ -> [typeName]
            DataTypeDeclaration _ typeName _ _ -> [typeName]
            AbstractTypeDeclaration _ typeName _ -> [typeName]
            _ -> []
        ]
      referenced = Set.unions
        $ map (Set.unions . map typeConstructors . declarationTypes)
        declarations
      external = Set.toAscList $ Set.filter
        ((== Nothing) . intrinsicKind)
        $ referenced `Set.difference` declared
  kinds <- mapM (const freshKind) external
  pure $ Map.fromAscList $ zip external kinds

collectExternalClassKinds
  :: KindInventoryPolicy
  -> [Declaration variable Void annotation]
  -> Either (KindInferenceError variable) (Map Name [Maybe InferenceKind])
collectExternalClassKinds ClosedKindInventory _ = Right Map.empty
collectExternalClassKinds OpenKindInventory declarations = do
  arities <- foldM insertArity Map.empty external
  pure $ replicatePolymorphic <$> arities
 where
  declared = Set.fromList
    [ name
    | ClassDeclaration _ name _ _ _ <- declarations
    ]
  external = filter ((`Set.notMember` declared) . constraintClass)
    $ concatMap declarationConstraints declarations
  insertArity arities constraint =
    let name = constraintClass constraint
        actual = length $ constraintArguments constraint
    in case Map.lookup name arities of
      Nothing -> Right $ Map.insert name actual arities
      Just expected
        | expected == actual -> Right arities
        | otherwise -> Left $ ClassArityMismatch name expected actual
  replicatePolymorphic arity = replicate arity Nothing

declarationConstraints
  :: Declaration variable kindVariable annotation
  -> [Constraint (Type variable)]
declarationConstraints declaration = direct ++
    concatMap typeConstraints (declarationTypes declaration)
 where
  direct = case declaration of
    ClassDeclaration _ _ _ superclasses _ -> superclasses
    InstanceDeclaration _ _ prerequisites headConstraint ->
      headConstraint : prerequisites
    _ -> []

typeConstraints :: Type variable -> [Constraint (Type variable)]
typeConstraints typeExpression = case typeExpression of
  TypeVariable{} -> []
  TypeConstructor{} -> []
  TypeApplication function argument ->
    typeConstraints function ++ typeConstraints argument
  FunctionType parameter result ->
    typeConstraints parameter ++ typeConstraints result
  TupleType _ elements -> concatMap typeConstraints elements
  ForallType _ constraints body -> constraints
    ++ concatMap (concatMap typeConstraints . constraintArguments) constraints
    ++ typeConstraints body

generalizeKind :: InferenceKind -> Inference variable (Maybe InferenceKind)
generalizeKind kind = do
  resolved <- follow kind
  pure $ case resolved of
    InferenceVariable _ -> Nothing
    fixed -> Just fixed

checkDefiningDeclaration
  :: Ord variable
  => InferenceAssumptions
  -> Map Name (Map variable InferenceKind)
  -> Map Name (Map variable InferenceKind)
  -> Declaration variable Void annotation
  -> Inference variable ()
checkDefiningDeclaration assumptions typeParameters classParameters
    declaration = case declaration of
  ValueDeclaration{} -> pure ()
  InstanceDeclaration{} -> pure ()
  _ -> checkWithContext assumptions typeParameters classParameters declaration

checkOperationalDeclaration
  :: Ord variable
  => InferenceAssumptions
  -> Map Name (Map variable InferenceKind)
  -> Map Name (Map variable InferenceKind)
  -> Declaration variable Void annotation
  -> Inference variable ()
checkOperationalDeclaration assumptions typeParameters classParameters
    declaration = case declaration of
  ValueDeclaration{} ->
    checkWithContext assumptions typeParameters classParameters declaration
  InstanceDeclaration{} ->
    checkWithContext assumptions typeParameters classParameters declaration
  _ -> pure ()

checkWithContext
  :: Ord variable
  => InferenceAssumptions
  -> Map Name (Map variable InferenceKind)
  -> Map Name (Map variable InferenceKind)
  -> Declaration variable Void annotation
  -> Inference variable ()
checkWithContext assumptions typeParameters classParameters declaration =
  StateT $ \state -> case runStateT
      (checkDeclaration assumptions typeParameters classParameters declaration)
      state of
    Left failure -> Left $ DeclarationKindError
      (declarationContext declaration) failure
    Right result -> Right result

declarationContext
  :: Declaration variable kindVariable annotation
  -> Name
declarationContext declaration = case declaration of
  TypeSynonymDeclaration _ name _ _ -> name
  DataTypeDeclaration _ name _ _ -> name
  AbstractTypeDeclaration _ name _ -> name
  ValueDeclaration signature -> valueName signature
  ClassDeclaration _ name _ _ _ -> name
  InstanceDeclaration _ _ _ headConstraint -> constraintClass headConstraint

validateDeclarationTypes
  :: Ord variable
  => Declaration variable Void annotation
  -> Either (KindInferenceError variable) ()
validateDeclarationTypes declaration = mapM_ validateInferenceType
  $ declarationTypes declaration

declarationTypes
  :: Declaration variable kindVariable annotation
  -> [Type variable]
declarationTypes declaration = case declaration of
  TypeSynonymDeclaration _ _ _ body -> [body]
  DataTypeDeclaration _ _ _ constructors ->
    concatMap constructorFields constructors
  AbstractTypeDeclaration{} -> []
  ValueDeclaration signature -> [valueType signature]
  ClassDeclaration _ _ _ superclasses methods ->
    concatMap constraintArguments superclasses ++ map valueType methods
  InstanceDeclaration _ _ prerequisites headConstraint ->
    concatMap constraintArguments $ headConstraint : prerequisites

rejectRecursiveSynonyms
  :: [Declaration variable Void annotation]
  -> Either (KindInferenceError variable) ()
rejectRecursiveSynonyms declarations = case
    [ map synonymName cycleDeclarations
    | CyclicSCC cycleDeclarations <- stronglyConnComp synonymNodes
    ] of
  cycleNames : _ -> Left $ RecursiveTypeDeclarations cycleNames
  [] -> Right ()
 where
  synonyms = Map.fromList
    [ (name, body)
    | TypeSynonymDeclaration _ name _ body <- declarations
    ]
  synonymNodes =
    [ ((name, body), name,
        Set.toAscList $ typeConstructors body `Set.intersection`
          Map.keysSet synonyms)
    | (name, body) <- Map.toAscList synonyms
    ]
  synonymName = fst

allocateTypeDeclarations
  :: Ord variable
  => Map Name InferenceKind
  -> [Declaration variable Void annotation]
  -> Inference variable
      (Map Name InferenceKind, Map Name (Map variable InferenceKind))
allocateTypeDeclarations initial = foldM allocate (initial, Map.empty)
 where
  allocate (kinds, parametersByName) declaration = case declaration of
    TypeSynonymDeclaration _ name parameters _ ->
      allocateInferred name parameters kinds parametersByName
    DataTypeDeclaration _ name parameters _ ->
      allocateInferred name parameters kinds parametersByName
    AbstractTypeDeclaration _ name kind -> pure
      (Map.insert name (fromGroundKind kind) kinds, parametersByName)
    _ -> pure (kinds, parametersByName)

  allocateInferred name parameters kinds parametersByName = do
    bindings <- allocateParameterKinds parameters
    let constructorKind = foldr InferenceFunction InferenceProper
          [ bindings Map.! parameterVariable parameter
          | parameter <- parameters
          ]
    pure
      ( Map.insert name constructorKind kinds
      , Map.insert name bindings parametersByName
      )

allocateClassDeclarations
  :: Ord variable
  => [Declaration variable Void annotation]
  -> Inference variable
      (Map Name [InferenceKind], Map Name (Map variable InferenceKind))
allocateClassDeclarations = foldM allocate (Map.empty, Map.empty)
 where
  allocate (kinds, parametersByName) declaration = case declaration of
    ClassDeclaration _ name parameters _ _ -> do
      bindings <- allocateParameterKinds parameters
      pure
        ( Map.insert name
            [ bindings Map.! parameterVariable parameter
            | parameter <- parameters
            ] kinds
        , Map.insert name bindings parametersByName
        )
    _ -> pure (kinds, parametersByName)

allocateParameterKinds
  :: Ord variable
  => [TypeParameter variable Void]
  -> Inference variable (Map variable InferenceKind)
allocateParameterKinds = foldM allocate Map.empty
 where
  allocate bindings parameter = do
    kind <- case parameterKind parameter of
      Nothing -> freshKind
      Just declared -> pure $ fromGroundKind declared
    pure $ Map.insert (parameterVariable parameter) kind bindings

checkDeclaration
  :: Ord variable
  => InferenceAssumptions
  -> Map Name (Map variable InferenceKind)
  -> Map Name (Map variable InferenceKind)
  -> Declaration variable Void annotation
  -> Inference variable ()
checkDeclaration assumptions typeParameters classParameters declaration =
  case declaration of
    TypeSynonymDeclaration _ name _ body ->
      checkProperWith (parameters typeParameters name) body
    DataTypeDeclaration _ name _ constructors -> mapM_
      (checkProperWith (parameters typeParameters name))
      (concatMap constructorFields constructors)
    AbstractTypeDeclaration{} -> pure ()
    ValueDeclaration signature -> checkWithFresh Map.empty
      $ valueType signature
    ClassDeclaration _ name _ superclasses methods -> do
      let shared = parameters classParameters name
      mapM_ (checkConstraint assumptions shared) superclasses
      mapM_ (checkWithFresh shared . valueType) methods
    InstanceDeclaration _ variables prerequisites headConstraint -> do
      bindings <- allocateVariables variables
      mapM_ (checkConstraint assumptions bindings)
        $ headConstraint : prerequisites
 where
  parameters table name = Map.findWithDefault Map.empty name table

  checkProperWith variables typeExpression = do
    actual <- inferType assumptions variables typeExpression
    unify actual InferenceProper

  checkWithFresh shared typeExpression =
    checkTypeWithFreshLocals assumptions shared typeExpression

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
  => InferenceAssumptions
  -> Map variable InferenceKind
  -> (GroundKind, Type variable)
  -> Inference variable ()
checkObligation assumptions variables (expected, typeExpression) = do
  actual <- inferType assumptions variables typeExpression
  unify actual $ fromGroundKind expected

checkTypeWithFreshLocals
  :: Ord variable
  => InferenceAssumptions
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
  => InferenceAssumptions
  -> Map variable InferenceKind
  -> Type variable
  -> Inference variable InferenceKind
inferType assumptions variables typeExpression = case typeExpression of
  TypeVariable variable -> case Map.lookup variable variables of
    Just kind -> pure kind
    Nothing -> lift $ Left $ UnknownTypeVariable variable
  TypeConstructor name -> case intrinsicKind name of
    Just kind -> pure $ fromGroundKind kind
    Nothing -> case Map.lookup name
        $ inferredTypeConstructorKinds assumptions of
      Just kind -> pure kind
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
  => InferenceAssumptions
  -> Map variable InferenceKind
  -> Constraint (Type variable)
  -> Inference variable ()
checkConstraint assumptions variables constraint = do
  parameterKinds <- case Map.lookup (constraintClass constraint)
      (inferredClassParameterKinds assumptions) of
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
    mapM_ (unify actual) expected

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

toInferenceAssumptions :: KindAssumptions -> InferenceAssumptions
toInferenceAssumptions assumptions = InferenceAssumptions
  { inferredTypeConstructorKinds =
      fromGroundKind <$> typeConstructorKinds assumptions
  , inferredClassParameterKinds =
      map (fmap fromGroundKind) <$> classParameterKinds assumptions
  }

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
