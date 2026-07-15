{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Backend-independent Haskell source types.
--
-- Search engines may retain richer internal types, but parsers and validated
-- declaration environments can meet at this representation. Declaration
-- bodies are intentionally absent: synonyms, data constructors, and opaque
-- types belong in a separate declaration layer rather than masquerading as
-- ordinary type expressions.
module Language.Haskell.Synthesis.Type
  ( Variable (..)
  , Type (..)
  , TypeError (..)
  , FreshVariableAllocator
  , SubstitutionError (..)
  , BinderNormalizationError (..)
  , canonicalizeType
  , applicationSpine
  , splitLeadingForalls
  , leadingForallVariables
  , containsForall
  , containsNestedForall
  , typeConstraints
  , typeConstructorHead
  , renameScopedVariables
  , uniquifyTypeBinders
  , freshenTypeBindersAwayFrom
  , substituteTypeVariables
  , validateType
  , freeVariablesInFirstOccurrenceOrder
  , freeVariables
  , constraintFreeVariables
  , typeConstructors
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (foldM, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
  ( StateT
  , evalStateT
  , get
  , put
  , runStateT
  )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Generics (Generic)
import Language.Haskell.Synthesis.Collection (distinctOn, firstDuplicate)
import Language.Haskell.Synthesis.Constraint
import Language.Haskell.Synthesis.Name

-- | Flexible inference variables and rigid skolems share an identity domain
-- without being unifiable by accident. Backends without this distinction can
-- use their identity type directly as the parameter of 'Type'.
data Variable identity
  = FlexibleVariable identity
  | RigidVariable identity
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData identity => NFData (Variable identity)

data Type variable
  = TypeVariable variable
  | TypeConstructor Name
  | TypeApplication (Type variable) (Type variable)
  | FunctionType (Type variable) (Type variable)
  | TupleType Boxity [Type variable]
  | ForallType
      [variable]
      [Constraint (Type variable)]
      (Type variable)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData variable => NFData (Type variable)

data TypeError variable
  = InvalidTypeConstructor Name
  | InvalidTupleTypeArity Boxity Int
  | DuplicateForallVariable variable
  | InvalidTypeConstraint ConstraintError
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData variable => NFData (TypeError variable)

-- | Choose a fresh replacement for a binder from the complete reserved set.
-- Returning 'Nothing' reports deterministic exhaustion to the caller.
type FreshVariableAllocator variable =
  Set variable -> variable -> Maybe variable

-- | Failures from binder freshening or capture-avoiding substitution.
--
-- Both cases identify the source binder that required alpha-renaming. The
-- second also records the invalid candidate returned by the allocator.
data SubstitutionError variable
  = FreshVariableSupplyExhausted variable
  | FreshVariableAlreadyReserved variable variable
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData variable => NFData (SubstitutionError variable)

-- | Failures while making every explicit binder globally unique.
--
-- The caller controls which binder identities its backend accepts. Allocation
-- failures reuse the same checked contract as substitution and synonym
-- freshening.
data BinderNormalizationError variable rejection
  = RejectedTypeBinder rejection
  | DuplicateTypeBinder variable
  | TypeBinderFresheningError (SubstitutionError variable)
  deriving (Eq, Ord, Show, Generic)

instance
  (NFData variable, NFData rejection) =>
  NFData (BinderNormalizationError variable rejection)

-- | Give saturated function and tuple constructors one structural form.
-- Partial and over-applied constructors remain ordinary applications so kind
-- checking can diagnose them with the surrounding declaration environment.
canonicalizeType :: Type variable -> Type variable
canonicalizeType source = case source of
  TypeVariable{} -> source
  TypeConstructor name
    | Just (TupleConstructor boxity 0) <- nameSpecial name ->
        TupleType boxity []
    | otherwise -> source
  TypeApplication{} ->
    let (headType, arguments) = applicationSpine source
        canonicalHead = canonicalizeType headType
        canonicalArguments = map canonicalizeType arguments
    in rebuildApplication canonicalHead canonicalArguments
  FunctionType parameter result ->
    FunctionType (canonicalizeType parameter) (canonicalizeType result)
  TupleType boxity elements ->
    TupleType boxity $ map canonicalizeType elements
  ForallType variables constraints body -> ForallType variables
    (map (fmap canonicalizeType) constraints)
    (canonicalizeType body)

-- | Decompose a left-associated type application into its head and arguments
-- in source order. Non-application types have an empty argument list.
applicationSpine :: Type variable -> (Type variable, [Type variable])
applicationSpine = collect []
  where
    collect arguments (TypeApplication function argument) =
      collect (argument : arguments) function
    collect arguments function = (function, arguments)

-- | Split the complete leading prenex chain into its binders, direct
-- constraints, and residual body. All lists preserve source order. A forall
-- below an application, function, tuple, or constraint boundary is residual
-- structure rather than part of the leading chain. Emitting an outer binder
-- or constraint does not inspect subsequent layers.
splitLeadingForalls
  :: Type variable
  -> ([variable], [Constraint (Type variable)], Type variable)
splitLeadingForalls (ForallType variables constraints body) =
  let (nestedVariables, nestedConstraints, residualBody) =
        splitLeadingForalls body
  in ( variables ++ nestedVariables
     , constraints ++ nestedConstraints
     , residualBody
     )
splitLeadingForalls body = ([], [], body)

-- | Collect binders from the complete leading prenex chain in source order.
leadingForallVariables :: Type variable -> [variable]
leadingForallVariables typeExpression = variables
 where
  (variables, _, _) = splitLeadingForalls typeExpression

-- | Whether explicit quantification occurs anywhere in a type.
containsForall :: Type variable -> Bool
containsForall typeExpression = case typeExpression of
  TypeVariable{} -> False
  TypeConstructor{} -> False
  TypeApplication function argument ->
    containsForall function || containsForall argument
  FunctionType parameter result ->
    containsForall parameter || containsForall result
  TupleType _ elements -> any containsForall elements
  ForallType{} -> True

-- | Whether explicit quantification occurs outside the leading prenex chain.
--
-- Quantification inside a leading constraint argument is nested even though
-- the constraint itself belongs to the prenex chain.
containsNestedForall :: Type variable -> Bool
containsNestedForall typeExpression =
  any (any containsForall . constraintArguments) constraints
    || containsForall body
 where
  (_, constraints, body) = splitLeadingForalls typeExpression

-- | Collect every explicit class constraint embedded in a type.
--
-- Constraints are returned in source traversal order. At each forall, its
-- direct constraints precede constraints nested in their arguments, which in
-- turn precede constraints in the body.
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

-- | Find the nominal constructor at the head of forall and application
-- layers. Structural tuples are reported through their corresponding
-- constructor name. Other structural forms and variables have no nominal
-- head. This query does not canonicalize its input.
typeConstructorHead :: Type variable -> Maybe Name
typeConstructorHead typeExpression = case typeExpression of
  ForallType _ _ body -> typeConstructorHead body
  TypeApplication function _ -> typeConstructorHead function
  TypeConstructor name -> Just name
  TupleType boxity elements -> either (const Nothing) Just
    $ tupleName boxity $ length elements
  _ -> Nothing

rebuildApplication :: Type variable -> [Type variable] -> Type variable
rebuildApplication headType arguments = case headType of
  TypeConstructor name
    | nameSpecial name == Just FunctionConstructor
    , [parameter, result] <- arguments -> FunctionType parameter result
    | Just (TupleConstructor boxity arity) <- nameSpecial name
    , arity == length arguments -> TupleType boxity arguments
  _ -> foldl TypeApplication headType arguments

-- | Rename occurrences owned by a surrounding lexical scope. A nested
-- forall that binds the same nominal identity shadows the supplied renaming
-- in both its context and body.
renameScopedVariables
  :: Ord variable
  => Map.Map variable variable
  -> Type variable
  -> Type variable
renameScopedVariables renaming source = case source of
  TypeVariable variable -> TypeVariable
    $ Map.findWithDefault variable variable renaming
  TypeConstructor{} -> source
  TypeApplication function argument -> TypeApplication
    (renameScopedVariables renaming function)
    (renameScopedVariables renaming argument)
  FunctionType parameter result -> FunctionType
    (renameScopedVariables renaming parameter)
    (renameScopedVariables renaming result)
  TupleType boxity elements -> TupleType boxity
    $ map (renameScopedVariables renaming) elements
  ForallType binders constraints body ->
    let visible = foldr Map.delete renaming binders
    in ForallType binders
      (map (fmap $ renameScopedVariables visible) constraints)
      (renameScopedVariables visible body)

-- | Make every explicit forall binder globally unique and disjoint from free
-- or caller-protected identities.
--
-- Every source identity is reserved before traversal, so a replacement cannot
-- capture an occurrence that appears later. A binder is retained when it has
-- not already been claimed; otherwise the allocator chooses its replacement.
-- Constraints precede the body and sibling types retain structural source
-- order. The caller-supplied rejection query runs before duplicate checking at
-- each forall, preserving backend-specific binder admissibility precedence.
-- The returned set contains the protected namespace, all source identities,
-- and every allocated replacement.
uniquifyTypeBinders
  :: Ord variable
  => (variable -> Maybe rejection)
  -> FreshVariableAllocator variable
  -> Set variable
  -> Type variable
  -> Either
      (BinderNormalizationError variable rejection)
      (Type variable, Set variable)
uniquifyTypeBinders rejectBinder fresh protected source = do
  (normalized, finalState) <- runStateT (normalize source) initialState
  pure (normalized, binderReserved finalState)
 where
  initialState = BinderNormalizationState
    { binderClaimed = protected `Set.union` freeVariables source
    , binderReserved = protected `Set.union` allTypeVariables source
    }

  normalize typeExpression = case typeExpression of
    TypeVariable{} -> pure typeExpression
    TypeConstructor{} -> pure typeExpression
    TypeApplication function argument -> TypeApplication
      <$> normalize function
      <*> normalize argument
    FunctionType parameter result -> FunctionType
      <$> normalize parameter
      <*> normalize result
    TupleType boxity elements -> TupleType boxity <$> mapM normalize elements
    ForallType binders constraints body -> do
      case firstRejectedBinder binders of
        Just rejection -> lift $ Left $ RejectedTypeBinder rejection
        Nothing -> pure ()
      case firstDuplicate binders of
        Just duplicate -> lift $ Left $ DuplicateTypeBinder duplicate
        Nothing -> pure ()
      renaming <- foldM normalizeBinder Map.empty binders
      let renamedBinders = map
            (\binder -> Map.findWithDefault binder binder renaming)
            binders
          renamedConstraints = map
            (fmap $ renameScopedVariables renaming) constraints
          renamedBody = renameScopedVariables renaming body
      ForallType renamedBinders
        <$> mapM normalizeConstraint renamedConstraints
        <*> normalize renamedBody

  normalizeConstraint (Constraint className arguments) =
    Constraint className <$> mapM normalize arguments

  normalizeBinder renaming binder = do
    state <- get
    if binder `Set.notMember` binderClaimed state
      then do
        put state {binderClaimed = Set.insert binder $ binderClaimed state}
        pure renaming
      else do
        replacement <- allocateNormalizedBinder state binder
        updated <- get
        put updated
          { binderClaimed = Set.insert replacement $ binderClaimed updated }
        pure $ Map.insert binder replacement renaming

  allocateNormalizedBinder state binder =
    case fresh (binderReserved state) binder of
      Nothing -> lift $ Left $ TypeBinderFresheningError
        $ FreshVariableSupplyExhausted binder
      Just candidate
        | candidate `Set.member` binderReserved state -> lift $ Left
            $ TypeBinderFresheningError
            $ FreshVariableAlreadyReserved binder candidate
        | otherwise -> do
            put state
              { binderReserved = Set.insert candidate $ binderReserved state }
            pure candidate

  firstRejectedBinder [] = Nothing
  firstRejectedBinder (binder : remaining) = case rejectBinder binder of
    Just rejection -> Just rejection
    Nothing -> firstRejectedBinder remaining

data BinderNormalizationState variable = BinderNormalizationState
  { binderClaimed :: Set variable
  , binderReserved :: Set variable
  }

-- | Alpha-rename binders that collide with a protected namespace.
--
-- This is distinct from substitution: it freshens a colliding binder even
-- when no free occurrence beneath that binder will be replaced.  Synonym
-- expansion uses that stronger rule to distinguish variables introduced by
-- an alias body from variables in the complete source type.  Binders already
-- outside the protected set retain their identities, and nested shadowing is
-- traversed scope by scope.
freshenTypeBindersAwayFrom
  :: Ord variable
  => FreshVariableAllocator variable
  -> Set variable
     -- ^ Identities unavailable as replacements during the surrounding
     -- multi-step transformation. Membership here alone does not trigger
     -- binder renaming.
  -> Set variable
     -- ^ Source identities that alias-introduced binders must not reuse.
  -> Type variable
  -> Either (SubstitutionError variable) (Type variable)
freshenTypeBindersAwayFrom fresh extraReserved protected source =
  evalStateT (freshen source) initialReserved
 where
  initialReserved = Set.unions
    [ extraReserved
    , protected
    , allTypeVariables source
    ]

  freshen typeExpression = case typeExpression of
    TypeVariable{} -> pure typeExpression
    TypeConstructor{} -> pure typeExpression
    TypeApplication function argument -> TypeApplication
      <$> freshen function
      <*> freshen argument
    FunctionType parameter result -> FunctionType
      <$> freshen parameter
      <*> freshen result
    TupleType boxity elements -> TupleType boxity
      <$> mapM freshen elements
    ForallType binders constraints body -> do
      renaming <- foldM freshenProtectedBinder Map.empty binders
      let renamedBinders = map
            (\binder -> Map.findWithDefault binder binder renaming)
            binders
          renamedConstraints = map
            (fmap $ renameScopedVariables renaming) constraints
          renamedBody = renameScopedVariables renaming body
      ForallType renamedBinders
        <$> mapM freshenConstraint renamedConstraints
        <*> freshen renamedBody

  freshenConstraint (Constraint className arguments) = Constraint className
    <$> mapM freshen arguments

  freshenProtectedBinder renaming binder
    | binder `Set.notMember` protected = pure renaming
    | otherwise = do
        replacement <- allocateFreshBinder fresh binder
        pure $ Map.insert binder replacement renaming

-- | Simultaneously substitute free type variables without capturing any
-- free variable of a replacement.
--
-- The allocator receives the complete set of identities that it must avoid
-- and the binder being renamed. The explicit reservation set lets a caller
-- coordinate several otherwise independent transformations; variables in
-- the subject, substitution domain, and substitution range are reserved
-- automatically. Binders and constraint arguments are visited in source
-- order, making allocation and exhaustion deterministic.
--
-- A forall binder is alpha-renamed only when a substitution that is active
-- in that binder's lexical scope would introduce the same identity. In
-- particular, an irrelevant substitution does not consume fresh supply.
-- Every same-named binder on a nested shadowing chain is nevertheless
-- freshened: renaming only the innermost binder would expose the next binder
-- and let that binder capture the replacement. Replacement types are inserted
-- as-is rather than recursively rewritten, so the operation is simultaneous
-- rather than sequential.
substituteTypeVariables
  :: Ord variable
  => FreshVariableAllocator variable
  -> Set variable
  -> Map.Map variable (Type variable)
  -> Type variable
  -> Either (SubstitutionError variable) (Type variable)
substituteTypeVariables fresh extraReserved substitutions source =
  evalStateT (substitute substitutions source) initialReserved
 where
  initialReserved = Set.unions
    [ extraReserved
    , allTypeVariables source
    , Map.keysSet substitutions
    , foldMap allTypeVariables substitutions
    ]

  substitute active typeExpression = case typeExpression of
    TypeVariable variable -> pure
      $ Map.findWithDefault typeExpression variable active
    TypeConstructor{} -> pure typeExpression
    TypeApplication function argument -> TypeApplication
      <$> substitute active function
      <*> substitute active argument
    FunctionType parameter result -> FunctionType
      <$> substitute active parameter
      <*> substitute active result
    TupleType boxity elements -> TupleType boxity
      <$> mapM (substitute active) elements
    ForallType binders constraints body -> do
      let visible = foldr Map.delete active binders
          subjectVariables = freeVariables
            $ ForallType binders constraints body
          relevant = Map.restrictKeys visible subjectVariables
          rangeVariables = foldMap freeVariables relevant
      renaming <- foldM (freshenBinder rangeVariables) Map.empty binders
      let renamedBinders = map
            (\binder -> Map.findWithDefault binder binder renaming)
            binders
          renamedConstraints = map
            (fmap $ renameScopedVariables renaming)
            constraints
          renamedBody = renameScopedVariables renaming body
          belowBinders = foldr Map.delete relevant renamedBinders
      substitutedConstraints <- mapM
        (substituteConstraint belowBinders)
        renamedConstraints
      substitutedBody <- substitute belowBinders renamedBody
      pure $ ForallType renamedBinders substitutedConstraints substitutedBody

  substituteConstraint active constraint = Constraint
    (constraintClass constraint)
    <$> mapM (substitute active) (constraintArguments constraint)

  freshenBinder rangeVariables renaming binder
    | binder `Set.notMember` rangeVariables = pure renaming
    | otherwise = do
        replacement <- allocateFreshBinder fresh binder
        pure $ Map.insert binder replacement renaming

allocateFreshBinder
  :: Ord variable
  => FreshVariableAllocator variable
  -> variable
  -> StateT
      (Set variable)
      (Either (SubstitutionError variable))
      variable
allocateFreshBinder fresh binder = do
  reserved <- get
  replacement <- case fresh reserved binder of
    Nothing -> lift $ Left $ FreshVariableSupplyExhausted binder
    Just candidate
      | candidate `Set.member` reserved -> lift $ Left
          $ FreshVariableAlreadyReserved binder candidate
      | otherwise -> pure candidate
  put $ Set.insert replacement reserved
  pure replacement

allTypeVariables :: Ord variable => Type variable -> Set variable
allTypeVariables = foldMap Set.singleton

validateType :: Ord variable => Type variable -> Either (TypeError variable) ()
validateType source = validate $ canonicalizeType source
  where
    validate typeExpression = case typeExpression of
      TypeVariable{} -> Right ()
      TypeConstructor name
        | validTypeConstructor name -> Right ()
        | otherwise -> Left $ InvalidTypeConstructor name
      TypeApplication function argument ->
        validate function >> validate argument
      FunctionType parameter result ->
        validate parameter >> validate result
      TupleType boxity elements -> do
        unless (validTupleArity boxity $ length elements) $
          Left $ InvalidTupleTypeArity boxity $ length elements
        mapM_ validate elements
      ForallType variables constraints body -> do
        case firstDuplicate variables of
          Just variable -> Left $ DuplicateForallVariable variable
          Nothing -> Right ()
        mapM_ validateForallConstraint constraints
        validate body

    validateForallConstraint constraint = do
      either (Left . InvalidTypeConstraint) Right $
        validateConstraint constraint
      mapM_ validate $ constraintArguments constraint

    validTypeConstructor name =
      nameLexicalClass name == ConstructorLike &&
        nameSpecial name /= Just ConsConstructor

validTupleArity :: Boxity -> Int -> Bool
validTupleArity Boxed arity = withinBounds arity && arity /= 1
validTupleArity Unboxed arity = withinBounds arity

withinBounds :: Int -> Bool
withinBounds arity = arity >= 0 && arity <= maximumTupleArity

-- | Collect free variables once each, ordered by their first source
-- occurrence. Constraint arguments at a forall precede its body, matching
-- their textual position before @=>@. Emitting a variable does not inspect
-- the unused suffix of the type.
freeVariablesInFirstOccurrenceOrder
  :: Ord variable
  => Type variable
  -> [variable]
freeVariablesInFirstOccurrenceOrder = distinctOn id . collect Set.empty
 where
  collect bound typeExpression = case typeExpression of
    TypeVariable variable
      | variable `Set.member` bound -> []
      | otherwise -> [variable]
    TypeConstructor{} -> []
    TypeApplication function argument ->
      collect bound function ++ collect bound argument
    FunctionType parameter result ->
      collect bound parameter ++ collect bound result
    TupleType _ elements -> concatMap (collect bound) elements
    ForallType variables constraints body ->
      let nestedBound = bound `Set.union` Set.fromList variables
      in concatMap
          (concatMap (collect nestedBound) . constraintArguments)
          constraints
        ++ collect nestedBound body

-- | The set of variables free in a type.
freeVariables :: Ord variable => Type variable -> Set variable
freeVariables = Set.fromList . freeVariablesInFirstOccurrenceOrder

-- | The variables free across all arguments of one class constraint.
-- Quantifiers nested inside an argument retain their ordinary lexical scope.
constraintFreeVariables
  :: Ord variable
  => Constraint (Type variable)
  -> Set variable
constraintFreeVariables = foldMap freeVariables

-- | Collect nominal constructor references from a type. Structural function
-- and tuple forms have intrinsic kinds and therefore contribute only the
-- constructors mentioned by their elements.
typeConstructors :: Type variable -> Set Name
typeConstructors typeExpression = case typeExpression of
  TypeVariable{} -> Set.empty
  TypeConstructor name -> Set.singleton name
  TypeApplication function argument ->
    typeConstructors function `Set.union` typeConstructors argument
  FunctionType parameter result ->
    typeConstructors parameter `Set.union` typeConstructors result
  TupleType _ elements -> Set.unions $ map typeConstructors elements
  ForallType _ constraints body -> Set.unions
    (typeConstructors body :
      [ typeConstructors argument
      | constraint <- constraints
      , argument <- constraintArguments constraint
      ])
