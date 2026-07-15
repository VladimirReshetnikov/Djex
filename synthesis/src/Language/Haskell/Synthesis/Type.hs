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
  , canonicalizeType
  , applicationSpine
  , renameScopedVariables
  , freshenTypeBindersAwayFrom
  , substituteTypeVariables
  , validateType
  , freeVariables
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
  )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Generics (Generic)
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

firstDuplicate :: Ord value => [value] -> Maybe value
firstDuplicate = go Set.empty
  where
    go _ [] = Nothing
    go seen (value : remaining)
      | value `Set.member` seen = Just value
      | otherwise = go (Set.insert value seen) remaining

freeVariables :: Ord variable => Type variable -> Set variable
freeVariables typeExpression = case typeExpression of
  TypeVariable variable -> Set.singleton variable
  TypeConstructor{} -> Set.empty
  TypeApplication function argument ->
    freeVariables function `Set.union` freeVariables argument
  FunctionType parameter result ->
    freeVariables parameter `Set.union` freeVariables result
  TupleType _ elements -> Set.unions $ map freeVariables elements
  ForallType variables constraints body ->
    (Set.unions
      (freeVariables body :
        [ freeVariables argument
        | constraint <- constraints
        , argument <- constraintArguments constraint
        ])) `Set.difference` Set.fromList variables

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
