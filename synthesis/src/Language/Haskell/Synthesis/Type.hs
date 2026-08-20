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
  , variableIdentity
  , isFlexibleVariable
  , flexibleVariableIdentity
  , rigidVariableIdentity
  , mapFlexibleVariable
  , foldFlexibleVariable
  , foldRigidVariable
  , Type (..)
  , TypeError (..)
  , FreshVariableAllocator
  , SubstitutionError (..)
  , BinderNormalizationError (..)
  , canonicalizeType
  , normalizeType
  , constructorApplicationForm
  , applyTypeArguments
  , applicationSpine
  , functionType
  , functionSpine
  , quantifyFreeVariables
  , implicitizeLeadingForalls
  , splitLeadingForalls
  , leadingForallVariables
  , typeBinderVariables
  , firstForallType
  , containsForall
  , constraintContainsForall
  , containsNestedForall
  , typeConstraints
  , typeConstructorHead
  , renameScopedVariables
  , uniquifyTypeBinders
  , freshenTypeBindersAwayFrom
  , substituteTypeVariables
  , substituteTypeVariablesBatch
  , validateTypeWidthsWith
  , validateType
  , freeVariablesInFirstOccurrenceOrder
  , freeVariables
  , constraintFreeVariables
  , typeConstructors
  , mapTypeNames
  ) where

import Data.Maybe (isJust)
import Control.DeepSeq (NFData)
import Control.Monad (foldM, unless, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
  ( StateT
  , evalStateT
  , get
  , put
  , runStateT
  )
import Data.Bifunctor (first)
import Data.Functor.Identity (Identity (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Generics (Generic)
import Language.Haskell.Synthesis.Collection
  ( distinctOn
  , firstDuplicate
  , firstPresent
  , observedListLength
  )
import Language.Haskell.Synthesis.Constraint
import Language.Haskell.Synthesis.Name

-- | Flexible inference variables and rigid skolems share an identity domain
-- without being unifiable by accident. Backends without this distinction can
-- use their identity type directly as the parameter of 'Type'.
data Variable identity
  = FlexibleVariable identity
    -- ^ A variable which unification may instantiate.
  | RigidVariable identity
    -- ^ A skolem or compatibility constant which unification must preserve.
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData identity => NFData (Variable identity)

-- | Recover the underlying identity, deliberately discarding its tag.
--
-- Use this only after the caller has handled the namespace distinction or at
-- a compatibility boundary whose historical key space did not contain tags.
variableIdentity :: Variable identity -> identity
variableIdentity variable = case variable of
  FlexibleVariable identity -> identity
  RigidVariable identity -> identity

-- | Whether this variable belongs to the flexible inference namespace.
isFlexibleVariable :: Variable identity -> Bool
isFlexibleVariable variable = case variable of
  FlexibleVariable{} -> True
  RigidVariable{} -> False

-- | Project only a flexible identity.
flexibleVariableIdentity :: Variable identity -> Maybe identity
flexibleVariableIdentity variable = case variable of
  FlexibleVariable identity -> Just identity
  RigidVariable{} -> Nothing

-- | Project only a rigid identity.
rigidVariableIdentity :: Variable identity -> Maybe identity
rigidVariableIdentity variable = case variable of
  FlexibleVariable{} -> Nothing
  RigidVariable identity -> Just identity

-- | Transform a flexible identity while preserving rigid search constants.
mapFlexibleVariable
  :: (identity -> identity)
  -> Variable identity
  -> Variable identity
mapFlexibleVariable transform variable = case variable of
  FlexibleVariable identity -> FlexibleVariable $ transform identity
  RigidVariable{} -> variable

-- | Fold a flexible identity into a monoid, ignoring rigid variables.
foldFlexibleVariable
  :: Monoid result
  => (identity -> result)
  -> Variable identity
  -> result
foldFlexibleVariable transform variable = case variable of
  FlexibleVariable identity -> transform identity
  RigidVariable{} -> mempty

-- | Fold a rigid identity into a monoid, ignoring flexible variables.
foldRigidVariable
  :: Monoid result
  => (identity -> result)
  -> Variable identity
  -> result
foldRigidVariable transform variable = case variable of
  FlexibleVariable{} -> mempty
  RigidVariable identity -> transform identity

-- | Shared source-type syntax. Applications are explicit and left-associated;
-- saturated arrows and tuples also have structural forms so both backends can
-- agree on their identity without recognizing constructor spellings.
data Type variable
  = TypeVariable variable
    -- ^ A backend-owned variable identity.
  | TypeConstructor Name
    -- ^ A validated nominal or structural constructor.
  | TypeApplication (Type variable) (Type variable)
    -- ^ Explicit type application.
  | FunctionType (Type variable) (Type variable)
    -- ^ Saturated function parameter and result.
  | TupleType Boxity [Type variable]
    -- ^ Saturated tuple elements in field order.
  | ForallType
      [variable]
      [Constraint (Type variable)]
      (Type variable)
    -- ^ One explicit binder layer, its context, and body.
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData variable => NFData (Type variable)

-- | Representation-local type failures independent of a declaration
-- environment. Kind, synonym, and known-class-arity checks belong to their
-- respective prepared environments.
data TypeError variable
  = InvalidTypeConstructor Name
    -- ^ Structural syntax occurred in an unsupported constructor position.
  | InvalidTupleTypeArity Boxity Int
    -- ^ A tuple element count is unsupported by the target GHC.
  | DuplicateForallVariable variable
    -- ^ One explicit binder layer repeats an identity.
  | InvalidTypeConstraint ConstraintError
    -- ^ A forall context contains an invalid nominal class identity.
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
    -- ^ No replacement could be allocated for the source binder.
  | FreshVariableAlreadyReserved variable variable
    -- ^ The allocator returned an identity already present in the reserved set.
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData variable => NFData (SubstitutionError variable)

-- | Failures while making every explicit binder globally unique.
--
-- The caller controls which binder identities its backend accepts. Allocation
-- failures reuse the same checked contract as substitution and synonym
-- freshening.
data BinderNormalizationError variable rejection
  = RejectedTypeBinder rejection
    -- ^ The backend does not permit this binder form.
  | DuplicateTypeBinder variable
    -- ^ A single explicit binder list repeats an accepted identity.
  | TypeBinderFresheningError (SubstitutionError variable)
    -- ^ A later scope could not be alpha-renamed safely.
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

-- | Expose structural functions and constructor-backed tuples as ordinary
-- constructor applications.
--
-- This is the applicative view used by higher-kinded unification: a flexible
-- head can unify with @(->)@ or an n-tuple constructor. It does not replace
-- 'canonicalizeType' as the storage convention. Unary unboxed tuples remain
-- structural because Haskell has no corresponding unary tuple constructor;
-- any other tuple shape for which 'tupleName' fails is likewise preserved so
-- this total transformation does not hide a later 'validateType' diagnostic.
constructorApplicationForm :: Type variable -> Type variable
constructorApplicationForm source = case source of
  TypeVariable{} -> source
  TypeConstructor{} -> source
  TypeApplication function argument -> TypeApplication
    (constructorApplicationForm function)
    (constructorApplicationForm argument)
  FunctionType parameter result -> applyConstructor functionName
    [ constructorApplicationForm parameter
    , constructorApplicationForm result
    ]
  TupleType boxity elements ->
    let convertedElements = map constructorApplicationForm elements
    in case tupleName boxity $ observedListLength maximumTupleArity elements of
      Right constructor -> applyConstructor constructor convertedElements
      Left _ -> TupleType boxity convertedElements
  ForallType variables constraints body -> ForallType variables
    (map (fmap constructorApplicationForm) constraints)
    (constructorApplicationForm body)
 where
  applyConstructor = applyTypeArguments . TypeConstructor

-- | Apply type arguments to a head in source order.
--
-- This is the raw left-associated constructor paired with 'applicationSpine';
-- it deliberately does not canonicalize saturated function or tuple
-- constructors. Use 'canonicalizeType' when the structural storage form is
-- required. The spine itself is built strictly so wide generated applications
-- do not retain a chain of pending folds; argument payloads remain lazy.
applyTypeArguments :: Type variable -> [Type variable] -> Type variable
applyTypeArguments = foldl' TypeApplication

-- | Decompose a left-associated type application into its head and arguments
-- in source order. Non-application types have an empty argument list.
applicationSpine :: Type variable -> (Type variable, [Type variable])
applicationSpine = collect []
  where
    collect arguments (TypeApplication function argument) =
      collect (argument : arguments) function
    collect arguments function = (function, arguments)

-- | Construct a right-associated function type from source-order parameters
-- and its final result. An empty parameter list returns the result unchanged.
functionType :: [Type variable] -> Type variable -> Type variable
functionType parameters result = foldr FunctionType result parameters

-- | Decompose a right-associated function type into its parameters and final
-- result in source order. A non-function type has no parameters. Quantifiers
-- or other structure below an arrow remain part of the final result.
functionSpine :: Type variable -> ([Type variable], Type variable)
functionSpine (FunctionType parameter result) =
  let (parameters, finalResult) = functionSpine result
  in (parameter : parameters, finalResult)
functionSpine result = ([], result)

-- | Quantify the selected free variables at the outermost scope.
--
-- Selected identities use their canonical 'Ord' order. When the source
-- already begins with a forall, new binders are prepended to that layer while
-- its constraints and body retain their exact structure. Otherwise one
-- explicit forall layer is introduced, including for a ground type. The
-- predicate lets a tagged namespace quantify flexible variables without
-- accidentally binding rigid skolems.
quantifyFreeVariables
  :: Ord variable
  => (variable -> Bool)
  -> Type variable
  -> Type variable
quantifyFreeVariables shouldQuantify source = case source of
  ForallType variables constraints body -> ForallType
    (selectedVariables ++ variables)
    constraints
    body
  _ -> ForallType selectedVariables [] source
 where
  selectedVariables = Set.toAscList
    $ Set.filter shouldQuantify
    $ freeVariables source

-- | Erase the complete leading forall chain into fresh implicit variables.
--
-- Every erased binder receives a fresh identity outside the protected and
-- complete source namespaces. This is necessary even for a binder that does
-- not initially collide: after its lexical scope is erased, retaining that
-- identity could conflate it with an enclosing declaration variable or with
-- a separately shadowed binder. Direct contexts from successive leading
-- foralls are retained in outer-to-inner order under one empty-binder forall.
-- A forall below any other type boundary remains untouched.
--
-- The caller controls admissible binder identities and fresh allocation. The
-- returned set contains the protected namespace, every source identity, and
-- all identities introduced while erasing the prenex chain.
implicitizeLeadingForalls
  :: Ord variable
  => (variable -> Maybe rejection)
  -> FreshVariableAllocator variable
  -> Set variable
  -> Type variable
  -> Either
      (BinderNormalizationError variable rejection)
      (Type variable, Set variable)
implicitizeLeadingForalls rejectBinder fresh protected source =
  go initiallyReserved [] source
 where
  initiallyReserved = protected `Set.union` allTypeVariables source

  go reserved contextChunks (ForallType binders embedded body) = do
    validateBinderList rejectBinder binders
    (renaming, reserved') <- first TypeBinderFresheningError
      $ runStateT (foldM allocateBinder Map.empty binders) reserved
    let renamedEmbedded = map
          (fmap $ renameScopedVariables renaming) embedded
        renamedBody = renameScopedVariables renaming body
    go reserved' (renamedEmbedded : contextChunks) renamedBody
  go reserved contextChunks body = Right
    ( case concat $ reverse contextChunks of
        [] -> body
        contexts -> ForallType [] contexts body
    , reserved
    )

  allocateBinder renaming binder = do
    replacement <- allocateFreshBinder fresh binder
    pure $ Map.insert binder replacement renaming

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

-- | Collect every explicit forall binder in structural source order.
--
-- At each forall its binder list precedes binders nested in direct constraint
-- arguments, which in turn precede binders in its body. Unlike a generic fold,
-- this observation excludes ordinary variable occurrences.
typeBinderVariables :: Type variable -> [variable]
typeBinderVariables = collect []
 where
  -- A continuation list preserves streaming while avoiding repeated append
  -- through deep application, function, tuple, or constraint spines.
  collect remaining typeExpression = case typeExpression of
    TypeVariable{} -> remaining
    TypeConstructor{} -> remaining
    TypeApplication function argument ->
      collect (collect remaining argument) function
    FunctionType parameter result ->
      collect (collect remaining result) parameter
    TupleType _ elements -> foldr (flip collect) remaining elements
    ForallType variables constraints body -> variables
      ++ foldr collectConstraint (collect remaining body) constraints

  collectConstraint constraint remaining =
    foldr (flip collect) remaining $ constraintArguments constraint

-- | Find the first explicit forall in structural source order.
--
-- The returned value is the complete quantified subtree, allowing callers to
-- retain an exact diagnostic witness instead of only a Boolean. A forall is
-- observed before inspecting its constraints or body.
firstForallType :: Type variable -> Maybe (Type variable)
firstForallType typeExpression = case typeExpression of
  TypeVariable{} -> Nothing
  TypeConstructor{} -> Nothing
  TypeApplication function argument -> firstPresent
    [firstForallType function, firstForallType argument]
  FunctionType parameter result -> firstPresent
    [firstForallType parameter, firstForallType result]
  TupleType _ elements -> firstPresent $ map firstForallType elements
  quantified@ForallType{} -> Just quantified

-- | Whether explicit quantification occurs anywhere in a type.
containsForall :: Type variable -> Bool
containsForall = isJust . firstForallType

-- | Whether explicit quantification occurs in any argument of a constraint.
--
-- Keeping this observation beside 'containsForall' gives validation and
-- backend compatibility layers one traversal authority for constraint types.
constraintContainsForall :: Constraint (Type variable) -> Bool
constraintContainsForall = any containsForall

-- | Whether explicit quantification occurs outside the leading prenex chain.
--
-- Quantification inside a leading constraint argument is nested even though
-- the constraint itself belongs to the prenex chain.
containsNestedForall :: Type variable -> Bool
containsNestedForall typeExpression =
  any constraintContainsForall constraints
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
    $ tupleName boxity $ observedListLength maximumTupleArity elements
  _ -> Nothing

rebuildApplication :: Type variable -> [Type variable] -> Type variable
rebuildApplication headType arguments = case headType of
  TypeConstructor name
    | nameSpecial name == Just FunctionConstructor
    , [parameter, result] <- arguments -> FunctionType parameter result
    | Just (TupleConstructor boxity arity) <- nameSpecial name
    , arity == length arguments -> TupleType boxity arguments
  _ -> applyTypeArguments headType arguments

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
      either (lift . Left) pure $ validateBinderList rejectBinder binders
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

data BinderNormalizationState variable = BinderNormalizationState
  { binderClaimed :: Set variable
  , binderReserved :: Set variable
  }

validateBinderList
  :: Ord variable
  => (variable -> Maybe rejection)
  -> [variable]
  -> Either (BinderNormalizationError variable rejection) ()
validateBinderList rejectBinder binders = do
  case firstRejectedBinder rejectBinder binders of
    Just rejection -> Left $ RejectedTypeBinder rejection
    Nothing -> Right ()
  case firstDuplicate binders of
    Just duplicate -> Left $ DuplicateTypeBinder duplicate
    Nothing -> Right ()

firstRejectedBinder
  :: (variable -> Maybe rejection)
  -> [variable]
  -> Maybe rejection
firstRejectedBinder _ [] = Nothing
firstRejectedBinder rejectBinder (binder : remaining) =
  case rejectBinder binder of
    Just rejection -> Just rejection
    Nothing -> firstRejectedBinder rejectBinder remaining

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
  runIdentity <$> substituteTypeVariablesTraversable
    fresh extraReserved substitutions (Identity source)

-- | Simultaneously substitute across a batch of types under one fresh-name
-- reservation set.
--
-- In addition to the guarantees of 'substituteTypeVariables', every source
-- identity is reserved before the first type is visited and binders allocated
-- for earlier types remain reserved for later ones. This lets constraints and
-- other grouped obligations preserve one variable namespace without packing
-- their members into a synthetic tuple or application node.
substituteTypeVariablesBatch
  :: Ord variable
  => FreshVariableAllocator variable
  -> Set variable
  -> Map.Map variable (Type variable)
  -> [Type variable]
  -> Either (SubstitutionError variable) [Type variable]
substituteTypeVariablesBatch = substituteTypeVariablesTraversable

substituteTypeVariablesTraversable
  :: (Ord variable, Traversable collection)
  => FreshVariableAllocator variable
  -> Set variable
  -> Map.Map variable (Type variable)
  -> collection (Type variable)
  -> Either (SubstitutionError variable) (collection (Type variable))
substituteTypeVariablesTraversable
    fresh extraReserved substitutions sources =
  evalStateT (traverse (substitute substitutions) sources) initialReserved
 where
  initialReserved = Set.unions
    [ extraReserved
    , foldMap allTypeVariables sources
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

-- | Canonicalize and structurally validate a source type in one boundary.
--
-- Saturated function and tuple constructors are rewritten before validation,
-- so the returned value has the shared storage form. Failures retain the
-- source-order traversal used by 'validateType'.
normalizeType
  :: Ord variable
  => Type variable
  -> Either (TypeError variable) (Type variable)
normalizeType source = do
  let canonical = canonicalizeType source
  validateCanonicalType canonical
  pure canonical

-- | Structurally validate a source type after applying the same
-- canonicalization used by 'normalizeType'.
validateType :: Ord variable => Type variable -> Either (TypeError variable) ()
validateType = void . normalizeType

-- | Preflight every finite-width type component before a full structural
-- validation or analysis.
--
-- Tuple spines are observed only through the first unsupported arity. At each
-- constraint, the caller's policy runs before its arguments are traversed;
-- this lets a known finite class arity reject a cyclic argument list. The
-- callback should check only properties that do not themselves require the
-- complete argument spine. All other name, binder, and type validation stays
-- with 'validateType'.
validateTypeWidthsWith
  :: (TypeError variable -> error)
  -> (Constraint (Type variable) -> Either error ())
  -> Type variable
  -> Either error ()
validateTypeWidthsWith mapTypeError validateConstraintWidth = inspect
 where
  inspect typeExpression = case typeExpression of
    TypeVariable{} -> Right ()
    TypeConstructor{} -> Right ()
    TypeApplication function argument ->
      inspect function >> inspect argument
    FunctionType parameter result -> inspect parameter >> inspect result
    TupleType boxity elements -> do
      let arity = observedListLength maximumTupleArity elements
      unless (validTupleArity boxity arity) $
        Left $ mapTypeError $ InvalidTupleTypeArity boxity arity
      mapM_ inspect elements
    ForallType _ constraints body -> do
      mapM_ inspectConstraint constraints
      inspect body

  inspectConstraint constraint = do
    validateConstraintWidth constraint
    mapM_ inspect $ constraintArguments constraint

-- The caller must supply a canonical type. Keeping this worker private makes
-- 'normalizeType' the only public operation that can return a checked value.
validateCanonicalType
  :: Ord variable
  => Type variable
  -> Either (TypeError variable) ()
validateCanonicalType typeExpression = case typeExpression of
  TypeVariable{} -> Right ()
  TypeConstructor name
    | validTypeConstructor name -> Right ()
    | otherwise -> Left $ InvalidTypeConstructor name
  TypeApplication function argument ->
    validateCanonicalType function >> validateCanonicalType argument
  FunctionType parameter result ->
    validateCanonicalType parameter >> validateCanonicalType result
  TupleType boxity elements -> do
    let arity = observedListLength maximumTupleArity elements
    unless (validTupleArity boxity arity) $
      Left $ InvalidTupleTypeArity boxity arity
    mapM_ validateCanonicalType elements
  ForallType variables constraints body -> do
    case firstDuplicate variables of
      Just variable -> Left $ DuplicateForallVariable variable
      Nothing -> Right ()
    mapM_ validateForallConstraint constraints
    validateCanonicalType body
 where
  validateForallConstraint constraint = do
    either (Left . InvalidTypeConstraint) Right $
      validateConstraint constraint
    mapM_ validateCanonicalType $ constraintArguments constraint

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

-- | Rewrite every nominal type-constructor and constraint-class identity.
--
-- Variables and structure are preserved. This is the shared projection used
-- when a frontend resolves canonical source names once and a backend stores a
-- deliberately renamed view of the same inventory.
mapTypeNames :: (Name -> Name) -> Type variable -> Type variable
mapTypeNames convert source = case source of
  TypeVariable{} -> source
  TypeConstructor name -> TypeConstructor $ convert name
  TypeApplication function argument -> TypeApplication
    (mapTypeNames convert function) (mapTypeNames convert argument)
  FunctionType parameter result -> FunctionType
    (mapTypeNames convert parameter) (mapTypeNames convert result)
  TupleType boxity elements -> TupleType boxity
    $ map (mapTypeNames convert) elements
  ForallType variables constraints body -> ForallType variables
    (map mapConstraintNames constraints)
    (mapTypeNames convert body)
 where
  mapConstraintNames (Constraint className arguments) = Constraint
    (convert className) $ map (mapTypeNames convert) arguments
