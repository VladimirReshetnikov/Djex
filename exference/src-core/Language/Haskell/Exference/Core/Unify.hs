-- | Exference's canonical type-unification implementation.
--
-- The public module owns the algorithms directly; there is no second
-- package-private implementation layer. The variants below differ only in
-- namespace ownership and substitution direction, which their contracts make
-- explicit.
module Language.Haskell.Exference.Core.Unify
  ( TypeEq (..)
  , unify
  , unifyDisjoint
  , unifyShared
  , unifyOffset
  , unifyRight
  , unifyRightEqs
  , unifyRightOffset
  )
where

import Control.Monad (foldM)
import Control.Monad.Trans.State.Strict (StateT (..), evalStateT, get, put)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Language.Haskell.Exference.Core.Internal.FlexibleIds
import Language.Haskell.Exference.Core.Internal.VariableSupply
  ( allocateFreshIdentifier
  , checkedAddIdentifier
  , freshSynthesisVariable
  , supplyFromIdentifiers
  )
import Language.Haskell.Exference.Core.Types
import qualified Language.Haskell.Synthesis.Type as SharedType
import qualified Language.Haskell.Synthesis.TypeAtom as SharedTypeAtom

-- | One equation @left ~ right@ for 'unifyRightEqs'.  The first type is the
-- pattern side whose flexible variables stay fixed; only flexible variables
-- of the second type may be bound.
data TypeEq = TypeEq !HsType !HsType

{-# INLINE unify #-}
-- | Backward-compatible name for 'unifyDisjoint'.
unify :: HsType -> HsType -> Maybe (Substs, Substs)
unify = unifyDisjoint

{-# INLINE unifyDisjoint #-}
-- | Unify types whose flexible identifiers belong to independent namespaces.
-- Returns one substitution per input. In the symmetric case, substituting the
-- right-hand (second) type will be preferred.
--
-- Examples:
--
-- @
-- unify v C = ([v => C], [])
-- unify C v = ([], [v => C])
-- unify v w = ([], [w => v])
-- @
unifyDisjoint :: HsType -> HsType -> Maybe (Substs, Substs)
unifyDisjoint rawLeft rawRight = do
  left <- canonicalUnificationType rawLeft
  right <- canonicalUnificationType rawRight
  taggedLeft <- tagType LeftVariable left
  taggedRight <- tagType RightVariable right
  substitutions <- solveTagged (const True)
    [(taggedLeft, taggedRight)] Map.empty
  projectTagged left right substitutions

{-# INLINE unifyShared #-}
-- | Unify types whose flexible identifiers already belong to one shared
-- namespace. Equal identifiers on the two sides denote the same metavariable,
-- so the occurs check spans both inputs. Use 'unifyDisjoint' instead when each
-- input has an independent namespace and therefore needs its own substitution.
unifyShared :: HsType -> HsType -> Maybe Substs
unifyShared rawLeft rawRight = do
  left <- canonicalUnificationType rawLeft
  right <- canonicalUnificationType rawRight
  -- Reusing one tag constructor is intentional: it makes a variable with the
  -- same identifier literally the same solver variable on both sides.
  taggedLeft <- tagType LeftVariable left
  taggedRight <- tagType LeftVariable right
  substitutions <- solveTagged (const True)
    [(taggedLeft, taggedRight)] Map.empty
  let variables = Set.toAscList $ Set.union
        (freeVars left)
        (freeVars right)
  projected <- mapM
    (projectVariableBinding plainIdentifier substitutions LeftVariable)
    variables
  pure $ IntMap.fromList $ catMaybes projected

{-# INLINE unifyOffset #-}
-- | 'unifyDisjoint' against a right-hand type whose free flexible variables
-- are first shifted by the offset carried in the 'HsTypeOffset'.  The second
-- returned substitution is keyed by, and applies to, the shifted right type;
-- an offset that would overflow a 'TVarId' yields 'Nothing'.
unifyOffset :: HsType -> HsTypeOffset -> Maybe (Substs, Substs)
unifyOffset left (HsTypeOffset right offset) =
  checkedOffsetType offset right >>= unifyDisjoint left

-- Symmetric and right-directed unification both need tagged variables. A
-- numeric ID on the left never aliases the same spelling on the right until
-- projection: symmetric search may bind either tag, while right-directed
-- matching treats every left tag as a rigid pattern variable.
data TaggedVariable
  = LeftVariable !TVarId
  | RightVariable !TVarId
  deriving (Eq, Ord)

-- Variables inside an opaque polytype retain both distinctions that ordinary
-- tagged nodes encode structurally: flexible variables belong to the left or
-- right unification namespace, while rigid variables are nominal constants
-- shared by both sides. Bound variables use the same representation in the
-- retained source tree, but 'TypeAtom' removes their spelling from equality.
data TaggedAtomVariable
  = TaggedAtomFlexible !TaggedVariable
  | TaggedAtomRigid !TVarId
  deriving (Eq, Ord)

-- Structural functions and constructor-backed tuples deliberately use the
-- same applicative kernel as their intrinsic constructors. Besides making the
-- two shared source spellings equivalent, this preserves higher-kinded
-- unification: a flexible application head can still be bound to @(->)@ or an
-- n-tuple constructor. A unary unboxed tuple has no corresponding constructor
-- name, so that one valid structural form retains an explicit tagged node.
-- 'untagTagged' restores the canonical structural form in either case.
data TaggedType
  = TaggedVar !TaggedVariable
  | TaggedConstant !TVarId
  | TaggedConstructor !QualifiedName
  | TaggedApplication !TaggedType !TaggedType
  | TaggedTuple !Boxity ![TaggedType]
  | TaggedOpaquePolytype !(SharedTypeAtom.TypeAtom TaggedAtomVariable)
  deriving (Eq)

type TaggedSubstitutions = Map.Map TaggedVariable TaggedType

-- | Canonicalize and structurally validate every public unifier input.
canonicalUnificationType :: HsType -> Maybe HsType
canonicalUnificationType = either (const Nothing) Just
  . SharedType.normalizeType

tagType :: (TVarId -> TaggedVariable) -> HsType -> Maybe TaggedType
tagType side = tag . SharedType.constructorApplicationForm
 where
  tag ty = case ty of
    TypeVar variable -> Just $ TaggedVar $ side variable
    TypeConstant constant -> Just $ TaggedConstant constant
    TypeCons constructor -> Just $ TaggedConstructor constructor
    TypeArrow{} -> Nothing
    TypeApp function argument ->
      TaggedApplication <$> tag function <*> tag argument
    -- The shared applicative view leaves unary unboxed tuples structural
    -- because Haskell has no corresponding unary constructor.
    TypeTuple boxity elements -> TaggedTuple boxity <$> mapM tag elements
    -- A quantified subterm is one first-order atom. The solver may bind a
    -- metavariable to the whole value, but never generates equations from its
    -- context or body. This is enough for impredicative constructor arguments
    -- without introducing higher-rank subsumption.
    quantified@TypeForallNative{} -> case SharedTypeAtom.mkTypeAtom quantified of
      Right sourceAtom -> TaggedOpaquePolytype <$> either (const Nothing) Just
        (SharedTypeAtom.mapTypeAtomVariables
          (tagAtomVariable side) sourceAtom)
      -- A vacuous forall is textually and semantically transparent. The shared
      -- atom constructor erases it and returns the remaining monotype.
      Left (SharedTypeAtom.MonomorphicTypeAtom monotype) -> tag monotype
      Left _ -> Nothing

tagAtomVariable
  :: (TVarId -> TaggedVariable)
  -> SynthesisVariable
  -> TaggedAtomVariable
tagAtomVariable side variable = case variable of
  SharedType.FlexibleVariable identifier ->
    TaggedAtomFlexible $ side identifier
  SharedType.RigidVariable identifier -> TaggedAtomRigid identifier

solveTagged
  :: (TaggedVariable -> Bool)
  -> [(TaggedType, TaggedType)]
  -> TaggedSubstitutions
  -> Maybe TaggedSubstitutions
solveTagged _ [] substitutions = Just substitutions
solveTagged bindable ((rawLeft, rawRight) : equations) substitutions = do
  left <- zonkTagged substitutions rawLeft
  right <- zonkTagged substitutions rawRight
  solveCurrent left right
 where
  solveCurrent left right
    | left == right = solveTagged bindable equations substitutions
    | TaggedVar variable <- right
    , bindable variable = bindTagged variable left
    | TaggedVar variable <- left
    , bindable variable = bindTagged variable right
    | TaggedConstant leftConstant <- left
    , TaggedConstant rightConstant <- right
    , leftConstant == rightConstant =
        solveTagged bindable equations substitutions
    | TaggedConstructor leftConstructor <- left
    , TaggedConstructor rightConstructor <- right
    , leftConstructor == rightConstructor =
        solveTagged bindable equations substitutions
    | TaggedApplication leftFunction leftArgument <- left
    , TaggedApplication rightFunction rightArgument <- right =
        solveTagged bindable
          ((leftFunction, rightFunction)
            : (leftArgument, rightArgument) : equations)
          substitutions
    | TaggedTuple leftBoxity leftElements <- left
    , TaggedTuple rightBoxity rightElements <- right
    , leftBoxity == rightBoxity
    , length leftElements == length rightElements =
        solveTagged bindable
          (zip leftElements rightElements ++ equations)
          substitutions
    -- Atom equality can become true after an independent outer equation
    -- substitutes one of their free variables. Solve those equations first,
    -- then compare the zonked atoms once; never inspect their internal shape.
    | TaggedOpaquePolytype{} <- left
    , TaggedOpaquePolytype{} <- right = do
        solved <- solveTagged bindable equations substitutions
        solvedLeft <- zonkTagged solved left
        solvedRight <- zonkTagged solved right
        if solvedLeft == solvedRight then Just solved else Nothing
    | otherwise = Nothing

  bindTagged variable replacement
    | occursTagged variable replacement = Nothing
    | otherwise = do
        substitutedEquations <- mapM
          (\(equationLeft, equationRight) -> (,)
            <$> substituteTagged variable replacement equationLeft
            <*> substituteTagged variable replacement equationRight)
          equations
        substitutedBindings <- traverse
          (substituteTagged variable replacement) substitutions
        solveTagged bindable substitutedEquations
          $ Map.insert variable replacement substitutedBindings

occursTagged :: TaggedVariable -> TaggedType -> Bool
occursTagged variable ty = case ty of
  TaggedVar candidate -> variable == candidate
  TaggedConstant{} -> False
  TaggedConstructor{} -> False
  TaggedApplication function argument ->
    occursTagged variable function || occursTagged variable argument
  TaggedTuple _ elements -> any (occursTagged variable) elements
  TaggedOpaquePolytype atom -> Set.member
    (TaggedAtomFlexible variable) $ SharedTypeAtom.typeAtomFreeVariables atom

substituteTagged
  :: TaggedVariable
  -> TaggedType
  -> TaggedType
  -> Maybe TaggedType
substituteTagged variable replacement ty = case ty of
  TaggedVar candidate
    | variable == candidate -> Just replacement
    | otherwise -> Just ty
  TaggedConstant{} -> Just ty
  TaggedConstructor{} -> Just ty
  TaggedApplication function argument -> TaggedApplication
    <$> substituteTagged variable replacement function
    <*> substituteTagged variable replacement argument
  TaggedTuple boxity elements -> TaggedTuple boxity
    <$> mapM (substituteTagged variable replacement) elements
  TaggedOpaquePolytype atom
    | TaggedAtomFlexible variable `Set.notMember`
        SharedTypeAtom.typeAtomFreeVariables atom -> Just ty
    | otherwise -> TaggedOpaquePolytype <$> either (const Nothing) Just
        (SharedTypeAtom.substituteTypeAtomVariables
          freshTaggedAtomVariable
          Set.empty
          (Map.singleton (TaggedAtomFlexible variable)
            $ taggedTypeAsAtomType replacement)
          atom)

zonkTagged :: TaggedSubstitutions -> TaggedType -> Maybe TaggedType
zonkTagged substitutions ty = case ty of
  TaggedVar variable -> maybe (Just ty) (zonkTagged substitutions)
    $ Map.lookup variable substitutions
  TaggedConstant{} -> Just ty
  TaggedConstructor{} -> Just ty
  TaggedApplication function argument -> TaggedApplication
    <$> zonkTagged substitutions function
    <*> zonkTagged substitutions argument
  TaggedTuple boxity elements -> TaggedTuple boxity
    <$> mapM (zonkTagged substitutions) elements
  TaggedOpaquePolytype atom -> foldM substituteFree ty
    $ Set.toAscList $ SharedTypeAtom.typeAtomFreeVariables atom
 where
  substituteFree current atomVariable = case atomVariable of
    TaggedAtomRigid{} -> Just current
    TaggedAtomFlexible variable -> case Map.lookup variable substitutions of
      Nothing -> Just current
      Just replacement -> do
        resolved <- zonkTagged substitutions replacement
        substituteTagged variable resolved current

taggedTypeAsAtomType :: TaggedType -> SharedType.Type TaggedAtomVariable
taggedTypeAsAtomType ty = case ty of
  TaggedVar variable -> SharedType.TypeVariable
    $ TaggedAtomFlexible variable
  TaggedConstant identifier -> SharedType.TypeVariable
    $ TaggedAtomRigid identifier
  TaggedConstructor constructor -> SharedType.TypeConstructor constructor
  TaggedApplication function argument -> SharedType.TypeApplication
    (taggedTypeAsAtomType function) (taggedTypeAsAtomType argument)
  TaggedTuple boxity elements -> SharedType.TupleType boxity
    $ map taggedTypeAsAtomType elements
  TaggedOpaquePolytype atom -> SharedTypeAtom.typeAtomType atom

freshTaggedAtomVariable
  :: Set.Set TaggedAtomVariable
  -> TaggedAtomVariable
  -> Maybe TaggedAtomVariable
freshTaggedAtomVariable reserved old = wrap . fst
  <$> allocateFreshIdentifier (supplyFromIdentifiers matchingIdentifiers)
 where
  (matchingIdentifiers, wrap) = case old of
    TaggedAtomFlexible LeftVariable{} ->
      ( [identifier
        | TaggedAtomFlexible (LeftVariable identifier) <- Set.toList reserved
        ]
      , TaggedAtomFlexible . LeftVariable
      )
    TaggedAtomFlexible RightVariable{} ->
      ( [identifier
        | TaggedAtomFlexible (RightVariable identifier) <- Set.toList reserved
        ]
      , TaggedAtomFlexible . RightVariable
      )
    TaggedAtomRigid{} ->
      ( [identifier
        | TaggedAtomRigid identifier <- Set.toList reserved
        ]
      , TaggedAtomRigid
      )

projectTagged
  :: HsType
  -> HsType
  -> TaggedSubstitutions
  -> Maybe (Substs, Substs)
projectTagged left right substitutions = do
  rightCanonical <- allocateRightVariables
    (Set.fromList leftVariables) rightVariables
  let
      canonicalRight variable = IntMap.findWithDefault variable variable
        rightCanonical
      -- Unlike the plain projection, right-side identities untag through
      -- their freshly allocated canonical spellings.
      externalIdentifier tagged = case tagged of
        LeftVariable variable -> variable
        RightVariable variable -> canonicalRight variable
      project side variables = catMaybes <$> mapM
        (projectVariableBinding externalIdentifier substitutions side)
        variables
  leftBindings <- project LeftVariable leftVariables
  rightBindings <- project RightVariable rightVariables
  pure
    ( IntMap.fromList leftBindings
    , IntMap.fromList rightBindings
    )
 where
  leftVariables = Set.toAscList $ freeVars left
  rightVariables = Set.toAscList $ freeVars right

-- Resolve one source-side variable through the solved substitutions and
-- retain a binding only when it resolved to something other than itself.
projectVariableBinding
  :: (TaggedVariable -> TVarId)
  -> TaggedSubstitutions
  -> (TVarId -> TaggedVariable)
  -> TVarId
  -> Maybe (Maybe (TVarId, HsType))
projectVariableBinding identifier substitutions side variable = do
  tagged <- zonkTagged substitutions $ TaggedVar $ side variable
  resolved <- untagTagged identifier tagged
  pure $ if resolved == TypeVar variable
    then Nothing
    else Just (variable, resolved)

-- Both solver tags project to their numeric identity. Entry points that can
-- only produce one tag still use this total projection, keeping the solver
-- invariant local and explicit.
plainIdentifier :: TaggedVariable -> TVarId
plainIdentifier tagged = case tagged of
  LeftVariable variable -> variable
  RightVariable variable -> variable

untagTagged
  :: (TaggedVariable -> TVarId)
  -> TaggedType
  -> Maybe HsType
untagTagged variableIdentifier tagged = SharedType.canonicalizeType
  <$> convert tagged
 where
  convert ty = case ty of
    TaggedVar variable -> Just $ TypeVar $ variableIdentifier variable
    TaggedConstant constant -> Just $ TypeConstant constant
    TaggedConstructor constructor -> Just $ TypeCons constructor
    TaggedApplication function argument -> TypeApp
      <$> convert function <*> convert argument
    TaggedTuple boxity elements -> TypeTuple boxity <$> mapM convert elements
    TaggedOpaquePolytype atom -> untagTypeAtom variableIdentifier atom

-- Erasing the solver-side tag is intentionally binder-aware. A bound variable
-- from one side may have the same numeric spelling as a free variable inserted
-- from the other; choosing binder names after reserving every mapped free name
-- prevents projection from capturing that variable.
untagTypeAtom
  :: (TaggedVariable -> TVarId)
  -> SharedTypeAtom.TypeAtom TaggedAtomVariable
  -> Maybe HsType
untagTypeAtom variableIdentifier atom = evalStateT
  (convert Map.empty $ SharedTypeAtom.typeAtomType atom)
  initialReserved
 where
  initialReserved = Set.map freeVariable
    $ SharedTypeAtom.typeAtomFreeVariables atom

  freeVariable atomVariable = case atomVariable of
    TaggedAtomFlexible variable -> SharedType.FlexibleVariable
      $ variableIdentifier variable
    TaggedAtomRigid identifier -> SharedType.RigidVariable identifier

  convert bindings source = case source of
    SharedType.TypeVariable variable -> pure $ SharedType.TypeVariable
      $ Map.findWithDefault (freeVariable variable) variable bindings
    SharedType.TypeConstructor constructor -> pure
      $ SharedType.TypeConstructor constructor
    SharedType.TypeApplication function argument -> SharedType.TypeApplication
      <$> convert bindings function
      <*> convert bindings argument
    SharedType.FunctionType parameter result -> SharedType.FunctionType
      <$> convert bindings parameter
      <*> convert bindings result
    SharedType.TupleType boxity elements -> SharedType.TupleType boxity
      <$> mapM (convert bindings) elements
    SharedType.ForallType variables constraints body -> do
      replacements <- mapM allocateBinder variables
      let nestedBindings = Map.fromList (zip variables replacements)
            `Map.union` bindings
      SharedType.ForallType replacements
        <$> mapM (traverse $ convert nestedBindings) constraints
        <*> convert nestedBindings body

  allocateBinder variable = do
    reserved <- get
    let preferred = freeVariable variable
    replacement <- if preferred `Set.notMember` reserved
      then pure preferred
      else StateT $ \current -> do
        fresh <- freshSynthesisVariable current preferred
        pure (fresh, current)
    put $ Set.insert replacement reserved
    pure replacement

allocateRightVariables
  :: Set.Set TVarId
  -> [TVarId]
  -> Maybe (IntMap.IntMap TVarId)
allocateRightVariables leftVariables rightVariables = fst
  <$> allocateCanonicalIdentifiers rightVariables
        (supplyFromIdentifiers leftVariables)

{-# INLINE unifyRight #-}
-- | Treat flexible variables in the first parameter as rigid pattern
-- identities and return bindings only for variables in the second parameter.
unifyRight :: HsType -> HsType -> Maybe Substs
unifyRight left right = unifyRightEqs [TypeEq left right]

{-# INLINE unifyRightEqs #-}
-- | Solve several 'TypeEq' equations simultaneously in the manner of
-- 'unifyRight': flexible variables on every left side stay fixed, and the
-- single returned substitution binds only flexible variables of the right
-- sides, which all share one namespace.  Any structurally invalid input or an
-- unsolvable equation yields 'Nothing'.
unifyRightEqs :: [TypeEq] -> Maybe Substs
unifyRightEqs rawEquations = do
  equations <- mapM canonicalEquation rawEquations
  taggedEquations <- mapM tagEquation equations
  substitutions <- solveTagged isRightVariable taggedEquations Map.empty
  projectRightSubstitutions equations substitutions
 where
  canonicalEquation (TypeEq left right) = TypeEq
    <$> canonicalUnificationType left
    <*> canonicalUnificationType right
  tagEquation (TypeEq left right) = (,)
    <$> tagType LeftVariable left
    <*> tagType RightVariable right
  isRightVariable variable = case variable of
    LeftVariable{} -> False
    RightVariable{} -> True

projectRightSubstitutions
  :: [TypeEq]
  -> TaggedSubstitutions
  -> Maybe Substs
projectRightSubstitutions equations substitutions = do
  projected <- mapM
    (projectVariableBinding plainIdentifier substitutions RightVariable)
    rightVariables
  pure $ IntMap.fromList $ catMaybes projected
 where
  rightVariables = Set.toAscList $ foldMap
    (\(TypeEq _ right) -> freeVars right) equations

{-# INLINE unifyRightOffset #-}
-- | 'unifyRight' against a right-hand type whose free flexible variables are
-- first shifted by the offset carried in the 'HsTypeOffset'.  The returned
-- substitution is keyed by, and applies to, the shifted right type; an offset
-- that would overflow a 'TVarId' yields 'Nothing'.
unifyRightOffset :: HsType -> HsTypeOffset -> Maybe Substs
unifyRightOffset left (HsTypeOffset right offset) =
  checkedOffsetType offset right >>= unifyRight left

checkedOffsetType :: TVarId -> HsType -> Maybe HsType
checkedOffsetType offset typeExpression = do
  pairs <- mapM shifted $ Set.toAscList $ freeVars typeExpression
  pure $ renameFlexibleType (IntMap.fromList pairs) typeExpression
 where
  shifted identifier = do
    result <- checkedAddIdentifier identifier offset
    pure (identifier, result)
