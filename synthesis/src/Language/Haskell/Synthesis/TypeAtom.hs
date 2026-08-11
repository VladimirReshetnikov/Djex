{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Validated opaque polymorphic types shared by both search engines.
--
-- A t'TypeAtom' retains the complete source tree for rendering, but its
-- equality and ordering ignore the spelling of lexically bound variables.
-- Free variables remain significant.  This is the deliberately small
-- semantic contract needed by engines which can carry a rank-N type without
-- opening, instantiating, or otherwise reasoning inside it.
module Language.Haskell.Synthesis.TypeAtom
  ( TypeAtom
  , TypeAtomError (..)
  , TypeAtomSubstitutionError (..)
  , TypeAtomKey
  , mkTypeAtom
  , typeAtomType
  , typeAtomKey
  , alphaTypeKey
  , typeAtomFreeVariables
  , mapTypeAtomVariables
  , substituteTypeAtomVariables
  , alphaEquivalentTypes
  , alphaEquivalentClosedTypes
  , isLeadingForallInstantiation
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Internal.Alpha
  ( AlphaVariable (..)
  , BinderSlotPolicy (PositionalBinderSlots)
  , alphaNormalizeTypeWith
  )
import Language.Haskell.Synthesis.Type
  ( FreshVariableAllocator
  , SubstitutionError
  , Type (..)
  , TypeError
  , canonicalizeType
  , freeVariables
  , normalizeType
  , substituteTypeVariables
  )

-- | A checked type whose outermost node is an explicit universal quantifier.
--
-- The constructor is hidden so an atom always contains canonical, structurally
-- valid shared syntax.  In particular, duplicate binders cannot make its alpha
-- identity ambiguous.
data TypeAtom variable = TypeAtom
  { atomSource :: !(Type variable)
  , atomKey :: !(TypeAtomKey variable)
  }
  deriving (Generic)

instance Show variable => Show (TypeAtom variable) where
  showsPrec precedence = showsPrec precedence . atomSource

instance NFData variable => NFData (TypeAtom variable)

-- | Why a shared type cannot become an opaque polymorphic atom.
data TypeAtomError variable
  = InvalidTypeAtom (TypeError variable)
    -- ^ The shared type itself is malformed.
  | MonomorphicTypeAtom (Type variable)
    -- ^ The canonical outermost node is not a 'ForallType'.
  | NonInjectiveTypeAtomVariableMapping variable
    -- ^ Distinct source identities were projected onto this target identity.
    -- Such a projection could capture a free variable or change which nested
    -- forall owns an occurrence.
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData variable => NFData (TypeAtomError variable)

-- | A failure while substituting the free variables of an opaque atom.
--
-- Substitution never opens the quantifier semantically.  It traverses the
-- retained source only to keep an enclosing first-order unifier honest about
-- the atom's free variables, and then rebuilds its cached alpha identity.
data TypeAtomSubstitutionError variable
  = TypeAtomSubstitutionFailure (SubstitutionError variable)
  | InvalidSubstitutedTypeAtom (TypeAtomError variable)
  deriving (Eq, Ord, Show, Generic)

instance NFData variable => NFData (TypeAtomSubstitutionError variable)

-- | Opaque alpha-normal comparison identity.
--
-- Bound variables are identified by their lexical scope and position in that
-- scope.  This is positional alpha-equivalence: renaming binders is ignored,
-- but reordering binders is not silently treated as a rename.  Arbitrary-size
-- counters prevent deep generated types from wrapping their identities.
newtype TypeAtomKey variable = TypeAtomKey
  (Type (AlphaVariable variable))
  deriving (Eq, Ord, Show, Generic)

instance NFData variable => NFData (TypeAtomKey variable)

instance Ord variable => Eq (TypeAtom variable) where
  left == right = atomKey left == atomKey right

instance Ord variable => Ord (TypeAtom variable) where
  compare left right = compare (atomKey left) (atomKey right)

-- | Validate, canonicalize, and seal one explicitly quantified type.
mkTypeAtom
  :: Ord variable
  => Type variable
  -> Either (TypeAtomError variable) (TypeAtom variable)
mkTypeAtom source = case normalizeType source of
  Left failure -> Left $ InvalidTypeAtom failure
  Right normalized -> case eraseVacuousForalls normalized of
    quantified@ForallType{} -> Right $ sealTypeAtom quantified
    monotype -> Left $ MonomorphicTypeAtom monotype

-- | Recover the canonical source tree retained for faithful rendering.
typeAtomType :: TypeAtom variable -> Type variable
typeAtomType = atomSource

-- | Recover the cached alpha-normal identity used by 'Eq' and 'Ord'.
typeAtomKey :: TypeAtom variable -> TypeAtomKey variable
typeAtomKey = atomKey

-- | Construct the same structural alpha identity for any shared type.
--
-- This is useful when a backend must keep an ordinary outer application as
-- one logical proposition while one of its children is an opaque polytype.
-- It is deliberately a key operation, not permission to open a nested atom.
alphaTypeKey :: Ord variable => Type variable -> TypeAtomKey variable
alphaTypeKey = TypeAtomKey
  . alphaNormalizeTypeWith PositionalBinderSlots
  . atomCanonicalForm

-- | The lexically free variables which an enclosing unifier may substitute.
-- Quantified variables never escape through this view.
typeAtomFreeVariables :: Ord variable => TypeAtom variable -> Set variable
typeAtomFreeVariables = freeVariables . atomSource

-- | Change the atom's variable representation and rebuild its checked key.
-- The mapping must be injective over every variable identity in the atom;
-- otherwise it could merge free variables, capture one beneath a binder, or
-- change the owner of an occurrence across nested scopes.
mapTypeAtomVariables
  :: (Ord source, Ord target)
  => (source -> target)
  -> TypeAtom source
  -> Either (TypeAtomError target) (TypeAtom target)
mapTypeAtomVariables convert atom = do
  -- Re-validating only the mapped tree catches duplicate binders in one
  -- binder list, but misses cross-scope capture.  Require the representation
  -- projection to preserve every distinct nominal identity before rebuilding
  -- the lexical tree.
  _ <- foldM rememberTarget Map.empty
    $ Set.toAscList $ foldMap Set.singleton $ atomSource atom
  mkTypeAtom $ fmap convert $ atomSource atom
 where
  rememberTarget seen source =
    let target = convert source
    in case Map.lookup target seen of
      Nothing -> Right $ Map.insert target source seen
      Just previous
        | previous == source -> Right seen
        | otherwise -> Left $ NonInjectiveTypeAtomVariableMapping target

-- | Capture-avoiding substitution of only the atom's free variables.
--
-- This is the operation engines need for impredicative first-order
-- unification.  The quantified type remains one inert atom; no equations are
-- generated from its body or context.
substituteTypeAtomVariables
  :: Ord variable
  => FreshVariableAllocator variable
  -> Set variable
  -> Map variable (Type variable)
  -> TypeAtom variable
  -> Either (TypeAtomSubstitutionError variable) (TypeAtom variable)
substituteTypeAtomVariables fresh reserved substitutions atom = do
  substituted <- first TypeAtomSubstitutionFailure
    $ substituteTypeVariables fresh reserved substitutions (atomSource atom)
  first InvalidSubstitutedTypeAtom $ mkTypeAtom substituted

-- | Compare two arbitrary shared types modulo lexical binder renaming.
--
-- Unlike 'mkTypeAtom', this helper does not require a leading forall or
-- validate its inputs.  Callers at an untrusted boundary should normalize the
-- types first.  It is useful for tests and for comparisons which already own a
-- stronger checked witness.
alphaEquivalentTypes
  :: Ord variable
  => Type variable
  -> Type variable
  -> Bool
alphaEquivalentTypes left right =
  alphaTypeKey left == alphaTypeKey right

-- | Compare lexically closed shared types across variable representations.
--
-- Both inputs are canonicalized and structurally validated before comparison.
-- Explicit forall binders are compared by lexical scope and position rather
-- than by their backend-owned identities.  If either input contains a
-- genuinely free variable, the comparison returns 'False'; this makes the
-- helper a safe bridge to closed generated visible type arguments without
-- conflating unrelated free-variable namespaces.
alphaEquivalentClosedTypes
  :: (Ord leftVariable, Ord rightVariable)
  => Type leftVariable
  -> Type rightVariable
  -> Bool
alphaEquivalentClosedTypes left right =
  case (normalizeType left, normalizeType right) of
    (Right normalizedLeft, Right normalizedRight) ->
      case (closeAlphaType normalizedLeft, closeAlphaType normalizedRight) of
        (Just closedLeft, Just closedRight) -> closedLeft == closedRight
        _ -> False
    _ -> False

-- | Check one exact, capture-avoiding instantiation of the first leading
-- forall binder.
--
-- The source, selected type, and claimed result are canonicalized and
-- structurally validated.  A leading @forall a b. body@ is instantiated one
-- slot at a time: selecting @chosen@ for @a@ leaves @forall b.@ in the result.
-- Constraints in the same forall scope are instantiated together with the
-- body.  Comparison ignores lexical binder spelling, but preserves all free
-- variable identities.
--
-- Bound variables from the source, selected type, and result are first moved
-- into disjoint internal namespaces.  Consequently a free variable in the
-- selected type cannot be captured by a same-spelled remaining source binder,
-- and no caller-provided fresh-name allocator is needed.
isLeadingForallInstantiation
  :: Ord variable
  => Type variable
  -> Type variable
  -> Type variable
  -> Bool
isLeadingForallInstantiation source selected result =
  case (normalizeType source, normalizeType selected, normalizeType result) of
    (Right normalizedSource, Right normalizedSelected, Right normalizedResult) ->
      case prepareInstantiationType SourceInstantiationBound normalizedSource of
        ForallType (selectedBinder : remainingBinders) constraints body ->
          let selectedType = prepareInstantiationType
                SelectedInstantiationBound normalizedSelected
              instantiate = replaceInstantiationVariable
                selectedBinder selectedType
              instantiatedConstraints = map (fmap instantiate) constraints
              instantiatedBody = instantiate body
              expected = case (remainingBinders, instantiatedConstraints) of
                ([], []) -> instantiatedBody
                _ -> ForallType remainingBinders
                  instantiatedConstraints instantiatedBody
              claimed = prepareInstantiationType
                ResultInstantiationBound normalizedResult
          in alphaEquivalentTypes expected claimed
        _ -> False
    _ -> False

data InstantiationVariable variable
  = SourceInstantiationBound !Natural !Natural
  | SelectedInstantiationBound !Natural !Natural
  | ResultInstantiationBound !Natural !Natural
  | InstantiationFree variable
  deriving (Eq, Ord)

prepareInstantiationType
  :: Ord variable
  => (Natural -> Natural -> InstantiationVariable variable)
  -> Type variable
  -> Type (InstantiationVariable variable)
prepareInstantiationType boundVariable = fmap convert
  . alphaNormalizeTypeWith PositionalBinderSlots
  . atomCanonicalForm
 where
  convert variable = case variable of
    AlphaBoundVariable scope slot -> boundVariable scope slot
    AlphaFreeVariable free -> InstantiationFree free

replaceInstantiationVariable
  :: Eq variable
  => variable
  -> Type variable
  -> Type variable
  -> Type variable
replaceInstantiationVariable selected replacement source = case source of
  TypeVariable variable
    | variable == selected -> replacement
    | otherwise -> source
  TypeConstructor{} -> source
  TypeApplication function argument -> TypeApplication
    (replaceInstantiationVariable selected replacement function)
    (replaceInstantiationVariable selected replacement argument)
  FunctionType parameter result -> FunctionType
    (replaceInstantiationVariable selected replacement parameter)
    (replaceInstantiationVariable selected replacement result)
  TupleType boxity elements -> TupleType boxity
    $ map (replaceInstantiationVariable selected replacement) elements
  ForallType variables constraints body
    | selected `elem` variables -> source
    | otherwise -> ForallType variables
        (map (fmap $ replaceInstantiationVariable selected replacement)
          constraints)
        (replaceInstantiationVariable selected replacement body)

closeAlphaType
  :: Ord variable
  => Type variable
  -> Maybe (Type (Natural, Natural))
closeAlphaType = traverse closeVariable
  . alphaNormalizeTypeWith PositionalBinderSlots
  . atomCanonicalForm
 where
  closeVariable variable = case variable of
    AlphaBoundVariable scope slot -> Just (scope, slot)
    AlphaFreeVariable{} -> Nothing

sealTypeAtom :: Ord variable => Type variable -> TypeAtom variable
sealTypeAtom source = TypeAtom source
  $ TypeAtomKey $ alphaNormalizeTypeWith PositionalBinderSlots source

-- Text rendering intentionally elides a forall with no binders or context.
-- Erasing those no-op nodes here gives sealed atoms a genuine textual
-- round-trip instead of retaining invisible structure.
eraseVacuousForalls :: Type variable -> Type variable
eraseVacuousForalls source = case source of
  TypeVariable{} -> source
  TypeConstructor{} -> source
  TypeApplication function argument -> TypeApplication
    (eraseVacuousForalls function) (eraseVacuousForalls argument)
  FunctionType parameter result -> FunctionType
    (eraseVacuousForalls parameter) (eraseVacuousForalls result)
  TupleType boxity elements -> TupleType boxity
    $ map eraseVacuousForalls elements
  ForallType [] [] body -> eraseVacuousForalls body
  ForallType variables constraints body -> ForallType variables
    (map (fmap eraseVacuousForalls) constraints)
    (eraseVacuousForalls body)

atomCanonicalForm :: Type variable -> Type variable
atomCanonicalForm = eraseVacuousForalls . canonicalizeType
