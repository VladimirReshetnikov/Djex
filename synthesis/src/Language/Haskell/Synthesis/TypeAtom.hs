{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Validated opaque polymorphic types shared by both search engines.
--
-- A 'TypeAtom' retains the complete source tree for rendering, but its
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
  , typeAtomFreeVariables
  , mapTypeAtomVariables
  , substituteTypeAtomVariables
  , alphaEquivalentTypes
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Set (Set)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Constraint (Constraint (..))
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

data AlphaVariable variable
  = AlphaBoundVariable !Natural !Natural
  | AlphaFreeVariable variable
  deriving (Eq, Ord, Show, Generic)

instance NFData variable => NFData (AlphaVariable variable)

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

-- | The lexically free variables which an enclosing unifier may substitute.
-- Quantified variables never escape through this view.
typeAtomFreeVariables :: Ord variable => TypeAtom variable -> Set variable
typeAtomFreeVariables = freeVariables . atomSource

-- | Change the atom's variable representation and rebuild its checked key.
-- The result is checked because a non-injective mapping can merge binders.
mapTypeAtomVariables
  :: Ord target
  => (source -> target)
  -> TypeAtom source
  -> Either (TypeAtomError target) (TypeAtom target)
mapTypeAtomVariables convert = mkTypeAtom . fmap convert . atomSource

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
  alphaNormalizeType (atomCanonicalForm left)
    == alphaNormalizeType (atomCanonicalForm right)

sealTypeAtom :: Ord variable => Type variable -> TypeAtom variable
sealTypeAtom source = TypeAtom source
  $ TypeAtomKey $ alphaNormalizeType source

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

alphaNormalizeType
  :: Ord variable
  => Type variable
  -> Type (AlphaVariable variable)
alphaNormalizeType source = evalState (normalize Map.empty source) 0
 where
  normalize bindings typeExpression = case typeExpression of
    TypeVariable variable -> pure $ TypeVariable
      $ Map.findWithDefault (AlphaFreeVariable variable) variable bindings
    TypeConstructor name -> pure $ TypeConstructor name
    TypeApplication function argument -> TypeApplication
      <$> normalize bindings function
      <*> normalize bindings argument
    FunctionType parameter result -> FunctionType
      <$> normalize bindings parameter
      <*> normalize bindings result
    TupleType boxity elements -> TupleType boxity
      <$> mapM (normalize bindings) elements
    ForallType variables constraints body -> do
      scope <- allocateScope
      let canonicalBinders =
            [ AlphaBoundVariable scope position
            | (position, _) <- zip [0 ..] variables
            ]
          nestedBindings = Map.fromList (zip variables canonicalBinders)
            `Map.union` bindings
      canonicalConstraints <- mapM
        (normalizeConstraint nestedBindings) constraints
      canonicalBody <- normalize nestedBindings body
      pure $ ForallType canonicalBinders canonicalConstraints canonicalBody

  normalizeConstraint bindings (Constraint className arguments) =
    Constraint className <$> mapM (normalize bindings) arguments

  allocateScope :: State Natural Natural
  allocateScope = do
    scope <- get
    put $ scope + 1
    pure scope
