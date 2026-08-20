{-# LANGUAGE DeriveGeneric #-}


-- | The state of one Exference search branch: the t'SearchNode' record with
-- its pending t'TGoal's, lexical 'Scopes' of t'VarPBinding's, use counts, and
-- rigid, identifier, and depth bookkeeping, plus the substitution and
-- goal-construction helpers over it.  Scope structure itself is delegated to
-- "Language.Haskell.Exference.Core.Internal.Scope"; the engine in
-- "Language.Haskell.Exference.Core.Internal.Exference" steps these nodes.
module Language.Haskell.Exference.Core.Internal.ExferenceNode
  ( SearchNode (..)
  , ForallGoalMode (..)
  , TupleGoalMode (..)
  , TGoal (..)
  , Scopes
  , ScopeId
  , VarPBinding
  , varPVariable
  , varPResult
  , varPParameters
  , VarBinding (..)
  , goalApplySubst
  , scopesApplySubsts
  , mkGoals
  , scopeGetAllBindings
  , scopesAddPBinding
  , splitBinding
  , splitBindingWithParameters
  , initialScopeId
  , initialScopes
  )
where

import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.Core.Expression
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.Core.Score
import Language.Haskell.Exference.Core.Internal.VariableSupply
  (FlexibleIdSupply)
import Language.Haskell.Exference.Core.RigidInstantiation
  (RigidInstantiationPlan)
import Language.Haskell.Exference.Core.Internal.RigidScope
  (RigidScope)
import Language.Haskell.Exference.Core.Internal.ScopedConstraint
  (ScopedConstraint)
import qualified Language.Haskell.Exference.Core.Internal.Scope as Scope

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Sequence
import Numeric.Natural (Natural)

import Control.DeepSeq
import GHC.Generics

-- | A term variable (or expression hole) paired with its type.  Goals carry
-- one as the hole to fill; 'splitBinding' turns one into the scoped
-- t'VarPBinding' form.
data VarBinding = VarBinding {-# UNPACK #-} !TVarId HsType
 deriving (Generic)

-- | A scoped variable together with the exposed arrow prefix of its type.
-- A quantified result stays intact until a use site either forwards the exact
-- opaque value or explicitly instantiates its leading forall chain. Function
-- constraints become scope-annotated obligations in 'nodeConstraintGoals',
-- not properties of lexical term values.
data VarPBinding = VarPBinding
  { varPVariable :: !TVarId
  , varPResult :: HsType
  , varPParameters :: [HsType]
  }
  deriving (Generic, Show)


instance Show VarBinding where
  show (VarBinding vid ty) = showVar vid ++ " :-> "
    ++ showHsType mempty ty

varBindingApplySubsts :: Substs -> VarBinding -> VarBinding
varBindingApplySubsts substs (VarBinding v t) =
  VarBinding v (snd $ applySubsts substs t)

varPBindingApplySubsts :: Substs -> VarPBinding -> VarPBinding
varPBindingApplySubsts substitutions binding =
  splitBindingWithParameters
    (map (snd . applySubsts substitutions) $ varPParameters binding)
    (VarBinding (varPVariable binding)
      $ snd $ applySubsts substitutions $ varPResult binding)

-- | An opaque reference to one lexical scope of a search node; see
-- "Language.Haskell.Exference.Core.Internal.Scope".
type ScopeId = Scope.ScopeId

-- | The lexical-scope forest of a search node, holding one t'VarPBinding'
-- per locally bound term variable.
type Scopes = Scope.Scopes VarPBinding

-- | The root scope ID every fresh search starts from; the initial goal is
-- placed in this scope.
initialScopeId :: ScopeId
initialScopeId = Scope.initialScopeId

-- | The scope forest of a fresh search node: only the empty root scope.
initialScopes :: Scopes
initialScopes = Scope.initialScopes

-- | Every binding visible from a scope, innermost scope first.
-- Search state is assembled exclusively through the checked 'Scope' API, so
-- failure here means an internal representation invariant was violated.  Keep
-- that exceptional boundary local instead of threading an impossible error
-- through every nondeterministic search branch.
scopeGetAllBindings :: ScopeId -> Scopes -> [VarPBinding]
scopeGetAllBindings sid = requireValidScopes . Scope.scopeGetAllBindings sid

-- | Apply a substitution to every binding in every scope, re-splitting each
-- result type so newly exposed arrows become parameters; scope identity and
-- ancestry are unchanged.
scopesApplySubsts :: Substs -> Scopes -> Scopes
scopesApplySubsts substs =
  Scope.scopesMapBindings (varPBindingApplySubsts substs)

-- | Prepend a binding to an existing scope, so it is returned before the
-- scope's earlier bindings by 'scopeGetAllBindings'.  A stale or unknown
-- scope ID is an internal invariant violation and raises an error.
scopesAddPBinding :: ScopeId -> VarPBinding -> Scopes -> Scopes
scopesAddPBinding sid binding =
  requireValidScopes . Scope.scopesAddBinding sid binding

requireValidScopes :: Either Scope.ScopeInvariantError a -> a
requireValidScopes = either
  (error . ("Exference internal scope invariant violated: " ++) . show)
  id

-- | Whether an outer forall on a goal is the query's prenex scheme or an
-- opaque rank-N value reached below a type constructor or arrow.
data ForallGoalMode
  = OpenLeadingForalls
  -- | Preserve historical opaque forwarding first, but permit a separate
  -- lower-priority branch which introduces a quantified value. Any direct
  -- contexts become givens local to that goal.
  | TryForallIntroduction
  -- | Continue opening every layer of a nested chain once introduction won.
  | ContinueForallIntroduction
  deriving (Eq, Generic)

-- | Whether a product goal may use the eager whole-tree shortcut. A nested
-- goal emitted by the shortcut's shallow sibling stays on the shallow lane;
-- otherwise each recursive child can rediscover the same saturated tuple
-- through every possible partition of the tree.
data TupleGoalMode
  = AllowTupleTree
  | ContinueShallowTuple
  deriving (Eq, Generic)

-- | An expression hole, its innermost lexical term scope, and the class
-- assumptions visible only while constructing that hole. Root-prenex givens
-- remain in 'nodeQueryClassEnv'; this local list is for contextual rank-N
-- introduction and must travel with every derived goal.
data TGoal = TGoal
  { goalBinding :: VarBinding
  , goalScope :: !ScopeId
  , goalForallMode :: !ForallGoalMode
  , goalTupleMode :: !TupleGoalMode
  , goalGivenConstraints :: [HsConstraint]
  }
  deriving Generic

-- | Apply a substitution to a goal's hole type and its local given
-- constraints; the scope and modes are untouched, and an empty substitution
-- returns the goal unchanged.
goalApplySubst :: Substs -> TGoal -> TGoal
goalApplySubst ss | IntMap.null ss = id
                  | otherwise      = \goal -> goal
                      { goalBinding = varBindingApplySubsts ss
                          $ goalBinding goal
                      , goalGivenConstraints = map
                          (snd . constraintApplySubsts ss)
                          $ goalGivenConstraints goal
                      }

-- | Make one goal per hole binding, all in the given scope and sharing the
-- given local constraints, in the default 'TryForallIntroduction' and
-- 'AllowTupleTree' modes.  The output order follows the input order.
mkGoals :: ScopeId
        -> [HsConstraint]
        -> [VarBinding]
        -> [TGoal]
mkGoals sid givens = map
  (\binding -> TGoal binding sid TryForallIntroduction AllowTupleTree givens)

-- | One state of the proof search: the pending goals, the scoped class
-- obligations, the lexical scopes and per-variable use counts, the
-- environment available to this branch, the partial expression built so far,
-- and the identifier, rigid-scope, and depth bookkeeping needed to extend it.
data SearchNode = SearchNode
  { nodeGoals           :: Seq TGoal
  , nodeConstraintGoals :: [ScopedConstraint]
  , nodeProvidedScopes  :: Scopes
  , nodeVarUses         :: IntMap.IntMap Natural
  , nodeFunctions       :: [FunctionBinding]
  , nodeFunctionSchemes :: Map.Map QualifiedName HsType
    -- ^ Exact specified leading-forall schemes retained by the checked stable
    -- inventory.  Compatibility inputs deliberately leave this empty because
    -- their flattened t'FunctionBinding' values cannot prove binder order.
  , nodeVisibleTypeCandidates :: [HsType]
    -- ^ Closed query subtrees proven to occupy proper-type positions.  These
    -- may explicitly instantiate a context-free foreign scheme whose binder
    -- is otherwise absent from its result.
  , nodeProviderInstantiationCandidates :: Map.Map QualifiedName [HsType]
    -- ^ Checked closed proper-type choices supplied for exact retained global
    -- providers.  The provider key prevents evidence from leaking to a local
    -- value or another global with an alpha-equivalent scheme.
  , nodeProviderInstantiationAssignments ::
      Map.Map QualifiedName [[HsType]]
    -- ^ Complete ordered leading-binder assignments supplied for exact
    -- retained globals. Vectors remain correlated through search instead of
    -- being expanded from the historical candidate pools.
  , nodeAggregateDonations :: Map.Map TVarId (Set.Set QualifiedName)
    -- ^ Scalar globals already exposed structurally at a particular goal
    -- hole. Singleton elimination preserves that hole, so this branch-local
    -- marker prevents rediscovering the same donation forever while allowing
    -- the same polymorphic global at a genuinely new application or result
    -- hole.
  , nodeDeconstructors  :: [DeconstructorBinding]
  , nodeQueryClassEnv   :: QueryClassEnv
  , nodeExpression      :: Expression
  , nodeNextVarId       :: {-# UNPACK #-} !TVarId
    -- Every source namespace allocated in this branch remains reserved.  This
    -- makes freshness independent of whether later substitutions erase all
    -- visible occurrences of an earlier instantiation.
  , nodeFlexibleIds     :: !FlexibleIdSupply
    -- The exact forall-binder/skolem plan is finite and prevalidated. Keeping
    -- the remaining pairs avoids both counter overflow and disagreement with
    -- the independent checker when nested leading foralls shadow an ID.
  , nodeRigidInstantiations :: [(TVarId, TVarId)]
  , nodeRigidPlan :: !RigidInstantiationPlan
  , nodeRigidScope :: !RigidScope
  , nodeDepth           :: {-# UNPACK #-} !Penalty
  , nodeLastStepBinding :: Maybe QualifiedName
  }
  deriving Generic

instance NFData VarBinding
instance NFData VarPBinding
instance NFData ForallGoalMode
instance NFData TupleGoalMode
instance NFData TGoal
instance NFData SearchNode

-- | Split a binding's type at its arrow chain into a scoped t'VarPBinding'
-- with no previously exposed parameters; see 'splitBindingWithParameters'.
splitBinding :: VarBinding -> VarPBinding
splitBinding = splitBindingWithParameters []

-- | Split a scoped value while retaining parameters already exposed by an
-- earlier partial application. Substitution can turn the stored result into
-- another arrow, so every construction path comes through this helper. A
-- quantified result is retained whole for use-site forwarding or elimination.
splitBindingWithParameters :: [HsType] -> VarBinding -> VarPBinding
splitBindingWithParameters previousParameters (VarBinding variable ty) =
  VarPBinding variable result $ previousParameters ++ parameters
 where
  -- Quantifiers below this arrow chain remain in the result or a parameter.
  (result, parameters) = splitArrowChain ty
