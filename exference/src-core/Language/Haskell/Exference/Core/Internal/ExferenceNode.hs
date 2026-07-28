{-# LANGUAGE DeriveGeneric #-}


module Language.Haskell.Exference.Core.Internal.ExferenceNode
  ( SearchNode (..)
  , ForallGoalMode (..)
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
import qualified Language.Haskell.Exference.Core.Internal.Scope as Scope

import qualified Data.IntMap.Strict as IntMap
import Data.Sequence
import Numeric.Natural (Natural)

import Control.DeepSeq
import GHC.Generics

data VarBinding = VarBinding {-# UNPACK #-} !TVarId HsType
 deriving (Generic)

-- | A scoped variable together with the exposed arrow prefix of its type.
-- Any quantified parameter or result stays intact as an opaque atom; function
-- constraints live in 'nodeConstraintGoals', not on lexical values.
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

type ScopeId = Scope.ScopeId
type Scopes = Scope.Scopes VarPBinding

initialScopeId :: ScopeId
initialScopeId = Scope.initialScopeId

initialScopes :: Scopes
initialScopes = Scope.initialScopes

-- Search state is assembled exclusively through the checked 'Scope' API, so
-- failure here means an internal representation invariant was violated.  Keep
-- that exceptional boundary local instead of threading an impossible error
-- through every nondeterministic search branch.
scopeGetAllBindings :: ScopeId -> Scopes -> [VarPBinding]
scopeGetAllBindings sid = requireValidScopes . Scope.scopeGetAllBindings sid

scopesApplySubsts :: Substs -> Scopes -> Scopes
scopesApplySubsts substs =
  Scope.scopesMapBindings (varPBindingApplySubsts substs)

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
  | KeepForallsOpaque
  deriving (Eq, Generic)

-- | An expression hole and the innermost lexical scope visible from it.
data TGoal = TGoal
  { goalBinding :: VarBinding
  , goalScope :: !ScopeId
  , goalForallMode :: !ForallGoalMode
  }
  deriving Generic

goalApplySubst :: Substs -> TGoal -> TGoal
goalApplySubst ss | IntMap.null ss = id
                  | otherwise      = \goal -> goal
                      { goalBinding = varBindingApplySubsts ss
                          $ goalBinding goal
                      }

mkGoals :: ScopeId
        -> [VarBinding]
        -> [TGoal]
mkGoals sid = map (\binding -> TGoal binding sid KeepForallsOpaque)

data SearchNode = SearchNode
  { nodeGoals           :: Seq TGoal
  , nodeConstraintGoals :: [HsConstraint]
  , nodeProvidedScopes  :: Scopes
  , nodeVarUses         :: IntMap.IntMap Natural
  , nodeFunctions       :: [FunctionBinding]
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
  , nodeDepth           :: {-# UNPACK #-} !Penalty
  , nodeLastStepBinding :: Maybe QualifiedName
  }
  deriving Generic

instance NFData VarBinding
instance NFData VarPBinding
instance NFData ForallGoalMode
instance NFData TGoal
instance NFData SearchNode

splitBinding :: VarBinding -> VarPBinding
splitBinding = splitBindingWithParameters []

-- | Split a scoped value while retaining parameters already exposed by an
-- earlier partial application. Substitution can turn the stored result into
-- another arrow, so every construction path comes through this helper. A
-- quantified result is retained whole and later unified as an opaque atom.
splitBindingWithParameters :: [HsType] -> VarBinding -> VarPBinding
splitBindingWithParameters previousParameters (VarBinding variable ty) =
  VarPBinding variable result $ previousParameters ++ parameters
 where
  -- Quantifiers below this arrow chain remain in the result or a parameter.
  (result, parameters) = splitArrowChain ty
