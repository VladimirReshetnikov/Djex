{-# LANGUAGE DeriveGeneric #-}


module Language.Haskell.Exference.Core.Internal.ExferenceNode
  ( SearchNode (..)
  , TGoal (..)
  , Scopes
  , ScopeId
  , VarPBinding (..)
  , VarBinding (..)
  , VarUsageMap
  , varBindingApplySubsts
  , varPBindingApplySubsts
  , goalApplySubst
  , scopesApplySubsts
  , mkGoals
  , addScope
  , scopeGetAllBindings
  , scopesAddPBinding
  , splitBinding
  , initialScopeId
  , initialScopes
  )
where

import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.Core.Expression
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.Core.Score
import qualified Language.Haskell.Exference.Core.Internal.Scope as Scope

import qualified Data.IntMap.Strict as IntMap
import Data.Sequence

import Control.DeepSeq
import GHC.Generics

data VarBinding = VarBinding {-# UNPACK #-} !TVarId HsType
 deriving (Generic)

-- | A variable together with the prenex decomposition of its type.  Naming
-- these components prevents the five adjacent lists and types from being
-- silently transposed at search-state boundaries.
data VarPBinding = VarPBinding
  { varPVariable :: !TVarId
  , varPResult :: HsType
  , varPParameters :: [HsType]
  , varPForallVariables :: [TVarId]
  , varPConstraints :: [HsConstraint]
  }
  deriving (Generic, Show)


instance Show VarBinding where
  show (VarBinding vid ty) = showVar vid ++ " :-> " ++ show ty

varBindingApplySubsts :: Substs -> VarBinding -> VarBinding
varBindingApplySubsts substs (VarBinding v t) =
  VarBinding v (snd $ applySubsts substs t)

varPBindingApplySubsts :: Substs -> VarPBinding -> VarPBinding
varPBindingApplySubsts ss binding =
  let
    v = varPVariable binding
    rt = varPResult binding
    pt = varPParameters binding
    fvs = varPForallVariables binding
    cs = varPConstraints binding
    relevantSS = foldr IntMap.delete ss fvs
    (newResult, params, newForalls, newCs) = splitArrowResultParams
                                           $ snd
                                           $ applySubsts relevantSS rt
  in
  VarPBinding v newResult
    (map (snd . applySubsts relevantSS) pt ++ params)
    (newForalls ++ fvs)
    (cs ++ newCs)

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

{-
scopesAddBinding :: ScopeId -> VarBinding -> Scopes -> Scopes
scopesAddBinding sid binding scopes =
  scopesAddPBinding sid (splitBinding binding) scopes
-}

scopesAddPBinding :: ScopeId -> VarPBinding -> Scopes -> Scopes
scopesAddPBinding sid binding =
  requireValidScopes . Scope.scopesAddBinding sid binding

addScope :: ScopeId -> Scopes -> (ScopeId, Scopes)
addScope parent = requireValidScopes . Scope.addScope parent

requireValidScopes :: Either Scope.ScopeInvariantError a -> a
requireValidScopes = either
  (error . ("Exference internal scope invariant violated: " ++) . show)
  id

type VarUsageMap = IntMap.IntMap Int

-- | An expression hole and the innermost lexical scope visible from it.
data TGoal = TGoal
  { goalBinding :: VarBinding
  , goalScope :: !ScopeId
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
mkGoals sid = map (`TGoal` sid)

data SearchNode = SearchNode
  { nodeGoals           :: Seq TGoal
  , nodeConstraintGoals :: [HsConstraint]
  , nodeProvidedScopes  :: Scopes
  , nodeVarUses         :: VarUsageMap
  , nodeFunctions       :: [FunctionBinding]
  , nodeDeconstructors  :: [DeconstructorBinding]
  , nodeQueryClassEnv   :: QueryClassEnv
  , nodeExpression      :: Expression
  , nodeNextVarId       :: {-# UNPACK #-} !TVarId
  , nodeMaxTVarId       :: {-# UNPACK #-} !TVarId
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
instance NFData TGoal
instance NFData SearchNode

splitBinding :: VarBinding -> VarPBinding
splitBinding (VarBinding v t) =
  let (result, parameters, variables, constraints) = splitArrowResultParams t
  in VarPBinding v result parameters variables constraints
