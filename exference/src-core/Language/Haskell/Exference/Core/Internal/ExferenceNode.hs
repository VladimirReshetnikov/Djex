{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


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
  -- SearchNode lenses
  , HasGoals (..)
  , HasConstraintGoals (..)
  , HasProvidedScopes (..)
  , HasVarUses (..)
  , HasFunctions (..)
  , HasDeconss (..)
  , HasQueryClassEnv (..)
  , HasExpression (..)
  , HasNextVarId (..)
  , HasMaxTVarId (..)
  , HasNextNVarId (..)
  , HasDepth (..)
  , HasLastStepReason (..)
  , HasLastStepBinding (..)
  )
where

import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.Core.Expression
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.Core.Score
import qualified Language.Haskell.Exference.Core.Internal.Scope as Scope

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector as V
import Data.Sequence

import Control.DeepSeq
import GHC.Generics
import Control.Lens.TH ( makeFields )

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
  { _searchNodeGoals           :: Seq TGoal
  , _searchNodeConstraintGoals :: [HsConstraint]
  , _searchNodeProvidedScopes  :: Scopes
  , _searchNodeVarUses         :: VarUsageMap
  , _searchNodeFunctions       :: V.Vector FunctionBinding
  , _searchNodeDeconss         :: [DeconstructorBinding]
  , _searchNodeQueryClassEnv   :: QueryClassEnv
  , _searchNodeExpression      :: Expression
  , _searchNodeNextVarId       :: {-# UNPACK #-} !TVarId
  , _searchNodeMaxTVarId       :: {-# UNPACK #-} !TVarId
  , _searchNodeNextNVarId      :: {-# UNPACK #-} !TVarId -- id used when resolving rankN-types
  , _searchNodeDepth           :: {-# UNPACK #-} !Penalty
  , _searchNodeLastStepReason  :: String
  , _searchNodeLastStepBinding :: Maybe String
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

makeFields ''SearchNode
