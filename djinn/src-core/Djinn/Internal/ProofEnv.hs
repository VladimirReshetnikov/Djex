--
-- Assign stable internal identities to external proof assumptions.
--
module Djinn.Internal.ProofEnv (
    ProofEnvironment, prepareProofEnvironment,
    proofBindings, proofBindingsIncludingTarget,
    targetWasExcluded, restoreProofTerm
    ) where

import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import Djinn.Internal.LJTFormula

data ProofEnvironment = ProofEnvironment {
    -- Bindings that are safe to use in the generated definition.
    proofBindings :: [(Symbol, Formula)],
    -- Every binding, including target-named assumptions.  This environment
    -- exists only to prove whether the more specific self-reference
    -- diagnostic is justified; its proofs must never be rendered.
    proofBindingsIncludingTarget :: [(Symbol, Formula)],
    displayBindings :: [(Symbol, Symbol)],
    targetWasExcluded :: Bool
    }

-- A same-named assumption would be printed as a recursive reference to the
-- definition being generated.  Other assumptions receive internal names so
-- proof checking never has to guess between overloaded display names.
prepareProofEnvironment :: Symbol -> [(Symbol, Formula)] -> ProofEnvironment
prepareProofEnvironment target bindings =
    ProofEnvironment {
        proofBindings = strip safe,
        proofBindingsIncludingTarget = strip internalized,
        displayBindings =
            [(internalName, external)
            | (external, internalName, _) <- safe],
        targetWasExcluded = length safe /= length internalized
        }
  where
    initiallyUsed = Set.fromList $
        map fst bindings ++ concatMap (formulaSymbols . snd) bindings
    internalized = build bindings initiallyUsed (1 :: Integer)
    safe = filter (\ (external, _, _) -> external /= target) internalized
    strip entries =
        [(internalName, formula) | (_, internalName, formula) <- entries]

    build [] _ _ = []
    build ((external, formula) : rest) used next =
        let (internalName, used', next') = freshInternal used next
        in (external, internalName, formula) : build rest used' next'

    freshInternal used next =
        let candidate = Symbol ("$assumption" ++ show next)
        in if candidate `Set.member` used then
               freshInternal used (next + 1)
           else
               (candidate, Set.insert candidate used, next + 1)

-- Restore only free assumption variables.  Removing a mapping below a lambda
-- makes this correct even for externally supplied terms that shadow an internal
-- name, although LJT itself reserves every environment symbol.
restoreProofTerm :: ProofEnvironment -> Term -> Term
restoreProofTerm environment = rename (displayBindings environment)
  where
    rename names (Var symbol) = Var $ fromMaybe symbol $ lookup symbol names
    rename names (Lam binder body) =
        Lam binder $ rename (filter ((/= binder) . fst) names) body
    rename names (Apply function argument) =
        Apply (rename names function) (rename names argument)
    rename names (Xsel index arity expression) =
        Xsel index arity (rename names expression)
    rename _ term = term
