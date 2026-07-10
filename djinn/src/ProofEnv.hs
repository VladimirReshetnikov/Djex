--
-- Assign stable internal identities to external proof assumptions.
--
module ProofEnv (
    ProofEnvironment, prepareProofEnvironment,
    proofBindings, targetWasExcluded, restoreProofTerm
    ) where

import qualified Data.Set as Set

import LJTFormula

data ProofEnvironment = ProofEnvironment {
    proofBindings :: [(Symbol, Formula)],
    displayBindings :: [(Symbol, Symbol)],
    targetWasExcluded :: Bool
    }

-- A same-named assumption would be printed as a recursive reference to the
-- definition being generated.  Other assumptions receive internal names so
-- proof checking never has to guess between overloaded display names.
prepareProofEnvironment :: Symbol -> [(Symbol, Formula)] -> ProofEnvironment
prepareProofEnvironment target bindings =
    ProofEnvironment internal display excluded
  where
    excluded = any ((== target) . fst) bindings
    safeBindings = filter ((/= target) . fst) bindings
    initiallyUsed = Set.fromList $
        map fst bindings ++ concatMap (formulaSymbols . snd) bindings
    (internal, display, _, _) =
        build safeBindings initiallyUsed (1 :: Integer)

    build [] used next = ([], [], used, next)
    build ((external, formula) : rest) used next =
        let (internalName, next', used') = freshInternal used next
            (env, names, finalUsed, finalNext) =
                build rest used' next'
        in ( (internalName, formula) : env
           , (internalName, external) : names
           , finalUsed
           , finalNext
           )

    freshInternal used next =
        let candidate = Symbol ("$assumption" ++ show next)
        in if candidate `Set.member` used then
               freshInternal used (next + 1)
           else
               (candidate, next + 1, Set.insert candidate used)

formulaSymbols :: Formula -> [Symbol]
formulaSymbols (Conj formulas) = concatMap formulaSymbols formulas
formulaSymbols (Disj alternatives) =
    concatMap (formulaSymbols . snd) alternatives
formulaSymbols (Empty name) = [name]
formulaSymbols (left :-> right) = formulaSymbols left ++ formulaSymbols right
formulaSymbols (PVar symbol) = [symbol]

-- Restore only free assumption variables.  Removing a mapping below a lambda
-- makes this correct even for externally supplied terms that shadow an internal
-- name, although LJT itself reserves every environment symbol.
restoreProofTerm :: ProofEnvironment -> Term -> Term
restoreProofTerm environment = rename (displayBindings environment)
  where
    rename names (Var symbol) = Var $ maybe symbol id $ lookup symbol names
    rename names (Lam binder body) =
        Lam binder $ rename (filter ((/= binder) . fst) names) body
    rename names (Apply function argument) =
        Apply (rename names function) (rename names argument)
    rename names (Xsel index arity expression) =
        Xsel index arity (rename names expression)
    rename _ term = term
