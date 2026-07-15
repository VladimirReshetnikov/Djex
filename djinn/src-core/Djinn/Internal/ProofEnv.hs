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
import Numeric.Natural (Natural)

import Djinn.Internal.Fresh (allocateFresh)
import Djinn.Internal.LJTFormula

data ProofEnvironment = ProofEnvironment
    [(Symbol, Formula)]
    [(Symbol, Formula)]
    [(Symbol, Symbol)]
    Bool

-- Ordinary projections are intentional. Exported record fields would allow
-- callers to update one derived view while leaving the hidden display-name
-- map and target-exclusion flag unchanged.
proofBindings :: ProofEnvironment -> [(Symbol, Formula)]
proofBindings (ProofEnvironment bindings _ _ _) = bindings

-- | Every binding, including target-named assumptions. This environment
-- exists only to prove whether the more specific self-reference diagnostic
-- is justified; its proofs must never be rendered.
proofBindingsIncludingTarget :: ProofEnvironment -> [(Symbol, Formula)]
proofBindingsIncludingTarget (ProofEnvironment _ bindings _ _) = bindings

targetWasExcluded :: ProofEnvironment -> Bool
targetWasExcluded (ProofEnvironment _ _ _ excluded) = excluded

-- A same-named assumption would be printed as a recursive reference to the
-- definition being generated.  Other assumptions receive internal names so
-- proof checking never has to guess between overloaded display names.
prepareProofEnvironment :: Symbol -> [(Symbol, Formula)] -> ProofEnvironment
prepareProofEnvironment target bindings =
    ProofEnvironment
        (strip safe)
        (strip internalized)
        [ (internalName, external)
        | (external, internalName, _) <- safe
        ]
        (any (\ (external, _, _) -> external == target) internalized)
  where
    initiallyUsed = Set.fromList $
        map fst bindings ++ concatMap (formulaSymbols . snd) bindings
    internalized = build bindings initiallyUsed (1 :: Natural)
    safe = filter (\ (external, _, _) -> external /= target) internalized
    strip entries =
        [(internalName, formula) | (_, internalName, formula) <- entries]

    build [] _ _ = []
    build ((external, formula) : rest) used next =
        let (internalName, used', next') = allocateFresh
                (\suffix ->
                    (Symbol $ "$assumption" ++ show suffix, suffix + 1))
                used next
        in (external, internalName, formula) : build rest used' next'

-- Restore only free assumption variables.  Removing a mapping below a lambda
-- makes this correct even for externally supplied terms that shadow an internal
-- name, although LJT itself reserves every environment symbol.
restoreProofTerm :: ProofEnvironment -> Term -> Term
restoreProofTerm (ProofEnvironment _ _ displayNames _) = rename displayNames
  where
    rename names (Var symbol) = Var $ fromMaybe symbol $ lookup symbol names
    rename names (Lam binder body) =
        Lam binder $ rename (filter ((/= binder) . fst) names) body
    rename names (Apply function argument) =
        Apply (rename names function) (rename names argument)
    rename names (Xsel index arity expression) =
        Xsel index arity (rename names expression)
    rename _ term = term
