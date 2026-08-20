-- | Assign stable internal identities to external proof assumptions.
--
-- 'prepareProofEnvironment' re-keys every assumption by a fresh internal
-- 'Symbol' and sets aside any assumption named like the target definition,
-- so proof search and checking never guess between overloaded display
-- names; 'restoreProofTerm' maps a finished proof back to the external
-- names.  "Djinn.Core" wraps every LJT search of a query in this way.
module Djinn.Internal.ProofEnv (
    ProofEnvironment, prepareProofEnvironment,
    proofBindings, proofBindingsIncludingTarget,
    targetWasExcluded, restoreProofTerm
    ) where

import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Numeric.Natural (Natural)

import Djinn.Internal.LJTFormula
import Language.Haskell.Synthesis.Fresh (allocateFresh)

-- | External proof assumptions re-keyed by fresh internal identities, built
-- by 'prepareProofEnvironment'.  It holds the search bindings without any
-- target-named assumption, the bindings including them, the map from
-- internal names back to the external display names, and whether a
-- target-named assumption was excluded.
data ProofEnvironment = ProofEnvironment
    [(Symbol, Formula)]
    [(Symbol, Formula)]
    [(Symbol, Symbol)]
    Bool

-- | The assumptions to search with, keyed by their unique internal names and
-- excluding any assumption named like the target.
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

-- | Whether at least one supplied assumption had the target's name and was
-- therefore left out of 'proofBindings'.
targetWasExcluded :: ProofEnvironment -> Bool
targetWasExcluded (ProofEnvironment _ _ _ excluded) = excluded

-- | Assign each assumption a fresh internal proof name, keeping the given
-- order, and set aside those named like the target definition.
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

-- | Rename the internal assumption names occurring free in a proof term back
-- to their external display names.
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
