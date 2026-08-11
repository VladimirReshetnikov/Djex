-- | Cabal-private recognition and erasure of checked instantiation evidence.
--
-- Proof checking consumes synthetic rank-N instantiation axioms.  Generated
-- Haskell must instead use the original polymorphic provider, so this small
-- structural pass runs only after the raw proof and its typed evidence have
-- been sealed together.
module Djinn.Internal.InstantiationEvidence
    ( usesInstantiationEvidence
    , eliminateInstantiationEvidence
    ) where

import qualified Data.Set as Set

import Djinn.Internal.LJTFormula (Symbol (..), Term (..))

-- | Whether a checked proof actually refers to one of the query's erased
-- instantiation axioms. Merely having axioms in the proof environment must not
-- perturb historical simplification for proofs which do not consume them.
usesInstantiationEvidence :: Set.Set Symbol -> Term -> Bool
usesInstantiationEvidence axioms
    | Set.null axioms = const False
    | otherwise = go
  where
    go term = case term of
        Var symbol -> symbol `Set.member` axioms
        Lam _ body -> go body
        Apply function argument -> go function || go argument
        Xsel _ _ expression -> go expression
        _ -> False

-- | Erase caller-selected implicit instantiation evidence from a checked
-- proof before generated conversion. Semantically each selected axiom is the
-- identity function: an applied occurrence reduces to its argument, while a
-- bare occurrence becomes an explicit identity lambda.
--
-- LJT allocates binders away from every environment symbol, so an axiom symbol
-- cannot be shadowed inside a proof term.
eliminateInstantiationEvidence :: Set.Set Symbol -> Term -> Term
eliminateInstantiationEvidence axioms
    | Set.null axioms = id
    | otherwise = go
  where
    go term = case term of
        -- The applied test precedes recursion: rewriting the axiom variable
        -- first would leave a redundant identity redex in the output.
        Apply (Var symbol) argument
            | symbol `Set.member` axioms -> go argument
        Apply function argument -> Apply (go function) $ go argument
        Var symbol
            | symbol `Set.member` axioms ->
                Lam identityBinder $ Var identityBinder
        Lam binder body -> Lam binder $ go body
        Xsel index arity body -> Xsel index arity $ go body
        _ -> term

    -- Renamed by generated-output freshening; the spelling only needs to be
    -- outside the declared-function namespace.
    identityBinder = Symbol "$djinn$instantiated"
