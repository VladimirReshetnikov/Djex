-- | Cabal-private recognition and erasure of checked instantiation evidence.
--
-- Proof checking consumes synthetic rank-N instantiation axioms.  Generated
-- Haskell must instead use the original polymorphic provider, so this small
-- structural pass runs only after the raw proof and its typed evidence have
-- been sealed together.
module Djinn.Internal.InstantiationEvidence
    ( usesInstantiationEvidence
    , eliminateInstantiationEvidence
    , independentConstructionScopes
    ) where

import qualified Data.Set as Set
import Data.List (isPrefixOf)

import Djinn.Internal.LJTFormula (Symbol (..), Term (..), symbolSpelling)

-- | One construction axiom owns one fresh forall scope. Separate sibling
-- uses are independent, but a recursive use below its own argument could
-- capture a local variable from the earlier introduction because the finite
-- formula has reused the same skolem names. Such a proof needs another fresh
-- instance and is rejected before evidence erasure. Other axiom families do
-- not introduce a scope and keep their historical unrestricted reuse.
independentConstructionScopes :: Set.Set Symbol -> Term -> Bool
independentConstructionScopes axioms = go Set.empty
  where
    constructors = Set.filter
        (isPrefixOf "$djinn$query-constructed-instantiation$" . symbolSpelling)
        axioms
    go active term = case applicationSpine [] term of
        (Var symbol, arguments) | symbol `Set.member` constructors ->
            symbol `Set.notMember` active &&
                parents symbol `Set.isSubsetOf` active &&
                all (go $ Set.insert symbol active) arguments
        _ -> case term of
            Lam _ body -> go active body
            Apply function argument -> go active function && go active argument
            Xsel _ _ body -> go active body
            _ -> True
    applicationSpine arguments (Apply function argument) =
        applicationSpine (argument : arguments) function
    applicationSpine arguments function = (function, arguments)
    -- Child symbols extend their exact parent's private evidence identity.
    -- Every ancestor must enclose a use, so a specialization mentioning an
    -- outer construction skolem cannot be emitted outside that introduction.
    parents symbol = Set.filter
        (\parent -> (symbolSpelling parent ++ "$nested$")
            `isPrefixOf` symbolSpelling symbol) constructors

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
