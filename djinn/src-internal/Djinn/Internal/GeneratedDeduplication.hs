-- | Cabal-private de-duplication over Djinn generated clauses.
--
-- The comparison accepts an explicit clause projection so callers retain an
-- entire private candidate/evidence association when its clause is the first
-- representative of an eta-equivalence class.  This avoids reconstructing
-- associations by zipping sidecars onto an already de-duplicated clause list.
module Djinn.Internal.GeneratedDeduplication
    ( deduplicateEtaEquivalentClausesOn
    ) where

import qualified Language.Haskell.Synthesis.Generated as Generated

-- | Remove alpha-equivalent clauses after comparing their fully eta-normal
-- denotations, retaining the first complete input value unchanged.
--
-- Function clauses and lambdas store multiple binders in one pattern group,
-- whereas the generic eta reducer contracts a unary lambda.  Split every
-- group into a nested unary suffix in the private comparison key so @f@ and
-- @\x y -> f x y@ compare alike without contracting the surviving value.
deduplicateEtaEquivalentClausesOn
    :: Ord local
    => (candidate -> Generated.FunctionClause local)
    -> [candidate]
    -> [candidate]
deduplicateEtaEquivalentClausesOn clauseOf = distinctBy etaAlphaEquivalent
  where
    etaAlphaEquivalent left right = Generated.alphaEquivalentExpression
        (etaNormalExpression $ clauseOf left)
        (etaNormalExpression $ clauseOf right)

    etaNormalExpression = Generated.simplifyExpressionBy id
        . splitLambdaGroups
        . Generated.functionClauseExpression

    splitLambdaGroups = Generated.rewriteExpressionBottomUp $ \expression ->
        case expression of
            Generated.Lambda patterns body -> foldr
                (\pattern nested -> Generated.Lambda [pattern] nested)
                body patterns
            other -> other

    distinctBy _ [] = []
    distinctBy equivalent (firstCandidate : remaining) =
        firstCandidate : distinctBy equivalent
            [ candidate
            | candidate <- remaining
            , not $ equivalent firstCandidate candidate
            ]
