-- | Cabal-private de-duplication over Djinn generated clauses.
--
-- The comparison accepts an explicit clause projection so callers retain an
-- entire private candidate/evidence association when its clause is the first
-- representative of an eta-equivalence class.  This avoids reconstructing
-- associations by zipping sidecars onto an already de-duplicated clause list.
module Djinn.Internal.GeneratedDeduplication
    ( deduplicateEtaEquivalentClausesOn
    , etaNormalClauseExpression
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
        (etaNormalClauseExpression $ clauseOf left)
        (etaNormalClauseExpression $ clauseOf right)

    distinctBy _ [] = []
    distinctBy equivalent (firstCandidate : remaining) =
        firstCandidate : distinctBy equivalent
            [ candidate
            | candidate <- remaining
            , not $ equivalent firstCandidate candidate
            ]

-- | The compact comparison payload retained by an incremental enumerator.
-- Keeping this expression does not retain proof evidence or source graphs
-- belonging to a candidate that has already been delivered.
etaNormalClauseExpression
    :: Ord local
    => Generated.FunctionClause local
    -> Generated.Expression local
etaNormalClauseExpression = Generated.simplifyExpressionBy id
    . splitLambdaGroups
    . Generated.functionClauseExpression
  where
    splitLambdaGroups = Generated.rewriteExpressionBottomUp $ \expression ->
        case expression of
            Generated.Lambda patterns body -> foldr
                (\pattern nested -> Generated.Lambda [pattern] nested)
                body patterns
            other -> other
