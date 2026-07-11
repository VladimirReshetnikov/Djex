module Language.Haskell.Exference.Core.ExpressionSimplify
  ( simplifyExpression
  )
where



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.Expression

import Data.Function ( on )



simplifyExpression :: Expression -> Expression
simplifyExpression = simplifyEta . simplifyLets

-- The rewrites below rely on the search engine's global-variable-ID invariant:
-- every binder receives a distinct ID. They also use Exference's total-term
-- model, under which removing an unused let binding preserves semantics.
-- They deliberately never introduce a global name: an environment-free
-- simplifier cannot assume that Prelude.id or (.) is in scope, unshadowed, or
-- has its standard type.

simplifyLets :: Expression -> Expression
simplifyLets e@ExpVar{}       = e
simplifyLets e@ExpName{}      = e
simplifyLets (ExpLambda i ty e)  = ExpLambda i ty $ simplifyLets e
simplifyLets (ExpApply e1 e2) = ExpApply (simplifyLets e1) (simplifyLets e2)
simplifyLets e@ExpHole{}      = e
simplifyLets (ExpLetMatch name vids bindExp inExp) =
  ExpLetMatch name vids (simplifyLets bindExp) (simplifyLets inExp)
simplifyLets (ExpLet i ty bindExp inExp) = case countUses i inExp of
  0 -> simplifyLets inExp
  1 -> simplifyLets $ replaceVar i bindExp inExp
  _ -> ExpLet i ty (simplifyLets bindExp) (simplifyLets inExp)
simplifyLets (ExpCaseMatch bindExp alts) =
  ExpCaseMatch (simplifyLets bindExp) [ (c, vars, simplifyLets expr)
                                      | (c, vars, expr) <- alts
                                      ]

simplifyEta :: Expression -> Expression
simplifyEta e@ExpVar{}         = e
simplifyEta e@ExpName{}        = e
simplifyEta (ExpLambda i ty e) = simplifyEta' $ ExpLambda i ty $ simplifyEta e
simplifyEta (ExpApply e1 e2)   = ExpApply (simplifyEta e1) (simplifyEta e2)
simplifyEta e@ExpHole{}        = e
simplifyEta (ExpLetMatch name vids bindExp inExp) =
  ExpLetMatch name vids (simplifyEta bindExp) (simplifyEta inExp)
simplifyEta (ExpLet i ty bindExp inExp) =
  ExpLet i ty (simplifyEta bindExp) (simplifyEta inExp)
simplifyEta (ExpCaseMatch bindExp alts) =
  ExpCaseMatch (simplifyEta bindExp) [ (c, vars, simplifyEta expr)
                                     | (c, vars, expr) <- alts
                                     ]

simplifyEta' :: Expression -> Expression
simplifyEta' (ExpLambda i _ (ExpApply e1 (ExpVar j _)))
  | i==j && 0==countUses i e1 = e1
simplifyEta' e = e

replaceVar :: TVarId -> Expression -> Expression -> Expression
replaceVar vid t orig@(ExpVar j _) | vid==j = t
                                   | otherwise = orig
replaceVar vid t (ExpLambda i ty e) = ExpLambda i ty $ replaceVar vid t e
replaceVar vid t (ExpApply e1 e2) = ExpApply (replaceVar vid t e1)
                                             (replaceVar vid t e2)
replaceVar vid t (ExpLetMatch n vars bindExp inExp) =
  ExpLetMatch n vars (replaceVar vid t bindExp) (replaceVar vid t inExp)
replaceVar vid t (ExpLet i ty bindExp inExp) =
  ExpLet i ty (replaceVar vid t bindExp) (replaceVar vid t inExp)
replaceVar vid t (ExpCaseMatch bindExp alts) =
  ExpCaseMatch (replaceVar vid t bindExp) [ (c, vars, replaceVar vid t expr)
                                          | (c, vars, expr) <- alts
                                          ]
replaceVar _ _ t@(ExpName _) = t
replaceVar _ _ t@(ExpHole _) = t


countUses :: TVarId -> Expression -> Int
countUses i (ExpVar j _) | i==j           = 1
                         | otherwise      = 0
countUses _ ExpName{}                     = 0
countUses i (ExpLambda j _ e) | i==j      = 0
                              | otherwise = countUses i e
countUses i (ExpApply e1 e2)              = ((+) `on` countUses i) e1 e2
countUses _ ExpHole{}                     = 0
countUses i (ExpLetMatch _ _ bindExp inExp) = ((+) `on` countUses i) bindExp inExp
countUses i (ExpLet _ _ bindExp inExp)    = ((+) `on` countUses i) bindExp inExp
countUses i (ExpCaseMatch bindExp alts)   = sum $ countUses i bindExp
                                                : [ countUses i expr
                                                  | (_, _, expr) <- alts
                                                  ]
