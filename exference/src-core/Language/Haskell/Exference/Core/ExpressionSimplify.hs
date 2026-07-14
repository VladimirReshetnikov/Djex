module Language.Haskell.Exference.Core.ExpressionSimplify
  ( simplifyExpression
  )
where



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.Expression

import qualified Data.IntSet as IntSet



simplifyExpression :: Expression -> Expression
simplifyExpression = simplifyEta . simplifyLets

-- Search expressions give every binder a globally unique ID, which keeps the
-- common path cheap.  The public 'Expression' constructors do not impose that
-- invariant, however, so the traversals below still respect lexical shadowing
-- and conservatively retain a let when inlining its right-hand side would
-- capture one of that expression's free variables.  Removing an unused let
-- relies on Exference's total-term model.  No rewrite introduces a global name:
-- an environment-free simplifier cannot assume that Prelude.id or (.) is in
-- scope, unshadowed, or has its standard type.

simplifyLets :: Expression -> Expression
simplifyLets e@ExpVar{}       = e
simplifyLets e@ExpName{}      = e
simplifyLets (ExpLambda i ty e)  = ExpLambda i ty $ simplifyLets e
simplifyLets (ExpApply e1 e2) = ExpApply (simplifyLets e1) (simplifyLets e2)
simplifyLets e@ExpHole{}      = e
simplifyLets (ExpLetMatch name vids bindExp inExp) =
  ExpLetMatch name vids (simplifyLets bindExp) (simplifyLets inExp)
simplifyLets (ExpLet i ty bindExp inExp) = case occurrencesOf i inExp of
  Unused -> simplifyLets inExp
  UsedOnce -> case replaceVar i bindExp inExp of
    Just replaced -> simplifyLets replaced
    Nothing -> retainLet
  UsedMany -> ExpLet i ty (simplifyLets bindExp) (simplifyLets inExp)
 where
  retainLet = ExpLet i ty (simplifyLets bindExp) (simplifyLets inExp)
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
  | i == j && occurrencesOf i e1 == Unused = e1
simplifyEta' e = e

-- Return 'Nothing' rather than alpha-renaming when a substitution would
-- capture a free variable.  That makes the public simplifier safe for
-- caller-constructed expressions while leaving the globally-unique search
-- representation on the straightforward no-renaming path.
replaceVar :: TVarId -> Expression -> Expression -> Maybe Expression
replaceVar replaced replacement = go
 where
  replacementFree = freeVariables replacement

  go original@(ExpVar variable _)
    | replaced == variable = Just replacement
    | otherwise = Just original
  go original@ExpName{} = Just original
  go original@(ExpLambda variable ty body)
    | replaced == variable = Just original
    | binderCaptures [variable] body = Nothing
    | otherwise = ExpLambda variable ty <$> go body
  go (ExpApply function argument) = ExpApply <$> go function <*> go argument
  go original@ExpHole{} = Just original
  go (ExpLetMatch constructor variables binding body) = do
    binding' <- go binding
    body' <- underBinders (map fst variables) body
    pure $ ExpLetMatch constructor variables binding' body'
  go (ExpLet variable ty binding body) = do
    binding' <- go binding
    body' <- underBinders [variable] body
    pure $ ExpLet variable ty binding' body'
  go (ExpCaseMatch scrutinee alternatives) = ExpCaseMatch
    <$> go scrutinee
    <*> traverse replaceAlternative alternatives

  replaceAlternative (constructor, variables, body) = do
    body' <- underBinders (map fst variables) body
    pure (constructor, variables, body')

  underBinders binders body
    | replaced `elem` binders = Just body
    | binderCaptures binders body = Nothing
    | otherwise = go body

  -- The usage guard avoids rejecting a harmless binder collision in a region
  -- where no replacement will actually be inserted.
  binderCaptures binders body =
    any (`IntSet.member` replacementFree) binders
      && occurrencesOf replaced body /= Unused


freeVariables :: Expression -> IntSet.IntSet
freeVariables expression = case expression of
  ExpVar variable _ -> IntSet.singleton variable
  ExpName{} -> IntSet.empty
  ExpLambda variable _ body -> IntSet.delete variable $ freeVariables body
  ExpApply function argument ->
    IntSet.union (freeVariables function) (freeVariables argument)
  ExpHole{} -> IntSet.empty
  ExpLetMatch _ variables binding body -> IntSet.union
    (freeVariables binding)
    (deleteBinders variables $ freeVariables body)
  ExpLet variable _ binding body -> IntSet.union
    (freeVariables binding)
    (IntSet.delete variable $ freeVariables body)
  ExpCaseMatch scrutinee alternatives -> IntSet.unions
    $ freeVariables scrutinee
    : [ deleteBinders variables $ freeVariables body
      | (_, variables, body) <- alternatives
      ]
 where
  deleteBinders variables free =
    foldr (IntSet.delete . fst) free variables


-- Only the distinctions below affect simplification: an unused binding can be
-- removed, a single use may be inlined, and every larger count must retain the
-- binding. Saturating at 'UsedMany' avoids making exact occurrence totals an
-- overflowable part of the optimizer's correctness boundary.
data Occurrences = Unused | UsedOnce | UsedMany
  deriving (Eq)

combineOccurrences :: Occurrences -> Occurrences -> Occurrences
combineOccurrences Unused right = right
combineOccurrences left Unused = left
combineOccurrences _ _ = UsedMany

occurrencesOf :: TVarId -> Expression -> Occurrences
occurrencesOf i (ExpVar j _)
  | i == j = UsedOnce
  | otherwise = Unused
occurrencesOf _ ExpName{} = Unused
occurrencesOf i (ExpLambda j _ e)
  | i == j = Unused
  | otherwise = occurrencesOf i e
occurrencesOf i (ExpApply e1 e2) =
  combineOccurrences (occurrencesOf i e1) (occurrencesOf i e2)
occurrencesOf _ ExpHole{} = Unused
occurrencesOf i (ExpLetMatch _ variables bindExp inExp) =
  combineOccurrences
    (occurrencesOf i bindExp)
    (occurrencesUnder (map fst variables) inExp)
 where
  occurrencesUnder binders body
    | i `elem` binders = Unused
    | otherwise = occurrencesOf i body
occurrencesOf i (ExpLet j _ bindExp inExp) = combineOccurrences
  (occurrencesOf i bindExp)
  (if i == j then Unused else occurrencesOf i inExp)
occurrencesOf i (ExpCaseMatch bindExp alts) = foldr
  (combineOccurrences . occurrenceInAlternative)
  (occurrencesOf i bindExp)
  alts
 where
  occurrenceInAlternative (_, variables, body)
    | i `elem` map fst variables = Unused
    | otherwise = occurrencesOf i body
