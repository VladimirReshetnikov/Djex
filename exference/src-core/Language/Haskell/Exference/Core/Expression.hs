{-# LANGUAGE DeriveGeneric #-}

module Language.Haskell.Exference.Core.Expression
  ( Expression (..)
  , ExpressionRenderError (..)
  , toGeneratedExpression
  , expressionNameHints
  , renderExpression
  , qualificationFromLevel
  , showExpression
  , fillExprHole
  , allocateExpressionNames
  )
where



import Language.Haskell.Exference.Core.Types
import qualified Data.List as L

import Control.DeepSeq
import GHC.Generics

import qualified Data.Map as M
import qualified Language.Haskell.Synthesis.Generated as Generated



data Expression = ExpVar TVarId HsType -- a
                   -- (type is just for choosing better id when printing)
                | ExpName QualifiedName -- Prelude.zip
                | ExpLambda TVarId HsType Expression -- \x -> exp
                | ExpApply Expression Expression -- f x
                | ExpHole TVarId                 -- h
                | ExpLetMatch QualifiedName [(TVarId, HsType)] Expression Expression
                            -- let (Foo a b c) = bExp in inExp
                | ExpLet TVarId HsType Expression Expression
                            -- let x = bExp in inExp
                | ExpCaseMatch
                    Expression
                    [(QualifiedName, [(TVarId, HsType)], Expression)]
                     -- case mExp of Foo a b -> e1; Bar c d -> e2
  deriving (Eq, Generic)

instance NFData Expression

data ExpressionRenderError
  = ExpressionScopeError (Generated.ScopeError TVarId)
  | ExpressionSyntaxError Generated.RenderError
  deriving (Eq, Show)

-- | Erase search-only type annotations while retaining stable local identity.
-- The result is independent of haskell-src-exts and is shared with Djinn's
-- checked output boundary.
toGeneratedExpression :: Expression -> Generated.Expression TVarId
toGeneratedExpression expression = case expression of
  ExpVar variable _ -> Generated.Local variable
  ExpName name -> Generated.Global name
  ExpLambda variable _ body ->
    Generated.Lambda [Generated.Bind variable]
      $ toGeneratedExpression body
  ExpApply function argument -> Generated.Apply
    (toGeneratedExpression function)
    (toGeneratedExpression argument)
  ExpHole variable -> Generated.Hole variable
  ExpLetMatch constructor variables binding body -> Generated.Let
    (Generated.Constructor constructor
      $ map (Generated.Bind . fst) variables)
    (toGeneratedExpression binding)
    (toGeneratedExpression body)
  ExpLet variable _ binding body -> Generated.Let
    (Generated.Bind variable)
    (toGeneratedExpression binding)
    (toGeneratedExpression body)
  ExpCaseMatch scrutinee alternatives -> Generated.Case
    (toGeneratedExpression scrutinee)
    [ ( Generated.Constructor constructor
          $ map (Generated.Bind . fst) variables
      , toGeneratedExpression body
      )
    | (constructor, variables, body) <- alternatives
    ]

-- | Allocate names through the common renderer so the HSE compatibility
-- adapter and the parser-independent text renderer cannot drift apart.
allocateExpressionNames
  :: Generated.Qualification
  -> [String]
  -> Expression
  -> Either Generated.RenderError (M.Map TVarId String)
allocateExpressionNames qualification reserved expression =
  Generated.allocateLocalNames options $ toGeneratedExpression expression
 where
  options = renderOptions qualification reserved expression

renderExpression
  :: Generated.Qualification
  -> Expression
  -> Either ExpressionRenderError String
renderExpression qualification expression = do
  let generated = toGeneratedExpression expression
  either (Left . ExpressionScopeError) Right
    $ Generated.validateExpressionScope generated
  either (Left . ExpressionSyntaxError) Right
    $ Generated.renderExpression
        (renderOptions qualification [] expression) generated

-- | Interpret Exference's historical numeric CLI policy once for every
-- output frontend.
qualificationFromLevel :: Int -> Generated.Qualification
qualificationFromLevel qualification
  | qualification <= 0 = Generated.Unqualified
  | qualification == 1 = Generated.QualifyIdentifiers
  | otherwise = Generated.FullyQualified

-- | Render partial search trees for diagnostics. Search nodes legitimately
-- contain locals whose binders live in the separate scope forest, so this
-- compatibility view deliberately skips the closed-expression scope check.
-- Checked result boundaries must use 'renderExpression'.
showExpression :: Expression -> String
showExpression expression = case Generated.renderExpression
    (renderOptions Generated.Unqualified [] expression)
    (toGeneratedExpression expression) of
  Right rendered -> rendered
  Left renderError -> "<invalid generated expression: "
    ++ show renderError ++ ">"

renderOptions
  :: Generated.Qualification
  -> [String]
  -> Expression
  -> Generated.RenderOptions TVarId
renderOptions qualification reserved expression =
  Generated.renderOptionsWithLocalNameHints
    qualification (expressionNameHints expression) showVar reserved

-- | Preserve the type-derived spelling preferences that would otherwise be
-- erased at the shared generated-expression boundary.  These remain hints:
-- the common allocator still resolves collisions with other locals, globals,
-- and caller-reserved names.
expressionNameHints :: Expression -> M.Map TVarId String
expressionNameHints expression = M.mapWithKey preferredVarName variableTypes
 where
  variableTypes = L.foldl' recordType M.empty $ variableObservations expression

  recordType types (variable, ty) = M.alter update variable types
    where
      update Nothing = Just ty
      update (Just TypeVar{}) = Just ty
      update (Just TypeConstant{}) = Just ty
      update existing = existing

variableObservations :: Expression -> [(TVarId, HsType)]
variableObservations (ExpVar variable ty) = [(variable, ty)]
variableObservations ExpName{} = []
variableObservations (ExpLambda variable ty body) =
  (variable, ty) : variableObservations body
variableObservations (ExpApply function argument) =
  variableObservations function ++ variableObservations argument
variableObservations ExpHole{} = []
variableObservations (ExpLetMatch _ variables binding body) =
  variables ++ variableObservations binding ++ variableObservations body
variableObservations (ExpLet variable ty binding body) =
  (variable, ty) : variableObservations binding ++ variableObservations body
variableObservations (ExpCaseMatch scrutinee alternatives) =
  variableObservations scrutinee
  ++ concat
    [ variables ++ variableObservations body
    | (_, variables, body) <- alternatives
    ]

fillExprHole :: TVarId -> Expression -> Expression -> Expression
fillExprHole vid t orig@(ExpHole j) | vid==j = t
                                    | otherwise = orig
fillExprHole vid t (ExpLambda i ty e) = ExpLambda i ty $ fillExprHole vid t e
fillExprHole vid t (ExpApply e1 e2) = ExpApply (fillExprHole vid t e1)
                                               (fillExprHole vid t e2)
fillExprHole vid t (ExpLetMatch n vars bindExp inExp) =
  ExpLetMatch n vars (fillExprHole vid t bindExp) (fillExprHole vid t inExp)
fillExprHole vid t (ExpLet i ty bindExp inExp) =
  ExpLet i ty (fillExprHole vid t bindExp) (fillExprHole vid t inExp)
fillExprHole vid t (ExpCaseMatch bindExp alts) =
  ExpCaseMatch (fillExprHole vid t bindExp) [ (c, vars, fillExprHole vid t expr)
                                            | (c, vars, expr) <- alts
                                            ]
fillExprHole _ _ t@ExpName{} = t
fillExprHole _ _ t@ExpVar{}  = t
