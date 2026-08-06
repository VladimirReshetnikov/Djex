{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Exference's typed compatibility view of the shared generated-term IR.
--
-- Search and the independent checker need a type annotation at every local
-- binder and occurrence. The recursive expression shape itself is no longer
-- backend-owned: annotations travel in the local payload of
-- 'Generated.Expression', while the historical constructors below remain
-- bidirectional patterns over that one shared tree.
module Language.Haskell.Exference.Core.Expression
  ( Expression
      ( ExpVar
      , ExpName
      , ExpLambda
      , ExpApply
      , ExpTypeApply
      , ExpTuple
      , ExpHole
      , ExpLetMatch
      , ExpLet
      , ExpCaseMatch
      )
  , ExpressionRenderError (..)
  , toGeneratedExpression
  , expressionTypedLocals
  , expressionNameHints
  , renderExpression
  , qualificationFromLevel
  , showExpression
  , fillExprHole
  , simplifyExpression
  )
where

import Control.DeepSeq (NFData)
import Data.Foldable (toList)
import qualified Data.List as List
import qualified Data.Map as Map
import GHC.Generics (Generic)

import Language.Haskell.Exference.Core.Types
import qualified Language.Haskell.Synthesis.Generated as Generated

-- | A shared local identity annotated for Exference's search and checker.
-- Holes carry no type because their expected type remains in the goal queue.
data AnnotatedLocal = AnnotatedLocal TVarId (Maybe HsType)
  deriving (Eq, Generic)

instance NFData AnnotatedLocal

-- | The shared generated-expression tree with Exference annotations in its
-- local payload. The constructor stays private so callers can create only the
-- historical, fully annotated subset represented by the bundled patterns.
newtype Expression = Expression (Generated.Expression AnnotatedLocal)
  deriving (Eq, Generic)

instance NFData Expression

pattern ExpVar :: TVarId -> HsType -> Expression
pattern ExpVar variable annotation <-
  Expression (Generated.Local (AnnotatedLocal variable (Just annotation)))
 where
  ExpVar variable annotation = Expression
    $ Generated.Local $ annotated variable annotation

pattern ExpName :: QualifiedName -> Expression
pattern ExpName name = Expression (Generated.Global name)

pattern ExpLambda :: TVarId -> HsType -> Expression -> Expression
pattern ExpLambda variable annotation body <-
  (matchLambda -> Just (variable, annotation, body))
 where
  ExpLambda variable annotation (Expression body) = Expression
    $ Generated.Lambda [Generated.Bind $ annotated variable annotation] body

pattern ExpApply :: Expression -> Expression -> Expression
pattern ExpApply function argument <-
  (matchApply -> Just (function, argument))
 where
  ExpApply (Expression function) (Expression argument) =
    Expression $ Generated.Apply function argument

-- | One bounded visible type application. The shared argument is either @\@_@
-- or a checked lexically closed type. Quantified arguments use alpha-normal
-- scope/slot identities, so they carry no Exference-local type identity into
-- the stable generated tree.
pattern ExpTypeApply
  :: Expression
  -> Generated.VisibleTypeArgument
  -> Expression
pattern ExpTypeApply function argument <-
  (matchTypeApply -> Just (function, argument))
 where
  ExpTypeApply (Expression function) argument = Expression
    $ Generated.VisibleTypeApplication function argument

-- | A structural boxed tuple.  Keeping saturated tuple introduction in the
-- shared generated tree avoids pretending that syntax-level constructors
-- must have been declared as ordinary environment bindings.
pattern ExpTuple :: [Expression] -> Expression
pattern ExpTuple elements <- (matchTuple -> Just elements)
 where
  ExpTuple elements = Expression
    $ Generated.Tuple [element | Expression element <- elements]

pattern ExpHole :: TVarId -> Expression
pattern ExpHole variable =
  Expression (Generated.Hole (AnnotatedLocal variable Nothing))

pattern ExpLetMatch
  :: QualifiedName
  -> [(TVarId, HsType)]
  -> Expression
  -> Expression
  -> Expression
pattern ExpLetMatch constructor variables binding body <-
  (matchLetMatch -> Just (constructor, variables, binding, body))
 where
  ExpLetMatch constructor variables (Expression binding) (Expression body) =
    Expression $ Generated.Let
      (Generated.Constructor constructor $ map typedBinder variables)
      binding
      body

pattern ExpLet
  :: TVarId
  -> HsType
  -> Expression
  -> Expression
  -> Expression
pattern ExpLet variable annotation binding body <-
  (matchLet -> Just (variable, annotation, binding, body))
 where
  ExpLet variable annotation (Expression binding) (Expression body) =
    Expression $ Generated.Let
      (Generated.Bind $ annotated variable annotation)
      binding
      body

pattern ExpCaseMatch
  :: Expression
  -> [(QualifiedName, [(TVarId, HsType)], Expression)]
  -> Expression
pattern ExpCaseMatch scrutinee alternatives <-
  (matchCase -> Just (scrutinee, alternatives))
 where
  ExpCaseMatch (Expression scrutinee) alternatives = Expression
    $ Generated.Case scrutinee $ map generatedAlternative alternatives

{-# COMPLETE ExpVar, ExpName, ExpLambda, ExpApply, ExpTypeApply, ExpTuple,
             ExpHole, ExpLetMatch, ExpLet, ExpCaseMatch #-}

annotated :: TVarId -> HsType -> AnnotatedLocal
annotated variable annotation = AnnotatedLocal variable $ Just annotation

typedBinder :: (TVarId, HsType) -> Generated.Pattern AnnotatedLocal
typedBinder (variable, annotation) =
  Generated.Bind $ annotated variable annotation

matchTypedBinder
  :: Generated.Pattern AnnotatedLocal
  -> Maybe (TVarId, HsType)
matchTypedBinder sourcePattern = case sourcePattern of
  Generated.Bind (AnnotatedLocal variable (Just annotation)) ->
    Just (variable, annotation)
  _ -> Nothing

matchLambda :: Expression -> Maybe (TVarId, HsType, Expression)
matchLambda (Expression expression) = case expression of
  Generated.Lambda
      [Generated.Bind (AnnotatedLocal variable (Just annotation))]
      body -> Just (variable, annotation, Expression body)
  _ -> Nothing

matchApply :: Expression -> Maybe (Expression, Expression)
matchApply (Expression expression) = case expression of
  Generated.Apply function argument ->
    Just (Expression function, Expression argument)
  _ -> Nothing

matchTypeApply
  :: Expression
  -> Maybe (Expression, Generated.VisibleTypeArgument)
matchTypeApply (Expression expression) = case expression of
  Generated.VisibleTypeApplication function argument ->
    Just (Expression function, argument)
  _ -> Nothing

matchTuple :: Expression -> Maybe [Expression]
matchTuple (Expression expression) = case expression of
  Generated.Tuple elements -> Just $ map Expression elements
  _ -> Nothing

matchLetMatch
  :: Expression
  -> Maybe
      ( QualifiedName
      , [(TVarId, HsType)]
      , Expression
      , Expression
      )
matchLetMatch (Expression expression) = case expression of
  Generated.Let (Generated.Constructor constructor patterns) binding body -> do
    variables <- traverse matchTypedBinder patterns
    pure (constructor, variables, Expression binding, Expression body)
  _ -> Nothing

matchLet
  :: Expression
  -> Maybe (TVarId, HsType, Expression, Expression)
matchLet (Expression expression) = case expression of
  Generated.Let
      (Generated.Bind (AnnotatedLocal variable (Just annotation)))
      binding
      body -> Just
        (variable, annotation, Expression binding, Expression body)
  _ -> Nothing

generatedAlternative
  :: (QualifiedName, [(TVarId, HsType)], Expression)
  -> (Generated.Pattern AnnotatedLocal, Generated.Expression AnnotatedLocal)
generatedAlternative (constructor, variables, Expression body) =
  (Generated.Constructor constructor $ map typedBinder variables, body)

matchAlternative
  :: (Generated.Pattern AnnotatedLocal, Generated.Expression AnnotatedLocal)
  -> Maybe (QualifiedName, [(TVarId, HsType)], Expression)
matchAlternative (Generated.Constructor constructor patterns, body) = do
  variables <- traverse matchTypedBinder patterns
  pure (constructor, variables, Expression body)
matchAlternative _ = Nothing

matchCase
  :: Expression
  -> Maybe
      ( Expression
      , [(QualifiedName, [(TVarId, HsType)], Expression)]
      )
matchCase (Expression expression) = case expression of
  Generated.Case scrutinee alternatives -> do
    matched <- traverse matchAlternative alternatives
    pure (Expression scrutinee, matched)
  _ -> Nothing

data ExpressionRenderError
  = ExpressionScopeError (Generated.ScopeError TVarId)
  | ExpressionSyntaxError Generated.RenderError
  deriving (Eq, Show)

-- | Erase search-only type annotations while retaining stable local identity.
-- This is now a functor projection over the canonical shared tree rather than
-- a second recursive syntax conversion.
toGeneratedExpression :: Expression -> Generated.Expression TVarId
toGeneratedExpression (Expression expression) =
  annotatedIdentity <$> expression

annotatedIdentity :: AnnotatedLocal -> TVarId
annotatedIdentity (AnnotatedLocal variable _) = variable

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
-- erased at the stable candidate boundary. These remain hints: the common
-- allocator still resolves collisions with other locals, globals, and
-- caller-reserved names.
expressionNameHints :: Expression -> Map.Map TVarId String
expressionNameHints expression =
  Map.mapWithKey preferredVarName variableTypes
 where
  variableTypes = List.foldl' recordType Map.empty
    $ expressionTypedLocals expression

  recordType types (variable, ty) = Map.alter update variable types
    where
      update Nothing = Just ty
      update (Just TypeVar{}) = Just ty
      update (Just TypeConstant{}) = Just ty
      update existing = existing

-- | Every typed local binder and occurrence in structural order.
--
-- Derived 'Foldable' order for the shared tree is exactly Exference's former
-- observation order: pattern binders precede their bodies, and let/case
-- bindings precede the regions that consume them. Holes have no annotation
-- and are intentionally excluded. Keeping this observation beside the opaque
-- wrapper prevents validators and identifier collectors from reimplementing
-- the shared expression grammar.
expressionTypedLocals :: Expression -> [(TVarId, HsType)]
expressionTypedLocals (Expression expression) =
  [ (variable, annotation)
  | AnnotatedLocal variable (Just annotation) <- toList expression
  ]

fillExprHole :: TVarId -> Expression -> Expression -> Expression
fillExprHole variable (Expression replacement) (Expression expression) =
  Expression $ Generated.fillExpressionHole
    (AnnotatedLocal variable Nothing)
    replacement
    expression

-- | Apply the shared capture-safe generated-term simplifier while comparing
-- annotated locals solely by Exference's stable numeric identity.
simplifyExpression :: Expression -> Expression
simplifyExpression (Expression expression) = Expression
  $ Generated.simplifyExpressionBy annotatedIdentity expression
