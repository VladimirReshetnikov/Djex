{-# LANGUAGE BangPatterns #-}
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
  , expressionQualityCost
  , enableExpressionQualityCache
  , expressionTypedLocals
  , expressionNameHints
  , renderExpression
  , qualificationFromLevel
  , showExpression
  , fillExprHole
  , simplifyExpression
  , reduceKnownConstructorCases
  , inlineVisibleTypeApplicationAliases
  )
where

import Control.DeepSeq (NFData (rnf))
import Control.Monad.Trans.Writer.Strict (execWriter, tell)
import Data.Foldable (toList)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Map.Strict as StrictMap
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Exference.Core.Types
import qualified Language.Haskell.Synthesis.CandidateQuality as Quality
import qualified Language.Haskell.Synthesis.Generated as Generated

-- | A shared local identity annotated for Exference's search and checker.
-- Holes carry no type because their expected type remains in the goal queue.
data AnnotatedLocal = AnnotatedLocal TVarId (Maybe HsType)
  deriving (Eq, Generic)

instance NFData AnnotatedLocal

-- | The shared generated-expression tree with Exference annotations in its
-- local payload. The constructor stays private so callers can create only the
-- historical, fully annotated subset represented by the bundled patterns.
data Expression = MeasuredExpression
  (Generated.Expression AnnotatedLocal)
  !(Maybe ExpressionQualitySummary)
  deriving Generic

-- The optional summary is enabled only by structural search. Legacy trees
-- retain no summary thunks and therefore cannot retain historical trees
-- through unevaluated cache updates. Equality and deep evaluation retain
-- their historical tree-only semantics, independent of cache availability.
instance Eq Expression where
  MeasuredExpression left _ == MeasuredExpression right _ = left == right

instance NFData Expression where
  rnf (MeasuredExpression expression _) = rnf expression

-- All general shared rewrites rebuild through this private pattern and drop
-- the optional cache rather than accidentally inherit stale measurements.
-- The search's hole-fill operation below updates an active cache exactly.
pattern Expression :: Generated.Expression AnnotatedLocal -> Expression
pattern Expression expression <- MeasuredExpression expression _
 where
  Expression expression = MeasuredExpression expression Nothing

{-# COMPLETE Expression #-}

data ExpressionQualitySummary = ExpressionQualitySummary
  { summarySize :: !Natural
  , summaryEliminations :: !Natural
  , summaryProviders :: !(StrictMap.Map QualifiedName Natural)
  , summaryHoles :: !(IntMap.IntMap Natural)
  }

summarizeExpression :: Generated.Expression AnnotatedLocal -> ExpressionQualitySummary
summarizeExpression expression = ExpressionQualitySummary
  (Quality.qualityTermSize quality)
  (Quality.qualityEliminations quality)
  providers holes
 where
  -- Reuse the shared metric as the sole definition of structural size and
  -- elimination costs. Only occurrence counts need a separate small fold.
  quality = Quality.candidateQuality (const 0) expression
  (providers, holes) = occurrences expression (StrictMap.empty, IntMap.empty)
  occurrences node counts = case node of
    Generated.Local _ -> counts
    Generated.Global name ->
      (StrictMap.insertWith (+) name 1 $ fst counts, snd counts)
    Generated.Hole (AnnotatedLocal variable Nothing) ->
      (fst counts, IntMap.insertWith (+) variable 1 $ snd counts)
    Generated.Hole _ -> counts
    Generated.Lambda _ body -> occurrences body counts
    Generated.Apply function argument -> occurrences argument $ occurrences function counts
    Generated.VisibleTypeApplication function _ -> occurrences function counts
    Generated.Tuple elements -> List.foldl' (flip occurrences) counts elements
    Generated.Let _ binding body -> occurrences body $ occurrences binding counts
    Generated.Case scrutinee alternatives -> List.foldl'
      (\current (_, body) -> occurrences body current)
      (occurrences scrutinee counts) alternatives

fillQualitySummary
  :: TVarId
  -> ExpressionQualitySummary
  -> ExpressionQualitySummary
  -> ExpressionQualitySummary
fillQualitySummary variable replacement original
  | count == 0 = original
  | otherwise = ExpressionQualitySummary
      (summarySize original - count + count * summarySize replacement)
      (summaryEliminations original + count * summaryEliminations replacement)
      (StrictMap.unionWith (+) (summaryProviders original)
        $ StrictMap.map (* count) $ summaryProviders replacement)
      (IntMap.unionWith (+) (IntMap.delete variable $ summaryHoles original)
        $ IntMap.map (* count) $ summaryHoles replacement)
 where
  count = IntMap.findWithDefault 0 variable $ summaryHoles original

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

-- | Why 'renderExpression' refused an expression: a local variable that is
-- unbound or bound twice at one pattern, or a shared syntax/rendering error.
-- The scope check runs first.
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

-- | Score the exact immutable summary retained with the annotated tree.
-- Hole filling updates it from only the inserted fragment and its occurrence
-- count, so frontier scoring no longer walks the growing partial expression.
-- Provider costs remain query-specific and use exact names; no policy or
-- caller's overrides are cached in an expression. Types and evidence are
-- untouched. Uncached expressions use the direct shared fold, and legacy
-- ranking does not inspect the expression.
expressionQualityCost
  :: Quality.CandidateRankingPolicy
  -> (QualifiedName -> Natural)
  -> Expression
  -> Natural
expressionQualityCost Quality.LegacyCandidateRanking _ _ = 0
expressionQualityCost ranking@(Quality.StructuralCandidateRanking weights) providerCost
    (MeasuredExpression expression cache) = case cache of
  Nothing -> Quality.candidateQualityCost ranking providerCost expression
  Just summary ->
    Quality.candidateSizeWeight weights * summarySize summary
      + Quality.candidateEliminationWeight weights * summaryEliminations summary
      + Quality.candidateProviderWeight weights * StrictMap.foldlWithKey'
          (\cost name count -> cost + count * providerCost name) 0
          (summaryProviders summary)

-- | Enable exact incremental quality measurements for a search root. An
-- existing cache is reused. Ordinary constructors and shared rewrites remain
-- uncached, so legacy search allocates no unused cache or delayed summary.
enableExpressionQualityCache :: Expression -> Expression
enableExpressionQualityCache original@(MeasuredExpression expression cache) = case cache of
  Just _ -> original
  Nothing -> let !summary = summarizeExpression expression
             in MeasuredExpression expression $ Just summary

annotatedIdentity :: AnnotatedLocal -> TVarId
annotatedIdentity (AnnotatedLocal variable _) = variable

-- | Render a closed expression as Haskell source under the given name
-- qualification policy.  Local-variable scope is validated first, then the
-- shared renderer allocates local names using this expression's
-- type-derived hints ('expressionNameHints'); use 'showExpression' for
-- unchecked diagnostic output of partial search trees.
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

-- | @fillExprHole hole replacement expression@ replaces every 'ExpHole' with
-- identity @hole@ in @expression@ by @replacement@.  The replacement is
-- inserted as a whole and not itself searched, so fresh holes it introduces
-- are never mistaken for the one just filled.
fillExprHole :: TVarId -> Expression -> Expression -> Expression
fillExprHole variable (MeasuredExpression replacement replacementCache)
    (MeasuredExpression expression originalCache) = case originalCache of
  Nothing -> MeasuredExpression filled Nothing
  Just originalSummary ->
    let replacementSummary = case replacementCache of
          Nothing -> summarizeExpression replacement
          Just summary -> summary
        !updatedSummary = fillQualitySummary variable replacementSummary originalSummary
    in MeasuredExpression filled $ Just updatedSummary
 where
  filled = Generated.fillExpressionHole
    (AnnotatedLocal variable Nothing) replacement expression

-- | Apply the shared capture-safe generated-term simplifier while comparing
-- annotated locals solely by Exference's stable numeric identity.
simplifyExpression :: Expression -> Expression
simplifyExpression (Expression expression) = Expression
  $ Generated.simplifyExpressionBy annotatedIdentity expression

-- | Reduce constructor matches using checked, non-strict constructor arities.
-- Keep local annotations intact and retain sharing through the shared let
-- simplifier. Callers independently check the returned expression before use.
reduceKnownConstructorCases :: (QualifiedName -> Maybe Int) -> Expression -> Expression
reduceKnownConstructorCases constructorArity (Expression expression) = Expression
  $ Generated.simplifyExpressionWithoutEtaBy annotatedIdentity
  $ Generated.reduceKnownConstructorCasesBy annotatedIdentity constructorArity
      expression

-- | Preserve visible instantiation on its original expression spine. GHC
-- does not expose inferred let binders as specified type-application slots.
-- Inline only nonrecursive aliases selected by an explicit type use, keeping
-- other lets as useful bidirectional checking boundaries. The shared
-- substitution rejects capture and respects lexical shadowing. Inlining all
-- uses of a selected alias preserves its pure expression meaning; it may
-- change sharing, but cannot introduce a value or type assumption.
inlineVisibleTypeApplicationAliases :: Expression -> Maybe Expression
inlineVisibleTypeApplicationAliases (Expression expression) = Expression <$>
  Generated.rewriteExpressionBottomUpM inlineAlias expression
 where
  inlineAlias original@(Generated.Let (Generated.Bind local) binding body)
    | annotatedIdentity local `elem` visibleHeads body =
        Generated.substituteExpressionLocalBy
          annotatedIdentity (annotatedIdentity local) binding body
    | otherwise = Just original
  inlineAlias original = Just original

  -- A shadowed same-identity use can only cause an unnecessary selection:
  -- the substitution itself is the lexical authority and leaves it alone.
  visibleHeads source = execWriter $
    Generated.rewriteExpressionBottomUpM observe source
  observe node = do
    case node of
      Generated.VisibleTypeApplication (Generated.Local local) _ ->
        tell [annotatedIdentity local]
      _ -> pure ()
    pure node
