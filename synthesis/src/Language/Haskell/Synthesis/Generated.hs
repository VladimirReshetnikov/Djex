{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Backend-independent generated Haskell syntax and rendering.
--
-- Search engines retain their own typed or proof-oriented terms.  At the
-- checked output boundary they can erase those private annotations into this
-- small tree, which distinguishes local identities from validated global
-- 'Name's and gives both backends one scope and printing policy.
module Language.Haskell.Synthesis.Generated
  ( DefinitionName
  , mkDefinitionName
  , definitionName
  , definitionSpelling
  , Pattern (..)
  , ClosedVisibleTypeVariable (..)
  , closedVisibleTypeVariableSpelling
  , VisibleTypeArgument
  , VisibleTypeArgumentError (..)
  , inferredVisibleTypeArgument
  , specifiedVisibleTypeArgument
  , isInferredVisibleTypeArgument
  , visibleTypeArgumentType
  , visibleTypeArgumentClosedType
  , Expression (..)
  , ApplicationArgument (..)
  , FunctionClause (..)
  , Qualification (..)
  , RenderOptions (..)
  , defaultRenderOptions
  , renderOptionsWithLocalNameHints
  , RenderError (..)
  , ScopeError (..)
  , validateExpressionScope
  , validateFunctionClauseScope
  , validateExpressionSyntax
  , validateFunctionClauseSyntax
  , validateDefinitionName
  , lambdaExpression
  , expressionLambdaSpine
  , functionClauseFromExpression
  , functionClauseExpression
  , applyExpressionArguments
  , expressionApplicationSpine
  , expressionFullApplicationSpine
  , rewriteExpressionBottomUp
  , rewriteExpressionBottomUpM
  , patternBindingSites
  , expressionBindingSites
  , functionClauseBindingSites
  , expressionFreeLocalIdentitiesBy
  , expressionGlobals
  , fillExpressionHole
  , substituteExpressionLocalBy
  , alphaEquivalentExpression
  , projectFieldSelectors
  , projectFieldSelectorsWithoutEta
  , simplifyCaseExpression
  , simplifyExpressionCases
  , normalizeExpressionPatterns
  , discardUnusedPatternBindingsBy
  , simplifyExpressionBy
  , simplifyExpressionWithoutEtaBy
  , expressionHoles
  , expressionSizeNatural
  , expressionSize
  , allocateLocalNames
  , allocateClauseLocalNames
  , renderExpression
  , renderFunctionClause
  ) where

import Prelude hiding ((<>))

import Control.DeepSeq (NFData (rnf))
import qualified Data.Bifunctor as Bifunctor
import Data.Foldable (toList)
import Data.Functor.Identity (Identity (..))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Void (Void)
import GHC.Generics (Generic)
import Language.Haskell.Synthesis.Collection (observedListLength)
import Language.Haskell.Synthesis.Count (saturatingNaturalToInt)
import qualified Language.Haskell.Synthesis.Fresh as Fresh
import qualified Language.Haskell.Synthesis.Internal.Alpha as Alpha
import Language.Haskell.Synthesis.Name
import Language.Haskell.Synthesis.Qualification
  ( Qualification (..)
  , emittedIdentifier
  , renderNameInfix
  , renderNamePrefix
  )
import qualified Language.Haskell.Synthesis.Type as SharedType
import qualified Language.Haskell.Synthesis.TypeRender as SharedTypeRender
import Numeric.Natural (Natural)
import Text.PrettyPrint.HughesPJ
  ( Doc
  , ($$)
  , (<+>)
  , (<>)
  , comma
  , fsep
  , nest
  , parens
  , punctuate
  , renderStyle
  , sep
  , style
  , text
  , vcat
  )

-- | A checked name for a generated top-level value definition.
--
-- The constructor is deliberately hidden: every value is unqualified,
-- variable-like, and distinct from the wildcard spelling @_@.  Keeping this
-- invariant in the type lets query adapters consume target spelling without
-- repeating a fallible backend-specific preflight.
data DefinitionName = DefinitionName !Name !String
  deriving (Eq, Ord)

-- Preserve the historical presentation of request records.  In particular,
-- derived 'Show' instances containing a t'DefinitionName' render the wrapped
-- structural name directly rather than exposing this implementation wrapper.
instance Show DefinitionName where
  showsPrec precedence (DefinitionName name _) = showsPrec precedence name

instance NFData DefinitionName where
  rnf (DefinitionName name spelling) = rnf name `seq` rnf spelling

-- | Recover the structural name at a backend or generated-output boundary.
definitionName :: DefinitionName -> Name
definitionName (DefinitionName name _) = name

-- | Recover the definition's unqualified identifier or operator spelling.
--
-- The spelling is retained at construction, so this accessor is total rather
-- than having to recover a 'Maybe' from the wrapped structural name.
definitionSpelling :: DefinitionName -> String
definitionSpelling (DefinitionName _ spelling) = spelling

-- | Patterns supported by both Djinn and Exference's generated output.
-- Constructor application is structural rather than an arbitrary pattern
-- application, so malformed variable-headed patterns are unrepresentable.
data Pattern local
  = Bind local
  | Wildcard
  | Constructor Name [Pattern local]
  | TuplePattern [Pattern local]
  | As local (Pattern local)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData local => NFData (Pattern local)

-- | Alpha-safe identity for a variable bound inside a closed visible type
-- argument. The scope is allocated in lexical preorder and the slot records
-- the binder's position within that scope. Consequently binder spelling is
-- irrelevant while shadowed binders remain distinct.
data ClosedVisibleTypeVariable = ClosedVisibleTypeVariable
  { closedVisibleTypeVariableScope :: !Natural
  , closedVisibleTypeVariableSlot :: !Natural
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData ClosedVisibleTypeVariable

-- | Stable, valid source spelling for one alpha-normalized bound variable.
closedVisibleTypeVariableSpelling :: ClosedVisibleTypeVariable -> String
closedVisibleTypeVariableSpelling variable =
  "a" ++ show (closedVisibleTypeVariableScope variable)
    ++ "_" ++ show (closedVisibleTypeVariableSlot variable)

-- | One checked visible type argument in generated term syntax.
--
-- The representation is deliberately abstract. An inferred argument renders
-- as @\@_@. A specified argument retains a canonical, structurally validated,
-- lexically closed type. Bound variables are alpha-normalized independently
-- of their source spellings, so quantified arguments do not depend on a type
-- variable scope carried by the enclosing 'FunctionClause'.
data VisibleTypeArgument
  = InferredVisibleTypeArgument
  | SpecifiedVisibleTypeArgument
      (SharedType.Type ClosedVisibleTypeVariable)
  deriving (Eq, Ord, Show)

instance NFData VisibleTypeArgument where
  rnf InferredVisibleTypeArgument = ()
  rnf (SpecifiedVisibleTypeArgument typeExpression) = rnf typeExpression

-- | Why a source type cannot become a bounded visible type argument.
-- Structural validation runs before the bounded-vocabulary check, so malformed
-- caller-built types retain the shared type error that made them invalid.
data VisibleTypeArgumentError variable
  = InvalidVisibleTypeArgument (SharedType.TypeError variable)
  | VisibleTypeArgumentVariable variable
  | VisibleTypeArgumentForall
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData variable => NFData (VisibleTypeArgumentError variable)

-- | The inferred visible type argument @\@_@.
inferredVisibleTypeArgument :: VisibleTypeArgument
inferredVisibleTypeArgument = InferredVisibleTypeArgument

-- | Check and alpha-normalize a lexically closed type into generated-term
-- syntax. Saturated arrows and tuples are canonicalized by the shared type
-- boundary. A variable is accepted only when an explicit enclosing forall
-- owns it; genuinely free variables retain their source identity in the
-- diagnostic.
specifiedVisibleTypeArgument
  :: Ord variable
  => SharedType.Type variable
  -> Either (VisibleTypeArgumentError variable) VisibleTypeArgument
specifiedVisibleTypeArgument source = do
  canonical <- Bifunctor.first InvalidVisibleTypeArgument
    $ SharedType.normalizeType source
  SpecifiedVisibleTypeArgument <$> traverse closeVariable
    (Alpha.alphaNormalizeTypeWith Alpha.PositionalBinderSlots
      $ eraseVacuousVisibleForalls canonical)
 where
  closeVariable variable = case variable of
    Alpha.AlphaBoundVariable scope slot ->
      Right $ ClosedVisibleTypeVariable scope slot
    Alpha.AlphaFreeVariable free ->
      Left $ VisibleTypeArgumentVariable free

-- A binderless, context-free forall is semantically and textually invisible.
-- Erase it before allocating lexical scope numbers so equivalent source trees
-- compare and render identically, including when the no-op wrapper surrounds a
-- later genuine binder.
eraseVacuousVisibleForalls
  :: SharedType.Type variable
  -> SharedType.Type variable
eraseVacuousVisibleForalls source = case source of
  SharedType.TypeVariable{} -> source
  SharedType.TypeConstructor{} -> source
  SharedType.TypeApplication function argument ->
    SharedType.TypeApplication
      (eraseVacuousVisibleForalls function)
      (eraseVacuousVisibleForalls argument)
  SharedType.FunctionType parameter result ->
    SharedType.FunctionType
      (eraseVacuousVisibleForalls parameter)
      (eraseVacuousVisibleForalls result)
  SharedType.TupleType boxity elements -> SharedType.TupleType boxity
    $ map eraseVacuousVisibleForalls elements
  SharedType.ForallType [] [] body -> eraseVacuousVisibleForalls body
  SharedType.ForallType variables constraints body ->
    SharedType.ForallType variables
      (map (fmap eraseVacuousVisibleForalls) constraints)
      (eraseVacuousVisibleForalls body)

-- | Whether this argument is the inferred placeholder @\@_@.
--
-- This discriminator is intentionally separate from the legacy monotype
-- projection: a specified quantified argument also has no monotype view.
isInferredVisibleTypeArgument :: VisibleTypeArgument -> Bool
isInferredVisibleTypeArgument argument = case argument of
  InferredVisibleTypeArgument -> True
  SpecifiedVisibleTypeArgument{} -> False

-- | Recover the specified closed monotype when the argument has no forall
-- layer, or 'Nothing' for either @\@_@ or a specified quantified type.
--
-- This compatibility projection preserves its historical @Void@ index.
-- New consumers which need to distinguish or render quantified arguments
-- should use 'isInferredVisibleTypeArgument' and
-- 'visibleTypeArgumentClosedType'.
visibleTypeArgumentType
  :: VisibleTypeArgument
  -> Maybe (SharedType.Type Void)
visibleTypeArgumentType argument = case argument of
  InferredVisibleTypeArgument -> Nothing
  SpecifiedVisibleTypeArgument typeExpression
    | SharedType.containsForall typeExpression -> Nothing
    | otherwise -> traverse (const Nothing) typeExpression

-- | Recover the complete specified closed type, including explicit forall
-- layers, or 'Nothing' only for @\@_@.
visibleTypeArgumentClosedType
  :: VisibleTypeArgument
  -> Maybe (SharedType.Type ClosedVisibleTypeVariable)
visibleTypeArgumentClosedType argument = case argument of
  InferredVisibleTypeArgument -> Nothing
  SpecifiedVisibleTypeArgument typeExpression -> Just typeExpression

-- | Surface expression tree after backend-specific checking.
--
-- 'Hole' is retained for Exference's inspectable intermediate candidates; a
-- completed checked result normally contains none.
data Expression local
  = Local local
  | Global Name
  | Lambda [Pattern local] (Expression local)
  | Apply (Expression local) (Expression local)
  | VisibleTypeApplication (Expression local) VisibleTypeArgument
  | Tuple [Expression local]
  | Hole local
  | Let (Pattern local) (Expression local) (Expression local)
  | Case (Expression local) [(Pattern local, Expression local)]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData local => NFData (Expression local)

-- | One term or visible type argument in a complete expression application
-- spine. The constructors distinguish the two surface forms while the
-- containing spine retains their source order.
data ApplicationArgument local
  = TermArgument (Expression local)
  | VisibleTypeArgumentArgument VisibleTypeArgument
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData local => NFData (ApplicationArgument local)

-- | One ordinary top-level function equation.
data FunctionClause local = FunctionClause
  { clauseName :: DefinitionName
  , clausePatterns :: [Pattern local]
  , clauseBody :: Expression local
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData local => NFData (FunctionClause local)

-- | Construct a nonempty lambda and merge it with the complete leading lambda
-- spine of its body.
--
-- An empty binder list denotes the body itself rather than the invalid
-- @Lambda [] body@ shape. Keeping this smart constructor beside the generated
-- tree gives cleanup passes and backends one canonical grouping policy.
lambdaExpression
  :: [Pattern local]
  -> Expression local
  -> Expression local
lambdaExpression [] expression = expression
lambdaExpression patterns expression =
  Lambda (patterns ++ morePatterns) body
 where
  ~(morePatterns, body) = expressionLambdaSpine expression

-- | Peel the complete leading nonempty lambda spine into patterns in lexical
-- order and its first non-lambda or malformed empty-lambda body.
--
-- A caller-built @Lambda [] body@ is a syntax error rather than an identity
-- node, so it acts as a barrier and remains in the returned body for
-- 'validateExpressionSyntax' to reject. Lambdas beneath any other expression
-- constructor likewise remain untouched. The returned pair and binder list are
-- lazy in the unconsumed spine, so a consumer can inspect a finite binder
-- prefix without evaluating the terminal body.
expressionLambdaSpine
  :: Expression local
  -> ([Pattern local], Expression local)
expressionLambdaSpine expression@(Lambda [] _) = ([], expression)
expressionLambdaSpine (Lambda patterns body) =
  (patterns ++ morePatterns, finalBody)
 where
  ~(morePatterns, finalBody) = expressionLambdaSpine body
expressionLambdaSpine body = ([], body)

-- | Turn an expression into a top-level equation by promoting its complete
-- leading lambda spine to clause patterns. A malformed empty lambda remains in
-- the clause body so the ordinary syntax validator can still reject it.
functionClauseFromExpression
  :: DefinitionName
  -> Expression local
  -> FunctionClause local
functionClauseFromExpression name expression =
  let (patterns, body) = expressionLambdaSpine expression
  in FunctionClause name patterns body

-- | Recover the expression denoted by a top-level function equation.
--
-- Clause patterns are binders for the body, so expression-oriented consumers
-- must retain them as a leading lambda.  A patternless value equation already
-- denotes its body directly; in particular, this helper never manufactures
-- the syntactically invalid @Lambda [] body@ shape. Any leading lambda in a
-- patterned clause body is folded into the same canonical group.
functionClauseExpression :: FunctionClause local -> Expression local
functionClauseExpression (FunctionClause _ patterns body) =
  lambdaExpression patterns body

-- | Apply term and visible type arguments to a head in source order.
--
-- This is the mixed generated-expression constructor paired with
-- 'expressionFullApplicationSpine'. The spine is built strictly so a wide
-- generated application does not retain a chain of pending folds; argument
-- payloads remain lazy.
applyExpressionArguments
  :: Expression local
  -> [ApplicationArgument local]
  -> Expression local
applyExpressionArguments = List.foldl' applyArgument
 where
  applyArgument function argument = case argument of
    TermArgument value -> Apply function value
    VisibleTypeArgumentArgument typeArgument ->
      VisibleTypeApplication function typeArgument

-- | Decompose a left-associated expression application into its head and
-- arguments in source order. A non-application has no arguments.
--
-- Keeping this structural observation beside the generated tree prevents
-- renderers and backend cleanup passes from maintaining identical private
-- application walks.
expressionApplicationSpine
  :: Expression local
  -> (Expression local, [Expression local])
expressionApplicationSpine = collect []
 where
  collect arguments (Apply function argument) =
    collect (argument : arguments) function
  collect arguments function = (function, arguments)

-- | Decompose a complete left-associated expression application into its head
-- and interleaved term and visible type arguments in source order.
--
-- Unlike 'expressionApplicationSpine', this crosses both 'Apply' and
-- 'VisibleTypeApplication' nodes. A non-application has no arguments.
expressionFullApplicationSpine
  :: Expression local
  -> (Expression local, [ApplicationArgument local])
expressionFullApplicationSpine = collect []
 where
  collect arguments (Apply function argument) =
    collect (TermArgument argument : arguments) function
  collect arguments (VisibleTypeApplication function argument) =
    collect (VisibleTypeArgumentArgument argument : arguments) function
  collect arguments function = (function, arguments)

-- | Rewrite every expression node bottom-up. Children are rewritten from left
-- to right before the supplied function is applied once to their rebuilt
-- parent. Patterns and visible type arguments are retained unchanged.
--
-- Nodes introduced by the supplied function are results, not new traversal
-- roots, so this operation always makes one pass over the source tree.
rewriteExpressionBottomUp
  :: (Expression local -> Expression local)
  -> Expression local
  -> Expression local
rewriteExpressionBottomUp rewrite expression = runIdentity
  $ rewriteExpressionBottomUpM (Identity . rewrite) expression

-- | Effectful 'rewriteExpressionBottomUp'. Effects run in left-to-right
-- postorder and a failing monad can stop before later siblings are inspected.
rewriteExpressionBottomUpM
  :: Monad effect
  => (Expression local -> effect (Expression local))
  -> Expression local
  -> effect (Expression local)
rewriteExpressionBottomUpM rewrite = visit
 where
  visit expression = do
    rebuilt <- descend expression
    rewrite rebuilt

  descend expression = case expression of
    Local{} -> pure expression
    Global{} -> pure expression
    Lambda patterns body -> Lambda patterns <$> visit body
    Apply function argument -> do
      function' <- visit function
      argument' <- visit argument
      pure $ Apply function' argument'
    VisibleTypeApplication function argument -> do
      function' <- visit function
      pure $ VisibleTypeApplication function' argument
    Tuple elements -> Tuple <$> traverse visit elements
    Hole{} -> pure expression
    Let pattern inner body -> do
      inner' <- visit inner
      body' <- visit body
      pure $ Let pattern inner' body'
    Case scrutinee alternatives -> do
      scrutinee' <- visit scrutinee
      alternatives' <- traverse visitAlternative alternatives
      pure $ Case scrutinee' alternatives'

  visitAlternative (pattern, body) = do
    body' <- visit body
    pure (pattern, body')

-- | Observe every pattern binding site in source order.  A present value is
-- an ordinary or as-pattern binder; 'Nothing' is a wildcard.  Unlike the
-- derived 'Foldable' instance, this retains discarded sites, which synthesis
-- backends may need when ranking how economically a candidate uses inputs.
patternBindingSites :: Pattern local -> [Maybe local]
patternBindingSites pattern = case pattern of
  Bind local -> [Just local]
  Wildcard -> [Nothing]
  Constructor _ arguments -> concatMap patternBindingSites arguments
  TuplePattern elements -> concatMap patternBindingSites elements
  As local nested -> Just local : patternBindingSites nested

-- | Collect observations over an expression in left-to-right structural
-- order: @onPattern@ contributes at every pattern position and @onNode@ at
-- every expression node, interleaved as the source reads (a lambda's
-- patterns before its body; a let's pattern, then its binding, then its
-- body; a case's scrutinee, then each alternative's pattern and body).
-- Duplicates are retained.  The whole-tree observers below are its
-- instances; scope-sensitive analyses keep their own recursion.
collectExpression
  :: (Pattern local -> [a])
  -> (Expression local -> [a])
  -> Expression local
  -> [a]
collectExpression onPattern onNode = go
 where
  go expression = onNode expression ++ case expression of
    Lambda patterns body -> concatMap onPattern patterns ++ go body
    Apply function argument -> go function ++ go argument
    VisibleTypeApplication function _ -> go function
    Tuple elements -> concatMap go elements
    Let pattern binding body -> onPattern pattern ++ go binding ++ go body
    Case scrutinee alternatives ->
      go scrutinee ++ concat
        [ onPattern pattern ++ go body | (pattern, body) <- alternatives ]
    Local{} -> []
    Global{} -> []
    Hole{} -> []

-- | Observe binding sites introduced anywhere in an expression, in
-- left-to-right structural order.  Ordinary local occurrences and holes are
-- uses rather than binding sites and therefore do not contribute.
expressionBindingSites :: Expression local -> [Maybe local]
expressionBindingSites = collectExpression patternBindingSites (const [])

-- | Observe all binding sites in a complete generated definition.
functionClauseBindingSites :: FunctionClause local -> [Maybe local]
functionClauseBindingSites (FunctionClause _ patterns body) =
  concatMap patternBindingSites patterns ++ expressionBindingSites body

-- | Collect the projected identities of locals that occur free in an
-- expression. Pattern binders scope only their corresponding body: a let
-- pattern does not scope its binding expression, and case patterns do not
-- scope the scrutinee.
--
-- Projecting first lets annotated backend locals share the same lexical
-- identity without discarding their payloads from the generated tree.
expressionFreeLocalIdentitiesBy
  :: Ord identity
  => (local -> identity)
  -> Expression local
  -> Set identity
expressionFreeLocalIdentitiesBy identity = free
 where
  free expression = case expression of
    Local local -> Set.singleton $ identity local
    Global{} -> Set.empty
    Lambda patterns body -> removePatternBinders patterns $ free body
    Apply function argument -> free function `Set.union` free argument
    VisibleTypeApplication function _ -> free function
    Tuple elements -> Set.unions $ map free elements
    Hole{} -> Set.empty
    Let pattern binding body -> free binding `Set.union`
      removePatternBinders [pattern] (free body)
    Case scrutinee alternatives -> Set.unions
      $ free scrutinee
      : [ removePatternBinders [pattern] $ free body
        | (pattern, body) <- alternatives
        ]

  removePatternBinders patterns freeIdentities =
    foldr Set.delete freeIdentities
      $ map identity $ concatMap patternLocals patterns

-- | Replace every hole with the selected identity by one expression.
--
-- The replacement is inserted as a complete subtree rather than searched
-- recursively. This is the operation needed by incremental synthesis engines:
-- a newly constructed fragment may itself contain fresh holes, but none of
-- those can denote the hole that has just been discharged.
fillExpressionHole
  :: Eq local
  => local
  -> Expression local
  -> Expression local
  -> Expression local
fillExpressionHole selected replacement = fill
 where
  fill original@(Hole local)
    | local == selected = replacement
    | otherwise = original
  fill original@Local{} = original
  fill original@Global{} = original
  fill (Lambda patterns body) = Lambda patterns $ fill body
  fill (Apply function argument) = Apply (fill function) (fill argument)
  fill (VisibleTypeApplication function argument) =
    VisibleTypeApplication (fill function) argument
  fill (Tuple elements) = Tuple $ map fill elements
  fill (Let pattern binding body) =
    Let pattern (fill binding) (fill body)
  fill (Case scrutinee alternatives) = Case (fill scrutinee)
    [(pattern, fill body) | (pattern, body) <- alternatives]

-- | Capture-avoiding substitution of one projected local identity.
--
-- A binder equal to the selected identity shadows the substitution. If a
-- different binder would capture a free local in the replacement, the
-- operation returns 'Nothing' rather than silently changing the generated
-- program. Backends retain responsibility for freshening their payloads when
-- they want to recover from that case.
substituteExpressionLocalBy
  :: Ord identity
  => (local -> identity)
  -> identity
  -> Expression local
  -> Expression local
  -> Maybe (Expression local)
substituteExpressionLocalBy identity selected replacement = replace
 where
  replacementFree = expressionFreeLocalIdentitiesBy identity replacement

  replace expression = case expression of
    original@(Local local)
      | identity local == selected -> Just replacement
      | otherwise -> Just original
    original@Global{} -> Just original
    Lambda patterns body -> Lambda patterns <$> underPatterns patterns body
    Apply function argument ->
      Apply <$> replace function <*> replace argument
    VisibleTypeApplication function argument ->
      (`VisibleTypeApplication` argument) <$> replace function
    Tuple elements -> Tuple <$> traverse replace elements
    original@Hole{} -> Just original
    Let pattern binding body -> Let pattern
      <$> replace binding
      <*> underPatterns [pattern] body
    Case scrutinee alternatives -> Case
      <$> replace scrutinee
      <*> traverse replaceAlternative alternatives

  replaceAlternative (pattern, body) = do
    replacedBody <- underPatterns [pattern] body
    pure (pattern, replacedBody)

  underPatterns patterns body
    | selected `elem` binders = Just body
    | binderCaptures binders body = Nothing
    | otherwise = replace body
   where
    binders = map identity $ concatMap patternLocals patterns

  binderCaptures binders body =
    any (`Set.member` replacementFree) binders
      && selected `Set.member` expressionFreeLocalIdentitiesBy identity body

-- | Compare generated expressions modulo the identities chosen at lexical
-- binding sites.
--
-- Free locals and holes remain exact. Lambda, let, and case binders extend a
-- bidirectional lexical correspondence only for their own bodies, so a free
-- local cannot compare equal to a same-spelled bound local and nested scopes
-- cannot leak renamings into siblings. Pattern constructors and shape remain
-- structural rather than alpha-renamed.
alphaEquivalentExpression
  :: Ord local
  => Expression local
  -> Expression local
  -> Bool
alphaEquivalentExpression = equivalent [] []
 where
  equivalent forward reverseBindings left right = case (left, right) of
    (Local leftLocal, Local rightLocal) ->
      equivalentLocal forward reverseBindings leftLocal rightLocal
    (Global leftName, Global rightName) -> leftName == rightName
    (Lambda leftPatterns leftBody, Lambda rightPatterns rightBody) ->
      underPatterns forward reverseBindings
        leftPatterns rightPatterns leftBody rightBody
    (Apply leftFunction leftArgument, Apply rightFunction rightArgument) ->
      equivalent forward reverseBindings leftFunction rightFunction
        && equivalent forward reverseBindings leftArgument rightArgument
    ( VisibleTypeApplication leftFunction leftArgument
      , VisibleTypeApplication rightFunction rightArgument
      ) -> leftArgument == rightArgument
        && equivalent forward reverseBindings leftFunction rightFunction
    (Tuple leftElements, Tuple rightElements) ->
      equivalentList forward reverseBindings leftElements rightElements
    (Hole leftLocal, Hole rightLocal) -> leftLocal == rightLocal
    (Let leftPattern leftBinding leftBody,
        Let rightPattern rightBinding rightBody) ->
      equivalent forward reverseBindings leftBinding rightBinding
        && underPatterns forward reverseBindings
            [leftPattern] [rightPattern] leftBody rightBody
    (Case leftScrutinee leftAlternatives,
        Case rightScrutinee rightAlternatives) ->
      equivalent forward reverseBindings leftScrutinee rightScrutinee
        && equivalentAlternatives forward reverseBindings
            leftAlternatives rightAlternatives
    _ -> False

  equivalentLocal forward reverseBindings left right = case lookup left forward of
    Just corresponding -> corresponding == right
      && lookup right reverseBindings == Just left
    Nothing -> case lookup right reverseBindings of
      Just _ -> False
      Nothing -> left == right

  equivalentList forward reverseBindings left right =
    length left == length right
      && and (zipWith (equivalent forward reverseBindings) left right)

  equivalentAlternatives forward reverseBindings left right =
    length left == length right
      && and (zipWith equivalentAlternative left right)
   where
    equivalentAlternative
        (leftPattern, leftBody) (rightPattern, rightBody) =
      underPatterns forward reverseBindings
        [leftPattern] [rightPattern] leftBody rightBody

  underPatterns forward reverseBindings
      leftPatterns rightPatterns leftBody rightBody =
    case matchPatternLists leftPatterns rightPatterns of
      Nothing -> False
      Just bindings -> equivalent
        (bindings ++ forward)
        (map (\(left, right) -> (right, left)) bindings ++ reverseBindings)
        leftBody
        rightBody

  matchPatternLists left right
    | length left /= length right = Nothing
    | otherwise = concat <$> sequence (zipWith matchPattern left right)

  matchPattern left right = case (left, right) of
    (Bind leftLocal, Bind rightLocal) ->
      Just [(leftLocal, rightLocal)]
    (Wildcard, Wildcard) -> Just []
    (Constructor leftName leftArguments,
        Constructor rightName rightArguments)
      | leftName == rightName ->
          matchPatternLists leftArguments rightArguments
    (TuplePattern leftElements, TuplePattern rightElements) ->
      matchPatternLists leftElements rightElements
    (As leftLocal leftNested, As rightLocal rightNested) -> do
      nested <- matchPattern leftNested rightNested
      pure $ (leftLocal, rightLocal) : nested
    _ -> Nothing

-- | Normalize a checked clause's record eliminations toward their simplest
-- spelling, then eta-contract the clause when a rewrite fired.
--
-- Three total-term reductions cooperate. A single-alternative elimination
-- that merely projects one field becomes an application of that field's
-- record selector, named by the @(constructor, field index)@ map. A
-- reconstruction @C v1 .. vn@ appearing under @let C v1 .. vn = x@ collapses
-- back to @x@, undoing the deconstruct-and-rebuild spelling that searches
-- required to use every bound variable. A @let@ none of whose binders is
-- used disappears: pattern bindings are lazy, so the dropped match was never
-- forced. Like 'simplifyCaseExpression', these are reductions for
-- independently checked total synthesis terms, not bottom-preserving Haskell
-- optimizations.
projectFieldSelectors
  :: Eq local
  => Map (Name, Int) Name
  -> FunctionClause local
  -> FunctionClause local
projectFieldSelectors = projectFieldSelectorsWithEta True

-- | Apply the same record-selector rewrites as 'projectFieldSelectors' while
-- retaining the clause's eta expansion. Djinn uses this after erased rank-N
-- evidence has crossed proof checking; presentation must not recreate a
-- simplified-subsumption error by contracting that checked boundary.
projectFieldSelectorsWithoutEta
  :: Eq local
  => Map (Name, Int) Name
  -> FunctionClause local
  -> FunctionClause local
projectFieldSelectorsWithoutEta = projectFieldSelectorsWithEta False

projectFieldSelectorsWithEta
  :: Eq local
  => Bool
  -> Map (Name, Int) Name
  -> FunctionClause local
  -> FunctionClause local
projectFieldSelectorsWithEta contractEta selectors clause
  | Map.null selectors = clause
  | rewritten == clauseBody clause = clause
  | contractEta = etaContractClause clause {clauseBody = rewritten}
  | otherwise = clause {clauseBody = rewritten}
 where
  rewritten = converge (8 :: Int) $ clauseBody clause
  converge fuel body
    | fuel <= 0 = body
    | next == body = body
    | otherwise = converge (fuel - 1) next
   where
    next = rewrite body

  rewrite expression = case descend expression of
    Case scrutinee [(Constructor constructor patterns, body)]
      | (Local result, arguments) <- expressionApplicationSpine body
      , Just selector <- fieldSelector constructor patterns result
      , argumentsIndependentOf patterns arguments ->
          foldl Apply (Apply (Global selector) scrutinee) arguments
    Let pattern bound body -> rewriteLet pattern bound body
    other -> other

  descend expression = case expression of
    Local local -> Local local
    Global name -> Global name
    Hole local -> Hole local
    Lambda patterns body -> Lambda patterns $ rewrite body
    Apply function argument -> Apply (rewrite function) (rewrite argument)
    VisibleTypeApplication function argument ->
      VisibleTypeApplication (rewrite function) argument
    Tuple elements -> Tuple $ map rewrite elements
    Let pattern bound body -> Let pattern (rewrite bound) (rewrite body)
    Case scrutinee alternatives -> Case (rewrite scrutinee)
      [(pattern, rewrite body) | (pattern, body) <- alternatives]

  rewriteLet pattern bound body
    | Constructor constructor patterns <- pattern
    , Local result <- body
    , Just selector <- fieldSelector constructor patterns result =
        Apply (Global selector) bound
    | Constructor constructor patterns <- pattern
    , duplicable bound
    , Just binders <- bindersOf patterns
    , let collapsed = substituteRebuild constructor binders bound body
    , collapsed /= body = rewriteLet pattern bound collapsed
    | not (any (`elem` toList body) (toList pattern)) = body
    | otherwise = Let pattern bound body

  -- Only variable-like bound expressions are duplicated into rebuild sites;
  -- anything larger would trade sharing for the shorter spelling.
  duplicable bound = case bound of
    Local _ -> True
    Global _ -> True
    _ -> False

  substituteRebuild constructor binders bound = replace
   where
    replace expression
      | rebuildsBinding expression = bound
      | otherwise = case expression of
          Lambda patterns body -> Lambda patterns $ replace body
          Apply function argument ->
            Apply (replace function) (replace argument)
          VisibleTypeApplication function argument ->
            VisibleTypeApplication (replace function) argument
          Tuple elements -> Tuple $ map replace elements
          Let pattern inner body -> Let pattern (replace inner) (replace body)
          Case scrutinee alternatives -> Case (replace scrutinee)
            [(pattern, replace body) | (pattern, body) <- alternatives]
          other -> other
    rebuildsBinding expression = case expressionApplicationSpine expression of
      (Global name, arguments) -> name == constructor
        && arguments == map Local binders
      _ -> False

  bindersOf = traverse asBind
   where
    asBind (Bind local) = Just local
    asBind _ = Nothing

  fieldSelector constructor patterns result = do
    index <- projectedIndex patterns result
    Map.lookup (constructor, index) selectors

  -- The projected binder must occur at exactly one field position, and every
  -- sibling pattern must be irrefutable and dead, so replacing the whole
  -- elimination with one selector cannot change which field is returned.
  projectedIndex patterns result = case
      [ index
      | (index, pattern) <- zip [0 ..] patterns
      , pattern == Bind result
      ] of
    [index] | all shallowPattern patterns -> Just index
    _ -> Nothing

  shallowPattern pattern = case pattern of
    Bind _ -> True
    Wildcard -> True
    _ -> False

  argumentsIndependentOf patterns = all $ \argument ->
    all (`notElem` toList argument) $ concatMap toList patterns

etaContractClause :: Eq local => FunctionClause local -> FunctionClause local
etaContractClause clause = case
    (reverse $ clausePatterns clause, clauseBody clause) of
  (Bind binder : reversedRest, Apply function (Local argument))
    | binder == argument
    , binder `notElem` toList function -> etaContractClause clause
        { clausePatterns = reverse reversedRest
        , clauseBody = function
        }
  _ -> clause

-- | Construct a case expression while applying the total-term reductions used
-- by synthesis output.
--
-- The operation removes identity cases and alpha-equivalent alternatives and
-- commutes a common leading lambda prefix out of every branch. It is not a
-- semantics-preserving Haskell optimization in the presence of bottoms or
-- @seq@; callers use it only for independently checked total synthesis terms.
simplifyCaseExpression
  :: Ord local
  => Expression local
  -> [(Pattern local, Expression local)]
  -> Expression local
simplifyCaseExpression scrutinee [] = Case scrutinee []
simplifyCaseExpression _ [(Constructor name [], expression)]
  | nameSpecial name == Just (TupleConstructor Boxed 0) = expression
simplifyCaseExpression scrutinee alternatives
  | all (uncurry patternEqualsExpression) alternatives = scrutinee
simplifyCaseExpression scrutinee alternatives@[(pattern, expression)]
  | (patterns@(_ : _), body) <- expressionLambdaSpine expression
  , let simplified = lambdaExpression patterns
          $ simplifyCaseExpression scrutinee [(pattern, body)]
  , rewritePreservesScope original simplified = simplified
 where
  original = Case scrutinee alternatives
simplifyCaseExpression scrutinee
    alternatives@(_ : _)
  | commonCount > 0
  , rewritePreservesScope original simplified = simplified
 where
  original = Case scrutinee alternatives
  simplified = lambdaExpression (map bindingPattern canonicalLocals)
    $ simplifyCaseExpression scrutinee convertedAlternatives

  commonCount = case decomposedAlternatives of
    [] -> 0
    first : rest -> commonBinderCount (availableBinderCount first) rest

  -- Generated alternatives can be a lazy search product. Once one branch has
  -- no hoistable binder, neither its remaining spine nor later alternatives
  -- can change the zero common prefix.
  commonBinderCount 0 _ = 0
  commonBinderCount count [] = count
  commonBinderCount count (alternative : rest) =
    commonBinderCount
      (min count $ availableBinderCount alternative)
      rest

  decomposedAlternatives =
    [ (constructorPattern, patterns, body)
    | (constructorPattern, expression) <- alternatives
    , let (patterns, body) = expressionLambdaSpine expression
    ]

  availableBinderCount (_, patterns, _) =
    length $ takeWhile isVariablePattern patterns

  convertedAlternatives =
    [ let (used, remaining) = splitAt commonCount patterns
          renamings =
            [ (source, target)
            | (Bind source, Just target) <- zip used canonicalLocals
            , source /= target
            ]
      in ( constructorPattern
         , renameLocals renamings $ lambdaExpression remaining expression
         )
    | (constructorPattern, patterns, expression) <- decomposedAlternatives
    ]

  binderColumns = List.transpose
    [ take commonCount patterns
    | (_, patterns, _) <- decomposedAlternatives
    ]
  canonicalLocals = map canonicalLocal binderColumns

  canonicalLocal patterns = case [local | Bind local <- patterns] of
    local : _ -> Just local
    [] -> Nothing
simplifyCaseExpression scrutinee
    alternatives@((_, expression) : remaining@(_ : _))
  | all (alphaEquivalentExpression expression . snd) remaining
  , rewritePreservesScope (Case scrutinee alternatives) expression = expression
simplifyCaseExpression scrutinee alternatives = Case scrutinee alternatives

isVariablePattern :: Pattern local -> Bool
isVariablePattern Bind{} = True
isVariablePattern Wildcard = True
isVariablePattern _ = False

bindingPattern :: Maybe local -> Pattern local
bindingPattern Nothing = Wildcard
bindingPattern (Just local) = Bind local

patternEqualsExpression
  :: Eq local
  => Pattern local
  -> Expression local
  -> Bool
patternEqualsExpression (Bind local) (Local local') = local == local'
patternEqualsExpression (Constructor name patterns) expression =
  case expressionApplicationSpine expression of
    (Global name', arguments) ->
      name == name' && length patterns == length arguments
        && and (zipWith patternEqualsExpression patterns arguments)
    _ -> False
patternEqualsExpression (TuplePattern patterns) (Tuple expressions) =
  length patterns == length expressions
    && and (zipWith patternEqualsExpression patterns expressions)
patternEqualsExpression _ _ = False

-- Rename only lexical occurrences, simultaneously. Patterns introduce new
-- scopes rather than occurrences, and holes are exact synthesis identities;
-- neither may be changed as a side effect of alpha-renaming a hoisted lambda.
renameLocals
  :: Ord local
  => [(local, local)]
  -> Expression local
  -> Expression local
renameLocals renamings = rename $ Map.fromList renamings
 where
  rename replacements expression = case expression of
    Local local -> Local $ Map.findWithDefault local local replacements
    original@Global{} -> original
    Lambda patterns body -> Lambda patterns
      $ rename (underPatterns replacements patterns) body
    Apply function argument -> Apply
      (rename replacements function) (rename replacements argument)
    VisibleTypeApplication function argument ->
      VisibleTypeApplication (rename replacements function) argument
    Tuple elements -> Tuple $ map (rename replacements) elements
    original@Hole{} -> original
    Let pattern binding body -> Let pattern
      (rename replacements binding)
      (rename (underPatterns replacements [pattern]) body)
    Case scrutinee alternatives -> Case
      (rename replacements scrutinee)
      [ (pattern, rename (underPatterns replacements [pattern]) body)
      | (pattern, body) <- alternatives
      ]

  underPatterns replacements patterns = foldr Map.delete replacements
    $ concatMap patternLocals patterns

-- Validate a tentative local rewrite in the lexical context that can contain
-- the original fragment. Closing exactly its free locals admits legitimate
-- outer references while detecting newly free, captured, or duplicate locals.
-- The shared scope validator remains the single authority for those rules.
rewritePreservesScope
  :: Ord local
  => Expression local
  -> Expression local
  -> Bool
rewritePreservesScope original rewritten =
  validateExpressionScope closed == Right ()
 where
  freeLocals = Set.toAscList $ expressionFreeLocalIdentitiesBy id original
  closed = lambdaExpression (map Bind freeLocals) rewritten

-- | Apply 'simplifyCaseExpression' bottom-up throughout a generated tree.
simplifyExpressionCases
  :: Ord local
  => Expression local
  -> Expression local
simplifyExpressionCases expression = case expression of
  original@Local{} -> original
  original@Global{} -> original
  Lambda patterns body -> Lambda patterns $ simplifyExpressionCases body
  Apply function argument -> Apply
    (simplifyExpressionCases function)
    (simplifyExpressionCases argument)
  VisibleTypeApplication function argument ->
    VisibleTypeApplication (simplifyExpressionCases function) argument
  Tuple elements -> Tuple $ map simplifyExpressionCases elements
  original@Hole{} -> original
  Let pattern binding body -> Let pattern
    (simplifyExpressionCases binding)
    (simplifyExpressionCases body)
  Case scrutinee alternatives -> simplifyCaseExpression
    (simplifyExpressionCases scrutinee)
    [ (pattern, simplifyExpressionCases body)
    | (pattern, body) <- alternatives
    ]

-- | Normalize aliases introduced by redundant as-patterns throughout one
-- generated expression.
--
-- An as-pattern over a wildcard becomes an ordinary binder, while an alias of
-- another binder keeps the nested binder and redirects uses of the alias to
-- it. On trees accepted by 'validateExpressionScope', lexical renamings are
-- masked by every nested pattern scope. A case whose alternatives bind nothing
-- and have alpha-equivalent bodies collapses to its first body; like Djinn's
-- historical cleanup, this transformation is intended for total synthesized
-- terms.
normalizeExpressionPatterns
  :: Ord local
  => Expression local
  -> Expression local
normalizeExpressionPatterns = normalize Map.empty
 where
  normalize renamings expression = case expression of
    Local local -> Local $ Map.findWithDefault local local renamings
    original@Global{} -> original
    Lambda patterns body ->
      let (normalizedPatterns, aliases) = normalizePatterns patterns
      in Lambda normalizedPatterns
        $ normalize (underPatterns renamings patterns aliases) body
    Apply function argument ->
      Apply (normalize renamings function) (normalize renamings argument)
    VisibleTypeApplication function argument ->
      VisibleTypeApplication (normalize renamings function) argument
    Tuple elements -> Tuple $ map (normalize renamings) elements
    original@Hole{} -> original
    Let pattern binding body ->
      let (normalizedPattern, aliases) = normalizePattern pattern
      in Let normalizedPattern
        (normalize renamings binding)
        (normalize (underPatterns renamings [pattern] aliases) body)
    Case scrutinee alternatives -> simplifyCaseExpression
      (normalize renamings scrutinee)
      (map (normalizeAlternative renamings) alternatives)

  normalizeAlternative renamings (pattern, body) =
    let (normalizedPattern, aliases) = normalizePattern pattern
    in ( normalizedPattern
       , normalize (underPatterns renamings [pattern] aliases) body
       )

  normalizePatterns patterns =
    let normalized = map normalizePattern patterns
    in (map fst normalized, Map.unions $ map snd normalized)

  normalizePattern pattern = case pattern of
    original@Bind{} -> (original, Map.empty)
    Wildcard -> (Wildcard, Map.empty)
    Constructor name arguments ->
      let (normalized, aliases) = normalizePatterns arguments
      in (Constructor name normalized, aliases)
    TuplePattern elements ->
      let (normalized, aliases) = normalizePatterns elements
      in (TuplePattern normalized, aliases)
    As local nested -> case normalizePattern nested of
      (Wildcard, aliases) -> (Bind local, aliases)
      (Bind nestedLocal, aliases) ->
        (Bind nestedLocal, Map.insert local nestedLocal aliases)
      (normalized, aliases) -> (As local normalized, aliases)

  underPatterns outer patterns aliases = aliases `Map.union`
    foldr Map.delete outer (concatMap patternLocals patterns)

-- | Replace unused pattern binders with wildcards using a backend's stable
-- local-identity projection.
--
-- The analysis is lexical: lambda binders scope only their body, let binders
-- do not scope the binding expression, and case binders are local to their
-- alternative. A tuple pattern collapses to a wildcard when none of its
-- nested binders remains useful; constructor shapes remain intact so that
-- alternatives stay distinguishable. A sole wildcard alternative likewise
-- loses its now-unobserved case wrapper under the total-term assumption used
-- by the synthesis cleanup pipeline.
discardUnusedPatternBindingsBy
  :: Ord identity
  => (local -> identity)
  -> Expression local
  -> Expression local
discardUnusedPatternBindingsBy identity expression = fst $ discard expression
 where
  discard original = case original of
    Lambda patterns body ->
      let (body', freeInBody) = discard body
          binders = patternIdentities patterns
      in ( Lambda (map (discardPattern freeInBody) patterns) body'
         , freeInBody `Set.difference` binders
         )
    Apply function argument ->
      let (function', freeInFunction) = discard function
          (argument', freeInArgument) = discard argument
      in ( Apply function' argument'
         , freeInFunction `Set.union` freeInArgument
         )
    VisibleTypeApplication function argument ->
      let (function', freeInFunction) = discard function
      in (VisibleTypeApplication function' argument, freeInFunction)
    Tuple elements ->
      let (elements', freeInElements) = unzip $ map discard elements
      in (Tuple elements', Set.unions freeInElements)
    Case scrutinee alternatives ->
      let (scrutinee', freeInScrutinee) = discard scrutinee
          (alternatives', freeInAlternatives) = unzip
            [ let (body', freeInBody) = discard body
                  binders = patternIdentities [pattern]
              in ( (discardPattern freeInBody pattern, body')
                 , freeInBody `Set.difference` binders
                 )
            | (pattern, body) <- alternatives
            ]
      in case alternatives' of
        [(Wildcard, body)] ->
          (body, Set.unions freeInAlternatives)
        _ ->
          ( Case scrutinee' alternatives'
          , freeInScrutinee `Set.union` Set.unions freeInAlternatives
          )
    Let pattern binding body ->
      let (binding', freeInBinding) = discard binding
          (body', freeInBody) = discard body
          binders = patternIdentities [pattern]
      in ( Let (discardPattern freeInBody pattern) binding' body'
         , freeInBinding `Set.union` (freeInBody `Set.difference` binders)
         )
    Local local -> (original, Set.singleton $ identity local)
    Global{} -> (original, Set.empty)
    Hole{} -> (original, Set.empty)

  patternIdentities = Set.fromList
    . map identity . concatMap patternLocals

  discardPattern freeInBody pattern = case pattern of
    original@(Bind local)
      | identity local `Set.member` freeInBody -> original
      | otherwise -> Wildcard
    Wildcard -> Wildcard
    Constructor name arguments ->
      Constructor name $ map (discardPattern freeInBody) arguments
    TuplePattern elements ->
      let retained = map (discardPattern freeInBody) elements
      in if all isWildcard retained
        then Wildcard
        else TuplePattern retained
    As local nested
      | identity local `Set.member` freeInBody ->
          As local $ discardPattern freeInBody nested
      | otherwise -> discardPattern freeInBody nested

  isWildcard Wildcard = True
  isWildcard _ = False

-- Exact totals are irrelevant to the simplifier: zero, one, and multiple
-- occurrences are its only semantic cases. Saturating here also keeps the
-- analysis independent of machine-sized counters.
data Occurrences = Unused | UsedOnce | UsedMany
  deriving (Eq)

combineOccurrences :: Occurrences -> Occurrences -> Occurrences
combineOccurrences Unused right = right
combineOccurrences left Unused = left
combineOccurrences _ _ = UsedMany

-- | Simplify a generated synthesis term using a backend's local-identity
-- projection.
--
-- The operation removes unused variable lets, capture-safely inlines a
-- single use, and applies the ordinary eta law. It is deliberately not a
-- general Haskell optimizer: synthesis backends use it for their total,
-- independently checked output trees. Local payloads (such as Exference's
-- type annotations) are preserved verbatim; only the projected identity
-- participates in binding and occurrence comparisons.
simplifyExpressionBy
  :: Ord identity
  => (local -> identity)
  -> Expression local
  -> Expression local
simplifyExpressionBy = simplifyExpressionWithEtaBy True

-- | Apply the binding cleanup from 'simplifyExpressionBy' without eta
-- contraction. Backends use this narrow variant when an eta-expanded term is
-- carrying evidence which disappears before rendering, so the ordinary
-- untyped eta law would not preserve Haskell's higher-rank subsumption rules.
simplifyExpressionWithoutEtaBy
  :: Ord identity
  => (local -> identity)
  -> Expression local
  -> Expression local
simplifyExpressionWithoutEtaBy = simplifyExpressionWithEtaBy False

simplifyExpressionWithEtaBy
  :: Ord identity
  => Bool
  -> (local -> identity)
  -> Expression local
  -> Expression local
simplifyExpressionWithEtaBy contractEta identity =
  (if contractEta then simplifyEta else id) . simplifyLets
 where
  simplifyLets expression = case expression of
    original@Local{} -> original
    original@Global{} -> original
    Lambda patterns body -> Lambda patterns $ simplifyLets body
    Apply function argument ->
      Apply (simplifyLets function) (simplifyLets argument)
    VisibleTypeApplication function argument ->
      VisibleTypeApplication (simplifyLets function) argument
    Tuple elements -> Tuple $ map simplifyLets elements
    original@Hole{} -> original
    Let pattern binding body -> case pattern of
      Bind local -> case occurrencesOf (identity local) body of
        Unused -> simplifyLets body
        UsedOnce -> case substituteExpressionLocalBy
            identity (identity local) binding body of
          Just replaced -> simplifyLets replaced
          Nothing -> retainLet
        UsedMany -> retainLet
      _ -> retainLet
     where
      retainLet = Let pattern
        (simplifyLets binding)
        (simplifyLets body)
    Case scrutinee alternatives -> Case (simplifyLets scrutinee)
      [ (pattern, simplifyLets body)
      | (pattern, body) <- alternatives
      ]

  simplifyEta expression = reduceEta $ case expression of
    original@Local{} -> original
    original@Global{} -> original
    Lambda patterns body -> Lambda patterns $ simplifyEta body
    Apply function argument ->
      Apply (simplifyEta function) (simplifyEta argument)
    VisibleTypeApplication function argument ->
      VisibleTypeApplication (simplifyEta function) argument
    Tuple elements -> Tuple $ map simplifyEta elements
    original@Hole{} -> original
    Let pattern binding body ->
      Let pattern (simplifyEta binding) (simplifyEta body)
    Case scrutinee alternatives -> Case (simplifyEta scrutinee)
      [ (pattern, simplifyEta body)
      | (pattern, body) <- alternatives
      ]

  reduceEta expression = case expression of
    Lambda [Bind binder] (Apply function (Local argument))
      | identity binder == identity argument
      , occurrencesOf (identity binder) function == Unused -> function
    _ -> expression

  occurrencesOf selected expression = case expression of
    Local local
      | identity local == selected -> UsedOnce
      | otherwise -> Unused
    Global{} -> Unused
    Lambda patterns body -> underPatterns patterns body
    Apply function argument -> combineOccurrences
      (occurrencesOf selected function)
      (occurrencesOf selected argument)
    VisibleTypeApplication function _ -> occurrencesOf selected function
    Tuple elements -> foldr
      (combineOccurrences . occurrencesOf selected)
      Unused
      elements
    Hole{} -> Unused
    Let pattern binding body -> combineOccurrences
      (occurrencesOf selected binding)
      (underPatterns [pattern] body)
    Case scrutinee alternatives -> foldr
      (combineOccurrences . occurrenceInAlternative)
      (occurrencesOf selected scrutinee)
      alternatives
   where
    underPatterns patterns body
      | selected `elem` map identity (concatMap patternLocals patterns) =
          Unused
      | otherwise = occurrencesOf selected body

    occurrenceInAlternative (pattern, body) = underPatterns [pattern] body

-- | Rendering choices that depend on a backend's local identity type.
data RenderOptions local = RenderOptions
  { renderQualification :: Qualification
  , localNamePreference :: local -> String
  , reservedLocalNames :: [String]
  }

-- | Fully qualified rendering with no explicit reservations.
defaultRenderOptions :: (local -> String) -> RenderOptions local
defaultRenderOptions preference = RenderOptions
  { renderQualification = FullyQualified
  , localNamePreference = preference
  , reservedLocalNames = []
  }

-- | Construct rendering options from backend-provided local-name hints.
--
-- A present hint is authoritative: the renderer validates it as supplied and
-- reports any lexical error instead of silently replacing it. Only locals
-- absent from the map use the fallback. Explicit reservations still
-- participate in ordinary collision avoidance during allocation.
renderOptionsWithLocalNameHints
  :: Ord local
  => Qualification
  -> Map local String
  -> (local -> String)
  -> [String]
  -> RenderOptions local
renderOptionsWithLocalNameHints qualification hints fallback reserved =
  RenderOptions
    { renderQualification = qualification
    , localNamePreference = \local ->
        Map.findWithDefault (fallback local) local hints
    , reservedLocalNames = reserved
    }

-- | Structural or lexical reason a generated expression cannot be emitted as
-- unambiguous Haskell source under the requested qualification policy.
data RenderError
  = InvalidLocalName String NameError
  | LocalNameIsWildcard
  -- Candidate rendering found a local with no enclosing binding site. The
  -- backend-specific identity remains available through 'ScopeError'.
  | UnboundLocalIdentity
  -- Candidate rendering found one identity at multiple binding sites.
  | DuplicateLocalBinderIdentity
  | InvalidGlobalExpression Name
  | InvalidTupleExpressionArity Int
  | InvalidTuplePatternArity Int
  | InvalidConstructorPattern Name
  | InvalidConstructorPatternArity Name Int Int
  | EmptyLambda
  | InvalidFunctionName Name
  | GlobalDefinitionCapture Name Name Qualification
  | UnexpectedResidualConstraints
    -- ^ A backend that produces only closed terms received a caller-built
    -- candidate carrying at least one unresolved obligation.
  deriving (Eq, Ord, Show, Generic)

instance NFData RenderError

-- | A local identity that is unbound or introduced more than once at one
-- pattern binding site.
data ScopeError local
  = UnboundLocal local
  | DuplicatePatternBinder local
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData local => NFData (ScopeError local)

-- | Check lexical local-variable scope independently of rendering.
validateExpressionScope
  :: Ord local
  => Expression local
  -> Either (ScopeError local) ()
validateExpressionScope = validateExpression Set.empty

-- | Check all pattern binders and local uses in a complete function clause.
validateFunctionClauseScope
  :: Ord local
  => FunctionClause local
  -> Either (ScopeError local) ()
validateFunctionClauseScope (FunctionClause _ patterns body) = do
  binders <- distinctPatternBinders patterns
  validateExpression (Set.fromList binders) body

validateExpression
  :: Ord local
  => Set local
  -> Expression local
  -> Either (ScopeError local) ()
validateExpression bound expression = case expression of
  Local local
    | local `Set.member` bound -> Right ()
    | otherwise -> Left $ UnboundLocal local
  Global{} -> Right ()
  Lambda patterns body -> do
    binders <- distinctPatternBinders patterns
    extended <- extendScope bound binders
    validateExpression extended body
  Apply function argument ->
    validateExpression bound function >> validateExpression bound argument
  VisibleTypeApplication function _ -> validateExpression bound function
  Tuple elements -> mapM_ (validateExpression bound) elements
  Hole{} -> Right ()
  Let pattern binding body -> do
    binders <- distinctPatternBinders [pattern]
    validateExpression bound binding
    extended <- extendScope bound binders
    validateExpression extended body
  Case scrutinee alternatives -> do
    validateExpression bound scrutinee
    mapM_ validateAlternative alternatives
    where
      validateAlternative (pattern, body) = do
        binders <- distinctPatternBinders [pattern]
        extended <- extendScope bound binders
        validateExpression extended body

extendScope
  :: Ord local
  => Set local
  -> [local]
  -> Either (ScopeError local) (Set local)
extendScope bound binders = case filter (`Set.member` bound) binders of
  duplicate : _ -> Left $ DuplicatePatternBinder duplicate
  [] -> Right $ bound `Set.union` Set.fromList binders

distinctPatternBinders
  :: Ord local
  => [Pattern local]
  -> Either (ScopeError local) [local]
distinctPatternBinders patterns = collect Set.empty [] $ concatMap binders patterns
  where
    collect _ result [] = Right $ reverse result
    collect seen result (local : rest)
      | local `Set.member` seen = Left $ DuplicatePatternBinder local
      | otherwise = collect (Set.insert local seen) (local : result) rest

    binders pattern = case pattern of
      Bind local -> [local]
      Wildcard -> []
      Constructor _ arguments -> concatMap binders arguments
      TuplePattern elements -> concatMap binders elements
      As local nested -> local : binders nested

-- | Allocate stable, distinct Haskell spellings for every local identity.
-- Global identifiers emitted without qualification and explicit caller
-- reservations participate in the same collision set.  A local rendered as a
-- 'Hole' owns both its base spelling and the emitted underscore-prefixed one.
allocateLocalNames
  :: Ord local
  => RenderOptions local
  -> Expression local
  -> Either RenderError (Map local String)
allocateLocalNames options expression =
  allocate options
    (expressionLocals expression)
    (Set.fromList $ expressionHoles expression)
    (expressionGlobals expression)

-- | Allocate collision-free local spellings for a complete function clause.
-- The definition name and every emitted global participate in collision
-- avoidance under the selected qualification policy.
allocateClauseLocalNames
  :: Ord local
  => RenderOptions local
  -> FunctionClause local
  -> Either RenderError (Map local String)
allocateClauseLocalNames options (FunctionClause name patterns body) =
  allocate options
    (concatMap patternLocals patterns ++ expressionLocals body)
    (Set.fromList $ expressionHoles body)
    (definitionName name :
      concatMap patternGlobals patterns ++ expressionGlobals body)

allocate
  :: Ord local
  => RenderOptions local
  -> [local]
  -> Set local
  -> [Name]
  -> Either RenderError (Map local String)
allocate options locals holeLocals globals =
  fmap fst $ List.foldl' allocateOne initial locals
  where
    initial = Right
      ( Map.empty
      , Set.fromList (reservedLocalNames options)
          `Set.union` Set.fromList
            [ spelling
            | name <- globals
            , Just spelling <- [emittedIdentifier
                (renderQualification options) name]
            ]
      )

    allocateOne state local = do
      (allocated, used) <- state
      case Map.lookup local allocated of
        Just _ -> Right (allocated, used)
        Nothing -> do
          preferred <- validateLocalName $ localNamePreference options local
          let hasHole = local `Set.member` holeLocals
              chosen = freshName used hasHole preferred
          Right
            ( Map.insert local chosen allocated
            , used `Set.union` Set.fromList (localSpellings hasHole chosen)
            )

validateLocalName :: String -> Either RenderError String
validateLocalName "_" = Left LocalNameIsWildcard
validateLocalName spelling = case mkIdentifier spelling of
  Left nameError -> Left $ InvalidLocalName spelling nameError
  Right name
    | nameLexicalClass name == VariableLike -> Right spelling
    | otherwise -> Left $ InvalidLocalName spelling
        (InvalidIdentifier spelling)

freshName :: Set String -> Bool -> String -> String
freshName used hasHole = Fresh.selectFreshBy conflicts (++ "'") used
 where
  conflicts spelling reserved = any (`Set.member` reserved)
    $ localSpellings hasHole spelling

-- The base spelling participates even when every occurrence is a hole: it is
-- the stable identity allocated to that local and must remain distinct from
-- ordinary locals.  Hole syntax contributes one additional emitted spelling.
localSpellings :: Bool -> String -> [String]
localSpellings hasHole spelling
  | hasHole = [spelling, '_' : spelling]
  | otherwise = [spelling]

-- | Validate and render a generated expression as Haskell source.
renderExpression
  :: Ord local
  => RenderOptions local
  -> Expression local
  -> Either RenderError String
renderExpression options expression = do
  validateExpressionSyntax expression
  names <- allocateLocalNames options expression
  Right $ renderStyle style $ ppExpression options names 0 expression

-- | Validate and render a generated top-level equation as Haskell source.
renderFunctionClause
  :: Ord local
  => RenderOptions local
  -> FunctionClause local
  -> Either RenderError String
renderFunctionClause options clause@(FunctionClause name patterns body) = do
  validateFunctionClauseSyntax (renderQualification options) clause
  names <- allocateClauseLocalNames options clause
  Right $ renderStyle style $ sep
    [ text (renderNamePrefix (renderQualification options)
        $ definitionName name) <+>
        sep (map (ppPattern options names 10) patterns) <+> text "="
    , nest 2 $ ppExpression options names 0 body
    ]

-- | Validate Haskell syntax constraints that are independent of local scope.
validateExpressionSyntax :: Expression local -> Either RenderError ()
validateExpressionSyntax expression = case expression of
  Local{} -> Right ()
  Global name
    | Just FunctionConstructor <- nameSpecial name ->
        Left $ InvalidGlobalExpression name
    | otherwise -> Right ()
  Lambda [] _ -> Left EmptyLambda
  Lambda patterns body ->
    mapM_ validatePatternSyntax patterns >> validateExpressionSyntax body
  Apply function argument ->
    validateExpressionSyntax function >> validateExpressionSyntax argument
  VisibleTypeApplication function _ -> validateExpressionSyntax function
  Tuple elements -> do
    validateTupleArity InvalidTupleExpressionArity elements
    mapM_ validateExpressionSyntax elements
  Hole{} -> Right ()
  Let pattern binding body ->
    validatePatternSyntax pattern >>
      validateExpressionSyntax binding >>
      validateExpressionSyntax body
  Case scrutinee alternatives ->
    validateExpressionSyntax scrutinee >> mapM_ validateAlternative alternatives
    where
      validateAlternative (pattern, body) =
        validatePatternSyntax pattern >> validateExpressionSyntax body

-- | Validate a top-level clause's generated syntax under the qualification
-- policy that will be used to emit it.  Definition capture belongs here, not
-- in a particular text renderer: erasing a global's qualifier can turn a
-- structurally non-recursive body into an accidental self-reference.
validateFunctionClauseSyntax
  :: Qualification
  -> FunctionClause local
  -> Either RenderError ()
validateFunctionClauseSyntax qualification
    (FunctionClause name patterns body) = do
  mapM_ validatePatternSyntax patterns
  validateExpressionSyntax body
  case List.find capturesDefinition $ expressionGlobals body of
    Just global -> Left $ GlobalDefinitionCapture
      (definitionName name) global qualification
    Nothing -> Right ()
 where
  capturesDefinition global =
    renderNamePrefix qualification global ==
      renderNamePrefix qualification (definitionName name)

-- | Count structural nodes losslessly, independently of rendered names and
-- qualification. Search heuristics can prefer smaller terms without making
-- rank depend on presentation policy or identifier length.
expressionSizeNatural :: Expression local -> Natural
expressionSizeNatural expression = 1 + case expression of
  Local{} -> 0
  Global{} -> 0
  Lambda _ body -> expressionSizeNatural body
  Apply function argument ->
    expressionSizeNatural function + expressionSizeNatural argument
  VisibleTypeApplication function _ -> expressionSizeNatural function
  Tuple elements -> sumExpressionSizes elements
  Hole{} -> 0
  Let _ value body ->
    expressionSizeNatural value + expressionSizeNatural body
  Case scrutinee alternatives -> expressionSizeNatural scrutinee
    + List.foldl'
        (\total (_, body) -> total + expressionSizeNatural body)
        0 alternatives
 where
  sumExpressionSizes = List.foldl'
    (\total child -> total + expressionSizeNatural child) 0

-- | Historical machine-sized projection of 'expressionSizeNatural'. Large
-- generated trees saturate instead of wrapping to a misleading small or
-- negative size.
expressionSize :: Expression local -> Int
expressionSize = saturatingNaturalToInt . expressionSizeNatural

validatePatternSyntax :: Pattern local -> Either RenderError ()
validatePatternSyntax pattern = case pattern of
  Bind{} -> Right ()
  Wildcard -> Right ()
  Constructor name arguments
    | nameLexicalClass name /= ConstructorLike ->
        Left $ InvalidConstructorPattern name
    | Just FunctionConstructor <- nameSpecial name ->
        Left $ InvalidConstructorPattern name
    | Just (TupleConstructor Unboxed _) <- nameSpecial name ->
        Left $ InvalidConstructorPattern name
    | Just expected <- patternConstructorArity name
    , let observed = observedListLength expected arguments
    , expected /= observed ->
        Left $ InvalidConstructorPatternArity
          name expected observed
    | otherwise -> mapM_ validatePatternSyntax arguments
  TuplePattern elements -> do
    validateTupleArity InvalidTuplePatternArity elements
    mapM_ validatePatternSyntax elements
  As _ nested -> validatePatternSyntax nested

-- Generated tuples are boxed and therefore share the exact representational
-- arity limit owned by 'Name'. Inspect at most one element beyond that limit:
-- once a tuple is known to be too wide, forcing an arbitrary or cyclic tail
-- cannot improve the diagnostic. The reported arity is consequently the
-- first invalid width for every oversized spine.
validateTupleArity
  :: (Int -> RenderError)
  -> [element]
  -> Either RenderError ()
validateTupleArity failure elements
  | observed == 1 || observed > maximumTupleArity = Left $ failure observed
  | otherwise = Right ()
 where
  observed = observedListLength maximumTupleArity elements

-- | Construct a checked generated top-level value name.
mkDefinitionName :: Name -> Either RenderError DefinitionName
mkDefinitionName name
  | nameModule name == Nothing
  , nameLexicalClass name == VariableLike
  , Just spelling <- nameSpelling name
  , spelling /= "_" = Right $ DefinitionName name spelling
  | otherwise = Left $ InvalidFunctionName name

-- | Validate a generated top-level value name independently of its body.
--
-- Retained for source compatibility with clients that only need validation;
-- new request boundaries should retain the t'DefinitionName' returned by
-- 'mkDefinitionName'.
validateDefinitionName :: Name -> Either RenderError ()
validateDefinitionName name = () <$ mkDefinitionName name

ppExpression
  :: Ord local
  => RenderOptions local
  -> Map local String
  -> Int
  -> Expression local
  -> Doc
ppExpression options names precedence expression = case expression of
  Local local -> text $ localName options names local
  Global name -> text $ renderNamePrefix qualification name
  Lambda patterns body -> parenthesize (precedence > 0) $
    sep
      [ text "\\" <> sep (map (ppPattern options names 10) patterns)
          <+> text "->"
      , nest 2 $ ppExpression options names 0 body
      ]
  application@Apply{} -> ppApplication options names precedence application
  VisibleTypeApplication function argument ->
    parenthesize (precedence > 11) $
      ppExpression options names 11 function <+>
        text ('@' : renderVisibleTypeArgument qualification argument)
  Tuple elements -> parens $ fsep $ punctuate comma
    $ map (ppExpression options names 0) elements
  Hole local -> text $ '_' : localName options names local
  Let pattern binding body -> parenthesize (precedence > 0) $
    sep
      [ text "let" <+> ppPattern options names 0 pattern <+> text "="
          <+> ppExpression options names 0 binding
      , text "in" <+> ppExpression options names 0 body
      ]
  Case scrutinee [] -> parenthesize (precedence > 0) $
    text "case" <+> ppExpression options names 0 scrutinee <+> text "of {}"
  Case scrutinee alternatives -> parenthesize (precedence > 0) $
    (text "case" <+> ppExpression options names 0 scrutinee <+> text "of")
      $$ vcat (map ppAlternative alternatives)
  where
    qualification = renderQualification options
    ppAlternative (pattern, body) =
      ppPattern options names 0 pattern <+> text "->" <+>
        ppExpression options names 0 body

renderVisibleTypeArgument :: Qualification -> VisibleTypeArgument -> String
renderVisibleTypeArgument qualification argument = case argument of
  InferredVisibleTypeArgument -> "_"
  SpecifiedVisibleTypeArgument typeExpression ->
    SharedTypeRender.showsTypeWithQualification qualification
      closedVisibleTypeVariableSpelling 2 typeExpression ""

ppApplication
  :: Ord local
  => RenderOptions local
  -> Map local String
  -> Int
  -> Expression local
  -> Doc
ppApplication options names precedence expression =
  case expressionApplicationSpine expression of
    (Global name, arguments)
      | Just arity <- boxedTupleArity name
      , arity == length arguments ->
          parens $ fsep $ punctuate comma
            $ map (ppExpression options names 0) arguments
    (Global name, [left, right])
      | isExpressionOperator name -> parenthesize (precedence > 4) $
          ppExpression options names 5 left <+>
          text (renderNameInfix (renderQualification options) name) <+>
          ppExpression options names 5 right
    (function, arguments) -> parenthesize (precedence > 11) $
      sep $ ppExpression options names 11 function
        : map (ppExpression options names 12) arguments

ppPattern
  :: Ord local
  => RenderOptions local
  -> Map local String
  -> Int
  -> Pattern local
  -> Doc
ppPattern options names precedence pattern = case pattern of
  Bind local -> text $ localName options names local
  Wildcard -> text "_"
  Constructor name arguments
    | Just arity <- boxedTupleArity name
    , arity == length arguments ->
        parens $ fsep $ punctuate comma
          $ map (ppPattern options names 0) arguments
    | [left, right] <- arguments
    , isPatternOperator name -> parenthesize (precedence > 1) $
        ppPattern options names 2 left <+>
        text (renderNameInfix (renderQualification options) name) <+>
        ppPattern options names 2 right
    | otherwise -> parenthesize (precedence > 1 && not (null arguments)) $
        sep $ text (renderNamePrefix (renderQualification options) name)
          : map (ppPattern options names 2) arguments
  TuplePattern elements -> parens $ fsep $ punctuate comma
    $ map (ppPattern options names 0) elements
  As local nested ->
    text (localName options names local) <> text "@" <>
      ppPattern options names 10 nested

parenthesize :: Bool -> Doc -> Doc
parenthesize True = parens
parenthesize False = id

localName :: Ord local => RenderOptions local -> Map local String -> local -> String
localName options names local =
  Map.findWithDefault (localNamePreference options local) local names

boxedTupleArity :: Name -> Maybe Int
boxedTupleArity name = case nameSpecial name of
  Just (TupleConstructor Boxed arity) -> Just arity
  _ -> Nothing

patternConstructorArity :: Name -> Maybe Int
patternConstructorArity name = case nameSpecial name of
  Just ListConstructor -> Just 0
  Just ConsConstructor -> Just 2
  Just FunctionConstructor -> Nothing
  Just (TupleConstructor _ arity) -> Just arity
  Nothing -> Nothing

isExpressionOperator :: Name -> Bool
isExpressionOperator name = case nameOccurrence name of
  OperatorOccurrence _ _ -> True
  SpecialOccurrence ConsConstructor -> True
  _ -> False

isPatternOperator :: Name -> Bool
isPatternOperator name = case nameOccurrence name of
  OperatorOccurrence ConstructorLike _ -> True
  SpecialOccurrence ConsConstructor -> True
  _ -> False

patternLocals :: Pattern local -> [local]
patternLocals = toList

patternGlobals :: Pattern local -> [Name]
patternGlobals pattern = case pattern of
  Bind{} -> []
  Wildcard -> []
  Constructor name arguments -> name : concatMap patternGlobals arguments
  TuplePattern elements -> concatMap patternGlobals elements
  As _ nested -> patternGlobals nested

expressionLocals :: Expression local -> [local]
expressionLocals = toList

-- | Collect hole identities in left-to-right structural order, retaining
-- duplicates. Patterns do not contain holes and therefore do not contribute
-- their binders. Keeping this independent of local-allocation order also lets
-- an ordinary occurrence precede a hole with the same identity safely.
expressionHoles :: Expression local -> [local]
expressionHoles = collectExpression (const []) hole
 where
  hole (Hole local) = [local]
  hole _ = []

-- | Collect every referenced global and pattern constructor in left-to-right
-- structural order, retaining duplicates.
expressionGlobals :: Expression local -> [Name]
expressionGlobals = collectExpression patternGlobals global
 where
  global (Global name) = [name]
  global _ = []
