module Language.Haskell.Exference.ExpressionToHaskellSrc
  ( HaskellSrcConversionError (..)
  , generatedExpressionToHaskellSrc
  , generatedFunctionClauseToHaskellSrc
  , expressionToHaskellSrc
  , functionToHaskellSrc
  )
where

import Control.Monad (forM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Bifunctor (first)
import Data.Map (Map)
import qualified Data.Map as Map
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo, noSrcSpan)
import Language.Haskell.Exts.Syntax

import qualified Language.Haskell.Exference.Core.Expression as E
import qualified Language.Haskell.Exference.Core.Types as T
import qualified Language.Haskell.Synthesis.Generated as Generated
import qualified Language.Haskell.Synthesis.Name as SharedName

type HsExp = Exp SrcSpanInfo
type HsDecl = Decl SrcSpanInfo
type Conversion local =
  ReaderT (Map local String) (Either (HaskellSrcConversionError local))

-- | A checked structural conversion can fail either at the shared lexical
-- scope boundary or while validating/allocating generated Haskell syntax.
data HaskellSrcConversionError local
  = HaskellSrcScopeError (Generated.ScopeError local)
  | HaskellSrcSyntaxError Generated.RenderError
  deriving (Eq, Show)

-- | Convert a checked, parser-independent generated expression directly to a
-- haskell-src-exts tree.  No render/parse round trip is involved.
generatedExpressionToHaskellSrc
  :: Ord local
  => Generated.RenderOptions local
  -> Generated.Expression local
  -> Either (HaskellSrcConversionError local) HsExp
generatedExpressionToHaskellSrc options expression = do
  first HaskellSrcScopeError
    $ Generated.validateExpressionScope expression
  first HaskellSrcSyntaxError
    $ Generated.validateExpressionSyntax expression
  names <- first HaskellSrcSyntaxError
    $ Generated.allocateLocalNames options expression
  runReaderT
    (convertRootExpression (Generated.renderQualification options) expression)
    names

-- | Convert a checked shared function clause to one ordinary HSE equation.
-- Definition capture is checked under the exact qualification policy used by
-- the structural conversion.
generatedFunctionClauseToHaskellSrc
  :: Ord local
  => Generated.RenderOptions local
  -> Generated.FunctionClause local
  -> Either (HaskellSrcConversionError local) HsDecl
generatedFunctionClauseToHaskellSrc options clause = do
  first HaskellSrcScopeError
    $ Generated.validateFunctionClauseScope clause
  convertCheckedFunctionClause options clause

-- The caller has established lexical scope.  Separating the remaining
-- structural conversion lets the raw-name compatibility entry point preserve
-- its historical scope-before-name diagnostics without checking scope twice.
convertCheckedFunctionClause
  :: Ord local
  => Generated.RenderOptions local
  -> Generated.FunctionClause local
  -> Either (HaskellSrcConversionError local) HsDecl
convertCheckedFunctionClause options clause = do
  first HaskellSrcSyntaxError
    $ Generated.validateFunctionClauseSyntax
        (Generated.renderQualification options) clause
  names <- first HaskellSrcSyntaxError
    $ Generated.allocateClauseLocalNames options clause
  runReaderT
    (convertFunctionClause (Generated.renderQualification options) clause)
    names

-- Qualification level: 0 emits unqualified names, 1 qualifies ordinary
-- identifiers but keeps operators infix-friendly, and 2 qualifies everything.
-- | Validate an Exference expression before producing a compatibility HSE
-- expression. Partial search trees with free locals are rejected explicitly.
expressionToHaskellSrc
  :: Int
  -> E.Expression
  -> Either E.ExpressionRenderError HsExp
expressionToHaskellSrc qualification expression = first toExpressionRenderError
  $ generatedExpressionToHaskellSrc options
  $ E.toGeneratedExpression expression
 where
  options = expressionRenderOptions
    (E.qualificationFromLevel qualification) expression

-- | Convert an Exference expression into one checked top-level HSE equation.
-- Structural names report invalid definitions and globals that qualification
-- would turn into accidental self-references.
functionToHaskellSrc
  :: Int
  -> SharedName.Name
  -> E.Expression
  -> Either E.ExpressionRenderError HsDecl
functionToHaskellSrc qualification functionName expression = do
  first toExpressionRenderError
    $ first HaskellSrcScopeError
    $ Generated.validateExpressionScope generated
  checkedName <- first (toExpressionRenderError . HaskellSrcSyntaxError)
    $ Generated.mkDefinitionName functionName
  first toExpressionRenderError
    $ convertCheckedFunctionClause options
    $ Generated.functionClauseFromExpression checkedName generated
 where
  policy = E.qualificationFromLevel qualification
  options = expressionRenderOptions policy expression
  generated = E.toGeneratedExpression expression

expressionRenderOptions
  :: Generated.Qualification
  -> E.Expression
  -> Generated.RenderOptions T.TVarId
expressionRenderOptions qualification expression =
  Generated.renderOptionsWithLocalNameHints
    qualification (E.expressionNameHints expression) T.showVar []

toExpressionRenderError
  :: HaskellSrcConversionError T.TVarId
  -> E.ExpressionRenderError
toExpressionRenderError conversionError = case conversionError of
  HaskellSrcScopeError scopeError -> E.ExpressionScopeError scopeError
  HaskellSrcSyntaxError syntaxError -> E.ExpressionSyntaxError syntaxError

-- Only the outermost lambda spine is flattened.  Nested lambdas beneath an
-- application, let, case, or lambda body retain their explicit HSE nodes.
convertRootExpression
  :: Ord local
  => Generated.Qualification
  -> Generated.Expression local
  -> Conversion local HsExp
convertRootExpression qualification expression =
  case Generated.expressionLambdaSpine expression of
    ([], body) -> convertInternal qualification 0 body
    (patterns, body) -> Lambda noLoc
      <$> mapM (convertPattern qualification) patterns
      <*> convertInternal qualification 0 body

convertFunctionClause
  :: Ord local
  => Generated.Qualification
  -> Generated.FunctionClause local
  -> Conversion local HsDecl
convertFunctionClause qualification (Generated.FunctionClause name patterns body) = do
  convertFunctionParts qualification (definitionName name) patterns body

convertFunctionParts
  :: Ord local
  => Generated.Qualification
  -> Name SrcSpanInfo
  -> [Generated.Pattern local]
  -> Generated.Expression local
  -> Conversion local HsDecl
convertFunctionParts qualification functionName patterns body = do
  convertedPatterns <- mapM (convertPattern qualification) patterns
  convertedBody <- convertInternal qualification 0 body
  pure $ FunBind noLoc
    [Match noLoc functionName convertedPatterns
      (UnGuardedRhs noLoc convertedBody) Nothing]

convertInternal
  :: Ord local
  => Generated.Qualification
  -> Int
  -> Generated.Expression local
  -> Conversion local HsExp
convertInternal _ _ (Generated.Local variable) =
  variableExpression <$> renderVariable variable
convertInternal qualification _ (Generated.Global name) =
  pure $ namedExpression qualification name
convertInternal qualification precedence (Generated.Lambda patterns body) = do
  convertedPatterns <- mapM (convertPattern qualification) patterns
  convertedBody <- convertInternal qualification 0 body
  pure $ parenthesize (precedence >= 1)
    $ Lambda noLoc convertedPatterns convertedBody
convertInternal qualification precedence application@Generated.Apply{} =
  gatherApplications application []
 where
  gatherApplications (Generated.Apply inner next) parameters =
    gatherApplications inner (next : parameters)
  gatherApplications (Generated.Global name) parameters =
    specialApplication name parameters
  gatherApplications functionExpression parameters =
    defaultApplication functionExpression parameters

  defaultApplication functionExpression parameters = do
    convertedFunction <- convertInternal qualification 2 functionExpression
    convertedParameters <- mapM (convertInternal qualification 3) parameters
    pure $ parenthesize (precedence >= 3)
      $ foldl (App noLoc) convertedFunction convertedParameters

  specialApplication name parameters
    | SharedName.SpecialOccurrence
        (SharedName.TupleConstructor SharedName.Boxed arity) <-
          SharedName.nameOccurrence name
    , usesSurfaceApplicationSugar qualification
    , arity == length parameters =
        Tuple noLoc Boxed <$> mapM (convertInternal qualification 0) parameters
  specialApplication name [left, right]
    | usesSurfaceApplicationSugar qualification
    , isOperator name = infixApplication name left right
  specialApplication name parameters =
    defaultApplication (Generated.Global name) parameters

  infixApplication name left right = do
    -- Generated names do not carry a fixity declaration.  Parenthesize an
    -- infix, lambda, let, or case on either side so pretty-printing and then
    -- parsing cannot silently reassociate the original expression tree.
    convertedLeft <- convertInternal qualification 2 left
    convertedRight <- convertInternal qualification 2 right
    pure $ parenthesize (precedence >= 2)
      $ InfixApp noLoc convertedLeft
          ((if isConstructor name then QConOp else QVarOp)
            noLoc $ toQName qualification name)
          convertedRight
convertInternal qualification _ (Generated.Tuple elements) =
  Tuple noLoc Boxed <$> mapM (convertInternal qualification 0) elements
convertInternal _ _ (Generated.Hole variable) =
  variableExpression . ('_' :) <$> renderVariable variable
convertInternal qualification precedence
    (Generated.Let pattern binding body) = do
  convertedPattern <- convertPattern qualification pattern
  convertedBinding <- convertInternal qualification 0 binding
  convertedBody <- convertInternal qualification 0 body
  let bindingPattern = case pattern of
        Generated.Constructor{} -> PParen noLoc convertedPattern
        _ -> convertedPattern
      declaration = PatBind noLoc bindingPattern
        (UnGuardedRhs noLoc convertedBinding) Nothing
  pure $ parenthesize (precedence >= 2)
    $ mergeLet declaration convertedBody
convertInternal qualification precedence
    (Generated.Case scrutinee alternatives) = do
  convertedScrutinee <- convertInternal qualification 0 scrutinee
  convertedAlternatives <- alternatives `forM` \(pattern, body) -> do
    convertedPattern <- convertPattern qualification pattern
    convertedBody <- convertInternal qualification 0 body
    pure $ Alt noLoc convertedPattern
      (UnGuardedRhs noLoc convertedBody) Nothing
  pure $ parenthesize (precedence >= 2)
    $ Case noLoc convertedScrutinee convertedAlternatives

convertPattern
  :: Ord local
  => Generated.Qualification
  -> Generated.Pattern local
  -> Conversion local (Pat SrcSpanInfo)
convertPattern qualification pattern = case pattern of
  Generated.Bind variable ->
    variablePattern <$> renderVariable variable
  Generated.Wildcard -> pure $ PWildCard noLoc
  Generated.Constructor constructor arguments ->
    PApp noLoc (toQName qualification constructor)
      <$> mapM (convertPattern qualification) arguments
  Generated.TuplePattern elements ->
    PTuple noLoc Boxed <$> mapM (convertPattern qualification) elements
  Generated.As variable nested -> PAsPat noLoc
    <$> (Ident noLoc <$> renderVariable variable)
    <*> convertPattern qualification nested

renderVariable :: Ord local => local -> Conversion local String
renderVariable variable = do
  names <- ask
  case Map.lookup variable names of
    Just name -> pure name
    Nothing -> lift $ Left
      $ HaskellSrcScopeError $ Generated.UnboundLocal variable

namedExpression :: Generated.Qualification -> SharedName.Name -> HsExp
namedExpression qualification name
  | isConstructor name = Con noLoc qname
  | otherwise = Var noLoc qname
 where
  qname = toQName qualification name

definitionName
  :: Generated.DefinitionName
  -> Name SrcSpanInfo
definitionName checked = case SharedName.nameOccurrence
    $ Generated.definitionName checked of
  SharedName.IdentifierOccurrence _ spelling -> Ident noLoc spelling
  SharedName.OperatorOccurrence _ spelling -> Symbol noLoc spelling
  -- 'DefinitionName' excludes every special occurrence.  Keeping this case
  -- explicit makes the structural conversion visibly total if the shared
  -- name representation gains another occurrence form.
  SharedName.SpecialOccurrence{} ->
    Ident noLoc $ Generated.definitionSpelling checked

variableExpression :: String -> HsExp
variableExpression = Var noLoc . UnQual noLoc . Ident noLoc

variablePattern :: String -> Pat SrcSpanInfo
variablePattern = PVar noLoc . Ident noLoc

toQName :: Generated.Qualification -> SharedName.Name -> QName SrcSpanInfo
toQName qualification name = case SharedName.nameOccurrence name of
  SharedName.SpecialOccurrence SharedName.ListConstructor ->
    Special noLoc (ListCon noLoc)
  SharedName.SpecialOccurrence
      (SharedName.TupleConstructor SharedName.Boxed arity) ->
    Special noLoc (TupleCon noLoc Boxed arity)
  SharedName.SpecialOccurrence SharedName.ConsConstructor ->
    Special noLoc (Cons noLoc)
  SharedName.SpecialOccurrence SharedName.FunctionConstructor ->
    Special noLoc (FunCon noLoc)
  SharedName.SpecialOccurrence
      (SharedName.TupleConstructor SharedName.Unboxed arity) ->
    Special noLoc (TupleCon noLoc Unboxed arity)
  SharedName.IdentifierOccurrence _ spelling ->
    qualify False $ Ident noLoc spelling
  SharedName.OperatorOccurrence _ spelling ->
    qualify True $ Symbol noLoc spelling
 where
  qualify symbolic syntaxName = case SharedName.nameModule name of
    Nothing -> UnQual noLoc syntaxName
    Just namespace
      | qualification == Generated.Unqualified
          || (qualification == Generated.QualifyIdentifiers && symbolic) ->
            UnQual noLoc syntaxName
      | otherwise -> Qual noLoc
          (ModuleName noLoc $ SharedName.renderModuleName namespace)
          syntaxName

isConstructor :: SharedName.Name -> Bool
isConstructor = (== SharedName.ConstructorLike) . SharedName.nameLexicalClass

isOperator :: SharedName.Name -> Bool
isOperator name = case SharedName.nameOccurrence name of
  SharedName.OperatorOccurrence{} -> True
  SharedName.SpecialOccurrence SharedName.ConsConstructor -> True
  SharedName.SpecialOccurrence SharedName.FunctionConstructor -> True
  _ -> False

usesSurfaceApplicationSugar :: Generated.Qualification -> Bool
usesSurfaceApplicationSugar qualification =
  qualification /= Generated.FullyQualified

parenthesize :: Bool -> HsExp -> HsExp
parenthesize True = Paren noLoc
parenthesize False = id

mergeLet :: HsDecl -> HsExp -> HsExp
mergeLet binding (Let _ (BDecls _ bindings) body) =
  Let noLoc (BDecls noLoc $ binding : bindings) body
mergeLet binding body = Let noLoc (BDecls noLoc [binding]) body

noLoc :: SrcSpanInfo
noLoc = noSrcSpan
