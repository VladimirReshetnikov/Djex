module Language.Haskell.Exference.ExpressionToHaskellSrc
  ( HaskellSrcConversionError (..)
  , generatedExpressionToHaskellSrc
  , generatedFunctionClauseToHaskellSrc
  , convert
  , convertChecked
  , convertToFunc
  , convertToFuncChecked
  )
where

import Control.Monad (forM)
import Control.Monad.Trans.Reader (Reader, ask, runReader)
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
type Conversion local = Reader (Map local String)

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
  pure $ runReader
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
  first HaskellSrcSyntaxError
    $ Generated.validateFunctionClauseSyntax
        (Generated.renderQualification options) clause
  names <- first HaskellSrcSyntaxError
    $ Generated.allocateClauseLocalNames options clause
  pure $ runReader
    (convertFunctionClause (Generated.renderQualification options) clause)
    names

{-# DEPRECATED convert
  "Use convertChecked, or consume the shared Generated.Expression directly." #-}
{-# DEPRECATED convertToFunc
  "Use convertToFuncChecked, or render the shared Generated.FunctionClause directly." #-}

-- Qualification level: 0 emits unqualified names, 1 qualifies ordinary
-- identifiers but keeps operators infix-friendly, and 2 qualifies everything.
-- This unchecked compatibility entry point intentionally accepts partial
-- search trees; it only retains the allocator invariant needed to emit names.
convert :: Int -> E.Expression -> HsExp
convert qualification expression =
  convertGeneratedExpressionUnchecked options generated
 where
  policy = E.qualificationFromLevel qualification
  options = expressionRenderOptions policy [] expression
  generated = E.toGeneratedExpression expression

-- | Validate the shared generated tree before producing a compatibility HSE
-- expression.  The historical 'convert' remains total for callers that use it
-- to inspect partial search trees.
convertChecked
  :: Int
  -> E.Expression
  -> Either E.ExpressionRenderError HsExp
convertChecked qualification expression = first toExpressionRenderError
  $ generatedExpressionToHaskellSrc options
  $ E.toGeneratedExpression expression
 where
  options = expressionRenderOptions
    (E.qualificationFromLevel qualification) [] expression

-- The raw-string legacy API deliberately keeps accepting names that have not
-- crossed the shared definition-name validator.
convertToFunc :: Int -> String -> E.Expression -> HsDecl
convertToFunc qualification functionName expression = runReader
  (convertFunctionParts policy
    (Ident noLoc functionName) patterns body)
  names
 where
  policy = E.qualificationFromLevel qualification
  options = expressionRenderOptions policy [functionName] expression
  generated = E.toGeneratedExpression expression
  names = allocatedNamesOrError options generated
  (patterns, body) = promoteLeadingLambdas generated

-- | Checked top-level compatibility adapter.  Unlike the raw-string legacy
-- entry point, the structural name can report invalid definitions and globals
-- that would become accidental self-references after qualification is erased.
convertToFuncChecked
  :: Int
  -> SharedName.Name
  -> E.Expression
  -> Either E.ExpressionRenderError HsDecl
convertToFuncChecked qualification functionName expression =
  first toExpressionRenderError
    $ generatedFunctionClauseToHaskellSrc options clause
 where
  policy = E.qualificationFromLevel qualification
  options = expressionRenderOptions policy [] expression
  generated = E.toGeneratedExpression expression
  (patterns, body) = promoteLeadingLambdas generated
  clause = Generated.FunctionClause functionName patterns body

expressionRenderOptions
  :: Generated.Qualification
  -> [String]
  -> E.Expression
  -> Generated.RenderOptions T.TVarId
expressionRenderOptions qualification reserved expression =
  Generated.RenderOptions qualification preferred reserved
 where
  hints = E.expressionNameHints expression
  preferred variable =
    Map.findWithDefault (T.showVar variable) variable hints

toExpressionRenderError
  :: HaskellSrcConversionError T.TVarId
  -> E.ExpressionRenderError
toExpressionRenderError conversionError = case conversionError of
  HaskellSrcScopeError scopeError -> E.ExpressionScopeError scopeError
  HaskellSrcSyntaxError syntaxError -> E.ExpressionSyntaxError syntaxError

convertGeneratedExpressionUnchecked
  :: Ord local
  => Generated.RenderOptions local
  -> Generated.Expression local
  -> HsExp
convertGeneratedExpressionUnchecked options expression = runReader
  (convertRootExpression (Generated.renderQualification options) expression)
  (allocatedNamesOrError options expression)

allocatedNamesOrError
  :: Ord local
  => Generated.RenderOptions local
  -> Generated.Expression local
  -> Map local String
allocatedNamesOrError options expression =
  case Generated.allocateLocalNames options expression of
    Right names -> names
    Left renderError -> error $ "cannot allocate generated local names: "
      ++ show renderError

-- Only the outermost lambda spine is flattened.  Nested lambdas beneath an
-- application, let, case, or lambda body retain their explicit HSE nodes.
convertRootExpression
  :: Ord local
  => Generated.Qualification
  -> Generated.Expression local
  -> Conversion local HsExp
convertRootExpression qualification expression =
  case promoteLeadingLambdas expression of
    ([], body) -> convertInternal qualification 0 body
    (patterns, body) -> Lambda noLoc
      <$> mapM (convertPattern qualification) patterns
      <*> convertInternal qualification 0 body

promoteLeadingLambdas
  :: Generated.Expression local
  -> ([Generated.Pattern local], Generated.Expression local)
promoteLeadingLambdas = collect []
 where
  collect groups (Generated.Lambda patterns body) =
    collect (patterns : groups) body
  collect groups body = (concat $ reverse groups, body)

convertFunctionClause
  :: Ord local
  => Generated.Qualification
  -> Generated.FunctionClause local
  -> Conversion local HsDecl
convertFunctionClause qualification (Generated.FunctionClause name patterns body) =
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
    convertedLeft <- convertInternal qualification 1 left
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
    Nothing -> error "generated local name was not allocated"

namedExpression :: Generated.Qualification -> SharedName.Name -> HsExp
namedExpression qualification name
  | isConstructor name = Con noLoc qname
  | otherwise = Var noLoc qname
 where
  qname = toQName qualification name

definitionName :: SharedName.Name -> Name SrcSpanInfo
definitionName name = case SharedName.nameOccurrence name of
  SharedName.IdentifierOccurrence _ spelling -> Ident noLoc spelling
  SharedName.OperatorOccurrence _ spelling -> Symbol noLoc spelling
  SharedName.SpecialOccurrence{} ->
    error "validated generated definition has a special name"

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
