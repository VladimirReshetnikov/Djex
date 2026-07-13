module Language.Haskell.Exference.ExpressionToHaskellSrc
  ( convert
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
type Conversion = Reader (Map T.TVarId String)

{-# DEPRECATED convert
  "Use convertChecked, or consume the shared Generated.Expression directly." #-}
{-# DEPRECATED convertToFunc
  "Use convertToFuncChecked, or render the shared Generated.FunctionClause directly." #-}

-- Qualification level: 0 emits unqualified names, 1 qualifies ordinary
-- identifiers but keeps operators infix-friendly, and 2 qualifies everything.
convert :: Int -> E.Expression -> HsExp
convert qualification expression = runReader
  (gatherLambdas expression [])
  (allocatedNames qualification [] expression)
  where
    gatherLambdas (E.ExpLambda variable ty body) parameters =
      gatherLambdas body ((variable, ty) : parameters)
    gatherLambdas body [] = convertExp qualification body
    gatherLambdas body parameters = do
      converted <- convertExp qualification body
      names <- mapM (renderVariable . fst) (reverse parameters)
      pure $ Lambda noLoc (map variablePattern names) converted

-- | Validate the shared generated tree before producing a compatibility HSE
-- expression.  The historical 'convert' remains total for callers that use it
-- to inspect partial search trees.
convertChecked
  :: Int
  -> E.Expression
  -> Either E.ExpressionRenderError HsExp
convertChecked qualification expression = do
  let generated = E.toGeneratedExpression expression
      policy = E.qualificationFromLevel qualification
  first E.ExpressionScopeError
    $ Generated.validateExpressionScope generated
  first E.ExpressionSyntaxError
    $ Generated.validateExpressionSyntax generated
  _ <- first E.ExpressionSyntaxError
    $ E.allocateExpressionNames policy [] expression
  pure $ convert qualification expression

convertToFunc :: Int -> String -> E.Expression -> HsDecl
convertToFunc qualification functionName = convertToFuncWithName
  qualification (Ident noLoc functionName) functionName

convertToFuncWithName
  :: Int
  -> Name SrcSpanInfo
  -> String
  -> E.Expression
  -> HsDecl
convertToFuncWithName qualification functionName reservedName expression = runReader
  (gatherLambdas expression [])
  (allocatedNames qualification [reservedName] expression)
  where
    gatherLambdas (E.ExpLambda variable ty body) parameters =
      gatherLambdas body ((variable, ty) : parameters)
    gatherLambdas body parameters = do
      converted <- convertExp qualification body
      names <- mapM (renderVariable . fst) (reverse parameters)
      pure $ FunBind noLoc
        [Match noLoc functionName (map variablePattern names)
          (UnGuardedRhs noLoc converted) Nothing]

-- | Checked top-level compatibility adapter.  Unlike the raw-string legacy
-- entry point, the structural name can report invalid definitions and globals
-- that would become accidental self-references after qualification is erased.
convertToFuncChecked
  :: Int
  -> SharedName.Name
  -> E.Expression
  -> Either E.ExpressionRenderError HsDecl
convertToFuncChecked qualification functionName expression = do
  let generated = E.toGeneratedExpression expression
      policy = E.qualificationFromLevel qualification
      hints = E.expressionNameHints expression
      options = Generated.RenderOptions policy preferred []
      clause = Generated.FunctionClause functionName [] generated
      preferred variable = Map.findWithDefault (T.showVar variable) variable hints
  first E.ExpressionScopeError
    $ Generated.validateFunctionClauseScope clause
  _ <- first E.ExpressionSyntaxError
    $ Generated.renderFunctionClause options clause
  case SharedName.nameOccurrence functionName of
    SharedName.IdentifierOccurrence _ spelling -> pure
      $ convertToFuncWithName qualification
          (Ident noLoc spelling) spelling expression
    SharedName.OperatorOccurrence _ spelling -> pure
      $ convertToFuncWithName qualification
          (Symbol noLoc spelling) spelling expression
    SharedName.SpecialOccurrence{} -> Left $ E.ExpressionSyntaxError
      $ Generated.InvalidFunctionName functionName

convertExp :: Int -> E.Expression -> Conversion HsExp
convertExp qualification = convertInternal qualification 0

convertInternal :: Int -> Int -> E.Expression -> Conversion HsExp
convertInternal _ _ (E.ExpVar variable _) =
  variableExpression <$> renderVariable variable
convertInternal qualification _ (E.ExpName name) =
  pure $ namedExpression qualification name
convertInternal qualification precedence (E.ExpLambda variable _ body) = do
  converted <- convertInternal qualification 0 body
  name <- renderVariable variable
  pure $ parenthesize (precedence >= 1)
    $ Lambda noLoc [variablePattern name] converted
convertInternal qualification precedence (E.ExpApply function parameter) =
  gatherApplications function [parameter]
  where
    gatherApplications (E.ExpApply inner next) parameters =
      gatherApplications inner (next : parameters)
    gatherApplications named@(E.ExpName name) parameters =
      specialApplication named name parameters
    gatherApplications other parameters = defaultApplication other parameters

    defaultApplication functionExpression parameters = do
      convertedFunction <- convertInternal qualification 2 functionExpression
      convertedParameters <- mapM (convertInternal qualification 3) parameters
      pure $ parenthesize (precedence >= 3)
        $ foldl (App noLoc) convertedFunction convertedParameters

    specialApplication _ name parameters
      | SharedName.SpecialOccurrence
          (SharedName.TupleConstructor SharedName.Boxed arity) <-
            T.qualifiedNameOccurrence name
      , qualification < 2
      , arity == length parameters =
          Tuple noLoc Boxed <$> mapM (convertInternal qualification 0) parameters
    specialApplication _ name [left, right]
      | SharedName.SpecialOccurrence SharedName.ConsConstructor <-
          T.qualifiedNameOccurrence name
      , qualification < 2 = infixApplication name left right
    specialApplication _ name [left, right]
      | qualification < 2 && isOperator name = infixApplication name left right
    specialApplication original _ parameters = defaultApplication original parameters

    infixApplication name left right = do
      convertedLeft <- convertInternal qualification 1 left
      convertedRight <- convertInternal qualification 2 right
      pure $ parenthesize (precedence >= 2)
        $ InfixApp noLoc convertedLeft
            ((if isConstructor name then QConOp else QVarOp)
              noLoc $ toQName qualification name)
            convertedRight
convertInternal _ _ (E.ExpHole variable) =
  variableExpression . ('_' :) <$> renderVariable variable
convertInternal qualification precedence (E.ExpLet variable _ binding body) = do
  convertedBinding <- convertInternal qualification 0 binding
  name <- renderVariable variable
  convertedBody <- convertInternal qualification 0 body
  pure $ parenthesize (precedence >= 2)
    $ mergeLet
        (PatBind noLoc (variablePattern name)
          (UnGuardedRhs noLoc convertedBinding) Nothing)
        convertedBody
convertInternal qualification precedence (E.ExpLetMatch constructor variables binding body) = do
  convertedBinding <- convertInternal qualification 0 binding
  names <- mapM (renderVariable . fst) variables
  convertedBody <- convertInternal qualification 0 body
  let patternBinding = PatBind noLoc
        (PParen noLoc $ constructorPattern qualification constructor names)
        (UnGuardedRhs noLoc convertedBinding)
        Nothing
  pure $ parenthesize (precedence >= 2) $ mergeLet patternBinding convertedBody
convertInternal qualification precedence (E.ExpCaseMatch scrutinee alternatives) = do
  convertedScrutinee <- convertInternal qualification 0 scrutinee
  convertedAlternatives <- alternatives `forM` \(constructor, variables, body) -> do
    convertedBody <- convertInternal qualification 0 body
    names <- mapM (renderVariable . fst) variables
    pure $ Alt noLoc
      (constructorPattern qualification constructor names)
      (UnGuardedRhs noLoc convertedBody)
      Nothing
  pure $ parenthesize (precedence >= 2)
    $ Case noLoc convertedScrutinee convertedAlternatives

renderVariable :: T.TVarId -> Conversion String
renderVariable variable = do
  names <- ask
  pure $ Map.findWithDefault (T.showVar variable) variable names

allocatedNames :: Int -> [String] -> E.Expression -> Map T.TVarId String
allocatedNames qualification reserved expression =
  case E.allocateExpressionNames
      (E.qualificationFromLevel qualification) reserved expression of
    Right names -> names
    -- Type-derived preferences are constructed as legal variable identifiers,
    -- so failure here denotes an internal invariant violation in the legacy
    -- pure HSE compatibility API.
    Left renderError -> error $ "cannot allocate generated local names: "
      ++ show renderError

namedExpression :: Int -> T.QualifiedName -> HsExp
namedExpression qualification name
  | isConstructor name = Con noLoc qname
  | otherwise = Var noLoc qname
  where
    qname = toQName qualification name

variableExpression :: String -> HsExp
variableExpression = Var noLoc . UnQual noLoc . Ident noLoc

variablePattern :: String -> Pat SrcSpanInfo
variablePattern = PVar noLoc . Ident noLoc

constructorPattern :: Int -> T.QualifiedName -> [String] -> Pat SrcSpanInfo
constructorPattern qualification constructor names =
  PApp noLoc (toQName qualification constructor) (map variablePattern names)

toQName :: Int -> T.QualifiedName -> QName SrcSpanInfo
toQName qualification qualifiedName = case T.qualifiedNameOccurrence qualifiedName of
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
    qualify $ Ident noLoc spelling
  SharedName.OperatorOccurrence _ spelling ->
    qualify $ Symbol noLoc spelling
  where
    qualify syntaxName = case T.qualifiedNameModule qualifiedName of
      Nothing -> UnQual noLoc syntaxName
      Just namespace ->
        if qualification <= 0
          || (qualification == 1 && isOperatorName syntaxName)
        then UnQual noLoc syntaxName
        else Qual noLoc
          (ModuleName noLoc $ SharedName.renderModuleName namespace)
          syntaxName

isConstructor :: T.QualifiedName -> Bool
isConstructor = (== SharedName.ConstructorLike)
  . SharedName.occurrenceLexicalClass
  . T.qualifiedNameOccurrence

isOperator :: T.QualifiedName -> Bool
isOperator name = case T.qualifiedNameOccurrence name of
  SharedName.OperatorOccurrence _ _ -> True
  SharedName.SpecialOccurrence SharedName.ConsConstructor -> True
  SharedName.SpecialOccurrence SharedName.FunctionConstructor -> True
  _ -> False

isOperatorName :: Name l -> Bool
isOperatorName Symbol{} = True
isOperatorName _ = False

parenthesize :: Bool -> HsExp -> HsExp
parenthesize True = Paren noLoc
parenthesize False = id

mergeLet :: HsDecl -> HsExp -> HsExp
mergeLet binding (Let _ (BDecls _ bindings) body) =
  Let noLoc (BDecls noLoc $ binding : bindings) body
mergeLet binding body = Let noLoc (BDecls noLoc [binding]) body

noLoc :: SrcSpanInfo
noLoc = noSrcSpan
