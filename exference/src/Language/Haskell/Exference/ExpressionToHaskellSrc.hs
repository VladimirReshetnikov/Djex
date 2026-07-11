{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Haskell.Exference.ExpressionToHaskellSrc
  ( convert
  , convertToFunc
  )
where

import Control.Monad (forM)
import Control.Monad.Trans.MultiState
import Data.Char (isUpper)
import Data.Functor.Identity (runIdentity)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo, noSrcSpan)
import Language.Haskell.Exts.Syntax

import qualified Language.Haskell.Exference.Core.Expression as E
import qualified Language.Haskell.Exference.Core.Types as T

type HsExp = Exp SrcSpanInfo
type HsDecl = Decl SrcSpanInfo
type Conversion = MultiState '[Map T.TVarId T.HsType]

-- Qualification level: 0 emits unqualified names, 1 qualifies ordinary
-- identifiers but keeps operators infix-friendly, and 2 qualifies everything.
convert :: Int -> E.Expression -> HsExp
convert qualification expression = runIdentity
  $ runMultiStateTNil
  $ withMultiStateA (Map.empty :: Map T.TVarId T.HsType)
  $ do
      E.collectVarTypes expression
      gatherLambdas expression []
  where
    gatherLambdas (E.ExpLambda variable ty body) parameters =
      gatherLambdas body ((variable, ty) : parameters)
    gatherLambdas body [] = convertExp qualification body
    gatherLambdas body parameters = do
      converted <- convertExp qualification body
      names <- mapM (T.showTypedVar . fst) (reverse parameters)
      pure $ Lambda noLoc (map variablePattern names) converted

convertToFunc :: Int -> String -> E.Expression -> HsDecl
convertToFunc qualification functionName expression = runIdentity
  $ runMultiStateTNil
  $ withMultiStateA (Map.empty :: Map T.TVarId T.HsType)
  $ do
      E.collectVarTypes expression
      gatherLambdas expression []
  where
    gatherLambdas (E.ExpLambda variable ty body) parameters =
      gatherLambdas body ((variable, ty) : parameters)
    gatherLambdas body parameters = do
      converted <- convertExp qualification body
      names <- mapM (T.showTypedVar . fst) (reverse parameters)
      pure $ FunBind noLoc
        [Match noLoc (Ident noLoc functionName) (map variablePattern names)
          (UnGuardedRhs noLoc converted) Nothing]

convertExp :: Int -> E.Expression -> Conversion HsExp
convertExp qualification = convertInternal qualification 0

convertInternal :: Int -> Int -> E.Expression -> Conversion HsExp
convertInternal _ _ (E.ExpVar variable _) =
  variableExpression <$> T.showTypedVar variable
convertInternal qualification _ (E.ExpName name) =
  pure $ namedExpression qualification name
convertInternal qualification precedence (E.ExpLambda variable _ body) = do
  converted <- convertInternal qualification 0 body
  name <- T.showTypedVar variable
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

    specialApplication _ (T.TupleCon arity) parameters
      | qualification < 2 && arity == length parameters =
          Tuple noLoc Boxed <$> mapM (convertInternal qualification 0) parameters
    specialApplication _ T.Cons [left, right]
      | qualification < 2 = infixApplication T.Cons left right
    specialApplication _ name [left, right]
      | qualification < 2 && isOperator name = infixApplication name left right
    specialApplication original _ parameters = defaultApplication original parameters

    infixApplication name left right = do
      convertedLeft <- convertInternal qualification 1 left
      convertedRight <- convertInternal qualification 2 right
      pure $ parenthesize (precedence >= 2)
        $ InfixApp noLoc convertedLeft
            (QVarOp noLoc $ toQName qualification name)
            convertedRight
convertInternal _ _ (E.ExpHole variable) =
  pure $ variableExpression ('_' : T.showVar variable)
convertInternal qualification precedence (E.ExpLet variable _ binding body) = do
  convertedBinding <- convertInternal qualification 0 binding
  name <- T.showTypedVar variable
  convertedBody <- convertInternal qualification 0 body
  pure $ parenthesize (precedence >= 2)
    $ mergeLet
        (PatBind noLoc (variablePattern name)
          (UnGuardedRhs noLoc convertedBinding) Nothing)
        convertedBody
convertInternal qualification precedence (E.ExpLetMatch constructor variables binding body) = do
  convertedBinding <- convertInternal qualification 0 binding
  names <- mapM (T.showTypedVar . fst) variables
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
    names <- mapM (T.showTypedVar . fst) variables
    pure $ Alt noLoc
      (constructorPattern qualification constructor names)
      (UnGuardedRhs noLoc convertedBody)
      Nothing
  pure $ parenthesize (precedence >= 2)
    $ Case noLoc convertedScrutinee convertedAlternatives

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
toQName _ T.ListCon = Special noLoc (ListCon noLoc)
toQName _ (T.TupleCon arity) = Special noLoc (TupleCon noLoc Boxed arity)
toQName _ T.Cons = Special noLoc (Cons noLoc)
toQName qualification (T.QualifiedName modules rawName) =
  qualify modules parsedName
  where
    parsedName = maybe (Ident noLoc rawName) (Symbol noLoc) (operatorText rawName)
    qualify [] name = UnQual noLoc name
    qualify namespace name
      | qualification == 0 = UnQual noLoc name
      | qualification == 1 && isOperatorName name = UnQual noLoc name
      | otherwise = Qual noLoc
          (ModuleName noLoc $ intercalate "." namespace)
          name

isConstructor :: T.QualifiedName -> Bool
isConstructor T.ListCon = True
isConstructor T.TupleCon{} = True
isConstructor T.Cons = True
isConstructor (T.QualifiedName _ name) = case operatorText name of
  Just (':' : _) -> True
  Just _ -> False
  Nothing -> maybe False isUpper (safeHead name)

isOperator :: T.QualifiedName -> Bool
isOperator T.Cons = True
isOperator (T.QualifiedName _ name) = maybe False (const True) (operatorText name)
isOperator _ = False

isOperatorName :: Name l -> Bool
isOperatorName Symbol{} = True
isOperatorName _ = False

operatorText :: String -> Maybe String
operatorText ('(' : rest) = case reverse rest of
  ')' : reversedOperator -> Just (reverse reversedOperator)
  _ -> Nothing
operatorText _ = Nothing

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (value : _) = Just value

parenthesize :: Bool -> HsExp -> HsExp
parenthesize True = Paren noLoc
parenthesize False = id

mergeLet :: HsDecl -> HsExp -> HsExp
mergeLet binding (Let _ (BDecls _ bindings) body) =
  Let noLoc (BDecls noLoc $ binding : bindings) body
mergeLet binding body = Let noLoc (BDecls noLoc [binding]) body

noLoc :: SrcSpanInfo
noLoc = noSrcSpan
