{-# LANGUAGE DeriveGeneric #-}

module Language.Haskell.Exference.Core.Expression
  ( Expression (..)
  , ExpressionRenderError (..)
  , toGeneratedExpression
  , renderExpression
  , qualificationFromLevel
  , showExpression
  , fillExprHole
  , collectVarTypes
  , allocateExpressionNames
  , allocateVariableNames
  )
where



import Language.Haskell.Exference.Core.Types
import qualified Data.List as L
import Control.Monad ( forM_ )

import Control.DeepSeq
import GHC.Generics

import Control.Monad.Trans.MultiRWS

-- import Debug.Hood.Observe
import Data.Map ( Map )
import qualified Data.Map as M
import qualified Data.Set as S
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

-- instance Show Expression where
--   showsPrec _ (ExpVar i) = showString $ showVar i
--   showsPrec d (ExpName s) = showsPrec d s
--   showsPrec d (ExpLambda i e) =
--     showParen (d>0) $ showString ("\\" ++ showVar i ++ " -> ") . showsPrec 1 e
--   showsPrec d (ExpApply e1 e2) =
--     showParen (d>1) $ showsPrec 2 e1 . showString " " . showsPrec 3 e2
--   showsPrec _ (ExpHole i) = showString $ "_" ++ showVar i
--   showsPrec d (ExpLetMatch n vars bindExp inExp) =
--       showParen (d>2)
--     $ showString ("let ("++show n++" "++intercalate " " (map showVar vars) ++ ") = ")
--     . shows bindExp . showString " in " . showsPrec 0 inExp
--   showsPrec d (ExpLet i bindExp inExp) =
--       showParen (d>2)
--     $ showString ("let " ++ showVar i ++ " = ")
--     . showsPrec 3 bindExp
--     . showString " in "
--     . showsPrec 0 inExp
--   showsPrec d (ExpCaseMatch bindExp alts) =
--       showParen (d>2)
--     $ showString ("case ")
--     . showsPrec 3 bindExp
--     . showString " of { "
--     . ( \s -> intercalate "; "
--            (map (\(cons, vars, expr) ->
--               show cons++" "++intercalate " " (map showVar vars)++" -> "
--               ++showsPrec 3 expr "")
--             alts)
--          ++ s
--       )
--     . showString " }"

refreshVarTypeBinding :: forall m
                       . MonadMultiState (Map TVarId HsType) m
                      => TVarId
                      -> HsType
                      -> m ()
refreshVarTypeBinding i ty = do
  m <- mGet
  case M.lookup i m of
    Nothing             -> mSet $ M.insert i ty m
    Just TypeVar{}      -> mSet $ M.insert i ty m
    Just TypeConstant{} -> mSet $ M.insert i ty m
    _                   -> return ()

collectVarTypes :: forall m
                 . MonadMultiState (Map TVarId HsType) m
                => Expression
                -> m ()
collectVarTypes (ExpVar i ty)       = refreshVarTypeBinding i ty
collectVarTypes ExpName{}           = return ()
collectVarTypes (ExpLambda i ty se) = do
  refreshVarTypeBinding i ty
  collectVarTypes se
collectVarTypes (ExpApply e1 e2)    = do
  collectVarTypes e1
  collectVarTypes e2
collectVarTypes ExpHole{}           = return ()
collectVarTypes (ExpLetMatch _ vars e1 e2) = do
  vars `forM_` uncurry refreshVarTypeBinding
  collectVarTypes e1
  collectVarTypes e2
collectVarTypes (ExpLet i ty e1 e2) = do
  refreshVarTypeBinding i ty
  collectVarTypes e1
  collectVarTypes e2
collectVarTypes (ExpCaseMatch se matches) = do
  collectVarTypes se
  matches `forM_` \(_, vars, me) -> do
    vars `forM_` uncurry refreshVarTypeBinding
    collectVarTypes me

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
  ExpName name -> Generated.Global $ toSynthesisName name
  ExpLambda variable _ body ->
    Generated.Lambda [Generated.Bind variable]
      $ toGeneratedExpression body
  ExpApply function argument -> Generated.Apply
    (toGeneratedExpression function)
    (toGeneratedExpression argument)
  ExpHole variable -> Generated.Hole variable
  ExpLetMatch constructor variables binding body -> Generated.Let
    (Generated.Constructor (toSynthesisName constructor)
      $ map (Generated.Bind . fst) variables)
    (toGeneratedExpression binding)
    (toGeneratedExpression body)
  ExpLet variable _ binding body -> Generated.Let
    (Generated.Bind variable)
    (toGeneratedExpression binding)
    (toGeneratedExpression body)
  ExpCaseMatch scrutinee alternatives -> Generated.Case
    (toGeneratedExpression scrutinee)
    [ ( Generated.Constructor (toSynthesisName constructor)
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

-- | Compatibility adapter for callers that describe their own qualification
-- policy. Allocation itself still belongs exclusively to the shared renderer;
-- the callback merely contributes emitted globals to its reservation set.
allocateVariableNames
  :: (QualifiedName -> Maybe String)
  -> S.Set String
  -> Expression
  -> M.Map TVarId String
allocateVariableNames emittedIdentifier reserved expression =
  case allocateExpressionNames Generated.FullyQualified reservations expression of
    Right names -> names
    Left renderError -> error $ "cannot allocate generated local names: "
      ++ show renderError
 where
  reservations = S.toList $ reserved `S.union` S.fromList
    [ spelling
    | global <- expressionGlobals expression
    , Just spelling <- [emittedIdentifier global]
    ]

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
renderOptions qualification reserved expression = Generated.RenderOptions
  qualification preferred reserved
 where
  variableTypes = L.foldl' recordType M.empty $ variableObservations expression
  preferred variable = maybe (showVar variable) (preferredVarName variable)
    $ M.lookup variable variableTypes

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

expressionGlobals :: Expression -> [QualifiedName]
expressionGlobals expression = case expression of
  ExpVar{} -> []
  ExpName name -> [name]
  ExpLambda _ _ body -> expressionGlobals body
  ExpApply function argument ->
    expressionGlobals function ++ expressionGlobals argument
  ExpHole{} -> []
  ExpLetMatch constructor _ binding body ->
    constructor : expressionGlobals binding ++ expressionGlobals body
  ExpLet _ _ binding body ->
    expressionGlobals binding ++ expressionGlobals body
  ExpCaseMatch scrutinee alternatives ->
    expressionGlobals scrutinee ++ concat
      [ constructor : expressionGlobals body
      | (constructor, _, body) <- alternatives
      ]

-- instance Observable Expression where
--   observer x = observeOpaque (show x) x

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
