{-# LANGUAGE DeriveGeneric #-}

module Language.Haskell.Exference.Core.Expression
  ( Expression (..)
  , showExpression
  , fillExprHole
  , collectVarTypes
  , allocateVariableNames
  )
where



import Language.Haskell.Exference.Core.Types
import Data.List ( intercalate )
import qualified Data.List as L
import Data.Maybe ( maybeToList )
import Control.Monad ( forM_ )

import Control.DeepSeq
import GHC.Generics

import Control.Monad.Trans.MultiRWS

-- import Debug.Hood.Observe
import Data.Map ( Map )
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.Haskell.Synthesis.Name as SharedName



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

-- | Allocate one stable Haskell binder spelling per variable identity.
--
-- Type-derived names are preferences, not identities: two different IDs can
-- both suggest @t6@, and an emitted global may already use the same spelling.
-- The caller describes which global names will be emitted unqualified under
-- its rendering policy and may reserve additional names (for example, a
-- generated top-level function).  Apostrophes preserve readability while
-- remaining legal identifier continuations.
allocateVariableNames
  :: (QualifiedName -> Maybe String)
  -> S.Set String
  -> Expression
  -> M.Map TVarId String
allocateVariableNames emittedIdentifier initiallyReserved expression = names
 where
  observations = variableObservations expression
  variableTypes = L.foldl' recordType M.empty observations
  reservedGlobals = S.fromList
    [ spelling
    | global <- expressionGlobals expression
    , spelling <- maybeToList $ emittedIdentifier global
    ]
  (names, _) = L.foldl' allocate
    (M.empty, initiallyReserved `S.union` reservedGlobals)
    $ map fst observations

  recordType types (variable, ty) = M.alter update variable types
    where
      update Nothing = Just ty
      update (Just TypeVar{}) = Just ty
      update (Just TypeConstant{}) = Just ty
      update existing = existing

  allocate state@(allocated, used) variable
    | M.member variable allocated = state
    | otherwise =
        (M.insert variable chosen allocated, S.insert chosen used)
    where
      preferred = maybe (showVar variable) (preferredVarName variable)
        $ M.lookup variable variableTypes
      chosen = freshName used preferred

  freshName used candidate
    | candidate `S.member` used = freshName used $ candidate ++ "'"
    | otherwise = candidate

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
expressionGlobals ExpVar{} = []
expressionGlobals (ExpName name) = [name]
expressionGlobals (ExpLambda _ _ body) = expressionGlobals body
expressionGlobals (ExpApply function argument) =
  expressionGlobals function ++ expressionGlobals argument
expressionGlobals ExpHole{} = []
expressionGlobals (ExpLetMatch constructor _ binding body) =
  constructor : expressionGlobals binding ++ expressionGlobals body
expressionGlobals (ExpLet _ _ binding body) =
  expressionGlobals binding ++ expressionGlobals body
expressionGlobals (ExpCaseMatch scrutinee alternatives) =
  expressionGlobals scrutinee
  ++ concat
    [ constructor : expressionGlobals body
    | (constructor, _, body) <- alternatives
    ]

showExpression :: Expression -> String
showExpression expression = h 0 expression ""
 where
  variableNames = allocateVariableNames textualIdentifier S.empty expression
  variableName variable = M.findWithDefault (showVar variable) variable variableNames
  textualIdentifier name = case qualifiedNameModule name of
    Nothing -> SharedName.nameIdentifier $ toSynthesisName name
    Just _ -> Nothing

  h :: Int -> Expression -> ShowS
  h _ (ExpVar i _) = showString $ variableName i
  h _ (ExpName s)  = shows s
  h d (ExpLambda i _ e1) =
    showParen (d>0) $ showString ("\\" ++ variableName i ++ " -> ") . h 1 e1
  h d (ExpApply e1 e2) =
    showParen (d>1) $ h 2 e1 . showString " " . h 3 e2
  h _ (ExpHole i) = showString $ "_" ++ showVar i
  h d (ExpLetMatch n vars bindExp inExp) =
    showParen (d>2)
    $ showString ("let ("
                  ++ show n
                  ++" "
                  ++ intercalate " " (map (variableName . fst) vars)
                  ++ ") = ")
    . h 0 bindExp . showString " in " . h 0 inExp
  h d (ExpLet i _ bindExp inExp) =
    showParen (d>2)
    $ showString ("let " ++ variableName i ++ " = ")
    . h 3 bindExp
    . showString " in "
    . h 0 inExp
  h d (ExpCaseMatch bindExp alts) =
    showParen (d>2)
    $ showString "case "
    . h 3 bindExp
    . showString " of { "
    . showString (intercalate "; " $ map showAlternative alts)
    . showString " }"
    where
      showAlternative (constructor, variables, body) =
        show constructor ++ " "
        ++ intercalate " " (map (variableName . fst) variables)
        ++ " -> " ++ h 3 body ""

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
