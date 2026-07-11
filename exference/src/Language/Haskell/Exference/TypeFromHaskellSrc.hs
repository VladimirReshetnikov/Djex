{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}

module Language.Haskell.Exference.TypeFromHaskellSrc
  ( ConvData(..)
  , convertTypeNoDecl
  , convertTypeNoDeclInternal
  , convertName
  , convertQName
  , convertModuleName
  , getVar
  -- , ConversionMonad
  , parseQualifiedName
  , tyVarTransform
  , haskellSrcExtsParseMode
  , findInvalidNames
  )
where



import Language.Haskell.Exts.Syntax
import qualified Language.Haskell.Exts.Parser as P
import Language.Haskell.Exts.Pretty ( prettyPrint )
import Language.Haskell.Exts.SrcLoc ( SrcSpanInfo )

import qualified Language.Haskell.Exference.Core.Types as T
import qualified Language.Haskell.Exference.Core.TypeUtils as TU
import Language.Haskell.Exference.Diagnostic
import Language.Haskell.Exference.HaskellSrcUtils
import qualified Data.Map as M

import Data.Maybe ( fromMaybe )
import Data.List ( find )

import Control.Monad.Trans.Except

import Data.List.Split ( wordsBy )
import Control.Monad.Trans.MultiRWS

import Language.Haskell.Exts.Extension ( Language (..)
                                       , Extension (..)
                                       , KnownExtension (..) )




-- type ConversionMonad = EitherT String (State (Int, ConvMap))

data ConvData = ConvData Int T.TypeVarIndex

haskellSrcExtsParseMode :: String -> P.ParseMode
haskellSrcExtsParseMode s = P.ParseMode (s++".hs")
                                      Haskell2010
                                      exts2
                                      False
                                      False
                                      Nothing
                                      False
  where
    exts1 = [ TypeOperators
            , ExplicitForAll
            , ExistentialQuantification
            , TypeFamilies
            , FunctionalDependencies
            , FlexibleContexts
            , MultiParamTypeClasses ]
    exts2 = map EnableExtension exts1

convertTypeNoDecl
  :: Monad m
  => [T.HsTypeClass]
  -> Maybe (ModuleName SrcSpanInfo)
  -> [T.QualifiedName]
  -> Type SrcSpanInfo
  -> ExceptT
       String
       (MultiRWST r w s m)
       (T.HsType, T.TypeVarIndex)
convertTypeNoDecl tcs mn ds t =
  mapExceptT conv $ convertTypeNoDeclInternal tcs mn ds t
 where
  conv m = [ [ (r, index)
             | r <- eith
             ]
           | (eith, ConvData _ index) <- withMultiStateAS (ConvData 0 M.empty) m
           ]

convertTypeNoDeclInternal
  :: (MonadMultiState ConvData m)
  => [T.HsTypeClass]
  -> Maybe (ModuleName SrcSpanInfo) -- default (for unqualified stuff)
                      -- Nothing uses a broad search for lookups
  -> [T.QualifiedName] -- list of fully qualified data types
                                         -- (to keep things unique)
  -> Type SrcSpanInfo
  -> ExceptT String m T.HsType
convertTypeNoDeclInternal tcs defModuleName ds ty = helper ty
 where
  helper (TyFun _ a b)      = T.TypeArrow
                              <$> helper a
                              <*> helper b
  helper tuple@(TyTuple _ Boxed ts)
    | length ts >= 2 = foldl T.TypeApp (T.TypeCons $ T.TupleCon (length ts))
        <$> mapM helper ts
    | otherwise = throwE $ "invalid boxed tuple arity " ++ show (length ts)
        ++ " in " ++ prettyPrint tuple
  helper tuple@(TyTuple _ Unboxed _)
                            = throwE $ "unsupported unboxed tuple type: "
                              ++ prettyPrint tuple
  helper (TyApp _ a b)      = T.TypeApp
                              <$> helper a
                              <*> helper b
  helper (TyVar _ vname)    = do
                              i <- getVar vname
                              return $ T.TypeVar i
  helper (TyCon _ name)     = T.TypeCons
                          <$> either throwE pure
                                (convertQName defModuleName ds name)
  helper (TyList _ t)       = T.TypeApp (T.TypeCons T.ListCon) <$> helper t
  helper (TyParen _ t)      = helper t
  helper TyInfix{}        = throwE "infix operator"
  helper TyKind{}         = throwE "kind annotation"
  helper TyPromoted{}     = throwE "promoted type"
  helper (TyForall _ maybeTVars context t) =
    T.TypeForall
      <$> case maybeTVars of
            Nothing -> return []
            Just tvs -> tyVarTransform `mapM` tvs
      <*> convertConstraint tcs defModuleName ds `mapM` contextConstraints context
      <*> helper t
  helper x                = throwE $ "unknown type element: " ++ show x -- TODO

getVar :: MonadMultiState ConvData m => Name SrcSpanInfo -> m Int
getVar n = do
  ConvData next m <- mGet
  let key = prettyPrint n
  case M.lookup key m of
    Nothing -> do
      mSet $ ConvData (next+1) (M.insert key next m)
      return next
    Just i ->
      return i

-- defaultModule -> potentially-qualified-name-thingy -> exference-q-name
--
-- Unboxed tuples deliberately have no core representation.  Returning an
-- error here prevents them from being confused with boxed tuples at every
-- elaboration site, including constraints and instance heads.
convertQName
  :: Maybe (ModuleName SrcSpanInfo)
  -> [T.QualifiedName]
  -> QName SrcSpanInfo
  -> Either String T.QualifiedName
convertQName _ _ (Special _ (UnitCon _)) = Right $ T.TupleCon 0
convertQName _ _ (Special _ (ListCon _)) = Right T.ListCon
convertQName _ _ (Special _ (FunCon _)) = Right $ T.QualifiedName [] "->"
convertQName _ _ (Special _ special@(TupleCon _ Unboxed _)) = Left
  $ "unsupported unboxed tuple constructor: " ++ prettyPrint special
convertQName _ _ (Special _ special@(TupleCon _ Boxed arity))
  | arity >= 2 = Right $ T.TupleCon arity
  | otherwise = Left $ "invalid boxed tuple constructor arity " ++ show arity
      ++ ": " ++ prettyPrint special
convertQName _ _ (Special _ (Cons _)) = Right T.Cons
convertQName _ _ (Special _ special@(UnboxedSingleCon _)) = Left
  $ "unsupported unboxed single constructor: " ++ prettyPrint special
convertQName _ _ (Special _ special@(ExprHole _)) = Left
  $ "unsupported special constructor: " ++ prettyPrint special
convertQName _ _ (Qual _ mn s) = Right $ convertModuleName mn s
convertQName (Just d) _ (UnQual _ s) = Right $ convertModuleName d s
convertQName Nothing ds (UnQual _ (Ident _ s)) = Right
  $ fromMaybe (T.QualifiedName [] s) $ find p ds
 where
  p (T.QualifiedName _ x) = x==s
  p _ = False
convertQName Nothing _ (UnQual _ s) = Right $ convertName s

convertName :: Name SrcSpanInfo -> T.QualifiedName
convertName (Ident _ s)  = T.QualifiedName [] s
convertName (Symbol _ s) = T.QualifiedName [] s

convertModuleName :: ModuleName SrcSpanInfo -> Name SrcSpanInfo -> T.QualifiedName
convertModuleName (ModuleName _ moduleName) name =
  T.QualifiedName (wordsBy (== '.') moduleName) $ case name of
    Ident _ value -> value
    Symbol _ value -> value

-- | Parse the external spelling used by rating files.  Operators use the
-- conventional @Module.(<*>)@ form, but their core payload is bare.  Built-in
-- constructors are recovered as their structural 'T.QualifiedName' variants
-- so rating lookup does not depend on rendered-text coincidences.
parseQualifiedName :: String -> Either Diagnostic T.QualifiedName
parseQualifiedName input
  | null input = invalid "qualified name is empty"
  | input == "[]" = pure T.ListCon
  | input == "()" = pure $ T.TupleCon 0
  | input == "(:)" = pure T.Cons
  | Just arity <- tupleArity input = pure $ T.TupleCon arity
  | otherwise = case break (== '(') input of
      (ordinary, "") -> fromParts $ splitDots ordinary
      (prefix, parenthesizedOperator)
        | Just operator <- validOperator parenthesizedOperator -> do
            modules <- parseOperatorModules prefix
            pure $ T.QualifiedName modules operator
        | otherwise -> invalid "malformed parenthesized operator"
  where
    fromParts :: [String] -> Either Diagnostic T.QualifiedName
    fromParts parts = case parts of
      [] -> invalid "qualified name is empty"
      _ | any null parts -> invalid "qualified name contains an empty segment"
      _ -> pure $ T.QualifiedName (init parts) (last parts)

    parseModules :: String -> Either Diagnostic [String]
    parseModules "" = pure []
    parseModules value = case splitDots value of
      parts | any null parts -> invalid "module name contains an empty segment"
            | otherwise -> pure parts

    parseOperatorModules "" = pure []
    parseOperatorModules value = case reverse value of
      '.' : rest -> parseModules $ reverse rest
      _ -> invalid "qualified operator is missing the dot before its parentheses"

    validOperator value = case value of
      '(' : rest -> case reverse rest of
        ')' : reversedOperator
          | let operator = reverse reversedOperator
          , T.qualifiedNameOperator (T.QualifiedName [] operator)
              == Just operator -> Just operator
        _ -> Nothing
      _ -> Nothing

    tupleArity value = case value of
      '(' : rest -> case reverse rest of
        ')' : reversedCommas
          | let commas = reverse reversedCommas
          , not (null commas)
          , all (== ',') commas -> Just $ length commas + 1
        _ -> Nothing
      _ -> Nothing

    splitDots = foldr splitCharacter [[]]
      where
        splitCharacter '.' parts = [] : parts
        splitCharacter character (part : parts) = (character : part) : parts
        splitCharacter _ [] = [] -- unreachable for the non-empty seed

    invalid :: String -> Either Diagnostic a
    invalid message = Left $ diagnostic
      $ "invalid qualified name " ++ show input ++ ": " ++ message

convertConstraint
  :: (MonadMultiState ConvData m)
  => [T.HsTypeClass]
  -> Maybe (ModuleName SrcSpanInfo)
  -> [T.QualifiedName]
  -> Asst SrcSpanInfo
  -> ExceptT String m T.HsConstraint
convertConstraint tcs defModuleName ds (TypeA _ classType) = do
  (qname, types) <- maybe
    (throwE $ "invalid class constraint: " ++ prettyPrint classType)
    pure
    (splitClassApplication classType)
  name <- either throwE pure $ convertQName defModuleName ds qname
  let typeClass = fromMaybe (TU.unknownTypeClass name)
        $ find ((== name) . T.tclass_name) tcs
  T.HsConstraint typeClass
    <$> mapM (convertTypeNoDeclInternal tcs defModuleName ds) types
convertConstraint env defModuleName ds (ParenA _ c)
  = convertConstraint env defModuleName ds c
convertConstraint _ _ _ c
  = throwE $ "bad constraint: " ++ show c

tyVarTransform :: MonadMultiState ConvData m
               => TyVarBind SrcSpanInfo
               -> ExceptT String m T.TVarId
tyVarTransform (KindedVar _ _ _) = throwE "kinded type variable"
tyVarTransform (UnkindedVar _ n) = getVar n

findInvalidNames :: [T.QualifiedName] -> T.HsType -> [T.QualifiedName]
findInvalidNames _ T.TypeVar {}          = []
findInvalidNames _ T.TypeConstant {}     = []
findInvalidNames valids (T.TypeCons qn) = case qn of
    n@(T.QualifiedName _ _) -> [ n | n `notElem` valids ]
    _                       -> []
findInvalidNames valids (T.TypeArrow t1 t2)   =
  findInvalidNames valids t1 ++ findInvalidNames valids t2
findInvalidNames valids (T.TypeApp t1 t2)     =
  findInvalidNames valids t1 ++ findInvalidNames valids t2
findInvalidNames valids (T.TypeForall _ constraints t1) =
  findInvalidNames valids t1
  ++ concatMap (concatMap (findInvalidNames valids) . T.constraint_params) constraints
