{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE PatternGuards #-}

module Language.Haskell.Exference.TypeDeclsFromHaskellSrc
  ( HsTypeDecl (..)
  , TypeDeclMap
  , applyTypeDecls
  , getTypeDecls
  , convertType
  , convertTypeInternal
  , parseType
  )
where



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.HaskellSrcUtils
import Language.Haskell.Exference.Diagnostic

import Language.Haskell.Exts.Syntax hiding (TypeApp)
import qualified Language.Haskell.Exts.Parser as P
import Language.Haskell.Exts.SrcLoc
  ( SrcLoc (..)
  , SrcSpanInfo
  )

import Control.Monad.Trans.MultiRWS

import Control.Monad.Trans.Except ( runExceptT
                                  , mapExceptT
                                  , ExceptT(..)
                                  , throwE
                                  )
import Control.Monad.Except ( liftEither )

import Control.Monad ( forM, liftM )
import Data.Either ( rights )
import Data.Bifunctor ( bimap, first )
import Data.Maybe ( maybeToList )
import Data.List ( intercalate )

import Data.Map ( Map )
import qualified Data.Map as M
import qualified Data.IntMap as IntMap



data HsTypeDecl = HsTypeDecl
  { tdecl_name :: QualifiedName
  , tdecl_params :: [TVarId]
  , tdecl_result :: HsType
  } deriving Show -- (Data, Show, Generic, Typeable)

type TypeDeclMap = Map QualifiedName HsTypeDecl

applyTypeDecls :: Map QualifiedName (Either String HsTypeDecl)
               -> HsType 
               -> Either String HsType
applyTypeDecls declarations = go []
 where
  go _ (TypeVar i)      = Right $ TypeVar i
  go _ (TypeConstant i) = Right $ TypeConstant i
  go path t@(TypeCons _)  = goApp path [] t
  go path (TypeArrow t1 t2) = [ TypeArrow t1' t2'
                         | t1' <- go path t1
                         , t2' <- go path t2
                         ]
  go path (TypeApp l r) = goApp path [r] l
  go path (TypeForall vars constraints t) = do
    constraints' <- mapM (mapConstraint $ go path) constraints
    TypeForall vars constraints' <$> go path t
  goApp path rs (TypeApp l r) = goApp path (r:rs) l
  goApp path rs (TypeCons qn) = case M.lookup qn declarations of
    Nothing -> foldl TypeApp (TypeCons qn) <$> mapM (go path) rs
    -- The declaration error is reported separately by 'getTypeDecls'. Keep an
    -- unexpanded use here, but do not lose its already converted arguments.
    Just (Left _) -> foldl TypeApp (TypeCons qn) <$> mapM (go path) rs
    Just (Right _) | qn `elem` path -> Left $ "cyclic type synonym: "
      ++ intercalate " -> " (map show $ reverse path ++ [qn])
    Just (Right (HsTypeDecl _ vs t))
                             | i <- length vs
                             , i <= length rs
                             -> [ foldl TypeApp expanded pUnchanged
                                | rs' <- mapM (go path) rs
                                , let pAffected = take i rs'
                                , let pUnchanged = drop i rs'
                                , let substs = IntMap.fromList $ zip vs pAffected
                                , let substituted = snd $ applySubsts substs t
                                , expanded <- go (qn : path) substituted
                                ]
    _                        -> Left $ "wrong number of parameters for type declaration " ++ show qn
  goApp path rs l = foldl1 TypeApp <$> mapM (go path) (l:rs)

  mapConstraint f (HsConstraint typeClass parameters) =
    HsConstraint typeClass <$> mapM f parameters

getTypeDecls :: ( Monad m
                )
             => [QualifiedName]
             -> [Module SrcSpanInfo]
             -> MultiRWST r w s m [Either String HsTypeDecl]
getTypeDecls ds modules = do
  rawList <- sequence $ do
    modul <- modules
    (mn, decls) <- maybeToList $ moduleNameAndDecls modul
    TypeDecl _ rawHead rawTy <- decls
    let (name, rawVars) = splitDeclHead rawHead
    return $ liftM (bimap (("when parsing type declaration "++show name++": ")++) id)
           $ runExceptT
           $ do
      (ty, tyVarIndex) <- convertTypeNoDecl [] (Just mn) ds rawTy
      qname <- either throwE pure $ convertModuleNameChecked mn name
      -- the 1000 is arbitrary, but it should not be used anyway.
      -- no new type variables should appear on the left hand side.
      vars <- mapExceptT (withMultiStateA (ConvData 1000 tyVarIndex)) $ rawVars `forM` tyVarTransform
      return $ HsTypeDecl qname vars ty
  let validDeclarations = rights rawList
      declarationMap = M.fromList
        [(tdecl_name declaration, Right declaration) | declaration <- validDeclarations]
      resolve declaration = HsTypeDecl
        (tdecl_name declaration)
        (tdecl_params declaration)
        <$> applyTypeDecls declarationMap (tdecl_result declaration)
  return $ [ e | e@(Left _) <- rawList ] ++ map resolve validDeclarations

convertType :: ( Monad m
               )
            => [HsTypeClass]
            -> Maybe (ModuleName SrcSpanInfo)
            -> [QualifiedName]
            -> TypeDeclMap
            -> Type SrcSpanInfo
            -> ExceptT String (MultiRWST r w s m) (HsType, TypeVarIndex)
convertType tcs mn ds declMap t = do
  (ty, index) <- convertTypeNoDecl tcs mn ds t
  ty' <- liftEither $ applyTypeDecls (M.map Right declMap) ty
  return $ (ty', index)

convertTypeInternal
  :: (MonadMultiState ConvData m)
  => [HsTypeClass]
  -> Maybe (ModuleName SrcSpanInfo) -- default (for unqualified stuff)
                      -- Nothing uses a broad search for lookups
  -> [QualifiedName] -- list of fully qualified data types
                                         -- (to keep things unique)
  -> TypeDeclMap
  -> Type SrcSpanInfo
  -> ExceptT String m HsType
convertTypeInternal tcs defModuleName ds declMap t = do
  ty <- convertTypeNoDeclInternal tcs defModuleName ds t
  ty' <- liftEither $ applyTypeDecls (M.map Right declMap) ty
  return $ ty'

parseType
  :: (Monad m)
  => [HsTypeClass]
  -> Maybe (ModuleName SrcSpanInfo)
  -> [QualifiedName]
  -> TypeDeclMap
  -> P.ParseMode
  -> String
  -> ExceptT
       Diagnostic
       (MultiRWST r w s m)
       (HsType, TypeVarIndex)
parseType tcs mn ds tDeclMap m s = case P.parseTypeWithMode m s of
  P.ParseFailed location message -> throwE
    $ withSpan (let position = SourcePosition
                      (srcLine location) (srcColumn location)
                in SourceSpan position position)
    $ withSource (srcFilename location)
    $ diagnostic message
  P.ParseOk ty -> ExceptT $ first conversionDiagnostic
    <$> runExceptT (convertType tcs mn ds tDeclMap ty)
  where
    conversionDiagnostic message =
      withSpan (SourceSpan (SourcePosition 1 1) (endPosition s))
      $ withSource (P.parseFilename m)
      $ diagnostic message

    endPosition = foldl advance (SourcePosition 1 1)
    advance (SourcePosition line _) '\n' = SourcePosition (line + 1) 1
    advance (SourcePosition line column) _ = SourcePosition line (column + 1)
