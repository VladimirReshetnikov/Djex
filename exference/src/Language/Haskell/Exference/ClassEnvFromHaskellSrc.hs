{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonadComprehensions #-}

module Language.Haskell.Exference.ClassEnvFromHaskellSrc
  ( getClassEnv
  )
where



import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc ( SrcSpanInfo )
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc
import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.HaskellSrcUtils

import qualified Data.Map.Strict as M
import qualified Data.Map.Lazy as LazyMap
import Control.Monad.Trans.Except
import Control.Monad.Except ( liftEither )
import Control.Monad.Fix ( MonadFix )
import Control.Monad ( forM )

import Data.Maybe ( maybeToList )
import Data.Either ( lefts, rights )
import Data.List ( find )

import Control.Monad.Trans.MultiRWS
import Data.HList.ContainsType (ContainsType)




-- | returns the environment and the number of class instances
--   found (before inflating the instances). The number of
--   classes can be derived from the other output.
--   The count may be used to inform the user (post-inflation count
--   would be bad for that purpose.)
getClassEnv :: ( ContainsType [String] w
               , MonadFix m
               , Applicative m
               )
            => [QualifiedName]
            -> TypeDeclMap
            -> [Module SrcSpanInfo]
            -> MultiRWST r w s m (StaticClassEnv, Int)
getClassEnv ds tDeclMap ms = do
  etcs <- getTypeClasses ds tDeclMap ms
  mapM_ (mTell . (:[])) $ lefts etcs
  let tcs = rights etcs
  einsts <- getInstances tcs ds tDeclMap ms
  mapM_ (mTell . (:[])) $ lefts einsts
  let insts_uninflated = rights einsts
  let insts = inflateInstances insts_uninflated
  return (mkStaticClassEnv tcs insts, length insts_uninflated)

type TempAsst = (QualifiedName, [HsType])

getTypeClasses :: forall m r w s m0
                . ( MonadFix m0
                  , Applicative m0
                  , m ~ MultiRWST r w s m0
                  )
               => [QualifiedName]
               -> TypeDeclMap
               -> [Module SrcSpanInfo]
               -> m [Either String HsTypeClass]
getTypeClasses ds tDeclMap ms = do
  secondMap :: M.Map QualifiedName (Either String ([TempAsst], [TVarId])) <-
    fmap M.fromList $ sequence
      [ [ (qn, x) --m (inner) -- []
        | let qn = convertModuleName moduleName name
        , x <- withMultiStateA (ConvData 0 M.empty) $ runExceptT $ let
              convF (TypeA _ classType) = do
                (qname, types) <- maybe
                  (throwE $ "invalid superclass constraint: " ++ prettyPrint classType)
                  pure
                  (splitClassApplication classType)
                (,) (convertQName (Just moduleName) ds qname)
                  <$> mapM (convertTypeInternal [] (Just moduleName) ds tDeclMap) types
              convF (ParenA _ c) = convF c
              convF c = throwE $ "unknown superclass constraint: " ++ show c
            in (,) <$> mapM convF (contextConstraints context)
                   <*> mapM tyVarTransform vars
        ]
      | modul <- ms
      , Just (moduleName, decls) <- [moduleNameAndDecls modul]
      , ClassDecl _ context rawHead _ _ <- decls
      , let (name, vars) = splitDeclHead rawHead
      ]
  let
    helper :: QualifiedName
           -> Either String ([TempAsst], [TVarId])
           -> Either String HsTypeClass
    helper qnid eTcRawData = do
      (tempAssts, tVarIds) <- eTcRawData
      HsTypeClass qnid tVarIds
        <$> tempAssts `forM` \(cQnid, vars) ->
          flip HsConstraint vars
            <$> M.findWithDefault (Right $ unknownTypeClass cQnid) cQnid resultMap

    resultMap :: LazyMap.Map QualifiedName (Either String HsTypeClass)
      -- CARE: DONT USE STRICT METHODS ON THIS MAP
      --       (COMPILER WONT COMPLAIN)
    resultMap = LazyMap.mapWithKey helper secondMap
  return $ LazyMap.elems $ resultMap

getInstances :: forall m m0 r w s
              . ( m ~ MultiRWST r w s m0
                , Monad m0
                )
             => [HsTypeClass]
             -> [QualifiedName]
             -> TypeDeclMap
             -> [Module SrcSpanInfo]
             -> m [Either String HsInstance]
getInstances tcs ds tDeclMap ms = sequence $ do
  modul <- ms
  (mn, decls) <- maybeToList $ moduleNameAndDecls modul
  InstDecl _ _ rule _ <- decls
  (_variables, context, qname, tps) <- maybeToList $ splitInstRule rule
    -- vars would be the forall binds in
    -- > instance forall a . Show (Foo a) where [..]
    -- which we can ignore, right?
  let name = convertQName (Just mn) ds qname
  return $ do
    let instClass = maybe (Left $ "unknown type class: "++show name) Right
                  $ find ((name==).tclass_name) tcs
    let
      sAction :: forall m1
               . ( MonadMultiState ConvData m1
                 )
              => ExceptT String m1 HsInstance
      sAction = do
        -- varIds <- mapM tyVarTransform vars
        constrs <- contextConstraints context `forM` \asst ->
          constrTransform
            (Just mn)
            ds
            tDeclMap
            (\str -> find ((str==).tclass_name) tcs)
            asst
        rtps <- convertTypeInternal tcs (Just mn) ds tDeclMap `mapM` tps
        ic <- liftEither instClass
        return $ HsInstance constrs ic rtps
        -- either (Left . (("instance for "++name++": ")++)) Right
    withMultiStateA (ConvData 0 M.empty) $ runExceptT sAction

constrTransform
  :: (MonadMultiState ConvData m)
  => Maybe (ModuleName SrcSpanInfo)
  -> [QualifiedName]
  -> TypeDeclMap
  -> (QualifiedName -> Maybe HsTypeClass)
  -> Asst SrcSpanInfo
  -> ExceptT String m HsConstraint
constrTransform mn ds tDeclMap tcLookupF (TypeA _ classType) = do
  (qname, types) <- maybe
    (throwE $ "invalid instance constraint: " ++ prettyPrint classType)
    pure
    (splitClassApplication classType)
  let ctypes = convertTypeInternal [] mn ds tDeclMap `mapM` types
  let qn = convertQName mn ds qname
  maybe
    (throwE $ "unknown type class: " ++ show qn)
    (\tc -> HsConstraint tc <$> ctypes)
    (tcLookupF qn)
constrTransform mn ds tDeclMap tcLookupF (ParenA _ c) = constrTransform mn ds tDeclMap tcLookupF c
constrTransform _ _ _ _ c = throwE $ "unknown HsConstraint: " ++ show c
