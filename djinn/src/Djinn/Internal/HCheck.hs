--
-- Copyright (c) 2005 Lennart Augustsson
-- See LICENSE for licensing details.
--
module Djinn.Internal.HCheck(
    htCheckEnv, htCheckType, htCheckTypeKind, htInferClassKinds
    ) where
import Data.List(union, (\\))
import Control.Monad.State
import Data.IntMap(IntMap)
import qualified Data.IntMap as IntMap
import Data.Graph(stronglyConnComp, SCC(..))

import Djinn.Internal.HTypes

-- Kind inference uses numbered unification variables (KVar).  The state maps
-- each variable to its solution: Nothing while unconstrained, and possibly a
-- chain of KVars that 'follow' resolves.  'ground' finally defaults any
-- still-unconstrained variable to *, as Haskell98 does.
type KState = (Int, IntMap (Maybe HKind))
initState :: KState
initState = (0, IntMap.empty)

type M a = StateT KState (Either String) a

type KEnv = [(HSymbol, HKind)]

-- These constructors are part of the type grammar rather than the mutable
-- synonym environment, so the kind checker must know about them explicitly.
intrinsicKinds :: KEnv
intrinsicKinds =
    [("[]", KArrow KStar KStar),
     ("->", KArrow KStar (KArrow KStar KStar))]

newKVar :: M HKind
newKVar = do
    (i, m) <- get
    put (i+1, IntMap.insert i Nothing m)
    return $ KVar i

follow :: HKind -> M HKind
follow k@(KVar i) = do
    (_, m) <- get
    case IntMap.lookup i m of
        Nothing -> lift $ Left $ "Unknown kind variable: " ++ show i
        Just Nothing -> return k
        Just (Just k') -> follow k'
follow k = return k

addMap :: Int -> HKind -> M ()
addMap i k = do
    (n, m) <- get
    put (n, IntMap.insert i (Just k) m)

clearState :: M ()
clearState = put initState

htCheckType :: [(HSymbol, ([HSymbol], HType, HKind))] -> HType -> Either String ()
htCheckType its = htCheckTypeKind its KStar

-- Check that a type is well-kinded and has the given (ground) kind.  Free
-- type variables receive fresh kinds, so a variable argument fits any
-- expected kind while a mis-kinded application is still rejected.
htCheckTypeKind :: [(HSymbol, ([HSymbol], HType, HKind))]
                -> HKind -> HType -> Either String ()
htCheckTypeKind its expected t = flip evalStateT initState $ do
    let vs = getHTVars t
    ks <- mapM (const newKVar) vs
    let env = zip vs ks ++ [(i, k) | (i, (_, _, k)) <- its] ++ intrinsicKinds
    k <- iHKind env t
    unifyK k expected

-- Infer the kind of every class parameter from the class's method types,
-- as Haskell98 does.  Parameters left unconstrained (including every
-- parameter of a method-less class) default to *.  Non-parameter method
-- variables share one scope across the methods, matching how the stored
-- method types are validated elsewhere.
htInferClassKinds :: [(HSymbol, ([HSymbol], HType, HKind))]
                  -> [HSymbol] -> [HType]
                  -> Either String [(HSymbol, HKind)]
htInferClassKinds its params methodTypes = flip evalStateT initState $ do
    paramKinds <- mapM (const newKVar) params
    let locals = foldr union [] (map getHTVars methodTypes) \\ params
    localKinds <- mapM (const newKVar) locals
    let env = zip params paramKinds ++ zip locals localKinds ++
              [(i, k) | (i, (_, _, k)) <- its] ++ intrinsicKinds
    mapM_ (iHKindStar env) methodTypes
    grounded <- mapM ground paramKinds
    return $ zip params grounded

htCheckEnv :: [(HSymbol, ([HSymbol], HType, a))] -> Either String [(HSymbol, ([HSymbol], HType, HKind))]
htCheckEnv its =
    let graph = [ (n, i, getHTCons t) | n@(i, (_, t, _)) <- its ]
        order = stronglyConnComp graph
    in  case [ c | CyclicSCC c <- order ] of
        c : _ -> Left $ "Recursive types are not allowed: " ++ unwords [ i | (i, _) <- c ]
        [] -> flip evalStateT initState $ addKinds
            where addKinds = do
                        env <- inferHKinds intrinsicKinds [n | AcyclicSCC n <- order]
                        let addKind (i, (vs, t, _)) =
                                case lookup i env of
                                    Nothing -> lift $ Left $
                                        "Internal kind inference error for " ++ i
                                    Just k -> return (i, (vs, t, k))
                        mapM addKind its

inferHKinds :: KEnv -> [(HSymbol, ([HSymbol], HType, a))] -> M KEnv
inferHKinds env [] = return env
inferHKinds env ((i, (vs, t, _)) : its) = do
    k <- inferHKind env vs t
    inferHKinds ((i, k) : env) its

inferHKind :: KEnv -> [HSymbol] -> HType -> M HKind
inferHKind _ _ (HTAbstract _ k) = return k
inferHKind env vs t = do
    clearState
    ks <- mapM (const newKVar) vs
    let env' = zip vs ks ++ env
    -- A Haskell type synonym or data declaration must produce a proper type;
    -- only an explicit HTAbstract declaration may end in a higher kind.
    iHKindStar env' t
    ground $ foldr KArrow KStar ks

iHKind :: KEnv -> HType -> M HKind
iHKind env (HTApp f a) = do
    kf <- iHKind env f
    ka <- iHKind env a
    r <- newKVar
    unifyK (KArrow ka r) kf
    return r
iHKind env (HTVar v) = lookupHKind "Undefined type variable" env v
iHKind env (HTCon c) = lookupHKind "Undefined type" env c
iHKind env (HTTuple ts) = do
    mapM_ (iHKindStar env) ts
    return KStar
iHKind env (HTArrow f a) = do
    iHKindStar env f
    iHKindStar env a
    return KStar
iHKind env (HTUnion cs) = do
    mapM_ (\ (_, ts) -> mapM_ (iHKindStar env) ts) cs
    return KStar
iHKind _ (HTAbstract _ k) = return k

iHKindStar :: KEnv -> HType -> M ()
iHKindStar env t = do
    k <- iHKind env t
    unifyK k KStar

unifyK :: HKind -> HKind -> M ()
unifyK k1 k2 = do
    let unify KStar KStar = return ()
        unify (KArrow k11 k12) (KArrow k21 k22) = do unifyK k11 k21; unifyK k12 k22
        unify (KVar i1) (KVar i2) | i1 == i2 = return ()
        unify (KVar i) k = do occurs i k; addMap i k
        unify k (KVar i) = do occurs i k; addMap i k
        unify left right = lift $ Left $
            "kind mismatch: " ++ show left ++ " vs " ++ show right
        occurs i k = do
            k' <- follow k
            case k' of
                KStar -> return ()
                KArrow f a -> occurs i f >> occurs i a
                KVar i'
                    | i == i' -> lift $ Left "cyclic kind"
                    | otherwise -> return ()
    k1' <- follow k1
    k2' <- follow k2
    unify k1' k2'


lookupHKind :: String -> KEnv -> HSymbol -> M HKind
lookupHKind missing env v =
    case lookup v env of
    Just k -> return k
    Nothing -> lift $ Left $ missing ++ " " ++ v

ground :: HKind -> M HKind
ground k = do
    k' <- follow k
    case k' of
        KStar -> return KStar
        KArrow k1 k2 -> KArrow <$> ground k1 <*> ground k2
        -- Unconstrained kind variables default to *, as Haskell98 requires.
        KVar _ -> return KStar

getHTCons :: HType -> [HSymbol]
getHTCons (HTApp f a) = getHTCons f `union` getHTCons a
getHTCons (HTVar _) = []
getHTCons (HTCon s) = [s]
getHTCons (HTTuple ts) = foldr union [] (map getHTCons ts)
getHTCons (HTArrow f a) = getHTCons f `union` getHTCons a
getHTCons (HTUnion alts) = foldr union [] [ getHTCons t | (_, ts) <- alts, t <- ts ]
getHTCons (HTAbstract _ _) = []
