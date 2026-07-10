{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
--
-- Copyright (c) 2005 Lennart Augustsson
-- See LICENSE for licensing details.
--
module HTypes(
        HKind(..), HType(..), HSymbol,
        hTypeToFormula, pHSymbol, pHType, pHDataType, pHTAtom, pHKind,
        prHSymbolOp, htNot, isHTUnion, getHTVars, substHT,
        HClause, HPat, HExpr(HEVar), hPrClause, termToHExpr, termToHClause,
        getBinderVars
    ) where
import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ(Doc, renderStyle, style, text, (<>), parens, ($$), vcat, punctuate,
         sep, fsep, nest, comma, (<+>))
import Data.List(union, (\\))
import Control.Monad(foldM, zipWithM)
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP
import HIdentifier
import LJTFormula

type HSymbol = String

data HKind
    = KStar
    | KArrow HKind HKind
    | KVar Int
    deriving (Eq)

instance Show HKind where
    showsPrec _ KStar = showString "*"
    showsPrec p (KArrow from to) =
        showParen (p > 0) $
            showsPrec 1 from . showString " -> " . showsPrec 0 to
    showsPrec _ (KVar i) = showString "k" . shows i

data HType
        = HTApp HType HType
        | HTVar HSymbol
        | HTCon HSymbol
        | HTTuple [HType]
        | HTArrow HType HType
        | HTUnion [(HSymbol, [HType])]          -- Only for data types; only at top level
        | HTAbstract HSymbol HKind              -- XXX Uninterpreted type, like a variable but different kind checking
        deriving (Eq)

isHTUnion :: HType -> Bool
isHTUnion (HTUnion _) = True
isHTUnion _ = False

htNot :: HSymbol -> HType
htNot x = HTArrow (HTVar x) (HTCon "Void")

instance Show HType where
    showsPrec _ (HTApp (HTCon "[]") t) = showString "[" . showsPrec 0 t . showString "]"
    showsPrec p (HTApp f a) = showParen (p > 2) $ showsPrec 2 f . showString " " . showsPrec 3 a
    showsPrec _ (HTVar s) = showString s
    showsPrec _ (HTCon "()") = showString "()"
    showsPrec _ (HTCon s) | not (isQualifiedConId s) =
        showParen True $ showString s
    showsPrec _ (HTCon s) = showString s
    showsPrec _ (HTTuple ss) = showParen True $ f ss
        where f [] = id
              f [t] = showsPrec 0 t
              f (t:ts) = showsPrec 0 t . showString ", " . f ts
    showsPrec p (HTArrow s t) = showParen (p > 0) $ showsPrec 1 s . showString " -> " . showsPrec 0 t
    showsPrec _ (HTUnion cs) = f cs
        where f [] = id
              f [cts] = scts cts
              f (cts : ctss) = scts cts . showString " | " . f ctss
              scts (c, ts) = foldl (\ s t -> s . showString " " . showsPrec 10 t) (showString c) ts
    showsPrec _ (HTAbstract s _) = showString s

instance Read HType where
    readsPrec _ = readP_to_S pHType'

pHType' :: ReadP HType
pHType' = do
    t <- pHType
    skipSpaces
    return t

pHType :: ReadP HType
pHType = do
    ts <- sepBy1 pHTypeApp (do schar '-'; char '>')
    return $ foldr1 HTArrow ts

pHDataType :: ReadP HType
pHDataType = do
    let con = do
            c <- pHSymbol True
            ts <- many pHTAtom
            return (c, ts)
    cts <- sepBy con (schar '|')
    return $ HTUnion cts

pHTAtom :: ReadP HType
pHTAtom = pHTVar +++ pHTCon +++ pHTList +++ pParen pHTTuple +++ pParen pHType +++ pUnit

pUnit :: ReadP HType
pUnit = do
    schar '('
    char ')'
    return $ HTCon "()"

pHTCon :: ReadP HType
pHTCon = (pQualifiedConId >>= return . HTCon)
       +++
         do schar '('; schar '-'; schar '>'; schar ')'; return (HTCon "->")

pHTVar :: ReadP HType
pHTVar = pHSymbol False >>= return . HTVar

pHSymbol :: Bool -> ReadP HSymbol
pHSymbol True = pConId
pHSymbol False = pVarId

pHTTuple :: ReadP HType
pHTTuple = do
    t <- pHType
    ts <- many1 (do schar ','; pHType)
    return $ HTTuple $ t:ts

pHTypeApp :: ReadP HType
pHTypeApp = do
    ts <- many1 pHTAtom
    return $ foldl1 hTApp ts

pHTList :: ReadP HType
pHTList = do
    schar '['
    t <- pHType
    schar ']'
    return $ HTApp (HTCon "[]") t

pHKind :: ReadP HKind
pHKind = do
    ts <- sepBy1 pHKindA (do schar '-'; char '>')
    return $ foldr1 KArrow ts

pHKindA :: ReadP HKind
pHKindA = (do schar '*'; return KStar) +++ pParen pHKind

pParen :: ReadP a -> ReadP a
pParen p = do
    schar '('
    e <- p
    schar ')'
    return e

schar :: Char -> ReadP ()
schar c = do
    skipSpaces
    char c
    return ()

getHTVars :: HType -> [HSymbol]
getHTVars (HTApp f a) = getHTVars f `union` getHTVars a
getHTVars (HTVar v) = [v]
getHTVars (HTCon _) = []
getHTVars (HTTuple ts) = foldr union [] (map getHTVars ts)
getHTVars (HTArrow f a) = getHTVars f `union` getHTVars a
getHTVars (HTUnion ctss) = foldr union [] [ getHTVars t | (_, ts) <- ctss, t <- ts ]
getHTVars (HTAbstract _ _) = []

-------------------------------

hTypeToFormula :: [(HSymbol, ([HSymbol], HType, a))] -> HType -> Formula
hTypeToFormula ss (HTTuple ts) = Conj (map (hTypeToFormula ss) ts)
hTypeToFormula ss (HTArrow t1 t2) = hTypeToFormula ss t1 :-> hTypeToFormula ss t2
hTypeToFormula _ (HTUnion []) = false
hTypeToFormula ss (HTUnion ctss) = Disj
    [(ConsDesc c (length ts), hTypeToFormula ss (HTTuple ts)) |
        (c, ts) <- ctss]
hTypeToFormula ss t =
    case expandSyn ss t [] of
    Nothing -> PVar $ Symbol $ show t
    -- Tag an empty datatype at the declaration that introduced it.  An alias
    -- is expanded again first, so aliases share the underlying nominal tag.
    Just (HTUnion []) -> Empty $ Symbol $ show $ normalizeAliases ss t
    Just t' -> hTypeToFormula ss t'

-- Empty propositions use their applied datatype as a nominal identity.  Type
-- synonyms inside that application are definitionally transparent in Haskell,
-- so normalize aliases without expanding the datatype declaration itself.
normalizeAliases :: [(HSymbol, ([HSymbol], HType, a))] -> HType -> HType
normalizeAliases definitions source =
    case application source [] of
        (HTCon name, arguments) ->
            case lookup name definitions of
                Just (parameters, body, _)
                    | length parameters == length arguments &&
                      isAliasBody body ->
                        normalizeAliases definitions $
                            substHT (zip parameters arguments) body
                _ -> foldl hTApp (HTCon name) $
                    map (normalizeAliases definitions) arguments
        _ -> normalizeStructure source
  where
    application (HTApp function argument) arguments =
        application function (argument : arguments)
    application headType arguments = (headType, arguments)

    isAliasBody (HTUnion _) = False
    isAliasBody (HTAbstract _ _) = False
    isAliasBody _ = True

    normalizeStructure (HTApp function argument) =
        hTApp (normalizeAliases definitions function) $
            normalizeAliases definitions argument
    normalizeStructure (HTTuple types) =
        HTTuple $ map (normalizeAliases definitions) types
    normalizeStructure (HTArrow argument result) =
        HTArrow (normalizeAliases definitions argument) $
            normalizeAliases definitions result
    normalizeStructure (HTUnion constructors) = HTUnion
        [(constructor, map (normalizeAliases definitions) types) |
            (constructor, types) <- constructors]
    normalizeStructure other = other

expandSyn :: [(HSymbol, ([HSymbol], HType, a))] -> HType -> [HType] -> Maybe HType
expandSyn ss (HTApp f a) as = expandSyn ss f (a:as)
expandSyn ss (HTCon c) as =
    case lookup c ss of
    Just (vs, t, _) | length vs == length as -> Just $ substHT (zip vs as) t
    _ -> Nothing
expandSyn _ _ _ = Nothing

substHT :: [(HSymbol, HType)] -> HType -> HType
substHT r (HTApp f a) = hTApp (substHT r f) (substHT r a)
substHT r t@(HTVar v) =
    case lookup v r of
    Nothing -> t
    Just t' -> t'
substHT _ t@(HTCon _) = t
substHT r (HTTuple ts) = HTTuple (map (substHT r) ts)
substHT r (HTArrow f a) = HTArrow (substHT r f) (substHT r a)
substHT r (HTUnion ctss) = HTUnion [ (c, map (substHT r) ts) | (c, ts) <- ctss ]
substHT _ t@(HTAbstract _ _) = t

-- Keep the prefix spelling `(->) a b` in the same canonical form as `a -> b`.
hTApp :: HType -> HType -> HType
hTApp (HTApp (HTCon "->") a) b = HTArrow a b
hTApp a b = HTApp a b

-------------------------------


data HClause = HClause HSymbol [HPat] HExpr
    deriving (Show, Eq)

data HPat = HPVar HSymbol | HPCon HSymbol | HPTuple [HPat] | HPAt HSymbol HPat | HPApply HPat HPat
    deriving (Show, Eq)

data HExpr = HELam [HPat] HExpr | HEApply HExpr HExpr | HECon HSymbol | HEVar HSymbol | HETuple [HExpr] |
        HECase HExpr [(HPat, HExpr)]
    deriving (Show, Eq)

hPrClause :: HClause -> String
hPrClause = renderStyle style . ppClause

ppClause :: HClause -> Doc
ppClause (HClause f ps e) =
    text (prHSymbolOp f) <+>
        sep [sep (map (ppPat 10) ps) <+> text "=", nest 2 $ ppExpr 0 e]

prHSymbolOp :: HSymbol -> String
prHSymbolOp = renderVarName

ppPat :: Int -> HPat -> Doc
ppPat _ (HPVar s) = text s
ppPat _ (HPCon s) = text s
ppPat _ (HPTuple ps) = parens $ fsep $ punctuate comma (map (ppPat 0) ps)
ppPat _ (HPAt s p) = text s <> text "@" <> ppPat 10 p
ppPat p (HPApply a b) = pparens (p > 1) $ ppPat 1 a <+> ppPat 2 b

ppExpr :: Int -> HExpr -> Doc
ppExpr p (HELam ps e) = pparens (p > 0) $ sep [ text "\\" <+> sep (map (ppPat 10) ps) <+> text "->",
                                                ppExpr 0 e]
ppExpr p (HEApply (HEApply (HEVar f) a1) a2) | isVarOperator f =
     pparens (p > 4) $ ppExpr 5 a1 <+> text f <+> ppExpr 5 a2
ppExpr p (HEApply f a) = pparens (p > 11) $ ppExpr 11 f <+> ppExpr 12 a
ppExpr _ (HECon s) = text s
ppExpr _ (HEVar s) | isVarOperator s = pparens True $ text s
ppExpr _ (HEVar s) = text s
ppExpr _ (HETuple es) = parens $ fsep $ punctuate comma (map (ppExpr 0) es)
ppExpr p (HECase s alts) = pparens (p > 0) $ (text "case" <+> ppExpr 0 s <+> text "of") $$
                            vcat (map ppAlt alts)
  where ppAlt (pp, e) = ppPat 0 pp <+> text "->" <+> ppExpr 0 e


pparens :: Bool -> Doc -> Doc
pparens True d = parens d
pparens False d = d

-------------------------------


unSymbol :: Symbol -> HSymbol
unSymbol (Symbol s) = s

termToHExpr :: Term -> Either String HExpr
termToHExpr term = do
    (expression, _) <- conv [] $ alphaRenameTerm term
    return $ niceNames $ etaReduce $ remUnusedVars $ collapseCase $
        fixSillyAt $ remUnusedVars expression
  -- Besides the expression, conversion records how enclosing variables are
  -- decomposed.  convV later turns those refinements into tuple/as-patterns.
  where conv _vs (Var s) = Right (HEVar $ unSymbol s, [])
        conv vs (Lam s te) = do
                let hs = unSymbol s
                (te', ss) <- conv (hs : vs) te
                pattern' <- convV hs ss
                return (hELam [pattern'] te', ss)
        conv vs (Apply (Cinj (ConsDesc s n) _) a) = do
                (ha, ss) <- conv vs a
                (wrap, arguments) <- unTuple n ha
                return
                    (wrap $ foldl HEApply (HECon s) arguments, ss)
        conv vs (Apply te1 te2) = convAp vs te1 [te2]
        conv _vs (Ctuple 0) = Right (HECon "()", [])
        conv _vs e = Left $ "unsupported proof term: " ++ show e

        unTuple 0 _ = Right (id, [])
        unTuple 1 a = Right (id, [a])
        unTuple n (HETuple as) | length as == n = Right (id, as)
        unTuple n e = Left $ "constructor payload has shape " ++ show e ++
            ", expected a tuple of arity " ++ show n

        unTupleP 0 _ = Right []
        unTupleP 1 pattern' = Right [pattern']
        unTupleP n (HPTuple ps) | length ps == n = Right ps
        unTupleP n p = Left $ "constructor pattern has shape " ++ show p ++
            ", expected a tuple of arity " ++ show n

        convAp vs (Apply te1 te2) as = convAp vs te1 (te2:as)
        convAp vs (Ctuple n) as | length as == n = do
                converted <- mapM (conv vs) as
                let (es, sss) = unzip converted
                return (hETuple es, concat sss)
        convAp _ (Ctuple n) as = Left $
                "tuple constructor expects " ++ show n ++
                " arguments, got " ++ show (length as)
        convAp vs (Ccases cds) (se : args) | length args >= numAlts =
                do
                    let (handlers, rest) = splitAt numAlts args
                    convertedAlts <- zipWithM (cAlt vs) handlers cds
                    (e', ess) <- conv vs se
                    convertedRest <- mapM (conv vs) rest
                    let (alts, ass) = unzip convertedAlts
                        (rest', rss) = unzip convertedRest
                    return
                        ( foldl HEApply (hECase e' alts) rest'
                        , ess ++ concat ass ++ concat rss
                        )
          where numAlts = length cds
        convAp _ (Ccases cds) args =
                Left $ "case eliminator expects a scrutinee and " ++
                    show (length cds) ++ " alternatives, got " ++
                    show (length args) ++ " arguments"
        convAp vs (Csplit n) (b : a : as) = do
                (hb, sb) <- conv vs b
                (a', sa) <- conv vs a
                convertedArgs <- mapM (conv vs) as
                (ps, b') <- unLam n hb
                let (as', sss) = unzip convertedArgs
                case a' of
                    HEVar v | v `elem` vs && null as ->
                        return (b', [(v, HPTuple ps)] ++ sb ++ sa)
                    _ -> return
                        ( foldl HEApply
                            (hECase a' [(HPTuple ps, b')]) as'
                        , sb ++ sa ++ concat sss
                        )
        convAp _ (Csplit n) args = Left $
                "tuple eliminator of arity " ++ show n ++
                " expects a handler and tuple, got " ++
                show (length args) ++ " arguments"

        convAp vs f as = do
                converted <- mapM (conv vs) (f:as)
                let (es, sss) = unzip converted
                return (foldl1 HEApply es, concat sss)

        convV hs ss =
                case [ y | (x, y) <- ss, x == hs ] of
                [] -> Right $ HPVar hs
                [p] -> Right $ HPAt hs p
                p : ps -> do
                    merged <- foldM combPat p ps
                    return $ HPAt hs merged

        combPat p p' | p == p' = Right p
        combPat (HPVar v) p = Right $ HPAt v p
        combPat p (HPVar v) = Right $ HPAt v p
        combPat (HPAt v p) (HPAt v' p') = do
                merged <- combPat p p'
                return $ case v == v' of
                    True -> HPAt v merged
                    False -> HPAt v $ HPAt v' merged
        combPat (HPAt v p) p' = HPAt v `fmap` combPat p p'
        combPat p (HPAt v p') = HPAt v `fmap` combPat p p'
        combPat (HPTuple ps) (HPTuple ps') | length ps == length ps' =
                HPTuple `fmap` zipWithM combPat ps ps'
        combPat p p' = Left $ "cannot merge incompatible patterns " ++
            show p ++ " and " ++ show p'

        cAlt vs (Lam v e) (ConsDesc c n) = do
            let hv = unSymbol v
            (he, ss) <- conv (hv : vs) e
            patterns <- case lookup hv ss of
                Nothing -> Right $ replicate n (HPVar "_")
                Just pattern' -> unTupleP n pattern'
            return ((foldl HPApply (HPCon c) patterns, he), ss)
        cAlt _ handler _ = Left $
            "case alternative is not a lambda: " ++ show handler

        unLam 0 e = Right ([], e)
        unLam n (HELam patterns e) | length patterns >= n =
            let (used, remaining) = splitAt n patterns
            in Right (used, hELam remaining e)
        unLam n e = Left $ "tuple handler has shape " ++ show e ++
            ", expected " ++ show n ++ " lambda argument(s)"

        hETuple [e] = e
        hETuple es = HETuple es

-- Downstream Haskell-AST rewrites historically assumed that every binder had
-- a globally unique name.  Enforce that invariant even for externally built
-- terms, while retaining all free assumption names verbatim.
alphaRenameTerm :: Term -> Term
alphaRenameTerm term = renamed
  where
    (renamed, _, _) =
        rename [] (Set.fromList $ freeVars term) (1 :: Integer) term

    rename environment used next proofTerm =
        case proofTerm of
            Var symbol ->
                (Var $ maybe symbol id $ lookup symbol environment, used, next)
            Lam binder body ->
                let (fresh, used', next') = freshBinder used next
                    (body', used'', next'') =
                        rename ((binder, fresh) : environment)
                            used' next' body
                in (Lam fresh body', used'', next'')
            Apply function argument ->
                let (function', used', next') =
                        rename environment used next function
                    (argument', used'', next'') =
                        rename environment used' next' argument
                in (Apply function' argument', used'', next'')
            Xsel index arity expression ->
                let (expression', used', next') =
                        rename environment used next expression
                in (Xsel index arity expression', used', next')
            _ -> (proofTerm, used, next)

    freshBinder used next =
        let candidate = Symbol $ "__djinn" ++ show next
        in if candidate `Set.member` used then
               freshBinder used (next + 1)
           else
               (candidate, Set.insert candidate used, next + 1)

-- Eliminate degenerate as-patterns such as x@y by retaining y and renaming x.
-- XXX This should be integrated into some earlier phase, but this is simpler.
fixSillyAt :: HExpr -> HExpr
fixSillyAt = fixAt []
  where fixAt s (HELam ps e) = HELam ps' (fixAt (concat ss ++ s) e) where (ps', ss) = unzip $ map findSilly ps
        fixAt s (HEApply f a) = HEApply (fixAt s f) (fixAt s a)
        fixAt _ e@(HECon _) = e
        fixAt s e@(HEVar v) = maybe e HEVar $ lookup v s
        fixAt s (HETuple es) = HETuple (map (fixAt s) es)
        fixAt s (HECase e alts) = HECase (fixAt s e) (map (fixAtAlt s) alts)
        fixAtAlt s (p, e) = (p', fixAt (s' ++ s) e) where (p', s') = findSilly p
        findSilly p@(HPVar _) = (p, [])
        findSilly p@(HPCon _) = (p, [])
        findSilly (HPTuple ps) = (HPTuple ps', concat ss) where (ps', ss) = unzip $ map findSilly ps
        findSilly (HPAt v p) = case findSilly p of
                               (p'@(HPVar v'), s) -> (p', (v, v'):s)
                               (p', s) -> (HPAt v p', s)
        findSilly (HPApply f a) = (HPApply f' a', sf ++ sa) where (f', sf) = findSilly f; (a', sa) = findSilly a

-- XXX This shouldn't be needed.  There's similar code in hECase,
-- but the fixSillyAt reveals new opportunities.
collapseCase :: HExpr -> HExpr
collapseCase (HELam ps e) = HELam ps (collapseCase e)
collapseCase (HEApply f a) = HEApply (collapseCase f) (collapseCase a)
collapseCase e@(HECon _) = e
collapseCase e@(HEVar _) = e
collapseCase (HETuple es) = HETuple (map collapseCase es)
collapseCase (HECase e alts) =
    case [(p, collapseCase b) | (p, b) <- alts ] of
    (p, b) : pes | noBound p && all (\ (p', b') -> alphaEq b b' && noBound p') pes -> b
    pes -> HECase (collapseCase e) pes
 where noBound = all (== "_") . getBinderVarsHP

niceNames :: HExpr -> HExpr
niceNames e =
    let bvars = filter (/= "_") $ getBinderVarsHE e
        nvars = [[c] | c <- ['a'..'z']] ++ [ "x" ++ show i | i <- [1::Integer ..]]
        freevars = getAllVars e \\ bvars
        vars = nvars \\ freevars
        sub = zip bvars vars
    in  hESubst sub e

hELam :: [HPat] -> HExpr -> HExpr
hELam [] e = e
hELam ps (HELam ps' e) = HELam (ps ++ ps') e
hELam ps e = HELam ps e

hECase :: HExpr -> [(HPat, HExpr)] -> HExpr
hECase e [] = HEApply (HEVar "void") e
hECase _ [(HPCon "()", e)] = e
hECase e pes | all (uncurry eqPatExpr) pes = e
hECase e [(p, HELam ps b)] = HELam ps $ hECase e [(p, b)]
hECase se alts@((_, HELam ops _):_) | m > 0 = HELam (take m ops) $ hECase se alts'
  where m = minimum (map (numBind . snd) alts)
        numBind (HELam ps _) = length (takeWhile isPVar ps)
        numBind _ = 0
        isPVar (HPVar _) = True
        isPVar _ = False
        alts' = [ let (ps1, ps2) = splitAt m ps
                      sub = zip [v | HPVar v <- ps1] ns
                  in (cps, hELam ps2 $ hESubst sub e)
                  | (cps, HELam ps e) <- alts ]
        ns = [ n | HPVar n <- take m ops ]
-- if all arms are equal and there are at least two alternatives there can be no bound vars
-- from the patterns
hECase _ ((_,e):alts@(_:_)) | all (alphaEq e . snd) alts = e
hECase e alts = HECase e alts

eqPatExpr :: HPat -> HExpr -> Bool
eqPatExpr (HPVar s) (HEVar s') = s == s'
eqPatExpr (HPCon s) (HECon s') = s == s'
eqPatExpr (HPTuple ps) (HETuple es) =
    length ps == length es && and (zipWith eqPatExpr ps es)
eqPatExpr (HPApply pf pa) (HEApply ef ea) = eqPatExpr pf ef && eqPatExpr pa ea
eqPatExpr _ _ = False

-- Converter-generated binders are globally fresh, so this simultaneous rename
-- is capture-free even though hESubst is deliberately simpler than a general
-- capture-avoiding substitution.
alphaEq :: HExpr -> HExpr -> Bool
alphaEq e1 e2 | e1 == e2 = True
alphaEq (HELam ps1 e1) (HELam ps2 e2) =
    case matchPat (HPTuple ps1) (HPTuple ps2) of
    Just s -> alphaEq (hESubst s e1) e2
    Nothing -> False
alphaEq (HEApply f1 a1) (HEApply f2 a2) = alphaEq f1 f2 && alphaEq a1 a2
alphaEq (HECon s1) (HECon s2) = s1 == s2
alphaEq (HEVar s1) (HEVar s2) = s1 == s2
alphaEq (HETuple es1) (HETuple es2) | length es1 == length es2 = and (zipWith alphaEq es1 es2)
alphaEq (HECase e1 alts1) (HECase e2 alts2) =
    length alts1 == length alts2 && alphaEq e1 e2 &&
        and (zipWith alphaEq
            [ HELam [p] e | (p, e) <- alts1 ]
            [ HELam [p] e | (p, e) <- alts2 ])
alphaEq _ _ = False

matchPat :: HPat -> HPat -> Maybe [(HSymbol, HSymbol)]
matchPat (HPVar s1) (HPVar s2) = return [(s1, s2)]
matchPat (HPCon s1) (HPCon s2) | s1 == s2 = return []
matchPat (HPTuple ps1) (HPTuple ps2) | length ps1 == length ps2 = do
    ss <- zipWithM matchPat ps1 ps2
    return $ concat ss
matchPat (HPAt s1 p1) (HPAt s2 p2) = do
    s <- matchPat p1 p2
    return $ (s1, s2) : s
matchPat (HPApply f1 a1) (HPApply f2 a2) = do
    s1 <- matchPat f1 f2
    s2 <- matchPat a1 a2
    return $ s1 ++ s2
matchPat _ _ = Nothing

hESubst :: [(HSymbol, HSymbol)] -> HExpr -> HExpr
hESubst s (HELam ps e) = HELam (map (hPSubst s) ps) (hESubst s e)
hESubst s (HEApply f a) = HEApply (hESubst s f) (hESubst s a)
hESubst _ e@(HECon _) = e
hESubst s (HEVar v) = HEVar $ maybe v id $ lookup v s
hESubst s (HETuple es) = HETuple (map (hESubst s) es)
hESubst s (HECase e alts) = HECase (hESubst s e) [(hPSubst s p, hESubst s b) | (p, b) <- alts]

hPSubst :: [(HSymbol, HSymbol)] -> HPat -> HPat
hPSubst s (HPVar v) = HPVar $ maybe v id $ lookup v s
hPSubst _ p@(HPCon _) = p
hPSubst s (HPTuple ps) = HPTuple (map (hPSubst s) ps)
hPSubst s (HPAt v p) = HPAt (maybe v id $ lookup v s) (hPSubst s p)
hPSubst s (HPApply f a) = HPApply (hPSubst s f) (hPSubst s a)


termToHClause :: HSymbol -> Term -> Either String HClause
termToHClause name term = toClause `fmap` termToHExpr term
  where
    toClause (HELam patterns expression) =
        HClause name patterns expression
    toClause expression = HClause name [] expression

remUnusedVars :: HExpr -> HExpr
remUnusedVars expr = fst $ remE expr
  -- The auxiliary lists contain every referenced name, including locally bound
  -- ones.  Converter-generated names are globally unique, so an enclosing
  -- pattern can use the list directly to decide which binders are unused.
  where remE (HELam ps e) =
            let (e', vs) = remE e
            in  (HELam (map (remP vs) ps) e', vs)
        remE (HEApply f a) =
            let (f', fs) = remE f
                (a', as) = remE a
            in  (HEApply f' a', fs ++ as)
        remE (HETuple es) =
            let (es', sss) = unzip (map remE es)
            in  (HETuple es', concat sss)
        remE (HECase e alts) =
            let (e', es) = remE e
                (alts', sss) = unzip [ let (ee', ss) = remE ee in ((remP ss p, ee'), ss) | (p, ee) <- alts ]
            in  case alts' of
                [(HPVar "_", b)] -> (b, concat sss)
                _ -> (hECase e' alts', es ++ concat sss)
        remE e@(HECon _) = (e, [])
        remE e@(HEVar v) = (e, [v])
        remP vs p@(HPVar v) = if v `elem` vs then p else HPVar "_"
        remP _vs p@(HPCon _) = p
        remP vs (HPTuple ps) = hPTuple (map (remP vs) ps)
        remP vs (HPAt v p) = if v `elem` vs then HPAt v (remP vs p) else remP vs p
        remP vs (HPApply f a) = HPApply (remP vs f) (remP vs a)
        hPTuple ps | all (== HPVar "_") ps = HPVar "_"
        hPTuple ps = HPTuple ps

getBinderVars :: HClause -> [HSymbol]
getBinderVars (HClause _ pats expr) = concatMap getBinderVarsHP pats ++ getBinderVarsHE expr

getBinderVarsHE :: HExpr -> [HSymbol]
getBinderVarsHE (HELam ps e) = concatMap getBinderVarsHP ps ++ getBinderVarsHE e
getBinderVarsHE (HEApply f a) = getBinderVarsHE f ++ getBinderVarsHE a
getBinderVarsHE (HETuple es) = concatMap getBinderVarsHE es
getBinderVarsHE (HECase se alts) =
    getBinderVarsHE se ++
        concatMap (\ (p, e) -> getBinderVarsHP p ++ getBinderVarsHE e) alts
getBinderVarsHE _ = []

getBinderVarsHP :: HPat -> [HSymbol]
getBinderVarsHP (HPVar s) = [s]
getBinderVarsHP (HPCon _) = []
getBinderVarsHP (HPTuple ps) = concatMap getBinderVarsHP ps
getBinderVarsHP (HPAt s p) = s : getBinderVarsHP p
getBinderVarsHP (HPApply f a) = getBinderVarsHP f ++ getBinderVarsHP a

getAllVars :: HExpr -> [HSymbol]
getAllVars (HELam _ e) = getAllVars e
getAllVars (HEApply f a) = getAllVars f `union` getAllVars a
getAllVars (HETuple es) = foldr union [] (map getAllVars es)
getAllVars (HECase se alts) =
    foldr union (getAllVars se) [ getAllVars e | (_, e) <- alts ]
getAllVars (HEVar s) = [s]
getAllVars _ = []

etaReduce :: HExpr -> HExpr
etaReduce expr = fst $ eta expr
  where eta (HELam [HPVar v] (HEApply f (HEVar v'))) | v == v' && v `notElem` vs = (f', vs)
            where (f', vs) = eta f
        eta (HELam ps e) = (HELam ps e', vs) where (e', vs) = eta e
        eta (HEApply f a) = (HEApply f' a', fvs++avs) where (f', fvs) = eta f; (a', avs) = eta a
        eta e@(HECon _) = (e, [])
        eta e@(HEVar s) = (e, [s])
        eta (HETuple es) = (HETuple es', concat vss) where (es', vss) = unzip $ map eta es
        eta (HECase e alts) = (HECase e' alts', vs ++ concat vss) where (e', vs) = eta e
                                                                        (alts', vss) = unzip $ [ let (a', ss) = eta a in ((p, a'), ss)
                                                                                                 | (p, a) <- alts ]
