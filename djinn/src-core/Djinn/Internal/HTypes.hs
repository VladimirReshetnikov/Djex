{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

--
-- Copyright (c) 2005 Lennart Augustsson
-- See LICENSE for licensing details.
--
module Djinn.Internal.HTypes(
        HKind(KStar, KArrow, KVar), HType(..), HSymbol,
        toSynthesisKind, fromSynthesisKind,
        groundHKind, checkedGroundHKind, fromGroundHKind,
        prepareTypeFormulaTranslator, hTypeToFormula,
        pHSymbol, pHType, pHContext, pHConstraint,
        pHDataType, pHTAtom, pHKind,
        prHSymbolOp, htNot, isHTUnion, getHTVars, substHT,
        HClause, HPat, HExpr(HEVar), hPrClause, renderGeneratedClause,
        toGeneratedClause, toGeneratedClauseWithName,
        termToHExpr, termToHClause,
        getBinderVars
    ) where
import Data.Bifunctor (first)
import Data.List(find, transpose, union, (\\))
import Data.Maybe(fromMaybe)
import Control.Monad(foldM, zipWithM)
import qualified Data.Set as Set
import Data.Void (Void, absurd)
import Numeric.Natural (Natural)
import Text.ParserCombinators.ReadP
import Djinn.Internal.Generated
import Djinn.Internal.HIdentifier
import Djinn.Internal.LJTFormula
import Djinn.Internal.TypeFormula
import Language.Haskell.Synthesis.Constraint (Constraint(..))
import Language.Haskell.Synthesis.Fresh (allocateFresh)
import qualified Language.Haskell.Synthesis.Kind as SharedKind
import qualified Language.Haskell.Synthesis.Name as SharedName

type HSymbol = String

-- | Djinn's historical kind vocabulary over the common Djex kind tree.
--
-- A newtype, rather than a type synonym, preserves Djinn's source-like 'Show'
-- contract.  The bundled patterns retain @HKind(..)@ imports and construction
-- syntax while keeping the representation constructor private.
newtype HKind = HKindRepresentation (SharedKind.Kind Int)
    deriving (Eq)

pattern KStar :: HKind
pattern KStar = HKindRepresentation SharedKind.ProperTypeKind

pattern KArrow :: HKind -> HKind -> HKind
pattern KArrow parameter result <-
    HKindRepresentation
        (SharedKind.FunctionKind
            (HKindRepresentation -> parameter)
            (HKindRepresentation -> result))
  where
    KArrow (HKindRepresentation parameter) (HKindRepresentation result) =
        HKindRepresentation $ SharedKind.FunctionKind parameter result

pattern KVar :: Int -> HKind
pattern KVar variable =
    HKindRepresentation (SharedKind.KindVariable variable)

{-# COMPLETE KStar, KArrow, KVar #-}

-- | Project a compatibility kind without recursively rebuilding it.
toSynthesisKind :: HKind -> SharedKind.Kind Int
toSynthesisKind (HKindRepresentation kind) = kind

-- | Wrap a shared kind in Djinn's compatibility rendering contract.
fromSynthesisKind :: SharedKind.Kind Int -> HKind
fromSynthesisKind = HKindRepresentation

-- | Eliminate inference variables while retaining the first unsolved identity.
-- Callers deliberately own its presentation: historical low-level checks show
-- the bare identity, whereas 'Djinn.Core' renders it as @kN@.
groundHKind :: HKind -> Either Int (SharedKind.Kind Void)
groundHKind = SharedKind.groundKind . toSynthesisKind

-- | Ground a kind using the bare unsolved-identity diagnostic preserved by
-- Djinn's historical low-level checker and editable-environment API.
checkedGroundHKind :: HKind -> Either String (SharedKind.Kind Void)
checkedGroundHKind = first renderUnsolved . groundHKind
  where
    renderUnsolved variable =
        "kind contains an unsolved variable: " ++ show variable

-- | Lift a fully solved shared kind back into Djinn's identity domain.
fromGroundHKind :: SharedKind.Kind Void -> HKind
fromGroundHKind = fromSynthesisKind . fmap absurd

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
        | HTUnion [(HSymbol, [HType])] -- Data declarations only; top-level.
        | HTAbstract HSymbol HKind     -- Opaque constructor with a declared kind.
        deriving (Eq)

isHTUnion :: HType -> Bool
isHTUnion (HTUnion _) = True
isHTUnion _ = False

htNot :: HSymbol -> HType
htNot x = HTArrow (HTVar x) (HTCon "Void")

-- Show renders parser-produced types in parseable syntax.  Raw
-- constructions with no Haskell spelling do not round-trip: HTTuple [t]
-- prints as a parenthesized t, HTTuple [] as unit, and HTUnion only makes
-- sense inside a data declaration.  Djinn.Core never builds such values.
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
    ts <- sepBy1 pHTypeApp (sstring "->")
    return $ foldr1 HTArrow ts

-- | Parse Djinn's historical optional query context.  Keeping this token-level
-- parser beside the type grammar lets both the compatibility REPL and the
-- checked Djex adapter consume precisely the same syntax.
pHContext :: ReadP [Constraint HType]
pHContext = do
    contexts <-
        pParen (sepBy1 pHConstraint (schar ','))
        +++ fmap (: []) pHConstraint
    sstring "=>"
    return contexts

pHConstraint :: ReadP (Constraint HType)
pHConstraint = do
    className <- pHSymbol True
    arguments <- many pHTAtom
    case SharedName.parseName className of
        Right name -> return $ Constraint name arguments
        Left _ -> pfail

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
    schar ')'
    return $ HTCon "()"

-- The prefix spelling of the function arrow, "(->)", is lexed like the infix
-- arrow: white space may surround the token but not split it.
pHTCon :: ReadP HType
pHTCon = fmap HTCon pQualifiedConId
       +++
         do pParen (sstring "->"); return (HTCon "->")

pHTVar :: ReadP HType
pHTVar = fmap HTVar (pHSymbol False)

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
    ts <- sepBy1 pHKindA (sstring "->")
    return $ foldr1 KArrow ts

pHKindA :: ReadP HKind
pHKindA = (do schar '*'; return KStar) +++ pParen pHKind

getHTVars :: HType -> [HSymbol]
getHTVars (HTApp f a) = getHTVars f `union` getHTVars a
getHTVars (HTVar v) = [v]
getHTVars (HTCon _) = []
getHTVars (HTTuple ts) = foldr union [] (map getHTVars ts)
getHTVars (HTArrow f a) = getHTVars f `union` getHTVars a
getHTVars (HTUnion ctss) = foldr union [] [ getHTVars t | (_, ts) <- ctss, t <- ts ]
getHTVars (HTAbstract _ _) = []

-------------------------------

-- | Check a raw definition table once and return its checked formula
-- translator. The package-private compiler owns all expansion machinery; this
-- exposed historical module supplies only the legacy one-layer view.
prepareTypeFormulaTranslator
    :: [(HSymbol, ([HSymbol], HType, a))]
    -> Either String (HType -> Either String Formula)
prepareTypeFormulaTranslator definitions = do
    prepared <- prepareFormulaCompiler hTypeFormulaView $
        map hTypeFormulaDefinition definitions
    return $ compileFormula hTypeFormulaView prepared

-- | Checked one-shot convenience wrapper around
-- 'prepareTypeFormulaTranslator'.
hTypeToFormula
    :: [(HSymbol, ([HSymbol], HType, a))]
    -> HType
    -> Either String Formula
hTypeToFormula definitions source =
    prepareTypeFormulaTranslator definitions >>= ($ source)

hTypeFormulaView :: TypeView HType
hTypeFormulaView source = Right $ case source of
    HTApp function argument ->
        TypeApplicationLayer function argument
    HTVar variable -> TypeVariableLayer variable
    HTCon name -> TypeConstructorLayer name
    HTTuple types -> TypeTupleLayer types
    HTArrow argument result -> TypeArrowLayer argument result
    HTUnion constructors -> TypeUnionLayer constructors
    HTAbstract name _ -> TypeAbstractLayer name

hTypeFormulaDefinition
    :: (HSymbol, ([HSymbol], HType, a))
    -> FormulaDefinition HType
hTypeFormulaDefinition (name, (parameters, body, _)) = case body of
    HTUnion constructors -> FormulaData name parameters constructors
    HTAbstract abstractName _ ->
        FormulaAbstract name parameters abstractName
    _ -> FormulaAlias name parameters body

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


prHSymbolOp :: HSymbol -> String
prHSymbolOp = renderVarName

-------------------------------


unSymbol :: Symbol -> HSymbol
unSymbol (Symbol s) = s

termToHExpr :: Term -> Either String HExpr
termToHExpr term = do
    (expression, _) <- conv [] renamedTerm
    let simplified = niceNames $ etaReduce $ remUnusedVars $
            fixSillyAt $ remUnusedVars expression
        allowed = Set.fromList $ map unSymbol $ freeVars term
        escaped = freeHExpr Set.empty simplified `Set.difference` allowed
    if Set.null escaped
        then return simplified
        else Left $ "proof conversion introduced unbound variable(s): " ++
            unwords (Set.toAscList escaped)
  -- Besides the expression, conversion records how enclosing variables are
  -- decomposed.  convV later turns those refinements into tuple/as-patterns.
  where renamedTerm = alphaRenameTerm term
        reservedNames = termNames renamedTerm

        conv _vs (Var s) = Right (HEVar $ unSymbol s, [])
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
                    -- Haskell has no 1-tuples: a unary split matches the
                    -- payload directly (a unary constructor field arrives
                    -- here as Conj [t], mirroring hETuple on expressions).
                    tuplePat [p] = p
                    tuplePat qs = HPTuple qs
                case a' of
                    HEVar v | v `elem` vs && null as ->
                        return (b', [(v, tuplePat ps)] ++ sb ++ sa)
                    _ -> return
                        ( foldl HEApply
                            (hECase a' [(tuplePat ps, b')]) as'
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
                return $ if v == v' then HPAt v merged
                         else HPAt v $ HPAt v' merged
        combPat (HPAt v p) p' = HPAt v `fmap` combPat p p'
        combPat p (HPAt v p') = HPAt v `fmap` combPat p p'
        combPat (HPTuple ps) (HPTuple ps') | length ps == length ps' =
                HPTuple `fmap` zipWithM combPat ps ps'
        combPat p p' = Left $ "cannot merge incompatible patterns " ++
            show p ++ " and " ++ show p'

        cAlt vs (Lam v e) (ConsDesc c n) = do
            let hv = unSymbol v
            (he, ss) <- conv (hv : vs) e
            payloadPattern <- case [p | (owner, p) <- ss, owner == hv] of
                [] -> Right Nothing
                p : ps -> Just `fmap` foldM combPat p ps
            patterns <- case payloadPattern of
                Nothing -> Right $ replicate n (HPVar "_")
                Just pattern' -> unTupleP n pattern'
            let payloadIsUsed = hv `elem` getAllVars he
                branchRefinements = filter ((/= hv) . fst) ss
            if payloadIsUsed
                then do
                    -- A case-handler variable denotes the constructor's
                    -- logical payload: unit for no fields, the field itself
                    -- for one, and a tuple for several.  Haskell patterns bind
                    -- the fields separately, so retain (or introduce) one
                    -- value binder per field and reconstruct that logical
                    -- payload in the body.
                    let (patterns', fields) = payloadValues hv patterns
                        reconstructed = hETuple fields
                        he' = replaceVariable hv reconstructed he
                    return
                        ( (foldl HPApply (HPCon c) patterns', he')
                        , branchRefinements
                        )
                else
                    return
                        ( (foldl HPApply (HPCon c) patterns, he)
                        , branchRefinements
                        )
        cAlt _ handler _ = Left $
            "case alternative is not a lambda: " ++ show handler

        -- Return a pattern and expression for every constructor field.  An
        -- existing variable/as-pattern already names its complete value;
        -- structural and wildcard patterns acquire a fresh as-binder.  The
        -- prefix comes from a globally unique proof binder and candidates are
        -- also checked against every source name, so external assumptions
        -- cannot be captured.
        payloadValues owner = go reservedNames (1 :: Natural)
          where
            go _ _ [] = ([], [])
            go used next (pattern' : patterns) =
                let (pattern'', field, used', next') =
                        patternValue used next pattern'
                    (patterns', fields) = go used' next' patterns
                in (pattern'' : patterns', field : fields)

            patternValue used next pattern' =
                case pattern' of
                    HPVar variable | variable /= "_" ->
                        (pattern', HEVar variable, used, next)
                    HPAt variable _ | variable /= "_" ->
                        (pattern', HEVar variable, used, next)
                    _ ->
                        let (fresh, used', next') = freshField used next
                        in (HPAt fresh pattern', HEVar fresh, used', next')

            freshField used next =
                allocateFresh
                    (\suffix ->
                        (owner ++ "_field" ++ show suffix, suffix + 1))
                    used next

        unLam 0 e = Right ([], e)
        unLam n (HELam patterns e) | length patterns >= n =
            let (used, remaining) = splitAt n patterns
            in Right (used, hELam remaining e)
        unLam n e = Left $ "tuple handler has shape " ++ show e ++
            ", expected " ++ show n ++ " lambda argument(s)"

        hETuple [e] = e
        hETuple es = HETuple es

-- Names present before Haskell conversion.  Extra binders needed to expose a
-- constructor's separate fields must avoid this complete set, not only the
-- variables visible at that particular case alternative.
termNames :: Term -> Set.Set HSymbol
termNames proofTerm =
    case proofTerm of
        Var symbol -> Set.singleton $ unSymbol symbol
        Lam symbol body -> Set.insert (unSymbol symbol) $ termNames body
        Apply function argument ->
            termNames function `Set.union` termNames argument
        Xsel _ _ expression -> termNames expression
        _ -> Set.empty

-- Replace the eliminated logical payload with its Haskell reconstruction.
-- Alpha-renaming makes binders globally unique, but honoring lexical shadowing
-- here keeps this helper correct for independently constructed Haskell ASTs.
replaceVariable :: HSymbol -> HExpr -> HExpr -> HExpr
replaceVariable target replacement = replace
  where
    replace expression =
        case expression of
            HELam patterns body
                | target `Set.member` patternNames patterns -> expression
                | otherwise -> HELam patterns $ replace body
            HEApply function argument ->
                HEApply (replace function) (replace argument)
            constructor@(HECon _) -> constructor
            variable@(HEVar name)
                | name == target -> replacement
                | otherwise -> variable
            HETuple expressions -> HETuple $ map replace expressions
            HECase scrutinee alternatives ->
                HECase (replace scrutinee) $
                    map replaceAlternative alternatives

    replaceAlternative alternative@(pattern', body)
        | target `Set.member` patternNames [pattern'] = alternative
        | otherwise = (pattern', replace body)

-- Lexical free-variable analysis for the post-conversion safety check.  The
-- renderer may simplify or eliminate binders, but it must never invent a free
-- name that was not a free proof assumption.  Empty elimination remains a
-- structural empty case, so it does not smuggle in a magic global dependency.
freeHExpr :: Set.Set HSymbol -> HExpr -> Set.Set HSymbol
freeHExpr bound expression =
    case expression of
        HELam patterns body ->
            freeHExpr (bound `Set.union` patternNames patterns) body
        HEApply function argument ->
            freeHExpr bound function `Set.union` freeHExpr bound argument
        HECon _ -> Set.empty
        HEVar name
            | name `Set.member` bound -> Set.empty
            | otherwise -> Set.singleton name
        HETuple expressions ->
            Set.unions $ map (freeHExpr bound) expressions
        HECase scrutinee alternatives ->
            freeHExpr bound scrutinee `Set.union`
                Set.unions
                    [ freeHExpr
                        (bound `Set.union` patternNames [pattern']) body
                    | (pattern', body) <- alternatives
                    ]

-- The slightly less compact definition makes the branch scope explicit and
-- avoids accidentally putting one alternative's binders in another branch.
patternNames :: [HPat] -> Set.Set HSymbol
patternNames = Set.fromList . filter (/= "_") . concatMap getBinderVarsHP

-- Downstream Haskell-AST rewrites historically assumed that every binder had
-- a globally unique name.  Enforce that invariant even for externally built
-- terms, while retaining all free assumption names verbatim.
alphaRenameTerm :: Term -> Term
alphaRenameTerm term = renamed
  where
    (renamed, _, _) =
        rename [] (Set.fromList $ freeVars term) (1 :: Natural) term

    rename environment used next proofTerm =
        case proofTerm of
            Var symbol ->
                (Var $ fromMaybe symbol $ lookup symbol environment, used, next)
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
        allocateFresh
            (\suffix -> (Symbol $ "__djinn" ++ show suffix, suffix + 1))
            used next

-- Eliminate degenerate as-patterns such as x@y by retaining y and renaming x.
-- This can make all branches of a case visibly equal, so collapse that case in
-- the same bottom-up traversal.
fixSillyAt :: HExpr -> HExpr
fixSillyAt = fixAt []
  where
    fixAt substitutions (HELam patterns expression) =
        let (patterns', renamings) = unzip $ map findSilly patterns
        in HELam patterns' $
            fixAt (concat renamings ++ substitutions) expression
    fixAt substitutions (HEApply function argument) =
        HEApply (fixAt substitutions function) (fixAt substitutions argument)
    fixAt _ expression@(HECon _) = expression
    fixAt substitutions expression@(HEVar variable) =
        maybe expression HEVar $ lookup variable substitutions
    fixAt substitutions (HETuple expressions) =
        HETuple $ map (fixAt substitutions) expressions
    fixAt substitutions (HECase scrutinee alternatives) =
        collapseCase (fixAt substitutions scrutinee) $
            map (fixAlternative substitutions) alternatives

    fixAlternative substitutions (pattern', expression) =
        let (pattern'', renamings) = findSilly pattern'
        in (pattern'', fixAt (renamings ++ substitutions) expression)

    findSilly pattern'@(HPVar _) = (pattern', [])
    findSilly pattern'@(HPCon _) = (pattern', [])
    findSilly (HPTuple patterns) =
        let (patterns', renamings) = unzip $ map findSilly patterns
        in (HPTuple patterns', concat renamings)
    findSilly (HPAt variable pattern') =
        case findSilly pattern' of
            (HPVar "_", renamings) ->
                -- @x@_@ binds x; replacing the complete as-pattern with the
                -- wildcard would turn every use of x into the typed hole @_@.
                (HPVar variable, renamings)
            (pattern''@(HPVar variable'), renamings) ->
                (pattern'', (variable, variable') : renamings)
            (pattern'', renamings) ->
                (HPAt variable pattern'', renamings)
    findSilly (HPApply function argument) =
        let (function', functionRenamings) = findSilly function
            (argument', argumentRenamings) = findSilly argument
        in ( HPApply function' argument'
           , functionRenamings ++ argumentRenamings
           )

    collapseCase scrutinee alternatives =
        case alternatives of
            (pattern', expression) : rest
                | noBoundVariables pattern' &&
                  all (sameUnboundExpression expression) rest -> expression
            _ -> HECase scrutinee alternatives

    sameUnboundExpression expression (pattern', expression') =
        noBoundVariables pattern' && alphaEq expression expression'

    noBoundVariables = all (== "_") . getBinderVarsHP

niceNames :: HExpr -> HExpr
niceNames e =
    let bvars = filter (/= "_") $ getBinderVarsHE e
        nvars = [[c] | c <- ['a'..'z']] ++
            ["x" ++ show i | i <- [1 :: Natural ..]]
        freevars = getAllVars e \\ bvars
        vars = nvars \\ freevars
        sub = zip bvars vars
    in  hESubst sub e

hELam :: [HPat] -> HExpr -> HExpr
hELam [] e = e
hELam ps (HELam ps' e) = HELam (ps ++ ps') e
hELam ps e = HELam ps e

hECase :: HExpr -> [(HPat, HExpr)] -> HExpr
hECase e [] = HECase e []
hECase _ [(HPCon "()", e)] = e
hECase e pes | all (uncurry eqPatExpr) pes = e
hECase e [(p, HELam ps b)] = HELam ps $ hECase e [(p, b)]
hECase se alts@((_, firstExpression@(HELam _ _)):rest) | m > 0 =
    HELam (map HPVar canonicalNames) $ hECase se alts'
  where -- The pattern supplies a nonempty seed, so this fold is total while
        -- retaining the minimum common lambda prefix across all alternatives.
        m = foldr (min . numBind . snd) (numBind firstExpression) rest
        numBind (HELam ps _) = length (takeWhile isPVar ps)
        numBind _ = 0
        isPVar (HPVar _) = True
        isPVar _ = False
        alts' = [ let (ps1, ps2) = splitAt m ps
                      -- A wildcard is not a binder and must never become
                      -- either side of a variable substitution.
                      sub = [ (source, target)
                            | (HPVar source, target) <-
                                zip ps1 canonicalNames
                            , source /= "_"
                            , source /= target
                            ]
                  in (cps, hELam ps2 $ hESubst sub e)
                  | (cps, HELam ps e) <- alts ]
        -- Commute the common lambda prefix out of the case.  Binder names are
        -- globally unique here, so any live name in a column is a safe common
        -- spelling.  Choosing from all branches is essential: taking the
        -- first branch's @_@ used to rewrite a later live occurrence to a
        -- typed hole.
        binderColumns = transpose
            [ take m ps | (_, HELam ps _) <- alts ]
        canonicalNames = map canonicalName binderColumns
        canonicalName patterns = fromMaybe "_" $
            find (/= "_") [ name | HPVar name <- patterns ]
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
hESubst s (HEVar v) = HEVar $ fromMaybe v $ lookup v s
hESubst s (HETuple es) = HETuple (map (hESubst s) es)
hESubst s (HECase e alts) = HECase (hESubst s e) [(hPSubst s p, hESubst s b) | (p, b) <- alts]

hPSubst :: [(HSymbol, HSymbol)] -> HPat -> HPat
hPSubst s (HPVar v) = HPVar $ fromMaybe v $ lookup v s
hPSubst _ p@(HPCon _) = p
hPSubst s (HPTuple ps) = HPTuple (map (hPSubst s) ps)
hPSubst s (HPAt v p) = HPAt (fromMaybe v $ lookup v s) (hPSubst s p)
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

getAllVars :: HExpr -> [HSymbol]
getAllVars (HELam _ e) = getAllVars e
getAllVars (HEApply f a) = getAllVars f `union` getAllVars a
getAllVars (HETuple es) = foldr union [] (map getAllVars es)
getAllVars (HECase se alts) =
    foldr union (getAllVars se) [ getAllVars e | (_, e) <- alts ]
getAllVars (HEVar s) = [s]
getAllVars _ = []

-- Rewrite \ v -> f v to f when v does not occur free in f.  Each traversal
-- returns the variables referenced by the rewritten expression so the guard
-- can make that occurs check without a second pass.
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
