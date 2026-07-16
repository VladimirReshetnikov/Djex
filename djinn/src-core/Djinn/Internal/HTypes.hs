{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

--
-- Copyright (c) 2005 Lennart Augustsson
-- See LICENSE for licensing details.
--
module Djinn.Internal.HTypes(
        HKind(KStar, KArrow, KVar),
        HType(HTApp, HTVar, HTCon, HTTuple, HTArrow, HTUnion, HTAbstract),
        HSymbol,
        toSynthesisKind, fromSynthesisKind,
        checkedGroundHKind, fromGroundHKind,
        hTypeSynthesisStructure, fromHTypeSynthesisStructure,
        prepareTypeFormulaTranslator, hTypeToFormula,
        pHSymbol, pHType, pHContext, pHConstraint,
        pHDataType, pHTAtom, pHKind,
        prHSymbolOp, htNot, isHTUnion,
        HClause, HPat, HExpr(HEVar), hPrClause, renderGeneratedClause,
        toGeneratedClause, toGeneratedClauseWithName,
        termToHExpr, termToHClause,
        getBinderVars
    ) where
import Data.Bifunctor (first)
import Data.Void (Void, absurd)
import Text.ParserCombinators.ReadP
import Djinn.Internal.Generated
import Djinn.Internal.HIdentifier
import Djinn.Internal.ProofToGenerated
import Djinn.Internal.LJTFormula
import Djinn.Internal.TypeFormula
import Language.Haskell.Synthesis.Constraint (Constraint(..))
import qualified Language.Haskell.Synthesis.Kind as SharedKind
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Type as SharedType

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

-- | Ground a kind using the bare unsolved-identity diagnostic preserved by
-- Djinn's historical low-level checker and editable-environment API.
checkedGroundHKind :: HKind -> Either String (SharedKind.Kind Void)
checkedGroundHKind = first renderUnsolved
    . SharedKind.groundKind . toSynthesisKind
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

-- | Djinn's historical type vocabulary over the common Djex source-type
-- tree. Ordinary source types store 'SharedType.Type' natively; the bundled
-- patterns preserve @HType(..)@ construction and matching syntax. The two
-- declaration-only forms stay outside that source tree, and a narrow
-- compatibility layer retains constructor-sensitive caller-built values such
-- as malformed names, the noncanonical @(->)@ atom, and an explicit empty
-- 'HTTuple'.
--
-- Consequently parser output and every checked query avoid a second
-- recursive type representation, while malformed raw values still reach the
-- historical checked diagnostic instead of making a compatibility constructor
-- partial.
data HType
        = HTypeSource (SharedType.Type HSymbol)
        | HTypeCompatibility HTypeLayer

data HTypeLayer
        = HTypeApplicationLayer HType HType
        | HTypeVariableLayer HSymbol
        | HTypeConstructorLayer HSymbol
        | HTypeTupleLayer [HType]
        | HTypeArrowLayer HType HType
        | HTypeUnionLayer [(HSymbol, [HType])]
        | HTypeAbstractLayer HSymbol HKind
        deriving (Eq)

-- Equality retains the historical constructor view rather than exposing
-- whether a value arrived as an already-canonical shared tree. In particular,
-- shared canonicalization stores unit as an empty boxed tuple, while Djinn's
-- compatibility view has always projected that nested form back to @HTCon
-- "()"@.
instance Eq HType where
    left == right = case (viewHType left, viewHType right) of
        (Just leftView, Just rightView) -> leftView == rightView
        _ -> False

pattern HTApp :: HType -> HType -> HType
pattern HTApp function argument <-
    (viewHType -> Just (HTypeApplicationLayer function argument))
  where
    HTApp function argument = case
            (hTypeSynthesisStructure function,
             hTypeSynthesisStructure argument) of
        (Just functionType, Just argumentType) -> HTypeSource $
            SharedType.TypeApplication functionType argumentType
        _ -> HTypeCompatibility $ HTypeApplicationLayer function argument

pattern HTVar :: HSymbol -> HType
pattern HTVar variable <-
    (viewHType -> Just (HTypeVariableLayer variable))
  where
    HTVar variable = HTypeSource $ SharedType.TypeVariable variable

pattern HTCon :: HSymbol -> HType
pattern HTCon sourceName <-
    (viewHType -> Just (HTypeConstructorLayer sourceName))
  where
    HTCon sourceName = case sharedConstructorName sourceName of
        Just name -> HTypeSource $ SharedType.TypeConstructor name
        Nothing -> HTypeCompatibility $ HTypeConstructorLayer sourceName

pattern HTTuple :: [HType] -> HType
pattern HTTuple elements <-
    (viewHType -> Just (HTypeTupleLayer elements))
  where
    HTTuple [] = HTypeCompatibility $ HTypeTupleLayer []
    HTTuple elements = case traverse hTypeSynthesisStructure elements of
        Just elementTypes -> HTypeSource $
            SharedType.TupleType SharedName.Boxed elementTypes
        Nothing -> HTypeCompatibility $ HTypeTupleLayer elements

pattern HTArrow :: HType -> HType -> HType
pattern HTArrow parameter result <-
    (viewHType -> Just (HTypeArrowLayer parameter result))
  where
    HTArrow parameter result = case
            (hTypeSynthesisStructure parameter,
             hTypeSynthesisStructure result) of
        (Just parameterType, Just resultType) -> HTypeSource $
            SharedType.FunctionType parameterType resultType
        _ -> HTypeCompatibility $ HTypeArrowLayer parameter result

pattern HTUnion :: [(HSymbol, [HType])] -> HType
pattern HTUnion constructors <-
    (viewHType -> Just (HTypeUnionLayer constructors))
  where
    HTUnion constructors = HTypeCompatibility $ HTypeUnionLayer constructors

pattern HTAbstract :: HSymbol -> HKind -> HType
pattern HTAbstract name kind <-
    (viewHType -> Just (HTypeAbstractLayer name kind))
  where
    HTAbstract name kind = HTypeCompatibility $
        HTypeAbstractLayer name kind

{-# COMPLETE HTApp, HTVar, HTCon, HTTuple, HTArrow, HTUnion, HTAbstract #-}

-- | Obtain the native shared structure of an ordinary compatibility type.
-- Declaration-only and constructor-sensitive fallback forms return 'Nothing'.
-- No validation is performed: raw Djinn boundaries retain their established
-- diagnostic precedence.
hTypeSynthesisStructure :: HType -> Maybe (SharedType.Type HSymbol)
hTypeSynthesisStructure (HTypeSource source) = Just source
hTypeSynthesisStructure _ = Nothing

-- | Wrap a representable shared source type without rebuilding it. The
-- shared tree may still be unchecked; this operation rejects only syntax that
-- Djinn's historical 'HType' cannot express at all.
fromHTypeSynthesisStructure
    :: SharedType.Type HSymbol
    -> Maybe HType
fromHTypeSynthesisStructure source
    | isRepresentable source = Just $ HTypeSource source
    | otherwise = Nothing
  where
    isRepresentable typeExpression = case typeExpression of
        SharedType.TypeVariable{} -> True
        SharedType.TypeConstructor{} -> True
        SharedType.TypeApplication function argument ->
            isRepresentable function && isRepresentable argument
        SharedType.FunctionType parameter result ->
            isRepresentable parameter && isRepresentable result
        SharedType.TupleType SharedName.Boxed elements ->
            all isRepresentable elements
        SharedType.TupleType SharedName.Unboxed _ -> False
        SharedType.ForallType{} -> False

viewHType :: HType -> Maybe HTypeLayer
viewHType source = case source of
    HTypeSource typeExpression -> case typeExpression of
        SharedType.TypeVariable variable ->
            Just $ HTypeVariableLayer variable
        SharedType.TypeConstructor name ->
            Just $ HTypeConstructorLayer $ djinnConstructorSpelling name
        SharedType.TypeApplication function argument ->
            Just $ HTypeApplicationLayer
                (HTypeSource function)
                (HTypeSource argument)
        SharedType.FunctionType parameter result ->
            Just $ HTypeArrowLayer
                (HTypeSource parameter) (HTypeSource result)
        SharedType.TupleType SharedName.Boxed [] ->
            Just $ HTypeConstructorLayer "()"
        SharedType.TupleType SharedName.Boxed elements ->
            Just $ HTypeTupleLayer $ map HTypeSource elements
        -- The private constructor can acquire neither form: every entrance
        -- crosses 'fromHTypeSynthesisStructure' or a bundled pattern above.
        SharedType.TupleType SharedName.Unboxed _ -> Nothing
        SharedType.ForallType{} -> Nothing
    HTypeCompatibility layer -> Just layer

-- Preserve Djinn's historical spelling for the prefix arrow. The shared
-- canonical spelling is @(->)@, but Djinn's parser and formula compiler have
-- always stored @->@.
djinnConstructorSpelling :: SharedName.Name -> HSymbol
djinnConstructorSpelling name
    | SharedName.nameSpecial name == Just SharedName.FunctionConstructor = "->"
    | otherwise = SharedName.renderCanonical name

sharedConstructorName :: HSymbol -> Maybe SharedName.Name
sharedConstructorName "(->)" = Nothing
sharedConstructorName sourceName = case SharedName.parseName sourceName of
    Right name -> Just name
    Left _ -> Nothing

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

-- Keep the prefix spelling `(->) a b` in the same canonical form as `a -> b`.
hTApp :: HType -> HType -> HType
hTApp (HTApp (HTCon "->") a) b = HTArrow a b
hTApp a b = HTApp a b

-------------------------------


prHSymbolOp :: HSymbol -> String
prHSymbolOp = renderVarName

-------------------------------


termToHExpr :: Term -> Either String HExpr
termToHExpr term =
    termToGeneratedExpression term >>= fromGeneratedExpression

termToHClause :: HSymbol -> Term -> Either String HClause
termToHClause name term = do
    expression <- termToGeneratedExpression term
    compatibility <- fromGeneratedExpression expression
    return $ case compatibility of
        HELam patterns body -> HClause name patterns body
        body -> HClause name [] body
