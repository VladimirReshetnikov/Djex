--
-- Copyright (c) 2005 Lennart Augustsson
-- See LICENSE for licensing details.
--

-- | The object language of the LJT prover: propositional 'Formula's over
-- 'Symbol' atoms, and the untyped proof 'Term's the search produces.  A
-- 'Symbol' names both proof variables and atoms; an opaque type atom
-- additionally seals a shared source type with alpha-normal identity.
-- "Djinn.Internal.LJT" re-exports this module and searches over it, while
-- "Djinn.Internal.TypeFormula" compiles source types into these formulae.
module Djinn.Internal.LJTFormula (
    Symbol(Symbol), opaqueTypeSymbol, opaqueSymbolSource, symbolSpelling,
    Formula(..), (<->), (&), (|:), fnot, false, true,
    formulaSymbols,
    ConsDesc(..), Term(..), applys, validateTermMetadata,
    freeVars, freshenTermBinders
    ) where

import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import Language.Haskell.Synthesis.Collection (distinctOn)
import Language.Haskell.Synthesis.Type (Type)
import Language.Haskell.Synthesis.TypeAtom (TypeAtomKey, alphaTypeKey)
import qualified Language.Haskell.Synthesis.TypeRender as SharedTypeRender

infixr 2 :->
infix  2 <->
infixl 3 |:
infixl 4 &

-- | A name in the LJT calculus, used both for proof variables and for
-- propositional atoms.
-- Proof variables and ordinary historical atoms retain their source spelling.
-- An opaque type atom additionally carries the exact shared source tree for
-- display and an alpha-normal key for logical identity. Keeping those roles in
-- one sum lets the existing LJT indexes stay unchanged without ever treating a
-- rendered forall spelling as its proposition key.
data Symbol
    = Symbol String
    | OpaqueTypeSymbol !(Type String) !(TypeAtomKey String)

instance Eq Symbol where
    Symbol left == Symbol right = left == right
    OpaqueTypeSymbol _ left == OpaqueTypeSymbol _ right = left == right
    _ == _ = False

instance Ord Symbol where
    compare (Symbol left) (Symbol right) = compare left right
    compare Symbol{} OpaqueTypeSymbol{} = LT
    compare OpaqueTypeSymbol{} Symbol{} = GT
    compare (OpaqueTypeSymbol _ left) (OpaqueTypeSymbol _ right) =
        compare left right

-- | Seal any shared source type as one opaque logical proposition. The caller
-- decides which enclosing structure remains logical; this operation merely
-- gives the selected subtree alpha-aware identity.
opaqueTypeSymbol :: Type String -> Symbol
opaqueTypeSymbol source = OpaqueTypeSymbol source $ alphaTypeKey source

-- | Project the exact shared source tree sealed inside an opaque atom.
-- Ordinary propositional atoms carry only a spelling and yield 'Nothing'.
-- Instantiation policy uses this to recover a quantified hypothesis type
-- without reparsing rendered text.
opaqueSymbolSource :: Symbol -> Maybe (Type String)
opaqueSymbolSource symbol = case symbol of
    Symbol _ -> Nothing
    OpaqueTypeSymbol source _ -> Just source

-- | Source-like display spelling. Proof terms contain only the ordinary
-- constructor, but keeping this projection total makes invariant failures
-- diagnosable instead of turning proof lowering into a partial match.
symbolSpelling :: Symbol -> String
symbolSpelling symbol = case symbol of
    Symbol spelling -> spelling
    OpaqueTypeSymbol source _ -> SharedTypeRender.renderType id source

instance Show Symbol where
    show = symbolSpelling

-- | A data constructor as seen by the prover: its name and its arity.
-- Disjunction alternatives, injections, and case eliminators are tagged with
-- one so proofs can be lowered back to named Haskell constructors.
data ConsDesc = ConsDesc String Int     -- name and arity
     deriving (Eq, Ord, Show)

-- | An intuitionistic propositional formula: an n-ary conjunction (@Conj []@
-- is 'true'), a constructor-tagged disjunction, a nominally distinct empty
-- type ('Empty', logically false), an implication, or an atom.  Implication
-- @:->@ associates to the right.
data Formula
        = Conj [Formula]
        | Disj [(ConsDesc, Formula)]
        -- Empty datatypes are logically false but remain nominally distinct.
        | Empty Symbol
        | Formula :-> Formula
        | PVar Symbol
     deriving (Eq, Ord)

(<->) :: Formula -> Formula -> Formula
x <-> y = (x:->y) & (y:->x)

(&) :: Formula -> Formula -> Formula
x & y = Conj [x, y]

(|:) :: Formula -> Formula -> Formula
x |: y = Disj [(ConsDesc "Left" 1, x), (ConsDesc "Right" 1, y)]

-- | Intuitionistic negation: the implication from the formula to 'false'.
fnot :: Formula -> Formula
fnot x = x :-> false

-- | Falsity, represented as the empty type named @Void@.
false :: Formula
false = Empty $ Symbol "Void"

-- | Truth, represented as the empty conjunction.
true :: Formula
true = Conj []

-- | Every symbol occurring in a formula: propositional atoms and the nominal
-- tags of empty types. Freshness machinery reserves all of them. An opaque
-- type symbol is structurally disjoint from every generated spelling, so it
-- cannot collide with a proof binder even when its rendered text does.
formulaSymbols :: Formula -> [Symbol]
formulaSymbols (Conj fs) = concatMap formulaSymbols fs
formulaSymbols (Disj alternatives) = concatMap (formulaSymbols . snd) alternatives
formulaSymbols (Empty name) = [name]
formulaSymbols (a :-> b) = formulaSymbols a ++ formulaSymbols b
formulaSymbols (PVar s) = [s]

-- Show formulae the LJT way
instance Show Formula where
    showsPrec _ (Conj []) = showString "true"
    showsPrec _ (Conj [c]) = showParen True $ showString "&" . showsPrec 0 c
    showsPrec p (Conj (c : cs)) =
        showParen (p > 40) $
            showsPrec 41 c . foldr showConjunct id cs
      where
        showConjunct f rest = showString " & " . showsPrec 41 f . rest
    showsPrec _ (Disj []) = showString "false"
    showsPrec _ (Disj [(_, c)]) = showParen True $ showString "|" . showsPrec 0 c
    showsPrec p (Disj ((_, d) : ds)) =
        showParen (p > 30) $
            showsPrec 31 d . foldr showDisjunct id ds
      where
        showDisjunct (_, f) rest = showString " v " . showsPrec 31 f . rest
    showsPrec _ (Empty name) =
        showString "false[" . showsPrec 0 name . showString "]"
    showsPrec _ (f1 :-> Empty _) =
        showString "~" . showsPrec 100 f1
    showsPrec p (f1 :-> f2) =
        showParen (p > 20) $
            showsPrec 21 f1 . showString " -> " . showsPrec 20 f2
    showsPrec p (PVar s) = showsPrec p s

------------------------------

-- | A proof term of the LJT calculus: an untyped lambda calculus over
-- 'Symbol' variables extended with constants for building and splitting
-- n-tuples ('Ctuple', 'Csplit'), injecting into and case-analysing
-- constructor-tagged sums ('Cinj', 'Ccases'), and a legacy tuple selector.
-- Constants take their operands through ordinary 'Apply' spines.
data Term
        = Var Symbol
        | Lam Symbol Term
        | Apply Term Term
        | Ctuple Int
        | Csplit Int
        | Cinj ConsDesc Int
        | Ccases [ConsDesc]
        -- Legacy selector retained for compatibility; the prover no longer
        -- constructs it.
        | Xsel Int Int Term
    deriving (Eq, Ord)

instance Show Term where
    showsPrec p (Var s) = showsPrec p s
    showsPrec p (Lam s e) =
        showParen (p > 0) $
            showString "\\" . showsPrec 0 s . showString "." . showsPrec 0 e
    showsPrec p (Apply f a) =
        showParen (p > 1) $ showsPrec 1 f . showString " " . showsPrec 2 a
    showsPrec _ (Cinj _ i) = showString $ "Inj" ++ show i
    showsPrec _ (Ctuple i) = showString $ "Tuple" ++ show i
    showsPrec _ (Csplit n) = showString $ "split" ++ show n
    showsPrec _ (Ccases cds) = showString $ "cases" ++ show (length cds)
    showsPrec p (Xsel i n e) =
        showParen (p > 0) $
            showString ("sel_" ++ show i ++ "_" ++ show n ++ " ") . showsPrec 2 e

-- | Apply a term to a list of arguments in order (left-nested 'Apply').
applys :: Term -> [Term] -> Term
applys = foldl' Apply

-- | Validate the representation-local metadata of a raw proof term.
--
-- This boundary deliberately needs neither a proof environment nor an
-- expected formula. It can therefore reject negative arities and injection
-- indices before either independent checking or generated-code conversion
-- interprets them. Whether a non-negative injection index exists in a
-- particular sum remains a proof-type question owned by @checkProof@.
validateTermMetadata :: Term -> Either String ()
validateTermMetadata term = case term of
    Var{} -> Right ()
    Lam _ body -> validateTermMetadata body
    Apply function argument ->
        validateTermMetadata function >> validateTermMetadata argument
    Ctuple arity -> validateNonNegative "tuple arity" arity
    Csplit arity -> validateNonNegative "split arity" arity
    Cinj constructor index -> do
        validateConstructorMetadata constructor
        validateNonNegative "injection index" index
    Ccases constructors -> mapM_ validateConstructorMetadata constructors
    -- The legacy selector has no proof-type or generated-code semantics. Its
    -- owning consumer reports that unsupported node without inspecting fields
    -- which cannot affect the result.
    Xsel{} -> Right ()

validateConstructorMetadata :: ConsDesc -> Either String ()
validateConstructorMetadata (ConsDesc name arity)
    | null name = Left "constructor descriptor has an empty name"
    | otherwise = validateNonNegative "constructor arity" arity

validateNonNegative :: String -> Int -> Either String ()
validateNonNegative description value
    | value < 0 = Left $
        description ++ " is negative: " ++ show value
    | otherwise = Right ()

-- | Free variables in first-occurrence order, each reported once.
freeVars :: Term -> [Symbol]
freeVars = distinctOn id . occurrences Set.empty
  where
    occurrences bound term = case term of
        Var s | Set.member s bound -> []
              | otherwise -> [s]
        Lam s e -> occurrences (Set.insert s bound) e
        Apply f a -> occurrences bound f ++ occurrences bound a
        Xsel _ _ e -> occurrences bound e
        _ -> []

-- | Rename every lambda binder to a fresh symbol from the supplied
-- allocator, renaming occurrences through ordinary shadowing while leaving
-- free variables intact. The prover copies substituted proof fragments
-- through this traversal so binders stay globally unique, and generated
-- output lowering freshens complete proofs with the same operation.
freshenTermBinders :: Monad m => m Symbol -> Term -> m Term
freshenTermBinders freshSymbol = go []
  where
    go renamings (Var s) = return $ Var $ fromMaybe s $ lookup s renamings
    go renamings (Lam s body) = do
        s' <- freshSymbol
        Lam s' <$> go ((s, s') : renamings) body
    go renamings (Apply f a) = Apply <$> go renamings f <*> go renamings a
    go renamings (Xsel i n e) = Xsel i n <$> go renamings e
    go _ t = return t
