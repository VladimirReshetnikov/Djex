--
-- Copyright (c) 2005 Lennart Augustsson
-- See LICENSE for licensing details.
--
module Djinn.Internal.LJTFormula (
    Symbol(..), Formula(..), (<->), (&), (|:), fnot, false, true,
    formulaSymbols,
    ConsDesc(..), Term(..), applys, validateTermMetadata,
    freeVars, freshenTermBinders
    ) where

import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import Language.Haskell.Synthesis.Collection (distinctOn)

infixr 2 :->
infix  2 <->
infixl 3 |:
infixl 4 &

newtype Symbol = Symbol String
     deriving (Eq, Ord)

instance Show Symbol where
    show (Symbol s) = s

data ConsDesc = ConsDesc String Int     -- name and arity
     deriving (Eq, Ord, Show)

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
x |: y = Disj [((ConsDesc "Left" 1), x), ((ConsDesc "Right" 1), y)]

fnot :: Formula -> Formula
fnot x = x :-> false

false :: Formula
false = Empty $ Symbol "Void"

true :: Formula
true = Conj []

-- Every symbol occurring in a formula: propositional atoms and the nominal
-- tags of empty types.  Freshness machinery reserves all of them, because
-- Symbol is shared by proof variables and propositional atoms.
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
