{-
  Church-encoded Data Structures and Operations in Haskell
  --------------------------------------------------------

  This module provides an elegant demonstration of emulation of
  algebraic data types (ADTs) using Church encoding in Haskell.

  What is Church Encoding?
  ------------------------
  Church encoding is a representation technique that transforms data types into
  higher-order functions, eliminating the need for primitive data constructors.
  Named after Alonzo Church, it represents data solely through their elimination
  forms—what you can do with the data rather than how it's constructed.

  In the context of Haskell's ADTs, Church encoding replaces:
  - Data constructors with higher-order functions
  - Pattern matching with function application
  - Recursion with higher-order combinators

  For example, a "Pair (a,b)" becomes a function that accepts another function
  and applies it to both elements: λf -> f a b.

  Why Rank-N Types Are Necessary:
  -------------------------------
  Church encodings require rank-N polymorphism (specifically rank-2 types)
  because the encoded type must universally quantify over the return type.
  Consider the Church-encoded Bool type:

    type Bool = ∀e. e -> e -> e

  The crucial "∀e." appears inside the type definition (not just at the top level),
  allowing the consumer of a Bool to choose the return type. Without this, we
  couldn't encode data that preserves parametricity and prevents inspection of
  the encoding's internal representation. `ImpredicativeTypes` and
  `ScopedTypeVariables` extensions enable these sophisticated type abstractions
  in Haskell.

  Purpose and Benefits:
  ---------------------
  - Educational exploration showcasing Church encodings, polymorphism,
    continuations, and advanced higher-order programming techniques.
  - Highlights Haskell's powerful type-system features (`ScopedTypeVariables`,
    `ImpredicativeTypes`).
  - Serves as an insightful reference for foundational functional programming
    concepts.

  Guiding GHC's Type Inference with Rank-N Polymorphism and Partial Type Signatures
  ----------------------------------------------------------------------------------

  Church-encoded data structures, with their inherent reliance on higher-order
  polymorphism, occasionally require manual assistance to guide GHC's type inference.
  Explicit local type signatures, often partially specified using underscores (`_`)
  via the `PartialTypeSignatures` extension, provide the compiler required hints
  to resolve complex higher-order polymorphic types.

  Tests for this module are in test/Spec.hs.

  ⚠️ IMPORTANT: No ADTs like `Bool`, `(,)` or `[]` are allowed in this module, even in
  local/helper functions and variables (excluding test code). Everything must be
  implemented using purely functional Church encodings (rank-N types). Functions
  may not delegate to Prelude functions that use ADTs. The only exception is the
  `Int` type, which is used in the provided helper functions for basic operations.
  Use of `undefined` and `error` is also allowed, because it doesn't involve ADTs.
  Also, the use of if-then-else is prohibited.
-}
{-# LANGUAGE UnicodeSyntax, TypeOperators, ScopedTypeVariables, ImpredicativeTypes, PartialTypeSignatures, TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -fno-max-relevant-binds #-}

module Church where

import Prelude
  ( Int, otherwise, (<=), (+), (-), (*), succ
  , const, id, flip, (.), ($)
  , undefined, error, liftA2, (<*>)
  )

------------------------------------------------------------
-- 0. Prerequisites
------------------------------------------------------------

ltInt :: Int -> Int -> Bool
ltInt = ltFromLE leInt

gtInt :: Int -> Int -> Bool
gtInt = flip ltInt

leInt :: Int -> Int -> Bool
leInt x y
  | x <= y    = true
  | otherwise = false

eqInt :: Int -> Int -> Bool
eqInt = eqFromLE leInt

geInt :: Int -> Int -> Bool
geInt = flip leInt

------------------------------------------------------------
-- 1. Church-Encoded Types
------------------------------------------------------------

-- | Church-encoded Boolean type
type Bool       = ∀e. e -> e -> e

-- | Church-encoded tuple type with two elements, corresponds to (a, b)
type Pair a b   = ∀e. (a -> b -> e) -> e

-- | Church-encoded list type, corresponds to [a]
type List a     = ∀e. (a -> e -> e) -> e -> e

-- | Church-encoded Maybe type, corresponds to `Prelude.Maybe a`
type Maybe a    = ∀e. e -> (a -> e) -> e

-- | Church-encoded Either type, corresponds to `Prelude.Either a b`
type Either a b = ∀e. (a -> e) -> (b -> e) -> e

------------------------------------------------------------
-- 2. Aliases with Designated Semantics
------------------------------------------------------------

-- | 'Tuple' represents a list of elements, whose instances are always
--   of the same length (although Haskell's type system cannot enforce
--   this constraint). It's used to represent homogenous tuples such as
--   a pair, triple, or quadruple, and allows to uniformly implement
--   operations such as 'zipWithN' on them.
--   A 'Tuple' is assumed to be non-empty.
type Tuple a = List a

-- 'Dict' is a type synonym for a list of key-value pairs.
-- Each key-value pair is represented as a 'Pair' of key and value.
-- Keys must be unique according to whatever equality relation will be used.
-- When using this type alias, users should assume that the order of key-value
-- pairs is not significant.
type Dict k v = List (k `Pair` v)

-- 'Equal' is a type synonym for a function that compares two values of the same
-- type for equality. For example, it is used to compare keys in 'Dict' operations.
type Equal a = a -> a -> Bool

-- 'LE' is a type synonym for a function that compares two values of the same type
-- for less-than-or-equal. Equality can be expressed in terms of less-than-or-equal,
-- by defining a = b as a ≤ b and b ≤ a. From the typesystem perspective, this
-- is exactly the same as 'Equal', but its purpose is different. Use of this type
-- alias indicates that the function is used for ordering comparisons.
type LE a = a -> a -> Bool

eqFromLE :: LE a -> Equal a
eqFromLE le x y = le x y (le y x) false

-- For a total less-than-or-equal, x < y is exactly ¬(y ≤ x).
ltFromLE :: LE a -> a -> a -> Bool
ltFromLE le x y = not $ le y x

-- 'Matrix' is a type synonym for a list of lists of values.
-- Each inner list represents a row in the matrix.
-- The outer list represents the collection of rows.
-- The matrix must be rectangular, meaning that all rows have the same length.
type Matrix a = List (List a)

------------------------------------------------------------
-- 3. Church-Encoded Booleans
------------------------------------------------------------

true ∷ Bool
true = const

false ∷ Bool
false = const id

not ∷ Bool -> Bool
not b = b false true

and ∷ Bool -> Bool -> Bool
and b = flip b false

or ∷ Bool -> Bool -> Bool
or b₁ = b₁ true

xor ∷ Bool -> Bool -> Bool
xor b = b not id

------------------------------------------------------------
-- 4. Church-Encoded Pairs
------------------------------------------------------------

pair ∷ a -> b -> a `Pair` b
pair x y f = f x y

fst ∷ a `Pair` _ -> a
fst = uncurry const

snd ∷ _ `Pair` b -> b
snd = uncurry $ const id

swap ∷ a `Pair` b -> b `Pair` a
swap = uncurry $ flip pair

curry ∷ (a `Pair` b -> c) -> a -> b -> c
curry f x = f . pair x

uncurry ∷ _ -> a `Pair` b -> c
uncurry f p = p f

pairToList ∷ a `Pair` b -> List (Either a b)
pairToList = uncurry $ \x y -> left x `cons` singleton (right y)

bimapPair :: (a -> c) -> _ -> a `Pair` b -> c `Pair` d
bimapPair f g = uncurry $ \x y -> f x `pair` g y

------------------------------------------------------------
-- 5. Church-Encoded Lists
------------------------------------------------------------

-- 5.1 Constructors and Basic Operations
----------------------------------------

-- 'nil' represents an empty list.
nil ∷ List a
nil = const id

-- 'cons' prepends an element to a list.
cons ∷ a -> List a -> List a
cons x xs c = c x . xs c

-- 'snoc' appends an element to a list.
snoc ∷ List a -> a -> List a
snoc xs x c = xs c . c x

uncons ∷ List a -> Maybe (a `Pair` List a)
uncons = caseList nothing (curry just)

unsnoc ∷ List a -> Maybe (List a `Pair` a)
unsnoc xs =
  null xs
    nothing
    $ just $ init xs `pair` last xs

singleton ∷ a -> List a
singleton x = ($ x)

-- | One-step case analysis in the style of 'Prelude.maybe': @caseList z f xs@
--   is @z@ when @xs@ is 'nil', and @f (head xs) (tail xs)@ otherwise.
--   It is the workhorse eliminator behind most non-fold list functions here.
caseList ∷ b -> (a -> List a -> b) -> List a -> b
caseList z f xs = null xs z $ f (head xs) $ tail xs

------------------------------------------------------------
-- 5.2 Sublist Access: head, tail, etc.
------------------------------------------------------------

head ∷ List a -> a
head = foldr const undefined

null ∷ List a -> Bool
null = foldr (const $ const false) true

tail ∷ List a -> List a
tail xs cons' nil' =
  xs
    (\x r skip -> skip (r false) (x `cons'` r false))
    (const nil')
    true

last ∷ List a -> a
last = foldl (flip const) undefined

init ∷ List a -> List a
init = liftA2 (zipWith const) id tail

at :: Int -> List a -> a
at n =
  ltInt n 0
    (error "at: negative index")
    (caseList (error "at: index out of bounds") const . drop n)

any :: (a -> Bool) -> List a -> Bool
any p = foldr (or . p) false

all :: (a -> Bool) -> List a -> Bool
all p = foldr (and . p) true

------------------------------------------------------------
-- 5.3 Folds, Scans, and General List Utilities
------------------------------------------------------------

foldr ∷ (a -> b -> b) -> b -> List a -> b
foldr f z xs = xs f z

foldl ∷ _ -> b -> List a -> b
foldl f z xs = xs (flip (.) . flip f) id z

foldl1 :: _ -> List a -> a
foldl1 = caseList (error "foldl1: empty list") . foldl

foldr1 :: _ -> List a -> a
foldr1 f xs = foldr (\x m -> just (m x (f x))) nothing xs
  (error "foldr1: empty list") id

scanr ∷ (a -> b -> b) -> b -> List a -> List b
scanr f q = foldr (\x p -> f x (head p) `cons` p) $ singleton q

-- | 'mapAccumR' applies a function to each element of a list, threading an
--   accumulator through the computation and collecting the results.
--   The function returns the final accumulator and a list of results.
mapAccumR ∷ (s -> a -> s `Pair` b) -> s -> List a -> s `Pair` List b
mapAccumR f s = bimapPair id reverse . mapAccumL f s . reverse

-- | 'scanl' is similar to 'foldl', but returns a list of successive reduced values
--   from the left. The resulting list will have one more element than the input list.
scanl ∷ _ -> b -> List a -> List b
scanl f q = cons q . caseList nil (scanl f . f q)

-- | 'mapAccumL' is similar to 'mapAccumR', but it processes the list from left to right.
--   It applies a function to each element of a list, threading an accumulator through the
--   computation and collecting the results. The function returns the final accumulator
--   and a list of results.
mapAccumL ∷ (s -> a -> s `Pair` b) -> s -> List a -> s `Pair` (List b)
mapAccumL f s = foldl
  (\acc x -> acc $ \s₁ bs -> bimapPair id (snoc bs) $ f s₁ x)
  $ s `pair` nil

-- | 'zipWith' combines two lists by applying a function to pairs of corresponding elements
--   from the two lists. The resulting list has the same length as the shorter
--   of the two input lists.
zipWith ∷ _ -> List a₁ -> List a₂ -> List b
zipWith f xs ys = caseList nil (\x xs' ->
  caseList nil (\y ys' -> f x y `cons` zipWith f xs' ys') ys) xs

-- | 'zipWithN' is a generalization of 'zipWith' that applies a function to a homogenous tuple
--   of elements from a tuple of lists. The resulting list has the same length as the shortest
--   list in the tuple.
zipWithN :: (Tuple a -> b) -> Tuple (List a) -> List b
zipWithN f = map f . transpose

-- | 'scanl2' effectively combines `scanl` with `zipWith` by applying a binary
--   function to pairs of elements from two lists, threading an accumulator
--   through the computation. The two lists are scanned in parallel left-to-right, and the
--   resulting list has length one greater than the shorter of the two input lists.
scanl2 ∷ _ -> b -> List a₁ -> List a₂ -> List b
scanl2 f q xs = scanl (uncurry . f) q . zip xs

-- | 'scanlN' is a generalization of 'scanl2' that applies a function to homogenous tuples
--   of elements from a tuple of lists, threading an accumulator through the computation.
--   The lists are scanned in parallel left-to-right, and the resulting list has length one
--   greater than the shortest list in the tuple.
--   It is effectively a combination of 'scanl' and 'zipWithN'.
--
-- NOTE: From the type system's point of view, `Tuple` is just an alias for `List`. But it
-- serves as a documentation of intent: all instances are supposed to be of the same length,
-- and they are interpeted as tuples. Haskell type system restricts this representation to
-- homogenous tuples.
scanlN ∷ (b -> Tuple a -> b) -> b -> Tuple (List a) -> List b
scanlN f q = scanl f q . transpose

-- | 'mapAccumL2' is a generalization of 'mapAccumL' that applies a function to pairs of
--   elements from two lists, threading an accumulator through the computation and collecting
--   the results. The function returns the final accumulator and a list of results. The lists
--   are scanned in parallel left-to-right, and the resulting list has length equal to the
--   shorter of the two input lists.
mapAccumL2 ∷ (s -> a₁ -> a₂ -> s `Pair` b) -> s -> List a₁ -> List a₂ -> s `Pair` List b
mapAccumL2 f s xs = mapAccumL (uncurry . f) s . zip xs

-- | 'mapAccumLN' is a generalization of 'mapAccumL2' that applies a function to homogenous tuples of
--   elements from a tuple of lists, threading an accumulator through the computation and collecting
--   the results. The function returns the final accumulator and a list of results. The lists
--   are scanned in parallel left-to-right, and the resulting list has length equal to the
--   shortest list in the tuple.
mapAccumLN ∷ (s -> Tuple a -> s `Pair` b) -> s -> Tuple (List a) -> s `Pair` List b
mapAccumLN f s = mapAccumL f s . transpose

zip ∷ List a -> List b -> List (a `Pair` b)
zip = zipWith pair

unzip ∷ List (a `Pair` b) -> List a `Pair` List b
unzip = liftA2 pair (map fst) (map snd)

-- | 'unfoldr' builds a list from a seed value by repeatedly applying a
--   function to generate the next element and seed. The function returns
--   'Nothing' when there are no more elements to generate.
unfoldr ∷ (b -> Maybe (a `Pair` b)) -> b -> List a
unfoldr f s = f s nil $ uncurry $ \x s' -> x `cons` unfoldr f s'

-- | 'unfoldTree' traverses a virtual tree structure starting from a seed value,
--   generating a list of elements in the process. Similar to 'unfoldr',
--   which builds a list by repeatedly applying a function, 'unfoldTree'
--   can branch out and traverse multiple paths from each node.
unfoldTree ∷ (b -> a `Pair` List b) -> b -> List a
unfoldTree f s = f s $ \x seeds -> x `cons` concatMap (unfoldTree f) seeds

-- An equivalent of (++)
append ∷ List a -> List a -> List a
append xs = xs cons

appendEither ∷ List a -> List b -> List (Either a b)
appendEither xs ys = map left xs `append` map right ys

map ∷ _ -> List a -> List b
map f xs c = xs $ c . f

concat ∷ List (List a) -> List a
concat = foldr append nil

concatMap ∷ (a -> List b) -> List a -> List b
concatMap f = concat . map f

filter ∷ (a -> Bool) -> List a -> List a
filter p = concatMap $ \x -> p x (singleton x) nil

length ∷ List a -> Int
length = foldr (const succ) 0

reverse ∷ List a -> List a
reverse = foldl (flip cons) nil

take :: Int -> List a -> List a
take n xs = zipWith const xs $ replicate n undefined

drop :: Int -> List a -> List a
drop n = compose $ replicate n tail

splitAt :: Int -> List a -> List a `Pair` List a
splitAt = liftA2 (liftA2 pair) take drop

takeLast :: Int -> List a -> List a
takeLast n = reverse . take n . reverse

dropLast :: Int -> List a -> List a
dropLast n = reverse . drop n . reverse

takeWhile ∷ (a -> Bool) -> List a -> List a
takeWhile p = foldr (\x acc -> p x (x `cons` acc) nil) nil

dropWhile ∷ (a -> Bool) -> List a -> List a
dropWhile p xs = caseList nil (\x xs' -> p x (dropWhile p xs') xs) xs

-- | 'partition' splits a list into two lists: those elements that satisfy
--   the given predicate, and those that do not.
partition ∷ (a -> Bool) -> List a -> List a `Pair` List a
partition p xs = filter p xs `pair` filter (not . p) xs

-- | 'partitionEvery' is the non-overlapping, complete-block case of
--   Wolfram Language 'Partition[list, n]'. A trailing short block is discarded.
partitionEvery :: Int -> List a -> List (List a)
partitionEvery n = partitionStep n n

-- | 'partitionStep' is the complete-block analogue of
--   Wolfram Language 'Partition[list, n, step]'.
partitionStep :: Int -> Int -> List a -> List (List a)
partitionStep n step xs =
  gtInt n 0
    (gtInt step 0
       (leInt n (length xs)
          (take n xs `cons` partitionStep n step (drop step xs))
          nil)
       nil)
    nil

-- | Sliding windows: 'windows n' is 'partitionStep n 1'.
windows :: Int -> List a -> List (List a)
windows = flip partitionStep 1

-- | 'inits' returns all initial segments of the list, shortest first.
inits :: List a -> List (List a)
inits = scanl snoc nil

tails :: ∀a. List a -> List (List a)
tails = scanr @a @(List a) cons nil

-- | 'span' splits a list into two parts: the longest prefix that satisfies
--   a predicate and the rest of the list.
span :: (a -> Bool) -> List a -> List a `Pair` List a
span = liftA2 (liftA2 pair) takeWhile dropWhile

replicate :: Int -> a -> List a
replicate n x = leInt n 0 nil (x `cons` replicate (n - 1) x)

-- | 'lexicographicLE' compares two lists lexicographically using a
--   custom less-than-or-equal function.
lexicographicLE :: LE a -> LE (List a)
lexicographicLE le xs ys =
  caseList true (\x xs' ->
    caseList false (\y ys' ->
      le x y
        (le y x (lexicographicLE le xs' ys') true)
        false) ys) xs

-- | Composes a list of endomorphisms (functions a -> a).
--   Applies them in right-to-left order: compose [f,g,h] x = f (g (h x)).
compose :: List (a -> a) -> a -> a
compose = foldr (.) id

-- | 'composeFlatten' composes a list of functions producing lists,
--   flattening the result. It's related to 'concatMap' and 'compose'.
composeFlatten :: ∀a. List (a -> List a) -> a -> List a
composeFlatten = foldr @(a -> List a) (\f g -> concatMap g . f) singleton

-- | 'cartesianWith' computes the Cartesian product of two lists
--   using a custom pairing function.
cartesianWith :: _ -> List a -> List b -> List c
cartesianWith f xs ys = concatMap (flip map ys . f) xs

-- | 'cartesian' computes the Cartesian product of two lists.
--   It pairs each element of the first list with each element of
--   the second list, producing a list of pairs.
cartesian :: List a -> List b -> List (a `Pair` b)
cartesian = cartesianWith pair

-- | 'cartesianN' computes the Cartesian product of a tuple of lists,
--   producing a list of tuples. It creates a list of all possible
--   combinations of elements from the input lists, selecting exactly
--   one element from each list.
cartesianN :: Tuple (List a) -> List (Tuple a)
cartesianN = foldl (cartesianWith snoc) (singleton nil)

intersperse :: a -> List a -> List a
intersperse sep = tail . concatMap (cons sep . singleton)

-- | Wolfram Language spelling of 'intersperse'.
riffle :: a -> List a -> List a
riffle = intersperse

rotateLeftN :: Int -> List a -> List a
rotateLeftN n = compose (replicate n rotateLeft)

rotateRightN :: Int -> List a -> List a
rotateRightN n = reverse . rotateLeftN n . reverse

rotateLeft :: List a -> List a
rotateLeft xs = caseList xs (flip snoc) xs

rotateRight :: List a -> List a
rotateRight = rotateRightN 1

-- | 'transpose' transposes a matrix represented as a list of lists.
--   It flips the rows and columns. The input matrix must be rectangular.
transpose :: ∀a. Matrix a -> Matrix a
transpose xss =
  null @(List a) xss
    nil
    (any null xss
      nil
      (map head xss `cons` transpose (map tail xss)))

------------------------------------------------------------
-- 6. Church-Encoded Maybe
------------------------------------------------------------

just ∷ a -> Maybe a
just x _ j = j x

nothing ∷ Maybe a
nothing = const

isNothing ∷ Maybe a -> Bool
isNothing = null . maybeToList

-- | 'fromJust' extracts the value from a 'Just' and throws an error on 'Nothing'.
fromJust ∷ Maybe a -> a
fromJust = fromMaybe undefined

-- | 'fromMaybe' extracts the value from a 'Just' or returns a default value on 'Nothing'.
fromMaybe ∷ a -> Maybe a -> a
fromMaybe x m = m x id

-- | 'maybeToList' converts a 'Just' value to a singleton list
--   and 'Nothing' to an empty list.
maybeToList ∷ Maybe a -> List a
maybeToList m = m nil singleton

-- | 'listToMaybe' extracts the first element of a list or returns 'Nothing'.
listToMaybe ∷ List a -> Maybe a
listToMaybe = foldr (const . just) nothing

-- | 'catMaybes' filters 'Just' values from a list of 'Maybe's
--   and returns a list of the extracted values.
catMaybes ∷ List (Maybe a) -> List a
catMaybes = mapMaybe id

mapMaybe ∷ (a -> Maybe b) -> List a -> List b
mapMaybe = concatMap . (maybeToList .)

-- | 'squashMaybe' collapses a nested 'Maybe' into a single 'Maybe'.
--   If the inner 'Maybe' is 'Nothing', the result is 'Nothing'.
squashMaybe ∷ Maybe (Maybe a) -> Maybe a
squashMaybe = fromMaybe nothing

-- | 'sequence' transforms a list of 'Maybe's into a 'Maybe' of a list.
--   If any 'Maybe' in the list is 'Nothing', the result is 'Nothing'.
sequence ∷ List (Maybe a) -> Maybe (List a)
sequence ms e = ms (\m r k -> m e $ \x -> r $ k . cons x) ($ nil)

------------------------------------------------------------
-- 7. Church-Encoded Either
------------------------------------------------------------

left ∷ a -> Either a _
left x l _ = l x

right ∷ b -> Either _ b
right x _ r = r x

isLeft ∷ Either _ _ -> Bool
isLeft = either (const true) (const false)

isRight ∷ Either _ _ -> Bool
isRight = not . isLeft

fromLeft ∷ Either a _ -> a
fromLeft e = e id undefined

fromRight ∷ Either _ b -> b
fromRight e = e undefined id

-- | Transforms Either (Maybe a) (Maybe b) to Maybe (Either a b)
maybeEither :: Either (Maybe a) (Maybe b) -> Maybe (Either a b)
maybeEither e = e
  (\ma -> ma nothing (just . left))
  (\mb -> mb nothing (just . right))

-- | Transforms Maybe (Either a b) to Either (Maybe a) (Maybe b)
eitherMaybe :: Maybe (Either a b) -> Either (Maybe a) (Maybe b)
eitherMaybe m = m (left nothing) (either (left . just) (\y -> right (just y)))

either ∷ (a -> c) -> _ -> Either a b -> c
either f g e = e f g

lefts ∷ List (Either a b) -> List a
lefts = concatMap (either singleton (const nil))

rights ∷ List (Either a b) -> List b
rights = concatMap (either (const nil) singleton)

partitionEithers ∷ List (Either a b) ->  List a `Pair` List b
partitionEithers = liftA2 pair lefts rights

------------------------------------------------------------
-- 8. Dictionary Operations
------------------------------------------------------------

-- | 'lookup' searches for a key in a list of key-value pairs using a custom equality predicate.
--   If the key is found, the corresponding value is returned; otherwise, 'nothing' is returned.
--   We relax the usual signature of this function by generalizing equality
--   to a more general correspondence relation @eq@ that can match values of
--   different types.
lookup :: (a -> c -> Bool) -> c -> Dict a b -> Maybe b
lookup eq key = listToMaybe . lookupAll eq key

-- | Return the value at the first key matching @key@, or @def@ when no key matches.
lookupDefault :: (a -> c -> Bool) -> b -> c -> Dict a b -> b
lookupDefault eq def key = fromMaybe def . lookup eq key

-- | Test whether the dictionary contains a key matching @key@.
keyExists :: (a -> c -> Bool) -> c -> Dict a b -> Bool
keyExists = member

-- | Synonym for 'keyExists': test whether a matching key is present.
keyMember :: (a -> c -> Bool) -> c -> Dict a b -> Bool
keyMember = keyExists

-- | Test whether no dictionary key matches @key@.
keyFree :: (a -> c -> Bool) -> c -> Dict a b -> Bool
keyFree = notMember

-- Note: 'keys' and 'values' are defined in the @<map>@-style dictionary
-- section further below; the helpers here reuse those definitions.

-- | Apply a function to every key and value, preserving dictionary order in the result list.
keyValueMap :: _ -> Dict k v -> List a
keyValueMap = map . uncurry

-- | Transform every key while preserving its value and position; collisions are not removed.
mapKeys :: (k₁ -> k₂) -> Dict k₁ v -> Dict k₂ v
mapKeys = map . flip bimapPair id

-- | Pair corresponding keys and values, stopping when either input list ends.
dictFromLists :: List k -> List v -> Dict k v
dictFromLists = zip

-- | Build a unique-key dictionary from keys by assigning @f key@ to each key.
--   Later occurrences of an equivalent key replace earlier values.
associationMap :: Equal k -> _ -> List k -> Dict k v
associationMap eq f = fromList eq . map (pair <*> f)

-- | 'insertOrUpdate' inserts or updates a key-value pair in a list of
--   key-value pairs. If the key is already present in the list, the value
--   is updated; if not, a new pair is inserted.
insertOrUpdate :: Equal a -> a -> b -> Dict a b -> Dict a b
insertOrUpdate = insertWith const

{-|
'invert' transforms a list of key-value pairs into a list of value-key-list
pairs, grouping all keys that share the same value.

* Each (k,v) from the original list becomes part of a grouping under v.
* If multiple keys map to the same value, they accumulate into one list of keys.
* The @eq@ predicate controls how values are considered equal.

(The exact order depends on fold right-to-left insertion, so the first unique value
encountered ends up at the head of the final list, and each key prepends to the
front of its group's key list.)
-}
invert :: ∀a b. Equal b -> Dict a b -> Dict b (List a)
invert eq = foldrWithKey
  (\k v -> upsertWith @b @(List a) eq v (singleton k) (cons k)) nil

-- | 'merge' merges a list of dictionaries into a single dictionary, using
--   a custom equality function to match keys. If a key appears in multiple
--   dictionaries, all associated values are gathered into one aggregated list.
merge :: Equal k -> List (Dict k v) -> Dict k (List v)
merge eq ds = fromListWith (flip append) eq (concatMap (mapValues singleton) ds)

-- | Merge dictionaries by key as in 'merge', then replace each gathered value list
--   with the result of @combine@.
mergeWith :: Equal k -> (List v -> r) -> List (Dict k v) -> Dict k r
mergeWith eq combine = mapValues combine . merge eq

-- | 'collapse' collapses a dictionary of dictionaries into a single dictionary,
--   combining keys from the outer and inner dictionaries into a single key with
--   two components.
collapse :: ∀k₁ k₂ v. Equal k₁ -> Equal k₂ -> Dict k₁ (Dict k₂ v) -> Dict (k₁ `Pair` k₂) v
collapse _ _ = concatMap @(k₁ `Pair` Dict k₂ v) @((k₁ `Pair` k₂) `Pair` v)
  (\kv -> kv $ \k dict -> mapKeys (pair k) dict)

-- | 'eitherDict' merges two dictionaries with different key types into a single dictionary
--   with keys of a sum type. The resulting dictionary contains all key-value pairs from
--   both input dictionaries, with keys tagged with 'left' for the first dictionary and 'right'
--   for the second dictionary.
eitherDict :: Dict k₁ v -> Dict k₂ v -> Dict (Either k₁ k₂) v
eitherDict dict₁ dict₂ = append (mapKeys left dict₁) (mapKeys right dict₂)

-- | Compose two dictionary relations: for each @k1 -> k2@ entry in the first
--   dictionary, emit @k1 -> v@ when @k2@ occurs in the second, dropping misses.
chain :: Equal k₁ -> Equal k₂ -> Dict k₁ k₂ -> Dict k₂ v -> Dict k₁ v
chain _ eq₂ = flip (mapMaybeValues . flip (lookup eq₂))

-- | Compose a list of endo-dictionaries from right to left with the supplied
--   dictionary as the final target; entries whose intermediate key is absent are dropped.
chainN :: Equal a -> Dict a a -> List (Dict a a) -> Dict a a
chainN eq = foldr (chain eq eq)

-- | 'mapValues' applies a function to each value in a dictionary, producing
--   a new dictionary. The keys remain unchanged.
mapValues :: (v₁ -> v₂) -> Dict k v₁ -> Dict k v₂
mapValues = map . bimapPair id

-- | 'mapMaybeValues' applies a function to each value in a dictionary,
--   producing a new dictionary. Returning 'nothing' removes
--   a key-value pair from the dictionary. The keys remain unchanged.
mapMaybeValues :: (v₁ -> Maybe v₂) -> Dict k v₁ -> Dict k v₂
mapMaybeValues = mapMaybeWithKey . const

-- | 'cartesianDict' computes the Cartesian product of two dictionaries.
--   It pairs each key-value pair from the first dictionary with each
--   key-value pair from the second dictionary, regrouping them so that
--   the keys are paired together into a composite key and the values
--   are paired together as well.
cartesianDict :: ∀k₁ k₂ v₁ v₂. Dict k₁ v₁ -> Dict k₂ v₂ -> Dict (k₁ `Pair` k₂) (v₁ `Pair` v₂)
cartesianDict = cartesianWith @(k₁ `Pair` v₁) @(k₂ `Pair` v₂)
  @((k₁ `Pair` k₂) `Pair` (v₁ `Pair` v₂))
  (uncurry $ \k v -> bimapPair (pair k) (pair v))

-- | 'deleteKey' removes the pair with the specified key from the dictionary
--   if it exists. If the key is not present, the dictionary is unchanged.
deleteKey :: Equal k -> k -> Dict k v -> Dict k v
deleteKey eq key = keySelect (not . flip eq key)

-- | Keep entries whose keys occur in @wanted@, preserving their dictionary order.
keyTake :: Equal k -> List k -> Dict k v -> Dict k v
keyTake eq = keySelect . flip (elemBy eq)

-- | Look up keys in @wanted@ order, emitting each found key and value and omitting misses.
keyTakeOrdered :: Equal k -> List k -> Dict k v -> Dict k v
keyTakeOrdered eq wanted dict = mapMaybe
  (\key -> lookup eq key dict nothing (just . pair key)) wanted

-- | Remove entries whose keys occur in @unwanted@.
keyDrop :: Equal k -> List k -> Dict k v -> Dict k v
keyDrop eq = keySelect . flip (notElemBy eq)

-- | Keep exactly the entries whose keys satisfy the predicate.
keySelect :: (k -> Bool) -> Dict k v -> Dict k v
keySelect p = filterWithKey (const . p)

-- | Insert @new@ for a missing key, or replace an existing value @old@ with @combine old@.
upsertWith :: Equal k -> k -> v -> _ -> Dict k v -> Dict k v
upsertWith eq key new combine = insertWith (const combine) eq key new

-- | Sort dictionary entries by a projection of their keys under @le@.
keySortBy :: LE a -> _ -> Dict k v -> Dict k v
keySortBy le keyOf = sortOn le (keyOf . fst)

-- | Sort dictionary entries by their keys under @le@.
keySort :: LE k -> Dict k v -> Dict k v
keySort = flip keySortBy id

-- | Return every key appearing in any dictionary, once, in first-occurrence order.
unionKeys :: ∀k v. Equal k -> List (Dict k v) -> List k
unionKeys eq = nubBy eq . concatMap @(Dict k v) keys

-- | Return the first dictionary's keys that occur in every remaining dictionary.
--   The empty dictionary list has no intersection keys.
intersectionKeys :: ∀k v. Equal k -> List (Dict k v) -> List k
intersectionKeys eq = caseList @_ @(Dict k v) nil $ \first ->
  flip filter (keys first) . flip (all . keyExists eq)

-- | Remove from a dictionary every key appearing in any dictionary in @others@.
keyComplement :: Equal k -> Dict k v -> List (Dict k w) -> Dict k v
keyComplement eq = flip $ keySelect . flip (notElemBy (keyExists eq))

-- | Project each input dictionary onto the keys common to all inputs, using
--   their common first-dictionary order.
keyIntersection :: Equal k -> List (Dict k v) -> List (Dict k v)
keyIntersection eq dicts = map
  (keyTakeOrdered eq (intersectionKeys eq dicts)) dicts

-- | Project every input dictionary onto the union of all keys, inserting
--   @missing key@ wherever that dictionary lacks a union key.
keyUnion :: ∀k v. Equal k -> _ -> List (Dict k v) -> List (Dict k v)
keyUnion eq missing dicts =
  map @(Dict k v) (\dict -> associationMap eq
    (\key -> lookupDefault eq (missing key) key dict) (unionKeys eq dicts)) dicts

------------------------------------------------------------
-- 9. Matrix Transformations
------------------------------------------------------------

-- | 'turn' rotates a rectangular matrix by 90 degrees clockwise.
turn :: ∀ a. Matrix a -> Matrix a
turn = transpose . reverse @(List a)

-- | 'unturn' rotates a rectangular matrix by 90 degrees counterclockwise.
unturn :: ∀ a. Matrix a -> Matrix a
unturn = reverse @(List a) . transpose

------------------------------------------------------------
-- 10. Additional List Combinators and Checks
------------------------------------------------------------

-- | Return every order-preserving subsequence, including @nil@ and the full list.
subsequences ∷ List a -> List (List a)
subsequences = foldr
  (\x rest -> rest `append` map (cons x) rest) (singleton nil)

-- | Return every permutation of the input; repeated input values produce repeated outputs.
permutations ∷ ∀a. List a -> List (List a)
permutations = foldr
  (concatMap @(List a) @(List a) . interleave) (singleton nil)
  where
    interleave ∷ a -> List a -> List (List a)
    interleave x ys = zipWith
      ((. cons x) . append) (inits ys) (tails ys)

-- | Apply a binary operation to projections of both arguments:
--   @on f key x y = f (key x) (key y)@.
on :: (b -> b -> c) -> _ -> a -> a -> c
on f key x = f (key x) . key

-- | Remove later elements equivalent to an earlier element, preserving first occurrences.
nubBy :: (a -> a -> Bool) -> List a -> List a
nubBy eq = foldl (\acc x -> elemBy eq x acc acc (acc `snoc` x)) nil

-- | Remove later elements whose projected keys equal an earlier projected key.
nubOn :: Equal b -> _ -> List a -> List a
nubOn eq = nubBy . on eq

-- | Remove the first element equivalent to @y@; leave the list unchanged when none matches.
deleteBy :: (a -> a -> Bool) -> a -> List a -> List a
deleteBy eq y xs = break (flip eq y) xs $ \pre rest -> pre `append` tail rest

-- | Delete one matching occurrence from the first list for each element of the second list.
deleteFirstsBy :: (a -> a -> Bool) -> List a -> List a -> List a
deleteFirstsBy eq = foldr (deleteBy eq)

-- | Append to @xs@ those elements of @ys@ that match no element of @xs@;
--   duplicates occurring only within @ys@ are retained.
unionBy :: (a -> a -> Bool) -> List a -> List a -> List a
unionBy eq xs ys = append xs (complementBy eq ys xs)

-- | Keep each element of @xs@ that matches at least one element of @ys@, retaining duplicates.
intersectBy :: (a -> a -> Bool) -> List a -> List a -> List a
intersectBy eq = flip (filter . flip (elemBy eq))

-- | Split a list into maximal consecutive groups whose elements relate to
--   the first element of their group under @eq@.
groupBy :: (a -> a -> Bool) -> List a -> List (List a)
groupBy eq = caseList nil $ \x xs' ->
  span (eq x) xs' $ \matching rest ->
    (x `cons` matching) `cons` groupBy eq rest

-- | Group consecutive elements when their projected keys relate under @eq@.
groupOn :: Equal b -> _ -> List a -> List (List a)
groupOn eq = groupBy . on eq

-- | 'gatherBy' groups elements whose keys compare equal, preserving the
--   first-occurrence order of keys and the original order inside each group.
--   This is the Church-list analogue of Wolfram Language @GatherBy@.
gatherBy :: ∀a k. Equal k -> _ -> List a -> List (List a)
gatherBy eq keyOf = values @k @(List a) . merge eq . map (liftA2 singletonDict keyOf id)

-- | Group all equivalent elements regardless of position, preserving the
--   first occurrence of each value as group order and input order within groups.
gather :: Equal a -> List a -> List (List a)
gather = flip gatherBy id

-- | Return a dictionary mapping each equivalence-class representative to its
--   number of occurrences; representatives appear in first-occurrence order.
countsBy :: Equal a -> List a -> Dict a Int
countsBy eq = fromListWith (+) eq . map (`pair` 1)

-- | Count elements by projected key, returning keys in first-occurrence order.
countsByKey :: Equal k -> _ -> List a -> Dict k Int
countsByKey eq keyOf = countsBy eq . map keyOf

-- | Return element-count pairs under @eq@, using the first occurrence as each
--   class representative and preserving representative order.
tallyBy :: Equal a -> List a -> Dict a Int
tallyBy = countsBy

-- | Count the distinct equivalence classes occurring in the list.
countDistinctBy :: Equal a -> List a -> Int
countDistinctBy eq = length . nubBy eq

-- | Map each equivalence-class representative to the one-based positions at
--   which its members occur, preserving representative and position order.
positionIndexBy :: Equal a -> List a -> Dict a (List Int)
positionIndexBy eq xs = merge eq (zipWith singletonDict xs (scanl (const . succ) 1 xs))

-- | 'pick' selects elements from two lists based on boolean conditions.
--   For each position in the 'keys' list, it chooses the corresponding
--   element from @xs@ if the key is true, or from @ys@ if the key is false.
--   It requires both @xs@ and @ys@ to have at least as many elements
--   as the 'keys' list.
pick :: List Bool -> List a -> List b -> List (Either a b)
pick keys xs ys = pick' keys (map left xs) (map right ys)

-- | For each Boolean selector, take the corresponding element from the first
--   list when true and the second when false; error if either value list is shorter.
pick' :: List Bool -> List a -> List a -> List a
pick' keys xs ys =
  caseList @_ @Bool nil (\k ks ->
    caseList (error "pick: xs shorter than keys") (\x xs' ->
      caseList (error "pick: ys shorter than keys") (\y ys' ->
        k x y `cons` pick' ks xs' ys') ys) xs) keys

-- | Checks if @ys@ contains @xs@ as a contiguous substring under @eq@.
--   We relax the usual signature of this function by generalizing equality
--   to a more general correspondence relation @eq@ that can match values of
--   different types.
isSubstring :: (a -> b -> Bool) -> List a -> List b -> Bool
isSubstring eq xs = any (isPrefixOf eq xs) . tails

-- | Checks if @ys@ contains @xs@ as a subsequence (not necessarily contiguous)
--   under @eq@.
--   We relax the usual signature of this function by generalizing equality
--   to a more general correspondence relation @eq@ that can match values of
--   different types.
isSubseq :: (a -> b -> Bool) -> List a -> List b -> Bool
isSubseq eq xs = caseList (null xs)
  (\y ys' -> caseList true (\x xs' -> isSubseq eq (eq x y xs' xs) ys') xs)

-- | Checks if @sub@ is a prefix of @lst@ using the correspondence relation @eq@.
--   We relax the usual signature of this function by generalizing equality
--   to a more general correspondence relation @eq@ that can match values of
--   different types.
isPrefixOf :: (a -> b -> Bool) -> List a -> List b -> Bool
isPrefixOf eq xs ys =
  eqInt (length xs) (length (keys (takeWhile (uncurry eq) (zip xs ys))))

-- | @break@ splits the list into two parts: the longest prefix of elements
--   that do NOT satisfy the predicate, and the remainder.
break :: (a -> Bool) -> List a -> List a `Pair` List a
break p = span (not . p)

-- | 'elemBy' tests whether the first argument appears in the list using
--   the given equality predicate.
--   We relax the usual signature of this function by generalizing equality
--   to a more general correspondence relation @eq@ that can match values of
--   different types.
elemBy :: (a -> b -> Bool) -> a -> List b -> Bool
elemBy eq = any . eq

-- Note: a predicate-style find (@(a -> Bool) -> List a -> Maybe a@) is provided
-- by 'findIf' in the @<algorithm>@ section below; the two-argument 'find'
-- (@std::find@: locate the first element matching a value under a relation) is
-- defined there as well.

-- | Return the zero-based position of the first element satisfying the predicate,
--   or 'nothing' when no element satisfies it.
findIndex :: (a -> Bool) -> List a -> Maybe Int
findIndex p = listToMaybe . findIndices p

-- | Return the zero-based positions of every element satisfying the predicate.
findIndices :: (a -> Bool) -> List a -> List Int
findIndices p xs = keys (filterValues p (zip (scanl (const . succ) 0 xs) xs))

-- | Return the zero-based position of the first element matching @x@, or 'nothing'.
elemIndex :: (a -> b -> Bool) -> a -> List b -> Maybe Int
elemIndex eq = findIndex . eq

-- | Checks if the first list is a suffix of the second list according to
--   the given correspondence relation @eq@.
--   We relax the usual signature of this function by generalizing equality
--   to a more general correspondence relation @eq@ that can match values of
--   different types.
isSuffixOf :: (a -> b -> Bool) -> List a -> List b -> Bool
isSuffixOf eq xs = isPrefixOf eq (reverse xs) . reverse

-- | Return the greatest element under @le@, choosing the last equivalent maximum;
--   error on an empty list.
maximumBy :: LE a -> List a -> a
maximumBy le = foldl1 (maxBy le)

-- | Return the element whose projected key is greatest, choosing the last tie;
--   error on an empty list.
maximumOn :: LE b -> _ -> List a -> a
maximumOn le = maximumBy . on le

-- | Return the least element under @le@, choosing the last equivalent minimum;
--   error on an empty list.
minimumBy :: LE a -> List a -> a
minimumBy le = foldl1 (flip (minBy le))

-- | Return the element whose projected key is least, choosing the last tie;
--   error on an empty list.
minimumOn :: LE b -> _ -> List a -> a
minimumOn le = minimumBy . on le

-- | Return @(minimum, maximum)@ for a non-empty list, choosing the last tie
--   for each component; error on @nil@.
minMaxBy :: LE a -> List a -> a `Pair` a
minMaxBy = liftA2 (liftA2 pair) minimumBy maximumBy

-- | Remove repeated values under @eq@ while retaining their first occurrences.
deleteDuplicatesBy :: Equal a -> List a -> List a
deleteDuplicatesBy = nubBy

-- | Keep elements of @xs@ that match no element of @ys@ under the relation.
complementBy :: (a -> b -> Bool) -> List a -> List b -> List a
complementBy eq xs ys = filter (not . flip (elemBy eq) ys) xs

-- | Test whether every element of @ys@ is matched by some element of @xs@.
containsAllBy :: (a -> b -> Bool) -> List a -> List b -> Bool
containsAllBy eq xs ys = containsOnlyBy (flip eq) ys xs

-- | Test whether any element of @ys@ is matched by some element of @xs@.
containsAnyBy :: (a -> b -> Bool) -> List a -> List b -> Bool
containsAnyBy eq = flip (any . flip (elemBy eq))

-- | Test whether no element of @ys@ is matched by an element of @xs@.
containsNoneBy :: (a -> b -> Bool) -> List a -> List b -> Bool
containsNoneBy eq xs = not . containsAnyBy eq xs

-- | Test whether every element of @xs@ matches some element of @allowed@.
containsOnlyBy :: (a -> b -> Bool) -> List a -> List b -> Bool
containsOnlyBy eq = flip (all . flip (elemBy eq))

------------------------------------------------------------
-- 11. Sorting
------------------------------------------------------------

-- Private helper: split a list around a pivot and pass less/equal/greater parts onward.
partition3 :: LE a -> a -> List a -> (List a -> List a -> List a -> r) -> r
partition3 le pivot xs k = partition (flip le pivot) xs $ \lower greater ->
  partition (le pivot) lower $ \equal less -> k less equal greater

-- Private helper: perform one stable three-way quicksort partitioning step.
quickStep :: LE a -> (List a -> List a) -> List a -> List a
quickStep le sort = caseList nil $ \pivot rest ->
  partition3 le pivot rest $ \less equal greater ->
    append (sort less) $ append (pivot `cons` equal) (sort greater)

-- | `mergesort` sorts a list using the mergesort algorithm.
--    The time complexity is O(n log n).
mergesort :: LE a -> List a -> List a
mergesort le xs = foldr @_ @(List _ `Pair` List _)
  (\x p -> p $ \l r -> (x `cons` r) `pair` l) (nil `pair` nil) xs $ \l r ->
    null r xs (mergeBy le (mergesort le l) (mergesort le r))

-- | Stably sort elements by projected keys under @le@; equal-key elements retain
--   their original relative order.
sortOn :: LE b -> _ -> List a -> List a
sortOn le = quicksort . on le

-- | `quicksort` sorts a list using the quicksort algorithm.
--   The time complexity is O(n^2) in the worst case, but
--   O(n log n) in most practical cases.
quicksort :: LE a -> List a -> List a
quicksort = liftA2 quickStep id quicksort

-- | `heapsort` retains its historical name; the compact ADT-free
--   implementation sorts by folding singleton runs.
heapsort :: LE a -> List a -> List a
heapsort le = foldr (mergeBy le . singleton) nil

-- | `introsort` sorts a list using a hybrid sorting
--   algorithm that combines quicksort and heapsort.
introsort :: ∀a. LE a -> List a -> List a
introsort le = go 16
  where
    go :: Int -> List a -> List a
    go depth xs = leInt (length xs) 16
      (quicksort le)
      (leInt depth 0 (heapsort le) (quickStep le (go (depth - 1)))) xs

-- | `nthElement` returns a fully sorted list, so its nth position is the
--   nth order statistic.
nthElement :: LE a -> Int -> List a -> List a
nthElement = const . heapsort

-- | `nthElement'` efficiently finds the nth element in a list using Introselect algorithm,
-- which combines quickselect with fallback to heapsort when recursion depth becomes excessive.
nthElement' :: ∀a. LE a -> Int -> List a -> List a
nthElement' le n xs = introselect 0 n xs
  where
    maxDepth = 2 * length xs
    -- 'introselect depth k ys' rearranges 'ys' so that index 'k' holds its k-th order
    -- statistic, with everything before <= it and everything after >= it. 'k' is the
    -- LOCAL index into 'ys' and is re-indexed on each recursion; every branch reassembles
    -- ALL of less ++ (pivot:equal) ++ greater so no elements are lost.
    introselect :: Int -> Int -> List a -> List a
    introselect depth k xs =
      geInt depth maxDepth
        (heapsort le xs)  -- Fall back to heapsort
        (caseList nil (\pivot rest ->
          partition3 le pivot rest $ \less equal greater ->
          let lessLen    = length less
              pivotBlock = pivot `cons` equal      -- the pivot and its duplicates
              pivotEnd   = lessLen + length pivotBlock
          in append (ltInt k lessLen (introselect (succ depth) k less) less)
                    (append pivotBlock
                            (ltInt k pivotEnd greater
                                   (introselect (succ depth) (k - pivotEnd) greater)))) xs)

------------------------------------------------------------
-- 12. Longest Common Substructure
------------------------------------------------------------

-- | 'longestCommonPrefix' finds the longest common prefix between two lists.
longestCommonPrefix :: Equal a -> List a -> List a -> List a
longestCommonPrefix eq xs = keys . takeWhile (uncurry eq) . zip xs

-- | 'longestCommonSuffix' finds the longest common suffix between two lists.
longestCommonSuffix :: Equal a -> List a -> List a -> List a
longestCommonSuffix eq xs = reverse . longestCommonPrefix eq (reverse xs) . reverse

-- | Concatenate the common prefix and common suffix of two lists. Despite its
--   historical name, this does not search for an interior common sublist, and
--   the prefix and suffix may overlap when the inputs are equal or nearly equal.
longestCommonSublist :: Equal a -> List a -> List a -> List a
longestCommonSublist eq = liftA2 (liftA2 append)
  (longestCommonPrefix eq) (longestCommonSuffix eq)

------------------------------------------------------------
-- 13. <algorithm> equivalents: non-modifying searches
------------------------------------------------------------
-- Equivalents of <std::algorithm> non-modifying sequence operations. As elsewhere,
-- equality/order relations are passed explicitly (no type classes), and several are
-- generalized to a heterogeneous correspondence relation @a -> b -> Bool@.

-- | 'none' is @std::none_of@: true iff no element satisfies the predicate.
none :: (a -> Bool) -> List a -> Bool
none p = not . any p

-- | 'findIf' is @std::find_if@: the first element satisfying @p@, or 'nothing'.
findIf :: (a -> Bool) -> List a -> Maybe a
findIf p = listToMaybe . filter p

-- | Return the first element that does not satisfy @p@, or 'nothing' if all do.
findIfNot :: (a -> Bool) -> List a -> Maybe a
findIfNot p = findIf (not . p)

-- | 'find' is @std::find@: the first element equal (under @eq@) to @v@.
find :: (a -> b -> Bool) -> b -> List a -> Maybe a
find eq = findIf . flip eq

-- | 'findLast' is @std::ranges::find_last_if@ (C++23): the last element satisfying @p@.
findLast :: (a -> Bool) -> List a -> Maybe a
findLast p = findIf p . reverse

-- | 'countIf' is @std::count_if@: how many elements satisfy @p@.
countIf :: (a -> Bool) -> List a -> Int
countIf p = length . filter p

-- | 'count' is @std::count@: how many elements equal (under @eq@) @v@.
count :: (a -> b -> Bool) -> b -> List a -> Int
count eq = countIf . flip eq

-- | 'mismatch' is @std::mismatch@: the first pair of corresponding elements that do
--   NOT satisfy @eq@ (or 'nothing' if the shorter range matches throughout).
mismatch :: (a -> b -> Bool) -> List a -> List b -> Maybe (a `Pair` b)
mismatch eq xs = findIf (not . uncurry eq) . zip xs

-- | 'adjacentFind' is @std::adjacent_find@: the first element that is equal (under
--   @eq@) to the element following it.
adjacentFind :: (a -> a -> Bool) -> List a -> Maybe a
adjacentFind eq xs = findIf (uncurry eq) (zip xs $ tail xs)
  nothing (just . fst)

-- | 'search' is @std::search@: the start index of the first occurrence of @needle@
--   inside @haystack@ (under @eq@), or 'nothing'. (An empty needle matches at 0.)
search :: (a -> b -> Bool) -> List a -> List b -> Maybe Int
search eq needle = findIndex (isPrefixOf eq needle) . tails

-- | 'findEnd' is @std::find_end@: the start index of the LAST occurrence of @needle@.
findEnd :: (a -> b -> Bool) -> List a -> List b -> Maybe Int
findEnd eq needle = listToMaybe . reverse . findIndices (isPrefixOf eq needle) . tails

-- | 'findFirstOf' is @std::find_first_of@: the first element of @xs@ that equals
--   (under @eq@) some element of @set@.
findFirstOf :: (a -> b -> Bool) -> List a -> List b -> Maybe a
findFirstOf eq = flip (findIf . flip (elemBy eq))

-- | 'searchN' is @std::search_n@: the start index of the first run of @n@ consecutive
--   elements all equal (under @eq@) to @v@. (@n <= 0@ matches at 0.)
searchN :: (a -> b -> Bool) -> Int -> b -> List a -> Maybe Int
searchN eq n = search (flip eq) . replicate n

------------------------------------------------------------
-- 14. <algorithm> equivalents: modifying sequence operations
------------------------------------------------------------

-- Note: 'take' and 'drop' are the Wolfram-style list operations from
-- section 5 above; they keep / discard the first @n@
-- elements (@std::ranges::take_view@ / @drop_view@) and back 'rotate' below.

-- | 'rotate' is @std::rotate@: moves the first @n@ elements to the back
--   (a left rotation by @n@).
rotate :: Int -> List a -> List a
rotate = liftA2 (liftA2 append) drop take

-- | 'removeIf' is @std::remove_if@: drops every element satisfying @p@.
removeIf :: (a -> Bool) -> List a -> List a
removeIf p = filter (not . p)

-- | 'remove' is @std::remove@: drops every element equal (under @eq@) to @v@.
remove :: (a -> b -> Bool) -> b -> List a -> List a
remove eq = removeIf . flip eq

-- | 'replaceIf' is @std::replace_if@: replaces every element satisfying @p@ with @new@.
replaceIf :: (a -> Bool) -> a -> List a -> List a
replaceIf p new = map (\x -> p x new x)

-- | 'replace' is @std::replace@: replaces every element equal (under @eq@) to @old@.
replace :: (a -> a -> Bool) -> a -> a -> List a -> List a
replace eq = replaceIf . flip eq

-- | 'uniqueBy' is @std::unique@: collapses consecutive runs of equal (under @eq@)
--   elements to a single element.
uniqueBy :: ∀a. (a -> a -> Bool) -> List a -> List a
uniqueBy eq = map @(List a) head . groupBy eq

------------------------------------------------------------
-- 15. <algorithm> equivalents: partitioning and sorting predicates
------------------------------------------------------------

-- | 'isPartitioned' is @std::is_partitioned@: true iff every element satisfying @p@
--   precedes every element that does not.
isPartitioned :: (a -> Bool) -> List a -> Bool
isPartitioned p = none p . dropWhile p

-- | 'partitionPoint' is @std::partition_point@: on a @p@-partitioned list, the index
--   of the first element that does NOT satisfy @p@ (i.e. the length of the @p@-prefix).
partitionPoint :: (a -> Bool) -> List a -> Int
partitionPoint p = length . takeWhile p

-- | 'isSorted' is @std::is_sorted@: true iff no adjacent pair violates @le@.
isSorted :: LE a -> List a -> Bool
isSorted le = all (uncurry le) . (zip <*> tail)

-- | 'isSortedUntil' is @std::is_sorted_until@: the index of the first element that
--   breaks @le@-sortedness (the length of the sorted prefix).
isSortedUntil :: LE a -> List a -> Int
isSortedUntil le xs = findIndex (not . uncurry le)
  (zip xs $ tail xs) (length xs) succ

------------------------------------------------------------
-- 16. <algorithm> equivalents: binary search on sorted ranges
------------------------------------------------------------
-- These assume @xs@ is sorted by @le@. (On a list the search is O(n), but the
-- results match @std::lower_bound@/@std::upper_bound@/etc.)

-- | 'lowerBound' is @std::lower_bound@: the index of the first element not less than
--   @v@ (equivalently, the count of elements strictly less than @v@).
lowerBound :: LE a -> a -> List a -> Int
lowerBound le v = countIf (not . le v)

-- | 'upperBound' is @std::upper_bound@: the index of the first element greater than
--   @v@ (equivalently, the count of elements not greater than @v@).
upperBound :: LE a -> a -> List a -> Int
upperBound le = countIf . flip le

-- | 'binarySearch' is @std::binary_search@: whether an element equivalent to @v@
--   (under @le@) is present.
binarySearch :: LE a -> a -> List a -> Bool
binarySearch le = any . eqFromLE le

-- | On a sorted list, return the half-open index range containing all elements
--   equivalent to @v@: the first not-less index paired with the first greater index.
equalRange :: LE a -> a -> List a -> Int `Pair` Int
equalRange le v = liftA2 pair (lowerBound le v) (upperBound le v)

------------------------------------------------------------
-- 17. <algorithm> equivalents: merge and set operations on sorted ranges
------------------------------------------------------------

-- | 'mergeBy' is @std::merge@: merges two @le@-sorted lists into one sorted list,
--   preserving duplicates (stable: equal elements keep @xs@-before-@ys@ order).
mergeBy :: LE a -> List a -> List a -> List a
mergeBy le = setOp le true true true false

-- | 'includes' is @std::includes@: whether sorted @xs@ contains sorted @ys@ as a
--   (multiset) subsequence.
includes :: LE a -> List a -> List a -> Bool
includes le xs ys = null (setDifference le ys xs)

-- Private helper parameterized by which source/equality cases emit values and
-- whether an equality consumes both inputs. The public set operations below
-- choose its Church-Boolean flags. For every std::set_* operation the
-- "emit x when x < y" flag coincides with "emit leftovers of xs" (@left@) and
-- "emit y when y < x" with "emit leftovers of ys" (@right@), so one flag serves
-- both roles.
setOp :: ∀a. LE a -> Bool -> Bool -> Bool -> Bool -> List a -> List a -> List a
setOp le left right eq both = go
  where
    go :: List a -> List a -> List a
    go xs ys = caseList (right ys nil) (\x xs' ->
      caseList (left xs nil) (\y ys' ->
        le x y
          (le y x
            (eq (cons x) id (both (go xs' ys') (go xs' ys)))
            (left (cons x) id (go xs' ys)))
          (right (cons y) id (go xs ys'))) ys) xs

-- | Merge two sorted multisets, retaining the maximum multiplicity of each
--   equivalence class and taking equal representatives from the first list first.
setUnion :: LE a -> List a -> List a -> List a
setUnion le = setOp le true true true true

-- | Return the sorted multiset intersection, retaining the minimum multiplicity
--   of each equivalence class and representatives from the first list.
setIntersection :: LE a -> List a -> List a -> List a
setIntersection le = setOp le false false true true

-- | Subtract the second sorted multiset from the first, removing one occurrence
--   for each equivalent occurrence in the second and preserving remaining order.
setDifference :: LE a -> List a -> List a -> List a
setDifference le = setOp le true false false true

-- | Return elements occurring in exactly one sorted multiset, with multiplicity
--   equal to the absolute difference between the two input multiplicities.
setSymmetricDifference :: LE a -> List a -> List a -> List a
setSymmetricDifference le = setOp le true true false true

------------------------------------------------------------
-- 18. <algorithm> equivalents: minimum/maximum and comparison
------------------------------------------------------------

-- | 'maxBy' returns the larger of two values, choosing the second on a tie.
maxBy :: LE a -> a -> a -> a
maxBy le x y = le x y y x

-- | Return the smaller of two values under @le@, choosing the first on a tie.
minBy :: LE a -> a -> a -> a
minBy le x y = le x y x y

-- | Return @(minimum, maximum)@ for two values; on a tie the first is the
--   minimum component and the second is the maximum component.
minmaxBy :: LE a -> a -> a -> a `Pair` a
minmaxBy le = liftA2 (liftA2 pair) (minBy le) (maxBy le)

-- | 'clamp' is @std::clamp@: @x@ confined to @[lo, hi]@, i.e. @max lo (min hi x)@.
clamp :: LE a -> a -> a -> a -> a
clamp le lo hi = maxBy le lo . minBy le hi

-- | Return the minimum and maximum of a non-empty list, choosing the first
--   minimum and last maximum on ties; error on the empty list.
minmaxElement :: LE a -> List a -> a `Pair` a
minmaxElement le = liftA2 pair (foldl1 (minBy le)) (foldl1 (maxBy le))

-- | 'equalBy' is @std::equal@: whether two lists are equal element-by-element
--   (and of equal length) under @eq@.
equalBy :: (a -> b -> Bool) -> List a -> List b -> Bool
equalBy eq xs ys = and (isPrefixOf eq xs ys) (isPrefixOf (flip eq) ys xs)

-- | 'lexicographicalLess' is @std::lexicographical_compare@: whether @xs@ precedes
--   @ys@ in lexicographic order under @le@.
lexicographicalLess :: LE a -> List a -> List a -> Bool
lexicographicalLess le = ltFromLE (lexicographicLE le)

-- | 'compareBy' is @std::lexicographical_compare_three_way@: @-1@/@0@/@1@ as @xs@
--   is lexicographically less than / equal to / greater than @ys@ under @le@.
--   (An @Int@ stands in for @std::strong_ordering@, since the module admits no ADTs.)
compareBy :: LE a -> List a -> List a -> Int
compareBy le xs ys = lexicographicLE le xs ys 0 1 - lexicographicLE le ys xs 0 1

------------------------------------------------------------
-- 19. <algorithm> equivalents: permutation operations
------------------------------------------------------------

-- | 'isPermutation' is @std::is_permutation@: whether @ys@ is a rearrangement of
--   @xs@ (same multiset) under @eq@.
isPermutation :: (a -> a -> Bool) -> List a -> List a -> Bool
isPermutation eq xs ys = and (eqInt (length xs) (length ys))
  (null (deleteFirstsBy eq ys xs))

-- | 'nextPermutation' is @std::next_permutation@: the next permutation in
--   lexicographic order under @le@, or 'nothing' if @xs@ is the last (descending) one.
nextPermutation :: LE a -> List a -> Maybe (List a)
nextPermutation le = caseList nothing $ \x xs ->
  nextPermutation le xs
    (findIf (ltFromLE le x) (quicksort le xs)
      nothing
      (\m -> just (m `cons` mergesort le (x `cons` deleteBy (eqFromLE le) m xs))))
    (just . cons x)

-- | 'prevPermutation' is @std::prev_permutation@: the previous permutation in
--   lexicographic order under @le@, or 'nothing' if @xs@ is the first (ascending) one.
prevPermutation :: LE a -> List a -> Maybe (List a)
prevPermutation le = nextPermutation (flip le)

------------------------------------------------------------
-- 20. <map>/<unordered_map> equivalents: dictionary query and construction
------------------------------------------------------------
-- A `Dict k v = List (k `Pair` v)` is an association list with unique keys (under a
-- supplied `Equal`). These mirror the *relation* surface of `std::map` /
-- `std::unordered_map` (and `Data.Map`); the container/hash/ordered-iteration
-- machinery (buckets, node handles, key-ordered `lower_bound`, ...) has no analogue
-- on a linearly-scanned list and is intentionally absent. Equality on keys is passed
-- explicitly; several queries relax it to a heterogeneous `a -> c -> Bool`.

-- | Test whether any dictionary key matches @key@ under the supplied relation.
member :: (a -> c -> Bool) -> c -> Dict a b -> Bool
member eq key = isJust . lookup eq key

-- | Test whether no dictionary key matches @key@ under the supplied relation.
notMember :: (a -> c -> Bool) -> c -> Dict a b -> Bool
notMember eq key = isNothing . lookup eq key

-- | Return the first value whose key matches @key@, or @def@ if no key matches.
findWithDefault :: b -> (a -> c -> Bool) -> c -> Dict a b -> b
findWithDefault = flip lookupDefault

-- | Return the first value whose key matches @key@; error if no key matches.
atKey :: (a -> c -> Bool) -> c -> Dict a b -> b
atKey eq = lookupDefault eq (error "atKey: key not found")

-- | Return the dictionary's keys in dictionary order.
keys :: ∀k v. Dict k v -> List k
keys = map @(k `Pair` v) fst

-- | Return the dictionary's values in dictionary order.
values :: ∀k v. Dict k v -> List v
values = map @(k `Pair` v) snd

-- | Synonym for 'values': return all dictionary values in dictionary order.
elems :: Dict k v -> List v
elems = values

-- | Return the number of key-value entries in the dictionary.
sizeDict :: ∀k v. Dict k v -> Int
sizeDict = length @(k `Pair` v)

-- | Test whether the dictionary has no entries.
nullDict :: ∀k v. Dict k v -> Bool
nullDict = null @(k `Pair` v)

-- | 'singletonDict' builds a one-entry dictionary.
singletonDict :: k -> v -> Dict k v
singletonDict k = singleton . pair k

-- | 'lookupAll' reads the dict as a *multimap*: every value stored under @key@.
lookupAll :: (a -> c -> Bool) -> c -> Dict a b -> List b
lookupAll eq key = values . keySelect (flip eq key)

-- | Count entries whose keys match @key@; unlike a valid 'Dict', a multimap-like
--   association list may therefore return more than one.
countKey :: (a -> c -> Bool) -> c -> Dict a b -> Int
countKey eq key = length . lookupAll eq key

------------------------------------------------------------
-- 21. <map> equivalents: insertion, construction and update
------------------------------------------------------------

-- | 'insert' is @std::map::insert@ / @try_emplace@: insert the pair only if @key@ is
--   absent (existing values are NOT overwritten — contrast 'insertOrUpdate').
insert :: Equal k -> k -> v -> Dict k v -> Dict k v
insert = insertWith (flip const)

-- | 'insertWith' inserts @value@ if @key@ is absent, otherwise replaces the stored
--   value @old@ with @f value old@ (new first, old second; @Data.Map.insertWith@).
--   Generalizes both 'insert' (@f = \\_ old -> old@, i.e. @flip const@ -- keep the
--   old value) and 'insertOrUpdate' (@f = const@ -- overwrite with the new value).
insertWith :: ∀k v. _ -> Equal k -> k -> v -> Dict k v -> Dict k v
insertWith f eq key value =
  caseList @_ @(k `Pair` v)
    (singleton (key `pair` value))
    (\kv rest ->
      kv $ \k v ->
        eq k key
          ((key `pair` f value v) `cons` rest)
          (kv `cons` insertWith f eq key value rest))

-- | 'fromList' builds a dictionary from a list of pairs; on duplicate keys the LAST
--   value wins (@Data.Map.fromList@).
fromList :: Equal k -> List (k `Pair` v) -> Dict k v
fromList = fromListWith const

-- | Build a unique-key dictionary, combining each later duplicate value @new@
--   with the stored value @old@ as @f new old@.
fromListWith :: (v -> v -> v) -> Equal k -> List (k `Pair` v) -> Dict k v
fromListWith f eq = foldlWithKey (flip (flip . insertWith f eq)) nil

-- | 'adjust' applies @f@ to the value at @key@ (if present); otherwise unchanged
--   (@Data.Map.adjust@). The keyed dual of 'mapValues'.
adjust :: Equal k -> _ -> k -> Dict k v -> Dict k v
adjust eq f key = mapWithKey (\k -> eq k key f id)

-- | 'alter' is the general insert/update/delete-at-a-key combinator
--   (@Data.Map.alter@): @f@ sees the current 'Maybe' value and returns the new one
--   ('nothing' deletes the key, @just v'@ sets it).
alter :: Equal k -> (Maybe v -> Maybe v) -> k -> Dict k v -> Dict k v
alter eq f key d =
  f (lookup eq key d)
    (deleteKey eq key d)
    (flip (insertOrUpdate eq key) d)

-- | Transform every value with access to its key, preserving keys and entry order.
mapWithKey :: _ -> Dict k v1 -> Dict k v2
mapWithKey f d = zip (keys d) (keyValueMap f d)

-- | Transform every value with access to its key; 'nothing' drops the entry and
--   @just value@ keeps the original key with the transformed value.
mapMaybeWithKey :: ∀k v1 v2. (k -> v1 -> Maybe v2) -> Dict k v1 -> Dict k v2
mapMaybeWithKey f = mapMaybe @(k `Pair` v1) @(k `Pair` v2)
  (uncurry $ \k v -> f k v nothing (just . pair k))

-- | Transform all keys and restore uniqueness by combining a colliding @new@
--   value with the stored @old@ value as @f new old@.
mapKeysWith :: (v -> v -> v) -> Equal k2 -> _ -> Dict k1 v -> Dict k2 v
mapKeysWith f eq key = fromListWith f eq . mapKeys key

-- | Update or delete an existing value; absent keys remain absent.
update :: Equal k -> (v -> Maybe v) -> k -> Dict k v -> Dict k v
update eq f = alter eq (\m -> m nothing f)

-- | For an existing @key@, call @f key value@; 'nothing' deletes the entry and
--   @just value'@ replaces it. An absent key remains absent.
updateWithKey :: Equal k -> (k -> v -> Maybe v) -> k -> Dict k v -> Dict k v
updateWithKey eq f key = update eq (f key) key

------------------------------------------------------------
-- 22. <map> equivalents: filtering, combining and folding
------------------------------------------------------------

-- | 'filterWithKey' keeps the entries whose @(key, value)@ satisfies @p@
--   (the @std::erase_if@-style filter; @Data.Map.filterWithKey@).
filterWithKey :: (k -> v -> Bool) -> Dict k v -> Dict k v
filterWithKey = filter . uncurry

-- | 'filterValues' keeps the entries whose value satisfies @p@ (@Data.Map.filter@).
filterValues :: (v -> Bool) -> Dict k v -> Dict k v
filterValues p = filterWithKey (const p)

-- | Split a dictionary into entries satisfying a key/value predicate and entries
--   rejecting it, preserving relative order in both result dictionaries.
partitionDict :: (k -> v -> Bool) -> Dict k v -> Dict k v `Pair` Dict k v
partitionDict = partition . uncurry

-- | 'unionWith' is the genuine @std::map::merge@ analogue: a left-biased union of two
--   dictionaries that combines shared-key values with @f leftValue rightValue@
--   (@Data.Map.unionWith@). (Contrast the existing 'merge', which aggregates values
--   into lists.)
unionWith :: (v -> v -> v) -> Equal k -> Dict k v -> Dict k v -> Dict k v
unionWith = unionWithKey . const

-- | Combine two dictionaries. Unique keys are retained; a shared key receives
--   @f key leftValue rightValue@.
unionWithKey :: ∀k v. _ -> Equal k -> Dict k v -> Dict k v -> Dict k v
unionWithKey f eq d1 d2 = foldrWithKey @k @v @(Dict k v)
  (\k -> insertWith (f k) eq k) d2 d1

-- | 'union' is the left-biased union (existing keys keep their @d1@ value).
union :: Equal k -> Dict k v -> Dict k v -> Dict k v
union = unionWith const

-- | Combine a list of dictionaries from left to right, resolving every collision
--   as @f earlierValue laterValue@.
unionsWith :: (v -> v -> v) -> Equal k -> List (Dict k v) -> Dict k v
unionsWith f eq = foldl (unionWith f eq) nil

-- | Form the left-biased union of a list of dictionaries, so the earliest value
--   for each key wins.
unions :: Equal k -> List (Dict k v) -> Dict k v
unions = unionsWith const

-- | 'intersectionWith' keeps the keys present in both, combining values with @f@
--   (@Data.Map.intersectionWith@).
intersectionWith :: (v1 -> v2 -> v3) -> Equal k -> Dict k v1 -> Dict k v2 -> Dict k v3
intersectionWith = intersectionWithKey . const

-- | Keep keys present in both dictionaries and assign each shared key
--   @f key leftValue rightValue@.
intersectionWithKey :: _ -> Equal k -> Dict k v1 -> Dict k v2 -> Dict k v3
intersectionWithKey f eq d1 d2 = mapMaybeWithKey
  (\k v1 -> lookup eq k d2 nothing (just . f k v1)) d1

-- | 'intersection' keeps the keys present in both, with @d1@'s values.
intersection :: Equal k -> Dict k v1 -> Dict k v2 -> Dict k v1
intersection = intersectionWith const

-- | 'difference' keeps the entries of @d1@ whose keys are absent from @d2@
--   (@Data.Map.difference@; the many-key generalization of 'deleteKey').
difference :: Equal k -> Dict k v1 -> Dict k v2 -> Dict k v1
difference eq d1 d2 = keySelect (flip (notMember eq) d2) d1

-- | Keep left-only entries unchanged. For a shared key, apply @f leftValue rightValue@;
--   'nothing' removes it and @just value@ retains it with that value.
differenceWith :: (v1 -> v2 -> Maybe v1) -> Equal k -> Dict k v1 -> Dict k v2 -> Dict k v1
differenceWith = differenceWithKey . const

-- | Key-aware 'differenceWith': keep left-only entries unchanged, and for a
--   shared key use @f key leftValue rightValue@ to remove or replace its value.
differenceWithKey :: (k -> v1 -> v2 -> Maybe v1) -> Equal k -> Dict k v1 -> Dict k v2 -> Dict k v1
differenceWithKey f eq d1 d2 = mapMaybeWithKey
  (\k v1 -> lookup eq k d2 (just v1) (f k v1)) d1

-- | 'restrictKeys' keeps only the entries whose key is in @ks@ (@Data.Map.restrictKeys@).
restrictKeys :: Equal k -> Dict k v -> List k -> Dict k v
restrictKeys = flip . keyTake

-- | 'withoutKeys' drops the entries whose key is in @ks@ (@Data.Map.withoutKeys@).
withoutKeys :: Equal k -> Dict k v -> List k -> Dict k v
withoutKeys = flip . keyDrop

-- | Fold entries from right to left in dictionary order, passing each key,
--   value, and accumulated result to the step function.
foldrWithKey :: (k -> v -> b -> b) -> b -> Dict k v -> b
foldrWithKey = foldr . uncurry

-- | Fold entries from left to right in dictionary order, passing the current
--   accumulator, key, and value to the step function.
foldlWithKey :: _ -> b -> Dict k v -> b
foldlWithKey = foldl . (uncurry .)

-- | 'isSubmapOfBy' tests whether every @(k, v1)@ of @d1@ occurs in @d2@ with a value
--   related by @eqV@ (@Data.Map.isSubmapOfBy@).
isSubmapOfBy :: (v1 -> v2 -> Bool) -> Equal k -> Dict k v1 -> Dict k v2 -> Bool
isSubmapOfBy eqV eq d1 d2 =
  all (uncurry $ \k v1 -> lookup eq k d2 false $ eqV v1) d1

-- | Test whether every key-value entry of the first dictionary has an equal
--   key and equal value in the second dictionary.
isSubmapOf :: Equal v -> Equal k -> Dict k v -> Dict k v -> Bool
isSubmapOf = isSubmapOfBy

-- | Test whether the dictionaries have no key in common; value types may differ.
disjoint :: Equal k -> Dict k v1 -> Dict k v2 -> Bool
disjoint eq d1 = nullDict . intersection eq d1

------------------------------------------------------------
-- 23. <numeric> equivalents: folds, products and scans
------------------------------------------------------------

-- | Fold a list left-to-right from an explicit seed, applying @f accumulator element@.
accumulate :: _ -> b -> List a -> b
accumulate = foldl

-- | Reduce a non-empty list left-associatively with no explicit seed; error on @nil@.
reduce :: _ -> List a -> a
reduce = foldl1

-- | Combine corresponding elements with @mul@ and left-fold the results with
--   @add@ from @z@, stopping when either input list ends.
innerProduct :: (c -> d -> c) -> _ -> c -> List a -> List b -> c
innerProduct add mul z xs = foldl add z . zipWith mul xs

-- | Return successive non-empty left reductions: @[x1, f x1 x2, ...]@;
--   return @nil@ for an empty input.
partialSum :: _ -> List a -> List a
partialSum = caseList nil . scanl

-- | 'adjacentDifference' keeps the first element, then applies @f current previous@.
adjacentDifference :: _ -> List a -> List a
adjacentDifference f = caseList nil $ \x xs' ->
  x `cons` zipWith f xs' (x `cons` xs')

-- | Return successive non-empty left reductions under @f@, starting with the
--   first input unchanged; return @nil@ for an empty list.
inclusiveScan :: _ -> List a -> List a
inclusiveScan = partialSum

-- | Return the accumulator value immediately before each input element is
--   incorporated, beginning with @z@; the output length equals the input length.
exclusiveScan :: _ -> a -> List a -> List a
exclusiveScan f z = init . scanl f z

-- | Transform each element with @g@, then left-fold the transformed values
--   with @f@ from seed @z@.
transformReduce :: (b -> b -> b) -> _ -> b -> List a -> b
transformReduce f g z = foldl f z . map g

-- | Combine corresponding elements with the second function and left-fold
--   those results with the first from the seed, stopping at the shorter list.
transformReduce2 :: (c -> d -> c) -> _ -> c -> List a -> List b -> c
transformReduce2 = innerProduct

-- | Transform every input with @g@ and return successive non-empty left
--   reductions under @f@; return @nil@ for an empty input.
transformInclusiveScan :: (b -> b -> b) -> _ -> List a -> List b
transformInclusiveScan f g = partialSum f . map g

-- | Transform inputs with @g@ and return the accumulator preceding each
--   transformed element, beginning with @z@.
transformExclusiveScan :: _ -> _ -> b -> List a -> List b
transformExclusiveScan f g z = exclusiveScan f z . map g

------------------------------------------------------------
-- 24. Typelevel surface parity
------------------------------------------------------------

-- These operations are ordinary higher-order functions here; their Typelevel
-- counterparts require named defunctionalization symbols.  Equality and order
-- remain explicit because the Church library deliberately uses encoded Bool
-- relations instead of Prelude type classes.

isJust :: Maybe a -> Bool
isJust = not . isNothing

dup :: a -> a `Pair` a
dup = pair <*> id

andList :: List Bool -> Bool
andList = foldr and true

orList :: List Bool -> Bool
orList = foldr or false

cycleN :: Int -> List a -> List a
cycleN n = concat . replicate n

iterateN :: Int -> _ -> a -> List a
iterateN n f x = leInt n 0 nil $ x `cons` iterateN (n - 1) f (f x)

range :: Int -> Int -> List Int
range from to = iterateN (to - from) succ from

factorial :: Int -> Int
factorial = product . range 1 . succ

isPalindrome :: Equal a -> List a -> Bool
isPalindrome eq = equalBy eq <*> reverse

isRotation :: Equal a -> List a -> List a -> Bool
isRotation eq xs ys =
  and (on eqInt length xs ys) $ isSubstring eq xs $ append ys ys

notElemBy :: (a -> b -> Bool) -> a -> List b -> Bool
notElemBy eq x = not . elemBy eq x

removeOnce :: Equal a -> a -> List a -> List a
removeOnce = deleteBy

replaceOnce :: Equal a -> a -> a -> List a -> List a
replaceOnce eq old new = caseList nil $ \x rest ->
  eq x old (new `cons` rest) $ x `cons` replaceOnce eq old new rest

intercalate :: ∀a. List a -> List (List a) -> List a
intercalate sep = concat . intersperse @(List a) sep

sum :: List Int -> Int
sum = foldl (+) 0

product :: List Int -> Int
product = foldl (*) 1

maximum :: LE a -> List a -> a
maximum = maximumBy

minimum :: LE a -> List a -> a
minimum = minimumBy

minMax :: LE a -> List a -> a `Pair` a
minMax = minMaxBy

sortBy :: LE a -> List a -> List a
sortBy = mergesort

sort :: LE a -> List a -> List a
sort = sortBy

group :: Equal a -> List a -> List (List a)
group = groupBy

nub :: Equal a -> List a -> List a
nub = nubBy

tally :: Equal a -> List a -> Dict a Int
tally = tallyBy

counts :: Equal a -> List a -> Dict a Int
counts = countsBy

countDistinct :: Equal a -> List a -> Int
countDistinct = countDistinctBy

positionIndex :: Equal a -> List a -> Dict a (List Int)
positionIndex = positionIndexBy

elem :: (a -> b -> Bool) -> a -> List b -> Bool
elem = elemBy

notElem :: (a -> b -> Bool) -> a -> List b -> Bool
notElem = notElemBy

delete :: Equal a -> a -> List a -> List a
delete = deleteBy

intersect :: Equal a -> List a -> List a -> List a
intersect = intersectBy

complement :: (a -> b -> Bool) -> List a -> List b -> List a
complement = complementBy

containsAll :: (a -> b -> Bool) -> List a -> List b -> Bool
containsAll = containsAllBy

containsAny :: (a -> b -> Bool) -> List a -> List b -> Bool
containsAny = containsAnyBy

containsNone :: (a -> b -> Bool) -> List a -> List b -> Bool
containsNone = containsNoneBy

containsOnly :: (a -> b -> Bool) -> List a -> List b -> Bool
containsOnly = containsOnlyBy

unique :: Equal a -> List a -> List a
unique = uniqueBy

isPrefix :: (a -> b -> Bool) -> List a -> List b -> Bool
isPrefix = isPrefixOf

isSuffix :: (a -> b -> Bool) -> List a -> List b -> Bool
isSuffix = isSuffixOf

compareThreeWay :: LE a -> List a -> List a -> Int
compareThreeWay = compareBy

lexLess :: LE a -> List a -> List a -> Bool
lexLess = lexicographicalLess

nextPermutationBy :: LE a -> List a -> Maybe (List a)
nextPermutationBy = nextPermutation

associationThread :: List k -> List v -> Dict k v
associationThread = dictFromLists

assocInsert :: Equal k -> k -> v -> Dict k v -> Dict k v
assocInsert = insertOrUpdate

assocDelete :: Equal k -> k -> Dict k v -> Dict k v
assocDelete = deleteKey

insertOrAssign :: Equal k -> k -> v -> Dict k v -> Dict k v
insertOrAssign = insertOrUpdate
