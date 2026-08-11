{-
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
  and applies it to both elements: λf → f a b.

  This module contains tests for module `Church` declared in `Church.hs`. Unlike
  in the main module, here we can use ADTs like `Bool`, `(,)` and `[]` to facilitate
  testing or call functions from the Prelude to compare results.

  ⚠️ IMPORTANT: The tests must be thorough and cover all edge cases. E.g. consider
  testing lists of different lengths, empty lists, lists with repeated elements,
  etc. For sorting algorithms, test with random, sorted, and reverse-sorted lists.
  Vary the length of the lists to cover various code paths in the algorithms.
-}
{-# LANGUAGE UnicodeSyntax, TypeOperators, ScopedTypeVariables, ImpredicativeTypes, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -fno-max-relevant-binds #-}

module Main where

import Prelude hiding
  ( Bool(..)
  , Either(..)
  , Maybe(..)
  , all
  , and
  , any
  , break
  , catMaybes
  , concat
  , concatMap
  , curry
  , deleteBy
  , deleteFirstsBy
  , drop
  , dropWhile
  , elem
  , either
  , filter
  , foldl
  , foldl1
  , foldr
  , foldr1
  , fst
  , fromJust
  , fromLeft
  , fromMaybe
  , fromRight
  , groupBy
  , head
  , init
  , intersectBy
  , intersperse
  , isLeft
  , isNothing
  , isRight
  , last
  , lefts
  , length
  , listToMaybe
  , lookup
  , map
  , mapAccumL
  , mapAccumR
  , maximum
  , maybeToList
  , minimum
  , nubBy
  , not
  , notElem
  , null
  , partitionEithers
  , permutations
  , replicate
  , reverse
  , rights
  , scanl
  , scanr
  , sequence
  , snd
  , span
  , subsequences
  , sum
  , product
  , swap
  , tail
  , tails
  , take
  , takeWhile
  , transpose
  , traverse
  , splitAt
  , uncurry
  , unionBy
  , uncons
  , unzip
  , zip
  , zipWith
  )
import Church
import qualified Prelude as Prelude'
import Test.HUnit hiding (counts)
import System.Exit
import Control.Monad (Monad(return))
import qualified Control.Exception as Exception
import qualified Data.List
import qualified System.Timeout as Timeout

------------------------------------------------------------
-- Test Helpers
------------------------------------------------------------

fromPreludeList ∷ [a] → List a
fromPreludeList = Prelude'.foldr cons nil

toPreludeList ∷ List a → [a]
toPreludeList xs = xs (:) []

toPreludeTuple ∷ a `Pair` b → (a, b)
toPreludeTuple t = t (,)

toPreludeBool ∷ Bool → Prelude'.Bool
toPreludeBool b = b Prelude'.True Prelude'.False

fromPreludeBool ∷ Prelude'.Bool → Bool
fromPreludeBool b = if b then true else false

toPreludeMaybe ∷ Maybe a → Prelude'.Maybe a
toPreludeMaybe m = m Prelude'.Nothing Prelude'.Just

toPreludeEither ∷ Either a b → Prelude'.Either a b
toPreludeEither e = e Prelude'.Left Prelude'.Right

toPreludePairList ∷ List (a `Pair` b) → [(a, b)]
toPreludePairList = Prelude'.map toPreludeTuple . toPreludeList

assertThrows ∷ String → Prelude'.IO a → Assertion
assertThrows label action = do
  result <- Exception.try action
  case result of
    Prelude'.Left (_ ∷ Exception.SomeException) → return ()
    Prelude'.Right _ → assertFailure (label Prelude'.++ ": expected an exception")

(&) ∷ a → (a → b) → b
(&) = flip ($)

------------------------------------------------------------
-- HUnit Tests
------------------------------------------------------------

main ∷ Prelude'.IO ()
main = do
  counts <- runTestTT allTests
  if errors counts + failures counts > 0
    then Prelude'.putStrLn "FAIL" >> exitFailure
    else Prelude'.putStrLn "PASS" >> exitSuccess

allTests :: Test
allTests = TestList
  [ TestLabel "Pair Tests" pairTests
  , TestLabel "List Tests" listTests
  , TestLabel "Maybe Tests" maybeTests
  , TestLabel "Uncons Tests" unconsTests
  , TestLabel "List Function Tests" listFunctionTests
  , TestLabel "Either Tests" eitherTests
  , TestLabel "Reverse Tests" reverseTests
  , TestLabel "Pair Swap Tests" pairSwapTests
  , TestLabel "List Operation Tests" listOpTests
  , TestLabel "Maybe Operation Tests" maybeOpTests
  , TestLabel "Either Operation Tests" eitherOpTests
  , TestLabel "Matrix Tests" matrixTests
  , TestLabel "Dict Tests" dictTests
  , TestLabel "Sort Tests" sortTests
  , TestLabel "NthElement Tests" nthElementTests
  , TestLabel "NthElement' Tests" nthElementPrimeTests
  , TestLabel "Subsequence Tests" subseqTests
  , TestLabel "Fold Tests" foldTests
  , TestLabel "Concat Tests" concatTests
  , TestLabel "Scanl2 Tests" scanl2Tests
  , TestLabel "Algorithm Tests" algorithmTests
  , TestLabel "Numeric Algorithm Tests" numericAlgorithmTests
  , TestLabel "Dict Algorithm Tests" dictAlgoTests
  , TestLabel "Wolfram-like List Tests" wolframListTests
  , TestLabel "Wolfram-like Association Tests" wolframAssociationTests
  , TestLabel "Wolfram-like Grouping Tests" wolframGroupingTests
  , TestLabel "Whole API Coverage Tests" wholeApiCoverageTests
  , TestLabel "Exhaustive Differential Tests" exhaustiveDifferentialTests
  , TestLabel "Partial and Productivity Tests" partialAndProductivityTests
  , TestLabel "Remaining Branch Tests" remainingBranchTests
  , TestLabel "Typelevel Parity Tests" typelevelParityTests
  ]

pairTests :: Test
pairTests = TestList
  [ TestCase $ assertEqual "fst" 10 (fst $ pair 10 20)
  , TestCase $ assertEqual "snd" 20 (snd $ pair 10 20)
  ]

listTests :: Test
listTests = let xs = fromPreludeList [1,2,3] in TestList
  [ TestCase $ assertEqual "toPreludeList . fromPreludeList" [1,2,3] (toPreludeList xs)
  , TestCase $ assertEqual "null (non-empty)" Prelude'.False (toPreludeBool $ null xs)
  , TestCase $ assertEqual "null (empty)" Prelude'.True (toPreludeBool $ null nil)
  ]

-- Maybe Tests
maybeTests :: Test
maybeTests = TestList
  [ TestCase $ assertEqual "just" "5" (just (5 ∷ Int) "nothing" show)
  , TestCase $ assertEqual "nothing" "nothing" (nothing "nothing" (show ∷ Int → String))
  ]

-- Uncons Tests
unconsTests :: Test
unconsTests = let
    xs = fromPreludeList [1,2,3,4,5] ∷ List Int
    empty = nil ∷ List Int
  in TestList
  [ TestCase $ assertEqual "uncons non-empty"
      (show (1, [2,3,4,5]))
      (uncons xs "nothing" (\p → p (\(x ∷ Int) (ys ∷ List Int) → show (x, toPreludeList ys))))
  , TestCase $ assertEqual "uncons empty"
      "nothing"
      (uncons empty "nothing" (\p → p (\(x ∷ Int) (ys ∷ List Int) → show (x, toPreludeList ys))))
  ]

-- List Function Tests
listFunctionTests :: Test
listFunctionTests = let
    xs = fromPreludeList [1,2,3,4,5]
    ys = fromPreludeList [6,7,8,9,10]
  in TestList
  [ TestCase $ assertEqual "foldr (+)" (Prelude'.foldr (+) 0 [1..5]) (foldr (+) 0 xs)
  , TestCase $ assertEqual "foldl (+)" (Prelude'.foldl (+) 0 [1..5]) (foldl (+) 0 xs)
  , TestCase $ assertEqual "append" ([1..5] Prelude'.++ [6..10]) (toPreludeList $ append xs ys)
  , TestCase $ assertEqual "concatMap"
      (Prelude'.concatMap (\x → [x, x*2]) [1..5])
      (toPreludeList $ concatMap (\x → fromPreludeList [x, x*2]) xs)
  , TestCase $ assertEqual "zipWith (+)"
      (Prelude'.zipWith (+) [1..5] [6..10])
      (toPreludeList $ zipWith (+) xs ys)
  , TestCase $ assertEqual "takeWhile (<4)"
      (Prelude'.takeWhile (<4) [1..5])
      (toPreludeList $ takeWhile (fromPreludeBool . (<4)) xs)
  , TestCase $ assertEqual "tail"
      (Prelude'.tail [1..5])
      (toPreludeList (tail xs))
  , TestCase $ assertEqual "last"
      (Prelude'.last [1..5])
      (last xs)
  , TestCase $ assertEqual "init"
      (Prelude'.init [1..5])
      (toPreludeList $ init xs)
  , TestCase $ assertEqual "zip"
      (Prelude'.zip [1..5] [6..10])
      (Prelude'.map toPreludeTuple . toPreludeList $ zip xs ys)
  -- Test for zipWithN with a sum function
  , TestCase $ assertEqual "zipWithN (sum of elements)"
      [6, 15, 24]
      (toPreludeList $ zipWithN (foldr (+) 0)
      (fromPreludeList [
          fromPreludeList [1, 2, 3],
          fromPreludeList [2, 4, 6],
          fromPreludeList [3, 9, 15]
      ]))

  -- Test for zipWithN with lists of different lengths
  , TestCase $ assertEqual "zipWithN with different length lists"
      [6, 15]
      (toPreludeList $ zipWithN (foldr (+) 0)
      (fromPreludeList [
          fromPreludeList [1, 2],
          fromPreludeList [2, 4, 6],
          fromPreludeList [3, 9, 15, 21]
      ]))

  -- Test for zipWithN with an empty list
  , TestCase $ assertEqual "zipWithN with an empty list"
      []
      (toPreludeList $ zipWithN (foldr (+) 0)
      (fromPreludeList [
          fromPreludeList [1, 2, 3],
          fromPreludeList [],
          fromPreludeList [3, 9, 15]
      ]))

  -- Test for zipWithN with product function
  , TestCase $ assertEqual "zipWithN (product of elements)"
      [6, 72, 270]
      (toPreludeList $ zipWithN (foldr (*) 1)
      (fromPreludeList [
          fromPreludeList [1, 2, 3],
          fromPreludeList [2, 4, 6],
          fromPreludeList [3, 9, 15]
      ]))
  , TestCase $ assertEqual "mapAccumR"
      (Data.List.mapAccumR (\acc x → (acc+x, x*2)) 0 [1..5])
      (toPreludeTuple (mapAccumR (\acc x → pair (acc+x) (x*2)) 0 xs) & fmap toPreludeList)
  , TestCase $ assertEqual "mapAccumL"
      (Data.List.mapAccumL (\acc x → (acc+x, x*2)) 0 [1..5])
      (toPreludeTuple (mapAccumL (\acc x → pair (acc+x) (x*2)) 0 xs) & fmap toPreludeList)
  , TestCase $ assertEqual "scanl (+)"
      (Prelude'.scanl (+) 0 [1..5])
      (toPreludeList $ scanl (+) 0 xs)
  , TestCase $ assertEqual "scanr (+)"
      (Prelude'.scanr (+) 0 [1..5])
      (toPreludeList $ scanr (+) 0 xs)
  , TestCase $ assertEqual "map (*2)"
      (Prelude'.map (*2) [1..5])
      (toPreludeList (map (*2) xs))
  , TestCase $ assertEqual "filter even"
      (Prelude'.filter Prelude'.even [1..5])
      (toPreludeList (filter (fromPreludeBool . Prelude'.even) xs))
  , TestCase $ assertEqual "length"
      (Prelude'.length [1..5])
      (length xs)
  , TestCase $ assertEqual "dropWhile (<3)"
      (Prelude'.dropWhile (<3) [1..5])
      (toPreludeList (dropWhile (fromPreludeBool . (<3)) xs))
  ]

-- Either Tests
eitherTests :: Test
eitherTests = let
    e1 = left (5 ∷ Int)
    e2 = right (10 ∷ Int)
  in TestList
  [ TestCase $ assertEqual "left (applied)" "5" (e1 show (show . negate))
  , TestCase $ assertEqual "right (applied)" "10" (e2 (show . negate) show)
  , TestCase $ assertEqual "isLeft left" Prelude'.True (toPreludeBool $ isLeft e1)
  , TestCase $ assertEqual "isLeft right" Prelude'.False (toPreludeBool $ isLeft e2)
  , TestCase $ assertEqual "isRight left" Prelude'.False (toPreludeBool $ isRight e1)
  , TestCase $ assertEqual "isRight right" Prelude'.True (toPreludeBool $ isRight e2)
  , TestCase $ assertEqual "fromLeft" 5 (fromLeft e1)
  , TestCase $ assertEqual "fromRight" 10 (fromRight e2)
  ]

reverseTests :: Test
reverseTests = let xs = fromPreludeList [1,2,3,4,5] in TestList
  [ TestCase $ assertEqual "reverse" (Prelude'.reverse [1..5]) (toPreludeList $ reverse xs)
  ]

pairSwapTests :: Test
pairSwapTests = let p = pair "hello" 42 in TestList
  [ TestCase $ assertEqual "swap" (42, "hello") (toPreludeTuple $ swap p)
  ]

listOpTests :: Test
listOpTests = let xs = fromPreludeList [1,2,3,4,5] in TestList
  [ TestCase $ assertEqual "snoc" ([1..5] ++ [6]) (toPreludeList $ snoc xs 6)
  , TestCase $ assertEqual "singleton" [7] (toPreludeList $ singleton 7)
  ]

maybeOpTests :: Test
maybeOpTests = TestList
  [ TestCase $ assertEqual "isNothing (nothing)" Prelude'.True (toPreludeBool $ isNothing nothing)
  , TestCase $ assertEqual "isNothing (just)" Prelude'.False (toPreludeBool $ isNothing (just 5))
  , TestCase $ assertEqual "fromJust" 42 (fromJust (just 42))
  , TestCase $ assertEqual "fromMaybe (nothing)" 99 (fromMaybe 99 nothing)
  , TestCase $ assertEqual "fromMaybe (just)" 42 (fromMaybe 99 (just 42))
  , TestCase $ assertEqual "maybeToList (nothing)"
      ([] ∷ [Int])
      (toPreludeList $ maybeToList (nothing ∷ Maybe Int))
  , TestCase $ assertEqual "maybeToList (just)" [42]
      (toPreludeList $ maybeToList (just 42))
  , TestCase $ assertEqual "listToMaybe (empty)"
      Prelude'.True
      (toPreludeBool $ isNothing $ listToMaybe nil)
  , TestCase $ assertEqual "listToMaybe (non-empty)" 1
      (fromJust $ listToMaybe (fromPreludeList [1,2,3,4,5]))
  ]

eitherOpTests :: Test
eitherOpTests = let
    e1 = left "error"
    e2 = right 42
  in TestList
  [ TestCase $ assertEqual "either (left)" 5 (either Prelude'.length (*2) e1)
  , TestCase $ assertEqual "either (right)" 84 (either (Prelude'.length ∷ String → Int) (*2) e2)
  ]

matrixTests :: Test
matrixTests = let
    matrix = fromPreludeList [fromPreludeList [1,2,3], fromPreludeList [4,5,6]] :: List (List Int)
    transposedMatrix = transpose matrix
    strList = fromPreludeList "hello"
    interspersed = intersperse ',' strList
  in TestList
  [ TestCase $ assertEqual "transpose"
      [[1,4],[2,5],[3,6]]
      (toPreludeList (map toPreludeList transposedMatrix))
  , TestCase $ assertEqual "intersperse"
      "h,e,l,l,o"
      (toPreludeList interspersed)
  ]

dictTests :: Test
dictTests = let
    pairList :: Dict Int String
    pairList = pair 1 "one" `cons` (pair 2 "two" `cons` (pair 3 "three" `cons` nil))
    insertResult = insertOrUpdate (\x y -> fromPreludeBool (x == y)) 4 "four" pairList
    updateResult = insertOrUpdate (\x y -> fromPreludeBool (x == y)) 2 "updated two" pairList
    emptyList = nil
    emptyInsertResult = insertOrUpdate (\x y -> fromPreludeBool (x == y)) 1 "one" emptyList
    pairs = fromPreludeList [ pair 1 "foo", pair 2 "foo", pair 3 "bar", pair 4 "foo" ]
            :: List (Int `Pair` String)
    result :: Dict String (List Int)
    result = invert (\x y -> fromPreludeBool (x == y)) pairs
    convertPair :: String `Pair` List Int -> (String, [Int])
    convertPair p = p (\val keys -> (val, toPreludeList keys))
  in TestList
  [ TestCase $ assertEqual "insertOrUpdate (insert new key)"
      [(1, "one"), (2, "two"), (3, "three"), (4, "four")]
      (Prelude'.map toPreludeTuple (toPreludeList insertResult))
  , TestCase $ assertEqual "insertOrUpdate (update existing key)"
      [(1, "one"), (2, "updated two"), (3, "three")]
      (Prelude'.map toPreludeTuple (toPreludeList updateResult))
  , TestCase $ assertEqual "insertOrUpdate (empty list)"
      [(1, "one")]
      (Prelude'.map toPreludeTuple (toPreludeList emptyInsertResult))
  , TestCase $ assertEqual "invert"
      [("foo",[1,2,4]), ("bar",[3])]
      (Prelude'.map convertPair (toPreludeList result))
  ]

sortTests :: Test
sortTests = let
    xs = fromPreludeList [5, 3, 1, 4, 2] :: List Int
    ys = fromPreludeList [1, 2, 3, 4, 5] :: List Int
    le :: LE Int
    le x y = fromPreludeBool (x Prelude'.<= y)

    quickSorted = quicksort le xs
    mergeSorted = mergesort le xs
    heapSorted = heapsort le xs
    introSorted = introsort le xs
  in TestList
  [ TestCase $ assertEqual "quicksort" (toPreludeList ys) (toPreludeList quickSorted)
  , TestCase $ assertEqual "mergesort" (toPreludeList ys) (toPreludeList mergeSorted)
  , TestCase $ assertEqual "heapsort" (toPreludeList ys) (toPreludeList heapSorted)
  , TestCase $ assertEqual "introsort" (toPreludeList ys) (toPreludeList introSorted)
  ]

nthElementTests :: Test
nthElementTests = let
    le :: LE Int
    le x y = fromPreludeBool (x Prelude'.<= y)

    emptyList = nil :: List Int
    singletonList = fromPreludeList [42] :: List Int
    unsortedList = fromPreludeList [5, 3, 1, 4, 2] :: List Int
    sortedList = fromPreludeList [1, 2, 3, 4, 5] :: List Int

  in TestList
  [ TestCase $ assertEqual "nthElement - empty list"
      (toPreludeList emptyList)
      (toPreludeList $ nthElement le 0 emptyList)

  , TestCase $ assertEqual "nthElement - singleton list"
      (toPreludeList singletonList)
      (toPreludeList $ nthElement le 0 singletonList)

  , TestCase $ assertEqual "nthElement - complete sort"
      (toPreludeList sortedList)
      (toPreludeList $ nthElement le 2 unsortedList)

  , TestCase $ assertEqual "nthElement - larger array"
      (Data.List.sort [9,3,7,1,5,8,2,6,4])
      (toPreludeList $ nthElement le 4 (fromPreludeList [9,3,7,1,5,8,2,6,4]))

  , TestCase $ assertEqual "nthElement - finds median element"
      3
      (Prelude'.foldr (\(i, x) acc -> if i == 2 then x else acc) 0 $
        Prelude'.zip [0..] $ toPreludeList $ nthElement le 2 unsortedList)
  ]

-- Property tests for the introselect 'nthElement''. Its output is only PARTIALLY
-- sorted (the n-th order statistic at index n, with <= before and >= after), so we
-- check that nth_element property rather than a fixed list. These cover the cases
-- that the original (buggy) implementation got wrong: the 'less' branch dropping
-- elements and the 'greater' branch using a stale index.
nthElementPrimeTests :: Test
nthElementPrimeTests = let
    le :: LE Int
    le x y = fromPreludeBool (x Prelude'.<= y)
    prop :: Int -> [Int] -> Prelude'.Bool
    prop n xs =
      let out = toPreludeList (nthElement' le n (fromPreludeList xs))
          s   = Data.List.sort xs
          piv = s Prelude'.!! n
      in (Data.List.sort out Prelude'.== s)
         Prelude'.&& ((out Prelude'.!! n) Prelude'.== piv)
         Prelude'.&& Prelude'.all (Prelude'.<= piv) (Prelude'.take n out)
         Prelude'.&& Prelude'.all (Prelude'.>= piv) (Prelude'.drop (n Prelude'.+ 1) out)
  in TestList
  [ TestCase $ assertBool "nthElement' singleton"        (prop 0 [42])
  , TestCase $ assertBool "nthElement' median"           (prop 2 [5,3,1,4,2])
  , TestCase $ assertBool "nthElement' less-branch loss" (prop 2 [5,4,3,2,1])
  , TestCase $ assertBool "nthElement' larger array n=3" (prop 3 [9,3,7,1,5,8,2,6,4])
  , TestCase $ assertBool "nthElement' first index"      (prop 0 [9,3,7,1,5,8,2,6,4])
  , TestCase $ assertBool "nthElement' last index"       (prop 8 [9,3,7,1,5,8,2,6,4])
  , TestCase $ assertBool "nthElement' duplicates"       (prop 2 [3,3,1,2,2])
  , TestCase $ assertBool "nthElement' all equal"        (prop 1 [7,7,7,7])
  ]

subseqTests :: Test
subseqTests = let
    xs1 = fromPreludeList "abc"
    xs2 = fromPreludeList []
  in TestList
  [ TestCase $ assertEqual "subsequences (non-empty)"
      (Data.List.sort ["","a","b","ab","c","ac","bc","abc"])
      (Data.List.sort $ Prelude'.map toPreludeList $ toPreludeList $ subsequences xs1)
  , TestCase $ assertEqual "subsequences (empty)"
      ([[]] :: [[Char]])
      (Prelude'.map toPreludeList $ toPreludeList $ subsequences xs2)
  , TestCase $ assertEqual "permutations [1,2,3]"
      (Data.List.sort $ Data.List.permutations [1,2,3])
      (Data.List.sort $ Prelude'.map toPreludeList $ toPreludeList $ permutations (fromPreludeList [1,2,3]))
  ]

foldTests :: Test
foldTests = let
    xs = fromPreludeList [1,2,3,4,5]
  in TestList
  [ TestCase $ assertEqual "foldl1 (+)"
      (Prelude'.foldl1 (+) [1..5])
      (foldl1 (+) xs)
  , TestCase $ assertEqual "foldr1 (+)"
      (Prelude'.foldr1 (+) [1..5])
      (foldr1 (+) xs)
  , TestCase $ assertEqual "foldl1 (-)"
      (Prelude'.foldl1 (-) [1..5])
      (foldl1 (-) xs)
  , TestCase $ assertEqual "foldr1 (-)"
      (Prelude'.foldr1 (-) [1..5])
      (foldr1 (-) xs)
  ]

concatTests :: Test
concatTests = let
    xss = fromPreludeList [fromPreludeList [1,2], fromPreludeList [3,4,5], fromPreludeList [6,7]]
           :: List (List Int)
  in TestList
  [ TestCase $ assertEqual "concat"
      [1..7]
      (toPreludeList $ concat xss)
  ]

scanl2Tests :: Test
scanl2Tests = TestList
  [ TestCase $ assertEqual "scanl2 (+) with equal length lists"
      [0, 5, 12, 21, 32]
      (toPreludeList $ scanl2 (\acc x y -> acc + x + y) 0 (fromPreludeList [1,2,3,4]) (fromPreludeList [4,5,6,7]))

  , TestCase $ assertEqual "scanl2 (*) with equal length lists"
      [1, 2, 12, 144, 3456]
      (toPreludeList $ scanl2 (\acc x y -> acc * x * y) 1 (fromPreludeList [1,2,3,4]) (fromPreludeList [2,3,4,6]))

  , TestCase $ assertEqual "scanl2 with first list shorter"
      [0, 5, 12, 21]
      (toPreludeList $ scanl2 (\acc x y -> acc + x + y) 0 (fromPreludeList [1,2,3]) (fromPreludeList [4,5,6,7,8]))

  , TestCase $ assertEqual "scanl2 with second list shorter"
      [0, 5, 12, 21]
      (toPreludeList $ scanl2 (\acc x y -> acc + x + y) 0 (fromPreludeList [1,2,3,4,5]) (fromPreludeList [4,5,6]))

  , TestCase $ assertEqual "scanl2 with empty first list"
      [0]
      (toPreludeList $ scanl2 (\acc x y -> acc + x + y) 0 nil (fromPreludeList [4,5,6]))

  , TestCase $ assertEqual "scanl2 with empty second list"
      [0]
      (toPreludeList $ scanl2 (\acc x y -> acc + x + y) 0 (fromPreludeList [1,2,3]) nil)

  , TestCase $ assertEqual "scanl2 with both lists empty"
      [0]
      (toPreludeList $ scanl2 (\acc x y -> acc + x + y) 0 nil nil)

  , TestCase $ assertEqual "scanl2 with non-commutative operation"
      [5, 0, -3, -8]
      (toPreludeList $ scanl2 (\acc x y -> acc - x - y) 5 (fromPreludeList [1,2,3]) (fromPreludeList [4,1,2]))

  , TestCase $ assertEqual "scanl2 with custom pair function"
      [0, 4, 13, 23]
      (toPreludeList $ scanl2 (\acc x y -> acc + (x * y)) 0
                      (fromPreludeList [1,3,5]) (fromPreludeList [4,3,2]))
  ]

------------------------------------------------------------
-- <algorithm> equivalents (Church.hs sections 13-19)
------------------------------------------------------------

algorithmTests :: Test
algorithmTests = let
    le :: LE Int
    le x y = fromPreludeBool (x Prelude'.<= y)
    eq :: Int -> Int -> Bool
    eq x y = fromPreludeBool (x Prelude'.== y)
    fl  = fromPreludeList
    tl  = toPreludeList
    toMb :: Maybe Int -> Prelude'.Maybe Int
    toMb m = m Prelude'.Nothing Prelude'.Just
    toMbL :: Maybe (List Int) -> Prelude'.Maybe [Int]
    toMbL m = m Prelude'.Nothing (\l -> Prelude'.Just (toPreludeList l))
    toMbTagged :: Maybe (List (Int `Pair` Int)) -> Prelude'.Maybe [(Int, Int)]
    toMbTagged m = m Prelude'.Nothing (Prelude'.Just . Prelude'.map toPreludeTuple . toPreludeList)
    taggedLE :: LE (Int `Pair` Int)
    taggedLE p q = le (fst p) (fst q)
    tagged :: List (Int `Pair` Int)
    tagged = 0 `pair` 0 `cons` (2 `pair` 20 `cons` (1 `pair` 10 `cons` (1 `pair` 11 `cons` nil)))
    toMbT :: Maybe (Int `Pair` Int) -> Prelude'.Maybe (Int, Int)
    toMbT m = m Prelude'.Nothing (\p -> Prelude'.Just (toPreludeTuple p))
    even' = fromPreludeBool . Prelude'.even
    nextP :: [Int] -> Prelude'.Maybe [Int]
    nextP cur = toMbL (nextPermutation le (fl cur))
    allPerms :: [Int] -> [[Int]]
    allPerms start = start : (case nextP start of
                                Prelude'.Nothing -> []
                                Prelude'.Just n  -> allPerms n)
  in TestList
  [ TestCase $ assertEqual "none (all<10)" Prelude'.True  (toPreludeBool $ none (fromPreludeBool . (Prelude'.> 10)) (fl [1,2,3]))
  , TestCase $ assertEqual "none (has even)" Prelude'.False (toPreludeBool $ none even' (fl [1,2,3]))
  , TestCase $ assertEqual "findIf even"  (Prelude'.Just 2) (toMb $ findIf even' (fl [1,2,3,4]))
  , TestCase $ assertEqual "findIf none"  Prelude'.Nothing  (toMb $ findIf (fromPreludeBool . (Prelude'.> 9)) (fl [1,2,3]))
  , TestCase $ assertEqual "findIfNot"    (Prelude'.Just 1) (toMb $ findIfNot even' (fl [2,4,1,6]))
  , TestCase $ assertEqual "find 3"       (Prelude'.Just 3) (toMb $ find eq 3 (fl [1,2,3,4]))
  , TestCase $ assertEqual "findLast even"(Prelude'.Just 4) (toMb $ findLast even' (fl [1,2,3,4,5]))
  , TestCase $ assertEqual "countIf even" 2 (countIf even' (fl [1,2,3,4]))
  , TestCase $ assertEqual "count 2"      3 (count eq 2 (fl [2,1,2,3,2]))
  , TestCase $ assertEqual "mismatch"     (Prelude'.Just (2,9)) (toMbT (mismatch eq (fl [1,2,3]) (fl [1,9,3])))
  , TestCase $ assertEqual "mismatch none" Prelude'.Nothing (toMbT (mismatch eq (fl [1,2]) (fl [1,2,3])))
  , TestCase $ assertEqual "adjacentFind" (Prelude'.Just 2) (toMb $ adjacentFind eq (fl [1,2,2,3]))
  , TestCase $ assertEqual "search"       (Prelude'.Just 1) (toMb $ search eq (fl [2,3]) (fl [1,2,3,4]))
  , TestCase $ assertEqual "search none"  Prelude'.Nothing  (toMb $ search eq (fl [9,9]) (fl [1,2,3]))
  , TestCase $ assertEqual "findEnd"      (Prelude'.Just 4) (toMb $ findEnd eq (fl [1]) (fl [1,2,1,3,1]))
  , TestCase $ assertEqual "findFirstOf"  (Prelude'.Just 2) (toMb $ findFirstOf eq (fl [1,2,3]) (fl [9,8,2]))
  , TestCase $ assertEqual "searchN"      (Prelude'.Just 2) (toMb $ searchN eq 2 7 (fl [7,1,7,7,2]))
  , TestCase $ assertEqual "take 3"  [1,2,3] (tl $ take 3 (fl [1,2,3,4,5]))
  , TestCase $ assertEqual "drop 2"  [3,4,5] (tl $ drop 2 (fl [1,2,3,4,5]))
  , TestCase $ assertEqual "rotate 2" [3,4,5,1,2] (tl $ rotate 2 (fl [1,2,3,4,5]))
  , TestCase $ assertEqual "removeIf even" [1,3] (tl $ removeIf even' (fl [1,2,3,4]))
  , TestCase $ assertEqual "remove 2" [1,3] (tl $ remove eq 2 (fl [1,2,3,2]))
  , TestCase $ assertEqual "replaceIf even 0" [1,0,3,0] (tl $ replaceIf even' 0 (fl [1,2,3,4]))
  , TestCase $ assertEqual "replace 2 9" [1,9,3,9] (tl $ replace eq 2 9 (fl [1,2,3,2]))
  , TestCase $ assertEqual "uniqueBy" [1,2,3,1] (tl $ uniqueBy eq (fl [1,1,2,3,3,3,1]))
  , TestCase $ assertEqual "isPartitioned T" Prelude'.True  (toPreludeBool $ isPartitioned even' (fl [2,4,1,3]))
  , TestCase $ assertEqual "isPartitioned F" Prelude'.False (toPreludeBool $ isPartitioned even' (fl [2,1,4]))
  , TestCase $ assertEqual "partitionPoint"  2 (partitionPoint even' (fl [2,4,1,3]))
  , TestCase $ assertEqual "isSorted T" Prelude'.True  (toPreludeBool $ isSorted le (fl [1,2,2,3]))
  , TestCase $ assertEqual "isSorted F" Prelude'.False (toPreludeBool $ isSorted le (fl [1,3,2]))
  , TestCase $ assertEqual "isSortedUntil" 2 (isSortedUntil le (fl [1,2,1,3]))
  , TestCase $ assertEqual "lowerBound 2" 1 (lowerBound le 2 (fl [1,2,2,3]))
  , TestCase $ assertEqual "upperBound 2" 3 (upperBound le 2 (fl [1,2,2,3]))
  , TestCase $ assertEqual "binarySearch T" Prelude'.True  (toPreludeBool $ binarySearch le 3 (fl [1,2,3,4]))
  , TestCase $ assertEqual "binarySearch F" Prelude'.False (toPreludeBool $ binarySearch le 9 (fl [1,2,3,4]))
  , TestCase $ assertEqual "equalRange" (1,3) (toPreludeTuple $ equalRange le 2 (fl [1,2,2,3]))
  , TestCase $ assertEqual "mergeBy" [1,2,3,4,5] (tl $ mergeBy le (fl [1,3,5]) (fl [2,4]))
  , TestCase $ assertEqual "includes T" Prelude'.True  (toPreludeBool $ includes le (fl [1,2,2,3]) (fl [2,3]))
  , TestCase $ assertEqual "includes F" Prelude'.False (toPreludeBool $ includes le (fl [1,2,3]) (fl [2,2]))
  , TestCase $ assertEqual "setUnion" [1,2,3,4] (tl $ setUnion le (fl [1,2,3]) (fl [2,3,4]))
  , TestCase $ assertEqual "setIntersection" [2,3] (tl $ setIntersection le (fl [1,2,3]) (fl [2,3,4]))
  , TestCase $ assertEqual "setDifference" [1,3] (tl $ setDifference le (fl [1,2,3,4]) (fl [2,4]))
  , TestCase $ assertEqual "setSymmetricDifference" [1,4] (tl $ setSymmetricDifference le (fl [1,2,3]) (fl [2,3,4]))
  , TestCase $ assertEqual "maxBy" 5 (maxBy le 3 5)
  , TestCase $ assertEqual "minBy" 3 (minBy le 3 5)
  , TestCase $ assertEqual "minmaxBy" (3,5) (toPreludeTuple $ minmaxBy le 5 3)
  , TestCase $ assertEqual "clamp hi" 10 (clamp le 0 10 15)
  , TestCase $ assertEqual "clamp lo" 0  (clamp le 0 10 (Prelude'.negate 5))
  , TestCase $ assertEqual "clamp in" 5  (clamp le 0 10 5)
  , TestCase $ assertEqual "minmaxElement" (1,5) (toPreludeTuple $ minmaxElement le (fl [3,1,4,1,5]))
  , TestCase $ assertEqual "equalBy T" Prelude'.True  (toPreludeBool $ equalBy eq (fl [1,2,3]) (fl [1,2,3]))
  , TestCase $ assertEqual "equalBy F" Prelude'.False (toPreludeBool $ equalBy eq (fl [1,2]) (fl [1,2,3]))
  , TestCase $ assertEqual "lexLess T" Prelude'.True  (toPreludeBool $ lexicographicalLess le (fl [1,2]) (fl [1,3]))
  , TestCase $ assertEqual "lexLess prefix" Prelude'.True (toPreludeBool $ lexicographicalLess le (fl [1]) (fl [1,2]))
  , TestCase $ assertEqual "lexLess F" Prelude'.False (toPreludeBool $ lexicographicalLess le (fl [1,2]) (fl [1,2]))
  , TestCase $ assertEqual "compareBy lt" (Prelude'.negate 1) (compareBy le (fl [1,2]) (fl [1,3]))
  , TestCase $ assertEqual "compareBy eq" 0 (compareBy le (fl [1,2]) (fl [1,2]))
  , TestCase $ assertEqual "compareBy gt" 1 (compareBy le (fl [2]) (fl [1]))
  , TestCase $ assertEqual "isPermutation T" Prelude'.True  (toPreludeBool $ isPermutation eq (fl [1,2,3]) (fl [3,2,1]))
  , TestCase $ assertEqual "isPermutation F" Prelude'.False (toPreludeBool $ isPermutation eq (fl [1,2,3]) (fl [1,2,4]))
  , TestCase $ assertEqual "nextPermutation step" (Prelude'.Just [1,3,2]) (toMbL $ nextPermutation le (fl [1,2,3]))
  , TestCase $ assertEqual "nextPermutation preserves equivalent identities"
      (Prelude'.Just [(1,10),(0,0),(1,11),(2,20)])
      (toMbTagged $ nextPermutation taggedLE tagged)
  , TestCase $ assertEqual "nextPermutation last" Prelude'.Nothing (toMbL $ nextPermutation le (fl [3,2,1]))
  , TestCase $ assertEqual "prevPermutation" (Prelude'.Just [1,3,2]) (toMbL $ prevPermutation le (fl [2,1,3]))
  , TestCase $ assertEqual "nextPermutation enumerates all (lex order)"
      (Data.List.sort (Data.List.permutations [1,2,3]))
      (Data.List.sort (allPerms [1,2,3]))
  , TestCase $ assertEqual "nextPermutation count == 4!"
      24 (Prelude'.length (allPerms [1,2,3,4]))
  , TestCase $ assertEqual "on" 12 (on (+) Prelude'.abs (Prelude'.negate 5) 7)
  , TestCase $ assertEqual "nubOn" [-1,-2,3]
      (tl $ nubOn eq Prelude'.abs (fl [-1,1,-2,2,3]))
  , TestCase $ assertEqual "groupOn" [[1,3],[2,4],[5]]
      (Prelude'.map tl $ tl $ groupOn eq (`Prelude'.mod` 2) (fl [1,3,2,4,5]))
  , TestCase $ assertEqual "sortOn" [0,1,-2,-3]
      (tl $ sortOn le Prelude'.abs (fl [-3,1,-2,0]))
  , TestCase $ assertEqual "sortOn stable" [-1,1,-2,2]
      (tl $ sortOn le Prelude'.abs (fl [-2,2,-1,1]))
  , TestCase $ assertEqual "minimumOn" 1 (minimumOn le Prelude'.abs (fl [-3,1,-2]))
  , TestCase $ assertEqual "maximumOn" (-3) (maximumOn le Prelude'.abs (fl [-3,1,-2]))
  ]

------------------------------------------------------------
-- <numeric> equivalents (Church.hs section 23)
------------------------------------------------------------

numericAlgorithmTests :: Test
numericAlgorithmTests = let
    fl = fromPreludeList
    tl = toPreludeList
  in TestList
  [ TestCase $ assertEqual "accumulate" 16 (accumulate (+) 10 (fl [1,2,3]))
  , TestCase $ assertEqual "accumulate order" 4 (accumulate (-) 10 (fl [1,2,3]))
  , TestCase $ assertEqual "reduce" 24 (reduce (*) (fl [2,3,4]))
  , TestCase $ assertEqual "innerProduct" 32
      (innerProduct (+) (*) 0 (fl [1,2,3]) (fl [4,5,6]))
  , TestCase $ assertEqual "innerProduct truncates" 14
      (innerProduct (+) (*) 0 (fl [1,2,3]) (fl [4,5]))
  , TestCase $ assertEqual "partialSum" [1,3,6,10] (tl $ partialSum (+) (fl [1,2,3,4]))
  , TestCase $ assertEqual "partialSum empty" [] (tl $ partialSum (+) (fl ([] :: [Int])))
  , TestCase $ assertEqual "adjacentDifference" [1,3,5,7]
      (tl $ adjacentDifference (-) (fl [1,4,9,16]))
  , TestCase $ assertEqual "inclusiveScan" [1,3,6] (tl $ inclusiveScan (+) (fl [1,2,3]))
  , TestCase $ assertEqual "exclusiveScan" [0,1,3] (tl $ exclusiveScan (+) 0 (fl [1,2,3]))
  , TestCase $ assertEqual "transformReduce" 12
      (transformReduce (+) (*2) 0 (fl [1,2,3]))
  , TestCase $ assertEqual "transformReduce2" 32
      (transformReduce2 (+) (*) 0 (fl [1,2,3]) (fl [4,5,6]))
  , TestCase $ assertEqual "transformInclusiveScan" [2,6,12]
      (tl $ transformInclusiveScan (+) (*2) (fl [1,2,3]))
  , TestCase $ assertEqual "transformExclusiveScan" [10,12,16]
      (tl $ transformExclusiveScan (+) (*2) 10 (fl [1,2,3]))
  ]

------------------------------------------------------------
-- <map>/<unordered_map> dictionary equivalents (Church.hs sections 20-22)
------------------------------------------------------------

dictAlgoTests :: Test
dictAlgoTests = let
    eq :: Int -> Int -> Bool
    eq x y = fromPreludeBool (x Prelude'.== y)
    mkDict :: [(Int, Int)] -> Dict Int Int
    mkDict = Prelude'.foldr (\(k, v) acc -> pair k v `cons` acc) nil
    toAL :: Dict Int Int -> [(Int, Int)]
    toAL d = Data.List.sortOn Prelude'.fst (Prelude'.map toPreludeTuple (toPreludeList d))
    toMb :: Maybe Int -> Prelude'.Maybe Int
    toMb m = m Prelude'.Nothing Prelude'.Just
    d12  = mkDict [(1,10),(2,20)]
    dmm  = mkDict [(1,10),(2,20),(1,30)]   -- multimap (duplicate key 1)
  in TestList
  [ -- query
    TestCase $ assertEqual "member T" Prelude'.True  (toPreludeBool $ member eq 2 d12)
  , TestCase $ assertEqual "member F" Prelude'.False (toPreludeBool $ member eq 9 d12)
  , TestCase $ assertEqual "notMember" Prelude'.True (toPreludeBool $ notMember eq 9 d12)
  , TestCase $ assertEqual "findWithDefault hit"  20 (findWithDefault 0 eq 2 d12)
  , TestCase $ assertEqual "findWithDefault miss" 0  (findWithDefault 0 eq 9 d12)
  , TestCase $ assertEqual "atKey" 10 (atKey eq 1 d12)
  , TestCase $ assertEqual "keys"   [1,2]   (Data.List.sort $ toPreludeList $ keys d12)
  , TestCase $ assertEqual "values" [10,20] (Data.List.sort $ toPreludeList $ values d12)
  , TestCase $ assertEqual "elems"  [10,20] (Data.List.sort $ toPreludeList $ elems d12)
  , TestCase $ assertEqual "sizeDict" 2 (sizeDict d12)
  , TestCase $ assertEqual "nullDict T" Prelude'.True  (toPreludeBool $ nullDict (nil :: Dict Int Int))
  , TestCase $ assertEqual "nullDict F" Prelude'.False (toPreludeBool $ nullDict d12)
  , TestCase $ assertEqual "singletonDict" [(7,8)] (toAL $ singletonDict 7 8)
  , TestCase $ assertEqual "lookupAll (multimap)" [10,30] (Data.List.sort $ toPreludeList $ lookupAll eq 1 dmm)
  , TestCase $ assertEqual "countKey" 2 (countKey eq 1 dmm)
  -- insertion / construction / update
  , TestCase $ assertEqual "insert absent"  [(1,10),(2,20),(3,30)] (toAL $ insert eq 3 30 d12)
  , TestCase $ assertEqual "insert present (no overwrite)" [(1,10),(2,20)] (toAL $ insert eq 1 99 d12)
  , TestCase $ assertEqual "insertWith present" [(1,15),(2,20)] (toAL $ insertWith (+) eq 1 5 d12)
  , TestCase $ assertEqual "insertWith absent"  [(1,10),(2,20),(3,5)] (toAL $ insertWith (+) eq 3 5 d12)
  , TestCase $ assertEqual "fromList (last wins)" [(1,30),(2,20)] (toAL $ fromList eq (mkDict [(1,10),(2,20),(1,30)]))
  , TestCase $ assertEqual "fromListWith (+)" [(1,40),(2,20)] (toAL $ fromListWith (+) eq (mkDict [(1,10),(2,20),(1,30)]))
  , TestCase $ assertEqual "adjust" [(1,11),(2,20)] (toAL $ adjust eq (+1) 1 d12)
  , TestCase $ assertEqual "adjust miss" [(1,10),(2,20)] (toAL $ adjust eq (+1) 9 d12)
  , TestCase $ assertEqual "alter delete" [(2,20)] (toAL $ alter eq (\_ -> nothing) 1 d12)
  , TestCase $ assertEqual "alter insert" [(1,10),(2,20),(3,99)] (toAL $ alter eq (\_ -> just 99) 3 d12)
  , TestCase $ assertEqual "alter update" [(1,11),(2,20)] (toAL $ alter eq (\m -> m (just 0) (\v -> just (v+1))) 1 d12)
  , TestCase $ assertEqual "mapWithKey" [(1,11),(2,22)] (toAL $ mapWithKey (\k v -> k+v) d12)
  , TestCase $ assertEqual "update" [(1,11),(2,20)] (toAL $ update eq (just . (+1)) 1 d12)
  , TestCase $ assertEqual "update delete" [(2,20)] (toAL $ update eq (const nothing) 1 d12)
  , TestCase $ assertEqual "update miss" [(1,10),(2,20)] (toAL $ update eq (just . (+1)) 9 d12)
  , TestCase $ assertEqual "updateWithKey" [(1,11),(2,20)]
      (toAL $ updateWithKey eq (\k v -> just (k+v)) 1 d12)
  , TestCase $ assertEqual "mapMaybeWithKey" [(2,22)]
      (toAL $ mapMaybeWithKey (\k v -> fromPreludeBool (Prelude'.even k) (just (k+v)) nothing) d12)
  , TestCase $ assertEqual "mapKeysWith" [(0,20),(1,40)]
      (toAL $ mapKeysWith (+) eq (`Prelude'.mod` 2) (mkDict [(1,10),(2,20),(3,30)]))
  , TestCase $ assertEqual "mapKeysWith new-old order" [(1,20)]
      (toAL $ mapKeysWith (-) eq (const 1) (mkDict [(1,10),(2,30)]))
  -- filtering / combining / folding
  , TestCase $ assertEqual "filterWithKey (even key)" [(2,20)] (toAL $ filterWithKey (\k _ -> fromPreludeBool (Prelude'.even k)) d12)
  , TestCase $ assertEqual "filterValues (>15)" [(2,20)] (toAL $ filterValues (\v -> fromPreludeBool (v Prelude'.> 15)) d12)
  , TestCase $ assertEqual "partitionDict" ([(2,20)],[(1,10)])
      (let p = partitionDict (\k _ -> fromPreludeBool (Prelude'.even k)) d12
       in p (\yes no -> (toAL yes, toAL no)))
  , TestCase $ assertEqual "union (left-biased)" [(1,10),(2,20),(3,30)]
      (toAL $ union eq d12 (mkDict [(2,99),(3,30)]))
  , TestCase $ assertEqual "unionWith (+)" [(1,3),(2,3)]
      (toAL $ unionWith (+) eq (mkDict [(1,1)]) (mkDict [(1,2),(2,3)]))
  , TestCase $ assertEqual "unionWithKey" [(1,13),(2,3)]
      (toAL $ unionWithKey (\k a b -> k+a+b) eq (mkDict [(1,10)]) (mkDict [(1,2),(2,3)]))
  , TestCase $ assertEqual "unionWithKey left-right order" [(1,9)]
      (toAL $ unionWithKey (\k a b -> k+a-b) eq (mkDict [(1,10)]) (mkDict [(1,2)]))
  , TestCase $ assertEqual "unionsWith (+)" [(1,3),(2,3)]
      (toAL $ unionsWith (+) eq (mkDict [(1,1)] `cons` (mkDict [(1,2),(2,3)] `cons` nil)))
  , TestCase $ assertEqual "unions" [(1,1),(2,3)]
      (toAL $ unions eq (mkDict [(1,1)] `cons` (mkDict [(1,2),(2,3)] `cons` nil)))
  , TestCase $ assertEqual "intersectionWith (+)" [(2,22)]
      (toAL $ intersectionWith (+) eq d12 (mkDict [(2,2),(3,3)]))
  , TestCase $ assertEqual "intersectionWithKey" [(2,24)]
      (toAL $ intersectionWithKey (\k a b -> k+a+b) eq d12 (mkDict [(2,2),(3,3)]))
  , TestCase $ assertEqual "intersection (left)" [(2,20)]
      (toAL $ intersection eq d12 (mkDict [(2,99),(3,30)]))
  , TestCase $ assertEqual "difference" [(1,10)]
      (toAL $ difference eq d12 (mkDict [(2,99),(3,30)]))
  , TestCase $ assertEqual "differenceWith update" [(1,10),(2,18)]
      (toAL $ differenceWith (\a b -> just (a-b)) eq d12 (mkDict [(2,2),(3,3)]))
  , TestCase $ assertEqual "differenceWith delete" [(1,10)]
      (toAL $ differenceWith (\_ _ -> nothing) eq d12 (mkDict [(2,2),(3,3)]))
  , TestCase $ assertEqual "differenceWithKey" [(1,10),(2,20)]
      (toAL $ differenceWithKey (\k a b -> just (k+a-b)) eq d12 (mkDict [(2,2),(3,3)]))
  , TestCase $ assertEqual "restrictKeys" [(2,20)]
      (toAL $ restrictKeys eq d12 (fromPreludeList [2,9]))
  , TestCase $ assertEqual "withoutKeys" [(1,10)]
      (toAL $ withoutKeys eq d12 (fromPreludeList [2,9]))
  , TestCase $ assertEqual "foldrWithKey (sum k+v)" 33 (foldrWithKey (\k v acc -> k+v+acc) 0 d12)
  , TestCase $ assertEqual "foldlWithKey (sum k+v)" 33 (foldlWithKey (\acc k v -> acc+k+v) 0 d12)
  , TestCase $ assertEqual "isSubmapOfBy T" Prelude'.True
      (toPreludeBool $ isSubmapOfBy (\a b -> fromPreludeBool (a Prelude'.== b)) eq (mkDict [(1,10)]) d12)
  , TestCase $ assertEqual "isSubmapOfBy F" Prelude'.False
      (toPreludeBool $ isSubmapOfBy (\a b -> fromPreludeBool (a Prelude'.== b)) eq (mkDict [(1,99)]) d12)
  , TestCase $ assertEqual "isSubmapOf" Prelude'.True
      (toPreludeBool $ isSubmapOf eq eq (mkDict [(1,10)]) d12)
  , TestCase $ assertEqual "disjoint T" Prelude'.True
      (toPreludeBool $ disjoint eq d12 (mkDict [(3,30)]))
  , TestCase $ assertEqual "disjoint F" Prelude'.False
      (toPreludeBool $ disjoint eq d12 (mkDict [(2,99)]))
  ]

wolframListTests :: Test
wolframListTests = let
    xs = fromPreludeList [1,2,3,4,5] :: List Int
    split = splitAt 3 xs
  in TestList
  [ TestCase $ assertEqual "at is zero-based"
      3
      (at 2 xs)
  , TestCase $ assertEqual "take"
      [1,2,3]
      (toPreludeList $ take 3 xs)
  , TestCase $ assertEqual "take beyond end"
      [1,2,3,4,5]
      (toPreludeList $ take 10 xs)
  , TestCase $ assertEqual "take non-positive"
      []
      (toPreludeList $ take 0 xs)
  , TestCase $ assertEqual "drop"
      [4,5]
      (toPreludeList $ drop 3 xs)
  , TestCase $ assertEqual "drop beyond end"
      []
      (toPreludeList $ drop 10 xs)
  , TestCase $ assertEqual "splitAt"
      ([1,2,3], [4,5])
      (split $ \l r -> (toPreludeList l, toPreludeList r))
  , TestCase $ assertEqual "takeLast"
      [4,5]
      (toPreludeList $ takeLast 2 xs)
  , TestCase $ assertEqual "dropLast"
      [1,2,3]
      (toPreludeList $ dropLast 2 xs)
  , TestCase $ assertEqual "partitionEvery discards trailing short chunk"
      [[1,2], [3,4]]
      (Prelude'.map toPreludeList $ toPreludeList $ partitionEvery 2 xs)
  , TestCase $ assertEqual "partitionStep with overlap"
      [[1,2,3], [3,4,5], [5,6,7]]
      (Prelude'.map toPreludeList $ toPreludeList $
        partitionStep 3 2 (fromPreludeList [1,2,3,4,5,6,7]))
  , TestCase $ assertEqual "windows"
      [[1,2,3], [2,3,4], [3,4,5]]
      (Prelude'.map toPreludeList $ toPreludeList $ windows 3 xs)
  , TestCase $ assertEqual "riffle"
      [1,0,2,0,3]
      (toPreludeList $ riffle 0 (fromPreludeList [1,2,3]))
  , TestCase $ assertEqual "rotateLeftN wraps by repeated rotation"
      [3,4,5,1,2]
      (toPreludeList $ rotateLeftN 7 xs)
  , TestCase $ assertEqual "rotateRightN"
      [4,5,1,2,3]
      (toPreludeList $ rotateRightN 2 xs)
  , TestCase $ assertEqual "rotate empty"
      ([] :: [Int])
      (toPreludeList $ rotateLeft (nil :: List Int))
  ]

wolframAssociationTests :: Test
wolframAssociationTests = let
    eqIntC :: Int -> Int -> Bool
    eqIntC x y = fromPreludeBool (x Prelude'.== y)
    dict :: Dict Int String
    dict = fromPreludeList [pair 1 "one", pair 2 "two", pair 3 "three"]
    dict2 :: Dict Int String
    dict2 = fromPreludeList [pair 3 "tres", pair 1 "uno"]
    dictA :: Dict Int String
    dictA = fromPreludeList [pair 2 "two", pair 1 "one"]
    dictB :: Dict Int String
    dictB = fromPreludeList [pair 3 "three", pair 1 "uno"]
    dictAB :: List (Dict Int String)
    dictAB = dictA `cons` (dictB `cons` nil)
    dict2ThenDict :: List (Dict Int String)
    dict2ThenDict = dict2 `cons` (dict `cons` nil)
    leIntC :: Int -> Int -> Bool
    leIntC x y = fromPreludeBool (x Prelude'.<= y)
    toDictList :: Dict Int String -> [(Int, String)]
    toDictList = Prelude'.map toPreludeTuple . toPreludeList
    toStringDictList :: Dict String String -> [(String, String)]
    toStringDictList = Prelude'.map toPreludeTuple . toPreludeList
    toIntDictList :: Dict Int Int -> [(Int, Int)]
    toIntDictList = Prelude'.map toPreludeTuple . toPreludeList
    toListOfDicts :: List (Dict Int String) -> [[(Int, String)]]
    toListOfDicts dicts = dicts (\d acc -> toDictList d : acc) []
  in TestList
  [ TestCase $ assertEqual "keys"
      [1,2,3]
      (toPreludeList $ keys dict)
  , TestCase $ assertEqual "values"
      ["one", "two", "three"]
      (toPreludeList $ values dict)
  , TestCase $ assertEqual "lookupDefault present"
      "two"
      (lookupDefault eqIntC "missing" 2 dict)
  , TestCase $ assertEqual "lookupDefault missing"
      "missing"
      (lookupDefault eqIntC "missing" 5 dict)
  , TestCase $ assertEqual "keyExists"
      Prelude'.True
      (toPreludeBool $ keyExists eqIntC 3 dict)
  , TestCase $ assertEqual "keyMember"
      Prelude'.True
      (toPreludeBool $ keyMember eqIntC 1 dict)
  , TestCase $ assertEqual "keyFree"
      Prelude'.True
      (toPreludeBool $ keyFree eqIntC 9 dict)
  , TestCase $ assertEqual "keyTake"
      [(1, "one"), (3, "three")]
      (toDictList $ keyTake eqIntC (fromPreludeList [3,1]) dict)
  , TestCase $ assertEqual "keyTakeOrdered"
      [(3, "three"), (1, "one")]
      (toDictList $ keyTakeOrdered eqIntC (fromPreludeList [3,1]) dict)
  , TestCase $ assertEqual "keyDrop"
      [(1, "one"), (3, "three")]
      (toDictList $ keyDrop eqIntC (fromPreludeList [2]) dict)
  , TestCase $ assertEqual "keySelect"
      [(1, "one"), (3, "three")]
      (toDictList $ keySelect (fromPreludeBool . Prelude'.odd) dict)
  , TestCase $ assertEqual "mapKeys"
      [("k1", "one"), ("k2", "two"), ("k3", "three")]
      (toStringDictList $ mapKeys (\k -> "k" Prelude'.++ show k) dict)
  , TestCase $ assertEqual "keyValueMap"
      ["1=one", "2=two", "3=three"]
      (toPreludeList $ keyValueMap (\k v -> show k Prelude'.++ "=" Prelude'.++ v) dict)
  , TestCase $ assertEqual "dictFromLists truncates to shorter input"
      [(1, "a"), (2, "b")]
      (toDictList $ dictFromLists (fromPreludeList [1,2,3]) (fromPreludeList ["a","b"]))
  , TestCase $ assertEqual "associationMap"
      [(2, "v2"), (1, "v1")]
      (toDictList $ associationMap eqIntC (\k -> "v" Prelude'.++ show k) (fromPreludeList [2,1,2]))
  , TestCase $ assertEqual "keySort"
      [(1, "one"), (2, "two"), (3, "three")]
      (toDictList $ keySort leIntC (fromPreludeList [pair 3 "three", pair 1 "one", pair 2 "two"]))
  , TestCase $ assertEqual "keySortBy"
      [(3, "three"), (2, "two"), (1, "one")]
      (toDictList $ keySortBy leIntC Prelude'.negate dict)
  , TestCase $ assertEqual "keyComplement"
      [(2, "two")]
      (toDictList $ keyComplement eqIntC dictA (dictB `cons` nil))
  , TestCase $ assertEqual "keyIntersection"
      [[(1, "one")], [(1, "uno")]]
      (toListOfDicts $ keyIntersection eqIntC dictAB)
  , TestCase $ assertEqual "keyUnion"
      [[(2, "two"), (1, "one"), (3, "missing3")]
      ,[(2, "missing2"), (1, "uno"), (3, "three")]]
      (toListOfDicts $ keyUnion eqIntC (\k -> "missing" Prelude'.++ show k) dictAB)
  , TestCase $ assertEqual "mergeWith"
      [(3, 2), (1, 2), (2, 1)]
      (toIntDictList $ mergeWith eqIntC length dict2ThenDict)
  ]

wolframGroupingTests :: Test
wolframGroupingTests = let
    eqIntC :: Int -> Int -> Bool
    eqIntC x y = fromPreludeBool (x Prelude'.== y)
    leIntC :: Int -> Int -> Bool
    leIntC x y = fromPreludeBool (x Prelude'.<= y)
    xs = fromPreludeList [1,2,1,3,2,1] :: List Int
    dictToList :: Dict Int Int -> [(Int, Int)]
    dictToList = Prelude'.map toPreludeTuple . toPreludeList
    dictOfListsToList :: Dict Int (List Int) -> [(Int, [Int])]
    dictOfListsToList dict = Prelude'.map convert (toPreludeList dict)
      where
        convert :: Int `Pair` List Int -> (Int, [Int])
        convert p = p $ \k vs -> (k, toPreludeList vs)
  in TestList
  [ TestCase $ assertEqual "gather"
      [[1,1,1], [2,2], [3]]
      (Prelude'.map toPreludeList $ toPreludeList $ gather eqIntC xs)
  , TestCase $ assertEqual "gatherBy"
      [[1,4,7], [2,5], [3,6]]
      (Prelude'.map toPreludeList $ toPreludeList $
        gatherBy eqIntC (`Prelude'.mod` 3) (fromPreludeList [1,2,3,4,5,6,7]))
  , TestCase $ assertEqual "countsBy"
      [(1,3), (2,2), (3,1)]
      (dictToList $ countsBy eqIntC xs)
  , TestCase $ assertEqual "countsByKey"
      [(1,4), (0,3)]
      (dictToList $ countsByKey eqIntC (`Prelude'.mod` 2) (fromPreludeList [1,2,3,4,5,6,7]))
  , TestCase $ assertEqual "tallyBy"
      [(1,3), (2,2), (3,1)]
      (dictToList $ tallyBy eqIntC xs)
  , TestCase $ assertEqual "countDistinctBy"
      3
      (countDistinctBy eqIntC xs)
  , TestCase $ assertEqual "positionIndexBy is one-based"
      [(1,[1,3,6]), (2,[2,5]), (3,[4])]
      (dictOfListsToList $ positionIndexBy eqIntC xs)
  , TestCase $ assertEqual "findIf"
      (Prelude'.Just 2)
      (toPreludeMaybe $ findIf (fromPreludeBool . Prelude'.even) xs)
  , TestCase $ assertEqual "findIndex is zero-based"
      (Prelude'.Just 1)
      (toPreludeMaybe $ findIndex (fromPreludeBool . Prelude'.even) xs)
  , TestCase $ assertEqual "findIndices"
      [1,4]
      (toPreludeList $ findIndices (fromPreludeBool . Prelude'.even) xs)
  , TestCase $ assertEqual "elemIndex"
      (Prelude'.Just 3)
      (toPreludeMaybe $ elemIndex eqIntC 3 xs)
  , TestCase $ assertEqual "minMaxBy"
      (1,4)
      (toPreludeTuple $ minMaxBy leIntC (fromPreludeList [3,1,4,2]))
  , TestCase $ assertEqual "deleteDuplicatesBy"
      [1,2,3]
      (toPreludeList $ deleteDuplicatesBy eqIntC xs)
  , TestCase $ assertEqual "complementBy"
      [1,3,4]
      (toPreludeList $ complementBy eqIntC (fromPreludeList [1,2,3,2,4]) (fromPreludeList [2,5]))
  , TestCase $ assertEqual "containsAllBy"
      Prelude'.True
      (toPreludeBool $ containsAllBy eqIntC (fromPreludeList [1,2,3]) (fromPreludeList [2,3]))
  , TestCase $ assertEqual "containsAnyBy"
      Prelude'.True
      (toPreludeBool $ containsAnyBy eqIntC (fromPreludeList [1,2,3]) (fromPreludeList [9,3]))
  , TestCase $ assertEqual "containsNoneBy"
      Prelude'.True
      (toPreludeBool $ containsNoneBy eqIntC (fromPreludeList [1,2,3]) (fromPreludeList [8,9]))
  , TestCase $ assertEqual "containsOnlyBy"
      Prelude'.False
      (toPreludeBool $ containsOnlyBy eqIntC (fromPreludeList [1,2,4]) (fromPreludeList [1,2,3]))
  ]

------------------------------------------------------------
-- Whole-public-API regression coverage
------------------------------------------------------------

-- These cases deliberately name every public definition that was not reached by
-- the older suite.  Besides guarding the small forwarding combinators, the tests
-- make their ordering, truncation, and tagging semantics explicit.
wholeApiCoverageTests :: Test
wholeApiCoverageTests = let
    eqIntC :: Int -> Int -> Bool
    eqIntC x y = fromPreludeBool (x Prelude'.== y)
    leIntC :: Int -> Int -> Bool
    leIntC x y = fromPreludeBool (x Prelude'.<= y)
    fl = fromPreludeList
    tl = toPreludeList
    matrixToLists :: Matrix Int -> [[Int]]
    matrixToLists = Prelude'.map tl . tl
    mkDict :: [(Int, Int)] -> Dict Int Int
    mkDict = Prelude'.foldr (\(k, v) acc -> pair k v `cons` acc) nil
    dictToList :: Dict Int Int -> [(Int, Int)]
    dictToList = toPreludePairList
    nested :: Dict Int (Dict Int String)
    nested = fl
      [ pair 1 (fl [pair 10 "a", pair 20 "b"])
      , pair 2 (fl [pair 30 "c"])
      ]
    collapsedToList :: Dict (Int `Pair` Int) String -> [((Int, Int), String)]
    collapsedToList d = d
      (\entry acc -> entry (\key value -> key (\k1 k2 -> ((k1, k2), value) : acc))) []
    eitherDictToList :: Dict (Either Int Char) String -> [(Prelude'.Either Int Char, String)]
    eitherDictToList d = d
      (\entry acc -> entry (\key value -> (key Prelude'.Left Prelude'.Right, value) : acc)) []
    cartesianDictToList
      :: Dict (Int `Pair` Char) (Int `Pair` Int)
      -> [((Int, Char), (Int, Int))]
    cartesianDictToList d = d
      (\entry acc -> entry (\key value ->
        key (\k1 k2 -> value (\v1 v2 -> ((k1, k2), (v1, v2)) : acc)))) []
    toEitherMaybe
      :: Either (Maybe Int) (Maybe Char)
      -> Prelude'.Either (Prelude'.Maybe Int) (Prelude'.Maybe Char)
    toEitherMaybe e = e (Prelude'.Left . toPreludeMaybe) (Prelude'.Right . toPreludeMaybe)
    selectors :: List Bool
    selectors = fl [true, false, true]
    commonA = fl [1,2,8,3,4]
    commonB = fl [1,2,9,3,4]
  in TestList
  [ TestCase $ assertEqual "xor truth table"
      [Prelude'.False, Prelude'.True, Prelude'.True, Prelude'.False]
      (Prelude'.map toPreludeBool [xor false false, xor false true, xor true false, xor true true])
  , TestCase $ assertEqual "curry" 7 (curry (uncurry (+)) 3 4)
  , TestCase $ assertEqual "pairToList preserves heterogeneous tags"
      [Prelude'.Left 3, Prelude'.Right 'x']
      (Prelude'.map toPreludeEither . tl $ pairToList (pair 3 'x'))
  , TestCase $ assertEqual "unsnoc non-empty"
      (Prelude'.Just ([1,2], 3))
      (unsnoc (fl [1,2,3]) Prelude'.Nothing
        (\p -> Prelude'.Just (p $ \xs x -> (tl xs, x))))
  , TestCase $ assertEqual "unsnoc empty"
      (Prelude'.Nothing :: Prelude'.Maybe ([Int], Int))
      (unsnoc (nil :: List Int) Prelude'.Nothing
        (\p -> Prelude'.Just (p $ \xs x -> (tl xs, x))))
  , TestCase $ assertEqual "scanlN"
      [0,11,33]
      (tl $ scanlN (\acc row -> acc + foldr (+) 0 row) 0
        (fl [fl [1,2], fl [10,20]]))
  , TestCase $ assertEqual "mapAccumL2"
      (10, [3,7])
      (mapAccumL2 (\s x y -> pair (s+x+y) (x+y)) 0 (fl [1,3]) (fl [2,4]) $
        \s xs -> (s, tl xs))
  , TestCase $ assertEqual "mapAccumLN truncates at the shortest input"
      (111, [111])
      (mapAccumLN
        (\s row -> let total = foldr (+) 0 row in pair (s+total) (s+total))
        0 (fl [fl [1,2], fl [10,20], fl [100]]) $
        \s xs -> (s, tl xs))
  , TestCase $ assertEqual "unzip"
      ([1,2], ['a','b'])
      (unzip (fl [pair 1 'a', pair 2 'b']) $ \xs ys -> (tl xs, tl ys))
  , TestCase $ assertEqual "unfoldr"
      [3,2,1]
      (tl $ unfoldr
        (\n -> fromPreludeBool (n Prelude'.<= 0) nothing (just (pair n (n-1)))) 3)
  , TestCase $ assertEqual "unfoldTree preorder"
      [1,2,4,5,3,6,7]
      (tl $ unfoldTree
        (\n -> pair n (fromPreludeBool (n Prelude'.< 4) (fl [2*n, 2*n+1]) nil)) 1)
  , TestCase $ assertEqual "appendEither"
      [Prelude'.Left 1, Prelude'.Left 2, Prelude'.Right 'a']
      (Prelude'.map toPreludeEither . tl $ appendEither (fl [1,2]) (fl ['a']))
  , TestCase $ assertEqual "composeFlatten"
      [6,8]
      (tl $ composeFlatten
        (fl [\x -> fl [x,x+1], \x -> fl [2*x]]) 3)
  , TestCase $ assertEqual "cartesianWith ordering"
      [11,21,12,22]
      (tl $ cartesianWith (+) (fl [1,2]) (fl [10,20]))
  , TestCase $ assertEqual "cartesian"
      [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
      (toPreludePairList $ cartesian (fl [1,2]) (fl ['a','b']))
  , TestCase $ assertEqual "cartesianN"
      [[1,10],[1,20],[2,10],[2,20]]
      (Prelude'.map tl . tl $ cartesianN (fl [fl [1,2], fl [10,20]]))
  , TestCase $ assertEqual "rotateRight" [4,1,2,3]
      (tl $ rotateRight (fl [1,2,3,4]))
  , TestCase $ assertEqual "catMaybes"
      [1,3]
      (tl $ catMaybes (fl [just 1, nothing, just 3]))
  , TestCase $ assertEqual "squashMaybe variants"
      [Prelude'.Nothing, Prelude'.Nothing, Prelude'.Just 4]
      (Prelude'.map toPreludeMaybe
        [ squashMaybe (nothing :: Maybe (Maybe Int))
        , squashMaybe (just nothing)
        , squashMaybe (just (just 4))
        ])
  , TestCase $ assertEqual "sequence succeeds"
      (Prelude'.Just [1,2,3])
      (toPreludeMaybe (sequence (fl [just 1, just 2, just 3])) Prelude'.>>= Prelude'.Just . tl)
  , TestCase $ assertEqual "sequence short-circuits on nothing"
      (Prelude'.Nothing :: Prelude'.Maybe [Int])
      (toPreludeMaybe (sequence (fl [just 1, nothing, just 3])) Prelude'.>>= Prelude'.Just . tl)
  , TestCase $ assertEqual "maybeEither all variants"
      [ Prelude'.Just (Prelude'.Left 1)
      , Prelude'.Nothing
      , Prelude'.Just (Prelude'.Right 'x')
      ]
      [ maybeEither (left (just 1) :: Either (Maybe Int) (Maybe Char))
          Prelude'.Nothing (\e -> Prelude'.Just (toPreludeEither e))
      , maybeEither (left nothing :: Either (Maybe Int) (Maybe Char))
          Prelude'.Nothing (\e -> Prelude'.Just (toPreludeEither e))
      , maybeEither (right (just 'x') :: Either (Maybe Int) (Maybe Char))
          Prelude'.Nothing (\e -> Prelude'.Just (toPreludeEither e))
      ]
  , TestCase $ assertEqual "eitherMaybe all variants"
      [ Prelude'.Left Prelude'.Nothing
      , Prelude'.Left (Prelude'.Just 1)
      , Prelude'.Right (Prelude'.Just 'x')
      ]
      (Prelude'.map toEitherMaybe
        [ eitherMaybe (nothing :: Maybe (Either Int Char))
        , eitherMaybe (just (left 1) :: Maybe (Either Int Char))
        , eitherMaybe (just (right 'x') :: Maybe (Either Int Char))
        ])
  , TestCase $ assertEqual "lefts" [1,2]
      (tl $ lefts (fl [left 1, right 'x', left 2]))
  , TestCase $ assertEqual "rights" ['x','y']
      (tl $ rights (fl [left 1, right 'x', right 'y']))
  , TestCase $ assertEqual "partitionEithers"
      ([1,2], ['x','y'])
      (partitionEithers (fl [left 1, right 'x', left 2, right 'y']) $
        \xs ys -> (tl xs, tl ys))

  , TestCase $ assertEqual "collapse"
      [((1,10),"a"),((1,20),"b"),((2,30),"c")]
      (collapsedToList $ collapse eqIntC eqIntC nested)
  , TestCase $ assertEqual "eitherDict"
      [(Prelude'.Left 1,"one"),(Prelude'.Right 'x',"ex")]
      (eitherDictToList $ eitherDict (fl [pair 1 "one"]) (fl [pair 'x' "ex"]))
  , TestCase $ assertEqual "chain drops missing intermediate keys"
      [(1,100),(2,200)]
      (dictToList $ chain eqIntC eqIntC
        (mkDict [(1,10),(2,20),(3,99)]) (mkDict [(10,100),(20,200)]))
  , TestCase $ assertEqual "chainN composes right to left"
      [(1,200)]
      (dictToList $ chainN eqIntC (mkDict [(20,200)])
        (fl [mkDict [(1,10)], mkDict [(10,20)]]))
  , TestCase $ assertEqual "mapMaybeValues filters and maps"
      [(2,21)]
      (dictToList $ mapMaybeValues
        (\v -> fromPreludeBool (Prelude'.even v) (just (v+1)) nothing)
        (mkDict [(1,11),(2,20)]))
  , TestCase $ assertEqual "cartesianDict"
      [((1,'x'),(10,7)),((2,'x'),(20,7))]
      (cartesianDictToList $ cartesianDict
        (fl [pair 1 10, pair 2 20]) (fl [pair 'x' 7]))
  , TestCase $ assertEqual "turn clockwise"
      [[4,1],[5,2],[6,3]]
      (matrixToLists $ turn (fl [fl [1,2,3], fl [4,5,6]]))
  , TestCase $ assertEqual "unturn counterclockwise"
      [[3,6],[2,5],[1,4]]
      (matrixToLists $ unturn (fl [fl [1,2,3], fl [4,5,6]]))
  , TestCase $ assertEqual "unionBy"
      [1,2,3,3]
      (tl $ unionBy eqIntC (fl [1,2]) (fl [2,3,3]))
  , TestCase $ assertEqual "intersectBy"
      [2,2]
      (tl $ intersectBy eqIntC (fl [1,2,2,3]) (fl [2,4]))
  , TestCase $ assertEqual "pick preserves source tags"
      [Prelude'.Left 1, Prelude'.Right 'b', Prelude'.Left 3]
      (Prelude'.map toPreludeEither . tl $ pick selectors (fl [1,2,3]) (fl ['a','b','c']))
  , TestCase $ assertEqual "pick'"
      [1,20,3]
      (tl $ pick' selectors (fl [1,2,3]) (fl [10,20,30]))
  , TestCase $ assertEqual "isSubstring true" Prelude'.True
      (toPreludeBool $ isSubstring eqIntC (fl [2,3]) (fl [1,2,3,4]))
  , TestCase $ assertEqual "isSubstring false" Prelude'.False
      (toPreludeBool $ isSubstring eqIntC (fl [2,4]) (fl [1,2,3,4]))
  , TestCase $ assertEqual "isSubseq true" Prelude'.True
      (toPreludeBool $ isSubseq eqIntC (fl [1,3,5]) (fl [1,2,3,4,5]))
  , TestCase $ assertEqual "isSubseq false" Prelude'.False
      (toPreludeBool $ isSubseq eqIntC (fl [3,2]) (fl [1,2,3]))
  , TestCase $ assertEqual "break"
      ([1,2],[3,4])
      (break (fromPreludeBool . (Prelude'.== 3)) (fl [1,2,3,4]) $
        \xs ys -> (tl xs, tl ys))
  , TestCase $ assertEqual "isSuffixOf true" Prelude'.True
      (toPreludeBool $ isSuffixOf eqIntC (fl [3,4]) (fl [1,2,3,4]))
  , TestCase $ assertEqual "isSuffixOf false" Prelude'.False
      (toPreludeBool $ isSuffixOf eqIntC (fl [2,3]) (fl [1,2,3,4]))
  , TestCase $ assertEqual "longestCommonPrefix" [1,2]
      (tl $ longestCommonPrefix eqIntC commonA commonB)
  , TestCase $ assertEqual "longestCommonSuffix" [3,4]
      (tl $ longestCommonSuffix eqIntC commonA commonB)
  , TestCase $ assertEqual "longestCommonSublist is prefix plus suffix" [1,2,3,4]
      (tl $ longestCommonSublist eqIntC commonA commonB)
  , TestCase $ assertEqual "ordering helper is exercised" Prelude'.True
      (toPreludeBool $ leIntC 1 2)
  ]

------------------------------------------------------------
-- Deterministic property-style differential coverage
------------------------------------------------------------

smallLists :: [[Int]]
smallLists = Prelude'.concatMap
  (\n -> Prelude'.sequence (Prelude'.replicate n [-1,0,1])) [0..4]

exhaustiveDifferentialTests :: Test
exhaustiveDifferentialTests = let
    eqIntC :: Int -> Int -> Bool
    eqIntC x y = fromPreludeBool (x Prelude'.== y)
    leIntC :: Int -> Int -> Bool
    leIntC x y = fromPreludeBool (x Prelude'.<= y)
    fl = fromPreludeList
    tl = toPreludeList
    shortLists = Prelude'.filter ((Prelude'.<= 3) . Prelude'.length) smallLists
    rotateLeftReference n xs = case xs of
      [] -> []
      _  -> let k = n `Prelude'.mod` Prelude'.length xs
            in Prelude'.drop k xs Prelude'.++ Prelude'.take k xs
    rotateRightReference n = Prelude'.reverse . rotateLeftReference n . Prelude'.reverse
  in TestList
  [ TestCase $ Prelude'.mapM_ (\xs -> do
      let cxs = fl xs
          context op = op Prelude'.++ " on " Prelude'.++ show xs
      assertEqual (context "reverse") (Prelude'.reverse xs) (tl $ reverse cxs)
      assertEqual (context "map") (Prelude'.map (+1) xs) (tl $ map (+1) cxs)
      assertEqual (context "filter") (Prelude'.filter Prelude'.even xs)
        (tl $ filter (fromPreludeBool . Prelude'.even) cxs)
      assertEqual (context "sort") (Data.List.sort xs) (tl $ quicksort leIntC cxs)
      assertEqual (context "mergesort") (Data.List.sort xs) (tl $ mergesort leIntC cxs)
      assertEqual (context "heapsort") (Data.List.sort xs) (tl $ heapsort leIntC cxs)
      assertEqual (context "introsort") (Data.List.sort xs) (tl $ introsort leIntC cxs)
      Prelude'.mapM_ (\n -> do
        assertEqual (context ("rotateLeftN " Prelude'.++ show n))
          (rotateLeftReference n xs) (tl $ rotateLeftN n cxs)
        assertEqual (context ("rotateRightN " Prelude'.++ show n))
          (rotateRightReference n xs) (tl $ rotateRightN n cxs)) [0..7]
      Prelude'.mapM_ (\n ->
        let out = tl $ nthElement' leIntC n cxs
            sorted = Data.List.sort xs
            pivot = sorted Prelude'.!! n
        in assertBool (context ("nthElement' " Prelude'.++ show n))
          (Data.List.sort out Prelude'.== sorted
           Prelude'.&& out Prelude'.!! n Prelude'.== pivot
           Prelude'.&& Prelude'.all (Prelude'.<= pivot) (Prelude'.take n out)
           Prelude'.&& Prelude'.all (Prelude'.>= pivot) (Prelude'.drop (n+1) out)))
        [0 .. Prelude'.length xs - 1]
      ) smallLists
  , TestCase $ Prelude'.mapM_ (\xs -> Prelude'.mapM_ (\ys -> do
      let cxs = fl xs
          cys = fl ys
          context op = op Prelude'.++ " on " Prelude'.++ show xs Prelude'.++ ", " Prelude'.++ show ys
      assertEqual (context "append") (xs Prelude'.++ ys) (tl $ append cxs cys)
      assertEqual (context "zip") (Prelude'.zip xs ys) (toPreludePairList $ zip cxs cys)
      assertEqual (context "isPrefixOf") (Data.List.isPrefixOf xs ys)
        (toPreludeBool $ isPrefixOf eqIntC cxs cys)
      assertEqual (context "isSuffixOf") (Data.List.isSuffixOf xs ys)
        (toPreludeBool $ isSuffixOf eqIntC cxs cys)
      assertEqual (context "isSubsequenceOf") (Data.List.isSubsequenceOf xs ys)
        (toPreludeBool $ isSubseq eqIntC cxs cys)
      assertEqual (context "isInfixOf") (Data.List.isInfixOf xs ys)
        (toPreludeBool $ isSubstring eqIntC cxs cys)
      assertEqual (context "intersectBy") (Data.List.intersectBy (Prelude'.==) xs ys)
        (tl $ intersectBy eqIntC cxs cys)
      ) shortLists) shortLists
  ]

------------------------------------------------------------
-- Partial functions and infinite-list productivity
------------------------------------------------------------

partialAndProductivityTests :: Test
partialAndProductivityTests = let
    leIntC :: Int -> Int -> Bool
    leIntC x y = fromPreludeBool (x Prelude'.<= y)
    naturalsFrom :: Int -> List Int
    naturalsFrom n = n `cons` naturalsFrom (n+1)
    assertCompletes :: (Prelude'.Eq a, Prelude'.Show a) => String -> a -> a -> Assertion
    assertCompletes label expected actual = do
      result <- Timeout.timeout 2000000 (Exception.evaluate actual)
      assertEqual label (Prelude'.Just expected) result
  in TestList
  [ TestCase $ assertThrows "head empty" (Exception.evaluate $ head (nil :: List Int))
  , TestCase $ assertEqual "tail empty" ([] :: [Int])
      (toPreludeList $ tail (nil :: List Int))
  , TestCase $ assertThrows "last empty" (Exception.evaluate $ last (nil :: List Int))
  , TestCase $ assertEqual "init empty" ([] :: [Int])
      (toPreludeList $ init (nil :: List Int))
  , TestCase $ assertThrows "foldl1 empty" (Exception.evaluate $ foldl1 (+) (nil :: List Int))
  , TestCase $ assertThrows "foldr1 empty" (Exception.evaluate $ foldr1 (+) (nil :: List Int))
  , TestCase $ assertThrows "at negative" (Exception.evaluate $ at (-1) (fromPreludeList [1,2]))
  , TestCase $ assertThrows "at past end" (Exception.evaluate $ at 2 (fromPreludeList [1,2]))
  , TestCase $ assertThrows "pick' first values too short"
      (Exception.evaluate . Prelude'.sum . toPreludeList $
        pick' (fromPreludeList [true]) (nil :: List Int) (fromPreludeList [1]))
  , TestCase $ assertThrows "pick' second values too short"
      (Exception.evaluate . Prelude'.sum . toPreludeList $
        pick' (fromPreludeList [false]) (fromPreludeList [1]) (nil :: List Int))
  , TestCase $ assertThrows "minMaxBy empty"
      (Exception.evaluate . fst $ minMaxBy leIntC (nil :: List Int))
  , TestCase $ assertCompletes "take is productive" 4950
      (Prelude'.sum . toPreludeList $ take 100 (naturalsFrom 0))
  , TestCase $ assertCompletes "findIf is productive" (Prelude'.Just 100)
      (toPreludeMaybe $ findIf (fromPreludeBool . (Prelude'.== 100)) (naturalsFrom 0))
  , TestCase $ assertCompletes "finite prefix comparison is productive" Prelude'.True
      (toPreludeBool $ isPrefixOf
        (\x y -> fromPreludeBool (x Prelude'.== y))
        (fromPreludeList [0..100]) (naturalsFrom 0))
  , TestCase $ assertCompletes "foldr can short-circuit" 0
      (foldr const (-1) (naturalsFrom 0))
  ]

remainingBranchTests :: Test
remainingBranchTests = let
    eqIntC :: Int -> Int -> Bool
    eqIntC x y = fromPreludeBool (x Prelude'.== y)
    leIntC :: Int -> Int -> Bool
    leIntC x y = fromPreludeBool (x Prelude'.<= y)
    fl = fromPreludeList
    tl = toPreludeList
    mkDict :: [(Int, Int)] -> Dict Int Int
    mkDict = Prelude'.foldr (\(k, v) acc -> pair k v `cons` acc) nil
    dictToList :: Dict Int Int -> [(Int, Int)]
    dictToList = toPreludePairList
  in TestList
  [ TestCase $ assertEqual "partitionStep rejects non-positive block size"
      ([] :: [[Int]])
      (Prelude'.map tl . tl $ partitionStep 0 1 (fl [1,2,3]))
  , TestCase $ assertEqual "partitionStep rejects non-positive step"
      ([] :: [[Int]])
      (Prelude'.map tl . tl $ partitionStep 2 0 (fl [1,2,3]))
  , TestCase $ assertEqual "transpose empty"
      ([] :: [[Int]])
      (Prelude'.map tl . tl $ transpose (nil :: Matrix Int))
  , TestCase $ assertThrows "fromJust nothing"
      (Exception.evaluate $ fromJust (nothing :: Maybe Int))
  , TestCase $ assertThrows "fromLeft right"
      (Exception.evaluate $ fromLeft (right 'x' :: Either Int Char))
  , TestCase $ assertThrows "fromRight left"
      (Exception.evaluate $ fromRight (left 1 :: Either Int Char))
  , TestCase $ assertEqual "maybeEither right nothing"
      (Prelude'.Nothing :: Prelude'.Maybe (Prelude'.Either Int Char))
      (maybeEither (right nothing :: Either (Maybe Int) (Maybe Char))
        Prelude'.Nothing (\e -> Prelude'.Just (toPreludeEither e)))
  , TestCase $ assertEqual "keyTakeOrdered omits misses"
      [(2,20),(1,10)]
      (dictToList $ keyTakeOrdered eqIntC (fl [2,9,1]) (mkDict [(1,10),(2,20)]))
  , TestCase $ assertEqual "intersectionKeys empty dictionary list"
      ([] :: [Int])
      (tl $ intersectionKeys eqIntC (nil :: List (Dict Int Int)))
  , TestCase $ assertEqual "deleteBy absent and empty"
      ([1,2], [] :: [Int])
      ( tl $ deleteBy eqIntC 9 (fl [1,2])
      , tl $ deleteBy eqIntC 9 (nil :: List Int)
      )
  , TestCase $ assertEqual "introsort exercises the recursive large-list path"
      [1..100]
      (tl $ introsort leIntC (fl [1..100]))
  , TestCase $ assertEqual "nthElement' handles empty input"
      ([] :: [Int])
      (tl $ nthElement' leIntC 0 (nil :: List Int))
  , TestCase $ assertEqual "adjacentFind no match"
      (Prelude'.Nothing :: Prelude'.Maybe Int)
      (toPreludeMaybe $ adjacentFind eqIntC (fl [1,2,3]))
  , TestCase $ assertEqual "isSortedUntil reaches the end"
      4
      (isSortedUntil leIntC (fl [1,2,3,4]))
  , TestCase $ assertEqual "set operations with empty ranges"
      ([1,2], [1,2], [], [] :: [Int])
      ( tl $ setUnion leIntC nil (fl [1,2])
      , tl $ setDifference leIntC (fl [1,2]) nil
      , tl $ setIntersection leIntC nil (fl [1,2])
      , tl $ setSymmetricDifference leIntC nil nil
      )
  , TestCase $ assertThrows "minmaxElement empty"
      (Exception.evaluate . fst $ minmaxElement leIntC (nil :: List Int))
  , TestCase $ assertThrows "atKey missing"
      (Exception.evaluate $ atKey eqIntC 9 (mkDict [(1,10)]))
  , TestCase $ assertEqual "isSubmapOfBy missing key" Prelude'.False
      (toPreludeBool $ isSubmapOfBy eqIntC eqIntC
        (mkDict [(9,90)]) (mkDict [(1,10)]))
  , TestCase $ assertEqual "adjacentDifference empty"
      ([] :: [Int])
      (tl $ adjacentDifference (-) (nil :: List Int))
  ]

typelevelParityTests :: Test
typelevelParityTests = let
    eqIntC :: Int -> Int -> Bool
    eqIntC x y = fromPreludeBool (x Prelude'.== y)
    leIntC :: Int -> Int -> Bool
    leIntC x y = fromPreludeBool (x Prelude'.<= y)
    fl = fromPreludeList
    tl = toPreludeList
    tll = Prelude'.map tl . tl
    mkDict :: [(Int, Int)] -> Dict Int Int
    mkDict = Prelude'.foldr (\(k, v) acc -> pair k v `cons` acc) nil
    dictToList = toPreludePairList
    positionToList :: Dict Int (List Int) -> [(Int, [Int])]
    positionToList d = d
      (\entry acc -> entry (\k ps -> (k, toPreludeList ps) : acc)) []
  in TestList
  [ TestCase $ assertEqual "isJust"
      (Prelude'.True, Prelude'.False)
      (toPreludeBool $ isJust (just 1), toPreludeBool $ isJust (nothing :: Maybe Int))
  , TestCase $ assertEqual "dup" (7,7) (toPreludeTuple $ dup (7 :: Int))
  , TestCase $ assertEqual "andList/orList"
      (Prelude'.False, Prelude'.True)
      ( toPreludeBool $ andList (fromPreludeList [true,false,true])
      , toPreludeBool $ orList (fromPreludeList [false,true,false])
      )
  , TestCase $ assertEqual "cycleN" [1,2,1,2,1,2]
      (tl $ cycleN 3 (fl [1,2]))
  , TestCase $ assertEqual "iterateN" [1,2,4,8]
      (tl $ iterateN 4 (* 2) (1 :: Int))
  , TestCase $ assertEqual "range" [3,4,5,6] (tl $ range 3 7)
  , TestCase $ assertEqual "factorial" 120 (factorial 5)
  , TestCase $ assertEqual "isPalindrome"
      (Prelude'.True, Prelude'.False)
      ( toPreludeBool $ isPalindrome eqIntC (fl [1,2,1])
      , toPreludeBool $ isPalindrome eqIntC (fl [1,2,3])
      )
  , TestCase $ assertEqual "isRotation"
      (Prelude'.True, Prelude'.False)
      ( toPreludeBool $ isRotation eqIntC (fl [1,2,3]) (fl [2,3,1])
      , toPreludeBool $ isRotation eqIntC (fl [1,2]) (fl [2,1,2])
      )
  , TestCase $ assertEqual "notElemBy/notElem"
      (Prelude'.True, Prelude'.False)
      ( toPreludeBool $ notElemBy eqIntC 9 (fl [1,2,3])
      , toPreludeBool $ notElem eqIntC 2 (fl [1,2,3])
      )
  , TestCase $ assertEqual "removeOnce/replaceOnce"
      ([1,3,2], [1,9,3,2])
      ( tl $ removeOnce eqIntC 2 (fl [1,2,3,2])
      , tl $ replaceOnce eqIntC 2 9 (fl [1,2,3,2])
      )
  , TestCase $ assertEqual "intercalate" [1,2,0,3,0,4,5]
      (tl $ intercalate (fl [0]) (fromPreludeList [fl [1,2], fl [3], fl [4,5]]))
  , TestCase $ assertEqual "sum/product" (10,24)
      (sum (fl [1,2,3,4]), product (fl [1,2,3,4]))
  , TestCase $ assertEqual "maximum/minimum/minMax"
      (4,1,(1,4))
      ( maximum leIntC (fl [3,1,4,2])
      , minimum leIntC (fl [3,1,4,2])
      , toPreludeTuple $ minMax leIntC (fl [3,1,4,2])
      )
  , TestCase $ assertEqual "sort/sortBy"
      ([1,2,3,4], [1,2,3,4])
      (tl $ sort leIntC (fl [3,1,4,2]), tl $ sortBy leIntC (fl [3,1,4,2]))
  , TestCase $ assertEqual "group/nub"
      ([[1,1],[2,2],[3]], [1,2,3])
      (tll $ group eqIntC (fl [1,1,2,2,3]), tl $ nub eqIntC (fl [1,2,1,3]))
  , TestCase $ assertEqual "tally/counts/countDistinct/positionIndex"
      ( [(1,2),(2,2),(3,1)]
      , [(1,2),(2,2),(3,1)]
      , 3
      , [(1,[1,3]),(2,[2,5]),(3,[4])]
      )
      ( dictToList $ tally eqIntC (fl [1,2,1,3,2])
      , dictToList $ counts eqIntC (fl [1,2,1,3,2])
      , countDistinct eqIntC (fl [1,2,1,3,2])
      , positionToList $ positionIndex eqIntC (fl [1,2,1,3,2])
      )
  , TestCase $ assertEqual "elem/delete/intersect/complement"
      (Prelude'.True, [1,3,2], [2,2], [1,3])
      ( toPreludeBool $ elem eqIntC 2 (fl [1,2,3])
      , tl $ delete eqIntC 2 (fl [1,2,3,2])
      , tl $ intersect eqIntC (fl [1,2,2,3]) (fl [2,4])
      , tl $ complement eqIntC (fl [1,2,3]) (fl [2])
      )
  , TestCase $ assertEqual "contains aliases"
      (Prelude'.True, Prelude'.True, Prelude'.True, Prelude'.True)
      ( toPreludeBool $ containsAll eqIntC (fl [1,2,3]) (fl [1,3])
      , toPreludeBool $ containsAny eqIntC (fl [1,2,3]) (fl [9,2])
      , toPreludeBool $ containsNone eqIntC (fl [1,2,3]) (fl [8,9])
      , toPreludeBool $ containsOnly eqIntC (fl [1,2,1]) (fl [1,2,3])
      )
  , TestCase $ assertEqual "unique/prefix/suffix"
      ([1,2,1], Prelude'.True, Prelude'.True)
      ( tl $ unique eqIntC (fl [1,1,2,2,1])
      , toPreludeBool $ isPrefix eqIntC (fl [1,2]) (fl [1,2,3])
      , toPreludeBool $ isSuffix eqIntC (fl [2,3]) (fl [1,2,3])
      )
  , TestCase $ assertEqual "comparison aliases"
      (-1, Prelude'.True)
      (compareThreeWay leIntC (fl [1,2]) (fl [1,3]),
       toPreludeBool $ lexLess leIntC (fl [1,2]) (fl [1,3]))
  , TestCase $ assertEqual "nextPermutationBy"
      (Prelude'.Just [1,3,2])
      (nextPermutationBy leIntC (fl [1,2,3])
        Prelude'.Nothing (Prelude'.Just . tl))
  , TestCase $ assertEqual "association aliases"
      ([(1,10),(2,20)], [(1,99),(2,20)], [(2,20)], [(1,99),(2,20)])
      ( dictToList $ associationThread (fl [1,2]) (fl [10,20])
      , dictToList $ assocInsert eqIntC 1 99 (mkDict [(1,10),(2,20)])
      , dictToList $ assocDelete eqIntC 1 (mkDict [(1,10),(2,20)])
      , dictToList $ insertOrAssign eqIntC 1 99 (mkDict [(1,10),(2,20)])
      )
  ]
