-- | Output assertions shared by the three CLI subprocess suites.
module CLIAssertions
  ( assertContains
  , countOccurrences
  ) where

import Data.List (isInfixOf, isPrefixOf)
import Test.Tasty.HUnit (Assertion, assertBool)

assertContains :: String -> String -> String -> Assertion
assertContains message needle haystack = assertBool
  (message ++ ": missing " ++ show needle)
  (needle `isInfixOf` haystack)

-- | Count non-overlapping occurrences. An empty needle occurs nowhere; the
-- suites previously carried diverging copies, one of which looped forever
-- on that input.
countOccurrences :: String -> String -> Int
countOccurrences needle
  | null needle = const 0
  | otherwise = go
 where
  go remaining
    | needle `isPrefixOf` remaining = 1 + go (drop (length needle) remaining)
    | _ : rest <- remaining = go rest
    | otherwise = 0
