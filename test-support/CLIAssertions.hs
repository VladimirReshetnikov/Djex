-- | Output assertions shared by the three CLI subprocess suites.
module CLIAssertions
  ( assertContains
  , assertContainsPath
  , countOccurrences
  , countOccurrencesPath
  , stripCarriageReturns
  ) where

import Data.List (isInfixOf, isPrefixOf)
import Test.Tasty.HUnit (Assertion, assertBool)

assertContains :: String -> String -> String -> Assertion
assertContains message needle haystack = assertBool
  (message ++ ": missing " ++ show needle)
  (needle `isInfixOf` haystack)

-- | 'assertContains' for path-bearing expectations. Fixtures join with
-- forward slashes, Windows canonicalization answers with backslashes, and
-- 'show'-rendered output doubles them, so both sides normalize every
-- backslash run to one forward slash before matching.
assertContainsPath :: String -> String -> String -> Assertion
assertContainsPath message needle haystack = assertBool
  (message ++ ": missing " ++ show needle)
  (normalizePathText needle `isInfixOf` normalizePathText haystack)

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

-- | 'countOccurrences' under the same normalization as 'assertContainsPath'.
countOccurrencesPath :: String -> String -> Int
countOccurrencesPath needle =
  countOccurrences (normalizePathText needle) . normalizePathText

-- | Remove the carriage returns Windows text-mode pipes add, so needles
-- containing line breaks match on every platform.
stripCarriageReturns :: String -> String
stripCarriageReturns = filter (/= '\r')

normalizePathText :: String -> String
normalizePathText = squashSlashes . map forwardSlash . stripCarriageReturns
 where
  forwardSlash '\\' = '/'
  forwardSlash character = character
  squashSlashes ('/' : '/' : rest) = squashSlashes ('/' : rest)
  squashSlashes (character : rest) = character : squashSlashes rest
  squashSlashes [] = []
