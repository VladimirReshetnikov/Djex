-- | Tiny text helpers shared by the Djex command and REPL modules.
--
-- Frontend parsing repeatedly trims user-entered fragments and compares
-- keywords case-insensitively. Keeping the two canonical spellings here stops
-- each module from re-deriving its own copy.
module Language.Haskell.Djex.Text
  ( trim
  , normalize
  ) where

import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd)

-- | Remove leading and trailing whitespace.
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Trim and lowercase a case-insensitive keyword token.
normalize :: String -> String
normalize = map toLower . trim
