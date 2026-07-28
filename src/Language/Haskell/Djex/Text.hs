-- | Tiny text helpers shared by the Djex command and REPL modules.
--
-- Frontend parsing repeatedly trims user-entered fragments and compares
-- keywords case-insensitively. Keeping the two canonical spellings here stops
-- each module from re-deriving its own copy.
module Language.Haskell.Djex.Text
  ( trim
  , normalize
  , stripLineComments
  ) where

import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd)
import Language.Haskell.Synthesis.Name (isOperatorCharacter)

-- | Remove leading and trailing whitespace.
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Trim and lowercase a case-insensitive keyword token.
normalize :: String -> String
normalize = map toLower . trim

-- | Strip Haskell line comments without mistaking a longer symbolic operator
-- such as @--*@ for a comment introducer. Quoted text stays intact as well:
-- the REPL applies this small lexer before classifying imports and synthesis
-- queries, where package names and promoted literals may contain @--@.
stripLineComments :: String -> String
stripLineComments = outsideLiteral
 where
  outsideLiteral [] = []
  outsideLiteral ('"' : rest) = '"' : insideLiteral '"' rest
  outsideLiteral ('\'' : rest)
    | beginsCharacterLiteral rest = '\'' : insideLiteral '\'' rest
    | otherwise = '\'' : outsideLiteral rest
  outsideLiteral ('-' : '-' : rest)
    | continuesOperator rest = '-' : '-' : outsideLiteral rest
    | otherwise = outsideLiteral $ dropWhile (/= '\n') rest
  outsideLiteral (character : rest) =
    character : outsideLiteral rest

  -- Copy escapes as one lexical unit, so an escaped quote cannot terminate
  -- its literal. An unescaped newline recovers to ordinary lexing; a valid
  -- multiline string gap reaches this branch through its preceding escape.
  insideLiteral _ [] = []
  insideLiteral quote ('\\' : escaped : rest) =
    '\\' : escaped : insideLiteral quote rest
  insideLiteral _ ['\\'] = "\\"
  insideLiteral _ ('\n' : rest) = '\n' : outsideLiteral rest
  insideLiteral quote (character : rest)
    | character == quote = character : outsideLiteral rest
    | otherwise = character : insideLiteral quote rest

  -- Apostrophes are also legal at the end of Haskell identifiers and before
  -- promoted constructors. Enter character-literal mode only for the
  -- ordinary one-character form or for a compact escape ending in a quote.
  beginsCharacterLiteral (character : '\'' : _) = character /= '\n'
    && character /= '\r' && character /= '\\'
  beginsCharacterLiteral ('\\' : rest) =
    '\'' `elem` takeWhile (not . isSpace) rest
  beginsCharacterLiteral _ = False

  continuesOperator (character : _) = isOperatorCharacter character
  continuesOperator [] = False
