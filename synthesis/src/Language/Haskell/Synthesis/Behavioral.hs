-- | The common outer syntax for executable behavioral examples.
--
-- This module only separates a named signature from its host-language
-- predicate. Haskell and Lean remain responsible for parsing and checking
-- their respective types and expressions. In particular, this syntax carries
-- no proof or candidate-selection authority.
module Language.Haskell.Synthesis.Behavioral
  ( BehavioralLanguage (..)
  , BehavioralQuery (..)
  , parseBehavioralQuery
  ) where

import Data.Char (isAlphaNum, isLetter, isLower, isSpace, isSymbol)
import Data.List (isPrefixOf)

data BehavioralLanguage = HaskellBehavioral | LeanBehavioral
  deriving (Eq, Ord, Show)

data BehavioralQuery = BehavioralQuery
  { behavioralName :: String
  , behavioralType :: String
  , behavioralPredicate :: String
  } deriving (Eq, Ord, Show)

-- | Recognize @f :: TYPE where BOOL@ or @f : TYPE where PROP@.
-- Ordinary unnamed queries return 'Nothing', including the established
-- leading @--where@ Length syntax. Once a named signature is recognized,
-- malformed or missing parts are errors rather than unconstrained queries.
-- Strings, character literals, nested comments and balanced delimiters in
-- the type cannot accidentally terminate it at an embedded @where@.
parseBehavioralQuery
  :: BehavioralLanguage -> String -> Either String (Maybe BehavioralQuery)
parseBehavioralQuery language source =
  let input = dropWhile isSpace source
      (name, suffix) = span identifierChar input
      signature = dropWhile isSpace suffix
      separator = case language of
        HaskellBehavioral -> "::"
        LeanBehavioral -> ":"
  in if not (validName language name) || not (separator `isPrefixOf` signature)
      || (language == LeanBehavioral && "::" `isPrefixOf` signature)
    then Right Nothing
    else do
      (target, predicate) <- splitWhere language
        $ drop (length separator) signature
      if null (trim target)
        then Left "a behavioral query needs a type before 'where'"
        else pure ()
      predicateContent <- dropTrivia language predicate
      if null predicateContent
        then Left "a behavioral query needs a predicate after 'where'"
        else pure ()
      pure $ Just BehavioralQuery
        { behavioralName = name
        , behavioralType = trim target
        , behavioralPredicate = trim predicate
        }

identifierChar :: Char -> Bool
identifierChar c = isAlphaNum c || c == '_' || c == '\''

validName :: BehavioralLanguage -> String -> Bool
validName _ [] = False
validName language name@(first : _) =
  name /= "_" && name `notElem` reserved
    && case language of
      HaskellBehavioral -> isLower first || first == '_'
      LeanBehavioral -> isLetter first || first == '_'
 where
  reserved = ["where", "let", "in", "if", "then", "else", "case", "of",
    "do", "forall", "fun", "match", "with", "by", "def", "theorem"]

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- The accumulator retains the exact spelling of the host type.
splitWhere :: BehavioralLanguage -> String -> Either String (String, String)
splitWhere language = walk [] []
 where
  walk _ _ [] = Left "a named behavioral query needs 'where' followed by a predicate"
  walk stack acc input
    | lineComment language input =
        let (comment, rest) = break (== '\n') input
        in walk stack (reverse comment ++ acc) rest
    | blockOpen language `isPrefixOf` input = do
        (comment, rest) <- blockComment language input
        walk stack (reverse comment ++ acc) rest
  walk stack acc input@('"' : _) = do
    (literal, rest) <- stringLiteral input
    walk stack (reverse literal ++ acc) rest
  walk stack acc input@('\'' : _)
    | Just (literal, rest) <- characterLiteral input =
        walk stack (reverse literal ++ acc) rest
  walk stack acc input@(c : rest)
    | isLetter c || c == '_' =
        let (token, suffix) = span identifierChar input
        in if null stack && token == "where" && not (qualified acc)
          then Right (reverse acc, suffix)
          else walk stack (reverse token ++ acc) suffix
    | Just closing <- lookup c [('(', ')'), ('[', ']'), ('{', '}')] =
        walk (closing : stack) (c : acc) rest
    | c `elem` ")]}" = case stack of
        expected : remaining | c == expected -> walk remaining (c : acc) rest
        _ -> Left "unbalanced delimiters in the behavioral query type"
    | otherwise = walk stack (c : acc) rest

  qualified (c : _) = c == '.' || c == '\''
  qualified [] = False

lineComment :: BehavioralLanguage -> String -> Bool
lineComment LeanBehavioral source = "--" `isPrefixOf` source
lineComment HaskellBehavioral ('-' : '-' : rest) = case rest of
  [] -> True
  c : _ -> not (isSymbol c || c `elem` ":!#$%&*+./<=>?@\\^|-~")
lineComment HaskellBehavioral _ = False

blockOpen, blockClose :: BehavioralLanguage -> String
blockOpen HaskellBehavioral = "{-"
blockOpen LeanBehavioral = "/-"
blockClose HaskellBehavioral = "-}"
blockClose LeanBehavioral = "-/"

blockComment :: BehavioralLanguage -> String -> Either String (String, String)
blockComment language source = go (1 :: Int) (reverse opening)
  $ drop (length opening) source
 where
  opening = blockOpen language
  closing = blockClose language
  go _ _ [] = Left "unterminated comment in the behavioral query"
  go depth acc input
    | opening `isPrefixOf` input =
        go (depth + 1) (reverse opening ++ acc) $ drop (length opening) input
    | closing `isPrefixOf` input =
        let acc' = reverse closing ++ acc
            rest = drop (length closing) input
        in if depth == 1 then Right (reverse acc', rest)
          else go (depth - 1) acc' rest
  go depth acc (c : rest) = go depth (c : acc) rest

stringLiteral :: String -> Either String (String, String)
stringLiteral ('"' : source) = go ['"'] source
 where
  go _ [] = Left "unterminated string in the behavioral query type"
  go acc ('\\' : c : rest) = go (c : '\\' : acc) rest
  go acc ('"' : rest) = Right (reverse ('"' : acc), rest)
  go acc (c : rest) = go (c : acc) rest
stringLiteral _ = Left "expected a string literal"

-- A promotion tick and an identifier suffix are not character literals.
characterLiteral :: String -> Maybe (String, String)
characterLiteral ('\'' : '\\' : '\'' : '\'' : rest) = Just ("'\\''", rest)
characterLiteral ('\'' : '\\' : source) =
  let (escaped, rest) = break (== '\'') source
  in case rest of
    '\'' : remaining | not (null escaped) && '\n' `notElem` escaped ->
      Just ("'\\" ++ escaped ++ "'", remaining)
    _ -> Nothing
characterLiteral ('\'' : c : '\'' : rest)
  | c /= '\n' = Just (['\'', c, '\''], rest)
characterLiteral _ = Nothing

dropTrivia :: BehavioralLanguage -> String -> Either String String
dropTrivia language source
  | null input = Right []
  | lineComment language input = dropTrivia language $ dropWhile (/= '\n') input
  | blockOpen language `isPrefixOf` input =
      blockComment language input >>= dropTrivia language . snd
  | otherwise = Right input
 where
  input = dropWhile isSpace source
