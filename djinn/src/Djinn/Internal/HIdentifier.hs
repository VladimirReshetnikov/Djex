--
-- Haskell identifier and operator syntax shared by parsers and printers,
-- plus the token-level ReadP helpers shared by every Djinn parser.
--
module Djinn.Internal.HIdentifier (
    pVarId, pConId, pQualifiedVarId, pQualifiedConId,
    pParenthesizedVarOp,
    isVarId, isConId, isQualifiedVarId, isQualifiedConId,
    isVarOperator, renderVarName,
    schar, sstring, pParen
    ) where

import Data.Char (isAlphaNum, isLower, isPunctuation, isSymbol, isUpper)
import Text.ParserCombinators.ReadP

-- Match a single token after skipping leading white space.
schar :: Char -> ReadP ()
schar c = skipSpaces >> char c >> return ()

sstring :: String -> ReadP ()
sstring s = skipSpaces >> string s >> return ()

pParen :: ReadP a -> ReadP a
pParen p = do
    schar '('
    e <- p
    schar ')'
    return e

pVarId :: ReadP String
pVarId = pValidated isVarId isIdentifierCharacter

pConId :: ReadP String
pConId = pValidated isConId isIdentifierCharacter

pQualifiedVarId :: ReadP String
pQualifiedVarId = pValidated isQualifiedVarId isQualifiedCharacter

pQualifiedConId :: ReadP String
pQualifiedConId = pValidated isQualifiedConId isQualifiedCharacter

pParenthesizedVarOp :: ReadP String
pParenthesizedVarOp = do
    skipSpaces
    _ <- char '('
    skipSpaces
    operator <- munch1 isOperatorCharacter
    skipSpaces
    _ <- char ')'
    if isVarOperator operator then return operator else pfail

pValidated :: (String -> Bool) -> (Char -> Bool) -> ReadP String
pValidated valid character = do
    skipSpaces
    token <- munch1 character
    if valid token then return token else pfail

isVarId :: String -> Bool
isVarId identifier =
    case identifier of
        first : rest ->
            (isLower first || first == '_') &&
            all isIdentifierCharacter rest &&
            identifier `notElem` reservedIdentifiers
        [] -> False

isConId :: String -> Bool
isConId (first : rest) =
    isUpper first && all isIdentifierCharacter rest
isConId [] = False

isQualifiedVarId :: String -> Bool
isQualifiedVarId identifier =
    case splitOnDots identifier of
        Just segments ->
            not (null segments) &&
            all isConId (init segments) && isVarId (last segments)
        Nothing -> False

isQualifiedConId :: String -> Bool
isQualifiedConId identifier =
    case splitOnDots identifier of
        Just segments -> not (null segments) && all isConId segments
        Nothing -> False

isVarOperator :: String -> Bool
isVarOperator operator =
    case operator of
        first : _ ->
            first /= ':' && all isOperatorCharacter operator &&
            operator `notElem` reservedOperators
        [] -> False

renderVarName :: String -> String
renderVarName name
    | isQualifiedVarId name = name
    | isVarOperator name = "(" ++ name ++ ")"
    | otherwise = name

isIdentifierCharacter :: Char -> Bool
isIdentifierCharacter character =
    isAlphaNum character || character == '_' || character == '\''

isQualifiedCharacter :: Char -> Bool
isQualifiedCharacter character =
    isIdentifierCharacter character || character == '.'

isOperatorCharacter :: Char -> Bool
isOperatorCharacter character =
    character `elem` "!#$%&*+./<=>?@\\^|-~:" ||
    ((isSymbol character || isPunctuation character) &&
        character `notElem` "(),;[]`{}_\"'")

splitOnDots :: String -> Maybe [String]
splitOnDots input = traverseNonEmpty $ split input
  where
    split value =
        case break (== '.') value of
            (segment, []) -> [segment]
            (segment, _ : rest) -> segment : split rest

    traverseNonEmpty [] = Nothing
    traverseNonEmpty segments
        | any null segments = Nothing
        | otherwise = Just segments

reservedIdentifiers :: [String]
reservedIdentifiers =
    [ "_", "as", "case", "class", "data", "default", "deriving"
    , "do", "else", "foreign", "hiding", "if", "import", "in"
    , "infix", "infixl", "infixr", "instance", "let", "module"
    , "newtype", "of", "qualified", "then", "type", "where"
    ]

reservedOperators :: [String]
reservedOperators =
    [ "..", "--", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>" ]
