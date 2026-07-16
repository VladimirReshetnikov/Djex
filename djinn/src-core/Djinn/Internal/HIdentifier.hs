--
-- Haskell identifier and operator syntax shared by parsers and printers,
-- plus the token-level ReadP helpers shared by every Djinn parser.
--
module Djinn.Internal.HIdentifier (
    pVarId, pConId, pQualifiedVarId, pQualifiedConId,
    pParenthesizedVarOp,
    isVarId, isConId, isQualifiedVarId, isQualifiedConId,
    isVarOperator, renderVarName, renderProofSymbolName,
    stripLineComments,
    schar, sstring, skeyword, pParen
    ) where

import Data.Char (isAlpha)
import Data.Maybe (isJust)
import Language.Haskell.Synthesis.Name (
    LexicalClass(..), Name,
    isIdentifierCharacter, isOperatorCharacter,
    mkIdentifier, mkOperator,
    nameIdentifier, nameLexicalClass, nameModule, nameOperator,
    parseName, renderCanonical, renderPrefix)
import Text.ParserCombinators.ReadP

-- Match a single token after skipping leading white space.
schar :: Char -> ReadP ()
schar c = skipSpaces >> char c >> return ()

sstring :: String -> ReadP ()
sstring s = skipSpaces >> string s >> return ()

-- Match an alphabetic keyword as a complete token.  'sstring' deliberately
-- remains boundary-free for punctuation such as @::@ and @->@; declaration
-- parsers use this stricter helper so @typeFoo@ cannot mean @type Foo@.
skeyword :: String -> ReadP ()
skeyword keyword
    | null keyword || not (all isAlpha keyword) = pfail
    | otherwise = do
        skipSpaces
        _ <- string keyword
        remaining <- look
        case remaining of
            next : _ | isIdentifierCharacter next -> pfail
            _ -> return ()

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
isVarId = isJust . unqualifiedIdentifier VariableLike

isConId :: String -> Bool
isConId = isJust . unqualifiedIdentifier ConstructorLike

-- These predicates historically accept both qualified and unqualified
-- identifiers despite their names.  Requiring canonical rendering preserves
-- their exact token semantics: unlike 'parseName', they do not trim outer
-- whitespace or accept alternate contextual syntax.
isQualifiedVarId :: String -> Bool
isQualifiedVarId = isJust . qualifiedIdentifier VariableLike

isQualifiedConId :: String -> Bool
isQualifiedConId = isJust . qualifiedIdentifier ConstructorLike

isVarOperator :: String -> Bool
isVarOperator = isJust . variableOperator

renderVarName :: String -> String
renderVarName source =
    case qualifiedIdentifier VariableLike source of
        Just name -> renderPrefix name
        Nothing -> case variableOperator source of
            Just name -> renderPrefix name
            Nothing -> source

-- | Recover Djinn's proof-symbol spelling from a structural name.
-- Unqualified ordinary operators are stored bare; qualified operators and
-- built-in syntax use the shared canonical form.
renderProofSymbolName :: Name -> String
renderProofSymbolName name = case (nameModule name, nameOperator name) of
    (Nothing, Just spelling) -> spelling
    _ -> renderCanonical name

unqualifiedIdentifier :: LexicalClass -> String -> Maybe Name
unqualifiedIdentifier expected source =
    case mkIdentifier source of
        Right name
            | nameLexicalClass name == expected -> Just name
        _ -> Nothing

qualifiedIdentifier :: LexicalClass -> String -> Maybe Name
qualifiedIdentifier expected source =
    case parseName source of
        Right name
            | isJust (nameIdentifier name)
            , nameLexicalClass name == expected
            , renderCanonical name == source -> Just name
        _ -> Nothing

variableOperator :: String -> Maybe Name
variableOperator source =
    case mkOperator source of
        Right name
            | nameLexicalClass name == VariableLike -> Just name
        _ -> Nothing

-- Strip Haskell-style line comments without mistaking a longer symbolic
-- operator such as @--*@ for a comment introducer.  Djinn has no string or
-- character literals, so operator continuation is the only lexical ambiguity.
stripLineComments :: String -> String
stripLineComments [] = []
stripLineComments ('-' : '-' : rest)
    | continuesOperator rest = '-' : '-' : stripLineComments rest
    | otherwise = stripLineComments $ dropWhile (/= '\n') rest
  where
    continuesOperator (character : _) = isOperatorCharacter character
    continuesOperator [] = False
stripLineComments (character : rest) =
    character : stripLineComments rest

isQualifiedCharacter :: Char -> Bool
isQualifiedCharacter character =
    isIdentifierCharacter character || character == '.'
