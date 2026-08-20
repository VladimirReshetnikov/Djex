-- | Haskell identifier and operator syntax shared by parsers and printers,
-- plus the token-level ReadP helpers shared by every Djinn parser.
--
-- The identifier predicates and renderers are thin views over the shared
-- "Language.Haskell.Synthesis.Name" vocabulary, and 'stripLineComments' is
-- re-exported from the shared frontend lexer used by both REPLs, so Djinn
-- keeps no second definition of what a valid name is.
module Djinn.Internal.HIdentifier (
    pVarId, pConId, pQualifiedVarId, pQualifiedConId,
    pParenthesizedVarOp,
    isVarId, isConId, isQualifiedVarId, isQualifiedConId,
    isVarOperator, renderVarName, renderProofSymbolName,
    generatedName, generatedGlobalName,
    stripLineComments,
    schar, sstring, skeyword, pParen
    ) where

import Data.Char (isAlpha)
import Control.Applicative ((<|>))
import Data.Maybe (isJust)
-- Keep the historical Internal-module export, while the shared frontend owns
-- the lexer used by both REPLs.
import Language.Haskell.Djex.Text (stripLineComments)
import Language.Haskell.Synthesis.Name (
    LexicalClass(..), Name,
    isIdentifierCharacter, isOperatorCharacter,
    mkIdentifier, mkOperator,
    nameIdentifier, nameLexicalClass, nameModule, nameOperator,
    parseName, renderCanonical, renderNameError, renderPrefix)
import Text.ParserCombinators.ReadP

-- | Match a single token after skipping leading white space.
schar :: Char -> ReadP ()
schar c = skipSpaces >> char c >> return ()

-- | Match a literal string after skipping leading white space, without any
-- token-boundary check (see 'skeyword' for alphabetic keywords).
sstring :: String -> ReadP ()
sstring s = skipSpaces >> string s >> return ()

-- | Match an alphabetic keyword as a complete token.  'sstring' deliberately
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

-- | Run a parser between @(@ and @)@, each preceded by optional white space.
pParen :: ReadP a -> ReadP a
pParen p = do
    schar '('
    e <- p
    schar ')'
    return e

-- | Parse an unqualified variable identifier token (see 'isVarId') after
-- skipping white space; the token is the maximal run of identifier
-- characters, so a qualified name is not split.
pVarId :: ReadP String
pVarId = pValidated isVarId isIdentifierCharacter

-- | Parse an unqualified constructor identifier token (see 'isConId') after
-- skipping white space.
pConId :: ReadP String
pConId = pValidated isConId isIdentifierCharacter

-- | Parse a possibly qualified variable identifier token (see
-- 'isQualifiedVarId'), taking the maximal run of identifier characters and
-- dots.
pQualifiedVarId :: ReadP String
pQualifiedVarId = pValidated isQualifiedVarId isQualifiedCharacter

-- | Parse a possibly qualified constructor identifier token (see
-- 'isQualifiedConId'), taking the maximal run of identifier characters and
-- dots.
pQualifiedConId :: ReadP String
pQualifiedConId = pValidated isQualifiedConId isQualifiedCharacter

-- | Parse a variable operator written in prefix form, such as @(+)@, and
-- return the bare operator spelling without the parentheses (see
-- 'isVarOperator').
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

-- | Whether a string is exactly an unqualified, non-reserved variable
-- identifier (lower-case or underscore initial).
isVarId :: String -> Bool
isVarId = isJust . unqualifiedIdentifier VariableLike

-- | Whether a string is exactly an unqualified, non-reserved constructor
-- identifier (upper-case initial).
isConId :: String -> Bool
isConId = isJust . unqualifiedIdentifier ConstructorLike

-- | Whether a string is exactly a variable identifier, with or without a
-- module qualifier.
-- These predicates historically accept both qualified and unqualified
-- identifiers despite their names.  Requiring canonical rendering preserves
-- their exact token semantics: unlike 'parseName', they do not trim outer
-- whitespace or accept alternate contextual syntax.
isQualifiedVarId :: String -> Bool
isQualifiedVarId = isJust . qualifiedIdentifier VariableLike

-- | Whether a string is exactly a constructor identifier, with or without a
-- module qualifier; see 'isQualifiedVarId' for the token semantics.
isQualifiedConId :: String -> Bool
isQualifiedConId = isJust . qualifiedIdentifier ConstructorLike

-- | Whether a string is a bare, unqualified, non-reserved variable operator,
-- i.e. one made of operator characters and not starting with @:@.
isVarOperator :: String -> Bool
isVarOperator = isJust . variableOperator

-- | Render a value name for prefix position: variable identifiers are
-- returned as they are and variable operators are wrapped in parentheses,
-- e.g. @+@ becomes @(+)@; any other string is returned unchanged.
renderVarName :: String -> String
renderVarName source = maybe source renderPrefix
    $ qualifiedIdentifier VariableLike source <|> variableOperator source

-- | Recover Djinn's proof-symbol spelling from a structural name.
-- Unqualified ordinary operators are stored bare; qualified operators and
-- built-in syntax use the shared canonical form.
renderProofSymbolName :: Name -> String
renderProofSymbolName name = case (nameModule name, nameOperator name) of
    (Nothing, Just spelling) -> spelling
    _ -> renderCanonical name

-- | Parse a global name at Djinn's generated-code boundary while retaining
-- the historical, role-specific diagnostic prefix.
generatedName :: String -> String -> Either String Name
generatedName description source = case parseName source of
    Left nameError -> Left $ "invalid generated " ++ description ++ " " ++
        show source ++ ": " ++ renderNameError nameError
    Right name -> Right name

-- | Parse a generated global and require the lexical namespace promised by
-- its source node.  Djinn's legacy tree distinguishes constructors from free
-- values even though the shared generated expression uses one global node.
generatedGlobalName
    :: LexicalClass -> String -> String -> Either String Name
generatedGlobalName expected description source = do
    name <- generatedName description source
    if nameLexicalClass name == expected then
        Right name
     else
        Left $ "invalid generated " ++ description ++ " " ++ show source ++
            ": expected " ++ expectedDescription expected
  where
    expectedDescription VariableLike = "a variable-like name"
    expectedDescription ConstructorLike = "a constructor-like name"

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

isQualifiedCharacter :: Char -> Bool
isQualifiedCharacter character =
    isIdentifierCharacter character || character == '.'
