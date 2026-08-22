-- | A small, total Haskell presentation lexer for the interactive frontend.
--
-- The REPL deliberately does not feed its displayed source back through a
-- parser: generated fragments may be truncated, contain extensions unknown to
-- a parser, or be interrupted midway through a token.  This lexer therefore
-- classifies flat, byte-preserving spans and leaves all semantic state alone.
module Language.Haskell.Djex.REPL.SyntaxHighlight
  ( SyntaxClass (..)
  , SyntaxSpan (..)
  , highlightHaskell
  , tokenizeHaskell
  ) where

import Data.Char
  ( GeneralCategory (..)
  , generalCategory
  , isAlpha
  , isAlphaNum
  , isDigit
  , isSpace
  , isUpper
  )

-- | The intentionally small palette shared by every Haskell output surface.
data SyntaxClass
  = SyntaxPlain
  | SyntaxKeyword
  | SyntaxType
  | SyntaxLiteral
  | SyntaxNumber
  | SyntaxComment
  | SyntaxOperator
  deriving (Eq, Show)

-- | One source-preserving lexical span.
data SyntaxSpan = SyntaxSpan SyntaxClass String
  deriving (Eq, Show)

-- | Decorate Haskell source with portable SGR sequences when enabled.
--
-- Disabling highlighting is an exact identity operation.  Enabling it only
-- inserts SGR bytes: removing those bytes reproduces the input exactly.
highlightHaskell :: Bool -> String -> String
highlightHaskell False = id
highlightHaskell True = concatMap renderSpan . tokenizeHaskell

renderSpan :: SyntaxSpan -> String
renderSpan (SyntaxSpan SyntaxPlain source) = source
renderSpan (SyntaxSpan syntaxClass source) =
  "\ESC[" ++ syntaxCode syntaxClass ++ "m" ++ source ++ "\ESC[0m"

syntaxCode :: SyntaxClass -> String
syntaxCode syntaxClass = case syntaxClass of
  SyntaxPlain -> "0"
  SyntaxKeyword -> "1;35"
  SyntaxType -> "36"
  SyntaxLiteral -> "33"
  SyntaxNumber -> "33"
  SyntaxComment -> "2"
  SyntaxOperator -> "34"

-- | Tokenize complete or partial source without rejecting malformed input.
tokenizeHaskell :: String -> [SyntaxSpan]
tokenizeHaskell = mergeAdjacent . scan
 where
  scan [] = []
  scan source@(character : rest)
    | isSpace character = spanToken SyntaxPlain isSpace source
    | Just (comment, remaining) <- lineComment source =
        SyntaxSpan SyntaxComment comment : scan remaining
    | Just (comment, remaining) <- blockComment source =
        SyntaxSpan SyntaxComment comment : scan remaining
    | character == '"' = literalToken '"' source
    | character == '\''
    , Just (literal, remaining) <- characterLiteral source =
        SyntaxSpan SyntaxLiteral literal : scan remaining
    | isDigit character = spanToken SyntaxNumber numberCharacter source
    | identifierStart character =
        let (identifier, remaining) = span identifierCharacter source
        in SyntaxSpan (identifierClass identifier) identifier : scan remaining
    | symbolicCharacter character =
        spanToken SyntaxOperator symbolicCharacter source
    | otherwise = SyntaxSpan SyntaxPlain [character] : scan rest

  literalToken quote source =
    let (literal, remaining) = quotedLiteral quote source
    in SyntaxSpan SyntaxLiteral literal : scan remaining

  spanToken syntaxClass predicate source =
    let (token, remaining) = span predicate source
    in SyntaxSpan syntaxClass token : scan remaining

-- Keep reset traffic bounded when a malformed token makes the scanner fall
-- back one character at a time.
mergeAdjacent :: [SyntaxSpan] -> [SyntaxSpan]
mergeAdjacent = foldr merge []
 where
  merge (SyntaxSpan leftClass left) (SyntaxSpan rightClass right : remaining)
    | leftClass == rightClass =
        SyntaxSpan leftClass (left ++ right) : remaining
  merge span' remaining = span' : remaining

lineComment :: String -> Maybe (String, String)
lineComment ('-' : '-' : rest)
  | not (continuesOperator rest) =
      let (body, remaining) = break (== '\n') rest
      in Just ("--" ++ body, remaining)
lineComment _ = Nothing

continuesOperator :: String -> Bool
continuesOperator (character : _) = symbolicCharacter character
continuesOperator [] = False

blockComment :: String -> Maybe (String, String)
blockComment ('{' : '-' : rest) =
  let (body, remaining) = nested 1 rest
 in Just ("{-" ++ body, remaining)
 where
  nested :: Int -> String -> (String, String)
  nested _ [] = ([], [])
  nested depth ('{' : '-' : remaining) =
    let (body, suffix) = nested (depth + 1) remaining
    in ("{-" ++ body, suffix)
  nested 1 ('-' : '}' : remaining) = ("-}", remaining)
  nested depth ('-' : '}' : remaining) =
    let (body, suffix) = nested (depth - 1) remaining
    in ("-}" ++ body, suffix)
  nested depth (character : remaining) =
    let (body, suffix) = nested depth remaining
    in (character : body, suffix)
blockComment _ = Nothing

quotedLiteral :: Char -> String -> (String, String)
quotedLiteral quote (opening : rest) =
  let (body, remaining) = inside rest
  in (opening : body, remaining)
 where
  inside [] = ([], [])
  inside ['\\'] = ("\\", [])
  inside ('\\' : escaped : remaining) =
    let (body, suffix) = inside remaining
    in ('\\' : escaped : body, suffix)
  inside (character : remaining)
    | character == quote = ([character], remaining)
    | character == '\n' = ([character], remaining)
    | otherwise =
        let (body, suffix) = inside remaining
        in (character : body, suffix)
quotedLiteral _ [] = ([], [])

-- A leading apostrophe can introduce Template Haskell/promoted syntax or be
-- part of an identifier.  Treat it as a character literal only when a compact
-- closing quote is actually present before whitespace or a line boundary.
characterLiteral :: String -> Maybe (String, String)
characterLiteral source@('\'' : rest) = case closingQuote rest of
  Nothing -> Nothing
  Just _ -> Just $ quotedLiteral '\'' source
 where
  closingQuote [] = Nothing
  closingQuote ['\\'] = Nothing
  closingQuote ('\\' : _ : remaining) = closingQuote remaining
  closingQuote ('\'' : _) = Just ()
  closingQuote (character : remaining)
    | isSpace character = Nothing
    | otherwise = closingQuote remaining
characterLiteral _ = Nothing

identifierStart :: Char -> Bool
identifierStart character = isAlpha character || character == '_'

identifierCharacter :: Char -> Bool
identifierCharacter character =
  isAlphaNum character || character `elem` "_'#"

numberCharacter :: Char -> Bool
numberCharacter character =
  isAlphaNum character || character `elem` "._'#"

identifierClass :: String -> SyntaxClass
identifierClass identifier
  | identifier `elem` haskellKeywords = SyntaxKeyword
  | startsWithUpper identifier = SyntaxType
  | otherwise = SyntaxPlain

startsWithUpper :: String -> Bool
startsWithUpper (character : _) = isUpper character
startsWithUpper [] = False

haskellKeywords :: [String]
haskellKeywords =
  [ "as", "by", "case", "class", "data", "default", "deriving", "do"
  , "else", "export", "family", "forall", "foreign", "group", "hiding"
  , "if", "import", "in", "infix", "infixl", "infixr", "instance"
  , "interruptible", "label", "let", "mdo", "module", "newtype", "of"
  , "pattern", "proc", "qualified", "rec", "role", "safe", "static"
  , "then", "type", "unsafe", "using", "where", "stock", "anyclass"
  , "via"
  ]

symbolicCharacter :: Char -> Bool
symbolicCharacter character =
  character `elem` "!#$%&*+./<=>?@\\^|-~:;,()[]{}" ||
  generalCategory character `elem`
    [ MathSymbol
    , CurrencySymbol
    , ModifierSymbol
    , OtherSymbol
    , ConnectorPunctuation
    , DashPunctuation
    , OtherPunctuation
    ]
