-- | Validated, parser-independent Haskell names.
--
-- Ordinary identifiers and operators retain an optional module qualifier.
-- Built-in constructors are structural: clients never need to recognize
-- @"[]"@, @"(:)"@, or comma counts by string comparison.  'Name' and
-- 'ModuleName' are opaque so every value satisfies the lexical invariants.
module Language.Haskell.Synthesis.Name
  ( Name
  , ModuleName
  , Boxity (..)
  , LexicalClass (..)
  , Occurrence (..)
  , SpecialName (..)
  , NameError (..)
  , maximumTupleArity
  , mkModuleName
  , mkModuleNameSegments
  , moduleNameSegments
  , renderModuleName
  , isIdentifierCharacter
  , isOperatorCharacter
  , mkIdentifier
  , mkQualifiedIdentifier
  , mkOperator
  , mkQualifiedOperator
  , specialName
  , listName
  , consName
  , functionName
  , tupleName
  , nameModule
  , nameOccurrence
  , nameLexicalClass
  , occurrenceLexicalClass
  , nameSpelling
  , nameIdentifier
  , nameOperator
  , nameSpecial
  , parseName
  , renderCanonical
  , renderPrefix
  , renderInfix
  , renderNameError
  ) where

import Control.DeepSeq (NFData (rnf))
import Data.Char
  ( isAlphaNum
  , isLower
  , isPunctuation
  , isSpace
  , isSymbol
  , isUpper
  )
import Data.List (elemIndices, intercalate)

-- | Whether a tuple constructor is lifted (ordinary) or unboxed.
data Boxity = Boxed | Unboxed
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The lexical namespace selected by an occurrence's initial character.
-- Keeping this classification in the validated value means clients never
-- need to repeat the case/colon test over a raw spelling.
data LexicalClass = VariableLike | ConstructorLike
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Built-in constructors whose identity is syntax, not an arbitrary string.
--
-- A tuple arity becomes a valid 'Name' only through 'specialName' or
-- 'tupleName'.  Boxed tuples admit arity zero and arities of at least two.
-- Unboxed tuple constructors follow the same arities: @\(# #\)@ is the
-- zero-field @Unit#@ constructor, while unary unboxed tuple /values/ use
-- @\(# value #\)@ and the distinct @MkSolo#@ constructor.
data SpecialName
  = ListConstructor
  | ConsConstructor
  | FunctionConstructor
  | TupleConstructor Boxity Int
  deriving (Eq, Ord, Show)

newtype ModuleName = ModuleName [String]
  deriving (Eq, Ord)

data NamePart
  = IdentifierPart LexicalClass String
  | OperatorPart LexicalClass String
  deriving (Eq, Ord)

data Name
  = OrdinaryName (Maybe ModuleName) NamePart
  | BuiltInName SpecialName
  deriving (Eq, Ord)

-- | The unqualified occurrence carried by a 'Name'.  Its constructors are
-- safe to inspect but do not construct a 'Name'; smart constructors remain
-- the only route into the validated abstraction.
data Occurrence
  = IdentifierOccurrence LexicalClass String
  | OperatorOccurrence LexicalClass String
  | SpecialOccurrence SpecialName
  deriving (Eq, Ord, Show)

-- | Precise construction, parsing, and contextual-rendering failures.
data NameError
  = EmptyName
  | EmptyModuleName
  | InvalidIdentifier String
  | ReservedIdentifier String
  | InvalidOperator String
  | ReservedOperator String
  | InvalidModuleSegment String
  | InvalidTupleArity Boxity Int
  | NameHasNoInfixForm SpecialName
  | InvalidNameSyntax String
  deriving (Eq, Ord)

-- | Maximum tuple constructor/type arity supported by the target GHC.
-- Keeping the bound at the validated name edge also prevents a tiny malformed
-- value from requesting an effectively unbounded comma string when rendered.
maximumTupleArity :: Int
maximumTupleArity = 64

instance Show ModuleName where
  show = renderModuleName

instance Show Name where
  show = renderCanonical

instance Show NameError where
  show = renderNameError

instance NFData Boxity where
  rnf Boxed = ()
  rnf Unboxed = ()

instance NFData LexicalClass where
  rnf VariableLike = ()
  rnf ConstructorLike = ()

instance NFData SpecialName where
  rnf ListConstructor = ()
  rnf ConsConstructor = ()
  rnf FunctionConstructor = ()
  rnf (TupleConstructor boxity arity) = rnf boxity `seq` rnf arity

instance NFData ModuleName where
  rnf (ModuleName segments) = rnf segments

instance NFData NamePart where
  rnf (IdentifierPart lexicalClass spelling) =
    rnf lexicalClass `seq` rnf spelling
  rnf (OperatorPart lexicalClass spelling) =
    rnf lexicalClass `seq` rnf spelling

instance NFData Name where
  rnf (OrdinaryName qualifier part) = rnf qualifier `seq` rnf part
  rnf (BuiltInName builtIn) = rnf builtIn

instance NFData Occurrence where
  rnf (IdentifierOccurrence lexicalClass spelling) =
    rnf lexicalClass `seq` rnf spelling
  rnf (OperatorOccurrence lexicalClass spelling) =
    rnf lexicalClass `seq` rnf spelling
  rnf (SpecialOccurrence builtIn) = rnf builtIn

instance NFData NameError where
  rnf EmptyName = ()
  rnf EmptyModuleName = ()
  rnf (InvalidIdentifier spelling) = rnf spelling
  rnf (ReservedIdentifier spelling) = rnf spelling
  rnf (InvalidOperator spelling) = rnf spelling
  rnf (ReservedOperator spelling) = rnf spelling
  rnf (InvalidModuleSegment segment) = rnf segment
  rnf (InvalidTupleArity boxity arity) = rnf boxity `seq` rnf arity
  rnf (NameHasNoInfixForm builtIn) = rnf builtIn
  rnf (InvalidNameSyntax source) = rnf source

-- | Validate a dotted Haskell module name.
mkModuleName :: String -> Either NameError ModuleName
mkModuleName "" = Left EmptyModuleName
mkModuleName source = mkModuleNameSegments (splitOnDots source)

-- | Validate already separated module-name components.
mkModuleNameSegments :: [String] -> Either NameError ModuleName
mkModuleNameSegments [] = Left EmptyModuleName
mkModuleNameSegments segments = do
  mapM_ validateSegment segments
  return (ModuleName segments)
  where
    validateSegment segment
      | isConstructorIdentifier segment = Right ()
      | otherwise = Left (InvalidModuleSegment segment)

moduleNameSegments :: ModuleName -> [String]
moduleNameSegments (ModuleName segments) = segments

renderModuleName :: ModuleName -> String
renderModuleName (ModuleName segments) = intercalate "." segments

mkIdentifier :: String -> Either NameError Name
mkIdentifier = mkOrdinary Nothing IdentifierPart validateIdentifier

mkQualifiedIdentifier :: ModuleName -> String -> Either NameError Name
mkQualifiedIdentifier qualifier =
  mkOrdinary (Just qualifier) IdentifierPart validateIdentifier

mkOperator :: String -> Either NameError Name
mkOperator = mkOrdinary Nothing OperatorPart validateOperator

mkQualifiedOperator :: ModuleName -> String -> Either NameError Name
mkQualifiedOperator qualifier =
  mkOrdinary (Just qualifier) OperatorPart validateOperator

mkOrdinary
  :: Maybe ModuleName
  -> (LexicalClass -> String -> NamePart)
  -> (String -> Either NameError LexicalClass)
  -> String
  -> Either NameError Name
mkOrdinary qualifier constructor validate spelling = do
  lexicalClass <- validate spelling
  return (OrdinaryName qualifier (constructor lexicalClass spelling))

-- | Construct a built-in name, validating tuple arity.
specialName :: SpecialName -> Either NameError Name
specialName builtIn@(TupleConstructor boxity arity)
  | validTupleArity boxity arity = Right (BuiltInName builtIn)
  | otherwise = Left (InvalidTupleArity boxity arity)
specialName builtIn = Right (BuiltInName builtIn)

listName :: Name
listName = BuiltInName ListConstructor

consName :: Name
consName = BuiltInName ConsConstructor

functionName :: Name
functionName = BuiltInName FunctionConstructor

tupleName :: Boxity -> Int -> Either NameError Name
tupleName boxity arity = specialName (TupleConstructor boxity arity)

nameModule :: Name -> Maybe ModuleName
nameModule (OrdinaryName qualifier _) = qualifier
nameModule (BuiltInName _) = Nothing

nameOccurrence :: Name -> Occurrence
nameOccurrence (OrdinaryName _ (IdentifierPart lexicalClass spelling)) =
  IdentifierOccurrence lexicalClass spelling
nameOccurrence (OrdinaryName _ (OperatorPart lexicalClass spelling)) =
  OperatorOccurrence lexicalClass spelling
nameOccurrence (BuiltInName builtIn) = SpecialOccurrence builtIn

nameLexicalClass :: Name -> LexicalClass
nameLexicalClass = occurrenceLexicalClass . nameOccurrence

occurrenceLexicalClass :: Occurrence -> LexicalClass
occurrenceLexicalClass (IdentifierOccurrence lexicalClass _) = lexicalClass
occurrenceLexicalClass (OperatorOccurrence lexicalClass _) = lexicalClass
occurrenceLexicalClass (SpecialOccurrence _) = ConstructorLike

-- | The bare, unqualified spelling of an ordinary identifier or operator.
-- Contextual delimiters such as parentheses and backticks are not part of
-- the spelling.  Built-in list, tuple, cons, and function names return
-- 'Nothing' because their identity is represented by 'SpecialName'.
nameSpelling :: Name -> Maybe String
nameSpelling (OrdinaryName _ (IdentifierPart _ spelling)) = Just spelling
nameSpelling (OrdinaryName _ (OperatorPart _ spelling)) = Just spelling
nameSpelling (BuiltInName _) = Nothing

nameIdentifier :: Name -> Maybe String
nameIdentifier (OrdinaryName _ (IdentifierPart _ spelling)) = Just spelling
nameIdentifier _ = Nothing

nameOperator :: Name -> Maybe String
nameOperator (OrdinaryName _ (OperatorPart _ spelling)) = Just spelling
nameOperator _ = Nothing

nameSpecial :: Name -> Maybe SpecialName
nameSpecial (BuiltInName builtIn) = Just builtIn
nameSpecial _ = Nothing

-- | Parse canonical, prefix, or infix name syntax.
--
-- Outer whitespace is ignored.  Parentheses denote an operator in prefix
-- position; backticks denote an identifier in infix position.  Built-in
-- list, cons, function, and tuple constructors are recognized before
-- ordinary operators, preserving their structural identity.
parseName :: String -> Either NameError Name
parseName source =
  let token = trim source
  in if null token then Left EmptyName else parseToken token

parseToken :: String -> Either NameError Name
parseToken "[]" = Right listName
parseToken ":" = Right consName
parseToken "(:)" = Right consName
parseToken "->" = Right functionName
parseToken "(->)" = Right functionName
parseToken token =
  case parseTuple token of
    Just result -> result
    Nothing
      | wrappedBy '`' '`' token -> parseBackticked token
      | wrappedBy '(' ')' token -> parseParenthesized token
      | otherwise -> parseBare token

parseTuple :: String -> Maybe (Either NameError Name)
parseTuple "()" = Just (tupleName Boxed 0)
parseTuple token
  | hasUnboxedDelimiters token =
      let middle = take (length token - 4) (drop 2 token)
      in if not (null middle) && all isSpace middle
           then Just (tupleName Unboxed 0)
           else if not (null middle) && all (== ',') middle
             then Just (tupleName Unboxed (length middle + 1))
             else Nothing
  | wrappedBy '(' ')' token =
      let middle = stripOuterCharacters token
      in if not (null middle) && all (== ',') middle
           then Just (tupleName Boxed (length middle + 1))
           else Nothing
  | otherwise = Nothing

hasUnboxedDelimiters :: String -> Bool
hasUnboxedDelimiters token =
  length token >= 4 && take 2 token == "(#" && drop (length token - 2) token == "#)"

parseBackticked :: String -> Either NameError Name
parseBackticked token =
  let middle = trim (stripOuterCharacters token)
  in case parseIdentifierToken middle of
       Just result -> result
       Nothing -> Left (InvalidNameSyntax token)

parseParenthesized :: String -> Either NameError Name
parseParenthesized token =
  let middle = trim (stripOuterCharacters token)
  in case parseOperatorToken middle of
       Just result -> result
       Nothing -> Left (InvalidNameSyntax token)

parseBare :: String -> Either NameError Name
parseBare token =
  case parseCanonicalQualifiedOperatorToken token of
    Just result -> result
    Nothing ->
      case parseIdentifierToken token of
        Just result -> result
        Nothing ->
          case parseOperatorToken token of
            Just result -> result
            Nothing -> Left (InvalidNameSyntax token)

-- Canonical qualified operator syntax puts parentheses around the occurrence
-- rather than around the whole qualified name: @Data.List.(++)@.
parseCanonicalQualifiedOperatorToken :: String -> Maybe (Either NameError Name)
parseCanonicalQualifiedOperatorToken token
  | not (endsWith ')' token) = Nothing
  | otherwise = seek [0 .. length token - 3]
  where
    seek [] = Nothing
    seek (index : rest)
      | take 2 (drop index token) == ".(" =
          let moduleSource = take index token
              withClosing = drop (index + 2) token
              spelling = take (length withClosing - 1) withClosing
          in case mkModuleName moduleSource of
               Right qualifier
                 | not (null spelling) && all isOperatorCharacter spelling ->
                     Just (mkQualifiedOperator qualifier spelling)
               _ -> seek rest
      | otherwise = seek rest

parseIdentifierToken :: String -> Maybe (Either NameError Name)
parseIdentifierToken token =
  case splitOnDots token of
    [spelling]
      | looksLikeIdentifier spelling -> Just (mkIdentifier spelling)
    segments
      | length segments >= 2
      , let spelling = last segments
      , looksLikeIdentifier spelling
      , all isIdentifierCharacter spelling -> Just $ do
          qualifier <- mkModuleNameSegments (init segments)
          mkQualifiedIdentifier qualifier spelling
    _ -> Nothing

parseOperatorToken :: String -> Maybe (Either NameError Name)
parseOperatorToken token
  | not (null token) && all isOperatorCharacter token = Just (mkOperator token)
  | otherwise = parseQualifiedOperatorToken token

parseQualifiedOperatorToken :: String -> Maybe (Either NameError Name)
parseQualifiedOperatorToken token = seek (elemIndices '.' token)
  where
    seek [] = Nothing
    seek (index : rest) =
      let moduleSource = take index token
          spelling = drop (index + 1) token
      in case mkModuleName moduleSource of
           Right qualifier
             | not (null spelling) && all isOperatorCharacter spelling ->
                 Just (mkQualifiedOperator qualifier spelling)
           _ -> seek rest

-- | Stable, parseable spelling.  Qualified symbolic names follow Haskell's
-- import/export form, such as @Data.List.(++)@; this deliberately differs
-- from their prefix-use form @(Data.List.++)@.
renderCanonical :: Name -> String
renderCanonical name =
  case name of
    OrdinaryName qualifier (IdentifierPart _ spelling) ->
      renderQualified qualifier spelling
    OrdinaryName Nothing (OperatorPart _ spelling) ->
      "(" ++ spelling ++ ")"
    OrdinaryName (Just qualifier) (OperatorPart _ spelling) ->
      renderModuleName qualifier ++ ".(" ++ spelling ++ ")"
    BuiltInName builtIn -> renderSpecialPrefix builtIn

-- | Render a name in prefix position.
renderPrefix :: Name -> String
renderPrefix name =
  case name of
    OrdinaryName qualifier (IdentifierPart _ spelling) ->
      renderQualified qualifier spelling
    OrdinaryName qualifier (OperatorPart _ spelling) ->
      "(" ++ renderQualified qualifier spelling ++ ")"
    BuiltInName builtIn -> renderSpecialPrefix builtIn

-- | Render a name in infix position.  Lists and tuples have no infix form;
-- identifiers use backticks, while symbolic constructors need no wrapper.
renderInfix :: Name -> Either NameError String
renderInfix name =
  case name of
    OrdinaryName qualifier (IdentifierPart _ spelling) ->
      Right ("`" ++ renderQualified qualifier spelling ++ "`")
    OrdinaryName qualifier (OperatorPart _ spelling) ->
      Right (renderQualified qualifier spelling)
    BuiltInName ConsConstructor -> Right ":"
    BuiltInName FunctionConstructor -> Right "->"
    BuiltInName builtIn -> Left (NameHasNoInfixForm builtIn)

renderQualified :: Maybe ModuleName -> String -> String
renderQualified Nothing spelling = spelling
renderQualified (Just qualifier) spelling =
  renderModuleName qualifier ++ "." ++ spelling

renderSpecialPrefix :: SpecialName -> String
renderSpecialPrefix ListConstructor = "[]"
renderSpecialPrefix ConsConstructor = "(:)"
renderSpecialPrefix FunctionConstructor = "(->)"
renderSpecialPrefix (TupleConstructor Boxed 0) = "()"
renderSpecialPrefix (TupleConstructor Boxed arity)
  | validTupleArity Boxed arity =
      "(" ++ replicate (arity - 1) ',' ++ ")"
renderSpecialPrefix (TupleConstructor Unboxed 0) = "(# #)"
renderSpecialPrefix (TupleConstructor Unboxed arity)
  | validTupleArity Unboxed arity =
      "(#" ++ replicate (arity - 1) ',' ++ "#)"
renderSpecialPrefix (TupleConstructor boxity arity) =
  "<invalid " ++ boxityDescription boxity
    ++ " tuple constructor arity " ++ show arity ++ ">"

renderNameError :: NameError -> String
renderNameError EmptyName = "name is empty"
renderNameError EmptyModuleName = "module name is empty"
renderNameError (InvalidIdentifier spelling) =
  "invalid Haskell identifier " ++ show spelling
renderNameError (ReservedIdentifier spelling) =
  "reserved Haskell identifier " ++ show spelling
renderNameError (InvalidOperator spelling) =
  "invalid Haskell operator " ++ show spelling
renderNameError (ReservedOperator spelling) =
  "reserved Haskell operator " ++ show spelling
renderNameError (InvalidModuleSegment segment) =
  "invalid Haskell module-name segment " ++ show segment
renderNameError (InvalidTupleArity boxity arity) =
  "invalid " ++ boxityDescription boxity ++ " tuple arity " ++ show arity ++
  "; " ++ tupleArityExpectation boxity
renderNameError (NameHasNoInfixForm builtIn) =
  "name " ++ renderSpecialPrefix builtIn ++ " has no infix form"
renderNameError (InvalidNameSyntax source) =
  "invalid Haskell name syntax " ++ show source

boxityDescription :: Boxity -> String
boxityDescription Boxed = "boxed"
boxityDescription Unboxed = "unboxed"

tupleArityExpectation :: Boxity -> String
tupleArityExpectation _ = "expected zero or 2 through "
  ++ show maximumTupleArity

validateIdentifier :: String -> Either NameError LexicalClass
validateIdentifier "" = Left EmptyName
validateIdentifier spelling
  | spelling `elem` reservedIdentifiers = Left (ReservedIdentifier spelling)
  | isVariableIdentifier spelling = Right VariableLike
  | isConstructorIdentifier spelling = Right ConstructorLike
  | otherwise = Left (InvalidIdentifier spelling)

validateOperator :: String -> Either NameError LexicalClass
validateOperator "" = Left EmptyName
validateOperator spelling
  | spelling `elem` reservedOperators = Left (ReservedOperator spelling)
  | all isOperatorCharacter spelling = Right (operatorLexicalClass spelling)
  | otherwise = Left (InvalidOperator spelling)

operatorLexicalClass :: String -> LexicalClass
operatorLexicalClass (':' : _) = ConstructorLike
operatorLexicalClass _ = VariableLike

isVariableIdentifier :: String -> Bool
isVariableIdentifier (first : rest) =
  (isLower first || first == '_') && all isIdentifierCharacter rest
isVariableIdentifier [] = False

isConstructorIdentifier :: String -> Bool
isConstructorIdentifier (first : rest) =
  isUpper first && all isIdentifierCharacter rest
isConstructorIdentifier [] = False

looksLikeIdentifier :: String -> Bool
looksLikeIdentifier (first : _) =
  isLower first || isUpper first || first == '_'
looksLikeIdentifier [] = False

-- | Whether a character may occur after the initial character of a Haskell
-- identifier or within a module-name segment.  Initial-character case and
-- reserved-word checks remain the responsibility of the name constructors.
-- This is the exact continuation predicate used by those constructors.
isIdentifierCharacter :: Char -> Bool
isIdentifierCharacter character =
  isAlphaNum character || character == '_' || character == '\''

-- | Whether a character belongs to Haskell's symbolic-operator alphabet.
-- Reserved whole operators and the leading-colon lexical class are checked
-- separately by the name constructors.  Tokenizers can use this predicate to
-- make exactly the same character-level decision as 'mkOperator' and
-- 'parseName'.
isOperatorCharacter :: Char -> Bool
isOperatorCharacter character =
  character `elem` "!#$%&*+./<=>?@\\^|-~:" ||
  ((isSymbol character || isPunctuation character) &&
    character `notElem` "(),;[]`{}_\"'")

validTupleArity :: Boxity -> Int -> Bool
validTupleArity _ arity = arity == 0
  || arity >= 2 && arity <= maximumTupleArity

wrappedBy :: Char -> Char -> String -> Bool
wrappedBy opening closing (first : rest@(_ : _)) =
  first == opening && endsWith closing rest
wrappedBy _ _ _ = False

endsWith :: Char -> String -> Bool
endsWith expected source =
  case reverse source of
    actual : _ -> actual == expected
    [] -> False

stripOuterCharacters :: String -> String
stripOuterCharacters token = take (length token - 2) (drop 1 token)

splitOnDots :: String -> [String]
splitOnDots source =
  case break (== '.') source of
    (segment, []) -> [segment]
    (segment, _ : rest) -> segment : splitOnDots rest

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd predicate = reverse . dropWhile predicate . reverse

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
