{-# LANGUAGE BangPatterns #-}

-- | Bounded surface syntax for one finite-spine Length postcondition.
--
-- The parser is deliberately solver-neutral.  It accepts one small ASCII
-- arithmetic relation, retains physical target-argument references, and
-- grants no authority to infer a modeled spine, result shape, argument role,
-- provider law, solver policy, or execution permission.  A caller must later
-- choose the scalar or binary-product domain and supply the complete
-- source-ordered role vector.  Elaboration then maps only explicitly observed
-- physical arguments to the compact input indices used by checked Length
-- contracts.
module Language.Haskell.Synthesis.Semantic.Length.Where
  ( LengthWhereDomain (..)
  , LengthWhereSource
  , LengthWhereContractSource (..)
  , lengthWhereMaximumSourceBytes
  , lengthWhereMaximumNestingDepth
  , LengthWhereExpected (..)
  , LengthWhereSyntaxLimit (..)
  , LengthWhereParseError (..)
  , LengthWhereElaborationError (..)
  , parseLengthWhereSource
  , parseHaskellLengthWhereSource
  , elaborateLengthWhereSource
  ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.List as List
import Data.Word (Word8)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Count
  ( observedNaturalBits
  )
import Language.Haskell.Synthesis.Collection (observedListLength)
import qualified Language.Haskell.Synthesis.Internal.Semantic.Length
  as Internal
import Language.Haskell.Synthesis.Semantic.Length
  ( LengthContractSource (..)
  , LengthContractVariable (..)
  , LengthExpression (..)
  , LengthFormula (..)
  , LengthLimits
  , LengthSpinePairComponent (..)
  , LengthSpinePairContractSource (..)
  , LengthSpinePairContractVariable (..)
  , LengthTargetArgumentRole (..)
  , lengthCollectionWidthLimit
  , lengthContractInputLimit
  , lengthFormulaClauseLimit
  , lengthLiteralBitLimit
  , lengthSyntaxNodeLimit
  )

-- | Explicit result domain selected by the caller's semantic profile.
data LengthWhereDomain
  = LengthWhereScalar
  | LengthWhereBinaryProduct
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Successfully parsed but not yet role-associated surface source.
--
-- The constructor and intermediate AST are intentionally opaque.  The exact
-- limits used at admission travel with the source so elaboration cannot be
-- paired with a more permissive role boundary.
data LengthWhereSource = LengthWhereSource
  !LengthLimits
  !(LengthFormula LengthWhereReference)
  [LengthWhereReference]

-- | Existing passive contract vocabulary produced by elaboration.
--
-- The exact supplied role vector is returned beside the lowered source.  This
-- is passive data, not a checked contract or behavioral receipt.
data LengthWhereContractSource
  = LengthWhereScalarContractSource
      [LengthTargetArgumentRole]
      LengthContractSource
  | LengthWhereBinaryProductContractSource
      [LengthTargetArgumentRole]
      LengthSpinePairContractSource
  deriving (Eq, Show)

-- | Hard byte admission limit for one inline constraint.
lengthWhereMaximumSourceBytes :: Natural
lengthWhereMaximumSourceBytes = 16384

-- | Hard parenthesis nesting limit for the surface grammar.
lengthWhereMaximumNestingDepth :: Natural
lengthWhereMaximumNestingDepth = 64

-- | Sanitized parser expectation.  No constructor retains source bytes.
data LengthWhereExpected
  = LengthWhereExpressionExpected
  | LengthWhereRelationExpected
  | LengthWhereLeftParenthesisExpected
  | LengthWhereRightParenthesisExpected
  | LengthWhereCommaExpected
  | LengthWhereReferenceExpected
  | LengthWhereEndExpected
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Which bounded syntactic resource reached its first excess.
data LengthWhereSyntaxLimit
  = LengthWhereNestingDepth
  | LengthWhereSyntaxNodes
  | LengthWhereFormulaClauses
  | LengthWhereCollectionWidth
  | LengthWhereLiteralBits
  | LengthWherePhysicalArgumentIndex
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Closed, sanitized parsing failures with zero-based byte offsets.
data LengthWhereParseError
  = LengthWhereSourceByteLimitExceeded !Natural !Natural
  | LengthWhereNonAsciiByte !Natural
  | LengthWhereUnexpectedEnd !Natural !LengthWhereExpected
  | LengthWhereUnexpectedToken !Natural !LengthWhereExpected
  | LengthWhereSyntaxLimitExceeded
      !LengthWhereSyntaxLimit !Natural !Natural !Natural
  | LengthWherePhysicalArgumentUnavailable !Natural
  | LengthWhereChainedComparison !Natural
  | LengthWhereNonlinearProduct !Natural
  | LengthWhereNonliteralDivisor !Natural
  | LengthWhereZeroDivisor !Natural
  deriving (Eq, Ord, Show)

-- | Failures which require the caller-supplied domain and physical roles.
data LengthWhereElaborationError
  = LengthWhereRoleVectorLimitExceeded !Natural !Natural
  | LengthWherePhysicalArgumentOutOfRange !Natural !Natural
  | LengthWherePhysicalArgumentNotObserved !Natural
  | LengthWhereScalarDomainPairResult !LengthSpinePairComponent
  | LengthWhereBinaryProductDomainScalarResult
  deriving (Eq, Ord, Show)

-- Internal physical namespace retained until role-aware elaboration.
data LengthWhereReference
  = LengthWherePhysicalArgument !Natural
  | LengthWhereScalarResult
  | LengthWherePairResult !LengthSpinePairComponent
  deriving (Eq, Ord)

data LengthWhereSurfaceSyntax
  = LengthWhereCompactSyntax
  | LengthWhereHaskellSyntax
  deriving (Eq)

data Token = Token !Natural !TokenKind

data TokenKind
  = TokenNatural !ByteString
  | TokenArgument !ByteString
  | TokenLen
  | TokenResult
  | TokenFirst
  | TokenSecond
  | TokenMinimum
  | TokenMaximum
  | TokenLeftParenthesis
  | TokenRightParenthesis
  | TokenComma
  | TokenDot
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDivide
  | TokenModulo
  | TokenDivideFunction
  | TokenModuloFunction
  | TokenEqual
  | TokenNotEqual
  | TokenAtMost
  | TokenLessThan
  | TokenAtLeast
  | TokenGreaterThan
  | TokenUnknown
  | TokenEnd

data ParserState = ParserState
  { parserTokens :: [Token]
  , parserLimits :: !LengthLimits
  , parserSurfaceSyntax :: !LengthWhereSurfaceSyntax
  }

data LiteralProvenance
  = DirectLiteral !Natural
  | FoldedLiteral !Natural

data ParsedExpression = ParsedExpression
  { parsedExpressionValue :: !(LengthExpression LengthWhereReference)
  , parsedExpressionLiteral :: !(Maybe LiteralProvenance)
  , parsedExpressionNodeOffsets :: [Natural]
  , parsedExpressionSumTerms :: Maybe [ParsedSumTerm]
  , parsedExpressionMinimumTerms :: Maybe [ParsedSumTerm]
  , parsedExpressionMaximumTerms :: Maybe [ParsedSumTerm]
  , parsedExpressionReferences :: [LengthWhereReference]
  }

newtype ParsedSumTerm = ParsedSumTerm ParsedExpression

-- | Parse one bounded ASCII postcondition.
--
-- Admission order is byte bound, whole-source ASCII validation, then
-- left-to-right grammar and semantic-limit validation.  The returned value
-- retains no source bytes.
parseLengthWhereSource
  :: LengthLimits
  -> ByteString
  -> Either LengthWhereParseError LengthWhereSource
parseLengthWhereSource = parseLengthWhereSourceWith LengthWhereCompactSyntax

-- | Parse one bounded Haskell-shaped Length postcondition.
--
-- This is a semantic surface parser, not a Haskell evaluator.  It accepts
-- ordinary application spellings such as @length arg0@, Haskell comparison
-- operators, and prefix or backticked @div@ and @mod@, then constructs the exact
-- same opaque source as 'parseLengthWhereSource'.  It grants no additional
-- model, role, solver, or execution authority.
parseHaskellLengthWhereSource
  :: LengthLimits
  -> ByteString
  -> Either LengthWhereParseError LengthWhereSource
parseHaskellLengthWhereSource =
  parseLengthWhereSourceWith LengthWhereHaskellSyntax

parseLengthWhereSourceWith
  :: LengthWhereSurfaceSyntax
  -> LengthLimits
  -> ByteString
  -> Either LengthWhereParseError LengthWhereSource
parseLengthWhereSourceWith surfaceSyntax limits source = do
  admitSourceBytes source
  admitAscii source
  let tokens = tokenize surfaceSyntax source
        ++ [Token (naturalLength source) TokenEnd]
      initial = ParserState tokens limits surfaceSyntax
  (left, afterLeft) <- parseSum 0 initial
  (relationOffset, relation, afterRelation) <- parseRelation afterLeft
  (right, afterRight) <- parseSum 0 afterRelation
  case peekToken afterRight of
    Token offset kind | isRelation kind ->
      Left $ LengthWhereChainedComparison offset
    trailing -> do
      validateEmittedSyntax limits relationOffset relation left right
      case trailing of
        Token _ TokenEnd -> do
          let formula = lowerRelation relation
                (parsedExpressionValue left)
                (parsedExpressionValue right)
          pure $ LengthWhereSource limits formula
            $ parsedExpressionReferences left
            ++ parsedExpressionReferences right
        Token offset _ ->
          Left $ LengthWhereUnexpectedToken offset LengthWhereEndExpected

-- | Elaborate physical references through one explicit complete role vector.
elaborateLengthWhereSource
  :: LengthWhereDomain
  -> [LengthTargetArgumentRole]
  -> LengthWhereSource
  -> Either LengthWhereElaborationError LengthWhereContractSource
elaborateLengthWhereSource domain roles
    (LengthWhereSource limits formula references) = do
  let maximumRoles = naturalInt $ lengthContractInputLimit limits
      observedRoles = naturalInt $ observedListLength
        (lengthContractInputLimit limits) roles
  if observedRoles > maximumRoles
    then Left $ LengthWhereRoleVectorLimitExceeded
      maximumRoles observedRoles
    else case domain of
      LengthWhereScalar -> do
        traverseList_ (scalarReference roles) references
        lowered <- traverseFormula (scalarReference roles) formula
        pure $ LengthWhereScalarContractSource roles LengthContractSource
          { lengthContractPrecondition = LengthTruth True
          , lengthContractPostcondition = lowered
          }
      LengthWhereBinaryProduct -> do
        traverseList_ (pairReference roles) references
        lowered <- traverseFormula (pairReference roles) formula
        pure $ LengthWhereBinaryProductContractSource roles
          LengthSpinePairContractSource
            { lengthSpinePairContractPrecondition = LengthTruth True
            , lengthSpinePairContractPostcondition = lowered
            }

-- Parsing --------------------------------------------------------------------

data Relation
  = RelationEqual
  | RelationNotEqual
  | RelationAtMost
  | RelationLessThan
  | RelationAtLeast
  | RelationGreaterThan

parseRelation
  :: ParserState
  -> Either LengthWhereParseError (Natural, Relation, ParserState)
parseRelation state = case peekToken state of
  Token offset TokenEqual -> pure (offset, RelationEqual, advance state)
  Token offset TokenNotEqual -> pure (offset, RelationNotEqual, advance state)
  Token offset TokenAtMost -> pure (offset, RelationAtMost, advance state)
  Token offset TokenLessThan -> pure (offset, RelationLessThan, advance state)
  Token offset TokenAtLeast -> pure (offset, RelationAtLeast, advance state)
  Token offset TokenGreaterThan ->
    pure (offset, RelationGreaterThan, advance state)
  token -> unexpectedToken LengthWhereRelationExpected token

parseSum
  :: Natural
  -> ParserState
  -> Either LengthWhereParseError (ParsedExpression, ParserState)
parseSum depth state = do
  (first, afterFirst) <- parseProduct depth state
  go first afterFirst
 where
  go left current = case peekToken current of
    Token offset TokenPlus -> do
      (right, afterRight) <- parseProduct depth $ advance current
      parsed <- sumExpression offset left right afterRight
      go parsed afterRight
    Token offset TokenMinus -> do
      (right, afterRight) <- parseProduct depth $ advance current
      let parsed = monusExpression offset left right
      go parsed afterRight
    _ -> pure (left, current)

parseProduct
  :: Natural
  -> ParserState
  -> Either LengthWhereParseError (ParsedExpression, ParserState)
parseProduct depth state = do
  (first, afterFirst) <- parseAtom depth state
  go first afterFirst
 where
  go left current = case peekToken current of
    Token offset TokenTimes -> do
      (right, afterRight) <- parseAtom depth $ advance current
      productExpression offset left right afterRight >>= uncurry go
    Token offset TokenDivide -> do
      (right, afterRight) <- parseAtom depth $ advance current
      divisor <- directPositiveDivisor offset right
      let parsed = quotientExpression offset divisor left
      go parsed afterRight
    Token offset TokenModulo -> do
      (right, afterRight) <- parseAtom depth $ advance current
      divisor <- directPositiveDivisor offset right
      let parsed = moduloExpression offset divisor left
      go parsed afterRight
    _ -> pure (left, current)

parseAtom
  :: Natural
  -> ParserState
  -> Either LengthWhereParseError (ParsedExpression, ParserState)
parseAtom depth state = case peekToken state of
  Token offset (TokenNatural digits) -> do
    value <- parseNaturalLiteral offset digits state
    pure
      ( literalExpression offset (DirectLiteral value) value
      , advance state
      )
  Token offset TokenLen -> parseLengthReference depth offset $ advance state
  Token offset TokenMinimum -> parseExtremum True depth offset $ advance state
  Token offset TokenMaximum -> parseExtremum False depth offset $ advance state
  Token offset TokenDivideFunction ->
    parseHaskellDivisorFunction True depth offset $ advance state
  Token offset TokenModuloFunction ->
    parseHaskellDivisorFunction False depth offset $ advance state
  Token offset TokenLeftParenthesis -> do
    nested <- enterNesting offset depth
    (inside, afterInside) <- parseSum nested $ advance state
    afterClose <- consumeExact LengthWhereRightParenthesisExpected
      TokenRightParenthesis afterInside
    pure (inside, afterClose)
  token -> unexpectedToken LengthWhereExpressionExpected token

parseLengthReference
  :: Natural
  -> Natural
  -> ParserState
  -> Either LengthWhereParseError (ParsedExpression, ParserState)
parseLengthReference depth offset state =
  case parserSurfaceSyntax state of
    LengthWhereCompactSyntax ->
      parseCompactLengthReference depth offset state
    LengthWhereHaskellSyntax ->
      parseHaskellLengthReference depth offset state

parseCompactLengthReference
  :: Natural
  -> Natural
  -> ParserState
  -> Either LengthWhereParseError (ParsedExpression, ParserState)
parseCompactLengthReference depth offset state = do
  nested <- enterNesting offset depth
  afterOpen <- consumeExact LengthWhereLeftParenthesisExpected
    TokenLeftParenthesis state
  (reference, afterReference) <- parseReference afterOpen
  afterClose <- consumeExact LengthWhereRightParenthesisExpected
    TokenRightParenthesis afterReference
  nested `seq` pure
    ( withReferences [reference]
        $ compoundExpression offset (LengthVariable reference) []
    , afterClose
    )

parseHaskellLengthReference
  :: Natural
  -> Natural
  -> ParserState
  -> Either LengthWhereParseError (ParsedExpression, ParserState)
parseHaskellLengthReference depth offset state = do
  (reference, afterReference) <- parseHaskellLengthArgument depth state
  pure
    ( withReferences [reference]
        $ compoundExpression offset (LengthVariable reference) []
    , afterReference
    )

parseHaskellLengthArgument
  :: Natural
  -> ParserState
  -> Either LengthWhereParseError (LengthWhereReference, ParserState)
parseHaskellLengthArgument depth state = case peekToken state of
  Token offset (TokenArgument digits) -> do
    index <- parsePhysicalArgument offset digits state
    pure (LengthWherePhysicalArgument index, advance state)
  Token _ TokenResult -> pure (LengthWhereScalarResult, advance state)
  Token offset TokenLeftParenthesis -> do
    nested <- enterNesting offset depth
    let afterOpen = advance state
    (reference, afterReference) <- case peekToken afterOpen of
      Token _ TokenFirst ->
        parseHaskellPairProjection LengthSpinePairFirst $ advance afterOpen
      Token _ TokenSecond ->
        parseHaskellPairProjection LengthSpinePairSecond $ advance afterOpen
      _ -> parseHaskellLengthArgument nested afterOpen
    afterClose <- consumeExact LengthWhereRightParenthesisExpected
      TokenRightParenthesis afterReference
    nested `seq` pure (reference, afterClose)
  token -> unexpectedToken LengthWhereReferenceExpected token

parseHaskellPairProjection
  :: LengthSpinePairComponent
  -> ParserState
  -> Either LengthWhereParseError (LengthWhereReference, ParserState)
parseHaskellPairProjection component state = case peekToken state of
  Token _ TokenResult ->
    pure (LengthWherePairResult component, advance state)
  token -> unexpectedToken LengthWhereReferenceExpected token

parseReference
  :: ParserState
  -> Either LengthWhereParseError (LengthWhereReference, ParserState)
parseReference state = case peekToken state of
  Token offset (TokenArgument digits) -> do
    index <- parsePhysicalArgument offset digits state
    pure (LengthWherePhysicalArgument index, advance state)
  Token _ TokenResult -> case peekToken $ advance state of
    Token _ TokenDot -> case peekToken $ advance $ advance state of
      Token _ TokenFirst -> pure
        ( LengthWherePairResult LengthSpinePairFirst
        , advance $ advance $ advance state
        )
      Token _ TokenSecond -> pure
        ( LengthWherePairResult LengthSpinePairSecond
        , advance $ advance $ advance state
        )
      token -> unexpectedToken LengthWhereReferenceExpected token
    _ -> pure (LengthWhereScalarResult, advance state)
  token -> unexpectedToken LengthWhereReferenceExpected token

parseExtremum
  :: Bool
  -> Natural
  -> Natural
  -> ParserState
  -> Either LengthWhereParseError (ParsedExpression, ParserState)
parseExtremum isMinimum depth offset state =
  case parserSurfaceSyntax state of
    LengthWhereCompactSyntax ->
      parseCompactExtremum isMinimum depth offset state
    LengthWhereHaskellSyntax ->
      parseHaskellExtremum isMinimum depth offset state

parseCompactExtremum
  :: Bool
  -> Natural
  -> Natural
  -> ParserState
  -> Either LengthWhereParseError (ParsedExpression, ParserState)
parseCompactExtremum isMinimum depth offset state = do
  nested <- enterNesting offset depth
  afterOpen <- consumeExact LengthWhereLeftParenthesisExpected
    TokenLeftParenthesis state
  (left, afterLeft) <- parseSum nested afterOpen
  afterComma <- consumeExact LengthWhereCommaExpected TokenComma afterLeft
  (right, afterRight) <- parseSum nested afterComma
  afterClose <- consumeExact LengthWhereRightParenthesisExpected
    TokenRightParenthesis afterRight
  let parsed = extremumExpression isMinimum offset left right
  pure
    ( parsed
    , afterClose
    )

parseHaskellExtremum
  :: Bool
  -> Natural
  -> Natural
  -> ParserState
  -> Either LengthWhereParseError (ParsedExpression, ParserState)
parseHaskellExtremum isMinimum depth offset state = do
  (left, afterLeft) <- parseHaskellFunctionArgument depth state
  (right, afterRight) <- parseHaskellFunctionArgument depth afterLeft
  pure (extremumExpression isMinimum offset left right, afterRight)

parseHaskellDivisorFunction
  :: Bool
  -> Natural
  -> Natural
  -> ParserState
  -> Either LengthWhereParseError (ParsedExpression, ParserState)
parseHaskellDivisorFunction isQuotient depth offset state = do
  (value, afterValue) <- parseHaskellFunctionArgument depth state
  (divisorExpression, afterDivisor) <-
    parseHaskellFunctionArgument depth afterValue
  divisor <- directPositiveDivisor offset divisorExpression
  pure
    ( if isQuotient
        then quotientExpression offset divisor value
        else moduloExpression offset divisor value
    , afterDivisor
    )

parseHaskellFunctionArgument
  :: Natural
  -> ParserState
  -> Either LengthWhereParseError (ParsedExpression, ParserState)
parseHaskellFunctionArgument depth state = case peekToken state of
  Token _ (TokenNatural _) -> parseAtom depth state
  Token _ TokenLeftParenthesis -> parseAtom depth state
  token -> unexpectedToken LengthWhereExpressionExpected token

productExpression
  :: Natural
  -> ParsedExpression
  -> ParsedExpression
  -> ParserState
  -> Either LengthWhereParseError (ParsedExpression, ParserState)
productExpression offset left right state =
  case (literalValue $ parsedExpressionLiteral left,
        literalValue $ parsedExpressionLiteral right) of
    (Just leftValue, Just rightValue) -> do
      let value = leftValue * rightValue
      checked <- checkLiteralBits offset value state
      pure
        ( withReferences references
            $ literalExpression offset (FoldedLiteral checked) checked
        , state
        )
    (Just factor, Nothing) -> do
      scaled <- scaleExpression offset factor right state
      pure (withReferences references scaled, state)
    (Nothing, Just factor) -> do
      scaled <- scaleExpression offset factor left state
      pure (withReferences references scaled, state)
    (Nothing, Nothing) -> Left $ LengthWhereNonlinearProduct offset
 where
  references = parsedExpressionReferences left
    ++ parsedExpressionReferences right

directPositiveDivisor
  :: Natural
  -> ParsedExpression
  -> Either LengthWhereParseError Natural
directPositiveDivisor offset expression =
  case parsedExpressionLiteral expression of
    Just (DirectLiteral 0) -> Left $ LengthWhereZeroDivisor offset
    Just (DirectLiteral value) -> Right value
    _ -> Left $ LengthWhereNonliteralDivisor offset

literalExpression
  :: Natural
  -> LiteralProvenance
  -> Natural
  -> ParsedExpression
literalExpression offset provenance value = ParsedExpression
  (LengthLiteral value) (Just provenance) [offset]
  Nothing Nothing Nothing []

compoundExpression
  :: Natural
  -> LengthExpression LengthWhereReference
  -> [ParsedExpression]
  -> ParsedExpression
compoundExpression offset value children = ParsedExpression
  value Nothing
  (offset : concatMap parsedExpressionNodeOffsets children)
  Nothing Nothing Nothing
  (concatMap parsedExpressionReferences children)

sumExpression
  :: Natural
  -> ParsedExpression
  -> ParsedExpression
  -> ParserState
  -> Either LengthWhereParseError ParsedExpression
sumExpression offset left right state = do
  let terms = sumTerms left ++ sumTerms right
  (literal, literalOffset, nonLiterals) <- collect 0 Nothing [] terms
  let retainedLiteral = case literalOffset of
        Nothing -> []
        Just firstOffset
          | literal == 0 && not (null nonLiterals) -> []
          | otherwise ->
              [ ParsedSumTerm
                  $ literalExpression firstOffset (FoldedLiteral literal) literal
              ]
      retained = List.sortOn sumTermValue
        $ retainedLiteral ++ reverse nonLiterals
  case retained of
    _ : _ : _ -> do
      _ <- checkCollectionWidth offset (naturalListLength retained) state
      Right ()
    _ -> Right ()
  pure $ withReferences references $ case retained of
    [] -> literalExpression offset (FoldedLiteral 0) 0
    [ParsedSumTerm expression] -> markFoldedLiteral expression
    _ -> ParsedExpression
      (LengthSum $ map parsedTermValue retained)
      Nothing
      (offset : concatMap parsedTermOffsets retained)
      (Just retained) Nothing Nothing
      []
 where
  references = parsedExpressionReferences left
    ++ parsedExpressionReferences right
  collect !literal !firstOffset !nonLiterals remaining = case remaining of
    [] -> Right (literal, firstOffset, nonLiterals)
    term : rest -> case parsedTermValue term of
      LengthLiteral termValue -> do
        combined <- checkLiteralBits offset (literal + termValue) state
        let termOffset = case parsedTermOffsets term of
              first : _ -> first
              [] -> offset
        collect combined
          (case firstOffset of
            Nothing -> Just termOffset
            Just original -> Just original)
          nonLiterals rest
      _ -> collect literal firstOffset
        (term : nonLiterals) rest

  sumTermValue = parsedTermValue

sumTerms :: ParsedExpression -> [ParsedSumTerm]
sumTerms expression = case parsedExpressionSumTerms expression of
  Just terms -> terms
  Nothing -> [ParsedSumTerm expression]

monusExpression
  :: Natural
  -> ParsedExpression
  -> ParsedExpression
  -> ParsedExpression
monusExpression offset left right =
  withReferences references
    $ case (parsedExpressionValue left, parsedExpressionValue right) of
    (LengthLiteral leftValue, LengthLiteral rightValue) ->
      literalExpression offset (FoldedLiteral value) value
     where
      value
        | leftValue >= rightValue = leftValue - rightValue
        | otherwise = 0
    (_, LengthLiteral 0) -> markFoldedLiteral left
    (leftValue, rightValue)
      | leftValue == rightValue ->
          literalExpression offset (FoldedLiteral 0) 0
      | otherwise -> compoundExpression offset
          (LengthMonus leftValue rightValue) [left, right]
 where
  references = parsedExpressionReferences left
    ++ parsedExpressionReferences right

quotientExpression
  :: Natural
  -> Natural
  -> ParsedExpression
  -> ParsedExpression
quotientExpression offset divisor expression =
  withReferences (parsedExpressionReferences expression)
    $ case parsedExpressionValue expression of
    LengthLiteral value -> literalExpression offset
      (FoldedLiteral result) result
     where
      result = value `quot` divisor
    _
      | divisor == 1 -> markFoldedLiteral expression
      | otherwise -> compoundExpression offset
          (LengthQuotient divisor $ parsedExpressionValue expression)
          [expression]

moduloExpression
  :: Natural
  -> Natural
  -> ParsedExpression
  -> ParsedExpression
moduloExpression offset divisor expression =
  withReferences (parsedExpressionReferences expression)
    $ case parsedExpressionValue expression of
    LengthLiteral value -> literalExpression offset
      (FoldedLiteral result) result
     where
      result = value `mod` divisor
    _
      | divisor == 1 -> literalExpression offset (FoldedLiteral 0) 0
      | otherwise -> compoundExpression offset
          (LengthModulo divisor $ parsedExpressionValue expression)
          [expression]

scaleExpression
  :: Natural
  -> Natural
  -> ParsedExpression
  -> ParserState
  -> Either LengthWhereParseError ParsedExpression
scaleExpression offset factor expression state = do
  case factor of
    0 -> pure $ literalExpression offset (FoldedLiteral 0) 0
    1 -> pure $ markFoldedLiteral expression
    _ -> do
      (combined, nested, nestedOffsets) <- combineLeadingScale factor
        (parsedExpressionValue expression)
        (parsedExpressionNodeOffsets expression)
      pure $ ParsedExpression
        (LengthScale combined nested)
        Nothing
        (offset : nestedOffsets)
        Nothing Nothing Nothing
        (parsedExpressionReferences expression)
 where
  combineLeadingScale !combined source offsets = case source of
    LengthScale nestedFactor nested -> do
      next <- checkLiteralBits offset (combined * nestedFactor) state
      combineLeadingScale next nested $ dropRootOffset offsets
    _ -> Right (combined, source, offsets)

  dropRootOffset offsets = case offsets of
    _ : remaining -> remaining
    [] -> []

extremumExpression
  :: Bool
  -> Natural
  -> ParsedExpression
  -> ParsedExpression
  -> ParsedExpression
extremumExpression isMinimum offset left right =
  withReferences references $ case retained of
    [] -> literalExpression offset (FoldedLiteral 0) 0
    [ParsedSumTerm expression] -> markFoldedLiteral expression
    first : remaining ->
      let (value, offsets) = List.foldl' append (termValue first) remaining
          minimumTerms = if isMinimum then Just retained else Nothing
          maximumTerms = if isMinimum then Nothing else Just retained
      in ParsedExpression value Nothing offsets
          Nothing minimumTerms maximumTerms []
 where
  references = parsedExpressionReferences left
    ++ parsedExpressionReferences right
  sourceTerms = extremumTerms isMinimum left
    ++ extremumTerms isMinimum right
  (literalTerm, nonLiteralTerms) = collectLiterals Nothing [] sourceTerms
  retained = deduplicateTerms $ List.sortOn parsedTermValue
    $ maybe id (:) literalTerm $ reverse nonLiteralTerms

  collectLiterals !literal !nonLiterals remaining = case remaining of
    [] -> (literal, nonLiterals)
    term : rest -> case parsedTermValue term of
      LengthLiteral current ->
        let combined = case literal of
              Nothing -> current
              Just previousTerm -> case parsedTermValue previousTerm of
                LengthLiteral previous ->
                  if isMinimum
                    then min previous current
                    else max previous current
                _ -> current
            firstOffset = case literal of
              Just original -> parsedTermOffsets original
              Nothing -> parsedTermOffsets term
        in collectLiterals
            (Just $ ParsedSumTerm $ literalExpression
              (firstOffsetOf offset firstOffset)
              (FoldedLiteral combined) combined)
            nonLiterals rest
      _ -> collectLiterals literal (term : nonLiterals) rest

  append (leftValue, leftOffsets) term =
    let (rightValue, rightOffsets) = termValue term
        value
          | isMinimum = LengthMinimum leftValue rightValue
          | otherwise = LengthMaximum leftValue rightValue
    in (value, offset : leftOffsets ++ rightOffsets)

  termValue term = (parsedTermValue term, parsedTermOffsets term)

extremumTerms :: Bool -> ParsedExpression -> [ParsedSumTerm]
extremumTerms isMinimum expression =
  case if isMinimum
      then parsedExpressionMinimumTerms expression
      else parsedExpressionMaximumTerms expression of
    Just terms -> terms
    Nothing -> [ParsedSumTerm expression]

parsedTermValue
  :: ParsedSumTerm
  -> LengthExpression LengthWhereReference
parsedTermValue (ParsedSumTerm expression) = parsedExpressionValue expression

parsedTermOffsets :: ParsedSumTerm -> [Natural]
parsedTermOffsets (ParsedSumTerm expression) =
  parsedExpressionNodeOffsets expression

firstOffsetOf :: Natural -> [Natural] -> Natural
firstOffsetOf fallback offsets = case offsets of
  first : _ -> first
  [] -> fallback

deduplicateTerms :: [ParsedSumTerm] -> [ParsedSumTerm]
deduplicateTerms source = case source of
  [] -> []
  first : remaining -> first : go (parsedTermValue first) remaining
 where
  go !_ [] = []
  go !previous (term : remaining)
    | parsedTermValue term == previous = go previous remaining
    | otherwise = term : go (parsedTermValue term) remaining

markFoldedLiteral :: ParsedExpression -> ParsedExpression
markFoldedLiteral expression = case parsedExpressionValue expression of
  LengthLiteral value -> expression
    { parsedExpressionLiteral = Just $ FoldedLiteral value }
  _ -> expression

withReferences
  :: [LengthWhereReference]
  -> ParsedExpression
  -> ParsedExpression
withReferences references expression = expression
  { parsedExpressionReferences = references }

literalValue :: Maybe LiteralProvenance -> Maybe Natural
literalValue source = case source of
  Just (DirectLiteral value) -> Just value
  Just (FoldedLiteral value) -> Just value
  Nothing -> Nothing

lowerRelation
  :: Relation
  -> LengthExpression variable
  -> LengthExpression variable
  -> LengthFormula variable
lowerRelation relation left right = case relation of
  RelationEqual -> LengthEqual left right
  RelationNotEqual -> LengthNot $ LengthEqual left right
  RelationAtMost -> LengthAtMost left right
  RelationLessThan -> LengthNot $ LengthAtMost right left
  RelationAtLeast -> LengthAtMost right left
  RelationGreaterThan -> LengthNot $ LengthAtMost left right

-- Limits ---------------------------------------------------------------------

admitSourceBytes :: ByteString -> Either LengthWhereParseError ()
admitSourceBytes source =
  let maximumBytes = lengthWhereMaximumSourceBytes
      actual = naturalLength source
  in if actual <= maximumBytes
      then Right ()
      else Left $ LengthWhereSourceByteLimitExceeded
        maximumBytes (maximumBytes + 1)

admitAscii :: ByteString -> Either LengthWhereParseError ()
admitAscii source = go 0
 where
  size = BS.length source
  go !offset
    | offset >= size = Right ()
    | BS.index source offset < 0x80 = go (offset + 1)
    | otherwise = Left $ LengthWhereNonAsciiByte $ fromIntegral offset

validateEmittedSyntax
  :: LengthLimits
  -> Natural
  -> Relation
  -> ParsedExpression
  -> ParsedExpression
  -> Either LengthWhereParseError ()
validateEmittedSyntax limits relationOffset relation left right = do
  validateSyntaxEvents limits
    $ [SyntaxNode 0, SyntaxClause 0]
    ++ postconditionEvents
  validateWithLengthNormalizer limits relationOffset
    $ lowerRelation relation
        (parsedExpressionValue left)
        (parsedExpressionValue right)
 where
  relationEvents = case relation of
    RelationNotEqual -> [relationOffset, relationOffset]
    RelationLessThan -> [relationOffset, relationOffset]
    RelationGreaterThan -> [relationOffset, relationOffset]
    _ -> [relationOffset]
  orderedExpressions = case relation of
    RelationLessThan -> [right, left]
    RelationAtLeast -> [right, left]
    _ -> [left, right]
  postconditionEvents = case relationEvents of
    [] -> []
    first : remaining -> SyntaxNode first
      : map SyntaxNode remaining
      ++ [SyntaxClause relationOffset]
      ++ concatMap
          (map SyntaxNode . parsedExpressionNodeOffsets)
          orderedExpressions

data SyntaxEvent
  = SyntaxNode !Natural
  | SyntaxClause !Natural

validateSyntaxEvents
  :: LengthLimits
  -> [SyntaxEvent]
  -> Either LengthWhereParseError ()
validateSyntaxEvents limits = go 0 0
 where
  maximumNodes = naturalInt $ lengthSyntaxNodeLimit limits
  maximumClauses = naturalInt $ lengthFormulaClauseLimit limits

  go !_ !_ [] = Right ()
  go !nodes !clauses (event : remaining) = case event of
    SyntaxNode offset
      | nodes < maximumNodes -> go (nodes + 1) clauses remaining
      | otherwise -> Left $ LengthWhereSyntaxLimitExceeded
          LengthWhereSyntaxNodes maximumNodes (maximumNodes + 1) offset
    SyntaxClause offset
      | clauses < maximumClauses -> go nodes (clauses + 1) remaining
      | otherwise -> Left $ LengthWhereSyntaxLimitExceeded
          LengthWhereFormulaClauses maximumClauses
            (maximumClauses + 1) offset

validateWithLengthNormalizer
  :: LengthLimits
  -> Natural
  -> LengthFormula LengthWhereReference
  -> Either LengthWhereParseError ()
validateWithLengthNormalizer limits offset postcondition =
  case Internal.normalizeLengthFormula limits (const $ Right ())
      Internal.emptySyntaxUsage
      (LengthTruth True :: LengthFormula LengthWhereReference) of
    Left failure -> Left $ translateLengthSyntaxFailure offset failure
    Right (_, afterPrecondition) ->
      case Internal.normalizeLengthFormula limits (const $ Right ())
          afterPrecondition postcondition of
        Left failure -> Left $ translateLengthSyntaxFailure offset failure
        Right _ -> Right ()

translateLengthSyntaxFailure
  :: Natural
  -> Internal.LengthSyntaxError
  -> LengthWhereParseError
translateLengthSyntaxFailure offset failure = case failure of
  Internal.LengthSyntaxNodeLimitExceeded maximumCount observed ->
    limit LengthWhereSyntaxNodes maximumCount observed
  Internal.LengthFormulaClauseLimitExceeded maximumCount observed ->
    limit LengthWhereFormulaClauses maximumCount observed
  Internal.LengthSyntaxCollectionLimitExceeded _ maximumCount observed ->
    limit LengthWhereCollectionWidth maximumCount observed
  Internal.LengthLiteralBitLimitExceeded maximumCount observed ->
    limit LengthWhereLiteralBits maximumCount observed
  Internal.LengthQuotientDivisorZero -> LengthWhereZeroDivisor offset
  Internal.LengthModuloDivisorZero -> LengthWhereZeroDivisor offset
  Internal.LengthResultNotAvailableInPrecondition -> referenceFailure
  Internal.LengthInputReferenceOutOfRange{} -> referenceFailure
  Internal.LengthProviderReferenceOutOfRange{} -> referenceFailure
  Internal.LengthProviderReferenceIsUnobserved{} -> referenceFailure
 where
  limit resource maximumCount observed = LengthWhereSyntaxLimitExceeded
    resource (naturalInt maximumCount) (naturalInt observed) offset
  referenceFailure = LengthWhereUnexpectedToken
    offset LengthWhereReferenceExpected

checkCollectionWidth
  :: Natural
  -> Natural
  -> ParserState
  -> Either LengthWhereParseError Natural
checkCollectionWidth offset observed state =
  let maximumWidth = naturalInt
        $ lengthCollectionWidthLimit $ parserLimits state
  in if observed <= maximumWidth
      then Right observed
      else Left $ LengthWhereSyntaxLimitExceeded
        LengthWhereCollectionWidth maximumWidth (maximumWidth + 1) offset

checkLiteralBits
  :: Natural
  -> Natural
  -> ParserState
  -> Either LengthWhereParseError Natural
checkLiteralBits offset value state =
  let maximumBitsInt = lengthLiteralBitLimit $ parserLimits state
      observedBitsInt = observedNaturalBits maximumBitsInt value
      maximumBits = naturalInt maximumBitsInt
      observedBits = naturalInt observedBitsInt
  in if observedBits <= maximumBits
      then Right value
      else Left $ LengthWhereSyntaxLimitExceeded
        LengthWhereLiteralBits maximumBits (maximumBits + 1) offset

parseNaturalLiteral
  :: Natural
  -> ByteString
  -> ParserState
  -> Either LengthWhereParseError Natural
parseNaturalLiteral offset digits state =
  let maximumBitsInt = lengthLiteralBitLimit $ parserLimits state
      maximumBits = naturalInt maximumBitsInt
      maximumValue
        | maximumBitsInt <= maximumDecimalSourceBits =
            Just $ (2 ^ maximumBitsInt) - 1
        | otherwise = Nothing
  in case decimalNaturalWithin maximumValue digits of
      Just value -> Right value
      Nothing -> Left $ LengthWhereSyntaxLimitExceeded
        LengthWhereLiteralBits maximumBits (maximumBits + 1) offset

parsePhysicalArgument
  :: Natural
  -> ByteString
  -> ParserState
  -> Either LengthWhereParseError Natural
parsePhysicalArgument offset digits state =
  let maximumCount = naturalInt
        $ lengthContractInputLimit $ parserLimits state
  in if maximumCount == 0
      then Left $ LengthWherePhysicalArgumentUnavailable offset
      else
        let maximumIndex = maximumCount - 1
        in case decimalNaturalWithin (Just maximumIndex) digits of
            Just value -> Right value
            Nothing -> Left $ LengthWhereSyntaxLimitExceeded
              LengthWherePhysicalArgumentIndex maximumIndex
                (maximumIndex + 1) offset

enterNesting
  :: Natural
  -> Natural
  -> Either LengthWhereParseError Natural
enterNesting offset depth =
  let observed = depth + 1
  in if observed <= lengthWhereMaximumNestingDepth
      then Right observed
      else Left $ LengthWhereSyntaxLimitExceeded
        LengthWhereNestingDepth lengthWhereMaximumNestingDepth
          (lengthWhereMaximumNestingDepth + 1) offset

-- Elaboration ----------------------------------------------------------------

scalarReference
  :: [LengthTargetArgumentRole]
  -> LengthWhereReference
  -> Either LengthWhereElaborationError LengthContractVariable
scalarReference roles reference = case reference of
  LengthWherePhysicalArgument index ->
    LengthInput <$> compactInputIndex roles index
  LengthWhereScalarResult -> Right LengthResult
  LengthWherePairResult component ->
    Left $ LengthWhereScalarDomainPairResult component

pairReference
  :: [LengthTargetArgumentRole]
  -> LengthWhereReference
  -> Either LengthWhereElaborationError LengthSpinePairContractVariable
pairReference roles reference = case reference of
  LengthWherePhysicalArgument index ->
    LengthSpinePairInput <$> compactInputIndex roles index
  LengthWhereScalarResult ->
    Left LengthWhereBinaryProductDomainScalarResult
  LengthWherePairResult component ->
    Right $ LengthSpinePairResult component

compactInputIndex
  :: [LengthTargetArgumentRole]
  -> Natural
  -> Either LengthWhereElaborationError Natural
compactInputIndex roles physical = go 0 0 roles
 where
  go !position !compact remaining = case remaining of
    [] -> Left $ LengthWherePhysicalArgumentOutOfRange
      physical position
    role : rest
      | position == physical -> case role of
          LengthObservedSpine -> Right compact
          LengthUnobservedTarget ->
            Left $ LengthWherePhysicalArgumentNotObserved physical
      | otherwise -> go (position + 1)
          (case role of
            LengthObservedSpine -> compact + 1
            LengthUnobservedTarget -> compact)
          rest

traverseFormula
  :: (source -> Either failure target)
  -> LengthFormula source
  -> Either failure (LengthFormula target)
traverseFormula convert =
  Internal.rewriteLengthFormula (fmap LengthVariable . convert)

traverseList_
  :: (source -> Either failure target)
  -> [source]
  -> Either failure ()
traverseList_ _ [] = Right ()
traverseList_ convert (value : remaining) = do
  _ <- convert value
  traverseList_ convert remaining

-- Tokenization ---------------------------------------------------------------

tokenize :: LengthWhereSurfaceSyntax -> ByteString -> [Token]
tokenize surfaceSyntax source = go 0
 where
  size = BS.length source

  go !offset
    | offset >= size = []
    | asciiWhitespace (BS.index source offset) = go $ skipWhitespace offset
    | asciiDigit (BS.index source offset) =
        let end = spanWhile asciiDigit offset
        in Token (fromIntegral offset)
             (TokenNatural $ BS.take (end - offset) $ BS.drop offset source)
           : go end
    | asciiIdentifierStart (BS.index source offset) =
        let end = spanWhile asciiIdentifierContinue offset
            spelling = BS.take (end - offset) $ BS.drop offset source
        in Token (fromIntegral offset)
             (identifierToken surfaceSyntax spelling) : go end
    | otherwise = symbolToken offset

  skipWhitespace = spanWhile asciiWhitespace

  spanWhile predicate = advanceWhile
   where
    advanceWhile !position
      | position < size && predicate (BS.index source position) =
          advanceWhile $ position + 1
      | otherwise = position

  symbolToken offset =
    let byte = BS.index source offset
        next = if offset + 1 < size
          then Just $ BS.index source (offset + 1)
          else Nothing
        remaining = BS.drop offset source
        two kind = Token (fromIntegral offset) kind : go (offset + 2)
        five kind = Token (fromIntegral offset) kind : go (offset + 5)
        one kind = Token (fromIntegral offset) kind : go (offset + 1)
    in case (surfaceSyntax, byte, next) of
      (LengthWhereCompactSyntax, 0x21, Just 0x3d) -> two TokenNotEqual
      (LengthWhereCompactSyntax, 0x3d, Just 0x3d) -> two TokenUnknown
      (LengthWhereHaskellSyntax, 0x2f, Just 0x3d) -> two TokenNotEqual
      (LengthWhereHaskellSyntax, 0x3d, Just 0x3d) -> two TokenEqual
      (LengthWhereHaskellSyntax, 0x21, Just 0x3d) -> two TokenUnknown
      (LengthWhereHaskellSyntax, 0x60, _)
        | BS.take 5 remaining == ascii "`div`" -> five TokenDivide
        | BS.take 5 remaining == ascii "`mod`" -> five TokenModulo
      (_, 0x3c, Just 0x3d) -> two TokenAtMost
      (_, 0x3e, Just 0x3d) -> two TokenAtLeast
      (_, 0x28, _) -> one TokenLeftParenthesis
      (_, 0x29, _) -> one TokenRightParenthesis
      (_, 0x2c, _) -> one TokenComma
      (_, 0x2e, _) -> one TokenDot
      (_, 0x2b, _) -> one TokenPlus
      (_, 0x2d, _) -> one TokenMinus
      (_, 0x2a, _) -> one TokenTimes
      (LengthWhereCompactSyntax, 0x2f, _) -> one TokenDivide
      (LengthWhereCompactSyntax, 0x25, _) -> one TokenModulo
      (LengthWhereCompactSyntax, 0x3d, _) -> one TokenEqual
      (LengthWhereHaskellSyntax, 0x2f, _) -> one TokenUnknown
      (LengthWhereHaskellSyntax, 0x25, _) -> one TokenUnknown
      (LengthWhereHaskellSyntax, 0x3d, _) -> one TokenUnknown
      (_, 0x3c, _) -> one TokenLessThan
      (_, 0x3e, _) -> one TokenGreaterThan
      _ -> one TokenUnknown

identifierToken :: LengthWhereSurfaceSyntax -> ByteString -> TokenKind
identifierToken surfaceSyntax spelling
  | spelling == lengthName = TokenLen
  | spelling == ascii "result" = TokenResult
  | spelling == firstName = TokenFirst
  | spelling == secondName = TokenSecond
  | spelling == ascii "min" = TokenMinimum
  | spelling == ascii "max" = TokenMaximum
  | surfaceSyntax == LengthWhereHaskellSyntax
  , spelling == ascii "div" = TokenDivideFunction
  | surfaceSyntax == LengthWhereHaskellSyntax
  , spelling == ascii "mod" = TokenModuloFunction
  | BS.take 3 spelling == ascii "arg"
  , let digits = BS.drop 3 spelling
  , not (BS.null digits)
  , BS.all asciiDigit digits = TokenArgument digits
  | otherwise = TokenUnknown
 where
  lengthName = case surfaceSyntax of
    LengthWhereCompactSyntax -> ascii "len"
    LengthWhereHaskellSyntax -> ascii "length"
  firstName = case surfaceSyntax of
    LengthWhereCompactSyntax -> ascii "first"
    LengthWhereHaskellSyntax -> ascii "fst"
  secondName = case surfaceSyntax of
    LengthWhereCompactSyntax -> ascii "second"
    LengthWhereHaskellSyntax -> ascii "snd"

ascii :: String -> ByteString
ascii = BS.pack . map (fromIntegral . fromEnum)

asciiWhitespace, asciiDigit, asciiIdentifierStart,
  asciiIdentifierContinue :: Word8 -> Bool
asciiWhitespace byte = byte == 0x20 || byte == 0x09
  || byte == 0x0a || byte == 0x0d
asciiDigit byte = byte >= 0x30 && byte <= 0x39
asciiIdentifierStart byte =
  (byte >= 0x41 && byte <= 0x5a) || (byte >= 0x61 && byte <= 0x7a)
asciiIdentifierContinue byte = asciiIdentifierStart byte || asciiDigit byte

peekToken :: ParserState -> Token
peekToken state = case parserTokens state of
  token : _ -> token
  [] -> Token 0 TokenEnd

advance :: ParserState -> ParserState
advance state = state
  { parserTokens = case parserTokens state of
      _ : remaining -> remaining
      [] -> []
  }

consumeExact
  :: LengthWhereExpected
  -> TokenKind
  -> ParserState
  -> Either LengthWhereParseError ParserState
consumeExact expected wanted state = case peekToken state of
  Token _ actual | sameToken actual wanted -> Right $ advance state
  token -> unexpectedToken expected token

unexpectedToken
  :: LengthWhereExpected
  -> Token
  -> Either LengthWhereParseError a
unexpectedToken expected (Token offset kind) = Left $ case kind of
  TokenEnd -> LengthWhereUnexpectedEnd offset expected
  _ -> LengthWhereUnexpectedToken offset expected

sameToken :: TokenKind -> TokenKind -> Bool
sameToken left right = case (left, right) of
  (TokenLeftParenthesis, TokenLeftParenthesis) -> True
  (TokenRightParenthesis, TokenRightParenthesis) -> True
  (TokenComma, TokenComma) -> True
  _ -> False

isRelation :: TokenKind -> Bool
isRelation source = case source of
  TokenEqual -> True
  TokenNotEqual -> True
  TokenAtMost -> True
  TokenLessThan -> True
  TokenAtLeast -> True
  TokenGreaterThan -> True
  _ -> False

decimalNaturalWithin :: Maybe Natural -> ByteString -> Maybe Natural
decimalNaturalWithin maximumValue = go 0
 where
  go !value remaining = case BS.uncons remaining of
    Nothing -> Just value
    Just (byte, rest) ->
      let digit = fromIntegral $ byte - 0x30
      in case maximumValue of
          Nothing -> go (value * 10 + digit) rest
          Just maximumAllowed
            | value > maximumAllowed `quot` 10 -> Nothing
            | value == maximumAllowed `quot` 10
            , digit > maximumAllowed `rem` 10 -> Nothing
            | otherwise -> go (value * 10 + digit) rest

-- Any 16,384-digit decimal numeral is strictly smaller than @2^65536@.
-- Above this bit limit the byte admission bound itself makes overflow
-- impossible, so no enormous power-of-two sentinel is constructed.
maximumDecimalSourceBits :: Int
maximumDecimalSourceBits = 65536

naturalLength :: ByteString -> Natural
naturalLength = fromIntegral . BS.length

naturalListLength :: [value] -> Natural
naturalListLength = go 0
 where
  go !count [] = count
  go !count (_ : remaining) = go (count + 1) remaining

naturalInt :: Int -> Natural
naturalInt = fromIntegral . max 0
