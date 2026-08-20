{-# LANGUAGE DeriveGeneric #-}

-- | Private, query-aware decoding of bounded SMT-LIB responses for Length.
--
-- The lexer and S-expression tree are shared implementation machinery.  This
-- module exposes only the narrow check-status and input-valuation shapes that
-- the canonical Length query can request.  It performs no process framing or
-- query/run association and grants no authority to a parsed solver status.
module Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Response
  ( lengthSMTLibResponseSchemaTag
  , LengthSMTLibResponseLimitSource (..)
  , LengthSMTLibResponseLimits
  , LengthSMTLibResponseLimitField (..)
  , LengthSMTLibResponseLimitError (..)
  , mkLengthSMTLibResponseLimits
  , defaultLengthSMTLibResponseLimitSource
  , defaultLengthSMTLibResponseLimits
  , lengthSMTLibResponseByteLimit
  , lengthSMTLibResponseNestingDepthLimit
  , lengthSMTLibResponseNodeLimit
  , lengthSMTLibResponseTokenByteLimit
  , lengthSMTLibResponseIntegerBitLimit
  , SMTLibTokenPart (..)
  , SMTLibParseError (..)
  , LengthSMTLibResponseError (..)
  , parseLengthSMTLibCheckResponse
  , parseLengthSMTLibInputValueResponse
  , parseLengthSpinePairSMTLibInputValueResponse
  ) where

import Control.DeepSeq (NFData (rnf))
import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Collection (observedListLength)
import Language.Haskell.Synthesis.Internal.SMTLib.Response
  ( SMTLibAtom (..)
  , SMTLibParseError (..)
  , SMTLibResponseLimitSource (..)
  , SMTLibResponseLimits
  , SMTLibSExpression (..)
  , SMTLibTokenPart (..)
  , mkSMTLibResponseLimits
  , parseSMTLibSExpression
  , smtLibResponseByteLimit
  , smtLibResponseNestingDepthLimit
  , smtLibResponseNodeLimit
  , smtLibResponseNumeralBitLimit
  , smtLibResponseTokenByteLimit
  , smtLibSymbolBytes
  )
import qualified Language.Haskell.Synthesis.Internal.SMTLib.Response.Standard
  as Standard
import Language.Haskell.Synthesis.Semantic.Length.SMTLib
  ( LengthSMTLibIntegerBinding (..)
  , LengthSMTLibQuery
  , LengthSpinePairSMTLibQuery
  , lengthSMTLibQueryInputSymbols
  , lengthSMTLibQueryInputValueRequestBytes
  , lengthSpinePairSMTLibQueryInputSymbols
  , lengthSpinePairSMTLibQueryInputValueRequestBytes
  )
import Language.Haskell.Synthesis.Semantic.Observation (SolverStatus)

-- | Versioned lexical, shape-checking, symbol-normalization, and integer
-- decoding policy.  A future execution identity must bind this schema as well
-- as the query and solver/process configuration.
lengthSMTLibResponseSchemaTag :: [Word8]
lengthSMTLibResponseSchemaTag = ascii "djex-length-z3-smtlib2-response/v1"

-- | Raw public limits.  Signed fields are validated in declaration order;
-- natural byte and node fields admit zero directly.
data LengthSMTLibResponseLimitSource = LengthSMTLibResponseLimitSource
  { lengthSMTLibResponseLimitSourceBytes :: Natural
  , lengthSMTLibResponseLimitSourceNestingDepth :: Int
  , lengthSMTLibResponseLimitSourceNodes :: Natural
  , lengthSMTLibResponseLimitSourceTokenBytes :: Natural
  , lengthSMTLibResponseLimitSourceIntegerBits :: Int
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibResponseLimitSource

-- | Validated response limits whose shared parser policy is their single
-- retained owner.  The strict outer data wrapper preserves the Length-specific
-- abstraction and its established weak-head demand.
data LengthSMTLibResponseLimits = LengthSMTLibResponseLimits
  !SMTLibResponseLimits
  deriving (Eq, Ord)

-- Preserve the historical six-field derived spelling even though the first
-- five values are now projections from the final parser-policy owner.
instance Show LengthSMTLibResponseLimits where
  showsPrec precedence limits@(LengthSMTLibResponseLimits parserLimits) =
    showParen (precedence > 10)
      $ showString "LengthSMTLibResponseLimits "
      . showsPrec 11 (lengthSMTLibResponseByteLimit limits)
      . showChar ' '
      . showsPrec 11 (lengthSMTLibResponseNestingDepthLimit limits)
      . showChar ' '
      . showsPrec 11 (lengthSMTLibResponseNodeLimit limits)
      . showChar ' '
      . showsPrec 11 (lengthSMTLibResponseTokenByteLimit limits)
      . showChar ' '
      . showsPrec 11 (lengthSMTLibResponseIntegerBitLimit limits)
      . showChar ' '
      . showsPrec 11 parserLimits

instance NFData LengthSMTLibResponseLimits where
  rnf (LengthSMTLibResponseLimits parserLimits) = rnf parserLimits

-- | Names the signed field of t'LengthSMTLibResponseLimitSource' that
-- 'mkLengthSMTLibResponseLimits' rejected.  Only the nesting depth and the
-- integer bit limit are signed and can fail validation.
data LengthSMTLibResponseLimitField
  = LengthSMTLibResponseNestingDepth
  | LengthSMTLibResponseIntegerBits
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibResponseLimitField

-- | Rejection of a negative signed limit, carrying the offending field and
-- its value.  Nesting depth is checked before integer bits.
data LengthSMTLibResponseLimitError = NegativeLengthSMTLibResponseLimit
  !LengthSMTLibResponseLimitField !Int
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibResponseLimitError

-- | Validate raw Length response limits.  The nesting depth and then the
-- integer bit limit must be non-negative; the natural byte, node, and token
-- fields are accepted as given, zero included.  On success the values feed
-- the shared parser policy through 'mkSMTLibResponseLimits'.
mkLengthSMTLibResponseLimits
  :: LengthSMTLibResponseLimitSource
  -> Either LengthSMTLibResponseLimitError LengthSMTLibResponseLimits
mkLengthSMTLibResponseLimits source
  | depth < 0 = Left $ NegativeLengthSMTLibResponseLimit
      LengthSMTLibResponseNestingDepth depth
  | integerBits < 0 = Left $ NegativeLengthSMTLibResponseLimit
      LengthSMTLibResponseIntegerBits integerBits
  | otherwise = Right $ buildLengthSMTLibResponseLimits source
 where
  depth = lengthSMTLibResponseLimitSourceNestingDepth source
  integerBits = lengthSMTLibResponseLimitSourceIntegerBits source

-- | Default raw limits: 65536 response bytes, nesting depth 64, 4096 nodes,
-- 4096 token bytes, and 4096 integer bits, the same values as
-- @defaultSMTLibResponseLimitSource@ in
-- "Language.Haskell.Synthesis.Internal.SMTLib.Response".
defaultLengthSMTLibResponseLimitSource :: LengthSMTLibResponseLimitSource
defaultLengthSMTLibResponseLimitSource = LengthSMTLibResponseLimitSource
  { lengthSMTLibResponseLimitSourceBytes = 65536
  , lengthSMTLibResponseLimitSourceNestingDepth = 64
  , lengthSMTLibResponseLimitSourceNodes = 4096
  , lengthSMTLibResponseLimitSourceTokenBytes = 4096
  , lengthSMTLibResponseLimitSourceIntegerBits = 4096
  }

-- | The validated form of 'defaultLengthSMTLibResponseLimitSource', built
-- directly because its signed fields are known to be non-negative.
defaultLengthSMTLibResponseLimits :: LengthSMTLibResponseLimits
defaultLengthSMTLibResponseLimits =
  buildLengthSMTLibResponseLimits defaultLengthSMTLibResponseLimitSource

buildLengthSMTLibResponseLimits
  :: LengthSMTLibResponseLimitSource
  -> LengthSMTLibResponseLimits
buildLengthSMTLibResponseLimits source = LengthSMTLibResponseLimits
  $ mkSMTLibResponseLimits SMTLibResponseLimitSource
      { smtLibResponseLimitSourceBytes = bytes
      , smtLibResponseLimitSourceNestingDepth = fromIntegral depth
      , smtLibResponseLimitSourceNodes = nodes
      , smtLibResponseLimitSourceTokenBytes = tokenBytes
      , smtLibResponseLimitSourceNumeralBits = fromIntegral integerBits
      }
 where
  bytes = lengthSMTLibResponseLimitSourceBytes source
  depth = lengthSMTLibResponseLimitSourceNestingDepth source
  nodes = lengthSMTLibResponseLimitSourceNodes source
  tokenBytes = lengthSMTLibResponseLimitSourceTokenBytes source
  integerBits = lengthSMTLibResponseLimitSourceIntegerBits source

-- | Maximum number of response bytes retained before tokenization; a longer
-- response is rejected with 'SMTLibResponseByteLimitExceeded' as soon as one
-- more byte is seen.
lengthSMTLibResponseByteLimit :: LengthSMTLibResponseLimits -> Natural
lengthSMTLibResponseByteLimit
    (LengthSMTLibResponseLimits limits) = smtLibResponseByteLimit limits

-- | Maximum list nesting depth, returned as the 'Int' the source supplied.
-- Opening a list when the current depth already equals this limit fails with
-- 'SMTLibNestingDepthLimitExceeded'.
lengthSMTLibResponseNestingDepthLimit
  :: LengthSMTLibResponseLimits
  -> Int
lengthSMTLibResponseNestingDepthLimit
    (LengthSMTLibResponseLimits limits) = fromIntegral
      $ smtLibResponseNestingDepthLimit limits

-- | Maximum number of S-expression nodes, counting every list and every atom,
-- admitted in one response; the next node fails with
-- 'SMTLibNodeLimitExceeded'.
lengthSMTLibResponseNodeLimit :: LengthSMTLibResponseLimits -> Natural
lengthSMTLibResponseNodeLimit
    (LengthSMTLibResponseLimits limits) = smtLibResponseNodeLimit limits

-- | Maximum content bytes of one token: bare tokens count their whole lexeme,
-- strings count both bytes of each doubled-quote escape, and quoted symbols
-- count their delimiter-free content (see 'SMTLibTokenPart').
lengthSMTLibResponseTokenByteLimit
  :: LengthSMTLibResponseLimits
  -> Natural
lengthSMTLibResponseTokenByteLimit
    (LengthSMTLibResponseLimits limits) = smtLibResponseTokenByteLimit limits

-- | Maximum bit width of a decoded integer magnitude, returned as the 'Int'
-- the source supplied.  A numeral whose value reaches @2 ^ limit@ is
-- rejected with 'SMTLibNumeralBitLimitExceeded'; this is the Length name for
-- the shared numeral bit limit.
lengthSMTLibResponseIntegerBitLimit
  :: LengthSMTLibResponseLimits
  -> Int
lengthSMTLibResponseIntegerBitLimit
    (LengthSMTLibResponseLimits limits) = fromIntegral
      $ smtLibResponseNumeralBitLimit limits

-- | A syntactically valid response can still be the wrong response for the
-- command.  Symbol-bearing errors retain only bytes already checked against
-- the configured token bound.
data LengthSMTLibResponseError
  = LengthSMTLibResponseSyntaxError !SMTLibParseError
  | LengthSMTLibUnsupportedResponse
  | LengthSMTLibSolverErrorResponse [Word8]
  | LengthSMTLibSuccessWhereStatusExpected
  | LengthSMTLibUnexpectedCheckResponse
  | LengthSMTLibInputValueResponseNotExpected
  | LengthSMTLibUnexpectedInputValueResponse
  | LengthSMTLibInputValueArityMismatch !Int !Int
  | LengthSMTLibValuationPairExpected !Int
  | LengthSMTLibValuationPairArityMismatch !Int !Int
  | LengthSMTLibValuationTermNotSymbol !Int
  | LengthSMTLibUnknownValuationSymbol !Int [Word8]
  | LengthSMTLibDuplicateValuationSymbol !Int [Word8]
  | LengthSMTLibValuationValueNotInteger !Int
  | LengthSMTLibNegativeZeroIntegerValue !Int
  | LengthSMTLibMissingValuationSymbol [Word8]
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibResponseError

-- | Decode exactly one standard @check-sat@ response.  This classification is
-- still a raw observation and never creates behavioral evidence.
parseLengthSMTLibCheckResponse
  :: LengthSMTLibResponseLimits
  -> [Word8]
  -> Either LengthSMTLibResponseError SolverStatus
parseLengthSMTLibCheckResponse
    (LengthSMTLibResponseLimits parserLimits) bytes =
  case Standard.parseSMTLibCheckResponse parserLimits bytes of
    Left failure -> Left $ mapCheckResponseError failure
    Right status -> Right status

-- | Decode the exact valuation set requested by one canonical query.  Pairs
-- may arrive in any order, but the returned bindings are restored to query
-- and source order.  This function does not validate natural-domain values or
-- the counterexample condition; callers must pass the result to
-- 'validateLengthSMTLibCounterexample'.
parseLengthSMTLibInputValueResponse
  :: LengthSMTLibResponseLimits
  -> LengthSMTLibQuery identity local
  -> [Word8]
  -> Either LengthSMTLibResponseError [LengthSMTLibIntegerBinding]
parseLengthSMTLibInputValueResponse limits query =
  parseInputValueResponseFor limits
    (lengthSMTLibQueryInputValueRequestBytes query)
    (lengthSMTLibQueryInputSymbols query)

-- | Decode the exact input-only valuation requested by one canonical
-- binary-product spine query.  The lexical and integer policy is shared with
-- the scalar decoder because the returned artifact is still only an
-- authority-free list of input bindings.  Product-domain authority is granted
-- only later by independent two-component replay.
parseLengthSpinePairSMTLibInputValueResponse
  :: LengthSMTLibResponseLimits
  -> LengthSpinePairSMTLibQuery identity local
  -> [Word8]
  -> Either LengthSMTLibResponseError [LengthSMTLibIntegerBinding]
parseLengthSpinePairSMTLibInputValueResponse limits query =
  parseInputValueResponseFor limits
    (lengthSpinePairSMTLibQueryInputValueRequestBytes query)
    (lengthSpinePairSMTLibQueryInputSymbols query)

-- | Shared body of the two input-value response parsers.  Both domains
-- retain the same rejection vocabulary, so only the query's value-request
-- projection and tracked input symbols arrive as arguments.
parseInputValueResponseFor
  :: LengthSMTLibResponseLimits
  -> Maybe [Word8]
  -> [[Word8]]
  -> [Word8]
  -> Either LengthSMTLibResponseError [LengthSMTLibIntegerBinding]
parseInputValueResponseFor limits valueRequest symbols bytes = do
  case valueRequest of
    Nothing -> Left LengthSMTLibInputValueResponseNotExpected
    Just _ -> Right ()
  expression <- parseResponse limits bytes
  case Standard.classifySMTLibStandardResponseFailure expression of
    Just failure -> Left $ mapStandardResponseFailure failure
    Nothing -> decodeValuationsFor symbols expression

parseResponse
  :: LengthSMTLibResponseLimits
  -> [Word8]
  -> Either LengthSMTLibResponseError SMTLibSExpression
parseResponse (LengthSMTLibResponseLimits parserLimits) bytes =
  case parseSMTLibSExpression parserLimits bytes of
    Left failure -> Left $ LengthSMTLibResponseSyntaxError failure
    Right expression -> Right expression

mapCheckResponseError
  :: Standard.SMTLibCheckResponseError
  -> LengthSMTLibResponseError
mapCheckResponseError failure = case failure of
  Standard.SMTLibCheckResponseSyntaxError syntax ->
    LengthSMTLibResponseSyntaxError syntax
  Standard.SMTLibCheckResponseStandardFailure standard ->
    mapStandardResponseFailure standard
  Standard.SMTLibCheckResponseSuccessWhereStatusExpected ->
    LengthSMTLibSuccessWhereStatusExpected
  Standard.SMTLibCheckResponseUnexpected ->
    LengthSMTLibUnexpectedCheckResponse

mapStandardResponseFailure
  :: Standard.SMTLibStandardResponseFailure
  -> LengthSMTLibResponseError
mapStandardResponseFailure failure = case failure of
  Standard.SMTLibStandardUnsupportedResponse ->
    LengthSMTLibUnsupportedResponse
  Standard.SMTLibStandardSolverErrorResponse message ->
    LengthSMTLibSolverErrorResponse message

decodeValuationsFor
  :: [[Word8]]
  -> SMTLibSExpression
  -> Either LengthSMTLibResponseError [LengthSMTLibIntegerBinding]
decodeValuationsFor symbols expression = case expression of
  SMTLibListExpression pairs -> do
    let expectedCount = length symbols
        observedCount = observedListLength expectedCount pairs
    if observedCount == expectedCount
      then Right ()
      else Left $ LengthSMTLibInputValueArityMismatch
        expectedCount observedCount
    decoded <- foldM
      (decodeValuation $ Map.fromList $ map (\symbol -> (symbol, ())) symbols)
      Map.empty
      $ zip [0 :: Int ..] pairs
    mapM (restoreBinding decoded) symbols
  _ -> Left LengthSMTLibUnexpectedInputValueResponse

decodeValuation
  :: Map [Word8] ()
  -> Map [Word8] Integer
  -> (Int, SMTLibSExpression)
  -> Either LengthSMTLibResponseError (Map [Word8] Integer)
decodeValuation expected decoded (pairIndex, expression) = do
  fields <- case expression of
    SMTLibListExpression values -> Right values
    _ -> Left $ LengthSMTLibValuationPairExpected pairIndex
  let observedArity = observedListLength 2 fields
  if observedArity == 2
    then Right ()
    else Left $ LengthSMTLibValuationPairArityMismatch
      pairIndex observedArity
  case fields of
    [rawSymbol, rawValue] -> do
      symbol <- decodeSymbol pairIndex rawSymbol
      if Map.member symbol expected
        then Right ()
        else Left $ LengthSMTLibUnknownValuationSymbol pairIndex symbol
      if Map.member symbol decoded
        then Left $ LengthSMTLibDuplicateValuationSymbol pairIndex symbol
        else Right ()
      value <- decodeInteger pairIndex rawValue
      Right $ Map.insert symbol value decoded
    _ -> Left $ LengthSMTLibValuationPairArityMismatch
      pairIndex observedArity

decodeSymbol
  :: Int
  -> SMTLibSExpression
  -> Either LengthSMTLibResponseError [Word8]
decodeSymbol pairIndex expression = case expression of
  SMTLibAtomExpression atom -> case smtLibSymbolBytes atom of
    Just bytes -> Right bytes
    Nothing -> Left $ LengthSMTLibValuationTermNotSymbol pairIndex
  _ -> Left $ LengthSMTLibValuationTermNotSymbol pairIndex

decodeInteger
  :: Int
  -> SMTLibSExpression
  -> Either LengthSMTLibResponseError Integer
decodeInteger pairIndex expression = case expression of
  SMTLibAtomExpression (SMTLibNumeral value) -> Right $ toInteger value
  SMTLibListExpression
      [ SMTLibAtomExpression operator
      , SMTLibAtomExpression (SMTLibNumeral magnitude)
      ]
    | smtLibSymbolBytes operator == Just (ascii "-") ->
        if magnitude == 0
          then Left $ LengthSMTLibNegativeZeroIntegerValue pairIndex
          else Right $ negate $ toInteger magnitude
  _ -> Left $ LengthSMTLibValuationValueNotInteger pairIndex

restoreBinding
  :: Map [Word8] Integer
  -> [Word8]
  -> Either LengthSMTLibResponseError LengthSMTLibIntegerBinding
restoreBinding decoded symbol = case Map.lookup symbol decoded of
  Nothing -> Left $ LengthSMTLibMissingValuationSymbol symbol
  Just value -> Right LengthSMTLibIntegerBinding
    { lengthSMTLibIntegerBindingSymbol = symbol
    , lengthSMTLibIntegerBindingValue = value
    }

ascii :: String -> [Word8]
ascii = map $ fromIntegral . fromEnum
