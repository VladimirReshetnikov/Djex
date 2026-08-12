{-# LANGUAGE DeriveGeneric #-}

-- | Domain-neutral classification of standard SMT-LIB command responses.
--
-- This layer bounds and consumes exactly one response slice.  It grants no
-- process, query, or semantic authority: a decoded status remains a raw
-- solver report, while command-specific callers retain association and
-- evidence obligations.  It deliberately owns no schema tag; a change to its
-- accepted bytes or classification requires every consuming domain to revise
-- the corresponding response/plan schema identity.
module Language.Haskell.Synthesis.Internal.SMTLib.Response.Standard
  ( SMTLibStandardResponseFailure (..)
  , classifySMTLibStandardResponseFailure
  , SMTLibCheckResponseError (..)
  , parseSMTLibCheckResponse
  , smtLibSolverStatusResponseBytes
  ) where

import Control.DeepSeq (NFData)
import Data.Word (Word8)
import GHC.Generics (Generic)

import Language.Haskell.Synthesis.Internal.SMTLib.Response
  ( SMTLibAtom (..)
  , SMTLibParseError
  , SMTLibResponseLimits
  , SMTLibSExpression (..)
  , parseSMTLibSExpression
  )
import Language.Haskell.Synthesis.Semantic.Observation
  ( SolverStatus (..) )

-- | Standard solver failures which can answer multiple SMT-LIB commands.
-- Error message bytes have already passed the parser's response and token
-- bounds.  The message remains lazy so classification does not inspect it.
data SMTLibStandardResponseFailure
  = SMTLibStandardUnsupportedResponse
  | SMTLibStandardSolverErrorResponse [Word8]
  deriving (Eq, Ord, Show, Generic)

instance NFData SMTLibStandardResponseFailure

-- | Recognize standard command-independent failures from one parsed response.
classifySMTLibStandardResponseFailure
  :: SMTLibSExpression
  -> Maybe SMTLibStandardResponseFailure
classifySMTLibStandardResponseFailure expression = case expression of
  SMTLibAtomExpression (SMTLibSimpleSymbol token)
    | token == ascii "unsupported" ->
        Just SMTLibStandardUnsupportedResponse
  SMTLibListExpression
      [ SMTLibAtomExpression (SMTLibSimpleSymbol name)
      , SMTLibAtomExpression (SMTLibString message)
      ]
    | name == ascii "error" ->
        Just $ SMTLibStandardSolverErrorResponse message
  _ -> Nothing

-- | Closed failure vocabulary for one bounded @check-sat@ response.
data SMTLibCheckResponseError
  = SMTLibCheckResponseSyntaxError !SMTLibParseError
  | SMTLibCheckResponseStandardFailure !SMTLibStandardResponseFailure
  | SMTLibCheckResponseSuccessWhereStatusExpected
  | SMTLibCheckResponseUnexpected
  deriving (Eq, Ord, Show, Generic)

instance NFData SMTLibCheckResponseError

-- | Parse and classify exactly one standard @check-sat@ response.
parseSMTLibCheckResponse
  :: SMTLibResponseLimits
  -> [Word8]
  -> Either SMTLibCheckResponseError SolverStatus
parseSMTLibCheckResponse limits bytes = do
  expression <- case parseSMTLibSExpression limits bytes of
    Left failure -> Left $ SMTLibCheckResponseSyntaxError failure
    Right value -> Right value
  case expression of
    SMTLibAtomExpression (SMTLibSimpleSymbol token)
      | token == smtLibSolverStatusResponseBytes SolverSatisfiable ->
          Right SolverSatisfiable
      | token == smtLibSolverStatusResponseBytes SolverUnsatisfiable ->
          Right SolverUnsatisfiable
      | token == smtLibSolverStatusResponseBytes SolverUnknown ->
          Right SolverUnknown
      | token == ascii "success" ->
          Left SMTLibCheckResponseSuccessWhereStatusExpected
    _ -> case classifySMTLibStandardResponseFailure expression of
      Just failure -> Left $ SMTLibCheckResponseStandardFailure failure
      Nothing -> Left SMTLibCheckResponseUnexpected

-- | Canonical exact simple-symbol bytes for one solver status.
smtLibSolverStatusResponseBytes :: SolverStatus -> [Word8]
smtLibSolverStatusResponseBytes status = ascii $ case status of
  SolverSatisfiable -> "sat"
  SolverUnsatisfiable -> "unsat"
  SolverUnknown -> "unknown"

ascii :: String -> [Word8]
ascii = map $ fromIntegral . fromEnum
