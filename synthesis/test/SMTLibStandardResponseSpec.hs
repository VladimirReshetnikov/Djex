module SMTLibStandardResponseSpec (smtLibStandardResponseTests) where

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Data.Word (Word8)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Language.Haskell.Synthesis.Internal.SMTLib.Response
  ( SMTLibAtom (..)
  , SMTLibParseError (..)
  , SMTLibResponseLimitSource (..)
  , SMTLibResponseLimits
  , SMTLibSExpression (..)
  , defaultSMTLibResponseLimitSource
  , defaultSMTLibResponseLimits
  , mkSMTLibResponseLimits
  )
import Language.Haskell.Synthesis.Internal.SMTLib.Response.Standard
import Language.Haskell.Synthesis.Semantic.Observation
  ( SolverStatus (..) )

smtLibStandardResponseTests :: TestTree
smtLibStandardResponseTests = testGroup "standard SMT-LIB responses"
  [ testCase "own canonical status bytes and bounded status decoding" $ do
      map smtLibSolverStatusResponseBytes
          [SolverSatisfiable, SolverUnsatisfiable, SolverUnknown] @?=
        map ascii ["sat", "unsat", "unknown"]
      parse (ascii "sat") @?= Right SolverSatisfiable
      parse (ascii " ; before\nunsat ; after\r\n") @?=
        Right SolverUnsatisfiable
      parse (ascii "\tunknown\r") @?= Right SolverUnknown
  , testCase "classify standard command failures without inspecting payload" $ do
      parse (ascii "unsupported") @?= Left
        (SMTLibCheckResponseStandardFailure
          SMTLibStandardUnsupportedResponse)
      parse (ascii "(error \"a\"\"b\\c\")") @?= Left
        (SMTLibCheckResponseStandardFailure
          $ SMTLibStandardSolverErrorResponse $ ascii "a\"b\\c")
      parse (ascii "(error \"" ++ [128] ++ ascii "\")") @?= Left
        (SMTLibCheckResponseStandardFailure
          $ SMTLibStandardSolverErrorResponse [128])
      _ <- evaluate $ case classifySMTLibStandardResponseFailure
          (SMTLibListExpression
            [ SMTLibAtomExpression $ SMTLibSimpleSymbol $ ascii "error"
            , SMTLibAtomExpression $ SMTLibString
                $ error "standard error payload forced"
            ]) of
        Just (SMTLibStandardSolverErrorResponse _) -> ()
        _ -> error "standard solver error was not classified"
      pure ()
  , testCase "distinguish success and unexpected status shapes" $ do
      parse (ascii "success") @?=
        Left SMTLibCheckResponseSuccessWhereStatusExpected
      map (parse . ascii) ["satjunk", "|sat|", "(sat)"] @?=
        replicate 3 (Left SMTLibCheckResponseUnexpected)
      parse (ascii "(error bad)") @?=
        Left SMTLibCheckResponseUnexpected
  , testCase "retain parser precedence and productive total-byte bounds" $ do
      parse (ascii "sat unsat") @?= Left
        (SMTLibCheckResponseSyntaxError $ SMTLibTrailingExpression 4)
      let zero = mkSMTLibResponseLimits
            defaultSMTLibResponseLimitSource
              { smtLibResponseLimitSourceBytes = 0 }
      parseWith zero (ascii "sat") @?= Left
        (SMTLibCheckResponseSyntaxError
          $ SMTLibResponseByteLimitExceeded 0 1)
      let three = mkSMTLibResponseLimits
            defaultSMTLibResponseLimitSource
              { smtLibResponseLimitSourceBytes = 3 }
          cyclic = 115 : cyclic
      cyclicResult <- timeout 1000000 $ evaluate $ parseWith three cyclic
      cyclicResult @?= Just
        (Left $ SMTLibCheckResponseSyntaxError
          $ SMTLibResponseByteLimitExceeded 3 4)
      rnf
          [ SMTLibCheckResponseSyntaxError SMTLibEmptyResponse
          , SMTLibCheckResponseStandardFailure
              SMTLibStandardUnsupportedResponse
          , SMTLibCheckResponseStandardFailure
              $ SMTLibStandardSolverErrorResponse $ ascii "bounded"
          , SMTLibCheckResponseSuccessWhereStatusExpected
          , SMTLibCheckResponseUnexpected
          ] @?= ()
  ]
 where
  parse = parseWith defaultSMTLibResponseLimits

parseWith
  :: SMTLibResponseLimits
  -> [Word8]
  -> Either SMTLibCheckResponseError SolverStatus
parseWith = parseSMTLibCheckResponse

ascii :: String -> [Word8]
ascii = map $ fromIntegral . fromEnum
