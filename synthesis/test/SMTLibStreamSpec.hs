module SMTLibStreamSpec (smtLibStreamTests) where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.Word (Word8)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool
  , assertFailure
  , testCase
  , (@?=)
  )

import Language.Haskell.Synthesis.Internal.SMTLib.Stream

smtLibStreamTests :: TestTree
smtLibStreamTests = testGroup "bounded incremental SMT-LIB framing"
  [ sentinelTests
  , defaultLimitTests
  , chunkBoundaryTests
  , lexicalIsolationTests
  , stringTests
  , responseTerminationTests
  , lexicalFailureTests
  , boundTests
  , lazyTailTests
  ]

sentinelTests :: TestTree
sentinelTests = testGroup "exact echo sentinels"
  [ testCase "render one fixed quoted echo marker from 256 nonce bits" $ do
      smtLibStreamFramingSchemaTag @?=
        ascii "djex-smtlib2-stream-framing/v1"
      smtLibEchoSentinelNonceByteCount @?= 32
      sentinel <- expectRight $ mkSMTLibEchoSentinel [0 .. 31]
      let content =
            "djex-smtlib-frame/v1/" ++
            "000102030405060708090a0b0c0d0e0f" ++
            "101112131415161718191a1b1c1d1e1f"
      smtLibEchoSentinelCommandBytes sentinel @?=
        ascii ("(echo \"" ++ content ++ "\")\n")
      smtLibEchoSentinelResponseBytes sentinel @?=
        ascii ("\"" ++ content ++ "\"")
      assertBool "the exact quoted response was not recognized"
        $ isExactSMTLibEchoSentinelResponse sentinel
        $ smtLibEchoSentinelResponseBytes sentinel
      assertBool "a longer string was accepted as the sentinel"
        $ not $ isExactSMTLibEchoSentinelResponse sentinel
        $ smtLibEchoSentinelResponseBytes sentinel ++ ascii "x"
  , testCase "retain every nonce byte but no nonce material in errors" $ do
      left <- expectRight $ mkSMTLibEchoSentinel $ 0 : replicate 31 1
      right <- expectRight $ mkSMTLibEchoSentinel $ 1 : replicate 31 1
      assertBool "distinct nonce bytes shared a sentinel" $ left /= right
      assertLeft (SMTLibEchoSentinelNonceLengthMismatch 32 31)
        $ mkSMTLibEchoSentinel $ replicate 31 0
      assertLeft (SMTLibEchoSentinelNonceLengthMismatch 32 33)
        $ mkSMTLibEchoSentinel $ replicate 33 0
      let cyclicNonce = 0 : cyclicNonce
      cyclic <- evaluateWithin $ mkSMTLibEchoSentinel cyclicNonce
      assertLeft (SMTLibEchoSentinelNonceLengthMismatch 32 33) cyclic
  ]

defaultLimitTests :: TestTree
defaultLimitTests = testGroup "independent framing limits"
  [ testCase "publish conservative natural defaults" $ do
      mkSMTLibStreamLimits defaultSMTLibStreamLimitSource @?=
        defaultSMTLibStreamLimits
      smtLibStreamTotalByteLimit defaultSMTLibStreamLimits @?= 131072
      smtLibStreamFrameByteLimit defaultSMTLibStreamLimits @?= 65536
      smtLibStreamNestingDepthLimit defaultSMTLibStreamLimits @?= 64
  ]

chunkBoundaryTests :: TestTree
chunkBoundaryTests = testGroup "chunk-invariant transcript framing"
  [ testCase "frame status, quoted sentinel, and model at every split" $ do
      sentinel <- fixtureSentinel
      let status = ascii "sat "
          marker = smtLibEchoSentinelResponseBytes sentinel
          modelText = "((|x;()| 3) (y (- 2)))"
          transcript =
            ascii "; leading marker text is trivia\r\n" ++
            status ++ marker ++ ascii "\n" ++ ascii modelText ++
            ascii "\nnext"
          expectedFrames =
            [ascii "sat", marker, ascii modelText]
          observe chunks = collectFrames 3 defaultSMTLibStreamLimits chunks
      (wholeFrames, wholeTail) <- expectRight $ observe [transcript]
      wholeFrames @?= expectedFrames
      wholeTail @?= ascii "\nnext"
      assertBool "the middle frame was not the exact sentinel"
        $ isExactSMTLibEchoSentinelResponse sentinel
        $ expectedFrames !! 1
      singleton <- expectRight $ observe $ map (: []) transcript
      singleton @?= (expectedFrames, ascii "\nnext")
      withEmpty <- expectRight $ observe
        $ concatMap (\byte -> [[], [byte]]) transcript ++ [[]]
      withEmpty @?= singleton
      forM_ [0 .. length transcript] $ \split -> do
        observed <- expectRight $ observe
          [take split transcript, drop split transcript]
        observed @?= singleton
  ]

lexicalIsolationTests :: TestTree
lexicalIsolationTests = testGroup "sentinel lexical isolation"
  [ testCase "ignore marker frames nested in strings, symbols, and comments" $ do
      sentinel <- fixtureSentinel
      let marker = smtLibEchoSentinelResponseBytes sentinel
          markerContent = quotedContent marker
          response =
            ascii "(outer " ++ marker ++
            ascii " |" ++ markerContent ++ ascii ";()|" ++
            ascii " ; " ++ marker ++ ascii " ) ignored\r\n" ++
            ascii " (error \"line\nwith \"\"quotes\"\";()\"))"
          transcript = response ++ ascii "\n" ++ marker ++ ascii "\n"
      (frames, _) <- expectRight $ collectFrames 2
        defaultSMTLibStreamLimits $ map (: []) transcript
      frames @?= [response, marker]
      assertBool "a nested marker was accepted positionlessly"
        $ not $ isExactSMTLibEchoSentinelResponse sentinel response
      assertBool "the real top-level marker was not recognized"
        $ isExactSMTLibEchoSentinelResponse sentinel marker
  , testCase "reject prefix, suffix, quoted-symbol, and list lookalikes" $ do
      sentinel <- fixtureSentinel
      let marker = smtLibEchoSentinelResponseBytes sentinel
          content = quotedContent marker
          lookalikes =
            [ ascii "\"prefix" ++ content ++ ascii "\""
            , dropLast marker ++ ascii "suffix\""
            , ascii "|" ++ content ++ ascii "|"
            , ascii "(" ++ marker ++ ascii ")"
            ]
      forM_ lookalikes $ \lookalike -> assertBool
        "a lookalike frame was accepted as the sentinel"
        $ not $ isExactSMTLibEchoSentinelResponse sentinel lookalike
  ]

stringTests :: TestTree
stringTests = testGroup "strings and quoted symbols"
  [ testCase "frame empty, doubled-quote, multiline, and raw-backslash strings" $ do
      let fixtures = map ascii
            [ "\"\""
            , "\"\"\"\""
            , "\"a\"\"b\""
            , "\"line one\nline two\""
            , "\"a\\n\\x0A\\u0008\""
            ]
      forM_ fixtures $ \fixture -> do
        let transcript = fixture ++ ascii " next"
        forM_ [0 .. length transcript] $ \split -> do
          (frame, tailBytes) <- expectRight $ frameOne
            defaultSMTLibStreamLimits
            [take split transcript, drop split transcript]
          frame @?= fixture
          tailBytes @?= ascii " next"
  , testCase "keep delimiters inert inside a multiline quoted symbol" $ do
      let fixture = ascii "|quoted ; () \"\n symbol|"
          transcript = fixture ++ ascii "\nnext"
      (frame, tailBytes) <- expectRight $ frameOne
        defaultSMTLibStreamLimits $ map (: []) transcript
      frame @?= fixture
      tailBytes @?= ascii "\nnext"
  , testCase "let comments hide delimiters until CR or LF" $ do
      let fixture = ascii "(a ; hidden ) | \"\r\n b)"
      (frame, tailBytes) <- expectRight $ frameOne
        defaultSMTLibStreamLimits [fixture ++ ascii "tail"]
      frame @?= fixture
      tailBytes @?= ascii "tail"
  , testCase "distinguish a doubled quote from a true closing quote at EOF" $ do
      assertFinishLeft (SMTLibStreamUnterminatedString 0) $ ascii "\"\"\""
      closedInList <- expectPending $ feedFresh defaultSMTLibStreamLimits
        $ ascii "(\"x\""
      finishSMTLibStreamFramer closedInList @?=
        Left (SMTLibStreamUnterminatedList 0)
  ]

responseTerminationTests :: TestTree
responseTerminationTests = testGroup "standard response termination"
  [ testCase "require whitespace after bare and quoted-symbol responses" $ do
      pendingBare <- expectPending $ feedSMTLibStreamFramer
        (startSMTLibStreamFramer defaultSMTLibStreamLimits) $ ascii "sat"
      finishSMTLibStreamFramer pendingBare @?=
        Left (SMTLibStreamMissingWhitespaceAfterAtom 3)
      pendingQuoted <- expectPending $ feedSMTLibStreamFramer
        (startSMTLibStreamFramer defaultSMTLibStreamLimits) $ ascii "|q|"
      finishSMTLibStreamFramer pendingQuoted @?=
        Left (SMTLibStreamMissingWhitespaceAfterAtom 3)
      assertLeft (SMTLibStreamNonWhitespaceAfterAtom 3 34)
        $ feedSMTLibStreamFramer
            (startSMTLibStreamFramer defaultSMTLibStreamLimits)
            (ascii "sat\"later\"")
      (bare, bareTail) <- expectRight $ frameOne
        defaultSMTLibStreamLimits [ascii "sat next"]
      bare @?= ascii "sat"
      bareTail @?= ascii " next"
      (quoted, quotedTail) <- expectRight $ frameOne
        defaultSMTLibStreamLimits [ascii "|q|\nnext"]
      quoted @?= ascii "|q|"
      quotedTail @?= ascii "\nnext"
  , testCase "complete parenthesized and quoted responses at lexical EOF" $ do
      listState <- expectComplete $ feedSMTLibStreamFramer
        (startSMTLibStreamFramer defaultSMTLibStreamLimits) $ ascii "(sat)"
      listState @?= (ascii "(sat)", [])
      stringState <- expectPending $ feedSMTLibStreamFramer
        (startSMTLibStreamFramer defaultSMTLibStreamLimits) $ ascii "\"done\""
      finishSMTLibStreamFramer stringState @?=
        Right (Just $ ascii "\"done\"")
      commentState <- expectPending $ feedSMTLibStreamFramer
        (startSMTLibStreamFramer defaultSMTLibStreamLimits)
        $ ascii "; no response"
      finishSMTLibStreamFramer commentState @?= Right Nothing
  ]

lexicalFailureTests :: TestTree
lexicalFailureTests = testGroup "structural failure offsets"
  [ testCase "reject unmatched and unterminated structures exactly" $ do
      assertLeft (SMTLibStreamUnexpectedClosingParenthesis 2)
        $ feedFresh defaultSMTLibStreamLimits $ ascii " \n)"
      let noFrame = limits $ \source -> source
            { smtLibStreamLimitSourceFrameBytes = 0 }
      assertLeft (SMTLibStreamFrameByteLimitExceeded 0 1)
        $ feedFresh noFrame $ ascii ")"
      assertFinishLeft (SMTLibStreamUnterminatedList 3) $ ascii "(a (b"
      assertFinishLeft (SMTLibStreamUnterminatedString 1) [40, 34, 97]
      assertFinishLeft (SMTLibStreamUnterminatedQuotedSymbol 1)
        $ ascii "(|a"
      commentInList <- expectPending $ feedFresh
        defaultSMTLibStreamLimits $ ascii "(; hidden"
      finishSMTLibStreamFramer commentInList @?=
        Left (SMTLibStreamUnterminatedList 0)
  , testCase "reject forbidden bytes before semantic parsing" $ do
      assertLeft (SMTLibStreamInvalidBareByte 0 128)
        $ feedFresh defaultSMTLibStreamLimits [128, 32]
      assertLeft (SMTLibStreamInvalidStringByte 2 0)
        $ feedFresh defaultSMTLibStreamLimits [34, 97, 0]
      assertLeft (SMTLibStreamInvalidQuotedSymbolByte 2 92)
        $ feedFresh defaultSMTLibStreamLimits $ ascii "|a\\b| "
      comment <- expectPending $ feedFresh defaultSMTLibStreamLimits
        [59, 0, 10]
      finishSMTLibStreamFramer comment @?= Right Nothing
  ]

boundTests :: TestTree
boundTests = testGroup "productive resource bounds"
  [ testCase "charge total bytes before retained frame bytes" $ do
      let zero = limits $ \source -> source
            { smtLibStreamLimitSourceTotalBytes = 0
            , smtLibStreamLimitSourceFrameBytes = 0
            }
      assertLeft (SMTLibStreamTotalByteLimitExceeded 0 1)
        $ feedFresh zero $ ascii "("
      let totalTwo = limits $ \source -> source
            { smtLibStreamLimitSourceTotalBytes = 2 }
      assertLeft (SMTLibStreamTotalByteLimitExceeded 2 3)
        $ feedFresh totalTwo $ ascii "sat "
      let totalThree = limits $ \source -> source
            { smtLibStreamLimitSourceTotalBytes = 3 }
      completed <- expectComplete $ feedFresh totalThree $ ascii "sat "
      completed @?= (ascii "sat", ascii " ")
  , testCase "enforce frame bytes and depth at exact boundaries" $ do
      let frameTwo = limits $ \source -> source
            { smtLibStreamLimitSourceFrameBytes = 2 }
      assertLeft (SMTLibStreamFrameByteLimitExceeded 2 3)
        $ feedFresh frameTwo $ ascii "sat "
      let frameThree = limits $ \source -> source
            { smtLibStreamLimitSourceFrameBytes = 3 }
      completed <- expectComplete $ feedFresh frameThree $ ascii "sat "
      completed @?= (ascii "sat", ascii " ")
      let depthOne = limits $ \source -> source
            { smtLibStreamLimitSourceNestingDepth = 1 }
      assertLeft (SMTLibStreamNestingDepthLimitExceeded 1 2)
        $ feedFresh depthOne $ ascii "(("
      exactDepth <- expectComplete $ feedFresh depthOne $ ascii "()"
      exactDepth @?= (ascii "()", [])
  , testCase "stop cyclic trivia, comments, and atoms at max plus one" $ do
      let totalThree = limits $ \source -> source
            { smtLibStreamLimitSourceTotalBytes = 3 }
          cyclicWhitespace = 32 : cyclicWhitespace
          cyclicComment = 59 : cyclicComment
      whitespace <- evaluateWithin $ feedFresh totalThree cyclicWhitespace
      assertLeft (SMTLibStreamTotalByteLimitExceeded 3 4) whitespace
      comment <- evaluateWithin $ feedFresh totalThree cyclicComment
      assertLeft (SMTLibStreamTotalByteLimitExceeded 3 4) comment
      let frameThree = limits $ \source -> source
            { smtLibStreamLimitSourceTotalBytes = 100
            , smtLibStreamLimitSourceFrameBytes = 3
            }
          cyclicAtom = 97 : cyclicAtom
      atom <- evaluateWithin $ feedFresh frameThree cyclicAtom
      assertLeft (SMTLibStreamFrameByteLimitExceeded 3 4) atom
      let cyclicString = 34 : repeat 97
          cyclicQuoted = 124 : repeat 97
          cyclicList = 40 : repeat 97
      stringResult <- evaluateWithin $ feedFresh totalThree cyclicString
      assertLeft (SMTLibStreamTotalByteLimitExceeded 3 4) stringResult
      quotedResult <- evaluateWithin $ feedFresh totalThree cyclicQuoted
      assertLeft (SMTLibStreamTotalByteLimitExceeded 3 4) quotedResult
      listResult <- evaluateWithin $ feedFresh totalThree cyclicList
      assertLeft (SMTLibStreamTotalByteLimitExceeded 3 4) listResult
  , testCase "track a wide allowed nesting stack in linear work" $ do
      let depth = 2000
          configured = mkSMTLibStreamLimits SMTLibStreamLimitSource
            { smtLibStreamLimitSourceTotalBytes = 2 * depth
            , smtLibStreamLimitSourceFrameBytes = 2 * depth
            , smtLibStreamLimitSourceNestingDepth = depth
            }
          nested = replicate (fromIntegral depth) 40 ++
            replicate (fromIntegral depth) 41
          observed = case feedFresh configured nested of
            Right (SMTLibStreamFramingComplete frame _) -> length frame
            _ -> -1
      frameLength <- evaluateWithin observed
      frameLength @?= fromIntegral (2 * depth)
  ]

lazyTailTests :: TestTree
lazyTailTests = testGroup "untouched post-frame tails"
  [ testCase "do not force a poison tail after a list or bare terminator" $ do
      listObserved <- evaluateWithin $ case feedFresh
          defaultSMTLibStreamLimits
          (ascii "(x)" ++ error "list tail forced") of
        Right (SMTLibStreamFramingComplete frame _) -> frame == ascii "(x)"
        _ -> False
      assertBool "list completion forced or lost its frame" listObserved
      bareObserved <- evaluateWithin $ case feedFresh
          defaultSMTLibStreamLimits
          (ascii "sat " ++ error "bare tail forced") of
        Right (SMTLibStreamFramingComplete frame _) -> frame == ascii "sat"
        _ -> False
      assertBool "bare completion forced or lost its frame" bareObserved
      stringObserved <- evaluateWithin $ case feedFresh
          defaultSMTLibStreamLimits
          (ascii "\"x\" " ++ error "string tail forced") of
        Right (SMTLibStreamFramingComplete frame _) ->
          frame == ascii "\"x\""
        _ -> False
      assertBool "string completion forced or lost its frame" stringObserved
      quotedObserved <- evaluateWithin $ case feedFresh
          defaultSMTLibStreamLimits
          (ascii "|x| " ++ error "quoted tail forced") of
        Right (SMTLibStreamFramingComplete frame _) -> frame == ascii "|x|"
        _ -> False
      assertBool "quoted completion forced or lost its frame" quotedObserved
  , testCase "count only bytes consumed before the pending tail" $ do
      pending <- expectPending $ feedFresh defaultSMTLibStreamLimits
        $ ascii " ;x\n"
      smtLibStreamFramerConsumedBytes pending @?= 4
  ]

fixtureSentinel :: IO SMTLibEchoSentinel
fixtureSentinel = expectRight $ mkSMTLibEchoSentinel [0 .. 31]

limits
  :: (SMTLibStreamLimitSource -> SMTLibStreamLimitSource)
  -> SMTLibStreamLimits
limits change = mkSMTLibStreamLimits $ change defaultSMTLibStreamLimitSource

feedFresh
  :: SMTLibStreamLimits
  -> [Word8]
  -> Either SMTLibStreamFramingError SMTLibStreamFramingStep
feedFresh configured =
  feedSMTLibStreamFramer $ startSMTLibStreamFramer configured

frameOne
  :: SMTLibStreamLimits
  -> [[Word8]]
  -> Either SMTLibStreamFramingError ([Word8], [Word8])
frameOne configured = go $ startSMTLibStreamFramer configured
 where
  go framer chunks = case chunks of
    [] -> do
      finished <- finishSMTLibStreamFramer framer
      case finished of
        Just frame -> Right (frame, [])
        Nothing -> error "frameOne reached clean EOF without a frame"
    chunk : remaining -> do
      step <- feedSMTLibStreamFramer framer chunk
      case step of
        SMTLibStreamFramingPending next -> go next remaining
        SMTLibStreamFramingComplete frame tailBytes ->
          Right (frame, tailBytes ++ concat remaining)

collectFrames
  :: Int
  -> SMTLibStreamLimits
  -> [[Word8]]
  -> Either SMTLibStreamFramingError ([[Word8]], [Word8])
collectFrames expected configured =
  go expected (startSMTLibStreamFramer configured) []
 where
  go remaining framer reversedFrames chunks
    | remaining == 0 = Right (reverse reversedFrames, concat chunks)
    | otherwise = case chunks of
        [] -> do
          finished <- finishSMTLibStreamFramer framer
          case finished of
            Just frame -> go (remaining - 1)
              (startSMTLibStreamFramer configured)
              (frame : reversedFrames) []
            Nothing -> error "collectFrames reached clean EOF too early"
        chunk : later -> do
          step <- feedSMTLibStreamFramer framer chunk
          case step of
            SMTLibStreamFramingPending next ->
              go remaining next reversedFrames later
            SMTLibStreamFramingComplete frame tailBytes ->
              go (remaining - 1)
                (startSMTLibStreamFramer configured)
                (frame : reversedFrames)
                (tailBytes : later)

assertFinishLeft :: SMTLibStreamFramingError -> [Word8] -> IO ()
assertFinishLeft expected bytes = do
  pending <- expectPending $ feedFresh defaultSMTLibStreamLimits bytes
  finishSMTLibStreamFramer pending @?= Left expected

expectPending
  :: Either SMTLibStreamFramingError SMTLibStreamFramingStep
  -> IO SMTLibStreamFramer
expectPending result = case result of
  Left failure -> assertFailure ("unexpected framing rejection: " ++ show failure)
  Right (SMTLibStreamFramingPending framer) -> pure framer
  Right (SMTLibStreamFramingComplete _ _) ->
    assertFailure "expected an incomplete frame"

expectComplete
  :: Either SMTLibStreamFramingError SMTLibStreamFramingStep
  -> IO ([Word8], [Word8])
expectComplete result = case result of
  Left failure -> assertFailure ("unexpected framing rejection: " ++ show failure)
  Right (SMTLibStreamFramingComplete frame tailBytes) ->
    pure (frame, tailBytes)
  Right (SMTLibStreamFramingPending _) ->
    assertFailure "expected one complete frame"

assertLeft
  :: (Eq error, Show error)
  => error
  -> Either error value
  -> IO ()
assertLeft expected result = case result of
  Left observed -> observed @?= expected
  Right _ -> assertFailure $ "expected rejection: " ++ show expected

expectRight :: Show error => Either error value -> IO value
expectRight result = case result of
  Left failure -> assertFailure $ "unexpected rejection: " ++ show failure
  Right value -> pure value

evaluateWithin :: value -> IO value
evaluateWithin value = do
  observed <- timeout 2000000 $ evaluate value
  case observed of
    Nothing -> assertFailure "bounded stream operation did not terminate"
    Just result -> pure result

ascii :: String -> [Word8]
ascii = map $ fromIntegral . fromEnum

quotedContent :: [byte] -> [byte]
quotedContent = dropLast . drop 1

dropLast :: [value] -> [value]
dropLast = reverse . drop 1 . reverse
