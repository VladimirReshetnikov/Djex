{-# LANGUAGE OverloadedStrings #-}

-- | Test-only stand-in for a live Z3 worker.
--
-- Tests copy this executable under one of the closed names in 'modeFromName'.
-- The live opener may then snapshot those exact executable bytes while
-- preserving the basename.  No sibling file, inherited environment variable,
-- or working-directory entry controls behavior.
--
-- Every observed or emitted byte string is written to the output-only
-- @<executable>.events@ sidecar using a length-delimited record format.  In
-- particular, a payload may contain newlines without becoming ambiguous to
-- the test-side reader.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, forever)
import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (ord, toLower)
import Data.List (sort)
import Data.Word (Word8)
import System.Directory (getCurrentDirectory, listDirectory)
import System.Environment (getArgs, getEnvironment, getExecutablePath)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import System.FilePath (dropExtension, takeExtension, takeFileName)
import System.IO
  ( BufferMode (NoBuffering)
  , Handle
  , IOMode (WriteMode)
  , hClose
  , hFlush
  , hSetBinaryMode
  , hSetBuffering
  , stderr
  , stdin
  , stdout
  , withBinaryFile
  )

data Mode
  = Healthy
  | UnsolicitedResetSuccess
  | WrongEcho
  | WrongStatus
  | WrongValue
  | ImmediateEOF
  | ImmediateExitNonzero
  | StderrByte
  | StderrFlood
  | Hang
  | DripOutput
  | SplitOutput
  | StubbornEOF
  | StdoutEOFHang
  | UnknownMode String
  deriving (Eq)

data ProbeAssertions = ProbeAssertions
  { sawProbeZero :: Bool
  , sawProbeOne :: Bool
  }

main :: IO ()
main = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBinaryMode stderr True
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  arguments <- getArgs
  environment <- getEnvironment
  executablePath <- getExecutablePath
  workingDirectory <- getCurrentDirectory
  initialEntries <- sort <$> listDirectory workingDirectory
  let executableName = takeFileName executablePath
      mode = modeFromName executableName
      eventPath = executablePath ++ ".events"

  -- Inspect the work directory before opening the output-only sidecar, so the
  -- start record can prove whether the live opener supplied a genuinely empty
  -- directory.  The sidecar cannot configure behavior: the preserved
  -- executable basename remains the only mode selector.
  withBinaryFile eventPath WriteMode $ \trace -> do
    hSetBinaryMode trace True
    hSetBuffering trace NoBuffering
    recordEvent trace "start" $
      [ field "trace-schema" "length-delimited/v1"
      , field "mode" $ ascii $ modeName mode
      , field "executable-basename" $ utf8 executableName
      , field "argv-count" $ decimal $ length arguments
      ]
      ++ map (field "argv" . utf8) arguments
      ++ [field "environment-count" $ decimal $ length environment]
      ++ concatMap environmentFields (sort environment)
      ++ [ field "cwd" $ utf8 workingDirectory
         , field "initial-listing-count" $ decimal $ length initialEntries
         ]
      ++ map (field "initial-listing-entry" . utf8) initialEntries
    runMode trace mode

runMode :: Handle -> Mode -> IO ()
runMode trace mode = case mode of
  UnknownMode name -> do
    recordEvent trace "unknown-mode" [field "basename" $ utf8 name]
    exitWith $ ExitFailure 64
  ImmediateEOF -> do
    recordEvent trace "process-exit" [field "status" "success"]
    exitSuccess
  ImmediateExitNonzero -> do
    recordEvent trace "process-exit" [field "status" "23"]
    exitWith $ ExitFailure 23
  Hang -> do
    recordEvent trace "hang" [field "phase" "startup"]
    hangForever
  StdoutEOFHang -> do
    recordEvent trace "stdout-close" []
    hClose stdout
    recordEvent trace "hang" [field "phase" "after-stdout-eof"]
    hangForever
  StderrByte -> do
    emitStderr trace "!"
    protocolLoop trace mode emptyProbeAssertions BS.empty
  StderrFlood -> do
    -- This is deliberately larger than ordinary pipe buffers, but finite.
    emitStderr trace $ BS.replicate (256 * 1024) 88
    protocolLoop trace mode emptyProbeAssertions BS.empty
  _ -> protocolLoop trace mode emptyProbeAssertions BS.empty

protocolLoop :: Handle -> Mode -> ProbeAssertions -> ByteString -> IO ()
protocolLoop trace mode assertions buffered = do
  next <- nextProtocolLine buffered
  case next of
    Nothing -> do
      recordEvent trace "stdin-eof" []
      if mode == StubbornEOF
        then do
          recordEvent trace "hang" [field "phase" "after-stdin-eof"]
          hangForever
        else recordEvent trace "process-exit" [field "status" "success"]
    Just (input, remaining) -> do
      recordEvent trace "stdin" [field "bytes" input]
      (continue, nextAssertions) <-
        handleCommand trace mode assertions $ trimSMTLine input
      if continue
        then protocolLoop trace mode nextAssertions remaining
        else if mode == StubbornEOF
          then do
            recordEvent trace "hang" [field "phase" "after-exit-command"]
            hangForever
          else do
            recordEvent trace "process-exit" [field "status" "success"]
            exitSuccess

-- Byte-preserving incremental line input.  A returned line includes its LF;
-- an unterminated final line is returned once before the following EOF.
nextProtocolLine
  :: ByteString
  -> IO (Maybe (ByteString, ByteString))
nextProtocolLine buffered = case BS.elemIndex lineFeed buffered of
  Just index ->
    let (line, remaining) = BS.splitAt (index + 1) buffered
    in pure $ Just (line, remaining)
  Nothing -> do
    chunk <- BS.hGetSome stdin 4096
    if BS.null chunk
      then if BS.null buffered
        then pure Nothing
        else pure $ Just (buffered, BS.empty)
      else nextProtocolLine $ buffered <> chunk

handleCommand
  :: Handle
  -> Mode
  -> ProbeAssertions
  -> ByteString
  -> IO (Bool, ProbeAssertions)
handleCommand trace mode assertions command
  | command == "(exit)" = do
      recordCommand trace "exit"
      pure (False, assertions)
  | command == "(check-sat)" = do
      recordCommand trace "check-sat"
      emitResponse trace mode $ case mode of
        WrongStatus -> "unknown\n"
        _ | contradictoryProbe assertions -> "unsat\n"
        _ -> "sat\n"
      pure (True, assertions)
  | Just argument <- echoArgument command = do
      recordCommand trace "echo"
      emitResponse trace mode $ case mode of
        WrongEcho -> "\"djex-fake-wrong-echo\"\n"
        _ -> argument <> "\n"
      pure (True, assertions)
  | "(get-value " `BS.isPrefixOf` command = do
      recordCommand trace "get-value"
      emitResponse trace mode $ case mode of
        WrongValue -> "((djex_capability_input 1))\n"
        _ -> "((djex_capability_input 0))\n"
      pure (True, assertions)
  | command == "(reset)" = do
      recordCommand trace "reset"
      if mode == UnsolicitedResetSuccess
        then emitResponse trace mode "success\n"
        else pure ()
      pure (True, emptyProbeAssertions)
  | command == probeZeroAssertion = do
      recordCommand trace "probe-assert-zero"
      pure (True, assertions {sawProbeZero = True})
  | command == probeOneAssertion = do
      recordCommand trace "probe-assert-one"
      pure (True, assertions {sawProbeOne = True})
  | any (`BS.isPrefixOf` command) silentCommandPrefixes = do
      recordCommand trace "silent"
      pure (True, assertions)
  | BS.null command = do
      recordCommand trace "blank"
      pure (True, assertions)
  | otherwise = do
      -- Unknown input is deliberately silent, matching print-success=false.
      -- Its exact bytes remain available in the preceding stdin event.
      recordCommand trace "unknown-silent"
      pure (True, assertions)

emptyProbeAssertions :: ProbeAssertions
emptyProbeAssertions = ProbeAssertions False False

contradictoryProbe :: ProbeAssertions -> Bool
contradictoryProbe assertions =
  sawProbeZero assertions && sawProbeOne assertions

probeZeroAssertion :: ByteString
probeZeroAssertion = "(assert (= djex_capability_input 0))"

probeOneAssertion :: ByteString
probeOneAssertion = "(assert (= djex_capability_input 1))"

silentCommandPrefixes :: [ByteString]
silentCommandPrefixes =
  [ "(set-logic "
  , "(set-option "
  , "(set-info "
  , "(assert "
  , "(declare-const "
  , "(declare-fun "
  , "(define-fun "
  , "(push"
  , "(pop"
  ]

echoArgument :: ByteString -> Maybe ByteString
echoArgument command = do
  afterPrefix <- BS.stripPrefix "(echo " command
  if not (BS.null afterPrefix) && BS.last afterPrefix == closeParen
    then Just $ BS.init afterPrefix
    else Nothing

trimSMTLine :: ByteString -> ByteString
trimSMTLine = BS.dropWhileEnd isSMTWhitespace . BS.dropWhile isSMTWhitespace

isSMTWhitespace :: Word8 -> Bool
isSMTWhitespace byte = byte `elem` [9, 10, 13, 32]

emitResponse :: Handle -> Mode -> ByteString -> IO ()
emitResponse trace mode bytes = case mode of
  SplitOutput -> forM_ (BS.unpack bytes) $ emitStdout trace . BS.singleton
  DripOutput -> forM_ (BS.unpack bytes) $ \byte -> do
    emitStdout trace $ BS.singleton byte
    threadDelay 2000
  _ -> emitStdout trace bytes

emitStdout :: Handle -> ByteString -> IO ()
emitStdout trace bytes = do
  recordEvent trace "stdout" [field "bytes" bytes]
  BS.hPut stdout bytes
  hFlush stdout

emitStderr :: Handle -> ByteString -> IO ()
emitStderr trace bytes = do
  recordEvent trace "stderr" [field "bytes" bytes]
  BS.hPut stderr bytes
  hFlush stderr

recordCommand :: Handle -> ByteString -> IO ()
recordCommand trace command =
  recordEvent trace "command" [field "kind" command]

recordEvent
  :: Handle
  -> ByteString
  -> [(ByteString, ByteString)]
  -> IO ()
recordEvent trace tag fields = do
  BS.hPut trace "EVENT "
  BS.hPut trace tag
  BS.hPut trace " "
  BS.hPut trace $ decimal $ length fields
  BS.hPut trace "\n"
  forM_ fields $ \(name, payload) -> do
    BS.hPut trace "FIELD "
    BS.hPut trace name
    BS.hPut trace " "
    BS.hPut trace $ decimal $ BS.length payload
    BS.hPut trace "\n"
    BS.hPut trace payload
    BS.hPut trace "\n"
  BS.hPut trace "END\n"
  hFlush trace

field :: String -> ByteString -> (ByteString, ByteString)
field name value = (ascii name, value)

environmentFields :: (String, String) -> [(ByteString, ByteString)]
environmentFields (name, value) =
  [field "environment-name" $ utf8 name, field "environment-value" $ utf8 value]

modeFromName :: FilePath -> Mode
modeFromName originalName = case portableStem originalName of
  "djex-fake-z3" -> Healthy
  "djex-fake-z3-healthy" -> Healthy
  "djex-fake-z3-unsolicited-reset-success" -> UnsolicitedResetSuccess
  "djex-fake-z3-wrong-echo" -> WrongEcho
  "djex-fake-z3-wrong-status" -> WrongStatus
  "djex-fake-z3-wrong-value" -> WrongValue
  "djex-fake-z3-immediate-eof" -> ImmediateEOF
  "djex-fake-z3-immediate-exit-nonzero" -> ImmediateExitNonzero
  "djex-fake-z3-stderr-byte" -> StderrByte
  "djex-fake-z3-stderr-flood" -> StderrFlood
  "djex-fake-z3-hang" -> Hang
  "djex-fake-z3-drip-output" -> DripOutput
  "djex-fake-z3-split-output" -> SplitOutput
  "djex-fake-z3-stubborn-eof" -> StubbornEOF
  "djex-fake-z3-stdout-eof-hang" -> StdoutEOFHang
  _ -> UnknownMode originalName

portableStem :: FilePath -> FilePath
portableStem name
  | map toLower (takeExtension name) == ".exe" = dropExtension name
  | otherwise = name

modeName :: Mode -> String
modeName mode = case mode of
  Healthy -> "healthy"
  UnsolicitedResetSuccess -> "unsolicited-reset-success"
  WrongEcho -> "wrong-echo"
  WrongStatus -> "wrong-status"
  WrongValue -> "wrong-value"
  ImmediateEOF -> "immediate-eof"
  ImmediateExitNonzero -> "immediate-exit-nonzero"
  StderrByte -> "stderr-byte"
  StderrFlood -> "stderr-flood"
  Hang -> "hang"
  DripOutput -> "drip-output"
  SplitOutput -> "split-output"
  StubbornEOF -> "stubborn-eof"
  StdoutEOFHang -> "stdout-eof-hang"
  UnknownMode _ -> "unknown"

hangForever :: IO a
hangForever = forever $ threadDelay 1000000

ascii :: String -> ByteString
ascii = BSC.pack

decimal :: Show value => value -> ByteString
decimal = ascii . show

-- Avoid locale-dependent handle encodings in metadata records.
utf8 :: String -> ByteString
utf8 = BS.pack . concatMap encodeChar

encodeChar :: Char -> [Word8]
encodeChar character
  | code <= 0x7f = [byte code]
  | code <= 0x7ff =
      [ byte $ 0xc0 + shiftR code 6
      , byte $ 0x80 + (code .&. 0x3f)
      ]
  | code <= 0xffff =
      [ byte $ 0xe0 + shiftR code 12
      , byte $ 0x80 + (shiftR code 6 .&. 0x3f)
      , byte $ 0x80 + (code .&. 0x3f)
      ]
  | otherwise =
      [ byte $ 0xf0 + shiftR code 18
      , byte $ 0x80 + (shiftR code 12 .&. 0x3f)
      , byte $ 0x80 + (shiftR code 6 .&. 0x3f)
      , byte $ 0x80 + (code .&. 0x3f)
      ]
  where
    code = ord character
    byte = fromIntegral

lineFeed :: Word8
lineFeed = 10

closeParen :: Word8
closeParen = 41
