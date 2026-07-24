-- | Test-only stand-in for the real Cabal executable.
--
-- The CLI suites copy this compiled binary onto a temporary @PATH@ as
-- @cabal@ (with the platform's executable extension), so package tests can
-- observe the exact argv Djex sends on every platform; a POSIX shell script
-- cannot be launched on Windows. Behavior is driven by data, never argv:
-- each argv element is appended to the file named by @DJEX_FAKE_CABAL_LOG@,
-- and sibling files written by the test select the exit status
-- (@cabal-status@) or the stream-observing mode (@cabal-mode@ = @io@).
module Main (main) where

import Control.Exception (IOException, try)
import System.Directory (doesFileExist)
import System.Environment (getArgs, getExecutablePath, lookupEnv)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, isEOF, stderr)
import Text.Read (readMaybe)

main :: IO ()
main = do
  arguments <- getArgs
  logPath <- lookupEnv "DJEX_FAKE_CABAL_LOG"
  mapM_ (appendCall arguments) logPath
  binDirectory <- takeDirectory <$> getExecutablePath
  mode <- readConfiguration binDirectory "cabal-mode"
  case mode of
    Just "io" -> observeStreams
    _ -> pure ()
  status <- readConfiguration binDirectory "cabal-status"
  case status >>= readMaybe of
    Just code | code /= (0 :: Int) -> exitWith $ ExitFailure code
    _ -> exitSuccess

appendCall :: [String] -> FilePath -> IO ()
appendCall arguments logPath = appendFile logPath
  $ unlines $ "CALL" : map ("ARG:" ++) arguments

-- The parent launches Cabal with no standard input, so reading must reach
-- end-of-file (or fail outright on a closed handle) without ever seeing data.
observeStreams :: IO ()
observeStreams = do
  atEnd <- either (const True :: IOException -> Bool) id <$> try isEOF
  if atEnd
    then putStrLn "FAKE_STDIN_EOF"
    else do
      line <- getLine
      putStrLn $ "FAKE_STDIN_DATA:" ++ line
  putStrLn "FAKE_STDOUT_MARKER"
  hPutStrLn stderr "FAKE_STDERR_MARKER"

readConfiguration :: FilePath -> FilePath -> IO (Maybe String)
readConfiguration directory name = do
  let path = directory </> name
  present <- doesFileExist path
  if present
    then Just . takeWhile (`notElem` "\r\n") <$> readFile path
    else pure Nothing
