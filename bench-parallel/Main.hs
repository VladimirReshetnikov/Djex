module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (bracket, evaluate)
import Control.Monad (unless)
import Data.Char (ord)
import Data.List (sort)
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory
  ( createDirectory
  , findExecutable
  , getTemporaryDirectory
  , removeFile
  , removePathForcibly
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- This end-to-end corpus deliberately balances one exhaustive Djinn proof
-- search against one bounded negative Exference frontier. It includes REPL
-- setup equally in both measurements and checks exact transcript equality
-- before and during the fixed alternating sample sequence.
main :: IO ()
main = withEmptyEnvironment $ \environment -> do
  executable <- findExecutable "djex" >>= maybe
    (fail "cannot locate the djex benchmark build tool") pure
  capabilities <- benchmarkCapabilities
  let run jobs = runComparison executable environment capabilities jobs
  serial <- run 1
  parallel <- run 2
  requireMeasuredPair serial parallel
  sampleCount <- benchmarkSamples
  (serialSamples, parallelSamples) <- measurePairs sampleCount run
  let serialMedian = percentile 0.5 serialSamples
      parallelMedian = percentile 0.5 parallelSamples
      serialP95 = percentile 0.95 serialSamples
      parallelP95 = percentile 0.95 parallelSamples
      speedup = serialMedian / parallelMedian
  putStrLn "Djex paired-backend benchmark"
  putStrLn "  workload: Peirce tower 10; Exference max-steps 64"
  putStrLn $ "  RTS capabilities: " ++ show capabilities
  putStrLn $ "  measured pairs: " ++ show sampleCount
  printf "  serial jobs=1:   median %.3fs, p95 %.3fs\n"
    serialMedian serialP95
  printf "  parallel jobs=2: median %.3fs, p95 %.3fs\n"
    parallelMedian parallelP95
  printf "  median speedup:  %.3fx\n" speedup

runComparison
  :: FilePath
  -> FilePath
  -> Int
  -> Int
  -> IO (ExitCode, String, String)
runComparison executable environment capabilities jobs = do
  transcript <- readProcessWithExitCode executable
    [ "repl"
    , "--environment", environment
    , "--ignore-startup"
    , "+RTS", "-N" ++ show capabilities, "-RTS"
    ] $ unlines
      [ ":set prompt \"\""
      , ":set render expression"
      , ":set qualification none"
      , ":set max-steps 64"
      , ":set jobs " ++ show jobs
      , ":compare " ++ peirceTower 10
      , ":quit"
      ]
  forceTranscript transcript

forceTranscript
  :: (ExitCode, String, String)
  -> IO (ExitCode, String, String)
forceTranscript transcript@(_, output, errors) = do
  _ <- evaluate $ force output
  _ <- evaluate $ force errors
  pure transcript

digest :: (ExitCode, String, String) -> (Int, Int, Int, Int, Int)
digest (exitCode, output, errors) =
  ( exitDigest exitCode
  , length output
  , foldl (\total character -> total + ord character) 0 output
  , length errors
  , foldl (\total character -> total + ord character) 0 errors
  )

exitDigest :: ExitCode -> Int
exitDigest ExitSuccess = 0
exitDigest (ExitFailure code) = code

peirceTower :: Int -> String
peirceTower depth = foldl layer "a" [1 .. depth]
 where
  layer inner index = "((" ++ inner ++ " -> b" ++ show index ++ ") -> "
    ++ inner ++ ") -> " ++ inner

benchmarkCapabilities :: IO Int
benchmarkCapabilities = do
  configured <- lookupEnv "DJEX_PARALLEL_BENCH_CAPABILITIES"
  case configured of
    Nothing -> pure 2
    Just source -> case readMaybe source of
      Just value | value > 0 -> pure value
      _ -> fail "DJEX_PARALLEL_BENCH_CAPABILITIES must be a positive integer"

benchmarkSamples :: IO Int
benchmarkSamples = do
  configured <- lookupEnv "DJEX_PARALLEL_BENCH_SAMPLES"
  case configured of
    Nothing -> pure 5
    Just source -> case readMaybe source of
      Just value | value > 0 -> pure value
      _ -> fail "DJEX_PARALLEL_BENCH_SAMPLES must be a positive integer"

measurePairs
  :: Int
  -> (Int -> IO (ExitCode, String, String))
  -> IO ([Double], [Double])
measurePairs sampleCount run = go 1 [] []
 where
  go index serialSamples parallelSamples
    | index > sampleCount = pure
        (reverse serialSamples, reverse parallelSamples)
    | odd index = do
        (serialTime, serial) <- timed $ run 1
        (parallelTime, parallel) <- timed $ run 2
        requireMeasuredPair serial parallel
        go (index + 1)
          (serialTime : serialSamples) (parallelTime : parallelSamples)
    | otherwise = do
        (parallelTime, parallel) <- timed $ run 2
        (serialTime, serial) <- timed $ run 1
        requireMeasuredPair serial parallel
        go (index + 1)
          (serialTime : serialSamples) (parallelTime : parallelSamples)

timed :: IO value -> IO (Double, value)
timed action = do
  started <- getMonotonicTimeNSec
  value <- action
  finished <- getMonotonicTimeNSec
  pure (fromIntegral (finished - started) / 1000000000, value)

requireEqualTranscripts
  :: (ExitCode, String, String)
  -> (ExitCode, String, String)
  -> IO ()
requireEqualTranscripts serial parallel = unless (serial == parallel) $
  fail $ "jobs=1 and jobs=2 produced different benchmark transcripts: "
    ++ show (digest serial) ++ " /= " ++ show (digest parallel)

requireMeasuredPair
  :: (ExitCode, String, String)
  -> (ExitCode, String, String)
  -> IO ()
requireMeasuredPair serial parallel = do
  requireSuccessfulTranscript serial
  requireSuccessfulTranscript parallel
  requireEqualTranscripts serial parallel

requireSuccessfulTranscript :: (ExitCode, String, String) -> IO ()
requireSuccessfulTranscript transcript@(exitCode, _, _) =
  unless (exitCode == ExitSuccess) $
    fail $ "benchmark command failed: " ++ show (digest transcript)

percentile :: Double -> [Double] -> Double
percentile fraction samples = ordered !! index
 where
  ordered = sort samples
  index = min (length ordered - 1)
    $ max 0 $ ceiling (fraction * fromIntegral (length ordered)) - 1

withEmptyEnvironment :: (FilePath -> IO result) -> IO result
withEmptyEnvironment action = do
  temporary <- getTemporaryDirectory
  bracket (makeDirectory temporary) removePathForcibly action
 where
  makeDirectory temporary = do
    (path, handle) <- openTempFile temporary "djex-parallel-bench"
    hClose handle
    removeFile path
    createDirectory path
    pure path
