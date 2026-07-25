-- | Cabal-backed package acquisition shared by the command-line and REPL
-- frontends.
--
-- Djex deliberately invokes Cabal directly rather than through a shell. The
-- @--@ separator makes every parsed target data, never a Cabal option. Package
-- installation is intentionally independent of the current Cabal project: a
-- REPL started in a checkout must not accidentally reinterpret a repository
-- target as one of that project's local components.
module Language.Haskell.Djex.Package
  ( PackageOperation (..)
  , PackageInstallMode (..)
  , packageOperationName
  , parsePackageInstall
  , validatePackageTargets
  , runPackageOperation
  ) where

import Control.Exception
  ( AsyncException (UserInterrupt)
  , handleJust
  )
import Data.Char (isControl)
import Data.List (intercalate)
import System.Directory (findExecutable)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (hClose, hPutStrLn, stderr)
import System.IO.Error (tryIOError)
import System.Process
  ( CreateProcess
      ( close_fds
      , create_group
      , delegate_ctlc
      , std_in
      , use_process_jobs
      )
  , ProcessHandle
  , StdStream (CreatePipe)
  , interruptProcessGroupOf
  , proc
  , terminateProcess
  , waitForProcess
  , withCreateProcess
  )
import System.Timeout (timeout)

import Language.Haskell.Synthesis.Diagnostic
  ( Severity (Error)
  , contextualDiagnostic
  , renderDiagnostic
  )

-- | The two explicit package-manager effects exposed by Djex.
data PackageOperation
  = DownloadOperation
  | InstallOperation PackageInstallMode
  deriving (Eq, Show)

-- | Which Cabal installation product the caller explicitly requested.
data PackageInstallMode
  = InstallExecutables
  | InstallLibraries
  deriving (Eq, Show)

packageOperationName :: PackageOperation -> String
packageOperationName DownloadOperation = "download"
packageOperationName (InstallOperation _) = "install"

-- | Parse Djex's one controlled install option. A leading @--@ escapes it and
-- is removed before the remaining target vector reaches Cabal.
parsePackageInstall
  :: [String]
  -> Either String (PackageInstallMode, [String])
parsePackageInstall arguments = case arguments of
  "--lib" : targets -> pair InstallLibraries targets
  "--" : targets -> pair InstallExecutables targets
  targets -> pair InstallExecutables targets
 where
  pair mode targets = (\validated -> (mode, validated))
    <$> validatePackageTargets targets

-- | Check the small target invariant Djex owns and leave Cabal to interpret
-- package names, versions, component selectors, local paths and archives, or
-- archive URLs. In particular, paths containing spaces remain valid argv
-- elements. Control characters are rejected because they make terminal
-- diagnostics ambiguous, and NUL cannot be represented by an operating-system
-- process argument.
validatePackageTargets :: [String] -> Either String [String]
validatePackageTargets [] = Left "expected at least one package target"
validatePackageTargets targets = case filter invalid targets of
  [] -> Right targets
  _ -> Left "package targets must be nonempty and contain no control characters"
 where
  invalid target = null target || any isControl target

-- | Run Cabal with inherited output streams and return its ordinary exit code.
-- A user interrupt is normalized to status 130 after bounded child cleanup.
--
-- Downloading uses Cabal's configured source cache and includes dependencies.
-- Installing preserves Cabal's ordinary executable mode unless the caller
-- explicitly selects library mode. Neither operation implicitly changes the
-- loaded Djex source workspace; compiled package interfaces are outside that
-- workspace's deliberately source-only contract.
runPackageOperation :: PackageOperation -> [String] -> IO ExitCode
runPackageOperation operation targets = do
  located <- tryIOError $ findExecutable "cabal"
  case located of
    Left failure -> launchFailure $ show failure
    Right Nothing -> launchFailure "cannot find `cabal' on PATH"
    Right (Just executable) -> do
      let packageProcess = (proc executable arguments)
            { close_fds = True
            , create_group = True
            , delegate_ctlc = False
            -- Keep descriptor 0 occupied by a real pipe until exec. Merely
            -- closing it (NoStream) lets a Haskell child reuse fd 0 for an
            -- RTS timer/event descriptor, after which reading stdin can
            -- block forever instead of observing EOF.
            , std_in = CreatePipe
            , use_process_jobs = True
            }
      outcome <- tryIOError $ withCreateProcess packageProcess
        $ \input _ _ process -> do
            maybe (pure ()) hClose input
            handleJust userInterrupt
              (const $ interruptPackageProcess process)
              (PackageCompleted <$> waitForProcess process)
      case outcome of
        Left failure -> launchFailure $ show failure
        Right PackageInterrupted -> do
          emitFailure "DJEX_PACKAGE_INTERRUPTED" "package command interrupted"
            renderedCommand
          pure $ ExitFailure 130
        Right (PackageCompleted ExitSuccess) -> do
          putStrLn $ completionSummary operation ++ ": "
            ++ intercalate ", " (map show targets)
          pure ExitSuccess
        Right (PackageCompleted status@(ExitFailure code)) -> do
          emitFailure "DJEX_PACKAGE_COMMAND" "Cabal package command failed"
            $ renderedCommand ++ ": exit status " ++ show code
          pure status
 where
  arguments = cabalArguments operation targets
  renderedCommand = unwords $ "cabal" : map show arguments
  launchFailure detail = do
    emitFailure "DJEX_PACKAGE_TOOL" "cannot run Cabal package command"
      $ renderedCommand ++ ": " ++ detail
    pure $ ExitFailure 1

data PackageOutcome
  = PackageCompleted ExitCode
  | PackageInterrupted

cabalArguments :: PackageOperation -> [String] -> [String]
cabalArguments DownloadOperation targets = "fetch" : "--" : targets
cabalArguments (InstallOperation mode) targets =
  ["install"] ++ libraryOption ++ ["--ignore-project", "--"] ++ targets
 where
  libraryOption = case mode of
    InstallExecutables -> []
    InstallLibraries -> ["--lib"]

completionSummary :: PackageOperation -> String
completionSummary DownloadOperation = "Cabal fetch completed for"
completionSummary (InstallOperation _) = "Cabal install completed for"

-- Give Cabal and every ordinary descendant in its dedicated process group a
-- chance to handle SIGINT and clean up. The process library's job support
-- extends forced termination to descendants on Windows; on POSIX, processes
-- that deliberately detach from the group remain an operating-system concern.
interruptPackageProcess :: ProcessHandle -> IO PackageOutcome
interruptPackageProcess process = do
  _ <- tryIOError $ interruptProcessGroupOf process
  stopped <- timeout packageInterruptGraceMicroseconds
    $ tryIOError $ waitForProcess process
  case stopped of
    Just _ -> pure PackageInterrupted
    Nothing -> do
      _ <- tryIOError $ terminateProcess process
      _ <- tryIOError $ waitForProcess process
      pure PackageInterrupted

packageInterruptGraceMicroseconds :: Int
packageInterruptGraceMicroseconds = 2000000

emitFailure :: String -> String -> String -> IO ()
emitFailure code summary detail = hPutStrLn stderr $ renderDiagnostic
  $ contextualDiagnostic Error code summary detail

userInterrupt :: AsyncException -> Maybe ()
userInterrupt UserInterrupt = Just ()
userInterrupt _ = Nothing
