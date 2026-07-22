-- | Cabal-backed package acquisition shared by the command-line and REPL
-- frontends.
--
-- Djex deliberately invokes Cabal directly rather than through a shell.  The
-- @--@ separator also makes every user-supplied value a target, never a Cabal
-- option.  Package installation is intentionally independent of the current
-- Cabal project: a REPL started in a checkout must not accidentally reinterpret
-- a Hackage package name as one of that project's local components.
module Language.Haskell.Djex.Package
  ( PackageOperation (..)
  , packageOperationName
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
import System.IO (hPutStrLn, stderr)
import System.IO.Error (tryIOError)
import System.Process
  ( CreateProcess (delegate_ctlc)
  , proc
  , waitForProcess
  , withCreateProcess
  )

import Language.Haskell.Synthesis.Diagnostic
  ( Severity (Error)
  , contextualDiagnostic
  , renderDiagnostic
  )

-- | The two explicit package-manager effects exposed by Djex.
data PackageOperation
  = DownloadOperation
  | InstallOperation
  deriving (Eq, Show)

packageOperationName :: PackageOperation -> String
packageOperationName DownloadOperation = "download"
packageOperationName InstallOperation = "install"

-- | Check the small target invariant Djex owns and leave Cabal to interpret
-- package names, versions, component selectors, and local paths.  In
-- particular, paths containing spaces remain valid argv elements.  Control
-- characters are rejected because they make terminal diagnostics ambiguous
-- and NUL cannot be represented by an operating-system process argument.
validatePackageTargets :: [String] -> Either String [String]
validatePackageTargets [] = Left "expected at least one package target"
validatePackageTargets targets = case filter invalid targets of
  [] -> Right targets
  _ -> Left "package targets must be nonempty and contain no control characters"
 where
  invalid target = null target || any isControl target

-- | Run Cabal with inherited standard streams and return its exact exit code.
--
-- Downloading uses Cabal's configured source cache and includes dependencies.
-- Installing explicitly selects Cabal's library mode rather than guessing at
-- package executables.  Neither operation implicitly changes the loaded Djex
-- source workspace; compiled package interfaces are outside that workspace's
-- deliberately source-only contract.
runPackageOperation :: PackageOperation -> [String] -> IO ExitCode
runPackageOperation operation targets = do
  located <- tryIOError $ findExecutable "cabal"
  case located of
    Left failure -> launchFailure $ show failure
    Right Nothing -> launchFailure "cannot find `cabal' on PATH"
    Right (Just executable) -> do
      outcome <- tryIOError $ handleJust userInterrupt
        (const $ pure PackageInterrupted)
        $ withCreateProcess
            ((proc executable arguments) {delegate_ctlc = True})
            $ \_ _ _ process -> PackageCompleted <$> waitForProcess process
      case outcome of
        Left failure -> launchFailure $ show failure
        Right PackageInterrupted -> do
          emitFailure "DJEX_PACKAGE_INTERRUPTED" "package command interrupted"
            renderedCommand
          pure $ ExitFailure 130
        Right (PackageCompleted ExitSuccess) -> do
          putStrLn $ pastTense operation ++ " through Cabal: "
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
cabalArguments DownloadOperation = ("fetch" :) . ("--" :)
cabalArguments InstallOperation =
  ("install" :) . ("--lib" :) . ("--ignore-project" :) . ("--" :)

pastTense :: PackageOperation -> String
pastTense DownloadOperation = "Downloaded"
pastTense InstallOperation = "Installed"

emitFailure :: String -> String -> String -> IO ()
emitFailure code summary detail = hPutStrLn stderr $ renderDiagnostic
  $ contextualDiagnostic Error code summary detail

userInterrupt :: AsyncException -> Maybe ()
userInterrupt UserInterrupt = Just ()
userInterrupt _ = Nothing
