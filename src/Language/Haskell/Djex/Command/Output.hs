-- | Strict, ordered command output prepared away from the process handles.
--
-- Concurrent search workers build and fully evaluate this private plan.  Only
-- the owning frontend thread replays it, so backend completion order can never
-- reorder stdout, stderr, or flush effects.
module Language.Haskell.Djex.Command.Output
  ( CommandOutputEvent (..)
  , CommandOutput (..)
  , replayCommandOutput
  ) where

import Control.DeepSeq (NFData (rnf))
import Control.Monad (forM_)
import System.Exit (ExitCode (..))
import System.IO (hFlush, hPutStrLn, stderr, stdout)

-- | One console effect in its original program order.
data CommandOutputEvent
  = CommandStandardOutputLine String
  | CommandStandardErrorLine String
  | CommandFlushStandardOutput
  deriving (Eq, Show)

instance NFData CommandOutputEvent where
  rnf event = case event of
    CommandStandardOutputLine line -> rnf line
    CommandStandardErrorLine line -> rnf line
    CommandFlushStandardOutput -> ()

-- | A complete non-streaming presentation and its command exit status.
data CommandOutput = CommandOutput [CommandOutputEvent] ExitCode
  deriving (Eq, Show)

instance NFData CommandOutput where
  rnf (CommandOutput events exitCode) = rnf events `seq` forceExitCode exitCode

forceExitCode :: ExitCode -> ()
forceExitCode ExitSuccess = ()
forceExitCode (ExitFailure code) = rnf code

-- | Replay one fully prepared plan against the real process handles.
replayCommandOutput :: CommandOutput -> IO ExitCode
replayCommandOutput (CommandOutput events exitCode) = do
  forM_ events $ \event -> case event of
    CommandStandardOutputLine line -> putStrLn line
    CommandStandardErrorLine line -> hPutStrLn stderr line
    CommandFlushStandardOutput -> hFlush stdout
  pure exitCode
