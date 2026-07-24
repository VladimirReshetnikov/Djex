-- | Private Haskeline driver for stateful Djex frontends.
--
-- The historical Djinn loop fixes its prompt at initialization and has no
-- command-aware completion or multiline input. This driver preserves its good
-- interrupt/EOF semantics while making the prompt and evaluator state-aware.
module Language.Haskell.Djex.REPL.Driver
  ( ReplStep (..)
  , runReplDriver
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import System.Console.Haskeline
  ( Completion
  , InputT
  , Settings (..)
  , completeFilename
  , completeWordWithPrev
  , getHistory
  , getInputLine
  , handleInterrupt
  , outputStrLn
  , runInputT
  , simpleCompletion
  , withInterrupt
  )
import System.Console.Haskeline.History (historyLines)

import Language.Haskell.Djex.REPL.Command
  ( backendNames
  , booleanSettingNames
  , commandNames
  , settingNames
  , showNames
  )
import Language.Haskell.Djex.Text (trim)

data ReplStep state
  = ContinueRepl state
  | ExitRepl state

runReplDriver
  :: Maybe FilePath
  -> state
  -> (state -> String)
  -> (state -> [String] -> String -> IO (ReplStep state))
  -> IO state
runReplDriver historyPath initial prompt evaluate =
  runInputT settings $ loop initial
 where
  settings = Settings
    { complete = replCompletion
    , historyFile = historyPath
    , autoAddHistory = True
    }

  loop state = step state >>= \outcome -> case outcome of
    ContinueRepl next -> loop next
    ExitRepl final -> pure final

  step state = handleInterrupt interrupted $ withInterrupt $ do
    -- Snapshot before reading the next line. On a terminal, Haskeline adds
    -- that line to history inside 'getInputLine'; :history should describe
    -- commands completed before itself, especially for ':history 1'.
    history <- reverse . historyLines <$> getHistory
    input <- readLogicalInput $ prompt state
    case input of
      Nothing -> pure $ ExitRepl state
      Just source -> liftIO $ evaluate state history source
   where
    interrupted = do
      outputStrLn "Interrupted."
      pure $ ContinueRepl state

readLogicalInput :: String -> InputT IO (Maybe String)
readLogicalInput prompt = do
  input <- getInputLine prompt
  case input of
    Just source
      | trim source == ":{" -> collect []
    _ -> pure input
 where
  collect reversed = do
    next <- getInputLine "djex| "
    case next of
      Nothing -> do
        outputStrLn "unterminated multiline input (expected :})"
        pure Nothing
      Just source
        | trim source == ":}" -> pure $ Just $ unlines $ reverse reversed
        | otherwise -> collect $ source : reversed

replCompletion :: (String, String) -> IO (String, [Completion])
replCompletion input@(left, _)
  | completingPath $ reverse left = completeFilename input
  | otherwise = completeWordWithPrev Nothing " \t" completeReplWord input

completeReplWord :: String -> String -> IO [Completion]
completeReplWord reversedPrevious word = pure
  [ simpleCompletion candidate
  | candidate <- candidatesFor $ words $ reverse reversedPrevious
  , word `isPrefixOf` candidate
  ]

candidatesFor :: [String] -> [String]
candidatesFor previous = case previous of
  [] -> commandNames
  [":backend"] -> backendNames
  [":set", "backend"] -> backendNames
  [":set"] -> settingNames ++ map ('+' :) booleanSettingNames
    ++ map ('-' :) booleanSettingNames
  [":unset"] -> settingNames
  [":show"] -> showNames
  _ -> []

completingPath :: String -> Bool
completingPath source = case words source of
  command : _ -> command `elem`
    [":load", ":l", ":add", ":unadd", ":script", ":cd"]
  [] -> False
