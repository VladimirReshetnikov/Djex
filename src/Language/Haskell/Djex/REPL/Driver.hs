-- | Private Haskeline driver for stateful Djex frontends.
--
-- The historical Djinn loop fixes its prompt at initialization and has no
-- command-aware completion or multiline input. This driver preserves its good
-- interrupt/EOF semantics while making the prompt, completion, and evaluator
-- state-aware: completion candidates follow the current workspace scope
-- through a mutable snapshot refreshed before every prompt.
module Language.Haskell.Djex.REPL.Driver
  ( ReplCompletions (..)
  , ReplStep (..)
  , runReplDriver
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
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

-- | Scope-derived completion candidates projected from the frontend state.
data ReplCompletions = ReplCompletions
  { completionModules :: [String]
    -- ^ Loaded module names, offered after module-oriented commands.
  , completionIdentifiers :: [String]
    -- ^ In-scope identifier spellings, offered at query positions.
  }

data ReplStep state
  = ContinueRepl state
  | ExitRepl state

runReplDriver
  :: Maybe FilePath
  -> state
  -> (state -> String)
  -> (state -> ReplCompletions)
  -> (state -> [String] -> String -> IO (ReplStep state))
  -> IO state
runReplDriver historyPath initial prompt completions evaluate = do
  snapshot <- newIORef $ completions initial
  runInputT (settings snapshot) $ loop snapshot initial
 where
  settings snapshot = Settings
    { complete = replCompletion $ readIORef snapshot
    , historyFile = historyPath
    , autoAddHistory = True
    }

  loop snapshot state = step snapshot state >>= \outcome -> case outcome of
    ContinueRepl next -> loop snapshot next
    ExitRepl final -> pure final

  step snapshot state = handleInterrupt interrupted $ withInterrupt $ do
    liftIO $ writeIORef snapshot $ completions state
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

replCompletion
  :: IO ReplCompletions
  -> (String, String)
  -> IO (String, [Completion])
replCompletion snapshot input@(left, _)
  | completingPath $ reverse left = completeFilename input
  | otherwise = completeWordWithPrev Nothing " \t"
      (completeReplWord snapshot) input

completeReplWord :: IO ReplCompletions -> String -> String -> IO [Completion]
completeReplWord snapshot reversedPrevious word = do
  completions <- snapshot
  pure
    [ simpleCompletion candidate
    | candidate <- candidatesFor completions
        (words $ reverse reversedPrevious) word
    , word `isPrefixOf` candidate
    ]

candidatesFor :: ReplCompletions -> [String] -> String -> [String]
candidatesFor completions previous word = case previous of
  []
    | ":" `isPrefixOf` word -> commandNames
    | otherwise -> completionIdentifiers completions
  [":backend"] -> backendNames
  [":set", "backend"] -> backendNames
  [":set"] -> settingNames ++ map ('+' :) booleanSettingNames
    ++ map ('-' :) booleanSettingNames
  [":unset"] -> settingNames
  [":show"] -> showNames
  (command : _)
    | command `elem` moduleCommands -> completionModules completions
        ++ map ('*' :) (completionModules completions)
    | not $ ":" `isPrefixOf` command -> completionIdentifiers completions
    | command `elem` queryCommands -> completionIdentifiers completions
  _ -> []
 where
  moduleCommands = [":module", ":m", ":browse"]
  queryCommands =
    [ ":info", ":i"
    , ":type", ":t"
    , ":kind", ":k", ":kind!", ":k!"
    , ":djinn", ":exference", ":compare", ":synth", ":sy"
    ]

completingPath :: String -> Bool
completingPath source = case words source of
  command : _ -> command `elem`
    [":load", ":l", ":add", ":unadd", ":script", ":cd", ":edit"]
  [] -> False
