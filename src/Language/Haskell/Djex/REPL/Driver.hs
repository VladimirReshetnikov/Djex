{-# LANGUAGE LambdaCase #-}

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
import Data.Char (isControl, isSpace, toLower)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import System.Console.Haskeline
  ( Completion (..)
  , InputT
  , Settings (..)
  , completeQuotedWord
  , completeWord
  , completeWordWithPrev
  , getHistory
  , getInputLine
  , handleInterrupt
  , listFiles
  , outputStrLn
  , runInputT
  , simpleCompletion
  , withInterrupt
  )
import System.Console.Haskeline.History (historyLines)

import Language.Haskell.Djex.Command (heuristicNames, searchStrategyNames)
import Language.Haskell.Djex.REPL.Command
  ( CompletionDomain (..)
  , backendNames
  , booleanSettingNames
  , commandCompletionDomain
  , commandNames
  , helpNames
  , resolveCommandToken
  , settingNames
  , showNames
  )
import Language.Haskell.Djex.Text (trim)

-- | Scope-derived completion candidates projected from the frontend state.
data ReplCompletions = ReplCompletions
  { completionModules :: [String]
    -- ^ Loaded module names, offered after module-oriented commands.
  , completionIdentifiers :: [String]
    -- ^ In-scope type and value spellings, offered where either namespace can
    -- occur (for example, import lists and expression type annotations).
  , completionTypeIdentifiers :: [String]
    -- ^ In-scope type-namespace spellings, offered for synthesis and kind
    -- queries. Keeping this projection separate prevents a value-only name
    -- from being suggested where the parser necessarily expects a type.
  }

-- | The evaluator's verdict after one logical input: keep prompting with
-- the updated state, or leave the loop and return it.
data ReplStep state
  = ContinueRepl state
  | ExitRepl state

-- | Run the Haskeline read-evaluate loop until the evaluator returns
-- 'ExitRepl' or input reaches end-of-file, then return the final state.
-- Before every prompt the completion snapshot is refreshed from the current
-- state; the evaluator receives the state, the history lines recorded
-- before the current input (oldest first), and one logical input,
-- where @:{@ ... @:}@ collects several lines. An interrupt prints
-- @Interrupted.@ and continues with the unchanged state.
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

  loop snapshot state = step snapshot state >>= \case
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
  | completingPath $ reverse left = completeDjexFilename input
  | otherwise = completeWordWithPrev Nothing " \t"
      (completeReplWord snapshot) input

-- Haskeline's stock filename completer emits shell-style backslash-escaped
-- whitespace. Djex path commands intentionally parse Haskell string
-- literals instead, so every filename is introduced as a double-quoted
-- literal and quoted continuation is delegated to Haskeline. The two grammars
-- now agree for spaces, brackets, quotes, and platform path separators.
completeDjexFilename
  :: (String, String)
  -> IO (String, [Completion])
completeDjexFilename = completeQuotedWord (Just '\\') "\"" listSafeFiles
  $ completeWord Nothing " \t\n\""
  $ fmap (map quoteInitialPath) . listSafeFiles
 where
  listSafeFiles path = filter (not . any isControl . replacement)
    <$> listFiles path

  quoteInitialPath completion = completion
    { replacement = if isFinished completion
        then show path
        else init $ show path
    }
   where
    path = replacement completion

completeReplWord :: IO ReplCompletions -> String -> String -> IO [Completion]
completeReplWord snapshot reversedPrevious word = do
  completions <- snapshot
  pure
    [ simpleCompletion candidate
    | candidate <- candidatesFor completions previous word
    , completionPrefix previous word candidate
    ]
 where
  previous = words $ reverse reversedPrevious

completionPrefix :: [String] -> String -> String -> Bool
completionPrefix previous word candidate
  | keywordCompletion previous =
      map toLower word `isPrefixOf` map toLower candidate
  | otherwise = word `isPrefixOf` candidate
 where
  keywordCompletion [] = ":" `isPrefixOf` word
  keywordCompletion (command : _) = case commandCompletionDomain command of
    Just BackendCompletion -> True
    Just CommandCompletion -> True
    Just SettingCompletion -> True
    Just ShowCompletion -> True
    _ -> False

candidatesFor :: ReplCompletions -> [String] -> String -> [String]
candidatesFor completions previous word = case previous of
  []
    | ":" `isPrefixOf` word -> commandNames
    | otherwise -> completionTypeIdentifiers completions
  command : arguments
    | importModulePosition command arguments ->
        moduleCandidates False completions word
    | command == "import" -> completionIdentifiers completions
    | not $ ":" `isPrefixOf` command ->
        completionTypeIdentifiers completions
    | otherwise -> case commandCompletionDomain command of
        Just BackendCompletion
          | null arguments -> backendNames
        Just CommandCompletion
          | null arguments -> if ":" `isPrefixOf` word
              then map (':' :) helpNames
              else helpNames
        Just IdentifierCompletion -> completionIdentifiers completions
        Just TypeIdentifierCompletion -> completionTypeIdentifiers completions
        Just SynthesisCompletion -> synthesisCandidates arguments completions
        Just ModuleCompletion
          | null arguments -> moduleCandidates False completions word
        Just ModuleContextCompletion ->
          moduleCandidates
            (null arguments && not (attachedModuleMode command))
            completions word
        Just SettingCompletion -> settingCandidates command arguments
        Just ShowCompletion
          | null arguments -> showNames
        _ -> []

 where
  synthesisCandidates arguments available = case arguments of
    [] -> "--where" : completionTypeIdentifiers available
    "--where" : clause
      | "--" `elem` clause -> completionTypeIdentifiers available
      | otherwise -> ["--"]
    _ -> completionTypeIdentifiers available

  settingCandidates command arguments = case
      (resolveCommandToken command, arguments) of
    (Right "set", []) -> settingNames
      ++ map ('+' :) booleanSettingNames
      ++ map ('-' :) booleanSettingNames
    (Right "set", [setting]) -> case map toLower setting of
      "backend" -> backendNames
      "djinn-strategy" -> searchStrategyNames
      "heuristic" -> heuristicNames
      _ -> []
    (Right "unset", []) -> settingNames
    _ -> []

  -- A bare import is Haskell syntax rather than a colon command, but its first
  -- semantic argument is still a loaded module. Keep the useful @qualified@
  -- and @safe@ prefixes in module-completion position; once a module has been
  -- entered, ordinary identifier completion resumes for its import list.
  importModulePosition command arguments =
    command == "import" && all importModifier arguments
  importModifier token = token `elem` ["qualified", "safe"]

attachedModuleMode :: String -> Bool
attachedModuleMode source = map toLower (dropWhile (== ':') source)
  `elem` ["module+", "module-"]

moduleCandidates :: Bool -> ReplCompletions -> String -> [String]
moduleCandidates allowSign completions word = case modulePrefix allowSign word of
  Nothing -> []
  Just (prefix, hasStar) ->
    map (prefix ++) modules
      ++ [prefix ++ ('*' : name) | not hasStar, name <- modules]
 where
  modules = completionModules completions

-- Preserve the mode and source-scope markers already typed before a module
-- fragment. @:module@ accepts one optional sign and one optional star;
-- @:browse@ accepts only the star.
modulePrefix :: Bool -> String -> Maybe (String, Bool)
modulePrefix allowSign source = do
  let (sign, afterSign) = case source of
        marker : rest
          | allowSign && marker `elem` "+-" -> ([marker], rest)
        _ -> ([], source)
      (star, fragment) = case afterSign of
        '*' : rest -> ("*", rest)
        _ -> ("", afterSign)
  if any (\character -> character `elem` "+-*") fragment
    then Nothing
    else Just (sign ++ star, not $ null star)

completingPath :: String -> Bool
completingPath source = case break isSpace $ dropWhile isSpace source of
  (command, _ : _) ->
    commandCompletionDomain command == Just PathCompletion
  _ -> False
