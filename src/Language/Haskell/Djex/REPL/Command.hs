-- | Pure command grammar and help inventory for the shared Djex REPL.
--
-- Command names, abbreviations, help, and completion all derive from the same
-- descriptor table. Exact aliases win; otherwise a nonempty unique prefix of
-- a canonical command is accepted, matching GHCi without making parser order
-- decide newly ambiguous abbreviations.
module Language.Haskell.Djex.REPL.Command
  ( ReplBackend (..)
  , replBackendName
  , ReplQueryTarget (..)
  , ReplInput (..)
  , ReplCommand (..)
  , parseReplInput
  , parseReplBackend
  , commandNames
  , backendNames
  , settingNames
  , showNames
  , shortHelp
  , commandHelp
  ) where

import Data.Char (isSpace, toLower)
import Data.List (intercalate, isPrefixOf)
import Text.Read (readMaybe)

import Language.Haskell.Djex (Backend (..))

-- | Backend selection retained by the interactive frontend.
data ReplBackend
  = OneBackend Backend
  | BothBackends
  deriving (Eq, Show)

replBackendName :: ReplBackend -> String
replBackendName (OneBackend DjinnBackend) = "djinn"
replBackendName (OneBackend ExferenceBackend) = "exference"
replBackendName BothBackends = "both"

-- | Whether a query uses the active selection or an explicit backend set.
data ReplQueryTarget
  = ActiveBackends
  | ExplicitBackends ReplBackend
  deriving (Eq, Show)

-- | One complete logical input after explicit multiline collection.
data ReplInput
  = ReplNoInput
  | ReplQuery ReplQueryTarget String
  | ReplCommand ReplCommand
  deriving (Eq, Show)

data ReplCommand
  = Browse
  | ChangeBackend (Maybe String)
  | ChangeDirectory FilePath
  | CompareBackends String
  | Help (Maybe String)
  | History (Maybe String)
  | InspectDeclaration String
  | LoadEnvironment FilePath
  | Quit
  | ReloadEnvironment
  | RepeatQuery
  | RunScript FilePath
  | RunShell String
  | SetOption String
  | ShowState (Maybe String)
  | UnsetOption String
  | Version
  deriving (Eq, Show)

data CommandDescriptor = CommandDescriptor
  { descriptorName :: String
  , descriptorAliases :: [String]
  , descriptorArguments :: String
  , descriptorSummary :: String
  , descriptorParser :: String -> Either String ReplInput
  }

parseReplInput :: String -> Either String ReplInput
parseReplInput source
  | null input = Right ReplNoInput
  | input == ":" = Right $ ReplCommand RepeatQuery
  | ":!" `isPrefixOf` input = ReplCommand . RunShell
      <$> required "a shell command" (trim $ drop 2 input)
  | ':' : commandSource <- input = parseColon commandSource
  | otherwise = Right $ ReplQuery ActiveBackends input
 where
  input = trim source

parseColon :: String -> Either String ReplInput
parseColon source = do
  let (token, arguments) = splitHead source
      normalized = map toLower token
  descriptor <- resolveCommand normalized
  descriptorParser descriptor arguments

resolveCommand :: String -> Either String CommandDescriptor
resolveCommand token = case exactMatches of
  [descriptor] -> Right descriptor
  descriptors@(_ : _) -> ambiguous descriptors
  [] -> case prefixMatches of
    [descriptor] -> Right descriptor
    [] -> Left $ "unknown command :" ++ token
    descriptors -> ambiguous descriptors
 where
  exactMatches = filter exact commandDescriptors
  exact descriptor = token == descriptorName descriptor
    || token `elem` descriptorAliases descriptor
  prefixMatches
    | null token = []
    | otherwise = filter (isPrefixOf token . descriptorName)
        commandDescriptors
  ambiguous descriptors = Left $ "ambiguous command :" ++ token
    ++ " (could be "
    ++ intercalate ", " (map ((':' :) . descriptorName) descriptors) ++ ")"

parseReplBackend :: String -> Either String ReplBackend
parseReplBackend source = case exactMatches of
  [backend] -> Right backend
  [] -> case prefixMatches of
    [backend] -> Right backend
    [] -> Left $ "unknown backend " ++ show source
    matches -> Left $ "ambiguous backend " ++ show source ++ " (could be "
      ++ intercalate ", " (map replBackendName matches) ++ ")"
  matches -> Left $ "ambiguous backend " ++ show source ++ " (could be "
    ++ intercalate ", " (map replBackendName matches) ++ ")"
 where
  token = map toLower $ trim source
  choices =
    [ OneBackend DjinnBackend
    , OneBackend ExferenceBackend
    , BothBackends
    ]
  exactMatches = filter ((== token) . replBackendName) choices
  prefixMatches
    | null token = []
    | otherwise = filter (isPrefixOf token . replBackendName) choices

commandDescriptors :: [CommandDescriptor]
commandDescriptors =
  [ command "backend" ["b"] "[djinn|exference|both]"
      "show or change the active backend selection"
      $ Right . ReplCommand . ChangeBackend . optionalText
  , command "browse" [] ""
      "list declarations loaded by the active backends"
      $ noArguments $ ReplCommand Browse
  , command "cd" [] "DIR" "change the process working directory"
      $ fmap (ReplCommand . ChangeDirectory) . pathArgument "a directory"
  , command "compare" [] "TYPE" "synthesize with both backends"
      $ fmap (ReplCommand . CompareBackends) . required "a type"
  , command "djinn" [] "TYPE" "synthesize once with Djinn"
      $ fmap (ReplQuery $ ExplicitBackends $ OneBackend DjinnBackend)
          . required "a type"
  , command "exference" [] "TYPE" "synthesize once with Exference"
      $ fmap (ReplQuery $ ExplicitBackends $ OneBackend ExferenceBackend)
          . required "a type"
  , command "help" ["h", "?"] "[COMMAND]" "show command help"
      $ Right . ReplCommand . Help . optionalText
  , command "history" ["hist"] "[N]" "show command history"
      $ Right . ReplCommand . History . optionalText
  , command "info" ["i"] "NAME" "inspect a declaration by exact name"
      $ fmap (ReplCommand . InspectDeclaration) . required "a declaration name"
  , command "load" ["l"] "DIR" "load an Exference environment directory"
      $ fmap (ReplCommand . LoadEnvironment)
          . pathArgument "an environment directory"
  , command "pwd" [] "" "show the current working directory"
      $ noArguments $ ReplCommand $ ShowState $ Just "directory"
  , command "quit" ["q"] "" "leave the REPL"
      $ noArguments $ ReplCommand Quit
  , command "reload" ["r"] "" "reload the Exference environment"
      $ noArguments $ ReplCommand ReloadEnvironment
  , command "script" [] "FILE" "execute a file of REPL inputs"
      $ fmap (ReplCommand . RunScript) . pathArgument "a script file"
  , command "set" ["s"] "[OPTION [VALUE]]" "show or change settings"
      $ Right . ReplCommand . SetOption
  , command "show" []
      "[settings|backends|environment|omissions|diagnostics|directory]"
      "inspect REPL and backend state"
      $ Right . ReplCommand . ShowState . optionalText
  , command "synth" ["sy"] "TYPE" "synthesize with the active backend(s)"
      $ fmap (ReplQuery ActiveBackends) . required "a type"
  , command "unset" [] "OPTION" "restore one setting to its default"
      $ fmap (ReplCommand . UnsetOption) . required "a setting name"
  , command "version" ["v"] "" "show the Djex version"
      $ noArguments $ ReplCommand Version
  ]

command
  :: String
  -> [String]
  -> String
  -> String
  -> (String -> Either String ReplInput)
  -> CommandDescriptor
command = CommandDescriptor

commandNames :: [String]
commandNames = map ((':' :) . descriptorName) commandDescriptors
  ++ [":!", ":{", ":}"]

backendNames :: [String]
backendNames = ["djinn", "exference", "both"]

settingNames :: [String]
settingNames =
  [ "backend"
  , "target"
  , "select"
  , "render"
  , "qualification"
  , "prompt"
  , "candidate-limit"
  , "choice-budget"
  , "allow-unused"
  , "allow-constraints"
  , "constraint-deferral-steps"
  , "multi-constructor-patterns"
  , "max-steps"
  , "max-queue"
  , "max-depth"
  , "fix"
  ]

showNames :: [String]
showNames =
  [ "settings"
  , "backends"
  , "environment"
  , "omissions"
  , "diagnostics"
  , "directory"
  ]

shortHelp :: String
shortHelp = unlines
  $ "Enter a Haskell type to synthesize with the active backend(s)."
  : "Use :{ and :} around multiline input; : repeats the last query."
  : ""
  : "Commands (unique prefixes are accepted):"
  : map renderDescriptor commandDescriptors
  ++ ["  :! COMMAND" ++ padding ":! COMMAND" ++ "run a shell command"]
 where
  width = maximum $ map (length . descriptorUsage) commandDescriptors
    ++ [length ":! COMMAND"]
  padding source = replicate (width - length source + 2) ' '
  renderDescriptor descriptor = "  " ++ usage ++ padding usage
    ++ descriptorSummary descriptor
   where
    usage = descriptorUsage descriptor

commandHelp :: String -> Either String String
commandHelp source = case token of
  "!" -> Right $ unlines
    [ ":! COMMAND"
    , "  run one shell command without changing REPL state"
    ]
  "{" -> Right multilineHelp
  "}" -> Right multilineHelp
  _ -> do
    descriptor <- resolveCommand token
    pure $ unlines
      $ [descriptorUsage descriptor, "  " ++ descriptorSummary descriptor]
      ++ aliasLines descriptor
      ++ descriptorDetails descriptor
 where
  token = dropWhile (== ':') $ map toLower $ trim source
  multilineHelp = unlines
    [ ":{"
    , "TYPE"
    , ":}"
    , "  collect a type query over multiple input lines"
    ]
  aliasLines descriptor = case descriptorAliases descriptor of
    [] -> []
    aliases -> ["  aliases: " ++ intercalate ", " (map (':' :) aliases)]
  descriptorDetails descriptor = case descriptorName descriptor of
    "backend" -> ["  choices: " ++ intercalate ", " backendNames]
    "set" ->
      [ "  settings: " ++ intercalate ", " settingNames
      , "  booleans also accept :set +NAME and :set -NAME"
      ]
    "show" -> ["  subjects: " ++ intercalate ", " showNames]
    "unset" -> ["  restores a setting to its built-in default"]
    _ -> []

descriptorUsage :: CommandDescriptor -> String
descriptorUsage descriptor = ':' : descriptorName descriptor
  ++ case descriptorArguments descriptor of
    "" -> ""
    arguments -> " " ++ arguments

noArguments :: ReplInput -> String -> Either String ReplInput
noArguments result source
  | null $ trim source = Right result
  | otherwise = Left "this command takes no arguments"

required :: String -> String -> Either String String
required description source
  | null value = Left $ "expected " ++ description
  | otherwise = Right value
 where
  value = trim source

optionalText :: String -> Maybe String
optionalText source = case trim source of
  "" -> Nothing
  value -> Just value

pathArgument :: String -> String -> Either String FilePath
pathArgument description source = do
  value <- required description source
  pure $ case readMaybe value of
    Just decoded -> decoded
    Nothing -> value

splitHead :: String -> (String, String)
splitHead source = (token, trim arguments)
 where
  withoutLeading = dropWhile isSpace source
  (token, arguments) = break isSpace withoutLeading

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
