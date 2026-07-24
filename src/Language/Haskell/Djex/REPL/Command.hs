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
  , ModuleChange (..)
  , KindNormalization (..)
  , TypeDefaulting (..)
  , ReplSetting (..)
  , replSettingName
  , parseReplSetting
  , parseReplInput
  , parseReplBackend
  , commandNames
  , backendNames
  , settingNames
  , booleanSettingNames
  , showNames
  , shortHelp
  , commandHelp
  ) where

import Data.Char (isSpace, toLower)
import Data.List (intercalate, isPrefixOf)
import Text.Read (readMaybe)

import Language.Haskell.Djex (Backend (..))
import Language.Haskell.Djex.Package
  ( PackageInstallMode
  , parsePackageInstall
  , validatePackageTargets
  )
import Language.Haskell.Djex.Text (normalize, trim)

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
  | ReplImport String
  | ReplCommand ReplCommand
  deriving (Eq, Show)

data ReplCommand
  = AddEnvironment [FilePath]
  | Browse (Maybe String)
  | ChangeBackend (Maybe String)
  | ChangeDirectory FilePath
  | ChangeModules ModuleChange [String]
  | CompareBackends String
  | DownloadPackages [String]
  | EditFile (Maybe FilePath)
  | Evaluate String
  | Help (Maybe String)
  | History (Maybe String)
  | InspectDeclaration String
  | InspectKind KindNormalization String
  | InspectType TypeDefaulting String
  | InstallPackages PackageInstallMode [String]
  | LoadEnvironment [FilePath]
  | Quit
  | ReloadEnvironment
  | RepeatQuery
  | RunScript FilePath
  | RunShell String
  | SetOption String
  | ShowState (Maybe String)
  | UnaddEnvironment [FilePath]
  | UnsetOption String
  | Version
  deriving (Eq, Show)

-- | How @:module@ changes the modules visible to synthesis queries.
--
-- The leading sign applies to the entire argument list, as it does in GHCi:
-- no sign replaces the context, @+@ extends it, and @-@ subtracts from it.
data ModuleChange
  = ReplaceModules
  | AddModules
  | RemoveModules
  deriving (Eq, Show)

-- | Whether @:kind@ also requests the type's normalized form.
--
-- The mode belongs to the command token: @:kind! T@ normalizes, while
-- @:kind ! T@ inspects the ordinary type text @! T@.
data KindNormalization
  = PreserveTypeSynonyms
  | NormalizeTypeSynonyms
  deriving (Eq, Show)

-- | Whether @:type@ applies its explicit numeric @+d@ defaulting request.
-- Ordinary ambiguity defaulting remains part of inference in either mode;
-- this flag additionally defaults eligible variables that occur in the
-- reported result type.
data TypeDefaulting
  = PreserveTypeVariables
  | DefaultTypeVariables
  deriving (Eq, Show)

-- | Every mutable REPL setting, in stable display and completion order.
data ReplSetting
  = BackendSetting
  | TargetSetting
  | SelectionSetting
  | RenderingSetting
  | QualificationSetting
  | PromptSetting
  | CandidateLimitSetting
  | ChoiceBudgetSetting
  | DjinnAxiomsSetting
  | AllowUnusedSetting
  | AllowConstraintsSetting
  | ConstraintDeferralStepsSetting
  | MultiConstructorPatternsSetting
  | MaximumStepsSetting
  | MaximumQueueSetting
  | MaximumDepthSetting
  | FixSetting
  deriving (Bounded, Enum, Eq, Show)

replSettingName :: ReplSetting -> String
replSettingName setting = case setting of
  BackendSetting -> "backend"
  TargetSetting -> "target"
  SelectionSetting -> "select"
  RenderingSetting -> "render"
  QualificationSetting -> "qualification"
  PromptSetting -> "prompt"
  CandidateLimitSetting -> "candidate-limit"
  ChoiceBudgetSetting -> "choice-budget"
  DjinnAxiomsSetting -> "djinn-axioms"
  AllowUnusedSetting -> "allow-unused"
  AllowConstraintsSetting -> "allow-constraints"
  ConstraintDeferralStepsSetting -> "constraint-deferral-steps"
  MultiConstructorPatternsSetting -> "multi-constructor-patterns"
  MaximumStepsSetting -> "max-steps"
  MaximumQueueSetting -> "max-queue"
  MaximumDepthSetting -> "max-depth"
  FixSetting -> "fix"

parseReplSetting :: String -> Either String ReplSetting
parseReplSetting source = case filter ((== normalize source) . replSettingName)
    [minBound .. maxBound] of
  [setting] -> Right setting
  _ -> Left $ "unknown setting " ++ show source

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
  | isImport input = Right $ ReplImport input
  | otherwise = Right $ ReplQuery ActiveBackends input
 where
  input = trim source
  isImport value = case stripKeyword "import" value of
    Just _ -> True
    Nothing -> False

parseColon :: String -> Either String ReplInput
parseColon source = do
  let (rawToken, rawArguments) = splitHead source
      (token, arguments) = normalizeAttachedModule rawToken rawArguments
      normalized = map toLower token
  case attachedKindBangBase normalized of
    Just _ -> ReplCommand . InspectKind NormalizeTypeSynonyms
      <$> required "a Haskell type" arguments
    Nothing -> do
      descriptor <- resolveCommand normalized
      descriptorParser descriptor arguments

-- GHCi requires whitespace between the command name and its @+@/@-@ mode.
-- Accepting the commonly written attached form costs no ambiguity because
-- neither spelling participates in alias or unique-prefix resolution.
normalizeAttachedModule :: String -> String -> (String, String)
normalizeAttachedModule token arguments = case map toLower token of
  "module+" -> ("module", prependMode '+' arguments)
  "module-" -> ("module", prependMode '-' arguments)
  _ -> (token, arguments)
 where
  prependMode mode "" = [mode]
  prependMode mode rest = mode : ' ' : rest

-- A trailing bang is a mode only when the preceding token already resolves to
-- the sole @kind@ descriptor. This accepts @:k!@ and every unique prefix while
-- keeping @:kind!T@ and bangs on unrelated commands ordinary unknown tokens.
attachedKindBangBase :: String -> Maybe String
attachedKindBangBase token = case reverse token of
  '!' : reversedBase
    | let base = reverse reversedBase
    , not $ null base
    , Right descriptor <- resolveCommand base
    , descriptorName descriptor == "kind" -> Just base
  _ -> Nothing

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
  token = normalize source
  exactMatches = filter ((== token) . replBackendName) replBackendChoices
  prefixMatches
    | null token = []
    | otherwise = filter (isPrefixOf token . replBackendName)
        replBackendChoices

replBackendChoices :: [ReplBackend]
replBackendChoices =
  [ OneBackend DjinnBackend
  , OneBackend ExferenceBackend
  , BothBackends
  ]

commandDescriptors :: [CommandDescriptor]
commandDescriptors =
  [ command "add" [] "[TARGET ...]"
      "add module or file targets and reload their dependencies"
      $ fmap (ReplCommand . AddEnvironment) . commandArguments
  , command "backend" ["b"] "[djinn|exference|both]"
      "show or change the active backend selection"
      $ Right . ReplCommand . ChangeBackend . optionalText
  , command "browse" [] "[[*]MODULE]"
      "list declarations exported by a module or the current scope"
      $ fmap (ReplCommand . Browse)
          . optionalArgument "at most one module name"
  , command "cd" [] "DIR" "change the process working directory"
      $ fmap (ReplCommand . ChangeDirectory) . pathArgument "a directory"
  , command "compare" [] "TYPE" "synthesize with both backends"
      $ fmap (ReplCommand . CompareBackends) . required "a type"
  , command "djinn" [] "TYPE" "synthesize once with Djinn"
      $ fmap (ReplQuery $ ExplicitBackends $ OneBackend DjinnBackend)
          . required "a type"
  , command "download" ["dl"] "CABAL_TARGET ..."
      "ask Cabal to fetch targets and dependencies into its source cache"
      $ fmap (ReplCommand . DownloadPackages) . packageArguments
  , command "edit" [] "[FILE]"
      "open the configured editor on a file"
      $ fmap (ReplCommand . EditFile) . optionalArgument "at most one file"
  , command "eval" [] "EXPRESSION"
      "evaluate a Haskell expression with real GHC"
      $ fmap (ReplCommand . Evaluate) . required "a Haskell expression"
  , command "exference" [] "TYPE" "synthesize once with Exference"
      $ fmap (ReplQuery $ ExplicitBackends $ OneBackend ExferenceBackend)
          . required "a type"
  , command "help" ["h", "?"] "[COMMAND]" "show command help"
      $ Right . ReplCommand . Help . optionalText
  , command "history" ["hist"] "[N]" "show command history"
      $ Right . ReplCommand . History . optionalText
  , command "info" ["i"] "NAME" "inspect a declaration by exact name"
      $ fmap (ReplCommand . InspectDeclaration) . required "a declaration name"
  , command "install" [] "[--lib] CABAL_TARGET ..."
      "build and install package executables or libraries through Cabal"
      $ fmap (\(mode, targets) -> ReplCommand $ InstallPackages mode targets)
          . installArguments
  , command "kind" ["k"] "TYPE"
      "infer the kind of a Haskell type in the current module scope"
      $ fmap (ReplCommand . InspectKind PreserveTypeSynonyms)
          . required "a Haskell type"
  , command "load" ["l"] "[TARGET ...]"
      "replace the module or file targets and load their dependencies"
      $ fmap (ReplCommand . LoadEnvironment) . commandArguments
  , command "module" ["m"] "[+|-] [[*]MODULE ...]"
      "set, add to, or remove from the module context"
      $ fmap ReplCommand . parseModuleChange
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
      ("[" ++ intercalate "|" showNames ++ "]")
      "inspect REPL and backend state"
      $ Right . ReplCommand . ShowState . optionalText
  , command "synth" ["sy"] "TYPE" "synthesize with the active backend(s)"
      $ fmap (ReplQuery ActiveBackends) . required "a type"
  , command "type" ["t"] "[+d] EXPRESSION"
      "infer the type of a Haskell expression in the current module scope"
      $ fmap ReplCommand . parseTypeInspection
  , command "unadd" [] "[TARGET ...]"
      "remove module or file targets and reload their dependencies"
      $ fmap (ReplCommand . UnaddEnvironment) . commandArguments
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
commandNames = concatMap completionNames commandDescriptors
  ++ [":kind!", ":k!", ":!", ":{", ":}"]
 where
  completionNames descriptor = map (':' :)
    $ descriptorName descriptor : descriptorAliases descriptor

backendNames :: [String]
backendNames = map replBackendName replBackendChoices

settingNames :: [String]
settingNames = map replSettingName [minBound .. maxBound]

booleanSettingNames :: [String]
booleanSettingNames = map replSettingName
  [ DjinnAxiomsSetting
  , AllowUnusedSetting
  , AllowConstraintsSetting
  , MultiConstructorPatternsSetting
  , FixSetting
  ]

showNames :: [String]
showNames =
  [ "settings"
  , "backends"
  , "environment"
  , "imports"
  , "modules"
  , "targets"
  , "omissions"
  , "diagnostics"
  , "directory"
  ]

shortHelp :: String
shortHelp = unlines
  $ "Enter a Haskell type to synthesize with the active backend(s)."
  : "Enter import MODULE to extend the modules visible to queries."
  : "Use :{ and :} around multiline input; : repeats the last query."
  : ""
  : "Commands (unique prefixes are accepted):"
  : renderImport
  : map renderDescriptor commandDescriptors
  ++ ["  :! COMMAND" ++ padding ":! COMMAND" ++ "run a shell command"]
 where
  width = maximum $ map (length . descriptorUsage) commandDescriptors
    ++ [length importUsage, length ":! COMMAND"]
  importUsage = "import DECLARATION"
  padding source = replicate (width - length source + 2) ' '
  renderImport = "  " ++ importUsage ++ padding importUsage
    ++ "add a Haskell import to the module context"
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
  "import" -> Right $ unlines
    [ "import DECLARATION"
    , "  add a Haskell import to the module context"
    , "  qualified, as, hiding, and explicit import lists are accepted"
    ]
  _ -> do
    descriptor <- resolveCommand $ normalizeHelpToken token
    pure $ unlines
      $ [descriptorUsage descriptor, "  " ++ descriptorSummary descriptor]
      ++ aliasLines descriptor
      ++ descriptorDetails descriptor
 where
  token = dropWhile (== ':') $ normalize source
  multilineHelp = unlines
    [ ":{"
    , "INPUT"
    , ":}"
    , "  collect one logical input over multiple lines"
    ]
  normalizeHelpToken value
    | Just base <- attachedKindBangBase value = base
  normalizeHelpToken "module+" = "module"
  normalizeHelpToken "module-" = "module"
  normalizeHelpToken value = value
  aliasLines descriptor = case descriptorAliases descriptor of
    [] -> []
    aliases -> ["  aliases: " ++ intercalate ", " (map (':' :) aliases)]
  descriptorDetails descriptor = case descriptorName descriptor of
    "add" -> targetDetails
    "backend" -> ["  choices: " ++ intercalate ", " backendNames]
    "browse" ->
      [ "  prefix a loaded module with * to include non-exported declarations"
      ]
    "download" -> packageDetails
    "edit" ->
      [ "  uses the VISUAL editor, or EDITOR when VISUAL is unset"
      , "  with no argument, edits the most recently loaded file target"
      , "  the environment is not reloaded; use :reload afterwards"
      ]
    "eval" ->
      [ "  compiles the loaded file targets into scope when they are"
      , "  real Haskell; otherwise evaluates against Prelude alone"
      , "  each :eval runs a fresh session, so bindings do not persist"
      ]
    "install" -> packageDetails ++
      [ "  defaults to Cabal's executable mode; --lib installs libraries"
      , "  a leading -- makes a following --lib an ordinary package target"
      , "  installation is independent of the surrounding Cabal project"
      ]
    "kind" ->
      [ "  append ! to the command or any accepted prefix to show normal form"
      , "  uses the loaded module scope independently of backend selection"
      , "  normal form expands saturated type synonyms, not type families"
      ]
    "load" ->
      [ "  accepts module names, file paths, quoted strings, or [String] syntax"
      , "  with no targets, unloads the current environment"
      ]
    "module" ->
      [ "  no sign replaces the context; + adds modules; - removes modules"
      , "  canonical examples: :module +Data.List, :module + Data.Maybe"
      , "  prefix a loaded module with * to include non-exported declarations"
      ]
    "set" ->
      [ "  settings: " ++ intercalate ", " settingNames
      , "  booleans also accept :set +NAME and :set -NAME"
      ]
    "show" -> ["  subjects: " ++ intercalate ", " showNames]
    "type" ->
      [ "  +d defaults eligible numeric type variables in the result"
      , "  the obsolete +v mode is no longer accepted"
      ]
    "unadd" -> targetDetails
    "unset" -> ["  restores a setting to its built-in default"]
    _ -> []
  targetDetails =
    [ "  accepts module names, file paths, quoted strings, or [String] syntax"
    ]
  packageDetails =
    [ "  accepts Cabal targets as words, quoted strings, or [String] syntax"
    , "  target values are passed to Cabal as data, never as command options"
    , "  this does not add compiled package modules to Djex's source workspace"
    ]

descriptorUsage :: CommandDescriptor -> String
descriptorUsage descriptor = ':' : usageName
  ++ case descriptorArguments descriptor of
    "" -> ""
    arguments -> " " ++ arguments
 where
  usageName
    | descriptorName descriptor == "kind" = "kind[!]"
    | otherwise = descriptorName descriptor

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
  arguments <- commandArguments source
  case arguments of
    [value] -> Right value
    [] -> Left $ "expected " ++ description
    _ -> Left $ "expected " ++ description ++ " as one argument"

optionalArgument :: String -> String -> Either String (Maybe String)
optionalArgument description source = do
  arguments <- commandArguments source
  case arguments of
    [] -> Right Nothing
    [value] -> Right $ Just value
    _ -> Left $ "expected " ++ description

parseModuleChange :: String -> Either String ReplCommand
parseModuleChange source = do
  let input = dropWhile isSpace source
      (change, argumentsSource) = case input of
        '+' : rest -> (AddModules, rest)
        '-' : rest -> (RemoveModules, rest)
        _ -> (ReplaceModules, input)
  ChangeModules change <$> commandArguments argumentsSource

parseTypeInspection :: String -> Either String ReplCommand
parseTypeInspection source = case stripKeyword "+d" input of
  Just expression -> InspectType DefaultTypeVariables
    <$> required "a Haskell expression" expression
  Nothing -> case stripKeyword "+v" input of
    Just _ -> Left "`:type +v' has gone; use `:type' instead"
    Nothing -> InspectType PreserveTypeVariables
      <$> required "a Haskell expression" input
 where
  input = trim source

packageArguments :: String -> Either String [String]
packageArguments source = do
  arguments <- commandArguments source
  validatePackageTargets arguments

installArguments
  :: String
  -> Either String (PackageInstallMode, [String])
installArguments source = commandArguments source >>= parsePackageInstall

-- | Parse the argument forms accepted by path- and module-oriented commands.
--
-- A whole @[String]@ is convenient in scripts, while ordinary interactive
-- use consists of whitespace-separated bare words or Haskell string literals.
-- Unlike the old single-path parser, malformed quotes are errors rather than
-- silently becoming literal quote characters in a path.
commandArguments :: String -> Either String [String]
commandArguments source = case trim source of
  "" -> Right []
  input@('[' : _) -> case readMaybe input of
    Just arguments -> Right arguments
    Nothing -> Left "malformed [String] command argument list"
  input -> parseWords input
 where
  parseWords "" = Right []
  parseWords input = case dropWhile isSpace input of
    "" -> Right []
    quoted@('"' : _) -> do
      (argument, rest) <- readStringArgument quoted
      (argument :) <$> parseWords rest
    unquoted -> do
      let (argument, rest) = break isSpace unquoted
      if '"' `elem` argument
        then Left "malformed string literal in command arguments"
        else (argument :) <$> parseWords rest

  readStringArgument input = case reads input of
    [(argument, "")] -> Right (argument, "")
    [(argument, rest@(first : _))]
      | isSpace first -> Right (argument, rest)
      | otherwise -> Left "expected whitespace after string literal"
    _ -> Left "malformed string literal in command arguments"

splitHead :: String -> (String, String)
splitHead source = (token, trim arguments)
 where
  withoutLeading = dropWhile isSpace source
  (token, arguments) = break isSpace withoutLeading

stripKeyword :: String -> String -> Maybe String
stripKeyword keyword source
  | not $ keyword `isPrefixOf` source = Nothing
  | otherwise = case drop (length keyword) source of
      "" -> Just ""
      rest@(first : _)
        | isSpace first -> Just $ dropWhile isSpace rest
        | otherwise -> Nothing
