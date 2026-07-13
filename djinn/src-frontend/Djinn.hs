--
-- Copyright (c) 2005 Lennart Augustsson
-- See LICENSE for licensing details.
--
module Djinn (main) where
import Data.Char(isAlpha, isDigit, isSpace)
import Data.List(isPrefixOf, intercalate)
import Data.Version(showVersion)
import Text.ParserCombinators.ReadP
import Control.Monad(when)
import System.Exit(exitWith, ExitCode(..))
import System.Environment(getArgs)
import System.IO(hPutStrLn, stderr)
import System.IO.Error(tryIOError)

import Djinn.Core
import Djinn.Internal.REPL
import Djinn.Internal.HTypes
import Djinn.Internal.HIdentifier
import Djinn.Internal.Help
import Language.Haskell.Djex.Djinn
import Language.Haskell.Synthesis.Candidate (candidateOutput)
import qualified Language.Haskell.Synthesis.Diagnostic as Diagnostic
import qualified Language.Haskell.Synthesis.Generated as Generated
import qualified Language.Haskell.Synthesis.Name as SharedName
import Language.Haskell.Synthesis.Query
import qualified Language.Haskell.Synthesis.Search as SharedSearch
import qualified Paths_djex

version :: String
version = "version " ++ showVersion Paths_djex.version

main :: IO ()
main = do
    args <- getArgs
    (files, state) <- case decodeArguments args of
        Left message -> do
            hPutStrLn stderr message
            usage
            exitWith $ ExitFailure 1
        Right result -> return result
    case files of
        [] -> repl (hsGenRepl state)
        _ -> do
            finalState <- loadFiles state files
            when (commandFailed finalState) $
                exitWith $ ExitFailure 1

decodeArguments :: [String] -> Either String ([FilePath], State)
decodeArguments = go startState []
  where
    go state reversed [] = Right (reverse reversed, state)
    go state reversed ("--" : remaining) =
        Right (reverse reversed ++ remaining, state)
    go state reversed (argument : remaining) = case argument of
        '-' : name | not (null name) -> applyOption False name
        '+' : name | not (null name) -> applyOption True name
        _ -> go state (argument : reversed) remaining
      where
        applyOption enabled name = do
            setter <- decodeOption name
            go (setter enabled state) reversed remaining

    decodeOption name = case exact of
        [setter] -> Right setter
        _ -> case prefixes of
            [] -> Left $ "Unknown Djinn option: " ++ name
            [setter] -> Right setter
            _ -> Left $ "Ambiguous Djinn option: " ++ name ++
                " (could be " ++ intercalate ", " matchingNames ++ ")"
      where
        exact = [setter | (command, _, _, setter) <- options,
            name == command]
        prefixes = [setter | (command, _, _, setter) <- options,
            name `isPrefixOf` command]
        matchingNames = [command | (command, _, _, _) <- options,
            name `isPrefixOf` command]

loadFiles :: State -> [FilePath] -> IO State
loadFiles state [] = return state
loadFiles state (file : remaining) = do
    putStrLn $ "-- loading file " ++ file
    (quit, state') <- loadFile state file
    if quit then return state' else loadFiles state' remaining

usage :: IO ()
usage = hPutStrLn stderr "Usage: djinn [option ...] [file ...]"

hsGenRepl :: State -> REPL State
hsGenRepl state = REPL {
    repl_init = welcome state,
    repl_eval = eval,
    repl_exit = exit
    }

data State = State {
    -- These two fields are committed together by 'installEnvironment'. The
    -- opaque session must always be the lowering of this exact editable REPL
    -- environment; otherwise a declaration can print successfully while later
    -- queries search a stale inventory.
    environment :: Environment,
    djinnSession :: DjinnSession,
    multi :: Bool,
    sorted :: Bool,
    debug :: Bool,
    cutOff :: Int,
    -- Search-step budget; 0 keeps the search an unlimited decision procedure.
    budget :: Integer,
    commandFailed :: Bool
    }

startState :: State
startState = State {
    environment = standardEnvironment,
    djinnSession = standardSession,
    multi = False,
    sorted = True,
    debug = False,
    cutOff = 200,
    budget = 0,
    commandFailed = False
    }

standardSession :: DjinnSession
standardSession = case sealCompatibilityEnvironment standardEnvironment of
    Right session -> session
    Left failure -> error $ "invalid standard Djinn environment: " ++
        failure


welcome :: State -> IO (String, State)
welcome state = do
    putStrLn $ "Welcome to Djinn " ++ version ++ "."
    putStrLn $ "Type :h to get help."
    return ("Djinn> ", state)

eval :: State -> String -> IO (Bool, State)
eval s line =
    case filter (null . snd) (readP_to_S pCmd line) of
        [] -> do
            putStrLn "Cannot parse command"
            return (False, markFailed s)
        (cmd, _) : _ -> runCmd s cmd

exit :: State -> IO ()
exit _ = putStrLn "Bye."

-- The raw parser output; Djinn.Core.declare attaches inferred kinds and
-- validates before anything enters the environment.
type RawClassDef = (HSymbol, ([HSymbol], [Method]))

-- Kind checking replaces this placeholder with the inferred kind.
rawType :: [HSymbol] -> HType -> ([HSymbol], HType, HKind)
rawType params body = (params, body, KStar)

data Cmd = Help Bool | Quit | Add HSymbol HType | Query HSymbol [Context] HType | Del HSymbol | Load HSymbol | Noop | Env |
           Type (HSymbol, ([HSymbol], HType, HKind)) | Set (State -> State) | Clear | Class RawClassDef |
           QueryInstance [Context] Context

pCmd :: ReadP Cmd
pCmd = do
    skipSpaces
    let adds (':':s) p = do schar ':'; pPrefix (takeWhile (/= ' ') s); c <- p; skipSpaces; return c
        adds _ p = do c <- p; skipSpaces; return c
    cmd <- foldr1 (+++) [adds s p | (s, p) <- commandParsers]
    skipSpaces
    return cmd

pPrefix :: String -> ReadP ()
pPrefix s = do
    skipSpaces
    cs <- look
    let w = takeWhile (\ c -> isAlpha c || c == '-') cs
    if isPrefix w s then
        string w >> return ()
     else
        pfail

isPrefix :: String -> String -> Bool
isPrefix p s = not (null p) && p `isPrefixOf` s

runCmd :: State -> Cmd -> IO (Bool, State)
runCmd s Noop = return (False, s)
runCmd s (Help verbose) = do
    putStr $ helpText ++ unlines (map getHelp commands) ++ getSettings s
    when verbose $ putStr verboseHelp
    return (False, s)
runCmd s Quit =
    return (True, s)
runCmd s (Load f) = loadFile s f
runCmd s (Add i t) = updateEnvironment s $ declare (Function i t)
runCmd s Clear =
    return (False, startState { commandFailed = commandFailed s })
runCmd s (Del i) =
    case removeDeclaration i (environment s) of
        Left message -> do
            putStrLn $ "Error: cannot delete " ++ i ++ ": " ++ message
            return (False, markFailed s)
        Right environment' -> installEnvironment s environment'
runCmd s Env = do
    let showType (i, (_, HTAbstract _ kind, _)) =
            "type " ++ i ++ " :: " ++ show kind
        showType (i, (vs, t, _)) =
            tname t ++ " " ++ unwords (i:vs) ++ showd t
        tname t = if isHTUnion t then "data" else "type"
        showd (HTUnion []) = ""
        showd t = " = " ++ show t
    mapM_ (putStrLn . showType)
        (reverse $ typeDeclarations $ environment s)
    mapM_ (\ (i, t) -> putStrLn $ prHSymbolOp i ++ " :: " ++ show t)
        (reverse $ functionDeclarations $ environment s)
    mapM_ (putStrLn . showClass)
        (reverse $ classDeclarations $ environment s)
    return (False, s)
runCmd s (Type (name, (params, body, _))) =
    updateEnvironment s $ declare $
        case body of
            HTUnion constructors -> DataType name params constructors
            HTAbstract _ kind -> AbstractType name kind
            _ -> TypeSynonym name params body
runCmd s (Set f) =
    return (False, f s)
runCmd s (Query i ctx g) =
    query True s i ctx g
runCmd s (Class (name, (params, methods))) =
    updateEnvironment s $ declare $ ClassDecl name params methods
runCmd s (QueryInstance ctx target) =
    case resolveInstanceMethods (environment s) ctx target of
        Left msg -> do
            putStrLn $ "Error: " ++ msg
            return (False, markFailed s)
        Right methods -> do
            let instanceHeading = "instance " ++ contextPrefix ctx ++
                    show target
                -- An instance needs one coherent body per method.  Ordinary
                -- query alternatives would become overlapping equations, and
                -- debug lines would split the layout block, so neither
                -- presentation option applies inside an instance declaration.
                methodPresentation = s { multi = False, debug = False }
                prepareMethod method@(name, goal) =
                    case makeDjinnResult methodPresentation name ctx goal of
                        Left failure -> Left $
                            "cannot generate method " ++ prHSymbolOp name ++
                            ": " ++ Diagnostic.renderDiagnostic failure
                        Right result -> case formatDjinnResult
                            False methodPresentation name ctx goal result of
                          Left message -> Left $
                            "cannot render method " ++ prHSymbolOp name ++
                            ": " ++ message
                          Right output -> Right (method, result, output)
                methodRealized (_, result, _) =
                    resultEvidence result == ValidatedCandidates
                -- The shared renderer may return a multi-line case expression
                -- as one string.  Prefix every physical line, not merely the
                -- first list element, to keep the complete method in the
                -- instance's layout block.
                printMethod (_, _, output) =
                    mapM_ (putStrLn . ("   " ++)) $ concatMap lines output
                printFailedMethod (_, _, output) = mapM_ putStrLn output
            case mapM prepareMethod methods of
                Left msg -> do
                    putStrLn $ "Error: " ++ msg
                    return (False, markFailed s)
                Right reports -> do
                    let failures = filter (not . methodRealized) reports
                    case failures of
                      [] -> do
                        putStrLn $ instanceHeading ++ " where"
                        mapM_ printMethod reports
                      _ -> do
                        -- Do not print an instance-shaped prefix unless every
                        -- method has a body.  A partial block is not useful
                        -- Haskell and can easily be mistaken for generated
                        -- code that merely needs a small edit.
                        putStrLn $ "-- cannot generate " ++ instanceHeading ++
                            ": one or more methods have no realization."
                        mapM_ printFailedMethod failures
                    return (False, s)

updateEnvironment :: State -> (Environment -> Either String Environment)
                  -> IO (Bool, State)
updateEnvironment s change =
    case change (environment s) of
        Left msg -> do
            putStrLn $ "Error: " ++ msg
            return (False, markFailed s)
        Right environment' -> installEnvironment s environment'

installEnvironment :: State -> Environment -> IO (Bool, State)
installEnvironment state environment' = case
    sealCompatibilityEnvironment environment' of
    Left failure -> do
        putStrLn $ "Error: " ++ failure
        return (False, markFailed state)
    Right session -> return (False, state {
        environment = environment',
        djinnSession = session
        })

-- Raw declarations remain convenient for the historical REPL, while the
-- reusable Djex session boundary consumes only the shared structural view.
sealCompatibilityEnvironment :: Environment -> Either String DjinnSession
sealCompatibilityEnvironment environment' = do
    shared <- either (Left . show) Right $ toSynthesisEnvironment environment'
    either (Left . Diagnostic.renderDiagnostic) Right $ mkDjinnSession shared

markFailed :: State -> State
markFailed state = state { commandFailed = True }

query :: Bool -> State -> String -> [Context] -> HType -> IO (Bool, State)
query prType s i ctx g = do
    case makeDjinnResult s i ctx g of
        Left failure -> do
            putStrLn $ "Error: " ++ commandDiagnostic failure
            return (False, markFailed s)
        Right result -> case formatDjinnResult prType s i ctx g result of
            Left message -> do
                putStrLn $ "Error: " ++ message
                return (False, markFailed s)
            Right output -> do
                mapM_ putStrLn output
                return (False, s)

makeDjinnResult :: State -> String -> [Context] -> HType
                -> Either Diagnostic.Diagnostic DjinnResult
makeDjinnResult s name contexts goal = do
    target <- case SharedName.parseName name of
        Left failure -> Left $ Diagnostic.contextualDiagnostic
            Diagnostic.Error "DJEX_DJINN_TARGET"
            "cannot convert the parsed Djinn target" (show failure)
        Right value -> Right value
    sharedGoal <- projectCompatibilityType "goal" goal
    sharedContexts <- traverse
        (traverse $ projectCompatibilityType "context argument") contexts
    request <- mkDjinnRequest QueryRequest {
        requestTarget = target,
        requestGoal = sharedGoal,
        requestContexts = sharedContexts,
        requestOptions = queryOptions
        }
    runDjinnQuery (djinnSession s) request
  where queryOptions = QueryOptions {
        optionAlternatives = multi s,
        optionSorted = sorted s,
        optionCutoff = cutOff s,
        optionBudget = if budget s > 0 then Just (budget s) else Nothing
        }

-- REPL parsers still produce the historical raw types. Their grammar lies in
-- the lossless shared subset, but keep the bridge checked so future grammar
-- extensions cannot smuggle a declaration-only node into the stable session.
projectCompatibilityType
    :: String -> HType -> Either Diagnostic.Diagnostic DjinnType
projectCompatibilityType role source = case toSynthesisType source of
    Left failure -> Left $ Diagnostic.contextualDiagnostic
        Diagnostic.Error "DJEX_DJINN_QUERY"
        "cannot project the parsed Djinn query type"
        (role ++ ": " ++ show failure)
    Right shared -> Right shared

formatDjinnResult :: Bool -> State -> String -> [Context] -> HType
                  -> DjinnResult -> Either String [String]
formatDjinnResult prType s name ctx goal result =
    fmap (debugFormula ++) $ case resultEvidence result of
      NoEvidence -> case SharedSearch.batchProgress search of
          SharedSearch.Completed (SharedSearch.Truncated _) ->
              Right ["-- " ++ name ++
                  ": no proof found within budget " ++
                  show (budget s) ++ "; inhabitation is undecided."]
          progress -> Left $ "Djinn returned no evidence after " ++ show progress
      RequiresTargetReference -> Right ["-- " ++ name ++
          " cannot be safely realized without a recursive self-reference."]
      ProvedUninhabitable ->
          Right ["-- " ++ name ++ " cannot be realized."]
      ValidatedCandidates -> do
          clauses <- mapM renderClause selectedCandidates
          case clauses of
            [] -> Left "Djinn returned candidate evidence without a candidate"
            firstClause : alternatives -> Right $
                debugProof ++ typeSignature ++ [firstClause] ++
                concat [["-- or", alternative] | alternative <- alternatives]
  where
    search = resultSearch result
    candidates = SharedSearch.batchCandidates search
    selectedCandidates
      | multi s = candidates
      | otherwise = take 1 candidates
    metadata = SharedSearch.batchMetadata search
    debugFormula
      | debug s = ["*** " ++ djinnTranslatedFormula metadata]
      | otherwise = []
    debugProof
      | debug s = maybe [] (\proof -> ["+++ " ++ proof]) $
          djinnFirstExploredProof metadata
      | otherwise = []
    typeSignature
      | prType = [prHSymbolOp name ++ " :: " ++
          contextPrefix ctx ++ show goal]
      | otherwise = []
    renderClause = either (Left . show) Right .
        Generated.renderFunctionClause (Generated.defaultRenderOptions id) .
        candidateOutput

-- The compatibility CLI promotes the innermost context to its traditional
-- @Error:@ headline. Remove that entry before rendering the structured body so
-- every context is still shown exactly once.
commandDiagnostic :: Diagnostic.Diagnostic -> String
commandDiagnostic failure = case reverse $ Diagnostic.diagnosticContext failure of
    [] -> Diagnostic.renderDiagnostic failure
    context : reversedOuterContexts ->
        let strippedFailure = failure
                { Diagnostic.diagnosticContext = reverse reversedOuterContexts }
        in context ++ "\n  " ++ Diagnostic.renderDiagnostic strippedFailure

contextPrefix :: [Context] -> String
contextPrefix [] = ""
contextPrefix contexts = showContexts contexts ++ " => "

loadFile :: State -> String -> IO (Bool, State)
loadFile s name = do
    result <- tryIOError $ do
        file <- readFile name
        evalCmds s $ lines $ stripLineComments file
    case result of
        Left err -> do
            putStrLn $ "Error loading " ++ show name ++ ": " ++ show err
            return (False, markFailed s)
        Right result' -> return result'

showClass :: (HSymbol, ([(HSymbol, HKind)], [Method])) -> String
showClass (c, (as, ms)) =
    "class " ++ show validatedHead ++ " where " ++
        intercalate "; " (map sm ms)
  where
    -- Class declarations have already passed the same name validation in the
    -- core.  Rebuilding their head through mkContext keeps the CLI renderer on
    -- the shared nominal representation without introducing another format.
    validatedHead =
        case mkContext c (map (HTVar . fst) as) of
            Right context -> context
            Left message -> error $ "Djinn.showClass: " ++ message
    sm (i, t) = prHSymbolOp i ++ " :: " ++ show t

showContexts :: [Context] -> String
showContexts [] = ""
showContexts cs = "(" ++ intercalate ", " (map show cs) ++ ")"

evalCmds :: State -> [String] -> IO (Bool, State)
evalCmds state [] = return (False, state)
evalCmds state (l:ls) = do
    qs@(q, state') <- eval state l
    if q then
        return qs
     else
        evalCmds state' ls

commands :: [(String, String, ReadP Cmd)]
commands = [
        (":clear",              "Clear environment and settings", return Clear),
        (":delete <sym>",       "Delete from environment.",     pDel),
        (":environment",        "Show environment",             return Env),
        (":help",               "Print this message.",          return (Help False)),
        (":load <file>",        "Load a file",                  pLoad),
        (":quit",               "Quit program.",                return Quit),
        (":set <option>",       "Set options",                  pSet),
        (":verbose-help",       "Print verbose help.",          return (Help True)),
        ("type <sym> <vars> = <type>", "Add a type synonym",    pType),
        ("data <sym> <vars> = <datatype>", "Add a data type",   pData),
        ("class <sym> <vars> where <methods>", "Add a class",   pClass),
        ("<sym> :: <type>",     "Add to environment",           pAdd),
        ("? <sym> :: <type>",   "Query",                        pQuery'),
        ("<sym> ? <type>",      "Query",                        pQuery),
        ("?instance <sym> <types>","Query instance",            pQueryInstance)
        ]

-- Keep accepting the historical camel-case spelling without advertising it.
commandParsers :: [(String, ReadP Cmd)]
commandParsers =
    [(name, parser) | (name, _, parser) <- commands] ++
    [(":verboseHelp", return (Help True)), ("", return Noop)]

options :: [(String, String, State->Bool, Bool->State->State)]
options = [
          ("multi",             "print multiple solutions",     multi,  \ v s -> s { multi  = v }),
          ("sorted",            "sort solutions",               sorted, \ v s -> s { sorted = v }),
          ("debug",             "debug mode",                   debug,  \ v s -> s { debug  = v })
          ]

getHelp :: (String, String, a) -> String
getHelp (cmd, help, _) = cmd ++ replicate (helpColumn - length cmd) ' ' ++ help

helpColumn :: Int
helpColumn = 2 + maximum [length cmd | (cmd, _, _) <- commands]

-- Accept anything that could have been declared: type and class names are
-- plain constructor identifiers, but axioms may carry a qualified name.
pDel :: ReadP Cmd
pDel = do
    s <- pConId +++ pExternalTermName
    return $ Del s

pLoad :: ReadP Cmd
pLoad = do
    skipSpaces
    s <- munch1 (not . isSpace)
    return $ Load s

pAdd :: ReadP Cmd
pAdd = do
    i <- pExternalTermName
    sstring "::"
    t <- pHType
    optional $ schar ';'
    return $ Add i t

pQuery :: ReadP Cmd
pQuery = do
    i <- pLocalTermName
    schar '?'
    c <- option [] pHContext
    t <- pHType
    optional $ schar ';'
    return $ Query i c t

pQuery' :: ReadP Cmd
pQuery' = do
    schar '?'
    i <- pLocalTermName
    sstring "::"
    c <- option [] pHContext
    t <- pHType
    optional $ schar ';'
    return $ Query i c t

pQueryInstance :: ReadP Cmd
pQueryInstance = do
    schar '?'
    sstring "instance"
    c <- option [] pHContext
    target <- pHConstraint
    optional $ schar ';'
    return $ QueryInstance c target

pType :: ReadP Cmd
pType = do
    sstring "type"
    syn <- pHSymbol True
    do args <- many (pHSymbol False)
       schar '='
       t <- pHType
       return $ Type (syn, rawType args t)
     +++
      do
       sstring "::"
       k <- pHKind
       return $ Type (syn, rawType [] (HTAbstract syn k))

pData :: ReadP Cmd
pData = do
    sstring "data"
    syn <- pHSymbol True
    args <- many (pHSymbol False)
    do schar '='
       t <- pHDataType
       case t of
           HTUnion [] -> pfail
           _ -> return $ Type (syn, rawType args t)
      +++
     do
       return $ Type (syn, rawType args (HTUnion []))

pClass :: ReadP Cmd
pClass = do
    sstring "class"
    cls <- pHSymbol True
    args <- many (pHSymbol False)
    sstring "where"
    mets <- sepBy pMethod (schar ';')
    return $ Class (cls, (args, mets))

type Method = (HSymbol, HType)

pMethod :: ReadP Method
pMethod = do
    i <- pLocalTermName
    sstring "::"
    t <- pHType
    return (i, t)

pLocalTermName :: ReadP HSymbol
pLocalTermName = pVarId +++ pParenthesizedVarOp

pExternalTermName :: ReadP HSymbol
pExternalTermName = pQualifiedVarId +++ pParenthesizedVarOp

pSet :: ReadP Cmd
pSet = pSetFlag +++ pSetVal

pSetFlag :: ReadP Cmd
pSetFlag = do
    val <- (do schar '+'; return True) +++ (do schar '-'; return False)
    f <- foldr (+++) pfail [ do pPrefix s; return (set val) | (s, _, _, set) <- options ]
    return $ Set $ f

pSetVal :: ReadP Cmd
pSetVal = pCutoff +++ pBudget
  where
    pCutoff = do
        n <- pNumericSetting "cutoff"
        if n > 0 && n <= toInteger (maxBound :: Int) then
            return $ Set $ \ s -> s { cutOff = fromInteger n }
         else
            pfail
    pBudget = do
        n <- pNumericSetting "budget"
        return $ Set $ \ s -> s { budget = n }

pNumericSetting :: String -> ReadP Integer
pNumericSetting name = do
    pPrefix name
    schar '='
    digits <- munch1 isDigit
    return (read digits)

helpText :: String
helpText = "\
\Djinn is a program that generates Haskell code from a type.\n\
\Given a type the program will deduce an expression of this type,\n\
\if one exists.  If the Djinn says the type is not realizable it is\n\
\because there is no (total) expression of the given type.\n\
\Djinn only knows about tuples, ->, and some data types in the\n\
\initial environment (use :environment for a list).\n\
\\n\
\Caveat emptor: Treat the generated expression as a candidate.  It may\n\
\need supporting declarations and still belongs in your compile/test loop.\n\
\\n\
\Send any comments and feedback to lennart@augustsson.net\n\
\\n\
\Commands (may be abbreviated):\n\
\"

getSettings :: State -> String
getSettings s = unlines $ [
    "",
    "Current settings" ] ++ [ "    " ++ (if gett s then "+" else "-") ++ name ++ replicate (10 - length name) ' ' ++ descr |
                              (name, descr, gett, _set) <- options ] ++
    [ "    cutoff=" ++ show (cutOff s) ++ " maximum number of solutions generated",
      "    budget=" ++ show (budget s) ++ " search-step budget, 0 = unlimited" ]
