--
-- Copyright (c) 2005 Lennart Augustsson
-- See LICENSE for licensing details.
--
module Djinn(main) where
import Data.Char(isAlpha, isDigit, isSpace)
import Data.List(isPrefixOf, intercalate)
import Data.Version(showVersion)
import Text.ParserCombinators.ReadP
import Control.Monad(when)
import System.Exit(exitWith, ExitCode(..))
import System.Environment(getArgs)
import System.IO.Error(tryIOError)

import Djinn.Core
import Djinn.Internal.REPL
import Djinn.Internal.HTypes
import Djinn.Internal.HIdentifier
import Djinn.Internal.Help
import qualified Paths_djinn

version :: String
version = "version " ++ showVersion Paths_djinn.version

main :: IO ()
main = do
    args <- getArgs
    let decodeOptions (('-':cs) : as) st = decodeOption cs >>= \f -> decodeOptions as (f False st)
        decodeOptions (('+':cs) : as) st = decodeOption cs >>= \f -> decodeOptions as (f True  st)
        decodeOptions as st = return (as, st)
        decodeOption cs = case [ set | (cmd, _, _, set) <- options, isPrefix cs cmd ] of
                          [] -> do usage; exitWith (ExitFailure 1)
                          set : _ -> return set
    (args', state) <- decodeOptions args startState
    case args' of
        [] -> repl (hsGenRepl state)
        _ -> loop state args'
              where loop _ [] = return ()
                    loop s (a:as) = do
                        putStrLn $ "-- loading file " ++ a
                        (q, s') <- loadFile s a
                        if q then
                            return ()
                         else
                            loop s' as

usage :: IO ()
usage = putStrLn "Usage: djinn [option ...] [file ...]"

hsGenRepl :: State -> REPL State
hsGenRepl state = REPL {
    repl_init = welcome state,
    repl_eval = eval,
    repl_exit = exit
    }

data State = State {
    environment :: Environment,
    multi :: Bool,
    sorted :: Bool,
    debug :: Bool,
    cutOff :: Int,
    -- Search-step budget; 0 keeps the search an unlimited decision procedure.
    budget :: Integer
    }

startState :: State
startState = State {
    environment = standardEnvironment,
    multi = False,
    sorted = True,
    debug = False,
    cutOff = 200,
    budget = 0
    }


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
            return (False, s)
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
           QueryInstance [Context] HSymbol [HType]

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
runCmd _ Clear =
    return (False, startState)
runCmd s (Del i) =
    case removeDeclaration i (environment s) of
        Left message -> do
            putStrLn $ "Error: cannot delete " ++ i ++ ": " ++ message
            return (False, s)
        Right environment' ->
            return (False, s { environment = environment' })
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
runCmd s (QueryInstance ctx cls ts) =
    case resolveInstanceMethods (environment s) ctx (cls, ts) of
        Left msg -> do putStrLn $ "Error: " ++ msg; return (False, s)
        Right methods -> do
            let instanceHeading = "instance " ++ contextPrefix ctx ++
                    show (foldl HTApp (HTCon cls) ts)
                prepareMethod method@(name, goal) =
                    case makeQueryReport s name ctx goal of
                        Left message -> Left $
                            "cannot generate method " ++ prHSymbolOp name ++
                            ": " ++ message
                        Right report -> Right (method, report)
                methodRealized (_, report) =
                    case reportOutcome report of
                        Realized (_ : _) -> True
                        _ -> False
                printMethod ((name, goal), report) = do
                    putStr "   "
                    printQueryReport False s name ctx goal report
                printFailedMethod ((name, goal), report) =
                    printQueryReport False s name ctx goal report
            case mapM prepareMethod methods of
                Left msg -> do
                    putStrLn $ "Error: " ++ msg
                    return (False, s)
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
            return (False, s)
        Right environment' ->
            return (False, s { environment = environment' })

query :: Bool -> State -> String -> [Context] -> HType -> IO (Bool, State)
query prType s i ctx g = do
    case makeQueryReport s i ctx g of
        Left msg -> putStrLn $ "Error: " ++ msg
        Right report -> printQueryReport prType s i ctx g report
    return (False, s)

makeQueryReport :: State -> String -> [Context] -> HType
                -> Either String QueryReport
makeQueryReport s name contexts goal =
    inhabit queryOptions (environment s) contexts name goal
  where queryOptions = QueryOptions {
        optionAlternatives = multi s,
        optionSorted = sorted s,
        optionCutoff = cutOff s,
        optionBudget = if budget s > 0 then Just (budget s) else Nothing
        }

printQueryReport :: Bool -> State -> String -> [Context] -> HType
                 -> QueryReport -> IO ()
printQueryReport prType s name ctx goal report = do
    when (debug s) $ putStrLn ("*** " ++ reportFormula report)
    case reportOutcome report of
        Undecided ->
            -- A budgeted search that ran out of steps has not decided
            -- anything; only a finished search justifies "cannot".
            putStrLn $ "-- " ++ name ++
                ": no proof found within budget " ++
                show (budget s) ++ "; inhabitation is undecided."
        UnrealizableWithoutSelfReference ->
            putStrLn $ "-- " ++ name ++ " cannot be safely realized \
                \without a recursive self-reference."
        Unrealizable ->
            putStrLn $ "-- " ++ name ++ " cannot be realized."
        Realized clauses -> do
            when (debug s) $
                mapM_ (putStrLn . ("+++ " ++)) (reportProof report)
            when prType $ putStrLn $
                prHSymbolOp name ++ " :: " ++ contextPrefix ctx ++ show goal
            case clauses of
                [] -> return ()
                clause : alternatives -> do
                    putStrLn clause
                    when (multi s) $ mapM_
                        (\ alternative ->
                            putStrLn "-- or" >> putStrLn alternative)
                        alternatives

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
            return (False, s)
        Right result' -> return result'

showClass :: (HSymbol, ([(HSymbol, HKind)], [Method])) -> String
showClass (c, (as, ms)) =
    "class " ++ showContext (c, map (HTVar . fst) as) ++ " where " ++
        intercalate "; " (map sm ms)
  where sm (i, t) = prHSymbolOp i ++ " :: " ++ show t

showContext :: Context -> String
showContext (c, as) = show $ foldl HTApp (HTCon c) as

showContexts :: [Context] -> String
showContexts [] = ""
showContexts cs = "(" ++ intercalate ", " (map showContext cs) ++ ")"

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
    c <- option [] pContext
    t <- pHType
    optional $ schar ';'
    return $ Query i c t

pQuery' :: ReadP Cmd
pQuery' = do
    schar '?'
    i <- pLocalTermName
    sstring "::"
    c <- option [] pContext
    t <- pHType
    optional $ schar ';'
    return $ Query i c t

pQueryInstance :: ReadP Cmd
pQueryInstance = do
    schar '?'
    sstring "instance"
    c <- option [] pContext
    cls <- pHSymbol True
    ts <- many pHTAtom
    optional $ schar ';'
    return $ QueryInstance c cls ts

pContext :: ReadP [Context]
pContext = do
    let pCtx = do c <- pHSymbol True; ts <- many pHTAtom; return (c, ts)
    ctx <-
        do
          schar '('
          ctx <- sepBy1 pCtx (schar ',')
          schar ')'
          return ctx
       +++
        do
          ctx <- pCtx
          return [ctx]
    sstring "=>"
    return ctx

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
