{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
--
-- Copyright (c) 2005 Lennart Augustsson
-- See LICENSE for licensing details.
--
module Djinn(main) where
import Data.Char(isAlpha, isSpace)
import Data.List(sortBy, nub, intercalate)
import Data.Ratio((%))
import Data.Version(showVersion)
import Text.ParserCombinators.ReadP
import Control.Monad(when)
import System.Exit(exitWith, ExitCode(..))
import System.Environment(getArgs)
import System.IO.Error(tryIOError)

import REPL
import Environment(validateEnvironment)
import LJT
import HTypes
import HIdentifier
import HCheck(htCheckEnv, htCheckType)
import Help
import ProofCheck(checkProof)
import ProofEnv
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
    repl_init = inIt state,
    repl_eval = eval,
    repl_exit = exit
    }

data State = State {
    synonyms :: [(HSymbol, ([HSymbol], HType, HKind))],
    axioms :: [(HSymbol, HType)],
    classes :: [ClassDef],
    multi :: Bool,
    sorted :: Bool,
    debug :: Bool,
    cutOff :: Int
    }
    deriving (Show)

startState :: State
startState = State {
    synonyms = syns,
    classes = clss,
    axioms = [],
    multi = False,
    sorted = True,
    debug = False,
    cutOff = 200
    }
 where syns = either (error . ("Bad initial environment: " ++)) id $ htCheckEnv $ reverse [
        ("()",     rawType []        (HTUnion [("()",[])])),
        ("Either", rawType ["a","b"] (HTUnion [("Left", [a]), ("Right", [b])])),
        ("Maybe",  rawType ["a"]     (HTUnion [("Nothing", []), ("Just", [a])])),
        ("Bool",   rawType []        (HTUnion [("False", []), ("True", [])])),
        ("Void",   rawType []        (HTUnion [])),
        ("Not",    rawType ["x"]     (htNot "x"))
        ]
       clss = [("Eq", (["a"], [("==", a `HTArrow` (a `HTArrow` HTCon "Bool"))])),
               ("Monad", (["m"], [("return", a `HTArrow` ma),
                                  (">>=", ma `HTArrow` ((a `HTArrow` mb) `HTArrow` mb))]))
              ] where ma = HTApp m a; mb = HTApp m b
       a = HTVar "a"
       b = HTVar "b"
       m = HTVar "m"


inIt :: State -> IO (String, State)
inIt state = do
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

type Context = (HSymbol, [HType])
type ClassDef = (HSymbol, ([HSymbol], [Method]))

-- htCheckEnv replaces this placeholder with the definition's inferred kind.
rawType :: [HSymbol] -> HType -> ([HSymbol], HType, HKind)
rawType params body = (params, body, KStar)

data Cmd = Help Bool | Quit | Add HSymbol HType | Query HSymbol [Context] HType | Del HSymbol | Load HSymbol | Noop | Env |
           Type (HSymbol, ([HSymbol], HType, HKind)) | Set (State -> State) | Clear | Class ClassDef |
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
isPrefix p s = not (null p) && length p <= length s && take (length p) s == p

runCmd :: State -> Cmd -> IO (Bool, State)
runCmd s Noop = return (False, s)
runCmd s (Help verbose) = do
    putStr $ helpText ++ unlines (map getHelp commands) ++ getSettings s
    when verbose $ putStr verboseHelp
    return (False, s)
runCmd s Quit =
    return (True, s)
runCmd s (Load f) = loadFile s f
runCmd s (Add i t) =
    case htCheckType (synonyms s) t of
    Left msg -> do putStrLn $ "Error: " ++ msg; return (False, s)
    Right _ -> return (False, s { axioms = replace i (i, t) (axioms s) })
runCmd _ Clear =
    return (False, startState)
runCmd s (Del i) = do
    let candidateAxioms = filter ((i /=) . fst) (axioms s)
        candidateSynonyms = filter ((i /=) . fst) (synonyms s)
        candidateClasses = filter ((i /=) . fst) (classes s)
    case validateEnvironment candidateSynonyms candidateAxioms
            candidateClasses of
        Left message -> do
            putStrLn $ "Error: cannot delete " ++ i ++ ": " ++ message
            return (False, s)
        Right checked ->
            return (False, s {
                axioms = candidateAxioms,
                synonyms = checked,
                classes = candidateClasses
                })
runCmd s Env = do
    let showType (i, (_, HTAbstract _ kind, _)) =
            "type " ++ i ++ " :: " ++ show kind
        showType (i, (vs, t, _)) =
            tname t ++ " " ++ unwords (i:vs) ++ showd t
        tname t = if isHTUnion t then "data" else "type"
        showd (HTUnion []) = ""
        showd t = " = " ++ show t
    mapM_ (putStrLn . showType) (reverse $ synonyms s)
    mapM_ (\ (i, t) -> putStrLn $ prHSymbolOp i ++ " :: " ++ show t) (reverse $ axioms s)
    mapM_ (putStrLn . showClass) (reverse $ classes s)
    return (False, s)
runCmd s (Type syn@(name, (params, body, _))) =
    case requireUnusedName "class" name (classes s) >>
         requireDistinct "type parameter" params >>
         checkConstructors name body (synonyms s) >>
         validateEnvironment (replace name syn (synonyms s))
            (axioms s) (classes s) of
        Left msg -> do putStrLn $ "Error: " ++ msg; return (False, s)
        Right syns -> return (False, s { synonyms = syns })
runCmd s (Set f) =
    return (False, f s)
runCmd s (Query i ctx g) =
    query True s i ctx g
runCmd s (Class c@(name, (params, methods))) =
    case requireUnusedName "type" name (synonyms s) >>
         requireDistinct "class parameter" params >>
         requireDistinct "method" (map fst methods) >>
         checkMethodNames name methods (classes s) >>
         checkMethods s methods of
        Left msg -> do putStrLn $ "Error: " ++ msg; return (False, s)
        Right _ -> return (False, s { classes = replace name c (classes s) })
runCmd s (QueryInstance ctx cls ts) =
    case do
        methods <- ctxLookup (classes s) (cls, ts)
        _ <- checkContexts s ctx
        checkMethods s methods
        return methods
      of
        Left msg -> do putStrLn $ "Error: " ++ msg; return (False, s)
        Right methods -> do
            let sctx = if null ctx then "" else showContexts ctx ++ " => "
                method (i, t) = do
                    putStr "   "
                    query False s i ctx t
            putStrLn $ "instance " ++ sctx ++ show (foldl HTApp (HTCon cls) ts) ++ " where"
            mapM_ method methods
            return (False, s)

query :: Bool -> State -> String -> [Context] -> HType -> IO (Bool, State)
query prType s i ctx g =
   case htCheckType (synonyms s) g >> checkContexts s ctx of
   Left msg -> do putStrLn $ "Error: " ++ msg; return (False, s)
   Right mss -> do
    let form = hTypeToFormula (synonyms s) g
        externalEnv = [ (Symbol v, hTypeToFormula (synonyms s) t) | (v, t) <- axioms s ] ++ ctxEnv
        ctxEnv = [ (Symbol v, hTypeToFormula (synonyms s) t) | ms <- mss, (v, t) <- ms ]
        proofEnv = prepareProofEnvironment (Symbol i) externalEnv
        internalEnv = proofBindings proofEnv
        mpr = prove (multi s || sorted s) internalEnv form
    when (debug s) $ putStrLn ("*** " ++ show form)
    case mpr of
        [] -> do
            putStrLn $ "-- " ++ i ++ cannotBeRealized proofEnv
            return (False, s)
        p:ps -> do
            let internalProofs = p : take (cutOff s - 1) ps
            case mapM (checkProof internalEnv form) internalProofs of
              Left message ->
                putStrLn $ "Error: generated an invalid proof: " ++ message
              Right _ -> do
                let proofs = map (restoreProofTerm proofEnv) internalProofs
                case mapM (termToHClause i) proofs of
                  Left message ->
                    putStrLn $ "Error: cannot render generated proof: " ++
                        message
                  Right rendered -> do
                    let score clause =
                           let bvs = getBinderVars clause
                               r = if null bvs then (0, 0) else (length (filter (== "_") bvs) % length bvs, length bvs)
                           in  (r, clause)
                        clauses = nub $
                                if sorted s then
                                    map snd $ sortBy (\ (x,_) (y,_) -> compare x y) $ map score rendered
                                else
                                    rendered
                        pr = putStrLn . hPrClause
                        sctx = if null ctx then "" else showContexts ctx ++ " => "
                    when (debug s) $ putStrLn ("+++ " ++ show p)
                    when prType $ putStrLn $ prHSymbolOp i ++ " :: " ++ sctx ++ show g
                    case clauses of
                        [] -> return () -- rendered is non-empty.
                        e:es -> do
                            pr e
                            when (multi s) $
                                mapM_ (\ x -> putStrLn "-- or" >> pr x) es
            return (False, s)

cannotBeRealized :: ProofEnvironment -> String
cannotBeRealized environment
    | targetWasExcluded environment =
        " cannot be safely realized without a recursive self-reference."
    | otherwise = " cannot be realized."

loadFile :: State -> String -> IO (Bool, State)
loadFile s name = do
    result <- tryIOError $ do
        file <- readFile name
        evalCmds s $ lines $ stripComments file
    case result of
        Left err -> do
            putStrLn $ "Error loading " ++ show name ++ ": " ++ show err
            return (False, s)
        Right result' -> return result'

stripComments :: String -> String
stripComments "" = ""
stripComments ('-':'-':cs) = skip cs
  where skip "" = ""
        skip s@('\n':_) = stripComments s
        skip (_:s) = skip s
stripComments (c:cs) = c : stripComments cs

showClass :: ClassDef -> String
showClass (c, (as, ms)) = "class " ++ showContext (c, map HTVar as) ++ " where " ++ intercalate "; " (map sm ms)
  where sm (i, t) = prHSymbolOp i ++ " :: " ++ show t

showContext :: Context -> String
showContext (c, as) = show $ foldl HTApp (HTCon c) as

showContexts :: [Context] -> String
showContexts [] = ""
showContexts cs = "(" ++ intercalate ", " (map showContext cs) ++ ")"

ctxLookup :: [ClassDef] -> Context -> Either String [Method]
ctxLookup clss (c, as) =
    case lookup c clss of
        Nothing -> Left $ "Class not found: " ++ c
        Just (ps, ms)
            | length ps == length as ->
                Right [(m, substHT (zip ps as) t) | (m, t) <- ms]
            | otherwise -> Left $
                "Class " ++ c ++ " expects " ++ show (length ps) ++
                " type argument(s), but got " ++ show (length as)

checkContexts :: State -> [Context] -> Either String [[Method]]
checkContexts s ctx = do
    methods <- mapM (ctxLookup (classes s)) ctx
    mapM_ (checkMethods s) methods
    return methods

checkMethods :: State -> [Method] -> Either String ()
checkMethods s methods =
    htCheckType (synonyms s) (HTTuple (map snd methods))

checkMethodNames :: HSymbol -> [Method] -> [ClassDef] -> Either String ()
checkMethodNames owner methods definitions =
    case [(method, className)
            | (className, (_, existing)) <- definitions
            , className /= owner
            , (method, _) <- existing
            , method `elem` names] of
        [] -> Right ()
        (method, className):_ -> Left $
            "Method " ++ prHSymbolOp method ++
            " is already defined by class " ++ className
  where names = map fst methods

checkConstructors :: HSymbol
                  -> HType
                  -> [(HSymbol, ([HSymbol], HType, HKind))]
                  -> Either String ()
checkConstructors owner (HTUnion constructors) definitions = do
    requireDistinct "data constructor" names
    case [(constructor, typeName)
            | (typeName, (_, HTUnion existing, _)) <- definitions
            , typeName /= owner
            , (constructor, _) <- existing
            , constructor `elem` names] of
        [] -> Right ()
        (constructor, typeName):_ -> Left $
            "Data constructor " ++ constructor ++
            " is already defined by " ++ typeName
  where names = map fst constructors
checkConstructors _ _ _ = Right ()

requireDistinct :: String -> [HSymbol] -> Either String ()
requireDistinct what names =
    case [name | name <- nub names, length (filter (== name) names) > 1] of
        [] -> Right ()
        name:_ -> Left $ "Duplicate " ++ what ++ ": " ++ name

requireUnusedName :: String -> HSymbol -> [(HSymbol, a)] -> Either String ()
requireUnusedName existingKind name definitions
    | name `elem` map fst definitions =
        Left $ name ++ " is already defined as a " ++ existingKind
    | otherwise = Right ()

replace :: HSymbol -> (HSymbol, a) -> [(HSymbol, a)] -> [(HSymbol, a)]
replace name binding = (binding :) . filter ((/= name) . fst)

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

pDel :: ReadP Cmd
pDel = do
    s <- pConId +++ pLocalTermName
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
       schar ':'; char ':'
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
pSetVal = do
    pPrefix "cutoff"
    schar '='
    digits <- many1 (satisfy (`elem` ['0'..'9']))
    let n = read digits :: Integer
    if n > 0 && n <= toInteger (maxBound :: Int) then
        return $ Set $ \ s -> s { cutOff = fromInteger n }
     else
        pfail

schar :: Char -> ReadP ()
schar c = do
    skipSpaces
    char c
    return ()

sstring :: String -> ReadP ()
sstring s = do
    skipSpaces
    string s
    return ()

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
    [ "    cutoff=" ++ show (cutOff s) ++ " maximum number of solutions generated" ]
