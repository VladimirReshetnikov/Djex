{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
--
-- Copyright (c) 2005 Lennart Augustsson
-- See LICENSE for licensing details.
--
module Djinn(main) where
import Data.Char(isAlpha, isDigit, isSpace)
import Data.List(isPrefixOf, nub, sortOn, intercalate)
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
import HCheck(htCheckEnv, htCheckType, htCheckTypeKind, htInferClassKinds)
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
    repl_init = welcome state,
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
    cutOff :: Int,
    -- Search-step budget; 0 keeps the search an unlimited decision procedure.
    budget :: Integer
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
    cutOff = 200,
    budget = 0
    }
 where syns = either (error . ("Bad initial environment: " ++)) id $ htCheckEnv $ reverse [
        ("()",     rawType []        (HTUnion [("()",[])])),
        ("Either", rawType ["a","b"] (HTUnion [("Left", [a]), ("Right", [b])])),
        ("Maybe",  rawType ["a"]     (HTUnion [("Nothing", []), ("Just", [a])])),
        ("Bool",   rawType []        (HTUnion [("False", []), ("True", [])])),
        ("Void",   rawType []        (HTUnion [])),
        ("Not",    rawType ["x"]     (htNot "x"))
        ]
       clss = map addParamKinds
              [("Eq", (["a"], [("==", a `HTArrow` (a `HTArrow` HTCon "Bool"))])),
               ("Monad", (["m"], [("return", a `HTArrow` ma),
                                  (">>=", ma `HTArrow` ((a `HTArrow` mb) `HTArrow` mb))]))
              ] where ma = HTApp m a; mb = HTApp m b
       addParamKinds (name, (params, methods)) =
           either (error . (("Bad initial class " ++ name ++ ": ") ++))
               (\ kinds -> (name, (kinds, methods)))
               (htInferClassKinds syns params (map snd methods))
       a = HTVar "a"
       b = HTVar "b"
       m = HTVar "m"


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

type Context = (HSymbol, [HType])

-- A stored class carries the kind of each parameter, inferred from the
-- method types at declaration time (Haskell98 style, with unconstrained
-- parameters defaulting to *).  The parser produces the raw form; kinds
-- are attached before a class enters the state.
type ClassDef = (HSymbol, ([(HSymbol, HKind)], [Method]))
type RawClassDef = (HSymbol, ([HSymbol], [Method]))

-- htCheckEnv replaces this placeholder with the definition's inferred kind.
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
runCmd s (Add i t) =
    case htCheckType (synonyms s) t of
    Left msg -> do putStrLn $ "Error: " ++ msg; return (False, s)
    Right _ -> return (False, s { axioms = replace i (i, t) (axioms s) })
runCmd _ Clear =
    return (False, startState)
runCmd s (Del i)
    | i `notElem` (map fst (axioms s) ++ map fst (synonyms s) ++
                   map fst (classes s)) = do
        putStrLn $ "Error: cannot delete " ++ i ++ ": it is not defined"
        return (False, s)
runCmd s (Del i) = do
    let candidateAxioms = filter ((i /=) . fst) (axioms s)
        candidateSynonyms = filter ((i /=) . fst) (synonyms s)
        candidateClasses = filter ((i /=) . fst) (classes s)
    case validateEnvironment candidateSynonyms candidateAxioms
            candidateClasses of
        Left message -> do
            putStrLn $ "Error: cannot delete " ++ i ++ ": " ++ message
            return (False, s)
        Right (checkedSynonyms, refreshedClasses) ->
            return (False, s {
                axioms = candidateAxioms,
                synonyms = checkedSynonyms,
                classes = refreshedClasses
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
        Right (syns, refreshedClasses) ->
            return (False, s { synonyms = syns, classes = refreshedClasses })
runCmd s (Set f) =
    return (False, f s)
runCmd s (Query i ctx g) =
    query True s i ctx g
runCmd s (Class (name, (params, methods))) =
    case requireUnusedName "type" name (synonyms s) >>
         requireDistinct "class parameter" params >>
         requireDistinct "method" (map fst methods) >>
         checkMethodNames name methods (classes s) >>
         -- Kind inference also kind-checks every method type.
         htInferClassKinds (synonyms s) params (map snd methods) of
        Left msg -> do putStrLn $ "Error: " ++ msg; return (False, s)
        Right kinds ->
            return (False, s {
                classes = replace name (name, (kinds, methods)) (classes s)
                })
runCmd s (QueryInstance ctx cls ts) =
    case do
        methods <- ctxLookup s (cls, ts)
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
        mode = (defaultSearchMode (multi s || sorted s)) {
            searchBudget = if budget s > 0 then Just (budget s) else Nothing
            }
        outcome = proveWithMode mode internalEnv form
    when (debug s) $ putStrLn ("*** " ++ show form)
    case searchProofs outcome of
        [] -> do
            -- A budgeted search that ran out of steps has not decided
            -- anything; only a finished search justifies "cannot".
            if searchExhausted outcome then
                putStrLn $ "-- " ++ i ++ ": no proof found within budget " ++
                    show (budget s) ++ "; inhabitation is undecided."
             else
                putStrLn $ "-- " ++ i ++ cannotBeRealized proofEnv
            return (False, s)
        p:ps -> do
            let internalProofs = p : take (cutOff s - 1) ps
                labeled what = either (Left . ((what ++ ": ") ++)) Right
                -- Every bounded candidate must check against the requested
                -- formula before display names are restored and it is
                -- converted to a Haskell clause.
                renderedProofs = do
                    labeled "generated an invalid proof" $
                        mapM_ (checkProof internalEnv form) internalProofs
                    labeled "cannot render generated proof" $
                        mapM (termToHClause i . restoreProofTerm proofEnv)
                            internalProofs
            case renderedProofs of
              Left message -> putStrLn $ "Error: " ++ message
              Right rendered -> do
                -- Rank a clause by the fraction of its binders that are
                -- unused, then by the total binder count: solutions that use
                -- more of their arguments are usually the intended ones.
                let score clause =
                       let bvs = getBinderVars clause
                           r = if null bvs then (0, 0) else (length (filter (== "_") bvs) % length bvs, length bvs)
                       in  (r, clause)
                    clauses = nub $
                            if sorted s then
                                map snd $ sortOn fst $ map score rendered
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
showClass (c, (as, ms)) =
    "class " ++ showContext (c, map (HTVar . fst) as) ++ " where " ++
        intercalate "; " (map sm ms)
  where sm (i, t) = prHSymbolOp i ++ " :: " ++ show t

showContext :: Context -> String
showContext (c, as) = show $ foldl HTApp (HTCon c) as

showContexts :: [Context] -> String
showContexts [] = ""
showContexts cs = "(" ++ intercalate ", " (map showContext cs) ++ ")"

-- Look up a class use, requiring exact arity and that every type argument
-- fits the kind inferred for its parameter.  A bare type variable fits any
-- parameter kind; an ill-kinded or kind-mismatched application is rejected
-- even for classes whose method list provides nothing else to check.
ctxLookup :: State -> Context -> Either String [Method]
ctxLookup s (c, as) =
    case lookup c (classes s) of
        Nothing -> Left $ "Class not found: " ++ c
        Just (ps, ms)
            | length ps == length as -> do
                sequence_
                    [ withArgument argument $
                        htCheckTypeKind (synonyms s) kind argument
                    | ((_, kind), argument) <- zip ps as ]
                Right [(m, substHT (zip (map fst ps) as) t) | (m, t) <- ms]
            | otherwise -> Left $
                "Class " ++ c ++ " expects " ++ show (length ps) ++
                " type argument(s), but got " ++ show (length as)
  where
    withArgument argument = either
        (Left . (("argument " ++ show argument ++ " of class " ++ c ++
            ": ") ++))
        Right

checkContexts :: State -> [Context] -> Either String [[Method]]
checkContexts s ctx = do
    methods <- mapM (ctxLookup s) ctx
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
