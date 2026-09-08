{-# LANGUAGE ScopedTypeVariables #-}

-- | Query-owned GHC interpreter process. This is bounded execution, not a
-- sandbox or proof oracle. A failed scope never falls back to another scope.
module Language.Haskell.Djex.REPL.BehavioralWorker
  ( BehavioralOutcome (..), BehavioralContext, prepareBehavioralContext
  , withBehavioralEvaluator, behavioralWorkerArgument, runBehavioralWorker
  ) where

import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, evaluate, finally, mask, onException, try)
import Control.Monad (forM, unless, when)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Catch as Catch
import Data.Char (isAlphaNum, toLower)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import qualified Data.Set as Set
import qualified Language.Haskell.Interpreter as Hint
import qualified Language.Haskell.Interpreter.Unsafe as HintUnsafe
import System.Directory
  ( createDirectory, createDirectoryIfMissing, getTemporaryDirectory
  , removeDirectoryRecursive, removeFile )
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), (<.>), takeDirectory)
import System.IO
  ( Handle, IOMode (WriteMode), hClose, hFlush, hGetChar, hIsEOF, hPutStr
  , hPutStrLn, hSetEncoding, openTempFile, stdin, stdout, stderr, utf8, withFile )
import System.IO.Error (tryIOError)
import System.Process
  ( CreateProcess (..), StdStream (..), getProcessExitCode
  , proc, terminateProcess
  , withCreateProcess )
import System.Timeout (timeout)
import Text.Read (readMaybe)

import Language.Haskell.Djex.REPL.Eval (renderInterpreterError)
import Language.Haskell.Djex.REPL.Scope
  ( ReplScope, ScopeEntry (..), scopeEntries, parseScopeImport )
import qualified Language.Haskell.Exts.Pretty as HSE
import qualified Language.Haskell.Exts.Parser as HSE
import qualified Language.Haskell.Exts.Syntax as HSE
import Language.Haskell.Exference.TypeFromHaskellSrc (haskellSrcExtsParseMode)
import qualified Language.Haskell.Synthesis.Name as Name
import Language.Haskell.Synthesis.Behavioral (BehavioralQuery (..))

data BehavioralOutcome
  = BehavioralPassed
  | BehavioralFalse
  | BehavioralCompilationError String
  | BehavioralRuntimeError String
  | BehavioralTimedOut
  | BehavioralUnavailable String
  deriving (Eq, Read, Show)

-- Exact immutable sources plus the checked prompt import surface.
data BehavioralContext = BehavioralContext [(String, String)] [String] [String]
  deriving (Read, Show)

prepareBehavioralContext :: Maybe ReplScope -> [(String, String)] -> BehavioralContext
prepareBehavioralContext scope sources = BehavioralContext sources starred imports
 where
  entries = maybe [] scopeEntries scope
  starred = [Name.renderModuleName name | ScopeModule _ True name <- entries]
  imports =
    [case entry of
       ScopeModule _ _ name -> "import " ++ Name.renderModuleName name
       ScopeImport source -> source
    | entry <- entries, case entry of ScopeModule _ True _ -> False; _ -> True]

behavioralWorkerArgument :: String
behavioralWorkerArgument = "--internal-behavioral-worker"

-- | One child/session per query, one fresh expression per candidate. A failed
-- or timed-out child is retired, never restarted with a weaker scope. Startup
-- and each compile/evaluation have a 30-second bound; the REPL can cancel sooner.
withBehavioralEvaluator
  :: BehavioralContext
  -> ((BehavioralQuery -> IO BehavioralOutcome)
      -> (BehavioralQuery -> String -> IO BehavioralOutcome) -> IO a)
  -> IO a
withBehavioralEvaluator context action = do
  executable <- getExecutablePath
  let child = (proc executable [behavioralWorkerArgument])
        { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe
        , create_group = True, use_process_jobs = True, close_fds = True
        , delegate_ctlc = False }
  withCreateProcess child $ \input output errors process -> do
    let cleanup = do
          -- EOF lets an idle interpreter exit normally. Do not put a live
          -- waitForProcess under System.Timeout: that wait need not be
          -- interruptible on Windows. Poll the nonblocking status operation,
          -- then terminate the owned process/job if it is still running.
          _ <- tryIOError $ mapM_ hClose input
          stopped <- pollExit (30 :: Int)
          unless stopped abortCleanup
        -- An active Windows pipe operation may hold a Handle lock while
        -- ignoring asynchronous exceptions. Kill the child/job FIRST on an
        -- aborted exchange; EOF/broken pipe then releases its I/O task before
        -- withAsync tries to cancel and join it. In particular, do not close
        -- stdin first while a writer could still own that handle.
        abortCleanup = do
          _ <- tryIOError $ terminateProcess process
          _ <- pollExit 200
          pure ()
        pollExit :: Int -> IO Bool
        pollExit remaining = do
          status <- getProcessExitCode process
          case status of
            Just _ -> pure True
            Nothing | remaining <= 0 -> pure False
            Nothing -> threadDelay 10000 >> pollExit (remaining - 1)
        run inputHandle outputHandle errorHandle = do
          mapM_ (`hSetEncoding` utf8) [inputHandle, outputHandle, errorHandle]
          -- Drain compiler diagnostics without retaining an unbounded buffer.
          -- Close the owned process before withAsync cancels and joins the
          -- diagnostic reader. A Windows pipe read may wait for EOF despite
          -- asynchronous cancellation while the idle child awaits stdin.
          mask $ \restoreSession ->
           withAsync (restoreSession $ drain errorHandle) $ \_ ->
            flip finally cleanup $ restoreSession $ do
            retired <- newIORef Nothing
            let exchange request = do
                  previous <- readIORef retired
                  case previous of
                    Just verdict -> pure verdict
                    Nothing -> do
                      -- Keep both writing and reading off the querying
                      -- thread. Async.wait uses STM, so these deadlines and
                      -- the enclosing REPL deadline remain interruptible even
                      -- when a Windows pipe operation itself is not.
                      answer <- mask $ \restoreExchange -> withAsync (restoreExchange $ tryIOError $ do
                          hPutStrLn inputHandle request
                          hFlush inputHandle
                          response <- readLineBounded 65536 outputHandle
                          maybe (ioError $ userError "invalid predicate worker response")
                            pure $ readMaybe response) $ \pending ->
                        (do
                          result <- restoreExchange $ timeout 30000000 $ wait pending
                          case result of
                            Nothing -> abortCleanup
                            Just _ -> pure ()
                          pure result) `onException` abortCleanup
                      let verdict = case answer of
                            Nothing -> BehavioralTimedOut
                            Just (Left failure) -> BehavioralUnavailable $ show failure
                            Just (Right result) -> result
                      case verdict of
                        BehavioralTimedOut -> writeIORef retired (Just verdict) >> abortCleanup
                        BehavioralUnavailable _ -> writeIORef retired (Just verdict) >> abortCleanup
                        _ -> pure ()
                      pure verdict
            initialized <- exchange $ show context
            case initialized of
              BehavioralPassed -> action
                (\query -> exchange $ show (False, candidateCheckExpression query
                  "(let { djexBottom = djexBottom } in djexBottom)"))
                (\query term -> exchange $ show (True, candidateCheckExpression query term))
              failure -> action (\_ -> pure failure) (\_ _ -> pure failure)
    (case (input, output, errors) of
      (Just i, Just o, Just e) -> run i o e
      _ -> let failure = BehavioralUnavailable "predicate worker pipes unavailable"
           in action (\_ -> pure failure) (\_ _ -> pure failure))
      `finally` cleanup

-- A fresh outer binding prevents the predicate's named binder recursively
-- capturing a same-spelled global in the exact candidate. Newlines protect
-- generated delimiters from trailing line comments. The worker separately
-- pins the result to its private, package-qualified runtime Boolean type.
candidateCheckExpression :: BehavioralQuery -> String -> String
candidateCheckExpression query term =
  "let { " ++ fresh ++ " :: " ++ ty ++ "\n; " ++ fresh ++ " = (" ++ term
  ++ "\n) } in let { " ++ name ++ " :: " ++ ty ++ "\n; " ++ name ++ " = "
  ++ fresh ++ concatMap (\binder -> " @" ++ binder) explicitBinders
  ++ " } in (" ++ behavioralPredicate query ++ "\n)"
 where
  name = behavioralName query
  -- GHC's lexical ScopedTypeVariables rule requires the explicit forall at
  -- the syntactic outside of the signature. Parentheses preserve the type
  -- but prevent its variables from scoping over the binding's RHS. Remove
  -- only enclosing type parentheses in these private checking signatures;
  -- the requested type and all inner quantifier scopes remain unchanged.
  ty = case parsedType of
    HSE.ParseOk parsed@HSE.TyParen{} -> HSE.prettyPrint $ withoutOuterParens parsed
    _ -> behavioralType query
  parsedType = HSE.parseTypeWithMode
    (haskellSrcExtsParseMode "behavioral-alias") $ behavioralType query
  withoutOuterParens parsed = case parsed of
    HSE.TyParen _ body -> withoutOuterParens body
    _ -> parsed
  -- The second binding must forward the first binding's exact type choices.
  -- Implicit subsumption loses a constraint-only parameter before the
  -- predicate gets to apply it. Only explicitly scoped source binders may
  -- name these applications; GHC still checks the complete original type.
  explicitBinders = case parsedType of
    HSE.ParseOk parsed -> leading parsed
    HSE.ParseFailed{} -> []
  leading parsed = case parsed of
    HSE.TyParen _ body -> leading body
    HSE.TyForall _ binders _ body ->
      maybe [] (map binderName) binders ++ leading body
    _ -> []
  binderName binder = HSE.prettyPrint $ case binder of
    HSE.UnkindedVar _ variable -> variable
    HSE.KindedVar _ variable _ -> variable
  tokens = Set.fromList $ words $ map
    (\c -> if isAlphaNum c || c == '_' || c == '\'' then c else ' ')
    $ unwords [name, ty, term, behavioralPredicate query]
  fresh = freshName (0 :: Integer)
  freshName index
    | Set.member candidate tokens = freshName $ index + 1
    | otherwise = candidate
   where candidate = "djexBehavioralCandidate" ++ show index

readLineBounded :: Int -> Handle -> IO String
readLineBounded limit handle = go limit []
 where
  go remaining reversed = do
    ended <- hIsEOF handle
    when ended $ ioError $ userError "predicate worker closed its protocol"
    when (remaining <= 0) $ ioError $ userError "predicate worker protocol limit"
    character <- hGetChar handle
    if character == '\n' then pure $ reverse reversed
      else go (remaining - 1) (character : reversed)

drain :: Handle -> IO ()
drain handle = do
  ended <- hIsEOF handle
  unless ended $ hGetChar handle >> drain handle

runBehavioralWorker :: IO ExitCode
runBehavioralWorker = do
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  source <- readLineBounded (16 * 1024 * 1024) stdin
  case readMaybe source of
    Nothing -> reply (BehavioralUnavailable "invalid predicate worker context")
      >> pure (ExitFailure 1)
    Just (BehavioralContext sources starred importSources) -> do
     let runtimeModule = freshRuntimeModule $
           concatMap (\(name, body) -> [name, body]) sources ++ starred ++ importSources
         runtimeSource = unlines
           [ "{-# LANGUAGE PackageImports #-}"
           , "module " ++ runtimeModule ++ " (HostBool) where"
           , "import qualified \"base\" Prelude as Runtime (Bool)"
           , "type HostBool = Runtime.Bool"
           ]
     withSnapshot (sources ++ [(runtimeModule, runtimeSource)]) $ \paths -> do
      result <- Hint.runInterpreter $ do
        Hint.set [Hint.languageExtensions Hint.:=
          [ Hint.RankNTypes, Hint.ImpredicativeTypes, Hint.ScopedTypeVariables
          , Hint.TypeApplications, Hint.AllowAmbiguousTypes
          , Hint.ExplicitNamespaces, Hint.PatternSynonyms ]]
        imports <- either (Catch.throwM . Hint.UnknownError) pure $
          traverse checkedImport importSources
        unless (null paths) $ Hint.loadModules paths
        Hint.setTopLevelModules starred
        let implicitPrelude
              | null starred && not (any ((== "Prelude") . Hint.modName) imports) =
                  [Hint.ModuleImport "Prelude" Hint.NotQualified Hint.NoImportList]
              | otherwise = []
        -- Hint.interpret prints Typeable's expected type as source text, so
        -- `as :: Bool` would resolve an unrelated prompt-local Bool and then
        -- unsafeCoerce its value to our runtime Bool. Anchor the ONLY crossing
        -- type in an exact package import. This helper exports no values and
        -- is never starred or admitted to the synthesis environment.
        Hint.setImportsF $ implicitPrelude ++ imports ++
          [Hint.ModuleImport runtimeModule (Hint.QualifiedAs Nothing)
             (Hint.ImportList ["HostBool"])]
        liftIO $ reply BehavioralPassed
        loop $ runtimeModule ++ ".HostBool"
      case result of
        Left failure -> reply (compilationFailure failure) >> pure (ExitFailure 1)
        Right () -> pure ExitSuccess
 where
  loop runtimeBool = do
    ended <- liftIO $ hIsEOF stdin
    unless ended $ do
      source <- liftIO $ readLineBounded (16 * 1024 * 1024) stdin
      case readMaybe source of
        Nothing -> liftIO $ reply $ BehavioralUnavailable "invalid predicate worker expression"
        Just (execute, expression) -> do
          verdict <- checkExpression runtimeBool execute expression `Catch.catch`
            (pure . compilationFailure)
          liftIO $ reply verdict
          loop runtimeBool
  checkExpression runtimeBool execute expression = do
    -- The generated alias denotes precisely base's runtime Bool. Keeping this
    -- explicit is essential: unsafeInterpret does not verify the host type.
    value <- HintUnsafe.unsafeInterpret expression runtimeBool
    -- Preflight checks the exact original polymorphic binding and host Bool
    -- without evaluating its deliberately bottom-valued placeholder.
    verdict <- if execute
      then liftIO (try $ evaluate value :: IO (Either SomeException Bool))
      else pure $ Right True
    pure $ case verdict of
      Left failure -> BehavioralRuntimeError $ take 2000 $ show failure
      Right True -> BehavioralPassed
      Right False -> BehavioralFalse
  compilationFailure = BehavioralCompilationError . take 2000 . renderInterpreterError
  reply verdict = print verdict >> hFlush stdout

-- Fresh against the complete source/import surface, including qualified
-- aliases, so the generated type authority cannot replace a user module. The
-- generated ASCII name also has to be fresh on case-insensitive filesystems.
freshRuntimeModule :: [String] -> String
freshRuntimeModule sources = choose (0 :: Integer)
 where
  tokens = Set.fromList $ map (map toLower) $ words $ map
    (\c -> if isAlphaNum c || c == '_' || c == '\'' then c else ' ') $
    unwords sources
  choose index
    | Set.member (map toLower name) tokens = choose $ index + 1
    | otherwise = name
   where name = "DjexBehavioralRuntime" ++ show index

checkedImport :: String -> Either String Hint.ModuleImport
checkedImport source = do
  declaration <- either (Left . show) Right $ parseScopeImport source
  when (HSE.importSafe declaration) $ Left "predicate checking cannot weaken import safe"
  let moduleText (HSE.ModuleName _ name) = name
      qualification = case (HSE.importQualified declaration, HSE.importAs declaration) of
        (True, Nothing) -> Hint.QualifiedAs Nothing
        (True, Just alias) -> Hint.QualifiedAs $ Just $ moduleText alias
        (False, Just alias) -> Hint.ImportAs $ moduleText alias
        (False, Nothing) -> Hint.NotQualified
      surface = case HSE.importSpecs declaration of
        Nothing -> Hint.NoImportList
        Just (HSE.ImportSpecList _ hiding specs)
          | hiding -> Hint.HidingList $ map HSE.prettyPrint specs
          | otherwise -> Hint.ImportList $ map HSE.prettyPrint specs
  pure $ Hint.ModuleImport (moduleText $ HSE.importModule declaration) qualification surface

-- Only this newly-created directory is recursively removed. Validated module
-- names cannot produce a path outside it. Text is the retained source snapshot.
withSnapshot :: [(String, String)] -> ([FilePath] -> IO a) -> IO a
withSnapshot sources action = bracket makeDirectory removeDirectoryRecursive $ \directory -> do
  paths <- forM sources $ \(name, source) -> do
    case Name.mkModuleName name of
      Left failure -> ioError $ userError $ show failure
      Right _ -> pure ()
    let path = directory </> intercalate "/" (splitDots name) <.> "hs"
    createDirectoryIfMissing True $ takeDirectory path
    withFile path WriteMode $ \handle -> hSetEncoding handle utf8 >> hPutStr handle source
    pure path
  action paths
 where
  makeDirectory = do
    temporary <- getTemporaryDirectory
    (path, handle) <- openTempFile temporary "djex-behavioral"
    hClose handle
    removeFile path
    createDirectory path
    pure path
  splitDots text = case break (== '.') text of
    (piece, []) -> [piece]
    (piece, _ : rest) -> piece : splitDots rest
