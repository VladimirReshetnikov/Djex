--
-- Copyright (c) 2005 Lennart Augustsson
-- See LICENSE for licensing details.
--
module REPL(REPL(..), repl) where
import Control.Monad.Trans(liftIO)
import System.Console.Haskeline

data REPL s = REPL {
    repl_init :: IO (String, s),                        -- prompt and initial state
    repl_eval :: s -> String -> IO (Bool, s),           -- quit flag and new state
    repl_exit :: s -> IO ()
    }

repl :: REPL s -> IO ()
repl p = do
    (prompt, state) <- repl_init p
    let loop s = step s >>= maybe (return ()) loop
        step s = handleInterrupt interrupted $ withInterrupt $ do
            line <- getInputLine prompt
            case line of
                Nothing -> liftIO (repl_exit p s) >> return Nothing
                Just input -> do
                    (quit, s') <- liftIO $ repl_eval p s input
                    if quit then
                        liftIO (repl_exit p s') >> return Nothing
                     else
                        return (Just s')
          where interrupted = outputStrLn "Interrupted." >> return (Just s)
    runInputT defaultSettings (loop state)
