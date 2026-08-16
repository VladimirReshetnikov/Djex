--
-- Copyright (c) 2005 Lennart Augustsson
-- See LICENSE for licensing details.
--
module Djinn.Internal.REPL (REPL(..), repl) where
import Control.Monad.Trans.Class (lift)
import System.Console.Haskeline

-- | The three callbacks of a line-oriented read-eval-print loop over a state
-- of type @s@: initialization returning the prompt and initial state,
-- evaluation of one input line returning a quit flag and the new state, and
-- the exit action.
data REPL s = REPL {
    repl_init :: IO (String, s),                        -- prompt and initial state
    repl_eval :: s -> String -> IO (Bool, s),           -- quit flag and new state
    repl_exit :: s -> IO ()
    }

-- | Drive a 'REPL' under Haskeline until 'repl_eval' asks to quit or input
-- ends (EOF), running 'repl_exit' in either case.  A keyboard interrupt
-- prints @Interrupted.@ and continues with the state from before the
-- interrupted line.
repl :: REPL s -> IO ()
repl p = do
    (prompt, state) <- repl_init p
    let loop s = step s >>= maybe (return ()) loop
        step s = handleInterrupt interrupted $ withInterrupt $ do
            line <- getInputLine prompt
            case line of
                Nothing -> lift (repl_exit p s) >> return Nothing
                Just input -> do
                    (quit, s') <- lift $ repl_eval p s input
                    if quit then
                        lift (repl_exit p s') >> return Nothing
                     else
                        return (Just s')
          where interrupted = outputStrLn "Interrupted." >> return (Just s)
    runInputT defaultSettings (loop state)
