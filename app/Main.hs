-- | Entry point of the @djex@ executable.  The process is a thin wrapper:
-- it delegates to the in-process command frontend
-- "Language.Haskell.Djex.CLI", which owns argument handling and the
-- interactive and one-shot @djex@ command itself.
module Main (main) where

import qualified Language.Haskell.Djex.CLI as CLI

main :: IO ()
main = CLI.main
