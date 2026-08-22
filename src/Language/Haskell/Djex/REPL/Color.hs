-- | Interactive color policy, kept separate from the total presentation
-- lexer so capability probing remains an owner-thread frontend concern.
module Language.Haskell.Djex.REPL.Color
  ( ColorMode (..)
  , TerminalColorSupport
  , colorEnabled
  , colorModeName
  , colorModeNames
  , detectTerminalColorSupport
  , parseColorMode
  , resolveTerminalColorSupport
  ) where

import System.Console.ANSI (hNowSupportsANSI)
import System.Environment (lookupEnv)
import System.IO (stdout)

import Language.Haskell.Djex.Text (normalize)

-- | Runtime policy selected by @:set color@.
data ColorMode = ColorAuto | ColorAlways | ColorNever
  deriving (Eq, Show)

-- | Capability resolved once on the REPL owner thread at startup.
data TerminalColorSupport
  = TerminalColorSupported
  | TerminalColorUnsupported
  deriving (Eq, Show)

-- | Detect whether automatic coloring is appropriate for standard output.
-- The ANSI probe enables Windows virtual-terminal processing when possible.
-- Environment opt-outs are checked first, avoiding even that side effect.
detectTerminalColorSupport :: IO TerminalColorSupport
detectTerminalColorSupport = do
  noColor <- lookupEnv "NO_COLOR"
  terminal <- fmap normalize <$> lookupEnv "TERM"
  case (noColor, terminal) of
    (Just _, _) -> pure $ resolveTerminalColorSupport noColor terminal False
    (_, Just "dumb") -> pure
      $ resolveTerminalColorSupport noColor terminal False
    _ -> resolveTerminalColorSupport noColor terminal
      <$> hNowSupportsANSI stdout

-- | Combine environment policy and terminal capability. Kept pure so every
-- branch of automatic policy is testable without requiring a pseudo-terminal.
resolveTerminalColorSupport
  :: Maybe String
  -> Maybe String
  -> Bool
  -> TerminalColorSupport
resolveTerminalColorSupport noColor terminal supportsAnsi
  | noColor /= Nothing = TerminalColorUnsupported
  | fmap normalize terminal == Just "dumb" = TerminalColorUnsupported
  | supportsAnsi = TerminalColorSupported
  | otherwise = TerminalColorUnsupported

-- | Resolve a runtime policy against the startup capability snapshot.
colorEnabled :: TerminalColorSupport -> ColorMode -> Bool
colorEnabled _ ColorAlways = True
colorEnabled _ ColorNever = False
colorEnabled TerminalColorSupported ColorAuto = True
colorEnabled TerminalColorUnsupported ColorAuto = False

colorModeName :: ColorMode -> String
colorModeName ColorAuto = "auto"
colorModeName ColorAlways = "always"
colorModeName ColorNever = "never"

colorModeNames :: [String]
colorModeNames = map colorModeName [ColorAuto, ColorAlways, ColorNever]

parseColorMode :: String -> String -> Either String ColorMode
parseColorMode subject source = case normalize source of
  "auto" -> Right ColorAuto
  "always" -> Right ColorAlways
  "never" -> Right ColorNever
  _ -> Left $ subject ++ " must be auto, always, or never"
