-- | Deterministic reversal of Exference's spelling-oriented source index.
--
-- Frontends retain every spelling because source lookup is spelling-to-ID.
-- Rendering needs the inverse relation, where aliases must collapse to one
-- canonical spelling. Keeping that choice here prevents raw compatibility
-- rendering and checked candidate hints from acquiring different policies.
module Language.Haskell.Exference.Core.Internal.SourceNames
  ( preferredSourceTypeVariableNames
  ) where

import qualified Data.IntMap.Strict as IntMap

-- | Reverse @(spelling, identifier)@ pairs, choosing the lexicographically
-- least spelling for every identifier regardless of input order.
--
-- Checked callers must validate every pair before invoking this function:
-- collapsing first could otherwise let a valid preferred alias conceal an
-- invalid alternative.
preferredSourceTypeVariableNames
  :: [(String, Int)]
  -> IntMap.IntMap String
preferredSourceTypeVariableNames = IntMap.fromListWith min
  . map (\(spelling, identifier) -> (identifier, spelling))
