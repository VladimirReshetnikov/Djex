-- | Unstable, parser-neutral seam for Exference source frontends.
--
-- This module intentionally exposes only the operations a checked source
-- adapter needs. Session representation and search invariants remain owned by
-- the private implementation module.
module Language.Haskell.Djex.Exference.Internal.Frontend
  ( sealProjectedExferenceSessionWithPolicy
  , sessionTypeNames
  , sessionClasses
  ) where

import Language.Haskell.Djex.Exference.Internal.Session
  ( sealProjectedExferenceSessionWithPolicy
  , sessionClasses
  , sessionTypeNames
  )
