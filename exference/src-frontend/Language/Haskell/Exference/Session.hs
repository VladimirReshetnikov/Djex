-- | Compatibility construction of Djex sessions from the HSE frontend's
-- parser-specific checked environment.
--
-- New applications should normally use @loadExferenceSession@ from
-- "Language.Haskell.Djex.Exference.HaskellSrc".
-- This explicit bridge exists for the historical CLI, programmatic frontend
-- fixtures, and migrations that already own a 'CheckedSourceEnvironment'.
module Language.Haskell.Exference.Session
  ( mkExferenceSession
  , mkExferenceSessionWithPolicy
  ) where

import Language.Haskell.Djex.Exference
  ( ExferenceSession
  , ExferenceSessionPolicy
      ( exferenceExcludedBindings
      , exferenceRatingOverrides
      )
  , defaultExferenceSessionPolicy
  )
import Language.Haskell.Djex.Exference.Internal.Session
  ( sealPreparedExferenceSessionWithPolicy
  )
import Language.Haskell.Exference.EnvironmentParser
  ( CheckedSourceEnvironment
  , checkedSourcePreparedInventory
  )
import Language.Haskell.Synthesis.Diagnostic (Diagnostic)

mkExferenceSession
  :: CheckedSourceEnvironment
  -> Either Diagnostic ExferenceSession
mkExferenceSession = mkExferenceSessionWithPolicy
  defaultExferenceSessionPolicy

mkExferenceSessionWithPolicy
  :: ExferenceSessionPolicy
  -> CheckedSourceEnvironment
  -> Either Diagnostic ExferenceSession
mkExferenceSessionWithPolicy policy checked =
  sealPreparedExferenceSessionWithPolicy
    (exferenceExcludedBindings policy)
    (exferenceRatingOverrides policy)
    (checkedSourcePreparedInventory checked)
