-- | Compatibility construction of Djex sessions from the HSE frontend's
-- parser-specific checked environment.
--
-- New applications should normally use @loadExferenceSession@ from
-- "Language.Haskell.Djex.Exference.HaskellSrc".
-- This explicit bridge exists for the historical CLI, programmatic frontend
-- fixtures, and migrations that already own a t'CheckedSourceEnvironment'.
module Language.Haskell.Exference.Session
  ( mkExferenceSession
  , mkExferenceSessionWithPolicy
  ) where

import Language.Haskell.Djex.Exference
  ( ExferenceSession
  , ExferenceSessionPolicy
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

-- | Seal a checked HSE source environment into an t'ExferenceSession' under
-- 'defaultExferenceSessionPolicy'.
mkExferenceSession
  :: CheckedSourceEnvironment
  -> Either Diagnostic ExferenceSession
mkExferenceSession = mkExferenceSessionWithPolicy
  defaultExferenceSessionPolicy

-- | Seal a checked HSE source environment into an t'ExferenceSession' under
-- an explicit policy. Only the environment's annotation-free prepared
-- inventory crosses into the session, so no lowering is repeated and no
-- parser-specific representation is retained. Policy exclusions remove
-- bindings from the search; a rating override naming no retained binding
-- is rejected with a t'Diagnostic'.
mkExferenceSessionWithPolicy
  :: ExferenceSessionPolicy
  -> CheckedSourceEnvironment
  -> Either Diagnostic ExferenceSession
mkExferenceSessionWithPolicy policy checked =
  sealPreparedExferenceSessionWithPolicy
    policy
    (checkedSourcePreparedInventory checked)
