-- | Compatibility construction of Djex sessions from the HSE frontend's
-- parser-specific checked environment.
--
-- New applications should normally use 'loadExferenceSession' from
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
import Language.Haskell.Djex.Exference.Internal.Frontend
  ( sealProjectedExferenceSessionWithPolicy
  )
import Language.Haskell.Exference.Core.FunctionBinding
  ( EnvDictionary (EnvDictionary) )
import Language.Haskell.Exference.EnvironmentParser
  ( CheckedSourceEnvironment
  , SourceEnvironment (sourceClasses, sourceDeconstructors)
  , checkedSourceInventory
  , checkedSourceProjection
  , sourceFunctions
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
  sealProjectedExferenceSessionWithPolicy
    (exferenceExcludedBindings policy)
    (exferenceRatingOverrides policy)
    inventory
    backend
 where
  source = checkedSourceProjection checked
  inventory = fmap (const ()) $ checkedSourceInventory checked
  -- Preserve the source loader's ratings and declaration order. The neutral
  -- inventory owns validation and kinds; this dictionary owns search policy.
  backend = EnvDictionary
    (sourceFunctions source)
    (sourceDeconstructors source)
    (sourceClasses source)
