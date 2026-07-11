-- This module is intentionally Haskell 2010 with no PatternSynonyms
-- extension.  It verifies that the historical bundled import continues to
-- expose all four constructor spellings to downstream source.
module CompatibilityImport
  ( legacyConstructorValues
  )
where

import Language.Haskell.Exference.Core.Types (QualifiedName (..))

legacyConstructorValues :: [QualifiedName]
legacyConstructorValues =
  [ QualifiedName [] "id"
  , ListCon
  , TupleCon 0
  , Cons
  ]
