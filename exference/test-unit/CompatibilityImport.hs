-- This module is intentionally Haskell 2010 with no PatternSynonyms
-- extension. It verifies that the historical bundled import still exposes
-- all four constructor spellings for downstream pattern consumption, while
-- input-bearing patterns are no longer partial value builders.
module CompatibilityImport
  ( legacyConstructorView
  )
where

import Language.Haskell.Exference.Core.Types (QualifiedName (..))

legacyConstructorView :: QualifiedName -> String
legacyConstructorView name = case name of
  QualifiedName modules spelling ->
    "ordinary:" ++ modulePrefix modules ++ spelling
  ListCon -> "list"
  TupleCon arity -> "tuple:" ++ show arity
  Cons -> "cons"
 where
  modulePrefix [] = ""
  modulePrefix modules = concatMap (++ ".") modules
