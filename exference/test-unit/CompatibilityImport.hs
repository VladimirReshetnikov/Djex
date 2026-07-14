{-# LANGUAGE PatternSynonyms #-}

-- The engine's name identity is now the shared 'Name', so historical views
-- are imported explicitly as compatibility patterns instead of pretending to
-- be constructors of a second wrapper type.
module CompatibilityImport
  ( legacyConstructorView
  , legacyConstraint
  , legacyConstraintClass
  )
where

import Language.Haskell.Exference.Core.Types
  ( HsConstraint
  , QualifiedName
  , pattern Cons
  , pattern HsConstraint
  , pattern ListCon
  , pattern QualifiedName
  , pattern TupleCon
  , pattern UnboxedTupleCon
  )

legacyConstructorView :: QualifiedName -> String
legacyConstructorView name = case name of
  QualifiedName modules spelling ->
    "ordinary:" ++ modulePrefix modules ++ spelling
  ListCon -> "list"
  TupleCon arity -> "tuple:" ++ show arity
  UnboxedTupleCon arity -> "unboxed-tuple:" ++ show arity
  Cons -> "cons"
 where
  modulePrefix [] = ""
  modulePrefix modules = concatMap (++ ".") modules

-- Keep this explicit import/construct/deconstruct fixture: unlike ordinary
-- whole-module imports, it exercises the source migration required when a
-- historical data constructor becomes a pattern over the shared constraint.
legacyConstraint :: QualifiedName -> HsConstraint
legacyConstraint className = HsConstraint className []

legacyConstraintClass :: HsConstraint -> QualifiedName
legacyConstraintClass (HsConstraint className _) = className
