module Language.Haskell.Exference.FunctionDecl
  ( HsFunctionDecl
  )
where

import Language.Haskell.Exference.Core.Types

type HsFunctionDecl = (QualifiedName, HsType)
