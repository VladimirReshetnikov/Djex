-- | Historical module name for Exference expression cleanup.
--
-- The implementation is now the identity-parameterized operation on Djex's
-- shared generated tree; this module remains only as a source-compatible
-- import boundary.
module Language.Haskell.Exference.Core.ExpressionSimplify
  ( simplifyExpression
  )
where

import Language.Haskell.Exference.Core.Expression (simplifyExpression)
