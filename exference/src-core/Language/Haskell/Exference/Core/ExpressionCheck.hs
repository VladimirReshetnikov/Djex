-- | Stable independent validation of Exference generated expressions.
--
-- The implementation retains additional typed evidence for the private engine
-- checkpoint, but this exposed wrapper deliberately preserves the historical
-- checker surface.
module Language.Haskell.Exference.Core.ExpressionCheck
  ( ExpressionCheckError (..)
  , ExpressionCheckContext
  , NestedRigidProvenance
  , prepareExpressionCheckContext
  , prepareExpressionCheckContextWithSchemes
  , checkExpressionInContext
  , checkExpressionInContextWithNestedRigidProvenance
  , checkExpression
  , checkExpressionWithRigidInstantiation
  )
where

import Language.Haskell.Exference.Core.Internal.ExpressionCheck
