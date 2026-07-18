{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Deliberate coverage for compatibility functions whose replacements are
-- tested elsewhere. Keep the warning waiver local to these exact calls so
-- new deprecated uses in the main suite still fail its strict build.
module DeprecatedCompatibility
  ( findOneExpression
  , findExpressionsWithStats
  , largestId
  ) where

import qualified Language.Haskell.Exference as Exference
import Language.Haskell.Exference
  ( ExferenceInput
  , ExferenceOutputElement
  )
import qualified Language.Haskell.Exference.Core as ExferenceCore
import Language.Haskell.Exference.Core (ExferenceChunkElement)
import qualified Language.Haskell.Exference.Core.TypeUtils as TypeUtils
import Language.Haskell.Exference.Core.Types (HsType, TVarId)

findOneExpression :: ExferenceInput -> Maybe ExferenceOutputElement
findOneExpression = Exference.findOneExpression

findExpressionsWithStats :: ExferenceInput -> [ExferenceChunkElement]
findExpressionsWithStats = ExferenceCore.findExpressionsWithStats

largestId :: HsType -> TVarId
largestId = TypeUtils.largestId
