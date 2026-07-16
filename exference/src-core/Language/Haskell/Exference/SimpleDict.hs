-- | Historical import path for parser-neutral Exference defaults.
-- 'defaultHeuristicsConfig' is owned by the core API and re-exported here;
-- 'emptyClassEnv' remains the compatibility spelling for an empty class
-- environment.
module Language.Haskell.Exference.SimpleDict
  ( emptyClassEnv
  , defaultHeuristicsConfig
  )
where

import Language.Haskell.Exference.Core (defaultHeuristicsConfig)
import Language.Haskell.Exference.Core.Types
  ( StaticClassEnv
  , emptyStaticClassEnv
  )

emptyClassEnv :: StaticClassEnv
emptyClassEnv = emptyStaticClassEnv
