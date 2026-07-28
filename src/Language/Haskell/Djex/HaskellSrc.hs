-- | Backend-neutral Haskell type parsing for Djex frontends.
--
-- The parser resolves names and checks kinds against one sealed source
-- inventory. A frontend can then lower the resulting shared type to either
-- search engine without parsing the user's text twice. The older Exference
-- source facade delegates to this module and remains API-compatible.
module Language.Haskell.Djex.HaskellSrc
  ( ExferenceQueryScope (..)
  , ParsedSourceType
  , parsedSourceType
  , parsedSourceTypeVariableNames
  , parsedSourceTypeLocation
  , parseSourceType
  , parseSourceTypeInScope
  ) where

import Control.Monad.Trans.Except (runExceptT)
import Data.Bifunctor (first)
import Data.Functor.Identity (runIdentity)
import qualified Language.Haskell.Exts.Parser as HSE
import qualified Language.Haskell.Exts.SrcLoc as HSEL
import qualified Language.Haskell.Exts.Syntax as HSES

import Language.Haskell.Exference.Core.Types
  ( TypeVarIndex
  , toSynthesisType
  )
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc
  ( parseTypeWithInventory
  , parseTypeWithInventoryInQualifiedScope
  , parseTypeWithInventoryInScope
  )
import Language.Haskell.Exference.TypeFromHaskellSrc
  ( haskellSrcExtsParseMode )
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , SourceLocation
  , shownErrorDiagnostic
  , sourceTextLocation
  , withCode
  , withSourceLocation
  )
import Language.Haskell.Synthesis.Inventory (Inventory)
import Language.Haskell.Synthesis.Name
  ( ModuleName
  , Name
  , renderModuleName
  )
import Language.Haskell.Synthesis.Type (Type, Variable)

-- | GHCi-style name-resolution state for one interactive query.
-- Exact visible names control bare lookup; the complete sealed inventory
-- remains available through canonical or admitted qualified names.
data ExferenceQueryScope = ExferenceQueryScope
  { exferenceQueryCurrentModule :: Maybe ModuleName
  , exferenceQueryVisibleNames :: [Name]
  , exferenceQueryModuleAliases :: [(ModuleName, ModuleName)]
  , exferenceQueryQualifiedNames :: [(ModuleName, [Name])]
  }
  deriving (Eq, Show)

-- | One parsed, resolved, kind-checked shared source type.
--
-- Variable spellings and source location are detached presentation and
-- diagnostic metadata. The type itself is the sole semantic query value.
data ParsedSourceType = ParsedSourceType
  { parsedSourceType :: Type (Variable Int)
  , parsedSourceTypeVariableNames :: TypeVarIndex
  , parsedSourceTypeLocation :: SourceLocation
  }
  deriving (Eq, Show)

-- | Parse against the complete namespace of one sealed source inventory.
parseSourceType
  :: Inventory inventoryVariable annotation
  -> FilePath
  -> String
  -> Either Diagnostic ParsedSourceType
parseSourceType inventory = parseSourceTypeWithScope inventory Nothing

-- | Parse with the same visibility and qualifier rules as the Djex REPL.
parseSourceTypeInScope
  :: Inventory inventoryVariable annotation
  -> ExferenceQueryScope
  -> FilePath
  -> String
  -> Either Diagnostic ParsedSourceType
parseSourceTypeInScope inventory scope =
  parseSourceTypeWithScope inventory $ Just scope

parseSourceTypeWithScope
  :: Inventory inventoryVariable annotation
  -> Maybe ExferenceQueryScope
  -> FilePath
  -> String
  -> Either Diagnostic ParsedSourceType
parseSourceTypeWithScope inventory maybeScope sourceName source = do
  let mode = haskellSrcExtsParseMode sourceName
      location = sourceTextLocation (HSE.parseFilename mode) source
      parsed = runIdentity $ runExceptT $ case maybeScope of
        Nothing -> parseTypeWithInventory inventory Nothing mode source
        Just scope
          | null $ exferenceQueryQualifiedNames scope ->
              parseTypeWithInventoryInScope
                inventory
                (toHseModuleName <$> exferenceQueryCurrentModule scope)
                (exferenceQueryVisibleNames scope)
                (exferenceQueryModuleAliases scope)
                mode
                source
          | otherwise -> parseTypeWithInventoryInQualifiedScope
              inventory
              (toHseModuleName <$> exferenceQueryCurrentModule scope)
              (exferenceQueryVisibleNames scope)
              (exferenceQueryModuleAliases scope)
              (exferenceQueryQualifiedNames scope)
              mode
              source
  (backendType, sourceVariables) <- first
    (withCode "DJEX_TYPE_PARSE") parsed
  sharedType <- either
    (Left . withSourceLocation location . shownErrorDiagnostic
      "DJEX_TYPE_PARSE" "parsed source type failed shared validation")
    Right
    $ toSynthesisType backendType
  pure ParsedSourceType
    { parsedSourceType = sharedType
    , parsedSourceTypeVariableNames = sourceVariables
    , parsedSourceTypeLocation = location
    }
 where
  toHseModuleName moduleName = HSES.ModuleName HSEL.noSrcSpan
    $ renderModuleName moduleName
