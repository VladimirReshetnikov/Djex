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
  , parsedSourceKinds
  , parsedSourceHasKindAnnotations
  , parsedSourceRequestKinds
  , parsedSourceTypeVariableNames
  , parsedSourceTypeLocation
  , parseSourceType
  , parseSourceTypeInScope
  ) where

import Control.Monad.Trans.Except (runExceptT)
import Data.Bifunctor (first)
import Data.Functor.Identity (runIdentity)
import qualified Data.Map.Strict as Map
import qualified Language.Haskell.Exts.Parser as HSE
import qualified Language.Haskell.Exts.SrcLoc as HSEL
import qualified Language.Haskell.Exts.Syntax as HSES

import Language.Haskell.Exference.Core.Types
  ( TypeVarIndex
  , toSynthesisType
  )
import Language.Haskell.Exference.Core.Declaration (freshSynthesisVariable)
import Language.Haskell.Exference.Internal.TypeParsing
  ( parseHaskellSrcType, typeResolverFromInventory )
import Language.Haskell.Exference.TypeFromHaskellSrc
  ( haskellSrcExtsParseMode, scopeTypeResolver, scopeTypeResolverWithQualifiedNames )
import Language.Haskell.Djex.HaskellSrc.Kinded (convertKindedSourceType)
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , SourceLocation
  , shownErrorDiagnostic
  , sourceTextLocation
  , withCode
  , withSourceLocation
  )
import Language.Haskell.Synthesis.Inventory (Inventory, inventoryKindAssumptions)
import Language.Haskell.Synthesis.Name
  ( ModuleName
  , Name
  , renderModuleName
  )
import Language.Haskell.Synthesis.Type (Type, Variable)
import qualified Language.Haskell.Synthesis.Type as SharedType
import Language.Haskell.Synthesis.SourceKind
  ( SourceTypeKinds, SourceKindAnnotation (..), SourceTypeStep (ForallBody)
  , prepareSourceTypeKinds, sourceKindsType, sourceKindAnnotations )
import Language.Haskell.Synthesis.SourceKind.Expansion (expandSourceTypeKinds)
import Language.Haskell.Djex.HaskellSrc.Scope (hasExplicitOuterForall)

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
-- Implicit root variables are universally quantified in first-occurrence
-- order. An explicit outer forall cannot leave unbound type variables.
--
-- Variable spellings and source location are detached presentation and
-- diagnostic metadata. The type and its lexical kind information are kept
-- together; ordinary projections cannot replace one half of that checked
-- pair through record updates.
data ParsedSourceType = ParsedSourceType
  (SourceTypeKinds (Variable Int))
  TypeVarIndex
  SourceLocation
  Bool
  deriving (Eq, Show)

parsedSourceType :: ParsedSourceType -> Type (Variable Int)
parsedSourceType = sourceKindsType . parsedSourceKinds

parsedSourceKinds :: ParsedSourceType -> SourceTypeKinds (Variable Int)
parsedSourceKinds (ParsedSourceType kinds _ _ _) = kinds

-- | Whether the written signature supplied binder-kind annotations. The
-- checked kind table also contains inferred facts for unannotated syntax;
-- this flag lets compatibility frontends retain their unannotated path.
parsedSourceHasKindAnnotations :: ParsedSourceType -> Bool
parsedSourceHasKindAnnotations (ParsedSourceType _ _ _ explicitKinds) = explicitKinds

-- | Effective obligations which a source-aware request must retain. Ordinary
-- unannotated syntax continues to use the existing session inference policy.
parsedSourceRequestKinds :: ParsedSourceType -> [SourceKindAnnotation]
parsedSourceRequestKinds parsed
  | parsedSourceHasKindAnnotations parsed = sourceKindAnnotations $ parsedSourceKinds parsed
  | otherwise = []

parsedSourceTypeVariableNames :: ParsedSourceType -> TypeVarIndex
parsedSourceTypeVariableNames (ParsedSourceType _ variables _ _) = variables

parsedSourceTypeLocation :: ParsedSourceType -> SourceLocation
parsedSourceTypeLocation (ParsedSourceType _ _ location _) = location

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
      originalResolver = typeResolverFromInventory inventory
      resolver = case maybeScope of
        Nothing -> originalResolver
        Just scope
          | null $ exferenceQueryQualifiedNames scope -> scopeTypeResolver
              (exferenceQueryVisibleNames scope) (exferenceQueryModuleAliases scope) originalResolver
          | otherwise -> scopeTypeResolverWithQualifiedNames
              (exferenceQueryVisibleNames scope) (exferenceQueryModuleAliases scope)
              (exferenceQueryQualifiedNames scope) originalResolver
      currentModule = maybeScope >>= fmap toHseModuleName . exferenceQueryCurrentModule
      parsed = runIdentity $ runExceptT $ parseHaskellSrcType
        (convertKindedSourceType (inventoryKindAssumptions inventory) resolver currentModule)
        mode source
  (rawKinds, sourceVariables) <- first
    (withCode "DJEX_TYPE_PARSE") parsed
  canonicalKinds <- either
    (Left . withSourceLocation location . shownErrorDiagnostic
      "DJEX_TYPE_PARSE" "source kind canonicalization failed")
    Right
    $ expandSourceTypeKinds freshSynthesisVariable Map.empty rawKinds
  -- Haskell implicitly quantifies a signature's free variables in written
  -- first-occurrence order. Close that source scheme before either backend
  -- constructs its candidate graph; renderer-only closure cannot repair an
  -- open graph or recover an erased contextual parameter later.
  let sharedType = sourceKindsType canonicalKinds
      free = SharedType.freeVariablesInFirstOccurrenceOrder sharedType
      explicit = hasExplicitOuterForall source
  if explicit && not (null free)
    then Left $ withSourceLocation location $ shownErrorDiagnostic
      "DJEX_TYPE_PARSE" "explicit forall leaves unbound type variables" free
    else pure ()
  closedType <- either
    (Left . withSourceLocation location . shownErrorDiagnostic
      "DJEX_TYPE_PARSE" "implicit source quantification failed shared validation")
    Right $ toSynthesisType $ if null free then sharedType
      else SharedType.ForallType free [] sharedType
  sourceKinds <- either
    (Left . withSourceLocation location . shownErrorDiagnostic
      "DJEX_TYPE_PARSE" "source binder kind validation failed")
    Right $ prepareSourceTypeKinds (inventoryKindAssumptions inventory) closedType
      [ annotation { sourceKindPath = if null free then sourceKindPath annotation
          else ForallBody : sourceKindPath annotation }
      | annotation <- sourceKindAnnotations canonicalKinds ]
  pure $ ParsedSourceType sourceKinds sourceVariables location
    (not $ null $ sourceKindAnnotations rawKinds)
 where
  toHseModuleName moduleName = HSES.ModuleName HSEL.noSrcSpan
    $ renderModuleName moduleName
