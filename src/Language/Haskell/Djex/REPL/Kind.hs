{-# LANGUAGE LambdaCase #-}

-- | Scoped, non-evaluating kind inspection for the shared REPL.
--
-- The neutral Exference inventory is the authority for type constructors,
-- classes, inferred kinds, and synonyms regardless of the currently selected
-- synthesis backend. Ordinary type kinds come from the shared inference engine;
-- this private frontend adds only the @Constraint@ result needed to display
-- class kinds, because constraints are deliberately not source 'Type' nodes.
module Language.Haskell.Djex.REPL.Kind
  ( KindInspection
  , inspectKind
  , renderKindInspection
  , renderGroundKind
  ) where

import Control.Monad (unless)
import Control.Monad.Trans.Except (runExceptT)
import Data.Bifunctor (first)
import Data.Functor.Identity (runIdentity)
import Data.List (mapAccumL)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (absurd)
import Numeric.Natural (Natural)

import qualified Language.Haskell.Exts.Parser as HSE
import qualified Language.Haskell.Exts.SrcLoc as HSE
import qualified Language.Haskell.Exts.Syntax as HSE

import Language.Haskell.Djex.Exference
  ( ExferenceSession
  , exferenceSessionInventory
  )
import qualified Language.Haskell.Djex.Exference.Internal.Session
  as ExferenceSession
import Language.Haskell.Djex.REPL.Command (KindNormalization (..))
import Language.Haskell.Djex.REPL.Scope
import Language.Haskell.Djex.Text (trim)
import Language.Haskell.Exference.Core.Types
  ( HsType
  , TypeVarIndex
  , showHsTypeWithQualification
  , toSynthesisType
  )
import Language.Haskell.Exference.EnvironmentParser
  ( haskellSrcExtsParseMode )
import Language.Haskell.Exference.Internal.TypeParsing
  ( parseTypeWithResolver
  , typeResolverFromInventory
  )
import Language.Haskell.Exference.TypeFromHaskellSrc
  ( TypeResolver (..)
  , scopeTypeResolver
  , scopeTypeResolverWithQualifiedNames
  )
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error)
  , contextualDiagnostic
  , sourceTextLocation
  , withCode
  , withSourceLocation
  )
import Language.Haskell.Synthesis.Inventory
  ( inventoryKindAssumptions )
import qualified Language.Haskell.Synthesis.Kind as Kind
import Language.Haskell.Synthesis.KindInference
  ( GroundKind
  , InferredKind
  , KindAssumptions (..)
  , checkClassApplicationKinds
  , inferTypeKind
  )
import Language.Haskell.Synthesis.Name
  ( ModuleName
  , Name
  , SpecialName (ListConstructor)
  , nameSpecial
  , renderCanonical
  , renderModuleName
  )
import Language.Haskell.Synthesis.Qualification (Qualification)
import qualified Language.Haskell.Synthesis.Type as Type

-- @Constraint@ remains presentation-only. Extending the shared 'Kind' tree
-- would break Djinn's intentionally complete KStar/KArrow/KVar compatibility
-- patterns even though constraint kinds never enter proof search.
data InspectionKind
  = InspectionType
  | InspectionConstraint
  | InspectionVariable Natural
  | InspectionFunction InspectionKind InspectionKind
  deriving (Eq, Show)

data KindInspection = KindInspection
  { inspectionSource :: String
  , inspectionKind :: InspectionKind
  , inspectionNormalForm :: HsType
  , inspectionVariableNames :: TypeVarIndex
  }

-- | Parse and infer one type in the exact current module context. Ordinary
-- @:kind@ validates synonym saturation without expanding the tree; @:kind!@
-- additionally builds the normal form and verifies that expansion preserves
-- the inferred kind.
inspectKind
  :: ExferenceSession
  -> ReplScope
  -> FilePath
  -> KindNormalization
  -> String
  -> Either Diagnostic KindInspection
inspectKind session scope sourceName normalization source = do
  (parsedType, sourceVariables) <- first (withCode "DJEX_REPL_KIND_PARSE")
    parsed
  sourceType <- first
    (kindFailure "DJEX_REPL_KIND_PARSE"
      "parsed kind input failed shared type validation" . show)
    $ toSynthesisType parsedType
  inferred <- first
    (kindFailure "DJEX_REPL_KIND_INFERENCE"
      "cannot infer the kind of the input type")
    $ inferInspectionKind assumptions sourceType
  normalForm <- case normalization of
    PreserveTypeSynonyms -> do
      first
        (kindFailure "DJEX_REPL_KIND_NORMALIZE"
          "type synonym saturation is invalid" . show)
        $ ExferenceSession.checkSessionTypeSynonymInspectionSaturation
            session sourceType
      pure sourceType
    NormalizeTypeSynonyms -> do
      normalized <- first
        (kindFailure "DJEX_REPL_KIND_NORMALIZE"
          "cannot normalize type synonyms" . show)
        $ ExferenceSession.normalizeSessionTypeSynonyms session sourceType
      normalizedKind <- first
        (kindFailure "DJEX_REPL_KIND_NORMALIZE"
          "the normalized type is ill-kinded")
        $ inferInspectionKind assumptions normalized
      unless (inferred == normalizedKind) $ Left $ kindFailure
        "DJEX_REPL_KIND_NORMALIZE"
        "type synonym normalization changed the inferred kind"
        $ renderInspectionKind inferred ++ " became "
          ++ renderInspectionKind normalizedKind
      pure normalized
  pure KindInspection
    { inspectionSource = trim source
    , inspectionKind = inferred
    , inspectionNormalForm = normalForm
    , inspectionVariableNames = sourceVariables
    }
 where
  inventory = exferenceSessionInventory session
  assumptions = inventoryKindAssumptions inventory
  mode = haskellSrcExtsParseMode sourceName
  parsed = runIdentity $ runExceptT $ parseTypeWithResolver
    resolver
    (toHseModuleName <$> scopeCurrentModule scope)
    mode
    source
  resolver = kindTypeResolver $ case scopeQualifiedNames scope of
    [] -> scopeTypeResolver
      (scopeUnqualifiedNames scope)
      (scopeQualifierAliases scope)
      $ typeResolverFromInventory inventory
    qualified -> scopeTypeResolverWithQualifiedNames
      (scopeUnqualifiedNames scope)
      (scopeQualifierAliases scope)
      qualified
      $ typeResolverFromInventory inventory
  location = sourceTextLocation (HSE.parseFilename mode) source
  kindFailure code summary detail = withSourceLocation location
    $ contextualDiagnostic Error code summary detail

-- | Render the GHCi-shaped one- or two-line result. The first line preserves
-- the user's trimmed source; only the normalized line follows Djex's nominal
-- qualification setting.
renderKindInspection
  :: Qualification
  -> KindNormalization
  -> KindInspection
  -> [String]
renderKindInspection qualification normalization inspection = firstLine : case
    normalization of
  PreserveTypeSynonyms -> []
  NormalizeTypeSynonyms ->
    ["= " ++ renderNormalForm]
 where
  firstLine = inspectionSource inspection ++ " :: "
    ++ renderInspectionKind (inspectionKind inspection)
  renderNormalForm = case stripLeadingForallBinders
      $ inspectionNormalForm inspection of
    Type.TypeConstructor name
      | nameSpecial name == Just ListConstructor -> "[]"
    normalized -> showHsTypeWithQualification qualification
      (inspectionVariableNames inspection) normalized

-- | Reuse the REPL kind spelling for declarations shown by @:browse@ and
-- @:info@. Inventory kinds are ground, so the impossible variable case remains
-- statically explicit at this boundary.
renderGroundKind :: GroundKind -> String
renderGroundKind = renderInspectionKind . mapKind
 where
  mapKind Kind.ProperTypeKind = InspectionType
  mapKind (Kind.KindVariable impossible) = absurd impossible
  mapKind (Kind.FunctionKind parameter result) = InspectionFunction
    (mapKind parameter) (mapKind result)

inferInspectionKind
  :: KindAssumptions
  -> HsType
  -> Either String InspectionKind
inferInspectionKind assumptions typeExpression = case
    Type.applicationSpine $ stripContextFreeForalls typeExpression of
  (Type.TypeConstructor name, arguments)
    | Map.notMember name $ typeConstructorKinds assumptions
    , Map.member name $ classParameterKinds assumptions -> do
        rejectNestedClasses assumptions arguments
        inferClassKind assumptions name arguments
  _ -> do
    rejectNestedClasses assumptions [typeExpression]
    first show $ inferredKind <$> inferTypeKind assumptions typeExpression

inferClassKind
  :: KindAssumptions
  -> Name
  -> [HsType]
  -> Either String InspectionKind
inferClassKind assumptions name arguments = do
  remaining <- first show
    $ checkClassApplicationKinds assumptions name arguments
  pure $ foldr InspectionFunction InspectionConstraint
    $ snd $ mapAccumL parameterKind 0 remaining
 where
  parameterKind next parameter = case parameter of
    Just kind -> (next, groundKind kind)
    Nothing -> (next + 1, InspectionVariable next)

-- Class constructors have a Constraint result, which the shared synthesis
-- Kind deliberately cannot represent. The private inspector supports one as
-- the complete operational head (beneath context-free prenex foralls), but
-- rejects nested Constraint-kinded forms before they can look like an unknown
-- ordinary type constructor to the shared inference engine.
rejectNestedClasses :: KindAssumptions -> [HsType] -> Either String ()
rejectNestedClasses assumptions typeExpressions = case Set.lookupMin
    $ Set.unions (map Type.typeConstructors typeExpressions)
        `Set.intersection` Map.keysSet (classParameterKinds assumptions) of
  Nothing -> Right ()
  Just name -> Left $ "class " ++ renderCanonical name
    ++ " has kind ending in Constraint and is supported only as the outer "
    ++ "inspected head"

stripContextFreeForalls :: HsType -> HsType
stripContextFreeForalls typeExpression = case typeExpression of
  Type.ForallType _ [] body -> stripContextFreeForalls body
  _ -> typeExpression

-- GHCi's normalized presentation elides explicit prenex forall binders. The
-- parser's source-name index still supplies their readable spellings after the
-- bound variables become presentation-only free occurrences.
stripLeadingForallBinders :: HsType -> HsType
stripLeadingForallBinders typeExpression = case typeExpression of
  Type.ForallType _ constraints body -> case
      stripLeadingForallBinders body of
    stripped
      | null constraints -> stripped
      | otherwise -> Type.ForallType [] constraints stripped
  _ -> typeExpression

inferredKind :: InferredKind -> InspectionKind
inferredKind kind = case kind of
  Kind.ProperTypeKind -> InspectionType
  Kind.KindVariable variable -> InspectionVariable variable
  Kind.FunctionKind parameter result -> InspectionFunction
    (inferredKind parameter) (inferredKind result)

groundKind :: GroundKind -> InspectionKind
groundKind kind = case kind of
  Kind.ProperTypeKind -> InspectionType
  Kind.KindVariable impossible -> absurd impossible
  Kind.FunctionKind parameter result -> InspectionFunction
    (groundKind parameter) (groundKind result)

renderInspectionKind :: InspectionKind -> String
renderInspectionKind kind = case kind of
  InspectionType -> "Type"
  InspectionConstraint -> "Constraint"
  InspectionVariable variable -> kindVariableName variable
  InspectionFunction parameter result -> renderParameter parameter
    ++ " -> " ++ renderInspectionKind result
 where
  renderParameter parameter@InspectionFunction{} =
    "(" ++ renderInspectionKind parameter ++ ")"
  renderParameter parameter = renderInspectionKind parameter
  kindVariableName 0 = "k"
  kindVariableName identifier = "k" ++ show identifier

toHseModuleName :: ModuleName -> HSE.ModuleName HSE.SrcSpanInfo
toHseModuleName moduleName = HSE.ModuleName HSE.noSrcSpan
  $ renderModuleName moduleName

-- Ordinary source conversion keeps class names out of the type-constructor
-- path. Kind inspection admits them only in this private transient resolver;
-- 'inferInspectionKind' immediately distinguishes the outer class head before
-- the shared type representation crosses any public boundary.
kindTypeResolver :: TypeResolver -> TypeResolver
kindTypeResolver resolver = resolver
  { resolverUnqualifiedTypeNames = resolverUnqualifiedTypeNames resolver
      ++ resolverUnqualifiedClassNames resolver
  }
