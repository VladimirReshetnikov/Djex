{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Checked, capture-avoiding expansion of source type synonyms.
--
-- Synonyms remain declarations in the neutral inventory so kind checking can
-- validate their applications before expansion. Backends that do not model
-- aliases then elaborate through this module instead of maintaining their own
-- substitution rules.
module Language.Haskell.Synthesis.TypeSynonym
  ( FreshVariable
  , TypeSynonyms
  , PreparedInventory
  , PreparedInventoryExpansion
  , SynonymExpansionError (..)
  , InventoryExpansionError (..)
  , ElaborationPhase (..)
  , TypeElaborationError (..)
  , prepareInventory
  , preparedInventory
  , preparedTypeSynonyms
  , prepareInventoryExpansion
  , inventoryExpansionPreparedInventory
  , inventoryExpansionDeclarations
  , inventoryExpansionRecursiveDataTypeNames
  , adjustPreparedInventoryDataTypeAnnotations
  , prepareTypeSynonyms
  , checkPreparedTypeSynonymApplicationSaturation
  , checkTypeSynonymSaturation
  , checkPreparedTypeSynonymSaturation
  , expandTypeSynonymDefinitions
  , expandTypeSynonyms
  , elaborateTypes
  , elaboratePreparedTypes
  , elaborateType
  , elaboratePreparedType
  , normalizePreparedTypeSynonyms
  , expandDeclarationTypeSynonyms
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
  ( StateT
  , evalStateT
  , get
  , put
  )
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Functor.Identity (Identity (..))
import Data.List (genericSplitAt)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Void (Void)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Collection (firstDuplicate)
import Language.Haskell.Synthesis.Constraint
  ( Constraint (Constraint)
  , constraintArguments
  , constraintClass
  )
import Language.Haskell.Synthesis.Count
  ( naturalLength
  , saturatingNaturalToInt
  )
import Language.Haskell.Synthesis.Declaration
  ( DataConstructor (DataConstructor)
  , Declaration (..)
  , TypeParameter (parameterVariable)
  , ValueSignature (ValueSignature)
  , declarationSubjectName
  , recursiveDataTypeNames
  )
import Language.Haskell.Synthesis.Inventory
  ( Inventory
  , adjustInventoryDataTypeAnnotations
  , inventoryEnvironment
  , inventoryKindAssumptions
  )
import Language.Haskell.Synthesis.KindInference
  ( GroundKind
  , KindAssumptions
  , KindInferenceError (InvalidKindInferenceType)
  , checkTypesKinds
  , emptyKindAssumptions
  )
import Language.Haskell.Synthesis.Name (Name, nameSpecial)
import Language.Haskell.Synthesis.Type
  ( FreshVariableAllocator
  , SubstitutionError (..)
  , Type (..)
  , TypeError
  , applyTypeArguments
  , applicationSpine
  , canonicalizeType
  , freshenTypeBindersAwayFrom
  , substituteTypeVariables
  )
import qualified Language.Haskell.Synthesis.Environment as Environment

-- | Allocate a replacement for a binder that would capture a substitution
-- range or collide with the source namespace during alias hygiene. The first
-- argument is the complete reserved set; the second is the old binder,
-- allowing callers to preserve distinctions such as flexible and rigid
-- variable namespaces.
type FreshVariable variable = FreshVariableAllocator variable

data SynonymDefinition variable = SynonymDefinition
  { definitionParameters :: [variable]
  , definitionBody :: Type variable
  }
  deriving (Eq, Show, Generic)

instance NFData variable => NFData (SynonymDefinition variable)

-- | Prepared aliases and the exact kind assumptions of their source
-- inventory. The constructor is private so definitions cannot drift from the
-- assumptions used by 'elaborateType'. A 'Generic' instance would defeat that
-- constructor boundary through 'GHC.Generics.to'.
data TypeSynonyms variable = TypeSynonyms
  { synonymDefinitions :: Map Name (SynonymDefinition variable)
  , synonymKindAssumptions :: KindAssumptions
  , synonymVariables :: Set variable
  }
  deriving (Eq, Show)

-- | A checked inventory paired with the exact alias table prepared from it.
--
-- The constructor is private: consumers may inspect either projection, but
-- cannot accidentally combine declarations and synonyms prepared from
-- different environments. Session code should prefer the prepared saturation
-- and elaboration operations, which consume the paired witness without
-- exposing its table. The annotation parameter remains functorial because
-- annotations do not participate in synonym preparation, kind assumptions,
-- or expansion.
data PreparedInventory variable annotation = PreparedInventory
  (Inventory variable annotation)
  (TypeSynonyms variable)
  deriving (Functor)

-- | One transient, alias-free operational view of an exact prepared
-- inventory. The constructor is private so backends cannot combine an alias
-- table, expanded declarations, and recursion classification derived from
-- different source inventories.
--
-- Sessions should retain only 'inventoryExpansionPreparedInventory' and their
-- own lowered indexes. Keeping this product separate from t'PreparedInventory'
-- prevents every long-lived session from retaining a duplicate declaration
-- tree.
data PreparedInventoryExpansion variable annotation =
  PreparedInventoryExpansion
    (PreparedInventory variable annotation)
    [Declaration variable Void annotation]
    (Set Name)

-- | The authoritative checked inventory owned by a prepared witness.
preparedInventory
  :: PreparedInventory variable annotation
  -> Inventory variable annotation
preparedInventory (PreparedInventory inventory _) = inventory

-- | The alias table prepared from the witness's own inventory.
preparedTypeSynonyms
  :: PreparedInventory variable annotation
  -> TypeSynonyms variable
preparedTypeSynonyms (PreparedInventory _ synonyms) = synonyms

-- | Failures specific to alias preparation, hygiene, or substitution.
data SynonymExpansionError variable
  = IntrinsicTypeSynonym Name
  | DuplicateTypeSynonymParameter Name variable
    -- ^ Synonym and first parameter encountered for a second time.
  | UnsaturatedTypeSynonym Name Int Int
    -- ^ Synonym, declared arity, supplied arity.
  | RecursiveTypeSynonyms (NonEmpty Name)
  | FreshVariableUnavailable variable
  | FreshVariableCollision variable variable
    -- ^ Old binder and invalid replacement returned by the allocator.
  deriving (Eq, Ord, Show, Generic)

instance NFData variable => NFData (SynonymExpansionError variable)

-- | Failures while preparing an inventory's complete alias table or expanding
-- the first operational declaration that uses it. Preparation follows map-key
-- order with dependency traversal and therefore has no single operational
-- use-site; declaration failures preserve source order and subject.
data InventoryExpansionError variable
  = InventorySynonymPreparationError (SynonymExpansionError variable)
  | InventoryDeclarationExpansionError
      Int -- ^ Zero-based source declaration index.
      Name -- ^ Nominal subject; the head class for an instance.
      (SynonymExpansionError variable)
  deriving (Eq, Ord, Show, Generic)

instance NFData variable => NFData (InventoryExpansionError variable)

-- | Whether a structural or kind failure describes the source spelling or
-- its alias-free elaboration.
data ElaborationPhase
  = BeforeExpansion
  | AfterExpansion
  deriving (Eq, Ord, Show, Generic)

instance NFData ElaborationPhase

-- | The complete checked-elaboration failure vocabulary.
data TypeElaborationError variable
  = InvalidElaborationType ElaborationPhase (TypeError variable)
  | IllKindedType ElaborationPhase (KindInferenceError variable)
  | SynonymExpansionFailed (SynonymExpansionError variable)
  deriving (Eq, Ord, Show, Generic)

-- | Prepare aliases from an inventory and retain both values as one opaque
-- witness.  Backends should carry this value through session construction
-- instead of storing an independently recombinable inventory/table pair.
prepareInventory
  :: Ord variable
  => FreshVariable variable
  -> Inventory variable annotation
  -> Either (SynonymExpansionError variable)
      (PreparedInventory variable annotation)
prepareInventory fresh inventory = PreparedInventory inventory
  <$> prepareTypeSynonyms fresh inventory

-- | Prepare the exact alias table, expand every operational declaration in
-- source order, and classify recursive datatypes from that same alias-free
-- stream. Synonym declarations stay source-shaped: preparation has already
-- validated and normalized every definition, while recursion ignores their
-- declaration bodies.
--
-- Each declaration is expanded independently. In particular, this function
-- deliberately introduces no allocator state around the traversal; the
-- lexical scopes and stable fresh-variable choices owned by
-- 'expandDeclarationTypeSynonyms' must not leak between declarations.
prepareInventoryExpansion
  :: Ord variable
  => FreshVariable variable
  -> Inventory variable annotation
  -> Either (InventoryExpansionError variable)
      (PreparedInventoryExpansion variable annotation)
prepareInventoryExpansion fresh inventory = do
  prepared <- first InventorySynonymPreparationError
    $ prepareInventory fresh inventory
  let synonyms = preparedTypeSynonyms prepared
      declarations = Environment.environmentDeclarations
        $ inventoryEnvironment $ preparedInventory prepared
  expanded <- mapM (expandOperationalDeclaration synonyms)
    $ zip [0 ..] declarations
  pure $ PreparedInventoryExpansion prepared expanded
    $ recursiveDataTypeNames expanded
 where
  expandOperationalDeclaration synonyms (index, declaration) =
    case declaration of
      TypeSynonymDeclaration{} -> Right declaration
      _ -> first
        (InventoryDeclarationExpansionError index
          $ declarationSubjectName declaration)
        $ expandDeclarationTypeSynonyms fresh synonyms declaration

-- | The exact prepared inventory from which the transient view was derived.
inventoryExpansionPreparedInventory
  :: PreparedInventoryExpansion variable annotation
  -> PreparedInventory variable annotation
inventoryExpansionPreparedInventory
    (PreparedInventoryExpansion prepared _ _) = prepared

-- | Source-ordered declarations with aliases expanded in every operational
-- type position. Synonym declarations retain their checked source spelling.
inventoryExpansionDeclarations
  :: PreparedInventoryExpansion variable annotation
  -> [Declaration variable Void annotation]
inventoryExpansionDeclarations
    (PreparedInventoryExpansion _ declarations _) = declarations

-- | Recursive datatype names derived from the exact expanded declarations.
inventoryExpansionRecursiveDataTypeNames
  :: PreparedInventoryExpansion variable annotation
  -> Set Name
inventoryExpansionRecursiveDataTypeNames
    (PreparedInventoryExpansion _ _ recursiveNames) = recursiveNames

-- | Adjust derived top-level datatype metadata without rebuilding either the
-- checked inventory indexes or its alias table.  Synonym definitions contain
-- no declaration annotations, so the prepared table remains exact.
adjustPreparedInventoryDataTypeAnnotations
  :: (Name -> annotation -> annotation)
  -> PreparedInventory variable annotation
  -> PreparedInventory variable annotation
adjustPreparedInventoryDataTypeAnnotations adjust
    (PreparedInventory inventory synonyms) = PreparedInventory
  (adjustInventoryDataTypeAnnotations adjust inventory) synonyms

-- | Compile and normalize every synonym in an already checked inventory.
-- Normalization validates even aliases that no operational declaration uses.
prepareTypeSynonyms
  :: Ord variable
  => FreshVariable variable
  -> Inventory variable annotation
  -> Either (SynonymExpansionError variable) (TypeSynonyms variable)
prepareTypeSynonyms fresh inventory = do
  let declarations = Environment.environmentDeclarations
        $ inventoryEnvironment inventory
      intrinsicAliases =
        [ name
        | TypeSynonymDeclaration _ name _ _ <- declarations
        , case nameSpecial name of
            Just _ -> True
            Nothing -> False
        ]
      rawDefinitions = Map.fromList
        [ (name, SynonymDefinition
            (map parameterVariable parameters) body)
        | TypeSynonymDeclaration _ name parameters body <- declarations
        ]
      reserved = foldMap definitionVariables rawDefinitions
      rawTable = TypeSynonyms rawDefinitions
        (inventoryKindAssumptions inventory) reserved
  case intrinsicAliases of
    name : _ -> Left $ IntrinsicTypeSynonym name
    [] -> pure ()
  normalized <- evalStateT
    (Map.traverseWithKey (normalizeDefinition fresh rawTable) rawDefinitions)
    reserved
  let normalizedVariables = foldMap definitionVariables normalized
  pure $ TypeSynonyms normalized
    (inventoryKindAssumptions inventory) normalizedVariables

-- | Expand against finite raw definitions while a parser adapter is still
-- assembling its checked inventory. Only aliases reachable from the source
-- are inspected, so an independently reported invalid declaration does not
-- mask conversion of unrelated syntax. A reached definition's parameters are
-- checked for duplicates before its application arity or substitution is
-- considered. Callers that already have an 'Inventory' should use
-- 'prepareTypeSynonyms' and 'expandTypeSynonyms'.
expandTypeSynonymDefinitions
  :: Ord variable
  => FreshVariable variable
  -> Map Name ([variable], Type variable)
  -> Type variable
  -> Either (SynonymExpansionError variable) (Type variable)
expandTypeSynonymDefinitions fresh definitions = expandWithTable fresh table
 where
  prepared = Map.map (uncurry SynonymDefinition) definitions
  variables = foldMap definitionVariables prepared
  -- Parser adapters use this boundary while assembling the inventory whose
  -- kinds will be checked later. Expansion itself needs definitions and a
  -- freshness namespace only; the empty assumptions are never consulted.
  table = TypeSynonyms prepared emptyKindAssumptions variables

-- | Expand every saturated alias occurrence in a checked prepared table.
-- Overapplication consumes the declared parameters and reapplies remaining
-- arguments in order; partial application is rejected even in a higher-kinded
-- position, matching Haskell type-synonym saturation rules.
expandTypeSynonyms
  :: Ord variable
  => FreshVariable variable
  -> TypeSynonyms variable
  -> Type variable
  -> Either (SynonymExpansionError variable) (Type variable)
expandTypeSynonyms = expandWithTable

-- | Check the minimum saturation of one nominal application head. The
-- supplied count is the complete application-spine arity; a bare constructor
-- therefore supplies zero arguments. This narrow operation lets adapters
-- preserve a compatibility syntax tree's traversal and failure order while
-- consulting the authoritative alias table.
checkTypeSynonymApplicationSaturation
  :: TypeSynonyms variable
  -> Name
  -> Natural
  -> Either (SynonymExpansionError variable) ()
checkTypeSynonymApplicationSaturation table name supplied =
  case Map.lookup name $ synonymDefinitions table of
    Just definition
      | supplied < expected -> Left
          $ unsaturatedTypeSynonymError name expected supplied
      where
        expected = naturalLength $ definitionParameters definition
    _ -> Right ()

-- The established public error predates exact shared counts and retains
-- machine-sized payloads. Keep that compatibility projection explicit and
-- saturating at this single boundary.
unsaturatedTypeSynonymError
  :: Name
  -> Natural
  -> Natural
  -> SynonymExpansionError variable
unsaturatedTypeSynonymError name expected supplied =
  UnsaturatedTypeSynonym name
    (saturatingNaturalToInt expected)
    (saturatingNaturalToInt supplied)

-- | Check one nominal application head against the exact alias table sealed
-- with a prepared inventory, without exposing or independently pairing that
-- table.
checkPreparedTypeSynonymApplicationSaturation
  :: PreparedInventory variable annotation
  -> Name
  -> Natural
  -> Either (SynonymExpansionError variable) ()
checkPreparedTypeSynonymApplicationSaturation prepared =
  checkTypeSynonymApplicationSaturation $ preparedTypeSynonyms prepared

-- | Check Haskell's minimum-saturation rule without expanding any alias.
--
-- Keeping this operation on the opaque prepared table makes its definitions
-- the sole authority for both elaboration and preflight.  Application heads
-- are checked before their arguments, and forall constraints before the body;
-- callers can therefore run this before structural or kind validation when
-- source-compatible diagnostic precedence requires it.  Overapplication is
-- accepted here and remains the kind checker's responsibility.
checkTypeSynonymSaturation
  :: TypeSynonyms variable
  -> Type variable
  -> Either (SynonymExpansionError variable) ()
checkTypeSynonymSaturation table = checkType
 where
  checkType application@TypeApplication{} = do
    let (headType, arguments) = applicationSpine application
    checkHead headType $ naturalLength arguments
    case headType of
      TypeConstructor{} -> pure ()
      _ -> checkType headType
    mapM_ checkType arguments
  checkType (TypeConstructor name) = checkName name 0
  checkType (TypeVariable _) = pure ()
  checkType (FunctionType parameter result) =
    checkType parameter >> checkType result
  checkType (TupleType _ elements) = mapM_ checkType elements
  checkType (ForallType _ constraints body) =
    mapM_ (mapM_ checkType) constraints >> checkType body

  checkHead (TypeConstructor name) supplied = checkName name supplied
  checkHead _ _ = pure ()

  checkName = checkTypeSynonymApplicationSaturation table

-- | Check Haskell's minimum-saturation rule against the exact alias table
-- sealed with a prepared inventory. This is the session-facing counterpart of
-- 'checkTypeSynonymSaturation': it preserves that operation's traversal and
-- first-failure order without exposing an independently pairable table.
checkPreparedTypeSynonymSaturation
  :: PreparedInventory variable annotation
  -> Type variable
  -> Either (SynonymExpansionError variable) ()
checkPreparedTypeSynonymSaturation prepared =
  checkTypeSynonymSaturation $ preparedTypeSynonyms prepared

expandWithTable
  :: Ord variable
  => FreshVariable variable
  -> TypeSynonyms variable
  -> Type variable
  -> Either (SynonymExpansionError variable) (Type variable)
expandWithTable fresh table source = evalStateT
  (expand fresh table sourceVariables emptyExpansionPath canonicalSource)
  (synonymVariables table `Set.union` sourceVariables)
 where
  canonicalSource = canonicalizeType source
  -- Alias bodies come from independently scoped declarations. Preserve the
  -- complete caller namespace so even a phantom argument cannot disappear
  -- and leave its identity available to an unrelated introduced binder.
  sourceVariables = typeVariables canonicalSource

-- | Elaborate several types in one kind-variable scope.
--
-- Each source is canonicalized and validated before the complete batch is
-- kind-checked. Every member is then expanded independently (its binders have
-- their own lexical scope), after which the complete batch is validated and
-- kind-checked again. The first kind pass is semantically significant: a
-- phantom synonym parameter must not erase an ill-kinded argument before it
-- is diagnosed. The empty batch is valid and returns an empty batch.
elaborateTypes
  :: Ord variable
  => FreshVariable variable
  -> TypeSynonyms variable
  -> [(GroundKind, Type variable)]
  -> Either (TypeElaborationError variable) [Type variable]
elaborateTypes = elaborateTypesTraversable

-- | Elaborate several types in one kind-variable scope using the exact alias
-- table and kind assumptions sealed with a prepared inventory.
elaboratePreparedTypes
  :: Ord variable
  => FreshVariable variable
  -> PreparedInventory variable annotation
  -> [(GroundKind, Type variable)]
  -> Either (TypeElaborationError variable) [Type variable]
elaboratePreparedTypes fresh prepared =
  elaborateTypes fresh $ preparedTypeSynonyms prepared

-- | Validate and kind-check before expanding, then validate and kind-check
-- the elaborated result defensively. This is the singleton specialization of
-- 'elaborateTypes'; keeping both entry points on one worker prevents their
-- validation or expansion order from drifting apart.
elaborateType
  :: Ord variable
  => FreshVariable variable
  -> TypeSynonyms variable
  -> GroundKind
  -> Type variable
  -> Either (TypeElaborationError variable) (Type variable)
elaborateType fresh table expected source = runIdentity <$>
  elaborateTypesTraversable fresh table (Identity (expected, source))

-- | Singleton specialization of 'elaboratePreparedTypes'.
elaboratePreparedType
  :: Ord variable
  => FreshVariable variable
  -> PreparedInventory variable annotation
  -> GroundKind
  -> Type variable
  -> Either (TypeElaborationError variable) (Type variable)
elaboratePreparedType fresh prepared =
  elaborateType fresh $ preparedTypeSynonyms prepared

-- | Normalize aliases for interactive kind inspection.
--
-- Saturated aliases, including zero-parameter aliases, use the same checked,
-- capture-avoiding expansion as 'elaboratePreparedType'.  The one additional
-- form admitted here is an undersaturated alias at the head of the complete
-- input, beneath any leading context-free forall layers: its head is retained
-- and each supplied argument is normalized strictly.  Consequently, an
-- undersaturated alias nested in an argument, constrained forall, function,
-- tuple, or ordinary constructor application still fails.  This models the
-- useful @:kind!@ distinction without weakening the strict saturation
-- contract used by backend queries and declarations.
--
-- Kind inference is intentionally separate.  Callers must infer the source
-- type before invoking this operation so an alias cannot erase an ill-kinded
-- phantom argument, and may infer the result again as a defensive check.
normalizePreparedTypeSynonyms
  :: Ord variable
  => FreshVariable variable
  -> PreparedInventory variable annotation
  -> Type variable
  -> Either (SynonymExpansionError variable) (Type variable)
normalizePreparedTypeSynonyms fresh prepared source = evalStateT
  (normalizeOuter canonicalSource)
  (synonymVariables table `Set.union` sourceVariables)
 where
  table = preparedTypeSynonyms prepared
  canonicalSource = canonicalizeType source
  sourceVariables = typeVariables canonicalSource

  -- A context-free prenex binder does not change which constructor is the
  -- complete input's operational head. Retain each layer verbatim around the
  -- normalized body. Stopping at the first non-empty context is deliberate:
  -- both aliases in that context and any partial alias below it remain subject
  -- to the ordinary strict expansion contract.
  normalizeOuter typeExpression = case typeExpression of
    ForallType variables [] body -> ForallType variables []
      <$> normalizeOuter body
    _ -> case applicationSpine typeExpression of
      (TypeConstructor name, arguments)
        | Just definition <- Map.lookup name $ synonymDefinitions table
        , naturalLength arguments < naturalLength
            (definitionParameters definition) -> do
            case firstDuplicate $ definitionParameters definition of
              Just duplicate -> lift $ Left
                $ DuplicateTypeSynonymParameter name duplicate
              Nothing -> pure ()
            normalizedArguments <- mapM
              (expand fresh table sourceVariables emptyExpansionPath)
              arguments
            pure $ canonicalizeType
              $ applyTypeArguments (TypeConstructor name) normalizedArguments
      _ -> expand fresh table sourceVariables emptyExpansionPath typeExpression

-- Preserve the caller's container shape so singleton elaboration can be total
-- without extracting the head of a list whose length is known only by
-- convention. The public batch API specializes this worker to lists.
elaborateTypesTraversable
  :: (Ord variable, Traversable collection)
  => FreshVariable variable
  -> TypeSynonyms variable
  -> collection (GroundKind, Type variable)
  -> Either (TypeElaborationError variable)
      (collection (Type variable))
elaborateTypesTraversable fresh table sources = do
  let canonical = fmap canonicalizeObligation sources
  checkKinds BeforeExpansion canonical
  expanded <- traverse expandOne canonical
  checkKinds AfterExpansion expanded
  pure $ snd <$> expanded
 where
  canonicalizeObligation (expected, source) =
    (expected, canonicalizeType source)

  expandOne (expected, source) = do
    expanded <- either (Left . SynonymExpansionFailed) Right
      $ expandTypeSynonyms fresh table source
    pure (expected, expanded)

  checkKinds phase types = case
      checkTypesKinds (synonymKindAssumptions table) $ toList types of
    Left (InvalidKindInferenceType typeError) ->
      Left $ InvalidElaborationType phase typeError
    Left kindError -> Left $ IllKindedType phase kindError
    Right () -> Right ()

-- | Expand every type-bearing position of a declaration while retaining its
-- names, binders, explicit kinds, annotations, and declaration shape.
expandDeclarationTypeSynonyms
  :: Ord variable
  => FreshVariable variable
  -> TypeSynonyms variable
  -> Declaration variable kindVariable annotation
  -> Either (SynonymExpansionError variable)
      (Declaration variable kindVariable annotation)
expandDeclarationTypeSynonyms fresh table declaration = case declaration of
  TypeSynonymDeclaration annotation name parameters body ->
    TypeSynonymDeclaration annotation name parameters
      <$> expandTypeSynonyms fresh table body
  DataTypeDeclaration annotation name parameters constructors ->
    DataTypeDeclaration annotation name parameters
      <$> mapM expandConstructor constructors
  AbstractTypeDeclaration{} -> Right declaration
  ValueDeclaration (ValueSignature annotation name valueType) ->
    ValueDeclaration . ValueSignature annotation name
      <$> expandTypeSynonyms fresh table valueType
  ClassDeclaration annotation name parameters superclasses methods ->
    ClassDeclaration annotation name parameters
      <$> mapM expandDeclarationConstraint superclasses
      <*> mapM expandSignature methods
  InstanceDeclaration annotation variables prerequisites headConstraint ->
    InstanceDeclaration annotation variables
      <$> mapM expandDeclarationConstraint prerequisites
      <*> expandDeclarationConstraint headConstraint
 where
  expandConstructor (DataConstructor annotation name fields) =
    DataConstructor annotation name
      <$> mapM (expandTypeSynonyms fresh table) fields

  expandSignature (ValueSignature annotation name valueType) =
    ValueSignature annotation name
      <$> expandTypeSynonyms fresh table valueType

  expandDeclarationConstraint (Constraint className arguments) = Constraint className
    <$> mapM (expandTypeSynonyms fresh table) arguments

type Expansion variable = StateT
  (Set variable)
  (Either (SynonymExpansionError variable))

-- The list stores the active expansion stack newest-first, so extending a
-- successful path is a cons rather than a linear append. The set contains
-- exactly the same names and makes the usual non-recursive lookup logarithmic;
-- the forward diagnostic path is reconstructed only when lookup finds a cycle.
data ExpansionPath = ExpansionPath
  { reversedExpansionNames :: [Name]
  , activeExpansionNames :: Set Name
  }

emptyExpansionPath :: ExpansionPath
emptyExpansionPath = ExpansionPath [] Set.empty

initialExpansionPath :: Name -> ExpansionPath
initialExpansionPath name = ExpansionPath [name] $ Set.singleton name

normalizeDefinition
  :: Ord variable
  => FreshVariable variable
  -> TypeSynonyms variable
  -> Name
  -> SynonymDefinition variable
  -> Expansion variable (SynonymDefinition variable)
normalizeDefinition fresh table name definition = do
  body <- expand fresh table (definitionVariables definition)
    (initialExpansionPath name)
    $ definitionBody definition
  pure definition {definitionBody = body}

expand
  :: Ord variable
  => FreshVariable variable
  -> TypeSynonyms variable
  -> Set variable
  -> ExpansionPath
  -> Type variable
  -> Expansion variable (Type variable)
expand fresh table protected path source = case source of
  TypeVariable{} -> pure source
  TypeConstructor{} ->
    expandApplication fresh table protected path source []
  TypeApplication{} ->
    let (headType, arguments) = applicationSpine source
    in expandApplication fresh table protected path headType arguments
  FunctionType parameter result -> FunctionType
    <$> expand fresh table protected path parameter
    <*> expand fresh table protected path result
  TupleType boxity elements -> TupleType boxity
    <$> mapM (expand fresh table protected path) elements
  ForallType variables constraints body -> ForallType variables
    <$> mapM (expandConstraint fresh table protected path) constraints
    <*> expand fresh table protected path body

expandApplication
  :: Ord variable
  => FreshVariable variable
  -> TypeSynonyms variable
  -> Set variable
  -> ExpansionPath
  -> Type variable
  -> [Type variable]
  -> Expansion variable (Type variable)
expandApplication fresh table protected path headType arguments = case headType of
  TypeConstructor name
    | Just definition <- Map.lookup name $ synonymDefinitions table -> do
        case firstDuplicate $ definitionParameters definition of
          Just duplicate -> lift $ Left
            $ DuplicateTypeSynonymParameter name duplicate
          Nothing -> pure ()
        let expected = naturalLength $ definitionParameters definition
            supplied = naturalLength arguments
        when (supplied < expected) $ lift $ Left
          $ unsaturatedTypeSynonymError name expected supplied
        bodyPath <- case pushExpansionName name path of
          Left cycleNames -> lift $ Left $ RecursiveTypeSynonyms cycleNames
          Right extended -> pure extended
        -- Arguments are independent source subtrees, not part of the alias
        -- body's recursion stack. Keeping the old path here also preserves
        -- the historical error order for failures inside arguments.
        expandedArguments <- mapM
          (expand fresh table protected path) arguments
        let (affected, trailing) = genericSplitAt expected expandedArguments
            substitutions = Map.fromList
              $ zip (definitionParameters definition) affected
        -- Definitions have their own lexical identity namespace. Freshen its
        -- colliding binders before substitution even when all corresponding
        -- arguments are phantom. Repeating this for every reached definition
        -- also covers nested and zero-argument aliases.
        hygienicBody <- freshenSynonymBody fresh protected
          $ definitionBody definition
        instantiated <- substitute fresh substitutions
          hygienicBody
        expandedBody <- expand fresh table protected bodyPath instantiated
        pure $ canonicalizeType
          $ applyTypeArguments expandedBody trailing
  _ -> do
    -- An application spine has already exposed an ordinary variable or
    -- constructor head. Re-entering 'expand' for a bare non-synonym
    -- constructor would rediscover the same zero-argument spine forever.
    expandedHead <- case headType of
      TypeVariable{} -> pure headType
      TypeConstructor{} -> pure headType
      _ -> expand fresh table protected path headType
    expandedArguments <- mapM
      (expand fresh table protected path) arguments
    pure $ canonicalizeType
      $ applyTypeArguments expandedHead expandedArguments

expandConstraint
  :: Ord variable
  => FreshVariable variable
  -> TypeSynonyms variable
  -> Set variable
  -> ExpansionPath
  -> Constraint (Type variable)
  -> Expansion variable (Constraint (Type variable))
expandConstraint fresh table protected path constraint = Constraint
  (constraintClass constraint)
  <$> mapM
      (expand fresh table protected path)
      (constraintArguments constraint)

-- Run the stronger alias-origin freshening rule inside the same supply used
-- by capture-avoiding substitution and later alias instantiations.
freshenSynonymBody
  :: Ord variable
  => FreshVariable variable
  -> Set variable
  -> Type variable
  -> Expansion variable (Type variable)
freshenSynonymBody fresh protected source = underExpansionSupply $ \reserved ->
  freshenTypeBindersAwayFrom fresh reserved protected source

substitute
  :: Ord variable
  => FreshVariable variable
  -> Map variable (Type variable)
  -> Type variable
  -> Expansion variable (Type variable)
substitute fresh substitutions source = underExpansionSupply $ \reserved ->
  substituteTypeVariables fresh reserved substitutions source

-- | Run one capture-avoiding 'Type' primitive under the expansion supply.
-- The primitive sees every identity currently reserved, its allocator
-- failures become expansion errors, and every binder it allocated is fed
-- back into the wider synonym-expansion supply: the shared primitives own a
-- local supply, and later instantiations must preserve the historical
-- deterministic freshness sequence.
underExpansionSupply
  :: Ord variable
  => (Set variable -> Either (SubstitutionError variable) (Type variable))
  -> Expansion variable (Type variable)
underExpansionSupply primitive = do
  reserved <- get
  result <- case primitive reserved of
    Left (FreshVariableSupplyExhausted binder) ->
      lift $ Left $ FreshVariableUnavailable binder
    Left (FreshVariableAlreadyReserved binder candidate) ->
      lift $ Left $ FreshVariableCollision binder candidate
    Right value -> pure value
  put $ reserved `Set.union` typeVariables result
  pure result

pushExpansionName
  :: Name
  -> ExpansionPath
  -> Either (NonEmpty Name) ExpansionPath
pushExpansionName name path
  | name `Set.member` activeExpansionNames path = Left
      $ cycleFromReversedPath name $ reversedExpansionNames path
  | otherwise = Right $ ExpansionPath
      (name : reversedExpansionNames path)
      (Set.insert name $ activeExpansionNames path)

-- The membership check above establishes that the name occurs in the private
-- path. Names preceding it in the reverse list are precisely the newer suffix
-- of the forward path, so reversing only that suffix reproduces the old
-- source-ordered cycle and omits any non-cyclic prefix.
cycleFromReversedPath :: Name -> [Name] -> NonEmpty Name
cycleFromReversedPath name reversed = name :|
  (reverse (takeWhile (/= name) reversed) ++ [name])

definitionVariables :: Ord variable => SynonymDefinition variable -> Set variable
definitionVariables definition = Set.fromList
  (definitionParameters definition) `Set.union`
  typeVariables (definitionBody definition)

typeVariables :: Ord variable => Type variable -> Set variable
typeVariables = foldMap Set.singleton
