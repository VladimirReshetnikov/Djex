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
  , SynonymExpansionError (..)
  , ElaborationPhase (..)
  , TypeElaborationError (..)
  , prepareInventory
  , preparedInventory
  , preparedTypeSynonyms
  , adjustPreparedInventoryDataTypeAnnotations
  , prepareTypeSynonyms
  , expandTypeSynonymDefinitions
  , expandTypeSynonyms
  , elaborateTypes
  , elaborateType
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
import Data.Foldable (toList)
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Generics (Generic)

import Language.Haskell.Synthesis.Constraint
  ( Constraint (Constraint)
  , constraintArguments
  , constraintClass
  )
import Language.Haskell.Synthesis.Declaration
  ( DataConstructor (DataConstructor)
  , Declaration (..)
  , TypeParameter (parameterVariable)
  , ValueSignature (ValueSignature)
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
  , KindInferenceError
  , checkTypesKinds
  , emptyKindAssumptions
  )
import Language.Haskell.Synthesis.Name (Name, nameSpecial)
import Language.Haskell.Synthesis.Type
  ( FreshVariableAllocator
  , SubstitutionError (..)
  , Type (..)
  , TypeError
  , applicationSpine
  , canonicalizeType
  , freshenTypeBindersAwayFrom
  , substituteTypeVariables
  , validateType
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
-- assumptions used by 'elaborateType'.
data TypeSynonyms variable = TypeSynonyms
  { synonymDefinitions :: Map Name (SynonymDefinition variable)
  , synonymKindAssumptions :: KindAssumptions
  , synonymVariables :: Set variable
  }
  deriving (Eq, Show, Generic)

-- | A checked inventory paired with the exact alias table prepared from it.
--
-- The constructor is private: consumers may inspect either projection, but
-- cannot accidentally combine declarations and synonyms prepared from
-- different environments.  The annotation parameter remains functorial
-- because annotations do not participate in synonym preparation, kind
-- assumptions, or expansion.
data PreparedInventory variable annotation = PreparedInventory
  (Inventory variable annotation)
  (TypeSynonyms variable)
  deriving (Functor)

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
  | UnsaturatedTypeSynonym Name Int Int
    -- ^ Synonym, declared arity, supplied arity.
  | RecursiveTypeSynonyms (NonEmpty Name)
  | FreshVariableUnavailable variable
  | FreshVariableCollision variable variable
    -- ^ Old binder and invalid replacement returned by the allocator.
  deriving (Eq, Ord, Show, Generic)

instance NFData variable => NFData (SynonymExpansionError variable)

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
-- mask conversion of unrelated syntax. Callers that already have an
-- 'Inventory' should use 'prepareTypeSynonyms' and 'expandTypeSynonyms'.
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
  canonical <- traverse canonicalizeAndValidate sources
  checkKinds BeforeExpansion canonical
  expanded <- traverse expandOne canonical
  validated <- traverse validateExpanded expanded
  checkKinds AfterExpansion validated
  pure $ snd <$> validated
 where
  canonicalizeAndValidate (expected, source) = do
    let canonical = canonicalizeType source
    either (Left . InvalidElaborationType BeforeExpansion) Right
      $ validateType canonical
    pure (expected, canonical)

  expandOne (expected, source) = do
    expanded <- either (Left . SynonymExpansionFailed) Right
      $ expandTypeSynonyms fresh table source
    pure (expected, expanded)

  validateExpanded (expected, expanded) = do
    either (Left . InvalidElaborationType AfterExpansion) Right
      $ validateType expanded
    pure (expected, expanded)

  checkKinds phase types = either (Left . IllKindedType phase) Right
    $ checkTypesKinds (synonymKindAssumptions table) $ toList types

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
        let expected = length $ definitionParameters definition
            supplied = length arguments
        when (supplied < expected) $ lift $ Left
          $ UnsaturatedTypeSynonym name expected supplied
        bodyPath <- case pushExpansionName name path of
          Left cycleNames -> lift $ Left $ RecursiveTypeSynonyms cycleNames
          Right extended -> pure extended
        -- Arguments are independent source subtrees, not part of the alias
        -- body's recursion stack. Keeping the old path here also preserves
        -- the historical error order for failures inside arguments.
        expandedArguments <- mapM
          (expand fresh table protected path) arguments
        let (affected, trailing) = splitAt expected expandedArguments
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
          $ foldl TypeApplication expandedBody trailing
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
      $ foldl TypeApplication expandedHead expandedArguments

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
freshenSynonymBody fresh protected source = do
  reserved <- get
  result <- case freshenTypeBindersAwayFrom
      fresh reserved protected source of
    Left (FreshVariableSupplyExhausted binder) ->
      lift $ Left $ FreshVariableUnavailable binder
    Left (FreshVariableAlreadyReserved binder candidate) ->
      lift $ Left $ FreshVariableCollision binder candidate
    Right freshened -> pure freshened
  put $ reserved `Set.union` typeVariables result
  pure result

substitute
  :: Ord variable
  => FreshVariable variable
  -> Map variable (Type variable)
  -> Type variable
  -> Expansion variable (Type variable)
substitute fresh substitutions source = do
  reserved <- get
  result <- case substituteTypeVariables
      fresh reserved substitutions source of
    Left (FreshVariableSupplyExhausted binder) ->
      lift $ Left $ FreshVariableUnavailable binder
    Left (FreshVariableAlreadyReserved binder candidate) ->
      lift $ Left $ FreshVariableCollision binder candidate
    Right substituted -> pure substituted
  -- The shared primitive owns a local supply. Feed every allocated binder
  -- back into the wider synonym-expansion supply so later instantiations
  -- preserve the historical deterministic freshness sequence.
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
