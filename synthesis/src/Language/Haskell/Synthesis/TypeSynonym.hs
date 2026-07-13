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
  , SynonymExpansionError (..)
  , ElaborationPhase (..)
  , TypeElaborationError (..)
  , prepareTypeSynonyms
  , expandTypeSynonymDefinitions
  , expandTypeSynonyms
  , elaborateType
  , expandDeclarationTypeSynonyms
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (foldM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
  ( StateT
  , evalStateT
  , get
  , put
  )
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
  ( Type (..)
  , TypeError
  , canonicalizeType
  , freeVariables
  , renameScopedVariables
  , validateType
  )
import qualified Language.Haskell.Synthesis.Environment as Environment

-- | Allocate a replacement for a binder that would capture a substitution
-- range. The first argument is the complete reserved set; the second is the
-- old binder, allowing callers to preserve distinctions such as flexible and
-- rigid variable namespaces.
type FreshVariable variable = Set variable -> variable -> Maybe variable

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

-- | Failures that are specific to alias preparation or substitution.
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
  (expand fresh table [] $ canonicalizeType source)
  (synonymVariables table `Set.union` typeVariables source)

-- | Validate and kind-check before expanding, then validate and kind-check
-- the elaborated result defensively. The first kind pass is semantically
-- significant: a phantom synonym parameter must not erase an ill-kinded
-- argument before it is diagnosed.
elaborateType
  :: Ord variable
  => FreshVariable variable
  -> TypeSynonyms variable
  -> GroundKind
  -> Type variable
  -> Either (TypeElaborationError variable) (Type variable)
elaborateType fresh table expected source = do
  let canonical = canonicalizeType source
  either (Left . InvalidElaborationType BeforeExpansion) Right
    $ validateType canonical
  either (Left . IllKindedType BeforeExpansion) Right
    $ checkTypesKinds (synonymKindAssumptions table) [(expected, canonical)]
  expanded <- either (Left . SynonymExpansionFailed) Right
    $ expandTypeSynonyms fresh table canonical
  either (Left . InvalidElaborationType AfterExpansion) Right
    $ validateType expanded
  either (Left . IllKindedType AfterExpansion) Right
    $ checkTypesKinds (synonymKindAssumptions table) [(expected, expanded)]
  pure expanded

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

normalizeDefinition
  :: Ord variable
  => FreshVariable variable
  -> TypeSynonyms variable
  -> Name
  -> SynonymDefinition variable
  -> Expansion variable (SynonymDefinition variable)
normalizeDefinition fresh table name definition = do
  body <- expand fresh table [name] $ definitionBody definition
  pure definition {definitionBody = body}

expand
  :: Ord variable
  => FreshVariable variable
  -> TypeSynonyms variable
  -> [Name]
  -> Type variable
  -> Expansion variable (Type variable)
expand fresh table path source = case source of
  TypeVariable{} -> pure source
  TypeConstructor{} -> expandApplication fresh table path source []
  TypeApplication{} ->
    let (headType, arguments) = applicationSpine source
    in expandApplication fresh table path headType arguments
  FunctionType parameter result -> FunctionType
    <$> expand fresh table path parameter
    <*> expand fresh table path result
  TupleType boxity elements -> TupleType boxity
    <$> mapM (expand fresh table path) elements
  ForallType variables constraints body -> ForallType variables
    <$> mapM (expandConstraint fresh table path) constraints
    <*> expand fresh table path body

expandApplication
  :: Ord variable
  => FreshVariable variable
  -> TypeSynonyms variable
  -> [Name]
  -> Type variable
  -> [Type variable]
  -> Expansion variable (Type variable)
expandApplication fresh table path headType arguments = case headType of
  TypeConstructor name
    | Just definition <- Map.lookup name $ synonymDefinitions table -> do
        let expected = length $ definitionParameters definition
            supplied = length arguments
        when (supplied < expected) $ lift $ Left
          $ UnsaturatedTypeSynonym name expected supplied
        case cycleFrom name path of
          Just cycleNames -> lift $ Left $ RecursiveTypeSynonyms cycleNames
          Nothing -> pure ()
        expandedArguments <- mapM (expand fresh table path) arguments
        let (affected, trailing) = splitAt expected expandedArguments
            substitutions = Map.fromList
              $ zip (definitionParameters definition) affected
        instantiated <- substitute fresh substitutions
          $ definitionBody definition
        expandedBody <- expand fresh table (path ++ [name]) instantiated
        pure $ canonicalizeType
          $ foldl TypeApplication expandedBody trailing
  _ -> do
    -- An application spine has already exposed an ordinary variable or
    -- constructor head. Re-entering 'expand' for a bare non-synonym
    -- constructor would rediscover the same zero-argument spine forever.
    expandedHead <- case headType of
      TypeVariable{} -> pure headType
      TypeConstructor{} -> pure headType
      _ -> expand fresh table path headType
    expandedArguments <- mapM (expand fresh table path) arguments
    pure $ canonicalizeType
      $ foldl TypeApplication expandedHead expandedArguments

expandConstraint
  :: Ord variable
  => FreshVariable variable
  -> TypeSynonyms variable
  -> [Name]
  -> Constraint (Type variable)
  -> Expansion variable (Constraint (Type variable))
expandConstraint fresh table path constraint = Constraint
  (constraintClass constraint)
  <$> mapM (expand fresh table path) (constraintArguments constraint)

substitute
  :: Ord variable
  => FreshVariable variable
  -> Map variable (Type variable)
  -> Type variable
  -> Expansion variable (Type variable)
substitute fresh substitutions source = case source of
  TypeVariable variable -> pure
    $ Map.findWithDefault source variable substitutions
  TypeConstructor{} -> pure source
  TypeApplication function argument -> TypeApplication
    <$> substitute fresh substitutions function
    <*> substitute fresh substitutions argument
  FunctionType parameter result -> FunctionType
    <$> substitute fresh substitutions parameter
    <*> substitute fresh substitutions result
  TupleType boxity elements -> TupleType boxity
    <$> mapM (substitute fresh substitutions) elements
  ForallType binders constraints body -> do
    let active = foldr Map.delete substitutions binders
        subjectVariables = freeVariables
          $ ForallType binders constraints body
        relevant = Map.restrictKeys active subjectVariables
        rangeVariables = foldMap freeVariables relevant
    renaming <- foldM (freshenBinder fresh rangeVariables)
      Map.empty binders
    let renamedBinders = map
          (\binder -> Map.findWithDefault binder binder renaming) binders
        renamedConstraints = map
          (fmap $ renameScopedVariables renaming) constraints
        renamedBody = renameScopedVariables renaming body
        withoutFreshBinders = foldr Map.delete relevant renamedBinders
    ForallType renamedBinders
      <$> mapM (substituteConstraint fresh withoutFreshBinders)
            renamedConstraints
      <*> substitute fresh withoutFreshBinders renamedBody

substituteConstraint
  :: Ord variable
  => FreshVariable variable
  -> Map variable (Type variable)
  -> Constraint (Type variable)
  -> Expansion variable (Constraint (Type variable))
substituteConstraint fresh substitutions constraint = Constraint
  (constraintClass constraint)
  <$> mapM (substitute fresh substitutions)
        (constraintArguments constraint)

freshenBinder
  :: Ord variable
  => FreshVariable variable
  -> Set variable
  -> Map variable variable
  -> variable
  -> Expansion variable (Map variable variable)
freshenBinder fresh rangeVariables renaming binder
  | binder `Set.notMember` rangeVariables = pure renaming
  | otherwise = do
      reserved <- get
      replacement <- case fresh reserved binder of
        Nothing -> lift $ Left $ FreshVariableUnavailable binder
        Just candidate
          | candidate `Set.member` reserved -> lift $ Left
              $ FreshVariableCollision binder candidate
          | otherwise -> pure candidate
      put $ Set.insert replacement reserved
      pure $ Map.insert binder replacement renaming

applicationSpine :: Type variable -> (Type variable, [Type variable])
applicationSpine = collect []
 where
  collect arguments (TypeApplication function argument) =
    collect (argument : arguments) function
  collect arguments function = (function, arguments)

cycleFrom :: Name -> [Name] -> Maybe (NonEmpty Name)
cycleFrom name path = case dropWhile (/= name) path of
  [] -> Nothing
  first : remaining -> Just $ first :| remaining ++ [name]

definitionVariables :: Ord variable => SynonymDefinition variable -> Set variable
definitionVariables definition = Set.fromList
  (definitionParameters definition) `Set.union`
  typeVariables (definitionBody definition)

typeVariables :: Ord variable => Type variable -> Set variable
typeVariables = foldMap Set.singleton
