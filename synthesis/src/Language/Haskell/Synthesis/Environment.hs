{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Opaque, structurally validated source-declaration environment.
--
-- This layer owns cross-declaration namespaces and indexes. Kind inference,
-- dependency cycles, unknown-name policy, and backend class/instance semantics
-- are deliberately separate checks layered over this stable inventory.
module Language.Haskell.Synthesis.Environment
  ( Environment
  , EnvironmentError (..)
  , mkEnvironment
  , groundEnvironmentKinds
  , environmentDeclarations
  , typeDeclarationMap
  , valueSignatureMap
  , dataConstructorMap
  , classDeclarationMap
  , instanceDeclarationMap
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Void (Void)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Language.Haskell.Synthesis.Constraint
import Language.Haskell.Synthesis.Declaration
import Language.Haskell.Synthesis.Name
import Language.Haskell.Synthesis.Type

data Environment typeVariable kindVariable annotation = Environment
  { reversedDeclarations ::
      [Declaration typeVariable kindVariable annotation]
  , typeDeclarationsByName ::
      Map Name (Declaration typeVariable kindVariable annotation)
  , valuesByName :: Map Name (ValueSignature typeVariable annotation)
  , constructorsByName ::
      Map Name (DataConstructor typeVariable annotation)
  , classesByName ::
      Map Name (Declaration typeVariable kindVariable annotation)
  , instancesByHead ::
      Map (Constraint (Type typeVariable))
        (Declaration typeVariable kindVariable annotation)
  , canonicalInstanceHeads ::
      Set (Constraint (Type (CanonicalInstanceVariable typeVariable)))
  , occupiedValueNames :: Set Name
  }
  deriving (Eq, Show, Functor, Generic)

instance
    (NFData typeVariable, NFData kindVariable, NFData annotation) =>
    NFData (Environment typeVariable kindVariable annotation)

data EnvironmentError typeVariable
  = InvalidEnvironmentDeclaration Int (DeclarationError typeVariable)
  | DuplicateTypeDeclaration Name
  | DuplicateValueDeclaration Name
  | DuplicateInstanceDeclaration (Constraint (Type typeVariable))
  deriving (Eq, Ord, Show, Generic)

instance NFData typeVariable => NFData (EnvironmentError typeVariable)

-- | Private alpha-normal form for variables in an instance head.  Bound
-- variables are identified by lexical scope and first occurrence within that
-- scope; genuinely free variables retain their source identity.  Source names
-- remain untouched everywhere else, including the public instance index and
-- duplicate diagnostics.
data CanonicalInstanceVariable typeVariable
  = CanonicalBoundVariable !Natural !Natural
  | CanonicalFreeVariable typeVariable
  deriving (Eq, Ord, Show, Generic)

instance NFData typeVariable =>
    NFData (CanonicalInstanceVariable typeVariable)

-- Scope and slot identities exist only while constructing a private
-- alpha-normal form. They use arbitrary-precision counters so sufficiently
-- deep or wide generated types cannot wrap and conflate distinct binders;
-- declaration indices and source-facing arities remain machine-sized.
data CanonicalizationState typeVariable = CanonicalizationState
  { canonicalVariableSlots :: Map (Natural, typeVariable) Natural
  , nextCanonicalSlotByScope :: Map Natural Natural
  , nextCanonicalScope :: !Natural
  }

mkEnvironment
  :: Ord typeVariable
  => [Declaration typeVariable kindVariable annotation]
  -> Either (EnvironmentError typeVariable)
      (Environment typeVariable kindVariable annotation)
mkEnvironment declarations = foldM insert emptyEnvironment
  $ zip [0 ..] declarations

-- | Ground every explicit kind while preserving the environment's validated
-- declaration order and indexes. The source-ordered declaration pass fixes
-- which unsolved kind is reported first; after it succeeds, grounding the
-- declaration-valued indexes is total because they contain the same sealed
-- declarations. No namespace validation or reindexing is repeated.
groundEnvironmentKinds
  :: Environment typeVariable kindVariable annotation
  -> Either kindVariable (Environment typeVariable Void annotation)
groundEnvironmentKinds environment = do
  declarations <- mapM groundDeclarationKinds
    $ environmentDeclarations environment
  groundedTypes <- traverse groundDeclarationKinds
    $ typeDeclarationsByName environment
  groundedClasses <- traverse groundDeclarationKinds
    $ classesByName environment
  groundedInstances <- traverse groundDeclarationKinds
    $ instancesByHead environment
  pure Environment
    { reversedDeclarations = reverse declarations
    , typeDeclarationsByName = groundedTypes
    , valuesByName = valuesByName environment
    , constructorsByName = constructorsByName environment
    , classesByName = groundedClasses
    , instancesByHead = groundedInstances
    , canonicalInstanceHeads = canonicalInstanceHeads environment
    , occupiedValueNames = occupiedValueNames environment
    }

emptyEnvironment :: Environment typeVariable kindVariable annotation
emptyEnvironment = Environment [] Map.empty Map.empty Map.empty
  Map.empty Map.empty Set.empty Set.empty

insert
  :: Ord typeVariable
  => Environment typeVariable kindVariable annotation
  -> (Int, Declaration typeVariable kindVariable annotation)
  -> Either (EnvironmentError typeVariable)
      (Environment typeVariable kindVariable annotation)
insert environment (index, declaration) = do
  either (Left . InvalidEnvironmentDeclaration index) Right
    $ validateDeclaration declaration
  indexed <- case declaration of
    TypeSynonymDeclaration _ name _ _ -> insertType name declaration environment
    DataTypeDeclaration _ name _ constructors -> do
      withType <- insertType name declaration environment
      foldM (flip insertConstructor) withType constructors
    AbstractTypeDeclaration _ name _ -> insertType name declaration environment
    ValueDeclaration signature -> insertValue signature environment
    ClassDeclaration _ name _ _ methods -> do
      withType <- insertType name declaration environment
      withClass <- Right withType
        { classesByName = Map.insert name declaration
            $ classesByName withType
        }
      foldM (flip insertValue) withClass methods
    InstanceDeclaration _ variables _ headConstraint ->
      insertInstance variables headConstraint declaration environment
  Right indexed
    { reversedDeclarations = declaration : reversedDeclarations indexed }

insertType
  :: Name
  -> Declaration typeVariable kindVariable annotation
  -> Environment typeVariable kindVariable annotation
  -> Either (EnvironmentError typeVariable)
      (Environment typeVariable kindVariable annotation)
insertType name declaration environment
  | Map.member name $ typeDeclarationsByName environment =
      Left $ DuplicateTypeDeclaration name
  | otherwise = Right environment
      { typeDeclarationsByName = Map.insert name declaration
          $ typeDeclarationsByName environment
      }

insertValue
  :: ValueSignature typeVariable annotation
  -> Environment typeVariable kindVariable annotation
  -> Either (EnvironmentError typeVariable)
      (Environment typeVariable kindVariable annotation)
insertValue signature environment = do
  reserveValueName (valueName signature) environment
    { valuesByName = Map.insert (valueName signature) signature
        $ valuesByName environment
    }

insertConstructor
  :: DataConstructor typeVariable annotation
  -> Environment typeVariable kindVariable annotation
  -> Either (EnvironmentError typeVariable)
      (Environment typeVariable kindVariable annotation)
insertConstructor constructor environment = do
  reserveValueName (constructorName constructor) environment
    { constructorsByName = Map.insert (constructorName constructor) constructor
        $ constructorsByName environment
    }

reserveValueName
  :: Name
  -> Environment typeVariable kindVariable annotation
  -> Either (EnvironmentError typeVariable)
      (Environment typeVariable kindVariable annotation)
reserveValueName name environment
  | name `Set.member` occupiedValueNames environment =
      Left $ DuplicateValueDeclaration name
  | otherwise = Right environment
      { occupiedValueNames = Set.insert name $ occupiedValueNames environment }

insertInstance
  :: Ord typeVariable
  => [typeVariable]
  -> Constraint (Type typeVariable)
  -> Declaration typeVariable kindVariable annotation
  -> Environment typeVariable kindVariable annotation
  -> Either (EnvironmentError typeVariable)
      (Environment typeVariable kindVariable annotation)
insertInstance variables headConstraint declaration environment
  | canonicalHead `Set.member` canonicalInstanceHeads environment =
      Left $ DuplicateInstanceDeclaration headConstraint
  | otherwise = Right environment
      { instancesByHead = Map.insert headConstraint declaration
          $ instancesByHead environment
      , canonicalInstanceHeads = Set.insert canonicalHead
          $ canonicalInstanceHeads environment
      }
 where
  canonicalHead = canonicalizeInstanceHead variables headConstraint

canonicalizeInstanceHead
  :: Ord typeVariable
  => [typeVariable]
  -> Constraint (Type typeVariable)
  -> Constraint (Type (CanonicalInstanceVariable typeVariable))
canonicalizeInstanceHead variables headConstraint =
  evalState (canonicalizeInstanceConstraint bindings headConstraint) initialState
 where
  -- Instance binders form an implicit outer scope. Their declaration order is
  -- not semantically significant, so slots are allocated when the head first
  -- mentions each variable rather than from the binder list.
  bindings = Map.fromList [(variable, 0) | variable <- variables]
  initialState = CanonicalizationState Map.empty Map.empty 1

canonicalizeInstanceConstraint
  :: Ord typeVariable
  => Map typeVariable Natural
  -> Constraint (Type typeVariable)
  -> State (CanonicalizationState typeVariable)
      (Constraint (Type (CanonicalInstanceVariable typeVariable)))
canonicalizeInstanceConstraint bindings constraint = Constraint
  (constraintClass constraint)
  <$> mapM (canonicalizeInstanceType bindings) (constraintArguments constraint)

canonicalizeInstanceType
  :: Ord typeVariable
  => Map typeVariable Natural
  -> Type typeVariable
  -> State (CanonicalizationState typeVariable)
      (Type (CanonicalInstanceVariable typeVariable))
canonicalizeInstanceType bindings source = case source of
  TypeVariable variable -> TypeVariable
    <$> canonicalizeVariable bindings variable
  TypeConstructor name -> pure $ TypeConstructor name
  TypeApplication function argument -> TypeApplication
    <$> canonicalizeInstanceType bindings function
    <*> canonicalizeInstanceType bindings argument
  FunctionType parameter result -> FunctionType
    <$> canonicalizeInstanceType bindings parameter
    <*> canonicalizeInstanceType bindings result
  TupleType boxity elements -> TupleType boxity
    <$> mapM (canonicalizeInstanceType bindings) elements
  ForallType variables constraints body -> do
    scope <- allocateCanonicalScope
    let nestedBindings = Map.fromList
          [(variable, scope) | variable <- variables] `Map.union` bindings
    canonicalConstraints <- mapM
      (canonicalizeInstanceConstraint nestedBindings) constraints
    canonicalBody <- canonicalizeInstanceType nestedBindings body
    pure $ ForallType
      [ CanonicalBoundVariable scope slot
      | (slot, _) <- zip [0 ..] variables
      ] canonicalConstraints canonicalBody

canonicalizeVariable
  :: Ord typeVariable
  => Map typeVariable Natural
  -> typeVariable
  -> State (CanonicalizationState typeVariable)
      (CanonicalInstanceVariable typeVariable)
canonicalizeVariable bindings variable = case Map.lookup variable bindings of
  Nothing -> pure $ CanonicalFreeVariable variable
  Just scope -> do
    state <- get
    case Map.lookup (scope, variable) $ canonicalVariableSlots state of
      Just slot -> pure $ CanonicalBoundVariable scope slot
      Nothing -> do
        let slot = Map.findWithDefault 0 scope
              $ nextCanonicalSlotByScope state
        put state
          { canonicalVariableSlots = Map.insert (scope, variable) slot
              $ canonicalVariableSlots state
          , nextCanonicalSlotByScope = Map.insert scope (slot + 1)
              $ nextCanonicalSlotByScope state
          }
        pure $ CanonicalBoundVariable scope slot

allocateCanonicalScope
  :: State (CanonicalizationState typeVariable) Natural
allocateCanonicalScope = do
  state <- get
  let scope = nextCanonicalScope state
  put state { nextCanonicalScope = scope + 1 }
  pure scope

environmentDeclarations
  :: Environment typeVariable kindVariable annotation
  -> [Declaration typeVariable kindVariable annotation]
environmentDeclarations = reverse . reversedDeclarations

typeDeclarationMap
  :: Environment typeVariable kindVariable annotation
  -> Map Name (Declaration typeVariable kindVariable annotation)
typeDeclarationMap = typeDeclarationsByName

valueSignatureMap
  :: Environment typeVariable kindVariable annotation
  -> Map Name (ValueSignature typeVariable annotation)
valueSignatureMap = valuesByName

dataConstructorMap
  :: Environment typeVariable kindVariable annotation
  -> Map Name (DataConstructor typeVariable annotation)
dataConstructorMap = constructorsByName

classDeclarationMap
  :: Environment typeVariable kindVariable annotation
  -> Map Name (Declaration typeVariable kindVariable annotation)
classDeclarationMap = classesByName

instanceDeclarationMap
  :: Environment typeVariable kindVariable annotation
  -> Map (Constraint (Type typeVariable))
      (Declaration typeVariable kindVariable annotation)
instanceDeclarationMap = instancesByHead
