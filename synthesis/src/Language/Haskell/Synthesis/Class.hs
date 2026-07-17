-- | Prepared, backend-neutral class and instance views.
--
-- An 'Inventory' already proves structural validity and owns the exact class
-- kind assumptions inferred from its declarations.  This module pairs those
-- facts once, in source order, without retaining declaration annotations or
-- introducing a backend resolution policy.
module Language.Haskell.Synthesis.Class
  ( PreparedClassIndex
  , PreparedClass
  , PreparedInstance
  , prepareClassIndex
  , lookupPreparedClass
  , preparedClasses
  , preparedExplicitInstances
  , preparedClassName
  , preparedClassParameters
  , preparedClassSuperclasses
  , preparedClassMethods
  , preparedInstanceBinders
  , preparedInstancePrerequisites
  , preparedInstanceHead
  ) where

import Control.DeepSeq (NFData (rnf))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Language.Haskell.Synthesis.Constraint
import Language.Haskell.Synthesis.Declaration
import qualified Language.Haskell.Synthesis.Environment as Environment
import Language.Haskell.Synthesis.Inventory
import Language.Haskell.Synthesis.KindInference
import Language.Haskell.Synthesis.Name
import Language.Haskell.Synthesis.Type

-- 'Generic' is intentionally absent from all three opaque types: a generic
-- representation would let downstream callers forge values whose projections
-- did not come from one checked Inventory.
--
-- | One annotation-free index paired from a checked Inventory's declarations
-- and final class-kind assumptions.
data PreparedClassIndex variable = PreparedClassIndex
  !(Map Name (PreparedClass variable))
  ![PreparedClass variable]
  ![PreparedInstance variable]
  deriving (Eq, Show)

-- | One declared class in exact source shape, with final inferred parameter
-- kinds attached. The constructor is private.
data PreparedClass variable = PreparedClass
  !Name
  ![(variable, Maybe GroundKind)]
  [Constraint (Type variable)]
  [(Name, Type variable)]
  deriving (Eq, Show)

-- | One explicit source instance. Backend-inferred closure entries are never
-- represented here, and the constructor is private.
data PreparedInstance variable = PreparedInstance
  [variable]
  [Constraint (Type variable)]
  !(Constraint (Type variable))
  deriving (Eq, Show)

instance NFData variable => NFData (PreparedClassIndex variable) where
  rnf (PreparedClassIndex classes sourceClasses instances) =
    rnf classes `seq` rnf sourceClasses `seq` rnf instances

instance NFData variable => NFData (PreparedClass variable) where
  rnf (PreparedClass name parameters superclasses methods) =
    rnf name `seq` rnf parameters `seq` rnf superclasses `seq` rnf methods

instance NFData variable => NFData (PreparedInstance variable) where
  rnf (PreparedInstance variables prerequisites headConstraint) =
    rnf variables `seq` rnf prerequisites `seq` rnf headConstraint

-- | Pair every declared class with the final kind of each parameter, and
-- retain explicit instances in declaration order.  Preparation is total for
-- an opaque 'Inventory': its constructor establishes that the class-kind map
-- contains exactly one arity-matched entry for every declared class.
--
-- Open-inventory assumptions for nominal external classes are deliberately
-- ignored because they have no source declaration to prepare.
prepareClassIndex
  :: Inventory variable annotation
  -> PreparedClassIndex variable
prepareClassIndex inventory = PreparedClassIndex
  (Map.fromList [(preparedClassName entry, entry) | entry <- classes])
  classes instances
 where
  declarations = Environment.environmentDeclarations
    $ inventoryEnvironment inventory
  assumptions = classParameterKinds $ inventoryKindAssumptions inventory
  ClassIndexBuilder reversedClasses reversedInstances = foldl' collect
    (ClassIndexBuilder [] []) declarations
  classes = reverse reversedClasses
  instances = reverse reversedInstances

  collect (ClassIndexBuilder classEntries instanceEntries) declaration =
    case declaration of
      ClassDeclaration _ name parameters superclasses methods ->
        let entry = PreparedClass name
              (pairParameterKinds assumptions name parameters)
              superclasses
              [ (valueName method, valueType method)
              | method <- methods
              ]
        in ClassIndexBuilder (entry : classEntries) instanceEntries
      InstanceDeclaration _ variables prerequisites headConstraint ->
        let entry = PreparedInstance variables prerequisites headConstraint
        in ClassIndexBuilder classEntries (entry : instanceEntries)
      _ -> ClassIndexBuilder classEntries instanceEntries

data ClassIndexBuilder variable = ClassIndexBuilder
  ![PreparedClass variable]
  ![PreparedInstance variable]

pairParameterKinds
  :: Map Name [Maybe GroundKind]
  -> Name
  -> [TypeParameter variable kindVariable]
  -> [(variable, Maybe GroundKind)]
pairParameterKinds assumptions name parameters = case Map.lookup name assumptions of
  Nothing -> inventoryInvariant $ "missing class-kind entry for "
    ++ renderCanonical name
  Just kinds
    | length kinds == length parameters ->
        zip (map parameterVariable parameters) kinds
    | otherwise -> inventoryInvariant $
        "class-kind arity changed for " ++ renderCanonical name ++
        ": declaration has " ++ show (length parameters) ++
        ", assumptions have " ++ show (length kinds)

inventoryInvariant :: String -> value
inventoryInvariant detail = error $
  "Language.Haskell.Synthesis.Class.prepareClassIndex: checked Inventory "
    ++ "invariant failed: " ++ detail

-- | Exact nominal lookup in the prepared declared-class map.
lookupPreparedClass
  :: Name
  -> PreparedClassIndex variable
  -> Maybe (PreparedClass variable)
lookupPreparedClass name (PreparedClassIndex classes _ _) =
  Map.lookup name classes

-- | Declared classes in source order, independently of nominal map ordering.
preparedClasses :: PreparedClassIndex variable -> [PreparedClass variable]
preparedClasses (PreparedClassIndex _ classes _) = classes

-- | Explicit instance declarations in source order.  No superclass-derived
-- instances or backend closure facts are introduced here.
preparedExplicitInstances
  :: PreparedClassIndex variable
  -> [PreparedInstance variable]
preparedExplicitInstances (PreparedClassIndex _ _ instances) = instances

-- | The declared nominal class name.
preparedClassName :: PreparedClass variable -> Name
preparedClassName (PreparedClass name _ _ _) = name

-- | Declaration-order parameters paired with their final inferred kind.
-- 'Nothing' denotes an intentionally generalized parameter, not a missing
-- kind fact.
preparedClassParameters
  :: PreparedClass variable
  -> [(variable, Maybe GroundKind)]
preparedClassParameters (PreparedClass _ parameters _ _) = parameters

-- | Superclass constraints in declaration order and source shape.
preparedClassSuperclasses
  :: PreparedClass variable
  -> [Constraint (Type variable)]
preparedClassSuperclasses (PreparedClass _ _ superclasses _) = superclasses

-- | Method names and types in declaration order, with annotations erased.
preparedClassMethods
  :: PreparedClass variable
  -> [(Name, Type variable)]
preparedClassMethods (PreparedClass _ _ _ methods) = methods

-- | Explicit instance binders in declaration order.
preparedInstanceBinders :: PreparedInstance variable -> [variable]
preparedInstanceBinders (PreparedInstance variables _ _) = variables

-- | Instance prerequisites in declaration order and source shape.
preparedInstancePrerequisites
  :: PreparedInstance variable
  -> [Constraint (Type variable)]
preparedInstancePrerequisites (PreparedInstance _ prerequisites _) = prerequisites

-- | The exact explicit source head.
preparedInstanceHead
  :: PreparedInstance variable
  -> Constraint (Type variable)
preparedInstanceHead (PreparedInstance _ _ headConstraint) = headConstraint
