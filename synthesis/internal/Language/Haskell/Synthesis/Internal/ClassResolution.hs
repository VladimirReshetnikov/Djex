{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Package-private class-resolution authority with explicit admission,
-- retention, and proof-search bounds.
--
-- The neutral declaration inventory intentionally does not choose a class
-- solver.  This module is the first shared executable policy: it seals one
-- exact checked inventory into a closed declared-class environment and can
-- discharge alias-free, first-order, ground constraints without givens.
--
-- Type synonyms are not expanded here because a generic inventory variable
-- has no package-owned fresh-name allocator.  Any synonym mentioned by a
-- superclass, instance, or query is therefore rejected explicitly.  Nested
-- 'ForallType's are likewise outside this first checkpoint.  These
-- restrictions keep directional instance substitution capture-free while
-- still covering ordinary, higher-kinded, function, and tuple instance
-- heads.
--
-- A successful t'CheckedConstraintDischarge' retains the exact checked
-- resolver environment.  Its proof can be recovered only by replaying the
-- receipt against that environment and the same canonical goal; a genuine
-- receipt cannot consequently be paired with unrelated class authority.
-- This module is not exported by the public Djex facade, and neither Length
-- nor Z3 consumes it yet.
module Language.Haskell.Synthesis.Internal.ClassResolution
  ( ClassResolutionLimitField (..)
  , ClassResolutionLimitError (..)
  , ClassResolutionLimits
  , mkClassResolutionLimits
  , defaultClassResolutionLimits
  , maximumClassResolutionDeclarations
  , maximumClassResolutionClasses
  , maximumClassResolutionInstances
  , maximumClassResolutionTypeConstructorKinds
  , maximumClassResolutionCollectionWidth
  , maximumClassResolutionTypeNodes
  , maximumClassResolutionKindNodes
  , maximumClassResolutionOverlapComparisons
  , maximumClassResolutionProofDepth
  , maximumClassResolutionProofNodes
  , ClassResolutionCollectionSite (..)
  , ClassResolutionKindSite (..)
  , ClassResolutionConstraintSite (..)
  , ClassResolutionConstraintError (..)
  , ClassResolutionEnvironmentError (..)
  , CheckedClassResolutionEnvironment
  , sealClassResolutionEnvironment
  , ClassResolutionQueryError (..)
  , ClassResolutionProof
  , classResolutionProofGoal
  , classResolutionProofInstanceHead
  , classResolutionProofPrerequisites
  , CheckedConstraintDischarge
  , checkedConstraintDischargeGoal
  , dischargeGroundConstraint
  , ClassResolutionReplayMismatch (..)
  , replayCheckedConstraintDischarge
  ) where

import Control.DeepSeq (NFData (rnf))
import Control.Monad (foldM, guard)
import Data.Graph (SCC (..), stronglyConnComp)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Numeric.Natural (Natural)

import qualified Language.Haskell.Synthesis.Class as Class
import Language.Haskell.Synthesis.Collection
  ( observedListLength )
import Language.Haskell.Synthesis.Constraint
import Language.Haskell.Synthesis.Declaration
import qualified Language.Haskell.Synthesis.Environment as Environment
import Language.Haskell.Synthesis.Inventory
import qualified Language.Haskell.Synthesis.Kind as Kind
import Language.Haskell.Synthesis.KindInference
import Language.Haskell.Synthesis.Name
import Language.Haskell.Synthesis.Type
import Language.Haskell.Synthesis.TypedGenerated
  ( TypeStructure (observeTypeWithin)
  , TypeStructureLimitError (..)
  , sharedTypeStructure
  )

-- | Independently configurable bounds for resolver sealing and discharge.
data ClassResolutionLimitField
  = ClassResolutionDeclarations
  | ClassResolutionClasses
  | ClassResolutionInstances
  | ClassResolutionTypeConstructorKinds
  | ClassResolutionCollectionWidth
  | ClassResolutionTypeNodes
  | ClassResolutionKindNodes
  | ClassResolutionOverlapComparisons
  | ClassResolutionProofDepth
  | ClassResolutionProofNodes
  deriving (Bounded, Enum, Eq, Ord, Show)

instance NFData ClassResolutionLimitField where
  rnf value = value `seq` ()

data ClassResolutionLimitError
  = NegativeClassResolutionLimit !ClassResolutionLimitField !Int
  deriving (Eq, Ord, Show)

instance NFData ClassResolutionLimitError where
  rnf (NegativeClassResolutionLimit field value) =
    rnf field `seq` rnf value

-- 'Generic' is deliberately absent.  The constructor is hidden so callers
-- cannot bypass nonnegative validation or silently reorder limit fields.
data ClassResolutionLimits = ClassResolutionLimits
  !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
  deriving (Eq, Ord, Show)

instance NFData ClassResolutionLimits where
  rnf (ClassResolutionLimits declarations classes instances constructorKinds
      width typeNodes kindNodes overlapComparisons depth proofs) =
    rnf declarations `seq` rnf classes `seq` rnf instances `seq`
      rnf constructorKinds `seq` rnf width `seq` rnf typeNodes `seq`
      rnf kindNodes `seq` rnf overlapComparisons `seq` rnf depth `seq`
      rnf proofs

mkClassResolutionLimits
  :: Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Either ClassResolutionLimitError ClassResolutionLimits
mkClassResolutionLimits declarations classes instances constructorKinds width
    typeNodes kindNodes overlapComparisons depth proofs
  | declarations < 0 = negative ClassResolutionDeclarations declarations
  | classes < 0 = negative ClassResolutionClasses classes
  | instances < 0 = negative ClassResolutionInstances instances
  | constructorKinds < 0 =
      negative ClassResolutionTypeConstructorKinds constructorKinds
  | width < 0 = negative ClassResolutionCollectionWidth width
  | typeNodes < 0 = negative ClassResolutionTypeNodes typeNodes
  | kindNodes < 0 = negative ClassResolutionKindNodes kindNodes
  | overlapComparisons < 0 =
      negative ClassResolutionOverlapComparisons overlapComparisons
  | depth < 0 = negative ClassResolutionProofDepth depth
  | proofs < 0 = negative ClassResolutionProofNodes proofs
  | otherwise = Right $ ClassResolutionLimits
      declarations classes instances constructorKinds width typeNodes
      kindNodes overlapComparisons depth proofs
 where
  negative field value = Left $ NegativeClassResolutionLimit field value

-- | Conservative defaults for one finite declaration inventory and proof.
defaultClassResolutionLimits :: ClassResolutionLimits
defaultClassResolutionLimits = ClassResolutionLimits
  32768 4096 16384 32768 256 4096 256 262144 256 4096

maximumClassResolutionDeclarations :: ClassResolutionLimits -> Int
maximumClassResolutionDeclarations
    (ClassResolutionLimits value _ _ _ _ _ _ _ _ _) = value

maximumClassResolutionClasses :: ClassResolutionLimits -> Int
maximumClassResolutionClasses
    (ClassResolutionLimits _ value _ _ _ _ _ _ _ _) = value

maximumClassResolutionInstances :: ClassResolutionLimits -> Int
maximumClassResolutionInstances
    (ClassResolutionLimits _ _ value _ _ _ _ _ _ _) = value

maximumClassResolutionTypeConstructorKinds :: ClassResolutionLimits -> Int
maximumClassResolutionTypeConstructorKinds
    (ClassResolutionLimits _ _ _ value _ _ _ _ _ _) = value

maximumClassResolutionCollectionWidth :: ClassResolutionLimits -> Int
maximumClassResolutionCollectionWidth
    (ClassResolutionLimits _ _ _ _ value _ _ _ _ _) = value

maximumClassResolutionTypeNodes :: ClassResolutionLimits -> Int
maximumClassResolutionTypeNodes
    (ClassResolutionLimits _ _ _ _ _ value _ _ _ _) = value

maximumClassResolutionKindNodes :: ClassResolutionLimits -> Int
maximumClassResolutionKindNodes
    (ClassResolutionLimits _ _ _ _ _ _ value _ _ _) = value

maximumClassResolutionOverlapComparisons :: ClassResolutionLimits -> Int
maximumClassResolutionOverlapComparisons
    (ClassResolutionLimits _ _ _ _ _ _ _ value _ _) = value

maximumClassResolutionProofDepth :: ClassResolutionLimits -> Int
maximumClassResolutionProofDepth
    (ClassResolutionLimits _ _ _ _ _ _ _ _ value _) = value

maximumClassResolutionProofNodes :: ClassResolutionLimits -> Int
maximumClassResolutionProofNodes
    (ClassResolutionLimits _ _ _ _ _ _ _ _ _ value) = value

-- | A declaration-owned list whose width is bounded before its payload.
data ClassResolutionCollectionSite
  = ClassResolutionClassParameters !Name
  | ClassResolutionClassSuperclasses !Name
  | ClassResolutionInstanceBinders !Natural
  | ClassResolutionInstancePrerequisites !Natural
  | ClassResolutionCompletedInstancePrerequisites !Natural
  deriving (Eq, Ord, Show)

instance NFData ClassResolutionCollectionSite where
  rnf site = case site of
    ClassResolutionClassParameters name -> rnf name
    ClassResolutionClassSuperclasses name -> rnf name
    ClassResolutionInstanceBinders ordinal -> rnf ordinal
    ClassResolutionInstancePrerequisites ordinal -> rnf ordinal
    ClassResolutionCompletedInstancePrerequisites ordinal -> rnf ordinal

-- | A retained kind tree whose node count is bounded before deep retention.
data ClassResolutionKindSite
  = ClassResolutionTypeConstructorKind !Name
  | ClassResolutionClassParameterKind !Name !Natural
  deriving (Eq, Ord, Show)

instance NFData ClassResolutionKindSite where
  rnf site = case site of
    ClassResolutionTypeConstructorKind name -> rnf name
    ClassResolutionClassParameterKind name ordinal ->
      rnf name `seq` rnf ordinal

-- | Exact semantic location of one retained or caller-supplied constraint.
data ClassResolutionConstraintSite
  = ClassResolutionSuperclass !Name !Natural
  | ClassResolutionInstanceHead !Natural
  | ClassResolutionInstancePrerequisite !Natural !Natural
  | ClassResolutionDerivedInstanceSuperclass !Natural !Natural
  | ClassResolutionGroundQuery
  deriving (Eq, Ord, Show)

instance NFData ClassResolutionConstraintSite where
  rnf site = case site of
    ClassResolutionSuperclass name ordinal -> rnf name `seq` rnf ordinal
    ClassResolutionInstanceHead ordinal -> rnf ordinal
    ClassResolutionInstancePrerequisite instanceOrdinal prerequisiteOrdinal ->
      rnf instanceOrdinal `seq` rnf prerequisiteOrdinal
    ClassResolutionDerivedInstanceSuperclass instanceOrdinal
        superclassOrdinal ->
      rnf instanceOrdinal `seq` rnf superclassOrdinal
    ClassResolutionGroundQuery -> ()

-- | A bounded structural, nominal, or kind failure in one constraint.
data ClassResolutionConstraintError variable
  = InvalidClassResolutionConstraintClass !ConstraintError
  | UnknownClassResolutionClass !Name
  | ClassResolutionConstraintArityMismatch !Name !Int !Int
  | ClassResolutionConstraintTypeNodeLimitExceeded
      !Natural !Int !Int
    -- ^ Argument ordinal, maximum, observed at least.
  | ClassResolutionConstraintTypeCollectionLimitExceeded
      !Natural !Int !Int
  | InvalidClassResolutionConstraintType
      !Natural !(TypeError variable)
  | ClassResolutionConstraintContainsTypeSynonym !Natural !Name
  | ClassResolutionConstraintForallUnsupported !Natural
  | IllKindedClassResolutionConstraint !(KindInferenceError variable)
  deriving (Eq, Ord, Show)

instance NFData variable => NFData (ClassResolutionConstraintError variable) where
  rnf failure = case failure of
    InvalidClassResolutionConstraintClass cause -> rnf cause
    UnknownClassResolutionClass name -> rnf name
    ClassResolutionConstraintArityMismatch name expected observed ->
      rnf name `seq` rnf expected `seq` rnf observed
    ClassResolutionConstraintTypeNodeLimitExceeded
        ordinal maximumAllowed observed ->
      rnf ordinal `seq` rnf maximumAllowed `seq` rnf observed
    ClassResolutionConstraintTypeCollectionLimitExceeded
        ordinal maximumAllowed observed ->
      rnf ordinal `seq` rnf maximumAllowed `seq` rnf observed
    InvalidClassResolutionConstraintType ordinal cause ->
      rnf ordinal `seq` rnf cause
    ClassResolutionConstraintContainsTypeSynonym ordinal name ->
      rnf ordinal `seq` rnf name
    ClassResolutionConstraintForallUnsupported ordinal -> rnf ordinal
    IllKindedClassResolutionConstraint cause -> rnf cause

data ClassResolutionEnvironmentError variable
  = ClassResolutionDeclarationLimitExceeded !Int !Int
  | ClassResolutionClassLimitExceeded !Int !Int
  | ClassResolutionInstanceLimitExceeded !Int !Int
  | ClassResolutionTypeConstructorKindLimitExceeded !Int !Int
  | ClassResolutionKindNodeLimitExceeded
      !ClassResolutionKindSite !Int !Int
  | ClassResolutionCollectionLimitExceeded
      !ClassResolutionCollectionSite !Int !Int
  | InvalidClassResolutionEnvironmentConstraint
      !ClassResolutionConstraintSite
      !(ClassResolutionConstraintError variable)
  | DuplicateClassResolutionInstanceHead
      !(Constraint (Type variable))
  | ClassResolutionOverlapComparisonLimitExceeded !Int !Int
  | OverlappingClassResolutionInstanceHeads
      !(Constraint (Type variable)) !(Constraint (Type variable))
  | ClassResolutionSuperclassCycle ![Name]
  | ExpandingClassResolutionInstancePrerequisite
      !Natural
      !(Constraint (Type variable))
      !(Constraint (Type variable))
  deriving (Eq, Ord, Show)

instance NFData variable => NFData (ClassResolutionEnvironmentError variable) where
  rnf failure = case failure of
    ClassResolutionDeclarationLimitExceeded maximumAllowed observed ->
      rnf maximumAllowed `seq` rnf observed
    ClassResolutionClassLimitExceeded maximumAllowed observed ->
      rnf maximumAllowed `seq` rnf observed
    ClassResolutionInstanceLimitExceeded maximumAllowed observed ->
      rnf maximumAllowed `seq` rnf observed
    ClassResolutionTypeConstructorKindLimitExceeded maximumAllowed observed ->
      rnf maximumAllowed `seq` rnf observed
    ClassResolutionKindNodeLimitExceeded site maximumAllowed observed ->
      rnf site `seq` rnf maximumAllowed `seq` rnf observed
    ClassResolutionCollectionLimitExceeded site maximumAllowed observed ->
      rnf site `seq` rnf maximumAllowed `seq` rnf observed
    InvalidClassResolutionEnvironmentConstraint site cause ->
      rnf site `seq` rnf cause
    DuplicateClassResolutionInstanceHead headConstraint -> rnf headConstraint
    ClassResolutionOverlapComparisonLimitExceeded maximumAllowed observed ->
      rnf maximumAllowed `seq` rnf observed
    OverlappingClassResolutionInstanceHeads left right ->
      rnf left `seq` rnf right
    ClassResolutionSuperclassCycle names -> rnf names
    ExpandingClassResolutionInstancePrerequisite ordinal headConstraint
        prerequisite ->
      rnf ordinal `seq` rnf headConstraint `seq` rnf prerequisite

data ResolutionClass variable = ResolutionClass
  !Name ![variable] ![Constraint (Type variable)]
  deriving (Eq)

instance NFData variable => NFData (ResolutionClass variable) where
  rnf (ResolutionClass name parameters superclasses) =
    rnf name `seq` rnf parameters `seq` rnf superclasses

data ResolutionInstance variable = ResolutionInstance
  !Natural
  ![variable]
  !(Set variable)
  ![Constraint (Type variable)]
  !(Constraint (Type variable))
  deriving (Eq)

instance NFData variable => NFData (ResolutionInstance variable) where
  rnf (ResolutionInstance ordinal binders binderSet prerequisites
      headConstraint) =
    rnf ordinal `seq` rnf binders `seq` rnf binderSet `seq`
      rnf prerequisites `seq` rnf headConstraint

-- 'Generic' and the constructor are intentionally absent: the list, maps,
-- assumptions, alias set, and limits must all derive from one sealing pass.
data CheckedClassResolutionEnvironment variable =
  CheckedClassResolutionEnvironment
    !ClassResolutionLimits
    !KindAssumptions
    !(Set Name)
    ![ResolutionClass variable]
    !(Map Name (ResolutionClass variable))
    ![ResolutionInstance variable]
    !(Map Name [ResolutionInstance variable])

type role CheckedClassResolutionEnvironment nominal

instance NFData variable =>
    NFData (CheckedClassResolutionEnvironment variable) where
  rnf (CheckedClassResolutionEnvironment limits assumptions aliases classes
      classMap instances instanceMap) =
    rnf limits `seq` rnf assumptions `seq` rnf aliases `seq` rnf classes `seq`
      rnf classMap `seq` rnf instances `seq` rnf instanceMap

-- | Seal the resolver-specific closed-world policy from one exact Inventory.
sealClassResolutionEnvironment
  :: Ord variable
  => ClassResolutionLimits
  -> Inventory variable annotation
  -> Either (ClassResolutionEnvironmentError variable)
      (CheckedClassResolutionEnvironment variable)
sealClassResolutionEnvironment limits inventory = do
  let rawEnvironment = inventoryEnvironment inventory
      rawDeclarations = Environment.environmentDeclarations rawEnvironment
      declarationMaximum = maximumClassResolutionDeclarations limits
      observedDeclarations = observedListLength
        declarationMaximum rawDeclarations
  if observedDeclarations > declarationMaximum
    then Left $ ClassResolutionDeclarationLimitExceeded
      declarationMaximum observedDeclarations
    else pure ()
  let classMaximum = maximumClassResolutionClasses limits
      instanceMaximum = maximumClassResolutionInstances limits
      observedClasses = observedDeclarationCount
        classMaximum isClassDeclaration rawDeclarations
      observedInstances = observedDeclarationCount
        instanceMaximum isInstanceDeclaration rawDeclarations
  if observedClasses > classMaximum
    then Left $ ClassResolutionClassLimitExceeded
      classMaximum observedClasses
    else pure ()
  if observedInstances > instanceMaximum
    then Left $ ClassResolutionInstanceLimitExceeded
      instanceMaximum observedInstances
    else pure ()
  -- Inspect each class arity directly from the already checked declaration
  -- stream before 'prepareClassIndex' pairs it with retained kind metadata.
  -- This keeps a wide parameter spine behind the resolver's own width gate.
  mapM_ observeDeclarationClassParameters rawDeclarations
  let prepared = Class.prepareClassIndex inventory
      rawClasses = Class.preparedClasses prepared
      rawInstances = Class.preparedExplicitInstances prepared
  observeTypeConstructorKindTable rawAssumptions
  -- Every declared arity is needed for forward superclass references. Bound
  -- all parameter lists first, then preserve class-constraint and instance
  -- source order for every remaining failure.
  mapM_ observeRawClassParameters rawClasses
  let classArities = Map.fromList
        [ (Class.preparedClassName source,
            length $ Class.preparedClassParameters source)
        | source <- rawClasses
        ]
      assumptions = KindAssumptions
        { typeConstructorKinds = typeConstructorKinds rawAssumptions
        , classParameterKinds = Map.fromList
            [ (Class.preparedClassName source,
                map snd $ Class.preparedClassParameters source)
            | source <- rawClasses
            ]
        }
  classes <- mapM (prepareClass assumptions classArities) rawClasses
  let classMap = Map.fromList
        [(resolutionClassName source, source) | source <- classes]
  instances <- mapM (uncurry $ prepareInstance assumptions classArities)
    $ zip [0 ..] rawInstances
  rejectDuplicateHeads instances
  rejectOverlappingHeads limits instances
  rejectSuperclassCycles classes
  completed <- mapM
    (completeInstance limits assumptions aliases classArities classMap)
    instances
  mapM_ rejectExpandingPrerequisites completed
  let indexed = Map.map reverse
        $ foldl' insertInstance Map.empty completed
  pure $ CheckedClassResolutionEnvironment
    limits assumptions aliases classes classMap completed indexed
 where
  rawAssumptions = inventoryKindAssumptions inventory
  aliases = Set.fromList
    [ name
    | (name, TypeSynonymDeclaration{}) <- Map.toAscList
        $ Environment.typeDeclarationMap $ inventoryEnvironment inventory
    ]

  prepareClass assumptions classArities source = do
    let name = Class.preparedClassName source
        rawParameters = Class.preparedClassParameters source
        rawSuperclasses = Class.preparedClassSuperclasses source
    observeCollection
      (ClassResolutionClassSuperclasses name) rawSuperclasses
    superclasses <- mapM
      (uncurry $ prepareEnvironmentConstraint limits assumptions aliases
        (`Map.lookup` classArities))
      [ (ClassResolutionSuperclass name ordinal, superclass)
      | (ordinal, superclass) <- zip [0 ..] rawSuperclasses
      ]
    pure $ ResolutionClass name (map fst rawParameters) superclasses

  prepareInstance assumptions classArities ordinal source = do
    let rawBinders = Class.preparedInstanceBinders source
        rawPrerequisites = Class.preparedInstancePrerequisites source
        rawHead = Class.preparedInstanceHead source
    observeCollection
      (ClassResolutionInstanceBinders ordinal) rawBinders
    observeCollection
      (ClassResolutionInstancePrerequisites ordinal) rawPrerequisites
    headConstraint <- prepareEnvironmentConstraint
      limits assumptions aliases (`Map.lookup` classArities)
      (ClassResolutionInstanceHead ordinal) rawHead
    prerequisites <- mapM
      (uncurry $ prepareEnvironmentConstraint
        limits assumptions aliases (`Map.lookup` classArities))
      [ (ClassResolutionInstancePrerequisite ordinal prerequisiteOrdinal
        , prerequisite)
      | (prerequisiteOrdinal, prerequisite) <- zip [0 ..] rawPrerequisites
      ]
    pure $ ResolutionInstance ordinal rawBinders (Set.fromList rawBinders)
      prerequisites headConstraint

  observeRawClassParameters source = do
    let name = Class.preparedClassName source
        parameters = Class.preparedClassParameters source
    mapM_ (uncurry $ observeMaybeKind .
      ClassResolutionClassParameterKind name)
      $ zip [0 ..] $ map snd parameters

  observeDeclarationClassParameters declaration = case declaration of
    ClassDeclaration _ name parameters _ _ ->
      observeCollection (ClassResolutionClassParameters name) parameters
    _ -> Right ()

  isClassDeclaration declaration = case declaration of
    ClassDeclaration{} -> True
    _ -> False

  isInstanceDeclaration declaration = case declaration of
    InstanceDeclaration{} -> True
    _ -> False

  observedDeclarationCount maximumExpected shouldCount = go 0
   where
    go observed [] = observed
    go observed (declaration : remaining)
      | shouldCount declaration
      , observed >= maximumExpected = observedBeyond maximumExpected
      | shouldCount declaration = go (observed + 1) remaining
      | otherwise = go observed remaining

    observedBeyond maximumAllowed
      | maximumAllowed == maxBound = maxBound
      | otherwise = maximumAllowed + 1

  observeTypeConstructorKindTable source = do
    let kinds = typeConstructorKinds source
        maximumKinds = maximumClassResolutionTypeConstructorKinds limits
        observedKinds = Map.size kinds
    if observedKinds > maximumKinds
      then Left $ ClassResolutionTypeConstructorKindLimitExceeded
        maximumKinds observedKinds
      else pure ()
    mapM_ (uncurry $ observeKind . ClassResolutionTypeConstructorKind)
      $ Map.toAscList kinds

  observeMaybeKind _ Nothing = Right ()
  observeMaybeKind site (Just kind) = observeKind site kind

  observeKind site kind =
    let maximumNodes = maximumClassResolutionKindNodes limits
        observed = Kind.observedKindNodeCount maximumNodes kind
    in if observed <= maximumNodes
        then Right ()
        else Left $ ClassResolutionKindNodeLimitExceeded
          site maximumNodes observed

  observeCollection site values =
    let maximumWidth = maximumClassResolutionCollectionWidth limits
        observed = observedListLength maximumWidth values
    in if observed <= maximumWidth
        then Right ()
        else Left $ ClassResolutionCollectionLimitExceeded
          site maximumWidth observed

  insertInstance table source = Map.insertWith (++)
    (constraintClass $ resolutionInstanceHead source) [source] table

-- | A malformed query or an exhausted finite proof budget.
data ClassResolutionQueryError variable
  = InvalidClassResolutionGroundConstraint
      !(ClassResolutionConstraintError variable)
  | InvalidClassResolutionDerivedConstraint
      !Natural !Natural !(ClassResolutionConstraintError variable)
    -- ^ Source instance ordinal, completed-prerequisite ordinal, cause.
  | ClassResolutionGroundConstraintHasFreeVariables ![variable]
  | ClassResolutionProofDepthLimitExceeded !Int !Int
  | ClassResolutionProofNodeLimitExceeded !Int !Int
  deriving (Eq, Ord, Show)

instance NFData variable => NFData (ClassResolutionQueryError variable) where
  rnf failure = case failure of
    InvalidClassResolutionGroundConstraint cause -> rnf cause
    InvalidClassResolutionDerivedConstraint instanceOrdinal
        prerequisiteOrdinal cause ->
      rnf instanceOrdinal `seq` rnf prerequisiteOrdinal `seq` rnf cause
    ClassResolutionGroundConstraintHasFreeVariables variables -> rnf variables
    ClassResolutionProofDepthLimitExceeded maximumAllowed observed ->
      rnf maximumAllowed `seq` rnf observed
    ClassResolutionProofNodeLimitExceeded maximumAllowed observed ->
      rnf maximumAllowed `seq` rnf observed

-- | Diagnostic observation of one canonical instance proof.  The constructor
-- is hidden; possession of this value is granted only by discharge or replay.
data ClassResolutionProof variable = ClassResolutionProof
  !(Constraint (Type variable))
  !(Constraint (Type variable))
  ![ClassResolutionProof variable]

type role ClassResolutionProof nominal

instance NFData variable => NFData (ClassResolutionProof variable) where
  rnf (ClassResolutionProof goal headConstraint prerequisites) =
    rnf goal `seq` rnf headConstraint `seq` rnf prerequisites

classResolutionProofGoal
  :: ClassResolutionProof variable
  -> Constraint (Type variable)
classResolutionProofGoal (ClassResolutionProof goal _ _) = goal

-- | Canonical source instance head selected for this proof node.  This is a
-- diagnostic observation; replay association, not the detached tree, grants
-- discharge authority.
classResolutionProofInstanceHead
  :: ClassResolutionProof variable
  -> Constraint (Type variable)
classResolutionProofInstanceHead (ClassResolutionProof _ headConstraint _) =
  headConstraint

classResolutionProofPrerequisites
  :: ClassResolutionProof variable
  -> [ClassResolutionProof variable]
classResolutionProofPrerequisites (ClassResolutionProof _ _ prerequisites) =
  prerequisites

-- | One unforgeable proof paired with the exact checked resolver environment.
data CheckedConstraintDischarge variable = CheckedConstraintDischarge
  !(CheckedClassResolutionEnvironment variable)
  !(Constraint (Type variable))
  !(ClassResolutionProof variable)

type role CheckedConstraintDischarge nominal

instance NFData variable => NFData (CheckedConstraintDischarge variable) where
  rnf (CheckedConstraintDischarge environment goal proof) =
    rnf environment `seq` rnf goal `seq` rnf proof

checkedConstraintDischargeGoal
  :: CheckedConstraintDischarge variable
  -> Constraint (Type variable)
checkedConstraintDischargeGoal (CheckedConstraintDischarge _ goal _) = goal

-- | Resolve one checked, alias-free, first-order ground constraint.  A normal
-- absence of evidence (including a current-path cycle or prerequisite-only
-- instance binder) is @Right Nothing@.
dischargeGroundConstraint
  :: Ord variable
  => CheckedClassResolutionEnvironment variable
  -> Constraint (Type variable)
  -> Either (ClassResolutionQueryError variable)
      (Maybe (CheckedConstraintDischarge variable))
dischargeGroundConstraint environment rawGoal = do
  goal <- prepareGroundQuery environment rawGoal
  (proof, _) <- resolveConstraint environment Set.empty 0 0 goal
  pure $ CheckedConstraintDischarge environment goal <$> proof

data ClassResolutionReplayMismatch variable
  = ClassResolutionReplayEnvironmentMismatch
  | ClassResolutionReplayGoalRejected
      !(ClassResolutionQueryError variable)
  | ClassResolutionReplayGoalMismatch
      !(Constraint (Type variable)) !(Constraint (Type variable))
  deriving (Eq, Ord, Show)

instance NFData variable => NFData (ClassResolutionReplayMismatch variable) where
  rnf mismatch = case mismatch of
    ClassResolutionReplayEnvironmentMismatch -> ()
    ClassResolutionReplayGoalRejected cause -> rnf cause
    ClassResolutionReplayGoalMismatch expected actual ->
      rnf expected `seq` rnf actual

-- | Recover proof authority only after exact environment association and
-- canonical-goal replay.  The environment check deliberately precedes all
-- inspection of the replay goal.
replayCheckedConstraintDischarge
  :: Ord variable
  => CheckedClassResolutionEnvironment variable
  -> Constraint (Type variable)
  -> CheckedConstraintDischarge variable
  -> Either (ClassResolutionReplayMismatch variable)
      (ClassResolutionProof variable)
replayCheckedConstraintDischarge environment rawGoal
    (CheckedConstraintDischarge retainedEnvironment expected proof)
  | not $ sameClassResolutionEnvironment environment retainedEnvironment =
      Left ClassResolutionReplayEnvironmentMismatch
  | otherwise = do
      actual <- either (Left . ClassResolutionReplayGoalRejected) Right
        $ prepareGroundQuery environment rawGoal
      if actual == expected
        then Right proof
        else Left $ ClassResolutionReplayGoalMismatch expected actual

prepareEnvironmentConstraint
  :: Ord variable
  => ClassResolutionLimits
  -> KindAssumptions
  -> Set Name
  -> (Name -> Maybe Int)
  -> ClassResolutionConstraintSite
  -> Constraint (Type variable)
  -> Either (ClassResolutionEnvironmentError variable)
      (Constraint (Type variable))
prepareEnvironmentConstraint limits assumptions aliases lookupArity site source =
  either (Left . InvalidClassResolutionEnvironmentConstraint site) Right $ do
    canonical <- prepareConstraintStructure
      limits aliases lookupArity source
    validateConstraintKinds assumptions canonical
    pure canonical

prepareGroundQuery
  :: Ord variable
  => CheckedClassResolutionEnvironment variable
  -> Constraint (Type variable)
  -> Either (ClassResolutionQueryError variable)
      (Constraint (Type variable))
prepareGroundQuery
    (CheckedClassResolutionEnvironment limits assumptions aliases _ classes
      _ _) source = do
  canonical <- either (Left . InvalidClassResolutionGroundConstraint) Right
    $ prepareConstraintStructure limits aliases
        (fmap (length . resolutionClassParameters) . (`Map.lookup` classes))
        source
  case Set.toAscList $ constraintFreeVariables canonical of
    [] -> do
      either (Left . InvalidClassResolutionGroundConstraint) Right
        $ validateConstraintKinds assumptions canonical
      Right canonical
    variables -> Left $ ClassResolutionGroundConstraintHasFreeVariables variables

prepareConstraintStructure
  :: Ord variable
  => ClassResolutionLimits
  -> Set Name
  -> (Name -> Maybe Int)
  -> Constraint (Type variable)
  -> Either (ClassResolutionConstraintError variable)
      (Constraint (Type variable))
prepareConstraintStructure limits aliases lookupArity source = do
  either (Left . InvalidClassResolutionConstraintClass) Right
    $ validateConstraintClassName name
  expected <- case lookupArity name of
    Nothing -> Left $ UnknownClassResolutionClass name
    Just arity -> Right arity
  let supplied = observedListLength expected rawArguments
  if supplied == expected
    then pure ()
    else Left $ ClassResolutionConstraintArityMismatch
      name expected supplied
  arguments <- mapM (uncurry prepareArgument) $ zip [0 ..] rawArguments
  pure $ Constraint name arguments
 where
  name = constraintClass source
  rawArguments = constraintArguments source

  prepareArgument ordinal rawType = do
    let maximumNodes = maximumClassResolutionTypeNodes limits
        maximumWidth = maximumClassResolutionCollectionWidth limits
    case observeTypeWithin sharedTypeStructure maximumNodes maximumWidth rawType of
      Left (TypeStructureNodeLimitExceeded observed) -> Left $
        ClassResolutionConstraintTypeNodeLimitExceeded
          ordinal maximumNodes observed
      Left (TypeStructureCollectionLimitExceeded observed) -> Left $
        ClassResolutionConstraintTypeCollectionLimitExceeded
          ordinal maximumWidth observed
      Right () -> pure ()
    normalized <- either
      (Left . InvalidClassResolutionConstraintType ordinal) Right
      $ normalizeType rawType
    case firstTypeSynonym aliases normalized of
      Just alias -> Left $
        ClassResolutionConstraintContainsTypeSynonym ordinal alias
      Nothing -> pure ()
    if containsForall normalized
      then Left $ ClassResolutionConstraintForallUnsupported ordinal
      else Right normalized

validateConstraintKinds
  :: Ord variable
  => KindAssumptions
  -> Constraint (Type variable)
  -> Either (ClassResolutionConstraintError variable) ()
validateConstraintKinds assumptions source =
  either (Left . IllKindedClassResolutionConstraint) Right
    $ () <$ checkClassApplicationKinds assumptions
        (constraintClass source) (constraintArguments source)

firstTypeSynonym :: Set Name -> Type variable -> Maybe Name
firstTypeSynonym aliases source = case source of
  TypeVariable{} -> Nothing
  TypeConstructor name
    | name `Set.member` aliases -> Just name
    | otherwise -> Nothing
  TypeApplication function argument ->
    firstTypeSynonym aliases function `orElse`
      firstTypeSynonym aliases argument
  FunctionType parameter result ->
    firstTypeSynonym aliases parameter `orElse`
      firstTypeSynonym aliases result
  TupleType _ elements -> firstPresentMap (firstTypeSynonym aliases) elements
  ForallType _ constraints body ->
    firstPresentMap
      (firstPresentMap (firstTypeSynonym aliases) . constraintArguments)
      constraints
      `orElse` firstTypeSynonym aliases body

orElse :: Maybe value -> Maybe value -> Maybe value
orElse present@Just{} _ = present
orElse Nothing later = later

firstPresentMap :: (value -> Maybe result) -> [value] -> Maybe result
firstPresentMap _ [] = Nothing
firstPresentMap inspect (value : remaining) =
  inspect value `orElse` firstPresentMap inspect remaining

resolutionClassName :: ResolutionClass variable -> Name
resolutionClassName (ResolutionClass name _ _) = name

resolutionClassParameters :: ResolutionClass variable -> [variable]
resolutionClassParameters (ResolutionClass _ parameters _) = parameters

resolutionClassSuperclasses
  :: ResolutionClass variable -> [Constraint (Type variable)]
resolutionClassSuperclasses (ResolutionClass _ _ superclasses) = superclasses

resolutionInstanceOrdinal :: ResolutionInstance variable -> Natural
resolutionInstanceOrdinal
    (ResolutionInstance ordinal _ _ _ _) = ordinal

resolutionInstanceBinders :: ResolutionInstance variable -> [variable]
resolutionInstanceBinders
    (ResolutionInstance _ binders _ _ _) = binders

resolutionInstanceBinderSet :: ResolutionInstance variable -> Set variable
resolutionInstanceBinderSet
    (ResolutionInstance _ _ binders _ _) = binders

resolutionInstancePrerequisites
  :: ResolutionInstance variable -> [Constraint (Type variable)]
resolutionInstancePrerequisites
    (ResolutionInstance _ _ _ prerequisites _) = prerequisites

resolutionInstanceHead
  :: ResolutionInstance variable -> Constraint (Type variable)
resolutionInstanceHead
    (ResolutionInstance _ _ _ _ headConstraint) = headConstraint

rejectDuplicateHeads
  :: Ord variable
  => [ResolutionInstance variable]
  -> Either (ClassResolutionEnvironmentError variable) ()
rejectDuplicateHeads instances = case
    Environment.repeatedInstanceHeadsInFirstRepetitionOrder
      [ (resolutionInstanceBinders source, resolutionInstanceHead source)
      | source <- instances
      ] of
  duplicate : _ -> Left $ DuplicateClassResolutionInstanceHead duplicate
  [] -> Right ()

rejectOverlappingHeads
  :: Ord variable
  => ClassResolutionLimits
  -> [ResolutionInstance variable]
  -> Either (ClassResolutionEnvironmentError variable) ()
rejectOverlappingHeads limits instances = compareHeads 0 buckets instances
 where
  maximumComparisons = maximumClassResolutionOverlapComparisons limits
  buckets = Map.map reverse $ foldl' insertBucket Map.empty instances
  insertBucket table source = Map.insertWith (++)
    (constraintClass $ resolutionInstanceHead source) [source] table

  compareHeads _ _ [] = Right ()
  compareHeads compared remainingByClass (left : remaining) = do
    let owner = constraintClass $ resolutionInstanceHead left
        sameClass = Map.findWithDefault [] owner remainingByClass
        laterSameClass = case sameClass of
          [] -> []
          _current : later -> later
    (overlap, afterComparisons) <- firstOverlap
      compared left laterSameClass
    case overlap of
      Just right -> Left $ OverlappingClassResolutionInstanceHeads
        (resolutionInstanceHead left) (resolutionInstanceHead right)
      Nothing -> compareHeads afterComparisons
        (Map.insert owner laterSameClass remainingByClass) remaining

  firstOverlap compared _ [] = Right (Nothing, compared)
  firstOverlap compared left (right : remaining)
    | compared >= maximumComparisons = Left $
        ClassResolutionOverlapComparisonLimitExceeded
          maximumComparisons $ observedBeyond maximumComparisons
    | resolutionHeadsOverlap left right =
        Right (Just right, compared + 1)
    | otherwise = firstOverlap (compared + 1) left remaining

  observedBeyond maximumAllowed
    | maximumAllowed == maxBound = maxBound
    | otherwise = maximumAllowed + 1

-- Both the overlap classifier and the runtime matcher use this applicative
-- arrow/tuple kernel.  Keeping the kernel local prevents a higher-kinded
-- instance variable from making two runtime rules overlap after a structural
-- classifier had declared them disjoint.
data ResolutionOverlapVariable variable
  = LeftResolutionBinder !variable
  | RightResolutionBinder !variable
  | RigidResolutionVariable !variable
  deriving (Eq, Ord)

type ResolutionOverlapSubstitutions variable =
  Map (ResolutionOverlapVariable variable)
    (Type (ResolutionOverlapVariable variable))

resolutionHeadsOverlap
  :: Ord variable
  => ResolutionInstance variable
  -> ResolutionInstance variable
  -> Bool
resolutionHeadsOverlap left right
  | constraintClass leftHead /= constraintClass rightHead = False
  | otherwise = case zipExactly
      (constraintArguments $ preparedHead LeftResolutionBinder left)
      (constraintArguments $ preparedHead RightResolutionBinder right) of
    Nothing -> False
    Just equations -> case solveResolutionOverlap equations Map.empty of
      Nothing -> False
      Just _ -> True
 where
  leftHead = resolutionInstanceHead left
  rightHead = resolutionInstanceHead right

  preparedHead side source = fmap
    (prepareType side $ resolutionInstanceBinderSet source)
    $ resolutionInstanceHead source

  prepareType side binders =
    fmap tag . constructorApplicationForm . canonicalizeType
   where
    tag variable
      | variable `Set.member` binders = side variable
      | otherwise = RigidResolutionVariable variable

  zipExactly [] [] = Just []
  zipExactly (leftType : leftRest) (rightType : rightRest) =
    ((leftType, rightType) :) <$> zipExactly leftRest rightRest
  zipExactly _ _ = Nothing

solveResolutionOverlap
  :: Ord variable
  => [( Type (ResolutionOverlapVariable variable)
      , Type (ResolutionOverlapVariable variable)
      )]
  -> ResolutionOverlapSubstitutions variable
  -> Maybe (ResolutionOverlapSubstitutions variable)
solveResolutionOverlap [] substitutions = Just substitutions
solveResolutionOverlap ((rawLeft, rawRight) : equations) substitutions
  | left == right = solveResolutionOverlap equations substitutions
  | TypeVariable variable <- left
  , resolutionOverlapBindable variable = bind variable right
  | TypeVariable variable <- right
  , resolutionOverlapBindable variable = bind variable left
  | TypeConstructor leftName <- left
  , TypeConstructor rightName <- right
  , leftName == rightName = solveResolutionOverlap equations substitutions
  | TypeApplication leftFunction leftArgument <- left
  , TypeApplication rightFunction rightArgument <- right =
      solveResolutionOverlap
        ((leftFunction, rightFunction) :
          (leftArgument, rightArgument) : equations)
        substitutions
  | FunctionType leftParameter leftResult <- left
  , FunctionType rightParameter rightResult <- right =
      solveResolutionOverlap
        ((leftParameter, rightParameter) :
          (leftResult, rightResult) : equations)
        substitutions
  | TupleType leftBoxity leftElements <- left
  , TupleType rightBoxity rightElements <- right
  , leftBoxity == rightBoxity
  , Just elementEquations <- zipExactly leftElements rightElements =
      solveResolutionOverlap (elementEquations ++ equations) substitutions
  | otherwise = Nothing
 where
  left = zonkResolutionOverlap substitutions rawLeft
  right = zonkResolutionOverlap substitutions rawRight

  bind variable replacement
    | resolutionOverlapOccurs variable replacement = Nothing
    | otherwise = solveResolutionOverlap
        [ ( substituteResolutionOverlap variable replacement equationLeft
          , substituteResolutionOverlap variable replacement equationRight
          )
        | (equationLeft, equationRight) <- equations
        ]
        $ Map.insert variable replacement
        $ Map.map (substituteResolutionOverlap variable replacement)
          substitutions

  zipExactly [] [] = Just []
  zipExactly (leftType : leftRest) (rightType : rightRest) =
    ((leftType, rightType) :) <$> zipExactly leftRest rightRest
  zipExactly _ _ = Nothing

resolutionOverlapBindable :: ResolutionOverlapVariable variable -> Bool
resolutionOverlapBindable variable = case variable of
  LeftResolutionBinder{} -> True
  RightResolutionBinder{} -> True
  RigidResolutionVariable{} -> False

resolutionOverlapOccurs
  :: Eq variable
  => ResolutionOverlapVariable variable
  -> Type (ResolutionOverlapVariable variable)
  -> Bool
resolutionOverlapOccurs variable source = case source of
  TypeVariable candidate -> variable == candidate
  TypeConstructor{} -> False
  TypeApplication function argument ->
    resolutionOverlapOccurs variable function ||
      resolutionOverlapOccurs variable argument
  FunctionType parameter result ->
    resolutionOverlapOccurs variable parameter ||
      resolutionOverlapOccurs variable result
  TupleType _ elements -> any (resolutionOverlapOccurs variable) elements
  ForallType{} -> False

substituteResolutionOverlap
  :: Eq variable
  => ResolutionOverlapVariable variable
  -> Type (ResolutionOverlapVariable variable)
  -> Type (ResolutionOverlapVariable variable)
  -> Type (ResolutionOverlapVariable variable)
substituteResolutionOverlap variable replacement source = case source of
  TypeVariable candidate
    | candidate == variable -> replacement
    | otherwise -> source
  TypeConstructor{} -> source
  TypeApplication function argument -> TypeApplication
    (substituteResolutionOverlap variable replacement function)
    (substituteResolutionOverlap variable replacement argument)
  FunctionType parameter result -> FunctionType
    (substituteResolutionOverlap variable replacement parameter)
    (substituteResolutionOverlap variable replacement result)
  TupleType boxity elements -> TupleType boxity
    $ map (substituteResolutionOverlap variable replacement) elements
  ForallType{} -> source

zonkResolutionOverlap
  :: Ord variable
  => ResolutionOverlapSubstitutions variable
  -> Type (ResolutionOverlapVariable variable)
  -> Type (ResolutionOverlapVariable variable)
zonkResolutionOverlap substitutions source = case source of
  TypeVariable variable -> case Map.lookup variable substitutions of
    Nothing -> source
    Just replacement -> zonkResolutionOverlap substitutions replacement
  TypeConstructor{} -> source
  TypeApplication function argument -> TypeApplication
    (zonkResolutionOverlap substitutions function)
    (zonkResolutionOverlap substitutions argument)
  FunctionType parameter result -> FunctionType
    (zonkResolutionOverlap substitutions parameter)
    (zonkResolutionOverlap substitutions result)
  TupleType boxity elements -> TupleType boxity
    $ map (zonkResolutionOverlap substitutions) elements
  ForallType{} -> source

rejectSuperclassCycles
  :: [ResolutionClass variable]
  -> Either (ClassResolutionEnvironmentError errorVariable) ()
rejectSuperclassCycles classes = case firstCycle of
  Nothing -> Right ()
  Just names -> Left $ ClassResolutionSuperclassCycle names
 where
  sourceNames = map resolutionClassName classes
  cyclicSets =
    [ Set.fromList $ map resolutionClassName members
    | CyclicSCC members <- stronglyConnComp
        [ (source, resolutionClassName source,
            map constraintClass $ resolutionClassSuperclasses source)
        | source <- classes
        ]
    ]
  firstCycle = firstPresentMap cycleAt sourceNames
  cycleAt name = case filter (Set.member name) cyclicSets of
    [] -> Nothing
    cycleSet : _ -> Just $ filter (`Set.member` cycleSet) sourceNames

completeInstance
  :: Ord variable
  => ClassResolutionLimits
  -> KindAssumptions
  -> Set Name
  -> Map Name Int
  -> Map Name (ResolutionClass variable)
  -> ResolutionInstance variable
  -> Either (ClassResolutionEnvironmentError variable)
      (ResolutionInstance variable)
completeInstance limits assumptions aliases classArities classes source =
  case Map.lookup
    (constraintClass headConstraint) classes of
  Nothing -> Left $ InvalidClassResolutionEnvironmentConstraint
    (ClassResolutionInstanceHead ordinal)
    $ UnknownClassResolutionClass $ constraintClass headConstraint
  Just owner -> do
    substitutions <- zipExactly (resolutionClassParameters owner)
      $ constraintArguments headConstraint
    let rawDirectSuperclasses = map
          (fmap $ canonicalizeType .
            substituteFirstOrder (Map.fromList substitutions))
          $ resolutionClassSuperclasses owner
    directSuperclasses <- mapM
      (uncurry $ prepareEnvironmentConstraint
        limits assumptions aliases (`Map.lookup` classArities))
      [ (ClassResolutionDerivedInstanceSuperclass ordinal
          superclassOrdinal, superclass)
      | (superclassOrdinal, superclass) <- zip [0 ..] rawDirectSuperclasses
      ]
    completed <- distinctConstraintsWithin
      (ClassResolutionCompletedInstancePrerequisites ordinal)
      (maximumClassResolutionCollectionWidth limits)
      $ resolutionInstancePrerequisites source ++ directSuperclasses
    pure $ ResolutionInstance ordinal
      (resolutionInstanceBinders source)
      (resolutionInstanceBinderSet source)
      completed headConstraint
 where
  ordinal = resolutionInstanceOrdinal source
  headConstraint = resolutionInstanceHead source

  zipExactly [] [] = Right []
  zipExactly (left : leftRest) (right : rightRest) =
    ((left, right) :) <$> zipExactly leftRest rightRest
  zipExactly _ _ = Left $ InvalidClassResolutionEnvironmentConstraint
    (ClassResolutionInstanceHead ordinal)
    $ ClassResolutionConstraintArityMismatch
        (constraintClass headConstraint)
        (length $ resolutionClassParameters $ classes Map.!
          constraintClass headConstraint)
        (observedListLength
          (length $ resolutionClassParameters $ classes Map.!
            constraintClass headConstraint)
          $ constraintArguments headConstraint)

distinctConstraintsWithin
  :: Ord variable
  => ClassResolutionCollectionSite
  -> Int
  -> [Constraint (Type variable)]
  -> Either (ClassResolutionEnvironmentError variable)
      [Constraint (Type variable)]
distinctConstraintsWithin site maximumWidth = go Set.empty 0 []
 where
  go _ _ reversed [] = Right $ reverse reversed
  go seen retained reversed (source : remaining)
    | source `Set.member` seen = go seen retained reversed remaining
    | retained >= maximumWidth = Left $ ClassResolutionCollectionLimitExceeded
        site maximumWidth $ observedBeyond maximumWidth
    | otherwise = go (Set.insert source seen) (retained + 1)
        (source : reversed) remaining

  observedBeyond maximumAllowed
    | maximumAllowed == maxBound = maxBound
    | otherwise = maximumAllowed + 1

substituteFirstOrder
  :: Ord variable
  => Map variable (Type variable)
  -> Type variable
  -> Type variable
substituteFirstOrder substitutions source = case source of
  TypeVariable variable -> Map.findWithDefault source variable substitutions
  TypeConstructor{} -> source
  TypeApplication function argument -> TypeApplication
    (substituteFirstOrder substitutions function)
    (substituteFirstOrder substitutions argument)
  FunctionType parameter result -> FunctionType
    (substituteFirstOrder substitutions parameter)
    (substituteFirstOrder substitutions result)
  TupleType boxity elements -> TupleType boxity
    $ map (substituteFirstOrder substitutions) elements
  ForallType{} -> source

rejectExpandingPrerequisites
  :: Ord variable
  => ResolutionInstance variable
  -> Either (ClassResolutionEnvironmentError variable) ()
rejectExpandingPrerequisites source = mapM_ inspect
  $ resolutionInstancePrerequisites source
 where
  headConstraint = resolutionInstanceHead source
  binders = resolutionInstanceBinderSet source
  (headSize, headOccurrences) = terminationMeasure binders headConstraint

  inspect prerequisite
    | not $ Map.keysSet prerequisiteOccurrences
        `Set.isSubsetOf` Map.keysSet headOccurrences = Right ()
    | prerequisiteSize <= headSize
    , all noMoreFrequent $ Map.toList prerequisiteOccurrences = Right ()
    | otherwise = Left $ ExpandingClassResolutionInstancePrerequisite
        (resolutionInstanceOrdinal source) headConstraint prerequisite
   where
    (prerequisiteSize, prerequisiteOccurrences) =
      terminationMeasure binders prerequisite
    noMoreFrequent (variable, count) = count <=
      Map.findWithDefault 0 variable headOccurrences

terminationMeasure
  :: Ord variable
  => Set variable
  -> Constraint (Type variable)
  -> (Integer, Map variable Integer)
terminationMeasure binders (Constraint _ arguments) = foldl'
  (\accumulated argument -> addMeasure accumulated
    $ measureType $ constructorApplicationForm $ canonicalizeType argument)
  (1, Map.empty) arguments
 where
  measureType source = case source of
    TypeVariable variable ->
      (1, if variable `Set.member` binders
        then Map.singleton variable 1 else Map.empty)
    TypeConstructor{} -> (1, Map.empty)
    TypeApplication function argument -> addMeasure (1, Map.empty)
      $ addMeasure (measureType function) (measureType argument)
    FunctionType parameter result -> addMeasure (1, Map.empty)
      $ addMeasure (measureType parameter) (measureType result)
    TupleType _ elements -> foldl'
      (\accumulated element -> addMeasure accumulated $ measureType element)
      (1, Map.empty) elements
    ForallType{} -> (1, Map.empty)

  addMeasure (leftSize, leftOccurrences) (rightSize, rightOccurrences) =
    (leftSize + rightSize, Map.unionWith (+) leftOccurrences rightOccurrences)

resolveConstraint
  :: Ord variable
  => CheckedClassResolutionEnvironment variable
  -> Set (Constraint (Type variable))
  -> Int
  -> Int
  -> Constraint (Type variable)
  -> Either (ClassResolutionQueryError variable)
      (Maybe (ClassResolutionProof variable), Int)
resolveConstraint environment@(CheckedClassResolutionEnvironment limits _ _ _
    _ _ instancesByClass) visiting depth proofCount goal
  | goal `Set.member` visiting = Right (Nothing, proofCount)
  | otherwise = case firstMatchingInstance candidates goal of
      Nothing -> Right (Nothing, proofCount)
      Just (source, substitutions) -> do
        let maximumDepth = maximumClassResolutionProofDepth limits
            maximumProofs = maximumClassResolutionProofNodes limits
            exceeded value maximumAllowed = value >= maximumAllowed
            observedBeyond maximumAllowed
              | maximumAllowed == maxBound = maxBound
              | otherwise = maximumAllowed + 1
        if exceeded depth maximumDepth
          then Left $ ClassResolutionProofDepthLimitExceeded
            maximumDepth $ observedBeyond maximumDepth
          else pure ()
        if exceeded proofCount maximumProofs
          then Left $ ClassResolutionProofNodeLimitExceeded
            maximumProofs $ observedBeyond maximumProofs
          else pure ()
        let observedDepth = depth + 1
            observedProofs = proofCount + 1
        (children, finalCount) <- resolvePrerequisiteSources
          (Set.insert goal visiting) observedDepth observedProofs
          (resolutionInstanceOrdinal source) substitutions 0
          $ resolutionInstancePrerequisites source
        pure ((\proofs -> ClassResolutionProof goal
          (resolutionInstanceHead source) proofs) <$> children, finalCount)
 where
  candidates = Map.findWithDefault [] (constraintClass goal) instancesByClass

  resolvePrerequisiteSources _ _ count _ _ _ [] = Right (Just [], count)
  resolvePrerequisiteSources nextVisiting nextDepth count instanceOrdinal
      substitutions ordinal (source : remaining) = do
    current <- instantiateConstraint environment instanceOrdinal
      substitutions ordinal source
    case current of
      Nothing -> Right (Nothing, count)
      Just prerequisite -> do
        (child, afterChild) <- resolveConstraint environment nextVisiting
          nextDepth count prerequisite
        case child of
          Nothing -> Right (Nothing, afterChild)
          Just proof -> do
            (later, finalCount) <- resolvePrerequisiteSources
              nextVisiting nextDepth afterChild instanceOrdinal
              substitutions (ordinal + 1) remaining
            pure ((proof :) <$> later, finalCount)

firstMatchingInstance
  :: Ord variable
  => [ResolutionInstance variable]
  -> Constraint (Type variable)
  -> Maybe (ResolutionInstance variable, Map variable (Type variable))
firstMatchingInstance [] _ = Nothing
firstMatchingInstance (source : remaining) goal = case matchInstance source goal of
  Just substitutions -> Just (source, substitutions)
  Nothing -> firstMatchingInstance remaining goal

matchInstance
  :: Ord variable
  => ResolutionInstance variable
  -> Constraint (Type variable)
  -> Maybe (Map variable (Type variable))
matchInstance source goal = do
  guard $ constraintClass headConstraint == constraintClass goal
  equations <- zipExactly
    (constraintArguments headConstraint) (constraintArguments goal)
  foldM matchEquation Map.empty equations
 where
  headConstraint = resolutionInstanceHead source
  binders = resolutionInstanceBinderSet source

  zipExactly [] [] = Just []
  zipExactly (left : leftRest) (right : rightRest) =
    ((left, right) :) <$> zipExactly leftRest rightRest
  zipExactly _ _ = Nothing

  matchEquation substitutions (rawPattern, rawTarget) =
    matchType substitutions
      (constructorApplicationForm $ canonicalizeType rawPattern)
      (constructorApplicationForm $ canonicalizeType rawTarget)

  matchType substitutions patternType targetType = case patternType of
    TypeVariable variable
      | variable `Set.member` binders -> case Map.lookup variable substitutions of
          Nothing -> Just $ Map.insert variable targetType substitutions
          Just previous
            | previous == targetType -> Just substitutions
            | otherwise -> Nothing
      | patternType == targetType -> Just substitutions
      | otherwise -> Nothing
    TypeConstructor patternName -> case targetType of
      TypeConstructor targetName
        | patternName == targetName -> Just substitutions
      _ -> Nothing
    TypeApplication patternFunction patternArgument -> case targetType of
      TypeApplication targetFunction targetArgument -> do
        afterFunction <- matchType substitutions patternFunction targetFunction
        matchType afterFunction patternArgument targetArgument
      _ -> Nothing
    FunctionType patternParameter patternResult -> case targetType of
      FunctionType targetParameter targetResult -> do
        afterParameter <- matchType substitutions
          patternParameter targetParameter
        matchType afterParameter patternResult targetResult
      _ -> Nothing
    TupleType patternBoxity patternElements -> case targetType of
      TupleType targetBoxity targetElements
        | patternBoxity == targetBoxity ->
            matchTypes substitutions patternElements targetElements
      _ -> Nothing
    ForallType{} -> Nothing

  matchTypes substitutions [] [] = Just substitutions
  matchTypes substitutions (patternType : patternRest)
      (targetType : targetRest) = do
    afterCurrent <- matchType substitutions patternType targetType
    matchTypes afterCurrent patternRest targetRest
  matchTypes _ _ _ = Nothing

instantiateConstraint
  :: Ord variable
  => CheckedClassResolutionEnvironment variable
  -> Natural
  -> Map variable (Type variable)
  -> Natural
  -> Constraint (Type variable)
  -> Either (ClassResolutionQueryError variable)
      (Maybe (Constraint (Type variable)))
instantiateConstraint
    (CheckedClassResolutionEnvironment limits assumptions aliases _ classes
      _ _)
    instanceOrdinal substitutions prerequisiteOrdinal source =
  case mapM instantiateType $ constraintArguments source of
    Nothing -> Right Nothing
    Just arguments -> do
      canonical <- either (Left . derivedFailure) Right
        $ prepareConstraintStructure limits aliases lookupArity
        $ Constraint (constraintClass source)
        $ map canonicalizeType arguments
      if Set.null $ constraintFreeVariables canonical
        then do
          either (Left . derivedFailure) Right
            $ validateConstraintKinds assumptions canonical
          Right $ Just canonical
        else Right Nothing
 where
  lookupArity name = length . resolutionClassParameters
    <$> Map.lookup name classes
  derivedFailure = InvalidClassResolutionDerivedConstraint
    instanceOrdinal prerequisiteOrdinal

  instantiateType rawType = case rawType of
    TypeVariable variable -> Map.lookup variable substitutions
    TypeConstructor{} -> Just rawType
    TypeApplication function argument -> TypeApplication
      <$> instantiateType function <*> instantiateType argument
    FunctionType parameter result -> FunctionType
      <$> instantiateType parameter <*> instantiateType result
    TupleType boxity elements -> TupleType boxity <$> mapM instantiateType elements
    ForallType{} -> Nothing

sameClassResolutionEnvironment
  :: Eq variable
  => CheckedClassResolutionEnvironment variable
  -> CheckedClassResolutionEnvironment variable
  -> Bool
sameClassResolutionEnvironment
    (CheckedClassResolutionEnvironment leftLimits leftAssumptions leftAliases
      leftClasses _ leftInstances _)
    (CheckedClassResolutionEnvironment rightLimits rightAssumptions rightAliases
      rightClasses _ rightInstances _) =
  leftLimits == rightLimits &&
  leftAssumptions == rightAssumptions &&
  leftAliases == rightAliases &&
  leftClasses == rightClasses &&
  leftInstances == rightInstances
