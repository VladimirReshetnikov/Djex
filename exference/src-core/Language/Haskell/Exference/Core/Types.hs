{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | The core type vocabulary of the Exference engine.  'HsType' is the shared
-- Djex type tree over 'SynthesisVariable's identified by 'TVarId'; the
-- pattern synonyms retain the historical constructor names without a second
-- representation.  The module also owns substitutions, 'HsConstraint's, the
-- validated 'StaticClassEnv' with its instance completion, the per-query
-- 'QueryClassEnv', and the compatibility renderers for types and constraints.
module Language.Haskell.Exference.Core.Types
  ( TVarId
  , module Language.Haskell.Exference.Core.Name
  , Boxity (..)
  , HsType
  , pattern TypeVar
  , pattern TypeConstant
  , pattern TypeCons
  , pattern TypeArrow
  , pattern TypeApp
  , pattern TypeTuple
  , pattern TypeForall
  , pattern TypeForallNative
  , HsTypeOffset (..)
  , SynthesisVariable
  , SynthesisTypeError (..)
  , toSynthesisType
  , fromSynthesisType
  , Subst (..)
  , Substs
  , HsSubstitutionError (..)
  , HsTypeClass (..)
  , HsInstance (..)
  , instanceConstraintVariables
  , HsConstraint
  , pattern HsConstraint
  , constraint_tclass
  , constraint_params
  , toSynthesisConstraint
  , fromSynthesisConstraint
  , ConstraintSite (..)
  , ClassEnvError (..)
  , StaticClassEnv
  , sClassEnv_tclasses
  , sClassEnv_instances
  , sClassEnv_explicitInstances
  , emptyStaticClassEnv
  , mkStaticClassEnv
  , validateConstraintInEnv
  , validateKnownConstraintArityInEnv
  , validateKnownConstraintInEnv
  , inflateInstances
  , QueryClassEnv
  , qClassEnv_env
  , qClassEnv_constraints
  , qClassEnv_inflatedConstraints
  , constraintApplySubsts
  , constraintApplySubstsChecked
  , inflateHsConstraints
  , applySubst
  , applySubstChecked
  , applySubsts
  , applySubstsChecked
  -- , typeParser
  , containsVar
  , showVar
  , defaultVariableName
  , preferredVarName
  , mkQueryClassEnv
  , addQueryClassEnv
  , freeVars
  , typeConstraints
  , showHsConstraint
  , TypeVarIndex
  , showHsType
  , showHsTypeWithQualification
  )
where



import Data.Bifunctor (first)
import Data.Char ( ord, chr, toLower )
import Data.Foldable (toList, traverse_)
import Data.Functor.Identity (Identity (..))
import Data.Graph (SCC (..), stronglyConnComp)
import Data.Monoid ( Any(..) )
import Numeric.Natural (Natural)

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as L

import Language.Haskell.Exference.Core.Internal.SourceNames
  ( preferredSourceTypeVariableNames )
import Language.Haskell.Exference.Core.Internal.VariableSupply
  ( freshSynthesisVariable )
import Language.Haskell.Exference.Core.Name
import qualified Language.Haskell.Synthesis.Collection as SharedCollection
import qualified Language.Haskell.Synthesis.Constraint as SharedConstraint
import qualified Language.Haskell.Synthesis.Environment as SharedEnvironment
import qualified Language.Haskell.Synthesis.Fresh as SharedFresh
import qualified Language.Haskell.Synthesis.Name as SharedName
import Language.Haskell.Synthesis.Name (Boxity (..))
import Language.Haskell.Synthesis.Qualification (Qualification (FullyQualified))
import qualified Language.Haskell.Synthesis.Type as SharedType
import qualified Language.Haskell.Synthesis.TypeRender as SharedRender

import Control.DeepSeq
import Control.Monad.Trans.State.Strict (State, get, put, runState)
import GHC.Generics




-- | Numeric identity of a type variable in Exference's search
-- representation.  The same identity space is used for flexible search
-- variables ('TypeVar') and rigid constants ('TypeConstant'); which kind a
-- given identity denotes is carried by the enclosing 'SynthesisVariable'.
type TVarId = Int
-- | A shared type variable tagged as flexible or rigid, identified by a
-- 'TVarId'.  This is the variable type of 'HsType'.
type SynthesisVariable = SharedType.Variable TVarId

-- | Exference now searches the same type tree used at the neutral Djex
-- boundary.  The patterns below retain the historical constructor vocabulary
-- without storing a second, recursively isomorphic representation.
type HsType = SharedType.Type SynthesisVariable

pattern TypeVar :: TVarId -> HsType
pattern TypeVar variable =
  SharedType.TypeVariable (SharedType.FlexibleVariable variable)

pattern TypeConstant :: TVarId -> HsType
pattern TypeConstant variable =
  SharedType.TypeVariable (SharedType.RigidVariable variable)

pattern TypeCons :: QualifiedName -> HsType
pattern TypeCons name = SharedType.TypeConstructor name

pattern TypeArrow :: HsType -> HsType -> HsType
pattern TypeArrow parameter result =
  SharedType.FunctionType parameter result

pattern TypeApp :: HsType -> HsType -> HsType
pattern TypeApp function argument =
  SharedType.TypeApplication function argument

pattern TypeTuple :: Boxity -> [HsType] -> HsType
pattern TypeTuple boxity elements = SharedType.TupleType boxity elements

-- | Historical forall view.  It intentionally matches only flexible
-- binders, because the old Exference representation could not express rigid
-- binders in that position.  Use 'TypeForallNative' for a total shared view.
pattern TypeForall :: [TVarId] -> [HsConstraint] -> HsType -> HsType
pattern TypeForall variables constraints body <-
  SharedType.ForallType
    (flexibleBinderList -> Just variables)
    constraints
    body
  where
    TypeForall variables constraints body = SharedType.ForallType
      (map SharedType.FlexibleVariable variables)
      constraints
      body

-- | Total forall view over the native shared representation.
pattern TypeForallNative
  :: [SynthesisVariable]
  -> [HsConstraint]
  -> HsType
  -> HsType
pattern TypeForallNative variables constraints body =
  SharedType.ForallType variables constraints body

{-# COMPLETE
  TypeVar,
  TypeConstant,
  TypeCons,
  TypeArrow,
  TypeApp,
  TypeTuple,
  TypeForallNative
  #-}

flexibleBinderList :: [SynthesisVariable] -> Maybe [TVarId]
flexibleBinderList = traverse SharedType.flexibleVariableIdentity

-- | Why a type or constraint was rejected at Exference's checked type
-- boundary ('toSynthesisType', 'fromSynthesisType', 'toSynthesisConstraint',
-- 'fromSynthesisConstraint'): a shared structural type error, a shared
-- constraint error, or a forall binding a rigid variable, which Exference
-- does not accept as a lexical binder.
data SynthesisTypeError
  = InvalidSynthesisType (SharedType.TypeError SynthesisVariable)
  | InvalidSynthesisConstraint SharedConstraint.ConstraintError
  | RigidForallBinder TVarId
  deriving (Eq, Ord, Show, Generic)

instance NFData SynthesisTypeError

-- | A single-variable substitution: replace the free flexible variable with
-- the given identity by the given type.  Applied by 'applySubst' and
-- 'applySubstChecked'.
data Subst  = Subst {-# UNPACK #-} !TVarId !HsType
-- | A simultaneous substitution of free flexible variables, keyed by
-- 'TVarId'.  Substitutions and unifiers ('applySubsts',
-- "Language.Haskell.Exference.Core.Unify") exchange this form.
type Substs = IntMap.IntMap HsType

-- | A checked substitution can fail when alpha-renaming exhausts Exference's
-- finite 'Int' identity space, or if the temporary tuple used to coordinate
-- substitution across constraint arguments loses its expected shape.
data HsSubstitutionError
  = SharedSubstitutionFailure
      (SharedType.SubstitutionError SynthesisVariable)
  -- | Retained for source compatibility. Constraint substitution now uses the
  -- shared batch operation and cannot produce a synthetic projection failure.
  | UnexpectedConstraintSubstitutionResult HsType
  deriving (Eq, Show, Generic)

instance NFData HsSubstitutionError

-- | Validate and canonicalize a native Exference/shared type at the public
-- boundary. Flexible and rigid IDs remain distinct, and saturated tuple
-- constructors are stored structurally.
toSynthesisType :: HsType -> Either SynthesisTypeError HsType
toSynthesisType = normalizeExferenceType

-- | Validate and canonicalize a shared type for Exference search.  This is no
-- longer a representational conversion: 'HsType' is the shared type.
fromSynthesisType
  :: HsType
  -> Either SynthesisTypeError HsType
fromSynthesisType = normalizeExferenceType

normalizeExferenceType
  :: HsType
  -> Either SynthesisTypeError HsType
normalizeExferenceType source = do
  canonical <- either (Left . InvalidSynthesisType) Right
    $ SharedType.normalizeType source
  ensureFlexibleForallBinders canonical
  return canonical

-- Deliberately skip validation here. Substitution is a representation-level
-- operation and can act on temporary search forms (for example a duplicate
-- binder list) as well as the native rigid-binder forms now expressible by
-- 'HsType'. Checked search boundaries reject unsupported rigid binders before
-- execution; this lower-level operation must nevertheless remain total over
-- values accepted by its native representation. Its result still enters
-- canonical storage so saturated functions and tuples have one spelling.
canonicalizeSubstitutionResult
  :: HsType
  -> HsType
canonicalizeSubstitutionResult = SharedType.canonicalizeType

-- Exference treats rigid variables as search constants, never as lexical
-- forall binders.  Check every nested context and structural tuple explicitly
-- now that callers can construct the shared representation directly.
ensureFlexibleForallBinders
  :: HsType
  -> Either SynthesisTypeError ()
ensureFlexibleForallBinders = traverse_ checkBinder
  . SharedType.typeBinderVariables
 where
  checkBinder variable = case SharedType.rigidVariableIdentity variable of
    Nothing -> Right ()
    Just identifier -> Left $ RigidForallBinder identifier

-- | A type paired with an offset to add to each of its free flexible variable
-- identities before use.  'unifyOffset' and 'unifyRightOffset' consume it to
-- move the type into a namespace disjoint from the other unification side.
data HsTypeOffset = HsTypeOffset !HsType {-# UNPACK #-} !Int

-- | Compatibility map from a source type-variable spelling to the 'TVarId' it
-- was assigned, used as rendering hints by 'showHsType' and
-- 'showHsConstraint'.
--
-- Source locations do not belong in variable identity. Keeping only the
-- spelling also decouples conversion and historical rendering from a
-- particular parser AST. This raw spelling-oriented compatibility map is not
-- a trusted canonical-search input; cross it through
-- @mkExferenceSourceTypeVariableHints@ or the checked request boundary first,
-- which also binds it to the exact canonical goal whose IDs it describes.
type TypeVarIndex = M.Map String Int

-- | A type class declaration as stored in a 'StaticClassEnv': its name, its
-- parameter variables, and the superclass constraints over those parameters.
-- 'mkStaticClassEnv' checks that the parameters are non-negative and
-- distinct and that superclass constraints mention only declared parameters.
data HsTypeClass = HsTypeClass
  { tclass_name :: QualifiedName
  , tclass_params :: [TVarId]
  , tclass_constraints :: [HsConstraint]
  }
  deriving (Eq, Ord, Show, Generic)

-- | An instance rule: a head constraint together with the prerequisite
-- constraints it requires.  Every free variable of the head and
-- prerequisites is implicitly quantified (see 'instanceConstraintVariables').
data HsInstance = HsInstance
  { instance_constraints :: [HsConstraint]
  , instance_head :: HsConstraint
  }
  deriving (Eq, Show, Ord, Generic)

-- | Every free variable of an instance's head and prerequisite constraints:
-- the binder set t'HsInstance' quantifies implicitly. The compatibility class
-- validator and the shared-declaration adapter both derive their alpha
-- identities from this single operation so the two cannot drift.
instanceConstraintVariables
  :: [HsConstraint]
  -> S.Set SynthesisVariable
instanceConstraintVariables = foldMap SharedType.constraintFreeVariables

-- | Exference constraints now use the shared nominal/traversable node
-- directly. The historical constructor name remains as a bidirectional
-- pattern, so existing engine code does not conceal another representation.
type HsConstraint = SharedConstraint.Constraint HsType

pattern HsConstraint :: QualifiedName -> [HsType] -> HsConstraint
pattern HsConstraint className arguments =
  SharedConstraint.Constraint className arguments

{-# COMPLETE HsConstraint #-}

-- | The class name of a constraint.
-- 'QualifiedName' guarantees general lexical validity; the
-- checked type and environment boundaries additionally enforce that the name
-- occupies the class namespace.
constraint_tclass :: HsConstraint -> QualifiedName
constraint_tclass = SharedConstraint.constraintClass

-- | The type arguments of a constraint, in source order.
constraint_params :: HsConstraint -> [HsType]
constraint_params = SharedConstraint.constraintArguments

-- | Validate and canonicalize both the nominal class identity and every type
-- argument of a native shared constraint.
toSynthesisConstraint
  :: HsConstraint
  -> Either SynthesisTypeError
       HsConstraint
toSynthesisConstraint = normalizeSynthesisConstraint

-- | Validate and canonicalize a shared constraint for Exference. This is the
-- same native boundary as 'toSynthesisConstraint'; the two compatibility
-- directions retain one nominal-before-argument failure order now that no
-- representation conversion separates them. Structural names such as tuple
-- constructors are rejected in class position, while unboxed tuple types
-- remain valid arguments.
fromSynthesisConstraint
  :: HsConstraint
  -> Either SynthesisTypeError HsConstraint
fromSynthesisConstraint = normalizeSynthesisConstraint

normalizeSynthesisConstraint
  :: HsConstraint
  -> Either SynthesisTypeError HsConstraint
normalizeSynthesisConstraint constraint = do
  either (Left . InvalidSynthesisConstraint) Right
    $ SharedConstraint.validateConstraint constraint
  traverse normalizeExferenceType constraint

-- | Location of a constraint while validating a class environment or public
-- search input.
data ConstraintSite
  = ClassSuperclass QualifiedName
  | InstanceHead
  | InstancePrerequisite QualifiedName
  | QueryConstraint
  | BindingConstraint QualifiedName
  deriving (Eq, Ord, Show)

-- | Structural failures that would otherwise make class lookup or
-- superclass substitution ambiguous or partial.
data ClassEnvError
  = InvalidClassName QualifiedName
  | DuplicateClassDeclaration QualifiedName
  | DuplicateClassParameter QualifiedName TVarId
  | NegativeClassParameter QualifiedName TVarId
  | UndeclaredSuperclassVariables QualifiedName [TVarId]
  | UnknownConstraintClass ConstraintSite QualifiedName
  | ConstraintArityMismatch ConstraintSite QualifiedName Int Int
    -- ^ Site, class name, declared arity, supplied arity.
  | InvalidConstraintArgument
      ConstraintSite
      QualifiedName
      Int -- ^ Zero-based argument index.
      SynthesisTypeError
  | DuplicateInstanceHeads [HsConstraint]
  | OverlappingInstanceHeads [(HsConstraint, HsConstraint)]
    -- ^ Pairwise-overlapping explicit heads, retained in source order.
  | ExpandingInstancePrerequisite HsConstraint HsConstraint
    -- ^ Instance head and a groundable prerequisite that can increase a goal.
  | SuperclassCycle [QualifiedName]
  deriving (Eq, Ord)

instance Show ClassEnvError where
  showsPrec precedence failure = showParen (precedence > 10)
    $ showString $ renderClassEnvError failure

renderClassEnvError :: ClassEnvError -> String
renderClassEnvError failure = case failure of
  InvalidClassName name -> constructor "InvalidClassName" [show name]
  DuplicateClassDeclaration name ->
    constructor "DuplicateClassDeclaration" [show name]
  DuplicateClassParameter name variable ->
    constructor "DuplicateClassParameter" [show name, show variable]
  NegativeClassParameter name variable ->
    constructor "NegativeClassParameter" [show name, show variable]
  UndeclaredSuperclassVariables name variables ->
    constructor "UndeclaredSuperclassVariables" [show name, show variables]
  UnknownConstraintClass site name ->
    constructor "UnknownConstraintClass" [show site, show name]
  ConstraintArityMismatch site name expected actual -> constructor
    "ConstraintArityMismatch" [show site, show name, show expected, show actual]
  InvalidConstraintArgument site name index typeFailure -> constructor
    "InvalidConstraintArgument"
    [show site, show name, show index, show typeFailure]
  DuplicateInstanceHeads constraints -> constructor
    "DuplicateInstanceHeads" [sourceConstraintList constraints]
  OverlappingInstanceHeads pairs -> constructor
    "OverlappingInstanceHeads" [sourceConstraintPairs pairs]
  ExpandingInstancePrerequisite headConstraint prerequisite -> constructor
    "ExpandingInstancePrerequisite"
    [sourceConstraint headConstraint, sourceConstraint prerequisite]
  SuperclassCycle names -> constructor "SuperclassCycle" [show names]
 where
  constructor name fields = unwords $ name : fields
  sourceConstraint = showHsConstraint M.empty
  sourceConstraintList constraints = "[" ++ L.intercalate ","
    (map sourceConstraint constraints) ++ "]"
  sourceConstraintPairs pairs = "[" ++ L.intercalate ","
    [ "(" ++ sourceConstraint left ++ "," ++ sourceConstraint right ++ ")"
    | (left, right) <- pairs
    ] ++ "]"

-- | A validated, sealed class environment: class declarations by name, the
-- explicit source instances, and a per-class index of instances whose
-- prerequisites have been completed with the superclass dictionaries.
-- Construct it with 'mkStaticClassEnv' or 'emptyStaticClassEnv'.
--
-- Positional fields are deliberate.  Exported record labels would let a
-- downstream caller update the declarations or either derived index and
-- bypass 'mkStaticClassEnv'. Source instances remain separate because the
-- per-class map stores their superclass-completed resolution rules.
data StaticClassEnv = StaticClassEnv
  !(M.Map QualifiedName HsTypeClass)
  ![HsInstance]
  !(M.Map QualifiedName [HsInstance])
  deriving (Eq, Show)

-- | The class declarations of the environment, keyed by class name.
sClassEnv_tclasses :: StaticClassEnv -> M.Map QualifiedName HsTypeClass
sClassEnv_tclasses (StaticClassEnv classes _ _) = classes

-- | The instances exactly as supplied to 'mkStaticClassEnv', in source order
-- and without superclass completion.
sClassEnv_explicitInstances :: StaticClassEnv -> [HsInstance]
sClassEnv_explicitInstances (StaticClassEnv _ instances _) = instances

-- | The resolution rules used by the constraint solver: every instance,
-- completed by 'inflateInstances' with its direct superclass prerequisites,
-- indexed by the class of its head.
sClassEnv_instances :: StaticClassEnv -> M.Map QualifiedName [HsInstance]
sClassEnv_instances (StaticClassEnv _ _ instances) = instances

-- | The canonical validated empty environment, also used for parser recovery.
-- Every non-empty environment is built with 'mkStaticClassEnv'.
emptyStaticClassEnv :: StaticClassEnv
emptyStaticClassEnv = StaticClassEnv M.empty [] M.empty

-- | Validate and index a finite class environment. Class declaration failures
-- follow input order even though superclass lookup supports forward
-- references. Instance completion runs only after declarations, heads,
-- prerequisites, arities, and the superclass graph have been checked.
mkStaticClassEnv
  :: [HsTypeClass]
  -> [HsInstance]
  -> Either ClassEnvError StaticClassEnv
mkStaticClassEnv sourceClasses sourceInstances = do
  -- The lookup table must exist before validating any superclass so forward
  -- references remain legal.  Building it is deliberately non-diagnostic:
  -- declarations are then checked exactly once in caller order, preventing a
  -- later duplicate or malformed superclass from hiding an earlier local
  -- declaration error.
  let classTable = buildFirstOccurrenceClassTable classes
  validateClassesInSourceOrder classTable classes
  traverse_ (preflightInstance classTable) instances
  case SharedEnvironment.repeatedInstanceHeadsInFirstRepetitionOrder
      [ (implicitInstanceVariables declaration, instance_head declaration)
      | declaration <- instances
      ] of
    [] -> Right ()
    duplicates -> Left $ DuplicateInstanceHeads duplicates
  traverse_ (validateInstance classTable) instances
  case SharedEnvironment.overlappingInstanceHeadPairsInSourceOrder
      [ (implicitInstanceVariables declaration, instance_head declaration)
      | declaration <- instances
      ] of
    [] -> Right ()
    overlaps -> Left $ OverlappingInstanceHeads overlaps
  validateSuperclassGraph classTable
  let declarations = StaticClassEnv classTable [] M.empty
      resolutionInstances = inflateInstances declarations instances
  traverse_ validateInstanceTermination resolutionInstances
  return $ StaticClassEnv classTable instances
    $ indexInstances resolutionInstances
 where
  -- Canonicalization is total, so normalizing before validation does not
  -- disturb error precedence.  It does ensure that duplicate-head checks,
  -- superclass completion, and every value sealed in the environment observe
  -- the same structural function/tuple representation.
  classes = map canonicalizeClass sourceClasses
  instances = map canonicalizeInstance sourceInstances

  -- Inspect every bounded-width component before analyses that compute free
  -- variables and therefore assume a finite type tree. The callback also
  -- rejects unknown nested forall constraints without forcing their raw
  -- argument spines.
  preflightClass table declaration = traverse_
    (preflightConstraint table $ ClassSuperclass $ tclass_name declaration)
    $ tclass_constraints declaration

  preflightInstance table declaration = do
    preflightConstraint table InstanceHead $ instance_head declaration
    traverse_
      (preflightConstraint table $ InstancePrerequisite
        $ constraint_tclass
        $ instance_head declaration)
      $ instance_constraints declaration

  preflightConstraint table site constraint = do
    validateConstraintHeader table site constraint
    traverse_ preflightArgument
      $ zip [0 ..] $ constraint_params constraint
   where
    className = constraint_tclass constraint
    preflightArgument (index, argument) = SharedType.validateTypeWidthsWith
      (InvalidConstraintArgument site className index
        . InvalidSynthesisType)
      (validateConstraintHeader table site)
      argument

  canonicalizeClass declaration = declaration
    { tclass_constraints = map canonicalizeConstraint
        $ tclass_constraints declaration
    }

  canonicalizeInstance declaration = declaration
    { instance_constraints = map canonicalizeConstraint
        $ instance_constraints declaration
    , instance_head = canonicalizeConstraint $ instance_head declaration
    }

  canonicalizeConstraint = fmap SharedType.canonicalizeType

  implicitInstanceVariables declaration = S.toAscList
    $ instanceConstraintVariables
    $ instance_head declaration : instance_constraints declaration

  buildFirstOccurrenceClassTable = L.foldl' insertFirst M.empty
    where
      insertFirst table declaration =
        M.insertWith (\_ firstDeclaration -> firstDeclaration)
          (tclass_name declaration) declaration table

  validateClassesInSourceOrder table = go S.empty
    where
      go _ [] = Right ()
      go seen (declaration : rest) = do
        validateClassName name
        if name `S.member` seen
          then Left $ DuplicateClassDeclaration name
          else do
            preflightClass table declaration
            validateClass table declaration
            go (S.insert name seen) rest
        where
          name = tclass_name declaration

  validateClass table declaration = traverse_
    (validateConstraintInTable table $ ClassSuperclass name)
    constraints
    >> validateParameters
    >> validateSuperclassVariables
    where
      name = tclass_name declaration
      parameters = tclass_params declaration
      constraints = tclass_constraints declaration
      validateParameters = case L.find (< 0) parameters of
        Just invalid -> Left $ NegativeClassParameter name invalid
        Nothing -> case SharedCollection.firstDuplicate parameters of
          Just duplicate -> Left $ DuplicateClassParameter name duplicate
          Nothing -> Right ()
      validateSuperclassVariables = case S.toAscList
          (constraintVariables constraints S.\\ S.fromList parameters) of
        [] -> Right ()
        unbound -> Left $ UndeclaredSuperclassVariables name unbound
      -- The historical class environment stores raw flexible IDs rather than
      -- tagged synthesis variables, so this projection is intentionally local.
      constraintVariables = foldMap
        (foldMap freeVars . constraint_params)

  validateInstance table instanceDeclaration = do
    let headConstraint = instance_head instanceDeclaration
        headName = constraint_tclass headConstraint
    validateConstraintInTable table InstanceHead headConstraint
    traverse_
      (validateConstraintInTable table $ InstancePrerequisite headName)
      (instance_constraints instanceDeclaration)

  -- Exact or size-preserving cycles are safe because the solver remembers
  -- constraints already on the current proof path. An expanding edge is not:
  -- @C [a] => C a@ turns a finite ground query into an infinite sequence of
  -- ever-larger constraints. These two Paterson-style checks make every
  -- explicit or superclass-completed prerequisite structurally
  -- non-increasing.
  validateInstanceTermination instanceDeclaration = traverse_
    validatePrerequisite $ instance_constraints instanceDeclaration
   where
    headConstraint = instance_head instanceDeclaration
    (headSize, headOccurrences) = constraintTerminationMeasure headConstraint
    validatePrerequisite prerequisite
      -- A variable absent from the head cannot be instantiated by matching
      -- this rule. The solver classifies that prerequisite as variable-bearing
      -- and stops before recursive resolution, so it poses no growth risk.
      | not $ M.keysSet prerequisiteOccurrences
          `S.isSubsetOf` M.keysSet headOccurrences = Right ()
      | prerequisiteSize <= headSize
      , all noMoreFrequent $ M.toList prerequisiteOccurrences = Right ()
      | otherwise = Left
          $ ExpandingInstancePrerequisite headConstraint prerequisite
     where
      (prerequisiteSize, prerequisiteOccurrences) =
        constraintTerminationMeasure prerequisite
      noMoreFrequent (variable, count) = count <= M.findWithDefault 0
        variable headOccurrences

  validateSuperclassGraph table = case
    [ map tclass_name declarations
    | CyclicSCC declarations <- stronglyConnComp
        [ (declaration, name, map constraint_tclass
            $ tclass_constraints declaration)
        | (name, declaration) <- M.toAscList table
        ]
    ] of
      cycleNames : _ -> Left $ SuperclassCycle cycleNames
      [] -> Right ()

  indexInstances = M.fromListWith (++) . map
    (\instanceDeclaration ->
      ( constraint_tclass $ instance_head instanceDeclaration
      , [instanceDeclaration]
      ))

-- | Structural size and free flexible-variable multiplicity used by the
-- instance termination check. Counts are unbounded 'Integer's so sealing a
-- very large but finite program cannot wrap around into an unsafe result.
constraintTerminationMeasure
  :: HsConstraint
  -> (Integer, M.Map TVarId Integer)
constraintTerminationMeasure = measureConstraint S.empty
 where
  measureConstraint bound (HsConstraint _ arguments) = L.foldl'
    (combineMeasure bound) (1, M.empty) arguments

  combineMeasure bound accumulated typeExpression =
    addMeasures accumulated $ measureType bound typeExpression

  measureType bound typeExpression = case typeExpression of
    SharedType.TypeVariable variable ->
      ( 1
      , case SharedType.flexibleVariableIdentity variable of
          Just identifier
            | variable `S.notMember` bound -> M.singleton identifier 1
          _ -> M.empty
      )
    SharedType.TypeConstructor{} -> (1, M.empty)
    SharedType.TypeApplication function argument -> addMeasures
      (1, M.empty)
      $ addMeasures (measureType bound function) (measureType bound argument)
    SharedType.FunctionType parameter result -> addMeasures
      (1, M.empty)
      $ addMeasures (measureType bound parameter) (measureType bound result)
    SharedType.TupleType _ elements -> L.foldl'
      (combineMeasure bound) (1, M.empty) elements
    SharedType.ForallType binders constraints body ->
      let nextBound = S.fromList binders `S.union` bound
          binderMeasure = L.foldl'
            (\measure _ -> addMeasures measure (1, M.empty))
            (1, M.empty)
            binders
          constraintMeasure = L.foldl'
            (\measure constraint -> addMeasures measure
              $ measureConstraint nextBound constraint)
            binderMeasure
            constraints
      in addMeasures constraintMeasure $ measureType nextBound body

  addMeasures (leftSize, leftOccurrences) (rightSize, rightOccurrences) =
    ( leftSize + rightSize
    , M.unionWith (+) leftOccurrences rightOccurrences
    )

validateConstraintHeader
  :: M.Map QualifiedName HsTypeClass
  -> ConstraintSite
  -> HsConstraint
  -> Either ClassEnvError ()
validateConstraintHeader table site constraint = do
  validateConstraintClass constraint
  case M.lookup name table of
    Nothing -> Left $ UnknownConstraintClass site name
    Just declaration -> validateConstraintArity site constraint declaration
 where
  name = constraint_tclass constraint

-- | Strict closed-world validation: the class must be declared and supplied
-- exactly its declared number of parameters.
validateConstraintInEnv
  :: StaticClassEnv
  -> ConstraintSite
  -> HsConstraint
  -> Either ClassEnvError ()
validateConstraintInEnv environment =
  validateConstraintInTable $ sClassEnv_tclasses environment

-- | Validate the arity of a known class while retaining an unknown class as a
-- nominal external constraint.  This is Exference's public query/signature
-- policy when only part of a source environment has been loaded.
validateKnownConstraintInEnv
  :: StaticClassEnv
  -> ConstraintSite
  -> HsConstraint
  -> Either ClassEnvError ()
validateKnownConstraintInEnv environment site constraint = do
  validateConstraintClass constraint
  validateKnownConstraintArityInEnv environment site constraint
  validateConstraintArguments site constraint

-- | Check only the arity of a class declared in the supplied environment.
-- Unknown external classes and class-name syntax remain the responsibility of
-- the caller's full policy. This narrow preflight is useful before traversing
-- raw constraint arguments: an already impossible known arity can be rejected
-- after one cell beyond the declaration width, even for a cyclic list.
validateKnownConstraintArityInEnv
  :: StaticClassEnv
  -> ConstraintSite
  -> HsConstraint
  -> Either ClassEnvError ()
validateKnownConstraintArityInEnv environment site constraint =
  SharedConstraint.validateKnownConstraintArityWith
    (\name -> length . tclass_params <$> M.lookup name
      (sClassEnv_tclasses environment))
    (ConstraintArityMismatch site)
    constraint

validateConstraintInTable
  :: M.Map QualifiedName HsTypeClass
  -> ConstraintSite
  -> HsConstraint
  -> Either ClassEnvError ()
validateConstraintInTable table site constraint =
  validateConstraintHeader table site constraint
    >> validateConstraintArguments site constraint

validateConstraintArity
  :: ConstraintSite
  -> HsConstraint
  -> HsTypeClass
  -> Either ClassEnvError ()
validateConstraintArity site constraint declaration
  = SharedConstraint.validateKnownConstraintArityWith
      (const $ Just expected)
      (ConstraintArityMismatch site)
      constraint
  where
    expected = length $ tclass_params declaration

validateConstraintClass :: HsConstraint -> Either ClassEnvError ()
validateConstraintClass constraint =
  validateClassName $ constraint_tclass constraint

-- Validate argument syntax only after the owning class and its arity.  This
-- preserves the historical nominal-error precedence while ensuring every
-- type sealed in a class environment satisfies the native shared invariant.
validateConstraintArguments
  :: ConstraintSite
  -> HsConstraint
  -> Either ClassEnvError ()
validateConstraintArguments site constraint = traverse_ validateArgument
  $ zip [0 ..] $ constraint_params constraint
 where
  className = constraint_tclass constraint
  validateArgument (index, argument) = case toSynthesisType argument of
    Left failure -> Left
      $ InvalidConstraintArgument site className index failure
    Right _ -> Right ()

validateClassName :: QualifiedName -> Either ClassEnvError ()
validateClassName name = case SharedConstraint.validateConstraintClassName
    name of
  Left _ -> Left $ InvalidClassName name
  Right () -> Right ()

-- | A 'StaticClassEnv' extended with the constraints a query may assume
-- (for example from the goal's context), together with their superclass
-- closure.  Build it with 'mkQueryClassEnv' and extend it with
-- 'addQueryClassEnv'.
--
-- This representation is sealed for the same reason: assumed constraints and
-- their superclass closure must be updated together by 'addQueryClassEnv'.
data QueryClassEnv = QueryClassEnv
  !StaticClassEnv
  !(S.Set HsConstraint)
  !(S.Set HsConstraint)

-- | The underlying static class environment.
qClassEnv_env :: QueryClassEnv -> StaticClassEnv
qClassEnv_env (QueryClassEnv environment _ _) = environment

-- | The assumed constraints as added, canonicalized but without their
-- superclass closure.
qClassEnv_constraints :: QueryClassEnv -> S.Set HsConstraint
qClassEnv_constraints (QueryClassEnv _ constraints _) = constraints

-- | The assumed constraints together with every superclass constraint they
-- transitively imply, as computed by 'inflateHsConstraints'; this is a
-- superset of 'qClassEnv_constraints'.
qClassEnv_inflatedConstraints :: QueryClassEnv -> S.Set HsConstraint
qClassEnv_inflatedConstraints (QueryClassEnv _ _ constraints) = constraints

-- deepseq provides the same Generic-derived defaults that this package used
-- to obtain from deepseq-generics.  The native shared type already has its
-- own 'NFData' instance; forcing a class environment still walks only finite
-- nominal constraints rather than a recursively tied declaration graph.
instance NFData HsTypeClass
instance NFData HsInstance
instance NFData StaticClassEnv where
  rnf (StaticClassEnv classes explicitInstances indexedInstances) =
    rnf classes `seq` rnf explicitInstances `seq` rnf indexedInstances

instance NFData QueryClassEnv where
  rnf (QueryClassEnv environment constraints inflatedConstraints) =
    rnf environment `seq` rnf constraints `seq` rnf inflatedConstraints

-- | Render a type as Haskell source with fully qualified names, spelling
-- variables by the supplied source-name hints where available and by
-- 'defaultVariableName' otherwise; see 'showHsTypeWithQualification'.
showHsType :: TypeVarIndex -> HsType -> String
showHsType = showHsTypeWithQualification FullyQualified

-- | Render a parsed source type with its original variable-name hints and an
-- explicit nominal qualification policy. Interactive kind normalization uses
-- this to follow the same @:set qualification@ contract as generated terms and
-- @:type@ results.
showHsTypeWithQualification
  :: Qualification
  -> TypeVarIndex
  -> HsType
  -> String
showHsTypeWithQualification qualification sourceNames source = case
    sourceRenderingPlan sourceNames $ Identity source of
  (Identity scopedSource, renderVariable) ->
    SharedRender.renderTypeWithQualification qualification renderVariable
      scopedSource

-- instance Read HsType where
--   readsPrec _ = maybeToList . parseType

-- | Render a constraint as Haskell source with fully qualified names, using
-- the same variable-naming plan as 'showHsType': source-name hints are
-- honoured first and remaining variables receive fresh default spellings.
showHsConstraint :: TypeVarIndex
                 -> HsConstraint
                 -> String
showHsConstraint sourceNames source = case
    sourceRenderingPlan sourceNames source of
  (scopedSource, renderVariable) ->
    SharedRender.renderConstraint renderVariable scopedSource

-- | The default rendered spelling of a variable that has no source-name
-- hint: 'showVar' of its identity for a flexible variable, and the same
-- prefixed with @C@ for a rigid one.
defaultVariableName :: SynthesisVariable -> String
defaultVariableName variable = case variable of
  SharedType.FlexibleVariable identifier -> showVar identifier
  SharedType.RigidVariable identifier -> "C" ++ showVar identifier

-- The raw shared tree identifies variables by engine ID.  That is sufficient
-- for inference, but source conversion may reuse an ID in a nested lexical
-- scope before its alpha-normalization pass assigns the inner binder a fresh
-- ID.  Rendering must therefore distinguish binder occurrences by scope as
-- well as by ID; otherwise a generated fallback can capture a free source
-- spelling (for example, an inner default @c@ and a free source variable @c@).
data SourceRenderVariable
  = SourceRenderFree !SynthesisVariable
  | SourceRenderBound !Natural !Natural
  deriving (Eq, Ord)

data SourceRenderState = SourceRenderState
  { sourceRenderNextScope :: !Natural
  , sourceRenderOriginalVariables
      :: !(M.Map SourceRenderVariable SynthesisVariable)
  }

-- Alpha identities intentionally discard source provenance. Rendering needs
-- the opposite combination: lexical identities for safety, plus each source
-- ID for spelling hints. Keep that naming plan separate from semantic alpha
-- comparison and share it across both complete types and constraints.
sourceRenderingPlan
  :: Traversable container
  => TypeVarIndex
  -> container HsType
  -> ( container (SharedType.Type SourceRenderVariable)
     , SourceRenderVariable -> String
     )
sourceRenderingPlan sourceNames source = (scopedSource, renderVariable)
 where
  (scopedSource, finalState) = runState
    (traverse (scopeRenderTypeWith M.empty) source)
    (SourceRenderState 0 M.empty)
  originalVariables = sourceRenderOriginalVariables finalState
  variableNames = allocateSourceVariableNames sourceNames originalVariables
    $ foldMap toList scopedSource
  renderVariable variable = M.findWithDefault
    (renderVariableFallback originalVariables variable)
    variable variableNames

scopeRenderTypeWith
  :: M.Map SynthesisVariable SourceRenderVariable
  -> HsType
  -> State SourceRenderState (SharedType.Type SourceRenderVariable)
scopeRenderTypeWith bindings source = case source of
  SharedType.TypeVariable original -> do
    let scoped = M.findWithDefault (SourceRenderFree original)
          original bindings
    rememberSourceRenderVariable scoped original
    pure $ SharedType.TypeVariable scoped
  SharedType.TypeConstructor constructor ->
    pure $ SharedType.TypeConstructor constructor
  SharedType.TypeApplication function argument ->
    SharedType.TypeApplication
      <$> scopeRenderTypeWith bindings function
      <*> scopeRenderTypeWith bindings argument
  SharedType.FunctionType parameter result -> SharedType.FunctionType
    <$> scopeRenderTypeWith bindings parameter
    <*> scopeRenderTypeWith bindings result
  SharedType.TupleType boxity elements -> SharedType.TupleType boxity
    <$> mapM (scopeRenderTypeWith bindings) elements
  SharedType.ForallType variables constraints body -> do
    scope <- allocateSourceRenderScope
    let scopedVariables = zipWith
          (\position _ -> SourceRenderBound scope position)
          [0 ..] variables
        nestedBindings = M.fromList (zip variables scopedVariables)
          `M.union` bindings
    mapM_ (uncurry rememberSourceRenderVariable)
      $ zip scopedVariables variables
    scopedConstraints <- mapM
      (traverse $ scopeRenderTypeWith nestedBindings) constraints
    scopedBody <- scopeRenderTypeWith nestedBindings body
    pure $ SharedType.ForallType scopedVariables scopedConstraints scopedBody

allocateSourceRenderScope :: State SourceRenderState Natural
allocateSourceRenderScope = do
  current <- get
  let scope = sourceRenderNextScope current
  put current {sourceRenderNextScope = scope + 1}
  pure scope

rememberSourceRenderVariable
  :: SourceRenderVariable
  -> SynthesisVariable
  -> State SourceRenderState ()
rememberSourceRenderVariable scoped original = do
  current <- get
  put current
    { sourceRenderOriginalVariables = M.insert scoped original
        $ sourceRenderOriginalVariables current
    }

-- Assign names over the complete rendered tree, not independently at each
-- occurrence.  Real source hints are reserved first, so a generated fallback
-- can never steal a spelling which belongs to a later free variable.
allocateSourceVariableNames
  :: TypeVarIndex
  -> M.Map SourceRenderVariable SynthesisVariable
  -> [SourceRenderVariable]
  -> M.Map SourceRenderVariable String
allocateSourceVariableNames sourceNames originalVariables variables = names
 where
  orderedVariables = SharedCollection.distinctOn id variables
  preferredNames = preferredSourceTypeVariableNames
    [ (validSpelling, variable)
    | (spelling, variable) <- M.toList sourceNames
    , Just validSpelling <- [validSourceVariableName spelling]
    ]
  (hintedNames, hintedSpellings) = L.foldl' reserveHint
    (M.empty, S.empty) orderedVariables
  (names, _) = L.foldl' allocateFallback
    (hintedNames, hintedSpellings) orderedVariables

  sourceHint variable = do
    original <- M.lookup variable originalVariables
    IntMap.lookup
      (SharedType.variableIdentity original) preferredNames

  preferredBase variable = case sourceHint variable of
    Just spelling -> spelling
    Nothing -> renderVariableFallback originalVariables variable

  reserveHint state@(assigned, used) variable = case sourceHint variable of
    Just spelling
      | S.notMember spelling used ->
          (M.insert variable spelling assigned, S.insert spelling used)
    _ -> state

  allocateFallback state@(assigned, used) variable
    | M.member variable assigned = state
    | otherwise =
        let spelling = SharedFresh.selectFresh (++ "'") used
              $ preferredBase variable
        in (M.insert variable spelling assigned, S.insert spelling used)

renderVariableFallback
  :: M.Map SourceRenderVariable SynthesisVariable
  -> SourceRenderVariable
  -> String
renderVariableFallback originalVariables variable = case
    M.lookup variable originalVariables of
  Just original -> defaultVariableName original
  Nothing -> "v0"

validSourceVariableName :: String -> Maybe String
validSourceVariableName spelling
  | spelling == "_" || spelling == "family" = Nothing
  | otherwise = case SharedName.mkIdentifier spelling of
      Right identifier
        | SharedName.nameLexicalClass identifier == SharedName.VariableLike ->
            Just spelling
      _ -> Nothing

instance Show QueryClassEnv where
  show (QueryClassEnv _ cs _) = "(QueryClassEnv _ " ++ show cs ++ " _)"

-- | Whether the flexible variable with the given identity occurs free in the
-- type; see 'freeVars'.
containsVar :: TVarId -> HsType -> Bool
containsVar i = S.member i . freeVars

-- | Build a query environment over a static class environment with the given
-- initially assumed constraints; equivalent to 'addQueryClassEnv' on an
-- environment with no assumptions.
mkQueryClassEnv :: StaticClassEnv -> [HsConstraint] -> QueryClassEnv
mkQueryClassEnv sClassEnv constrs = addQueryClassEnv constrs
  $ QueryClassEnv sClassEnv S.empty S.empty

-- | Add further assumed constraints.  They are canonicalized, unioned with
-- the existing assumptions, and the superclass closure
-- ('qClassEnv_inflatedConstraints') is recomputed for the whole set.
addQueryClassEnv :: [HsConstraint] -> QueryClassEnv -> QueryClassEnv
addQueryClassEnv constrs env = QueryClassEnv
  (qClassEnv_env env) csSet inflated
  where
    canonicalConstraints = map
      (fmap SharedType.canonicalizeType)
      constrs
    csSet = S.fromList canonicalConstraints
      `S.union` qClassEnv_constraints env
    inflated = inflateHsConstraints (qClassEnv_env env) csSet

-- | Add all transitively implied superclasses using declarations from the
-- explicit environment.  Arity is checked before constructing the
-- substitution: malformed query constraints never receive a silently
-- truncated @zip@ substitution.
inflateHsConstraints
  :: StaticClassEnv
  -> S.Set HsConstraint
  -> S.Set HsConstraint
inflateHsConstraints environment = SharedCollection.transitiveClosure
  $ S.fromList . directSuperclasses environment

-- | Complete each explicit instance rule with the direct superclass
-- dictionaries required by its class declaration. The head is never
-- projected to a superclass: having a way to construct @B a@ from @A a@ does
-- not provide an @A a@ dictionary in the opposite direction. Existing source
-- prerequisites win during stable deduplication.
--
-- The historical name is retained for compatibility. Exact arity is checked
-- by 'directSuperclasses', so malformed caller-built heads simply receive no
-- added obligations rather than being truncated by @zip@.
inflateInstances :: StaticClassEnv -> [HsInstance] -> [HsInstance]
inflateInstances environment =
  map completeSuperclasses
 where
  completeSuperclasses (HsInstance prerequisites headConstraint) = HsInstance
    (SharedCollection.distinctOn id
      $ prerequisites ++ directSuperclasses environment headConstraint)
    headConstraint

-- | Instantiate one constraint's declared immediate superclasses. The arity
-- guard keeps the two low-level closure helpers total for caller-built
-- constraints even though sealed class environments validate every stored
-- declaration and instance before invoking them.
directSuperclasses :: StaticClassEnv -> HsConstraint -> [HsConstraint]
directSuperclasses environment
    (HsConstraint className arguments) = case
  M.lookup className $ sClassEnv_tclasses environment of
    Just (HsTypeClass _ parameters constraints)
      | let expected = length parameters
      , SharedCollection.observedListLength expected arguments == expected ->
          let substitutions = IntMap.fromList $ zip parameters arguments
          in map (snd . constraintApplySubsts substitutions) constraints
    _ -> []

-- | Checked simultaneous substitution across every argument of a constraint
-- under one shared fresh-variable reservation set.
constraintApplySubstsChecked
  :: Substs
  -> HsConstraint
  -> Either HsSubstitutionError (Any, HsConstraint)
constraintApplySubstsChecked substitutions constraint
  | IntMap.null substitutions = Right (Any False, constraint)
  | HsConstraint className parameters <- constraint = do
      substituted <- substituteSharedBatch substitutions parameters
      Right
        ( Any $ substitutionsAffect substitutions $ foldMap freeVars parameters
        , HsConstraint className
            $ map canonicalizeSubstitutionResult substituted
        )

{-# INLINE constraintApplySubsts #-}
-- | Compatibility wrapper around 'constraintApplySubstsChecked'.  It throws a
-- descriptive exception only if capture avoidance would require a fresh
-- variable after every 'Int' identity has been reserved.
constraintApplySubsts :: Substs -> HsConstraint -> (Any, HsConstraint)
constraintApplySubsts substitutions = checkedSubstitution
  "constraintApplySubsts" . constraintApplySubstsChecked substitutions

-- | Default source spelling of a flexible variable identity: @v0@ for 0,
-- single letters @a@ to @z@ for 1 to 26, @t@ followed by @i - 27@ above that,
-- and @vn@ followed by the magnitude for negative identities.
showVar :: TVarId -> String
showVar 0 = "v0"
showVar i
  | i < 0 = "vn" ++ show (negate $ toInteger i)
  | i < 27 = [chr (ord 'a' + i - 1)]
  | otherwise = "t" ++ show (i - 27)

-- | Suggest a readable binder spelling from its type.  This is only a
-- preference: a renderer must still allocate a fresh name because distinct
-- variable IDs can have the same suggestion and globals may use it too.
-- Present hints are authoritative at the shared rendering boundary, so a
-- derived plural such as local 1's historical list suggestion @as@ must fall
-- back here instead of becoming a late rendering error.
preferredVarName :: TVarId -> HsType -> String
preferredVarName i source = case SharedName.mkIdentifier suggestion of
  Right candidate
    | SharedName.nameLexicalClass candidate == SharedName.VariableLike ->
        suggestion
  _ -> showVar i
 where
  suggestion = h source
  suffix
    | i < 0 = "n" ++ show (negate $ toInteger i)
    | otherwise = show i
  h TypeVar{}          = showVar i
  h TypeConstant{}     = showVar i
  h (TypeCons qName) = case qualifiedNameOccurrence qName of
    SharedName.IdentifierOccurrence _ (c : _) -> toLower c : suffix
    SharedName.IdentifierOccurrence _ [] -> showVar i
    -- A symbolic type constructor has no identifier stem from which to form a
    -- legal term binder.  Falling back avoids historical names such as @:1@.
    SharedName.OperatorOccurrence _ _ -> showVar i
    SharedName.SpecialOccurrence SharedName.ListConstructor -> showVar i ++ "s"
    SharedName.SpecialOccurrence _ -> showVar i
  h TypeArrow{}        = "f" ++ suffix
  h (TypeApp t _)      = h t
  h TypeTuple{}        = showVar i
  h (TypeForallNative _ _ t) = h t

-- | Capture-avoiding single-variable substitution.
applySubstChecked
  :: Subst
  -> HsType
  -> Either HsSubstitutionError HsType
applySubstChecked (Subst variable replacement) source = snd
  <$> applySubstsChecked (IntMap.singleton variable replacement) source

-- | Compatibility wrapper around 'applySubstChecked'; see 'applySubsts' for
-- the exceptional finite-namespace invariant.
applySubst :: Subst -> HsType -> HsType
applySubst substitution = checkedSubstitution "applySubst"
  . applySubstChecked substitution

-- | Simultaneously substitute free flexible variables, alpha-renaming forall
-- binders when a replacement would otherwise be captured.  The @Any@ flag
-- retains Exference's historical operational meaning: it is true whenever a
-- substitution key occurs free in the source, including an identity mapping.
applySubstsChecked
  :: Substs
  -> HsType
  -> Either HsSubstitutionError (Any, HsType)
applySubstsChecked substitutions source
  | IntMap.null substitutions = Right (Any False, source)
  | otherwise = do
      substituted <- substituteShared substitutions
        source
      Right
        ( Any $ substitutionsAffect substitutions $ freeVars source
        , canonicalizeSubstitutionResult substituted
        )

-- | Legacy total-shaped substitution API.  Capture avoidance over a finite
-- 'Int' namespace is itself fallible: this wrapper throws a descriptive
-- exception if every identity is reserved.  Call 'applySubstsChecked' when
-- that theoretical exhaustion case must be represented explicitly.
applySubsts :: Substs -> HsType -> (Any, HsType)
applySubsts substitutions = checkedSubstitution "applySubsts"
  . applySubstsChecked substitutions

substituteShared
  :: Substs
  -> HsType
  -> Either HsSubstitutionError HsType
substituteShared substitutions = first SharedSubstitutionFailure
  . SharedType.substituteTypeVariables
      freshSynthesisVariable S.empty (sharedSubstitutions substitutions)

substituteSharedBatch
  :: Substs
  -> [HsType]
  -> Either HsSubstitutionError [HsType]
substituteSharedBatch substitutions = first SharedSubstitutionFailure
  . SharedType.substituteTypeVariablesBatch
      freshSynthesisVariable S.empty (sharedSubstitutions substitutions)

sharedSubstitutions :: Substs -> M.Map SynthesisVariable HsType
sharedSubstitutions substitutions = M.fromList
  [ (SharedType.FlexibleVariable variable, replacement)
  | (variable, replacement) <- IntMap.toList substitutions
  ]

substitutionsAffect :: Substs -> S.Set TVarId -> Bool
substitutionsAffect substitutions variables = any
  (`S.member` variables) $ IntMap.keys substitutions

checkedSubstitution :: String -> Either HsSubstitutionError result -> result
checkedSubstitution operation = either failure id
 where
  failure substitutionError = error
    $ "Language.Haskell.Exference.Core.Types."
    ++ operation
    ++ ": capture-avoiding substitution failed: "
    ++ show substitutionError

-- | The identities of the flexible variables occurring free in a type.
-- Rigid constants and forall-bound variables are excluded.
freeVars :: HsType -> S.Set TVarId
freeVars typeExpression = S.fromList
  [ identifier
  | SharedType.FlexibleVariable identifier <- S.toList
      $ SharedType.freeVariables typeExpression
  ]

-- | Collect every class constraint embedded in a type, including contexts
-- nested under arrows and applications.  Keeping this traversal in the core
-- type module lets both public-input validation and source-environment
-- validation apply their deliberately different unknown-class policies to
-- exactly the same syntax.
typeConstraints :: HsType -> [HsConstraint]
typeConstraints = SharedType.typeConstraints
