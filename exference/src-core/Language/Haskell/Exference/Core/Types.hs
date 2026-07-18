{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

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
  )
where



import Data.Bifunctor (first)
import Data.Char ( ord, chr, toLower )
import Data.Foldable (traverse_)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.Monoid ( Any(..) )

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
import qualified Language.Haskell.Synthesis.Name as SharedName
import Language.Haskell.Synthesis.Name (Boxity (..))
import qualified Language.Haskell.Synthesis.Type as SharedType
import qualified Language.Haskell.Synthesis.TypeRender as SharedRender

import Control.DeepSeq
import GHC.Generics




type TVarId = Int
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

data SynthesisTypeError
  = InvalidSynthesisType (SharedType.TypeError SynthesisVariable)
  | InvalidSynthesisConstraint SharedConstraint.ConstraintError
  | RigidForallBinder TVarId
  deriving (Eq, Ord, Show, Generic)

instance NFData SynthesisTypeError

data Subst  = Subst {-# UNPACK #-} !TVarId !HsType
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

data HsTypeOffset = HsTypeOffset !HsType {-# UNPACK #-} !Int

-- Source locations do not belong in variable identity. Keeping only the
-- spelling also decouples conversion and historical rendering from a
-- particular parser AST. This raw spelling-oriented compatibility map is not
-- a trusted canonical-search input; cross it through
-- @mkExferenceSourceTypeVariableHints@ or the checked request boundary first,
-- which also binds it to the exact canonical goal whose IDs it describes.
type TypeVarIndex = M.Map String Int

data HsTypeClass = HsTypeClass
  { tclass_name :: QualifiedName
  , tclass_params :: [TVarId]
  , tclass_constraints :: [HsConstraint]
  }
  deriving (Eq, Ord, Show, Generic)

data HsInstance = HsInstance
  { instance_constraints :: [HsConstraint]
  , instance_head :: HsConstraint
  }
  deriving (Eq, Show, Ord, Generic)

-- | Every free variable of an instance's head and prerequisite constraints:
-- the binder set 'HsInstance' quantifies implicitly. The compatibility class
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

-- 'QualifiedName' guarantees general lexical validity; the
-- checked type and environment boundaries additionally enforce that the name
-- occupies the class namespace.
constraint_tclass :: HsConstraint -> QualifiedName
constraint_tclass = SharedConstraint.constraintClass

constraint_params :: HsConstraint -> [HsType]
constraint_params = SharedConstraint.constraintArguments

-- | Validate and canonicalize both the nominal class identity and every type
-- argument of a native shared constraint.
toSynthesisConstraint
  :: HsConstraint
  -> Either SynthesisTypeError
       HsConstraint
toSynthesisConstraint constraint = do
  converted <- traverse toSynthesisType constraint
  either (Left . InvalidSynthesisConstraint) Right
    $ SharedConstraint.validateConstraint converted
  return converted

-- | Validate and canonicalize a shared constraint for Exference. Structural
-- names such as unboxed tuple constructors are rejected in the /class-name/
-- position by shared namespace validation; unboxed tuple types remain valid
-- arguments.
fromSynthesisConstraint
  :: HsConstraint
  -> Either SynthesisTypeError HsConstraint
fromSynthesisConstraint constraint = do
  either (Left . InvalidSynthesisConstraint) Right
    $ SharedConstraint.validateConstraint constraint
  traverse fromSynthesisType constraint

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
  SuperclassCycle names -> constructor "SuperclassCycle" [show names]
 where
  constructor name fields = unwords $ name : fields
  sourceConstraintList constraints = "[" ++ L.intercalate ","
    (map (showHsConstraint M.empty) constraints) ++ "]"

-- Positional fields are deliberate.  Exported record labels would let a
-- downstream caller update the declarations or either derived index and
-- bypass 'mkStaticClassEnv'. Explicit instances remain separate because the
-- per-class map also contains implied superclass instances.
data StaticClassEnv = StaticClassEnv
  !(M.Map QualifiedName HsTypeClass)
  ![HsInstance]
  !(M.Map QualifiedName [HsInstance])
  deriving (Eq, Show)

sClassEnv_tclasses :: StaticClassEnv -> M.Map QualifiedName HsTypeClass
sClassEnv_tclasses (StaticClassEnv classes _ _) = classes

sClassEnv_explicitInstances :: StaticClassEnv -> [HsInstance]
sClassEnv_explicitInstances (StaticClassEnv _ instances _) = instances

sClassEnv_instances :: StaticClassEnv -> M.Map QualifiedName [HsInstance]
sClassEnv_instances (StaticClassEnv _ _ instances) = instances

-- | The canonical validated empty environment, also used for parser recovery.
-- Every non-empty environment is built with 'mkStaticClassEnv'.
emptyStaticClassEnv :: StaticClassEnv
emptyStaticClassEnv = StaticClassEnv M.empty [] M.empty

-- | Validate and index a finite class environment.  Instance inflation runs
-- only after declarations, heads, prerequisites, arities, and the superclass
-- graph have been checked.
mkStaticClassEnv
  :: [HsTypeClass]
  -> [HsInstance]
  -> Either ClassEnvError StaticClassEnv
mkStaticClassEnv sourceClasses sourceInstances = do
  classTable <- buildClassTable classes
  traverse_ (validateClass classTable) classes
  case SharedEnvironment.repeatedInstanceHeadsInFirstRepetitionOrder
      [ (implicitInstanceVariables declaration, instance_head declaration)
      | declaration <- instances
      ] of
    [] -> Right ()
    duplicates -> Left $ DuplicateInstanceHeads duplicates
  traverse_ (validateInstance classTable) instances
  validateSuperclassGraph classTable
  let declarations = StaticClassEnv classTable [] M.empty
      allInstances = inflateInstances declarations instances
  return $ StaticClassEnv classTable instances $ indexInstances allInstances
 where
  -- Canonicalization is total, so normalizing before validation does not
  -- disturb error precedence.  It does ensure that duplicate-head checks,
  -- superclass inflation, and every value sealed in the environment observe
  -- the same structural function/tuple representation.
  classes = map canonicalizeClass sourceClasses
  instances = map canonicalizeInstance sourceInstances

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

  buildClassTable = go M.empty
    where
      go table [] = Right table
      go table (declaration : rest) = do
        validateClassName name
        if M.member name table
          then Left $ DuplicateClassDeclaration name
          else go (M.insert name declaration table) rest
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
  case M.lookup (constraint_tclass constraint)
      (sClassEnv_tclasses environment) of
    Nothing -> validateConstraintArguments site constraint
    Just declaration -> validateConstraintArity site constraint declaration
      >> validateConstraintArguments site constraint

validateConstraintInTable
  :: M.Map QualifiedName HsTypeClass
  -> ConstraintSite
  -> HsConstraint
  -> Either ClassEnvError ()
validateConstraintInTable table site constraint = do
  validateConstraintClass constraint
  case M.lookup name table of
    Nothing -> Left $ UnknownConstraintClass site name
    Just declaration -> validateConstraintArity site constraint declaration
      >> validateConstraintArguments site constraint
  where
    name = constraint_tclass constraint

validateConstraintArity
  :: ConstraintSite
  -> HsConstraint
  -> HsTypeClass
  -> Either ClassEnvError ()
validateConstraintArity site constraint declaration
  | expected /= actual = Left $
      ConstraintArityMismatch site name expected actual
  | otherwise = Right ()
  where
    name = constraint_tclass constraint
    expected = length $ tclass_params declaration
    actual = SharedCollection.observedListLength expected
      $ constraint_params constraint

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

-- This representation is sealed for the same reason: assumed constraints and
-- their superclass closure must be updated together by 'addQueryClassEnv'.
data QueryClassEnv = QueryClassEnv
  !StaticClassEnv
  !(S.Set HsConstraint)
  !(S.Set HsConstraint)

qClassEnv_env :: QueryClassEnv -> StaticClassEnv
qClassEnv_env (QueryClassEnv environment _ _) = environment

qClassEnv_constraints :: QueryClassEnv -> S.Set HsConstraint
qClassEnv_constraints (QueryClassEnv _ constraints _) = constraints

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

showHsType :: TypeVarIndex -> HsType -> String
showHsType sourceNames = SharedRender.renderType
  (sourceVariableName sourceNames)

-- instance Read HsType where
--   readsPrec _ = maybeToList . parseType

showHsConstraint :: TypeVarIndex
                 -> HsConstraint
                 -> String
showHsConstraint sourceNames = SharedRender.renderConstraint
  (sourceVariableName sourceNames)

defaultVariableName :: SynthesisVariable -> String
defaultVariableName variable = case variable of
  SharedType.FlexibleVariable identifier -> showVar identifier
  SharedType.RigidVariable identifier -> "C" ++ showVar identifier

-- The parser index is spelling-to-ID because name lookup is its primary job.
-- Rendering reverses it once, retaining the lexicographically first spelling
-- if a caller supplies multiple aliases for one ID (the historical behavior).
sourceVariableName
  :: TypeVarIndex
  -> SynthesisVariable
  -> String
sourceVariableName sourceNames = renderVariable
 where
  preferredNames = preferredSourceTypeVariableNames $ M.toList sourceNames
  -- This raw compatibility map predates tagged variables and has only one
  -- numeric key space. Stable candidate hints use separate tagged keys; this
  -- renderer is the intentional legacy boundary where the tag is erased.
  renderVariable variable = IntMap.findWithDefault
    (defaultVariableName variable)
    (SharedType.variableIdentity variable)
    preferredNames

instance Show QueryClassEnv where
  show (QueryClassEnv _ cs _) = "(QueryClassEnv _ " ++ show cs ++ " _)"

containsVar :: TVarId -> HsType -> Bool
containsVar i = S.member i . freeVars

mkQueryClassEnv :: StaticClassEnv -> [HsConstraint] -> QueryClassEnv
mkQueryClassEnv sClassEnv constrs = addQueryClassEnv constrs
  $ QueryClassEnv sClassEnv S.empty S.empty

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

-- | Add instance heads implied by superclass declarations.  Exact arity is
-- checked before substitution, so no malformed head can be truncated by
-- @zip@ even if this helper is called independently of 'mkStaticClassEnv'.
inflateInstances :: StaticClassEnv -> [HsInstance] -> [HsInstance]
inflateInstances environment =
  S.toList
    . SharedCollection.transitiveClosure (S.fromList . superclasses)
    . S.fromList
 where
  superclasses (HsInstance prerequisites headConstraint) = map
    (HsInstance prerequisites) $ directSuperclasses environment headConstraint

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

-- | Compatibility wrapper around 'constraintApplySubstsChecked'.  It throws a
-- descriptive exception only if capture avoidance would require a fresh
-- variable after every 'Int' identity has been reserved.
{-# INLINE constraintApplySubsts #-}
constraintApplySubsts :: Substs -> HsConstraint -> (Any, HsConstraint)
constraintApplySubsts substitutions = checkedSubstitution
  "constraintApplySubsts" . constraintApplySubstsChecked substitutions

showVar :: TVarId -> String
showVar 0 = "v0"
showVar i
  | i < 0 = "vn" ++ show (negate $ toInteger i)
  | i < 27 = [chr (ord 'a' + i - 1)]
  | otherwise = "t" ++ show (i - 27)

-- | Suggest a readable binder spelling from its type.  This is only a
-- preference: a renderer must still allocate a fresh name because distinct
-- variable IDs can have the same suggestion and globals may use it too.
preferredVarName :: TVarId -> HsType -> String
preferredVarName i = h
 where
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
-- binders when a replacement would otherwise be captured.  The 'Any' flag
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
