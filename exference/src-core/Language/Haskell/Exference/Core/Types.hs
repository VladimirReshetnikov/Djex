{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Haskell.Exference.Core.Types
  ( TVarId
  , module Language.Haskell.Exference.Core.Name
  , HsType (..)
  , HsTypeOffset (..)
  , SynthesisVariable
  , SynthesisType
  , SynthesisTypeError (..)
  , toSynthesisTypeStructure
  , toSynthesisType
  , fromSynthesisType
  , Subst (..)
  , Substs
  , HsSubstitutionError (..)
  , HsTypeClass (..)
  , HsInstance (..)
  , HsConstraint
  , pattern HsConstraint
  , constraint_tclass
  , constraint_params
  , toSynthesisConstraintStructure
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



import Data.Char ( ord, chr, toLower )
import Data.Foldable (traverse_)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.Monoid ( Any(..) )

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as L

import Language.Haskell.Exference.Core.Internal.Closure ( closure )
import Language.Haskell.Exference.Core.Internal.VariableSupply
  ( freshSynthesisVariable )
import Language.Haskell.Exference.Core.Name
import qualified Language.Haskell.Synthesis.Collection as SharedCollection
import qualified Language.Haskell.Synthesis.Constraint as SharedConstraint
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Type as SharedType
import qualified Language.Haskell.Synthesis.TypeRender as SharedRender

import Control.DeepSeq
import GHC.Generics




type TVarId = Int
type SynthesisVariable = SharedType.Variable TVarId
type SynthesisType = SharedType.Type SynthesisVariable

data SynthesisTypeError
  = InvalidSynthesisType (SharedType.TypeError SynthesisVariable)
  | InvalidSynthesisConstraint SharedConstraint.ConstraintError
  | RigidForallBinder TVarId
  deriving (Eq, Show, Generic)

instance NFData SynthesisTypeError

data Subst  = Subst {-# UNPACK #-} !TVarId !HsType
type Substs = IntMap.IntMap HsType

-- | A checked substitution can fail only when alpha-renaming exhausts
-- Exference's finite 'Int' identity space, or if the shared substitution
-- primitive violates the structural projection invariant used by this
-- adapter.  The latter constructors keep such an internal defect observable
-- instead of silently returning a captured or unsubstituted type.
data HsSubstitutionError
  = SharedSubstitutionFailure
      (SharedType.SubstitutionError SynthesisVariable)
  | SubstitutionResultTypeError SynthesisTypeError
  | UnexpectedConstraintSubstitutionResult SynthesisType
  deriving (Eq, Show, Generic)

instance NFData HsSubstitutionError

data HsType = TypeVar      {-# UNPACK #-} !TVarId
            | TypeConstant {-# UNPACK #-} !TVarId
              -- like TypeCons, for exference-internal purposes.
            | TypeCons     QualifiedName
            | TypeArrow    !HsType !HsType
            | TypeApp      !HsType !HsType
            | TypeForall   [TVarId] [HsConstraint] !HsType
  deriving (Ord, Eq, Generic)

-- | Structurally project Exference's search representation into the common
-- source type without claiming that binders or constraints are valid.  This
-- total traversal is useful inside the checked search engine; public inputs
-- should normally cross the validating 'toSynthesisType' boundary instead.
toSynthesisTypeStructure :: HsType -> SynthesisType
toSynthesisTypeStructure typeExpression = case typeExpression of
  TypeVar variable -> SharedType.TypeVariable
    $ SharedType.FlexibleVariable variable
  TypeConstant variable -> SharedType.TypeVariable
    $ SharedType.RigidVariable variable
  TypeCons name -> SharedType.TypeConstructor name
  TypeArrow parameter result -> SharedType.FunctionType
    (toSynthesisTypeStructure parameter)
    (toSynthesisTypeStructure result)
  TypeApp function argument -> SharedType.TypeApplication
    (toSynthesisTypeStructure function)
    (toSynthesisTypeStructure argument)
  TypeForall variables constraints body -> SharedType.ForallType
    (map SharedType.FlexibleVariable variables)
    (map toSynthesisConstraintStructure constraints)
    (toSynthesisTypeStructure body)

-- | Convert and validate an Exference type at the shared source boundary.
-- Flexible and rigid IDs remain distinct, and saturated tuple constructors
-- are canonicalized structurally.
toSynthesisType :: HsType -> Either SynthesisTypeError SynthesisType
toSynthesisType source = do
  let canonical = SharedType.canonicalizeType
        $ toSynthesisTypeStructure source
  either (Left . InvalidSynthesisType) Right
    $ SharedType.validateType canonical
  return canonical

-- | Narrow a common type back to Exference's typed search vocabulary.
fromSynthesisType
  :: SynthesisType
  -> Either SynthesisTypeError HsType
fromSynthesisType source = do
  let canonical = SharedType.canonicalizeType source
  either (Left . InvalidSynthesisType) Right
    $ SharedType.validateType canonical
  fromSynthesisTypeStructure canonical

-- Deliberately skip validation and canonicalization here.  Substitution acts
-- on the structural image of an existing 'HsType', which can include
-- temporary search forms (for example a duplicate binder list) that checked
-- public boundaries reject but legacy low-level operations must preserve.
fromSynthesisTypeStructure
  :: SynthesisType
  -> Either SynthesisTypeError HsType
fromSynthesisTypeStructure = convert
 where
  convert typeExpression = case typeExpression of
    SharedType.TypeVariable (SharedType.FlexibleVariable variable) ->
      Right $ TypeVar variable
    SharedType.TypeVariable (SharedType.RigidVariable variable) ->
      Right $ TypeConstant variable
    SharedType.TypeConstructor name -> Right $ TypeCons name
    SharedType.TypeApplication function argument -> TypeApp
      <$> convert function <*> convert argument
    SharedType.FunctionType parameter result -> TypeArrow
      <$> convert parameter <*> convert result
    SharedType.TupleType boxity elements -> do
      tuple <- either
        (const $ Left $ InvalidSynthesisType
          $ SharedType.InvalidTupleTypeArity boxity $ length elements)
        Right
        $ SharedName.tupleName boxity $ length elements
      foldl TypeApp (TypeCons tuple) <$> mapM convert elements
    SharedType.ForallType variables constraints body -> do
      binders <- mapM flexibleBinder variables
      TypeForall binders
        <$> mapM convertConstraint constraints
        <*> convert body

  flexibleBinder (SharedType.FlexibleVariable variable) = Right variable
  flexibleBinder (SharedType.RigidVariable variable) =
    Left $ RigidForallBinder variable

  convertConstraint = traverse convert

data HsTypeOffset = HsTypeOffset !HsType {-# UNPACK #-} !Int

-- Source locations do not belong in variable identity. Keeping only the
-- spelling also decouples the search core from a particular parser AST.
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

-- | Project the whole constraint, including every type argument, into shared
-- syntax without validation.  The checked engine uses this only after input
-- validation has established the invariants preserved by search.
toSynthesisConstraintStructure
  :: HsConstraint
  -> SharedConstraint.Constraint SynthesisType
toSynthesisConstraintStructure = fmap toSynthesisTypeStructure

-- | Convert a constraint completely to shared syntax and validate both its
-- nominal class identity and all type arguments.
toSynthesisConstraint
  :: HsConstraint
  -> Either SynthesisTypeError
       (SharedConstraint.Constraint SynthesisType)
toSynthesisConstraint constraint = do
  converted <- traverse toSynthesisType constraint
  either (Left . InvalidSynthesisConstraint) Right
    $ SharedConstraint.validateConstraint converted
  return converted

-- | Narrow a fully shared constraint back to Exference. Structural names such
-- as unboxed tuple constructors are rejected in the /class-name/ position by
-- shared namespace validation; unboxed tuple types remain valid arguments.
fromSynthesisConstraint
  :: SharedConstraint.Constraint SynthesisType
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
  | DuplicateInstanceHeads [HsConstraint]
  | SuperclassCycle [QualifiedName]
  deriving (Eq, Ord, Show)

-- Positional fields are deliberate.  Exported record labels would let a
-- downstream caller update the declarations or either derived index and
-- bypass 'mkStaticClassEnv'. Explicit instances remain separate because the
-- per-class map also contains implied superclass instances.
data StaticClassEnv = StaticClassEnv
  !(M.Map QualifiedName HsTypeClass)
  ![HsInstance]
  !(M.Map QualifiedName [HsInstance])
  deriving (Eq, Show, Generic)

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
mkStaticClassEnv tclasses instances = do
  classTable <- buildClassTable tclasses
  traverse_ (validateClass classTable) tclasses
  case SharedCollection.repeatedValuesInFirstRepetitionOrder
      $ SharedCollection.summarizeDuplicates
      $ map instance_head instances of
    [] -> Right ()
    duplicates -> Left $ DuplicateInstanceHeads duplicates
  traverse_ (validateInstance classTable) instances
  validateSuperclassGraph classTable
  let declarations = StaticClassEnv classTable [] M.empty
      allInstances = inflateInstances declarations instances
  return $ StaticClassEnv classTable instances $ indexInstances allInstances
 where
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
        Nothing -> case SharedCollection.repeatedValuesInFirstRepetitionOrder
            $ SharedCollection.summarizeDuplicates parameters of
          duplicate : _ -> Left $ DuplicateClassParameter name duplicate
          [] -> Right ()
      validateSuperclassVariables = case S.toAscList
          (constraintVariables constraints S.\\ S.fromList parameters) of
        [] -> Right ()
        unbound -> Left $ UndeclaredSuperclassVariables name unbound
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
    Nothing -> Right ()
    Just declaration -> validateConstraintArity site constraint declaration

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
    actual = length $ constraint_params constraint

validateConstraintClass :: HsConstraint -> Either ClassEnvError ()
validateConstraintClass constraint =
  validateClassName $ constraint_tclass constraint

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
  deriving (Generic)

qClassEnv_env :: QueryClassEnv -> StaticClassEnv
qClassEnv_env (QueryClassEnv environment _ _) = environment

qClassEnv_constraints :: QueryClassEnv -> S.Set HsConstraint
qClassEnv_constraints (QueryClassEnv _ constraints _) = constraints

qClassEnv_inflatedConstraints :: QueryClassEnv -> S.Set HsConstraint
qClassEnv_inflatedConstraints (QueryClassEnv _ _ constraints) = constraints

-- deepseq provides the same Generic-derived defaults that this package used
-- to obtain from deepseq-generics.  Constraints now contain only shared
-- nominal class names, so forcing a class environment walks a finite value
-- rather than a recursively tied declaration graph.
instance NFData HsType
instance NFData HsTypeClass
instance NFData HsInstance
instance NFData StaticClassEnv
instance NFData QueryClassEnv

instance Show HsType where
  -- Render the total structural projection without validating or
  -- canonicalizing it: Show is also needed while reporting rejected input.
  showsPrec precedence source = SharedRender.showsType
    defaultVariableName precedence $ toSynthesisTypeStructure source

showHsType :: TypeVarIndex -> HsType -> String
showHsType sourceNames = SharedRender.renderType
  (sourceVariableName sourceNames) . toSynthesisTypeStructure

-- instance Read HsType where
--   readsPrec _ = maybeToList . parseType

showHsConstraint :: TypeVarIndex
                 -> HsConstraint
                 -> String
showHsConstraint sourceNames = SharedRender.renderConstraint
  (sourceVariableName sourceNames) . toSynthesisConstraintStructure

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
  preferredNames = IntMap.fromListWith min
    [ (identifier, spelling)
    | (spelling, identifier) <- M.toList sourceNames
    ]
  renderVariable variable = IntMap.findWithDefault
    (defaultVariableName variable)
    (variableIdentifier variable)
    preferredNames
  variableIdentifier variable = case variable of
    SharedType.FlexibleVariable identifier -> identifier
    SharedType.RigidVariable identifier -> identifier
  

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
    csSet = S.fromList constrs `S.union` qClassEnv_constraints env
    inflated = inflateHsConstraints (qClassEnv_env env) csSet

-- | Add all transitively implied superclasses using declarations from the
-- explicit environment.  Arity is checked before constructing the
-- substitution: malformed query constraints never receive a silently
-- truncated @zip@ substitution.
inflateHsConstraints
  :: StaticClassEnv
  -> S.Set HsConstraint
  -> S.Set HsConstraint
inflateHsConstraints environment = closure (S.fromList . superclasses)
  where
    superclasses :: HsConstraint -> [HsConstraint]
    superclasses (HsConstraint className arguments) = case
      M.lookup className (sClassEnv_tclasses environment) of
        Just (HsTypeClass _ parameters constraints)
          | length parameters == length arguments ->
              let substitutions = IntMap.fromList $ zip parameters arguments
              in map (snd . constraintApplySubsts substitutions) constraints
        _ -> []

-- | Add instance heads implied by superclass declarations.  Exact arity is
-- checked before substitution, so no malformed head can be truncated by
-- @zip@ even if this helper is called independently of 'mkStaticClassEnv'.
inflateInstances :: StaticClassEnv -> [HsInstance] -> [HsInstance]
inflateInstances environment =
  S.toList . closure (S.fromList . superclasses) . S.fromList
 where
  superclasses :: HsInstance -> [HsInstance]
  superclasses (HsInstance prerequisites
      (HsConstraint className arguments)) = case
    M.lookup className (sClassEnv_tclasses environment) of
      Just (HsTypeClass _ parameters constraints)
        | length parameters == length arguments ->
            let substitutions = IntMap.fromList $ zip parameters arguments
            in map
                (HsInstance prerequisites
                  . snd
                  . constraintApplySubsts substitutions)
                constraints
      _ -> []

-- | Checked simultaneous substitution across every argument of a constraint.
-- A structural tuple coordinates the fresh-variable reservation set across
-- sibling arguments; it is removed again before the result is returned.
constraintApplySubstsChecked
  :: Substs
  -> HsConstraint
  -> Either HsSubstitutionError (Any, HsConstraint)
constraintApplySubstsChecked substitutions constraint
  | IntMap.null substitutions = Right (Any False, constraint)
  | HsConstraint className parameters <- constraint = do
      substituted <- substituteShared substitutions $ SharedType.TupleType
        SharedName.Unboxed $ map toSynthesisTypeStructure parameters
      resultParameters <- case substituted of
        SharedType.TupleType SharedName.Unboxed results ->
          mapM lowerSubstitutionResult results
        unexpected -> Left $ UnexpectedConstraintSubstitutionResult unexpected
      Right
        ( Any $ substitutionsAffect substitutions $ foldMap freeVars parameters
        , HsConstraint className resultParameters
        )

-- | Compatibility wrapper around 'constraintApplySubstsChecked'.  It throws a
-- descriptive exception only if capture avoidance would require a fresh
-- variable after every 'Int' identity has been reserved, or if an internal
-- shared/core projection invariant is broken.
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
  h (TypeForall _ _ t) = h t

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
        $ toSynthesisTypeStructure source
      result <- lowerSubstitutionResult substituted
      Right
        ( Any $ substitutionsAffect substitutions $ freeVars source
        , result
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
  -> SynthesisType
  -> Either HsSubstitutionError SynthesisType
substituteShared substitutions source = case
    SharedType.substituteTypeVariables
      freshSynthesisVariable
      S.empty
      (M.fromList
        [ ( SharedType.FlexibleVariable variable
          , toSynthesisTypeStructure replacement
          )
        | (variable, replacement) <- IntMap.toList substitutions
        ])
      source of
  Left failure -> Left $ SharedSubstitutionFailure failure
  Right result -> Right result

lowerSubstitutionResult
  :: SynthesisType
  -> Either HsSubstitutionError HsType
lowerSubstitutionResult = either
  (Left . SubstitutionResultTypeError)
  Right
  . fromSynthesisTypeStructure

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
freeVars (TypeVar i)         = S.singleton i
freeVars (TypeConstant _)    = S.empty
freeVars (TypeCons _)        = S.empty
freeVars (TypeArrow t1 t2)   = S.union (freeVars t1) (freeVars t2)
freeVars (TypeApp t1 t2)     = S.union (freeVars t1) (freeVars t2)
freeVars (TypeForall is cs t) = foldr S.delete allVars is
  where
    allVars = freeVars t `S.union` foldMap (foldMap freeVars . constraint_params) cs

-- | Collect every class constraint embedded in a type, including contexts
-- nested under arrows and applications.  Keeping this traversal in the core
-- type module lets both public-input validation and source-environment
-- validation apply their deliberately different unknown-class policies to
-- exactly the same syntax.
typeConstraints :: HsType -> [HsConstraint]
typeConstraints TypeVar{} = []
typeConstraints TypeConstant{} = []
typeConstraints TypeCons{} = []
typeConstraints (TypeArrow parameter result) =
  typeConstraints parameter ++ typeConstraints result
typeConstraints (TypeApp function argument) =
  typeConstraints function ++ typeConstraints argument
typeConstraints (TypeForall _ constraints body) =
  constraints
    ++ concatMap (concatMap typeConstraints . constraint_params) constraints
    ++ typeConstraints body
