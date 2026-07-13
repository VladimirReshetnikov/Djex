{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE PatternGuards #-}

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
  , HsTypeClass (..)
  , HsInstance (..)
  , HsConstraint (HsConstraint)
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
  , inflateHsConstraints
  , applySubst
  , applySubsts
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
import Data.Maybe ( fromMaybe )
import Data.Monoid ( Any(..) )
import Control.Monad ( liftM2 )

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as L

import Language.Haskell.Exference.Core.Internal.Closure ( closure )
import Language.Haskell.Exference.Core.Name
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
  | UnsupportedSynthesisName QualifiedNameError
  | RigidForallBinder TVarId
  deriving (Eq, Show, Generic)

instance NFData SynthesisTypeError

data Subst  = Subst {-# UNPACK #-} !TVarId !HsType
type Substs = IntMap.IntMap HsType

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
  TypeCons name -> SharedType.TypeConstructor $ toSynthesisName name
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
  convert canonical
 where
  convert typeExpression = case typeExpression of
    SharedType.TypeVariable (SharedType.FlexibleVariable variable) ->
      Right $ TypeVar variable
    SharedType.TypeVariable (SharedType.RigidVariable variable) ->
      Right $ TypeConstant variable
    SharedType.TypeConstructor name ->
      TypeCons <$> checkedName name
    SharedType.TypeApplication function argument -> TypeApp
      <$> convert function <*> convert argument
    SharedType.FunctionType parameter result -> TypeArrow
      <$> convert parameter <*> convert result
    SharedType.TupleType boxity elements -> do
      tuple <- either
        (Left . UnsupportedSynthesisName . InvalidQualifiedName)
        Right
        $ SharedName.tupleName boxity $ length elements
      constructor <- checkedName tuple
      foldl TypeApp (TypeCons constructor) <$> mapM convert elements
    SharedType.ForallType variables constraints body -> do
      binders <- mapM flexibleBinder variables
      TypeForall binders
        <$> mapM convertConstraint constraints
        <*> convert body

  checkedName = either (Left . UnsupportedSynthesisName) Right
    . fromSynthesisName

  flexibleBinder (SharedType.FlexibleVariable variable) = Right variable
  flexibleBinder (SharedType.RigidVariable variable) =
    Left $ RigidForallBinder variable

  convertConstraint (SharedConstraint.Constraint className arguments) = do
    convertedArguments <- mapM convert arguments
    either (Left . UnsupportedSynthesisName) Right
      $ fromSynthesisConstraintRepresentation
      $ SharedConstraint.Constraint className convertedArguments

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

-- | A finite nominal class constraint. Class declarations live exclusively in
-- 'StaticClassEnv'; storing the already narrowed name directly makes ordinary
-- access independent of the shared conversion boundary.
data HsConstraint = HsConstraint !QualifiedName [HsType]
  deriving (Eq, Ord)

-- 'QualifiedName' guarantees general lexical validity; the
-- checked type and environment boundaries additionally enforce that the name
-- occupies the class namespace.
constraint_tclass :: HsConstraint -> QualifiedName
constraint_tclass (HsConstraint className _) = className

constraint_params :: HsConstraint -> [HsType]
constraint_params (HsConstraint _ arguments) = arguments

-- | A nominal shared view of the direct Exference representation. Its type
-- arguments remain in Exference's internal vocabulary, so this is a private
-- implementation detail rather than the public shared conversion.
constraintRepresentation :: HsConstraint -> SharedConstraint.Constraint HsType
constraintRepresentation (HsConstraint className arguments) =
  SharedConstraint.Constraint (toSynthesisName className) arguments

-- | Project the whole constraint, including every type argument, into shared
-- syntax without validation.  The checked engine uses this only after input
-- validation has established the invariants preserved by search.
toSynthesisConstraintStructure
  :: HsConstraint
  -> SharedConstraint.Constraint SynthesisType
toSynthesisConstraintStructure =
  fmap toSynthesisTypeStructure . constraintRepresentation

-- | Convert a constraint completely to shared syntax and validate both its
-- nominal class identity and all type arguments.
toSynthesisConstraint
  :: HsConstraint
  -> Either SynthesisTypeError
       (SharedConstraint.Constraint SynthesisType)
toSynthesisConstraint constraint = do
  converted <- traverse toSynthesisType $ constraintRepresentation constraint
  either (Left . InvalidSynthesisConstraint) Right
    $ SharedConstraint.validateConstraint converted
  return converted

-- | Narrow a fully shared constraint back to Exference.  In particular,
-- unboxed tuple constructor names are rejected rather than smuggled through
-- an opaque wrapper.
fromSynthesisConstraint
  :: SharedConstraint.Constraint SynthesisType
  -> Either SynthesisTypeError HsConstraint
fromSynthesisConstraint constraint = do
  either (Left . InvalidSynthesisConstraint) Right
    $ SharedConstraint.validateConstraint constraint
  converted <- traverse fromSynthesisType constraint
  either (Left . UnsupportedSynthesisName) Right
    $ fromSynthesisConstraintRepresentation converted

fromSynthesisConstraintRepresentation
  :: SharedConstraint.Constraint HsType
  -> Either QualifiedNameError HsConstraint
fromSynthesisConstraintRepresentation
    (SharedConstraint.Constraint className arguments) = do
  exferenceName <- fromSynthesisName className
  return $ HsConstraint exferenceName arguments

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
  case repeatedValues $ map instance_head instances of
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
        Nothing -> case firstDuplicate parameters of
          Just duplicate -> Left $ DuplicateClassParameter name duplicate
          Nothing -> Right ()
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
    (toSynthesisName name) of
  Left _ -> Left $ InvalidClassName name
  Right () -> Right ()

firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate = go S.empty
 where
  go _ [] = Nothing
  go seen (value : rest)
    | value `S.member` seen = Just value
    | otherwise = go (S.insert value seen) rest

-- | Return every repeated value once, in the order its first repetition is
-- encountered.  Reporting the complete set lets environment maintainers fix
-- independent duplicate instance declarations in one pass.
repeatedValues :: Ord a => [a] -> [a]
repeatedValues = go S.empty S.empty
 where
  go _ _ [] = []
  go seen reported (value : rest)
    | value `S.member` reported = go seen reported rest
    | value `S.member` seen = value : go seen (S.insert value reported) rest
    | otherwise = go (S.insert value seen) reported rest

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
instance NFData HsConstraint where
  rnf = rnf . constraintRepresentation
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

instance Show HsConstraint where
  showsPrec precedence source = SharedRender.showsConstraint
    defaultVariableName precedence $ toSynthesisConstraintStructure source

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

constraintApplySubst :: Subst -> HsConstraint -> HsConstraint
constraintApplySubst s (HsConstraint c ps) =
  HsConstraint c $ map (applySubst s) ps

-- returns if any change was necessary,
-- plus the (potentially changed) constraint
-- constraintApplySubst' :: Subst -> HsConstraint -> (Bool, HsConstraint)
-- constraintApplySubst' s (HsConstraint c ps) =
--   let applied = map (applySubst' s) ps
--   in (any fst applied, HsConstraint c $ snd <$> applied)

-- returns if any change was necessary,
-- plus the (potentially changed) constraint
{-# INLINE constraintApplySubsts #-}
constraintApplySubsts :: Substs -> HsConstraint -> (Any, HsConstraint)
constraintApplySubsts ss c
  | IntMap.null ss = return c
  | HsConstraint cl ps <- c =
    HsConstraint cl <$> mapM (applySubsts ss) ps

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

applySubst :: Subst -> HsType -> HsType
applySubst (Subst i t) v@(TypeVar j) = if i==j then t else v
applySubst _ c@(TypeConstant _) = c
applySubst _ c@(TypeCons _)     = c
applySubst s (TypeArrow t1 t2)  = TypeArrow (applySubst s t1) (applySubst s t2)
applySubst s (TypeApp t1 t2)    = TypeApp (applySubst s t1) (applySubst s t2)
applySubst s@(Subst i _) f@(TypeForall js cs t) = if i `elem` js
  then f
  else TypeForall js (constraintApplySubst s <$> cs) (applySubst s t)

applySubsts :: Substs -> HsType -> (Any, HsType)
applySubsts s v@(TypeVar i)      = fromMaybe (return v)
                                  $ (,) (Any True) <$> IntMap.lookup i s
applySubsts _ c@(TypeConstant _) = return c
applySubsts _ c@(TypeCons _)     = return c
applySubsts s (TypeArrow t1 t2)  = liftM2 TypeArrow (applySubsts s t1) (applySubsts s t2)
applySubsts s (TypeApp t1 t2)    = liftM2 TypeApp   (applySubsts s t1) (applySubsts s t2)
applySubsts s (TypeForall js cs t) = liftM2 (TypeForall js)
  (sequence $ constraintApplySubsts unbound <$> cs)
  (applySubsts unbound t)
  where
    -- Bound variables are protected in both the context and the body.
    unbound = foldr IntMap.delete s js

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
