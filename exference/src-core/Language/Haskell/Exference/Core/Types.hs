{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.Exference.Core.Types
  ( TVarId
  , module Language.Haskell.Exference.Core.Name
  , HsType (..)
  , HsTypeOffset (..)
  , SynthesisVariable
  , SynthesisTypeError (..)
  , toSynthesisType
  , fromSynthesisType
  , Subst (..)
  , Substs
  , HsTypeClass (..)
  , HsInstance (..)
  , HsConstraint (HsConstraint)
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
  , inflateHsConstraints
  , applySubst
  , applySubsts
  -- , typeParser
  , containsVar
  , showVar
  , preferredVarName
  , showTypedVar
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
import Data.List ( intercalate )
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

import Control.DeepSeq
import GHC.Generics
import Control.Monad.Trans.MultiState




type TVarId = Int
type SynthesisVariable = SharedType.Variable TVarId

data SynthesisTypeError
  = InvalidSynthesisType (SharedType.TypeError SynthesisVariable)
  | UnsupportedSynthesisName QualifiedNameError
  | RigidForallBinder TVarId
  deriving (Eq, Show)

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

-- | Erase Exference's search representation into the common source type.
-- Flexible and rigid IDs remain distinct, and saturated tuple constructors
-- are canonicalized structurally.
toSynthesisType
  :: HsType
  -> Either SynthesisTypeError (SharedType.Type SynthesisVariable)
toSynthesisType source = do
  converted <- convert source
  let canonical = SharedType.canonicalizeType converted
  either (Left . InvalidSynthesisType) Right
    $ SharedType.validateType canonical
  return canonical
 where
  convert typeExpression = case typeExpression of
    TypeVar variable -> Right $ SharedType.TypeVariable
      $ SharedType.FlexibleVariable variable
    TypeConstant variable -> Right $ SharedType.TypeVariable
      $ SharedType.RigidVariable variable
    TypeCons name -> Right $ SharedType.TypeConstructor
      $ toSynthesisName name
    TypeArrow parameter result -> SharedType.FunctionType
      <$> convert parameter <*> convert result
    TypeApp function argument -> SharedType.TypeApplication
      <$> convert function <*> convert argument
    TypeForall variables constraints body -> SharedType.ForallType
      (map SharedType.FlexibleVariable variables)
      <$> mapM convertConstraint constraints
      <*> convert body

  convertConstraint (HsConstraint className arguments) =
    SharedConstraint.Constraint (toSynthesisName className)
      <$> mapM convert arguments

-- | Narrow a common type back to Exference's typed search vocabulary.
fromSynthesisType
  :: SharedType.Type SynthesisVariable
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
      $ fromSynthesisConstraint
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

-- | Exference's compatibility view of the backend-independent constraint
-- syntax.  Class declarations live exclusively in 'StaticClassEnv'; keeping
-- only the nominal class name here makes declaration, superclass, and
-- instance graphs ordinary finite values.
newtype HsConstraint = HsConstraint_
  (SharedConstraint.Constraint HsType)
  deriving (Eq, Ord)

-- | Historical constructor view, now nominal rather than embedding the class
-- declaration.  'QualifiedName' guarantees general lexical validity; the
-- checked type and environment boundaries additionally enforce that the name
-- occupies the class namespace.
pattern HsConstraint :: QualifiedName -> [HsType] -> HsConstraint
pattern HsConstraint className arguments <-
  (constraintView -> (className, arguments))
  where
    HsConstraint className arguments = HsConstraint_
      $ SharedConstraint.Constraint (toSynthesisName className) arguments

{-# COMPLETE HsConstraint #-}

constraint_tclass :: HsConstraint -> QualifiedName
constraint_tclass = fst . constraintView

constraint_params :: HsConstraint -> [HsType]
constraint_params = snd . constraintView

-- | Forget the Exference compatibility wrapper.  This direction is total:
-- Exference's accepted names form a subset of the shared name domain.
toSynthesisConstraint
  :: HsConstraint
  -> SharedConstraint.Constraint HsType
toSynthesisConstraint (HsConstraint_ constraint) = constraint

-- | Narrow a shared constraint to Exference's name subset.  In particular,
-- unboxed tuple constructor names are rejected rather than smuggled through
-- an opaque wrapper.
fromSynthesisConstraint
  :: SharedConstraint.Constraint HsType
  -> Either QualifiedNameError HsConstraint
fromSynthesisConstraint (SharedConstraint.Constraint className arguments) = do
  exferenceName <- fromSynthesisName className
  return $ HsConstraint exferenceName arguments

constraintView :: HsConstraint -> (QualifiedName, [HsType])
constraintView (HsConstraint_ (SharedConstraint.Constraint className arguments)) =
  case fromSynthesisName className of
    Right exferenceName -> (exferenceName, arguments)
    -- The private representation can only be populated through the checked
    -- conversion above or from an already validated QualifiedName.
    Left _ -> error "invalid shared name in Exference HsConstraint"

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
  rnf = rnf . toSynthesisConstraint
instance NFData StaticClassEnv
instance NFData QueryClassEnv

instance Show HsType where
  showsPrec _ (TypeVar i) = showString $ showVar i
  showsPrec _ (TypeConstant i) = showString $ "C" ++ showVar i
  showsPrec d (TypeCons s) = showsPrec d s
  showsPrec d (TypeArrow t1 t2) =
    showParen (d> -2) $ showsPrec (-1) t1 . showString " -> " . showsPrec (-1) t2
  showsPrec d (TypeApp t1 t2) =
    showParen (d> -1) $ showsPrec 0 t1 . showString " " . showsPrec 0 t2
  showsPrec d (TypeForall [] [] t) = showsPrec d t
  showsPrec d (TypeForall is cs t) =
    showParen (d>0)
    $ showString quantifier
    . showString context
    . showsPrec (-2) t
    where
      quantifier
        | null is = ""
        | otherwise = "forall " ++ intercalate ", " (showVar <$> is) ++ " . "
      context
        | null cs = ""
        | otherwise = "(" ++ intercalate ", " (map show cs) ++ ") => "

showHsType :: TypeVarIndex -> HsType -> String
showHsType convMap t = h 0 t ""
 where
  variableName i = fromMaybe (showVar i)
    $ fst <$> L.find ((i ==) . snd) (M.toList convMap)
  constantName i = fromMaybe ("C" ++ showVar i)
    $ fst <$> L.find ((i ==) . snd) (M.toList convMap)
  h :: Int -> HsType -> ShowS
  h _ (TypeVar i) = showString $ variableName i
  h _ (TypeConstant i) = showString $ constantName i
  h _ (TypeCons s) = shows s
  h d (TypeArrow t1 t2) =
    showParen (d> -2) $ t1Shows . showString " -> " . t2Shows
    where
      t1Shows = h (-1) t1
      t2Shows = h (-1) t2
  h d (TypeApp t1 t2) =
    showParen (d> -1) $ t1Shows . showString " " . t2Shows
    where
      t1Shows = h 0 t1
      t2Shows = h 0 t2
  h d (TypeForall [] [] ty) = h d ty
  h d (TypeForall is cs ty) =
    showParen (d>0)
      $ showString quantifier
      . showString context
      . tShows
    where
      quantifier
        | null is = ""
        | otherwise = "forall " ++ intercalate ", " (map variableName is) ++ " . "
      context
        | null cs = ""
        | otherwise = "(" ++ intercalate ", "
            (map (showHsConstraint convMap) cs) ++ ") => "
      tShows = h (-2) ty

-- instance Read HsType where
--   readsPrec _ = maybeToList . parseType

instance Show HsConstraint where
  showsPrec precedence =
    showsPrec precedence . toSynthesisConstraint

showHsConstraint :: TypeVarIndex
                 -> HsConstraint
                 -> String
showHsConstraint convMap (HsConstraint c ps) =
  unwords $ show c : tyStrs
 where
  tyStrs = showHsType convMap <$> ps
  

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
showVar i | i<27      = [chr (ord 'a' + i - 1)]
          | otherwise = "t"++show (i-27)

-- | Suggest a readable binder spelling from its type.  This is only a
-- preference: a renderer must still allocate a fresh name because distinct
-- variable IDs can have the same suggestion and globals may use it too.
preferredVarName :: TVarId -> HsType -> String
preferredVarName i = h
 where
  h TypeVar{}          = showVar i
  h TypeConstant{}     = showVar i
  h (TypeCons qName) = case qualifiedNameOccurrence qName of
    SharedName.IdentifierOccurrence _ (c : _) -> toLower c : show i
    SharedName.IdentifierOccurrence _ [] -> showVar i
    -- A symbolic type constructor has no identifier stem from which to form a
    -- legal term binder.  Falling back avoids historical names such as @:1@.
    SharedName.OperatorOccurrence _ _ -> showVar i
    SharedName.SpecialOccurrence SharedName.ListConstructor -> showVar i ++ "s"
    SharedName.SpecialOccurrence _ -> showVar i
  h TypeArrow{}        = "f" ++ show i
  h (TypeApp t _)      = h t
  h (TypeForall _ _ t) = h t

showTypedVar :: forall m
              . ( MonadMultiState (M.Map TVarId HsType) m )
             => TVarId
             -> m String
showTypedVar i = do
  m <- mGet
  -- The ID-only fallback keeps this compatibility helper total when a caller
  -- did not run the optional 'collectVarTypes' pass first.
  return $ maybe (showVar i) (preferredVarName i) $ M.lookup i m

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
