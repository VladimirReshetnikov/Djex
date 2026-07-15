module Language.Haskell.Exference.Core.Internal.Unify
  ( unify
  , unifyDisjoint
  , unifyShared
  , unifyOffset
  , unifyRight
  , unifyRightEqs
  , unifyRightOffset
  , TypeEq (..)
  )
where

import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Language.Haskell.Exference.Core.Internal.FlexibleIds
import Language.Haskell.Exference.Core.Types
import qualified Language.Haskell.Synthesis.Type as SharedType

data TypeEq = TypeEq !HsType !HsType

-- | Backward-compatible name for 'unifyDisjoint'.
{-# INLINE unify #-}
unify :: HsType -> HsType -> Maybe (Substs, Substs)
unify = unifyDisjoint

-- | Unify types whose flexible identifiers belong to independent namespaces.
-- Returns one substitution per input. In the symmetric case, substituting the
-- right-hand (second) type will be preferred.
--
-- Examples:
--
-- @
-- unify v C = ([v => C], [])
-- unify C v = ([], [v => C])
-- unify v w = ([], [w => v])
-- @
{-# INLINE unifyDisjoint #-}
unifyDisjoint :: HsType -> HsType -> Maybe (Substs, Substs)
unifyDisjoint rawLeft rawRight = do
  left <- canonicalUnificationType rawLeft
  right <- canonicalUnificationType rawRight
  taggedLeft <- tagType LeftVariable left
  taggedRight <- tagType RightVariable right
  substitutions <- solveTagged (const True)
    [(taggedLeft, taggedRight)] Map.empty
  projectTagged left right substitutions

-- | Unify types whose flexible identifiers already belong to one shared
-- namespace. Equal identifiers on the two sides denote the same metavariable,
-- so the occurs check spans both inputs. Use 'unifyDisjoint' instead when each
-- input has an independent namespace and therefore needs its own substitution.
{-# INLINE unifyShared #-}
unifyShared :: HsType -> HsType -> Maybe Substs
unifyShared rawLeft rawRight = do
  left <- canonicalUnificationType rawLeft
  right <- canonicalUnificationType rawRight
  -- Reusing one tag constructor is intentional: it makes a variable with the
  -- same identifier literally the same solver variable on both sides.
  taggedLeft <- tagType LeftVariable left
  taggedRight <- tagType LeftVariable right
  substitutions <- solveTagged (const True)
    [(taggedLeft, taggedRight)] Map.empty
  pure $ IntMap.fromList $ mapMaybe (project substitutions) variables
 where
  variables = Set.toAscList $ Set.union
    (freeVars $ SharedType.canonicalizeType rawLeft)
    (freeVars $ SharedType.canonicalizeType rawRight)
  project substitutions variable =
    let resolved = untagTagged taggedIdentifier
          $ zonkTagged substitutions
          $ TaggedVar
          $ LeftVariable variable
    in if resolved == TypeVar variable
       then Nothing
       else Just (variable, resolved)
  -- 'RightVariable' cannot be produced by this entry point, but keeping the
  -- projection total makes that solver invariant local and explicit.
  taggedIdentifier tagged = case tagged of
    LeftVariable variable -> variable
    RightVariable variable -> variable

{-# INLINE unifyOffset #-}
unifyOffset :: HsType -> HsTypeOffset -> Maybe (Substs, Substs)
unifyOffset left (HsTypeOffset right offset) =
  checkedOffsetType offset right >>= unifyDisjoint left

-- Symmetric and right-directed unification both need tagged variables. A
-- numeric ID on the left never aliases the same spelling on the right until
-- projection: symmetric search may bind either tag, while right-directed
-- matching treats every left tag as a rigid pattern variable.
data TaggedVariable
  = LeftVariable !TVarId
  | RightVariable !TVarId
  deriving (Eq, Ord)

-- Structural functions and constructor-backed tuples deliberately use the
-- same applicative kernel as their intrinsic constructors. Besides making the
-- two shared source spellings equivalent, this preserves higher-kinded
-- unification: a flexible application head can still be bound to @(->)@ or an
-- n-tuple constructor. A unary unboxed tuple has no corresponding constructor
-- name, so that one valid structural form retains an explicit tagged node.
-- 'untagTagged' restores the canonical structural form in either case.
data TaggedType
  = TaggedVar !TaggedVariable
  | TaggedConstant !TVarId
  | TaggedConstructor !QualifiedName
  | TaggedApplication !TaggedType !TaggedType
  | TaggedTuple !Boxity ![TaggedType]
  deriving (Eq)

type TaggedSubstitutions = Map.Map TaggedVariable TaggedType

-- | Canonicalize and structurally validate every public unifier input.
-- Quantifiers are rejected separately by 'tagType', including a forall nested
-- in a tuple or application and a native forall with rigid binders.
canonicalUnificationType :: HsType -> Maybe HsType
canonicalUnificationType source = case SharedType.validateType canonical of
  Left _ -> Nothing
  Right () -> Just canonical
 where
  canonical = SharedType.canonicalizeType source

tagType :: (TVarId -> TaggedVariable) -> HsType -> Maybe TaggedType
tagType side = tag . SharedType.constructorApplicationForm
 where
  tag ty = case ty of
    TypeVar variable -> Just $ TaggedVar $ side variable
    TypeConstant constant -> Just $ TaggedConstant constant
    TypeCons constructor -> Just $ TaggedConstructor constructor
    TypeArrow{} -> Nothing
    TypeApp function argument ->
      TaggedApplication <$> tag function <*> tag argument
    -- The shared applicative view leaves unary unboxed tuples structural
    -- because Haskell has no corresponding unary constructor.
    TypeTuple boxity elements -> TaggedTuple boxity <$> mapM tag elements
    -- Higher-rank subsumption requires skolemization and escape checks.
    -- Conservatively reject every native forall instead of erasing binders or
    -- accidentally accepting the match-only legacy 'TypeForall' view.
    TypeForallNative{} -> Nothing

solveTagged
  :: (TaggedVariable -> Bool)
  -> [(TaggedType, TaggedType)]
  -> TaggedSubstitutions
  -> Maybe TaggedSubstitutions
solveTagged _ [] substitutions = Just substitutions
solveTagged bindable ((rawLeft, rawRight) : equations) substitutions
  | left == right = solveTagged bindable equations substitutions
  | TaggedVar variable <- right
  , bindable variable = bindTagged variable left
  | TaggedVar variable <- left
  , bindable variable = bindTagged variable right
  | TaggedConstant leftConstant <- left
  , TaggedConstant rightConstant <- right
  , leftConstant == rightConstant =
      solveTagged bindable equations substitutions
  | TaggedConstructor leftConstructor <- left
  , TaggedConstructor rightConstructor <- right
  , leftConstructor == rightConstructor =
      solveTagged bindable equations substitutions
  | TaggedApplication leftFunction leftArgument <- left
  , TaggedApplication rightFunction rightArgument <- right =
      solveTagged bindable
        ((leftFunction, rightFunction) : (leftArgument, rightArgument) : equations)
        substitutions
  | TaggedTuple leftBoxity leftElements <- left
  , TaggedTuple rightBoxity rightElements <- right
  , leftBoxity == rightBoxity
  , length leftElements == length rightElements =
      solveTagged bindable
        (zip leftElements rightElements ++ equations)
        substitutions
  | otherwise = Nothing
 where
  left = zonkTagged substitutions rawLeft
  right = zonkTagged substitutions rawRight
  bindTagged variable replacement
    | occursTagged variable replacement = Nothing
    | otherwise = solveTagged bindable
        [ ( substituteTagged variable replacement equationLeft
          , substituteTagged variable replacement equationRight
          )
        | (equationLeft, equationRight) <- equations
        ]
        $ Map.insert variable replacement
        $ Map.map (substituteTagged variable replacement) substitutions

occursTagged :: TaggedVariable -> TaggedType -> Bool
occursTagged variable ty = case ty of
  TaggedVar candidate -> variable == candidate
  TaggedConstant{} -> False
  TaggedConstructor{} -> False
  TaggedApplication function argument ->
    occursTagged variable function || occursTagged variable argument
  TaggedTuple _ elements -> any (occursTagged variable) elements

substituteTagged :: TaggedVariable -> TaggedType -> TaggedType -> TaggedType
substituteTagged variable replacement ty = case ty of
  TaggedVar candidate
    | variable == candidate -> replacement
    | otherwise -> ty
  TaggedConstant{} -> ty
  TaggedConstructor{} -> ty
  TaggedApplication function argument -> TaggedApplication
    (substituteTagged variable replacement function)
    (substituteTagged variable replacement argument)
  TaggedTuple boxity elements -> TaggedTuple boxity
    $ map (substituteTagged variable replacement) elements

zonkTagged :: TaggedSubstitutions -> TaggedType -> TaggedType
zonkTagged substitutions ty = case ty of
  TaggedVar variable -> maybe ty (zonkTagged substitutions)
    $ Map.lookup variable substitutions
  TaggedConstant{} -> ty
  TaggedConstructor{} -> ty
  TaggedApplication function argument -> TaggedApplication
    (zonkTagged substitutions function)
    (zonkTagged substitutions argument)
  TaggedTuple boxity elements -> TaggedTuple boxity
    $ map (zonkTagged substitutions) elements

projectTagged
  :: HsType
  -> HsType
  -> TaggedSubstitutions
  -> Maybe (Substs, Substs)
projectTagged left right substitutions = do
  rightCanonical <- allocateRightVariables
    (Set.fromList leftVariables) rightVariables
  let
      canonicalRight variable = IntMap.findWithDefault variable variable
        rightCanonical
      project side variable =
        let resolved = untagTagged externalIdentifier
              $ zonkTagged substitutions
              $ TaggedVar $ side variable
        in if resolved == TypeVar variable
           then Nothing
           else Just (variable, resolved)
      externalIdentifier tagged = case tagged of
        LeftVariable variable -> variable
        RightVariable variable -> canonicalRight variable
  pure
    ( IntMap.fromList $ mapMaybe (project LeftVariable) leftVariables
    , IntMap.fromList $ mapMaybe (project RightVariable) rightVariables
    )
 where
  leftVariables = Set.toAscList $ freeVars left
  rightVariables = Set.toAscList $ freeVars right

untagTagged :: (TaggedVariable -> TVarId) -> TaggedType -> HsType
untagTagged variableIdentifier = SharedType.canonicalizeType . convert
 where
  convert ty = case ty of
    TaggedVar variable -> TypeVar $ variableIdentifier variable
    TaggedConstant constant -> TypeConstant constant
    TaggedConstructor constructor -> TypeCons constructor
    TaggedApplication function argument -> TypeApp
      (convert function) (convert argument)
    TaggedTuple boxity elements -> TypeTuple boxity $ map convert elements

allocateRightVariables
  :: Set.Set TVarId
  -> [TVarId]
  -> Maybe (IntMap.IntMap TVarId)
allocateRightVariables leftVariables rightVariables = fst
  <$> allocateCanonicalIdentifiers rightVariables
        (supplyFromIdentifiers leftVariables)

-- | Treat flexible variables in the first parameter as rigid pattern
-- identities and return bindings only for variables in the second parameter.
{-# INLINE unifyRight #-}
unifyRight :: HsType -> HsType -> Maybe Substs
unifyRight left right = unifyRightEqs [TypeEq left right]

{-# INLINE unifyRightEqs #-}
unifyRightEqs :: [TypeEq] -> Maybe Substs
unifyRightEqs rawEquations = do
  equations <- mapM canonicalEquation rawEquations
  taggedEquations <- mapM tagEquation equations
  substitutions <- solveTagged isRightVariable taggedEquations Map.empty
  pure $ projectRightSubstitutions equations substitutions
 where
  canonicalEquation (TypeEq left right) = TypeEq
    <$> canonicalUnificationType left
    <*> canonicalUnificationType right
  tagEquation (TypeEq left right) = (,)
    <$> tagType LeftVariable left
    <*> tagType RightVariable right
  isRightVariable variable = case variable of
    LeftVariable{} -> False
    RightVariable{} -> True

projectRightSubstitutions
  :: [TypeEq]
  -> TaggedSubstitutions
  -> Substs
projectRightSubstitutions equations substitutions = IntMap.fromList
  $ mapMaybe project rightVariables
 where
  rightVariables = Set.toAscList $ foldMap
    (\(TypeEq _ right) -> freeVars right) equations
  project variable =
    let resolved = untagTagged externalIdentifier
          $ zonkTagged substitutions
          $ TaggedVar $ RightVariable variable
    in if resolved == TypeVar variable
       then Nothing
       else Just (variable, resolved)
  externalIdentifier tagged = case tagged of
    LeftVariable variable -> variable
    RightVariable variable -> variable

{-# INLINE unifyRightOffset #-}
unifyRightOffset :: HsType -> HsTypeOffset -> Maybe Substs
unifyRightOffset left (HsTypeOffset right offset) =
  checkedOffsetType offset right >>= unifyRight left

checkedOffsetType :: TVarId -> HsType -> Maybe HsType
checkedOffsetType offset typeExpression = do
  pairs <- mapM shifted $ Set.toAscList $ freeVars typeExpression
  pure $ renameFlexibleType (IntMap.fromList pairs) typeExpression
 where
  shifted identifier = do
    result <- checkedAddIdentifier identifier offset
    pure (identifier, result)
