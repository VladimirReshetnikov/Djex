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



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.Internal.FlexibleIds
import Data.Maybe (mapMaybe)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set



data TypeEq = TypeEq !HsType
                     !HsType

data UniState1 = UniState1 [TypeEq] Substs

occursIn :: TVarId -> HsType -> Bool
occursIn i (TypeVar j)         = i==j
occursIn _ (TypeConstant _)    = False
occursIn _ (TypeCons _)        = False
occursIn i (TypeArrow t1 t2)   = occursIn i t1 || occursIn i t2
occursIn i (TypeApp t1 t2)     = occursIn i t1 || occursIn i t2
occursIn i (TypeForall js _ t) = (i `notElem` js) && occursIn i t

-- | Backward-compatible name for 'unifyDisjoint'.
{-# INLINE unify #-}
unify :: HsType -> HsType -> Maybe (Substs, Substs)
unify = unifyDisjoint

-- | Unify types whose flexible identifiers belong to independent namespaces.
-- Returns one substitution per input. In the symmetric case, substituting the
-- right-hand (second) type will be preferred.
-- examples:
-- unify v C -> ([v=>C], [])
-- unify C v -> ([], [v=>C])
-- unify v w -> ([], [w=>v])
{-# INLINE unifyDisjoint #-}
unifyDisjoint :: HsType -> HsType -> Maybe (Substs, Substs)
unifyDisjoint left right = unifyTagged left right id


-- | Unify types whose flexible identifiers already belong to one shared
-- namespace.  Equal identifiers on the two sides denote the same metavariable,
-- so the occurs check spans both inputs.  Use 'unifyDisjoint' instead when each
-- input has an independent namespace and therefore needs its own substitution.
{-# INLINE unifyShared #-}
unifyShared :: HsType -> HsType -> Maybe Substs
unifyShared left right = do
  -- Reusing one tag constructor is intentional: it makes a variable with the
  -- same identifier literally the same solver variable on both sides.
  taggedLeft <- tagType LeftVariable left
  taggedRight <- tagType LeftVariable right
  substitutions <- solveTagged [(taggedLeft, taggedRight)] Map.empty
  pure $ IntMap.fromList $ mapMaybe (project substitutions) variables
 where
  variables = Set.toAscList $ Set.union (freeVars left) (freeVars right)
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


{-# INLINE unifyOffset #-}                   -- left, rightOffset
unifyOffset :: HsType -> HsTypeOffset -> Maybe (Substs, Substs)
unifyOffset left (HsTypeOffset right offset) =
  checkedOffsetType offset right >>= unifyDisjoint left

-- Symmetric unification needs tagged variables internally.  The old pair of
-- untagged substitution maps lost which namespace a range belonged to; a
-- later left substitution therefore failed to close an earlier right range
-- (and vice versa), while equal numeric IDs could be rewritten on the wrong
-- side.  Projection below chooses a common integer spelling only after the
-- tagged substitution is fully zonked.
data TaggedVariable
  = LeftVariable !TVarId
  | RightVariable !TVarId
  deriving (Eq, Ord)

data TaggedType
  = TaggedVar !TaggedVariable
  | TaggedConstant !TVarId
  | TaggedConstructor !QualifiedName
  | TaggedArrow !TaggedType !TaggedType
  | TaggedApplication !TaggedType !TaggedType
  deriving (Eq)

type TaggedSubstitutions = Map.Map TaggedVariable TaggedType

unifyTagged
  :: HsType
  -> HsType
  -> (TVarId -> TVarId)
  -> Maybe (Substs, Substs)
unifyTagged left right rightKey = do
  taggedLeft <- tagType LeftVariable left
  taggedRight <- tagType RightVariable right
  substitutions <- solveTagged [(taggedLeft, taggedRight)] Map.empty
  projectTagged left right rightKey substitutions

tagType :: (TVarId -> TaggedVariable) -> HsType -> Maybe TaggedType
tagType side ty = case ty of
  TypeVar variable -> Just $ TaggedVar $ side variable
  TypeConstant constant -> Just $ TaggedConstant constant
  TypeCons constructor -> Just $ TaggedConstructor constructor
  TypeArrow parameter result ->
    TaggedArrow <$> tagType side parameter <*> tagType side result
  TypeApp function argument ->
    TaggedApplication <$> tagType side function <*> tagType side argument
  -- Higher-rank subsumption requires skolemization and escape checks.
  -- Conservatively reject nested foralls instead of erasing the quantifier.
  TypeForall{} -> Nothing

solveTagged
  :: [(TaggedType, TaggedType)]
  -> TaggedSubstitutions
  -> Maybe TaggedSubstitutions
solveTagged [] substitutions = Just substitutions
solveTagged ((rawLeft, rawRight) : equations) substitutions
  | left == right = solveTagged equations substitutions
  | TaggedVar variable <- right = bindTagged variable left
  | TaggedVar variable <- left = bindTagged variable right
  | TaggedConstant leftConstant <- left
  , TaggedConstant rightConstant <- right
  , leftConstant == rightConstant = solveTagged equations substitutions
  | TaggedConstructor leftConstructor <- left
  , TaggedConstructor rightConstructor <- right
  , leftConstructor == rightConstructor = solveTagged equations substitutions
  | TaggedArrow leftParameter leftResult <- left
  , TaggedArrow rightParameter rightResult <- right =
      solveTagged
        ((leftParameter, rightParameter) : (leftResult, rightResult) : equations)
        substitutions
  | TaggedApplication leftFunction leftArgument <- left
  , TaggedApplication rightFunction rightArgument <- right =
      solveTagged
        ((leftFunction, rightFunction) : (leftArgument, rightArgument) : equations)
        substitutions
  | otherwise = Nothing
 where
  left = zonkTagged substitutions rawLeft
  right = zonkTagged substitutions rawRight
  bindTagged variable replacement
    | occursTagged variable replacement = Nothing
    | otherwise = solveTagged
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
  TaggedArrow parameter result ->
    occursTagged variable parameter || occursTagged variable result
  TaggedApplication function argument ->
    occursTagged variable function || occursTagged variable argument

substituteTagged :: TaggedVariable -> TaggedType -> TaggedType -> TaggedType
substituteTagged variable replacement ty = case ty of
  TaggedVar candidate
    | variable == candidate -> replacement
    | otherwise -> ty
  TaggedConstant{} -> ty
  TaggedConstructor{} -> ty
  TaggedArrow parameter result -> TaggedArrow
    (substituteTagged variable replacement parameter)
    (substituteTagged variable replacement result)
  TaggedApplication function argument -> TaggedApplication
    (substituteTagged variable replacement function)
    (substituteTagged variable replacement argument)

zonkTagged :: TaggedSubstitutions -> TaggedType -> TaggedType
zonkTagged substitutions ty = case ty of
  TaggedVar variable -> maybe ty (zonkTagged substitutions)
    $ Map.lookup variable substitutions
  TaggedConstant{} -> ty
  TaggedConstructor{} -> ty
  TaggedArrow parameter result ->
    TaggedArrow (zonkTagged substitutions parameter) (zonkTagged substitutions result)
  TaggedApplication function argument -> TaggedApplication
    (zonkTagged substitutions function) (zonkTagged substitutions argument)

projectTagged
  :: HsType
  -> HsType
  -> (TVarId -> TVarId)
  -> TaggedSubstitutions
  -> Maybe (Substs, Substs)
projectTagged left right rightKey substitutions = do
  rightCanonical <- allocateRightVariables
    (Set.fromList leftVariables) (map rightKey rightVariables)
  let
      canonicalRight variable = IntMap.findWithDefault (rightKey variable)
        (rightKey variable) rightCanonical
      project side key variable =
        let externalKey = key variable
            resolved = untagTagged externalIdentifier
              $ zonkTagged substitutions
              $ TaggedVar $ side variable
        in if resolved == TypeVar externalKey
           then Nothing
           else Just (externalKey, resolved)
      externalIdentifier tagged = case tagged of
        LeftVariable variable -> variable
        RightVariable variable -> canonicalRight variable
  pure
    ( IntMap.fromList $ mapMaybe (project LeftVariable id) leftVariables
    , IntMap.fromList $ mapMaybe (project RightVariable rightKey) rightVariables
    )
 where
  leftVariables = Set.toAscList $ freeVars left
  rightVariables = Set.toAscList $ freeVars right

untagTagged :: (TaggedVariable -> TVarId) -> TaggedType -> HsType
untagTagged variableIdentifier ty = case ty of
  TaggedVar variable -> TypeVar $ variableIdentifier variable
  TaggedConstant constant -> TypeConstant constant
  TaggedConstructor constructor -> TypeCons constructor
  TaggedArrow parameter result -> TypeArrow
    (untagTagged variableIdentifier parameter)
    (untagTagged variableIdentifier result)
  TaggedApplication function argument -> TypeApp
    (untagTagged variableIdentifier function)
    (untagTagged variableIdentifier argument)

allocateRightVariables
  :: Set.Set TVarId
  -> [TVarId]
  -> Maybe (IntMap.IntMap TVarId)
allocateRightVariables leftVariables rightVariables = fst
  <$> allocateCanonicalIdentifiers rightVariables
        (supplyFromIdentifiers leftVariables)

-- treats the variables in the first parameter as constants, and returns
-- the variable bindings for the second parameter that unify both types.
{-# INLINE unifyRight #-}
unifyRight :: HsType -> HsType -> Maybe Substs
unifyRight ut1 ut2 = unifyRightEqs [TypeEq ut1 ut2]

{-# INLINE unifyRightEqs #-}
unifyRightEqs :: [TypeEq] -> Maybe Substs
unifyRightEqs teqs = unify' $ UniState1 teqs IntMap.empty
  where
    unify' :: UniState1 -> Maybe Substs
    unify' (UniState1 [] x) = Just x
    unify' (UniState1 (x:xr) ss) = uniStepRight x >>= (
      \r -> unify' $ case r of
        Left subst@(Subst i t) -> let f = applySubst subst in UniState1
          [ TypeEq a (f b) | TypeEq a b <- xr]
          (IntMap.insert i t $ IntMap.map f ss)
        Right eqs -> UniState1 (eqs++xr) ss
      )


-- treats the variables in the first parameter as constants, and returns
-- the variable bindings for the second parameter that unify both types.
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


uniStepRight :: TypeEq -> Maybe (Either Subst [TypeEq])
uniStepRight (TypeEq TypeForall{} _) = Nothing
uniStepRight (TypeEq _ TypeForall{}) = Nothing
uniStepRight (TypeEq (TypeVar i1) (TypeVar i2)) | i1==i2 = Just (Right [])
uniStepRight (TypeEq (t1) (TypeVar i2)) = if occursIn i2 t1
  then Nothing
  else Just $ Left $ Subst i2 t1
uniStepRight (TypeEq (TypeVar _) _) = Nothing
uniStepRight (TypeEq (TypeConstant i1) (TypeConstant i2)) | i1==i2 = Just (Right [])
uniStepRight (TypeEq (TypeCons s1) (TypeCons s2)) | s1==s2 = Just (Right [])
uniStepRight (TypeEq (TypeArrow t1 t2) (TypeArrow t3 t4)) = Just (Right [TypeEq t1 t3, TypeEq t2 t4])
uniStepRight (TypeEq (TypeApp t1 t2) (TypeApp t3 t4)) = Just (Right [TypeEq t1 t3, TypeEq t2 t4])
uniStepRight _ = Nothing
