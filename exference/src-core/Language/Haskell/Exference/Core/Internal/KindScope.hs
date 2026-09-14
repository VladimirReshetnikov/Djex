{-# LANGUAGE DeriveGeneric #-}

-- | Private lexical kind authority for Exference. Source schemes acquire
-- distinct binder identities before entering a scope. Allocation and
-- substitution retain those identities' kinds, including vacuous binders.
-- Intermediate flexible kinds remain obligations rather than prematurely
-- defaulted entries in the authoritative table.
module Language.Haskell.Exference.Core.Internal.KindScope
  ( KindScope
  , prepareKindScope
  , extendKindScope
  , kindScopeVariables
  , kindScopeBinderKind
  , kindScopeAnnotations
  , retainLeadingForallKinds
  , substituteKindScope
  , checkKindScopeTypes
  , checkKindScopeCompatibility
  , checkKindScopeEquality
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Exference.Core.Internal.Polytype
  ( LeadingForallOpening, leadingForallOpeningSource
  , leadingForallOpeningBindings )
import Language.Haskell.Exference.Core.Internal.VariableSupply
  ( freshSynthesisVariable )
import Language.Haskell.Exference.Core.Types (HsType, SynthesisVariable)
import Language.Haskell.Synthesis.Constraint (Constraint (..))
import Language.Haskell.Synthesis.Kind (Kind (ProperTypeKind))
import Language.Haskell.Synthesis.KindInference
  ( GroundKind, KindAssumptions, checkTypesKinds
  , inferVariableKindsForObligations )
import qualified Language.Haskell.Synthesis.SourceKind as S
import qualified Language.Haskell.Synthesis.Type as T

data KindScope = KindScope
  KindAssumptions
  (Map.Map SynthesisVariable GroundKind)
  [(GroundKind, HsType)]
  deriving (Eq, Show, Generic)

instance NFData KindScope

kindScopeVariables :: KindScope -> Set.Set SynthesisVariable
kindScopeVariables (KindScope _ kinds obligations) = Set.unions
  [Map.keysSet kinds, foldMap (foldMap Set.singleton . snd) obligations]

kindScopeBinderKind :: KindScope -> SynthesisVariable -> Maybe GroundKind
kindScopeBinderKind (KindScope _ kinds _) variable = Map.lookup variable kinds

kindScopeAnnotations :: KindScope -> [(HsType, GroundKind)]
kindScopeAnnotations (KindScope _ kinds _) =
  [(T.TypeVariable variable, kind) | (variable, kind) <- Map.toAscList kinds]

-- Tags travel through the shared capture-avoiding operations. They are
-- metadata, not part of variable identity or allocator ordering.
data Tagged = Tagged SynthesisVariable (Maybe GroundKind) deriving Show

instance Eq Tagged where
  Tagged left _ == Tagged right _ = left == right

instance Ord Tagged where
  compare (Tagged left _) (Tagged right _) = compare left right

identity :: Tagged -> SynthesisVariable
identity (Tagged variable _) = variable

freshTagged :: T.FreshVariableAllocator Tagged
freshTagged reserved (Tagged variable kind) =
  (\fresh -> Tagged fresh kind) <$> freshSynthesisVariable (Set.map identity reserved) variable

prepareKindScope
  :: Set.Set SynthesisVariable
  -> S.SourceTypeKinds SynthesisVariable
  -> Either String (HsType, KindScope)
prepareKindScope reserved checked = do
  tagged <- attach [] Map.empty $ S.sourceKindsType checked
  let protected = Set.union reserved $ Set.map identity $ foldMap Set.singleton tagged
  (unique, _) <- first show $ T.uniquifyTypeBinders
    (const (Nothing :: Maybe Void)) freshTagged
    (Set.map (\variable -> Tagged variable Nothing) protected) tagged
  kinds <- collect Map.empty unique
  let ty = fmap identity unique
      assumptions = S.sourceKindAssumptions checked
      scope = KindScope assumptions kinds []
      free = Set.toAscList $ T.freeVariables ty
  -- Source-free variables are source identities too. Infer their ground
  -- kinds jointly with the exact lexical binder facts before sealing them.
  inferred <- first show $ inferVariableKindsForObligations assumptions
    (map FreeKindVariable free) $ scopeObligations scope [(ProperTypeKind, ty)]
  completed <- foldM retain kinds
    [(variable, kind) | (FreeKindVariable variable, kind) <- inferred]
  pure (ty, KindScope assumptions completed [])
 where
  attach path scope ty = case ty of
    T.TypeVariable variable -> pure $ T.TypeVariable $
      Map.findWithDefault (Tagged variable Nothing) variable scope
    T.TypeConstructor name -> pure $ T.TypeConstructor name
    T.TypeApplication function argument -> T.TypeApplication
      <$> descend S.ApplicationFunction function <*> descend S.ApplicationArgument argument
    T.FunctionType parameter result -> T.FunctionType
      <$> descend S.FunctionParameter parameter <*> descend S.FunctionResult result
    T.TupleType box fields -> T.TupleType box <$> sequence
      [descend (S.TupleElement index) field | (index, field) <- zip [0..] fields]
    T.ForallType binders constraints body -> do
      marked <- sequence
        [ case Map.lookup (path, slot) $ S.sourceBinderKinds checked of
            Nothing -> Left "checked source binder has no lexical kind"
            Just kind -> Right $ Tagged binder $ Just kind
        | (slot, binder) <- zip [0..] binders ]
      let nested = Map.union (Map.fromList $ zip binders marked) scope
      context <- sequence
        [ Constraint name <$> sequence
            [attach (path ++ [S.ForallConstraintArgument index argumentIndex]) nested argument
            | (argumentIndex, argument) <- zip [0..] arguments]
        | (index, Constraint name arguments) <- zip [0..] constraints ]
      T.ForallType marked context <$> attach (path ++ [S.ForallBody]) nested body
   where
    descend step = attach (path ++ [step]) scope

-- | Acquire a provider's source facts independently, then extend the branch
-- with its fresh namespace. A checked source from another inventory cannot
-- replace this scope's nominal or class kind assumptions.
extendKindScope
  :: Set.Set SynthesisVariable -> KindScope
  -> S.SourceTypeKinds SynthesisVariable
  -> Either String (HsType, KindScope)
extendKindScope reserved scope@(KindScope assumptions kinds pending) checked
  | assumptions /= S.sourceKindAssumptions checked =
      Left "source kinds belong to a different inventory"
  | otherwise = do
      (ty, KindScope _ incoming _) <- prepareKindScope
        (Set.union reserved $ kindScopeVariables scope) checked
      combined <- foldM retain kinds $ Map.toList incoming
      let next = KindScope assumptions combined pending
      checkKindScopeTypes next [(ProperTypeKind, ty)]
      pure (ty, next)

retain
  :: Map.Map SynthesisVariable GroundKind
  -> (SynthesisVariable, GroundKind)
  -> Either String (Map.Map SynthesisVariable GroundKind)
retain kinds (variable, kind) = case Map.lookup variable kinds of
  Just previous | previous /= kind ->
    Left $ "conflicting lexical kinds for " ++ show variable
  _ -> Right $ Map.insert variable kind kinds

collect
  :: Map.Map SynthesisVariable GroundKind
  -> T.Type Tagged
  -> Either String (Map.Map SynthesisVariable GroundKind)
collect initial = foldM add initial . foldr (:) []
 where
  add kinds (Tagged _ Nothing) = Right kinds
  add kinds (Tagged variable (Just kind)) = retain kinds (variable, kind)

-- | Consume actual allocation evidence, not an inferred offset or a map
-- recovered from the opened body. Later layers see earlier layers' facts.
retainLeadingForallKinds
  :: KindScope -> [LeadingForallOpening] -> Either String KindScope
retainLeadingForallKinds = foldM opening
 where
  opening scope@(KindScope assumptions kinds obligations) evidence = do
    let bindings = leadingForallOpeningBindings evidence
        targets = map (T.FlexibleVariable . snd) bindings
    if length targets /= Set.size (Set.fromList targets)
        || any (\target -> Set.member target $ kindScopeVariables scope) targets
      then Left "forall allocation reuses an owned kind identity"
      else pure ()
    case leadingForallOpeningSource evidence of
      T.ForallType binders _ _
        | binders == map (T.FlexibleVariable . fst) bindings -> pure ()
      _ -> Left "forall allocation evidence does not identify its source slots"
    selected <- mapM (binding scope) bindings
    updated <- foldM retain kinds selected
    pure $ KindScope assumptions updated obligations
  binding scope (old, new) =
    case kindScopeBinderKind scope $ T.FlexibleVariable old of
      Nothing -> Left $ "opened binder has no checked kind: " ++ show old
      Just kind -> Right (T.FlexibleVariable new, kind)

-- | Substitute a whole state fragment and its pending kind obligations under
-- one reservation set. Replacements are simultaneous. Only source-owned
-- kinds are fixed; kinds of intermediate metavariables are solved afresh
-- from the retained obligations on each check.
substituteKindScope
  :: KindScope
  -> Map.Map SynthesisVariable HsType
  -> [HsType]
  -> Either String ([HsType], KindScope)
substituteKindScope scope@(KindScope assumptions kinds obligations) substitutions sources = do
  let selections =
        [(kind, image) | (variable, image) <- Map.toAscList substitutions,
          Just kind <- [Map.lookup variable kinds]]
      tag variable = Tagged variable $ Map.lookup variable kinds
      batch = sources ++ map snd obligations
  substituted <- first show $ T.substituteTypeVariablesBatch freshTagged
    (Set.map tag $ kindScopeVariables scope)
    (Map.fromList [(tag variable, fmap tag ty) | (variable, ty) <- Map.toList substitutions])
    (map (fmap tag) batch)
  transported <- foldM collect kinds substituted
  updated <- foldM retain transported
    [(variable, kind) | (kind, T.TypeVariable variable) <- selections]
  let (results, pending) = splitAt (length sources) $ map (fmap identity) substituted
      next = KindScope assumptions updated $ Set.toAscList $ Set.fromList $
        zip (map fst obligations) pending ++ selections
  checkKindScopeTypes next []
  pure (results, next)

-- Each obligation's nested binders have separate inference tokens. An
-- unrelated scheme reusing a bound spelling cannot shadow another
-- obligation's free metavariable.
data KindVariable
  = FreeKindVariable SynthesisVariable
  | BoundKindVariable Natural [S.SourceTypeStep] Int
  | KindEquality Natural
  deriving (Eq, Ord, Show)

checkKindScopeTypes :: KindScope -> [(GroundKind, HsType)] -> Either String ()
checkKindScopeTypes scope@(KindScope assumptions _ pending) obligations = do
  mapM_ (first show . T.validateType . snd) $ obligations ++ pending
  first show $ checkTypesKinds assumptions $ scopeObligations scope obligations

scopeObligations :: KindScope -> [(GroundKind, HsType)] -> [(GroundKind, T.Type KindVariable)]
scopeObligations = scopeObligationsFrom 0

scopeObligationsFrom
  :: Natural -> KindScope -> [(GroundKind, HsType)]
  -> [(GroundKind, T.Type KindVariable)]
scopeObligationsFrom start (KindScope _ kinds pending) obligations =
  [(kind, T.TypeVariable $ FreeKindVariable variable) | (variable, kind) <- Map.toList kinds]
  ++ concat
    [ let (opened, exact) = openKinds kinds index [] Map.empty ty
      in (kind, opened) : exact
    | (index, (kind, ty)) <- zip [start..] $ obligations ++ pending ]

openKinds
  :: Map.Map SynthesisVariable GroundKind
  -> Natural -> [S.SourceTypeStep]
  -> Map.Map SynthesisVariable KindVariable -> HsType
  -> (T.Type KindVariable, [(GroundKind, T.Type KindVariable)])
openKinds kinds index path scope ty = case ty of
  T.TypeVariable variable ->
    (T.TypeVariable $ Map.findWithDefault (FreeKindVariable variable) variable scope, [])
  T.TypeConstructor name -> (T.TypeConstructor name, [])
  T.TypeApplication function argument -> binary T.TypeApplication
    (S.ApplicationFunction, function) (S.ApplicationArgument, argument)
  T.FunctionType parameter result -> binary T.FunctionType
    (S.FunctionParameter, parameter) (S.FunctionResult, result)
  T.TupleType box fields ->
    let parts = [descend (S.TupleElement slot) field | (slot, field) <- zip [0..] fields]
    in (T.TupleType box $ map fst parts, concatMap snd parts)
  T.ForallType binders constraints body ->
    let tokens = [BoundKindVariable index path slot | slot <- [0 .. length binders - 1]]
        nested = Map.union (Map.fromList $ zip binders tokens) scope
        exact = [(kind, T.TypeVariable token) | (binder, token) <- zip binders tokens,
                  Just kind <- [Map.lookup binder kinds]]
        contextParts =
          [ let parts =
                  [openKinds kinds index (path ++ [S.ForallConstraintArgument ci ai]) nested argument
                  | (ai, argument) <- zip [0..] arguments]
            in (Constraint name $ map fst parts, concatMap snd parts)
          | (ci, Constraint name arguments) <- zip [0..] constraints ]
        (openedBody, bodyExact) = openKinds kinds index (path ++ [S.ForallBody]) nested body
    -- Retaining a binderless forall preserves the proper-type requirement on
    -- its body and the kind obligations of every dictionary argument.
    in (T.ForallType [] (map fst contextParts) openedBody,
        exact ++ concatMap snd contextParts ++ bodyExact)
 where
  descend step = openKinds kinds index (path ++ [step]) scope
  binary constructor (leftStep, left) (rightStep, right) =
    let (leftType, leftExact) = descend leftStep left
        (rightType, rightExact) = descend rightStep right
    in (constructor leftType rightType, leftExact ++ rightExact)

-- | Additional obligations for ordinary type equality. The type unifier
-- still owns structural equality. Here corresponding forall slots must have
-- equal kinds, even when their bound variables never occur in either body.
checkKindScopeCompatibility
  :: KindScope -> GroundKind -> HsType -> HsType -> Either String ()
checkKindScopeCompatibility scope kind left right = do
  checkKindScopeTypes scope [(kind, left), (kind, right)]
  checkKindScopeEquality scope left right

-- | The unifier also compares class arguments and constructor heads, whose
-- common kind need not be Type. Infer that common kind jointly with the
-- retained obligations instead of imposing the expression-result kind.
checkKindScopeEquality :: KindScope -> HsType -> HsType -> Either String ()
checkKindScopeEquality scope@(KindScope assumptions kinds _) left right = do
  mapM_ (first show . T.validateType) [left, right]
  first show $ checkTypesKinds assumptions $
    scopeObligationsFrom 2 scope [] ++ leftExact ++ rightExact
      ++ rootEquality ++ equalities
 where
  canonicalLeft = T.canonicalizeType left
  canonicalRight = T.canonicalizeType right
  (openedLeft, leftExact) = openKinds kinds 0 [] Map.empty canonicalLeft
  (openedRight, rightExact) = openKinds kinds 1 [] Map.empty canonicalRight
  rootEquality =
    [(ProperTypeKind, T.TypeApplication (T.TypeVariable $ KindEquality 0) ty)
    | ty <- [openedLeft, openedRight]]
  pairs = corresponding [] [] canonicalLeft canonicalRight
  equalities =
    [ (ProperTypeKind, T.TypeApplication
        (T.TypeVariable $ KindEquality index) (T.TypeVariable variable))
    | (index, (leftVariable, rightVariable)) <- zip [1..] pairs
    , variable <- [leftVariable, rightVariable] ]
  corresponding lp rp l r = case (l, r) of
    (T.TypeApplication lf la, T.TypeApplication rf ra) ->
      descend S.ApplicationFunction lf rf ++ descend S.ApplicationArgument la ra
    (T.FunctionType la lb, T.FunctionType ra rb) ->
      descend S.FunctionParameter la ra ++ descend S.FunctionResult lb rb
    (T.TupleType _ ls, T.TupleType _ rs) -> concat
      [descend (S.TupleElement slot) a b | (slot, (a, b)) <- zip [0..] $ zip ls rs]
    (T.ForallType lbs lcs lb, T.ForallType rbs rcs rb) ->
      [(BoundKindVariable 0 lp slot, BoundKindVariable 1 rp slot)
      | slot <- [0 .. min (length lbs) (length rbs) - 1]]
      ++ concat
        [descend (S.ForallConstraintArgument ci ai) a b
        | (ci, (Constraint _ las, Constraint _ ras)) <- zip [0..] $ zip lcs rcs
        , (ai, (a, b)) <- zip [0..] $ zip las ras]
      ++ descend S.ForallBody lb rb
    _ -> []
   where
    descend step = corresponding (lp ++ [step]) (rp ++ [step])
