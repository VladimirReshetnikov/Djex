-- | Preserve lexical binder kinds through capture-avoiding synonym expansion.
-- Kind evidence travels with bound identities while the existing synonym
-- traversal moves or copies subtrees. It is recovered at the resulting
-- structural positions, never matched by spelling or an erasing alpha key.
module Language.Haskell.Synthesis.SourceKind.Expansion
  ( SourceKindExpansionError (..)
  , expandSourceTypeKinds
  ) where

import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import Language.Haskell.Synthesis.Constraint (Constraint (..))
import Language.Haskell.Synthesis.KindInference (GroundKind)
import Language.Haskell.Synthesis.Name (Name)
import Language.Haskell.Synthesis.SourceKind
import Language.Haskell.Synthesis.Type
  ( Type (..), FreshVariableAllocator, BinderNormalizationError (..)
  , uniquifyTypeBinders )
import Language.Haskell.Synthesis.TypeSynonym
  ( SynonymExpansionError (..), expandTypeSynonymDefinitions )

data SourceKindExpansionError variable
  = SourceKindExpansionFailure (SynonymExpansionError variable)
  | SourceKindExpansionNormalizationFailure
      (BinderNormalizationError variable Void)
  | InvalidExpandedSourceKinds (SourceKindError variable)
  deriving (Eq, Ord, Show)

-- This tag is private and temporary. Free variables and synonym-definition
-- variables have no attached source kind. Metadata never changes variable
-- identity: the shared lexical substitution rules must still see collisions
-- between a source binder and a same-named definition parameter or free use.
data KindedVariable variable = KindedVariable variable (Maybe GroundKind)
  deriving (Show)

instance Eq variable => Eq (KindedVariable variable) where
  left == right = underlying left == underlying right

instance Ord variable => Ord (KindedVariable variable) where
  compare left right = compare (underlying left) (underlying right)

underlying :: KindedVariable variable -> variable
underlying (KindedVariable variable _) = variable

-- | Expand the parser adapter's unannotated synonym definitions while keeping
-- every checked source binder's kind, including an inferred kind whose only
-- use is in a phantom synonym argument. The returned annotations are effective
-- obligations on the expanded type; they are not a list of written source
-- annotations. Retain the input separately when displaying original syntax.
--
-- The input was checked before expansion, so dropping a phantom argument
-- cannot make an invalid source annotation acceptable. Recheck after expansion
-- against exactly the same nominal assumptions. Existing synonym saturation,
-- recursion, hygiene and fresh-allocation checks remain authoritative.
expandSourceTypeKinds
  :: Ord variable
  => FreshVariableAllocator variable
  -> Map.Map Name ([variable], Type variable)
  -> SourceTypeKinds variable
  -> Either (SourceKindExpansionError variable) (SourceTypeKinds variable)
expandSourceTypeKinds fresh definitions checked = do
  tagged <- first InvalidExpandedSourceKinds $
    attach [] Map.empty $ sourceKindsType checked
  expanded <- first (SourceKindExpansionFailure . untagExpansionError) $
    expandTypeSynonymDefinitions freshTagged taggedDefinitions tagged
  -- Make binder identities globally unique before projecting, including
  -- cloned arguments and definition binders. Keep unclaimed source identities
  -- so existing source spelling hints remain usable. Metadata is ignored by
  -- Eq/Ord, so the shared namespace checks already see projected collisions.
  (unique, _) <- first
    (SourceKindExpansionNormalizationFailure . untagNormalizationError) $
    uniquifyTypeBinders (const Nothing) freshTagged
      Set.empty expanded
  first InvalidExpandedSourceKinds $ prepareSourceTypeKinds
    (sourceKindAssumptions checked) (fmap underlying unique)
    (collect [] unique)
 where
  plain variable = KindedVariable variable Nothing
  taggedDefinitions = Map.map
    (\(parameters, body) -> (map plain parameters, fmap plain body)) definitions
  freshTagged reserved (KindedVariable old kind) = do
    replacement <- fresh (Set.map underlying reserved) old
    pure $ KindedVariable replacement kind

  attach path scope ty = case ty of
    TypeVariable variable -> pure $ TypeVariable $
      Map.findWithDefault (plain variable) variable scope
    TypeConstructor name -> pure $ TypeConstructor name
    TypeApplication function argument -> TypeApplication
      <$> descend ApplicationFunction function <*> descend ApplicationArgument argument
    FunctionType parameter result -> FunctionType
      <$> descend FunctionParameter parameter <*> descend FunctionResult result
    TupleType boxity fields -> TupleType boxity
      <$> sequence [descend (TupleElement index) field | (index, field) <- zip [0..] fields]
    ForallType binders constraints body -> do
      marked <- sequence
        [ case Map.lookup (path, slot) $ sourceBinderKinds checked of
            Nothing -> Left $ MissingSourceKindBinder path slot
            Just kind -> Right $ KindedVariable binder $ Just kind
        | (slot, binder) <- zip [0..] binders ]
      let nested = Map.union (Map.fromList $ zip binders marked) scope
      context <- sequence
        [ Constraint name <$> sequence
            [ attach (path ++ [ForallConstraintArgument index argumentIndex]) nested argument
            | (argumentIndex, argument) <- zip [0..] arguments ]
        | (index, Constraint name arguments) <- zip [0..] constraints ]
      ForallType marked context <$> attach (path ++ [ForallBody]) nested body
   where
    descend step = attach (path ++ [step]) scope

collect :: [SourceTypeStep] -> Type (KindedVariable variable) -> [SourceKindAnnotation]
collect path ty = case ty of
  TypeVariable{} -> []
  TypeConstructor{} -> []
  TypeApplication function argument ->
    descend ApplicationFunction function ++ descend ApplicationArgument argument
  FunctionType parameter result ->
    descend FunctionParameter parameter ++ descend FunctionResult result
  TupleType _ fields -> concat
    [descend (TupleElement index) field | (index, field) <- zip [0..] fields]
  ForallType binders constraints body ->
    [SourceKindAnnotation path slot kind
    | (slot, KindedVariable _ (Just kind)) <- zip [0..] binders] ++
    concat
      [ descend (ForallConstraintArgument index argumentIndex) argument
      | (index, Constraint _ arguments) <- zip [0..] constraints
      , (argumentIndex, argument) <- zip [0..] arguments ] ++
    descend ForallBody body
 where
  descend step = collect (path ++ [step])

untagExpansionError
  :: SynonymExpansionError (KindedVariable variable) -> SynonymExpansionError variable
untagExpansionError failure = case failure of
  IntrinsicTypeSynonym name -> IntrinsicTypeSynonym name
  DuplicateTypeSynonymParameter name variable -> DuplicateTypeSynonymParameter name $ underlying variable
  UnsaturatedTypeSynonym name expected actual -> UnsaturatedTypeSynonym name expected actual
  RecursiveTypeSynonyms names -> RecursiveTypeSynonyms names
  FreshVariableUnavailable variable -> FreshVariableUnavailable $ underlying variable
  FreshVariableCollision old replacement -> FreshVariableCollision (underlying old) (underlying replacement)

untagNormalizationError
  :: BinderNormalizationError (KindedVariable variable) Void
  -> BinderNormalizationError variable Void
untagNormalizationError failure = case failure of
  RejectedTypeBinder rejection -> RejectedTypeBinder rejection
  DuplicateTypeBinder variable -> DuplicateTypeBinder $ underlying variable
  TypeBinderFresheningError substitution -> TypeBinderFresheningError $ fmap underlying substitution
