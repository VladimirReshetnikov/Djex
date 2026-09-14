-- | Private kind ownership acquired from a checked lexical source type.
-- Names are freshened before they become keys. Substitution transports tags
-- through the shared capture-avoiding operation; it never rediscovers kinds
-- by source spelling or by an alpha key which erases vacuous binders.
module Djinn.Internal.LexicalKinds
  ( LexicalKinds, emptyLexicalKinds, hasLexicalKinds
  , prepareLexicalKinds, lexicalBinderKind, lexicalVariables, lexicalKindAnnotations
  , substituteLexicalKinds, retainKindSelection, lexicalSelections
  ) where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import Language.Haskell.Synthesis.Constraint (Constraint (..))
import Language.Haskell.Synthesis.KindInference (GroundKind)
import qualified Language.Haskell.Synthesis.SourceKind as S
import qualified Language.Haskell.Synthesis.Type as T

type Variable = T.Variable String
type Type = T.Type Variable

data LexicalKinds = LexicalKinds (Map.Map Variable GroundKind) [(GroundKind, Type)]

emptyLexicalKinds :: LexicalKinds
emptyLexicalKinds = LexicalKinds Map.empty []

hasLexicalKinds :: LexicalKinds -> Bool
hasLexicalKinds (LexicalKinds kinds _) = not $ Map.null kinds

lexicalBinderKind :: LexicalKinds -> Variable -> Maybe GroundKind
lexicalBinderKind (LexicalKinds kinds _) variable = Map.lookup variable kinds

lexicalVariables :: LexicalKinds -> Set.Set Variable
lexicalVariables (LexicalKinds kinds _) = Map.keysSet kinds

-- Exact variable identities only; this table travels with the checked graph.
lexicalKindAnnotations :: LexicalKinds -> [(Type, GroundKind)]
lexicalKindAnnotations (LexicalKinds kinds _) =
  [(T.TypeVariable variable, kind) | (variable, kind) <- Map.toAscList kinds]

lexicalSelections :: LexicalKinds -> [(GroundKind, Type)]
lexicalSelections (LexicalKinds _ selections) = selections

retainKindSelection :: Variable -> Type -> LexicalKinds -> LexicalKinds
retainKindSelection binder selected authority@(LexicalKinds kinds selections) =
  case Map.lookup binder kinds of
    Nothing -> authority
    Just kind -> LexicalKinds kinds ((kind, selected) : selections)

data Tagged = Tagged Variable (Maybe GroundKind) deriving Show
instance Eq Tagged where
  Tagged left _ == Tagged right _ = left == right
instance Ord Tagged where
  compare (Tagged left _) (Tagged right _) = compare left right

identity :: Tagged -> Variable
identity (Tagged variable _) = variable

fresh :: T.FreshVariableAllocator Variable
fresh reserved variable = Just $ choose $ fmap (++ "'") variable
 where
  choose candidate
    | Set.member candidate reserved = choose $ fmap (++ "'") candidate
    | otherwise = candidate

freshTagged :: T.FreshVariableAllocator Tagged
freshTagged reserved (Tagged variable kind) =
  (`Tagged` kind) <$> fresh (Set.map identity reserved) variable

prepareLexicalKinds
  :: Set.Set Variable -> S.SourceTypeKinds String -> Either String (Type, LexicalKinds)
prepareLexicalKinds reserved checked = do
  tagged <- attach [] Map.empty $ S.sourceKindsType checked
  -- Reserve every input identity, including binder identities, so the keys
  -- cannot accidentally identify an unannotated neighboring source scheme.
  let protected = Set.union reserved $ Set.fromList $ map identity $ foldr (:) [] tagged
      normalize = T.uniquifyTypeBinders (const (Nothing :: Maybe Void)) freshTagged
        (Set.map (\variable -> Tagged variable Nothing) protected)
  (unique, _) <- first show $ normalize tagged
  kinds <- collect Map.empty unique
  pure (fmap identity unique, LexicalKinds kinds [])
 where
  attach path scope ty = case ty of
    T.TypeVariable variable -> pure $ T.TypeVariable $
      Map.findWithDefault (Tagged (T.FlexibleVariable variable) Nothing) variable scope
    T.TypeConstructor name -> pure $ T.TypeConstructor name
    T.TypeApplication f x -> T.TypeApplication
      <$> descend S.ApplicationFunction f <*> descend S.ApplicationArgument x
    T.FunctionType a b -> T.FunctionType
      <$> descend S.FunctionParameter a <*> descend S.FunctionResult b
    T.TupleType box fields -> T.TupleType box <$> sequence
      [descend (S.TupleElement index) field | (index, field) <- zip [0..] fields]
    T.ForallType binders constraints body -> do
      marked <- sequence
        [ case Map.lookup (path, slot) $ S.sourceBinderKinds checked of
            Nothing -> Left "checked lexical source binder has no kind"
            Just kind -> Right $ Tagged (T.FlexibleVariable binder) $ Just kind
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

substituteLexicalKinds
  :: LexicalKinds -> Map.Map Variable Type -> Type -> Either String (Type, LexicalKinds)
substituteLexicalKinds authority@(LexicalKinds kinds selections) substitutions source
  | Map.null kinds = do
      result <- first show $ T.substituteTypeVariables fresh Set.empty substitutions source
      pure (result, authority)
  | otherwise = do
      let tag variable = Tagged variable $ Map.lookup variable kinds
      result <- first show $ T.substituteTypeVariables freshTagged
        (Set.map tag $ Map.keysSet kinds)
        (Map.fromList [(tag variable, fmap tag ty) | (variable, ty) <- Map.toList substitutions])
        (fmap tag source)
      retained <- collect kinds result
      pure (fmap identity result, LexicalKinds retained selections)

collect :: Map.Map Variable GroundKind -> T.Type Tagged -> Either String (Map.Map Variable GroundKind)
collect initial = foldM retain initial . foldr (:) []
 where
  retain kinds (Tagged _ Nothing) = Right kinds
  retain kinds (Tagged variable (Just kind)) = case Map.lookup variable kinds of
    Just previous | previous /= kind -> Left "conflicting kinds for one exact lexical identity"
    _ -> Right $ Map.insert variable kind kinds
