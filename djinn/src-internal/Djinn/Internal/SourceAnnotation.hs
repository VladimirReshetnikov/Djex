{-# LANGUAGE DeriveGeneric #-}

-- | Private, occurrence-attached instructions retained from checked source
-- erasure. Markers are temporary globals used only during expression cleanup;
-- they are neither search premises nor globals in a sealed source graph.
-- Keeping a marker around its expression allows beta substitution and name
-- cleanup to move or duplicate the association together with that expression.
module Djinn.Internal.SourceAnnotation
  ( SourceAnnotation(..)
  , SourceAnnotations
  , sourceAnnotationAt
  , eraseSourceAnnotations
  , annotatedApplicationSpine
  , SourceSelection
  , sourceAnnotationComparisonExpression
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map
import Djinn.Internal.LJTFormula (Symbol)
import qualified Language.Haskell.Synthesis.Generated as G
import Language.Haskell.Synthesis.Name (Name)
import qualified Language.Haskell.Synthesis.Type as T
import qualified Language.Haskell.Synthesis.TypeAtom as A

data SourceAnnotation
  = RootSourceOpening [String] [Symbol]
  | NestedSourceOpening (T.Type String) [Symbol]
  | SelectedSourceApplication (T.Type String) [T.Type String] [Symbol]
  deriving (Eq, Show, Generic)

instance NFData SourceAnnotation

type SourceAnnotations = Map.Map Name SourceAnnotation

-- Only selections used by the normalized expression enter its comparison
-- key. Lexical positions replace proof-local names; nested type binders are
-- independently alpha-normalized. An evidence node is a free Left local,
-- disjoint from every actual source binder (Right), so its exact occurrence
-- survives alpha/eta comparison without introducing a possible global clash.
data SourceSelection = SourceSelection
  (A.TypeAtomKey (Either Integer String))
  [A.TypeAtomKey (Either Integer String)]
  [Either Symbol Integer]
  deriving (Eq, Ord, Show, Generic)

instance NFData SourceSelection

sourceAnnotationComparisonExpression
  :: SourceAnnotations -> G.Expression local
  -> G.Expression (Either SourceSelection local)
sourceAnnotationComparisonExpression annotations = go Map.empty Map.empty
 where
  extend bindings names = Map.union
    (Map.fromList $ zip names [toInteger (Map.size bindings) ..]) bindings
  go variables dictionaries expression = case sourceAnnotationAt annotations expression of
    Just (RootSourceOpening opened givens, body) ->
      go (extend variables opened) (extend dictionaries givens) body
    Just (NestedSourceOpening _ givens, body) ->
      go variables (extend dictionaries givens) body
    Just (SelectedSourceApplication source arguments givens, body) ->
      let typeKey = A.alphaTypeKey . fmap
            (\variable -> maybe (Right variable) Left $ Map.lookup variable variables)
          dictionaryKey symbol = maybe (Left symbol) Right $ Map.lookup symbol dictionaries
          selection = SourceSelection (typeKey source) (map typeKey arguments)
            (map dictionaryKey givens)
      in G.Apply (G.Local $ Left selection) $ go variables dictionaries body
    Nothing -> case expression of
      G.Local local -> G.Local $ Right local
      G.Global name -> G.Global name
      G.Lambda patterns body -> G.Lambda (map (fmap Right) patterns) $ recur body
      G.Apply function argument -> G.Apply (recur function) (recur argument)
      G.VisibleTypeApplication function argument -> G.VisibleTypeApplication (recur function) argument
      G.Tuple fields -> G.Tuple $ map recur fields
      G.Hole local -> G.Hole $ Right local
      G.Let pattern' value body -> G.Let (fmap Right pattern') (recur value) (recur body)
      G.Case scrutinee branches -> G.Case (recur scrutinee)
        [(fmap Right pattern', recur body) | (pattern', body) <- branches]
     where
      recur = go variables dictionaries

sourceAnnotationAt
  :: SourceAnnotations -> G.Expression local
  -> Maybe (SourceAnnotation, G.Expression local)
sourceAnnotationAt annotations expression = case expression of
  G.Apply (G.Global marker) body ->
    (\annotation -> (annotation, body)) <$> Map.lookup marker annotations
  _ -> Nothing

-- The compatibility clause is the erasure of the same normalized annotated
-- expression that the independent checker consumes, never a second lowering.
-- A marker that escapes its unary wrapper is refused rather than displayed.
eraseSourceAnnotations
  :: SourceAnnotations -> G.Expression local -> Either String (G.Expression local)
eraseSourceAnnotations annotations = go
 where
  go expression | Just (_, body) <- sourceAnnotationAt annotations expression = go body
  go expression = case expression of
    G.Global name | Map.member name annotations -> Left "source annotation escaped its expression"
    G.Lambda patterns body -> G.lambdaExpression patterns <$> go body
    G.Apply function argument -> G.Apply <$> go function <*> go argument
    G.VisibleTypeApplication function argument ->
      (`G.VisibleTypeApplication` argument) <$> go function
    G.Tuple fields -> G.Tuple <$> mapM go fields
    G.Let pattern' value body -> G.Let pattern' <$> go value <*> go body
    G.Case scrutinee branches -> G.Case <$> go scrutinee <*>
      mapM (\(pattern', body) -> (,) pattern' <$> go body) branches
    _ -> Right expression

-- An attached annotation is an application-spine head, not an ordinary
-- function receiving its expression as the first value argument.
annotatedApplicationSpine
  :: SourceAnnotations -> G.Expression local
  -> (G.Expression local, [G.ApplicationArgument local])
annotatedApplicationSpine annotations = go []
 where
  go arguments expression
    | Just _ <- sourceAnnotationAt annotations expression = (expression, arguments)
  go arguments (G.Apply function argument) = go (G.TermArgument argument : arguments) function
  go arguments (G.VisibleTypeApplication function argument) =
    go (G.VisibleTypeArgumentArgument argument : arguments) function
  go arguments expression = (expression, arguments)
