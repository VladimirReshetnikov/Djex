-- | Demand-directed, capture-safe instantiation of context-free schemes.
-- Binder count is not a language boundary here: matching solves all observed
-- binders together. The caller owns the budget for unobserved choices.
module Djinn.Internal.DirectedInstantiation (directedInstantiationTuples) where

import Control.Monad (foldM, guard, replicateM)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Language.Haskell.Synthesis.Collection (distinctOn)
import Language.Haskell.Synthesis.Constraint (Constraint (..))
import Language.Haskell.Synthesis.Type
import Language.Haskell.Synthesis.TypeAtom (alphaTypeKey)

-- | Match a scheme's whole body and every application-result suffix against
-- actual demands. A demand is rigid; only the scheme's leading binders may be
-- selected. Later context-free forall groups in the result spine contribute
-- temporary matching variables, so an eventual result can constrain an earlier
-- choice. Their solutions are discarded here: subsequent checked elimination
-- evidence must still establish those later instances. Whole polymorphic
-- subtrees are ordinary selections. Unobserved
-- binders draw from the supplied, scope-checked vocabulary lazily.
directedInstantiationTuples
    :: Int -> Type String -> [Type String] -> [Type String] -> [[Type String]]
directedInstantiationTuples attempts source demands candidates = case unique source of
    Nothing -> []
    Just normalized -> case splitLeadingForalls normalized of
        (binders@(_ : _), [], body) ->
            let matches =
                    [ (solved, filter (`Map.notMember` solved) binders)
                    | (laterBinders, suffix) <- resultSuffixes [] body
                    , actual <- mapMaybe unique demands
                    , Just solved <- [matchType (Set.fromList $ binders ++ laterBinders)
                        Map.empty Set.empty Map.empty suffix actual]
                    ]
                vectors (solved, missing) =
                    [map (selection solved missing choices) binders
                    | choices <- unobservedChoices $ length missing]
            -- Charge raw proposals before alpha deduplication. Otherwise a
            -- finite output allowance can still inspect a Cartesian-sized
            -- duplicate suffix before discovering that no new tuple remains.
            in distinctOn (map alphaTypeKey) $ take attempts $
                concatMap vectors (filter (null . snd) matches) ++
                roundRobin (map vectors $ filter (not . null . snd) matches)
        _ -> []
  where
    unique ty = either (const Nothing) (Just . fst) $
        uniquifyTypeBinders (const (Nothing :: Maybe ())) fresh Set.empty ty
    -- Selected images pass through the ordinary source kind checker later.
    -- Keep renamed lexical binders valid source identifiers; private '$'
    -- spellings are reserved for separately owned rigid variables.
    fresh reserved _old = Just $ choose (0 :: Integer)
      where
        choose n
            | candidate `Set.member` reserved = choose $ n + 1
            | otherwise = candidate
          where candidate = "djinnDirected" ++ show n
    roundRobin streams = case [(x, xs) | x : xs <- streams] of
        [] -> []
        active -> map fst active ++ roundRobin (map snd active)
    selection solved missing choices binder =
        (Map.union solved $ Map.fromList $ zip missing choices) Map.! binder
    -- A scheme which ignores its result parameters often needs the same
    -- available argument repeatedly. Visit these diagonal choices before a
    -- Cartesian tail, whose first coordinate otherwise starves all others.
    unobservedChoices 0 = [[]]
    unobservedChoices count =
        map (replicate count) candidates ++ replicateM count candidates
    resultSuffixes later body = (later, body) : case body of
        FunctionType _ result -> resultSuffixes later result
        ForallType binders [] result -> resultSuffixes (later ++ binders) result
        _ -> []

-- A separate lexical correspondence records nested binders. It takes
-- precedence over selectable outer names, including when source syntax
-- shadows a leading binder. No selected image may refer to an actual-side
-- binder which exists only inside this comparison.
matchType
    :: Set.Set String -> Map.Map String String -> Set.Set String
    -> Map.Map String (Type String) -> Type String -> Type String
    -> Maybe (Map.Map String (Type String))
matchType bindable lexical protected solved patternType actual =
    case patternType of
        TypeVariable variable
            | Just partner <- Map.lookup variable lexical ->
                solved <$ guard (actual == TypeVariable partner)
            | variable `Set.member` bindable -> do
                guard $ Set.null $ Set.intersection protected $ freeVariables actual
                case Map.lookup variable solved of
                    Nothing -> pure $ Map.insert variable actual solved
                    Just previous -> solved <$ guard
                        (alphaTypeKey previous == alphaTypeKey actual)
        _ -> structural
  where
    descend = matchType bindable lexical protected
    pair current left right = uncurry (descend current) (left, right)
    pairs current left right = do
        guard $ length left == length right
        foldM (\state (a, b) -> pair state a b) current $ zip left right
    structural = case (patternType, actual) of
        (TypeVariable a, TypeVariable b) -> solved <$ guard (a == b)
        (TypeConstructor a, TypeConstructor b) -> solved <$ guard (a == b)
        (TypeApplication f a, TypeApplication g b) ->
            descend solved f g >>= \next -> descend next a b
        (FunctionType a b, FunctionType c d) ->
            descend solved a c >>= \next -> descend next b d
        (TupleType box as, TupleType other bs) -> do
            guard $ box == other
            pairs solved as bs
        (ForallType as cs body, ForallType bs ds other) -> do
            guard $ length as == length bs && length cs == length ds
            let nested = matchType (bindable `Set.difference` Set.fromList as)
                    (Map.union (Map.fromList $ zip as bs) lexical)
                    (protected `Set.union` Set.fromList bs)
                context state (Constraint c xs, Constraint d ys) = do
                    guard $ c == d && length xs == length ys
                    foldM (\next (x, y) -> nested next x y) state $ zip xs ys
            next <- foldM context solved $ zip cs ds
            nested next body other
        _ -> Nothing
