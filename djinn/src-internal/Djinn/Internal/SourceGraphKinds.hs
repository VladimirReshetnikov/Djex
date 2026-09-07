-- | Kind validation for a source-checked candidate before graph sealing.
-- Erased selections carry the same kind obligations as visible selections.
-- All obligations share one free-variable kind scope; an unused source binder
-- uses Djinn's ordinary proper-type default unless its exact source provider
-- owns an already-validated complete ground-kind vector.
module Djinn.Internal.SourceGraphKinds
  ( validateSourceGraphKinds
  , inferSourceGraphMetavariableKinds
  ) where

import Control.Monad (foldM, unless, when)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void, absurd)

import Djinn.Internal.Environment (preparedEnvironmentInventory)
import Djinn.Internal.SourceTypingContext
  ( SourceTypingContext, sourceTypingPreparedEnvironment
  , sourceTypingProviderKinds, sourceTypingTermSchemes )
import Language.Haskell.Synthesis.Collection (observedListLength)
import qualified Language.Haskell.Synthesis.Inventory as Inventory
import qualified Language.Haskell.Synthesis.Kind as K
import qualified Language.Haskell.Synthesis.KindInference as KI
import qualified Language.Haskell.Synthesis.Type as T
import qualified Language.Haskell.Synthesis.TypeAtom as A
import qualified Language.Haskell.Synthesis.TypedGenerated as Q

type Variable = T.Variable String
type Type = T.Type Variable
type GroundKind = K.Kind Void
type Node = (Q.TermNodeId, Q.TermNode Type String)

validateSourceGraphKinds
  :: SourceTypingContext
  -> Q.TermGraphSource Type String
  -> Either String ()
validateSourceGraphKinds context source = do
  (assumptions, obligations, _) <- sourceGraphKindObligations context source
  checkObligations assumptions obligations

-- | Infer only still-referenced requested flexible variables, after fixing
-- source binder kinds exactly as ordinary graph validation does. The returned
-- kinds authorize no substitution: the caller must choose closed source types,
-- apply them, and re-run source/graph validation. Generalized, discarded, and
-- forall-bound variables do not create completion obligations.
inferSourceGraphMetavariableKinds
  :: SourceTypingContext
  -> [Variable]
  -> Q.TermGraphSource Type String
  -> Either String [(Variable, GroundKind)]
inferSourceGraphMetavariableKinds context requested source = do
  let limits = Q.defaultTermGraphLimits
      -- A finite upper bound derived from existing graph/type capacities,
      -- including discarded requests rather than following an unbounded tail.
      capacity = fromInteger $ min (toInteger (maxBound :: Int)) $
        toInteger (Q.maximumTermGraphTypeNodes limits)
          * (4 * toInteger (Q.maximumTermGraphNodes limits)
             + toInteger (Q.maximumTermGraphPatternNodes limits))
  when (observedListLength capacity requested > capacity) $
    Left "source graph kind completion request exceeds graph capacities"
  (assumptions, obligations, graphFree) <- sourceGraphKindObligations context source
  checkObligations assumptions obligations
  let referenced = Set.toAscList $ Set.filter T.isFlexibleVariable $
        Set.intersection graphFree $ Set.fromList requested
      reserved = Set.unions $ map (allVariables . snd) obligations
      proper = concat
        [ kindConstraints (Q.termNodeId ordinal) reserved kind ty
        | (ordinal, (kind, ty)) <- zip [0 ..] obligations
        ]
      shared = Set.toAscList $ Set.unions $ map T.freeVariables proper
  inferred <- first ("source graph completion kinds: " ++) . first show $
    KI.inferSharedVariableKinds assumptions shared proper
  mapM (lookupKind $ Map.fromList inferred) referenced
 where
  lookupKind inferred variable = case Map.lookup variable inferred of
    Nothing -> Left "referenced source metavariable has no inferred kind"
    Just kind -> Right (variable, kind)

checkObligations :: KI.KindAssumptions -> [(GroundKind, Type)] -> Either String ()
checkObligations assumptions obligations =
  first ("source graph kind mismatch: " ++) . first show $
    KI.checkTypesKinds assumptions obligations

sourceGraphKindObligations
  :: SourceTypingContext
  -> Q.TermGraphSource Type String
  -> Either String (KI.KindAssumptions, [(GroundKind, Type)], Set.Set Variable)
sourceGraphKindObligations context source = do
  observe "node table" (Q.maximumTermGraphNodes limits) rawNodes
  unless (Map.size nodes == length rawNodes) $ Left "duplicate source graph node identity"
  unless (Map.member (Q.termGraphSourceRoot source) nodes) $
    Left "source graph root is absent during kind checking"
  proper <- fmap concat $ mapM properAnnotations rawNodes
  selections <- fmap concat $ mapM selectionAnnotations rawNodes
  let allTypes = proper ++ concatMap (\(_, before, selected) -> [before, selected]) selections
  mapM_ observeType allTypes
  globals <- first ("source kind inventory: " ++) $ sourceTypingTermSchemes context
  let globals' = Map.map (fmap T.FlexibleVariable) globals
      reserved = Set.unions $ map allVariables allTypes
  prepared <- mapM (prepareSelection globals' reserved) selections
  overrideConstraints <- fmap concat $ mapM (retainedKindConstraints reserved) prepared
  let sourceTypes = proper ++ [body | (_, _, body, _, _) <- prepared] ++ overrideConstraints
      sharedVariables = Set.toAscList $ Set.unions $
        Set.fromList [binder | (_, binder, _, _, _) <- prepared]
          : map T.freeVariables sourceTypes
  inferred <- first ("source forall binder kinds: " ++) . first show $
    KI.inferSharedVariableKinds assumptions sharedVariables sourceTypes
  selectionObligations <- fmap concat $ mapM (selectionKinds $ Map.fromList inferred) prepared
  pure
    ( assumptions
    , map ((,) K.ProperTypeKind) sourceTypes ++ selectionObligations
    , Set.unions $ map T.freeVariables allTypes
    )
 where
  limits = Q.defaultTermGraphLimits
  rawNodes = Q.termGraphSourceNodes source
  nodes = Map.fromList rawNodes
  assumptions = Inventory.inventoryKindAssumptions $
    preparedEnvironmentInventory $ sourceTypingPreparedEnvironment context
  providerKinds = sourceTypingProviderKinds context

  observe label bound values =
    when (observedListLength bound values > bound) $
      Left $ "source graph kind " ++ label ++ " exceeds graph limits"

  observeType ty = first ("source graph kind type limit: " ++) . first show $
    Q.observeTypeWithin Q.sharedTypeStructure
      (Q.maximumTermGraphTypeNodes limits)
      (Q.maximumTermGraphCollectionWidth limits) ty

  -- Pattern collection limits are checked before recursively reading field
  -- annotations, including in a caller-built test table.
  properAnnotations (_, Q.TermNode ty form) = do
    patterns <- case form of
      Q.TypedLambda values _ -> do
        observe "lambda width" (Q.maximumTermGraphCollectionWidth limits) values
        pure values
      Q.TypedLet pattern' _ _ -> pure [pattern']
      Q.TypedCase _ branches -> do
        observe "case width" (Q.maximumTermGraphCollectionWidth limits) branches
        pure $ map fst branches
      _ -> pure []
    (_, patternTypes) <- foldM collectPattern
      (Q.maximumTermGraphPatternNodes limits, []) patterns
    let witnesses = case form of
          Q.TypedApply _ _ witness ->
            [Q.applicationDomain witness, Q.applicationResult witness]
          Q.TypedVisibleTypeApplication _ _ _ witness ->
            [Q.typeApplicationSource witness, Q.typeApplicationResult witness]
          Q.TypedImplicitTypeApplication _ _ witness ->
            [Q.implicitTypeApplicationSource witness, Q.implicitTypeApplicationResult witness]
          Q.TypedForallIntroduction _ _ witness ->
            [Q.forallIntroductionSource witness, Q.forallIntroductionBody witness]
          -- Checking the complete qualified source uses the original
          -- inventory's class arities and parameter kinds. Constraint
          -- arguments may have higher kinds and must not be checked as
          -- independent proper types.
          Q.TypedContextIntroduction _ _ witness ->
            [Q.contextIntroductionSource witness, Q.contextIntroductionBody witness]
          Q.TypedContextApplication _ _ witness ->
            [Q.contextApplicationSource witness, Q.contextApplicationResult witness]
          _ -> []
    pure $ ty : witnesses ++ patternTypes

  collectPattern (remaining, collected) pattern' = do
    when (remaining <= 0) $ Left "source graph kind pattern count exceeds graph limits"
    let fields = case Q.typedPatternNode pattern' of
          Q.TypedConstructor _ values -> values
          Q.TypedTuplePattern values -> values
          Q.TypedAs _ nested -> [nested]
          _ -> []
    observe "pattern width" (Q.maximumTermGraphCollectionWidth limits) fields
    foldM collectPattern
      (remaining - 1, Q.typedPatternType pattern' : collected) fields

  prepareSelection globals reserved (owner, before, selected) = do
    (binder, openedBody) <- freshSourceBinder owner reserved before
    origin <- applicationOrigin globals Set.empty owner
    override <- case origin of
      Nothing -> Right Nothing
      Just (name, consumed) -> case Map.lookup name providerKinds of
        Nothing -> Right Nothing
        Just kinds -> case drop (consumed - 1) kinds of
          kind : _ -> Right $ Just kind
          [] -> Left "source provider kind vector does not cover its exact application slot"
    pure (owner, binder, openedBody, selected, override)

  retainedKindConstraints reserved (owner, binder, _, _, override) = case override of
    Nothing -> Right []
    Just kind -> do
      when (K.observedKindNodeCount (Q.maximumTermGraphTypeNodes limits) kind
          > Q.maximumTermGraphTypeNodes limits) $
        Left "source provider retained kind exceeds graph limits"
      pure $ kindConstraints owner reserved kind $ T.TypeVariable binder

  selectionKinds inferred (_, binder, _, selected, _) = do
    kind <- maybe (Left "source forall binder kind was not returned") Right $
      Map.lookup binder inferred
    -- Infer source-side kinds first. Inferring from a vacuous selected type
    -- would silently replace the ordinary * policy with kind polymorphism.
    pure [(kind, T.TypeVariable binder), (kind, selected)]

  applicationOrigin globals active owner
    | owner `Set.member` active = Left "cyclic source kind application chain"
    | otherwise = case Map.lookup owner nodes of
        Nothing -> Left "dangling source kind application chain"
        Just (Q.TermNode ty form) -> case form of
          Q.TypedGlobal _ name -> do
            expected <- maybe (Left "source kind provider is absent from the inventory") Right $
              Map.lookup name globals
            unless (A.alphaEquivalentTypes expected ty) $
              Left "source kind provider annotation differs from its original scheme"
            pure $ Just (name, 0 :: Int)
          Q.TypedVisibleTypeApplication _ child _ witness ->
            advance globals active owner child $ Q.typeApplicationSource witness
          Q.TypedImplicitTypeApplication _ child witness ->
            advance globals active owner child $ Q.implicitTypeApplicationSource witness
          Q.TypedContextApplication _ child witness ->
            follow globals active owner child $ Q.contextApplicationSource witness
          -- Only the leading source telescope owns the retained kind vector.
          -- An intervening term application, local alias, or introduced
          -- forall begins a different typing site and cannot inherit it.
          _ -> Right Nothing

  advance globals active owner child before = do
    fmap (fmap $ \(name, slot) -> (name, slot + 1)) $
      follow globals active owner child before

  -- A dictionary application keeps the provider's consumed type-binder
  -- count. Only type applications advance its positional kind vector.
  follow globals active owner child before = do
    childType <- case Map.lookup child nodes of
      Nothing -> Left "dangling source kind function child"
      Just node -> Right $ Q.termNodeType node
    unless (A.alphaEquivalentTypes childType before) $
      Left "source kind selection is not attached to its actual function type"
    applicationOrigin globals (Set.insert owner active) child

selectionAnnotations :: Node -> Either String [(Q.TermNodeId, Type, Type)]
selectionAnnotations (owner, Q.TermNode _ form) = case form of
  Q.TypedVisibleTypeApplication _ _ _ witness ->
    pure [(owner, Q.typeApplicationSource witness, Q.typeApplicationSelected witness)]
  Q.TypedImplicitTypeApplication _ _ witness ->
    pure [(owner, Q.implicitTypeApplicationSource witness, Q.implicitTypeApplicationSelected witness)]
  Q.TypedForallIntroduction _ _ witness ->
    pure [(owner, Q.forallIntroductionSource witness, Q.forallIntroductionVariable witness)]
  _ -> pure []

allVariables :: Type -> Set.Set Variable
allVariables ty = T.freeVariables ty `Set.union` Set.fromList (T.typeBinderVariables ty)

-- Express a retained ground-kind equation using only ordinary proper-type
-- obligations. For f :: k -> l, introduce a private x :: k and require f x
-- to have kind l. These variables occur only in the kind checker, never in
-- the source inventory, candidate graph, or compatibility term.
kindConstraints :: Q.TermNodeId -> Set.Set Variable -> GroundKind -> Type -> [Type]
kindConstraints owner reserved = go "root"
 where
  go path kind ty = case kind of
    K.ProperTypeKind -> [ty]
    K.FunctionKind domain result ->
      let argument = T.TypeVariable $ firstUnused reserved $ T.RigidVariable $
            "$djinn$kind$argument$" ++ show (Q.termNodeIdValue owner) ++ "$" ++ path
      in go (path ++ "a") domain argument
          ++ go (path ++ "r") result (T.TypeApplication ty argument)
    K.KindVariable impossible -> absurd impossible

-- The selected binder is renamed before opening: an identically spelled free
-- variable in another source annotation must not constrain its local kind.
freshSourceBinder
  :: Q.TermNodeId -> Set.Set Variable -> Type -> Either String (Variable, Type)
freshSourceBinder owner reserved before = case before of
  T.ForallType (binder : rest) constraints body -> do
    let fresh = firstUnused reserved $ T.RigidVariable $
          "$djinn$kind$" ++ show (Q.termNodeIdValue owner)
        remainder = case (rest, constraints) of
          ([], []) -> body
          _ -> T.ForallType rest constraints body
    opened <- first ("source kind binder opening: " ++) . first show $
      T.substituteTypeVariables freshen reserved
        (Map.singleton binder $ T.TypeVariable fresh) remainder
    normalized <- first ("source kind opened type: " ++) . first show $ T.normalizeType opened
    pure (fresh, normalized)
  _ -> Left "source kind selection does not consume a leading forall"
 where
  freshen used variable = Just $ firstUnused used variable

firstUnused :: Set.Set Variable -> Variable -> Variable
firstUnused reserved variable
  | variable `Set.member` reserved = firstUnused reserved $ fmap (++ "'") variable
  | otherwise = variable
