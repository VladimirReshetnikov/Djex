{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Package-private atomic ownership of structural specialization plans and
-- their exact typed-term occurrences.
--
-- The entrance accepts an untrusted graph source rather than a detachable
-- sealed graph.  It first rebuilds and matches every independently checked
-- origin, then seals the graph while provisionally admitting only the
-- certificate-bearing visible-application witness check.  Before a graph can
-- escape, this module derives every occurrence from the rooted graph and
-- checks the stronger source, selection, result, owner, and complete-chain
-- invariants against the rebuilt plans.
--
-- Certificate, node, and occurrence identifiers remain candidate-local
-- coordinates.  This atom proves no inventory membership, declaration
-- provenance, kind correctness, obligation discharge identity, behavioral
-- meaning, or fingerprint authority.
module Language.Haskell.Synthesis.Internal.TypedGenerated.Certificate.Association
  ( TypeApplicationCertificateOrigin (..)
  , TypeApplicationCertificateObservation (..)
  , TypeApplicationCertificateAssociationError (..)
  , CheckedTypeApplicationCertificateGraph
  , sealCheckedTypeApplicationCertificateGraph
  , checkedTypeApplicationCertificateGraph
  , foldCheckedTypeApplicationCertificateGraph
  ) where

import Control.DeepSeq (NFData (rnf))
import Control.Monad (foldM, unless)
import Data.Bifunctor (first)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import qualified Language.Haskell.Synthesis.Generated as Generated
import Language.Haskell.Synthesis.Internal.TypedGenerated.Certificate
  ( CheckedTypeApplicationCertificatePlan
  , CheckedTypeApplicationCertificateStep
  , CheckedTypeApplicationCertificateTable
  , TypeApplicationCertificateError (InvalidTypeApplicationCertificateType)
  , TypeApplicationCertificateLimits
  , TypeApplicationCertificateObservation (..)
  , TypeApplicationCertificatePlanVariable (TypeApplicationCertificateFree)
  , TypeApplicationCertificateSource (..)
  , TypeApplicationCertificateTypeSite
      (TypeApplicationCertificateSchemeType)
  , checkedTypeApplicationCertificateStepCount
  , checkedTypeApplicationCertificateStepResult
  , checkedTypeApplicationCertificateStepSelected
  , checkedTypeApplicationCertificateStepSlot
  , checkedTypeApplicationCertificateStepSource
  , checkedTypeApplicationCertificateSteps
  , matchCheckedTypeApplicationCertificateObservations
  , sealTypeApplicationCertificateTable
  )
import Language.Haskell.Synthesis.Name (Name)
import Language.Haskell.Synthesis.Type (Type, normalizeType)
import qualified Language.Haskell.Synthesis.TypeAtom as TypeAtom
import Language.Haskell.Synthesis.TypedGenerated
  ( CertificateId
  , OccurrenceId
  , TermGraph
  , TermGraphError
  , TermGraphLimits
  , TermGraphSource
  , TermNode (..)
  , TermNodeForm (..)
  , TermNodeId
  , TypeApplicationWitness (..)
  , TypeStructure (..)
  , lookupTermNode
  , sealTermGraph
  , termGraphRoot
  )

-- | One engine-owned independently checked global specialization.
--
-- Only the lookup identity is strict, so the certificate table's outer and
-- duplicate-coordinate gates retain their productive failure behavior before
-- inspecting owner or type payloads.
data TypeApplicationCertificateOrigin variable =
  TypeApplicationCertificateOrigin
    { typeApplicationCertificateOriginId :: !CertificateId
    , typeApplicationCertificateOriginOwner :: Name
    , typeApplicationCertificateOriginScheme :: Type variable
    , typeApplicationCertificateOriginObservations ::
        [TypeApplicationCertificateObservation variable]
    }
  deriving (Eq, Ord, Show, Generic)

instance NFData variable =>
    NFData (TypeApplicationCertificateOrigin variable)

-- | Failure of the atomic structural-table, graph, or occurrence association
-- gate. Direct type-mismatch errors retain coordinates rather than duplicating
-- compared type trees. Owner-name diagnostics and the wrapped plan/graph
-- errors preserve their existing diagnostic payloads; none of
-- these failures is a successful evidence or association channel.
data TypeApplicationCertificateAssociationError variable local
  = TypeApplicationCertificateAssociationPlanError
      (TypeApplicationCertificateError variable)
  | TypeApplicationCertificateAssociationGraphError
      (TermGraphError (Type variable) local)
  | DuplicateGraphTypeApplicationCertificateUse
      !CertificateId !Natural !TermNodeId !TermNodeId
  | UnexpectedGraphTypeApplicationCertificateUse
      !CertificateId !Natural !TermNodeId
  | MissingGraphTypeApplicationCertificateUse !CertificateId !Natural
  | MissingAssociatedSealedTermNode !TermNodeId
  | ExpectedTypeApplicationCertificateGlobalBase
      !CertificateId !TermNodeId
  | TypeApplicationCertificateGlobalOwnerMismatch
      !CertificateId !Name !Name
  | TypeApplicationCertificateGlobalSchemeMismatch !CertificateId
  | TypeApplicationCertificateChildChainMismatch
      !CertificateId !Natural !TermNodeId !TermNodeId
  | TypeApplicationCertificateChildSourceMismatch
      !CertificateId !Natural
  | TypeApplicationCertificateWitnessSourceMismatch
      !CertificateId !Natural
  | TypeApplicationCertificateVisibleArgumentMismatch
      !CertificateId !Natural
  | TypeApplicationCertificateWitnessSelectedMismatch
      !CertificateId !Natural
  | TypeApplicationCertificateNodeResultMismatch
      !CertificateId !Natural
  | TypeApplicationCertificateWitnessResultMismatch
      !CertificateId !Natural
  | TypeApplicationCertificateInternalStepArityMismatch
      !CertificateId !Int !Int
  deriving (Eq, Ord, Show, Generic)

instance (NFData variable, NFData local) =>
    NFData (TypeApplicationCertificateAssociationError variable local)

data CheckedTypeApplicationCertificateReceipt variable =
  CheckedTypeApplicationCertificateReceipt
    !TermNodeId
    !OccurrenceId
    !(CheckedTypeApplicationCertificateStep variable)

type role CheckedTypeApplicationCertificateReceipt nominal

instance NFData variable =>
    NFData (CheckedTypeApplicationCertificateReceipt variable) where
  rnf (CheckedTypeApplicationCertificateReceipt node occurrence step) =
    rnf node `seq` rnf occurrence `seq` rnf step

data CheckedTypeApplicationCertificateAssociation variable =
  CheckedTypeApplicationCertificateAssociation
    !CertificateId
    !Name
    !(Type variable)
    !TermNodeId
    !OccurrenceId
    ![CheckedTypeApplicationCertificateReceipt variable]

type role CheckedTypeApplicationCertificateAssociation nominal

instance NFData variable =>
    NFData (CheckedTypeApplicationCertificateAssociation variable) where
  rnf (CheckedTypeApplicationCertificateAssociation certificate owner scheme
      baseNode baseOccurrence receipts) =
    rnf certificate `seq` rnf owner `seq` rnf scheme `seq`
      rnf baseNode `seq` rnf baseOccurrence `seq` rnf receipts

-- | One indivisible graph/table/occurrence atom.  The table is deliberately
-- not projected.  The fold below does hand out retainable checked step
-- observations, but no caller can detach or recombine the checked table or
-- association authority from them.
data CheckedTypeApplicationCertificateGraph variable local =
  CheckedTypeApplicationCertificateGraph
    !(TermGraph (Type variable) local)
    !(CheckedTypeApplicationCertificateTable variable)
    ![CheckedTypeApplicationCertificateAssociation variable]

type role CheckedTypeApplicationCertificateGraph nominal nominal

instance (NFData variable, NFData local) =>
    NFData (CheckedTypeApplicationCertificateGraph variable local) where
  rnf (CheckedTypeApplicationCertificateGraph graph table associations) =
    rnf graph `seq` rnf table `seq` rnf associations

data PreparedOrigin variable = PreparedOrigin
  !(TypeApplicationCertificateOrigin variable)
  !(Type variable)
  !(CheckedTypeApplicationCertificatePlan variable)

data GraphCertificateUse variable = GraphCertificateUse
  !TermNodeId
  !OccurrenceId
  !TermNodeId
  !Generated.VisibleTypeArgument
  !(TypeApplicationWitness (Type variable))

-- | Build the structural table, match independent checker observations, seal
-- the raw graph, and derive one exhaustive occurrence association atomically.
--
-- The supplied t'TypeStructure' remains authoritative for all ordinary graph
-- typing and constructor schemas.  Its visible-application predicate is
-- relaxed only for a witness carrying a certificate handle; this function's
-- subsequent plan comparison is the sole route by which such a graph can be
-- returned.
sealCheckedTypeApplicationCertificateGraph
  :: (Ord variable, Ord local)
  => TypeApplicationCertificateLimits
  -> TypeStructure (Type variable)
  -> TermGraphLimits
  -> TermGraphSource (Type variable) local
  -> [TypeApplicationCertificateOrigin variable]
  -> Either
      (TypeApplicationCertificateAssociationError variable local)
      (CheckedTypeApplicationCertificateGraph variable local)
sealCheckedTypeApplicationCertificateGraph certificateLimits baseStructure
    graphLimits graphSource origins = do
  table <- first TypeApplicationCertificateAssociationPlanError $
    sealTypeApplicationCertificateTable certificateLimits $
      map certificateSource origins
  preparedOrigins <- mapM (prepareOrigin table) origins
  graph <- first TypeApplicationCertificateAssociationGraphError $
    sealTermGraph (provisionalCertificateStructure baseStructure)
      graphLimits graphSource
  rooted <- rootedTermNodes graph
  (uses, reversedUses) <- collectCertificateUses rooted
  let rootedUses = reverse reversedUses
  let originsByCertificate = Map.fromList
        [ (typeApplicationCertificateOriginId origin, prepared)
        | prepared@(PreparedOrigin origin _ _) <- preparedOrigins
        ]
  mapM_ (rejectUnknownUse originsByCertificate) rootedUses
  mapM_ (rejectOutOfRangeUse originsByCertificate) rootedUses
  let rootedCertificates = distinctRootedCertificates rootedUses
  -- Origin row order controls only deterministic failure precedence.  Once
  -- every row has succeeded, reorder the retained atoms by rooted structural
  -- occurrence so neither caller table order nor certificate allocation order
  -- leaks into the future-consumer fold.
  validatedAssociations <- mapM (associatePreparedOrigin graph uses)
    preparedOrigins
  let associationsByCertificate = Map.fromList
        [ (typeApplicationCertificateOriginId origin, association)
        | (PreparedOrigin origin _ _, association) <-
            zip preparedOrigins validatedAssociations
        ]
  rootedAssociations <- mapM
    (lookupValidatedAssociation associationsByCertificate)
    rootedCertificates
  pure $ CheckedTypeApplicationCertificateGraph
    graph table rootedAssociations
 where
  certificateSource origin = TypeApplicationCertificateSource
    (typeApplicationCertificateOriginId origin)
    (typeApplicationCertificateOriginScheme origin)
    (map typeApplicationCertificateObservationSelected $
      typeApplicationCertificateOriginObservations origin)

  prepareOrigin table origin = do
    let certificate = typeApplicationCertificateOriginId origin
    plan <- first TypeApplicationCertificateAssociationPlanError $
      matchCheckedTypeApplicationCertificateObservations certificateLimits
        certificate
        (typeApplicationCertificateOriginObservations origin)
        table
    normalizedScheme <- first
      (TypeApplicationCertificateAssociationPlanError .
        InvalidTypeApplicationCertificateType
          (TypeApplicationCertificateSchemeType certificate)) $
      normalizeType $ typeApplicationCertificateOriginScheme origin
    pure $ PreparedOrigin origin normalizedScheme plan

  lookupValidatedAssociation associations certificate = case
      Map.lookup certificate associations of
    Nothing -> Left $
      MissingGraphTypeApplicationCertificateUse certificate 0
    Just association -> Right association

-- Admit only the one witness relationship which the association immediately
-- replaces with its stronger checked plan.  Every uncertified application
-- still delegates to the caller's trusted structure unchanged.
provisionalCertificateStructure
  :: TypeStructure ty -> TypeStructure ty
provisionalCertificateStructure base = base
  { validTypeApplicationWitness = \argument witness ->
      case typeApplicationCertificate witness of
        Nothing -> validTypeApplicationWitness base argument witness
        Just _ -> True
  }

rootedTermNodes
  :: TermGraph (Type variable) local
  -> Either
      (TypeApplicationCertificateAssociationError variable local)
      [(TermNodeId, TermNode (Type variable) local)]
rootedTermNodes graph = walk $ termGraphRoot graph
 where
  walk nodeId = case lookupTermNode nodeId graph of
    Nothing -> Left $ MissingAssociatedSealedTermNode nodeId
    Just node@(TermNode _ form) -> do
      descendants <- mapM walk $ childNodes form
      pure $ (nodeId, node) : concat descendants

childNodes :: TermNodeForm ty local -> [TermNodeId]
childNodes form = case form of
  TypedLocal{} -> []
  TypedGlobal{} -> []
  TypedLambda _ body -> [body]
  TypedApply function argument _ -> [function, argument]
  TypedVisibleTypeApplication _ function _ _ -> [function]
  TypedTuple elements -> elements
  TypedHole{} -> []
  TypedLet _ binding body -> [binding, body]
  TypedCase scrutinee alternatives -> scrutinee : map snd alternatives

collectCertificateUses
  :: [(TermNodeId, TermNode (Type variable) local)]
  -> Either
      (TypeApplicationCertificateAssociationError variable local)
      ( Map (CertificateId, Natural) (GraphCertificateUse variable)
      , [((CertificateId, Natural), GraphCertificateUse variable)]
      )
collectCertificateUses = foldM collect (Map.empty, [])
 where
  collect current@(_, ordered) (nodeId, TermNode _ form) = case form of
    TypedVisibleTypeApplication occurrence function argument witness ->
      case typeApplicationCertificate witness of
        Nothing -> Right current
        Just key@(certificate, slot) -> case Map.lookup key $ fst current of
          Just (GraphCertificateUse firstNode _ _ _ _) -> Left $
            DuplicateGraphTypeApplicationCertificateUse
              certificate slot firstNode nodeId
          Nothing -> Right
            ( Map.insert key
                (GraphCertificateUse nodeId occurrence function argument witness)
                $ fst current
            , (key,
                GraphCertificateUse nodeId occurrence function argument witness)
                : ordered
            )
    TypedLocal{} -> Right current
    TypedGlobal{} -> Right current
    TypedLambda{} -> Right current
    TypedApply{} -> Right current
    TypedTuple{} -> Right current
    TypedHole{} -> Right current
    TypedLet{} -> Right current
    TypedCase{} -> Right current

rejectUnknownUse
  :: Map CertificateId (PreparedOrigin variable)
  -> ((CertificateId, Natural), GraphCertificateUse variable)
  -> Either
      (TypeApplicationCertificateAssociationError variable local)
      ()
rejectUnknownUse origins ((certificate, slot), GraphCertificateUse node _ _ _ _)
  | Map.member certificate origins = Right ()
  | otherwise = Left $
      UnexpectedGraphTypeApplicationCertificateUse certificate slot node

rejectOutOfRangeUse
  :: Map CertificateId (PreparedOrigin variable)
  -> ((CertificateId, Natural), GraphCertificateUse variable)
  -> Either
      (TypeApplicationCertificateAssociationError variable local)
      ()
rejectOutOfRangeUse origins
    ((certificate, slot), GraphCertificateUse node _ _ _ _) =
  case Map.lookup certificate origins of
    Nothing -> Left $
      UnexpectedGraphTypeApplicationCertificateUse certificate slot node
    Just (PreparedOrigin _ _ plan)
      | slot < fromIntegral
          (checkedTypeApplicationCertificateStepCount plan) -> Right ()
      | otherwise -> Left $
          UnexpectedGraphTypeApplicationCertificateUse certificate slot node

distinctRootedCertificates
  :: [((CertificateId, Natural), GraphCertificateUse variable)]
  -> [CertificateId]
distinctRootedCertificates = reverse . snd . List.foldl' collect (Set.empty, [])
 where
  collect current@(seen, reversed) ((certificate, _), _)
    | certificate `Set.member` seen = current
    | otherwise = (Set.insert certificate seen, certificate : reversed)

associatePreparedOrigin
  :: Ord variable
  => TermGraph (Type variable) local
  -> Map (CertificateId, Natural) (GraphCertificateUse variable)
  -> PreparedOrigin variable
  -> Either
      (TypeApplicationCertificateAssociationError variable local)
      (CheckedTypeApplicationCertificateAssociation variable)
associatePreparedOrigin graph uses
    (PreparedOrigin origin normalizedScheme plan) = do
  graphUses <- mapM requiredUse $
    checkedTypeApplicationCertificateSteps plan
  case graphUses of
    [] -> Left $ MissingGraphTypeApplicationCertificateUse certificate 0
    firstUse : _ -> do
      (baseNode, baseOccurrence) <- validateBase firstUse
      validateChildSources graphUses $
        checkedTypeApplicationCertificateSteps plan
      receipts <- validateSteps baseNode graphUses
        $ checkedTypeApplicationCertificateSteps plan
      pure $ CheckedTypeApplicationCertificateAssociation
        certificate
        (typeApplicationCertificateOriginOwner origin)
        normalizedScheme
        baseNode baseOccurrence receipts
 where
  certificate = typeApplicationCertificateOriginId origin

  requiredUse step =
    let slot = checkedTypeApplicationCertificateStepSlot step
    in case Map.lookup (certificate, slot) uses of
      Nothing -> Left $
        MissingGraphTypeApplicationCertificateUse certificate slot
      Just graphUse -> Right graphUse

  validateBase (GraphCertificateUse _ _ function _ _) =
    case lookupTermNode function graph of
      Nothing -> Left $ MissingAssociatedSealedTermNode function
      Just (TermNode baseType baseForm) -> case baseForm of
        TypedGlobal occurrence owner -> do
          unless (owner == typeApplicationCertificateOriginOwner origin) $
            Left $ TypeApplicationCertificateGlobalOwnerMismatch certificate
              (typeApplicationCertificateOriginOwner origin) owner
          unless (TypeAtom.alphaEquivalentTypes baseType normalizedScheme) $
            Left $ TypeApplicationCertificateGlobalSchemeMismatch certificate
          pure (function, occurrence)
        _ -> Left $
          ExpectedTypeApplicationCertificateGlobalBase certificate function

  validateSteps _ [] [] = Right []
  validateSteps expectedChild
      (graphUse@(GraphCertificateUse node occurrence function _ _) : restUses)
      (step : restSteps) = do
    let slot = checkedTypeApplicationCertificateStepSlot step
    unless (function == expectedChild) $ Left $
      TypeApplicationCertificateChildChainMismatch certificate slot
        expectedChild function
    validateStep graphUse step
    remaining <- validateSteps node restUses restSteps
    pure $ CheckedTypeApplicationCertificateReceipt
      node occurrence step : remaining
  validateSteps _ remainingUses remainingSteps = Left $
    TypeApplicationCertificateInternalStepArityMismatch certificate
      (length remainingSteps) (length remainingUses)

  validateChildSources [] [] = Right ()
  validateChildSources
      (GraphCertificateUse _ _ function _ _ : remainingUses)
      (step : remainingSteps) = do
    child <- case lookupTermNode function graph of
      Nothing -> Left $ MissingAssociatedSealedTermNode function
      Just checked -> Right checked
    let slot = checkedTypeApplicationCertificateStepSlot step
    unless (matchesPlan (termNodeType child) $
        checkedTypeApplicationCertificateStepSource step) $ Left $
      TypeApplicationCertificateChildSourceMismatch certificate slot
    validateChildSources remainingUses remainingSteps
  validateChildSources remainingUses remainingSteps = Left $
    TypeApplicationCertificateInternalStepArityMismatch certificate
      (length remainingSteps) (length remainingUses)

  validateStep (GraphCertificateUse node _ _ argument witness) step = do
    current <- case lookupTermNode node graph of
      Nothing -> Left $ MissingAssociatedSealedTermNode node
      Just checked -> Right checked
    let slot = checkedTypeApplicationCertificateStepSlot step
    unless (matchesPlan (typeApplicationSource witness) $
        checkedTypeApplicationCertificateStepSource step) $ Left $
      TypeApplicationCertificateWitnessSourceMismatch certificate slot
    unless (visibleArgumentMatches argument $
        checkedTypeApplicationCertificateStepSelected step) $ Left $
      TypeApplicationCertificateVisibleArgumentMismatch certificate slot
    unless (matchesPlan (typeApplicationSelected witness) $
        checkedTypeApplicationCertificateStepSelected step) $ Left $
      TypeApplicationCertificateWitnessSelectedMismatch certificate slot
    unless (matchesPlan (termNodeType current) $
        checkedTypeApplicationCertificateStepResult step) $ Left $
      TypeApplicationCertificateNodeResultMismatch certificate slot
    unless (matchesPlan (typeApplicationResult witness) $
        checkedTypeApplicationCertificateStepResult step) $ Left $
      TypeApplicationCertificateWitnessResultMismatch certificate slot

  matchesPlan actual expected = TypeAtom.alphaEquivalentTypes
    (fmap TypeApplicationCertificateFree actual) expected

  visibleArgumentMatches argument expected = case
      Generated.visibleTypeArgumentClosedType argument of
    Nothing -> False
    Just selected -> TypeAtom.alphaEquivalentClosedTypes expected selected

-- | Project a bare legacy graph observation.
--
-- This projection loses association authority. Certificate handles retained
-- in the returned graph remain coordinates and confer no authority on their
-- own; a consumer which needs the checked association must retain and consume
-- the opaque atom instead.
checkedTypeApplicationCertificateGraph
  :: CheckedTypeApplicationCertificateGraph variable local
  -> TermGraph (Type variable) local
checkedTypeApplicationCertificateGraph
    (CheckedTypeApplicationCertificateGraph graph _ _) = graph

-- | Consume verified associations in rooted structural preorder.
--
-- The callback receives observations of the exact normalized owner scheme,
-- derived base global receipt, canonical plan steps, and complete source-order
-- visible receipt chain.  Those observations may be retained, but no checked
-- table or graph-association carrier can be detached and recombined through
-- this interface.  Consumers must still treat every identifier and the order
-- itself as candidate-local coordinates, never as semantic provenance or a
-- fingerprint field.
foldCheckedTypeApplicationCertificateGraph
  :: ( accumulator
       -> CertificateId
       -> Name
       -> Type variable
       -> TermNodeId
       -> OccurrenceId
       -> [( TermNodeId
           , OccurrenceId
           , CheckedTypeApplicationCertificateStep variable
           )]
       -> accumulator
     )
  -> accumulator
  -> CheckedTypeApplicationCertificateGraph variable local
  -> accumulator
foldCheckedTypeApplicationCertificateGraph consume initial
    (CheckedTypeApplicationCertificateGraph _ _ associations) =
  List.foldl' consumeAssociation initial associations
 where
  consumeAssociation accumulator
      (CheckedTypeApplicationCertificateAssociation certificate owner scheme
        baseNode baseOccurrence receipts) =
    consume accumulator certificate owner scheme baseNode baseOccurrence
      [ (node, occurrence, step)
      | CheckedTypeApplicationCertificateReceipt node occurrence step <-
          receipts
      ]
