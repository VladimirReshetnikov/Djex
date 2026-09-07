{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}

-- | A bounded, typed candidate graph beside the compatibility generated AST.
--
-- Search backends historically erased their typed proof terms directly into
-- 'Generated.Expression'.  Rich frontends then had to reconstruct types and
-- source occurrence identity from that tree.  This module is the shared
-- checked edge for retaining those facts without changing the legacy AST or
-- candidate constructors.
--
-- The raw graph is intentionally caller-constructible.  'sealTermGraph'
-- bounds and validates it, rejects duplicate, dangling, cyclic, and repeated
-- references, checks the neutral typing relationships, and stores the one
-- checked compatibility projection. Repeated references are rejected because
-- legacy tree erasure would otherwise duplicate one source occurrence
-- identity into several surface uses. Explicit 'TypedLet' nodes represent
-- semantic sharing. Consumers of a sealed t'TermGraph' therefore do not need
-- their own graph walk or type recovery pass.
module Language.Haskell.Synthesis.TypedGenerated
  ( TermNodeId
  , termNodeId
  , termNodeIdValue
  , OccurrenceId
  , occurrenceId
  , occurrenceIdValue
  , CertificateId
  , certificateId
  , certificateIdValue
  , TermGraphLimits
  , TermGraphLimitError (..)
  , mkTermGraphLimits
  , defaultTermGraphLimits
  , maximumTermGraphNodes
  , maximumTermGraphEdges
  , maximumTermGraphPatternNodes
  , maximumTermGraphTypeNodes
  , maximumTermGraphCollectionWidth
  , maximumTermGraphProjectionNodes
  , TypeStructureLimitError (..)
  , TypeStructure (..)
  , sharedTypeStructure
  , ForallTypeStructure (..)
  , sharedForallTypeStructure
  , sharedContextualForallTypeStructure
  , ContextTypeStructure (..)
  , sharedContextTypeStructure
  , EvidenceBinderId
  , evidenceBinderIntroduction
  , evidenceBinderSlot
  , ContextEvidence
  , givenContextEvidence
  , contextEvidenceBinder
  , ContextIntroductionWitness
  , contextIntroductionWitness
  , contextIntroductionSource
  , contextIntroductionConstraints
  , contextIntroductionBody
  , ContextApplicationWitness
  , contextApplicationWitness
  , contextApplicationSource
  , contextApplicationConstraints
  , contextApplicationEvidence
  , contextApplicationResult
  , ApplicationWitness (..)
  , TypeApplicationWitness (..)
  , ForallIntroductionWitness (..)
  , ImplicitTypeApplicationWitness (..)
  , TypedPattern (..)
  , TypedPatternNode (..)
  , TermNode (..)
  , TermNodeForm (..)
  , TermGraphSource (..)
  , GraphCollectionSite (..)
  , GraphTypeSite (..)
  , TermGraphError (..)
  , TypedGraphMetrics (..)
  , TermGraph
  , sealTermGraph
  , sealTermGraphWithContext
  , termGraphRoot
  , termGraphNodes
  , lookupTermNode
  , termGraphMetrics
  , eraseTermGraph
  , eraseTermGraphToFunctionClause
  ) where

import Control.DeepSeq (NFData (rnf))
import Control.Monad (foldM, unless, when)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Either (isRight)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Collection (observedListLength)
import Language.Haskell.Synthesis.Constraint (Constraint (..), constraintArguments)
import qualified Language.Haskell.Synthesis.Generated as Generated
import Language.Haskell.Synthesis.Name (Boxity (Boxed), Name)
import qualified Language.Haskell.Synthesis.Type as SharedType
import qualified Language.Haskell.Synthesis.TypeAtom as TypeAtom
import Language.Haskell.Synthesis.Count
  ( saturatedSuccessor
  )

-- | Stable identity for a search-created term node in one sealed query.
newtype TermNodeId = TermNodeId Natural
  deriving (Eq, Ord, Show, NFData)

-- | Allocate a term-node identity from a frontend or backend-owned counter.
--
-- Identity uniqueness is a graph invariant checked by 'sealTermGraph', not a
-- property of one number in isolation.
-- | Wrap a raw allocation number as a node identity.
termNodeId :: Natural -> TermNodeId
termNodeId = TermNodeId

-- | Inspect the allocation number without deriving semantic meaning from it.
termNodeIdValue :: TermNodeId -> Natural
termNodeIdValue (TermNodeId value) = value

-- | Stable source occurrence identity within one sealed query.
--
-- Occurrences deliberately remain distinct even when their terms or types are
-- alpha-equivalent.  Source metadata must key on this identity rather than on
-- a rendered name.
newtype OccurrenceId = OccurrenceId Natural
  deriving (Eq, Ord, Show, NFData)

-- | Wrap a raw allocation number as an occurrence identity.
occurrenceId :: Natural -> OccurrenceId
occurrenceId = OccurrenceId

-- | Inspect the allocation number without deriving semantic meaning from it.
occurrenceIdValue :: OccurrenceId -> Natural
occurrenceIdValue (OccurrenceId value) = value

-- | Stable handle for source-certified specialization evidence.
--
-- The generalized certificate table is deliberately a later layer.  The
-- typed spine retains the handle now so visible specializations never need to
-- be associated with evidence by spelling or traversal position.
newtype CertificateId = CertificateId Natural
  deriving (Eq, Ord, Show, NFData)

-- | Wrap a raw allocation number as a certificate handle.
certificateId :: Natural -> CertificateId
certificateId = CertificateId

-- | Inspect the allocation number without deriving semantic meaning from it.
certificateIdValue :: CertificateId -> Natural
certificateIdValue (CertificateId value) = value

-- | Finite resources used while sealing a caller-built graph.
--
-- The graph-node bound controls stored semantic nodes.  Pattern nodes are
-- separate because one elimination pattern may expose many typed fields. The
-- type-node bound applies independently to every stored annotation, while the
-- collection-width bound covers graph, pattern, and type-owned lists. The
-- projection bound counts both term and pattern nodes in the compatibility
-- tree.
data TermGraphLimits = TermGraphLimits
  { maximumTermGraphNodes :: !Int
  , maximumTermGraphEdges :: !Int
  , maximumTermGraphPatternNodes :: !Int
  , maximumTermGraphTypeNodes :: !Int
  , maximumTermGraphCollectionWidth :: !Int
  , maximumTermGraphProjectionNodes :: !Int
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData TermGraphLimits

-- | Rejection of a limit table by 'mkTermGraphLimits': the named bound was
-- supplied with the recorded negative value.  Bounds are checked in field
-- order and only the first offender is reported.
data TermGraphLimitError
  = NegativeTermGraphNodeLimit Int
  | NegativeTermGraphEdgeLimit Int
  | NegativeTermGraphPatternLimit Int
  | NegativeTermGraphTypeNodeLimit Int
  | NegativeTermGraphCollectionWidth Int
  | NegativeTermGraphProjectionLimit Int
  deriving (Eq, Ord, Show, Generic)

instance NFData TermGraphLimitError

-- | Validate custom graph limits.  Zero is useful for focused fail-closed
-- tests; ordinary callers should use 'defaultTermGraphLimits'.
mkTermGraphLimits
  :: Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Either TermGraphLimitError TermGraphLimits
mkTermGraphLimits nodes edges patterns typeNodes width projection
  | nodes < 0 = Left $ NegativeTermGraphNodeLimit nodes
  | edges < 0 = Left $ NegativeTermGraphEdgeLimit edges
  | patterns < 0 = Left $ NegativeTermGraphPatternLimit patterns
  | typeNodes < 0 = Left $ NegativeTermGraphTypeNodeLimit typeNodes
  | width < 0 = Left $ NegativeTermGraphCollectionWidth width
  | projection < 0 = Left $ NegativeTermGraphProjectionLimit projection
  | otherwise = Right TermGraphLimits
      { maximumTermGraphNodes = nodes
      , maximumTermGraphEdges = edges
      , maximumTermGraphPatternNodes = patterns
      , maximumTermGraphTypeNodes = typeNodes
      , maximumTermGraphCollectionWidth = width
      , maximumTermGraphProjectionNodes = projection
      }

-- | Compatibility defaults for one complete candidate graph.
--
-- These limits are intentionally independent of search cutoffs: a backend may
-- stream many small candidates without retaining their graphs together.
defaultTermGraphLimits :: TermGraphLimits
defaultTermGraphLimits = TermGraphLimits
  { maximumTermGraphNodes = 4096
  , maximumTermGraphEdges = 16384
  , maximumTermGraphPatternNodes = 4096
  , maximumTermGraphTypeNodes = 4096
  , maximumTermGraphCollectionWidth = 256
  , maximumTermGraphProjectionNodes = 16384
  }

-- | The two neutral type observations needed by the first typed spine.
--
-- Type application carries its own checked witness because validating source
-- substitution requires the later certificate table.  Constructor-pattern
-- field schemas similarly remain source-certified until family descriptors
-- are introduced.
data TypeStructureLimitError
  = TypeStructureNodeLimitExceeded Int
  | TypeStructureCollectionLimitExceeded Int
  deriving (Eq, Ord, Show, Generic)

instance NFData TypeStructureLimitError

-- | The type-language operations consulted by 'sealTermGraph', abstracted
-- over the annotation type @ty@: equivalence, bounded structural observation,
-- annotation validity, function and tuple decomposition, constructor field
-- schemas, and visible-type-application witness checking.
-- 'sharedTypeStructure' instantiates it for the shared synthesis type.
data TypeStructure ty = TypeStructure
  { equivalentTypes :: ty -> ty -> Bool
  , observeTypeWithin
      :: Int -> Int -> ty -> Either TypeStructureLimitError ()
    -- ^ Bound type nodes and each type-owned collection width, in that order.
  , validTypeAnnotation :: ty -> Bool
  , functionTypeComponents :: ty -> Maybe (ty, ty)
  , tupleTypeComponents :: ty -> Maybe [ty]
  , constructorPatternFieldTypes :: Name -> ty -> Maybe [ty]
  , validTypeApplicationWitness
      :: Generated.VisibleTypeArgument -> TypeApplicationWitness ty -> Bool
  , forallTypeStructure :: Maybe (ForallTypeStructure ty)
    -- ^ Explicit authority for erased quantifier rules. The ordinary shared
    -- structure supplies no such authority; existing callers remain unchanged.
  }

-- | Source-type observations needed for erased quantifier rules. These
-- callbacks must compare exact free-variable identities, expose every free
-- variable, and recognize only fresh rigid variables as introduction binders.
-- The sealer independently checks their lexical scope throughout the graph.
data ForallTypeStructure ty = ForallTypeStructure
  { forallFreeTypeVariables :: ty -> [ty]
  , validForallIntroductionWitness :: ForallIntroductionWitness ty -> Bool
  , validImplicitTypeApplicationWitness
      :: ImplicitTypeApplicationWitness ty -> Bool
  }

-- | Constraint-free System F rules over shared source types. Opening requires
-- a rigid variable; flexible inference variables are never generalization
-- evidence. Constraint-bearing foralls require dictionary evidence and are
-- deliberately not admitted by this structural authority.
sharedForallTypeStructure
  :: Ord identity
  => ForallTypeStructure (SharedType.Type (SharedType.Variable identity))
sharedForallTypeStructure = ForallTypeStructure
  { forallFreeTypeVariables = map SharedType.TypeVariable
      . SharedType.freeVariablesInFirstOccurrenceOrder
  , validForallIntroductionWitness = \witness ->
      case forallIntroductionVariable witness of
        variable@(SharedType.TypeVariable (SharedType.RigidVariable _)) ->
          constraintFreeLeadingForall (forallIntroductionSource witness)
            && TypeAtom.isLeadingForallInstantiation
                (forallIntroductionSource witness) variable
                (forallIntroductionBody witness)
        _ -> False
  , validImplicitTypeApplicationWitness = \witness ->
      constraintFreeLeadingForall (implicitTypeApplicationSource witness)
        && TypeAtom.isLeadingForallInstantiation
            (implicitTypeApplicationSource witness)
            (implicitTypeApplicationSelected witness)
            (implicitTypeApplicationResult witness)
  }
 where
  constraintFreeLeadingForall source = case source of
    SharedType.ForallType (_ : _) [] _ -> True
    _ -> False

-- | Quantifier rules which preserve a qualified layer after substituting its
-- type binders. This grants no dictionary discharge: the resulting contextual
-- type must be consumed separately by a checked context application.
sharedContextualForallTypeStructure
  :: Ord identity
  => ForallTypeStructure (SharedType.Type (SharedType.Variable identity))
sharedContextualForallTypeStructure = sharedForallTypeStructure
  { validForallIntroductionWitness = \witness ->
      case forallIntroductionVariable witness of
        variable@(SharedType.TypeVariable (SharedType.RigidVariable _)) ->
          TypeAtom.isLeadingForallInstantiation
            (forallIntroductionSource witness) variable
            (forallIntroductionBody witness)
        _ -> False
  , validImplicitTypeApplicationWitness = \witness ->
      TypeAtom.isLeadingForallInstantiation
        (implicitTypeApplicationSource witness)
        (implicitTypeApplicationSelected witness)
        (implicitTypeApplicationResult witness)
  }

-- | Opt-in source-type observations for lexical dictionary evidence. Only one
-- binderless, nonempty qualified layer is exposed; type binders and a later
-- qualified layer are not consumed by this operation. The sealer independently
-- rechecks this observation and bounds its output before traversing it.
newtype ContextTypeStructure ty = ContextTypeStructure
  { contextTypeComponents :: ty -> Maybe ([Constraint ty], ty) }

sharedContextTypeStructure :: ContextTypeStructure (SharedType.Type variable)
sharedContextTypeStructure = ContextTypeStructure $ \source -> case source of
  SharedType.ForallType [] constraints@(_ : _) body -> Just (constraints, body)
  _ -> Nothing

-- | A dictionary binder is identified by its actual introduction occurrence
-- and its ordered source-telescope slot. An arbitrary number cannot introduce
-- a given: only a validated context-introduction node creates these bindings.
data EvidenceBinderId = EvidenceBinderId !OccurrenceId !Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData EvidenceBinderId

evidenceBinderIntroduction :: EvidenceBinderId -> OccurrenceId
evidenceBinderIntroduction (EvidenceBinderId occurrence _) = occurrence

evidenceBinderSlot :: EvidenceBinderId -> Natural
evidenceBinderSlot (EvidenceBinderId _ slot) = slot

-- | An untrusted reference to one lexical given. Instance and superclass
-- derivations are deliberately absent until their source authority is retained.
newtype ContextEvidence = ContextGiven EvidenceBinderId
  deriving (Eq, Ord, Show, NFData)

-- | Reference an introduction/slot, without granting authority to that pair.
-- Sealing must find the exact source-derived binder in the current scope.
givenContextEvidence :: OccurrenceId -> Natural -> ContextEvidence
givenContextEvidence occurrence = ContextGiven . EvidenceBinderId occurrence

contextEvidenceBinder :: ContextEvidence -> EvidenceBinderId
contextEvidenceBinder (ContextGiven binder) = binder

-- The constructors stay private: a checker builds witnesses from a source
-- qualified type, never an independently claimed constraint/body tuple.
data ContextIntroductionWitness ty = ContextIntroductionWitness
  ty [Constraint ty] ty
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData ty => NFData (ContextIntroductionWitness ty)

contextIntroductionWitness
  :: ContextTypeStructure ty -> ty -> Maybe (ContextIntroductionWitness ty)
contextIntroductionWitness structure source = do
  (constraints, body) <- contextTypeComponents structure source
  pure $ ContextIntroductionWitness source constraints body

contextIntroductionSource :: ContextIntroductionWitness ty -> ty
contextIntroductionSource (ContextIntroductionWitness source _ _) = source

contextIntroductionConstraints :: ContextIntroductionWitness ty -> [Constraint ty]
contextIntroductionConstraints (ContextIntroductionWitness _ constraints _) = constraints

contextIntroductionBody :: ContextIntroductionWitness ty -> ty
contextIntroductionBody (ContextIntroductionWitness _ _ body) = body

data ContextApplicationWitness ty = ContextApplicationWitness
  ty [Constraint ty] [ContextEvidence] ty
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData ty => NFData (ContextApplicationWitness ty)

contextApplicationWitness
  :: ContextTypeStructure ty -> ty -> [ContextEvidence]
  -> Maybe (ContextApplicationWitness ty)
contextApplicationWitness structure source evidence = do
  (constraints, body) <- contextTypeComponents structure source
  pure $ ContextApplicationWitness source constraints evidence body

contextApplicationSource :: ContextApplicationWitness ty -> ty
contextApplicationSource (ContextApplicationWitness source _ _ _) = source

contextApplicationConstraints :: ContextApplicationWitness ty -> [Constraint ty]
contextApplicationConstraints (ContextApplicationWitness _ constraints _ _) = constraints

contextApplicationEvidence :: ContextApplicationWitness ty -> [ContextEvidence]
contextApplicationEvidence (ContextApplicationWitness _ _ evidence _) = evidence

contextApplicationResult :: ContextApplicationWitness ty -> ty
contextApplicationResult (ContextApplicationWitness _ _ _ result) = result

-- | Structural observations for the shared synthesis type language.
-- Checked source types compare modulo lexical forall-binder spelling while
-- retaining nominal free-variable identity.
sharedTypeStructure
  :: Ord variable
  => TypeStructure (SharedType.Type variable)
sharedTypeStructure = TypeStructure
  { equivalentTypes = TypeAtom.alphaEquivalentTypes
  , observeTypeWithin = observeSharedTypeWithin
  , validTypeAnnotation = isRight . SharedType.validateType
  , functionTypeComponents = \ty -> case ty of
      SharedType.FunctionType domain result -> Just (domain, result)
      _ -> Nothing
  , tupleTypeComponents = \ty -> case ty of
      SharedType.TupleType Boxed fields -> Just fields
      _ -> Nothing
  , constructorPatternFieldTypes = \_ _ -> Nothing
  , validTypeApplicationWitness = \argument witness ->
      TypeAtom.isLeadingForallInstantiation
        (typeApplicationSource witness)
        (typeApplicationSelected witness)
        (typeApplicationResult witness)
        && Generated.visibleTypeArgumentMatches argument (typeApplicationSelected witness)
  , forallTypeStructure = Nothing
  }

observeSharedTypeWithin
  :: Int
  -> Int
  -> SharedType.Type variable
  -> Either TypeStructureLimitError ()
observeSharedTypeWithin maximumNodes maximumWidth source =
  () <$ inspect 0 source
 where
  inspect !count ty
    | count >= maximumNodes = Left $ TypeStructureNodeLimitExceeded
        $ saturatedSuccessor maximumNodes
    | otherwise = case ty of
        SharedType.TypeVariable{} -> Right next
        SharedType.TypeConstructor{} -> Right next
        SharedType.TypeApplication function argument ->
          inspect next function >>= (`inspect` argument)
        SharedType.FunctionType domain result ->
          inspect next domain >>= (`inspect` result)
        SharedType.TupleType _ fields -> do
          observeCollection fields
          foldM inspect next fields
        SharedType.ForallType binders constraints body -> do
          observeCollection binders
          observeCollection constraints
          afterConstraints <- foldM inspectConstraint next constraints
          inspect afterConstraints body
   where
    next = count + 1

  inspectConstraint !count constraint = do
    let arguments = constraintArguments constraint
    observeCollection arguments
    foldM inspect count arguments

  observeCollection values =
    let observed = observedListLength maximumWidth values
    in if observed <= maximumWidth
        then Right ()
        else Left $ TypeStructureCollectionLimitExceeded observed

-- | Exact neutral domain and result used for one term application.
data ApplicationWitness ty = ApplicationWitness
  { applicationDomain :: ty
  , applicationResult :: ty
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData ty => NFData (ApplicationWitness ty)

-- | Checked before/after types and optional source certificate for one visible
-- type application.
--
-- A missing certificate is permitted for compatibility while backends are
-- migrated.  Source-sensitive provider specializations should always retain
-- the exact t'CertificateId'; later query sealing will make that relationship
-- mandatory for certificate-owned occurrences.
data TypeApplicationWitness ty = TypeApplicationWitness
  { typeApplicationSource :: ty
    -- ^ Exact quantified type before consuming one leading binder.
  , typeApplicationSelected :: ty
    -- ^ Exact type selected for that binder, including an impredicative type.
  , typeApplicationResult :: ty
    -- ^ Capture-avoiding result after consuming the binder.
  , typeApplicationCertificate :: Maybe (CertificateId, Natural)
    -- ^ Certificate identity and its source-telescope slot.
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData ty => NFData (TypeApplicationWitness ty)

-- | One erased forall introduction. The child is checked at the exact opened
-- body type; the containing node has the original quantified source type.
data ForallIntroductionWitness ty = ForallIntroductionWitness
  { forallIntroductionSource :: ty
  , forallIntroductionVariable :: ty
  , forallIntroductionBody :: ty
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData ty => NFData (ForallIntroductionWitness ty)

-- | One inferred instantiation, erased from the compatibility syntax. Unlike
-- visible applications this cannot own or borrow a source-VTA certificate.
data ImplicitTypeApplicationWitness ty = ImplicitTypeApplicationWitness
  { implicitTypeApplicationSource :: ty
  , implicitTypeApplicationSelected :: ty
  , implicitTypeApplicationResult :: ty
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData ty => NFData (ImplicitTypeApplicationWitness ty)

-- | One typed pattern node.  Its occurrence identifies the exact source or
-- generated binding/elimination site independently of local spelling.
-- A 'TypedBind' or 'TypedAs' local identity is unique across the whole sealed
-- graph, including disjoint branches. Backends must allocate a fresh identity
-- for source-level shadowing; this makes every 'TypedLocal' annotation
-- unambiguous without recovering a lexical binder during type checking.
data TypedPattern ty local = TypedPattern
  { typedPatternOccurrence :: !OccurrenceId
  , typedPatternType :: ty
  , typedPatternNode :: TypedPatternNode ty local
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (NFData ty, NFData local) => NFData (TypedPattern ty local)

-- | The shape of one t'TypedPattern': a binder, a wildcard, a constructor or
-- tuple pattern over typed sub-patterns, or an as-pattern.  'TypedBind' and
-- 'TypedAs' introduce a local whose identity must be unique across the sealed
-- graph.
data TypedPatternNode ty local
  = TypedBind local
  | TypedWildcard
  | TypedConstructor Name [TypedPattern ty local]
  | TypedTuplePattern [TypedPattern ty local]
  | TypedAs local (TypedPattern ty local)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (NFData ty, NFData local) => NFData (TypedPatternNode ty local)

-- | One typed term node stored under a separate t'TermNodeId'.
data TermNode ty local = TermNode
  { termNodeType :: ty
  , termNodeForm :: TermNodeForm ty local
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (NFData ty, NFData local) => NFData (TermNode ty local)

-- | Typed counterparts of every current compatibility expression form.
--
-- Locals, globals, holes, and pattern sites carry stable occurrence identity.
-- Search-created structural nodes use their containing t'TermNodeId'.
data TermNodeForm ty local
  = TypedLocal !OccurrenceId local
  | TypedGlobal !OccurrenceId Name
  | TypedLambda [TypedPattern ty local] !TermNodeId
  | TypedApply !TermNodeId !TermNodeId (ApplicationWitness ty)
  | TypedVisibleTypeApplication
      !OccurrenceId
      !TermNodeId
      Generated.VisibleTypeArgument
      (TypeApplicationWitness ty)
  | TypedForallIntroduction
      !OccurrenceId !TermNodeId (ForallIntroductionWitness ty)
  | TypedImplicitTypeApplication
      !OccurrenceId !TermNodeId (ImplicitTypeApplicationWitness ty)
  | TypedContextIntroduction
      !OccurrenceId !TermNodeId (ContextIntroductionWitness ty)
  | TypedContextApplication
      !OccurrenceId !TermNodeId (ContextApplicationWitness ty)
  | TypedTuple [TermNodeId]
  | TypedHole !OccurrenceId local
  | TypedLet (TypedPattern ty local) !TermNodeId !TermNodeId
  | TypedCase !TermNodeId [(TypedPattern ty local, TermNodeId)]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (NFData ty, NFData local) => NFData (TermNodeForm ty local)

-- | Untrusted graph input.  Node order is retained for stable diagnostics and
-- serialization; it has no semantic effect.
data TermGraphSource ty local = TermGraphSource
  { termGraphSourceRoot :: !TermNodeId
  , termGraphSourceNodes :: [(TermNodeId, TermNode ty local)]
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (NFData ty, NFData local) => NFData (TermGraphSource ty local)

-- | Which graph-owned list exceeded 'maximumTermGraphNodes' (the node table)
-- or 'maximumTermGraphCollectionWidth' (a lambda's patterns, a tuple's
-- elements, a case's alternatives, or a constructor or tuple pattern's
-- fields), as reported by 'TermGraphCollectionLimitExceeded'.
data GraphCollectionSite
  = GraphNodeTable
  | LambdaPatternList TermNodeId
  | TupleElementList TermNodeId
  | CaseAlternativeList TermNodeId
  | ConstructorPatternFieldList OccurrenceId
  | TuplePatternFieldList OccurrenceId
  | ContextConstraintList TermNodeId
  | ContextConstraintArgumentList TermNodeId Int
  | ContextEvidenceList TermNodeId
  deriving (Eq, Ord, Show, Generic)

instance NFData GraphCollectionSite

-- | Exact annotation site used by bounded type diagnostics.
data GraphTypeSite
  = GraphTermNodeType TermNodeId
  | GraphPatternType OccurrenceId
  | GraphApplicationDomainType TermNodeId
  | GraphApplicationResultType TermNodeId
  | GraphTypeApplicationSourceType TermNodeId
  | GraphTypeApplicationSelectedType TermNodeId
  | GraphTypeApplicationResultType TermNodeId
  | GraphContextSourceType TermNodeId
  | GraphContextResultType TermNodeId
  | GraphContextConstraintArgumentType TermNodeId Int Int
  deriving (Eq, Ord, Show, Generic)

instance NFData GraphTypeSite

-- | Focused rejection from the sealed typed-candidate boundary.
data TermGraphError ty local
  = TermGraphCollectionLimitExceeded
      GraphCollectionSite Int Int
  | TermGraphEdgeLimitExceeded Int Int
  | TermGraphPatternLimitExceeded Int Int
  | TermGraphTypeNodeLimitExceeded GraphTypeSite Int Int
  | TermGraphTypeCollectionLimitExceeded GraphTypeSite Int Int
  | InvalidTermGraphTypeAnnotation GraphTypeSite ty
  | DuplicateTermNodeId TermNodeId
  | MissingTermGraphRoot TermNodeId
  | DanglingTermNodeReference TermNodeId TermNodeId
  | RepeatedTermNodeReference TermNodeId TermNodeId TermNodeId
    -- ^ Referenced node, first parent, and repeated parent. A repeated parent
    -- may equal the first when one collection names the child twice.
  | CyclicTermNodeReference [TermNodeId]
  | UnreachableTermNode TermNodeId
  | DuplicateOccurrenceId OccurrenceId
  | DuplicateTypedLocalBinder local
  | UnboundTypedLocal TermNodeId local
  | TypedLocalTypeMismatch TermNodeId local ty ty
  | ExpectedFunctionType TermNodeId ty
  | LambdaDomainTypeMismatch TermNodeId OccurrenceId ty ty
  | LambdaResultTypeMismatch TermNodeId ty ty
  | ApplicationDomainTypeMismatch TermNodeId ty ty
  | ApplicationArgumentTypeMismatch TermNodeId ty ty
  | ApplicationResultTypeMismatch TermNodeId ty ty
  | VisibleTypeApplicationSourceMismatch TermNodeId ty ty
  | VisibleTypeApplicationResultMismatch TermNodeId ty ty
  | InvalidVisibleTypeApplicationWitness
      TermNodeId Generated.VisibleTypeArgument (TypeApplicationWitness ty)
  | ErasedForallTypeStructureUnavailable TermNodeId
  | InvalidForallIntroductionWitness TermNodeId (ForallIntroductionWitness ty)
  | ForallIntroductionSourceMismatch TermNodeId ty ty
  | ForallIntroductionBodyMismatch TermNodeId ty ty
  | InvalidImplicitTypeApplicationWitness
      TermNodeId (ImplicitTypeApplicationWitness ty)
  | ImplicitTypeApplicationSourceMismatch TermNodeId ty ty
  | ImplicitTypeApplicationResultMismatch TermNodeId ty ty
  | DuplicateForallIntroductionVariable TermNodeId TermNodeId ty
  | ForallIntroductionVariableEscapes TermNodeId ty
  | ForallIntroductionVariableInGlobal TermNodeId ty
  | ContextTypeStructureUnavailable TermNodeId
  | InvalidContextSource TermNodeId ty
  | ContextSourceMismatch TermNodeId ty ty
  | ContextResultMismatch TermNodeId ty ty
  | ContextConstraintMismatch TermNodeId [Constraint ty] [Constraint ty]
  | ContextEvidenceArityMismatch TermNodeId Int Int
  | UnboundContextEvidence TermNodeId EvidenceBinderId
  | ContextGivenTypeMismatch TermNodeId EvidenceBinderId (Constraint ty) (Constraint ty)
  | TermGraphContextEvidenceLimitExceeded Int Int
  | ExpectedTupleType TermNodeId ty
  | TupleArityTypeMismatch TermNodeId Int Int
  | TupleFieldTypeMismatch TermNodeId Int ty ty
  | LetPatternTypeMismatch TermNodeId ty ty
  | LetResultTypeMismatch TermNodeId ty ty
  | CasePatternTypeMismatch TermNodeId OccurrenceId ty ty
  | CaseResultTypeMismatch TermNodeId ty ty
  | ExpectedTuplePatternType OccurrenceId ty
  | TuplePatternArityTypeMismatch OccurrenceId Int Int
  | TuplePatternFieldTypeMismatch OccurrenceId Int ty ty
  | AsPatternTypeMismatch OccurrenceId ty ty
  | UnknownConstructorPatternSchema OccurrenceId Name ty
  | ConstructorPatternArityTypeMismatch OccurrenceId Int Int
  | ConstructorPatternFieldTypeMismatch OccurrenceId Int ty ty
  | TermGraphProjectionLimitExceeded Int Int
  | ProjectedExpressionScopeError (Generated.ScopeError local)
  | ProjectedExpressionSyntaxError Generated.RenderError
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (NFData ty, NFData local) => NFData (TermGraphError ty local)

-- | Deterministic semantic work at the typed candidate edge.
--
-- The record is additive so engine/query observability can aggregate it
-- without inspecting graph internals.
data TypedGraphMetrics = TypedGraphMetrics
  { typedGraphTermNodes :: !Natural
  , typedGraphEdges :: !Natural
  , typedGraphPatternNodes :: !Natural
  , typedGraphSourceOccurrences :: !Natural
  , typedGraphLocalUses :: !Natural
  , typedGraphGlobalUses :: !Natural
  , typedGraphApplications :: !Natural
  , typedGraphVisibleTypeApplications :: !Natural
  , typedGraphTuples :: !Natural
  , typedGraphHoles :: !Natural
  , typedGraphLets :: !Natural
  , typedGraphCases :: !Natural
  , typedGraphCaseBranches :: !Natural
  , typedGraphProjectedNodes :: !Natural
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData TypedGraphMetrics

instance Semigroup TypedGraphMetrics where
  left <> right = TypedGraphMetrics
    { typedGraphTermNodes = add typedGraphTermNodes
    , typedGraphEdges = add typedGraphEdges
    , typedGraphPatternNodes = add typedGraphPatternNodes
    , typedGraphSourceOccurrences = add typedGraphSourceOccurrences
    , typedGraphLocalUses = add typedGraphLocalUses
    , typedGraphGlobalUses = add typedGraphGlobalUses
    , typedGraphApplications = add typedGraphApplications
    , typedGraphVisibleTypeApplications = add
        typedGraphVisibleTypeApplications
    , typedGraphTuples = add typedGraphTuples
    , typedGraphHoles = add typedGraphHoles
    , typedGraphLets = add typedGraphLets
    , typedGraphCases = add typedGraphCases
    , typedGraphCaseBranches = add typedGraphCaseBranches
    , typedGraphProjectedNodes = add typedGraphProjectedNodes
    }
   where
    add field = field left + field right

instance Monoid TypedGraphMetrics where
  mempty = TypedGraphMetrics 0 0 0 0 0 0 0 0 0 0 0 0 0 0

-- | A sealed finite graph and its checked one-way compatibility projection.
-- The constructor is private so these views cannot drift apart.
data TermGraph ty local = TermGraph
  !TermNodeId
  [(TermNodeId, TermNode ty local)]
  !(Map TermNodeId (TermNode ty local))
  (Generated.Expression local)
  !TypedGraphMetrics
  deriving (Eq, Ord, Show)

type role TermGraph nominal nominal

instance (NFData ty, NFData local) => NFData (TermGraph ty local) where
  rnf (TermGraph root nodes nodeMap expression metrics) =
    rnf root `seq`
      rnf nodes `seq`
        rnf nodeMap `seq`
          rnf expression `seq`
            rnf metrics

-- | The node at which evaluation of the sealed term begins.
termGraphRoot :: TermGraph ty local -> TermNodeId
termGraphRoot (TermGraph root _ _ _ _) = root

-- | Every sealed node in allocation order, dead nodes included.
termGraphNodes :: TermGraph ty local -> [(TermNodeId, TermNode ty local)]
termGraphNodes (TermGraph _ nodes _ _ _) = nodes

-- | Resolve one node identity within this graph; 'Nothing' for a foreign or
-- out-of-range identity.
lookupTermNode
  :: TermNodeId
  -> TermGraph ty local
  -> Maybe (TermNode ty local)
lookupTermNode nodeId' (TermGraph _ _ nodes _ _) = Map.lookup nodeId' nodes

-- | The size and shape observations recorded while the graph was sealed.
termGraphMetrics :: TermGraph ty local -> TypedGraphMetrics
termGraphMetrics (TermGraph _ _ _ _ metrics) = metrics

-- | The compatibility expression checked while the graph was sealed.
eraseTermGraph :: TermGraph ty local -> Generated.Expression local
eraseTermGraph (TermGraph _ _ _ expression _) = expression

-- | Project the sealed graph to one legacy top-level clause.  Leading typed
-- lambdas become clause patterns through the compatibility AST's sole
-- canonical conversion.
eraseTermGraphToFunctionClause
  :: Generated.DefinitionName
  -> TermGraph ty local
  -> Generated.FunctionClause local
eraseTermGraphToFunctionClause name =
  Generated.functionClauseFromExpression name . eraseTermGraph

-- | Validate, bound, type-check, and compatibility-project a raw graph.
sealTermGraph
  :: (Ord local)
  => TypeStructure ty
  -> TermGraphLimits
  -> TermGraphSource ty local
  -> Either (TermGraphError ty local) (TermGraph ty local)
sealTermGraph = sealTermGraphWithContextAuthority Nothing

-- | Seal with explicit authority for qualified type structure. Lexical givens
-- come only from this graph's checked introductions, never from the observer
-- or an unscoped class-environment fact. The ordinary entry point deliberately
-- rejects context nodes, preserving its previous authority boundary.
sealTermGraphWithContext
  :: Ord local
  => ContextTypeStructure ty
  -> TypeStructure ty
  -> TermGraphLimits
  -> TermGraphSource ty local
  -> Either (TermGraphError ty local) (TermGraph ty local)
sealTermGraphWithContext contextStructure =
  sealTermGraphWithContextAuthority $ Just contextStructure

sealTermGraphWithContextAuthority
  :: Ord local
  => Maybe (ContextTypeStructure ty)
  -> TypeStructure ty
  -> TermGraphLimits
  -> TermGraphSource ty local
  -> Either (TermGraphError ty local) (TermGraph ty local)
sealTermGraphWithContextAuthority contextStructure typeStructure limits source = do
  let rawNodes = termGraphSourceNodes source
      root = termGraphSourceRoot source
  observeWithin GraphNodeTable (maximumTermGraphNodes limits) rawNodes
  nodes <- buildNodeMap rawNodes
  unless (Map.member root nodes) $ Left $ MissingTermGraphRoot root
  (patternCount, occurrences, binderTypes) <-
    validateNodeCollections limits rawNodes
  validateGraphTypeAnnotations typeStructure limits rawNodes
  references <- traverse nodeReferences rawNodes
  edgeCount <- validateEdgeCount limits references
  validateReferences nodes references
  validateUniqueParents references
  reachable <- validateAcyclic references root
  case [nodeId' | (nodeId', _) <- rawNodes,
      nodeId' `Set.notMember` reachable] of
    unreachable : _ -> Left $ UnreachableTermNode unreachable
    [] -> pure ()
  validateNodeTypes contextStructure typeStructure limits nodes binderTypes rawNodes
  validateForallScopes typeStructure limits nodes root rawNodes
  validateContextScopes typeStructure nodes root
  (projection, projectedCount) <- projectGraph limits nodes root
  either (Left . ProjectedExpressionScopeError) Right
    $ Generated.validateExpressionScope projection
  either (Left . ProjectedExpressionSyntaxError) Right
    $ Generated.validateExpressionSyntax projection
  let metrics = graphMetrics
        rawNodes edgeCount patternCount occurrences projectedCount
  pure $ TermGraph root rawNodes nodes projection metrics

buildNodeMap
  :: [(TermNodeId, TermNode ty local)]
  -> Either (TermGraphError ty local) (Map TermNodeId (TermNode ty local))
buildNodeMap = foldM insert Map.empty
 where
  insert nodes (nodeId', node)
    | Map.member nodeId' nodes = Left $ DuplicateTermNodeId nodeId'
    | otherwise = Right $ Map.insert nodeId' node nodes

observeWithin
  :: GraphCollectionSite
  -> Int
  -> [value]
  -> Either (TermGraphError ty local) ()
observeWithin site maximumExpected values =
  let observed = observedListLength maximumExpected values
  in if observed <= maximumExpected
      then Right ()
      else Left $ TermGraphCollectionLimitExceeded
        site maximumExpected observed

data CollectionState ty local = CollectionState
  { collectionPatternCount :: !Int
  , collectionContextEvidenceCount :: !Int
  , collectionOccurrences :: !(Set OccurrenceId)
  , collectionBinderTypes :: !(Map local ty)
  }

validateNodeCollections
  :: (Ord local)
  => TermGraphLimits
  -> [(TermNodeId, TermNode ty local)]
  -> Either
      (TermGraphError ty local)
      (Int, Set OccurrenceId, Map local ty)
validateNodeCollections limits nodes = do
  final <- foldM visitNode (CollectionState 0 0 Set.empty Map.empty) nodes
  pure
    ( collectionPatternCount final
    , collectionOccurrences final
    , collectionBinderTypes final
    )
 where
  width = maximumTermGraphCollectionWidth limits

  visitNode state (nodeId', TermNode _ form) = case form of
    TypedLocal occurrence _ -> addOccurrence occurrence state
    TypedGlobal occurrence _ -> addOccurrence occurrence state
    TypedLambda patterns _ -> do
      observeWithin (LambdaPatternList nodeId') width patterns
      foldM visitPattern state patterns
    TypedApply{} -> Right state
    TypedVisibleTypeApplication occurrence _ _ _ ->
      addOccurrence occurrence state
    TypedForallIntroduction occurrence _ _ -> addOccurrence occurrence state
    TypedImplicitTypeApplication occurrence _ _ -> addOccurrence occurrence state
    TypedContextIntroduction occurrence _ witness -> do
      inspectConstraints nodeId' $ contextIntroductionConstraints witness
      charged <- addEvidenceCount state $ contextIntroductionConstraints witness
      addOccurrence occurrence charged
    TypedContextApplication occurrence _ witness -> do
      inspectConstraints nodeId' $ contextApplicationConstraints witness
      observeWithin (ContextEvidenceList nodeId') width $ contextApplicationEvidence witness
      charged <- addEvidenceCount state $ contextApplicationEvidence witness
      addOccurrence occurrence charged
    TypedTuple elements ->
      observeWithin (TupleElementList nodeId') width elements >> Right state
    TypedHole occurrence _ -> addOccurrence occurrence state
    TypedLet pattern _ _ -> visitPattern state pattern
    TypedCase _ alternatives -> do
      observeWithin (CaseAlternativeList nodeId') width alternatives
      foldM (\current (pattern, _) -> visitPattern current pattern)
        state alternatives

  inspectConstraints owner constraints = do
    observeWithin (ContextConstraintList owner) width constraints
    mapM_ (\(index, constraint) -> observeWithin
      (ContextConstraintArgumentList owner index) width $ constraintArguments constraint)
      $ zip [0 ..] constraints

  -- Dictionary slots have a separate counter under the existing pattern-size
  -- allowance. This bounds total evidence, not merely each node's list width,
  -- without changing the public limit-table constructor or pattern metrics.
  addEvidenceCount state evidence =
    let maximumEvidence = maximumTermGraphPatternNodes limits
        remaining = max 0 $ maximumEvidence - collectionContextEvidenceCount state
        observed = observedListLength remaining evidence
    in if observed > remaining
      then Left $ TermGraphContextEvidenceLimitExceeded maximumEvidence
        (saturatedSuccessor maximumEvidence)
      else Right state
        { collectionContextEvidenceCount = collectionContextEvidenceCount state + observed }

  visitPattern state pattern = do
    let currentCount = collectionPatternCount state
        maximumPatterns = maximumTermGraphPatternNodes limits
    when (currentCount >= maximumPatterns) $ Left $
      TermGraphPatternLimitExceeded maximumPatterns
        (saturatedSuccessor maximumPatterns)
    let nextCount = currentCount + 1
    withOccurrence <- addOccurrence
      (typedPatternOccurrence pattern)
      state {collectionPatternCount = nextCount}
    case typedPatternNode pattern of
      TypedBind local -> addBinder local (typedPatternType pattern) withOccurrence
      TypedWildcard -> Right withOccurrence
      TypedConstructor _ fields -> do
        observeWithin
          (ConstructorPatternFieldList $ typedPatternOccurrence pattern)
          width fields
        foldM visitPattern withOccurrence fields
      TypedTuplePattern fields -> do
        observeWithin
          (TuplePatternFieldList $ typedPatternOccurrence pattern)
          width fields
        foldM visitPattern withOccurrence fields
      TypedAs local nested -> do
        withBinder <- addBinder local (typedPatternType pattern) withOccurrence
        visitPattern withBinder nested

  addOccurrence occurrence state
    | occurrence `Set.member` collectionOccurrences state =
        Left $ DuplicateOccurrenceId occurrence
    | otherwise = Right state
        { collectionOccurrences = Set.insert occurrence
            $ collectionOccurrences state
        }

  addBinder local ty state
    | Map.member local $ collectionBinderTypes state =
        Left $ DuplicateTypedLocalBinder local
    | otherwise = Right state
        { collectionBinderTypes = Map.insert local ty
            $ collectionBinderTypes state
        }

validateGraphTypeAnnotations
  :: TypeStructure ty
  -> TermGraphLimits
  -> [(TermNodeId, TermNode ty local)]
  -> Either (TermGraphError ty local) ()
validateGraphTypeAnnotations typeStructure limits = mapM_ visitNode
 where
  maximumNodes = maximumTermGraphTypeNodes limits
  maximumWidth = maximumTermGraphCollectionWidth limits

  inspect site ty = do
    case observeTypeWithin typeStructure maximumNodes maximumWidth ty of
      Left (TypeStructureNodeLimitExceeded observed) -> Left $
        TermGraphTypeNodeLimitExceeded site maximumNodes observed
      Left (TypeStructureCollectionLimitExceeded observed) -> Left $
        TermGraphTypeCollectionLimitExceeded site maximumWidth observed
      Right () -> Right ()
    unless (validTypeAnnotation typeStructure ty) $ Left $
      InvalidTermGraphTypeAnnotation site ty

  visitNode (nodeId', TermNode ty form) = do
    inspect (GraphTermNodeType nodeId') ty
    case form of
      TypedLocal{} -> Right ()
      TypedGlobal{} -> Right ()
      TypedLambda patterns _ -> mapM_ visitPattern patterns
      TypedApply _ _ witness -> do
        inspect (GraphApplicationDomainType nodeId')
          $ applicationDomain witness
        inspect (GraphApplicationResultType nodeId')
          $ applicationResult witness
      TypedVisibleTypeApplication _ _ _ witness -> do
        inspect (GraphTypeApplicationSourceType nodeId')
          $ typeApplicationSource witness
        inspect (GraphTypeApplicationSelectedType nodeId')
          $ typeApplicationSelected witness
        inspect (GraphTypeApplicationResultType nodeId')
          $ typeApplicationResult witness
      TypedForallIntroduction _ _ witness -> do
        inspect (GraphTypeApplicationSourceType nodeId')
          $ forallIntroductionSource witness
        inspect (GraphTypeApplicationSelectedType nodeId')
          $ forallIntroductionVariable witness
        inspect (GraphTypeApplicationResultType nodeId')
          $ forallIntroductionBody witness
      TypedImplicitTypeApplication _ _ witness -> do
        inspect (GraphTypeApplicationSourceType nodeId')
          $ implicitTypeApplicationSource witness
        inspect (GraphTypeApplicationSelectedType nodeId')
          $ implicitTypeApplicationSelected witness
        inspect (GraphTypeApplicationResultType nodeId')
          $ implicitTypeApplicationResult witness
      TypedContextIntroduction _ _ witness ->
        inspectContext nodeId' (contextIntroductionSource witness)
          (contextIntroductionConstraints witness) (contextIntroductionBody witness)
      TypedContextApplication _ _ witness ->
        inspectContext nodeId' (contextApplicationSource witness)
          (contextApplicationConstraints witness) (contextApplicationResult witness)
      TypedTuple{} -> Right ()
      TypedHole{} -> Right ()
      TypedLet pattern _ _ -> visitPattern pattern
      TypedCase _ alternatives -> mapM_ (visitPattern . fst) alternatives

  inspectContext owner source constraints result = do
    inspect (GraphContextSourceType owner) source
    inspect (GraphContextResultType owner) result
    mapM_ (\(constraintIndex, constraint) ->
      mapM_ (\(argumentIndex, argument) -> inspect
        (GraphContextConstraintArgumentType owner constraintIndex argumentIndex) argument)
        $ zip [0 ..] $ constraintArguments constraint)
      $ zip [0 ..] constraints

  visitPattern pattern = do
    inspect (GraphPatternType $ typedPatternOccurrence pattern)
      $ typedPatternType pattern
    case typedPatternNode pattern of
      TypedBind{} -> Right ()
      TypedWildcard -> Right ()
      TypedConstructor _ fields -> mapM_ visitPattern fields
      TypedTuplePattern fields -> mapM_ visitPattern fields
      TypedAs _ nested -> visitPattern nested

nodeReferences
  :: (TermNodeId, TermNode ty local)
  -> Either (TermGraphError ty local) (TermNodeId, [TermNodeId])
nodeReferences (nodeId', TermNode _ form) = Right (nodeId', references form)
 where
  references nodeForm = case nodeForm of
    TypedLocal{} -> []
    TypedGlobal{} -> []
    TypedLambda _ body -> [body]
    TypedApply function argument _ -> [function, argument]
    TypedVisibleTypeApplication _ function _ _ -> [function]
    TypedForallIntroduction _ body _ -> [body]
    TypedImplicitTypeApplication _ function _ -> [function]
    TypedContextIntroduction _ body _ -> [body]
    TypedContextApplication _ function _ -> [function]
    TypedTuple elements -> elements
    TypedHole{} -> []
    TypedLet _ binding body -> [binding, body]
    TypedCase scrutinee alternatives ->
      scrutinee : map snd alternatives

validateReferences
  :: Map TermNodeId (TermNode ty local)
  -> [(TermNodeId, [TermNodeId])]
  -> Either (TermGraphError ty local) ()
validateReferences nodes = mapM_ validateOwner
 where
  validateOwner (owner, references) = mapM_ (validateReference owner) references
  validateReference owner reference = unless (Map.member reference nodes) $
    Left $ DanglingTermNodeReference owner reference

-- A compatibility expression is a tree of occurrences. Sharing a stored node
-- would duplicate its complete occurrence-bearing subtree during erasure, so
-- the raw table uses graph identities for validation but seals only trees.
-- Explicit let/local nodes retain semantic sharing without identity collapse.
validateUniqueParents
  :: [(TermNodeId, [TermNodeId])]
  -> Either (TermGraphError ty local) ()
validateUniqueParents references = () <$ foldM visitOwner Map.empty references
 where
  visitOwner parents (owner, children) = foldM (visitChild owner) parents children

  visitChild owner parents child = case Map.lookup child parents of
    Nothing -> Right $ Map.insert child owner parents
    Just firstOwner -> Left $
      RepeatedTermNodeReference child firstOwner owner

validateEdgeCount
  :: TermGraphLimits
  -> [(TermNodeId, [TermNodeId])]
  -> Either (TermGraphError ty local) Int
validateEdgeCount limits = foldM add 0
 where
  maximumEdges = maximumTermGraphEdges limits
  add count (_, references) =
    let remaining = max 0 $ maximumEdges - count
        observed = observedListLength remaining references
    in if observed <= remaining
        then Right $ count + observed
        else Left $ TermGraphEdgeLimitExceeded maximumEdges
          (saturatedSuccessor maximumEdges)

validateAcyclic
  :: [(TermNodeId, [TermNodeId])]
  -> TermNodeId
  -> Either (TermGraphError ty local) (Set TermNodeId)
validateAcyclic references root = visit [] Set.empty root
 where
  referenceMap = Map.fromList references

  visit path visited nodeId'
    | nodeId' `elem` path =
        Left $ CyclicTermNodeReference $ reverse (nodeId' : path)
    | nodeId' `Set.member` visited = Right visited
    | otherwise = do
        let children = Map.findWithDefault [] nodeId' referenceMap
        reached <- foldM (visit (nodeId' : path)) visited children
        pure $ Set.insert nodeId' reached

-- Introduction skolems are lexical type binders, not arbitrary annotations.
-- Inspect every annotation (including witnesses and patterns), so an unused
-- branch or an erased type application cannot hide a scope escape. Globals
-- always denote the source inventory and cannot acquire a local skolem.
validateForallScopes
  :: TypeStructure ty
  -> TermGraphLimits
  -> Map TermNodeId (TermNode ty local)
  -> TermNodeId
  -> [(TermNodeId, TermNode ty local)]
  -> Either (TermGraphError ty local) ()
validateForallScopes structure limits nodes root rawNodes =
  case introductions of
    [] -> Right ()
    _ -> case forallTypeStructure structure of
      Nothing -> Left $ ErasedForallTypeStructureUnavailable root
      Just authority -> do
        _ <- foldM distinct [] introductions
        visit authority [] root
 where
  equivalent = equivalentTypes structure
  introductions =
    [ (owner, forallIntroductionVariable witness)
    | (owner, TermNode _ (TypedForallIntroduction _ _ witness)) <- rawNodes
    ]
  introduced = map snd introductions
  member variable = any (equivalent variable)

  distinct previous current@(owner, variable) =
    case List.find (equivalent variable . snd) previous of
      Just (firstOwner, _) -> Left $
        DuplicateForallIntroductionVariable firstOwner owner variable
      Nothing -> Right $ current : previous

  inspect authority owner active ty = do
    let variables = forallFreeTypeVariables authority ty
        maximumVariables = maximumTermGraphTypeNodes limits
        observed = observedListLength maximumVariables variables
    when (observed > maximumVariables) $ Left $
      TermGraphTypeNodeLimitExceeded (GraphTermNodeType owner)
        maximumVariables observed
    mapM_ (checkVariable owner active) variables

  checkVariable owner active variable
    | member variable introduced && not (member variable active) =
        Left $ ForallIntroductionVariableEscapes owner variable
    | otherwise = Right ()

  visit authority active owner = case Map.lookup owner nodes of
    Nothing -> Left $ DanglingTermNodeReference owner owner
    Just (TermNode ty form) -> do
      inspect authority owner active ty
      let inspectHere = inspect authority owner active
          visitHere = visit authority active
          patternHere = visitPattern authority owner active
      case form of
        TypedLocal{} -> Right ()
        TypedGlobal{} -> case List.find (`member` introduced)
            $ forallFreeTypeVariables authority ty of
          Just variable -> Left $
            ForallIntroductionVariableInGlobal owner variable
          Nothing -> Right ()
        TypedLambda patterns body ->
          mapM_ patternHere patterns >> visitHere body
        TypedApply function argument witness -> do
          inspectHere $ applicationDomain witness
          inspectHere $ applicationResult witness
          visitHere function
          visitHere argument
        TypedVisibleTypeApplication _ function _ witness -> do
          mapM_ inspectHere
            [ typeApplicationSource witness
            , typeApplicationSelected witness
            , typeApplicationResult witness
            ]
          visitHere function
        TypedForallIntroduction _ body witness -> do
          inspectHere $ forallIntroductionSource witness
          let opened = forallIntroductionVariable witness : active
          inspect authority owner opened $ forallIntroductionVariable witness
          inspect authority owner opened $ forallIntroductionBody witness
          visit authority opened body
        TypedImplicitTypeApplication _ function witness -> do
          mapM_ inspectHere
            [ implicitTypeApplicationSource witness
            , implicitTypeApplicationSelected witness
            , implicitTypeApplicationResult witness
            ]
          visitHere function
        TypedContextIntroduction _ body witness -> do
          mapM_ inspectHere $ contextIntroductionSource witness
            : contextIntroductionBody witness
            : concatMap constraintArguments (contextIntroductionConstraints witness)
          visitHere body
        TypedContextApplication _ function witness -> do
          mapM_ inspectHere $ contextApplicationSource witness
            : contextApplicationResult witness
            : concatMap constraintArguments (contextApplicationConstraints witness)
          visitHere function
        TypedTuple elements -> mapM_ visitHere elements
        TypedHole{} -> Right ()
        TypedLet pattern binding body -> do
          patternHere pattern
          visitHere binding
          visitHere body
        TypedCase scrutinee alternatives -> do
          visitHere scrutinee
          mapM_ (\(pattern, body) -> patternHere pattern >> visitHere body)
            alternatives

  visitPattern authority owner active pattern = do
    inspect authority owner active $ typedPatternType pattern
    case typedPatternNode pattern of
      TypedBind{} -> Right ()
      TypedWildcard -> Right ()
      TypedConstructor _ fields ->
        mapM_ (visitPattern authority owner active) fields
      TypedTuplePattern fields ->
        mapM_ (visitPattern authority owner active) fields
      TypedAs _ nested -> visitPattern authority owner active nested

-- Only a context introduction creates dictionary bindings. The active map is
-- extended for its child alone; siblings and a let binding's unrelated body
-- cannot consume evidence introduced elsewhere in the tree. Type substitutions
-- have already been normalized in the graph and exact source constraints are
-- checked before this traversal.
validateContextScopes
  :: TypeStructure ty
  -> Map TermNodeId (TermNode ty local)
  -> TermNodeId
  -> Either (TermGraphError ty local) ()
validateContextScopes structure nodes = visit Map.empty
 where
  equivalent = equivalentConstraint (equivalentTypes structure)
  visit active owner = case Map.lookup owner nodes of
    Nothing -> Left $ DanglingTermNodeReference owner owner
    Just node -> case termNodeForm node of
      TypedContextIntroduction occurrence body witness ->
        let bindings = Map.fromList
              [ (EvidenceBinderId occurrence slot, constraint)
              | (slot, constraint) <- zip [0 ..] $ contextIntroductionConstraints witness ]
        in visit (Map.union bindings active) body
      TypedContextApplication _ function witness -> do
        mapM_ (checkGiven owner active) $ zip
          (contextApplicationConstraints witness) (contextApplicationEvidence witness)
        visit active function
      _ -> do
        (_, children) <- nodeReferences (owner, node)
        mapM_ (visit active) children

  checkGiven owner active (required, evidence) =
    let binder = contextEvidenceBinder evidence
    in case Map.lookup binder active of
      Nothing -> Left $ UnboundContextEvidence owner binder
      Just actual -> unless (required `equivalent` actual) $ Left $
        ContextGivenTypeMismatch owner binder required actual

equivalentConstraint :: (ty -> ty -> Bool) -> Constraint ty -> Constraint ty -> Bool
equivalentConstraint equivalent (Constraint leftName left) (Constraint rightName right) =
  leftName == rightName && length left == length right
    && and (zipWith equivalent left right)

validateNodeTypes
  :: (Ord local)
  => Maybe (ContextTypeStructure ty)
  -> TypeStructure ty
  -> TermGraphLimits
  -> Map TermNodeId (TermNode ty local)
  -> Map local ty
  -> [(TermNodeId, TermNode ty local)]
  -> Either (TermGraphError ty local) ()
validateNodeTypes contextStructure typeStructure limits nodes binderTypes = mapM_ validateNode
 where
  equivalent = equivalentTypes typeStructure

  lookupNodeType owner reference = case Map.lookup reference nodes of
    Just node -> Right $ termNodeType node
    Nothing -> Left $ DanglingTermNodeReference owner reference

  validateNode (nodeId', node) = case termNodeForm node of
    TypedLocal _ local -> case Map.lookup local binderTypes of
      Nothing -> Left $ UnboundTypedLocal nodeId' local
      Just expected
        | expected `equivalent` termNodeType node -> Right ()
        | otherwise -> Left $ TypedLocalTypeMismatch
            nodeId' local expected (termNodeType node)
    TypedGlobal{} -> Right ()
    TypedLambda patterns body -> do
      bodyType <- lookupNodeType nodeId' body
      validateLambda nodeId' (termNodeType node) patterns bodyType
    TypedApply function argument witness -> do
      functionType <- lookupNodeType nodeId' function
      argumentType <- lookupNodeType nodeId' argument
      (domain, result) <- case functionTypeComponents typeStructure
          functionType of
        Nothing -> Left $ ExpectedFunctionType nodeId' functionType
        Just components -> Right components
      unless (domain `equivalent` applicationDomain witness) $ Left $
        ApplicationDomainTypeMismatch nodeId'
          domain (applicationDomain witness)
      unless (argumentType `equivalent` domain) $ Left $
        ApplicationArgumentTypeMismatch nodeId'
          domain argumentType
      unless (result `equivalent` applicationResult witness) $ Left $
        ApplicationResultTypeMismatch nodeId'
          result (applicationResult witness)
      unless (termNodeType node `equivalent` result) $ Left $
        ApplicationResultTypeMismatch nodeId' result (termNodeType node)
    TypedVisibleTypeApplication _ function argument witness -> do
      functionType <- lookupNodeType nodeId' function
      unless (functionType `equivalent` typeApplicationSource witness) $ Left $
        VisibleTypeApplicationSourceMismatch nodeId'
          functionType (typeApplicationSource witness)
      unless (termNodeType node
          `equivalent` typeApplicationResult witness) $ Left $
        VisibleTypeApplicationResultMismatch nodeId'
          (typeApplicationResult witness) (termNodeType node)
      unless (validTypeApplicationWitness typeStructure argument witness) $
        Left $ InvalidVisibleTypeApplicationWitness nodeId' argument witness
    TypedForallIntroduction _ body witness -> do
      authority <- requireForallStructure nodeId'
      bodyType <- lookupNodeType nodeId' body
      unless (validForallIntroductionWitness authority witness) $
        Left $ InvalidForallIntroductionWitness nodeId' witness
      unless (termNodeType node `equivalent` forallIntroductionSource witness) $
        Left $ ForallIntroductionSourceMismatch nodeId'
          (forallIntroductionSource witness) (termNodeType node)
      unless (bodyType `equivalent` forallIntroductionBody witness) $
        Left $ ForallIntroductionBodyMismatch nodeId'
          (forallIntroductionBody witness) bodyType
    TypedImplicitTypeApplication _ function witness -> do
      authority <- requireForallStructure nodeId'
      functionType <- lookupNodeType nodeId' function
      unless (validImplicitTypeApplicationWitness authority witness) $
        Left $ InvalidImplicitTypeApplicationWitness nodeId' witness
      unless (functionType `equivalent` implicitTypeApplicationSource witness) $
        Left $ ImplicitTypeApplicationSourceMismatch nodeId'
          functionType (implicitTypeApplicationSource witness)
      unless (termNodeType node `equivalent` implicitTypeApplicationResult witness) $
        Left $ ImplicitTypeApplicationResultMismatch nodeId'
          (implicitTypeApplicationResult witness) (termNodeType node)
    TypedContextIntroduction _ body witness -> do
      validateContextSource nodeId' (contextIntroductionSource witness)
        (contextIntroductionConstraints witness) (contextIntroductionBody witness)
      unless (termNodeType node `equivalent` contextIntroductionSource witness) $
        Left $ ContextSourceMismatch nodeId' (termNodeType node)
          (contextIntroductionSource witness)
      bodyType <- lookupNodeType nodeId' body
      unless (bodyType `equivalent` contextIntroductionBody witness) $
        Left $ ContextResultMismatch nodeId' bodyType (contextIntroductionBody witness)
    TypedContextApplication _ function witness -> do
      validateContextSource nodeId' (contextApplicationSource witness)
        (contextApplicationConstraints witness) (contextApplicationResult witness)
      functionType <- lookupNodeType nodeId' function
      unless (functionType `equivalent` contextApplicationSource witness) $
        Left $ ContextSourceMismatch nodeId' functionType (contextApplicationSource witness)
      unless (termNodeType node `equivalent` contextApplicationResult witness) $
        Left $ ContextResultMismatch nodeId' (termNodeType node)
          (contextApplicationResult witness)
      let expected = length $ contextApplicationConstraints witness
          actual = length $ contextApplicationEvidence witness
      unless (actual == expected) $
        Left $ ContextEvidenceArityMismatch nodeId' expected actual
    TypedTuple elements -> validateTuple nodeId' (termNodeType node) elements
    TypedHole{} -> Right ()
    TypedLet pattern binding body -> do
      validatePattern pattern
      bindingType <- lookupNodeType nodeId' binding
      bodyType <- lookupNodeType nodeId' body
      unless (typedPatternType pattern `equivalent` bindingType) $ Left $
        LetPatternTypeMismatch nodeId'
          bindingType (typedPatternType pattern)
      unless (termNodeType node `equivalent` bodyType) $ Left $
        LetResultTypeMismatch nodeId' bodyType (termNodeType node)
    TypedCase scrutinee alternatives -> do
      scrutineeType <- lookupNodeType nodeId' scrutinee
      mapM_ (validateAlternative nodeId' scrutineeType) alternatives
      mapM_ (validateBranchResult nodeId' $ termNodeType node) alternatives

  requireForallStructure nodeId' = case forallTypeStructure typeStructure of
    Nothing -> Left $ ErasedForallTypeStructureUnavailable nodeId'
    Just authority -> Right authority

  validateContextSource owner source constraints result = do
    authority <- case contextStructure of
      Nothing -> Left $ ContextTypeStructureUnavailable owner
      Just checked -> Right checked
    (expected, body) <- case contextTypeComponents authority source of
      Nothing -> Left $ InvalidContextSource owner source
      Just components -> Right components
    let width = maximumTermGraphCollectionWidth limits
    observeWithin (ContextConstraintList owner) width expected
    mapM_ (\(index, constraint) -> observeWithin
      (ContextConstraintArgumentList owner index) width $ constraintArguments constraint)
      $ zip [0 ..] expected
    inspectObserved (GraphContextResultType owner) body
    mapM_ (\(constraintIndex, constraint) ->
      mapM_ (\(argumentIndex, argument) -> inspectObserved
        (GraphContextConstraintArgumentType owner constraintIndex argumentIndex) argument)
        $ zip [0 ..] $ constraintArguments constraint)
      $ zip [0 ..] expected
    when (null expected) $ Left $ InvalidContextSource owner source
    unless (length expected == length constraints
        && and (zipWith (equivalentConstraint equivalent) expected constraints)) $
      Left $ ContextConstraintMismatch owner expected constraints
    unless (body `equivalent` result) $ Left $ ContextResultMismatch owner body result

  -- An authority callback is an observation boundary too: it must not smuggle
  -- an unbounded or malformed type into equality before the normal type gate.
  inspectObserved site ty = do
    let maximumNodes = maximumTermGraphTypeNodes limits
        maximumWidth = maximumTermGraphCollectionWidth limits
    case observeTypeWithin typeStructure maximumNodes maximumWidth ty of
      Left (TypeStructureNodeLimitExceeded observed) -> Left $
        TermGraphTypeNodeLimitExceeded site maximumNodes observed
      Left (TypeStructureCollectionLimitExceeded observed) -> Left $
        TermGraphTypeCollectionLimitExceeded site maximumWidth observed
      Right () -> Right ()
    unless (validTypeAnnotation typeStructure ty) $ Left $
      InvalidTermGraphTypeAnnotation site ty

  validateLambda nodeId' lambdaType patterns bodyType =
    consume lambdaType patterns
   where
    consume remaining [] = unless (remaining `equivalent` bodyType) $ Left $
      LambdaResultTypeMismatch nodeId' bodyType remaining
    consume remaining (pattern : rest) = do
      validatePattern pattern
      (domain, result) <- case functionTypeComponents typeStructure remaining of
        Nothing -> Left $ ExpectedFunctionType nodeId' remaining
        Just components -> Right components
      unless (domain `equivalent` typedPatternType pattern) $ Left $
        LambdaDomainTypeMismatch nodeId'
          (typedPatternOccurrence pattern) domain (typedPatternType pattern)
      consume result rest

  validateTuple nodeId' tupleType elements = do
    expected <- case tupleTypeComponents typeStructure tupleType of
      Nothing -> Left $ ExpectedTupleType nodeId' tupleType
      Just fields -> Right fields
    let actualCount = length elements
        expectedCount = observedListLength actualCount expected
    unless (expectedCount == actualCount) $ Left $
      TupleArityTypeMismatch nodeId' expectedCount actualCount
    mapM_ (validateTupleField nodeId') $ zip3 [0 ..] expected elements

  validateTupleField nodeId' (index, expected, element) = do
    elementType <- lookupNodeType nodeId' element
    unless (expected `equivalent` elementType) $ Left $
      TupleFieldTypeMismatch nodeId' index expected elementType

  validateAlternative nodeId' scrutineeType (pattern, _) = do
    validatePattern pattern
    unless (typedPatternType pattern `equivalent` scrutineeType) $ Left $
      CasePatternTypeMismatch nodeId'
        (typedPatternOccurrence pattern)
        scrutineeType (typedPatternType pattern)

  validateBranchResult nodeId' resultType (_, body) = do
    bodyType <- lookupNodeType nodeId' body
    unless (bodyType `equivalent` resultType) $ Left $
      CaseResultTypeMismatch nodeId' resultType bodyType

  validatePattern pattern = case typedPatternNode pattern of
    TypedBind{} -> Right ()
    TypedWildcard -> Right ()
    TypedConstructor name fields -> do
      expected <- case constructorPatternFieldTypes typeStructure name
          $ typedPatternType pattern of
        Nothing -> Left $ UnknownConstructorPatternSchema
          (typedPatternOccurrence pattern) name (typedPatternType pattern)
        Just fieldTypes -> Right fieldTypes
      let actualCount = length fields
          expectedCount = observedListLength actualCount expected
      unless (expectedCount == actualCount) $ Left $
        ConstructorPatternArityTypeMismatch
          (typedPatternOccurrence pattern) expectedCount actualCount
      mapM_ (validateConstructorPatternField
          $ typedPatternOccurrence pattern) $
        zip3 [0 ..] expected fields
      mapM_ validatePattern fields
    TypedTuplePattern fields -> do
      expected <- case tupleTypeComponents typeStructure
          $ typedPatternType pattern of
        Nothing -> Left $ ExpectedTuplePatternType
          (typedPatternOccurrence pattern) (typedPatternType pattern)
        Just fieldTypes -> Right fieldTypes
      let actualCount = length fields
          expectedCount = observedListLength actualCount expected
      unless (expectedCount == actualCount) $ Left $
        TuplePatternArityTypeMismatch
          (typedPatternOccurrence pattern)
          expectedCount actualCount
      mapM_ (validatePatternField $ typedPatternOccurrence pattern) $
        zip3 [0 ..] expected fields
      mapM_ validatePattern fields
    TypedAs _ nested -> do
      unless (typedPatternType nested
          `equivalent` typedPatternType pattern) $ Left $
        AsPatternTypeMismatch
          (typedPatternOccurrence pattern)
          (typedPatternType pattern) (typedPatternType nested)
      validatePattern nested

  validatePatternField occurrence (index, expected, field) =
    unless (typedPatternType field `equivalent` expected) $ Left $
      TuplePatternFieldTypeMismatch occurrence index
        expected (typedPatternType field)

  validateConstructorPatternField occurrence (index, expected, field) =
    unless (typedPatternType field `equivalent` expected) $ Left $
      ConstructorPatternFieldTypeMismatch occurrence index
        expected (typedPatternType field)

projectGraph
  :: TermGraphLimits
  -> Map TermNodeId (TermNode ty local)
  -> TermNodeId
  -> Either
      (TermGraphError ty local)
      (Generated.Expression local, Int)
projectGraph limits nodes root = do
  (expression, remaining) <- projectNode
    (maximumTermGraphProjectionNodes limits) root
  pure (expression, maximumTermGraphProjectionNodes limits - remaining)
 where
  -- Source lambda groups can cross an erased type-binder boundary. Keep
  -- their canonical grouping while preserving the separate typed scopes.
  -- Ordinary nested lambda nodes retain their historical exact projection.
  crossesErasedForall node = case Map.lookup node nodes of
    Just (TermNode _ TypedForallIntroduction{}) -> True
    Just (TermNode _ (TypedImplicitTypeApplication _ child _)) -> crossesErasedForall child
    Just (TermNode _ TypedContextIntroduction{}) -> True
    Just (TermNode _ (TypedContextApplication _ child _)) -> crossesErasedForall child
    _ -> False

  consumeProjectionNode remaining
    | remaining <= 0 = Left $ TermGraphProjectionLimitExceeded
        (maximumTermGraphProjectionNodes limits)
        (saturatedSuccessor $ maximumTermGraphProjectionNodes limits)
    | otherwise = Right $ remaining - 1

  projectNode = projectNodeWithMergedLambda False

  projectNodeWithMergedLambda merged remaining nodeId' = do
    case Map.lookup nodeId' nodes of
      Nothing -> Left $ DanglingTermNodeReference nodeId' nodeId'
      Just (TermNode _ form) -> case form of
        -- Erased evidence nodes are bounded by the graph-node/edge quotas;
        -- they create no compatibility node and consume no projection slot.
        TypedForallIntroduction _ child _ -> projectNodeWithMergedLambda merged remaining child
        TypedImplicitTypeApplication _ child _ -> projectNodeWithMergedLambda merged remaining child
        TypedContextIntroduction _ child _ -> projectNodeWithMergedLambda merged remaining child
        TypedContextApplication _ child _ -> projectNodeWithMergedLambda merged remaining child
        _ -> do
          remaining' <- case form of
            TypedLambda{} | merged -> Right remaining
            _ -> consumeProjectionNode remaining
          projectForm remaining' form

  projectForm remaining form = case form of
    TypedLocal _ local -> Right (Generated.Local local, remaining)
    TypedGlobal _ name -> Right (Generated.Global name, remaining)
    TypedLambda patterns body -> do
      (projectedPatterns, remaining') <- projectPatterns remaining patterns
      (bodyExpression, remaining'') <- projectNodeWithMergedLambda
        (crossesErasedForall body) remaining' body
      pure
        ( (if crossesErasedForall body then Generated.lambdaExpression else Generated.Lambda)
            projectedPatterns bodyExpression
        , remaining''
        )
    TypedApply function argument _ -> do
      (functionExpression, remaining') <- projectNode remaining function
      (argumentExpression, remaining'') <- projectNode remaining' argument
      pure
        (Generated.Apply functionExpression argumentExpression, remaining'')
    TypedVisibleTypeApplication _ function argument _ -> do
      (functionExpression, remaining') <- projectNode remaining function
      pure
        ( Generated.VisibleTypeApplication functionExpression argument
        , remaining'
        )
    TypedForallIntroduction _ body _ -> projectNode remaining body
    TypedImplicitTypeApplication _ function _ -> projectNode remaining function
    TypedContextIntroduction _ body _ -> projectNode remaining body
    TypedContextApplication _ function _ -> projectNode remaining function
    TypedTuple elements -> do
      (expressions, remaining') <- projectMany remaining elements
      pure (Generated.Tuple (reverse expressions), remaining')
    TypedHole _ local -> Right (Generated.Hole local, remaining)
    TypedLet pattern binding body -> do
      (projectedPattern, remaining') <- projectPattern remaining pattern
      (bindingExpression, remaining'') <- projectNode remaining' binding
      (bodyExpression, remaining''') <- projectNode remaining'' body
      pure
        ( Generated.Let projectedPattern
            bindingExpression bodyExpression
        , remaining'''
        )
    TypedCase scrutinee alternatives -> do
      (scrutineeExpression, remaining') <- projectNode remaining scrutinee
      (projectedAlternatives, remaining'') <-
        foldM projectAlternative ([], remaining') alternatives
      pure
        ( Generated.Case scrutineeExpression $ reverse projectedAlternatives
        , remaining''
        )

  projectMany remaining = foldM projectOne ([], remaining)
   where
    projectOne (reversed, available) nodeId' = do
      (expression, available') <- projectNode available nodeId'
      pure (expression : reversed, available')

  projectPatterns remaining patterns = do
    (reversed, remaining') <- foldM projectPatternOne
      ([], remaining) patterns
    pure (reverse reversed, remaining')
   where
    projectPatternOne (reversed, available) pattern = do
      (projected, available') <- projectPattern available pattern
      pure (projected : reversed, available')

  projectPattern remaining pattern = do
    remaining' <- consumeProjectionNode remaining
    case typedPatternNode pattern of
      TypedBind local -> Right (Generated.Bind local, remaining')
      TypedWildcard -> Right (Generated.Wildcard, remaining')
      TypedConstructor name fields -> do
        (projected, remaining'') <- projectPatterns remaining' fields
        pure (Generated.Constructor name projected, remaining'')
      TypedTuplePattern fields -> do
        (projected, remaining'') <- projectPatterns remaining' fields
        pure (Generated.TuplePattern projected, remaining'')
      TypedAs local nested -> do
        (projected, remaining'') <- projectPattern remaining' nested
        pure (Generated.As local projected, remaining'')

  projectAlternative (reversed, remaining) (pattern, body) = do
    (projectedPattern, remaining') <- projectPattern remaining pattern
    (bodyExpression, remaining'') <- projectNode remaining' body
    pure ((projectedPattern, bodyExpression) : reversed, remaining'')

graphMetrics
  :: [(TermNodeId, TermNode ty local)]
  -> Int
  -> Int
  -> Set OccurrenceId
  -> Int
  -> TypedGraphMetrics
graphMetrics nodes edgeCount patternCount occurrences projectedCount =
  List.foldl' countNode initial nodes
 where
  initial = mempty
    { typedGraphTermNodes = fromIntegral $ length nodes
    , typedGraphEdges = fromIntegral edgeCount
    , typedGraphPatternNodes = fromIntegral patternCount
    , typedGraphSourceOccurrences = fromIntegral $ Set.size occurrences
    , typedGraphProjectedNodes = fromIntegral projectedCount
    }

  countNode metrics (_, TermNode _ form) = case form of
    TypedLocal{} -> metrics
      { typedGraphLocalUses = typedGraphLocalUses metrics + 1 }
    TypedGlobal{} -> metrics
      { typedGraphGlobalUses = typedGraphGlobalUses metrics + 1 }
    TypedLambda{} -> metrics
    TypedApply{} -> metrics
      { typedGraphApplications = typedGraphApplications metrics + 1 }
    TypedVisibleTypeApplication{} -> metrics
      { typedGraphVisibleTypeApplications =
          typedGraphVisibleTypeApplications metrics + 1 }
    TypedForallIntroduction{} -> metrics
    TypedImplicitTypeApplication{} -> metrics
    TypedContextIntroduction{} -> metrics
    TypedContextApplication{} -> metrics
    TypedTuple{} -> metrics
      { typedGraphTuples = typedGraphTuples metrics + 1 }
    TypedHole{} -> metrics
      { typedGraphHoles = typedGraphHoles metrics + 1 }
    TypedLet{} -> metrics
      { typedGraphLets = typedGraphLets metrics + 1 }
    TypedCase _ alternatives -> metrics
      { typedGraphCases = typedGraphCases metrics + 1
      , typedGraphCaseBranches = typedGraphCaseBranches metrics
          + fromIntegral (length alternatives)
      }
