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
-- semantic sharing. Consumers of a sealed 'TermGraph' therefore do not need
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
  , ApplicationWitness (..)
  , TypeApplicationWitness (..)
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
import Language.Haskell.Synthesis.Constraint (constraintArguments)
import qualified Language.Haskell.Synthesis.Generated as Generated
import Language.Haskell.Synthesis.Name (Boxity (Boxed), Name)
import qualified Language.Haskell.Synthesis.Type as SharedType
import qualified Language.Haskell.Synthesis.TypeAtom as TypeAtom

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
  }

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
        && case Generated.visibleTypeArgumentClosedType argument of
          Nothing -> True
          Just specified -> TypeAtom.alphaEquivalentClosedTypes
            (typeApplicationSelected witness) specified
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
-- the exact 'CertificateId'; later query sealing will make that relationship
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

data TypedPatternNode ty local
  = TypedBind local
  | TypedWildcard
  | TypedConstructor Name [TypedPattern ty local]
  | TypedTuplePattern [TypedPattern ty local]
  | TypedAs local (TypedPattern ty local)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (NFData ty, NFData local) => NFData (TypedPatternNode ty local)

-- | One typed term node stored under a separate 'TermNodeId'.
data TermNode ty local = TermNode
  { termNodeType :: ty
  , termNodeForm :: TermNodeForm ty local
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (NFData ty, NFData local) => NFData (TermNode ty local)

-- | Typed counterparts of every current compatibility expression form.
--
-- Locals, globals, holes, and pattern sites carry stable occurrence identity.
-- Search-created structural nodes use their containing 'TermNodeId'.
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

data GraphCollectionSite
  = GraphNodeTable
  | LambdaPatternList TermNodeId
  | TupleElementList TermNodeId
  | CaseAlternativeList TermNodeId
  | ConstructorPatternFieldList OccurrenceId
  | TuplePatternFieldList OccurrenceId
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
sealTermGraph typeStructure limits source = do
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
  validateNodeTypes typeStructure nodes binderTypes rawNodes
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
  final <- foldM visitNode (CollectionState 0 Set.empty Map.empty) nodes
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
    TypedTuple elements ->
      observeWithin (TupleElementList nodeId') width elements >> Right state
    TypedHole occurrence _ -> addOccurrence occurrence state
    TypedLet pattern _ _ -> visitPattern state pattern
    TypedCase _ alternatives -> do
      observeWithin (CaseAlternativeList nodeId') width alternatives
      foldM (\current (pattern, _) -> visitPattern current pattern)
        state alternatives

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
      TypedTuple{} -> Right ()
      TypedHole{} -> Right ()
      TypedLet pattern _ _ -> visitPattern pattern
      TypedCase _ alternatives -> mapM_ (visitPattern . fst) alternatives

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

validateNodeTypes
  :: (Ord local)
  => TypeStructure ty
  -> Map TermNodeId (TermNode ty local)
  -> Map local ty
  -> [(TermNodeId, TermNode ty local)]
  -> Either (TermGraphError ty local) ()
validateNodeTypes typeStructure nodes binderTypes = mapM_ validateNode
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
  consumeProjectionNode remaining
    | remaining <= 0 = Left $ TermGraphProjectionLimitExceeded
        (maximumTermGraphProjectionNodes limits)
        (saturatedSuccessor $ maximumTermGraphProjectionNodes limits)
    | otherwise = Right $ remaining - 1

  projectNode remaining nodeId' = do
    remaining' <- consumeProjectionNode remaining
    case Map.lookup nodeId' nodes of
      Nothing -> Left $ DanglingTermNodeReference nodeId' nodeId'
      Just (TermNode _ form) -> projectForm remaining' form

  projectForm remaining form = case form of
    TypedLocal _ local -> Right (Generated.Local local, remaining)
    TypedGlobal _ name -> Right (Generated.Global name, remaining)
    TypedLambda patterns body -> do
      (projectedPatterns, remaining') <- projectPatterns remaining patterns
      (bodyExpression, remaining'') <- projectNode remaining' body
      pure
        ( Generated.Lambda projectedPatterns bodyExpression
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

saturatedSuccessor :: Int -> Int
saturatedSuccessor value
  | value == maxBound = maxBound
  | otherwise = value + 1

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
