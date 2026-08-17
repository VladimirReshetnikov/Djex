-- | Explicit forall elimination at scoped-value use sites.
--
-- The first-order unifier deliberately treats quantified subtrees as atoms.
-- Opening a provider is a typing rule, not a unification rule, so search and
-- the independent expression checker share that operation here.
module Language.Haskell.Exference.Core.Internal.Polytype
  ( ProviderUseMode (..)
  , GroundProviderInstantiation (..)
  , classifyProviderUse
  , quantifiedProviderSubsumes
  , instantiateLeadingForallsWith
  , groundProviderInstantiations
  , candidateProviderInstantiations
  , assignmentProviderInstantiations
  , isProviderAssignmentArgument
  , isVisibleTypeCandidate
  )
where

import Control.Monad (guard)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Language.Haskell.Exference.Core.Internal.FlexibleIds
  ( FlexibleRenaming
  , flexibleIdentifiers
  )
import Language.Haskell.Exference.Core.Internal.VariableSupply
  ( FlexibleIdSupply
  , reserveIdentifiers
  )
import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
  ( alphaNormalizeForalls
  , containsForall
  )
import Language.Haskell.Exference.Core.Unify
  ( TypeEq (..)
  , unifyRight
  , unifyRightEqs
  )
import qualified Language.Haskell.Synthesis.Collection as SharedCollection
import qualified Language.Haskell.Synthesis.Generated as SharedGenerated
import qualified Language.Haskell.Synthesis.Query as SharedQuery
import qualified Language.Haskell.Synthesis.Type as SharedType
import qualified Language.Haskell.Synthesis.TypeAtom as SharedTypeAtom

-- | How a scoped provider participates at one occurrence. Keeping this
-- classification beside forall instantiation prevents search and independent
-- checking from assigning different meanings to the same annotation shape.
data ProviderUseMode
  = OrdinaryProviderUse
  | OpaqueProviderForwarding
  | SubsumedProviderForwarding
  | InstantiateProviderUse
  deriving (Eq, Show)

-- | One bounded visible instantiation of a provider.
--
-- An evidence-directed argument is a closed monotype already present in a
-- validated instance head. The query-derived route may additionally retain a
-- complete closed, context-free quantified proper type. Search therefore
-- neither invents a type nor needs a source spelling for a free type variable.
-- The instantiated body and its direct obligations are cached beside the
-- ordered arguments so expression construction and constraint solving consume
-- exactly the same witness.
data GroundProviderInstantiation = GroundProviderInstantiation
  { groundProviderArguments :: [HsType]
  , groundProviderType :: HsType
  , groundProviderConstraints :: [HsConstraint]
  }
  deriving (Eq, Show)

-- | Classify a provider against the type requested at its occurrence.
--
-- Empty-binder, empty-context forall wrappers are semantic no-ops to opaque
-- atom construction and unification. Peel exactly those wrappers before
-- inspecting either root, so a vacuously wrapped monotype does not accidentally
-- request opaque forwarding. A binderless wrapper with a context remains
-- significant and is not peeled.
classifyProviderUse :: HsType -> HsType -> ProviderUseMode
classifyProviderUse rawProvider rawRequested =
  case peelVacuousForalls rawProvider of
    provider@TypeForallNative{} -> case peelVacuousForalls rawRequested of
      requested@TypeForallNative{}
        | SharedTypeAtom.alphaEquivalentTypes provider requested ->
            OpaqueProviderForwarding
        | quantifiedProviderSubsumes provider requested ->
            SubsumedProviderForwarding
        | otherwise -> OpaqueProviderForwarding
      _ -> InstantiateProviderUse
    _ -> OrdinaryProviderUse
 where
  peelVacuousForalls (TypeForallNative [] [] body) =
    peelVacuousForalls body
  peelVacuousForalls ty = ty

-- | Whether one context-free prenex scheme with no free flexible variables
-- can be instantiated to another such requested scheme.
--
-- This is deliberately shallow subsumption, not general rank-N
-- subsumption.  The requested body is the rigid left side of 'unifyRight';
-- only variables bound by the provider's leading forall may therefore be
-- solved.  Requiring both complete schemes to have no free flexible variable
-- prevents an ambient inference variable from being mistaken for one of those
-- instantiable binders.  Ambient rigid constants are allowed and share their
-- nominal namespace across both sides. Direct contexts are excluded until
-- entailment between provider and requested constraints has an equally
-- explicit rule.
--
-- A provider binder may be solved impredicatively, but only in the guarded
-- Quick-Look sense: the polytype image must already occur as a quantified
-- subtree of the requested scheme itself.  First-order unification against
-- the rigid requested side yields exactly such subtrees, so the explicit
-- membership check documents and defends the principle rather than
-- restricting it further: no polytype is ever invented, and the search never
-- guesses a quantifier the query did not supply.
--
-- Alpha-normalization is essential before the forall prefixes disappear: two
-- successive layers may legally shadow the same source binder identity.
quantifiedProviderSubsumes :: HsType -> HsType -> Bool
quantifiedProviderSubsumes provider requested = case
    (prepare provider, prepare requested) of
  (Just providerBody, Just requestedBody) -> case
      unifyRight requestedBody providerBody of
    Just substitutions -> all
      (admissibleInstantiation requestedBody)
      $ IntMap.elems substitutions
    Nothing -> False
  _ -> False
 where
  prepare source = do
    guard $ Set.null $ freeVars source
    normalized <- either (const Nothing) (Just . fst)
      $ alphaNormalizeForalls IntSet.empty source
    let (binders, constraints, body) =
          SharedType.splitLeadingForalls normalized
    guard $ not $ null binders
    guard $ null constraints
    pure body

  admissibleInstantiation requestedBody image =
    not (containsForall image)
      || any (SharedTypeAtom.alphaEquivalentTypes image)
          (quantifiedSubtrees requestedBody)

-- Every subtree containing quantification, in structural order. These are the
-- only polytypes a guarded impredicative instantiation may produce, so the
-- membership test above stays linear in the requested scheme's size.
quantifiedSubtrees :: HsType -> [HsType]
quantifiedSubtrees source =
  [subtree | subtree <- subtrees source, containsForall subtree]
 where
  subtrees ty = ty : case ty of
    TypeVar{} -> []
    TypeConstant{} -> []
    TypeCons{} -> []
    TypeArrow parameter result -> subtrees parameter ++ subtrees result
    TypeApp function argument -> subtrees function ++ subtrees argument
    TypeTuple _ elements -> concatMap subtrees elements
    TypeForallNative _ constraints body ->
      concatMap subtrees (concatMap constraint_params constraints)
        ++ subtrees body

-- | Replace every binder in the complete leading forall chain with a fresh
-- flexible variable. Direct contexts are returned as proof obligations in
-- outer-to-inner order; a forall below an arrow or other type boundary stays
-- opaque.
--
-- The caller supplies the namespace allocator so live search keeps its finite
-- test seam while the checker uses the production allocator. All source IDs
-- are reserved before allocation: after a binder is erased, reusing its old
-- spelling could capture a free occurrence or conflate shadowed layers.
-- Checked Exference inputs guarantee flexible, duplicate-free binder lists;
-- 'Nothing' therefore denotes identifier-space exhaustion at call sites.
instantiateLeadingForallsWith
  :: ([TVarId]
      -> FlexibleIdSupply
      -> Maybe (FlexibleRenaming, FlexibleIdSupply))
  -> FlexibleIdSupply
  -> HsType
  -> Maybe (HsType, [HsConstraint], FlexibleIdSupply)
instantiateLeadingForallsWith allocate initialSupply source =
  go reservedSupply [] source
 where
  reservedSupply = reserveIdentifiers
    (IntSet.toAscList $ flexibleIdentifiers source)
    initialSupply

  go supply contextChunks (TypeForallNative binders contexts body) = do
    identifiers <- traverse SharedType.flexibleVariableIdentity binders
    guard $ IntSet.size (IntSet.fromList identifiers) == length identifiers
    (renaming, nextSupply) <- allocate identifiers supply
    -- This must be a lexical rename, not a whole-namespace traversal: an
    -- inner forall may deliberately shadow the same nominal binder ID.
    let scopedRenaming = Map.fromList
          [ ( SharedType.FlexibleVariable old
            , SharedType.FlexibleVariable fresh
            )
          | (old, fresh) <- IntMap.toAscList renaming
          ]
        rename = SharedType.renameScopedVariables scopedRenaming
    go nextSupply
      (map (fmap rename) contexts : contextChunks)
      (rename body)
  go supply contextChunks body = Just
    (body, concat $ reverse contextChunks, supply)

-- | Enumerate the finite closed instantiations justified by explicit instance
-- heads in the current class environment.
--
-- A single direct provider constraint must match one ground instance head and
-- determine every binder in the complete leading forall chain. Other direct
-- constraints are retained as ordinary obligations and must still be solved.
-- Requiring one head to close the whole prefix deliberately avoids a product
-- search over unrelated instance choices; later rules may broaden that bound
-- without changing the evidence represented here. Evidence matching uses an
-- alpha-normalized view of the complete prefix, so a free variable outside a
-- later binder's scope cannot be confused with that binder and legal shadowing
-- across successive layers remains positional.
groundProviderInstantiations
  :: QueryClassEnv
  -> HsType
  -> [GroundProviderInstantiation]
groundProviderInstantiations environment source =
  SharedCollection.distinctOn groundProviderArguments $ do
    normalized <- either (const []) (pure . fst)
      $ alphaNormalizeForalls IntSet.empty source
    let (binders, constraints, _) =
          SharedType.splitLeadingForalls normalized
        binderIdentifiers =
          traverse SharedType.flexibleVariableIdentity binders
    binder <- maybe [] pure binderIdentifiers
    guard $ not $ null binder
    providerConstraint <- constraints
    instanceDeclaration <- sClassEnv_explicitInstances
      $ qClassEnv_env environment
    let instanceHead = instance_head instanceDeclaration
    guard $ constraint_tclass providerConstraint
      == constraint_tclass instanceHead
    let providerArguments = constraint_params providerConstraint
        instanceArguments = constraint_params instanceHead
        expectedArity = length providerArguments
    guard $ length instanceArguments == expectedArity
    guard $ all isGroundMonotype instanceArguments
    substitutions <- maybe [] pure $ unifyRightEqs
      $ zipWith TypeEq instanceArguments providerArguments
    guard $ IntSet.fromList (IntMap.keys substitutions)
      == IntSet.fromList binder
    orderedArguments <- maybe [] pure
      $ traverse (`IntMap.lookup` substitutions) binder
    guard $ all isGroundMonotype orderedArguments
    (instantiated, instantiatedConstraints) <- maybe [] pure
      $ instantiateLeadingForallsAt isGroundMonotype orderedArguments source
    pure GroundProviderInstantiation
      { groundProviderArguments = orderedArguments
      , groundProviderType = instantiated
      , groundProviderConstraints = instantiatedConstraints
      }

-- | Enumerate explicit instantiations of a context-free provider from a
-- caller-supplied list of closed proper types.  This complements
-- 'groundProviderInstantiations': a foreign frontend may intentionally erase
-- dictionary binders while retaining an otherwise ambiguous type binder (Lean
-- class-instance arguments are one example), so no Haskell instance head is
-- available to select it.  The caller owns the kind proof for every candidate;
-- this worker admits ground monotypes and complete closed, context-free
-- quantified types while retaining a finite prefix.
--
-- The complete leading binder chain is instantiated in source order. Keep the
-- practical rank-N bound aligned with the neighboring Djinn rule and shared
-- exact-assignment boundary; the tuple cap prevents a wide query vocabulary
-- from turning one global lookup into an unbounded product search.
candidateProviderInstantiations
  :: [HsType]
  -> HsType
  -> [GroundProviderInstantiation]
candidateProviderInstantiations rawCandidates source =
  take 32 $ SharedCollection.distinctOn groundProviderArguments $ do
    -- A query may open its free proper-type variables into ambient rigids
    -- before this scoped provider is considered. Those constants are fixed by
    -- the query's rigid-instantiation plan and remain untouched by the visible
    -- substitution below. Reject only unresolved flexible variables: admitting
    -- one would let this evidence-directed branch participate in ordinary
    -- inference instead of selecting a closed caller-supplied argument.
    guard $ Set.null $ freeVars source
    normalized <- either (const []) (pure . fst)
      $ alphaNormalizeForalls IntSet.empty source
    let (binders, constraints, body) =
          SharedType.splitLeadingForalls normalized
        binderIdentifiers =
          traverse SharedType.flexibleVariableIdentity binders
    orderedBinders <- maybe [] pure binderIdentifiers
    guard $ not $ null orderedBinders
    guard $ length orderedBinders <=
      SharedQuery.maximumProviderInstantiationArguments
    guard $ null constraints
    -- Query-selected arguments exist only for the foreign-erasure case: the
    -- source binder has no surviving term-level type occurrence from which
    -- ordinary unification could infer it. Besides avoiding redundant siblings
    -- for ordinary polymorphic functions, this fails closed for a higher-kinded
    -- binder whose kind is not represented in HsType itself.
    guard $ all (`Set.notMember` freeVars body) orderedBinders
    let candidates = SharedCollection.distinctOn
          SharedTypeAtom.alphaTypeKey
          [ candidate
          | candidate <- rawCandidates
          , isVisibleTypeCandidate candidate
          ]
    arguments <- sequence $ replicate (length orderedBinders) candidates
    (instantiated, instantiatedConstraints) <- maybe [] pure
      $ instantiateLeadingForallsAt isVisibleTypeCandidate arguments normalized
    pure GroundProviderInstantiation
      { groundProviderArguments = arguments
      , groundProviderType = instantiated
      , groundProviderConstraints = instantiatedConstraints
      }

-- | Consume complete, ordered leading-binder assignments established for one
-- exact provider by a checked adapter. Each vector is tried once; unlike the
-- historical candidate pool, this route neither constructs a Cartesian
-- product nor requires the selected binders to be absent from the provider
-- body. The stable boundary has already proved every argument's exact
-- positional binder kind, while this worker independently retains closure,
-- provider context, arity, and visible application shape. Contexts nested in
-- one closed argument remain part of that specified source type; they are not
-- provider obligations.
assignmentProviderInstantiations
  :: [[HsType]]
  -> HsType
  -> [GroundProviderInstantiation]
assignmentProviderInstantiations rawAssignments source =
  take 32 $ SharedCollection.distinctOn groundProviderArguments $ do
    guard $ Set.null $ freeVars source
    normalized <- either (const []) (pure . fst)
      $ alphaNormalizeForalls IntSet.empty source
    let (binders, constraints, _) =
          SharedType.splitLeadingForalls normalized
        binderIdentifiers =
          traverse SharedType.flexibleVariableIdentity binders
    orderedBinders <- maybe [] pure binderIdentifiers
    guard $ not $ null orderedBinders
    guard $ length orderedBinders <=
      SharedQuery.maximumProviderInstantiationArguments
    guard $ null constraints
    arguments <- SharedCollection.distinctOn
      (map SharedTypeAtom.alphaTypeKey) rawAssignments
    guard $ length arguments == length orderedBinders
    guard $ all isProviderAssignmentArgument arguments
    (instantiated, instantiatedConstraints) <- maybe [] pure
      $ instantiateLeadingForallsAt
          isProviderAssignmentArgument arguments normalized
    pure GroundProviderInstantiation
      { groundProviderArguments = arguments
      , groundProviderType = instantiated
      , groundProviderConstraints = instantiatedConstraints
      }

-- | Structural boundary expected of one adapter-checked assignment argument.
-- The adapter separately proves the provider binder's positional kind in its
-- sealed synonym and kind environment. Here we retain lexical closure and the
-- exact generated visible-argument representation, including
-- applications which contain a nested quantified or contextual proper type.
isProviderAssignmentArgument :: HsType -> Bool
isProviderAssignmentArgument source =
  Set.null (SharedType.freeVariables source)
    && case SharedGenerated.specifiedVisibleTypeArgument source of
      Right _ -> True
      Left _ -> False

-- Replace a complete leading prefix with an ordered collection of admissible
-- closed types. Closed replacements cannot be captured, so deleting shadowed
-- binder identities is sufficient for capture avoidance at nested layers. The
-- caller supplies the route-specific boundary: instance-head evidence remains
-- monotype-only while query-derived candidates may include bounded polytypes.
instantiateLeadingForallsAt
  :: (HsType -> Bool)
  -> [HsType]
  -> HsType
  -> Maybe (HsType, [HsConstraint])
instantiateLeadingForallsAt admissible arguments source =
  go arguments [] source
 where
  go remaining contextChunks (TypeForallNative binders contexts body) = do
    identifiers <- traverse SharedType.flexibleVariableIdentity binders
    guard $ IntSet.size (IntSet.fromList identifiers) == length identifiers
    let (currentArguments, laterArguments) =
          splitAt (length identifiers) remaining
    guard $ length currentArguments == length identifiers
    guard $ all admissible currentArguments
    let substitutions = Map.fromList
          $ zip (map SharedType.FlexibleVariable identifiers) currentArguments
        substitute = substituteClosedVariables substitutions
    go laterArguments
      (map (fmap substitute) contexts : contextChunks)
      (substitute body)
  go [] contextChunks body = Just
    (body, concat $ reverse contextChunks)
  go _ _ _ = Nothing

substituteClosedVariables
  :: Map.Map SynthesisVariable HsType
  -> HsType
  -> HsType
substituteClosedVariables substitutions source = case source of
  SharedType.TypeVariable variable ->
    Map.findWithDefault source variable substitutions
  SharedType.TypeConstructor{} -> source
  SharedType.TypeApplication function argument -> SharedType.TypeApplication
    (substituteClosedVariables substitutions function)
    (substituteClosedVariables substitutions argument)
  SharedType.FunctionType parameter result -> SharedType.FunctionType
    (substituteClosedVariables substitutions parameter)
    (substituteClosedVariables substitutions result)
  SharedType.TupleType boxity elements -> SharedType.TupleType boxity
    $ map (substituteClosedVariables substitutions) elements
  SharedType.ForallType binders constraints body ->
    let visible = foldr Map.delete substitutions binders
    in SharedType.ForallType binders
      (map (fmap $ substituteClosedVariables visible) constraints)
      (substituteClosedVariables visible body)

-- | Whether a query subtree may serve as a visible type argument: it must be
-- a ground monotype, or a closed forall-rooted type with no constraints
-- anywhere in its tree.
-- Query-derived visible arguments stay deliberately narrower than arbitrary
-- closed types. A quantified candidate must be the complete forall-rooted type
-- observed in a proper-type position, and every context in its tree must be
-- empty. In particular, an application merely containing a forall is not
-- independently kind-proven by the first-order core.
isVisibleTypeCandidate :: HsType -> Bool
isVisibleTypeCandidate source =
  isGroundMonotype source || closedContextFreeForall source
 where
  closedContextFreeForall quantified@TypeForallNative{} =
    Set.null (SharedType.freeVariables quantified)
      && null (SharedType.typeConstraints quantified)
  closedContextFreeForall _ = False

isGroundMonotype :: HsType -> Bool
isGroundMonotype typeExpression =
  null typeExpression
    && not (SharedType.containsForall typeExpression)
