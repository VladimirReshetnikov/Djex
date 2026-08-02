-- | Projection of the shared module scope into a checked Djinn session.
--
-- The REPL's uniform environment is authoritative on the Exference side,
-- where the source workspace is parsed and scoped. This module projects the
-- same scope into Djinn so both backends see the loaded declarations. Djinn's
-- grammar is stricter than the neutral vocabulary, so the projection degrades
-- rather than fails: unrepresentable declarations become abstract types where
-- that preserves meaning and are omitted with a recorded reason otherwise.
-- Visible recursive datatypes are not degraded merely for being recursive.
-- Their exact alias-expanded identities come from the prepared Exference
-- session; Djinn retains their constructors for bounded positive introduction
-- while withholding recursive elimination. Constructor-hidden datatypes and
-- declarations which require a later repair remain abstract, so no unavailable
-- constructor can enter search or presentation.
--
-- Value declarations become LJT axioms, and axiom sets of even moderate size
-- make Djinn's otherwise-terminating proof search intractable. They are
-- therefore excluded unless the caller opts in ('IncludeDjinnAxioms'), which
-- the REPL exposes as the @djinn-axioms@ setting. Scope-visible record
-- selectors are the exception: a selector whose parent datatype cannot be
-- case-eliminated (recursive, hidden or missing constructors) is the only
-- route to its field, so it projects under either policy. Hidden selectors do
-- not enter the session. Selectors of fully eliminable records stay out of the
-- axiom set — they would only multiply equivalent proofs of what structural
-- elimination already derives, and the projection instead reports their
-- positions so presentation can name a visible eliminated field.
module Language.Haskell.Djex.REPL.DjinnScope
  ( DjinnAxiomPolicy (..)
  , DjinnProjection (..)
  , DjinnScopeOmission (..)
  , projectDjinnScope
  , renderDjinnScopeOmission
  ) where

import Data.Either (partitionEithers)
import Data.List (partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Set as Set
import Data.Void (Void, absurd)

import qualified Djinn.Core as DjinnCore
import qualified Djinn.Internal.Declaration as DjinnDeclaration
import Language.Haskell.Djex.Djinn.Internal.Session
  ( DjinnSession
  , djinnSessionEnvironment
  , markDjinnSessionContextualProvidersOmitted
  , mkDjinnSessionChecked
  )
import Language.Haskell.Synthesis.Constraint (Constraint (..))
import Language.Haskell.Synthesis.Declaration
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error)
  , contextualDiagnostic
  )
import Language.Haskell.Synthesis.Environment
  ( environmentDeclarations
  , mkEnvironment
  )
import Language.Haskell.Synthesis.Kind (Kind (..))
import Language.Haskell.Synthesis.Name
  ( Name
  , mkIdentifier
  , mkOperator
  , nameIdentifier
  , nameOperator
  , nameSpecial
  , renderCanonical
  )
import Language.Haskell.Synthesis.Type (Type (..))
import qualified Language.Haskell.Synthesis.Type as SharedType

-- | Whether scope-visible value declarations become Djinn proof axioms.
data DjinnAxiomPolicy
  = ExcludeDjinnAxioms
  | IncludeDjinnAxioms
  deriving (Eq, Show)

-- | One sealed Djinn session and everything the projection left out.
data DjinnProjection = DjinnProjection
  { djinnProjectionSession :: DjinnSession
  , djinnProjectionOmissions :: [DjinnScopeOmission]
  , djinnProjectionPromptNames :: Map.Map Name Name
    -- ^ Canonical source identities to the unqualified spellings used by the
    -- sealed Djinn session. The shared scope resolves user input canonically;
    -- this final translation keeps inspection independent of how the user
    -- reached that identity (bare, canonically qualified, or through an
    -- import alias).
  , djinnProjectionFieldSelectors :: Map.Map (Name, Int) Name
    -- ^ Selector spellings for @(constructor, field index)@ positions in
    -- the session's renamed vocabulary, for presenting field projections.
    -- A position is present only while its selector is visible unqualified;
    -- otherwise presentation must retain the structural elimination.
  }

-- | One projection compromise, in user-reportable form.
data DjinnScopeOmission = DjinnScopeOmission
  { djinnOmissionSubject :: String
  , djinnOmissionReason :: String
  }
  deriving (Eq, Show)

renderDjinnScopeOmission :: DjinnScopeOmission -> String
renderDjinnScopeOmission omission =
  djinnOmissionSubject omission ++ ": " ++ djinnOmissionReason omission

-- Haskell permits a type/class and a value/constructor to share one
-- occurrence spelling. Djinn's concrete grammar still needs both declarations
-- renamed, so ambiguity detection must retain the source namespace even though
-- the shared 'Name' itself is deliberately namespace-neutral.
data NameNamespace
  = TypeNamespace
  | ValueNamespace
  deriving (Eq, Ord, Show)

declarationOwnedNameClaims
  :: Declaration variable kind annotation
  -> [(NameNamespace, Name)]
declarationOwnedNameClaims declaration = case declaration of
  TypeSynonymDeclaration _ name _ _ -> [(TypeNamespace, name)]
  DataTypeDeclaration _ name _ constructors ->
    (TypeNamespace, name)
      : [(ValueNamespace, constructorName constructor)
        | constructor <- constructors]
  AbstractTypeDeclaration _ name _ -> [(TypeNamespace, name)]
  ValueDeclaration signature -> [(ValueNamespace, valueName signature)]
  ClassDeclaration _ name _ _ methods ->
    (TypeNamespace, name)
      : [(ValueNamespace, valueName method) | method <- methods]
  -- Instances refer to a class; they do not introduce a name. Scope shaping
  -- removes them before renaming, but spelling this out keeps the ownership
  -- helper correct independently of that ordering.
  InstanceDeclaration {} -> []

type ScopeDeclaration = Declaration String Void ()

-- | Project the unqualified-visible part of the shared environment into a
-- Djinn session. The input declarations use canonical names; the projection
-- renames them to their in-scope unqualified spellings, because Djinn's
-- declaration grammar has no qualified type, class, or constructor names.
-- Record datatypes arrive as @(parent, [(constructor, selectors in field
-- order)])@ groups; scope-visible selectors of parents Djinn cannot
-- case-eliminate enter the session under every axiom policy.
projectDjinnScope
  :: DjinnAxiomPolicy
  -> [(Name, [(Name, [Name])])]
  -> Map.Map Name (Kind Void)
  -- ^ Authoritative ground kinds inferred by the shared source inventory.
  -> Set.Set Name
  -- ^ Datatype heads classified recursive after source synonym expansion.
  -> [ScopeDeclaration]
  -> Set.Set Name
  -- ^ Canonical types/classes visible unqualified in the prompt scope.
  -> Set.Set Name
  -- ^ Canonical values/constructors visible unqualified in the prompt scope.
  -> Either Diagnostic DjinnProjection
projectDjinnScope policy records inferredKinds recursive declarations
    visibleTypes visibleValues = do
  let fullyEliminable = Set.fromList
        [ name
        | DataTypeDeclaration _ name _ constructors <- declarations
        , name `Set.member` visibleTypes
        , not $ null constructors
        , all ((`Set.member` visibleValues) . constructorName) constructors
        , not $ name `Set.member` recursive
        ]
      allSelectors = Set.fromList
        [ selector
        | (_, constructors) <- records
        , (_, selectorNames) <- constructors
        , selector <- selectorNames
        ]
      axiomSelectors = Set.fromList
        [ selector
        | (parent, constructors) <- records
        , not $ parent `Set.member` fullyEliminable
        , (_, selectorNames) <- constructors
        , selector <- selectorNames
        ]
      (shaped, shapeOmissions) = shapeDeclarations
        inferredKinds policy axiomSelectors allSelectors
        visibleTypes visibleValues declarations
      (renamed, renameOmissions, forward) = renameDeclarations shaped
      -- Source kind assumptions use canonical names, while every declaration
      -- Djinn accepts uses its prompt spelling. Retain both views: canonical
      -- keys still describe out-of-scope references, and renamed keys make
      -- the same inferred facts available to every later repair pass.
      projectionKinds = renamedInferredKinds forward inferredKinds
      recursivePromptNames = Set.fromList
        [ promptName
        | canonicalName <- Set.toAscList recursive
        , Just promptName <- [Map.lookup canonicalName forward]
        ]
      fieldSelectors = Map.fromList
        [ ((renamedConstructor, index), renamedSelector)
        | (_, constructors) <- records
        , (constructor, selectorNames) <- constructors
        , Just renamedConstructor <- [Map.lookup constructor forward]
        , (index, selector) <- zip [0 ..] selectorNames
        , selector `Set.member` visibleValues
        , Just renamedSelector <- [unqualifyName selector]
        ]
      ( admitted
        , admissionOmissions
        , contextualProvidersOmitted
        ) = admitDeclarations renamed
      (stubbed, stubOmissions) =
        stubUnknownReferences projectionKinds admitted
  (resolved, referenceOmissions) <-
    resolveScopeReferences projectionKinds stubbed
  (sealedSession, sealOmissions) <- sealWithRepairs projectionKinds resolved
  let session
        | contextualProvidersOmitted =
            markDjinnSessionContextualProvidersOmitted sealedSession
        | otherwise = sealedSession
  let recursionOmissions = recursiveDataTypeIntroductionOmissions
        recursivePromptNames
        (environmentDeclarations $ djinnSessionEnvironment session)
  pure DjinnProjection
    { djinnProjectionSession = session
    , djinnProjectionOmissions = concat
        [ shapeOmissions
        , renameOmissions
        , recursionOmissions
        , admissionOmissions
        , stubOmissions
        , referenceOmissions
        , sealOmissions
        ]
    , djinnProjectionPromptNames = forward
    , djinnProjectionFieldSelectors = fieldSelectors
    }

-- Scope filtering and structural policy. Invisible declarations vanish
-- silently, exactly as they do from Exference's search scope; visible
-- declarations that Djinn cannot take whole are degraded or omitted loudly.
shapeDeclarations
  :: Map.Map Name (Kind Void)
  -> DjinnAxiomPolicy
  -> Set.Set Name
  -> Set.Set Name
  -> Set.Set Name
  -> Set.Set Name
  -> [ScopeDeclaration]
  -> ([ScopeDeclaration], [DjinnScopeOmission])
shapeDeclarations inferredKinds policy axiomSelectors allSelectors
    visibleTypes visibleValues declarations =
  (kept, omissions ++ instanceSummary)
 where
  (kept, omissions, instanceCount) =
    foldr shape ([], [], 0 :: Int) declarations
  instanceSummary
    | instanceCount == 0 = []
    | otherwise =
        [ DjinnScopeOmission (show instanceCount ++ " instance declarations")
            "instance declarations are not supported by Djinn"
        ]
  isTypeVisible name = name `Set.member` visibleTypes
  isValueVisible name = name `Set.member` visibleValues

  shape declaration (keptSoFar, omitted, instances) = case declaration of
    InstanceDeclaration {} -> (keptSoFar, omitted, instances + 1)
    ValueDeclaration signature
      | not $ isValueVisible $ valueName signature -> skip
      | policy == IncludeDjinnAxioms
          || valueName signature `Set.member` axiomSelectors ->
            keep declaration
      -- A selector of an eliminable record is not lost: its field is
      -- reachable structurally, and presentation names the projection.
      | valueName signature `Set.member` allSelectors -> skip
      | otherwise -> omit (valueName signature)
          "value axioms are excluded; :set djinn-axioms on to include them"
    TypeSynonymDeclaration _ name _ _
      | isTypeVisible name -> keep declaration
      | otherwise -> skip
    AbstractTypeDeclaration _ name _
      | isTypeVisible name -> keep declaration
      | otherwise -> skip
    DataTypeDeclaration annotation name parameters constructors
      | not $ isTypeVisible name -> skip
      -- Visibility-aware source loading has already distinguished an abstract
      -- catalogue stub from a genuine empty datatype.  The former reaches us
      -- as 'AbstractTypeDeclaration'; retaining an empty declaration here is
      -- therefore both safe and necessary for Djinn's explicit empty-case
      -- elimination.
      | all (isValueVisible . constructorName) constructors -> keep declaration
      | otherwise -> degradeToAbstract annotation name parameters
          "some constructors are hidden; projected as an abstract type"
    ClassDeclaration annotation name parameters superclasses methods
      | isTypeVisible name -> keep $ ClassDeclaration annotation name parameters
          superclasses (filter (isValueVisible . valueName) methods)
      | otherwise -> skip
   where
    skip = (keptSoFar, omitted, instances)
    keep shapedDeclaration = (shapedDeclaration : keptSoFar, omitted, instances)
    omit name reason =
      ( keptSoFar
      , DjinnScopeOmission (renderCanonical name) reason : omitted
      , instances
      )
    degradeToAbstract annotation name parameters reason =
      ( AbstractTypeDeclaration annotation name
          (inferredDataTypeKind inferredKinds name parameters) : keptSoFar
      , DjinnScopeOmission (renderCanonical name) reason : omitted
      , instances
      )

-- Rename canonical names to their unqualified spellings, dropping any
-- declaration whose unqualified spelling is claimed by a different canonical
-- name in the same Haskell namespace. References to renamed names follow the
-- same map; references to names outside it keep their canonical spelling and
-- are resolved by stubbing.
renameDeclarations
  :: [ScopeDeclaration]
  -> ([ScopeDeclaration], [DjinnScopeOmission], Map.Map Name Name)
renameDeclarations declarations = (renamed, omissions, forward)
 where
  claims = concatMap declarationOwnedNameClaims declarations
  (forward, ambiguous) = renameMap claims
  contested declaration =
    filter (`Set.member` ambiguous) $ declarationOwnedNameClaims declaration
  (renamed, omissions) = partitionEithers
    [ case contested declaration of
        [] -> Left $ renameDeclaration forward declaration
        (_, name) : _ -> Right $ DjinnScopeOmission (renderCanonical name)
          "its unqualified spelling is ambiguous in this scope"
    | declaration <- declarations
    ]

renameMap
  :: [(NameNamespace, Name)]
  -> (Map.Map Name Name, Set.Set (NameNamespace, Name))
renameMap claims = finish
  $ foldl' claim (Map.empty, Map.empty, Set.empty) claims
 where
  claim (forward, owners, ambiguous) owned@(namespace, name) =
    case unqualifyName name of
    Nothing -> (forward, owners, Set.insert owned ambiguous)
    Just unqualified -> case Map.lookup (namespace, unqualified) owners of
      Just owner
        | owner /= name ->
            ( forward
            , owners
            , Set.insert (namespace, owner) $ Set.insert owned ambiguous
            )
      _ ->
        ( Map.insert name unqualified forward
        , Map.insert (namespace, unqualified) name owners
        , ambiguous
        )
  finish (forward, _, ambiguous) =
    ( Map.withoutKeys forward $ Set.map snd ambiguous
    , ambiguous
    )

unqualifyName :: Name -> Maybe Name
unqualifyName name
  | isJust $ nameSpecial name = Just name
  | Just identifier <- nameIdentifier name =
      either (const Nothing) Just $ mkIdentifier identifier
  | Just operator <- nameOperator name =
      either (const Nothing) Just $ mkOperator operator
  | otherwise = Nothing

renameDeclaration :: Map.Map Name Name -> ScopeDeclaration -> ScopeDeclaration
renameDeclaration forward = onDeclarationNames rename
 where
  rename name = Map.findWithDefault name name forward

-- | Index inferred kinds under both source-canonical and Djinn-renamed names.
-- The renamed side is left-biased because it describes the exact declaration
-- that survived ambiguity filtering and entered the projected vocabulary.
renamedInferredKinds
  :: Map.Map Name Name
  -> Map.Map Name (Kind Void)
  -> Map.Map Name (Kind Void)
renamedInferredKinds forward inferredKinds = renamed `Map.union` inferredKinds
 where
  renamed = Map.fromList
    [ (Map.findWithDefault canonical canonical forward, kind)
    | (canonical, kind) <- Map.toList inferredKinds
    ]

onDeclarationNames
  :: (Name -> Name)
  -> ScopeDeclaration
  -> ScopeDeclaration
onDeclarationNames rename declaration = case declaration of
  TypeSynonymDeclaration annotation name parameters body ->
    TypeSynonymDeclaration annotation (rename name) parameters (onType body)
  DataTypeDeclaration annotation name parameters constructors ->
    DataTypeDeclaration annotation (rename name) parameters
      [ DataConstructor fieldsAnnotation (rename constructor)
          (map onType fields)
      | DataConstructor fieldsAnnotation constructor fields <- constructors
      ]
  AbstractTypeDeclaration annotation name kind ->
    AbstractTypeDeclaration annotation (rename name) kind
  ValueDeclaration signature -> ValueDeclaration $ onSignature signature
  ClassDeclaration annotation name parameters superclasses methods ->
    ClassDeclaration annotation (rename name) parameters
      (map onConstraint superclasses) (map onSignature methods)
  InstanceDeclaration annotation binders prerequisites headConstraint ->
    InstanceDeclaration annotation binders
      (map onConstraint prerequisites) (onConstraint headConstraint)
 where
  onSignature (ValueSignature annotation name valueBody) =
    ValueSignature annotation (rename name) (onType valueBody)
  onConstraint constraint = Constraint
    (rename $ constraintClass constraint)
    (map onType $ constraintArguments constraint)
  onType body = case body of
    TypeVariable variable -> TypeVariable variable
    TypeConstructor name -> TypeConstructor $ rename name
    TypeApplication function argument ->
      TypeApplication (onType function) (onType argument)
    FunctionType parameter result ->
      FunctionType (onType parameter) (onType result)
    TupleType boxity elements -> TupleType boxity $ map onType elements
    ForallType binders constraints body' ->
      ForallType binders (map onConstraint constraints) (onType body')

-- Djinn's bounded recursive projection retains constructor introduction but
-- deliberately withholds structural elimination. Describe that compromise
-- from the final sealed declaration stream: a later scope or sealing repair
-- may have degraded the datatype to abstract, in which case no constructors
-- remain and the repair's omission is the only accurate user-facing account.
recursiveDataTypeIntroductionOmissions
  :: Set.Set Name
  -> [ScopeDeclaration]
  -> [DjinnScopeOmission]
recursiveDataTypeIntroductionOmissions recursive = mapMaybe describe
 where
  describe declaration = case declaration of
    DataTypeDeclaration _ name _ _
      | name `Set.member` recursive -> Just $ DjinnScopeOmission
          (renderCanonical name)
          "recursive datatype; constructors are introduction-only in Djinn"
    _ -> Nothing

unzipOmissions
  :: [(declaration, Maybe DjinnScopeOmission)]
  -> ([declaration], [DjinnScopeOmission])
unzipOmissions results = (map fst results, mapMaybe snd results)

-- Per-declaration admission through the exact conversion Djinn's raw layer
-- applies. A class sheds unrepresentable methods before the whole class is
-- given up on.
admitDeclarations
  :: [ScopeDeclaration]
  -> ([ScopeDeclaration], [DjinnScopeOmission], Bool)
admitDeclarations = foldr admit ([], [], False)
 where
  admit declaration (kept, omitted, contextualProvidersOmitted) =
    case declaration of
    ClassDeclaration annotation name parameters superclasses methods ->
      let (goodMethods, badMethods) = partition admissibleMethod methods
          shrunk = ClassDeclaration annotation name parameters
            superclasses goodMethods
          methodOmissions =
            [ DjinnScopeOmission (renderCanonical $ valueName method)
                "its method type is not representable in Djinn"
            | method <- badMethods
            ]
      in case checkDeclaration shrunk of
        Right () ->
          ( shrunk : kept
          , methodOmissions ++ omitted
          , contextualProvidersOmitted
          )
        Left failure ->
          ( kept
          , describeOmission name failure : omitted
          , contextualProvidersOmitted
          )
    ValueDeclaration signature
      | hasLeadingContext $ valueType signature ->
          ( kept
          , DjinnScopeOmission
              (renderCanonical $ valueName signature)
              "its residual class context cannot become a proof axiom"
              : omitted
          , True
          )
    _ -> case checkDeclaration declaration of
      Right () ->
        (declaration : kept, omitted, contextualProvidersOmitted)
      Left failure ->
        ( kept
        , describeOmission (declarationSubjectName declaration) failure
            : omitted
        , contextualProvidersOmitted
        )
  admissibleMethod signature =
    checkDeclaration (ValueDeclaration signature) == Right ()

  -- Context-free prenex binders are safe: the environment sealer merely
  -- implicitizes them before formula compilation. A residual dictionary
  -- context would instead turn a conditional Haskell value into an
  -- unconditional propositional premise, so it remains an explicit omission.
  hasLeadingContext source = case SharedType.splitLeadingForalls source of
    (_, [], _) -> False
    _ -> True

checkDeclaration :: ScopeDeclaration -> Either String ()
checkDeclaration declaration = case
    DjinnDeclaration.fromSynthesisDeclaration
      (mapDeclarationKindVariables absurd declaration) of
  Right _ -> Right ()
  Left failure -> Left $ describeAdmissionFailure failure

describeOmission :: Name -> String -> DjinnScopeOmission
describeOmission name = DjinnScopeOmission (renderCanonical name)

describeAdmissionFailure :: DjinnDeclaration.SynthesisDeclarationError -> String
describeAdmissionFailure failure = case failure of
  DjinnDeclaration.ClassSuperclassesUnsupported ->
    "class superclasses are not supported by Djinn"
  DjinnDeclaration.InstanceDeclarationUnsupported ->
    "instance declarations are not supported by Djinn"
  DjinnDeclaration.UnsupportedDjinnDeclarationName _ _ ->
    "its name is not representable in Djinn's declaration grammar"
  DjinnDeclaration.DeclarationTypeConversionError _ ->
    "its type is not representable in Djinn"
  other -> show other

-- Referenced-but-undeclared nominal type constructors become abstract stubs,
-- keeping declarations whose signatures mention out-of-scope types usable
-- instead of cascading into omissions. Prefer the exact kind inferred while
-- sealing the shared source inventory: application arity alone cannot
-- distinguish @F Int@ from @F Maybe@, whose arguments have different kinds.
-- The arity-derived kind remains a compatibility fallback for a nominal name
-- absent from that authoritative inventory. Structural names (functions,
-- tuples, lists) are native to Djinn and never stubbed.
stubUnknownReferences
  :: Map.Map Name (Kind Void)
  -> [ScopeDeclaration]
  -> ([ScopeDeclaration], [DjinnScopeOmission])
stubUnknownReferences inferredKinds declarations =
  (declarations ++ stubs, omissions)
 where
  -- A value constructor and a type constructor may legally have the same
  -- canonical Haskell name. Only type-owning declarations satisfy a type
  -- reference; treating every owned name as interchangeable would suppress
  -- the abstract stub and make the later closed Djinn inventory fail.
  defined = Set.fromList
    [ name
    | declaration <- declarations
    , TypeRequirement name <- declarationOwnedRequirements declaration
    ]
  arities = Map.fromListWith max
    [ reference
    | declaration <- declarations
    , reference <- declarationTypeReferences declaration
    ]
  unknown =
    [ (name, arity)
    | (name, arity) <- Map.toList arities
    , not $ name `Set.member` defined
    , not $ isJust $ nameSpecial name
    ]
  (omissions, stubs) = partitionEithers
    [ case checkDeclaration stub of
        Right () -> Right stub
        Left _ -> Left $ DjinnScopeOmission (renderCanonical name)
          "referenced type is not representable in Djinn"
    | (name, arity) <- unknown
    , let stub = AbstractTypeDeclaration () name
            $ Map.findWithDefault (arityKind arity) name inferredKinds
    ]

-- | Type constructors referenced by a declaration with the largest applied
-- arity seen at any occurrence.
declarationTypeReferences :: ScopeDeclaration -> [(Name, Int)]
declarationTypeReferences declaration = case declaration of
  TypeSynonymDeclaration _ _ _ body -> typeReferences body
  DataTypeDeclaration _ _ _ constructors ->
    concatMap (concatMap typeReferences . constructorFields) constructors
  AbstractTypeDeclaration {} -> []
  ValueDeclaration signature -> typeReferences $ valueType signature
  ClassDeclaration _ _ _ superclasses methods ->
    concatMap constraintTypeReferences superclasses
      ++ concatMap (typeReferences . valueType) methods
  InstanceDeclaration _ _ prerequisites headConstraint ->
    concatMap constraintTypeReferences (headConstraint : prerequisites)

typeReferences :: Type String -> [(Name, Int)]
typeReferences = go 0
 where
  go arity body = case body of
    TypeVariable _ -> []
    TypeConstructor name -> [(name, arity)]
    TypeApplication function argument -> go (arity + 1) function ++ go 0 argument
    FunctionType parameter result -> go 0 parameter ++ go 0 result
    TupleType _ elements -> concatMap (go 0) elements
    ForallType _ constraints body' ->
      concatMap constraintTypeReferences constraints ++ go 0 body'

constraintTypeReferences :: Constraint (Type String) -> [(Name, Int)]
constraintTypeReferences = concatMap (typeReferences) . constraintArguments

-- A class and an ordinary type share Haskell's source namespace, but they do
-- not satisfy the same backend obligation: a datatype cannot resolve a class
-- constraint, and a class cannot supply a type constructor to Djinn's formula
-- compiler. Keep requirement identity after the earlier spelling projection.
data DeclarationRequirement
  = TypeRequirement Name
  | ClassRequirement Name
  deriving (Eq, Ord, Show)

requirementName :: DeclarationRequirement -> Name
requirementName requirement = case requirement of
  TypeRequirement name -> name
  ClassRequirement name -> name

declarationOwnedRequirements
  :: ScopeDeclaration
  -> [DeclarationRequirement]
declarationOwnedRequirements declaration = case declaration of
  TypeSynonymDeclaration _ name _ _ -> [TypeRequirement name]
  DataTypeDeclaration _ name _ _ -> [TypeRequirement name]
  AbstractTypeDeclaration _ name _ -> [TypeRequirement name]
  ClassDeclaration _ name _ _ _ -> [ClassRequirement name]
  ValueDeclaration {} -> []
  InstanceDeclaration {} -> []

-- | Every backend obligation a declaration mentions: referenced type
-- constructors and constraint classes, with their distinct resolution roles.
declarationMentions :: ScopeDeclaration -> Set.Set DeclarationRequirement
declarationMentions declaration = Set.union
  (Set.fromList
    $ map (TypeRequirement . fst) $ declarationTypeReferences declaration)
  (Set.fromList $ map ClassRequirement $ constraintClasses declaration)
 where
  constraintClasses inner = case inner of
    ClassDeclaration _ _ _ superclasses methods ->
      map constraintClass superclasses
        ++ concatMap (typeConstraintClasses . valueType) methods
    ValueDeclaration signature -> typeConstraintClasses $ valueType signature
    TypeSynonymDeclaration _ _ _ body -> typeConstraintClasses body
    DataTypeDeclaration _ _ _ constructors -> concatMap
      (concatMap typeConstraintClasses . constructorFields) constructors
    AbstractTypeDeclaration {} -> []
    InstanceDeclaration _ _ prerequisites headConstraint ->
      map constraintClass $ headConstraint : prerequisites
  typeConstraintClasses body = case body of
    ForallType _ constraints body' ->
      map constraintClass constraints
        ++ concatMap (concatMap typeConstraintClasses . constraintArguments)
            constraints
        ++ typeConstraintClasses body'
    TypeApplication function argument ->
      typeConstraintClasses function ++ typeConstraintClasses argument
    FunctionType parameter result ->
      typeConstraintClasses parameter ++ typeConstraintClasses result
    TupleType _ elements -> concatMap typeConstraintClasses elements
    _ -> []

-- | A natural-valued potential shared by both projection-repair loops. Every
-- declaration contributes one step, each class method contributes one more,
-- and a concrete datatype contributes an extra step for its possible
-- degradation to an abstract type. All supported repairs therefore decrease
-- this measure: they drop a declaration, shed at least one method, or replace
-- a concrete datatype with its abstract form.
projectionRepairMeasure :: [ScopeDeclaration] -> Integer
projectionRepairMeasure = sum . map declarationMeasure
 where
  declarationMeasure declaration = 1 + case declaration of
    ClassDeclaration _ _ _ _ methods -> toInteger $ length methods
    DataTypeDeclaration {} -> 1
    _ -> 0

-- Names that remain undefined after stubbing cannot survive Djinn's closed
-- kind inference, so the scope is resolved to a fixpoint by shedding exactly
-- the affected pieces: values and synonyms are dropped, classes lose the
-- offending methods, and datatypes degrade to abstract types.
resolveScopeReferences
  :: Map.Map Name (Kind Void)
  -> [ScopeDeclaration]
  -> Either Diagnostic ([ScopeDeclaration], [DjinnScopeOmission])
resolveScopeReferences inferredKinds = go []
 where
  go omissions declarations
    | repaired == declarations && null newOmissions =
        Right (declarations, reverse omissions)
    | projectionRepairMeasure repaired
        < projectionRepairMeasure declarations =
          go (reverse newOmissions ++ omissions) repaired
    | otherwise = Left $ projectionFailure
        "scope-reference repair did not decrease its structural measure"
   where
    defined = Set.fromList
      $ concatMap declarationOwnedRequirements declarations
    isResolved requirement =
      requirement `Set.member` defined || case requirement of
        TypeRequirement name -> isJust $ nameSpecial name
        ClassRequirement _ -> False
    unresolvedIn = filter (not . isResolved) . Set.toList
      . declarationMentions
    shedded = map shed declarations
    resolved = map fst shedded
    repaired = concat resolved
    newOmissions = concatMap snd shedded
    shed declaration = case unresolvedIn declaration of
      [] -> ([declaration], [])
      missing : _ -> case declaration of
        ClassDeclaration annotation name parameters superclasses methods
          | let (goodMethods, badMethods) =
                  partition (null . unresolvedMethod) methods
          , all (null . unresolvedConstraint) superclasses
          , not $ null badMethods ->
              ( [ ClassDeclaration annotation name parameters
                    superclasses goodMethods
                ]
              , [ mentionOmission (valueName method) badName
                | method <- badMethods
                , badName <- take 1 $ unresolvedMethod method
                ]
              )
        DataTypeDeclaration annotation name parameters _ ->
          ( [ AbstractTypeDeclaration annotation name
                (inferredDataTypeKind inferredKinds name parameters)
            ]
          , [ DjinnScopeOmission (renderCanonical name)
                ("its constructors mention "
                  ++ renderCanonical (requirementName missing)
                  ++ ", which is outside the Djinn scope; projected as an"
                  ++ " abstract type")
            ]
          )
        _ ->
          ( []
          , [mentionOmission (declarationSubjectName declaration) missing]
          )
     where
      -- Reuse the complete mention traversal so a method whose missing name
      -- appears only as a constraint class is shed just like one whose result
      -- type names an unavailable constructor.
      unresolvedMethod = filter (not . isResolved) . Set.toList
        . declarationMentions . ValueDeclaration
      unresolvedConstraint constraint = filter (not . isResolved)
        $ ClassRequirement (constraintClass constraint)
          : map (TypeRequirement . fst) (constraintTypeReferences constraint)
    mentionOmission subject missing = DjinnScopeOmission
      (renderCanonical subject)
      ("it mentions " ++ renderCanonical (requirementName missing)
        ++ ", which is outside the Djinn scope")

-- Seal the projection, repairing the specific failure Djinn reports until it
-- accepts the environment. Every repair strictly shrinks or simplifies the
-- declaration list, so the loop terminates.
sealWithRepairs
  :: Map.Map Name (Kind Void)
  -> [ScopeDeclaration]
  -> Either Diagnostic (DjinnSession, [DjinnScopeOmission])
sealWithRepairs inferredKinds = go []
 where
  go omissions declarations = case mkEnvironment declarations of
    Left failure -> Left $ projectionFailure $ show failure
    Right environment -> case mkDjinnSessionChecked environment of
      Right session -> Right (session, reverse omissions)
      Left failure -> case repairFor inferredKinds failure declarations of
        Just (repaired, newOmissions)
          | projectionRepairMeasure repaired
              < projectionRepairMeasure declarations ->
                -- 'go' keeps accumulated omissions reversed. Reverse this
                -- source-ordered batch too, so the final reversal preserves
                -- order within and across repair rounds.
                go (reverse newOmissions ++ omissions) repaired
          | otherwise -> Left $ projectionFailure
              "Djinn's requested repair did not decrease its structural measure"
        Nothing -> Left $ projectionFailure
          $ DjinnCore.renderEnvironmentEditFailure failure

repairFor
  :: Map.Map Name (Kind Void)
  -> DjinnCore.SynthesisEnvironmentError
  -> [ScopeDeclaration]
  -> Maybe ([ScopeDeclaration], [DjinnScopeOmission])
repairFor inferredKinds failure declarations = case failure of
  DjinnCore.RecursiveSynthesisDataTypes names
    -- Degrade full datatypes first; drop the named subjects outright when a
    -- previous degradation was insufficient, so every round makes progress.
    | degraded /= declarations -> Just (degraded, degradedOmissions)
    | otherwise -> Just $ dropWhere
        ((`elem` names) . declarationSubjectName)
        (const "it participates in datatype recursion Djinn cannot take")
   where
    recursive = Set.fromList names
    (degraded, degradedOmissions) = unzipOmissions
      $ map degradeRecursive declarations
    degradeRecursive declaration = case declaration of
      DataTypeDeclaration annotation name parameters _
        | name `Set.member` recursive ->
            ( AbstractTypeDeclaration annotation name
                (inferredDataTypeKind inferredKinds name parameters)
            , Just $ DjinnScopeOmission (renderCanonical name)
                "recursive datatype; projected as an abstract type"
            )
      _ -> (declaration, Nothing)
  DjinnCore.MissingSynthesisTypeKind name ->
    Just $ dropRequiring $ TypeRequirement name
  DjinnCore.MissingSynthesisClassKinds name ->
    Just $ dropRequiring $ ClassRequirement name
  DjinnCore.UnresolvedSynthesisClassKind name _ ->
    Just $ dropRequiring $ ClassRequirement name
  DjinnCore.SynthesisClassKindArityMismatch name _ _ -> Just $ dropWhere
    (declarationOwns $ ClassRequirement name)
    (const "Djinn rejected its kind")
  _ -> Nothing
 where
  dropRequiring requirement = dropWhere
    (\declaration ->
      declarationOwns requirement declaration
        || requirement `Set.member` declarationMentions declaration)
    (const $ "it mentions " ++ renderCanonical (requirementName requirement)
      ++ ", which Djinn cannot resolve")
  declarationOwns requirement =
    (requirement `elem`) . declarationOwnedRequirements
  dropWhere predicate reason =
    ( filter (not . predicate) declarations
    , [ DjinnScopeOmission
          (renderCanonical $ declarationSubjectName declaration)
          (reason declaration)
      | declaration <- declarations
      , predicate declaration
      ]
    )

parameterCountKind :: [TypeParameter String Void] -> Kind Void
parameterCountKind = foldr (const $ FunctionKind ProperTypeKind) ProperTypeKind

-- Every datatype originating in the checked inventory has an authoritative
-- inferred kind. The parameter-count shape is retained only as a defensive
-- fallback for a declaration supplied without a matching inventory entry;
-- ordinary REPL projection never takes that branch.
inferredDataTypeKind
  :: Map.Map Name (Kind Void)
  -> Name
  -> [TypeParameter String Void]
  -> Kind Void
inferredDataTypeKind inferredKinds name parameters =
  Map.findWithDefault (parameterCountKind parameters) name inferredKinds

arityKind :: Int -> Kind Void
arityKind arity = iterate (FunctionKind ProperTypeKind) ProperTypeKind !! arity

projectionFailure :: String -> Diagnostic
projectionFailure = contextualDiagnostic Error "DJEX_REPL_DJINN_PROJECTION"
  "cannot project the module scope into a Djinn session"
