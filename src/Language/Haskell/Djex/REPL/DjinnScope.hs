-- | Projection of the shared module scope into a checked Djinn session.
--
-- The REPL's uniform environment is authoritative on the Exference side,
-- where the source workspace is parsed and scoped. This module projects the
-- same scope into Djinn so both backends see the loaded declarations. Djinn's
-- grammar is stricter than the neutral vocabulary, so the projection degrades
-- rather than fails: unrepresentable declarations become abstract types where
-- that preserves meaning and are omitted with a recorded reason otherwise.
--
-- Value declarations become LJT axioms, and axiom sets of even moderate size
-- make Djinn's otherwise-terminating proof search intractable. They are
-- therefore excluded unless the caller opts in ('IncludeDjinnAxioms'), which
-- the REPL exposes as the @djinn-axioms@ setting.
module Language.Haskell.Djex.REPL.DjinnScope
  ( DjinnAxiomPolicy (..)
  , DjinnProjection (..)
  , DjinnScopeOmission (..)
  , declarationOwnedNames
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
  , mkDjinnSessionChecked
  )
import Language.Haskell.Synthesis.Constraint (Constraint (..))
import Language.Haskell.Synthesis.Declaration
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error)
  , contextualDiagnostic
  )
import Language.Haskell.Synthesis.Environment (mkEnvironment)
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

-- | Whether scope-visible value declarations become Djinn proof axioms.
data DjinnAxiomPolicy
  = ExcludeDjinnAxioms
  | IncludeDjinnAxioms
  deriving (Eq, Show)

-- | One sealed Djinn session and everything the projection left out.
data DjinnProjection = DjinnProjection
  { djinnProjectionSession :: DjinnSession
  , djinnProjectionOmissions :: [DjinnScopeOmission]
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

-- | Every name a declaration makes usable: its subject plus constructors and
-- class methods. Shared by scope projection and the REPL's @:info@ lookup.
declarationOwnedNames :: Declaration variable kind annotation -> [Name]
declarationOwnedNames declaration =
  declarationSubjectName declaration : case declaration of
    DataTypeDeclaration _ _ _ constructors -> map constructorName constructors
    ClassDeclaration _ _ _ _ methods -> map valueName methods
    _ -> []

type ScopeDeclaration = Declaration String Void ()

-- | Project the unqualified-visible part of the shared environment into a
-- Djinn session. The input declarations use canonical names; the projection
-- renames them to their in-scope unqualified spellings, because Djinn's
-- declaration grammar has no qualified type, class, or constructor names.
projectDjinnScope
  :: DjinnAxiomPolicy
  -> [ScopeDeclaration]
  -> Set.Set Name
  -> Either Diagnostic DjinnProjection
projectDjinnScope policy declarations visible = do
  let (shaped, shapeOmissions) = shapeDeclarations policy visible declarations
      (renamed, renameOmissions) = renameDeclarations shaped
      (grounded, recursionOmissions) = degradeRecursiveDataTypes renamed
      (admitted, admissionOmissions) = admitDeclarations grounded
      (stubbed, stubOmissions) = stubUnknownReferences admitted
      (resolved, referenceOmissions) = resolveScopeReferences stubbed
  (session, sealOmissions) <- sealWithRepairs resolved
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
    }

-- Scope filtering and structural policy. Invisible declarations vanish
-- silently, exactly as they do from Exference's search scope; visible
-- declarations that Djinn cannot take whole are degraded or omitted loudly.
shapeDeclarations
  :: DjinnAxiomPolicy
  -> Set.Set Name
  -> [ScopeDeclaration]
  -> ([ScopeDeclaration], [DjinnScopeOmission])
shapeDeclarations policy visible declarations =
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
  isVisible name = name `Set.member` visible

  shape declaration (keptSoFar, omitted, instances) = case declaration of
    InstanceDeclaration {} -> (keptSoFar, omitted, instances + 1)
    ValueDeclaration signature
      | not $ isVisible $ valueName signature -> skip
      | policy == IncludeDjinnAxioms -> keep declaration
      | otherwise -> omit (valueName signature)
          "value axioms are excluded; :set djinn-axioms on to include them"
    TypeSynonymDeclaration _ name _ _
      | isVisible name -> keep declaration
      | otherwise -> skip
    AbstractTypeDeclaration _ name _
      | isVisible name -> keep declaration
      | otherwise -> skip
    DataTypeDeclaration annotation name parameters constructors
      | not $ isVisible name -> skip
      | all (isVisible . constructorName) constructors -> keep declaration
      | otherwise ->
          ( AbstractTypeDeclaration annotation name
              (parameterCountKind parameters) : keptSoFar
          , DjinnScopeOmission (renderCanonical name)
              "some constructors are hidden; projected as an abstract type"
              : omitted
          , instances
          )
    ClassDeclaration annotation name parameters superclasses methods
      | isVisible name -> keep $ ClassDeclaration annotation name parameters
          superclasses (filter (isVisible . valueName) methods)
      | otherwise -> skip
   where
    skip = (keptSoFar, omitted, instances)
    keep shapedDeclaration = (shapedDeclaration : keptSoFar, omitted, instances)
    omit name reason =
      ( keptSoFar
      , DjinnScopeOmission (renderCanonical name) reason : omitted
      , instances
      )

-- Rename canonical names to their unqualified spellings, dropping any
-- declaration whose unqualified spelling is claimed by a different canonical
-- name. References to renamed names follow the same map; references to names
-- outside it keep their canonical spelling and are resolved by stubbing.
renameDeclarations
  :: [ScopeDeclaration]
  -> ([ScopeDeclaration], [DjinnScopeOmission])
renameDeclarations declarations = (renamed, omissions)
 where
  owned = concatMap declarationOwnedNames declarations
  (forward, ambiguous) = renameMap owned
  contested declaration =
    filter (`Set.member` ambiguous) $ declarationOwnedNames declaration
  (renamed, omissions) = partitionEithers
    [ case contested declaration of
        [] -> Left $ renameDeclaration forward declaration
        name : _ -> Right $ DjinnScopeOmission (renderCanonical name)
          "its unqualified spelling is ambiguous in this scope"
    | declaration <- declarations
    ]

renameMap :: [Name] -> (Map.Map Name Name, Set.Set Name)
renameMap = finish . foldl' claim (Map.empty, Map.empty, Set.empty)
 where
  claim (forward, owners, ambiguous) name = case unqualifyName name of
    Nothing -> (forward, owners, Set.insert name ambiguous)
    Just unqualified -> case Map.lookup unqualified owners of
      Just owner
        | owner /= name ->
            (forward, owners, Set.insert owner $ Set.insert name ambiguous)
      _ ->
        ( Map.insert name unqualified forward
        , Map.insert unqualified name owners
        , ambiguous
        )
  finish (forward, _, ambiguous) =
    (Map.withoutKeys forward ambiguous, ambiguous)

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

-- Djinn's LJT calculus cannot eliminate recursive datatypes, so they stay
-- available as opaque abstract types instead of disappearing from signatures.
degradeRecursiveDataTypes
  :: [ScopeDeclaration]
  -> ([ScopeDeclaration], [DjinnScopeOmission])
degradeRecursiveDataTypes declarations
  | Set.null recursive = (declarations, [])
  | otherwise = unzipOmissions $ map degrade declarations
 where
  recursive = recursiveDataTypeNames declarations
  degrade declaration = case declaration of
    DataTypeDeclaration annotation name parameters _
      | name `Set.member` recursive ->
          ( AbstractTypeDeclaration annotation name
              (parameterCountKind parameters)
          , Just $ DjinnScopeOmission (renderCanonical name)
              "recursive datatype; projected as an abstract type"
          )
    _ -> (declaration, Nothing)

unzipOmissions
  :: [(declaration, Maybe DjinnScopeOmission)]
  -> ([declaration], [DjinnScopeOmission])
unzipOmissions results = (map fst results, mapMaybe snd results)

-- Per-declaration admission through the exact conversion Djinn's raw layer
-- applies. A class sheds unrepresentable methods before the whole class is
-- given up on.
admitDeclarations
  :: [ScopeDeclaration]
  -> ([ScopeDeclaration], [DjinnScopeOmission])
admitDeclarations = foldr admit ([], [])
 where
  admit declaration (kept, omitted) = case declaration of
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
        Right () -> (shrunk : kept, methodOmissions ++ omitted)
        Left failure -> (kept, describeOmission name failure : omitted)
    _ -> case checkDeclaration declaration of
      Right () -> (declaration : kept, omitted)
      Left failure ->
        ( kept
        , describeOmission (declarationSubjectName declaration) failure
            : omitted
        )
  admissibleMethod signature =
    checkDeclaration (ValueDeclaration signature) == Right ()

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

-- Referenced-but-undeclared nominal type constructors become abstract stubs
-- with an arity-derived kind, keeping declarations whose signatures mention
-- out-of-scope types usable instead of cascading into omissions. Structural
-- names (functions, tuples, lists) are native to Djinn and never stubbed.
stubUnknownReferences
  :: [ScopeDeclaration]
  -> ([ScopeDeclaration], [DjinnScopeOmission])
stubUnknownReferences declarations = (declarations ++ stubs, omissions)
 where
  defined = Set.fromList $ concatMap declarationOwnedNames declarations
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
    , let stub = AbstractTypeDeclaration () name (arityKind arity)
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

-- | Every name a declaration mentions that sealing may require to be
-- declared: referenced type constructors and constraint classes.
declarationMentions :: ScopeDeclaration -> Set.Set Name
declarationMentions declaration = Set.union
  (Set.fromList $ map fst $ declarationTypeReferences declaration)
  (Set.fromList $ constraintClasses declaration)
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

-- Names that remain undefined after stubbing cannot survive Djinn's closed
-- kind inference, so the scope is resolved to a fixpoint by shedding exactly
-- the affected pieces: values and synonyms are dropped, classes lose the
-- offending methods, and datatypes degrade to abstract types.
resolveScopeReferences
  :: [ScopeDeclaration]
  -> ([ScopeDeclaration], [DjinnScopeOmission])
resolveScopeReferences = go (0 :: Int) []
 where
  -- The declaration count strictly shrinks or a datatype becomes abstract in
  -- every looping round, but the seal loop backstops this bound anyway.
  go rounds omissions declarations
    | rounds > 200 = (declarations, omissions)
    | null newOmissions = (declarations, omissions)
    | otherwise = go (rounds + 1) (omissions ++ newOmissions) (concat resolved)
   where
    defined = Set.fromList $ concatMap declarationOwnedNames declarations
    isResolved name =
      name `Set.member` defined || isJust (nameSpecial name)
    unresolvedIn = filter (not . isResolved) . Set.toList
      . declarationMentions
    shedded = map shed declarations
    resolved = map fst shedded
    newOmissions = concatMap snd shedded
    shed declaration = case unresolvedIn declaration of
      [] -> ([declaration], [])
      missing : _ -> case declaration of
        ClassDeclaration annotation name parameters superclasses methods
          | let (goodMethods, badMethods) =
                  partition (null . unresolvedMethod) methods
          , all (null . unresolvedConstraint) superclasses
          , not $ null goodMethods ->
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
                (parameterCountKind parameters)
            ]
          , [ DjinnScopeOmission (renderCanonical name)
                ("its constructors mention " ++ renderCanonical missing
                  ++ ", which is outside the Djinn scope; projected as an"
                  ++ " abstract type")
            ]
          )
        _ ->
          ( []
          , [mentionOmission (declarationSubjectName declaration) missing]
          )
     where
      unresolvedMethod = filter (not . isResolved) . map fst
        . typeReferences . valueType
      unresolvedConstraint constraint = filter (not . isResolved)
        $ constraintClass constraint
          : map fst (constraintTypeReferences constraint)
    mentionOmission subject missing = DjinnScopeOmission
      (renderCanonical subject)
      ("it mentions " ++ renderCanonical missing
        ++ ", which is outside the Djinn scope")

-- Seal the projection, repairing the specific failure Djinn reports until it
-- accepts the environment. Every repair strictly shrinks or simplifies the
-- declaration list, so the loop terminates.
sealWithRepairs
  :: [ScopeDeclaration]
  -> Either Diagnostic (DjinnSession, [DjinnScopeOmission])
sealWithRepairs = go (0 :: Int) []
 where
  limit = 200
  go rounds omissions declarations
    | rounds > limit = Left
        $ projectionFailure "the projection repair loop did not converge"
    | otherwise = case mkEnvironment declarations of
        Left failure -> Left $ projectionFailure $ show failure
        Right environment -> case mkDjinnSessionChecked environment of
          Right session -> Right (session, reverse omissions)
          Left failure -> case repairFor failure declarations of
            Just (repaired, newOmissions)
              | repaired /= declarations ->
                  go (rounds + 1) (newOmissions ++ omissions) repaired
            _ -> Left $ projectionFailure
              $ DjinnCore.renderEnvironmentEditFailure failure

repairFor
  :: DjinnCore.SynthesisEnvironmentError
  -> [ScopeDeclaration]
  -> Maybe ([ScopeDeclaration], [DjinnScopeOmission])
repairFor failure declarations = case failure of
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
                (parameterCountKind parameters)
            , Just $ DjinnScopeOmission (renderCanonical name)
                "recursive datatype; projected as an abstract type"
            )
      _ -> (declaration, Nothing)
  DjinnCore.MissingSynthesisTypeKind name -> Just $ dropMentioning name
  DjinnCore.MissingSynthesisClassKinds name -> Just $ dropMentioning name
  DjinnCore.UnresolvedSynthesisClassKind name _ -> Just $ dropMentioning name
  DjinnCore.SynthesisClassKindArityMismatch name _ _ -> Just $ dropWhere
    ((== name) . declarationSubjectName)
    (const "Djinn rejected its kind")
  _ -> Nothing
 where
  dropMentioning name = dropWhere
    ((name `Set.member`) . declarationMentions)
    (const $ "it mentions " ++ renderCanonical name
      ++ ", which Djinn cannot resolve")
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

arityKind :: Int -> Kind Void
arityKind arity = iterate (FunctionKind ProperTypeKind) ProperTypeKind !! arity

projectionFailure :: String -> Diagnostic
projectionFailure = contextualDiagnostic Error "DJEX_REPL_DJINN_PROJECTION"
  "cannot project the module scope into a Djinn session"
