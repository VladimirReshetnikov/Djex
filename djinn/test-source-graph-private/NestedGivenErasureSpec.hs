-- | Source preparation and actual checked-proof controls for the bounded
-- monomorphic nested-Given rule. These are not synthesis acceptance receipts.
module NestedGivenErasureSpec (tests) where

import Control.Monad (forM_)
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import qualified Djinn.Internal.ContextualInstantiation as I
import Djinn.Internal.Environment
  ( PreparedEnvironment, prepareGroundSynthesisEnvironment, prepareNestedGivenOpenings
  , prepareRootGivenOpening
  , nestedGivenOpeningSource, checkPreparedSynthesisTypesKinds
  , preparedEnvironmentSynthesisFormulaTranslator
  , preparedEnvironmentPolarizedFunctionPremises )
import Djinn.Internal.HTypes (HKind(KStar))
import Djinn.Internal.LJTFormula
  ( Formula(..), Symbol(Symbol), Term(..), applys, dictionarySymbol, opaqueTypeSymbol )
import Djinn.Internal.ProofCheck.Evidence (checkProofWithEvidence)
import Djinn.Internal.ProofEnv
  ( ProofEnvironment, prepareProofEnvironment, proofBindings, restoreProofTerm )
import qualified Djinn.Internal.SourceEvidence as S
import Djinn.Internal.SourceGraph (checkSourceClauseGraph)
import Language.Haskell.Synthesis.Constraint (Constraint(..))
import Language.Haskell.Synthesis.Declaration
  ( Declaration(..), DataConstructor(..), TypeParameter(..), ValueSignature(..) )
import qualified Language.Haskell.Synthesis.Environment as E
import qualified Language.Haskell.Synthesis.Generated as G
import Language.Haskell.Synthesis.Kind (Kind(ProperTypeKind))
import Language.Haskell.Synthesis.Name (Name, Boxity(Boxed), parseName)
import qualified Language.Haskell.Synthesis.Type as T
import qualified Language.Haskell.Synthesis.TypeAtom as A
import qualified Language.Haskell.Synthesis.TypedGenerated as Q

type SourceType = T.Type String

data Fixture = Fixture PreparedEnvironment S.SourceTypingContext
  I.ContextualInstantiation I.ContextualIntroduction S.RootGivenErasure

tests :: TestTree
tests = testGroup "checked nested monomorphic Given erasure"
  [ testCase "erase only nested evidence and preserve both ordinary argument orders" $ do
      fixture@(Fixture _ context helper introduction receipt) <- prepare
      forM_ [[x, y], [y, x]] $ \arguments -> do
        let environment = proofEnvironment fixture []
        h <- internal environment $ I.contextualInstantiationSymbol helper
        i <- internal environment $ I.contextualIntroductionSymbol introduction
        p <- internal environment providerName
        let term = outer $ Apply (Var consumer) $ Apply (Var i) $ Lam dictionary $
              applys (Var h) $ map Var $ [p, dictionary] ++ arguments
            expected = outer $ Apply (Var consumer) $ applys (Var providerName) $ map Var arguments
        evidence <- right $ checkProofWithEvidence (proofBindings environment)
          (S.rootGivenErasureGoal receipt) term
        candidate <- right $ S.lowerCheckedContextualSourceCandidate receipt context environment
          Set.empty Map.empty Map.empty target evidence
        admitted <- right $ S.admitCheckedContextualSourceCandidate receipt context environment
          Set.empty Map.empty Map.empty target evidence
        assertEqual "search admission changed a supported checked occurrence"
          (S.sourceCandidateClause candidate) $ S.sourceCandidateClause admitted
        assertEqual "nested erasure dropped or reordered ordinary arguments" expected $
          S.sourceCandidateErasedProof candidate
        let clause = S.sourceCandidateClause candidate
        graph <- right $ checkSourceClauseGraph 91 context clause
        assertEqual "nested erasure changed its exact source clause" clause $
          Q.eraseTermGraphToFunctionClause (G.clauseName clause) graph
        root <- maybe (fail "missing nested source root") pure $
          Q.lookupTermNode (Q.termGraphRoot graph) graph
        assertBool "nested graph lost its full source type" $
          A.alphaEquivalentClosedTypes goal $ Q.termNodeType root
        let introductions =
              [Q.contextEvidenceBinder $ Q.givenContextEvidence occurrence 0
              | (_, Q.TermNode _ (Q.TypedContextIntroduction occurrence _ _)) <- Q.termGraphNodes graph]
            uses = concat
              [map Q.contextEvidenceBinder $ Q.contextApplicationEvidence witness
              | (_, Q.TermNode _ (Q.TypedContextApplication _ _ witness)) <- Q.termGraphNodes graph]
        assertEqual "nested introduction lost its unique lexical dictionary" 1 $ length introductions
        assertEqual "nested application borrowed another introduction or slot" introductions uses
  , testCase "recursive introduction is a charged raw refusal, not malformed evidence" $ do
      fixture@(Fixture _ context helper introduction receipt) <- prepare
      let environment = proofEnvironment fixture []
          innerDictionary = Symbol "innerDictionary"
      h <- internal environment $ I.contextualInstantiationSymbol helper
      i <- internal environment $ I.contextualIntroductionSymbol introduction
      p <- internal environment providerName
      let inner = Apply (Var i) $ Lam innerDictionary $
            applys (Var h) $ map Var [p, innerDictionary, x, y]
          term = outer $ Apply (Var consumer) $ Apply (Var i) $ Lam dictionary $
            Apply (Var consumer) inner
      evidence <- right $ checkProofWithEvidence (proofBindings environment)
        (S.rootGivenErasureGoal receipt) term
      case S.admitCheckedContextualSourceCandidate receipt context environment
          Set.empty Map.empty Map.empty target evidence of
        Left (S.UnsupportedContextualErasure message) -> assertEqual
          "recursive helper refusal lost its exact scope reason"
          "contextual dictionary introduction overlaps an equal active Given" message
        Left failure -> fail $ "valid unsupported proof became an internal error: " ++ show failure
        Right _ -> fail "recursive helper acquired an ambiguous dictionary owner"
      rejectsChecked fixture environment term
  , testCase "admission never demotes a mismatched checked environment to an optional refusal" $ do
      fixture@(Fixture _ context helper introduction receipt) <- prepare
      let environment = proofEnvironment fixture []
          otherEnvironment = proofEnvironment fixture [(Symbol "unrelated", dictionaryFormula)]
      h <- internal environment $ I.contextualInstantiationSymbol helper
      i <- internal environment $ I.contextualIntroductionSymbol introduction
      p <- internal environment providerName
      evidence <- right $ checkProofWithEvidence (proofBindings environment)
        (S.rootGivenErasureGoal receipt) $ outer $ Apply (Var consumer) $ Apply (Var i) $
          Lam dictionary $ applys (Var h) $ map Var [p, dictionary, x, y]
      case S.admitCheckedContextualSourceCandidate receipt context otherEnvironment
          Set.empty Map.empty Map.empty target evidence of
        Left (S.InvalidContextualLowering _) -> pure ()
        Left failure -> fail $ "mismatched evidence became an optional refusal: " ++ show failure
        Right _ -> fail "admission accepted another checked environment"
  , testCase "an equal external dictionary cannot replace the actual nested binder" $ do
      fixture@(Fixture _ _ helper introduction _) <- prepare
      let foreignDictionary = Symbol "foreignDictionary"
          environment = proofEnvironment fixture [(foreignDictionary, dictionaryFormula)]
      h <- internal environment $ I.contextualInstantiationSymbol helper
      i <- internal environment $ I.contextualIntroductionSymbol introduction
      p <- internal environment providerName
      external <- internal environment foreignDictionary
      rejectsChecked fixture environment $ outer $ Apply (Var consumer) $ Apply (Var i) $
        Lam dictionary $ applys (Var h) $ map Var [p, external, x, y]
  , testCase "a shadowing dictionary lambda cannot borrow the removed nested binder" $ do
      fixture@(Fixture _ _ helper introduction _) <- prepare
      let environment = proofEnvironment fixture []
      h <- internal environment $ I.contextualInstantiationSymbol helper
      i <- internal environment $ I.contextualIntroductionSymbol introduction
      p <- internal environment providerName
      rejectsChecked fixture environment $ outer $ Apply (Var consumer) $ Apply (Var i) $
        Lam dictionary $ Apply
          (Lam dictionary $ applys (Var h) $ map Var [p, dictionary, x, y]) (Var dictionary)
  , testCase "a checked dictionary function cannot replace an actual introduction lambda" $ do
      fixture@(Fixture prepared _ _ introduction _) <- prepare
      result <- right $ preparedEnvironmentSynthesisFormulaTranslator prepared token
      let ready = Symbol "ready"
          environment = proofEnvironment fixture [(ready, dictionaryFormula :-> result)]
      i <- internal environment $ I.contextualIntroductionSymbol introduction
      r <- internal environment ready
      rejectsChecked fixture environment $ outer $ Apply (Var consumer) $ Apply (Var i) (Var r)
  , testCase "a nested binder cannot occur outside its checked lambda subtree" $ do
      fixture@(Fixture prepared _ helper introduction receipt) <- prepare
      result <- right $ preparedEnvironmentSynthesisFormulaTranslator prepared token
      let sink = Symbol "sink"
          environment = proofEnvironment fixture
            [(sink, PVar (opaqueTypeSymbol qualifiedToken) :-> dictionaryFormula :-> result)]
      h <- internal environment $ I.contextualInstantiationSymbol helper
      i <- internal environment $ I.contextualIntroductionSymbol introduction
      p <- internal environment providerName
      s <- internal environment sink
      let term = outer $ applys (Var s)
            [Apply (Var i) $ Lam dictionary $ applys (Var h) $ map Var [p, dictionary, x, y],
             Var dictionary]
      assertBool "a sibling reference acquired checker-owned proof evidence" $ isLeft $
        checkProofWithEvidence (proofBindings environment) (S.rootGivenErasureGoal receipt) term
  , testCase "equal active Givens and nested forall eigenvariables remain unsupported" $ do
      Fixture prepared _ _ _ _ <- prepare
      let duplicateRoot = T.ForallType [] [constraint atomA] goal
          quantified = T.ForallType ["b"] [constraint $ T.TypeVariable "b"] token
          quantifiedGoal = T.FunctionType (T.FunctionType quantified token) token
      duplicate <- right $ prepareNestedGivenOpenings prepared duplicateRoot goal
      nestedForall <- right $ prepareNestedGivenOpenings prepared quantifiedGoal quantifiedGoal
      assertEqual "an equal root/nested Given was silently assigned a slot" 0 $ length duplicate
      assertEqual "nested forall acquired an unowned eigenvariable" 0 $ length nestedForall
  , testCase "root specialization cannot bypass overlapping residual qualification scopes" $ do
      Fixture prepared _ _ _ _ <- prepare
      let unit = T.TupleType Boxed []
          body nestedConstraint = T.FunctionType unit $ T.FunctionType atomA $
            T.ForallType [] [nestedConstraint] token
          overlapBody = body $ constraint atomA
          overlap = T.ForallType [] [constraint atomA] overlapBody
          disjointBody = body $ constraint unit
          disjoint = T.ForallType [] [constraint atomA] disjointBody
          siblings = T.TupleType Boxed [qualifiedToken, qualifiedToken]
      root <- right $ prepareRootGivenOpening prepared overlap overlapBody
      nested <- right $ prepareNestedGivenOpenings prepared overlap overlapBody
      assertBool "a root helper could choose the outer Given before residual source checking"
        $ case root of Nothing -> True; Just _ -> False
      assertEqual "a nested plan bypassed the query-wide overlap refusal" 0 $ length nested
      allowedRoot <- right $ prepareRootGivenOpening prepared disjoint disjointBody
      allowedNested <- right $ prepareNestedGivenOpenings prepared disjoint disjointBody
      assertBool "distinct root/nested predicates were rejected"
        $ case allowedRoot of Just _ -> True; Nothing -> False
      assertEqual "distinct root/nested predicates lost their lexical bridge" 1 $ length allowedNested
      separate <- right $ prepareNestedGivenOpenings prepared siblings siblings
      assertEqual "equal sibling predicates were incorrectly treated as overlapping" 2 $ length separate
  , testCase "nested scope kind checking retains outer ambient kind equations" $ do
      Fixture prepared _ _ _ _ <- prepare
      let f = T.TypeVariable "f"
          a = T.TypeVariable "a"
          unit = T.TupleType Boxed []
          callback = T.ForallType [] [constraint unit] token
          kindGoal = T.FunctionType (T.TypeApplication f a) $
            T.FunctionType (T.FunctionType callback token) token
          g = T.TypeVariable "g"
          b = T.TypeVariable "b"
          scheme = T.ForallType ["g", "b"] [constraint b] $
            T.FunctionType (T.FunctionType (T.TypeApplication g b) $ T.TypeApplication g b) token
      openings <- right $ prepareNestedGivenOpenings prepared kindGoal kindGoal
      case openings of
        [opening] -> do
          right $ I.checkNestedContextualInstantiationKinds prepared opening scheme [f, unit]
          assertBool "nested selection retuned outer a from proper to unary" $ isLeft $
            I.checkNestedContextualInstantiationKinds prepared opening scheme [a, unit]
        _ -> fail "ambient-kind fixture did not retain one nested scope"
  , testCase "checked constructor fields cannot hide equal root Givens" $ do
      prepared <- prepareFieldScopes
      let applied owner argument = T.TypeApplication (T.TypeConstructor $ name owner) argument
          source result = T.ForallType [] [constraint atomA] $ T.FunctionType atomA result
          body result = T.FunctionType atomA result
      forM_ [applied "Box" qualifiedToken, applied "Hidden" atomA, applied "Indirect" atomA] $ \result -> do
        root <- right $ prepareRootGivenOpening prepared (source result) (body result)
        nested <- right $ prepareNestedGivenOpenings prepared (source result) (body result)
        assertBool "a qualified type argument or reachable field bypassed root admission" $
          case root of Nothing -> True; Just _ -> False
        assertEqual "a qualified field bypassed nested admission" 0 $ length nested
  , testCase "constructor specialization preserves distinct predicates and sibling scopes" $ do
      prepared <- prepareFieldScopes
      let unit = T.TupleType Boxed []
          hidden argument = T.TypeApplication (T.TypeConstructor $ name "Hidden") argument
          result = T.TupleType Boxed [hidden unit, hidden unit]
          body = T.FunctionType atomA result
          source = T.ForallType [] [constraint atomA] body
      root <- right $ prepareRootGivenOpening prepared source body
      assertBool "equal sibling field scopes or distinct C arguments were conflated" $
        case root of Just _ -> True; Nothing -> False
  , testCase "recursive field inspection closes exact cycles and refuses growing instances" $ do
      prepared <- prepareFieldScopes
      let query owner = T.FunctionType atomA $
            T.TypeApplication (T.TypeConstructor $ name owner) atomA
          source body = T.ForallType [] [constraint atomA] body
      stable <- right $ prepareRootGivenOpening prepared (source $ query "Loop") $ query "Loop"
      growing <- right $ prepareRootGivenOpening prepared (source $ query "Growing") $ query "Growing"
      assertBool "an unchanged recursive field state was unnecessarily unfolded or rejected" $
        case stable of Just _ -> True; Nothing -> False
      assertBool "growing recursive type arguments bypassed the finite inspection boundary" $
        case growing of Nothing -> True; Just _ -> False
  , testCase "opening a qualified constructor field retains source incompleteness through aliases" $
      forM_ [False, True] $ \throughAlias -> do
        (plainFormula, plainIncomplete) <- qualifiedFieldProjection throughAlias False
        (qualifiedFormula, qualifiedIncomplete) <- qualifiedFieldProjection throughAlias True
        assertEqual "qualification honesty changed the existing positive body formula"
          plainFormula qualifiedFormula
        assertBool "an omitted positive field dictionary authorized negative evidence"
          qualifiedIncomplete
        assertBool "the same unconstrained field became incomplete" $ not plainIncomplete
  ]

-- A function premise is lowered negatively, so its Hidden argument and that
-- constructor's field are positive. This observes the same compiler honesty
-- bit used by goal projection, without adding a test-only compiler entrance.
qualifiedFieldProjection :: Bool -> Bool -> IO (Formula, Bool)
qualifiedFieldProjection throughAlias qualified = do
  let a = T.TypeVariable "a"
      parameters = [TypeParameter "a" Nothing]
      body = T.FunctionType a token
      field = if qualified then T.ForallType [] [constraint a] body else body
      selectedField = if throughAlias
        then T.TypeApplication (T.TypeConstructor $ name "Field") a
        else field
      sourceDeclarations :: [Declaration String Void ()]
      sourceDeclarations =
        [ ClassDeclaration () (name "C") [TypeParameter "a" $ Just ProperTypeKind] []
            [ValueSignature () (name "make") body]
        , AbstractTypeDeclaration () (name "A") ProperTypeKind
        , AbstractTypeDeclaration () (name "Token") ProperTypeKind
        ] ++
        [TypeSynonymDeclaration () (name "Field") parameters field | throughAlias] ++
        [ DataTypeDeclaration () (name "Hidden") parameters
            [DataConstructor () (name "Hidden") [selectedField]]
        , ValueDeclaration $ ValueSignature () (name "consumer") $
            T.FunctionType (T.TypeApplication (T.TypeConstructor $ name "Hidden") atomA) token
        ]
  environment <- right $ E.mkEnvironment sourceDeclarations
  prepared <- right $ prepareGroundSynthesisEnvironment environment
  case preparedEnvironmentPolarizedFunctionPremises prepared of
    ((owner, formula) : _, incomplete, _) -> do
      assertEqual "a class method displaced the sole ordinary premise" (Symbol "consumer") owner
      pure (formula, incomplete)
    _ -> fail "the qualified-field fixture lost its ordinary premise"

prepareFieldScopes :: IO PreparedEnvironment
prepareFieldScopes = do
  let a = T.TypeVariable "a"
      applied owner argument = T.TypeApplication (T.TypeConstructor $ name owner) argument
      dataType owner constructors = DataTypeDeclaration () (name owner)
        [TypeParameter "a" Nothing] constructors
      constructor owner fields = DataConstructor () (name owner) fields
      sourceDeclarations :: [Declaration String Void ()]
      sourceDeclarations =
        [ ClassDeclaration () (name "C") [TypeParameter "a" $ Just ProperTypeKind] [] []
        , AbstractTypeDeclaration () (name "A") ProperTypeKind
        , AbstractTypeDeclaration () (name "Token") ProperTypeKind
        , dataType "Box" [constructor "Box" [a]]
        , dataType "Hidden" [constructor "Hidden" [T.ForallType [] [constraint a] token]]
        , dataType "Indirect" [constructor "Indirect" [applied "Hidden" a]]
        , dataType "Loop" [constructor "Stop" [a], constructor "More" [applied "Loop" a]]
        , dataType "Growing" [constructor "Grow" [applied "Growing" $ T.TupleType Boxed [a, a]]]
        ]
  environment <- right $ E.mkEnvironment sourceDeclarations
  right $ prepareGroundSynthesisEnvironment environment

prepare :: IO Fixture
prepare = do
  environment <- right $ E.mkEnvironment declarations
  prepared <- right $ prepareGroundSynthesisEnvironment environment
  openings <- right $ prepareNestedGivenOpenings prepared goal goal
  opening <- case openings of
    [one] -> pure one
    _ -> fail "fixture did not retain exactly one nested qualification"
  assertEqual "nested preparation changed the exact source qualification" qualifiedToken $
    nestedGivenOpeningSource opening
  right $ I.checkNestedContextualInstantiationKinds prepared opening providerScheme [atomA]
  request <- right $ I.prepareContextualInstantiation
    (\ty -> checkPreparedSynthesisTypesKinds prepared [(KStar, ty)])
    (preparedEnvironmentSynthesisFormulaTranslator prepared) providerScheme [atomA]
  helper <- case I.contextualInstantiations $ I.contextualInstantiationAxioms [request] of
    [one] -> pure one
    _ -> fail "fixture did not retain one specialization"
  introductions <- right $ I.contextualIntroductions prepared openings
  introduction <- case introductions of
    [one] -> pure one
    _ -> fail "fixture did not retain one introduction"
  let context = S.sourceTypingContext prepared goal
  receipt <- right $ S.nestedGivenErasure context [helper] introductions
  pure $ Fixture prepared context helper introduction receipt
 where
  declarations :: [Declaration String Void ()]
  declarations =
    [ ClassDeclaration () (name "C") [TypeParameter "a" $ Just ProperTypeKind] [] []
    , AbstractTypeDeclaration () (name "A") ProperTypeKind
    , AbstractTypeDeclaration () (name "Token") ProperTypeKind
    , ValueDeclaration $ ValueSignature () (name "provider") providerScheme
    ]

proofEnvironment :: Fixture -> [(Symbol, Formula)] -> ProofEnvironment
proofEnvironment (Fixture _ _ helper introduction _) extra = prepareProofEnvironment (Symbol "answer") $
  [(I.contextualInstantiationSymbol helper, I.contextualInstantiationFormula helper),
   (I.contextualIntroductionSymbol introduction, I.contextualIntroductionFormula introduction),
   (providerName, PVar $ opaqueTypeSymbol providerScheme)] ++ extra

rejectsChecked :: Fixture -> ProofEnvironment -> Term -> IO ()
rejectsChecked (Fixture _ context _ _ receipt) environment term = do
  evidence <- right $ checkProofWithEvidence (proofBindings environment)
    (S.rootGivenErasureGoal receipt) term
  assertBool "invalid lexical erasure passed the production lowerer" $ isLeft $
    S.lowerCheckedContextualSourceCandidate receipt context environment
      Set.empty Map.empty Map.empty target evidence

internal :: ProofEnvironment -> Symbol -> IO Symbol
internal environment external = case
  [candidate | (candidate, _) <- proofBindings environment,
    restoreProofTerm environment (Var candidate) == Var external] of
  [candidate] -> pure candidate
  _ -> fail "external test premise has no unique checked identity"

goal, qualifiedToken, providerScheme, atomA, token :: SourceType
goal = T.FunctionType atomA $ T.FunctionType atomA $
  T.FunctionType (T.FunctionType qualifiedToken token) token
qualifiedToken = T.ForallType [] [constraint atomA] token
providerScheme = T.ForallType ["b"] [constraint $ T.TypeVariable "b"] $
  T.FunctionType (T.TypeVariable "b") $ T.FunctionType (T.TypeVariable "b") token
atomA = T.TypeConstructor $ name "A"
token = T.TypeConstructor $ name "Token"

constraint :: SourceType -> Constraint SourceType
constraint ty = Constraint (name "C") [ty]

dictionaryFormula :: Formula
dictionaryFormula = PVar $ dictionarySymbol $ constraint atomA

x, y, consumer, dictionary, providerName :: Symbol
x = Symbol "first"
y = Symbol "second"
consumer = Symbol "consumer"
dictionary = Symbol "nestedDictionary"
providerName = Symbol "provider"

outer :: Term -> Term
outer = Lam x . Lam y . Lam consumer

target :: G.DefinitionName
target = either (error . show) id $ G.mkDefinitionName $ name "answer"

name :: String -> Name
name = either (error . show) id . parseName

right :: Show error => Either error value -> IO value
right = either (fail . show) pure
