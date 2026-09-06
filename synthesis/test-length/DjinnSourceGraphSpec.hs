module DjinnSourceGraphSpec (tests) where

import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Unsafe.Coerce (unsafeCoerce)

import Djinn.Internal.Environment
  ( PreparedEnvironment, prepareGroundSynthesisEnvironment, preparedEnvironmentInventory )
import Djinn.Internal.SourceGraph (SourceGraphError, checkSourceClauseGraph)
import Djinn.Internal.SourceTypingContext (sourceTypingContext)
import qualified Language.Haskell.Djex as Djex
import qualified Language.Haskell.Synthesis.Candidate as C
import Language.Haskell.Synthesis.Declaration
  ( Declaration(..), DataConstructor(..), TypeParameter(..) )
import qualified Language.Haskell.Synthesis.Environment as E
import qualified Language.Haskell.Synthesis.Generated as G
import qualified Language.Haskell.Synthesis.Inventory as I
import qualified Language.Haskell.Synthesis.Internal.TypedCandidate as InternalCandidate
import Language.Haskell.Synthesis.Name (Name, Boxity(Boxed), parseName)
import qualified Language.Haskell.Synthesis.Semantic.Length as L
import qualified Language.Haskell.Synthesis.Semantic.Length.Evaluate as V
import qualified Language.Haskell.Synthesis.Semantic.Length.Problem as P
import qualified Language.Haskell.Synthesis.Type as T
import qualified Language.Haskell.Synthesis.TypedCandidate as CTyped
import qualified Language.Haskell.Synthesis.TypedGenerated as Q

type SourceType = T.Type String
type GraphType = T.Type (T.Variable String)
type Graph = Q.TermGraph GraphType String
type Candidate = CTyped.TypedCandidate SourceGraphError GraphType String
  (C.Candidate GraphType () (G.FunctionClause String))

-- This is private source-checker -> consumer coverage. It supplies an exact
-- retained source clause, not a claimed public recursive-list search result.
-- Djinn's historical abstraction of negative recursive data remains intact.
tests :: TestTree
tests = testGroup "Djinn source graph to Length"
  [ testCase "interpret a checked recursive tail case and replay its counterexample" $ do
      (prepared, clause, graph) <- fixture
      session <- exactSession prepared
      contract <- right $ P.sealLengthContractInSession session taggedGoal identityLaw
      problem <- right $ P.sealExactSpineCaseLengthTypedCandidateProblem
        P.defaultLengthProblemLimits session contract $ candidate clause graph
      let checked = P.checkedLengthProblemCandidate problem
          input = L.LengthVariable $ L.LengthInput 0
          expected = L.LengthIf (L.LengthEqual input $ L.LengthLiteral 0)
            (L.LengthLiteral 0) (L.LengthMonus input $ L.LengthLiteral 1)
      assertEqual "source case lost its exact zero/step interpretation"
        expected $ P.checkedLengthCandidateResult checked
      assertEqual "tail case acquired an assumed provider law"
        [] $ P.checkedLengthCandidateUsedProviders checked
      assertEqual "tail case changed its observed input arity"
        1 $ P.checkedLengthProblemInputCount problem
      evidence <- counterexample $ V.validateLengthProblemCounterexample
        V.defaultLengthEvaluationLimits problem $ V.LengthProblemAssignment [3]
      receipt <- right $ Djex.replayBehavioralEvidence
        (P.checkedLengthProblemBehavioralProblem problem) evidence
      assertEqual "replay changed the independently supplied input"
        [3] $ V.validatedLengthCounterexampleInputs receipt
      assertEqual "tail case did not refute identity length by 3 -> 2"
        2 $ V.validatedLengthCounterexampleResult receipt
      noCounterexample <- right $ V.validateLengthProblemCounterexample
        V.defaultLengthEvaluationLimits problem $ V.LengthProblemAssignment [0]
      case noCounterexample of
        Nothing -> pure ()
        Just _ -> assertFailure "the zero branch incorrectly refuted identity length"

  , testCase "ordinary Length policy cannot acquire recursive case authority" $ do
      (prepared, clause, graph) <- fixture
      session <- right $ P.sealRoleAwareLengthSession L.defaultLengthLimits roles
        (sourceInventory prepared) spineModel []
      contract <- right $ P.sealLengthContractInSession session taggedGoal identityLaw
      case P.sealRoleAwareLengthTypedCandidateProblem P.defaultLengthProblemLimits
          session contract $ candidate clause graph of
        Left (P.LengthProblemTermGraphFingerprintRejected
            (Djex.TermGraphFingerprintSharedResealError
              (Q.UnknownConstructorPatternSchema _ constructor _))) ->
          assertBool "ordinary policy rejected an unrelated schema" $
            constructor == name "Nil" || constructor == name "Cons"
        Left other -> assertFailure $ "unexpected ordinary-policy rejection: " ++ show other
        Right _ -> assertFailure "ordinary Length policy admitted a source case"

  , testCase "a reordered recursive schema cannot borrow the checked source graph" $ do
      (prepared, clause, graph) <- fixture
      session <- exactSession prepared
      contract <- right $ P.sealLengthContractInSession session taggedGoal identityLaw
      foreignPrepared <- prepare True
      foreignSession <- exactSession foreignPrepared
      assertBool "different source schemas shared an inventory fingerprint" $
        P.lengthSessionInventoryFingerprint session /=
          P.lengthSessionInventoryFingerprint foreignSession
      case P.sealExactSpineCaseLengthTypedCandidateProblem P.defaultLengthProblemLimits
          foreignSession contract $ candidate clause graph of
        Left P.LengthProblemContractContextMismatch -> pure ()
        Left other -> assertFailure $ "unexpected foreign-contract rejection: " ++ show other
        Right _ -> assertFailure "foreign source schema reused the original contract"
      foreignContract <- right $
        P.sealLengthContractInSession foreignSession taggedGoal identityLaw
      case P.sealExactSpineCaseLengthTypedCandidateProblem P.defaultLengthProblemLimits
          foreignSession foreignContract $ candidate clause graph of
        Left (P.LengthProblemTermGraphFingerprintRejected
            (Djex.TermGraphFingerprintSharedResealError
              Q.ConstructorPatternFieldTypeMismatch{})) -> pure ()
        Left other -> assertFailure $ "unexpected foreign-graph rejection: " ++ show other
        Right _ -> assertFailure "foreign source schema reused the original constructor fields"
  ]

name :: String -> Name
name = either (error . show) id . parseName

list :: T.Type variable -> T.Type variable
list = T.TypeApplication $ T.TypeConstructor $ name "List"

sourceGoal :: SourceType
sourceGoal = T.FunctionType spine spine
 where
  spine = list $ T.TupleType Boxed []

taggedGoal :: GraphType
taggedGoal = fmap T.FlexibleVariable sourceGoal

roles :: [L.LengthTargetArgumentRole]
roles = [L.LengthObservedSpine]

spineModel :: L.LengthSpineModelSource
spineModel = L.DeclaredListSpine (name "List") (name "Nil") (name "Cons")

identityLaw :: L.LengthContractSource
identityLaw = L.LengthContractSource
  { L.lengthContractPrecondition = L.LengthTruth True
  , L.lengthContractPostcondition = L.LengthEqual
      (L.LengthVariable L.LengthResult) (L.LengthVariable $ L.LengthInput 0)
  }

prepare :: Bool -> IO PreparedEnvironment
prepare recursiveFirst = do
  let payload = T.TypeVariable "element"
      fields = if recursiveFirst then [list payload, payload] else [payload, list payload]
      declaration = DataTypeDeclaration () (name "List")
        [TypeParameter "element" Nothing]
        [DataConstructor () (name "Nil") [], DataConstructor () (name "Cons") fields]
        :: Declaration String Void ()
  environment <- right $ E.mkEnvironment [declaration]
  right $ prepareGroundSynthesisEnvironment environment

sourceInventory :: PreparedEnvironment -> I.Inventory (T.Variable String) ()
sourceInventory = I.tagInventoryTypeVariables . preparedEnvironmentInventory

exactSession :: PreparedEnvironment -> IO (P.CheckedLengthSession String ())
exactSession prepared = right $ P.sealExactSpineCaseLengthSession
  L.defaultLengthLimits roles (sourceInventory prepared) spineModel []

fixture :: IO (PreparedEnvironment, G.FunctionClause String, Graph)
fixture = do
  prepared <- prepare False
  target <- right $ G.mkDefinitionName $ name "checkedTail"
  let clause = G.FunctionClause target [G.Bind "input"] $ G.Case (G.Local "input")
        [ (G.Constructor (name "Nil") [], G.Global $ name "Nil")
        , (G.Constructor (name "Cons") [G.Wildcard, G.Bind "rest"], G.Local "rest")
        ]
  graph <- right $ checkSourceClauseGraph 29
    (sourceTypingContext prepared sourceGoal) clause
  assertEqual "source checker changed the retained case clause"
    clause $ Q.eraseTermGraphToFunctionClause target graph
  root <- maybe (fail "source graph has no root") pure $
    Q.lookupTermNode (Q.termGraphRoot graph) graph
  assertEqual "source checker changed the recursive nominal goal"
    taggedGoal $ Q.termNodeType root
  assertBool "source checking erased the recursive case" $
    any (\(_, node) -> case Q.termNodeForm node of
      Q.TypedCase{} -> True
      _ -> False) $ Q.termGraphNodes graph
  pure (prepared, clause, graph)

-- TEST ONLY: the same-package home copy of Internal.TypedCandidate has a
-- distinct Cabal unit identity from the library's opaque public type. As in
-- Spec.adversarialTypedCandidate, this converts that unit identity alone.
-- Graph type variables, local names, occurrences, and the exact clause remain
-- untouched. No production association constructor or coercion is exposed.
candidate :: G.FunctionClause String -> Graph -> Candidate
candidate clause graph = unsafeCoerce $ InternalCandidate.mkTypedCandidate
  (C.Candidate clause [] () :: C.Candidate GraphType () (G.FunctionClause String))
  (Right graph :: Either SourceGraphError Graph)

right :: Show failure => Either failure value -> IO value
right = either (fail . show) pure

counterexample :: Show failure => Either failure (Maybe evidence) -> IO evidence
counterexample result = do
  observed <- right result
  maybe (fail "the violating assignment produced no replayable evidence") pure observed
