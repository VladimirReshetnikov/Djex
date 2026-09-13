-- Checked proof -> cleanup -> independently checked source graph. These
-- fixtures test retained choices, not search reachability or compiler replay.
module SourceSelectionSpec (tests) where

import Control.Monad (forM_)
import Data.Either (isLeft)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import qualified Djinn.Internal.ContextualInstantiation as I
import Djinn.Internal.Environment
  ( prepareGroundSynthesisEnvironment, prepareRootGivenOpening
  , rootGivenOpeningContexts, checkPreparedSynthesisTypesKinds
  , preparedEnvironmentSynthesisFormulaTranslator )
import Djinn.Internal.HTypes (HKind(KStar))
import Djinn.Internal.LJTFormula
  ( Symbol(Symbol), Term(..), Formula(..), applys, opaqueTypeSymbol )
import Djinn.Internal.ProofCheck.Evidence (checkProofWithEvidence)
import Djinn.Internal.ProofEnv
  ( prepareProofEnvironment, proofBindings, restoreProofTerm )
import qualified Djinn.Internal.SourceEvidence as S
import Djinn.Internal.GeneratedDeduplication
  (deduplicateEtaEquivalentClausesOn, etaNormalClauseExpression)
import Djinn.Internal.SourceAnnotation
  (SourceAnnotation(..), eraseSourceAnnotations, sourceAnnotationComparisonExpression)
import Djinn.Internal.SourceGraph (checkSourceClauseGraph, checkAnnotatedSourceClauseGraph)
import Language.Haskell.Synthesis.Constraint (Constraint(..), constraintArguments)
import Language.Haskell.Synthesis.Declaration
  ( Declaration(..), TypeParameter(..), ValueSignature(..) )
import qualified Language.Haskell.Synthesis.Environment as E
import qualified Language.Haskell.Synthesis.Generated as G
import Language.Haskell.Synthesis.Kind (Kind(ProperTypeKind))
import Language.Haskell.Synthesis.Name (Name, Boxity(Boxed), parseName)
import qualified Language.Haskell.Synthesis.Type as T
import qualified Language.Haskell.Synthesis.TypeAtom as A
import qualified Language.Haskell.Synthesis.TypedGenerated as Q

tests :: TestTree
tests = testGroup "proof-selected lexical source evidence" $
  selectedResultTest : selectionPositionTest :
  [ testCase label $ forM_ [0, 1] $ \selected -> do
      candidate <- fixture source paired selected
      let context = S.sourceCandidateContext candidate
          clause = S.sourceCandidateClause candidate
          annotations = S.sourceCandidateAnnotations candidate
          annotated = S.sourceCandidateAnnotatedClause candidate
          check table = checkAnnotatedSourceClauseGraph 812 context table annotated
      assertBool "unannotated reconstruction silently chose an ambiguous source type" $
        isLeft $ checkSourceClauseGraph 812 context clause
      graph <- right $ check annotations
      assertEqual "annotations changed the exact displayed source expression" clause $
        Q.eraseTermGraphToFunctionClause (G.clauseName clause) graph
      root <- maybe (fail "source graph has no root") pure $
        Q.lookupTermNode (Q.termGraphRoot graph) graph
      assertBool "source selection specialized the original query" $
        A.alphaEquivalentClosedTypes source $ Q.termNodeType root
      let owners = sort [Q.contextEvidenceBinder $ Q.givenContextEvidence occurrence 0
            | (_, Q.TermNode _ (Q.TypedContextIntroduction occurrence _ _)) <- Q.termGraphNodes graph]
          uses = concat [map Q.contextEvidenceBinder $ Q.contextApplicationEvidence witness
            | (_, Q.TermNode _ (Q.TypedContextApplication _ _ witness)) <- Q.termGraphNodes graph]
      assertEqual "the provider acquired an artificial local dictionary scope" 2 $ length owners
      assertBool "cleanup lost every selected dictionary use" $ not $ null uses
      assertBool "reconstruction changed the proof-selected dictionary occurrence" $
        all (== owners !! selected) uses
      let foreignDictionary (SelectedSourceApplication ty args _) =
            SelectedSourceApplication ty args [Symbol "notAnIntroducedDictionary"]
          foreignDictionary annotation = annotation
          wrongSlot (SelectedSourceApplication ty args _) =
            SelectedSourceApplication ty args [dictionaryNames !! (1 - selected)]
          wrongSlot annotation = annotation
          missingType (SelectedSourceApplication ty _ ds) = SelectedSourceApplication ty [] ds
          missingType annotation = annotation
      assertBool "a dictionary from outside the lexical scope was accepted" $
        isLeft $ check $ Map.map foreignDictionary annotations
      assertBool "a different lexical dictionary discharged the selected source type" $
        isLeft $ check $ Map.map wrongSlot annotations
      assertBool "a missing source type selection was silently inferred" $
        isLeft $ check $ Map.map missingType annotations
  | (label, source, paired) <-
      [ ("distinct parameters survive beta cleanup", query False False False, False)
      , ("vacuous and shadowed parameters retain their positions", query True True False, False)
      , ("duplicated expressions retain the same selected dictionary", query False False True, True)
      ]
  ]

-- A type selected for a provider parameter can expose a new forall at its
-- result. Checking must stop at the original provider telescope, preserving
-- the resulting polymorphism for ordinary source checking to consume later.
selectedResultTest :: TestTree
selectedResultTest = testCase "a selected impredicative result is not another provider parameter" $ do
  let unit = T.TupleType Boxed []
      poly = T.ForallType ["b"] [] $ T.FunctionType (variable "b") (variable "b")
      given = Constraint (name "C") [unit]
      providerType = T.ForallType ["p"] [given] $ variable "p"
      goal = T.ForallType [] [given] $ T.FunctionType unit poly
      declarations :: [Declaration String Void ()]
      declarations = [ClassDeclaration () (name "C") [TypeParameter "p" (Just ProperTypeKind)] [] [],
        ValueDeclaration $ ValueSignature () (name "provider") providerType]
      dictionary = Symbol "resultDictionary"
      annotations = Map.fromList
        [(name "rootMark", RootSourceOpening [] [dictionary]),
         (name "selectionMark", SelectedSourceApplication providerType [poly] [dictionary])]
      body = G.Apply (G.Global $ name "rootMark") $ G.lambdaExpression [G.Wildcard] $
        G.Apply (G.Global $ name "selectionMark") $ G.Global $ name "provider"
  environment <- right $ E.mkEnvironment declarations
  prepared <- right $ prepareGroundSynthesisEnvironment environment
  target <- right $ G.mkDefinitionName $ name "selectedResult"
  let context = S.sourceTypingContext prepared goal
      clause = G.functionClauseFromExpression target body
  graph <- right $ checkAnnotatedSourceClauseGraph 913 context annotations clause
  assertEqual "selected result lost its exact expression"
    (G.lambdaExpression [G.Wildcard] $ G.Global $ name "provider") $ Q.eraseTermGraph graph
  let missing (SelectedSourceApplication source _ _) = SelectedSourceApplication source [] []
      missing annotation = annotation
  assertBool "an empty selection vector was treated as a request for inference" $
    isLeft $ checkAnnotatedSourceClauseGraph 913 context (Map.map missing annotations) clause

fixture :: T.Type String -> Bool -> Int -> IO S.SourceCandidate
fixture source paired selected = fixtureWithProof source paired $ \helpers provider ->
  let chosen = applys (Var $ helpers !! selected) [Var provider, Var $ dictionaryNames !! selected]
      local = Symbol "betaPayload"
      body | paired = applys (Ctuple 2) [Var local, Var local]
           | otherwise = Var local
  in Apply (Lam local body) chosen

-- The two checked proofs allocate the same evidence markers before beta
-- reduction, but use the two selected values in opposite tuple positions.
-- A table beside the erased expression loses this distinction.
selectionPositionTest :: TestTree
selectionPositionTest = testCase "selection positions survive beta reduction and deduplication" $ do
  candidates <- mapM (\reversed -> fixtureWithProof (query False False True) True $ \helpers provider ->
    let selected index = applys (Var $ helpers !! index)
          [Var provider, Var $ dictionaryNames !! index]
        first = Symbol "firstPayload"
        second = Symbol "secondPayload"
        fields = if reversed then [second, first] else [first, second]
    in applys (Lam first $ Lam second $ applys (Ctuple 2) $ map Var fields)
         [selected 0, selected 1]) [False, True]
  case candidates of
    [forward, backward] -> do
      assertEqual "fixture did not retain the same annotation table"
        (S.sourceCandidateAnnotations forward) (S.sourceCandidateAnnotations backward)
      -- Exercise capture-avoiding substitution explicitly. Ordinary eta-key
      -- cleanup does not promise general beta reduction of source lambdas.
      rewritten <- mapM (G.rewriteExpressionBottomUpM beta . etaNormalClauseExpression .
        S.sourceCandidateAnnotatedClause) candidates
      erased <- sequence [right $ eraseSourceAnnotations (S.sourceCandidateAnnotations candidate) expression
        | (candidate, expression) <- zip candidates rewritten]
      assertBool "fixture did not reduce to the same erased expression" $ case erased of
        [left, rightExpression] -> G.alphaEquivalentExpression left rightExpression
        _ -> False
      graphs <- sequence [right $ checkAnnotatedSourceClauseGraph 947
        (S.sourceCandidateContext candidate) (S.sourceCandidateAnnotations candidate)
        (G.functionClauseFromExpression (G.clauseName $ S.sourceCandidateAnnotatedClause candidate) expression)
        | (candidate, expression) <- zip candidates rewritten]
      let uses graph = concat [map Q.contextEvidenceBinder $ Q.contextApplicationEvidence witness
            | (_, Q.TermNode _ (Q.TypedContextApplication _ _ witness)) <- Q.termGraphNodes graph]
      case map uses graphs of
        [left, rightUses] -> do
          assertEqual "fixture lost a selected tuple field" 2 $ length left
          assertBool "the graph forgot selection positions" $ left /= rightUses
          assertEqual "the graph changed a selected dictionary" (reverse left) rightUses
        _ -> fail "missing checked selection graphs"
      let comparisonClauses =
            [G.functionClauseFromExpression (G.clauseName $ S.sourceCandidateClause candidate) $
              sourceAnnotationComparisonExpression (S.sourceCandidateAnnotations candidate) expression
            | (candidate, expression) <- zip candidates rewritten]
      assertEqual "deduplication merged distinct selection positions" 2 $ length $
        deduplicateEtaEquivalentClausesOn id comparisonClauses
      assertEqual "deduplication retained an identical selected implementation" 2 $ length $
        deduplicateEtaEquivalentClausesOn id $ comparisonClauses ++ comparisonClauses
    _ -> fail "missing checked selection candidates"
 where
  beta (G.Apply (G.Lambda [G.Bind binder] body) argument) =
    maybe (fail "fixture beta substitution would capture a binder") pure $
      G.substituteExpressionLocalBy id binder argument body
  beta expression = pure expression

fixtureWithProof
  :: T.Type String -> Bool -> ([Symbol] -> Symbol -> Term) -> IO S.SourceCandidate
fixtureWithProof source paired makeBody = do
  environment <- right $ E.mkEnvironment declarations
  prepared <- right $ prepareGroundSynthesisEnvironment environment
  opening <- right (prepareRootGivenOpening prepared source result)
    >>= maybe (fail "source fixture lacks its checked root opening") pure
  let arguments = map constraintArguments $ rootGivenOpeningContexts opening
  requests <- mapM (\vector -> do
    right $ I.checkContextualInstantiationKinds prepared opening providerScheme vector
    right $ I.prepareContextualInstantiation
      (\ty -> checkPreparedSynthesisTypesKinds prepared [(KStar, ty)])
      (preparedEnvironmentSynthesisFormulaTranslator prepared) providerScheme vector) arguments
  let helpers = I.contextualInstantiations $ I.contextualInstantiationAxioms requests
      context = S.sourceTypingContext prepared source
  receipt <- right $ S.rootGivenErasure context opening helpers
  let environment' = prepareProofEnvironment (Symbol "selectedCandidate") $
        (Symbol "provider", PVar $ opaqueTypeSymbol providerScheme) :
        [(I.contextualInstantiationSymbol helper, I.contextualInstantiationFormula helper)
        | helper <- helpers]
      internal symbol = case [name' | (name', _) <- proofBindings environment',
          restoreProofTerm environment' (Var name') == Var symbol] of
        [name'] -> pure name'
        _ -> fail "checked proof environment did not preserve a unique source premise"
  internalHelpers <- mapM (internal . I.contextualInstantiationSymbol) helpers
  provider <- internal $ Symbol "provider"
  let proof = foldr Lam (makeBody internalHelpers provider) dictionaryNames
  evidence <- right $ checkProofWithEvidence (proofBindings environment')
    (S.rootGivenErasureGoal receipt) proof
  target <- right $ G.mkDefinitionName $ name "selectedCandidate"
  right $ S.lowerCheckedContextualSourceCandidate receipt context environment'
    Set.empty Map.empty Map.empty target evidence
 where
  result = if paired then T.TupleType Boxed [token, token] else token
  declarations :: [Declaration String Void ()]
  declarations =
    [ ClassDeclaration () (name "C") [TypeParameter "p" $ Just ProperTypeKind] [] []
    , AbstractTypeDeclaration () (name "Token") ProperTypeKind
    , ValueDeclaration $ ValueSignature () (name "provider") providerScheme
    ]

query :: Bool -> Bool -> Bool -> T.Type String
query vacuous shadow paired =
  T.ForallType ((if vacuous then ["unused"] else []) ++ ["a"]) [constraint $ variable "a"] $
    T.ForallType [second] [constraint $ variable second] $
      if paired then T.TupleType Boxed [token, token] else token
 where
  second = if shadow then "a" else "b"

dictionaryNames :: [Symbol]
dictionaryNames = [Symbol "djinnSourceEvidence0", Symbol "otherDictionary"]

providerScheme, token :: T.Type String
providerScheme = T.ForallType ["p"] [constraint $ variable "p"] token
token = T.TypeConstructor $ name "Token"

variable :: String -> T.Type String
variable = T.TypeVariable

constraint :: T.Type String -> Constraint (T.Type String)
constraint ty = Constraint (name "C") [ty]

name :: String -> Name
name = either (error . show) id . parseName

right :: Show failure => Either failure result -> IO result
right = either (fail . show) pure
