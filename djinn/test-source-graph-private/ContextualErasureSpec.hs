-- | Direct adversarial checks of the production contextual proof lowerer.
-- These are supplied proofs, not synthesis or external compiler receipts.
module ContextualErasureSpec (tests) where

import Control.Monad (forM_)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Djinn.Internal.ContextualInstantiation as I
import Djinn.Internal.Environment
  ( PreparedEnvironment, prepareGroundSynthesisEnvironment, prepareRootGivenOpening
  , checkPreparedSynthesisTypesKinds, preparedEnvironmentSynthesisFormulaTranslator )
import Djinn.Internal.HTypes (HKind(KStar))
import Djinn.Internal.LJTFormula
  ( Formula(..), Symbol(Symbol), Term(..), applys, dictionarySymbol, opaqueTypeSymbol )
import Djinn.Internal.ProofCheck.Evidence
  ( CheckedProofEvidence, checkProofWithEvidence, checkedProofExpectedType )
import Djinn.Internal.ProofEnv
  ( ProofEnvironment, prepareProofEnvironment, proofBindings, restoreProofTerm )
import qualified Djinn.Internal.SourceEvidence as S
import Djinn.Internal.SourceGraph (checkSourceClauseGraph)
import Language.Haskell.Synthesis.Constraint (Constraint(..))
import Language.Haskell.Synthesis.Declaration
  ( Declaration(..), TypeParameter(..), ValueSignature(..) )
import qualified Language.Haskell.Synthesis.Environment as E
import qualified Language.Haskell.Synthesis.Generated as G
import Language.Haskell.Synthesis.Kind (Kind(ProperTypeKind))
import Language.Haskell.Synthesis.Name (Name, parseName)
import qualified Language.Haskell.Synthesis.Type as T
import qualified Language.Haskell.Synthesis.TypeAtom as A
import qualified Language.Haskell.Synthesis.TypedGenerated as Q

type SourceType = T.Type String

data Fixture = Fixture
  { fixturePrepared :: PreparedEnvironment
  , fixtureContext :: S.SourceTypingContext
  , fixtureHelper :: I.ContextualInstantiation
  , fixtureErasure :: S.RootGivenErasure
  , fixtureTokenFormula :: Formula
  }

tests :: TestTree
tests = testGroup "checked contextual dictionary erasure (not synthesis)"
  [ testCase "erase only the exact root dictionaries and preserve two ordinary arguments in either order" $ do
      fixture <- prepareFixture
      forM_ [[x, y], [y, x]] $ \arguments -> do
        let environment = proofEnvironment fixture []
        helper <- internal environment $ I.contextualInstantiationSymbol $ fixtureHelper fixture
        provider <- internal environment providerName
        let term = rootLambdas $ applys (Var helper) $
              map Var $ [provider, rootC, rootD] ++ arguments
            expected = Lam x $ Lam y $ applys (Var providerName) $ map Var arguments
        evidence <- checked fixture environment term
        candidate <- right $ lower fixture environment evidence
        assertEqual "ordinary arguments, their order, or their binders were erased"
          expected $ S.sourceCandidateErasedProof candidate
        assertEqual "provider restoration changed the preserved ordinary spine"
          expected $ S.sourceCandidateProviderProof candidate
        assertEqual "source evidence lost the checked dictionary telescope"
          (S.rootGivenErasureGoal $ fixtureErasure fixture)
          (checkedProofExpectedType $ S.sourceCandidateProofEvidence candidate)
        seal fixture candidate
  , testCase "an ordinary binder may shadow a removed dictionary spelling without becoming evidence" $ do
      fixture <- prepareFixture
      let environment = proofEnvironment fixture []
      supplied <- internal environment tokenName
      let term = Lam rootC $ Lam rootD $ Lam rootC $ Lam y $ Var supplied
          expected = Lam rootC $ Lam y $ Var tokenName
      evidence <- checked fixture environment term
      candidate <- right $ lower fixture environment evidence
      assertEqual "ordinary shadowing binder was mistaken for a dictionary"
        expected $ S.sourceCandidateErasedProof candidate
      seal fixture candidate
  , testCase "a checked value of the whole implication cannot replace the actual root lambda prefix" $ do
      fixture <- prepareFixture
      let whole = Symbol "wholeProof"
          environment = proofEnvironment fixture
            [(whole, S.rootGivenErasureGoal $ fixtureErasure fixture)]
      supplied <- internal environment whole
      evidence <- checked fixture environment $ Var supplied
      rejectsWith "lacks its checked root dictionary introduction" $ lower fixture environment evidence
  , testCase "a differently ordered checked root telescope cannot borrow the original receipt" $ do
      fixture <- prepareFixture
      let environment = proofEnvironment fixture []
      helper <- internal environment $ I.contextualInstantiationSymbol $ fixtureHelper fixture
      provider <- internal environment providerName
      body <- right $ preparedEnvironmentSynthesisFormulaTranslator (fixturePrepared fixture) sourceBody
      let reorderedGoal = dictionaryD :-> dictionaryC :-> body
          term = Lam rootD $ Lam rootC $ Lam x $ Lam y $
            applys (Var helper) $ map Var [provider, rootC, rootD, x, y]
      evidence <- right $ checkProofWithEvidence (proofBindings environment) reorderedGoal term
      rejectsWith "does not belong to its root opening" $ lower fixture environment evidence
  , testCase "an equal global dictionary proposition cannot impersonate the actual root binder" $ do
      fixture <- prepareFixture
      let unrelatedName = Symbol "foreignDictionary"
          environment = proofEnvironment fixture [(unrelatedName, dictionaryC)]
      helper <- internal environment $ I.contextualInstantiationSymbol $ fixtureHelper fixture
      provider <- internal environment providerName
      unrelated <- internal environment unrelatedName
      evidence <- checked fixture environment $ rootLambdas $
        applys (Var helper) $ map Var [provider, unrelated, rootD, x, y]
      rejectsWith "not its actual root Given" $ lower fixture environment evidence
  , testCase "a nested dictionary alias is outside the root-only erasure pilot" $ do
      fixture <- prepareFixture
      let environment = proofEnvironment fixture []
          sibling = Symbol "siblingGiven"
      helper <- internal environment $ I.contextualInstantiationSymbol $ fixtureHelper fixture
      provider <- internal environment providerName
      let nested = Lam sibling $ applys (Var helper) $
            map Var [provider, sibling, rootD, x, y]
      evidence <- checked fixture environment $ rootLambdas $ Apply nested $ Var rootC
      rejectsWith "not its actual root Given" $ lower fixture environment evidence
  , testCase "a shadowing dictionary binder cannot reuse the removed root binder's authority" $ do
      fixture <- prepareFixture
      let environment = proofEnvironment fixture []
      helper <- internal environment $ I.contextualInstantiationSymbol $ fixtureHelper fixture
      provider <- internal environment providerName
      let nested = Lam rootC $ applys (Var helper) $
            map Var [provider, rootC, rootD, x, y]
      evidence <- checked fixture environment $ rootLambdas $ Apply nested $ Var rootC
      rejectsWith "not its actual root Given" $ lower fixture environment evidence
  , testCase "a root dictionary cannot escape through an ordinary application" $ do
      fixture <- prepareFixture
      result <- right $ preparedEnvironmentSynthesisFormulaTranslator (fixturePrepared fixture) token
      let sink = Symbol "dictionarySink"
          environment = proofEnvironment fixture [(sink, dictionaryC :-> result)]
      supplied <- internal environment sink
      evidence <- checked fixture environment $ rootLambdas $ Apply (Var supplied) $ Var rootC
      rejectsWith "root dictionary escaped its conditional helper" $ lower fixture environment evidence
  , testCase "a checked unsaturated helper cannot escape as an ordinary function argument" $ do
      fixture <- prepareFixture
      result <- right $ preparedEnvironmentSynthesisFormulaTranslator (fixturePrepared fixture) token
      residual <- case I.contextualInstantiationFormula $ fixtureHelper fixture of
        _ :-> rest -> pure rest
        _ -> fail "fixture helper lost its source-provider argument"
      let sink = Symbol "partialHelperSink"
          environment = proofEnvironment fixture [(sink, residual :-> result)]
      helper <- internal environment $ I.contextualInstantiationSymbol $ fixtureHelper fixture
      provider <- internal environment providerName
      supplied <- internal environment sink
      evidence <- checked fixture environment $ rootLambdas $
        Apply (Var supplied) $ Apply (Var helper) $ Var provider
      rejectsWith "unsaturated conditional helper cannot be erased" $ lower fixture environment evidence
  , testCase "the proof checker rejects a provider missing part of the exact source qualification" $ do
      fixture <- prepareFixture
      let wrong = Symbol "wrongProvider"
          wrongSource = T.ForallType ["a"] [constraint "C" $ T.TypeVariable "a"] $
            T.FunctionType (T.TypeVariable "a") $ T.FunctionType (T.TypeVariable "a") token
          environment = proofEnvironment fixture [(wrong, PVar $ opaqueTypeSymbol wrongSource)]
      helper <- internal environment $ I.contextualInstantiationSymbol $ fixtureHelper fixture
      supplied <- internal environment wrong
      -- The independent proof checker must refuse to create the evidence that
      -- the lowerer requires; no synthetic CheckedProofEvidence is substituted.
      let proof = rootLambdas $ applys (Var helper) $
            map Var [supplied, rootC, rootD, x, y]
      rejects "a different full provider scheme acquired checked lowering evidence" $
        checkProofWithEvidence (proofBindings environment)
          (S.rootGivenErasureGoal $ fixtureErasure fixture) proof
  , testCase "checked evidence cannot be lowered with another proof environment" $ do
      fixture <- prepareFixture
      let environment = proofEnvironment fixture []
          other = proofEnvironment fixture [(Symbol "extraUnused", dictionaryC)]
      helper <- internal environment $ I.contextualInstantiationSymbol $ fixtureHelper fixture
      provider <- internal environment providerName
      evidence <- checked fixture environment $ rootLambdas $
        applys (Var helper) $ map Var [provider, rootC, rootD, x, y]
      rejectsWith "evidence for another proof environment" $ lower fixture other evidence
  ]

prepareFixture :: IO Fixture
prepareFixture = do
  environment <- right $ E.mkEnvironment declarations
  prepared <- right $ prepareGroundSynthesisEnvironment environment
  opening <- right (prepareRootGivenOpening prepared sourceGoal sourceBody)
    >>= maybe (fail "fixture did not produce an opaque root Given opening") pure
  right $ I.checkContextualInstantiationKinds prepared opening providerScheme [atomA]
  request <- right $ I.prepareContextualInstantiation
    (\ty -> checkPreparedSynthesisTypesKinds prepared [(KStar, ty)])
    (preparedEnvironmentSynthesisFormulaTranslator prepared) providerScheme [atomA]
  helper <- case I.contextualInstantiations $ I.contextualInstantiationAxioms [request] of
    [row] -> pure row
    _ -> fail "fixture did not allocate one exact conditional helper"
  let context = S.sourceTypingContext prepared sourceGoal
  receipt <- right $ S.rootGivenErasure context opening [helper]
  tokenFormula <- right $ preparedEnvironmentSynthesisFormulaTranslator prepared token
  pure $ Fixture prepared context helper receipt tokenFormula
 where
  declarations :: [Declaration String Void ()]
  declarations =
    [ClassDeclaration () (name className) [TypeParameter "a" $ Just ProperTypeKind] [] []
      | className <- ["C", "D"]]
    ++ [AbstractTypeDeclaration () (name spelling) ProperTypeKind | spelling <- ["A", "B", "Token"]]
    ++ [ValueDeclaration $ ValueSignature () (name "provider") providerScheme,
        ValueDeclaration $ ValueSignature () (name "token") token]

-- No dictionary is pooled into the real fixture environment. Individual
-- adversarial cases deliberately add propositional impostors; their successful
-- logical checks must still fail the stricter lexical lowering boundary.
proofEnvironment :: Fixture -> [(Symbol, Formula)] -> ProofEnvironment
proofEnvironment fixture extra = prepareProofEnvironment targetSymbol $
  [(I.contextualInstantiationSymbol helper, I.contextualInstantiationFormula helper),
   (providerName, PVar $ opaqueTypeSymbol providerScheme),
   (tokenName, fixtureTokenFormula fixture)] ++ extra
 where
  helper = fixtureHelper fixture

internal :: ProofEnvironment -> Symbol -> IO Symbol
internal environment external = case
    [candidate | (candidate, _) <- proofBindings environment,
      restoreProofTerm environment (Var candidate) == Var external] of
  [candidate] -> pure candidate
  _ -> fail $ "fixture external assumption has no unique exact proof identity: " ++ show external

checked :: Fixture -> ProofEnvironment -> Term -> IO CheckedProofEvidence
checked fixture environment = right . checkProofWithEvidence
  (proofBindings environment) (S.rootGivenErasureGoal $ fixtureErasure fixture)

lower :: Fixture -> ProofEnvironment -> CheckedProofEvidence -> Either String S.SourceCandidate
lower fixture environment = S.lowerCheckedContextualSourceCandidate
  (fixtureErasure fixture) (fixtureContext fixture) environment
  Set.empty Map.empty Map.empty target

seal :: Fixture -> S.SourceCandidate -> IO ()
seal fixture candidate = do
  let clause = S.sourceCandidateClause candidate
  graph <- right $ checkSourceClauseGraph 73 (S.sourceCandidateContext candidate) clause
  assertEqual "sealing changed the lowerer's exact generated clause"
    clause $ Q.eraseTermGraphToFunctionClause (G.clauseName clause) graph
  root <- maybe (fail "sealed lowered graph has no root") pure $
    Q.lookupTermNode (Q.termGraphRoot graph) graph
  assertBool "sealed lowered graph lost its complete original source goal" $
    A.alphaEquivalentClosedTypes (S.sourceTypingGoal $ fixtureContext fixture) $ Q.termNodeType root

sourceGoal, sourceBody, providerScheme, atomA, atomB, token :: SourceType
sourceGoal = T.ForallType [] [constraint "C" atomA, constraint "D" atomB] sourceBody
sourceBody = T.FunctionType atomA $ T.FunctionType atomA token
providerScheme = T.ForallType ["a"] [constraint "C" $ T.TypeVariable "a", constraint "D" atomB] $
  T.FunctionType (T.TypeVariable "a") $ T.FunctionType (T.TypeVariable "a") token
atomA = T.TypeConstructor $ name "A"
atomB = T.TypeConstructor $ name "B"
token = T.TypeConstructor $ name "Token"

dictionaryC, dictionaryD :: Formula
dictionaryC = PVar $ dictionarySymbol $ constraint "C" atomA
dictionaryD = PVar $ dictionarySymbol $ constraint "D" atomB

rootC, rootD, x, y, providerName, tokenName, targetSymbol :: Symbol
rootC = Symbol "rootC"
rootD = Symbol "rootD"
x = Symbol "firstArgument"
y = Symbol "secondArgument"
providerName = Symbol "provider"
tokenName = Symbol "token"
targetSymbol = Symbol "erasureCandidate"

rootLambdas :: Term -> Term
rootLambdas body = foldr Lam body [rootC, rootD, x, y]

target :: G.DefinitionName
target = either (error . show) id $ G.mkDefinitionName $ name "erasureCandidate"

constraint :: String -> SourceType -> Constraint SourceType
constraint spelling ty = Constraint (name spelling) [ty]

name :: String -> Name
name = either (error . show) id . parseName

right :: Show failure => Either failure result -> IO result
right = either (fail . show) pure

rejects :: String -> Either String result -> IO ()
rejects message result = case result of
  Left _ -> pure ()
  Right _ -> assertFailure message

rejectsWith :: String -> Either String result -> IO ()
rejectsWith expected result = case result of
  Left failure -> assertBool ("unexpected rejection: " ++ failure) $ expected `isInfixOf` failure
  Right _ -> assertFailure $ "expected rejection containing: " ++ expected
