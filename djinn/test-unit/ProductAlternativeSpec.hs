module ProductAlternativeSpec (tests) where

import Control.Monad (forM_)
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import qualified Data.Set as Set
import qualified Djinn.Core as Core
import Djinn.Internal.LJT
import Djinn.Internal.ProofCheck (checkProof)
import qualified Language.Haskell.Synthesis.Generated as Generated
import qualified Language.Haskell.Synthesis.Type as Type
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual)

tests :: [(String, Assertion)]
tests =
    [ ("retain all mixed products after atomic implication consumption", mixedProducts)
    , ("construct nested and empty products and forward exact products", nestedProducts)
    , ("construct mixed products in ordinary function arguments", productArguments)
    , ("retain exact atom identity inside product alternatives", productIdentity)
    , ("preserve first-proof fuel and unsuccessful product evidence", productPrefix)
    , ("retain product alternatives in focused proof cursors", focusedProducts)
    , ("retain projections from values and applied product providers", productProjections)
    , ("apply projected functions without capturing input names", projectedFunctions)
    , ("retain mixed products after the sequential source-plan prefix", sourceBatchProducts)
    ]

a, b :: Formula
a = PVar $ Symbol "productInput"
b = PVar $ Symbol "productOutput"

environment :: [(Symbol, Formula)]
environment = [(Symbol "bridge", a :-> b), (Symbol "left", a), (Symbol "right", a)]

use :: String -> Proof
use name = Apply (Var $ Symbol "bridge") (Var $ Symbol name)

tuple :: [Proof] -> Proof
tuple fields = applys (Ctuple $ length fields) fields

mode :: Strategy -> SearchMode
mode strategy = (defaultSearchMode True)
    { searchStrategy = strategy, searchTermAlternatives = True, searchBudget = Just 5000 }

checked :: Either String value -> IO value
checked = either fail pure

finite :: [(Symbol, Formula)] -> Formula -> [Proof] -> Assertion
finite assumptions goal wanted = forM_ [DepthFirst, Interleave] $ \strategy -> do
    mapM_ (checked . checkProof assumptions goal) wanted
    result <- checked $ proveWithModeChecked (mode strategy) assumptions goal
    mapM_ (checked . checkProof assumptions goal) $ searchProofs result
    assertBool ("finite product grammar exhausted its allowance: " ++ show strategy) $
        not $ searchExhausted result
    assertBool "an inhabited finite fixture returned no proof" $ not $ null $ searchProofs result
    forM_ wanted $ \proof -> assertBool ("lost checked product alternative: " ++ show proof) $
        canonical proof `elem` map canonical (searchProofs result)

canonical :: Proof -> Proof
canonical proof = evalState (freshenTermBinders fresh proof) 0
  where
    fresh :: State Integer Symbol
    fresh = do
        next <- get
        put $ next + 1
        let symbol = Symbol $ "$productCanonical" ++ show next
        if symbol `elem` freeVars proof then fresh else pure symbol

project :: Int -> Int -> Proof -> Proof
project index width proof = applys (Csplit width)
    [foldr Lam (Var $ Symbol $ "slot" ++ show index) variables,proof]
  where
    variables = [Symbol $ "slot" ++ show slot | slot <- [0 .. width - 1]]

mixedProducts :: Assertion
mixedProducts = finite environment (Conj [b,b])
    [tuple [use left, use right] | left <- ["left","right"], right <- ["left","right"]]

nestedProducts :: Assertion
nestedProducts = do
    let pair = Conj [b,b]
        goal = Conj [pair, Conj []]
        supplied = Symbol "suppliedPair"
    finite ((supplied,pair) : environment) goal
        [tuple [tuple [use "left",use "right"], tuple []], tuple [Var supplied,tuple []]]
    finite [] (Conj []) [tuple []]
    finite [] (Conj [a :-> a,a :-> a]) []

productArguments :: Assertion
productArguments = do
    let consume = Symbol "consumePair"
        assumptions = [(consume,Conj [a,a] :-> b), (Symbol "left",a), (Symbol "right",a)]
    finite assumptions b
        [Apply (Var consume) $ tuple [Var $ Symbol left,Var $ Symbol right]
        | left <- ["left","right"], right <- ["left","right"]]

productIdentity :: Assertion
productIdentity = do
    let plain = PVar $ Symbol "samePrintedAtom"
        opaque = PVar $ opaqueTypeSymbol $ Type.TypeVariable "samePrintedAtom"
        left = Symbol "plainValue"
        right = Symbol "opaqueValue"
    finite [(left,plain),(right,opaque)] (Conj [plain,opaque]) [tuple [Var left,Var right]]

productPrefix :: Assertion
productPrefix = forM_ [DepthFirst,Interleave] $ \strategy -> do
    forM_ [0..20] $ \budget -> do
        let original = (mode strategy) {searchTermAlternatives=False,searchBudget=Just budget}
            extended = original {searchTermAlternatives=True}
            first selected = checked $ proveFirstWithModeChecked selected environment (Conj [b,b])
        old <- first original
        new <- first extended
        assertEqual "product alternatives changed the historical first proof or its fuel"
            (view old) (view new)
    old <- checked $ proveWithModeChecked (mode strategy) {searchTermAlternatives=False} [] (Conj [b,b])
    new <- checked $ proveWithModeChecked (mode strategy) [] (Conj [b,b])
    assertEqual "product alternatives changed a completed historical refutation" (view old) (view new)
  where
    view result = (searchProofs result,searchExhausted result,remainingSearchBudget result)

focusedProducts :: Assertion
focusedProducts = forM_ constructors $ \start -> do
    cursor <- checked $ start (mode Interleave) environment goal
    proofs <- collect 10000 cursor
    mapM_ (checked . checkProof environment goal) proofs
    assertBool "focused cursor lost the mixed product" $ wanted `elem` proofs
  where
    goal = Conj [b,b]
    wanted = tuple [use "left",use "right"]
    constructors =
        [ startProofSearchWithAssumptionUseChecked $ Set.singleton $ Symbol "right"
        , startProofSearchWithAcyclicHeadsChecked
        ]
    collect :: Int -> ProofSearchCursor -> IO [Proof]
    collect remaining _ | remaining <= 0 = fail "finite focused product cursor did not finish"
    collect remaining cursor = case observeProofSearch cursor of
        ProofSearchFinished -> pure []
        ProofSearchChoice rest -> collect (remaining - 1) rest
        ProofSearchResult proof rest -> (proof :) <$> collect (remaining - 1) rest

productProjections :: Assertion
productProjections = do
    let pair = Symbol "pair"
        pairInput = Var pair
        bridge = Var $ Symbol "bridge"
    finite [(pair,Conj [a,a]),(Symbol "bridge",a :-> b)] (Conj [b,b])
        [tuple [Apply bridge $ project first 2 pairInput,Apply bridge $ project second 2 pairInput]
        | first <- [0,1],second <- [0,1]]
    let produce = Symbol "producePair"
        result name = Apply (Var produce) $ Var $ Symbol name
    finite [(produce,a :-> Conj [b,b]),(Symbol "left",a),(Symbol "right",a)] (Conj [b,b])
        [tuple [project 0 2 $ result "left",project 1 2 $ result "right"]]

projectedFunctions :: Assertion
projectedFunctions = do
    let pair = Symbol "p0"
        assumptions = [(pair,Conj [a :-> b,a :-> b]),(Symbol "left",a),(Symbol "right",a)]
    finite assumptions b
        [Apply (project field 2 $ Var pair) (Var $ Symbol name)
        | field <- [0,1],name <- ["left","right"]]
    let cyclic = [(pair,Conj [a :-> a,a :-> a]),(Symbol "left",a)]
        wanted = Apply (project 0 2 $ Var pair) $
            Apply (project 1 2 $ Var pair) (Var $ Symbol "left")
    checked $ checkProof cyclic a wanted
    cursor <- checked $ startProofSearchWithAcyclicHeadsChecked (mode Interleave) cyclic a
    seek cyclic wanted 5000 cursor
  where
    seek :: [(Symbol,Formula)] -> Proof -> Int -> ProofSearchCursor -> Assertion
    seek _ _ remaining _ | remaining <= 0 = fail "distinct projected heads did not compose within the fixed observation bound"
    seek assumptions wanted remaining cursor = case observeProofSearch cursor of
        ProofSearchFinished -> fail "projected-function cursor ended without the mixed composition"
        ProofSearchChoice rest -> seek assumptions wanted (remaining - 1) rest
        ProofSearchResult proof rest -> do
            checked $ checkProof assumptions a proof
            if canonical wanted == canonical proof then pure ()
            else seek assumptions wanted (remaining - 1) rest

sourceBatchProducts :: Assertion
sourceBatchProducts = do
    goal <- checked $ Core.parseHType "(a -> b) -> a -> a -> (b,b)"
    result <- either (fail . show) pure $ Core.inhabit
        Core.defaultQueryOptions {Core.optionAlternatives=True,Core.optionBudget=Just 5000}
        Core.emptyEnvironment [] "mixedSourceProducts" goal
    let source = Generated.Local "function"
        inputs = [Generated.Local "left",Generated.Local "right"]
        actual = map Generated.functionClauseExpression $ Core.reportGeneratedClauses result
        normalize = Generated.discardUnusedPatternBindingsBy id . Generated.simplifyExpressionBy id
            . Generated.rewriteExpressionBottomUp split
        split (Generated.Lambda patterns body) = foldr
            (\pattern rest -> Generated.Lambda [pattern] rest) body patterns
        split expression = expression
        expected left right = Generated.Lambda (map Generated.Bind ["function","left","right"]) $
            Generated.Tuple [Generated.Apply source left,Generated.Apply source right]
    forM_ [(left,right) | left <- inputs,right <- inputs] $ \(left,right) ->
        assertBool ("sequential source search lost a mixed application after its historical plans: " ++
            show (expected left right, actual))
            $ any (Generated.alphaEquivalentExpression (normalize $ expected left right) . normalize) actual
