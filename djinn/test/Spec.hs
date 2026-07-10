module Main (main) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (unless)
import Data.List (isInfixOf, isSuffixOf, nub)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

import HCheck (htCheckEnv, htCheckType)
import HTypes
import LJT
import ProofCheck (checkProof)
import ProofEnv

type Test = (String, IO ())

main :: IO ()
main = do
    failures <- concat <$> mapM runTest tests
    unless (null failures) $ do
        putStrLn ""
        putStrLn $ show (length failures) ++ " test(s) failed:"
        mapM_ (putStrLn . ("  - " ++)) failures
        exitFailure

runTest :: Test -> IO [String]
runTest (name, action) = do
    result <- try action :: IO (Either SomeException ())
    case result of
        Right () -> do
            putStrLn $ "PASS  " ++ name
            return []
        Left exception -> do
            let failure = name ++ ": " ++ displayException exception
            putStrLn $ "FAIL  " ++ failure
            return [failure]

tests :: [Test]
tests =
    [ ("parse prefix function constructor", testPrefixArrowParsing)
    , ("kind-check intrinsic list syntax", testIntrinsicListKind)
    , ("render canonical units and kinds", testCanonicalRendering)
    , ("infer and reuse a higher-kinded synonym", testHigherKindedGrounding)
    , ("reject an ill-kinded higher-kinded application", testIllKindedApplication)
    , ("reject a higher-kinded synonym body", testHigherKindedSynonymBody)
    , ("prove intuitionistic tautologies", testProvableBasics)
    , ("reject non-theorems", testNonTheorems)
    , ("use an assumption as its named proof", testNamedAssumption)
    , ("do not capture a caller-supplied proof symbol", testCallerSymbolCapture)
    , ("keep disjunction continuation atoms fresh", testContinuationAtomCapture)
    , ("preserve residual application after Csplit", testCsplitResidualArguments)
    , ("preserve residual application after Ccases", testCcasesResidualArguments)
    , ("isolate external proof identities", testProofEnvironment)
    , ("type-check generated proofs independently", testGeneratedProofsCheck)
    , ("reject malformed proof terms", testMalformedProofTerms)
    , ("preserve nominal empty types", testNominalEmptyTypes)
    ]

testPrefixArrowParsing :: IO ()
testPrefixArrowParsing = do
    let expected = HTArrow (HTVar "a") (HTVar "b")
        parsed = readMaybe "(->) a b"
    assertEqual "prefix and infix arrows should have one representation"
        (Just expected) parsed
    assertEqual "the canonical rendering should use infix arrow syntax"
        "a -> b" (show expected)

testIntrinsicListKind :: IO ()
testIntrinsicListKind = do
    let listOfA = HTApp (HTCon "[]") (HTVar "a")
    assertEqual "list syntax should parse to the intrinsic [] constructor"
        (Just listOfA) (readMaybe "[a]")
    assertEqual "the intrinsic list constructor should render canonically"
        "[a]" (show listOfA)
    assertEqual "a list type should kind-check without an environment declaration"
        (Right ()) (htCheckType [] $ HTArrow listOfA listOfA)

testCanonicalRendering :: IO ()
testCanonicalRendering = do
    assertEqual "unit should not acquire an extra pair of parentheses"
        "()" (show $ HTCon "()")
    assertEqual "kinds should use the syntax accepted by the parser"
        "(* -> *) -> * -> *"
        (show $ KArrow (KArrow KStar KStar) (KArrow KStar KStar))

-- Grounding must recursively eliminate every unification variable.  Foo's
-- inferred kind is reused after kind inference has reset its local IntMap;
-- leaving a KVar behind used to make this second check crash at IntMap.!
testHigherKindedGrounding :: IO ()
testHigherKindedGrounding = do
    checked <- checkedKindEnvironment
    let expectedFooKind = KArrow (KArrow KStar KStar) (KArrow KStar KStar)
        actualFooKind = lookup "Foo"
            [(name, kind) | (name, (_, _, kind)) <- checked]
        fooMaybeBool = HTApp (HTApp (HTCon "Foo") (HTCon "Maybe")) (HTCon "Bool")
    assertEqual "Foo f a = f a has kind (* -> *) -> * -> *"
        (Just expectedFooKind) actualFooKind
    assertEqual "a grounded higher kind remains usable in a later check"
        (Right ()) (htCheckType checked (HTArrow fooMaybeBool fooMaybeBool))

testIllKindedApplication :: IO ()
testIllKindedApplication = do
    checked <- checkedKindEnvironment
    let fooBoolBool = HTApp (HTApp (HTCon "Foo") (HTCon "Bool")) (HTCon "Bool")
    assertLeft "Foo's first argument must have kind * -> *"
        (htCheckType checked fooBoolBool)

testHigherKindedSynonymBody :: IO ()
testHigherKindedSynonymBody =
    assertLeft "an ordinary synonym body must have result kind *"
        (htCheckEnv
            [("Endo", (["a"], HTApp (HTCon "->") (HTVar "a"), ()))])

checkedKindEnvironment :: IO [(HSymbol, ([HSymbol], HType, HKind))]
checkedKindEnvironment =
    case htCheckEnv kindDefinitions of
        Left message -> fail $ "kind environment was rejected: " ++ message
        Right checked -> return checked

kindDefinitions :: [(HSymbol, ([HSymbol], HType, ()))]
kindDefinitions =
    [ ( "Foo"
      , (["f", "a"], HTApp (HTVar "f") (HTVar "a"), ())
      )
    , ( "Maybe"
      , ( ["a"]
        , HTUnion [("Nothing", []), ("Just", [HTVar "a"])]
        , ()
        )
      )
    , ( "Bool"
      , ([], HTUnion [("False", []), ("True", [])], ())
      )
    ]

testProvableBasics :: IO ()
testProvableBasics =
    mapM_ (uncurry assertTrue)
        [ ("identity", atomA :-> atomA)
        , ("conjunction commutes", (atomA & atomB) :-> (atomB & atomA))
        , ("a value injects into a disjunction", atomA :-> (atomA |: atomB))
        , ("false eliminates", false :-> atomA)
        ]
  where
    assertTrue name formula = assertBool name (provable formula)

testNonTheorems :: IO ()
testNonTheorems =
    mapM_ (uncurry assertFalse)
        [ ("an unconstrained atom", atomA)
        , ("Peirce's law is not intuitionistic", ((atomA :-> atomB) :-> atomA) :-> atomA)
        ]
  where
    assertFalse name formula = assertBool name (not $ provable formula)

testNamedAssumption :: IO ()
testNamedAssumption = do
    let assumption = Symbol "givenA"
    assertEqual "proof search should preserve the assumption's term symbol"
        [Var assumption] (prove False [(assumption, atomA)] atomA)

-- The generated binder for b must not reuse x2.  Reuse changed the intended
-- constant function into the ill-typed identity `f a = a` during rendering.
testCallerSymbolCapture :: IO ()
testCallerSymbolCapture = do
    let caller = Symbol "x2"
        proofs = prove False [(caller, atomA)] (atomB :-> atomA)
    case proofs of
        [] -> fail "expected b -> a to be realizable from x2 :: a"
        proof : _ ->
            assertEqual "the rendered proof must refer to the caller's x2"
                "f _ = x2" (hPrClause $ termToHClause "f" proof)

-- The continuation atom introduced while proving a disjunction lives in the
-- same Symbol namespace as caller formula atoms.  `_2` in the environment is
-- unrelated to a or b and must not accidentally make their disjunction true.
testContinuationAtomCapture :: IO ()
testContinuationAtomCapture =
    assertEqual "an unrelated _2 assumption cannot prove a | b"
        [] (prove False [(Symbol "u", PVar $ Symbol "_2")] (atomA |: atomB))

testCsplitResidualArguments :: IO ()
testCsplitResidualArguments = do
    let left = Symbol "left"
        right = Symbol "right"
        handler = Lam left $ Lam right $
            applys (Var $ Symbol "combine") [Var left, Var right]
        term = applys (Csplit 2)
            [ handler
            , Var $ Symbol "pair"
            , Var $ Symbol "arg1"
            , Var $ Symbol "arg2"
            ]
        rendered = hPrClause $ termToHClause "f" term
    assertContains "Csplit should still render its tuple case" "case pair of" rendered
    assertWordsSuffix "Csplit residual arguments must retain left-to-right order"
        ["arg1", "arg2"] rendered

testCcasesResidualArguments :: IO ()
testCcasesResidualArguments = do
    let leftConstructor = ConsDesc "LeftC" 1
        rightConstructor = ConsDesc "RightC" 1
        left = Symbol "leftPayload"
        right = Symbol "rightPayload"
        leftHandler = Lam left $ Apply (Var $ Symbol "onLeft") (Var left)
        rightHandler = Lam right $ Apply (Var $ Symbol "onRight") (Var right)
        term = applys (Ccases [leftConstructor, rightConstructor])
            [ Var $ Symbol "choice"
            , leftHandler
            , rightHandler
            , Var $ Symbol "arg1"
            , Var $ Symbol "arg2"
            ]
        rendered = hPrClause $ termToHClause "f" term
    assertContains "Ccases should still render its scrutiny" "case choice of" rendered
    assertContains "Ccases should retain its first alternative" "LeftC" rendered
    assertContains "Ccases should retain its second alternative" "RightC" rendered
    assertWordsSuffix "Ccases residual arguments must not be dropped or reordered"
        ["arg1", "arg2"] rendered

testProofEnvironment :: IO ()
testProofEnvironment = do
    let target = Symbol "answer"
        duplicate = Symbol "shared"
        environment = prepareProofEnvironment target
            [ (target, atomA)
            , (duplicate, atomA)
            , (duplicate, atomB)
            ]
        bindings = proofBindings environment
    assertBool "a target-named assumption must be excluded"
        (targetWasExcluded environment)
    assertEqual "only the unsafe target binding should be removed"
        [atomA, atomB] (map snd bindings)
    assertEqual "every remaining assumption needs a unique proof identity"
        2 (length $ nub $ map fst bindings)
    case bindings of
        (internal, _) : _ -> do
            assertEqual "free proof identities should regain their display names"
                (Var duplicate) (restoreProofTerm environment $ Var internal)
            assertEqual "a safe alternate assumption should prove the target"
                [Var duplicate]
                (map (restoreProofTerm environment) $
                    prove False bindings atomA)
        [] -> fail "expected safe proof bindings"

testGeneratedProofsCheck :: IO ()
testGeneratedProofsCheck = do
    let cases =
            [ ([], atomA :-> atomA)
            , ([], (atomA & atomB) :-> (atomB & atomA))
            , ([], atomA :-> (atomA |: atomB))
            , ([], false :-> atomA)
            , ([(Symbol "given", atomA)], atomB :-> atomA)
            ]
    mapM_ checkGenerated cases
  where
    checkGenerated (environment, formula) =
        case prove False environment formula of
            [] -> fail $ "expected a generated proof of " ++ show formula
            proof : _ -> assertEqual
                ("independent checker rejected " ++ show proof)
                (Right ()) (checkProof environment formula proof)

testMalformedProofTerms :: IO ()
testMalformedProofTerms = do
    let a = Symbol "a"
        b = Symbol "b"
        leftConstructor = ConsDesc "Left" 1
    assertLeft "identity cannot prove a -> b"
        (checkProof [] (atomA :-> atomB) $ Lam a $ Var a)
    assertLeft "an injection must name the expected constructor"
        (checkProof [] (atomA :-> (atomA |: atomB)) $
            Lam a $ Apply (Cinj (ConsDesc "Wrong" 1) 0) (Var a))
    assertLeft "a one-element tuple cannot prove a pair"
        (checkProof [] ((atomA & atomB) :-> (atomA & atomB)) $
            Lam a $ Apply (Ctuple 1) (Var a))
    assertLeft "legacy selectors have no independently checkable semantics"
        (checkProof [(b, atomA)] atomA $ Xsel 0 1 (Var b))
    assertLeft "an injection index must be in range"
        (checkProof [] (atomA :-> (atomA |: atomB)) $
            Lam a $ Apply (Cinj leftConstructor 2) (Var a))

testNominalEmptyTypes :: IO ()
testNominalEmptyTypes = do
    let definitions =
            [ ("EmptyA", ([], HTUnion [], ()))
            , ("EmptyB", ([], HTUnion [], ()))
            , ("AliasA", ([], HTCon "EmptyA", ()))
            ]
        emptyA = hTypeToFormula definitions $ HTCon "EmptyA"
        emptyB = hTypeToFormula definitions $ HTCon "EmptyB"
        aliasA = hTypeToFormula definitions $ HTCon "AliasA"
        cast = emptyA :-> emptyB
        identity = emptyA :-> emptyA
    assertBool "distinct empty datatypes need distinct propositions"
        (emptyA /= emptyB)
    assertEqual "an alias should retain its underlying nominal identity"
        emptyA aliasA
    case prove True [] identity of
        [Lam binder (Var used)] ->
            assertEqual "the same empty type should use identity" binder used
        proofs -> fail $ "expected only empty identity, got " ++ show proofs
    case prove False [] cast of
        [proof@(Lam binder (Apply (Ccases []) (Var used)))] -> do
            assertEqual "empty conversion must eliminate its argument explicitly"
                binder used
            assertEqual "the explicit empty elimination should type-check"
                (Right ()) (checkProof [] cast proof)
            assertContains "empty elimination should render via void"
                "void" (hPrClause $ termToHClause "cast" proof)
        proofs -> fail $ "expected one explicit empty elimination, got " ++
            show proofs
    assertLeft "a direct identity cast between empty types must be rejected"
        (checkProof [] cast $ Lam (Symbol "x") (Var $ Symbol "x"))

atomA :: Formula
atomA = PVar $ Symbol "a"

atomB :: Formula
atomB = PVar $ Symbol "b"

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual message expected actual =
    unless (expected == actual) $
        fail $ message ++ ": expected " ++ show expected ++ ", got " ++ show actual

assertBool :: String -> Bool -> IO ()
assertBool message condition = unless condition $ fail message

assertLeft :: Show a => String -> Either String a -> IO ()
assertLeft _ (Left _) = return ()
assertLeft message (Right value) =
    fail $ message ++ ": expected an error, got " ++ show value

assertContains :: String -> String -> String -> IO ()
assertContains message needle haystack =
    assertBool (message ++ ": " ++ show needle ++ " not found in " ++ show haystack)
        (needle `isInfixOf` haystack)

assertWordsSuffix :: String -> [String] -> String -> IO ()
assertWordsSuffix message suffix rendered =
    assertBool (message ++ ": got " ++ show rendered)
        (suffix `isSuffixOf` words rendered)
