module Main (main) where

import Data.List (isInfixOf, isPrefixOf, isSuffixOf, nub, sort)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Text.ParserCombinators.ReadP (ReadP, eof, readP_to_S, skipSpaces)
import Text.Read (readMaybe)

import Djinn.Core (
    Declaration(..), QueryOutcome(..),
    declare, defaultQueryOptions, emptyEnvironment, inhabit,
    kArrow, kStar, optionAlternatives, optionBudget, optionSorted,
    parseHKind, parseHType, removeDeclaration,
    reportOutcome, resolveContext, standardEnvironment)
import Djinn.Internal.Environment (validateEnvironment)
import Djinn.Internal.HCheck (
    htCheckEnv, htCheckType, htCheckTypeKind, htCheckTypesKinds,
    htInferClassKinds)
import Djinn.Internal.HIdentifier
import Djinn.Internal.HTypes
import Djinn.Internal.LJT
import Djinn.Internal.ProofCheck (checkProof)
import Djinn.Internal.ProofEnv

main :: IO ()
main = defaultMain $ testGroup "Djinn unit tests" $
    [testCase name action | (name, action) <- tests]

tests :: [(String, Assertion)]
tests =
    [ ("parse prefix function constructor", testPrefixArrowParsing)
    , ("kind-check intrinsic list syntax", testIntrinsicListKind)
    , ("render canonical units and kinds", testCanonicalRendering)
    , ("normalize aliases inside opaque formula atoms", testOpaqueAliasAtoms)
    , ("infer and reuse a higher-kinded synonym", testHigherKindedGrounding)
    , ("reject an ill-kinded higher-kinded application", testIllKindedApplication)
    , ("reject a higher-kinded synonym body", testHigherKindedSynonymBody)
    , ("infer and enforce class parameter kinds", testClassParameterKinds)
    , ("prove intuitionistic tautologies", testProvableBasics)
    , ("prove empty goals from contradictions", testEmptyGoalContradiction)
    , ("reject non-theorems", testNonTheorems)
    , ("honor search budgets and strategies", testSearchModes)
    , ("use an assumption as its named proof", testNamedAssumption)
    , ("do not capture a caller-supplied proof symbol", testCallerSymbolCapture)
    , ("keep disjunction continuation atoms fresh", testContinuationAtomCapture)
    , ("preserve residual application after Csplit", testCsplitResidualArguments)
    , ("preserve a whole product through Csplit", testCsplitProductIdentity)
    , ("bind unary constructor fields without tuple parentheses",
          testUnaryConstructorPattern)
    , ("preserve residual application after Ccases", testCcasesResidualArguments)
    , ("preserve tuple payloads in unary constructors", testUnaryTuplePayload)
    , ("merge tuple refinements across case branches", testBranchRefinements)
    , ("reconstruct whole constructor payloads", testWholeConstructorPayload)
    , ("isolate external proof identities", testProofEnvironment)
    , ("type-check generated proofs independently", testGeneratedProofsCheck)
    , ("reject malformed proof terms", testMalformedProofTerms)
    , ("preserve nominal empty types", testNominalEmptyTypes)
    , ("validate declaration mutations transactionally", testEnvironmentValidation)
    , ("render shadowing terms without capture", testScopeSafeRendering)
    , ("report malformed proof rendering", testMalformedRendering)
    , ("accept only Haskell identifiers and operators", testIdentifiers)
    , ("validate every boundary of the Djinn.Core facade", testCoreFacade)
    ]

-- The library facade must make invalid environments unrepresentable and
-- report search results honestly.
testCoreFacade :: IO ()
testCoreFacade = do
    -- Boundary validation of declarations.
    assertLeft "a lowercase type name is rejected"
        (declare (DataType "bad" [] []) emptyEnvironment)
    assertLeft "a duplicate type parameter is rejected"
        (declare (TypeSynonym "Pair" ["a", "a"]
            (HTTuple [HTVar "a", HTVar "a"])) emptyEnvironment)
    assertLeft "a recursive data type is rejected"
        (declare (DataType "Nat" []
            [("Zero", []), ("Succ", [HTCon "Nat"])]) emptyEnvironment)
    assertLeft "a constructor owned by another type is rejected"
        (declare (DataType "MyBool" [] [("True", [])])
            standardEnvironment)
    assertLeft "an unsolved kind variable cannot be declared"
        (declare (AbstractType "Mystery" (KVar 0)) emptyEnvironment)
    assertLeft "an ill-kinded function type is rejected"
        (declare (Function "f" (HTApp (HTCon "Bool") (HTVar "a")))
            standardEnvironment)
    assertLeft "a type synonym must be fully saturated in higher-kinded use" $ do
        environment <- declare
            (TypeSynonym "Pair" ["a", "b"]
                (HTTuple [HTVar "a", HTVar "b"]))
            standardEnvironment
        environment' <- declare
            (AbstractType "HK" $ kArrow (kArrow kStar kStar) kStar)
            environment
        let partialPair = HTApp (HTCon "Pair") (HTVar "a")
            wrapped = HTApp (HTCon "HK") partialPair
        inhabit defaultQueryOptions environment' [] "badSynonym"
            (HTArrow wrapped wrapped)
    assertLeft "a method-owning clash across classes is rejected"
        (declare (ClassDecl "Eq2" ["a"]
            [("==", HTVar "a")]) standardEnvironment)

    -- Removal is transactional and total.
    assertLeft "removing an undefined name is an error"
        (removeDeclaration "nosuch" standardEnvironment)
    assertLeft "removing a depended-upon type is rejected"
        (removeDeclaration "Void" standardEnvironment)
    case removeDeclaration "Not" standardEnvironment of
        Left message -> fail $ "removing a leaf synonym failed: " ++ message
        Right environment ->
            assertLeft "the removal must actually take effect"
                (removeDeclaration "Not" environment)

    -- Parsing consumes the whole input.
    assertEqual "parseHType parses ordinary types"
        (Right $ HTArrow (HTVar "a") (HTVar "a")) (parseHType "a -> a")
    assertLeft "trailing garbage is a parse error" (parseHType "a -> a ->")
    assertEqual "parseHKind parses higher kinds"
        (Right $ KArrow (KArrow KStar KStar) KStar)
        (parseHKind "(* -> *) -> *")

    -- Queries report honest outcomes.
    swap <- expectRight $ parseHType "(a, b) -> (b, a)"
    swapReport <- expectRight $
        inhabit defaultQueryOptions standardEnvironment [] "swap" swap
    assertEqual "swap is realized with the canonical clause"
        (Realized ["swap (a, b) = (b, a)"]) (reportOutcome swapReport)
    peirce <- expectRight $ parseHType "((a -> b) -> a) -> a"
    peirceReport <- expectRight $
        inhabit defaultQueryOptions standardEnvironment [] "peirce" peirce
    assertEqual "Peirce's law is decided unrealizable"
        Unrealizable (reportOutcome peirceReport)
    starved <- expectRight $ inhabit
        defaultQueryOptions { optionBudget = Just 0 }
        standardEnvironment [] "peirce" peirce
    assertEqual "an expired budget is undecided, not unrealizable"
        Undecided (reportOutcome starved)
    selfRef <- expectRight $ do
        environment <- declare (Function "token" (HTVar "a"))
            standardEnvironment
        inhabit defaultQueryOptions environment [] "token" (HTVar "a")
    assertEqual "a lone same-named assumption is flagged, not recursed"
        UnrealizableWithoutSelfReference (reportOutcome selfRef)

    -- Contexts resolve through inferred kinds.
    assertLeft "a kind-mismatched class argument is rejected"
        (resolveContext standardEnvironment ("Monad", [HTCon "Bool"]))
    assertLeft "one variable cannot have inconsistent kinds across arguments" $ do
        environment <- declare
            (ClassDecl "ApplyToBool" ["f", "unused"]
                [("applyToBool", HTApp (HTVar "f") (HTCon "Bool"))])
            standardEnvironment
        resolveContext environment
            ("ApplyToBool", [HTVar "shared", HTVar "shared"])
    assertLeft "a context and goal must share kind assignments" $ do
        environment <- declare (ClassDecl "Value" ["a"] [])
            standardEnvironment
        let higherKinded = HTApp (HTVar "f") (HTCon "Bool")
        inhabit defaultQueryOptions environment [("Value", [HTVar "f"])]
            "badKinds" (HTArrow higherKinded higherKinded)
    assertLeft "negative public search budgets are rejected" $
        inhabit defaultQueryOptions { optionBudget = Just (-1) }
            standardEnvironment [] "identity" (HTArrow (HTVar "a") (HTVar "a"))
    reflexive <- expectRight $ inhabit defaultQueryOptions
        standardEnvironment [("Eq", [HTVar "a"])] "reflexive"
        (HTArrow (HTVar "a") (HTCon "Bool"))
    case reportOutcome reflexive of
        Realized (best : _) ->
            assertEqual "an Eq context supplies its method, ranked first"
                "reflexive a = a == a" best
        other -> fail $ "reflexive was not realized: " ++ show other

expectRight :: Either String a -> IO a
expectRight = either fail return

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
    assertEqual "unit syntax permits ordinary token whitespace"
        (Just $ HTCon "()") (readMaybe "( )")
    assertEqual "kinds should use the syntax accepted by the parser"
        "(* -> *) -> * -> *"
        (show $ KArrow (KArrow KStar KStar) (KArrow KStar KStar))

-- Type synonyms are transparent even when they occur below an opaque type
-- constructor.  The whole opaque application remains one proposition, but
-- its atom name must use the normalized type rather than the surface alias.
testOpaqueAliasAtoms :: IO ()
testOpaqueAliasAtoms = do
    let definitions =
            [ ("Id", (["a"], HTVar "a", ()))
            , ("Pair", (["a"], HTTuple [HTVar "a", HTVar "a"], ()))
            , ("F", ([], HTAbstract "F" (KArrow KStar KStar), ()))
            ]
        app constructor argument = HTApp (HTCon constructor) argument
    assertEqual "an alias below an abstract constructor is transparent"
        (hTypeToFormula definitions $ app "F" $ HTVar "a")
        (hTypeToFormula definitions $ app "F" $ app "Id" $ HTVar "a")
    assertEqual "aliases normalize recursively below intrinsic applications"
        (hTypeToFormula definitions $ app "[]" $
            HTTuple [HTVar "b", HTVar "b"])
        (hTypeToFormula definitions $ app "[]" $ app "Pair" $ HTVar "b")

    let result = do
            idBody <- parseHType "a"
            goal <- parseHType "F (Id a) -> F a"
            environment <- declare
                (TypeSynonym "Id" ["a"] idBody) emptyEnvironment
            environment' <- declare
                (AbstractType "F" $ kArrow kStar kStar) environment
            inhabit defaultQueryOptions environment' [] "coerce" goal
    case result of
        Left message -> fail $ "opaque alias query failed: " ++ message
        Right report -> assertEqual
            "the normalized opaque application should admit identity"
            (Realized ["coerce a = a"]) (reportOutcome report)

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

-- Class parameter kinds come from the method types (Haskell98 style) and
-- default to * when unconstrained, including for method-less classes.
-- Class arguments must then fit the inferred parameter kind, closing the
-- historical hole where `class Empty a where` accepted any argument.
testClassParameterKinds :: IO ()
testClassParameterKinds = do
    checked <- checkedKindEnvironment
    let m = HTVar "m"
        a = HTVar "a"
        b = HTVar "b"
        ma = HTApp m a
        mb = HTApp m b
        monadMethods =
            [ HTArrow a ma
            , HTArrow ma (HTArrow (HTArrow a mb) mb)
            ]
    assertEqual "Monad's parameter must infer to * -> *"
        (Right [("m", KArrow KStar KStar)])
        (htInferClassKinds checked ["m"] monadMethods)
    assertEqual "a method-less class parameter defaults to *"
        (Right [("phantom", KStar)])
        (htInferClassKinds checked ["phantom"] [])
    assertLeft "a method type that misuses a parameter is rejected"
        (htInferClassKinds checked ["c"]
            [HTArrow (HTVar "c") (HTApp (HTVar "c") a)])
    assertRight "a proper-type argument fits kind *"
        (htCheckTypeKind checked KStar (HTCon "Bool"))
    assertRight "a variable argument fits any kind"
        (htCheckTypeKind checked (KArrow KStar KStar) (HTVar "f"))
    assertRight "a constructor fits its higher kind"
        (htCheckTypeKind checked (KArrow KStar KStar) (HTCon "Maybe"))
    assertLeft "an ill-kinded application is rejected even against *"
        (htCheckTypeKind checked KStar (HTApp (HTCon "Bool") a))
    assertLeft "a kind-mismatched argument is rejected"
        (htCheckTypeKind checked (KArrow KStar KStar) (HTCon "Bool"))
    assertLeft "joint checks share the kind of a free variable"
        (htCheckTypesKinds checked
            [ (KStar, HTVar "shared")
            , (KArrow KStar KStar, HTVar "shared")
            ])

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
        , ( "conjunction commutes for nested implications"
          , (nested & atomA) :-> (atomA & nested)
          )
        , ("a value injects into a disjunction", atomA :-> (atomA |: atomB))
        , ("false eliminates", false :-> atomA)
        ]
  where
    assertTrue name formula = assertBool name (provable formula)
    nested = fnot $ fnot $ Empty $ Symbol "EmptyA"

-- The goal of the double negation of excluded middle reduces to Void with
-- contradictory antecedents.  redsucc must route an Empty goal through the
-- same fresh-atom encoding as a disjunction; rejecting it outright (mzero)
-- silently lost every theorem whose final goal is an empty type.
testEmptyGoalContradiction :: IO ()
testEmptyGoalContradiction = do
    let assertProvableAndChecked name goal =
            case prove False [] goal of
                [] -> fail $ name ++ " must be provable"
                proof : _ ->
                    assertRight
                        (name ++ ": the proof must check against its formula")
                        (checkProof [] goal proof)
    assertProvableAndChecked "Not (Not (Either a (Not a)))" $
        fnot $ fnot $ atomA |: fnot atomA
    -- QuickCheck-discovered case: the proof feeds one ex falso result into
    -- another, leaving an interior proof type free.  The checker must
    -- default that empty-eliminator input rather than reject the proof
    -- as ambiguous.
    let nested = (atomA :-> atomA) :-> Conj [atomB]
    assertProvableAndChecked "Not x -> Not c -> Not (Either x c)" $
        fnot nested :-> (fnot atomC :-> fnot (nested |: atomC))
    -- Raw internal clients can still construct the old structural encoding
    -- of false.  It eliminates into nominal Void, but is not definitionally
    -- identical to it and therefore needs an explicit empty-case term.
    let structuralToNominal = Disj [] :-> false
    case prove False [] structuralToNominal of
        [proof@(Lam binder (Apply (Ccases []) (Var used)))] -> do
            assertEqual "empty elimination must consume its premise" binder used
            assertRight "the structural-to-nominal proof must check" $
                checkProof [] structuralToNominal proof
            assertRendered "structural false should render with void"
                "eliminate = void" "eliminate" proof
        proofs -> fail $ "expected one structural empty eliminator, got " ++
            show proofs

testNonTheorems :: IO ()
testNonTheorems =
    mapM_ (uncurry assertFalse)
        [ ("an unconstrained atom", atomA)
        , ("Peirce's law is not intuitionistic", ((atomA :-> atomB) :-> atomA) :-> atomA)
        ]
  where
    assertFalse name formula = assertBool name (not $ provable formula)

testSearchModes :: IO ()
testSearchModes = do
    let peirce = ((atomA :-> atomB) :-> atomA) :-> atomA
        pick3 = atomA :-> (atomA :-> (atomA :-> atomA))
        unlimited = defaultSearchMode True
        run mode = proveWithMode mode []

    -- A finished search decides; it is never marked exhausted.
    let complete = run unlimited peirce
    assertEqual "a finished search refutes a non-theorem"
        [] (searchProofs complete)
    assertBool "a finished search must not be marked exhausted" $
        not (searchExhausted complete)

    -- A zero budget cannot decide anything that branches.
    let starved = run unlimited { searchBudget = Just 0 } peirce
    assertEqual "a starved search finds nothing" [] (searchProofs starved)
    assertBool "an expired budget must be reported" $ searchExhausted starved

    -- Internal callers can construct SearchMode directly.  Treat a negative
    -- budget as already expired rather than accidentally making it unlimited.
    let invalidBudget = run unlimited { searchBudget = Just (-1) } peirce
    assertEqual "a negative internal budget finds nothing"
        [] (searchProofs invalidBudget)
    assertBool "a negative internal budget must be reported as expired" $
        searchExhausted invalidBudget

    -- A bounded depth-first search yields a prefix of the unbounded stream.
    let full = searchProofs $ run unlimited pick3
        bounded = run unlimited { searchBudget = Just 1 } pick3
    assertBool "bounded proofs must be a prefix of the unbounded stream" $
        searchProofs bounded `isPrefixOf` full
    assertBool "the pick-3 space is larger than one step" $
        searchExhausted bounded

    -- Interleaving may reorder proofs but proves and refutes the same
    -- formulas over a finite space.
    let fair = unlimited { searchStrategy = Interleave }
    assertBool "interleaved search still refutes Peirce's law" $
        null $ searchProofs $ run fair peirce
    assertEqual "interleaved search finds the same pick-3 proof set"
        (sort full) (sort $ searchProofs $ run fair pick3)

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
            assertRendered "the rendered proof must refer to the caller's x2"
                "f _ = x2" "f" proof

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
    rendered <- renderTerm "f" term
    assertContains "Csplit should still render its tuple case" "case pair of" rendered
    assertWordsSuffix "Csplit residual arguments must retain left-to-right order"
        ["arg1", "arg2"] rendered

-- If a split handler returns the original product, its component patterns are
-- unused but the as-binder is not.  Simplifying @pair@_@ to @_@ used to emit a
-- typed hole; it must simplify to the still-bound @pair@ instead.
testCsplitProductIdentity :: IO ()
testCsplitProductIdentity = do
    let pair = Symbol "pair"
        left = Symbol "left"
        right = Symbol "right"
        term = Lam pair $ applys (Csplit 2)
            [Lam left $ Lam right $ Var pair, Var pair]
        formula = (atomA & atomB) :-> (atomA & atomB)
    assertRight "the raw product identity must be a valid proof" $
        checkProof [] formula term
    assertRendered "an as-pattern over wildcards must retain its binder"
        "identity a = a" "identity" term

-- A unary constructor field arrives as a 1-ary split.  Haskell has no
-- 1-tuples, so the field must bind as `Wrap a`, not as `Wrap (a)`.
testUnaryConstructorPattern :: IO ()
testUnaryConstructorPattern = do
    let payload = Symbol "payload"
        field = Symbol "field"
        handler = Lam payload $ applys (Csplit 1)
            [ Lam field $ Apply (Var $ Symbol "use") (Var field)
            , Var payload
            ]
        term = applys (Ccases [ConsDesc "Wrap" 1])
            [Var $ Symbol "value", handler]
    rendered <- renderTerm "unwrap" term
    assertContains "a unary constructor field binds the payload directly"
        "Wrap a" rendered
    assertBool "no singleton tuple pattern may remain in the output" $
        not $ "(a)" `isInfixOf` rendered

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
    rendered <- renderTerm "f" term
    assertContains "Ccases should still render its scrutiny" "case choice of" rendered
    assertContains "Ccases should retain its first alternative" "LeftC" rendered
    assertContains "Ccases should retain its second alternative" "RightC" rendered
    assertWordsSuffix "Ccases residual arguments must not be dropped or reordered"
        ["arg1", "arg2"] rendered

testUnaryTuplePayload :: IO ()
testUnaryTuplePayload = do
    let constructor = ConsDesc "Only" 1
        payload = Symbol "payload"
        left = Symbol "left"
        right = Symbol "right"
        handler = Lam payload $ applys (Csplit 2)
            [ Lam left $ Lam right $ Apply (Var $ Symbol "consume") $
                applys (Ctuple 2) [Var left, Var right]
            , Var payload
            ]
        term = applys (Ccases [constructor])
            [Var $ Symbol "value", handler]
    rendered <- renderTerm "unwrap" term
    assertContains "the tuple must remain one constructor field"
        "Only (a, b)" rendered

testBranchRefinements :: IO ()
testBranchRefinements = do
    let outer = Symbol "outer"
        choice = Symbol "choice"
        shared = Symbol "shared"
        leftConstructor = ConsDesc "LeftC" 1
        rightConstructor = ConsDesc "RightC" 1
        splitShared prefix =
            let first = Symbol $ prefix ++ "First"
                second = Symbol $ prefix ++ "Second"
            in applys (Csplit 2)
                [ Lam first $ Lam second $
                    applys (Ctuple 2) [Var first, Var second]
                , Var shared
                ]
        body = applys (Ccases [leftConstructor, rightConstructor])
            [ Var choice
            , Lam (Symbol "leftPayload") $ splitShared "left"
            , Lam (Symbol "rightPayload") $ splitShared "right"
            ]
        term = Lam outer $ applys (Csplit 2)
            [Lam choice $ Lam shared body, Var outer]
    _ <- renderTerm "mergeBranches" term
    return ()

-- A disjunction handler receives one logical payload value.  For a constructor
-- with several Haskell fields that value is their tuple, so a handler that
-- returns it whole must bind the fields and reconstruct the tuple explicitly.
testWholeConstructorPayload :: IO ()
testWholeConstructorPayload = do
    let constructor = ConsDesc "C" 2
        payload = Conj [atomA, atomB]
        formula = Disj [(constructor, payload)] :-> payload
        proofs = prove True [] formula
        expected = "f a =\n" ++
            "    case a of\n" ++
            "    C b c -> (b, c)"
    assertEqual "the theorem should expose both proof-search alternatives"
        2 (length proofs)
    rendered <- mapM (renderTerm "f") proofs
    assertEqual "every alternative must reconstruct the constructor payload"
        [expected, expected] rendered
    assertBool "no alpha-renamed implementation binder may escape" $
        all (not . isInfixOf "__djinn") rendered

    goal <- either fail return $ parseHType "T a b -> (a, b)"
    environment <- either fail return $
        declare (DataType "T" ["a", "b"]
            [("C", [HTVar "a", HTVar "b"])]) emptyEnvironment
    let options = defaultQueryOptions {
            optionAlternatives = True,
            optionSorted = False
            }
    report <- either fail return $
        inhabit options environment [] "f" goal
    assertEqual "the public boundary should de-duplicate equivalent clauses"
        (Realized [expected]) (reportOutcome report)

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
            , ("EmptyOf", (["a"], HTUnion [], ()))
            , ("Flag", ([], HTUnion [("Flag", [])], ()))
            , ("FlagAlias", ([], HTCon "Flag", ()))
            ]
        emptyA = hTypeToFormula definitions $ HTCon "EmptyA"
        emptyB = hTypeToFormula definitions $ HTCon "EmptyB"
        aliasA = hTypeToFormula definitions $ HTCon "AliasA"
        emptyOfFlag = hTypeToFormula definitions $
            HTApp (HTCon "EmptyOf") (HTCon "Flag")
        emptyOfAlias = hTypeToFormula definitions $
            HTApp (HTCon "EmptyOf") (HTCon "FlagAlias")
        cast = emptyA :-> emptyB
        identity = emptyA :-> emptyA
    assertBool "distinct empty datatypes need distinct propositions"
        (emptyA /= emptyB)
    assertEqual "an alias should retain its underlying nominal identity"
        emptyA aliasA
    assertEqual "aliases in empty-type arguments should be transparent"
        emptyOfFlag emptyOfAlias
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
            rendered <- renderTerm "cast" proof
            assertContains "empty elimination should render via void"
                "void" rendered
        proofs -> fail $ "expected one explicit empty elimination, got " ++
            show proofs
    assertLeft "a direct identity cast between empty types must be rejected"
        (checkProof [] cast $ Lam (Symbol "x") (Var $ Symbol "x"))

testEnvironmentValidation :: IO ()
testEnvironmentValidation = do
    let base =
            [ ("Base", ([], HTUnion [("Base", [])], KStar))
            , ("Alias", ([], HTCon "Base", KStar))
            ]
        withoutBase = filter ((/= "Base") . fst) base
        changedArity =
            ("Base", (["a"], HTVar "a", KStar)) : withoutBase
        axiom = ("given", HTCon "Base")
        classDefinition =
            ("UsesBase", ([], [("useBase", HTCon "Base")]))
    assertLeft "deletion must reject a dependent synonym"
        (validateEnvironment withoutBase [] [])
    assertLeft "replacement must reject a newly unsaturated dependency"
        (validateEnvironment changedArity [] [])
    assertLeft "deletion must reject a dependent axiom"
        (validateEnvironment [] [axiom] [])
    assertLeft "deletion must reject a dependent class method"
        (validateEnvironment [] [] [classDefinition])
    assertRight "an unrelated declaration set should still rebuild"
        (validateEnvironment
            [("Other", ([], HTUnion [("Other", [])], KStar))]
            [] [])

testScopeSafeRendering :: IO ()
testScopeSafeRendering = do
    let shadowed = Symbol "x"
        term = Lam shadowed $
            Apply (Var shadowed) (Lam shadowed $ Var shadowed)
    rendered <- renderTerm "applyIdentity" term
    assertEqual "nested shadowing binders should receive distinct names"
        "applyIdentity a = a (\\ b -> b)" rendered
    let generatedPrefix = Symbol "__djinn1"
        argument = Symbol "argument"
        externalTerm = Lam argument $
            Apply (Var generatedPrefix) (Var argument)
    assertRendered "a generated-prefix external assumption remains free"
        "applyExternal = __djinn1" "applyExternal" externalTerm

    -- This spelling is also the first field name the payload elaborator would
    -- prefer for its first alpha-renamed branch binder.  Reserving all source
    -- names forces a different local binder and prevents a string collision
    -- from hiding a scope leak from the final validator.
    let externalField = Symbol "__djinn1_field1"
        choice = Symbol "choice"
        payload = Symbol "payload"
        constructor = ConsDesc "C" 2
        caseTerm = applys (Ccases [constructor])
            [ Var choice
            , Lam payload $ Apply (Var externalField) (Var payload)
            ]
    renderedCase <- renderTerm "applyPayload" caseTerm
    assertContains "the adversarial external name must remain untouched"
        "__djinn1_field1" renderedCase
    assertContains "the constructor fields must still be lexically bound"
        "C a b -> __djinn1_field1 (a, b)" renderedCase

testMalformedRendering :: IO ()
testMalformedRendering = do
    assertLeft "legacy selectors should return a conversion error"
        (termToHExpr $ Xsel 0 1 $ Var $ Symbol "x")
    assertLeft "bare non-unit tuple combinators should not crash"
        (termToHExpr $ Ctuple 2)
    assertLeft "case alternatives must be lambdas"
        (termToHExpr $ applys (Ccases [ConsDesc "Only" 1])
            [Var $ Symbol "choice", Var $ Symbol "handler"])

testIdentifiers :: IO ()
testIdentifiers = do
    assertParses "leading underscores are valid variable identifiers"
        pVarId "_compose'"
    assertParses "qualified external variables are valid"
        pQualifiedVarId "Data.Function.id"
    assertParses "qualified constructors are valid"
        pQualifiedConId "Data.Maybe.Maybe"
    assertParses "the full ASCII operator alphabet is available"
        pParenthesizedVarOp "(/?)"
    assertParses "Unicode Haskell operators are available"
        pParenthesizedVarOp "(⊕)"
    assertDoesNotParse "reserved words are not identifiers" pVarId "case"
    assertDoesNotParse "a bare underscore is not a binding name" pVarId "_"
    assertDoesNotParse "module segments must be constructors"
        pQualifiedVarId "data.module.value"
    assertDoesNotParse "empty qualification segments are invalid"
        pQualifiedVarId "Data..value"
    assertDoesNotParse "trailing qualification dots are invalid"
        pQualifiedVarId "Data.value."
    assertDoesNotParse "reserved operators are invalid binding names"
        pParenthesizedVarOp "(->)"
    assertDoesNotParse "a line-comment introducer is not an operator name"
        pParenthesizedVarOp "(--)"
    assertEqual "underscore identifiers must not be printed as operators"
        "_compose" (prHSymbolOp "_compose")
    assertEqual "qualified variables must remain prefix names"
        "Data.Function.id" (prHSymbolOp "Data.Function.id")
    assertEqual "operators should be parenthesized in prefix positions"
        "(/?)" (prHSymbolOp "/?")
    assertEqual "qualified type constructors should parse structurally"
        (Just $ HTApp (HTCon "Data.Maybe.Maybe") (HTVar "a"))
        (readMaybe "Data.Maybe.Maybe a")
    assertEqual "ordinary command-file comments are removed"
        "identity ? a -> a \n\n"
        (stripLineComments "identity ? a -> a -- trailing\n-- whole line\n")
    assertEqual "dash-prefixed operators are not mistaken for comments"
        "(--*) :: a -> a\n"
        (stripLineComments "(--*) :: a -> a\n")

atomA :: Formula
atomA = PVar $ Symbol "a"

atomB :: Formula
atomB = PVar $ Symbol "b"

atomC :: Formula
atomC = PVar $ Symbol "c"

assertLeft :: Show a => String -> Either String a -> IO ()
assertLeft _ (Left _) = return ()
assertLeft message (Right value) =
    fail $ message ++ ": expected an error, got " ++ show value

assertRight :: Show a => String -> Either String a -> IO ()
assertRight _ (Right _) = return ()
assertRight message (Left errorMessage) =
    fail $ message ++ ": unexpected error: " ++ errorMessage

assertRendered :: String -> String -> HSymbol -> Term -> IO ()
assertRendered message expected name term = do
    actual <- renderTerm name term
    assertEqual message expected actual

renderTerm :: HSymbol -> Term -> IO String
renderTerm name term =
    case termToHClause name term of
        Left message -> fail $ "proof rendering failed: " ++ message
        Right clause -> return $ hPrClause clause

assertParses :: String -> ReadP String -> String -> IO ()
assertParses message parser input =
    assertBool message $ not $ null $ parseFully parser input

assertDoesNotParse :: String -> ReadP String -> String -> IO ()
assertDoesNotParse message parser input =
    assertBool message $ null $ parseFully parser input

parseFully :: ReadP a -> String -> [a]
parseFully parser input =
    [value | (value, "") <- readP_to_S complete input]
  where
    complete = do
        value <- parser
        skipSpaces
        eof
        return value

assertContains :: String -> String -> String -> IO ()
assertContains message needle haystack =
    assertBool (message ++ ": " ++ show needle ++ " not found in " ++ show haystack)
        (needle `isInfixOf` haystack)

assertWordsSuffix :: String -> [String] -> String -> IO ()
assertWordsSuffix message suffix rendered =
    assertBool (message ++ ": got " ++ show rendered)
        (suffix `isSuffixOf` words rendered)
