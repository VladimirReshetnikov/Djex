module Main (main) where

import Data.List (isInfixOf, isPrefixOf, isSuffixOf, nub, sort)
import qualified Data.Map.Strict as Map
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Text.ParserCombinators.ReadP (ReadP, eof, readP_to_S, skipSpaces)
import Text.Read (readMaybe)

import Djinn.Core (
    Context, Declaration(..), QueryOutcome(..),
    SynthesisDeclarationError(..), SynthesisEnvironmentError(..),
    SynthesisTypeError(..),
    classDeclarations, declare, defaultQueryOptions, emptyEnvironment,
    functionDeclarations, inhabit,
    kArrow, kStar, optionAlternatives, optionBudget, optionSorted,
    fromSynthesisDeclaration, fromSynthesisEnvironment,
    fromSynthesisKind, fromSynthesisType,
    mkContext, parseHKind, parseHType, removeDeclaration,
    reportCompletion, reportGeneratedClauses, reportOutcome,
    resolveContext, resolveInstanceMethods,
    standardEnvironment, toSynthesisDeclaration, toSynthesisEnvironment,
    toSynthesisInventory,
    toSynthesisKind,
    toSynthesisType, typeDeclarations)
import Djinn.Internal.Environment (validateEnvironment)
import Djinn.Internal.HCheck (
    htCheckEnv, htCheckType, htCheckTypeKind, htCheckTypesKinds,
    htInferClassKinds)
import Djinn.Internal.HIdentifier
import Djinn.Internal.HTypes
import Djinn.Internal.LJT
import Djinn.Internal.ProofCheck (checkProof)
import Djinn.Internal.ProofEnv
import Language.Haskell.Synthesis.Constraint
    (Constraint(..), constraintArguments, constraintArity, constraintClass)
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Declaration as SharedDeclaration
import qualified Language.Haskell.Synthesis.Generated as SharedGenerated
import qualified Language.Haskell.Synthesis.Environment as SharedEnvironment
import qualified Language.Haskell.Synthesis.Inventory as SharedInventory
import qualified Language.Haskell.Synthesis.Kind as SharedKind
import qualified Language.Haskell.Synthesis.KindInference as SharedInference
import qualified Language.Haskell.Synthesis.Search as SharedSearch
import qualified Language.Haskell.Synthesis.Type as SharedType

main :: IO ()
main = defaultMain $ testGroup "Djinn unit tests" $
    [testCase name action | (name, action) <- tests]

tests :: [(String, Assertion)]
tests =
    [ ("parse prefix function constructor", testPrefixArrowParsing)
    , ("kind-check intrinsic list syntax", testIntrinsicListKind)
    , ("render canonical units and kinds", testCanonicalRendering)
    , ("round-trip shared source types", testSharedTypeAdapter)
    , ("round-trip shared declarations", testSharedDeclarationAdapter)
    , ("round-trip shared environments", testSharedEnvironmentAdapter)
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
    , ("hoist mixed wildcard and live case binders safely",
          testMixedCaseLambdaBinders)
    , ("preserve tuple payloads in unary constructors", testUnaryTuplePayload)
    , ("merge tuple refinements across case branches", testBranchRefinements)
    , ("reconstruct whole constructor payloads", testWholeConstructorPayload)
    , ("resolve instance contexts in one kind scope",
          testResolveInstanceMethods)
    , ("justify self-reference diagnostics with proof evidence",
          testSelfReferenceEvidence)
    , ("isolate external proof identities", testProofEnvironment)
    , ("type-check generated proofs independently", testGeneratedProofsCheck)
    , ("reject malformed proof terms", testMalformedProofTerms)
    , ("preserve nominal empty types", testNominalEmptyTypes)
    , ("validate declaration mutations transactionally", testEnvironmentValidation)
    , ("keep the printed value namespace unambiguous",
          testPrintedValueNamespace)
    , ("reserve unit declarations for the standard environment",
          testTrustedUnitDeclaration)
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

    -- Contexts use the shared nominal syntax while Djinn retains lookup and
    -- kind semantics behind its checked string bridge.
    let eqContext = context "Eq" [HTVar "a"]
    assertEqual "a shared context retains its nominal class"
        "Eq" (show $ constraintClass eqContext)
    assertEqual "a shared context retains its backend type arguments"
        [HTVar "a"] (constraintArguments eqContext)
    assertEqual "a shared context reports its arity" 1
        (constraintArity eqContext)
    assertEqual "a shared context has stable Haskell rendering"
        "Eq a" (show eqContext)
    assertLeft "a variable cannot name a class context"
        (mkContext "eq" [HTVar "a"])
    assertLeft "direct shared contexts still pass Djinn's class-name guard"
        (resolveContext standardEnvironment $
            Constraint (sharedName "eq") [HTVar "a"])

    -- Queries report honest outcomes.
    swap <- expectRight $ parseHType "(a, b) -> (b, a)"
    swapReport <- expectRight $
        inhabit defaultQueryOptions standardEnvironment [] "swap" swap
    assertEqual "swap is realized with the canonical clause"
        (Realized ["swap (a, b) = (b, a)"]) (reportOutcome swapReport)
    assertEqual "a completed realization reports finished exploration"
        SharedSearch.Finished (reportCompletion swapReport)
    case reportGeneratedClauses swapReport of
        [clause] -> assertEqual
            "rendered compatibility output derives from the shared clause"
            (Right "swap (a, b) = (b, a)")
            (SharedGenerated.renderFunctionClause
                (SharedGenerated.RenderOptions
                    SharedGenerated.FullyQualified id []) clause)
        clauses -> fail $ "unexpected structured swap candidates: " ++ show clauses
    peirce <- expectRight $ parseHType "((a -> b) -> a) -> a"
    peirceReport <- expectRight $
        inhabit defaultQueryOptions standardEnvironment [] "peirce" peirce
    assertEqual "Peirce's law is decided unrealizable"
        Unrealizable (reportOutcome peirceReport)
    assertEqual "logical refutation completed operationally"
        SharedSearch.Finished (reportCompletion peirceReport)
    starved <- expectRight $ inhabit
        defaultQueryOptions { optionBudget = Just 0 }
        standardEnvironment [] "peirce" peirce
    assertEqual "an expired budget is undecided, not unrealizable"
        Undecided (reportOutcome starved)
    assertEqual "budget exhaustion retains its operational reason"
        (SharedSearch.truncated SharedSearch.ChoicePointLimitReached)
        (reportCompletion starved)
    selfRef <- expectRight $ do
        environment <- declare (Function "token" (HTVar "a"))
            standardEnvironment
        inhabit defaultQueryOptions environment [] "token" (HTVar "a")
    assertEqual "a lone same-named assumption is flagged, not recursed"
        UnrealizableWithoutSelfReference (reportOutcome selfRef)

    -- Contexts resolve through inferred kinds.
    assertLeft "a kind-mismatched class argument is rejected"
        (resolveContext standardEnvironment $ context "Monad" [HTCon "Bool"])
    assertLeft "one variable cannot have inconsistent kinds across arguments" $ do
        environment <- declare
            (ClassDecl "ApplyToBool" ["f", "unused"]
                [("applyToBool", HTApp (HTVar "f") (HTCon "Bool"))])
            standardEnvironment
        resolveContext environment $ context "ApplyToBool"
            [HTVar "shared", HTVar "shared"]
    assertLeft "a context and goal must share kind assignments" $ do
        environment <- declare (ClassDecl "Value" ["a"] [])
            standardEnvironment
        let higherKinded = HTApp (HTVar "f") (HTCon "Bool")
        inhabit defaultQueryOptions environment [context "Value" [HTVar "f"]]
            "badKinds" (HTArrow higherKinded higherKinded)
    assertLeft "negative public search budgets are rejected" $
        inhabit defaultQueryOptions { optionBudget = Just (-1) }
            standardEnvironment [] "identity" (HTArrow (HTVar "a") (HTVar "a"))
    reflexive <- expectRight $ inhabit defaultQueryOptions
        standardEnvironment [context "Eq" [HTVar "a"]] "reflexive"
        (HTArrow (HTVar "a") (HTCon "Bool"))
    case reportOutcome reflexive of
        Realized (best : _) ->
            assertEqual "an Eq context supplies its method, ranked first"
                "reflexive a = a == a" best
        other -> fail $ "reflexive was not realized: " ++ show other

expectRight :: Either String a -> IO a
expectRight = either fail return

context :: HSymbol -> [HType] -> Context
context className arguments =
    case mkContext className arguments of
        Left message -> error $
            "invalid context in Djinn regression suite: " ++ message
        Right result -> result

sharedName :: String -> SharedName.Name
sharedName source =
    case SharedName.parseName source of
        Left nameError -> error $
            "invalid shared name in Djinn regression suite: " ++
            SharedName.renderNameError nameError
        Right result -> result

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

testSharedTypeAdapter :: IO ()
testSharedTypeAdapter = do
    source <- expectRight $ parseHType "(a, [b]) -> Maybe a"
    shared <- either (fail . show) pure $ toSynthesisType source
    assertEqual "the shared type satisfies its own invariants"
        (Right ()) (SharedType.validateType shared)
    assertEqual "Djinn's source-type subset round-trips losslessly"
        (Right source) (fromSynthesisType shared)
    unit <- either (fail . show) pure $ toSynthesisType $ HTCon "()"
    assertEqual "unit is the structural nullary tuple"
        (SharedType.TupleType SharedName.Boxed []) unit
    assertEqual "unit returns to Djinn's canonical constructor form"
        (Right $ HTCon "()") (fromSynthesisType unit)
    assertEqual "declaration bodies cannot masquerade as source types"
        (Left $ DeclarationBodyIsNotSourceType $ HTUnion [])
        (toSynthesisType $ HTUnion [])
    assertEqual "explicit foralls remain outside Djinn's supported subset"
        (Left SynthesisForallUnsupported)
        (fromSynthesisType $ SharedType.ForallType ["a"] []
            $ SharedType.TypeVariable "a")

testSharedDeclarationAdapter :: IO ()
testSharedDeclarationAdapter = do
    let declarations =
            [ TypeSynonym "Identity" ["a"] (HTVar "a")
            , DataType "Maybe2" ["a"]
                [("Nothing2", []), ("Just2", [HTVar "a"])]
            , AbstractType "HK" $ KArrow (KVar 3) KStar
            , ClassDecl "Comparable" ["a"]
                [("compareTo", HTArrow (HTVar "a") (HTVar "a"))]
            , Function "M.value" $ HTCon "()"
            ]
    mapM_ assertRoundTrip declarations
    let kind = KArrow (KVar 4) (KArrow KStar KStar)
    assertEqual "kind conversion is lossless"
        kind (fromSynthesisKind $ toSynthesisKind kind)
    let parameter = SharedDeclaration.TypeParameter "a" Nothing
        superclass = Constraint (sharedName "Comparable")
            [SharedType.TypeVariable "a"]
        sharedClass = SharedDeclaration.ClassDeclaration ()
            (sharedName "Comparable") [parameter] [superclass] []
    assertEqual "Djinn lowering rejects unsupported superclass semantics"
        (Left ClassSuperclassesUnsupported)
        (fromSynthesisDeclaration sharedClass)
    assertEqual "shared validation catches a function in the type namespace"
        (Left $ InvalidSharedDeclaration
            $ SharedDeclaration.InvalidDeclaredValueName $ sharedName "T")
        (toSynthesisDeclaration $ Function "T" $ HTCon "()")
  where
    assertRoundTrip declaration = do
        shared <- either (fail . show) pure
            $ toSynthesisDeclaration declaration
        lowered <- either (fail . show) pure
            $ fromSynthesisDeclaration shared
        assertEqual "Djinn declaration round-trip changed its compatibility view"
            (show declaration) (show lowered)

testSharedEnvironmentAdapter :: IO ()
testSharedEnvironmentAdapter = do
    withFirstFunction <- expectRight $ declare
        (Function "firstAssumption" $ HTCon "()") standardEnvironment
    orderedEnvironment <- expectRight $ declare
        (Function "secondAssumption" $ HTCon "()") withFirstFunction
    shared <- either (fail . show) pure
        $ toSynthesisEnvironment orderedEnvironment
    inventory <- either (fail . show) pure
        $ toSynthesisInventory orderedEnvironment
    assertEqual "inventory and compatibility projection disagree"
        (Map.keys $ SharedEnvironment.typeDeclarationMap shared)
        (Map.keys $ SharedEnvironment.typeDeclarationMap
            $ SharedInventory.inventoryEnvironment inventory)
    assertEqual "inventory lost Maybe's inferred kind"
        (Just $ SharedKind.FunctionKind
            SharedKind.ProperTypeKind SharedKind.ProperTypeKind)
        (Map.lookup (sharedName "Maybe")
            $ SharedInference.typeConstructorKinds
            $ SharedInventory.inventoryKindAssumptions inventory)
    markerEnvironment <- expectRight $ declare
        (ClassDecl "Marker" ["a"] []) emptyEnvironment
    markerInventory <- either (fail . show) pure
        $ toSynthesisInventory markerEnvironment
    assertEqual "inventory generalized Djinn's defaulted class kind"
        (Just [Just SharedKind.ProperTypeKind])
        (Map.lookup (sharedName "Marker")
            $ SharedInference.classParameterKinds
            $ SharedInventory.inventoryKindAssumptions markerInventory)
    assertBool "shared standard environment lost unit"
        $ Map.member (sharedName "()")
        $ SharedEnvironment.typeDeclarationMap shared
    assertBool "shared standard environment lost Eq"
        $ Map.member (sharedName "Eq")
        $ SharedEnvironment.classDeclarationMap shared
    lowered <- either (fail . show) pure $ fromSynthesisEnvironment shared
    assertEqual "type order changed through the shared environment"
        (map fst $ typeDeclarations orderedEnvironment)
        (map fst $ typeDeclarations lowered)
    assertEqual "function order changed through the shared environment"
        (map fst $ functionDeclarations orderedEnvironment)
        (map fst $ functionDeclarations lowered)
    assertEqual "class order changed through the shared environment"
        (map fst $ classDeclarations orderedEnvironment)
        (map fst $ classDeclarations lowered)
    let sharedInstance = SharedDeclaration.InstanceDeclaration () ["a"] []
            $ Constraint (sharedName "Eq") [SharedType.TypeVariable "a"]
    instanceEnvironment <- either (fail . show) pure
        $ SharedEnvironment.mkEnvironment [sharedInstance]
    assertEqual "Djinn does not silently install shared instances"
        (Left $ SynthesisEnvironmentDeclarationError
            InstanceDeclarationUnsupported)
        (fromSynthesisEnvironment instanceEnvironment)

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
    assertEqual "method-local variables have independent kind scopes"
        (Right [("a", KStar)])
        (htInferClassKinds checked ["a"]
            [HTApp (HTVar "f") (HTVar "a"), HTVar "f"])
    assertLeft "one method still shares repeated occurrences of its local" $
        htInferClassKinds checked ["a"]
            [HTTuple [HTApp (HTVar "f") (HTVar "a"), HTVar "f"]]
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
    assertEqual "an unlimited search has no finite fuel remainder"
        Nothing (remainingSearchBudget complete)

    -- A zero budget cannot decide anything that branches.
    let starved = run unlimited { searchBudget = Just 0 } peirce
    assertEqual "a starved search finds nothing" [] (searchProofs starved)
    assertBool "an expired budget must be reported" $ searchExhausted starved
    assertEqual "an expired search has no fuel left"
        (Just 0) (remainingSearchBudget starved)

    -- Internal callers can construct SearchMode directly.  Treat a negative
    -- budget as already expired rather than accidentally making it unlimited.
    let invalidBudget = run unlimited { searchBudget = Just (-1) } peirce
    assertEqual "a negative internal budget finds nothing"
        [] (searchProofs invalidBudget)
    assertBool "a negative internal budget must be reported as expired" $
        searchExhausted invalidBudget
    assertEqual "expired negative fuel is normalized to zero"
        (Just 0) (remainingSearchBudget invalidBudget)
    let immediateWithNegativeFuel =
            run unlimited { searchBudget = Just (-1) } (Conj [])
    assertEqual "a zero-step proof remains available at zero normalized fuel"
        [Ctuple 0] (searchProofs immediateWithNegativeFuel)
    assertBool "a zero-step proof does not exhaust normalized fuel" $
        not (searchExhausted immediateWithNegativeFuel)
    assertEqual "even a finished zero-step search reports non-negative fuel"
        (Just 0) (remainingSearchBudget immediateWithNegativeFuel)

    -- A complete refutation reports exactly the fuel that a diagnostic
    -- follow-up may spend without exceeding the original query budget.
    let atomicRefutation =
            run unlimited { searchBudget = Just 1 } atomA
    assertEqual "one choice point suffices to refute an unconstrained atom"
        [] (searchProofs atomicRefutation)
    assertBool "the one-step atomic refutation is complete" $
        not (searchExhausted atomicRefutation)
    assertEqual "the atomic refutation spends its complete budget"
        (Just 0) (remainingSearchBudget atomicRefutation)

    -- A bounded depth-first search yields a prefix of the unbounded stream.
    let full = searchProofs $ run unlimited pick3
        bounded = run unlimited { searchBudget = Just 1 } pick3
    assertBool "bounded proofs must be a prefix of the unbounded stream" $
        searchProofs bounded `isPrefixOf` full
    assertBool "the pick-3 space is larger than one step" $
        searchExhausted bounded
    assertEqual "bounded alternative search consumes all available fuel"
        (Just 0) (remainingSearchBudget bounded)

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

-- Case cleanup commutes a lambda shared by every alternative out of the case.
-- The first branch below discards that argument while the second uses it.
-- Choosing the first branch's wildcard as the common name used to substitute
-- the live occurrence with @_@ and make this valid proof unrenderable.
testMixedCaseLambdaBinders :: IO ()
testMixedCaseLambdaBinders = do
    let choice = Symbol "choice"
        ignored = Symbol "ignored"
        used = Symbol "used"
        argument = Symbol "argument"
        live = Symbol "live"
        nothing = ConsDesc "Nothing" 0
        just = ConsDesc "Just" 1
        constructors = [nothing, just]
        term = Lam choice $ applys (Ccases constructors)
            [ Var choice
            , Lam ignored $ Lam argument $ Ctuple 0
            , Lam used $ Lam live $ Apply (Var used) (Var live)
            ]
        formula = Disj
            [ (nothing, true)
            , (just, atomA :-> true)
            ] :-> atomA :-> true
        expected = "minimal a b =\n" ++
            "  case a of\n" ++
            "  Nothing -> ()\n" ++
            "  Just c -> c b"
    assertRight "the mixed-binder term must independently type-check" $
        checkProof [] formula term
    assertRendered "a live branch binder must not become a typed hole"
        expected "minimal" term

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
            "  case a of\n" ++
            "  C b c -> (b, c)"
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

testResolveInstanceMethods :: IO ()
testResolveInstanceMethods = do
    let parameter = HTVar "f"
        local = HTVar "a"
        applied = HTApp parameter local
        valueClass = ClassDecl "Value" ["a"] []
        higherClass = ClassDecl "Higher" ["f"]
            [("use", HTArrow applied applied)]
    environment <- expectRight $ do
        withValue <- declare valueClass standardEnvironment
        declare higherClass withValue

    let target = context "Higher" [HTCon "Maybe"]
    assertEqual "joint resolution preserves exact target method substitution"
        (resolveContext environment target)
        (resolveInstanceMethods environment [] target)

    assertLeftContains
        "an instance head and its prerequisites share kind variables"
        "argument f of class Higher" $
        resolveInstanceMethods environment
            [context "Value" [HTVar "f"], context "Higher" [HTVar "f"]]
            (context "Value" [HTVar "x"])

    -- A method-local variable with the same spelling as an instance argument
    -- must not be captured by class-parameter substitution.
    captureEnvironment <- expectRight $
        declare (ClassDecl "Capture" ["a"]
            [("capture", HTApp (HTVar "f") (HTVar "a"))]) environment
    let capture = context "Capture" [HTVar "f"]
    assertEqual "context instantiation alpha-renames a captured method local"
        (Right [("capture", HTApp (HTVar "f'") (HTVar "f"))])
        (resolveContext captureEnvironment capture)
    safe <- expectRight $ inhabit defaultQueryOptions captureEnvironment
        [capture] "safe" (HTArrow (HTVar "x") (HTVar "x"))
    assertEqual "an unused capture-safe context does not poison a query"
        (Realized ["safe a = a"]) (reportOutcome safe)

    -- Identical local spellings in different signatures denote different
    -- implicit quantifiers and can therefore have different kinds.
    independentEnvironment <- expectRight $
        declare (ClassDecl "Independent" ["a"]
            [ ("left", HTApp (HTVar "f") (HTVar "a"))
            , ("right", HTVar "f")
            ]) captureEnvironment
    assertEqual "instantiated sibling methods retain independent local scopes"
        (Right
            [ ("left", HTApp (HTVar "f") (HTCon "Bool"))
            , ("right", HTVar "f")
            ])
        (resolveContext independentEnvironment $
            context "Independent" [HTCon "Bool"])

    -- A colliding image for a parameter absent from this method performs no
    -- substitution and therefore must not rename a useful shallow local.
    inactiveEnvironment <- expectRight $
        declare (ClassDecl "Inactive" ["a", "b"]
            [("inactive", HTApp (HTVar "f") (HTVar "b"))])
            independentEnvironment
    assertEqual "inactive substitution images do not trigger alpha-renaming"
        (Right [("inactive", HTApp (HTVar "f") (HTCon "Bool"))])
        (resolveContext inactiveEnvironment $
            context "Inactive" [HTVar "f", HTCon "Bool"])

    -- Fresh allocation must avoid both existing primes and every name in a
    -- compound substitution image while renaming multiple locals at once.
    multiCaptureEnvironment <- expectRight $
        declare (ClassDecl "MultiCapture" ["a"]
            [("multiCapture", HTTuple
                [ HTApp (HTVar "f") (HTVar "a")
                , HTApp (HTVar "f'") (HTVar "a")
                ])]) inactiveEnvironment
    let compoundArgument = HTTuple [HTVar "f", HTVar "f'"]
    assertEqual "multiple captured locals receive distinct fresh primes"
        (Right [("multiCapture", HTTuple
            [ HTApp (HTVar "f''") compoundArgument
            , HTApp (HTVar "f'''") compoundArgument
            ])])
        (resolveContext multiCaptureEnvironment $
            context "MultiCapture" [compoundArgument])

testSelfReferenceEvidence :: IO ()
testSelfReferenceEvidence = do
    let a = HTVar "a"
        b = HTVar "b"

    unrelatedEnvironment <- expectRight $
        declare (Function "token" $ HTCon "Bool") standardEnvironment
    unrelated <- expectRight $
        inhabit defaultQueryOptions unrelatedEnvironment [] "token"
            (HTArrow a b)
    assertEqual "an unrelated collision does not justify a recursion warning"
        Unrealizable (reportOutcome unrelated)

    combinedEnvironment <- expectRight $ do
        withSeed <- declare (Function "seed" a) standardEnvironment
        declare (Function "token" $ HTArrow a b) withSeed
    combined <- expectRight $
        inhabit defaultQueryOptions combinedEnvironment [] "token" b
    assertEqual
        "a proof needing both safe and excluded assumptions justifies the warning"
        UnrealizableWithoutSelfReference (reportOutcome combined)

    exactEnvironment <- expectRight $
        declare (Function "token" a) standardEnvironment
    exact <- expectRight $
        inhabit defaultQueryOptions exactEnvironment [] "token" a
    assertEqual "an exact target assumption still justifies the warning"
        UnrealizableWithoutSelfReference (reportOutcome exact)

    diagnosticHeavyEnvironment <- expectRight $
        declare (Function "token" $ HTArrow (HTArrow a b) a)
            standardEnvironment
    diagnosticHeavy <- expectRight $
        inhabit defaultQueryOptions { optionBudget = Just 1 }
            diagnosticHeavyEnvironment [] "token" b
    assertEqual
        "diagnostic fuel exhaustion cannot undo a completed safe refutation"
        Unrealizable (reportOutcome diagnosticHeavy)

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
        allBindings = proofBindingsIncludingTarget environment
    assertBool "a target-named assumption must be excluded"
        (targetWasExcluded environment)
    assertEqual "only the unsafe target binding should be removed"
        [atomA, atomB] (map snd bindings)
    assertEqual "every remaining assumption needs a unique proof identity"
        2 (length $ nub $ map fst bindings)
    assertEqual "the diagnostic environment retains every assumption"
        [atomA, atomA, atomB] (map snd allBindings)
    assertEqual "excluded assumptions also receive unique proof identities"
        3 (length $ nub $ map fst allBindings)
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

testPrintedValueNamespace :: IO ()
testPrintedValueNamespace = do
    let bool = HTCon "Bool"
        selectable = ClassDecl "Selectable" ["a"]
            [("select", HTVar "a")]
        conflict =
            "Function assumption select conflicts with method select " ++
            "of class Selectable"

    functionFirst <- expectRight $
        declare (Function "select" bool) standardEnvironment
    assertLeftMessage "a later class method must not shadow an assumption"
        conflict (declare selectable functionFirst)

    classFirst <- expectRight $ declare selectable standardEnvironment
    assertLeftMessage "a later assumption must not shadow a class selector"
        conflict (declare (Function "select" bool) classFirst)
    assertLeftMessage "operator selectors share the same printed namespace"
        "Function assumption (==) conflicts with method (==) of class Eq"
        (declare (Function "==" bool) standardEnvironment)

    -- Qualified references retain their qualification in generated code and
    -- therefore cannot shadow an unqualified selector with the same suffix.
    qualifiedFirst <- expectRight $
        declare (Function "External.select" bool) standardEnvironment
    qualifiedWithClass <- expectRight $ declare selectable qualifiedFirst
    assertEqual "the qualified assumption remains alongside the selector"
        (Just bool)
        (lookup "External.select" $ functionDeclarations qualifiedWithClass)
    assertBool "the class declaration is retained too" $
        "Selectable" `elem` map fst (classDeclarations qualifiedWithClass)
    _ <- expectRight $ declare (Function "External.select" bool) classFirst

    -- Removing the owning declaration releases the spelling, while failed
    -- candidates leave their immutable input environment untouched.
    withoutFunction <- expectRight $ removeDeclaration "select" functionFirst
    _ <- expectRight $ declare selectable withoutFunction
    withoutClass <- expectRight $ removeDeclaration "Selectable" classFirst
    _ <- expectRight $ declare (Function "select" bool) withoutClass

    -- The internal rebuilding boundary owns the invariant as well; it is not
    -- merely an ad-hoc check in the two public declaration branches.
    assertLeftMessage "raw environment validation rejects the same ambiguity"
        conflict
        (validateEnvironment [] [("select", HTVar "a")]
            [("Selectable", ([("a", KStar)], [("select", HTVar "a")]))])
    assertLeftContains "raw validation rejects duplicate assumptions"
        "Duplicate function assumption: duplicate"
        (validateEnvironment []
            [("duplicate", bool), ("duplicate", bool)] [])
    assertLeftContains "raw validation rejects duplicate class owners"
        "Duplicate class: Selectable"
        (validateEnvironment [] []
            [ ("Selectable", ([("a", KStar)], []))
            , ("Selectable", ([("a", KStar)], []))
            ])

testTrustedUnitDeclaration :: IO ()
testTrustedUnitDeclaration = do
    let exactUnit = ([], HTUnion [("()", [])], KStar)
    assertEqual "the standard environment contains exactly the wired-in unit"
        (Just exactUnit) (lookup "()" $ typeDeclarations standardEnvironment)

    unitType <- expectRight $ parseHType "()"
    report <- expectRight $
        inhabit defaultQueryOptions standardEnvironment [] "unitValue" unitType
    assertEqual "the trusted constructor still realizes the unit type"
        (Realized ["unitValue = ()"]) (reportOutcome report)

    -- The spelling remains valid inside ordinary type expressions.
    assertRight "a synonym body may refer to the built-in unit" $
        declare (TypeSynonym "UnitAlias" [] unitType) standardEnvironment
    assertRight "a constructor field may have the built-in unit type" $
        declare (DataType "CarriesUnit" [] [("CarriesUnit", [unitType])])
            standardEnvironment

    -- It is not, however, a user-definable ConId.  In particular, the exact
    -- standard declaration is private rather than a loophole in this rule.
    assertLeftContains "a unit-named synonym is rejected"
        "not a valid type constructor name"
        (declare (TypeSynonym "()" [] $ HTCon "Bool") standardEnvironment)
    assertLeftContains "a unit-named abstract type is rejected"
        "not a valid type constructor name"
        (declare (AbstractType "()" KStar) standardEnvironment)
    assertLeftContains "a unit-named data type is rejected"
        "not a valid type constructor name"
        (declare (DataType "()" [] []) standardEnvironment)
    assertLeftContains "even the exact built-in data declaration is private"
        "not a valid data constructor name"
        (declare (DataType "()" [] [("()", [])]) emptyEnvironment)
    assertLeftContains "a unit-named class is rejected"
        "not a valid class name"
        (declare (ClassDecl "()" [] []) standardEnvironment)
    assertLeftContains "the unit constructor cannot belong to another type"
        "not a valid data constructor name"
        (declare (DataType "CounterfeitUnit" [] [("()", [])])
            standardEnvironment)

    assertLeftMessage "the trusted unit cannot be deleted and recreated"
        "() is a built-in type and cannot be removed"
        (removeDeclaration "()" standardEnvironment)

testScopeSafeRendering :: IO ()
testScopeSafeRendering = do
    let shadowed = Symbol "x"
        term = Lam shadowed $
            Apply (Var shadowed) (Lam shadowed $ Var shadowed)
    rendered <- renderTerm "applyIdentity" term
    assertEqual "nested shadowing binders should receive distinct names"
        "applyIdentity a = a (\\b -> b)" rendered
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
    assertParses "Unicode lowercase letters are variable identifiers"
        pVarId "λvalue"
    assertParses "Unicode uppercase letters are constructor identifiers"
        pConId "Δelta"
    assertParses "qualified external variables are valid"
        pQualifiedVarId "Data.Function.id"
    assertParses "qualified Unicode variables are valid"
        pQualifiedVarId "Data.λvalue"
    assertParses "qualified constructors are valid"
        pQualifiedConId "Data.Maybe.Maybe"
    assertBool "identifier lexical classes remain distinct"
        (isVarId "value" && not (isConId "value") &&
         isConId "Value" && not (isVarId "Value"))
    assertBool "qualified constructor lexical classes remain distinct"
        (isQualifiedConId "Data.Maybe.Just" &&
         not (isQualifiedVarId "Data.Maybe.Just"))
    assertParses "the full ASCII operator alphabet is available"
        pParenthesizedVarOp "(/?)"
    assertParses "Unicode Haskell operators are available"
        pParenthesizedVarOp "(⊕)"
    assertBool "Unicode operators retain their variable lexical class"
        (isVarOperator "⊕" && renderVarName "⊕" == "(⊕)")
    assertDoesNotParse "constructor operators are not term binding names"
        pParenthesizedVarOp "(:+:)"
    assertBool "constructor operators remain outside the variable namespace"
        (not (isVarOperator ":+:") && renderVarName ":+:" == ":+:")
    assertDoesNotParse "reserved words are not identifiers" pVarId "case"
    assertDoesNotParse "a bare underscore is not a binding name" pVarId "_"
    assertBool "the complete shared reserved-word policy reaches Djinn"
        (all (not . isVarId) ["_", "as", "case", "module", "where"])
    assertDoesNotParse "module segments must be constructors"
        pQualifiedVarId "data.module.value"
    assertDoesNotParse "lowercase intermediate module segments are invalid"
        pQualifiedVarId "Data.internal.value"
    assertDoesNotParse "leading qualification dots are invalid"
        pQualifiedVarId ".Data.value"
    assertDoesNotParse "empty qualification segments are invalid"
        pQualifiedVarId "Data..value"
    assertDoesNotParse "trailing qualification dots are invalid"
        pQualifiedVarId "Data.value."
    assertBool "classification does not inherit parseName whitespace trimming"
        (not $ isQualifiedVarId " Data.value")
    assertParses "ReadP still owns and consumes leading whitespace"
        pQualifiedVarId " Data.value"
    assertDoesNotParse "qualified operators remain outside Djinn's grammar"
        pQualifiedVarId "Data.(+)"
    assertBool "qualified operators are not reclassified as identifiers"
        (not (isQualifiedVarId "Data.(+)") &&
         renderVarName "Data.(+)" == "Data.(+)")
    assertDoesNotParse "reserved operators are invalid binding names"
        pParenthesizedVarOp "(->)"
    assertDoesNotParse "a line-comment introducer is not an operator name"
        pParenthesizedVarOp "(--)"
    assertBool "the complete shared reserved-operator policy reaches Djinn"
        (all (not . isVarOperator)
            ["..", "--", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"])
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
    assertEqual "Unicode symbols also continue dash-prefixed operators"
        "(--⊕) :: a -> a\n"
        (stripLineComments "(--⊕) :: a -> a\n")

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

assertLeftMessage :: Show a => String -> String -> Either String a -> IO ()
assertLeftMessage message expected result =
    case result of
        Left actual -> assertEqual message expected actual
        Right value ->
            fail $ message ++ ": expected an error, got " ++ show value

assertLeftContains :: Show a => String -> String -> Either String a -> IO ()
assertLeftContains message expectedFragment result =
    case result of
        Left actual -> assertContains message expectedFragment actual
        Right value ->
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
        Right clause -> either
            (fail . ("shared clause rendering failed: " ++))
            return
            (hPrClause clause)

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
