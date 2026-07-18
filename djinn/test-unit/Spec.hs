module Main (main) where

import Data.List (isInfixOf, isPrefixOf, isSuffixOf, nub, sort)
import qualified Data.Map.Strict as Map
import Data.Void (Void, absurd)
import Numeric.Natural (Natural)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Text.ParserCombinators.ReadP (ReadP, eof, readP_to_S, skipSpaces)
import Text.Read (readMaybe)

import Djinn.Core (
    Context, Declaration(..), DjinnCandidateDetails(..),
    DjinnQueryMetadata(..),
    DjinnDeclarationNameRole(..), QueryOutcome(..),
    DjinnQueryError(..), DjinnQueryOptionsError(..),
    SynthesisDeclarationError(..), SynthesisEnvironmentError(..),
    SynthesisTypeError(..),
    classDeclarations, declare, defaultQueryOptions, emptyEnvironment,
    functionDeclarations, generatedReportCandidates,
    generatedReportCompletion, inhabit, inhabitGenerated, inhabitResult,
    inhabitGeneratedPrepared, inhabitResultPrepared,
    inhabitSynthesisResultPrepared,
    kArrow, kStar, optionAlternatives, optionBudget, optionCutoff, optionSorted,
    fromSynthesisDeclaration, fromSynthesisEnvironment,
    fromSynthesisKind, fromSynthesisType,
    mkContext, parseContextualHType, parseHKind, parseHType,
    prepareEnvironment, removeDeclaration,
    reportCompletion, reportGeneratedClauses, reportOutcome,
    resolveContext, resolveInstanceMethods, resolvePreparedContext,
    standardEnvironment, toSynthesisDeclaration, toSynthesisEnvironment,
    toSynthesisInventory,
    toSynthesisKind,
    toSynthesisType, typeDeclarations)
import Djinn.Internal.Environment (validateEnvironment)
import qualified Djinn.Internal.Environment as RawEnvironment
import qualified Djinn.Internal.Generated as DjinnGenerated
import Djinn.Internal.HCheck (
    htCheckEnv, htCheckType, htCheckTypeKind, htCheckTypesKinds,
    htInferClassKinds)
import Djinn.Internal.HIdentifier
import Djinn.Internal.HTypes hiding (fromSynthesisKind, toSynthesisKind)
import Djinn.Internal.LJT
import Djinn.Internal.ProofCheck (checkProof)
import Djinn.Internal.ProofEnv
import HCheckCompatibility (hCheckCompatibilityTests)
import HKindCompatibility (hKindCompatibilityTests)
import HTypeCompatibility (hTypeCompatibilityTests)
import qualified Language.Haskell.Djex.Djinn as Djex
import Language.Haskell.Synthesis.Constraint
    (Constraint(..), constraintArguments, constraintArity, constraintClass)
import qualified Language.Haskell.Synthesis.Candidate as SharedCandidate
import qualified Language.Haskell.Synthesis.Diagnostic as SharedDiagnostic
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Declaration as SharedDeclaration
import qualified Language.Haskell.Synthesis.Generated as SharedGenerated
import qualified Language.Haskell.Synthesis.Environment as SharedEnvironment
import qualified Language.Haskell.Synthesis.Inventory as SharedInventory
import qualified Language.Haskell.Synthesis.Kind as SharedKind
import qualified Language.Haskell.Synthesis.KindInference as SharedInference
import qualified Language.Haskell.Synthesis.Query as SharedQuery
import qualified Language.Haskell.Synthesis.Search as SharedSearch
import qualified Language.Haskell.Synthesis.Type as SharedType
import qualified Language.Haskell.Synthesis.TypeRender as SharedTypeRender
import qualified Language.Haskell.Synthesis.TypeSynonym as SharedTypeSynonym

main :: IO ()
main = defaultMain $ testGroup "Djinn unit tests" $
    [testCase name action | (name, action) <- tests]

tests :: [(String, Assertion)]
tests =
    hKindCompatibilityTests ++
    hTypeCompatibilityTests ++
    hCheckCompatibilityTests ++
    [ ("parse prefix function constructor", testPrefixArrowParsing)
    , ("parse and render through the checked Djinn adapter",
          testCheckedDjinnAdapter)
    , ("reuse canonical shared Djinn request plans across sessions",
          testCheckedDjinnRequestReuse)
    , ("edit checked Djinn sessions through the shared environment",
          testCheckedDjinnSessionEditing)
    , ("invalidate prepared Djinn caches transactionally",
          testCheckedDjinnCacheInvalidation)
    , ("agree between ground and weakened edit transactions",
          testGroundEditAgreement)
    , ("kind-check intrinsic list syntax", testIntrinsicListKind)
    , ("render canonical units and kinds", testCanonicalRendering)
    , ("round-trip shared source types", testSharedTypeAdapter)
    , ("round-trip shared declarations", testSharedDeclarationAdapter)
    , ("round-trip shared environments", testSharedEnvironmentAdapter)
    , ("prepare neutral Djinn environments authoritatively",
          testNeutralDjinnPreparation)
    , ("cache prepared global premises in declaration order",
          testPreparedFunctionPremises)
    , ("compile prepared raw and shared types to identical formulas",
          testPreparedFormulaParity)
    , ("project raw Djinn kinds from the authoritative inventory",
          testRawDjinnPreparationKinds)
    , ("bound malformed Djinn context arity observation",
          testBoundedContextArity)
    , ("normalize aliases inside opaque formula atoms", testOpaqueAliasAtoms)
    , ("reject every raw recursive type-expansion graph finitely",
          testRawTypeExpansionCycles)
    , ("preserve raw expansion substitution semantics",
          testRawExpansionSubstitution)
    , ("preflight raw environment recursion after alias expansion",
          testRawEnvironmentRecursionPreflight)
    , ("validate unused synonyms before recursion preflight",
          testPreparedSynonymValidationOwnership)
    , ("preserve raw operational synonym errors",
          testOperationalSynonymCompatibilityError)
    , ("separate synonym saturation from kind errors",
          testSynonymSaturationBoundary)
    , ("elaborate prepared query synonyms without changing compatibility views",
          testPreparedQuerySynonyms)
    , ("infer and reuse a higher-kinded synonym", testHigherKindedGrounding)
    , ("reject an ill-kinded higher-kinded application", testIllKindedApplication)
    , ("reject a higher-kinded synonym body", testHigherKindedSynonymBody)
    , ("infer and enforce class parameter kinds", testClassParameterKinds)
    , ("prove intuitionistic tautologies", testProvableBasics)
    , ("prove empty goals from contradictions", testEmptyGoalContradiction)
    , ("reject non-theorems", testNonTheorems)
    , ("honor search budgets and strategies", testSearchModes)
    , ("use an assumption as its named proof", testNamedAssumption)
    , ("reject ambiguous raw proof environments",
          testCheckedProofSearchEnvironment)
    , ("do not capture a caller-supplied proof symbol", testCallerSymbolCapture)
    , ("keep disjunction continuation atoms fresh", testContinuationAtomCapture)
    , ("preserve residual application after Csplit", testCsplitResidualArguments)
    , ("preserve residual handler lambdas after Csplit",
          testCsplitResidualHandlerLambda)
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
    , ("allocate wide proof metavariable plans without reuse",
          testWideProofMetas)
    , ("reject malformed proof terms", testMalformedProofTerms)
    , ("preserve nominal empty types", testNominalEmptyTypes)
    , ("validate declaration mutations transactionally", testEnvironmentValidation)
    , ("keep the printed value namespace unambiguous",
          testPrintedValueNamespace)
    , ("reserve unit declarations for the standard environment",
          testTrustedUnitDeclaration)
    , ("render shadowing terms without capture", testScopeSafeRendering)
    , ("report malformed proof rendering", testMalformedRendering)
    , ("validate generated clauses at conversion", testGeneratedClauseBoundary)
    , ("respect declaration keyword token boundaries", testKeywordTokens)
    , ("accept only Haskell identifiers and operators", testIdentifiers)
    , ("validate every boundary of the Djinn.Core facade", testCoreFacade)
    ]

testCheckedDjinnAdapter :: IO ()
testCheckedDjinnAdapter = do
    session <- expectShownRight Djex.standardDjinnSession
    target <- expectShownRight $ SharedName.mkIdentifier "identity"
    request <- expectShownRight $ Djex.parseDjinnRequest
        session defaultQueryOptions target "identity-query.djinn"
        "Eq a => a -> a"
    let expectedGoal = SharedType.FunctionType
            (SharedType.TypeVariable "a")
            (SharedType.TypeVariable "a")
        expectedContexts =
            [Constraint (sharedName "Eq") [SharedType.TypeVariable "a"]]
        parsedQuery = Djex.djinnRequestQuery request
    SharedGenerated.definitionName (SharedQuery.requestTarget parsedQuery)
        `assertEqualReversed` target
    SharedQuery.requestGoal parsedQuery `assertEqualReversed` expectedGoal
    SharedQuery.requestContexts parsedQuery `assertEqualReversed`
        expectedContexts
    SharedQuery.requestOptions parsedQuery `assertEqualReversed`
        defaultQueryOptions

    -- Programmatic callers cross the same checked boundary as the parser;
    -- the opaque request preserves the shared query losslessly once sealed.
    checkedTarget <- expectShownRight $
        SharedGenerated.mkDefinitionName target
    checkedRequest <- expectShownRight $ Djex.parseDjinnRequestWithCheckedTarget
        session defaultQueryOptions checkedTarget "identity-query.djinn"
        "Eq a => a -> a"
    assertEqual "raw and checked-target Djinn parsers agree"
        request checkedRequest
    let programmaticQuery = SharedQuery.QueryRequest
            { SharedQuery.requestTarget = checkedTarget
            , SharedQuery.requestGoal = expectedGoal
            , SharedQuery.requestContexts = expectedContexts
            , SharedQuery.requestOptions = defaultQueryOptions
            }
    programmaticRequest <- expectShownRight $
        Djex.mkDjinnRequest programmaticQuery
    assertEqual "programmatic request round-trip"
        programmaticQuery (Djex.djinnRequestQuery programmaticRequest)
    assertEqual "request provenance does not affect equality"
        request programmaticRequest
    assertEqual "request provenance does not affect display"
        (show programmaticRequest) (show request)

    result <- expectShownRight $
        Djex.runDjinnQuery session programmaticRequest
    case SharedSearch.batchCandidates $ SharedQuery.resultSearch result of
        candidate : _ -> do
            assertEqual "Djinn candidates expose shared residual types"
                [] (SharedCandidate.candidateResidualConstraints candidate)
            assertEqual "the adapter renders a complete top-level definition"
                (Right "identity a = a")
                (Djex.renderDjinnCandidateDefinition
                    Djex.FullyQualified candidate)
            assertEqual "expression rendering reconstructs clause lambdas"
                (Right "\\a -> a")
                (Djex.renderDjinnCandidateExpression
                    Djex.FullyQualified candidate)
        [] -> fail "the checked Djinn adapter found no identity candidate"

    -- The private execution plan is canonical, but the public request remains
    -- the exact shared value supplied by its caller. In particular, sealing a
    -- saturated prefix arrow must not rewrite djinnRequestQuery even though
    -- the proof core consumes the structural FunctionType form.
    let noncanonicalGoal = SharedType.TypeApplication
            (SharedType.TypeApplication
                (SharedType.TypeConstructor SharedName.functionName)
                (SharedType.TypeVariable "a"))
            (SharedType.TypeVariable "a")
        noncanonicalQuery = programmaticQuery
            { SharedQuery.requestGoal = noncanonicalGoal
            , SharedQuery.requestContexts = []
            }
    noncanonicalRequest <- expectShownRight $
        Djex.mkDjinnRequest noncanonicalQuery
    assertEqual "request projection rewrote a noncanonical shared type"
        noncanonicalQuery (Djex.djinnRequestQuery noncanonicalRequest)
    nativeResult <- expectShownRight $
        Djex.runDjinnQuery session noncanonicalRequest
    prepared <- expectShownRight $ prepareEnvironment standardEnvironment
    rawResult <- expectShownRight $
        inhabitResultPrepared defaultQueryOptions prepared [] checkedTarget
            (HTArrow (HTVar "a") (HTVar "a"))
    nativeCoreResult <- expectShownRight $
        inhabitSynthesisResultPrepared defaultQueryOptions prepared []
            checkedTarget noncanonicalGoal
    assertEqual "native and raw prepared core query paths diverged"
        rawResult nativeCoreResult
    assertEqual "the stable adapter rebuilt the native core result"
        nativeCoreResult nativeResult

    let malformedSource = "Eq a => a -> a ;"
    case Djex.parseDjinnRequestWithCheckedTarget
            session defaultQueryOptions checkedTarget
            "malformed-query.djinn" malformedSource of
        Left failure -> do
            assertEqual "parse failures have a stable diagnostic code"
                (Just "DJEX_DJINN_PARSE")
                (SharedDiagnostic.diagnosticCode failure)
            assertEqual "parse failures retain their caller-supplied source"
                (Just "malformed-query.djinn")
                (SharedDiagnostic.diagnosticSource failure)
            assertEqual "parse failures retain their complete input span"
                (Just $ SharedDiagnostic.sourceTextSpan malformedSource)
                (SharedDiagnostic.diagnosticSpan failure)
        Right _ -> fail "the checked Djinn parser accepted trailing input"

    let unsupportedGoal = SharedType.TupleType SharedName.Unboxed
            [SharedType.TypeVariable "a"]
        unsupportedQuery = programmaticQuery
            { SharedQuery.requestGoal = unsupportedGoal }
    case Djex.mkDjinnRequest unsupportedQuery of
        Left failure -> assertEqual
            "unsupported shared types fail while sealing the request"
            (Just "DJEX_DJINN_LOWER")
            (SharedDiagnostic.diagnosticCode failure)
        Right _ -> fail "the Djinn request constructor accepted an unboxed tuple"

    qualifier <- expectShownRight $ SharedName.mkModuleName "External"
    invalidClass <- expectShownRight $
        SharedName.mkQualifiedIdentifier qualifier "Eq"
    let invalidClassQuery = programmaticQuery
            { SharedQuery.requestContexts =
                [Constraint invalidClass [SharedType.TypeVariable "a"]] }
    invalidClassFailure <- case Djex.mkDjinnRequest invalidClassQuery of
        Left failure -> do
            assertEqual "qualified classes fail while sealing the request"
                (Just "DJEX_DJINN_LOWER")
                (SharedDiagnostic.diagnosticCode failure)
            pure failure
        Right _ -> fail "the Djinn request constructor accepted a qualified class"
    let interContextPrecedenceQuery = programmaticQuery
            { SharedQuery.requestContexts =
                [ Constraint invalidClass [SharedType.TypeVariable "a"]
                , Constraint (sharedName "Eq") [unsupportedGoal]
                ]
            }
    case Djex.mkDjinnRequest interContextPrecedenceQuery of
        Left failure -> assertEqual
            "an earlier class failure was overtaken by a later context type"
            invalidClassFailure failure
        Right _ -> fail "the Djinn request constructor lost both context errors"

    invalidTarget <- expectShownRight $
        SharedName.mkQualifiedIdentifier qualifier "identity"
    case Djex.parseDjinnRequest session defaultQueryOptions invalidTarget
            "bad-target.djinn" "(" of
        Left failure -> do
            assertEqual "target validation precedes query parsing"
                (Just "DJEX_DJINN_TARGET")
                (SharedDiagnostic.diagnosticCode failure)
            assertEqual "a target error is not mislabeled as a source error"
                Nothing (SharedDiagnostic.diagnosticSource failure)
        Right _ -> fail "the checked Djinn parser accepted a qualified target"
  where
    -- Keep the visually useful expected value on the right without obscuring
    -- the field projected through the opaque request boundary.
    assertEqualReversed actual expected = assertEqual "parsed request field"
        expected actual

testCheckedDjinnRequestReuse :: IO ()
testCheckedDjinnRequestReuse = do
    initial <- expectShownRight Djex.standardDjinnSession
    boolSession <- expectShownRight $ Djex.declareDjinnDeclaration
        (TypeSynonym "Selected" [] $ HTCon "Bool") initial
    voidSession <- expectShownRight $ Djex.declareDjinnDeclaration
        (TypeSynonym "Selected" [] $ HTCon "Void") initial
    target <- expectShownRight $ SharedName.mkIdentifier "selectedValue"
    checkedTarget <- expectShownRight $ SharedGenerated.mkDefinitionName target
    let query = SharedQuery.QueryRequest
            { SharedQuery.requestTarget = checkedTarget
            , SharedQuery.requestGoal =
                SharedType.TypeConstructor $ sharedName "Selected"
            , SharedQuery.requestContexts = []
            , SharedQuery.requestOptions = defaultQueryOptions
            }
    request <- expectShownRight $ Djex.mkDjinnRequest query
    boolResult <- expectShownRight $ Djex.runDjinnQuery boolSession request
    voidResult <- expectShownRight $ Djex.runDjinnQuery voidSession request
    assertBool "a request did not elaborate Selected against the Bool session"
        $ not $ null $ SharedSearch.batchCandidates
        $ SharedQuery.resultSearch boolResult
    assertEqual "a request retained the Bool meaning of Selected in another session"
        [] $ SharedSearch.batchCandidates $ SharedQuery.resultSearch voidResult
    assertEqual "the empty Selected session lost proof-backed evidence"
        SharedQuery.ProvedUninhabitable
        (SharedQuery.resultEvidence voidResult)

expectShownRight :: Show failure => Either failure value -> IO value
expectShownRight = either (fail . show) return

testCheckedDjinnSessionEditing :: IO ()
testCheckedDjinnSessionEditing = do
    initial <- expectShownRight Djex.standardDjinnSession
    let declarationSnapshot =
            Djex.djinnSessionDeclarationSnapshot initial
    assertEqual "the combined compatibility projection changed type declarations"
        (Djex.djinnSnapshotTypeDeclarations declarationSnapshot)
        (Djex.djinnSessionTypeDeclarations initial)
    assertEqual "the combined compatibility projection changed function declarations"
        (Djex.djinnSnapshotFunctionDeclarations declarationSnapshot)
        (Djex.djinnSessionFunctionDeclarations initial)
    assertEqual "the combined compatibility projection changed class declarations"
        (Djex.djinnSnapshotClassDeclarations declarationSnapshot)
        (Djex.djinnSessionClassDeclarations initial)
    first <- expectShownRight $ Djex.declareDjinnDeclaration
        (Function "first" $ HTCon "Bool") initial
    second <- expectShownRight $ Djex.declareDjinnDeclaration
        (Function "second" $ HTCon "Bool") first
    replaced <- expectShownRight $ Djex.declareDjinnDeclaration
        (Function "first" $ HTArrow (HTCon "Bool") (HTCon "Bool"))
        second
    assertEqual "replacement lost newest-first function search order"
        ["first", "second"]
        (map fst $ Djex.djinnSessionFunctionDeclarations replaced)

    case Djex.declareDjinnDeclaration
        (TypeSynonym "Bad" ["a"] $ HTVar "b") replaced of
      Left failure -> assertEqual
        "failed edits did not use the structured environment boundary"
        (Just "DJEX_DJINN_ENV")
        (SharedDiagnostic.diagnosticCode failure)
      Right _ -> fail "an invalid synonym committed to the checked session"
    assertEqual "a failed edit changed the prior session"
        ["first", "second"]
        (map fst $ Djex.djinnSessionFunctionDeclarations replaced)

    case Djex.declareDjinnDeclaration
        (DataType "()" [] [("()", [])]) initial of
      Left failure -> do
        assertEqual "unit declaration lost its diagnostic code"
            (Just "DJEX_DJINN_ENV")
            (SharedDiagnostic.diagnosticCode failure)
        assertBool "unit declaration lost its protected-name explanation"
            $ "cannot be declared" `isInfixOf`
                unwords (SharedDiagnostic.diagnosticContext failure)
      Right _ -> fail "the shared editor installed the protected unit"

    withoutSecond <- expectShownRight $
        Djex.removeDjinnDeclaration "second" replaced
    assertEqual "deletion reordered surviving functions"
        ["first"]
        (map fst $ Djex.djinnSessionFunctionDeclarations withoutSecond)

    withBase <- expectShownRight $ Djex.declareDjinnDeclaration
        (AbstractType "Base" KStar) withoutSecond
    withAlias <- expectShownRight $ Djex.declareDjinnDeclaration
        (TypeSynonym "UsesBase" [] $ HTCon "Base") withBase
    case Djex.removeDjinnDeclaration "Base" withAlias of
      Left failure -> assertEqual "dependent deletion lost its diagnostic code"
        (Just "DJEX_DJINN_ENV")
        (SharedDiagnostic.diagnosticCode failure)
      Right _ -> fail "deletion committed while a surviving alias depended on it"

    case Djex.removeDjinnDeclaration "()" initial of
      Left failure -> do
        assertEqual "unit deletion lost its diagnostic code"
            (Just "DJEX_DJINN_ENV")
            (SharedDiagnostic.diagnosticCode failure)
        assertBool "unit deletion lost its compatibility explanation"
            $ "built-in type" `isInfixOf`
                unwords (SharedDiagnostic.diagnosticContext failure)
      Right _ -> fail "the shared editor removed the canonical unit"

    equality <- either fail pure $
        mkContext "Eq" [HTCon "Bool"]
    methods <- expectShownRight $
        Djex.resolveDjinnInstanceMethods initial [] equality
    assertEqual "session-level instance lookup changed class method order"
        ["=="] (map fst methods)

    withTokenType <- expectShownRight $ Djex.declareDjinnDeclaration
        (AbstractType "Token" KStar) initial
    withTokenValue <- expectShownRight $ Djex.declareDjinnDeclaration
        (Function "token" $ HTCon "Token") withTokenType
    target <- expectShownRight $ SharedName.mkIdentifier "answer"
    request <- expectShownRight $ Djex.parseDjinnRequest
        withTokenValue defaultQueryOptions target "premise-cache.djinn" "Token"
    inhabited <- expectShownRight $
        Djex.runDjinnQuery withTokenValue request
    assertBool "a newly sealed global premise was unavailable to proof search"
        $ not $ null $ SharedSearch.batchCandidates
        $ SharedQuery.resultSearch inhabited

    withoutTokenValue <- expectShownRight $ Djex.declareDjinnDeclaration
        (Function "token" $ HTArrow (HTCon "Token") (HTCon "Token"))
        withTokenValue
    uninhabited <- expectShownRight $
        Djex.runDjinnQuery withoutTokenValue request
    assertEqual "function replacement retained a stale prepared premise"
        []
        (SharedSearch.batchCandidates $ SharedQuery.resultSearch uninhabited)

    -- The ground editor rejects an unsolved kind variable at the entrance
    -- with the same error value the weakened path produced during resealing.
    case Djex.declareDjinnDeclaration (AbstractType "Mystery" (KVar 0)) initial of
      Left failure -> do
        assertEqual "ungrounded kinds lost the environment diagnostic code"
            (Just "DJEX_DJINN_ENV")
            (SharedDiagnostic.diagnosticCode failure)
        assertBool "ungrounded kinds lost the historical unsolved-kind text"
            $ "kind contains an unsolved variable: k0" `isInfixOf`
                unwords (SharedDiagnostic.diagnosticContext failure)
      Right _ -> fail "the ground editor accepted an unsolved kind variable"

-- The stable adapter now edits the sealed ground environment directly while
-- the raw compatibility API still weakens kinds to Int and re-grounds during
-- resealing. Pin that the two transactions cannot drift by running the same
-- edits through both and comparing every observable projection, including
-- failure values.
testGroundEditAgreement :: IO ()
testGroundEditAgreement = do
    initial <- expectShownRight Djex.standardDjinnSession
    let groundEnv = Djex.djinnSessionEnvironment initial
        weakened = SharedEnvironment.mapEnvironmentKindVariables
            absurd groundEnv
        agreeDeclare label declaration = agree label
            (RawEnvironment.declareGroundSynthesisEnvironment
                declaration groundEnv)
            (RawEnvironment.declareSynthesisEnvironment declaration weakened)
        agreeRemove label name = agree label
            (RawEnvironment.removeGroundSynthesisDeclaration name groundEnv)
            (RawEnvironment.removeSynthesisDeclaration name weakened)
        agree label groundResult weakenedResult =
            case (groundResult, weakenedResult) of
                (Left groundFailure, Left weakenedFailure) -> assertEqual
                    (label ++ ": failure values diverged")
                    weakenedFailure groundFailure
                (Right (groundCandidate, groundPrepared),
                 Right (weakenedCandidate, weakenedPrepared)) -> do
                    assertEqual (label ++ ": edited environments diverged")
                        weakenedCandidate
                        (SharedEnvironment.mapEnvironmentKindVariables absurd
                            groundCandidate)
                    assertEqual (label ++ ": raw projections diverged")
                        (RawEnvironment.preparedEnvironmentSource
                            weakenedPrepared)
                        (RawEnvironment.preparedEnvironmentSource
                            groundPrepared)
                    assertEqual (label ++ ": sealed inventories diverged")
                        (SharedInventory.inventoryEnvironment
                            $ RawEnvironment.preparedEnvironmentInventory
                                weakenedPrepared)
                        (SharedInventory.inventoryEnvironment
                            $ RawEnvironment.preparedEnvironmentInventory
                                groundPrepared)
                (Left failure, Right _) -> fail $ label
                    ++ ": only the ground transaction failed: " ++ show failure
                (Right _, Left failure) -> fail $ label
                    ++ ": only the weakened transaction failed: "
                    ++ show failure
    agreeDeclare "declare a function"
        $ Function "probe" $ HTArrow (HTVar "a") (HTVar "a")
    agreeDeclare "declare a synonym"
        $ TypeSynonym "Probe" ["a"] $ HTVar "a"
    agreeDeclare "declare a class" $ ClassDecl "Probeable" ["a"]
        [("probeOut", HTVar "a")]
    agreeDeclare "reject the protected unit" $ DataType "()" [] [("()", [])]
    agreeDeclare "reject an unsolved kind"
        $ AbstractType "Mystery" $ KVar 0
    agreeRemove "remove a standalone datatype" "Either"
    agreeRemove "reject removing the unit" "()"
    agreeRemove "reject removing a missing name" "missing"

testCheckedDjinnCacheInvalidation :: IO ()
testCheckedDjinnCacheInvalidation = do
    initial <- expectShownRight Djex.standardDjinnSession
    withA <- expectShownRight $ Djex.declareDjinnDeclaration
        (AbstractType "A" KStar) initial
    withAB <- expectShownRight $ Djex.declareDjinnDeclaration
        (AbstractType "B" KStar) withA
    withAlias <- expectShownRight $ Djex.declareDjinnDeclaration
        (TypeSynonym "Selected" [] $ HTCon "A") withAB
    withSelected <- expectShownRight $ Djex.declareDjinnDeclaration
        (Function "selected" $ HTCon "Selected") withAlias
    targetA <- expectShownRight $ SharedName.mkIdentifier "answerA"
    requestA <- expectShownRight $ Djex.parseDjinnRequest
        withSelected defaultQueryOptions targetA "synonym-cache.djinn" "A"
    beforeReplacement <- expectShownRight $
        Djex.runDjinnQuery withSelected requestA
    assertBool "the alias-backed premise did not initially prove A"
        $ hasCandidates beforeReplacement

    retargeted <- expectShownRight $ Djex.declareDjinnDeclaration
        (TypeSynonym "Selected" [] $ HTCon "B") withSelected
    staleA <- expectShownRight $ Djex.runDjinnQuery retargeted requestA
    assertBool "synonym replacement retained the old global formula"
        $ not $ hasCandidates staleA
    targetB <- expectShownRight $ SharedName.mkIdentifier "answerB"
    requestB <- expectShownRight $ Djex.parseDjinnRequest
        retargeted defaultQueryOptions targetB "synonym-cache.djinn" "B"
    freshB <- expectShownRight $ Djex.runDjinnQuery retargeted requestB
    assertBool "synonym replacement did not rebuild the global formula"
        $ hasCandidates freshB

    withOldClass <- expectShownRight $ Djex.declareDjinnDeclaration
        (ClassDecl "Selectable" ["a"] [("oldMethod", HTVar "a")])
        initial
    selectedBool <- either fail pure $
        mkContext "Selectable" [HTCon "Bool"]
    oldMethods <- expectShownRight $ Djex.resolveDjinnInstanceMethods
        withOldClass [] selectedBool
    assertEqual "the initial class index lost its method"
        ["oldMethod"] (map fst oldMethods)

    withNewClass <- expectShownRight $ Djex.declareDjinnDeclaration
        (ClassDecl "Selectable" ["a"] [("newMethod", HTVar "a")])
        withOldClass
    newMethods <- expectShownRight $ Djex.resolveDjinnInstanceMethods
        withNewClass [] selectedBool
    assertEqual "class replacement retained a stale method index"
        ["newMethod"] (map fst newMethods)
    withoutClass <- expectShownRight $
        Djex.removeDjinnDeclaration "Selectable" withNewClass
    case Djex.resolveDjinnInstanceMethods withoutClass [] selectedBool of
        Left failure -> assertBool "class deletion lost its lookup diagnostic"
            $ "Class not found: Selectable" `isInfixOf`
                unwords (SharedDiagnostic.diagnosticContext failure)
        Right methods -> fail $ "class deletion retained methods: " ++
            show methods
  where
    hasCandidates = not . null . SharedSearch.batchCandidates .
        SharedQuery.resultSearch

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
    assertEqual "contextual parsing shares the complete compatibility grammar"
        (Right ([context "Eq" [HTVar "a"]],
            HTArrow (HTVar "a") (HTVar "a")))
        (parseContextualHType "Eq a => a -> a")
    assertLeft "contextual parsing rejects trailing input"
        (parseContextualHType "Eq a => a -> a ;")
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
    checkedIdentity <- expectShownRight $ SharedGenerated.mkDefinitionName $
        sharedName "identity"
    case inhabitResult defaultQueryOptions {optionCutoff = 0}
            standardEnvironment [] checkedIdentity
            (HTArrow (HTVar "a") (HTVar "a")) of
        Left (DjinnQueryOptionsFailure (NonPositiveCandidateCutoff 0)) ->
            return ()
        other -> fail $ "unexpected cutoff validation result: " ++ show other
    case inhabitResult defaultQueryOptions {optionBudget = Just (-7)}
            standardEnvironment [] checkedIdentity
            (HTArrow (HTVar "a") (HTVar "a")) of
        Left (DjinnQueryOptionsFailure (NegativeChoicePointBudget (-7))) ->
            return ()
        other -> fail $ "unexpected budget validation result: " ++ show other
    assertEqual "the historical option error text remains stable"
        (Left "optionCutoff must be positive")
        (inhabitGenerated defaultQueryOptions {optionCutoff = 0}
            standardEnvironment [] "identity"
            (HTArrow (HTVar "a") (HTVar "a")))
    assertEqual "the historical budget error text remains stable"
        (Left "optionBudget must be non-negative")
        (inhabitGenerated defaultQueryOptions {optionBudget = Just (-7)}
            standardEnvironment [] "identity"
            (HTArrow (HTVar "a") (HTVar "a")))
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
    assertEqual "the shared renderer preserves Djinn source syntax"
        (show source) (SharedTypeRender.renderType id shared)
    assertEqual "ordinary Djinn types retain the native shared tree"
        (Just shared) (hTypeSynthesisStructure source)
    assertEqual "native shared trees wrap without recursive conversion"
        (Just source) (fromHTypeSynthesisStructure shared)
    assertEqual "Djinn's source-type subset round-trips losslessly"
        (Right source) (fromSynthesisType shared)
    unit <- either (fail . show) pure $ toSynthesisType $ HTCon "()"
    assertEqual "unit is the structural nullary tuple"
        (SharedType.TupleType SharedName.Boxed []) unit
    assertEqual "unit returns to Djinn's canonical constructor form"
        (Right $ HTCon "()") (fromSynthesisType unit)
    assertBool "a caller-built empty tuple retains its historical constructor"
        (HTTuple [] /= HTCon "()")
    assertEqual "nested canonical units project as constructor atoms"
        (Right $ HTTuple [HTCon "()", HTCon "()"])
        (fromSynthesisType $ SharedType.TupleType SharedName.Boxed
            [ SharedType.TupleType SharedName.Boxed []
            , SharedType.TupleType SharedName.Boxed []
            ])
    let malformedConstructor = HTCon "not a type"
    assertEqual "malformed compatibility names stay outside the shared tree"
        Nothing (hTypeSynthesisStructure malformedConstructor)
    assertBool "malformed compatibility names retain checked diagnostics" $
        case toSynthesisType malformedConstructor of
            Left (InvalidHTypeName "not a type" _) -> True
            _ -> False
    assertEqual "declaration bodies cannot masquerade as source types"
        (Left $ DeclarationBodyIsNotSourceType $ HTUnion [])
        (toSynthesisType $ HTUnion [])
    assertEqual "explicit foralls remain outside Djinn's supported subset"
        (Left SynthesisForallUnsupported)
        (fromSynthesisType $ SharedType.ForallType ["a"] []
            $ SharedType.TypeVariable "a")
    assertEqual "shared binder errors precede Djinn's forall restriction"
        (Left $ InvalidSynthesisType
            $ SharedType.DuplicateForallVariable "a")
        (fromSynthesisType $ SharedType.ForallType ["a", "a"] []
            $ SharedType.TypeVariable "a")
    let oversizedTuple = HTTuple $ repeat $ HTVar "a"
    case oversizedTuple of
        HTTuple _ -> pure ()
        _ -> fail "an oversized compatibility tuple did not construct"
    assertEqual "oversized tuples stay outside the native unchecked fast path"
        Nothing (hTypeSynthesisStructure oversizedTuple)
    assertEqual "oversized compatibility tuples fail at the bounded arity"
        (Left $ InvalidSynthesisType
            $ SharedType.InvalidTupleTypeArity SharedName.Boxed
                $ SharedName.maximumTupleArity + 1)
        (toSynthesisType oversizedTuple)
    assertEqual "constructor-like strings cannot become Djinn type variables"
        (Left $ InvalidDjinnTypeVariable "A")
        (fromSynthesisType $ SharedType.TypeVariable "A")
    assertEqual "reserved strings cannot leave Djinn as type variables"
        (Left $ InvalidDjinnTypeVariable "case")
        (toSynthesisType $ HTVar "case")
    assertEqual "qualified strings cannot become Djinn type variables"
        (Left $ InvalidDjinnTypeVariable "M.a")
        (fromSynthesisType $ SharedType.TypeVariable "M.a")
    let typeOperator = sharedName "(:+:)"
    assertEqual "shared type operators cannot enter Djinn's prefix-only parser"
        (Left $ UnsupportedDjinnTypeConstructorName typeOperator)
        (fromSynthesisType $ SharedType.TypeConstructor typeOperator)
    assertEqual "raw Djinn type operators cannot cross the shared boundary"
        (Left $ UnsupportedDjinnTypeConstructorName typeOperator)
        (toSynthesisType $ HTCon "(:+:)")
    mapM_ assertSharedRendering
        [ HTCon "[]"
        , HTApp (HTCon "[]") (HTVar "a")
        , HTApp (HTApp (HTCon "[]") (HTVar "a")) (HTVar "b")
        , HTApp (HTCon "[]") $
            HTApp (HTCon "[]") (HTVar "a")
        ]
  where
    assertSharedRendering raw = do
        projected <- expectShownRight $ toSynthesisType raw
        assertEqual ("shared rendering changed " ++ show raw)
            (show raw) (SharedTypeRender.renderType id projected)

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
        (Left $ UnsupportedDjinnDeclarationName FunctionOwner
            $ sharedName "T")
        (toSynthesisDeclaration $ Function "T" $ HTCon "()")
    let unitType = SharedType.TupleType SharedName.Boxed []
        unkinded variable = SharedDeclaration.TypeParameter variable Nothing
        signature name = SharedDeclaration.ValueSignature ()
            (sharedName name) unitType
        sharedType name parameters =
            SharedDeclaration.TypeSynonymDeclaration () (sharedName name)
                parameters unitType
        sharedData name constructor =
            SharedDeclaration.DataTypeDeclaration () (sharedName name) []
                [SharedDeclaration.DataConstructor ()
                    (sharedName constructor) []]
        roleCheckedClass name methods = SharedDeclaration.ClassDeclaration ()
            (sharedName name) [] [] methods
    assertEqual "unused uppercase parameters still obey Djinn's VarId grammar"
        (Left $ DeclarationTypeConversionError
            $ InvalidDjinnTypeVariable "A")
        (fromSynthesisDeclaration $ sharedType "Phantom" [unkinded "A"])
    assertEqual "type owners remain local ConIds"
        (Left $ UnsupportedDjinnDeclarationName TypeOwner
            $ sharedName "M.Alias")
        (fromSynthesisDeclaration $ sharedType "M.Alias" [])
    assertEqual "symbolic type owners remain outside Djinn's declaration syntax"
        (Left $ UnsupportedDjinnDeclarationName TypeOwner
            $ sharedName "(:+:)")
        (fromSynthesisDeclaration $
            SharedDeclaration.AbstractTypeDeclaration ()
                (sharedName "(:+:)") SharedKind.ProperTypeKind)
    assertEqual "data constructors remain local ConIds"
        (Left $ UnsupportedDjinnDeclarationName DataConstructorOwner
            $ sharedName "M.Box")
        (fromSynthesisDeclaration $ sharedData "Box" "M.Box")
    assertEqual "class owners remain local ConIds"
        (Left $ UnsupportedDjinnDeclarationName ClassOwner
            $ sharedName "M.Marker")
        (fromSynthesisDeclaration $ roleCheckedClass "M.Marker" [])
    assertEqual "method owners remain unqualified value names"
        (Left $ UnsupportedDjinnDeclarationName MethodOwner
            $ sharedName "M.marker")
        (fromSynthesisDeclaration $
            roleCheckedClass "Marker" [signature "M.marker"])
    assertEqual "type operators are rejected inside declaration signatures"
        (Left $ DeclarationTypeConversionError
            $ UnsupportedDjinnTypeConstructorName $ sharedName "(:+:)")
        (fromSynthesisDeclaration $ SharedDeclaration.ValueDeclaration
            $ SharedDeclaration.ValueSignature () (sharedName "value")
            $ SharedType.TypeConstructor $ sharedName "(:+:)")
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
    let unitName = sharedName "()"
        unitType = SharedType.TupleType SharedName.Boxed []
        forgedUnitAlias = SharedDeclaration.TypeSynonymDeclaration ()
            unitName [] unitType
        stolenUnitConstructor = SharedDeclaration.DataTypeDeclaration ()
            (sharedName "CounterfeitUnit") []
            [SharedDeclaration.DataConstructor () unitName []]
        canonicalUnit = SharedDeclaration.DataTypeDeclaration () unitName []
            [SharedDeclaration.DataConstructor () unitName []]
        rejectsForgedUnit description declaration = do
            forged <- either (fail . show) pure
                $ SharedEnvironment.mkEnvironment [declaration]
            assertEqual description
                (Left $ SynthesisEnvironmentDeclarationError
                    NonCanonicalUnitDeclaration)
                (fromSynthesisEnvironment forged)
    rejectsForgedUnit "a synonym cannot own structural unit"
        forgedUnitAlias
    rejectsForgedUnit "another datatype cannot own the unit constructor"
        stolenUnitConstructor
    canonicalUnitEnvironment <- either (fail . show) pure
        $ SharedEnvironment.mkEnvironment [canonicalUnit]
    loweredUnit <- either (fail . show) pure
        $ fromSynthesisEnvironment canonicalUnitEnvironment
    assertEqual "only canonical data () = () crosses the trusted unit path"
        [ ("()", ([], HTUnion [("()", [])], KStar)) ]
        (typeDeclarations loweredUnit)

-- The stable Djex boundary must preserve the neutral Environment as the
-- authoritative declaration inventory. Historical raw tables are on-demand
-- compatibility projections; making them the retained source instead would
-- lose global order and can change synonym-transparent recursion.
testNeutralDjinnPreparation :: IO ()
testNeutralDjinnPreparation = do
    let proper = SharedKind.ProperTypeKind
        abstract name kind =
            SharedDeclaration.AbstractTypeDeclaration ()
                (sharedName name) kind
        value name valueType = SharedDeclaration.ValueDeclaration
            $ SharedDeclaration.ValueSignature () (sharedName name) valueType
        variable name = SharedType.TypeVariable name
        constructor name = SharedType.TypeConstructor $ sharedName name
        apply = SharedType.TypeApplication
        parameter name = SharedDeclaration.TypeParameter name Nothing
        dataType name fields = SharedDeclaration.DataTypeDeclaration ()
            (sharedName name) []
            [SharedDeclaration.DataConstructor () (sharedName name) fields]
        methodlessClass = SharedDeclaration.ClassDeclaration ()
            (sharedName "Marker") [parameter "a"] [] []
        orderedDeclarations =
            [ abstract "First" proper
            , value "firstValue" $ constructor "First"
            , methodlessClass
            , abstract "Second" proper
            , value "secondValue" $ constructor "Second"
            ]

    orderedEnvironment <- mkNeutralDjinnEnvironment orderedDeclarations
    orderedSession <- expectShownRight
        $ Djex.mkDjinnSession orderedEnvironment
    assertEqual "the session changed its derived editable environment"
        orderedEnvironment
        (Djex.djinnSessionEnvironment orderedSession)
    assertEqual "the session inventory changed global declaration order"
        (SharedEnvironment.environmentDeclarations orderedEnvironment)
        (SharedEnvironment.environmentDeclarations
            $ SharedInventory.inventoryEnvironment
            $ Djex.djinnSessionInventory orderedSession)
    assertEqual "a methodless Djinn class did not default its parameter to *"
        (Just [Just proper])
        (Map.lookup (sharedName "Marker")
            $ SharedInference.classParameterKinds
            $ SharedInventory.inventoryKindAssumptions
            $ Djex.djinnSessionInventory orderedSession)

    -- Synonym expansion must precede the recursive-datatype policy.  The
    -- parameter of Phantom is semantically absent, so this datatype is not
    -- recursive despite the surface occurrence of D in its constructor.
    let boolDeclaration = abstract "Bool" proper
        phantomDeclaration = SharedDeclaration.TypeSynonymDeclaration ()
            (sharedName "Phantom") [parameter "a"] $ constructor "Bool"
        phantomUse = apply (constructor "Phantom") (constructor "D")
        phantomDeclarations =
            [boolDeclaration, phantomDeclaration, dataType "D" [phantomUse]]
    phantomEnvironment <- mkNeutralDjinnEnvironment phantomDeclarations
    _ <- expectShownRight $ fromSynthesisEnvironment
        $ SharedEnvironment.mapEnvironmentKindVariables absurd
        phantomEnvironment
    _ <- expectShownRight $ Djex.mkDjinnSession phantomEnvironment

    let identityDeclaration = SharedDeclaration.TypeSynonymDeclaration ()
            (sharedName "Identity") [parameter "a"] $ variable "a"
        identityUse name = apply (constructor "Identity") (constructor name)
        recursiveCases =
            [ ("direct recursion",
                [dataType "Direct" [constructor "Direct"]])
            , ("mutual recursion",
                [ dataType "Even" [constructor "Odd"]
                , dataType "Odd" [constructor "Even"]
                ])
            , ("recursion exposed by synonym expansion",
                [ identityDeclaration
                , dataType "AliasLoop" [identityUse "AliasLoop"]
                ])
            ]
    mapM_ assertRecursiveRejection recursiveCases

    -- Kind inference alone accepts this partially applied synonym: Pair Bool
    -- has kind * -> *, exactly the argument kind expected by Higher.  Djinn's
    -- Haskell-compatible boundary must nevertheless reject every supported
    -- declaration position in which a synonym is not fully saturated.
    let higherKind = SharedKind.FunctionKind
            (SharedKind.FunctionKind proper proper) proper
        pairDeclaration = SharedDeclaration.TypeSynonymDeclaration ()
            (sharedName "Pair") [parameter "a", parameter "b"]
            $ SharedType.TupleType SharedName.Boxed
                [variable "a", variable "b"]
        partialPair = apply (constructor "Pair") (constructor "Bool")
        partialUse = apply (constructor "Higher") partialPair
        saturationBase =
            [ boolDeclaration
            , abstract "Higher" higherKind
            , pairDeclaration
            ]
        badSynonym = SharedDeclaration.TypeSynonymDeclaration ()
            (sharedName "BadSynonym") [] partialUse
        badData = dataType "BadData" [partialUse]
        badValue = value "badValue" partialUse
        badClass = SharedDeclaration.ClassDeclaration ()
            (sharedName "BadClass") [parameter "a"] []
            [SharedDeclaration.ValueSignature ()
                (sharedName "badMethod") partialUse]
        saturationCases =
            [ ("a synonym body", badSynonym)
            , ("a data-constructor field", badData)
            , ("a value signature", badValue)
            , ("a class method", badClass)
            ]
    saturationEnvironment <- mkNeutralDjinnEnvironment saturationBase
    _ <- expectShownRight $ Djex.mkDjinnSession saturationEnvironment
    mapM_ (assertUnsaturatedRejection saturationBase) saturationCases
  where
    assertRecursiveRejection (description, declarations) = do
        environment <- mkNeutralDjinnEnvironment declarations
        assertDjinnSessionRejected description
            $ Djex.mkDjinnSession environment

    assertUnsaturatedRejection base (description, declaration) = do
        environment <- mkNeutralDjinnEnvironment $ base ++ [declaration]
        case Djex.mkDjinnSession environment of
            Left failure -> do
                assertEqual (description ++ " has the environment code")
                    (Just "DJEX_DJINN_ENV")
                    (SharedDiagnostic.diagnosticCode failure)
                let contextText = unwords
                        $ SharedDiagnostic.diagnosticContext failure
                assertBool (description ++ " did not report Pair")
                    $ "Pair" `isInfixOf` contextText
                assertBool (description ++ " did not report saturation")
                    $ ("expects 2" `isInfixOf` contextText &&
                        "got 1" `isInfixOf` contextText) ||
                      ("UnsaturatedTypeSynonym" `isInfixOf` contextText &&
                        "1" `isInfixOf` contextText &&
                        "2" `isInfixOf` contextText)
            Right _ -> fail $ description ++
                ": an unsaturated type synonym reached Djinn"

-- Global assumptions are invariant across queries, so sealing translates
-- them once in exact declaration order. The raw table remains an on-demand
-- compatibility projection of the shared inventory rather than retained
-- session state.
testPreparedFunctionPremises :: IO ()
testPreparedFunctionPremises = do
    let atom = HTCon "Atom"
        alias = HTCon "Alias"
        environment = RawEnvironment.Environment
            [ ("Atom", ([], HTAbstract "Atom" KStar, KStar))
            , ("Alias", ([], atom, KStar))
            ]
            [ ("zAliased", alias)
            , ("External.zAliased", alias)
            , ("%%", alias)
            , ("aIdentity", HTArrow alias alias)
            ]
            []
        atomFormula = PVar $ Symbol "Atom"
        expectedPremises =
            [ (Symbol "zAliased", atomFormula)
            , (Symbol "External.zAliased", atomFormula)
            , (Symbol "%%", atomFormula)
            , (Symbol "aIdentity", atomFormula :-> atomFormula)
            ]
    prepared <- expectShownRight $ prepareEnvironment environment
    assertEqual "prepared premises changed source order or alias expansion"
        expectedPremises
        (RawEnvironment.preparedEnvironmentFunctionPremises prepared)
    neutral <- expectShownRight $ toSynthesisEnvironment environment
    nativePrepared <- expectShownRight $
        RawEnvironment.prepareSynthesisEnvironment neutral
    assertEqual "native preparation changed premise order or operator names"
        expectedPremises
        (RawEnvironment.preparedEnvironmentFunctionPremises nativePrepared)
    assertEqual "the compatibility projection changed the sealed declarations"
        environment (RawEnvironment.preparedEnvironmentSource prepared)
    assertEqual "native preparation lost its on-demand raw projection"
        environment (RawEnvironment.preparedEnvironmentSource nativePrepared)
    firstResult <- expectRight $ inhabitGeneratedPrepared
        defaultQueryOptions prepared [] "answer" alias
    secondResult <- expectRight $ inhabitGeneratedPrepared
        defaultQueryOptions prepared [] "answer" alias
    assertBool "cached global premises were not consumed by proof search"
        $ not $ null $ generatedReportCandidates firstResult
    assertEqual "reusing cached premises changed a repeated query"
        firstResult secondResult

-- The stable query path and global-premise cache must not need an HType
-- round-trip. Assert explicit formulas, rather than equality alone, so the two
-- entrances cannot drift together. In particular, unit is a nominal datatype
-- in Djinn: treating the shared zero-tuple as structural truth renders the
-- same Haskell value but changes both the formula and explored proof.
testPreparedFormulaParity :: IO ()
testPreparedFormulaParity = do
    environment <- expectRight $ do
        withIdentity <- declare
            (TypeSynonym "Id" ["x"] $ HTVar "x")
            standardEnvironment
        withAbstract <- declare
            (AbstractType "F" $ KArrow KStar KStar)
            withIdentity
        withEmpty <- declare
            (DataType "EmptyOf" ["x"] []) withAbstract
        declare
            (DataType "Box" ["x"] [("MkBox", [HTVar "x"])])
            withEmpty
    prepared <- expectShownRight $ prepareEnvironment environment
    rawFormula <- expectRight $ prepareTypeFormulaTranslator $
        RawEnvironment.envTypes environment

    let atom name = PVar $ Symbol name
        pair = HTTuple [HTVar "a", HTVar "b"]
        pairFormula = Conj [atomA, atomB]
        apply name arguments = foldl HTApp (HTCon name) arguments
        identity source = apply "Id" [source]
        list source = apply "[]" [source]
        abstract source = apply "F" [source]
        empty source = apply "EmptyOf" [source]
        box source = apply "Box" [source]
        listPairFormula = atom "[(a, b)]"
        emptyPairFormula = Empty $ Symbol "EmptyOf (a, b)"
        unitFormula = Disj [(ConsDesc "()" 0, Conj [])]
        cases =
            [ ("unit", HTCon "()", unitFormula)
            , ("qualified opaque constructor", HTCon "External.T",
                  atom "External.T")
            , ("prefix function constructor atom", HTCon "->",
                  atom "(->)")
            , ("tuple", pair, pairFormula)
            , ("opaque list", list pair, listPairFormula)
            , ("transparent alias", identity pair, pairFormula)
            , ("alias under list", list $ identity pair, listPairFormula)
            , ("alias under abstract", abstract $ identity pair,
                  atom "F (a, b)")
            , ("parameterized empty type", empty $ identity pair,
                  emptyPairFormula)
            , ("unary tuple payload", box $ identity pair,
                  Disj [(ConsDesc "MkBox" 1, Conj [pairFormula])])
            , ("constructor order", apply "Maybe" [HTVar "a"],
                  Disj
                    [ (ConsDesc "Nothing" 0, Conj [])
                    , (ConsDesc "Just" 1, Conj [atomA])
                    ])
            , ("recursive arrow view",
                  HTArrow (list $ identity pair) (empty $ identity pair),
                  listPairFormula :-> emptyPairFormula)
            ]

    mapM_ (assertFormulaParity rawFormula prepared) cases

    target <- expectShownRight $ SharedGenerated.mkDefinitionName $
        sharedName "nativeUnit"
    result <- expectShownRight $ inhabitSynthesisResultPrepared
        defaultQueryOptions prepared [] target $
            SharedType.TupleType SharedName.Boxed []
    let search = SharedQuery.resultSearch result
        metadata = SharedSearch.batchMetadata search
    assertEqual "native unit lost its nominal formula"
        "(|true)" (djinnTranslatedFormula metadata)
    assertEqual "native unit lost its constructor injection"
        (Just "Inj0 Tuple0") (djinnFirstExploredProof metadata)
    case SharedSearch.batchCandidates search of
        candidate : _ -> assertEqual
            "native unit candidate changed despite stable metadata"
            (Right "nativeUnit = ()") $
                SharedGenerated.renderFunctionClause
                    (SharedGenerated.defaultRenderOptions id)
                    (SharedCandidate.candidateOutput candidate)
        [] -> fail "native unit query produced no candidate"
  where
    assertFormulaParity rawFormula prepared (description, raw, expected) = do
        shared <- expectShownRight $ toSynthesisType raw
        compatibilityFormula <- expectRight $ rawFormula raw
        nativeFormula <- expectRight $
            RawEnvironment.preparedEnvironmentSynthesisFormulaTranslator
                prepared shared
        assertEqual (description ++ ": raw formula changed")
            expected compatibilityFormula
        assertEqual (description ++ ": shared formula changed")
            expected nativeFormula

mkNeutralDjinnEnvironment
    :: [SharedDeclaration.Declaration String Void ()]
    -> IO Djex.DjinnEnvironment
mkNeutralDjinnEnvironment = expectShownRight .
    SharedEnvironment.mkEnvironment

assertDjinnSessionRejected
    :: String
    -> Either SharedDiagnostic.Diagnostic Djex.DjinnSession
    -> IO ()
assertDjinnSessionRejected description result = case result of
    Left failure -> assertEqual (description ++ " has the environment code")
        (Just "DJEX_DJINN_ENV") (SharedDiagnostic.diagnosticCode failure)
    Right _ -> fail $ description ++ ": a recursive datatype reached Djinn"

-- Raw Environment is a compatibility input, not a second kind authority.
-- In particular, callers of the internal constructor can forge stale class
-- parameter kinds; preparation must replace them with the assumptions inferred
-- from the same declarations before context lookup sees the backend table.
testRawDjinnPreparationKinds :: IO ()
testRawDjinnPreparationKinds = do
    let RawEnvironment.Environment types functions _ = standardEnvironment
        forged = RawEnvironment.Environment types functions
            [("Marker", ([("a", KArrow KStar KStar)], []))]
    prepared <- expectShownRight $ prepareEnvironment forged
    assertEqual "raw class kinds did not follow the checked inventory"
        [("Marker", ([("a", KStar)], []))]
        (classDeclarations $ RawEnvironment.preparedEnvironmentSource prepared)
    markerMaybe <- either fail pure $ mkContext "Marker" [HTCon "Maybe"]
    assertLeft "a stale raw class kind overrode the shared inventory"
        $ resolvePreparedContext prepared markerMaybe

testBoundedContextArity :: IO ()
testBoundedContextArity = do
    prepared <- expectShownRight $ prepareEnvironment standardEnvironment
    let arguments = repeat $ HTVar "a"
        malformed = Constraint (sharedName "Eq") arguments
    case resolvePreparedContext prepared malformed of
        Left message -> assertBool "context arity did not use its first failure"
            $ "expects 1 type argument(s), but got 2" `isInfixOf` message
        Right _ -> fail "an infinite known-class context was accepted"

-- Saturation and kind checking own different failures. A partial synonym can
-- be kind-compatible in a higher-kinded position, so the explicit arity guard
-- must reject it. An extra argument, however, is not an arity shortage; in
-- Djinn's proper-result synonym subset it proceeds to the shared kind checker
-- and is rejected there. Keep both the raw compatibility path and the stable
-- neutral-session path on that same diagnostic boundary.
testSynonymSaturationBoundary :: IO ()
testSynonymSaturationBoundary = do
    let applyDefinition =
            ("Apply", (["f"], HTVar "f", ()))
        maybeLikeDefinition =
            ("MaybeLike", ([],
                HTAbstract "MaybeLike" (KArrow KStar KStar), ()))
        intDefinition =
            ("Int2", ([], HTAbstract "Int2" KStar, ()))
        definitions =
            [applyDefinition, maybeLikeDefinition, intDefinition]
        apply = HTCon "Apply"
        maybeLike = HTCon "MaybeLike"
        intType = HTCon "Int2"
        maybeInt = HTApp maybeLike intType
        overapplied = HTApp (HTApp apply maybeLike) intType
    checked <- expectShownRight $ htCheckEnv definitions
    assertRawKindFailure "raw overapplication"
        $ htCheckType checked $ HTArrow overapplied maybeInt
    assertRawSaturationFailure "raw partial application"
        $ htCheckType checked $ HTArrow apply apply

    let proper = SharedKind.ProperTypeKind
        parameter = SharedDeclaration.TypeParameter "f" Nothing
        sharedApply = SharedDeclaration.TypeSynonymDeclaration ()
            (sharedName "Apply") [parameter] $ SharedType.TypeVariable "f"
        sharedMaybeLike = SharedDeclaration.AbstractTypeDeclaration ()
            (sharedName "MaybeLike")
            (SharedKind.FunctionKind proper proper)
        sharedInt = SharedDeclaration.AbstractTypeDeclaration ()
            (sharedName "Int2") proper
    environment <- mkNeutralDjinnEnvironment
        [sharedApply, sharedMaybeLike, sharedInt]
    session <- expectShownRight $ Djex.mkDjinnSession environment
    target <- expectShownRight $ SharedName.mkIdentifier "witness"
    assertStableKindFailure session target
        "Apply MaybeLike Int2 -> MaybeLike Int2"
    assertStableSaturationFailure session target "Apply -> Apply"
  where
    assertRawKindFailure description result = case result of
        Left message -> do
            assertBool (description ++ " was mislabeled as unsaturated")
                $ "expects at least" `notElemText` message
            assertBool (description ++ " did not reach kind inference")
                $ "KindMismatch" `isInfixOf` message
        Right () -> fail $ description ++ " was accepted"

    assertRawSaturationFailure description result = case result of
        Left message -> assertBool
            (description ++ " lost its saturation diagnostic")
            $ "expects at least 1 argument(s), but got 0" `isInfixOf` message
        Right () -> fail $ description ++ " was accepted"

    assertStableKindFailure session target source = do
        request <- expectShownRight $ Djex.parseDjinnRequest
            session defaultQueryOptions target "overapplied.djinn" source
        case Djex.runDjinnQuery session request of
            Left failure -> do
                assertEqual "stable overapplication has the query code"
                    (Just "DJEX_DJINN_QUERY")
                    (SharedDiagnostic.diagnosticCode failure)
                let message = unwords
                        $ SharedDiagnostic.diagnosticContext failure
                assertBool "stable overapplication was mislabeled as unsaturated"
                    $ "expects at least" `notElemText` message
                assertBool "stable overapplication did not reach kind inference"
                    $ "KindMismatch" `isInfixOf` message
            Right _ -> fail "stable overapplication was accepted"

    assertStableSaturationFailure session target source = do
        request <- expectShownRight $ Djex.parseDjinnRequest
            session defaultQueryOptions target "partial.djinn" source
        case Djex.runDjinnQuery session request of
            Left failure -> assertBool
                "stable partial application lost its saturation diagnostic"
                $ "expects at least 1 argument(s), but got 0" `isInfixOf`
                    unwords (SharedDiagnostic.diagnosticContext failure)
            Right _ -> fail "stable partial application was accepted"

    needle `notElemText` haystack = not $ needle `isInfixOf` haystack

-- Prepared sessions retain an exact shared Inventory/synonym witness for
-- operational queries, while the historical context-resolution surface
-- continues to return the caller's alias-bearing raw spelling. This distinction
-- keeps requests and compatibility inspection source-oriented without making
-- proof-search atoms depend on how an alias happened to be written.
testPreparedQuerySynonyms :: IO ()
testPreparedQuerySynonyms = do
    let variable = HTVar "a"
        boolType = HTCon "Bool"
        voidType = HTCon "Void"
        identity argument = HTApp (HTCon "Identity") argument
        valueMethod valueType = HTArrow valueType valueType
    withIdentity <- expectRight $ declare
        (TypeSynonym "Identity" ["a"] variable) standardEnvironment
    withFirst <- expectRight $ declare
        (TypeSynonym "First" ["a", "b"] variable) withIdentity
    environment <- expectRight $ declare
        (ClassDecl "ValueAlias" ["x"]
            [("valueAlias", valueMethod $ HTVar "x")])
        withFirst
    prepared <- expectShownRight $ prepareEnvironment environment
    sharedBool <- expectShownRight $ toSynthesisType boolType
    sharedVoid <- expectShownRight $ toSynthesisType voidType
    sharedIdentityBool <- expectShownRight $
        toSynthesisType $ identity boolType
    sharedIdentityVoid <- expectShownRight $
        toSynthesisType $ identity voidType

    assertEqual "the prepared environment retained its exact alias table"
        (Right [sharedBool, sharedVoid])
        (RawEnvironment.elaboratePreparedSynthesisTypes prepared
            [ (KStar, sharedIdentityBool)
            , (KStar, sharedIdentityVoid)
            ])

    let aliasContext = context "ValueAlias" [identity boolType]
        expandedContext = context "ValueAlias" [boolType]
        aliasMethod = valueMethod $ identity boolType
    assertEqual "prepared compatibility resolution erased an alias spelling"
        (Right [("valueAlias", aliasMethod)])
        (resolvePreparedContext prepared aliasContext)

    let expandedGoal = HTArrow variable variable
        aliasGoal = HTArrow (identity variable) variable
    aliasGoalReport <- expectRight $ inhabitGeneratedPrepared
        defaultQueryOptions prepared [] "aliasGoal" aliasGoal
    expandedGoalReport <- expectRight $ inhabitGeneratedPrepared
        defaultQueryOptions prepared [] "aliasGoal" expandedGoal
    assertEqual "a goal alias changed prepared proof search"
        expandedGoalReport aliasGoalReport

    let contextualGoal = valueMethod boolType
    aliasContextReport <- expectRight $ inhabitGeneratedPrepared
        defaultQueryOptions prepared [aliasContext]
        "aliasContext" contextualGoal
    expandedContextReport <- expectRight $ inhabitGeneratedPrepared
        defaultQueryOptions prepared [expandedContext]
        "aliasContext" contextualGoal
    assertEqual "a class-argument alias changed prepared proof search"
        expandedContextReport aliasContextReport

    case inhabitGeneratedPrepared defaultQueryOptions prepared
        [context "ValueAlias" [HTCon "Identity"]]
        "partialAlias" expandedGoal of
      Left message -> assertBool
        "a partial class-argument alias lost its compatibility diagnostic"
        $ ("argument Identity of class ValueAlias: Type synonym Identity " ++
          "expects at least 1 argument(s), but got 0") `isInfixOf` message
      Right _ -> fail "a partial class-argument alias reached proof search"

    -- Shared elaboration checks kinds before expansion, but Djinn has always
    -- diagnosed saturation first within one raw HType. The compatibility
    -- preflight deliberately preserves that ordering even when another tuple
    -- element is independently ill-kinded.
    let mixedFailure = HTTuple
            [ HTCon "Identity"
            , HTApp boolType variable
            ]
    case inhabitGeneratedPrepared defaultQueryOptions prepared []
        "mixedAliasFailure" mixedFailure of
      Left message -> do
        assertBool "legacy saturation precedence was lost"
            $ "Type synonym Identity expects at least 1 argument(s), but got 0"
                `isInfixOf` message
        assertBool "an unrelated kind error replaced the saturation failure"
            $ "KindMismatch" `notElemText` message
      Right _ -> fail "an unsaturated, ill-kinded query reached proof search"

    let unprojectableFailure = HTTuple
            [ HTCon "Identity"
            , HTUnion []
            ]
    case inhabitGeneratedPrepared defaultQueryOptions prepared []
        "unprojectableAliasFailure" unprojectableFailure of
      Left message -> assertBool
        "raw conversion overtook synonym saturation"
        $ "Type synonym Identity expects at least 1 argument(s), but got 0"
            `isInfixOf` message
      Right _ -> fail
        "an unsaturated declaration-only query reached proof search"

    -- The sealed adapter walks declaration-only raw nodes even though they
    -- cannot cross the shared query boundary. This keeps the historical local
    -- saturation failure ahead of the later projection failure.
    let nestedDeclarationFailure = HTUnion
            [("Only", [HTCon "Identity"])]
    case inhabitGeneratedPrepared defaultQueryOptions prepared []
        "nestedDeclarationFailure" nestedDeclarationFailure of
      Left message -> assertBool
        "raw declaration traversal lost nested synonym saturation"
        $ "Type synonym Identity expects at least 1 argument(s), but got 0"
            `isInfixOf` message
      Right _ -> fail
        "an unsaturated alias inside HTUnion reached projection"

    -- Application heads are checked before their arguments. In particular,
    -- a partial alias keeps its arity diagnostic even if the supplied argument
    -- is not itself representable in the checked shared name vocabulary.
    let partialHeadFailure = HTApp
            (HTCon "First") (HTCon "not a type")
    case inhabitGeneratedPrepared defaultQueryOptions prepared []
        "partialHeadFailure" partialHeadFailure of
      Left message -> assertBool
        "a malformed argument overtook raw head saturation"
        $ "Type synonym First expects at least 2 argument(s), but got 1"
            `isInfixOf` message
      Right _ -> fail
        "a partial raw alias with a malformed argument reached proof search"

    -- A malformed constructor is not a candidate alias name. Ignore it only
    -- during this preflight walk so a later alias retains saturation
    -- precedence; structural conversion still owns the malformed name when
    -- no earlier compatibility rule fails.
    let malformedBeforeAlias = HTTuple
            [HTCon "not a type", HTCon "Identity"]
    case inhabitGeneratedPrepared defaultQueryOptions prepared []
        "malformedBeforeAlias" malformedBeforeAlias of
      Left message -> assertBool
        "raw name parsing stopped the saturation walk"
        $ "Type synonym Identity expects at least 1 argument(s), but got 0"
            `isInfixOf` message
      Right _ -> fail
        "a later unsaturated alias was hidden by raw name parsing"

    -- Ignoring a malformed spelling during alias lookup delegates rather than
    -- suppresses its error: with no later saturation failure, checked raw-type
    -- conversion remains the diagnostic owner.
    case inhabitGeneratedPrepared defaultQueryOptions prepared []
        "malformedOnly" (HTCon "not a type") of
      Left message -> assertBool "raw malformed-name ownership was lost"
        $ "InvalidHTypeName" `isInfixOf` message
      Right _ -> fail "a malformed raw constructor reached proof search"

    -- The joint batch sees the later partial alias during saturation, then the
    -- compatibility reporter retries each obligation to retain its precise
    -- source label. The earlier malformed goal must therefore remain the
    -- published failure rather than the context's joint-batch error.
    case inhabitGeneratedPrepared defaultQueryOptions prepared
        [context "ValueAlias" [HTCon "Identity"]]
        "firstInvalidObligation" (HTCon "not a type") of
      Left message -> do
        assertBool "an earlier malformed goal lost batch precedence"
            $ "InvalidHTypeName" `isInfixOf` message
        assertBool "a later context saturation error escaped the reporter"
            $ "Type synonym Identity" `notElemText` message
      Right _ -> fail "two invalid raw obligations reached proof search"

    -- Minimum saturation accepts extra arguments; the shared kind checker
    -- remains responsible for rejecting this proper-result synonym as a
    -- function. This is the raw PreparedEnvironment path, complementing the
    -- stable source-query boundary above.
    let overappliedIdentity = HTApp (identity boolType) voidType
    case inhabitGeneratedPrepared defaultQueryOptions prepared []
        "overappliedIdentity" overappliedIdentity of
      Left message -> do
        assertBool "raw prepared overapplication was called unsaturated"
            $ "expects at least" `notElemText` message
        assertBool "raw prepared overapplication skipped kind inference"
            $ "KindMismatch" `isInfixOf` message
      Right _ -> fail "an overapplied raw alias reached proof search"

    -- Context lookup and exact class arity precede every inspection of the
    -- goal tree, including an otherwise immediate alias-saturation failure.
    case inhabitGeneratedPrepared defaultQueryOptions prepared
        [context "ValueAlias" []]
        "classArityFirst" (HTCon "Identity") of
      Left message -> assertBool
        "goal saturation overtook class arity"
        $ "Class ValueAlias expects 1 type argument(s), but got 0"
            `isInfixOf` message
      Right _ -> fail "a bad raw class arity reached type checking"

    -- Raw callers historically resolve every context before inspecting the
    -- goal tree. Keep that observable order at the compatibility preflight:
    -- a declaration-only goal must not hide the more immediate missing-class
    -- error merely because the native worker accepts only shared source types.
    case inhabitGeneratedPrepared defaultQueryOptions prepared
        [context "MissingClass" []]
        "missingClassFirst" (HTUnion []) of
      Left message -> assertBool
        "raw goal projection overtook context lookup"
        $ "Class not found: MissingClass" `isInfixOf` message
      Right _ -> fail "a missing raw context class reached goal projection"

    targetName <- expectShownRight $
        SharedName.mkIdentifier "sharedPrecedence"
    checkedTarget <- expectShownRight $
        SharedGenerated.mkDefinitionName targetName
    missingClassName <- expectShownRight $
        SharedName.mkIdentifier "MissingClass"
    let invalidSharedGoal = SharedType.TupleType SharedName.Boxed
            [ SharedType.TypeConstructor $ sharedName "Identity"
            , SharedType.TypeVariable "A"
            ]
        missingSharedContext = Constraint missingClassName []
    case inhabitSynthesisResultPrepared defaultQueryOptions prepared
        [missingSharedContext] checkedTarget invalidSharedGoal of
      Left (DjinnQueryFailure message) -> assertBool
        "native type validation overtook context lookup"
        $ "Class not found: MissingClass" `isInfixOf` message
      result -> fail $ "native missing-class precedence changed: " ++ show result
    case inhabitSynthesisResultPrepared defaultQueryOptions prepared []
        checkedTarget invalidSharedGoal of
      Left (DjinnQueryFailure message) -> assertBool
        "native structural validation overtook synonym saturation"
        $ "Type synonym Identity expects at least 1 argument(s), but got 0"
            `isInfixOf` message
      result -> fail $ "native saturation precedence changed: " ++ show result
  where
    needle `notElemText` haystack = not $ needle `isInfixOf` haystack

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

-- The low-level translator is deliberately exposed for research clients, so
-- malformed caller-built tables must fail rather than relying on the checked
-- Environment facade to make them unreachable. Validate the whole first-
-- binding table: otherwise an unused cycle remains a latent divergence.
testRawTypeExpansionCycles :: IO ()
testRawTypeExpansionCycles = do
    let definition name parameters body = (name, (parameters, body, ()))
        apply constructor argument = HTApp (HTCon constructor) argument
        applications = foldl HTApp
        direct = [definition "A" [] $ HTCon "A"]
        mutual =
            [ definition "A" [] $ HTCon "B"
            , definition "B" [] $ HTCon "A"
            ]
        growing = [definition "A" ["a"] $
            apply "A" $ apply "[]" $ HTVar "a"]
        underSaturated = [definition "A" ["a"] $ HTCon "A"]
        overSaturated = [definition "A" [] $
            apply "A" $ HTCon "Bool"]
        higherOrder =
            [ definition "Apply" ["f", "a"] $
                HTApp (HTVar "f") (HTVar "a")
            , definition "A" ["a"] $
                HTApp (apply "Apply" $ HTCon "A") (HTVar "a")
            ]
        sourceOnly = [definition "S" ["f"] $
            HTApp (HTVar "f") (HTVar "f")]
        normalizationCycle =
            [ definition "S" ["f"] $
                HTApp (HTVar "f") (HTVar "f")
            , definition "A" [] $
                apply "S" $ HTCon "S"
            ]
        phantomCycle =
            [ definition "S" ["f"] $
                HTApp (HTVar "f") (HTVar "f")
            , definition "Phantom" ["unused"] $ HTCon "Bool"
            , definition "A" [] $
                apply "Phantom" $ apply "S" $ HTCon "S"
            ]
        wrappedNormalizationCycle =
            [ definition "S" ["f"] $
                HTApp (HTVar "f") (HTVar "f")
            , definition "Identity" ["a"] $ HTVar "a"
            , definition "A" [] $
                apply "Identity" $ apply "S" $ HTCon "S"
            ]
        lateNormalizationCycle =
            [ definition "S" ["f"] $
                HTApp (HTVar "f") (HTVar "f")
            , definition "D" [] $ HTUnion
                [("MkD", [HTCon "D", apply "S" $ HTCon "S"])]
            ]
        saturatedPrefixArrow =
            [ definition "->" [] $ HTUnion
                [("MkArrow", [HTCon "A"])]
            , definition "A" [] $
                applications (HTCon "->") [HTCon "X", HTCon "Y"]
            ]
        opaqueSourceOnly = definition "F" []
            (HTAbstract "F" $ KArrow KStar KStar) : sourceOnly
        directData = [definition "D" [] $
            HTUnion [("MkD", [HTCon "D"])]]
        mixed =
            [ definition "Alias" [] $ HTCon "D"
            , definition "D" [] $
                HTUnion [("MkD", [HTCon "Alias"])]
            ]
        opaqueDirect = definition "F" []
            (HTAbstract "F" $ KArrow KStar KStar) : direct

    assertLeftMessage "a direct synonym cycle is finite and diagnosed"
        "recursive type synonym: A"
        (hTypeToFormula direct $ HTCon "A")
    assertLeftMessage "a mutual synonym cycle has source-ordered names"
        "recursive type synonyms: A, B"
        (hTypeToFormula mutual $ HTCon "A")
    assertLeftMessage "a cycle below an opaque atom is diagnosed"
        "recursive type synonym: A"
        (hTypeToFormula opaqueDirect $
            apply "F" $ HTCon "A")
    assertLeftMessage "a growing synonym cycle cannot evade the SCC check"
        "recursive type synonym: A"
        (hTypeToFormula growing $ apply "A" $ HTVar "x")
    assertLeftMessage "an inert under-saturated raw cycle is rejected"
        "recursive type synonym: A"
        (hTypeToFormula underSaturated $ HTVar "unrelated")
    assertLeftMessage "an inert over-saturated raw cycle is rejected"
        "recursive type synonym: A"
        (hTypeToFormula overSaturated $ HTVar "unrelated")
    assertLeftMessage "higher-order substitution cannot create a hidden cycle"
        "recursive type synonym: A"
        (hTypeToFormula higherOrder $ apply "A" $ HTVar "x")
    sourceOnlyTranslate <- expectRight $
        prepareTypeFormulaTranslator sourceOnly
    assertLeftMessage "a source-created self-application cycle is diagnosed"
        "recursive type synonym expansion: S, S"
        (sourceOnlyTranslate $ apply "S" $ HTCon "S")
    opaqueSourceOnlyTranslate <- expectRight $
        prepareTypeFormulaTranslator opaqueSourceOnly
    assertLeftMessage "an opaque atom cannot hide a source-created cycle"
        "recursive type synonym expansion: S, S"
        (opaqueSourceOnlyTranslate $
            apply "F" $ apply "S" $ HTCon "S")
    assertLeftMessage "whole-table alias normalization is itself finite"
        "type definition A: recursive type synonym expansion: S, S"
        (hTypeToFormula normalizationCycle $ HTVar "unrelated")
    assertEqual "a phantom alias does not inspect its cyclic argument"
        (Right $ PVar $ Symbol "unrelated")
        (hTypeToFormula phantomCycle $ HTVar "unrelated")
    assertLeftMessage "a transparent wrapper retains the argument cycle path"
        "type definition A: recursive type synonym expansion: S, S"
        (hTypeToFormula wrappedNormalizationCycle $ HTVar "unrelated")
    assertLeftMessage "a late expansion error precedes a known structural edge"
        "type definition D: recursive type synonym expansion: S, S"
        (hTypeToFormula lateNormalizationCycle $ HTVar "unrelated")
    assertEqual "a saturated prefix arrow contributes no raw constructor edge"
        (Right $ PVar $ Symbol "unrelated")
        (hTypeToFormula saturatedPrefixArrow $ HTVar "unrelated")
    assertLeftMessage "an unused cycle invalidates the whole supplied table"
        "recursive type synonym: A"
        (hTypeToFormula direct $ HTVar "unrelated")
    assertLeftMessage "a directly recursive datatype is diagnosed"
        "recursive type definition: D"
        (hTypeToFormula directData $ HTCon "D")
    assertLeftMessage "an alias-datatype cycle is diagnosed after expansion"
        "recursive type definition: D"
        (hTypeToFormula mixed $ HTCon "Alias")

    let identity = [definition "Id" ["a"] $ HTVar "a"]
    assertEqual "finite repetition of one acyclic alias is not a cycle"
        (Right $ PVar $ Symbol "x")
        (hTypeToFormula identity $
            apply "Id" $ apply "Id" $ HTVar "x")

    -- A partial datatype supplied as a higher-kinded argument may be
    -- saturated inside another expansion. Distinct finite source occurrences
    -- and distinct instances of one cached definition body must not be
    -- mistaken for recursive use of the same occurrence.
    let higherKindedDefinitions =
            [ definition "D" ["f", "a"] $ HTUnion
                [("MkD", [HTApp (HTVar "f") (HTVar "a")])]
            , definition "Id" ["a"] $ HTVar "a"
            , definition "F" [] $
                HTAbstract "F" $ KArrow KStar KStar
            ]
        d arguments = applications (HTCon "D") arguments
        nestedD formula = Disj [(ConsDesc "MkD" 1, Conj [formula])]
    assertEqual "a finite nested partial datatype keeps source provenance"
        (Right $ nestedD $ nestedD $ PVar $ Symbol "F x")
        (hTypeToFormula higherKindedDefinitions $
            d [d [HTCon "F"], HTVar "x"])
    assertEqual "cached body occurrences are fresh for each finite instance"
        (Right $ nestedD $ nestedD $ nestedD $ PVar $ Symbol "x")
        (hTypeToFormula higherKindedDefinitions $
            d [d [HTCon "Id"], d [HTCon "Id", HTVar "x"]])

    let sourceLoopDefinitions =
            [ definition "S" ["f"] $
                HTApp (HTVar "f") (HTVar "f")
            , definition "D" ["f", "a"] $ HTUnion
                [("MkD", [HTApp (HTVar "f") (HTVar "a")])]
            ]
    assertLeftMessage "partial-application provenance still detects a loop"
        "recursive type definition expansion: D, S, D"
        (hTypeToFormula sourceLoopDefinitions $
            apply "S" $ d [HTCon "S"])

    -- Arbitrary raw inputs are not kind-checked and can encode untyped
    -- normalization. Re-entering one active source occurrence is therefore a
    -- deliberately conservative boundary: it rejects a finite but ill-kinded
    -- rewrite as well as non-repeating growth before either can diverge.
    let finiteReentryDefinitions =
            [ definition "A" ["f", "x"] $
                HTApp (HTApp (HTVar "f") (HTCon "C")) (HTVar "x")
            , definition "S" ["f", "x"] $
                HTApp (HTApp (HTVar "f") (HTVar "f")) (HTVar "x")
            ]
        growingSourceDefinitions =
            [ definition "S" ["f", "x"] $
                HTApp
                    (HTApp (HTVar "f") (HTVar "f"))
                    (apply "F" $ HTVar "x")
            ]
    assertLeftMessage "raw active-occurrence re-entry is conservative"
        "recursive type synonym expansion: A, A"
        (hTypeToFormula finiteReentryDefinitions $
            applications (HTCon "S") [HTCon "A", HTVar "y"])
    assertLeftMessage "growing untyped self-application is rejected early"
        "recursive type synonym expansion: S, S"
        (hTypeToFormula growingSourceDefinitions $
            applications (HTCon "S") [HTCon "S", HTVar "x"])

    -- Substitution may supply @(->)@ in function position, either directly or
    -- already applied to its domain. Both spellings must retain 'hTApp' arrow
    -- canonicalization while carrying argument-origin markers.
    let applyDefinitions =
            [ definition "Apply" ["f", "a"] $
                HTApp (HTVar "f") (HTVar "a")
            , definition "Apply2" ["f", "a", "b"] $
                HTApp (HTApp (HTVar "f") (HTVar "a")) (HTVar "b")
            ]
        argument = HTVar "x"
        result = HTVar "y"
        arrow = HTArrow argument result
        partialArrow = HTApp (HTCon "->") argument
        rawPrefixArrow = HTApp partialArrow result
    assertEqual "a direct raw prefix arrow retains its historical atom"
        (Right $ PVar $ Symbol "x -> y")
        (hTypeToFormula applyDefinitions rawPrefixArrow)
    assertEqual "a marked partial arrow remains a logical implication"
        (hTypeToFormula applyDefinitions arrow)
        (hTypeToFormula applyDefinitions $
            applications (HTCon "Apply") [partialArrow, result])
    assertEqual "a marked arrow constructor remains a logical implication"
        (hTypeToFormula applyDefinitions arrow)
        (hTypeToFormula applyDefinitions $
            applications (HTCon "Apply2")
                [HTCon "->", argument, result])

    -- Both expansion and cycle analysis follow association-list lookup: the
    -- unreachable duplicate must not poison a valid first definition.
    let firstWins =
            [ definition "A" [] $ HTCon "Bool"
            , definition "A" [] $ HTCon "A"
            ]
    assertEqual "cycle validation honors first-binding lookup semantics"
        (Right $ PVar $ Symbol "Bool")
        (hTypeToFormula firstWins $ HTCon "A")

-- Raw definition parameters predate checked declaration validation. Preserve
-- their historical first-binding behavior while covering an arity wide enough
-- to exercise the logarithmic substitution index rather than list position.
testRawExpansionSubstitution :: IO ()
testRawExpansionSubstitution = do
    let definition name parameters body = (name, (parameters, body, ()))
        applications = foldl HTApp
        duplicateParameters =
            [ definition "F" [] $
                HTAbstract "F" $ KArrow KStar KStar
            , definition "PickFirst" ["a", "a"] $ HTVar "a"
            ]
        duplicateArgument = applications (HTCon "PickFirst")
            [HTVar "x", HTVar "y"]
    assertEqual "duplicate raw parameters retain their first argument"
        (Right $ PVar $ Symbol "F x")
        (hTypeToFormula duplicateParameters $
            HTApp (HTCon "F") duplicateArgument)

    let indices = [0 .. 127] :: [Int]
        parameters = map (("p" ++) . show) indices
        argumentNames = map (("x" ++) . show) indices
        wideDefinition = [definition "Wide" parameters $
            HTTuple $ map HTVar $ reverse parameters]
        wideQuery = applications (HTCon "Wide") $
            map HTVar argumentNames
        expected = Conj $ map (PVar . Symbol) $ reverse argumentNames
    assertEqual "wide raw substitution preserves every parameter position"
        (Right expected)
        (hTypeToFormula wideDefinition wideQuery)

-- Raw Environment constructors remain available to explicit low-level
-- clients. Their checked preparation must apply the same expand-first
-- recursion policy as neutral Djinn sessions: real data cycles fail, while a
-- phantom alias can erase a merely apparent surface edge.
testRawEnvironmentRecursionPreflight :: IO ()
testRawEnvironmentRecursionPreflight = do
    let direct = RawEnvironment.Environment
            [ ("D", ([], HTUnion [("MkD", [HTCon "D"])], KStar)) ]
            [] []
        mixed = RawEnvironment.Environment
            [ ("Alias", ([], HTCon "D", KStar))
            , ("D", ([], HTUnion [("MkD", [HTCon "Alias"])], KStar))
            ] [] []
        phantomUse = HTApp (HTCon "Phantom") (HTCon "D")
        phantom = RawEnvironment.Environment
            [ ("Bool", ([], HTAbstract "Bool" KStar, KStar))
            , ("Phantom", (["a"], HTCon "Bool", KArrow KStar KStar))
            , ("D", ([], HTUnion [("MkD", [phantomUse])], KStar))
            ] [] []
        higherKinded = RawEnvironment.Environment
            [ ("F", ([], HTAbstract "F" $ KArrow KStar KStar,
                KArrow KStar KStar))
            , ("D", (["f", "a"],
                HTUnion [("MkD", [HTApp (HTVar "f") (HTVar "a")])],
                KArrow (KArrow KStar KStar) $ KArrow KStar KStar))
            ] [] []

    assertRecursiveDataRejection "direct raw data recursion" "D" direct
    assertRecursiveDataRejection "mixed raw alias/data recursion" "D" mixed
    case prepareEnvironment phantom of
        Left failure -> fail $ "phantom alias invented raw recursion: " ++
            show failure
        Right _ -> return ()
    preparedHigherKinded <- case prepareEnvironment higherKinded of
        Left failure -> fail $ "valid higher-kinded data was rejected: " ++
            show failure
        Right prepared -> return prepared
    let applyMany = foldl HTApp
        partialD = applyMany (HTCon "D") [HTCon "F"]
        nestedGoal = applyMany (HTCon "D") [partialD, HTVar "x"]
    case inhabitGeneratedPrepared defaultQueryOptions
            preparedHigherKinded [] "nested" nestedGoal of
        Left failure -> fail $
            "cached formula translation rejected finite nesting: " ++ failure
        Right _ -> return ()
  where
    assertRecursiveDataRejection description expected environment =
        case prepareEnvironment environment of
            Left (RecursiveSynthesisDataTypes names) ->
                assertEqual description [sharedName expected] names
            Left failure -> fail $ description ++
                " produced the wrong failure: " ++ show failure
            Right _ -> fail $ description ++ " reached a prepared environment"

-- The recursion preflight may retain synonym declarations verbatim because
-- the prepared synonym table owns whole-table validation. Keep that ownership
-- explicit: even an otherwise unused bad synonym must fail in the first phase.
testPreparedSynonymValidationOwnership :: IO ()
testPreparedSynonymValidationOwnership = do
    let proper = KStar
        higherKind = KArrow (KArrow proper proper) proper
        pairKind = KArrow proper $ KArrow proper proper
        partialPair = HTApp (HTCon "Pair") (HTCon "Bool")
        environment = RawEnvironment.Environment
            [ ("Bool", ([], HTAbstract "Bool" proper, proper))
            , ("Higher", ([], HTAbstract "Higher" higherKind, higherKind))
            , ("Pair", (["a", "b"],
                HTTuple [HTVar "a", HTVar "b"], pairKind))
            , ("UnusedBad", ([],
                HTApp (HTCon "Higher") partialPair, proper))
            ] [] []
    case prepareEnvironment environment of
        Left (InvalidSynthesisTypeSynonyms
                (SharedTypeSynonym.UnsaturatedTypeSynonym name expected supplied)) -> do
            assertEqual "the prepared table identified the unused synonym"
                (sharedName "Pair") name
            assertEqual "the prepared table retained the declared arity"
                2 expected
            assertEqual "the prepared table retained the supplied arity"
                1 supplied
        Left failure -> fail $ "unused bad synonym produced the wrong failure: " ++
            show failure
        Right _ -> fail "an unused bad synonym reached recursion preflight"

-- The shared expansion error carries an index and subject, but the exported
-- raw Djinn API historically projects both synonym phases to this constructor.
testOperationalSynonymCompatibilityError :: IO ()
testOperationalSynonymCompatibilityError = do
    let proper = KStar
        pairKind = KArrow proper $ KArrow proper proper
        higherKind = KArrow pairKind proper
        environment = RawEnvironment.Environment
            [ ("Pair", (["a", "b"],
                HTTuple [HTVar "a", HTVar "b"], pairKind))
            , ("Higher", ([], HTAbstract "Higher" higherKind, higherKind))
            ]
            [("bad", HTApp (HTCon "Higher") $ HTCon "Pair")] []
    case prepareEnvironment environment of
        Left (InvalidSynthesisTypeSynonyms
                (SharedTypeSynonym.UnsaturatedTypeSynonym
                  name expected supplied)) -> do
            assertEqual "the alias identity is retained" (sharedName "Pair") name
            assertEqual "the declared arity is retained" 2 expected
            assertEqual "the supplied arity is retained" 0 supplied
        Left failure -> fail $
            "operational alias produced the wrong raw failure: " ++ show failure
        Right _ -> fail "an unsaturated value alias reached Djinn sealing"

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
            assertRendered "structural false should render as an empty case"
                "eliminate a = case a of {}" "eliminate" proof
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

testCheckedProofSearchEnvironment :: IO ()
testCheckedProofSearchEnvironment = do
    let duplicate = Symbol "given"
        environment = [(duplicate, atomA), (duplicate, atomB)]
        mode = defaultSearchMode False
        checked = fmap (const ()) $
            proveWithModeChecked mode environment atomA
    assertEqual "search and proof checking share the duplicate diagnostic"
        (checkProof environment atomA $ Var duplicate)
        checked
    assertEqual "the checked boundary reports the ambiguous identity"
        (Left "duplicate proof identity in environment: given")
        checked
    case proveWithModeChecked mode [(duplicate, atomA)] atomA of
        Left failure -> fail $
            "a unique proof environment was rejected: " ++ failure
        Right outcome ->
            assertEqual "checked search preserves ordinary proof results"
                [Var duplicate] (searchProofs outcome)
    -- Keep the historical unchecked API source- and behavior-compatible for
    -- callers that deliberately own its association-list invariant.
    assertEqual "historical search retains first-match compatibility"
        [Var duplicate]
        (prove False environment atomA)

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

testCsplitResidualHandlerLambda :: IO ()
testCsplitResidualHandlerLambda = do
    let field = Symbol "field"
        argument = Symbol "argument"
        handler = Lam field $ Lam argument $
            applys (Var $ Symbol "combine") [Var field, Var argument]
        term = applys (Csplit 1)
            [handler, Var $ Symbol "payload"]
    assertRendered "Csplit consumes only its requested handler prefix"
        ("f a =\n" ++
          "  case payload of\n" ++
          "  b -> combine b a")
        "f" term

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

    let beyondMachineCount = fromIntegral (maxBound :: Int) + 1 :: Natural
    assertBool "candidate ordering retains binder counts beyond Int" $
        DjinnCandidateDetails 0 beyondMachineCount <
            DjinnCandidateDetails 0 (beyondMachineCount + 1)

    goal <- either fail return $ parseHType "T a b -> (a, b)"
    environment <- either fail return $
        declare (DataType "T" ["a", "b"]
            [("C", [HTVar "a", HTVar "b"])]) emptyEnvironment
    let options = defaultQueryOptions {
            optionAlternatives = True,
            optionSorted = False
            }
        oneCandidate = options {optionCutoff = 1}
        allCandidates = options {optionCutoff = 2}
    limited <- either fail return $
        inhabitGenerated oneCandidate environment [] "f" goal
    assertEqual "the first canonical candidate survives the cutoff"
        1 (length $ generatedReportCandidates limited)
    assertEqual "stopping before another proof is a candidate truncation"
        (SharedSearch.truncated SharedSearch.CandidateLimitReached)
        (generatedReportCompletion limited)
    complete <- either fail return $
        inhabitGenerated allCandidates environment [] "f" goal
    assertEqual "exhausting both proofs completes the canonical search"
        SharedSearch.Finished (generatedReportCompletion complete)
    maximumCutoff <- either fail return $ inhabitGenerated
        options {optionCutoff = maxBound} environment [] "f" goal
    assertEqual "the largest valid cutoff does not overflow its witness bound"
        SharedSearch.Finished (generatedReportCompletion maximumCutoff)
    case generatedReportCandidates complete of
        [candidate] -> do
            assertEqual "equivalent proofs are de-duplicated canonically"
                (Right expected) $ SharedGenerated.renderFunctionClause
                    (SharedGenerated.defaultRenderOptions id)
                    (SharedCandidate.candidateOutput candidate)
            assertEqual "Djinn discharges every candidate constraint"
                [] $ SharedCandidate.candidateResidualConstraints candidate
            let details = SharedCandidate.candidateDetails candidate
            assertEqual "the candidate uses every generated binder"
                0 $ djinnUnusedBinderFraction details
            assertEqual "ranking accounts for argument and case binders"
                3 $ djinnBinderCount details
        candidates -> fail $ "expected one canonical candidate, got " ++
            show (length candidates)
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

    -- Exercise a substantial collision chain without a machine-sized prime
    -- count or rebuilding every candidate with 'replicate'. Candidate
    -- spellings are extended directly, so the next name stays exact.
    wideCaptureEnvironment <- expectRight $
        declare (ClassDecl "WideCapture" ["a"]
            [("wideCapture", HTApp (HTVar "f") (HTVar "a"))])
            multiCaptureEnvironment
    let primeNames = iterate (++ "'") "f"
        occupiedVariables = take 65 primeNames
        freshVariable = primeNames !! 65
        wideArgument = foldr
            (\variable rest -> HTTuple [HTVar variable, rest])
            (HTCon "()") occupiedVariables
    assertEqual "long prime collision chains retain the next exact spelling"
        (Right [("wideCapture", HTApp (HTVar freshVariable) wideArgument)])
        (resolveContext wideCaptureEnvironment $
            context "WideCapture" [wideArgument])

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
    let numberedCollisionEnvironment = prepareProofEnvironment target
            [ (Symbol "$assumption1", atomA)
            , (Symbol "$assumption2", atomB)
            ]
    assertEqual "internal proof identities skip occupied numeric suffixes"
        [Symbol "$assumption3", Symbol "$assumption4"]
        (map fst $ proofBindings numberedCollisionEnvironment)
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

testWideProofMetas :: IO ()
testWideProofMetas = do
    let arity = 512
        elements = replicate arity true
        handler = foldr (:->) true elements
        formula = handler :-> Conj elements :-> true
    assertEqual "every wide product component keeps a distinct metavariable"
        (Right ()) (checkProof [] formula $ Csplit arity)

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
    translate <- expectRight $ prepareTypeFormulaTranslator definitions
    emptyA <- expectRight $ translate $ HTCon "EmptyA"
    emptyB <- expectRight $ translate $ HTCon "EmptyB"
    aliasA <- expectRight $ translate $ HTCon "AliasA"
    emptyOfFlag <- expectRight $ translate $
        HTApp (HTCon "EmptyOf") (HTCon "Flag")
    emptyOfAlias <- expectRight $ translate $
        HTApp (HTCon "EmptyOf") (HTCon "FlagAlias")
    let cast = emptyA :-> emptyB
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
            assertEqual "empty elimination should remain structural"
                "cast a = case a of {}" rendered
        proofs -> fail $ "expected one explicit empty elimination, got " ++
            show proofs
    assertLeft "a direct identity cast between empty types must be rejected"
        (checkProof [] cast $ Lam (Symbol "x") (Var $ Symbol "x"))

    -- Empty elimination used to become a magic free variable named @void@.
    -- That made @void@ unusable as a target and silently captured an unrelated
    -- assumption with the same spelling.  A structural empty case has neither
    -- collision.
    let emptyGoal = HTArrow (HTCon "Void") (HTVar "a")
    voidTarget <- expectRight $ inhabit defaultQueryOptions
        standardEnvironment [] "void" emptyGoal
    assertEqual "void remains a legal generated definition name"
        (Realized ["void a = case a of {}"])
        (reportOutcome voidTarget)
    collidingEnvironment <- expectRight $
        declare (Function "void" $ HTCon "Bool") standardEnvironment
    collisionSafe <- expectRight $ inhabit defaultQueryOptions
        collidingEnvironment [] "eliminate" emptyGoal
    assertEqual "an unrelated void assumption cannot capture empty elimination"
        (Realized ["eliminate a = case a of {}"])
        (reportOutcome collisionSafe)

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
        invalidTypes =
            [ ("Alias", (["a"], HTVar "a", KStar))
            , ("Broken", ([], HTCon "Alias", KStar))
            ]
        invalidAxioms =
            [ ("first", HTCon "MissingFirst")
            , ("second", HTCon "MissingSecond")
            ]
        invalidClass =
            ("BrokenClass",
                ([], [ ("firstMethod", HTCon "MissingFirst")
                     , ("secondMethod", HTCon "MissingSecond")
                     ]))
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
    -- Batch preparation must not change the transactional validation order:
    -- types precede values, values precede classes, and methods retain source
    -- order within their class.
    assertLeftContains "type errors still precede dependent declarations"
        "type environment: Type synonym Alias"
        (validateEnvironment invalidTypes invalidAxioms [invalidClass])
    assertLeftContains "axioms still precede classes in source order"
        "axiom first:"
        (validateEnvironment [] invalidAxioms [invalidClass])
    assertLeftContains "class methods still retain source order"
        "method firstMethod of class BrokenClass:"
        (validateEnvironment [] [] [invalidClass])

testPrintedValueNamespace :: IO ()
testPrintedValueNamespace = do
    let bool = HTCon "Bool"
        selectable = ClassDecl "Selectable" ["a"]
            [("select", HTVar "a")]
        sharedConflict = "Value name is already declared: select"
        rawConflict =
            "Function assumption select conflicts with method select " ++
            "of class Selectable"

    functionFirst <- expectRight $
        declare (Function "select" bool) standardEnvironment
    assertLeftMessage "a later class method must not shadow an assumption"
        sharedConflict (declare selectable functionFirst)

    classFirst <- expectRight $ declare selectable standardEnvironment
    assertLeftMessage "a later assumption must not shadow a class selector"
        sharedConflict (declare (Function "select" bool) classFirst)
    assertLeftMessage "operator selectors share the same printed namespace"
        "Value name is already declared: (==)"
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
        rawConflict
        (validateEnvironment [] [("select", HTVar "a")]
            [("Selectable", ([("a", KStar)], [("select", HTVar "a")]))])
    assertLeftContains "raw validation rejects duplicate assumptions"
        "Duplicate function assumption: duplicate"
        (validateEnvironment []
            [("duplicate", bool), ("duplicate", bool)] [])
    assertLeftContains "duplicate precedence follows the first occurrence"
        "Duplicate function assumption: first"
        (validateEnvironment []
            [ ("first", bool)
            , ("second", bool)
            , ("second", bool)
            , ("first", bool)
            ] [])
    assertLeftContains "raw validation rejects duplicate class owners"
        "Duplicate class: Selectable"
        (validateEnvironment [] []
            [ ("Selectable", ([("a", KStar)], []))
            , ("Selectable", ([("a", KStar)], []))
            ])

testTrustedUnitDeclaration :: IO ()
testTrustedUnitDeclaration = do
    let exactUnit = ([], HTUnion [("()", [])], KStar)
        declarationFailure = "() is a built-in type and cannot be declared"
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
    assertLeftMessage "a unit-named synonym is rejected" declarationFailure
        (declare (TypeSynonym "()" [] $ HTCon "Bool") standardEnvironment)
    assertLeftMessage "a unit-named abstract type is rejected" declarationFailure
        (declare (AbstractType "()" KStar) standardEnvironment)
    assertLeftMessage "a unit-named data type is rejected" declarationFailure
        (declare (DataType "()" [] []) standardEnvironment)
    assertLeftMessage "even the exact built-in data declaration is private"
        declarationFailure
        (declare (DataType "()" [] [("()", [])]) emptyEnvironment)
    assertLeftMessage "a unit-named class is rejected" declarationFailure
        (declare (ClassDecl "()" [] []) standardEnvironment)
    assertLeftMessage "the unit constructor cannot belong to another type"
        declarationFailure
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

testGeneratedClauseBoundary :: IO ()
testGeneratedClauseBoundary = do
    let duplicateTupleBinder = DjinnGenerated.HClause "badTuple"
            [DjinnGenerated.HPTuple
                [DjinnGenerated.HPVar "value", DjinnGenerated.HPVar "value"]]
            (DjinnGenerated.HEVar "value")
        duplicateAsBinder = DjinnGenerated.HClause "badAs"
            [DjinnGenerated.HPAt "value" $ DjinnGenerated.HPVar "value"]
            (DjinnGenerated.HEVar "value")
        invalidDefinition = DjinnGenerated.HClause "Result" []
            (DjinnGenerated.HEVar "value")
        invalidDefinitionAndScope = DjinnGenerated.HClause "Result"
            [DjinnGenerated.HPVar "value", DjinnGenerated.HPVar "value"]
            (DjinnGenerated.HEVar "value")
        lowercaseConstructor = DjinnGenerated.HClause "badConstructor" []
            (DjinnGenerated.HECon "lower")
        uppercaseFreeValue = DjinnGenerated.HClause "badValue" []
            (DjinnGenerated.HEVar "Upper")
    assertLeftContains "tuple patterns cannot bind a local twice"
        "DuplicatePatternBinder \"value\""
        (DjinnGenerated.toGeneratedClause duplicateTupleBinder)
    assertLeftContains "as-patterns cannot bind a local twice"
        "DuplicatePatternBinder \"value\""
        (DjinnGenerated.toGeneratedClause duplicateAsBinder)
    assertLeftContains "raw clauses reject invalid definition names"
        "InvalidFunctionName Result"
        (DjinnGenerated.toGeneratedClause invalidDefinition)
    assertLeftContains "scope errors retain precedence over definition syntax"
        "DuplicatePatternBinder \"value\""
        (DjinnGenerated.toGeneratedClause invalidDefinitionAndScope)
    assertLeftContains "legacy constructor nodes retain their lexical role"
        "expected a constructor-like name"
        (DjinnGenerated.toGeneratedClause lowercaseConstructor)
    assertLeftContains "legacy free-value nodes retain their lexical role"
        "expected a variable-like name"
        (DjinnGenerated.toGeneratedClause uppercaseFreeValue)
    assertLeftContains "raw proof values cannot become constructors by spelling"
        "expected a variable-like name"
        (termToHExpr $ Var $ Symbol "Upper")
    assertLeftContains
        "raw proof constructors cannot become values by spelling"
        "expected a constructor-like name"
        (termToHExpr $
            Apply (Cinj (ConsDesc "lower" 0) 0) (Ctuple 0))

    checkedProjection <- expectShownRight $ SharedGenerated.mkDefinitionName
        $ sharedName "projected"
    let just = sharedName "Just"
        sharedProjection = SharedGenerated.FunctionClause checkedProjection
            [SharedGenerated.As "whole" $
                SharedGenerated.Constructor just
                    [SharedGenerated.Bind "value"]]
            (SharedGenerated.Case (SharedGenerated.Local "whole")
                [ ( SharedGenerated.Constructor just
                        [SharedGenerated.Bind "nested"]
                  , SharedGenerated.Tuple
                        [ SharedGenerated.Local "value"
                        , SharedGenerated.Local "nested"
                        ]
                  )
                ])
    legacyProjection <- either fail return $
        DjinnGenerated.fromGeneratedClause sharedProjection
    assertEqual "legacy generated syntax is an on-demand lossless view"
        (Right sharedProjection)
        (DjinnGenerated.toGeneratedClauseWithName
            checkedProjection legacyProjection)
    assertLeftContains "legacy expressions reject shared holes explicitly"
        "cannot represent generated holes"
        (DjinnGenerated.fromGeneratedExpression $
            SharedGenerated.Hole "missing")
    assertLeftContains "legacy expressions reject shared lets explicitly"
        "cannot represent generated lets"
        (DjinnGenerated.fromGeneratedExpression $
            SharedGenerated.Let (SharedGenerated.Bind "bound")
                (SharedGenerated.Tuple [])
                (SharedGenerated.Local "bound"))

    goals <- mapM (either fail return . parseHType)
        ["a -> a", "a -> b -> a"]
    reports <- mapM
        (either fail return .
            inhabitGenerated defaultQueryOptions emptyEnvironment [] "generated")
        goals
    let candidates = concatMap generatedReportCandidates reports
    assertEqual "representative proofs should reach the generated boundary"
        2 (length candidates)
    mapM_ validateCandidate candidates
  where
    validateCandidate candidate = do
        let clause = SharedCandidate.candidateOutput candidate
        assertEqual "generated clause retains its checked definition"
            "generated" $ SharedGenerated.definitionSpelling
                $ SharedGenerated.clauseName clause
        assertEqual "generated candidate has valid lexical scope"
            (Right ()) $ SharedGenerated.validateFunctionClauseScope clause
        assertEqual "generated candidate has valid renderer-independent syntax"
            (Right ()) $ SharedGenerated.validateFunctionClauseSyntax
                SharedGenerated.FullyQualified clause

testKeywordTokens :: IO ()
testKeywordTokens = do
    mapM_ rejectGluedKeyword
        [ ("type", "Foo")
        , ("data", "Bar")
        , ("class", "Quux")
        , ("where", "identity")
        , ("instance", "Quux")
        ]
    assertParses "white space terminates an alphabetic keyword"
        (skeyword "type" >> pConId) "type Foo"
    assertParses "punctuation terminates an alphabetic keyword"
        (skeyword "instance" >> pParen pHConstraint)
        "instance(Eq a)"
    assertParses "a stripped line comment terminates a keyword"
        (skeyword "where") (stripLineComments "where-- trailing\n")
    assertDoesNotParse "the keyword helper is not for symbolic tokens"
        (skeyword "::") "::"
    assertParses "ordinary symbolic tokens retain boundary-free matching"
        (sstring "::") "::"
  where
    rejectGluedKeyword (keyword, suffix) =
        assertDoesNotParse
            (keyword ++ " must not consume the prefix of " ++
                keyword ++ suffix)
            (skeyword keyword) (keyword ++ suffix)

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
    assertEqual "proof symbols keep unqualified operators bare"
        "⊕" (renderProofSymbolName $ sharedName "(⊕)")
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
    assertEqual "proof symbols keep qualified operators canonical"
        "Data.(+)" (renderProofSymbolName $ sharedName "Data.(+)")
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

assertRight :: String -> Either String a -> IO ()
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

assertParses :: String -> ReadP a -> String -> IO ()
assertParses message parser input =
    assertBool message $ not $ null $ parseFully parser input

assertDoesNotParse :: String -> ReadP a -> String -> IO ()
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
