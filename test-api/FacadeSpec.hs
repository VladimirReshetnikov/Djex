module FacadeSpec (facadeTests) where

import Data.Either (isRight)
import qualified Data.Set as Set
import Data.Void (Void)
import Data.Word (Word8)

import ExferencePatternImports (patternViewsRoundTrip)
import Language.Haskell.Djex
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

type FacadeBehavioralDomain = ()
type FacadeRawArtifact = ()

facadeTests :: TestTree
facadeTests = testGroup "public Djex facade"
  [ testCase "enumerates both checked backends" $
      map backend availableBackends @?= [DjinnBackend, ExferenceBackend]
  , testCase "exports the shared name vocabulary" $
      assertBool "qualified name was rejected" $
        isRight $ parseName "Data.Function.fix"
  , testCase "exports shared qualification helpers" $ do
      value <- expectRight $ parseName "Data.Function.fix"
      renderNamePrefix FullyQualified value @?= "Data.Function.fix"
      emittedIdentifier Unqualified value @?= Just "fix"
  , testCase "exports shared collection observations" $
      ( multiplicityOf "value" (summarizeDuplicates ["value", "value"])
      , firstDuplicate ["first", "second", "first"]
      , distinctOn fst
          ([ ("first", 1), ("first", 2), ("second", 3) ]
            :: [(String, Int)])
      , firstPresent [Nothing, Just "present"]
      , maximumPresent [Nothing, Just (3 :: Int), Just 5]
      ) @?=
        ( OccursMultipleTimes
        , Just "first"
        , [("first", 1), ("second", 3)]
        , Just "present"
        , Just 5
        )
  , testCase "exports exact shared synthesis observations" $ do
      let observations :: ObservationCounts SynthesisMetric
          observations = recordObservations EngineStateVisited 2
            $ recordObservation RankNPlanCompiled noObservations
          snapshot = observationSnapshot observations "candidate"
      observationEntries observations @?=
        [(RankNPlanCompiled, 1), (EngineStateVisited, 2)]
      synthesisMetricCode EngineStateVisited @?= "engine-state-visited"
      snapshotValue snapshot @?= "candidate"
      snapshotObservations snapshot @?= observations
  , testCase "exports opaque fingerprints and solver-neutral observations" $ do
      let solver = UnknownObservation "timeout"
            :: SolverObservation String Int String
          behavior = BehaviorBoundedObservation (7 :: Int)
            :: BehavioralObservation String Bool Int Char
      inspectFingerprint `seq` pure ()
      solverObservationStatus solver @?= SolverUnknown
      solver @?= UnknownObservation "timeout"
      behavioralObservationStatus behavior @?= BehaviorValidatedWithin
      behavior @?= BehaviorBoundedObservation 7
  , testCase "exports bounded raw behavioral problem observations" $ do
      let limits = mkRawArtifactLimits 4 3
      raw <- expectRight
        (mkBoundedRawArtifact limits [0x72, 0x61, 0x77] [1, 2, 3]
          :: Either RawArtifactLimitError
              (BoundedRawArtifact FacadeRawArtifact))
      rawArtifactFormatByteLimit limits @?= 4
      rawArtifactPayloadByteLimit limits @?= 3
      boundedRawArtifactFormat raw @?= [0x72, 0x61, 0x77]
      boundedRawArtifactBytes raw @?= [1, 2, 3]
      mkBoundedRawArtifact limits [0, 1, 2, 3, 4] [] @?=
        (Left $ RawArtifactLimitExceeded RawArtifactFormat 4 5
          :: Either RawArtifactLimitError
              (BoundedRawArtifact FacadeRawArtifact))
      inspectBehavioralProblem `seq` inspectBehavioralEvidence `seq` pure ()
      [ RawSolverModelHint
        , RawSolverUnsatRelativeToEncoding
        , RawSolverUnknown
        , RawBehaviorEstablishedClaim
        , RawBehaviorCounterexampleClaim
        , RawBehaviorBoundedValidation
        , RawBehaviorUnknown
        ] @?= [minBound .. maxBound]
      (HeuristicRankingOnly :: RawObservationUse) @?= minBound
  , testCase "exports the finite list-spine length vocabulary" $ do
      finiteListSpineLengthDomainTag @?=
        map (fromIntegral . fromEnum) "finite-list-spine-length/v1"
      mkLengthLimits defaultLengthLimitSource @?= Right defaultLengthLimits
      map ($ defaultLengthLimits)
          [ lengthTypeNodeLimit
          , lengthContractInputLimit
          , lengthSyntaxNodeLimit
          , lengthFormulaClauseLimit
          , lengthCollectionWidthLimit
          , lengthProviderSummaryLimit
          , lengthProviderArgumentLimit
          , lengthLiteralBitLimit
          , lengthFingerprintByteLimit
          ] @?= [4096, 8, 1024, 32, 64, 256, 16, 256, 65536]
      mkLengthEvaluationLimits defaultLengthEvaluationLimitSource @?=
        Right defaultLengthEvaluationLimits
      map ($ defaultLengthEvaluationLimits)
          [ lengthAssignmentValueBitLimit
          , lengthIntermediateValueBitLimit
          ] @?= [4096, 4096]

      providerName <- expectRight $ mkIdentifier "lengthProvider"
      let input = LengthVariable (LengthInput 0)
          result = LengthVariable LengthResult
          condition = LengthAll
            [ LengthTruth True
            , LengthNot (LengthTruth False)
            , LengthEqual
                (LengthSum [input, LengthLiteral 1])
                (LengthMaximum input $ LengthLiteral 1)
            , LengthAtMost input (LengthScale 2 input)
            ]
          transfer = LengthIf condition
            (LengthMaximum
              (LengthScale 2 input)
              (LengthMonus result input))
            (LengthMinimum input (LengthLiteral 0))
          contract = LengthContractSource condition
            (LengthEqual result transfer)
          payload = TupleType Boxed [] :: Type String
          listType = TypeApplication (TypeConstructor listName) payload
          target = FunctionType listType listType
          providerScheme = ForallType ["element"] [] $ FunctionType
            (TypeApplication
              (TypeConstructor listName)
              (TypeVariable "element"))
            (TypeApplication
              (TypeConstructor listName)
              (TypeVariable "element"))
          provider :: LengthProviderSummarySource String
          provider = AssumedProviderSummary
            providerName
            providerScheme
            [LengthSpineArgument]
            (LengthVariable $ LengthProviderArgument 0)
      sourceInventory <- expectRight
        (mkInventory ClosedKindInventory
          [ ValueDeclaration
              $ ValueSignature () providerName providerScheme
          ] :: Either
              (InventoryError String Void)
              (Inventory String ()))
      checkedContext <- expectRight $ sealLengthContext
        defaultLengthLimits sourceInventory BuiltinListSpine
      checkedContract <- expectRight $ sealLengthContractInContext
        defaultLengthLimits checkedContext target contract
      checkedProviders <- expectRight $ sealLengthProviderInventoryInContext
        defaultLengthLimits checkedContext [provider]
      lengthContextInventory checkedContext @?= sourceInventory
      let spineModel = lengthContextSpineModel checkedContext
      checkedLengthSpineTypeName spineModel @?= listName
      checkedLengthSpineZeroConstructor spineModel @?= listName
      checkedLengthSpineStepConstructor spineModel @?= consName
      checkedLengthSpineRecursiveField spineModel @?= 1
      checkedLengthSpineModelTrust spineModel @?=
        BuiltinStructuralListSpine
      lengthContractPrecondition contract @?= condition
      lengthContractPostcondition contract @?= LengthEqual result transfer
      checkedLengthContractTarget checkedContract @?= target
      checkedLengthContractInputCount checkedContract @?= 1
      ( lengthProviderName provider
        , lengthProviderScheme provider
        , lengthProviderArgumentRoles provider
        , lengthProviderTransfer provider
        ) @?=
          ( providerName
          , ForallType ["element"] [] $ FunctionType
              (TypeApplication
                (TypeConstructor listName)
                (TypeVariable "element"))
              (TypeApplication
                (TypeConstructor listName)
                (TypeVariable "element"))
          , [LengthSpineArgument]
          , LengthVariable $ LengthProviderArgument 0
          )
      case checkedLengthProviderSummaries checkedProviders of
        [summary] -> do
          checkedLengthProviderTrust summary @?= AssumedProviderLaw
          evaluateLengthProviderApplication defaultLengthEvaluationLimits
              summary [ObservedSpineLength 7] @?= Right 7
        summaries -> fail $ "unexpected checked provider count: "
          ++ show (length summaries)
      evaluateLengthContractAssignment defaultLengthEvaluationLimits
          checkedContract (LengthContractAssignment [0] 3) @?=
        Right LengthPostconditionSatisfied
      [LengthSpineArgument, LengthUnobservedArgument] @?= [minBound .. maxBound]
      (AssumedProviderLaw :: LengthProviderTrust) @?= minBound
      [BuiltinStructuralListSpine, DerivedFromListLikeDataDeclaration] @?=
        [minBound .. maxBound]
  , testCase "exports the prepared class authority" $ do
      className <- expectRight $ mkIdentifier "Class"
      missingName <- expectRight $ mkIdentifier "Missing"
      inventory <- expectRight
        ( mkInventory ClosedKindInventory
            [ ClassDeclaration () className
                [TypeParameter "a" Nothing] [] []
            ]
          :: Either (InventoryError String Void) (Inventory String ())
        )
      let index :: PreparedClassIndex String
          index = prepareClassIndex inventory
      map preparedClassName (preparedClasses index) @?= [className]
      preparedExplicitInstances index @?= []
      inventoryClassArity inventory className @?= Just 1
      inventoryClassArity inventory missingName @?= Nothing
  , testCase "exports shared collision-free allocation" $ do
      let candidate suffix = ("v" ++ show suffix, suffix + 1 :: Int)
          reserved = Set.fromList ["v0", "v1"]
      allocateFresh candidate reserved 0 @?=
        ("v2", Set.insert "v2" reserved, 3)
      allocateFreshMaybe
          (\available -> if available then Nothing else Just ("v", True))
          (Set.singleton "v") False @?= Nothing
      allocateFreshBy elem (:) candidate ["v0", "v2"] 0 @?=
        ("v1", ["v1", "v0", "v2"], 2)
  , testCase "exports shared type inspection" $ do
      checkedName <- expectRight $ mkIdentifier "Box"
      let acceptBinder _ = Nothing :: Maybe String
          flexible = FlexibleVariable "a"
          rigid = RigidVariable "s"
          typeExpression = ForallType ["a"] []
            $ TypeApplication (TypeConstructor checkedName)
            $ TypeVariable "a"
      ( variableIdentity flexible
        , isFlexibleVariable flexible
        , flexibleVariableIdentity rigid
        , rigidVariableIdentity rigid
        , mapFlexibleVariable (++ "'") rigid
        , foldFlexibleVariable (: []) flexible
        , foldRigidVariable (: []) rigid
        ) @?=
          ("a", True, Nothing, Just "s", rigid, ["a"], ["s"])
      leadingForallVariables typeExpression @?= ["a"]
      typeBinderVariables typeExpression @?= ["a"]
      firstForallType typeExpression @?= Just typeExpression
      containsForall typeExpression @?= True
      containsNestedForall typeExpression @?= False
      splitLeadingForalls typeExpression @?=
        ( ["a"]
        , []
        , TypeApplication (TypeConstructor checkedName) (TypeVariable "a")
        )
      typeConstraints typeExpression @?= []
      typeConstructorHead typeExpression @?= Just checkedName
      applyTypeArguments (TypeConstructor checkedName) [TypeVariable "a"]
        @?= TypeApplication (TypeConstructor checkedName) (TypeVariable "a")
      functionType [TypeVariable "a"] (TypeVariable "b") @?=
        FunctionType (TypeVariable "a") (TypeVariable "b")
      normalizeType typeExpression @?= Right typeExpression
      validateType typeExpression @?= Right ()
      functionSpine typeExpression @?= ([], typeExpression)
      freeVariablesInFirstOccurrenceOrder typeExpression @?= []
      constraintFreeVariables
          (Constraint checkedName [typeExpression]) @?= Set.empty
      quantifyFreeVariables (const True) (TypeVariable "b") @?=
        ForallType ["b"] [] (TypeVariable "b")
      implicitizeLeadingForalls acceptBinder (\_ _ -> Just "b")
          Set.empty typeExpression @?=
        Right
          ( TypeApplication (TypeConstructor checkedName) (TypeVariable "b")
          , Set.fromList ["a", "b"]
          )
      uniquifyTypeBinders acceptBinder (\_ _ -> Nothing) Set.empty
          typeExpression @?= Right (typeExpression, Set.singleton "a")
      declaredValueName <- expectRight $ mkIdentifier "value"
      let declaration = ValueDeclaration
            $ ValueSignature () declaredValueName typeExpression
      declarationSubjectName declaration @?= declaredValueName
      declarationTypeVariables declaration @?= ["a", "a"]
      declarationTypeVariables
          (mapDeclarationTypeVariables length declaration) @?= [1, 1]
      let requestTraversal
            :: (RequestTypeSite -> String -> Maybe String)
            -> (Constraint String -> Maybe (Constraint String))
            -> QueryRequest String ()
            -> Maybe (QueryRequest String ())
          requestTraversal = traverseRequestTypes
      requestTraversal `seq`
        (RequestGoal < RequestContextArgument) @?= True
  , testCase "shares explicit context scope across adapters" $ do
      className <- expectRight $ mkIdentifier "Eq"
      targetName <- expectRight $ mkIdentifier "scopedIdentity"
      target <- expectRight $ mkDefinitionName targetName
      let variable name = TypeVariable name
          identity name = FunctionType (variable name) (variable name)
          request goal contexts = QueryRequest
            { requestTarget = target
            , requestGoal = goal
            , requestContexts = contexts
            , requestOptions = defaultQueryOptions
            }
          context name = Constraint className [variable name]
          escaped = request (identity "a") [context "b"]
          malformedBeforeEscaped = request (identity "a")
            [ Constraint className [variable "Bad"]
            , context "b"
            ]
          leadingBound = request
            (ForallType ["a"] [] $ identity "a") [context "a"]
          embedded = request
            (ForallType [] [context "b"] $ identity "a") [context "b"]
          nestedBound = request
            (FunctionType (variable "a")
              $ ForallType ["b"] [] $ variable "b")
            [context "b"]
      requestContextVariablesNotInScope escaped @?= ["b"]
      requestContextVariablesNotInScope leadingBound @?= []
      -- An embedded context is part of the goal itself, while a binder below
      -- an arrow is not in scope for the separate request context list.
      requestContextVariablesNotInScope embedded @?= []
      requestContextVariablesNotInScope nestedBound @?= ["b"]

      session <- expectRight standardDjinnSession
      escapedRequest <- expectRight $ mkDjinnRequest escaped
      case runDjinnQuery session escapedRequest of
        Left failure -> diagnosticCode failure @?= Just "DJEX_DJINN_QUERY"
        Right _ -> assertBool
          "Djinn accepted an explicit context variable outside the goal"
          False
      malformedRequest <- expectRight $ mkDjinnRequest malformedBeforeEscaped
      case runDjinnQuery session malformedRequest of
        Left failure -> diagnosticCode failure @?= Just "DJEX_DJINN_LOWER"
        Right _ -> assertBool
          "Djinn's aggregate scope check hid an earlier malformed context"
          False
      leadingRequest <- expectRight $ mkDjinnRequest leadingBound
      leadingResult <- expectRight $ runDjinnQuery session leadingRequest
      assertBool "Djinn rejected a leading forall variable in context scope"
        $ not $ null $ batchCandidates $ resultSearch leadingResult
  , testCase "exports generated-code rendering" $ do
      target <- expectRight $ mkIdentifier "identity"
      checkedTarget <- expectRight $ mkDefinitionName target
      definitionName checkedTarget @?= target
      definitionSpelling checkedTarget @?= "identity"
      renderFunctionClause (defaultRenderOptions id)
          (FunctionClause checkedTarget [Bind "value"] $ Local "value") @?=
        Right "identity value = value"
  , testCase "exports generated-expression consumer helpers" $ do
      generatedFunctionName <- expectRight $ mkIdentifier "function"
      let function = Global generatedFunctionName :: Expression String
          source = Apply
            (VisibleTypeApplication function inferredVisibleTypeArgument)
            (Local "value")
          preserve
            :: Expression String
            -> Either String (Expression String)
          preserve = Right
      expressionFullApplicationSpine source @?=
        ( function
        , [ VisibleTypeArgumentArgument inferredVisibleTypeArgument
          , TermArgument $ Local "value"
          ]
        )
      applyExpressionArguments function
          [ VisibleTypeArgumentArgument inferredVisibleTypeArgument
          , TermArgument $ Local "value"
          ] @?= source
      uncurry applyExpressionArguments
          (expressionFullApplicationSpine source) @?= source
      rewriteExpressionBottomUp
          (\expression -> case expression of
            Local local -> Hole local
            other -> other)
          source @?=
        Apply
          (VisibleTypeApplication function inferredVisibleTypeArgument)
          (Hole "value")
      rewriteExpressionBottomUpM preserve source @?= Right source
  , testCase "exports the checked typed-candidate boundary" $ do
      globalName <- expectRight $ parseName "Fixture.value"
      let typeA = TypeVariable "a"
          source :: TermGraphSource (Type String) Int
          source = TermGraphSource (termNodeId 0)
            [ ( termNodeId 0
              , TermNode typeA $
                  TypedGlobal (occurrenceId 0) globalName
              )
            ]
      graph <- expectRight $ sealTermGraph
        (sharedTypeStructure :: TypeStructure (Type String))
        defaultTermGraphLimits source
      eraseTermGraph graph @?= Global globalName
      typedGraphSourceOccurrences (termGraphMetrics graph) @?= 1

      let fingerprintSource
            :: TermGraphSource (Type (Variable String)) Int
          fingerprintSource = TermGraphSource (termNodeId 7)
            [ ( termNodeId 7
              , TermNode (TypeVariable $ FlexibleVariable "private") $
                  TypedGlobal (occurrenceId 99) globalName
              )
            ]
      fingerprintGraph <- expectRight $ sealTermGraph sharedTypeStructure
        defaultTermGraphLimits fingerprintSource
      graphFingerprint <- expectRight $ fingerprintSharedTermGraph
        defaultTermGraphLimits defaultTermGraphFingerprintByteLimit
        fingerprintGraph
      assertBool "the canonical graph fingerprint was empty"
        $ not $ null $ fingerprintCanonicalBytes graphFingerprint

      let quantified = ForallType ["bound"] [] $ TypeVariable "bound"
      isLeadingForallInstantiation quantified typeA typeA @?= True
  , testCase "exports atomic finite-spine candidate problems" $ do
      let sealer
            :: LengthProblemLimits
            -> CheckedLengthSession Int ()
            -> CheckedLengthContract ExferenceTypeVariable
            -> ExferenceTypedCandidate
            -> Either
                (LengthProblemError
                  ExferenceTermGraphAbsence Int ExferenceLocal)
                (CheckedLengthProblem Int ExferenceLocal)
          sealer = sealLengthTypedCandidateProblem
          candidateResultProjection
            :: CheckedLengthCandidate Int ExferenceLocal
            -> LengthExpression LengthContractVariable
          candidateResultProjection = checkedLengthCandidateResult
          problemProjection
            :: CheckedLengthProblem Int ExferenceLocal
            -> BehavioralProblem FiniteListSpineLengthV1
          problemProjection = checkedLengthProblemBehavioralProblem
          inputCountProjection
            :: CheckedLengthProblem Int ExferenceLocal
            -> Int
          inputCountProjection = checkedLengthProblemInputCount
          preconditionProjection
            :: CheckedLengthProblem Int ExferenceLocal
            -> LengthFormula LengthContractVariable
          preconditionProjection = checkedLengthProblemPrecondition
          postconditionProjection
            :: CheckedLengthProblem Int ExferenceLocal
            -> LengthFormula LengthContractVariable
          postconditionProjection = checkedLengthProblemPostcondition
          basisProjection
            :: ValidatedLengthCounterexample
            -> LengthCounterexampleBasis
          basisProjection = validatedLengthCounterexampleBasis
          counterexampleValidator
            :: LengthEvaluationLimits
            -> CheckedLengthProblem Int ExferenceLocal
            -> LengthProblemAssignment
            -> Either LengthEvaluationError
                (Maybe
                  (BehavioralEvidence
                    FiniteListSpineLengthV1
                    ValidatedLengthCounterexample))
          counterexampleValidator = validateLengthProblemCounterexample
          queryObservationAssociator
            :: LengthSMTLibQuery Int ExferenceLocal
            -> LengthSMTLibRawSolverObservation
                FacadeRawArtifact FacadeRawArtifact FacadeRawArtifact
            -> AssociatedLengthSMTLibSolverObservation
                Int ExferenceLocal
                FacadeRawArtifact FacadeRawArtifact FacadeRawArtifact
          queryObservationAssociator = associateLengthSMTLibSolverObservation
          queryObservationReplayer
            :: LengthSMTLibQuery Int ExferenceLocal
            -> AssociatedLengthSMTLibSolverObservation
                Int ExferenceLocal
                FacadeRawArtifact FacadeRawArtifact FacadeRawArtifact
            -> Either
                LengthSMTLibObservationReplayError
                (LengthSMTLibRawSolverObservation
                  FacadeRawArtifact FacadeRawArtifact FacadeRawArtifact)
          queryObservationReplayer =
            replayAssociatedLengthSMTLibSolverObservation
          checkResponseParser
            :: LengthSMTLibResponseLimits
            -> [Word8]
            -> Either LengthSMTLibResponseError SolverStatus
          checkResponseParser = parseLengthSMTLibCheckResponse
          inputValueResponseParser
            :: LengthSMTLibResponseLimits
            -> LengthSMTLibQuery Int ExferenceLocal
            -> [Word8]
            -> Either
                LengthSMTLibResponseError
                [LengthSMTLibIntegerBinding]
          inputValueResponseParser = parseLengthSMTLibInputValueResponse
          executionLimitsBuilder
            :: LengthSMTLibExecutionLimitSource
            -> LengthSMTLibExecutionLimits
          executionLimitsBuilder = mkLengthSMTLibExecutionLimits
          executionConfigSealer
            :: LengthSMTLibExecutionLimits
            -> LengthSMTLibExecutionConfigSource
            -> Either
                LengthSMTLibExecutionConfigError
                LengthSMTLibExecutionConfig
          executionConfigSealer = mkLengthSMTLibExecutionConfig
          executionTimeoutProjection
            :: LengthSMTLibExecutionConfig
            -> Int
          executionTimeoutProjection =
            lengthSMTLibExecutionSolverTimeoutMilliseconds
          executionResourceProjection
            :: LengthSMTLibExecutionConfig
            -> Int
          executionResourceProjection = lengthSMTLibExecutionSolverResourceLimit
          executionDeadlineProjection
            :: LengthSMTLibExecutionConfig
            -> Int
          executionDeadlineProjection =
            lengthSMTLibExecutionHostDeadlineMilliseconds
          executionArtifactProjection
            :: LengthSMTLibExecutionConfig
            -> LengthSMTLibArtifactPolicy
          executionArtifactProjection = lengthSMTLibExecutionArtifactPolicy
          executionResponseProjection
            :: LengthSMTLibExecutionConfig
            -> LengthSMTLibResponseLimits
          executionResponseProjection = lengthSMTLibExecutionResponseLimits
      sealer `seq` candidateResultProjection `seq` problemProjection `seq`
        inputCountProjection `seq` preconditionProjection `seq`
        postconditionProjection `seq` basisProjection `seq`
        counterexampleValidator `seq` queryObservationAssociator `seq`
        queryObservationReplayer `seq` checkResponseParser `seq`
        inputValueResponseParser `seq` executionLimitsBuilder `seq`
        executionConfigSealer `seq` executionTimeoutProjection `seq`
        executionResourceProjection `seq` executionDeadlineProjection `seq`
        executionArtifactProjection `seq` executionResponseProjection `seq`
        pure ()
      lengthProblemAssignmentInputs (LengthProblemAssignment [1, 2]) @?=
        [1, 2]
      (ProviderIndependentFiniteSpineModel :: LengthCounterexampleBasis) @?=
        ProviderIndependentFiniteSpineModel
      lengthProblemTermGraphLimits defaultLengthProblemLimits @?=
        defaultTermGraphLimits
      lengthProblemGraphFingerprintByteLimit defaultLengthProblemLimits @?=
        defaultTermGraphFingerprintByteLimit
      lengthProblemEvaluationStepLimit defaultLengthProblemLimits @?= 65536
      mkLengthProblemLimits defaultTermGraphLimits 0 (-1) @?= Left
        (NegativeLengthProblemEvaluationStepLimit (-1))
      ( LengthSMTLibObservationQueryFingerprintMismatch
          :: LengthSMTLibObservationReplayError
        ) @?= LengthSMTLibObservationQueryFingerprintMismatch
      mkLengthSMTLibResponseLimits
          defaultLengthSMTLibResponseLimitSource @?=
        Right defaultLengthSMTLibResponseLimits
      lengthSMTLibExecutionArgumentPrefix @?=
        ["-in", "-smt2", "smtlib2_compliant=true"]
      lengthSMTLibExecutionProtocolSchemaTag @?=
        map (fromIntegral . fromEnum)
          ("djex-length-z3-smtlib2-session-protocol/v1" :: String)
      lengthSMTLibExecutionStartupCommandBytes @?=
        map (fromIntegral . fromEnum)
          ("(set-option :print-success false)\n" :: String)
      lengthSMTLibExecutionEnvironmentPolicyTag @?=
        map (fromIntegral . fromEnum) ("empty-environment/v1" :: String)
  , testCase "rejects residual constraints at the Djinn render boundary" $ do
      target <- expectRight $ mkIdentifier "identity"
      checkedTarget <- expectRight $ mkDefinitionName target
      className <- expectRight $ mkIdentifier "Eq"
      let residual = Constraint className [TypeVariable "a"]
          candidateWith output = Candidate output [residual]
            (DjinnCandidateDetails 0 0) :: DjinnCandidate
          validCandidate = candidateWith
            $ FunctionClause checkedTarget [Bind "value"] $ Local "value"
          invalidCandidate = candidateWith
            $ FunctionClause checkedTarget [] $ Local "free"
          invalidNameCandidate = candidateWith
            $ FunctionClause checkedTarget [Bind "case"] $ Local "case"
      renderDjinnCandidateExpression Unqualified validCandidate @?=
        Left UnexpectedResidualConstraints
      renderDjinnCandidateDefinition Unqualified validCandidate @?=
        Left UnexpectedResidualConstraints
      -- Generated-code errors remain primary when both public record halves
      -- are caller-forged.
      renderDjinnCandidateExpression Unqualified invalidCandidate @?=
        Left UnboundLocalIdentity
      renderDjinnCandidateDefinition Unqualified invalidCandidate @?=
        Left UnboundLocalIdentity
      renderDjinnCandidateExpression Unqualified invalidNameCandidate @?=
        Left (InvalidLocalName "case" $ ReservedIdentifier "case")
      renderDjinnCandidateDefinition Unqualified invalidNameCandidate @?=
        Left (InvalidLocalName "case" $ ReservedIdentifier "case")
  , testCase "exports checked Exference options" $
      exferenceMaximumSteps defaultExferenceOptions @?= 65536
  , testCase "exports explicit Exference record-pattern views" $
      patternViewsRoundTrip @?= True
  , testCase "exports checked session entry points" $ do
      assertBool "the standard Djinn session did not seal" $
        isRight standardDjinnSession
      let djinnTypeProjection :: DjinnType -> Type DjinnTypeVariable
          djinnTypeProjection = id
          djinnRequestProjection
            :: DjinnRequest -> QueryRequest DjinnType QueryOptions
          djinnRequestProjection = djinnRequestQuery
          djinnCandidateProjection
            :: DjinnCandidate
            -> Candidate DjinnType DjinnCandidateDetails
                (FunctionClause DjinnLocal)
          djinnCandidateProjection = id
          djinnEnvironmentProjection
            :: DjinnEnvironment
            -> Environment DjinnTypeVariable Void ()
          djinnEnvironmentProjection = id
          inventoryProjection
            :: ExferenceSession -> ExferenceInventory
          inventoryProjection = exferenceSessionInventory
          sessionEnvironmentProjection
            :: ExferenceSession -> ExferenceEnvironment
          sessionEnvironmentProjection = exferenceSessionEnvironment
          environmentProjection
            :: ExferenceEnvironment
            -> Environment ExferenceTypeVariable Void ()
          environmentProjection = id
          requestProjection
            :: ExferenceRequest
            -> QueryRequest ExferenceType ExferenceOptions
          requestProjection = exferenceRequestQuery
          candidateProjection
            :: ExferenceCandidate -> ExferenceCandidateDetails
          candidateProjection = candidateDetails
          residualRendererProjection
            :: ExferenceCandidate
            -> Either ExferenceResidualRenderError [String]
          residualRendererProjection = renderExferenceResidualConstraints
          qualifiedResidualRendererProjection
            :: Qualification
            -> ExferenceCandidate
            -> Either ExferenceResidualRenderError [String]
          qualifiedResidualRendererProjection =
            renderExferenceResidualConstraintsWithQualification
          metadataProjection
            :: ExferenceResult -> ExferenceBatchMetadata
          metadataProjection = batchMetadata . resultSearch
          typedCompatibilityProjection
            :: ExferenceTypedCandidate -> ExferenceCandidate
          typedCompatibilityProjection = typedCandidateCompatibility
          typedGraphProjection
            :: ExferenceTypedCandidate
            -> Either ExferenceTermGraphAbsence
                (TermGraph ExferenceType ExferenceLocal)
          typedGraphProjection = typedCandidateTermGraph
          providerEvidenceProjection
            :: ProviderInstantiationCandidate String
            -> (Name, Type String)
          providerEvidenceProjection evidence =
            ( providerInstantiationCandidateProvider evidence
            , providerInstantiationCandidateType evidence
            )
          providerAssignmentProjection
            :: ProviderInstantiationAssignment String
            -> (Name, [Type String])
          providerAssignmentProjection assignment =
            ( providerInstantiationAssignmentProvider assignment
            , providerInstantiationAssignmentArguments assignment
            )
          kindedProviderAssignmentProjection
            :: KindedProviderInstantiationAssignment String
            -> (Name, [(GroundKind, Type String)])
          kindedProviderAssignmentProjection assignment =
            ( kindedProviderInstantiationAssignmentProvider assignment
            , kindedProviderInstantiationAssignmentArguments assignment
            )
          djinnEvidenceRunner
            :: DjinnSession
            -> [ProviderInstantiationCandidate DjinnTypeVariable]
            -> DjinnRequest
            -> Either Diagnostic DjinnResult
          djinnEvidenceRunner = runDjinnQueryWithInstantiationCandidates
          djinnAssignmentRunner
            :: DjinnSession
            -> [ProviderInstantiationAssignment DjinnTypeVariable]
            -> DjinnRequest
            -> Either Diagnostic DjinnResult
          djinnAssignmentRunner = runDjinnQueryWithInstantiationAssignments
          djinnKindedAssignmentRunner
            :: DjinnSession
            -> [KindedProviderInstantiationAssignment DjinnTypeVariable]
            -> DjinnRequest
            -> Either Diagnostic DjinnResult
          djinnKindedAssignmentRunner =
            runDjinnQueryWithKindedInstantiationAssignments
          exferenceEvidenceRunner
            :: ExferenceSession
            -> [ProviderInstantiationCandidate ExferenceTypeVariable]
            -> ExferenceRequest
            -> Either Diagnostic [ExferenceResult]
          exferenceEvidenceRunner =
            runExferenceQueryWithInstantiationCandidates
          exferenceTypedRunner
            :: ExferenceSession
            -> ExferenceRequest
            -> Either Diagnostic [ExferenceTypedResult]
          exferenceTypedRunner = runExferenceTypedQuery
          exferenceTypedEvidenceRunner
            :: ExferenceSession
            -> [ProviderInstantiationCandidate ExferenceTypeVariable]
            -> ExferenceRequest
            -> Either Diagnostic [ExferenceTypedResult]
          exferenceTypedEvidenceRunner =
            runExferenceTypedQueryWithInstantiationCandidates
          exferenceAssignmentRunner
            :: ExferenceSession
            -> [ProviderInstantiationAssignment ExferenceTypeVariable]
            -> ExferenceRequest
            -> Either Diagnostic [ExferenceResult]
          exferenceAssignmentRunner =
            runExferenceQueryWithInstantiationAssignments
          exferenceTypedAssignmentRunner
            :: ExferenceSession
            -> [ProviderInstantiationAssignment ExferenceTypeVariable]
            -> ExferenceRequest
            -> Either Diagnostic [ExferenceTypedResult]
          exferenceTypedAssignmentRunner =
            runExferenceTypedQueryWithInstantiationAssignments
          exferenceKindedAssignmentRunner
            :: ExferenceSession
            -> [KindedProviderInstantiationAssignment ExferenceTypeVariable]
            -> ExferenceRequest
            -> Either Diagnostic [ExferenceResult]
          exferenceKindedAssignmentRunner =
            runExferenceQueryWithKindedInstantiationAssignments
          exferenceTypedKindedAssignmentRunner
            :: ExferenceSession
            -> [KindedProviderInstantiationAssignment ExferenceTypeVariable]
            -> ExferenceRequest
            -> Either Diagnostic [ExferenceTypedResult]
          exferenceTypedKindedAssignmentRunner =
            runExferenceTypedQueryWithKindedInstantiationAssignments
      djinnTypeProjection `seq` djinnRequestProjection `seq`
        djinnCandidateProjection `seq` djinnEnvironmentProjection `seq`
        inventoryProjection `seq`
        sessionEnvironmentProjection `seq`
        environmentProjection `seq`
        requestProjection `seq` candidateProjection `seq`
        typedCompatibilityProjection `seq` typedGraphProjection `seq`
        residualRendererProjection `seq`
        qualifiedResidualRendererProjection `seq`
        metadataProjection `seq` providerEvidenceProjection `seq`
        providerAssignmentProjection `seq`
        kindedProviderAssignmentProjection `seq`
        djinnEvidenceRunner `seq` djinnAssignmentRunner `seq`
        djinnKindedAssignmentRunner `seq`
        exferenceEvidenceRunner `seq` exferenceTypedRunner `seq`
        exferenceTypedEvidenceRunner `seq` exferenceAssignmentRunner `seq`
        exferenceTypedAssignmentRunner `seq`
        exferenceKindedAssignmentRunner `seq`
        exferenceTypedKindedAssignmentRunner `seq` pure ()
      mkDjinnRequest `seq` mkExferenceSession `seq`
        mkExferenceSessionWithPolicy `seq` pure ()
      maximumProviderInstantiationCandidates @?= 32
      maximumProviderInstantiationAssignments @?= 32
      maximumProviderInstantiationArguments @?= 6
      maximumProviderInstantiationKindNodes @?= 129
  , testCase "retains checked Exference graphs beside exact legacy results" $ do
      environment <- expectRight
        (mkEnvironment [] ::
          Either (EnvironmentError ExferenceTypeVariable)
            ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      targetName <- expectRight $ mkIdentifier "typedIdentity"
      target <- expectRight $ mkDefinitionName targetName
      let variable = FlexibleVariable 0
          goal = FunctionType (TypeVariable variable)
            (TypeVariable variable)
          query = QueryRequest
            { requestTarget = target
            , requestGoal = goal
            , requestContexts = []
            , requestOptions = defaultExferenceOptions
                { exferenceMaximumSteps = 4 }
            }
      request <- expectRight $ mkExferenceRequest query
      typedResults <- expectRight $ runExferenceTypedQuery session request
      legacyResults <- expectRight $ runExferenceQuery session request
      map (fmap typedCandidateCompatibility) typedResults @?= legacyResults
      repeated <- expectRight $ runExferenceTypedQuery session request
      typedResults @?= repeated
      case
          [ candidate
          | result <- typedResults
          , candidate <- batchCandidates $ resultSearch result
          ] of
        [] -> fail "the identity query returned no typed candidate"
        candidate : _ -> case typedCandidateTermGraph candidate of
          Left absence -> fail $ "the identity graph was unavailable: "
            ++ show absence
          Right graph -> do
            eraseTermGraphToFunctionClause target graph @?=
              candidateOutput (typedCandidateCompatibility candidate)
            assertBool "the typed graph lost source occurrence identities"
              $ typedGraphSourceOccurrences (termGraphMetrics graph) > 0
  , testCase "retains explicit typed fallback without weakening evidence" $ do
      environment <- expectRight
        (mkEnvironment [] ::
          Either (EnvironmentError ExferenceTypeVariable)
            ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      targetName <- expectRight $ mkIdentifier "typedForallIdentity"
      target <- expectRight $ mkDefinitionName targetName
      let outerVariable = FlexibleVariable 0
          nestedVariable = FlexibleVariable 1
          goal = FunctionType (TypeVariable outerVariable)
            $ ForallType [nestedVariable] []
            $ FunctionType (TypeVariable nestedVariable)
                (TypeVariable nestedVariable)
          query = QueryRequest
            { requestTarget = target
            , requestGoal = goal
            , requestContexts = []
            , requestOptions = defaultExferenceOptions
                { exferenceAllowUnused = True
                , exferenceMaximumSteps = 20
                }
            }
      request <- expectRight $ mkExferenceRequest query
      typedResults <- expectRight $ runExferenceTypedQuery session request
      legacyResults <- expectRight $ runExferenceQuery session request
      map (fmap typedCandidateCompatibility) typedResults @?= legacyResults
      case
          [ (result, candidate)
          | result <- typedResults
          , candidate <- batchCandidates $ resultSearch result
          ] of
        [] -> fail "the forall identity query returned no candidate"
        (result, candidate) : _ -> do
          resultEvidence result @?= ValidatedCandidates
          case typedCandidateTermGraph candidate of
            Left NestedForallIntroduction{} -> pure ()
            Left absence -> fail $ "unexpected typed fallback: "
              ++ show absence
            Right _ -> fail "the unsupported forall introduction claimed a graph"
  , testCase "seals Djinn from the neutral environment vocabulary" $ do
      let checkedEnvironment
            :: Either (EnvironmentError DjinnTypeVariable) DjinnEnvironment
          checkedEnvironment = mkEnvironment []
      environment <- expectRight checkedEnvironment
      session <- expectRight $ mkDjinnSession environment
      let inventory :: DjinnInventory
          inventory = djinnSessionInventory session
      environmentDeclarations (inventoryEnvironment inventory) @?= []
  , testCase "seals Exference from the neutral environment vocabulary" $ do
      let checkedEnvironment
            :: Either
                (EnvironmentError ExferenceTypeVariable)
                ExferenceEnvironment
          checkedEnvironment = mkEnvironment []
      environment <- expectRight checkedEnvironment
      session <- expectRight $ mkExferenceSession environment
      let inventory :: ExferenceInventory
          inventory = exferenceSessionInventory session
      exferenceSessionEnvironment session @?= environment
      environmentDeclarations (inventoryEnvironment inventory) @?= []
      let fresh :: FreshVariable ExferenceTypeVariable
          fresh _ _ = Nothing
          goal :: ExferenceType
          goal = TypeVariable $ FlexibleVariable 0
      _ <- expectRight $ prepareTypeSynonyms fresh inventory
      prepared <- expectRight $ prepareInventory fresh inventory
      unknown <- expectRight $ mkIdentifier "UnknownAlias"
      checkPreparedTypeSynonymApplicationSaturation prepared unknown 0 @?=
        Right ()
      checkPreparedTypeSynonymSaturation prepared goal @?= Right ()
      elaboratePreparedType fresh prepared ProperTypeKind goal @?= Right goal
      elaboratePreparedTypes fresh prepared [(ProperTypeKind, goal)] @?=
        Right [goal]
  ]

inspectFingerprint :: Fingerprint subject -> ([Word8], String)
inspectFingerprint fingerprint =
  (fingerprintCanonicalBytes fingerprint, fingerprintCode fingerprint)

inspectBehavioralProblem
  :: BehavioralProblem FacadeBehavioralDomain
  -> ([Word8], String, String, String, String)
inspectBehavioralProblem problem =
  ( behavioralProblemDomain problem
  , fingerprintCode $ behavioralProblemInventoryFingerprint problem
  , fingerprintCode $ behavioralProblemEncodingFingerprint problem
  , fingerprintCode $ behavioralProblemCandidateFingerprint problem
  , fingerprintCode $ behavioralProblemFingerprint problem
  )

inspectBehavioralEvidence
  :: BehavioralProblem FacadeBehavioralDomain
  -> BehavioralEvidence FacadeBehavioralDomain String
  -> ( [Word8]
     , String
     , String
     , String
     , String
     , Either ReplayMismatch String
     )
inspectBehavioralEvidence problem evidence =
  ( behavioralEvidenceDomain evidence
  , fingerprintCode $ behavioralEvidenceInventoryFingerprint evidence
  , fingerprintCode $ behavioralEvidenceEncodingFingerprint evidence
  , fingerprintCode $ behavioralEvidenceCandidateFingerprint evidence
  , fingerprintCode $ behavioralEvidenceProblemFingerprint evidence
  , replayBehavioralEvidence problem evidence
  )

expectRight :: Show error => Either error value -> IO value
expectRight result = case result of
  Left failure -> fail $ show failure
  Right value -> pure value
