module FacadeSpec (facadeTests) where

import Data.Either (isRight)
import qualified Data.Set as Set
import Data.Void (Void)

import ExferencePatternImports (patternViewsRoundTrip)
import Language.Haskell.Djex
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

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
      rewriteExpressionBottomUp
          (\expression -> case expression of
            Local local -> Hole local
            other -> other)
          source @?=
        Apply
          (VisibleTypeApplication function inferredVisibleTypeArgument)
          (Hole "value")
      rewriteExpressionBottomUpM preserve source @?= Right source
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
          exferenceAssignmentRunner
            :: ExferenceSession
            -> [ProviderInstantiationAssignment ExferenceTypeVariable]
            -> ExferenceRequest
            -> Either Diagnostic [ExferenceResult]
          exferenceAssignmentRunner =
            runExferenceQueryWithInstantiationAssignments
          exferenceKindedAssignmentRunner
            :: ExferenceSession
            -> [KindedProviderInstantiationAssignment ExferenceTypeVariable]
            -> ExferenceRequest
            -> Either Diagnostic [ExferenceResult]
          exferenceKindedAssignmentRunner =
            runExferenceQueryWithKindedInstantiationAssignments
      djinnTypeProjection `seq` djinnRequestProjection `seq`
        djinnCandidateProjection `seq` djinnEnvironmentProjection `seq`
        inventoryProjection `seq`
        sessionEnvironmentProjection `seq`
        environmentProjection `seq`
        requestProjection `seq` candidateProjection `seq`
        residualRendererProjection `seq`
        qualifiedResidualRendererProjection `seq`
        metadataProjection `seq` providerEvidenceProjection `seq`
        providerAssignmentProjection `seq`
        kindedProviderAssignmentProjection `seq`
        djinnEvidenceRunner `seq` djinnAssignmentRunner `seq`
        djinnKindedAssignmentRunner `seq`
        exferenceEvidenceRunner `seq` exferenceAssignmentRunner `seq`
        exferenceKindedAssignmentRunner `seq` pure ()
      mkDjinnRequest `seq` mkExferenceSession `seq`
        mkExferenceSessionWithPolicy `seq` pure ()
      maximumProviderInstantiationCandidates @?= 32
      maximumProviderInstantiationAssignments @?= 32
      maximumProviderInstantiationArguments @?= 4
      maximumProviderInstantiationKindNodes @?= 129
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

expectRight :: Show error => Either error value -> IO value
expectRight result = case result of
  Left failure -> fail $ show failure
  Right value -> pure value
