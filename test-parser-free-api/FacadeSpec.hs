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
  , testCase "exports shared collision-free allocation" $ do
      let candidate suffix = ("v" ++ show suffix, suffix + 1 :: Int)
          reserved = Set.fromList ["v0", "v1"]
      allocateFresh candidate reserved 0 @?=
        ("v2", Set.insert "v2" reserved, 3)
      allocateFreshMaybe
          (\available -> if available then Nothing else Just ("v", True))
          (Set.singleton "v") False @?= Nothing
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
  , testCase "exports generated-code rendering" $ do
      target <- expectRight $ mkIdentifier "identity"
      checkedTarget <- expectRight $ mkDefinitionName target
      definitionName checkedTarget @?= target
      definitionSpelling checkedTarget @?= "identity"
      renderFunctionClause (defaultRenderOptions id)
          (FunctionClause checkedTarget [Bind "value"] $ Local "value") @?=
        Right "identity value = value"
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
          metadataProjection
            :: ExferenceResult -> ExferenceBatchMetadata
          metadataProjection = batchMetadata . resultSearch
      djinnTypeProjection `seq` djinnRequestProjection `seq`
        djinnCandidateProjection `seq` djinnEnvironmentProjection `seq`
        inventoryProjection `seq`
        sessionEnvironmentProjection `seq`
        environmentProjection `seq`
        requestProjection `seq` candidateProjection `seq`
        metadataProjection `seq` pure ()
      mkDjinnRequest `seq` mkExferenceSession `seq`
        mkExferenceSessionWithPolicy `seq` pure ()
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
      assertBool "the facade did not reexport synonym elaboration"
        $ isRight $ prepareTypeSynonyms fresh inventory
  ]

expectRight :: Show error => Either error value -> IO value
expectRight result = case result of
  Left failure -> fail $ show failure
  Right value -> pure value
