module Main (main) where

import Control.Exception (evaluate)
import Data.List (intercalate, isInfixOf, nub, sort)
import Data.Word (Word8)
import Numeric.Natural (Natural)
import qualified System.Info as SystemInfo
import System.Timeout (timeout)
import Unsafe.Coerce (unsafeCoerce)

import qualified Language.Haskell.Djex as Djex
import Language.Haskell.Synthesis.Constraint (Constraint (..))
import Language.Haskell.Synthesis.Declaration
  ( DataConstructor (DataConstructor)
  , Declaration
      ( AbstractTypeDeclaration
      , ClassDeclaration
      , DataTypeDeclaration
      , InstanceDeclaration
      , ValueDeclaration
      )
  , TypeParameter (TypeParameter)
  , ValueSignature (ValueSignature)
  )
import qualified Language.Haskell.Synthesis.Fingerprint as Fingerprint
import Language.Haskell.Synthesis.Inventory
  ( Inventory
  , mkInventory
  )
import qualified Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Execution
  as InternalSMTLibExecution
import qualified Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol
  as SMTLibProtocol
import qualified Language.Haskell.Synthesis.Internal.SMTLib.Stream
  as SMTLibStream
import qualified Language.Haskell.Synthesis.Internal.TypedCandidate
  as InternalTypedCandidate
import Language.Haskell.Synthesis.Kind (Kind (ProperTypeKind))
import Language.Haskell.Synthesis.KindInference
  ( KindInventoryPolicy (ClosedKindInventory) )
import Language.Haskell.Synthesis.Name
  ( Boxity (Boxed)
  , Name
  , consName
  , listName
  , parseName
  )
import qualified Language.Haskell.Synthesis.Semantic.Length as Length
import qualified Language.Haskell.Synthesis.Semantic.Length.Evaluate as Evaluate
import qualified Language.Haskell.Synthesis.Semantic.Length.Problem as LengthProblem
import qualified Language.Haskell.Synthesis.Semantic.Length.SMTLib as SMTLib
import qualified Language.Haskell.Synthesis.Semantic.Length.SMTLib.Execution
  as SMTLibExecution
import qualified Language.Haskell.Synthesis.Semantic.Length.SMTLib.Observation
  as SMTLibObservation
import qualified Language.Haskell.Synthesis.Semantic.Length.SMTLib.Response
  as SMTLibResponse
import qualified Language.Haskell.Synthesis.Semantic.Observation as Observation
import qualified Language.Haskell.Synthesis.Semantic.Problem as SemanticProblem
import Language.Haskell.Synthesis.Type (Type (..), Variable (..))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
  ( assertBool
  , assertFailure
  , testCase
  , (@?=)
  )

main :: IO ()
main = defaultMain lengthTests

lengthTests :: TestTree
lengthTests = testGroup "finite-list-spine-length/v1"
  [ limitTests
  , contextTests
  , contractTests
  , providerTests
  , sessionTests
  , candidateProblemTests
  , problemReplayTests
  , smtLibTests
  , smtLibProtocolTests
  , evaluationTests
  , normalizationTests
  , productiveBoundTests
  , fingerprintTests
  ]

sessionTests :: TestTree
sessionTests = testGroup "checked length sessions"
  [ testCase "bind context and provider laws to one exact inventory" $ do
      providerName <- expectName "Fixture.sessionProvider"
      let provider = sessionUnaryProvider providerName "element"
          inventory = sessionInventory ()
            [sessionProviderDeclaration () provider]
      session <- expectRight $ LengthProblem.sealLengthSession
        Length.defaultLengthLimits inventory Length.BuiltinListSpine [provider]
      Length.lengthContextInventory
          (LengthProblem.checkedLengthSessionContext session) @?= inventory
      assertBool "checked provider disappeared from the atomic session" $
        case Length.lookupCheckedLengthProviderSummary providerName
            $ LengthProblem.checkedLengthSessionProviderInventory session of
          Nothing -> False
          Just _ -> True
  , testCase "erase annotations but retain every neutral declaration" $ do
      providerName <- expectName "Fixture.annotationProvider"
      unusedName <- expectName "Fixture.unusedSessionValue"
      let provider = sessionUnaryProvider providerName "element"
          providerDeclaration annotation =
            sessionProviderDeclaration annotation provider
          base annotation = sessionInventory annotation
            [providerDeclaration annotation]
          widened = sessionInventory (3 :: Int)
            [ providerDeclaration 3
            , ValueDeclaration $ ValueSignature 3 unusedName sessionPayloadType
            ]
      first <- expectRight $ LengthProblem.sealLengthSession
        Length.defaultLengthLimits (base (1 :: Int))
        Length.BuiltinListSpine [provider]
      annotated <- expectRight $ LengthProblem.sealLengthSession
        Length.defaultLengthLimits (base (2 :: Int))
        Length.BuiltinListSpine [provider]
      withUnused <- expectRight $ LengthProblem.sealLengthSession
        Length.defaultLengthLimits widened Length.BuiltinListSpine [provider]
      LengthProblem.lengthSessionInventoryFingerprint first @?=
        LengthProblem.lengthSessionInventoryFingerprint annotated
      assertBool "an unused neutral declaration was absent from exact identity" $
        LengthProblem.lengthSessionInventoryFingerprint first /=
          LengthProblem.lengthSessionInventoryFingerprint withUnused
      LengthProblem.lengthSessionEncodingPolicyFingerprint first @?=
        LengthProblem.lengthSessionEncodingPolicyFingerprint withUnused
  , testCase "canonicalize declaration and provider type binders together" $ do
      providerName <- expectName "Fixture.alphaSessionProvider"
      let firstProvider = sessionUnaryProvider providerName "first"
          secondProvider = sessionUnaryProvider providerName "renamed"
          firstInventory = sessionInventory ()
            [sessionProviderDeclaration () firstProvider]
          secondInventory = sessionInventory ()
            [sessionProviderDeclaration () secondProvider]
      first <- expectRight $ LengthProblem.sealLengthSession
        Length.defaultLengthLimits firstInventory
        Length.BuiltinListSpine [firstProvider]
      second <- expectRight $ LengthProblem.sealLengthSession
        Length.defaultLengthLimits secondInventory
        Length.BuiltinListSpine [secondProvider]
      LengthProblem.lengthSessionInventoryFingerprint first @?=
        LengthProblem.lengthSessionInventoryFingerprint second
      LengthProblem.lengthSessionEncodingPolicyFingerprint first @?=
        LengthProblem.lengthSessionEncodingPolicyFingerprint second
  , testCase "ignore instance binder spelling and declaration order" $ do
      className <- expectName "Fixture.SessionRelation"
      let classParameter identity = TypeParameter
            (FlexibleVariable identity) (Just ProperTypeKind)
          classDeclaration = ClassDeclaration () className
            [classParameter "class-left", classParameter "class-right"] [] []
          firstLeft = FlexibleVariable "first-left"
          firstRight = FlexibleVariable "first-right"
          renamedLeft = FlexibleVariable "renamed-left"
          renamedRight = FlexibleVariable "renamed-right"
          instanceDeclaration binders left right = InstanceDeclaration
            () binders [] $ Constraint className
              [TypeVariable left, TypeVariable right]
          firstInventory = sessionInventory ()
            [ classDeclaration
            , instanceDeclaration
                [firstLeft, firstRight] firstLeft firstRight
            ]
          reorderedInventory = sessionInventory ()
            [ classDeclaration
            , instanceDeclaration
                [renamedRight, renamedLeft] renamedLeft renamedRight
            ]
      first <- expectRight $ LengthProblem.sealLengthSession
        Length.defaultLengthLimits firstInventory Length.BuiltinListSpine []
      reordered <- expectRight $ LengthProblem.sealLengthSession
        Length.defaultLengthLimits reorderedInventory Length.BuiltinListSpine []
      LengthProblem.lengthSessionInventoryFingerprint first @?=
        LengthProblem.lengthSessionInventoryFingerprint reordered
  , testCase "reject a provider projection not owned by the source inventory" $ do
      providerName <- expectName "Fixture.foreignSessionProvider"
      let provider = sessionUnaryProvider providerName "element"
          inventory = sessionInventory () []
      case LengthProblem.sealLengthSession Length.defaultLengthLimits
          inventory Length.BuiltinListSpine [provider] of
        Left (LengthProblem.LengthSessionProviderInventoryRejected
            (Length.LengthProviderSummaryRejected 0 rejectedName
              (Length.LengthProviderNotInSourceInventory missingName))) -> do
          rejectedName @?= providerName
          missingName @?= providerName
        Left other -> assertFailure $ "unexpected rejection: " ++ show other
        Right _ -> assertFailure "a foreign provider was admitted"
  , testCase "surface the first bounded session identity failure" $ do
      let inventory = sessionInventory () []
      context <- expectRight $ Length.sealLengthContext
        Length.defaultLengthLimits inventory Length.BuiltinListSpine
      providers <- expectRight $ Length.sealLengthProviderInventoryInContext
        Length.defaultLengthLimits context []
      let providerBytes = length $ Fingerprint.fingerprintCanonicalBytes
            $ Length.lengthProviderInventoryFingerprint providers
          limits = limitsWith $ \source -> source
            { Length.lengthLimitSourceFingerprintBytes = providerBytes }
      assertLeft
        (LengthProblem.LengthSessionFingerprintLimitExceeded
          LengthProblem.LengthSemanticInventoryFingerprint
          (fromIntegral providerBytes) (fromIntegral providerBytes + 1))
        $ LengthProblem.sealLengthSession limits inventory
            Length.BuiltinListSpine []
  , testCase "reserve modeled constructor names from provider laws" $ do
      typeName <- expectName "Fixture.SessionSpine"
      zeroName <- expectName "Fixture.SessionEmpty"
      stepName <- expectName "Fixture.SessionStep"
      let element = FlexibleVariable "element"
          spine = TypeApplication (TypeConstructor typeName)
            $ TypeVariable element
          declaration = DataTypeDeclaration () typeName
            [TypeParameter element Nothing]
            [ DataConstructor () zeroName []
            , DataConstructor () stepName [TypeVariable element, spine]
            ]
          inventory = sessionInventory () [declaration]
          zeroScheme = ForallType [element] [] spine
          conflicting = Length.AssumedProviderSummary
            { Length.lengthProviderName = zeroName
            , Length.lengthProviderScheme = zeroScheme
            , Length.lengthProviderArgumentRoles = []
            , Length.lengthProviderTransfer = Length.LengthLiteral 0
            }
      assertLeft
        (LengthProblem.LengthSessionProviderConflictsWithSpineConstructor
          zeroName)
        $ LengthProblem.sealLengthSession Length.defaultLengthLimits inventory
            (Length.DeclaredListSpine typeName zeroName stepName) [conflicting]
  ]

candidateProblemTests :: TestTree
candidateProblemTests = testGroup "typed candidate behavioral problems"
  [ testCase "interpret one real Exference list identity atomically" $ do
      (lengthSession, contract, candidate) <- realListIdentityFixture
        $ TypeVariable $ FlexibleVariable 0
      problem <- case LengthProblem.sealLengthTypedCandidateProblem
          LengthProblem.defaultLengthProblemLimits
          lengthSession contract candidate of
        Right value -> pure value
        Left failure -> do
          graph <- expectRight $ Djex.typedCandidateTermGraph candidate
          let root = Djex.termGraphRoot graph
              rootType = Djex.termNodeType <$> Djex.lookupTermNode root graph
          assertFailure $ "unexpected candidate rejection: " ++ show failure
            ++ "; contract target: "
            ++ show (Length.checkedLengthContractTarget contract)
            ++ "; graph root type: " ++ show rootType
      let checkedCandidate = LengthProblem.checkedLengthProblemCandidate problem
      LengthProblem.checkedLengthCandidateResult checkedCandidate @?=
        Length.LengthVariable (Length.LengthInput 0)
      LengthProblem.checkedLengthCandidateUsedProviders checkedCandidate @?= []
      LengthProblem.checkedLengthProblemCounterexampleCondition problem @?=
        Length.LengthTruth False
      Djex.behavioralProblemDomain
          (LengthProblem.checkedLengthProblemBehavioralProblem problem) @?=
        Length.finiteListSpineLengthDomainTag
      Djex.behavioralProblemCandidateFingerprint
          (LengthProblem.checkedLengthProblemBehavioralProblem problem) @?=
        LengthProblem.checkedLengthCandidateFingerprint checkedCandidate
      Djex.behavioralProblemEncodingFingerprint
          (LengthProblem.checkedLengthProblemBehavioralProblem problem) @?=
        LengthProblem.checkedLengthProblemEncodingFingerprint problem
  , testCase "separate contract identity from graph and candidate identity" $ do
      session <- adversarialLengthSession [] []
      let target = FunctionType adversarialClosedList adversarialClosedList
      identityContract <- adversarialLengthContract
        session target identityLengthContract
      trivialContract <- adversarialLengthContract
        session target trivialLengthContract
      graph <- sealAdversarialGraph
        $ adversarialIdentityGraphSource adversarialClosedList
      let candidate = adversarialTypedCandidate $ Right graph
      identityProblem <- expectRight
        $ LengthProblem.sealLengthTypedCandidateProblem
            LengthProblem.defaultLengthProblemLimits
            session identityContract candidate
      trivialProblem <- expectRight
        $ LengthProblem.sealLengthTypedCandidateProblem
            LengthProblem.defaultLengthProblemLimits
            session trivialContract candidate
      let identityCandidate = LengthProblem.checkedLengthProblemCandidate
            identityProblem
          trivialCandidate = LengthProblem.checkedLengthProblemCandidate
            trivialProblem
          identityEnvelope = LengthProblem.checkedLengthProblemBehavioralProblem
            identityProblem
          trivialEnvelope = LengthProblem.checkedLengthProblemBehavioralProblem
            trivialProblem
      LengthProblem.checkedLengthCandidateTermGraphFingerprint
          identityCandidate @?=
        LengthProblem.checkedLengthCandidateTermGraphFingerprint
          trivialCandidate
      LengthProblem.checkedLengthCandidateFingerprint identityCandidate @?=
        LengthProblem.checkedLengthCandidateFingerprint trivialCandidate
      assertBool "contract identity was omitted from concrete encoding" $
        LengthProblem.checkedLengthProblemEncodingFingerprint identityProblem /=
          LengthProblem.checkedLengthProblemEncodingFingerprint trivialProblem
      assertBool "changed encoding did not reach complete problem identity" $
        Djex.behavioralProblemFingerprint identityEnvelope /=
          Djex.behavioralProblemFingerprint trivialEnvelope
  , testCase "keep unused inventory identity out of candidate and encoding" $ do
      unusedName <- expectName "Fixture.unusedProblemInventoryValue"
      baseSession <- adversarialLengthSession [] []
      widenedSession <- adversarialLengthSession
        [ ValueDeclaration
            $ ValueSignature () unusedName adversarialClosedList
        ] []
      let target = FunctionType adversarialClosedList adversarialClosedList
      contract <- adversarialLengthContract
        baseSession target identityLengthContract
      graph <- sealAdversarialGraph
        $ adversarialIdentityGraphSource adversarialClosedList
      let candidate = adversarialTypedCandidate $ Right graph
      baseProblem <- expectRight
        $ LengthProblem.sealLengthTypedCandidateProblem
            LengthProblem.defaultLengthProblemLimits
            baseSession contract candidate
      widenedProblem <- expectRight
        $ LengthProblem.sealLengthTypedCandidateProblem
            LengthProblem.defaultLengthProblemLimits
            widenedSession contract candidate
      let baseCandidate = LengthProblem.checkedLengthProblemCandidate baseProblem
          widenedCandidate = LengthProblem.checkedLengthProblemCandidate
            widenedProblem
          baseEnvelope = LengthProblem.checkedLengthProblemBehavioralProblem
            baseProblem
          widenedEnvelope = LengthProblem.checkedLengthProblemBehavioralProblem
            widenedProblem
      assertBool "an unused declaration was omitted from inventory identity" $
        Djex.behavioralProblemInventoryFingerprint baseEnvelope /=
          Djex.behavioralProblemInventoryFingerprint widenedEnvelope
      LengthProblem.checkedLengthCandidateTermGraphFingerprint baseCandidate @?=
        LengthProblem.checkedLengthCandidateTermGraphFingerprint
          widenedCandidate
      LengthProblem.checkedLengthCandidateFingerprint baseCandidate @?=
        LengthProblem.checkedLengthCandidateFingerprint widenedCandidate
      LengthProblem.checkedLengthProblemEncodingFingerprint baseProblem @?=
        LengthProblem.checkedLengthProblemEncodingFingerprint widenedProblem
      assertBool "changed inventory did not reach complete problem identity" $
        Djex.behavioralProblemFingerprint baseEnvelope /=
          Djex.behavioralProblemFingerprint widenedEnvelope
  , testCase "separate structural graph identity from concrete semantics" $ do
      session <- adversarialLengthSession [] []
      let target = FunctionType adversarialClosedList adversarialClosedList
      contract <- adversarialLengthContract
        session target identityLengthContract
      directGraph <- sealAdversarialGraph
        $ adversarialIdentityGraphSource adversarialClosedList
      letGraph <- sealAdversarialGraph
        $ adversarialLetIdentityGraphSource adversarialClosedList
      directProblem <- expectRight
        $ LengthProblem.sealLengthTypedCandidateProblem
            LengthProblem.defaultLengthProblemLimits session contract
            $ adversarialTypedCandidate $ Right directGraph
      letProblem <- expectRight
        $ LengthProblem.sealLengthTypedCandidateProblem
            LengthProblem.defaultLengthProblemLimits session contract
            $ adversarialTypedCandidate $ Right letGraph
      let directCandidate = LengthProblem.checkedLengthProblemCandidate
            directProblem
          letCandidate = LengthProblem.checkedLengthProblemCandidate letProblem
          directEnvelope = LengthProblem.checkedLengthProblemBehavioralProblem
            directProblem
          letEnvelope = LengthProblem.checkedLengthProblemBehavioralProblem
            letProblem
      LengthProblem.checkedLengthCandidateResult directCandidate @?=
        LengthProblem.checkedLengthCandidateResult letCandidate
      assertBool "structurally distinct graphs shared graph identity" $
        LengthProblem.checkedLengthCandidateTermGraphFingerprint
            directCandidate /=
          LengthProblem.checkedLengthCandidateTermGraphFingerprint letCandidate
      assertBool "structurally distinct graphs shared candidate identity" $
        LengthProblem.checkedLengthCandidateFingerprint directCandidate /=
          LengthProblem.checkedLengthCandidateFingerprint letCandidate
      LengthProblem.checkedLengthProblemEncodingFingerprint directProblem @?=
        LengthProblem.checkedLengthProblemEncodingFingerprint letProblem
      assertBool "changed candidate did not reach complete problem identity" $
        Djex.behavioralProblemFingerprint directEnvelope /=
          Djex.behavioralProblemFingerprint letEnvelope
  , testCase "validate candidate work limits before use" $ do
      LengthProblem.mkLengthProblemLimits Djex.defaultTermGraphLimits 0 (-1)
        @?= Left
          (LengthProblem.NegativeLengthProblemEvaluationStepLimit (-1))
      limits <- expectRight $ LengthProblem.mkLengthProblemLimits
        Djex.defaultTermGraphLimits 0 0
      LengthProblem.lengthProblemTermGraphLimits limits @?=
        Djex.defaultTermGraphLimits
      LengthProblem.lengthProblemGraphFingerprintByteLimit limits @?= 0
      LengthProblem.lengthProblemEvaluationStepLimit limits @?= 0
  , testCase "keep an impredicative list payload opaque end to end" $ do
      let binder = FlexibleVariable 1
          payload = ForallType [binder] []
            $ FunctionType (TypeVariable binder) (TypeVariable binder)
      (session, contract, candidate) <- realListIdentityFixture payload
      problem <- expectRight $ LengthProblem.sealLengthTypedCandidateProblem
        LengthProblem.defaultLengthProblemLimits session contract candidate
      LengthProblem.checkedLengthCandidateResult
          (LengthProblem.checkedLengthProblemCandidate problem) @?=
        Length.LengthVariable (Length.LengthInput 0)
      LengthProblem.checkedLengthProblemCounterexampleCondition problem @?=
        Length.LengthTruth False
  , testCase "apply one inventory-owned provider law through a real graph" $ do
      providerName <- expectName "Fixture.lengthDouble"
      let element = FlexibleVariable 0
          providerSpine = TypeApplication (TypeConstructor listName)
            $ TypeVariable element
          providerScheme = ForallType [element] []
            $ FunctionType providerSpine providerSpine
          declaration = ValueDeclaration
            $ ValueSignature () providerName providerScheme
          provider = Length.AssumedProviderSummary
            { Length.lengthProviderName = providerName
            , Length.lengthProviderScheme = providerScheme
            , Length.lengthProviderArgumentRoles =
                [Length.LengthSpineArgument]
            , Length.lengthProviderTransfer = Length.LengthScale 2
                $ Length.LengthVariable $ Length.LengthProviderArgument 0
            }
      environment <- expectRight
        (Djex.mkEnvironment [declaration] ::
          Either (Djex.EnvironmentError Djex.ExferenceTypeVariable)
            Djex.ExferenceEnvironment)
      exferenceSession <- expectRight $ Djex.mkExferenceSession environment
      session <- expectRight $ LengthProblem.sealLengthSession
        Length.defaultLengthLimits
        (Djex.exferenceSessionInventory exferenceSession)
        Length.BuiltinListSpine [provider]
      targetName <- expectName "lengthProviderCandidate"
      target <- expectRight $ Djex.mkDefinitionName targetName
      let goalElement = FlexibleVariable 1
          goalSpine = TypeApplication (TypeConstructor listName)
            $ TypeVariable goalElement
          goal = FunctionType goalSpine goalSpine
          query = Djex.QueryRequest
            { Djex.requestTarget = target
            , Djex.requestGoal = goal
            , Djex.requestContexts = []
            , Djex.requestOptions = Djex.defaultExferenceOptions
                { Djex.exferenceMaximumSteps = 32 }
            }
      request <- expectRight $ Djex.mkExferenceRequest query
      contract <- expectRight $ Length.sealLengthContractInContext
        Length.defaultLengthLimits
        (LengthProblem.checkedLengthSessionContext session)
        goal identityLengthContract
      results <- expectRight
        $ Djex.runExferenceTypedQuery exferenceSession request
      candidate <- case
          [ value
          | result <- results
          , value <- Djex.batchCandidates $ Djex.resultSearch result
          , Right graph <- [Djex.typedCandidateTermGraph value]
          , any (isNamedGlobal providerName . snd) $ Djex.termGraphNodes graph
          ] of
        [] -> assertFailure "the provider query returned no retained provider"
        value : _ -> pure value
      problem <- expectRight $ LengthProblem.sealLengthTypedCandidateProblem
        LengthProblem.defaultLengthProblemLimits session contract candidate
      let checkedCandidate = LengthProblem.checkedLengthProblemCandidate problem
      LengthProblem.checkedLengthCandidateResult checkedCandidate @?=
        Length.LengthScale 2
          (Length.LengthVariable $ Length.LengthInput 0)
      LengthProblem.checkedLengthCandidateUsedProviders checkedCandidate @?=
        [providerName]
  , testCase "reject residual dictionaries before inspecting graph semantics" $ do
      className <- expectName "C"
      producerName <- expectName "Fixture.produceList"
      let element = FlexibleVariable 0
          elementType = TypeVariable element
          resultType = TypeApplication (TypeConstructor listName) elementType
          declarations =
            [ ClassDeclaration () className
                [TypeParameter element Nothing] [] []
            , ValueDeclaration $ ValueSignature () producerName
                $ ForallType [element]
                    [Constraint className [elementType]] resultType
            ]
      environment <- expectRight
        (Djex.mkEnvironment declarations ::
          Either (Djex.EnvironmentError Djex.ExferenceTypeVariable)
            Djex.ExferenceEnvironment)
      exferenceSession <- expectRight $ Djex.mkExferenceSession environment
      session <- expectRight $ LengthProblem.sealLengthSession
        Length.defaultLengthLimits
        (Djex.exferenceSessionInventory exferenceSession)
        Length.BuiltinListSpine []
      targetName <- expectName "residualLengthCandidate"
      target <- expectRight $ Djex.mkDefinitionName targetName
      let goalElement = FlexibleVariable 1
          goal = TypeApplication (TypeConstructor listName)
            $ TypeVariable goalElement
          query = Djex.QueryRequest
            { Djex.requestTarget = target
            , Djex.requestGoal = goal
            , Djex.requestContexts = []
            , Djex.requestOptions = Djex.defaultExferenceOptions
                { Djex.exferenceAllowResidualConstraints = True
                , Djex.exferenceMaximumSteps = 64
                }
            }
      request <- expectRight $ Djex.mkExferenceRequest query
      contract <- expectRight $ Length.sealLengthContractInContext
        Length.defaultLengthLimits
        (LengthProblem.checkedLengthSessionContext session)
        goal trivialLengthContract
      results <- expectRight
        $ Djex.runExferenceTypedQuery exferenceSession request
      (candidate, residual) <- case
          [ (value, firstResidual)
          | result <- results
          , value <- Djex.batchCandidates $ Djex.resultSearch result
          , firstResidual : _ <-
              [Djex.candidateResidualConstraints
                $ Djex.typedCandidateCompatibility value]
          ] of
        [] -> assertFailure "the constrained query returned no residual"
        value : _ -> pure value
      assertLeft (LengthProblem.LengthProblemResidualConstraint residual)
        $ LengthProblem.sealLengthTypedCandidateProblem
            LengthProblem.defaultLengthProblemLimits
            session contract candidate
  , testCase "interpret an inventory-authorized zero constructor" $ do
      typeName <- expectName "Fixture.CandidateSpine"
      zeroName <- expectName "Fixture.CandidateEmpty"
      stepName <- expectName "Fixture.CandidateStep"
      let declaredElement = FlexibleVariable 0
          declaredSpine = TypeApplication (TypeConstructor typeName)
            $ TypeVariable declaredElement
          declaration = DataTypeDeclaration () typeName
            [TypeParameter declaredElement Nothing]
            [ DataConstructor () zeroName []
            , DataConstructor () stepName
                [TypeVariable declaredElement, declaredSpine]
            ]
      environment <- expectRight
        (Djex.mkEnvironment [declaration] ::
          Either (Djex.EnvironmentError Djex.ExferenceTypeVariable)
            Djex.ExferenceEnvironment)
      exferenceSession <- expectRight $ Djex.mkExferenceSession environment
      session <- expectRight $ LengthProblem.sealLengthSession
        Length.defaultLengthLimits
        (Djex.exferenceSessionInventory exferenceSession)
        (Length.DeclaredListSpine typeName zeroName stepName) []
      targetName <- expectName "emptyLengthCandidate"
      target <- expectRight $ Djex.mkDefinitionName targetName
      let element = FlexibleVariable 0
          goal = TypeApplication (TypeConstructor typeName)
            $ TypeVariable element
          query = Djex.QueryRequest
            { Djex.requestTarget = target
            , Djex.requestGoal = goal
            , Djex.requestContexts = []
            , Djex.requestOptions = Djex.defaultExferenceOptions
                { Djex.exferenceMaximumSteps = 16 }
            }
          contractSource = contractWith (Length.LengthTruth True)
            $ Length.LengthEqual
                (Length.LengthVariable Length.LengthResult)
                (Length.LengthLiteral 0)
      request <- expectRight $ Djex.mkExferenceRequest query
      contract <- expectRight $ Length.sealLengthContractInContext
        Length.defaultLengthLimits
        (LengthProblem.checkedLengthSessionContext session)
        goal contractSource
      results <- expectRight
        $ Djex.runExferenceTypedQuery exferenceSession request
      candidate <- case
          [ value
          | result <- results
          , value <- Djex.batchCandidates $ Djex.resultSearch result
          , Right graph <- [Djex.typedCandidateTermGraph value]
          , any (isNamedGlobal zeroName . snd) $ Djex.termGraphNodes graph
          ] of
        [] -> assertFailure "the empty-list query returned no zero constructor"
        value : _ -> pure value
      problem <- expectRight $ LengthProblem.sealLengthTypedCandidateProblem
        LengthProblem.defaultLengthProblemLimits session contract candidate
      LengthProblem.checkedLengthCandidateResult
          (LengthProblem.checkedLengthProblemCandidate problem) @?=
        Length.LengthLiteral 0
      LengthProblem.checkedLengthProblemCounterexampleCondition problem @?=
        Length.LengthTruth False
  , testCase "interpret the recursive field of an authorized step" $ do
      typeName <- expectName "Fixture.StepSpine"
      zeroName <- expectName "Fixture.StepEmpty"
      stepName <- expectName "Fixture.StepLink"
      let declaredElement = FlexibleVariable 0
          declaredSpine = TypeApplication (TypeConstructor typeName)
            $ TypeVariable declaredElement
          declaration = DataTypeDeclaration () typeName
            [TypeParameter declaredElement Nothing]
            [ DataConstructor () zeroName []
            , DataConstructor () stepName
                [TypeVariable declaredElement, declaredSpine]
            ]
      environment <- expectRight
        (Djex.mkEnvironment [declaration] ::
          Either (Djex.EnvironmentError Djex.ExferenceTypeVariable)
            Djex.ExferenceEnvironment)
      exferenceSession <- expectRight $ Djex.mkExferenceSession environment
      session <- expectRight $ LengthProblem.sealLengthSession
        Length.defaultLengthLimits
        (Djex.exferenceSessionInventory exferenceSession)
        (Length.DeclaredListSpine typeName zeroName stepName) []
      targetName <- expectName "stepLengthCandidate"
      target <- expectRight $ Djex.mkDefinitionName targetName
      let element = FlexibleVariable 1
          innerSpine = TypeApplication (TypeConstructor typeName)
            $ TypeVariable element
          outerSpine = TypeApplication (TypeConstructor typeName) innerSpine
          goal = FunctionType innerSpine
            $ FunctionType outerSpine outerSpine
          expectedResult = Length.LengthSum
            [ Length.LengthVariable $ Length.LengthInput 1
            , Length.LengthLiteral 1
            ]
          query = Djex.QueryRequest
            { Djex.requestTarget = target
            , Djex.requestGoal = goal
            , Djex.requestContexts = []
            , Djex.requestOptions = Djex.defaultExferenceOptions
                { Djex.exferenceMaximumSteps = 32 }
            }
          contractSource = contractWith (Length.LengthTruth True)
            $ Length.LengthEqual
                (Length.LengthVariable Length.LengthResult) expectedResult
      request <- expectRight $ Djex.mkExferenceRequest query
      contract <- expectRight $ Length.sealLengthContractInContext
        Length.defaultLengthLimits
        (LengthProblem.checkedLengthSessionContext session)
        goal contractSource
      results <- expectRight
        $ Djex.runExferenceTypedQuery exferenceSession request
      candidate <- case
          [ value
          | result <- results
          , value <- Djex.batchCandidates $ Djex.resultSearch result
          , Right graph <- [Djex.typedCandidateTermGraph value]
          , any (isNamedGlobal stepName . snd) $ Djex.termGraphNodes graph
          ] of
        [] -> assertFailure "the step query returned no step constructor"
        value : _ -> pure value
      problem <- expectRight $ LengthProblem.sealLengthTypedCandidateProblem
        LengthProblem.defaultLengthProblemLimits session contract candidate
      LengthProblem.checkedLengthCandidateResult
          (LengthProblem.checkedLengthProblemCandidate problem) @?=
        expectedResult
      LengthProblem.checkedLengthProblemCounterexampleCondition problem @?=
        Length.LengthTruth False
  , testCase "require fresh distinct rigid root openings" $ do
      session <- adversarialLengthSession [] []
      let flexible = FlexibleVariable "root-flexible"
          existingRigid = RigidVariable "root-existing-rigid"
          freshRigid = RigidVariable "root-fresh-rigid"
          flexibleSpine = adversarialListOf $ TypeVariable flexible
          existingSpine = adversarialListOf $ TypeVariable existingRigid
          freshSpine = adversarialListOf $ TypeVariable freshRigid
          flexibleTarget = FunctionType flexibleSpine flexibleSpine
      contract <- adversarialLengthContract
        session flexibleTarget identityLengthContract

      flexibleGraph <- sealAdversarialGraph
        $ adversarialIdentityGraphSource flexibleSpine
      assertLeft
        (LengthProblem.LengthProblemRootOpeningRejected
          $ LengthProblem.LengthRootOpeningSelectionIsNotRigid 0)
        $ LengthProblem.sealLengthTypedCandidateProblem
            LengthProblem.defaultLengthProblemLimits session contract
            $ adversarialTypedCandidate $ Right flexibleGraph

      let collidingTarget = FunctionType flexibleSpine existingSpine
      collidingContract <- adversarialLengthContract
        session collidingTarget trivialLengthContract
      collidingGraph <- sealAdversarialGraph
        $ adversarialIdentityGraphSource existingSpine
      assertLeft
        (LengthProblem.LengthProblemRootOpeningRejected
          $ LengthProblem.LengthRootOpeningRigidIsNotInjective
              0 "root-existing-rigid")
        $ LengthProblem.sealLengthTypedCandidateProblem
            LengthProblem.defaultLengthProblemLimits session collidingContract
            $ adversarialTypedCandidate $ Right collidingGraph

      freshGraph <- sealAdversarialGraph
        $ adversarialIdentityGraphSource freshSpine
      freshProblem <- expectRight
        $ LengthProblem.sealLengthTypedCandidateProblem
            LengthProblem.defaultLengthProblemLimits session contract
            $ adversarialTypedCandidate $ Right freshGraph
      LengthProblem.checkedLengthCandidateResult
          (LengthProblem.checkedLengthProblemCandidate freshProblem) @?=
        Length.LengthVariable (Length.LengthInput 0)
  , testCase "interpret an inventory-owned zero-arity provider" $ do
      providerName <- expectName "Fixture.zeroArityLengthProvider"
      let providerScheme = adversarialClosedList
          provider = Length.AssumedProviderSummary
            { Length.lengthProviderName = providerName
            , Length.lengthProviderScheme = providerScheme
            , Length.lengthProviderArgumentRoles = []
            , Length.lengthProviderTransfer = Length.LengthLiteral 7
            }
          declaration = ValueDeclaration
            $ ValueSignature () providerName providerScheme
          contractSource = contractWith (Length.LengthTruth True)
            $ Length.LengthEqual
                (Length.LengthVariable Length.LengthResult)
                (Length.LengthLiteral 7)
      session <- adversarialLengthSession [declaration] [provider]
      contract <- adversarialLengthContract session providerScheme contractSource
      graph <- sealAdversarialGraph $ Djex.TermGraphSource
        (Djex.termNodeId 0)
        [ ( Djex.termNodeId 0
          , Djex.TermNode providerScheme
              $ Djex.TypedGlobal (Djex.occurrenceId 0) providerName
          )
        ]
      problem <- expectRight $ LengthProblem.sealLengthTypedCandidateProblem
        LengthProblem.defaultLengthProblemLimits session contract
        $ adversarialTypedCandidate $ Right graph
      let checked = LengthProblem.checkedLengthProblemCandidate problem
      LengthProblem.checkedLengthCandidateResult checked @?=
        Length.LengthLiteral 7
      LengthProblem.checkedLengthCandidateUsedProviders checked @?=
        [providerName]
      LengthProblem.checkedLengthProblemCounterexampleCondition problem @?=
        Length.LengthTruth False
  , testCase "reject a visible selection outside the root rigid scope" $ do
      providerName <- expectName "Fixture.visibleLengthProvider"
      let binder = FlexibleVariable "provider-binder"
          escaped = RigidVariable "escaped-visible-selection"
          providerScheme = ForallType [binder] []
            $ adversarialListOf $ TypeVariable binder
          selected = TypeVariable escaped
          selectedSpine = adversarialListOf selected
          provider = Length.AssumedProviderSummary
            { Length.lengthProviderName = providerName
            , Length.lengthProviderScheme = providerScheme
            , Length.lengthProviderArgumentRoles = []
            , Length.lengthProviderTransfer = Length.LengthLiteral 0
            }
          declaration = ValueDeclaration
            $ ValueSignature () providerName providerScheme
          witness = Djex.TypeApplicationWitness
            providerScheme selected selectedSpine Nothing
          ignored = Djex.TypedPattern
            (Djex.occurrenceId 2) selectedSpine Djex.TypedWildcard
          source = Djex.TermGraphSource (Djex.termNodeId 3)
            [ ( Djex.termNodeId 0
              , Djex.TermNode providerScheme
                  $ Djex.TypedGlobal (Djex.occurrenceId 0) providerName
              )
            , ( Djex.termNodeId 1
              , Djex.TermNode selectedSpine
                  $ Djex.TypedVisibleTypeApplication
                      (Djex.occurrenceId 1)
                      (Djex.termNodeId 0)
                      Djex.inferredVisibleTypeArgument
                      witness
              )
            , ( Djex.termNodeId 2
              , Djex.TermNode adversarialClosedList
                  $ Djex.TypedGlobal (Djex.occurrenceId 3) listName
              )
            , ( Djex.termNodeId 3
              , Djex.TermNode adversarialClosedList
                  $ Djex.TypedLet ignored
                      (Djex.termNodeId 1) (Djex.termNodeId 2)
              )
            ]
      session <- adversarialLengthSession [declaration] [provider]
      contract <- adversarialLengthContract
        session adversarialClosedList trivialLengthContract
      graph <- sealAdversarialGraph source
      assertLeft
        (LengthProblem.LengthProblemVisibleTypeSelectionRejected
          (Djex.termNodeId 1) selected)
        $ LengthProblem.sealLengthTypedCandidateProblem
            LengthProblem.defaultLengthProblemLimits session contract
            $ adversarialTypedCandidate $ Right graph
  , testCase "admit closed visible selections at their inferred binder kind" $ do
      higherProvider <- expectName "Fixture.higherKindedVisibleProvider"
      let constructorBinder = FlexibleVariable "constructor-binder"
          unitType = TupleType Boxed []
          higherScheme = ForallType [constructorBinder] []
            $ adversarialListOf
            $ TypeApplication (TypeVariable constructorBinder) unitType
          higherSelection = TypeConstructor listName
          higherResult = adversarialListOf
            $ adversarialListOf unitType
      higherProblem <- expectRight
        =<< adversarialVisibleProviderProblem
          higherProvider higherScheme higherSelection higherResult Nothing
      let higherCandidate = LengthProblem.checkedLengthProblemCandidate
            higherProblem
      LengthProblem.checkedLengthCandidateResult higherCandidate @?=
        Length.LengthLiteral 0
      LengthProblem.checkedLengthCandidateUsedProviders higherCandidate @?=
        [higherProvider]

      impredicativeProvider <- expectName
        "Fixture.impredicativeVisibleProvider"
      let elementBinder = FlexibleVariable "element-binder"
          innerBinder = FlexibleVariable "inner-binder"
          impredicativeScheme = ForallType [elementBinder] []
            $ adversarialListOf $ TypeVariable elementBinder
          impredicativeSelection = ForallType [innerBinder] []
            $ FunctionType
                (TypeVariable innerBinder) (TypeVariable innerBinder)
          impredicativeResult = adversarialListOf impredicativeSelection
      impredicativeProblem <- expectRight
        =<< adversarialVisibleProviderProblem
          impredicativeProvider
          impredicativeScheme
          impredicativeSelection
          impredicativeResult
          Nothing
      LengthProblem.checkedLengthCandidateResult
          (LengthProblem.checkedLengthProblemCandidate impredicativeProblem) @?=
        Length.LengthLiteral 0
  , testCase "reject certificate-bearing visible applications at graph identity" $ do
      providerName <- expectName "Fixture.certifiedVisibleProvider"
      let binder = FlexibleVariable "certified-binder"
          selected = TupleType Boxed []
          scheme = ForallType [binder] []
            $ adversarialListOf $ TypeVariable binder
          result = adversarialListOf selected
          certificate = Djex.certificateId 37
      assertLeft
        (LengthProblem.LengthProblemTermGraphFingerprintRejected
          $ Djex.TermGraphFingerprintUnsupportedCertificate certificate)
        =<< adversarialVisibleProviderProblem
          providerName scheme selected result (Just (certificate, 0))
  , testCase "kind-check every graph annotation in the exact session" $ do
      unknownTypeName <- expectName "Fixture.UnknownGraphType"
      unknownValueName <- expectName "Fixture.unknownGraphValue"
      let unknownType = TypeConstructor unknownTypeName
          ignored = Djex.TypedPattern
            (Djex.occurrenceId 1) unknownType Djex.TypedWildcard
          source = Djex.TermGraphSource (Djex.termNodeId 2)
            [ ( Djex.termNodeId 0
              , Djex.TermNode unknownType
                  $ Djex.TypedGlobal (Djex.occurrenceId 0) unknownValueName
              )
            , ( Djex.termNodeId 1
              , Djex.TermNode adversarialClosedList
                  $ Djex.TypedGlobal (Djex.occurrenceId 2) listName
              )
            , ( Djex.termNodeId 2
              , Djex.TermNode adversarialClosedList
                  $ Djex.TypedLet ignored
                      (Djex.termNodeId 0) (Djex.termNodeId 1)
              )
            ]
      session <- adversarialLengthSession [] []
      contract <- adversarialLengthContract
        session adversarialClosedList trivialLengthContract
      graph <- sealAdversarialGraph source
      case LengthProblem.sealLengthTypedCandidateProblem
          LengthProblem.defaultLengthProblemLimits session contract
          (adversarialTypedCandidate $ Right graph) of
        Left LengthProblem.LengthProblemGraphKindRejected{} -> pure ()
        Left other -> assertFailure $ "unexpected rejection: " ++ show other
        Right _ -> assertFailure "an unknown graph type escaped session kinding"
  , testCase "distinguish absent and semantically unmodeled globals" $ do
      missingName <- expectName "Fixture.missingLengthGlobal"
      unmodeledName <- expectName "Fixture.unmodeledLengthGlobal"
      let declaration = ValueDeclaration
            $ ValueSignature () unmodeledName adversarialClosedList
          globalSource name = Djex.TermGraphSource (Djex.termNodeId 0)
            [ ( Djex.termNodeId 0
              , Djex.TermNode adversarialClosedList
                  $ Djex.TypedGlobal (Djex.occurrenceId 0) name
              )
            ]
      session <- adversarialLengthSession [declaration] []
      contract <- adversarialLengthContract
        session adversarialClosedList trivialLengthContract
      missingGraph <- sealAdversarialGraph $ globalSource missingName
      unmodeledGraph <- sealAdversarialGraph $ globalSource unmodeledName
      assertLeft
        (LengthProblem.LengthProblemGlobalNotInSourceInventory
          (Djex.termNodeId 0) missingName)
        $ LengthProblem.sealLengthTypedCandidateProblem
            LengthProblem.defaultLengthProblemLimits session contract
            $ adversarialTypedCandidate $ Right missingGraph
      assertLeft
        (LengthProblem.LengthProblemGlobalHasNoLengthSummary
          (Djex.termNodeId 0) unmodeledName)
        $ LengthProblem.sealLengthTypedCandidateProblem
            LengthProblem.defaultLengthProblemLimits session contract
            $ adversarialTypedCandidate $ Right unmodeledGraph
  , testCase "retain typed-graph absence after contract resealing" $ do
      session <- adversarialLengthSession [] []
      contract <- adversarialLengthContract
        session adversarialClosedList trivialLengthContract
      assertLeft (LengthProblem.LengthProblemTypedGraphUnavailable
          "fixture graph unavailable")
        $ LengthProblem.sealLengthTypedCandidateProblem
            LengthProblem.defaultLengthProblemLimits session contract
            $ adversarialTypedCandidate $ Left "fixture graph unavailable"
  , testCase "bound graph identity before symbolic evaluation" $ do
      (session, contract, candidate) <- realListIdentityFixture
        $ TypeVariable $ FlexibleVariable 0
      noGraphBytes <- expectRight $ LengthProblem.mkLengthProblemLimits
        Djex.defaultTermGraphLimits 0 65536
      assertLeft
        (LengthProblem.LengthProblemTermGraphFingerprintRejected
          $ Djex.TermGraphFingerprintByteLimitExceeded 0 1)
        $ LengthProblem.sealLengthTypedCandidateProblem
            noGraphBytes session contract candidate
      noEvaluation <- expectRight $ LengthProblem.mkLengthProblemLimits
        Djex.defaultTermGraphLimits
        Djex.defaultTermGraphFingerprintByteLimit 0
      assertLeft
        (LengthProblem.LengthProblemEvaluationStepLimitExceeded 0 1)
        $ LengthProblem.sealLengthTypedCandidateProblem
            noEvaluation session contract candidate
  ]

problemReplayTests :: TestTree
problemReplayTests = testGroup "exact candidate problem replay"
  [ testCase "find no counterexample for one real Exference identity" $ do
      (session, contract, candidate) <- realListIdentityFixture
        $ TypeVariable $ FlexibleVariable 0
      problem <- expectRight $ LengthProblem.sealLengthTypedCandidateProblem
        LengthProblem.defaultLengthProblemLimits session contract candidate
      LengthProblem.checkedLengthProblemInputCount problem @?= 1
      mapM_ (\input -> expectNoCounterexample
        $ Evaluate.validateLengthProblemCounterexample
            Evaluate.defaultLengthEvaluationLimits problem
            $ Evaluate.LengthProblemAssignment [input]) [0, 1, 8]
  , testCase "compute a violating candidate result and bind its evidence" $ do
      problem <- adversarialConstantZeroProblem identityLengthContract
      LengthProblem.checkedLengthProblemInputCount problem @?= 1
      evidence <- expectCounterexample
        $ Evaluate.validateLengthProblemCounterexample
            Evaluate.defaultLengthEvaluationLimits problem
            $ Evaluate.LengthProblemAssignment [3]
      receipt <- expectRight $ Djex.replayBehavioralEvidence
        (LengthProblem.checkedLengthProblemBehavioralProblem problem) evidence
      Evaluate.validatedLengthCounterexampleInputs receipt @?= [3]
      Evaluate.validatedLengthCounterexampleResult receipt @?= 0
      Evaluate.validatedLengthCounterexampleBasis receipt @?=
        Evaluate.ProviderIndependentFiniteSpineModel

      -- The assignment API has no result field: zero above is recomputed from
      -- the sealed candidate rather than accepted from a model decoder.
      expectNoCounterexample
        $ Evaluate.validateLengthProblemCounterexample
            Evaluate.defaultLengthEvaluationLimits problem
            $ Evaluate.LengthProblemAssignment [0]

      differentEncoding <- adversarialConstantZeroProblem trivialLengthContract
      assertLeft Djex.ReplayEncodingFingerprintMismatch
        $ Djex.replayBehavioralEvidence
            (LengthProblem.checkedLengthProblemBehavioralProblem differentEncoding)
            evidence
  , testCase "reject problem assignment shape and values productively" $ do
      problem <- adversarialConstantZeroProblem identityLengthContract
      let replay limits inputs = Evaluate.validateLengthProblemCounterexample
            limits problem $ Evaluate.LengthProblemAssignment inputs
      assertLeft (Evaluate.LengthProblemAssignmentArityMismatch 1 0)
        $ replay Evaluate.defaultLengthEvaluationLimits []
      assertLeft (Evaluate.LengthProblemAssignmentArityMismatch 1 2)
        $ replay Evaluate.defaultLengthEvaluationLimits
            [0, 2 ^ (5000 :: Int)]
      assertLeft
        (Evaluate.LengthEvaluationValueBitLimitExceeded
          (Evaluate.LengthProblemInputValue 0) 2 3)
        $ replay (evaluationLimitsWith 2 8) [4]

      let cyclicInputs = 0 : cyclicInputs
      cyclicResult <- evaluateWithin
        $ replay Evaluate.defaultLengthEvaluationLimits cyclicInputs
      assertLeft (Evaluate.LengthProblemAssignmentArityMismatch 1 2)
        cyclicResult
  , testCase "short-circuit an input-dependent false precondition" $ do
      let input = Length.LengthVariable $ Length.LengthInput 0
          result = Length.LengthVariable Length.LengthResult
          precondition = Length.LengthEqual input $ Length.LengthLiteral 0
          postcondition = Length.LengthEqual result $ Length.LengthLiteral 0
          source = contractWith precondition postcondition
      problem <- adversarialConstantProviderProblem 7 source
      LengthProblem.checkedLengthProblemInputCount problem @?= 1
      LengthProblem.checkedLengthCandidateResult
          (LengthProblem.checkedLengthProblemCandidate problem) @?=
        Length.LengthLiteral 7
      assertBool "the retained precondition was collapsed to false" $
        LengthProblem.checkedLengthProblemPrecondition problem /=
          Length.LengthTruth False
      case LengthProblem.checkedLengthProblemPostcondition problem of
        Length.LengthEqual left right -> assertBool
          "the retained postcondition no longer references the result"
          $ left == result || right == result
        other -> assertFailure $ "unexpected retained postcondition: " ++ show other

      -- Input one makes the precondition false.  If replay forces either the
      -- candidate's literal seven or the result-dependent postcondition, the
      -- zero-bit intermediate limit rejects it instead of returning Nothing.
      expectNoCounterexample
        $ Evaluate.validateLengthProblemCounterexample
            (evaluationLimitsWith 1 0) problem
            $ Evaluate.LengthProblemAssignment [1]

      evidence <- expectCounterexample
        $ Evaluate.validateLengthProblemCounterexample
            Evaluate.defaultLengthEvaluationLimits problem
            $ Evaluate.LengthProblemAssignment [0]
      receipt <- expectRight $ Djex.replayBehavioralEvidence
        (LengthProblem.checkedLengthProblemBehavioralProblem problem) evidence
      providerName <- expectName "Fixture.problemReplayConstant"
      Evaluate.validatedLengthCounterexampleInputs receipt @?= [0]
      Evaluate.validatedLengthCounterexampleResult receipt @?= 7
      Evaluate.validatedLengthCounterexampleBasis receipt @?=
        Evaluate.FiniteSpineModelUnderAssumedProviderLaws [providerName]

      -- With a true result-independent postcondition, replay also leaves the
      -- shared candidate-result computation unforced after precondition
      -- success.
      irrelevantResult <- adversarialConstantProviderProblem
        7 trivialLengthContract
      expectNoCounterexample
        $ Evaluate.validateLengthProblemCounterexample
            (evaluationLimitsWith 0 0) irrelevantResult
            $ Evaluate.LengthProblemAssignment [0]

      -- Canonical conjunction ordering used to move this bad-postcondition
      -- clause ahead of the false precondition.  Evaluating the scaled result
      -- at input one exceeds the one-bit intermediate bound, so returning no
      -- counterexample demonstrates that replay no longer consumes the sorted
      -- combined formula as its operational plan.
      let orderedPrecondition = Length.LengthNot
            $ Length.LengthAtMost input $ Length.LengthLiteral 1
          orderedSource = contractWith orderedPrecondition postcondition
      scaledResult <- adversarialScaledProviderProblem 2 orderedSource
      case LengthProblem.checkedLengthProblemCounterexampleCondition
          scaledResult of
        Length.LengthAll [firstClause, _] -> assertBool
          "the adversarial combined formula did not reorder its bad post first"
          $ firstClause /=
              LengthProblem.checkedLengthProblemPrecondition scaledResult
        other -> assertFailure $ "unexpected adversarial condition: " ++ show other
      expectNoCounterexample
        $ Evaluate.validateLengthProblemCounterexample
            (evaluationLimitsWith 1 1) scaledResult
            $ Evaluate.LengthProblemAssignment [1]
  ]

smtLibTests :: TestTree
smtLibTests = testGroup
  "bounded QF_LIA query, execution policy, response, and model boundary"
  [ testCase "publish one pure fixed Z3 execution-policy default" $ do
      SMTLibExecution.lengthSMTLibExecutionPolicySchemaTag @?=
        asciiBytes "djex-length-z3-smtlib2-execution-policy/v2"
      SMTLibExecution.lengthSMTLibExecutionProtocolSchemaTag @?=
        asciiBytes "djex-length-z3-smtlib2-session-protocol/v1"
      SMTLibExecution.lengthSMTLibExecutionArgumentPrefix @?=
        ["-in", "-smt2", "smtlib2_compliant=true"]
      SMTLibExecution.lengthSMTLibExecutionArgumentVector @?=
        SMTLibExecution.lengthSMTLibExecutionArgumentPrefix
      SMTLibExecution.lengthSMTLibExecutionStartupCommandBytes @?=
        asciiBytes "(set-option :print-success false)\n"
      SMTLibExecution.lengthSMTLibExecutionQueryResetBytes @?=
        asciiBytes "(reset)\n(set-option :print-success false)\n"
      SMTLibExecution.lengthSMTLibExecutionEnvironmentPolicyTag @?=
        asciiBytes "empty-environment/v1"
      SMTLibExecution.lengthSMTLibExecutionWorkingDirectoryPolicyTag @?=
        asciiBytes "fresh-empty-working-directory/v1"
      SMTLibExecution.lengthSMTLibExecutionExpectedDigestSchemaTag @?=
        asciiBytes "sha256/exact-executable-file-bytes/v1"
      SMTLibExecution.lengthSMTLibMinimumHostDeadlineMarginMilliseconds @?= 100
      assertBool "validated execution defaults changed representation"
        $ SMTLibExecution.mkLengthSMTLibExecutionLimits
            SMTLibExecution.defaultLengthSMTLibExecutionLimitSource ==
          SMTLibExecution.defaultLengthSMTLibExecutionLimits
      SMTLibExecution.lengthSMTLibExecutionExecutablePathCharacterLimit
          SMTLibExecution.defaultLengthSMTLibExecutionLimits @?= 4096
      SMTLibExecution.lengthSMTLibExecutionPolicyFingerprintByteLimit
          SMTLibExecution.defaultLengthSMTLibExecutionLimits @?= 262144
      config <- expectRight $ SMTLibExecution.mkLengthSMTLibExecutionConfig
        SMTLibExecution.defaultLengthSMTLibExecutionLimits
        $ SMTLibExecution.defaultLengthSMTLibExecutionConfigSource
            absoluteFixtureExecutable Nothing
      SMTLibExecution.lengthSMTLibExecutionSolverTimeoutMilliseconds config
        @?= 1000
      SMTLibExecution.lengthSMTLibExecutionSolverResourceLimit config
        @?= 100000
      SMTLibExecution.lengthSMTLibExecutionConfiguredArgumentVector config @?=
        [ "-in"
        , "-smt2"
        , "smtlib2_compliant=true"
        , "timeout=1000"
        , "rlimit=100000"
        ]
      nondefaultConfig <- expectRight
        $ SMTLibExecution.mkLengthSMTLibExecutionConfig
            SMTLibExecution.defaultLengthSMTLibExecutionLimits
        $ (SMTLibExecution.defaultLengthSMTLibExecutionConfigSource
            absoluteFixtureExecutable Nothing)
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceSolverTimeoutMilliseconds =
                17
            , SMTLibExecution.lengthSMTLibExecutionConfigSourceSolverResourceLimit =
                23
            }
      SMTLibExecution.lengthSMTLibExecutionConfiguredArgumentVector
          nondefaultConfig @?=
        [ "-in"
        , "-smt2"
        , "smtlib2_compliant=true"
        , "timeout=17"
        , "rlimit=23"
        ]
      SMTLibExecution.lengthSMTLibExecutionHostDeadlineMilliseconds config
        @?= 1500
      SMTLibExecution.lengthSMTLibExecutionArtifactPolicy config @?=
        SMTLibExecution.LengthSMTLibInputValuesAfterSatisfiable
      SMTLibExecution.lengthSMTLibExecutionResponseLimits config @?=
        SMTLibResponse.defaultLengthSMTLibResponseLimits
  , testCase "reject signed policy fields in declaration order" $ do
      let defaults = SMTLibExecution.defaultLengthSMTLibExecutionConfigSource
            "relative-z3" (Just [0])
          seal source = SMTLibExecution.mkLengthSMTLibExecutionConfig
            SMTLibExecution.defaultLengthSMTLibExecutionLimits source
      assertLeft
        (SMTLibExecution.NegativeLengthSMTLibExecutionConfigField
          SMTLibExecution.LengthSMTLibExecutionSolverTimeoutMilliseconds (-1))
        $ seal defaults
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceSolverTimeoutMilliseconds = -1
            , SMTLibExecution.lengthSMTLibExecutionConfigSourceSolverResourceLimit = -1
            , SMTLibExecution.lengthSMTLibExecutionConfigSourceHostDeadlineMilliseconds = -1
            }
      assertLeft
        (SMTLibExecution.NegativeLengthSMTLibExecutionConfigField
          SMTLibExecution.LengthSMTLibExecutionSolverResourceLimit (-1))
        $ seal defaults
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceSolverResourceLimit = -1
            , SMTLibExecution.lengthSMTLibExecutionConfigSourceHostDeadlineMilliseconds = -1
            }
      assertLeft
        (SMTLibExecution.NegativeLengthSMTLibExecutionConfigField
          SMTLibExecution.LengthSMTLibExecutionHostDeadlineMilliseconds (-1))
        $ seal defaults
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceHostDeadlineMilliseconds = -1
            }
  , testCase "bound and validate executable paths and digest pins productively" $ do
      let defaults path digest =
            SMTLibExecution.defaultLengthSMTLibExecutionConfigSource path digest
          sealWith limits = SMTLibExecution.mkLengthSMTLibExecutionConfig limits
          seal = sealWith SMTLibExecution.defaultLengthSMTLibExecutionLimits
      assertLeft SMTLibExecution.LengthSMTLibExecutionEmptyExecutablePath
        $ seal $ defaults "" Nothing
      assertLeft SMTLibExecution.LengthSMTLibExecutionExecutablePathNotAbsolute
        $ seal $ defaults "z3" $ Just []
      assertLeft
        (SMTLibExecution.LengthSMTLibExecutionInvalidExecutablePathCharacter 3
          SMTLibExecution.LengthSMTLibExecutionPathContainsNul)
        $ seal $ defaults (absoluteFixturePrefix ++ ['\0']) Nothing
      assertLeft
        (SMTLibExecution.LengthSMTLibExecutionInvalidExecutablePathCharacter 3
          SMTLibExecution.LengthSMTLibExecutionPathContainsSurrogate)
        $ seal $ defaults (absoluteFixturePrefix ++ [toEnum 0xd800]) Nothing
      let tinyLimits = SMTLibExecution.mkLengthSMTLibExecutionLimits
            SMTLibExecution.defaultLengthSMTLibExecutionLimitSource
              { SMTLibExecution.lengthSMTLibExecutionLimitSourceExecutablePathCharacters = 3 }
      assertLeft
        (SMTLibExecution.LengthSMTLibExecutionExecutablePathCharacterLimitExceeded
          3 4)
        $ sealWith tinyLimits $ defaults (absoluteFixturePrefix ++ "x") Nothing
      let cyclicTail = 'z' : cyclicTail
          cyclicPath = absoluteFixturePrefix ++ cyclicTail
      cyclicPathResult <- evaluateWithin
        $ sealWith tinyLimits $ defaults cyclicPath Nothing
      assertLeft
        (SMTLibExecution.LengthSMTLibExecutionExecutablePathCharacterLimitExceeded
          3 4)
        cyclicPathResult
      _ <- expectRight
        $ seal $ defaults absoluteFixtureExecutable $ Just $ replicate 32 0
      assertLeft
        (SMTLibExecution.LengthSMTLibExecutionExpectedExecutableSHA256LengthMismatch
          32 31)
        $ seal $ defaults absoluteFixtureExecutable $ Just $ replicate 31 0
      assertLeft
        (SMTLibExecution.LengthSMTLibExecutionExpectedExecutableSHA256LengthMismatch
          32 33)
        $ seal $ defaults absoluteFixtureExecutable $ Just $ replicate 33 0
      let cyclicDigest = 0 : cyclicDigest
      cyclicDigestResult <- evaluateWithin
        $ seal $ defaults absoluteFixtureExecutable $ Just cyclicDigest
      assertLeft
        (SMTLibExecution.LengthSMTLibExecutionExpectedExecutableSHA256LengthMismatch
          32 33)
        cyclicDigestResult
  , testCase "validate finite solver and stricter host time budgets" $ do
      let defaults = SMTLibExecution.defaultLengthSMTLibExecutionConfigSource
            absoluteFixtureExecutable Nothing
          seal source = SMTLibExecution.mkLengthSMTLibExecutionConfig
            SMTLibExecution.defaultLengthSMTLibExecutionLimits source
          fieldError field maximumValue observed =
            SMTLibExecution.LengthSMTLibExecutionConfigFieldAboveMaximum
              field maximumValue observed
      assertLeft
        (SMTLibExecution.ZeroLengthSMTLibExecutionConfigField
          SMTLibExecution.LengthSMTLibExecutionSolverTimeoutMilliseconds)
        $ seal defaults
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceSolverTimeoutMilliseconds = 0 }
      assertLeft
        (SMTLibExecution.ZeroLengthSMTLibExecutionConfigField
          SMTLibExecution.LengthSMTLibExecutionSolverResourceLimit)
        $ seal defaults
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceSolverResourceLimit = 0 }
      let word32Maximum = 4294967295 :: Integer
      if toInteger (maxBound :: Int) >= word32Maximum
        then do
          let maximumInt = fromInteger word32Maximum
          assertLeft
            (fieldError
              SMTLibExecution.LengthSMTLibExecutionSolverTimeoutMilliseconds
              (word32Maximum - 1) word32Maximum)
            $ seal defaults
                { SMTLibExecution.lengthSMTLibExecutionConfigSourceSolverTimeoutMilliseconds =
                    maximumInt }
          _ <- expectRight $ seal defaults
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceSolverResourceLimit =
                maximumInt }
          pure ()
        else pure ()
      if toInteger (maxBound :: Int) > word32Maximum
        then do
          let aboveMaximumInt = fromInteger $ word32Maximum + 1
          assertLeft
            (fieldError SMTLibExecution.LengthSMTLibExecutionSolverResourceLimit
              word32Maximum (word32Maximum + 1))
            $ seal defaults
                { SMTLibExecution.lengthSMTLibExecutionConfigSourceSolverResourceLimit =
                    aboveMaximumInt }
        else pure ()
      assertLeft
        (SMTLibExecution.LengthSMTLibExecutionHostDeadlineMarginTooSmall
          1000 1099 100)
        $ seal defaults
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceHostDeadlineMilliseconds =
                1099 }
      exactMargin <- expectRight $ seal defaults
        { SMTLibExecution.lengthSMTLibExecutionConfigSourceHostDeadlineMilliseconds =
            1100 }
      SMTLibExecution.lengthSMTLibExecutionHostDeadlineMilliseconds exactMargin
        @?= 1100
      let overflowDeadline = maxBound `div` 1000 + 1
      assertLeft
        (SMTLibExecution.LengthSMTLibExecutionHostDeadlineMicrosecondsOverflow
          overflowDeadline)
        $ seal defaults
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceHostDeadlineMilliseconds =
                overflowDeadline }
  , testCase "bind every configurable policy field into private identity" $ do
      let source = SMTLibExecution.defaultLengthSMTLibExecutionConfigSource
            absoluteFixtureExecutable Nothing
          sealWith limits candidate = expectRight
            $ SMTLibExecution.mkLengthSMTLibExecutionConfig limits candidate
          seal = sealWith SMTLibExecution.defaultLengthSMTLibExecutionLimits
          response change = expectRight
            $ SMTLibResponse.mkLengthSMTLibResponseLimits
            $ change SMTLibResponse.defaultLengthSMTLibResponseLimitSource
      baseline <- seal source
      responseBytes <- response $ \limits -> limits
        { SMTLibResponse.lengthSMTLibResponseLimitSourceBytes = 65535 }
      responseDepth <- response $ \limits -> limits
        { SMTLibResponse.lengthSMTLibResponseLimitSourceNestingDepth = 63 }
      responseNodes <- response $ \limits -> limits
        { SMTLibResponse.lengthSMTLibResponseLimitSourceNodes = 4095 }
      responseTokens <- response $ \limits -> limits
        { SMTLibResponse.lengthSMTLibResponseLimitSourceTokenBytes = 4095 }
      responseIntegers <- response $ \limits -> limits
        { SMTLibResponse.lengthSMTLibResponseLimitSourceIntegerBits = 4095 }
      changed <- mapM seal
        [ source
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceExecutablePath =
                absoluteFixtureExecutable ++ "-other" }
        , source
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceExecutablePath =
                absoluteFixtureExecutable ++ "-\945" }
        , source
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceExecutablePath =
                absoluteFixtureExecutable ++ [toEnum 0x0101] }
        , source
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceExecutablePath =
                absoluteFixtureExecutable ++ [toEnum 0x0201] }
        , source
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceExpectedExecutableSHA256 =
                Just $ replicate 32 0 }
        , source
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceExpectedExecutableSHA256 =
                Just $ 1 : replicate 31 0 }
        , source
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceSolverTimeoutMilliseconds =
                1001 }
        , source
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceSolverResourceLimit =
                100001 }
        , source
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceHostDeadlineMilliseconds =
                1501 }
        , source
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceArtifactPolicy =
                SMTLibExecution.LengthSMTLibStatusOnly }
        , source
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceResponseLimits =
                responseBytes }
        , source
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceResponseLimits =
                responseDepth }
        , source
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceResponseLimits =
                responseNodes }
        , source
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceResponseLimits =
                responseTokens }
        , source
            { SMTLibExecution.lengthSMTLibExecutionConfigSourceResponseLimits =
                responseIntegers }
        ]
      assertBool "a retained policy field was absent from private identity"
        $ all (/= baseline) changed
      assertBool "distinct retained field values shared private identity"
        $ length (nub $ baseline : changed) == length changed + 1
      let widerAdmission = SMTLibExecution.mkLengthSMTLibExecutionLimits
            SMTLibExecution.defaultLengthSMTLibExecutionLimitSource
              { SMTLibExecution.lengthSMTLibExecutionLimitSourceExecutablePathCharacters =
                  8192 }
      resealed <- sealWith widerAdmission source
      assertBool "admission-only limits changed sealed policy identity"
        $ resealed == baseline
  , testCase "bound the private complete policy fingerprint" $ do
      let noFingerprint = SMTLibExecution.mkLengthSMTLibExecutionLimits
            SMTLibExecution.defaultLengthSMTLibExecutionLimitSource
              { SMTLibExecution.lengthSMTLibExecutionLimitSourcePolicyFingerprintBytes =
                  0 }
      assertLeft
        (SMTLibExecution.LengthSMTLibExecutionPolicyFingerprintByteLimitExceeded
          0 1)
        $ SMTLibExecution.mkLengthSMTLibExecutionConfig noFingerprint
        $ SMTLibExecution.defaultLengthSMTLibExecutionConfigSource
            absoluteFixtureExecutable Nothing
  , testCase "emit one exact canonical constant-zero query" $ do
      problem <- adversarialConstantZeroProblem identityLengthContract
      query <- expectRight $ SMTLib.sealLengthSMTLibQuery
        SMTLib.defaultLengthSMTLibLimits problem
      SMTLib.lengthSMTLibQuerySchemaTag @?=
        asciiBytes "djex-length-z3-qf-lia-smtlib2/v2"
      SMTLib.lengthSMTLibQueryLogic @?= asciiBytes "QF_LIA"
      SMTLib.lengthSMTLibQueryInputSymbols query @?=
        [asciiBytes "djex_length_input_0"]
      SMTLib.lengthSMTLibQueryCheckBytes query @?=
        asciiBytes constantZeroSMTLibCheck
      SMTLib.lengthSMTLibQueryInputValueRequestBytes query @?=
        Just (asciiBytes "(get-value (djex_length_input_0))\n")
      Djex.behavioralProblemFingerprint
          (SMTLib.lengthSMTLibQueryBehavioralProblem query) @?=
        Djex.behavioralProblemFingerprint
          (LengthProblem.checkedLengthProblemBehavioralProblem problem)
  , testCase "decode input symbols order independently and replay the model" $ do
      problem <- adversarialBinaryConstantZeroProblem identityLengthContract
      query <- expectRight $ SMTLib.sealLengthSMTLibQuery
        SMTLib.defaultLengthSMTLibLimits problem
      case SMTLib.lengthSMTLibQueryInputSymbols query of
        [first, second] -> do
          evidence <- expectCounterexample
            $ SMTLib.validateLengthSMTLibCounterexample
                Evaluate.defaultLengthEvaluationLimits query
                [ smtIntegerBinding second 7
                , smtIntegerBinding first 3
                ]
          receipt <- expectRight $ Djex.replayBehavioralEvidence
            (LengthProblem.checkedLengthProblemBehavioralProblem problem)
            evidence
          Evaluate.validatedLengthCounterexampleInputs receipt @?= [3, 7]
          Evaluate.validatedLengthCounterexampleResult receipt @?= 0
        symbols -> assertFailure $ "unexpected input symbols: " ++ show symbols
  , testCase "omit value requests for a zero-input counterexample" $ do
      let result = Length.LengthVariable Length.LengthResult
          source = contractWith (Length.LengthTruth True)
            $ Length.LengthEqual result $ Length.LengthLiteral 1
      problem <- adversarialZeroInputProblem source
      query <- expectRight $ SMTLib.sealLengthSMTLibQuery
        SMTLib.defaultLengthSMTLibLimits problem
      SMTLib.lengthSMTLibQueryInputSymbols query @?= []
      SMTLib.lengthSMTLibQueryInputValueRequestBytes query @?= Nothing
      evidence <- expectCounterexample
        $ SMTLib.validateLengthSMTLibCounterexample
            Evaluate.defaultLengthEvaluationLimits query []
      receipt <- expectRight $ Djex.replayBehavioralEvidence
        (LengthProblem.checkedLengthProblemBehavioralProblem problem) evidence
      Evaluate.validatedLengthCounterexampleInputs receipt @?= []
      Evaluate.validatedLengthCounterexampleResult receipt @?= 0
  , testCase "translate every normalized Length operator into QF_LIA" $ do
      let input = Length.LengthVariable $ Length.LengthInput 0
          literal = Length.LengthLiteral
          bounded expression = Length.LengthAtMost expression $ literal 100
          source = contractWith
            (Length.LengthAll
              [ bounded $ Length.LengthSum [input, literal 1]
              , bounded $ Length.LengthScale 2 input
              , bounded $ Length.LengthMonus input $ literal 1
              , bounded $ Length.LengthMinimum input $ literal 2
              , bounded $ Length.LengthMaximum input $ literal 3
              , bounded $ Length.LengthIf
                  (Length.LengthAtMost input $ literal 4)
                  input
                  $ literal 1
              , Length.LengthEqual input $ literal 5
              , Length.LengthNot $ Length.LengthEqual input $ literal 6
              ])
            $ Length.LengthTruth False
      problem <- adversarialConstantZeroProblem source
      query <- expectRight $ SMTLib.sealLengthSMTLibQuery
        SMTLib.defaultLengthSMTLibLimits problem
      let script = map (toEnum . fromIntegral)
            $ SMTLib.lengthSMTLibQueryCheckBytes query
          expectedFragments =
            [ "(<= (+ djex_length_input_0 1) 100)"
            , "(<= (* 2 djex_length_input_0) 100)"
            , "(<= (djex_nat_monus djex_length_input_0 1) 100)"
            , "(<= (djex_nat_min djex_length_input_0 2) 100)"
            , "(<= (djex_nat_max djex_length_input_0 3) 100)"
            , "(<= (ite (<= djex_length_input_0 4) \
                \djex_length_input_0 1) 100)"
            , "(= djex_length_input_0 5)"
            , "(not (= djex_length_input_0 6))"
            ]
      mapM_ (\fragment -> assertBool
          ("missing translated fragment: " ++ fragment)
          $ fragment `isInfixOf` script)
        expectedFragments
      evidence <- expectCounterexample
        $ SMTLib.validateLengthSMTLibCounterexample
            Evaluate.defaultLengthEvaluationLimits query
            [smtIntegerBinding (asciiBytes "djex_length_input_0") 5]
      _ <- expectRight $ Djex.replayBehavioralEvidence
        (LengthProblem.checkedLengthProblemBehavioralProblem problem) evidence
      pure ()
  , testCase "replay a decoded identity model without manufacturing evidence" $ do
      (session, contract, candidate) <- realListIdentityFixture
        $ TypeVariable $ FlexibleVariable 0
      problem <- expectRight $ LengthProblem.sealLengthTypedCandidateProblem
        LengthProblem.defaultLengthProblemLimits session contract candidate
      query <- expectRight $ SMTLib.sealLengthSMTLibQuery
        SMTLib.defaultLengthSMTLibLimits problem
      case SMTLib.lengthSMTLibQueryInputSymbols query of
        [symbol] -> expectNoCounterexample
          $ SMTLib.validateLengthSMTLibCounterexample
              Evaluate.defaultLengthEvaluationLimits query
              [smtIntegerBinding symbol 8]
        symbols -> assertFailure $ "unexpected input symbols: " ++ show symbols
  , testCase "reject malformed decoded models before independent replay" $ do
      unaryProblem <- adversarialConstantZeroProblem identityLengthContract
      unaryQuery <- expectRight $ SMTLib.sealLengthSMTLibQuery
        SMTLib.defaultLengthSMTLibLimits unaryProblem
      binaryProblem <- adversarialBinaryConstantZeroProblem
        identityLengthContract
      binaryQuery <- expectRight $ SMTLib.sealLengthSMTLibQuery
        SMTLib.defaultLengthSMTLibLimits binaryProblem
      case ( SMTLib.lengthSMTLibQueryInputSymbols unaryQuery
           , SMTLib.lengthSMTLibQueryInputSymbols binaryQuery
           ) of
        ([unary], [first, _]) -> do
          let validateUnary = SMTLib.validateLengthSMTLibCounterexample
                Evaluate.defaultLengthEvaluationLimits unaryQuery
              unknown = asciiBytes "djex.input.999"
          assertLeft (SMTLib.LengthSMTLibBindingArityMismatch 1 0)
            $ validateUnary []
          assertLeft (SMTLib.LengthSMTLibBindingArityMismatch 1 2)
            $ validateUnary
                [smtIntegerBinding unary 1, smtIntegerBinding unary 2]
          assertLeft (SMTLib.LengthSMTLibUnknownInputSymbol 0 unknown)
            $ validateUnary [smtIntegerBinding unknown 1]
          assertLeft (SMTLib.LengthSMTLibNegativeInputValue 0 unary (-1))
            $ validateUnary [smtIntegerBinding unary (-1)]
          assertLeft (SMTLib.LengthSMTLibDuplicateInputSymbol 1 first)
            $ SMTLib.validateLengthSMTLibCounterexample
                Evaluate.defaultLengthEvaluationLimits binaryQuery
                [smtIntegerBinding first 1, smtIntegerBinding first 2]
          assertLeft
            (SMTLib.LengthSMTLibCounterexampleReplayRejected
              $ Evaluate.LengthEvaluationValueBitLimitExceeded
                  (Evaluate.LengthProblemInputValue 0) 2 3)
            $ SMTLib.validateLengthSMTLibCounterexample
                (evaluationLimitsWith 2 8) unaryQuery
                [smtIntegerBinding unary 4]

          let cyclicBindings = smtIntegerBinding unary 1 : cyclicBindings
          cyclicBindingsResult <- evaluateWithin $ validateUnary cyclicBindings
          assertLeft (SMTLib.LengthSMTLibBindingArityMismatch 1 2)
            cyclicBindingsResult

          let cyclicSymbol = fromIntegral (fromEnum 'x') : cyclicSymbol
              symbolLimit = fromIntegral $ length unary
          cyclicSymbolResult <- evaluateWithin $ validateUnary
            [smtIntegerBinding cyclicSymbol 1]
          assertLeft
            (SMTLib.LengthSMTLibBindingSymbolByteLimitExceeded
              0 symbolLimit (symbolLimit + 1))
            cyclicSymbolResult
        symbols -> assertFailure $ "unexpected input symbols: " ++ show symbols
  , testCase "bind query identity to the exact checked problem" $ do
      identityProblem <- adversarialConstantZeroProblem identityLengthContract
      trivialProblem <- adversarialConstantZeroProblem trivialLengthContract
      identityQuery <- expectRight $ SMTLib.sealLengthSMTLibQuery
        SMTLib.defaultLengthSMTLibLimits identityProblem
      trivialQuery <- expectRight $ SMTLib.sealLengthSMTLibQuery
        SMTLib.defaultLengthSMTLibLimits trivialProblem
      assertBool "distinct checked problems shared one SMT-LIB query identity" $
        SMTLib.lengthSMTLibQueryFingerprint identityQuery /=
          SMTLib.lengthSMTLibQueryFingerprint trivialQuery
  , testCase "associate and replay every bounded raw solver status exactly" $ do
      problem <- adversarialConstantZeroProblem identityLengthContract
      query <- expectRight $ SMTLib.sealLengthSMTLibQuery
        SMTLib.defaultLengthSMTLibLimits problem
      satisfiableArtifact <- smtRawArtifact [0x73, 0x61, 0x74] [1]
      unsatisfiableArtifact <- smtRawArtifact [0x75, 0x6e, 0x73, 0x61, 0x74] [2]
      unknownArtifact <- smtRawArtifact [0x75, 0x6e, 0x6b] [3]
      let observations ::
            [ SMTLibObservation.LengthSMTLibRawSolverObservation () () () ]
          observations =
            [ Observation.SatisfiableObservation satisfiableArtifact
            , Observation.UnsatisfiableObservation unsatisfiableArtifact
            , Observation.UnknownObservation unknownArtifact
            ]
          associated = map
            (SMTLibObservation.associateLengthSMTLibSolverObservation query)
            observations
      map SMTLibObservation.associatedLengthSMTLibQueryFingerprint associated
        @?= replicate 3 (SMTLib.lengthSMTLibQueryFingerprint query)
      map SMTLibObservation.associatedLengthSMTLibSolverStatus associated @?=
        [ Observation.SolverSatisfiable
        , Observation.SolverUnsatisfiable
        , Observation.SolverUnknown
        ]
      map SMTLibObservation.associatedLengthSMTLibResultStrength associated @?=
        [ SemanticProblem.RawSolverModelHint
        , SemanticProblem.RawSolverUnsatRelativeToEncoding
        , SemanticProblem.RawSolverUnknown
        ]
      map SMTLibObservation.associatedLengthSMTLibUse associated @?=
        replicate 3 SemanticProblem.HeuristicRankingOnly
      map
          (SMTLibObservation.replayAssociatedLengthSMTLibSolverObservation query)
          associated
        @?= map Right observations
  , testCase "wrap a stale checked problem mismatch before revealing raw bytes" $ do
      originalProblem <- adversarialConstantZeroProblem identityLengthContract
      staleProblem <- adversarialConstantZeroProblem trivialLengthContract
      originalQuery <- expectRight $ SMTLib.sealLengthSMTLibQuery
        SMTLib.defaultLengthSMTLibLimits originalProblem
      staleQuery <- expectRight $ SMTLib.sealLengthSMTLibQuery
        SMTLib.defaultLengthSMTLibLimits staleProblem
      artifact <- smtRawArtifact [0x75, 0x6e, 0x6b] [0]
      let observation ::
            SMTLibObservation.LengthSMTLibRawSolverObservation () () ()
          observation = Observation.UnknownObservation artifact
          associated =
            SMTLibObservation.associateLengthSMTLibSolverObservation
              originalQuery observation
      SMTLibObservation.replayAssociatedLengthSMTLibSolverObservation
          staleQuery associated @?=
        Left
          (SMTLibObservation.LengthSMTLibObservationProblemMismatch
            SemanticProblem.ReplayEncodingFingerprintMismatch)
  , testCase "parse exact statuses and classify standard solver failures" $ do
      let parse = SMTLibResponse.parseLengthSMTLibCheckResponse
            SMTLibResponse.defaultLengthSMTLibResponseLimits . asciiBytes
      parse "sat" @?= Right Observation.SolverSatisfiable
      parse "\t; before status\r\nunsat ; after status" @?=
        Right Observation.SolverUnsatisfiable
      parse "unknown\n" @?= Right Observation.SolverUnknown
      parse "unsupported" @?=
        Left SMTLibResponse.LengthSMTLibUnsupportedResponse
      parse "(error \"bad \"\"model\"\"\\path\")" @?=
        Left (SMTLibResponse.LengthSMTLibSolverErrorResponse
          $ asciiBytes "bad \"model\"\\path")
      parse "success" @?=
        Left SMTLibResponse.LengthSMTLibSuccessWhereStatusExpected
      parse "satjunk" @?=
        Left SMTLibResponse.LengthSMTLibUnexpectedCheckResponse
      parse "|sat|" @?=
        Left SMTLibResponse.LengthSMTLibUnexpectedCheckResponse
      parse "(sat)" @?=
        Left SMTLibResponse.LengthSMTLibUnexpectedCheckResponse
      parse "sat unsat" @?=
        Left (SMTLibResponse.LengthSMTLibResponseSyntaxError
          $ SMTLibResponse.SMTLibTrailingExpression 4)
  , testCase "decode unordered quoted input valuations in query order" $ do
      problem <- adversarialBinaryConstantZeroProblem identityLengthContract
      query <- expectRight $ SMTLib.sealLengthSMTLibQuery
        SMTLib.defaultLengthSMTLibLimits problem
      case SMTLib.lengthSMTLibQueryInputSymbols query of
        [first, second] -> do
          bindings <- expectRight
            $ SMTLibResponse.parseLengthSMTLibInputValueResponse
                SMTLibResponse.defaultLengthSMTLibResponseLimits query
                $ asciiBytes
                  "((|djex_length_input_1| 7)\n\
                  \ (djex_length_input_0 3))"
          bindings @?=
            [smtIntegerBinding first 3, smtIntegerBinding second 7]
          evidence <- expectCounterexample
            $ SMTLib.validateLengthSMTLibCounterexample
                Evaluate.defaultLengthEvaluationLimits query bindings
          receipt <- expectRight $ Djex.replayBehavioralEvidence
            (LengthProblem.checkedLengthProblemBehavioralProblem problem)
            evidence
          Evaluate.validatedLengthCounterexampleInputs receipt @?= [3, 7]
          Evaluate.validatedLengthCounterexampleResult receipt @?= 0
        symbols -> assertFailure $ "unexpected input symbols: " ++ show symbols
  , testCase "parse standard negative integers but leave naturals to replay" $ do
      problem <- adversarialConstantZeroProblem identityLengthContract
      query <- expectRight $ SMTLib.sealLengthSMTLibQuery
        SMTLib.defaultLengthSMTLibLimits problem
      case SMTLib.lengthSMTLibQueryInputSymbols query of
        [symbol] -> do
          bindings <- expectRight
            $ SMTLibResponse.parseLengthSMTLibInputValueResponse
                SMTLibResponse.defaultLengthSMTLibResponseLimits query
                $ asciiBytes "((djex_length_input_0 (- 3)))"
          bindings @?= [smtIntegerBinding symbol (-3)]
          assertLeft
            (SMTLib.LengthSMTLibNegativeInputValue 0 symbol (-3))
            $ SMTLib.validateLengthSMTLibCounterexample
                Evaluate.defaultLengthEvaluationLimits query bindings
          assertLeft (SMTLibResponse.LengthSMTLibNegativeZeroIntegerValue 0)
            $ SMTLibResponse.parseLengthSMTLibInputValueResponse
                SMTLibResponse.defaultLengthSMTLibResponseLimits query
                $ asciiBytes "((djex_length_input_0 (- 0)))"
        symbols -> assertFailure $ "unexpected input symbols: " ++ show symbols
  , testCase "reject malformed valuation shapes and sorts deterministically" $ do
      unaryProblem <- adversarialConstantZeroProblem identityLengthContract
      unaryQuery <- expectRight $ SMTLib.sealLengthSMTLibQuery
        SMTLib.defaultLengthSMTLibLimits unaryProblem
      binaryProblem <- adversarialBinaryConstantZeroProblem
        identityLengthContract
      binaryQuery <- expectRight $ SMTLib.sealLengthSMTLibQuery
        SMTLib.defaultLengthSMTLibLimits binaryProblem
      let parseUnary = SMTLibResponse.parseLengthSMTLibInputValueResponse
            SMTLibResponse.defaultLengthSMTLibResponseLimits unaryQuery
            . asciiBytes
          parseBinary = SMTLibResponse.parseLengthSMTLibInputValueResponse
            SMTLibResponse.defaultLengthSMTLibResponseLimits binaryQuery
            . asciiBytes
          unknown = asciiBytes "djex_length_input_9"
          first = asciiBytes "djex_length_input_0"
      parseUnary "()" @?= Left
        (SMTLibResponse.LengthSMTLibInputValueArityMismatch 1 0)
      parseUnary "((djex_length_input_0 1) (djex_length_input_0 2))" @?=
        Left (SMTLibResponse.LengthSMTLibInputValueArityMismatch 1 2)
      parseUnary "(djex_length_input_0)" @?=
        Left (SMTLibResponse.LengthSMTLibValuationPairExpected 0)
      parseUnary "((djex_length_input_0))" @?=
        Left (SMTLibResponse.LengthSMTLibValuationPairArityMismatch 0 1)
      parseUnary "(((djex_length_input_0) 1))" @?=
        Left (SMTLibResponse.LengthSMTLibValuationTermNotSymbol 0)
      parseUnary "((djex_length_input_9 1))" @?=
        Left (SMTLibResponse.LengthSMTLibUnknownValuationSymbol 0 unknown)
      parseBinary
          "((djex_length_input_0 1) (djex_length_input_0 2))" @?=
        Left (SMTLibResponse.LengthSMTLibDuplicateValuationSymbol 1 first)
      map parseUnary
          [ "((djex_length_input_0 1.0))"
          , "((djex_length_input_0 #x01))"
          , "((djex_length_input_0 #b01))"
          , "((djex_length_input_0 (+ 1)))"
          , "((djex_length_input_0 \"1\"))"
          , "((djex_length_input_0 :reason))"
          , "((djex_length_input_0 :56))"
          , "((djex_length_input_0 let))"
          ] @?= replicate 8
            (Left $ SMTLibResponse.LengthSMTLibValuationValueNotInteger 0)
      case parseUnary "((djex_length_input_0 01))" of
        Left (SMTLibResponse.LengthSMTLibResponseSyntaxError
            (SMTLibResponse.SMTLibInvalidBareToken _ token)) ->
          token @?= asciiBytes "01"
        other -> assertFailure $ "unexpected leading-zero result: " ++ show other
      parseUnary "unsupported" @?=
        Left SMTLibResponse.LengthSMTLibUnsupportedResponse
      parseUnary "(error \"no model\")" @?=
        Left (SMTLibResponse.LengthSMTLibSolverErrorResponse
          $ asciiBytes "no model")
  , testCase "keep quoted-symbol comments local and reject forbidden escapes" $ do
      problem <- adversarialConstantZeroProblem identityLengthContract
      query <- expectRight $ SMTLib.sealLengthSMTLibQuery
        SMTLib.defaultLengthSMTLibLimits problem
      let parse = SMTLibResponse.parseLengthSMTLibInputValueResponse
            SMTLibResponse.defaultLengthSMTLibResponseLimits query
          quotedUnknown = asciiBytes "djex_length_input_0;not-a-comment"
      parse (asciiBytes "((|djex_length_input_0;not-a-comment| 1))") @?=
        Left
          (SMTLibResponse.LengthSMTLibUnknownValuationSymbol 0 quotedUnknown)
      case parse (asciiBytes "((|djex_length\\input| 1))") of
        Left (SMTLibResponse.LengthSMTLibResponseSyntaxError
            (SMTLibResponse.SMTLibInvalidQuotedSymbolByte _ byte)) ->
          byte @?= fromIntegral (fromEnum '\\')
        other -> assertFailure $ "unexpected quoted-symbol result: " ++ show other
  , testCase "reject incomplete syntax and retain opaque non-ASCII text" $ do
      let parse = SMTLibResponse.parseLengthSMTLibCheckResponse
            SMTLibResponse.defaultLengthSMTLibResponseLimits
          syntax expected bytes = parse bytes @?=
            Left (SMTLibResponse.LengthSMTLibResponseSyntaxError expected)
      syntax SMTLibResponse.SMTLibEmptyResponse []
      syntax (SMTLibResponse.SMTLibUnexpectedClosingParenthesis 0)
        $ asciiBytes ")"
      syntax (SMTLibResponse.SMTLibUnterminatedList 0)
        $ asciiBytes "(sat"
      syntax (SMTLibResponse.SMTLibUnterminatedString 7)
        $ asciiBytes "(error \"unfinished"
      syntax (SMTLibResponse.SMTLibUnterminatedQuotedSymbol 0)
        $ asciiBytes "|unfinished"
      syntax (SMTLibResponse.SMTLibInvalidStringByte 8 0)
        $ asciiBytes "(error \"" ++ [0] ++ asciiBytes "\")"
      parse (asciiBytes "(error \"" ++ [0x80, 0xff] ++ asciiBytes "\")") @?=
        Left (SMTLibResponse.LengthSMTLibSolverErrorResponse [0x80, 0xff])

      stringLimit <- expectRight
        $ SMTLibResponse.mkLengthSMTLibResponseLimits
            SMTLibResponse.defaultLengthSMTLibResponseLimitSource
              { SMTLibResponse.lengthSMTLibResponseLimitSourceTokenBytes = 2 }
      SMTLibResponse.parseLengthSMTLibCheckResponse stringLimit
          (asciiBytes "\"abc\"") @?=
        Left (SMTLibResponse.LengthSMTLibResponseSyntaxError
          $ SMTLibResponse.SMTLibTokenByteLimitExceeded
              SMTLibResponse.SMTLibStringToken 2 3)
      SMTLibResponse.parseLengthSMTLibCheckResponse stringLimit
          (asciiBytes "|abc|") @?=
        Left (SMTLibResponse.LengthSMTLibResponseSyntaxError
          $ SMTLibResponse.SMTLibTokenByteLimitExceeded
              SMTLibResponse.SMTLibQuotedSymbolToken 2 3)
  , testCase "enforce every response resource bound productively" $ do
      problem <- adversarialConstantZeroProblem identityLengthContract
      query <- expectRight $ SMTLib.sealLengthSMTLibQuery
        SMTLib.defaultLengthSMTLibLimits problem
      let limits change = expectRight $ SMTLibResponse.mkLengthSMTLibResponseLimits
            $ change SMTLibResponse.defaultLengthSMTLibResponseLimitSource
          parseWith configured =
            SMTLibResponse.parseLengthSMTLibInputValueResponse configured query
      noBytes <- limits $ \source -> source
        { SMTLibResponse.lengthSMTLibResponseLimitSourceBytes = 0 }
      parseWith noBytes (asciiBytes "(") @?=
        Left (SMTLibResponse.LengthSMTLibResponseSyntaxError
          $ SMTLibResponse.SMTLibResponseByteLimitExceeded 0 1)
      oneDepth <- limits $ \source -> source
        { SMTLibResponse.lengthSMTLibResponseLimitSourceNestingDepth = 1 }
      parseWith oneDepth (asciiBytes "(())") @?=
        Left (SMTLibResponse.LengthSMTLibResponseSyntaxError
          $ SMTLibResponse.SMTLibNestingDepthLimitExceeded 1 2)
      twoNodes <- limits $ \source -> source
        { SMTLibResponse.lengthSMTLibResponseLimitSourceNodes = 2 }
      parseWith twoNodes (asciiBytes "((djex_length_input_0 1))") @?=
        Left (SMTLibResponse.LengthSMTLibResponseSyntaxError
          $ SMTLibResponse.SMTLibNodeLimitExceeded 2 3)
      twoTokenBytes <- limits $ \source -> source
        { SMTLibResponse.lengthSMTLibResponseLimitSourceTokenBytes = 2 }
      parseWith twoTokenBytes (asciiBytes "((djex_length_input_0 1))") @?=
        Left (SMTLibResponse.LengthSMTLibResponseSyntaxError
          $ SMTLibResponse.SMTLibTokenByteLimitExceeded
              SMTLibResponse.SMTLibBareToken 2 3)
      twoIntegerBits <- limits $ \source -> source
        { SMTLibResponse.lengthSMTLibResponseLimitSourceIntegerBits = 2 }
      parseWith twoIntegerBits
          (asciiBytes "((djex_length_input_0 4))") @?=
        Left (SMTLibResponse.LengthSMTLibResponseSyntaxError
          $ SMTLibResponse.SMTLibNumeralBitLimitExceeded 2 3)

      threeBytes <- limits $ \source -> source
        { SMTLibResponse.lengthSMTLibResponseLimitSourceBytes = 3 }
      let cyclicStatus = asciiBytes "sat" ++ cyclicStatus
      cyclicResult <- evaluateWithin
        $ SMTLibResponse.parseLengthSMTLibCheckResponse
            threeBytes cyclicStatus
      assertLeft
        (SMTLibResponse.LengthSMTLibResponseSyntaxError
          $ SMTLibResponse.SMTLibResponseByteLimitExceeded 3 4)
        cyclicResult
  , testCase "refuse unsolicited values for a zero-input query" $ do
      let result = Length.LengthVariable Length.LengthResult
          source = contractWith (Length.LengthTruth True)
            $ Length.LengthEqual result $ Length.LengthLiteral 1
      problem <- adversarialZeroInputProblem source
      query <- expectRight $ SMTLib.sealLengthSMTLibQuery
        SMTLib.defaultLengthSMTLibLimits problem
      let cyclicBytes = fromIntegral (fromEnum '(') : cyclicBytes
      observed <- evaluateWithin
        $ SMTLibResponse.parseLengthSMTLibInputValueResponse
            SMTLibResponse.defaultLengthSMTLibResponseLimits query cyclicBytes
      observed @?= Left SMTLibResponse.LengthSMTLibInputValueResponseNotExpected
  , testCase "publish validated conservative response defaults" $ do
      SMTLibResponse.lengthSMTLibResponseSchemaTag @?=
        asciiBytes "djex-length-z3-smtlib2-response/v1"
      SMTLibResponse.mkLengthSMTLibResponseLimits
          SMTLibResponse.defaultLengthSMTLibResponseLimitSource @?=
        Right SMTLibResponse.defaultLengthSMTLibResponseLimits
      let limits = SMTLibResponse.defaultLengthSMTLibResponseLimits
      SMTLibResponse.lengthSMTLibResponseByteLimit limits @?= 65536
      SMTLibResponse.lengthSMTLibResponseNestingDepthLimit limits @?= 64
      SMTLibResponse.lengthSMTLibResponseNodeLimit limits @?= 4096
      SMTLibResponse.lengthSMTLibResponseTokenByteLimit limits @?= 4096
      SMTLibResponse.lengthSMTLibResponseIntegerBitLimit limits @?= 4096
      assertLeft
        (SMTLibResponse.NegativeLengthSMTLibResponseLimit
          SMTLibResponse.LengthSMTLibResponseNestingDepth (-1))
        $ SMTLibResponse.mkLengthSMTLibResponseLimits
            SMTLibResponse.defaultLengthSMTLibResponseLimitSource
              { SMTLibResponse.lengthSMTLibResponseLimitSourceNestingDepth = -1 }
      assertLeft
        (SMTLibResponse.NegativeLengthSMTLibResponseLimit
          SMTLibResponse.LengthSMTLibResponseIntegerBits (-1))
        $ SMTLibResponse.mkLengthSMTLibResponseLimits
            SMTLibResponse.defaultLengthSMTLibResponseLimitSource
              { SMTLibResponse.lengthSMTLibResponseLimitSourceIntegerBits = -1 }
  , testCase "validate declaration and emission bounds productively" $ do
      SMTLib.mkLengthSMTLibLimits SMTLib.defaultLengthSMTLibLimitSource @?=
        Right SMTLib.defaultLengthSMTLibLimits
      SMTLib.lengthSMTLibCommandByteLimit SMTLib.defaultLengthSMTLibLimits
        @?= 65536
      SMTLib.lengthSMTLibFingerprintByteLimit
          SMTLib.defaultLengthSMTLibLimits @?= 262144
      SMTLib.lengthSMTLibNumeralBitLimit SMTLib.defaultLengthSMTLibLimits
        @?= 4096
      assertLeft
        (SMTLib.NegativeLengthSMTLibLimit
          SMTLib.LengthSMTLibNumeralBits (-1))
        $ SMTLib.mkLengthSMTLibLimits
            SMTLib.defaultLengthSMTLibLimitSource
              { SMTLib.lengthSMTLibLimitSourceNumeralBits = -1 }

      problem <- adversarialConstantZeroProblem identityLengthContract
      noCommand <- expectRight $ SMTLib.mkLengthSMTLibLimits
        SMTLib.defaultLengthSMTLibLimitSource
          { SMTLib.lengthSMTLibLimitSourceCommandBytes = 0 }
      assertLeft
        (SMTLib.LengthSMTLibCommandByteLimitExceeded
          SMTLib.LengthSMTLibCheckCommand 0 1)
        $ SMTLib.sealLengthSMTLibQuery noCommand problem

      noFingerprint <- expectRight $ SMTLib.mkLengthSMTLibLimits
        SMTLib.defaultLengthSMTLibLimitSource
          { SMTLib.lengthSMTLibLimitSourceFingerprintBytes = 0 }
      assertLeft (SMTLib.LengthSMTLibFingerprintByteLimitExceeded 0 1)
        $ SMTLib.sealLengthSMTLibQuery noFingerprint problem

      scaledProblem <- adversarialScaledProviderProblem 2
        identityLengthContract
      oneBitNumerals <- expectRight $ SMTLib.mkLengthSMTLibLimits
        SMTLib.defaultLengthSMTLibLimitSource
          { SMTLib.lengthSMTLibLimitSourceNumeralBits = 1 }
      assertLeft
        (SMTLib.LengthSMTLibNumeralBitLimitExceeded
          SMTLib.LengthSMTLibScaleNumeral 1 2)
        $ SMTLib.sealLengthSMTLibQuery oneBitNumerals scaledProblem
  ]

smtLibProtocolTests :: TestTree
smtLibProtocolTests = testGroup
  "package-private bounded Length SMT-LIB protocol"
  [ testCase "seal exact initial and conditional value writes" $ do
      (query, plan) <- protocolUnaryPlan
        InternalSMTLibExecution.LengthSMTLibInputValuesAfterSatisfiable
      checkBarrier <- protocolSentinel protocolCheckNonce
      valueBarrier <- protocolSentinel protocolValueNonce
      let expectedInitial =
            InternalSMTLibExecution.lengthSMTLibExecutionQueryResetBytes ++
            SMTLib.lengthSMTLibQueryCheckBytes query ++
            SMTLibStream.smtLibEchoSentinelCommandBytes checkBarrier
          expectedValue =
            fmap (++ SMTLibStream.smtLibEchoSentinelCommandBytes valueBarrier)
              $ SMTLib.lengthSMTLibQueryInputValueRequestBytes query
      SMTLibProtocol.lengthSMTLibProtocolInitialWriteBytes plan @?=
        expectedInitial
      SMTLibProtocol.lengthSMTLibProtocolInputValueWriteBytes plan @?=
        expectedValue
      checkReceiver <- expectProtocolWrite
        SMTLibProtocol.LengthSMTLibProtocolInitialQueryWrite
        expectedInitial
        $ SMTLibProtocol.startLengthSMTLibProtocol plan
      SMTLibProtocol.lengthSMTLibProtocolReceiverPhase checkReceiver @?=
        SMTLibProtocol.LengthSMTLibProtocolCheckStatusPhase
      checkAction <- feedProtocolChunks checkReceiver
        $ map (: [])
        $ asciiBytes "sat\n" ++
          SMTLibStream.smtLibEchoSentinelResponseBytes checkBarrier ++
          asciiBytes "\n"
      valueReceiver <- expectProtocolWrite
        SMTLibProtocol.LengthSMTLibProtocolInputValueWrite
        (maybe [] id expectedValue) checkAction
      SMTLibProtocol.lengthSMTLibProtocolReceiverPhase valueReceiver @?=
        SMTLibProtocol.LengthSMTLibProtocolInputValuePhase
      let rawValues = protocolValueFrame query [3]
      valueAction <- feedProtocolChunks valueReceiver
        $ map (: [])
        $ rawValues ++ asciiBytes "\n" ++
          SMTLibStream.smtLibEchoSentinelResponseBytes valueBarrier ++
          asciiBytes "\n"
      decoded <- expectProtocolComplete valueAction
      SMTLibProtocol.lengthSMTLibProtocolDecodedStatus decoded @?=
        Observation.SolverSatisfiable
      SMTLibProtocol.lengthSMTLibProtocolDecodedStatusFrame decoded @?=
        asciiBytes "sat"
      SMTLibProtocol.lengthSMTLibProtocolDecodedInputValueFrame decoded @?=
        Just rawValues
      SMTLibProtocol.lengthSMTLibProtocolDecodedInputValues decoded @?=
        Just [smtIntegerBinding (asciiBytes "djex_length_input_0") 3]
      SMTLibProtocol.lengthSMTLibProtocolDecodedPlanFingerprint decoded @?=
        SMTLibProtocol.lengthSMTLibProtocolPlanFingerprint plan
  , testCase "publish schemas, validated defaults, and exact admission minima" $ do
      SMTLibProtocol.lengthSMTLibProtocolPlanSchemaTag @?=
        asciiBytes "djex-length-z3-smtlib2-protocol-plan/v1"
      SMTLibProtocol.lengthSMTLibProtocolPhaseMachineSchemaTag @?=
        asciiBytes "djex-length-z3-smtlib2-protocol-phase-machine/v1"
      SMTLibProtocol.lengthSMTLibProtocolPostBarrierSchemaTag @?=
        asciiBytes "djex-smtlib2-post-barrier-whitespace/v1"
      assertBool "validated protocol defaults changed representation"
        $ SMTLibProtocol.mkLengthSMTLibProtocolLimits
            SMTLibProtocol.defaultLengthSMTLibProtocolLimitSource ==
          SMTLibProtocol.defaultLengthSMTLibProtocolLimits
      let defaults = SMTLibProtocol.defaultLengthSMTLibProtocolLimits
          stream = SMTLibProtocol.lengthSMTLibProtocolStreamLimits defaults
      stream @?= SMTLibStream.defaultSMTLibStreamLimits
      SMTLibStream.smtLibStreamTotalByteLimit stream @?= 131072
      SMTLibStream.smtLibStreamFrameByteLimit stream @?= 65536
      SMTLibStream.smtLibStreamNestingDepthLimit stream @?= 64
      SMTLibProtocol.lengthSMTLibProtocolCumulativeStdoutByteLimit defaults
        @?= 524288
      SMTLibProtocol.lengthSMTLibProtocolPlanFingerprintByteLimit defaults
        @?= 262144

      problem <- adversarialConstantZeroProblem identityLengthContract
      query <- expectRight $ SMTLib.sealLengthSMTLibQuery
        SMTLib.defaultLengthSMTLibLimits problem
      execution <- protocolExecutionConfig
        InternalSMTLibExecution.LengthSMTLibInputValuesAfterSatisfiable
      let seal limits configured =
            SMTLibProtocol.sealLengthSMTLibProtocolPlan limits configured query
              protocolCheckNonce $ Just protocolValueNonce
          protocolLimits change = SMTLibProtocol.mkLengthSMTLibProtocolLimits
            $ change SMTLibProtocol.defaultLengthSMTLibProtocolLimitSource
          streamLimits change = protocolLimits $ \source -> source
            { SMTLibProtocol.lengthSMTLibProtocolLimitSourceStreamLimits =
                change SMTLibStream.defaultSMTLibStreamLimitSource }
          tooSmall site field admitted required =
            SMTLibProtocol.LengthSMTLibProtocolRequiredLimitTooSmall
              site field admitted required
      assertLeft
        (tooSmall SMTLibProtocol.LengthSMTLibProtocolCheckStatusFrame
          SMTLibProtocol.LengthSMTLibProtocolStreamTotalBytes 6 7)
        $ seal (streamLimits $ \source -> source
            { SMTLibStream.smtLibStreamLimitSourceTotalBytes = 6 }) execution
      assertLeft
        (tooSmall SMTLibProtocol.LengthSMTLibProtocolCheckStatusFrame
          SMTLibProtocol.LengthSMTLibProtocolStreamFrameBytes 6 7)
        $ seal (streamLimits $ \source -> source
            { SMTLibStream.smtLibStreamLimitSourceFrameBytes = 6 }) execution
      assertLeft
        (tooSmall SMTLibProtocol.LengthSMTLibProtocolCheckBarrierFrame
          SMTLibProtocol.LengthSMTLibProtocolStreamTotalBytes 86 87)
        $ seal (streamLimits $ \source -> source
            { SMTLibStream.smtLibStreamLimitSourceTotalBytes = 86 }) execution
      assertLeft
        (tooSmall SMTLibProtocol.LengthSMTLibProtocolInputValueFrame
          SMTLibProtocol.LengthSMTLibProtocolStreamNestingDepth 1 2)
        $ seal (streamLimits $ \source -> source
            { SMTLibStream.smtLibStreamLimitSourceNestingDepth = 1 }) execution

      let response change = expectRight $ SMTLibResponse.mkLengthSMTLibResponseLimits
            $ change SMTLibResponse.defaultLengthSMTLibResponseLimitSource
          responseCase change expected = do
            configured <- protocolExecutionConfigWithResponse
              InternalSMTLibExecution.LengthSMTLibInputValuesAfterSatisfiable
              =<< response change
            assertLeft expected $ seal defaults configured
      responseCase
        (\source -> source
          { SMTLibResponse.lengthSMTLibResponseLimitSourceBytes = 6 })
        $ tooSmall SMTLibProtocol.LengthSMTLibProtocolCheckStatusFrame
            SMTLibProtocol.LengthSMTLibProtocolResponseBytes 6 7
      responseCase
        (\source -> source
          { SMTLibResponse.lengthSMTLibResponseLimitSourceNestingDepth = 1 })
        $ tooSmall SMTLibProtocol.LengthSMTLibProtocolInputValueFrame
            SMTLibProtocol.LengthSMTLibProtocolResponseNestingDepth 1 2
      responseCase
        (\source -> source
          { SMTLibResponse.lengthSMTLibResponseLimitSourceNodes = 3 })
        $ tooSmall SMTLibProtocol.LengthSMTLibProtocolInputValueFrame
            SMTLibProtocol.LengthSMTLibProtocolResponseNodes 3 4
      responseCase
        (\source -> source
          { SMTLibResponse.lengthSMTLibResponseLimitSourceTokenBytes = 18 })
        $ tooSmall SMTLibProtocol.LengthSMTLibProtocolInputValueFrame
            SMTLibProtocol.LengthSMTLibProtocolResponseTokenBytes 18 19

      let tooLittleStdout = protocolLimits $ \source -> source
            { SMTLibProtocol.lengthSMTLibProtocolLimitSourceCumulativeStdoutBytes =
                204 }
          noFingerprint = protocolLimits $ \source -> source
            { SMTLibProtocol.lengthSMTLibProtocolLimitSourcePlanFingerprintBytes =
                0 }
      assertLeft
        (SMTLibProtocol.LengthSMTLibProtocolMinimumStdoutByteLimitExceeded
          204 205)
        $ seal tooLittleStdout execution
      assertLeft
        (SMTLibProtocol.LengthSMTLibProtocolPlanFingerprintByteLimitExceeded
          0 1)
        $ seal noFingerprint execution
  , testCase "complete every non-value status without a later write" $ do
      checkBarrier <- protocolSentinel protocolCheckNonce
      let marker = SMTLibStream.smtLibEchoSentinelResponseBytes checkBarrier
          cases =
            [ ("sat", Observation.SolverSatisfiable)
            , ("unsat", Observation.SolverUnsatisfiable)
            , ("unknown", Observation.SolverUnknown)
            ]
      (_, statusOnly) <- protocolUnaryPlan
        InternalSMTLibExecution.LengthSMTLibStatusOnly
      mapM_ (assertProtocolTerminalStatus statusOnly marker) cases
      (_, values) <- protocolUnaryPlan
        InternalSMTLibExecution.LengthSMTLibInputValuesAfterSatisfiable
      mapM_ (assertProtocolTerminalStatus values marker)
        [ ("unsat", Observation.SolverUnsatisfiable)
        , ("unknown", Observation.SolverUnknown)
        ]
      (_, zeroInput) <- protocolZeroInputPlan
        InternalSMTLibExecution.LengthSMTLibInputValuesAfterSatisfiable
      SMTLibProtocol.lengthSMTLibProtocolInputValueWriteBytes zeroInput @?=
        Nothing
      zeroReceiver <- expectProtocolWrite
        SMTLibProtocol.LengthSMTLibProtocolInitialQueryWrite
        (SMTLibProtocol.lengthSMTLibProtocolInitialWriteBytes zeroInput)
        $ SMTLibProtocol.startLengthSMTLibProtocol zeroInput
      zeroAction <- expectRight $ SMTLibProtocol.feedLengthSMTLibProtocol
        zeroReceiver $ asciiBytes "sat\n" ++ marker ++ asciiBytes "\n"
      zeroDecoded <- expectProtocolComplete zeroAction
      SMTLibProtocol.lengthSMTLibProtocolDecodedStatus zeroDecoded @?=
        Observation.SolverSatisfiable
      SMTLibProtocol.lengthSMTLibProtocolDecodedInputValueFrame zeroDecoded @?=
        Nothing
      SMTLibProtocol.lengthSMTLibProtocolDecodedInputValues zeroDecoded @?=
        Just []
      (_, zeroStatusOnly) <- protocolZeroInputPlan
        InternalSMTLibExecution.LengthSMTLibStatusOnly
      assertProtocolTerminalStatus zeroStatusOnly marker
        ("sat", Observation.SolverSatisfiable)
  , testCase "reject absent, surplus, malformed, and repeated barrier nonces" $ do
      problem <- adversarialConstantZeroProblem identityLengthContract
      query <- expectRight $ SMTLib.sealLengthSMTLibQuery
        SMTLib.defaultLengthSMTLibLimits problem
      values <- protocolExecutionConfig
        InternalSMTLibExecution.LengthSMTLibInputValuesAfterSatisfiable
      statusOnly <- protocolExecutionConfig
        InternalSMTLibExecution.LengthSMTLibStatusOnly
      let seal execution check value =
            SMTLibProtocol.sealLengthSMTLibProtocolPlan
              SMTLibProtocol.defaultLengthSMTLibProtocolLimits
              execution query check value
      assertLeft SMTLibProtocol.LengthSMTLibProtocolMissingInputValueBarrierNonce
        $ seal values protocolCheckNonce Nothing
      assertLeft SMTLibProtocol.LengthSMTLibProtocolUnexpectedInputValueBarrierNonce
        $ seal statusOnly protocolCheckNonce $ Just protocolValueNonce
      assertLeft SMTLibProtocol.LengthSMTLibProtocolRepeatedBarrierNonce
        $ seal values protocolCheckNonce $ Just protocolCheckNonce
      assertLeft
        (SMTLibProtocol.LengthSMTLibProtocolBarrierNonceError
          SMTLibProtocol.LengthSMTLibProtocolCheckBarrier
          $ SMTLibStream.SMTLibEchoSentinelNonceLengthMismatch 32 31)
        $ seal values (replicate 31 0) $ Just protocolValueNonce
      let cyclicNonce = 0 : cyclicNonce
      cyclic <- evaluateWithin $ seal values cyclicNonce $ Just protocolValueNonce
      assertLeft
        (SMTLibProtocol.LengthSMTLibProtocolBarrierNonceError
          SMTLibProtocol.LengthSMTLibProtocolCheckBarrier
          $ SMTLibStream.SMTLibEchoSentinelNonceLengthMismatch 32 33)
        cyclic
  , testCase "fail closed at decoder, framing, barrier, and EOF boundaries" $ do
      (query, plan) <- protocolUnaryPlan
        InternalSMTLibExecution.LengthSMTLibInputValuesAfterSatisfiable
      checkBarrier <- protocolSentinel protocolCheckNonce
      wrongBarrier <- protocolSentinel protocolValueNonce
      initial <- expectProtocolWrite
        SMTLibProtocol.LengthSMTLibProtocolInitialQueryWrite
        (SMTLibProtocol.lengthSMTLibProtocolInitialWriteBytes plan)
        $ SMTLibProtocol.startLengthSMTLibProtocol plan
      assertLeft
        (SMTLibProtocol.LengthSMTLibProtocolResponseFailure
          SMTLibProtocol.LengthSMTLibProtocolCheckStatusPhase
          SMTLibResponse.LengthSMTLibSuccessWhereStatusExpected)
        $ SMTLibProtocol.feedLengthSMTLibProtocol initial
        $ asciiBytes "success\n"
      assertLeft
        (SMTLibProtocol.LengthSMTLibProtocolFramingFailure
          SMTLibProtocol.LengthSMTLibProtocolCheckStatusPhase
          $ SMTLibStream.SMTLibStreamUnexpectedClosingParenthesis 0)
        $ SMTLibProtocol.feedLengthSMTLibProtocol initial [41]
      assertLeft
        (SMTLibProtocol.LengthSMTLibProtocolUnexpectedEOF
          SMTLibProtocol.LengthSMTLibProtocolCheckStatusPhase)
        $ SMTLibProtocol.finishLengthSMTLibProtocol initial
      partial <- expectProtocolAwait =<< expectRight
        (SMTLibProtocol.feedLengthSMTLibProtocol initial $ asciiBytes "sa")
      assertLeft
        (SMTLibProtocol.LengthSMTLibProtocolFramingFailure
          SMTLibProtocol.LengthSMTLibProtocolCheckStatusPhase
          $ SMTLibStream.SMTLibStreamMissingWhitespaceAfterAtom 2)
        $ SMTLibProtocol.finishLengthSMTLibProtocol partial
      checkPhase <- expectProtocolAwait =<< expectRight
        (SMTLibProtocol.feedLengthSMTLibProtocol initial $ asciiBytes "sat\n")
      SMTLibProtocol.lengthSMTLibProtocolReceiverPhase checkPhase @?=
        SMTLibProtocol.LengthSMTLibProtocolCheckBarrierPhase
      assertLeft
        (SMTLibProtocol.LengthSMTLibProtocolBarrierMismatch
          SMTLibProtocol.LengthSMTLibProtocolCheckBarrier)
        $ SMTLibProtocol.feedLengthSMTLibProtocol checkPhase
        $ SMTLibStream.smtLibEchoSentinelResponseBytes wrongBarrier ++
          asciiBytes "\n"
      let staleValues = protocolValueFrame query [3]
      case SMTLibProtocol.feedLengthSMTLibProtocol initial $
          asciiBytes "sat\n" ++
          SMTLibStream.smtLibEchoSentinelResponseBytes checkBarrier ++
          asciiBytes "\n" ++ staleValues of
        Left (SMTLibProtocol.LengthSMTLibProtocolUnexpectedPostBarrierByte
            _ 40) -> pure ()
        Left other -> assertFailure $ "unexpected stale-frame rejection: " ++
          show other
        Right _ -> assertFailure
          "a valuation buffered before its write capability was accepted"
  , testCase "bind semantic plan inputs but exclude fingerprint admission" $ do
      problem <- adversarialConstantZeroProblem identityLengthContract
      query <- expectRight $ SMTLib.sealLengthSMTLibQuery
        SMTLib.defaultLengthSMTLibLimits problem
      binaryProblem <- adversarialBinaryConstantZeroProblem
        identityLengthContract
      binaryQuery <- expectRight $ SMTLib.sealLengthSMTLibQuery
        SMTLib.defaultLengthSMTLibLimits binaryProblem
      execution <- protocolExecutionConfig
        InternalSMTLibExecution.LengthSMTLibInputValuesAfterSatisfiable
      changedExecution <- expectRight
        $ InternalSMTLibExecution.mkLengthSMTLibExecutionConfig
            InternalSMTLibExecution.defaultLengthSMTLibExecutionLimits
        $ (InternalSMTLibExecution.defaultLengthSMTLibExecutionConfigSource
            absoluteFixtureExecutable Nothing)
            { InternalSMTLibExecution.lengthSMTLibExecutionConfigSourceSolverTimeoutMilliseconds =
                1001 }
      let defaults = SMTLibProtocol.defaultLengthSMTLibProtocolLimits
          source = SMTLibProtocol.defaultLengthSMTLibProtocolLimitSource
          alteredStream = SMTLibProtocol.mkLengthSMTLibProtocolLimits
            $ source
              { SMTLibProtocol.lengthSMTLibProtocolLimitSourceStreamLimits =
                  SMTLibStream.defaultSMTLibStreamLimitSource
                    { SMTLibStream.smtLibStreamLimitSourceTotalBytes = 131071 }
              }
          alteredCumulative = SMTLibProtocol.mkLengthSMTLibProtocolLimits
            $ source
              { SMTLibProtocol.lengthSMTLibProtocolLimitSourceCumulativeStdoutBytes =
                  524287 }
          widerAdmission = SMTLibProtocol.mkLengthSMTLibProtocolLimits
            $ source
              { SMTLibProtocol.lengthSMTLibProtocolLimitSourcePlanFingerprintBytes =
                  524288 }
          seal limits configured selected checkNonce valueNonce = expectRight
            $ SMTLibProtocol.sealLengthSMTLibProtocolPlan limits configured
                selected checkNonce $ Just valueNonce
      baseline <- seal defaults execution query
        protocolCheckNonce protocolValueNonce
      resealed <- seal widerAdmission execution query
        protocolCheckNonce protocolValueNonce
      SMTLibProtocol.lengthSMTLibProtocolPlanFingerprint resealed @?=
        SMTLibProtocol.lengthSMTLibProtocolPlanFingerprint baseline
      changed <- sequence
        [ seal alteredStream execution query
            protocolCheckNonce protocolValueNonce
        , seal alteredCumulative execution query
            protocolCheckNonce protocolValueNonce
        , seal defaults changedExecution query
            protocolCheckNonce protocolValueNonce
        , seal defaults execution binaryQuery
            protocolCheckNonce protocolValueNonce
        , seal defaults execution query [1 .. 32] protocolValueNonce
        , seal defaults execution query protocolCheckNonce [64 .. 95]
        ]
      let baselineFingerprint =
            SMTLibProtocol.lengthSMTLibProtocolPlanFingerprint baseline
          changedFingerprints =
            map SMTLibProtocol.lengthSMTLibProtocolPlanFingerprint changed
          fingerprints = baselineFingerprint : changedFingerprints
      assertBool "a semantic protocol-plan input was absent from identity"
        $ all (/= baselineFingerprint) changedFingerprints
      assertBool "distinct protocol plans shared a private complete key"
        $ length (nub fingerprints) == length fingerprints
  , testCase "enforce exact cumulative accounting and value-phase barriers" $ do
      (query, defaultPlan) <- protocolUnaryPlan
        InternalSMTLibExecution.LengthSMTLibInputValuesAfterSatisfiable
      execution <- protocolExecutionConfig
        InternalSMTLibExecution.LengthSMTLibInputValuesAfterSatisfiable
      let exactLimits = SMTLibProtocol.mkLengthSMTLibProtocolLimits
            $ SMTLibProtocol.defaultLengthSMTLibProtocolLimitSource
                { SMTLibProtocol.lengthSMTLibProtocolLimitSourceCumulativeStdoutBytes =
                    205 }
      exactPlan <- expectRight $ SMTLibProtocol.sealLengthSMTLibProtocolPlan
        exactLimits execution query protocolCheckNonce $ Just protocolValueNonce
      checkBarrier <- protocolSentinel protocolCheckNonce
      valueBarrier <- protocolSentinel protocolValueNonce
      let checkTranscript = asciiBytes "sat\n" ++
            SMTLibStream.smtLibEchoSentinelResponseBytes checkBarrier ++
            asciiBytes "\n"
          rawValues = protocolValueFrame query [0]
          valueTranscript = rawValues ++
            SMTLibStream.smtLibEchoSentinelResponseBytes valueBarrier ++
            asciiBytes "\n"
          begin plan = expectProtocolWrite
            SMTLibProtocol.LengthSMTLibProtocolInitialQueryWrite
            (SMTLibProtocol.lengthSMTLibProtocolInitialWriteBytes plan)
            $ SMTLibProtocol.startLengthSMTLibProtocol plan
      exactInitial <- begin exactPlan
      exactValueAction <- expectRight
        $ SMTLibProtocol.feedLengthSMTLibProtocol exactInitial checkTranscript
      exactValue <- expectProtocolWrite
        SMTLibProtocol.LengthSMTLibProtocolInputValueWrite
        (maybe [] id $ SMTLibProtocol.lengthSMTLibProtocolInputValueWriteBytes
          exactPlan)
        exactValueAction
      exactDecoded <- expectProtocolComplete =<< expectRight
        (SMTLibProtocol.feedLengthSMTLibProtocol exactValue valueTranscript)
      SMTLibProtocol.lengthSMTLibProtocolDecodedInputValues exactDecoded @?=
        Just [smtIntegerBinding (asciiBytes "djex_length_input_0") 0]

      overflowingInitial <- begin exactPlan
      overflowingAction <- expectRight
        $ SMTLibProtocol.feedLengthSMTLibProtocol
            overflowingInitial checkTranscript
      overflowingValue <- expectProtocolWrite
        SMTLibProtocol.LengthSMTLibProtocolInputValueWrite
        (maybe [] id $ SMTLibProtocol.lengthSMTLibProtocolInputValueWriteBytes
          exactPlan)
        overflowingAction
      assertLeft
        (SMTLibProtocol.LengthSMTLibProtocolCumulativeStdoutByteLimitExceeded
          205 206)
        $ SMTLibProtocol.feedLengthSMTLibProtocol overflowingValue
        $ valueTranscript ++ asciiBytes " "
      cyclicInitial <- begin exactPlan
      cyclicAction <- expectRight
        $ SMTLibProtocol.feedLengthSMTLibProtocol cyclicInitial checkTranscript
      cyclicValue <- expectProtocolWrite
        SMTLibProtocol.LengthSMTLibProtocolInputValueWrite
        (maybe [] id $ SMTLibProtocol.lengthSMTLibProtocolInputValueWriteBytes
          exactPlan)
        cyclicAction
      let cyclicWhitespace = 32 : cyclicWhitespace
      cyclicOverflow <- evaluateWithin
        $ SMTLibProtocol.feedLengthSMTLibProtocol cyclicValue
        $ rawValues ++
          SMTLibStream.smtLibEchoSentinelResponseBytes valueBarrier ++
          cyclicWhitespace
      assertLeft
        (SMTLibProtocol.LengthSMTLibProtocolCumulativeStdoutByteLimitExceeded
          205 206)
        cyclicOverflow

      statusExecution <- protocolExecutionConfig
        InternalSMTLibExecution.LengthSMTLibStatusOnly
      let equalCapLimits = SMTLibProtocol.mkLengthSMTLibProtocolLimits
            $ SMTLibProtocol.defaultLengthSMTLibProtocolLimitSource
                { SMTLibProtocol.lengthSMTLibProtocolLimitSourceStreamLimits =
                    SMTLibStream.defaultSMTLibStreamLimitSource
                      { SMTLibStream.smtLibStreamLimitSourceTotalBytes = 87 }
                , SMTLibProtocol.lengthSMTLibProtocolLimitSourceCumulativeStdoutBytes =
                    96
                }
      equalCapPlan <- expectRight $ SMTLibProtocol.sealLengthSMTLibProtocolPlan
        equalCapLimits statusExecution query protocolCheckNonce Nothing
      equalCapInitial <- begin equalCapPlan
      assertLeft
        (SMTLibProtocol.LengthSMTLibProtocolFramingFailure
          SMTLibProtocol.LengthSMTLibProtocolCheckBarrierPhase
          $ SMTLibStream.SMTLibStreamTotalByteLimitExceeded 87 88)
        $ SMTLibProtocol.feedLengthSMTLibProtocol equalCapInitial
        $ asciiBytes "  unknown\n" ++
          SMTLibStream.smtLibEchoSentinelResponseBytes checkBarrier ++
          asciiBytes "\n"

      defaultInitial <- begin defaultPlan
      defaultValueAction <- expectRight
        $ SMTLibProtocol.feedLengthSMTLibProtocol defaultInitial checkTranscript
      defaultValue <- expectProtocolWrite
        SMTLibProtocol.LengthSMTLibProtocolInputValueWrite
        (maybe [] id $ SMTLibProtocol.lengthSMTLibProtocolInputValueWriteBytes
          defaultPlan)
        defaultValueAction
      assertLeft
        (SMTLibProtocol.LengthSMTLibProtocolUnexpectedEOF
          SMTLibProtocol.LengthSMTLibProtocolInputValuePhase)
        $ SMTLibProtocol.finishLengthSMTLibProtocol defaultValue
      assertLeft
        (SMTLibProtocol.LengthSMTLibProtocolResponseFailure
          SMTLibProtocol.LengthSMTLibProtocolInputValuePhase
          SMTLibResponse.LengthSMTLibUnexpectedInputValueResponse)
        $ SMTLibProtocol.feedLengthSMTLibProtocol defaultValue
        $ asciiBytes "success\n"
      valueBarrierPhase <- expectProtocolAwait =<< expectRight
        (SMTLibProtocol.feedLengthSMTLibProtocol defaultValue
          $ rawValues ++ asciiBytes "\n")
      SMTLibProtocol.lengthSMTLibProtocolReceiverPhase valueBarrierPhase @?=
        SMTLibProtocol.LengthSMTLibProtocolInputValueBarrierPhase
      assertLeft
        (SMTLibProtocol.LengthSMTLibProtocolUnexpectedEOF
          SMTLibProtocol.LengthSMTLibProtocolInputValueBarrierPhase)
        $ SMTLibProtocol.finishLengthSMTLibProtocol valueBarrierPhase
      assertLeft
        (SMTLibProtocol.LengthSMTLibProtocolBarrierMismatch
          SMTLibProtocol.LengthSMTLibProtocolInputValueBarrier)
        $ SMTLibProtocol.feedLengthSMTLibProtocol valueBarrierPhase
        $ SMTLibStream.smtLibEchoSentinelResponseBytes checkBarrier ++
          asciiBytes "\n"

      (_, statusOnly) <- protocolUnaryPlan
        InternalSMTLibExecution.LengthSMTLibStatusOnly
      statusInitial <- begin statusOnly
      statusDecoded <- expectProtocolComplete =<< expectRight
        (SMTLibProtocol.feedLengthSMTLibProtocol statusInitial
          $ asciiBytes "unknown\n" ++
            SMTLibStream.smtLibEchoSentinelResponseBytes checkBarrier ++
            [9, 10, 13, 32])
      SMTLibProtocol.lengthSMTLibProtocolDecodedStatus statusDecoded @?=
        Observation.SolverUnknown
      commentInitial <- begin statusOnly
      case SMTLibProtocol.feedLengthSMTLibProtocol commentInitial $
          asciiBytes "unknown\n" ++
          SMTLibStream.smtLibEchoSentinelResponseBytes checkBarrier ++
          asciiBytes "; not transport whitespace" of
        Left (SMTLibProtocol.LengthSMTLibProtocolUnexpectedPostBarrierByte
            _ 59) -> pure ()
        Left other -> assertFailure $ "unexpected post-barrier rejection: " ++
          show other
        Right _ -> assertFailure "a post-barrier comment was accepted as trivia"
  ]

protocolCheckNonce, protocolValueNonce :: [Word8]
protocolCheckNonce = [0 .. 31]
protocolValueNonce = [32 .. 63]

protocolSentinel :: [Word8] -> IO SMTLibStream.SMTLibEchoSentinel
protocolSentinel = expectRight . SMTLibStream.mkSMTLibEchoSentinel

protocolExecutionConfig
  :: InternalSMTLibExecution.LengthSMTLibArtifactPolicy
  -> IO InternalSMTLibExecution.LengthSMTLibExecutionConfig
protocolExecutionConfig policy = expectRight
  $ protocolExecutionConfigWithResponseEither policy
      SMTLibResponse.defaultLengthSMTLibResponseLimits

protocolExecutionConfigWithResponse
  :: InternalSMTLibExecution.LengthSMTLibArtifactPolicy
  -> SMTLibResponse.LengthSMTLibResponseLimits
  -> IO InternalSMTLibExecution.LengthSMTLibExecutionConfig
protocolExecutionConfigWithResponse policy response = expectRight
  $ protocolExecutionConfigWithResponseEither policy response

protocolExecutionConfigWithResponseEither
  :: InternalSMTLibExecution.LengthSMTLibArtifactPolicy
  -> SMTLibResponse.LengthSMTLibResponseLimits
  -> Either
      InternalSMTLibExecution.LengthSMTLibExecutionConfigError
      InternalSMTLibExecution.LengthSMTLibExecutionConfig
protocolExecutionConfigWithResponseEither policy response =
  InternalSMTLibExecution.mkLengthSMTLibExecutionConfig
    InternalSMTLibExecution.defaultLengthSMTLibExecutionLimits
  $ (InternalSMTLibExecution.defaultLengthSMTLibExecutionConfigSource
      absoluteFixtureExecutable Nothing)
      { InternalSMTLibExecution.lengthSMTLibExecutionConfigSourceArtifactPolicy =
          policy
      , InternalSMTLibExecution.lengthSMTLibExecutionConfigSourceResponseLimits =
          response
      }

protocolUnaryPlan
  :: InternalSMTLibExecution.LengthSMTLibArtifactPolicy
  -> IO
      ( SMTLib.LengthSMTLibQuery AdversarialIdentity AdversarialLocal
      , SMTLibProtocol.LengthSMTLibProtocolPlan
          AdversarialIdentity AdversarialLocal
      )
protocolUnaryPlan policy = do
  problem <- adversarialConstantZeroProblem identityLengthContract
  query <- expectRight $ SMTLib.sealLengthSMTLibQuery
    SMTLib.defaultLengthSMTLibLimits problem
  execution <- protocolExecutionConfig policy
  let valueNonce = case policy of
        InternalSMTLibExecution.LengthSMTLibStatusOnly -> Nothing
        InternalSMTLibExecution.LengthSMTLibInputValuesAfterSatisfiable ->
          Just protocolValueNonce
  plan <- expectRight $ SMTLibProtocol.sealLengthSMTLibProtocolPlan
    SMTLibProtocol.defaultLengthSMTLibProtocolLimits
    execution query protocolCheckNonce valueNonce
  pure (query, plan)

protocolZeroInputPlan
  :: InternalSMTLibExecution.LengthSMTLibArtifactPolicy
  -> IO
      ( SMTLib.LengthSMTLibQuery AdversarialIdentity AdversarialLocal
      , SMTLibProtocol.LengthSMTLibProtocolPlan
          AdversarialIdentity AdversarialLocal
      )
protocolZeroInputPlan policy = do
  let result = Length.LengthVariable Length.LengthResult
      source = contractWith (Length.LengthTruth True)
        $ Length.LengthEqual result $ Length.LengthLiteral 1
  problem <- adversarialZeroInputProblem source
  query <- expectRight $ SMTLib.sealLengthSMTLibQuery
    SMTLib.defaultLengthSMTLibLimits problem
  execution <- protocolExecutionConfig policy
  plan <- expectRight $ SMTLibProtocol.sealLengthSMTLibProtocolPlan
    SMTLibProtocol.defaultLengthSMTLibProtocolLimits
    execution query protocolCheckNonce Nothing
  pure (query, plan)

protocolValueFrame
  :: SMTLib.LengthSMTLibQuery identity local
  -> [Integer]
  -> [Word8]
protocolValueFrame query values =
  [40] ++ intercalate [32]
    [ [40] ++ symbol ++ [32] ++ asciiBytes (show value) ++ [41]
    | (symbol, value) <-
        zip (SMTLib.lengthSMTLibQueryInputSymbols query) values
    ] ++ [41]

expectProtocolWrite
  :: SMTLibProtocol.LengthSMTLibProtocolWriteKind
  -> [Word8]
  -> SMTLibProtocol.LengthSMTLibProtocolAction identity local
  -> IO (SMTLibProtocol.LengthSMTLibProtocolReceiver identity local)
expectProtocolWrite expectedKind expectedBytes action = case action of
  SMTLibProtocol.LengthSMTLibProtocolWrite kind bytes receiver -> do
    kind @?= expectedKind
    bytes @?= expectedBytes
    pure receiver
  SMTLibProtocol.LengthSMTLibProtocolAwait{} ->
    assertFailure "expected a protocol write action, observed await"
  SMTLibProtocol.LengthSMTLibProtocolComplete{} ->
    assertFailure "expected a protocol write action, observed completion"

expectProtocolAwait
  :: SMTLibProtocol.LengthSMTLibProtocolAction identity local
  -> IO (SMTLibProtocol.LengthSMTLibProtocolReceiver identity local)
expectProtocolAwait action = case action of
  SMTLibProtocol.LengthSMTLibProtocolAwait receiver -> pure receiver
  SMTLibProtocol.LengthSMTLibProtocolWrite{} ->
    assertFailure "expected a protocol await action, observed write"
  SMTLibProtocol.LengthSMTLibProtocolComplete{} ->
    assertFailure "expected a protocol await action, observed completion"

expectProtocolComplete
  :: SMTLibProtocol.LengthSMTLibProtocolAction identity local
  -> IO (SMTLibProtocol.LengthSMTLibProtocolDecoded identity local)
expectProtocolComplete action = case action of
  SMTLibProtocol.LengthSMTLibProtocolComplete decoded -> pure decoded
  SMTLibProtocol.LengthSMTLibProtocolWrite{} ->
    assertFailure "expected protocol completion, observed write"
  SMTLibProtocol.LengthSMTLibProtocolAwait{} ->
    assertFailure "expected protocol completion, observed await"

feedProtocolChunks
  :: SMTLibProtocol.LengthSMTLibProtocolReceiver identity local
  -> [[Word8]]
  -> IO (SMTLibProtocol.LengthSMTLibProtocolAction identity local)
feedProtocolChunks receiver chunks = case chunks of
  [] -> pure $ SMTLibProtocol.LengthSMTLibProtocolAwait receiver
  chunk : remaining -> do
    action <- expectRight
      $ SMTLibProtocol.feedLengthSMTLibProtocol receiver chunk
    case action of
      SMTLibProtocol.LengthSMTLibProtocolAwait next ->
        feedProtocolChunks next remaining
      _
        | null remaining -> pure action
        | otherwise -> assertFailure
            "the protocol produced an action before all response chunks"

assertProtocolTerminalStatus
  :: SMTLibProtocol.LengthSMTLibProtocolPlan identity local
  -> [Word8]
  -> (String, Observation.SolverStatus)
  -> IO ()
assertProtocolTerminalStatus plan marker (rawStatus, expectedStatus) = do
  receiver <- expectProtocolWrite
    SMTLibProtocol.LengthSMTLibProtocolInitialQueryWrite
    (SMTLibProtocol.lengthSMTLibProtocolInitialWriteBytes plan)
    $ SMTLibProtocol.startLengthSMTLibProtocol plan
  action <- expectRight $ SMTLibProtocol.feedLengthSMTLibProtocol receiver
    $ asciiBytes (rawStatus ++ "\n") ++ marker ++ asciiBytes "\n"
  decoded <- expectProtocolComplete action
  SMTLibProtocol.lengthSMTLibProtocolDecodedStatus decoded @?= expectedStatus
  SMTLibProtocol.lengthSMTLibProtocolDecodedStatusFrame decoded @?=
    asciiBytes rawStatus
  SMTLibProtocol.lengthSMTLibProtocolDecodedInputValueFrame decoded @?= Nothing
  SMTLibProtocol.lengthSMTLibProtocolDecodedInputValues decoded @?= Nothing

constantZeroSMTLibCheck :: String
constantZeroSMTLibCheck = unlines
  [ "(set-option :produce-models true)"
  , "(set-option :random-seed 1)"
  , "(set-logic QF_LIA)"
  , "(define-fun djex_nat_monus ((x Int) (y Int)) Int (ite (<= y x) (- x y) 0))"
  , "(define-fun djex_nat_min ((x Int) (y Int)) Int (ite (<= x y) x y))"
  , "(define-fun djex_nat_max ((x Int) (y Int)) Int (ite (<= x y) y x))"
  , "(declare-const djex_length_input_0 Int)"
  , "(assert (<= 0 djex_length_input_0))"
  , "(assert (not (= djex_length_input_0 0)))"
  , "(check-sat)"
  ]

limitTests :: TestTree
limitTests = testGroup "limits"
  [ testCase "publish the exact versioned domain tag" $
      Length.finiteListSpineLengthDomainTag @?=
        map (fromIntegral . fromEnum)
          ("finite-list-spine-length/v1" :: String)
  , testCase "publish the intended conservative defaults" $ do
      Length.mkLengthLimits Length.defaultLengthLimitSource @?=
        Right Length.defaultLengthLimits
      let limits = Length.defaultLengthLimits
      Length.lengthTypeNodeLimit limits @?= 4096
      Length.lengthContractInputLimit limits @?= 8
      Length.lengthSyntaxNodeLimit limits @?= 1024
      Length.lengthFormulaClauseLimit limits @?= 32
      Length.lengthCollectionWidthLimit limits @?= 64
      Length.lengthProviderSummaryLimit limits @?= 256
      Length.lengthProviderArgumentLimit limits @?= 16
      Length.lengthLiteralBitLimit limits @?= 256
      Length.lengthFingerprintByteLimit limits @?= 65536
  , testCase "reject every negative field in declaration order" $
      mapM_ assertNegativeLimit negativeLimitCases
  , testCase "admit zero for every bound" $ do
      limits <- expectRight $ Length.mkLengthLimits zeroLengthLimitSource
      map ($ limits)
        [ Length.lengthTypeNodeLimit
        , Length.lengthContractInputLimit
        , Length.lengthSyntaxNodeLimit
        , Length.lengthFormulaClauseLimit
        , Length.lengthCollectionWidthLimit
        , Length.lengthProviderSummaryLimit
        , Length.lengthProviderArgumentLimit
        , Length.lengthLiteralBitLimit
        , Length.lengthFingerprintByteLimit
        ] @?= replicate 9 0
  ]

contextTests :: TestTree
contextTests = testGroup "checked semantic contexts"
  [ testCase "retain the exact inventory and builtin structural-list model" $ do
      context <- expectRight $ Length.sealLengthContext
        Length.defaultLengthLimits fixtureInventory Length.BuiltinListSpine
      Length.lengthContextInventory context @?= fixtureInventory
      let model = Length.lengthContextSpineModel context
      Length.checkedLengthSpineTypeName model @?= listName
      Length.checkedLengthSpineZeroConstructor model @?= listName
      Length.checkedLengthSpineStepConstructor model @?= consName
      Length.checkedLengthSpineRecursiveField model @?= 1
      Length.checkedLengthSpineModelTrust model @?=
        Length.BuiltinStructuralListSpine
  , testCase "derive both legal recursive-field orders from declarations" $ do
      typeName <- expectName "Fixture.Sequence"
      zeroName <- expectName "Fixture.Empty"
      stepName <- expectName "Fixture.Link"
      let source = Length.DeclaredListSpine typeName zeroName stepName
          payloadFirstInventory = fixtureInventoryFromDeclarations
            [listLikeDeclaration typeName zeroName stepName False]
          recursiveFirstInventory = fixtureInventoryFromDeclarations
            [listLikeDeclaration typeName zeroName stepName True]
      payloadFirst <- expectRight $ Length.sealLengthContext
        Length.defaultLengthLimits payloadFirstInventory source
      recursiveFirst <- expectRight $ Length.sealLengthContext
        Length.defaultLengthLimits recursiveFirstInventory source
      let payloadFirstModel = Length.lengthContextSpineModel payloadFirst
          recursiveFirstModel = Length.lengthContextSpineModel recursiveFirst
      map Length.checkedLengthSpineTypeName
          [payloadFirstModel, recursiveFirstModel] @?=
        [typeName, typeName]
      map Length.checkedLengthSpineZeroConstructor
          [payloadFirstModel, recursiveFirstModel] @?=
        [zeroName, zeroName]
      map Length.checkedLengthSpineStepConstructor
          [payloadFirstModel, recursiveFirstModel] @?=
        [stepName, stepName]
      map Length.checkedLengthSpineRecursiveField
          [payloadFirstModel, recursiveFirstModel] @?=
        [1, 0]
      map Length.checkedLengthSpineModelTrust
          [payloadFirstModel, recursiveFirstModel] @?=
        replicate 2 Length.DerivedFromListLikeDataDeclaration
  , testCase "reject absent, non-data, ambiguous, and malformed schemas" $ do
      typeName <- expectName "Fixture.Sequence"
      zeroName <- expectName "Fixture.Empty"
      stepName <- expectName "Fixture.Link"
      otherName <- expectName "Fixture.Other"
      let source = Length.DeclaredListSpine typeName zeroName stepName
          parameter = TypeParameter "element" Nothing
          payload = TypeVariable "element"
          recursive = TypeApplication (TypeConstructor typeName) payload
          nonDataInventory = fixtureInventoryFromDeclarations
            [AbstractTypeDeclaration () typeName ProperTypeKind]
          noParameterInventory = fixtureInventoryFromDeclarations
            [DataTypeDeclaration () typeName []
              [DataConstructor () zeroName [], DataConstructor () stepName []]]
          oneConstructorInventory = fixtureInventoryFromDeclarations
            [DataTypeDeclaration () typeName [parameter]
              [DataConstructor () zeroName []]]
          malformedFieldsInventory = fixtureInventoryFromDeclarations
            [DataTypeDeclaration () typeName [parameter]
              [ DataConstructor () zeroName []
              , DataConstructor () stepName [payload, payload]
              ]]
          nonemptyZeroInventory = fixtureInventoryFromDeclarations
            [DataTypeDeclaration () typeName [parameter]
              [ DataConstructor () zeroName [payload]
              , DataConstructor () stepName [payload, recursive]
              ]]
          unaryStepInventory = fixtureInventoryFromDeclarations
            [DataTypeDeclaration () typeName [parameter]
              [ DataConstructor () zeroName []
              , DataConstructor () stepName [recursive]
              ]]
          validInventory = fixtureInventoryFromDeclarations
            [DataTypeDeclaration () typeName [parameter]
              [ DataConstructor () zeroName []
              , DataConstructor () stepName [payload, recursive]
              ]]
      assertLeft (Length.LengthSpineTypeDeclarationMissing typeName)
        $ Length.sealLengthContext
            Length.defaultLengthLimits fixtureInventory source
      assertLeft (Length.LengthSpineTypeIsNotData typeName)
        $ Length.sealLengthContext
            Length.defaultLengthLimits nonDataInventory source
      assertLeft (Length.LengthSpineParameterArityMismatch typeName 0)
        $ Length.sealLengthContext
            Length.defaultLengthLimits noParameterInventory source
      assertLeft (Length.LengthSpineConstructorArityMismatch typeName 1)
        $ Length.sealLengthContext
            Length.defaultLengthLimits oneConstructorInventory source
      assertLeft (Length.LengthSpineConstructorsMustDiffer zeroName)
        $ Length.sealLengthContext Length.defaultLengthLimits validInventory
            (Length.DeclaredListSpine typeName zeroName zeroName)
      assertLeft (Length.LengthSpineZeroConstructorMissing otherName)
        $ Length.sealLengthContext Length.defaultLengthLimits validInventory
            (Length.DeclaredListSpine typeName otherName stepName)
      assertLeft (Length.LengthSpineStepConstructorMissing otherName)
        $ Length.sealLengthContext Length.defaultLengthLimits validInventory
            (Length.DeclaredListSpine typeName zeroName otherName)
      assertLeft (Length.LengthSpineZeroFieldArityMismatch zeroName 1)
        $ Length.sealLengthContext
            Length.defaultLengthLimits nonemptyZeroInventory source
      assertLeft (Length.LengthSpineStepFieldArityMismatch stepName 1)
        $ Length.sealLengthContext
            Length.defaultLengthLimits unaryStepInventory source
      assertLeft
        (Length.LengthSpineStepFieldsDoNotMatch
          stepName payload payload)
        $ Length.sealLengthContext
            Length.defaultLengthLimits malformedFieldsInventory source
  , testCase "keep builtin and declared spine families disjoint" $ do
      typeName <- expectName "Fixture.DisjointSequence"
      zeroName <- expectName "Fixture.DisjointEmpty"
      stepName <- expectName "Fixture.DisjointLink"
      let inventory = fixtureInventoryFromDeclarations
            [listLikeDeclaration typeName zeroName stepName False]
          modeled payload = TypeApplication
            (TypeConstructor typeName) payload
          builtinTarget = FunctionType
            (listOf closedPayloadType) (listOf closedPayloadType)
          declaredTarget = FunctionType
            (modeled closedPayloadType) (modeled closedPayloadType)
      builtinContext <- expectRight $ Length.sealLengthContext
        Length.defaultLengthLimits inventory Length.BuiltinListSpine
      declaredContext <- expectRight $ Length.sealLengthContext
        Length.defaultLengthLimits inventory
        $ Length.DeclaredListSpine typeName zeroName stepName
      assertLeft
        (Length.LengthContractInputIsNotList 0
          $ modeled closedPayloadType)
        $ Length.sealLengthContractInContext
            Length.defaultLengthLimits builtinContext
            declaredTarget identityLengthContract
      assertLeft
        (Length.LengthContractInputIsNotList 0
          $ listOf closedPayloadType)
        $ Length.sealLengthContractInContext
            Length.defaultLengthLimits declaredContext
            builtinTarget identityLengthContract
  ]

contractTests :: TestTree
contractTests = testGroup "checked contracts"
  [ testCase "admit opaque impredicative list payloads" $ do
      let payload = polymorphicIdentityType
          target = FunctionType (listOf payload) (listOf payload)
      checked <- expectRight $ sealContract
        Length.defaultLengthLimits target identityLengthContract
      Length.checkedLengthContractTarget checked @?= target
      Length.checkedLengthContractInputCount checked @?= 1
      Length.checkedLengthContractPrecondition checked @?=
        Length.LengthTruth True
      Length.checkedLengthContractPostcondition checked @?=
        Length.LengthEqual
          (Length.LengthVariable $ Length.LengthInput 0)
          (Length.LengthVariable Length.LengthResult)
  , testCase "reject a direct higher-rank term input" $ do
      let rankNInput = polymorphicIdentityType
          target = FunctionType rankNInput (listOf rankNInput)
      case sealContract
          Length.defaultLengthLimits target identityLengthContract of
        Left (Length.LengthContractInputIsNotList 0 rejected) ->
          rejected @?= rankNInput
        Left other -> assertFailure $ "unexpected rejection: " ++ show other
        Right _ -> assertFailure "direct higher-rank input was admitted as a list"
  , testCase "require proper-kind authority for opaque list payloads" $ do
      let illKindedTarget =
            listOf (TypeConstructor listName) :: Type String
      case sealContract Length.defaultLengthLimits
          illKindedTarget trivialLengthContract of
        Left Length.LengthContractTargetKindError{} -> pure ()
        Left other -> assertFailure $ "unexpected rejection: " ++ show other
        Right _ -> assertFailure "an ill-kinded list payload was admitted"
  , testCase "reject a proper-kinded leading contract context" $ do
      className <- expectName "Fixture.ContractConstraint"
      let classDeclaration :: Declaration String () ()
          classDeclaration = ClassDeclaration () className [] [] []
          target = ForallType [] [Constraint className []]
            $ listOf closedPayloadType
      inventory <- expectRight $ mkInventory
        ClosedKindInventory [classDeclaration]
      assertLeft Length.LengthContractConstrainedTarget
        $ Length.sealLengthContract Length.defaultLengthLimits
            inventory target trivialLengthContract
  , testCase "reject result references in a precondition" $ do
      let source = Length.LengthContractSource
            { Length.lengthContractPrecondition = Length.LengthEqual
                (Length.LengthVariable Length.LengthResult)
                (Length.LengthLiteral 0)
            , Length.lengthContractPostcondition = Length.LengthTruth True
            }
      assertLeft
        (Length.LengthContractPreconditionError
          Length.LengthResultNotAvailableInPrecondition)
        $ sealContract Length.defaultLengthLimits
            (listOf closedPayloadType) source
  , testCase "reject input references outside the target function spine" $ do
      let source = Length.LengthContractSource
            { Length.lengthContractPrecondition = Length.LengthTruth True
            , Length.lengthContractPostcondition = Length.LengthEqual
                (Length.LengthVariable $ Length.LengthInput 1)
                (Length.LengthVariable Length.LengthResult)
            }
      assertLeft
        (Length.LengthContractPostconditionError
          $ Length.LengthInputReferenceOutOfRange 1 1)
        $ sealContract Length.defaultLengthLimits
            (FunctionType
              (listOf closedPayloadType)
              (listOf closedPayloadType)) source
  , testCase "report the first input beyond an exact zero bound" $ do
      let limits = limitsWith $ \source -> source
            { Length.lengthLimitSourceContractInputs = 0 }
      assertLeft (Length.LengthContractInputLimitExceeded 0 1)
        $ sealContract limits
            (FunctionType
              (listOf closedPayloadType)
              (listOf closedPayloadType)) identityLengthContract
  , testCase "bound raw syntax, conjunctions, widths, and literals" $ do
      let target = FunctionType
            (listOf closedPayloadType)
            (listOf closedPayloadType)
          syntaxLimits = limitsWith $ \source -> source
            { Length.lengthLimitSourceSyntaxNodes = 1 }
          syntaxSource = contractWith
            (Length.LengthNot $ Length.LengthTruth True)
            (Length.LengthTruth True)
          clauseLimits = limitsWith $ \source -> source
            { Length.lengthLimitSourceFormulaClauses = 1 }
          clauses = Length.LengthAll
            [Length.LengthTruth True, Length.LengthTruth False]
          widthLimits = limitsWith $ \source -> source
            { Length.lengthLimitSourceCollectionWidth = 1 }
          literalLimits = limitsWith $ \source -> source
            { Length.lengthLimitSourceLiteralBits = 3 }
          literalSource = contractWith
            (Length.LengthEqual
              (Length.LengthVariable $ Length.LengthInput 0)
              (Length.LengthLiteral 8))
            (Length.LengthTruth True)
          foldedSumSource = contractWith
            (Length.LengthEqual
              (Length.LengthSum
                [Length.LengthLiteral 7, Length.LengthLiteral 1])
              (Length.LengthLiteral 0))
            (Length.LengthTruth True)
          foldedScaleSource = contractWith
            (Length.LengthEqual
              (Length.LengthScale 2 $ Length.LengthLiteral 4)
              (Length.LengthLiteral 0))
            (Length.LengthTruth True)
      assertLeft
        (Length.LengthContractPreconditionError
          $ Length.LengthSyntaxNodeLimitExceeded 1 2)
        $ sealContract syntaxLimits target syntaxSource
      assertLeft
        (Length.LengthContractPreconditionError
          $ Length.LengthFormulaClauseLimitExceeded 1 2)
        $ sealContract clauseLimits target
            (contractWith clauses $ Length.LengthTruth True)
      assertLeft
        (Length.LengthContractPreconditionError
          $ Length.LengthSyntaxCollectionLimitExceeded
              Length.LengthConjunctionClauses 1 2)
        $ sealContract widthLimits target
            (contractWith clauses $ Length.LengthTruth True)
      assertLeft
        (Length.LengthContractPreconditionError
          $ Length.LengthLiteralBitLimitExceeded 3 4)
        $ sealContract literalLimits target literalSource
      assertLeft
        (Length.LengthContractPreconditionError
          $ Length.LengthLiteralBitLimitExceeded 3 4)
        $ sealContract literalLimits target foldedSumSource
      assertLeft
        (Length.LengthContractPreconditionError
          $ Length.LengthLiteralBitLimitExceeded 3 4)
        $ sealContract literalLimits target foldedScaleSource
  , testCase "bound target structure and type collections" $ do
      let nodeLimits = limitsWith $ \source -> source
            { Length.lengthLimitSourceTypeNodes = 0 }
          widthLimits = limitsWith $ \source -> source
            { Length.lengthLimitSourceCollectionWidth = 1 }
          wideTarget = ForallType ["left", "right"] []
            (listOf closedPayloadType)
      assertLeft
        (Length.LengthContractTargetBoundError
          $ Length.LengthTypeNodeLimitExceeded 0 1)
        $ sealContract nodeLimits
            (listOf closedPayloadType) trivialLengthContract
      assertLeft
        (Length.LengthContractTargetBoundError
          $ Length.LengthTypeCollectionLimitExceeded
              Length.LengthForallBinders 1 2)
        $ sealContract widthLimits wideTarget
            trivialLengthContract
  , testCase "surface a bounded fingerprint failure at max plus one" $ do
      let limits = limitsWith $ \source -> source
            { Length.lengthLimitSourceFingerprintBytes = 0 }
      assertLeft (Length.LengthContractFingerprintLimitExceeded 0 1)
        $ sealContract limits
            (listOf closedPayloadType) trivialLengthContract
  ]

providerTests :: TestTree
providerTests = testGroup "assumed provider inventory"
  [ testCase "retain closed schemes, roles, transfers, and assumed trust" $ do
      providerName <- expectName "Fixture.preserve"
      let source = unaryListProvider providerName
            Length.LengthSpineArgument
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
      inventory <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [source]
      case Length.lookupCheckedLengthProviderSummary providerName inventory of
        Nothing -> assertFailure "checked provider disappeared from inventory"
        Just checked -> do
          Length.checkedLengthProviderName checked @?= providerName
          Length.checkedLengthProviderScheme checked @?=
            Length.lengthProviderScheme source
          Length.checkedLengthProviderArgumentRoles checked @?=
            [Length.LengthSpineArgument]
          Length.checkedLengthProviderTransfer checked @?=
            Length.LengthVariable (Length.LengthProviderArgument 0)
          Length.checkedLengthProviderTrust checked @?=
            Length.AssumedProviderLaw
  , testCase "admit non-list arguments only when their spines are unobserved" $ do
      providerName <- expectName "Fixture.constant"
      let scheme = FunctionType closedPayloadType
            (listOf closedPayloadType)
          source = providerSource providerName scheme
            [Length.LengthUnobservedArgument]
            (Length.LengthLiteral 0)
      inventory <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [source]
      let summaries = Length.checkedLengthProviderSummaries inventory
      map Length.checkedLengthProviderArgumentRoles summaries @?=
        [[Length.LengthUnobservedArgument]]
  , testCase "admit rank-N unobserved arguments and impredicative list payloads" $ do
      unobservedName <- expectName "Fixture.rankNUnobserved"
      spineName <- expectName "Fixture.impredicativeSpine"
      let unobserved = providerSource unobservedName
            (FunctionType polymorphicIdentityType
              $ listOf closedPayloadType)
            [Length.LengthUnobservedArgument]
            (Length.LengthLiteral 0)
          impredicative = providerSource spineName
            (FunctionType
              (listOf polymorphicIdentityType)
              (listOf polymorphicIdentityType))
            [Length.LengthSpineArgument]
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
      inventory <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [unobserved, impredicative]
      map Length.checkedLengthProviderName
          (Length.checkedLengthProviderSummaries inventory) @?=
        sort [unobservedName, spineName]
  , testCase "resolve names before inspecting poisoned summary fields" $ do
      providerName <- expectName "Fixture.missing"
      let cyclicScheme = FunctionType cyclicScheme cyclicScheme
          cyclicRoles = Length.LengthSpineArgument : cyclicRoles
          cyclicTransfer = Length.LengthScale 1 cyclicTransfer
          source = providerSource providerName cyclicScheme
            cyclicRoles cyclicTransfer
      observed <- evaluateWithin $ Length.sealLengthProviderInventory
        Length.defaultLengthLimits fixtureInventory [source]
      assertLeft
        (Length.LengthProviderSummaryRejected 0 providerName
          $ Length.LengthProviderNotInSourceInventory providerName)
        observed
  , testCase "reject source-scheme mismatch before semantic shape errors" $ do
      providerName <- expectName "Fixture.mismatch"
      let sourceScheme = ForallType ["source"] [] $ FunctionType
            (listOf $ TypeVariable "source")
            (listOf $ TypeVariable "source")
          claimedScheme = ForallType ["claimed"] [] $ FunctionType
            closedPayloadType (listOf $ TypeVariable "claimed")
          source = providerSource providerName claimedScheme []
            (Length.LengthVariable $ Length.LengthProviderArgument 99)
          inventory = fixtureInventoryFromDeclarations
            [ValueDeclaration $ ValueSignature () providerName sourceScheme]
      assertLeft
        (Length.LengthProviderSummaryRejected 0 providerName
          $ Length.LengthProviderSourceSchemeMismatch
              sourceScheme claimedScheme)
        $ Length.sealLengthProviderInventory
            Length.defaultLengthLimits inventory [source]
  , testCase "accept alpha-equivalent claims and retain the source scheme" $ do
      providerName <- expectName "Fixture.alphaClaim"
      let sourceScheme = ForallType ["source"] [] $ FunctionType
            (listOf $ TypeVariable "source")
            (listOf $ TypeVariable "source")
          claimedScheme = ForallType ["claimed"] [] $ FunctionType
            (listOf $ TypeVariable "claimed")
            (listOf $ TypeVariable "claimed")
          source = providerSource providerName claimedScheme
            [Length.LengthSpineArgument]
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
          inventory = fixtureInventoryFromDeclarations
            [ValueDeclaration $ ValueSignature () providerName sourceScheme]
      checkedInventory <- expectRight $ Length.sealLengthProviderInventory
        Length.defaultLengthLimits inventory [source]
      checked <- case Length.lookupCheckedLengthProviderSummary
          providerName checkedInventory of
        Nothing -> assertFailure "alpha-equivalent provider disappeared"
        Just summary -> pure summary
      Length.checkedLengthProviderScheme checked @?= sourceScheme
  , testCase "close implicit source variables before exact comparison" $ do
      providerName <- expectName "Fixture.implicit"
      let openSourceScheme = FunctionType
            (listOf $ TypeVariable "free")
            (listOf $ TypeVariable "free")
          closedClaim = ForallType ["renamed"] [] $ FunctionType
            (listOf $ TypeVariable "renamed")
            (listOf $ TypeVariable "renamed")
          source = providerSource providerName closedClaim
            [Length.LengthSpineArgument]
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
          inventory = fixtureInventoryFromDeclarations
            [ValueDeclaration $ ValueSignature () providerName openSourceScheme]
      checkedInventory <- expectRight $ Length.sealLengthProviderInventory
        Length.defaultLengthLimits inventory [source]
      case Length.lookupCheckedLengthProviderSummary
          providerName checkedInventory of
        Nothing -> assertFailure "implicitly closed provider disappeared"
        Just checked -> Length.checkedLengthProviderScheme checked @?=
          ForallType ["free"] [] openSourceScheme
  , testCase "bound only the exact provider scheme, not unrelated terms" $ do
      providerName <- expectName "Fixture.localScheme"
      unrelatedName <- expectName "Fixture.unrelatedWideScheme"
      let provider = unaryListProvider providerName
            Length.LengthSpineArgument
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
          unrelatedScheme = ForallType ["wide"] []
            $ foldr FunctionType (TypeVariable "wide")
            $ replicate 128 $ TypeVariable "wide"
          inventory = fixtureInventoryFromDeclarations
            [ ValueDeclaration
                $ ValueSignature () unrelatedName unrelatedScheme
            , ValueDeclaration
                $ ValueSignature () providerName
                $ Length.lengthProviderScheme provider
            ]
          exactSchemeLimit = limitsWith $ \limits -> limits
            { Length.lengthLimitSourceTypeNodes = 8 }
      checked <- expectRight $ Length.sealLengthProviderInventory
        exactSchemeLimit inventory [provider]
      map Length.checkedLengthProviderName
          (Length.checkedLengthProviderSummaries checked) @?= [providerName]
  , testCase "reject a proper-kinded leading provider context" $ do
      className <- expectName "Fixture.ProviderConstraint"
      providerName <- expectName "Fixture.constrained"
      let classDeclaration :: Declaration String () ()
          classDeclaration = ClassDeclaration () className [] [] []
          scheme = ForallType [] [Constraint className []]
            $ FunctionType
                (listOf closedPayloadType)
                (listOf closedPayloadType)
          source = providerSource providerName scheme
            [Length.LengthSpineArgument]
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
      inventory <- expectRight $ mkInventory
        ClosedKindInventory
          [ classDeclaration
          , ValueDeclaration $ ValueSignature () providerName scheme
          ]
      assertLeft
        (Length.LengthProviderSummaryRejected 0 providerName
          Length.LengthProviderConstrainedScheme)
        $ Length.sealLengthProviderInventory
            Length.defaultLengthLimits inventory [source]
  , testCase "reject direct rank-N spines and observed non-list arguments" $ do
      rankNName <- expectName "Fixture.rankN"
      nonListName <- expectName "Fixture.nonList"
      let rankNScheme = FunctionType polymorphicIdentityType
            (listOf closedPayloadType)
          rankNSource = providerSource rankNName rankNScheme
            [Length.LengthSpineArgument]
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
          nonListScheme = FunctionType closedPayloadType
            (listOf closedPayloadType)
          nonListSource = providerSource nonListName nonListScheme
            [Length.LengthSpineArgument]
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
      case sealProviderInventory
          Length.defaultLengthLimits [rankNSource] of
        Left (Length.LengthProviderSummaryRejected 0 name
            (Length.LengthProviderSpineArgumentIsNotList 0 _)) ->
          name @?= rankNName
        Left other -> assertFailure $ "unexpected rejection: " ++ show other
        Right _ -> assertFailure "direct higher-rank provider input was admitted"
      assertLeft
        (Length.LengthProviderSummaryRejected 0 nonListName
          $ Length.LengthProviderSpineArgumentIsNotList 0 closedPayloadType)
        $ sealProviderInventory
            Length.defaultLengthLimits [nonListSource]
  , testCase "reject transfer references to absent or unobserved arguments" $ do
      providerName <- expectName "Fixture.badTransfer"
      let scheme = FunctionType closedPayloadType
            (listOf closedPayloadType)
          outOfRange = providerSource providerName scheme
            [Length.LengthUnobservedArgument]
            (Length.LengthVariable $ Length.LengthProviderArgument 1)
          unobserved = providerSource providerName scheme
            [Length.LengthUnobservedArgument]
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
      assertLeft
        (Length.LengthProviderSummaryRejected 0 providerName
          $ Length.LengthProviderTransferError
          $ Length.LengthProviderReferenceOutOfRange 1 1)
        $ sealProviderInventory
            Length.defaultLengthLimits [outOfRange]
      assertLeft
        (Length.LengthProviderSummaryRejected 0 providerName
          $ Length.LengthProviderTransferError
          $ Length.LengthProviderReferenceIsUnobserved 0)
        $ sealProviderInventory
            Length.defaultLengthLimits [unobserved]
  , testCase "reject role arity mismatches and duplicate names" $ do
      providerName <- expectName "Fixture.duplicate"
      let source = unaryListProvider providerName
            Length.LengthSpineArgument
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
          missingRole = source { Length.lengthProviderArgumentRoles = [] }
      assertLeft
        (Length.LengthProviderSummaryRejected 0 providerName
          $ Length.LengthProviderRoleArityMismatch 1 0)
        $ sealProviderInventory
            Length.defaultLengthLimits [missingRole]
      assertLeft (Length.DuplicateLengthProvider providerName)
        $ sealProviderInventory
            Length.defaultLengthLimits [source, source]
      assertLeft (Length.DuplicateLengthProvider providerName)
        $ sealProviderInventory Length.defaultLengthLimits
            [ source
            , source { Length.lengthProviderArgumentRoles = [] }
            ]
  , testCase "canonicalize source order without losing deterministic name order" $ do
      alpha <- expectName "Fixture.alpha"
      beta <- expectName "Fixture.beta"
      let first = unaryListProvider alpha Length.LengthSpineArgument
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
          second = unaryListProvider beta Length.LengthSpineArgument
            (Length.LengthScale 2
              $ Length.LengthVariable $ Length.LengthProviderArgument 0)
      forward <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [first, second]
      reverseOrder <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [second, first]
      Length.lengthProviderInventoryFingerprint forward @?=
        Length.lengthProviderInventoryFingerprint reverseOrder
      map Length.checkedLengthProviderName
          (Length.checkedLengthProviderSummaries forward) @?=
        sort [alpha, beta]
      map Length.checkedLengthProviderName
          (Length.checkedLengthProviderSummaries reverseOrder) @?=
        sort [alpha, beta]
  , testCase "enforce provider count, argument count, and fingerprint bounds" $ do
      providerName <- expectName "Fixture.bounded"
      let source = unaryListProvider providerName
            Length.LengthSpineArgument
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
          noSummaries = limitsWith $ \limits -> limits
            { Length.lengthLimitSourceProviderSummaries = 0 }
          noArguments = limitsWith $ \limits -> limits
            { Length.lengthLimitSourceProviderArguments = 0 }
          noFingerprint = limitsWith $ \limits -> limits
            { Length.lengthLimitSourceFingerprintBytes = 0 }
      assertLeft (Length.LengthProviderSummaryLimitExceeded 0 1)
        $ sealProviderInventory noSummaries [source]
      assertLeft
        (Length.LengthProviderSummaryRejected 0 providerName
          $ Length.LengthProviderArgumentLimitExceeded 0 1)
        $ sealProviderInventory noArguments [source]
      assertLeft
        (Length.LengthProviderInventoryFingerprintLimitExceeded 0 1)
        $ sealProviderInventory noFingerprint
            ([] :: [Length.LengthProviderSummarySource String])
  ]

evaluationTests :: TestTree
evaluationTests = testGroup "solver-neutral concrete replay"
  [ testCase "validate evaluation limits in declaration order" $ do
      Evaluate.mkLengthEvaluationLimits
          Evaluate.defaultLengthEvaluationLimitSource @?=
        Right Evaluate.defaultLengthEvaluationLimits
      Evaluate.lengthAssignmentValueBitLimit
          Evaluate.defaultLengthEvaluationLimits @?= 4096
      Evaluate.lengthIntermediateValueBitLimit
          Evaluate.defaultLengthEvaluationLimits @?= 4096
      let bothNegative = Evaluate.LengthEvaluationLimitSource
            { Evaluate.lengthEvaluationLimitSourceAssignmentValueBits = -1
            , Evaluate.lengthEvaluationLimitSourceIntermediateValueBits = -2
            }
          secondNegative = bothNegative
            { Evaluate.lengthEvaluationLimitSourceAssignmentValueBits = 0 }
          allZero = secondNegative
            { Evaluate.lengthEvaluationLimitSourceIntermediateValueBits = 0 }
      Evaluate.mkLengthEvaluationLimits bothNegative @?= Left
        (Evaluate.NegativeLengthEvaluationLimit
          Evaluate.LengthAssignmentValueBits (-1))
      Evaluate.mkLengthEvaluationLimits secondNegative @?= Left
        (Evaluate.NegativeLengthEvaluationLimit
          Evaluate.LengthIntermediateValueBits (-2))
      limits <- expectRight $ Evaluate.mkLengthEvaluationLimits allZero
      map ($ limits)
          [ Evaluate.lengthAssignmentValueBitLimit
          , Evaluate.lengthIntermediateValueBitLimit
          ] @?= [0, 0]
  , testCase "classify the three contract outcomes exactly" $ do
      let input = Length.LengthVariable $ Length.LengthInput 0
          result = Length.LengthVariable Length.LengthResult
          source = contractWith
            (Length.LengthAtMost input $ Length.LengthLiteral 2)
            (Length.LengthEqual result
              $ Length.LengthSum [input, Length.LengthLiteral 1])
          target = FunctionType
            (listOf closedPayloadType) (listOf closedPayloadType)
      contract <- expectRight $ sealContract
        Length.defaultLengthLimits target source
      let replay inputs observedResult =
            Evaluate.evaluateLengthContractAssignment
              Evaluate.defaultLengthEvaluationLimits contract
              $ Evaluate.LengthContractAssignment inputs observedResult
      replay [3] 99 @?= Right Evaluate.LengthPreconditionNotMet
      replay [1] 2 @?= Right Evaluate.LengthPostconditionSatisfied
      replay [1] 3 @?= Right Evaluate.LengthPostconditionViolated
      [ Evaluate.LengthPreconditionNotMet
        , Evaluate.LengthPostconditionSatisfied
        , Evaluate.LengthPostconditionViolated
        ] @?= [minBound .. maxBound]
  , testCase "reject contract assignment arity before inspecting values" $ do
      let target = FunctionType
            (listOf closedPayloadType) (listOf closedPayloadType)
      contract <- expectRight $ sealContract
        Length.defaultLengthLimits target identityLengthContract
      let replay inputs = Evaluate.evaluateLengthContractAssignment
            Evaluate.defaultLengthEvaluationLimits contract
            $ Evaluate.LengthContractAssignment inputs 0
      replay [] @?= Left
        (Evaluate.LengthContractAssignmentArityMismatch 1 0)
      replay [0, 2 ^ (5000 :: Int)] @?= Left
        (Evaluate.LengthContractAssignmentArityMismatch 1 2)
  , testCase "reject cyclic assignments at the first excess cell" $ do
      providerName <- expectName "Fixture.cyclicReplay"
      let target = FunctionType
            (listOf closedPayloadType) (listOf closedPayloadType)
          cyclicInputs = 0 : cyclicInputs
          cyclicArguments =
            Evaluate.ObservedSpineLength 0 : cyclicArguments
          provider = unaryListProvider providerName
            Length.LengthSpineArgument
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
      contract <- expectRight $ sealContract
        Length.defaultLengthLimits target identityLengthContract
      checkedProvider <- expectCheckedProvider provider
      contractResult <- evaluateWithin
        $ Evaluate.evaluateLengthContractAssignment
            Evaluate.defaultLengthEvaluationLimits contract
            $ Evaluate.LengthContractAssignment cyclicInputs 0
      providerResult <- evaluateWithin
        $ Evaluate.evaluateLengthProviderApplication
            Evaluate.defaultLengthEvaluationLimits checkedProvider
            cyclicArguments
      contractResult @?= Left
        (Evaluate.LengthContractAssignmentArityMismatch 1 2)
      providerResult @?= Left
        (Evaluate.LengthProviderAssignmentArityMismatch 1 2)
  , testCase "enforce assignment and intermediate bit bounds by site" $ do
      let target = FunctionType
            (listOf closedPayloadType) (listOf closedPayloadType)
          four = 4
          inputLimits = evaluationLimitsWith 2 8
      contract <- expectRight $ sealContract
        Length.defaultLengthLimits target identityLengthContract
      Evaluate.evaluateLengthContractAssignment inputLimits contract
          (Evaluate.LengthContractAssignment [four] 0) @?= Left
        (Evaluate.LengthEvaluationValueBitLimitExceeded
          (Evaluate.LengthContractInputValue 0) 2 3)
      Evaluate.evaluateLengthContractAssignment inputLimits contract
          (Evaluate.LengthContractAssignment [0] four) @?= Left
        (Evaluate.LengthEvaluationValueBitLimitExceeded
          Evaluate.LengthContractResultValue 2 3)

      providerName <- expectName "Fixture.bitBounded"
      let transfer = Length.LengthScale 2
            $ Length.LengthVariable $ Length.LengthProviderArgument 0
          provider = unaryListProvider providerName
            Length.LengthSpineArgument transfer
      checked <- expectCheckedProvider provider
      Evaluate.evaluateLengthProviderApplication inputLimits checked
          [Evaluate.ObservedSpineLength four] @?= Left
        (Evaluate.LengthEvaluationValueBitLimitExceeded
          (Evaluate.LengthProviderSpineValue 0) 2 3)
      let intermediateLimits = evaluationLimitsWith 3 3
      Evaluate.evaluateLengthProviderApplication intermediateLimits checked
          [Evaluate.ObservedSpineLength four] @?= Left
        (Evaluate.LengthEvaluationValueBitLimitExceeded
          Evaluate.LengthIntermediateValue 3 4)
  , testCase "report multi-input assignment bounds from left to right" $ do
      let target = FunctionType
            (listOf closedPayloadType)
            $ FunctionType
                (listOf closedPayloadType) (listOf closedPayloadType)
          limits = evaluationLimitsWith 2 8
      contract <- expectRight $ sealContract
        Length.defaultLengthLimits target trivialLengthContract
      let replay inputs result = Evaluate.evaluateLengthContractAssignment
            limits contract $ Evaluate.LengthContractAssignment inputs result
      replay [4, 8] 16 @?= Left
        (Evaluate.LengthEvaluationValueBitLimitExceeded
          (Evaluate.LengthContractInputValue 0) 2 3)
      replay [0, 4] 8 @?= Left
        (Evaluate.LengthEvaluationValueBitLimitExceeded
          (Evaluate.LengthContractInputValue 1) 2 3)
      replay [0, 0] 4 @?= Left
        (Evaluate.LengthEvaluationValueBitLimitExceeded
          Evaluate.LengthContractResultValue 2 3)
  , testCase "enforce exact provider arity and observed-role boundaries" $ do
      providerName <- expectName "Fixture.rolesReplay"
      let scheme = FunctionType
            (listOf closedPayloadType)
            $ FunctionType closedPayloadType (listOf closedPayloadType)
          provider = providerSource providerName scheme
            [ Length.LengthSpineArgument
            , Length.LengthUnobservedArgument
            ]
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
      checked <- expectCheckedProvider provider
      let replay = Evaluate.evaluateLengthProviderApplication
            Evaluate.defaultLengthEvaluationLimits checked
      replay [] @?= Left
        (Evaluate.LengthProviderAssignmentArityMismatch 2 0)
      replay
          [ Evaluate.ObservedSpineLength 1
          , Evaluate.UnobservedLengthArgument
          , Evaluate.UnobservedLengthArgument
          ] @?= Left
        (Evaluate.LengthProviderAssignmentArityMismatch 2 3)
      replay
          [ Evaluate.UnobservedLengthArgument
          , Evaluate.UnobservedLengthArgument
          ] @?= Left
        (Evaluate.LengthProviderArgumentRoleMismatch 0
          Length.LengthSpineArgument Evaluate.UnobservedLengthArgument)
      replay
          [Evaluate.ObservedSpineLength 1, Evaluate.ObservedSpineLength 2] @?=
        Left (Evaluate.LengthProviderArgumentRoleMismatch 1
          Length.LengthUnobservedArgument
          $ Evaluate.ObservedSpineLength 2)
      replay
          [Evaluate.ObservedSpineLength 7, Evaluate.UnobservedLengthArgument]
        @?= Right 7
  , testCase "evaluate natural operators without integer subtraction" $ do
      providerName <- expectName "Fixture.operatorReplay"
      let input = Length.LengthVariable $ Length.LengthProviderArgument 0
          transfer = Length.LengthSum
            [ Length.LengthScale 2 input
            , Length.LengthMonus (Length.LengthLiteral 3)
                (Length.LengthLiteral 5)
            , Length.LengthMinimum
                (Length.LengthLiteral 7) (Length.LengthLiteral 2)
            , Length.LengthMaximum
                (Length.LengthLiteral 1) (Length.LengthLiteral 6)
            ]
          provider = unaryListProvider providerName
            Length.LengthSpineArgument transfer
      checked <- expectCheckedProvider provider
      Evaluate.evaluateLengthProviderApplication
          Evaluate.defaultLengthEvaluationLimits checked
          [Evaluate.ObservedSpineLength 4] @?= Right 16
  , testCase "agree with direct natural arithmetic across small inputs" $ do
      providerName <- expectName "Fixture.smallNaturalDifferential"
      let input = Length.LengthVariable $ Length.LengthProviderArgument 0
          condition = Length.LengthAll
            [ Length.LengthAtMost input $ Length.LengthLiteral 4
            , Length.LengthNot
                $ Length.LengthEqual input $ Length.LengthLiteral 2
            ]
          transfer = Length.LengthIf condition
            (Length.LengthSum
              [ Length.LengthScale 2 input
              , Length.LengthMonus
                  (Length.LengthLiteral 5) input
              , Length.LengthMinimum input $ Length.LengthLiteral 3
              ])
            (Length.LengthMaximum input $ Length.LengthLiteral 4)
          expected value
            | value <= 4 && value /= 2 =
                2 * value
                  + (if value <= 5 then 5 - value else 0)
                  + min value 3
            | otherwise = max value 4
          provider = unaryListProvider providerName
            Length.LengthSpineArgument transfer
      checked <- expectCheckedProvider provider
      mapM_ (\value ->
        Evaluate.evaluateLengthProviderApplication
            Evaluate.defaultLengthEvaluationLimits checked
            [Evaluate.ObservedSpineLength value] @?= Right (expected value))
        [0 .. 12]
  , testCase "skip dead if branches and short-circuit conjunctions" $ do
      providerName <- expectName "Fixture.shortCircuit"
      let input = Length.LengthVariable $ Length.LengthProviderArgument 0
          dangerous = Length.LengthScale 8
            $ Length.LengthMaximum input $ Length.LengthLiteral 1
          firstClause = Length.LengthEqual input $ Length.LengthLiteral 1
          dangerousClause = Length.LengthEqual dangerous
            $ Length.LengthLiteral 0
          transfer = Length.LengthIf
            (Length.LengthAll [firstClause, dangerousClause])
            (Length.LengthLiteral 8)
            (Length.LengthLiteral 1)
          provider = unaryListProvider providerName
            Length.LengthSpineArgument transfer
          tightLimits = evaluationLimitsWith 4 1
      checked <- expectCheckedProvider provider
      Evaluate.evaluateLengthProviderApplication tightLimits checked
          [Evaluate.ObservedSpineLength 0] @?= Right 1
      Evaluate.evaluateLengthProviderApplication tightLimits checked
          [Evaluate.ObservedSpineLength 1] @?= Left
        (Evaluate.LengthEvaluationValueBitLimitExceeded
          Evaluate.LengthIntermediateValue 1 2)
  ]

normalizationTests :: TestTree
normalizationTests = testGroup "normalization"
  [ testCase "make conjunction and additive permutations idempotent" $ do
      let target = FunctionType
            (listOf closedPayloadType)
            (listOf closedPayloadType)
          input = Length.LengthVariable $ Length.LengthInput 0
          result = Length.LengthVariable Length.LengthResult
          lowerBound = Length.LengthAtMost (Length.LengthLiteral 1) input
          fixedPoint = Length.LengthEqual input input
          leftSource = contractWith
            (Length.LengthAll
              [ fixedPoint
              , lowerBound
              , fixedPoint
              , Length.LengthTruth True
              ])
            (Length.LengthEqual result
              $ Length.LengthSum
                  [input, Length.LengthLiteral 0, Length.LengthLiteral 1])
          rightSource = contractWith
            (Length.LengthAll
              [ Length.LengthAll [lowerBound, fixedPoint]
              , fixedPoint
              ])
            (Length.LengthEqual
              (Length.LengthSum
                [Length.LengthLiteral 1, input, Length.LengthLiteral 0])
              result)
      left <- expectRight $ sealContract
        Length.defaultLengthLimits target leftSource
      right <- expectRight $ sealContract
        Length.defaultLengthLimits target rightSource
      Length.checkedLengthContractPrecondition left @?=
        Length.checkedLengthContractPrecondition right
      Length.checkedLengthContractPostcondition left @?=
        Length.checkedLengthContractPostcondition right
      Length.lengthContractFingerprint left @?=
        Length.lengthContractFingerprint right
      resealed <- expectRight $ sealContract
        Length.defaultLengthLimits
        (Length.checkedLengthContractTarget left)
        Length.LengthContractSource
          { Length.lengthContractPrecondition =
              Length.checkedLengthContractPrecondition left
          , Length.lengthContractPostcondition =
              Length.checkedLengthContractPostcondition left
          }
      Length.checkedLengthContractPrecondition resealed @?=
        Length.checkedLengthContractPrecondition left
      Length.checkedLengthContractPostcondition resealed @?=
        Length.checkedLengthContractPostcondition left
      Length.lengthContractFingerprint resealed @?=
        Length.lengthContractFingerprint left
  , testCase "keep alpha-renamed impredicative payloads opaque" $ do
      let firstPayload = ForallType ["a"] [] $ FunctionType
            (TypeVariable "a") (TypeVariable "a")
          secondPayload = ForallType ["renamed"] [] $ FunctionType
            (TypeVariable "renamed") (TypeVariable "renamed")
      first <- expectRight $ sealContract
        Length.defaultLengthLimits
        (listOf firstPayload) trivialLengthContract
      second <- expectRight $ sealContract
        Length.defaultLengthLimits
        (listOf secondPayload) trivialLengthContract
      Length.lengthContractFingerprint first @?=
        Length.lengthContractFingerprint second
  , testCase "canonicalize minimum and maximum association and order" $ do
      let target = FunctionType
            (listOf closedPayloadType)
            (listOf closedPayloadType)
          input = Length.LengthVariable $ Length.LengthInput 0
          result = Length.LengthVariable Length.LengthResult
          firstSource = contractWith (Length.LengthTruth True)
            (Length.LengthAll
              [ Length.LengthEqual result
                  (Length.LengthMinimum input
                    $ Length.LengthMinimum
                        (Length.LengthLiteral 3) result)
              , Length.LengthEqual input
                  (Length.LengthMaximum result
                    $ Length.LengthMaximum
                        (Length.LengthLiteral 2) input)
              ])
          reassociated = contractWith (Length.LengthTruth True)
            (Length.LengthAll
              [ Length.LengthEqual input
                  (Length.LengthMaximum
                    (Length.LengthMaximum input
                      $ Length.LengthLiteral 2)
                    result)
              , Length.LengthEqual result
                  (Length.LengthMinimum
                    (Length.LengthMinimum result input)
                    $ Length.LengthLiteral 3)
              ])
      first <- expectRight $ sealContract
        Length.defaultLengthLimits target firstSource
      second <- expectRight $ sealContract
        Length.defaultLengthLimits target reassociated
      Length.checkedLengthContractPostcondition first @?=
        Length.checkedLengthContractPostcondition second
      Length.lengthContractFingerprint first @?=
        Length.lengthContractFingerprint second
  ]

productiveBoundTests :: TestTree
productiveBoundTests = testGroup "productive bounded traversal"
  [ testCase "stop on a cyclic target at the first type node past the bound" $ do
      let limits = limitsWith $ \source -> source
            { Length.lengthLimitSourceTypeNodes = 4 }
          cyclicTarget = FunctionType
            (listOf closedPayloadType) cyclicTarget
      observed <- evaluateWithin $ sealContract
        limits cyclicTarget trivialLengthContract
      assertLeft
        (Length.LengthContractTargetBoundError
          $ Length.LengthTypeNodeLimitExceeded 4 5)
        observed
  , testCase "stop on a cyclic formula AST at the first node past the bound" $ do
      let limits = limitsWith $ \limitSource -> limitSource
            { Length.lengthLimitSourceSyntaxNodes = 3 }
          cyclicFormula = Length.LengthNot cyclicFormula
          source = contractWith cyclicFormula (Length.LengthTruth True)
      observed <- evaluateWithin $ sealContract
        limits (listOf closedPayloadType) source
      assertLeft
        (Length.LengthContractPreconditionError
          $ Length.LengthSyntaxNodeLimitExceeded 3 4)
        observed
  , testCase "stop on cyclic sum terms at collection width plus one" $ do
      let limits = limitsWith $ \limitSource -> limitSource
            { Length.lengthLimitSourceCollectionWidth = 1 }
          cyclicTerms = Length.LengthLiteral 0 : cyclicTerms
          source = contractWith
            (Length.LengthEqual
              (Length.LengthSum cyclicTerms)
              (Length.LengthLiteral 0))
            (Length.LengthTruth True)
      observed <- evaluateWithin $ sealContract
        limits (listOf closedPayloadType) source
      assertLeft
        (Length.LengthContractPreconditionError
          $ Length.LengthSyntaxCollectionLimitExceeded
              Length.LengthSumTerms 1 2)
        observed
  , testCase "stop on a cyclic provider source list at max plus one" $ do
      providerName <- expectName "Fixture.cyclic"
      let limits = limitsWith $ \source -> source
            { Length.lengthLimitSourceProviderSummaries = 1 }
          provider = unaryListProvider providerName
            Length.LengthSpineArgument
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
          cyclicProviders = provider : cyclicProviders
          inventory = providerFixtureInventory [provider]
      observed <- evaluateWithin $ Length.sealLengthProviderInventory
        limits inventory cyclicProviders
      assertLeft (Length.LengthProviderSummaryLimitExceeded 1 2) observed
  , testCase "stop on a cyclic role list at the argument bound" $ do
      providerName <- expectName "Fixture.cyclicRoles"
      let roles = Length.LengthSpineArgument : roles
          provider = (unaryListProvider providerName
            Length.LengthSpineArgument
            (Length.LengthVariable $ Length.LengthProviderArgument 0))
              { Length.lengthProviderArgumentRoles = roles }
      observed <- evaluateWithin $ sealProviderInventory
        Length.defaultLengthLimits [provider]
      assertLeft
        (Length.LengthProviderSummaryRejected 0 providerName
          $ Length.LengthProviderArgumentLimitExceeded 16 17)
        observed
  , testCase "stop on a cyclic provider transfer at the syntax bound" $ do
      providerName <- expectName "Fixture.cyclicTransfer"
      let limits = limitsWith $ \source -> source
            { Length.lengthLimitSourceSyntaxNodes = 3 }
          transfer = Length.LengthScale 1 transfer
          provider = unaryListProvider providerName
            Length.LengthSpineArgument transfer
      observed <- evaluateWithin $ sealProviderInventory limits [provider]
      assertLeft
        (Length.LengthProviderSummaryRejected 0 providerName
          $ Length.LengthProviderTransferError
          $ Length.LengthSyntaxNodeLimitExceeded 3 4)
        observed
  , testCase "validate semantically discarded provider subtrees" $ do
      providerName <- expectName "Fixture.discardedTransfer"
      let limits = limitsWith $ \source -> source
            { Length.lengthLimitSourceSyntaxNodes = 4 }
          cyclic = Length.LengthScale 1 cyclic
          conditional = unaryListProvider providerName
            Length.LengthSpineArgument
            (Length.LengthIf
              (Length.LengthTruth True)
              (Length.LengthLiteral 0)
              cyclic)
          scaledAway = unaryListProvider providerName
            Length.LengthSpineArgument
            (Length.LengthScale 0 cyclic)
      conditionalResult <- evaluateWithin
        $ sealProviderInventory limits [conditional]
      assertLeft
        (Length.LengthProviderSummaryRejected 0 providerName
          $ Length.LengthProviderTransferError
          $ Length.LengthSyntaxNodeLimitExceeded 4 5)
        conditionalResult
      scaledResult <- evaluateWithin
        $ sealProviderInventory limits [scaledAway]
      assertLeft
        (Length.LengthProviderSummaryRejected 0 providerName
          $ Length.LengthProviderTransferError
          $ Length.LengthSyntaxNodeLimitExceeded 4 5)
        scaledResult
  ]

fingerprintTests :: TestTree
fingerprintTests = testGroup "identity sensitivity"
  [ testCase "abstract opaque payloads but distinguish behavioral formulas" $ do
      let firstTarget = listOf closedPayloadType
          secondTarget = listOf
            (FunctionType closedPayloadType closedPayloadType)
          stronger = contractWith (Length.LengthTruth True)
            (Length.LengthEqual
              (Length.LengthVariable Length.LengthResult)
              (Length.LengthLiteral 1))
      baseline <- expectRight $ sealContract
        Length.defaultLengthLimits firstTarget trivialLengthContract
      differentPayload <- expectRight $ sealContract
        Length.defaultLengthLimits secondTarget trivialLengthContract
      differentFormula <- expectRight $ sealContract
        Length.defaultLengthLimits firstTarget stronger
      assertBool "opaque payload leaked into length-contract identity" $
        Length.lengthContractFingerprint baseline ==
          Length.lengthContractFingerprint differentPayload
      assertBool "behavioral formula was omitted from contract identity" $
        Length.lengthContractFingerprint baseline /=
          Length.lengthContractFingerprint differentFormula
  , testCase "distinguish the ordered contract input spine" $ do
      nullary <- expectRight $ sealContract
        Length.defaultLengthLimits
        (listOf closedPayloadType) trivialLengthContract
      unary <- expectRight $ sealContract
        Length.defaultLengthLimits
        (FunctionType
          (listOf closedPayloadType)
          (listOf closedPayloadType))
        trivialLengthContract
      assertBool "contract input arity was omitted from identity" $
        Length.lengthContractFingerprint nullary /=
          Length.lengthContractFingerprint unary
  , testCase "exclude admission limits from contract and inventory identity" $ do
      providerName <- expectName "Fixture.limitIndependent"
      let tightLimits = limitsWith $ \source -> source
            { Length.lengthLimitSourceTypeNodes = 32
            , Length.lengthLimitSourceContractInputs = 1
            , Length.lengthLimitSourceSyntaxNodes = 16
            , Length.lengthLimitSourceFormulaClauses = 8
            , Length.lengthLimitSourceCollectionWidth = 8
            , Length.lengthLimitSourceProviderSummaries = 1
            , Length.lengthLimitSourceProviderArguments = 1
            , Length.lengthLimitSourceLiteralBits = 8
            , Length.lengthLimitSourceFingerprintBytes = 65535
            }
          target = FunctionType
            (listOf closedPayloadType)
            (listOf closedPayloadType)
          provider = unaryListProvider providerName
            Length.LengthSpineArgument
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
      defaultContract <- expectRight $ sealContract
        Length.defaultLengthLimits target identityLengthContract
      tightContract <- expectRight $ sealContract
        tightLimits target identityLengthContract
      defaultInventory <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [provider]
      tightInventory <- expectRight $ sealProviderInventory
        tightLimits [provider]
      Length.lengthContractFingerprint defaultContract @?=
        Length.lengthContractFingerprint tightContract
      Length.lengthProviderInventoryFingerprint defaultInventory @?=
        Length.lengthProviderInventoryFingerprint tightInventory
  , testCase "identify alpha-equivalent closed provider schemes" $ do
      providerName <- expectName "Fixture.alphaEquivalent"
      let scheme binder = ForallType [binder] [] $ FunctionType
            (listOf $ TypeVariable binder)
            (listOf $ TypeVariable binder)
          source binder = providerSource providerName (scheme binder)
            [Length.LengthSpineArgument]
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
      first <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [source "element"]
      renamed <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [source "renamed"]
      Length.lengthProviderInventoryFingerprint first @?=
        Length.lengthProviderInventoryFingerprint renamed
  , testCase "retain ordered provider argument roles in identity" $ do
      providerName <- expectName "Fixture.roles"
      let scheme = FunctionType
            (listOf closedPayloadType)
            (FunctionType
              (listOf closedPayloadType)
              (listOf closedPayloadType))
          source roles = providerSource providerName scheme roles
            (Length.LengthLiteral 0)
      first <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits
        [source
          [ Length.LengthSpineArgument
          , Length.LengthUnobservedArgument
          ]]
      swapped <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits
        [source
          [ Length.LengthUnobservedArgument
          , Length.LengthSpineArgument
          ]]
      assertBool "ordered provider roles were omitted from identity" $
        Length.lengthProviderInventoryFingerprint first /=
          Length.lengthProviderInventoryFingerprint swapped
  , testCase "distinguish provider name, scheme, and assumed transfer" $ do
      firstName <- expectName "Fixture.first"
      secondName <- expectName "Fixture.second"
      let identitySource = unaryListProvider firstName
            Length.LengthSpineArgument
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
          renamedSource = identitySource
            { Length.lengthProviderName = secondName }
          scaledSource = identitySource
            { Length.lengthProviderTransfer = Length.LengthScale 2
                $ Length.LengthVariable $ Length.LengthProviderArgument 0 }
          changedScheme = providerSource firstName
            (FunctionType
              (listOf closedPayloadType)
              (listOf $ FunctionType
                closedPayloadType closedPayloadType))
            [Length.LengthSpineArgument]
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
      baseline <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [identitySource]
      renamed <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [renamedSource]
      scaled <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [scaledSource]
      changed <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [changedScheme]
      let baselineFingerprint =
            Length.lengthProviderInventoryFingerprint baseline
      assertBool "provider name was omitted from inventory identity" $
        baselineFingerprint /= Length.lengthProviderInventoryFingerprint renamed
      assertBool "provider transfer was omitted from inventory identity" $
        baselineFingerprint /= Length.lengthProviderInventoryFingerprint scaled
      assertBool "provider scheme was omitted from inventory identity" $
        baselineFingerprint /= Length.lengthProviderInventoryFingerprint changed
  , testCase "bind contract and provider identity to recursive-field order" $ do
      typeName <- expectName "Fixture.ModelSensitive"
      zeroName <- expectName "Fixture.ModelZero"
      stepName <- expectName "Fixture.ModelStep"
      providerName <- expectName "Fixture.modelProvider"
      let payload = TypeVariable "element"
          modeled = TypeApplication (TypeConstructor typeName) payload
          providerScheme = ForallType ["element"] []
            $ FunctionType modeled modeled
          provider = providerSource providerName providerScheme
            [Length.LengthSpineArgument]
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
          declarations recursiveFirst =
            [ listLikeDeclaration
                typeName zeroName stepName recursiveFirst
            , ValueDeclaration
                $ ValueSignature () providerName providerScheme
            ]
          source = Length.DeclaredListSpine typeName zeroName stepName
          target = FunctionType
            (TypeApplication (TypeConstructor typeName) closedPayloadType)
            (TypeApplication (TypeConstructor typeName) closedPayloadType)
      payloadFirstContext <- expectRight $ Length.sealLengthContext
        Length.defaultLengthLimits
        (fixtureInventoryFromDeclarations $ declarations False) source
      recursiveFirstContext <- expectRight $ Length.sealLengthContext
        Length.defaultLengthLimits
        (fixtureInventoryFromDeclarations $ declarations True) source
      payloadFirstContract <- expectRight $ Length.sealLengthContractInContext
        Length.defaultLengthLimits payloadFirstContext target identityLengthContract
      recursiveFirstContract <- expectRight $ Length.sealLengthContractInContext
        Length.defaultLengthLimits recursiveFirstContext target identityLengthContract
      payloadFirstProviders <- expectRight
        $ Length.sealLengthProviderInventoryInContext
            Length.defaultLengthLimits payloadFirstContext [provider]
      recursiveFirstProviders <- expectRight
        $ Length.sealLengthProviderInventoryInContext
            Length.defaultLengthLimits recursiveFirstContext [provider]
      assertBool "spine-field order was omitted from contract identity" $
        Length.lengthContractFingerprint payloadFirstContract /=
          Length.lengthContractFingerprint recursiveFirstContract
      assertBool "spine-field order was omitted from provider identity" $
        Length.lengthProviderInventoryFingerprint payloadFirstProviders /=
          Length.lengthProviderInventoryFingerprint recursiveFirstProviders
  ]

negativeLimitCases
  :: [(String, Length.LengthLimitField, Length.LengthLimitSource)]
negativeLimitCases =
  [ ("type nodes", Length.LengthTypeNodes, defaults
      { Length.lengthLimitSourceTypeNodes = -1 })
  , ("contract inputs", Length.LengthContractInputs, defaults
      { Length.lengthLimitSourceContractInputs = -1 })
  , ("syntax nodes", Length.LengthSyntaxNodes, defaults
      { Length.lengthLimitSourceSyntaxNodes = -1 })
  , ("formula clauses", Length.LengthFormulaClauses, defaults
      { Length.lengthLimitSourceFormulaClauses = -1 })
  , ("collection width", Length.LengthCollectionWidth, defaults
      { Length.lengthLimitSourceCollectionWidth = -1 })
  , ("provider summaries", Length.LengthProviderSummaries, defaults
      { Length.lengthLimitSourceProviderSummaries = -1 })
  , ("provider arguments", Length.LengthProviderArguments, defaults
      { Length.lengthLimitSourceProviderArguments = -1 })
  , ("literal bits", Length.LengthLiteralBits, defaults
      { Length.lengthLimitSourceLiteralBits = -1 })
  , ("fingerprint bytes", Length.LengthFingerprintBytes, defaults
      { Length.lengthLimitSourceFingerprintBytes = -1 })
  ]
 where
  defaults = Length.defaultLengthLimitSource

assertNegativeLimit
  :: (String, Length.LengthLimitField, Length.LengthLimitSource)
  -> IO ()
assertNegativeLimit (label, field, source) =
  case Length.mkLengthLimits source of
    Left failure -> failure @?= Length.NegativeLengthLimit field (-1)
    Right _ -> assertFailure $ "negative " ++ label ++ " limit was accepted"

zeroLengthLimitSource :: Length.LengthLimitSource
zeroLengthLimitSource = Length.LengthLimitSource
  { Length.lengthLimitSourceTypeNodes = 0
  , Length.lengthLimitSourceContractInputs = 0
  , Length.lengthLimitSourceSyntaxNodes = 0
  , Length.lengthLimitSourceFormulaClauses = 0
  , Length.lengthLimitSourceCollectionWidth = 0
  , Length.lengthLimitSourceProviderSummaries = 0
  , Length.lengthLimitSourceProviderArguments = 0
  , Length.lengthLimitSourceLiteralBits = 0
  , Length.lengthLimitSourceFingerprintBytes = 0
  }

limitsWith
  :: (Length.LengthLimitSource -> Length.LengthLimitSource)
  -> Length.LengthLimits
limitsWith transform = case Length.mkLengthLimits
    $ transform Length.defaultLengthLimitSource of
  Left failure -> error $ "invalid test limits: " ++ show failure
  Right limits -> limits

closedPayloadType :: Type String
closedPayloadType = TupleType Boxed []

polymorphicIdentityType :: Type String
polymorphicIdentityType = ForallType ["element"] [] $ FunctionType
  (TypeVariable "element") (TypeVariable "element")

listOf :: Type variable -> Type variable
listOf = TypeApplication $ TypeConstructor listName

identityLengthContract :: Length.LengthContractSource
identityLengthContract = contractWith
  (Length.LengthTruth True)
  (Length.LengthEqual
    (Length.LengthVariable Length.LengthResult)
    (Length.LengthVariable $ Length.LengthInput 0))

trivialLengthContract :: Length.LengthContractSource
trivialLengthContract = contractWith
  (Length.LengthTruth True) (Length.LengthTruth True)

contractWith
  :: Length.LengthFormula Length.LengthContractVariable
  -> Length.LengthFormula Length.LengthContractVariable
  -> Length.LengthContractSource
contractWith precondition postcondition = Length.LengthContractSource
  { Length.lengthContractPrecondition = precondition
  , Length.lengthContractPostcondition = postcondition
  }

fixtureInventory :: Inventory String ()
fixtureInventory = fixtureInventoryFromDeclarations []

fixtureInventoryFromDeclarations
  :: [Declaration String () ()]
  -> Inventory String ()
fixtureInventoryFromDeclarations declarations = case mkInventory
    ClosedKindInventory declarations of
  Left failure -> error $ "invalid fixture inventory: " ++ show failure
  Right inventory -> inventory

providerFixtureInventory
  :: [Length.LengthProviderSummarySource String]
  -> Inventory String ()
providerFixtureInventory = fixtureInventoryFromDeclarations
  . uniqueProviderDeclarations []
 where
  uniqueProviderDeclarations _ [] = []
  uniqueProviderDeclarations seen (source : remaining)
    | providerName `elem` seen =
        uniqueProviderDeclarations seen remaining
    | otherwise =
        ValueDeclaration
          (ValueSignature () providerName $ Length.lengthProviderScheme source)
          : uniqueProviderDeclarations (providerName : seen) remaining
   where
    providerName = Length.lengthProviderName source

listLikeDeclaration
  :: Name
  -> Name
  -> Name
  -> Bool
  -> Declaration String () ()
listLikeDeclaration typeName zeroName stepName recursiveFirst =
  DataTypeDeclaration () typeName [TypeParameter "element" Nothing]
    [ DataConstructor () zeroName []
    , DataConstructor () stepName fields
    ]
 where
  payload = TypeVariable "element"
  recursive = TypeApplication (TypeConstructor typeName) payload
  fields
    | recursiveFirst = [recursive, payload]
    | otherwise = [payload, recursive]

sessionInventory
  :: annotation
  -> [Declaration (Variable String) () annotation]
  -> Inventory (Variable String) annotation
sessionInventory _ declarations = case mkInventory
    ClosedKindInventory declarations of
  Left failure -> error $ "invalid session fixture inventory: " ++ show failure
  Right inventory -> inventory

sessionProviderDeclaration
  :: annotation
  -> Length.LengthProviderSummarySource (Variable String)
  -> Declaration (Variable String) () annotation
sessionProviderDeclaration annotation source = ValueDeclaration
  $ ValueSignature annotation
      (Length.lengthProviderName source)
      (Length.lengthProviderScheme source)

sessionUnaryProvider
  :: Name
  -> String
  -> Length.LengthProviderSummarySource (Variable String)
sessionUnaryProvider providerName identity = Length.AssumedProviderSummary
  { Length.lengthProviderName = providerName
  , Length.lengthProviderScheme = ForallType [variable] []
      $ FunctionType
          (sessionListOf $ TypeVariable variable)
          (sessionListOf $ TypeVariable variable)
  , Length.lengthProviderArgumentRoles = [Length.LengthSpineArgument]
  , Length.lengthProviderTransfer = Length.LengthVariable
      $ Length.LengthProviderArgument 0
  }
 where
  variable = FlexibleVariable identity

sessionPayloadType :: Type (Variable String)
sessionPayloadType = TupleType Boxed []

sessionListOf
  :: Type (Variable String)
  -> Type (Variable String)
sessionListOf = TypeApplication $ TypeConstructor listName

type AdversarialIdentity = String
type AdversarialLocal = Int
type AdversarialType = Type (Variable AdversarialIdentity)
type AdversarialGraph = Djex.TermGraph AdversarialType AdversarialLocal
type AdversarialCandidate = Djex.TypedCandidate
  String
  AdversarialType
  AdversarialLocal
  (Djex.Candidate AdversarialType () ())

adversarialLengthSession
  :: [Declaration (Variable AdversarialIdentity) () ()]
  -> [Length.LengthProviderSummarySource
        (Variable AdversarialIdentity)]
  -> IO (LengthProblem.CheckedLengthSession AdversarialIdentity ())
adversarialLengthSession declarations providers = expectRight
  $ LengthProblem.sealLengthSession
      Length.defaultLengthLimits
      (sessionInventory () declarations)
      Length.BuiltinListSpine
      providers

adversarialLengthContract
  :: LengthProblem.CheckedLengthSession AdversarialIdentity ()
  -> AdversarialType
  -> Length.LengthContractSource
  -> IO (Length.CheckedLengthContract
      (Variable AdversarialIdentity))
adversarialLengthContract session target source = expectRight
  $ Length.sealLengthContractInContext
      Length.defaultLengthLimits
      (LengthProblem.checkedLengthSessionContext session)
      target
      source

adversarialListOf :: AdversarialType -> AdversarialType
adversarialListOf = TypeApplication $ TypeConstructor listName

adversarialClosedList :: AdversarialType
adversarialClosedList = adversarialListOf $ TupleType Boxed []

adversarialVisibleProviderProblem
  :: Name
  -> AdversarialType
  -> AdversarialType
  -> AdversarialType
  -> Maybe (Djex.CertificateId, Natural)
  -> IO
      (Either
        (LengthProblem.LengthProblemError
          String AdversarialIdentity AdversarialLocal)
        (LengthProblem.CheckedLengthProblem
          AdversarialIdentity AdversarialLocal))
adversarialVisibleProviderProblem providerName providerScheme selected result
    certificate = do
  let provider = Length.AssumedProviderSummary
        { Length.lengthProviderName = providerName
        , Length.lengthProviderScheme = providerScheme
        , Length.lengthProviderArgumentRoles = []
        , Length.lengthProviderTransfer = Length.LengthLiteral 0
        }
      declaration = ValueDeclaration
        $ ValueSignature () providerName providerScheme
      witness = Djex.TypeApplicationWitness
        providerScheme selected result certificate
      source = Djex.TermGraphSource (Djex.termNodeId 1)
        [ ( Djex.termNodeId 0
          , Djex.TermNode providerScheme
              $ Djex.TypedGlobal (Djex.occurrenceId 0) providerName
          )
        , ( Djex.termNodeId 1
          , Djex.TermNode result
              $ Djex.TypedVisibleTypeApplication
                  (Djex.occurrenceId 1)
                  (Djex.termNodeId 0)
                  Djex.inferredVisibleTypeArgument
                  witness
          )
        ]
  session <- adversarialLengthSession [declaration] [provider]
  contract <- adversarialLengthContract session result trivialLengthContract
  graph <- sealAdversarialGraph source
  pure $ LengthProblem.sealLengthTypedCandidateProblem
    LengthProblem.defaultLengthProblemLimits session contract
    $ adversarialTypedCandidate $ Right graph

adversarialIdentityGraphSource
  :: AdversarialType
  -> Djex.TermGraphSource AdversarialType AdversarialLocal
adversarialIdentityGraphSource spine = Djex.TermGraphSource
  (Djex.termNodeId 1)
  [ ( Djex.termNodeId 0
    , Djex.TermNode spine
        $ Djex.TypedLocal (Djex.occurrenceId 1) 0
    )
  , ( Djex.termNodeId 1
    , Djex.TermNode (FunctionType spine spine)
        $ Djex.TypedLambda
            [ Djex.TypedPattern
                (Djex.occurrenceId 0) spine (Djex.TypedBind 0)
            ]
            (Djex.termNodeId 0)
    )
  ]

adversarialConstantZeroProblem
  :: Length.LengthContractSource
  -> IO
      (LengthProblem.CheckedLengthProblem
        AdversarialIdentity AdversarialLocal)
adversarialConstantZeroProblem contractSource = do
  session <- adversarialLengthSession [] []
  let target = FunctionType adversarialClosedList adversarialClosedList
      source = Djex.TermGraphSource (Djex.termNodeId 1)
        [ ( Djex.termNodeId 0
          , Djex.TermNode adversarialClosedList
              $ Djex.TypedGlobal (Djex.occurrenceId 1) listName
          )
        , ( Djex.termNodeId 1
          , Djex.TermNode target
              $ Djex.TypedLambda
                  [ Djex.TypedPattern
                      (Djex.occurrenceId 0)
                      adversarialClosedList
                      Djex.TypedWildcard
                  ]
                  (Djex.termNodeId 0)
          )
        ]
  contract <- adversarialLengthContract session target contractSource
  graph <- sealAdversarialGraph source
  expectRight $ LengthProblem.sealLengthTypedCandidateProblem
    LengthProblem.defaultLengthProblemLimits session contract
    $ adversarialTypedCandidate $ Right graph

adversarialZeroInputProblem
  :: Length.LengthContractSource
  -> IO
      (LengthProblem.CheckedLengthProblem
        AdversarialIdentity AdversarialLocal)
adversarialZeroInputProblem contractSource = do
  session <- adversarialLengthSession [] []
  let source = Djex.TermGraphSource (Djex.termNodeId 0)
        [ ( Djex.termNodeId 0
          , Djex.TermNode adversarialClosedList
              $ Djex.TypedGlobal (Djex.occurrenceId 0) listName
          )
        ]
  contract <- adversarialLengthContract
    session adversarialClosedList contractSource
  graph <- sealAdversarialGraph source
  expectRight $ LengthProblem.sealLengthTypedCandidateProblem
    LengthProblem.defaultLengthProblemLimits session contract
    $ adversarialTypedCandidate $ Right graph

adversarialBinaryConstantZeroProblem
  :: Length.LengthContractSource
  -> IO
      (LengthProblem.CheckedLengthProblem
        AdversarialIdentity AdversarialLocal)
adversarialBinaryConstantZeroProblem contractSource = do
  session <- adversarialLengthSession [] []
  let target = FunctionType adversarialClosedList
        $ FunctionType adversarialClosedList adversarialClosedList
      source = Djex.TermGraphSource (Djex.termNodeId 1)
        [ ( Djex.termNodeId 0
          , Djex.TermNode adversarialClosedList
              $ Djex.TypedGlobal (Djex.occurrenceId 2) listName
          )
        , ( Djex.termNodeId 1
          , Djex.TermNode target
              $ Djex.TypedLambda
                  [ Djex.TypedPattern
                      (Djex.occurrenceId 0)
                      adversarialClosedList
                      Djex.TypedWildcard
                  , Djex.TypedPattern
                      (Djex.occurrenceId 1)
                      adversarialClosedList
                      Djex.TypedWildcard
                  ]
                  (Djex.termNodeId 0)
          )
        ]
  contract <- adversarialLengthContract session target contractSource
  graph <- sealAdversarialGraph source
  expectRight $ LengthProblem.sealLengthTypedCandidateProblem
    LengthProblem.defaultLengthProblemLimits session contract
    $ adversarialTypedCandidate $ Right graph

adversarialConstantProviderProblem
  :: Natural
  -> Length.LengthContractSource
  -> IO
      (LengthProblem.CheckedLengthProblem
        AdversarialIdentity AdversarialLocal)
adversarialConstantProviderProblem result contractSource = do
  providerName <- expectName "Fixture.problemReplayConstant"
  let target = FunctionType adversarialClosedList adversarialClosedList
      provider = Length.AssumedProviderSummary
        { Length.lengthProviderName = providerName
        , Length.lengthProviderScheme = adversarialClosedList
        , Length.lengthProviderArgumentRoles = []
        , Length.lengthProviderTransfer = Length.LengthLiteral result
        }
      declaration = ValueDeclaration
        $ ValueSignature () providerName adversarialClosedList
      source = Djex.TermGraphSource (Djex.termNodeId 1)
        [ ( Djex.termNodeId 0
          , Djex.TermNode adversarialClosedList
              $ Djex.TypedGlobal (Djex.occurrenceId 1) providerName
          )
        , ( Djex.termNodeId 1
          , Djex.TermNode target
              $ Djex.TypedLambda
                  [ Djex.TypedPattern
                      (Djex.occurrenceId 0)
                      adversarialClosedList
                      Djex.TypedWildcard
                  ]
                  (Djex.termNodeId 0)
          )
        ]
  session <- adversarialLengthSession [declaration] [provider]
  contract <- adversarialLengthContract session target contractSource
  graph <- sealAdversarialGraph source
  expectRight $ LengthProblem.sealLengthTypedCandidateProblem
    LengthProblem.defaultLengthProblemLimits session contract
    $ adversarialTypedCandidate $ Right graph

adversarialScaledProviderProblem
  :: Natural
  -> Length.LengthContractSource
  -> IO
      (LengthProblem.CheckedLengthProblem
        AdversarialIdentity AdversarialLocal)
adversarialScaledProviderProblem factor contractSource = do
  providerName <- expectName "Fixture.problemReplayScale"
  let target = FunctionType adversarialClosedList adversarialClosedList
      provider = Length.AssumedProviderSummary
        { Length.lengthProviderName = providerName
        , Length.lengthProviderScheme = target
        , Length.lengthProviderArgumentRoles = [Length.LengthSpineArgument]
        , Length.lengthProviderTransfer = Length.LengthScale factor
            $ Length.LengthVariable $ Length.LengthProviderArgument 0
        }
      declaration = ValueDeclaration
        $ ValueSignature () providerName target
      source = Djex.TermGraphSource (Djex.termNodeId 3)
        [ ( Djex.termNodeId 0
          , Djex.TermNode target
              $ Djex.TypedGlobal (Djex.occurrenceId 0) providerName
          )
        , ( Djex.termNodeId 1
          , Djex.TermNode adversarialClosedList
              $ Djex.TypedLocal (Djex.occurrenceId 1) 0
          )
        , ( Djex.termNodeId 2
          , Djex.TermNode adversarialClosedList
              $ Djex.TypedApply
                  (Djex.termNodeId 0)
                  (Djex.termNodeId 1)
                  (Djex.ApplicationWitness
                    adversarialClosedList adversarialClosedList)
          )
        , ( Djex.termNodeId 3
          , Djex.TermNode target
              $ Djex.TypedLambda
                  [ Djex.TypedPattern
                      (Djex.occurrenceId 2)
                      adversarialClosedList
                      (Djex.TypedBind 0)
                  ]
                  (Djex.termNodeId 2)
          )
        ]
  session <- adversarialLengthSession [declaration] [provider]
  contract <- adversarialLengthContract session target contractSource
  graph <- sealAdversarialGraph source
  expectRight $ LengthProblem.sealLengthTypedCandidateProblem
    LengthProblem.defaultLengthProblemLimits session contract
    $ adversarialTypedCandidate $ Right graph

adversarialLetIdentityGraphSource
  :: AdversarialType
  -> Djex.TermGraphSource AdversarialType AdversarialLocal
adversarialLetIdentityGraphSource spine = Djex.TermGraphSource
  (Djex.termNodeId 3)
  [ ( Djex.termNodeId 0
    , Djex.TermNode spine
        $ Djex.TypedLocal (Djex.occurrenceId 1) 0
    )
  , ( Djex.termNodeId 1
    , Djex.TermNode spine
        $ Djex.TypedLocal (Djex.occurrenceId 3) 0
    )
  , ( Djex.termNodeId 2
    , Djex.TermNode spine
        $ Djex.TypedLet
            (Djex.TypedPattern
              (Djex.occurrenceId 2) spine Djex.TypedWildcard)
            (Djex.termNodeId 0)
            (Djex.termNodeId 1)
    )
  , ( Djex.termNodeId 3
    , Djex.TermNode (FunctionType spine spine)
        $ Djex.TypedLambda
            [ Djex.TypedPattern
                (Djex.occurrenceId 0) spine (Djex.TypedBind 0)
            ]
            (Djex.termNodeId 2)
    )
  ]

sealAdversarialGraph
  :: Djex.TermGraphSource AdversarialType AdversarialLocal
  -> IO AdversarialGraph
sealAdversarialGraph = expectRight
  . Djex.sealTermGraph Djex.sharedTypeStructure Djex.defaultTermGraphLimits

-- IMPORTANT: this coercion is confined to the adversarial same-package test
-- component.
-- Cabal compiles the exact private representation as a home module, whose unit
-- identity is deliberately distinct from the library's opaque public type even
-- though both definitions and all field types are identical.  Production code
-- never receives this constructor or coercion; downstream opacity remains
-- covered independently by the negative API tests.
adversarialTypedCandidate
  :: Either String AdversarialGraph
  -> AdversarialCandidate
adversarialTypedCandidate graph = unsafeCoerce
  $ InternalTypedCandidate.mkTypedCandidate adversarialCompatibility graph

adversarialCompatibility :: Djex.Candidate AdversarialType () ()
adversarialCompatibility = Djex.Candidate
  { Djex.candidateOutput = ()
  , Djex.candidateResidualConstraints = []
  , Djex.candidateDetails = ()
  }

realListIdentityFixture
  :: Type (Variable Int)
  -> IO
      ( LengthProblem.CheckedLengthSession Int ()
      , Length.CheckedLengthContract (Variable Int)
      , Djex.ExferenceTypedCandidate
      )
realListIdentityFixture payload = do
  environment <- expectRight
    (Djex.mkEnvironment [] ::
      Either (Djex.EnvironmentError Djex.ExferenceTypeVariable)
        Djex.ExferenceEnvironment)
  exferenceSession <- expectRight $ Djex.mkExferenceSession environment
  lengthSession <- expectRight $ LengthProblem.sealLengthSession
    Length.defaultLengthLimits
    (Djex.exferenceSessionInventory exferenceSession)
    Length.BuiltinListSpine []
  targetName <- expectName "lengthIdentityCandidate"
  target <- expectRight $ Djex.mkDefinitionName targetName
  let spine = TypeApplication (TypeConstructor listName) payload
      goal = FunctionType spine spine
      query = Djex.QueryRequest
        { Djex.requestTarget = target
        , Djex.requestGoal = goal
        , Djex.requestContexts = []
        , Djex.requestOptions = Djex.defaultExferenceOptions
            { Djex.exferenceMaximumSteps = 8 }
        }
  request <- expectRight $ Djex.mkExferenceRequest query
  contract <- expectRight $ Length.sealLengthContractInContext
    Length.defaultLengthLimits
    (LengthProblem.checkedLengthSessionContext lengthSession)
    goal identityLengthContract
  results <- expectRight $ Djex.runExferenceTypedQuery exferenceSession request
  candidate <- case
      [ value
      | result <- results
      , value <- Djex.batchCandidates $ Djex.resultSearch result
      ] of
    [] -> assertFailure "the list identity query returned no candidate"
    value : _ -> pure value
  pure (lengthSession, contract, candidate)

isNamedGlobal :: Name -> Djex.TermNode ty local -> Bool
isNamedGlobal expected (Djex.TermNode _ form) = case form of
  Djex.TypedGlobal _ actual -> actual == expected
  _ -> False

sealContract
  :: Length.LengthLimits
  -> Type String
  -> Length.LengthContractSource
  -> Either
      (Length.LengthContractError String)
      (Length.CheckedLengthContract String)
sealContract limits = Length.sealLengthContract limits fixtureInventory

sealProviderInventory
  :: Length.LengthLimits
  -> [Length.LengthProviderSummarySource String]
  -> Either
      (Length.LengthProviderInventoryError String)
      (Length.CheckedLengthProviderInventory String)
sealProviderInventory limits =
  \sources -> Length.sealLengthProviderInventory
    limits (providerFixtureInventory sources) sources

providerSource
  :: Name
  -> Type String
  -> [Length.LengthProviderArgumentRole]
  -> Length.LengthExpression Length.LengthProviderVariable
  -> Length.LengthProviderSummarySource String
providerSource providerName scheme roles transfer =
  Length.AssumedProviderSummary
    { Length.lengthProviderName = providerName
    , Length.lengthProviderScheme = scheme
    , Length.lengthProviderArgumentRoles = roles
    , Length.lengthProviderTransfer = transfer
    }

unaryListProvider
  :: Name
  -> Length.LengthProviderArgumentRole
  -> Length.LengthExpression Length.LengthProviderVariable
  -> Length.LengthProviderSummarySource String
unaryListProvider providerName role transfer = providerSource providerName
  (ForallType ["element"] [] $ FunctionType
    (listOf $ TypeVariable "element")
    (listOf $ TypeVariable "element"))
  [role]
  transfer

expectCheckedProvider
  :: Length.LengthProviderSummarySource String
  -> IO (Length.CheckedLengthProviderSummary String)
expectCheckedProvider source = do
  inventory <- expectRight $ sealProviderInventory
    Length.defaultLengthLimits [source]
  case Length.lookupCheckedLengthProviderSummary
      (Length.lengthProviderName source) inventory of
    Nothing -> assertFailure "checked provider disappeared from its inventory"
    Just summary -> pure summary

evaluationLimitsWith :: Int -> Int -> Evaluate.LengthEvaluationLimits
evaluationLimitsWith assignmentBits intermediateBits = case
    Evaluate.mkLengthEvaluationLimits Evaluate.LengthEvaluationLimitSource
      { Evaluate.lengthEvaluationLimitSourceAssignmentValueBits = assignmentBits
      , Evaluate.lengthEvaluationLimitSourceIntermediateValueBits =
          intermediateBits
      } of
  Left failure -> error $ "invalid evaluation test limits: " ++ show failure
  Right limits -> limits

asciiBytes :: String -> [Word8]
asciiBytes = map $ fromIntegral . fromEnum

absoluteFixtureExecutable :: FilePath
absoluteFixtureExecutable
  | SystemInfo.os == "mingw32" = "C:\\djex\\z3.exe"
  | otherwise = "/usr/bin/z3"

absoluteFixturePrefix :: FilePath
absoluteFixturePrefix
  | SystemInfo.os == "mingw32" = "C:\\"
  | otherwise = "/z3"

smtIntegerBinding
  :: [Word8]
  -> Integer
  -> SMTLib.LengthSMTLibIntegerBinding
smtIntegerBinding symbol value = SMTLib.LengthSMTLibIntegerBinding
  { SMTLib.lengthSMTLibIntegerBindingSymbol = symbol
  , SMTLib.lengthSMTLibIntegerBindingValue = value
  }

smtRawArtifact
  :: [Word8]
  -> [Word8]
  -> IO (SemanticProblem.BoundedRawArtifact ())
smtRawArtifact format bytes = expectRight
  $ SemanticProblem.mkBoundedRawArtifact
      SemanticProblem.defaultRawArtifactLimits format bytes

expectName :: String -> IO Name
expectName = expectRight . parseName

expectRight :: Show error => Either error value -> IO value
expectRight result = case result of
  Left failure -> assertFailure $ "unexpected rejection: " ++ show failure
  Right value -> pure value

expectNoCounterexample
  :: Show error
  => Either error (Maybe evidence)
  -> IO ()
expectNoCounterexample result = case result of
  Left failure -> assertFailure $ "unexpected replay rejection: " ++ show failure
  Right Nothing -> pure ()
  Right Just{} -> assertFailure "an assignment unexpectedly refuted the problem"

expectCounterexample
  :: Show error
  => Either error (Maybe evidence)
  -> IO evidence
expectCounterexample result = case result of
  Left failure -> assertFailure $ "unexpected replay rejection: " ++ show failure
  Right Nothing -> assertFailure "the violating assignment produced no evidence"
  Right (Just evidence) -> pure evidence

assertLeft
  :: (Eq error, Show error)
  => error
  -> Either error value
  -> IO ()
assertLeft expected result = case result of
  Left failure -> failure @?= expected
  Right _ -> assertFailure $ "expected rejection: " ++ show expected

evaluateWithin :: value -> IO value
evaluateWithin value = do
  observed <- timeout 2000000 $ evaluate value
  case observed of
    Nothing -> fail "bounded validation did not terminate within two seconds"
    Just result -> pure result
