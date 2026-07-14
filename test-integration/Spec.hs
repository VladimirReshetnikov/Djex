module Main (main) where

import Data.Either (isRight)
import Data.List (isInfixOf, nub)
import qualified Data.Map.Strict as Map
import Djinn.Core
  ( Context
  , Declaration (ClassDecl, DataType, Function, TypeSynonym)
  , HType
  , declare
  , emptyEnvironment
  , generatedReportCandidates
  , generatedReportCompletion
  , generatedReportEvidence
  , generatedReportFormula
  , generatedReportProof
  , inhabitGenerated
  , mkContext
  , parseHType
  , standardEnvironment
  , toSynthesisEnvironment
  , toSynthesisType
  )
import qualified Djinn.Core as DjinnCore (DjinnCandidate, Environment)
-- Raw Exference fixtures below use their historical @functionName@ field;
-- hide the shared structural-name accessor at this integration-only seam.
import Language.Haskell.Djex hiding (functionName)
import Language.Haskell.Djex.Exference.HaskellSrc
  ( parseExferenceRequest
  , parseExferenceRequestWithCheckedTarget
  )
import qualified Language.Haskell.Exference.Session as ExferenceCompatibility
import Language.Haskell.Exference.Core.FunctionBinding (FunctionBinding (..))
import Language.Haskell.Exference.Core.Name (mkQualifiedName)
import Language.Haskell.Exference.Core.Types
  ( HsType (TypeForall, TypeVar)
  , HsTypeClass (HsTypeClass)
  , emptyStaticClassEnv
  , mkStaticClassEnv
  )
import Language.Haskell.Exference.EnvironmentParser
  ( SourceBinding (SourceFunction)
  , SourceEnvironment (..)
  , checkSourceEnvironment
  , checkedSourceInventory
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Djex facade"
  [ testCase "enumerate backends in stable presentation order" $
      map backend availableBackends @?= [DjinnBackend, ExferenceBackend]
  , testCase "backend metadata is the canonical projection" $
      availableBackends @?= map backendInfo [DjinnBackend, ExferenceBackend]
  , testCase "advertise only the checked backend capabilities" $ do
      backendCapabilities (backendInfo DjinnBackend) @?=
        [DecidingInhabitation, TypeClassConstraints]
      backendCapabilities (backendInfo ExferenceBackend) @?=
        [ HeuristicSearch
        , PrenexPolymorphism
        , RankedCandidates
        , TypeClassConstraints
        ]
  , testCase "do not duplicate capabilities" $
      map backendCapabilities availableBackends @?=
        map (nub . backendCapabilities) availableBackends
  , testCase "stable backend APIs are reachable through a djex dependency" $ do
      assertBool "Djinn API was not reexported" $ isRight $ parseHType "a -> a"
      assertBool "Exference API was not reexported" $
        isRight $ mkQualifiedName [] "id"
      assertBool "synthesis API was not reexported" $ isRight $ parseName "id"
  , testCase "run a checked Djinn session through the shared envelope" $ do
      session <- sealDjinnEnvironment standardEnvironment
      target <- expectRight $ mkIdentifier "swap"
      goal <- expectRight $ parseHType "(a, b) -> (b, a)"
      request <- sharedDjinnRequest target [] defaultQueryOptions goal
      result <- expectRight $ runDjinnQuery session request
      resultEvidence result @?= ValidatedCandidates
      batchProgress (resultSearch result) @?= Completed Finished
      assertBool "translated formula metadata was lost" $
        not $ null $ djinnTranslatedFormula $ batchMetadata $ resultSearch result
      assertBool "first proof metadata was lost" $
        case djinnFirstExploredProof $ batchMetadata $ resultSearch result of
          Just _ -> True
          Nothing -> False
      case batchCandidates $ resultSearch result of
        candidate : _ -> do
          candidateResidualConstraints candidate @?= []
          clauseName (candidateOutput candidate) @?=
            requestTarget (djinnRequestQuery request)
          definitionName (clauseName $ candidateOutput candidate) @?= target
          djinnUnusedBinderFraction (candidateDetails candidate) @?= 0
          djinnBinderCount (candidateDetails candidate) @?= 2
          renderFunctionClause
              (defaultRenderOptions id) (candidateOutput candidate) @?=
            Right "swap (a, b) = (b, a)"
        [] -> fail "Djinn reported candidate evidence without a candidate"
  , testCase "preserve Djinn candidate-limit truncation" $ do
      first <- expectRight $ parseHType "a"
      second <- expectRight $ parseHType "b"
      goal <- expectRight $ parseHType "T a b -> (a, b)"
      environment <- expectRight $ declare
        (DataType "T" ["a", "b"] [("C", [first, second])])
        emptyEnvironment
      session <- sealDjinnEnvironment environment
      target <- expectRight $ mkIdentifier "pair"
      request <- sharedDjinnRequest target []
        defaultQueryOptions
          { optionAlternatives = True
          , optionSorted = False
          , optionCutoff = 1
          }
        goal
      result <- expectRight $ runDjinnQuery session request
      resultEvidence result @?= ValidatedCandidates
      length (batchCandidates $ resultSearch result) @?= 1
      case batchProgress $ resultSearch result of
        Completed (Truncated reasons) ->
          assertBool "candidate-limit truncation reason was lost" $
            CandidateLimitReached `elem` reasons
        completion -> fail $ "expected candidate truncation, got "
          ++ show completion
  , testCase "keep Djinn evidence independent of search completion" $ do
      session <- sealDjinnEnvironment standardEnvironment
      target <- expectRight $ mkIdentifier "peirce"
      goal <- expectRight $ parseHType "((a -> b) -> a) -> a"
      refutationRequest <- sharedDjinnRequest
        target [] defaultQueryOptions goal
      refutation <- expectRight
        $ runDjinnQuery session refutationRequest
      resultEvidence refutation @?= ProvedUninhabitable
      batchProgress (resultSearch refutation) @?= Completed Finished
      undecidedRequest <- sharedDjinnRequest target []
        defaultQueryOptions { optionBudget = Just 0 } goal
      undecided <- expectRight $ runDjinnQuery session undecidedRequest
      resultEvidence undecided @?= NoEvidence
      case batchProgress $ resultSearch undecided of
        Completed (Truncated reasons) ->
          assertBool "choice-point truncation reason was lost" $
            ChoicePointLimitReached `elem` reasons
        completion -> fail $ "expected a truncated search, got " ++ show completion
  , testCase "attach parsed Djinn sources only to deferred diagnostics" $ do
      session <- sealDjinnEnvironment standardEnvironment
      target <- expectRight $ mkIdentifier "deferredFailure"
      let sourceName = "deferred-synonym.djinn"
          source = "Not"
      parsed <- expectRight $ parseDjinnRequest session
        defaultQueryOptions target sourceName source
      case runDjinnQuery session parsed of
        Left failure -> do
          diagnosticCode failure @?= Just "DJEX_DJINN_QUERY"
          diagnosticSource failure @?= Just sourceName
          diagnosticSpan failure @?= Just (sourceTextSpan source)
        Right _ -> fail "Djinn accepted an unsaturated parsed synonym"
      programmatic <- expectRight $ mkDjinnRequest $ djinnRequestQuery parsed
      parsed @?= programmatic
      show parsed @?= show programmatic
      case runDjinnQuery session programmatic of
        Left failure -> do
          diagnosticCode failure @?= Just "DJEX_DJINN_QUERY"
          diagnosticSource failure @?= Nothing
          diagnosticSpan failure @?= Nothing
        Right _ -> fail "Djinn accepted an unsaturated programmatic synonym"
  , testCase "preserve Djinn's target-reference evidence" $ do
      variable <- expectRight $ parseHType "a"
      environment <- expectRight $
        declare (Function "token" variable) standardEnvironment
      session <- sealDjinnEnvironment environment
      target <- expectRight $ mkIdentifier "token"
      request <- sharedDjinnRequest target [] defaultQueryOptions variable
      result <- expectRight $ runDjinnQuery session request
      resultEvidence result @?= RequiresTargetReference
      batchCandidates (resultSearch result) @?= []
  , testCase "reuse sealed Djinn kinds without changing query semantics" $ do
      standardSession <- sealDjinnEnvironment standardEnvironment

      ordinary <- expectRight $ parseHType "(a, b) -> (b, a)"
      assertDjinnCompatibility "ordinary goal" standardEnvironment
        standardSession [] defaultQueryOptions "swapPrepared" ordinary

      monadArgument <- expectRight $ parseHType "m"
      monadContext <- expectRight $ mkContext "Monad" [monadArgument]
      constrained <- expectRight $ parseHType "a -> m a"
      assertDjinnCompatibility "explicit class context" standardEnvironment
        standardSession [monadContext] defaultQueryOptions
        "returnPrepared" constrained

      refutation <- expectRight $ parseHType "((a -> b) -> a) -> a"
      assertDjinnCompatibility "completed refutation" standardEnvironment
        standardSession [] defaultQueryOptions "peircePrepared" refutation
      assertDjinnCompatibility "budget completion and evidence"
        standardEnvironment standardSession []
        (defaultQueryOptions { optionBudget = Just 0 })
        "peircePrepared" refutation

      captureMethod <- expectRight $ parseHType "f a"
      captureEnvironment <- expectRight $ declare
        (ClassDecl "CapturePrepared" ["a"]
          [("capturePrepared", captureMethod)]) standardEnvironment
      captureSession <- sealDjinnEnvironment captureEnvironment
      captureArgument <- expectRight $ parseHType "f"
      captureContext <- expectRight $
        mkContext "CapturePrepared" [captureArgument]
      captureGoal <- expectRight $ parseHType "x -> x"
      assertDjinnCompatibility "capture-safe instantiated method"
        captureEnvironment captureSession [captureContext]
        defaultQueryOptions "captureSafePrepared" captureGoal

      higherMethod <- expectRight $ parseHType "f a -> f a"
      higherEnvironment <- expectRight $ declare
        (ClassDecl "HigherPrepared" ["f"]
          [("higherPrepared", higherMethod)]) standardEnvironment
      higherSession <- sealDjinnEnvironment higherEnvironment
      unsaturatedSynonym <- expectRight $ parseHType "Not"
      higherContext <- expectRight $
        mkContext "HigherPrepared" [unsaturatedSynonym]
      higherTarget <- expectRight $ mkIdentifier "higherGoalPrepared"
      higherRequest <- sharedDjinnRequest higherTarget [higherContext]
        defaultQueryOptions captureGoal
      case ( inhabitGenerated defaultQueryOptions higherEnvironment
               [higherContext] "higherGoalPrepared" captureGoal
           , runDjinnQuery higherSession higherRequest
           ) of
        (Left compatibilityFailure, Left sessionFailure) -> do
          assertBool "prepared path skipped Djinn synonym saturation" $
            "Type synonym Not expects at least 1 argument(s), but got 0"
              `isInfixOf` compatibilityFailure
          diagnosticContext sessionFailure @?= [compatibilityFailure]
        (compatibilityResult, sessionResult) -> fail $
          "synonym-saturation paths diverged: " ++ show compatibilityResult
            ++ " versus " ++ show sessionResult

      invalidKind <- expectRight $ parseHType "Maybe"
      invalidTarget <- expectRight $ mkIdentifier "invalidPrepared"
      invalidRequest <- sharedDjinnRequest invalidTarget []
        defaultQueryOptions invalidKind
      case ( inhabitGenerated defaultQueryOptions standardEnvironment []
               "invalidPrepared" invalidKind
           , runDjinnQuery standardSession invalidRequest
           ) of
        (Left compatibilityFailure, Left sessionFailure) -> do
          diagnosticCode sessionFailure @?= Just "DJEX_DJINN_QUERY"
          diagnosticContext sessionFailure @?= [compatibilityFailure]
        (compatibilityResult, sessionResult) -> fail $
          "invalid-kind paths diverged: " ++ show compatibilityResult
            ++ " versus " ++ show sessionResult
  , testCase "reject targets outside the shared output namespace" $ do
      qualifier <- expectRight $ mkModuleName "External"
      target <- expectRight $ mkQualifiedIdentifier qualifier "answer"
      mkDefinitionName target @?= Left (InvalidFunctionName target)
  , testCase "run a checked Exference session through the shared envelope" $ do
      checked <- expectRight $ checkSourceEnvironment emptyExferenceSource
      session <- expectRight $ ExferenceCompatibility.mkExferenceSession checked
      exferenceSessionInventory session @?=
        fmap (const ()) (checkedSourceInventory checked)
      exferenceSessionOmissions session @?= []
      exferenceSessionDiagnostics session @?= []
      target <- expectRight $ mkIdentifier "identity"
      request <- expectRight $ parseExferenceRequest
        session defaultExferenceOptions target "identity-query" "a -> a"
      results <- expectRight $ runExferenceQuery session request
      case dropWhile (null . batchCandidates . resultSearch) results of
        result : _ -> do
          resultEvidence result @?= ValidatedCandidates
          case batchCandidates $ resultSearch result of
            candidate : _ -> case candidateOutput candidate of
              FunctionClause actualTarget patterns _ -> do
                actualTarget @?=
                  requestTarget (exferenceRequestQuery request)
                definitionName actualTarget @?= target
                patterns @?= []
                exferenceCandidateMetrics candidate @?=
                  exferenceCandidateStatistics (candidateDetails candidate)
            [] -> fail "Exference reported candidate evidence without a candidate"
          let metadata = batchMetadata $ resultSearch result
          exferenceResultBindingUsages result @?=
            exferenceBatchBindingUsages metadata
        [] -> fail "Exference found no identity candidate"
  , testCase "hide Exference provenance while retaining source spellings" $ do
      environment <- expectRight
        (mkEnvironment [] :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      target <- expectRight $ mkIdentifier "opaqueRequest"
      sourced <- expectRight $ parseExferenceRequest session
        defaultExferenceOptions {exferenceMaximumSteps = 32}
        target "opaque-request.djex" "sourceVariable -> sourceVariable"
      let query = exferenceRequestQuery sourced
      plain <- expectRight $ mkExferenceRequest query
      plain @?= sourced
      show plain @?= show sourced
      show plain @?= show query
      candidate <- firstExferenceCandidate =<< expectRight
        (runExferenceQuery session sourced)
      assertBool "the internal source cache did not reach rendering hints"
        $ "sourceVariable" `elem` Map.elems
            (exferenceCandidateTypeVariableNames $ candidateDetails candidate)
  , testCase "keep checked-HSE and neutral Exference sessions equivalent" $ do
      backendIdentityName <- expectRight
        $ mkQualifiedName ["Fixture"] "identity"
      target <- expectRight $ mkIdentifier "adapterIdentity"
      let identityBinding = FunctionBinding
            { functionResult = TypeVar 0
            , functionName = backendIdentityName
            , functionPenalty = Penalty 0
            , functionConstraints = []
            , functionParameters = [TypeVar 0]
            }
          source = emptyExferenceSource
            {sourceBindings = [SourceFunction identityBinding]}
      checked <- expectRight $ checkSourceEnvironment source
      hseSession <- expectRight
        $ ExferenceCompatibility.mkExferenceSession checked
      let neutralEnvironment :: ExferenceEnvironment
          neutralEnvironment = inventoryEnvironment
            $ fmap (const ()) $ checkedSourceInventory checked
      neutralSession <- expectRight $ mkExferenceSession neutralEnvironment
      checkedTarget <- expectRight $ mkDefinitionName target
      let variableType = TypeVariable $ FlexibleVariable 0
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = checkedTarget
        , requestGoal = FunctionType variableType variableType
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            {exferenceMaximumSteps = 64}
        }
      hseResults <- expectRight $ runExferenceQuery hseSession request
      neutralResults <- expectRight $ runExferenceQuery neutralSession request
      hseResults @?= neutralResults
  , testCase "seal Exference directly from a neutral environment" $ do
      environment <- expectRight
        (mkEnvironment [] :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      environmentDeclarations
          (inventoryEnvironment $ exferenceSessionInventory session) @?= []
      exferenceSessionOmissions session @?= []
      target <- expectRight $ mkIdentifier "neutralIdentity"
      request <- expectRight $ parseExferenceRequest session
        defaultExferenceOptions {exferenceMaximumSteps = 32}
        target "neutral-identity" "a -> a"
      _ <- firstExferenceCandidate =<< expectRight
        (runExferenceQuery session request)
      pure ()
  , testCase "open neutral queries around maxBound rigid variables" $ do
      boundaryName <- expectRight $ parseName "Fixture.boundary"
      target <- expectRight $ mkIdentifier "boundaryIdentity"
      let rigidType = TypeVariable $ RigidVariable maxBound
          declaration = ValueDeclaration
            $ ValueSignature () boundaryName rigidType
          variable = FlexibleVariable 0
          variableType = TypeVariable variable
          goal = ForallType [variable] []
            $ FunctionType variableType variableType
      environment <- expectRight
        (mkEnvironment [declaration] :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      checkedTarget <- expectRight $ mkDefinitionName target
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = checkedTarget
        , requestGoal = goal
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            {exferenceMaximumSteps = 32}
        }
      _ <- firstExferenceCandidate =<< expectRight
        (runExferenceQuery session request)
      pure ()
  , testCase "expand neutral synonyms through both checked backends" $ do
      djinnAliasBody <- expectRight $ parseHType "a"
      djinnEnvironment <- expectRight $ declare
        (TypeSynonym "Identity" ["a"] djinnAliasBody) standardEnvironment
      djinnSession <- sealDjinnEnvironment djinnEnvironment
      djinnTarget <- expectRight $ mkIdentifier "djinnSynonymIdentity"
      djinnGoal <- expectRight $ parseHType "Identity a -> a"
      djinnRequest <- sharedDjinnRequest djinnTarget []
        defaultQueryOptions djinnGoal
      originalDjinnGoal <- expectRight $ toSynthesisType djinnGoal
      requestGoal (djinnRequestQuery djinnRequest) @?= originalDjinnGoal
      djinnResult <- expectRight $ runDjinnQuery djinnSession djinnRequest
      assertBool "Djinn did not elaborate its session-dependent goal alias"
        $ not $ null $ batchCandidates $ resultSearch djinnResult

      aliasName <- expectRight $ parseName "Fixture.Identity"
      target <- expectRight $ mkIdentifier "synonymIdentity"
      let variable = FlexibleVariable 0
          variableType = TypeVariable variable
          aliasDeclaration = TypeSynonymDeclaration () aliasName
            [TypeParameter variable Nothing] variableType
      environment <- expectRight
        (mkEnvironment [aliasDeclaration] :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      parsed <- expectRight $ parseExferenceRequest session
        defaultExferenceOptions {exferenceMaximumSteps = 32}
        target "synonym-query" "Fixture.Identity a -> a"
      _ <- firstExferenceCandidate =<< expectRight
        (runExferenceQuery session parsed)
      checkedTarget <- expectRight $ mkDefinitionName target
      let sharedGoal = FunctionType
            (TypeApplication (TypeConstructor aliasName) variableType)
            variableType
          sharedQuery = QueryRequest
            { requestTarget = checkedTarget
            , requestGoal = sharedGoal
            , requestContexts = []
            , requestOptions = defaultExferenceOptions
                {exferenceMaximumSteps = 32}
            }
      shared <- expectRight $ mkExferenceRequest sharedQuery
      _ <- firstExferenceCandidate =<< expectRight
        (runExferenceQuery session shared)
      pure ()
  , testCase "retain source locations for deferred synonym failures" $ do
      aliasName <- expectRight $ parseName "Fixture.Alias"
      higherName <- expectRight $ parseName "Fixture.Higher"
      target <- expectRight $ mkIdentifier "partialAlias"
      let variable = FlexibleVariable 0
          alias = TypeSynonymDeclaration () aliasName
            [TypeParameter variable Nothing] $ TypeVariable variable
          higher = AbstractTypeDeclaration () higherName
            $ FunctionKind (FunctionKind ProperTypeKind ProperTypeKind)
                ProperTypeKind
      environment <- expectRight
        (mkEnvironment [alias, higher] :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ parseExferenceRequest session
        defaultExferenceOptions target "partial-alias.djex"
        "Fixture.Higher Fixture.Alias"
      case runExferenceQuery session request of
        Left failure -> do
          diagnosticCode failure @?= Just "DJEX_EXF_SYNONYM"
          diagnosticSource failure @?= Just "partial-alias.djex"
          assertBool "deferred synonym failure lost its source range"
            $ diagnosticSpan failure /= Nothing
        Right _ -> fail "an unsaturated synonym reached Exference search"
  , testCase "apply exact exclusions to a neutral Exference session" $ do
      bindingName <- expectRight $ parseName "Fixture.identity"
      let variableType = TypeVariable $ FlexibleVariable 0
          declaration = ValueDeclaration $ ValueSignature () bindingName
            $ FunctionType variableType variableType
          policy = defaultExferenceSessionPolicy
            {exferenceExcludedBindings = [bindingName]}
      environment <- expectRight
        (mkEnvironment [declaration] :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSessionWithPolicy policy environment
      case exferenceSessionOmissions session of
        [omission] -> do
          omittedName omission @?= bindingName
          omittedCapability omission @?= BindingIntroduction
          omittedReason omission @?= ExcludedByPolicy
        omissions -> fail $ "unexpected policy omissions: " ++ show omissions
  , testCase "apply neutral rating overrides to candidate penalties" $ do
      tokenName <- expectRight $ parseName "Fixture.Token"
      preferredName <- expectRight $ parseName "Fixture.preferred"
      ordinaryName <- expectRight $ parseName "Fixture.ordinary"
      target <- expectRight $ mkIdentifier "ratedToken"
      checkedTarget <- expectRight $ mkDefinitionName target
      let tokenType = TypeConstructor tokenName
          declarations =
            [ AbstractTypeDeclaration () tokenName ProperTypeKind
            , ValueDeclaration $ ValueSignature () preferredName tokenType
            , ValueDeclaration $ ValueSignature () ordinaryName tokenType
            ]
          ratedPolicy = defaultExferenceSessionPolicy
            { exferenceRatingOverrides = Map.fromList
                [ (preferredName, Penalty (-5))
                , (ordinaryName, Penalty 5)
                ]
            }
          scores policy = do
            environment <- expectRight
              (mkEnvironment declarations :: Either
                (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
            session <- expectRight
              $ mkExferenceSessionWithPolicy policy environment
            request <- expectRight $ mkExferenceRequest QueryRequest
              { requestTarget = checkedTarget
              , requestGoal = tokenType
              , requestContexts = []
              , requestOptions = defaultExferenceOptions
                  {exferenceMaximumSteps = 64}
              }
            results <- expectRight $ runExferenceQuery session request
            pure $ Map.fromList
              [ (globalName, exferenceCandidateComplexity
                    $ exferenceCandidateMetrics candidate)
              | result <- results
              , candidate <- batchCandidates $ resultSearch result
              , FunctionClause _ [] (Global globalName) <-
                  [candidateOutput candidate]
              ]
      baseline <- scores defaultExferenceSessionPolicy
      rated <- scores ratedPolicy
      let scoreOf label bindingName table = case Map.lookup bindingName table of
            Just score -> pure score
            Nothing -> fail $ label ++ " candidate was not generated"
      baselinePreferred <- scoreOf "baseline preferred" preferredName baseline
      baselineOrdinary <- scoreOf "baseline ordinary" ordinaryName baseline
      ratedPreferred <- scoreOf "rated preferred" preferredName rated
      ratedOrdinary <- scoreOf "rated ordinary" ordinaryName rated
      baselinePreferred @?= baselineOrdinary
      assertBool "negative override did not reduce the candidate penalty"
        $ ratedPreferred < baselinePreferred
      assertBool "positive override did not increase the candidate penalty"
        $ ratedOrdinary > baselineOrdinary
  , testCase "report recursive Exference elimination as a session omission" $ do
      loopName <- expectRight $ parseName "Fixture.Loop"
      let declaration = DataTypeDeclaration () loopName []
            [DataConstructor () loopName [TypeConstructor loopName]]
      environment <- expectRight
        (mkEnvironment [declaration] :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      case exferenceSessionOmissions session of
        [omission] -> do
          omittedName omission @?= loopName
          omittedCapability omission @?= DataElimination
          omittedReason omission @?=
            RecursiveDataEliminationUnsupported
        omissions -> fail $ "unexpected recursive omissions: " ++ show omissions
      map diagnosticCode (exferenceSessionDiagnostics session) @?=
        [Just "DJEX_EXF_RECURSIVE_OMISSION"]
  , testCase "reject Exference contexts whose variables escape the goal" $ do
      target <- expectRight $ mkIdentifier "constrained"
      checkedTarget <- expectRight $ mkDefinitionName target
      className <- expectRight $ mkIdentifier "C"
      let variable identifier = TypeVariable $ FlexibleVariable identifier
          query = QueryRequest
            { requestTarget = checkedTarget
            , requestGoal = variable 0
            , requestContexts = [Constraint className [variable 1]]
            , requestOptions = defaultExferenceOptions
            }
      case mkExferenceRequest query of
        Left failure ->
          diagnosticCode failure @?= Just "DJEX_EXF_REQUEST"
        Right _ -> fail "Exference accepted an out-of-scope context variable"
  , testCase "reject contexts bound only by a nested forall" $ do
      target <- expectRight $ mkIdentifier "nestedConstraint"
      checkedTarget <- expectRight $ mkDefinitionName target
      className <- expectRight $ mkIdentifier "C"
      let variable identifier = TypeVariable $ FlexibleVariable identifier
          goal = FunctionType (variable 0)
            $ ForallType [FlexibleVariable 1] []
            $ FunctionType (variable 1) (variable 1)
          query = QueryRequest
            { requestTarget = checkedTarget
            , requestGoal = goal
            , requestContexts = [Constraint className [variable 1]]
            , requestOptions = defaultExferenceOptions
            }
      case mkExferenceRequest query of
        Left failure ->
          diagnosticCode failure @?= Just "DJEX_EXF_REQUEST"
        Right _ -> fail "Exference accepted a context beneath a nested forall"
  , testCase "validate Exference targets before parsing source" $ do
      checked <- expectRight $ checkSourceEnvironment emptyExferenceSource
      session <- expectRight $ ExferenceCompatibility.mkExferenceSession checked
      qualifier <- expectRight $ mkModuleName "External"
      target <- expectRight $ mkQualifiedIdentifier qualifier "answer"
      case parseExferenceRequest session defaultExferenceOptions
          target "query" "(" of
        Left failure ->
          diagnosticCode failure @?= Just "DJEX_EXF_TARGET"
        Right _ -> fail "Exference accepted an invalid qualified target"
  , testCase "reuse checked Exference targets and retain parse provenance" $ do
      checked <- expectRight $ checkSourceEnvironment emptyExferenceSource
      session <- expectRight $ ExferenceCompatibility.mkExferenceSession checked
      target <- expectRight $ mkIdentifier "answer"
      checkedTarget <- expectRight $ mkDefinitionName target
      rawRequest <- expectRight $ parseExferenceRequest
        session defaultExferenceOptions target "query.hs" "a -> a"
      checkedRequest <- expectRight $ parseExferenceRequestWithCheckedTarget
        session defaultExferenceOptions checkedTarget "query.hs" "a -> a"
      rawRequest @?= checkedRequest
      case parseExferenceRequestWithCheckedTarget session
          defaultExferenceOptions checkedTarget "checked-query.hs" "(" of
        Left failure -> do
          diagnosticCode failure @?= Just "DJEX_EXF_PARSE"
          diagnosticSource failure @?= Just "checked-query.hs"
        Right _ -> fail "Exference parsed an incomplete checked-target query"
  , testCase "scope explicit contexts under every leading forall" $ do
      target <- expectRight $ mkIdentifier "nestedIdentity"
      checkedTarget <- expectRight $ mkDefinitionName target
      className <- expectRight $ mkIdentifier "C"
      backendClassName <- expectRight $ mkQualifiedName [] "C"
      classEnvironment <- expectRight
        $ mkStaticClassEnv [HsTypeClass backendClassName [0] []] []
      let variable identifier = TypeVariable $ FlexibleVariable identifier
          goal = ForallType [FlexibleVariable 0] []
            $ ForallType [FlexibleVariable 1] []
            $ FunctionType (variable 1) (variable 1)
          query = QueryRequest
            { requestTarget = checkedTarget
            , requestGoal = goal
            , requestContexts = [Constraint className [variable 1]]
            , requestOptions = defaultExferenceOptions
                {exferenceMaximumSteps = 32}
            }
          source = emptyExferenceSource {sourceClasses = classEnvironment}
      checked <- expectRight $ checkSourceEnvironment source
      session <- expectRight $ ExferenceCompatibility.mkExferenceSession checked
      request <- expectRight $ mkExferenceRequest query
      results <- expectRight $ runExferenceQuery session request
      assertBool "scoped nested-forall context produced no identity"
        $ any (not . null . batchCandidates . resultSearch) results
  , testCase "classify malformed Exference options independently" $ do
      checked <- expectRight $ checkSourceEnvironment emptyExferenceSource
      session <- expectRight $ ExferenceCompatibility.mkExferenceSession checked
      target <- expectRight $ mkIdentifier "identity"
      request <- expectRight $ parseExferenceRequest session
        defaultExferenceOptions {exferenceMaximumSteps = 0}
        target "invalid-options" "a -> a"
      case runExferenceQuery session request of
        Left failure ->
          diagnosticCode failure @?= Just "DJEX_EXF_OPTIONS"
        Right _ -> fail "Exference accepted a zero search-step limit"
  , testCase "preserve an Exference query filename extension" $ do
      checked <- expectRight $ checkSourceEnvironment emptyExferenceSource
      session <- expectRight $ ExferenceCompatibility.mkExferenceSession checked
      target <- expectRight $ mkIdentifier "broken"
      case parseExferenceRequest
          session defaultExferenceOptions target "query.hs" "(" of
        Left failure -> do
          diagnosticCode failure @?= Just "DJEX_EXF_PARSE"
          diagnosticSource failure @?= Just "query.hs"
        Right _ -> fail "Exference parsed an incomplete input type"
  , testCase "do not turn an Exference environment binding into recursion" $ do
      target <- expectRight $ mkIdentifier "identity"
      backendTarget <- expectRight $ mkQualifiedName [] "identity"
      let identityBinding = FunctionBinding
            { functionResult = TypeVar 0
            , functionName = backendTarget
            , functionPenalty = Penalty 0
            , functionConstraints = []
            , functionParameters = [TypeVar 0]
            }
          source = emptyExferenceSource
            {sourceBindings = [SourceFunction identityBinding]}
          options = defaultExferenceOptions
            {exferenceMaximumSteps = 32}
      checked <- expectRight $ checkSourceEnvironment source
      session <- expectRight $ ExferenceCompatibility.mkExferenceSession checked
      request <- expectRight $ parseExferenceRequest
        session options target "recursive-target" "a -> a"
      results <- expectRight $ runExferenceQuery session request
      case dropWhile (null . batchCandidates . resultSearch) results of
        result : _ -> case batchCandidates $ resultSearch result of
          candidate : _ -> case candidateOutput candidate of
            FunctionClause _ _ body -> assertBool
              "the old environment binding became a self-reference"
              $ not $ referencesGlobal target body
          [] -> fail "Exference reported candidate evidence without a candidate"
        [] -> fail "Exference found no identity candidate after target exclusion"
  , testCase "seal unsupported Exference bindings once with diagnostics" $ do
      rankNName <- expectRight $ mkQualifiedName [] "rankN"
      let rankN = FunctionBinding
            { functionResult = TypeVar 0
            , functionName = rankNName
            , functionPenalty = Penalty 0
            , functionConstraints = []
            , functionParameters =
                [TypeForall [1] [] $ TypeVar 1]
            }
          source = emptyExferenceSource
            {sourceBindings = [SourceFunction rankN]}
      checked <- expectRight $ checkSourceEnvironment source
      session <- expectRight $ ExferenceCompatibility.mkExferenceSession checked
      environmentDeclarations
          (inventoryEnvironment $ exferenceSessionInventory session) @?=
        map (fmap $ const ())
          (environmentDeclarations
            $ inventoryEnvironment $ checkedSourceInventory checked)
      case exferenceSessionOmissions session of
        [omission] -> do
          omittedCapability omission @?= BindingIntroduction
          omittedReason omission @?= UnsupportedNestedForall
        omissions -> fail $ "expected one rank-N omission, got " ++ show omissions
      map diagnosticCode (exferenceSessionDiagnostics session) @?=
        [Just "DJEX_EXF_OMISSION"]
      map diagnosticSeverity (exferenceSessionDiagnostics session) @?=
        [Warning]
  , testCase "exclude Exference bindings by exact structural policy name" $ do
      blockedBackendName <- expectRight
        $ mkQualifiedName ["Data", "Function"] "fix"
      retainedBackendName <- expectRight
        $ mkQualifiedName ["Fixture"] "fix"
      blockedName <- expectRight $ parseName "Data.Function.fix"
      let binding name = FunctionBinding
            { functionResult = TypeVar 0
            , functionName = name
            , functionPenalty = Penalty 0
            , functionConstraints = []
            , functionParameters = [TypeVar 0]
            }
          source = emptyExferenceSource
            { sourceBindings = map SourceFunction
                [binding blockedBackendName, binding retainedBackendName]
            }
          policy = defaultExferenceSessionPolicy
            {exferenceExcludedBindings = [blockedName]}
      checked <- expectRight $ checkSourceEnvironment source
      session <- expectRight $
        ExferenceCompatibility.mkExferenceSessionWithPolicy policy checked
      case exferenceSessionOmissions session of
        [omission] -> do
          omittedName omission @?= blockedName
          omittedReason omission @?= ExcludedByPolicy
        omissions -> fail $ "expected one policy omission, got " ++ show omissions
      map diagnosticCode (exferenceSessionDiagnostics session) @?=
        [Just "DJEX_EXF_POLICY_OMISSION"]
  , testCase "reject definition qualification that creates self-reference" $ do
      target <- expectRight $ mkIdentifier "result"
      checkedTarget <- expectRight $ mkDefinitionName target
      sharedGlobal <- expectRight $ parseName "Fixture.result"
      let candidate = Candidate
            (FunctionClause checkedTarget [] $ Global sharedGlobal)
            [] emptyExferenceCandidateDetails
      renderExferenceCandidateDefinition Unqualified candidate @?=
        Left (GlobalDefinitionCapture target sharedGlobal Unqualified)
      renderExferenceCandidateDefinition FullyQualified candidate @?=
        Right "result = Fixture.result"
  , testCase "preserve Exference clause binders in expression rendering" $ do
      target <- expectRight $ mkIdentifier "result"
      checkedTarget <- expectRight $ mkDefinitionName target
      sharedGlobal <- expectRight $ parseName "Fixture.value"
      let patternedCandidate = Candidate
            (FunctionClause checkedTarget [Wildcard] $ Global sharedGlobal)
            [] emptyExferenceCandidateDetails
      renderExferenceCandidateExpression FullyQualified patternedCandidate @?=
        Right "\\_ -> Fixture.value"
  ]

assertDjinnCompatibility
  :: String
  -> DjinnCore.Environment
  -> DjinnSession
  -> [Context]
  -> QueryOptions
  -> String
  -> HType
  -> IO ()
assertDjinnCompatibility label environment session contexts options target goal = do
  targetName <- expectRight $ mkIdentifier target
  compatibility <- expectRight $ inhabitGenerated
    options environment contexts target goal
  request <- sharedDjinnRequest targetName contexts options goal
  shared <- expectRight $ runDjinnQuery session request
  generatedReportEvidence compatibility @?= resultEvidence shared
  case batchProgress $ resultSearch shared of
    Completed completion ->
      generatedReportCompletion compatibility @?= completion
    Continuing -> fail $ label ++ ": Djinn returned a nonterminal batch"
  compatibilityCandidates <- mapM projectDjinnCandidate
    $ generatedReportCandidates compatibility
  compatibilityCandidates @?= batchCandidates (resultSearch shared)
  let metadata = batchMetadata $ resultSearch shared
  assertBool (label ++ ": translated formula changed") $
    generatedReportFormula compatibility == djinnTranslatedFormula metadata
  assertBool (label ++ ": first proof changed") $
    generatedReportProof compatibility == djinnFirstExploredProof metadata

sharedDjinnRequest
  :: Name
  -> [Context]
  -> QueryOptions
  -> HType
  -> IO DjinnRequest
sharedDjinnRequest target contexts options goal =
  expectRight . mkDjinnRequest =<<
    sharedDjinnQuery target contexts options goal

sharedDjinnQuery
  :: Name
  -> [Context]
  -> QueryOptions
  -> HType
  -> IO (QueryRequest DjinnType QueryOptions)
sharedDjinnQuery target contexts options goal = do
  checkedTarget <- expectRight $ mkDefinitionName target
  sharedGoal <- expectRight $ toSynthesisType goal
  sharedContexts <- expectRight
    $ traverse (traverse toSynthesisType) contexts
  pure $ QueryRequest
    { requestTarget = checkedTarget
    , requestGoal = sharedGoal
    , requestContexts = sharedContexts
    , requestOptions = options
    }

projectDjinnCandidate :: DjinnCore.DjinnCandidate -> IO DjinnCandidate
projectDjinnCandidate candidate = do
  residualConstraints <- expectRight
    $ traverse (traverse toSynthesisType)
    $ candidateResidualConstraints candidate
  pure Candidate
    { candidateOutput = candidateOutput candidate
    , candidateResidualConstraints = residualConstraints
    , candidateDetails = candidateDetails candidate
    }

sealDjinnEnvironment :: DjinnCore.Environment -> IO DjinnSession
sealDjinnEnvironment environment = do
  shared <- expectRight $ toSynthesisEnvironment environment
  expectRight $ mkDjinnSession shared

emptyExferenceCandidateDetails :: ExferenceCandidateDetails
emptyExferenceCandidateDetails = ExferenceCandidateDetails
  { exferenceCandidateStatistics = ExferenceCandidateMetrics 0 0 0
  , exferenceCandidateLocalNames = mempty
  , exferenceCandidateTypeVariableNames = mempty
  }

emptyExferenceSource :: SourceEnvironment
emptyExferenceSource = SourceEnvironment
  { sourceBindings = []
  , sourceDeconstructors = []
  , sourceClasses = emptyStaticClassEnv
  , sourceTypeNames = []
  , sourceTypeSynonyms = []
  }

referencesGlobal :: Name -> Expression local -> Bool
referencesGlobal target expression = case expression of
  Local _ -> False
  Global name -> name == target
  Lambda _ body -> referencesGlobal target body
  Apply function argument ->
    referencesGlobal target function || referencesGlobal target argument
  Tuple elements -> any (referencesGlobal target) elements
  Hole _ -> False
  Let _ value body ->
    referencesGlobal target value || referencesGlobal target body
  Case scrutinee alternatives ->
    referencesGlobal target scrutinee ||
      any (referencesGlobal target . snd) alternatives

firstExferenceCandidate :: [ExferenceResult] -> IO ExferenceCandidate
firstExferenceCandidate results = case
    dropWhile (null . batchCandidates . resultSearch) results of
  result : _ -> case batchCandidates $ resultSearch result of
    candidate : _ -> pure candidate
    [] -> fail "Exference reported a nonempty batch without a candidate"
  [] -> fail "Exference produced no candidate"

expectRight :: Show error => Either error value -> IO value
expectRight result = case result of
  Left failure -> fail $ show failure
  Right value -> pure value
