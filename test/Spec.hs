module Main (main) where

import Data.Either (isRight)
import Data.List (isInfixOf, nub)
import Djinn.Core
  ( Declaration (ClassDecl, DataType, Function)
  , Environment
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
  )
-- Raw Exference fixtures below use their historical @functionName@ field;
-- hide the shared structural-name accessor at this integration-only seam.
import Language.Haskell.Djex hiding (functionName)
import Language.Haskell.Exference.Core.FunctionBinding (FunctionBinding (..))
import Language.Haskell.Exference.Core.Candidate
  ( mkExferenceGeneratedCandidate )
import Language.Haskell.Exference.Core.ExferenceStats
  ( ExferenceStats (ExferenceStats) )
import qualified Language.Haskell.Exference.Core.Expression as CoreExpression
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
      session <- expectRight $ mkDjinnSession standardEnvironment
      target <- expectRight $ mkIdentifier "swap"
      goal <- expectRight $ parseHType "(a, b) -> (b, a)"
      result <- expectRight $ runDjinnQuery session QueryRequest
        { requestTarget = target
        , requestGoal = goal
        , requestContexts = []
        , requestOptions = defaultQueryOptions
        }
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
      session <- expectRight $ mkDjinnSession environment
      target <- expectRight $ mkIdentifier "pair"
      result <- expectRight $ runDjinnQuery session QueryRequest
        { requestTarget = target
        , requestGoal = goal
        , requestContexts = []
        , requestOptions = defaultQueryOptions
            { optionAlternatives = True
            , optionSorted = False
            , optionCutoff = 1
            }
        }
      resultEvidence result @?= ValidatedCandidates
      length (batchCandidates $ resultSearch result) @?= 1
      case batchProgress $ resultSearch result of
        Completed (Truncated reasons) ->
          assertBool "candidate-limit truncation reason was lost" $
            CandidateLimitReached `elem` reasons
        completion -> fail $ "expected candidate truncation, got "
          ++ show completion
  , testCase "keep Djinn evidence independent of search completion" $ do
      session <- expectRight $ mkDjinnSession standardEnvironment
      target <- expectRight $ mkIdentifier "peirce"
      goal <- expectRight $ parseHType "((a -> b) -> a) -> a"
      refutation <- expectRight $ runDjinnQuery session QueryRequest
        { requestTarget = target
        , requestGoal = goal
        , requestContexts = []
        , requestOptions = defaultQueryOptions
        }
      resultEvidence refutation @?= ProvedUninhabitable
      batchProgress (resultSearch refutation) @?= Completed Finished
      undecided <- expectRight $ runDjinnQuery session QueryRequest
        { requestTarget = target
        , requestGoal = goal
        , requestContexts = []
        , requestOptions = defaultQueryOptions { optionBudget = Just 0 }
        }
      resultEvidence undecided @?= NoEvidence
      case batchProgress $ resultSearch undecided of
        Completed (Truncated reasons) ->
          assertBool "choice-point truncation reason was lost" $
            ChoicePointLimitReached `elem` reasons
        completion -> fail $ "expected a truncated search, got " ++ show completion
  , testCase "preserve Djinn's target-reference evidence" $ do
      variable <- expectRight $ parseHType "a"
      environment <- expectRight $
        declare (Function "token" variable) standardEnvironment
      session <- expectRight $ mkDjinnSession environment
      target <- expectRight $ mkIdentifier "token"
      result <- expectRight $ runDjinnQuery session QueryRequest
        { requestTarget = target
        , requestGoal = variable
        , requestContexts = []
        , requestOptions = defaultQueryOptions
        }
      resultEvidence result @?= RequiresTargetReference
      batchCandidates (resultSearch result) @?= []
  , testCase "reuse sealed Djinn kinds without changing query semantics" $ do
      standardSession <- expectRight $ mkDjinnSession standardEnvironment

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
      captureSession <- expectRight $ mkDjinnSession captureEnvironment
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
      higherSession <- expectRight $ mkDjinnSession higherEnvironment
      unsaturatedSynonym <- expectRight $ parseHType "Not"
      higherContext <- expectRight $
        mkContext "HigherPrepared" [unsaturatedSynonym]
      higherTarget <- expectRight $ mkIdentifier "higherGoalPrepared"
      let higherRequest = QueryRequest
            { requestTarget = higherTarget
            , requestGoal = captureGoal
            , requestContexts = [higherContext]
            , requestOptions = defaultQueryOptions
            }
      case ( inhabitGenerated defaultQueryOptions higherEnvironment
               [higherContext] "higherGoalPrepared" captureGoal
           , runDjinnQuery higherSession higherRequest
           ) of
        (Left compatibilityFailure, Left sessionFailure) -> do
          assertBool "prepared path skipped Djinn synonym saturation" $
            "Type synonym Not expects 1 argument(s), but got 0"
              `isInfixOf` compatibilityFailure
          diagnosticContext sessionFailure @?= [compatibilityFailure]
        (compatibilityResult, sessionResult) -> fail $
          "synonym-saturation paths diverged: " ++ show compatibilityResult
            ++ " versus " ++ show sessionResult

      invalidKind <- expectRight $ parseHType "Maybe"
      invalidTarget <- expectRight $ mkIdentifier "invalidPrepared"
      let invalidRequest = QueryRequest
            { requestTarget = invalidTarget
            , requestGoal = invalidKind
            , requestContexts = []
            , requestOptions = defaultQueryOptions
            }
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
  , testCase "reject targets outside Djinn's output namespace" $ do
      session <- expectRight $ mkDjinnSession standardEnvironment
      qualifier <- expectRight $ mkModuleName "External"
      target <- expectRight $ mkQualifiedIdentifier qualifier "answer"
      goal <- expectRight $ parseHType "a -> a"
      case runDjinnQuery session QueryRequest
          { requestTarget = target
          , requestGoal = goal
          , requestContexts = []
          , requestOptions = defaultQueryOptions
          } of
        Left failure -> diagnosticCode failure @?= Just "DJEX_DJINN_TARGET"
        Right _ -> fail "Djinn accepted a qualified generated definition"
  , testCase "run a checked Exference session through the shared envelope" $ do
      checked <- expectRight $ checkSourceEnvironment emptyExferenceSource
      session <- expectRight $ mkExferenceSession checked
      exferenceSessionInventory session @?= checkedSourceInventory checked
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
                actualTarget @?= target
                patterns @?= []
            [] -> fail "Exference reported candidate evidence without a candidate"
        [] -> fail "Exference found no identity candidate"
  , testCase "reject Exference contexts whose variables escape the goal" $ do
      target <- expectRight $ mkIdentifier "constrained"
      className <- expectRight $ mkIdentifier "C"
      let variable identifier = TypeVariable $ FlexibleVariable identifier
          query = QueryRequest
            { requestTarget = target
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
      className <- expectRight $ mkIdentifier "C"
      let variable identifier = TypeVariable $ FlexibleVariable identifier
          goal = FunctionType (variable 0)
            $ ForallType [FlexibleVariable 1] []
            $ FunctionType (variable 1) (variable 1)
          query = QueryRequest
            { requestTarget = target
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
      session <- expectRight $ mkExferenceSession checked
      qualifier <- expectRight $ mkModuleName "External"
      target <- expectRight $ mkQualifiedIdentifier qualifier "answer"
      case parseExferenceRequest session defaultExferenceOptions
          target "query" "(" of
        Left failure ->
          diagnosticCode failure @?= Just "DJEX_EXF_TARGET"
        Right _ -> fail "Exference accepted an invalid qualified target"
  , testCase "scope explicit contexts under every leading forall" $ do
      target <- expectRight $ mkIdentifier "nestedIdentity"
      className <- expectRight $ mkIdentifier "C"
      backendClassName <- expectRight $ mkQualifiedName [] "C"
      classEnvironment <- expectRight
        $ mkStaticClassEnv [HsTypeClass backendClassName [0] []] []
      let variable identifier = TypeVariable $ FlexibleVariable identifier
          goal = ForallType [FlexibleVariable 0] []
            $ ForallType [FlexibleVariable 1] []
            $ FunctionType (variable 1) (variable 1)
          query = QueryRequest
            { requestTarget = target
            , requestGoal = goal
            , requestContexts = [Constraint className [variable 1]]
            , requestOptions = defaultExferenceOptions
                {exferenceMaximumSteps = 32}
            }
          source = emptyExferenceSource {sourceClasses = classEnvironment}
      checked <- expectRight $ checkSourceEnvironment source
      session <- expectRight $ mkExferenceSession checked
      request <- expectRight $ mkExferenceRequest query
      results <- expectRight $ runExferenceQuery session request
      assertBool "scoped nested-forall context produced no identity"
        $ any (not . null . batchCandidates . resultSearch) results
  , testCase "classify malformed Exference options independently" $ do
      checked <- expectRight $ checkSourceEnvironment emptyExferenceSource
      session <- expectRight $ mkExferenceSession checked
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
      session <- expectRight $ mkExferenceSession checked
      target <- expectRight $ mkIdentifier "broken"
      case parseExferenceRequest
          session defaultExferenceOptions target "query.hs" "(" of
        Left failure -> diagnosticSource failure @?= Just "query.hs"
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
      session <- expectRight $ mkExferenceSession checked
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
      session <- expectRight $ mkExferenceSession checked
      exferenceSessionInventory session @?= checkedSourceInventory checked
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
      session <- expectRight $ mkExferenceSessionWithPolicy policy checked
      case exferenceSessionOmissions session of
        [omission] -> do
          omittedName omission @?= blockedName
          omittedReason omission @?= ExcludedByPolicy
        omissions -> fail $ "expected one policy omission, got " ++ show omissions
      map diagnosticCode (exferenceSessionDiagnostics session) @?=
        [Just "DJEX_EXF_POLICY_OMISSION"]
  , testCase "reject definition qualification that creates self-reference" $ do
      target <- expectRight $ mkIdentifier "result"
      backendGlobal <- expectRight
        $ mkQualifiedName ["Fixture"] "result"
      sharedGlobal <- expectRight $ parseName "Fixture.result"
      raw <- expectRight $ mkExferenceGeneratedCandidate mempty
        (CoreExpression.ExpName backendGlobal) [] (ExferenceStats 1 0 0)
      let candidate = fmap (FunctionClause target []) raw
      renderExferenceCandidateDefinition Unqualified candidate @?=
        Left (ExferenceGeneratedRenderError
          $ GlobalDefinitionCapture target
              sharedGlobal Unqualified)
      renderExferenceCandidateDefinition FullyQualified candidate @?=
        Right "result = Fixture.result"
  ]

assertDjinnCompatibility
  :: String
  -> Environment
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
  shared <- expectRight $ runDjinnQuery session QueryRequest
    { requestTarget = targetName
    , requestGoal = goal
    , requestContexts = contexts
    , requestOptions = options
    }
  generatedReportEvidence compatibility @?= resultEvidence shared
  case batchProgress $ resultSearch shared of
    Completed completion ->
      generatedReportCompletion compatibility @?= completion
    Continuing -> fail $ label ++ ": Djinn returned a nonterminal batch"
  generatedReportCandidates compatibility @?=
    batchCandidates (resultSearch shared)
  let metadata = batchMetadata $ resultSearch shared
  assertBool (label ++ ": translated formula changed") $
    generatedReportFormula compatibility == djinnTranslatedFormula metadata
  assertBool (label ++ ": first proof changed") $
    generatedReportProof compatibility == djinnFirstExploredProof metadata

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

expectRight :: Show error => Either error value -> IO value
expectRight result = case result of
  Left failure -> fail $ show failure
  Right value -> pure value
