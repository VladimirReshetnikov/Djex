{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Control.Exception
  ( AsyncException (ThreadKilled)
  , SomeException
  , bracket
  , evaluate
  , fromException
  , throw
  , try
  )
import Data.Either (isRight)
import Data.List (find, isInfixOf, isPrefixOf, nub, tails)
import qualified Data.Map.Strict as Map
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hClose, hPutStr, openTempFile)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
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
  , inhabitResult
  , mkContext
  , parseHType
  , standardEnvironment
  , toSynthesisEnvironment
  , toSynthesisType
  )
import qualified Djinn.Core as DjinnCore (Environment)
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
  ( HsTypeClass (HsTypeClass)
  , emptyStaticClassEnv
  , mkStaticClassEnv
  , pattern TypeForall
  , pattern TypeVar
  )
import Language.Haskell.Exference.EnvironmentParser
  ( LoadReport (..)
  , SourceBinding (SourceFunction)
  , SourceEnvironment (..)
  , checkSourceEnvironment
  , checkedSourceInventory
  , environmentFromFiles
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertEqual, testCase)

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
        [ DecidingInhabitation
        , PrenexPolymorphism
        , RankNIntroduction
        , OpaqueRankNTypes
        , ImpredicativeTypes
        , TypeClassConstraints
        ]
      backendCapabilities (backendInfo ExferenceBackend) @?=
        [ HeuristicSearch
        , PrenexPolymorphism
        , RankNIntroduction
        , RankNElimination
        , OpaqueRankNTypes
        , ImpredicativeTypes
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
  , testCase "retain the exact checked Djinn operator target" $ do
      session <- sealDjinnEnvironment standardEnvironment
      target <- expectRight $ mkOperator "<~>"
      goal <- expectRight $ parseHType "a -> a"
      request <- sharedDjinnRequest target [] defaultQueryOptions goal
      result <- expectRight $ runDjinnQuery session request
      case batchCandidates $ resultSearch result of
        candidate : _ -> do
          clauseName (candidateOutput candidate) @?=
            requestTarget (djinnRequestQuery request)
          definitionName (clauseName $ candidateOutput candidate) @?= target
        [] -> fail "Djinn returned no candidate for the identity operator"
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
  , testCase "materialize Djinn provenance while sealing a request" $ do
      session <- sealDjinnEnvironment standardEnvironment
      target <- expectRight $ mkIdentifier "strictProvenance"
      result <- try $ evaluate $ parseDjinnRequest session
        defaultQueryOptions target
        (error "unforced Djinn source name") "a -> a"
      case result
          :: Either SomeException (Either Diagnostic DjinnRequest) of
        Left _ -> pure ()
        Right _ -> fail
          "the Djinn adapter retained lazy provenance past request sealing"
  , testCase "defer cyclic Djinn context spines to session arity" $ do
      session <- sealDjinnEnvironment standardEnvironment
      targetName <- expectRight $ mkIdentifier "cyclicDjinnContext"
      target <- expectRight $ mkDefinitionName targetName
      className <- expectRight $ mkIdentifier "Eq"
      let argument = TypeVariable "a"
          arguments = argument : arguments
          query = QueryRequest
            { requestTarget = target
            , requestGoal = argument
            , requestContexts = [Constraint className arguments]
            , requestOptions = defaultQueryOptions
            }
      sealed <- expectWithin "Djinn request sealing" $ evaluate
        $ mkDjinnRequest query
      request <- expectRight sealed
      result <- expectWithin "Djinn context arity validation" $ evaluate
        $ runDjinnQuery session request
      case result of
        Left failure -> do
          diagnosticCode failure @?= Just "DJEX_DJINN_QUERY"
          assertBool "Djinn lost the bounded arity failure"
            $ any ("expects 1 type argument(s), but got 2" `isInfixOf`)
            $ diagnosticContext failure
        Right _ -> fail "Djinn accepted a cyclic unary-class argument spine"
      let embeddedQuery = query
            { requestGoal = ForallType []
                [Constraint className arguments]
                argument
            , requestContexts = []
            }
      embeddedSealed <- expectWithin "embedded Djinn request sealing"
        $ evaluate $ mkDjinnRequest embeddedQuery
      embeddedRequest <- expectRight embeddedSealed
      embeddedResult <- expectWithin "embedded Djinn context arity validation"
        $ evaluate $ runDjinnQuery session embeddedRequest
      case embeddedResult of
        Left failure -> do
          diagnosticCode failure @?= Just "DJEX_DJINN_QUERY"
          assertBool "an embedded context lost bounded arity validation"
            $ any ("expects 1 type argument(s), but got 2" `isInfixOf`)
            $ diagnosticContext failure
        Right _ -> fail "Djinn accepted an embedded cyclic class spine"
  , testCase "accept prenex Djinn constraints without assuming methods" $ do
      session <- sealDjinnEnvironment standardEnvironment
      targetName <- expectRight $ mkIdentifier "prenexIdentity"
      target <- expectRight $ mkDefinitionName targetName
      eqName <- expectRight $ mkIdentifier "Eq"
      let variable = TypeVariable "a"
          signature = ForallType ["a"]
            [Constraint eqName [variable]]
            (FunctionType variable variable)
          query = QueryRequest
            { requestTarget = target
            , requestGoal = signature
            , requestContexts = []
            , requestOptions = defaultQueryOptions
            }
      request <- expectRight $ mkDjinnRequest query
      djinnRequestQuery request @?= query
      result <- expectRight $ runDjinnQuery session request
      resultEvidence result @?= ValidatedCandidates
      assertBool "a prenex irrelevant constraint hid the identity proof"
        $ not $ null $ batchCandidates $ resultSearch result

      binderlessTargetName <- expectRight $ mkIdentifier "binderlessIdentity"
      binderlessTarget <- expectRight $ mkDefinitionName binderlessTargetName
      let binderlessQuery = query
            { requestTarget = binderlessTarget
            , requestGoal = ForallType []
                [Constraint eqName [variable]]
                (FunctionType variable variable)
            }
      binderlessRequest <- expectRight $ mkDjinnRequest binderlessQuery
      binderlessResult <- expectRight $
        runDjinnQuery session binderlessRequest
      resultEvidence binderlessResult @?= ValidatedCandidates

      nestedTargetName <- expectRight $ mkIdentifier "nestedForall"
      nestedTarget <- expectRight $ mkDefinitionName nestedTargetName
      listConstructor <- expectRight $ specialName ListConstructor
      let polymorphic binder = ForallType [binder] []
            $ FunctionType (TypeVariable binder) (TypeVariable binder)
          listOf element = TypeApplication
            (TypeConstructor listConstructor) element
      let nestedQuery = query
            { requestTarget = nestedTarget
            , requestGoal = FunctionType
                (polymorphic "input") (polymorphic "output")
            }
      nestedRequest <- expectRight $ mkDjinnRequest nestedQuery
      nestedResult <- expectRight $ runDjinnQuery session nestedRequest
      resultEvidence nestedResult @?= ValidatedCandidates
      assertBool "alpha-renamed rank-N identity produced no proof"
        $ not $ null $ batchCandidates $ resultSearch nestedResult

      contextualTargetName <- expectRight
        $ mkIdentifier "contextualRankNCallback"
      contextualTarget <- expectRight
        $ mkDefinitionName contextualTargetName
      let contextualQuery = query
            { requestTarget = contextualTarget
            , requestGoal = FunctionType
                (FunctionType signature $ TypeVariable "result")
                (TypeVariable "result")
            }
      contextualRequest <- expectRight $ mkDjinnRequest contextualQuery
      contextualResult <- expectRight
        $ runDjinnQuery session contextualRequest
      resultEvidence contextualResult @?= ValidatedCandidates
      assertBool "Djinn did not introduce a contextual rank-N callback"
        $ not $ null $ batchCandidates $ resultSearch contextualResult

      impredicativeTargetName <- expectRight
        $ mkIdentifier "impredicativeIdentity"
      impredicativeTarget <- expectRight
        $ mkDefinitionName impredicativeTargetName
      let impredicativeQuery = nestedQuery
            { requestTarget = impredicativeTarget
            , requestGoal = FunctionType
                (listOf $ polymorphic "element")
                (listOf $ polymorphic "renamed")
            }
      impredicativeRequest <- expectRight $ mkDjinnRequest impredicativeQuery
      impredicativeResult <- expectRight
        $ runDjinnQuery session impredicativeRequest
      resultEvidence impredicativeResult @?= ValidatedCandidates
      assertBool "impredicative list identity produced no proof"
        $ not $ null $ batchCandidates $ resultSearch impredicativeResult
  , testCase "classify Djinn options without attributing type source" $ do
      session <- sealDjinnEnvironment standardEnvironment
      target <- expectRight $ mkIdentifier "invalidOptions"
      let options = defaultQueryOptions {optionCutoff = 0}
      parsed <- expectRight $ parseDjinnRequest session options target
        "invalid-options.djinn" "a -> a"
      programmatic <- expectRight $ mkDjinnRequest $ djinnRequestQuery parsed
      parsed @?= programmatic
      parsedFailure <- case runDjinnQuery session parsed of
        Left failure -> pure failure
        Right _ -> fail "Djinn accepted a parsed zero candidate cutoff"
      programmaticFailure <- case runDjinnQuery session programmatic of
        Left failure -> pure failure
        Right _ -> fail "Djinn accepted a programmatic zero candidate cutoff"
      diagnosticCode parsedFailure @?= Just "DJEX_DJINN_OPTIONS"
      diagnosticContext parsedFailure @?=
        ["NonPositiveCandidateCutoff 0"]
      diagnosticSource parsedFailure @?= Nothing
      diagnosticSpan parsedFailure @?= Nothing
      parsedFailure @?= programmaticFailure
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
      -- The class argument @f@ collides with the method-local @f@. The query
      -- still validates that context without admitting its instantiated method
      -- as a hidden proof premise.
      captureGoal <- expectRight $ parseHType "f' f"
      assertDjinnCompatibility "capture-safe context validation"
        captureEnvironment captureSession [captureContext]
        defaultQueryOptions "captureSafePrepared" captureGoal
      captureTarget <- expectRight $ mkIdentifier "captureSafePrepared"
      captureRequest <- sharedDjinnRequest captureTarget [captureContext]
        defaultQueryOptions captureGoal
      captureResult <- expectRight $
        runDjinnQuery captureSession captureRequest
      batchCandidates (resultSearch captureResult) @?= []
      resultEvidence captureResult @?= ProvedUninhabitable

      -- Use an otherwise uninhabited nominal result so the operator method
      -- would be essential. Dictionary-independent search must reject it.
      tokenType <- expectRight $ parseHType "TokenPrepared"
      operatorMethod <- expectRight $ parseHType "a -> ProofPrepared"
      operatorEnvironment <- expectRight $ do
        withToken <- declare
          (DataType "TokenPrepared" [] [("TokenPrepared", [])])
          emptyEnvironment
        withProof <- declare
          (DataType "ProofPrepared" [] []) withToken
        declare (ClassDecl "EqualPrepared" ["a"]
          [("==", operatorMethod)]) withProof
      operatorSession <- sealDjinnEnvironment operatorEnvironment
      operatorContext <- expectRight $
        mkContext "EqualPrepared" [tokenType]
      operatorGoal <- expectRight $
        parseHType "TokenPrepared -> ProofPrepared"
      operatorTarget <- expectRight $ mkIdentifier "operatorPrepared"
      checkedOperatorTarget <- expectRight $
        mkDefinitionName operatorTarget
      withoutContext <- expectRight $ inhabitResult
        defaultQueryOptions operatorEnvironment []
        checkedOperatorTarget operatorGoal
      batchCandidates (resultSearch withoutContext) @?= []
      assertDjinnCompatibility "essential operator method"
        operatorEnvironment operatorSession [operatorContext]
        defaultQueryOptions "operatorPrepared" operatorGoal
      operatorRequest <- sharedDjinnRequest operatorTarget [operatorContext]
        defaultQueryOptions operatorGoal
      operatorResult <- expectRight $
        runDjinnQuery operatorSession operatorRequest
      batchCandidates (resultSearch operatorResult) @?= []
      resultEvidence operatorResult @?= ProvedUninhabitable

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
  , testCase "render closed visible instantiation accepted by GHC 9.12" $ do
      targetName <- expectRight $ mkIdentifier "use"
      checkedTarget <- expectRight $ mkDefinitionName targetName
      integerName <- expectRight $ mkIdentifier "Int"
      integerArgument <- expectRight $ specifiedVisibleTypeArgument
        (TypeConstructor integerName :: Type String)
      let instantiatedProvider = VisibleTypeApplication
            (Local "provider") integerArgument
          clause = FunctionClause checkedTarget [Bind "provider"]
            instantiatedProvider
          renderOptions =
            (defaultRenderOptions id) {renderQualification = Unqualified}
      renderExpression renderOptions
          (Lambda [Bind "provider"] instantiatedProvider) @?=
        Right "\\provider -> provider @Int"
      rendered <- expectRight $ renderFunctionClause renderOptions clause
      rendered @?= "use provider = provider @Int"

      (versionExit, versionOutput, versionErrors) <-
        readProcessWithExitCode "ghc" ["--numeric-version"] ""
      assertEqual
        ("cannot inspect installed GHC version: " ++ versionErrors)
        ExitSuccess versionExit
      assertBool
        ("integration fixture requires GHC 9.12, got " ++ show versionOutput)
        $ "9.12." `isPrefixOf` versionOutput

      let fixture = unlines
            [ "module VisibleTypeApplicationFixture where"
            , ""
            , "data Token = Token"
            , ""
            , "class C a"
            , ""
            , "instance C Int"
            , ""
            , "use :: (forall a. C a => Token) -> Token"
            , rendered
            ]
      withTemporaryHaskellModule fixture $ \sourcePath -> do
        (exitCode, output, errors) <- readProcessWithExitCode "ghc"
          [ "-v0"
          , "-fforce-recomp"
          , "-fno-code"
          , "-fno-write-interface"
          , "-XHaskell2010"
          , "-XAllowAmbiguousTypes"
          , "-XRankNTypes"
          , "-XTypeApplications"
          , sourcePath
          ] ""
        assertEqual
          ("GHC rejected generated visible type application\nstdout:\n"
            ++ output ++ "\nstderr:\n" ++ errors)
          ExitSuccess exitCode
  , testCase "compile query-local Djinn instantiation at a closed monotype" $ do
      closedName <- expectRight $ parseName "MonoClosed"
      tokenName <- expectRight $ parseName "MonoToken"
      indexedName <- expectRight $ parseName "MonoIndexed"
      targetName <- expectRight $ mkIdentifier "useQueryLocalMono"
      let proper = ProperTypeKind
          declarations =
            [ AbstractTypeDeclaration () closedName proper
            , AbstractTypeDeclaration () tokenName proper
            , AbstractTypeDeclaration () indexedName $
                FunctionKind proper proper
            ]
          query =
            "(forall a. (a -> MonoToken) -> a -> MonoIndexed a) -> "
              ++ "(MonoClosed -> MonoToken) -> MonoClosed -> "
              ++ "MonoIndexed MonoClosed"
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError DjinnTypeVariable) DjinnEnvironment)
      session <- expectRight $ mkDjinnSession environment
      request <- expectRight $ parseDjinnRequest session
        defaultQueryOptions targetName "query-local-closed.djinn" query
      result <- expectRight $ runDjinnQuery session request
      resultEvidence result @?= ValidatedCandidates
      -- Returning the rank-N local is its implicit use at MonoClosed: the
      -- remaining result arrows and indexed result fix that monotype.
      let usesLocalProvider candidate = case candidateOutput candidate of
            FunctionClause actualTarget [Bind provider]
                (Local usedProvider) ->
              actualTarget == requestTarget (djinnRequestQuery request)
                && usedProvider == provider
            _ -> False
      candidate <- case filter usesLocalProvider
          (batchCandidates $ resultSearch result) of
        selected : _ -> pure selected
        [] -> fail $ "Djinn did not use the query-local provider at "
          ++ "MonoClosed: " ++ show (batchCandidates $ resultSearch result)
      generated <- expectRight $
        renderDjinnCandidateDefinition Unqualified candidate
      generated @?= "useQueryLocalMono a = a"

      let fixture = unlines
            [ "module QueryLocalClosedDjinnFixture where"
            , ""
            , "data MonoClosed"
            , "data MonoToken"
            , "data MonoIndexed a"
            , ""
            , "useQueryLocalMono ::"
            , "  (forall a. (a -> MonoToken) -> a -> MonoIndexed a) ->"
            , "  (MonoClosed -> MonoToken) -> MonoClosed ->"
            , "  MonoIndexed MonoClosed"
            , generated
            ]
      withTemporaryHaskellModule fixture $ \sourcePath -> do
        (exitCode, output, errors) <- readProcessWithExitCode "ghc"
          [ "-v0"
          , "-fforce-recomp"
          , "-fno-code"
          , "-fno-write-interface"
          , "-XHaskell2010"
          , "-XEmptyDataDecls"
          , "-XImpredicativeTypes"
          , "-XRankNTypes"
          , sourcePath
          ] ""
        assertEqual
          ("GHC rejected query-local closed-monotype Djinn evidence\nstdout:\n"
            ++ output ++ "\nstderr:\n" ++ errors
            ++ "\ngenerated:\n" ++ generated)
          ExitSuccess exitCode
  , testCase "render nominal Djinn transport accepted by GHC 9.12" $ do
      dataName <- expectRight $ parseName "MaybeLike"
      nothingName <- expectRight $ parseName "NothingLike"
      justName <- expectRight $ parseName "JustLike"
      resultName <- expectRight $ parseName "NominalResult"
      finishName <- expectRight $ parseName "finish"
      targetName <- expectRight $ mkIdentifier "finishNominalMaybeLike"
      target <- expectRight $ mkDefinitionName targetName
      let parameter = TypeParameter "parameter" Nothing
          parameterType = TypeVariable "parameter"
          maybeType element = TypeApplication
            (TypeConstructor dataName) element
          polymorphic binder = ForallType [binder] [] $
            FunctionType (TypeVariable binder) (TypeVariable binder)
          resultType = TypeConstructor resultName
          declarations =
            [ AbstractTypeDeclaration () resultName ProperTypeKind
            , DataTypeDeclaration () dataName [parameter]
                [ DataConstructor () nothingName []
                , DataConstructor () justName [parameterType]
                ]
            , ValueDeclaration $ ValueSignature () finishName $
                FunctionType (maybeType $ polymorphic "accepted") resultType
            ]
          goal = FunctionType
            (ForallType ["supplied"] [] $
              maybeType $ TypeVariable "supplied")
            resultType
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError DjinnTypeVariable) DjinnEnvironment)
      session <- expectRight $ mkDjinnSession environment
      request <- expectRight $ mkDjinnRequest QueryRequest
        { requestTarget = target
        , requestGoal = goal
        , requestContexts = []
        , requestOptions = defaultQueryOptions
        }
      result <- expectRight $ runDjinnQuery session request
      rendered <- traverse
        (expectRight . renderDjinnCandidateDefinition Unqualified)
        $ batchCandidates $ resultSearch result
      let expected = "finishNominalMaybeLike a = finish a"
      generated <- case filter (== expected) rendered of
        candidate : _ -> pure candidate
        [] -> fail $ "Djinn omitted required eta expansion: " ++ show rendered

      let fixture = unlines
            [ "module NominalDjinnTransportFixture where"
            , ""
            , "data MaybeLike a = NothingLike | JustLike a"
            , ""
            , "data NominalResult = NominalResult"
            , ""
            , "finish :: MaybeLike (forall a. a -> a) -> NominalResult"
            , "finish _ = NominalResult"
            , ""
            , "finishNominalMaybeLike ::"
            , "  (forall a. MaybeLike a) -> NominalResult"
            , generated
            ]
      withTemporaryHaskellModule fixture $ \sourcePath -> do
        (exitCode, output, errors) <- readProcessWithExitCode "ghc"
          [ "-v0"
          , "-fforce-recomp"
          , "-fno-code"
          , "-fno-write-interface"
          , "-XHaskell2010"
          , "-XRankNTypes"
          , "-XImpredicativeTypes"
          , sourcePath
          ] ""
        assertEqual
          ("GHC rejected generated nominal Djinn transport\nstdout:\n"
            ++ output ++ "\nstderr:\n" ++ errors)
          ExitSuccess exitCode
  , testCase "compile a loaded Djinn scheme used at two monotypes" $ do
      leftName <- expectRight $ parseName "LoadedLeft"
      rightName <- expectRight $ parseName "LoadedRight"
      familyName <- expectRight $ parseName "LoadedFamily"
      resultName <- expectRight $ parseName "LoadedResult"
      leftValueName <- expectRight $ parseName "loadedLeft"
      rightValueName <- expectRight $ parseName "loadedRight"
      makeName <- expectRight $ parseName "loadedMake"
      finishName <- expectRight $ parseName "loadedFinish"
      targetName <- expectRight $ mkIdentifier "buildLoadedPair"
      target <- expectRight $ mkDefinitionName targetName
      let proper = ProperTypeKind
          unary = FunctionKind proper proper
          leftType = TypeConstructor leftName
          rightType = TypeConstructor rightName
          resultType = TypeConstructor resultName
          familyType element = TypeApplication
            (TypeConstructor familyName) element
          makeType = ForallType ["made"] [] $
            FunctionType (TypeVariable "made") $
              familyType $ TypeVariable "made"
          finishType = FunctionType (familyType leftType) $
            FunctionType (familyType rightType) resultType
          value name signatureType = ValueDeclaration $
            ValueSignature () name signatureType
          declarations =
            [ AbstractTypeDeclaration () leftName proper
            , AbstractTypeDeclaration () rightName proper
            , AbstractTypeDeclaration () familyName unary
            , AbstractTypeDeclaration () resultName proper
            , value leftValueName leftType
            , value rightValueName rightType
            , value makeName makeType
            , value finishName finishType
            ]
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError DjinnTypeVariable) DjinnEnvironment)
      session <- expectRight $ mkDjinnSession environment
      request <- expectRight $ mkDjinnRequest QueryRequest
        { requestTarget = target
        , requestGoal = resultType
        , requestContexts = []
        , requestOptions = defaultQueryOptions
        }
      result <- expectRight $ runDjinnQuery session request
      rendered <- traverse
        (expectRight . renderDjinnCandidateDefinition Unqualified)
        $ batchCandidates $ resultSearch result
      let isTwoInstanceTerm candidate =
            all (`isInfixOf` candidate)
              ["loadedFinish", "loadedLeft", "loadedRight"] &&
            length
              [ ()
              | suffix <- tails candidate
              , "loadedMake" `isPrefixOf` suffix
              ] >= 2
      generated <- case filter isTwoInstanceTerm rendered of
        candidate : _ -> pure candidate
        [] -> fail $ "Djinn omitted the two-instance term: " ++ show rendered

      let fixture = unlines
            [ "{-# LANGUAGE RankNTypes #-}"
            , "module LoadedDjinnInstantiationFixture where"
            , ""
            , "data LoadedLeft = LoadedLeft"
            , "data LoadedRight = LoadedRight"
            , "newtype LoadedFamily a = LoadedFamily a"
            , "data LoadedResult = LoadedResult"
            , ""
            , "loadedLeft :: LoadedLeft"
            , "loadedLeft = LoadedLeft"
            , ""
            , "loadedRight :: LoadedRight"
            , "loadedRight = LoadedRight"
            , ""
            , "loadedMake :: forall a. a -> LoadedFamily a"
            , "loadedMake = LoadedFamily"
            , ""
            , "loadedFinish ::"
            , "  LoadedFamily LoadedLeft ->"
            , "  LoadedFamily LoadedRight -> LoadedResult"
            , "loadedFinish _ _ = LoadedResult"
            , ""
            , "buildLoadedPair :: LoadedResult"
            , generated
            ]
      withTemporaryHaskellModule fixture $ \sourcePath -> do
        (exitCode, output, errors) <- readProcessWithExitCode "ghc"
          [ "-v0"
          , "-fforce-recomp"
          , "-fno-code"
          , "-fno-write-interface"
          , "-XHaskell2010"
          , "-XRankNTypes"
          , sourcePath
          ] ""
        assertEqual
          ("GHC rejected a twice-instantiated loaded Djinn value\nstdout:\n"
            ++ output ++ "\nstderr:\n" ++ errors)
          ExitSuccess exitCode
  , testCase "compile explicit evidence for an ambiguous Djinn provider" $ do
      seedName <- expectRight $ parseName "AmbiguousSeed"
      tokenName <- expectRight $ parseName "AmbiguousToken"
      providerName <- expectRight $ parseName "ambiguousToken"
      targetName <- expectRight $ mkIdentifier "useAmbiguousToken"
      target <- expectRight $ mkDefinitionName targetName
      let seedType = TypeConstructor seedName
          tokenType = TypeConstructor tokenName
          providerType = ForallType ["chosen"] [] tokenType
          declarations =
            [ AbstractTypeDeclaration () seedName ProperTypeKind
            , AbstractTypeDeclaration () tokenName ProperTypeKind
            , ValueDeclaration $ ValueSignature () providerName providerType
            ]
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError DjinnTypeVariable) DjinnEnvironment)
      session <- expectRight $ mkDjinnSession environment
      request <- expectRight $ mkDjinnRequest QueryRequest
        { requestTarget = target
        , requestGoal = FunctionType seedType tokenType
        , requestContexts = []
        , requestOptions = defaultQueryOptions
        }
      result <- expectRight $ runDjinnQuery session request
      rendered <- traverse
        (expectRight . renderDjinnCandidateDefinition Unqualified)
        $ batchCandidates $ resultSearch result
      generated <- case filter
          ("ambiguousToken @AmbiguousSeed" `isInfixOf`) rendered of
        candidate : _ -> pure candidate
        [] -> fail $ "Djinn erased the provider's selected type: "
          ++ show rendered

      let fixture = unlines
            [ "module AmbiguousDjinnProviderFixture where"
            , ""
            , "data AmbiguousSeed = AmbiguousSeed"
            , "data AmbiguousToken = AmbiguousToken"
            , ""
            , "ambiguousToken :: forall a. AmbiguousToken"
            , "ambiguousToken = AmbiguousToken"
            , ""
            , "useAmbiguousToken :: AmbiguousSeed -> AmbiguousToken"
            , generated
            ]
      withTemporaryHaskellModule fixture $ \sourcePath -> do
        (exitCode, output, errors) <- readProcessWithExitCode "ghc"
          [ "-v0"
          , "-fforce-recomp"
          , "-fno-code"
          , "-fno-write-interface"
          , "-XHaskell2010"
          , "-XAllowAmbiguousTypes"
          , "-XRankNTypes"
          , "-XTypeApplications"
          , sourcePath
          ] ""
        assertEqual
          ("GHC rejected explicit ambiguous-provider evidence\nstdout:\n"
            ++ output ++ "\nstderr:\n" ++ errors
            ++ "\ngenerated:\n" ++ generated)
          ExitSuccess exitCode
  , testCase "compile quantified evidence for an ambiguous Djinn provider" $ do
      tokenName <- expectRight $ parseName "QuantifiedToken"
      providerName <- expectRight $ parseName "ambiguousQuantifiedToken"
      targetName <- expectRight $
        mkIdentifier "useQuantifiedAmbiguousToken"
      target <- expectRight $ mkDefinitionName targetName
      let tokenType = TypeConstructor tokenName
          quantifiedIdentity = ForallType ["identity"] [] $
            FunctionType
              (TypeVariable "identity")
              (TypeVariable "identity")
          providerType = ForallType ["chosen"] [] tokenType
          declarations =
            [ AbstractTypeDeclaration () tokenName ProperTypeKind
            , ValueDeclaration $ ValueSignature () providerName providerType
            ]
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError DjinnTypeVariable) DjinnEnvironment)
      session <- expectRight $ mkDjinnSession environment
      request <- expectRight $ mkDjinnRequest QueryRequest
        { requestTarget = target
        , requestGoal = FunctionType quantifiedIdentity tokenType
        , requestContexts = []
        , requestOptions = defaultQueryOptions
        }
      result <- expectRight $ runDjinnQuery session request
      rendered <- traverse
        (expectRight . renderDjinnCandidateDefinition Unqualified)
        $ batchCandidates $ resultSearch result
      generated <- case filter
          ("ambiguousQuantifiedToken @(forall a0_0. a0_0 -> a0_0)"
            `isInfixOf`) rendered of
        candidate : _ -> pure candidate
        [] -> fail $ "Djinn downgraded the selected quantified type: "
          ++ show rendered

      let fixture = unlines
            [ "module QuantifiedDjinnProviderFixture where"
            , ""
            , "data QuantifiedToken = QuantifiedToken"
            , ""
            , "ambiguousQuantifiedToken :: forall a. QuantifiedToken"
            , "ambiguousQuantifiedToken = QuantifiedToken"
            , ""
            , "useQuantifiedAmbiguousToken ::"
            , "  (forall x. x -> x) -> QuantifiedToken"
            , generated
            ]
      withTemporaryHaskellModule fixture $ \sourcePath -> do
        (exitCode, output, errors) <- readProcessWithExitCode "ghc"
          [ "-v0"
          , "-fforce-recomp"
          , "-fno-code"
          , "-fno-write-interface"
          , "-XHaskell2010"
          , "-XAllowAmbiguousTypes"
          , "-XImpredicativeTypes"
          , "-XRankNTypes"
          , "-XTypeApplications"
          , sourcePath
          ] ""
        assertEqual
          ("GHC rejected quantified ambiguous-provider evidence\nstdout:\n"
            ++ output ++ "\nstderr:\n" ++ errors
            ++ "\ngenerated:\n" ++ generated)
          ExitSuccess exitCode
  , testCase "compile a mixed-kind visible Djinn prefix" $ do
      higherConstructorName <- expectRight $
        parseName "MixedKindConstructor"
      argumentName <- expectRight $ parseName "MixedKindArgument"
      tokenName <- expectRight $ parseName "MixedKindToken"
      providerName <- expectRight $ parseName "mixedKindProvider"
      targetName <- expectRight $ mkIdentifier "useMixedKindProvider"
      target <- expectRight $ mkDefinitionName targetName
      let constructorType = TypeConstructor higherConstructorName
          argumentType = TypeConstructor argumentName
          tokenType = TypeConstructor tokenName
          appliedType = TypeApplication constructorType argumentType
          providerType = ForallType ["f", "a", "hidden"] [] $
            FunctionType
              (TypeApplication (TypeVariable "f") (TypeVariable "a"))
              tokenType
          declarations =
            [ AbstractTypeDeclaration () higherConstructorName $
                FunctionKind ProperTypeKind ProperTypeKind
            , AbstractTypeDeclaration () argumentName ProperTypeKind
            , AbstractTypeDeclaration () tokenName ProperTypeKind
            , ValueDeclaration $ ValueSignature () providerName providerType
            ]
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError DjinnTypeVariable) DjinnEnvironment)
      session <- expectRight $ mkDjinnSession environment
      request <- expectRight $ mkDjinnRequest QueryRequest
        { requestTarget = target
        , requestGoal = FunctionType appliedType tokenType
        , requestContexts = []
        , requestOptions = defaultQueryOptions
        }
      result <- expectRight $ runDjinnQuery session request
      rendered <- traverse
        (expectRight . renderDjinnCandidateDefinition Unqualified)
        $ batchCandidates $ resultSearch result
      generated <- case filter
          ("mixedKindProvider @_ @_ @" `isInfixOf`) rendered of
        candidate : _ -> pure candidate
        [] -> fail $ "Djinn discarded the higher-kinded visible prefix: "
          ++ show rendered

      let fixture = unlines
            [ "module MixedKindDjinnProviderFixture where"
            , ""
            , "data MixedKindConstructor a = MixedKindConstructor a"
            , "data MixedKindArgument = MixedKindArgument"
            , "data MixedKindToken = MixedKindToken"
            , ""
            , "mixedKindProvider :: forall f a hidden. f a -> MixedKindToken"
            , "mixedKindProvider _ = MixedKindToken"
            , ""
            , "useMixedKindProvider ::"
            , "  MixedKindConstructor MixedKindArgument -> MixedKindToken"
            , generated
            ]
      withTemporaryHaskellModule fixture $ \sourcePath -> do
        (exitCode, output, errors) <- readProcessWithExitCode "ghc"
          [ "-v0"
          , "-fforce-recomp"
          , "-fno-code"
          , "-fno-write-interface"
          , "-XHaskell2010"
          , "-XAllowAmbiguousTypes"
          , "-XRankNTypes"
          , "-XTypeApplications"
          , sourcePath
          ] ""
        assertEqual
          ("GHC rejected a mixed-kind visible prefix\nstdout:\n"
            ++ output ++ "\nstderr:\n" ++ errors
            ++ "\ngenerated:\n" ++ generated)
          ExitSuccess exitCode
  , testCase "retain nominal eta through selector presentation for GHC" $ do
      dataName <- expectRight $ parseName "SelectorData"
      dataConstructorName <- expectRight $ parseName "SelectorDataValue"
      holderName <- expectRight $ parseName "SelectorHolder"
      holderConstructorName <- expectRight $ parseName "SelectorHolderValue"
      selectorName <- expectRight $ parseName "selectorField"
      resultName <- expectRight $ parseName "SelectorResult"
      targetName <- expectRight $ mkIdentifier "finishThroughSelector"
      target <- expectRight $ mkDefinitionName targetName
      let parameter = TypeParameter "parameter" Nothing
          parameterType = TypeVariable "parameter"
          dataType element = TypeApplication
            (TypeConstructor dataName) element
          polymorphic binder = ForallType [binder] [] $
            FunctionType (TypeVariable binder) (TypeVariable binder)
          resultType = TypeConstructor resultName
          holderType = TypeConstructor holderName
          fieldType = FunctionType
            (dataType $ polymorphic "accepted") resultType
          declarations =
            [ AbstractTypeDeclaration () resultName ProperTypeKind
            , DataTypeDeclaration () dataName [parameter]
                [DataConstructor () dataConstructorName [parameterType]]
            , DataTypeDeclaration () holderName []
                [DataConstructor () holderConstructorName [fieldType]]
            ]
          goal = FunctionType holderType $
            FunctionType
              (ForallType ["supplied"] [] $
                dataType $ TypeVariable "supplied")
              resultType
          selectors = Map.singleton (holderConstructorName, 0) selectorName
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError DjinnTypeVariable) DjinnEnvironment)
      session <- expectRight $ mkDjinnSession environment
      request <- expectRight $ mkDjinnRequest QueryRequest
        { requestTarget = target
        , requestGoal = goal
        , requestContexts = []
        , requestOptions = defaultQueryOptions
        }
      result <- expectRight $ runDjinnQuery session request
      rendered <- traverse
        (expectRight . renderDjinnCandidateDefinition Unqualified
          . fmap (projectFieldSelectorsWithoutEta selectors))
        $ batchCandidates $ resultSearch result
      let expected =
            "finishThroughSelector a b = selectorField a b"
      generated <- case filter (== expected) rendered of
        candidate : _ -> pure candidate
        [] -> fail $
          "selector presentation omitted protected eta expansion: "
            ++ show rendered

      let fixture = unlines
            [ "module NominalSelectorFixture where"
            , ""
            , "data SelectorData a = SelectorDataValue a"
            , ""
            , "data SelectorResult = SelectorResult"
            , ""
            , "data SelectorHolder = SelectorHolderValue"
            , "  { selectorField ::"
            , "      SelectorData (forall a. a -> a) -> SelectorResult"
            , "  }"
            , ""
            , "finishThroughSelector ::"
            , "  SelectorHolder ->"
            , "  (forall a. SelectorData a) -> SelectorResult"
            , generated
            ]
      withTemporaryHaskellModule fixture $ \sourcePath -> do
        (exitCode, output, errors) <- readProcessWithExitCode "ghc"
          [ "-v0"
          , "-fforce-recomp"
          , "-fno-code"
          , "-fno-write-interface"
          , "-XHaskell2010"
          , "-XRankNTypes"
          , "-XImpredicativeTypes"
          , sourcePath
          ] ""
        assertEqual
          ("GHC rejected selector-presented nominal transport\nstdout:\n"
            ++ output ++ "\nstderr:\n" ++ errors)
          ExitSuccess exitCode
  , testCase "compile rank-N fields eliminated from a loaded product" $ do
      inputName <- expectRight $ parseName "StructuredInput"
      resultName <- expectRight $ parseName "StructuredResult"
      sourceName <- expectRight $ parseName "structuredSource"
      djinnTargetName <- expectRight $ mkIdentifier "structuredDjinn"
      exferenceTargetName <- expectRight $
        mkIdentifier "structuredExference"
      djinnTarget <- expectRight $ mkDefinitionName djinnTargetName
      exferenceTarget <- expectRight $
        mkDefinitionName exferenceTargetName
      let inputType = TypeConstructor inputName
          resultType = TypeConstructor resultName
          djinnVariable = TypeVariable "element"
          djinnProvider = ForallType ["element"] [] $
            FunctionType djinnVariable resultType
          djinnSource = TupleType Boxed
            [djinnProvider, TupleType Boxed []]
          djinnDeclarations =
            [ AbstractTypeDeclaration () inputName ProperTypeKind
            , AbstractTypeDeclaration () resultName ProperTypeKind
            , ValueDeclaration $ ValueSignature () sourceName djinnSource
            ]
      djinnEnvironment <- expectRight
        (mkEnvironment djinnDeclarations :: Either
          (EnvironmentError DjinnTypeVariable) DjinnEnvironment)
      djinnSession <- expectRight $ mkDjinnSession djinnEnvironment
      djinnRequest <- expectRight $ mkDjinnRequest QueryRequest
        { requestTarget = djinnTarget
        , requestGoal = FunctionType inputType resultType
        , requestContexts = []
        , requestOptions = defaultQueryOptions
            { optionAlternatives = True
            , optionSorted = False
            , optionCutoff = 20
            }
        }
      djinnResult <- expectRight $ runDjinnQuery djinnSession djinnRequest
      djinnCandidate <- maybe
        (fail "Djinn did not eliminate the loaded product")
        pure
        $ find (candidateEliminatesProduct sourceName)
        $ batchCandidates $ resultSearch djinnResult
      djinnGenerated <- expectRight $
        renderDjinnCandidateDefinition Unqualified djinnCandidate

      let exferenceVariable = FlexibleVariable 0
          exferenceVariableType = TypeVariable exferenceVariable
          exferenceProvider = ForallType [exferenceVariable] [] $
            FunctionType exferenceVariableType resultType
          exferenceSource = TupleType Boxed
            [exferenceProvider, TupleType Boxed []]
          exferenceDeclarations =
            [ AbstractTypeDeclaration () inputName ProperTypeKind
            , AbstractTypeDeclaration () resultName ProperTypeKind
            , ValueDeclaration $ ValueSignature () sourceName exferenceSource
            ]
      exferenceEnvironment <- expectRight
        (mkEnvironment exferenceDeclarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      exferenceSession <- expectRight $
        mkExferenceSession exferenceEnvironment
      exferenceRequest <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = exferenceTarget
        , requestGoal = FunctionType inputType resultType
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            { exferenceAllowUnused = True
            , exferenceMaximumSteps = 512
            , exferenceMaximumQueueSize = Just 256
            }
        }
      exferenceResults <- expectRight $
        runExferenceQuery exferenceSession exferenceRequest
      exferenceCandidate <- maybe
        (fail "Exference did not eliminate the loaded product")
        pure
        $ find (candidateEliminatesProduct sourceName)
        $ concatMap (batchCandidates . resultSearch) exferenceResults
      exferenceGenerated <- expectRight $
        renderExferenceCandidateDefinition Unqualified exferenceCandidate

      let fixture = unlines
            [ "module LoadedProductEliminationFixture where"
            , ""
            , "data StructuredInput = StructuredInput"
            , "data StructuredResult = StructuredResult"
            , ""
            , "structuredSource ::"
            , "  (forall a. a -> StructuredResult, ())"
            , "structuredSource = (const StructuredResult, ())"
            , ""
            , "structuredDjinn :: StructuredInput -> StructuredResult"
            , djinnGenerated
            , ""
            , "structuredExference :: StructuredInput -> StructuredResult"
            , exferenceGenerated
            ]
      withTemporaryHaskellModule fixture $ \sourcePath -> do
        (exitCode, output, errors) <- readProcessWithExitCode "ghc"
          [ "-v0"
          , "-fforce-recomp"
          , "-fno-code"
          , "-fno-write-interface"
          , "-XHaskell2010"
          , "-XRankNTypes"
          , "-XImpredicativeTypes"
          , sourcePath
          ] ""
        assertEqual
          ("GHC rejected loaded-product rank-N elimination\nstdout:\n"
            ++ output ++ "\nstderr:\n" ++ errors
            ++ "\nDjinn generated:\n" ++ djinnGenerated
            ++ "\nExference generated:\n" ++ exferenceGenerated)
          ExitSuccess exitCode
  , testCase "eliminate a product exposed only by provider assignment" $ do
      inputName <- expectRight $ parseName "AssignedInput"
      resultName <- expectRight $ parseName "AssignedResult"
      sourceName <- expectRight $ parseName "assignedProductSource"
      targetName <- expectRight $ mkIdentifier "assignedProductElimination"
      target <- expectRight $ mkDefinitionName targetName
      let providerVariable = FlexibleVariable 0
          fieldVariable = FlexibleVariable 1
          inputType = TypeConstructor inputName
          resultType = TypeConstructor resultName
          fieldType = ForallType [fieldVariable] [] $
            FunctionType (TypeVariable fieldVariable) resultType
          assignedProduct = TupleType Boxed
            [fieldType, TupleType Boxed []]
          providerType = ForallType [providerVariable] [] $
            TypeVariable providerVariable
          declarations =
            [ AbstractTypeDeclaration () inputName ProperTypeKind
            , AbstractTypeDeclaration () resultName ProperTypeKind
            , ValueDeclaration $ ValueSignature () sourceName providerType
            ]
          assignment = ProviderInstantiationAssignment
            { providerInstantiationAssignmentProvider = sourceName
            , providerInstantiationAssignmentArguments = [assignedProduct]
            }
      assignedArgument <- expectRight $
        specifiedVisibleTypeArgument assignedProduct
      let referencesAssignedSource expression = case expression of
            Local _ -> False
            Global _ -> False
            Lambda _ body -> referencesAssignedSource body
            Apply function argument ->
              referencesAssignedSource function ||
                referencesAssignedSource argument
            VisibleTypeApplication (Global occurrence) argument ->
              occurrence == sourceName && argument == assignedArgument
            VisibleTypeApplication function _ ->
              referencesAssignedSource function
            Tuple elements -> any referencesAssignedSource elements
            Hole _ -> False
            Let _ value body ->
              referencesAssignedSource value || referencesAssignedSource body
            Case scrutinee alternatives ->
              referencesAssignedSource scrutinee ||
                any (referencesAssignedSource . snd) alternatives
          appliesLocalField expression = case expression of
            Apply (Local _) (Local _) -> True
            Local _ -> False
            Global _ -> False
            Lambda _ body -> appliesLocalField body
            Apply function argument ->
              appliesLocalField function || appliesLocalField argument
            VisibleTypeApplication function _ -> appliesLocalField function
            Tuple elements -> any appliesLocalField elements
            Hole _ -> False
            Let _ value body ->
              appliesLocalField value || appliesLocalField body
            Case scrutinee alternatives ->
              appliesLocalField scrutinee ||
                any (appliesLocalField . snd) alternatives
          isAssignedElimination candidate =
            candidateEliminatesProduct sourceName candidate &&
              case candidateOutput candidate of
                FunctionClause _ _ body ->
                  referencesAssignedSource body && appliesLocalField body
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = target
        , requestGoal = FunctionType inputType resultType
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            { exferenceAllowUnused = True
            , exferenceMaximumSteps = 128
            , exferenceMaximumQueueSize = Just 128
            }
        }
      results <- expectRight $ runExferenceQueryWithInstantiationAssignments
        session [assignment] request
      let candidates = concatMap
            (batchCandidates . resultSearch) results
      _ <- maybe
        (fail "the assigned rank-N product was not visibly eliminated")
        pure
        $ find isAssignedElimination candidates
      pure ()
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
              FunctionClause actualTarget patterns body -> do
                actualTarget @?=
                  requestTarget (exferenceRequestQuery request)
                definitionName actualTarget @?= target
                case (patterns, body) of
                  ([Bind binder], Local occurrence) -> binder @?= occurrence
                  _ -> fail $ "identity lambda was not promoted to a clause: "
                    ++ show (patterns, body)
                exferenceCandidateMetrics candidate @?=
                  exferenceCandidateStatistics (candidateDetails candidate)
            [] -> fail "Exference reported candidate evidence without a candidate"
          let metadata = batchMetadata $ resultSearch result
          exferenceResultBindingUsages result @?=
            exferenceBatchBindingUsages metadata
        [] -> fail "Exference found no identity candidate"
  , testCase "introduce nested Exference foralls through the stable facade" $ do
      checked <- expectRight $ checkSourceEnvironment emptyExferenceSource
      session <- expectRight $ ExferenceCompatibility.mkExferenceSession checked
      target <- expectRight $ mkIdentifier "rankNCallback"
      request <- expectRight $ parseExferenceRequest session
        defaultExferenceOptions {exferenceMaximumSteps = 200}
        target
        "rank-n-introduction"
        "((forall a. a -> a) -> result) -> result"
      candidate <- firstExferenceCandidate =<< expectRight
        (runExferenceQuery session request)
      definitionName (clauseName $ candidateOutput candidate) @?= target
  , testCase "consume nested Exference contexts through the stable facade" $ do
      className <- expectRight $ mkIdentifier "C"
      tokenName <- expectRight $ mkIdentifier "ContextToken"
      resultName <- expectRight $ mkIdentifier "ContextResult"
      consumeName <- expectRight $ parseName "Fixture.consumeContext"
      methodName <- expectRight $ parseName "Fixture.contextMethod"
      target <- expectRight $ mkIdentifier "contextualCallback"
      let outerVariable = FlexibleVariable 0
          nestedVariable = FlexibleVariable 1
          outerType = TypeVariable outerVariable
          nestedType = TypeVariable nestedVariable
          tokenType = TypeConstructor tokenName
          resultType = TypeConstructor resultName
          contextual = ForallType [nestedVariable]
            [Constraint className [nestedType]]
            $ FunctionType nestedType tokenType
          declarations =
            [ AbstractTypeDeclaration () tokenName ProperTypeKind
            , AbstractTypeDeclaration () resultName ProperTypeKind
            , ClassDeclaration () className
                [TypeParameter outerVariable Nothing] [] []
            , ValueDeclaration $ ValueSignature () consumeName
                $ FunctionType contextual resultType
            , ValueDeclaration $ ValueSignature () methodName
                $ ForallType [outerVariable]
                    [Constraint className [outerType]]
                    $ FunctionType outerType tokenType
            ]
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ parseExferenceRequest session
        defaultExferenceOptions {exferenceMaximumSteps = 500}
        target "contextual-rank-n" "ContextResult"
      candidate <- firstExferenceCandidate =<< expectRight
        (runExferenceQuery session request)
      candidateResidualConstraints candidate @?= []
      case candidateOutput candidate of
        FunctionClause _ _ body -> do
          assertBool "contextual callback did not use its consumer"
            $ referencesGlobal consumeName body
          assertBool "nested class evidence did not enable the method"
            $ referencesGlobal methodName body
  , testCase "instantiate an alias-expanded loaded Exference provider visibly" $ do
      integerName <- expectRight $ mkIdentifier "Int"
      tokenName <- expectRight $ mkIdentifier "Token"
      aliasName <- expectRight $ mkIdentifier "Identity"
      className <- expectRight $ mkIdentifier "C"
      globalName <- expectRight $ mkIdentifier "global"
      targetName <- expectRight $ mkIdentifier "use"
      target <- expectRight $ mkDefinitionName targetName
      integerArgument <- expectRight $ specifiedVisibleTypeArgument
        (TypeConstructor integerName :: Type ExferenceTypeVariable)
      let variable = FlexibleVariable 0
          variableType = TypeVariable variable
          integerType = TypeConstructor integerName
          tokenType = TypeConstructor tokenName
          aliased argument = TypeApplication
            (TypeConstructor aliasName) argument
          providerConstraint = Constraint className
            [aliased variableType]
          declarations =
            [ AbstractTypeDeclaration () integerName ProperTypeKind
            , AbstractTypeDeclaration () tokenName ProperTypeKind
            , TypeSynonymDeclaration () aliasName
                [TypeParameter variable Nothing] variableType
            , ClassDeclaration () className
                [TypeParameter variable Nothing] [] []
            , InstanceDeclaration () [] []
                $ Constraint className [integerType]
            , ValueDeclaration $ ValueSignature () globalName
                $ ForallType [variable] [providerConstraint] tokenType
            ]
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = target
        , requestGoal = tokenType
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            {exferenceMaximumSteps = 128}
        }
      results <- expectRight $ runExferenceQuery session request
      let candidates = concatMap
            (batchCandidates . resultSearch) results
          isVisibleGlobal candidate = case candidateOutput candidate of
            FunctionClause _ []
                (VisibleTypeApplication (Global occurrence) argument) ->
              occurrence == globalName && argument == integerArgument
            _ -> False
      candidate <- maybe
        (fail "loaded global did not preserve its instance-selected @Int")
        pure
        $ find isVisibleGlobal candidates
      candidateResidualConstraints candidate @?= []
      rendered <- expectRight
        $ renderExferenceCandidateDefinition Unqualified candidate
      rendered @?= "use = global @Int"

      let fixture = unlines
            [ "module LoadedVisibleProviderFixture where"
            , ""
            , "data Token = Token"
            , ""
            , "type Identity a = a"
            , ""
            , "class C a"
            , "instance C Int"
            , ""
            , "global :: forall a. C (Identity a) => Token"
            , "global = error \"fixture\""
            , ""
            , rendered
            ]
      withTemporaryHaskellModule fixture $ \sourcePath -> do
        (exitCode, output, errors) <- readProcessWithExitCode "ghc"
          [ "-v0"
          , "-fforce-recomp"
          , "-fno-code"
          , "-fno-write-interface"
          , "-XHaskell2010"
          , "-XAllowAmbiguousTypes"
          , "-XRankNTypes"
          , "-XTypeApplications"
          , sourcePath
          ] ""
        assertEqual
          ("GHC rejected loaded visible provider specialization\nstdout:\n"
            ++ output ++ "\nstderr:\n" ++ errors)
          ExitSuccess exitCode
  , testCase "specialize a context-free loaded provider from the query" $ do
      seedName <- expectRight $ mkIdentifier "Seed"
      tokenName <- expectRight $ mkIdentifier "Token"
      globalName <- expectRight $ mkIdentifier "global"
      targetName <- expectRight $ mkIdentifier "use"
      target <- expectRight $ mkDefinitionName targetName
      seedArgument <- expectRight $ specifiedVisibleTypeArgument
        (TypeConstructor seedName :: Type ExferenceTypeVariable)
      let variable = FlexibleVariable 0
          seedType = TypeConstructor seedName
          tokenType = TypeConstructor tokenName
          declarations =
            [ AbstractTypeDeclaration () seedName ProperTypeKind
            , AbstractTypeDeclaration () tokenName ProperTypeKind
            , ValueDeclaration $ ValueSignature () globalName
                $ ForallType [variable] [] tokenType
            ]
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = target
        , requestGoal = FunctionType seedType tokenType
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            { exferenceAllowUnused = True
            , exferenceMaximumSteps = 256
            }
        }
      results <- expectRight $ runExferenceQuery session request
      let candidates = concatMap
            (batchCandidates . resultSearch) results
          isVisibleGlobal candidate = case candidateOutput candidate of
            FunctionClause _ [_]
                (VisibleTypeApplication (Global occurrence) argument) ->
              occurrence == globalName && argument == seedArgument
            _ -> False
      candidate <- maybe
        (fail "query-supplied Seed did not produce loaded global @Seed")
        pure
        $ find isVisibleGlobal candidates
      candidateResidualConstraints candidate @?= []
      rendered <- expectRight
        $ renderExferenceCandidateDefinition Unqualified candidate

      let fixture = unlines
            [ "module QuerySelectedVisibleProviderFixture where"
            , ""
            , "data Seed = Seed"
            , "data Token = Token"
            , ""
            , "global :: forall a. Token"
            , "global = Token"
            , ""
            , "use :: Seed -> Token"
            , rendered
            ]
      withTemporaryHaskellModule fixture $ \sourcePath -> do
        (exitCode, output, errors) <- readProcessWithExitCode "ghc"
          [ "-v0"
          , "-fforce-recomp"
          , "-fno-code"
          , "-fno-write-interface"
          , "-XHaskell2010"
          , "-XAllowAmbiguousTypes"
          , "-XRankNTypes"
          , "-XTypeApplications"
          , sourcePath
          ] ""
        assertEqual
          ("GHC rejected query-selected visible provider specialization\nstdout:\n"
            ++ output ++ "\nstderr:\n" ++ errors
            ++ "\ngenerated:\n" ++ rendered)
          ExitSuccess exitCode
  , testCase "specialize an Exference provider from caller evidence" $ do
      tokenName <- expectRight $ mkIdentifier "EvidenceToken"
      providerName <- expectRight $ mkIdentifier "evidenceProvider"
      targetName <- expectRight $ mkIdentifier "useEvidenceProvider"
      target <- expectRight $ mkDefinitionName targetName
      let providerVariable = FlexibleVariable 0
          identityVariable = FlexibleVariable 1
          identityType = TypeVariable identityVariable
          quantifiedIdentity = ForallType [identityVariable] [] $
            FunctionType identityType identityType
          tokenType = TypeConstructor tokenName
          providerType = ForallType [providerVariable] [] tokenType
          declarations =
            [ AbstractTypeDeclaration () tokenName ProperTypeKind
            , ValueDeclaration $ ValueSignature () providerName providerType
            ]
          evidence = ProviderInstantiationCandidate
            { providerInstantiationCandidateProvider = providerName
            , providerInstantiationCandidateType = quantifiedIdentity
            }
      quantifiedArgument <- expectRight
        $ specifiedVisibleTypeArgument quantifiedIdentity
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = target
        , requestGoal = tokenType
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            {exferenceMaximumSteps = 512}
        }
      results <- expectRight $ runExferenceQueryWithInstantiationCandidates
        session [evidence] request
      let candidates = concatMap
            (batchCandidates . resultSearch) results
          isSuppliedApplication candidate = case candidateOutput candidate of
            FunctionClause _ []
                (VisibleTypeApplication (Global occurrence) argument) ->
              occurrence == providerName && argument == quantifiedArgument
            _ -> False
      candidate <- maybe
        (fail $ "caller evidence produced no quantified provider application: "
          ++ show (map candidateOutput candidates))
        pure
        $ find isSuppliedApplication candidates
      rendered <- expectRight
        $ renderExferenceCandidateDefinition Unqualified candidate
      assertBool "caller evidence lost the exact provider application"
        $ "evidenceProvider @(forall" `isInfixOf` rendered

  , testCase "specialize a non-vacuous Exference provider from caller evidence" $ do
      wrapperName <- expectRight $ mkIdentifier "EvidenceWrapper"
      providerName <- expectRight $ mkIdentifier "evidenceWrapperProvider"
      targetName <- expectRight $ mkIdentifier "useEvidenceWrapperProvider"
      target <- expectRight $ mkDefinitionName targetName
      let providerVariable = FlexibleVariable 0
          identityVariable = FlexibleVariable 1
          identityType = TypeVariable identityVariable
          quantifiedIdentity = ForallType [identityVariable] [] $
            FunctionType identityType identityType
          wrapperConstructor = TypeConstructor wrapperName
          wrapperType argument =
            TypeApplication wrapperConstructor argument
          providerType = ForallType [providerVariable] [] $
            wrapperType $ TypeVariable providerVariable
          goalType = wrapperType quantifiedIdentity
          declarations =
            [ AbstractTypeDeclaration () wrapperName $
                FunctionKind ProperTypeKind ProperTypeKind
            , ValueDeclaration $ ValueSignature () providerName providerType
            ]
          assignment = ProviderInstantiationAssignment
            { providerInstantiationAssignmentProvider = providerName
            , providerInstantiationAssignmentArguments = [quantifiedIdentity]
            }
      quantifiedArgument <- expectRight
        $ specifiedVisibleTypeArgument quantifiedIdentity
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = target
        , requestGoal = goalType
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            {exferenceMaximumSteps = 512}
        }
      results <- expectRight $ runExferenceQueryWithInstantiationAssignments
        session [assignment] request
      let candidates = concatMap
            (batchCandidates . resultSearch) results
          isSuppliedApplication candidate = case candidateOutput candidate of
            FunctionClause _ []
                (VisibleTypeApplication (Global occurrence) argument) ->
              occurrence == providerName && argument == quantifiedArgument
            _ -> False
      candidate <- maybe
        (fail $ "caller evidence produced no impredicative wrapper result: "
          ++ show (map candidateOutput candidates))
        pure
        $ find isSuppliedApplication candidates
      rendered <- expectRight
        $ renderExferenceCandidateDefinition Unqualified candidate
      assertBool "non-vacuous evidence lost the exact provider application"
        $ "evidenceWrapperProvider @(forall" `isInfixOf` rendered

  , testCase "retain a structural impredicative Exference assignment" $ do
      tokenName <- expectRight $ mkIdentifier "StructuralEvidenceToken"
      wrapperName <- expectRight $ mkIdentifier "StructuralEvidenceWrapper"
      providerName <- expectRight $ mkIdentifier "structuralEvidenceProvider"
      targetName <- expectRight $ mkIdentifier "useStructuralEvidenceProvider"
      target <- expectRight $ mkDefinitionName targetName
      let providerVariable = FlexibleVariable 0
          identityVariable = FlexibleVariable 1
          identityType = TypeVariable identityVariable
          quantifiedIdentity = ForallType [identityVariable] [] $
            FunctionType identityType identityType
          wrapperConstructor = TypeConstructor wrapperName
          structuralChoice =
            TypeApplication wrapperConstructor quantifiedIdentity
          tokenType = TypeConstructor tokenName
          providerType = ForallType [providerVariable] [] tokenType
          declarations =
            [ AbstractTypeDeclaration () tokenName ProperTypeKind
            , AbstractTypeDeclaration () wrapperName $
                FunctionKind ProperTypeKind ProperTypeKind
            , ValueDeclaration $ ValueSignature () providerName providerType
            ]
          assignment = ProviderInstantiationAssignment
            { providerInstantiationAssignmentProvider = providerName
            , providerInstantiationAssignmentArguments = [structuralChoice]
            }
      structuralArgument <- expectRight
        $ specifiedVisibleTypeArgument structuralChoice
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = target
        , requestGoal = tokenType
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            {exferenceMaximumSteps = 512}
        }
      results <- expectRight $ runExferenceQueryWithInstantiationAssignments
        session [assignment] request
      let candidates = concatMap
            (batchCandidates . resultSearch) results
          isStructuralApplication candidate = case candidateOutput candidate of
            FunctionClause _ []
                (VisibleTypeApplication (Global occurrence) argument) ->
              occurrence == providerName && argument == structuralArgument
            _ -> False
      candidate <- maybe
        (fail $ "structural assignment produced no visible application: "
          ++ show (map candidateOutput candidates))
        pure
        $ find isStructuralApplication candidates
      rendered <- expectRight
        $ renderExferenceCandidateDefinition Unqualified candidate
      assertBool "structural assignment lost its nested quantified argument"
        $ all (`isInfixOf` rendered)
            ["structuralEvidenceProvider @(", "forall"]

  , testCase "retain an exact four-binder Exference assignment" $ do
      tokenName <- expectRight $ mkIdentifier "FourEvidenceToken"
      providerName <- expectRight $ mkIdentifier "fourEvidenceProvider"
      targetName <- expectRight $ mkIdentifier "useFourEvidenceProvider"
      target <- expectRight $ mkDefinitionName targetName
      let providerVariables = map FlexibleVariable [0 .. 3]
          quantified binder body = ForallType [FlexibleVariable binder] [] body
          variable binder = TypeVariable $ FlexibleVariable binder
          tokenType = TypeConstructor tokenName
          arguments =
            [ quantified 10 $ FunctionType (variable 10) (variable 10)
            , quantified 11 $ FunctionType (variable 11) tokenType
            , quantified 12 $ FunctionType tokenType (variable 12)
            , quantified 13 $ FunctionType (variable 13) $
                FunctionType (variable 13) (variable 13)
            ]
          providerType = ForallType providerVariables [] tokenType
          declarations =
            [ AbstractTypeDeclaration () tokenName ProperTypeKind
            , ValueDeclaration $ ValueSignature () providerName providerType
            ]
          assignment = ProviderInstantiationAssignment
            { providerInstantiationAssignmentProvider = providerName
            , providerInstantiationAssignmentArguments = arguments
            }
          visibleSpine expression = case expression of
            Global name -> Just (name, [])
            VisibleTypeApplication function argument -> do
              (name, earlier) <- visibleSpine function
              pure (name, earlier ++ [argument])
            _ -> Nothing
      visibleArguments <- expectRight $
        traverse specifiedVisibleTypeArgument arguments
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = target
        , requestGoal = tokenType
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            {exferenceMaximumSteps = 1024}
        }
      results <- expectRight $ runExferenceQueryWithInstantiationAssignments
        session [assignment] request
      let candidates = concatMap
            (batchCandidates . resultSearch) results
          visibleVectors =
            [ actual
            | candidate <- candidates
            , FunctionClause _ [] body <- [candidateOutput candidate]
            , Just (occurrence, actual@(_ : _)) <- [visibleSpine body]
            , occurrence == providerName
            ]
      assertBool
        ("the exact four-binder assignment was absent: " ++ show visibleVectors)
        $ visibleArguments `elem` visibleVectors

  , testCase "retain six-binder Exference assignments and reject seven" $ do
      tokenName <- expectRight $ mkIdentifier "SixEvidenceToken"
      providerName <- expectRight $ mkIdentifier "sixEvidenceProvider"
      sevenProviderName <- expectRight $
        mkIdentifier "sevenEvidenceProvider"
      targetName <- expectRight $ mkIdentifier "useSixEvidenceProvider"
      target <- expectRight $ mkDefinitionName targetName
      let providerVariables = map FlexibleVariable [0 .. 5]
          quantified binder body = ForallType [FlexibleVariable binder] [] body
          variable binder = TypeVariable $ FlexibleVariable binder
          tokenType = TypeConstructor tokenName
          arguments =
            [ quantified 10 $ FunctionType (variable 10) (variable 10)
            , quantified 11 $ FunctionType (variable 11) tokenType
            , quantified 12 $ FunctionType tokenType (variable 12)
            , quantified 13 $ FunctionType (variable 13) $
                FunctionType (variable 13) (variable 13)
            , quantified 14 $ FunctionType
                (FunctionType (variable 14) tokenType) (variable 14)
            , quantified 15 $ FunctionType (variable 15) $
                FunctionType tokenType (variable 15)
            ]
          seventhArgument = quantified 16 $ FunctionType
            (FunctionType (variable 16) tokenType) (variable 16)
          providerType = ForallType providerVariables [] tokenType
          sevenProviderType = ForallType
            (map FlexibleVariable [0 .. 6]) [] tokenType
          declarations =
            [ AbstractTypeDeclaration () tokenName ProperTypeKind
            , ValueDeclaration $ ValueSignature () providerName providerType
            , ValueDeclaration $ ValueSignature () sevenProviderName
                sevenProviderType
            ]
          assignment = ProviderInstantiationAssignment
            { providerInstantiationAssignmentProvider = providerName
            , providerInstantiationAssignmentArguments = arguments
            }
          visibleSpine expression = case expression of
            Global name -> Just (name, [])
            VisibleTypeApplication function argument -> do
              (name, earlier) <- visibleSpine function
              pure (name, earlier ++ [argument])
            _ -> Nothing
      visibleArguments <- expectRight $
        traverse specifiedVisibleTypeArgument arguments
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = target
        , requestGoal = tokenType
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            {exferenceMaximumSteps = 1024}
        }
      results <- expectRight $ runExferenceQueryWithInstantiationAssignments
        session [assignment] request
      let candidates = concatMap
            (batchCandidates . resultSearch) results
          visibleVectors =
            [ actual
            | candidate <- candidates
            , FunctionClause _ [] body <- [candidateOutput candidate]
            , Just (occurrence, actual@(_ : _)) <- [visibleSpine body]
            , occurrence == providerName
            ]
      assertBool
        ("the exact six-binder assignment was absent: " ++ show visibleVectors)
        $ visibleArguments `elem` visibleVectors
      let sevenAssignment = ProviderInstantiationAssignment
            { providerInstantiationAssignmentProvider = sevenProviderName
            , providerInstantiationAssignmentArguments =
                arguments ++ [seventhArgument]
            }
      case runExferenceQueryWithInstantiationAssignments
          session [sevenAssignment] request of
        Left failure -> diagnosticCode failure @?=
          Just "DJEX_EXF_ASSIGNMENT_ARGUMENT_LIMIT"
        Right result -> fail $
          "Exference accepted a seven-binder assignment: " ++ show result

  , testCase "empty Exference provider evidence is exactly inert" $ do
      tokenName <- expectRight $ mkIdentifier "EmptyEvidenceToken"
      providerName <- expectRight $ mkIdentifier "emptyEvidenceProvider"
      targetName <- expectRight $ mkIdentifier "useEmptyEvidenceProvider"
      target <- expectRight $ mkDefinitionName targetName
      let providerVariable = FlexibleVariable 0
          tokenType = TypeConstructor tokenName
          declarations =
            [ AbstractTypeDeclaration () tokenName ProperTypeKind
            , ValueDeclaration $ ValueSignature () providerName
                $ ForallType [providerVariable] [] tokenType
            ]
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = target
        , requestGoal = tokenType
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            {exferenceMaximumSteps = 256}
        }
      historical <- expectRight $ runExferenceQuery session request
      explicitEmpty <- expectRight
        $ runExferenceQueryWithInstantiationCandidates session [] request
      exactEmpty <- expectRight
        $ runExferenceQueryWithInstantiationAssignments session [] request
      kindedEmpty <- expectRight
        $ runExferenceQueryWithKindedInstantiationAssignments
            session [] request
      explicitEmpty @?= historical
      exactEmpty @?= historical
      kindedEmpty @?= historical

  , testCase "bound Exference provider evidence before entering elements" $ do
      tokenName <- expectRight $ mkIdentifier "BoundedEvidenceToken"
      targetName <- expectRight $ mkIdentifier "boundedEvidence"
      target <- expectRight $ mkDefinitionName targetName
      let tokenType = TypeConstructor tokenName
          declarations =
            [AbstractTypeDeclaration () tokenName ProperTypeKind]
          poisonedCandidates =
            (error "Exference entered an over-limit evidence element")
              : poisonedCandidates
          poisonedAssignments =
            (error "Exference entered an over-limit assignment element")
              : poisonedAssignments
          poisonedKindedAssignments =
            (error "Exference entered an over-limit kinded assignment element")
              : poisonedKindedAssignments
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = target
        , requestGoal = tokenType
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            {exferenceMaximumSteps = 32}
        }
      result <- expectWithin "Exference provider evidence bound" $ evaluate
        $ runExferenceQueryWithInstantiationCandidates
            session poisonedCandidates request
      case result of
        Left failure -> diagnosticCode failure @?=
          Just "DJEX_EXF_CANDIDATE_LIMIT"
        Right _ -> fail "Exference accepted an over-limit cyclic evidence list"
      assignmentResult <- expectWithin "Exference provider assignment bound" $
        evaluate $ runExferenceQueryWithInstantiationAssignments
          session poisonedAssignments request
      case assignmentResult of
        Left failure -> diagnosticCode failure @?=
          Just "DJEX_EXF_ASSIGNMENT_LIMIT"
        Right _ -> fail "Exference accepted an over-limit cyclic assignment list"
      kindedAssignmentResult <- expectWithin
        "Exference kinded provider assignment bound" $ evaluate $
          runExferenceQueryWithKindedInstantiationAssignments
            session poisonedKindedAssignments request
      case kindedAssignmentResult of
        Left failure -> diagnosticCode failure @?=
          Just "DJEX_EXF_ASSIGNMENT_LIMIT"
        Right _ -> fail
          "Exference accepted an over-limit cyclic kinded assignment list"

  , testCase "validate Exference assignment vectors before search" $ do
      tokenName <- expectRight $ mkIdentifier "AssignmentBoundaryToken"
      providerName <- expectRight $ mkIdentifier "assignmentBoundaryProvider"
      targetName <- expectRight $ mkIdentifier "assignmentBoundaryTarget"
      target <- expectRight $ mkDefinitionName targetName
      let tokenType = TypeConstructor tokenName
          providerType = ForallType [FlexibleVariable 0] [] tokenType
          declarations =
            [ AbstractTypeDeclaration () tokenName ProperTypeKind
            , ValueDeclaration $ ValueSignature () providerName providerType
            ]
          poisonedArguments =
            (error "Exference entered an over-limit assignment argument")
              : poisonedArguments
          poisonedKindedArguments =
            (error "Exference entered an over-limit kinded assignment argument")
              : poisonedKindedArguments
          assignment arguments = ProviderInstantiationAssignment
            { providerInstantiationAssignmentProvider = providerName
            , providerInstantiationAssignmentArguments = arguments
            }
          kindedAssignment arguments = KindedProviderInstantiationAssignment
            { kindedProviderInstantiationAssignmentProvider = providerName
            , kindedProviderInstantiationAssignmentArguments = arguments
            }
          cyclicGroundKind =
            FunctionKind cyclicGroundKind ProperTypeKind
          cyclicKindedAssignment = kindedAssignment
            [ ( cyclicGroundKind
              , error "Exference forced a type paired with a cyclic kind"
              )
            ]
          validKindedAssignment =
            kindedAssignment [(ProperTypeKind, tokenType)]
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = target
        , requestGoal = tokenType
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            {exferenceMaximumSteps = 32}
        }
      let expectFailure label code supplied = do
            outcome <- expectWithin label $ evaluate $
              runExferenceQueryWithInstantiationAssignments
                session [assignment supplied] request
            case outcome of
              Left failure -> diagnosticCode failure @?= Just code
              Right _ -> fail $ label ++ " was accepted"
      expectFailure "cyclic assignment argument spine"
        "DJEX_EXF_ASSIGNMENT_ARGUMENT_LIMIT" poisonedArguments
      expectFailure "empty assignment vector"
        "DJEX_EXF_ASSIGNMENT_ARITY" []
      expectFailure "wrong assignment arity"
        "DJEX_EXF_ASSIGNMENT_ARITY" [tokenType, tokenType]
      kindedOutcome <- expectWithin "cyclic kinded assignment argument spine" $
        evaluate $ runExferenceQueryWithKindedInstantiationAssignments
          session [kindedAssignment poisonedKindedArguments] request
      case kindedOutcome of
        Left failure -> diagnosticCode failure @?=
          Just "DJEX_EXF_ASSIGNMENT_ARGUMENT_LIMIT"
        Right _ -> fail
          "cyclic kinded assignment argument spine was accepted"
      let expectKindLimit label supplied = do
            outcome <- expectWithin label $ evaluate $
              runExferenceQueryWithKindedInstantiationAssignments
                session supplied request
            case outcome of
              Left failure -> do
                diagnosticCode failure @?=
                  Just "DJEX_EXF_ASSIGNMENT_KIND_LIMIT"
                diagnosticSource failure @?= Nothing
              Right _ -> fail $ label ++ " was accepted"
      expectKindLimit "cyclic Exference assignment kind"
        [cyclicKindedAssignment]
      expectKindLimit
        "cyclic Exference kind before same-provider comparison"
        [validKindedAssignment, cyclicKindedAssignment]

  , testCase "kind a vacuous Exference provider from caller evidence" $ do
      tokenName <- expectRight $ mkIdentifier "VacuousKindedToken"
      wrapperName <- expectRight $ mkIdentifier "VacuousKindedWrapper"
      providerName <- expectRight $ mkIdentifier "vacuousKindedProvider"
      targetName <- expectRight $ mkIdentifier "useVacuousKindedProvider"
      target <- expectRight $ mkDefinitionName targetName
      let providerVariable = FlexibleVariable 0
          tokenType = TypeConstructor tokenName
          wrapperConstructor :: ExferenceType
          wrapperConstructor = TypeConstructor wrapperName
          constructorKind =
            FunctionKind ProperTypeKind ProperTypeKind
          providerType = ForallType [providerVariable] [] tokenType
          declarations =
            [ AbstractTypeDeclaration () tokenName ProperTypeKind
            , AbstractTypeDeclaration () wrapperName constructorKind
            , ValueDeclaration $ ValueSignature () providerName providerType
            ]
          legacyAssignment = ProviderInstantiationAssignment
            { providerInstantiationAssignmentProvider = providerName
            , providerInstantiationAssignmentArguments = [wrapperConstructor]
            }
          kindedAssignment = KindedProviderInstantiationAssignment
            { kindedProviderInstantiationAssignmentProvider = providerName
            , kindedProviderInstantiationAssignmentArguments =
                [(constructorKind, wrapperConstructor)]
            }
          visibleSpine expression = case expression of
            Global name -> Just (name, [])
            VisibleTypeApplication function argument -> do
              (name, earlier) <- visibleSpine function
              pure (name, earlier ++ [argument])
            _ -> Nothing
      visibleWrapper <- expectRight $
        specifiedVisibleTypeArgument wrapperConstructor
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = target
        , requestGoal = tokenType
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            {exferenceMaximumSteps = 512}
        }
      case runExferenceQueryWithInstantiationAssignments
          session [legacyAssignment] request of
        Left failure -> diagnosticCode failure @?=
          Just "DJEX_EXF_ASSIGNMENT_KIND"
        Right _ -> fail
          "the legacy assignment API accepted a vacuous higher-kinded choice"
      results <- expectRight $
        runExferenceQueryWithKindedInstantiationAssignments
          session [kindedAssignment] request
      let visibleVectors =
            [ actual
            | candidate <- concatMap
                (batchCandidates . resultSearch) results
            , FunctionClause _ [] body <- [candidateOutput candidate]
            , Just (occurrence, actual) <- [visibleSpine body]
            , occurrence == providerName
            ]
      assertBool
        ("the vacuous kinded assignment was absent: " ++ show visibleVectors)
        $ [visibleWrapper] `elem` visibleVectors

  , testCase "retain an ordered multi-vacuous Exference assignment" $ do
      tokenName <- expectRight $ mkIdentifier "MultiVacuousKindedToken"
      wrapperName <- expectRight $ mkIdentifier "MultiVacuousKindedWrapper"
      tripleName <- expectRight $ mkIdentifier "MultiVacuousKindedTriple"
      providerName <- expectRight $ mkIdentifier "multiVacuousKindedProvider"
      targetName <- expectRight $ mkIdentifier "useMultiVacuousKindedProvider"
      target <- expectRight $ mkDefinitionName targetName
      let firstVariable = FlexibleVariable 0
          secondVariable = FlexibleVariable 1
          tokenType = TypeConstructor tokenName
          wrapperConstructor :: ExferenceType
          wrapperConstructor = TypeConstructor wrapperName
          tripleConstructor :: ExferenceType
          tripleConstructor = TypeConstructor tripleName
          partialTriple = TypeApplication tripleConstructor tokenType
          constructorKind =
            FunctionKind ProperTypeKind ProperTypeKind
          binaryConstructorKind =
            FunctionKind ProperTypeKind constructorKind
          tripleKind = FunctionKind ProperTypeKind binaryConstructorKind
          providerType = ForallType
            [firstVariable, secondVariable] [] tokenType
          declarations =
            [ AbstractTypeDeclaration () tokenName ProperTypeKind
            , AbstractTypeDeclaration () wrapperName constructorKind
            , AbstractTypeDeclaration () tripleName tripleKind
            , ValueDeclaration $ ValueSignature () providerName providerType
            ]
          kindedAssignment = KindedProviderInstantiationAssignment
            { kindedProviderInstantiationAssignmentProvider = providerName
            , kindedProviderInstantiationAssignmentArguments =
                [ (constructorKind, wrapperConstructor)
                , (binaryConstructorKind, partialTriple)
                ]
            }
          visibleSpine expression = case expression of
            Global name -> Just (name, [])
            VisibleTypeApplication function argument -> do
              (name, earlier) <- visibleSpine function
              pure (name, earlier ++ [argument])
            _ -> Nothing
      visibleArguments <- expectRight $
        traverse specifiedVisibleTypeArgument
          [wrapperConstructor, partialTriple]
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = target
        , requestGoal = tokenType
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            {exferenceMaximumSteps = 512}
        }
      results <- expectRight $
        runExferenceQueryWithKindedInstantiationAssignments
          session [kindedAssignment] request
      let visibleVectors =
            [ actual
            | candidate <- concatMap
                (batchCandidates . resultSearch) results
            , FunctionClause _ [] body <- [candidateOutput candidate]
            , Just (occurrence, actual) <- [visibleSpine body]
            , occurrence == providerName
            ]
      assertBool
        ("the multi-vacuous kinded assignment lost its partial constructor "
          ++ "or positional order: " ++ show visibleVectors)
        $ visibleArguments `elem` visibleVectors

  , testCase "retain distinct higher-order Exference assignments" $ do
      tokenName <- expectRight $ mkIdentifier "HigherOrderKindedToken"
      higherOrderConstructorName <- expectRight $
        mkIdentifier "HigherOrderKindedConstructor"
      builderName <- expectRight $ mkIdentifier "HigherOrderKindedBuilder"
      providerName <- expectRight $ mkIdentifier "higherOrderKindedProvider"
      targetName <- expectRight $ mkIdentifier "useHigherOrderKindedProvider"
      target <- expectRight $ mkDefinitionName targetName
      let providerVariable = FlexibleVariable 0
          tokenType = TypeConstructor tokenName
          higherOrderConstructor :: ExferenceType
          higherOrderConstructor =
            TypeConstructor higherOrderConstructorName
          builderConstructor :: ExferenceType
          builderConstructor = TypeConstructor builderName
          partialBuilder = TypeApplication builderConstructor tokenType
          unaryKind = FunctionKind ProperTypeKind ProperTypeKind
          higherOrderKind = FunctionKind unaryKind ProperTypeKind
          builderKind = FunctionKind ProperTypeKind higherOrderKind
          providerType = ForallType [providerVariable] [] tokenType
          declarations =
            [ AbstractTypeDeclaration () tokenName ProperTypeKind
            , AbstractTypeDeclaration () higherOrderConstructorName
                higherOrderKind
            , AbstractTypeDeclaration () builderName builderKind
            , ValueDeclaration $ ValueSignature () providerName providerType
            ]
          assignment argument = KindedProviderInstantiationAssignment
            { kindedProviderInstantiationAssignmentProvider = providerName
            , kindedProviderInstantiationAssignmentArguments =
                [(higherOrderKind, argument)]
            }
          firstAssignment = assignment higherOrderConstructor
          secondAssignment = assignment partialBuilder
          visibleSpine expression = case expression of
            Global name -> Just (name, [])
            VisibleTypeApplication function argument -> do
              (name, earlier) <- visibleSpine function
              pure (name, earlier ++ [argument])
            _ -> Nothing
      visibleArguments <- expectRight $
        traverse specifiedVisibleTypeArgument
          [higherOrderConstructor, partialBuilder]
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = target
        , requestGoal = tokenType
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            {exferenceMaximumSteps = 512}
        }
      results <- expectRight $
        runExferenceQueryWithKindedInstantiationAssignments
          session [firstAssignment, secondAssignment, firstAssignment] request
      let visibleVectors =
            [ actual
            | candidate <- concatMap
                (batchCandidates . resultSearch) results
            , FunctionClause _ [] body <- [candidateOutput candidate]
            , Just (occurrence, actual) <- [visibleSpine body]
            , occurrence == providerName
            ]
      mapM_ (\expected ->
        assertEqual
          ("a distinct same-kind higher-order assignment was lost or "
            ++ "duplicated: " ++ show visibleVectors)
          1
          (length $ filter (== [expected]) visibleVectors))
        visibleArguments

  , testCase "accept an Exference higher-kinded assignment" $ do
      tokenName <- expectRight $ mkIdentifier "HigherAssignmentToken"
      wrapperName <- expectRight $ mkIdentifier "HigherAssignmentWrapper"
      providerName <- expectRight $ mkIdentifier "higherAssignmentProvider"
      targetName <- expectRight $ mkIdentifier "higherAssignmentTarget"
      target <- expectRight $ mkDefinitionName targetName
      let constructorVariable = FlexibleVariable 0
          tokenType = TypeConstructor tokenName
          wrapperConstructor :: ExferenceType
          wrapperConstructor = TypeConstructor wrapperName
          wrappedToken = TypeApplication wrapperConstructor tokenType
          providerType = ForallType [constructorVariable] [] $
            FunctionType
              (TypeApplication
                (TypeVariable constructorVariable) tokenType)
              tokenType
          goalType = FunctionType wrappedToken tokenType
          declarations =
            [ AbstractTypeDeclaration () tokenName ProperTypeKind
            , AbstractTypeDeclaration () wrapperName $
                FunctionKind ProperTypeKind ProperTypeKind
            , ValueDeclaration $ ValueSignature () providerName providerType
            ]
          assignment = ProviderInstantiationAssignment
            { providerInstantiationAssignmentProvider = providerName
            , providerInstantiationAssignmentArguments = [wrapperConstructor]
            }
          scalarCandidate = ProviderInstantiationCandidate
            { providerInstantiationCandidateProvider = providerName
            , providerInstantiationCandidateType = wrapperConstructor
            }
          visibleSpine expression = case expression of
            Global name -> Just (name, [])
            VisibleTypeApplication function argument -> do
              (name, earlier) <- visibleSpine function
              pure (name, earlier ++ [argument])
            _ -> Nothing
      visibleWrapper <- expectRight $
        specifiedVisibleTypeArgument wrapperConstructor
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = target
        , requestGoal = goalType
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            {exferenceMaximumSteps = 512}
        }
      results <- expectRight $ runExferenceQueryWithInstantiationAssignments
        session [assignment] request
      let visibleVectors =
            [ actual
            | candidate <- concatMap
                (batchCandidates . resultSearch) results
            , FunctionClause _ [] body <- [candidateOutput candidate]
            , Just (occurrence, actual) <- [visibleSpine body]
            , occurrence == providerName
            ]
      assertBool
        ("the higher-kinded assignment was absent: " ++ show visibleVectors)
        $ [visibleWrapper] `elem` visibleVectors
      case runExferenceQueryWithInstantiationCandidates
          session [scalarCandidate] request of
        Left failure -> diagnosticCode failure @?=
          Just "DJEX_EXF_CANDIDATE_KIND"
        Right _ -> fail
          "the legacy scalar Candidate API accepted a higher-kinded type"

  , testCase "retain a mixed higher-kinded and impredicative Exference assignment" $ do
      tokenName <- expectRight $ mkIdentifier "MixedKindAssignmentToken"
      wrapperName <- expectRight $ mkIdentifier "MixedKindAssignmentWrapper"
      providerName <- expectRight $ mkIdentifier "mixedKindAssignmentProvider"
      targetName <- expectRight $ mkIdentifier "mixedKindAssignmentTarget"
      target <- expectRight $ mkDefinitionName targetName
      let constructorVariable = FlexibleVariable 0
          hiddenVariable = FlexibleVariable 1
          identityVariable = FlexibleVariable 2
          identityType = TypeVariable identityVariable
          quantifiedIdentity = ForallType [identityVariable] [] $
            FunctionType identityType identityType
          tokenType = TypeConstructor tokenName
          wrapperConstructor :: ExferenceType
          wrapperConstructor = TypeConstructor wrapperName
          wrappedToken = TypeApplication wrapperConstructor tokenType
          providerType = ForallType
            [constructorVariable, hiddenVariable] [] $
              FunctionType
                (TypeApplication
                  (TypeVariable constructorVariable) tokenType)
                tokenType
          goalType = FunctionType wrappedToken tokenType
          arguments = [wrapperConstructor, quantifiedIdentity]
          declarations =
            [ AbstractTypeDeclaration () tokenName ProperTypeKind
            , AbstractTypeDeclaration () wrapperName $
                FunctionKind ProperTypeKind ProperTypeKind
            , ValueDeclaration $ ValueSignature () providerName providerType
            ]
          assignment = ProviderInstantiationAssignment
            { providerInstantiationAssignmentProvider = providerName
            , providerInstantiationAssignmentArguments = arguments
            }
          visibleSpine expression = case expression of
            Global name -> Just (name, [])
            VisibleTypeApplication function argument -> do
              (name, earlier) <- visibleSpine function
              pure (name, earlier ++ [argument])
            _ -> Nothing
      visibleArguments <- expectRight $
        traverse specifiedVisibleTypeArgument arguments
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = target
        , requestGoal = goalType
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            {exferenceMaximumSteps = 512}
        }
      results <- expectRight $ runExferenceQueryWithInstantiationAssignments
        session [assignment] request
      let visibleVectors =
            [ actual
            | candidate <- concatMap
                (batchCandidates . resultSearch) results
            , FunctionClause _ [] body <- [candidateOutput candidate]
            , Just (occurrence, actual) <- [visibleSpine body]
            , occurrence == providerName
            ]
      assertBool
        ("the mixed higher-kinded/impredicative assignment was absent: " ++
          show visibleVectors)
        $ visibleArguments `elem` visibleVectors

  , testCase "reject both directions of Exference assignment kind mismatch" $ do
      tokenName <- expectRight $ mkIdentifier "AssignmentKindToken"
      wrapperName <- expectRight $ mkIdentifier "AssignmentKindWrapper"
      higherProviderName <- expectRight $
        mkIdentifier "higherKindAssignmentProvider"
      properProviderName <- expectRight $
        mkIdentifier "properKindAssignmentProvider"
      targetName <- expectRight $ mkIdentifier "assignmentKindTarget"
      target <- expectRight $ mkDefinitionName targetName
      let constructorVariable = FlexibleVariable 0
          properVariable = FlexibleVariable 1
          tokenType = TypeConstructor tokenName
          wrapperConstructor = TypeConstructor wrapperName
          constructorKind =
            FunctionKind ProperTypeKind ProperTypeKind
          -- The occurrence as an application fixes this binder at
          -- Type -> Type; the other provider's vacuous binder defaults to Type.
          higherProviderType = ForallType [constructorVariable] [] $
            FunctionType
              (TypeApplication
                (TypeVariable constructorVariable) tokenType)
              tokenType
          properProviderType = ForallType [properVariable] [] tokenType
          declarations =
            [ AbstractTypeDeclaration () tokenName ProperTypeKind
            , AbstractTypeDeclaration () wrapperName constructorKind
            , ValueDeclaration $ ValueSignature ()
                higherProviderName higherProviderType
            , ValueDeclaration $ ValueSignature ()
                properProviderName properProviderType
            ]
          assignment provider arguments = ProviderInstantiationAssignment
            { providerInstantiationAssignmentProvider = provider
            , providerInstantiationAssignmentArguments = arguments
            }
          kindedAssignment provider arguments =
            KindedProviderInstantiationAssignment
              { kindedProviderInstantiationAssignmentProvider = provider
              , kindedProviderInstantiationAssignmentArguments = arguments
              }
          bodyKindMismatch = kindedAssignment higherProviderName
            [(ProperTypeKind, tokenType)]
          argumentKindMismatch = kindedAssignment properProviderName
            [(constructorKind, tokenType)]
          properVacuous = kindedAssignment properProviderName
            [(ProperTypeKind, tokenType)]
          higherVacuous = kindedAssignment properProviderName
            [(constructorKind, wrapperConstructor)]
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = target
        , requestGoal = tokenType
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            {exferenceMaximumSteps = 64}
        }
      let expectKindFailure label supplied = case
            runExferenceQueryWithInstantiationAssignments
              session [supplied] request of
            Left failure -> diagnosticCode failure @?=
              Just "DJEX_EXF_ASSIGNMENT_KIND"
            Right _ -> fail $ label ++ " was accepted"
      expectKindFailure
        "a proper type for a Type -> Type provider binder"
        $ assignment higherProviderName [tokenType]
      expectKindFailure
        "a Type -> Type constructor for a Type provider binder"
        $ assignment properProviderName [wrapperConstructor]
      let expectExplicitKindFailure label supplied = case
            runExferenceQueryWithKindedInstantiationAssignments
              session supplied request of
            Left failure -> diagnosticCode failure @?=
              Just "DJEX_EXF_ASSIGNMENT_KIND"
            Right _ -> fail $ label ++ " was accepted"
      expectExplicitKindFailure
        "a supplied kind conflicting with the provider body"
        [bodyKindMismatch]
      expectExplicitKindFailure
        "an argument conflicting with its supplied kind"
        [argumentKindMismatch]
      _ <- expectRight $ runExferenceQueryWithKindedInstantiationAssignments
        session [properVacuous] request
      _ <- expectRight $ runExferenceQueryWithKindedInstantiationAssignments
        session [higherVacuous] request
      expectExplicitKindFailure
        "conflicting kind vectors for one provider"
        [properVacuous, higherVacuous]

  , testCase "accept an ordered proper-type Exference assignment" $ do
      tokenName <- expectRight $ mkIdentifier "ProperAssignmentToken"
      wrapperName <- expectRight $ mkIdentifier "ProperAssignmentWrapper"
      pairName <- expectRight $ mkIdentifier "ProperAssignmentPair"
      providerName <- expectRight $ mkIdentifier "properAssignmentProvider"
      targetName <- expectRight $ mkIdentifier "properAssignmentTarget"
      target <- expectRight $ mkDefinitionName targetName
      let firstVariable = FlexibleVariable 0
          secondVariable = FlexibleVariable 1
          tokenType = TypeConstructor tokenName
          wrapperConstructor = TypeConstructor wrapperName
          pairConstructor = TypeConstructor pairName
          wrapperType argument =
            TypeApplication wrapperConstructor argument
          pairType first second = TypeApplication
            (TypeApplication pairConstructor first) second
          arguments :: [ExferenceType]
          arguments = [tokenType, wrapperType tokenType]
          providerType = ForallType [firstVariable, secondVariable] [] $
            pairType
              (TypeVariable firstVariable)
              (TypeVariable secondVariable)
          goalType = pairType tokenType $ wrapperType tokenType
          declarations =
            [ AbstractTypeDeclaration () tokenName ProperTypeKind
            , AbstractTypeDeclaration () wrapperName $
                FunctionKind ProperTypeKind ProperTypeKind
            , AbstractTypeDeclaration () pairName $
                FunctionKind ProperTypeKind $
                  FunctionKind ProperTypeKind ProperTypeKind
            , ValueDeclaration $ ValueSignature () providerName providerType
            ]
          assignment = ProviderInstantiationAssignment
            { providerInstantiationAssignmentProvider = providerName
            , providerInstantiationAssignmentArguments = arguments
            }
          visibleSpine expression = case expression of
            Global name -> Just (name, [])
            VisibleTypeApplication function argument -> do
              (name, earlier) <- visibleSpine function
              pure (name, earlier ++ [argument])
            _ -> Nothing
      visibleArguments <- expectRight $
        traverse specifiedVisibleTypeArgument arguments
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = target
        , requestGoal = goalType
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            {exferenceMaximumSteps = 512}
        }
      results <- expectRight $ runExferenceQueryWithInstantiationAssignments
        session [assignment] request
      let visibleVectors =
            [ actual
            | candidate <- concatMap
                (batchCandidates . resultSearch) results
            , FunctionClause _ [] body <- [candidateOutput candidate]
            , Just (occurrence, actual) <- [visibleSpine body]
            , occurrence == providerName
            ]
      assertBool
        ("the ordered proper-type assignment was absent: "
          ++ show visibleVectors)
        $ visibleArguments `elem` visibleVectors

  , testCase "keep Exference provider evidence local to an exact name" $ do
      tokenName <- expectRight $ mkIdentifier "LocalEvidenceToken"
      selectedName <- expectRight $ mkIdentifier "selectedProvider"
      unrelatedName <- expectRight $ mkIdentifier "unrelatedProvider"
      targetName <- expectRight $ mkIdentifier "useSelectedProvider"
      target <- expectRight $ mkDefinitionName targetName
      let selectedVariable = FlexibleVariable 0
          unrelatedVariable = FlexibleVariable 1
          identityVariable = FlexibleVariable 2
          identityType = TypeVariable identityVariable
          quantifiedIdentity = ForallType [identityVariable] [] $
            FunctionType identityType identityType
          tokenType = TypeConstructor tokenName
          declarations =
            [ AbstractTypeDeclaration () tokenName ProperTypeKind
            , ValueDeclaration $ ValueSignature () selectedName
                $ ForallType [selectedVariable] [] tokenType
            , ValueDeclaration $ ValueSignature () unrelatedName
                $ ForallType [unrelatedVariable] [] tokenType
            ]
          evidence = ProviderInstantiationCandidate
            { providerInstantiationCandidateProvider = selectedName
            , providerInstantiationCandidateType = quantifiedIdentity
            }
          assignment = ProviderInstantiationAssignment
            { providerInstantiationAssignmentProvider = selectedName
            , providerInstantiationAssignmentArguments = [quantifiedIdentity]
            }
      quantifiedArgument <- expectRight
        $ specifiedVisibleTypeArgument quantifiedIdentity
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = target
        , requestGoal = tokenType
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            {exferenceMaximumSteps = 1024}
        }
      results <- expectRight $ runExferenceQueryWithInstantiationCandidates
        session [evidence] request
      let candidates = concatMap
            (batchCandidates . resultSearch) results
          suppliedTo provider candidate = case candidateOutput candidate of
            FunctionClause _ []
                (VisibleTypeApplication (Global occurrence) argument) ->
              occurrence == provider && argument == quantifiedArgument
            _ -> False
      assertBool "selected provider did not receive its supplied candidate"
        $ any (suppliedTo selectedName) candidates
      assertBool
        "alpha-identical unrelated provider received another provider's evidence"
        $ not $ any (suppliedTo unrelatedName) candidates
      assignmentResults <- expectRight $
        runExferenceQueryWithInstantiationAssignments
          session [assignment] request
      let assignmentCandidates = concatMap
            (batchCandidates . resultSearch) assignmentResults
      assertBool "selected provider did not receive its exact assignment"
        $ any (suppliedTo selectedName) assignmentCandidates
      assertBool
        "alpha-identical unrelated provider received another provider's assignment"
        $ not $ any (suppliedTo unrelatedName) assignmentCandidates

  , testCase "specialize an Exference provider at a closed quantified type" $ do
      tokenName <- expectRight $ mkIdentifier "Token"
      globalName <- expectRight $ mkIdentifier "globalPoly"
      targetName <- expectRight $ mkIdentifier "usePoly"
      target <- expectRight $ mkDefinitionName targetName
      let providerVariable = FlexibleVariable 0
          identityVariable = FlexibleVariable 1
          identityType = TypeVariable identityVariable
          quantifiedIdentity = ForallType [identityVariable] [] $
            FunctionType identityType identityType
          tokenType = TypeConstructor tokenName
          declarations =
            [ AbstractTypeDeclaration () tokenName ProperTypeKind
            , ValueDeclaration $ ValueSignature () globalName
                $ ForallType [providerVariable] [] tokenType
            ]
      quantifiedArgument <- expectRight
        $ specifiedVisibleTypeArgument quantifiedIdentity
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = target
        , requestGoal = FunctionType quantifiedIdentity tokenType
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            { exferenceAllowUnused = True
            , exferenceMaximumSteps = 512
            }
        }
      results <- expectRight $ runExferenceQuery session request
      let candidates = concatMap
            (batchCandidates . resultSearch) results
          isQuantifiedGlobal candidate = case candidateOutput candidate of
            FunctionClause _ [_]
                (VisibleTypeApplication (Global occurrence) argument) ->
              occurrence == globalName && argument == quantifiedArgument
            _ -> False
      candidate <- maybe
        (fail $ "query-supplied forall did not produce globalPoly @(forall): "
          ++ show (map candidateOutput candidates))
        pure
        $ find isQuantifiedGlobal candidates
      candidateResidualConstraints candidate @?= []
      rendered <- expectRight
        $ renderExferenceCandidateDefinition Unqualified candidate
      assertBool "quantified visible evidence was rendered as inferred"
        $ "globalPoly @(forall a0_0. a0_0 -> a0_0)" `isInfixOf` rendered

      let fixture = unlines
            [ "module QuerySelectedQuantifiedProviderFixture where"
            , ""
            , "data Token = Token"
            , ""
            , "globalPoly :: forall a. Token"
            , "globalPoly = Token"
            , ""
            , "usePoly :: (forall x. x -> x) -> Token"
            , rendered
            ]
      withTemporaryHaskellModule fixture $ \sourcePath -> do
        (exitCode, output, errors) <- readProcessWithExitCode "ghc"
          [ "-v0"
          , "-fforce-recomp"
          , "-fno-code"
          , "-fno-write-interface"
          , "-XHaskell2010"
          , "-XAllowAmbiguousTypes"
          , "-XImpredicativeTypes"
          , "-XRankNTypes"
          , "-XTypeApplications"
          , sourcePath
          ] ""
        assertEqual
          ("GHC rejected quantified Exference specialization\nstdout:\n"
            ++ output ++ "\nstderr:\n" ++ errors
            ++ "\ngenerated:\n" ++ rendered)
          ExitSuccess exitCode
  , testCase "compile scoped quantified provider evidence from both engines" $ do
      tokenName <- expectRight $ parseName "ScopedToken"
      djinnTargetName <- expectRight $ mkIdentifier "useDjinnScoped"
      djinnTarget <- expectRight $ mkDefinitionName djinnTargetName
      let hasQuantifiedApplication =
            ("@(forall a0_0. a0_0 -> a0_0)" `isInfixOf`)
          djinnIdentity :: Type String
          djinnIdentity = ForallType ["identity"] [] $
            FunctionType
              (TypeVariable "identity")
              (TypeVariable "identity")
          djinnProvider = ForallType ["chosen"] [] $
            TypeConstructor tokenName
          djinnGoal = FunctionType djinnIdentity $
            FunctionType djinnProvider $ TypeConstructor tokenName
          djinnDeclarations =
            [AbstractTypeDeclaration () tokenName ProperTypeKind]
      djinnEnvironment <- expectRight
        (mkEnvironment djinnDeclarations :: Either
          (EnvironmentError DjinnTypeVariable) DjinnEnvironment)
      djinnSession <- expectRight $ mkDjinnSession djinnEnvironment
      djinnRequest <- expectRight $ mkDjinnRequest QueryRequest
        { requestTarget = djinnTarget
        , requestGoal = djinnGoal
        , requestContexts = []
        , requestOptions = defaultQueryOptions
        }
      djinnResult <- expectRight $ runDjinnQuery djinnSession djinnRequest
      djinnRendered <- traverse
        (expectRight . renderDjinnCandidateDefinition Unqualified)
        $ batchCandidates $ resultSearch djinnResult
      djinnGenerated <- case filter hasQuantifiedApplication djinnRendered of
        candidate : _ -> pure candidate
        [] -> fail $ "Djinn lost scoped quantified evidence: "
          ++ show djinnRendered

      exferenceTargetName <- expectRight $
        mkIdentifier "useExferenceScoped"
      exferenceTarget <- expectRight $
        mkDefinitionName exferenceTargetName
      let providerVariable = FlexibleVariable 0
          identityVariable = FlexibleVariable 1
          ambientVariable = FlexibleVariable 2
          identityType = TypeVariable identityVariable
          quantifiedIdentity = ForallType [identityVariable] [] $
            FunctionType identityType identityType
          ambientType = TypeVariable ambientVariable
          providerType = ForallType [providerVariable] [] ambientType
          exferenceGoal = FunctionType quantifiedIdentity $
            FunctionType providerType ambientType
          exferenceDeclarations = []
      exferenceEnvironment <- expectRight
        (mkEnvironment exferenceDeclarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      exferenceSession <- expectRight $
        mkExferenceSession exferenceEnvironment
      exferenceRequest <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = exferenceTarget
        , requestGoal = exferenceGoal
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            { exferenceAllowUnused = True
            , exferenceMaximumSteps = 512
            }
        }
      exferenceResults <- expectRight $
        runExferenceQuery exferenceSession exferenceRequest
      let findQuantifiedCandidate [] =
            fail "Exference lost scoped quantified evidence"
          findQuantifiedCandidate (candidate : remaining) = do
            rendered <- expectRight $
              renderExferenceCandidateDefinition Unqualified candidate
            if hasQuantifiedApplication rendered
              then pure rendered
              else findQuantifiedCandidate remaining
      exferenceGenerated <- findQuantifiedCandidate $
        concatMap (batchCandidates . resultSearch) exferenceResults

      let fixture = unlines
            [ "module ScopedQuantifiedProviderFixture where"
            , ""
            , "data ScopedToken = ScopedToken"
            , ""
            , "useDjinnScoped ::"
            , "  (forall x. x -> x) ->"
            , "  (forall a. ScopedToken) -> ScopedToken"
            , djinnGenerated
            , ""
            , "useExferenceScoped :: forall r."
            , "  (forall x. x -> x) ->"
            , "  (forall a. r) -> r"
            , exferenceGenerated
            ]
      withTemporaryHaskellModule fixture $ \sourcePath -> do
        (exitCode, output, errors) <- readProcessWithExitCode "ghc"
          [ "-v0"
          , "-fforce-recomp"
          , "-fno-code"
          , "-fno-write-interface"
          , "-XHaskell2010"
          , "-XAllowAmbiguousTypes"
          , "-XImpredicativeTypes"
          , "-XRankNTypes"
          , "-XTypeApplications"
          , sourcePath
          ] ""
        assertEqual
          ("GHC rejected scoped quantified provider evidence\nstdout:\n"
            ++ output ++ "\nstderr:\n" ++ errors
            ++ "\nDjinn generated:\n" ++ djinnGenerated
            ++ "\nExference generated:\n" ++ exferenceGenerated)
          ExitSuccess exitCode
  , testCase
      ("reject phantom provider mismatches and compile concrete exact "
        ++ "assignments through both engines") $
      do
        className <- expectRight $ parseName "Assignable"
        boxName <- expectRight $ parseName "ContextualBox"
        boxConstructorName <- expectRight $ parseName "MkContextualBox"
        mixedName <- expectRight $ parseName "MixedContextual"
        mixedConstructorName <- expectRight $ parseName "MkMixedContextual"
        providerName <- expectRight $ parseName "contextualBoxProvider"
        intLikeName <- expectRight $ parseName "AssignmentIntLike"
        boolLikeName <- expectRight $ parseName "AssignmentBoolLike"
        nestedProviderName <- expectRight $
          parseName "nestedPhantomProvider"
        mixedProviderName <- expectRight $
          parseName "mixedContextualProvider"
        higherProviderName <- expectRight $
          parseName "higherPhantomProvider"
        djinnTargetName <- expectRight $
          mkIdentifier "useDjinnContextualAssignment"
        djinnTarget <- expectRight $ mkDefinitionName djinnTargetName
        let box argument = TypeApplication (TypeConstructor boxName) argument
            mixed argument =
              TypeApplication (TypeConstructor mixedName) argument
            visibleProviderSpine expression = case expression of
              Global occurrence -> Just (occurrence, [])
              VisibleTypeApplication function argument -> do
                (occurrence, earlier) <- visibleProviderSpine function
                pure (occurrence, earlier ++ [argument])
              _ -> Nothing
            containsVisibleProviderSpine occurrence arguments expression =
              visibleProviderSpine expression == Just (occurrence, arguments)
                || case expression of
                  Local _ -> False
                  Global _ -> False
                  Lambda _ body ->
                    containsVisibleProviderSpine occurrence arguments body
                  Apply function argument ->
                    containsVisibleProviderSpine occurrence arguments function
                      || containsVisibleProviderSpine
                        occurrence arguments argument
                  VisibleTypeApplication function _ ->
                    containsVisibleProviderSpine occurrence arguments function
                  Tuple elements -> any
                    (containsVisibleProviderSpine occurrence arguments)
                    elements
                  Hole _ -> False
                  Let _ binding body ->
                    containsVisibleProviderSpine occurrence arguments binding
                      || containsVisibleProviderSpine
                        occurrence arguments body
                  Case scrutinee alternatives ->
                    containsVisibleProviderSpine
                        occurrence arguments scrutinee
                      || any
                        (containsVisibleProviderSpine occurrence arguments
                          . snd)
                        alternatives
            djinnContextualVariable = "contextual"
            djinnContextualType = TypeVariable djinnContextualVariable
            djinnContextualArgument :: Type DjinnTypeVariable
            djinnContextualArgument = ForallType [djinnContextualVariable]
              [Constraint className [djinnContextualType]]
              $ FunctionType djinnContextualType djinnContextualType
            djinnIdentityVariable = "identity"
            djinnIdentityType = TypeVariable djinnIdentityVariable
            djinnIdentityArgument :: Type DjinnTypeVariable
            djinnIdentityArgument = ForallType [djinnIdentityVariable] [] $
              FunctionType djinnIdentityType djinnIdentityType
            djinnProviderType = ForallType ["selected", "hidden"] [] $
              box $ TypeVariable "selected"
            nestedProviderType = ForallType ["assigned"] [] $
              TypeVariable "assigned"
            mixedProviderType = ForallType ["assigned"] [] $
              mixed $ TypeVariable "assigned"
            higherProviderType = ForallType ["shape"] [] $
              TypeApplication
                (TypeVariable "shape")
                (TypeConstructor intLikeName)
            nestedAssignmentType :: Type DjinnTypeVariable
            nestedAssignmentType = box $ TypeConstructor intLikeName
            nestedMismatchType :: Type DjinnTypeVariable
            nestedMismatchType = box $ TypeConstructor boolLikeName
            djinnDeclarations =
              [ ClassDeclaration () className
                  [TypeParameter "classParameter" Nothing] [] []
              , AbstractTypeDeclaration () intLikeName ProperTypeKind
              , AbstractTypeDeclaration () boolLikeName ProperTypeKind
              , DataTypeDeclaration () boxName
                  [TypeParameter "boxParameter" Nothing]
                  [DataConstructor () boxConstructorName []]
              , DataTypeDeclaration () mixedName
                  [TypeParameter "mixedParameter" Nothing]
                  [DataConstructor () mixedConstructorName
                    [ TypeVariable "mixedParameter"
                    , box $ TypeVariable "mixedParameter"
                    ]]
              , ValueDeclaration $ ValueSignature () providerName
                  djinnProviderType
              , ValueDeclaration $ ValueSignature () nestedProviderName
                  nestedProviderType
              , ValueDeclaration $ ValueSignature () mixedProviderName
                  mixedProviderType
              , ValueDeclaration $ ValueSignature () higherProviderName
                  higherProviderType
              ]
            djinnAssignment = ProviderInstantiationAssignment
              { providerInstantiationAssignmentProvider = providerName
              , providerInstantiationAssignmentArguments =
                  [djinnContextualArgument, djinnIdentityArgument]
              }
            nestedAssignment = ProviderInstantiationAssignment
              { providerInstantiationAssignmentProvider = nestedProviderName
              , providerInstantiationAssignmentArguments =
                  [nestedAssignmentType]
              }
            nestedScalarCandidate = ProviderInstantiationCandidate
              { providerInstantiationCandidateProvider = nestedProviderName
              , providerInstantiationCandidateType = nestedAssignmentType
              }
            mixedAssignment = ProviderInstantiationAssignment
              { providerInstantiationAssignmentProvider = mixedProviderName
              , providerInstantiationAssignmentArguments =
                  [TypeConstructor intLikeName]
              }
            mixedScalarCandidate = ProviderInstantiationCandidate
              { providerInstantiationCandidateProvider = mixedProviderName
              , providerInstantiationCandidateType =
                  TypeConstructor intLikeName
              }
            higherAssignment = ProviderInstantiationAssignment
              { providerInstantiationAssignmentProvider = higherProviderName
              , providerInstantiationAssignmentArguments =
                  [TypeConstructor boxName]
              }
        djinnVisibleArguments <- expectRight $ traverse
          specifiedVisibleTypeArgument
          [djinnContextualArgument, djinnIdentityArgument]
        djinnEnvironment <- expectRight
          (mkEnvironment djinnDeclarations :: Either
            (EnvironmentError DjinnTypeVariable) DjinnEnvironment)
        djinnSession <- expectRight $ mkDjinnSession djinnEnvironment
        djinnMismatchTargetName <- expectRight $
          mkIdentifier "rejectDjinnContextualAssignmentMismatch"
        djinnMismatchTarget <- expectRight $
          mkDefinitionName djinnMismatchTargetName
        djinnMismatchRequest <- expectRight $ mkDjinnRequest QueryRequest
          { requestTarget = djinnMismatchTarget
          , requestGoal = box djinnIdentityArgument
          , requestContexts = []
          , requestOptions = defaultQueryOptions
              { optionAlternatives = True
              , optionCutoff = 64
              }
          }
        djinnMismatchResult <- expectRight $
          runDjinnQueryWithInstantiationAssignments
            djinnSession [djinnAssignment] djinnMismatchRequest
        let djinnMismatchCandidates =
              batchCandidates $ resultSearch djinnMismatchResult
        assertBool "the phantom mismatch fixture produced no candidates" $
          not $ null djinnMismatchCandidates
        assertBool
          ("Djinn reused a structurally erased exact assignment at a "
            ++ "nominal mismatch")
          $ not $ any
              (\candidate -> case candidateOutput candidate of
                FunctionClause _ [] body -> containsVisibleProviderSpine
                  providerName djinnVisibleArguments body
                _ -> False)
              djinnMismatchCandidates

        -- The assignment itself can contain an erased datatype application.
        -- Marking only the uninstantiated scheme binder accepts this exploit:
        -- both instantiated bodies lower to the nullary constructor formula,
        -- but GHC cannot use @p @(ContextualBox Int)@ at ContextualBox Bool.
        nestedTargetName <- expectRight $
          mkIdentifier "rejectNestedPhantomAssignmentMismatch"
        nestedTarget <- expectRight $ mkDefinitionName nestedTargetName
        nestedRequest <- expectRight $ mkDjinnRequest QueryRequest
          { requestTarget = nestedTarget
          , requestGoal = nestedMismatchType
          , requestContexts = []
          , requestOptions = defaultQueryOptions
              { optionAlternatives = True
              , optionCutoff = 64
              }
          }
        nestedResult <- expectRight $
          runDjinnQueryWithInstantiationAssignments
            djinnSession [nestedAssignment] nestedRequest
        nestedVisible <- expectRight $
          specifiedVisibleTypeArgument nestedAssignmentType
        let nestedCandidates = batchCandidates $ resultSearch nestedResult
            usesNestedExact candidate = case candidateOutput candidate of
              FunctionClause _ [] body -> containsVisibleProviderSpine
                nestedProviderName [nestedVisible] body
              _ -> False
        assertBool "the nested phantom fixture produced no safe candidate" $
          not $ null nestedCandidates
        assertBool
          "Djinn retained p @(Phantom Int) for a Phantom Bool goal"
          $ not $ any usesNestedExact nestedCandidates
        nestedScalarResult <- expectRight $
          runDjinnQueryWithInstantiationCandidates
            djinnSession [nestedScalarCandidate] nestedRequest
        let nestedScalarCandidates =
              batchCandidates $ resultSearch nestedScalarResult
        assertBool
          "the nested scalar phantom fixture produced no safe candidate"
          $ not $ null nestedScalarCandidates
        assertBool
          "Djinn retained scalar p @(Phantom Int) for a Phantom Bool goal"
          $ not $ any usesNestedExact nestedScalarCandidates

        -- A faithful sibling field must not mask a second path which erases
        -- the same visible choice. Structural projection can reach the
        -- ContextualBox field of MixedContextual Int, but it cannot inhabit
        -- ContextualBox Bool with that value.
        mixedVisible <- expectRight $ specifiedVisibleTypeArgument
          (TypeConstructor intLikeName :: Type DjinnTypeVariable)
        let usesMixedVisible candidate = case candidateOutput candidate of
              FunctionClause _ [] body -> containsVisibleProviderSpine
                mixedProviderName [mixedVisible] body
              _ -> False
        mixedScalarResult <- expectRight $
          runDjinnQueryWithInstantiationCandidates
            djinnSession [mixedScalarCandidate] nestedRequest
        let mixedScalarCandidates =
              batchCandidates $ resultSearch mixedScalarResult
        assertBool
          "the mixed scalar-candidate fixture produced no safe candidate"
          $ not $ null mixedScalarCandidates
        assertBool
          "Djinn retained scalar Mixed Int evidence for a Phantom Bool goal"
          $ not $ any usesMixedVisible mixedScalarCandidates
        mixedExactResult <- expectRight $
          runDjinnQueryWithInstantiationAssignments
            djinnSession [mixedAssignment] nestedRequest
        let mixedExactCandidates =
              batchCandidates $ resultSearch mixedExactResult
        assertBool
          "the mixed exact-assignment fixture produced no safe candidate"
          $ not $ null mixedExactCandidates
        assertBool
          "Djinn retained exact Mixed Int evidence for a Phantom Bool goal"
          $ not $ any usesMixedVisible mixedExactCandidates
        nestedSafeCandidate <- maybe
          (fail $ "Djinn returned no provider-free alternative: "
            ++ show nestedCandidates)
          pure
          $ find
              (\candidate -> case candidateOutput candidate of
                FunctionClause _ [] body -> not $
                  containsVisibleProviderSpine nestedProviderName [] body
                _ -> False)
              nestedCandidates
        nestedSafeGenerated <- expectRight $
          renderDjinnCandidateDefinition Unqualified nestedSafeCandidate

        -- Higher-kinded substitution has to retain the marker after the
        -- assigned constructor becomes saturated by the scheme body.
        higherTargetName <- expectRight $
          mkIdentifier "rejectHigherPhantomAssignmentMismatch"
        higherTarget <- expectRight $ mkDefinitionName higherTargetName
        higherRequest <- expectRight $ mkDjinnRequest QueryRequest
          { requestTarget = higherTarget
          , requestGoal = nestedMismatchType
          , requestContexts = []
          , requestOptions = defaultQueryOptions
              { optionAlternatives = True
              , optionCutoff = 64
              }
          }
        higherResult <- expectRight $
          runDjinnQueryWithInstantiationAssignments
            djinnSession [higherAssignment] higherRequest
        higherVisible <- expectRight $
          specifiedVisibleTypeArgument
            (TypeConstructor boxName :: Type DjinnTypeVariable)
        let higherCandidates = batchCandidates $ resultSearch higherResult
        assertBool "the higher-kinded phantom fixture produced no candidates" $
          not $ null higherCandidates
        assertBool
          "Djinn retained f := Phantom for f Int at a Phantom Bool goal"
          $ not $ any
              (\candidate -> case candidateOutput candidate of
                FunctionClause _ [] body -> containsVisibleProviderSpine
                  higherProviderName [higherVisible] body
                _ -> False)
              higherCandidates

        djinnRequest <- expectRight $ mkDjinnRequest QueryRequest
          { requestTarget = djinnTarget
          , requestGoal = box djinnContextualArgument
          , requestContexts = []
          , requestOptions = defaultQueryOptions
              { optionAlternatives = True
              , optionCutoff = 64
              }
          }
        djinnResult <- expectRight $
          runDjinnQueryWithInstantiationAssignments
            djinnSession [djinnAssignment] djinnRequest
        djinnCandidate <- maybe
          (fail "Djinn lost the exact contextual assignment vector")
          pure
          $ find
              (\candidate -> case candidateOutput candidate of
                FunctionClause _ [] body ->
                  visibleProviderSpine body ==
                    Just (providerName, djinnVisibleArguments)
                _ -> False)
          $ batchCandidates $ resultSearch djinnResult
        djinnGenerated <- expectRight $
          renderDjinnCandidateDefinition Unqualified djinnCandidate

        exferenceTargetName <- expectRight $
          mkIdentifier "useExferenceContextualAssignment"
        exferenceTarget <- expectRight $ mkDefinitionName exferenceTargetName
        let selectedVariable = FlexibleVariable 0
            hiddenVariable = FlexibleVariable 1
            classParameter = FlexibleVariable 2
            contextualVariable = FlexibleVariable 3
            contextualType = TypeVariable contextualVariable
            contextualArgument = ForallType [contextualVariable]
              [Constraint className [contextualType]]
              $ FunctionType contextualType contextualType
            identityVariable = FlexibleVariable 4
            identityType = TypeVariable identityVariable
            identityArgument = ForallType [identityVariable] [] $
              FunctionType identityType identityType
            boxParameter = FlexibleVariable 5
            providerType = ForallType [selectedVariable, hiddenVariable] [] $
              box $ TypeVariable selectedVariable
            exferenceDeclarations =
              [ ClassDeclaration () className
                  [TypeParameter classParameter Nothing] [] []
              , DataTypeDeclaration () boxName
                  [TypeParameter boxParameter Nothing]
                  [DataConstructor () boxConstructorName []]
              , ValueDeclaration $ ValueSignature () providerName providerType
              ]
            exferenceAssignment = ProviderInstantiationAssignment
              { providerInstantiationAssignmentProvider = providerName
              , providerInstantiationAssignmentArguments =
                  [contextualArgument, identityArgument]
              }
        exferenceVisibleArguments <- expectRight $ traverse
          specifiedVisibleTypeArgument [contextualArgument, identityArgument]
        exferenceEnvironment <- expectRight
          (mkEnvironment exferenceDeclarations :: Either
            (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
        exferenceSession <- expectRight $
          mkExferenceSession exferenceEnvironment
        exferenceRequest <- expectRight $ mkExferenceRequest QueryRequest
          { requestTarget = exferenceTarget
          , requestGoal = box contextualArgument
          , requestContexts = []
          , requestOptions = defaultExferenceOptions
              { exferenceMaximumSteps = 1024
              , exferenceMaximumQueueSize = Just 512
              }
          }
        exferenceResults <- expectRight $
          runExferenceQueryWithInstantiationAssignments
            exferenceSession [exferenceAssignment] exferenceRequest
        exferenceCandidate <- maybe
          (fail "Exference lost the exact contextual assignment vector")
          pure
          $ find
              (\candidate -> case candidateOutput candidate of
                FunctionClause _ [] body ->
                  visibleProviderSpine body ==
                    Just (providerName, exferenceVisibleArguments)
                _ -> False)
          $ concatMap (batchCandidates . resultSearch) exferenceResults
        exferenceGenerated <- expectRight $
          renderExferenceCandidateDefinition Unqualified exferenceCandidate

        let fixture = unlines
              [ "module ContextualImpredicativeAssignmentFixture where"
              , ""
              , "class Assignable a"
              , "data AssignmentIntLike"
              , "data AssignmentBoolLike"
              , "data ContextualBox a = MkContextualBox"
              , ""
              , "contextualBoxProvider :: forall selected hidden."
              , "  ContextualBox selected"
              , "contextualBoxProvider = MkContextualBox"
              , ""
              , "nestedPhantomProvider :: forall assigned. assigned"
              , "nestedPhantomProvider = undefined"
              , ""
              , "rejectNestedPhantomAssignmentMismatch ::"
              , "  ContextualBox AssignmentBoolLike"
              , nestedSafeGenerated
              , ""
              , "useDjinnContextualAssignment ::"
              , "  ContextualBox (forall a. Assignable a => a -> a)"
              , djinnGenerated
              , ""
              , "useExferenceContextualAssignment ::"
              , "  ContextualBox (forall a. Assignable a => a -> a)"
              , exferenceGenerated
              ]
        withTemporaryHaskellModule fixture $ \sourcePath -> do
          (exitCode, output, errors) <- readProcessWithExitCode "ghc"
            [ "-v0"
            , "-fforce-recomp"
            , "-fno-code"
            , "-fno-write-interface"
            , "-XHaskell2010"
            , "-XAllowAmbiguousTypes"
            , "-XImpredicativeTypes"
            , "-XRankNTypes"
            , "-XTypeApplications"
            , sourcePath
            ] ""
          assertEqual
            ("GHC rejected exact contextual assignments\nstdout:\n" ++ output
              ++ "\nstderr:\n" ++ errors
              ++ "\nDjinn generated:\n" ++ djinnGenerated
              ++ "\nExference generated:\n" ++ exferenceGenerated)
            ExitSuccess exitCode
  , testCase
      "compile higher-kinded contextual assignments through both engines" $ do
      className <- expectRight $ parseName "HigherContextClass"
      familyName <- expectRight $ parseName "HigherContextFamily"
      naturalName <- expectRight $ parseName "HigherContextNatural"
      tokenName <- expectRight $ parseName "HigherContextToken"
      tokenConstructorName <- expectRight $ parseName "MkHigherContextToken"
      providerName <- expectRight $ parseName "higherContextProvider"
      let higherKind = FunctionKind ProperTypeKind ProperTypeKind
          djinnElement = "element"
          djinnElementType = TypeVariable djinnElement
          djinnContextualArgument :: Type DjinnTypeVariable
          djinnContextualArgument = ForallType [djinnElement]
            [Constraint className [TypeConstructor familyName]] $
              FunctionType djinnElementType djinnElementType
          djinnProviderType = ForallType ["selected"] [] $
            TypeConstructor tokenName
          djinnDeclarations =
            [ AbstractTypeDeclaration () familyName higherKind
            , AbstractTypeDeclaration () naturalName ProperTypeKind
            , DataTypeDeclaration () tokenName []
                [DataConstructor () tokenConstructorName []]
            , ClassDeclaration () className
                [TypeParameter "f" $ Just higherKind] [] []
            , ValueDeclaration $ ValueSignature () providerName
                djinnProviderType
            ]
          djinnAssignment = ProviderInstantiationAssignment
            { providerInstantiationAssignmentProvider = providerName
            , providerInstantiationAssignmentArguments =
                [djinnContextualArgument]
            }
          usesVisibleProvider candidate = case candidateOutput candidate of
            FunctionClause _ [] body ->
              referencesVisibleGlobal providerName body
            _ -> False
      djinnEnvironment <- expectRight
        (mkEnvironment djinnDeclarations :: Either
          (EnvironmentError DjinnTypeVariable) DjinnEnvironment)
      djinnSession <- expectRight $ mkDjinnSession djinnEnvironment
      djinnTargetName <- expectRight $
        mkIdentifier "useDjinnHigherContextAssignment"
      djinnTarget <- expectRight $ mkDefinitionName djinnTargetName
      djinnRequest <- expectRight $ mkDjinnRequest QueryRequest
        { requestTarget = djinnTarget
        , requestGoal = TypeConstructor tokenName
        , requestContexts = []
        , requestOptions = defaultQueryOptions
            { optionAlternatives = True
            , optionCutoff = 64
            }
        }
      djinnResult <- expectRight $
        runDjinnQueryWithInstantiationAssignments
          djinnSession [djinnAssignment] djinnRequest
      djinnCandidate <- maybe
        (fail "Djinn lost the higher-kinded contextual assignment")
        pure $ find usesVisibleProvider $
          batchCandidates $ resultSearch djinnResult
      djinnGenerated <- expectRight $
        renderDjinnCandidateDefinition Unqualified djinnCandidate

      let classParameter = FlexibleVariable 0
          selectedVariable = FlexibleVariable 1
          elementVariable = FlexibleVariable 2
          elementType = TypeVariable elementVariable
          contextualArgument :: Type ExferenceTypeVariable
          contextualArgument = ForallType [elementVariable]
            [Constraint className [TypeConstructor familyName]] $
              FunctionType elementType elementType
          providerType = ForallType [selectedVariable] [] $
            TypeConstructor tokenName
          exferenceDeclarations =
            [ AbstractTypeDeclaration () familyName higherKind
            , AbstractTypeDeclaration () naturalName ProperTypeKind
            , DataTypeDeclaration () tokenName []
                [DataConstructor () tokenConstructorName []]
            , ClassDeclaration () className
                [TypeParameter classParameter $ Just higherKind] [] []
            , ValueDeclaration $ ValueSignature () providerName providerType
            ]
          exferenceAssignment = ProviderInstantiationAssignment
            { providerInstantiationAssignmentProvider = providerName
            , providerInstantiationAssignmentArguments = [contextualArgument]
            }
      exferenceEnvironment <- expectRight
        (mkEnvironment exferenceDeclarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      exferenceSession <- expectRight $
        mkExferenceSession exferenceEnvironment
      exferenceTargetName <- expectRight $
        mkIdentifier "useExferenceHigherContextAssignment"
      exferenceTarget <- expectRight $ mkDefinitionName exferenceTargetName
      exferenceRequest <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = exferenceTarget
        , requestGoal = TypeConstructor tokenName
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            { exferenceMaximumSteps = 1024
            , exferenceMaximumQueueSize = Just 512
            }
        }
      exferenceResults <- expectRight $
        runExferenceQueryWithInstantiationAssignments
          exferenceSession [exferenceAssignment] exferenceRequest
      exferenceCandidate <- maybe
        (fail "Exference lost the higher-kinded contextual assignment")
        pure $ find usesVisibleProvider $
          concatMap (batchCandidates . resultSearch) exferenceResults
      exferenceGenerated <- expectRight $
        renderExferenceCandidateDefinition Unqualified exferenceCandidate

      let fixture = unlines
            [ "module HigherKindedContextualAssignmentFixture where"
            , ""
            , "class HigherContextClass (f :: * -> *)"
            , "data HigherContextFamily a"
            , "data HigherContextNatural"
            , "data HigherContextToken = MkHigherContextToken"
            , ""
            , "higherContextProvider :: forall selected. HigherContextToken"
            , "higherContextProvider = MkHigherContextToken"
            , ""
            , "useDjinnHigherContextAssignment :: HigherContextToken"
            , djinnGenerated
            , ""
            , "useExferenceHigherContextAssignment :: HigherContextToken"
            , exferenceGenerated
            ]
      withTemporaryHaskellModule fixture $ \sourcePath -> do
        (exitCode, output, errors) <- readProcessWithExitCode "ghc"
          [ "-v0"
          , "-fforce-recomp"
          , "-fno-code"
          , "-fno-write-interface"
          , "-XFlexibleContexts"
          , "-XImpredicativeTypes"
          , "-XKindSignatures"
          , "-XRankNTypes"
          , "-XTypeApplications"
          , sourcePath
          ] ""
        assertEqual
          ("GHC rejected higher-kinded contextual assignments\nstdout:\n"
            ++ output ++ "\nstderr:\n" ++ errors
            ++ "\nDjinn generated:\n" ++ djinnGenerated
            ++ "\nExference generated:\n" ++ exferenceGenerated)
          ExitSuccess exitCode
  , testCase
      "compile structural higher-kinded assignments through both engines" $
      do
      pairName <- expectRight $ tupleName Boxed 2
      eitherName <- expectRight $ mkIdentifier "Either"
      leftName <- expectRight $ mkIdentifier "Left"
      rightName <- expectRight $ mkIdentifier "Right"
      naturalName <- expectRight $ mkIdentifier "StructuralNatural"
      booleanName <- expectRight $ mkIdentifier "StructuralBoolean"
      tokenName <- expectRight $ mkIdentifier "StructuralToken"
      tokenConstructorName <- expectRight $ mkIdentifier "MkStructuralToken"
      providerName <- expectRight $ mkIdentifier "structuralProvider"
      className <- expectRight $ mkIdentifier "StructuralContext"
      contextProviderName <- expectRight $
        mkIdentifier "structuralContextProvider"
      let proper = ProperTypeKind
          unary = FunctionKind proper proper
          binary = FunctionKind proper unary
          apply2 constructor first second = TypeApplication
            (TypeApplication constructor first) second
          pairConstructor = TypeConstructor pairName
          eitherConstructor = TypeConstructor eitherName
          naturalType = TypeConstructor naturalName
          booleanType = TypeConstructor booleanName
          tokenType = TypeConstructor tokenName
          pairType = TupleType Boxed [naturalType, booleanType]
          partialEither = TypeApplication eitherConstructor naturalType
          eitherType = TypeApplication partialEither booleanType
          usesProvider provider candidate = case candidateOutput candidate of
            FunctionClause _ _ body -> referencesVisibleGlobal provider body

      let djinnFirstParameter = "eitherLeft"
          djinnSecondParameter = "eitherRight"
          djinnFirstType = TypeVariable djinnFirstParameter
          djinnSecondType = TypeVariable djinnSecondParameter
          djinnFamily = "family"
          djinnPartial = "partial"
          djinnElement = "element"
          djinnElementType = TypeVariable djinnElement
          djinnProviderType = ForallType [djinnFamily, djinnPartial] [] $
            FunctionType
              (apply2 (TypeVariable djinnFamily) naturalType booleanType) $
                FunctionType
                  (TypeApplication (TypeVariable djinnPartial) booleanType)
                  tokenType
          djinnContextualArgument = ForallType [djinnElement]
            [Constraint className [pairConstructor, partialEither]] $
              FunctionType djinnElementType djinnElementType
          djinnContextProviderType = ForallType ["selected"] [] tokenType
          djinnBaseDeclarations =
            [ AbstractTypeDeclaration () naturalName proper
            , AbstractTypeDeclaration () booleanName proper
            , DataTypeDeclaration () eitherName
                [ TypeParameter djinnFirstParameter Nothing
                , TypeParameter djinnSecondParameter Nothing
                ]
                [ DataConstructor () leftName [djinnFirstType]
                , DataConstructor () rightName [djinnSecondType]
                ]
            , DataTypeDeclaration () tokenName []
                [DataConstructor () tokenConstructorName []]
            , ClassDeclaration () className
                [ TypeParameter "f" $ Just binary
                , TypeParameter "g" $ Just unary
                ] [] []
            ]
          djinnStructuralDeclarations = djinnBaseDeclarations ++
            [ValueDeclaration $ ValueSignature () providerName
              djinnProviderType]
          djinnContextDeclarations = djinnBaseDeclarations ++
            [ValueDeclaration $ ValueSignature () contextProviderName
              djinnContextProviderType]
          djinnStructuralAssignment = KindedProviderInstantiationAssignment
            { kindedProviderInstantiationAssignmentProvider = providerName
            , kindedProviderInstantiationAssignmentArguments =
                [ (binary, pairConstructor)
                , (unary, partialEither)
                ]
            }
          djinnContextAssignment = KindedProviderInstantiationAssignment
            { kindedProviderInstantiationAssignmentProvider =
                contextProviderName
            , kindedProviderInstantiationAssignmentArguments =
                [(proper, djinnContextualArgument)]
            }
      djinnStructuralEnvironment <- expectRight
        (mkEnvironment djinnStructuralDeclarations :: Either
          (EnvironmentError DjinnTypeVariable) DjinnEnvironment)
      djinnContextEnvironment <- expectRight
        (mkEnvironment djinnContextDeclarations :: Either
          (EnvironmentError DjinnTypeVariable) DjinnEnvironment)
      djinnStructuralSession <- expectRight $
        mkDjinnSession djinnStructuralEnvironment
      djinnContextSession <- expectRight $
        mkDjinnSession djinnContextEnvironment
      let runDjinn session targetSpelling goal provider assignment = do
            targetName <- expectRight $ mkIdentifier targetSpelling
            target <- expectRight $ mkDefinitionName targetName
            request <- expectRight $ mkDjinnRequest QueryRequest
              { requestTarget = target
              , requestGoal = goal
              , requestContexts = []
              , requestOptions = defaultQueryOptions
                  { optionAlternatives = True
                  , optionCutoff = 64
                  }
              }
            result <- expectRight $
              runDjinnQueryWithKindedInstantiationAssignments
                session [assignment] request
            candidate <- maybe
              (fail $ "Djinn lost structural assignment for " ++ targetSpelling)
              pure $ find (usesProvider provider) $
                batchCandidates $ resultSearch result
            expectRight $
              renderDjinnCandidateDefinition Unqualified candidate
      djinnStructuralGenerated <- runDjinn djinnStructuralSession
        "useDjinnStructuralAssignment"
        (FunctionType pairType $ FunctionType eitherType tokenType)
        providerName djinnStructuralAssignment
      djinnContextGenerated <- runDjinn djinnContextSession
        "useDjinnStructuralContext" tokenType
        contextProviderName djinnContextAssignment

      let exferenceFirstParameter = FlexibleVariable 0
          exferenceSecondParameter = FlexibleVariable 1
          exferenceFamily = FlexibleVariable 2
          exferencePartial = FlexibleVariable 3
          exferenceSelected = FlexibleVariable 4
          exferenceElement = FlexibleVariable 5
          exferenceElementType = TypeVariable exferenceElement
          exferenceProviderType = ForallType
            [exferenceFamily, exferencePartial] [] $
              FunctionType
                (apply2 (TypeVariable exferenceFamily)
                  naturalType booleanType) $
                  FunctionType
                    (TypeApplication
                      (TypeVariable exferencePartial) booleanType)
                    tokenType
          exferenceContextualArgument = ForallType [exferenceElement]
            [Constraint className [pairConstructor, partialEither]] $
              FunctionType exferenceElementType exferenceElementType
          exferenceContextProviderType = ForallType
            [exferenceSelected] [] tokenType
          exferenceBaseDeclarations =
            [ AbstractTypeDeclaration () naturalName proper
            , AbstractTypeDeclaration () booleanName proper
            , DataTypeDeclaration () eitherName
                [ TypeParameter exferenceFirstParameter Nothing
                , TypeParameter exferenceSecondParameter Nothing
                ]
                [ DataConstructor () leftName
                    [TypeVariable exferenceFirstParameter]
                , DataConstructor () rightName
                    [TypeVariable exferenceSecondParameter]
                ]
            , DataTypeDeclaration () tokenName []
                [DataConstructor () tokenConstructorName []]
            , ClassDeclaration () className
                [ TypeParameter (FlexibleVariable 6) $ Just binary
                , TypeParameter (FlexibleVariable 7) $ Just unary
                ] [] []
            ]
          exferenceStructuralDeclarations = exferenceBaseDeclarations ++
            [ValueDeclaration $ ValueSignature () providerName
              exferenceProviderType]
          exferenceContextDeclarations = exferenceBaseDeclarations ++
            [ValueDeclaration $ ValueSignature () contextProviderName
              exferenceContextProviderType]
          exferenceStructuralAssignment =
            KindedProviderInstantiationAssignment
              { kindedProviderInstantiationAssignmentProvider = providerName
              , kindedProviderInstantiationAssignmentArguments =
                  [ (binary, pairConstructor)
                  , (unary, partialEither)
                  ]
              }
          exferenceContextAssignment =
            KindedProviderInstantiationAssignment
              { kindedProviderInstantiationAssignmentProvider =
                  contextProviderName
              , kindedProviderInstantiationAssignmentArguments =
                  [(proper, exferenceContextualArgument)]
              }
      exferenceStructuralEnvironment <- expectRight
        (mkEnvironment exferenceStructuralDeclarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      exferenceContextEnvironment <- expectRight
        (mkEnvironment exferenceContextDeclarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      exferenceStructuralSession <- expectRight $
        mkExferenceSession exferenceStructuralEnvironment
      exferenceContextSession <- expectRight $
        mkExferenceSession exferenceContextEnvironment
      let runExference session targetSpelling goal provider assignment = do
            targetName <- expectRight $ mkIdentifier targetSpelling
            target <- expectRight $ mkDefinitionName targetName
            request <- expectRight $ mkExferenceRequest QueryRequest
              { requestTarget = target
              , requestGoal = goal
              , requestContexts = []
              , requestOptions = defaultExferenceOptions
                  { exferenceMaximumSteps = 2048
                  , exferenceMaximumQueueSize = Just 1024
                  }
              }
            results <- expectRight $
              runExferenceQueryWithKindedInstantiationAssignments
                session [assignment] request
            candidate <- maybe
              (fail $ "Exference lost structural assignment for "
                ++ targetSpelling)
              pure $ find (usesProvider provider) $
                concatMap (batchCandidates . resultSearch) results
            expectRight $
              renderExferenceCandidateDefinition Unqualified candidate
      exferenceStructuralGenerated <- runExference exferenceStructuralSession
        "useExferenceStructuralAssignment"
        (FunctionType pairType $ FunctionType eitherType tokenType)
        providerName exferenceStructuralAssignment
      exferenceContextGenerated <- runExference exferenceContextSession
        "useExferenceStructuralContext" tokenType
        contextProviderName exferenceContextAssignment

      let structuralGenerated =
            [djinnStructuralGenerated, exferenceStructuralGenerated]
          contextGenerated =
            [djinnContextGenerated, exferenceContextGenerated]
          generated = structuralGenerated ++ contextGenerated
      mapM_ (\source -> do
          assertBool ("boxed-pair assignment was not rendered: " ++ source) $
            "@(,)" `isInfixOf` source
          assertBool ("partial Either assignment was not rendered: " ++ source) $
            "Either StructuralNatural" `isInfixOf` source)
        structuralGenerated
      mapM_ (\source -> assertBool
          ("structural contextual heads were not rendered: " ++ source) $
          "StructuralContext (,) (Either StructuralNatural)"
            `isInfixOf` source)
        contextGenerated

      let fixture = unlines
            [ "module StructuralHigherKindedAssignmentFixture where"
            , ""
            , "data StructuralNatural"
            , "data StructuralBoolean"
            , "data StructuralToken = MkStructuralToken"
            , ""
            , "class StructuralContext (f :: * -> * -> *) (g :: * -> *)"
            , ""
            , "structuralProvider :: forall f g."
            , "  f StructuralNatural StructuralBoolean ->"
            , "  g StructuralBoolean -> StructuralToken"
            , "structuralProvider _ _ = MkStructuralToken"
            , ""
            , "structuralContextProvider :: forall selected. StructuralToken"
            , "structuralContextProvider = MkStructuralToken"
            , ""
            , "useDjinnStructuralAssignment ::"
            , "  (StructuralNatural, StructuralBoolean) ->"
            , "  Either StructuralNatural StructuralBoolean -> StructuralToken"
            , djinnStructuralGenerated
            , ""
            , "useExferenceStructuralAssignment ::"
            , "  (StructuralNatural, StructuralBoolean) ->"
            , "  Either StructuralNatural StructuralBoolean -> StructuralToken"
            , exferenceStructuralGenerated
            , ""
            , "useDjinnStructuralContext :: StructuralToken"
            , djinnContextGenerated
            , ""
            , "useExferenceStructuralContext :: StructuralToken"
            , exferenceContextGenerated
            ]
      withTemporaryHaskellModule fixture $ \sourcePath -> do
        (exitCode, output, errors) <- readProcessWithExitCode "ghc"
          [ "-v0"
          , "-fforce-recomp"
          , "-fno-code"
          , "-fno-write-interface"
          , "-XAllowAmbiguousTypes"
          , "-XEmptyDataDecls"
          , "-XFlexibleContexts"
          , "-XImpredicativeTypes"
          , "-XKindSignatures"
          , "-XRankNTypes"
          , "-XTypeApplications"
          , sourcePath
          ] ""
        assertEqual
          ("GHC rejected structural higher-kinded assignments\nstdout:\n"
            ++ output ++ "\nstderr:\n" ++ errors
            ++ "\nGenerated:\n" ++ unlines generated)
          ExitSuccess exitCode
  , testCase
      "compile correlated impredicative instances through both engines" $ do
      let source =
            "(forall a b. f a b) -> "
            ++ "f (forall x. x -> x) (forall y. y -> y -> y)"
          signature target = unlines
            [ target ++ " :: forall f."
            , "  (forall a b. f a b) ->"
            , "  f (forall x. x -> x) (forall y. y -> y -> y)"
            ]

      djinnSession <- sealDjinnEnvironment standardEnvironment
      djinnTarget <- expectRight $ mkIdentifier "correlatedDjinn"
      djinnGoal <- expectRight $ parseHType source
      djinnRequest <- sharedDjinnRequest
        djinnTarget [] defaultQueryOptions djinnGoal
      djinnResult <- expectRight $ runDjinnQuery djinnSession djinnRequest
      resultEvidence djinnResult @?= ValidatedCandidates
      djinnCandidate <- case batchCandidates $ resultSearch djinnResult of
        candidate : _ -> pure candidate
        [] -> fail "Djinn returned correlated evidence without a candidate"
      candidateResidualConstraints djinnCandidate @?= []
      djinnGenerated <- expectRight $
        renderDjinnCandidateDefinition Unqualified djinnCandidate

      checked <- expectRight $ checkSourceEnvironment emptyExferenceSource
      exferenceSession <- expectRight $
        ExferenceCompatibility.mkExferenceSession checked
      exferenceTarget <- expectRight $ mkIdentifier "correlatedExference"
      exferenceRequest <- expectRight $ parseExferenceRequest exferenceSession
        defaultExferenceOptions {exferenceMaximumSteps = 512}
        exferenceTarget "correlated-impredicative" source
      exferenceCandidate <- firstExferenceCandidate =<< expectRight
        (runExferenceQuery exferenceSession exferenceRequest)
      candidateResidualConstraints exferenceCandidate @?= []
      exferenceGenerated <- expectRight $
        renderExferenceCandidateDefinition Unqualified exferenceCandidate

      let fixture = unlines
            [ "module CorrelatedImpredicativeFixture where"
            , ""
            , signature "correlatedDjinn"
            , djinnGenerated
            , ""
            , signature "correlatedExference"
            , exferenceGenerated
            ]
      withTemporaryHaskellModule fixture $ \sourcePath -> do
        (exitCode, output, errors) <- readProcessWithExitCode "ghc"
          [ "-v0"
          , "-fforce-recomp"
          , "-fno-code"
          , "-fno-write-interface"
          , "-XHaskell2010"
          , "-XImpredicativeTypes"
          , "-XRankNTypes"
          , sourcePath
          ] ""
        assertEqual
          ("GHC rejected correlated impredicative instances\nstdout:\n"
            ++ output ++ "\nstderr:\n" ++ errors
            ++ "\nDjinn generated:\n" ++ djinnGenerated
            ++ "\nExference generated:\n" ++ exferenceGenerated)
          ExitSuccess exitCode
  , testCase "compile the quartic rank-N frontier through the Djinn facade" $ do
      let source =
            "(forall a b c d e f g. q) -> "
            ++ "(forall a b c d e f g. r) -> "
            ++ "(forall a b c d e f g. z) -> "
            ++ "(forall a b c d e f g. m) -> "
            ++ "((forall v w x y u t s. q), "
            ++ "(forall v w x y u t s. r), "
            ++ "(forall v w x y u t s. z), "
            ++ "(forall v w x y u t s. m), "
            ++ "(forall e. e -> e), (forall f. f -> f), "
            ++ "(forall g. g -> g), (forall h. h -> h))"
          signature target = unlines
            [ target ++ " :: forall q r z m."
            , "  (forall a b c d e f g. q) ->"
            , "  (forall a b c d e f g. r) ->"
            , "  (forall a b c d e f g. z) ->"
            , "  (forall a b c d e f g. m) ->"
            , "  ( (forall v w x y u t s. q)"
            , "  , (forall v w x y u t s. r)"
            , "  , (forall v w x y u t s. z)"
            , "  , (forall v w x y u t s. m)"
            , "  , (forall e. e -> e)"
            , "  , (forall f. f -> f)"
            , "  , (forall g. g -> g)"
            , "  , (forall h. h -> h)"
            , "  )"
            ]

      djinnSession <- sealDjinnEnvironment standardEnvironment
      djinnTarget <- expectRight $ mkIdentifier "quarticDjinn"
      djinnGoal <- expectRight $ parseHType source
      djinnRequest <- sharedDjinnRequest
        djinnTarget [] defaultQueryOptions djinnGoal
      djinnResult <- expectRight $ runDjinnQuery djinnSession djinnRequest
      resultEvidence djinnResult @?= ValidatedCandidates
      djinnCandidate <- case batchCandidates $ resultSearch djinnResult of
        candidate : _ -> pure candidate
        [] -> fail "Djinn returned quartic candidate evidence without a term"
      candidateResidualConstraints djinnCandidate @?= []
      djinnGenerated <- expectRight $
        renderDjinnCandidateDefinition Unqualified djinnCandidate

      let fixture = unlines
            [ "module QuarticRankNFixture where"
            , ""
            , signature "quarticDjinn"
            , djinnGenerated
            ]
      withTemporaryHaskellModule fixture $ \sourcePath -> do
        (exitCode, output, errors) <- readProcessWithExitCode "ghc"
          [ "-v0"
          , "-fforce-recomp"
          , "-fno-code"
          , "-fno-write-interface"
          , "-XHaskell2010"
          , "-XAllowAmbiguousTypes"
          , "-XImpredicativeTypes"
          , "-XRankNTypes"
          , sourcePath
          ] ""
        assertEqual
          ("GHC rejected the public quartic rank-N candidate\nstdout:\n"
            ++ output ++ "\nstderr:\n" ++ errors
            ++ "\nDjinn generated:\n" ++ djinnGenerated)
          ExitSuccess exitCode
  , testCase "compile the quintic rank-N frontier through the Djinn facade" $ do
      let source =
            "(forall a b c d e f g. q) -> "
            ++ "(forall a b c d e f g. r) -> "
            ++ "(forall a b c d e f g. z) -> "
            ++ "(forall a b c d e f g. m) -> "
            ++ "(forall a b c d e f g. n) -> "
            ++ "((forall v w x y u t s. q), "
            ++ "(forall v w x y u t s. r), "
            ++ "(forall v w x y u t s. z), "
            ++ "(forall v w x y u t s. m), "
            ++ "(forall e. e -> e), (forall f. f -> f), "
            ++ "(forall g. g -> g), (forall h. h -> h), "
            ++ "(forall i. i -> i), (forall v w x y u t s. n))"
          signature target = unlines
            [ target ++ " :: forall q r z m n."
            , "  (forall a b c d e f g. q) ->"
            , "  (forall a b c d e f g. r) ->"
            , "  (forall a b c d e f g. z) ->"
            , "  (forall a b c d e f g. m) ->"
            , "  (forall a b c d e f g. n) ->"
            , "  ( (forall v w x y u t s. q)"
            , "  , (forall v w x y u t s. r)"
            , "  , (forall v w x y u t s. z)"
            , "  , (forall v w x y u t s. m)"
            , "  , (forall e. e -> e)"
            , "  , (forall f. f -> f)"
            , "  , (forall g. g -> g)"
            , "  , (forall h. h -> h)"
            , "  , (forall i. i -> i)"
            , "  , (forall v w x y u t s. n)"
            , "  )"
            ]

      djinnSession <- sealDjinnEnvironment standardEnvironment
      djinnTarget <- expectRight $ mkIdentifier "quinticDjinn"
      djinnGoal <- expectRight $ parseHType source
      djinnRequest <- sharedDjinnRequest
        djinnTarget [] defaultQueryOptions
          { optionAlternatives = False
          , optionSorted = False
          , optionCutoff = 1
          } djinnGoal
      djinnResult <- expectRight $ runDjinnQuery djinnSession djinnRequest
      resultEvidence djinnResult @?= ValidatedCandidates
      djinnCandidate <- case batchCandidates $ resultSearch djinnResult of
        candidate : _ -> pure candidate
        [] -> fail "Djinn returned quintic candidate evidence without a term"
      candidateResidualConstraints djinnCandidate @?= []
      djinnGenerated <- expectRight $
        renderDjinnCandidateDefinition Unqualified djinnCandidate

      let fixture = unlines
            [ "module QuinticRankNFixture where"
            , ""
            , signature "quinticDjinn"
            , djinnGenerated
            ]
      withTemporaryHaskellModule fixture $ \sourcePath -> do
        (exitCode, output, errors) <- readProcessWithExitCode "ghc"
          [ "-v0"
          , "-fforce-recomp"
          , "-fno-code"
          , "-fno-write-interface"
          , "-XHaskell2010"
          , "-XAllowAmbiguousTypes"
          , "-XImpredicativeTypes"
          , "-XRankNTypes"
          , sourcePath
          ] ""
        assertEqual
          ("GHC rejected the public quintic rank-N candidate\nstdout:\n"
            ++ output ++ "\nstderr:\n" ++ errors
            ++ "\nDjinn generated:\n" ++ djinnGenerated)
          ExitSuccess exitCode
  , testCase
      "compile nested quartic rank-N values through both Exference adapters" $
      do
      let inputs =
            [ "(forall a b c d e f g. q)"
            , "(forall a b c d e f g. r)"
            , "(forall a b c d e f g. z)"
            , "(forall a b c d e f g. m)"
            ]
          components =
            [ "(forall v w x y u t s. q)"
            , "(forall v w x y u t s. r)"
            , "(forall v w x y u t s. z)"
            , "(forall v w x y u t s. m)"
            , "(forall e. e -> e)"
            , "(forall f. f -> f)"
            , "(forall g. g -> g)"
            , "(forall h. h -> h)"
            ]
          nestedProducts = foldr1
            (\left right -> "(" ++ left ++ ", " ++ right ++ ")")
            components
          source = foldr (\input body -> input ++ " -> " ++ body)
            nestedProducts inputs
      LoadReport checkedResult _ <- environmentFromFiles [] []
      checked <- expectRight checkedResult
      hseSession <- expectRight $
        ExferenceCompatibility.mkExferenceSession checked
      neutralEnvironment <- expectRight
        (mkEnvironment [] :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      neutralSession <- expectRight $ mkExferenceSession neutralEnvironment
      target <- expectRight $ mkIdentifier "quarticExference"
      let checkSession (label, session) = do
            request <- expectRight $ parseExferenceRequest session
              defaultExferenceOptions
                { exferenceMaximumSteps = 4096
                , exferenceMaximumQueueSize = Just 1024
                }
              target "quartic-rank-n-exference" source
            candidate <- firstExferenceCandidate =<< expectRight
              (runExferenceQuery session request)
            candidateResidualConstraints candidate @?= []
            generated <- expectRight $
              renderExferenceCandidateDefinition Unqualified candidate
            let fixture = unlines
                  [ "module NestedQuarticRankNFixture where"
                  , ""
                  , "quarticExference :: forall q r z m. " ++ source
                  , generated
                  ]
            withTemporaryHaskellModule fixture $ \sourcePath -> do
              (exitCode, output, errors) <- readProcessWithExitCode "ghc"
                [ "-v0"
                , "-fforce-recomp"
                , "-fno-code"
                , "-fno-write-interface"
                , "-XHaskell2010"
                , "-XAllowAmbiguousTypes"
                , "-XImpredicativeTypes"
                , "-XRankNTypes"
                , sourcePath
                ] ""
              assertEqual
                ("GHC rejected the " ++ label
                  ++ " nested quartic Exference candidate\nstdout:\n"
                  ++ output ++ "\nstderr:\n" ++ errors
                  ++ "\nExference generated:\n" ++ generated)
                ExitSuccess exitCode
      mapM_ checkSession
        [ ("checked HSE", hseSession)
        , ("neutral", neutralSession)
        ]
  , testCase "do not guess visible arguments for non-vacuous providers" $ do
      seedName <- expectRight $ mkIdentifier "Seed"
      tokenName <- expectRight $ mkIdentifier "Token"
      globalName <- expectRight $ mkIdentifier "consume"
      targetName <- expectRight $ mkIdentifier "use"
      target <- expectRight $ mkDefinitionName targetName
      let variable = FlexibleVariable 0
          variableType = TypeVariable variable
          seedType = TypeConstructor seedName
          tokenType = TypeConstructor tokenName
          declarations =
            [ AbstractTypeDeclaration () seedName ProperTypeKind
            , AbstractTypeDeclaration () tokenName ProperTypeKind
            , ValueDeclaration $ ValueSignature () globalName
                $ ForallType [variable] []
                $ FunctionType variableType tokenType
            ]
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = target
        , requestGoal = FunctionType seedType tokenType
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            {exferenceMaximumSteps = 256}
        }
      results <- expectRight $ runExferenceQuery session request
      let candidates = concatMap
            (batchCandidates . resultSearch) results
          bodies = [body | candidate <- candidates,
            FunctionClause _ _ body <- [candidateOutput candidate]]
      assertBool "ordinary loaded provider application disappeared"
        $ any (referencesGlobal globalName) bodies
      assertBool "query guessing visibly instantiated a non-vacuous binder"
        $ not $ any (referencesVisibleGlobal globalName) bodies
  , testCase "preserve exact Exference requests behind one canonical plan" $ do
      environment <- expectRight
        (mkEnvironment [] :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      targetName <- expectRight $ mkIdentifier "exactIdentity"
      target <- expectRight $ mkDefinitionName targetName
      arrow <- expectRight $ parseName "(->)"
      let variable = TypeVariable $ FlexibleVariable 0
          exactGoal = TypeApplication
            (TypeApplication (TypeConstructor arrow) variable)
            variable
          canonicalGoal = FunctionType variable variable
          query goal = QueryRequest
            { requestTarget = target
            , requestGoal = goal
            , requestContexts = []
            , requestOptions = defaultExferenceOptions
                {exferenceMaximumSteps = 32}
            }
      exactRequest <- expectRight $ mkExferenceRequest $ query exactGoal
      canonicalRequest <- expectRight
        $ mkExferenceRequest $ query canonicalGoal
      exferenceRequestQuery exactRequest @?= query exactGoal
      exferenceRequestQuery canonicalRequest @?= query canonicalGoal
      assertBool "exact request spellings collapsed in equality"
        $ exactRequest /= canonicalRequest
      exactResults <- expectRight $ runExferenceQuery session exactRequest
      canonicalResults <- expectRight
        $ runExferenceQuery session canonicalRequest
      exactResults @?= canonicalResults
      assertBool "the shared canonical request plan found no identity"
        $ any (not . null . batchCandidates . resultSearch) exactResults
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
  , testCase "propagate parsed spellings to flexible and rigid constraints" $ do
      className <- expectRight $ mkIdentifier "C"
      producerName <- expectRight $ parseName "Fixture.produce"
      target <- expectRight $ mkIdentifier "residualProducer"
      let variable = FlexibleVariable 0
          variableType = TypeVariable variable
          declarations =
            [ ClassDeclaration () className
                [TypeParameter variable Nothing] [] []
            , ValueDeclaration $ ValueSignature () producerName
                $ ForallType [variable]
                    [Constraint className [variableType]] variableType
            ]
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ parseExferenceRequest session
        defaultExferenceOptions
          { exferenceAllowResidualConstraints = True
          , exferenceMaximumSteps = 64
          }
        target "residual-source-hints" "forall source. source"
      candidate <- firstExferenceCandidate =<< expectRight
        (runExferenceQuery session request)
      candidateResidualConstraints candidate @?=
        [Constraint className [TypeVariable $ RigidVariable 0]]
      exferenceCandidateTypeVariableNames
          (candidateDetails candidate) @?=
        Map.fromList
          [ (FlexibleVariable 0, "source")
          , (RigidVariable 0, "source")
          ]
      renderExferenceResidualConstraints candidate @?= Right ["C source"]
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
          (exferenceSessionEnvironment session) @?= []
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
  , testCase "apply backend recursion policies to one shared analysis" $ do
      intName <- expectRight $ parseName "Int"
      aliasName <- expectRight $ parseName "Alias"
      phantomName <- expectRight $ parseName "Phantom"
      loopName <- expectRight $ parseName "Loop"
      erasedName <- expectRight $ parseName "Erased"
      mkLoopName <- expectRight $ parseName "MkLoop"
      mkErasedName <- expectRight $ parseName "MkErased"
      let nominal = TypeConstructor
          apply name argument = TypeApplication (nominal name) argument
          djinnPhantomDeclarations =
            [ AbstractTypeDeclaration () intName ProperTypeKind
            , TypeSynonymDeclaration () phantomName
                [TypeParameter "a" Nothing] $ nominal intName
            , DataTypeDeclaration () erasedName []
                [DataConstructor () mkErasedName
                  [apply phantomName $ nominal erasedName]]
            ]
          djinnRecursiveDeclarations =
            [ TypeSynonymDeclaration () aliasName [] $ nominal loopName
            , DataTypeDeclaration () loopName []
                [DataConstructor () mkLoopName [nominal aliasName]]
            ]
      djinnPhantomEnvironment <- expectRight
        (mkEnvironment djinnPhantomDeclarations :: Either
          (EnvironmentError DjinnTypeVariable) DjinnEnvironment)
      _ <- expectRight $ mkDjinnSession djinnPhantomEnvironment
      djinnRecursiveEnvironment <- expectRight
        (mkEnvironment djinnRecursiveDeclarations :: Either
          (EnvironmentError DjinnTypeVariable) DjinnEnvironment)
      -- Djinn now consumes the shared alias-expanded recursive classification
      -- to retain bounded constructor introduction.  Session sealing must not
      -- rediscover the source spelling as an unsupported recursive graph.
      _ <- expectRight $ mkDjinnSession djinnRecursiveEnvironment

      let exferenceVariable = FlexibleVariable 0
          exferenceDeclarations =
            [ AbstractTypeDeclaration () intName ProperTypeKind
            , TypeSynonymDeclaration () aliasName [] $ nominal loopName
            , TypeSynonymDeclaration () phantomName
                [TypeParameter exferenceVariable Nothing] $ nominal intName
            , DataTypeDeclaration () loopName []
                [DataConstructor () mkLoopName [nominal aliasName]]
            , DataTypeDeclaration () erasedName []
                [DataConstructor () mkErasedName
                  [apply phantomName $ nominal erasedName]]
            ]
      exferenceEnvironment <- expectRight
        (mkEnvironment exferenceDeclarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      exferenceSession <- expectRight $ mkExferenceSession exferenceEnvironment
      exferenceSessionOmissions exferenceSession @?= []
  , testCase "do not reuse erased hints for synonym-introduced binders" $ do
      innerName <- expectRight $ parseName "Fixture.Inner"
      phantomName <- expectRight $ parseName "Fixture.Phantom"
      target <- expectRight $ mkIdentifier "phantomIdentity"
      let introduced = FlexibleVariable 0
          parameter = FlexibleVariable 1
          introducedType = TypeVariable introduced
          declarations =
            [ TypeSynonymDeclaration () innerName []
                $ ForallType [introduced] []
                $ FunctionType introducedType introducedType
            , TypeSynonymDeclaration () phantomName
                [TypeParameter parameter Nothing]
                (TypeConstructor innerName)
            ]
      environment <- expectRight
        (mkEnvironment declarations :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ parseExferenceRequest session
        defaultExferenceOptions
          { exferenceMaximumSteps = 64 }
        target "phantom-source-hints"
        "Fixture.Phantom erased"
      candidate <- firstExferenceCandidate =<< expectRight
        (runExferenceQuery session request)
      exferenceCandidateTypeVariableNames
          (candidateDetails candidate) @?= Map.empty
  , testCase "canonicalize Exference source names for every failure phase" $ do
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
      let sourceName = "partial-alias"
          canonicalSourceName = "partial-alias.hs"
      case parseExferenceRequest session defaultExferenceOptions target
          sourceName "(" of
        Left failure ->
          diagnosticSource failure @?= Just canonicalSourceName
        Right _ -> fail "an incomplete Exference type parsed successfully"
      request <- expectRight $ parseExferenceRequest session
        defaultExferenceOptions target sourceName
        "Fixture.Higher Fixture.Alias"
      case runExferenceQuery session request of
        Left failure -> do
          diagnosticCode failure @?= Just "DJEX_EXF_SYNONYM"
          diagnosticSource failure @?= Just canonicalSourceName
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
      exferenceSessionEnvironment session @?= environment
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
  , testCase "synthesize one recursive Exference elimination layer" $ do
      naturalName <- expectRight $ parseName "Fixture.Natural"
      zeroName <- expectRight $ parseName "Fixture.Zero"
      successorName <- expectRight $ parseName "Fixture.Successor"
      targetName <- expectRight $ mkIdentifier "dispatchNatural"
      target <- expectRight $ mkDefinitionName targetName
      let natural = TypeConstructor naturalName
          result = TypeVariable $ FlexibleVariable 0
          declaration = DataTypeDeclaration () naturalName []
            [ DataConstructor () zeroName []
            , DataConstructor () successorName [natural]
            ]
          goal = FunctionType result
            $ FunctionType (FunctionType natural result)
            $ FunctionType natural result
      environment <- expectRight
        (mkEnvironment [declaration] :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      exferenceSessionOmissions session @?= []
      exferenceSessionDiagnostics session @?= []
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = target
        , requestGoal = goal
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            { exferenceMultiConstructorPatterns = True
            , exferenceMaximumSteps = 256
            }
        }
      candidate <- firstExferenceCandidate =<< expectRight
        (runExferenceQuery session request)
      candidateResidualConstraints candidate @?= []
      case candidateOutput candidate of
        FunctionClause _
            [Bind zeroResult, Bind onSuccessor, Bind scrutinee]
            (Case (Local matchedScrutinee)
              [ (Constructor matchedZero [], Local returnedZero)
              , ( Constructor matchedSuccessor [Bind predecessor]
                , Apply (Local usedSuccessor) (Local usedPredecessor)
                )
              ]) -> do
          matchedScrutinee @?= scrutinee
          matchedZero @?= zeroName
          returnedZero @?= zeroResult
          matchedSuccessor @?= successorName
          usedSuccessor @?= onSuccessor
          usedPredecessor @?= predecessor
        output -> fail $ "unexpected recursive candidate: " ++ show output
  , testCase
      "carry a production Exference spine case through QF_LIA replay" $ do
      payloadName <- expectRight $ parseName "Fixture.Payload"
      spineName <- expectRight $ parseName "Fixture.Spine"
      zeroName <- expectRight $ parseName "Fixture.Zero"
      stepName <- expectRight $ parseName "Fixture.Step"
      targetName <- expectRight $ mkIdentifier "rebuildSpine"
      target <- expectRight $ mkDefinitionName targetName
      let element = FlexibleVariable 0
          payload = TypeConstructor payloadName
          spineOf value = TypeApplication (TypeConstructor spineName) value
          polymorphicSpine = spineOf $ TypeVariable element
          spine = spineOf payload
          declaration = DataTypeDeclaration () spineName
            [TypeParameter element Nothing]
            [ DataConstructor () zeroName []
            , DataConstructor () stepName
                [TypeVariable element, polymorphicSpine]
            ]
          goal = FunctionType spine spine
          roles = [LengthObservedSpine]
          input = LengthVariable $ LengthInput 0
          contractSource = LengthContractSource
            { lengthContractPrecondition = LengthTruth True
            , lengthContractPostcondition = LengthEqual
                (LengthVariable LengthResult)
                (LengthSum [input, LengthLiteral 1])
            }
          isRebuildCase candidate = case typedCandidateTermGraph candidate of
            Right graph -> case eraseTermGraph graph of
              Lambda [Bind inputBinder]
                  (Case (Local scrutinee)
                    [ (Constructor actualZero [], Global returnedZero)
                    , ( Constructor actualStep
                          [Bind payloadBinder, Bind spineTail]
                      , Apply
                          (Apply (Global returnedStep)
                            (Local returnedPayload))
                          (Local returnedTail)
                      )
                    ]) ->
                inputBinder == scrutinee &&
                  actualZero == zeroName &&
                  returnedZero == zeroName &&
                  actualStep == stepName &&
                  returnedStep == stepName &&
                  payloadBinder == returnedPayload &&
                  spineTail == returnedTail
              _ -> False
            Left _ -> False
      environment <- expectRight
        (mkEnvironment
          [ AbstractTypeDeclaration () payloadName ProperTypeKind
          , declaration
          ] :: Either
            (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ mkExferenceRequest QueryRequest
        { requestTarget = target
        , requestGoal = goal
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
            { exferenceMultiConstructorPatterns = True
            , exferenceMaximumSteps = 1024
            , exferenceMaximumQueueSize = Just 1024
            }
        }
      results <- expectRight $ runExferenceTypedQuery session request
      let candidates =
            [ candidate
            | result <- results
            , candidate <- batchCandidates $ resultSearch result
            ]
      candidate <- maybe
        (fail "production Exference search retained no rebuild-case graph")
        pure
        $ find isRebuildCase candidates
      lengthSession <- expectRight $ sealExactSpineCaseLengthSession
        defaultLengthLimits roles (exferenceSessionInventory session)
        (DeclaredListSpine spineName zeroName stepName) []
      contract <- expectRight $ sealRoleAwareLengthContractInContext
        defaultLengthLimits (checkedLengthSessionContext lengthSession)
        roles goal contractSource
      problem <- expectRight $ sealExactSpineCaseLengthTypedCandidateProblem
        defaultLengthProblemLimits lengthSession contract candidate
      let expectedCandidateResult = LengthIf
            (LengthEqual input $ LengthLiteral 0)
            (LengthLiteral 0)
            (LengthSum
              [ LengthLiteral 1
              , LengthMonus input $ LengthLiteral 1
              ])
      checkedLengthCandidateResult
          (checkedLengthProblemCandidate problem) @?=
        expectedCandidateResult
      query <- expectRight $ sealLengthSMTLibQuery
        defaultLengthSMTLibLimits problem
      let ascii = map (fromIntegral . fromEnum)
      lengthSMTLibQueryLogic @?= ascii "QF_LIA"
      assertBool "the sealed query did not select QF_LIA"
        $ ascii "(set-logic QF_LIA)\n" `isInfixOf`
          lengthSMTLibQueryCheckBytes query
      case lengthSMTLibQueryInputSymbols query of
        [symbol] -> do
          evidence <- case validateLengthSMTLibCounterexample
              defaultLengthEvaluationLimits query
              [LengthSMTLibIntegerBinding symbol 3] of
            Left failure -> fail $ "counterexample validation failed: "
              ++ show failure
            Right Nothing -> fail
              "binding 3 did not violate the sealed Length contract"
            Right (Just value) -> pure value
          receipt <- expectRight $ replayBehavioralEvidence
            (checkedLengthProblemBehavioralProblem problem) evidence
          validatedLengthCounterexampleInputs receipt @?= [3]
          validatedLengthCounterexampleResult receipt @?= 3
        symbols -> fail $ "unexpected exact-case input symbols: "
          ++ show symbols
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
      session <- exferenceSessionWithUnaryClass className
      request <- expectRight $ mkExferenceRequest query
      case runExferenceQuery session request of
        Left failure ->
          diagnosticCode failure @?= Just "DJEX_EXF_REQUEST"
        Right _ -> fail "Exference accepted an out-of-scope context variable"
  , testCase "defer cyclic Exference context spines to session arity" $ do
      targetName <- expectRight $ mkIdentifier "cyclicExferenceContext"
      target <- expectRight $ mkDefinitionName targetName
      className <- expectRight $ mkIdentifier "C"
      session <- exferenceSessionWithUnaryClass className
      let argument = TypeVariable $ FlexibleVariable 0
          arguments = argument : arguments
          query = QueryRequest
            { requestTarget = target
            , requestGoal = argument
            , requestContexts = [Constraint className arguments]
            , requestOptions = defaultExferenceOptions
            }
      sealed <- expectWithin "Exference request sealing" $ evaluate
        $ mkExferenceRequest query
      request <- expectRight sealed
      result <- expectWithin "Exference context arity validation" $ evaluate
        $ runExferenceQuery session request
      case result of
        Left failure -> do
          diagnosticCode failure @?= Just "DJEX_EXF_KIND"
          assertBool "Exference lost the bounded arity failure"
            $ any ("ClassArityMismatch" `isInfixOf`)
            $ diagnosticContext failure
        Right _ -> fail
          "Exference accepted a cyclic unary-class argument spine"
  , testCase "validate Exference contexts in source order" $ do
      target <- expectRight $ mkIdentifier "orderedContexts"
      checkedTarget <- expectRight $ mkDefinitionName target
      invalidClass <- expectRight $ mkIdentifier "notAClass"
      validClass <- expectRight $ mkIdentifier "C"
      let variable = TypeVariable $ FlexibleVariable 0
          invalidTuple = TupleType Boxed [variable]
          query contexts = QueryRequest
            { requestTarget = checkedTarget
            , requestGoal = variable
            , requestContexts = contexts
            , requestOptions = defaultExferenceOptions
            }
          firstContext = Constraint invalidClass [variable]
      firstFailure <- case mkExferenceRequest $ query [firstContext] of
        Left failure -> pure failure
        Right _ -> fail "Exference accepted an invalid context class"
      case mkExferenceRequest $ query
          [firstContext, Constraint validClass [invalidTuple]] of
        Left failure -> failure @?= firstFailure
        Right _ -> fail "a later context type hid an earlier class failure"
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
      session <- exferenceSessionWithUnaryClass className
      request <- expectRight $ mkExferenceRequest query
      case runExferenceQuery session request of
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
  , testCase "classify Exference options without attributing type source" $ do
      checked <- expectRight $ checkSourceEnvironment emptyExferenceSource
      session <- expectRight $ ExferenceCompatibility.mkExferenceSession checked
      target <- expectRight $ mkIdentifier "identity"
      let sourceName = "invalid-options"
          source = "a -> a"
      parsed <- expectRight $ parseExferenceRequest session
        defaultExferenceOptions {exferenceMaximumSteps = 0}
        target sourceName source
      programmatic <- expectRight $ mkExferenceRequest
        $ exferenceRequestQuery parsed
      parsed @?= programmatic
      parsedFailure <- case runExferenceQuery session parsed of
        Left failure -> pure failure
        Right _ -> fail "Exference accepted parsed zero-step options"
      programmaticFailure <- case runExferenceQuery session programmatic of
        Left failure -> pure failure
        Right _ -> fail "Exference accepted programmatic zero-step options"
      diagnosticCode parsedFailure @?= Just "DJEX_EXF_OPTIONS"
      diagnosticSource parsedFailure @?= Nothing
      diagnosticSpan parsedFailure @?= Nothing
      diagnosticSource programmaticFailure @?= Nothing
      diagnosticSpan programmaticFailure @?= Nothing
      parsedFailure @?= programmaticFailure
      aliasName <- expectRight $ parseName "Fixture.Identity"
      let variable = FlexibleVariable 0
          alias = TypeSynonymDeclaration () aliasName
            [TypeParameter variable Nothing] $ TypeVariable variable
      aliasEnvironment <- expectRight
        (mkEnvironment [alias] :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      aliasSession <- expectRight $ mkExferenceSession aliasEnvironment
      incompatible <- expectRight $ parseExferenceRequest aliasSession
        defaultExferenceOptions {exferenceMaximumSteps = 0}
        target "invalid-options-alias" "Fixture.Identity a -> a"
      incompatibleFailure <- case runExferenceQuery session incompatible of
        Left failure -> pure failure
        Right _ -> fail
          "session elaboration hid invalid Exference options"
      diagnosticCode incompatibleFailure @?= Just "DJEX_EXF_OPTIONS"
      diagnosticSource incompatibleFailure @?= Nothing
      diagnosticSpan incompatibleFailure @?= Nothing
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
  , testCase "preserve virtual Exference source names verbatim" $ do
      checked <- expectRight $ checkSourceEnvironment emptyExferenceSource
      session <- expectRight $ ExferenceCompatibility.mkExferenceSession checked
      target <- expectRight $ mkIdentifier "broken"
      case parseExferenceRequest session defaultExferenceOptions target
          "<command-line>" "(" of
        Left failure ->
          diagnosticSource failure @?= Just "<command-line>"
        Right _ -> fail "Exference parsed an incomplete virtual-buffer type"
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
  , testCase "retain Exference rank-N bindings without omissions" $ do
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
          (exferenceSessionEnvironment session) @?=
        map (fmap $ const ())
          (environmentDeclarations
            $ inventoryEnvironment $ checkedSourceInventory checked)
      exferenceSessionOmissions session @?= []
      exferenceSessionDiagnostics session @?= []
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
  , testCase "reject caller-forged candidate scope in both stable adapters" $ do
      target <- expectRight $ mkIdentifier "result"
      checkedTarget <- expectRight $ mkDefinitionName target
      let djinnCandidate :: DjinnCandidate
          djinnCandidate = Candidate
            (FunctionClause checkedTarget [] $ Local "free")
            []
            (DjinnCandidateDetails 0 0)
          exferenceCandidate :: ExferenceCandidate
          exferenceCandidate = Candidate
            (FunctionClause checkedTarget [Bind 0, Bind 0] $ Local 0)
            []
            emptyExferenceCandidateDetails
      renderDjinnCandidateExpression Unqualified djinnCandidate @?=
        Left UnboundLocalIdentity
      renderDjinnCandidateDefinition Unqualified djinnCandidate @?=
        Left UnboundLocalIdentity
      renderExferenceCandidateExpression Unqualified exferenceCandidate @?=
        Left DuplicateLocalBinderIdentity
      renderExferenceCandidateDefinition Unqualified exferenceCandidate @?=
        Left DuplicateLocalBinderIdentity
  , testCase "preserve Exference clause binders in expression rendering" $ do
      target <- expectRight $ mkIdentifier "result"
      checkedTarget <- expectRight $ mkDefinitionName target
      sharedGlobal <- expectRight $ parseName "Fixture.value"
      let patternedCandidate = Candidate
            (FunctionClause checkedTarget [Wildcard] $ Global sharedGlobal)
            [] emptyExferenceCandidateDetails
      renderExferenceCandidateExpression FullyQualified patternedCandidate @?=
        Right "\\_ -> Fixture.value"
  , testCase "sanitize caller-created Exference residual hints" $ do
      className <- expectRight $ mkIdentifier "C"
      target <- expectRight $ mkIdentifier "result"
      checkedTarget <- expectRight $ mkDefinitionName target
      let same = FlexibleVariable 0
          duplicate = FlexibleVariable 1
          wildcard = FlexibleVariable 2
          control = FlexibleVariable 3
          partial = FlexibleVariable 4
          infinite = FlexibleVariable 5
          collision = FlexibleVariable 6
          typeFamilyKeyword = FlexibleVariable 7
          forallKeyword = FlexibleVariable 8
          variables =
            [ same, duplicate, wildcard, control, partial, infinite, collision
            , typeFamilyKeyword, forallKeyword
            ]
          details = emptyExferenceCandidateDetails
            { exferenceCandidateTypeVariableNames = Map.fromList
                [ (same, "same")
                , (duplicate, "same")
                , (wildcard, "_")
                , (control, "bad\nname")
                , (partial, 'p' : error "unforced rendering-hint tail")
                , (infinite, repeat 'i')
                , (collision, "a")
                , (typeFamilyKeyword, "family")
                , (forallKeyword, "forall")
                ]
            }
          candidate = Candidate
            (FunctionClause checkedTarget [] $ Local 0)
            [Constraint className $ map TypeVariable variables]
            details
          expected = Right ["C same a' b c d e a g h"]
          candidateWithHint variable hint = Candidate
            (FunctionClause checkedTarget [] $ Local 0)
            [Constraint className [TypeVariable variable]]
            emptyExferenceCandidateDetails
              { exferenceCandidateTypeVariableNames =
                  Map.singleton variable hint
              }
      rendered <- try $ evaluate
        $ renderExferenceResidualConstraints candidate == expected
      case rendered :: Either SomeException Bool of
        Left failure -> fail $ "residual rendering forced an unsafe hint: "
          ++ show failure
        Right matches -> assertBool
          "residual rendering did not validate or freshen raw hints" matches
      let atLimit = replicate 4096 'x'
      renderExferenceResidualConstraints
          (candidateWithHint (FlexibleVariable 9) atLimit) @?=
        Right ["C " ++ atLimit]
      renderExferenceResidualConstraints
          (candidateWithHint (FlexibleVariable 10) $ replicate 4097 'x') @?=
        Right ["C j"]
      asyncResult <- try $ evaluate
        $ renderExferenceResidualConstraints
            (candidateWithHint (FlexibleVariable 11)
              $ 'x' : throw ThreadKilled)
        == Right ["C k"]
      case asyncResult :: Either SomeException Bool of
        Left failure -> case fromException failure of
          Just ThreadKilled -> pure ()
          _ -> fail $ "residual rendering changed an asynchronous exception: "
            ++ show failure
        Right _ -> fail "residual rendering swallowed ThreadKilled"
  , testCase "reject caller-forged Exference residual constraints in order" $ do
      invalidClass <- expectRight $ mkIdentifier "notAClass"
      validClass <- expectRight $ mkIdentifier "C"
      target <- expectRight $ mkIdentifier "result"
      checkedTarget <- expectRight $ mkDefinitionName target
      let variable = FlexibleVariable 0
          validArgument = TypeVariable variable
          candidate constraints = Candidate
            (FunctionClause checkedTarget [] $ Tuple [])
            constraints emptyExferenceCandidateDetails
          invalidNestedType = ForallType []
            [Constraint invalidClass []] validArgument
          classFirst = candidate
            [Constraint invalidClass $ error "unforced invalid-class arguments"]
          argumentFirst = candidate
            $ Constraint validClass [validArgument]
            : Constraint validClass [validArgument, invalidNestedType]
            : error "unforced residual-constraint tail"
      renderExferenceResidualConstraints classFirst @?= Left
        (InvalidResidualConstraintClass 0
          $ InvalidConstraintClass invalidClass)
      renderExferenceResidualConstraintsWithQualification
          Unqualified argumentFirst @?= Left
        (InvalidResidualConstraintArgument 1 1
          $ InvalidTypeConstraint $ InvalidConstraintClass invalidClass)
  , testCase "bound forged residual type validation at invalid tuple width" $ do
      className <- expectRight $ mkIdentifier "C"
      target <- expectRight $ mkIdentifier "result"
      checkedTarget <- expectRight $ mkDefinitionName target
      let variable = FlexibleVariable 0
          oversizedTuple = TupleType Boxed
            $ replicate 65 (TypeVariable variable)
            ++ error "unforced oversized-tuple tail"
          candidate = Candidate
            (FunctionClause checkedTarget [] $ Tuple [])
            [Constraint className [oversizedTuple]]
            emptyExferenceCandidateDetails
      renderExferenceResidualConstraints candidate @?= Left
        (InvalidResidualConstraintArgument 0 0
          $ InvalidTupleTypeArity Boxed 65)
  , testCase "bound caller-created Exference local hints" $ do
      target <- expectRight $ mkIdentifier "result"
      checkedTarget <- expectRight $ mkDefinitionName target
      let candidateWithHint local hint = Candidate
            (FunctionClause checkedTarget []
              $ Lambda [Bind local] $ Local local)
            [] emptyExferenceCandidateDetails
              { exferenceCandidateLocalNames = Map.singleton local hint }
          render local hint = renderExferenceCandidateExpression Unqualified
            $ candidateWithHint local hint
      render 0 ('p' : error "unforced local-hint tail") @?=
        Right "\\v0 -> v0"
      render 1 (repeat 'i') @?= Right "\\a -> a"
      render 2 (replicate 4097 'x') @?= Right "\\b -> b"
      let atLimit = replicate 4096 'x'
      case render 3 atLimit of
        Left failure -> fail $ "maximum-length local hint was rejected: "
          ++ show failure
        Right rendered ->
          filter (`notElem` " \n\r\t") rendered @?=
            "\\" ++ atLimit ++ "->" ++ atLimit
      case render 4 "bad\nname" of
        Left _ -> pure ()
        Right rendered -> fail $ "malformed local hint rendered as " ++ rendered
      case render 5 "forall" of
        Left _ -> pure ()
        Right rendered -> fail $ "reserved local hint rendered as " ++ rendered
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
  checkedTarget <- expectRight $ mkDefinitionName targetName
  compatibility <- expectRight $ inhabitGenerated
    options environment contexts target goal
  canonical <- expectRight $ inhabitResult
    options environment contexts checkedTarget goal
  request <- sharedDjinnRequest targetName contexts options goal
  shared <- expectRight $ runDjinnQuery session request
  assertEqual (label ++ ": checked adapter rebuilt the core result")
    canonical shared
  let search = resultSearch canonical
      metadata = batchMetadata search
  generatedReportEvidence compatibility @?= resultEvidence canonical
  case batchProgress search of
    Completed completion ->
      generatedReportCompletion compatibility @?= completion
    Continuing -> fail $ label ++ ": Djinn returned a nonterminal batch"
  generatedReportCandidates compatibility @?= batchCandidates search
  generatedReportFormula compatibility @?= djinnTranslatedFormula metadata
  generatedReportProof compatibility @?= djinnFirstExploredProof metadata

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

sealDjinnEnvironment :: DjinnCore.Environment -> IO DjinnSession
sealDjinnEnvironment environment = do
  shared <- expectRight $ toSynthesisEnvironment environment
  grounded <- expectRight $ groundEnvironmentKinds shared
  expectRight $ mkDjinnSession grounded

exferenceSessionWithUnaryClass :: Name -> IO ExferenceSession
exferenceSessionWithUnaryClass className = do
  let parameter = FlexibleVariable 0
      declaration = ClassDeclaration () className
        [TypeParameter parameter Nothing] [] []
  environment <- expectRight
    (mkEnvironment [declaration] :: Either
      (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
  expectRight $ mkExferenceSession environment

expectWithin :: String -> IO value -> IO value
expectWithin label action = do
  result <- timeout 2000000 action
  case result of
    Just value -> pure value
    Nothing -> fail $ label ++ " did not terminate"

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
  VisibleTypeApplication function _ -> referencesGlobal target function
  Tuple elements -> any (referencesGlobal target) elements
  Hole _ -> False
  Let _ value body ->
    referencesGlobal target value || referencesGlobal target body
  Case scrutinee alternatives ->
    referencesGlobal target scrutinee ||
      any (referencesGlobal target . snd) alternatives

referencesVisibleGlobal :: Name -> Expression local -> Bool
referencesVisibleGlobal target expression = case expression of
  Local _ -> False
  Global _ -> False
  Lambda _ body -> referencesVisibleGlobal target body
  Apply function argument ->
    referencesVisibleGlobal target function ||
      referencesVisibleGlobal target argument
  VisibleTypeApplication (Global name) _ -> name == target
  VisibleTypeApplication function _ -> referencesVisibleGlobal target function
  Tuple elements -> any (referencesVisibleGlobal target) elements
  Hole _ -> False
  Let _ value body ->
    referencesVisibleGlobal target value ||
      referencesVisibleGlobal target body
  Case scrutinee alternatives ->
    referencesVisibleGlobal target scrutinee ||
      any (referencesVisibleGlobal target . snd) alternatives

candidateEliminatesProduct
  :: Name
  -> Candidate ty details (FunctionClause local)
  -> Bool
candidateEliminatesProduct target candidate = case candidateOutput candidate of
  FunctionClause _ _ body -> eliminates body
 where
  eliminates expression = case expression of
    Local _ -> False
    Global _ -> False
    Lambda _ body -> eliminates body
    Apply function argument -> eliminates function || eliminates argument
    VisibleTypeApplication function _ -> eliminates function
    Tuple elements -> any eliminates elements
    Hole _ -> False
    Let sourcePattern value body ->
      (productPattern sourcePattern && referencesGlobal target value) ||
        eliminates value || eliminates body
    Case scrutinee alternatives ->
      ( referencesGlobal target scrutinee
          && any (productPattern . fst) alternatives
      ) || eliminates scrutinee || any (eliminates . snd) alternatives

  productPattern sourcePattern = case sourcePattern of
    Bind _ -> False
    Wildcard -> False
    Constructor name _ ->
      nameSpecial name == Just (TupleConstructor Boxed 2)
    TuplePattern elements -> length elements == 2
    As _ nested -> productPattern nested

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

withTemporaryHaskellModule
  :: String
  -> (FilePath -> IO value)
  -> IO value
withTemporaryHaskellModule source action = do
  temporaryDirectory <- getTemporaryDirectory
  bracket
    (openTempFile temporaryDirectory "djex-visible-type-application.hs")
    cleanup
    $ \(sourcePath, handle) -> do
        hPutStr handle source
        hClose handle
        action sourcePath
 where
  cleanup (sourcePath, handle) = do
    _ <- tryClose handle
    removeFile sourcePath

  tryClose handle = try (hClose handle) :: IO (Either IOError ())
