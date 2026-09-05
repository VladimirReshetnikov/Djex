-- Independent adversarial probes through the public API.  These are type-only
-- queries: only explicitly listed provider signatures enter search.  Their
-- total compiler support bodies are isolated in a separate, opaque module.
-- Run: cabal exec -- runghc -package=djex test-church/probe_scopes.hs
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM, unless, when)
import Data.List (intercalate)
import Language.Haskell.Djex
import Language.Haskell.Djex.Exference.HaskellSrc (parseExferenceRequest)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (ExitSuccess), exitFailure)
import System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

data Probe
  = Probe String Bool String
  | ProviderProbe String Bool String [(String, String)]

probeDetails :: Probe -> (String, Bool, String, [(String, String)])
probeDetails (Probe name expected signature) = (name, expected, signature, [])
probeDetails (ProviderProbe name expected signature providers) =
  (name, expected, signature, providers)

delayedProviderSignature, delayedValueSignature :: String
delayedProviderSignature = "forall a. F (forall b. b -> a) -> Token"
delayedValueSignature = "F (forall c. c -> (forall d. d -> d))"

probes :: [Probe]
probes =
  [ Probe "closedPolyvalue" True
      "(forall a. F a) -> F (forall b. b -> b)"
  , Probe "correlatedAlpha" True
      "(forall a. G a (F a)) -> G (forall q. q -> q) (F (forall z. z -> z))"
  , Probe "compoundAlpha" True
      "(forall a. G (F a) (H a)) -> G (F (forall b. b -> b)) (H (forall c. c -> c))"
  , Probe "shadowedCallback" True
      "forall a. a -> ((forall a. a -> a) -> F a) -> F a"
  , Probe "shadowedResult" True
      "forall a. (forall b. G a b) -> G a (forall a. a -> a)"
  , Probe "nestedPolyResult" True
      "(forall a. F (forall b. b -> a)) -> F (forall c. c -> (forall d. d -> d))"
  , Probe "nestedAmbientResult" True
      "forall x. (forall a. F (forall b. b -> a)) -> F (forall c. c -> (forall d. x -> d -> d))"
  , Probe "nestedAbstractConstructor" True
      "forall f. (forall a. f (forall b. b -> a)) -> f (forall c. c -> (forall d. d -> d))"
  , Probe "nestedRepeatedAmbient" True
      "forall x y. (forall a. F (forall b. b -> a)) -> F (forall c. c -> (forall d. x -> y -> x -> d -> d))"
  , Probe "nestedHigherKindedAmbient" True
      "forall f. (forall a. F (forall b. b -> a)) -> F (forall c. c -> (forall d. f d -> d -> f d))"
  , Probe "nestedCorrelatedAmbient" True
      "forall x y. (forall a. G (forall b. b -> a) a) -> G (forall c. c -> (forall d. x -> y -> d -> x)) (forall e. x -> y -> e -> x)"
  , Probe "nestedAmbientShadowing" True
      "forall a. (forall b. F (forall a. a -> b)) -> F (forall b. b -> (forall c. a -> c -> a))"
  , Probe "nestedCorrelatedResult" True
      "(forall a. G (forall b. b -> a) a) -> G (forall x. x -> (forall y. y -> y)) (forall z. z -> z)"
  , Probe "successiveQuantifiers" True
      "(forall a. (forall b. G a b)) -> G (forall x. x -> x) (forall y. y -> y)"
  , Probe "outerRigidCapture" True
      "forall a. ((forall b. a -> b -> b) -> F a) -> F a"
  , Probe "higherKindedCorrelation" True
      "forall f x. (forall a b. G (f a) (f b)) -> G (f x) (f (forall z. z -> z))"
  , Probe "higherKindedSubstitution" True
      "(forall f a. G (f a) a) -> G (F (forall z. z -> z)) (forall y. y -> y)"
  , Probe "scopedPolyvalueForwarding" True
      "(forall a. a -> a) -> ((forall x. x -> x) -> Token) -> Token"
  , Probe "delayedArgumentInstantiation" True
      "(forall a. F (forall b. b -> a) -> Token) -> F (forall c. c -> (forall d. d -> d)) -> Token"
  , Probe "delayedAmbientArgument" True
      "forall x. (forall a. F (forall b. b -> a) -> Token) -> F (forall c. c -> (forall d. x -> d -> d)) -> Token"
  , ProviderProbe "delayedGlobalArgument" True "Token"
      [("delayedProvider", delayedProviderSignature), ("delayedValue", delayedValueSignature)]
  , ProviderProbe "delayedGlobalAmbientArgument" True
      "forall x. F (forall c. c -> (forall d. x -> d -> d)) -> Token"
      [("delayedProvider", delayedProviderSignature)]
  , Probe "polymorphicResultAfterArgument" True
      "forall seed. seed -> (seed -> (forall a. a -> F a)) -> F (forall b. b -> b)"
  , Probe "constructPolymorphicArgument" True
      "(forall a. a -> F a) -> F (forall b. b -> b)"
  , Probe "constructTwoPolymorphicArguments" True
      "(forall a b. a -> b -> G a b) -> G (forall c. c -> c) (forall d e. d -> e -> d)"
  , Probe "constructAmbientPolymorphicArgument" True
      "forall x. (forall a. a -> F a) -> F (forall b. x -> b -> x)"
  , Probe "constructHigherKindedPolymorphicArgument" True
      "forall g. (forall a. a -> F a) -> F (forall b. g b -> g b)"
  , ProviderProbe "polymorphicResultAfterUnit" True
      "(() -> (forall a. a -> F a)) -> F (forall b. b -> b)"
      [("unitSeed", "()")]
  , Probe "rejectDirectEscape" False
      "(forall a. F (forall b. b -> a)) -> F (forall c. c -> c)"
  , Probe "rejectEscapeBeneathForall" False
      "(forall a. F (forall b. b -> a)) -> F (forall c. c -> (forall d. d -> c))"
  , Probe "rejectCorrelatedMismatch" False
      "(forall a. G a a) -> G (forall b. b -> b) (forall c d. c -> d -> c)"
  , Probe "rejectIndirectEscape" False
      "(forall a. G (forall b. b -> a) a) -> G (forall c. c -> c) (forall d. d -> d)"
  , Probe "rejectAmbientRigidSpecialization" False
      "forall a. a -> (forall b. b)"
  , Probe "rejectShadowedProviderBinder" False
      "(forall a. F (forall a. a -> a)) -> F (forall a. a -> Token)"
  , Probe "rejectApplicationEscape" False
      "(forall a. F (forall b. G b a)) -> F (forall c. G c c)"
  , Probe "rejectHigherKindedEscape" False
      "(forall f. F (forall b. f b)) -> F (forall c. G c c)"
  , Probe "rejectEmptyPolytypeConstruction" False
      "(forall a. a -> F a) -> F (forall b. b)"
  , Probe "rejectAmbientPolytypeEscape" False
      "forall x. x -> (forall a. a -> F a) -> F (forall b. b)"
  , Probe "rejectTwoEmptyPolytypes" False
      "(forall a b. a -> b -> G a b) -> G (forall c. c) (forall d. d)"
  ]

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetBuffering stdout LineBuffering
  createDirectoryIfMissing True "test-church/results/scopes"
  writeFile "test-church/results/scopes/ScopeProviders.hs" $ unlines
    [ "{-# LANGUAGE RankNTypes, ImpredicativeTypes, NoImplicitPrelude, NoPolyKinds, EmptyDataDecls #-}"
    , "module ScopeProviders (F, H, G, Token, delayedProvider, delayedValue, unitSeed) where"
    , "data F a = F"
    , "data H a"
    , "data G a b"
    , "data Token = Token"
    , "delayedProvider :: " ++ delayedProviderSignature
    , "delayedProvider _ = Token"
    , "delayedValue :: " ++ delayedValueSignature
    , "delayedValue = F"
    , "unitSeed :: ()"
    , "unitSeed = ()"
    ]
  failures <- forM ["djinn", "exference"] $ \engine -> do
    results <- forM probes $ \probe -> do
      let (name, shouldSucceed, _, _) = probeDetails probe
      attempted <- try $ timeout 3000000 $ do
        definition <- synthesize engine probe
        _ <- evaluate $ maybe 0 length definition
        pure definition
      let (status, success, definition) = case attempted :: Either SomeException (Maybe (Maybe String)) of
            Left problem -> ("ERROR " ++ show problem, False, Nothing)
            Right Nothing -> ("INCONCLUSIVE_TIMEOUT", False, Nothing)
            Right (Just Nothing) ->
              (if shouldSucceed then "MISSING" else "NO_CANDIDATE_WITHIN_BUDGET", not shouldSucceed, Nothing)
            Right (Just (Just term)) ->
              (if shouldSucceed then "SYNTHESIZED" else "UNEXPECTED_CANDIDATE", shouldSucceed, Just term)
      putStrLn $ engine ++ "\t" ++ name ++ "\t" ++ status
      pure (probe, success, definition, status)
    let accepted = [(probe, term) | (probe, _, Just term, _) <- results]
    sourcePaths <- forM accepted $ \(probe, term) -> do
      let (name, _, signature, providers) = probeDetails probe
          sourcePath = "test-church/results/scopes/" ++ engine ++ "-" ++ name ++ ".hs"
          fixture = unlines
            [ "{-# LANGUAGE RankNTypes, ImpredicativeTypes, ScopedTypeVariables #-}"
            , "{-# LANGUAGE NoImplicitPrelude, TypeApplications, NoPolyKinds #-}"
            , "module Scope_" ++ name ++ " where"
            , "import ScopeProviders (" ++ intercalate ", " (["F", "H", "G", "Token"] ++ map fst providers) ++ ")"
            , name ++ " :: " ++ signature
            , term
            ]
      writeFile sourcePath fixture
      pure sourcePath
    writeFile ("test-church/results/scopes/" ++ engine ++ "-results.tsv") $ unlines
      [name ++ "\t" ++ show expected ++ "\t" ++ status
      | (probe, _, _, status) <- results, let (name, expected, _, _) = probeDetails probe]
    (compilerExit, output, errors) <- readProcessWithExitCode "ghc"
      (["-v0", "-fno-code", "-fno-write-interface", "-fforce-recomp", "-itest-church/results/scopes"] ++ sourcePaths) ""
    writeFile ("test-church/results/scopes/" ++ engine ++ "-ghc.txt") $ output ++ errors
    putStrLn $ "GHC " ++ engine ++ ": " ++ show compilerExit
    when (compilerExit /= ExitSuccess) $ putStrLn errors
    pure $ compilerExit /= ExitSuccess || any (\(_, passed, _, _) -> not passed) results
  when (or failures) exitFailure

synthesize :: String -> Probe -> IO (Maybe String)
synthesize engine probe = do
  let (name, shouldSucceed, signature, providers) = probeDetails probe
      -- Negative queries test that no unsound candidate escapes a bounded
      -- search. They are not decision procedures for System F inhabitation;
      -- spend a smaller uniform budget on those intentionally empty goals.
      searchBudget = if shouldSucceed then 10000 else 1000
  f <- expectRight $ mkIdentifier "F"
  g <- expectRight $ mkIdentifier "G"
  h <- expectRight $ mkIdentifier "H"
  token <- expectRight $ mkIdentifier "Token"
  target <- expectRight $ mkIdentifier name
  let unary = FunctionKind ProperTypeKind ProperTypeKind
      binary = FunctionKind ProperTypeKind unary
      declarations =
        [ AbstractTypeDeclaration () f unary
        , AbstractTypeDeclaration () g binary
        , AbstractTypeDeclaration () h unary
        , AbstractTypeDeclaration () token ProperTypeKind
        ]
  if engine == "djinn"
    then do
      baseEnvironment <- expectRight $ mkEnvironment declarations
      baseSession <- expectRight $ mkDjinnSession baseEnvironment
      values <- forM providers $ \(providerName, providerSignature) -> do
        identifier <- expectRight $ mkIdentifier providerName
        parsed <- expectRight $ parseDjinnRequest baseSession defaultQueryOptions identifier "scope-provider" providerSignature
        pure $ ValueDeclaration $ ValueSignature () identifier $ requestGoal $ djinnRequestQuery parsed
      environment <- expectRight $ mkEnvironment $ declarations ++ values
      session <- expectRight $ mkDjinnSession environment
      request <- expectRight $ parseDjinnRequest session
        defaultQueryOptions {optionSorted = False, optionCutoff = 1, optionBudget = Just searchBudget}
        target "scope-probe" signature
      result <- expectRight $ runDjinnQuery session request
      case batchCandidates $ resultSearch result of
        candidate : _ -> Just <$> expectRight (renderDjinnCandidateDefinition Unqualified candidate)
        [] -> pure Nothing
    else do
      baseEnvironment <- expectRight $ mkEnvironment declarations
      baseSession <- expectRight $ mkExferenceSession baseEnvironment
      values <- forM providers $ \(providerName, providerSignature) -> do
        identifier <- expectRight $ mkIdentifier providerName
        parsed <- expectRight $ parseExferenceRequest baseSession defaultExferenceOptions identifier "scope-provider" providerSignature
        pure $ ValueDeclaration $ ValueSignature () identifier $ requestGoal $ exferenceRequestQuery parsed
      environment <- expectRight $ mkEnvironment $ declarations ++ values
      session <- expectRight $ mkExferenceSession environment
      request <- expectRight $ parseExferenceRequest session
        defaultExferenceOptions {exferenceAllowUnused = True, exferenceMaximumSteps = fromIntegral searchBudget}
        target "scope-probe" signature
      results <- expectRight $ runExferenceQuery session request
      case concatMap (batchCandidates . resultSearch) results of
        candidate : _ -> do
          unless (null $ candidateResidualConstraints candidate) $ fail "unresolved constraints"
          Just <$> expectRight (renderExferenceCandidateDefinition Unqualified candidate)
        [] -> pure Nothing

expectRight :: Show failure => Either failure value -> IO value
expectRight = either (fail . show) pure
