{-# LANGUAGE EmptyDataDecls #-}

module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.List (nub)
import Data.Word (Word8)
import Numeric.Natural (Natural)

import qualified Language.Haskell.Synthesis.Internal.Fingerprint as Fingerprint
import qualified Language.Haskell.Synthesis.Internal.Semantic.Problem as Problem
import Language.Haskell.Synthesis.Name
  ( Boxity (..)
  , Name
  , parseName
  , tupleName
  )
import qualified Language.Haskell.Synthesis.Semantic.Observation as Observation
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
  ( assertBool
  , assertFailure
  , testCase
  , (@?=)
  )

data FixtureFingerprint
data FixtureDomain
data FixtureArtifact

main :: IO ()
main = defaultMain fingerprintTests

fingerprintTests :: TestTree
fingerprintTests = testGroup "canonical structural fingerprints"
  [ testCase "retain exact bytes and render deterministic lowercase hex" $ do
      let fingerprint = fixture 1 [0x72]
            [ Fingerprint.FingerprintNatural 0
            , Fingerprint.FingerprintBytes []
            ]
          expectedBytes =
            [ 0x44, 0x4a, 0x45, 0x58, 0x46, 0x50, 0x01
            , 0x01, 0x01, 0x01
            , 0x02, 0x01, 0x72
            , 0x03, 0x05, 0x01, 0x01, 0x00, 0x02, 0x00
            ]
      Fingerprint.fingerprintCanonicalBytes fingerprint @?= expectedBytes
      Fingerprint.fingerprintCode fingerprint @?=
        "444a455846500101010102017203050101000200"
      show fingerprint @?=
        "Fingerprint \"444a455846500101010102017203050101000200\""
      _ <- evaluate $ force fingerprint
      pure ()
  , testCase "make adjacent byte and tag boundaries unambiguous" $ do
      let separatedBytes = fixture 1 [0x72]
            [ Fingerprint.FingerprintBytes [0x01]
            , Fingerprint.FingerprintBytes [0x02]
            ]
          joinedBytes = fixture 1 [0x72]
            [Fingerprint.FingerprintBytes [0x01, 0x02]]
          firstTag = fixture 1 [0x72]
            [ Fingerprint.FingerprintTag [0x01]
                [Fingerprint.FingerprintBytes [0x02, 0x03]]
            ]
          secondTag = fixture 1 [0x72]
            [ Fingerprint.FingerprintTag [0x01, 0x02]
                [Fingerprint.FingerprintBytes [0x03]]
            ]
      assertBool "field concatenation erased a sequence boundary" $
        separatedBytes /= joinedBytes
      assertBool "tag concatenation erased a tag/payload boundary" $
        firstTag /= secondTag
  , testCase "retain field order and order fingerprints by exact bytes" $ do
      let left = fixture 1 [0x72]
            [ Fingerprint.FingerprintNatural 1
            , Fingerprint.FingerprintNatural 2
            ]
          right = fixture 1 [0x72]
            [ Fingerprint.FingerprintNatural 2
            , Fingerprint.FingerprintNatural 1
            ]
      assertBool "ordered fields were canonicalized as an unordered bag" $
        left /= right
      compare left right @?= compare
        (Fingerprint.fingerprintCanonicalBytes left)
        (Fingerprint.fingerprintCanonicalBytes right)
  , testCase "encode arbitrary-precision Natural fields without Int wrapping" $ do
      let maximumInt = fromIntegral (maxBound :: Int) :: Natural
          beyondInt = maximumInt + 1
          maximumFingerprint = fixture 1 [0x72]
            [Fingerprint.FingerprintNatural maximumInt]
          beyondFingerprint = fixture 1 [0x72]
            [Fingerprint.FingerprintNatural beyondInt]
          twoHundredFiftySix = fixture 1 [0x72]
            [Fingerprint.FingerprintNatural 256]
      assertBool "Natural encoding wrapped at the Int boundary" $
        maximumFingerprint /= beyondFingerprint
      assertBool "multi-byte Natural magnitude was not retained" $
        [0x01, 0x02, 0x01, 0x00] `isSuffixOfBytes`
          Fingerprint.fingerprintCanonicalBytes twoHundredFiftySix
  , testCase "encode payload lengths canonically across the LEB128 boundary" $ do
      let length127 = Fingerprint.fingerprintCanonicalBytes $
            fixture 1 (replicate 127 0) []
          length128 = Fingerprint.fingerprintCanonicalBytes $
            fixture 1 (replicate 128 0) []
      take 2 (drop 10 length127) @?= [0x02, 0x7f]
      take 3 (drop 10 length128) @?= [0x02, 0x80, 0x01]
  , testCase "include both semantic version and role in identity" $ do
      let versionOne = fixture 1 [0x72] []
          versionTwo = fixture 2 [0x72] []
          anotherRole = fixture 1 [0x73] []
      assertBool "semantic schema version was omitted" $
        versionOne /= versionTwo
      assertBool "identity role was omitted" $
        versionOne /= anotherRole
  , testCase "encode validated names structurally rather than through Show" $ do
      qualified <- expectRight $ parseName "Fixture.value"
      unqualified <- expectRight $ parseName "value"
      operator <- expectRight $ parseName "(+)"
      boxedTuple <- expectRight $ tupleName Boxed 2
      unboxedTuple <- expectRight $ tupleName Unboxed 2
      let names =
            [qualified, unqualified, operator, boxedTuple, unboxedTuple]
          fingerprints = map nameFingerprint names
      length (nub fingerprints) @?= length names
      assertBool "qualified and unqualified names shared an identity" $
        nameFingerprint qualified /= nameFingerprint unqualified
      assertBool "tuple boxity was erased" $
        nameFingerprint boxedTuple /= nameFingerprint unboxedTuple
  , boundedFingerprintTests
  , behavioralProblemTests
  ]

boundedFingerprintTests :: TestTree
boundedFingerprintTests = testGroup "bounded canonical construction"
  [ testCase "accept the exact byte boundary and preserve canonical bytes" $ do
      let builder = fixtureBuilder 1 [0x62]
            [ Fingerprint.FingerprintNatural 7
            , Fingerprint.FingerprintBytes [8, 9]
            ]
          unbounded = Fingerprint.buildFingerprint builder
          exactLimit = fromIntegral $ length
            $ Fingerprint.fingerprintCanonicalBytes unbounded
      Fingerprint.buildFingerprintWithin exactLimit builder @?= Right unbounded
      Fingerprint.buildFingerprintWithin (exactLimit - 1) builder @?=
        Left Fingerprint.FingerprintLimitExceeded
          { Fingerprint.fingerprintMaximumBytes = exactLimit - 1
          , Fingerprint.fingerprintObservedBytesAtLeast = exactLimit
          }
  , testCase "match every finite field shape at its exact byte boundary" $ do
      qualifiedName <- expectRight $ parseName "Fixture.Inner.value"
      let builder = fixtureBuilder 513 (replicate 128 0x72)
            [ Fingerprint.FingerprintNatural 65536
            , Fingerprint.FingerprintBytes $ replicate 130 0xab
            , Fingerprint.FingerprintTag [0x74, 0x61, 0x67]
                [ Fingerprint.FingerprintSequence
                    [ Fingerprint.FingerprintNatural 256
                    , Fingerprint.FingerprintTag [0x6e, 0x65, 0x73, 0x74]
                        [Fingerprint.FingerprintBytes [1, 2, 3]]
                    ]
                , Fingerprint.FingerprintName qualifiedName
                ]
            , Fingerprint.FingerprintSequence
                [ Fingerprint.FingerprintBytes $ replicate 128 0xcd
                , Fingerprint.FingerprintName qualifiedName
                ]
            ]
          unbounded = Fingerprint.buildFingerprint builder
          exactLimit = fromIntegral $ length
            $ Fingerprint.fingerprintCanonicalBytes unbounded
      Fingerprint.buildFingerprintWithin exactLimit builder @?= Right unbounded
      Fingerprint.buildFingerprintWithin (exactLimit - 1) builder @?=
        Left Fingerprint.FingerprintLimitExceeded
          { Fingerprint.fingerprintMaximumBytes = exactLimit - 1
          , Fingerprint.fingerprintObservedBytesAtLeast = exactLimit
          }
  , testCase "reject infinite byte input after limit plus one" $ do
      let maximumBytes = 32
          builder = fixtureBuilder 1 (repeat 0x72) []
      Fingerprint.buildFingerprintWithin maximumBytes builder @?=
        Left Fingerprint.FingerprintLimitExceeded
          { Fingerprint.fingerprintMaximumBytes = maximumBytes
          , Fingerprint.fingerprintObservedBytesAtLeast = maximumBytes + 1
          }
  , testCase "reject a cyclic field without traversing forever" $ do
      let maximumBytes = 64
          cyclicField = Fingerprint.FingerprintTag [] [cyclicField]
          builder = fixtureBuilder 1 [0x72] [cyclicField]
      Fingerprint.buildFingerprintWithin maximumBytes builder @?=
        Left Fingerprint.FingerprintLimitExceeded
          { Fingerprint.fingerprintMaximumBytes = maximumBytes
          , Fingerprint.fingerprintObservedBytesAtLeast = maximumBytes + 1
          }
  , testCase "bound a huge Natural before constructing its full magnitude" $ do
      let maximumBytes = 32
          hugeNatural = 256 ^ (100000 :: Int)
          builder = fixtureBuilder 1 [0x72]
            [Fingerprint.FingerprintNatural hugeNatural]
      Fingerprint.buildFingerprintWithin maximumBytes builder @?=
        Left Fingerprint.FingerprintLimitExceeded
          { Fingerprint.fingerprintMaximumBytes = maximumBytes
          , Fingerprint.fingerprintObservedBytesAtLeast = maximumBytes + 1
          }
  ]

behavioralProblemTests :: TestTree
behavioralProblemTests = testGroup "solver-neutral behavioral problem envelope"
  [ testCase "bound exact raw bytes productively in fixed part order" $ do
      let limits = Problem.mkRawArtifactLimits 2 3
      artifact <- expectRight
        (Problem.mkBoundedRawArtifact limits [1, 2] [3, 4, 5]
          :: Either Problem.RawArtifactLimitError
              (Problem.BoundedRawArtifact FixtureArtifact))
      Problem.boundedRawArtifactFormat artifact @?= [1, 2]
      Problem.boundedRawArtifactBytes artifact @?= [3, 4, 5]
      Problem.mkBoundedRawArtifact limits [1, 2, 3] (repeat 4) @?=
        Left (Problem.RawArtifactLimitExceeded
          Problem.RawArtifactFormat 2 3)
      Problem.mkBoundedRawArtifact limits [1] (repeat 4) @?=
        Left (Problem.RawArtifactLimitExceeded
          Problem.RawArtifactPayload 3 4)
  , testCase "associate every raw solver result as heuristic-only" $ do
      artifact <- fixtureArtifact
      let problem = fixtureProblem [0x64] 1 2 3 4
          observations =
            [ Observation.SatisfiableObservation artifact
            , Observation.UnsatisfiableObservation artifact
            , Observation.UnknownObservation artifact
            ]
          associated = map (Problem.associateSolverObservation problem)
            observations
      map Problem.associatedObservationResultStrength associated @?=
        [ Problem.RawSolverModelHint
        , Problem.RawSolverUnsatRelativeToEncoding
        , Problem.RawSolverUnknown
        ]
      map Problem.associatedObservationUse associated @?=
        replicate 3 Problem.HeuristicRankingOnly
      map (Problem.replayAssociatedObservation problem) associated @?=
        map Right observations
      map Problem.associatedObservationInventoryFingerprint associated @?=
        replicate 3 (Problem.behavioralProblemInventoryFingerprint problem)
      map Problem.associatedObservationEncodingFingerprint associated @?=
        replicate 3 (Problem.behavioralProblemEncodingFingerprint problem)
      map Problem.associatedObservationCandidateFingerprint associated @?=
        replicate 3 (Problem.behavioralProblemCandidateFingerprint problem)
      map Problem.associatedObservationProblemFingerprint associated @?=
        replicate 3 (Problem.behavioralProblemFingerprint problem)
  , testCase "associate every raw behavioral result as heuristic-only" $ do
      artifact <- fixtureArtifact
      let problem = fixtureProblem [0x64] 1 2 3 4
          observations =
            [ Observation.BehaviorEstablishedObservation artifact
            , Observation.BehaviorViolationObservation artifact
            , Observation.BehaviorBoundedObservation artifact
            , Observation.BehaviorUnknownObservation artifact
            ]
          associated = map (Problem.associateBehavioralObservation problem)
            observations
      map Problem.associatedObservationResultStrength associated @?=
        [ Problem.RawBehaviorEstablishedClaim
        , Problem.RawBehaviorCounterexampleClaim
        , Problem.RawBehaviorBoundedValidation
        , Problem.RawBehaviorUnknown
        ]
      map Problem.associatedObservationUse associated @?=
        replicate 4 Problem.HeuristicRankingOnly
      map (Problem.replayAssociatedObservation problem) associated @?=
        map Right observations
  , testCase "derive raw classifications without forcing artifacts" $ do
      let problem = fixtureProblem [0x64] 1 2 3 4
          solverObservation = Observation.SatisfiableObservation
            (error "solver artifact forced")
              :: Observation.SolverObservation
                  (Problem.BoundedRawArtifact FixtureArtifact)
                  (Problem.BoundedRawArtifact FixtureArtifact)
                  (Problem.BoundedRawArtifact FixtureArtifact)
          solverAssociated =
            Problem.associateSolverObservation problem solverObservation
          behavioralObservation = Observation.BehaviorViolationObservation
            (error "behavioral artifact forced")
              :: Observation.BehavioralObservation
                  (Problem.BoundedRawArtifact FixtureArtifact)
                  (Problem.BoundedRawArtifact FixtureArtifact)
                  (Problem.BoundedRawArtifact FixtureArtifact)
                  (Problem.BoundedRawArtifact FixtureArtifact)
          behavioralAssociated = Problem.associateBehavioralObservation
            problem behavioralObservation
      Problem.associatedSolverObservationStatus solverAssociated @?=
        Observation.SolverSatisfiable
      Problem.associatedObservationResultStrength solverAssociated @?=
        Problem.RawSolverModelHint
      Problem.associatedObservationUse solverAssociated @?=
        Problem.HeuristicRankingOnly
      Problem.associatedObservationResultStrength behavioralAssociated @?=
        Problem.RawBehaviorCounterexampleClaim
      Problem.associatedObservationUse behavioralAssociated @?=
        Problem.HeuristicRankingOnly
  , testCase "fail replay in domain-to-problem precedence" $ do
      artifact <- fixtureArtifact
      let original = fixtureProblem [0x64] 1 2 3 4
          associated = Problem.associateSolverObservation original
            $ Observation.UnknownObservation artifact
          replays =
            [ Problem.replayAssociatedObservation
                (fixtureProblem [0x65] 9 9 9 9) associated
            , Problem.replayAssociatedObservation
                (fixtureProblem [0x64] 9 9 9 9) associated
            , Problem.replayAssociatedObservation
                (fixtureProblem [0x64] 1 9 9 9) associated
            , Problem.replayAssociatedObservation
                (fixtureProblem [0x64] 1 2 9 9) associated
            , Problem.replayAssociatedObservation
                (fixtureProblem [0x64] 1 2 3 9) associated
            ]
      map (either Just $ const Nothing) replays @?=
        map Just
          [ Problem.ReplayDomainMismatch
          , Problem.ReplayInventoryFingerprintMismatch
          , Problem.ReplayEncodingFingerprintMismatch
          , Problem.ReplayCandidateFingerprintMismatch
          , Problem.ReplayProblemFingerprintMismatch
          ]
  , testCase "retain only a private authoritative evidence construction seam" $ do
      let problem = fixtureProblem [0x64] 1 2 3 4
          evidence = Problem.mkBehavioralEvidence problem "lean-receipt"
      Problem.behavioralEvidenceDomain evidence @?= [0x64]
      Problem.replayBehavioralEvidence problem evidence @?=
        Right "lean-receipt"
      Problem.replayBehavioralEvidence
          (fixtureProblem [0x64] 1 2 9 4) evidence @?=
        Left Problem.ReplayCandidateFingerprintMismatch
  ]

fixtureArtifact :: IO (Problem.BoundedRawArtifact FixtureArtifact)
fixtureArtifact = expectRight $ Problem.mkBoundedRawArtifact
  Problem.defaultRawArtifactLimits [0x72, 0x61, 0x77] [1, 2, 3]

fixtureProblem
  :: [Word8]
  -> Natural
  -> Natural
  -> Natural
  -> Natural
  -> Problem.BehavioralProblem FixtureDomain
fixtureProblem domain inventory encoding candidate problem =
  Problem.mkBehavioralProblem domain
    (identityFingerprint [0x69] inventory)
    (identityFingerprint [0x65] encoding)
    (identityFingerprint [0x63] candidate)
    (identityFingerprint [0x70] problem)

identityFingerprint
  :: [Word8]
  -> Natural
  -> Fingerprint.Fingerprint subject
identityFingerprint role value = Fingerprint.buildFingerprint
  Fingerprint.FingerprintBuilder
    { Fingerprint.fingerprintBuilderVersion = 1
    , Fingerprint.fingerprintBuilderRole = role
    , Fingerprint.fingerprintBuilderFields =
        [Fingerprint.FingerprintNatural value]
    }

fixture
  :: Natural
  -> [Word8]
  -> [Fingerprint.FingerprintField]
  -> Fingerprint.Fingerprint FixtureFingerprint
fixture version role fields = Fingerprint.buildFingerprint $
  fixtureBuilder version role fields

fixtureBuilder
  :: Natural
  -> [Word8]
  -> [Fingerprint.FingerprintField]
  -> Fingerprint.FingerprintBuilder FixtureFingerprint
fixtureBuilder version role fields = Fingerprint.FingerprintBuilder
  { Fingerprint.fingerprintBuilderVersion = version
  , Fingerprint.fingerprintBuilderRole = role
  , Fingerprint.fingerprintBuilderFields = fields
  }

nameFingerprint :: Name -> Fingerprint.Fingerprint FixtureFingerprint
nameFingerprint name = fixture 1 [0x6e, 0x61, 0x6d, 0x65]
  [Fingerprint.FingerprintName name]

isSuffixOfBytes :: Eq value => [value] -> [value] -> Bool
isSuffixOfBytes suffix source = reverse suffix `isPrefixOfBytes` reverse source

isPrefixOfBytes :: Eq value => [value] -> [value] -> Bool
isPrefixOfBytes [] _ = True
isPrefixOfBytes _ [] = False
isPrefixOfBytes (expected : remainingExpected) (actual : remainingActual) =
  expected == actual
  && isPrefixOfBytes remainingExpected remainingActual

expectRight :: Show error => Either error value -> IO value
expectRight result = case result of
  Left failure -> assertFailure $ "unexpected rejection: " ++ show failure
  Right value -> pure value
