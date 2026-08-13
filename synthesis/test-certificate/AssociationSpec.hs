module AssociationSpec
  ( tests
  , typedCandidateCertificateGraphFixture
  ) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Word (Word8)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Constraint (Constraint (..))
import qualified Language.Haskell.Synthesis.Generated as Generated
import qualified Language.Haskell.Synthesis.Fingerprint as CanonicalFingerprint
import Language.Haskell.Synthesis.Internal.TypedGenerated.Certificate
import Language.Haskell.Synthesis.Internal.TypedGenerated.Certificate.Association
import qualified Language.Haskell.Synthesis.Internal.TypedGenerated.Fingerprint
  as CertificateFingerprint
import Language.Haskell.Synthesis.Name (Boxity (Boxed), Name, parseName)
import Language.Haskell.Synthesis.Type (Type (..), Variable (..))
import Language.Haskell.Synthesis.TypedGenerated
  ( CertificateId
  , OccurrenceId
  , TermGraphError (..)
  , TermGraphSource (..)
  , TermNode (..)
  , TermNodeForm (..)
  , TypedPattern (..)
  , TypedPatternNode (..)
  , TypeApplicationWitness (..)
  , TypeStructure (..)
  , TermNodeId
  , certificateId
  , defaultTermGraphLimits
  , eraseTermGraph
  , occurrenceId
  , occurrenceIdValue
  , sealTermGraph
  , sharedTypeStructure
  , termGraphMetrics
  , termGraphNodes
  , termNodeId
  , termNodeIdValue
  )
import qualified Language.Haskell.Synthesis.TypedGenerated.Fingerprint as Fingerprint
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool
  , assertFailure
  , testCase
  , (@?=)
  )

type TestVariable = Variable String
type TestType = Type TestVariable
type TestLocal = Int

-- Kept behind the private certificate-suite module boundary so carrier tests
-- exercise the same stamped graph atom as the association tests without
-- duplicating its substantial sealing fixture.
typedCandidateCertificateGraphFixture
  :: IO (CheckedTypeApplicationCertificateGraph TestVariable TestLocal)
typedCandidateCertificateGraphFixture =
  expectRight $ sealGraph oneSlotGraph [oneSlotOrigin]

tests :: TestTree
tests = testGroup "atomic certificate graph occurrence associations"
  [ successTests
  , certificateFingerprintTests
  , observationTests
  , occurrenceTests
  , demandTests
  ]

certificateFingerprintTests :: TestTree
certificateFingerprintTests = testGroup "carrier-aware canonical fingerprints"
  [ testCase "empty carriers delegate literally to v1 bytes and failures" $ do
      checked <- expectRight $ sealGraph plainGlobalGraph []
      let projected = checkedTypeApplicationCertificateGraph checked
          legacy maximumBytes =
            CertificateFingerprint.fingerprintTermGraphWithTypeStructure
              sharedTypeStructure defaultTermGraphLimits maximumBytes projected
          carrier maximumBytes = fingerprintCertificateGraphWith
            sharedTypeStructure maximumBytes checked
      carrier CertificateFingerprint.defaultTermGraphFingerprintByteLimit
        @?= legacy CertificateFingerprint.defaultTermGraphFingerprintByteLimit
      carrier 0 @?= legacy 0
      baseline <- expectRight $ carrier
        CertificateFingerprint.defaultTermGraphFingerprintByteLimit
      let exact = canonicalFingerprintSize baseline
      carrier exact @?= Right baseline
      carrier (exact - 1) @?= Left
        (CertificateFingerprint.TermGraphFingerprintByteLimitExceeded
          (exact - 1) exact)

      malformed <- expectRight $
        sealGraphWith annotationPermissiveTypeStructure
          invalidAnnotationGraph []
      let malformedGraph = checkedTypeApplicationCertificateGraph malformed
          legacyFailure =
            CertificateFingerprint.fingerprintTermGraphWithTypeStructure
              sharedTypeStructure defaultTermGraphLimits
              CertificateFingerprint.defaultTermGraphFingerprintByteLimit
              malformedGraph
      fingerprintCertificateGraphWith sharedTypeStructure
          CertificateFingerprint.defaultTermGraphFingerprintByteLimit malformed
        @?= legacyFailure
  , testCase "encode one checked specialization as a v2 semantic row" $ do
      checked <- expectRight $ sealGraph oneSlotGraph [oneSlotOrigin]
      fingerprint <- expectRight $ fingerprintCertificateGraph checked
      let bytes = CanonicalFingerprint.fingerprintCanonicalBytes fingerprint
      take 10 bytes @?=
        [0x44, 0x4a, 0x45, 0x58, 0x46, 0x50, 0x01, 0x01, 0x01, 0x02]
      mapM_ (assertCanonicalTag bytes)
        [ "shared-typed-term-graph/v2"
        , "certificate-semantic-associations/v1"
        , "certificate-semantic-reference"
        , "certificate-association"
        , "certificate-step"
        , "source-bound"
        ]
      assertCanonicalTagOrder bytes
        [ "dialect"
        , "normalization"
        , "root"
        , "certificate-associations"
        ]
      assertCanonicalTagOrder bytes
        [ "owner-scheme"
        , "steps"
        , "source"
        , "selected"
        , "result"
        , "activated-obligations"
        ]
      occurrencesOf (encodedTagBytes "certificate-semantic-reference") bytes
        @?= 1
      Fingerprint.fingerprintSharedTermGraph defaultTermGraphLimits
        Fingerprint.defaultTermGraphFingerprintByteLimit
        (checkedTypeApplicationCertificateGraph checked)
        @?= Left (Fingerprint.TermGraphFingerprintUnsupportedCertificate cert7)
  , testCase "retain ordered two-slot semantics independent of node-table order" $ do
      first <- expectRight $ sealGraph twoSlotGraph [twoSlotOrigin]
      reordered <- expectRight $
        sealGraph reorderedTwoSlotGraph [twoSlotOrigin]
      firstFingerprint <- expectRight $ fingerprintCertificateGraph first
      reorderedFingerprint <- expectRight $
        fingerprintCertificateGraph reordered
      firstFingerprint @?= reorderedFingerprint
      let bytes = CanonicalFingerprint.fingerprintCanonicalBytes firstFingerprint
      occurrencesOf (encodedTagBytes "certificate-semantic-reference") bytes
        @?= 2
      occurrencesOf (encodedTagBytes "certificate-step") bytes @?= 2
      assertCanonicalReferenceOrder bytes [(0, 0), (0, 1)]
  , testCase "canonicalize two rooted rows across every raw coordinate" $ do
      first <- expectRight $ sealGraph twoRowGraph
        [twoRowLeftOrigin cert7, twoRowRightOrigin cert8]
      renumbered <- expectRight $ sealGraph renumberedTwoRowGraph
        [twoRowRightOrigin cert3, twoRowLeftOrigin cert19]
      firstFingerprint <- expectRight $ fingerprintCertificateGraph first
      renumberedFingerprint <- expectRight $
        fingerprintCertificateGraph renumbered
      firstFingerprint @?= renumberedFingerprint
      let bytes = CanonicalFingerprint.fingerprintCanonicalBytes firstFingerprint
      occurrencesOf (encodedTagBytes "certificate-association") bytes @?= 2
      occurrencesOf (encodedTagBytes "certificate-semantic-reference") bytes
        @?= 2
      assertCanonicalReferenceOrder bytes [(0, 0), (1, 0)]
  , testCase "retain contextual obligations in the semantic row" $ do
      plain <- expectRight $ sealGraph oneSlotGraph [oneSlotOrigin]
      contextual <- expectRight $
        sealGraph contextualGraph [contextualOrigin]
      plainFingerprint <- expectRight $ fingerprintCertificateGraph plain
      contextualFingerprint <- expectRight $
        fingerprintCertificateGraph contextual
      assertBool "activated constraint disappeared from the identity" $
        contextualFingerprint /= plainFingerprint
      let bytes = CanonicalFingerprint.fingerprintCanonicalBytes
            contextualFingerprint
      mapM_ (assertCanonicalTag bytes)
        ["activated-obligations", "constraint"]
      occurrencesOf (encodedTagBytes "constraint") bytes @?= 5
  , testCase "keep an untagged returned-polymorphic suffix outside its row" $ do
      checked <- expectRight $
        sealGraph returnedPolymorphicSuffixGraph [returnedPolymorphicOrigin]
      fingerprint <- expectRight $ fingerprintCertificateGraph checked
      let bytes = CanonicalFingerprint.fingerprintCanonicalBytes fingerprint
      occurrencesOf (encodedTagBytes "certificate-semantic-reference") bytes
        @?= 1
      occurrencesOf (encodedTagBytes "certificate-step") bytes @?= 1
      assertCanonicalTag bytes "selection-bound"
  , testCase "ignore alpha spelling but retain free-variable equality patterns" $ do
      original <- expectRight $
        sealGraph returnedPolymorphicSuffixGraph [returnedPolymorphicOrigin]
      renamed <- expectRight $
        sealGraph renamedReturnedPolymorphicSuffixGraph
          [renamedReturnedPolymorphicOrigin]
      originalFingerprint <- expectRight $ fingerprintCertificateGraph original
      renamedFingerprint <- expectRight $ fingerprintCertificateGraph renamed
      originalFingerprint @?= renamedFingerprint

      nominalLeft <- expectRight $ sealGraph
        nominalRepeatedFreeGraph [nominalRepeatedFreeOrigin]
      nominalRight <- expectRight $ sealGraph
        nominalDistinctFreeGraph [nominalDistinctFreeOrigin]
      leftFingerprint <- expectRight $
        fingerprintCertificateGraph nominalLeft
      rightFingerprint <- expectRight $
        fingerprintCertificateGraph nominalRight
      assertBool "free-variable equality pattern was discarded" $
        leftFingerprint /= rightFingerprint
  , testCase "accept exact constructor schemas and reseal before encoding rows" $ do
      checked <- expectRight $ sealGraphWith constructorTypeStructure
        constructorAssociationGraph [oneSlotOrigin]
      fingerprint <- expectRight $ fingerprintCertificateGraphWith
        constructorTypeStructure
        CertificateFingerprint.defaultTermGraphFingerprintByteLimit checked
      assertCanonicalTag
        (CanonicalFingerprint.fingerprintCanonicalBytes fingerprint)
        "constructor-pattern"
      case fingerprintCertificateGraphWith sharedTypeStructure
          0 checked of
        Left (CertificateFingerprint.TermGraphFingerprintSharedResealError
            (UnknownConstructorPatternSchema _ name _)) ->
          name @?= constructorName
        Left failure -> assertFailure $ "unexpected reseal failure: " ++
          show failure
        Right _ -> assertFailure "wrong constructor schema fingerprinted a row"
  , testCase "accept the exact nonempty byte limit and reject one byte short" $ do
      checked <- expectRight $ sealGraph contextualGraph [contextualOrigin]
      baseline <- expectRight $ fingerprintCertificateGraph checked
      let exact = canonicalFingerprintSize baseline
      fingerprintCertificateGraphWith sharedTypeStructure exact checked
        @?= Right baseline
      fingerprintCertificateGraphWith sharedTypeStructure (exact - 1) checked
        @?= Left
          (CertificateFingerprint.TermGraphFingerprintByteLimitExceeded
            (exact - 1) exact)
  ]

fingerprintCertificateGraph
  :: CheckedTypeApplicationCertificateGraph TestVariable TestLocal
  -> Either
      (CertificateFingerprint.TermGraphFingerprintError String TestLocal)
      (CanonicalFingerprint.Fingerprint
        CertificateFingerprint.TermGraphFingerprintSubject)
fingerprintCertificateGraph = fingerprintCertificateGraphWith
  sharedTypeStructure
  CertificateFingerprint.defaultTermGraphFingerprintByteLimit

fingerprintCertificateGraphWith
  :: TypeStructure TestType
  -> Natural
  -> CheckedTypeApplicationCertificateGraph TestVariable TestLocal
  -> Either
      (CertificateFingerprint.TermGraphFingerprintError String TestLocal)
      (CanonicalFingerprint.Fingerprint
        CertificateFingerprint.TermGraphFingerprintSubject)
fingerprintCertificateGraphWith structure maximumBytes =
  CertificateFingerprint.fingerprintCheckedTypeApplicationCertificateGraphWithTypeStructure
    structure defaultTermGraphLimits maximumBytes

canonicalFingerprintSize
  :: CanonicalFingerprint.Fingerprint subject -> Natural
canonicalFingerprintSize = fromIntegral . length .
  CanonicalFingerprint.fingerprintCanonicalBytes

asciiBytes :: String -> [Word8]
asciiBytes = map $ fromIntegral . fromEnum

encodedTagBytes :: String -> [Word8]
encodedTagBytes tag = 0x02 : fromIntegral (length tag) : asciiBytes tag

assertCanonicalTag :: [Word8] -> String -> IO ()
assertCanonicalTag bytes tag = assertBool
  ("missing canonical tag: " ++ tag)
  (encodedTagBytes tag `isContiguousSubsequenceOf` bytes)

assertCanonicalTagOrder :: [Word8] -> [String] -> IO ()
assertCanonicalTagOrder bytes tags = case traverse
    (firstContiguousSubsequenceOffset bytes . encodedTagBytes) tags of
  Nothing -> assertFailure $ "missing one of ordered canonical tags: " ++
    show tags
  Just offsets -> assertBool
    ("canonical tags out of order: " ++ show (tags, offsets))
    (strictlyIncreasing offsets)
 where
  strictlyIncreasing [] = True
  strictlyIncreasing [_] = True
  strictlyIncreasing (left : right : remaining) =
    left < right && strictlyIncreasing (right : remaining)

assertCanonicalReferenceOrder
  :: [Word8] -> [(Natural, Natural)] -> IO ()
assertCanonicalReferenceOrder bytes references = case traverse
    (firstContiguousSubsequenceOffset bytes .
      uncurry encodedCertificateReference) references of
  Nothing -> assertFailure $ "missing exact canonical reference: " ++
    show references
  Just offsets -> assertBool
    ("canonical references out of order: " ++ show (references, offsets))
    $ strictlyIncreasing offsets
 where
  strictlyIncreasing [] = True
  strictlyIncreasing [_] = True
  strictlyIncreasing (left : right : remaining) =
    left < right && strictlyIncreasing (right : remaining)

encodedCertificateReference :: Natural -> Natural -> [Word8]
encodedCertificateReference row step = encodedSmallField 0x04 $
  encodedSmallField 0x02 (asciiBytes "certificate-semantic-reference") ++
  encodedSmallField 0x03
    (encodedSmallNatural row ++ encodedSmallNatural step)

encodedSmallNatural :: Natural -> [Word8]
encodedSmallNatural value
  | value <= 255 = encodedSmallField 0x01 [fromIntegral value]
  | otherwise = error "canonical reference fixture exceeds one byte"

encodedSmallField :: Word8 -> [Word8] -> [Word8]
encodedSmallField fieldTag payload
  | length payload < 128 =
      fieldTag : fromIntegral (length payload) : payload
  | otherwise = error "canonical reference fixture exceeds one-byte length"

firstContiguousSubsequenceOffset
  :: Eq value => [value] -> [value] -> Maybe Int
firstContiguousSubsequenceOffset source expected = go 0 source
 where
  go offset remaining
    | expected `prefixOf` remaining = Just offset
    | otherwise = case remaining of
        [] -> Nothing
        _ : rest -> go (offset + 1) rest

  prefixOf [] _ = True
  prefixOf _ [] = False
  prefixOf (left : lefts) (right : rights) =
    left == right && prefixOf lefts rights

isContiguousSubsequenceOf :: Eq value => [value] -> [value] -> Bool
isContiguousSubsequenceOf expected source = case expected of
  [] -> True
  _ -> any (expected `prefixOf`) $ suffixes source
 where
  prefixOf [] _ = True
  prefixOf _ [] = False
  prefixOf (left : lefts) (right : rights) =
    left == right && prefixOf lefts rights

  suffixes [] = [[]]
  suffixes values@(_ : remaining) = values : suffixes remaining

occurrencesOf :: Eq value => [value] -> [value] -> Int
occurrencesOf needle = go 0
 where
  go count []
    | null needle = count + 1
    | otherwise = count
  go count values@(_ : remaining)
    | needle `isPrefix` values = go (count + 1) remaining
    | otherwise = go count remaining

  isPrefix [] _ = True
  isPrefix _ [] = False
  isPrefix (left : lefts) (right : rights) =
    left == right && isPrefix lefts rights

successTests :: TestTree
successTests = testGroup "successful atoms"
  [ testCase "co-own an exact two-slot table and complete rooted receipts" $ do
      checked <- expectRight $ sealGraph twoSlotGraph [twoSlotOrigin]
      length (termGraphNodes $
        checkedTypeApplicationCertificateGraph checked) @?= 3
      let rows = foldCheckedTypeApplicationCertificateGraph collect [] checked
      rows @?=
        [ (cert7, providerName, twoSlotScheme, 0, 10,
            [(0, 1, 12), (1, 2, 11)])
        ]
  , testCase "derive source slots despite outer-first occurrence allocation" $ do
      checked <- expectRight $
        sealGraph reorderedTwoSlotGraph [twoSlotOrigin]
      foldCheckedTypeApplicationCertificateGraph collect [] checked @?=
        [ (cert7, providerName, twoSlotScheme, 0, 10,
            [(0, 1, 12), (1, 2, 11)])
        ]
  , testCase "retain activated source contexts in the canonical plan" $ do
      checked <- expectRight $
        sealGraph contextualGraph [contextualOrigin]
      foldCheckedTypeApplicationCertificateGraph collectObligations [] checked
        @?= [(cert7, [[Constraint classC
          [fmap TypeApplicationCertificateFree intType]]])]
  , testCase "allow a certificate-free graph and empty origin table" $ do
      checked <- expectRight $ sealGraph plainGlobalGraph []
      ordinary <- expectRight $ sealTermGraph sharedTypeStructure
        defaultTermGraphLimits plainGlobalGraph
      let associated = checkedTypeApplicationCertificateGraph checked
      foldCheckedTypeApplicationCertificateGraph collect [] checked @?= []
      associated @?= ordinary
      eraseTermGraph associated @?= eraseTermGraph ordinary
      termGraphMetrics associated @?= termGraphMetrics ordinary
      ordinaryFingerprint <- expectRight $ Fingerprint.fingerprintSharedTermGraph
        defaultTermGraphLimits
        Fingerprint.defaultTermGraphFingerprintByteLimit ordinary
      associatedFingerprint <- expectRight $
        Fingerprint.fingerprintSharedTermGraph defaultTermGraphLimits
          Fingerprint.defaultTermGraphFingerprintByteLimit associated
      associatedFingerprint @?= ordinaryFingerprint
  , testCase "projected stamped graphs retain no fingerprint authority" $ do
      checked <- expectRight $ sealGraph oneSlotGraph [oneSlotOrigin]
      Fingerprint.fingerprintSharedTermGraph defaultTermGraphLimits
        Fingerprint.defaultTermGraphFingerprintByteLimit
        (checkedTypeApplicationCertificateGraph checked)
        @?= Left (Fingerprint.TermGraphFingerprintUnsupportedCertificate cert7)
  , testCase "ignore an untagged returned-polymorphic suffix" $ do
      checked <- expectRight $
        sealGraph returnedPolymorphicSuffixGraph [returnedPolymorphicOrigin]
      foldCheckedTypeApplicationCertificateGraph collect [] checked @?=
        [ (cert7, providerName, returnedPolymorphicScheme, 0, 10,
            [(0, 1, 11)])
        ]
  , testCase "force every retained graph, table, owner, and receipt" $ do
      checked <- expectRight $ sealGraph twoSlotGraph [twoSlotOrigin]
      _ <- evaluate $ force checked
      pure ()
  ]

observationTests :: TestTree
observationTests = testGroup "independent checker observations"
  [ testCase "reject a non-zero observation slot before its type payload" $ do
      let (first, second) = case
              typeApplicationCertificateOriginObservations twoSlotOrigin of
            [firstObservation, secondObservation] ->
              (firstObservation, secondObservation)
            _ -> error "invalid two-slot origin fixture"
          poisoned = first
            { typeApplicationCertificateObservationSlot = 9
            , typeApplicationCertificateObservationSource =
                error "slot-mismatch-source"
            }
          origin = twoSlotOrigin
            { typeApplicationCertificateOriginObservations =
                [poisoned, second]
            }
      assertPlanLeft
        (TypeApplicationCertificateObservationSlotMismatch cert7 0 9)
        $ sealGraph twoSlotGraph [origin]
  , testCase "reject a checker source which differs from structural replay" $ do
      let origin = mapFirstObservation
            (\step -> step
              { typeApplicationCertificateObservationSource = intType })
            twoSlotOrigin
      assertPlanLeft
        (TypeApplicationCertificateObservationSourceMismatch cert7 0)
        $ sealGraph twoSlotGraph [origin]
  , testCase "accept alpha-renamed impredicative observations" $ do
      table <- expectRight $ sealTypeApplicationCertificateTable
        defaultTypeApplicationCertificateLimits
        [TypeApplicationCertificateSource cert7 oneSlotScheme
          [selectedPolymorphicType]]
      let renamedSource = ForallType [FlexibleVariable "renamed-source"] []
            $ TypeVariable $ FlexibleVariable "renamed-source"
          renamedSelected = ForallType [FlexibleVariable "renamed-selected"] []
            $ TypeVariable $ FlexibleVariable "renamed-selected"
          renamedResult = ForallType [FlexibleVariable "renamed-result"] []
            $ TypeVariable $ FlexibleVariable "renamed-result"
      _ <- expectRight $ matchCheckedTypeApplicationCertificateObservations
        defaultTypeApplicationCertificateLimits cert7
        [TypeApplicationCertificateObservation 0 renamedSource
          renamedSelected renamedResult []]
        table
      pure ()
  , testCase "preserve nominal free variables across observation matching" $ do
      let free = RigidVariable "owner-free"
          otherFree = RigidVariable "different-free"
          binder = FlexibleVariable "a"
          scheme = ForallType [binder] []
            $ pairType (TypeVariable binder) (TypeVariable free)
          selected = intType
          wrongResult = pairType intType (TypeVariable otherFree)
      table <- expectRight $ sealTypeApplicationCertificateTable
        defaultTypeApplicationCertificateLimits
        [TypeApplicationCertificateSource cert7 scheme [selected]]
      case matchCheckedTypeApplicationCertificateObservations
          defaultTypeApplicationCertificateLimits cert7
          [TypeApplicationCertificateObservation 0 scheme selected
            wrongResult []]
          table of
        Left failure -> failure @?=
          TypeApplicationCertificateObservationResultMismatch cert7 0
        Right _ -> assertFailure "nominal free variables compared alpha-only"
  , testCase "reject a missing plan before observation payload demand" $ do
      let emptyResult :: Either
            (TypeApplicationCertificateError TestVariable)
            (CheckedTypeApplicationCertificateTable TestVariable)
          emptyResult = sealTypeApplicationCertificateTable
            defaultTypeApplicationCertificateLimits []
      empty <- expectRight emptyResult
      case matchCheckedTypeApplicationCertificateObservations
          defaultTypeApplicationCertificateLimits cert7
          [error "missing-plan-observation-payload"] empty of
        Left failure -> failure @?=
          TypeApplicationCertificateObservationMissingPlan cert7
        Right _ -> assertFailure "missing certificate returned a plan"
  , testCase "reject a checker result which differs from structural replay" $ do
      let origin = mapFirstObservation
            (\step -> step
              { typeApplicationCertificateObservationResult = boolType })
            twoSlotOrigin
      assertPlanLeft
        (TypeApplicationCertificateObservationResultMismatch cert7 0)
        $ sealGraph twoSlotGraph [origin]
  , testCase "reject a selected type which differs from the checked table" $ do
      table <- oneSlotTable
      case matchCheckedTypeApplicationCertificateObservations
          defaultTypeApplicationCertificateLimits cert7
          [TypeApplicationCertificateObservation 0 oneSlotScheme
            boolType intType []]
          table of
        Left failure -> failure @?=
          TypeApplicationCertificateObservationSelectedMismatch cert7 0
        Right _ -> assertFailure "changed selected type matched the plan"
  , testCase "reject changed activated obligation identity and arguments" $ do
      let wrongClass = contextualOrigin
            { typeApplicationCertificateOriginObservations =
                [contextualObservation
                  { typeApplicationCertificateObservationObligations =
                      [Constraint classD [intType]]
                  }]
            }
      assertPlanLeft
        (TypeApplicationCertificateObservedObligationClassMismatch
          cert7 0 0 classC classD)
        $ sealGraph contextualGraph [wrongClass]

      let wrongArgument = contextualOrigin
            { typeApplicationCertificateOriginObservations =
                [contextualObservation
                  { typeApplicationCertificateObservationObligations =
                      [Constraint classC [boolType]]
                  }]
            }
      assertPlanLeft
        (TypeApplicationCertificateObservedObligationArgumentMismatch
          cert7 0 0 0)
        $ sealGraph contextualGraph [wrongArgument]
  , testCase "reject observed obligation and argument count changes" $ do
      table <- contextualTable
      case matchCheckedTypeApplicationCertificateObservations
          defaultTypeApplicationCertificateLimits cert7
          [contextualObservation
            { typeApplicationCertificateObservationObligations = [] }]
          table of
        Left failure -> failure @?=
          TypeApplicationCertificateObservedObligationCountMismatch
            cert7 0 1 0
        Right _ -> assertFailure "missing observed obligation matched"
      case matchCheckedTypeApplicationCertificateObservations
          defaultTypeApplicationCertificateLimits cert7
          [contextualObservation
            { typeApplicationCertificateObservationObligations =
                [Constraint classC []]
            }]
          table of
        Left failure -> failure @?=
          TypeApplicationCertificateObservedObligationArgumentCountMismatch
            cert7 0 0 1 0
        Right _ -> assertFailure "missing observed argument matched"
  , testCase "bound observed obligation arguments before payload demand" $ do
      table <- contextualTable
      limits <- expectRight $
        mkTypeApplicationCertificateLimits 1 1 1 64 1
      let cyclic = error "observed-argument-payload" : cyclic
      case matchCheckedTypeApplicationCertificateObservations limits cert7
          [contextualObservation
            { typeApplicationCertificateObservationObligations =
                [Constraint classC cyclic]
            }]
          table of
        Left failure -> failure @?=
          TypeApplicationCertificateTypeCollectionLimitExceeded
            (TypeApplicationCertificateObservedObligationArguments cert7 0 0)
            1 2
        Right _ -> assertFailure "cyclic obligation arguments matched"
  , testCase "report the exact bounded observed type site" $ do
      table <- oneSlotTable
      limits <- expectRight $
        mkTypeApplicationCertificateLimits 1 1 1 2 16
      let cyclicType = TypeApplication cyclicType intType
      case matchCheckedTypeApplicationCertificateObservations limits cert7
          [TypeApplicationCertificateObservation 0 cyclicType
            intType intType []]
          table of
        Left failure -> failure @?=
          TypeApplicationCertificateTypeNodeLimitExceeded
            (TypeApplicationCertificateObservedSourceType cert7 0) 2 3
        Right _ -> assertFailure "cyclic observed source matched"
  , testCase "bound cyclic observed obligations before obligation payloads" $ do
      table <- contextualTable
      limits <- expectRight $
        mkTypeApplicationCertificateLimits 1 1 0 64 16
      let cyclic = error "observed-obligation-payload" : cyclic
      case matchCheckedTypeApplicationCertificateObservations limits cert7
          [contextualObservation
            { typeApplicationCertificateObservationObligations = cyclic }]
          table of
        Left failure -> failure @?=
          TypeApplicationCertificateObservedObligationLimitExceeded cert7 0 1
        Right _ -> assertFailure "cyclic observed obligations matched"
  ]

occurrenceTests :: TestTree
occurrenceTests = testGroup "derived graph coverage"
  [ testCase "reject an unknown certified occurrence in rooted order" $ do
      assertLeft
        (UnexpectedGraphTypeApplicationCertificateUse cert7 0 $ termNodeId 1)
        $ sealGraph oneSlotGraph []
  , testCase "reject an unused checked origin" $ do
      assertLeft
        (MissingGraphTypeApplicationCertificateUse cert7 0)
        $ sealGraph plainGlobalGraph [oneSlotOrigin]
  , testCase "reject an origin with an incomplete graph chain" $ do
      assertLeft
        (MissingGraphTypeApplicationCertificateUse cert7 1)
        $ sealGraph oneSlotFromTwoSlotGraph [twoSlotOrigin]
  , testCase "reject repeated uses of one certificate handle" $ do
      assertLeft
        (DuplicateGraphTypeApplicationCertificateUse
          cert7 0 (termNodeId 1) (termNodeId 3))
        $ sealGraph duplicateUseGraph [oneSlotOrigin]
  , testCase "derive and check the exact global owner" $ do
      assertLeft
        (TypeApplicationCertificateGlobalOwnerMismatch
          cert7 providerName otherProviderName)
        $ sealGraph wrongOwnerGraph [oneSlotOrigin]
  , testCase "derive and check the exact owner scheme" $ do
      assertLeft
        (TypeApplicationCertificateGlobalSchemeMismatch cert7)
        $ sealGraphWith permissiveTypeStructure wrongSchemeGraph [oneSlotOrigin]
  , testCase "check witness selection after the specified argument" $ do
      assertLeft
        (TypeApplicationCertificateWitnessSelectedMismatch cert7 0)
        $ sealGraphWith permissiveTypeStructure wrongSelectedWitnessGraph
            [oneSlotOrigin]
  , testCase "check the child and witness source independently" $ do
      assertLeft
        (TypeApplicationCertificateChildSourceMismatch cert7 1)
        $ sealGraphWith permissiveTypeStructure wrongChildSourceGraph
            [twoSlotOrigin]
      assertLeft
        (TypeApplicationCertificateWitnessSourceMismatch cert7 0)
        $ sealGraphWith permissiveTypeStructure wrongSourceWitnessGraph
            [oneSlotOrigin]
  , testCase "check the typed node result independently of its witness" $ do
      assertLeft
        (TypeApplicationCertificateNodeResultMismatch cert7 0)
        $ sealGraphWith permissiveTypeStructure wrongNodeResultGraph
            [oneSlotOrigin]
  , testCase "check the witness result independently of the node" $ do
      assertLeft
        (TypeApplicationCertificateWitnessResultMismatch cert7 0)
        $ sealGraphWith permissiveTypeStructure wrongResultWitnessGraph
            [oneSlotOrigin]
  , testCase "require a specified visible argument matching the plan" $ do
      assertLeft
        (TypeApplicationCertificateVisibleArgumentMismatch cert7 0)
        $ sealGraph inferredArgumentGraph [oneSlotOrigin]
  , testCase "reject a tagged returned-polymorphic suffix as out of range" $ do
      assertLeft
        (UnexpectedGraphTypeApplicationCertificateUse cert7 1 $ termNodeId 2)
        $ sealGraph taggedReturnedPolymorphicSuffixGraph
            [returnedPolymorphicOrigin]
  , testCase "reject a non-contiguous two-slot child chain" $ do
      assertLeft
        (TypeApplicationCertificateChildChainMismatch
          cert7 1 (termNodeId 1) (termNodeId 3))
        $ sealGraphWith permissiveTypeStructure brokenChainGraph [twoSlotOrigin]
  , testCase "delegate every uncertified witness to the base structure" $ do
      let witness = TypeApplicationWitness oneSlotScheme boolType intType Nothing
          source = TermGraphSource (termNodeId 1)
            [ globalNode 0 10 providerName oneSlotScheme
            , (termNodeId 1, TermNode intType $
                TypedVisibleTypeApplication (occurrenceId 11) (termNodeId 0)
                  (specified intType) witness)
            ]
      assertLeft
        (TypeApplicationCertificateAssociationGraphError $
          InvalidVisibleTypeApplicationWitness
            (termNodeId 1) (specified intType) witness)
        $ sealGraph source []
  , testCase "wrap graph sealing failures after plan matching" $ do
      let source = TermGraphSource (termNodeId 99)
            [globalNode 0 10 providerName intType]
      assertLeft
        (TypeApplicationCertificateAssociationGraphError $
          MissingTermGraphRoot $ termNodeId 99)
        $ sealGraph source []
  ]

demandTests :: TestTree
demandTests = testGroup "productive entrance"
  [ testCase "bound origin rows before graph or row payloads" $ do
      limits <- expectRight $
        mkTypeApplicationCertificateLimits 0 2 2 64 16
      let result :: Either
            (TypeApplicationCertificateAssociationError
              TestVariable TestLocal)
            (CheckedTypeApplicationCertificateGraph TestVariable TestLocal)
          result = sealCheckedTypeApplicationCertificateGraph limits
            sharedTypeStructure defaultTermGraphLimits
            (error "graph-source-before-origin-bound")
            [error "origin-row-payload"]
      case result of
        Left (TypeApplicationCertificateAssociationPlanError failure) ->
          failure @?= TypeApplicationCertificateEntryLimitExceeded 0 1
        Left failure -> assertFailure $ "unexpected failure: " ++ show failure
        Right _ -> assertFailure "overwide origins produced an atom"
  , testCase "reject duplicate origins before duplicate row payloads" $ do
      let duplicate = TypeApplicationCertificateOrigin cert7
            (error "duplicate-origin-owner")
            (error "duplicate-origin-scheme")
            (error "duplicate-origin-observations")
      assertPlanLeft (DuplicateTypeApplicationCertificateId cert7) $
        sealGraph oneSlotGraph [oneSlotOrigin, duplicate]
  , testCase "bound a cyclic observation spine before step payloads" $ do
      limits <- expectRight $
        mkTypeApplicationCertificateLimits 1 1 2 64 16
      let cyclic = error "observation-step-payload" : cyclic
          origin = TypeApplicationCertificateOrigin cert7 providerName
            oneSlotScheme cyclic
          result :: Either
            (TypeApplicationCertificateAssociationError
              TestVariable TestLocal)
            (CheckedTypeApplicationCertificateGraph TestVariable TestLocal)
          result = sealCheckedTypeApplicationCertificateGraph limits
            sharedTypeStructure defaultTermGraphLimits
            (error "graph-source-before-step-bound") [origin]
      case result of
        Left (TypeApplicationCertificateAssociationPlanError failure) ->
          failure @?= TypeApplicationCertificateSelectionLimitExceeded
            cert7 1 2
        Left failure -> assertFailure $ "unexpected failure: " ++ show failure
        Right _ -> assertFailure "cyclic observations produced an atom"
  ]

collect
  :: [(CertificateId, Name, TestType, Natural, Natural,
        [(Natural, Natural, Natural)])]
  -> CertificateId
  -> Name
  -> TestType
  -> TermNodeId
  -> OccurrenceId
  -> [( TermNodeId
      , OccurrenceId
      , CheckedTypeApplicationCertificateStep TestVariable
      )]
  -> [(CertificateId, Name, TestType, Natural, Natural,
        [(Natural, Natural, Natural)])]
collect rows certificate owner scheme baseNode baseOccurrence receipts =
  rows ++
    [ ( certificate
      , owner
      , scheme
      , termNodeIdValue baseNode
      , occurrenceIdValue baseOccurrence
      , [ ( checkedTypeApplicationCertificateStepSlot step
          , termNodeIdValue node
          , occurrenceIdValue occurrence
          )
        | (node, occurrence, step) <- receipts
        ]
      )
    ]

collectObligations
  :: [(CertificateId, [[Constraint
        (Type (TypeApplicationCertificatePlanVariable TestVariable))]])]
  -> CertificateId
  -> Name
  -> TestType
  -> TermNodeId
  -> OccurrenceId
  -> [( TermNodeId
      , OccurrenceId
      , CheckedTypeApplicationCertificateStep TestVariable
      )]
  -> [(CertificateId, [[Constraint
        (Type (TypeApplicationCertificatePlanVariable TestVariable))]])]
collectObligations rows certificate _ _ _ _ receipts =
  rows ++
    [(certificate,
      map (checkedTypeApplicationCertificateStepObligations . third) receipts)]
 where
  third (_, _, value) = value

sealGraph
  :: TermGraphSource TestType TestLocal
  -> [TypeApplicationCertificateOrigin TestVariable]
  -> Either
      (TypeApplicationCertificateAssociationError TestVariable TestLocal)
      (CheckedTypeApplicationCertificateGraph TestVariable TestLocal)
sealGraph = sealGraphWith sharedTypeStructure

sealGraphWith
  :: TypeStructure TestType
  -> TermGraphSource TestType TestLocal
  -> [TypeApplicationCertificateOrigin TestVariable]
  -> Either
      (TypeApplicationCertificateAssociationError TestVariable TestLocal)
      (CheckedTypeApplicationCertificateGraph TestVariable TestLocal)
sealGraphWith structure = sealCheckedTypeApplicationCertificateGraph
  defaultTypeApplicationCertificateLimits structure defaultTermGraphLimits

assertPlanLeft
  :: TypeApplicationCertificateError TestVariable
  -> Either
      (TypeApplicationCertificateAssociationError TestVariable TestLocal)
      value
  -> IO ()
assertPlanLeft expected source = case source of
  Left (TypeApplicationCertificateAssociationPlanError actual) ->
    actual @?= expected
  Left failure -> assertFailure $ "unexpected failure: " ++ show failure
  Right _ -> assertFailure "expected plan observation rejection"

assertLeft
  :: TypeApplicationCertificateAssociationError TestVariable TestLocal
  -> Either
      (TypeApplicationCertificateAssociationError TestVariable TestLocal)
      value
  -> IO ()
assertLeft expected source = case source of
  Left actual -> actual @?= expected
  Right _ -> assertFailure "expected association rejection"

expectRight :: Show failure => Either failure value -> IO value
expectRight source = case source of
  Left failure -> assertFailure $ "expected Right, got: " ++ show failure
  Right value -> pure value

oneSlotTable :: IO (CheckedTypeApplicationCertificateTable TestVariable)
oneSlotTable = expectRight $ sealTypeApplicationCertificateTable
  defaultTypeApplicationCertificateLimits
  [TypeApplicationCertificateSource cert7 oneSlotScheme [intType]]

contextualTable
  :: IO (CheckedTypeApplicationCertificateTable TestVariable)
contextualTable = expectRight $ sealTypeApplicationCertificateTable
  defaultTypeApplicationCertificateLimits
  [TypeApplicationCertificateSource cert7 contextualScheme [intType]]

mapFirstObservation
  :: ( TypeApplicationCertificateObservation TestVariable
       -> TypeApplicationCertificateObservation TestVariable )
  -> TypeApplicationCertificateOrigin TestVariable
  -> TypeApplicationCertificateOrigin TestVariable
mapFirstObservation change origin = origin
  { typeApplicationCertificateOriginObservations = case
      typeApplicationCertificateOriginObservations origin of
      [] -> []
      first : remaining -> change first : remaining
  }

twoSlotOrigin :: TypeApplicationCertificateOrigin TestVariable
twoSlotOrigin = TypeApplicationCertificateOrigin cert7 providerName
  twoSlotScheme
  [ TypeApplicationCertificateObservation 0
      twoSlotScheme intType firstTwoSlotResult []
  , TypeApplicationCertificateObservation 1
      firstTwoSlotResult boolType twoSlotResult []
  ]

oneSlotOrigin :: TypeApplicationCertificateOrigin TestVariable
oneSlotOrigin = TypeApplicationCertificateOrigin cert7 providerName
  oneSlotScheme
  [ TypeApplicationCertificateObservation 0
      oneSlotScheme intType intType []
  ]

contextualOrigin :: TypeApplicationCertificateOrigin TestVariable
contextualOrigin = TypeApplicationCertificateOrigin cert7 providerName
  contextualScheme [contextualObservation]

contextualObservation :: TypeApplicationCertificateObservation TestVariable
contextualObservation = TypeApplicationCertificateObservation 0
  contextualScheme intType intType [Constraint classC [intType]]

returnedPolymorphicOrigin
  :: TypeApplicationCertificateOrigin TestVariable
returnedPolymorphicOrigin = TypeApplicationCertificateOrigin cert7 providerName
  returnedPolymorphicScheme
  [ TypeApplicationCertificateObservation 0 returnedPolymorphicScheme
      selectedPolymorphicType selectedPolymorphicType []
  ]

renamedReturnedPolymorphicOrigin
  :: TypeApplicationCertificateOrigin TestVariable
renamedReturnedPolymorphicOrigin = TypeApplicationCertificateOrigin cert19
  providerName renamedReturnedPolymorphicScheme
  [ TypeApplicationCertificateObservation 0 renamedReturnedPolymorphicScheme
      renamedSelectedPolymorphicType renamedSelectedPolymorphicType []
  ]

twoRowLeftOrigin
  :: CertificateId -> TypeApplicationCertificateOrigin TestVariable
twoRowLeftOrigin certificate = TypeApplicationCertificateOrigin certificate
  providerName oneSlotScheme
  [ TypeApplicationCertificateObservation 0 oneSlotScheme intType intType [] ]

twoRowRightOrigin
  :: CertificateId -> TypeApplicationCertificateOrigin TestVariable
twoRowRightOrigin certificate = TypeApplicationCertificateOrigin certificate
  otherProviderName boolIdentityScheme
  [ TypeApplicationCertificateObservation 0 boolIdentityScheme boolType
      boolType []
  ]

nominalRepeatedFreeOrigin
  :: TypeApplicationCertificateOrigin TestVariable
nominalRepeatedFreeOrigin = nominalFreeOrigin repeatedNominalScheme

nominalDistinctFreeOrigin
  :: TypeApplicationCertificateOrigin TestVariable
nominalDistinctFreeOrigin = nominalFreeOrigin distinctNominalScheme

nominalFreeOrigin
  :: TestType -> TypeApplicationCertificateOrigin TestVariable
nominalFreeOrigin scheme = TypeApplicationCertificateOrigin cert7 providerName
  scheme
  [ TypeApplicationCertificateObservation 0 scheme intType
      (nominalResult scheme) []
  ]

twoSlotGraph :: TermGraphSource TestType TestLocal
twoSlotGraph = TermGraphSource (termNodeId 2)
  [ globalNode 0 10 providerName twoSlotScheme
  , visibleNode 1 12 0 intType twoSlotScheme intType firstTwoSlotResult
      $ Just (cert7, 0)
  , visibleNode 2 11 1 boolType firstTwoSlotResult boolType twoSlotResult
      $ Just (cert7, 1)
  ]

-- The retained node table is diagnostic storage only.  Association receipts
-- come from rooted child traversal and therefore remain stable when the table
-- is supplied in a different order.
reorderedTwoSlotGraph :: TermGraphSource TestType TestLocal
reorderedTwoSlotGraph = twoSlotGraph
  { termGraphSourceNodes = reverse $ termGraphSourceNodes twoSlotGraph }

oneSlotGraph :: TermGraphSource TestType TestLocal
oneSlotGraph = TermGraphSource (termNodeId 1)
  [ globalNode 0 10 providerName oneSlotScheme
  , visibleNode 1 11 0 intType oneSlotScheme intType intType
      $ Just (cert7, 0)
  ]

oneSlotFromTwoSlotGraph :: TermGraphSource TestType TestLocal
oneSlotFromTwoSlotGraph = TermGraphSource (termNodeId 1)
  [ globalNode 0 10 providerName twoSlotScheme
  , visibleNode 1 11 0 intType twoSlotScheme intType firstTwoSlotResult
      $ Just (cert7, 0)
  ]

contextualGraph :: TermGraphSource TestType TestLocal
contextualGraph = TermGraphSource (termNodeId 1)
  [ globalNode 0 10 providerName contextualScheme
  , visibleNode 1 11 0 intType contextualScheme intType intType
      $ Just (cert7, 0)
  ]

plainGlobalGraph :: TermGraphSource TestType TestLocal
plainGlobalGraph = TermGraphSource (termNodeId 0)
  [globalNode 0 10 providerName intType]

returnedPolymorphicSuffixGraph :: TermGraphSource TestType TestLocal
returnedPolymorphicSuffixGraph = TermGraphSource (termNodeId 2)
  [ globalNode 0 10 providerName returnedPolymorphicScheme
  , visibleNode 1 11 0 selectedPolymorphicType returnedPolymorphicScheme
      selectedPolymorphicType selectedPolymorphicType $ Just (cert7, 0)
  , visibleNode 2 12 1 intType selectedPolymorphicType intType intType Nothing
  ]

renamedReturnedPolymorphicSuffixGraph :: TermGraphSource TestType TestLocal
renamedReturnedPolymorphicSuffixGraph = TermGraphSource (termNodeId 29)
  [ globalNode 27 71 providerName renamedReturnedPolymorphicScheme
  , visibleNode 28 72 27 renamedSelectedPolymorphicType
      renamedReturnedPolymorphicScheme renamedSelectedPolymorphicType
      renamedSelectedPolymorphicType $ Just (cert19, 0)
  , visibleNode 29 73 28 intType renamedSelectedPolymorphicType
      intType intType Nothing
  ]

twoRowGraph :: TermGraphSource TestType TestLocal
twoRowGraph = TermGraphSource (termNodeId 4)
  [ globalNode 0 10 providerName oneSlotScheme
  , visibleNode 1 11 0 intType oneSlotScheme intType intType
      $ Just (cert7, 0)
  , globalNode 2 12 otherProviderName boolIdentityScheme
  , visibleNode 3 13 2 boolType boolIdentityScheme boolType boolType
      $ Just (cert8, 0)
  , (termNodeId 4, TermNode (TupleType Boxed [intType, boolType]) $
      TypedTuple [termNodeId 1, termNodeId 3])
  ]

renumberedTwoRowGraph :: TermGraphSource TestType TestLocal
renumberedTwoRowGraph = TermGraphSource (termNodeId 91)
  [ (termNodeId 91, TermNode (TupleType Boxed [intType, boolType]) $
      TypedTuple [termNodeId 87, termNodeId 12])
  , visibleNode 12 401 55 boolType boolIdentityScheme boolType boolType
      $ Just (cert3, 0)
  , globalNode 55 400 otherProviderName boolIdentityScheme
  , visibleNode 87 301 44 intType oneSlotScheme intType intType
      $ Just (cert19, 0)
  , globalNode 44 300 providerName oneSlotScheme
  ]

nominalRepeatedFreeGraph, nominalDistinctFreeGraph
  :: TermGraphSource TestType TestLocal
nominalRepeatedFreeGraph = nominalFreeGraph repeatedNominalScheme
nominalDistinctFreeGraph = nominalFreeGraph distinctNominalScheme

nominalFreeGraph :: TestType -> TermGraphSource TestType TestLocal
nominalFreeGraph scheme = TermGraphSource (termNodeId 1)
  [ globalNode 0 10 providerName scheme
  , visibleNode 1 11 0 intType scheme intType (nominalResult scheme)
      $ Just (cert7, 0)
  ]

invalidAnnotationGraph :: TermGraphSource TestType TestLocal
invalidAnnotationGraph = TermGraphSource (termNodeId 0)
  [globalNode 0 10 providerName malformedAnnotationType]

constructorAssociationGraph :: TermGraphSource TestType TestLocal
constructorAssociationGraph = TermGraphSource (termNodeId 3)
  [ globalNode 0 10 providerName oneSlotScheme
  , visibleNode 1 11 0 intType oneSlotScheme intType intType
      $ Just (cert7, 0)
  , (termNodeId 2, TermNode intType $ TypedGlobal (occurrenceId 12)
      otherProviderName)
  , (termNodeId 3, TermNode intType $ TypedCase (termNodeId 2)
      [ (TypedPattern (occurrenceId 13) intType $
            TypedConstructor constructorName [], termNodeId 1)
      ])
  ]

taggedReturnedPolymorphicSuffixGraph
  :: TermGraphSource TestType TestLocal
taggedReturnedPolymorphicSuffixGraph = TermGraphSource (termNodeId 2)
  [ globalNode 0 10 providerName returnedPolymorphicScheme
  , visibleNode 1 11 0 selectedPolymorphicType returnedPolymorphicScheme
      selectedPolymorphicType selectedPolymorphicType $ Just (cert7, 0)
  , visibleNode 2 12 1 intType selectedPolymorphicType intType intType
      $ Just (cert7, 1)
  ]

duplicateUseGraph :: TermGraphSource TestType TestLocal
duplicateUseGraph = TermGraphSource (termNodeId 4)
  [ globalNode 0 10 providerName oneSlotScheme
  , visibleNode 1 11 0 intType oneSlotScheme intType intType
      $ Just (cert7, 0)
  , globalNode 2 12 providerName oneSlotScheme
  , visibleNode 3 13 2 intType oneSlotScheme intType intType
      $ Just (cert7, 0)
  , (termNodeId 4, TermNode (TupleType Boxed [intType, intType])
      $ TypedTuple [termNodeId 1, termNodeId 3])
  ]

wrongOwnerGraph :: TermGraphSource TestType TestLocal
wrongOwnerGraph = TermGraphSource (termNodeId 1)
  [ globalNode 0 10 otherProviderName oneSlotScheme
  , visibleNode 1 11 0 intType oneSlotScheme intType intType
      $ Just (cert7, 0)
  ]

wrongSchemeGraph :: TermGraphSource TestType TestLocal
wrongSchemeGraph = TermGraphSource (termNodeId 1)
  [ globalNode 0 10 providerName intType
  , visibleNode 1 11 0 intType oneSlotScheme intType intType
      $ Just (cert7, 0)
  ]

wrongSelectedWitnessGraph :: TermGraphSource TestType TestLocal
wrongSelectedWitnessGraph = TermGraphSource (termNodeId 1)
  [ globalNode 0 10 providerName oneSlotScheme
  , visibleNode 1 11 0 intType oneSlotScheme boolType intType
      $ Just (cert7, 0)
  ]

wrongChildSourceGraph :: TermGraphSource TestType TestLocal
wrongChildSourceGraph = TermGraphSource (termNodeId 2)
  [ globalNode 0 10 providerName twoSlotScheme
  , (termNodeId 1, TermNode boolType $
      TypedVisibleTypeApplication (occurrenceId 11) (termNodeId 0)
        (specified intType)
        (TypeApplicationWitness twoSlotScheme intType firstTwoSlotResult
          $ Just (cert7, 0)))
  , visibleNode 2 12 1 boolType firstTwoSlotResult boolType twoSlotResult
      $ Just (cert7, 1)
  ]

wrongSourceWitnessGraph :: TermGraphSource TestType TestLocal
wrongSourceWitnessGraph = TermGraphSource (termNodeId 1)
  [ globalNode 0 10 providerName oneSlotScheme
  , visibleNode 1 11 0 intType boolType intType intType
      $ Just (cert7, 0)
  ]

wrongNodeResultGraph :: TermGraphSource TestType TestLocal
wrongNodeResultGraph = TermGraphSource (termNodeId 1)
  [ globalNode 0 10 providerName oneSlotScheme
  , (termNodeId 1, TermNode boolType $
      TypedVisibleTypeApplication (occurrenceId 11) (termNodeId 0)
        (specified intType)
        (TypeApplicationWitness oneSlotScheme intType intType
          $ Just (cert7, 0)))
  ]

wrongResultWitnessGraph :: TermGraphSource TestType TestLocal
wrongResultWitnessGraph = TermGraphSource (termNodeId 1)
  [ globalNode 0 10 providerName oneSlotScheme
  , (termNodeId 1, TermNode intType $
      TypedVisibleTypeApplication (occurrenceId 11) (termNodeId 0)
        (specified intType)
        (TypeApplicationWitness oneSlotScheme intType boolType
          $ Just (cert7, 0)))
  ]

inferredArgumentGraph :: TermGraphSource TestType TestLocal
inferredArgumentGraph = TermGraphSource (termNodeId 1)
  [ globalNode 0 10 providerName oneSlotScheme
  , (termNodeId 1, TermNode intType $
      TypedVisibleTypeApplication (occurrenceId 11) (termNodeId 0)
        Generated.inferredVisibleTypeArgument
        (TypeApplicationWitness oneSlotScheme intType intType
          $ Just (cert7, 0)))
  ]

brokenChainGraph :: TermGraphSource TestType TestLocal
brokenChainGraph = TermGraphSource (termNodeId 4)
  [ globalNode 0 10 providerName twoSlotScheme
  , visibleNode 1 11 0 intType twoSlotScheme intType firstTwoSlotResult
      $ Just (cert7, 0)
  , globalNode 3 13 providerName firstTwoSlotResult
  , visibleNode 2 12 3 boolType firstTwoSlotResult boolType twoSlotResult
      $ Just (cert7, 1)
  , (termNodeId 4,
      TermNode (TupleType Boxed [twoSlotResult, firstTwoSlotResult])
        $ TypedTuple [termNodeId 2, termNodeId 1])
  ]

globalNode
  :: Natural
  -> Natural
  -> Name
  -> TestType
  -> (TermNodeId,
      TermNode TestType TestLocal)
globalNode node occurrence owner ty =
  (termNodeId node, TermNode ty $
    TypedGlobal (occurrenceId occurrence) owner)

visibleNode
  :: Natural
  -> Natural
  -> Natural
  -> TestType
  -> TestType
  -> TestType
  -> TestType
  -> Maybe (CertificateId, Natural)
  -> (TermNodeId,
      TermNode TestType TestLocal)
visibleNode node occurrence function argument source selected result certificate =
  (termNodeId node, TermNode result $
    TypedVisibleTypeApplication (occurrenceId occurrence) (termNodeId function)
      (specified argument)
      (TypeApplicationWitness source selected result certificate))

specified :: TestType -> Generated.VisibleTypeArgument
specified ty = case Generated.specifiedVisibleTypeArgument ty of
  Left failure -> error $ "invalid specified fixture: " ++ show failure
  Right argument -> argument

permissiveTypeStructure :: TypeStructure TestType
permissiveTypeStructure = sharedTypeStructure
  { equivalentTypes = \_ _ -> True }

annotationPermissiveTypeStructure :: TypeStructure TestType
annotationPermissiveTypeStructure = sharedTypeStructure
  { validTypeAnnotation = const True }

constructorTypeStructure :: TypeStructure TestType
constructorTypeStructure = sharedTypeStructure
  { constructorPatternFieldTypes = \name ty ->
      if name == constructorName && ty == intType then Just [] else Nothing
  }

twoSlotScheme, firstTwoSlotResult, twoSlotResult :: TestType
twoSlotScheme = ForallType [FlexibleVariable "a", FlexibleVariable "b"] []
  $ pairType
      (TypeVariable $ FlexibleVariable "a")
      (TypeVariable $ FlexibleVariable "b")
firstTwoSlotResult = ForallType [FlexibleVariable "b"] []
  $ pairType intType (TypeVariable $ FlexibleVariable "b")
twoSlotResult = pairType intType boolType

oneSlotScheme :: TestType
oneSlotScheme = ForallType [FlexibleVariable "a"] []
  $ TypeVariable $ FlexibleVariable "a"

contextualScheme :: TestType
contextualScheme = ForallType [FlexibleVariable "a"]
  [Constraint classC [TypeVariable $ FlexibleVariable "a"]]
  $ TypeVariable $ FlexibleVariable "a"

returnedPolymorphicScheme, selectedPolymorphicType :: TestType
returnedPolymorphicScheme = ForallType [FlexibleVariable "a"] []
  $ TypeVariable $ FlexibleVariable "a"
selectedPolymorphicType = ForallType [FlexibleVariable "selected"] []
  $ TypeVariable $ FlexibleVariable "selected"

renamedReturnedPolymorphicScheme, renamedSelectedPolymorphicType :: TestType
renamedReturnedPolymorphicScheme = ForallType [FlexibleVariable "outer"] []
  $ TypeVariable $ FlexibleVariable "outer"
renamedSelectedPolymorphicType = ForallType [FlexibleVariable "inner"] []
  $ TypeVariable $ FlexibleVariable "inner"

boolIdentityScheme :: TestType
boolIdentityScheme = ForallType [FlexibleVariable "b"] []
  $ TypeVariable $ FlexibleVariable "b"

repeatedNominalScheme, distinctNominalScheme :: TestType
repeatedNominalScheme = ForallType [FlexibleVariable "a"] [] $
  pairType (TypeVariable $ FlexibleVariable "a")
    (pairType
      (TypeVariable $ RigidVariable "shared-free")
      (TypeVariable $ RigidVariable "shared-free"))
distinctNominalScheme = ForallType [FlexibleVariable "a"] [] $
  pairType (TypeVariable $ FlexibleVariable "a")
    (pairType
      (TypeVariable $ RigidVariable "first-free")
      (TypeVariable $ RigidVariable "second-free"))

nominalResult :: TestType -> TestType
nominalResult scheme = case scheme of
  ForallType [_] [] body -> substituteLeadingBinder body
  _ -> error "invalid nominal scheme fixture"
 where
  substituteLeadingBinder source = case source of
    TypeVariable (FlexibleVariable "a") -> intType
    TypeVariable variable -> TypeVariable variable
    TypeConstructor name -> TypeConstructor name
    TypeApplication function argument -> TypeApplication
      (substituteLeadingBinder function) (substituteLeadingBinder argument)
    FunctionType domain result -> FunctionType
      (substituteLeadingBinder domain) (substituteLeadingBinder result)
    TupleType boxity fields -> TupleType boxity $
      map substituteLeadingBinder fields
    ForallType binders contexts body -> ForallType binders
      (map (fmap substituteLeadingBinder) contexts)
      (substituteLeadingBinder body)

malformedAnnotationType :: TestType
malformedAnnotationType = ForallType [duplicate, duplicate] []
  $ TypeVariable duplicate
 where
  duplicate = FlexibleVariable "duplicate"

pairType :: TestType -> TestType -> TestType
pairType left right = TypeApplication
  (TypeApplication (TypeConstructor pairName) left) right

intType, boolType :: TestType
intType = TypeConstructor intName
boolType = TypeConstructor boolName

cert7 :: CertificateId
cert7 = certificateId 7

cert3, cert8, cert19 :: CertificateId
cert3 = certificateId 3
cert8 = certificateId 8
cert19 = certificateId 19

providerName, otherProviderName, pairName, intName, boolName, classC, classD,
  constructorName
  :: Name
providerName = parsedName "Fixture.provider"
otherProviderName = parsedName "Fixture.otherProvider"
pairName = parsedName "Fixture.Pair"
intName = parsedName "Int"
boolName = parsedName "Bool"
classC = parsedName "Fixture.C"
classD = parsedName "Fixture.D"
constructorName = parsedName "Fixture.Zero"

parsedName :: String -> Name
parsedName source = case parseName source of
  Left failure -> error $ "invalid fixture name: " ++ show failure
  Right name -> name
