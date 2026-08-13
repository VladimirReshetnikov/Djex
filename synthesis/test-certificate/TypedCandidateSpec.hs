{-# LANGUAGE ScopedTypeVariables #-}

module TypedCandidateSpec (tests) where

import Control.DeepSeq (NFData, force, rnf)
import Control.Exception
  ( SomeException
  , displayException
  , evaluate
  , try
  )
import Data.List (isInfixOf)

import qualified AssociationSpec
import Language.Haskell.Synthesis.Internal.TypedCandidate
  ( TypedCandidate
  , foldTypedCandidateGraph
  , mkCertificateAssociatedTypedCandidate
  , mkCertificateCapableTypedCandidate
  , mkTypedCandidate
  , typedCandidateCompatibility
  , typedCandidateTermGraph
  )
import Language.Haskell.Synthesis.Internal.TypedGenerated.Certificate
  ( defaultTypeApplicationCertificateLimits )
import Language.Haskell.Synthesis.Internal.TypedGenerated.Certificate.Association
  ( CheckedTypeApplicationCertificateGraph
  , checkedTypeApplicationCertificateGraph
  , sealCheckedTypeApplicationCertificateGraph
  )
import Language.Haskell.Synthesis.Name (Name, parseName)
import Language.Haskell.Synthesis.Type (Type (..), Variable)
import Language.Haskell.Synthesis.TypedGenerated
  ( TermGraph
  , TermGraphSource (..)
  , TermNode (..)
  , TermNodeForm (..)
  , defaultTermGraphLimits
  , occurrenceId
  , sharedTypeStructure
  , termGraphRoot
  , termNodeId
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertFailure
  , testCase
  , (@?=)
  )

type TestVariable = Variable String
type TestType = Type TestVariable
type TestLocal = Int
type TestCandidate = TypedCandidate String TestType TestLocal Int

tests :: TestTree
tests = testGroup "typed candidate certificate carrier"
  [ constructionTests
  , observationTests
  , demandTests
  , exactTypeTests
  ]

constructionTests :: TestTree
constructionTests = testGroup "hidden three-way retention"
  [ testCase "fold all branches with their exact compatibility candidate" $ do
      checked <- AssociationSpec.typedCandidateCertificateGraphFixture
      let graph = checkedTypeApplicationCertificateGraph checked
          unavailable = mkCertificateCapableTypedCandidate 3 $ Left "absent"
          plain = mkCertificateCapableTypedCandidate 5 $
            Right $ Left graph
          associated = mkCertificateCapableTypedCandidate 7 $
            Right $ Right checked
      branch unavailable @?= ("unavailable", 3)
      branch plain @?= ("plain", 5)
      branch associated @?= ("associated", 7)
  , testCase "associated-only entrance delegates both lazy Either branches" $ do
      checked <- AssociationSpec.typedCandidateCertificateGraphFixture
      branch (mkCertificateAssociatedTypedCandidate 11 $ Left "absent")
        @?= ("unavailable", 11)
      branch (mkCertificateAssociatedTypedCandidate 13 $ Right checked)
        @?= ("associated", 13)
  , testCase "legacy constructor retains unavailable and plain behavior" $ do
      checked <- AssociationSpec.typedCandidateCertificateGraphFixture
      let graph = checkedTypeApplicationCertificateGraph checked
      typedCandidateTermGraph
          (mkTypedCandidate 1 (Left "legacy") :: TestCandidate)
        @?= Left "legacy"
      typedCandidateTermGraph
          (mkTypedCandidate 1 $ Right graph :: TestCandidate)
        @?= Right graph
  ]

observationTests :: TestTree
observationTests = testGroup "legacy observations"
  [ testCase "plain and associated forms are equal and order-equivalent" $ do
      checked <- AssociationSpec.typedCandidateCertificateGraphFixture
      let graph = checkedTypeApplicationCertificateGraph checked
          plain = mkTypedCandidate 17 (Right graph) :: TestCandidate
          capablePlain = mkCertificateCapableTypedCandidate 17 $
            Right $ Left graph
          associated = mkCertificateCapableTypedCandidate 17 $
            Right $ Right checked
      plain == capablePlain @?= True
      plain == associated @?= True
      compare plain capablePlain @?= EQ
      compare plain associated @?= EQ
  , testCase "plain and associated forms render exactly like the old pair" $ do
      checked <- AssociationSpec.typedCandidateCertificateGraphFixture
      let graph = checkedTypeApplicationCertificateGraph checked
          expected = legacyShowsPrec 0 (19 :: Int)
            (Right graph :: Either String (TermGraph TestType TestLocal)) ""
          expectedNested = legacyShowsPrec 11 (19 :: Int)
            (Right graph :: Either String (TermGraph TestType TestLocal)) ""
          plain = mkTypedCandidate 19 (Right graph) :: TestCandidate
          associated = mkCertificateAssociatedTypedCandidate 19
            (Right checked) :: TestCandidate
      show plain @?= expected
      show associated @?= expected
      showsPrec 11 plain "" @?= expectedNested
      showsPrec 11 associated "" @?= expectedNested
  , testCase "unavailable rendering retains the exact old constructor shape" $
      let graph = Left "missing" :: Either String
            (TermGraph TestType TestLocal)
          candidate = mkTypedCandidate 23 graph :: TestCandidate
      in show candidate @?= legacyShowsPrec 0 (23 :: Int) graph ""
  , testCase "compatibility remains the first equality and ordering demand" $ do
      let left = poisonedCarrierCandidate 1 "equality-carrier"
          right = poisonedCarrierCandidate 2 "equality-carrier"
      left == right @?= False
      compare left right @?= LT
  , testCase "show exposes its compatibility prefix before graph demand" $
      take (length prefix) (show $ poisonedCarrierCandidate 29 "show-carrier")
        @?= prefix
  ]
 where
  prefix = "TypedCandidate 29 "

demandTests :: TestTree
demandTests = testGroup "non-strict access and deep forcing"
  [ testCase "compatibility projection ignores the whole three-way source" $
      typedCandidateCompatibility
        (mkCertificateCapableTypedCandidate 31
          (error "three-way-availability") :: TestCandidate)
        @?= (31 :: Int)
  , testCase "compatibility projection ignores associated-only availability" $
      typedCandidateCompatibility
        (mkCertificateAssociatedTypedCandidate 37
          (error "associated-availability") :: TestCandidate)
        @?= (37 :: Int)
  , testCase "graph projection ignores compatibility" $
      case typedCandidateTermGraph candidate of
        Left failure -> failure @?= "absent"
        Right _ -> assertFailure "unavailable candidate projected a graph"
  , testCase "fold selects no unchosen callback and keeps payloads lazy" $
      foldTypedCandidateGraph
        (\_ _ -> "unavailable")
        (\_ _ -> error "plain-callback")
        (\_ _ -> error "associated-callback")
        candidate
        @?= "unavailable"
  , testCase "NFData forces compatibility before graph availability" $
      assertThrowsContaining "compatibility-first" $ evaluate $ force
        (mkCertificateCapableTypedCandidate
          (error "compatibility-first")
          (error "carrier-second") :: TestCandidate)
  , testCase "NFData forces a complete associated carrier" $ do
      checked <- AssociationSpec.typedCandidateCertificateGraphFixture
      let retained = mkCertificateAssociatedTypedCandidate 41
            (Right checked) :: TestCandidate
      _ <- evaluate $ force retained
      pure ()
  , testCase "graph projection is shallow but NFData reaches carrier leaves" $ do
      checked <- deepPoisonCertificateGraph
      let associated = mkCertificateAssociatedTypedCandidate 43
            (Right checked) :: TypedCandidate String TestType [Int] Int
      case typedCandidateTermGraph associated of
        Left failure -> assertFailure $ "unexpected absence: " ++ failure
        Right graph -> termGraphRoot graph @?= termNodeId 0
      assertThrowsContaining "deep-carrier-local" $
        evaluate $ force associated
  ]
 where
  candidate = mkCertificateCapableTypedCandidate
    (error "compatibility") (Left "absent") :: TestCandidate

exactTypeTests :: TestTree
exactTypeTests = testGroup "exact private and public-compatible types"
  [ testCase "constructors and atomic fold retain their intended types" $ do
      exactLegacyConstructor `seq`
        exactCapableConstructor `seq`
        exactAssociatedConstructor `seq`
        exactFold `seq` pure ()
  , testCase "observational instances need only the historical constraints" $
      (exactEquality :: TestCandidate -> TestCandidate -> Bool) `seq`
        (exactOrdering :: TestCandidate -> TestCandidate -> Ordering) `seq`
        (exactShowing :: TestCandidate -> String) `seq`
        (exactDeepEvaluation :: TestCandidate -> ()) `seq`
        pure ()
  ]

branch :: TestCandidate -> (String, Int)
branch = foldTypedCandidateGraph
  (\compatibility _ -> ("unavailable", compatibility))
  (\compatibility _ -> ("plain", compatibility))
  (\compatibility _ -> ("associated", compatibility))

poisonedCarrierCandidate :: Int -> String -> TestCandidate
poisonedCarrierCandidate compatibility label =
  mkCertificateCapableTypedCandidate compatibility $ error label

legacyShowsPrec
  :: (Show candidate, Show failure, Show ty, Show local)
  => Int
  -> candidate
  -> Either failure (TermGraph ty local)
  -> ShowS
legacyShowsPrec precedence compatibility graph =
  showParen (precedence > 10) $
    showString "TypedCandidate " .
    showsPrec 11 compatibility .
    showChar ' ' .
    showsPrec 11 graph

deepPoisonCertificateGraph
  :: IO (CheckedTypeApplicationCertificateGraph TestVariable [Int])
deepPoisonCertificateGraph = expectRight $
  sealCheckedTypeApplicationCertificateGraph
    defaultTypeApplicationCertificateLimits
    sharedTypeStructure
    defaultTermGraphLimits
    (TermGraphSource (termNodeId 0)
      [ ( termNodeId 0
        , TermNode intType $ TypedHole (occurrenceId 0)
            [1, error "deep-carrier-local"]
        )
      ])
    []

exactLegacyConstructor
  :: Int
  -> Either String (TermGraph TestType TestLocal)
  -> TestCandidate
exactLegacyConstructor = mkTypedCandidate

exactCapableConstructor
  :: Int
  -> Either String
      (Either
        (TermGraph TestType TestLocal)
        (CheckedTypeApplicationCertificateGraph TestVariable TestLocal))
  -> TestCandidate
exactCapableConstructor = mkCertificateCapableTypedCandidate

exactAssociatedConstructor
  :: Int
  -> Either String
      (CheckedTypeApplicationCertificateGraph TestVariable TestLocal)
  -> TestCandidate
exactAssociatedConstructor = mkCertificateAssociatedTypedCandidate

exactFold
  :: (Int -> String -> result)
  -> (Int -> TermGraph TestType TestLocal -> result)
  -> ( Int
       -> CheckedTypeApplicationCertificateGraph TestVariable TestLocal
       -> result
     )
  -> TestCandidate
  -> result
exactFold = foldTypedCandidateGraph

exactEquality
  :: (Eq failure, Eq ty, Eq local, Eq candidate)
  => TypedCandidate failure ty local candidate
  -> TypedCandidate failure ty local candidate
  -> Bool
exactEquality = (==)

exactOrdering
  :: (Ord failure, Ord ty, Ord local, Ord candidate)
  => TypedCandidate failure ty local candidate
  -> TypedCandidate failure ty local candidate
  -> Ordering
exactOrdering = compare

exactShowing
  :: (Show failure, Show ty, Show local, Show candidate)
  => TypedCandidate failure ty local candidate
  -> String
exactShowing = show

exactDeepEvaluation
  :: (NFData failure, NFData ty, NFData local, NFData candidate)
  => TypedCandidate failure ty local candidate
  -> ()
exactDeepEvaluation = rnf

assertThrowsContaining :: String -> IO value -> IO ()
assertThrowsContaining expected action = do
  attempted <- try action
  case attempted of
    Left failure
      | expected `isInfixOf` displayException (failure :: SomeException) ->
          pure ()
      | otherwise -> assertFailure $ "unexpected exception: " ++
          displayException failure
    Right _ -> assertFailure $ "expected exception containing " ++ show expected

expectRight :: Show failure => Either failure value -> IO value
expectRight source = case source of
  Left failure -> assertFailure $ "expected Right, got: " ++ show failure
  Right value -> pure value

intType :: TestType
intType = TypeConstructor intName

intName :: Name
intName = case parseName "Int" of
  Left failure -> error $ "invalid fixture name: " ++ show failure
  Right name -> name
