module Main (main) where

import Numeric.Natural (Natural)

import qualified Language.Haskell.Synthesis.Fingerprint as Fingerprint
import qualified Language.Haskell.Synthesis.Generated as Generated
import Language.Haskell.Synthesis.Name (Boxity (Boxed), Name, parseName)
import qualified Language.Haskell.Synthesis.Type as Type
import qualified Language.Haskell.Synthesis.TypedGenerated as Typed
import qualified Language.Haskell.Synthesis.TypedGenerated.Fingerprint as GraphFingerprint
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
  ( assertBool
  , assertFailure
  , testCase
  , (@?=)
  )

type Identity = String
type Local = Int
type FixtureType = Type.Type (Type.Variable Identity)
type FixtureSource = Typed.TermGraphSource FixtureType Local
type FixtureGraph = Typed.TermGraph FixtureType Local

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "shared term-graph fingerprints"
  [ testCase "ignore allocation, table order, locals, and binder spelling" $ do
      left <- fingerprintSource $ renamingFixture
        fixtureGlobal (10, 20, 30, 40) (1, 2, 3)
        7 "a" "meta-17" False
      right <- fingerprintSource $ renamingFixture
        fixtureGlobal (901, 502, 703, 104) (91, 82, 73)
        999 "renamed" "meta-9000" True
      left @?= right
  , testCase "retain exact global names" $ do
      left <- fingerprintSource $ renamingFixture
        fixtureGlobal (10, 20, 30, 40) (1, 2, 3)
        7 "a" "meta" False
      right <- fingerprintSource $ renamingFixture
        changedGlobal (10, 20, 30, 40) (1, 2, 3)
        7 "a" "meta" False
      assertBool "distinct global names shared a fingerprint" $ left /= right
  , testCase "distinguish flexible variables from rigid variables" $ do
      flexible <- fingerprintSource $ variableFixture
        $ Type.FlexibleVariable "slot"
      rigid <- fingerprintSource $ variableFixture
        $ Type.RigidVariable "slot"
      assertBool "variable flavor was erased" $ flexible /= rigid
  , testCase "distinguish inferred and specified impredicative arguments" $ do
      specified <- expectRight
        $ Generated.specifiedVisibleTypeArgument impredicativeType
      inferredFingerprint <- fingerprintSource
        $ visibleApplicationFixture Generated.inferredVisibleTypeArgument Nothing
      specifiedFingerprint <- fingerprintSource
        $ visibleApplicationFixture specified Nothing
      assertBool "visible-argument syntax was erased" $
        inferredFingerprint /= specifiedFingerprint
  , testCase "reject certificate-bearing graphs until certificates are sealed" $ do
      graph <- expectRight $ sealShared $ visibleApplicationFixture
        Generated.inferredVisibleTypeArgument
        (Just (Typed.certificateId 7, 0))
      fingerprintGraph GraphFingerprint.defaultTermGraphFingerprintByteLimit graph
        @?= Left (GraphFingerprint.TermGraphFingerprintUnsupportedCertificate
          $ Typed.certificateId 7)
  , testCase "reject a graph that fails the fresh shared reseal" $ do
      graph <- expectRight $ Typed.sealTermGraph permissiveTypeStructure
        Typed.defaultTermGraphLimits malformedSource
      fingerprintGraph GraphFingerprint.defaultTermGraphFingerprintByteLimit graph
        @?= Left (GraphFingerprint.TermGraphFingerprintSharedResealError
          $ Typed.InvalidTermGraphTypeAnnotation
              (Typed.GraphTermNodeType $ nodeId 77)
              malformedType)
  , testCase "require inventory authority for constructor patterns" $ do
      graph <- expectRight $ Typed.sealTermGraph constructorAwareTypeStructure
        Typed.defaultTermGraphLimits constructorPatternSource
      fingerprintGraph GraphFingerprint.defaultTermGraphFingerprintByteLimit graph
        @?= Left (GraphFingerprint.TermGraphFingerprintSharedResealError
          $ Typed.UnknownConstructorPatternSchema
              (occurrenceId 203) constructorName simpleType)
  , testCase "accept the exact byte limit and reject one byte short" $ do
      graph <- expectRight $ sealShared $ renamingFixture
        fixtureGlobal (10, 20, 30, 40) (1, 2, 3)
        7 "a" "meta" False
      baseline <- expectRight $ fingerprintGraph
        GraphFingerprint.defaultTermGraphFingerprintByteLimit graph
      let exact = fromIntegral
            (length $ Fingerprint.fingerprintCanonicalBytes baseline) :: Natural
      fingerprintGraph exact graph @?= Right baseline
      fingerprintGraph (exact - 1) graph @?=
        Left (GraphFingerprint.TermGraphFingerprintByteLimitExceeded
          (exact - 1) exact)
  ]

fingerprintSource
  :: FixtureSource
  -> IO (Fingerprint.Fingerprint GraphFingerprint.TermGraphFingerprintSubject)
fingerprintSource source = do
  graph <- expectRight $ sealShared source
  expectRight $ fingerprintGraph
    GraphFingerprint.defaultTermGraphFingerprintByteLimit graph

fingerprintGraph
  :: Natural
  -> FixtureGraph
  -> Either
      (GraphFingerprint.TermGraphFingerprintError Identity Local)
      (Fingerprint.Fingerprint GraphFingerprint.TermGraphFingerprintSubject)
fingerprintGraph = GraphFingerprint.fingerprintSharedTermGraph
  Typed.defaultTermGraphLimits

sealShared
  :: FixtureSource
  -> Either (Typed.TermGraphError FixtureType Local) FixtureGraph
sealShared = Typed.sealTermGraph
  (Typed.sharedTypeStructure :: Typed.TypeStructure FixtureType)
  Typed.defaultTermGraphLimits

renamingFixture
  :: Name
  -> (Natural, Natural, Natural, Natural)
  -> (Natural, Natural, Natural)
  -> Local
  -> Identity
  -> Identity
  -> Bool
  -> FixtureSource
renamingFixture globalName
    (bodyNumber, lambdaNumber, globalNumber, rootNumber)
    (localOccurrence, patternOccurrence, globalOccurrence)
    local binderName flexibleName reverseTable =
  Typed.TermGraphSource (nodeId rootNumber) arrangedNodes
 where
  polymorphic = polymorphicIdentity binderName
  flexible = Type.TypeVariable $ Type.FlexibleVariable flexibleName
  lambdaType = Type.FunctionType polymorphic polymorphic
  tupleType = Type.TupleType Boxed [lambdaType, flexible]
  nodes =
    [ (nodeId rootNumber, Typed.TermNode tupleType
        $ Typed.TypedTuple [nodeId lambdaNumber, nodeId globalNumber])
    , (nodeId bodyNumber, Typed.TermNode polymorphic
        $ Typed.TypedLocal (occurrenceId localOccurrence) local)
    , (nodeId globalNumber, Typed.TermNode flexible
        $ Typed.TypedGlobal (occurrenceId globalOccurrence) globalName)
    , (nodeId lambdaNumber, Typed.TermNode lambdaType
        $ Typed.TypedLambda
            [ Typed.TypedPattern
                (occurrenceId patternOccurrence)
                polymorphic
                (Typed.TypedBind local)
            ]
            (nodeId bodyNumber))
    ]
  arrangedNodes = if reverseTable then reverse nodes else nodes

variableFixture :: Type.Variable Identity -> FixtureSource
variableFixture variable = Typed.TermGraphSource (nodeId 0)
  [ (nodeId 0, Typed.TermNode (Type.TypeVariable variable)
      $ Typed.TypedGlobal (occurrenceId 0) fixtureGlobal)
  ]

visibleApplicationFixture
  :: Generated.VisibleTypeArgument
  -> Maybe (Typed.CertificateId, Natural)
  -> FixtureSource
visibleApplicationFixture argument certificate =
  Typed.TermGraphSource (nodeId 1)
    [ (nodeId 1, Typed.TermNode impredicativeType
        $ Typed.TypedVisibleTypeApplication
            (occurrenceId 1)
            (nodeId 0)
            argument
            witness)
    , (nodeId 0, Typed.TermNode sourceType
        $ Typed.TypedGlobal (occurrenceId 0) providerGlobal)
    ]
 where
  sourceType = polymorphicIdentity "source"
  witness = Typed.TypeApplicationWitness
    sourceType impredicativeType impredicativeType certificate

polymorphicIdentity :: Identity -> FixtureType
polymorphicIdentity identity = Type.ForallType [variable] []
  $ Type.TypeVariable variable
 where
  variable = Type.FlexibleVariable identity

impredicativeType :: FixtureType
impredicativeType = polymorphicIdentity "selected"

malformedSource :: FixtureSource
malformedSource = Typed.TermGraphSource (nodeId 77)
  [ (nodeId 77, Typed.TermNode malformedType
      $ Typed.TypedGlobal (occurrenceId 77) fixtureGlobal)
  ]

malformedType :: FixtureType
malformedType = Type.ForallType [duplicate, duplicate] []
  $ Type.TypeVariable duplicate
 where
  duplicate = Type.FlexibleVariable "duplicate"

constructorPatternSource :: FixtureSource
constructorPatternSource = Typed.TermGraphSource (nodeId 200)
  [ (nodeId 200, Typed.TermNode simpleType
      $ Typed.TypedCase (nodeId 201)
          [ ( Typed.TypedPattern (occurrenceId 203) simpleType
                $ Typed.TypedConstructor constructorName []
            , nodeId 202
            )
          ])
  , (nodeId 201, Typed.TermNode simpleType
      $ Typed.TypedGlobal (occurrenceId 201) fixtureGlobal)
  , (nodeId 202, Typed.TermNode simpleType
      $ Typed.TypedGlobal (occurrenceId 202) changedGlobal)
  ]

simpleType :: FixtureType
simpleType = Type.TypeVariable $ Type.FlexibleVariable "simple"

constructorName :: Name
constructorName = checkedName "Fixture.Constructor"

constructorAwareTypeStructure :: Typed.TypeStructure FixtureType
constructorAwareTypeStructure =
  (Typed.sharedTypeStructure :: Typed.TypeStructure FixtureType)
    { Typed.constructorPatternFieldTypes = \_ _ -> Just [] }

permissiveTypeStructure :: Typed.TypeStructure FixtureType
permissiveTypeStructure = Typed.TypeStructure
  { Typed.equivalentTypes = (==)
  , Typed.observeTypeWithin = \_ _ _ -> Right ()
  , Typed.validTypeAnnotation = const True
  , Typed.functionTypeComponents = const Nothing
  , Typed.tupleTypeComponents = const Nothing
  , Typed.constructorPatternFieldTypes = \_ _ -> Nothing
  , Typed.validTypeApplicationWitness = \_ _ -> True
  }

fixtureGlobal :: Name
fixtureGlobal = checkedName "Fixture.value"

changedGlobal :: Name
changedGlobal = checkedName "Fixture.changed"

providerGlobal :: Name
providerGlobal = checkedName "Fixture.provider"

checkedName :: String -> Name
checkedName spelling = case parseName spelling of
  Left failure -> error $ "invalid fixture name: " ++ show failure
  Right name -> name

nodeId :: Natural -> Typed.TermNodeId
nodeId = Typed.termNodeId

occurrenceId :: Natural -> Typed.OccurrenceId
occurrenceId = Typed.occurrenceId

expectRight :: Show error => Either error value -> IO value
expectRight result = case result of
  Left failure -> assertFailure $ "unexpected rejection: " ++ show failure
  Right value -> pure value
