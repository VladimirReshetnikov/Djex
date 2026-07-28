module Main (main) where

import qualified Data.Map.Strict as Map
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertEqual, testCase)

import Language.Haskell.Exference.Core.Candidate
  ( emptyExferenceSourceTypeVariableHints )
import Language.Haskell.Exference.Core.Expression (Expression (..))
import Language.Haskell.Exference.Core.ExpressionCheck (checkExpression)
import Language.Haskell.Exference.Core.FunctionBinding
  ( ConstructorBinding (..)
  , DeconstructorBinding (..)
  , EnvDictionary (..)
  , FunctionBinding (..)
  )
import qualified Language.Haskell.Exference.Core.Internal.Exference as E
import Language.Haskell.Exference.Core.Internal.FlexibleIds
  ( allocateNamespace )
import Language.Haskell.Exference.Core.Internal.Options
  ( ExferenceHeuristicsConfig (..)
  , ExferenceOptions (..)
  , defaultHeuristicsConfig
  )
import Language.Haskell.Exference.Core.Internal.Polytype
  ( instantiateLeadingForallsWith )
import Language.Haskell.Exference.Core.Internal.Testing
import Language.Haskell.Exference.Core.Internal.VariableSupply
  ( supplyFromIdentifiers )
import Language.Haskell.Exference.Core.Score (Penalty (..))
import qualified Language.Haskell.Exference.Core.Score as Score
import Language.Haskell.Exference.Core.Types
import qualified Language.Haskell.Synthesis.Candidate as SharedCandidate
import qualified Language.Haskell.Synthesis.Generated as Generated
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Query as SharedQuery
import qualified Language.Haskell.Synthesis.Search as SharedSearch

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Exference private engine boundaries"
  [ testCase "prepared queries consume one validated options witness" $ do
      environment <- expectRight $ sealLegacyEnvironment identityInput
      target <- checkedIdentifierTarget "singleOptionValidation"
      let query = legacyInputQuery identityInput
          sourceHints = emptyExferenceSourceTypeVariableHints
            $ E.queryGoalType query
      singleOptionValidationStrictnessForTesting
        target sourceHints environment query @?= Right ()
  , testCase "term identifier exhaustion truncates instead of colliding" $ do
      chunk <- lastCapacityChunk
        (IdentifierCapacities 0 100 100) identityInput
      E.chunkStatus chunk @?=
        E.SearchStatus E.SearchIdentifierSpaceExhausted 0 0
      assertBool "an exhausted term-ID branch produced a candidate"
        $ null $ E.chunkElements chunk
  , testCase "identifier truncation survives a successful sibling" $ do
      let integer = TypeCons $ name "Int"
          polymorphic = FunctionBinding
            (TypeVar 0) (name "polymorphic") 0 [] []
          constant = FunctionBinding integer (name "constant") 0 [] []
          input = identityInput
            { E.input_goalType = integer
            , E.input_envFuncs = [polymorphic, constant]
            }
          capacities = IdentifierCapacities 100 0 100
      chunk <- lastCapacityChunk capacities input
      E.searchCompletion (E.chunkStatus chunk) @?=
        E.SearchIdentifierSpaceExhausted
      assertBool "a viable sibling was suppressed by identifier exhaustion"
        $ not $ null $ E.chunkElements chunk
      target <- checkedIdentifierTarget "generated"
      environment <- expectRight $ sealLegacyEnvironment input
      results <- expectRight
        $ findQueryResultsWithIdentifierCapacitiesEither
            capacities target
            (emptyExferenceSourceTypeVariableHints $ E.input_goalType input)
            environment (legacyInputQuery input)
      result <- lastResult results
      let batch = SharedQuery.resultSearch result
      SharedQuery.resultEvidence result @?=
        SharedQuery.ValidatedCandidates
      SharedSearch.batchProgress batch @?= SharedSearch.Completed
        (SharedSearch.Truncated
          $ SharedSearch.IdentifierSpaceExhausted :| [])
      assertBool "the direct result lost its checked target"
        $ all ((== target) . Generated.clauseName
            . SharedCandidate.candidateOutput)
        $ SharedSearch.batchCandidates batch
  , testCase "provider forall exhaustion truncates the affected branch" $ do
      let integer = TypeCons $ name "Int"
          polymorphic = TypeForall [0] []
            $ TypeArrow (TypeVar 0) (TypeVar 0)
          input = identityInput
            { E.input_goalType =
                TypeArrow polymorphic $ TypeArrow integer integer
            , E.input_maxSteps = 100
            }
      -- Binder 0 already occupies the only flexible slot. Per-use
      -- instantiation needs one additional spelling and must fail without
      -- wrapping or reusing the binder identity.
      chunk <- lastCapacityChunk
        (IdentifierCapacities 100 1 100) input
      E.chunkStatus chunk @?=
        E.SearchStatus E.SearchIdentifierSpaceExhausted 0 0
      assertBool "an exhausted forall instantiation produced a candidate"
        $ null $ E.chunkElements chunk
  , testCase "bare provider foralls cross the checked result boundary" $ do
      let unit = TypeTuple Boxed []
          vacuousUnit = TypeForall [] [] unit
          polymorphic = TypeForall [0] [] $ TypeVar 0
          input = identityInput
            { E.input_goalType = TypeArrow polymorphic unit
            , E.input_maxSteps = 100
            }
      chunks <- expectRight
        $ findExpressionsWithIdentifierCapacitiesEither
            (IdentifierCapacities 100 100 100) input
      assertBool
        "forall a. a was mistaken for opaque forwarding at a flexible use"
        $ not $ null $ concatMap E.chunkElements chunks
      wrappedChunks <- expectRight
        $ findExpressionsWithIdentifierCapacitiesEither
            (IdentifierCapacities 100 100 100)
            (input {E.input_goalType = TypeArrow polymorphic vacuousUnit})
      assertBool "a vacuous forall wrapper suppressed provider instantiation"
        $ not $ null $ concatMap E.chunkElements wrappedChunks
      let wrappedOccurrence = ExpLambda 1 polymorphic
            $ ExpVar 1 $ TypeForall [] [] $ TypeVar 2
      checkExpression (mkQueryClassEnv emptyStaticClassEnv []) [] []
        (TypeArrow polymorphic vacuousUnit) [] wrappedOccurrence @?= Right ()
      let distinct = TypeForall [1] []
            $ TypeArrow (TypeVar 1) (TypeVar 1)
      quantifiedChunks <- expectRight
        $ findExpressionsWithIdentifierCapacitiesEither
            (IdentifierCapacities 100 100 100)
            (input {E.input_goalType = TypeArrow polymorphic distinct})
      assertBool "provider elimination crossed a quantified goal"
        $ null $ concatMap E.chunkElements quantifiedChunks
  , testCase "provider forall opening preserves nested lexical scopes" $ do
      let outerClass = name "Outer"
          innerClass = name "Inner"
          evidence className variable = HsConstraint className [variable]
          shadowed = TypeForall [0] [evidence outerClass $ TypeVar 0]
            $ TypeForall [0] [evidence innerClass $ TypeVar 0]
            $ TypeVar 0
      case instantiateLeadingForallsWith
          allocateNamespace (supplyFromIdentifiers []) shadowed of
        Just
            ( TypeVar bodyIdentifier
            , [ HsConstraint outerName [TypeVar outerIdentifier]
              , HsConstraint innerName [TypeVar innerIdentifier]
              ]
            , _
            ) -> do
          outerName @?= outerClass
          innerName @?= innerClass
          assertBool "shadowed forall binders reused one fresh identity"
            $ outerIdentifier /= innerIdentifier
          bodyIdentifier @?= innerIdentifier
        actual -> fail $ "unexpected shadowed-forall instantiation: "
          ++ show actual
  , testCase "generic deconstructors need no persistent flexible IDs" $ do
      let integer = TypeCons $ name "Int"
          box argument = TypeApp (TypeCons $ name "Box") argument
          deconstructor = DeconstructorBinding
            (box $ TypeVar 0)
            [ConstructorBinding (name "Box") [TypeVar 0]]
            False
          input = identityInput
            { E.input_goalType = TypeArrow (box integer) integer
            , E.input_envDeconsS = [deconstructor]
            }
          capacities = IdentifierCapacities 100 0 100
          isBoxElimination candidate = case candidate of
            ( ExpLambda scrutinee _
                (ExpLetMatch constructor [(field, annotation)]
                  (ExpVar matchedScrutinee _)
                  (ExpVar returnedField _))
              , []
              , _
              ) -> constructor == name "Box"
                && matchedScrutinee == scrutinee
                && returnedField == field
                && annotation == integer
            _ -> False
      chunks <- expectRight
        $ findExpressionsWithIdentifierCapacitiesEither capacities input
      finalChunk <- lastChunk "generic Box elimination" chunks
      E.chunkStatus finalChunk @?= E.SearchStatus E.SearchExhausted 0 0
      assertBool "generic Box elimination consumed a flexible ID"
        $ any isBoxElimination $ concatMap E.chunkElements chunks
  , testCase "multi-case deconstructors need no persistent flexible IDs" $ do
      let integer = TypeCons $ name "Int"
          choice argument = TypeApp (TypeCons $ name "Choice") argument
          genericChoice = choice $ TypeVar 0
          integerChoice = choice integer
          leftName = name "First"
          rightName = name "Second"
          deconstructor = DeconstructorBinding genericChoice
            [ ConstructorBinding leftName [TypeVar 0]
            , ConstructorBinding rightName [TypeVar 0]
            ] False
          input = identityInput
            { E.input_goalType = TypeArrow integerChoice integer
            , E.input_envDeconsS = [deconstructor]
            , E.input_multiPM = True
            , E.input_maxSteps = 200
            }
          capacities = IdentifierCapacities 100 0 100
          returnsAnnotatedField (_, [(field, annotation)], body) =
            annotation == integer && body == ExpVar field annotation
          returnsAnnotatedField _ = False
          isChoiceElimination candidate = case candidate of
            ( ExpLambda scrutinee _
                (ExpCaseMatch (ExpVar matchedScrutinee _) alternatives)
              , []
              , _
              ) -> matchedScrutinee == scrutinee
                && map (\(constructor, _, _) -> constructor) alternatives
                  == [leftName, rightName]
                && all returnsAnnotatedField alternatives
            _ -> False
      chunks <- expectRight
        $ findExpressionsWithIdentifierCapacitiesEither capacities input
      finalChunk <- lastChunk "generic Choice elimination" chunks
      E.chunkStatus finalChunk @?= E.SearchStatus E.SearchExhausted 0 0
      assertBool "generic Choice elimination consumed a flexible ID"
        $ any isChoiceElimination $ concatMap E.chunkElements chunks
  , testCase "empty elimination consumes no flexible identifier" $ do
      let integer = TypeCons $ name "Int"
          empty argument = TypeApp (TypeCons $ name "Empty") argument
          deconstructor = DeconstructorBinding
            (empty $ TypeVar 0) [] False
          input = identityInput
            { E.input_goalType = TypeArrow (empty integer) integer
            , E.input_envDeconsS = [deconstructor]
            }
      chunk <- lastCapacityChunk
        (IdentifierCapacities 100 0 100) input
      E.chunkStatus chunk @?= E.SearchStatus E.SearchExhausted 0 0
      assertBool "empty elimination required a non-escaping flexible ID"
        $ not $ null $ E.chunkElements chunk
  , testCase "scope identifier collisions are operational truncations" $ do
      chunk <- lastCapacityChunk
        (IdentifierCapacities 100 100 1) identityInput
      E.chunkStatus chunk @?=
        E.SearchStatus E.SearchIdentifierSpaceExhausted 0 0
  , testCase "exact progress retains simultaneous step and ID limits" $ do
      let integer = TypeCons $ name "Int"
          boolean = TypeCons $ name "Bool"
          polymorphic = FunctionBinding
            (TypeVar 0) (name "polymorphic") 0 [] []
          deferred = FunctionBinding boolean (name "deferred") 0 [] [integer]
          input = identityInput
            { E.input_goalType = boolean
            , E.input_envFuncs = [polymorphic, deferred]
            -- The root opens even an empty leading forall before the
            -- binding-expansion step under test.
            , E.input_maxSteps = 2
            }
          capacities = IdentifierCapacities 100 0 100
      chunk <- lastCapacityChunk capacities input
      E.searchCompletion (E.chunkStatus chunk) @?=
        E.SearchIdentifierSpaceExhausted
      target <- checkedIdentifierTarget "generated"
      environment <- expectRight $ sealLegacyEnvironment input
      results <- expectRight
        $ findQueryResultsWithIdentifierCapacitiesEither
            capacities target
            (emptyExferenceSourceTypeVariableHints $ E.input_goalType input)
            environment (legacyInputQuery input)
      result <- lastResult results
      let batch = SharedQuery.resultSearch result
      SharedQuery.resultEvidence result @?= SharedQuery.NoEvidence
      SharedSearch.batchProgress batch @?= SharedSearch.Completed
        (SharedSearch.Truncated
          $ SharedSearch.StepLimitReached
            :| [SharedSearch.IdentifierSpaceExhausted])
  , testCase "queue representation overflow retains the best priorities" $ do
      let queued = [(2, 20)]
          generated = [(3, 30), (1, 10)]
      mergePriorityQueueAtCapacity 3 Nothing queued generated @?=
        ([(3, 30), (2, 20), (1, 10)], 0)
      mergePriorityQueueAtCapacity 2 Nothing queued generated @?=
        ([(3, 30), (2, 20)], 1)
      mergePriorityQueueAtCapacity 2 (Just 1) queued generated @?=
        ([(3, 30)], 2)
      mergePriorityQueueAtCapacity 3 (Just (-1)) queued generated @?=
        ([], 3)
  , testCase "compatibility pruning counts saturate without losing reasons" $ do
      let maximumCount = fromIntegral (maxBound :: Int) :: Natural
          queueTotal = maximumCount + 1
          depthTotal = maximumCount + 2
          binding = name "usedBinding"
      compatibilityPruningCount (maximumCount - 1) @?= maxBound - 1
      compatibilityPruningCount maximumCount @?= maxBound
      compatibilityPruningCount queueTotal @?= maxBound
      compatibilityBindingUsageCounts
          (Map.singleton binding queueTotal) @?=
        Map.singleton binding maxBound
      pruningReasonsFromNaturalTotals queueTotal depthTotal @?=
        [ SharedSearch.QueueLimitPruned queueTotal
        , SharedSearch.DepthLimitPruned depthTotal
        ]
  , testCase "structural tuple ranking exactly preserves applications" $ do
      tupleConstructor <- expectRight $ SharedName.tupleName Boxed 3
      let elements =
            [ TypeVar 0
            , TypeConstant 1
            , TypeArrow (TypeVar 2) (TypeConstant 3)
            ]
          structural = TypeTuple Boxed elements
          legacy = foldl TypeApp (TypeCons tupleConstructor) elements
          fractional = defaultHeuristicsConfig
            { heuristics_goalVar = 0.1
            , heuristics_goalCons = 0.2
            , heuristics_goalArrow = 0.3
            , heuristics_goalApp = 0.4
            }
          near divisor = Penalty
            $ Score.penaltyValue Score.maxPenalty / divisor
          nearSaturation = defaultHeuristicsConfig
            { heuristics_goalVar = near 13
            , heuristics_goalCons = near 11
            , heuristics_goalArrow = near 9
            , heuristics_goalApp = near 7
            }
          complexity config = typeComplexityForTesting config
      assertEqual "fractional accumulation order"
        (complexity fractional legacy)
        (complexity fractional structural)
      assertEqual "near-saturation accumulation order"
        (complexity nearSaturation legacy)
        (complexity nearSaturation structural)
  , testCase "query-result projection preserves its envelope lazily" $ do
      targetName <- expectRight $ SharedName.mkOperator "<~>"
      target <- expectRight $ Generated.mkDefinitionName targetName
      let metadata = E.ExferenceBatchMetadata Map.empty 2 3
          (hasValidatedEvidence, progress, observedMetadata,
            observedTarget) =
              queryProjectionStrictnessForTesting target Map.empty
      hasValidatedEvidence @?= True
      progress @?= SharedSearch.Continuing
      observedMetadata @?= metadata
      observedTarget @?= target
  ]

identityInput :: E.ExferenceInput
identityInput = E.ExferenceInput
  (TypeArrow (TypeVar 0) (TypeVar 0))
  [] [] emptyStaticClassEnv
  False False 0 False 20 Nothing Nothing defaultHeuristicsConfig

legacyInputEnvironment :: E.ExferenceInput -> EnvDictionary
legacyInputEnvironment input = EnvDictionary
  { environmentFunctions = E.input_envFuncs input
  , environmentDeconstructors = E.input_envDeconsS input
  , environmentClasses = E.input_envClasses input
  }

legacyInputQuery :: E.ExferenceInput -> E.ExferenceQuery
legacyInputQuery input = E.ExferenceQuery
  { E.queryGoalType = E.input_goalType input
  , E.queryExcludedBindings = Set.empty
  , E.querySearchOptions = ExferenceOptions
      { exferenceAllowUnused = E.input_allowUnused input
      , exferenceAllowResidualConstraints = E.input_allowConstraints input
      , exferenceConstraintDeferralSteps =
          E.input_allowConstraintsStopStep input
      , exferenceMultiConstructorPatterns = E.input_multiPM input
      , exferenceMaximumSteps = E.input_maxSteps input
      , exferenceMaximumQueueSize = E.input_maxQueueSize input
      , exferenceMaximumDepth = E.input_maxDepth input
      , exferenceHeuristics = E.input_heuristicsConfig input
      }
  }

sealLegacyEnvironment
  :: E.ExferenceInput
  -> Either E.ExferenceInputError E.ExferenceEnvironment
sealLegacyEnvironment = E.mkExferenceEnvironment . legacyInputEnvironment

lastCapacityChunk
  :: IdentifierCapacities
  -> E.ExferenceInput
  -> IO E.ExferenceChunkElement
lastCapacityChunk capacities input = do
  chunks <- expectRight
    $ findExpressionsWithIdentifierCapacitiesEither capacities input
  lastChunk "capacity-limited search" chunks

lastChunk :: String -> [value] -> IO value
lastChunk description chunks = case chunks of
  [] -> fail $ description ++ " produced no search chunks"
  first : remaining -> pure $ lastElement first remaining

lastResult :: [value] -> IO value
lastResult results = case results of
  [] -> fail "expected at least one capacity-limited query result"
  first : remaining -> pure $ lastElement first remaining

lastElement :: value -> [value] -> value
lastElement latest [] = latest
lastElement _ (next : remaining) = lastElement next remaining

checkedIdentifierTarget :: String -> IO Generated.DefinitionName
checkedIdentifierTarget spelling = do
  targetName <- expectRight $ SharedName.mkIdentifier spelling
  expectRight $ Generated.mkDefinitionName targetName

name :: String -> QualifiedName
name spelling = either (error . show) id $ mkQualifiedName [] spelling

expectRight :: Show problem => Either problem result -> IO result
expectRight = either (fail . show) pure
