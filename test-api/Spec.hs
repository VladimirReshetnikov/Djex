{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM_)
import Data.Either (isRight)
import Data.Foldable (toList)
import Data.List (isInfixOf)
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)

import Djinn.Core (parseHType)
import qualified Djinn.Internal.LJTFormula as LJT
import qualified Djinn.Internal.ProofEnv as ProofEnv
import Language.Haskell.Djex.Exference
import Language.Haskell.Djex.Exference.FrontendSupport
  ( allocateFreshTypeVariableId
  , mkExferenceRequestWithSourceInfo
  , sealPreparedExferenceSessionWithPolicy
  , validateExferenceTarget
  )
import Language.Haskell.Exference.Core.Declaration
  ( prepareSynthesisInventory )
import qualified Language.Haskell.Exference.Core.Declaration as Declaration
import Language.Haskell.Exference.Core
  ( ExferenceQuery (..)
  , emptyExferenceSourceTypeVariableHints
  , findQueryResultsInEnvironmentEither
  , mkExferenceEnvironment
  )
import Language.Haskell.Exference.Core.FunctionBinding
  ( EnvDictionary (..)
  )
import qualified Language.Haskell.Exference.Core.RigidInstantiation as Rigid
import qualified Language.Haskell.Exference.Core.Types as CoreTypes
import Language.Haskell.Synthesis.Candidate (candidateOutput)
import Language.Haskell.Synthesis.Constraint (Constraint (..))
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , diagnosticCode
  , diagnosticSource
  , diagnosticSpan
  , sourceTextLocation
  , sourceTextSpan
  )
import Language.Haskell.Synthesis.Generated (clauseName)
import qualified Language.Haskell.Synthesis.Environment as Environment
import Language.Haskell.Synthesis.Inventory
  ( InventoryError
  , mkInventory
  )
import qualified Language.Haskell.Synthesis.Inventory as Inventory
import Language.Haskell.Synthesis.KindInference
  ( KindInventoryPolicy (OpenKindInventory) )
import qualified Language.Haskell.Synthesis.KindInference as KindInference
import Language.Haskell.Synthesis.Name
  ( Boxity (Boxed)
  , mkIdentifier
  , mkOperator
  , tupleName
  )
import Language.Haskell.Synthesis.Query
  ( QueryRequest (..)
  , resultSearch
  )
import qualified Language.Haskell.Synthesis.Query as Query
import Language.Haskell.Synthesis.Search (batchCandidates)
import qualified Language.Haskell.Synthesis.Search as Search
import Language.Haskell.Synthesis.Type
  ( Type (FunctionType, TypeVariable)
  , Variable (FlexibleVariable, RigidVariable)
  )
import qualified Language.Haskell.Synthesis.TypeSynonym as TypeSynonym
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit
  ( (@?=)
  , assertBool
  , assertFailure
  , testCase
  )

import AbstractionBoundary
  ( allowedConstructionAttempts
  , forbiddenConstructionAttempts
  )
import FacadeSpec (facadeTests)

-- These references compile without deferred type errors. They pin the result
-- type of every ordinary projection whose same-named record label is rejected
-- by 'AbstractionBoundary'.
projectionSignatures :: ()
projectionSignatures =
  (Inventory.inventoryEnvironment
    :: Inventory.Inventory Int ()
    -> Environment.Environment Int Void ()) `seq`
  (Inventory.inventoryKindAssumptions
    :: Inventory.Inventory Int ()
    -> KindInference.KindAssumptions) `seq`
  (Query.resultEvidence
    :: Query.QueryResult () ()
    -> Query.QueryEvidence) `seq`
  (Query.resultSearch
    :: Query.QueryResult () ()
    -> Search.SearchBatch () ()) `seq`
  (Rigid.rigidInstantiations
    :: Rigid.RigidInstantiationPlan
    -> [(CoreTypes.TVarId, CoreTypes.TVarId)]) `seq`
  (TypeSynonym.preparedInventory
    :: TypeSynonym.PreparedInventory Int ()
    -> Inventory.Inventory Int ()) `seq`
  (TypeSynonym.preparedTypeSynonyms
    :: TypeSynonym.PreparedInventory Int ()
    -> TypeSynonym.TypeSynonyms Int) `seq`
  (CoreTypes.sClassEnv_tclasses
    :: CoreTypes.StaticClassEnv
    -> Map.Map CoreTypes.QualifiedName CoreTypes.HsTypeClass) `seq`
  (CoreTypes.sClassEnv_explicitInstances
    :: CoreTypes.StaticClassEnv
    -> [CoreTypes.HsInstance]) `seq`
  (CoreTypes.sClassEnv_instances
    :: CoreTypes.StaticClassEnv
    -> Map.Map CoreTypes.QualifiedName [CoreTypes.HsInstance]) `seq`
  (CoreTypes.qClassEnv_env
    :: CoreTypes.QueryClassEnv
    -> CoreTypes.StaticClassEnv) `seq`
  (CoreTypes.qClassEnv_constraints
    :: CoreTypes.QueryClassEnv
    -> Set.Set CoreTypes.HsConstraint) `seq`
  (CoreTypes.qClassEnv_inflatedConstraints
    :: CoreTypes.QueryClassEnv
    -> Set.Set CoreTypes.HsConstraint) `seq`
  (Declaration.preparedSynthesisWitness
    :: Declaration.PreparedSynthesisInventory ()
    -> TypeSynonym.PreparedInventory CoreTypes.SynthesisVariable ()) `seq`
  (Declaration.preparedSynthesisBackend
    :: Declaration.PreparedSynthesisInventory ()
    -> EnvDictionary) `seq`
  (ProofEnv.proofBindings
    :: ProofEnv.ProofEnvironment
    -> [(LJT.Symbol, LJT.Formula)]) `seq`
  (ProofEnv.proofBindingsIncludingTarget
    :: ProofEnv.ProofEnvironment
    -> [(LJT.Symbol, LJT.Formula)]) `seq`
  (ProofEnv.targetWasExcluded
    :: ProofEnv.ProofEnvironment
    -> Bool) `seq`
  ()

main :: IO ()
main = defaultMain $ testGroup "Djex downstream API"
  [ facadeTests
  , testCase "both engines are available from one dependency" $
      assertBool "the Djinn parser was unavailable from djex"
        $ isRight $ parseHType "a -> a"
  , testCase "abstraction-boundary controls can obtain public dictionaries" $
      forM_ allowedConstructionAttempts $ \(description, attempt) -> do
        result <- try $ evaluate attempt
        case result :: Either SomeException () of
          Left exception -> assertFailure $
            description ++ " failed: " ++ displayException exception
          Right () -> pure ()
  , testCase "opaque witnesses expose no representation constructors" $
      projectionSignatures `seq`
      forM_ forbiddenConstructionAttempts (\(description, attempt) -> do
        result <- try $ evaluate attempt
        case result :: Either SomeException () of
          Left exception -> do
            let message = displayException exception
                isMissingDictionary =
                  "No instance for" `isInfixOf` message &&
                  ("Generic" `isInfixOf` message ||
                    "HasField" `isInfixOf` message)
            assertBool
              (description ++ " raised an unrelated exception: " ++ message)
              isMissingDictionary
          Right () -> assertFailure description)
  , testCase "the Exference frontend-support boundary is complete" $ do
      inventory <- expectRight
        (mkInventory OpenKindInventory []
          :: Either
              (InventoryError ExferenceTypeVariable Void)
              ExferenceInventory)
      prepared <- expectRight $ prepareSynthesisInventory inventory
      session <- expectRight
        $ sealPreparedExferenceSessionWithPolicy [] mempty prepared

      allocateFreshTypeVariableId mempty @?= Just 0
      allocateFreshTypeVariableId (IntSet.singleton maxBound) @?= Just 0
      allocateFreshTypeVariableId
          (IntSet.fromList [0, maxBound, minBound]) @?= Just 1

      target <- expectRight $ mkOperator "<~>"
      checkedTarget <- expectRight $ validateExferenceTarget target
      let variable = FlexibleVariable 0
          goal = FunctionType
            (TypeVariable variable)
            (TypeVariable variable)
          options = defaultExferenceOptions
            { exferenceMaximumSteps = 16 }
      let source = "a -> a"
          location = sourceTextLocation "djex-api" source
      request <- expectRight
        $ mkExferenceRequestWithSourceInfo mempty location QueryRequest
          { requestTarget = checkedTarget
          , requestGoal = goal
          , requestContexts = []
          , requestOptions = options
          }
      className <- expectRight $ mkIdentifier "C"
      case mkExferenceRequestWithSourceInfo mempty location QueryRequest
          { requestTarget = checkedTarget
          , requestGoal = goal
          , requestContexts =
              [Constraint className [TypeVariable $ FlexibleVariable 1]]
          , requestOptions = options
          } of
        Left failure -> do
          diagnosticCode failure @?= Just "DJEX_EXF_REQUEST"
          diagnosticSource failure @?= Just "djex-api"
          diagnosticSpan failure @?= Just (sourceTextSpan source)
        Right _ -> assertFailure
          "the sourced SPI accepted an out-of-scope context variable"
      case mkExferenceRequestWithSourceInfo
          (Map.singleton "where" 0) location QueryRequest
          { requestTarget = checkedTarget
          , requestGoal = goal
          , requestContexts = []
          , requestOptions = options
          } of
        Left failure -> do
          diagnosticCode failure @?= Just "DJEX_EXF_SOURCE_HINT"
          diagnosticSource failure @?= Just "djex-api"
          diagnosticSpan failure @?= Just (sourceTextSpan source)
        Right _ -> assertFailure
          "the sourced SPI accepted a reserved type-variable spelling"
      results <- expectRight $ runExferenceQuery session request
      backendGoal <- expectRight $ CoreTypes.fromSynthesisType goal
      coreEnvironment <- expectRight $ mkExferenceEnvironment
        $ EnvDictionary [] [] CoreTypes.emptyStaticClassEnv
      direct <- expectRight $ findQueryResultsInEnvironmentEither
        checkedTarget (emptyExferenceSourceTypeVariableHints backendGoal)
        coreEnvironment ExferenceQuery
          { queryGoalType = backendGoal
          , queryExcludedBindings = mempty
          , queryAllowUnused = exferenceAllowUnused options
          , queryAllowConstraints =
              exferenceAllowResidualConstraints options
          , queryConstraintDeferralSteps =
              exferenceConstraintDeferralSteps options
          , queryMultiConstructorPatterns =
              exferenceMultiConstructorPatterns options
          , queryMaximumSteps = exferenceMaximumSteps options
          , queryMaximumQueueSize = exferenceMaximumQueueSize options
          , queryMaximumDepth = exferenceMaximumDepth options
          , queryHeuristics = exferenceHeuristics options
          }
      results @?= direct
      assertBool "the core-only adapter found no identity candidate"
        $ any (not . null . batchCandidates . resultSearch) results
      case concatMap (batchCandidates . resultSearch) results of
        candidate : _ -> clauseName (candidateOutput candidate) @?=
          checkedTarget
        [] -> assertFailure "the direct result path found no identity"
  , testCase "source-aware request sealing materializes source spans" $ do
      targetName <- expectRight $ mkIdentifier "identity"
      target <- expectRight $ validateExferenceTarget targetName
      let variable = FlexibleVariable 0
          goal = FunctionType
            (TypeVariable variable)
            (TypeVariable variable)
          partialSource = 'a' : error "unforced adapter source tail"
          location = sourceTextLocation "djex-api" partialSource
          query = QueryRequest
            { requestTarget = target
            , requestGoal = goal
            , requestContexts = []
            , requestOptions = defaultExferenceOptions
            }
      result <- try $ evaluate $
        mkExferenceRequestWithSourceInfo mempty location query
      case result
          :: Either
              SomeException
              (Either Diagnostic ExferenceRequest) of
        Left _ -> pure ()
        Right _ -> assertFailure
          "the source-aware adapter retained a lazy source-span traversal"
      let finiteLocation = sourceTextLocation "djex-api" "a -> a"
          partialSpelling = 'a' : error "unforced source-hint tail"
      hintResult <- try $ evaluate $
        mkExferenceRequestWithSourceInfo
          (Map.singleton partialSpelling 0) finiteLocation query
      case hintResult
          :: Either
              SomeException
              (Either Diagnostic ExferenceRequest) of
        Left _ -> pure ()
        Right _ -> assertFailure
          "the source-aware adapter retained a lazy spelling validation"
      let partialMap = Map.fromDistinctAscList
            $ ("source", 0) : error "unforced source-hint map tail"
      mapResult <- try $ evaluate $
        mkExferenceRequestWithSourceInfo partialMap finiteLocation query
      case mapResult
          :: Either
              SomeException
              (Either Diagnostic ExferenceRequest) of
        Left _ -> pure ()
        Right _ -> assertFailure
          "the source-aware adapter retained a lazy source-hint map"
  , testCase "the native shared type has honest compatibility views" $ do
      let rigidForall = CoreTypes.TypeForallNative
            [RigidVariable 7]
            []
            (CoreTypes.TypeTuple Boxed
              [CoreTypes.TypeVar 0, CoreTypes.TypeConstant 7])
      case rigidForall of
        CoreTypes.TypeForall{} -> assertFailure
          "the flexible-only legacy forall view matched a rigid binder"
        CoreTypes.TypeForallNative binders _ _ ->
          binders @?= [RigidVariable 7]
        _ -> assertFailure "the total native forall view did not match"
      CoreTypes.fromSynthesisType rigidForall @?=
        Left (CoreTypes.RigidForallBinder 7)
      CoreTypes.toSynthesisType rigidForall @?=
        Left (CoreTypes.RigidForallBinder 7)
  , testCase "checked conversion stores structural tuples canonically" $ do
      pairName <- expectRight $ tupleName Boxed 2
      let first = CoreTypes.TypeVar 0
          second = CoreTypes.TypeConstant 1
          application = CoreTypes.TypeApp
            (CoreTypes.TypeApp (CoreTypes.TypeCons pairName) first)
            second
      CoreTypes.toSynthesisType application @?=
        Right (CoreTypes.TypeTuple Boxed [first, second])
  , testCase "stable requests store canonical types and reject rigid binders" $ do
      targetName <- expectRight $ mkIdentifier "tupled"
      target <- expectRight $ validateExferenceTarget targetName
      pairName <- expectRight $ tupleName Boxed 2
      let first = CoreTypes.TypeVar 0
          second = CoreTypes.TypeConstant 1
          application = CoreTypes.TypeApp
            (CoreTypes.TypeApp (CoreTypes.TypeCons pairName) first)
            second
          request goal = QueryRequest
            { requestTarget = target
            , requestGoal = goal
            , requestContexts = []
            , requestOptions = defaultExferenceOptions
            }
      checked <- expectRight $ mkExferenceRequest $ request application
      requestGoal (exferenceRequestQuery checked) @?=
        CoreTypes.TypeTuple Boxed [first, second]
      case mkExferenceRequest $ request $ CoreTypes.TypeForallNative
          [RigidVariable 2] [] first of
        Left _ -> pure ()
        Right _ -> assertFailure "a stable request retained a rigid forall binder"
  , testCase "sealed class environments store canonical constraints" $ do
      className <- expectRight $ mkIdentifier "C"
      pairName <- expectRight $ tupleName Boxed 2
      let first = CoreTypes.TypeVar 0
          second = CoreTypes.TypeConstant 1
          application = CoreTypes.TypeApp
            (CoreTypes.TypeApp (CoreTypes.TypeCons pairName) first)
            second
          canonical = CoreTypes.TypeTuple Boxed [first, second]
          classDeclaration = CoreTypes.HsTypeClass className [0] []
          sourceInstance = CoreTypes.HsInstance []
            $ CoreTypes.HsConstraint className [application]
          canonicalInstance = CoreTypes.HsInstance []
            $ CoreTypes.HsConstraint className [canonical]
      environment <- expectRight
        $ CoreTypes.mkStaticClassEnv [classDeclaration] [sourceInstance]
      CoreTypes.sClassEnv_explicitInstances environment @?=
        [canonicalInstance]
      let queryEnvironment = CoreTypes.mkQueryClassEnv environment
            [CoreTypes.HsConstraint className [application]]
      toList (CoreTypes.qClassEnv_constraints queryEnvironment) @?=
        [CoreTypes.HsConstraint className [canonical]]
  ]

expectRight :: Show error => Either error value -> IO value
expectRight result = case result of
  Left failure -> fail $ show failure
  Right value -> pure value
