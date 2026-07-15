{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Data.Either (isRight)
import Data.Foldable (toList)
import Data.Void (Void)

import Djinn.Core (parseHType)
import Language.Haskell.Djex.Exference
import Language.Haskell.Djex.Exference.FrontendSupport
  ( allocateFreshTypeVariableId
  , mkExferenceRequestWithSourceInfo
  , sealPreparedExferenceSessionWithPolicy
  , validateExferenceTarget
  )
import Language.Haskell.Exference.Core.Declaration
  ( prepareNeutralSynthesisInventory )
import Language.Haskell.Exference.Core
  ( ExferenceQuery (..)
  , findQueryResultsInEnvironmentEither
  , mkExferenceEnvironment
  )
import Language.Haskell.Exference.Core.FunctionBinding
  ( EnvDictionary (..)
  )
import qualified Language.Haskell.Exference.Core.Types as CoreTypes
import Language.Haskell.Synthesis.Candidate (candidateOutput)
import Language.Haskell.Synthesis.Generated (clauseName)
import Language.Haskell.Synthesis.Inventory
  ( InventoryError
  , mkInventory
  )
import Language.Haskell.Synthesis.KindInference
  ( KindInventoryPolicy (OpenKindInventory) )
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
import Language.Haskell.Synthesis.Search (batchCandidates)
import Language.Haskell.Synthesis.Type
  ( Type (FunctionType, TypeVariable)
  , Variable (FlexibleVariable, RigidVariable)
  )
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit
  ( (@?=)
  , assertBool
  , assertFailure
  , testCase
  )

main :: IO ()
main = defaultMain $ testGroup "Djex parser-free API"
  [ testCase "both engines are available from one parser-free dependency" $
      assertBool "the Djinn parser was unavailable from djex"
        $ isRight $ parseHType "a -> a"
  , testCase "the Exference frontend-support boundary is complete" $ do
      inventory <- expectRight
        (mkInventory OpenKindInventory []
          :: Either
              (InventoryError ExferenceTypeVariable Void)
              ExferenceInventory)
      prepared <- expectRight $ prepareNeutralSynthesisInventory inventory
      session <- expectRight
        $ sealPreparedExferenceSessionWithPolicy [] mempty prepared

      allocateFreshTypeVariableId mempty @?= Just 0

      target <- expectRight $ mkOperator "<~>"
      checkedTarget <- expectRight $ validateExferenceTarget target
      let variable = FlexibleVariable 0
          goal = FunctionType
            (TypeVariable variable)
            (TypeVariable variable)
          options = defaultExferenceOptions
            { exferenceMaximumSteps = 16 }
      request <- expectRight
        $ mkExferenceRequestWithSourceInfo mempty Nothing QueryRequest
          { requestTarget = checkedTarget
          , requestGoal = goal
          , requestContexts = []
          , requestOptions = options
          }
      results <- expectRight $ runExferenceQuery session request
      backendGoal <- expectRight $ CoreTypes.fromSynthesisType goal
      coreEnvironment <- expectRight $ mkExferenceEnvironment
        $ EnvDictionary [] [] CoreTypes.emptyStaticClassEnv
      direct <- expectRight $ findQueryResultsInEnvironmentEither
        checkedTarget mempty coreEnvironment ExferenceQuery
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
