{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Data.Void (Void)
import Data.Foldable (toList)

import Language.Haskell.Djex.Exference
import Language.Haskell.Djex.Exference.FrontendSupport
  ( allocateFreshTypeVariableId
  , mkExferenceRequestWithSourceInfo
  , sealPreparedExferenceSessionWithPolicy
  , sessionClasses
  , sessionTypeNames
  , validateExferenceTarget
  )
import Language.Haskell.Exference.Core.Declaration
  ( prepareNeutralSynthesisInventory )
import qualified Language.Haskell.Exference.Core.Types as CoreTypes
import Language.Haskell.Synthesis.Inventory
  ( InventoryError
  , mkInventory
  )
import Language.Haskell.Synthesis.KindInference
  ( KindInventoryPolicy (OpenKindInventory) )
import Language.Haskell.Synthesis.Name
  ( Boxity (Boxed)
  , mkIdentifier
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
main = defaultMain $ testGroup "Exference core API"
  [ testCase "the parser-neutral frontend-support boundary is complete" $ do
      inventory <- expectRight
        (mkInventory OpenKindInventory []
          :: Either
              (InventoryError ExferenceTypeVariable Void)
              ExferenceInventory)
      prepared <- expectRight $ prepareNeutralSynthesisInventory inventory
      session <- expectRight
        $ sealPreparedExferenceSessionWithPolicy [] mempty prepared

      sessionTypeNames session @?= []
      sessionClasses session @?= mempty
      allocateFreshTypeVariableId mempty @?= Just 0

      target <- expectRight $ mkIdentifier "identity"
      checkedTarget <- expectRight $ validateExferenceTarget target
      let variable = FlexibleVariable 0
          goal = FunctionType
            (TypeVariable variable)
            (TypeVariable variable)
      request <- expectRight
        $ mkExferenceRequestWithSourceInfo mempty Nothing QueryRequest
          { requestTarget = checkedTarget
          , requestGoal = goal
          , requestContexts = []
          , requestOptions = defaultExferenceOptions
          }
      results <- expectRight $ runExferenceQuery session request
      assertBool "the core-only adapter found no identity candidate"
        $ any (not . null . batchCandidates . resultSearch) results
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
