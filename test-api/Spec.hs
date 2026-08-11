{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM_)
import Data.Either (isRight)
import Data.Foldable (toList)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import Numeric.Natural (Natural)

import Djinn.Core (parseHType)
import qualified Djinn.Internal.LJTFormula as LJT
import qualified Djinn.Internal.ProofEnv as ProofEnv
import Language.Haskell.Djex.Exference
import Language.Haskell.Djex (Backend (DjinnBackend))
import Language.Haskell.Djex.CLI (runArguments)
import Language.Haskell.Djex.REPL
  ( ReplBackend (OneBackend)
  , ReplOptions (..)
  , defaultReplOptions
  , runRepl
  )
import System.Exit (ExitCode)
import qualified Language.Haskell.Exference.Core.Declaration as Declaration
import Language.Haskell.Exference.Core.FunctionBinding
  ( EnvDictionary (..)
  )
import qualified Language.Haskell.Exference.Core.RigidInstantiation as Rigid
import qualified Language.Haskell.Exference.Core.Types as CoreTypes
import qualified Language.Haskell.Synthesis.Declaration as SynthesisDeclaration
import qualified Language.Haskell.Synthesis.Environment as Environment
import qualified Language.Haskell.Synthesis.Fingerprint as Fingerprint
import Language.Haskell.Synthesis.Generated (mkDefinitionName)
import qualified Language.Haskell.Synthesis.Inventory as Inventory
import qualified Language.Haskell.Synthesis.KindInference as KindInference
import Language.Haskell.Synthesis.Name
  ( Boxity (Boxed)
  , Name
  , mkIdentifier
  , tupleName
  )
import Language.Haskell.Synthesis.Query
  ( QueryRequest (..) )
import qualified Language.Haskell.Synthesis.Query as Query
import qualified Language.Haskell.Synthesis.Search as Search
import qualified Language.Haskell.Synthesis.Semantic.Length as Length
import qualified Language.Haskell.Synthesis.TypedGenerated as TypedGenerated
import qualified Language.Haskell.Synthesis.TypedGenerated.Fingerprint
  as TypedGeneratedFingerprint
import Language.Haskell.Synthesis.Type
  ( Type
  , Variable (RigidVariable)
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
  (Inventory.inventoryClassArity
    :: Inventory.Inventory Int ()
    -> Name
    -> Maybe Int) `seq`
  (KindInference.inferTypeKind
    :: KindInference.KindAssumptions
    -> Type Int
    -> Either (KindInference.KindInferenceError Int)
        KindInference.InferredKind) `seq`
  (KindInference.checkClassApplicationKinds
    :: KindInference.KindAssumptions
    -> Name
    -> [Type Int]
    -> Either (KindInference.KindInferenceError Int)
        [Maybe KindInference.GroundKind]) `seq`
  (Query.resultEvidence
    :: Query.QueryResult () ()
    -> Query.QueryEvidence) `seq`
  (Query.resultSearch
    :: Query.QueryResult () ()
    -> Search.SearchBatch () ()) `seq`
  (Query.requestContextVariablesNotInScope
    :: Query.QueryRequest (Type Int) ()
    -> [Int]) `seq`
  (Rigid.rigidInstantiations
    :: Rigid.RigidInstantiationPlan
    -> [(CoreTypes.TVarId, CoreTypes.TVarId)]) `seq`
  (TypeSynonym.preparedInventory
    :: TypeSynonym.PreparedInventory Int ()
    -> Inventory.Inventory Int ()) `seq`
  (TypeSynonym.preparedTypeSynonyms
    :: TypeSynonym.PreparedInventory Int ()
    -> TypeSynonym.TypeSynonyms Int) `seq`
  (TypeSynonym.checkPreparedTypeSynonymSaturation
    :: TypeSynonym.PreparedInventory Int ()
    -> Type Int
    -> Either (TypeSynonym.SynonymExpansionError Int) ()) `seq`
  (TypeSynonym.checkPreparedTypeSynonymApplicationSaturation
    :: TypeSynonym.PreparedInventory Int ()
    -> Name
    -> Natural
    -> Either (TypeSynonym.SynonymExpansionError Int) ()) `seq`
  (TypeSynonym.elaboratePreparedTypes
    :: TypeSynonym.FreshVariable Int
    -> TypeSynonym.PreparedInventory Int ()
    -> [(KindInference.GroundKind, Type Int)]
    -> Either (TypeSynonym.TypeElaborationError Int) [Type Int]) `seq`
  (TypeSynonym.elaboratePreparedType
    :: TypeSynonym.FreshVariable Int
    -> TypeSynonym.PreparedInventory Int ()
    -> KindInference.GroundKind
    -> Type Int
    -> Either (TypeSynonym.TypeElaborationError Int) (Type Int)) `seq`
  (TypeSynonym.normalizePreparedTypeSynonyms
    :: TypeSynonym.FreshVariable Int
    -> TypeSynonym.PreparedInventory Int ()
    -> Type Int
    -> Either (TypeSynonym.SynonymExpansionError Int) (Type Int)) `seq`
  (TypeSynonym.inventoryExpansionPreparedInventory
    :: TypeSynonym.PreparedInventoryExpansion Int ()
    -> TypeSynonym.PreparedInventory Int ()) `seq`
  (TypeSynonym.inventoryExpansionDeclarations
    :: TypeSynonym.PreparedInventoryExpansion Int ()
    -> [SynthesisDeclaration.Declaration Int Void ()]) `seq`
  (TypeSynonym.inventoryExpansionRecursiveDataTypeNames
    :: TypeSynonym.PreparedInventoryExpansion Int ()
    -> Set.Set Name) `seq`
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
  (CoreTypes.showHsTypeWithQualification
    :: Qualification
    -> CoreTypes.TypeVarIndex
    -> CoreTypes.HsType
    -> String) `seq`
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
  (Length.lengthTypeNodeLimit
    :: Length.LengthLimits -> Int) `seq`
  (Length.lengthContractInputLimit
    :: Length.LengthLimits -> Int) `seq`
  (Length.lengthSyntaxNodeLimit
    :: Length.LengthLimits -> Int) `seq`
  (Length.lengthFormulaClauseLimit
    :: Length.LengthLimits -> Int) `seq`
  (Length.lengthCollectionWidthLimit
    :: Length.LengthLimits -> Int) `seq`
  (Length.lengthProviderSummaryLimit
    :: Length.LengthLimits -> Int) `seq`
  (Length.lengthProviderArgumentLimit
    :: Length.LengthLimits -> Int) `seq`
  (Length.lengthLiteralBitLimit
    :: Length.LengthLimits -> Int) `seq`
  (Length.lengthFingerprintByteLimit
    :: Length.LengthLimits -> Int) `seq`
  (Length.checkedLengthContractTarget
    :: Length.CheckedLengthContract Int -> Type Int) `seq`
  (Length.checkedLengthContractInputCount
    :: Length.CheckedLengthContract Int -> Int) `seq`
  (Length.checkedLengthContractPrecondition
    :: Length.CheckedLengthContract Int
    -> Length.LengthFormula Length.LengthContractVariable) `seq`
  (Length.checkedLengthContractPostcondition
    :: Length.CheckedLengthContract Int
    -> Length.LengthFormula Length.LengthContractVariable) `seq`
  (Length.lengthContractFingerprint
    :: Length.CheckedLengthContract Int
    -> Fingerprint.Fingerprint Length.LengthContractFingerprintSubject) `seq`
  (Length.checkedLengthProviderName
    :: Length.CheckedLengthProviderSummary Int -> Name) `seq`
  (TypedGeneratedFingerprint.fingerprintSharedTermGraph
    :: TypedGenerated.TermGraphLimits
    -> Natural
    -> TypedGenerated.TermGraph (Type (Variable Int)) Int
    -> Either
        (TypedGeneratedFingerprint.TermGraphFingerprintError Int Int)
        (Fingerprint.Fingerprint
          TypedGeneratedFingerprint.TermGraphFingerprintSubject)) `seq`
  (Length.checkedLengthProviderScheme
    :: Length.CheckedLengthProviderSummary Int -> Type Int) `seq`
  (Length.checkedLengthProviderArgumentRoles
    :: Length.CheckedLengthProviderSummary Int
    -> [Length.LengthProviderArgumentRole]) `seq`
  (Length.checkedLengthProviderTransfer
    :: Length.CheckedLengthProviderSummary Int
    -> Length.LengthExpression Length.LengthProviderVariable) `seq`
  (Length.checkedLengthProviderTrust
    :: Length.CheckedLengthProviderSummary Int
    -> Length.LengthProviderTrust) `seq`
  (Length.checkedLengthProviderSummaries
    :: Length.CheckedLengthProviderInventory Int
    -> [Length.CheckedLengthProviderSummary Int]) `seq`
  (Length.lengthProviderInventoryFingerprint
    :: Length.CheckedLengthProviderInventory Int
    -> Fingerprint.Fingerprint
        Length.LengthProviderInventoryFingerprintSubject) `seq`
  ()

main :: IO ()
main = defaultMain $ testGroup "Djex downstream API"
  [ facadeTests
  , testCase "the shared frontends expose non-terminating launchers" $ do
      let runCli :: [String] -> IO ExitCode
          runCli = runArguments
      replInitialBackend defaultReplOptions @?= OneBackend DjinnBackend
      runCli ["--version"] `seq` pure ()
      runRepl defaultReplOptions `seq` pure ()
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
  , testCase "opaque witnesses expose no representation constructors" $ do
      projectionSignatures `seq` pure ()
      forM_ forbiddenConstructionAttempts (\(description, attempt) -> do
        result <- try $ evaluate attempt
        case result :: Either SomeException () of
          Left exception -> do
            let message = displayException exception
                isMissingDictionary =
                  ( "No instance for" `isInfixOf` message &&
                    ( "Generic" `isInfixOf` message ||
                      "HasField" `isInfixOf` message
                    )
                  ) ||
                  ( "Couldn't match type" `isInfixOf` message &&
                    any (`isInfixOf` message)
                      [ "forbiddenFingerprintCoercion"
                      , "forbiddenTypedCandidateCoercion"
                      , "forbiddenBehavioralProblemCoercion"
                      , "forbiddenBehavioralFingerprintRoleCoercion"
                      , "forbiddenAssociatedObservationDomainCoercion"
                      , "forbiddenAssociatedObservationPayloadCoercion"
                      , "forbiddenBehavioralEvidenceDomainCoercion"
                      , "forbiddenBehavioralEvidenceReceiptCoercion"
                      , "forbiddenBoundedRawArtifactCoercion"
                      , "forbiddenCheckedLengthContractCoercion"
                      , "forbiddenCheckedLengthContextVariableCoercion"
                      , "forbiddenCheckedLengthContextAnnotationCoercion"
                      , "forbiddenCheckedLengthSpineModelCoercion"
                      , "forbiddenCheckedLengthProviderSummaryCoercion"
                      , "forbiddenCheckedLengthProviderInventoryCoercion"
                      ]
                  ) ||
                  ( "Variable not in scope" `isInfixOf` message &&
                    any (`isInfixOf` message)
                      [ "associatedObservation"
                      , "behavioralEvidenceReceipt"
                      ]
                  )
            assertBool
              (description ++ " raised an unrelated exception: " ++ message)
              isMissingDictionary
          Right () -> assertFailure description)
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
  , testCase "stable requests preserve exact types and reject rigid binders" $ do
      targetName <- expectRight $ mkIdentifier "tupled"
      target <- expectRight $ mkDefinitionName targetName
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
      exferenceRequestQuery checked @?= request application
      let canonical = CoreTypes.TypeTuple Boxed [first, second]
      canonicalChecked <- expectRight $ mkExferenceRequest $ request canonical
      assertBool
        "differently spelled exact Exference requests compared equal"
        $ checked /= canonicalChecked
      show checked @?= show (request application)
      show canonicalChecked @?= show (request canonical)
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
