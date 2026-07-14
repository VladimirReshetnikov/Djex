module Main (main) where

import Data.Either (isRight)
import Data.Void (Void)

import ExferencePatternImports (patternViewsRoundTrip)
import Language.Haskell.Djex
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

main :: IO ()
main = defaultMain $ testGroup "public Djex facade"
  [ testCase "enumerates both checked backends" $
      map backend availableBackends @?= [DjinnBackend, ExferenceBackend]
  , testCase "exports the shared name vocabulary" $
      assertBool "qualified name was rejected" $
        isRight $ parseName "Data.Function.fix"
  , testCase "exports shared duplicate classification" $
      multiplicityOf "value" (summarizeDuplicates ["value", "value"])
        @?= OccursMultipleTimes
  , testCase "exports generated-code rendering" $ do
      target <- expectRight $ mkIdentifier "identity"
      checkedTarget <- expectRight $ mkDefinitionName target
      definitionName checkedTarget @?= target
      definitionSpelling checkedTarget @?= "identity"
      renderFunctionClause (defaultRenderOptions id)
          (FunctionClause checkedTarget [Bind "value"] $ Local "value") @?=
        Right "identity value = value"
  , testCase "exports checked Exference options" $
      exferenceMaximumSteps defaultExferenceOptions @?= 65536
  , testCase "exports explicit Exference record-pattern views" $
      patternViewsRoundTrip @?= True
  , testCase "exports checked session entry points" $ do
      assertBool "the standard Djinn session did not seal" $
        isRight standardDjinnSession
      let djinnTypeProjection :: DjinnType -> Type DjinnTypeVariable
          djinnTypeProjection = id
          djinnRequestProjection
            :: DjinnRequest -> QueryRequest DjinnType QueryOptions
          djinnRequestProjection = djinnRequestQuery
          djinnCandidateProjection
            :: DjinnCandidate
            -> Candidate DjinnType DjinnCandidateDetails
                (FunctionClause DjinnLocal)
          djinnCandidateProjection = id
          inventoryProjection
            :: ExferenceSession -> ExferenceInventory
          inventoryProjection = exferenceSessionInventory
          environmentProjection
            :: ExferenceEnvironment
            -> Environment ExferenceTypeVariable Void ()
          environmentProjection = id
          requestProjection
            :: ExferenceRequest
            -> QueryRequest ExferenceType ExferenceOptions
          requestProjection = exferenceRequestQuery
          candidateProjection
            :: ExferenceCandidate -> ExferenceCandidateDetails
          candidateProjection = candidateDetails
          metadataProjection
            :: ExferenceResult -> ExferenceBatchMetadata
          metadataProjection = batchMetadata . resultSearch
      djinnTypeProjection `seq` djinnRequestProjection `seq`
        djinnCandidateProjection `seq` inventoryProjection `seq`
        environmentProjection `seq`
        requestProjection `seq` candidateProjection `seq`
        metadataProjection `seq` pure ()
      mkDjinnRequest `seq` mkExferenceSession `seq`
        mkExferenceSessionWithPolicy `seq` pure ()
  , testCase "seals Djinn from the neutral environment vocabulary" $ do
      let checkedEnvironment
            :: Either (EnvironmentError DjinnTypeVariable) DjinnEnvironment
          checkedEnvironment = mkEnvironment []
      environment <- expectRight checkedEnvironment
      session <- expectRight $ mkDjinnSession environment
      let inventory :: DjinnInventory
          inventory = djinnSessionInventory session
      environmentDeclarations (inventoryEnvironment inventory) @?= []
  , testCase "seals Exference from the neutral environment vocabulary" $ do
      let checkedEnvironment
            :: Either
                (EnvironmentError ExferenceTypeVariable)
                ExferenceEnvironment
          checkedEnvironment = mkEnvironment []
      environment <- expectRight checkedEnvironment
      session <- expectRight $ mkExferenceSession environment
      let inventory :: ExferenceInventory
          inventory = exferenceSessionInventory session
      environmentDeclarations (inventoryEnvironment inventory) @?= []
      let fresh :: FreshVariable ExferenceTypeVariable
          fresh _ _ = Nothing
      assertBool "the facade did not reexport synonym elaboration"
        $ isRight $ prepareTypeSynonyms fresh inventory
  ]

expectRight :: Show error => Either error value -> IO value
expectRight result = case result of
  Left failure -> fail $ show failure
  Right value -> pure value
