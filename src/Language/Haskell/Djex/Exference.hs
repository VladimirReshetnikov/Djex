-- | Checked Exference sessions behind Djex's shared query envelope.
--
-- Session construction seals the source inventory and computes Exference's
-- supported search projection once. Query execution performs every fallible
-- validation before returning a lazy sequence of total shared result batches.
module Language.Haskell.Djex.Exference
  ( ExferenceSession
  , ExferenceOptions (..)
  , defaultExferenceOptions
  , ExferenceOmission (..)
  , ExferenceOmissionCapability (..)
  , ExferenceOmissionReason (..)
  , ExferenceRequest
  , ExferenceCandidate
  , ExferenceResult
  , mkExferenceSession
  , exferenceSessionInventory
  , exferenceSessionOmissions
  , exferenceSessionDiagnostics
  , mkExferenceRequest
  , exferenceRequestQuery
  , parseExferenceRequest
  , runExferenceQuery
  ) where

import Control.Monad.Trans.Except (runExceptT)
import Data.Foldable (toList)
import Data.Functor.Identity (runIdentity)
import Data.List (partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set

import Language.Haskell.Exference.Core
  ( ExferenceBatchMetadata
  , ExferenceCandidateDetails
  , ExferenceGeneratedSearchBatch
  , ExferenceHeuristicsConfig
  , ExferenceInput (..)
  , ExferenceInputError (..)
  , Penalty
  , findGeneratedSearchBatchesWithHintsEither
  , typeVariableHints
  , validateExferenceInput
  )
import Language.Haskell.Exference.Core.Declaration (SynthesisInventory)
import Language.Haskell.Exference.Core.FunctionBinding
  ( ConstructorBinding (..)
  , DeconstructorBinding (..)
  , FunctionBinding (..)
  )
import Language.Haskell.Exference.Core.Types
  ( HsType (..)
  , SynthesisType
  , TVarId
  , TypeVarIndex
  , constraint_params
  , fromSynthesisType
  , sClassEnv_tclasses
  , toSynthesisType
  , toSynthesisName
  )
import Language.Haskell.Exference.EnvironmentParser
  ( CheckedSourceEnvironment
  , SourceEnvironment (..)
  , checkedSourceInventory
  , checkedSourceProjection
  , haskellSrcExtsParseMode
  , sourceTypeSynonymMap
  )
import Language.Haskell.Exference.SimpleDict (defaultHeuristicsConfig)
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc
  ( TypeDeclMap
  , parseTypeWithKinds
  )
import Language.Haskell.Synthesis.Candidate (Candidate)
import Language.Haskell.Synthesis.Constraint
  ( constraintArguments )
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error, Warning)
  , diagnostic
  , withCode
  , withContext
  )
import Language.Haskell.Synthesis.Generated
  ( FunctionClause (FunctionClause)
  , validateDefinitionName
  )
import Language.Haskell.Synthesis.Inventory
  ( inventoryKindAssumptions )
import Language.Haskell.Synthesis.Kind (Kind (ProperTypeKind))
import Language.Haskell.Synthesis.KindInference (checkTypesKinds)
import Language.Haskell.Synthesis.Name (Name, renderCanonical)
import Language.Haskell.Synthesis.Query
  ( QueryEvidence (NoEvidence, ValidatedCandidates)
  , QueryRequest (..)
  , QueryResult (QueryResult)
  )
import Language.Haskell.Synthesis.Search (batchCandidates)
import qualified Language.Haskell.Synthesis.Type as SharedType

data ExferenceOptions = ExferenceOptions
  { exferenceAllowUnused :: Bool
  , exferenceAllowResidualConstraints :: Bool
  , exferenceConstraintDeferralSteps :: Int
  , exferenceMultiConstructorPatterns :: Bool
  , exferenceMaximumSteps :: Int
  , exferenceMaximumQueueSize :: Maybe Int
  , exferenceMaximumDepth :: Maybe Penalty
  , exferenceHeuristics :: ExferenceHeuristicsConfig
  }
  deriving (Eq, Show)

defaultExferenceOptions :: ExferenceOptions
defaultExferenceOptions = ExferenceOptions
  { exferenceAllowUnused = False
  , exferenceAllowResidualConstraints = False
  , exferenceConstraintDeferralSteps = 8192
  , exferenceMultiConstructorPatterns = False
  , exferenceMaximumSteps = 65536
  , exferenceMaximumQueueSize = Just 8192
  , exferenceMaximumDepth = Nothing
  , exferenceHeuristics = defaultHeuristicsConfig
  }

data ExferenceOmissionCapability
  = BindingIntroduction
  | DataElimination
  deriving (Eq, Ord, Show)

data ExferenceOmissionReason = UnsupportedNestedForall
  deriving (Eq, Ord, Show)

data ExferenceOmission = ExferenceOmission
  { omittedName :: Name
  , omittedCapability :: ExferenceOmissionCapability
  , omittedReason :: ExferenceOmissionReason
  }
  deriving (Eq, Ord, Show)

data ExferenceSession = ExferenceSession
  { sessionSource :: SourceEnvironment FunctionBinding
  , sessionInventory :: SynthesisInventory
  , sessionTypeDeclarations :: TypeDeclMap
  , sessionOmissions :: [ExferenceOmission]
  , sessionDiagnostics :: [Diagnostic]
  }

-- Keep the frontend spelling index private: it is meaningful only when paired
-- with the exact parsed goal and must be converted after explicit contexts are
-- merged, because that operation can change Exference's rigid-ID allocation.
data ExferenceRequest = ExferenceRequest
  { requestQuery :: QueryRequest SynthesisType ExferenceOptions
  , requestSourceTypeVariables :: TypeVarIndex
  }
  deriving (Eq, Show)

type ExferenceCandidate =
  Candidate SynthesisType ExferenceCandidateDetails (FunctionClause TVarId)

type ExferenceResult =
  QueryResult ExferenceBatchMetadata ExferenceCandidate

mkExferenceSession
  :: CheckedSourceEnvironment
  -> Either Diagnostic ExferenceSession
mkExferenceSession checked = do
  let source = checkedSourceProjection checked
      (supportedFunctions, omittedFunctions) = partition functionSupported
        $ sourceFunctions source
      (supportedDeconstructors, omittedDeconstructors) =
        partition deconstructorSupported $ sourceDeconstructors source
      supportedSource = source
        { sourceFunctions = supportedFunctions
        , sourceDeconstructors = supportedDeconstructors
        }
      omissions =
        [ ExferenceOmission
            (toSynthesisName $ functionName binding)
            BindingIntroduction
            UnsupportedNestedForall
        | binding <- omittedFunctions
        ] ++ mapMaybe deconstructorOmission omittedDeconstructors
      value = ExferenceSession
        { sessionSource = supportedSource
        , sessionInventory = checkedSourceInventory checked
        , sessionTypeDeclarations = sourceTypeSynonymMap source
        , sessionOmissions = omissions
        , sessionDiagnostics = map omissionDiagnostic omissions
        }
      probeOptions = defaultExferenceOptions {exferenceMaximumSteps = 1}
  case validateExferenceInput
      $ searchInput value Nothing (TypeVar 0) probeOptions of
    Left failure -> Left $ failureDiagnostic
      "DJEX_EXF_ENV"
      "cannot seal the Exference session environment"
      failure
    Right () -> Right value

exferenceSessionInventory :: ExferenceSession -> SynthesisInventory
exferenceSessionInventory = sessionInventory

exferenceSessionOmissions :: ExferenceSession -> [ExferenceOmission]
exferenceSessionOmissions = sessionOmissions

exferenceSessionDiagnostics :: ExferenceSession -> [Diagnostic]
exferenceSessionDiagnostics = sessionDiagnostics

mkExferenceRequest
  :: QueryRequest SynthesisType ExferenceOptions
  -> Either Diagnostic ExferenceRequest
mkExferenceRequest query = do
  validateRequest query
  pure $ ExferenceRequest query Map.empty

exferenceRequestQuery
  :: ExferenceRequest
  -> QueryRequest SynthesisType ExferenceOptions
exferenceRequestQuery = requestQuery

parseExferenceRequest
  :: ExferenceSession
  -> ExferenceOptions
  -> Name
  -> FilePath
  -> String
  -> Either Diagnostic ExferenceRequest
parseExferenceRequest session options target sourceName source = do
  validateTarget target
  let environment = sessionSource session
      parsed = runIdentity $ runExceptT $ parseTypeWithKinds
        (inventoryKindAssumptions $ sessionInventory session)
        (sClassEnv_tclasses $ sourceClasses environment)
        Nothing
        (sourceTypeNames environment)
        (sessionTypeDeclarations session)
        (haskellSrcExtsParseMode sourceName)
        source
  (backendType, sourceVariables) <- parsed
  sharedType <- either
    (Left . failureDiagnostic
      "DJEX_EXF_PARSE"
      "cannot project the parsed Exference type"
    )
    Right
    $ toSynthesisType backendType
  let query = QueryRequest
        { requestTarget = target
        , requestGoal = sharedType
        , requestContexts = []
        , requestOptions = options
        }
  validateRequest query
  pure $ ExferenceRequest query sourceVariables

runExferenceQuery
  :: ExferenceSession
  -> ExferenceRequest
  -> Either Diagnostic [ExferenceResult]
runExferenceQuery session request = do
  let query = requestQuery request
      target = requestTarget query
      sharedGoal = contextualGoal query
  validateRequest query
  either
    (Left . failureDiagnostic
      "DJEX_EXF_KIND"
      "Exference rejected the query kind"
    )
    Right
    $ checkTypesKinds
        (inventoryKindAssumptions $ sessionInventory session)
        [(ProperTypeKind, sharedGoal)]
  backendGoal <- either
    (Left . failureDiagnostic
      "DJEX_EXF_LOWER"
      "cannot lower the shared query to Exference"
    )
    Right
    $ fromSynthesisType sharedGoal
  let hints = typeVariableHints backendGoal
        $ requestSourceTypeVariables request
      input = searchInput session (Just target) backendGoal
        $ requestOptions query
  batches <- either
    (\failure -> Left $ failureDiagnostic
      (if optionFailure failure
        then "DJEX_EXF_OPTIONS"
        else "DJEX_EXF_QUERY")
      (if optionFailure failure
        then "invalid Exference search options"
        else "Exference rejected the query")
      failure
    )
    Right
    $ findGeneratedSearchBatchesWithHintsEither hints input
  pure $ map (resultBatch target) batches

validateRequest
  :: QueryRequest SynthesisType ExferenceOptions
  -> Either Diagnostic ()
validateRequest query = do
  validateTarget $ requestTarget query
  either
    (Left . failureDiagnostic
      "DJEX_EXF_REQUEST"
      "invalid shared Exference request"
    )
    Right
    $ SharedType.validateType $ contextualGoal query
  let goalVariables = Set.fromList $ toList $ requestGoal query
      contextVariables = Set.unions
        [ SharedType.freeVariables argument
        | constraint <- requestContexts query
        , argument <- constraintArguments constraint
        ]
      extraneous = Set.toAscList $ contextVariables Set.\\ goalVariables
  case extraneous of
    [] -> Right ()
    _ -> Left $ failureDiagnostic
      "DJEX_EXF_REQUEST"
      "explicit Exference contexts contain variables absent from the goal"
      extraneous

validateTarget :: Name -> Either Diagnostic ()
validateTarget target = case validateDefinitionName target of
  Right () -> Right ()
  Left failure -> Left $ failureDiagnostic
    "DJEX_EXF_TARGET"
    "Exference targets must be unqualified value identifiers or operators"
    (renderCanonical target, failure)

contextualGoal
  :: QueryRequest SynthesisType options
  -> SynthesisType
contextualGoal query
  | null contexts = requestGoal query
  | otherwise = insertUnderLeadingForalls $ requestGoal query
 where
  contexts = requestContexts query

  -- Explicit contexts are scoped by the complete leading quantifier chain.
  -- Attaching them to only the first forall makes a variable bound by a later
  -- leading forall free in the outer context, then binds the same identity a
  -- second time below it.
  insertUnderLeadingForalls (SharedType.ForallType variables embedded body)
    | SharedType.ForallType{} <- body = SharedType.ForallType
        variables embedded $ insertUnderLeadingForalls body
    | otherwise = SharedType.ForallType
        variables (contexts ++ embedded) body
  insertUnderLeadingForalls goal =
    SharedType.ForallType [] contexts goal

optionFailure :: ExferenceInputError -> Bool
optionFailure failure = case failure of
  InvalidMaxSteps{} -> True
  InvalidConstraintDeferralSteps{} -> True
  InvalidMaxQueueSize{} -> True
  InvalidMaxDepth{} -> True
  InvalidHeuristic{} -> True
  _ -> False

resultBatch
  :: Name
  -> ExferenceGeneratedSearchBatch
  -> ExferenceResult
resultBatch target batch = QueryResult evidence
  $ fmap (fmap $ FunctionClause target []) batch
 where
  evidence
    | null $ batchCandidates batch = NoEvidence
    | otherwise = ValidatedCandidates

searchInput
  :: ExferenceSession
  -> Maybe Name
  -> HsType
  -> ExferenceOptions
  -> ExferenceInput
searchInput session excludedTarget goal options = ExferenceInput
  { input_goalType = goal
  , input_envFuncs = filter targetIsAvailable $ sourceFunctions source
  , input_envDeconsS = sourceDeconstructors source
  , input_envClasses = sourceClasses source
  , input_allowUnused = exferenceAllowUnused options
  , input_allowConstraints = exferenceAllowResidualConstraints options
  , input_allowConstraintsStopStep = exferenceConstraintDeferralSteps options
  , input_multiPM = exferenceMultiConstructorPatterns options
  , input_maxSteps = exferenceMaximumSteps options
  , input_maxQueueSize = exferenceMaximumQueueSize options
  , input_maxDepth = exferenceMaximumDepth options
  , input_heuristicsConfig = exferenceHeuristics options
  }
 where
  source = sessionSource session
  -- An unqualified source binding equal to the generated definition would
  -- change meaning after wrapping the expression in a target-bearing clause:
  -- @target = target@ is recursion, not a reference to the old environment.
  -- Qualified homonyms remain distinct structural names and are safe.
  targetIsAvailable binding = case excludedTarget of
    Nothing -> True
    Just target -> toSynthesisName (functionName binding) /= target

functionSupported :: FunctionBinding -> Bool
functionSupported binding = all (not . containsForall)
  $ functionResult binding
  : functionParameters binding
  ++ concatMap constraint_params (functionConstraints binding)

deconstructorSupported :: DeconstructorBinding -> Bool
deconstructorSupported binding = all (not . containsForall)
  $ deconstructorInput binding
  : concatMap constructorFields (deconstructorConstructors binding)

containsForall :: HsType -> Bool
containsForall TypeForall{} = True
containsForall (TypeArrow parameter result) =
  containsForall parameter || containsForall result
containsForall (TypeApp function argument) =
  containsForall function || containsForall argument
containsForall _ = False

deconstructorOmission :: DeconstructorBinding -> Maybe ExferenceOmission
deconstructorOmission binding = do
  name <- typeHead $ deconstructorInput binding
  pure $ ExferenceOmission
    (toSynthesisName name)
    DataElimination
    UnsupportedNestedForall
 where
  typeHead (TypeForall _ _ body) = typeHead body
  typeHead (TypeApp function _) = typeHead function
  typeHead (TypeCons name) = Just name
  typeHead _ = Nothing

omissionDiagnostic :: ExferenceOmission -> Diagnostic
omissionDiagnostic omission = withContext
  (renderCanonical $ omittedName omission)
  $ withCode "DJEX_EXF_OMISSION"
  $ diagnostic Warning
      "Exference omitted a source capability containing a nested forall"

failureDiagnostic
  :: Show detail
  => String
  -> String
  -> detail
  -> Diagnostic
failureDiagnostic code message detail = withContext (show detail)
  $ withCode code
  $ diagnostic Error message
