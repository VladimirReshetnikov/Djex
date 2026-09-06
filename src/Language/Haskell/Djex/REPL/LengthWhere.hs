-- | Conservative built-in-list defaults for Djex REPL Length constraints.
--
-- This package-private adapter receives an already elaborated source target
-- and its exact engine-owned inventory. It observes every structural list
-- argument, treats every other physical arrow argument as opaque, and accepts only a scalar
-- list result or a boxed pair of list results. The resulting session and
-- contract are checked by the existing Length boundaries; no solver or
-- candidate authority is created here.
module Language.Haskell.Djex.REPL.LengthWhere
  ( ReplLengthWhereResolution (..)
  , ReplLengthWhereResolutionError (..)
  , ReplLengthWhereCandidateAssessment (..)
  , ReplLengthWhereCandidateAssessmentFailure (..)
  , assessReplLengthWhereCandidate
  , replLengthWhereResolutionProfile
  , replLengthWhereResolutionObservedInputCount
  , closeReplDjinnLengthWhereSourceGoal
  , resolveReplLengthWhereSource
  ) where

import Control.Monad (when)
import Data.Bifunctor (first)

import Language.Haskell.Synthesis.Candidate (Candidate)
import Language.Haskell.Synthesis.Collection (observedListLength)
import Language.Haskell.Synthesis.Inventory (Inventory)
import Language.Haskell.Synthesis.Name (Boxity (Boxed), listName)
import Language.Haskell.Synthesis.Semantic.Length
  ( CheckedLengthContract
  , CheckedLengthSpinePairContract
  , LengthContractError
  , LengthSpineModelSource (BuiltinListSpine)
  , LengthSpinePairContractError
  , LengthTargetArgumentRole (..)
  , checkedLengthContractInputCount
  , checkedLengthSpinePairContractInputCount
  , defaultLengthLimits
  , lengthContractInputLimit
  )
import Language.Haskell.Synthesis.Semantic.Length.Problem
  ( CheckedLengthSession
  , LengthProblemError
  , LengthSpinePairProblemError
  , LengthSessionError
  , defaultLengthProblemLimits
  , sealExactSpineCaseLengthSession
  , sealLengthContractInSession
  , sealLengthSpinePairTypedCandidateProblemInSession
  , sealLengthSpinePairContractInSession
  , sealLengthTypedCandidateProblemInSession
  )
import Language.Haskell.Synthesis.Semantic.Length.Evaluate
  ( defaultLengthEvaluationLimits )
import Language.Haskell.Synthesis.Semantic.Length.SMTLib
  ( LengthSMTLibQueryError
  , LengthSpinePairSMTLibQueryError
  , defaultLengthSMTLibLimits
  , sealLengthSMTLibQuery
  , sealLengthSpinePairSMTLibQuery
  )
import Language.Haskell.Synthesis.Semantic.Length.SMTLib.Live
  ( LengthSMTLibLiveObservationReplayError
  , LengthSMTLibLiveQueryError
  , LengthSMTLibLiveSession
  , LengthSpinePairSMTLibLiveObservationReplayError
  , LengthSpinePairSMTLibLiveQueryError
  , replayLengthSMTLibLiveQueryObservation
  , replayLengthSpinePairSMTLibLiveQueryObservation
  , runLengthSMTLibLiveQuery
  , runLengthSpinePairSMTLibLiveQuery
  )
import Language.Haskell.Synthesis.Semantic.Length.Where
  ( LengthWhereContractSource (..)
  , LengthWhereDomain (..)
  , LengthWhereElaborationError
  , LengthWhereSource
  , elaborateLengthWhereSource
  )
import Language.Haskell.Synthesis.Type
  ( Type (..)
  , Variable
  , applicationSpine
  , functionSpine
  , quantifyFreeVariables
  , splitLeadingForalls
  )
import Language.Haskell.Synthesis.TypedCandidate (TypedCandidate)

-- | Fully checked scalar or binary-product profile ready for candidate use.
-- Constructors are package-private because the enclosing module is not part
-- of the public facade. The nominal checked values remain distinct.
data ReplLengthWhereResolution identity
  = ReplLengthWhereScalarResolution
      (CheckedLengthSession identity ())
      (CheckedLengthContract (Variable identity))
  | ReplLengthWhereBinaryProductResolution
      (CheckedLengthSession identity ())
      (CheckedLengthSpinePairContract (Variable identity))

-- | Closed target/profile failures. No constructor retains clause source.
data ReplLengthWhereResolutionError identity
  = ReplLengthWherePhysicalArgumentLimitExceeded !Int !Int
  | ReplLengthWhereUnsupportedResult
  | ReplLengthWhereElaborationRejected !LengthWhereElaborationError
  | ReplLengthWhereSessionRejected !(LengthSessionError identity)
  | ReplLengthWhereScalarContractRejected
      !(LengthContractError (Variable identity))
  | ReplLengthWhereBinaryProductContractRejected
      !(LengthSpinePairContractError (Variable identity))
  deriving (Eq, Show)

-- | Candidate-specific failures retain the candidate instead of granting a
-- false refutation. Every payload comes from an existing closed, bounded
-- Length boundary; no constructor retains the user's clause or solver bytes.
data ReplLengthWhereCandidateAssessmentFailure failure identity local
  = ReplLengthWhereScalarProblemRejected
      !(LengthProblemError failure identity local)
  | ReplLengthWhereBinaryProductProblemRejected
      !(LengthSpinePairProblemError failure identity local)
  | ReplLengthWhereScalarQueryRejected !LengthSMTLibQueryError
  | ReplLengthWhereBinaryProductQueryRejected
      !LengthSpinePairSMTLibQueryError
  | ReplLengthWhereScalarLiveQueryRejected !LengthSMTLibLiveQueryError
  | ReplLengthWhereBinaryProductLiveQueryRejected
      !LengthSpinePairSMTLibLiveQueryError
  | ReplLengthWhereScalarObservationRejected
      !LengthSMTLibLiveObservationReplayError
  | ReplLengthWhereBinaryProductObservationRejected
      !LengthSpinePairSMTLibLiveObservationReplayError
  deriving (Eq, Show)

-- | Result of checking one exact typed candidate against the resolved clause.
data ReplLengthWhereCandidateAssessment failure identity local
  = ReplLengthWhereCandidateRetained
  | ReplLengthWhereCandidateRefuted
  | ReplLengthWhereCandidateUnassessed
      !(ReplLengthWhereCandidateAssessmentFailure failure identity local)
  deriving (Eq, Show)

-- | Run the existing problem, query, live-observation, and independent replay
-- gates for one exact typed candidate. Identity domains are never converted:
-- the session, target contract, and graph must agree statically. Only a freshly
-- replayed counterexample refutes; every failure and every evidence-free status keeps
-- the candidate.
assessReplLengthWhereCandidate
  :: (Ord identity, Ord local)
  => ReplLengthWhereResolution identity
  -> LengthSMTLibLiveSession epoch
  -> TypedCandidate failure (Type (Variable identity)) local
      (Candidate (Type (Variable identity)) details output)
  -> IO (ReplLengthWhereCandidateAssessment failure identity local)
assessReplLengthWhereCandidate resolution liveSession candidate =
  case resolution of
    ReplLengthWhereScalarResolution session contract -> assessWith
      (first ReplLengthWhereScalarProblemRejected
        $ sealLengthTypedCandidateProblemInSession
            defaultLengthProblemLimits session contract candidate)
      (first ReplLengthWhereScalarQueryRejected
        . sealLengthSMTLibQuery defaultLengthSMTLibLimits)
      (fmap (first ReplLengthWhereScalarLiveQueryRejected)
        . runLengthSMTLibLiveQuery defaultLengthEvaluationLimits liveSession)
      (\query -> first ReplLengthWhereScalarObservationRejected
        . replayLengthSMTLibLiveQueryObservation query)
    ReplLengthWhereBinaryProductResolution session contract -> assessWith
      (first ReplLengthWhereBinaryProductProblemRejected
        $ sealLengthSpinePairTypedCandidateProblemInSession
            defaultLengthProblemLimits session contract candidate)
      (first ReplLengthWhereBinaryProductQueryRejected
        . sealLengthSpinePairSMTLibQuery defaultLengthSMTLibLimits)
      (fmap (first ReplLengthWhereBinaryProductLiveQueryRejected)
        . runLengthSpinePairSMTLibLiveQuery
            defaultLengthEvaluationLimits liveSession)
      (\query -> first ReplLengthWhereBinaryProductObservationRejected
        . replayLengthSpinePairSMTLibLiveQueryObservation query)

-- | The one domain-neutral gate order: sealed problem, sealed query, live
-- observation, then independent replay.  The stages arrive already wrapped
-- into the closed failure vocabulary, so this pipeline is the sole authority
-- for the retention rule: only a freshly replayed counterexample refutes,
-- and every failure keeps the candidate unassessed.
assessWith
  :: Either (ReplLengthWhereCandidateAssessmentFailure failure identity local) problem
  -> (problem
      -> Either (ReplLengthWhereCandidateAssessmentFailure failure identity local) query)
  -> (query
      -> IO (Either (ReplLengthWhereCandidateAssessmentFailure failure identity local) observation))
  -> (query
      -> observation
      -> Either (ReplLengthWhereCandidateAssessmentFailure failure identity local)
           (Maybe counterexample))
  -> IO (ReplLengthWhereCandidateAssessment failure identity local)
assessWith sealedProblem sealQuery runLive replay =
  case sealedProblem >>= sealQuery of
    Left failure -> pure $ ReplLengthWhereCandidateUnassessed failure
    Right query -> do
      observed <- runLive query
      pure $ case observed >>= replay query of
        Left failure -> ReplLengthWhereCandidateUnassessed failure
        Right Nothing -> ReplLengthWhereCandidateRetained
        Right (Just _) -> ReplLengthWhereCandidateRefuted

-- | Stable diagnostic name of the selected built-in profile.
replLengthWhereResolutionProfile :: ReplLengthWhereResolution identity -> String
replLengthWhereResolutionProfile resolution = case resolution of
  ReplLengthWhereScalarResolution{} -> "list-scalar-exact-cases"
  ReplLengthWhereBinaryProductResolution{} ->
    "list-binary-product-exact-cases"

-- | Number of structural list arguments observed in physical source order.
replLengthWhereResolutionObservedInputCount
  :: ReplLengthWhereResolution identity
  -> Int
replLengthWhereResolutionObservedInputCount resolution = case resolution of
  ReplLengthWhereScalarResolution _ contract ->
    checkedLengthContractInputCount contract
  ReplLengthWhereBinaryProductResolution _ contract ->
    checkedLengthSpinePairContractInputCount contract

-- | Make Haskell's implicit source quantification explicit before building
-- either a Djinn request or its Length contract. This operates on the original
-- source identities, before they are tagged for the consumer; it never opens
-- or relabels a candidate graph. Existing explicit binders and nested scopes
-- are preserved by the shared capture-safe free-variable traversal.
closeReplDjinnLengthWhereSourceGoal :: Type String -> Type String
closeReplDjinnLengthWhereSourceGoal = quantifyFreeVariables $ const True

-- | Infer, elaborate, and seal the one conservative built-in profile.
resolveReplLengthWhereSource
  :: Ord identity
  => Inventory (Variable identity) ()
  -> Type (Variable identity)
  -> LengthWhereSource
  -> Either (ReplLengthWhereResolutionError identity) (ReplLengthWhereResolution identity)
resolveReplLengthWhereSource inventory target source = do
  let (_, _, body) = splitLeadingForalls target
      (parameters, result) = functionSpine body
      maximumArguments = lengthContractInputLimit defaultLengthLimits
      observedArguments = observedListLength maximumArguments parameters
  when (observedArguments > maximumArguments)
    $ Left $ ReplLengthWherePhysicalArgumentLimitExceeded
      maximumArguments observedArguments
  let roles = map argumentRole parameters
  domain <- resultDomain result
  session <- first ReplLengthWhereSessionRejected
    $ sealExactSpineCaseLengthSession
      defaultLengthLimits roles inventory BuiltinListSpine []
  contractSource <- first ReplLengthWhereElaborationRejected
    $ elaborateLengthWhereSource domain roles source
  case contractSource of
    LengthWhereScalarContractSource _ contract ->
      ReplLengthWhereScalarResolution session
        <$> first ReplLengthWhereScalarContractRejected
          (sealLengthContractInSession session target contract)
    LengthWhereBinaryProductContractSource _ contract ->
      ReplLengthWhereBinaryProductResolution session
        <$> first ReplLengthWhereBinaryProductContractRejected
          (sealLengthSpinePairContractInSession session target contract)

argumentRole :: Type variable -> LengthTargetArgumentRole
argumentRole argument
  | isBuiltinList argument = LengthObservedSpine
  | otherwise = LengthUnobservedTarget

resultDomain
  :: Type variable
  -> Either (ReplLengthWhereResolutionError identity) LengthWhereDomain
resultDomain result
  | isBuiltinList result = Right LengthWhereScalar
resultDomain (TupleType Boxed [left, right])
  | isBuiltinList left
  , isBuiltinList right = Right LengthWhereBinaryProduct
resultDomain _ = Left ReplLengthWhereUnsupportedResult

isBuiltinList :: Type variable -> Bool
isBuiltinList source = case applicationSpine source of
  (TypeConstructor constructor, [_]) -> constructor == listName
  _ -> False
