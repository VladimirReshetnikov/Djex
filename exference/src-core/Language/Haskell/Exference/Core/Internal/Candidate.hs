{-# LANGUAGE DeriveGeneric #-}

module Language.Haskell.Exference.Core.Internal.Candidate
  ( ExferenceCandidateDetails (..)
  , ExferenceCandidateError (..)
  , ExferenceTypeVariableHints
  , ExferenceGeneratedCandidate
  , mkExferenceGeneratedCandidate
  , projectValidatedCandidate
  , typeVariableHints
  , typeVariableHintsWithPlan
  ) where

import Control.DeepSeq (NFData, force)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

import qualified Language.Haskell.Exference.Core.Expression as Exference
import Language.Haskell.Exference.Core.ExferenceStats (ExferenceStats (..))
import Language.Haskell.Exference.Core.FunctionBinding (EnvDictionary (..))
import Language.Haskell.Exference.Core.RigidInstantiation
  ( RigidInstantiationPlan
  , mkRigidInstantiationContext
  , planRigidInstantiation
  , rigidInstantiations
  )
import Language.Haskell.Exference.Core.Types
  ( HsConstraint
  , HsType
  , SynthesisType
  , SynthesisTypeError
  , SynthesisVariable
  , TVarId
  , TypeVarIndex
  , emptyStaticClassEnv
  , toSynthesisConstraint
  )
import Language.Haskell.Exference.Core.Score
  (Penalty, isFiniteScore, normalizePenalty)
import Language.Haskell.Synthesis.Candidate (Candidate (..))
import Language.Haskell.Synthesis.Constraint (Constraint)
import qualified Language.Haskell.Synthesis.Generated as Generated
import qualified Language.Haskell.Synthesis.Type as SharedType

type ExferenceTypeVariableHints = Map.Map SynthesisVariable String

-- | Backend details needed to inspect or faithfully render a shared result.
--
-- Expression annotations provide good term-binder names, while a frontend's
-- type-variable index provides source spellings for residual constraints.
-- Both are hints only; shared renderers still validate and freshen names.
data ExferenceCandidateDetails = ExferenceCandidateDetails
  { exferenceCandidateStats :: ExferenceStats
  , exferenceLocalNameHints :: Map.Map TVarId String
  , exferenceTypeVariableHints :: ExferenceTypeVariableHints
  }
  deriving (Eq, Show, Generic)

instance NFData ExferenceCandidateDetails

data ExferenceCandidateError
  = InvalidCandidateSyntax Generated.RenderError
  | InvalidCandidateScope (Generated.ScopeError TVarId)
  | IncompleteCandidate (NonEmpty TVarId)
  | InvalidCandidateType SynthesisTypeError
  | InvalidCandidateSteps Int
  | InvalidCandidateFinalQueueSize Int
  | InvalidCandidateComplexity Penalty
  deriving (Eq, Show, Generic)

instance NFData ExferenceCandidateError

type ExferenceGeneratedCandidate =
  Candidate SynthesisType ExferenceCandidateDetails
    (Generated.Expression TVarId)

-- | Check and erase one typed Exference result without losing residual
-- obligations, search statistics, or renderer hints.
mkExferenceGeneratedCandidate
  :: ExferenceTypeVariableHints
  -> Exference.Expression
  -> [HsConstraint]
  -> ExferenceStats
  -> Either ExferenceCandidateError ExferenceGeneratedCandidate
mkExferenceGeneratedCandidate typeNames expression constraints statistics = do
  let generated = Exference.toGeneratedExpression expression
  either (Left . InvalidCandidateSyntax) Right
    $ Generated.validateExpressionSyntax generated
  either (Left . InvalidCandidateScope) Right
    $ Generated.validateExpressionScope generated
  case Generated.expressionHoles generated of
    firstHole : remainingHoles ->
      Left $ IncompleteCandidate $ firstHole :| remainingHoles
    [] -> pure ()
  sharedConstraints <- either (Left . InvalidCandidateType) Right
    $ traverse toSynthesisConstraint constraints
  case exference_steps statistics of
    steps
      | steps < 0 -> Left $ InvalidCandidateSteps steps
      | otherwise -> pure ()
  case exference_finalSize statistics of
    finalSize
      | finalSize < 0 -> Left $ InvalidCandidateFinalQueueSize finalSize
      | otherwise -> pure ()
  case exference_complexityRating statistics of
    complexity
      | isFiniteScore complexity -> pure ()
      | otherwise -> Left $ InvalidCandidateComplexity complexity
  pure $ detachCandidate
    typeNames expression generated sharedConstraints statistics

-- The engine calls this only after input, typing, scope, completeness, and
-- generated-syntax checks.  Keeping it in an Internal module makes that
-- precondition unavailable as an unchecked public escape hatch.
projectValidatedCandidate
  :: ExferenceTypeVariableHints
  -> Exference.Expression
  -> [HsConstraint]
  -> ExferenceStats
  -> ExferenceGeneratedCandidate
projectValidatedCandidate typeNames expression constraints statistics =
  detachCandidate
    typeNames
    expression
    (Exference.toGeneratedExpression expression)
    constraints
    statistics

detachCandidate
  :: ExferenceTypeVariableHints
  -> Exference.Expression
  -> Generated.Expression TVarId
  -> [Constraint SynthesisType]
  -> ExferenceStats
  -> ExferenceGeneratedCandidate
detachCandidate typeNames expression generated constraints statistics = force
  $ Candidate generated constraints ExferenceCandidateDetails
      { exferenceCandidateStats = statistics
          { exference_complexityRating = normalizePenalty
              $ exference_complexityRating statistics
          }
      , exferenceLocalNameHints = Exference.expressionNameHints expression
      , exferenceTypeVariableHints = typeNames
      }

-- | Environment-free compatibility helper.  Core and Djex session searches
-- use the checked environment-aware helper exported from @Core@, because an
-- existing rigid variable in an environment changes the allocation plan.
--
-- If the finite identifier space is exhausted there can be no valid search;
-- retaining only flexible source hints keeps this convenience function total.
typeVariableHints :: HsType -> TypeVarIndex -> ExferenceTypeVariableHints
typeVariableHints goal sourceNames = either
  (const $ flexibleTypeVariableHints sourceNames)
  (`typeVariableHintsWithPlan` sourceNames)
  $ planRigidInstantiation
      (mkRigidInstantiationContext
        $ EnvDictionary [] [] emptyStaticClassEnv) [] goal

-- | Apply source spellings to the exact rigid-instantiation plan consumed by
-- search and independent checking.
typeVariableHintsWithPlan
  :: RigidInstantiationPlan
  -> TypeVarIndex
  -> ExferenceTypeVariableHints
typeVariableHintsWithPlan plan sourceNames =
  flexibleHints `Map.union` rigidHints
 where
  flexibleHints = flexibleTypeVariableHints sourceNames

  rigidHints = Map.fromList
    [ (SharedType.RigidVariable rigid, sourceName)
    | (flexible, rigid) <- rigidInstantiations plan
    , Just sourceName <- [Map.lookup
        (SharedType.FlexibleVariable flexible) flexibleHints]
    ]

flexibleTypeVariableHints
  :: TypeVarIndex
  -> ExferenceTypeVariableHints
flexibleTypeVariableHints = foldr insertFlexible Map.empty . Map.toAscList
 where
  insertFlexible (sourceName, variable) = Map.insert
    (SharedType.FlexibleVariable variable) sourceName
