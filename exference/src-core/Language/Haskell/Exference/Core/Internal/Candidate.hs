{-# LANGUAGE DeriveGeneric #-}

module Language.Haskell.Exference.Core.Internal.Candidate
  ( ExferenceCandidateDetails (..)
  , ExferenceCandidateError (..)
  , ExferenceTypeVariableHints
  , ExferenceGeneratedCandidate
  , mkExferenceGeneratedCandidate
  , projectValidatedCandidate
  , typeVariableHints
  ) where

import Control.DeepSeq (NFData, force)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

import qualified Language.Haskell.Exference.Core.Expression as Exference
import Language.Haskell.Exference.Core.ExferenceStats (ExferenceStats)
import Language.Haskell.Exference.Core.Types
  ( HsConstraint
  , HsType (TypeForall)
  , SynthesisType
  , SynthesisTypeError
  , SynthesisVariable
  , TVarId
  , TypeVarIndex
  , toSynthesisConstraint
  , toSynthesisConstraintStructure
  )
import Language.Haskell.Exference.Core.TypeUtils (forallify)
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
  case candidateHoles generated of
    firstHole : remainingHoles ->
      Left $ IncompleteCandidate $ firstHole :| remainingHoles
    [] -> pure ()
  sharedConstraints <- either (Left . InvalidCandidateType) Right
    $ traverse toSynthesisConstraint constraints
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
    (map (fmap SharedType.canonicalizeType . toSynthesisConstraintStructure)
      constraints)
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
      , exferenceLocalNameHints = Exference.expressionNameHints expression
      , exferenceTypeVariableHints = typeNames
      }

candidateHoles :: Generated.Expression local -> [local]
candidateHoles expression = case expression of
  Generated.Local{} -> []
  Generated.Global{} -> []
  Generated.Lambda _ body -> candidateHoles body
  Generated.Apply function argument ->
    candidateHoles function ++ candidateHoles argument
  Generated.Tuple elements -> concatMap candidateHoles elements
  Generated.Hole local -> [local]
  Generated.Let _ binding body ->
    candidateHoles binding ++ candidateHoles body
  Generated.Case scrutinee alternatives ->
    candidateHoles scrutinee
      ++ concatMap (candidateHoles . snd) alternatives

-- | Invert a frontend source-name index and propagate outer query-binder names
-- to the rigid constants introduced by Exference's initial forall step.  The
-- tagged shared keys keep flexible and rigid ID spaces distinct even when
-- their integers coincide.
typeVariableHints :: HsType -> TypeVarIndex -> ExferenceTypeVariableHints
typeVariableHints goal sourceNames = flexibleHints `Map.union` rigidHints
 where
  flexibleHints = foldr insertFlexible Map.empty $ Map.toAscList sourceNames
  insertFlexible (sourceName, variable) = Map.insert
    (SharedType.FlexibleVariable variable) sourceName

  rigidHints = Map.fromList
    [ (SharedType.RigidVariable rigid, sourceName)
    | (rigid, flexible) <- zip [0 ..] outerBinders
    , Just sourceName <- [Map.lookup
        (SharedType.FlexibleVariable flexible) flexibleHints]
    ]

  outerBinders = collectBinders $ forallify goal
  collectBinders (TypeForall variables _ body) =
    variables ++ collectBinders body
  collectBinders _ = []
