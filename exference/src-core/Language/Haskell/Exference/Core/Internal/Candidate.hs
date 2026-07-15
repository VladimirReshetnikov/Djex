{-# LANGUAGE DeriveGeneric #-}

module Language.Haskell.Exference.Core.Internal.Candidate
  ( ExferenceCandidateDetails (..)
  , ExferenceCandidateError (..)
  , ExferenceTypeVariableHints
  , ExferenceSourceTypeVariableHints
  , ExferenceSourceTypeVariableHintError (..)
  , ExferenceGeneratedCandidate
  , mkExferenceGeneratedCandidate
  , projectValidatedCandidate
  , emptyExferenceSourceTypeVariableHints
  , mkExferenceSourceTypeVariableHints
  , retargetExferenceSourceTypeVariableHints
  , validateExferenceTypeVariableSpelling
  , typeVariableHints
  , typeVariableHintsWithPlan
  , compatibilityTypeVariableHintsWithPlan
  ) where

import Control.DeepSeq (NFData (rnf), force)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

import qualified Language.Haskell.Exference.Core.Expression as Exference
import Language.Haskell.Exference.Core.ExferenceStats (ExferenceStats (..))
import Language.Haskell.Exference.Core.FunctionBinding (EnvDictionary (..))
import Language.Haskell.Exference.Core.Internal.FlexibleIds
  ( flexibleIdentifiers )
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
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Type as SharedType

type ExferenceTypeVariableHints = Map.Map SynthesisVariable String

-- | Checked source spellings paired with their canonical source goal.
--
-- The constructor stays private so search can trust that every retained
-- spelling is a non-wildcard Haskell variable identifier in the stated
-- source type. The retained goal prevents an opaque value checked for one
-- local integer namespace from being reused with another query. An
-- 'IntMap.IntMap' makes the spelling representation independent of aliases
-- and of the frontend map's spelling-oriented ordering.
data ExferenceSourceTypeVariableHints = ExferenceSourceTypeVariableHints
  !HsType
  !(IntMap.IntMap String)
  deriving (Eq, Show)

-- Do not derive 'Generic' for an abstract invariant witness: its public
-- 'Generic.to' method would reconstruct the hidden representation.
instance NFData ExferenceSourceTypeVariableHints where
  rnf (ExferenceSourceTypeVariableHints sourceType sourceNames) =
    rnf sourceType `seq` rnf sourceNames

-- | Why a frontend spelling cannot become a trusted rendering hint.
--
-- The spelling is retained beside lexical and scope errors so a diagnostic
-- need not inspect the rejected frontend map again.
data ExferenceSourceTypeVariableHintError
  = InvalidSourceTypeVariableSpelling String SharedName.NameError
  | WildcardSourceTypeVariableSpelling
  | SourceTypeVariableHintOutOfScope String TVarId
  | SourceTypeVariableHintGoalMismatch
      HsType -- ^ Canonical goal retained by the hint value.
      HsType -- ^ Canonical goal supplied to search.
  deriving (Eq, Show, Generic)

instance NFData ExferenceSourceTypeVariableHintError

-- | Bind an empty spelling set to one canonical source goal.
emptyExferenceSourceTypeVariableHints
  :: HsType
  -> ExferenceSourceTypeVariableHints
emptyExferenceSourceTypeVariableHints sourceType = force
  $ ExferenceSourceTypeVariableHints
      (SharedType.canonicalizeType sourceType)
      IntMap.empty

-- | Validate and detach a frontend's source-spelling index.
--
-- All aliases are checked before aliases for the same ID are collapsed. This
-- prevents a valid preferred alias from concealing an invalid entry.  The
-- lexicographically least valid alias wins deterministically.  Forcing the
-- result here makes the outer 'Right' a genuine sealing boundary: no source
-- map traversal, spelling validation, or source-goal normalization remains
-- suspended behind it.
mkExferenceSourceTypeVariableHints
  :: HsType
  -> TypeVarIndex
  -> Either ExferenceSourceTypeVariableHintError
       ExferenceSourceTypeVariableHints
mkExferenceSourceTypeVariableHints sourceType sourceNames = do
  validSpellings <- traverse validateSpelling $ Map.toAscList sourceNames
  inScopeSpellings <- traverse requireInScope validSpellings
  pure $! force $ ExferenceSourceTypeVariableHints canonicalSourceType
    $ IntMap.fromListWith min
      [ (variable, spelling)
      | (spelling, variable) <- inScopeSpellings
      ]
 where
  canonicalSourceType = SharedType.canonicalizeType sourceType
  sourceVariables = flexibleIdentifiers canonicalSourceType

  validateSpelling (lazySpelling, variable)
    | spelling == "_" = reject WildcardSourceTypeVariableSpelling
    | otherwise = case validateExferenceTypeVariableSpelling spelling of
        Left nameError -> reject
          $ InvalidSourceTypeVariableSpelling spelling nameError
        Right () -> Right (spelling, variable)
   where
    -- Validate the complete finite spelling before constructing either
    -- branch. Otherwise an invalid first character could leave a lazy tail
    -- hidden in a nominally checked diagnostic.
    spelling = force lazySpelling

  requireInScope entry@(spelling, variable)
    | IntSet.member variable sourceVariables = Right entry
    | otherwise = reject
        $ SourceTypeVariableHintOutOfScope spelling variable

  reject failure = Left $! force failure

-- | Validate a type-variable token in Exference's supported source grammar.
--
-- Shared names reserve the keywords that are unconditionally active for
-- generated Djex source, including @forall@. Exference additionally parses
-- with @TypeFamilies@, where @family@ is unavailable in a type-variable
-- position even though it remains a legal term identifier.
validateExferenceTypeVariableSpelling
  :: String
  -> Either SharedName.NameError ()
validateExferenceTypeVariableSpelling spelling
  | spelling == "family" = Left $ SharedName.ReservedIdentifier spelling
  | otherwise = do
      name <- SharedName.mkIdentifier spelling
      if SharedName.nameLexicalClass name == SharedName.VariableLike
        then Right ()
        else Left $ SharedName.InvalidIdentifier spelling

-- | Rebind checked hints to a type produced by origin-safe elaboration.
--
-- This operation is internal because an arbitrary target type cannot prove
-- provenance.  The stable adapter calls it only after shared synonym
-- expansion has alpha-freshened every introduced binder away from the
-- complete original source namespace.  Integer membership then denotes
-- actual survival rather than a coincidental reuse of an erased source ID.
retargetExferenceSourceTypeVariableHints
  :: HsType
  -> ExferenceSourceTypeVariableHints
  -> ExferenceSourceTypeVariableHints
retargetExferenceSourceTypeVariableHints sourceType
    (ExferenceSourceTypeVariableHints _ sourceNames) = force
  $ ExferenceSourceTypeVariableHints canonicalSourceType
  $ IntMap.filterWithKey
      (\variable _ -> IntSet.member variable sourceVariables)
      sourceNames
 where
  canonicalSourceType = SharedType.canonicalizeType sourceType
  sourceVariables = flexibleIdentifiers canonicalSourceType

-- | Backend details needed to inspect or faithfully render a shared result.
--
-- Expression annotations provide good term-binder names, while a frontend's
-- type-variable index provides source spellings for residual constraints.
-- Both are hints only. Canonical search supplies checked type-variable names;
-- the stable adapter also validates and deduplicates caller-built
-- compatibility values before rendering them.
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

-- | Environment-free compatibility helper.  Its environment-aware companion
-- can account for an existing rigid variable changing the allocation plan,
-- but both retain their historical unchecked spelling-map contract.
--
-- If the finite identifier space is exhausted there can be no valid search;
-- retaining only flexible source hints keeps this convenience function total.
-- New request and result boundaries must instead construct
-- 'ExferenceSourceTypeVariableHints'.
typeVariableHints :: HsType -> TypeVarIndex -> ExferenceTypeVariableHints
typeVariableHints goal sourceNames = either
  (const flexibleHints)
  (`compatibilityTypeVariableHintsWithPlan` sourceNames)
  rigidPlan
 where
  flexibleHints = compatibilityFlexibleTypeVariableHints sourceNames
  rigidPlan = planRigidInstantiation
    (mkRigidInstantiationContext
      $ EnvDictionary [] [] emptyStaticClassEnv) [] goal

-- | Apply source spellings to the exact rigid-instantiation plan consumed by
-- search and independent checking.  The opaque input guarantees that raw
-- frontend strings have crossed the lexical and scope boundary and that the
-- retained canonical goal is exactly the one used to prepare this plan.
typeVariableHintsWithPlan
  :: HsType
  -> RigidInstantiationPlan
  -> ExferenceSourceTypeVariableHints
  -> Either ExferenceSourceTypeVariableHintError
       ExferenceTypeVariableHints
typeVariableHintsWithPlan expectedGoal plan sourceNames
  | hintedGoal == canonicalExpectedGoal = Right
      $ extendTypeVariableHintsWithPlan plan
      $ checkedFlexibleTypeVariableHints sourceNames
  | otherwise = reject
      $ SourceTypeVariableHintGoalMismatch hintedGoal canonicalExpectedGoal
 where
  hintedGoal = sourceTypeVariableHintGoal sourceNames
  canonicalExpectedGoal = force $ SharedType.canonicalizeType expectedGoal
  reject failure = Left $! force failure

-- | Preserve the historical unchecked spelling-map API where a public
-- compatibility helper still promises it.  New request and result paths must
-- use 'typeVariableHintsWithPlan' instead.
compatibilityTypeVariableHintsWithPlan
  :: RigidInstantiationPlan
  -> TypeVarIndex
  -> ExferenceTypeVariableHints
compatibilityTypeVariableHintsWithPlan plan =
  extendTypeVariableHintsWithPlan plan
    . compatibilityFlexibleTypeVariableHints

extendTypeVariableHintsWithPlan
  :: RigidInstantiationPlan
  -> ExferenceTypeVariableHints
  -> ExferenceTypeVariableHints
extendTypeVariableHintsWithPlan plan flexibleHints =
  flexibleHints `Map.union` rigidHints
 where
  rigidHints = Map.fromList
    [ (SharedType.RigidVariable rigid, sourceName)
    | (flexible, rigid) <- rigidInstantiations plan
    , Just sourceName <- [Map.lookup
        (SharedType.FlexibleVariable flexible) flexibleHints]
    ]

checkedFlexibleTypeVariableHints
  :: ExferenceSourceTypeVariableHints
  -> ExferenceTypeVariableHints
checkedFlexibleTypeVariableHints
    (ExferenceSourceTypeVariableHints _ sourceNames) =
  Map.fromList
    [ (SharedType.FlexibleVariable variable, sourceName)
    | (variable, sourceName) <- IntMap.toAscList sourceNames
    ]

sourceTypeVariableHintGoal :: ExferenceSourceTypeVariableHints -> HsType
sourceTypeVariableHintGoal (ExferenceSourceTypeVariableHints sourceType _) =
  sourceType

compatibilityFlexibleTypeVariableHints
  :: TypeVarIndex
  -> ExferenceTypeVariableHints
compatibilityFlexibleTypeVariableHints =
  foldr insertFlexible Map.empty . Map.toAscList
 where
  insertFlexible (sourceName, variable) = Map.insert
    (SharedType.FlexibleVariable variable) sourceName
