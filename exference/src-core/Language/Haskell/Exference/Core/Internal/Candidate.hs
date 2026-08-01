{-# LANGUAGE DeriveGeneric #-}

module Language.Haskell.Exference.Core.Internal.Candidate
  ( ExferenceCandidateDetails (..)
  , ExferenceTypeVariableHints
  , ExferenceSourceTypeVariableNames
  , ExferenceSourceTypeVariableHints
  , ExferenceSourceTypeVariableHintError (..)
  , ExferenceCandidate
  , projectValidatedCandidate
  , emptyExferenceSourceTypeVariableHints
  , mkExferenceSourceTypeVariableNames
  , bindExferenceSourceTypeVariableHints
  , mkExferenceSourceTypeVariableHints
  , retargetExferenceSourceTypeVariableHints
  , sourceTypeVariableHintGoal
  , validateExferenceTypeVariableSpelling
  , typeVariableHintsWithPlan
  ) where

import Control.DeepSeq (NFData (rnf), force)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

import qualified Language.Haskell.Exference.Core.Expression as Exference
import Language.Haskell.Exference.Core.ExferenceStats (ExferenceStats (..))
import Language.Haskell.Exference.Core.Internal.FlexibleIds
  ( flexibleIdentifiers )
import Language.Haskell.Exference.Core.Internal.SourceNames
  ( preferredSourceTypeVariableNames )
import Language.Haskell.Exference.Core.RigidInstantiation
  ( RigidInstantiationPlan
  , rigidInstantiations
  )
import Language.Haskell.Exference.Core.Types
  ( HsConstraint
  , HsType
  , SynthesisVariable
  , TVarId
  , TypeVarIndex
  )
import Language.Haskell.Exference.Core.Score (normalizePenalty)
import Language.Haskell.Synthesis.Candidate (Candidate (..))
import qualified Language.Haskell.Synthesis.Generated as Generated
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Type as SharedType

type ExferenceTypeVariableHints = Map.Map SynthesisVariable String

-- | Detached, lexically checked frontend spellings in source-map order.
--
-- Request sealing owns this intermediate witness because a programmatic
-- context cannot be bound to a canonical contextual goal until a session has
-- supplied the owning classes' arities. The original spelling-keyed map is
-- not retained, and every lazy 'String' has been materialized.
newtype ExferenceSourceTypeVariableNames = ExferenceSourceTypeVariableNames
  [(String, TVarId)]

instance NFData ExferenceSourceTypeVariableNames where
  rnf (ExferenceSourceTypeVariableNames sourceNames) = rnf sourceNames

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
mkExferenceSourceTypeVariableHints sourceType sourceNames =
  mkExferenceSourceTypeVariableNames sourceNames
    >>= bindExferenceSourceTypeVariableHints sourceType

-- | Validate and detach a frontend spelling index without choosing the goal
-- that brings its variables into scope.
--
-- Entries remain in the source map's spelling order so a later scope check
-- reports the same first failure as 'mkExferenceSourceTypeVariableHints'.
mkExferenceSourceTypeVariableNames
  :: TypeVarIndex
  -> Either ExferenceSourceTypeVariableHintError
       ExferenceSourceTypeVariableNames
mkExferenceSourceTypeVariableNames sourceNames = do
  validSpellings <- traverse validateSpelling $ Map.toAscList sourceNames
  pure $! force $ ExferenceSourceTypeVariableNames validSpellings
 where
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

  reject failure = Left $! force failure

-- | Bind detached spellings to one checked source goal.
--
-- Scope validation deliberately precedes alias collapse: an out-of-scope
-- alias cannot be hidden by another spelling for the same integer identity.
bindExferenceSourceTypeVariableHints
  :: HsType
  -> ExferenceSourceTypeVariableNames
  -> Either ExferenceSourceTypeVariableHintError
       ExferenceSourceTypeVariableHints
bindExferenceSourceTypeVariableHints sourceType
    (ExferenceSourceTypeVariableNames sourceNames) = do
  inScopeSpellings <- traverse requireInScope sourceNames
  pure $! force $ ExferenceSourceTypeVariableHints canonicalSourceType
    $ preferredSourceTypeVariableNames inScopeSpellings
 where
  canonicalSourceType = SharedType.canonicalizeType sourceType
  sourceVariables = flexibleIdentifiers canonicalSourceType

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

-- | The canonical Exference payload carries the exact checked definition
-- target rather than an intermediate expression-only candidate.
type ExferenceCandidate =
  Candidate HsType ExferenceCandidateDetails
    (Generated.FunctionClause TVarId)

-- The engine calls this only after input, typing, scope, completeness, and
-- generated-syntax checks.  Keeping it in an Internal module makes that
-- precondition unavailable as an unchecked public escape hatch.
projectValidatedCandidate
  :: Generated.DefinitionName
  -> ExferenceTypeVariableHints
  -> Exference.Expression
  -> [HsConstraint]
  -> ExferenceStats
  -> ExferenceCandidate
projectValidatedCandidate target typeNames expression constraints statistics =
  fmap (Generated.functionClauseFromExpression target) $ force
  $ Candidate cleanedExpression
      constraints ExferenceCandidateDetails
      { exferenceCandidateStats = statistics
          { exference_complexityRating = normalizePenalty
              $ exference_complexityRating statistics
          }
      , exferenceLocalNameHints = Exference.expressionNameHints expression
      , exferenceTypeVariableHints = typeNames
      }
 where
  -- Search and the independent checker consume the fully typed expression.
  -- Once that boundary has accepted a candidate, an intentionally unused
  -- lambda or constructor-field binder carries no evidence and can be exposed
  -- honestly as a wildcard.  Keeping this cleanup at projection time avoids
  -- teaching Exference's typed compatibility patterns about an unannotated
  -- binder while giving @exferenceAllowUnused@ clean stable output.
  cleanedExpression = Generated.discardUnusedPatternBindingsBy id
    $ Exference.toGeneratedExpression expression

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

-- | Recover the canonical goal sealed into a checked source-hint witness.
--
-- This projection is exported only from the hidden implementation module so
-- the checked request can use the witness as its private execution plan. The
-- public core still cannot inspect the witness or pair its spellings with a
-- different goal.
sourceTypeVariableHintGoal :: ExferenceSourceTypeVariableHints -> HsType
sourceTypeVariableHintGoal (ExferenceSourceTypeVariableHints sourceType _) =
  sourceType
