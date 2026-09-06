-- | The source authority and exact lowering history of one checked Djinn
-- proof.  This module deliberately precedes source graph construction: no
-- type is recovered from a logical atom or from a rendered Haskell string.
module Djinn.Internal.SourceEvidence
  ( SourceTypingContext
  , sourceTypingContext
  , sourceTypingContextWithProviderKinds
  , sourceTypingPreparedEnvironment
  , sourceTypingGoal
  , sourceTypingProviderKinds
  , sourceTypingTermSchemes
  , SourceCandidate
  , lowerCheckedSourceCandidate
  , sourceCandidateClause
  , sourceCandidateContext
  , sourceCandidateProofEvidence
  , sourceCandidateRestoredProof
  , sourceCandidateProviderProof
  , sourceCandidateErasedProof
  , sourceCandidateVisibleApplications
  , sourceCandidateProviderApplications
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Djinn.Internal.Instantiation
  ( eliminateInstantiationEvidence, rewriteProviderInstantiationEvidence
  , usesInstantiationEvidence )
import Djinn.Internal.LJTFormula (Symbol, Term)
import Djinn.Internal.ProofCheck.Evidence
  ( CheckedProofEvidence, checkedProofEnvironment, checkedProofRoot
  , checkedProofNodeTerm )
import Djinn.Internal.ProofEnv
  ( ProofEnvironment, proofBindings, restoreProofTerm )
import Djinn.Internal.ProofToGenerated
  ( termToGeneratedClause, termToGeneratedClauseWithVisibleApplications )
import Djinn.Internal.SourceTypingContext
  ( SourceTypingContext, sourceTypingContext, sourceTypingContextWithProviderKinds
  , sourceTypingPreparedEnvironment, sourceTypingGoal, sourceTypingProviderKinds
  , sourceTypingTermSchemes )
import qualified Language.Haskell.Synthesis.Generated as Generated

-- | One indivisible association.  Every term in the history is computed
-- from the checker-owned root by this constructor; callers cannot supply a
-- different clause beside a checked proof or replace an intermediate stage.
data SourceCandidate = SourceCandidate
  SourceTypingContext
  CheckedProofEvidence
  Term
  Term
  Term
  (Map.Map Symbol [Generated.VisibleTypeArgument])
  (Map.Map Symbol (Symbol, [Generated.VisibleTypeArgument]))
  (Generated.FunctionClause String)

lowerCheckedSourceCandidate
  :: SourceTypingContext
  -> ProofEnvironment
  -> Set.Set Symbol
  -> Map.Map Symbol [Generated.VisibleTypeArgument]
  -> Map.Map Symbol (Symbol, [Generated.VisibleTypeArgument])
  -> Generated.DefinitionName
  -> CheckedProofEvidence
  -> Either String SourceCandidate
lowerCheckedSourceCandidate context proofEnvironment axiomSymbols visible
    providers target evidence
  | checkedProofEnvironment evidence /= proofBindings proofEnvironment =
      Left "source lowering received evidence for another proof environment"
  | otherwise = do
      let raw = checkedProofNodeTerm $ checkedProofRoot evidence
          restored = restoreProofTerm proofEnvironment raw
          providerApplied = rewriteProviderInstantiationEvidence providers restored
          implicitSymbols = axiomSymbols `Set.difference` Map.keysSet visible
          erased = eliminateInstantiationEvidence implicitSymbols providerApplied
          convert
            | usesInstantiationEvidence axiomSymbols restored =
                termToGeneratedClauseWithVisibleApplications visible
            | otherwise = termToGeneratedClause
      clause <- convert target erased
      pure $ SourceCandidate context evidence restored providerApplied erased
        visible providers clause

sourceCandidateClause :: SourceCandidate -> Generated.FunctionClause String
sourceCandidateClause (SourceCandidate _ _ _ _ _ _ _ clause) = clause

sourceCandidateContext :: SourceCandidate -> SourceTypingContext
sourceCandidateContext (SourceCandidate context _ _ _ _ _ _ _) = context

sourceCandidateProofEvidence :: SourceCandidate -> CheckedProofEvidence
sourceCandidateProofEvidence (SourceCandidate _ evidence _ _ _ _ _ _) = evidence

sourceCandidateRestoredProof :: SourceCandidate -> Term
sourceCandidateRestoredProof (SourceCandidate _ _ restored _ _ _ _ _) = restored

sourceCandidateProviderProof :: SourceCandidate -> Term
sourceCandidateProviderProof (SourceCandidate _ _ _ providers _ _ _ _) = providers

sourceCandidateErasedProof :: SourceCandidate -> Term
sourceCandidateErasedProof (SourceCandidate _ _ _ _ erased _ _ _) = erased

sourceCandidateVisibleApplications
  :: SourceCandidate -> Map.Map Symbol [Generated.VisibleTypeArgument]
sourceCandidateVisibleApplications (SourceCandidate _ _ _ _ _ visible _ _) = visible

sourceCandidateProviderApplications
  :: SourceCandidate -> Map.Map Symbol (Symbol, [Generated.VisibleTypeArgument])
sourceCandidateProviderApplications (SourceCandidate _ _ _ _ _ _ providers _) = providers
