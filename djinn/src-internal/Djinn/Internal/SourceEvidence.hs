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
  , RootGivenErasure, rootGivenErasure, rootGivenErasureGoal
  , lowerCheckedContextualSourceCandidate
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
import Control.Monad (unless, zipWithM_)
import qualified Djinn.Internal.ContextualInstantiation as Contextual
import Djinn.Internal.Environment
  ( PreparedRootGivenOpening, rootGivenOpeningSource, rootGivenOpeningContexts
  , rootGivenOpeningBody, preparedEnvironmentSynthesisFormulaTranslator )

import Djinn.Internal.Instantiation
  ( eliminateInstantiationEvidence, rewriteProviderInstantiationEvidence
  , usesInstantiationEvidence )
import Djinn.Internal.LJTFormula
  ( Symbol, Formula(..), Term(..), dictionarySymbol, opaqueTypeSymbol, applys )
import Djinn.Internal.ProofCheck.Evidence
  ( CheckedProofEvidence, checkedProofEnvironment, checkedProofRoot
  , checkedProofExpectedType, CheckedProofNode, checkedProofNodeTerm
  , checkedProofNodeType, checkedProofNodeChildren, checkedProofTypeExactFormula )
import Djinn.Internal.ProofEnv
  ( ProofEnvironment, proofBindings, restoreProofTerm )
import Djinn.Internal.ProofToGenerated
  ( termToGeneratedClauseWithSourceApplications )
import Djinn.Internal.SourceTypingContext
  ( SourceTypingContext, sourceTypingContext, sourceTypingContextWithProviderKinds
  , sourceTypingPreparedEnvironment, sourceTypingGoal, sourceTypingProviderKinds
  , sourceTypingTermSchemes, sourceTypingConstructorNames )
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
  (Maybe RootGivenErasure)
  (Generated.FunctionClause String)

-- | Plan-local authority for the only dictionary erasure supported here.
-- The full source opening and independently sealed conditional helpers stay
-- together; this is not a query-wide table of available dictionaries.
data RootGivenErasure = RootGivenErasure
  PreparedRootGivenOpening Formula [Contextual.ContextualInstantiation]

rootGivenErasure
  :: SourceTypingContext -> PreparedRootGivenOpening
  -> [Contextual.ContextualInstantiation] -> Either String RootGivenErasure
rootGivenErasure context opening helpers = do
  unless (sourceTypingGoal context == rootGivenOpeningSource opening) $
    Left "root-Given erasure belongs to another source goal"
  let contexts = rootGivenOpeningContexts opening
      names = map Contextual.contextualInstantiationSymbol helpers
  unless (not (null contexts) && length names == Set.size (Set.fromList names)) $
    Left "ambiguous root-Given erasure inventory"
  unless (all (all (`elem` contexts) . Contextual.contextualInstantiationObligations) helpers) $
    Left "conditional helper requires a dictionary outside the root context"
  body <- preparedEnvironmentSynthesisFormulaTranslator
    (sourceTypingPreparedEnvironment context) $ rootGivenOpeningBody opening
  pure $ RootGivenErasure opening
    (foldr ((:->) . PVar . dictionarySymbol) body contexts) helpers

rootGivenErasureGoal :: RootGivenErasure -> Formula
rootGivenErasureGoal (RootGivenErasure _ goal _) = goal

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
    providers target evidence = lowerSourceCandidate Nothing context
      proofEnvironment axiomSymbols visible providers target evidence

lowerCheckedContextualSourceCandidate
  :: RootGivenErasure -> SourceTypingContext -> ProofEnvironment
  -> Set.Set Symbol -> Map.Map Symbol [Generated.VisibleTypeArgument]
  -> Map.Map Symbol (Symbol, [Generated.VisibleTypeArgument])
  -> Generated.DefinitionName -> CheckedProofEvidence -> Either String SourceCandidate
lowerCheckedContextualSourceCandidate receipt = lowerSourceCandidate $ Just receipt

lowerSourceCandidate
  :: Maybe RootGivenErasure -> SourceTypingContext -> ProofEnvironment
  -> Set.Set Symbol -> Map.Map Symbol [Generated.VisibleTypeArgument]
  -> Map.Map Symbol (Symbol, [Generated.VisibleTypeArgument])
  -> Generated.DefinitionName -> CheckedProofEvidence -> Either String SourceCandidate
lowerSourceCandidate contextual context proofEnvironment axiomSymbols visible
    providers target evidence
  | checkedProofEnvironment evidence /= proofBindings proofEnvironment =
      Left "source lowering received evidence for another proof environment"
  | otherwise = do
      let raw = checkedProofNodeTerm $ checkedProofRoot evidence
          restored = restoreProofTerm proofEnvironment raw
      contextualProof <- case contextual of
        Nothing -> Right raw
        Just receipt -> eraseRootGivenProof context proofEnvironment receipt evidence
      let providerApplied = rewriteProviderInstantiationEvidence providers $
            restoreProofTerm proofEnvironment contextualProof
          implicitSymbols = axiomSymbols `Set.difference` Map.keysSet visible
          erased = eliminateInstantiationEvidence implicitSymbols providerApplied
          constructors = sourceTypingConstructorNames context
          convert = termToGeneratedClauseWithSourceApplications
              (not $ usesInstantiationEvidence axiomSymbols restored) visible constructors
      clause <- convert target erased
      pure $ SourceCandidate context evidence restored providerApplied erased
        visible providers contextual clause

sourceCandidateClause :: SourceCandidate -> Generated.FunctionClause String
sourceCandidateClause (SourceCandidate _ _ _ _ _ _ _ _ clause) = clause

sourceCandidateContext :: SourceCandidate -> SourceTypingContext
sourceCandidateContext (SourceCandidate context _ _ _ _ _ _ _ _) = context

sourceCandidateProofEvidence :: SourceCandidate -> CheckedProofEvidence
sourceCandidateProofEvidence (SourceCandidate _ evidence _ _ _ _ _ _ _) = evidence

sourceCandidateRestoredProof :: SourceCandidate -> Term
sourceCandidateRestoredProof (SourceCandidate _ _ restored _ _ _ _ _ _) = restored

sourceCandidateProviderProof :: SourceCandidate -> Term
sourceCandidateProviderProof (SourceCandidate _ _ _ providers _ _ _ _ _) = providers

sourceCandidateErasedProof :: SourceCandidate -> Term
sourceCandidateErasedProof (SourceCandidate _ _ _ _ erased _ _ _ _) = erased

sourceCandidateVisibleApplications
  :: SourceCandidate -> Map.Map Symbol [Generated.VisibleTypeArgument]
sourceCandidateVisibleApplications (SourceCandidate _ _ _ _ _ visible _ _ _) = visible

sourceCandidateProviderApplications
  :: SourceCandidate -> Map.Map Symbol (Symbol, [Generated.VisibleTypeArgument])
sourceCandidateProviderApplications (SourceCandidate _ _ _ _ _ _ providers _ _) = providers

-- Consume the checked occurrence tree, not a separately supplied raw term.
-- Every removed lambda is in the actual root dictionary prefix. Every removed
-- argument is a reference to that exact lexical binder, with the exact checked
-- dictionary formula. The remaining ordinary arguments stay in source order.
eraseRootGivenProof
  :: SourceTypingContext -> ProofEnvironment -> RootGivenErasure
  -> CheckedProofEvidence -> Either String Term
eraseRootGivenProof context proofEnvironment
    (RootGivenErasure opening goal helpers) evidence = do
  unless (sourceTypingGoal context == rootGivenOpeningSource opening &&
      checkedProofExpectedType evidence == goal) $
    Left "contextual proof does not belong to its root opening"
  (rootBinders, body) <- peel Map.empty
    (map (PVar . dictionarySymbol) $ rootGivenOpeningContexts opening)
    $ checkedProofRoot evidence
  erase rootBinders helperTable body
 where
  sourceHelpers = Map.fromList
    [(Contextual.contextualInstantiationSymbol helper, helper) | helper <- helpers]
  helperTable = Map.fromList
    [ (internal, helper)
    | (internal, formula) <- proofBindings proofEnvironment
    , Var source <- [restoreProofTerm proofEnvironment $ Var internal]
    , Just helper <- [Map.lookup source sourceHelpers]
    , formula == Contextual.contextualInstantiationFormula helper
    ]
  exact :: CheckedProofNode -> Maybe Formula
  exact node = checkedProofTypeExactFormula $ checkedProofNodeType node
  peel bindings [] node = Right (bindings, node)
  peel bindings (dictionary : remaining) node =
    case (checkedProofNodeTerm node, checkedProofNodeChildren node, exact node) of
      (Lam binder _, [child], Just (domain :-> _))
        | domain == dictionary, Map.notMember binder bindings ->
            peel (Map.insert binder dictionary bindings) remaining child
      _ -> Left "contextual proof lacks its checked root dictionary introduction"
  spine node arguments = case (checkedProofNodeTerm node, checkedProofNodeChildren node) of
    (Apply _ _, [function, argument]) -> spine function $ argument : arguments
    _ -> (node, arguments)
  erase bindings availableHelpers node = case spine node [] of
    (headNode, arguments)
      | Var name <- checkedProofNodeTerm headNode
      , Just helper <- Map.lookup name availableHelpers -> do
          unless (exact headNode == Just (Contextual.contextualInstantiationFormula helper)) $
            Left "conditional helper occurrence has another checked formula"
          let obligations = Contextual.contextualInstantiationObligations helper
              count = length obligations
          case arguments of
            provider : supplied | length supplied >= count -> do
              unless (exact provider == Just (PVar $ opaqueTypeSymbol $
                  Contextual.contextualInstantiationSource helper)) $
                Left "conditional helper source occurrence changed its complete scheme"
              let (dictionaries, ordinary) = splitAt count supplied
              zipWithM_ (checkDictionary bindings) obligations dictionaries
              appliedProvider <- erase bindings availableHelpers provider
              applys appliedProvider <$> mapM (erase bindings availableHelpers) ordinary
            _ -> Left "unsaturated conditional helper cannot be erased"
    _ -> case (checkedProofNodeTerm node, checkedProofNodeChildren node) of
      (Var binder, [])
        | Map.member binder bindings -> Left "root dictionary escaped its conditional helper"
        | otherwise -> Right $ Var binder
      (Lam binder _, [body]) -> Lam binder <$>
        erase (Map.delete binder bindings) (Map.delete binder availableHelpers) body
      (Apply _ _, [function, argument]) ->
        Apply <$> erase bindings availableHelpers function <*> erase bindings availableHelpers argument
      (Xsel index arity _, [body]) -> Xsel index arity <$> erase bindings availableHelpers body
      (primitive, []) -> Right primitive
      _ -> Left "contextual erasure encountered inconsistent checked children"
  checkDictionary bindings obligation node = do
    let expected = PVar $ dictionarySymbol obligation
    unless (exact node == Just expected) $
      Left "conditional helper dictionary has another checked constraint"
    case checkedProofNodeTerm node of
      Var binder | Map.lookup binder bindings == Just expected -> Right ()
      _ -> Left "conditional helper dictionary is not its actual root Given"
