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
  , nestedGivenErasure
  , lowerCheckedContextualSourceCandidate
  , ContextualLoweringFailure(..), admitCheckedContextualSourceCandidate
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
import Data.Bifunctor (first)
import qualified Djinn.Internal.ContextualInstantiation as Contextual
import Djinn.Internal.Environment
  ( PreparedRootGivenOpening, rootGivenOpeningSource, rootGivenOpeningContexts
  , rootGivenOpeningBody, prepareRootGivenOpening
  , nestedGivenOpeningQuerySource, nestedGivenOpeningQueryBody
  , nestedGivenOpeningKindScope, nestedGivenOpeningSource
  , nestedGivenOpeningContexts, nestedGivenOpeningAvailableContexts
  , preparedEnvironmentSynthesisFormulaTranslator )

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
import qualified Language.Haskell.Synthesis.Type as Type

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
  | NestedGivenErasure
      (Type.Type String) (Maybe PreparedRootGivenOpening) Formula
      [Contextual.ContextualInstantiation] [Contextual.ContextualIntroduction]

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
rootGivenErasureGoal (NestedGivenErasure _ _ goal _ _) = goal

-- | Admit lexical introduction rules only for one checked original query.
-- Their dictionaries are assumptions of their own proof argument, never
-- additional premises of the enclosing search or of a sibling callback.
nestedGivenErasure
  :: SourceTypingContext -> [Contextual.ContextualInstantiation]
  -> [Contextual.ContextualIntroduction] -> Either String RootGivenErasure
nestedGivenErasure _ _ [] = Left "nested-Given erasure has no source introduction"
nestedGivenErasure context helpers introductions@(firstIntroduction : _) = do
  let firstOpening = Contextual.contextualIntroductionOpening firstIntroduction
      source = sourceTypingGoal context
      body = nestedGivenOpeningQueryBody firstOpening
      openings = map Contextual.contextualIntroductionOpening introductions
      names = map Contextual.contextualInstantiationSymbol helpers ++
        map Contextual.contextualIntroductionSymbol introductions
      available = map nestedGivenOpeningAvailableContexts openings
      prepared = sourceTypingPreparedEnvironment context
  unless (all (\opening -> nestedGivenOpeningQuerySource opening == source &&
      nestedGivenOpeningQueryBody opening == body &&
      nestedGivenOpeningKindScope opening == nestedGivenOpeningKindScope firstOpening) openings) $
    Left "nested-Given erasure belongs to another source query or kind scope"
  unless (length names == Set.size (Set.fromList names)) $
    Left "nested-Given erasure has ambiguous helper identities"
  unless (all (\helper -> any
      (\givens -> all (`elem` givens) $ Contextual.contextualInstantiationObligations helper)
      available) helpers) $
    Left "conditional helper combines dictionaries from unrelated lexical scopes"
  root <- prepareRootGivenOpening prepared source body
  let (_, rootContexts, _) = Type.splitLeadingForalls $ nestedGivenOpeningKindScope firstOpening
  unless (maybe (null rootContexts)
      ((== rootContexts) . rootGivenOpeningContexts) root) $
    Left "nested-Given erasure lacks its unambiguous root opening"
  bodyFormula <- preparedEnvironmentSynthesisFormulaTranslator prepared body
  let goal = foldr ((:->) . PVar . dictionarySymbol) bodyFormula rootContexts
  pure $ NestedGivenErasure source root goal helpers introductions

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

-- A valid propositional proof may be outside the lexical source-erasure
-- fragment. In particular LJT's implication rule can reuse an introduction
-- helper under an equal active dictionary. Such a proof consumes its original
-- search/raw allowance but must not abort other candidates or acquire a graph.
-- Broken checked evidence or source associations remain internal failures.
data ContextualLoweringFailure
  = UnsupportedContextualErasure String
  | InvalidContextualLowering String
  deriving (Eq, Show)

admitCheckedContextualSourceCandidate
  :: RootGivenErasure -> SourceTypingContext -> ProofEnvironment
  -> Set.Set Symbol -> Map.Map Symbol [Generated.VisibleTypeArgument]
  -> Map.Map Symbol (Symbol, [Generated.VisibleTypeArgument])
  -> Generated.DefinitionName -> CheckedProofEvidence
  -> Either ContextualLoweringFailure SourceCandidate
admitCheckedContextualSourceCandidate receipt context proofEnvironment axiomSymbols
    visible providers target evidence = do
  unless (checkedProofEnvironment evidence == proofBindings proofEnvironment) $
    Left $ InvalidContextualLowering "source lowering received evidence for another proof environment"
  contextualProof <- eraseRootGivenProof context proofEnvironment receipt evidence
  first InvalidContextualLowering $ constructSourceCandidate (Just receipt) context
    proofEnvironment axiomSymbols visible providers target evidence contextualProof

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
      contextualProof <- case contextual of
        Nothing -> Right $ checkedProofNodeTerm $ checkedProofRoot evidence
        Just receipt -> first contextualFailureMessage $
          eraseRootGivenProof context proofEnvironment receipt evidence
      constructSourceCandidate contextual context proofEnvironment axiomSymbols
        visible providers target evidence contextualProof

contextualFailureMessage :: ContextualLoweringFailure -> String
contextualFailureMessage (UnsupportedContextualErasure message) = message
contextualFailureMessage (InvalidContextualLowering message) = message

constructSourceCandidate
  :: Maybe RootGivenErasure -> SourceTypingContext -> ProofEnvironment
  -> Set.Set Symbol -> Map.Map Symbol [Generated.VisibleTypeArgument]
  -> Map.Map Symbol (Symbol, [Generated.VisibleTypeArgument])
  -> Generated.DefinitionName -> CheckedProofEvidence -> Term
  -> Either String SourceCandidate
constructSourceCandidate contextual context proofEnvironment axiomSymbols visible
    providers target evidence contextualProof = do
      let raw = checkedProofNodeTerm $ checkedProofRoot evidence
          restored = restoreProofTerm proofEnvironment raw
          providerApplied = rewriteProviderInstantiationEvidence providers $
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
  -> CheckedProofEvidence -> Either ContextualLoweringFailure Term
eraseRootGivenProof context proofEnvironment receipt evidence = do
  unless (sourceTypingGoal context == source &&
      checkedProofExpectedType evidence == goal) $
    invalid "contextual proof does not belong to its root opening"
  (rootBinders, body) <- peel False Map.empty
    (map (PVar . dictionarySymbol) rootContexts)
    $ checkedProofRoot evidence
  erase rootBinders helperTable body
 where
  (source, rootContexts, goal, helpers, introductions) = case receipt of
    RootGivenErasure opening form rows ->
      (rootGivenOpeningSource opening, rootGivenOpeningContexts opening, form, rows, [])
    NestedGivenErasure original opening form rows nested ->
      (original, maybe [] rootGivenOpeningContexts opening, form, rows, nested)
  sourceHelpers = Map.fromList
    ([(Contextual.contextualInstantiationSymbol helper, Left helper) | helper <- helpers] ++
     [(Contextual.contextualIntroductionSymbol helper, Right helper) | helper <- introductions])
  helperFormula = either Contextual.contextualInstantiationFormula
    Contextual.contextualIntroductionFormula
  helperTable = Map.fromList
    [ (internal, helper)
    | (internal, formula) <- proofBindings proofEnvironment
    , Var restoredSymbol <- [restoreProofTerm proofEnvironment $ Var internal]
    , Just helper <- [Map.lookup restoredSymbol sourceHelpers]
    , formula == helperFormula helper
    ]
  exact :: CheckedProofNode -> Maybe Formula
  exact node = checkedProofTypeExactFormula $ checkedProofNodeType node
  invalid = Left . InvalidContextualLowering
  unsupported = Left . UnsupportedContextualErasure
  peel _ bindings [] node = Right (bindings, node)
  peel nested bindings (dictionary : remaining) node =
    case (checkedProofNodeTerm node, checkedProofNodeChildren node, exact node) of
      (Lam binder _, [child], Just (domain :-> _))
        | domain /= dictionary -> invalid "contextual dictionary introduction has another checked domain"
        | Map.member binder bindings -> unsupported "contextual dictionary introduction shadows an active binder"
        | dictionary `elem` Map.elems bindings ->
            unsupported "contextual dictionary introduction overlaps an equal active Given"
        | otherwise -> peel nested (Map.insert binder dictionary bindings) remaining child
      _ | nested -> unsupported "nested contextual proof lacks its actual dictionary-lambda prefix"
        | otherwise -> invalid "contextual proof lacks its checked root dictionary introduction"
  spine node arguments = case (checkedProofNodeTerm node, checkedProofNodeChildren node) of
    (Apply _ _, [function, argument]) -> spine function $ argument : arguments
    _ -> (node, arguments)
  erase bindings availableHelpers node = case spine node [] of
    (headNode, arguments)
      | Var name <- checkedProofNodeTerm headNode
      , Just (Left helper) <- Map.lookup name availableHelpers -> do
          unless (exact headNode == Just (Contextual.contextualInstantiationFormula helper)) $
            invalid "conditional helper occurrence has another checked formula"
          let obligations = Contextual.contextualInstantiationObligations helper
              count = length obligations
          case arguments of
            provider : supplied | length supplied >= count -> do
              unless (exact provider == Just (PVar $ opaqueTypeSymbol $
                  Contextual.contextualInstantiationSource helper)) $
                invalid "conditional helper source occurrence changed its complete scheme"
              let (dictionaries, ordinary) = splitAt count supplied
              zipWithM_ (checkDictionary bindings) obligations dictionaries
              appliedProvider <- erase bindings availableHelpers provider
              applys appliedProvider <$> mapM (erase bindings availableHelpers) ordinary
            _ -> unsupported "unsaturated conditional helper cannot be erased"
    (headNode, arguments)
      | Var name <- checkedProofNodeTerm headNode
      , Just (Right introduction) <- Map.lookup name availableHelpers -> do
          unless (exact headNode == Just (Contextual.contextualIntroductionFormula introduction)) $
            invalid "nested introduction occurrence has another checked formula"
          let opening = Contextual.contextualIntroductionOpening introduction
              dictionaries = map (PVar . dictionarySymbol) $ nestedGivenOpeningContexts opening
              expectedArgument = foldr (:->)
                (Contextual.contextualIntroductionBodyFormula introduction) dictionaries
          unless (exact node == Just (PVar $ opaqueTypeSymbol $ nestedGivenOpeningSource opening)) $
            invalid "nested introduction changed its exact qualified result"
          case arguments of
            [argument] | exact argument == Just expectedArgument -> do
              (localBindings, body) <- peel True bindings dictionaries argument
              erase localBindings availableHelpers body
            _ -> invalid "nested introduction requires its exact checked dictionary-lambda argument"
    _ -> case (checkedProofNodeTerm node, checkedProofNodeChildren node) of
      (Var binder, [])
        | Map.member binder bindings -> unsupported "root dictionary escaped its conditional helper"
        | otherwise -> Right $ Var binder
      (Lam binder _, [body]) -> Lam binder <$>
        erase (Map.delete binder bindings) (Map.delete binder availableHelpers) body
      (Apply _ _, [function, argument]) ->
        Apply <$> erase bindings availableHelpers function <*> erase bindings availableHelpers argument
      (Xsel index arity _, [body]) -> Xsel index arity <$> erase bindings availableHelpers body
      (primitive, []) -> Right primitive
      _ -> invalid "contextual erasure encountered inconsistent checked children"
  checkDictionary bindings obligation node = do
    let expected = PVar $ dictionarySymbol obligation
    unless (exact node == Just expected) $
      invalid "conditional helper dictionary has another checked constraint"
    case checkedProofNodeTerm node of
      Var binder | Map.lookup binder bindings == Just expected -> Right ()
      _ -> unsupported "conditional helper dictionary is not its actual root Given"
