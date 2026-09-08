-- | Conditional specialization of complete constrained provider schemes.
--
-- Each rule has the shape
-- @Opaque(source) -> D(context[0]) -> ... -> compile(body)@. The ordered
-- dictionary premises remain explicit propositions; no ambient or pooled
-- class assumption is used to justify the rule. A checked proof must supply
-- both the provider and its dictionaries before lowering can erase this
-- evidence. These helpers therefore never belong to the unary identity
-- evidence handled by "Djinn.Internal.Instantiation".
module Djinn.Internal.ContextualInstantiation
    ( ContextualInstantiationRequest
    , ContextualInstantiationAxioms
    , ContextualInstantiation
    , checkContextualInstantiationKinds
    , checkNestedContextualInstantiationKinds
    , ContextualIntroduction, contextualIntroductions
    , contextualIntroductionSymbol, contextualIntroductionOpening
    , contextualIntroductionFormula, contextualIntroductionBodyFormula
    , prepareContextualInstantiation
    , sealContextualInstantiation
    , contextualInstantiationAxioms
    , buildContextualInstantiationAxioms
    , contextualInstantiationPremises
    , contextualInstantiations
    , contextualInstantiationSymbol
    , contextualInstantiationSource
    , contextualInstantiationArguments
    , contextualInstantiationObligations
    , contextualInstantiationBody
    , contextualInstantiationFormula
    , contextualInstantiationEvidenceArity
    , contextualInstantiationDictionaryArity
    ) where

import Control.Monad (unless, when)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Djinn.Internal.Environment
    ( PreparedEnvironment, PreparedRootGivenOpening, PreparedNestedGivenOpening
    , preparedEnvironmentInventory, checkPreparedSynthesisTypesKindsWithRigids
    , preparedEnvironmentSynthesisFormulaTranslator
    , rootGivenOpeningContexts, rootGivenOpeningBody
    , nestedGivenOpeningKindScope, nestedGivenOpeningSource
    , nestedGivenOpeningContexts, nestedGivenOpeningBody
    , nestedGivenOpeningAvailableContexts )
import Djinn.Internal.HTypes (HKind(KStar), fromGroundHKind)
import Djinn.Internal.LJTFormula
    ( Formula (..), Symbol (Symbol), dictionarySymbol, opaqueTypeSymbol )
import Language.Haskell.Synthesis.Collection (distinctOn, observedListLength)
import Language.Haskell.Synthesis.Constraint (Constraint)
import qualified Language.Haskell.Synthesis.Inventory as Inventory
import qualified Language.Haskell.Synthesis.KindInference as KindInference
import qualified Language.Haskell.Synthesis.Query as Query
import qualified Language.Haskell.Synthesis.Type as Type
import qualified Language.Haskell.Synthesis.TypeAtom as TypeAtom
import qualified Language.Haskell.Synthesis.TypedGenerated as Typed

-- Constructors and record-update syntax are deliberately unavailable outside
-- this module. Preparing a row retains the complete source and one correlated
-- vector, but does not allocate a helper identity. Optional rejected tuples
-- can thus be filtered before one family allocates all of its private names.
data ContextualInstantiationRequest = ContextualInstantiationRequest
    (Type.Type String)
    [Type.Type String]
    [Constraint (Type.Type String)]
    (Type.Type String)
    Formula

data ContextualInstantiation = ContextualInstantiation
    Symbol ContextualInstantiationRequest

newtype ContextualInstantiationAxioms = ContextualInstantiationAxioms
    [ContextualInstantiation]

-- | Validate a complete vector against its original source telescope and the
-- exact root opening used by this proof plan. This is the production admission
-- gate, separate from structural substitution below. Both provider and ambient
-- root kinds are inferred before selection, then fixed jointly: a selection
-- cannot change an ambient variable's original Haskell-98 defaulted kind.
checkContextualInstantiationKinds
    :: PreparedEnvironment
    -> PreparedRootGivenOpening
    -> Type.Type String
    -> [Type.Type String]
    -> Either String ()
checkContextualInstantiationKinds prepared opening source vector = do
    let root = Type.ForallType [] (rootGivenOpeningContexts opening) $
            rootGivenOpeningBody opening
    checkInstantiationKindsInScope prepared root source vector

checkNestedContextualInstantiationKinds
    :: PreparedEnvironment -> PreparedNestedGivenOpening
    -> Type.Type String -> [Type.Type String] -> Either String ()
checkNestedContextualInstantiationKinds prepared opening =
    checkInstantiationKindsInScope prepared $ nestedGivenOpeningKindScope opening

checkInstantiationKindsInScope
    :: PreparedEnvironment -> Type.Type String
    -> Type.Type String -> [Type.Type String] -> Either String ()
checkInstantiationKindsInScope prepared root source vector = do
    let ambient = Type.freeVariables root
        reserved = Set.unions $ ambient : map allVariables vector
    (unique, _) <- first show $ Type.uniquifyTypeBinders
        (const (Nothing :: Maybe ())) freshVariable reserved source
    let (binders, contexts, body) = Type.splitLeadingForalls unique
        qualification = Type.ForallType [] contexts body
        shared = binders ++ Set.toAscList ambient
    unless (length binders == length vector) $
        Left "contextual kind check requires a complete source vector"
    inferred <- first show $ KindInference.inferSharedVariableKinds
        (Inventory.inventoryKindAssumptions $ preparedEnvironmentInventory prepared)
        shared [qualification, root]
    let (sourceKinds, ambientKinds) = splitAt (length binders) inferred
        kinds = map (fromGroundHKind . snd) sourceKinds
        fixedAmbient =
            [(fromGroundHKind kind, Type.TypeVariable variable)
            | (variable, kind) <- ambientKinds]
    checkPreparedSynthesisTypesKindsWithRigids prepared ambient $
        (KStar, root) : fixedAmbient ++ zip kinds vector

-- | A source-owned qualification introduction. It supplies no dictionary or
-- body value: its sole argument must prove the body under actual dictionary
-- lambdas. The checked eraser consumes those lambdas only inside this helper.
data ContextualIntroduction = ContextualIntroduction
    Symbol PreparedNestedGivenOpening Formula

contextualIntroductions
    :: PreparedEnvironment -> [PreparedNestedGivenOpening]
    -> Either String [ContextualIntroduction]
contextualIntroductions prepared openings = mapM prepare $
    zip [0 :: Int ..] $ distinctOn
        (\opening -> (TypeAtom.alphaTypeKey $ nestedGivenOpeningSource opening,
            map dictionarySymbol $ nestedGivenOpeningAvailableContexts opening)) openings
  where
    prepare (index, opening) = do
        body <- preparedEnvironmentSynthesisFormulaTranslator prepared $
            nestedGivenOpeningBody opening
        pure $ ContextualIntroduction
            (Symbol $ "$djinn$context-introduction$" ++ show index) opening body

contextualIntroductionSymbol :: ContextualIntroduction -> Symbol
contextualIntroductionSymbol (ContextualIntroduction symbol _ _) = symbol

contextualIntroductionOpening :: ContextualIntroduction -> PreparedNestedGivenOpening
contextualIntroductionOpening (ContextualIntroduction _ opening _) = opening

contextualIntroductionBodyFormula :: ContextualIntroduction -> Formula
contextualIntroductionBodyFormula (ContextualIntroduction _ _ body) = body

contextualIntroductionFormula :: ContextualIntroduction -> Formula
contextualIntroductionFormula (ContextualIntroduction _ opening body) =
    foldr (\constraint rest -> PVar (dictionarySymbol constraint) :-> rest)
        body (nestedGivenOpeningContexts opening) :->
    PVar (opaqueTypeSymbol $ nestedGivenOpeningSource opening)

-- | Check structural specialization of one exact source scheme. Production
-- preparation must first validate the complete selected vector against the
-- original source binder kinds in the actual surrounding scope. The first
-- callback then checks the original and substituted complete types, including
-- their class declarations. These two complete-type checks alone do not
-- establish the selected vector's original kinds: substituting a higher-kind
-- head together with its argument can change both kinds while leaving the
-- complete result well-kinded. The second callback compiles the specialized
-- body in the same source plan. The source/vector preparation and callbacks
-- are trusted translation authority, as for the existing instantiation family.
--
-- Every original leading binder is selected simultaneously. Original direct
-- contexts retain their order and duplicates.
-- A selected polytype's own foralls or contexts stay inside that selection;
-- they are never mistaken for another layer of the provider's telescope.
-- Selected arguments are not independently required to have kind @Type@:
-- their exact kinds come from the caller's original source-vector authority.
-- Wholly vacuous leading binders are outside this first boundary: validating
-- their selected kinds requires separate source-scheme/type-vector authority,
-- since the selection disappears from the complete substituted type. Opaque
-- forwarding of the unspecialized qualified value remains a separate rule.
prepareContextualInstantiation
    :: (Type.Type String -> Either String ())
    -> (Type.Type String -> Either String Formula)
    -> Type.Type String
    -> [Type.Type String]
    -> Either String ContextualInstantiationRequest
prepareContextualInstantiation checkType compileBody source arguments = do
    when (observedListLength maximumArguments arguments > maximumArguments) $
        Left "contextual instantiation argument limit exceeded"
    source' <- normalize "source" source
    arguments' <- mapM (normalize "argument") arguments
    checkType source'
    let protected = Set.unions $ map allVariables arguments'
    (uniqueSource, reserved) <- first
        (("contextual instantiation source binders: " ++) . show) $
        Type.uniquifyTypeBinders (const (Nothing :: Maybe ()))
            freshVariable protected source'
    let (binders, obligations, body) = Type.splitLeadingForalls uniqueSource
    unless (length binders == length arguments') $
        Left "contextual instantiation requires the complete leading type vector"
    when (null obligations) $
        Left "contextual instantiation requires a constrained source scheme"
    let qualifiedBody = Type.ForallType [] obligations body
        usedBinders = Type.freeVariables qualifiedBody
    when (any (`Set.notMember` usedBinders) binders) $
        Left $ "unsupported contextual instantiation: vacuous leading binder " ++
            "requires exact type-vector kind evidence"
    -- Only the original telescope is stripped. Keeping this qualification
    -- wrapper through substitution lets one simultaneous operation freshen
    -- nested binders across both obligations and body without flattening any
    -- inserted polytype's own leading context into the original obligations.
    specialized <- first
        (("contextual instantiation substitution: " ++) . show) $
        Type.substituteTypeVariables freshVariable reserved
            (Map.fromList $ zip binders arguments')
            qualifiedBody
    specialized' <- normalize "specialized source" specialized
    checkType specialized'
    case specialized' of
        Type.ForallType [] obligations' body' -> do
            bodyFormula <- compileBody body'
            pure $ ContextualInstantiationRequest source' arguments'
                obligations' body' bodyFormula
        _ -> Left "contextual instantiation lost its original qualification"
  where
    maximumArguments = Query.maximumProviderInstantiationArguments

-- | Alias emphasizing the checked boundary when preparing optional rows.
sealContextualInstantiation
    :: (Type.Type String -> Either String ())
    -> (Type.Type String -> Either String Formula)
    -> Type.Type String
    -> [Type.Type String]
    -> Either String ContextualInstantiationRequest
sealContextualInstantiation = prepareContextualInstantiation

-- | Allocate one family from the caller's existing bounded candidate pool.
-- Stable alpha-aware deduplication preserves the first exact source view and
-- its ordered selections. Build once per proof plan: independently allocated
-- families have plan-local identities and must not be concatenated afterward.
contextualInstantiationAxioms
    :: [ContextualInstantiationRequest] -> ContextualInstantiationAxioms
contextualInstantiationAxioms requests = ContextualInstantiationAxioms
    $ zipWith allocate [0 :: Int ..] $ distinctOn requestKey requests
  where
    allocate index = ContextualInstantiation
        $ Symbol $ "$djinn$context-instantiation$" ++ show index

-- | All-or-error convenience entrance. Search callers exploring optional
-- tuples should prepare each row separately and allocate the retained family
-- once with 'contextualInstantiationAxioms'.
buildContextualInstantiationAxioms
    :: (Type.Type String -> Either String ())
    -> (Type.Type String -> Either String Formula)
    -> [(Type.Type String, [Type.Type String])]
    -> Either String ContextualInstantiationAxioms
buildContextualInstantiationAxioms checkType compileBody requests =
    contextualInstantiationAxioms <$> mapM
        (uncurry $ prepareContextualInstantiation checkType compileBody) requests

contextualInstantiationPremises
    :: ContextualInstantiationAxioms -> [(Symbol, Formula)]
contextualInstantiationPremises = map premise . contextualInstantiations
  where
    premise row =
        (contextualInstantiationSymbol row, contextualInstantiationFormula row)

contextualInstantiations
    :: ContextualInstantiationAxioms -> [ContextualInstantiation]
contextualInstantiations (ContextualInstantiationAxioms rows) = rows

contextualInstantiationSymbol :: ContextualInstantiation -> Symbol
contextualInstantiationSymbol (ContextualInstantiation symbol _) = symbol

contextualInstantiationSource :: ContextualInstantiation -> Type.Type String
contextualInstantiationSource (ContextualInstantiation _
    (ContextualInstantiationRequest source _ _ _ _)) = source

contextualInstantiationArguments
    :: ContextualInstantiation -> [Type.Type String]
contextualInstantiationArguments (ContextualInstantiation _
    (ContextualInstantiationRequest _ arguments _ _ _)) = arguments

contextualInstantiationObligations
    :: ContextualInstantiation -> [Constraint (Type.Type String)]
contextualInstantiationObligations (ContextualInstantiation _
    (ContextualInstantiationRequest _ _ obligations _ _)) = obligations

contextualInstantiationBody :: ContextualInstantiation -> Type.Type String
contextualInstantiationBody (ContextualInstantiation _
    (ContextualInstantiationRequest _ _ _ body _)) = body

contextualInstantiationFormula :: ContextualInstantiation -> Formula
contextualInstantiationFormula (ContextualInstantiation _
    (ContextualInstantiationRequest source _ obligations _ bodyFormula)) =
    PVar (opaqueTypeSymbol source) :-> foldr dictionaryPremise bodyFormula obligations
  where
    dictionaryPremise obligation result = PVar (dictionarySymbol obligation) :-> result

-- | The provider followed by every original dictionary argument.
contextualInstantiationEvidenceArity :: ContextualInstantiation -> Int
contextualInstantiationEvidenceArity = (1 +) . contextualInstantiationDictionaryArity

contextualInstantiationDictionaryArity :: ContextualInstantiation -> Int
contextualInstantiationDictionaryArity = length . contextualInstantiationObligations

requestKey
    :: ContextualInstantiationRequest
    -> (TypeAtom.TypeAtomKey String, [TypeAtom.TypeAtomKey String])
requestKey (ContextualInstantiationRequest source arguments _ _ _) =
    (TypeAtom.alphaTypeKey source, map TypeAtom.alphaTypeKey arguments)

normalize :: String -> Type.Type String -> Either String (Type.Type String)
normalize label source = do
    first ((prefix ++) . show) $
        Typed.observeTypeWithin Typed.sharedTypeStructure
            (Typed.maximumTermGraphTypeNodes limits)
            (Typed.maximumTermGraphCollectionWidth limits) source
    first ((prefix ++) . show) $ Type.normalizeType source
  where
    prefix = "contextual instantiation " ++ label ++ ": "
    limits = Typed.defaultTermGraphLimits

allVariables :: Type.Type String -> Set.Set String
allVariables source = Type.freeVariables source
    `Set.union` Set.fromList (Type.typeBinderVariables source)

freshVariable :: Type.FreshVariableAllocator String
freshVariable reserved variable = Just $ choose $ variable ++ "'"
  where
    choose candidate
        | candidate `Set.member` reserved = choose $ candidate ++ "'"
        | otherwise = candidate
