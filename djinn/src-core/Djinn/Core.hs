{-# LANGUAGE BangPatterns #-}

-- |
-- The stable, validated interface to the Djinn core.
--
-- Build a checked t'Environment' from declarations, then ask 'inhabitResult'
-- for a checked shared result of a given type; 'inhabitGenerated' and
-- 'inhabit' are structured and rendered compatibility projections. Every
-- entry point validates its
-- input — names must be lexically valid, declarations are kind-checked
-- transactionally, class arguments must fit their parameters' inferred
-- kinds — so the proof machinery only ever sees well-formed data.  The
-- @Djinn.Internal.*@ modules expose the raw types and functions without
-- these guarantees and without any API-stability promise.
--
-- > ghci> let Right t = parseHType "(a, b) -> (b, a)"
-- > ghci> reportOutcome <$> inhabit defaultQueryOptions standardEnvironment [] "swap" t
-- > Right (Realized ["swap (a, b) = (b, a)"])
module Djinn.Core (
    -- * Names, types, and kinds
    HSymbol, HType, HKind, kStar, kArrow,
    parseHType, parseContextualHType, parseHKind, SynthesisTypeError(..),
    normalizeSynthesisType,
    toSynthesisType, fromSynthesisType,
    -- * Declarations
    Constructor, Declaration(..), SynthesisDeclaration,
    DjinnDeclarationNameRole(..),
    SynthesisDeclarationError(..), toSynthesisKind, fromSynthesisKind,
    toSynthesisDeclaration, fromSynthesisDeclaration,
    -- * Environments
    Environment, emptyEnvironment, standardEnvironment,
    PreparedEnvironment, prepareEnvironment, prepareSynthesisEnvironment,
    preparedEnvironmentSource, preparedEnvironmentInventory,
    SynthesisEnvironment, SynthesisInventory,
    SynthesisEnvironmentError(..),
    toSynthesisEnvironment, toSynthesisInventory,
    fromSynthesisEnvironment,
    declareSynthesisEnvironment, removeSynthesisDeclaration,
    declare, removeDeclaration, renderEnvironmentEditFailure,
    typeDeclarations, functionDeclarations, classDeclarations,
    -- * Queries
    Context, mkContext, resolveContext, resolveInstanceMethods,
    resolvePreparedContext, resolvePreparedInstanceMethods,
    QueryOptions(..), defaultQueryOptions,
    DjinnCandidateDetails(..), DjinnCandidate,
    DjinnQueryMetadata(..), DjinnResult,
    DjinnQueryOptionsError(..), DjinnQueryError(..),
    inhabitResult, inhabitResultPrepared, inhabitSynthesisResultPrepared,
    inhabitSynthesisResultPreparedWithInstantiationCandidates,
    inhabitSynthesisResultPreparedWithInstantiationAssignments,
    inhabitSynthesisResultPreparedWithKindedInstantiationAssignments,
    GeneratedQueryReport(..), inhabitGenerated, inhabitGeneratedPrepared,
    QueryOutcome(..), QueryReport(..), inhabit
    ) where

import Control.Monad (foldM, unless)
import Data.Bifunctor (first)
import Data.List (intercalate, mapAccumL, sortOn)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import Text.ParserCombinators.ReadP
    (ReadP, option, readP_to_S, skipSpaces)

import Language.Haskell.Synthesis.Constraint
    (Constraint(..), validateKnownConstraintArityWith)
import qualified Language.Haskell.Synthesis.Candidate as SharedCandidate
import qualified Language.Haskell.Synthesis.Collection as SharedCollection
import qualified Language.Haskell.Synthesis.Environment as SharedEnvironment
import qualified Language.Haskell.Synthesis.Inventory as SharedInventory
import qualified Language.Haskell.Synthesis.Kind as SharedKind
import qualified Language.Haskell.Synthesis.KindInference as SharedKindInference
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Generated as SharedGenerated
import qualified Language.Haskell.Synthesis.Query as SharedQuery
import qualified Language.Haskell.Synthesis.Search as SharedSearch
import qualified Language.Haskell.Synthesis.Type as SharedType
import qualified Language.Haskell.Synthesis.TypeAtom as SharedTypeAtom

import Djinn.Internal.Environment
import Djinn.Internal.Declaration
import Djinn.Internal.Generated (deduplicateEtaEquivalentClauses)
import Djinn.Internal.HTypes
import Djinn.Internal.Instantiation
    ( closedMonotypeSubtrees
    , eliminateInstantiationEvidence
    , instantiationAxiomPremises
    , instantiationAxiomSymbols
    , instantiationVisibleApplications
    , instantiationAxioms
    , loadedInstantiationAxioms
    , providerInstantiationApplications
    , providerInstantiationAssignmentPremises
    , providerInstantiationPremiseBindings
    , providerInstantiationPremises
    , rewriteProviderInstantiationEvidence
    , usesInstantiationEvidence
    )
import Djinn.Internal.LJT
import Djinn.Internal.ProofCheck (checkProof)
import Djinn.Internal.ProofEnv
import Djinn.Internal.ProofToGenerated
    ( termToGeneratedClause
    , termToGeneratedClauseWithVisibleApplications
    )
import Djinn.Internal.Type
import Djinn.Internal.TypeFormula
    ( PolarizedFormulaPlans
    , exactOpaqueFormulaPlan
    , polarizedFormulaPlanSkolems
    , primaryFormulaPlan
    , pairOpaqueFormulaPlans
    , pairOpenFormulaPlans
    , tripleOpaqueFormulaPlans
    , tripleOpenFormulaPlans
    , quadrupleOpaqueFormulaPlans
    , quadrupleOpenFormulaPlans
    , singleOpaqueFormulaPlans
    , singleOpenFormulaPlans
    , translatedFormula
    , translationIncomplete
    )

------------------------------------------------------------------
-- Kinds

-- | The kind of proper types, @*@.
kStar :: HKind
kStar = KStar

-- | A function kind, e.g. @kArrow kStar kStar@ is @* -> *@.
kArrow :: HKind -> HKind -> HKind
kArrow = KArrow

-- | Parse a type in Djinn's Haskell-like syntax, requiring the whole input to
-- be consumed. Explicit @forall@ is accepted at any type position, including
-- inside constructor applications. Checked queries reopen validated positive
-- occurrences in the polarized formula plan, including contextual ones under
-- dictionary-independent semantics; raw conversion and unsupported
-- occurrences retain one inert, alpha-aware atom.
parseHType :: String -> Either String HType
parseHType = parseWith pHType "type"

-- | Parse the complete type portion of a historical Djinn query, including
-- its optional class context.  Keeping full-consumption handling here lets
-- checked adapters reuse the compatibility grammar without importing raw
-- parser combinators or an internal parser module.
parseContextualHType :: String -> Either String ([Context], HType)
parseContextualHType = parseWith parser "contextual type"
  where
    parser = do
        contexts <- option [] pHContext
        goal <- pHType
        return (contexts, goal)

-- | Parse a kind, e.g. @\"(* -> *) -> *\"@.
parseHKind :: String -> Either String HKind
parseHKind = parseWith pHKind "kind"

parseWith :: ReadP a -> String -> String -> Either String a
parseWith parser what input =
    case [value | (value, "") <- readP_to_S parseAll input] of
        value : _ -> Right value
        [] -> Left $ "cannot parse " ++ what ++ ": " ++ show input
  where
    parseAll = do
        value <- parser
        skipSpaces
        return value

------------------------------------------------------------------
-- Declarations and environments

-- | Stored type declarations: @(name, (parameters, body, kind))@.
typeDeclarations :: Environment -> [(HSymbol, ([HSymbol], HType, HKind))]
typeDeclarations = envTypes

-- | Stored function assumptions.
functionDeclarations :: Environment -> [(HSymbol, HType)]
functionDeclarations = envFunctions

-- | Stored classes: @(name, (parameters with kinds, methods))@.
classDeclarations ::
    Environment -> [(HSymbol, ([(HSymbol, HKind)], [(HSymbol, HType)]))]
classDeclarations = envClasses

-- | No declarations at all: no types, no functions, no classes.
emptyEnvironment :: Environment
emptyEnvironment = Environment [] [] []

-- | Lower a shared structural environment through Djinn's stricter lexical,
-- dependency, and kind validation. Unsupported declarations fail before any
-- mutation is committed.
fromSynthesisEnvironment
    :: SynthesisEnvironment
    -> Either SynthesisEnvironmentError Environment
fromSynthesisEnvironment =
    fmap preparedEnvironmentSource . prepareSynthesisEnvironment

-- | The environment the interactive Djinn starts with: @()@, @Bool@,
-- @Either@, @Maybe@, @Void@, @type Not x = x -> Void@, and small @Eq@
-- and @Monad@ classes.
standardEnvironment :: Environment
standardEnvironment =
    either (error . ("Djinn.Core.standardEnvironment: " ++)) id $ do
        unitEnvironment <- trustedUnitEnvironment
        foldM (flip declare) unitEnvironment
            [ DataType "Either" ["a", "b"]
                [("Left", [a]), ("Right", [b])]
            , DataType "Maybe" ["a"] [("Nothing", []), ("Just", [a])]
            , DataType "Bool" [] [("False", []), ("True", [])]
            , DataType "Void" [] []
            , TypeSynonym "Not" ["x"] (htNot "x")
            , ClassDecl "Eq" ["a"]
                [("==", a `HTArrow` (a `HTArrow` HTCon "Bool"))]
            , ClassDecl "Monad" ["m"]
                [ ("return", a `HTArrow` ma)
                , (">>=", ma `HTArrow` ((a `HTArrow` mb) `HTArrow` mb))
                ]
            ]
  where
    a = HTVar "a"
    b = HTVar "b"
    m = HTVar "m"
    ma = HTApp m a
    mb = HTApp m b

-- @()@ is a grammar-level special form, not a constructor identifier that a
-- caller may repurpose.  Install exactly the standard nullary type and value
-- constructor through this private path. Even this trusted constant crosses
-- the operational shared preparation boundary; the historical raw validator
-- remains only a compatibility surface with its own diagnostic ordering.
trustedUnitEnvironment :: Either String Environment
trustedUnitEnvironment = first show $
    preparedEnvironmentSource <$> prepareEnvironment Environment
        { envTypes = [("()", ([], HTUnion [("()", [])], KStar))]
        , envFunctions = []
        , envClasses = []
        }

-- | Add (or overwrite, for the same name in the same category) one
-- declaration.  The whole environment is revalidated transactionally:
-- on 'Left' the original environment is still the one to use.
declare :: Declaration -> Environment -> Either String Environment
declare declaration = editEnvironment $ declareSynthesisEnvironment declaration

-- | Remove a declaration by name, from whichever category holds it.
-- Rejected if the name is not defined or if a remaining declaration
-- depends on it; on 'Left' the environment is unchanged.
removeDeclaration :: HSymbol -> Environment -> Either String Environment
removeDeclaration name = editEnvironment $ removeSynthesisDeclaration name

-- Raw environments remain a compatibility view, not a second editable
-- authority. Convert once, apply the same checked shared transaction used by
-- stable sessions, and project the exact sealed result back only on success.
editEnvironment
    :: (SynthesisEnvironment
        -> Either SynthesisEnvironmentError
            (SynthesisEnvironment, PreparedEnvironment))
    -> Environment
    -> Either String Environment
editEnvironment edit environment = do
    source <- first renderEnvironmentEditFailure $
        toSynthesisEnvironment environment
    (_, prepared) <- first renderEnvironmentEditFailure $ edit source
    return $ preparedEnvironmentSource prepared

-- | Keep the raw string API useful without reconstructing the discarded raw
-- candidate merely to recover category-specific legacy wording. This is the
-- one rendering authority for edit failures: the raw string API and the
-- stable adapter's structured diagnostics both consume it.
renderEnvironmentEditFailure :: SynthesisEnvironmentError -> String
renderEnvironmentEditFailure failure = case failure of
    InvalidSynthesisInventory
            (SharedInventory.UngroundedInventoryKind variable) ->
        "kind contains an unsolved variable: " ++ show (KVar variable)
    SynthesisEnvironmentDeclarationError NonCanonicalUnitDeclaration ->
        unitDeclarationMessage
    ProtectedSynthesisUnitDeclaration ->
        unitDeclarationMessage
    InvalidSynthesisEnvironment
            (SharedEnvironment.DuplicateValueDeclaration name) ->
        "Value name is already declared: " ++ SharedName.renderCanonical name
    InvalidSynthesisEnvironment
            (SharedEnvironment.DuplicateTypeDeclaration name) ->
        "Type name is already declared: " ++ SharedName.renderCanonical name
    SynthesisDeclarationNotFound name -> name ++ " is not defined"
    ProtectedSynthesisUnit -> "() is a built-in type and cannot be removed"
    _ -> show failure

unitDeclarationMessage :: String
unitDeclarationMessage = "() is a built-in type and cannot be declared"

requireName :: String -> (HSymbol -> Bool) -> HSymbol -> Either String ()
requireName what valid name
    | valid name = Right ()
    | otherwise = Left $ show name ++ " is not a valid " ++ what ++ " name"

------------------------------------------------------------------
-- Queries

-- | A class constraint represented by the backend-neutral synthesis
-- vocabulary.  Djinn retains ownership of class lookup, kind checking, and
-- instance-generation policy; its ordinary inhabitation queries treat the
-- finite nominal syntax as validation obligations rather than proof premises.
type Context = Constraint HType

-- | Build a context from Djinn's historical string class name.  Class
-- declarations are currently unqualified constructor identifiers, so this
-- bridge deliberately enforces the same namespace before constructing the
-- shared nominal value.
mkContext :: HSymbol -> [HType] -> Either String Context
mkContext className arguments = do
    requireName "class" (isDjinnDeclarationName ClassOwner) className
    case SharedName.parseName className of
        Left nameError -> Left $ SharedName.renderNameError nameError
        Right sharedName -> Right $ Constraint sharedName arguments

-- | Look up a class use, requiring exact arity, arguments that fit the
-- parameters' inferred kinds, and well-kinded instantiated methods.
-- Returns each method at its instantiated type.
resolveContext :: Environment -> Context -> Either String [(HSymbol, HType)]
resolveContext environment context = do
    prepared <- prepareCompatibilityEnvironment environment
    resolvePreparedContext prepared context

-- | Resolve one context against an already sealed environment.  Unlike the
-- historical wrapper, this path performs no whole-environment kind
-- conversion and is therefore suitable for repeated session queries.
resolvePreparedContext
    :: PreparedEnvironment -> Context -> Either String [(HSymbol, HType)]
resolvePreparedContext prepared context =
    concat <$> resolveContexts prepared [context]

-- | Resolve the methods of an instance target while checking the target and
-- all prerequisite contexts in one kind-variable scope.  The returned methods
-- belong only to the target and are instantiated exactly as by
-- 'resolveContext'.
resolveInstanceMethods :: Environment -> [Context] -> Context
                       -> Either String [(HSymbol, HType)]
resolveInstanceMethods environment prerequisites target = do
    prepared <- prepareCompatibilityEnvironment environment
    resolvePreparedInstanceMethods prepared prerequisites target

-- | Prepared counterpart of 'resolveInstanceMethods'.
resolvePreparedInstanceMethods :: PreparedEnvironment -> [Context] -> Context
                               -> Either String [(HSymbol, HType)]
resolvePreparedInstanceMethods prepared prerequisites target = do
    resolved <- resolveContexts prepared (target : prerequisites)
    case resolved of
        targetMethods : _ -> Right targetMethods
        [] -> Left "internal error: instance target was not resolved"

prepareCompatibilityEnvironment
    :: Environment -> Either String PreparedEnvironment
prepareCompatibilityEnvironment = first show . prepareEnvironment

-- The intermediate record keeps lookup/arity validation separate from the
-- joint kind check and substitution.  In particular, every argument and the
-- query goal must share one scope for their free type variables.
data ResolvedContext typeExpression = ResolvedContext {
    resolvedConstraint :: Constraint typeExpression,
    resolvedParameters :: [(HSymbol, HKind)],
    -- Keep sealed methods in the authoritative shared representation.  The
    -- compatibility API wraps only the final instantiated result, rather
    -- than routing substitution through the historical HType patterns in
    -- stable queries.
    resolvedMethods ::
        [(SharedName.Name, SharedType.Type HSymbol)]
    }

resolvedName :: ResolvedContext typeExpression -> HSymbol
resolvedName = SharedName.renderCanonical . constraintClass . resolvedConstraint

resolvedArguments :: ResolvedContext typeExpression -> [typeExpression]
resolvedArguments = constraintArguments . resolvedConstraint

resolveContexts :: PreparedEnvironment -> [Context]
                -> Either String [[(HSymbol, HType)]]
resolveContexts prepared contexts = do
    resolved <- mapM (lookupResolvedContext prepared) contexts
    checkKindObligations prepared $ concatMap argumentObligations resolved
    mapM (instantiateContext prepared) resolved

internalQueryFailure :: String -> Either DjinnQueryError value
internalQueryFailure =
    Left . DjinnInternalQueryFailure . ("internal error: " ++)

-- Class lookup and arity validation are representation-independent.  Raw and
-- native callers retain their input type tree in the polymorphic constraint;
-- the sealed parameters and methods come from the same shared class index.
lookupResolvedContext
    :: PreparedEnvironment
    -> Constraint typeExpression
    -> Either String (ResolvedContext typeExpression)
lookupResolvedContext prepared context = do
    -- Context is a shared, intentionally permissive syntax node.  Reassert
    -- Djinn's narrower class namespace even when a caller constructs that
    -- node directly instead of going through mkContext.
    requireName "class" (isDjinnDeclarationName ClassOwner) name
    case lookupPreparedSynthesisClass (constraintClass context) prepared of
        Nothing -> Left $ "Class not found: " ++ name
        Just (params, methods) -> do
            validateKnownConstraintArityWith
                (const $ Just $ length params)
                (\_ expected actual ->
                    "Class " ++ name ++ " expects " ++ show expected ++
                    " type argument(s), but got " ++ show actual)
                context
            Right ResolvedContext {
                resolvedConstraint = context,
                resolvedParameters = params,
                resolvedMethods = methods
                }
  where
    name = SharedName.renderCanonical $ constraintClass context

argumentObligations
    :: ResolvedContext HType
    -> [(String, HKind, HType)]
argumentObligations context =
    [ ("argument " ++ show argument ++ " of class " ++ resolvedName context,
       kind, argument)
    | ((_, kind), argument) <-
        zip (resolvedParameters context) (resolvedArguments context) ]

checkKindObligations :: PreparedEnvironment -> [(String, HKind, HType)]
                     -> Either String ()
checkKindObligations prepared =
    checkKindObligationsWith $ checkPreparedTypesKinds prepared

-- Retain the precise historical diagnostic when one type is independently
-- ill-kinded. If every component works alone, report the actual problem:
-- inconsistent kinds assigned to a free variable shared by components. Raw
-- and native query paths differ only in the checked type representation.
checkKindObligationsWith
    :: ([(HKind, source)] -> Either String ())
    -> [(String, HKind, source)]
    -> Either String ()
checkKindObligationsWith check obligations =
    case check [(kind, source) | (_, kind, source) <- obligations] of
        Right () -> Right ()
        Left jointError ->
            case [(label, message)
                    | (label, kind, source) <- obligations
                    , Left message <-
                        [check [(kind, source)]]] of
                (label, message) : _ -> Left $ label ++ ": " ++ message
                [] -> Left $
                    "inconsistent kinds across " ++
                    intercalate ", " [label | (label, _, _) <- obligations] ++
                    ": " ++ jointError

instantiateContext :: PreparedEnvironment -> ResolvedContext HType
                   -> Either String [(HSymbol, HType)]
instantiateContext prepared context = do
    arguments <- mapM projectArgument $ resolvedArguments context
    instantiateContextMethods prepared context arguments project
  where
    projectArgument source = first
        (("internal checked context projection failed: " ++) . show) $
        toSynthesisType source

    project label methodType = first
        (\failure -> label ++ "sealed method projection failed: "
            ++ show failure)
        $ fromSynthesisType methodType

-- Native shared-type context validation used by the Djex adapter. The raw
-- 'Context' operations above remain exact compatibility projections that can
-- inspect methods; ordinary search keeps only the goal and class arguments in
-- the common type tree through kind checking and synonym elaboration.
type SynthesisContext = Constraint (SharedType.Type HSymbol)

type ResolvedSynthesisContext =
    ResolvedContext (SharedType.Type HSymbol)

resolveSynthesisQueryContexts
    :: PreparedEnvironment
    -> (String, HKind, SharedType.Type HSymbol)
    -> [SynthesisContext]
    -> Either DjinnQueryError (SharedType.Type HSymbol)
resolveSynthesisQueryContexts prepared goalObligation contexts = do
    resolved <- queryFailure $
        mapM (lookupResolvedContext prepared) contexts
    let obligations = goalObligation :
            concatMap synthesisArgumentObligations resolved
    queryFailure $ checkSynthesisKindObligations prepared obligations
    elaborated <- queryFailure $ elaboratePreparedSynthesisTypes prepared
        [(kind, source) | (_, kind, source) <- obligations]
    case elaborated of
        [] -> internalQueryFailure "query elaboration dropped its goal"
        elaboratedGoal : elaboratedArguments
            | length elaboratedArguments == length obligations - 1 ->
                return elaboratedGoal
            | otherwise -> internalQueryFailure
                "query elaboration changed the context-argument batch shape"
  where
    queryFailure = first DjinnQueryFailure

synthesisArgumentObligations
    :: ResolvedSynthesisContext
    -> [(String, HKind, SharedType.Type HSymbol)]
synthesisArgumentObligations context =
    [ ( "argument " ++ renderSynthesisType argument ++ " of class " ++
          resolvedName context
      , kind
      , argument
      )
    | ((_, kind), argument) <- zip
        (resolvedParameters context)
        (resolvedArguments context)
    ]

checkSynthesisKindObligations
    :: PreparedEnvironment
    -> [(String, HKind, SharedType.Type HSymbol)]
    -> Either String ()
checkSynthesisKindObligations prepared =
    checkKindObligationsWith $
        checkPreparedSynthesisTypesKinds prepared

-- Shared instantiate-and-kind-check step for one resolved context. Each
-- method's non-class variables are implicitly quantified by that signature,
-- not shared with identically spelled variables in sibling methods; checking
-- one synthetic tuple would accidentally reunify them. The raw path
-- afterwards projects each checked method back to 'HType', while the native
-- path elaborates its synonyms; both attach the same historical
-- method-of-class label to their failures.
instantiateContextMethods
    :: PreparedEnvironment
    -> ResolvedContext argument
    -> [SharedType.Type HSymbol]
    -> (String -> SharedType.Type HSymbol -> Either String result)
    -> Either String [(HSymbol, result)]
instantiateContextMethods prepared context arguments finalize = do
    instantiated <- mapM instantiate $ resolvedMethods context
    mapM checkAndFinalize instantiated
  where
    instantiate (methodName, methodType) = do
        instantiatedType <- instantiateSynthesisMethod
            (resolvedParameters context) arguments methodType
        methodSymbol <- synthesisMethodSymbol methodName
        return (methodSymbol, instantiatedType)

    checkAndFinalize (methodSymbol, methodType) = do
        let label = methodLabel methodSymbol
        case checkPreparedSynthesisTypesKinds
                prepared [(KStar, methodType)] of
            Left message -> Left $ label ++ message
            Right () -> Right ()
        result <- finalize label methodType
        return (methodSymbol, result)

    methodLabel methodSymbol =
        "method " ++ prHSymbolOp methodSymbol ++ " of class " ++
        resolvedName context ++ ": "

-- Preserve Djinn's implicit method-local quantifier semantics on the common
-- tree. A local is renamed only when an active class-argument substitution
-- would otherwise capture its spelling. Both stable and compatibility
-- context resolution use this single substitution implementation.
instantiateSynthesisMethod
    :: [(HSymbol, HKind)]
    -> [SharedType.Type HSymbol]
    -> SharedType.Type HSymbol
    -> Either String (SharedType.Type HSymbol)
instantiateSynthesisMethod parameters arguments methodType = do
    renamed <- substitute renamings methodType
    substitute substitution renamed
  where
    parameterNames = map fst parameters
    substitution = zip parameterNames arguments
    methodVariables =
        SharedType.freeVariablesInFirstOccurrenceOrder methodType
    activeImages =
        [ argument
        | (parameter, argument) <- substitution
        , parameter `elem` methodVariables
        ]
    imageVariables = Set.fromList $ concatMap
        SharedType.freeVariablesInFirstOccurrenceOrder activeImages
    localVariables = filter (`notElem` parameterNames) methodVariables
    capturedLocals = filter (`Set.member` imageVariables) localVariables
    initiallyUnavailable = Set.fromList $
        parameterNames ++ methodVariables ++
        concatMap SharedType.freeVariablesInFirstOccurrenceOrder arguments
    (_, renamings) = mapAccumL allocateRenaming
        initiallyUnavailable capturedLocals

    allocateRenaming unavailable variable =
        let (fresh, unavailable') = freshPrimedVariable unavailable variable
        in ( unavailable'
           , (variable, SharedType.TypeVariable fresh)
           )

    substitute replacements = first show .
        SharedType.substituteTypeVariables freshTypeVariable Set.empty
            (Map.fromList replacements)

    freshTypeVariable unavailable variable =
        Just $ fst $ freshPrimedVariable unavailable variable

-- | Search breadth, ranking, result-limit, and fuel controls for one Djinn
-- query. Validation occurs at the checked query boundary before proof search.
data QueryOptions = QueryOptions {
    -- | Collect alternative solutions beyond the first.
    optionAlternatives :: Bool,
    -- | Rank solutions by the fraction of unused binders, then binder
    -- count; implies collecting alternatives.
    optionSorted :: Bool,
    -- | Maximum number of candidate proofs considered across all formula
    -- plans (positive). Observing one more proof reports
    -- 'SharedSearch.CandidateLimitReached'.
    optionCutoff :: Int,
    -- | Choice-point budget; 'Nothing' keeps the search a complete
    -- decision procedure.
    optionBudget :: Maybe Integer
    }
    deriving (Eq, Show)

-- | Prefer the best solution after considering up to 200 candidates, with
-- no choice-point budget.
defaultQueryOptions :: QueryOptions
defaultQueryOptions = QueryOptions {
    optionAlternatives = False,
    optionSorted = True,
    optionCutoff = 200,
    optionBudget = Nothing
    }

-- | Ranking information retained with every checked Djinn candidate.
--
-- The historical compatibility API orders alternatives by the fraction of
-- unused binders and then by total binder count.  Keeping both inputs here
-- makes that backend-specific judgement inspectable without coupling the
-- shared candidate boundary to Djinn's policy.  The count is arbitrary-
-- precision because it is both observable metadata and the secondary rank.
data DjinnCandidateDetails = DjinnCandidateDetails {
    djinnUnusedBinderFraction :: Rational,
    djinnBinderCount :: Natural
    }
    deriving (Eq, Ord, Show)

-- | A checked Djinn result in the backend-neutral candidate envelope.
-- Djinn proves closed obligations, so its residual-constraint list is empty.
-- Give that invariantly empty slot the shared source-type representation at
-- construction time, so checked adapters can consume candidates verbatim.
type DjinnCandidate =
    SharedCandidate.Candidate (SharedType.Type HSymbol) DjinnCandidateDetails
        (SharedGenerated.FunctionClause HSymbol)

-- | Djinn-specific explanatory data retained in the shared result batch.
-- Formula translation and the first explored proof are diagnostics rather
-- than operational search status or logical evidence.
data DjinnQueryMetadata = DjinnQueryMetadata {
    djinnTranslatedFormula :: String,
    djinnFirstExploredProof :: Maybe String
    }
    deriving (Eq, Show)

-- | Canonical structured result of one Djinn proof search.  The proof core
-- constructs the shared envelope directly; checked adapters consume this
-- value without unpacking and rebuilding an intermediate report.
type DjinnResult =
    SharedQuery.QueryResult DjinnQueryMetadata DjinnCandidate

-- | Failure from the canonical checked result path. Invalid controls,
-- ordinary input rejection, internal proof/projection failures, and an
-- impossible evidence/candidate mismatch remain distinguishable for
-- structured adapters; compatibility projections retain historical text.
data DjinnQueryError
    = DjinnQueryOptionsFailure DjinnQueryOptionsError
    | DjinnQueryFailure String
    | DjinnInstantiationCandidateFailure String
    | DjinnInstantiationAssignmentFailure String
    | DjinnInternalQueryFailure String
    | DjinnResultInvariantFailure SharedQuery.QueryResultInvariantError
    deriving (Eq, Show)

-- | Invalid controls rejected before proof search. Keeping the supplied value
-- makes the stable adapter able to classify options independently while the
-- historical string API can retain its exact message.
data DjinnQueryOptionsError
    = NonPositiveCandidateCutoff Int
    | NegativeChoicePointBudget Integer
    deriving (Eq, Show)

-- | Historical structured report retained only at the compatibility edge.
-- Proof search itself constructs 'DjinnResult'; this DTO is projected from
-- that checked value for callers of 'inhabitGenerated'.
data GeneratedQueryReport = GeneratedQueryReport {
    generatedReportFormula :: String,
    generatedReportProof :: Maybe String,
    generatedReportCompletion :: SharedSearch.Completion,
    generatedReportCandidates :: [DjinnCandidate],
    generatedReportEvidence :: SharedQuery.QueryEvidence
    }
    deriving (Eq, Show)

data QueryOutcome
    -- | Rendered Haskell clauses, best candidate first, de-duplicated.
    = Realized [String]
    -- | The search space was exhausted: no total inhabitant exists.
    | Unrealizable
    -- | The only assumption that could realize the goal is the target
    -- name itself, which would print as general recursion.
    | UnrealizableWithoutSelfReference
    -- | The search budget expired or formula coverage remained incomplete;
    -- inhabitation is undecided.
    | Undecided
    deriving (Eq, Show)

data QueryReport = QueryReport {
    -- | The intuitionistic formula the goal type translated to.
    reportFormula :: String,
    -- | The first internal proof term, when one was found.
    reportProof :: Maybe String,
    -- | Whether the configured proof exploration finished normally or stopped
    -- at its choice-point or candidate bound. Logical negative evidence
    -- remains in 'reportOutcome' rather than being conflated with this status.
    reportCompletion :: SharedSearch.Completion,
    -- | Scope-checked backend-independent candidates. The rendered
    -- compatibility strings in 'Realized' are derived from these values.
    reportGeneratedClauses ::
        [SharedGenerated.FunctionClause HSymbol],
    reportOutcome :: QueryOutcome
    }
    deriving (Show)

-- | Compatibility search that renders every canonical result as Haskell.
--
-- The target name is the defined identifier in the rendered clauses; a
-- same-named function assumption is excluded from the search rather than
-- rendered as recursion.  Contexts are resolved through the environment's
-- classes for arity and kind validation, but their methods do not become
-- proof premises.  'Left' reports invalid input or an internal rendering
-- failure, never an unprovable goal — that is 'Unrealizable'.
inhabit :: QueryOptions -> Environment -> [Context] -> HSymbol -> HType
        -> Either String QueryReport
inhabit options environment contexts name goal = do
    generated <- inhabitGenerated options environment contexts name goal
    -- Rendering belongs only to this compatibility layer.  Canonical callers
    -- retain the scope-checked clause and choose their own presentation policy.
    renderedClauses <- labeled "cannot render generated clause" $
        mapM (renderGeneratedClause . SharedCandidate.candidateOutput) $
            generatedReportCandidates generated
    return QueryReport {
        reportFormula = generatedReportFormula generated,
        reportProof = generatedReportProof generated,
        reportCompletion = generatedReportCompletion generated,
        reportGeneratedClauses =
            map SharedCandidate.candidateOutput $
                generatedReportCandidates generated,
        reportOutcome = evidenceOutcome
            (generatedReportEvidence generated) renderedClauses
        }
  where
    labeled what = either (Left . ((what ++ ": ") ++)) Right

-- | Canonical search for checked structured candidates without committing to
-- a rendering policy. The exact checked target becomes the candidate clause
-- name and the proof core constructs the shared result envelope directly.
--
-- Across all candidate-producing formula plans, at most @optionCutoff + 1@
-- proofs are observed. The extra observation is solely a truncation witness:
-- when present, neither the remaining proof stream nor 'searchExhausted' is
-- forced. When absent, reaching the end of the prefix has already established
-- whether proof search finished or spent its choice-point budget.
inhabitResult
    :: QueryOptions
    -> Environment
    -> [Context]
    -> SharedGenerated.DefinitionName
    -> HType
    -> Either DjinnQueryError DjinnResult
inhabitResult options environment contexts target goal = do
    first DjinnQueryOptionsFailure $ validateQueryOptions options
    prepared <- first DjinnQueryFailure $
        prepareCompatibilityEnvironment environment
    inhabitResultPreparedChecked options prepared contexts target goal

-- | Search using a sealed environment.  The context and goal obligations share
-- the cached kind and synonym assumptions prepared with the session; this
-- function never reconstructs them from 'envTypes'.
inhabitResultPrepared
    :: QueryOptions
    -> PreparedEnvironment
    -> [Context]
    -> SharedGenerated.DefinitionName
    -> HType
    -> Either DjinnQueryError DjinnResult
inhabitResultPrepared options prepared contexts target goal = do
    first DjinnQueryOptionsFailure $ validateQueryOptions options
    inhabitResultPreparedChecked options prepared contexts target goal

-- | Search a sealed Djinn environment while keeping the query in Djex's
-- common type representation through validation, kind checking, synonym
-- elaboration, and formula compilation. The
-- native path never crosses even the zero-copy historical 'HType' view.
inhabitSynthesisResultPrepared
    :: QueryOptions
    -> PreparedEnvironment
    -> [Constraint (SharedType.Type HSymbol)]
    -> SharedGenerated.DefinitionName
    -> SharedType.Type HSymbol
    -> Either DjinnQueryError DjinnResult
inhabitSynthesisResultPrepared options prepared contexts target goal =
    inhabitSynthesisResultPreparedWithInstantiationCandidates
        options prepared contexts [] target goal

-- | Search a sealed Djinn environment with externally established,
-- provider-local type choices. Each association is checked against the exact
-- prepared provider and type/kind/synonym scope before it can add a separate
-- proof-producing search tail. The ordinary entrance above is the exact empty
-- evidence case.
inhabitSynthesisResultPreparedWithInstantiationCandidates
    :: QueryOptions
    -> PreparedEnvironment
    -> [Constraint (SharedType.Type HSymbol)]
    -> [SharedQuery.ProviderInstantiationCandidate HSymbol]
    -> SharedGenerated.DefinitionName
    -> SharedType.Type HSymbol
    -> Either DjinnQueryError DjinnResult
inhabitSynthesisResultPreparedWithInstantiationCandidates options prepared
        contexts candidates target goal = do
    first DjinnQueryOptionsFailure $ validateQueryOptions options
    -- Resolution owns the observable query-failure order: class lookup and
    -- arity precede type inspection, while synonym saturation precedes full
    -- structural validation and kind inference. Elaboration returns the
    -- canonical alias-free types consumed at the formula boundary.
    inhabitSynthesisResultPreparedChecked
        options prepared contexts candidates
        (InferredProviderInstantiationAssignments []) target goal

-- | Search a sealed Djinn environment with externally established complete
-- leading-binder assignments for exact providers.  Each ordered vector is
-- validated and consumed directly, without reconstructing candidate tuples.
-- The historical and candidate entrances remain the exact empty-assignment
-- cases of the common checked worker.
inhabitSynthesisResultPreparedWithInstantiationAssignments
    :: QueryOptions
    -> PreparedEnvironment
    -> [Constraint (SharedType.Type HSymbol)]
    -> [SharedQuery.ProviderInstantiationAssignment HSymbol]
    -> SharedGenerated.DefinitionName
    -> SharedType.Type HSymbol
    -> Either DjinnQueryError DjinnResult
inhabitSynthesisResultPreparedWithInstantiationAssignments options prepared
        contexts assignments target goal = do
    first DjinnQueryOptionsFailure $ validateQueryOptions options
    inhabitSynthesisResultPreparedChecked
        options prepared contexts []
        (InferredProviderInstantiationAssignments assignments) target goal

-- | Search a sealed Djinn environment with complete assignments whose exact
-- leading-binder ground kinds were retained by the caller. Supplied kinds are
-- checked against every occurrence in the provider body; unlike the
-- compatibility entrance above, an otherwise vacuous binder need not default
-- to @Type@.
inhabitSynthesisResultPreparedWithKindedInstantiationAssignments
    :: QueryOptions
    -> PreparedEnvironment
    -> [Constraint (SharedType.Type HSymbol)]
    -> [SharedQuery.KindedProviderInstantiationAssignment HSymbol]
    -> SharedGenerated.DefinitionName
    -> SharedType.Type HSymbol
    -> Either DjinnQueryError DjinnResult
inhabitSynthesisResultPreparedWithKindedInstantiationAssignments options
        prepared contexts assignments target goal = do
    first DjinnQueryOptionsFailure $ validateQueryOptions options
    inhabitSynthesisResultPreparedChecked
        options prepared contexts []
        (KindedProviderInstantiationAssignments assignments) target goal

-- | Compatibility projection of 'inhabitResult'.
inhabitGenerated :: QueryOptions -> Environment -> [Context] -> HSymbol -> HType
                 -> Either String GeneratedQueryReport
inhabitGenerated options environment contexts name goal = do
    target <- validateGeneratedQueryTarget name
    result <- first renderDjinnQueryError $
        inhabitResult options environment contexts target goal
    resultToGeneratedReport result

-- | Compatibility projection of 'inhabitResultPrepared'.
inhabitGeneratedPrepared
    :: QueryOptions -> PreparedEnvironment -> [Context] -> HSymbol -> HType
    -> Either String GeneratedQueryReport
inhabitGeneratedPrepared options prepared contexts name goal = do
    target <- validateGeneratedQueryTarget name
    result <- first renderDjinnQueryError $
        inhabitResultPrepared options prepared contexts target goal
    resultToGeneratedReport result

validateGeneratedQueryTarget
    :: HSymbol
    -> Either String SharedGenerated.DefinitionName
validateGeneratedQueryTarget name = do
    requireName "target" (isDjinnDeclarationName MethodOwner) name
    rawName <- first show $ SharedName.parseName name
    first show $ SharedGenerated.mkDefinitionName rawName

validateQueryOptions :: QueryOptions -> Either DjinnQueryOptionsError ()
validateQueryOptions options = do
    unless (optionCutoff options > 0) $
        Left $ NonPositiveCandidateCutoff $ optionCutoff options
    case optionBudget options of
        Just n | n < 0 -> Left $ NegativeChoicePointBudget n
        _ -> Right ()

inhabitSynthesisResultPreparedChecked
    :: QueryOptions
    -> PreparedEnvironment
    -> [Constraint (SharedType.Type HSymbol)]
    -> [SharedQuery.ProviderInstantiationCandidate HSymbol]
    -> ProviderInstantiationAssignmentEvidence
    -> SharedGenerated.DefinitionName
    -> SharedType.Type HSymbol
    -> Either DjinnQueryError DjinnResult
inhabitSynthesisResultPreparedChecked options prepared contexts candidates
        assignmentEvidence target goal = do
    elaboratedGoal <- resolveSynthesisQueryContexts prepared
        ( "goal type " ++ renderSynthesisType goal
        , KStar
        , goal
        )
        contexts
    let translatePlans =
            preparedEnvironmentPolarizedSynthesisFormulaPlans prepared
        translateNominalPlans =
            preparedEnvironmentNominalPolarizedSynthesisFormulaPlans prepared
        translateType translator label source =
            first (label ++) $ translator source
    -- Contexts have been checked against the same inventory and kind scope as
    -- the goal, but their methods are intentionally absent from proof search.
    -- A class method is polymorphic in its method-local variables; translating
    -- it to one monomorphic LJT premise made alpha-equivalent signatures
    -- depend on source spelling. Djinn supports the sound, useful subset in
    -- which the synthesized term does not require a class method.
    plans <- translatorFailure $
        translateType translatePlans "goal type: " elaboratedGoal
    let parametricDataRelevant =
            preparedEnvironmentQueryUsesParametricData prepared elaboratedGoal
    nominalPlans <- if parametricDataRelevant
        then translatorFailure $
            translateType translateNominalPlans
                "nominal goal type: " elaboratedGoal
        else Right plans
    checkedCandidates <- prepareProviderInstantiationCandidates
        prepared candidates
    checkedAssignments <- prepareProviderInstantiationAssignments
        prepared assignmentEvidence
    searchPreparedFormula options prepared checkedCandidates checkedAssignments target
        elaboratedGoal
        parametricDataRelevant
        plans nominalPlans
  where
    translatorFailure = first DjinnInternalQueryFailure

inhabitResultPreparedChecked
    :: QueryOptions
    -> PreparedEnvironment
    -> [Context]
    -> SharedGenerated.DefinitionName
    -> HType
    -> Either DjinnQueryError DjinnResult
inhabitResultPreparedChecked options prepared contexts target goal = do
    -- Preserve the historical raw-API precedence before crossing the
    -- compatibility boundary: class lookup and arity errors precede errors in
    -- the goal, while synonym saturation precedes structural conversion.
    -- The native worker repeats these cheap checks against the shared class
    -- index, then owns elaboration, method instantiation, and proof search.
    resolved <- first DjinnQueryFailure $
        mapM (lookupResolvedContext prepared) contexts
    first DjinnQueryFailure $ checkKindObligations prepared $
        ("goal type " ++ show goal, KStar, goal) :
        concatMap argumentObligations resolved
    sharedGoal <- projectGoal goal
    sharedContexts <- projectContexts contexts
    inhabitSynthesisResultPreparedChecked
        options prepared sharedContexts []
        (InferredProviderInstantiationAssignments []) target sharedGoal
  where
    projectGoal source = case toSynthesisType source of
        Right projected -> Right projected
        Left failure -> internalQueryFailure $
            "checked raw goal projection failed: " ++ show failure

    projectContexts sources = case mapM (traverse toSynthesisType) sources of
        Right projected -> Right projected
        Left failure -> internalQueryFailure $
            "checked raw context projection failed: " ++ show failure

type PreparedProviderInstantiationCandidate =
    ( Symbol
    , SharedType.Type HSymbol
    , SharedGenerated.VisibleTypeArgument
    )

type PreparedProviderInstantiationAssignment =
    ( Symbol
    , [( SharedType.Type HSymbol
       , SharedGenerated.VisibleTypeArgument
       )]
    )

data ProviderInstantiationAssignmentEvidence
    = InferredProviderInstantiationAssignments
        [SharedQuery.ProviderInstantiationAssignment HSymbol]
    | KindedProviderInstantiationAssignments
        [SharedQuery.KindedProviderInstantiationAssignment HSymbol]

data ProviderInstantiationAssignmentInput
    = InferredProviderInstantiationAssignment
        (SharedQuery.ProviderInstantiationAssignment HSymbol)
    | KindedProviderInstantiationAssignment
        (SharedQuery.KindedProviderInstantiationAssignment HSymbol)

-- Validate external provider evidence only after the ordinary goal and
-- contexts have crossed their established request boundary. The list spine is
-- bounded before any element is entered, making cyclic caller-built evidence
-- a finite query failure. Types are then elaborated independently in source
-- order so one provider's binders cannot share kind variables with another;
-- closedness makes that independence semantic rather than merely diagnostic.
prepareProviderInstantiationCandidates
    :: PreparedEnvironment
    -> [SharedQuery.ProviderInstantiationCandidate HSymbol]
    -> Either DjinnQueryError [PreparedProviderInstantiationCandidate]
prepareProviderInstantiationCandidates prepared rawCandidates = do
    let maximumCandidates =
            SharedQuery.maximumProviderInstantiationCandidates
        observed = SharedCollection.observedListLength
            maximumCandidates rawCandidates
    if observed > maximumCandidates
        then Left $ DjinnInstantiationCandidateFailure $
            "provider instantiation candidate count exceeds " ++
                show maximumCandidates
        else return ()
    (_, retained) <- foldM validateCandidate (Map.empty, []) $
        zip [0 :: Int ..] rawCandidates
    return $ reverse retained
  where
    (loadedSchemes, nominalLoadedSchemes, _) =
        preparedEnvironmentLoadedFunctionInstantiation prepared
    loadedProviders = Set.fromList $
        map fst loadedSchemes ++ map fst nominalLoadedSchemes

    validateCandidate (seen, retained) (index, candidate) = do
        let providerName =
                SharedQuery.providerInstantiationCandidateProvider candidate
            source = SharedQuery.providerInstantiationCandidateType candidate
            label = "provider instantiation candidate #" ++ show index ++
                " for " ++ SharedName.renderCanonical providerName ++ ": "
        providerSpelling <- first
            (DjinnInstantiationCandidateFailure . (label ++)) $
            synthesisFunctionSymbol providerName
        let provider = Symbol providerSpelling
        if provider `Set.member` loadedProviders
            then return ()
            else Left $ DjinnInstantiationCandidateFailure $ label ++
                "provider is not an exact loaded polymorphic value"
        elaborated <- first
            (DjinnInstantiationCandidateFailure . (label ++)) $
            elaboratePreparedSynthesisTypes prepared [(KStar, source)]
        checked <- case elaborated of
            [one] -> Right one
            _ -> Left $ DjinnInternalQueryFailure $
                "provider candidate elaboration changed batch shape"
        unless (Set.null $ SharedType.freeVariables checked) $
            Left $ DjinnInstantiationCandidateFailure $
                label ++ "type is not closed"
        unless (null $ SharedType.typeConstraints checked) $
            Left $ DjinnInstantiationCandidateFailure $
                label ++ "type is not context-free"
        visible <- first
            (DjinnInstantiationCandidateFailure . (label ++) . show) $
            SharedGenerated.specifiedVisibleTypeArgument checked
        let key = SharedTypeAtom.alphaTypeKey checked
            providerKeys = Map.findWithDefault Set.empty provider seen
        if key `Set.member` providerKeys
            then Right (seen, retained)
            else Right
                ( Map.insert provider (Set.insert key providerKeys) seen
                , (provider, checked, visible) : retained
                )

-- Bound both list spines before entering caller-owned argument values.  Once
-- an assignment is known to have the exact provider arity, either infer its
-- binder kinds from the retained provider body or validate the caller's exact
-- positional kinds against that body, then elaborate each paired argument and
-- retain its visible syntax. The complete substituted provider body is
-- kind-checked as a final independent specialization guard.
-- Alpha-equivalent vectors are de-duplicated per provider without losing the
-- order or correlation within a vector.
prepareProviderInstantiationAssignments
    :: PreparedEnvironment
    -> ProviderInstantiationAssignmentEvidence
    -> Either DjinnQueryError [PreparedProviderInstantiationAssignment]
prepareProviderInstantiationAssignments prepared evidence = do
    let maximumAssignments =
            SharedQuery.maximumProviderInstantiationAssignments
        rawAssignments = case evidence of
            InferredProviderInstantiationAssignments assignments ->
                map InferredProviderInstantiationAssignment assignments
            KindedProviderInstantiationAssignments assignments ->
                map KindedProviderInstantiationAssignment assignments
        observed = SharedCollection.observedListLength
            maximumAssignments rawAssignments
    if observed > maximumAssignments
        then Left $ DjinnInstantiationAssignmentFailure $
            "provider instantiation assignment count exceeds " ++
                show maximumAssignments
        else return ()
    (_, _, retained) <- foldM validateAssignment
        (Map.empty, Map.empty, []) $
        zip [0 :: Int ..] rawAssignments
    return $ reverse retained
  where
    maximumArguments = SharedQuery.maximumProviderInstantiationArguments
    (loadedSchemes, _, _) =
        preparedEnvironmentLoadedFunctionInstantiation prepared
    loadedSchemeSources = Map.fromList
        [ (provider, source)
        | (provider, formula) <- loadedSchemes
        , PVar opaque <- [formula]
        , Just source <- [opaqueSymbolSource opaque]
        ]

    validateAssignment
            (seenKinds, seenArguments, retained) (index, assignment) = do
        let (providerName, observedArguments, suppliedKinds, arguments) =
                case assignment of
                    InferredProviderInstantiationAssignment unkinded ->
                        let rawArguments =
                                SharedQuery.providerInstantiationAssignmentArguments
                                    unkinded
                        in
                        ( SharedQuery.providerInstantiationAssignmentProvider
                            unkinded
                        , SharedCollection.observedListLength
                            maximumArguments rawArguments
                        , Nothing
                        , rawArguments
                        )
                    KindedProviderInstantiationAssignment kinded ->
                        let rawArguments =
                                SharedQuery.kindedProviderInstantiationAssignmentArguments
                                    kinded
                        in
                        ( SharedQuery.kindedProviderInstantiationAssignmentProvider
                            kinded
                        , SharedCollection.observedListLength
                            maximumArguments rawArguments
                        , Just $ map fst rawArguments
                        , map snd rawArguments
                        )
            assignmentLabel =
                "provider instantiation assignment #" ++ show index ++ ": "
        if observedArguments > maximumArguments
            then Left $ DjinnInstantiationAssignmentFailure $
                assignmentLabel ++ "argument count exceeds " ++
                    show maximumArguments
            else return ()
        let providerLabel = assignmentLabel ++ "provider " ++
                SharedName.renderCanonical providerName ++ ": "
        providerSpelling <- first
            (DjinnInstantiationAssignmentFailure . (providerLabel ++)) $
            synthesisFunctionSymbol providerName
        let provider = Symbol providerSpelling
        schemeSource <- case Map.lookup provider loadedSchemeSources of
            Just source -> Right source
            Nothing -> Left $ DjinnInstantiationAssignmentFailure $
                providerLabel ++
                    "provider is not an exact loaded polymorphic value"
        let (binders, schemeConstraints, schemeBody) =
                SharedType.splitLeadingForalls schemeSource
            arity = length binders
        unless (null schemeConstraints) $
            Left $ DjinnInstantiationAssignmentFailure $
                providerLabel ++ "provider scheme is not context-free"
        unless (arity >= 1 && arity <= maximumArguments) $
            Left $ DjinnInstantiationAssignmentFailure $
                providerLabel ++ "leading forall arity " ++ show arity ++
                    " is outside the supported range 1.." ++
                    show maximumArguments
        unless (observedArguments == arity) $
            Left $ DjinnInstantiationAssignmentFailure $
                providerLabel ++ "expected " ++ show arity ++
                    " ordered argument(s), but received " ++
                    show observedArguments
        let kindAssumptions = SharedInventory.inventoryKindAssumptions $
                preparedEnvironmentInventory prepared
        groundBinderKinds <- case suppliedKinds of
            Nothing -> map snd <$> first
                (DjinnInstantiationAssignmentFailure .
                    (providerLabel ++) .
                    ("cannot infer provider binder kinds: " ++) . show)
                (SharedKindInference.inferSharedVariableKinds
                    kindAssumptions binders [schemeBody])
            Just suppliedGroundKinds -> do
                mapM_ (validateSuppliedKind providerLabel) $
                    zip [0 :: Int ..] suppliedGroundKinds
                first
                    (DjinnInstantiationAssignmentFailure .
                        (providerLabel ++) .
                        (("supplied provider binder kinds conflict with the " ++
                            "retained body: ") ++) . show) $
                    SharedKindInference.checkTypesKinds kindAssumptions $
                        (SharedKind.ProperTypeKind, schemeBody) :
                        zip suppliedGroundKinds
                            (map SharedType.TypeVariable binders)
                return suppliedGroundKinds
        case Map.lookup provider seenKinds of
            Just previousKinds | previousKinds /= groundBinderKinds ->
                Left $ DjinnInstantiationAssignmentFailure $
                    providerLabel ++
                        "binder kind vector conflicts with another " ++
                        "assignment for this provider; expected " ++
                        show previousKinds ++ ", but received " ++
                        show groundBinderKinds
            _ -> return ()
        let binderKinds = map fromGroundHKind groundBinderKinds
        checkedArguments <- mapM
            (validateArgument providerLabel) $
                zip3 [0 :: Int ..] binderKinds arguments
        instantiatedBody <- first
            (DjinnInstantiationAssignmentFailure .
                (providerLabel ++) .
                ("cannot instantiate provider body: " ++) . show) $
            SharedType.substituteTypeVariables freshTypeVariable Set.empty
                (Map.fromList $ zip binders $ map fst checkedArguments)
                schemeBody
        first (DjinnInstantiationAssignmentFailure .
                (providerLabel ++) .
                ("instantiated provider body is ill-kinded: " ++)) $
            checkPreparedSynthesisTypesKinds prepared [(KStar, instantiatedBody)]
        let key = map (SharedTypeAtom.alphaTypeKey . fst) checkedArguments
            providerKeys = Map.findWithDefault
                Set.empty provider seenArguments
            retainedKinds = Map.insert provider groundBinderKinds seenKinds
        if key `Set.member` providerKeys
            then Right (retainedKinds, seenArguments, retained)
            else Right
                ( retainedKinds
                , Map.insert provider (Set.insert key providerKeys)
                    seenArguments
                , (provider, checkedArguments) : retained
                )

    validateSuppliedKind providerLabel (argumentIndex, kind) =
        let maximumNodes =
                SharedQuery.maximumProviderInstantiationKindNodes
            observedNodes =
                SharedKind.observedKindNodeCount maximumNodes kind
            label = providerLabel ++ "argument #" ++
                show argumentIndex ++ ": "
        in unless (observedNodes <= maximumNodes) $
            Left $ DjinnInstantiationAssignmentFailure $
                label ++ "supplied ground kind node count exceeds " ++
                    show maximumNodes ++ " (observed " ++
                    show observedNodes ++ ")"

    validateArgument providerLabel (argumentIndex, binderKind, source) = do
        let label = providerLabel ++ "argument #" ++
                show argumentIndex ++ ": "
        elaborated <- first
            (DjinnInstantiationAssignmentFailure . (label ++)) $
            elaboratePreparedSynthesisTypes prepared [(binderKind, source)]
        checked <- case elaborated of
            [one] -> Right one
            _ -> Left $ DjinnInternalQueryFailure $
                "provider assignment elaboration changed batch shape"
        unless (Set.null $ SharedType.freeVariables checked) $
            Left $ DjinnInstantiationAssignmentFailure $
                label ++ "type is not closed"
        unless (null $ SharedType.typeConstraints checked) $
            Left $ DjinnInstantiationAssignmentFailure $
                label ++ "type is not context-free"
        visible <- first
            (DjinnInstantiationAssignmentFailure . (label ++) . show) $
            SharedGenerated.specifiedVisibleTypeArgument checked
        return (checked, visible)

    freshTypeVariable unavailable variable =
        Just $ fst $ freshPrimedVariable unavailable variable

-- Proof search is representation-independent once the checked source query
-- has become a formula. Both the native and raw entrances meet at this single
-- worker; validated class contexts add no premises here, while bounded
-- hypothesis-instantiation axioms occupy appended plans under erased
-- evidence.
searchPreparedFormula
    :: QueryOptions
    -> PreparedEnvironment
    -> [PreparedProviderInstantiationCandidate]
    -> [PreparedProviderInstantiationAssignment]
    -> SharedGenerated.DefinitionName
    -> SharedType.Type HSymbol
    -> Bool
    -> PolarizedFormulaPlans
    -> PolarizedFormulaPlans
    -> Either DjinnQueryError DjinnResult
searchPreparedFormula options prepared providerCandidates providerAssignments
        target elaboratedGoal parametricDataRelevant formulaPlans
        nominalFormulaPlans = do
    let (premises, premiseTranslationIncomplete, premiseSpellings) =
            preparedEnvironmentPolarizedFunctionPremises prepared
        (nominalPremises, _, nominalPremiseSpellings) =
            preparedEnvironmentNominalPolarizedFunctionPremises prepared
        ( loadedSchemePremises
          , nominalLoadedSchemePremises
          , environmentClosedCandidates
          ) = preparedEnvironmentLoadedFunctionInstantiation prepared
        targetSymbol = Symbol $ SharedGenerated.definitionSpelling target
        activePremises = filter ((/= targetSymbol) . fst) premises
        targetPremises = filter ((== targetSymbol) . fst) premises
        activeNominalPremises =
            filter ((/= targetSymbol) . fst) nominalPremises
        targetNominalPremises =
            filter ((== targetSymbol) . fst) nominalPremises
        activeLoadedSchemePremises = filter
            ((/= targetSymbol) . fst) loadedSchemePremises
        targetLoadedSchemePremises = filter
            ((== targetSymbol) . fst) loadedSchemePremises
        activeNominalLoadedSchemePremises = filter
            ((/= targetSymbol) . fst) nominalLoadedSchemePremises
        targetNominalLoadedSchemePremises = filter
            ((== targetSymbol) . fst) nominalLoadedSchemePremises
        goalVariables =
            SharedType.freeVariablesInFirstOccurrenceOrder elaboratedGoal
        closedCandidates =
            SharedCollection.distinctOn SharedTypeAtom.alphaTypeKey $
                closedMonotypeSubtrees elaboratedGoal ++
                    environmentClosedCandidates
        checkedTranslator translator source = do
            checkPreparedSynthesisTypesKinds prepared [(KStar, source)]
            translator source
        visibleArgument source = case
                checkPreparedSynthesisTypesKinds prepared [(KStar, source)] of
            Left _ -> Nothing
            Right () -> Just $ either
                (const SharedGenerated.inferredVisibleTypeArgument)
                id
                $ SharedGenerated.specifiedVisibleTypeArgument source
        structuralTranslator = checkedTranslator $
            preparedEnvironmentSynthesisFormulaTranslator prepared
        nominalTranslator = checkedTranslator $
            preparedEnvironmentNominalSynthesisFormulaTranslator prepared
        collectAcrossPlans =
            optionAlternatives options || optionSorted options
        primary = primaryFormulaPlan formulaPlans
        primaryTranslationSound = not $
            translationIncomplete primary || premiseTranslationIncomplete
        primarySound = primaryTranslationSound &&
            null activeLoadedSchemePremises
        alternativeForms
            | primaryTranslationSound = []
            | otherwise =
                exactOpaqueFormulaPlan formulaPlans :
                map translatedFormula
                    (singleOpaqueFormulaPlans formulaPlans) ++
                map translatedFormula
                    (singleOpenFormulaPlans formulaPlans) ++
                map translatedFormula
                    (pairOpaqueFormulaPlans formulaPlans) ++
                map translatedFormula
                    (pairOpenFormulaPlans formulaPlans) ++
                map translatedFormula
                    (tripleOpaqueFormulaPlans formulaPlans) ++
                map translatedFormula
                    (tripleOpenFormulaPlans formulaPlans) ++
                map translatedFormula
                    (quadrupleOpaqueFormulaPlans formulaPlans) ++
                map translatedFormula
                    (quadrupleOpenFormulaPlans formulaPlans)
        rawPlans =
            (translatedFormula primary, primarySound) :
            [ (formula, False)
            | formula <- alternativeForms
            ]
        plans = SharedCollection.distinctOn fst rawPlans
        nominalPlans = SharedCollection.distinctOn fst
            [ (formula, False)
            | formula <- formulaFamilyForms nominalFormulaPlans
            ]
        nominalProjectionDistinct =
            map fst nominalPlans /= map fst plans ||
                nominalPremises /= premises
        useNominalProjection =
            parametricDataRelevant &&
                not primaryTranslationSound && nominalProjectionDistinct
        -- The candidate spellings are source-level facts: the goal's free
        -- variables, every opened-forall skolem of the goal plans, and the
        -- sealed premise scopes. No rendered atom text is parsed back.
        activeAxioms = instantiationAxioms
            (preparedEnvironmentSynthesisFormulaTranslator prepared)
            visibleArgument
            (goalVariables ++
                polarizedFormulaPlanSkolems formulaPlans ++
                premiseSpellings)
            (map fst plans)
            (map snd activePremises)
        activeAxiomSymbols = instantiationAxiomSymbols activeAxioms
        activeAxiomPremises = instantiationAxiomPremises activeAxioms
        activeVisibleApplications =
            instantiationVisibleApplications activeAxioms
        targetAxioms = instantiationAxioms
            (preparedEnvironmentSynthesisFormulaTranslator prepared)
            visibleArgument
            (goalVariables ++
                polarizedFormulaPlanSkolems formulaPlans ++
                premiseSpellings)
            (map fst plans)
            (map snd targetPremises)
        targetAxiomPremises = instantiationAxiomPremises targetAxioms
        activeNominalAxioms = instantiationAxioms
            (preparedEnvironmentNominalSynthesisFormulaTranslator prepared)
            visibleArgument
            (goalVariables ++
                polarizedFormulaPlanSkolems nominalFormulaPlans ++
                nominalPremiseSpellings)
            (map fst nominalPlans)
            (map snd activeNominalPremises)
        activeNominalAxiomSymbols =
            instantiationAxiomSymbols activeNominalAxioms
        activeNominalAxiomPremises =
            instantiationAxiomPremises activeNominalAxioms
        activeNominalVisibleApplications =
            instantiationVisibleApplications activeNominalAxioms
        targetNominalAxioms = instantiationAxioms
            (preparedEnvironmentNominalSynthesisFormulaTranslator prepared)
            visibleArgument
            (goalVariables ++
                polarizedFormulaPlanSkolems nominalFormulaPlans ++
                nominalPremiseSpellings)
            (map fst nominalPlans)
            (map snd targetNominalPremises)
        targetNominalAxiomPremises =
            instantiationAxiomPremises targetNominalAxioms
        activeLoadedAxioms = loadedInstantiationAxioms
            structuralTranslator
            visibleArgument
            (goalVariables ++
                polarizedFormulaPlanSkolems formulaPlans ++
                premiseSpellings)
            closedCandidates
            (map fst plans)
            (map snd premises)
            (map snd activeLoadedSchemePremises)
        activeLoadedAxiomSymbols =
            instantiationAxiomSymbols activeLoadedAxioms
        activeLoadedAxiomPremises =
            instantiationAxiomPremises activeLoadedAxioms
        activeLoadedVisibleApplications =
            instantiationVisibleApplications activeLoadedAxioms
        targetLoadedAxioms = loadedInstantiationAxioms
            structuralTranslator
            visibleArgument
            (goalVariables ++
                polarizedFormulaPlanSkolems formulaPlans ++
                premiseSpellings)
            closedCandidates
            (map fst plans)
            (map snd premises)
            (map snd targetLoadedSchemePremises)
        targetLoadedAxiomPremises =
            instantiationAxiomPremises targetLoadedAxioms
        activeNominalLoadedAxioms = loadedInstantiationAxioms
            nominalTranslator
            visibleArgument
            (goalVariables ++
                polarizedFormulaPlanSkolems nominalFormulaPlans ++
                nominalPremiseSpellings)
            closedCandidates
            (map fst nominalPlans)
            (map snd nominalPremises)
            (map snd activeNominalLoadedSchemePremises)
        activeNominalLoadedAxiomSymbols =
            instantiationAxiomSymbols activeNominalLoadedAxioms
        activeNominalLoadedAxiomPremises =
            instantiationAxiomPremises activeNominalLoadedAxioms
        activeNominalLoadedVisibleApplications =
            instantiationVisibleApplications activeNominalLoadedAxioms
        targetNominalLoadedAxioms = loadedInstantiationAxioms
            nominalTranslator
            visibleArgument
            (goalVariables ++
                polarizedFormulaPlanSkolems nominalFormulaPlans ++
                nominalPremiseSpellings)
            closedCandidates
            (map fst nominalPlans)
            (map snd nominalPremises)
            (map snd targetNominalLoadedSchemePremises)
        targetNominalLoadedAxiomPremises =
            instantiationAxiomPremises targetNominalLoadedAxioms
        activeProviderInstantiations = providerInstantiationPremises
            "$djinn$provider-instantiation$active$"
            structuralTranslator
            activeLoadedSchemePremises
            providerCandidates
        activeProviderPremises = providerInstantiationPremiseBindings
            activeProviderInstantiations
        activeProviderApplications = providerInstantiationApplications
            activeProviderInstantiations
        activeProviderAssignmentInstantiations =
            providerInstantiationAssignmentPremises
                "$djinn$provider-assignment$active$"
                structuralTranslator
                activeLoadedSchemePremises
                providerAssignments
        activeProviderAssignmentPremises =
            providerInstantiationPremiseBindings
                activeProviderAssignmentInstantiations
        activeProviderAssignmentApplications =
            providerInstantiationApplications
                activeProviderAssignmentInstantiations
        activeAllProviderPremises =
            activeProviderPremises ++ activeProviderAssignmentPremises
        activeAllProviderApplications =
            activeProviderApplications `Map.union`
                activeProviderAssignmentApplications
        activeAllProviderSymbols = Map.keysSet activeAllProviderApplications
        activeAllProviderVisibleApplications =
            Map.map snd activeAllProviderApplications
        activeProviderAssignmentSymbols =
            Map.keysSet activeProviderAssignmentApplications
        activeProviderAssignmentVisibleApplications =
            Map.map snd activeProviderAssignmentApplications
        activeProviderAssignmentNames = Set.fromList $
            map fst $ Map.elems activeProviderAssignmentApplications
        targetProviderInstantiations = providerInstantiationPremises
            "$djinn$provider-instantiation$target$"
            structuralTranslator
            targetLoadedSchemePremises
            providerCandidates
        targetProviderPremises = providerInstantiationPremiseBindings
            targetProviderInstantiations
        targetProviderAssignmentInstantiations =
            providerInstantiationAssignmentPremises
                "$djinn$provider-assignment$target$"
                structuralTranslator
                targetLoadedSchemePremises
                providerAssignments
        targetProviderAssignmentPremises =
            providerInstantiationPremiseBindings
                targetProviderAssignmentInstantiations
        targetAllProviderPremises =
            targetProviderPremises ++ targetProviderAssignmentPremises
        activeNominalProviderInstantiations = providerInstantiationPremises
            "$djinn$nominal-provider-instantiation$active$"
            nominalTranslator
            activeNominalLoadedSchemePremises
            providerCandidates
        activeNominalProviderPremises = providerInstantiationPremiseBindings
            activeNominalProviderInstantiations
        activeNominalProviderApplications = providerInstantiationApplications
            activeNominalProviderInstantiations
        activeNominalProviderAssignmentInstantiations =
            providerInstantiationAssignmentPremises
                "$djinn$nominal-provider-assignment$active$"
                nominalTranslator
                activeNominalLoadedSchemePremises
                providerAssignments
        activeNominalProviderAssignmentPremises =
            providerInstantiationPremiseBindings
                activeNominalProviderAssignmentInstantiations
        activeNominalProviderAssignmentApplications =
            providerInstantiationApplications
                activeNominalProviderAssignmentInstantiations
        activeAllNominalProviderPremises =
            activeNominalProviderPremises ++
                activeNominalProviderAssignmentPremises
        activeAllNominalProviderApplications =
            activeNominalProviderApplications `Map.union`
                activeNominalProviderAssignmentApplications
        activeAllNominalProviderSymbols =
            Map.keysSet activeAllNominalProviderApplications
        activeAllNominalProviderVisibleApplications =
            Map.map snd activeAllNominalProviderApplications
        activeNominalProviderAssignmentSymbols =
            Map.keysSet activeNominalProviderAssignmentApplications
        activeNominalProviderAssignmentVisibleApplications =
            Map.map snd activeNominalProviderAssignmentApplications
        activeNominalProviderAssignmentNames = Set.fromList $
            map fst $ Map.elems activeNominalProviderAssignmentApplications
        targetNominalProviderInstantiations = providerInstantiationPremises
            "$djinn$nominal-provider-instantiation$target$"
            nominalTranslator
            targetNominalLoadedSchemePremises
            providerCandidates
        targetNominalProviderPremises = providerInstantiationPremiseBindings
            targetNominalProviderInstantiations
        targetNominalProviderAssignmentInstantiations =
            providerInstantiationAssignmentPremises
                "$djinn$nominal-provider-assignment$target$"
                nominalTranslator
                targetNominalLoadedSchemePremises
                providerAssignments
        targetNominalProviderAssignmentPremises =
            providerInstantiationPremiseBindings
                targetNominalProviderAssignmentInstantiations
        targetAllNominalProviderPremises =
            targetNominalProviderPremises ++
                targetNominalProviderAssignmentPremises
        useNominalLoadedProjection =
            parametricDataRelevant &&
                ( useNominalProjection ||
                    activeNominalLoadedSchemePremises /=
                        activeLoadedSchemePremises ||
                    activeNominalLoadedAxiomPremises /=
                        activeLoadedAxiomPremises
                )
        useNominalProviderProjection =
            parametricDataRelevant &&
                ( nominalProjectionDistinct ||
                    activeAllNominalProviderPremises /=
                        activeAllProviderPremises ||
                    targetAllNominalProviderPremises /=
                        targetAllProviderPremises
                )
        -- Preserve the complete historical structural/no-axiom prefix. This
        -- keeps first-result behavior, frontier ordering, and finite-budget
        -- observations stable; the nominal family shares only the cutoff and
        -- fuel left by that prefix and still precedes the structural axiom
        -- phase which previously formed the appended transport tail.
        structuralSearchPlans =
            [ ( premises, [], Set.empty, Map.empty, Map.empty
              , form, sound
              )
            | (form, sound) <- plans
            ]
        structuralAxiomSearchPlans =
            [ ( premises ++ activeAxiomPremises
              , targetAxiomPremises
              , activeAxiomSymbols
              , activeVisibleApplications
              , Map.empty
              , form
              , sound && null activeAxiomPremises
              )
            | not (null activeAxiomPremises) ||
                not (null targetAxiomPremises)
            , (form, sound) <- plans
            ]
        loadedStructuralSearchPlans =
            [ ( premises ++ loadedSchemePremises ++ activeAxiomPremises ++
                    activeLoadedAxiomPremises
              , targetAxiomPremises ++ targetLoadedAxiomPremises
              , activeAxiomSymbols `Set.union` activeLoadedAxiomSymbols
              , activeVisibleApplications `Map.union`
                    activeLoadedVisibleApplications
              , Map.empty
              , form
              , sound && null activeAxiomPremises &&
                    null activeLoadedSchemePremises
              )
            | not (null loadedSchemePremises)
            , (form, sound) <- plans
            ]
        -- The nominal-data family is a complementary proof-producing
        -- approximation, never a refutation. Its premise views and erased
        -- instantiation axioms are compiled independently with the matching
        -- nominal translator, so structural candidate caps and ordering are
        -- unchanged. Pair each plain nominal form with its guarded transport
        -- form before moving to the next frontier; policy remains part of the
        -- search plan even when a formula happens to equal a structural view.
        nominalSearchPlans
            | not useNominalProjection = []
            | otherwise = concatMap nominalFormSearchPlans nominalPlans
        nominalFormSearchPlans (form, _) =
            ( nominalPremises, [], Set.empty, Map.empty, Map.empty
            , form, False
            ) :
            [ ( nominalPremises ++ activeNominalAxiomPremises
              , targetNominalAxiomPremises
              , activeNominalAxiomSymbols
              , activeNominalVisibleApplications
              , Map.empty
              , form
              , False
              )
            | not (null activeNominalAxiomPremises) ||
                not (null targetNominalAxiomPremises)
            ]
        loadedNominalSearchPlans
            | not useNominalLoadedProjection = []
            | otherwise =
                [ ( nominalPremises ++ nominalLoadedSchemePremises ++
                        activeNominalAxiomPremises ++
                        activeNominalLoadedAxiomPremises
                  , targetNominalAxiomPremises ++
                        targetNominalLoadedAxiomPremises
                  , activeNominalAxiomSymbols `Set.union`
                        activeNominalLoadedAxiomSymbols
                  , activeNominalVisibleApplications `Map.union`
                        activeNominalLoadedVisibleApplications
                  , Map.empty
                  , form
                  , False
                  )
                | not (null nominalLoadedSchemePremises)
                , (form, _) <- nominalPlans
                ]
        -- Exact ordered assignments receive one positive-only priority plan.
        -- Replace the ordinary views of each assigned provider in this plan,
        -- then put its exact premise at the tail. LJT introduces folded
        -- environment arrows from left to right: a nominal empty premise
        -- encountered before later arrows is eliminated, while the tail
        -- premise reaches the final matching goal by direct identity and
        -- therefore preserves its visible type application. The unfiltered
        -- provider superset below remains available for proofs which compose
        -- an exact assignment with an ordinary use of that same provider.
        providerAssignmentPriorityStructuralSearchPlans =
            [ ( withoutProviders activeProviderAssignmentNames premises ++
                    withoutProviders activeProviderAssignmentNames
                        loadedSchemePremises ++
                    activeAxiomPremises ++ activeLoadedAxiomPremises ++
                    activeProviderAssignmentPremises
              , []
              , activeAxiomSymbols `Set.union` activeLoadedAxiomSymbols
                    `Set.union` activeProviderAssignmentSymbols
              , activeVisibleApplications `Map.union`
                    activeLoadedVisibleApplications `Map.union`
                    activeProviderAssignmentVisibleApplications
              , activeProviderAssignmentApplications
              , form
              , False
              )
            | not (null activeProviderAssignmentPremises)
            , (form, _) <- plans
            ]
        providerAssignmentPriorityNominalSearchPlans
            | not useNominalProviderProjection = []
            | otherwise =
                [ ( withoutProviders activeNominalProviderAssignmentNames
                        nominalPremises ++
                        withoutProviders activeNominalProviderAssignmentNames
                            nominalLoadedSchemePremises ++
                        activeNominalAxiomPremises ++
                        activeNominalLoadedAxiomPremises ++
                        activeNominalProviderAssignmentPremises
                  , []
                  , activeNominalAxiomSymbols `Set.union`
                        activeNominalLoadedAxiomSymbols `Set.union`
                        activeNominalProviderAssignmentSymbols
                  , activeNominalVisibleApplications `Map.union`
                        activeNominalLoadedVisibleApplications `Map.union`
                        activeNominalProviderAssignmentVisibleApplications
                  , activeNominalProviderAssignmentApplications
                  , form
                  , False
                  )
                | not (null activeNominalProviderAssignmentPremises)
                , (form, _) <- nominalPlans
                ]
        -- The historical combined scalar-candidate and exact-assignment plan
        -- remains an unfiltered additive superset at its established position.
        -- This preserves legacy candidate ordering and allows one proof to use
        -- the same provider through both exact and ordinary instantiation.
        providerStructuralSearchPlans =
            [ ( premises ++ loadedSchemePremises ++
                    activeAxiomPremises ++ activeLoadedAxiomPremises ++
                    activeAllProviderPremises
              , targetAxiomPremises ++ targetLoadedAxiomPremises ++
                    targetAllProviderPremises
              , activeAxiomSymbols `Set.union` activeLoadedAxiomSymbols
                    `Set.union` activeAllProviderSymbols
              , activeVisibleApplications `Map.union`
                    activeLoadedVisibleApplications `Map.union`
                    activeAllProviderVisibleApplications
              , activeAllProviderApplications
              , form
              , False
              )
            | not (null activeAllProviderPremises) ||
                not (null targetAllProviderPremises)
            , (form, _) <- plans
            ]
        providerNominalSearchPlans
            | not useNominalProviderProjection = []
            | otherwise =
                [ ( nominalPremises ++ nominalLoadedSchemePremises ++
                        activeNominalAxiomPremises ++
                        activeNominalLoadedAxiomPremises ++
                        activeAllNominalProviderPremises
                  , targetNominalAxiomPremises ++
                        targetNominalLoadedAxiomPremises ++
                        targetAllNominalProviderPremises
                  , activeNominalAxiomSymbols `Set.union`
                        activeNominalLoadedAxiomSymbols `Set.union`
                        activeAllNominalProviderSymbols
                  , activeNominalVisibleApplications `Map.union`
                        activeNominalLoadedVisibleApplications `Map.union`
                        activeAllNominalProviderVisibleApplications
                  , activeAllNominalProviderApplications
                  , form
                  , False
                  )
                | not (null activeAllNominalProviderPremises) ||
                    not (null targetAllNominalProviderPremises)
                , (form, _) <- nominalPlans
                ]
        searchPlans =
            -- Exact assignment priority is absent for both the scalar-only
            -- and empty-evidence entrances, so their historical plan order is
            -- unchanged byte for byte.
            providerAssignmentPriorityStructuralSearchPlans ++
            providerAssignmentPriorityNominalSearchPlans ++
            structuralSearchPlans ++
            nominalSearchPlans ++
            structuralAxiomSearchPlans ++
            -- A productive historical loaded proof stream can consume the
            -- global candidate cutoff without finishing.  Run its strict
            -- provider-evidence superset first when that optional family
            -- exists; the empty-evidence plan list remains exactly historical.
            providerStructuralSearchPlans ++
            providerNominalSearchPlans ++
            loadedStructuralSearchPlans ++
            loadedNominalSearchPlans
        withoutProviders providerNames =
            filter ((`Set.notMember` providerNames) . fst)
    results <- runPlans collectAcrossPlans
        options (optionCutoff options) [] searchPlans
    mergeFormulaPlanResults options results
  where
    runPlans _ _ _ completed [] = Right $ reverse completed
    runPlans collect currentOptions candidateLimit completed
            (( planPremises
              , diagnosticOnlyPremises
              , axiomSymbols
              , visibleApplications
              , providerApplications
              , form
              , negativeEvidenceSound
              )
                : remaining) = do
        result <- searchPreparedFormulaPlan
            currentOptions candidateLimit target planPremises axiomSymbols
            visibleApplications providerApplications diagnosticOnlyPremises form
            negativeEvidenceSound
        let completed' = result : completed
            nextLimit = candidateLimit - formulaPlanProofCount result
            continue =
                formulaPlanFinished result &&
                (collect || null (formulaPlanClauses result)) &&
                evidenceCanBenefitFromAnotherPlan result
        if continue
            then runPlans collect
                currentOptions {
                    optionBudget = formulaPlanRemainingBudget result
                    }
                nextLimit completed' remaining
            else Right $ reverse completed'

    -- A proof-backed target diagnostic is already the sharpest candidate-free
    -- result. A completed refutation may still be sharpened by a later
    -- target-instantiation plan, but only while finite search fuel remains;
    -- running that optional tail at zero fuel would replace established
    -- logical evidence with an operational truncation.
    evidenceCanBenefitFromAnotherPlan result =
        case formulaPlanEvidence result of
            SharedQuery.RequiresTargetReference -> False
            SharedQuery.ProvedUninhabitable ->
                formulaPlanRemainingBudget result /= Just 0
            _ -> True

    formulaFamilyForms plans = SharedCollection.distinctOn id $
        translatedFormula (primaryFormulaPlan plans) :
        exactOpaqueFormulaPlan plans :
        map translatedFormula (singleOpaqueFormulaPlans plans) ++
        map translatedFormula (singleOpenFormulaPlans plans) ++
        map translatedFormula (pairOpaqueFormulaPlans plans) ++
        map translatedFormula (pairOpenFormulaPlans plans) ++
        map translatedFormula (tripleOpaqueFormulaPlans plans) ++
        map translatedFormula (tripleOpenFormulaPlans plans) ++
        map translatedFormula (quadrupleOpaqueFormulaPlans plans) ++
        map translatedFormula (quadrupleOpenFormulaPlans plans)

-- Goal plans retain their historical linear prefix: the fully opened
-- translation, the exact opaque fallback, one independently opaque positive
-- forall at a time, and one independently opened branch among opaque siblings.
-- Pairwise, triple, and quadruple choices form deterministic polynomial tails.
-- Global premises expose the same sound views simultaneously under
-- distinct internal proof identities, so one term may use different views at
-- different occurrences of a reusable source function. Every proof remains
-- checked against the exact goal formula that produced it.

-- One formula plan retains clauses before cross-plan de-duplication and
-- ranking. 'formulaPlanProofCount' counts raw proofs before either operation,
-- which makes the caller's cutoff global across every translation.
data FormulaPlanResult = FormulaPlanResult
    { formulaPlanFormula :: String
    , formulaPlanFirstProof :: Maybe String
    , formulaPlanCompletion :: SharedSearch.Completion
    , formulaPlanClauses ::
        [SharedGenerated.FunctionClause HSymbol]
    , formulaPlanEvidence :: SharedQuery.QueryEvidence
    , formulaPlanRemainingBudget :: Maybe Integer
    , formulaPlanProofCount :: Int
    }

-- | One proof-search plan. The final flag authorizes logical negative
-- evidence only when translation covered every quantified subtree. Checked
-- proofs remain useful under an incomplete plan, but absence of one does not
-- establish that the original Haskell type is uninhabited. The extra premise
-- list is diagnostic-only: it contains target-derived instantiation axioms
-- or provider specializations which must never enter safe proof search. Other
-- instantiation axioms participate in search and proof checking under their
-- reserved symbols; their evidence is erased only after checking. The second
-- application map retains direct provider-local specializations: proof
-- checking sees their synthetic premise, then generated conversion rewrites a
-- synthetic occurrence through its exact provider before the existing visible
-- type-application lowering runs.
searchPreparedFormulaPlan
    :: QueryOptions
    -> Int
    -> SharedGenerated.DefinitionName
    -> [(Symbol, Formula)]
    -> Set.Set Symbol
    -> Map.Map Symbol [SharedGenerated.VisibleTypeArgument]
    -> Map.Map Symbol
        (Symbol, [SharedGenerated.VisibleTypeArgument])
    -> [(Symbol, Formula)]
    -> Formula
    -> Bool
    -> Either DjinnQueryError FormulaPlanResult
searchPreparedFormulaPlan options candidateLimit target externalEnv
        axiomSymbols visibleApplications providerApplications
        diagnosticOnlyEnv form
        negativeEvidenceSound = do
    let name = SharedGenerated.definitionSpelling target
        proofEnv = prepareProofEnvironment (Symbol name) externalEnv
        internalEnv = proofBindings proofEnv
        mode = (defaultSearchMode
                    (optionAlternatives options || optionSorted options)) {
            searchBudget = optionBudget options
            }
        internalFailure what = first $
            DjinnInternalQueryFailure . ((what ++ ": ") ++)
    outcome <- internalFailure "invalid proof-search environment" $
        proveWithModeChecked mode internalEnv form
    case searchProofs outcome of
        [] -> do
            failure <-
                if searchExhausted outcome then
                    return Undecided
                else if not negativeEvidenceSound then
                    return Undecided
                else if not (targetWasExcluded proofEnv) then
                    return Unrealizable
                else do
                    -- The safe search has already decided that no admissible
                    -- proof exists.  Reintroduce target-named assumptions only
                    -- to justify the sharper diagnostic, spending no more than
                    -- the first search's unused fuel.  Exhaustion here cannot
                    -- make the already-decided safe result 'Undecided'.
                    let diagnosticEnv =
                            proofBindingsIncludingTarget $
                                prepareProofEnvironment (Symbol name) $
                                    externalEnv ++ diagnosticOnlyEnv
                        diagnosticMode = mode {
                            searchAlternatives = False,
                            searchBudget = remainingSearchBudget outcome
                            }
                    diagnosticOutcome <- internalFailure
                        "invalid diagnostic proof-search environment" $
                        proveWithModeChecked diagnosticMode diagnosticEnv form
                    case searchProofs diagnosticOutcome of
                        diagnosticProof : _ -> do
                            internalFailure
                                "generated an invalid self-reference proof" $
                                checkProof diagnosticEnv form diagnosticProof
                            return UnrealizableWithoutSelfReference
                        [] -> return Unrealizable
            return FormulaPlanResult {
                formulaPlanFormula = show form,
                formulaPlanFirstProof = Nothing,
                formulaPlanCompletion = queryCompletion outcome,
                formulaPlanClauses = [],
                formulaPlanEvidence = outcomeEvidence failure,
                formulaPlanRemainingBudget = remainingSearchBudget outcome,
                formulaPlanProofCount = 0
                }
        proofs@(_ : _) -> do
            -- Bound the raw proof stream before checking and conversion. The
            -- outer worker subtracts this count before a complementary plan,
            -- so duplicate printed clauses and both translations consume the
            -- same documented proof-candidate cutoff.
            let (internalProofs, overflow) =
                    splitAt candidateLimit proofs
                candidateLimitReached = not $ null overflow
                convertProof internalProof =
                    let restored = restoreProofTerm proofEnv internalProof
                        providerApplied =
                            rewriteProviderInstantiationEvidence
                                providerApplications restored
                        implicitAxiomSymbols = axiomSymbols
                            `Set.difference`
                            Map.keysSet visibleApplications
                        erased = eliminateInstantiationEvidence
                            implicitAxiomSymbols providerApplied
                        convert
                            | usesInstantiationEvidence
                                axiomSymbols restored =
                                termToGeneratedClauseWithVisibleApplications
                                    visibleApplications
                            | otherwise = termToGeneratedClause
                    in convert target erased
            -- Every candidate must check against the requested formula
            -- before display names are restored and it is rendered.
            internalFailure "generated an invalid proof" $
                mapM_ (checkProof internalEnv form) internalProofs
            generatedClauses <- internalFailure
                "cannot construct generated clause" $
                mapM convertProof internalProofs
            let firstProof = case internalProofs of
                    firstProofTerm : _ -> Just $ show firstProofTerm
                    [] -> Nothing
                completion
                    | candidateLimitReached = SharedSearch.truncated
                        SharedSearch.CandidateLimitReached
                    | otherwise = queryCompletion outcome
                evidence = case generatedClauses of
                    [] -> SharedQuery.NoEvidence
                    _ : _ -> SharedQuery.ValidatedCandidates
            return FormulaPlanResult {
                formulaPlanFormula = show form,
                formulaPlanFirstProof = firstProof,
                formulaPlanCompletion = completion,
                formulaPlanClauses = generatedClauses,
                formulaPlanEvidence = evidence,
                formulaPlanRemainingBudget = remainingSearchBudget outcome,
                formulaPlanProofCount = length internalProofs
                }

formulaPlanFinished :: FormulaPlanResult -> Bool
formulaPlanFinished result =
    formulaPlanCompletion result == SharedSearch.Finished

-- Build the public result only after all requested plans have run. Clauses
-- are structurally de-duplicated across the union, then the surviving
-- candidates are ranked once. Proof checking remains plan-local above.
mergeFormulaPlanResults
    :: QueryOptions
    -> [FormulaPlanResult]
    -> Either DjinnQueryError DjinnResult
mergeFormulaPlanResults _ [] = internalQueryFailure
    "rank-N formula planning produced no search plan"
mergeFormulaPlanResults options results = makeDjinnResult
    (formulaPlanFormula metadataPlan)
    (formulaPlanFirstProof metadataPlan)
    (formulaPlanCompletion terminalPlan)
    candidates
    evidence
  where
    terminalPlan = last results
    metadataPlan = case filter (not . null . formulaPlanClauses) results of
        result : _ -> result
        [] -> terminalPlan
    mergedClauses = concatMap formulaPlanClauses results
    -- Axiom-consuming proofs retain eta expansion because Haskell simplified
    -- subsumption is not closed under the propositional eta law. Use the
    -- historical eta-normal form only for alpha-aware de-duplication. This
    -- collapses safe expanded spellings against their earlier compact
    -- candidate without rewriting the first, potentially eta-sensitive output
    -- we retain.
    distinctClauses = deduplicateEtaEquivalentClauses mergedClauses
    unrankedCandidates = map makeCandidate distinctClauses
    candidates
        | optionSorted options = sortOn
            SharedCandidate.candidateDetails unrankedCandidates
        | otherwise = unrankedCandidates
    evidence = case candidates of
        _ : _ -> SharedQuery.ValidatedCandidates
        [] -> formulaPlanEvidence terminalPlan

candidateDetails
    :: SharedGenerated.FunctionClause HSymbol
    -> DjinnCandidateDetails
candidateDetails clause
    | total == 0 = DjinnCandidateDetails 0 0
    | otherwise = DjinnCandidateDetails
        (toInteger unused % toInteger total)
        total
  where
    (unused, total) = List.foldl' countBinder (0, 0) $
        SharedGenerated.functionClauseBindingSites clause

    countBinder
        :: (Natural, Natural)
        -> Maybe HSymbol
        -> (Natural, Natural)
    countBinder (!unusedCount, !totalCount) binding =
        let !nextUnused =
                case binding of
                    Nothing -> unusedCount + 1
                    Just _ -> unusedCount
            !nextTotal = totalCount + 1
        in (nextUnused, nextTotal)

makeCandidate
    :: SharedGenerated.FunctionClause HSymbol
    -> DjinnCandidate
makeCandidate clause = SharedCandidate.Candidate {
    SharedCandidate.candidateOutput = clause,
    SharedCandidate.candidateResidualConstraints = [],
    SharedCandidate.candidateDetails = candidateDetails clause
    }

makeDjinnResult
    :: String
    -> Maybe String
    -> SharedSearch.Completion
    -> [DjinnCandidate]
    -> SharedQuery.QueryEvidence
    -> Either DjinnQueryError DjinnResult
makeDjinnResult formula proof completion candidates evidence =
    first DjinnResultInvariantFailure $
        SharedQuery.mkQueryResult evidence $
            SharedSearch.SearchBatch
                (SharedSearch.Completed completion)
                (DjinnQueryMetadata formula proof)
                candidates

resultToGeneratedReport :: DjinnResult -> Either String GeneratedQueryReport
resultToGeneratedReport result = case SharedSearch.batchProgress search of
    SharedSearch.Completed completion -> Right GeneratedQueryReport {
        generatedReportFormula = djinnTranslatedFormula metadata,
        generatedReportProof = djinnFirstExploredProof metadata,
        generatedReportCompletion = completion,
        generatedReportCandidates = SharedSearch.batchCandidates search,
        generatedReportEvidence = SharedQuery.resultEvidence result
        }
    SharedSearch.Continuing -> Left $
        "internal Djinn result invariant: proof search returned a continuing batch"
  where
    search = SharedQuery.resultSearch result
    metadata = SharedSearch.batchMetadata search

renderDjinnQueryError :: DjinnQueryError -> String
renderDjinnQueryError failure = case failure of
    DjinnQueryOptionsFailure optionsFailure ->
        renderDjinnQueryOptionsError optionsFailure
    DjinnQueryFailure message -> message
    DjinnInstantiationCandidateFailure message -> message
    DjinnInstantiationAssignmentFailure message -> message
    DjinnInternalQueryFailure message -> message
    DjinnResultInvariantFailure invariant ->
        "internal Djinn result invariant: " ++ show invariant

renderDjinnQueryOptionsError :: DjinnQueryOptionsError -> String
renderDjinnQueryOptionsError failure = case failure of
    NonPositiveCandidateCutoff _ -> "optionCutoff must be positive"
    NegativeChoicePointBudget _ -> "optionBudget must be non-negative"

outcomeEvidence :: QueryOutcome -> SharedQuery.QueryEvidence
outcomeEvidence outcome = case outcome of
    Realized{} -> SharedQuery.ValidatedCandidates
    Unrealizable -> SharedQuery.ProvedUninhabitable
    UnrealizableWithoutSelfReference -> SharedQuery.RequiresTargetReference
    Undecided -> SharedQuery.NoEvidence

evidenceOutcome :: SharedQuery.QueryEvidence -> [String] -> QueryOutcome
evidenceOutcome evidence rendered = case evidence of
    SharedQuery.ValidatedCandidates -> Realized rendered
    SharedQuery.ProvedUninhabitable -> Unrealizable
    SharedQuery.RequiresTargetReference -> UnrealizableWithoutSelfReference
    SharedQuery.NoEvidence -> Undecided

queryCompletion :: SearchOutcome -> SharedSearch.Completion
queryCompletion outcome
    | searchExhausted outcome =
        SharedSearch.truncated SharedSearch.ChoicePointLimitReached
    | otherwise = SharedSearch.Finished
