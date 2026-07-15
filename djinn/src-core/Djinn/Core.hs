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
    declare, removeDeclaration,
    typeDeclarations, functionDeclarations, classDeclarations,
    -- * Queries
    Context, mkContext, resolveContext, resolveInstanceMethods,
    resolvePreparedContext, resolvePreparedInstanceMethods,
    QueryOptions(..), defaultQueryOptions,
    DjinnCandidateDetails(..), DjinnCandidate,
    DjinnQueryMetadata(..), DjinnResult,
    DjinnQueryOptionsError(..), DjinnQueryError(..),
    inhabitResult, inhabitResultPrepared,
    GeneratedQueryReport(..), inhabitGenerated, inhabitGeneratedPrepared,
    QueryOutcome(..), QueryReport(..), inhabit
    ) where

import Control.Monad (foldM, unless)
import Data.Bifunctor (first)
import Data.List (intercalate, mapAccumL, nub, sortOn)
import qualified Data.List as List
import Data.Ratio ((%))
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import Text.ParserCombinators.ReadP
    (ReadP, option, readP_to_S, skipSpaces)

import Language.Haskell.Synthesis.Constraint
    (Constraint(..), constraintArity)
import qualified Language.Haskell.Synthesis.Candidate as SharedCandidate
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Generated as SharedGenerated
import qualified Language.Haskell.Synthesis.Query as SharedQuery
import qualified Language.Haskell.Synthesis.Search as SharedSearch
import qualified Language.Haskell.Synthesis.Type as SharedType

import Djinn.Internal.Environment
import Djinn.Internal.Declaration
import qualified Djinn.Internal.Fresh as Fresh
import Djinn.Internal.HTypes
import Djinn.Internal.LJT
import Djinn.Internal.ProofCheck (checkProof)
import Djinn.Internal.ProofEnv
import Djinn.Internal.Type

------------------------------------------------------------------
-- Kinds

-- | The kind of proper types, @*@.
kStar :: HKind
kStar = KStar

-- | A function kind, e.g. @kArrow kStar kStar@ is @* -> *@.
kArrow :: HKind -> HKind -> HKind
kArrow = KArrow

-- | Parse a type in Djinn's Haskell-like syntax, requiring the whole
-- input to be consumed.
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
-- constructor through this private path; all public declarations continue
-- through the ordinary lexical checks below.
trustedUnitEnvironment :: Either String Environment
trustedUnitEnvironment = do
    (types, classes) <- validateEnvironment
        [("()", ([], HTUnion [("()", [])], KStar))] [] []
    return Environment {
        envTypes = types,
        envFunctions = [],
        envClasses = classes
        }

-- | Add (or overwrite, for the same name in the same category) one
-- declaration.  The whole environment is revalidated transactionally:
-- on 'Left' the original environment is still the one to use.
declare :: Declaration -> Environment -> Either String Environment
declare declaration environment =
    case declaration of
        TypeSynonym name params body ->
            declareType name params body
        DataType name params constructors -> do
            mapM_ (requireName "data constructor"
                (isDjinnDeclarationName DataConstructorOwner) . fst)
                constructors
            declareType name params (HTUnion constructors)
        AbstractType name kind -> do
            requireGroundKind kind
            declareType name [] (HTAbstract name kind)
        ClassDecl name params methods -> do
            requireName "class"
                (isDjinnDeclarationName ClassOwner) name
            mapM_ (requireName "class parameter" isDjinnTypeVariable) params
            mapM_ (requireName "method"
                (isDjinnDeclarationName MethodOwner) . fst) methods
            requireUnusedName "type" name (envTypes environment)
            requireDistinct "class parameter" params
            requireDistinct "method" (map fst methods)
            let uncheckedParameters = [(param, KStar) | param <- params]
                candidateClasses = replace name
                    (name, (uncheckedParameters, methods))
                    (envClasses environment)
            (types, classes) <- validateEnvironment
                (envTypes environment) (envFunctions environment)
                candidateClasses
            return Environment {
                envTypes = types,
                envFunctions = envFunctions environment,
                envClasses = classes
                }
        Function name declaredType -> do
            requireName "function"
                (isDjinnDeclarationName FunctionOwner) name
            let functions = replace name (name, declaredType)
                    (envFunctions environment)
            (types, classes) <- validateEnvironment
                (envTypes environment) functions (envClasses environment)
            return Environment {
                envTypes = types,
                envFunctions = functions,
                envClasses = classes
                }
  where
    declareType name params body = do
        requireName "type constructor"
            (isDjinnDeclarationName TypeOwner) name
        mapM_ (requireName "type parameter" isDjinnTypeVariable) params
        requireUnusedName "class" name (envClasses environment)
        requireDistinct "type parameter" params
        checkConstructors name body (envTypes environment)
        (types, classes) <- validateEnvironment
            (replace name (name, (params, body, KStar))
                (envTypes environment))
            (envFunctions environment) (envClasses environment)
        return environment { envTypes = types, envClasses = classes }

-- | Remove a declaration by name, from whichever category holds it.
-- Rejected if the name is not defined or if a remaining declaration
-- depends on it; on 'Left' the environment is unchanged.
removeDeclaration :: HSymbol -> Environment -> Either String Environment
removeDeclaration name environment
    -- Unlike an ordinary leaf declaration, the standard unit binding is the
    -- trusted interpretation of grammar-level @()@ and public 'declare' cannot
    -- recreate it.  Keep environments derived from 'standardEnvironment'
    -- stable; callers wanting no built-ins can start from 'emptyEnvironment'.
    | name == "()" && name `elem` map fst (envTypes environment) =
        Left "() is a built-in type and cannot be removed"
    | name `notElem` (map fst (envTypes environment) ++
                      map fst (envFunctions environment) ++
                      map fst (envClasses environment)) =
        Left $ name ++ " is not defined"
    | otherwise = do
        let keep :: [(HSymbol, a)] -> [(HSymbol, a)]
            keep = filter ((name /=) . fst)
            candidateTypes = keep (envTypes environment)
            candidateFunctions = keep (envFunctions environment)
            candidateClasses = keep (envClasses environment)
        (types, classes) <- validateEnvironment candidateTypes
            candidateFunctions candidateClasses
        return Environment {
            envTypes = types,
            envFunctions = candidateFunctions,
            envClasses = classes
            }

requireName :: String -> (HSymbol -> Bool) -> HSymbol -> Either String ()
requireName what valid name
    | valid name = Right ()
    | otherwise = Left $ show name ++ " is not a valid " ++ what ++ " name"

requireGroundKind :: HKind -> Either String ()
requireGroundKind kind = case groundHKind kind of
    Right _ -> Right ()
    Left variable -> Left $
        "kind contains an unsolved variable: " ++ show (KVar variable)

------------------------------------------------------------------
-- Queries

-- | A class constraint represented by the backend-neutral synthesis
-- vocabulary.  Djinn retains ownership of class lookup, kind checking, and
-- the interpretation of methods as premises; only the finite nominal syntax
-- is shared with Exference.
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
    concat <$> resolveContexts prepared [] [context]

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
    resolved <- resolveContexts prepared [] (target : prerequisites)
    case resolved of
        targetMethods : _ -> Right targetMethods
        [] -> Left "internal error: instance target was not resolved"

prepareCompatibilityEnvironment
    :: Environment -> Either String PreparedEnvironment
prepareCompatibilityEnvironment = first show . prepareEnvironment

-- The intermediate record keeps lookup/arity validation separate from the
-- joint kind check and substitution.  In particular, every argument and the
-- query goal must share one scope for their free type variables.
data ResolvedContext = ResolvedContext {
    resolvedConstraint :: Context,
    resolvedParameters :: [(HSymbol, HKind)],
    resolvedMethods :: [(HSymbol, HType)]
    }

resolvedName :: ResolvedContext -> HSymbol
resolvedName = SharedName.renderCanonical . constraintClass . resolvedConstraint

resolvedArguments :: ResolvedContext -> [HType]
resolvedArguments = constraintArguments . resolvedConstraint

resolveContexts :: PreparedEnvironment -> [(String, HKind, HType)] -> [Context]
                -> Either String [[(HSymbol, HType)]]
resolveContexts prepared additionalTypes contexts = do
    resolved <- mapM (lookupContext prepared) contexts
    checkKindObligations prepared $
        additionalTypes ++ concatMap argumentObligations resolved
    mapM (instantiateContext prepared) resolved

-- Query aliases are session-dependent, so elaborate only after lookup and
-- arity validation against the prepared environment. The goal and every
-- context argument form one batch: a free variable therefore cannot acquire
-- incompatible kinds in different parts of the same query.
--
-- Keep 'checkKindObligations' as a compatibility preflight. In particular,
-- Djinn historically reports an unsaturated synonym before an independent
-- kind error inside the same raw type, whereas the shared elaborator must kind
-- check before expansion so a phantom parameter cannot erase a bad argument.
-- Running the established check first preserves that observable diagnostic
-- order and spelling; shared elaboration then owns the alias-free query sent
-- to proof search.
resolveQueryContexts
    :: PreparedEnvironment
    -> (String, HKind, HType)
    -> [Context]
    -> Either DjinnQueryError (HType, [[(HSymbol, HType)]])
resolveQueryContexts prepared goalObligation contexts = do
    resolved <- queryFailure $ mapM (lookupContext prepared) contexts
    let obligations = goalObligation : concatMap argumentObligations resolved
    queryFailure $
        checkKindObligations prepared obligations
    elaborated <- queryFailure $ elaboratePreparedTypes prepared
        [(kind, source) | (_, kind, source) <- obligations]
    case elaborated of
        [] -> internalQueryFailure "query elaboration dropped its goal"
        elaboratedGoal : elaboratedArguments -> do
            elaboratedContexts <- attachElaboratedArguments
                resolved elaboratedArguments
            -- Resolution, kind checking, and shared elaboration have already
            -- validated these method types. A failure while instantiating the
            -- sealed class inventory is therefore an implementation/session
            -- invariant, not a defect in the query source.
            methods <- first DjinnInternalQueryFailure $
                mapM (instantiateContext prepared) elaboratedContexts
            return (elaboratedGoal, methods)
  where
    queryFailure = first DjinnQueryFailure
-- Rebuild the already resolved constraints with their alias-free arguments.
-- The shared batch operation is shape-preserving, but check that invariant at
-- this representation boundary rather than silently dropping a suffix if it
-- is ever changed.
attachElaboratedArguments
    :: [ResolvedContext]
    -> [HType]
    -> Either DjinnQueryError [ResolvedContext]
attachElaboratedArguments [] [] = Right []
attachElaboratedArguments [] _ =
    internalQueryFailure "query elaboration returned extra context arguments"
attachElaboratedArguments (context : contexts) arguments = do
    let arity = length $ resolvedParameters context
        (current, remaining) = splitAt arity arguments
    if length current /= arity then
        internalQueryFailure "query elaboration dropped a context argument"
    else do
        rest <- attachElaboratedArguments contexts remaining
        let constraint = (resolvedConstraint context) {
                constraintArguments = current
                }
        return (context {resolvedConstraint = constraint} : rest)

internalQueryFailure :: String -> Either DjinnQueryError value
internalQueryFailure =
    Left . DjinnInternalQueryFailure . ("internal error: " ++)

lookupContext :: PreparedEnvironment -> Context -> Either String ResolvedContext
lookupContext prepared context = do
    -- Context is a shared, intentionally permissive syntax node.  Reassert
    -- Djinn's narrower class namespace even when a caller constructs that
    -- node directly instead of going through mkContext.
    requireName "class" (isDjinnDeclarationName ClassOwner) name
    case lookupPreparedEnvironmentClass name prepared of
        Nothing -> Left $ "Class not found: " ++ name
        Just (params, methods)
            | length params == constraintArity context ->
                Right ResolvedContext {
                    resolvedConstraint = context,
                    resolvedParameters = params,
                    resolvedMethods = methods
                    }
            | otherwise -> Left $
                "Class " ++ name ++ " expects " ++ show (length params) ++
                " type argument(s), but got " ++ show (constraintArity context)
  where
    name = SharedName.renderCanonical $ constraintClass context

argumentObligations :: ResolvedContext -> [(String, HKind, HType)]
argumentObligations context =
    [ ("argument " ++ show argument ++ " of class " ++ resolvedName context,
       kind, argument)
    | ((_, kind), argument) <-
        zip (resolvedParameters context) (resolvedArguments context) ]

-- Retain the precise historical diagnostic when one type is independently
-- ill-kinded.  If every component works alone, report the actual problem:
-- inconsistent kinds assigned to a free variable shared by components.
checkKindObligations :: PreparedEnvironment -> [(String, HKind, HType)]
                     -> Either String ()
checkKindObligations prepared obligations =
    case checkPreparedTypesKinds prepared
            [(kind, t) | (_, kind, t) <- obligations] of
        Right () -> Right ()
        Left jointError ->
            case [(label, message)
                    | (label, kind, t) <- obligations
                    , Left message <-
                        [checkPreparedTypesKinds prepared [(kind, t)]]] of
                (label, message) : _ -> Left $ label ++ ": " ++ message
                [] -> Left $
                    "inconsistent kinds across " ++
                    intercalate ", " [label | (label, _, _) <- obligations] ++
                    ": " ++ jointError

instantiateContext :: PreparedEnvironment -> ResolvedContext
                   -> Either String [(HSymbol, HType)]
instantiateContext prepared context = do
    let parameters = resolvedParameters context
        arguments = resolvedArguments context
        instantiated =
            [ (methodName,
               instantiateMethod parameters arguments methodType)
            | (methodName, methodType) <- resolvedMethods context ]
    -- Each method's non-class variables are implicitly quantified by that
    -- signature, not shared with identically spelled variables in sibling
    -- methods.  Checking one synthetic tuple would accidentally reunify them.
    mapM_ checkMethod instantiated
    return instantiated
  where
    checkMethod (methodName, methodType) =
        case checkPreparedTypesKinds prepared [(KStar, methodType)] of
            Left message -> Left $
                "method " ++ prHSymbolOp methodName ++ " of class " ++
                resolvedName context ++ ": " ++ message
            Right () -> Right ()

-- Class parameters and method-local variables live in different implicit
-- quantifier scopes.  Before substituting class arguments, alpha-rename only
-- those locals that occur in an active substitution image.  Renaming every
-- local would unnecessarily sever Djinn's intentionally shallow matching of
-- a method-local spelling with the query goal (for example @return@'s @a@).
instantiateMethod :: [(HSymbol, HKind)] -> [HType] -> HType -> HType
instantiateMethod parameters arguments methodType =
    substHT substitution $ substHT renamings methodType
  where
    parameterNames = map fst parameters
    substitution = zip parameterNames arguments
    methodVariables = getHTVars methodType
    activeImages =
        [ argument
        | (parameter, argument) <- substitution
        , parameter `elem` methodVariables
        ]
    imageVariables = Set.fromList $ concatMap getHTVars activeImages
    localVariables =
        filter (`notElem` parameterNames) methodVariables
    capturedLocals =
        filter (`Set.member` imageVariables) localVariables
    initiallyUnavailable = Set.fromList $
        parameterNames ++ methodVariables ++ concatMap getHTVars arguments
    (_, renamings) = mapAccumL allocateRenaming
        initiallyUnavailable capturedLocals

    allocateRenaming unavailable variable =
        let (fresh, unavailable', _) = Fresh.allocateFresh
                (\candidate -> (candidate, candidate ++ "'"))
                unavailable (variable ++ "'")
        in (unavailable', (variable, HTVar fresh))

data QueryOptions = QueryOptions {
    -- | Collect alternative solutions beyond the first.
    optionAlternatives :: Bool,
    -- | Rank solutions by the fraction of unused binders, then binder
    -- count; implies collecting alternatives.
    optionSorted :: Bool,
    -- | Maximum number of candidate proofs considered (positive). Observing
    -- one more proof reports 'SharedSearch.CandidateLimitReached'.
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
    -- | The search budget expired first; inhabitation is undecided.
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
-- classes and simply contribute each instantiated method as an extra
-- premise.  'Left' reports invalid input or an internal rendering
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
-- At most @optionCutoff + 1@ proofs are observed.  The extra observation is
-- solely a truncation witness: when present, neither the remaining proof
-- stream nor 'searchExhausted' is forced.  When absent, reaching the end of
-- the prefix has already established whether proof search finished or spent
-- its choice-point budget.
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

-- | Search using a sealed environment.  All context, goal, and instantiated
-- method obligations share the cached assumptions prepared with the session;
-- this function never reconstructs them from 'envTypes'.
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

inhabitResultPreparedChecked
    :: QueryOptions
    -> PreparedEnvironment
    -> [Context]
    -> SharedGenerated.DefinitionName
    -> HType
    -> Either DjinnQueryError DjinnResult
inhabitResultPreparedChecked options prepared contexts target goal = do
    (elaboratedGoal, contextMethods) <-
        resolveQueryContexts prepared
        ("goal type " ++ show goal, KStar, goal) contexts
    let translate = preparedEnvironmentFormulaTranslator prepared
        translateMethod (symbol, source) =
            (,) (Symbol symbol) `fmap`
                first (("method " ++ prHSymbolOp symbol ++ ": ") ++)
                    (translate source)
    -- The prepared translator was built from the same checked environment,
    -- and this request batch has already been alias-elaborated. Its remaining
    -- failure mode is a broken sealed-session invariant and must not be
    -- attributed to the query's source span.
    form <- translatorFailure $ first ("goal type: " ++) $
        translate elaboratedGoal
    methodEnv <- translatorFailure $ concat `fmap`
        mapM (mapM translateMethod) contextMethods
    let externalEnv = preparedEnvironmentFunctionPremises prepared ++ methodEnv
        proofEnv = prepareProofEnvironment (Symbol name) externalEnv
        internalEnv = proofBindings proofEnv
        mode = (defaultSearchMode
                    (optionAlternatives options || optionSorted options)) {
            searchBudget = optionBudget options
            }
        outcome = proveWithMode mode internalEnv form
        internalFailure what = first $
            DjinnInternalQueryFailure . ((what ++ ": ") ++)
    case searchProofs outcome of
        [] -> do
            failure <-
                if searchExhausted outcome then
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
                            proofBindingsIncludingTarget proofEnv
                        diagnosticMode = mode {
                            searchAlternatives = False,
                            searchBudget = remainingSearchBudget outcome
                            }
                        diagnosticOutcome =
                            proveWithMode diagnosticMode diagnosticEnv form
                    case searchProofs diagnosticOutcome of
                        diagnosticProof : _ -> do
                            internalFailure
                                "generated an invalid self-reference proof" $
                                checkProof diagnosticEnv form diagnosticProof
                            return UnrealizableWithoutSelfReference
                        [] -> return Unrealizable
            makeDjinnResult
                (show form)
                Nothing
                (queryCompletion outcome)
                []
                (outcomeEvidence failure)
        proofs@(p : _) -> do
            -- Bound the raw proof stream before checking, conversion, ranking,
            -- or de-duplication.  In particular, duplicate printed clauses
            -- still consume the documented proof-candidate cutoff.
            let (internalProofs, overflow) =
                    splitAt (optionCutoff options) proofs
                candidateLimitReached = not $ null overflow
            -- Every candidate must check against the requested formula
            -- before display names are restored and it is rendered.
            internalFailure "generated an invalid proof" $
                mapM_ (checkProof internalEnv form) internalProofs
            rendered <- internalFailure "cannot render generated proof" $
                mapM (termToHClause name . restoreProofTerm proofEnv)
                    internalProofs
            -- Preserve the historical stable ratio-then-count ordering over
            -- raw clauses.  De-duplication intentionally follows sorting.
            let scored clause = (candidateDetails clause, clause)
                clauses = nub $
                    if optionSorted options then
                        map snd $ sortOn fst $ map scored rendered
                    else
                        rendered
            generatedClauses <- internalFailure
                "cannot convert generated clause" $
                mapM (toGeneratedClauseWithName target) clauses
            let candidates = zipWith makeCandidate clauses generatedClauses
                completion
                    | candidateLimitReached = SharedSearch.truncated
                        SharedSearch.CandidateLimitReached
                    | otherwise = queryCompletion outcome
            makeDjinnResult
                (show form)
                (Just $ show p)
                completion
                candidates
                SharedQuery.ValidatedCandidates
  where
    name = SharedGenerated.definitionSpelling target
    translatorFailure = first DjinnInternalQueryFailure

candidateDetails :: HClause -> DjinnCandidateDetails
candidateDetails clause
    | total == 0 = DjinnCandidateDetails 0 0
    | otherwise = DjinnCandidateDetails
        (toInteger unused % toInteger total)
        total
  where
    (unused, total) = List.foldl' countBinder (0, 0) $ getBinderVars clause

    countBinder :: (Natural, Natural) -> HSymbol -> (Natural, Natural)
    countBinder (!unusedCount, !totalCount) binder =
        let !nextUnused =
                if binder == "_" then unusedCount + 1 else unusedCount
            !nextTotal = totalCount + 1
        in (nextUnused, nextTotal)

makeCandidate
    :: HClause
    -> SharedGenerated.FunctionClause HSymbol
    -> DjinnCandidate
makeCandidate clause generated = SharedCandidate.Candidate {
    SharedCandidate.candidateOutput = generated,
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
