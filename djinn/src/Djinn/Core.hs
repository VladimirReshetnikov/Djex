-- |
-- The stable, validated interface to the Djinn core.
--
-- Build an 'Environment' from declarations, then ask 'inhabit' for a
-- Haskell expression of a given type.  Every entry point validates its
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
    parseHType, parseHKind, SynthesisTypeError(..),
    toSynthesisType, fromSynthesisType,
    -- * Declarations
    Constructor, Declaration(..),
    -- * Environments
    Environment, emptyEnvironment, standardEnvironment,
    declare, removeDeclaration,
    typeDeclarations, functionDeclarations, classDeclarations,
    -- * Queries
    Context, mkContext, resolveContext, resolveInstanceMethods,
    QueryOptions(..), defaultQueryOptions,
    QueryOutcome(..), QueryReport(..), inhabit
    ) where

import Control.Monad (foldM, unless)
import Data.List (intercalate, mapAccumL, nub, sortOn)
import Data.Ratio ((%))
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP (ReadP, readP_to_S, skipSpaces)

import Language.Haskell.Synthesis.Constraint
    (Constraint(..), constraintArity)
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Search as SharedSearch

import Djinn.Internal.Environment
import Djinn.Internal.HCheck (
    htCheckType, htCheckTypeKind, htCheckTypesKinds)
import Djinn.Internal.HIdentifier (
    isConId, isQualifiedVarId, isVarId, isVarOperator)
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

-- | A data constructor: name and field types.
type Constructor = (HSymbol, [HType])

data Declaration
    -- | @type Name params = body@.  The body must be a proper type.
    = TypeSynonym HSymbol [HSymbol] HType
    -- | @data Name params = C1 fields | ...@.  No constructors declares
    -- an empty (uninhabited) type.  Recursive types are rejected.
    | DataType HSymbol [HSymbol] [Constructor]
    -- | @type Name :: kind@ — an opaque type constructor.
    | AbstractType HSymbol HKind
    -- | @class Name params where methods@.  Parameter kinds are inferred
    -- from the method types and default to @*@.
    | ClassDecl HSymbol [HSymbol] [(HSymbol, HType)]
    -- | A function assumption available to proof search, used at exactly
    -- this monomorphic type.  The name may be qualified.
    | Function HSymbol HType
    deriving (Show)

-- | A validated set of declarations.  Values of this type can only be
-- produced by 'emptyEnvironment', 'standardEnvironment', 'declare', and
-- 'removeDeclaration', so every stored declaration is well-kinded, kinds
-- are never stale, and the declaration graph is acyclic.
data Environment = Environment {
    envTypes :: [TypeDefinition],
    envFunctions :: [Axiom],
    envClasses :: [ClassDefinition]
    }
    deriving (Show)

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
            mapM_ (requireName "data constructor" isTypeName . fst)
                constructors
            declareType name params (HTUnion constructors)
        AbstractType name kind -> do
            requireGroundKind kind
            declareType name [] (HTAbstract name kind)
        ClassDecl name params methods -> do
            requireName "class" isTypeName name
            mapM_ (requireName "class parameter" isVarId) params
            mapM_ (requireName "method" isMethodName . fst) methods
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
            requireName "function" isFunctionName name
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
        requireName "type constructor" isTypeName name
        mapM_ (requireName "type parameter" isVarId) params
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

-- The unit spelling belongs to the type grammar, but is not a Haskell ConId.
-- Its one legitimate declaration is installed by 'trustedUnitEnvironment';
-- accepting it here would also admit arbitrary synonyms, classes, or owners
-- for the @()@ constructor through the public API.
isTypeName :: HSymbol -> Bool
isTypeName = isConId

isMethodName :: HSymbol -> Bool
isMethodName name = isVarId name || isVarOperator name

isFunctionName :: HSymbol -> Bool
isFunctionName name = isQualifiedVarId name || isVarOperator name

requireGroundKind :: HKind -> Either String ()
requireGroundKind KStar = Right ()
requireGroundKind (KArrow argument result) =
    requireGroundKind argument >> requireGroundKind result
requireGroundKind kind =
    Left $ "kind contains an unsolved variable: " ++ show kind

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
    requireName "class" isTypeName className
    case SharedName.parseName className of
        Left nameError -> Left $ SharedName.renderNameError nameError
        Right sharedName -> Right $ Constraint sharedName arguments

-- | Look up a class use, requiring exact arity, arguments that fit the
-- parameters' inferred kinds, and well-kinded instantiated methods.
-- Returns each method at its instantiated type.
resolveContext :: Environment -> Context -> Either String [(HSymbol, HType)]
resolveContext environment context =
    concat <$> resolveContexts environment [] [context]

-- | Resolve the methods of an instance target while checking the target and
-- all prerequisite contexts in one kind-variable scope.  The returned methods
-- belong only to the target and are instantiated exactly as by
-- 'resolveContext'.
resolveInstanceMethods :: Environment -> [Context] -> Context
                       -> Either String [(HSymbol, HType)]
resolveInstanceMethods environment prerequisites target = do
    resolved <- resolveContexts environment [] (target : prerequisites)
    case resolved of
        targetMethods : _ -> Right targetMethods
        [] -> Left "internal error: instance target was not resolved"

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

resolveContexts :: Environment -> [(String, HKind, HType)] -> [Context]
                -> Either String [[(HSymbol, HType)]]
resolveContexts environment additionalTypes contexts = do
    resolved <- mapM (lookupContext environment) contexts
    checkKindObligations (envTypes environment) $
        additionalTypes ++ concatMap argumentObligations resolved
    mapM (instantiateContext environment) resolved

lookupContext :: Environment -> Context -> Either String ResolvedContext
lookupContext environment context = do
    -- Context is a shared, intentionally permissive syntax node.  Reassert
    -- Djinn's narrower class namespace even when a caller constructs that
    -- node directly instead of going through mkContext.
    requireName "class" isTypeName name
    case lookup name (envClasses environment) of
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
checkKindObligations :: [TypeDefinition] -> [(String, HKind, HType)]
                     -> Either String ()
checkKindObligations definitions obligations =
    case htCheckTypesKinds definitions
            [(kind, t) | (_, kind, t) <- obligations] of
        Right () -> Right ()
        Left jointError ->
            case [(label, message)
                    | (label, kind, t) <- obligations
                    , Left message <- [htCheckTypeKind definitions kind t]] of
                (label, message) : _ -> Left $ label ++ ": " ++ message
                [] -> Left $
                    "inconsistent kinds across " ++
                    intercalate ", " [label | (label, _, _) <- obligations] ++
                    ": " ++ jointError

instantiateContext :: Environment -> ResolvedContext
                   -> Either String [(HSymbol, HType)]
instantiateContext environment context = do
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
        case htCheckType (envTypes environment) methodType of
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
    (_, renamings) = mapAccumL allocateFresh
        initiallyUnavailable capturedLocals

    allocateFresh unavailable variable =
        let fresh = chooseFresh 1
            chooseFresh primeCount =
                let candidate = variable ++ replicate primeCount '\''
                in if Set.member candidate unavailable then
                       chooseFresh (primeCount + 1)
                   else
                       candidate
        in (Set.insert fresh unavailable, (variable, HTVar fresh))

data QueryOptions = QueryOptions {
    -- | Collect alternative solutions beyond the first.
    optionAlternatives :: Bool,
    -- | Rank solutions by the fraction of unused binders, then binder
    -- count; implies collecting alternatives.
    optionSorted :: Bool,
    -- | Maximum number of candidate proofs considered (positive).
    optionCutoff :: Int,
    -- | Choice-point budget; 'Nothing' keeps the search a complete
    -- decision procedure.
    optionBudget :: Maybe Integer
    }
    deriving (Show)

-- | One solution, sorted, up to 200 candidates, no budget.
defaultQueryOptions :: QueryOptions
defaultQueryOptions = QueryOptions {
    optionAlternatives = False,
    optionSorted = True,
    optionCutoff = 200,
    optionBudget = Nothing
    }

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
    -- | Whether the configured proof exploration finished normally or spent
    -- its choice-point budget. Logical negative evidence remains in
    -- 'reportOutcome' rather than being conflated with this status.
    reportCompletion :: SharedSearch.Completion,
    reportOutcome :: QueryOutcome
    }
    deriving (Show)

-- | Search for total Haskell realizations of a type.
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
    requireName "target" isMethodName name
    unless (optionCutoff options > 0) $
        Left "optionCutoff must be positive"
    case optionBudget options of
        Just n | n < 0 -> Left "optionBudget must be non-negative"
        _ -> Right ()
    contextMethods <- resolveContexts environment
        [("goal type " ++ show goal, KStar, goal)] contexts
    let types = envTypes environment
        form = hTypeToFormula types goal
        externalEnv =
            [ (Symbol v, hTypeToFormula types t)
            | (v, t) <- envFunctions environment ] ++
            [ (Symbol v, hTypeToFormula types t)
            | methods <- contextMethods, (v, t) <- methods ]
        proofEnv = prepareProofEnvironment (Symbol name) externalEnv
        internalEnv = proofBindings proofEnv
        mode = (defaultSearchMode
                    (optionAlternatives options || optionSorted options)) {
            searchBudget = optionBudget options
            }
        outcome = proveWithMode mode internalEnv form
        labeled what = either (Left . ((what ++ ": ") ++)) Right
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
                            labeled "generated an invalid self-reference proof" $
                                checkProof diagnosticEnv form diagnosticProof
                            return UnrealizableWithoutSelfReference
                        [] -> return Unrealizable
            return QueryReport {
                reportFormula = show form,
                reportProof = Nothing,
                reportCompletion = queryCompletion outcome,
                reportOutcome = failure
                }
        p : ps -> do
            let internalProofs = p : take (optionCutoff options - 1) ps
            -- Every candidate must check against the requested formula
            -- before display names are restored and it is rendered.
            labeled "generated an invalid proof" $
                mapM_ (checkProof internalEnv form) internalProofs
            rendered <- labeled "cannot render generated proof" $
                mapM (termToHClause name . restoreProofTerm proofEnv)
                    internalProofs
            -- Rank a clause by the fraction of its binders that are
            -- unused, then by the total binder count: solutions that use
            -- more of their arguments are usually the intended ones.
            let score clause =
                    let bvs = getBinderVars clause
                        r | null bvs = (0, 0)
                          | otherwise =
                              ( length (filter (== "_") bvs) % length bvs
                              , length bvs )
                    in (r, clause)
                clauses = nub $
                    if optionSorted options then
                        map snd $ sortOn fst $ map score rendered
                    else
                        rendered
            renderedClauses <- labeled "cannot render generated clause" $
                mapM hPrClause clauses
            return QueryReport {
                reportFormula = show form,
                reportProof = Just (show p),
                reportCompletion = queryCompletion outcome,
                reportOutcome = Realized renderedClauses
                }

queryCompletion :: SearchOutcome -> SharedSearch.Completion
queryCompletion outcome
    | searchExhausted outcome =
        SharedSearch.truncated SharedSearch.ChoicePointLimitReached
    | otherwise = SharedSearch.Finished
