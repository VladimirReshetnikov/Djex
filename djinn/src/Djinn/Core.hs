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
    parseHType, parseHKind,
    -- * Declarations
    Constructor, Declaration(..),
    -- * Environments
    Environment, emptyEnvironment, standardEnvironment,
    declare, removeDeclaration,
    typeDeclarations, functionDeclarations, classDeclarations,
    -- * Queries
    Context, resolveContext,
    QueryOptions(..), defaultQueryOptions,
    QueryOutcome(..), QueryReport(..), inhabit
    ) where

import Control.Monad (foldM, unless)
import Data.List (nub, sortOn)
import Data.Ratio ((%))
import Text.ParserCombinators.ReadP (ReadP, readP_to_S, skipSpaces)

import Djinn.Internal.Environment
import Djinn.Internal.HCheck (
    htCheckType, htCheckTypeKind, htInferClassKinds)
import Djinn.Internal.HIdentifier (
    isConId, isQualifiedVarId, isVarId, isVarOperator)
import Djinn.Internal.HTypes
import Djinn.Internal.LJT
import Djinn.Internal.ProofCheck (checkProof)
import Djinn.Internal.ProofEnv

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
    either (error . ("Djinn.Core.standardEnvironment: " ++)) id $
        foldM (flip declare) emptyEnvironment
            [ DataType "()" [] [("()", [])]
            , DataType "Either" ["a", "b"]
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
            checkMethodNames name methods (envClasses environment)
            kinds <- htInferClassKinds (envTypes environment) params
                (map snd methods)
            return environment {
                envClasses = replace name (name, (kinds, methods))
                    (envClasses environment)
                }
        Function name declaredType -> do
            requireName "function" isFunctionName name
            htCheckType (envTypes environment) declaredType
            return environment {
                envFunctions = replace name (name, declaredType)
                    (envFunctions environment)
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

-- The unit type is grammar, but the standard environment declares it.
isTypeName :: HSymbol -> Bool
isTypeName name = isConId name || name == "()"

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

-- | A class constraint: class name and type arguments.
type Context = (HSymbol, [HType])

-- | Look up a class use, requiring exact arity, arguments that fit the
-- parameters' inferred kinds, and well-kinded instantiated methods.
-- Returns each method at its instantiated type.
resolveContext :: Environment -> Context -> Either String [(HSymbol, HType)]
resolveContext environment (name, arguments) =
    case lookup name (envClasses environment) of
        Nothing -> Left $ "Class not found: " ++ name
        Just (params, methods)
            | length params == length arguments -> do
                sequence_
                    [ describeArgument argument $
                        htCheckTypeKind (envTypes environment) kind argument
                    | ((_, kind), argument) <- zip params arguments ]
                let instantiated =
                        [ (methodName,
                           substHT (zip (map fst params) arguments)
                               methodType)
                        | (methodName, methodType) <- methods ]
                either
                    (Left . (("methods of class " ++ name ++ ": ") ++))
                    Right $
                    htCheckType (envTypes environment)
                        (HTTuple (map snd instantiated))
                return instantiated
            | otherwise -> Left $
                "Class " ++ name ++ " expects " ++ show (length params) ++
                " type argument(s), but got " ++ show (length arguments)
  where
    describeArgument argument = either
        (Left . (("argument " ++ show argument ++ " of class " ++ name ++
            ": ") ++))
        Right

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
    htCheckType (envTypes environment) goal
    contextMethods <- mapM (resolveContext environment) contexts
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
    case searchProofs outcome of
        [] ->
            let failure
                    | searchExhausted outcome = Undecided
                    | targetWasExcluded proofEnv =
                        UnrealizableWithoutSelfReference
                    | otherwise = Unrealizable
            in return QueryReport {
                reportFormula = show form,
                reportProof = Nothing,
                reportOutcome = failure
                }
        p : ps -> do
            let internalProofs = p : take (optionCutoff options - 1) ps
                labeled what =
                    either (Left . ((what ++ ": ") ++)) Right
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
            return QueryReport {
                reportFormula = show form,
                reportProof = Just (show p),
                reportOutcome = Realized (map hPrClause clauses)
                }
