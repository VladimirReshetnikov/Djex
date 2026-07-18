--
-- Independent type checking for LJT proof terms.
--
module Djinn.Internal.ProofCheck (
    checkProofEnvironment,
    checkProof
    ) where

import Control.Monad (replicateM, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (
    StateT, evalStateT, get, gets, modify, put)
import Data.List (intercalate, (!?))
import qualified Data.Map.Strict as Map
import Numeric.Natural (Natural)

import Djinn.Internal.LJTFormula
import Language.Haskell.Synthesis.Collection (firstDuplicate)

data ProofType
    = Meta Natural
    | Product [ProofType]
    | Sum [(ConsDesc, ProofType)]
    | EmptyType Symbol
    | ProofType :~> ProofType
    | Atom Symbol
    deriving (Eq)

infixr 2 :~>

data Constraint
    = Injection ProofType ConsDesc Int ProofType
    | EmptyEliminator ProofType

data CheckState = CheckState {
    -- Metavariables are private identities, so an arbitrary-precision counter
    -- avoids semantic reuse without changing the public Int-valued term arities.
    nextMeta :: Natural,
    substitutions :: Map.Map Natural ProofType,
    constraints :: [Constraint]
    }

type Check a = StateT CheckState (Either String) a
type Environment = [(Symbol, ProofType)]

initialState :: CheckState
initialState = CheckState 0 Map.empty []

checkProof :: [(Symbol, Formula)] -> Formula -> Term -> Either String ()
checkProof environment expected term =
    evalStateT check initialState
  where
    -- Successful unification with the goal grounds every metavariable
    -- reachable from the inferred type, because 'fromFormula expected' never
    -- contains one; only pending injection and empty-eliminator constraints
    -- can still fail afterwards.
    check = do
        ensureUniqueEnvironment environment
        actual <- infer (map (\ (symbol, formula) ->
            (symbol, fromFormula formula)) environment) term
        unify actual (fromFormula expected)
        solveConstraints

-- | Reject an ambiguous external proof environment before either search or
-- independent checking gives its first occurrence precedence.  Proof binders
-- may still shadow outer identities normally; this check applies only to the
-- caller-supplied assumption list.
checkProofEnvironment :: [(Symbol, Formula)] -> Either String ()
checkProofEnvironment environment =
    case firstDuplicate $ map fst environment of
        Nothing -> Right ()
        Just symbol -> Left $
            "duplicate proof identity in environment: " ++ show symbol

ensureUniqueEnvironment :: [(Symbol, Formula)] -> Check ()
ensureUniqueEnvironment = lift . checkProofEnvironment

infer :: Environment -> Term -> Check ProofType
infer environment term =
    case term of
        Var symbol ->
            case lookup symbol environment of
                Just proofType -> return proofType
                Nothing -> failCheck $ "unbound proof variable: " ++ show symbol
        Lam binder body -> do
            argument <- freshMeta
            result <- infer ((binder, argument) : environment) body
            return $ argument :~> result
        Apply function argument -> do
            functionType <- infer environment function
            argumentType <- infer environment argument
            resultType <- freshMeta
            unify functionType (argumentType :~> resultType)
            return resultType
        Ctuple arity -> do
            ensureNonNegative "tuple arity" arity
            elements <- freshMetas arity
            return $ foldr (:~>) (Product elements) elements
        Csplit arity -> do
            ensureNonNegative "split arity" arity
            elements <- freshMetas arity
            result <- freshMeta
            let handler = foldr (:~>) result elements
            return $ handler :~> Product elements :~> result
        Cinj constructor index -> do
            ensureConstructor constructor
            ensureNonNegative "injection index" index
            payload <- freshMeta
            result <- freshMeta
            addConstraint $ Injection result constructor index payload
            return $ payload :~> result
        Ccases constructors -> do
            mapM_ ensureConstructor constructors
            -- Constructors already are the allocation plan; following that
            -- spine avoids a lossy list-length projection.
            branches <- mapM (const freshMeta) constructors
            result <- freshMeta
            if null constructors then do
                emptyInput <- freshMeta
                addConstraint $ EmptyEliminator emptyInput
                return $ emptyInput :~> result
             else do
                let input = Sum (zip constructors branches)
                    handlers = map (:~> result) branches
                return $ foldr (:~>) result (input : handlers)
        Xsel _ _ _ -> failCheck "legacy Xsel has no proof-type semantics"

ensureConstructor :: ConsDesc -> Check ()
ensureConstructor (ConsDesc name arity) = do
    when (null name) $ failCheck "constructor descriptor has an empty name"
    ensureNonNegative "constructor arity" arity

ensureNonNegative :: String -> Int -> Check ()
ensureNonNegative description value =
    when (value < 0) $ failCheck $
        description ++ " is negative: " ++ show value

freshMeta :: Check ProofType
freshMeta = do
    checkState <- get
    let index = nextMeta checkState
    put checkState { nextMeta = index + 1 }
    return $ Meta index

freshMetas :: Int -> Check [ProofType]
-- Raw proof combinators retain their historical Int arities. They are checked
-- non-negative before this finite compatibility count reaches 'replicateM'.
freshMetas count = replicateM count freshMeta

addConstraint :: Constraint -> Check ()
addConstraint constraint =
    modify $ \ checkState -> checkState {
        constraints = constraint : constraints checkState
        }

unify :: ProofType -> ProofType -> Check ()
unify left right = do
    left' <- prune left
    right' <- prune right
    case (left', right') of
        (Meta first, Meta second) | first == second -> return ()
        (Meta index, proofType) -> bind index proofType
        (proofType, Meta index) -> bind index proofType
        (Product first, Product second) ->
            unifyLists "product" first second
        (Sum first, Sum second) -> do
            let firstConstructors = map fst first
                secondConstructors = map fst second
            unless (firstConstructors == secondConstructors) $
                mismatch left' right'
            unifyLists "sum" (map snd first) (map snd second)
        (EmptyType first, EmptyType second) | first == second -> return ()
        (firstArgument :~> firstResult,
         secondArgument :~> secondResult) -> do
            unify firstArgument secondArgument
            unify firstResult secondResult
        (Atom first, Atom second) | first == second -> return ()
        _ -> mismatch left' right'

unifyLists :: String -> [ProofType] -> [ProofType] -> Check ()
unifyLists description first second = do
    unless (length first == length second) $ failCheck $
        description ++ " arity mismatch: " ++
        show (length first) ++ " vs " ++ show (length second)
    sequence_ $ zipWith unify first second

bind :: Natural -> ProofType -> Check ()
bind index proofType = do
    cyclic <- occurs index proofType
    when cyclic $ failCheck $
        "cyclic proof type: t" ++ show index ++ " occurs in " ++
        showProofType proofType
    modify $ \ checkState -> checkState {
        substitutions = Map.insert index proofType
            (substitutions checkState)
        }

occurs :: Natural -> ProofType -> Check Bool
occurs index proofType = do
    proofType' <- prune proofType
    case proofType' of
        Meta other -> return $ index == other
        Product elements -> anyM (occurs index) elements
        Sum alternatives -> anyM (occurs index . snd) alternatives
        EmptyType _ -> return False
        argument :~> result -> do
            inArgument <- occurs index argument
            if inArgument then return True else occurs index result
        Atom _ -> return False

anyM :: (a -> Check Bool) -> [a] -> Check Bool
anyM _ [] = return False
anyM predicate (value : values) = do
    result <- predicate value
    if result then return True else anyM predicate values

prune :: ProofType -> Check ProofType
prune proofType@(Meta index) = do
    table <- gets substitutions
    case Map.lookup index table of
        Nothing -> return proofType
        Just replacement -> do
            replacement' <- prune replacement
            modify $ \ checkState -> checkState {
                substitutions = Map.insert index replacement'
                    (substitutions checkState)
                }
            return replacement'
prune proofType = return proofType

solveConstraints :: Check ()
solveConstraints = do
    pending <- gets constraints
    modify $ \ checkState -> checkState { constraints = [] }
    solve pending
  where
    solve [] = return ()
    solve pending = do
        results <- mapM solveConstraint pending
        let remaining = [constraint |
                (constraint, resolved) <- zip pending results, not resolved]
        if length remaining < length pending then
            solve remaining
         else
            defaultStuck remaining

    -- Every remaining constraint is stuck on an unsolved metavariable.  An
    -- empty eliminator whose input type is still free consumes a value the
    -- rest of the proof leaves unconstrained (for example, one ex falso
    -- result fed into another); any empty instantiation witnesses a valid
    -- typing, so choose Sum [] and resume.  A proof that also injects into
    -- that type still fails unification on the next pass.
    defaultStuck remaining =
        case [input | EmptyEliminator input <- remaining] of
            input : _ -> do
                unify input (Sum [])
                solve remaining
            [] -> failCheck $ "unresolved proof constraints: " ++
                show (length remaining)

solveConstraint :: Constraint -> Check Bool
solveConstraint constraint =
    case constraint of
        Injection result constructor index payload -> do
            result' <- prune result
            case result' of
                Meta _ -> return False
                Sum alternatives ->
                    case alternatives !? index of
                        Nothing -> failCheck $
                            "injection index " ++ show index ++
                            " is outside a sum with " ++
                            show (length alternatives) ++ " alternatives"
                        Just (expectedConstructor, branch) -> do
                            unless (constructor == expectedConstructor) $
                                failCheck $ "injection constructor mismatch: " ++
                                    show constructor ++ " vs " ++
                                    show expectedConstructor
                            unify payload branch
                            return True
                _ -> failCheck $ "injection result is not a sum: " ++
                    showProofType result'
        EmptyEliminator input -> do
            input' <- prune input
            case input' of
                Meta _ -> return False
                Sum [] -> return True
                EmptyType _ -> return True
                _ -> failCheck $ "empty eliminator received " ++
                    showProofType input'

fromFormula :: Formula -> ProofType
fromFormula (Conj formulas) = Product (map fromFormula formulas)
fromFormula (Disj alternatives) = Sum
    [(constructor, fromFormula formula) |
        (constructor, formula) <- alternatives]
fromFormula (Empty name) = EmptyType name
fromFormula (argument :-> result) =
    fromFormula argument :~> fromFormula result
fromFormula (PVar symbol) = Atom symbol

mismatch :: ProofType -> ProofType -> Check a
mismatch left right = failCheck $
    "proof type mismatch: " ++ showProofType left ++
    " vs " ++ showProofType right

showProofType :: ProofType -> String
showProofType (Meta index) = "t" ++ show index
showProofType (Product elements) =
    "(" ++ intercalate ", " (map showProofType elements) ++ ")"
showProofType (Sum alternatives) =
    "{" ++ intercalate " | "
        [show constructor ++ " " ++ showProofType branch |
            (constructor, branch) <- alternatives] ++ "}"
showProofType (EmptyType name) = "empty[" ++ show name ++ "]"
showProofType (argument :~> result) =
    "(" ++ showProofType argument ++ " -> " ++ showProofType result ++ ")"
showProofType (Atom symbol) = show symbol

failCheck :: String -> Check a
failCheck = lift . Left
