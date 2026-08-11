--
-- | Cabal-private, independently checked LJT proof evidence.
--
-- A successful check retains the exact input environment, expected formula,
-- proof tree, and the fully pruned checker type of every proof node.  A root
-- unified with the expected formula is exact, while an intermediate node may
-- honestly retain correlated unconstrained metavariables which do not affect
-- the successful proof.  The evidence and node constructors stay hidden:
-- downstream code can traverse
-- and observe the tree owned by one successful check, but cannot construct or
-- re-associate a checked node or manufacture checked evidence.
--
-- Rank-N Haskell types selected as opaque logical atoms retain their exact
-- source trees through 'Symbol'.  Application nodes therefore preserve every
-- source, intermediate, and result type which the LJT checker itself knows.
-- The LJT 'Term' language has no visible-type-application node and receives no
-- selected visible-argument vector from @Djinn.Internal.Instantiation@, so this
-- evidence deliberately makes no claim about those later lowering choices.
-- Threading that plan context into a future sealing step is required before
-- Djinn can issue visible-instantiation witnesses.
module Djinn.Internal.ProofCheck.Evidence
    ( CheckedProofEvidence
    , CheckedProofNode
    , CheckedProofType
    , checkProofEnvironment
    , checkProofWithEvidence
    , checkedProofEnvironment
    , checkedProofExpectedType
    , checkedProofRoot
    , checkedProofNodeTerm
    , checkedProofNodeType
    , checkedProofNodeChildren
    , checkedProofTypeExactFormula
    , checkedProofTypeHasUnconstrained
    , checkedProofTypeUnconstrainedCount
    ) where

import Control.Monad (replicateM, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
    ( StateT, evalStateT, get, gets, modify, put )
import Data.List (intercalate, (!?))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Numeric.Natural (Natural)

import Djinn.Internal.LJTFormula
import Language.Haskell.Synthesis.Collection (firstDuplicate)

-- | One complete successful proof check.  There is intentionally no public
-- constructor, generic representation, or function which accepts a separate
-- term beside this value.
data CheckedProofEvidence = CheckedProofEvidence
    [(Symbol, Formula)]
    Formula
    CheckedProofNode

-- | One term occurrence paired with its final checker type and its exact
-- checked children.  Keeping the constructor private makes this association
-- constructible only by the successful checker path.
data CheckedProofNode = CheckedProofNode
    Term
    CheckedProofType
    [CheckedProofNode]

-- | Fully pruned type retained by the successful checker.
--
-- Its constructor and private metavariable identities stay hidden.  Repeated
-- occurrences of one unresolved identity therefore retain their correlation,
-- including below products, sums, and arrows, without allowing downstream
-- code to forge either that identity or a checked type association.
data CheckedProofType
    = CheckedUnconstrained Natural
    | CheckedProduct [CheckedProofType]
    | CheckedSum [(ConsDesc, CheckedProofType)]
    | CheckedEmptyType Symbol
    | CheckedProofType :~~> CheckedProofType
    | CheckedAtom Symbol

infixr 2 :~~>

-- | Recover the exact environment checked with this proof.  It is an
-- observation of the sealed association, not an input to another check.
checkedProofEnvironment :: CheckedProofEvidence -> [(Symbol, Formula)]
checkedProofEnvironment (CheckedProofEvidence environment _ _) = environment

-- | Recover the exact expected formula checked at the root.
checkedProofExpectedType :: CheckedProofEvidence -> Formula
checkedProofExpectedType (CheckedProofEvidence _ expected _) = expected

-- | Enter the checker-owned typed proof tree.
checkedProofRoot :: CheckedProofEvidence -> CheckedProofNode
checkedProofRoot (CheckedProofEvidence _ _ root) = root

-- | Observe the exact raw term occurrence associated with this node.
checkedProofNodeTerm :: CheckedProofNode -> Term
checkedProofNodeTerm (CheckedProofNode term _ _) = term

-- | Observe the fully pruned checker type associated with this node.
checkedProofNodeType :: CheckedProofNode -> CheckedProofType
checkedProofNodeType (CheckedProofNode _ proofType _) = proofType

-- | Traverse the exact checked children in source order.  Lambdas have one
-- child, applications have function then argument, and primitive combinators
-- have none.
checkedProofNodeChildren :: CheckedProofNode -> [CheckedProofNode]
checkedProofNodeChildren (CheckedProofNode _ _ children) = children

-- | Project a fully known checked type back to the historical logical type.
-- Any unresolved metavariable at any depth makes the result explicitly
-- unavailable; no formula is defaulted, guessed, or partially reconstructed.
checkedProofTypeExactFormula :: CheckedProofType -> Maybe Formula
checkedProofTypeExactFormula checkedType = case checkedType of
    CheckedUnconstrained _ -> Nothing
    CheckedProduct elements ->
        Conj <$> mapM checkedProofTypeExactFormula elements
    CheckedSum alternatives -> Disj <$> mapM exactAlternative alternatives
    CheckedEmptyType name -> Just $ Empty name
    argument :~~> result ->
        (:->) <$> checkedProofTypeExactFormula argument
            <*> checkedProofTypeExactFormula result
    CheckedAtom symbol -> Just $ PVar symbol
  where
    exactAlternative (constructor, branch) =
        fmap ((,) constructor) $ checkedProofTypeExactFormula branch

-- | Whether this checked type contains any explicitly unconstrained identity.
checkedProofTypeHasUnconstrained :: CheckedProofType -> Bool
checkedProofTypeHasUnconstrained =
    (> 0) . checkedProofTypeUnconstrainedCount

-- | Number of distinct unconstrained identities retained by this type.
-- Counting identities rather than occurrences preserves an observable account
-- of correlation: @t -> t@ has one unknown, not two unrelated holes.
checkedProofTypeUnconstrainedCount :: CheckedProofType -> Natural
checkedProofTypeUnconstrainedCount =
    fromIntegral . Set.size . unconstrainedIdentities

unconstrainedIdentities :: CheckedProofType -> Set.Set Natural
unconstrainedIdentities checkedType = case checkedType of
    CheckedUnconstrained identity -> Set.singleton identity
    CheckedProduct elements -> Set.unions $ map unconstrainedIdentities elements
    CheckedSum alternatives -> Set.unions $
        map (unconstrainedIdentities . snd) alternatives
    CheckedEmptyType _ -> Set.empty
    argument :~~> result -> Set.union
        (unconstrainedIdentities argument)
        (unconstrainedIdentities result)
    CheckedAtom _ -> Set.empty

data ProofType
    = Meta Natural
    | Product [ProofType]
    | Sum [(ConsDesc, ProofType)]
    | EmptyType Symbol
    | ProofType :~> ProofType
    | Atom Symbol
    deriving (Eq)

infixr 2 :~>

data InferredProofNode = InferredProofNode
    Term
    ProofType
    [InferredProofNode]

inferredProofType :: InferredProofNode -> ProofType
inferredProofType (InferredProofNode _ proofType _) = proofType

data Constraint
    = Injection ProofType ConsDesc Int ProofType
    | EmptyEliminator ProofType

data CheckState = CheckState
    { nextMeta :: Natural
    , substitutions :: Map.Map Natural ProofType
    , constraints :: [Constraint]
    }

type Check a = StateT CheckState (Either String) a
type Environment = [(Symbol, ProofType)]

initialState :: CheckState
initialState = CheckState 0 Map.empty []

-- | Independently check a proof and retain its exact final typed tree.
--
-- Node sealing happens only after goal unification and pending structural
-- constraints have succeeded.  The immutable final substitution table is
-- captured under the evidence constructor, then node and type finalization is
-- pure and lazy: the compatibility projection can discard evidence without a
-- second proof-tree traversal.  When evidence is observed, every substitution
-- is recursively pruned and genuinely unconstrained identities are retained.
checkProofWithEvidence
    :: [(Symbol, Formula)]
    -> Formula
    -> Term
    -> Either String CheckedProofEvidence
checkProofWithEvidence environment expected term =
    evalStateT check initialState
  where
    check = do
        ensureUniqueEnvironment environment
        lift $ validateTermMetadata term
        inferred <- infer
            (map (\(symbol, formula) -> (symbol, fromFormula formula))
                environment)
            term
        unify (inferredProofType inferred) (fromFormula expected)
        solveConstraints
        finalSubstitutions <- gets substitutions
        let checked = finalizeProofNode finalSubstitutions inferred
        return $ CheckedProofEvidence environment expected checked

-- | Reject an ambiguous external proof environment before either search or
-- independent checking gives its first occurrence precedence.
checkProofEnvironment :: [(Symbol, Formula)] -> Either String ()
checkProofEnvironment environment =
    case firstDuplicate $ map fst environment of
        Nothing -> Right ()
        Just symbol -> Left $
            "duplicate proof identity in environment: " ++ show symbol

ensureUniqueEnvironment :: [(Symbol, Formula)] -> Check ()
ensureUniqueEnvironment = lift . checkProofEnvironment

infer :: Environment -> Term -> Check InferredProofNode
infer environment term =
    case term of
        Var symbol ->
            case lookup symbol environment of
                Just proofType -> node proofType []
                Nothing -> failCheck $
                    "unbound proof variable: " ++ show symbol
        Lam binder body -> do
            argument <- freshMeta
            checkedBody <- infer ((binder, argument) : environment) body
            node (argument :~> inferredProofType checkedBody) [checkedBody]
        Apply function argument -> do
            checkedFunction <- infer environment function
            checkedArgument <- infer environment argument
            resultType <- freshMeta
            unify
                (inferredProofType checkedFunction)
                (inferredProofType checkedArgument :~> resultType)
            node resultType [checkedFunction, checkedArgument]
        Ctuple arity -> do
            elements <- freshMetas arity
            node (foldr (:~>) (Product elements) elements) []
        Csplit arity -> do
            elements <- freshMetas arity
            result <- freshMeta
            let handler = foldr (:~>) result elements
            node (handler :~> Product elements :~> result) []
        Cinj constructor index -> do
            payload <- freshMeta
            result <- freshMeta
            addConstraint $ Injection result constructor index payload
            node (payload :~> result) []
        Ccases constructors -> do
            branches <- mapM (const freshMeta) constructors
            result <- freshMeta
            if null constructors then do
                emptyInput <- freshMeta
                addConstraint $ EmptyEliminator emptyInput
                node (emptyInput :~> result) []
             else do
                let input = Sum $ zip constructors branches
                    handlers = map (:~> result) branches
                node (foldr (:~>) result (input : handlers)) []
        Xsel _ _ _ -> failCheck "legacy Xsel has no proof-type semantics"
  where
    node proofType children =
        return $ InferredProofNode term proofType children

freshMeta :: Check ProofType
freshMeta = do
    checkState <- get
    let index = nextMeta checkState
    put checkState { nextMeta = index + 1 }
    return $ Meta index

freshMetas :: Int -> Check [ProofType]
freshMetas count = replicateM count freshMeta

addConstraint :: Constraint -> Check ()
addConstraint constraint =
    modify $ \checkState -> checkState
        { constraints = constraint : constraints checkState }

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
    modify $ \checkState -> checkState
        { substitutions = Map.insert index proofType
            (substitutions checkState) }

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
            modify $ \checkState -> checkState
                { substitutions = Map.insert index replacement'
                    (substitutions checkState) }
            return replacement'
prune proofType = return proofType

-- | Pure lazy projection through the frozen final substitution table.
-- An unconstrained identity can occur in an unused intermediate domain even
-- though the proof's root is unified with an exact expected type.
finalizeProofType
    :: Map.Map Natural ProofType
    -> ProofType
    -> CheckedProofType
finalizeProofType finalSubstitutions proofType = case proofType of
    Meta index -> case Map.lookup index finalSubstitutions of
        Nothing -> CheckedUnconstrained index
        Just replacement ->
            finalizeProofType finalSubstitutions replacement
    Product elements -> CheckedProduct $
        map (finalizeProofType finalSubstitutions) elements
    Sum alternatives -> CheckedSum
        [ (constructor, finalizeProofType finalSubstitutions branch)
        | (constructor, branch) <- alternatives
        ]
    EmptyType name -> CheckedEmptyType name
    argument :~> result ->
        finalizeProofType finalSubstitutions argument :~~>
            finalizeProofType finalSubstitutions result
    Atom symbol -> CheckedAtom symbol

-- | Package the source tree with lazy type and child projections.  Constructing
-- the evidence root does not enter either projection, and mapping the children
-- does not enter the tail until a consumer traverses it.
finalizeProofNode
    :: Map.Map Natural ProofType
    -> InferredProofNode
    -> CheckedProofNode
finalizeProofNode finalSubstitutions
        (InferredProofNode term proofType children) =
    CheckedProofNode
        term
        (finalizeProofType finalSubstitutions proofType)
        (map (finalizeProofNode finalSubstitutions) children)

solveConstraints :: Check ()
solveConstraints = do
    pending <- gets constraints
    modify $ \checkState -> checkState { constraints = [] }
    solve pending
  where
    solve [] = return ()
    solve pending = do
        results <- mapM solveConstraint pending
        let remaining =
                [ constraint
                | (constraint, resolved) <- zip pending results
                , not resolved
                ]
        if length remaining < length pending
            then solve remaining
            else defaultStuck remaining

    defaultStuck remaining =
        case [input | EmptyEliminator input <- remaining] of
            input : _ -> do
                unify input (Sum [])
                solve remaining
            [] -> failCheck $
                "unresolved proof constraints: " ++ show (length remaining)

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
                                failCheck $
                                    "injection constructor mismatch: " ++
                                    show constructor ++ " vs " ++
                                    show expectedConstructor
                            unify payload branch
                            return True
                _ -> failCheck $
                    "injection result is not a sum: " ++
                    showProofType result'
        EmptyEliminator input -> do
            input' <- prune input
            case input' of
                Meta _ -> return False
                Sum [] -> return True
                EmptyType _ -> return True
                _ -> failCheck $
                    "empty eliminator received " ++ showProofType input'

fromFormula :: Formula -> ProofType
fromFormula formula = case formula of
    Conj formulas -> Product $ map fromFormula formulas
    Disj alternatives -> Sum
        [ (constructor, fromFormula branch)
        | (constructor, branch) <- alternatives
        ]
    Empty name -> EmptyType name
    argument :-> result -> fromFormula argument :~> fromFormula result
    PVar symbol -> Atom symbol

mismatch :: ProofType -> ProofType -> Check a
mismatch left right = failCheck $
    "proof type mismatch: " ++ showProofType left ++
    " vs " ++ showProofType right

showProofType :: ProofType -> String
showProofType proofType = case proofType of
    Meta index -> "t" ++ show index
    Product elements ->
        "(" ++ intercalate ", " (map showProofType elements) ++ ")"
    Sum alternatives ->
        "{" ++ intercalate " | "
            [ show constructor ++ " " ++ showProofType branch
            | (constructor, branch) <- alternatives
            ] ++ "}"
    EmptyType name -> "empty[" ++ show name ++ "]"
    argument :~> result ->
        "(" ++ showProofType argument ++ " -> " ++
        showProofType result ++ ")"
    Atom symbol -> show symbol

failCheck :: String -> Check a
failCheck = lift . Left
