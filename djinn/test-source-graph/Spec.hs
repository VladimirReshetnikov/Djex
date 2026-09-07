module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM_, unless)
import Data.List (nub, sort)
import qualified Data.Set as Set
import Data.Void (Void)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import qualified KindCases

import qualified Language.Haskell.Djex.Djinn as D
import Language.Haskell.Synthesis.Candidate (candidateOutput)
import Language.Haskell.Synthesis.Declaration
    (Declaration (..), DataConstructor (..), TypeParameter (..), ValueSignature (..))
import qualified Language.Haskell.Synthesis.Environment as E
import qualified Language.Haskell.Synthesis.Generated as G
import Language.Haskell.Synthesis.Kind (Kind (..))
import Language.Haskell.Synthesis.Name (Name, mkIdentifier, parseName)
import qualified Language.Haskell.Synthesis.Name as N
import qualified Language.Haskell.Synthesis.Query as Q
import qualified Language.Haskell.Synthesis.Search as S
import Language.Haskell.Synthesis.Type (Type (..))
import Language.Haskell.Synthesis.TypeAtom (alphaEquivalentClosedTypes)
import qualified Language.Haskell.Synthesis.TypedCandidate as C
import qualified Language.Haskell.Synthesis.TypedGenerated as T

-- This downstream-only suite asks the public producer for source authority.
-- It does not manufacture a graph or turn a missing graph into a passing case.
-- Search bounds are explicit; absence of a candidate is a test failure for a
-- positive case, not a vacuous successful graph traversal.
main :: IO ()
main = defaultMain $ testGroup "Djinn source-typed evidence graph"
    [ testGroup "ordinary source forms" ordinaryTests
    , testGroup "intrinsic list source authority" intrinsicListTests
    , testGroup "rank-N and impredicative source types" rankNTests
    , testGroup "Church source signatures" churchTests
    , testGroup "provider authority" providerTests
    , testGroup "result compatibility" compatibilityTests
    , testGroup "scope rejection" rejectionTests
    , KindCases.tests
    ]

type SourceDeclaration = Declaration String Void ()
type Graph = T.TermGraph D.DjinnTermGraphType String

options :: D.QueryOptions
options = D.defaultQueryOptions
    { D.optionSorted = False
    , D.optionCutoff = 16
    , D.optionBudget = Just 10000
    }

alternatives :: D.QueryOptions
alternatives = options {D.optionAlternatives = True, D.optionCutoff = 32}

name :: String -> Name
name spelling = either (error . show) id $ parseName spelling

variable :: String -> Type String
variable = TypeVariable

nominal :: String -> Type String
nominal = TypeConstructor . name

apply :: String -> [Type String] -> Type String
apply spelling = foldl TypeApplication (nominal spelling)

abstract :: String -> Int -> SourceDeclaration
abstract spelling arity = AbstractTypeDeclaration () (name spelling)
    $ foldr FunctionKind ProperTypeKind $ replicate arity ProperTypeKind

value :: String -> Type String -> SourceDeclaration
value spelling = ValueDeclaration . ValueSignature () (name spelling)

datatype :: String -> [String] -> [(String, [Type String])] -> SourceDeclaration
datatype spelling parameters constructors = DataTypeDeclaration () (name spelling)
    [TypeParameter parameter Nothing | parameter <- parameters]
    [DataConstructor () (name constructor) fields | (constructor, fields) <- constructors]

scopeDeclarations :: [SourceDeclaration]
scopeDeclarations = [abstract "F" 1, abstract "G" 2, abstract "G3" 3,
    abstract "Seed" 0, abstract "Token" 0]

seal :: [SourceDeclaration] -> IO D.DjinnSession
seal declarations = do
    environment <- right $ E.mkEnvironment declarations
    right $ D.mkDjinnSession environment

request :: D.DjinnSession -> D.QueryOptions -> String -> IO D.DjinnRequest
request session configured source = do
    target <- right $ mkIdentifier "graphCandidate"
    right $ D.parseDjinnRequest session configured target "source-graph-test" source

parseType :: String -> IO (Type String)
parseType source = do
    session <- seal []
    Q.requestGoal . D.djinnRequestQuery <$> request session options source

right :: Show failure => Either failure result -> IO result
right = either (fail . show) pure

queryGraphs
    :: [SourceDeclaration] -> D.QueryOptions -> String
    -> IO (D.DjinnRequest, D.DjinnTypedResult, [Graph])
queryGraphs declarations configured source = do
    session <- seal declarations
    query <- request session configured source
    result <- right $ D.runDjinnTypedQuery session query
    graphs <- checkResult query result
    pure (query, result, graphs)

checkResult :: D.DjinnRequest -> D.DjinnTypedResult -> IO [Graph]
checkResult query result = do
    let candidates = S.batchCandidates $ Q.resultSearch result
    assertBool "positive query returned no candidate" $ not $ null candidates
    assertEqual "graph collection changed logical evidence"
        Q.ValidatedCandidates (Q.resultEvidence result)
    mapM (checkCandidate query) candidates

checkCandidate :: D.DjinnRequest -> D.DjinnTypedCandidate -> IO Graph
checkCandidate query candidate = do
    graph <- case C.typedCandidateTermGraph candidate of
        Left absence -> fail $ "source graph unavailable: " ++ show absence
        Right graph -> pure graph
    let original = candidateOutput $ C.typedCandidateCompatibility candidate
        sourceQuery = D.djinnRequestQuery query
        nodes = T.termGraphNodes graph
    assertEqual "graph does not erase to its exact associated clause"
        original $ T.eraseTermGraphToFunctionClause (Q.requestTarget sourceQuery) graph
    root <- case T.lookupTermNode (T.termGraphRoot graph) graph of
        Nothing -> fail "sealed graph has no root"
        Just node -> pure node
    assertBool ("root lost the closed source goal: " ++ show (T.termNodeType root)) $
        alphaEquivalentClosedTypes (Q.requestGoal sourceQuery) (T.termNodeType root)
    assertBool "graph lost all source occurrences" $
        T.typedGraphSourceOccurrences (T.termGraphMetrics graph) > 0
    assertEqual "graph duplicated a node identity"
        (length nodes) (Set.size $ Set.fromList $ map fst nodes)
    assertEqual "a completed source graph contains a hole" [] $
        G.expressionHoles $ T.eraseTermGraph graph
    pure graph

positive :: String -> [SourceDeclaration] -> String -> TestTree
positive label declarations source = testCase label $ do
    _ <- queryGraphs declarations options source
    pure ()

ordinaryTests :: [TestTree]
ordinaryTests =
    [ positive "closed ordinary identity" [] "forall a. a -> a"
    , positive "distinct source variables in composition" []
        "forall a b c. (b -> c) -> (a -> b) -> a -> c"
    , positive "partial function forwarding" []
        "forall a b. (a -> b) -> a -> b"
    , positive "nested tuple construction" []
        "forall a b c. a -> b -> c -> ((a, b), c)"
    , positive "nested tuple elimination" []
        "forall a b c. ((a, b), c) -> (c, a)"
    , positive "nominal nullary constructor" tokenData "TokenData"
    , positive "nominal parameterized constructor" boxData "forall a. a -> Box a"
    , positive "nominal constructor field elimination" boxData "forall a. Box a -> a"
    , positive "nominal recursive container forwarding" chainData
        "forall a. Chain a -> Chain a"
    , testCase "sum elimination retains a real typed case" $ do
        (_, _, graphs) <- queryGraphs choiceData options
            "forall a b r. (a -> r) -> (b -> r) -> Choice a b -> r"
        assertBool "sum eliminator did not exercise a case graph" $
            any (any (isCase . T.termNodeForm . snd) . T.termGraphNodes) graphs
    , positive "empty datatype elimination" [datatype "Empty" [] []]
        "forall a. Empty -> a"
    ]
  where
    tokenData = [datatype "TokenData" [] [("TokenData", [])]]
    boxData = [datatype "Box" ["a"] [("Box", [variable "a"])]]
    choiceData = [datatype "Choice" ["a", "b"]
        [("FirstChoice", [variable "a"]), ("SecondChoice", [variable "b"])]]
    chainData = [datatype "Chain" ["a"]
        [("End", []), ("Next", [variable "a", apply "Chain" [variable "a"]])]]
    isCase T.TypedCase{} = True
    isCase _ = False

-- Use the actual builtin identities, not a nominal List/Nil/Cons lookalike.
-- Admitting this complete declaration authorizes the finite constructor view;
-- unknown or merely same-spelled families do not supply that authority.
intrinsicListTests :: [TestTree]
intrinsicListTests =
    [ testCase "the public session retains the exact builtin list declaration" $
        forM_ [Nothing, Just ProperTypeKind] $ \kind -> do
            let declaration = listDeclaration kind
            session <- seal [declaration]
            assertEqual "list admission changed its source declaration"
                [declaration] $ E.environmentDeclarations $ D.djinnSessionEnvironment session
    , testCase "forced polymorphic nil has an exact source graph" $ do
        (_, _, graphs) <- queryGraphs [listDeclaration Nothing] alternatives
            "forall a b. [a] -> [b]"
        forM_ graphs $ \graph -> do
            assertBool "forced nil did not retain the actual builtin [] global" $
                N.listName `elem` globalNames graph
        -- Case alternatives can now reconstruct an input list before returning
        -- nil. Such an intermediate (:) is not a payload of the unrelated
        -- output type; every graph above is checked at the full forall type.
        assertBool "the simple nil inhabitant was lost" $
            any (notElem N.consName . globalNames) graphs
    , testCase "list introduction retains the actual builtin cons global" $ do
        (_, _, graphs) <- queryGraphs [listDeclaration Nothing] alternatives
            "forall a. a -> [a] -> [a]"
        assertBool "no source-checked candidate actually introduced (:)" $
            any (\graph -> N.consName `elem` globalNames graph
                && N.consName `elem` G.expressionGlobals (T.eraseTermGraph graph)) graphs
    , testCase "builtin list cases retain forwarding and no false refutation" $ do
        let declarations = [listDeclaration Nothing]
        (_, _, forwarded) <- queryGraphs declarations alternatives
            "forall a. [a] -> [a]"
        assertBool "the exact recursive input was not forwarded" $
            any (null . globalNames) forwarded
        assertBool "the recursive view produced no checked case eliminator" $
            any (any (isCase . T.termNodeForm . snd) . T.termGraphNodes) forwarded
        session <- seal declarations
        query <- request session options "forall a. [a] -> a"
        result <- right $ D.runDjinnTypedQuery session query
        assertEqual "recursive input elimination invented an element" [] $
            S.batchCandidates $ Q.resultSearch result
        assertEqual "opaque recursive input supplied negative logical authority"
            Q.NoEvidence $ Q.resultEvidence result
        assertEqual "the finite recursive-input plans did not finish"
            (S.Completed S.Finished) $ S.batchProgress $ Q.resultSearch result
    , testCase "ordinary list observations retain full source graphs" $ do
        let flag = name "Flag"
            flagDeclaration = DataTypeDeclaration () flag []
                [DataConstructor () (name "No") [], DataConstructor () (name "Yes") []]
        forM_ ["forall a. [a] -> Flag", "forall a. a -> [a] -> a",
                "forall a. [a] -> [a] -> [a]"] $ \signature -> do
            (_, _, graphs) <- queryGraphs [listDeclaration Nothing, flagDeclaration]
                alternatives signature
            assertBool ("no source-checked list case for " ++ signature) $
                any (any (isCase . T.termNodeForm . snd) . T.termGraphNodes) graphs
    , testCase "one-layer tree inspection retains exact constructor identities" $ do
        let tree = name "Tree"
            element = TypeVariable "element"
            treeType = TypeApplication (TypeConstructor tree) element
            declaration = DataTypeDeclaration () tree [TypeParameter "element" Nothing]
                [ DataConstructor () (name "Tip") [element]
                , DataConstructor () (name "Fork") [treeType, treeType]
                ]
        (_, _, graphs) <- queryGraphs [declaration] alternatives
            "forall a. a -> Tree a -> a"
        assertBool "no checked tree case was synthesized" $
            any (any (isCase . T.termNodeForm . snd) . T.termGraphNodes) graphs
        (_, _, introduced) <- queryGraphs [declaration] alternatives
            "forall a. a -> Tree a"
        assertBool "case plans reopened a purely positive recursive constructor" $
            all (notElem (name "Fork") . globalNames) introduced
    , testCase "recursive cases compose with exact loaded providers" $ do
        let declarations = [listDeclaration Nothing, abstract "Seed" 0, abstract "Token" 0,
                value "fallback" (nominal "Token"),
                value "observe" (FunctionType (nominal "Seed") (nominal "Token"))]
        (_, _, graphs) <- queryGraphs declarations alternatives "[Seed] -> Token"
        assertBool "a recursive case lost its loaded observation provider" $
            any (\graph -> name "observe" `elem` globalNames graph
                && any (isCase . T.termNodeForm . snd) (T.termGraphNodes graph)) graphs
        assertBool "the empty branch invented an abstract default" $
            all (elem (name "fallback") . globalNames) graphs
    , testCase "one-layer mutual recursion retains opaque sibling fields" $ do
        let declarations =
                [ datatype "LeftTree" ["a"]
                    [("LeftLeaf", [variable "a"]), ("ToRight", [apply "RightTree" [variable "a"]])]
                , datatype "RightTree" ["a"]
                    [("ToLeft", [apply "LeftTree" [variable "a"]])]
                ]
        (_, _, graphs) <- queryGraphs declarations alternatives
            "forall a. a -> LeftTree a -> a"
        assertBool "no checked mutual-family case was synthesized" $
            any (any (isCase . T.termNodeForm . snd) . T.termGraphNodes) graphs
    ]
  where
    listDeclaration kind = DataTypeDeclaration () N.listName
        [TypeParameter "element" kind]
        [ DataConstructor () N.listName []
        , DataConstructor () N.consName
            [ TypeVariable "element"
            , TypeApplication (TypeConstructor N.listName) (TypeVariable "element")
            ]
        ]
    isCase T.TypedCase{} = True
    isCase _ = False

rankNTests :: [TestTree]
rankNTests =
    [ positive "higher-rank argument instantiation" scopeDeclarations
        "forall a. (forall b. F b -> G b b) -> F a -> G a a"
    , positive "one local scheme used at two distinct source types" scopeDeclarations
        "forall a b. (forall x. F x -> G x x) -> F a -> F b -> (G a a, G b b)"
    , positive "forall result after an ordinary argument" []
        "forall a. a -> (forall b. b -> b)"
    , positive "nested polymorphic forwarding" scopeDeclarations
        "(forall a. a -> a) -> ((forall b. b -> b) -> Token) -> Token"
        -- Token is intentionally abstract: no unrelated constructor proves it.
    , positive "closed impredicative result" scopeDeclarations
        "(forall a. F a) -> F (forall b. b -> b)"
    , positive "correlated alpha-equivalent images" scopeDeclarations
        "(forall a. G a (F a)) -> G (forall b. b -> b) (F (forall c. c -> c))"
    , positive "construct an impredicative argument" scopeDeclarations
        "(forall a. a -> F a) -> F (forall b. b -> b)"
    , positive "construct two distinct polymorphic arguments" scopeDeclarations
        "(forall a b. a -> b -> G a b) -> G (forall c. c -> c) (forall d e. d -> e -> d)"
    , positive "shadowed source type binders stay distinct" scopeDeclarations
        "forall a. a -> ((forall a. a -> a) -> F a) -> F a"
    , positive "ambient rigid identity beneath a forall" scopeDeclarations
        "forall x. (forall a. F (forall b. b -> a)) -> F (forall c. c -> (forall d. x -> d -> d))"
    , positive "higher-kinded impredicative substitution" scopeDeclarations
        "(forall f a. G (f a) a) -> G (F (forall z. z -> z)) (forall y. y -> y)"
    , positive "alternating term and forall source spine" scopeDeclarations
        "forall seed. seed -> (seed -> forall a. a -> seed -> forall b. b -> G a b) -> G (forall c. c -> c) (forall d. d -> d)"
    , positive "polymorphic constructor field" [datatype "PolyBox" []
        [("PolyBox", [ForallType ["a"] [] $ FunctionType (variable "a") (variable "a")])]]
        "(forall a. a -> a) -> PolyBox"
    ]

-- These are type/graph checks for the six independently behavior-tested
-- signatures. Type inhabitation alone does not establish not/map/filter/etc.
churchTests :: [TestTree]
churchTests = [positive operation [] source | (operation, source) <-
    [ ("not", bool ++ " -> " ++ bool)
    , ("swap", "forall a b. " ++ pair "a" "b" ++ " -> " ++ pair "b" "a")
    , ("map", "forall a b. (a -> b) -> " ++ list "a" ++ " -> " ++ list "b")
    , ("append", "forall a. " ++ list "a" ++ " -> " ++ list "a" ++ " -> " ++ list "a")
    , ("reverse", "forall a. " ++ list "a" ++ " -> " ++ list "a")
    , ("filter", "forall a. (a -> " ++ bool ++ ") -> " ++ list "a" ++ " -> " ++ list "a")
    ]]
  where
    bool = "(forall r. r -> r -> r)"
    pair a b = "(forall r. (" ++ a ++ " -> " ++ b ++ " -> r) -> r)"
    list a = "(forall r. (" ++ a ++ " -> r -> r) -> r -> r)"

providerTests :: [TestTree]
providerTests =
    [ testCase "exact global identity and source scheme" $ do
        let provider = name "sourceValue"
        (_, _, graphs) <- queryGraphs
            [abstract "Token" 0, value "sourceValue" $ nominal "Token"] options "Token"
        forM_ graphs $ \graph -> assertEqual "global identity was reconstructed by spelling"
            [provider] $ globalNames graph
    , testCase "distinct provider candidates retain their own graph association" $ do
        (_, _, graphs) <- queryGraphs [abstract "Token" 0,
            value "firstValue" $ nominal "Token", value "secondValue" $ nominal "Token"]
            alternatives "Token"
        assertEqual "one graph was borrowed for another provider"
            (sort [name "firstValue", name "secondValue"])
            (sort $ nub $ concatMap globalNames graphs)
    , testCase "polymorphic provider retains its unspecialized source type" $ do
        scheme <- parseType "forall a. a -> a"
        (_, _, graphs) <- queryGraphs [abstract "Token" 0, value "polyIdentity" scheme]
            alternatives "Token -> Token"
        let providerNodes = [T.termNodeType node | graph <- graphs,
                (_, node) <- T.termGraphNodes graph,
                T.TypedGlobal _ provider <- [T.termNodeForm node], provider == name "polyIdentity"]
        assertBool "provider-use control returned only a local identity" $ not $ null providerNodes
        forM_ providerNodes $ \ty -> assertBool "global lost its complete quantified scheme" $
            alphaEquivalentClosedTypes scheme ty
    , testCase "complete correlated provider vector" $ do
        scheme <- parseType "forall a b. a -> b -> G a b"
        firstImage <- parseType "forall x. x -> x"
        secondImage <- parseType "forall x y. x -> y -> x"
        session <- seal $ scopeDeclarations ++ [value "buildPair" scheme]
        query <- request session options
            "G (forall x. x -> x) (forall y z. y -> z -> y)"
        let assignment = Q.ProviderInstantiationAssignment (name "buildPair") [firstImage, secondImage]
            kinded = Q.KindedProviderInstantiationAssignment (name "buildPair")
                [(ProperTypeKind, firstImage), (ProperTypeKind, secondImage)]
        forM_
            [ (D.runDjinnTypedQueryWithInstantiationAssignments session [assignment] query,
                D.runDjinnQueryWithInstantiationAssignments session [assignment] query)
            , (D.runDjinnTypedQueryWithKindedInstantiationAssignments session [kinded] query,
                D.runDjinnQueryWithKindedInstantiationAssignments session [kinded] query)
            ] $ \(typedRun, legacyRun) -> do
                result <- right typedRun
                legacy <- right legacyRun
                assertEqual "nonempty assignment runner changed compatibility"
                    legacy $ C.typedQueryResultCompatibility result
                graphs <- checkResult query result
                forM_ graphs $ \graph -> do
                    assertEqual "exact assignment was attached to the wrong provider"
                        [name "buildPair"] $ globalNames graph
                    let exactVector (owner, [firstType, secondType]) =
                            owner == name "buildPair"
                                && alphaEquivalentClosedTypes firstImage firstType
                                && alphaEquivalentClosedTypes secondImage secondType
                        exactVector _ = False
                    assertBool "the complete ordered impredicative source vector is missing" $
                        any exactVector [spine | (nodeId, _) <- T.termGraphNodes graph,
                            Just spine <- [typeApplicationSpine graph nodeId]]
    , testCase "bounded alternatives retain an actual complete visible assignment" $ do
        scheme <- parseType "forall a b. Token"
        firstImage <- parseType "forall x. x -> x"
        secondImage <- parseType "forall x y. x -> y -> x"
        session <- seal $ scopeDeclarations ++ [value "buildPair" scheme]
        query <- request session alternatives "Token"
        let assignment = Q.ProviderInstantiationAssignment (name "buildPair") [firstImage, secondImage]
            kinded = Q.KindedProviderInstantiationAssignment (name "buildPair")
                [(ProperTypeKind, firstImage), (ProperTypeKind, secondImage)]
            selectedExactly expected (argument, selected) =
                alphaEquivalentClosedTypes expected selected &&
                    case G.visibleTypeArgumentPatternType argument of
                        Just annotated -> alphaEquivalentClosedTypes expected annotated
                        Nothing -> False
            exactVisibleVector (owner, [firstSlot, secondSlot]) =
                owner == name "buildPair" && selectedExactly firstImage firstSlot &&
                    selectedExactly secondImage secondSlot
            exactVisibleVector _ = False
        -- These slots cannot be recovered from the result type. The
        -- separate construction test above checks correlated impredicative
        -- arguments; this one requires an actual complete visible vector.
        forM_
            [ (D.runDjinnTypedQueryWithInstantiationAssignments session [assignment] query,
                D.runDjinnQueryWithInstantiationAssignments session [assignment] query)
            , (D.runDjinnTypedQueryWithKindedInstantiationAssignments session [kinded] query,
                D.runDjinnQueryWithKindedInstantiationAssignments session [kinded] query)
            ] $ \(typedRun, legacyRun) -> do
                result <- right typedRun
                legacy <- right legacyRun
                assertEqual "visible assignment alternatives changed compatibility"
                    legacy $ C.typedQueryResultCompatibility result
                graphs <- checkResult query result
                assertBool "no complete ordered visible assignment occurred in the bounded alternatives" $
                    any exactVisibleVector [spine | graph <- graphs,
                        (nodeId, _) <- T.termGraphNodes graph,
                        Just spine <- [visibleTypeApplicationSpine graph nodeId]]
    , testCase "candidate-type evidence retains its exact provider" $ do
        scheme <- parseType "forall a. a -> F a"
        selected <- parseType "forall x. x -> x"
        session <- seal $ scopeDeclarations ++ [value "makeF" scheme]
        query <- request session options "F (forall x. x -> x)"
        let evidence = Q.ProviderInstantiationCandidate (name "makeF") selected
        typed <- right $ D.runDjinnTypedQueryWithInstantiationCandidates session [evidence] query
        legacy <- right $ D.runDjinnQueryWithInstantiationCandidates session [evidence] query
        assertEqual "candidate evidence changed compatibility" legacy $
            C.typedQueryResultCompatibility typed
        graphs <- checkResult query typed
        forM_ graphs $ \graph -> assertEqual "candidate evidence borrowed provider authority"
            [name "makeF"] $ globalNames graph
    , testCase "a rebuilt session cannot reuse another provider source scheme" $ do
        firstSession <- seal [abstract "A" 0, abstract "B" 0, value "selected" $ nominal "A"]
        secondSession <- seal [abstract "A" 0, abstract "B" 0, value "selected" $ nominal "B"]
        query <- request firstSession options "A"
        firstResult <- right $ D.runDjinnTypedQuery firstSession query
        _ <- checkResult query firstResult
        secondResult <- right $ D.runDjinnTypedQuery secondSession query
        assertEqual "a source graph supplied stale session authority" [] $
            S.batchCandidates $ Q.resultSearch secondResult
    ]

globalNames :: Graph -> [Name]
globalNames graph = nub [provider | (_, node) <- T.termGraphNodes graph,
    T.TypedGlobal _ provider <- [T.termNodeForm node]]

typeApplicationSpine :: Graph -> T.TermNodeId -> Maybe (Name, [D.DjinnTermGraphType])
typeApplicationSpine graph nodeId = do
    node <- T.lookupTermNode nodeId graph
    case T.termNodeForm node of
        T.TypedGlobal _ provider -> Just (provider, [])
        T.TypedVisibleTypeApplication _ function _ witness -> do
            (provider, selected) <- typeApplicationSpine graph function
            pure (provider, selected ++ [T.typeApplicationSelected witness])
        T.TypedImplicitTypeApplication _ function witness -> do
            (provider, selected) <- typeApplicationSpine graph function
            pure (provider, selected ++ [T.implicitTypeApplicationSelected witness])
        _ -> Nothing

-- Only contiguous visible selections rooted at the exact global qualify.
-- An erased selection, term application, local alias, or introduction ends
-- this observation; none can borrow the original provider's visible slots.
visibleTypeApplicationSpine
    :: Graph -> T.TermNodeId
    -> Maybe (Name, [(G.VisibleTypeArgument, D.DjinnTermGraphType)])
visibleTypeApplicationSpine graph nodeId = do
    node <- T.lookupTermNode nodeId graph
    case T.termNodeForm node of
        T.TypedGlobal _ provider -> Just (provider, [])
        T.TypedVisibleTypeApplication _ function argument witness -> do
            (provider, selected) <- visibleTypeApplicationSpine graph function
            pure (provider, selected ++ [(argument, T.typeApplicationSelected witness)])
        _ -> Nothing

compatibilityTests :: [TestTree]
compatibilityTests =
    [ testCase "all four typed runners preserve their complete legacy results" $ do
        session <- seal []
        query <- request session alternatives "forall a. a -> a -> a"
        let pairs =
                [ (D.runDjinnTypedQuery session query, D.runDjinnQuery session query)
                , (D.runDjinnTypedQueryWithInstantiationCandidates session [] query,
                    D.runDjinnQueryWithInstantiationCandidates session [] query)
                , (D.runDjinnTypedQueryWithInstantiationAssignments session [] query,
                    D.runDjinnQueryWithInstantiationAssignments session [] query)
                , (D.runDjinnTypedQueryWithKindedInstantiationAssignments session [] query,
                    D.runDjinnQueryWithKindedInstantiationAssignments session [] query)
                ]
        forM_ pairs $ \(typedRun, legacyRun) -> do
            typed <- right typedRun
            legacy <- right legacyRun
            assertEqual "typed projection changed output/evidence/progress/metadata"
                legacy $ C.typedQueryResultCompatibility typed
            _ <- checkResult query typed
            pure ()
    , testCase "compatibility projection does not force an unobserved typed tail" $ do
        session <- seal []
        query <- request session options "forall a. a -> a"
        typed <- right $ D.runDjinnTypedQuery session query
        case S.batchCandidates $ Q.resultSearch typed of
            [] -> fail "lazy projection control has no first candidate"
            candidate : _ -> do
                let batch = (Q.resultSearch typed)
                        {S.batchCandidates = candidate : error "typed tail forced"}
                poisoned <- right $ Q.mkQueryResult (Q.resultEvidence typed) batch
                let projected = C.typedQueryResultCompatibility poisoned
                case S.batchCandidates $ Q.resultSearch projected of
                    [] -> fail "projection lost its first candidate"
                    first : _ -> do
                        observed <- evaluate first
                        assertEqual "projection changed the observed first candidate"
                            (C.typedCandidateCompatibility candidate) observed
    , testCase "zero-choice query preserves operational and logical status" $ do
        session <- seal []
        query <- request session (options {D.optionBudget = Just 0})
            "forall a b c. (a -> b) -> (b -> c) -> a -> c"
        typed <- right $ D.runDjinnTypedQuery session query
        legacy <- right $ D.runDjinnQuery session query
        assertEqual "graph work changed zero-choice search" legacy $
            C.typedQueryResultCompatibility typed
    ]

rejectionTests :: [TestTree]
rejectionTests = [testCase label $ do
    session <- seal scopeDeclarations
    query <- request session options source
    result <- right $ D.runDjinnTypedQuery session query
    unless (null $ S.batchCandidates $ Q.resultSearch result) $
        fail "an invalid scope acquired a candidate and potential source graph"
    | (label, source) <-
        [ ("reject direct bound-variable escape",
            "(forall a. F (forall b. b -> a)) -> F (forall c. c -> c)")
        , ("reject correlated type-image mismatch",
            "(forall a. G a a) -> G (forall b. b -> b) (forall c d. c -> d -> c)")
        , ("reject specializing an ambient rigid binder",
            "forall a. a -> (forall b. b)")
        , ("reject construction of an empty polymorphic argument",
            "(forall a. a -> F a) -> F (forall b. b)")
        ]]
