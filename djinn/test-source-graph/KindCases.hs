module KindCases (tests) where

import Control.Monad (forM_)
import Data.Either (isLeft)
import Data.List (isSuffixOf)
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Language.Haskell.Djex.Djinn as D
import Language.Haskell.Synthesis.Candidate (candidateOutput)
import Language.Haskell.Synthesis.Declaration
  ( Declaration (..), ValueSignature (..) )
import qualified Language.Haskell.Synthesis.Environment as E
import qualified Language.Haskell.Synthesis.Generated as G
import Language.Haskell.Synthesis.Kind (Kind (..))
import Language.Haskell.Synthesis.Name (Name, mkIdentifier, parseName)
import qualified Language.Haskell.Synthesis.Query as Q
import qualified Language.Haskell.Synthesis.Search as S
import Language.Haskell.Synthesis.Type (Type (..))
import Language.Haskell.Synthesis.TypeAtom (alphaEquivalentClosedTypes)
import qualified Language.Haskell.Synthesis.TypedCandidate as C
import qualified Language.Haskell.Synthesis.TypedGenerated as T

type Declaration' = Declaration String Void ()
type Graph = T.TermGraph D.DjinnTermGraphType String

-- Public-producer controls: no forged graph or private kind-checker entrance.
-- Invalid assignments must fail at the checked request boundary; positive
-- assignments must actually occur in a graph with the original exact clause.
tests :: TestTree
tests = testGroup "source graph kinds and exact provider ownership"
  [ testCase "ordinary vacuous default rejects a constructor kind; exact owner accepts it" $ do
      session <- seal [value "vacuous" $ ForallType ["unused"] [] token]
      query <- request session "Token"
      assertBool "ordinary vacuous binder acquired a higher kind" $ isLeft $
        D.runDjinnTypedQueryWithInstantiationAssignments session
          [Q.ProviderInstantiationAssignment (name "vacuous") [unaryConstructor]] query
      let evidence = [assignment "vacuous" [(unaryKind, unaryConstructor)]]
      result <- right $ D.runDjinnTypedQueryWithKindedInstantiationAssignments session evidence query
      legacy <- right $ D.runDjinnQueryWithKindedInstantiationAssignments session evidence query
      assertEqual "typed admission changed the historical candidate/evidence result"
        legacy $ C.typedQueryResultCompatibility result
      assertEqual "the explicit three-way soundness control changed its candidate count" 3 $
        length $ S.batchCandidates $ Q.resultSearch result
      graphs <- checkedVacuousKindMatrix query result "vacuous"
      requireVector "validated higher-kind selection disappeared" graphs
        "vacuous" [unaryConstructor]

  , testCase "distinct providers retain their own incompatible vacuous kind vectors" $ do
      session <- seal
        [ value "higherOwner" $ ForallType ["slot"] [] token
        , value "properOwner" $ ForallType ["slot"] [] token
        ]
      query <- request session "Token"
      let evidence =
            [ assignment "higherOwner" [(unaryKind, unaryConstructor)]
            , assignment "properOwner" [(ProperTypeKind, atom)]
            ]
      result <- right $ D.runDjinnTypedQueryWithKindedInstantiationAssignments session evidence query
      legacy <- right $ D.runDjinnQueryWithKindedInstantiationAssignments session evidence query
      assertEqual "typed owner checking changed the historical candidate/evidence result"
        legacy $ C.typedQueryResultCompatibility result
      graphs <- checkedVacuousKindMatrix query result "higherOwner"
      requireVector "higher owner lost its own kind evidence" graphs "higherOwner" [unaryConstructor]
      requireVector "proper owner borrowed another provider's kind" graphs "properOwner" [atom]
      assertBool "a neighboring valid override authorized an invalid owner" $ isLeft $
        D.runDjinnTypedQueryWithKindedInstantiationAssignments session
          [ assignment "higherOwner" [(unaryKind, unaryConstructor)]
          , assignment "properOwner" [(ProperTypeKind, unaryConstructor)]
          ] query

  , testCase "a complete visible assignment retains both correlated source positions" $ do
      session <- seal [value "mixed" $ ForallType ["payload", "hidden"] [] $
        FunctionType (TypeVariable "payload") token]
      query <- request session "A -> Token"
      let evidence = [Q.ProviderInstantiationAssignment (name "mixed") [atom, identityType]]
      result <- right $ D.runDjinnTypedQueryWithInstantiationAssignments session evidence query
      legacy <- right $ D.runDjinnQueryWithInstantiationAssignments session evidence query
      assertEqual "typed complete assignment changed the historical result"
        legacy $ C.typedQueryResultCompatibility result
      graphs <- checkedGraphs query result
      let expected (owner, [(Just firstArgument, firstType), (Just secondArgument, secondType)]) =
            owner == name "mixed"
              && maybe False (alphaEquivalentClosedTypes atom)
                  (G.visibleTypeArgumentPatternType firstArgument)
              && maybe False (alphaEquivalentClosedTypes identityType)
                  (G.visibleTypeArgumentPatternType secondArgument)
              && alphaEquivalentClosedTypes atom firstType
              && alphaEquivalentClosedTypes identityType secondType
          expected _ = False
      assertBool "the complete @A / @Id source prefix was absent or its slots were exchanged" $
        any expected $ concatMap spines graphs
      -- This public test requires a supplied complete assignment to occur in
      -- search output. The private source-checker suite separately certifies
      -- the exact @_ / @Id grammar without assuming a search prefix emits it.

  , testCase "a non-vacuous binder rejects an incompatible external override" $ do
      let scheme = ForallType ["f", "a"] [] $ FunctionType
            (TypeApplication (TypeVariable "f") $ TypeVariable "a") token
      session <- seal [value "nonVacuous" scheme]
      query <- request session "F A -> Token"
      assertBool "a used type-constructor binder was given proper kind" $ isLeft $
        D.runDjinnTypedQueryWithKindedInstantiationAssignments session
          [assignment "nonVacuous" [(ProperTypeKind, atom), (ProperTypeKind, atom)]] query
      result <- right $ D.runDjinnTypedQueryWithKindedInstantiationAssignments session
        [assignment "nonVacuous" [(unaryKind, unaryConstructor), (ProperTypeKind, atom)]] query
      graphs <- checkedGraphs query result
      requireVector "valid non-vacuous kinds did not produce their exact graph" graphs
        "nonVacuous" [unaryConstructor, atom]

  , testCase "an impredicative proper-type selection retains its quantified source" $ do
      session <- seal [value "makeBox" $ ForallType ["p"] [] $
        FunctionType (TypeVariable "p") (applied "Box" [TypeVariable "p"])]
      query <- request session "Box (forall a. a -> a)"
      result <- right $ D.runDjinnTypedQueryWithKindedInstantiationAssignments session
        [assignment "makeBox" [(ProperTypeKind, identityType)]] query
      graphs <- checkedGraphs query result
      requireVector "the proper impredicative selection was erased" graphs "makeBox" [identityType]

  , testCase "same-spelled lexical binders may have different source kinds" $ do
      session <- seal []
      query <- request session $
        "(forall f. f -> Box f) -> " ++
        "(forall f a. f a -> Mark (f a)) -> A -> F A -> (Box A, Mark (F A))"
      result <- right $ D.runDjinnTypedQuery session query
      graphs <- checkedGraphs query result
      assertBool "higher-rank local applications lost inferred selections" $
        any (any isImplicit . map (T.termNodeForm . snd) . T.termGraphNodes) graphs

  , testCase "nested binder kinds share the exact ambient higher-kind variable" $ do
      session <- seal []
      query <- request session $
        "forall h. (forall f. f h -> Mark (f h)) -> Outer h -> Mark (Outer h)"
      result <- right $ D.runDjinnTypedQuery session query
      graphs <- checkedGraphs query result
      let selectedOuter node = case T.termNodeForm node of
            T.TypedImplicitTypeApplication _ _ witness ->
              alphaEquivalentClosedTypes (nominal "Outer") $ T.implicitTypeApplicationSelected witness
            T.TypedVisibleTypeApplication _ _ _ witness ->
              alphaEquivalentClosedTypes (nominal "Outer") $ T.typeApplicationSelected witness
            _ -> False
      assertBool "the shared ambient h did not retain its higher-order instantiation" $
        any (any (selectedOuter . snd) . T.termGraphNodes) graphs
  ]

options :: D.QueryOptions
options = D.defaultQueryOptions
  { D.optionAlternatives = True
  , D.optionStrategy = D.Interleave
  , D.optionSorted = False
  , D.optionCutoff = 128
  , D.optionBudget = Just 100000
  }

name :: String -> Name
name = either (error . show) id . parseName

nominal :: String -> Type String
nominal = TypeConstructor . name

applied :: String -> [Type String] -> Type String
applied spelling = foldl TypeApplication $ nominal spelling

atom, token, unaryConstructor, identityType :: Type String
atom = nominal "A"
token = nominal "Token"
unaryConstructor = nominal "F"
identityType = ForallType ["identityArgument"] [] $
  FunctionType (TypeVariable "identityArgument") (TypeVariable "identityArgument")

unaryKind :: Kind Void
unaryKind = FunctionKind ProperTypeKind ProperTypeKind

value :: String -> Type String -> Declaration'
value spelling = ValueDeclaration . ValueSignature () (name spelling)

assignment :: String -> [(Kind Void, Type String)] -> Q.KindedProviderInstantiationAssignment String
assignment spelling = Q.KindedProviderInstantiationAssignment $ name spelling

seal :: [Declaration'] -> IO D.DjinnSession
seal extra = do
  environment <- right $ E.mkEnvironment $
    [ AbstractTypeDeclaration () (name "A") ProperTypeKind
    , AbstractTypeDeclaration () (name "Token") ProperTypeKind
    , AbstractTypeDeclaration () (name "F") unaryKind
    , AbstractTypeDeclaration () (name "Box") unaryKind
    , AbstractTypeDeclaration () (name "Mark") unaryKind
    , AbstractTypeDeclaration () (name "Outer") $ FunctionKind unaryKind ProperTypeKind
    ] ++ extra
  right $ D.mkDjinnSession environment

request :: D.DjinnSession -> String -> IO D.DjinnRequest
request session source = do
  target <- right $ mkIdentifier "kindCandidate"
  right $ D.parseDjinnRequest session options target "source-graph-kind-case" source

right :: Show failure => Either failure result -> IO result
right = either (fail . show) pure

checkedGraphs :: D.DjinnRequest -> D.DjinnTypedResult -> IO [Graph]
checkedGraphs query result = do
  let candidates = S.batchCandidates $ Q.resultSearch result
  assertBool "positive kind query returned no candidates" $ not $ null candidates
  graphs <- mapM graph candidates
  forM_ (zip candidates graphs) $ uncurry $ checkGraph query
  pure graphs
 where
  graph candidate = case C.typedCandidateTermGraph candidate of
    Left absence -> fail $ "positive source kind graph unavailable: " ++ show absence
      ++ "; exact clause: " ++ show (candidateOutput $ C.typedCandidateCompatibility candidate)
    Right typed -> pure typed

checkGraph :: D.DjinnRequest -> D.DjinnTypedCandidate -> Graph -> IO ()
checkGraph query candidate typed = do
  let original = candidateOutput $ C.typedCandidateCompatibility candidate
  assertEqual "kind evidence changed the exact generated clause" original $
    T.eraseTermGraphToFunctionClause (Q.requestTarget $ D.djinnRequestQuery query) typed
  root <- maybe (fail "sealed kind graph lost its root") pure $
    T.lookupTermNode (T.termGraphRoot typed) typed
  assertBool "kind graph lost the closed original target" $
    alphaEquivalentClosedTypes (Q.requestGoal $ D.djinnRequestQuery query) $ T.termNodeType root

-- Historical transport alternatives are preserved by the typed facade. A
-- bare higher-kind provider can infer its admitted F selection, whereas its
-- already-specified @Token alternative conflicts with that same owner kind.
-- Only that exact known clause may lack a graph, with this exact kind error;
-- every other retained clause must have its own valid graph. No Left is skipped.
checkedVacuousKindMatrix :: D.DjinnRequest -> D.DjinnTypedResult -> String -> IO [Graph]
checkedVacuousKindMatrix query result higherOwner = do
  let candidates = S.batchCandidates $ Q.resultSearch result
      expressionOf = G.functionClauseExpression . candidateOutput . C.typedCandidateCompatibility
      higherGlobal = G.Global $ name higherOwner
      higherVisible = G.VisibleTypeApplication higherGlobal $
        either (error . show) id $ G.specifiedVisibleTypeArgument unaryConstructor
      rejectedExpression = G.VisibleTypeApplication (G.Global $ name higherOwner) $
        either (error . show) id $ G.specifiedVisibleTypeArgument token
      isNegative candidate = expressionOf candidate == rejectedExpression
      exactMismatch =
        "KindMismatch ProperTypeKind (FunctionKind ProperTypeKind ProperTypeKind)\""
  assertEqual "the historical incompatible @Token control disappeared or duplicated" 1 $
    length $ filter isNegative candidates
  assertEqual "the compatible bare higher-kind provider disappeared or duplicated" 1 $
    length $ filter ((== higherGlobal) . expressionOf) candidates
  assertEqual "the compatible specified higher-kind provider disappeared or duplicated" 1 $
    length $ filter ((== higherVisible) . expressionOf) candidates
  graphs <- fmap concat $ mapM (\candidate ->
    if isNegative candidate
      then do
        case C.typedCandidateTermGraph candidate of
          Left (D.DjinnTermGraphSourceTypingFailure detail) -> assertBool
            ("incompatible visible selection failed for an unrelated reason: " ++ detail) $
              exactMismatch `isSuffixOf` detail
          Left absence -> assertFailure $ "incompatible visible selection lost its source-kind reason: " ++ show absence
          Right _ -> assertFailure "a known proper selection acquired the owner's higher-kind graph authority"
        pure []
      else case C.typedCandidateTermGraph candidate of
        Left absence -> fail $ "source-compatible candidate lost its graph: " ++ show absence
          ++ "; exact clause: " ++ show (candidateOutput $ C.typedCandidateCompatibility candidate)
        Right graph -> checkGraph query candidate graph >> pure [graph]) candidates
  assertEqual "the soundness matrix silently omitted a retained candidate"
    (length candidates) (length graphs + 1)
  pure graphs

requireVector :: String -> [Graph] -> String -> [Type String] -> IO ()
requireVector message graphs provider expected = assertBool message $
  any matches $ concatMap spines graphs
 where
  matches (owner, arguments) = owner == name provider
    && length arguments == length expected
    && and (zipWith alphaEquivalentClosedTypes expected $ map snd arguments)

spines :: Graph -> [(Name, [(Maybe G.VisibleTypeArgument, D.DjinnTermGraphType)])]
spines graph = [spine | (owner, _) <- T.termGraphNodes graph,
  Just spine <- [walk owner]]
 where
  walk owner = do
    node <- T.lookupTermNode owner graph
    case T.termNodeForm node of
      T.TypedGlobal _ provider -> Just (provider, [])
      T.TypedVisibleTypeApplication _ child argument witness -> do
        (provider, previous) <- walk child
        pure (provider, previous ++ [(Just argument, T.typeApplicationSelected witness)])
      T.TypedImplicitTypeApplication _ child witness -> do
        (provider, previous) <- walk child
        pure (provider, previous ++ [(Nothing, T.implicitTypeApplicationSelected witness)])
      _ -> Nothing

isImplicit :: T.TermNodeForm ty local -> Bool
isImplicit T.TypedImplicitTypeApplication{} = True
isImplicit _ = False
