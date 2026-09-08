-- | Production-search budget controls for the conditional root-Given pilot.
-- These tests observe the public batch and streaming entrances. They do not
-- reconstruct formula plans or infer an unexposed choice counter from output
-- size. Every candidate observed at a boundary must retain its exact source
-- graph, including the original lexical dictionary introduction and uses.
module ContextBudgetSpec (tests) where

import Control.Monad (forM, forM_)
import Data.List (tails)
import qualified Data.Map.Strict as Map
import Language.Haskell.Djex
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

data Fixture
  = ForcedLocal | ForcedGlobal | ReusableLocal | WrongLocal | WrongGlobal
  deriving (Eq, Show)

data Entrance = Batch | Stream
  deriving (Eq, Show)

data Observation = Observation
  { observedCap :: Int
  , observedBudget :: Integer
  , observedCompletion :: Completion
  , observedTerms :: [Expression String]
  , observedApplications :: Int
  }
  deriving (Show)

tests :: TestTree
tests = testGroup "Conditional Given production budgets"
  ([ testCase (show fixture ++ " " ++ show entrance ++ " bounded matrix") $ do
       observations <- matrix fixture entrance
       forM_ [1, 2, 8] $ \cap -> do
         let positive = row observations cap 40
         assertBool ("the finite positive control found no candidate: " ++ show positive) $
           not $ null $ observedTerms positive
         if fixture == ReusableLocal then pure () else
           assertBool "a forced application lost its explicit Given evidence" $
             observedApplications positive > 0
       -- Only streaming promises discovery order; batch may rank its complete
       -- pool. Larger raw windows must retain the earlier streaming prefix.
       if entrance == Stream then
         forM_ [0, 1, 2, 4, 40] $ \fuel ->
           forM_ [(1, 2), (2, 8)] $ \(small, large) -> do
             let earlier = observedTerms $ row observations small fuel
                 later = observedTerms $ row observations large fuel
             assertBool ("a larger window lost the observed prefix: "
                 ++ show (fixture, fuel, small, large, earlier, later)) $
               length earlier <= length later && and
                 (zipWith alphaEquivalentExpression earlier later)
         else pure ()
       if fixture == ReusableLocal then do
         assertBool ("the cutoff control never reached its conditional provider: " ++ show observations) $
           any ((> 0) . observedApplications) observations
         else pure ()
   | fixture <- [ForcedLocal, ForcedGlobal, ReusableLocal]
   , entrance <- [Batch, Stream]
   ] ++
   [ testCase (show fixture ++ " never borrows another type's Given") $ do
       (session, source) <- prepare fixture
       forM_ [Batch, Stream] $ \entrance -> forM_ [0, 4, 40] $ \fuel -> do
         observation <- observe fixture entrance session source 8 fuel
         observedTerms observation @?= []
   | fixture <- [WrongLocal, WrongGlobal]
   ] ++ [duplicateCutoffTests])

-- The conditional cursor retains its LJT continuation and normal-term
-- enumeration, which can rediscover an existing term. At this observed
-- boundary, three raw proofs yield only two distinct accepted candidates.
-- The discarded duplicate must consume its original slot: neither entrance
-- may refill the window or change its completion to choice exhaustion.
duplicateCutoffTests :: TestTree
duplicateCutoffTests = testGroup "Conditional Given duplicate raw cutoff"
  [ testCase (show entrance) $ do
      (session, source) <- prepare ReusableLocal
      observation <- observe ReusableLocal entrance session source 3 64
      observedCap observation @?= 3
      observedBudget observation @?= 64
      observedCompletion observation @?= truncated CandidateLimitReached
      length (observedTerms observation) @?= 2
      -- inspectCandidate has checked each candidate's source root, exact
      -- graph/erasure association and every actual Given/provider use. One
      -- output retains the unused Given; the other uses that exact dictionary
      -- once. No generated variable or rendering spelling is prescribed.
      observedApplications observation @?= 1
  | entrance <- [Batch, Stream]
  ]

matrix :: Fixture -> Entrance -> IO [Observation]
matrix fixture entrance = do
  (session, source) <- prepare fixture
  forM [(cap, fuel) | cap <- [1, 2, 8], fuel <- [0, 1, 2, 4, 40]] $
    \(cap, fuel) -> observe fixture entrance session source cap fuel

row :: [Observation] -> Int -> Integer -> Observation
row observations cap fuel = case
    filter (\observation -> observedCap observation == cap
      && observedBudget observation == fuel) observations of
  [observation] -> observation
  _ -> error "test matrix omitted or repeated a requested boundary"

signature :: Fixture -> String
signature ForcedLocal =
  "forall a. C a => (forall b. C b => b -> Token) -> a -> Token"
signature ForcedGlobal = "forall a. C a => a -> Token"
signature ReusableLocal =
  "forall a. C a => (forall b. C b => b -> b) -> a -> a"
signature WrongLocal =
  "forall a b. C a => (forall c. C c => c -> Token) -> b -> Token"
signature WrongGlobal = "forall a b. C a => b -> Token"

isGlobal :: Fixture -> Bool
isGlobal fixture = fixture `elem` [ForcedGlobal, WrongGlobal]

providerScheme :: Fixture -> IO (Type String)
providerScheme fixture = do
  className <- expectRight $ parseName "C"
  tokenName <- expectRight $ parseName "Token"
  let variable = TypeVariable "source"
      result = if fixture == ReusableLocal then variable else TypeConstructor tokenName
  pure $ ForallType ["source"] [Constraint className [variable]] $
    FunctionType variable result

prepare :: Fixture -> IO (DjinnSession, DjinnRequest)
prepare fixture = do
  className <- expectRight $ parseName "C"
  tokenName <- expectRight $ parseName "Token"
  providerName <- expectRight $ mkIdentifier "token"
  scheme <- providerScheme fixture
  let declarations =
        [ ClassDeclaration () className [TypeParameter "a" Nothing] [] []
        , AbstractTypeDeclaration () tokenName ProperTypeKind
        ] ++ [ValueDeclaration $ ValueSignature () providerName scheme | isGlobal fixture]
  environment <- expectRight (mkEnvironment declarations ::
    Either (EnvironmentError DjinnTypeVariable) DjinnEnvironment)
  session <- expectRight $ mkDjinnSession environment
  -- Search receives the declared empty class, abstract Token and, in global
  -- rows only, token's generic scheme. No implementation or instance is added.
  environmentDeclarations (djinnSessionEnvironment session) @?= declarations
  target <- expectRight $ mkIdentifier "contextBudget"
  source <- expectRight $ parseDjinnRequest session defaultQueryOptions
    target "conditional-given-budget" $ signature fixture
  pure (session, source)

observe
  :: Fixture -> Entrance -> DjinnSession -> DjinnRequest -> Int -> Integer
  -> IO Observation
observe fixture entrance session source cap fuel = do
  let original = djinnRequestQuery source
      options = defaultQueryOptions
        { optionCutoff = cap, optionBudget = Just fuel
        , optionAlternatives = True, optionSorted = False
        , optionStrategy = Interleave, optionRanking = LegacyCandidateRanking
        }
      query = original {requestOptions = options}
  request <- expectRight $ mkDjinnRequest query
  djinnRequestQuery request @?= query
  requestGoal query @?= requestGoal original
  requestContexts query @?= requestContexts original
  requestTarget query @?= requestTarget original
  results <- case entrance of
    Batch -> (: []) <$> expectRight (runDjinnTypedQuery session request)
    Stream -> do
      stream <- expectRight $ runDjinnTypedQueryStream session request
      -- One possible terminal result plus one overflow observation suffices
      -- to detect resetting the requested cap for each resumed batch. Do not
      -- consume an unbounded tail if this contract regresses.
      traverse expectRight $ take (cap + 2) stream
  assertBool "the production entrance returned no terminal observation" $ not $ null results
  let candidates = concatMap (batchCandidates . resultSearch) results
  checked <- traverse (inspectCandidate fixture query) candidates
  let terms = map fst checked
      applications = sum $ map snd checked
      describe = show (fixture, entrance, cap, fuel, map
        (\result -> (resultEvidence result, batchProgress $ resultSearch result)) results, terms)
  assertBool ("the original candidate window was refilled: " ++ describe) $
    length candidates <= cap
  forM_ results $ \result -> do
    let empty = null $ batchCandidates $ resultSearch result
    -- An approximation with no checked witness cannot claim a refutation;
    -- the forced-positive and mismatched-Given controls exercise both sides.
    resultEvidence result @?= if empty then NoEvidence else ValidatedCandidates
  case entrance of
    Batch -> length results @?= 1
    Stream -> do
      assertBool ("stream reset its per-request result cap: " ++ describe) $
        length results <= cap + 1
      forM_ (init results) $ \result -> do
        batchProgress (resultSearch result) @?= Continuing
        length (batchCandidates $ resultSearch result) @?= 1
      batchCandidates (resultSearch $ last results) @?= []
  completion <- case batchProgress $ resultSearch $ last results of
    Completed value -> pure value
    Continuing -> assertFailure ("bounded search never terminated: " ++ describe)
      >> error "unreachable"
  if fuel == 0 then do
    terms @?= []
    completion @?= truncated ChoicePointLimitReached
    forM_ results $ \result ->
      djinnFirstExploredProof (batchMetadata $ resultSearch result) @?= Nothing
    else pure ()
  -- A forced contextual call needs its source term introductions, the exact
  -- dictionary premise and the provider application. Tiny allowances cannot
  -- be replenished when the historical cursor hands over to that plan.
  if fixture `elem` [ForcedLocal, ForcedGlobal] && fuel <= 2 then do
    terms @?= []
    completion @?= truncated ChoicePointLimitReached
    else pure ()
  forM_ [reason | Truncated reasons <- [completion], reason <- foldr (:) [] reasons] $
    \reason -> assertBool ("unrelated budget was reported: " ++ describe) $
      reason `elem` [ChoicePointLimitReached, CandidateLimitReached]
  forM_ [(left, right) | left : rest <- tails terms, right <- rest] $ \(left, right) ->
    assertBool "the public result stream repeated an alpha-equivalent candidate" $
      not $ alphaEquivalentExpression left right
  pure $ Observation cap fuel completion terms applications

inspectCandidate
  :: Fixture -> QueryRequest DjinnType QueryOptions -> DjinnTypedCandidate
  -> IO (Expression String, Int)
inspectCandidate fixture query candidate = do
  graph <- expectRight $ typedCandidateTermGraph candidate
  let compatibility = typedCandidateCompatibility candidate
      clause = candidateOutput compatibility
      expression = eraseTermGraph graph
      expected = fmap FlexibleVariable $ requestContextualType query
  clauseName clause @?= requestTarget query
  candidateResidualConstraints compatibility @?= []
  eraseTermGraphToFunctionClause (clauseName clause) graph @?= clause
  root <- lookupNode graph $ termGraphRoot graph
  assertBool "graph root lost the complete original contextual type" $
    alphaEquivalentClosedTypes expected $ termNodeType root
  (binder, given) <- rootGiven graph $ termGraphRoot graph
  evidenceBinderSlot binder @?= 0
  className <- expectRight $ parseName "C"
  case given of
    Constraint actualClass [TypeVariable RigidVariable{}] -> actualClass @?= className
    _ -> assertFailure "root Given lost its class or scoped rigid argument"
  provider <- fmap (fmap FlexibleVariable) $ providerScheme fixture
  providerName <- expectRight $ mkIdentifier "token"
  let expectedGlobals = [providerName | isGlobal fixture]
  forM_ [name | (_, TermNode _ (TypedGlobal _ name)) <- termGraphNodes graph] $ \name ->
    assertBool "a helper, constructor or undeclared value escaped proof lowering" $
      name `elem` expectedGlobals
  applications <- visit graph provider providerName (Map.singleton binder given) $
    termGraphRoot graph
  if fixture `elem` [ForcedLocal, ForcedGlobal] then
    assertBool "a forced provider application has no exact Given use" $ applications > 0
    else pure ()
  pure (expression, applications)
 where
  rootGiven graph owner = do
    TermNode _ form <- lookupNode graph owner
    case form of
      TypedForallIntroduction _ child _ -> rootGiven graph child
      TypedContextIntroduction occurrence _ witness -> case contextIntroductionConstraints witness of
        [constraint] -> pure (contextEvidenceBinder $ givenContextEvidence occurrence 0, constraint)
        _ -> fail "root Given telescope changed its original arity"
      _ -> fail "the source root Given was erased or moved below a term binder"

  visit graph provider providerName rootBindings owner = do
    TermNode _ form <- lookupNode graph owner
    case form of
      TypedContextIntroduction occurrence child witness -> do
        let declared = Map.fromList
              [(contextEvidenceBinder $ givenContextEvidence occurrence slot, constraint)
              | (slot, constraint) <- zip [0 ..] $ contextIntroductionConstraints witness]
        declared @?= rootBindings
        visit graph provider providerName rootBindings child
      TypedContextApplication _ child witness -> do
        let constraints = contextApplicationConstraints witness
            evidence = contextApplicationEvidence witness
        length constraints @?= 1
        length evidence @?= 1
        forM_ (zip constraints evidence) $ \(required, proof) ->
          Map.lookup (contextEvidenceBinder proof) rootBindings @?= Just required
        providerBase graph provider providerName child
        rest <- visit graph provider providerName rootBindings child
        pure $ length evidence + rest
      TypedHole{} -> fail "a budget-limited candidate acquired an implementation hole"
      _ -> sum <$> traverse (visit graph provider providerName rootBindings) (children form)

  providerBase graph provider providerName owner = do
    TermNode source form <- lookupNode graph owner
    let retained = assertBool "provider lost its complete original quantified scheme" $
          alphaEquivalentClosedTypes provider source
    case form of
      TypedLocal{} -> do
        assertBool "a global fixture silently switched to a local provider" $ not $ isGlobal fixture
        retained
      TypedGlobal _ actual -> do
        assertBool "a local fixture silently acquired a global provider" $ isGlobal fixture
        actual @?= providerName
        retained
      TypedVisibleTypeApplication _ child _ _ -> providerBase graph provider providerName child
      TypedImplicitTypeApplication _ child _ -> providerBase graph provider providerName child
      TypedContextApplication _ child _ -> providerBase graph provider providerName child
      _ -> fail "a Given application lost its original local/global provider"

lookupNode :: TermGraph ty local -> TermNodeId -> IO (TermNode ty local)
lookupNode graph owner = case lookupTermNode owner graph of
  Just node -> pure node
  Nothing -> fail "checked candidate graph referenced a missing node"

children :: TermNodeForm ty local -> [TermNodeId]
children form = case form of
  TypedLocal{} -> []
  TypedGlobal{} -> []
  TypedLambda _ body -> [body]
  TypedApply function argument _ -> [function, argument]
  TypedVisibleTypeApplication _ child _ _ -> [child]
  TypedForallIntroduction _ child _ -> [child]
  TypedImplicitTypeApplication _ child _ -> [child]
  TypedContextIntroduction _ child _ -> [child]
  TypedContextApplication _ child _ -> [child]
  TypedTuple values -> values
  TypedHole{} -> []
  TypedLet _ binding body -> [binding, body]
  TypedCase scrutinee alternatives -> scrutinee : map snd alternatives

expectRight :: Show failure => Either failure value -> IO value
expectRight = either (fail . show) pure
