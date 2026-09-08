-- | Direct private source-checker acceptance for lexical Givens. These clauses are
-- supplied to the private checker; they are not synthesis acceptance receipts.
-- Constrained global admission and dictionary search remain separate work.
module DjinnContextSpec (tests) where

import Control.Monad (forM_)
import Data.List (isInfixOf)
import qualified Data.Set as Set
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Djinn.Internal.Environment (prepareGroundSynthesisEnvironment)
import Djinn.Internal.SourceGraph (SourceGraphError, checkSourceClauseGraph)
import Djinn.Internal.SourceTypingContext (sourceTypingContext)
import Language.Haskell.Synthesis.Constraint (Constraint(..))
import Language.Haskell.Synthesis.Declaration (Declaration(..), TypeParameter(..), ValueSignature(..))
import qualified Language.Haskell.Synthesis.Environment as E
import qualified Language.Haskell.Synthesis.Generated as G
import Language.Haskell.Synthesis.Kind (Kind(..))
import Language.Haskell.Synthesis.Name (Name, Boxity(Boxed), parseName)
import qualified Language.Haskell.Synthesis.Type as T
import qualified Language.Haskell.Synthesis.TypeAtom as A
import qualified Language.Haskell.Synthesis.TypedGenerated as Q

type SourceType = T.Type String
type Expression = G.Expression String
type DeclarationSource = Declaration String Void ()
type Graph = Q.TermGraph (T.Type (T.Variable String)) String

tests :: TestTree
tests = testGroup "Djinn lexical Given source checker (not synthesis)"
  [ testCase "retain an unused root context and its exact source slot" $ do
      graph <- positive declarations
        (forallWith ["a"] [given "C" a] $ arrow a a) $ lambda ["x"] $ local "x"
      assertEqual "root context introductions" [0] $ map Q.evidenceBinderSlot $ introduced graph
      assertEqual "dictionary-independent identity acquired an application" [] $ applied graph

  , testCase "a nested callback owns its unused Given" $ do
      let callback = forallWith ["a"] [given "C" a] $ arrow a a
      graph <- positive declarations (arrow (arrow callback token) token) $
        lambda ["consume"] $ G.Apply (local "consume") $ lambda ["x"] $ local "x"
      assertEqual "callback context count" 1 $ length $ introduced graph
      assertEqual "callback acquired a dictionary application" [] $ applied graph
      root <- node graph $ Q.termGraphRoot graph
      case Q.termNodeForm root of
        Q.TypedLambda{} -> pure ()
        _ -> assertFailure "nested context moved outside the consuming term lambda"

  , testCase "exact contextual forwarding keeps the original local scheme opaque" $ do
      let renamed = forallWith ["b"] [given "C" b] $ arrow b token
      graph <- positive declarations (arrow provider renamed) $ lambda ["p"] $ local "p"
      assertEqual "opaque forwarding opened dictionaries" [] $ introduced graph
      assertEqual "opaque forwarding applied dictionaries" [] $ applied graph
      assertBool "the local provider lost its full qualified forall" $
        any (\(_, Q.TermNode ty form) -> case form of
          Q.TypedLocal{} -> A.alphaEquivalentTypes (fmap T.FlexibleVariable provider) ty
          _ -> False) $ Q.termGraphNodes graph

  , testCase "a later term argument selects the exact root Given" $ do
      graph <- positive declarations
        (forallWith ["a"] [given "C" a] $ arrow provider $ arrow a token) $
        lambda ["p", "x"] $ G.Apply (local "p") $ local "x"
      assertEqual "application changed source Given ownership"
        (introduced graph) (applied graph)
      assertEqual "expected one dictionary application" 1 $ length $ applied graph

  , testCase "a bare qualified local uses expected result typing before discharge" $ do
      let supplied = forallWith ["b"] [given "C" b] b
      graph <- positive declarations
        (forallWith ["a"] [given "C" a] $ arrow supplied a) $
        lambda ["p"] $ local "p"
      assertEqual "result-selected application used a different Given"
        (introduced graph) (applied graph)

  , testCase "a bare global method infers its constraint-only type parameter" $ do
      let method = forallWith ["b"] [given "C" b] token
      graph <- positive (declarations ++ [ValueDeclaration $ ValueSignature () (name "method") method])
        (forallWith ["a"] [given "C" a] token) $ G.Global $ name "method"
      assertEqual "global method lost its exact Given" (introduced graph) (applied graph)
      assertEqual "global method did not retain its inferred type application" 1 $
        length [() | (_, Q.TermNode _ Q.TypedImplicitTypeApplication{}) <- Q.termGraphNodes graph]

  , testCase "a bare local method infers its constraint-only type parameter" $ do
      let method = forallWith ["b"] [given "C" b] token
      graph <- positive declarations
        (forallWith ["a"] [given "C" a] $ arrow method token) $ lambda ["p"] $ local "p"
      assertEqual "local method lost its exact Given" (introduced graph) (applied graph)

  , testCase "constraint-only inference solves the complete context coherently" $ do
      let method = forallWith ["x"] [given "C" $ var "x", given "D" $ var "x"] token
      graph <- positive (declarations ++ [classDeclaration "D" []])
        (forallWith ["a", "b"] [given "C" a, given "C" b, given "D" b] $
          arrow method token) $ lambda ["p"] $ local "p"
      assertEqual "constraints did not jointly select b" [1, 2] $
        map Q.evidenceBinderSlot $ applied graph

  , testCase "constraint-only inference matches a function-shaped class argument" $ do
      let method = forallWith ["b"] [given "C" $ arrow b token] token
      graph <- positive declarations
        (forallWith ["a"] [given "C" $ arrow a token] $ arrow method token) $
        lambda ["p"] $ local "p"
      assertEqual "function-shaped constraint lost its Given" (introduced graph) (applied graph)

  , testCase "a lexical Given can determine a whole impredicative selection" $ do
      let identity = forallWith ["x"] [] $ arrow (var "x") $ var "x"
          method = forallWith ["b"] [given "C" b] token
      graph <- positive declarations
        (forallWith [] [given "C" identity] $ arrow method token) $ lambda ["p"] $ local "p"
      assertEqual "impredicative method lost its Given" (introduced graph) (applied graph)

  , testCase "different lexical type instantiations remain explicitly ambiguous" $ do
      let method = forallWith ["x"] [given "C" $ var "x"] token
      negative "ambiguous lexical Given type instantiation" declarations
        (forallWith ["a", "b"] [given "C" a, given "C" b] $ arrow method token) $
        lambda ["p"] $ local "p"

  , testCase "constraint matching cannot capture a Given's quantified variable" $ do
      let method = forallWith ["x"]
            [given "C" $ forallWith ["b"] [] $ arrow b $ var "x"] token
          identity = forallWith ["a"] [] $ arrow a a
      negative "no exact lexical given" declarations
        (forallWith [] [given "C" identity] $ arrow method token) $
        lambda ["p"] $ local "p"

  , testCase "constraint-only inference cannot combine inconsistent selections" $ do
      let method = forallWith ["x"] [given "C" $ var "x", given "D" $ var "x"] token
      negative "no exact lexical given" (declarations ++ [classDeclaration "D" []])
        (forallWith ["a", "b"] [given "C" a, given "D" b] $ arrow method token) $
        lambda ["p"] $ local "p"

  , testCase "provider constraints retain their own order while selecting root slots" $ do
      let binary = forallWith ["x", "y"] [given "C" $ var "y", given "C" $ var "x"] $
            arrow (var "x") $ arrow (var "y") token
      graph <- positive declarations
        (forallWith ["a", "b"] [given "C" a, given "C" b] $
          arrow binary $ arrow a $ arrow b token) $
        lambda ["p", "x", "y"] $ apply (local "p") [local "x", local "y"]
      assertEqual "provider context reordered root evidence" [1, 0] $
        map Q.evidenceBinderSlot $ applied graph
      assertBool "application borrowed another context introduction" $
        all (`elem` introduced graph) $ applied graph

  , testCase "a context in the first tuple component cannot supply its sibling" $ do
      let first = forallWith [] [given "C" a] $ arrow a token
          body = lambda ["x"] $ G.Apply (local "p") $ local "x"
      _ <- positive declarations
        (forallWith ["a"] [] $ arrow provider first) $ lambda ["p"] body
      negative "no exact lexical given" declarations
        (forallWith ["a"] [] $ arrow provider $ T.TupleType Boxed [first, arrow a token]) $
        lambda ["p"] $ G.Tuple [body, body]

  , testCase "a Given cannot change its nominal type argument" $
      negative "no exact lexical given" declarations
        (forallWith ["a", "b"] [given "C" a] $ arrow provider $ arrow b token) $
        lambda ["p", "x"] $ G.Apply (local "p") $ local "x"

  , testCase "a Given for another class cannot discharge C" $
      negative "no exact lexical given" (declarations ++ [classDeclaration "D" []])
        (forallWith ["a"] [given "D" a] $ arrow provider $ arrow a token) $
        lambda ["p", "x"] $ G.Apply (local "p") $ local "x"

  , testCase "superclass declarations are rejected before lexical Given checking" $ do
      environment <- right $ E.mkEnvironment $
        declarations ++ [classDeclaration "D" [given "C" a]]
      case prepareGroundSynthesisEnvironment environment of
        Left failure -> assertBool ("unexpected preparation failure: " ++ show failure) $
          "ClassSuperclassesUnsupported" `isInfixOf` show failure
        Right _ -> assertFailure "a superclass declaration acquired source-checking authority"

  , testCase "class parameter kinds constrain even unused forall binders" $ do
      let unary = FunctionKind ProperTypeKind ProperTypeKind
          inventory = declarations ++
            [ClassDeclaration () (name "HK") [TypeParameter "f" $ Just unary] [] []]
          unit = T.TupleType Boxed []
      graph <- positive inventory
        (forallWith ["f"] [given "HK" $ var "f"] $ arrow unit unit) $
        lambda ["x"] $ local "x"
      assertEqual "higher-kind context count" 1 $ length $ introduced graph

  , testCase "an undeclared class is not admitted by a dictionary-independent body" $
      negative "Unknown" declarations
        (forallWith ["a"] [given "Unknown" a] $ arrow a a) $
        lambda ["x"] $ local "x"
  ]

name :: String -> Name
name = either (error . show) id . parseName

var :: String -> SourceType
var = T.TypeVariable

a, b, token, provider :: SourceType
a = var "a"
b = var "b"
token = T.TypeConstructor $ name "Token"
provider = forallWith ["b"] [given "C" b] $ arrow b token

arrow :: SourceType -> SourceType -> SourceType
arrow = T.FunctionType

forallWith :: [String] -> [Constraint SourceType] -> SourceType -> SourceType
forallWith = T.ForallType

given :: String -> SourceType -> Constraint SourceType
given className argument = Constraint (name className) [argument]

classDeclaration :: String -> [Constraint SourceType] -> DeclarationSource
classDeclaration spelling superclasses =
  ClassDeclaration () (name spelling) [TypeParameter "a" Nothing] superclasses []

declarations :: [DeclarationSource]
declarations =
  [ classDeclaration "C" []
  , AbstractTypeDeclaration () (name "Token") ProperTypeKind
  ]

lambda :: [String] -> Expression -> Expression
lambda = G.Lambda . map G.Bind

local :: String -> Expression
local = G.Local

apply :: Expression -> [Expression] -> Expression
apply = foldl G.Apply

clause :: Expression -> G.FunctionClause String
clause = G.functionClauseFromExpression $
  either (error . show) id $ G.mkDefinitionName $ name "givenCandidate"

checked :: [DeclarationSource] -> SourceType -> Expression -> IO (Either SourceGraphError Graph)
checked inventory goal expression = do
  environment <- right $ E.mkEnvironment inventory
  prepared <- right $ prepareGroundSynthesisEnvironment environment
  pure $ checkSourceClauseGraph 41 (sourceTypingContext prepared goal) $ clause expression

positive :: [DeclarationSource] -> SourceType -> Expression -> IO Graph
positive inventory goal expression = do
  graph <- checked inventory goal expression >>= right
  let original = clause expression
      nodes = Q.termGraphNodes graph
  assertEqual "context evidence changed compatibility syntax" original $
    Q.eraseTermGraphToFunctionClause (G.clauseName original) graph
  root <- node graph $ Q.termGraphRoot graph
  assertBool "the full original source signature was lost" $
    A.alphaEquivalentTypes (fmap T.FlexibleVariable goal) $ Q.termNodeType root
  assertEqual "node occurrence identity was reused" (length nodes) $
    Set.size $ Set.fromList $ map fst nodes
  forM_ (applied graph) $ \evidence -> assertBool "application has no lexical introduction" $
    evidence `elem` introduced graph
  pure graph

negative :: String -> [DeclarationSource] -> SourceType -> Expression -> IO ()
negative message inventory goal expression = do
  result <- checked inventory goal expression
  case result of
    Left failure -> assertBool ("unexpected rejection: " ++ show failure) $
      message `isInfixOf` show failure
    Right graph -> assertFailure $ "invalid contextual clause acquired a graph: " ++ show graph

introduced :: Graph -> [Q.EvidenceBinderId]
introduced graph =
  [ Q.contextEvidenceBinder $ Q.givenContextEvidence occurrence slot
  | (_, Q.TermNode _ (Q.TypedContextIntroduction occurrence _ witness)) <- Q.termGraphNodes graph
  , (slot, _) <- zip [0 ..] $ Q.contextIntroductionConstraints witness
  ]

applied :: Graph -> [Q.EvidenceBinderId]
applied graph =
  [ Q.contextEvidenceBinder evidence
  | (_, Q.TermNode _ (Q.TypedContextApplication _ _ witness)) <- Q.termGraphNodes graph
  , evidence <- Q.contextApplicationEvidence witness
  ]

node :: Graph -> Q.TermNodeId -> IO (Q.TermNode (T.Type (T.Variable String)) String)
node graph owner = maybe (fail "missing source graph node") pure $ Q.lookupTermNode owner graph

right :: Show failure => Either failure value -> IO value
right = either (fail . show) pure
