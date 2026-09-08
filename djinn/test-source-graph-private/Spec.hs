module Main (main) where

import Control.Monad (forM_)
import qualified Data.Set as Set
import Data.Void (Void)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import qualified KindGraphCases
import qualified DjinnContextSpec
import qualified ContextualInstantiationSpec
import qualified ContextualErasureSpec
import qualified NestedGivenErasureSpec

import Djinn.Internal.Environment (prepareGroundSynthesisEnvironment)
import Djinn.Internal.SourceGraph (SourceGraphError, checkSourceClauseGraph)
import Djinn.Internal.SourceTypingContext (sourceTypingContext)
import Language.Haskell.Synthesis.Declaration
  ( Declaration(..), DataConstructor(..), TypeParameter(..), ValueSignature(..) )
import qualified Language.Haskell.Synthesis.Environment as E
import qualified Language.Haskell.Synthesis.Generated as G
import Language.Haskell.Synthesis.Kind (Kind(..))
import Language.Haskell.Synthesis.Name (Name, Boxity(Boxed), parseName)
import qualified Language.Haskell.Synthesis.Type as T
import Language.Haskell.Synthesis.TypeAtom (alphaEquivalentClosedTypes)
import qualified Language.Haskell.Synthesis.TypedGenerated as Q

-- These are exact source clauses, not generated search candidates. In
-- particular the polymorphic let is intentionally retained: its single RHS
-- must acquire a scheme, and two uses must instantiate that same scheme.
-- No public candidate/graph association constructor is exposed by this suite.
main :: IO ()
main = defaultMain $ testGroup "private Djinn source graph checker"
  [ sharingTests, lexicalTests, constructorTests, specializationTests
  , KindGraphCases.tests, DjinnContextSpec.tests, ContextualInstantiationSpec.tests
  , ContextualErasureSpec.tests, NestedGivenErasureSpec.tests ]

type DeclarationSource = Declaration String Void ()
type SourceType = T.Type String
type Expression = G.Expression String
type Graph = Q.TermGraph (T.Type (T.Variable String)) String

name :: String -> Name
name = either (error . show) id . parseName

variable :: String -> SourceType
variable = T.TypeVariable

nominal :: String -> SourceType
nominal = T.TypeConstructor . name

arrow :: SourceType -> SourceType -> SourceType
arrow = T.FunctionType

forallType :: [String] -> SourceType -> SourceType
forallType binders = T.ForallType binders []

applied :: String -> [SourceType] -> SourceType
applied constructor = foldl T.TypeApplication (nominal constructor)

tupleType :: [SourceType] -> SourceType
tupleType = T.TupleType Boxed

unitType, boolType, identityType :: SourceType
unitType = tupleType []
boolType = nominal "Bool"
identityType = forallType ["i"] $ arrow (variable "i") (variable "i")

datatype :: String -> [String] -> [(String, [SourceType])] -> DeclarationSource
datatype constructor parameters fields = DataTypeDeclaration () (name constructor)
  [TypeParameter parameter Nothing | parameter <- parameters]
  [DataConstructor () (name field) types | (field, types) <- fields]

value :: String -> SourceType -> DeclarationSource
value spelling = ValueDeclaration . ValueSignature () (name spelling)

abstract :: String -> Int -> DeclarationSource
abstract spelling arity = AbstractTypeDeclaration () (name spelling)
  $ foldr FunctionKind ProperTypeKind $ replicate arity ProperTypeKind

boolDeclarations :: [DeclarationSource]
boolDeclarations = [datatype "Bool" [] [("False", []), ("True", [])]]

lambda :: [String] -> Expression -> Expression
lambda binders = G.Lambda $ map G.Bind binders

local :: String -> Expression
local = G.Local

global :: String -> Expression
global = G.Global . name

applications :: Expression -> [Expression] -> Expression
applications = foldl G.Apply

visible :: Expression -> SourceType -> Expression
visible expression source = G.VisibleTypeApplication expression
  $ either (error . show) id $ G.specifiedVisibleTypeArgument source

clause :: Expression -> G.FunctionClause String
clause = G.functionClauseFromExpression target
 where
  target = either (error . show) id $ G.mkDefinitionName $ name "sourceCandidate"

checked
  :: [DeclarationSource] -> SourceType -> Expression
  -> IO (Either SourceGraphError Graph)
checked declarations goal expression = do
  environment <- right $ E.mkEnvironment declarations
  prepared <- right $ prepareGroundSynthesisEnvironment environment
  pure $ checkSourceClauseGraph 17 (sourceTypingContext prepared goal) $ clause expression

right :: Show failure => Either failure result -> IO result
right = either (fail . show) pure

positive :: [DeclarationSource] -> SourceType -> Expression -> IO Graph
positive declarations goal expression = do
  graph <- checked declarations goal expression >>= right
  let exactClause = clause expression
      nodes = Q.termGraphNodes graph
  assertEqual "source checking changed exact syntax, local identities, or sharing"
    exactClause $ Q.eraseTermGraphToFunctionClause (G.clauseName exactClause) graph
  root <- maybe (fail "sealed source graph lost its root") pure
    $ Q.lookupTermNode (Q.termGraphRoot graph) graph
  assertBool "source graph root changed the requested closed source type"
    $ alphaEquivalentClosedTypes goal $ Q.termNodeType root
  assertEqual "source graph reused a node identity"
    (length nodes) $ Set.size $ Set.fromList $ map fst nodes
  assertBool "source graph unexpectedly empty" $ not $ null nodes
  pure graph

negative :: [DeclarationSource] -> SourceType -> Expression -> IO ()
negative declarations goal expression = do
  result <- checked declarations goal expression
  case result of
    Left _ -> pure ()
    Right graph -> assertFailure $ "invalid source clause acquired a graph: "
      ++ show (Q.eraseTermGraph graph)

sharingTests :: TestTree
sharingTests = testGroup "retained sharing and generalization"
  [ testCase "one let identity is polymorphic at Bool and unit" $ do
      let expression = G.Let (G.Bind "identity")
            (lambda ["argument"] $ local "argument")
            (G.Tuple [G.Apply (local "identity") (global "True"),
                      G.Apply (local "identity") (G.Tuple [])])
      graph <- positive boolDeclarations (tupleType [boolType, unitType]) expression
      let forms = map (Q.termNodeForm . snd) $ Q.termGraphNodes graph
          lets = [pattern' | Q.TypedLet pattern' _ _ <- forms]
          uses = [() | Q.TypedLocal _ "identity" <- forms]
      assertEqual "the let RHS was duplicated or removed" 1 $ length lets
      assertEqual "the two uses stopped referring to one retained let binder" 2 $ length uses
      case lets of
        [pattern'] -> case Q.typedPatternType pattern' of
          T.ForallType [_] [] _ -> pure ()
          other -> assertFailure $ "let identity was not generalized: " ++ show other
        _ -> assertFailure "missing unique retained let"
  , testCase "an ordinary alias retains two local uses and one payload" $ do
      let goal = forallType ["a"] $ arrow (variable "a")
            $ tupleType [variable "a", variable "a"]
          expression = lambda ["input"] $ G.Let (G.Bind "shared") (local "input")
            $ G.Tuple [local "shared", local "shared"]
      graph <- positive [] goal expression
      let forms = map (Q.termNodeForm . snd) $ Q.termGraphNodes graph
      assertEqual "sharing let vanished" 1 $ length [() | Q.TypedLet{} <- forms]
      assertEqual "payload use was duplicated" 1 $ length [() | Q.TypedLocal _ "input" <- forms]
      assertEqual "shared binder does not own both uses" 2 $ length [() | Q.TypedLocal _ "shared" <- forms]
  , testCase "an ambient value cannot be generalized into a let identity" $
      negative []
        (forallType ["a"] $ arrow (variable "a") identityType)
        (lambda ["outer"] $ G.Let (G.Bind "captured") (local "outer")
          $ lambda ["inner"] $ local "captured")
  ]

lexicalTests :: TestTree
lexicalTests = testGroup "lexical and skolem scope"
  [ testCase "shadowed source type spelling opens two independent rigid scopes" $ do
      _ <- positive []
        (forallType ["a"] $ arrow (variable "a")
          $ forallType ["a"] $ arrow (variable "a") (variable "a"))
        (lambda ["outer", "inner"] $ local "inner")
      pure ()
  , testCase "outer value cannot escape as an unrelated inner forall result" $
      negative []
        (forallType ["a"] $ arrow (variable "a")
          $ forallType ["a"] $ arrow (variable "a") (variable "a"))
        (lambda ["outer", "inner"] $ local "outer")
  , testCase "an inferred argument cannot capture a later forall skolem" $
      negative []
        (forallType ["a"] $ arrow (arrow (variable "a") (variable "a")) identityType)
        (lambda ["function", "inner"] $ G.Apply (local "function") (local "inner"))
  , testCase "unbound local is rejected" $
      negative boolDeclarations boolType (local "missing")
  , testCase "sibling binders cannot reuse one source identity" $
      negative boolDeclarations
        (tupleType [arrow boolType boolType, arrow unitType unitType])
        (G.Tuple [lambda ["same"] $ local "same", lambda ["same"] $ local "same"])
  , testCase "a let binder is not in scope in its nonrecursive RHS" $
      negative boolDeclarations boolType
        (G.Let (G.Bind "self") (local "self") $ local "self")
  , testCase "a hole never receives complete source authority" $
      negative boolDeclarations boolType (G.Hole "unfinished")
  ]

constructorTests :: TestTree
constructorTests = testGroup "exact declaration ownership"
  [ testCase "exact empty datatype permits empty-case elimination" $ do
      _ <- positive [datatype "Empty" [] []]
        (forallType ["a"] $ arrow (nominal "Empty") (variable "a"))
        (lambda ["impossible"] $ G.Case (local "impossible") [])
      pure ()
  , testCase "nonempty datatype does not permit an empty case" $
      negative boolDeclarations (arrow boolType unitType)
        (lambda ["input"] $ G.Case (local "input") [])
  , testCase "abstract nominal type has no empty-datatype authority" $
      negative [abstract "Opaque" 0] (arrow (nominal "Opaque") unitType)
        (lambda ["input"] $ G.Case (local "input") [])
  , testCase "a value scheme is not a constructor pattern declaration" $
      negative (boolDeclarations ++ [value "pretendConstructor" boolType])
        (arrow boolType unitType)
        (lambda ["input"] $ G.Case (local "input")
          [(G.Constructor (name "pretendConstructor") [], G.Tuple [])])
  , testCase "a same-shaped constructor from another family is rejected" $
      negative boxes (arrow (applied "Box" [boolType]) boolType)
        (lambda ["input"] $ G.Case (local "input")
          [(G.Constructor (name "Wrap") [G.Bind "field"], local "field")])
  , testCase "constructor patterns require the complete declared field arity" $
      negative boxes (arrow (applied "Box" [boolType]) unitType)
        (lambda ["input"] $ G.Case (local "input")
          [(G.Constructor (name "Box") [], G.Tuple [])])
  , testCase "a constructor field is checked at its exact selected type" $
      negative boxes (applied "Box" [boolType])
        (G.Apply (visible (global "Box") boolType) $ G.Tuple [])
  , testCase "unknown global does not borrow the expected source type" $
      negative boolDeclarations boolType (global "absentProvider")
  , testCase "a changed provider session cannot certify its old clause" $ do
      _ <- positive (boolDeclarations ++ [value "sourceValue" boolType]) boolType
        $ global "sourceValue"
      negative (boolDeclarations ++ [value "sourceValue" unitType]) boolType
        $ global "sourceValue"
  ]
 where
  boxes = boolDeclarations ++
    [ datatype "Box" ["a"] [("Box", [variable "a"])]
    , datatype "Wrapped" ["a"] [("Wrap", [variable "a"])]
    ]

specializationTests :: TestTree
specializationTests = testGroup "correlated rank-N specialization and kinds"
  [ testCase "two distinct impredicative images are fixed by the nominal result" $ do
      let firstType = identityType
          secondType = forallType ["a", "b"]
            $ arrow (variable "a") $ arrow (variable "b") (variable "a")
          goal = applied "PairCarrier" [firstType, secondType]
          expression = applications (global "pack")
            [lambda ["identityInput"] $ local "identityInput",
             lambda ["firstInput", "secondInput"] $ local "firstInput"]
      _ <- positive packDeclarations goal expression
      pure ()
  , testCase "one higher-rank local specializes separately at Bool and unit" $ do
      _ <- positive boolDeclarations
        (arrow identityType $ tupleType [boolType, unitType])
        (lambda ["poly"] $ G.Tuple
          [G.Apply (local "poly") (global "True"), G.Apply (local "poly") (G.Tuple [])])
      pure ()
  , testCase "mixed visible and ordinary arguments retain their exact spine" $ do
      let scheme = forallType ["a"] $ arrow (variable "a")
            $ forallType ["b"] $ arrow (variable "b") (variable "a")
          expression = G.Apply
            (visible (G.Apply (visible (global "after") boolType) (global "True")) unitType)
            (G.Tuple [])
      graph <- positive (boolDeclarations ++ [value "after" scheme]) boolType expression
      assertEqual "mixed spine lost visible application nodes" 2
        $ length [() | (_, Q.TermNode _ Q.TypedVisibleTypeApplication{}) <- Q.termGraphNodes graph]
  , testCase "a visible wildcard before a specified polytype retains both source slots" $ do
      let atom = nominal "A"
          token = nominal "Token"
          scheme = forallType ["payload", "hidden"] $ arrow (variable "payload") token
          function = visible
            (G.VisibleTypeApplication (global "mixed") G.inferredVisibleTypeArgument) identityType
          expression = lambda ["input"] $ G.Apply function $ local "input"
      -- A direct source-checker grammar test, not a claim that the public
      -- search emits this wildcard spelling within a particular prefix.
      graph <- positive
        [abstract "A" 0, abstract "Token" 0, value "mixed" scheme]
        (arrow atom token) expression
      let occurrences =
            [ (firstArgument, firstWitness, secondArgument, secondWitness, application, inputType)
            | (_, Q.TermNode _ (Q.TypedApply functionNode inputNode application)) <- Q.termGraphNodes graph
            , Just (Q.TermNode _ (Q.TypedVisibleTypeApplication _ firstNode secondArgument secondWitness)) <-
                [Q.lookupTermNode functionNode graph]
            , Just (Q.TermNode _ (Q.TypedVisibleTypeApplication _ globalNode firstArgument firstWitness)) <-
                [Q.lookupTermNode firstNode graph]
            , Just (Q.TermNode _ (Q.TypedGlobal _ provider)) <- [Q.lookupTermNode globalNode graph]
            , provider == name "mixed"
            , Just (Q.TermNode inputType (Q.TypedLocal _ "input")) <- [Q.lookupTermNode inputNode graph]
            ]
      case occurrences of
        [(firstArgument, firstWitness, secondArgument, secondWitness, application, inputType)] -> do
          assertEqual "the first source slot lost its visible wildcard syntax"
            G.inferredVisibleTypeArgument firstArgument
          assertBool "the first wildcard was not solved from its actual A argument" $
            alphaEquivalentClosedTypes atom $ Q.typeApplicationSelected firstWitness
          assertBool "the second slot lost its exact specified identity polytype" $
            maybe False (alphaEquivalentClosedTypes identityType) $
              G.visibleTypeArgumentPatternType secondArgument
          assertBool "the two source slot selections were exchanged" $
            alphaEquivalentClosedTypes identityType $ Q.typeApplicationSelected secondWitness
          assertBool "the first slot lost correlation with the ordinary argument" $
            alphaEquivalentClosedTypes atom (Q.applicationDomain application)
              && alphaEquivalentClosedTypes atom inputType
        _ -> assertFailure $ "expected exactly one complete mixed source application, found " ++ show (length occurrences)
  , testCase "reordering a correlated visible vector changes argument typing" $
      negative chooseDeclarations boolType
        (applications (visible (visible (global "choose") unitType) boolType)
          [global "True", G.Tuple []])
  , testCase "an overlong visible vector cannot specialize a monomorphic result" $
      negative chooseDeclarations boolType
        (applications (visible (visible (visible (global "choose") boolType) unitType) boolType)
          [global "True", G.Tuple []])
  , testCase "vacuous proper-type selection cannot accept a higher-kinded constructor" $
      negative (boolDeclarations ++ [abstract "Unary" 1,
          value "vacuous" $ forallType ["unused"] boolType]) boolType
        (visible (global "vacuous") $ nominal "Unary")
  , testCase "unused implicit proper-type selection completes to a checked closed type" $ do
      graph <- positive (boolDeclarations ++
        [value "vacuous" $ forallType ["unused"] boolType]) boolType (global "vacuous")
      let selections = [Q.implicitTypeApplicationSelected witness
            | (_, Q.TermNode _ (Q.TypedImplicitTypeApplication _ _ witness)) <- Q.termGraphNodes graph]
      assertEqual "vacuous application did not retain one completed choice" 1 $ length selections
      forM_ selections $ \selected -> assertBool "completion retained an unadmitted free identity" $
        T.freeVariables selected == Set.empty && alphaEquivalentClosedTypes unitType selected
  , testCase "argument inference completes after checking an unconstrained identity" $ do
      graph <- positive (boolDeclarations ++ [value "consumeIdentity" $
        forallType ["a"] $ arrow (arrow (variable "a") $ variable "a") boolType]) boolType $
          G.Apply (global "consumeIdentity") $ lambda ["input"] $ local "input"
      assertBool "inferred argument completion left a free local type" $ all
        (Set.null . T.freeVariables . Q.termNodeType . snd) $ Q.termGraphNodes graph
  , testCase "kind-invalid application is rejected even under a vacuous binder" $
      negative (boolDeclarations ++ [value "vacuous" $ forallType ["unused"] boolType]) boolType
        (visible (global "vacuous") $ T.TypeApplication boolType boolType)
  , testCase "closed source type is never borrowed from a synthetic provider name" $
      forM_ ["missingAxiom", "djinnProviderMissing"] $ \spelling ->
        negative boolDeclarations boolType $ global spelling
  ]
 where
  packDeclarations = [abstract "PairCarrier" 2,
    value "pack" $ forallType ["a", "b"] $ arrow (variable "a")
      $ arrow (variable "b") $ applied "PairCarrier" [variable "a", variable "b"]]
  chooseDeclarations = boolDeclarations ++ [value "choose" $ forallType ["a", "b"]
    $ arrow (variable "a") $ arrow (variable "b") (variable "a")]
