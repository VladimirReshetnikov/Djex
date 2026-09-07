module ContextHaskellSpec (contextHaskellTests) where

import Data.List (isInfixOf)
import Language.Haskell.Synthesis.Constraint (Constraint (..))
import qualified Language.Haskell.Synthesis.Generated as G
import Language.Haskell.Synthesis.Name (Boxity (Boxed), Name, parseName)
import qualified Language.Haskell.Synthesis.Type as T
import qualified Language.Haskell.Synthesis.TypedGenerated as Q
import qualified Language.Haskell.Synthesis.TypedGenerated.Haskell as H
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

type Ty = T.Type (T.Variable String)
type Source = Q.TermGraphSource Ty Int
type Graph = Q.TermGraph Ty Int

contextHaskellTests :: TestTree
contextHaskellTests = testGroup "Haskell lexical context graph rendering"
  [ testCase "annotate a dictionary-independent body with its complete source context" $ do
      let source = qualified [classC unit] $ arrow unit unit
          graph = checked $ Q.TermGraphSource (nid 0)
            [ (nid 0, node source $ intro 0 1 source)
            , (nid 1, node (arrow unit unit) $ Q.TypedLambda [bind 1 0 unit] (nid 2))
            , (nid 2, node unit $ Q.TypedLocal (oid 2) 0)
            ]
      rendered <- rendering graph
      contains " :: (Fixture.C ()) => () -> ()" rendered
      contains "\\(x :: ()) -> x" rendered
      Q.eraseTermGraph graph @?= G.Lambda [G.Bind 0] (G.Local 0)
      Q.typedGraphProjectedNodes (Q.termGraphMetrics graph) @?= 3
  , testCase "retain both qualified operand and discharged result annotations" $ do
      let graph = checked $ forwarding [classC unit] [given 0 0]
      rendered <- rendering graph
      contains "((Fixture.provider :: (Fixture.C ()) => ()) :: ())" rendered
      contains " :: (Fixture.C ()) => ()" rendered
      Q.eraseTermGraph graph @?= G.Global provider
      Q.typedGraphProjectedNodes (Q.termGraphMetrics graph) @?= 1
  , testCase "render a constrained local parameter under its unique lexical given" $ do
      let offered = qualified [classC unit] unit
          body = arrow offered unit
          source = qualified [classC unit] body
          graph = checked $ Q.TermGraphSource (nid 0)
            [ (nid 0, node source $ intro 0 1 source)
            , (nid 1, node body $ Q.TypedLambda [bind 1 0 offered] (nid 2))
            , (nid 2, node unit $ apply 2 3 offered [given 0 0])
            , (nid 3, node offered $ Q.TypedLocal (oid 3) 0)
            ]
      rendered <- rendering graph
      contains "\\(x :: (Fixture.C ()) => ())" rendered
      contains "((x :: (Fixture.C ()) => ()) :: ())" rendered
      Q.eraseTermGraph graph @?= G.Lambda [G.Bind 0] (G.Local 0)
  , testCase "open a constrained forall with one scoped source type identity" $ do
      let source = T.ForallType [bound] [classC $ variable bound] $
            arrow (variable bound) $ variable bound
          body = arrow rigid rigid
          opened = qualified [classC rigid] body
          graph = checked $ Q.TermGraphSource (nid 0)
            [ (nid 0, node source $ Q.TypedForallIntroduction (oid 0) (nid 1) $
                Q.ForallIntroductionWitness source rigid opened)
            , (nid 1, node opened $ intro 1 2 opened)
            , (nid 2, node body $ Q.TypedLambda [bind 2 0 rigid] (nid 3))
            , (nid 3, node rigid $ Q.TypedLocal (oid 3) 0)
            ]
      rendered <- rendering graph
      contains "forall djexSkolem0. (Fixture.C djexSkolem0) => djexSkolem0 -> djexSkolem0" rendered
      contains "x :: djexSkolem0" rendered
      contains " :: (Fixture.C djexSkolem0) => djexSkolem0 -> djexSkolem0" rendered
      Q.eraseTermGraph graph @?= G.Lambda [G.Bind 0] (G.Local 0)
  , testCase "preserve a type application before exact implicit dictionary discharge" $ do
      let source = T.ForallType [bound] [classC $ variable bound] $
            arrow (variable bound) $ variable bound
          result = arrow unit unit
          selected = qualified [classC unit] result
          graph = checked $ Q.TermGraphSource (nid 0)
            [ (nid 0, node selected $ intro 0 1 selected)
            , (nid 1, node result $ apply 1 2 selected [given 0 0])
            , (nid 2, node selected $ Q.TypedImplicitTypeApplication (oid 2) (nid 3) $
                Q.ImplicitTypeApplicationWitness source unit selected)
            , (nid 3, node source $ Q.TypedGlobal (oid 3) provider)
            ]
      rendered <- rendering graph
      contains "forall djexBound0_0. (Fixture.C djexBound0_0) => djexBound0_0 -> djexBound0_0" rendered
      contains "@(())" rendered
      contains " :: () -> ()" rendered
      Q.eraseTermGraph graph @?= G.Global provider
  , testCase "keep an undisclosed forall inside a context's result" $ do
      let poly = T.ForallType [bound] [] $ arrow (variable bound) $ variable bound
          source = qualified [classC unit] poly
          graph = checked $ Q.TermGraphSource (nid 0)
            [ (nid 0, node source $ intro 0 1 source)
            , (nid 1, node poly $ apply 1 2 source [given 0 0])
            , (nid 2, node source $ Q.TypedGlobal (oid 2) provider)
            ]
      rendered <- rendering graph
      contains "(Fixture.C ()) => forall djexBound1_0." rendered
      contains " :: forall djexBound0_0." rendered
      Q.eraseTermGraph graph @?= G.Global provider
  , testCase "carry a unique outer given through a different inner context" $ do
      let source = qualified [classC unit] $ qualified [classD unit] unit
          inner = qualified [classD unit] unit
          offered = qualified [classC unit] unit
          graph = checked $ Q.TermGraphSource (nid 0)
            [ (nid 0, node source $ intro 0 1 source)
            , (nid 1, node inner $ intro 1 2 inner)
            , (nid 2, node unit $ apply 2 3 offered [given 0 0])
            , (nid 3, node offered $ Q.TypedGlobal (oid 3) provider)
            ]
      rendered <- rendering graph
      contains "Fixture.C ()" rendered
      contains "Fixture.D ()" rendered
      Q.eraseTermGraph graph @?= G.Global provider
  , testCase "retain duplicate unused givens without inventing dictionary arguments" $ do
      let source = qualified [classC unit, classC unit] unit
          graph = checked $ Q.TermGraphSource (nid 0)
            [ (nid 0, node source $ intro 0 1 source)
            , (nid 1, node unit $ Q.TypedGlobal (oid 1) provider)
            ]
      rendered <- rendering graph
      rendered @?= "(Fixture.provider :: (Fixture.C (), Fixture.C ()) => ())"
      Q.eraseTermGraph graph @?= G.Global provider
  , testCase "reject selection between repeated dictionary slots" $ do
      let graph = checked $ forwarding [classC unit, classC unit] [given 0 1]
      H.renderHaskellTermGraph options graph @?= Left H.HaskellGraphUnsupportedContextEvidence
  , testCase "distinguish givens with the same class and different exact argument types" $ do
      let graph = checked $ forwarding [classC unit, classC $ arrow unit unit] [given 0 0]
      rendered <- rendering graph
      contains "Fixture.C (() -> ())" rendered
  , testCase "reject an exact outer dictionary selection shadowed by an equal inner given" $ do
      let offered = qualified [classC unit] unit
          source = qualified [classC unit] offered
          graph = checked $ Q.TermGraphSource (nid 0)
            [ (nid 0, node source $ intro 0 1 source)
            , (nid 1, node offered $ intro 1 2 offered)
            , (nid 2, node unit $ apply 2 3 offered [given 0 0])
            , (nid 3, node offered $ Q.TypedGlobal (oid 3) provider)
            ]
      H.renderHaskellTermGraph options graph @?= Left H.HaskellGraphUnsupportedContextEvidence
  , testCase "reject a noncanonical context claimed by an alternate source observer" $ do
      let observer = Q.ContextTypeStructure $ const $ Just ([classC unit], unit)
          witness = required $ Q.contextIntroductionWitness observer unit
          source = Q.TermGraphSource (nid 0)
            [ (nid 0, node unit $ Q.TypedContextIntroduction (oid 0) (nid 1) witness)
            , (nid 1, node unit $ Q.TypedGlobal (oid 1) provider)
            ]
          graph = right $ Q.sealTermGraphWithContext observer structure Q.defaultTermGraphLimits source
      H.renderHaskellTermGraph options graph @?= Left H.HaskellGraphUnsupportedContextEvidence
  ]

options :: G.RenderOptions Int
options = G.defaultRenderOptions $ const "x"

rendering :: Graph -> IO String
rendering graph = case H.renderHaskellTermGraph options graph of
  Left failure -> fail $ "context rendering failed: " ++ show failure
  Right rendered -> pure rendered

contains :: String -> String -> Assertion
contains expected actual = assertBool ("missing " ++ show expected ++ " in " ++ actual) $
  expected `isInfixOf` actual

right :: Show error => Either error value -> value
right = either (error . show) id

required :: Maybe value -> value
required = maybe (error "invalid context renderer fixture") id

structure :: Q.TypeStructure Ty
structure = Q.sharedTypeStructure
  {Q.forallTypeStructure = Just Q.sharedContextualForallTypeStructure}

checked :: Source -> Graph
checked = right . Q.sealTermGraphWithContext Q.sharedContextTypeStructure structure Q.defaultTermGraphLimits

variable :: T.Variable String -> Ty
variable = T.TypeVariable

bound :: T.Variable String
bound = T.FlexibleVariable "a"

rigid :: Ty
rigid = variable $ T.RigidVariable "contextRendererOpening"

unit :: Ty
unit = T.TupleType Boxed []

arrow :: Ty -> Ty -> Ty
arrow = T.FunctionType

qualified :: [Constraint Ty] -> Ty -> Ty
qualified = T.ForallType []

classC, classD :: Ty -> Constraint Ty
classC ty = Constraint (right $ parseName "Fixture.C") [ty]
classD ty = Constraint (right $ parseName "Fixture.D") [ty]

provider :: Name
provider = right $ parseName "Fixture.provider"

nid :: Natural -> Q.TermNodeId
nid = Q.termNodeId

oid :: Natural -> Q.OccurrenceId
oid = Q.occurrenceId

node :: Ty -> Q.TermNodeForm Ty Int -> Q.TermNode Ty Int
node = Q.TermNode

bind :: Natural -> Int -> Ty -> Q.TypedPattern Ty Int
bind occurrence local ty = Q.TypedPattern (oid occurrence) ty $ Q.TypedBind local

given :: Natural -> Natural -> Q.ContextEvidence
given occurrence = Q.givenContextEvidence $ oid occurrence

intro :: Natural -> Natural -> Ty -> Q.TermNodeForm Ty Int
intro occurrence child source = Q.TypedContextIntroduction (oid occurrence) (nid child) $
  required $ Q.contextIntroductionWitness Q.sharedContextTypeStructure source

apply :: Natural -> Natural -> Ty -> [Q.ContextEvidence] -> Q.TermNodeForm Ty Int
apply occurrence child source evidence = Q.TypedContextApplication (oid occurrence) (nid child) $
  required $ Q.contextApplicationWitness Q.sharedContextTypeStructure source evidence

forwarding :: [Constraint Ty] -> [Q.ContextEvidence] -> Source
forwarding givens evidence = Q.TermGraphSource (nid 0)
  [ (nid 0, node source $ intro 0 1 source)
  , (nid 1, node unit $ apply 1 2 offered evidence)
  , (nid 2, node offered $ Q.TypedGlobal (oid 2) provider)
  ]
 where
  source = qualified givens unit
  offered = qualified [classC unit] unit
