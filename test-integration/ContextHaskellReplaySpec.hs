-- | Independent GHC execution of rendered checked lexical-Given graphs.
-- No synthesis engine, oracle provider inventory, or handcrafted rendered RHS
-- is involved. Each positive fixture compiles the renderer's actual output
-- under the original complete signature and observes class-method payloads.
module ContextHaskellReplaySpec (tests) where

import Control.Exception (bracket, try)
import Control.Monad (forM_)
import Language.Haskell.Synthesis.Constraint (Constraint (..))
import qualified Language.Haskell.Synthesis.Generated as G
import Language.Haskell.Synthesis.Name (Boxity (Boxed), Name, parseName)
import qualified Language.Haskell.Synthesis.Type as T
import qualified Language.Haskell.Synthesis.TypedGenerated as Q
import qualified Language.Haskell.Synthesis.TypedGenerated.Haskell as H
import Numeric.Natural (Natural)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hClose, hPutStr, openTempFile)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase, (@?=))

type Ty = T.Type (T.Variable String)
type Source = Q.TermGraphSource Ty Int
type Graph = Q.TermGraph Ty Int

tests :: TestTree
tests = testGroup "GHC replay of rendered lexical contexts"
  [ testCase "a global provider consumes the exact polymorphic given" $
      compileAndExecute globalForwarding
        "forall a. Main.C a => a -> Int"
        "(candidate @Int 7, candidate @Bool True, candidate @Bool False)"
        "(218,907,509)"
  , testCase "a constrained local callback retains its caller's type and dictionary" $
      compileAndExecute localCallback
        "forall a. Main.C a => (Main.C a => a -> Int) -> a -> Int"
        "(candidate @Int (\\x -> Main.payload x + 37) 7, candidate @Bool (\\x -> 2 * Main.payload x) True, candidate @Bool (\\x -> 2 * Main.payload x) False)"
        "(255,1814,1018)"
  , testCase "nested rank-N contexts distinguish outer and inner class arguments" $
      compileAndExecute nestedForalls
        "forall a. Main.C a => a -> (forall b. Main.C b => b -> (Int, Int))"
        "(candidate @Int 7 @Bool True, candidate @Bool False @Int (-5))"
        "((218,907),(509,206))"
  , testCase "repeated and nested equal givens reject before any GHC invocation" $
      forM_ [repeatedGiven, shadowedGiven] $ \source -> do
        graph <- checked source
        H.renderHaskellTermGraph options graph @?= Left H.HaskellGraphUnsupportedContextEvidence
        Q.eraseTermGraph graph @?= G.Global providerName
  ]

compileAndExecute :: Source -> String -> String -> String -> Assertion
compileAndExecute source signature observation expected = do
  graph <- checked source
  rendered <- expectRight $ H.renderHaskellTermGraph options graph
  let fixture = unlines $ runtimePreamble ++
        [ "candidate :: " ++ signature
        , "candidate = " ++ rendered
        , "main :: IO ()"
        , "main = print (" ++ observation ++ ")"
        ]
  withTemporaryModule fixture $ \path -> do
    -- runghc independently elaborates/compiles the complete source module and
    -- then executes main. A type-only success or an unevaluated RHS is not a
    -- passing receipt. Match the exact method-dependent observation payload.
    completed <- timeout 60000000 $ readProcessWithExitCode "runghc" [path] ""
    case completed of
      Just (ExitSuccess, output, errors) -> assertEqual
        ("rendered contextual candidate returned the wrong dictionary payload\n" ++ errors ++ fixture)
        expected (filter (`notElem` "\r\n") output)
      other -> fail $ "independent contextual GHC compilation/execution failed: "
        ++ show other ++ "\n" ++ fixture

runtimePreamble :: [String]
runtimePreamble =
  [ "{-# LANGUAGE RankNTypes, ImpredicativeTypes, ScopedTypeVariables, TypeApplications, FlexibleContexts #-}"
  , "module Main where"
  , "class C a where payload :: a -> Int"
  , "instance C Int where payload x = x + 211"
  , "instance C Bool where payload flag = if flag then 907 else 509"
  , "provider :: forall a. C a => a -> Int"
  , "provider = payload"
  ]

globalForwarding :: Source
globalForwarding = Q.TermGraphSource (nid 0)
  [ (nid 0, node providerType $ forallIntro 0 1 providerType rigidA opened)
  , (nid 1, node opened $ intro 1 2 opened)
  , (nid 2, node body $ contextApply 2 3 opened [given 1 0])
  , (nid 3, node opened $ typeApply 3 4 providerType rigidA opened)
  , (nid 4, node providerType $ Q.TypedGlobal (oid 4) providerName)
  ]
 where
  body = arrow rigidA int
  opened = context [classC rigidA] body

localCallback :: Source
localCallback = Q.TermGraphSource (nid 0)
  [ (nid 0, node source $ forallIntro 0 1 source rigidA opened)
  , (nid 1, node opened $ intro 1 2 opened)
  , (nid 2, node body $ Q.TypedLambda [bind 2 0 offered] (nid 3))
  , (nid 3, node result $ contextApply 3 4 offered [given 1 0])
  , (nid 4, node offered $ Q.TypedLocal (oid 4) 0)
  ]
 where
  a = variable boundA
  source = T.ForallType [boundA] [classC a] $
    arrow (context [classC a] $ arrow a int) $ arrow a int
  result = arrow rigidA int
  offered = context [classC rigidA] result
  body = arrow offered result
  opened = context [classC rigidA] body

nestedForalls :: Source
nestedForalls = Q.TermGraphSource (nid 0)
  [ (nid 0, node source $ forallIntro 0 1 source rigidA openedOuter)
  , (nid 1, node openedOuter $ intro 1 2 openedOuter)
  , (nid 2, node (arrow rigidA inner) $ Q.TypedLambda [bind 2 0 rigidA] (nid 3))
  , (nid 3, node inner $ forallIntro 3 4 inner rigidB openedInner)
  , (nid 4, node openedInner $ intro 4 5 openedInner)
  , (nid 5, node (arrow rigidB pair) $ Q.TypedLambda [bind 5 1 rigidB] (nid 6))
  , (nid 6, node pair $ Q.TypedTuple [nid 7, nid 12])
  , (nid 7, node int $ Q.TypedApply (nid 8) (nid 11) $ Q.ApplicationWitness rigidA int)
  , (nid 8, node (arrow rigidA int) $ contextApply 8 9 offeredA [given 1 0])
  , (nid 9, node offeredA $ typeApply 9 10 providerType rigidA offeredA)
  , (nid 10, node providerType $ Q.TypedGlobal (oid 10) providerName)
  , (nid 11, node rigidA $ Q.TypedLocal (oid 11) 0)
  , (nid 12, node int $ Q.TypedApply (nid 13) (nid 16) $ Q.ApplicationWitness rigidB int)
  , (nid 13, node (arrow rigidB int) $ contextApply 13 14 offeredB [given 4 0])
  , (nid 14, node offeredB $ typeApply 14 15 providerType rigidB offeredB)
  , (nid 15, node providerType $ Q.TypedGlobal (oid 15) providerName)
  , (nid 16, node rigidB $ Q.TypedLocal (oid 16) 1)
  ]
 where
  pair = T.TupleType Boxed [int, int]
  inner = T.ForallType [boundB] [classC $ variable boundB] $ arrow (variable boundB) pair
  source = T.ForallType [boundA] [classC $ variable boundA] $ arrow (variable boundA) inner
  openedOuter = context [classC rigidA] $ arrow rigidA inner
  openedInner = context [classC rigidB] $ arrow rigidB pair
  offeredA = context [classC rigidA] $ arrow rigidA int
  offeredB = context [classC rigidB] $ arrow rigidB int

repeatedGiven :: Source
repeatedGiven = Q.TermGraphSource (nid 0)
  [ (nid 0, node source $ intro 0 1 source)
  , (nid 1, node (arrow int int) $ contextApply 1 2 offered [given 0 1])
  , (nid 2, node offered $ typeApply 2 3 providerType int offered)
  , (nid 3, node providerType $ Q.TypedGlobal (oid 3) providerName)
  ]
 where
  offered = context [classC int] $ arrow int int
  source = context [classC int, classC int] $ arrow int int

shadowedGiven :: Source
shadowedGiven = Q.TermGraphSource (nid 0)
  [ (nid 0, node source $ intro 0 1 source)
  , (nid 1, node offered $ intro 1 2 offered)
  , (nid 2, node (arrow int int) $ contextApply 2 3 offered [given 0 0])
  , (nid 3, node offered $ typeApply 3 4 providerType int offered)
  , (nid 4, node providerType $ Q.TypedGlobal (oid 4) providerName)
  ]
 where
  offered = context [classC int] $ arrow int int
  source = context [classC int] offered

options :: G.RenderOptions Int
options = G.defaultRenderOptions $ \local -> "v" ++ show local

checked :: Source -> IO Graph
checked = expectRight . Q.sealTermGraphWithContext Q.sharedContextTypeStructure structure Q.defaultTermGraphLimits
 where
  structure = Q.sharedTypeStructure
    {Q.forallTypeStructure = Just Q.sharedContextualForallTypeStructure}

expectRight :: Show error => Either error value -> IO value
expectRight = either (fail . show) pure

required :: Maybe value -> value
required = maybe (error "invalid contextual replay fixture") id

withTemporaryModule :: String -> (FilePath -> IO value) -> IO value
withTemporaryModule source action = do
  temporaryDirectory <- getTemporaryDirectory
  bracket (openTempFile temporaryDirectory "djex-context-renderer-replay.hs") cleanup $ \(path, handle) -> do
    hPutStr handle source
    hClose handle
    action path
 where
  cleanup (path, handle) = do
    _ <- try (hClose handle) :: IO (Either IOError ())
    removeFile path

variable :: T.Variable String -> Ty
variable = T.TypeVariable

boundA, boundB :: T.Variable String
boundA = T.FlexibleVariable "a"
boundB = T.FlexibleVariable "b"

rigidA, rigidB, int :: Ty
rigidA = variable $ T.RigidVariable "outerReplay"
rigidB = variable $ T.RigidVariable "innerReplay"
int = T.TypeConstructor $ either (error . show) id $ parseName "Prelude.Int"

providerName :: Name
providerName = either (error . show) id $ parseName "Main.provider"

providerType :: Ty
providerType = T.ForallType [boundA] [classC $ variable boundA] $ arrow (variable boundA) int

classC :: Ty -> Constraint Ty
classC ty = Constraint (either (error . show) id $ parseName "Main.C") [ty]

arrow :: Ty -> Ty -> Ty
arrow = T.FunctionType

context :: [Constraint Ty] -> Ty -> Ty
context = T.ForallType []

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

contextApply :: Natural -> Natural -> Ty -> [Q.ContextEvidence] -> Q.TermNodeForm Ty Int
contextApply occurrence child source evidence = Q.TypedContextApplication (oid occurrence) (nid child) $
  required $ Q.contextApplicationWitness Q.sharedContextTypeStructure source evidence

forallIntro :: Natural -> Natural -> Ty -> Ty -> Ty -> Q.TermNodeForm Ty Int
forallIntro occurrence child source selected result = Q.TypedForallIntroduction (oid occurrence) (nid child) $
  Q.ForallIntroductionWitness source selected result

typeApply :: Natural -> Natural -> Ty -> Ty -> Ty -> Q.TermNodeForm Ty Int
typeApply occurrence child source selected result = Q.TypedImplicitTypeApplication (oid occurrence) (nid child) $
  Q.ImplicitTypeApplicationWitness source selected result
