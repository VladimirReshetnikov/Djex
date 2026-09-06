module KindGraphCases (tests) where

import Data.Either (isLeft)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Djinn.Internal.Environment (prepareGroundSynthesisEnvironment)
import Djinn.Internal.SourceGraphKinds
  ( validateSourceGraphKinds, inferSourceGraphMetavariableKinds )
import Djinn.Internal.SourceTypingContext
  ( SourceTypingContext, sourceTypingContextWithProviderKinds )
import Language.Haskell.Synthesis.Declaration (Declaration (..), ValueSignature (..))
import qualified Language.Haskell.Synthesis.Environment as E
import qualified Language.Haskell.Synthesis.Generated as G
import Language.Haskell.Synthesis.Kind (Kind (..))
import Language.Haskell.Synthesis.Name (Name, Boxity (Boxed), parseName)
import qualified Language.Haskell.Synthesis.Type as T
import qualified Language.Haskell.Synthesis.TypedGenerated as Q

type SourceType = T.Type String
type GraphType = T.Type (T.Variable String)
type Source = Q.TermGraphSource GraphType String
type Declaration' = Declaration String Void ()
type GroundKind = Kind Void

-- Synthetic kind-guard tests, deliberately distinct from public producer and
-- source-term replay tests. In particular an erased implicit application
-- followed by a visible application is not ordinary emitted Haskell syntax.
-- These tests assert only the kind validator's exact chain/identity contract.
tests :: TestTree
tests = testGroup "private source graph kind boundaries"
  [ testCase "an erased implicit slot advances the exact visible override ordinal" $ do
      authority <- context providers [("owned", [ProperTypeKind, unary])]
      assertEqual "the second slot used the first slot's proper kind" (Right ()) $
        validateSourceGraphKinds authority $ chain "owned" token
      swapped <- context providers [("owned", [unary, ProperTypeKind])]
      assertBool "the first implicit slot escaped its own kind obligation" $ isLeft $
        validateSourceGraphKinds swapped $ chain "owned" token

  , testCase "a same-shaped neighboring global cannot donate its kind vector" $ do
      wrongOwner <- context providers [("other", [ProperTypeKind, unary])]
      assertBool "an adjacent provider authorized owned's higher-kind slot" $ isLeft $
        validateSourceGraphKinds wrongOwner $ chain "owned" token
      correctOwner <- context providers [("owned", [ProperTypeKind, unary])]
      assertEqual "the exact owner control failed" (Right ()) $
        validateSourceGraphKinds correctOwner $ chain "owned" token

  , testCase "a retained vector must include the actual second source slot" $ do
      incomplete <- context providers [("owned", [ProperTypeKind])]
      rejectsWith "does not cover its exact application slot" $
        validateSourceGraphKinds incomplete $ chain "owned" token

  , testCase "a type-application witness cannot detach from its real function child" $ do
      authority <- context providers [("owned", [ProperTypeKind, unary])]
      let alter (owner, Q.TermNode ty form) = (owner, Q.TermNode ty $ case form of
            Q.TypedVisibleTypeApplication occurrence child argument witness ->
              Q.TypedVisibleTypeApplication occurrence child argument
                (witness { Q.typeApplicationSource = graphType $ forallType ["second"] atom })
            _ -> form)
          original = chain "owned" token
          detached = original {Q.termGraphSourceNodes = map alter $ Q.termGraphSourceNodes original}
      rejectsWith "actual function type" $ validateSourceGraphKinds authority detached

  , testCase "a globally coherent substituted scheme cannot impersonate the original provider" $ do
      authority <- context providers [("owned", [ProperTypeKind, unary])]
      rejectsWith "original scheme" $
        validateSourceGraphKinds authority $ chain "owned" atom

  , testCase "a term argument ends the leading provider kind telescope" $ do
      authority <- context appliedProviders [("appliedOwner", [unary])]
      assertEqual "a later proper binder inherited an exhausted provider vector" (Right ()) $
        validateSourceGraphKinds authority $ afterTermArgument atom
      assertBool "a later binder borrowed the leading higher-kind override" $ isLeft $
        validateSourceGraphKinds authority $ afterTermArgument unaryType

  , testCase "fresh source binder avoids an existing rigid with its generated spelling" $ do
      authority <- context [value "properInput" $ forallType ["a"] $
        arrow (T.TypeVariable "a") token] []
      assertEqual "source-local proper binder captured the ambient unary rigid" (Right ()) $
        validateSourceGraphKinds authority binderCollision

  , testCase "all source annotations jointly constrain an ambient higher-kind variable" $ do
      authority <- context [] []
      assertEqual "the free ambient h was prematurely defaulted to proper kind" (Right ()) $
        validateSourceGraphKinds authority $ ambientSelection "Outer"
      assertBool "an ordinary unary constructor accepted a higher-kind argument" $ isLeft $
        validateSourceGraphKinds authority $ ambientSelection "F"

  , testCase "an implicit vacuous meta inherits its exact owner's higher kind" $ do
      authority <- context vacuousProviders [("vacuous", [unary])]
      assertEqual "implicit completion replaced the retained higher kind with proper kind"
        (Right [(completionMeta, unary)]) $
          inferSourceGraphMetavariableKinds authority [completionMeta] $
            implicitCompletion $ T.TypeVariable completionMeta
      assertEqual "the closed higher-kind completion does not revalidate" (Right ()) $
        validateSourceGraphKinds authority $ implicitCompletion $ graphType unaryType
      assertBool "the exact owner's kind equation accepted a proper completion" $ isLeft $
        validateSourceGraphKinds authority $ implicitCompletion $ graphType atom

  , testCase "an ordinary implicit vacuous meta retains the proper-type default" $ do
      authority <- context vacuousProviders []
      assertEqual "the ordinary source binder became kind-polymorphic"
        (Right [(completionMeta, ProperTypeKind)]) $
          inferSourceGraphMetavariableKinds authority [completionMeta] $
            implicitCompletion $ T.TypeVariable completionMeta
      assertEqual "the ordinary closed completion does not revalidate" (Right ()) $
        validateSourceGraphKinds authority $ implicitCompletion $ graphType atom

  , testCase "implicit completion cannot borrow a neighboring provider's kind" $ do
      authority <- context vacuousProviders [("vacuousNeighbor", [unary])]
      assertEqual "implicit completion borrowed a same-shaped neighbor's override"
        (Right [(completionMeta, ProperTypeKind)]) $
          inferSourceGraphMetavariableKinds authority [completionMeta] $
            implicitCompletion $ T.TypeVariable completionMeta

  , testCase "completion infers its kind jointly with ambient source annotations" $ do
      authority <- context [] []
      assertEqual "completion defaulted the ambient argument before solving the selected meta"
        (Right [(completionMeta, FunctionKind unary ProperTypeKind)]) $
          inferSourceGraphMetavariableKinds authority [completionMeta] $
            ambientSelectionType $ T.TypeVariable completionMeta

  , testCase "completion returns only requested free flexible identities" $ do
      authority <- context [] []
      let bound = T.FlexibleVariable "completion-bound"
          discarded = T.FlexibleVariable "completion-discarded"
          rigid = T.RigidVariable "completion-rigid"
          polytype = T.ForallType [bound] [] $
            T.FunctionType (T.TypeVariable bound) $ T.TypeVariable bound
          ty = T.TupleType Boxed [T.TypeVariable completionMeta, polytype, T.TypeVariable rigid]
          source = Q.TermGraphSource (nid 0)
            [(nid 0, node ty $ Q.TypedLocal (oid 0) "synthetic-kind-only")]
      assertEqual "completion retained a bound, rigid, discarded, or duplicate request"
        (Right [(completionMeta, ProperTypeKind)]) $
          inferSourceGraphMetavariableKinds authority
            [bound, completionMeta, discarded, rigid, completionMeta] source
      assertEqual "completion invented an unrequested obligation" (Right []) $
        inferSourceGraphMetavariableKinds authority [bound, discarded, rigid] source
  ]

completionMeta :: T.Variable String
completionMeta = T.FlexibleVariable "completion-live"

vacuousProviders :: [Declaration']
vacuousProviders =
  [value owner $ forallType ["unused"] token | owner <- ["vacuous", "vacuousNeighbor"]]

implicitCompletion :: GraphType -> Source
implicitCompletion selected = Q.TermGraphSource (nid 1)
  [ (nid 0, node before $ Q.TypedGlobal (oid 0) $ name "vacuous")
  , (nid 1, node after $ Q.TypedImplicitTypeApplication (oid 1) (nid 0) $
      Q.ImplicitTypeApplicationWitness before selected after)
  ]
 where
  before = graphType $ forallType ["unused"] token
  after = graphType token

providers :: [Declaration']
providers = [value owner $ forallType ["first", "second"] token | owner <- ["owned", "other"]]

chain :: String -> SourceType -> Source
chain owner result = Q.TermGraphSource (nid 2)
  [ (nid 0, node before $ Q.TypedGlobal (oid 0) $ name owner)
  , (nid 1, node between $ Q.TypedImplicitTypeApplication (oid 1) (nid 0) $
      Q.ImplicitTypeApplicationWitness before (graphType atom) between)
  , (nid 2, node after $ Q.TypedVisibleTypeApplication (oid 2) (nid 1) (specified unaryType) $
      Q.TypeApplicationWitness between (graphType unaryType) after Nothing)
  ]
 where
  before = graphType $ forallType ["first", "second"] result
  between = graphType $ forallType ["second"] result
  after = graphType result

appliedProviders :: [Declaration']
appliedProviders =
  [ value "appliedOwner" $ forallType ["f"] $
      arrow (T.TypeApplication (T.TypeVariable "f") atom) $ forallType ["later"] token
  , value "inputValue" $ application "F" [atom]
  ]

afterTermArgument :: SourceType -> Source
afterTermArgument selected = Q.TermGraphSource (nid 4)
  [ (nid 0, node original $ Q.TypedGlobal (oid 0) $ name "appliedOwner")
  , (nid 1, node instantiated $ Q.TypedImplicitTypeApplication (oid 1) (nid 0) $
      Q.ImplicitTypeApplicationWitness original (graphType unaryType) instantiated)
  , (nid 2, node domain $ Q.TypedGlobal (oid 2) $ name "inputValue")
  , (nid 3, node residual $ Q.TypedApply (nid 1) (nid 2) $ Q.ApplicationWitness domain residual)
  , (nid 4, node (graphType token) $ Q.TypedVisibleTypeApplication (oid 4) (nid 3)
      (specified selected) $ Q.TypeApplicationWitness residual (graphType selected) (graphType token) Nothing)
  ]
 where
  residual = graphType $ forallType ["later"] token
  domain = graphType $ application "F" [atom]
  instantiated = T.FunctionType domain residual
  original = graphType $ forallType ["f"] $
    arrow (T.TypeApplication (T.TypeVariable "f") atom) $ forallType ["later"] token

binderCollision :: Source
binderCollision = Q.TermGraphSource (nid 14)
  [ (nid 0, node source $ Q.TypedGlobal (oid 0) $ name "properInput")
  , (nid 10, node result $ Q.TypedImplicitTypeApplication (oid 10) (nid 0) $
      Q.ImplicitTypeApplicationWitness source (graphType atom) result)
  , (nid 11, node ambientValue $ Q.TypedLocal (oid 11) "ambient")
  , (nid 12, node pair $ Q.TypedTuple [nid 10, nid 11])
  , (nid 14, node (T.FunctionType ambientValue pair) $
      Q.TypedLambda [bind 14 "ambient" ambientValue] (nid 12))
  ]
 where
  source = graphType $ forallType ["a"] $ arrow (T.TypeVariable "a") token
  result = graphType $ arrow atom token
  -- Node10's private kind-opening allocator starts with exactly this name.
  -- This existing rigid has a different kind, so failure to freshen is visible.
  ambient = T.TypeVariable $ T.RigidVariable "$djinn$kind$10"
  ambientValue = T.TypeApplication ambient $ graphType atom
  pair = T.TupleType Boxed [result, ambientValue]

ambientSelection :: String -> Source
ambientSelection = ambientSelectionType . graphType . nominal

ambientSelectionType :: GraphType -> Source
ambientSelectionType selected = Q.TermGraphSource (nid 4)
  [ (nid 0, node source $ Q.TypedLocal (oid 0) "function")
  , (nid 1, node result $ Q.TypedImplicitTypeApplication (oid 1) (nid 0) $
      Q.ImplicitTypeApplicationWitness source selected result)
  , (nid 2, node argument $ Q.TypedLocal (oid 2) "argument")
  , (nid 3, node (graphType token) $ Q.TypedApply (nid 1) (nid 2) $
      Q.ApplicationWitness argument (graphType token))
  , (nid 4, node (T.FunctionType source $ T.FunctionType argument $ graphType token) $
      Q.TypedLambda [bind 4 "function" source, bind 5 "argument" argument] (nid 3))
  ]
 where
  h = T.TypeVariable $ T.RigidVariable "ambient-h"
  f = T.FlexibleVariable "f"
  source = T.ForallType [f] [] $ T.FunctionType
    (T.TypeApplication (T.TypeVariable f) h) $ graphType token
  result = T.FunctionType (T.TypeApplication selected h) $ graphType token
  -- The actual argument fixes h :: * -> *, independently of the source forall.
  argument = T.TypeApplication (graphType $ nominal "Outer") h

context :: [Declaration'] -> [(String, [GroundKind])] -> IO SourceTypingContext
context extra overrides = do
  environment <- right $ E.mkEnvironment $
    [ AbstractTypeDeclaration () (name "A") ProperTypeKind
    , AbstractTypeDeclaration () (name "Token") ProperTypeKind
    , AbstractTypeDeclaration () (name "F") unary
    , AbstractTypeDeclaration () (name "Outer") $ FunctionKind unary ProperTypeKind
    ] ++ extra
  prepared <- right $ prepareGroundSynthesisEnvironment environment
  pure $ sourceTypingContextWithProviderKinds prepared token $
    Map.fromList [(name owner, kinds) | (owner, kinds) <- overrides]

value :: String -> SourceType -> Declaration'
value owner = ValueDeclaration . ValueSignature () (name owner)

name :: String -> Name
name = either (error . show) id . parseName

nominal :: String -> SourceType
nominal = T.TypeConstructor . name

atom, token, unaryType :: SourceType
atom = nominal "A"
token = nominal "Token"
unaryType = nominal "F"

unary :: GroundKind
unary = FunctionKind ProperTypeKind ProperTypeKind

arrow :: SourceType -> SourceType -> SourceType
arrow = T.FunctionType

forallType :: [String] -> SourceType -> SourceType
forallType binders = T.ForallType binders []

application :: String -> [SourceType] -> SourceType
application constructor = foldl T.TypeApplication $ nominal constructor

graphType :: SourceType -> GraphType
graphType = fmap T.FlexibleVariable

specified :: SourceType -> G.VisibleTypeArgument
specified = either (error . show) id . G.specifiedVisibleTypeArgument

nid :: Integer -> Q.TermNodeId
nid = Q.termNodeId . fromInteger

oid :: Integer -> Q.OccurrenceId
oid = Q.occurrenceId . fromInteger

node :: GraphType -> Q.TermNodeForm GraphType String -> Q.TermNode GraphType String
node = Q.TermNode

bind :: Integer -> String -> GraphType -> Q.TypedPattern GraphType String
bind occurrence local ty = Q.TypedPattern (oid occurrence) ty $ Q.TypedBind local

right :: Show failure => Either failure result -> IO result
right = either (fail . show) pure

rejectsWith :: String -> Either String () -> IO ()
rejectsWith expected result = case result of
  Left failure -> assertBool ("wrong rejection: " ++ failure) $ expected `isInfixOf` failure
  Right () -> assertFailure $ "expected kind rejection containing " ++ expected
