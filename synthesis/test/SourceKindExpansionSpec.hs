module SourceKindExpansionSpec (sourceKindExpansionTests) where

import Data.Either (isLeft)
import Data.Maybe (listToMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Language.Haskell.Synthesis.Constraint (Constraint (..))
import Language.Haskell.Synthesis.Kind (Kind (..))
import Language.Haskell.Synthesis.KindInference (KindAssumptions (..))
import Language.Haskell.Synthesis.Name (Name, Boxity (Boxed), mkIdentifier, tupleName)
import Language.Haskell.Synthesis.SourceKind
import Language.Haskell.Synthesis.SourceKind.Expansion
import Language.Haskell.Synthesis.Type (Type (..), freeVariables)
import Language.Haskell.Synthesis.TypeSynonym (SynonymExpansionError (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

sourceKindExpansionTests :: TestTree
sourceKindExpansionTests = testGroup "source kinds through synonym expansion"
  [ testCase "unchanged source binders retain their spelling-hint identities" $ do
      checked <- right $ prepareSourceTypeKinds (nominal []) poly [at [] 0 higher]
      expanded <- right $ expandSourceTypeKinds fresh Map.empty checked
      sourceKindsType expanded @?= poly
      sourceBinderKinds expanded @?= sourceBinderKinds checked
  , testCase "cloned polymorphic arguments retain vacuous higher kinds" $ do
      dup <- name "Dup"
      let assumptions = nominal [(dup, higher)]
          source = TypeApplication (TypeConstructor dup) poly
          definitions = Map.singleton dup (["x"], FunctionType (v "x") (v "x"))
      checked <- right $ prepareSourceTypeKinds assumptions source
        [at [ApplicationArgument] 0 higher]
      expanded <- right $ expandSourceTypeKinds fresh definitions checked
      sourceBinderKinds expanded @?= Map.fromList
        [ (([FunctionParameter],0),higher), (([FunctionParameter],1),proper)
        , (([FunctionResult],0),higher), (([FunctionResult],1),proper) ]
      case sourceKindsType expanded of
        FunctionType (ForallType left _ _) (ForallType rightBinders _ _) ->
          assertBool "cloned binders are not disjoint" $
            Set.null $ Set.intersection (Set.fromList left) (Set.fromList rightBinders)
        other -> fail $ show other
  , testCase "tuple canonicalization relocates source binder paths" $ do
      tuple <- right $ tupleName Boxed 2
      let ty = TypeApplication (TypeApplication (TypeConstructor tuple) poly) poly
      checked <- right $ prepareSourceTypeKinds (nominal []) ty
        [ at [ApplicationFunction,ApplicationArgument] 0 higher
        , at [ApplicationArgument] 0 proper ]
      expanded <- right $ expandSourceTypeKinds fresh Map.empty checked
      Map.lookup ([TupleElement 0],0) (sourceBinderKinds expanded) @?= Just higher
      Map.lookup ([TupleElement 1],0) (sourceBinderKinds expanded) @?= Just proper
  , testCase "discarded argument leaves no stale annotation site" $ do
      forget <- name "Forget"
      int <- name "Int"
      let assumptions = nominal [(forget,higher),(int,proper)]
          ty = TypeApplication (TypeConstructor forget) poly
          definitions = Map.singleton forget (["x"],TypeConstructor int)
      checked <- right $ prepareSourceTypeKinds assumptions ty [at [ApplicationArgument] 0 higher]
      expanded <- right $ expandSourceTypeKinds fresh definitions checked
      sourceKindsType expanded @?= TypeConstructor int
      sourceKindAnnotations expanded @?= []
  , testCase "invalid annotation is rejected before a phantom argument disappears" $ do
      forget <- name "Forget"
      let ty = TypeApplication (TypeConstructor forget) $
            ForallType ["f","a"] [] $ FunctionType (TypeApplication (v "f") (v "a")) (v "a")
      assertBool "phantom source was not checked" $ isLeft $
        prepareSourceTypeKinds (nominal [(forget,higher)]) ty [at [ApplicationArgument] 0 proper]
  , testCase "inferred kind survives erasure of its only constraining use" $ do
      forget <- name "ForgetHigher"
      int <- name "Int"
      let assumptions = nominal [(forget,FunctionKind higher proper),(int,proper)]
          ty = ForallType ["f"] [] $ TypeApplication (TypeConstructor forget) (v "f")
          definitions = Map.singleton forget (["x"],TypeConstructor int)
      checked <- right $ prepareSourceTypeKinds assumptions ty []
      expanded <- right $ expandSourceTypeKinds fresh definitions checked
      Map.lookup ([],0) (sourceBinderKinds expanded) @?= Just higher
      sourceKindAssumptions expanded @?= assumptions
  , testCase "definition binder cannot capture a source free variable" $ do
      bind <- name "Bind"
      let ty = TypeApplication (TypeConstructor bind) (v "a")
          definitions = Map.singleton bind (["x"],ForallType ["a"] [] $ FunctionType (v "x") (v "a"))
      checked <- right $ prepareSourceTypeKinds (nominal [(bind,higher)]) ty []
      expanded <- right $ expandSourceTypeKinds fresh definitions checked
      freeVariables (sourceKindsType expanded) @?= Set.singleton "a"
      case sourceKindsType expanded of
        ForallType [binder] [] (FunctionType (TypeVariable free) (TypeVariable bound)) -> do
          free @?= "a"
          bound @?= binder
          assertBool "source free variable captured" $ binder /= free
        other -> fail $ show other
  , testCase "shadowed binders retain separate kind evidence after expansion" $ do
      identity <- name "Identity"
      let ty = ForallType ["f","a"] [] $ FunctionType
            (TypeApplication (v "f") (v "a"))
            (TypeApplication (TypeConstructor identity) $ ForallType ["f"] [] $ FunctionType (v "f") (v "f"))
      checked <- right $ prepareSourceTypeKinds (nominal [(identity,higher)]) ty
        [at [] 0 higher,at [ForallBody,FunctionResult,ApplicationArgument] 0 proper]
      expanded <- right $ expandSourceTypeKinds fresh (Map.singleton identity (["x"],v "x")) checked
      Map.lookup ([],0) (sourceBinderKinds expanded) @?= Just higher
      Map.lookup ([ForallBody,FunctionResult],0) (sourceBinderKinds expanded) @?= Just proper
      Set.size (foldMap Set.singleton $ sourceKindsType expanded) @?= 3
  , testCase "class argument expansion preserves moved nested binder evidence" $ do
      identity <- name "Identity"
      cls <- name "C"
      let assumptions = KindAssumptions (Map.singleton identity higher) (Map.singleton cls [Just proper])
          ty = ForallType ["a"] [Constraint cls [TypeApplication (TypeConstructor identity) poly]] $ v "a"
      checked <- right $ prepareSourceTypeKinds assumptions ty
        [at [ForallConstraintArgument 0 0,ApplicationArgument] 0 higher]
      expanded <- right $ expandSourceTypeKinds fresh (Map.singleton identity (["x"],v "x")) checked
      Map.lookup ([ForallConstraintArgument 0 0],0) (sourceBinderKinds expanded) @?= Just higher
  , testCase "post-expansion checking refuses inconsistent raw definitions" $ do
      bad <- name "Bad"
      int <- name "Int"
      let assumptions = nominal [(bad,proper),(int,proper)]
      checked <- right $ prepareSourceTypeKinds assumptions (TypeConstructor bad) []
      let definitions = Map.singleton bad ([],TypeApplication (TypeConstructor int) (TypeConstructor int))
      assertBool "ill-kinded definition survived expansion" $ isLeft $
        expandSourceTypeKinds fresh definitions checked
  , testCase "source synonym saturation errors remain visible" $ do
      alias <- name "Alias"
      checked <- right $ prepareSourceTypeKinds (nominal [(alias,proper)]) (TypeConstructor alias) []
      expandSourceTypeKinds fresh (Map.singleton alias (["x"],v "x")) checked @?=
        Left (SourceKindExpansionFailure $ UnsaturatedTypeSynonym alias 1 0)
  , testCase "projected allocator collision cannot hide behind a different kind tag" $ do
      let ty = FunctionType (v "f") poly
      checked <- right $ prepareSourceTypeKinds (nominal []) ty [at [FunctionResult] 0 higher]
      assertBool "colliding underlying variable accepted" $ isLeft $
        expandSourceTypeKinds (\_ _ -> Just "f") Map.empty checked
  , testCase "fresh supply exhaustion is a checked failure" $ do
      checked <- right $ prepareSourceTypeKinds (nominal []) (FunctionType poly poly)
        [at [FunctionParameter] 0 higher,at [FunctionResult] 0 higher]
      assertBool "exhausted allocator accepted" $ isLeft $
        expandSourceTypeKinds (\_ _ -> Nothing) Map.empty checked
  ]
 where
  poly = ForallType ["f","a"] [] $ FunctionType (v "a") (v "a")
  proper = ProperTypeKind
  higher = FunctionKind proper proper
  v = TypeVariable
  at = SourceKindAnnotation
  nominal entries = KindAssumptions (Map.fromList entries) Map.empty

fresh :: Set.Set String -> String -> Maybe String
fresh reserved old = listToMaybe
  [candidate | index <- [(0 :: Integer)..], let candidate = old ++ "_" ++ show index, Set.notMember candidate reserved]

name :: String -> IO Name
name = right . mkIdentifier

right :: Show failure => Either failure value -> IO value
right = either (fail . show) pure
