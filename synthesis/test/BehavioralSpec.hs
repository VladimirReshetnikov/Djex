module BehavioralSpec (behavioralTests) where

import Data.Either (isLeft)
import Language.Haskell.Synthesis.Behavioral
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

behavioralTests :: TestTree
behavioralTests = testGroup "named behavioral query syntax"
  [ testCase "Haskell named signature uses an ordinary Boolean expression" $
      parseBehavioralQuery HaskellBehavioral
        "f :: [Int] -> [Int] where f [1,2,3] == [3,2,1]"
        @?= Right (Just (BehavioralQuery "f" "[Int] -> [Int]"
          "f [1,2,3] == [3,2,1]"))
  , testCase "Lean named signature retains a host proposition" $
      parseBehavioralQuery LeanBehavioral
        " f : List Nat → List Nat where f [1, 2] = [2, 1] ∧ f [] = [] "
        @?= Right (Just (BehavioralQuery "f" "List Nat → List Nat"
          "f [1, 2] = [2, 1] ∧ f [] = []"))
  , testCase "nested rank-N binders and multiline predicates remain exact" $
      parseBehavioralQuery HaskellBehavioral
        "f :: (forall a. a -> a) -> Int where\n let x = 1 in f id == x"
        @?= Right (Just (BehavioralQuery "f" "(forall a. a -> a) -> Int"
          "let x = 1 in f id == x"))
  , testCase "unnamed and established Length queries stay on their own path" $
      mapM_ (\input -> parseBehavioralQuery LeanBehavioral input @?= Right Nothing)
        ["List Nat → List Nat", "∀ α, α → α",
         "--where List.length result = List.length arg0 -- List Nat → List Nat"]
  , testCase "Haskell forall and type constraints are ordinary queries" $
      mapM_ (\input -> parseBehavioralQuery HaskellBehavioral input @?= Right Nothing)
        ["forall a. a -> a", "Eq a => a -> a", "(a :: Type)"]
  , testCase "nested Lean comments do not expose an internal delimiter" $
      parseBehavioralQuery LeanBehavioral
        "f : Nat /- where /- nested -/ end -/ → Nat where f 1 = 1"
        @?= Right (Just (BehavioralQuery "f"
          "Nat /- where /- nested -/ end -/ → Nat" "f 1 = 1"))
  , testCase "nested Haskell comments and type-level strings stay in type" $
      parseBehavioralQuery HaskellBehavioral
        "f :: Proxy \"where\" {- where {- nested -} -} where True"
        @?= Right (Just (BehavioralQuery "f"
          "Proxy \"where\" {- where {- nested -} -}" "True"))
  , testCase "line comments and nested syntax stay in type" $
      parseBehavioralQuery LeanBehavioral
        "f : (let whereValue := Nat; whereValue) -- where ignored\n → Nat where True"
        @?= Right (Just (BehavioralQuery "f"
          "(let whereValue := Nat; whereValue) -- where ignored\n → Nat" "True"))
  , testCase "Haskell operator starting with two hyphens is not a comment" $
      parseBehavioralQuery HaskellBehavioral "f :: a --> b where True"
        @?= Right (Just (BehavioralQuery "f" "a --> b" "True"))
  , testCase "promoted names and apostrophe identifiers are preserved" $
      parseBehavioralQuery HaskellBehavioral "f' :: Proxy 'Just -> a' where True"
        @?= Right (Just (BehavioralQuery "f'" "Proxy 'Just -> a'" "True"))
  , testCase "where in a qualified name is not the separator" $
      parseBehavioralQuery LeanBehavioral "f : Namespace.where → Nat where True"
        @?= Right (Just (BehavioralQuery "f" "Namespace.where → Nat" "True"))
  , testCase "Lean quoted type names do not expose a where delimiter" $
      parseBehavioralQuery LeanBehavioral
        "f : Namespace.«where () /-» → Nat where True"
        @?= Right (Just (BehavioralQuery "f" "Namespace.«where () /-» → Nat" "True"))
  , testCase "malformed named queries cannot degrade into unconstrained search" $
      mapM_ (\input -> assertBool input $ isLeft $
        parseBehavioralQuery LeanBehavioral input)
        ["f : Nat", "f : where True", "f : Nat where", "f : Nat where -- empty",
         "f : Nat /- unterminated", "f : (Nat where True", "f : Nat) where True",
         "f : \"where", "f : Nat where /- only a comment -/",
         "f : /- no type -/ where True", "f : «where"]
  , testCase "Lean does not consume Haskell's signature separator" $
      parseBehavioralQuery LeanBehavioral "f :: Nat where True" @?= Right Nothing
  ]
