module SourceKindSpec (sourceKindTests) where

import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import Language.Haskell.Synthesis.Constraint (Constraint (..))
import Language.Haskell.Synthesis.Kind (Kind (..))
import Language.Haskell.Synthesis.KindInference
  ( KindAssumptions (..), inferVariableKindsForObligations )
import Language.Haskell.Synthesis.Name (mkIdentifier)
import Language.Haskell.Synthesis.SourceKind
import Language.Haskell.Synthesis.Type (Type (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

sourceKindTests :: TestTree
sourceKindTests = testGroup "lexical source binder kinds"
  [ testCase "retain an explicit kind on a vacuous binder" $ do
      let ty = ForallType ["f", "a"] [] $ FunctionType (v "a") (v "a")
          annotations = [at [] 0 higher]
      checked <- right $ prepareSourceTypeKinds empty ty annotations
      sourceKindsType checked @?= ty
      sourceKindAnnotations checked @?= annotations
      sourceBinderKinds checked @?= Map.fromList [(([],0),higher),(([],1),ProperTypeKind)]
      inferred <- right $ prepareSourceTypeKinds empty ty []
      Map.lookup ([],0) (sourceBinderKinds inferred) @?= Just ProperTypeKind
  , testCase "constrain every occurrence with the explicit higher kind" $ do
      let ty = ForallType ["f","a"] [] $ FunctionType (app (v "f") (v "a")) (app (v "f") (v "a"))
      checked <- right $ prepareSourceTypeKinds empty ty [at [] 0 higher]
      Map.lookup ([],0) (sourceBinderKinds checked) @?= Just higher
      assertBool "a proper-kind annotation accepted constructor application" $
        isLeft $ prepareSourceTypeKinds empty ty [at [] 0 ProperTypeKind]
  , testCase "shadowed names retain different lexical kinds" $ do
      let inner = ForallType ["f"] [] $ FunctionType (v "f") (v "f")
          ty = ForallType ["f","a"] [] $ FunctionType (app (v "f") (v "a")) inner
          nested = [ForallBody,FunctionResult]
      checked <- right $ prepareSourceTypeKinds empty ty [at [] 0 higher,at nested 0 ProperTypeKind]
      Map.lookup (nested,0) (sourceBinderKinds checked) @?= Just ProperTypeKind
      Map.lookup ([],0) (sourceBinderKinds checked) @?= Just higher
      assertBool "the inner binder inherited the outer kind" $
        isLeft $ prepareSourceTypeKinds empty ty [at [] 0 higher,at nested 0 higher]
  , testCase "equal sibling spellings have independent vacuous kind authority" $ do
      let scheme = ForallType ["f","a"] [] $ FunctionType (v "a") (v "a")
      checked <- right $ prepareSourceTypeKinds empty (FunctionType scheme scheme)
        [at [FunctionParameter] 0 ProperTypeKind,at [FunctionResult] 0 higher]
      Map.lookup ([FunctionParameter],0) (sourceBinderKinds checked) @?= Just ProperTypeKind
      Map.lookup ([FunctionResult],0) (sourceBinderKinds checked) @?= Just higher
  , testCase "class parameter kinds and explicit binder kinds are checked jointly" $ do
      className <- right $ mkIdentifier "Higher"
      let assumptions = KindAssumptions Map.empty $ Map.singleton className [Just higher]
          ty = ForallType ["f","a"] [Constraint className [v "f"]] $ FunctionType (v "a") (v "a")
      checked <- right $ prepareSourceTypeKinds assumptions ty [at [] 0 higher]
      sourceKindAssumptions checked @?= assumptions
      assertBool "class use did not constrain the annotated binder" $
        isLeft $ prepareSourceTypeKinds assumptions ty [at [] 0 ProperTypeKind]
  , testCase "forall sites inside class arguments remain addressable" $ do
      className <- right $ mkIdentifier "Proper"
      let assumptions = KindAssumptions Map.empty $ Map.singleton className [Just ProperTypeKind]
          nested = ForallType ["f","a"] [] $ FunctionType (v "a") (v "a")
          ty = ForallType ["a"] [Constraint className [nested]] $ v "a"
          site = [ForallConstraintArgument 0 0]
      checked <- right $ prepareSourceTypeKinds assumptions ty [at site 0 higher]
      Map.lookup (site,0) (sourceBinderKinds checked) @?= Just higher
  , testCase "free variables do not borrow a same-spelled bound kind" $ do
      let ty = FunctionType (v "f") $ ForallType ["f","a"] [] $ FunctionType (v "a") (v "a")
      checked <- right $ prepareSourceTypeKinds empty ty [at [FunctionResult] 0 higher]
      Map.lookup ([FunctionResult],0) (sourceBinderKinds checked) @?= Just higher
  , testCase "an annotation cannot refer to a different site or missing slot" $ do
      let ty = ForallType ["a"] [] $ v "a"
      prepareSourceTypeKinds empty ty [at [ForallBody] 0 ProperTypeKind] @?=
        Left (MissingSourceKindBinder [ForallBody] 0)
      prepareSourceTypeKinds empty ty [at [] 1 ProperTypeKind] @?= Left (MissingSourceKindBinder [] 1)
      prepareSourceTypeKinds empty ty [at [] (-1) ProperTypeKind] @?= Left (MissingSourceKindBinder [] (-1))
  , testCase "duplicate annotations are refused even when equal" $ do
      let ty = ForallType ["a"] [] $ v "a"
      prepareSourceTypeKinds empty ty [at [] 0 ProperTypeKind,at [] 0 ProperTypeKind] @?=
        Left (DuplicateSourceKindAnnotation [] 0)
  , testCase "malformed binder lists do not acquire kind authority" $
      assertBool "duplicate source binder accepted" $ isLeft $
        prepareSourceTypeKinds empty (ForallType ["a","a"] [] $ v "a") []
  , testCase "renaming source spellings preserves lexical kind positions" $ do
      let ty = ForallType ["f","a"] [] $ FunctionType (app (v "f") (v "a")) (v "a")
      before <- right $ prepareSourceTypeKinds empty ty [at [] 0 higher]
      after <- right $ prepareSourceTypeKinds empty (fmap ("renamed_" ++) ty) [at [] 0 higher]
      sourceBinderKinds before @?= sourceBinderKinds after
  , testCase "joint inference does not duplicate requested variable identities" $
      assertBool "duplicate inference request accepted" $ isLeft $
        inferVariableKindsForObligations empty ["a","a"] [(ProperTypeKind,v "a")]
  ]
 where
  empty = KindAssumptions Map.empty Map.empty
  higher = FunctionKind ProperTypeKind ProperTypeKind
  v = TypeVariable
  app = TypeApplication
  at = SourceKindAnnotation
  right result = either (fail . show) pure result
