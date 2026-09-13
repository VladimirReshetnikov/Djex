module KindedSourceConversionSpec (tests) where

import Control.Monad.Trans.Except (runExceptT)
import Data.Either (isLeft)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (listToMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Language.Haskell.Exts as HSE
import Language.Haskell.Djex.HaskellSrc.Kinded (convertKindedSourceType)
import Language.Haskell.Exference.TypeFromHaskellSrc
  ( TypeResolver (..), legacyTypeResolver, haskellSrcExtsParseMode
  , convertTypeNoDeclWithResolver )
import Language.Haskell.Synthesis.Kind (Kind (..))
import Language.Haskell.Synthesis.KindInference (KindAssumptions (..))
import Language.Haskell.Synthesis.Name (mkIdentifier, maximumTupleArity)
import Language.Haskell.Synthesis.SourceKind
import Language.Haskell.Synthesis.SourceKind.Expansion (expandSourceTypeKinds)
import Language.Haskell.Synthesis.Type (Type (..), Variable (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests = testGroup "checked kinded source conversion"
  [ testCase "outer higher-kinded source annotation" $ do
      checked <- convert empty "forall (f :: * -> *) a. f a -> f a"
      Map.lookup ([],0) (sourceBinderKinds checked) @?= Just higher
      sourceKindAnnotations checked @?= [at [] 0 higher]
  , testCase "nested polymorphic callback preserves its annotation" $ do
      checked <- convert empty "forall b. ((forall (f :: * -> *) a. f a -> f a) -> b) -> b"
      Map.lookup ([ForallBody,FunctionParameter,FunctionParameter],0)
        (sourceBinderKinds checked) @?= Just higher
  , testCase "vacuous kinded binder retains its explicit higher kind" $ do
      checked <- convert empty "forall (f :: (* -> *)) a. a -> a"
      Map.lookup ([],0) (sourceBinderKinds checked) @?= Just higher
  , testCase "contradictory explicit kind fails before synthesis" $ do
      ast <- parse "forall (f :: *) a. f a -> f a"
      assertBool "contradictory source annotation accepted" $ isLeft $
        runIdentity $ runExceptT $ convertKindedSourceType empty (resolver empty) Nothing ast
  , testCase "shadowed source names have independent kinds" $ do
      checked <- convert empty "forall (f :: * -> *) a. f a -> (forall (f :: *). f -> f)"
      Map.lookup ([],0) (sourceBinderKinds checked) @?= Just higher
      Map.lookup ([ForallBody,FunctionResult],0) (sourceBinderKinds checked) @?= Just proper
  , testCase "tuple conversion and canonicalization transport both annotations" $ do
      checked <- convert empty "((forall (f :: * -> *) a. a -> a), (forall (f :: *) a. a -> a))"
      expanded <- right $ expandSourceTypeKinds fresh Map.empty checked
      Map.lookup ([TupleElement 0],0) (sourceBinderKinds expanded) @?= Just higher
      Map.lookup ([TupleElement 1],0) (sourceBinderKinds expanded) @?= Just proper
  , testCase "class argument conversion keeps nested source annotations" $ do
      cls <- right $ mkIdentifier "C"
      let assumptions = KindAssumptions Map.empty (Map.singleton cls [Just proper])
      checked <- convert assumptions "forall a. C (forall (f :: * -> *) b. b -> b) => a -> a"
      Map.lookup ([ForallConstraintArgument 0 0],0) (sourceBinderKinds checked) @?= Just higher
  , testCase "source synonym expansion copies annotations end to end" $ do
      alias <- right $ mkIdentifier "Dup"
      let assumptions = KindAssumptions (Map.singleton alias higher) Map.empty
      checked <- convert assumptions "Dup (forall (f :: * -> *) a. a -> a)"
      let x = FlexibleVariable 100
          definitions = Map.singleton alias ([x],FunctionType (TypeVariable x) (TypeVariable x))
      expanded <- right $ expandSourceTypeKinds fresh definitions checked
      Map.lookup ([FunctionParameter],0) (sourceBinderKinds expanded) @?= Just higher
      Map.lookup ([FunctionResult],0) (sourceBinderKinds expanded) @?= Just higher
  , testCase "the type-only converter still refuses annotated source" $ do
      ast <- parse "forall (f :: * -> *) a. a -> a"
      assertBool "type-only conversion silently discarded a kind" $ isLeft $
        runIdentity $ runExceptT $ convertTypeNoDeclWithResolver (resolver empty) Nothing ast
  , testCase "an unresolved kind variable cannot be silently defaulted" $ do
      ast <- parse "forall (f :: k) a. a -> a"
      assertBool "unresolved source kind was erased" $ isLeft $
        runIdentity $ runExceptT $ convertKindedSourceType empty (resolver empty) Nothing ast
  , testCase "invalid tuple width is rejected before reading its elements" $ do
      let ast = HSE.TyTuple HSE.noSrcSpan HSE.Boxed $
            replicate (maximumTupleArity + 1) (error "invalid tuple element was forced")
      assertBool "oversized caller-built tuple accepted" $ isLeft $
        runIdentity $ runExceptT $ convertKindedSourceType empty (resolver empty) Nothing ast
  ]
 where
  proper = ProperTypeKind
  higher = FunctionKind proper proper
  empty = KindAssumptions Map.empty Map.empty
  at = SourceKindAnnotation
  resolver assumptions = (legacyTypeResolver Map.empty $ Map.keys $ typeConstructorKinds assumptions)
    { resolverClassArities = Map.map length $ classParameterKinds assumptions
    , resolverUnqualifiedClassNames = Map.keys $ classParameterKinds assumptions }
  convert assumptions source = do
    ast <- parse source
    fst <$> right (runIdentity $ runExceptT $
      convertKindedSourceType assumptions (resolver assumptions) Nothing ast)
  fresh reserved _ = listToMaybe
    [candidate | index <- [0..], let candidate = FlexibleVariable index, Set.notMember candidate reserved]

parse :: String -> IO (HSE.Type HSE.SrcSpanInfo)
parse source = case HSE.parseTypeWithMode (haskellSrcExtsParseMode "kinded-conversion") source of
  HSE.ParseOk ast -> pure ast
  HSE.ParseFailed location failure -> fail $ show location ++ ": " ++ failure

right :: Show failure => Either failure value -> IO value
right = either (fail . show) pure
