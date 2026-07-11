module Main (main) where

import Data.Monoid (Any (..))
import Data.Tree (Tree (..))
import Data.Functor.Identity (runIdentity)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.MultiRWS (runMultiRWSTNil)
import qualified Language.Haskell.Exts.Syntax as HSE

import Language.Haskell.Exference.Core.SearchTree
import Language.Haskell.Exference.Core (constraintsRelaxedAtStep)
import Language.Haskell.Exference.Core.ConstraintSolver
import Language.Haskell.Exference.Core.Expression (Expression (..))
import Language.Haskell.Exference.Core.ExpressionCheck
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.Unify
import Language.Haskell.Exference
  ( ExferenceInput (..)
  , ExferenceInputError (..)
  , findExpressionsEither
  , findOneExpression
  )
import Language.Haskell.Exference.EnvironmentParser (parseRatings)
import Language.Haskell.Exference.ExpressionToHaskellSrc (convert)
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc (applyTypeDecls, parseType)
import Language.Haskell.Exference.TypeFromHaskellSrc (haskellSrcExtsParseMode)
import Language.Haskell.Exference.SimpleDict (defaultHeuristicsConfig, emptyClassEnv)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Exference"
  [ testGroup "class environments"
      [ testCase "superclass closure terminates across cycles" $ do
          let classA = HsTypeClass (name "A") [0]
                [HsConstraint classB [TypeVar 0]]
              classB = HsTypeClass (name "B") [0]
                [HsConstraint classA [TypeVar 0]]
              constraints = Set.singleton $ HsConstraint classA [TypeVar 1]
          Set.map (tclass_name . constraint_tclass)
            (inflateHsConstraints constraints)
            @?= Set.fromList [name "A", name "B"]
      , testCase "adding constraints retains the existing variable index" $ do
          let cls = HsTypeClass (name "C") [0] []
              env0 = mkQueryClassEnv (mkStaticClassEnv [cls] [])
                [HsConstraint cls [TypeVar 1]]
              env1 = addQueryClassEnv [HsConstraint cls [TypeVar 2]] env0
          IntMap.keys (qClassEnv_varConstraints env1) @?= [1, 2]
      , testCase "unknown classes remain nominally distinct" $
          unknownTypeClass (name "Foo") == unknownTypeClass (name "Bar") @?= False
      , testCase "cyclic instance prerequisites remain unresolved" $ do
          let cls = HsTypeClass (name "C") [0] []
              prerequisite = HsConstraint cls [TypeVar 0]
              cyclicInstance = HsInstance [prerequisite] cls [TypeVar 0]
              query = HsConstraint cls [TypeCons $ name "Int"]
              environment = mkQueryClassEnv
                (mkStaticClassEnv [cls] [cyclicInstance]) []
          isPossible environment [query] @?= Just [query]
          filterUnresolved environment [query] @?= Just [query]
      ]
  , testGroup "type traversal"
      [ testCase "forall substitution protects context binders" $ do
          let cls = HsTypeClass (name "C") [0, 1] []
              ty = TypeForall [0]
                [HsConstraint cls [TypeVar 0, TypeVar 1]]
                (TypeVar 0)
              replacement = TypeCons (name "Replacement")
              (changed, actual) = applySubsts
                (IntMap.fromList [(0, replacement), (1, replacement)]) ty
          getAny changed @?= True
          actual @?= TypeForall [0]
            [HsConstraint cls [TypeVar 0, replacement]]
            (TypeVar 0)
      , testCase "free variables include a forall context" $ do
          let cls = HsTypeClass (name "C") [0] []
              ty = TypeForall [0]
                [HsConstraint cls [TypeVar 0, TypeVar 1]]
                (TypeVar 0)
          freeVars ty @?= Set.singleton 1
      , testCase "largestId includes forall binders and context variables" $ do
          let cls = HsTypeClass (name "C") [0] []
              ty = TypeForall [7] [HsConstraint cls [TypeVar 9]] (TypeVar 2)
          largestId ty @?= 9
      , testCase "unification rejects nested foralls conservatively" $ do
          let polymorphic = TypeForall [0] [] (TypeVar 0)
          unify polymorphic (TypeVar 1) @?= Nothing
          unifyRight (TypeVar 1) polymorphic @?= Nothing
      , testCase "failed synonym expansion preserves arguments" $ do
          let alias = name "Alias"
              argument = TypeCons (name "Argument")
          applyTypeDecls (Map.singleton alias $ Left "invalid declaration")
            (TypeApp (TypeCons alias) argument)
            @?= Right (TypeApp (TypeCons alias) argument)
      ]
  , testGroup "search policy"
      [ testCase "constraint relaxation ends at the configured step" $ do
          constraintsRelaxedAtStep False 2 1 @?= True
          constraintsRelaxedAtStep False 2 2 @?= True
          constraintsRelaxedAtStep False 2 3 @?= False
          constraintsRelaxedAtStep True 2 3 @?= True
      , testCase "nested foralls return a structured input error" $ do
          let polymorphic = TypeForall [0] [] (TypeVar 0)
              goal = TypeArrow polymorphic polymorphic
              input = ExferenceInput goal [] [] emptyClassEnv
                False False 0 False 20 Nothing defaultHeuristicsConfig
          case findExpressionsEither input of
            Left actual -> actual @?= NestedForallInGoal goal
            Right _ -> fail "nested forall was accepted"
      ]
  , testGroup "parsing and diagnostics"
      [ testCase "ratings reject a missing value" $
          parseRatings "foo" @?= Left
            "rating file ends with a name but no numeric rating"
      , testCase "ratings reject a malformed number" $
          parseRatings "foo nope" @?= Left "invalid rating for foo: nope"
      , testCase "ratings reject non-finite values" $
          parseRatings "foo NaN" @?= Left "rating for foo must be finite: NaN"
      , testCase "current HSE parses and elaborates constrained types" $
          case parseTypePure "Eq a => a -> a" of
            Left message -> fail message
            Right (TypeForall [] [HsConstraint cls [TypeVar 0]]
                    (TypeArrow (TypeVar 0) (TypeVar 0)), _) ->
              tclass_name cls @?= name "Eq"
            Right result -> fail $ "unexpected elaboration: " ++ show result
      ]
  , testGroup "Haskell AST conversion"
      [ testCase "lowercase names use Var rather than Con" $
          case convert 0 (ExpName $ name "id") of
            HSE.Var{} -> pure ()
            expression -> fail $ "expected Var, got " ++ show expression
      , testCase "uppercase names use Con" $
          case convert 0 (ExpName $ name "Just") of
            HSE.Con{} -> pure ()
            expression -> fail $ "expected Con, got " ++ show expression
      ]
  , testGroup "independent expression checking"
      [ testCase "accepts a typed identity" $ do
          let variable = TypeVar 0
              identity = ExpLambda 1 variable (ExpVar 1 variable)
              classEnvironment = mkQueryClassEnv (mkStaticClassEnv [] []) []
          checkExpression classEnvironment [] []
            (TypeArrow variable variable) [] identity @?= Right ()
      , testCase "rejects a mismatched variable annotation" $ do
          let integer = TypeCons $ name "Int"
              boolean = TypeCons $ name "Bool"
              malformed = ExpLambda 1 integer (ExpVar 1 boolean)
              classEnvironment = mkQueryClassEnv (mkStaticClassEnv [] []) []
          checkExpression classEnvironment [] []
            (TypeArrow integer integer) [] malformed
            @?= Left (TypeMismatch integer boolean)
      , testCase "guards the live search result boundary" $ do
          let variable = TypeVar 0
              input = ExferenceInput
                (TypeArrow variable variable)
                [] [] emptyClassEnv
                False False 0 False 20 Nothing defaultHeuristicsConfig
          case findOneExpression input of
            Nothing -> fail "identity search produced no checked result"
            Just _ -> pure ()
      ]
  , testCase "search tree counts processed descendants" $ do
      let expression = error "expression should remain lazy in this test"
          tree = buildSearchTree
            ([(0, 0, expression), (1, 0, expression), (2, 1, expression)], [0, 1, 2])
            (0 :: Int)
      case tree of
        Node (total, processed, _) _ -> (total, processed) @?= (3, 3)
  ]

name :: String -> QualifiedName
name = QualifiedName []

parseTypePure :: String -> Either String (HsType, TypeVarIndex)
parseTypePure source = runIdentity
  $ runMultiRWSTNil
  $ runExceptT
  $ parseType [] Nothing [] Map.empty (haskellSrcExtsParseMode "test") source
